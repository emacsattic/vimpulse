;;;; State system

;; What is "modes" in Vim is "states" in Vimpulse. States are defined
;; with the macro `vimpulse-define-state'.
;;
;; A state consists of a universal keymap (like
;; `vimpulse-vi-state-map' for vi state) and a buffer-local keymap for
;; overriding the former (like `vimpulse-vi-state-local-map').
;; Sandwiched between these keymaps may be so-called auxiliary
;; keymaps, which contain state bindings assigned to an Emacs mode
;; (minor or major): more on that below.
;;
;; A state may "inherit" keymaps from another state. For example,
;; Visual state will enable vi state's keymaps in addition to its own.
;; The keymap order then becomes:
;;
;;     <visual-local-map>
;;     <visual auxiliary maps>
;;     <visual-universal-map>
;;     <vi-local-map>
;;     <vi auxiliary maps>
;;     <vi-universal-map>
;;
;; Since the activation of auxiliary maps depends on the current
;; buffer and its modes, states are necessarily buffer-local.
;; Different buffers can have different states, and different buffers
;; enable states differently. (Thus, what keymaps to enable cannot be
;; determined at compile time.) For example, the user may define some
;; Visual state bindings for foo-mode, and if he enters foo-mode and
;; Visual state in the current buffer, then the auxiliary keymap
;; containing those bindings will be active. In a buffer where
;; foo-mode is not enabled, it will not be.
;;
;; Hence, state bindings may be grouped into Emacs modes. This is
;; useful for writing extensions.
;;
;; All state keymaps are listed in `vimpulse-mode-map-alist', which is
;; then listed in `emulation-mode-map-alist'. This gives state keymaps
;; precedence over other keymaps. Note that `vimpulse-mode-map-alist'
;; has both a default (global) value and a buffer-local value. The
;; default value is constructed when Vimpulse is loaded and its states
;; are defined. Afterwards, when entering a buffer, the default value
;; is copied into the buffer-local value, and that value is reordered
;; according to the current state (pushing Visual keymaps to the top
;; when the user enters Visual state, etc.).

(require 'vimpulse-common)

(defun vimpulse-enable ()
  "Enable Vimpulse in the current buffer, if appropriate.
To enable Vimpulse globally, do (vimpulse-mode 1)."
  ;; TODO: option for enabling vi keys in the minibuffer
  (unless (minibufferp)
    (vimpulse-local-mode 1)))

(define-minor-mode vimpulse-local-mode
  "Minor mode for setting up Vimpulse in a single buffer."
  :init-value nil
  (cond
   (vimpulse-local-mode
    (setq emulation-mode-map-alists
          (vimpulse-concat-lists '(vimpulse-mode-map-alist)
                                 emulation-mode-map-alists))
    (vimpulse-refresh-local-maps)
    (unless (memq 'vimpulse-modeline-tag global-mode-string)
      (setq global-mode-string
            (append '(" " vimpulse-modeline-tag " ")
                    global-mode-string)))
    (vimpulse-vi-state))
   (t
    (when vimpulse-state
      (funcall (vimpulse-state-func) -1)))))

(define-globalized-minor-mode vimpulse-mode
  vimpulse-local-mode vimpulse-enable)

(put 'vimpulse-mode 'function-documentation
     "Toggle Vimpulse in all buffers.
Enable with positive ARG and disable with negative ARG.
See `vimpulse-local-mode' to toggle Vimpulse in the
current buffer only.")

(defun vimpulse-state-property (state prop)
  "Return property PROP for STATE."
  (vimpulse-get-property vimpulse-states-alist state prop))

(defun vimpulse-state-p (sym)
  "Whether SYM is the name of a state."
  (assq sym vimpulse-states-alist))

(defun vimpulse-state-func (&optional state)
  "Return the toggle function for STATE."
  (setq state (or state vimpulse-state))
  (vimpulse-state-property state :mode))

(defun vimpulse-state-keymaps (state &rest excluded)
  "Return an ordered list of keymaps activated by STATE."
  (let* ((state (or state vimpulse-state))
         (map (symbol-value (vimpulse-state-property state :keymap)))
         (local-map (symbol-value (vimpulse-state-property
                                   state :local-keymap)))
         (aux-maps (vimpulse-state-auxiliary-keymaps state))
         (enable (vimpulse-state-property state :enable))
         (excluded (add-to-list 'excluded state))
         ;; the keymaps for STATE
         (result (append (list local-map) aux-maps (list map))))
    ;; the keymaps for other states and modes enabled by STATE
    (dolist (entry enable result)
      (cond
       ((vimpulse-state-p entry)
        (unless (memq entry excluded)
          (dolist (mode (vimpulse-state-modes entry excluded))
            (add-to-list 'result mode t))))
       (t
        (add-to-list 'result entry t))))))

(defun vimpulse-state-auxiliary-keymaps (state)
  "Return an ordered list of auxiliary keymaps for STATE."
  (let* ((state (or state vimpulse-state))
         (alist (symbol-value (vimpulse-state-property state :aux)))
         result)
    (dolist (map (current-active-maps) result)
      (when (keymapp (setq map (cdr (assq map alist))))
        (add-to-list 'result map t)))))

(defun vimpulse-normalize-keymaps (&optional state)
  "Create a buffer-local value for `vimpulse-mode-map-alist'.
Its order reflects the state in the current buffer."
  (let ((state (or state vimpulse-state)) alist mode)
    ;; initialize a buffer-local value
    (setq vimpulse-mode-map-alist
          (copy-sequence (default-value 'vimpulse-mode-map-alist)))
    ;; update references to buffer-local keymaps
    (vimpulse-refresh-local-maps)
    ;; disable all modes
    (dolist (entry vimpulse-mode-map-alist)
      (set (car entry) nil))
    ;; enable modes for current state
    (unless (null state)
      (dolist (map (vimpulse-state-keymaps state))
        (setq mode (or (car (rassq map vimpulse-mode-map-alist))
                       (car (rassq map minor-mode-map-alist))))
        (when mode
          (set mode t)
          (add-to-list 'alist (cons mode map) t)))
      ;; move the enabled modes to the front of the list
      (setq vimpulse-mode-map-alist
            (vimpulse-concat-lists
             alist vimpulse-mode-map-alist)))))

;; Local keymaps are implemented using buffer-local variables.
;; However, unless a buffer-local value already exists,
;; `define-key' acts on the variable's default (global) value.
;; So we need to initialize the variable whenever we enter a
;; new buffer or when the buffer-local values are reset.
(defun vimpulse-refresh-local-maps ()
  "Initialize a buffer-local value for all local keymaps."
  (let ((modes (vimpulse-state-property nil :local-mode))
        (maps (vimpulse-state-property nil :local-keymap))
        map mode state)
    (dolist (entry maps)
      (setq state (car entry)
            map (cdr entry)
            mode (cdr (assq state modes)))
      ;; initalize the variable
      (unless (symbol-value map)
        (set map (make-sparse-keymap)))
      ;; refresh the keymap's entry in `vimpulse-mode-map-alist'
      (setq vimpulse-mode-map-alist
            (copy-sequence vimpulse-mode-map-alist))
      (vimpulse-add-to-alist 'vimpulse-mode-map-alist mode
                             (symbol-value map)))))

(defun vimpulse-set-cursor (specs)
  "Change the cursor's apperance according to SPECS.
SPECS may be a cursor type as per `cursor-type', a color
string as passed to `set-cursor-color', a zero-argument
function for changing the cursor, or a list of the above.
If SPECS is nil, make the cursor a black box."
  (set-cursor-color "black")
  (setq cursor-type 'box)
  (unless (and (listp specs) (not (consp specs)))
    (setq specs (list specs)))
  (dolist (spec specs)
    (cond
     ((functionp spec)
      (funcall spec))
     ((stringp spec)
      (set-cursor-color spec))
     (t
      (setq cursor-type spec))))
  (redisplay))

(defun vimpulse-change-state (state)
  "Change state to STATE.
Disable all states if nil."
  (let ((func (vimpulse-state-property
               (or state vimpulse-state 'emacs) :mode)))
    (funcall func (if state 1 -1))))

(defmacro vimpulse-define-state (state doc &rest body)
  "Define a Vimpulse state STATE.
DOC is a general description and shows up in all docstrings.
Then follows one or more optional keywords:

:tag STRING             Mode line indicator.
:message STRING         Echo area message when changing to STATE.
:cursor SPEC            Cursor to use in STATE.
:entry-hook LIST        Hooks run when changing to STATE.
:exit-hook LIST         Hooks run when changing from STATE.
:enable LIST            List of other states and modes enabled by STATE.

Following the keywords is optional code to be executed each time
the state is enabled or disabled.

For example:

    (vimpulse-define-state test
      \"A simple test state.\"
      :tag \"<T> \")

The basic keymap of this state will then be
`vimpulse-test-state-map', and so on."
  (declare (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp sexp]]
                           def-body))
           (indent defun))
  (let ((mode (intern (format "vimpulse-%s-state" state)))
        (keymap (intern (format "vimpulse-%s-state-map" state)))
        (local-mode (intern (format "vimpulse-%s-state-local" state)))
        (local-keymap (intern (format "vimpulse-%s-state-local-map" state)))
        (aux (intern (format "vimpulse-%s-state-auxiliary-maps" state)))
        (predicate (intern (format "vimpulse-%s-state-p" state)))
        (tag (intern (format "vimpulse-%s-state-tag" state)))
        (message (intern (format "vimpulse-%s-state-message" state)))
        (cursor (intern (format "vimpulse-%s-state-cursor" state)))
        (entry-hook (intern (format "vimpulse-%s-state-entry-hook" state)))
        (exit-hook (intern (format "vimpulse-%s-state-exit-hook" state)))
        cursor-value enable entry-hook-value exit-hook-value keyword
        message-value tag-value)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :tag)
        (setq tag-value (pop body)))
       ((eq keyword :message)
        (setq message-value (pop body)))
       ((eq keyword :cursor)
        (setq cursor-value (pop body)))
       ((eq keyword :entry-hook)
        (setq entry-hook-value (pop body)))
       ((eq keyword :exit-hook)
        (setq exit-hook-value (pop body)))
       ((eq keyword :enable)
        (setq enable (pop body)))
       (t
        (pop body))))

    ;; macro expansion
    `(let ((mode-map-alist (default-value 'vimpulse-mode-map-alist)))

       ;; Save the state's properties in `vimpulse-states-alist' for
       ;; runtime lookup. Among other things, this information is used
       ;; to determine what keymaps should be activated by the state
       ;; (and, when processing :enable, what keymaps are activated by
       ;; other states). We cannot know this at compile time because
       ;; it depends on the current buffer and its active keymaps
       ;; (to which we may have assigned state bindings), as well as
       ;; states whose definitions may not have been processed yet.
       (vimpulse-put-property
        'vimpulse-states-alist ',state
        :tag (defvar ,tag ,tag-value
               ,(format "Modeline tag for %s state.\n\n%s" state doc))
        :message (defvar ,message ,message-value
                   ,(format "Echo area indicator for %s state.\n\n%s"
                            state doc))
        :cursor (defvar ,cursor ,cursor-value
                  ,(format "Cursor for %s state.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above.\n\n%s" state doc))
        :entry-hook (defvar ,entry-hook ,entry-hook-value
                      ,(format "Hooks to run when entering %s state.\n\n%s"
                               state doc))
        :exit-hook (defvar ,exit-hook ,exit-hook-value
                     ,(format "Hooks to run when exiting %s state.\n\n%s"
                              state doc))
        :mode (defvar ,mode nil
                ,(format "Non-nil if %s state is enabled.
Use the command `%s' to change this variable." state mode))
        :keymap (defvar ,keymap (make-sparse-keymap)
                  ,(format "Keymap for %s state.\n\n%s" state doc))
        :local-mode (defvar ,local-mode nil
                      ,(format "Non-nil if %s state is enabled.
Use the command `%s' to change this variable." state mode))
        :local-keymap (defvar ,local-keymap nil
                        ,(format "Buffer-local keymap for %s state.\n\n%s"
                                 state doc))
        :aux (defvar ,aux nil
               ,(format "Association list of auxiliary keymaps for %s state.
Elements have the form (KEYMAP . AUX-MAP), where AUX-MAP contains state
bindings to be activated whenever KEYMAP and %s state are active."
                        state state))
        :predicate (defun ,predicate ()
                     ,(format "Whether the current state is %s." state)
                     (eq vimpulse-state ',state))
        :enable ',enable)

       (vimpulse-add-to-alist 'mode-map-alist
                              ',local-mode ,local-keymap
                              ',mode ,keymap)

       (setq-default vimpulse-mode-map-alist mode-map-alist)

       (make-variable-buffer-local ',mode)
       (make-variable-buffer-local ',local-mode)
       (make-variable-buffer-local ',local-keymap)

       ;; define state function
       (defun ,mode (&optional arg)
         ,(format "Enable %s state. Disable with negative ARG.\n\n%s"
                  state doc)
         (interactive)
         (cond
          ((and (numberp arg) (< arg 1))
           (unwind-protect
               (let (vimpulse-state)
                 (vimpulse-normalize-keymaps)
                 (run-hooks ',exit-hook)
                 ,@body)
             (setq vimpulse-state nil)))
          (t
           (unless vimpulse-local-mode
             (vimpulse-enable))
           (when vimpulse-state
             (funcall (vimpulse-state-func) -1))
           (unwind-protect
               (let ((vimpulse-state ',state))
                 (vimpulse-normalize-keymaps)
                 (setq vimpulse-modeline-tag ,tag)
                 (force-mode-line-update)
                 (vimpulse-set-cursor ,cursor)
                 ,@body
                 (run-hooks ',entry-hook)
                 (when ,message (vimpulse-unlogged-message ,message)))
             (setq vimpulse-state ',state)))))

       ',state)))

;; Define vi (command) state

(vimpulse-define-state vi
  "Command state, AKA \"Normal\" state."
  :tag "<V>")

(vimpulse-define-state emacs
  "Emacs state."
  :tag "<E>")

(define-key vimpulse-vi-state-map "\C-z" 'vimpulse-emacs-state)
(define-key vimpulse-emacs-state-map "\C-z" 'vimpulse-vi-state)

(provide 'vimpulse-states)
