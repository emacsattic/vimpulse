(defcustom vimpulse-want-change-state nil
  "Whether commands like \"cw\" invoke Replace state, vi-like.
The default is to delete the text and enter Insert state,
like in Vim."
  :group 'vimpulse
  :type  'boolean)

(defadvice viper-change
  (around vimpulse-want-change-state activate)
  "Disable Replace state if `vimpulse-want-change-state' is nil."
  (cond
   (vimpulse-want-change-state
    ad-do-it)
   (t
    ;; We don't want Viper's Replace mode when changing text;
    ;; just delete and enter Insert state.
    (kill-region beg end)
    (viper-change-state-to-insert))))

(defvar vimpulse-goto-line t
  "*Goto line with \"G\" like in Vim.")

(defun vimpulse-goto-line (arg)
  "Go to ARG's line; without ARG go to end of buffer.
Works like Vim's \"G\"."
  (interactive "P")
  (let ((val (viper-P-val arg))
        (com (viper-getCom arg)))
    (when (eq ?c com) (setq com ?C))
    (viper-move-marker-locally 'viper-com-point (point))
    (viper-deactivate-mark)
    (push-mark nil t)
    (cond
     ((null val)
      (goto-char (point-max)))
     (t
      (goto-line val)))
    (when com
      (viper-execute-com 'vimpulse-goto-line val com))))

(when vimpulse-goto-line
  (fset 'viper-goto-line 'vimpulse-goto-line))

;;; CODE FOR ADDING EXTRA STATES

;; State index variables: for keeping track of which modes belong
;; to which states, et cetera.
(defvar vimpulse-state-maps-alist nil
  "Alist of Vimpulse state maps.
Entries have the form (MODE . MAP-EXPR), where MAP-EXPR is an
expression for determining the keymap of MODE. The first entries
get the highest priority.")

(defvar vimpulse-state-modes-alist
  '((vi-state
     . ((viper-vi-intercept-minor-mode . t)
        (viper-vi-minibuffer-minor-mode . (viper-is-in-minibuffer))
        (viper-vi-local-user-minor-mode . t)
        (viper-vi-kbd-minor-mode . (not (viper-is-in-minibuffer)))
        (viper-vi-global-user-minor-mode . t)
        (viper-vi-state-modifier-minor-mode . t)
        (viper-vi-diehard-minor-mode
         . (not (or viper-want-emacs-keys-in-vi
                    (viper-is-in-minibuffer))))
        (viper-vi-basic-minor-mode . t)))
    (insert-state
     . ((viper-insert-intercept-minor-mode . t)
        (viper-replace-minor-mode . (eq state 'replace-state))
        (viper-insert-minibuffer-minor-mode . (viper-is-in-minibuffer))
        (viper-insert-local-user-minor-mode . t)
        (viper-insert-kbd-minor-mode . (not (viper-is-in-minibuffer)))
        (viper-insert-global-user-minor-mode . t)
        (viper-insert-state-modifier-minor-mode . t)
        (viper-insert-diehard-minor-mode
         . (not (or viper-want-emacs-keys-in-insert
                    (viper-is-in-minibuffer))))
        (viper-insert-basic-minor-mode . t)))
    (replace-state
     . ((viper-insert-intercept-minor-mode . t)
        (viper-replace-minor-mode . (eq state 'replace-state))
        (viper-insert-minibuffer-minor-mode . (viper-is-in-minibuffer))
        (viper-insert-local-user-minor-mode . t)
        (viper-insert-kbd-minor-mode . (not (viper-is-in-minibuffer)))
        (viper-insert-global-user-minor-mode . t)
        (viper-insert-state-modifier-minor-mode . t)
        (viper-insert-diehard-minor-mode
         . (not (or viper-want-emacs-keys-in-insert
                    (viper-is-in-minibuffer))))
        (viper-insert-basic-minor-mode . t)))
    (emacs-state
     . ((viper-emacs-intercept-minor-mode . t)
        (viper-emacs-local-user-minor-mode . t)
        (viper-emacs-kbd-minor-mode . (not (viper-is-in-minibuffer)))
        (viper-emacs-global-user-minor-mode . t)
        (viper-emacs-state-modifier-minor-mode . t))))
  "Alist of Vimpulse state modes.
Entries have the form (STATE . ((MODE . EXPR) ...)), where STATE
is the name of a state, MODE is a mode associated with STATE and
EXPR is an expression with which to enable or disable MODE.")

(defvar vimpulse-state-modifier-alist
  '((vi-state . viper-vi-state-modifier-alist)
    (insert-state . viper-insert-state-modifier-alist)
    (emacs-state . viper-emacs-state-modifier-alist))
  "Alist of state modifier alists.
Entries have the form (STATE . MODIFIER-ALIST-VAR), where
MODIFIER-ALIST-VAR is the name of the variable containing the
modifier alist of STATE.")

(defvar vimpulse-state-id-alist nil
  "Alist of Vimpulse state mode line indicators.
Entries have the form (STATE . ID-VAR), where ID-VAR is the name
of the variable containing the state's indicator.")

;; State-changing code: this uses the variables above.
(defadvice viper-normalize-minor-mode-map-alist
  (after vimpulse-states activate)
  "Normalize Vimpulse state maps."
  (let (temp mode map alists)
    (cond
     ((featurep 'xemacs)
      (setq alists '(viper--key-maps minor-mode-map-alist)))
     ((>= emacs-major-version 22)
      (setq alists '(viper--key-maps)))
     (t
      (setq alists '(minor-mode-map-alist))))
    (dolist (alist alists)
      (dolist (entry (reverse vimpulse-state-maps-alist))
        (setq mode (car entry)
              map  (eval (cdr entry)))
        (setq temp (default-value alist))
        (setq temp (assq-delete-all mode temp)) ; already there?
        (add-to-list 'temp (cons mode map))
        (set-default alist temp)
        (setq temp (eval alist))
        (setq temp (assq-delete-all mode temp))
        (add-to-list 'temp (cons mode map))
        (set alist temp)))))

(defadvice viper-refresh-mode-line (after vimpulse-states activate)
  "Refresh mode line tag for Vimpulse states."
  (let ((id (eval (cdr (assq viper-current-state
                             vimpulse-state-id-alist)))))
    (when id
      (set (make-local-variable 'viper-mode-string) id)
      (force-mode-line-update))))

(defadvice viper-set-mode-vars-for (after vimpulse-states activate)
  "Toggle Vimpulse state modes."
  (let (enable disable)
    ;; Determine which modes to enable
    (setq enable (cdr (assq state vimpulse-state-modes-alist)))
    (when enable
      ;; Determine which modes to disable
      (dolist (entry vimpulse-state-modes-alist)
        (dolist (mode (mapcar 'car (cdr entry)))
          (unless (assq mode enable)
            (add-to-list 'disable mode t))))
      ;; Enable modes
      (dolist (entry enable)
        (set (car entry) (eval (cdr entry))))
      ;; Disable modes
      (dolist (entry disable)
        (set entry nil)))))

(defun vimpulse-modify-major-mode (mode state keymap)
  "Modify key bindings in a major-mode in a Viper state using a keymap.

If the default for a major mode is emacs-state, then
modifications to this major mode may not take effect until the
buffer switches state to Vi, Insert or Emacs. If this happens,
add `viper-change-state-to-emacs' to this major mode's hook.
If no such hook exists, you may have to put an advice on the
function that invokes the major mode. See `viper-set-hooks'
for hints.

The above needs not to be done for major modes that come up in
Vi or Insert state by default."
  (let ((alist (cdr (assq state vimpulse-state-modifier-alist))) elt)
    (if (setq elt (assoc mode (eval alist)))
        (set alist (delq elt (eval alist))))
    (set alist (cons (cons mode keymap) (eval alist)))
    (viper-normalize-minor-mode-map-alist)
    (viper-set-mode-vars-for viper-current-state)))

(fset 'viper-modify-major-mode 'vimpulse-modify-major-mode)

(defun vimpulse-modifier-map (state &optional mode)
  "Return the current major mode modifier map for STATE.
If none, return an empty keymap (`viper-empty-keymap')."
  (setq mode (or mode major-mode))
  (setq state (eval (cdr (assq state vimpulse-state-modifier-alist))))
  (if (keymapp (cdr (assoc mode state)))
      (cdr (assoc mode state))
    viper-empty-keymap))

;; TODO: `viper-add-local-keys'

;; Macro for defining new Viper states. This saves us the trouble of
;; indexing all those minor modes manually.
(defmacro vimpulse-define-state (state doc &rest body)
  "Define a new Viper state STATE.
DOC is a general description and shows up in all docstrings.
Then follows one or more optional keywords:

:id ID                  Mode line indicator.
:basic-mode MODE        Basic minor mode for STATE.
:basic-map MAP          Keymap of :basic-mode.
:modifier-mode MODE     Minor mode for modifying major modes.
:modifier-map MAP       Keymap of :modifier-mode.
:global-user-mode MODE  Minor mode for global user bindings.
:global-user-map MAP    Keymap of :global-user-mode.
:local-user-mode MODE   Minor mode for local user bindings.
:local-user-map MAP     Keymap of :local-user-mode.
:enable LIST            List of other modes enabled by STATE.
:prefix PREFIX          Variable prefix, default \"vimpulse-\".
:advice TYPE            Toggle advice type, default `after'.

It is not necessary to specify all of these; the minor modes are
created automatically unless you provide an existing mode. The
only keyword you should really specify is :id, the mode line tag.
For example:

    (vimpulse-define-state test
      \"A simple test state.\"
      :id \"<T> \")

The basic keymap of this state will then be
`vimpulse-test-basic-map', and so on.

Following the keywords is optional code to be executed each time
the state is enabled or disabled. This is stored in a `defadvice'
of `viper-change-state'. :advice specifies the advice type
\(default `after')."
  (declare (debug (&define name stringp
                           [&rest [keywordp sexp]]
                           def-body))
           (indent defun))
  (let (advice basic-map basic-mode enable enable-modes-alist
        enable-states-alist global-user-map global-user-mode id
        id-string keyword local-user-map local-user-mode
        modifier-alist modifier-mode name name-string prefix
        prefixed-name-string state-name state-name-string)
    ;; Collect keywords
    (while (keywordp (setq keyword (car body)))
      (setq body (cdr body))
      (cond
       ((eq :prefix keyword)
        (setq prefix (vimpulse-unquote (pop body))))
       ((eq :enable keyword)
        (setq enable (vimpulse-unquote (pop body))))
       ((eq :advice keyword)
        (setq advice (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-id :id))
        (setq id (vimpulse-unquote (pop body))))
       ((memq keyword '(:basic-mode :basic-minor-mode))
        (setq basic-mode (vimpulse-unquote (pop body))))
       ((eq :basic-map keyword)
        (setq basic-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:local-user-mode :local-user-minor-mode))
        (setq local-user-mode (vimpulse-unquote (pop body))))
       ((eq :local-user-map keyword)
        (setq local-user-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:global-user-mode :global-user-minor-mode))
        (setq global-user-mode (vimpulse-unquote (pop body))))
       ((eq :global-user-map keyword)
        (setq global-user-map (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-modifier-minor-mode
                        :state-modifier-mode
                        :modifier-minor-mode
                        :modifier-mode))
        (setq modifier-mode (vimpulse-unquote (pop body))))
       ((memq keyword '(:state-modifier-alist :modifier-alist))
        (setq modifier-alist (vimpulse-unquote (pop body))))
       (t
        (pop body))))
    ;; Set up state names
    (setq name-string (replace-regexp-in-string
                       "-state$" "" (symbol-name state)))
    (setq name (intern name-string))
    (setq state-name-string (concat name-string "-state"))
    (setq state-name (intern state-name-string))
    (when (and prefix (symbolp prefix))
      (setq prefix (symbol-name prefix)))
    (setq prefix (or prefix "vimpulse-"))
    (setq prefixed-name-string (concat prefix name-string))
    ;; Create state variables
    (setq id
          (vimpulse-define-symbol
           id (concat prefixed-name-string "-state-id")
           (format "<%s> " (upcase name-string)) 'stringp
           (format "Mode line tag indicating %s.\n\n%s"
                   state-name doc)))
    (setq basic-mode
          (vimpulse-define-symbol
           basic-mode
           (concat prefixed-name-string "-basic-minor-mode")
           nil nil (format "Basic minor mode for %s.\n\n%s"
                           state-name doc)))
    (setq basic-map
          (vimpulse-define-symbol
           basic-map (concat prefixed-name-string "-basic-map")
           (make-sparse-keymap) 'keymapp
           (format "The basic %s keymap.\n\n%s" state-name doc)))
    (setq local-user-mode
          (vimpulse-define-symbol
           local-user-mode
           (concat prefixed-name-string "-local-user-minor-mode")
           nil nil (format "Auxiliary minor mode for user-defined \
local bindings in %s.\n\n%s" state-name doc)))
    (setq local-user-map
          (vimpulse-define-symbol
           local-user-map
           (concat prefixed-name-string "-local-user-map")
           (make-sparse-keymap) 'keymapp
           (format "Auxiliary map for per-buffer user-defined \
keybindings in %s.\n\n%s" state-name doc)))
    (setq global-user-mode
          (vimpulse-define-symbol
           global-user-mode
           (concat prefixed-name-string "-global-user-minor-mode")
           nil nil (format "Auxiliary minor mode for global \
user-defined bindings in %s.\n\n%s" state-name doc)))
    (setq global-user-map
          (vimpulse-define-symbol
           global-user-map
           (concat prefixed-name-string "-global-user-map")
           (make-sparse-keymap) 'keymapp
           (format "Auxiliary map for global user-defined keybindings \
in %s.\n\n%s" state-name doc)))
    (setq modifier-mode
          (vimpulse-define-symbol
           modifier-mode
           (concat prefixed-name-string "-state-modifier-minor-mode")
           nil nil (format "Minor mode used to make major \
mode-specific modifications to %s.\n\n%s" state-name doc)))
    (setq modifier-alist
          (vimpulse-define-symbol
           modifier-alist
           (concat prefixed-name-string "-state-modifier-alist")
           nil 'listp))
    ;; Index the keymaps
    (add-to-list 'vimpulse-state-maps-alist
                 (cons basic-mode basic-map))
    (add-to-list 'vimpulse-state-maps-alist
                 (cons modifier-mode
                       `(if (keymapp
                             (cdr (assoc major-mode
                                         ,modifier-alist)))
                            (cdr (assoc major-mode
                                        ,modifier-alist)))))
    (add-to-list 'vimpulse-state-maps-alist
                 (cons global-user-mode global-user-map))
    (add-to-list 'vimpulse-state-maps-alist
                 (cons local-user-mode local-user-map))
    ;; Re-normalize keymaps for good measure
    (viper-normalize-minor-mode-map-alist)
    ;; Index the minor modes.
    ;; First, sort modes from states in :enable.
    (unless (listp enable)
      (setq enable (list enable)))
    (dolist (entry enable)
      (let ((mode entry) (val t))
        (when (listp entry)
          (setq mode (car entry)
                val (cadr entry)))
        (cond
         ((assq mode vimpulse-state-modes-alist)
          (add-to-list 'enable-states-alist (cons mode val) t))
         (mode
          (add-to-list 'enable-modes-alist (cons mode val) t)))))
    ;; Then add the state's own modes to the front
    ;; if they're not already there
    (dolist (mode (list basic-mode
                        modifier-mode
                        global-user-mode
                        local-user-mode))
      (unless (assq mode enable-modes-alist)
        (add-to-list 'enable-modes-alist (cons mode t))))
    ;; Finally, add the modes of other states
    (dolist (entry enable-states-alist)
      (let ((state (car entry)) (val (cdr entry)))
        (dolist (mode (cdr (assq state vimpulse-state-modes-alist)))
          (unless (assq (car mode) enable-modes-alist)
            (add-to-list 'enable-modes-alist
                         (if val mode
                           (cons (car mode) nil)) t)))))
    ;; Index the modes
    (add-to-list 'vimpulse-state-modes-alist
                 (cons state-name enable-modes-alist))
    ;; Index the modifier alist
    (add-to-list 'vimpulse-state-modifier-alist
                 (cons state-name modifier-alist) t)
    ;; Index the mode line indicator
    (add-to-list 'vimpulse-state-id-alist
                 (cons state-name id) t)
    ;; Make toggle-advice (this is the macro expansion)
    (setq advice (or advice 'after))
    (when body
      `(defadvice viper-change-state (,advice ,state-name activate)
         ,(format "Toggle %s." state-name)
         ,@body))))

;; These are for making `vimpulse-define-state' more forgiving
(defun vimpulse-unquote (exp)
  "Return EXP unquoted."
  (if (and (listp exp)
           (eq 'quote (car exp)))
      (eval exp)
    exp))

(defun vimpulse-define-symbol
  (sym-or-val varname varval &optional val-p doc)
  "Accept a symbol or a value and define a variable for it.
 If SYM-OR-VAL is a symbol, set that symbol's value to VARVAL.
 If SYM-OR-VAL is a value, set VARNAME's value to SYM-OR-VAL.
 VAL-P checks whether SYM-OR-VAL's value is \"valid\", in which case it is kept;
 otherwise we default to VARVAL.
 DOC is the docstring for the defined variable.
 Return the result."
  (cond
   ((and sym-or-val (symbolp sym-or-val)) ; nil is a symbol
    (setq varname sym-or-val))
   ((or (not val-p) (funcall val-p sym-or-val))
    (setq varval sym-or-val)))
  (when (stringp varname)
    (setq varname (intern varname)))
  (unless (and (boundp varname) val-p
               (funcall val-p (eval varname)))
    (eval `(defvar ,varname (quote ,varval) ,doc))
    (set varname varval))
  varname)

;;
;; Thanks to the anonymous poster for the idea on how to modify the viper
;; function to add the di da ci and ca partial commands.
;;
(defcustom vimpulse-text-objects t
  "Text objects support, on by default."
  :group 'vimpulse
  :type  'boolean)

;; REDEFINITION OF VIPER FUNCTION
;;
;; `viper-prefix-arg-com', originally defined in viper-cmd.el, does
;; much of the work of reading keyboard input and chosing the
;; appropriate command. As an ugly way of getting basic "delete inner
;; parentheses" functionality, we extend it here with entries for our
;; custom `vimpulse-di' and `vimpulse-ci' functions (defined below).
;;
;; This should be done in a cleaner way. Michael Kifer gives some
;; hints in viper.el:
;;
;;     Some of the code that is inherited from VIP-3.5 is rather
;;     convoluted. Instead of viper-command-argument, keymaps should
;;     bind the actual commands. E.g., "dw" should be bound to a
;;     generic command viper-delete that will delete things based on
;;     the value of last-command-char. This would greatly simplify the
;;     logic and the code.
;;
;; For (some) brewity, Kifer's comments are removed. The added lines
;; are annotated with ";; MODIFICATION".

(defun vimpulse-prefix-arg-com (char value com)
  (let ((cont t)
        cmd-info
        cmd-to-exec-at-end)
    (while (and cont
                (viper-memq-char char
                                 (list ?q ?i ?a ?c ?d ?y ?! ?< ?> ?= ?# ?r ?R ?\" ;;modified ?i ?a ?q
                                       viper-buffer-search-char)))
      (if com
          ;; this means that we already have a command character, so we
          ;; construct a com list and exit while.  however, if char is "
          ;; it is an error.
          (progn
            ;; new com is (CHAR . OLDCOM)
            (if (viper-memq-char char '(?# ?\")) (error "Viper bell"))
            (setq com (cons char com))
            (setq cont nil))
        ;; If com is nil we set com as char, and read more.  Again, if char is
        ;; ", we read the name of register and store it in viper-use-register.
        ;; if char is !, =, or #, a complete com is formed so we exit the while
        ;; loop.
        (cond ((viper-memq-char char '(?! ?=))
               (setq com char)
               (setq char (read-char))
               (setq cont nil))
              ((viper= char ?#)
               ;; read a char and encode it as com
               (setq com (+ 128 (read-char)))
               (setq char (read-char)))
              ((viper= char ?\")
               (let ((reg (read-char)))
                 (if (viper-valid-register reg)
                     (setq viper-use-register reg)
                   (error "Viper bell"))
                 (setq char (read-char))))
              (t
               (setq com char)
               (setq char (read-char))))))
    (if (atom com)
        ;; `com' is a single char, so we construct the command argument
        ;; and if `char' is `?', we describe the arg; otherwise
        ;; we prepare the command that will be executed at the end.
        (progn
          (setq cmd-info (cons value com))
          (while (viper= char ?U)
            (viper-describe-arg cmd-info)
            (setq char (read-char)))
          ;; `char' is a movement cmd, a digit arg cmd, or a register cmd---so
          ;; we execute it at the very end
          (or (viper-movement-command-p char)
              (viper-digit-command-p char)
              (viper-regsuffix-command-p char)
              (viper= char ?!) ; bang command
              (viper= char ?g) ; the gg command (like G0)
              (viper= char ?=) ; the == command
              (error "Viper bell"))
          (setq cmd-to-exec-at-end
                (viper-exec-form-in-vi
                 `(key-binding (char-to-string ,char)))))
      ;; as com is non-nil, this means that we have a command to execute
      (if (viper-memq-char (car com) '(?r ?R))
          ;; execute apropriate region command.
          (let ((char (car com)) (com (cdr com)))
            (setq prefix-arg (cons value com))
            (if (viper= char ?r)
                (viper-region prefix-arg)
              (viper-Region prefix-arg))
            ;; reset prefix-arg
            (setq prefix-arg nil))
        ;; otherwise, reset prefix arg and call appropriate command
        (setq value (if (null value) 1 value))
        (setq prefix-arg nil)
        (cond
         ;; If we change ?C to ?c here, then cc will enter replacement mode
         ;; rather than deleting lines.  However, it will affect 1 less line
         ;; than normal.  We decided to not use replacement mode here and
         ;; follow Vi, since replacement mode on n full lines can be achieved
         ;; with nC.
         ((equal com '(?q . ?d)) (vimpulse-test-function value))
         ((equal com '(?a . ?d)) (vimpulse-delete-text-objects-command value ?a)) ; da<x>
         ((equal com '(?a . ?c)) (vimpulse-change-text-objects-command value ?a)) ; ca<x>
         ((equal com '(?a . ?y)) (vimpulse-yank-text-objects-command value ?a))   ; ya<x>
         ((equal com '(?i . ?d)) (vimpulse-delete-text-objects-command value ?i)) ; di<x>
         ((equal com '(?i . ?c)) (vimpulse-change-text-objects-command value ?i)) ; ci<x>
         ((equal com '(?i . ?y)) (vimpulse-yank-text-objects-command value ?i))   ; yi<x>
         ((equal com '(?c . ?c)) (viper-line (cons value ?C)))
         ((equal com '(?d . ?d)) (viper-line (cons value ?D)))
         ((equal com '(?d . ?y)) (viper-yank-defun))
         ((equal com '(?y . ?y)) (viper-line (cons value ?Y)))
         ((equal com '(?< . ?<)) (viper-line (cons value ?<)))
         ((equal com '(?> . ?>)) (viper-line (cons value ?>)))
         ((equal com '(?! . ?!)) (viper-line (cons value ?!)))
         ((equal com '(?= . ?=)) (viper-line (cons value ?=)))
         ;; gg  acts as G0
         ((equal (car com) ?g)   (viper-goto-line 0))
         (t (error "Viper bell")))))
    (if cmd-to-exec-at-end
        (progn
          (setq last-command-event
                (viper-copy-event
                 (if (featurep 'xemacs) (character-to-event char) char)))
          (condition-case err
              (funcall cmd-to-exec-at-end cmd-info)
            (error
             (error "%s" (error-message-string err))))))))

(when vimpulse-text-objects
  (fset 'viper-prefix-arg-com 'vimpulse-prefix-arg-com))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Redefining viper-ex to get a similar behavior to vim when ;;;
;;; issuing ":" when visual selecting.                        ;;;
;;; NOTE: this is a kludge.                                   ;;;
;;;       Vimpulse eats 'y and 'z marks to emulate vim's      ;;;
;;;       behavior instead of introducing '< and '>, because  ;;;
;;;       introducing them would introduce even more kludges  ;;;
;;;       like this one.                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom vimpulse-visual-ex t
  "Ex support for visual selections, on by default."
  :group 'vimpulse
  :type  'boolean)

(defun vimpulse-ex (arg &optional string)
  (interactive "P")
  (or string
      (setq ex-g-flag nil
            ex-g-variant nil))
  (let* ((map (copy-keymap minibuffer-local-map))
         (address nil)
         (cont t)
         (dot (point))
         (initial-str
          (when (and vimpulse-visual-mode
                     (not (eq 'block vimpulse-visual-mode)))
            "'y,'z"))
         reg-beg reg-end
         reg-beg-line reg-end-line
         prev-token-type com-str)
    (viper-add-keymap viper-ex-cmd-map map)
    (if arg
        (progn
          (viper-enlarge-region (mark t) (point))
          (if (> (point) (mark t))
              (setq reg-beg (mark t)
                    reg-end (point))
            (setq reg-end (mark t)
                  reg-beg (point)))
          (save-excursion
            (goto-char reg-beg)
            (setq reg-beg-line (1+ (count-lines (point-min) (point)))
                  reg-end-line
                  (+ reg-beg-line (count-lines reg-beg reg-end) -1)))))
    (if reg-beg-line
        (setq initial-str (format "%d,%d" reg-beg-line reg-end-line)))
    (setq com-str
          (if string
              (concat initial-str string)
            (viper-read-string-with-history
             ":"
             initial-str
             'viper-ex-history
             ;; No default when working on region
             (if initial-str
                 nil
               (car viper-ex-history))
             map
             (if initial-str
                 " [Type command to execute on current region]"))))
    (save-window-excursion
      ;; Just a precaution
      (setq viper-ex-work-buf (get-buffer-create viper-ex-work-buf-name))
      (set-buffer viper-ex-work-buf)
      (delete-region (point-min) (point-max))
      (insert com-str "\n")
      (goto-char (point-min)))
    (setq ex-token-type nil
          ex-addresses nil)
    (while cont
      (viper-get-ex-token)
      (cond ((memq ex-token-type '(command end-mark))
             (if address (setq ex-addresses (cons address ex-addresses)))
             (viper-deactivate-mark)
             (let ((cmd (ex-cmd-assoc ex-token ex-token-alist)))
               (if (null cmd)
                   (error "`%s': %s" ex-token viper-BadExCommand))
               (ex-cmd-execute cmd)
               (if (or (ex-cmd-is-mashed-with-args cmd)
                       (ex-cmd-is-one-letter cmd))
                   (setq cont nil)
                 (save-excursion
                   (save-window-excursion
                     (setq viper-ex-work-buf
                           (get-buffer-create viper-ex-work-buf-name))
                     (set-buffer viper-ex-work-buf)
                     (skip-chars-forward " \t")
                     (cond ((looking-at "|")
                            (forward-char 1))
                           ((looking-at "\n")
                            (setq cont nil))
                           (t
                            (error
                             "`%s': %s"
                             ex-token
                             viper-SpuriousText))))))))
            ((eq ex-token-type 'non-command)
             (error "`%s': %s" ex-token viper-BadExCommand))
            ((eq ex-token-type 'whole)
             (setq address nil)
             (setq ex-addresses
                   (if ex-addresses
                       (cons (point-max) ex-addresses)
                     (cons (point-max) (cons (point-min) ex-addresses)))))
            ((eq ex-token-type 'comma)
             (if (eq prev-token-type 'whole)
                 (setq address (point-min)))
             (setq ex-addresses
                   (cons (if (null address) (point) address) ex-addresses)))
            ((eq ex-token-type 'semi-colon)
             (if (eq prev-token-type 'whole)
                 (setq address (point-min)))
             (if address (setq dot address))
             (setq ex-addresses
                   (cons (if (null address) (point) address) ex-addresses)))
            (t (let ((ans (viper-get-ex-address-subr address dot)))
                 (if ans (setq address ans)))))
      (setq prev-token-type ex-token-type))))

(when vimpulse-visual-ex
  (fset 'viper-ex 'vimpulse-ex))

;; Viper's definition lacks an indentation specification
(defmacro viper-deflocalvar
  (var default-value &optional documentation)
  "Define VAR as a buffer-local variable.
DEFAULT-VALUE is the default value and DOCUMENTATION is the
docstring. The variable becomes buffer-local whenever set."
  (declare (indent defun))
  `(progn
    (defvar ,var ,default-value
      ,(format "%s\n\(buffer local\)" documentation))
    (make-variable-buffer-local ',var)))

(provide 'vimpulse-viper-function-redefinitions)

