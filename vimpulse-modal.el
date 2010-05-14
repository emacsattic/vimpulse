;;;; Modal keybinding functions

;; This provides the functions `vimpulse-map', `vimpulse-imap',
;; `vimpulse-vmap' and `vimpulse-omap', which mimic :map, :imap,
;; :vmap and :omap in Vim, as well as `vimpulse-define-key', a
;; general-purpose function for binding keys in a "careful" way.
;;
;; BACKGROUND
;;
;; The :map, :imap, :vmap and :omap commands of Vim let one make two
;; key mappings starting with the same sequence of characters without
;; one overwriting the other. For example:
;;
;;     :imap aa foo
;;     :imap aaa bar
;;
;; When Vim has read "aa" in Insert mode, it will wait for another
;; character to decide whether to insert "foo" or "bar". If the user
;; types "a", "bar" is inserted; if another letter, "foo" plus that
;; letter.
;;
;; Compare with the analogous use of Emacs' `global-set-key' function:
;;
;;     (global-set-key "aa" 'foo)
;;     (global-set-key "aaa" 'bar)
;;
;; Here, the first binding is simply overwritten by the more specific
;; second. The end result is that "aaa" is bound to `bar', while any
;; other sequence starting with "aa" is not bound to anything.
;;
;; The solution is a set of Vim-like or "modal" functions for making
;; new key bindings "on top of" previous bindings. They are
;; `vimpulse-map', `vimpulse-imap', `vimpulse-vmap' and
;; `vimpulse-omap', which mimic Vim's commands, and
;; `vimpulse-define-key', a general function for specifying the
;; keymap. Returning to the example:
;;
;;     (vimpulse-imap "aa" 'foo)
;;     (vimpulse-imap "aaa" 'bar)
;;
;; This will bind "aaa" to `bar', and "aa" + any other key to `foo'.
;; The syntax is the same as that of `global-set-key'. The key
;; sequence may be specified as a string, like above, as a vector
;; (like [?a ?b ?c]), or as a call to `kbd' (like (kbd "a b c")).
;;
;; To make a binding in vi (command) mode, use `vimpulse-map';
;; in Insert mode, `vimpulse-imap'; in Visual mode, `vimpulse-vmap';
;; in Operator-Pending mode, `vimpulse-omap'. The more general
;; `vimpulse-define-key' function lets one specify the keymap to store
;; the binding in, as when using `define-key':
;;
;;     (vimpulse-define-key keymap "abc" 'command)
;;
;; IMPLEMENTATION
;;
;; The code depends on a little-known GNU Emacs feature called
;; "default key bindings". A default key binding is a binding ending
;; with the Lisp symbol t, which roughly stands for "any other key".
;; Default bindings allow a keymap to bind all possibilities without
;; having to enumerate them. For example, we may bind the sequence
;; "AB" + any key as such:
;;
;;     (global-set-key (kbd "A B <t>") 'foo)
;;
;; This means that "ABA" will execute `foo', as will "ABB", "ABC",
;; and so on. For more on default key bindings, see the GNU Emacs
;; Lisp Reference Manual, chapter 22.3: "Format of Keymaps".
;;
;; What is done by functions like `vimpulse-define-key' and
;; `vimpulse-map' (which depends on the former) is to generate these
;; default bindings automatically. If "AB" is already bound to `foo'
;; and we modally bind "ABC" to `bar', the old binding is first
;; replaced by a default binding, as if we issued the following:
;;
;;     (global-set-key (kbd "A B") nil) ; delete old binding
;;     (global-set-key (kbd "A B <t>") 'foo)
;;     (global-set-key (kbd "A B C") 'bar)
;;
;; Then, "ABC" runs `bar', while "AB" + any other key than C
;; runs `foo'.
;;
;; This almost gets us where we want with regard to Vimpulse, but not
;; quite. The problem is that quite a few commands must necessarily
;; read and parse keyboard input to decide what to do. For instance,
;; Viper binds "d" to the general command `viper-command-argument',
;; which, depending on the next key-presses, deletes a line, two
;; words, or any motion entered by the user. What happens if we decide
;; to modally bind, say, "dq" to a custom command `foo' of our own?
;;
;;     (global-set-key (kbd "d") nil) ; delete old binding
;;     (global-set-key (kbd "d <t>") 'viper-command-argument)
;;     (global-set-key (kbd "d q") 'foo)
;;
;; Now, if the user enters "dq", `foo' is called. But when the user
;; enters "dw" to delete a word, `viper-command-argument' is called
;; only after the "w" is entered. This destroys the logic of the
;; command, which depends on "d" being the last key-press (stored in
;; `last-command-event') before "w" is read through `read-char'. It
;; obviously won't work as intended with a single "w" missing a
;; preceding "d", which is what it sees.
;;
;; So, we need to find a way to pass "d" and "w" along in the proper
;; manner; that is, to make the default binding appear the same as the
;; old binding it replaces. This is done by `vimpulse-modal-pre-hook',
;; which unreads "w" (so it can be read again) and changes
;; `last-command-event' to "d". Of course, this behavior is only
;; needed for default key bindings, and only for default key bindings
;; made by the modal binding functions. To that end, every time
;; `vimpulse-define-key' makes a default binding, the binding is
;; listed in `vimpulse-modal-alist' for future reference. Checking
;; against the list, `vimpulse-modal-pre-hook' only does its thing if
;; the current binding comes back positive.
;;
;; XEmacs is somewhat fuzzy about its command loop variables, not
;; allowing direct modification of `last-command-event'. However,
;; shadowing it with a `let' binding is possible, and a wrap-around
;; advice of the current command is employed to accomplish this. Also,
;; XEmacs does not have default key bindings in quite the same way as
;; GNU Emacs; `vimpulse-default-binding' takes care of the differences.
;;
;; LIMITATIONS
;;
;; Vim has a `timeout' option which lets one specify the time in
;; milliseconds that is waited for a key code or mapped key sequence
;; to complete. Emacs, on the other hand, will wait indefinitely. This
;; behavior is probably not implementable.

;;; Advice

;; For XEmacs, construct a wrap-around advice of the current command
;; shadowing the read-only command loop variables with a
;; `let' binding
(defmacro vimpulse-advice-command (command)
  "Make wrap-around advice for shadowing `last-command-event'.
XEmacs does not allow us to change its command loop variables
directly, but shadowing them with a `let' binding works."
  `(defadvice ,command (around vimpulse-modal activate)
     "Shadow `last-command-event' with a `let' binding."
     (cond
      (vimpulse-last-command-event
       (let* ((last-command-event
               (character-to-event vimpulse-last-command-event))
              (last-command-char vimpulse-last-command-event)
              (last-input-event last-command-event)
              (last-input-char last-command-char))
         ad-do-it))
      (t
       ad-do-it))))

;;; General functions

(defun vimpulse-modal-check (key-sequence)
  "Return t if KEY-SEQUENCE defaults to `this-command',
but only for bindings listed in `vimpulse-modal-alist'."
  (let ((temp-sequence (vimpulse-strip-prefix key-sequence)))
    (setq temp-sequence (vimpulse-truncate temp-sequence -1))
    (and this-command ; may be nil
         (not (key-binding key-sequence)) ; only default bindings
         (eq (cdr (assoc temp-sequence vimpulse-modal-alist))
             this-command))))

(defun vimpulse-modal-remove (key-vector &optional recursive)
  "Delete entry with KEY-VECTOR from `vimpulse-modal-alist'.
If RECURSIVE is non-nil, also delete entries whose key-vectors
start with KEY-VECTOR."
  (if recursive
      (dolist (entry vimpulse-modal-alist)
        (when (equal (vimpulse-truncate (car entry)
                                        (length key-vector))
                     key-vector)
          (setq vimpulse-modal-alist
                (delq entry vimpulse-modal-alist))))
    (assq-delete-all key-vector vimpulse-modal-alist)))

(defun vimpulse-xemacs-def-binding
  (keymap key def &optional modal-binding define-func)
  "Make a default binding in XEmacs. If MODAL-BINDING is
non-nil, advice DEF by means of `vimpulse-advice-command'."
  (let ((temp-sequence (vconcat key))
        (submap (lookup-key keymap key)))
    (unless define-func (setq define-func 'define-key))
    (and modal-binding (commandp def)
         (eval `(vimpulse-advice-command ,def)))
    (and (> (length temp-sequence) 1)
         (eq (aref temp-sequence (1- (length temp-sequence))) t)
         (setq temp-sequence (vimpulse-truncate temp-sequence -1)))
    ;; The following is from
    ;; http://tracker.xemacs.org/XEmacs/its/msg2021
    (unless (keymapp submap)
      (setq submap (make-sparse-keymap)))
    (when (fboundp 'set-keymap-default-binding)
      (set-keymap-default-binding submap def))
    (funcall define-func keymap temp-sequence submap)))

(defun vimpulse-default-binding
  (keymap key def &optional modal-binding define-func)
  "Make a default binding in GNU Emacs or XEmacs,
whichever is appropriate. If MODAL-BINDING is non-nil,
the binding is listed in `vimpulse-modal-alist'."
  (let ((temp-sequence (vconcat key)))
    (unless define-func (setq define-func 'define-key))
    (cond
     ((featurep 'xemacs)
      (vimpulse-xemacs-def-binding
       keymap temp-sequence def modal-binding define-func))
     (t
      (unless (eq (aref temp-sequence (1- (length temp-sequence))) t)
        (setq temp-sequence (vconcat temp-sequence [t])))
      (funcall define-func keymap temp-sequence def)))
    (when modal-binding
      (add-to-list 'vimpulse-modal-alist
                   (cons (vimpulse-truncate temp-sequence -1) def)))))

;;; Hook run before each command

;; If the current command is a default key binding made by the modal
;; binding functions, we need to unread the last input events and
;; change some command loop variables to give the command the
;; impression of its "old" binding
(defun vimpulse-modal-pre-hook ()
  "Update `vimpulse-last-command-event' and `unread-command-events'.
If the current key-sequence defaults to a shorter key-sequence,
the difference is stored in these two variables, to be passed on
via the `last-command-event' variable and the `read-char'
functions, respectively."
  (setq vimpulse-last-command-event nil)
  (let ((key-sequence (vconcat (this-command-keys))))
    ;; If XEmacs, get rid of the event object type
    (when (featurep 'xemacs)
      (setq key-sequence (events-to-keys key-sequence)))
    (while (and (> (length key-sequence) 1)
                (vimpulse-modal-check key-sequence))
      ;; Unread last event
      (setq vimpulse-last-command-event
            (elt key-sequence (1- (length key-sequence))))
      (when (featurep 'xemacs)
        (setq vimpulse-last-command-event
              (character-to-event vimpulse-last-command-event)))
      (add-to-list 'unread-command-events vimpulse-last-command-event)
      ;; Change command loop variables
      (setq vimpulse-last-command-event
            (elt key-sequence (1- (1- (length key-sequence)))))
      (unless (featurep 'xemacs)      ; if XEmacs, do this with advice
        (with-no-warnings
          (setq last-command-event vimpulse-last-command-event
                last-command-char  vimpulse-last-command-event
                last-input-event   vimpulse-last-command-event
                last-input-char    vimpulse-last-command-event)))
      (setq key-sequence
            (vimpulse-truncate key-sequence -1)))))

;;; hook run after each command

;; This merely ensures `vimpulse-last-command-event' is reset
(defun vimpulse-modal-post-hook ()
  "Erase `vimpulse-last-command-event'."
  (setq vimpulse-last-command-event nil))

(add-hook 'pre-command-hook  'vimpulse-modal-pre-hook)
(add-hook 'post-command-hook 'vimpulse-modal-post-hook)

;;; Modal binding functions

;; `vimpulse-define-key' is general; `vimpulse-map', `vimpulse-imap'
;; and `vimpulse-vmap' imitate Vim's :map, :imap and :vmap,
;; respectively.
(defun vimpulse-define-key
  (keymap key def &optional dont-list define-func)
  "Carefully bind KEY to DEF in KEYMAP.
\"Carefully\" means that if a subset of the key sequence is already
bound, a default binding is made so that the new binding won't
overwrite the old. E.g., if we want to carefully bind \"A B C\" to
`foo', and \"A B\" is already bound to `bar', the end result is

    \"A B C\"   => `foo'
    \"A B <t>\" => `bar'

which means that \"A B D\", for example, defaults to `bar'. (For
more on default bindings, see `define-key'.) The default binding
gets listed in `vimpulse-modal-alist', so that, with regard to
command loop variables, it appears exactly the same as the
binding it replaced. To override this, use DONT-LIST.
DEFINE-FUNC specifies a function to be used in place of
`define-key'.

To remove a binding, bind it to nil.

NOTE: If the original binding \"A B\" is not stored in KEYMAP,
but in some other map which is active only in a certain
state (say, Insert mode), this function can detect that binding
only if called in the same state. The functions `vimpulse-map',
`vimpulse-imap' and `vimpulse-vmap' take care of this."
  (let (key-vector temp-sequence current-binding previous-binding)
    ;; For each subset of KEY-VECTOR (stored in `temp-sequence'), check
    ;; the binding (stored in `current-binding'); if it isn't bound,
    ;; use `previous-binding'.
    (setq define-func (or define-func 'define-key))
    (setq key-vector key)
    (when (stringp key-vector)
      (condition-case nil
          (setq key-vector (eval `(kbd ,key-vector)))
        (error nil))
      (when (memq key-vector '("" [] nil))
        (setq key-vector key)))
    (setq key-vector (vconcat key-vector))
    (cond
     ;; nil unbinds the key-sequence
     ((not def)
      (funcall define-func keymap key-vector def)
      (while (and (> (length key-vector) 1)
                  (not (lookup-key keymap key-vector)))
        (vimpulse-modal-remove key-vector t)
        (setq key-vector (vimpulse-truncate key-vector -1))))
     ;; undefined also unbinds, but less forcefully
     ((eq def 'undefined)
      (if (keymapp (lookup-key keymap key-vector))
          (vimpulse-default-binding keymap key-vector nil t define-func)
        (funcall define-func keymap key-vector def))
      (vimpulse-modal-remove key-vector))
     ;; Regular binding: convert previous bindings to default bindings
     (t
      (dotimes (i (1- (length key-vector)))
        (setq temp-sequence (vimpulse-truncate key-vector (1+ i)))
        (setq current-binding (lookup-key keymap temp-sequence t))
        (when (or (numberp current-binding) (not current-binding))
          (setq current-binding
                (or (key-binding temp-sequence t) previous-binding)))
        (setq previous-binding current-binding)
        ;; If `current-binding' is a keymap, do nothing, since our modal
        ;; binding can exist happily as part of that keymap. However, if
        ;; `current-binding' is a command, we need to make room for the
        ;; modal binding by creating a default binding.
        (unless (keymapp current-binding)
          (setq temp-sequence (vconcat temp-sequence [t]))
          (setq current-binding (lookup-key keymap temp-sequence t))
          (when (or (numberp current-binding) (not current-binding))
            (setq current-binding
                  (or (key-binding temp-sequence t) previous-binding))
            (define-key keymap
              (vimpulse-truncate temp-sequence -1) nil)
            (vimpulse-default-binding
             keymap temp-sequence current-binding
             (not dont-list) define-func))
          (setq previous-binding current-binding)))
      ;; Defaults are taken care of; we may now bind the key.
      ;; If a longer binding starting with KEY-VECTOR exists,
      ;; make a default binding so it's not overwritten.
      (if (keymapp (lookup-key keymap key-vector))
          (vimpulse-default-binding
           keymap key-vector def (not dont-list) define-func)
        (funcall define-func keymap key def))))))

(define-minor-mode vimpulse-modal-minor-mode
  "Minor mode of bindings overwritten by `vimpulse-map' et al."
  :keymap vimpulse-modal-map
  (dolist (entry vimpulse-modal-alist)
    (unless (lookup-key vimpulse-modal-map (car entry))
      (define-key vimpulse-modal-map (car entry) (cdr entry))))
  (when vimpulse-modal-minor-mode
    (viper-normalize-minor-mode-map-alist)))

(add-to-list 'vimpulse-state-maps-alist
             (cons 'vimpulse-modal-minor-mode 'vimpulse-modal-map))

(defun vimpulse-map-state (state key def &optional modes)
  "Modally bind KEY to DEF in STATE.
Don't use this function directly; see `vimpulse-map',
`vimpulse-imap', `vimpulse-vmap' and `vimpulse-omap' instead."
  (let* ((old-state viper-current-state)
         (map (cdr (assq state vimpulse-state-vars-alist)))
         (basic-map (eval (cdr (assq 'basic-map map))))
         (global-user-map (eval (cdr (assq 'global-user-map map)))))
    (viper-set-mode-vars-for state)
    (let ((viper-current-state state))
      (viper-normalize-minor-mode-map-alist))
    (cond
     (modes
      (dolist (mode modes)
        (if (eq mode t)
            (vimpulse-define-key global-user-map key def)
          (setq map (vimpulse-modifier-map state mode))
          (vimpulse-define-key map key def)
          (viper-modify-major-mode mode state map))))
     (t
      (vimpulse-define-key basic-map key def)))
    (viper-set-mode-vars-for old-state)
    (viper-normalize-minor-mode-map-alist)))

(defun vimpulse-map-state-local (state key def)
  "Make a buffer-local binding for KEY and DEF in STATE.
Don't use this function directly; see `vimpulse-map-local',
`vimpulse-imap-local' and `vimpulse-vmap-local' instead."
  (viper-add-local-keys state `((,key . ,def))))

(defun vimpulse-map (key def &rest modes)
  "Modally bind KEY to DEF in vi (command) state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-map \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-map \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (vimpulse-map-state 'vi-state key def modes))

(defun vimpulse-imap (key def &rest modes)
  "Modally bind KEY to DEF in Insert state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-imap \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-imap \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (vimpulse-map-state 'insert-state key def modes))

(defun vimpulse-vmap (key def &rest modes)
  "Modally bind KEY to DEF in the Visual state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-vmap \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-vmap \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (vimpulse-map-state 'visual-state key def modes))

(defun vimpulse-omap (key def &rest modes)
  "Modally bind KEY to DEF in the Operator-Pending state.
The syntax is the same as that of `global-set-key', e.g.,

    (vimpulse-omap \"abc\" 'abc-command)

The optional MODES argument specifies which major modes the
binding is seen in:

    (vimpulse-omap \"abc\" 'abc-command 'lisp-mode 'text-mode)

Otherwise, the binding is universal, but has lower priority.
Pass t to MODES to create an universal binding with presedence
over mode-specific bindings."
  (vimpulse-map-state 'operator-state key def modes))

(defun vimpulse-map! (key def &rest modes)
  "Bind KEY to DEF in vi (command) state and the Visual state.
To bind in Insert state, use `vimpulse-imap'."
  (vimpulse-map key def modes)
  (vimpulse-vmap key def modes))

(defun vimpulse-map-local (key def)
  "Make a buffer-local binding of KEY to DEF in vi (command) state.
The syntax is the same as that of `local-set-key', e.g.,

    (vimpulse-map-local \"abc\" 'abc-command)

You would typically use this in a mode hook. To make a global
binding, use `vimpulse-map'."
  (vimpulse-map-state-local 'vi-state key def))

(defun vimpulse-imap-local (key def)
  "Make a buffer-local binding of KEY to DEF in Insert state.
The syntax is the same as that of `local-set-key', e.g.,

    (vimpulse-imap-local \"abc\" 'abc-command)

You would typically use this in a mode hook. To make a global
binding, use `vimpulse-imap'."
  (vimpulse-map-state-local 'insert-state key def))

(defun vimpulse-vmap-local (key def)
  "Make a buffer-local binding of KEY to DEF in the Visual state.
The syntax is the same as that of `local-set-key', e.g.,

    (vimpulse-vmap-local \"abc\" 'abc-command)

You would typically use this in a mode hook. To make a global
binding, use `vimpulse-vmap'."
  (vimpulse-map-state-local 'visual-state key def))

(defun vimpulse-omap-local (key def)
  "Make a buffer-local binding of KEY to DEF in the Operator-Pending state.
The syntax is the same as that of `local-set-key', e.g.,

    (vimpulse-omap-local \"abc\" 'abc-command)

You would typically use this in a mode hook. To make a global
binding, use `vimpulse-omap'."
  (vimpulse-map-state-local 'visual-state key def))

(provide 'vimpulse-modal)
