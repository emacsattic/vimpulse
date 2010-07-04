;;;; Modal keybinding functions

;; This provides the functions `vimpulse-map', `vimpulse-imap',
;; `vimpulse-vmap' and `vimpulse-omap', which mimic :map, :imap, :vmap
;; and :omap in Vim, as well as `vimpulse-make-careful-binding', which
;; makes bindings "on top of" previous bindings.
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
;; `vimpulse-make-careful-binding', a general function for specifying
;; the keymap. Returning to the example:
;;
;;     (vimpulse-imap "aa" 'foo)
;;     (vimpulse-imap "aaa" 'bar)
;;
;; This will bind "aaa" to `bar', and "aa" + any other key to `foo'.
;; The syntax is the same as that of `global-set-key'. The key
;; sequence may be specified as a string, like above, as a vector
;; (like [?a ?b ?c]), or as a call to `kbd' (like (kbd "a b c")).
;;
;; To make a binding in vi (command) mode, use `vimpulse-map'; in
;; Insert mode, `vimpulse-imap'; in Visual mode, `vimpulse-vmap'; in
;; Operator-Pending mode, `vimpulse-omap'. The more general
;; `vimpulse-make-careful-binding' function lets one specify the
;; keymap to store the binding in, as when using `define-key':
;;
;;     (vimpulse-make-careful-binding keymap "abc" 'command)
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
;; What is done by functions like `vimpulse-make-careful-binding' and
;; `vimpulse-map' (which depends on the former) is to generate these
;; default bindings automatically. If "AB" is already bound to `foo'
;; and we carefully bind "ABC" to `bar', the old binding is first
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
;; to carefully bind, say, "dq" to a custom command `foo' of our own?
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
;; old binding it replaces. This is done by
;; `vimpulse-careful-pre-hook', which unreads "w" (so it can be read
;; again) and changes `last-command-event' to "d". Of course, this
;; behavior is only needed for default key bindings, and only for
;; default key bindings made by careful bindings. To that end, every
;; time `vimpulse-make-careful-binding' makes a default binding, the
;; binding is listed in `vimpulse-careful-alist' for future reference.
;; Checking against the list, `vimpulse-careful-pre-hook' only does
;; its thing if the current binding comes back positive.
;;
;; XEmacs is somewhat fuzzy about its command loop variables, not
;; allowing direct modification of `last-command-event'. However,
;; shadowing it with a `let' binding is possible, and a wrap-around
;; advice of the current command is employed to accomplish this. Also,
;; XEmacs does not have default key bindings in quite the same way as
;; GNU Emacs; `vimpulse-default-binding' takes care of the
;; differences.
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
;; `let' binding.
(defmacro vimpulse-advice-command (command)
  "Make wrap-around advice for shadowing `last-command-event'.
XEmacs does not allow us to change its command loop variables
directly, but shadowing them with a `let' binding works."
  `(defadvice ,command (around vimpulse-careful activate)
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

(defun vimpulse-careful-check (key-sequence)
  "Return t if KEY-SEQUENCE defaults to `this-command',
but only for bindings listed in `vimpulse-careful-alist'."
  (let ((temp-sequence (vimpulse-strip-prefix key-sequence)))
    (setq temp-sequence (vimpulse-truncate temp-sequence -1))
    (and this-command ; may be nil
         (not (key-binding key-sequence)) ; only default bindings
         (eq (cdr (assoc temp-sequence vimpulse-careful-alist))
             this-command))))

(defun vimpulse-careful-remove (key-vector &optional recursive)
  "Delete entry with KEY-VECTOR from `vimpulse-careful-alist'.
If RECURSIVE is non-nil, also delete entries whose key-vectors
start with KEY-VECTOR."
  (if recursive
      (dolist (entry vimpulse-careful-alist)
        (when (equal (vimpulse-truncate (car entry)
                                        (length key-vector))
                     key-vector)
          (setq vimpulse-careful-alist
                (delq entry vimpulse-careful-alist))))
    (setq vimpulse-careful-alist
          (assq-delete-all key-vector vimpulse-careful-alist))))

(defun vimpulse-xemacs-def-binding
  (keymap key def &optional careful-binding define-func)
  "Make a default binding in XEmacs. If CAREFUL-BINDING is
non-nil, advice DEF by means of `vimpulse-advice-command'."
  (let ((temp-sequence (vconcat key))
        (submap (lookup-key keymap key)))
    (unless define-func (setq define-func 'define-key))
    (and careful-binding (commandp def)
         (eval `(vimpulse-advice-command ,def)))
    (and (> (length temp-sequence) 1)
         (eq (aref temp-sequence (1- (length temp-sequence))) t)
         (setq temp-sequence (vimpulse-truncate temp-sequence -1)))
    ;; The following is from
    ;; http://tracker.xemacs.org/XEmacs/its/msg2021.
    (unless (keymapp submap)
      (setq submap (make-sparse-keymap)))
    (when (fboundp 'set-keymap-default-binding)
      (set-keymap-default-binding submap def))
    (funcall define-func keymap temp-sequence submap)))

(defun vimpulse-default-binding
  (keymap key def &optional careful-binding define-func)
  "Make a default binding in GNU Emacs or XEmacs,
whichever is appropriate. If CAREFUL-BINDING is non-nil,
the binding is listed in `vimpulse-careful-alist'."
  (let ((temp-sequence (vconcat key)))
    (unless define-func (setq define-func 'define-key))
    (cond
     ((featurep 'xemacs)
      (vimpulse-xemacs-def-binding
       keymap temp-sequence def careful-binding define-func))
     (t
      (unless (eq (aref temp-sequence (1- (length temp-sequence))) t)
        (setq temp-sequence (vconcat temp-sequence [t])))
      (funcall define-func keymap temp-sequence def)))
    (when careful-binding
      (add-to-list 'vimpulse-careful-alist
                   (cons (vimpulse-truncate temp-sequence -1) def)))))

;;; Hook run before each command

;; If the current command is a default key binding made by
;; `vimpulse-make-careful-binding', we need to unread the last input
;; events and change some command loop variables to give the command
;; the impression of its "old" binding.
(defun vimpulse-careful-pre-hook ()
  "Update `vimpulse-last-command-event' and `unread-command-events'.
If the current key-sequence defaults to a shorter key-sequence,
the difference is stored in these two variables, to be passed on
via the `last-command-event' variable and the `read-char'
functions, respectively."
  (setq vimpulse-last-command-event nil)
  (let ((key-sequence (vconcat (this-command-keys))))
    ;; If XEmacs, get rid of the event object type.
    (when (featurep 'xemacs)
      (setq key-sequence (events-to-keys key-sequence)))
    (while (and (> (length key-sequence) 1)
                (vimpulse-careful-check key-sequence))
      ;; Unread last event.
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

;;; Hook run after each command

;; This merely ensures `vimpulse-last-command-event' is reset.
(defun vimpulse-careful-post-hook ()
  "Erase `vimpulse-last-command-event'."
  (setq vimpulse-last-command-event nil))

(add-hook 'pre-command-hook  'vimpulse-careful-pre-hook)
(add-hook 'post-command-hook 'vimpulse-careful-post-hook)

;;; Modal binding functions

;; `vimpulse-make-careful-binding' is general; `vimpulse-map',
;; `vimpulse-imap', `vimpulse-vmap' and `vimpulse-omap' imitate Vim's
;; :map, :imap, :vmap and :omap, respectively.
(defun vimpulse-make-careful-binding
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
gets listed in `vimpulse-careful-alist', so that, with regard to
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
     ;; nil unbinds the key-sequence.
     ((not def)
      (funcall define-func keymap key-vector def)
      (while (and (> (length key-vector) 1)
                  (not (lookup-key keymap key-vector)))
        (vimpulse-careful-remove key-vector t)
        (setq key-vector (vimpulse-truncate key-vector -1))))
     ;; `undefined' also unbinds, but less forcefully.
     ((eq def 'undefined)
      (if (keymapp (lookup-key keymap key-vector))
          (vimpulse-default-binding keymap key-vector nil t define-func)
        (funcall define-func keymap key-vector def))
      (vimpulse-careful-remove key-vector))
     ;; Regular binding: convert previous bindings to default bindings.
     (t
      (dotimes (i (1- (length key-vector)))
        (setq temp-sequence (vimpulse-truncate key-vector (1+ i)))
        (setq current-binding (lookup-key keymap temp-sequence t))
        (when (or (numberp current-binding) (not current-binding))
          (setq current-binding
                (or (key-binding temp-sequence t) previous-binding)))
        (setq previous-binding current-binding)
        ;; If `current-binding' is a keymap, do nothing, since our
        ;; careful binding can exist happily as part of that keymap.
        ;; However, if `current-binding' is a command, we need to make
        ;; room for the careful binding by creating a default binding.
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

(define-minor-mode vimpulse-careful-minor-mode
  "Minor mode of bindings overwritten by `vimpulse-map' et al."
  :keymap vimpulse-careful-map
  (dolist (entry vimpulse-careful-alist)
    (unless (lookup-key vimpulse-careful-map (car entry))
      (define-key vimpulse-careful-map (car entry) (cdr entry))))
  (when vimpulse-careful-minor-mode
    (viper-normalize-minor-mode-map-alist)))

(add-to-list 'vimpulse-state-maps-alist
             (cons 'vimpulse-careful-minor-mode 'vimpulse-careful-map))

(defun vimpulse-define-key (mode state key def &optional careful)
  "Modally bind KEY to DEF in STATE for MODE.
MODE is an Emacs mode (minor or major), while STATE is one of
`vi-state', `insert-state', `visual-state' or `operator-state'.
For example:

    (vimpulse-define-key 'text-mode 'vi-state \"a\" 'foo)
    (vimpulse-define-key 'visual-line-mode 'visual-state \"b\" 'bar)

If CAREFUL is non-nil, make a careful binding with
`vimpulse-make-careful-binding'."
  (let* ((entry (cdr (assq state vimpulse-auxiliary-modes-alist)))
         (aux   (cdr (assq mode (symbol-value entry))))
         (map   (eval (cdr (assq aux vimpulse-state-maps-alist)))))
    ;; If no auxiliary mode exists, create one.
    (unless (keymapp map)
      (setq aux (intern (format "vimpulse-%s-%s" state mode))
            map (intern (format "vimpulse-%s-%s-map" state mode)))
      (eval `(viper-deflocalvar ,aux nil
               ,(format "Auxiliary %s mode for `%s'." state mode)))
      (eval `(defvar ,map (make-sparse-keymap)
               ,(format "Auxiliary %s keymap for `%s'." state mode)))
      (add-to-list 'vimpulse-state-maps-alist (cons aux map) t)
      (add-to-list entry (cons mode aux) t)
      (setq map (eval map)))
    ;; Define key.
    (if careful
        (vimpulse-with-state state
          (vimpulse-make-careful-binding map key def))
      (define-key map key def))))

;; This modifies the major mode extension keymap, i.e., it's
;; a reuseable front-end to `viper-modify-major-mode'.
;; (By itself, `viper-modify-major-mode' discards the previous keymap.)
(defun vimpulse-define-major-key (mode state key def &optional careful)
  "Modally bind KEY to DEF in STATE for major mode MODE.
STATE is one of `vi-state', `insert-state', `visual-state' or
`operator-state'. If CAREFUL is non-nil, make a careful binding
with `vimpulse-make-careful-binding'."
  (let ((modifier-map (vimpulse-modifier-map state mode)))
    (if careful
        (vimpulse-with-state state
          (vimpulse-make-careful-binding modifier-map key def))
      (define-key modifier-map key def))
    (viper-modify-major-mode mode state modifier-map)))

(defalias 'vimpulse-define-minor-key 'vimpulse-define-key)

(defun vimpulse-global-set-key (state key def &optional careful)
  "Modally bind KEY to DEF in STATE.
STATE is one of `vi-state', `insert-state', `visual-state' or `operator-state'.
If CAREFUL is non-nil, don't overwrite previous bindings."
  (let* ((map (cdr (assq state vimpulse-state-vars-alist)))
         (global-user-map (eval (cdr (assq 'global-user-map map)))))
    (if careful
        (vimpulse-with-state state
          (vimpulse-make-careful-binding global-user-map key def))
      (define-key global-user-map key def))))

(defun vimpulse-local-set-key (state key def)
  "Modally bind KEY to DEF in STATE, locally.
STATE is one of `vi-state', `insert-state', `visual-state' or `operator-state'."
  (viper-add-local-keys state `((,key . ,def))))

(defun vimpulse-map-state (state key def &optional modes)
  "Modally bind KEY to DEF in STATE.
Don't use this function directly; see `vimpulse-map',
`vimpulse-imap', `vimpulse-vmap' and `vimpulse-omap' instead."
  (let* ((map (cdr (assq state vimpulse-state-vars-alist)))
         (basic-map (eval (cdr (assq 'basic-map map)))))
    (if modes
        (dolist (mode modes)
          (if (eq mode t)
              (vimpulse-global-set-key 'vi-state key def t)
            (vimpulse-define-major-key mode 'vi-state key def t)))
      (vimpulse-with-state state
        (vimpulse-make-careful-binding basic-map key def)))))

(defalias 'vimpulse-map-state-local 'vimpulse-local-set-key)

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
