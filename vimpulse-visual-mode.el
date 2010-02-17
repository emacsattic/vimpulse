;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This file contains all the code relative to Visual mode.  ;;;;;
;;;; Visual mode is implemented as a Viper state.              ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Minor Mode code ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup vimpulse-visual nil
  "visual-mode for viper"
  :prefix "vimpulse-visual-"
  :group  'emulations)

(defcustom vimpulse-visual-basic-map (make-sparse-keymap)
  "Visual mode keymap.
This keymap is active when in visual mode."
  :type  'keymap
  :group 'vimpulse-visual)

(define-minor-mode vimpulse-visual-mode
  "Toggles visual mode in viper"
  :initial-value nil
  :keymap vimpulse-visual-basic-map
  :global nil
  :group 'vimpulse-visual
  (cond
   (vimpulse-visual-mode
    (unless (memq vimpulse-visual-mode '(normal line block))
      (vimpulse-visual-activate 'normal)))
   (t
    ;; This is executed when we do (vimpulse-visual-mode -1).
    ;; It must work even if Visual mode is not active.
    (vimpulse-visual-highlight -1)
    ;; Clean up local variables
    (mapcar (lambda (var)
              (when (assq var vimpulse-visual-vars-alist)
                (set var (cdr (assq var vimpulse-visual-vars-alist))))
              (when (memq var vimpulse-visual-global-vars)
                (kill-local-variable var)))
            vimpulse-visual-local-vars)
    ;; Deactivate mark
    (when vimpulse-visual-vars-alist
      (vimpulse-deactivate-mark t))
    (vimpulse-visual-transient-restore)
    (kill-local-variable 'vimpulse-visual-vars-alist)
    (kill-local-variable 'vimpulse-visual-global-vars)
    ;; If Viper state is not already changed,
    ;; change it to vi state
    (when (eq viper-current-state 'visual-state)
      (cond
       ((eq 'emacs-state vimpulse-visual-previous-state)
        (viper-change-state-to-emacs))
       (t
        (viper-change-state-to-vi))))
    (kill-local-variable 'vimpulse-visual-previous-state))))

;; These become minor modes when `vimpulse-add-visual-maps-macro'
;; is called below
(viper-deflocalvar
 vimpulse-visual-state-modifier-minor-mode nil
 "For making major mode-specific modifications to Visual state.")

(viper-deflocalvar
 vimpulse-visual-global-user-minor-mode nil
 "For user-defined global bindings in Visual state.")

(defvar vimpulse-visual-global-user-map (make-sparse-keymap)
  "Auxiliary map for user-defined bindings in Visual state.")

(defvar vimpulse-visual-state-modifier-alist nil)

(defvar vimpulse-visual-state-id "<VIS> "
  "Mode line tag for identifying Visual state.")

(defun vimpulse-region-face ()
  "Return face of region."
  (if (featurep 'xemacs) 'zmacs-region 'region))

(defvar vimpulse-visual-mode nil
  "Current visual mode: may be nil, `normal', `line' or `block'.")

(defcustom vimpulse-visual-load-hook nil
  "Hooks to run after loading vimpulse-visual-mode."
  :type 'hook
  :group 'vimpulse-visual)

(defcustom vimpulse-visual-mode-hook nil
  "This hook is run whenever vimpulse-visual-mode is toggled."
  :type 'hook
  :group 'vimpulse-visual)

(viper-deflocalvar
 vimpulse-visual-global-vars nil
 "List of variables which were global.")

(viper-deflocalvar
 vimpulse-visual-local-vars
 '(cua-mode
   mark-active
   transient-mark-mode
   zmacs-regions
   vimpulse-visual-region-changed)
 "System variables which are reset for each Visual session.")

(viper-deflocalvar
 vimpulse-visual-vars-alist nil
 "Alist of old variable values.")

(viper-deflocalvar
 vimpulse-visual-last nil
 "Last active Visual mode: may be nil, `normal', `line' or `block'.")

(viper-deflocalvar
 vimpulse-visual-previous-state 'viper-state
 "Previous state before enabling Visual mode.
This lets us revert to Emacs state in non-vi buffers.")

(viper-deflocalvar
 vimpulse-visual-region-changed nil
 "Whether region is expanded to Visual selection.")

(viper-deflocalvar
 vimpulse-visual-point nil
 "Last value of `point' in Visual mode.")

(viper-deflocalvar
 vimpulse-visual-mark nil
 "Last value of `mark' in Visual mode.")

(viper-deflocalvar
 vimpulse-undo-needs-adjust nil
 "If true, several commands in the undo-list should be connected.")

(defconst vimpulse-buffer-undo-list-mark 'vimpulse
  "Everything up to this mark is united in the undo-list.")

;; This variable holds the point and column of the first line
;; as well as the number of lines in the region
(defvar vimpulse-visual-insert-coords nil
  "A list with (I-COM UL-POS COL NLINES), where
I-COM is the insert command (?i, ?a, ?I or ?A),
UL-POS is the position of the upper left corner of the region,
COL is the column of insertion, and
NLINES is the number of lines in the region.")

(defun vimpulse-modifier-map (state &optional mode)
  "Return the current major mode modifier map for STATE.
If none, return an empty keymap (`viper-empty-keymap')."
  (setq mode (or mode major-mode))
  (setq state
        (cond
         ((eq state 'vi-state)
          viper-vi-state-modifier-alist)
         ((eq state 'insert-state)
          viper-insert-state-modifier-alist)
         ((eq state 'emacs-state)
          viper-emacs-state-modifier-alist)
         ((eq state 'visual-state)
          vimpulse-visual-state-modifier-alist)))
  (if (keymapp (cdr (assoc mode state)))
      (cdr (assoc mode state))
    viper-empty-keymap))

;; Adding Visual state maps. The advice for this gets somewhat
;; elaborate because Viper insists on making `minor-mode-map-alist'
;; buffer-local in XEmacs, so we need to set both the default value
;; and the local value.
(defmacro vimpulse-add-visual-maps-macro (keymaps)
  `(defadvice viper-normalize-minor-mode-map-alist
     (after ,keymaps activate)
     ,(format "Modifies `%s' to include visual keymaps." keymaps)
     (let (temp)
       (dolist (mode (list
                      (cons 'vimpulse-visual-mode
                            vimpulse-visual-basic-map)
                      (cons 'vimpulse-visual-state-modifier-minor-mode
                            (vimpulse-modifier-map 'visual-state))
                      (cons 'vimpulse-visual-global-user-minor-mode
                            vimpulse-visual-global-user-map)))
         (setq temp (default-value ',keymaps))
         (setq temp (assq-delete-all (car mode) temp)) ; already there?
         (add-to-list 'temp mode)
         (setq-default ,keymaps temp)
         (setq temp ,keymaps)
         (setq temp (assq-delete-all (car mode) temp))
         (add-to-list 'temp mode)
         (setq ,keymaps temp)))))

(cond
 ((featurep 'xemacs)
  (vimpulse-add-visual-maps-macro viper--key-maps)
  (vimpulse-add-visual-maps-macro minor-mode-map-alist))
 ((>= emacs-major-version 22)
  (vimpulse-add-visual-maps-macro viper--key-maps))
 (t
  (vimpulse-add-visual-maps-macro minor-mode-map-alist)))

(viper-normalize-minor-mode-map-alist)

(defadvice viper-refresh-mode-line (after vimpulse-states activate)
  "Add mode line tag for Visual state."
  (when (eq viper-current-state 'visual-state)
    (set (make-local-variable 'viper-mode-string)
         vimpulse-visual-state-id)
    (force-mode-line-update)))

(defadvice viper-change-state (around vimpulse-states activate)
  "Toggle Visual mode."
  (and (eq 'visual-state viper-current-state)
       (eq 'insert-state new-state)
       (viper-move-marker-locally 'viper-insert-point (point)))
  ad-do-it
  (cond
   ((eq 'visual-state new-state)
    (unless (memq vimpulse-visual-mode '(normal line block))
      (vimpulse-visual-mode 1)))
   (t
    (vimpulse-visual-mode -1)))
  (viper-normalize-minor-mode-map-alist))

(defadvice viper-set-mode-vars-for (after vimpulse-states activate)
  "Activate minor modes for Visual state."
  (cond
   ((eq state 'visual-state)
    (setq vimpulse-visual-mode (or vimpulse-visual-mode t)
          vimpulse-visual-global-user-minor-mode t
          vimpulse-visual-state-modifier-minor-mode t
          ;; The rest is vi (command) maps
          viper-vi-intercept-minor-mode t
          viper-vi-minibuffer-minor-mode
          (viper-is-in-minibuffer)
          viper-vi-local-user-minor-mode t
          viper-vi-kbd-minor-mode
          (not (viper-is-in-minibuffer))
          viper-vi-global-user-minor-mode t
          viper-vi-state-modifier-minor-mode t
          viper-vi-diehard-minor-mode
          (not
           (or viper-want-emacs-keys-in-vi
               (viper-is-in-minibuffer)))
          viper-vi-basic-minor-mode t
          viper-emacs-intercept-minor-mode nil
          viper-emacs-local-user-minor-mode nil
          viper-emacs-kbd-minor-mode nil
          viper-emacs-global-user-minor-mode nil
          viper-emacs-state-modifier-minor-mode nil))
   (t
    (setq vimpulse-visual-mode nil
          vimpulse-visual-global-user-minor-mode nil
          vimpulse-visual-state-modifier-minor-mode nil))))

(defadvice viper-modify-major-mode (after vimpulse-visual activate)
  "Modify Visual state."
  (when (eq state 'visual-state)
    (let ((alist 'vimpulse-visual-state-modifier-alist) elt)
      (when (setq elt (assoc mode (eval alist)))
        (set alist (delq elt (eval alist))))
      (set alist (cons (cons mode keymap) (eval alist)))
      (viper-normalize-minor-mode-map-alist)
      (viper-set-mode-vars-for viper-current-state))))

(defun vimpulse-filter-undos (undo-list)
  "Filters all `nil' marks from `undo-list' until the first
occurrence of `vimpulse-buffer-undo-list-mark'."
  (cond
   ((null undo-list)
    nil)
   ((eq (car undo-list) 'vimpulse)
    (cdr undo-list))
   ((null (car undo-list))
    (vimpulse-filter-undos (cdr undo-list)))
   (t
    (cons (car undo-list)
          (vimpulse-filter-undos (cdr undo-list))))))

(defun vimpulse-connect-undos ()
  "Connects all undo-steps from `buffer-undo-list' up to the
first occurrence of `vimpulse-buffer-undo-list-mark'."
  (when (and vimpulse-undo-needs-adjust
             (listp buffer-undo-list))
    (setq buffer-undo-list
          (vimpulse-filter-undos buffer-undo-list)))
  (setq vimpulse-undo-needs-adjust nil))

(defun vimpulse-push-buffer-undo-list-mark ()
  (setq vimpulse-undo-needs-adjust t)
  (push vimpulse-buffer-undo-list-mark buffer-undo-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions related to visual selection activation, ;;;
;;; mode of operation change (character-wise,         ;;;
;;; line-wise, block-wise)                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-activate (&optional mode)
  "Activate Visual mode. MODE is `normal', `line' or `block'.
May also be used to change the Visual mode."
  (unless (memq vimpulse-visual-mode '(normal line block))
    ;; We are activating Visual mode for the first time
    (kill-local-variable 'vimpulse-visual-vars-alist)
    (kill-local-variable 'vimpulse-visual-global-vars)
    (setq vimpulse-visual-previous-state viper-current-state)
    ;; Make global variables buffer-local
    (setq vimpulse-visual-vars-alist nil)
    (mapcar (lambda (var)
              (when (boundp var)
                (add-to-list 'vimpulse-visual-vars-alist
                             (cons var (eval var))))
              (unless (assoc var (buffer-local-variables))
                (make-local-variable var)
                (add-to-list 'vimpulse-visual-global-vars var)))
            vimpulse-visual-local-vars)
    ;; Re-add hooks in case they were cleared
    (add-hook 'pre-command-hook 'vimpulse-visual-pre-command)
    (add-hook 'post-command-hook 'vimpulse-visual-post-command)
    (if (featurep 'xemacs)
        (add-hook 'zmacs-deactivate-region-hook
                  'vimpulse-visual-deactivate-hook)
      (add-hook 'deactivate-mark-hook 'vimpulse-visual-deactivate-hook))
    ;; Remove nonsensical t value
    (and (boundp 'mark-active)
         (setq mark-active (vimpulse-mark-active)))
    ;; Activate mark at point
    (cond
     ((eq 'block mode)
      (set-mark (point))
      (vimpulse-deactivate-mark t)          ; `set-mark' activates the mark
      (vimpulse-transient-mark -1))
     (t
      (vimpulse-transient-mark 1)
      ;; Convert active Emacs region to Visual selection, if any.
      ;; To avoid confusion, do not move point, even if this means the
      ;; selection increases by one character when mark is before
      ;; point.
      (cond
       ((vimpulse-mark-active)
        (vimpulse-visual-contract-region)
        (setq vimpulse-visual-region-changed t))
       (t
        (vimpulse-activate-mark (point))))
      (vimpulse-visual-highlight))))
  ;; Set the Visual mode
  (setq mode (or mode 'normal))
  (setq vimpulse-visual-mode mode
        vimpulse-visual-last mode)
  (viper-change-state 'visual-state)
  (viper-restore-cursor-type)           ; use vi cursor
  ;; Reactivate mark
  (cond
   ((eq 'block mode)
    (vimpulse-deactivate-mark t)
    (vimpulse-transient-mark -1))
   (t
    (vimpulse-transient-mark 1)
    (vimpulse-activate-mark))))

(defalias 'vimpulse-change-state-to-visual 'vimpulse-visual-activate)

(defun vimpulse-visual-toggle (mode)
  "Activates visual-mode MODE if this is not the current visual mode.
Otherwise deactivates visual mode."
  (if (eq vimpulse-visual-mode mode)
      (vimpulse-visual-mode -1)
    (vimpulse-visual-activate mode)))

(defun vimpulse-visual-activate-normal ()
  "Activates normal visual mode."
  (interactive)
  (vimpulse-visual-activate 'normal)
  (message "-- VISUAL --"))

(defun vimpulse-visual-activate-line ()
  "Activates linewise visual mode."
  (interactive)
  (vimpulse-visual-activate 'line)
  (message "-- VISUAL LINE --"))

(defun vimpulse-visual-activate-block ()
  "Activates block visual mode."
  (interactive)
  (vimpulse-visual-activate 'block)
  (message "-- VISUAL BLOCK --"))

(defun vimpulse-visual-toggle-normal ()
  "Activates normal visual mode if it is not active.
Deactivates visual mode otherwise."
  (interactive)
  (vimpulse-visual-toggle 'normal)
  (when vimpulse-visual-mode
    (message "-- VISUAL --")))

(defun vimpulse-visual-toggle-line ()
  "Activates linewise visual mode if it is not active.
Deactivates visual mode otherwise."
  (interactive)
  (vimpulse-visual-toggle 'line)
  (when vimpulse-visual-mode
    (message "-- VISUAL LINE --")))

(defun vimpulse-visual-toggle-block ()
  "Activates block visual mode if it is not active.
Deactivates visual mode otherwise."
  (interactive)
  (vimpulse-visual-toggle 'block)
  (when vimpulse-visual-mode
    (message "-- VISUAL BLOCK --")))

;;;;;;;;;;;;;
;;; Lists ;;;
;;;;;;;;;;;;;

(defvar vimpulse-movement-cmds
  '(backward-char backward-list backward-paragraph backward-sentence
    backward-sexp backward-up-list backward-word beginning-of-buffer
    beginning-of-defun beginning-of-line beginning-of-visual-line
    down-list end-of-buffer end-of-defun end-of-line
    end-of-visual-line exchange-point-and-mark forward-char
    forward-list forward-paragraph forward-sentence forward-sexp
    forward-word move-beginning-of-line move-end-of-line next-line
    previous-line up-list vimpulse-goto-first-line viper-backward-Word
    viper-backward-char viper-backward-paragraph
    viper-backward-sentence viper-backward-word
    viper-beginning-of-line viper-end-of-Word viper-end-of-word
    viper-find-char-backward viper-find-char-forward
    viper-forward-Word viper-forward-char viper-forward-paragraph
    viper-forward-sentence viper-forward-word viper-goto-char-backward
    viper-goto-eol viper-goto-char-forward viper-goto-line
    viper-line-to-bottom viper-line-to-middle viper-line-to-top
    viper-next-line viper-previous-line viper-search-backward
    viper-search-forward viper-search-Next viper-search-next
    viper-window-bottom viper-window-middle viper-window-top
    vimpulse-visual-exchange-corners
    vimpulse-visual-select-text-object)
  "List of commands that move point.
If a command is listed here, or in `vimpulse-boundaries-cmds', or in
`vimpulse-misc-cmds', the region is not expanded to the visual selection
before executing it.")

(defvar vimpulse-boundaries-cmds
  '(mark-defun mark-end-of-sentence mark-paragraph mark-sexp mark-word)
  "List of commands that change boundaries of region.
If a command is listed here, or in `vimpulse-movement-cmds', or in
`vimpulse-misc-cmds', the region is not expanded to the visual selection
before executing it. It may, however, be adjusted afterwards.")

(defvar vimpulse-misc-cmds
  '(cua-cancel keyboard-quit scroll-down scroll-up undo
    viper-exec-mapped-kbd-macro viper-insert viper-intercept-ESC-key
    vimpulse-visual-toggle-normal vimpulse-visual-toggle-line
    vimpulse-visual-toggle-block vimpulse-visual-restore)
  "List of miscellaneous commands not acting on region.
If a command is listed here, or in `vimpulse-movement-cmds', or in
`vimpulse-boundaries-cmds', the region is not expanded to the visual selection
before executing it.")

(defun vimpulse-movement-cmd-p (command)
  "Whether COMMAND is a \"movement\" command.
That is, whether it is listed in `vimpulse-movement-cmds'."
  ;; We use `member' rather than `memq' to allow lambdas
  (member command vimpulse-movement-cmds))

(defun vimpulse-boundaries-cmd-p (command)
  "Whether COMMAND is a \"boundaries\" command.
 That is, whether it is listed in `vimpulse-boundaries-cmds'."
  (member command vimpulse-boundaries-cmds))

(defun vimpulse-misc-cmd-p (command)
  "Whether COMMAND is a \"misc\" command.
 That is, whether it is listed in `vimpulse-misc-cmds'."
  (member command vimpulse-misc-cmds))

(defun vimpulse-region-cmd-p (command)
  "Whether COMMAND may be acting on the contents of region."
  (and (not (vimpulse-movement-cmd-p command))
       (not (vimpulse-boundaries-cmd-p command))
       (not (vimpulse-misc-cmd-p command))))

;;;;;;;;;;;;;;;;;;;;
;;; Key bindings ;;;
;;;;;;;;;;;;;;;;;;;;

(define-key viper-vi-basic-map "v" 'vimpulse-visual-toggle-normal)
(define-key viper-vi-basic-map "V" 'vimpulse-visual-toggle-line)
(define-key viper-vi-basic-map "\C-v" 'vimpulse-visual-toggle-block)
(define-key viper-vi-basic-map "\C-p" 'yank-rectangle)
(define-key viper-vi-basic-map "gv" 'vimpulse-visual-restore)

(define-key vimpulse-visual-basic-map "v" 'vimpulse-visual-toggle-normal)
(define-key vimpulse-visual-basic-map "V" 'vimpulse-visual-toggle-line)
(define-key vimpulse-visual-basic-map "\C-v" 'vimpulse-visual-toggle-block)
(define-key vimpulse-visual-basic-map "d" 'vimpulse-visual-delete)
(define-key vimpulse-visual-basic-map "x" 'vimpulse-visual-delete)
(define-key vimpulse-visual-basic-map "D" 'vimpulse-visual-delete)
(define-key vimpulse-visual-basic-map "d" 'vimpulse-visual-delete)
(define-key vimpulse-visual-basic-map "y" 'vimpulse-visual-yank)
(define-key vimpulse-visual-basic-map "u" 'vimpulse-visual-mode)
(define-key vimpulse-visual-basic-map "R" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "r" 'vimpulse-visual-replace-region)
(define-key vimpulse-visual-basic-map "c" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "C" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "s" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "S" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "\"" 'vimpulse-visual-set-current-register)
(define-key vimpulse-visual-basic-map "o" 'exchange-point-and-mark)
(define-key vimpulse-visual-basic-map "O" 'vimpulse-visual-exchange-corners)
(define-key vimpulse-visual-basic-map "I" 'vimpulse-visual-insert)
(define-key vimpulse-visual-basic-map "A" 'vimpulse-visual-append)
(define-key vimpulse-visual-basic-map "U" 'vimpulse-visual-make-upcase)
(define-key vimpulse-visual-basic-map "u" 'vimpulse-visual-make-downcase)
(define-key vimpulse-visual-basic-map "~" 'vimpulse-visual-toggle-case)
(define-key vimpulse-visual-basic-map "J" 'vimpulse-visual-join)
(define-key vimpulse-visual-basic-map "<" 'vimpulse-visual-shift-left)
(define-key vimpulse-visual-basic-map ">" 'vimpulse-visual-shift-right)
(define-key vimpulse-visual-basic-map "=" 'indent-region)
(define-key vimpulse-visual-basic-map "a" 'vimpulse-visual-select-text-object)
(define-key vimpulse-visual-basic-map "i" 'vimpulse-visual-select-text-object)
;; Keys that have no effect in visual mode
(define-key vimpulse-visual-basic-map "." 'undefined)

;; Advice viper-intercept-ESC-key to exit visual mode with ESC
(defadvice viper-intercept-ESC-key
  (around vimpulse-ESC-exit-visual-mode activate)
  "Exit Visual mode with ESC."
  (let ((viper-ESC-moves-cursor-back (not (vimpulse-mark-active)))
        deactivate-mark)
    (if (and vimpulse-visual-mode
             (not (input-pending-p)))
        (vimpulse-visual-mode -1)
      ad-do-it)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual selection visualization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(viper-deflocalvar
 vimpulse-visual-overlay nil
 "Overlay for Visual selection.
In XEmacs, this is an extent.")

(viper-deflocalvar
 vimpulse-visual-block-overlays nil
 "Overlays for Visual Block selection.")

;; Set functions for handling overlays (not yet provided by Viper)
(cond
 ((featurep 'xemacs)                    ; XEmacs
  (fset 'vimpulse-delete-overlay 'delete-extent))
 (t                                     ; GNU Emacs
  (fset 'vimpulse-delete-overlay 'delete-overlay)))

(defun vimpulse-mark-active ()
  "Return t if mark is meaningfully active."
  (cond
   ((featurep 'xemacs)
    (region-exists-p))
   (t
    (and (boundp 'transient-mark-mode)
         transient-mark-mode
         (boundp 'mark-active)
         mark-active))))

(defun vimpulse-deactivate-mark (&optional now)
  "Don't deactivate mark in Visual mode."
  (cond
   ((and vimpulse-visual-mode
         (not (eq 'block vimpulse-visual-mode)))
    nil)
   ((and (boundp 'cua-mode) cua-mode)
    (cua--deactivate now))
   ((featurep 'xemacs)
    (let ((zmacs-region-active-p t))
      (zmacs-deactivate-region)))
   (now
    (setq mark-active nil))
   (t
    (setq deactivate-mark t))))

(fset 'viper-deactivate-mark 'vimpulse-deactivate-mark)

;; Complement to `vimpulse-deactivate-mark'
(defun vimpulse-activate-mark (&optional pos)
  "Activate mark if there is one. Otherwise set mark at point.
If POS if specified, set mark at POS instead."
  (setq pos (or pos (mark t) (point)))
  (cond
   ((and (boundp 'cua-mode) cua-mode)
    (let ((opoint (point))
          cua-toggle-set-mark)
      (goto-char (or pos (mark t) (point)))
      (cua-set-mark)
      (goto-char opoint)))
   (t
    (let (this-command)
      (push-mark pos t t)))))

(defun vimpulse-transient-mark (&optional arg)
  "Enable Transient Mark mode (and Cua mode) if not already enabled.
 Enable forcefully with positive ARG. Disable with negative ARG."
  (let (deactivate-mark)
    (cond
     ;; Disable Transient Mark/Cua
     ((and (integerp arg) (> 1 arg))
      (and (fboundp 'cua-mode)
           cua-mode
           (cua-mode -1))
      (and (fboundp 'transient-mark-mode)
           transient-mark-mode
           (transient-mark-mode -1))
      (and (boundp 'zmacs-regions)
           (setq zmacs-regions nil)))
     ;; Enable Transient Mark/Cua
     ((and (fboundp 'cua-mode)
           (vimpulse-visual-before (eq cua-mode t))
           (or (not cua-mode) (numberp arg)))
      (cua-mode 1))
     ((and (fboundp 'transient-mark-mode)
           (or (not transient-mark-mode) (numberp arg)))
      (transient-mark-mode 1))
     ((and (boundp 'zmacs-regions)
           (or (not zmacs-regions) (numberp arg)))
      (setq zmacs-regions t)))))

(defun vimpulse-visual-transient-restore ()
  "Restore Transient Mark mode to what is was before Visual mode.
 Also restores Cua mode."
  (when (boundp 'transient-mark-mode)
    (if (vimpulse-visual-before transient-mark-mode)
        (transient-mark-mode 1)
      (transient-mark-mode -1)))
  (when (boundp 'cua-mode)
    (if (vimpulse-visual-before cua-mode)
        (cua-mode 1)
      (cua-mode -1)))
  (when (boundp 'zmacs-regions)
    (let ((oldval (vimpulse-visual-before zmacs-regions)))
      (setq zmacs-regions oldval))))

(defmacro vimpulse-visual-before (&rest body)
  "Evaluate BODY with original system values from before Visual mode.
This is based on `vimpulse-visual-vars-alist'."
  `(let ,(mapcar (lambda (elt)
                   (list (car elt) `(quote ,(cdr elt))))
                 vimpulse-visual-vars-alist)
     ,@body))

(defun vimpulse-visual-beginning (&optional mode)
  "Return beginning of Visual selection,
based on `point', `mark' and `vimpulse-visual-mode'.
The Visual mode may be specified explicitly with MODE,
which must be one of `normal', `line' and `block'.

In Normal mode, return beginning of region.
In Line mode, return beginning of first line.
In Block mode, return upper left corner of rectangle.

See also `vimpulse-visual-end'."
  (setq mode (or mode vimpulse-visual-mode))
  (cond
   ;; Upper left corner of block selection
   ((eq 'block mode)
    (let* ((start (min (point) (or (mark t) 1)))
           (end   (max (point) (or (mark t) 1)))
           (start-col (save-excursion
                        (goto-char start)
                        (current-column)))
           (end-col   (save-excursion
                        (goto-char end)
                        (current-column))))
      (if (<= start-col end-col)
          start
        (save-excursion
          (goto-char start)
          (condition-case nil
              (move-to-column end-col)
            (error nil))
          (point)))))
   ;; Beginning of first line
   ((eq 'line mode)
    (cond
     ((not (mark t))
      (line-beginning-position))
     (t
      (save-excursion
        (goto-char (min (point) (mark t)))
        (line-beginning-position)))))
   ;; Beginning of region
   (t
    (min (point) (or (mark t) 1)))))

(defun vimpulse-visual-end (&optional mode)
  "Return end of Visual selection,
based on `point', `mark' and `vimpulse-visual-mode'.
The Visual mode may be specified explicitly with MODE,
which must be one of `normal', `line' and `block'.

In Normal mode, return end of region plus one character.
In Line mode, return end of last line, including newline.
In Block mode, return lower right corner of rectangle.

See also `vimpulse-visual-beginning'."
  (setq mode (or mode vimpulse-visual-mode))
  (cond
   ((eq 'block mode)
    ;; Lower right corner of block selection
    (let* ((start (min (point) (or (mark t) 1)))
           (end   (max (point) (or (mark t) 1)))
           (start-col (save-excursion
                        (goto-char start)
                        (current-column)))
           (end-col   (save-excursion
                        (goto-char end)
                        (current-column))))
      (if (<= start-col end-col)
          (1+ end)
        (save-excursion
          (goto-char end)
          (condition-case nil
              (move-to-column start-col)
            (error nil))
          (1+ (point))))))
   ;; End of last line (including newline)
   ((eq 'line mode)
    (cond
     ((not (mark t))
      (line-beginning-position 2))
     (t
      (save-excursion
        (goto-char (max (point) (mark t)))
        (line-beginning-position 2)))))
   ;; End of region plus one character
   (t
    (1+ (max (point) (or (mark t) 1))))))

(defun vimpulse-visual-select (beg end &optional widen)
  "Visually select text from BEG to END.
Return nil if selection is unchanged. If WIDEN is non-nil, only
modify selection if it does not already encompass BEG and END.

Under the hood, this function changes Emacs' `point' and `mark'.
The boundaries of the Visual selection are deduced from these and
the current Visual mode."
  (cond
   (widen
    (vimpulse-visual-select
     (min beg end (vimpulse-visual-beginning))
     (max beg end (vimpulse-visual-end))))
   (t
    (let ((opoint (point)) (omark (mark t)) mark-active)
      (cond
       ((< (point) (mark t))
        (goto-char (min beg end))
        ;; `vimpulse-visual-end' is always 1 larger than region's end
        ;; to ensure at least one character is selected. Therefore,
        ;; subtract 1 from region's end. However, make sure we don't
        ;; get END < BEG.
        (set-mark (max (min beg end)
                       (1- (max beg end)))))
       (t
        (set-mark (min beg end))
        (goto-char (max (min beg end)
                        (1- (max beg end))))))
      ;; Was selection changed?
      (not (and (eq opoint (point))
                (eq omark  (mark t))))))))

(defun vimpulse-visual-expand-region ()
  "Expand Emacs region to Visual selection."
  (let ((newpoint (vimpulse-visual-beginning))
        (newmark  (vimpulse-visual-end))
        mark-active)
    (when (< (or (mark t) 1) (point))
      (setq newpoint (prog1 newmark
                       (setq newmark newpoint))))
    (set-mark  newmark)
    (goto-char newpoint)))

(defun vimpulse-visual-contract-region ()
  "Opposite of `vimpulse-visual-expand-region'.
I.e., the resulting Visual selection is equivalent to the former
Emacs region."
  (vimpulse-visual-select (region-beginning)
                          (region-end)))

(defun vimpulse-visual-restore ()
  "Restore previous selection."
  (interactive)
  (cond
   ;; If no previous selection, try a quick C-x C-x
   ((or (not vimpulse-visual-point)
        (not vimpulse-visual-mark))
    (vimpulse-activate-mark nil)
    (vimpulse-visual-mode 1))
   (t
    (unless vimpulse-visual-mode
      (cond
       ((eq 'line vimpulse-visual-last)
        (vimpulse-visual-activate-line))
       ((eq 'block vimpulse-visual-last)
        (vimpulse-visual-activate-block))
       (t                               ; normal
        (vimpulse-visual-activate-normal))))
    (set-mark vimpulse-visual-mark)
    (goto-char vimpulse-visual-point)
    (vimpulse-visual-highlight))))

(defun vimpulse-visual-highlight (&optional arg)
  "Highlight Visual selection, depending on region and Visual mode.
With negative ARG, removes highlighting."
  (cond
   ((and (numberp arg) (> 1 arg))
    (when (viper-overlay-live-p vimpulse-visual-overlay)
      (vimpulse-delete-overlay vimpulse-visual-overlay))
    (mapcar 'vimpulse-delete-overlay vimpulse-visual-block-overlays)
    (setq vimpulse-visual-block-overlays nil))
   ((eq 'block vimpulse-visual-mode)
    ;; Remove any normal/line highlighting
    (when (viper-overlay-live-p vimpulse-visual-overlay)
      (vimpulse-delete-overlay vimpulse-visual-overlay))
    ;; Block highlighting isn't perfect
    (condition-case nil
        (vimpulse-visual-highlight-block
         (vimpulse-visual-beginning)
         (vimpulse-visual-end))
      (error nil)))
   (vimpulse-visual-mode                ; normal or line
    ;; Remove any block highlighting
    (mapcar 'vimpulse-delete-overlay vimpulse-visual-block-overlays)
    (setq vimpulse-visual-block-overlays nil)
    ;; Reuse overlay if possible
    (if (viper-overlay-live-p vimpulse-visual-overlay)
        (viper-move-overlay vimpulse-visual-overlay
                            (vimpulse-visual-beginning)
                            (vimpulse-visual-end))
      (setq vimpulse-visual-overlay
            (viper-make-overlay (vimpulse-visual-beginning)
                                (vimpulse-visual-end)))
      (viper-overlay-put vimpulse-visual-overlay
                         'face (vimpulse-region-face))
      (viper-overlay-put vimpulse-visual-overlay
                         'priority 99)))))

(defun vimpulse-visual-highlight-block (beg end)
  "Highlight rectangular region from BEG to END.
We do this by putting an overlay on each line within the
rectangle. Each overlay extends across all the columns of the
rectangle. We try to reuse overlays where possible because this
is more efficient and results in less flicker.

Adapted from: `rm-highlight-rectangle' in rect-mark.el."
  (save-excursion
    ;; Calculate the rectangular region represented by point and mark,
    ;; putting BEG in the north-west corner and END in the
    ;; south-east corner
    (let ((beg-col (save-excursion
                     (goto-char beg)
                     (current-column)))
          (end-col (save-excursion
                     (goto-char end)
                     (current-column))))
      (if (>= beg-col end-col)
          (setq beg-col (prog1
                            (1- end-col)
                          (setq end-col (1+ beg-col)))
                beg (save-excursion
                      (goto-char beg)
                      (move-to-column beg-col nil)
                      (point))
                end (save-excursion
                      (goto-char end)
                      (move-to-column end-col nil)
                      (point))))
      ;; Force a redisplay so we can do reliable window BEG/END
      ;; calculations
      (sit-for 0)
      (let* ((old vimpulse-visual-block-overlays)
             (new nil)
             overlay
             (window-beg (max (window-start) beg))
             (window-end (min (window-end) end))
             (nlines (count-lines window-beg
                                  (min window-end
                                       (point-max)))))
        ;; Iterate over those lines of the rectangle which are visible
        ;; in the currently selected window
        (goto-char window-beg)
        (dotimes (i nlines)
          (let ((row-beg (progn
                           (move-to-column beg-col nil)
                           (point)))
                (row-end (progn
                           (move-to-column end-col nil)
                           (min (point)
                                (line-end-position)))))
            ;; Trim old leading overlays
            (while (and old
                        (setq overlay (car old))
                        (< (viper-overlay-start overlay) row-beg)
                        (/= (viper-overlay-end overlay) row-end))
              (vimpulse-delete-overlay overlay)
              (setq old (cdr old)))
            ;; Reuse an overlay if possible, otherwise create one
            (if (and old
                     (setq overlay (car old))
                     (or (= (viper-overlay-start overlay) row-beg)
                         (= (viper-overlay-end overlay) row-end)))
                (progn
                  (viper-move-overlay overlay row-beg row-end)
                  (setq new (cons overlay new)
                        old (cdr old)))
              (setq overlay (viper-make-overlay row-beg row-end))
              (viper-overlay-put overlay 'face (vimpulse-region-face))
              (viper-overlay-put overlay 'priority 99)
              (setq new (cons overlay new))))
          (forward-line 1))
        ;; Trim old trailing overlays
        (mapcar 'vimpulse-delete-overlay old)
        (setq vimpulse-visual-block-overlays (nreverse new))))))

(defun vimpulse-visual-pre-command ()
  "Run before each command in Visual mode."
  (when vimpulse-visual-mode
    (viper-move-marker-locally 'vimpulse-visual-point (point))
    (viper-move-marker-locally 'vimpulse-visual-mark  (mark t))
    (set-register (viper-int-to-char (1+ (- ?y ?a)))
                  (vimpulse-visual-beginning))
    (set-register (viper-int-to-char (1+ (- ?z ?a)))
                  (vimpulse-visual-end))
    (cond
     ((eq 'insert-state viper-current-state)
      nil)
     ((eq 'exchange-point-and-mark this-command)
      (setq vimpulse-visual-region-changed nil))
     ((vimpulse-movement-cmd-p this-command)
      (setq vimpulse-visual-region-changed nil))
     (vimpulse-visual-region-changed
      (vimpulse-visual-expand-region))
     ((vimpulse-region-cmd-p this-command)
      (vimpulse-visual-expand-region)
      (setq vimpulse-visual-region-changed t)))))

(defun vimpulse-visual-post-command ()
  "Run after each command in Visual mode."
  (cond
   (vimpulse-visual-mode
    (cond
     (quit-flag                         ; C-g
      (vimpulse-visual-mode -1))
     ((eq 'keyboard-quit this-command)
      (vimpulse-visual-mode -1))
     ((and (not (vimpulse-mark-active))
           (not (eq 'block vimpulse-visual-mode)))
      (vimpulse-visual-mode -1))
     (t
      (cond
       ((eq 'block vimpulse-visual-mode)
        (when vimpulse-visual-region-changed
          (vimpulse-visual-restore)
          (setq vimpulse-visual-region-changed nil)))
       ((vimpulse-boundaries-cmd-p this-command)
        (vimpulse-visual-contract-region)
        (setq vimpulse-visual-region-changed t))
       (vimpulse-visual-region-changed
        (vimpulse-visual-restore)
        (setq vimpulse-visual-region-changed nil)))
      (vimpulse-visual-highlight))))
   ((and (vimpulse-mark-active)
         (eq 'vi-state viper-current-state)
         (if (boundp 'deactivate-mark) (not deactivate-mark) t))
    (vimpulse-visual-mode 1))))

(defun vimpulse-visual-deactivate-hook ()
  "Hook run when mark is deactivated in visual mode."
  (when vimpulse-visual-mode
    (and (not (vimpulse-mark-active))
         (vimpulse-region-cmd-p this-command)
         ;; (not (eq 'block vimpulse-visual-mode))
         (vimpulse-visual-mode -1))))

(add-hook 'pre-command-hook 'vimpulse-visual-pre-command)
(add-hook 'post-command-hook 'vimpulse-visual-post-command)
(if (featurep 'xemacs)
    (add-hook 'zmacs-deactivate-region-hook
              'vimpulse-visual-deactivate-hook)
  (add-hook 'deactivate-mark-hook 'vimpulse-visual-deactivate-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Destructive commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-delete (beg end &optional dont-save)
  "Kills the Visual selection to the kill-ring.
If DONT-SAVE is non-nil, just delete it."
  (interactive "r")
  (let ((length (- end beg)))
    (cond
     (dont-save
      (cond
       ((eq 'block vimpulse-visual-mode)
        (delete-rectangle beg end)
        (goto-char (min vimpulse-visual-point vimpulse-visual-mark)))
       (t
        (delete-region beg end)
        (goto-char beg)))
      (vimpulse-visual-mode -1))
     ((eq 'normal vimpulse-visual-mode)
      (viper-prefix-arg-com ?r 1 ?d)
      (viper-set-destructive-command
       (list 'viper-forward-char
             length ?d viper-use-register nil nil)))
     ((eq 'line vimpulse-visual-mode)
      (setq length (count-lines beg end))
      (goto-char (min vimpulse-visual-point vimpulse-visual-mark))
      (viper-line (cons length ?D)))
     ((eq 'block vimpulse-visual-mode)
      (kill-rectangle beg end)
      (goto-char (min vimpulse-visual-point vimpulse-visual-mark))
      (vimpulse-visual-mode -1)))))

(defun vimpulse-visual-change (beg end &optional dont-save)
  "Change the Visual selection to the kill-ring.
If DONT-SAVE is non-nil, just delete it."
  (interactive "r")
  (let ((length (- end beg))
        (mode vimpulse-visual-mode))
    (vimpulse-visual-delete beg end dont-save)
    (setq length (min length (- (buffer-size) (point))))
    (let (viper-d-com)
      (cond
       ((eq 'block mode)
        (viper-insert nil))
       ((eq 'line mode)
        (viper-Open-line nil))
       (t
        ;; If at last character on line, append
        (if (or (eolp) (save-excursion (forward-char) (eolp)))
            (viper-append nil)
          (viper-insert nil)))))
    (if (eq 'line mode)
        (setcar (nthcdr 2 viper-d-com) ?C)
      (setcar (nthcdr 1 viper-d-com) length)
      (setcar (nthcdr 2 viper-d-com) ?c))))

(defun vimpulse-visual-replace-region (beg end &optional arg)
  "Replace all selected characters with ARG."
  (interactive "r")
  (cond
   ((memq vimpulse-visual-mode '(normal line))
    (goto-char beg)
    (viper-replace-char arg)
    (let ((c (char-after (point))))
      (dotimes (i (- end beg))
        (cond
         ((member (char-after (point)) '(?\r ?\n))
          (forward-char))
         (t (delete-char 1)
            (insert c))))))
   ((eq 'block vimpulse-visual-mode)
    (goto-char beg)
    (viper-replace-char arg)
    (let* ((c (char-after (point)))
           (begin-col (current-column))
           (len (- (save-excursion
                     (goto-char end)
                     (current-column))
                   begin-col)))
      (while (< (point) end)
        (move-to-column begin-col nil)
        (let ((n 0))
          (while (and (< n len)
                      (not (member (char-after (point))
                                   '(?\r ?\n))))
            (delete-char 1)
            (insert c)
            (setq n (1+ n))))
        (forward-line))))
   (t
    (error "Viper not in Visual mode.")))
  (vimpulse-visual-mode -1)
  (goto-char beg))

;; These two functions implement insertion at the beginning/end
;; of a visual block or linewise selection
(defun vimpulse-visual-insert (beg end &optional arg)
  "Enter Insert state at beginning of Visual selection."
  (interactive "r\nP")
  (let (deactivate-mark)
    (cond
     ((eq 'normal vimpulse-visual-mode)
      (vimpulse-visual-mode -1)
      (viper-insert arg)
      (push-mark end t t)
      (goto-char beg))
     ((eq 'line vimpulse-visual-mode)
      (vimpulse-visual-mode -1)
      (push-mark (save-excursion
                   (goto-char end)
                   ;; Don't want trailing newline
                   (when (bolp) (backward-char))
                   (point))
                 t t)
      (goto-char beg)
      (viper-insert arg))
     ((eq 'block vimpulse-visual-mode)
      (vimpulse-visual-mode -1)
      (goto-char
       (vimpulse-visual-create-coords 'block ?i beg end))
      (viper-insert arg))
     (t
      (error "Viper not in Visual mode.")))))

(defun vimpulse-visual-append (beg end &optional arg)
  "Enter Insert state at end of Visual selection."
  (interactive "r\nP")
  (let (deactivate-mark)
    (cond
     ((eq 'normal vimpulse-visual-mode)
      (vimpulse-visual-mode -1)
      (viper-insert arg)
      (push-mark beg t t)
      (goto-char end))
     ((eq 'line vimpulse-visual-mode)
      (vimpulse-visual-mode -1)
      (push-mark beg t t)
      (goto-char end)
      ;; Don't want trailing newline
      (when (bolp) (backward-char))
      (viper-insert arg))
     ((eq 'block vimpulse-visual-mode)
      (vimpulse-visual-mode -1)
      (goto-char
       (vimpulse-visual-create-coords 'block ?a beg end))
      (viper-append arg))
     (t
      (error "Viper not in Visual mode.")))))

(defun vimpulse-visual-make-upcase (beg end)
  "Converts all selected characters to upper case."
  (interactive "r")
  (vimpulse-visual-change-case beg end 'upcase-region))

(defun vimpulse-visual-make-downcase (beg end)
  "Converts all selected characters to lower case."
  (interactive "r")
  (vimpulse-visual-change-case beg end 'downcase-region))

(defun vimpulse-visual-toggle-case (beg end)
  "Toggles the case of all selected characters."
  (interactive "r")
  (vimpulse-visual-change-case beg end 'vimpulse-visual-toggle-case-region))

(defun vimpulse-visual-change-case (beg end &optional case-func)
  (setq case-func (or case-func 'vimpulse-visual-toggle-case-region))
  (cond
   ((memq vimpulse-visual-mode '(normal line))
    (funcall case-func beg end))
   ((eq 'block vimpulse-visual-mode)
    (let ((begin-col (save-excursion
                       (goto-char beg)
                       (current-column)))
          (len  (- (save-excursion
                     (goto-char end)
                     (current-column))
                   (save-excursion
                     (goto-char beg)
                     (current-column)))))
      (goto-char beg)
      (while (< (point) end)
        (let ((from (save-excursion
                      (move-to-column begin-col nil)
                      (point)))
              (to (save-excursion
                    (move-to-column (+ begin-col len) nil)
                    (point))))
          (funcall case-func from to)
          (forward-line)))))
   (t
    (error "Viper not in Visual mode.")))
  (vimpulse-visual-mode -1)
  (goto-char beg))

(defun vimpulse-visual-toggle-case-region (beg end)
  "Toggles the case of all characters from BEG to END (exclusive)."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< beg end)
      (setq c (following-char))
      (delete-char 1 nil)
      (if (eq c (upcase c))
          (insert-char (downcase c) 1)
        (insert-char (upcase c) 1))
      (setq beg (1+ beg)))))

(defun vimpulse-visual-join (beg end)
  "Joins the selected lines."
  (interactive "r")
  (when vimpulse-visual-mode
    (vimpulse-visual-mode -1)
    (goto-char beg)
    (viper-join-lines (count-lines beg end))))

;; Currently, I don't know how to take the argument ARG
;; into the Repeat-command
(defun vimpulse-visual-shift-left (beg end &optional arg)
  "Shift all selected lines to the left."
  (interactive "r\nP")
  (setq arg (viper-p-val arg))
  (vimpulse-visual-mode -1)
  (vimpulse-push-buffer-undo-list-mark)
  (let ((nlines (1- (count-lines beg end))))
    (dotimes (i arg)
      (goto-char beg)
      (viper-next-line (cons nlines ?<)))
    (vimpulse-connect-undos)))

(defun vimpulse-visual-shift-right (beg end &optional arg)
  "Shift all selected lines to the right."
  (interactive "r\nP")
  (setq arg (viper-p-val arg))
  (vimpulse-visual-mode -1)
  (vimpulse-push-buffer-undo-list-mark)
  (let ((nlines (1- (count-lines beg end))))
    (dotimes (i (or arg 1))
      (goto-char beg)
      (viper-next-line (cons nlines ?>)))
    (vimpulse-connect-undos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intermediate commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-set-current-register ()
  (interactive)
  (setq viper-use-register (read-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-destructive commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-yank (beg end)
  "Save the Visual selection in the kill-ring."
  (interactive "r")
  (cond
   ((memq vimpulse-visual-mode '(normal line))
    (viper-prefix-arg-com ?r 1 ?y))
   ((eq 'block vimpulse-visual-mode)
    (kill-rectangle beg end)
    (goto-char beg)
    (yank-rectangle)
    ;; Associate the rectangle with the last entry in the kill-ring
    (put 'killed-rectangle 'previous-kill (current-kill 0)))
   (t
    (error "Viper not in Visual mode.")))
  (vimpulse-visual-mode -1)
  (goto-char beg))

(defun vimpulse-visual-exchange-corners ()
  "Rearrange corners in Visual Block mode.
For example, if mark is in the upper left corner and point in
the lower right, this function puts mark in the upper right
corner and point in the lower left."
  (interactive)
  (cond
   ((memq vimpulse-visual-mode '(normal line))
    (exchange-point-and-mark))
   ((eq 'block vimpulse-visual-mode)
    (let ((mark-col (save-excursion
                      (goto-char (mark t))
                      (current-column)))
          (point-col (current-column)))
      (set-mark (save-excursion
                  (goto-char (mark t))
                  (move-to-column point-col t)
                  (point)))
      (move-to-column mark-col t)))
   (t
    (error "Viper not in Visual mode."))))

(defun vimpulse-visual-select-text-object
  (count &optional char motion)
  "Visually select a text object, read from keyboard."
  (interactive "p")
  (let* ((char   (or char last-command-event))
         (motion (or motion (read-char)))
         (bounds (vimpulse-unify-multiple-bounds
                  (point) char count motion))
         (beg    (car bounds))
         (end    (cadr bounds)))
    (when (and beg end)
      (setq end (1+ end))
      (unless (vimpulse-visual-select beg end t)
        ;; We're stuck; move and try again
        (if (< (point) (mark t))
            (backward-char) (forward-char))
        (setq bounds (vimpulse-unify-multiple-bounds
                      (point) char count motion)
              beg (car bounds)
              end (cadr bounds))
        (when (and beg end)
          (vimpulse-visual-select beg end t)))
      (setq vimpulse-last-object-selection
            (list count char motion)))))

(defun vimpulse-widen-selection (beg end)
  "Widen visual selection to BEG and END.
When called interactively, derives BEG and END from
previous text object selection."
  (interactive
   (let ((count  (nth 0 vimpulse-last-object-selection))
         (char   (nth 1 vimpulse-last-object-selection))
         (motion (nth 2 vimpulse-last-object-selection)))
     (when vimpulse-last-object-selection
       (vimpulse-visual-select-text-object count char motion))
     '(nil nil)))                       ; that's it, we're done
  (cond
   ((or (not (numberp beg)) (not (numberp end)))
    nil)
   (t
    (vimpulse-visual-select beg end t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual Block Mode Support ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-create-coords
  (mode i-com upper-left lower-right)
  "Update the list of block insert coordinates with current rectangle.
I-COM should be ?c, ?i, ?a, ?I or ?A; the column for the
insertion will be chosen according to this command.
Returns the insertion point."
  (setq vimpulse-visual-insert-coords nil)
  (let ((nlines (count-lines upper-left lower-right))
        (col 0)) ; for ?I and ?A, trivial -- column is 0
    (when (or (eq i-com ?a) (eq i-com ?i) (eq i-com ?c))
      ;; For ?i and ?a, choose the left (the right) rectangle column
      (let ((beg-col (save-excursion
                       (goto-char upper-left)
                       (current-column)))
            (end-col (save-excursion
                       (goto-char lower-right)
                       (current-column))))
        ;; Decide if we use the left or right column
        (setq col (max 0 (if (or (eq i-com ?i) (eq i-com ?c))
                             beg-col
                           (1- end-col))))))
    ;; Save the information
    (setq vimpulse-visual-insert-coords
          (list mode i-com upper-left col nlines))
    (save-excursion
      (goto-char upper-left)
      (move-to-column col nil)
      (point))))

;; Redefinitions of Viper functions to handle Visual block selection,
;; that is, the "update all lines when we hit ESC" part.
;; This function is not in viper-functions-redefinitions.el
;; because its code is closely related to Visual mode.
(defun viper-exit-insert-state ()
  (interactive)
  (viper-change-state-to-vi)
  (when vimpulse-visual-insert-coords
    ;; Get the saved info about the visual region
    (let ((mode   (nth 0 vimpulse-visual-insert-coords))
          (i-com  (nth 1 vimpulse-visual-insert-coords))
          (pos    (nth 2 vimpulse-visual-insert-coords))
          (col    (nth 3 vimpulse-visual-insert-coords))
          (nlines (nth 4 vimpulse-visual-insert-coords)))
      (goto-char pos)
      (save-excursion
        (dotimes (i (1- nlines))
          (forward-line 1)
          (let ((cur-col (move-to-column col)))
            ;; If we are in block mode, this line, but do not hit the
            ;; correct column, we check if we should convert tabs
            ;; and/or append spaces
            (if (and (eq mode 'block)
                     (or (/= col cur-col) ; wrong column or
                         (eolp)))         ; end of line
                (cond ((< col cur-col)    ; we are inside a tab
                       (move-to-column (1+ col) 'fill) ; convert to spaces
                       (move-to-column col 'fill) ; this is needed for ?a
                       (viper-repeat nil))
                      ((and (>= col cur-col) ; we are behind the end
                            (eq i-com ?a))   ; and I-COM is ?a
                       (move-to-column (1+ col) t) ; append spaces
                       (viper-repeat nil)))
              (viper-repeat nil)))))
      (setq vimpulse-visual-insert-coords nil)))
  ;; Update undo-list
  (vimpulse-connect-undos))

(defadvice viper-Put-back (around vimpulse-visual activate)
  "Delete selection before pasting in Visual mode."
  (let (inserted-text replaced-text mode)
    (setq yank-window-start (window-start))
    (cond
     (vimpulse-visual-mode
      (setq mode vimpulse-visual-mode)
      (unless (eq 'block mode)
        ;; Add replaced text to the kill-ring before the current kill
        (setq inserted-text (current-kill 0))
        (setq replaced-text
              (buffer-substring (region-beginning) (region-end)))
        (kill-new replaced-text t)
        (kill-new inserted-text))
      (vimpulse-visual-delete (region-beginning) (region-end) t)
      (and (eq 'normal mode)
           (viper-end-with-a-newline-p inserted-text)
           (newline)))
     ((vimpulse-mark-active)
      (delete-region (region-beginning) (region-end))))
    (if (and killed-rectangle
             (eq (current-kill 0)
                 (get 'killed-rectangle 'previous-kill)))
        (yank-rectangle)
      ad-do-it)))

(defadvice viper-put-back (around vimpulse-visual activate)
  "Delete selection before pasting in Visual mode."
  (setq yank-window-start (window-start))
  (cond
   (vimpulse-visual-mode
    (viper-Put-back arg))
   ((vimpulse-mark-active)
    (viper-Put-back arg))
   (t
    (if (and killed-rectangle
             (eq (current-kill 0)
                 (get 'killed-rectangle 'previous-kill)))
        (yank-rectangle)
      ad-do-it))))

;; Viper's larger movement commands use the mark to store the previous
;; position, which is fine and useful when the mark isn't active. When
;; it is, however, it has the effect of remaking the whole region.
(defadvice push-mark (around vimpulse-visual-mode activate)
  (unless (and vimpulse-visual-mode
               ;; Note: if you really need to call `push-mark'
               ;; in proximity with the commands below (i.e., in a hook),
               ;; do (let (this-command) (push-mark)).
               (memq this-command
                     '(vimpulse-goto-first-line
                       viper-backward-paragraph
                       viper-backward-sentence
                       viper-forward-paragraph
                       viper-forward-sentence
                       viper-goto-line
                       viper-window-bottom
                       viper-window-middle
                       viper-window-top)))
    ad-do-it))

(provide 'vimpulse-visual-mode)

