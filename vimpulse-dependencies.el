;;; Code:

(defconst vimpulse-version "0.4+git"
  "The current version of Vimpulse")

(defun vimpulse-version ()
  (interactive)
  (message "Vimpulse version is %s" vimpulse-version))

;; Load Viper
(defvar viper-mode t)
(defvar viper-inhibit-startup-message t)
(defvar viper-expert-level 5)
(defvar viper-want-ctl-h-help t)
(require 'viper)

;; Load undo-tree.el if available, with redo.el as fall-back
(unless (featurep 'undo-tree)
  (condition-case nil
      (require 'undo-tree)
    (error (condition-case nil
               (require 'redo)
             (error nil)))))
(and (fboundp 'global-undo-tree-mode)
     (global-undo-tree-mode 1))

;;; Customization group for Vimpulse

(defgroup vimpulse nil
  "Vim emulation within Emacs."
  :group  'emulations
  :link   '(custom-group-link "viper")
  :prefix 'vimpulse-)

(defcustom vimpulse-experimental t
  "Whether or not to use experimental features.
Turned on by default, so you will give feedback :P"
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-change-state nil
  "Whether commands like \"cw\" invoke Replace state, vi-like.
The default is to delete the text and enter Insert state,
like in Vim."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-C-u-like-Vim nil
  "Whether C-u scrolls like in Vim, off by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-enhanced-paren-matching t
  "Enhanced matching of parentheses, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-operator-pending-cursor t
  "Whether the cursor changes in Operator-Pending mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-visual-block-untabify nil
  "Whether Block mode may change tabs to spaces for fine movement.
Off by default."
  :type  'boolean
  :group 'vimpulse-visual)

(defcustom vimpulse-incremental-search t
  "Use isearch for / and ?, on by default.")

(defcustom vimpulse-flash-delay 2
  "Number of seconds to flash search matches.")

(defvar vimpulse-flash-timer nil
  "Timer for flashing search results.")

(defcustom vimpulse-want-vi-keys-in-apropos t
  "Whether to use vi keys in Apropos mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-vi-keys-in-buffmenu t
  "Whether to use vi keys in Buffer menu, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-vi-keys-in-dired t
  "Whether to use vi keys in Dired mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-vi-keys-in-Info t
  "Whether to use vi keys in Info mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-want-vi-keys-in-help t
  "Whether to use vi keys in Help mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(defcustom vimpulse-fold-level 0
  "Default fold level."
  :type  'integer
  :group 'vimpulse)

;; The secrets discovered from untold diggings among
;; the ruins of Customize code
(defun vimpulse-custom-value-p (symbol)
  "Non-nil if SYMBOL has a customized value."
  (or (get symbol 'customized-value)
      (get symbol 'customized-face)
      (get symbol 'saved-value)))

(defmacro vimpulse-setq-custom (sym val &rest body)
  "Set the customized value of SYM to VAL."
  `(progn
     (prog1 (setq ,sym ,val)            ; return VAL
       (when (get ',sym 'custom-autoload)
         (custom-load-symbol ',sym))
       (put ',sym 'customized-value (list (custom-quote ,val))))
     ,@(when body
         `((vimpulse-setq-custom ,@body)))))

(defmacro vimpulse-setq-custom-default (symbol value &rest body)
  "Set the customized default value of SYMBOL to VALUE."
  `(progn
     (prog1 ,value                       ; return VALUE
       (when (get ',symbol 'custom-autoload)
         (custom-load-symbol ',symbol))
       (put ',symbol 'standard-value (list (custom-quote ,value))))
     ,@(when body
         `((vimpulse-setq-custom-default ,@body)))))

(defmacro vimpulse-setq (sym val &rest body)
  "Set SYM to VAL, defaults included, unless SYM is customized.
SYM is unquoted. Returns VAL."
  `(progn
     (cond
      ;; Customized value: just set custom standard value
      ((vimpulse-custom-value-p ',sym)
       (vimpulse-setq-custom-default ,sym ,val))
      ;; Customized variable: set custom and regular values
      ((custom-variable-p ',sym)
       (vimpulse-setq-custom-default ,sym ,val)
       (vimpulse-setq-custom ,sym ,val)
       (setq-default ,sym ,val)
       (setq ,sym ,val))
      ;; Regular variable; set default and local values
      (t
       (setq-default ,sym ,val)
       (setq ,sym ,val)))
     ,@(when body
         `((vimpulse-setq ,@body)))))

;;; Initialize variables

(defvar dabbrev--last-abbrev-location)
(defvar dabbrev--last-abbreviation)
(defvar dabbrev--last-direction)
(defvar dabbrev--last-expansion)
(defvar dabbrev--last-expansion-location)
(defvar isearch-forward)
(defvar isearch-lazy-highlight-end)
(defvar isearch-lazy-highlight-last-string)
(defvar isearch-lazy-highlight-start)
(defvar isearch-lazy-highlight-wrapped)
(defvar isearch-regexp)
(defvar isearch-string)
(defvar killed-rectangle nil)
(defvar show-paren-delay)
(defvar undo-tree-visualizer-map)
(defvar woman-use-own-frame)
(defvar woman-use-topic-at-point)

(defvar ex-token-alist)
(defvar viper-mode-string)

(defvar vimpulse-viper-movement-cmds
  '(viper-backward-Word viper-backward-char viper-backward-paragraph
    viper-backward-sentence viper-backward-word
    viper-beginning-of-line viper-command-argument
    viper-digit-argument viper-end-of-Word viper-end-of-word
    viper-exec-mapped-kbd-macro viper-find-char-backward
    viper-find-char-forward viper-forward-Word viper-forward-char
    viper-forward-paragraph viper-forward-sentence viper-forward-word
    viper-goto-char-backward viper-goto-char-forward viper-goto-eol
    viper-goto-line viper-line-to-bottom viper-line-to-middle
    viper-line-to-top viper-next-line viper-previous-line
    viper-scroll-down-one viper-scroll-down viper-scroll-up
    viper-scroll-up-one viper-window-bottom viper-window-middle
    viper-window-top vimpulse-end-of-previous-word
    vimpulse-goto-first-line vimpulse-goto-definition
    vimpulse-goto-line vimpulse-search-backward-for-symbol-at-point
    vimpulse-search-forward-for-symbol-at-point vimpulse-jump-backward
    vimpulse-jump-forward vimpulse-visual-toggle-normal
    vimpulse-visual-toggle-line vimpulse-visual-toggle-block)
  "List of Viper/Vimpulse movement commands.")

(defvar vimpulse-core-movement-cmds
  '(viper-backward-char
    viper-next-line
    viper-previous-line
    viper-forward-char
    viper-ex)
  "List of Viper \"core\" movement commands.
These should be present in every mode, to avoid confusion.")

(viper-deflocalvar vimpulse-mark-list nil
  "List of mark positions to jump to with `vimpulse-jump-forward'.
 They are stored as markers, the current position first:

     (car vimpulse-mark-list)  = current position (last popped)
     (cdr vimpulse-mark-list)  = future positions (previously popped)
     (cadr vimpulse-mark-list) = next position (to jump to)

 In other words, a sort of \"reverse mark ring\": marks that are
 popped off the mark ring, are collected here.")

(viper-deflocalvar vimpulse-local-marks-alist nil
  "Association list of local marks.
Entries have the form (CHAR (FILE . POS)) where POS is a marker
or a character position.")

(defvar vimpulse-global-marks-alist nil
  "Association list of global marks.
Entries have the form (CHAR (FILE . POS)) where POS is a marker
or a character position.")

(viper-deflocalvar vimpulse-replace-alist nil
  "Alist of characters overwritten in Replace mode.
 Used by `vimpulse-replace-backspace' to restore text.
 The format is (POS . CHAR).")

(viper-deflocalvar vimpulse-exit-point nil
  "Like `viper-insert-point', but when exiting Insert mode.")

(defvar vimpulse-last-command-event nil
  "Value for overwriting `last-command-event'.
Used by `vimpulse-modal-pre-hook'.")

(defvar vimpulse-modal-alist nil
  "Key bindings for which `vimpulse-modal-pre-hook' is active.
That is, `last-command-event' and `read-char' work differently
for these bindings. The format is (KEY-VECTOR . COMMAND).")

(defvar vimpulse-modal-map (make-sparse-keymap)
  "Keymap of bindings overwritten by `vimpulse-map' et al.")

(defvar vimpulse-paren-overlay-open nil
  "Overlay used to highlight the opening paren.")

(defvar vimpulse-paren-overlay-close nil
  "Overlay used to highlight the closing paren.")

(defvar vimpulse-operator-basic-map)

(defvar vimpulse-operator-remap-map (make-sparse-keymap))

(defvar vimpulse-operator-remap-alist nil
  "Association list of command remappings in Operator-Pending mode.")

(defvar vimpulse-this-operator nil
  "Current operator.
In general, motions and operators are orthogonal, with some exceptions:
\"cw\" and \"dw\" work on slightly different ranges, for example.
Motions can check this variable if they need to know what
operator receives their range. See also `vimpulse-this-motion'.")

(defvar vimpulse-this-motion nil
  "Current motion.
In general, motions and operators are orthogonal, with some exceptions:
\"cc\" may indent the current line while \"cw\" may not, for example.
Operators may check this variable if they need to know what
motion produced the current range. See also `vimpulse-this-operator'.")

(defvar vimpulse-this-count nil
  "Current count (operator count times motion count).")

(defvar vimpulse-this-motion-type nil
  "Current motion type.
May be `block', `line', `inclusive', `exclusive' or nil.")

(defvar vimpulse-last-motion-type nil
  "Last repeated range type.
May be `block', `line', `inclusive', `exclusive' or nil.")

(defvar vimpulse-last-operator nil
  "Last repeated operator.
Used by `vimpulse-operator-repeat'.")

(defvar vimpulse-last-motion nil
  "Last repeated motion.
Used by `vimpulse-operator-repeat'.")

(defvar vimpulse-visual-basic-map)

(defvar vimpulse-visual-remap-alist nil
  "Association list of command remappings in Visual mode.")

(put 'vimpulse-visual-basic-map
     'remap-alist 'vimpulse-visual-remap-alist)

(viper-deflocalvar vimpulse-visual-mode nil
  "Current Visual mode: may be nil, `normal', `line' or `block'.")

(viper-deflocalvar vimpulse-visual-global-vars nil
  "List of variables that were global.")

(viper-deflocalvar vimpulse-visual-local-vars
  '(cua-mode
    mark-active
    transient-mark-mode
    zmacs-regions
    vimpulse-visual-region-expanded)
  "System variables that are reset for each Visual session.")

(viper-deflocalvar vimpulse-visual-vars-alist nil
  "Alist of old variable values.")

(viper-deflocalvar vimpulse-visual-last nil
  "Last active Visual mode.
May be `normal', `line', `block' or nil.")

(viper-deflocalvar vimpulse-visual-previous-state 'viper-state
  "Previous state before enabling Visual mode.
This lets us revert to Emacs state in non-vi buffers.")

(viper-deflocalvar vimpulse-visual-region-expanded nil
  "Whether region is expanded to the Visual selection.")

(viper-deflocalvar vimpulse-visual-point nil
  "Last expanded `point' in Visual mode.")

(viper-deflocalvar vimpulse-visual-mark nil
  "Last expanded `mark' in Visual mode.")

(viper-deflocalvar vimpulse-visual-overlay nil
  "Overlay for Visual selection.
In XEmacs, this is an extent.")

(viper-deflocalvar vimpulse-visual-block-overlays nil
  "Overlays for Visual Block selection.")

(viper-deflocalvar vimpulse-visual-whitespace-overlay nil
  "Overlay encompassing text inserted into the buffer
to make Block selection at least one column wide.")

(viper-deflocalvar vimpulse-undo-needs-adjust nil
  "If true, several commands in the undo-list should be connected.")

(defconst vimpulse-buffer-undo-list-mark 'vimpulse
  "Everything up to this mark is united in the undo-list.")

(defvar vimpulse-visual-height nil
  "Height of last Visual selection.")

(defvar vimpulse-visual-width nil
  "Width of last Visual selection.")

(defvar vimpulse-visual-insert-coords nil
  "List with (I-COM UL-POS COL NLINES), where
I-COM is the insert command (?i, ?a, ?I or ?A),
UL-POS is the position of the upper left corner of the region,
COL is the column of insertion, and
NLINES is the number of lines in the region.")

(defvar vimpulse-movement-cmds
  '(backward-char backward-list backward-paragraph backward-sentence
    backward-sexp backward-up-list backward-word beginning-of-buffer
    beginning-of-defun beginning-of-line beginning-of-visual-line
    cua-cancel digit-argument down-list end-of-buffer end-of-defun
    end-of-line end-of-visual-line exchange-point-and-mark
    forward-char forward-list forward-paragraph forward-sentence
    forward-sexp forward-word keyboard-quit mouse-drag-region
    mouse-save-then-kill mouse-set-point mouse-set-region
    move-beginning-of-line move-end-of-line next-line previous-line
    scroll-down scroll-up undo universal-argument up-list
    vimpulse-end-of-previous-word vimpulse-goto-definition
    vimpulse-goto-first-line vimpulse-goto-line
    vimpulse-visual-block-rotate vimpulse-visual-exchange-corners
    vimpulse-visual-reselect vimpulse-visual-restore
    vimpulse-visual-toggle-block vimpulse-visual-toggle-line
    vimpulse-visual-toggle-normal viper-backward-Word
    viper-backward-char viper-backward-paragraph
    viper-backward-sentence viper-backward-word
    viper-beginning-of-line viper-digit-argument viper-end-of-Word
    viper-end-of-word viper-exec-mapped-kbd-macro
    viper-find-char-backward viper-find-char-forward
    viper-forward-Word viper-forward-char viper-forward-paragraph
    viper-forward-sentence viper-forward-word viper-goto-char-backward
    viper-goto-char-forward viper-goto-eol viper-goto-line
    viper-insert viper-intercept-ESC-key viper-line-to-bottom
    viper-line-to-middle viper-line-to-top viper-next-line
    viper-paren-match viper-previous-line viper-search-Next
    viper-search-backward viper-search-forward viper-search-next
    viper-window-bottom viper-window-middle viper-window-top)
  "List of commands that move point.
If listed here, the region is not expanded to the
Visual selection before the command is executed.")

(defvar vimpulse-newline-cmds
  '(cua-copy-region cua-cut-region cua-delete-region delete-region
    exchange-point-and-mark execute-extended-command kill-region
    kill-ring-save vimpulse-Put-and-indent vimpulse-put-and-indent
    vimpulse-visual-exchange-corners viper-Put-back viper-put-back)
  "Non-operator commands needing trailing newline in Visual Line mode.
In most cases, it's more useful not to include this newline in
the region acted on.")

(defvar vimpulse-search-prompt nil
  "String to use for vi-like searching.")

;;; Carefully set Viper/woman variables

(defun vimpulse-configure-variables ()
  "Set various variables, unless customized."
  ;; Can backspace past start of insert/line
  (vimpulse-setq viper-ex-style-editing nil)
  ;; Don't create new frame for manpages
  (vimpulse-setq woman-use-own-frame nil)
  ;; Don't prompt upon K key (manpage display)
  (vimpulse-setq woman-use-topic-at-point t)
  ;; No start-up message
  (vimpulse-setq viper-inhibit-startup-message t)
  ;; Viper expert level 5
  (vimpulse-setq viper-expert-level 5)
  ;; Make cursor color consistent
  (vimpulse-setq viper-insert-state-cursor-color
                 viper-vi-state-cursor-color)
  ;; Cursor moves backwards when exiting Insert state
  (vimpulse-setq viper-ESC-moves-cursor-back t)
  ;; Not in Vim: C-h is indispensable in Emacs
  (vimpulse-setq viper-want-ctl-h-help t)
  ;; Refresh Viper settings
  (viper-change-state-to-vi))

(if (and (boundp 'after-init-time) after-init-time)
    (vimpulse-configure-variables)
  (add-hook 'after-init-hook 'vimpulse-configure-variables))

(provide 'vimpulse-dependencies)
