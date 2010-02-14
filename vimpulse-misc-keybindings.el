;;;;
;;;; Utility code
;;;;
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
    viper-window-top vimpulse-goto-first-line
    vimpulse-search-backward-for-symbol-at-point
    vimpulse-search-forward-for-symbol-at-point
    vimpulse-jump-backward vimpulse-jump-forward
    vimpulse-visual-toggle-normal vimpulse-visual-toggle-line
    vimpulse-visual-toggle-block)
  "List of Viper/Vimpulse movement commands.")

(defvar vimpulse-core-movement-cmds
  '(viper-backward-char
    viper-next-line
    viper-previous-line
    viper-forward-char)
  "List of Viper \"core\" movement commands.
These should be present in every mode, to avoid confusion.")

(defun vimpulse-augment-keymap
  (map augment-alist &optional replace)
  "Augment MAP with bindings from AUGMENT-ALIST.
If REPLACE is non-nil, bindings in MAP may be overwritten.
AUGMENT-ALIST has the format ((KEY . DEF) ...),
where KEY and DEF are passed to `define-key'."
  (let (key def num)
    (dolist (binding augment-alist)
      (setq key (car binding)
            def (cdr binding)
            num (lookup-key map key))
      (cond
       (replace
        (when (numberp num)
          (define-key map (vimpulse-truncate key num) nil))
        (define-key map key def))
       (t
        (when (numberp num)
          (setq num (lookup-key map (vimpulse-truncate key num))))
        (unless num
          (define-key map key def)))))))

(defun vimpulse-add-vi-bindings (map cmds &optional replace)
  "Add vi bindings for CMDS to MAP."
  (let (pmap keys)
    (setq pmap (make-sparse-keymap))
    (dolist (cmd cmds map)
      (dolist (vimap (list viper-vi-intercept-map
                           viper-vi-local-user-map
                           viper-vi-global-user-map
                           viper-vi-kbd-map
                           viper-vi-diehard-map
                           viper-vi-basic-map))
        (setq keys (where-is-internal cmd vimap))
        (dolist (key keys)
          (when (or (not (lookup-key pmap key))
                    (numberp (lookup-key pmap key)))
            (vimpulse-augment-keymap map
                                     `((,key . ,cmd))
                                     replace)
            ;; To prioritize between maps in `vimap',
            ;; we keep track of bindings by augmenting `pmap'.
            (vimpulse-augment-keymap pmap
                                     `((,key . ,cmd)))))))))

(defun vimpulse-add-movement-cmds (map &optional replace)
  "Add Viper/Vimpulse movement commands to MAP.
The commands are taken from `vimpulse-viper-movement-cmds' and looked
up in vi keymaps. If REPLACE is non-nil, may overwrite bindings
in MAP."
  (vimpulse-add-vi-bindings map vimpulse-viper-movement-cmds replace))

;; The default for this function is to replace rather than augment,
;; as core navigation should be present everywhere.
(defun vimpulse-add-core-movement-cmds (map &optional augment)
  "Add \"core\" movement commands to MAP, forcefully.
The commands are taken from `vimpulse-core-movement-cmds'.
If AUGMENT is non-nil, don't overwrite bindings in MAP."
  (vimpulse-add-vi-bindings map
                            vimpulse-core-movement-cmds
                            (not augment)))

(defun vimpulse-inhibit-movement-cmds (map &optional replace)
  "Remap Viper movement commands to `viper-nil' in MAP.
The commands are taken from `vimpulse-viper-movement-cmds'.
If REPLACE is non-nil, may overwrite bindings in MAP."
  (dolist (cmd vimpulse-viper-movement-cmds)
    (eval `(vimpulse-augment-keymap
            map '(([remap ,cmd] . viper-nil))
            replace))))

(defun vimpulse-inhibit-destructive-cmds (map &optional replace)
  "Remap destructive Viper commands to `viper-nil' in MAP.
This isn't complete since `viper-command-argument' is left out so
that yanking may work, but as change and delete fail silently in
read-only buffers anyway, it does the job."
  (dolist (cmd '(viper-Append
                 viper-Insert
                 viper-append
                 viper-change-to-eol
                 viper-insert
                 viper-kill-line
                 viper-substitute
                 viper-substitute-line
                 vimpulse-visual-append
                 vimpulse-visual-change
                 vimpulse-visual-insert))
    (eval `(vimpulse-augment-keymap
            map '(([remap ,cmd] . viper-nil))
            replace))))

;; Buffer-menu
(defcustom vimpulse-want-vi-keys-in-buffmenu t
  "Whether to use vi keys in Buffer menu, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load "buff-menu"
  '(when vimpulse-want-vi-keys-in-buffmenu
     (vimpulse-add-core-movement-cmds Buffer-menu-mode-map)
     (vimpulse-add-movement-cmds Buffer-menu-mode-map)
     (vimpulse-inhibit-destructive-cmds Buffer-menu-mode-map)))

;; Dired
(defcustom vimpulse-want-vi-keys-in-dired t
  "Whether to use vi keys in Dired mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load 'dired
  '(when vimpulse-want-vi-keys-in-dired
     (vimpulse-add-core-movement-cmds dired-mode-map)
     (vimpulse-add-movement-cmds dired-mode-map)
     (vimpulse-inhibit-destructive-cmds dired-mode-map)))

;; Info
(defcustom vimpulse-want-vi-keys-in-Info t
  "Whether to use vi keys in Info mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load 'info
  '(when vimpulse-want-vi-keys-in-Info
     (vimpulse-add-core-movement-cmds Info-mode-map)
     (vimpulse-add-movement-cmds Info-mode-map)
     (define-key Info-mode-map "\C-t" 'Info-history-back) ; l
     (define-key Info-mode-map "\M-h" 'Info-help)         ; h
     (define-key Info-mode-map " " 'Info-scroll-up)
     (define-key Info-mode-map [backspace] 'Info-scroll-down)
     (vimpulse-inhibit-destructive-cmds Info-mode-map)))

;; Help
(defcustom vimpulse-want-vi-keys-in-help t
  "Whether to use vi keys in Help mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load 'help-mode
  '(when vimpulse-want-vi-keys-in-help
     (vimpulse-add-core-movement-cmds help-mode-map)
     (vimpulse-add-movement-cmds help-mode-map)
     (vimpulse-inhibit-destructive-cmds help-mode-map)))

;; ElDoc compatibility
(eval-after-load 'eldoc
  '(apply 'eldoc-add-command
          (append vimpulse-viper-movement-cmds
                  vimpulse-core-movement-cmds)))
;;;;
;;;; Almost all of this code is taken from extended-viper
;;;; coded by Brad Beveridge (bradbev@gmail.com)
;;;; - I changed the prefix of the custom functions to vimpulse
;;;;   to avoid multiple prefixes
;;;;
(defvar vimpulse-fold-level 0)
(defun vimpulse-hs-Open ()
  (interactive)
  (hs-show-block)
  (hs-hide-level -1))
(when (boundp 'hs-minor-mode)
  (add-hook 'hs-minor-mode-hook
            (lambda ()
              (call-interactively 'hs-hide-all)
              (define-key viper-vi-basic-map "za"
                (lambda () (hs-toggle-hiding) (hs-hide-level h)))
              (define-key viper-vi-basic-map "zA" 'hs-toggle-hiding)
              (define-key viper-vi-basic-map "zM" 'hs-hide-all)
              (define-key viper-vi-basic-map "zR" 'hs-show-all)
              (define-key viper-vi-basic-map "zO" 'vimpulse-hs-Open)
              (define-key viper-vi-basic-map "zo" 'hs-show-block)
              (define-key viper-vi-basic-map "zc" 'hs-hide-block))))

(define-key viper-vi-basic-map "K" 'woman)
(define-key viper-vi-basic-map "g" nil) ; delete `viper-nil' binding
(define-key viper-vi-basic-map "gd" 'vimpulse-goto-definition)
(define-key viper-vi-basic-map "gf" 'find-file-at-point)
(define-key viper-vi-basic-map "gg" 'vimpulse-goto-first-line)
(define-key viper-vi-basic-map "gh" 'backward-char)
(define-key viper-vi-basic-map "gj" 'next-line)
(define-key viper-vi-basic-map "gk" 'previous-line)
(define-key viper-vi-basic-map "gl" 'forward-char)
(define-key viper-vi-basic-map "zb" 'viper-line-to-bottom)
(define-key viper-vi-basic-map "zh" 'scroll-right)
(define-key viper-vi-basic-map "zl" 'scroll-left)
(define-key viper-vi-basic-map "zt" 'viper-line-to-top)
(define-key viper-vi-basic-map "zz" 'viper-line-to-middle)
(define-key viper-vi-basic-map "*" 'vimpulse-search-forward-for-symbol-at-point)
(define-key viper-vi-basic-map "#" 'vimpulse-search-backward-for-symbol-at-point)
(define-key viper-vi-basic-map "\C-]" 'vimpulse-jump-to-tag-at-point)
(define-key viper-vi-basic-map "\C-t" 'pop-tag-mark)
;; Map undo and redo from XEmacs' redo.el
(define-key viper-vi-basic-map "u" 'undo)
(define-key viper-vi-basic-map "\C-r" 'redo)

;; Window manipulation
(define-key viper-vi-basic-map "\C-w" (make-sparse-keymap))
(define-key viper-vi-basic-map "\C-w\C-w" 'vimpulse-cycle-windows)
(define-key viper-vi-basic-map "\C-ww" 'vimpulse-cycle-windows)
(define-key viper-vi-basic-map "\C-wo" 'delete-other-windows)
(define-key viper-vi-basic-map "\C-wc" 'delete-window)
(define-key viper-vi-basic-map "\C-ws" 'split-window-vertically)
(define-key viper-vi-basic-map "\C-wv" 'split-window-horizontally)
(add-to-list 'ex-token-alist '("on" (delete-other-windows)))
(add-to-list 'ex-token-alist '("only" (delete-other-windows)))
(add-to-list 'ex-token-alist '("clo" (delete-window)))
(add-to-list 'ex-token-alist '("close" (delete-window)))

(when (fboundp 'windmove-left)
  (define-key viper-vi-basic-map "\C-wh" 'windmove-left)
  (define-key viper-vi-basic-map "\C-wj" 'windmove-down)
  (define-key viper-vi-basic-map "\C-wk" 'windmove-up)
  (define-key viper-vi-basic-map "\C-wl" 'windmove-right))

;;; Insert mode keys
;; Vim-like completion keys
(define-key viper-insert-basic-map "\C-p" 'dabbrev-expand)
(define-key viper-insert-basic-map "\C-n" 'vimpulse-abbrev-expand-after)
;; (define-key viper-insert-basic-map [backspace] 'backward-delete-char-untabify) ; vim doesn't do this!
(define-key viper-insert-basic-map [delete] 'delete-char) ;; delete key
                                        ; make ^[ work
(define-key viper-insert-basic-map (kbd "ESC") 'viper-exit-insert-state)

;;; My code (Alessandro)
(defun vimpulse-indent-lines (count)
  (save-excursion
    (dotimes (i count)
      (indent-according-to-mode)
      (forward-line))))

;;; His code (Brad)
(defun vimpulse-goto-first-line ()
  "Send point to the start of the first line."
  (interactive)
  (viper-goto-line 1))

(defun vimpulse-cycle-windows ()
  "Cycle point to another window."
  (interactive)
  (select-window (next-window)))

(defun vimpulse-search-for-symbol-at-point (forward-p &optional pos force)
  "Search forwards or backwards for the symbol under point.
FORWARD-P specifies the direction, POS the position from where
to start the search."
  (let ((str (thing-at-point 'symbol)))
    ;; If there's no symbol under point, go forwards
    ;; (or backwards) to find one
    (save-excursion
      (while (and (not str) (or (and forward-p (not (eobp)))
                                (and (not forward-p) (not (bobp)))))
        (if forward-p (forward-char) (backward-char))
        (setq str (thing-at-point 'symbol))))
    (when pos (goto-char pos))
    (cond
     ((stringp str)
      (setq str (regexp-quote str))
      (setq str (concat "\\_<" str "\\_>"))
      ;; If searching several times in a row,
      ;; use the same search string each time
      (when (or force
                (string= "" viper-s-string)
                (not (looking-at-p viper-s-string)))
        (setq viper-s-string str))
      (setq viper-s-forward forward-p)
      (viper-search viper-s-string forward-p 1))
     (t
      (error "No string under cursor")))))

(defun vimpulse-search-forward-for-symbol-at-point ()
  (interactive)
  (vimpulse-search-for-symbol-at-point t nil t))

(defun vimpulse-search-backward-for-symbol-at-point ()
  (interactive)
  (vimpulse-search-for-symbol-at-point nil nil t))

(defun vimpulse-goto-definition ()
  (interactive)
  (vimpulse-search-for-symbol-at-point t (point-min)))

(defun vimpulse-jump-to-tag-at-point ()
  (interactive)
  (let ((tag (thing-at-point 'word)))
    (find-tag tag)))

;; Auto-indent
(defadvice viper-line (after vimpulse activate)
  "Indent if `viper-auto-indent' is t."
  (and (boundp 'viper-auto-indent) viper-auto-indent
       (eq ?C (cdr arg))
       (indent-according-to-mode)))

;; C-o/C-i
(viper-deflocalvar
 vimpulse-mark-list nil
 "List of mark positions to jump to with `vimpulse-jump-forward'.
They are stored as markers, the current position first:

    (car vimpulse-mark-list)  = current position (last popped)
    (cdr vimpulse-mark-list)  = future positions (previously popped)
    (cadr vimpulse-mark-list) = next position (to jump to)

In other words, a sort of \"reverse mark ring\": marks which are
popped off the mark ring, are collected here.")

(defadvice set-mark (after vimpulse activate)
  "Clear `vimpulse-mark-list'."
  (mapc (lambda (marker)
          (set-marker marker nil))
        vimpulse-mark-list)
  (setq vimpulse-mark-list nil))

(defadvice push-mark (after vimpulse activate)
  "Clear `vimpulse-mark-list'."
  (mapc (lambda (marker)
          (set-marker marker nil))
        vimpulse-mark-list)
  (setq vimpulse-mark-list nil))

(defun vimpulse-jump-backward (arg)
  "Go to older position in jump list.
To go the other way, press \\[vimpulse-jump-forward]."
  (interactive "p")
  (let ((current-pos (make-marker)) i)
    (unless vimpulse-mark-list
      (move-marker current-pos (point))
      (add-to-list 'vimpulse-mark-list current-pos))
    (dotimes (arg arg)
      (setq current-pos (make-marker))
      ;; Skip past duplicate entries in the mark ring
      (setq i (length mark-ring))
      (while (progn (move-marker current-pos (point))
                    (let (vimpulse-mark-list)
                      ;; Protect `vimpulse-mark-list'
                      (set-mark-command 0))
                    (setq i (1- i))
                    (and (= (point) current-pos) (< 0 i))))
      ;; Already there?
      (unless (= current-pos (car vimpulse-mark-list))
        (setq vimpulse-mark-list
              (cons current-pos vimpulse-mark-list))))))

(defun vimpulse-jump-forward (arg)
  "Go to newer position in jump list.
To go the other way, press \\[vimpulse-jump-backward]."
  (interactive "p")
  (let (current-pos next-pos)
    (dotimes (arg arg)
      (setq current-pos (car vimpulse-mark-list)
            next-pos (cadr vimpulse-mark-list))
      (when next-pos
        ;; Protect `vimpulse-mark-list'
        (let (vimpulse-mark-list)
          (push-mark current-pos t nil))
        (goto-char next-pos)
        (setq vimpulse-mark-list (cdr vimpulse-mark-list))))))

(define-key viper-vi-basic-map "\C-o" 'vimpulse-jump-backward)
(define-key viper-vi-basic-map "\C-i" 'vimpulse-jump-forward)
(global-set-key "\M-o" 'open-line) ; some may miss this command

;; N%
(defadvice viper-paren-match (around vimpulse activate)
  "Go to percentage in the file when ARG >= 10."
  (let ((val (viper-p-val arg)))
    (cond
     ((<= 10 val)
      (goto-char (+ (point-min)
                    (floor (* (- (point-max) (point-min)) 0.01
                              (max 0 (min 100 val))))))
      (beginning-of-line))
     (t
      ad-do-it))))

;; Replace backspace
(defcustom vimpulse-backspace-restore t
  "Whether Backspace restores the original text in Replace mode.
On by default."
  :group 'vimpulse
  :type  'boolean)

(viper-deflocalvar
 vimpulse-replace-alist nil
 "Alist of characters overwritten in Replace mode.
Used by `vimpulse-replace-backspace' to restore text.
The format is (POS . CHAR).")

(defun vimpulse-replace-pre-command ()
  "Remember the character under point."
  (cond
   (viper-replace-minor-mode
    (unless (assq (point) vimpulse-replace-alist)
      (add-to-list 'vimpulse-replace-alist
                   (cons (point) (char-after)))))
   ;; If not in Replace mode, remove itself
   (t
    (remove-hook 'pre-command-hook 'vimpulse-replace-pre-command))))

(add-hook 'viper-replace-state-hook
          (lambda ()
            (setq vimpulse-replace-alist nil)
            (vimpulse-replace-pre-command)
            (add-hook 'pre-command-hook
                      'vimpulse-replace-pre-command)))

(defun vimpulse-replace-backspace ()
  "Restore character under cursor.
If `vimpulse-backspace-restore' is nil,
call `viper-del-backward-char-in-replace' instead."
  (interactive)
  (cond
   (vimpulse-backspace-restore
    (backward-char)
    (let ((oldchar (cdr (assq (point) vimpulse-replace-alist))))
      (when oldchar
        (save-excursion
          (delete-char 1)
          (insert oldchar)))))
   (t
    (viper-del-backward-char-in-replace))))

(defadvice viper-adjust-keys-for (after vimpulse activate)
  "Map <backspace> to `vimpulse-replace-backspace' in Replace mode."
  (define-key viper-replace-map [backspace] 'vimpulse-replace-backspace))

;;; cppjavaperl's code
(defun vimpulse-abbrev-expand-after ()
  (interactive)
  (dabbrev-expand -1))

(provide 'vimpulse-misc-keybindings)

