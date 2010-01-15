;;;;
;;;; Utility code
;;;;
(defvar vimpulse-movement-cmds
  '(viper-append viper-backward-Word viper-backward-char
    viper-backward-paragraph viper-backward-sentence
    viper-backward-word viper-beginning-of-line
    viper-digit-argument viper-end-of-Word viper-end-of-word
    viper-exec-mapped-kbd-macro viper-find-char-backward
    viper-find-char-forward viper-forward-Word
    viper-forward-char viper-forward-paragraph
    viper-forward-sentence viper-forward-word
    viper-goto-char-backward viper-goto-char-forward
    viper-goto-eol viper-goto-line viper-insert
    viper-line-to-bottom viper-line-to-middle
    viper-line-to-top viper-next-line viper-previous-line
    vimpulse-goto-first-line)
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
  (let (binding key def)
    (dolist (binding augment-alist)
      (setq key (car binding)
            def (cdr binding))
      (when (or replace
                (not (lookup-key map key))
                (not (numberp (lookup-key map key))))
        (define-key map key def)))))

(defun vimpulse-add-vi-bindings (map cmds &optional replace)
  "Add vi bindings for CMDS to MAP."
  (let (cmd vimap pmap keys key)
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
          (unless (lookup-key pmap key)
            (vimpulse-augment-keymap map
                                     `((,key . ,cmd))
                                     replace)
            ;; To prioritize between maps in `vimap',
            ;; we keep track of bindings by augmenting `pmap'.
            (vimpulse-augment-keymap pmap
                                     `((,key . ,cmd)))))))))

(defun vimpulse-add-movement-cmds (map &optional replace)
  "Add Viper/Vimpulse movement commands to MAP.
The commands are taken from `vimpulse-movement-cmds' and looked
up in vi keymaps. If REPLACE is non-nil, may overwrite bindings
in MAP."
  (vimpulse-add-vi-bindings map vimpulse-movement-cmds replace))

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
The commands are taken from `vimpulse-movement-cmds'.
If REPLACE is non-nil, may overwrite bindings in MAP."
  (let (cmd)
    (dolist (cmd vimpulse-movement-cmds)
      (eval `(vimpulse-augment-keymap
              map '(([remap ,cmd] . viper-nil))
              replace)))))

;; Info mode
;; (eval-after-load 'info
;;   '(progn
;;      (vimpulse-add-core-movement-cmds Info-mode-map)
;;      (vimpulse-add-movement-cmds Info-mode-map)))

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
              (define-key viper-vi-global-user-map "za"
                (lambda () (hs-toggle-hiding) (hs-hide-level h)))
              (define-key viper-vi-global-user-map "zA"   'hs-toggle-hiding)
              (define-key viper-vi-global-user-map "zM"   'hs-hide-all)
              (define-key viper-vi-global-user-map "zR"   'hs-show-all)
              (define-key viper-vi-global-user-map "zO" 'vimpulse-hs-Open)
              (define-key viper-vi-global-user-map "zo"   'hs-show-block)
              (define-key viper-vi-global-user-map "zc"   'hs-hide-block))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; VISUAL MODE BINDINGS ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key viper-vi-basic-map "v" 'vimpulse-visual-mode)
(define-key viper-vi-basic-map "V" 'vimpulse-visual-mode-linewise)

(define-key viper-vi-global-user-map "K"    'woman)
(define-key viper-vi-global-user-map "gf"   'find-file-at-point)
(define-key viper-vi-global-user-map "gg"   'vimpulse-goto-first-line)
(define-key viper-vi-global-user-map "zb"   'viper-line-to-bottom)
(define-key viper-vi-global-user-map "zh"   'scroll-right)
(define-key viper-vi-global-user-map "zl"   'scroll-left)
(define-key viper-vi-global-user-map "zt"   'viper-line-to-top)
(define-key viper-vi-global-user-map "zz"   'viper-line-to-middle)
(define-key viper-vi-global-user-map "*"    'vimpulse-search-forward-for-symbol-at-point)
(define-key viper-vi-global-user-map "#"    'vimpulse-search-backward-for-symbol-at-point)
(define-key viper-vi-global-user-map " "    nil)
(define-key viper-vi-global-user-map "\C-]" 'vimpulse-jump-to-tag-at-point)
(define-key viper-vi-global-user-map "\C-t" 'pop-tag-mark)
;; Map undo and redo from XEmacs' redo.el
(define-key viper-vi-global-user-map "u"    'undo)
(define-key viper-vi-global-user-map "\C-r" 'redo)

;; Window manipulation
(define-key viper-vi-global-user-map "\C-w" (make-sparse-keymap))
(define-key viper-vi-global-user-map "\C-w\C-w" 'vimpulse-cycle-windows)
(define-key viper-vi-global-user-map "\C-ww" 'vimpulse-cycle-windows)
(define-key viper-vi-global-user-map "\C-wo" 'delete-other-windows)
(define-key viper-vi-global-user-map "\C-wc" 'delete-window)
(define-key viper-vi-global-user-map "\C-ws" 'split-window-vertically)
(define-key viper-vi-global-user-map "\C-wv" 'split-window-horizontally)
(add-to-list 'ex-token-alist '("on"    (delete-other-windows)))
(add-to-list 'ex-token-alist '("only"  (delete-other-windows)))
(add-to-list 'ex-token-alist '("clo"   (delete-window)))
(add-to-list 'ex-token-alist '("close" (delete-window)))

(when (fboundp 'windmove-left)
  (define-key viper-vi-global-user-map "\C-wh" 'windmove-left)
  (define-key viper-vi-global-user-map "\C-wj" 'windmove-down)
  (define-key viper-vi-global-user-map "\C-wk" 'windmove-up)
  (define-key viper-vi-global-user-map "\C-wl" 'windmove-right))

;; Block Visual Mode keys
(define-key viper-vi-global-user-map "\C-p" 'yank-rectangle)
(define-key viper-vi-global-user-map "\C-v" 'vimpulse-visual-mode-block)

;; Insert mode keys
;; Vim-like completion keys
(define-key viper-insert-global-user-map "\C-p" 'dabbrev-expand)
(define-key viper-insert-global-user-map "\C-n" 'vimpulse-abbrev-expand-after)
;; (define-key viper-insert-global-user-map [backspace] 'backward-delete-char-untabify) ; vim doesn't do this!
(define-key viper-insert-global-user-map [delete] 'delete-char) ;; delete key
                                        ; make ^[ work
(define-key viper-insert-global-user-map (kbd "ESC") 'viper-exit-insert-state)

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

(defun vimpulse-search-for-symbol-at-point (whether-forward)
  "Search forwards or backwards for the symbol under point."
  (let* ((str (regexp-quote (thing-at-point 'symbol)))
         (search-str (concat "\\<" str "\\>"
                             "\\|"
                             "\\_<" str "\\_>")))
    (setq viper-s-string search-str)
    (setq viper-s-forward whether-forward)
    (viper-search search-str whether-forward 1)))

(defun vimpulse-search-forward-for-symbol-at-point ()
  (interactive)
  (vimpulse-search-for-symbol-at-point t))

(defun vimpulse-search-backward-for-symbol-at-point ()
  (interactive)
  (vimpulse-search-for-symbol-at-point nil))

(defun vimpulse-jump-to-tag-at-point ()
  (interactive)
  (let ((tag (thing-at-point 'word)))
    (find-tag tag)))

;;; cppjavaperl's code
(defun vimpulse-abbrev-expand-after ()
  (interactive)
  (dabbrev-expand -1))

(provide 'vimpulse-misc-keybindings)
