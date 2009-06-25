;;;;
;;;; Almost all of this code is taken from extended-viper 
;;;; coded by Brad Beveridge (bradbev@gmail.com)
;;;; - I changed the prefix of the custom functions to vimpulse 
;;;;   to avoid multiple prefixes
;;;;
(defvar vimpulse-fold-level 0)
(when (boundp 'hs-minor-mode)
  (add-hook hs-minor-mode-hook (lambda () 
				 (call-interactively 'hs-hide-all)
				 (define-key viper-vi-global-user-map "za"   'hs-toggle-hiding)
				 (define-key viper-vi-global-user-map "zM"   'hs-hide-all)
				 (define-key viper-vi-global-user-map "zR"   'hs-show-all)
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
(define-key viper-vi-global-user-map "O"    'vimpulse-open-new-line-above)
(define-key viper-vi-global-user-map "o"    'vimpulse-open-new-line-below)
(define-key viper-vi-global-user-map "\C-]" 'vimpulse-jump-to-tag-at-point)
(define-key viper-vi-global-user-map "\C-t" 'pop-tag-mark)
;; Map undo and redo from XEmacs' redo.el
(define-key viper-vi-global-user-map "u"    'undo)
(define-key viper-vi-global-user-map "\C-r" 'redo)

; Window manipulation
(define-key viper-vi-global-user-map "\C-w" (make-sparse-keymap))
(define-key viper-vi-global-user-map "\C-w\C-w" 'vimpulse-cycle-windows)
(define-key viper-vi-global-user-map "\C-ww" '-cycle-windows)
(define-key viper-vi-global-user-map "\C-wo" 'delete-other-windows)
(define-key viper-vi-global-user-map "\C-wc" 'delete-window)
(define-key viper-vi-global-user-map "\C-ws" 'split-window-vertically)
(define-key viper-vi-global-user-map "\C-wS" 'split-window-vertically)

; Block Visual Mode keys
(define-key viper-vi-global-user-map "\C-p" 'yank-rectangle)
(define-key viper-vi-global-user-map "\C-v" 'vimpulse-visual-mode-block)

; Insert mode keys
; Vim-like completion keys
(define-key viper-insert-global-user-map "\C-p" 'dabbrev-expand)
(define-key viper-insert-global-user-map "\C-n" 'vimpulse-abbrev-expand-after)
(define-key viper-insert-global-user-map [backspace] 'backward-delete-char-untabify)
; automatic enter indents
(define-key viper-insert-global-user-map (kbd "RET") 'vimpulse-enter-indents)
; make ^[ work
(define-key viper-insert-global-user-map (kbd "ESC") 'viper-exit-insert-state)

;;; My code (Alessandro)
(defun vimpulse-indent-lines (count)
  (save-excursion
    (dotimes (i count)
      (indent-according-to-mode)
      (next-line))))
(defun vimpulse-open-new-line-above (&optional arg)
  "Opens a line above the cursor, enters insert mode and indents."
  (interactive)
  (viper-Open-line arg)
  (indent-according-to-mode))
(defun vimpulse-open-new-line-below (&optional arg)
  "Opens a line below the cursor, enters insert mode and indents."
  (interactive)
  (viper-open-line arg)
  (indent-according-to-mode))

(defun vimpulse-enter-indents (&optional arg)
  "Indents the current line, inserts a newline and indents the new line."
  (interactive)
  (indent-according-to-mode)
  (insert ?\n)
  (indent-according-to-mode))

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
  (let ((symbol (concat "\\<" (thing-at-point 'symbol) "\\>")))
    (setq viper-s-string symbol)
    (setq viper-s-forward whether-forward)
    (viper-search symbol whether-forward 1)))

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
