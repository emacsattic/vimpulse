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
  (add-hook 'hs-minor-mode-hook (lambda () 
				 (call-interactively 'hs-hide-all)
				 (define-key viper-vi-global-user-map "za" '(lambda () (hs-toggle-hiding) (hs-hide-level h)))
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

; Window manipulation
(define-key viper-vi-global-user-map "\C-w" (make-sparse-keymap))
(define-key viper-vi-global-user-map "\C-w\C-w" 'vimpulse-cycle-windows)
(define-key viper-vi-global-user-map "\C-ww" 'vimpulse-cycle-windows)
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
;;(define-key viper-insert-global-user-map [backspace] 'backward-delete-char-untabify) ;vim doesn't do this!
(define-key viper-insert-global-user-map [delete] 'delete-char) ;; delete key
; make ^[ work
(define-key viper-insert-global-user-map (kbd "ESC") 'viper-exit-insert-state)

;;; My code (Alessandro)
(defun vimpulse-indent-lines (count)
  (save-excursion
    (dotimes (i count)
      (indent-according-to-mode)
      (next-line))))
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
