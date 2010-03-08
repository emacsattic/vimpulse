;;; Add vi navigation to help buffers

;; C-u
(defcustom vimpulse-want-C-u-like-Vim nil
  "Whether C-u scrolls like in Vim, off by default."
  :group 'vimpulse
  :type  'boolean)

(unless vimpulse-want-C-u-like-Vim
  (define-key viper-vi-basic-map "\C-u" 'universal-argument))

;; Buffer-menu
(defcustom vimpulse-want-vi-keys-in-buffmenu t
  "Whether to use vi keys in Buffer menu, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load "buff-menu"
  '(when vimpulse-want-vi-keys-in-buffmenu
     (setq viper-emacs-state-mode-list
           (delq 'Buffer-menu-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'Buffer-menu-mode)
     (let ((map Buffer-menu-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'Buffer-menu-mode 'vi-state map))))

;; Dired
(defcustom vimpulse-want-vi-keys-in-dired t
  "Whether to use vi keys in Dired mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load 'dired
  '(when vimpulse-want-vi-keys-in-dired
     (setq viper-emacs-state-mode-list
           (delq 'dired-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'dired-mode)
     (let ((map dired-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'dired-mode 'vi-state map))))

;; Info
(defcustom vimpulse-want-vi-keys-in-Info t
  "Whether to use vi keys in Info mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load 'info
  '(when vimpulse-want-vi-keys-in-Info
     (setq viper-emacs-state-mode-list
           (delq 'Info-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'Info-mode)
     (let ((map Info-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (define-key map "\C-t" 'Info-history-back) ; l
       (define-key map "\C-o" 'Info-history-back)
       (define-key map "\M-h" 'Info-help) ; h
       (define-key map " " 'Info-scroll-up)
       (define-key map "\C-]" 'Info-follow-nearest-node)
       (define-key map [backspace] 'Info-scroll-down)
       (viper-modify-major-mode 'Info-mode 'vi-state map))))

;; Help
(defcustom vimpulse-want-vi-keys-in-help t
  "Whether to use vi keys in Help mode, on by default."
  :group 'vimpulse
  :type  'boolean)

(eval-after-load 'help-mode
  '(when vimpulse-want-vi-keys-in-help
     (setq viper-emacs-state-mode-list
           (delq 'help-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'help-mode)
     (let ((map help-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'help-mode 'vi-state map))))

;; ElDoc compatibility
(eval-after-load 'eldoc
  '(apply 'eldoc-add-command
          (append vimpulse-viper-movement-cmds
                  vimpulse-core-movement-cmds)))
;;;;
;;;; Almost all of this code is taken from extended-viper
;;;; coded by Brad Beveridge (bradbev at gmail.com)
;;;; - I changed the prefix of the custom functions to `vimpulse'
;;;;   to avoid multiple prefixes
;;;;
(defcustom vimpulse-fold-level 0
  "Default fold level."
  :type  'boolean
  :group 'vimpulse)

(defun vimpulse-hs-Open ()
  (interactive)
  (hs-show-block)
  (hs-hide-level -1))

(eval-after-load 'hideshow
  '(add-hook 'hs-minor-mode-hook
             (lambda ()
               (call-interactively 'hs-hide-all)
               (define-key viper-vi-basic-map "za"
                 (lambda ()
                   (interactive)
                   (hs-toggle-hiding)
                   (hs-hide-level vimpulse-fold-level)))
               (define-key viper-vi-basic-map "za" 'hs-toggle-hiding)
               (define-key viper-vi-basic-map "zm" 'hs-hide-all)
               (define-key viper-vi-basic-map "zr" 'hs-show-all)
               (define-key viper-vi-basic-map "zo" 'hs-show-block)
               (define-key viper-vi-basic-map "zc" 'hs-hide-block))))

;; Load reveal.el if available
(unless (featurep 'reveal)
  (condition-case nil
      (require 'reveal)
    (error nil)))
(when (fboundp 'global-reveal-mode)
  (global-reveal-mode 1))

(define-key viper-vi-basic-map "K" 'woman)
(define-key viper-vi-basic-map "g" nil) ; delete `viper-nil' binding
(define-key viper-vi-basic-map "gb" 'vimpulse-end-of-previous-word)
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
(define-key viper-vi-basic-map "+" 'vimpulse-previous-line-skip-white)
(define-key viper-vi-basic-map "_" 'vimpulse-next-line-skip-white)
(define-key viper-vi-basic-map "\C-]" 'vimpulse-jump-to-tag-at-point)
(define-key viper-vi-basic-map "\C-t" 'pop-tag-mark)
;; Map undo and redo from XEmacs' redo.el
(define-key viper-vi-basic-map "u" 'undo)
(when (fboundp 'redo)
  (define-key viper-vi-basic-map "\C-r" 'redo))

;; Window manipulation
(define-key viper-vi-basic-map "\C-w" (make-sparse-keymap))
(define-key viper-vi-basic-map "\C-w\C-w" 'vimpulse-cycle-windows)
(define-key viper-vi-basic-map "\C-ww" 'vimpulse-cycle-windows)
(define-key viper-vi-basic-map "\C-wo" 'delete-other-windows)
(define-key viper-vi-basic-map "\C-wc" 'delete-window)
(define-key viper-vi-basic-map "\C-ws" 'split-window-vertically)
(define-key viper-vi-basic-map "\C-wv" 'split-window-horizontally)
(add-to-list 'ex-token-alist '("on" "only"))
(add-to-list 'ex-token-alist '("only" (delete-other-windows)))
(add-to-list 'ex-token-alist '("clo" "close"))
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

(defun vimpulse-previous-line-skip-white (&optional arg)
  "Go ARG lines backward and to the first non-blank character."
  (interactive "P")
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (forward-line (- val))
    (back-to-indentation)
    (when com
      (viper-execute-com 'vimpulse-previous-line-nonblank val com))))

(defun vimpulse-next-line-skip-white (&optional arg)
  "Go ARG lines forward and to the first non-blank character."
  (interactive "P")
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (forward-line val)
    (back-to-indentation)
    (when com
      (viper-execute-com 'vimpulse-next-line-nonblank val com))))

(defun vimpulse-search-string (&optional pos thing backward regexp)
  "Find something to search for near POS or point.
THING is a `thing-at-point', default `symbol'.
BACKWARD, if t, specifies reverse direction.
REGEXP, if t, means the string is `regexp-quote'd.
Returns the empty string if nothing is found."
  (save-excursion
    (setq pos (or pos (point))
          thing (or thing 'symbol))
    (goto-char pos)
    (let ((str (thing-at-point thing)))
      ;; If there's nothing under point, go forwards
      ;; (or backwards) to find it
      (while (and (not str) (or (and backward (not (bobp)))
                                (and (not backward) (not (eobp)))))
        (if backward (backward-char) (forward-char))
        (setq str (thing-at-point 'symbol)))
      (setq str (or str ""))
      ;; No text properties, thank you very much
      (set-text-properties 0 (length str) nil str)
      (when regexp
        (setq str (regexp-quote str)))
      str)))

(defun vimpulse-search-for-symbol (&optional backward pos search)
  "Search forwards or backwards for the symbol under point.
If BACKWARD is t, search in the reverse direction.
SEARCH is a regular expression to use for searching instead of
the symbol under point; it is wrapped in \"\\\\_<\" and \"\\\\_>\".
POS specifies an alternative position to search from. Note that
if POS is specified and at the beginning of a match, that match
is highlighted rather than skipped past."
  (setq search (or search (vimpulse-search-string
                           (point) 'symbol backward t)))
  (cond
   ((string= "" search)
    (error "No string under cursor"))
   (t
    (setq viper-s-string  (concat "\\_<" search "\\_>")
          viper-s-forward (not backward))
    (cond
     (pos
      (unless (vimpulse-mark-active)
        (push-mark nil t))
      (goto-char pos)
      (cond
       ((looking-at search)
        (save-excursion
          (search-forward search))
        (viper-flash-search-pattern))
       (t
        (viper-search viper-s-string (not backward) 1)
        (unless (vimpulse-mark-active)
          (pop-mark)))))
     (t
      (viper-search viper-s-string (not backward) 1))))))

(defun vimpulse-search-forward-for-symbol-at-point ()
  (interactive)
  (vimpulse-search-for-symbol))

(defun vimpulse-search-backward-for-symbol-at-point ()
  (interactive)
  (vimpulse-search-for-symbol t))

(defun vimpulse-beginning-of-word-p ()
  (save-excursion
    (or (bobp)
        (cond
         ((viper-looking-at-alpha)
          (backward-char)
          (not (viper-looking-at-alpha)))
         ((not (viper-looking-at-alphasep))
          (backward-char)
          (viper-looking-at-alphasep))))))

(defun vimpulse-end-of-previous-word (arg)
  "Move point to end of previous word."
  (interactive "P")
  (viper-leave-region-active)
  (let ((val (viper-p-val arg))
	(com (viper-getcom arg)))
    (when com
      (viper-move-marker-locally 'viper-com-point (point)))
    (unless (or (vimpulse-beginning-of-word-p)
                (not (viper-looking-at-alpha)))
      (viper-backward-Word 1))
    (viper-backward-Word val)
    (unless (viper-end-of-word-p)
      (viper-end-of-word 1))
    (when com
      (viper-execute-com 'viper-end-of-word val com))))

(defun vimpulse-goto-definition ()
  "Go to definition or first occurrence of symbol under cursor."
  (interactive)
  (let ((str (vimpulse-search-string (point) 'symbol))
        ientry ipos)
    (cond
     ((string= "" str)
      (error "No string under cursor"))
     ;; If imenu is available, try it
     ((or (featurep 'imenu)
          (load "imenu" t))
      (setq ientry
            (condition-case nil
                (imenu--make-index-alist)
              (error nil)))
      (setq ientry (assoc str ientry))
      (setq ipos (cdr ientry))
      (unless (markerp ipos)
        (setq ipos (cadr ientry)))
      (cond
       ;; imenu found a position, so go there and
       ;; highlight the occurrence
       ((and (markerp ipos)
             (eq (current-buffer) (marker-buffer ipos)))
        (vimpulse-search-for-symbol nil ipos str))
       ;; imenu failed, so just go to first occurrence in buffer
       (t
        (vimpulse-search-for-symbol nil (point-min)))))
     ;; No imenu, so just go to first occurrence in buffer
     (t
      (vimpulse-search-for-symbol nil (point-min))))))

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
      (move-marker current-pos (point))
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
(unless (key-binding "\C-c\C-o")
  (global-set-key "\C-c\C-o" 'open-line)) ; some may miss this command

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

