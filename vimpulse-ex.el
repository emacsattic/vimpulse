;;;; Ex commands

;; All this code is taken from Brad Beveridge's extended viper
(defvar vimpulse-extra-ex-commands
  '(("b" "buffer")
    ("bdelete" (vimpulse-kill-current-buffer))
    ("bnext" "next")
    ("clo" "close")
    ("close" (delete-window))
    ("on" "only")
    ("only" (delete-other-windows))
    ("quit" (save-buffers-kill-emacs))
    ("split" (split-window))
    ("syntax" (global-font-lock-mode))
    ;; Emacs and Vim use inverted naming conventions for splits
    ("vsplit" (split-window-horizontally)))
  "Extra Ex commands, added to `ex-token-alist' when Vimpulse loads.")

(defun vimpulse-kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil))

;; Additional Ex mode features: `ex-token-alist' is defined as a
;; constant, but it appears I can safely push values to it!
(dolist (entry vimpulse-extra-ex-commands)
  (setq ex-token-alist
        (delete (assoc (car entry) ex-token-alist) ex-token-alist))
  (add-to-list 'ex-token-alist entry t))

(provide 'vimpulse-ex)
