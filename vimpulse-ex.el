;;; All this code is taken from Brad Beveridge's extended viper.
(defvar vimpulse-extra-ex-commands '(
      ("b" "buffer")
      ("bdelete" (vimpulse-kill-current-buffer))
      ("bnext" "next")
      ("syntax" (global-font-lock-mode))
      ("split" (split-window))
      ; Emacs and Vim use inverted naming conventions for splits.
      ("vsplit" (split-window-horizontally))
))
 

(defun vimpulse-kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil)) 


;;; Manipulation of Vipers functions by using the advice feature
;;; Many of the functions here rely as heavily on Viper's internals as Viper itself
;;; Additional Ex mode features.
;;; ex-token-alist is defined as a constant, but it appears I can safely push values to it!
(defadvice viper-ex (around vimpulse-extended-ex-commands (arg &optional string) activate)
  ad-do-it) 

(setq ex-token-alist (append vimpulse-extra-ex-commands ex-token-alist))


(provide 'vimpulse-ex)
