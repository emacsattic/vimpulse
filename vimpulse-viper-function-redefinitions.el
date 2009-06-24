
;; This function replaces viper's original viper-exec-change function
;; which is invoked by key sequences starting with 'c'.  When the user
;; requests a command like 'cw', this function calls a sequence like
;; 'dwi' instead.  This stops viper from indicating the change
;; operation with distracting colored overlays and $ signs.  Instead,
;; it simply deletes the text then enters Insert mode, like Vim does.
;; 
;; The function works fine at eol and eob but TODO: understand the
;; original viper-exec-change command and see if mine does everything
;; it does.
(unless vimpulse-experimental
  (defun viper-exec-change (m-com com)
    (save-excursion ;; Added by Alessandro Piras, to fix cW suckage 
      (viper-exec-delete m-com com)) ;; on the last word of a line
    (if (eq m-com 'viper-goto-eol)
					; use viper-append here since vi's C (change to end of line)
					; command works differently than c
	(viper-append nil) 
      (viper-insert nil))))

;;EXPERIMENTAL: make the changecommand work like vim 
(when vimpulse-experimental
  (defun viper-exec-change (m-com com)
    (cond
     ((vimpulse-is-whitespace viper-com-point)            ;; check if the command has been issued on a whitespace
      (save-excursion (viper-exec-delete m-com com))      ;; deletes the stuff as in the old code
      (while (vimpulse-is-whitespace (point))             ;; eliminates all trailing whitespace like vim does
	(delete-char 1))                               
      (viper-insert nil))                                 
     (t                                                   ;; Old code executed in the other cases
      (save-excursion ;; Added by Alessandro Piras, to fix cW suckage 
	(viper-exec-delete m-com com)) ;; on the last word of a line
      (if (eq m-com 'viper-goto-eol) ; use viper-append here since vi's C (change to end of line)
					; command works differently than c
	  (viper-append nil) 
	(viper-insert nil)))))
  )

(when nil
  (defun viper-adjust-undo ()
    "This viper function has been redefined by vimpulse.el to
do nothing.  This way, in insert mode, typing then moving 
the cursor then typing more counts as two separately undoable 
actions instead of one."
    )
  )



(provide 'vimpulse-viper-function-redefinitions)
