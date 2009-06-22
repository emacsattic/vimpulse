

;;; Code:
;; Begin main Vim emulation code {{{
(defvar vimpulse-experimental t
  "Define whether or not use experimental features. Turned on by default, so you will give feedback :P.")
;; Load advice.el.
(require 'advice)
;; Load redo.el if available.  Sadly we can't use APEL's require
;; function to get 'noerror functionality because GNU Emacs 21 doesn't
;; ship with APEL included.
(unless (featurep 'redo)
  (load "redo" 'noerror))
;;;;
;;;; INSERT MODE KEYS (Alessandro Piras, laynor@gmail.com)
;;;;
(defun vimpulse-enter-indents (&optional arg)
 (interactive)
 (indent-according-to-mode)
 (insert ?\n)
 (indent-according-to-mode))

(define-key viper-insert-global-user-map (kbd "RET") 'vimpulse-enter-indents)
(define-key viper-insert-global-user-map (kbd "ESC") 'viper-exit-insert-state)



;;;;
;;;; Almost all of this code is taken from extended-viper 
;;;; coded by Brad Beveridge (bradbev@gmail.com)
;;;; - I changed the prefix of the custom functions to vimpulse 
;;;;   to avoid multiple prefixes
;;;;

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

; Map undo and redo from XEmacs' redo.el
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

(defvar vimpulse-extra-ex-commands '(
      ("b" "buffer")
      ("bdelete" (vimpulse-kill-current-buffer))
      ("bnext" "next")
      ("syntax" (global-font-lock-mode))
      ("split" (split-window))
      ; Emacs and Vim use inverted naming conventions for splits.
      ("vsplit" (split-window-horizontally))
))
 
;;; My code (Alessandro)
(defun vimpulse-open-new-line-above (&optional arg)
  (interactive)
  (viper-Open-line arg)
  (indent-according-to-mode))
(defun vimpulse-open-new-line-below (&optional arg)
  (interactive)
  (viper-open-line arg)
  (indent-according-to-mode))

;;; His code (Brad)
(defun vimpulse-goto-first-line ()
  "Send point to the start of the first line."
  (interactive)
  (viper-goto-line 1)) 

(defun vimpulse-kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer nil)) 

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

;;; Manipulation of Vipers functions by using the advice feature
;;; Many of the functions here rely as heavily on Viper's internals as Viper itself
;;; Additional Ex mode features.
;;; ex-token-alist is defined as a constant, but it appears I can safely push values to it!
(defadvice viper-ex (around vimpulse-extended-ex-commands (arg &optional string) activate)
  ad-do-it) 

(setq ex-token-alist (append vimpulse-extra-ex-commands ex-token-alist))
;;End of Brad's code

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
(defun vimpulse-is-whitespace (pos)
  "Returns true if the character at `pos' is whitespace, nil otherwhise"
  (equal (char-syntax (char-after pos)) 32))

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
;; asdf asdf

(when nil
(defun viper-adjust-undo ()
  "This viper function has been redefined by vimpulse.el to
do nothing.  This way, in insert mode, typing then moving 
the cursor then typing more counts as two separately undoable 
actions instead of one."
  )
)

;;; cppjavaperl's code
(defun vimpulse-abbrev-expand-after ()
  (interactive)
  (dabbrev-expand -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; VISUAL MODE HACKS ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key viper-vi-basic-map "v" 'vimpulse-visual-mode)
(define-key viper-vi-basic-map "V" 'vimpulse-visual-mode-linewise)

;; Define a helper function that sets up the viper keys in a given map.
;; This function is useful for creating movement maps or altering existing
;; maps
(defun vimpulse-set-movement-keys-for-map (map)
  (define-key map "\C-d" 'viper-scroll-up)
  (define-key map "\C-u" 'viper-scroll-down)
  (define-key map "j" 'viper-next-line)
  (define-key map "k" 'viper-previous-line)
  (define-key map "l" 'viper-forward-char)
  (define-key map "h" 'viper-backward-char))

;; EXAMPLE, the following lines enable Vim style movement in help
;; and dired modes.
;; create a movement map and set the keys
;(setq vimpulse-movement-map (make-sparse-keymap))
;(vimpulse-set-movement-keys-for-map vimpulse-movement-map)
;(viper-modify-major-mode 'dired-mode 'emacs-state vimpulse-movement-map) 
;(viper-modify-major-mode 'help-mode 'emacs-state vimpulse-movement-map)

;; }}} End main Vim emulation code

(provide 'vimpulse)



 



;; Begin visual mode code {{{

(eval-when-compile (require 'easy-mmode))

;; local variables
(defgroup vimpulse-visual nil
  "visual-mode for viper"
  :prefix "vimpulse-visual-"
  :group 'emulations)

 (define-minor-mode vimpulse-visual-mode
  "Toggles visual mode in viper"
  :lighter " visual"
  :initial-value nil
  :global nil
  :group 'vimpulse-visual)   
(defvar vimpulse-visual-mode-map (make-sparse-keymap)
  "Viper Visual mode keymap. This keymap is active when viper is in VISUAL mode")
(defvar vimpulse-visual-mode-linewise nil
  "If non nil visual mode will operate linewise")
(defvar vimpulse-visual-mode-block nil
  "If non nil visual mode will operate blockwise")
(defcustom vimpulse-visual-load-hook nil
  "Hooks to run after loading vimpulse-visual-mode."
  :type 'hook
  :group 'vimpulse-visual)

(defadvice viper-move-marker-locally (around vimpulse-move-marker-locally-wrap activate)
 (unless vimpulse-visual-mode
   ad-do-it))

(defadvice viper-deactivate-mark (around vimpulse-deactivate-mark-wrap activate)
 (unless vimpulse-visual-mode
   ad-do-it))

;; this thing is just to silence the byte compiler
;; and stop it bugging about free variable
;; viper--key-maps in emacs 21 :)
;; update: and to stop emacs 23 bugging about the old macro
(defmacro vimpulse-add-visual-maps-macro(keymap)
  `(defadvice viper-normalize-minor-mode-map-alist (after vimpulse-add-visual-maps activate)
     "This function modifies minor-mode-map-alists to include the visual mode keymap"
     (push (cons 'vimpulse-visual-mode vimpulse-visual-mode-map) ,keymap)))

(cond
 ((>= emacs-major-version 22)
  (vimpulse-add-visual-maps-macro viper--key-maps))
 (t 
  (vimpulse-add-visual-maps-macro minor-mode-map-alist)))

;; Keys that differ from normal mode
(defun vimpulse-visual-mode-to-insert-mode () ;TODO: fix behavior to behave like vim
  (interactive)                            ;   -In which way does it differ?
  (vimpulse-visual-mode 'toggle)              ;    probably it has to do with the last
  (viper-change-state-to-insert))          ;    fixes I made.



;; The next definitions are for visual-block-mode


;; This variable holds the point and column of the first line
;; as well as the number of lines in the region.
(defvar vimpulse-visual-insert-coords nil
  "A list with (i-com ul-pos col nlines), where
`i-com' is the insert command (?i, ?a, ?I or ?A)
`ul-pos' is the position of the upper left corner of the region
`col' is the column of insertion
`nlines' is the number of lines in the region")

;;
;; Modified by Alessandro Piras
;;
(defun vimpulse-create-coords (i-com)
  "Updates the list of block insert coordinates with the current rectangle.
`i-com' should be ?c, ?i, ?a, ?I or ?A, the column for the insertion will be
chosen according to this command."
  ;; New visual mode handling: instead of using (region-beginning) and 
  ;; (region-end) we use rb and re, bound to the right bounds:
  ;; overlay bounds in normal and linewise mode, region bounds in block mode.
  (destructuring-bind (rb re) (if vimpulse-visual-mode-block 
				  (list (region-beginning) (region-end))
				(list (overlay-start vimpulse-visual-overlay)
				      (overlay-end vimpulse-visual-overlay)))
    (make-local-variable 'vimpulse-visual-insert-coords)
    (setq vimpulse-visual-insert-coords nil)
  
    (let ((nlines (count-lines re rb))
	  (col 0))		  ; For ?I and ?A trivial: column is 0
      (when (or (eq i-com ?a) (eq i-com ?i) (eq i-com ?c))
	;; for ?i and ?a chose the left (the right) column of the rectangle
	(let ((start-col (save-excursion 
			   (goto-char rb)
			   (current-column)))
	      (end-col (save-excursion
			 (goto-char re)
			 (current-column))))
	  ;; decide if we use the left or the right column
	  (setq col (max 0 (if (or (eq i-com ?i) (eq i-com ?c))
			       (min start-col end-col)
			     (1- (max start-col end-col)))))))
    
      ;; Ok we have all information, so go to the insert-point ...
      (goto-char rb) 
      (move-to-column col)
      ;; ... and save the information
      (setq vimpulse-visual-insert-coords 
	    (list i-com (point) col nlines)))))


(defun vimpulse-visual-insert (&optional arg)
  "Called when in visual mode to go insert mode at the beginning of the selection"
  (interactive "P")
  
  (when vimpulse-visual-mode-linewise (vimpulse-create-coords ?I))
  (when vimpulse-visual-mode-block (vimpulse-create-coords ?i))
  (vimpulse-visual-mode nil)
  (if vimpulse-visual-mode-linewise
      (viper-Insert arg)
      (viper-insert arg)))


(defun vimpulse-visual-append (&optional arg)
  "Called when in visual mode to go to insert mode at the end of the selection"
  (interactive "P")
  
  (when vimpulse-visual-mode-linewise (vimpulse-create-coords ?A))
  (when vimpulse-visual-mode-block (vimpulse-create-coords ?a))
  (vimpulse-visual-mode nil)
  (if vimpulse-visual-mode-linewise
      (viper-Append arg)
      (viper-append arg)))


(defun connect-undos (n undo-list)
  "Connects the last n undo steps in undo-list to one step"
  (when (and (listp undo-list) 
	     (listp (cdr undo-list)) 
	     (> n 1))
    (if (null (cadr undo-list))
	(progn 
	  (setcdr undo-list (cddr undo-list))
	  (connect-undos (1- n) undo-list))
        (connect-undos n (cdr undo-list)))))


;; Redefinitions of viper functions to handle visual block-mode
(defun viper-exit-insert-state ()
  (interactive)
  (viper-change-state-to-vi)
  (when vimpulse-visual-insert-coords
    ;; Get the saved info about the visual region
    (let ((i-com (car vimpulse-visual-insert-coords))
	  (pos (cadr vimpulse-visual-insert-coords))
	  (col (caddr vimpulse-visual-insert-coords))
	  (nlines (cadddr vimpulse-visual-insert-coords)))
      (goto-char pos)
      (save-excursion
	(dotimes (i (1- nlines))
	  (forward-line 1)
	  (let ((cur-col (move-to-column col)))
	    ;; If we are in block mode this line but do not hit the correct 
            ;; column, we check if we should convert tabs and/or append spaces
	    (if (and vimpulse-visual-mode-block
		     (or (/= col cur-col) ;; wrong column or 
			 (eolp)))         ;; end of line 
		(cond ((< col cur-col)	  ;; we are inside a tab 
		       (move-to-column (1+ col) 'fill) ;; -> convert to spaces
		       (move-to-column col 'fill) ;; this is needed for ?a
		       (viper-repeat nil))
		      ((and (>= col cur-col) ;; we are behind the end
			    (eq i-com ?a))   ;; and i-com is ?a
		       (move-to-column (1+ col) t) ;; -> append spaces
		       (viper-repeat nil)))
		    
	      (viper-repeat nil)))))
      (setq vimpulse-visual-insert-coords nil)
    
      ;; update the last two undos
      (if (> nlines 1)
	  (if (eq i-com ?c)
	      (connect-undos 3 buffer-undo-list) ; delete, insert, repeat
	    (connect-undos 2 buffer-undo-list))	 ; insert, repeat
	(if (eq i-com ?c)
	    (connect-undos 2 buffer-undo-list)	 ; delete, insert
	  (connect-undos 1 buffer-undo-list))))) ; insert
  (if (and (/= (char-before (point)) ?\r) 
	   (/= (char-before (point)) ?\n))
      (backward-char 1))) ;; <---------- a[ESC] leaves the cursor where it was before in VIM, without 
                          ;; backward char it advances 1 character.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual block hack:
;; the yank command in visual block now inserts 
;; "\02VimpulseVisualBlockMode\03"
;; in the kill ring. 
;; I chose that string because the presence of 
;; non-printable chars makes it _very_ unlikely
;; to be generated by a normal copy or cut command
;; when it is found in the kill ring, the block
;; is pasted instead. (Alessandro Piras)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless vimpulse-experimental
  (defadvice viper-put-back (around vimpulse-visual-block-put-back (arg) activate)
    (if (equal (current-kill 0) "\02VimpulseVisualBlockMode\03")
	(yank-rectangle)
      ad-do-it)))
;; EXPERIMENTAL: FIX to make pasting when in visual mode work like vim
(when vimpulse-experimental
  (defadvice viper-put-back (around vimpulse-visual-block-put-back (arg) activate)
    (let ((was-in-visual-mode vimpulse-visual-mode))
      (when vimpulse-visual-mode
	(vimpulse-visual-delete-command)
	(current-kill 1)
	(backward-char ))
    (if (equal (current-kill 0) "\02VimpulseVisualBlockMode\03")
	(yank-rectangle)
      ad-do-it)
    (when was-in-visual-mode
      (current-kill -1))))
  )


;; 
;; custom visual selection 
;;

;; Overlay used to highlight the current visual selection
(defvar vimpulse-visual-overlay nil) ;; default value set in vimpulse-visual-mode-toggle
(make-variable-buffer-local 'vimpulse-visual-overlay)

(defun vimpulse-visual-yank-command ()
  "Saves the visual selection in the kill-ring"
  (interactive)
  (cond 
   (vimpulse-visual-mode-block
    (kill-new "\02VimpulseVisualBlockMode\03")
    (vimpulse-visual-mode nil)
					;(rm-kill-ring-save (region-beginning) (region-end))
    (kill-rectangle (region-beginning) (region-end))
    (goto-char (region-beginning))
    (yank-rectangle)
    (goto-char (region-beginning)))
   (t
      (goto-char (overlay-start vimpulse-visual-overlay))
      (vimpulse-set-mark (overlay-end vimpulse-visual-overlay))
      (vimpulse-visual-mode nil)
      (viper-prefix-arg-com ?r 1 ?y))))

(defun vimpulse-visual-indent-command ()
  "Indents the visual selection."
  (interactive)
  (unless vimpulse-visual-mode-block
    (save-excursion
     (indent-region (overlay-start vimpulse-visual-overlay) (overlay-end vimpulse-visual-overlay))
     (vimpulse-visual-mode nil))))

(defun vimpulse-visual-delete-command ()
  "Deletes the visual selection"
  (interactive)
  (cond 
   (vimpulse-visual-mode-block
    (vimpulse-visual-mode nil)
					;(rm-kill-region (region-beginning) (region-end))
    (kill-rectangle (region-beginning) (region-end))
    (goto-char (region-beginning)))
   (t
    (save-excursion
      (goto-char (overlay-start vimpulse-visual-overlay))
      (vimpulse-set-mark (overlay-end vimpulse-visual-overlay))
      (vimpulse-visual-mode nil)
      (viper-prefix-arg-com ?r 1 ?d)))))


(defun vimpulse-visual-change-command ()
  "Called when in visual (block) mode to delete the selected region and go to insert mode"
  (interactive)
  
  (cond 
   (vimpulse-visual-mode-block
    (let ((beg (region-beginning))
	  (end (region-end)))
      (vimpulse-create-coords ?c)
      (kill-rectangle beg end)
      (vimpulse-visual-mode nil) 
      (viper-insert nil)))
   (t
    (goto-char (overlay-start vimpulse-visual-overlay))
    (vimpulse-set-mark (overlay-end vimpulse-visual-overlay))
    (vimpulse-visual-mode nil)
    (viper-prefix-arg-com ?r 1 ?c))))

 
(defun vimpulse-visual-replace-region (&optional arg)
  (interactive "P")
  (goto-char (overlay-start vimpulse-visual-overlay))
  (vimpulse-set-mark (overlay-end vimpulse-visual-overlay))
  (vimpulse-visual-mode 'toggle)
  (cond
   ((= (mark) (point)) nil)
   (t 
    (if (< (mark) (point)) (exchange-point-and-mark))
    (viper-replace-char arg)		
    (let ((c (char-after (point))))
      (dotimes (i (- (mark) (point)))
	(cond
	 ((member (char-after (point)) '(?\r ?\n))
	  (forward-char))
	  (t (delete-char 1)
	     (insert c))))))))

(defun vimpulse-visual-mode-ch-charwise ()
  (interactive)
  (cond
   (vimpulse-visual-mode-linewise
    (setq vimpulse-visual-mode-linewise nil))
   (t
    (vimpulse-visual-mode nil))))
(defun vimpulse-visual-mode-ch-linewise ()
  (interactive)
  (cond
   (vimpulse-visual-mode-block
    (vimpulse-visual-mode nil))
   ((not vimpulse-visual-mode-linewise)
    (setq vimpulse-visual-mode-linewise t))
   (t
    (vimpulse-visual-mode nil))))

(define-key vimpulse-visual-mode-map "v" 'vimpulse-visual-mode-ch-charwise)
(define-key vimpulse-visual-mode-map "V" 'vimpulse-visual-mode-ch-linewise)
(define-key vimpulse-visual-mode-map "\C-v" 'vimpulse-visual-mode)
(define-key vimpulse-visual-mode-map "d" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "x" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "D" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "d" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "y" 'vimpulse-visual-yank-command)
(define-key vimpulse-visual-mode-map "i" 'vimpulse-visual-mode-to-insert-mode)
(define-key vimpulse-visual-mode-map "u" 'vimpulse-visual-mode)
(define-key vimpulse-visual-mode-map "c" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "F" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "c" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "C" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "s" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "S" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "r" 'vimpulse-visual-replace-region)
(define-key vimpulse-visual-mode-map "o" 'exchange-point-and-mark)
(define-key vimpulse-visual-mode-map "O" 'exchange-point-and-mark)
(define-key vimpulse-visual-mode-map "I" 'vimpulse-visual-insert)
(define-key vimpulse-visual-mode-map "A" 'vimpulse-visual-append)
(define-key vimpulse-visual-mode-map "=" 'vimpulse-visual-indent-command)
;; Keys that have no effect in visual mode
(define-key vimpulse-visual-mode-map "t" 'undefined)
(define-key vimpulse-visual-mode-map "." 'undefined)
(define-key vimpulse-visual-mode-map "T" 'undefined)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODIFICATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; advice viper-intercept-ESC-key to exit visual mode with esc 
;;;
(defadvice viper-intercept-ESC-key (around vimpulse-esc-exit-visual-mode activate)
  (message "Buttana")
  (when vimpulse-visual-mode
    (vimpulse-visual-mode nil))
  ad-do-it)
;;
;; vimpulse-visual-overlay-origin: stores the point location where the visual selection started
;;
(defvar vimpulse-visual-overlay-origin nil)
(make-variable-buffer-local 'vimpulse-visual-overlay-origin)

;; functions used for linewise mode
(defun vimpulse-get-line-margins (&optional p)
  "Returns a structure containing the beginning-of-line and end-of-line markers of the line where the merker `p' resides.
If `p' is nil, (point-marker) is used instead. The information can be retrieved using `vimpulse-get-bol' and `vimpulse-get-eol'."
  (save-excursion
    (if p (goto-char p)) 
    (cons (+ 1 (point-at-eol)) (point-at-bol))))
;; accessors 
;; NOTE: THE STRUCTURE RETURNED BY vimpulse-get-line-margins _CAN_ CHANGE TO A MORE OPAQUE ONE IN THE FUTURE. 
;; USE THE PROVIDED ACCESSORS.
(defun vimpulse-get-bol (line-margins)
  "Retrieves the beginning-of-line marker from the structure returned by vimpulse-get-line-margins."
    (cdr line-margins))
(defun vimpulse-get-eol (line-margins)
  "Retrieves the end-of-line marker from the structure returned by vimpulse-get-line-margins."
    (car line-margins))

(defun vimpulse-set-visual-overlay ()
  (setq vimpulse-visual-overlay-origin (point-marker))
  (vimpulse-update-overlay))
      

(defun vimpulse-update-visual-overlay-mode-normal ()
  (let ((pt (point)))
    (if (< pt vimpulse-visual-overlay-origin) 
        (move-overlay vimpulse-visual-overlay 
		      pt 
		      (+ 1 vimpulse-visual-overlay-origin))
      (move-overlay vimpulse-visual-overlay 
		    vimpulse-visual-overlay-origin 
		    (+ 1 pt)))))

(defun vimpulse-update-visual-overlay-mode-linewise ()
  (let ((pt (point-marker)))
    (let ((lm-point (vimpulse-get-line-margins pt))
          (lm-origin (vimpulse-get-line-margins vimpulse-visual-overlay-origin)))
      (cond 
       ((< pt vimpulse-visual-overlay-origin)
	(move-overlay vimpulse-visual-overlay 
		      (vimpulse-get-bol lm-point) 
		      (vimpulse-get-eol lm-origin)))
       (t
	(move-overlay vimpulse-visual-overlay 
		      (vimpulse-get-bol lm-origin) 
		      (vimpulse-get-eol lm-point)))))))
(defun vimpulse-update-overlay ()
  (save-excursion
    (cond 
     (vimpulse-visual-mode-linewise 
      (vimpulse-update-visual-overlay-mode-linewise))
     (t
      (vimpulse-update-visual-overlay-mode-normal)))))
      
(add-hook 'post-command-hook '(lambda ()
				(if (and vimpulse-visual-mode 
					 (not vimpulse-visual-mode-block))
				    (vimpulse-update-overlay))))

(defun vimpulse-set-mark (pos)
  "Sets the region respecting the Emacsen-version, activates highlighting"
  (set-mark pos)
  (when (fboundp 'activate-region) 
    (activate-region))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Force transient-mark-mode to have visual selection ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (when (fboundp 'transient-mark-mode)
    (transient-mark-mode t)))

(defun vimpulse-deactivate-mark ()
  "Deactivates the region respecting the Emacsen-version and type"
  (interactive)
  (if (and vimpulse-visual-mode-block (fboundp 'rm-deactivate-mark))
      (rm-deactivate-mark)
      (viper-deactivate-mark)))

;;;###auto-load
(defun vimpulse-visual-mode-toggle (&optional arg)
  (interactive "P")
  (make-local-variable 'vimpulse-visual-mode-linewise)
  (when (not vimpulse-visual-overlay)
      (setq vimpulse-visual-overlay (make-overlay (point) (point)))
      (delete-overlay vimpulse-visual-overlay)
      (overlay-put vimpulse-visual-overlay 'face (cons 'background-color "blue")))
  (unless vimpulse-visual-mode
    (vimpulse-deactivate-mark)
    (delete-overlay vimpulse-visual-overlay)
    (viper-change-state-to-vi))
  (when vimpulse-visual-mode
    (setq vimpulse-visual-mode-linewise nil)
    (setq vimpulse-visual-mode-block nil)
    (vimpulse-set-visual-overlay)))
    ;(vimpulse-set-mark (point))))


(defun vimpulse-visual-mode-linewise (&optional arg)
  "Starts viper visual mode in `linewise' mode"
  (interactive "P")
  ;(beginning-of-line)
  (vimpulse-visual-mode 'toggle)
  (setq vimpulse-visual-mode-linewise t)
  (setq vimpulse-visual-mode-block nil)
  (vimpulse-set-visual-overlay))
(add-hook 'vimpulse-visual-mode-hook 'vimpulse-visual-mode-toggle t)
(run-hooks 'vimpulse-visual-load-hook)

(defun vimpulse-visual-mode-block (&optional arg)
  "Starts viper visual mode in `block' mode"
  (interactive "P")
  (vimpulse-visual-mode t)
  (setq vimpulse-visual-mode-block t)
  ;; perhaps a bad hack -> rm-set-mark deactivates the normal mark
  (when (fboundp 'rm-set-mark) 
    (rm-set-mark nil)))

;; We need to detect when a command has deactivated the mark so that
;; Vimpulse is able to exit Visual mode
(defun vimpulse-detect-mark-deactivate ()
  (when (and vimpulse-visual-mode (not mark-active))
    (vimpulse-visual-mode 'toggle)))
(add-hook 'deactivate-mark-hook 'vimpulse-detect-mark-deactivate)
;;; }}} End visual mode code

;;;
;;; parens matching LOADED BY DEFAULT
;;; set vimpulse-enhanced-paren-matching nil in your .emacs before requiring vimpulse to avoid loading it
;;;

;;; Begin Paren Matching Code {{{
(unless (boundp 'vimpulse-enhanced-paren-matching)  ;; Enhanced paren matching is enabled by default. To disable it 
  (setq vimpulse-enhanced-paren-matching t))       ;; just add (setq vimpulse-enhanced-paren-matching nil) to your .emacs

(when vimpulse-enhanced-paren-matching
  (require 'paren)
  (show-paren-mode 't) ;; enable the normal paren match highlight 
  
  (defvar vimpulse-paren-overlay-open nil) 		;;overlay used to highlight the opening paren
  (defvar vimpulse-paren-overlay-close nil)		;; overlay used to highlight the closing paren
  (make-variable-buffer-local 'vimpulse-paren-overlay-open) ;; overlays are buffer local.
  (make-variable-buffer-local 'vimpulse-paren-overlay-close)
  
  (defun vimpulse-pm-parenp (pos)
    (let ((class (syntax-class (syntax-after pos))))
      (or (= class 4) (= class 5))))
  (defun vimpulse-pm-open-parenp (pos)
    "Returns t if the character at position `pos' is an opening paren."
    (let ((class (syntax-class (syntax-after pos))))
      (= 4 class)))
  (defun vimpulse-pm-close-parenp (pos)
    "Returns t if the character at position `pos' is an closing paren."
    (let ((class (syntax-class (syntax-after pos))))
      (= 5 class)))

  (defun vimpulse-pm-get-candidate-pos (pos)
    "Returns the position of the possible matching paren of the character at position `pos'
if it's a paren, 'not-a-paren if it's not a paren, nil if no match is found."
    (let ((result nil))
      (condition-case ()
	  (cond 
	   ((vimpulse-pm-open-parenp pos)
	    (setq result (1- (scan-sexps pos 1))))
	   ((vimpulse-pm-close-parenp pos)
	    (setq result (scan-sexps (1+ pos) -1)))
	   (t 
	    (setq result 'not-a-paren)))
	(error (setq result nil)))
      result))
  
  (defun vimpulse-pm-is-real-match (pos1 pos2)
    "Checks the characters at position `pos1' and `pos2' and returns t if they are matching 
characters (in a paren match meaning), nil otherwise."
    (destructuring-bind ((class1 . match1) (class2 . match2)) 
	(list (syntax-after pos1) (syntax-after pos2))
      (or (eq match1 (char-after pos2))
	  (eq match2 (char-after pos1))
	  (eq match1 match2))))
  
  (defun vimpulse-pm-highlight-pos (pos face)
    "Highlights the paren at pos `pos' using `face'."
    (let ((ovl (if (vimpulse-pm-open-parenp pos) 
		   vimpulse-paren-overlay-open 
		 vimpulse-paren-overlay-close)))
      (overlay-put ovl 'face face)
      (move-overlay ovl pos (1+ pos))))
  
  (defun vimpulse-pm-show-paren ()
    "Paren matching routine. Highlights the paren at (point) and the eventual 
matching paren, or mismatched paren." ;;FIXME: this description sucks.
    (let ((candidate-pos (vimpulse-pm-get-candidate-pos (point))))
      (cond 
       ((not candidate-pos)
	(vimpulse-pm-highlight-pos (point) 'show-paren-mismatch))
       ((eq candidate-pos 'not-a-paren)
	(delete-overlay vimpulse-paren-overlay-open)
	(delete-overlay vimpulse-paren-overlay-close))
       (t
	(let ((pos-1 (vimpulse-pm-get-candidate-pos candidate-pos)))
	  (cond 
	   ((/= (point) pos-1)
	    (vimpulse-pm-highlight-pos (point) 'show-paren-mismatch))
	   ((vimpulse-pm-is-real-match candidate-pos pos-1)
	    (vimpulse-pm-highlight-pos (point) 'show-paren-match)
	    (vimpulse-pm-highlight-pos candidate-pos 'show-paren-match))
	   (t
	    (vimpulse-pm-highlight-pos (point) 'show-paren-mismatch)
	    (vimpulse-pm-highlight-pos candidate-pos 'show-paren-mismatch))))))))
	    
  ;;;
  ;;; We advice show-paren-function and use it for insert mode
  ;;;
  ;;; TODO: check if it's a problem using this paren matching function in replace mode
  ;;;
  (defadvice show-paren-function (around vimpulse-parenmatching activate)
    (unless vimpulse-paren-overlay-open ;; define overlays if they don't exist
      (setq vimpulse-paren-overlay-open (make-overlay (point) (point)))
      (setq vimpulse-paren-overlay-close (make-overlay (point) (point)))
      (delete-overlay vimpulse-paren-overlay-open)
      (delete-overlay vimpulse-paren-overlay-close))
    (cond 
     ((and show-paren-mode viper-mode (not (eq viper-current-state 'insert-state))) ;; viper not in insert mode
      (when (boundp 'show-paren-overlay)                                            ;; we delete the overlays used by show-paren-function
	(delete-overlay show-paren-overlay)                                         ;; and call the custom paren-matching function
	(delete-overlay show-paren-overlay-1))
      (vimpulse-pm-show-paren))
     (t                                                 ;; viper in insert mode 
      (delete-overlay vimpulse-paren-overlay-open)      ;; delete the overlays used by the custom function
      (delete-overlay vimpulse-paren-overlay-close)
      ad-do-it)))                                       ;; call the adviced function
  
  )
;;; }}} End Paren Matching code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; EXPERIMENTAL    
;;;; Text Object Support:
;;;; This code implements the support for text objects, 
;;;; and implements commands like diw daw ciw caw.
;;;; It's still experimental, and not all that's supported in vim
;;;; is (still) supported here. However, the most common text objects 
;;;; are supported: 
;;;;    - paren blocks: { [ ( < > ) ] }
;;;;    - sentences
;;;;    - paragraphs
;;;;    - quoted expressions " and '
;;;;    - words and Words
;;;; Using text objects as motions in visual mode is (still) not supported.
;;;; Please note that Vimpulse's text objects are very close to Vim's, but
;;;; the behavior on certain occasions (e.g. daw issued with the cursor 
;;;; lying on whitespace) may be a little different. My aim was not acheiving 
;;;; the exact same behavior in all limit cases, but rather to give a close 
;;;; and consistent behavior to the commands.
;;;; Alessandro Piras
;;; Begin Text Objects code{{{
(when vimpulse-experimental
  (defun vimpulse-get-syntaxes-bounds (pos syntaxes)
    "Returns the bounds of contiguous character that match `syntaxes', 
where syntaxes is an emacs' syntax specification."
    (let ((result))
      (save-excursion
	(goto-char pos)
	(skip-syntax-forward syntaxes)
	(push (1- (point)) result)
	(skip-syntax-backward syntaxes)
	(cons (point) result))))



  (defvar vimpulse-paren-matching-table
    (make-hash-table)
    "Table used for paren matching:
table[key] = (match . opening-paren)"
    )
  (puthash ?\( 
	   '( ?\) . ?\( )
	   vimpulse-paren-matching-table)
  (puthash ?\) 
	   '( ?\( . ?\( )
	   vimpulse-paren-matching-table)
  (puthash ?{ 
	   '( ?} . ?\{ )
	   vimpulse-paren-matching-table)
  (puthash ?} 
	   '( ?{ . ?\{ ) 
	   vimpulse-paren-matching-table)
  (puthash ?\[ 
	   '( ?\] . ?\[)
	   vimpulse-paren-matching-table)
  (puthash ?\] 
	   '( ?\[ . ?\[ )
	   vimpulse-paren-matching-table)
  (puthash ?\< 
	   '( ?\> . ?\< )
	   vimpulse-paren-matching-table)
  (puthash ?\> 
	   '( ?\< . ?\< )
	   vimpulse-paren-matching-table)

  (defun vimpulse-skip-until-delimiters (pos paren match limb lime dir)
    "Skips all the character different from `paren' and `match' starting 
from `pos' following the direction `dir', with pos in [limb, lime]."
    (let ((pos-1 pos))
      (while (and (/= (char-after pos-1) paren)
		  (/= (char-after pos-1) match)
		  (or (and (= dir -1) (/= pos-1 limb)) ;; reached limits
		      (and (= dir 1) (/= pos-1 lime))))
	(setq pos-1 (+ dir pos-1)))
      pos-1))

  (defun vimpulse-find-first-unbalanced-1 (pos paren match limb lime dir)
    "Finds the first unbalanced `paren' following the direction `dir', starting 
from position `pos'. `match' is the paren that matches with `paren', limb is the 
lower bound of the position, lime is the upper bound to the position."
    (message "searching at %s" pos)
    (cond
     ((or (eq pos 'not-found))
      'not-found)
     ((= (char-after pos) paren) 
      pos)
     ((or (and (= dir -1) (= pos limb)) ;; reached limits
	  (and (= dir 1) (= pos lime)))
      'not-found)
     ((= (char-after pos) match) ;;
      (let ((pos-1 (vimpulse-find-first-unbalanced-1 (+ dir pos) paren match limb lime dir)))
	(vimpulse-find-first-unbalanced-1 (+ dir pos-1) paren match limb lime dir)))
     (t 
      (let ((pos-1 (vimpulse-skip-until-delimiters pos paren match limb lime dir)))
	(vimpulse-find-first-unbalanced-1 pos-1 paren match limb lime dir)))))

  (defvar vimpulse-balanced-bounds-char-list 
    (list 
     ?\( ?\) ?\[ ?\] ?\{ ?\} ?\< ?\>)
    "Parens supported by the text-object system.")

  (defun vimpulse-get-balanced-bounds (pos paren)
    "Returns the boundaries of a balanced expression."
    (let* ((limb (point-min))
	   (lime (1- (point-max)))
	   (paren-o (cdr (gethash paren vimpulse-paren-matching-table)))
	   (paren-c (car (gethash paren-o vimpulse-paren-matching-table)))
	   (pos-o (vimpulse-find-first-unbalanced-1 pos paren-o paren-c limb lime -1))
	   (pos-c (vimpulse-find-first-unbalanced-1 (if (integerp pos-o) (1+ pos-o) pos-o) paren-c paren-o limb lime 1)))
      (cond 
       ((eq pos-c 'not-found)
	nil)
       (t
	(list pos-o pos-c)))))

  (defun vimpulse-get-vword-bounds (pos)
    "Returns the boundaries of a word."
    (let ((syntaxes)
	  (syntax (char-syntax (char-after pos))))
      (cond
       ((= syntax ?\))
	(setq syntaxes (string syntax ?\()))
       ((= syntax ?\()
	(setq syntaxes (string syntax ?\))))
       (t 
	(setq syntaxes (string syntax))))
      (vimpulse-get-syntaxes-bounds pos syntaxes)))

  (defun vimpulse-get-vWord-bounds (pos)
    "Returns the boundaries of a Word."
    (let ((result))
      (save-excursion 
	(goto-char pos)
	(re-search-forward "[?\n?\r[:space:]]")
	(push (- (point) 2) result)
	(backward-char)
	(re-search-backward "[?\n?\r[:space:]]")
	(cons (1+ (point)) result))))

  (defun vimpulse-get-sentence-bounds (pos)
    "Returns the boundaries of a sentence."
    (let ((result))
      (save-excursion
	(goto-char pos)
	(when (not (posix-search-forward "\\(^\r\\|^\n\\|\\.[\n\r]?\\)" (point-max) t))
	  (goto-char (point-max)))
	(push (1- (point)) result)
	(backward-char (length (match-string 0)))
	(cond 
	 ((not (posix-search-backward "\\(^\r\\|^\n\\|\\.[\r\n]?\\)" (point-min) t))
	  (goto-char (point-min)))
	 (t 
	  (forward-char 1)))
	(posix-search-forward "\\(\n\\|\r\\|[[:space:]]\\)*" (point-max) t)
	(push (point) result))))

  (defun vimpulse-get-paragraph-bounds (pos)
    "Returns the boundaries of a paragraph."
    (let ((result))
      (save-excursion
	(goto-char pos)
	(when (not (re-search-forward "\\(^\r\\|^\n\\)" (point-max) t))
	  (goto-char (point-max)))
	(push (- (point) 2) result)
	(backward-char (length (match-string 0)))
	(cond 
	 ((not (re-search-backward "\\(^\r\\|^\n\\)" (point-min) t))
	  (goto-char (point-min)))
	 (t 
	  (forward-char 1)))
	(push (point) result))))

  (defun vimpulse-get-paired-bounds (pos char)
    "Returns the boundaries of a `char'-quoted expression."
    (save-excursion
      (goto-char pos)
      (if (= (char-before (point)) ?\\) (backward-char))
      (let ((result))
	(when (re-search-forward (concat "[^\\\\]" (string char)) (point-max) t)
	  (push (1- (point)) result)
	  (condition-case ()
	      (push (scan-sexps (point) -1) result)
	    (error (setq result nil))))
	result)))
  
  (defvar vimpulse-paired-expression-delimiters (list ?\" ?\')
    "Quotes supported by the text-object system.")

  (defun vimpulse-get-text-object-bounds-i (pos char)
    "Returns the inner boundaries of a text object at point `pos'.
`char' identifies the text object:
  - w -> word
  - W -> Word
  - s -> sentence
  - p -> paragraph
  - <paren> -> paren block (see variable `vimpulse-paren-matching-table'
               to see the supported parens.
  - <quote> -> quoted expression (see variable `paired-expression-delimiter'
               to see the type of quotes supported."
    (cond
     ((= char ?w) (vimpulse-get-vword-bounds pos))
     ((= char ?W) (vimpulse-get-vWord-bounds pos))
     ((= char ?s) (vimpulse-get-sentence-bounds pos))
     ((= char ?p) (vimpulse-get-paragraph-bounds pos))
     ((memq char vimpulse-paired-expression-delimiters)
      (let ((bounds (vimpulse-get-paired-bounds pos char)))
	(message "Bounds: %s" bounds)
	(if bounds 
	    (destructuring-bind (s e) bounds
	      (list (1+ s) (1- e)))
	  nil)))
     ((memq char vimpulse-balanced-bounds-char-list) 
      (let ((bounds (vimpulse-get-balanced-bounds pos char)))
	(if bounds 
	    (destructuring-bind (s e) bounds
	      (list (1+ s) (1- e)))
	  nil)))
     (t nil)))

  (defun vimpulse-get-bounds-with-whitespace (func pos &optional trailing-newlines)
    "Given a function that returns inner boundaries, returns a boundary that includes 
the whitespace needed to get the \"a\" behavior. The logic 
followed is the same:
  - include all whitespace and newlines before the text object
  - include the text object
  - include trailing whitespace
  - if trailing-newlines is t, include also the trailing newlines"
    (save-excursion
      (let ((start (point))
	    (end nil))
	
	(skip-chars-forward "[:space:]\n\r")
	(let ((bounds (apply func  (list (point)))))
	  (cond
	   (bounds
	    (goto-char (1+ (cadr bounds)))
	    (skip-chars-forward (concat "[:space:]" (if trailing-newlines "\n\r" "")))
	    (list (min start (car bounds)) (1- (point))))
	   (t nil))))))

  
  (defun vimpulse-get-text-object-bounds-a (pos char)
    "Returns the boundaries of `a' text object, whitespace to be killed included."
    (cond
     ((= char ?w) 
      (vimpulse-get-bounds-with-whitespace 'vimpulse-get-vword-bounds pos))
     ((= char ?W) (vimpulse-get-bounds-with-whitespace 'vimpulse-get-vWord-bounds pos))
     ((= char ?s) (vimpulse-get-bounds-with-whitespace 'vimpulse-get-sentence-bounds pos))
     ((= char ?p) (vimpulse-get-bounds-with-whitespace 'vimpulse-get-paragraph-bounds pos t))
     ((memq char vimpulse-paired-expression-delimiters)
      (vimpulse-get-paired-bounds pos char))
     ((memq char vimpulse-balanced-bounds-char-list) 
      (vimpulse-get-balanced-bounds pos char))
     (t nil)))

  ;;
  ;; Thanks to the anonymous poster for the idea on how to modify the viper 
  ;; function to add the di da ci and ca partial commands.
  ;;


  ;; REDEFINITION OF VIPER FUNCTION
  ;;
  ;; `viper-prefix-arg-com', originally defined in viper-cmd.el, does
  ;; much of the work of reading keyboard input and chosing the
  ;; appropriate command. As an ugly way of getting basic "delete inner
  ;; parentheses" functionality, we extend it here with entries for our
  ;; custom `vimpulse-di' and `vimpulse-ci' functions (defined below).
  ;;
  ;; This should be done in a cleaner way. Michael Kifer gives some
  ;; hints in viper.el:
  ;;
  ;;     Some of the code that is inherited from VIP-3.5 is rather
  ;;     convoluted. Instead of viper-command-argument, keymaps should
  ;;     bind the actual commands. E.g., "dw" should be bound to a
  ;;     generic command viper-delete that will delete things based on
  ;;     the value of last-command-char. This would greatly simplify the
  ;;     logic and the code.
  ;;
  ;; For (some) brewity, Kifer's comments are removed. The added lines
  ;; are annotated with ";; MODIFICATION".

  (defun viper-prefix-arg-com (char value com)
    (let ((cont t)
	  cmd-info
	  cmd-to-exec-at-end)
      (while (and cont
		  (viper-memq-char char
				   (list ?i ?a ?c ?d ?y ?! ?< ?> ?= ?# ?r ?R ?\" ;;modified ?i ?a
					 viper-buffer-search-char)))
	(if com
	    ;; this means that we already have a command character, so we
	    ;; construct a com list and exit while.  however, if char is "
	    ;; it is an error.
	    (progn
	      ;; new com is (CHAR . OLDCOM)
	      (if (viper-memq-char char '(?# ?\")) (error "Viper bell"))
	      (setq com (cons char com))
	      (setq cont nil))
	  ;; If com is nil we set com as char, and read more.  Again, if char is
	  ;; ", we read the name of register and store it in viper-use-register.
	  ;; if char is !, =, or #, a complete com is formed so we exit the while
	  ;; loop.
	  (cond ((viper-memq-char char '(?! ?=))
		 (setq com char)
		 (setq char (read-char))
		 (setq cont nil))
		((viper= char ?#)
		 ;; read a char and encode it as com
		 (setq com (+ 128 (read-char)))
		 (setq char (read-char)))
		((viper= char ?\")
		 (let ((reg (read-char)))
		   (if (viper-valid-register reg)
		       (setq viper-use-register reg)
		     (error "Viper bell"))
		   (setq char (read-char))))
		(t
		 (setq com char)
		 (setq char (read-char))))))

      (if (atom com)
	  ;; `com' is a single char, so we construct the command argument
	  ;; and if `char' is `?', we describe the arg; otherwise
	  ;; we prepare the command that will be executed at the end.
	  (progn
	    (setq cmd-info (cons value com))
	    (while (viper= char ?U)
	      (viper-describe-arg cmd-info)
	      (setq char (read-char)))
	    ;; `char' is a movement cmd, a digit arg cmd, or a register cmd---so
	    ;; we execute it at the very end
	    (or (viper-movement-command-p char)
		(viper-digit-command-p char)
		(viper-regsuffix-command-p char)
		(viper= char ?!) ; bang command
		(viper= char ?g) ; the gg command (like G0)
		(error "Viper bell"))
	    (setq cmd-to-exec-at-end
		  (viper-exec-form-in-vi
		   `(key-binding (char-to-string ,char)))))

	;; as com is non-nil, this means that we have a command to execute
	(if (viper-memq-char (car com) '(?r ?R))
	    ;; execute apropriate region command.
	    (let ((char (car com)) (com (cdr com)))
	      (setq prefix-arg (cons value com))
	      (if (viper= char ?r)
		  (viper-region prefix-arg)
		(viper-Region prefix-arg))
	      ;; reset prefix-arg
	      (setq prefix-arg nil))
	  ;; otherwise, reset prefix arg and call appropriate command
	  (setq value (if (null value) 1 value))
	  (setq prefix-arg nil)
	  (cond
	   ;; If we change ?C to ?c here, then cc will enter replacement mode
	   ;; rather than deleting lines.  However, it will affect 1 less line
	   ;; than normal.  We decided to not use replacement mode here and
	   ;; follow Vi, since replacement mode on n full lines can be achieved
	   ;; with nC.
	   ((equal com '(?a . ?d)) (vimpulse-da)) ; da<x>
	   ((equal com '(?a . ?c)) (vimpulse-da)) ; ca<x>
	   ((equal com '(?i . ?d)) (vimpulse-di)) ; di<x>
	   ((equal com '(?i . ?c)) (vimpulse-ci)) ; ci<x>
	   ((equal com '(?c . ?c)) (viper-line (cons value ?C)))
	   ((equal com '(?d . ?d)) (viper-line (cons value ?D)))
	   ((equal com '(?d . ?y)) (viper-yank-defun))
	   ((equal com '(?y . ?y)) (viper-line (cons value ?Y)))
	   ((equal com '(?< . ?<)) (viper-line (cons value ?<)))
	   ((equal com '(?> . ?>)) (viper-line (cons value ?>)))
	   ((equal com '(?! . ?!)) (viper-line (cons value ?!)))
	   ((equal com '(?= . ?=)) (viper-line (cons value ?=)))
	   ;; gg  acts as G0
	   ((equal (car com) ?g)   (viper-goto-line 0))
	   (t (error "Viper bell")))))
      
      (if cmd-to-exec-at-end
	  (progn
	    (setq last-command-event
		  (viper-copy-event
		   (if (featurep 'xemacs) (character-to-event char) char)))
	    (condition-case err
		(funcall cmd-to-exec-at-end cmd-info)
	      (error
	       (error "%s" (error-message-string err))))))
      ))
  
;;;;;;;;;;;;;;;;;;;;
;;;   Commands   ;;;
;;;;;;;;;;;;;;;;;;;;
  (defun vimpulse-da ()
    "Deletes `a' text object."
    (interactive)
    (let ((bounds (vimpulse-get-text-object-bounds-a (point) (read-char))))
      (message "Bounds : %s" bounds)
      (when bounds
	(goto-char (car bounds))
	(set-mark (1+ (cadr bounds)))
	(call-interactively 'kill-region)
	)))
  
  (defun vimpulse-di ()
    "Deletes `inner' text object."
    (interactive)
    (let ((bounds (vimpulse-get-text-object-bounds-i (point) (read-char))))
      (message "Bounds : %s" bounds)
      (when bounds
	(goto-char (car bounds))
	(set-mark (1+ (cadr bounds)))
	(call-interactively 'kill-region))))

  (defun vimpulse-ci ()
    "Changes `inner' text object."
    (interactive) ; if you want to bind it yourself ... feel free to
    (vimpulse-di)
    (call-interactively 'viper-insert))

  (defun vimpulse-ca ()
    "Changes `a' text object."
    (interactive) ; if you want to bind it yourself ... feel free to
    (vimpulse-da)
    (call-interactively 'viper-insert))

  (when nil ;; TODO: check if this is necessary for old emacsen, maybe they don't support featurep
    (defun viper-prefix-arg-com (char value com)
      (let ((cont t)
	    cmd-info
	    cmd-to-exec-at-end)
	(while (and cont
		    (viper-memq-char char
				     (list ?i ?c ?d ?y ?! ?< ?> ?= ?# ?r ?R ?\"
					   viper-buffer-search-char)))
	  (if com
	      (progn
		(if (viper-memq-char char '(?# ?\")) (error ""))
		(setq com (cons char com))
		(setq cont nil))
	    (cond ((viper-memq-char char '(?! ?=))
		   (setq com char)
		   (setq char (read-char))
		   (setq cont nil))
		  ((viper= char ?#)
		   (setq com (+ 128 (read-char)))
		   (setq char (read-char)))
		  ((viper= char ?\")
		   (let ((reg (read-char)))
		     (if (viper-valid-register reg)
			 (setq viper-use-register reg)
		       (error ""))
		     (setq char (read-char))))
		  (t
		   (setq com char)
		   (setq char (read-char))))))

	(if (atom com)
	    (progn
	      (setq cmd-info (cons value com))
	      (while (viper= char ?U)
		(viper-describe-arg cmd-info)
		(setq char (read-char)))
	      (or (viper-movement-command-p char)
		  (viper-digit-command-p char)
		  (viper-regsuffix-command-p char)
		  (viper= char ?!)
		  (viper= char ?g)
		  (error ""))
	      (setq cmd-to-exec-at-end
		    (viper-exec-form-in-vi
		     `(key-binding (char-to-string ,char)))))

	  (if (viper-memq-char (car com) '(?r ?R))
	      (let ((char (car com)) (com (cdr com)))
		(setq prefix-arg (cons value com))
		(if (viper= char ?r)
		    (viper-region prefix-arg)
		  (viper-Region prefix-arg))
		(setq prefix-arg nil))
	    (setq value (if (null value)
			    1
			  value))
	    (setq prefix-arg nil)
	    (cond
	     ;; MODIFICATION: `di' and `ci' added to list
	     ((equal com '(?a . ?d)) (vimpulse-da)) ; da<x>
	     ((equal com '(?a . ?c)) (vimpulse-da)) ; ca<x>
	     ((equal com '(?i . ?d)) (vimpulse-di)) ; di<x>
	     ((equal com '(?i . ?c)) (vimpulse-ci)) ; ci<x>
	     ((equal com '(?c . ?c)) (viper-line (cons value ?C)))
	     ((equal com '(?d . ?d)) (viper-line (cons value ?D)))
	     ((equal com '(?d . ?y)) (viper-yank-defun))
	     ((equal com '(?y . ?y)) (viper-line (cons value ?Y)))
	     ((equal com '(?< . ?<)) (viper-line (cons value ?<)))
	     ((equal com '(?> . ?>)) (viper-line (cons value ?>)))
	     ((equal com '(?! . ?!)) (viper-line (cons value ?!)))
	     ((equal com '(?= . ?=)) (viper-line (cons value ?=)))
	     ((equal (car com) ?g)   (viper-goto-line 0))
	     (t (error "")))))

	(if cmd-to-exec-at-end
	    (progn
	      (setq last-command-char char)
	      (setq last-command-event
		    (viper-copy-event
		     (if viper-xemacs-p (character-to-event char) char)))
	      (condition-case nil
		  (funcall cmd-to-exec-at-end cmd-info)
		(error
		 (error "")))))))
    )
  )
;;; }}} End Text Objects code
