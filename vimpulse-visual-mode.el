;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This file contains All the code relative to visual mode.  ;;;;;
;;;; Visual mode is implemented as a minor mode.               ;;;;; 
;;;; Currently, visual selection highlighting is done through  ;;;;;
;;;; the use of overlays for linewise and characterwise modes, ;;;;;
;;;; while for blockwise mode, rect-mark.el is needed.         ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Minor Mode code ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local variables
(eval-when-compile (require 'easy-mmode))
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
(defvar vimpulse-visual-current-register nil)
(defcustom vimpulse-visual-load-hook nil
  "Hooks to run after loading vimpulse-visual-mode."
  :type 'hook
  :group 'vimpulse-visual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions related to visual selection activation,     ;;;
;;; mode of operation change (character-wise, block-wise, ;;;
;;; line-wise)                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This advice is needed to know whether the next object to 
;; paste is a rectangle.
(defadvice kill-new (around notita (string &optional replace yank-handler) activate)
  (setq vimpulse-last-yank nil)
  ad-do-it)

(defun vimpulse-visual-mode-ch-charwise ()
  "Starts Visual selection in character-wise mode or sets the current
mode of operation to character-wise if Visual selection is already started."
  (interactive)
  (cond
   (vimpulse-visual-mode-linewise
    (setq vimpulse-visual-mode-linewise nil))
   (t
    (vimpulse-visual-mode nil))))

(defun vimpulse-visual-mode-ch-linewise ()
  "Starts Visual selection in line-wise mode or sets the current
mode of operation to line-wise if Visual selection is already started."
  (interactive)
  (cond
   (vimpulse-visual-mode-block
    (vimpulse-visual-mode nil))
   ((not vimpulse-visual-mode-linewise)
    (setq vimpulse-visual-mode-linewise t))
   (t
    (vimpulse-visual-mode nil))))

(defun vimpulse-set-mark (pos)
  "Sets the region respecting the Emacsen-version, activates highlighting"
  (set-mark pos)
  (when (fboundp 'activate-region) 
    (activate-region))
  ;; Force transient-mark-mode to have visual selection 
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
    (setq vimpulse-visual-current-register nil)
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
;;;;;;;;;;;;;;;;;;;;
;;; Key bindings ;;;
;;;;;;;;;;;;;;;;;;;;
(define-key vimpulse-visual-mode-map "v" 'vimpulse-visual-mode-ch-charwise)
(define-key vimpulse-visual-mode-map "V" 'vimpulse-visual-mode-ch-linewise)
(define-key vimpulse-visual-mode-map "\C-v" 'vimpulse-visual-mode)
(define-key vimpulse-visual-mode-map "d" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "x" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "D" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "d" 'vimpulse-visual-delete-command)
(define-key vimpulse-visual-mode-map "y" 'vimpulse-visual-yank-command)
(define-key vimpulse-visual-mode-map "u" 'vimpulse-visual-mode)
(define-key vimpulse-visual-mode-map "c" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "F" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "c" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "C" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "s" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "S" 'vimpulse-visual-change-command)
(define-key vimpulse-visual-mode-map "r" 'vimpulse-visual-replace-region)
(define-key vimpulse-visual-mode-map "\"" 'vimpulse-visual-set-current-register)
(define-key vimpulse-visual-mode-map "o" 'vimpulse-invert-origin-and-cursor)
(define-key vimpulse-visual-mode-map "O" 'vimpulse-invert-origin-and-cursor)
(define-key vimpulse-visual-mode-map "I" 'vimpulse-visual-insert)
(define-key vimpulse-visual-mode-map "A" 'vimpulse-visual-append)
(define-key vimpulse-visual-mode-map "=" 'vimpulse-visual-indent-command)
;; Keys that have no effect in visual mode
(define-key vimpulse-visual-mode-map "t" 'undefined)
(define-key vimpulse-visual-mode-map "." 'undefined)
(define-key vimpulse-visual-mode-map "T" 'undefined)
;; advice viper-intercept-ESC-key to exit visual mode with esc 
(defadvice viper-intercept-ESC-key (around vimpulse-esc-exit-visual-mode activate)
  (when (and vimpulse-visual-mode
	     (not (input-pending-p)))
    (vimpulse-visual-mode nil))
  ad-do-it)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual selection visualization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Overlay used to highlight the current visual selection
(defvar vimpulse-visual-overlay nil) ;; default value set in vimpulse-visual-mode-toggle
(make-variable-buffer-local 'vimpulse-visual-overlay)

;; vimpulse-visual-overlay-origin: stores the point location where the visual selection started
(defvar vimpulse-visual-overlay-origin nil)
(make-variable-buffer-local 'vimpulse-visual-overlay-origin)

;; functions used for linewise mode to access line margins
;; note: the structure returned by vimpulse-get-line-margins _can_ change to a more opaque one in the future. 
;; use the provided accessors.
(defun vimpulse-get-line-margins (&optional p)
  "Returns a structure containing the beginning-of-line and end-of-line 
markers of the line where the merker `p' resides. If `p' is nil, 
(point-marker) is used instead. The information can be retrieved using 
`vimpulse-get-bol' and `vimpulse-get-eol'."
  (save-excursion
    (if p (goto-char p)) 
    (list (point-at-bol) (point-at-eol))))
(defun vimpulse-get-bol (line-margins)
  "Retrieves the beginning-of-line marker from the structure returned by vimpulse-get-line-margins."
    (car line-margins))
(defun vimpulse-get-eol (line-margins)
  "Retrieves the end-of-line marker from the structure returned by vimpulse-get-line-margins."
    (cadr line-margins))

(defun vimpulse-set-visual-overlay ()
  (setq vimpulse-visual-overlay-origin (point-marker))
  (vimpulse-update-overlay))
      
(defun vimpulse-get-vs-bounds ()
  (list (overlay-start vimpulse-visual-overlay) (overlay-end vimpulse-visual-overlay)))
(defun vimpulse-get-vs-start ()
  (overlay-start vimpulse-visual-overlay))
(defun vimpulse-get-vs-end ()
  (overlay-end vimpulse-visual-overlay))

(defvar vimpulse-vs-start-marker (make-marker))
(defvar vimpulse-vs-end-marker (make-marker))

;; This is a kludge. It's intended to emulate vim's behavior when
;; issuing : when visual selecting. To implement the kludge, 
;; viper-ex is redefined, see viper-function-redefinitions.el
;; furthermore, this function has to be called from vimpulse-update-overlay.
(defun vimpulse-set-vs-registers ()
  (set-marker vimpulse-vs-start-marker (vimpulse-get-vs-start))
  (set-marker vimpulse-vs-end-marker (1- (vimpulse-get-vs-end)))
  (set-register (viper-int-to-char (1+ (- ?y ?a))) vimpulse-vs-start-marker)
  (set-register (viper-int-to-char (1+ (- ?z ?a))) vimpulse-vs-end-marker))

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
		      (1+ (vimpulse-get-eol lm-origin))))
       (t
	(move-overlay vimpulse-visual-overlay 
		      (vimpulse-get-bol lm-origin) 
		      (1+ (vimpulse-get-eol lm-point))))))))

(defun vimpulse-update-overlay ()
  (save-excursion
    (cond 
     (vimpulse-visual-mode-linewise 
      (vimpulse-update-visual-overlay-mode-linewise))
     (t
      (vimpulse-update-visual-overlay-mode-normal)))
    (vimpulse-set-vs-registers)))
      
(add-hook 'post-command-hook '(lambda ()
				(if (and vimpulse-visual-mode 
					 (not vimpulse-visual-mode-block))
				    (vimpulse-update-overlay))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Destructive commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-delete-command ()
  "Deletes the visual selection"
  (interactive)
  (cond 
   (vimpulse-visual-mode-block
    (vimpulse-visual-mode nil)
    (setq vimpulse-last-yank 'rectangle)
    ;;(rm-kill-region (region-beginning) (region-end))
    (kill-rectangle (region-beginning) (region-end))
    (goto-char (region-beginning)))
   (t
    (goto-char (vimpulse-get-vs-start))
    (let ((count (if vimpulse-visual-mode-linewise (count-lines (vimpulse-get-vs-start) (vimpulse-get-vs-end)) 1))
	  (char (if vimpulse-visual-mode-linewise ?l ?r))
	  (motion (unless vimpulse-visual-mode-linewise (vimpulse-get-vs-bounds))))
      (viper-set-destructive-command (list 'vimpulse-delete-text-objects-function 
					   (list count char motion) ?d nil nil nil))
      (vimpulse-delete-text-objects-function (cons (list count char motion) ?d))))))

(defun vimpulse-visual-indent-command ()
  "Indents the visual selection."
  (interactive)
  (unless vimpulse-visual-mode-block
    (save-excursion
     (indent-region (vimpulse-get-vs-start) (vimpulse-get-vs-end))
     (vimpulse-visual-mode nil))))


(defun vimpulse-visual-change-command ()
  "Called when in visual (block) mode to delete the selected region and go to insert mode"
  (interactive)
  
  (cond 
   (vimpulse-visual-mode-block
    (let ((beg (region-beginning))
	  (end (region-end)))
      (setq vimpulse-last-yank 'rectangle)
      (vimpulse-create-coords ?c)
      (kill-rectangle beg end)
      (vimpulse-visual-mode nil) 
      (viper-insert nil)))
   (t
    (goto-char (vimpulse-get-vs-start))
    (let ((count (if vimpulse-visual-mode-linewise (count-lines (vimpulse-get-vs-start) (vimpulse-get-vs-end)) 1))
	  (char (if vimpulse-visual-mode-linewise ?l ?r))
	  (motion (unless vimpulse-visual-mode-linewise (vimpulse-get-vs-bounds))))
      (viper-set-destructive-command (list 'vimpulse-change-text-objects-function 
					   (list count char motion) ?d viper-use-register nil nil))
      (vimpulse-delete-text-objects-function (cons (list count char motion) ?c))
      (vimpulse-visual-mode nil)
      (when (= char ?l)
	(open-line 1))
      (viper-change-state-to-insert)))))

(defun vimpulse-replace-chars-in-selection-function (arg)
  "Fills the text in the region identified by COUNT, VISUAL-MODE and MOTION 
with the CHAR character, without replacing the newlines."
  (destructuring-bind (count visual-mode motion char) (car arg)
    (let ((bounds (vimpulse-unify-multiple-bounds (point) visual-mode count motion)))
      (when bounds
	(goto-char (car bounds))
	(save-excursion
	  (dotimes (i (1+ (- (cadr bounds) (car bounds))))
	    (unless (memq (char-after (point)) '(?\r ?\n))
	      (delete-char 1)
	      (insert char))))))))
	      
(defun vimpulse-visual-replace-region (&optional arg)
  (interactive)
  (cond
   ((not vimpulse-visual-mode-block)
    
    (let ((count (if vimpulse-visual-mode-linewise (count-lines (vimpulse-get-vs-start) (vimpulse-get-vs-end)) 1))
	  (visual-selection-mode (if vimpulse-visual-mode-linewise ?l ?r))
	  (motion (unless vimpulse-visual-mode-linewise (vimpulse-get-vs-bounds)))
	  (char (read-char))
	  (start (vimpulse-get-vs-start)))
      (viper-set-destructive-command (list 'vimpulse-replace-chars-in-selection-function
					   (list count visual-selection-mode motion char) ?r nil nil nil))
      (goto-char start) 
      (vimpulse-visual-mode nil)
      (vimpulse-replace-chars-in-selection-function (cons (list count visual-selection-mode motion char) ?r))))))
      
(defun vimpulse-visual-replace-region (&optional arg)
  (interactive "P")
  (goto-char (vimpulse-get-vs-start))
  (vimpulse-set-mark (vimpulse-get-vs-end))
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
(defun vimpulse-visual-replace-region (&optional arg)
  (interactive "P")
  (goto-char (vimpulse-get-vs-start))
  (vimpulse-set-mark (vimpulse-get-vs-end))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Intermediate  commands  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vimpulse-visual-set-current-register ()
  (interactive)
  (setq viper-use-register (read-char)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non destructive commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar vimpulse-last-yank nil)
(defun vimpulse-visual-yank-command ()
  "Saves the visual selection in the kill-ring"
  (interactive)
  (cond 
   (vimpulse-visual-mode-block
    ;;(kill-new "\02VimpulseVisualBlockMode\03")
    (setq vimpulse-last-yank 'rectangle)
    (vimpulse-visual-mode nil)
    ;;(rm-kill-ring-save (region-beginning) (region-end))
    (kill-rectangle (region-beginning) (region-end))
    (goto-char (region-beginning))
    (yank-rectangle)
    (goto-char (region-beginning)))
   (t
    (goto-char (vimpulse-get-vs-start))
    (let ((count (if vimpulse-visual-mode-linewise (count-lines (vimpulse-get-vs-start) (vimpulse-get-vs-end)) 1))
	  (char (if vimpulse-visual-mode-linewise ?l ?r))
	  (motion (unless vimpulse-visual-mode-linewise (vimpulse-get-vs-bounds))))
      (vimpulse-yank-text-objects-function (cons (list count char motion) ?y))
      (vimpulse-visual-mode nil)))))

(defun vimpulse-invert-origin-and-cursor ()
  (interactive)
  (cond
   (vimpulse-visual-mode-block
    (exchange-point-and-mark))
   (t
    (let ((origin vimpulse-visual-overlay-origin))
      (setq vimpulse-visual-overlay-origin (point))
      (goto-char origin)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual Block Mode Support ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This variable holds the point and column of the first line
;; as well as the number of lines in the region.
(defvar vimpulse-visual-insert-coords nil
  "A list with (i-com ul-pos col nlines), where
`i-com' is the insert command (?i, ?a, ?I or ?A)
`ul-pos' is the position of the upper left corner of the region
`col' is the column of insertion
`nlines' is the number of lines in the region")

;; Modified by Alessandro Piras
(defun vimpulse-create-coords (i-com)
  "Updates the list of block insert coordinates with the current rectangle.
`i-com' should be ?c, ?i, ?a, ?I or ?A, the column for the insertion will be
chosen according to this command."
  ;; New visual mode handling: instead of using (region-beginning) and 
  ;; (region-end) we use rb and re, bound to the right bounds:
  ;; overlay bounds in normal and linewise mode, region bounds in block mode.
  (destructuring-bind (rb re) (if vimpulse-visual-mode-block 
				  (list (region-beginning) (region-end))
				(list (vimpulse-get-vs-start)
				      (vimpulse-get-vs-end)))
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


;; FIXME This function or the follwing one is bug ridden
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

;; __Redefinitions of viper functions to handle visual block-mode__
;; This function is not in viper-functions-redefinitions.el 
;; because its code is closely related to visual mode.
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
      (backward-char 1)))               ; <---------- a[ESC] leaves the cursor 
					; where it was before in VIM, without 
					; backward-char it advances 1 character.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual block hack:                              ;;
;; the yank command in visual block now inserts    ;;
;; "\02VimpulseVisualBlockMode\03"                 ;;
;; in the kill ring.                               ;;
;; I chose that string because the presence of     ;;
;; non-printable chars makes it _very_ unlikely    ;;
;; to be generated by a normal copy or cut command ;;
;; when it is found in the kill ring, the block    ;;
;; is pasted instead. (Alessandro Piras)           ;;
;; This still sucks and it's a kludge and an       ;;
;; anti-pattern.                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice viper-Put-back (around vimpulse-visual-block-Put-back (arg) activate)
  (let ((was-in-visual-mode vimpulse-visual-mode))
    (when vimpulse-visual-mode
      (vimpulse-visual-delete-command)
      (current-kill 1)
      (backward-char ))
    (if vimpulse-last-yank
	(yank-rectangle)
      ad-do-it)
    (when was-in-visual-mode
      (current-kill -1))))

(defadvice viper-put-back (around vimpulse-visual-block-put-back (arg) activate)
  (let ((was-in-visual-mode vimpulse-visual-mode))
    (when vimpulse-visual-mode
      (vimpulse-visual-delete-command)
      (current-kill 1)
      (backward-char ))
    (if vimpulse-last-yank
	(progn (forward-char)
	       (yank-rectangle))
      ad-do-it)
    (when was-in-visual-mode
      (current-kill -1))))


;; These 2 functions implement insertion at the beginning/ end of a visual 
;; block or linewise selection
(defun vimpulse-visual-insert (&optional arg)
  "Called when in visual mode to go insert mode at the beginning of the selection."
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

;; CHECKME: this stuff probably doesn't work well with the new visual
;;           mode code
;; We need to detect when a command has deactivated the mark so that
;; Vimpulse is able to exit Visual mode
(defun vimpulse-detect-mark-deactivate ()
  (when (and vimpulse-visual-mode (not mark-active))
    (vimpulse-visual-mode 'toggle)))
(add-hook 'deactivate-mark-hook 'vimpulse-detect-mark-deactivate)

;;;
;;; 
;;;
;; CHECKME: is this still needed?
(defadvice viper-move-marker-locally (around vimpulse-move-marker-locally-wrap activate)
 (unless vimpulse-visual-mode
   ad-do-it))

;; CHECKME: is this still needed?
(defadvice viper-deactivate-mark (around vimpulse-deactivate-mark-wrap activate)
 (unless vimpulse-visual-mode
   ad-do-it))

;;; }}} End visual mode code
(provide 'vimpulse-visual-mode)
