;;;;
;;;; This file contains an alternate paren matching function used when
;;;; viper is in vi mode, so that the paren under the cursor is matched, 
;;;; instead of the paren before the cursor. This visually makes checking 
;;;; parens at the end of the line possible.
;;;;

;;;
;;; paren matching LOADED BY DEFAULT
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
  ;;; TODO: check if using this paren matching function in replace mode is a problem 
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

(provide 'vimpulse-paren-matching)
