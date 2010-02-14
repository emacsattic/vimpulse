;;;;
;;;; When highlighting matching parentheses, Emacs matches the closing
;;;; parenthesis before the cursor instead of under it (like in Vim).
;;;; This file provides an alternate parenthesis matching function
;;;; used when Viper is in vi (command) mode, so that the parenthesis
;;;; under the cursor is matched. This makes it possible to visually
;;;; inspect a closing parenthesis at the end of the line.
;;;;
;;;; In Insert mode, Emacs' scheme is deemed best and kept as is.
;;;;
;;;; Custom paren-matching LOADED BY DEFAULT.
;;;; Set `vimpulse-enhanced-paren-matching' to nil in your .emacs
;;;; before requiring Vimpulse to avoid loading it.
;;;;

;;; Begin Paren Matching Code {{{

;; Do we really need this option?
(defcustom vimpulse-enhanced-paren-matching t
  "Enhanced matching of parentheses, on by default."
  :group 'vimpulse
  :type  'boolean)

;; Safely enable show-paren-mode (normal highlighting)
(and (fboundp 'show-paren-mode)
     (show-paren-mode 1))

;; Overlays are buffer-local
(viper-deflocalvar
 vimpulse-paren-overlay-open nil
 "Overlay used to highlight the opening paren.")

(viper-deflocalvar
 vimpulse-paren-overlay-close nil
 "Overlay used to highlight the closing paren.")

(defun vimpulse-paren-open-p (&optional pos)
  "Return t if the character at POS or point is an opening paren."
  (setq pos (or pos (point)))
  (let ((class (syntax-class (syntax-after pos))))
    (= 4 class)))

(defun vimpulse-paren-close-p (&optional pos)
  "Returns t if the character at POS or point is an closing paren."
  (setq pos (or pos (point)))
  (let ((class (syntax-class (syntax-after pos))))
    (= 5 class)))

(defun vimpulse-paren-match (&optional pos)
  "Return the position of possible matching paren at POS or point.
If not a paren, return `not-a-paren'. If not found, return nil."
  (setq pos (or pos (point)))
  (condition-case nil
      (cond
       ((vimpulse-paren-open-p pos)
        (1- (scan-sexps pos 1)))
       ((vimpulse-paren-close-p pos)
        (scan-sexps (1+ pos) -1))
       (t
        'not-a-paren))
    (error nil)))

(defun vimpulse-paren-match-p (pos1 pos2)
  "Return t if POS1 and POS2 are matching characters.
Checks the characters at position POS1 and POS2 and returns t
if they are matching characters (in a paren match meaning),
nil otherwise."
  (let ((class1 (car (syntax-after pos1)))
        (match1 (cdr (syntax-after pos1)))
        (class2 (car (syntax-after pos2)))
        (match2 (cdr (syntax-after pos2))))
    (or (eq match1 (char-after pos2))
        (eq match2 (char-after pos1))
        (eq match1 match2))))

(defun vimpulse-paren-highlight (face &optional pos)
  "Highlight the paren at POS with FACE."
  (setq pos (or pos (point)))
  (let ((ovl (if (vimpulse-paren-open-p pos)
                 vimpulse-paren-overlay-open
               vimpulse-paren-overlay-close)))
    (overlay-put ovl 'face face)
    (move-overlay ovl pos (1+ pos))))

;; FIXME: this description sucks
(defun vimpulse-paren-highlight-pair (&optional pos)
  "Highlight paren pair.
Highlights the paren at POS and eventual matching
or mismatched paren."
  (setq pos (or pos (point)))
  (let ((match (vimpulse-paren-match pos)))
    (cond
     ((not match)
      (vimpulse-paren-highlight 'show-paren-mismatch pos))
     ((eq match 'not-a-paren)
      (delete-overlay vimpulse-paren-overlay-open)
      (delete-overlay vimpulse-paren-overlay-close))
     ((/= pos (vimpulse-paren-match match))
      (vimpulse-paren-highlight 'show-paren-mismatch pos))
     ((vimpulse-paren-match-p pos match)
      (vimpulse-paren-highlight 'show-paren-match pos)
      (vimpulse-paren-highlight 'show-paren-match match))
     (t
      (vimpulse-paren-highlight 'show-paren-mismatch pos)
      (vimpulse-paren-highlight 'show-paren-mismatch match)))))

;;; We advice `show-paren-function' and use it for Insert mode.
;;; TODO: check if using this in Replace mode is a problem.
(defadvice show-paren-function (around vimpulse-paren activate)
  "Use custom highlighting if `vimpulse-enhanced-paren-matching' is t."
  ;; Define overlays if they don't exist
  (cond
   (vimpulse-enhanced-paren-matching
    (unless vimpulse-paren-overlay-open
      (setq vimpulse-paren-overlay-open
            (make-overlay (point) (point))
            vimpulse-paren-overlay-close
            (make-overlay (point) (point)))
      (delete-overlay vimpulse-paren-overlay-open)
      (delete-overlay vimpulse-paren-overlay-close))
    (cond
     ;; Viper not in Insert mode
     ((and (not (eq viper-current-state 'insert-state))
           show-paren-mode viper-mode)
      ;; Safely delete the overlays used by `show-paren-function'
      ;; and call our custom function instead
      (and (viper-overlay-live-p show-paren-overlay)
           (delete-overlay show-paren-overlay))
      (and (viper-overlay-live-p show-paren-overlay-1)
           (delete-overlay show-paren-overlay-1))
      (vimpulse-paren-highlight-pair))
     ;; Viper in Insert mode
     (t
      ;; Delete the overlays used by our custom function
      (delete-overlay vimpulse-paren-overlay-open)
      (delete-overlay vimpulse-paren-overlay-close)
      ad-do-it)))
   (t
    ad-do-it)))

;;; }}} End Paren Matching code

(provide 'vimpulse-paren-matching)

