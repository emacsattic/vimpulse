;;;; Type system

;; A type defines a transformation on a pair of buffer positions.
;; Types are used by Visual state (character/line/block selection)
;; and Operator-Pending state (character/line/block motions).
;;
;; The basic transformation is "expansion". For example, the `line'
;; type "expands" a pair of positions to whole lines by moving the
;; first position to the beginning of its line and the last position
;; to the end of its line. That line selection is what the rest of
;; Emacs actually sees and acts on.
;;
;; The opposite of expansion is "contraction", which requires the
;; expansion to be one-to-one. The `inclusive' type, which increases
;; the last position by one, is one-to-one and contractable. The
;; `line' type has no contraction procedure, since it may expand
;; multiple positions to the same lines.
;;
;; Furthermore, a type can be measured (e.g., the number of lines)
;; and described (the string "2 lines"). Such a measurement can be
;; used to create a similar pair of positions elsewhere.
;;
;; Types are defined at the end of this file using the macro
;; `vimpulse-define-type'.

(defun vimpulse-expand-overlay (overlay &optional copy)
  "Expand OVERLAY.
If COPY is non-nil, return a copy of OVERLAY;
otherwise act on OVERLAY directly.

See `vimpulse-contract-overlay' for the inverse operation."
  (vimpulse-transform-overlay 'expand overlay copy))

(defun vimpulse-contract-overlay (overlay &optional copy)
  "Contract OVERLAY.
If COPY is non-nil, return a copy of OVERLAY;
otherwise act on OVERLAY directly.
This is the inverse of `vimpulse-expand-overlay'.

Note that not all transformations are one-to-one and can be
reversed. If no contraction procedure is defined, this function
simply restores the original positions instead."
  (let ((type (vimpulse-type overlay)))
    (if (vimpulse-type-property type :contract)
        (vimpulse-transform-overlay 'contract overlay copy)
      (vimpulse-restore-overlay overlay copy))))

(defun vimpulse-overlay-size (overlay)
  "Return the size of OVERLAY.
The size may be a number or a list of numbers, depending on
OVERLAY's `type' property.

See `vimpulse-describe-overlay' for the textual equivalent."
  (vimpulse-apply-overlay 'vimpulse-size overlay))

(defun vimpulse-describe-overlay (overlay &optional type)
  "Return description string of OVERLAY.
This is the textual equivalent of `vimpulse-overlay-size'."
  (vimpulse-apply-overlay 'vimpulse-describe overlay))

(defun vimpulse-select-overlay
  (size &optional pos buffer &rest properties)
  "Make an overlay of SIZE about point in the current buffer.
SIZE may be a number or a list of numbers, depending on the
`type' property. POS and BUFFER may specify a different position."
  (let* ((overlay (vimpulse-make-overlay (point) (point)))
         (select (apply 'vimpulse-select type size pos buffer properties)))
    (when select
      (apply 'vimpulse-set-overlay select))
    overlay))

(defun vimpulse-transform-overlay (transform overlay &optional copy)
  "Apply TRANSFORM on OVERLAY.
If COPY is non-nil, return a copy of OVERLAY;
otherwise act on OVERLAY directly."
  (let* ((type (vimpulse-type overlay))
         (func (vimpulse-type-property type transform))
         (transform (vimpulse-apply-overlay func overlay)))
    (when copy
      (setq overlay (vimpulse-copy-overlay overlay)))
    (when transform
      (vimpulse-save-overlay overlay)
      (apply 'vimpulse-set-overlay transform))
    overlay))

(defun vimpulse-save-overlay (overlay &optional copy)
  "Back up current positions of OVERLAY.
If OVERLAY is changed, it can be restored to its old self
with `vimpulse-restore-overlay'."
  (when copy
    (setq overlay (vimpulse-copy-overlay overlay)))
  (vimpulse-overlay-put 'orig nil) ; reset when restoring
  (vimpulse-overlay-put 'orig (vimpulse-overlay-to-list overlay t))
  overlay)

(defun vimpulse-restore-overlay (overlay &optional copy)
  "Restore original positions of OVERLAY.
See also `vimpulse-save-overlay'."
  (let ((orig (vimpulse-overlay-get overlay 'orig)))
    (when copy
      (setq overlay (vimpulse-copy-overlay overlay)))
    (when orig
      (unwind-protect
          (apply 'vimpulse-set-overlay orig)
        (unless copy
          (set-marker (pop orig) nil)
          (set-marker (pop orig) nil))))
    overlay))

(defun vimpulse-apply-overlay (function overlay)
  "Call FUNCTION with the properties of OVERLAY.
FUNCTION is assumed to have arguments
\(beg end &optional buffer &rest properties)."
  (when (functionp function)
    (apply function (vimpulse-overlay-to-list overlay))))

(defun vimpulse-overlay-to-list (overlay &optional markers)
  "Return a list (BEG END BUFFER PROPERTIES ...).
If MARKERS is non-nil, return BEG and END as markers."
  (let ((beg (vimpulse-overlay-start overlay))
        (end (vimpulse-overlay-end overlay))
        (buffer (vimpulse-overlay-buffer overlay))
        (properties (vimpulse-overlay-properties overlay)))
    (when markers
      (setq beg (set-marker (make-marker) beg buffer)
            end (set-marker (make-marker) end buffer)))
    (append (list beg end buffer) properties)))

(defun vimpulse-expand (beg end type &optional buffer &rest properties)
  "Expand BEG and END as TYPE in BUFFER with PROPERTIES.
Returns a list (BEG END ...), where the tail is a property list."
  (apply 'vimpulse-transform beg end type 'expand buffer properties))

(defun vimpulse-contract (beg end type &optional buffer &rest properties)
  "Contract BEG and END as TYPE in BUFFER with PROPERTIES.
Returns a list (BEG END ...), where the tail is a property list."
  (apply 'vimpulse-transform beg end type 'contract buffer properties))

(defun vimpulse-size (beg end type &optional buffer &rest properties)
  "Return size from BEG to END in BUFFER with PROPERTIES.
The size may be a number or a list of numbers, depending on
the `type' property."
  (let* ((type (or type (vimpulse-type properties)))
         (properties (plist-put properties 'type type))
         (size (vimpulse-type-property type :size)))
    (when size
      (apply size beg end buffer properties))))

(defun vimpulse-describe (beg end type &optional buffer &rest properties)
  "Return description of BEG and END in BUFFER with PROPERTIES.
If no description is available, return the empty string."
  (let* ((type (or type (vimpulse-type properties)))
         (properties (plist-put properties 'type type))
         (describe (vimpulse-type-property type :describe)))
    (if describe
        (apply describe beg end buffer properties)
      "")))

(defun vimpulse-select (type size &optional pos buffer &rest properties)
  "Return TYPE selection of SIZE about POS in BUFFER with PROPERTIES.
SIZE may be a number or a list of numbers, depending on
the `type' property."
  (let* ((type (or type (vimpulse-type properties)))
         (properties (plist-put properties 'type type))
         (select (vimpulse-type-property type :select))
         (pos (or pos (point))))
    (if select
        (apply select
               (append (if (listp size) size (list size))
                       (list pos buffer)
                       properties))
      (append (list pos pos) properties))))

(defun vimpulse-transform
  (beg end type transform &optional buffer &rest properties)
  "Apply TRANSFORM on BEG and END in BUFFER with PROPERTIES.
Returns a list (BEG END ...), where the tail is a property list.
If TRANSFORM is undefined, return positions unchanged."
  (let* ((type (or type (vimpulse-type properties)))
         (properties (plist-put properties 'type type))
         (buffer (or buffer (current-buffer)))
         (transform (vimpulse-type-property type transform)))
    (if transform
        (apply transform beg end buffer properties)
      (append (list beg end buffer) properties))))

(defun vimpulse-type (object &optional default)
  "Return the type of OBJECT, or DEFAULT if none."
  (or (cond
       ((vimpulse-overlay-p)
        (vimpulse-overlay-get overlay 'type))
       ((listp object)
        ;; `vimpulse-expand' et al. return a list
        ;; with the properties in the tail
        (while (or (number-or-marker-p (car object))
                   (bufferp (car object)))
          (setq object (cdr object)))
        (plist-get object 'type))
       ((symbolp object)
        (get object 'type)))
      default))

(defun vimpulse-type-property (type prop)
  "Return property PROP for TYPE.
For example, (vimpulse-type-property 'line :expand)
returns the expansion function for the `line' type."
  (vimpulse-get-property vimpulse-types-alist state prop))

(defvar vimpulse-types-alist nil
  "Specifications made by `vimpulse-define-type'.
Entries have the form (TYPE . PLIST), where PLIST is a property
list specifying functions for handling the type: expanding it,
describing it, etc.")

;; TODO: when a keystrokes-based repeat system is in place,
;; :select will be superfluous and :size can be merged into
;; :describe. (Maybe :describe can be purged as well?)
(defmacro vimpulse-define-type (type doc &rest body)
  "Define type TYPE.
DOC is a general description and shows up in all docstrings.
It is followed by a list of keywords and functions:

:expand FUNC    Expansion function. This function should take two
                positions in the current buffer, BEG and END, and
                return a pair of expanded buffer positions.
:contract FUNC  Contraction function, optional. This is the opposite
                of :expand, provided the expansion is reversible.
:size FUNC      Size function. This takes a pair of expanded positions
                and returns a number or a list of numbers measuring
                their \"size\" according to the type.
:describe FUNC  Description function. This takes the input of :size
                and returns a human-readable string.
:select FUNC    Selection function. This takes the input of :size and
                returns a pair of unexpanded positions about point.

For example, the `inclusive' character type, which always includes
the character under point, may be defined as follows:

    (vimpulse-define-type inclusive
      \"Include the character under point.\"
      :expand (lambda (beg end)
                (list beg (1+ end)))
      :contract (lambda (beg end)
                  (list beg (1- end)))
      :size (lambda (beg end)
              (- end beg))
      :describe (lambda (width)
                  (format \"%s character(s)\" width))
      :select (lambda (width)
                (forward-char (1- width))))

Further keywords and functions may be specified. These are assumed to
be transformations on buffer positions, like :expand and :contract."
  (declare (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp sexp function-form]]))
           (indent defun))
  (let* ((dimensions (or (plist-get body :dimensions)
                         (car (cdr-safe (plist-get :describe)))
                         (car (cdr-safe (plist-get :select)))))
         (plist (plist-put nil :dimensions dimensions))
         keyword func args string sym name)
    (while (keywordp (car body))
      (setq keyword (pop body)
            func (pop body)
            sym (intern (replace-regexp-in-string
                         "^:" "" (symbol-name keyword)))
            name (intern (format "vimpulse-%s-%s" type sym))
            args (car (cdr-safe func))
            string (car (cdr (cdr-safe func)))
            string (if (stringp string) string "")
            plist (plist-put keyword name))
      (add-to-list
       'defun-forms
       (cond
        ((eq keyword :size)
         `(defun ,name (beg end &optional buffer &rest properties)
            ,(format "Return size of %s from BEG to END \
in BUFFER with PROPERTIES.\n%s\n\n%s" type string doc)
            (let ((buffer (or buffer (current-buffer)))
                  result)
              (save-excursion
                (switch-to-buffer buffer)
                (setq beg (prog1 (min beg end)
                            (setq end (max beg end))))
                (unless (plist-get properties 'expanded)
                  (setq result (vimpulse-expand
                                beg end ',type buffer properties)
                        beg (pop result)
                        end (pop result))
                  (when (bufferp (car result))
                    (setq buffer (pop result)))
                  (setq properties result))
                ,(if (> (length args) 1)
                     '(apply ',func beg end properties)
                   '(funcall ',func beg end))))))
        ((eq keyword :describe)
         `(defun ,name (beg end &optional buffer &rest properties)
            ,(format "Return description string of %s from BEG to END \
in BUFFER with PROPERTIES.\n%s\n\n%s" type string doc)
            (let ((size (apply ',size beg end buffer properties)))
              (save-excursion
                (if (listp size)
                    (apply ',func size)
                  (funcall ',func size))))))
        ((eq keyword :select)
         `(defun ,name (,@dimensions &optional pos buffer &rest properties)
            ,(format "Make a %s selection about POS \
in BUFFER with PROPERTIES.\n%s\n\n%s" type string doc)
            (let ((buffer (or buffer (current-buffer)))
                  (pos (or pos (point)))
                  beg end result)
              (save-excursion
                (switch-to-buffer buffer)
                (goto-char pos)
                (setq properties (plist-put properties 'type ',type)
                      result
                      ,(if (> (length args) 1)
                           `(apply ',func ,@dimensions properties)
                         `(funcall ',func ,@dimensions))
                      beg (pop result)
                      end (pop result))
                (vimpulse-sort beg end)
                (when (bufferp (car result))
                  (setq buffer (pop result)))
                (while result
                  (setq properties (plist-put properties
                                              (pop result) (pop result))))
                (if (plist-get properties 'expanded)
                    (vimpulse-expand beg end buffer properties)
                  (append (list beg end buffer) properties))))))
        (t
         `(defun ,name (beg end &optional buffer &rest properties)
            ,(format "Perform %s transformation on %s from BEG to END \
in BUFFER with PROPERTIES.\n%s\n\n%s" sym type string doc)
            (let ((buffer (or buffer (current-buffer)))
                  result)
              (save-excursion
                (switch-to-buffer buffer)
                (when (and beg end)
                  (vimpulse-sort beg end)
                  (setq properties
                        (plist-put properties 'type ',type
                                   ,@(when (memq keyword '(:expand :contract))
                                       `(('expanded ,(eq keyword :expand)))))
                        result
                        ,(if (> (length args) 2)
                             `(apply ',func beg end properties)
                           `(funcall ',func beg end))
                        beg (pop result)
                        end (pop result))
                  (vimpulse-sort beg end))
                (when (bufferp (car result))
                  (setq buffer (pop result)))
                (while result
                  (setq properties (plist-put properties
                                              (pop result) (pop result))))
                (append (list beg end buffer) properties))))))))
    `(progn
       ;; index type functions
       (vimpulse-put-property vimpulse-types-alist ',type ,@plist)
       ;; define them
       ,@defun-forms
       ',type)))

;;; Type definitions

(vimpulse-define-type exclusive
  "Return the positions unchanged, with the following exceptions:

* If the end position is at the beginning of a line and the
  beginning position is at or before the first non-blank
  on the line, return `line' (expanded).

* Otherwise, move the end position to the end of the previous
  line and return `inclusive' (expanded)."
  :expand (lambda (beg end)
            (cond
             ((and (/= beg end)
                   (progn
                     (goto-char end)
                     (bolp)))
              (backward-char)
              (setq end (max beg (point)))
              (cond
               ((progn
                  (goto-char beg)
                  (looking-back "^[ \f\t\v]*"))
                (vimpulse-expand beg end 'line))
               (t
                (vimpulse-expand beg end 'inclusive))))
             (t
              (list beg end))))
  :size (lambda (beg end)
          (- end beg))
  :describe (lambda (width)
              (format "%s character%s" width
                      (if (= width 1) "" "s")))
  :select (lambda (width)
            (forward-char width)))

(vimpulse-define-type inclusive
  "Include the character under point."
  :expand (lambda (beg end)
            (list beg (min (1+ end) (point-max))))
  :contract (lambda (beg end)
              (list beg (max beg (1- end))))
  :size (lambda (beg end)
          (- end beg))
  :describe (lambda (width)
              (format "%s character%s" width
                      (if (= width 1) "" "s")))
  :select (lambda (width)
            (forward-char (1- width))))

(vimpulse-define-type line
  "Include whole lines."
  :expand (lambda (beg end)
            (list
             (progn
               (goto-char beg)
               (line-beginning-position))
             (progn
               (goto-char end)
               (line-beginning-position 2))))
  :size (lambda (beg end)
          (count-lines beg end))
  :describe (lambda (height)
              (format "%s line%s" height
                      (if (= height 1) "" "s")))
  :select (lambda (height)
            (forward-line (1- height))))

(vimpulse-define-type block
  "Like `inclusive', but for rectangles:
the last column is included."
  :expand (lambda (beg end &rest properties)
            (let* ((beg-col (progn
                              (goto-char beg)
                              (current-column)))
                   (end-col (progn
                              (goto-char end)
                              (current-column)))
                   (corner (plist-get properties 'corner)))
              (cond
               ((= beg-col end-col)
                (goto-char end)
                (cond
                 ((eolp)
                  (goto-char beg)
                  (if (eolp)
                      (list beg end)
                    (list (1+ beg) end)))
                 ((memq corner ('lower-right 'upper-right 'right))
                  (list (1+ beg) end))
                 (t
                  (list beg (1+ end)))))
               ((< beg-col end-col)
                (goto-char end)
                (if (eolp)
                    (list beg end)
                  (list beg (1+ end))))
               (t
                (goto-char beg)
                (if (eolp)
                    (list beg end)
                  (list (1+ beg) end))))))
  :contract (lambda (beg end)
              (let* ((beg-col (progn
                                (goto-char beg)
                                (current-column)))
                     (end-col (progn
                                (goto-char end)
                                (current-column))))
                (if (> beg-col end-col)
                    (list (1- beg) end)
                  (list beg (max beg (1- end))))))
  :size (lambda (beg end)
          (let ((height (count-lines
                         beg
                         (progn
                           (goto-char end)
                           (if (and (bolp) (not (eobp)))
                               (1+ end)
                             end))))
                (width (abs (- (progn
                                 (goto-char beg)
                                 (current-column))
                               (progn
                                 (goto-char end)
                                 (current-column))))))
            (list height width)))
  :describe (lambda (height width)
              (format "%s row%s and %s column%s"
                      height
                      (if (= height 1) "" "s")
                      width
                      (if (= width 1) "" "s")))
  :select (lambda (height width)
            (let (beg end)
              (setq beg (point)
                    height (1- height)
                    width (+ (current-column) (1- width)))
              (forward-line height)
              (vimpulse-move-to-column width)
              (setq height (count-lines beg (point)))
              (while (and (not (eq (current-column) width))
                          (> height 1))
                (forward-line -1)
                (setq height (1- height))
                (move-to-column width))
              (setq end (point))
              (list beg end)))
  :rotate (lambda (beg end &rest properties)
            (let* ((beg-col (progn
                              (goto-char beg)
                              (current-column)))
                   (end-col (progn
                              (goto-char end)
                              (current-column)))
                   (left  (min beg-col end-col))
                   (right (max beg-col end-col))
                   (corner (or (plist-get 'corner properties))
                           'upper-left))
              (goto-char beg)
              (if (memq corner '(upper-right lower-left))
                  (move-to-column right)
                (move-to-column left))
              (setq beg (point))
              (goto-char end)
              (if (memq corner '(upper-right lower-left))
                  (move-to-column left)
                (move-to-column right))
              (setq end (point))
              (setq properties (plist-put properties
                                          'corner corner))
              (append (list beg end) properties))))

;; this belongs in vimpulse-visual-mode.el
(defun vimpulse-visual-block-corner (&optional point mark corner)
  "Return the block corner corresponding to POINT.
Depending on POINT and MARK, the return value is `upper-left',
`upper-right', `lower-left' or `lower-right'.

One-column or one-row blocks are ambiguous. In such cases,
the horizontal or vertical component of CORNER is used.
CORNER defaults to `upper-left'."
  (let* ((point (or point (point)))
         (mark (or mark (mark t)))
         (corner (symbol-value corner))
         (point-col (save-excursion
                      (goto-char point)
                      (current-column)))
         (mark-col (save-excursion
                     (goto-char mark)
                     (current-column)))
         (horizontal (or (and (string-match "left\\|right" corner)
                              (match-string 0 corner))
                         "left"))
         (vertical (or (and (string-match "upper\\|lower" corner)
                            (match-string 0 corner))
                       "upper")))
    (cond
     ((< point-col mark-col)
      (setq horizontal "left"))
     ((> point-col mark-col)
      (setq horizonal "right")))
    (cond
     ((< point mark)
      (setq vertical "upper"))
     ((> point mark)
      (setq vertical "lower")))
    (intern (format "%s-%s" vertical horizontal))))

(provide 'vimpulse-types)
