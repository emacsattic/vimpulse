;;;; Text objects support

;; The following code implements support for text objects and commands
;; like diw, daw, ciw, caw. Currently, the most common objects are
;; supported:
;;
;;   - paren-blocks: b B { [ ( < > ) ] }
;;   - sentences: s
;;   - paragraphs: p
;;   - quoted expressions: " and '
;;   - words: w and W
;;
;; Vimpulse's text objects are fairly close to Vim's, and are based on
;; Viper's movement commands. More objects are easily added.

;;; Begin Text Objects code {{{

(defun vimpulse-mark-object (range-func count &rest range-args)
  "Select range determined by RANGE-FUNC.
COUNT and RANGE-ARGS are the arguments to RANGE-FUNC.
RANGE-FUNC must evaluate to a range (BEG END).

In Visual mode, the current selection is expanded to include the range.
If RANGE-FUNC fails to produce a range not already selected, it
may be called again at a different position in the buffer."
  (let (vimpulse-this-motion
        vimpulse-last-motion
        range dir)
    (cond
     ((region-active-p)
      (setq dir (if (< (point) (mark t)) -1 1))
      (when (eq 'line vimpulse-visual-mode)
        (vimpulse-visual-activate 'normal))
      (when (and vimpulse-visual-mode
                 (not vimpulse-visual-region-expanded))
        (vimpulse-visual-expand-region))
      (setq range (apply range-func (* dir count) range-args))
      (unless (vimpulse-mark-range range t)
        ;; Are we stuck (unchanged region)?
        ;; Move forward and try again.
        (viper-forward-char-carefully dir)
        (setq range (apply range-func (* dir count) range-args))
        (vimpulse-mark-range range t)))
     (t
      (setq range (apply range-func count range-args))
      (vimpulse-mark-range range)))))

(defun vimpulse-mark-range (range &optional widen)
  "Mark RANGE, which has the form (TYPE BEG END).
If WIDEN is non-nil, expand existing region."
  (let* ((type  (vimpulse-motion-type range))
         (range (vimpulse-motion-range range))
         (beg (apply 'min range))
         (end (apply 'max range)))
    (cond
     ((eq 'exclusive type)
      (if vimpulse-visual-mode
          (vimpulse-visual-select beg end widen)
        (vimpulse-set-region beg end widen)))
     (t
      (when vimpulse-visual-mode
        (unless (memq type '(line block))
          (setq type 'normal))
        (unless (eq type vimpulse-visual-mode)
          (vimpulse-visual-activate type)))
      (vimpulse-visual-select beg end widen)))))

(defun vimpulse-object-range
  (count backward-func forward-func &optional type)
  "Return a text object range (TYPE BEG END).
BACKWARD-FUNC moves point to the object's beginning,
FORWARD-FUNC moves to its end. Schematically,

\(vimpulse-object-range <num> <beg-of-object> <end-of-object>)

COUNT is the number of objects. If positive, go forwards and
then backwards; if negative, go backwards and then forwards.

The type of the object (`exclusive', `inclusive' or `line')
may be specified with TYPE. Otherwise, the type is inferred
from the motion types of BACKWARD-FUNC and FORWARD-FUNC."
  (let ((types '(exclusive inclusive line block))
        beg end forward-range backward-range
        vimpulse-this-motion-type)
    (save-excursion
      (setq count (or (if (eq 0 count) 1 count) 1))
      (if (> 0 count)
          (setq backward-range
                (vimpulse-make-motion-range
                 (abs count) backward-func type t)
                forward-range
                (vimpulse-make-motion-range
                 (abs count) forward-func type t))
        (setq forward-range
              (vimpulse-make-motion-range
               (abs count) forward-func type t)
              backward-range
              (vimpulse-make-motion-range
               (abs count) backward-func type t)))
      (setq beg (apply 'min (vimpulse-motion-range backward-range))
            end (apply 'max (vimpulse-motion-range forward-range)))
      (unless type
        (setq type 'exclusive)
        (dolist (elt types)
          (when (or (eq elt (vimpulse-motion-type backward-range))
                    (eq elt (vimpulse-motion-type forward-range)))
            (setq type elt))))
      (list type beg end))))

(defun vimpulse-an-object-range
  (count backward-func forward-func &optional include-newlines regexp)
  "Return a text object range (BEG END) with whitespace.
Unless INCLUDE-NEWLINES is t, whitespace inclusion is restricted
to the line(s) the object is on. REGEXP is a regular expression
for matching whitespace; the default is \"[ \\f\\t\\n\\r\\v]+\".
See `vimpulse-object-range' for more details."
  (let (range beg end line-beg line-end mark-active-p)
    (save-excursion
      (setq count (or (if (eq 0 count) 1 count) 1))
      (setq regexp (or regexp "[ \f\t\n\r\v]+"))
      (setq range (vimpulse-motion-range
                   (vimpulse-object-range
                    count backward-func forward-func)))
      ;; Let `end' be the boundary furthest from point,
      ;; based on the direction we are going
      (if (> 0 count)
          (setq beg (cadr range)
                end (car range))
        (setq beg (car range)
              end (cadr range)))
      ;; If INCLUDE-NEWLINES is nil, never move past
      ;; the line boundaries of the text object
      (unless include-newlines
        (setq line-beg (line-beginning-position)
              line-end (line-end-position))
        (when (< (max (* count line-beg) (* count line-end))
                 (* count beg))
          (setq count (- count))
          (setq range (vimpulse-motion-range
                       (vimpulse-object-range
                        count backward-func forward-func)))
          (if (> 0 count)
              (setq beg (cadr range)
                    end (car range))
            (setq beg (car range)
                  end (cadr range))))
        (setq line-beg (save-excursion
                         (goto-char (min beg end))
                         (line-beginning-position))
              line-end (save-excursion
                         (goto-char (max beg end))
                         (line-end-position))))
      ;; Generally only include whitespace at one side (but see below).
      ;; If we are before the object, include leading whitespace;
      ;; if we are inside the object, include trailing whitespace.
      ;; If trailing whitespace inclusion fails, include leading.
      (setq count (if (> 0 count) -1 1))
      (when (or (< (* count (point)) (* count beg))
                (eq end (setq end (save-excursion
                                    (goto-char end)
                                    (vimpulse-skip-regexp
                                     regexp count line-beg line-end)))))
        (setq beg (save-excursion
                    (goto-char beg)
                    (if (and (not include-newlines)
                             (looking-back "^[ \t]*"))
                        beg
                      (vimpulse-skip-regexp
                       regexp (- count) line-beg line-end))))
        ;; Before/after adjustment for whole lines: if the object is
        ;; followed by a blank line, include that as trailing
        ;; whitespace and subtract a line from the leading whitespace
        (when include-newlines
          (goto-char end)
          (forward-line count)
          (when (looking-at "[ \t]*$")
            (setq end (line-beginning-position))
            (goto-char beg)
            (when (looking-at "[ \t]*$")
              (forward-line count)
              (setq beg (line-beginning-position))))))
      ;; Return the range
      (list (min beg end) (max beg end)))))

(defun vimpulse-inner-object-range
  (count backward-func forward-func)
  "Return a text object range (BEG END) including point.
If point is outside the object, it is included in the range.
To include whitespace, use `vimpulse-an-object-range'.
See `vimpulse-object-range' for more details."
  (let (range beg end line-beg line-end)
    (setq count (or (if (eq 0 count) 1 count) 1))
    (setq range (vimpulse-motion-range
                 (vimpulse-object-range
                  count backward-func forward-func)))
    (setq beg (car range)
          end (cadr range))
    (setq line-beg (line-beginning-position)
          line-end (line-end-position))
    (when (< (max (* count line-beg) (* count line-end))
             (min (* count beg) (* count end)))
      (setq count (- count))
      (setq range (vimpulse-motion-range
                   (vimpulse-object-range
                    count backward-func forward-func))
            beg (car range)
            end (cadr range)))
    ;; Return the range, including point
    (list (min beg (point)) (max end (point)))))

(defun vimpulse-paren-range (count &optional open close include-parentheses)
  "Return a parenthetical expression range (BEG END).
The type of parentheses may be specified with OPEN and CLOSE,
which must be characters. INCLUDE-PARENTHESES specifies
whether to include the parentheses in the range."
  (let ((beg (point)) (end (point))
        line-beg line-end)
    (setq count (if (eq 0 count) 1 (abs count)))
    (save-excursion
      (setq open  (if (characterp open)
                      (regexp-quote (string open)) "")
            close (if (characterp close)
                      (regexp-quote (string close)) ""))
      (when (and (not (string= "" open))
                 (looking-at open))
        (forward-char))
      ;; Find opening and closing paren with
      ;; Emacs' S-exp facilities
      (while (progn
               (vimpulse-backward-up-list 1)
               (not (when (looking-at open)
                      (when (save-excursion
                              (forward-sexp)
                              (when (looking-back close)
                                (setq end (point))))
                        (if (<= 0 count)
                            (setq beg (point))
                          (setq count (1- count)) nil))))))
      (if include-parentheses
          (list beg end)
        (setq beg (prog1 (min (1+ beg) end)
                    (setq end (max (1- end) beg))))
        ;; Multi-line inner range: select whole lines
        (if (>= 1 (count-lines beg end))
            (list beg end)
          (goto-char beg)
          (when (looking-at "[ \f\t\n\r\v]*$")
            (forward-line)
            ;; Include indentation?
            (if (and viper-auto-indent
                     (not (eq 'vimpulse-delete
                              vimpulse-this-operator)))
                (back-to-indentation)
              (beginning-of-line))
            (setq beg (point)))
          (goto-char end)
          (when (and (looking-back "^[ \f\t\n\r\v]*")
                     (not (eq 'vimpulse-delete
                              vimpulse-this-operator)))
            (setq end (line-end-position 0))
            (goto-char end))
          (list (min beg end) (max beg end)))))))

(defun vimpulse-quote-range (count &optional quote include-quotes)
  "Return a quoted expression range (BEG END).
QUOTE is a quote character (default ?\\\"). INCLUDE-QUOTES
specifies whether to include the quote marks in the range."
  (let ((beg (point)) (end (point))
        regexp)
    (save-excursion
      (setq count (if (eq 0 count) 1 (abs count)))
      (setq quote (or quote ?\"))
      (setq quote (if (characterp quote)
                      (regexp-quote (string quote)) "")
            regexp (concat "\\([^\\\\]\\|^\\)" quote))
      (when (and (not (string= "" quote))
                 (looking-at quote))
        (forward-char))
      ;; Search forward for a closing quote
      (while (and (< 0 count)
                  (re-search-forward regexp nil t))
        (setq count (1- count))
        (setq end (point))
        ;; Find the matching opening quote
        (condition-case nil
            (progn
              (setq beg (scan-sexps end -1))
              ;; Emacs' S-exp logic doesn't work in text mode
              (save-excursion
                (goto-char beg)
                (unless (looking-at quote)
                  (re-search-backward regexp)
                  (unless (looking-at quote)
                    (forward-char))
                  (setq beg (point)))))
          ;; Finding the opening quote failed. Maybe we're already at
          ;; the opening quote and should look for the closing instead?
          (error (condition-case nil
                     (progn
                       (viper-backward-char-carefully)
                       (setq beg (point))
                       (setq end (scan-sexps beg 1))
                       (unless (looking-back quote)
                         (re-search-forward regexp)
                         (unless (looking-back quote)
                           (backward-char))
                         (setq end (point))))
                   (error (setq end beg))))))
      (if include-quotes
          (list beg end)
        (list (min (1+ beg) end) (max (1- end) beg))))))

(defun vimpulse-line (arg)
  "Select ARG lines."
  (interactive "p")
  (setq arg (1- arg))
  (vimpulse-mark-object
   'vimpulse-line-range
   (point)
   (save-excursion
     (when (< 0 arg)
       (viper-next-line-carefully arg))
     (point))))
(put 'vimpulse-line 'motion-type 'line)

(defun vimpulse-a-word (arg)
  "Select a word."
  (interactive "p")
  (vimpulse-mark-object
   'vimpulse-an-object-range arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-end-of-word (cons arg ?r))))))

(defun vimpulse-inner-word (arg)
  "Select inner word."
  (interactive "p")
  (vimpulse-mark-object
   'vimpulse-inner-object-range arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-end-of-word (cons arg ?r))))))

(defun vimpulse-a-Word (arg)
  "Select a Word."
  (interactive "p")
  (vimpulse-mark-object
   'vimpulse-an-object-range arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-Word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-end-of-Word (cons arg ?r))))))

(defun vimpulse-inner-Word (arg)
  "Select inner Word."
  (interactive "p")
  (vimpulse-mark-object
   'vimpulse-inner-object-range arg
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-backward-Word (cons arg ?r))))
   (lambda (arg)
     (vimpulse-limit (line-beginning-position) (line-end-position)
       (viper-end-of-Word (cons arg ?r))))))

(defun vimpulse-a-sentence (arg)
  "Select a sentence."
  (interactive "p")
  (vimpulse-mark-object
   'vimpulse-an-object-range arg
   (lambda (arg)
     (viper-backward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (viper-forward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1))))

(defun vimpulse-inner-sentence (arg)
  "Select inner sentence."
  (interactive "p")
  (vimpulse-mark-object
   'vimpulse-inner-object-range arg
   (lambda (arg)
     (viper-backward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (viper-forward-sentence arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1))))

(defun vimpulse-a-paragraph (arg)
  "Select a paragraph."
  (interactive "p")
  (vimpulse-mark-object
   'vimpulse-an-object-range arg
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1)
     (viper-backward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1)
     (viper-forward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1)) t))

(defun vimpulse-inner-paragraph (arg)
  "Select inner paragraph."
  (interactive "p")
  (vimpulse-mark-object
   'vimpulse-inner-object-range arg
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1)
     (viper-backward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1))
   (lambda (arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" 1)
     (viper-forward-paragraph arg)
     (vimpulse-skip-regexp "[ \f\t\n\r\v]+" -1))))

(defun vimpulse-a-paren (arg)
  "Select a parenthesis."
  (interactive "p")
  (vimpulse-mark-object 'vimpulse-paren-range arg ?\( nil t))

(defun vimpulse-inner-paren (arg)
  "Select inner parenthesis."
  (interactive "p")
  (vimpulse-mark-object 'vimpulse-paren-range arg ?\())

(defun vimpulse-a-bracket (arg)
  "Select a bracket parenthesis."
  (interactive "p")
  (vimpulse-mark-object 'vimpulse-paren-range arg ?\[ nil t))

(defun vimpulse-inner-bracket (arg)
  "Select inner bracket parenthesis."
  (interactive "p")
  (vimpulse-mark-object 'vimpulse-paren-range arg ?\[))

(defun vimpulse-a-curly (arg)
  "Select a curly parenthesis."
  (interactive "p")
  (vimpulse-mark-object 'vimpulse-paren-range arg ?{ nil t))

(defun vimpulse-inner-curly (arg)
  "Select inner curly parenthesis."
  (interactive "p")
  (vimpulse-mark-object 'vimpulse-paren-range arg ?{))

(defun vimpulse-an-angle (arg)
  "Select an angle bracket."
  (interactive "p")
  (vimpulse-mark-object 'vimpulse-paren-range arg ?< nil t))

(defun vimpulse-inner-angle (arg)
  "Select inner angle bracket."
  (interactive "p")
  (vimpulse-mark-object 'vimpulse-paren-range arg ?<))

(defun vimpulse-a-single-quote (arg)
  "Select a single quoted expression."
  (interactive "p")
  (vimpulse-mark-object 'vimpulse-quote-range arg ?' t))

(defun vimpulse-inner-single-quote (arg)
  "Select inner single quoted expression."
  (interactive "p")
  (vimpulse-mark-object 'vimpulse-quote-range arg ?'))

(defun vimpulse-a-double-quote (arg)
  "Select a double quoted expression."
  (interactive "p")
  (vimpulse-mark-object 'vimpulse-quote-range arg ?\" t))

(defun vimpulse-inner-double-quote (arg)
  "Select inner double quoted expression."
  (interactive "p")
  (vimpulse-mark-object 'vimpulse-quote-range arg ?\"))

(define-key vimpulse-operator-basic-map "aw" 'vimpulse-a-word)
(define-key vimpulse-operator-basic-map "iw" 'vimpulse-inner-word)
(define-key vimpulse-operator-basic-map "aW" 'vimpulse-a-Word)
(define-key vimpulse-operator-basic-map "iW" 'vimpulse-inner-Word)
(define-key vimpulse-operator-basic-map "as" 'vimpulse-a-sentence)
(define-key vimpulse-operator-basic-map "is" 'vimpulse-inner-sentence)
(define-key vimpulse-operator-basic-map "ap" 'vimpulse-a-paragraph)
(define-key vimpulse-operator-basic-map "ip" 'vimpulse-inner-paragraph)
(define-key vimpulse-operator-basic-map "ab" 'vimpulse-a-paren)
(define-key vimpulse-operator-basic-map "a(" 'vimpulse-a-paren)
(define-key vimpulse-operator-basic-map "a)" 'vimpulse-a-paren)
(define-key vimpulse-operator-basic-map "ib" 'vimpulse-inner-paren)
(define-key vimpulse-operator-basic-map "i(" 'vimpulse-inner-paren)
(define-key vimpulse-operator-basic-map "i)" 'vimpulse-inner-paren)
(define-key vimpulse-operator-basic-map "aB" 'vimpulse-a-curly)
(define-key vimpulse-operator-basic-map "a{" 'vimpulse-a-curly)
(define-key vimpulse-operator-basic-map "a}" 'vimpulse-a-curly)
(define-key vimpulse-operator-basic-map "iB" 'vimpulse-inner-curly)
(define-key vimpulse-operator-basic-map "i{" 'vimpulse-inner-curly)
(define-key vimpulse-operator-basic-map "i}" 'vimpulse-inner-curly)
(define-key vimpulse-operator-basic-map "a[" 'vimpulse-a-bracket)
(define-key vimpulse-operator-basic-map "a]" 'vimpulse-a-bracket)
(define-key vimpulse-operator-basic-map "i[" 'vimpulse-inner-bracket)
(define-key vimpulse-operator-basic-map "i]" 'vimpulse-inner-bracket)
(define-key vimpulse-operator-basic-map "a<" 'vimpulse-an-angle)
(define-key vimpulse-operator-basic-map "a>" 'vimpulse-an-angle)
(define-key vimpulse-operator-basic-map "i<" 'vimpulse-inner-angle)
(define-key vimpulse-operator-basic-map "i>" 'vimpulse-inner-angle)
(define-key vimpulse-operator-basic-map "a\"" 'vimpulse-a-double-quote)
(define-key vimpulse-operator-basic-map "i\"" 'vimpulse-inner-double-quote)
(define-key vimpulse-operator-basic-map "a'" 'vimpulse-a-single-quote)
(define-key vimpulse-operator-basic-map "i'" 'vimpulse-inner-single-quote)

;;; }}} End Text Objects code

(provide 'vimpulse-text-object-system)

