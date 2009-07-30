
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
	(re-search-forward "[\n\r[:space:]]")
	(push (- (point) 2) result)
	(backward-char)
	(re-search-backward "[\n\r[:space:]]")
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

  (defun vimpulse-get-text-object-bounds-i (pos motion)
    "Returns the inner boundaries of a text object at point `pos'.
`motion' identifies the text object:
  - w -> word
  - W -> Word
  - s -> sentence
  - p -> paragraph
  - <paren> -> paren block (see variable `vimpulse-paren-matching-table'
               to see the supported parens.
  - <quote> -> quoted expression (see variable `paired-expression-delimiter'
               to see the type of quotes supported."
    (cond
     ((= motion ?w) (vimpulse-get-vword-bounds pos))
     ((= motion ?W) (vimpulse-get-vWord-bounds pos))
     ((= motion ?s) (vimpulse-get-sentence-bounds pos))
     ((= motion ?p) (vimpulse-get-paragraph-bounds pos))
     ((memq motion vimpulse-paired-expression-delimiters)
      (let ((bounds (vimpulse-get-paired-bounds pos motion)))
	(when bounds 
	    (destructuring-bind (s e) bounds
	      (list (1+ s) (1- e))))))
     ((memq motion vimpulse-balanced-bounds-char-list) 
      (let ((bounds (vimpulse-get-balanced-bounds pos motion)))
	(when bounds 
	    (destructuring-bind (s e) bounds
	      (list (1+ s) (1- e))))))
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
      (goto-char pos)
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

  
  (defun vimpulse-get-text-object-bounds-a (pos motion)
    "Returns the boundaries of `a' text object, whitespace to be killed included."
    (cond
     ((= motion ?w) 
      (vimpulse-get-bounds-with-whitespace 'vimpulse-get-vword-bounds pos))
     ((= motion ?W) (vimpulse-get-bounds-with-whitespace 'vimpulse-get-vWord-bounds pos))
     ((= motion ?s) (vimpulse-get-bounds-with-whitespace 'vimpulse-get-sentence-bounds pos))
     ((= motion ?p) (vimpulse-get-bounds-with-whitespace 'vimpulse-get-paragraph-bounds pos t))
     ((memq motion vimpulse-paired-expression-delimiters)
      (vimpulse-get-paired-bounds pos motion))
     ((memq motion vimpulse-balanced-bounds-char-list) 
      (vimpulse-get-balanced-bounds pos motion))
     (t nil)))
 
  (defun vimpulse-get-text-object-bounds (pos char motion)
    "Returns the boundaries of a text object. 'pos' indicates the start position,
char indicates 'inner' (?i) or 'a' (?a) behavior, 'motion' indicates the text-object."
    (cond 
     ((= char ?a) (vimpulse-get-text-object-bounds-a pos motion))
     ((= char ?i) (vimpulse-get-text-object-bounds-i pos motion))
     ((= char ?r) (list pos (+ pos (- (cadr motion) (car motion) 1))))
     ((= char ?l) (vimpulse-get-line-margins pos))
     (t (error "called with wrong arguments"))))
  (defun vimpulse-message-all-args (&rest args)
    "Helper function that prints all its arguments, plus some other values."
    (message "ARGS: %s, reg: %s" args (string viper-use-register)))
  (defun vimpulse-test-function (value)
    "This function is only defined for developing purposes."
    (viper-set-destructive-command (list 'vimpulse-message-all-args 'first-argument ?d viper-use-register "cane" nil)))
;;;;;;;;;;;;;;;;;;;;
;;;   Commands   ;;;
;;;;;;;;;;;;;;;;;;;;
  (defun vimpulse-unify-multiple-bounds (pos char count motion)
    "Returns the boundaries of a multiple text object motion. 
POS is the starting position,
CHAR indicates 'inner' or 'a' behavior,
COUNT indicates how many text objects to include,
MOTION indicates the kind of text object."
    (let* ((bounds-1 (vimpulse-get-text-object-bounds pos char motion))
	   (start (when bounds-1 (car bounds-1)))
	   (end (when bounds-1 (cadr bounds-1))))
      (dotimes (i (1- count))
	(setq end (cadr (vimpulse-get-text-object-bounds (1+ end) char motion))))
      (if end (list start end) nil)))

  (defun vimpulse-delete-text-objects-function (arg)
    "Deletes COUNT text objects of MOTION kind starting from `point', following the 
behavior indicated by CHAR: ?i stands for 'inner', ?a stands for 'a'. 
ARG has the form ((COUNT CHAR MOTION) . ?d)"
    (destructuring-bind (count char motion) (car arg)
      (let ((bounds (vimpulse-unify-multiple-bounds (point) char count motion)))
	(when bounds
	  (when viper-use-register ;; copy stuff to registers
	    ;; This code is take from viper-exec-delete
	    (cond 
	     ((viper-valid-register viper-use-register '(letter digit))
	      (copy-to-register
	       viper-use-register (car bounds) (1+ (cadr bounds)) nil))
	     ((viper-valid-register viper-use-register '(Letter))
	      (viper-append-to-register
	       (downcase viper-use-register) (car bounds) (1+ (cadr bounds))))
	     (t (setq viper-use-register nil)
		(error viper-InvalidRegister viper-use-register)))
	    (setq viper-use-register nil))
	  ;;end of viper-exec-delete code
	  (goto-char (car bounds))
	  (set-mark (1+ (cadr bounds)))
	  (call-interactively 'kill-region)))))

  (defun vimpulse-delete-text-objects-command (count char)
    "Deletes COUNT text objects following the behavior CHAR ('inner' or 'a').
The kind of text object is asked interactively to the user using `read-char'."
    (interactive)
    (let ((motion (read-char)))
      (viper-set-destructive-command (list 'vimpulse-delete-text-objects-function 
					   (list count char motion) ?d viper-use-register nil nil))
      (vimpulse-delete-text-objects-function (cons (list count char motion) ?d))))

  (defun vimpulse-change-text-objects-function (arg)
    "Executes `vimpulse-delete-text-objects-function' passing ARG to it and yanks the last insertion."
    (vimpulse-delete-text-objects-function arg)
    (viper-yank-last-insertion))

  (defun vimpulse-change-text-objects-command (count char)
    "Changes COUNT text objects following the behavior CHAR ('inner' or 'a').
The kind of text object is asked interactively to the user using `read-char'."
    (interactive)
    (let ((motion (read-char)))
      (viper-set-destructive-command (list 'vimpulse-change-text-objects-function (list count char motion) 
					   ?c viper-use-register nil nil))
      (vimpulse-delete-text-objects-function (cons (list count char motion) ?c))
      (viper-change-state-to-insert)))
  
  (defun vimpulse-yank-text-objects-function (arg)
    "Yanks COUNT text objects of MOTION kind starting from `point', following the 
behavior indicated by CHAR: ?i stands for 'inner', ?a stands for 'a'. 
ARG has the form ((COUNT CHAR MOTION) . ?d)"
    (destructuring-bind (count char motion) (car arg)
      (let ((bounds (vimpulse-unify-multiple-bounds (point) char count motion)))
	(when bounds
	  (when viper-use-register ;; copy stuff to registers
	    ;; This code is take from viper-exec-delete
	    (cond 
	     ((viper-valid-register viper-use-register '(letter digit))
	      (copy-to-register
	       viper-use-register (car bounds) (1+ (cadr bounds)) nil))
	     ((viper-valid-register viper-use-register '(Letter))
	      (viper-append-to-register
	       (downcase viper-use-register) (car bounds) (1+ (cadr bounds))))
	     (t (setq viper-use-register nil)
		(error viper-InvalidRegister viper-use-register)))
	    (setq viper-use-register nil))
	  ;;end of viper-exec-delete code
	  (copy-region-as-kill (car bounds) (1+ (cadr bounds)))
	  (goto-char (car bounds))))))

  (defun vimpulse-yank-text-objects-command (count char)
    "Yanks COUNT text objects following the behavior CHAR ('inner' or 'a').
The kind of text object is asked interactively to the user using `read-char'."
    (interactive)
    (let ((motion (read-char)))
      (vimpulse-yank-text-objects-function (cons (list count char motion) ?y))))
;; This is for silencing viper when he checks if the insertion must be repeated, never true for 
;; this kind of commands.
(defvar vimpulse-text-objects-command (list 'vimpulse-delete-text-objects-function 
					    'vimpulse-change-text-objects-function 
					    'vimpulse-yank-text-objects-function))
(defadvice viper-repeat-insert-command (around vimpulse-text-objects-repeat-insert-command-fix activate)
  (when (not (memq (car viper-d-com) vimpulse-text-objects-command))
    ad-do-it))
;;; }}} End Text Objects code
(provide 'vimpulse-text-object-system)
