
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
     (t (error "called with wrong arguments"))))
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
	    (message "debug1")
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
	   ((equal com '(?a . ?d)) (vimpulse-delete-text-objects-command value ?a)) ; da<x>
	   ((equal com '(?a . ?c)) (vimpulse-change-text-objects-command value ?a)) ; ca<x>
	   ((equal com '(?i . ?d)) (vimpulse-delete-text-objects-command value ?i)) ; di<x>
	   ((equal com '(?i . ?c)) (vimpulse-change-text-objects-command value ?i)) ; ci<x>
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
	  (goto-char (car bounds))
	  (set-mark (1+ (cadr bounds)))
	  (call-interactively 'kill-region)))))

  (defun vimpulse-delete-text-objects-command (count char)
    "Deletes COUNT text objects following the behavior CHAR ('inner' or 'a').
The kind of text object is asked interactively to the user using `read-char'."
    (interactive)
    (let ((motion (read-char)))
      (viper-set-destructive-command (list 'vimpulse-delete-text-objects-function (list count char motion) ?d nil nil nil))
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
      (viper-set-destructive-command (list 'vimpulse-change-text-objects-function (list count char motion) ?c nil nil nil))
      (vimpulse-delete-text-objects-function (cons (list count char motion) ?c))
      (viper-change-state-to-insert)))
  )
;;; }}} End Text Objects code
(provide 'vimpulse-text-object-system)
