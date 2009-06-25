
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
				   (list ?q ?i ?a ?c ?d ?y ?! ?< ?> ?= ?# ?r ?R ?\" ;;modified ?i ?a ?q
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
		     (error "giovanni"))
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
		(viper= char ?=) ; the == command
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
	   ((equal com '(?q . ?d)) (vimpulse-test-function value))
	   ((equal com '(?a . ?d)) (vimpulse-delete-text-objects-command value ?a)) ; da<x>
	   ((equal com '(?a . ?c)) (vimpulse-change-text-objects-command value ?a)) ; ca<x>
	   ((equal com '(?a . ?y)) (vimpulse-yank-text-objects-command value ?a)) ; ya<x>
	   ((equal com '(?i . ?d)) (vimpulse-delete-text-objects-command value ?i)) ; di<x>
	   ((equal com '(?i . ?c)) (vimpulse-change-text-objects-command value ?i)) ; ci<x>
	   ((equal com '(?i . ?y)) (vimpulse-yank-text-objects-command value ?i)) ; yi<x>
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

(provide 'vimpulse-viper-function-redefinitions)
