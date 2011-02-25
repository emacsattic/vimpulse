;;;; Common functions and variables

;;; Variables

(defvar vimpulse-state nil
  "The current Vimpulse state.
To change the state, use `vimpulse-change-state'
or call the state function (e.g., `vimpulse-vi-state').")
(make-variable-buffer-local 'vimpulse-state)

(defvar vimpulse-modeline-tag nil
  "Modeline indicator for the current state.")
(make-variable-buffer-local 'vimpulse-modeline-tag)

(defvar vimpulse-states-alist nil
  "Specifications made by `vimpulse-define-state'.
Entries have the form (STATE . PLIST), where PLIST is a property
list specifying various aspects of the state. To access a property,
use `vimpulse-state-property'.")

(defvar vimpulse-mode-map-alist nil
  "Association list of keymaps to use for Vimpulse modes.
Elements have the form (MODE . KEYMAP), with the first keymaps
having higher priority.")
(make-variable-buffer-local 'vimpulse-mode-map-alist)

(defconst vimpulse-version "0.5+development"
  "The current version of Vimpulse")

(defun vimpulse-version ()
  (interactive)
  (message "Vimpulse version is %s" vimpulse-version))

;;; Functions

(defun vimpulse-add-to-alist (list-var key val &rest elements)
  "Add the assocation of KEY and VAL to the value of LIST-VAR.
If the list already contains an entry for KEY, update that entry;
otherwise add at the end of the list."
  (let ((tail (symbol-value list-var)))
    (while (and tail (not (equal (car-safe (car-safe tail)) key)))
      (setq tail (cdr tail)))
    (if tail
        (setcar tail (cons key val))
      (add-to-list list-var (cons key val) t))
    (if elements
        (apply 'vimpulse-add-to-alist list-var elements)
      (symbol-value list-var))))

(defun vimpulse-concat-lists (&rest sequences)
  "Concatenate lists, removing duplicates.
The firstmost occurrence of an element is retained.
Cons cells are treated as alist entries."
  (let ((result (pop sequences))
        (tail (copy-sequence (pop sequences))))
    (catch 'empty
      (dolist (elt result)
        (cond
         ((null tail)
          (throw 'empty t))
         ((consp elt)
          (setq tail (assq-delete-all (car elt) tail)))
         (t
          (setq tail (delq elt tail))))))
    (if sequences
        (apply 'vimpulse-concatenate-lists result sequences)
      (append result tail))))

(defun vimpulse-get-property (alist key prop)
  "Return property PROP for KEY in ALIST.
ALIST is an association list with entries in the form
\(KEY . PLIST), where PLIST is a property list.
If KEY is nil, return an association list of states and
their PROP values."
  (let (result val)
    (unless (keywordp prop)
      (setq prop (intern (format ":%s" prop))))
    (if key
        (plist-get (cdr (assq key alist)) prop)
      (dolist (entry alist result)
        (setq key (car entry)
              val (plist-get (cdr entry) prop))
        (when val
          (add-to-list 'result (cons key val) t))))))

(defun vimpulse-put-property (alist-var key prop val &rest properties)
  "Set PROP to VAL for KEY in ALIST-VAR.
ALIST-VAR points to an association list with entries in the form
\(KEY . PLIST), where PLIST is a property list storing PROP and VAL."
  (let* ((alist (symbol-value alist-var))
         (plist (cdr (assq key alist))))
    (while
        (progn
          (unless (keywordp prop)
            (setq prop (intern (format ":%s" prop))))
          (setq plist (plist-put plist prop val))
          (when properties
            (setq prop (pop properties)
                  val (pop properties)))))
    (set alist-var (assq-delete-all key alist))
    (add-to-list alist-var (cons key plist) t)))

;; simple version of cl.el's `rotatef'
(defmacro vimpulse-swap (this that &rest vars)
  "Swap the values of variables THIS and THAT.
If three or more arguments are given, the values are rotated.
E.g., (vimpulse-swap A B C) sets A to B, B to C, and C to A."
  `(progn
     (setq ,this (prog1 ,that
                   (setq ,that ,this)))
     ,@(when vars
         `((vimpulse-swap ,that ,@vars)))))

(defmacro vimpulse-sort (min max &rest vars)
  "Place the smallest value in MIN and the largest in MAX.
If three or more arguments are given, place the smallest
value in the first argument and the largest in the last,
sorting in between."
  `(let ((sorted (sort (list ,min ,max ,@vars) '<)))
     (setq ,min (pop sorted)
           ,max (pop sorted)
           ,@(let (forms)
               (while vars
                 (add-to-list 'forms (pop vars) t)
                 (add-to-list 'forms '(pop sorted) t))
               forms))))

;; macro helper
(eval-and-compile
  (defun vimpulse-unquote (exp)
    "Return EXP unquoted."
    (if (eq (car-safe exp) 'quote)
        (cadr exp)
      exp)))

(defmacro vimpulse-save-echo-area (&rest body)
  "Save the echo area; execute BODY; restore the echo area.
Intermittent messages are not logged in the *Messages* buffer."
  (declare (indent defun))
  `(let ((oldmsg (current-message))
         message-log-max)
     (unwind-protect
         (progn ,@body)
       (if oldmsg (message "%s" oldmsg)
         (message nil)))))

(defun vimpulse-unlogged-message (string &rest args)
  "Display an unlogged message in the echo area.
That is, the message is not logged in the *Messages* buffer.
\(To log the message, just use `message'.)"
  (let (message-log-max)
    (apply 'message string args)))

(provide 'vimpulse-common)
