;; test-framework.el --- framework for unit testing -*- coding: utf-8 -*-

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Created: May 2010
;; Keywords: unit testing

;;; Commentary:

;; This file provides assertions, tests, suites, fixtures,
;; mocks, stubs and a lengthy description thereof.

;;; Tests

;; A simple test may look like:
;;
;;     (deftest test-foo
;;       (assert (= (+ 2 2) 4))
;;       (assert (= (* 3 3) 9))
;;       (assert (= (% 4 2) 0)))
;;
;; This checks that 2 + 2 = 4, that 3 * 3 = 9, and that 4 % 2 = 0.
;; (If it isn't, something is seriously wrong!) To run the test:
;;
;;     (test-foo)    ; `M-x test-foo' interactively
;;
;; To run the test when it's defined, specify `:run t':
;;
;;     (deftest test-foo
;;       :run t
;;       (assert (= (+ 2 2) 4))
;;       (assert (= (* 3 3) 9))
;;       (assert (= (% 4 2) 0)))
;;
;; Note that it's a good idea to name tests with a prefix like "test-"
;; to avoid overwriting other functions.
;;
;; Let's simplify it a bit. Most `assert-' forms defined here accept
;; multiple (sets of) arguments, similarly to, e.g., `setq':
;;
;;     (deftest test-foo
;;       :run t
;;       (assert
;;         (= (+ 2 2) 4)
;;         (= (* 3 3) 9)
;;         (= (% 4 2) 0)))
;;
;; This is Lisp, after all! To remove the `=' noise, use `assert-=':
;;
;;     (deftest test-foo
;;       :run t
;;       (assert-=
;;         (+ 2 2) 4
;;         (* 3 3) 9
;;         (% 4 2) 0))
;;
;; Note that xUnit frameworks often use the reverse order, e.g.,
;; "assertEquals(4, 2 + 2);", where the expected value comes first.
;; Here, however, the expectation always comes last, mirroring the
;; original code.
;;
;; At this point it's advisable to add some documentation. Tests as
;; well as assertions can have docstrings:
;;
;;     (deftest test-foo
;;       "Example test."
;;       :run t
;;       (assert-=
;;         "Elementary truths."
;;         (+ 2 2) 4
;;         (* 3 3) 9
;;         (% 4 2) 0))
;;
;; If the test fails, these strings show up in the failure report.
;; (Note, again, that the forms must be ordered as shown above for
;; the report to make sense.)
;;
;; Related actions may be grouped together with `assert-progn', or
;; `should' for a more BDD-like name. It only checks the last form:
;;
;;     (deftest test-baz
;;       "A list."
;;       (let ((list '(a b c)))
;;         (should
;;           "Push element to the front."
;;           (push 'd list)
;;           (eq (first list) 'd))))
;;
;; This is both readable and robust: should the assertion crash
;; midways, the preceding docstring provides context in the failure
;; report. To that end, `should' statements may be nested:
;;
;;     (deftest test-baz
;;       "A list."
;;       (let ((list '(a b c)))
;;         (should
;;           "Push elements to the front."
;;           (push 'd list)
;;           (should (eq (first list) 'd))
;;           (push 'e list)
;;           (should (eq (first list) 'e)))))
;;
;; BDD aliases are defined for all assertions: `should-eq' instead
;; of `assert-eq', `should-=' instead of `assert-=', and so on.
;; `should' can be used in most cases, though: it provides a recursive
;; inspection of the failing form. The alias for `assert' is
;; `should-all', which checks each and every form.
;;
;; NOTE: `assert' only accepts multiple arguments inside `deftest'.
;; Outside `deftest' it's a different macro (defined by cl.el).

;;; Test suites

;; Tests can be grouped into suites with `defsuite'. The most
;; straightforward way is simply to wrap it around them:
;;
;;     (defsuite test-foo-suite
;;       (deftest test-foo
;;         (assert-=
;;           (+ 2 2) 4))
;;       (deftest test-bar
;;         (assert-=
;;           (* 3 3) 9)))
;;
;; Like tests, the suite is executed with (test-foo-suite),
;; `M-x test-foo-suite' or `:run t' in the definition. Suites
;; can also have docstrings:
;;
;;     (defsuite test-foo-suite
;;       "Example suite."
;;       :run t
;;       (deftest test-foo
;;        (assert-=
;;          (+ 2 2) 4)))
;;
;; One can also define the test suite first and then add tests
;; and suites to it, using the `:suite' keyword or `add-to-suite':
;;
;;     (defsuite test-foo-suite
;;       "Example suite.")
;;
;;     (deftest test-foo
;;       :suite test-foo-suite
;;       (assert-=
;;         (+ 2 2) 4))
;;
;;     (deftest test-bar
;;       (assert-=
;;         (* 3 3) 9))
;;
;;     (add-to-suite 'test-foo-suite 'test-bar)
;;
;; Furthermore, `defsuite' forms may nested. (Self-referencing suite
;; definitions should be avoided, although some safeguards exist to
;; prevent infinite loops.)

;;; Fixtures

;; Sometimes it's useful to set up and tear down an environment for
;; each test in a suite. This can be done with the :setup and
;; :teardown keyword arguments, which accept a list of expressions to
;; evaluate before and after each test.
;;
;;     (defsuite test-foo-suite
;;       "Example suite."
;;       :setup ((wibble) (wobble))
;;       :teardown ((wubble) (flob))
;;       (deftest test-foo
;;         ...)
;;       (deftest test-bar
;;         ...))
;;
;; However, this may not be enough: what if the setup and teardown
;; need to share variables, or the test should be wrapped in a macro
;; like `save-restriction'? To that end, the more powerful :fixture
;; keyword argument may be used. It accepts a one-argument function
;; which is used to call the test:
;;
;;     (defsuite test-foo-suite
;;       "Example suite."
;;       :fixture (lambda (body)
;;                  (let (foo bar)
;;                    (wibble)
;;                    (wobble)
;;                    (save-restriction
;;                      (funcall body))   ; run test
;;                    (wubble)
;;                    (flob)))
;;       (deftest test-foo
;;         ...)
;;       (deftest test-bar
;;         ...))
;;
;; As shown above, the function must contain (funcall body) somewhere
;; in its definition for the test to be run at all.
;;
;; There's also the :wrap keyword argument, which specifies an
;; around-advice for the whole test, e.g., :wrap ((wobble) ad-do-it).
;; See the docstring of `defadvice' for more details on advice. While
;; the other fixtures are repeated for each test in the suite, :wrap
;; is executed once for the whole suite. The order is:
;;
;;                  +-------------------+
;;                  |:wrap              |
;;                  |  +--------------+ |
;;                  |  |:setup        | |
;;                  |  |  +---------+ | |
;;                  |  |  |:fixture | | |
;;                  |  |  |  +----+ | | |
;;                  |  |  |  |TEST| | | |
;;                  |  |  |  +----+ | | |
;;                  |  |  +---------+ | |
;;                  |  |:teardown     | |
;;                  |  +--------------+ |
;;                  +-------------------+
;;
;; A test defined as part of a suite carries with it the suite's
;; fixtures even when called outside the suite. However, when the test
;; is called by a different suite, that suite's fixtures temporarily
;; override the fixtures inherited from the original suite.
;;
;; When defining a function to use as a fixture, make sure to define
;; it before the tests are run (before the test if using `:run t').

;;; Mocks and stubs

;; Mocks and stubs are temporary stand-ins for other pieces of code.
;; They are useful for disabling (or "stubbing out") external behavior
;; while testing a unit.
;;
;; To stub a function, use `stub':
;;
;;     (deftest test-foo
;;       "Example test."
;;       (stub foo)
;;       (assert-not (foo)))  ; foo returns nil
;;
;; In the rest of the test, any calls to the stubbed function will
;; return nil. To return a different value, specify the stub's body,
;; e.g., (stub foo t).
;;
;; A stub only changes a function's output, not its input: the
;; argument list remains the same. The stub's body may refer to the
;; original arguments. To change a function's input too, use `mock':
;;
;;     (deftest test-foo
;;       "Example test."
;;       (mock foo (arg)
;;         (1+ arg))
;;       (assert-= (foo 1) 2))  ; foo returns 2
;;
;; `mock' specifies a temporary function to assign to `foo' for the
;; duration of the test. Here it increments its argument by one. When
;; the test completes (or fails), all stubs and mocks are released.
;;
;; If the same mock is frequently reused, put it in a fixture or
;; define a function for it and call that function in the test. Just
;; ensure that it is never called outside a test, otherwise it will
;; not be released (unless wrapped in `with-mocks-and-stubs').

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'advice)
  (require 'warnings))

(eval-and-compile
  (defvar all-suites nil)
  (defvar all-tests nil)
  (defvar silent-tests nil
    "If t, don't echo test results.")
  (defvar logged-tests t
    "If t, log echoed test results in the *Messages* buffer.")
  (defvar deftest-macros nil
    "Macros that shadow global definitions inside `deftest'.")
  (defvar assert-name nil
    "Name of the current assertion.")
  (defvar assert-string nil
    "Textual description of the current assertion.")
  (defvar test-string nil
    "Textual description of the current test.")
  (defvar suite-name nil
    "Name of the current suite."))

;;; Test suite macro: `defsuite'

(defmacro defsuite (suite &rest body)
  "Define a test suite."
  (declare (indent defun)
           (debug (&define name
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let (debug doc dont-add fixture keyword parents run setup teardown wrap)
    ;; collect docstring
    (when (stringp (car body))
      (setq doc (pop body)))
    ;; collect keywords
    (while (keywordp (car body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :run)
        (setq run (pop body))
        (unless run
          (setq dont-add t)))
       ((memq keyword '(:parent :suite))
        (if (listp (car body))
            (setq parents (append parents (pop body)))
          (add-to-list 'parents (pop body))))
       ((eq keyword :fixture)
        (setq fixture (pop body))
        (unless (or (symbolp fixture)
                    (functionp fixture))
          (setq fixture `(lambda () ,@fixture))))
       ((eq keyword :setup)
        (setq setup (pop body))
        (unless (or (symbolp setup)
                    (functionp setup))
          (setq setup `(lambda () ,@setup))))
       ((eq keyword :teardown)
        (setq teardown (pop body))
        (unless (or (symbolp teardown)
                    (functionp teardown))
          (setq teardown `(lambda () ,@teardown))))
       ((memq keyword '(:advice :wrap))
        (setq wrap (pop body)))
       ((eq keyword :debug)
        (setq debug (pop body)))
       (t
        (pop body))))
    ;; macro expansion
    `(macrolet ,deftest-macros
       (when (and (boundp 'suite-name) suite-name ,(not dont-add))
         (add-to-suite suite-name ',suite))
       ;; create a `let' binding that test definitions can pick up on
       (let ((suite-name ',suite))
         (add-to-list 'all-suites ',suite)
         (dolist (suite ',parents)
           (add-to-suite suite ',suite))
         ;; create the suite function and suite variable
         (defvar ,suite nil ,doc)
         (defun ,suite (&optional debug &rest tests)
           ,doc
           (interactive "p")
           (let ((logged-tests logged-tests)
                 (silent-tests silent-tests)
                 (suite-name ',suite)
                 (passed t)
                 own-tests)
             (setq debug (or debug ',debug))
             (unless (memq debug '(batch debug))
               (setq debug (if debug 'debug 'batch)))
             (unless tests
               (setq own-tests t
                     tests ,suite)
               (test-message "Test suite `%s' running ..." ',suite))
             (dolist (test tests)
               (let (test-string assert-name assert-string error-string)
                 (if (eq debug 'debug)
                     (let ((debug-on-error t))
                       (with-fixtures ,fixture ,setup ,teardown
                         (funcall test 'batch)))
                   (condition-case err
                       ,(if (eq suite 'empty-suite)
                            '(funcall test
                                      (unless (default-suite test)
                                        'batch))
                          `(with-fixtures ,fixture ,setup ,teardown
                             (funcall test 'batch)))
                     (error (prog1 nil
                              (setq error-string
                                    (error-message-string err))))))
                 (if (null error-string)
                     (test-message "%sTest `%s' passed!"
                                   (if own-tests "    " "") test)
                   (test-message "%sTest `%s' failed!"
                                 (if own-tests "    " "") test)
                   (test-warning
                    ',(if (eq suite 'empty-suite) 'test suite)
                    (replace-regexp-in-string
                     "\n\\([\n]*\\)\\'" "" ; remove extra trailing newlines
                     (replace-regexp-in-string
                      "\\`[ \t\n]*" "" ; remove leading space
                      (replace-regexp-in-string
                       "\\(\n\\)[^\n]" "\n\t" ; indent message
                       (format "%s%s\n\n%s\n"
                               ,(if (and doc (not (eq suite 'empty-suite)))
                                    (format "%s\n\n" doc) "")
                               (if test-string
                                   (format "%s (%s)" test-string test)
                                 (format "%s" test))
                               (cond
                                ((and assert-name assert-string)
                                 (format "`%s' failed: %s\n\n%s"
                                         assert-name
                                         assert-string
                                         error-string))
                                (assert-name
                                 (format "`%s' failed:\n\n%s"
                                         assert-name error-string))
                                (t
                                 (format "%s" error-string))))
                       nil nil 1))
                     nil nil 1))
                   (setq passed nil))))
             (when own-tests
               ;; if `silent-tests' is t and the suite is called
               ;; interactively, echo an unlogged summary
               (when (and silent-tests
                          ,(if (version< emacs-version "23")
                               '(called-interactively-p)
                             '(called-interactively-p 'any)))
                 (setq logged-tests nil
                       silent-tests nil))
               (if passed
                   (test-message "Test suite `%s' passed!" ',suite)
                 (test-message "Test suite `%s' failed!" ',suite)))
             passed))
         ;; :wrap function?
         ,@(when wrap
             `((defadvice ,suite (around wrap activate)
                 ,@(if (listp wrap)
                       wrap
                     `((,wrap ad-do-it))))))
         ,@body
         (when ,run (,suite))
         ',suite))))

(defsuite empty-suite
  "Pseudo-suite for suiteless tests.
Tests can call themselves via this suite if not associated
with any other suite.")

(defun add-to-suite (suite test)
  "Add TEST to SUITE."
  (unless (boundp suite)
    (eval `(defsuite ,suite)))
  ;; suites are basically hooks
  (add-hook suite test))

(defmacro with-fixtures (fixture setup teardown &rest body)
  "Run BODY with fixtures.
FIXTURE is a one-argument function with which to run the contents
of BODY; SETUP and TEARDOWN are zero-argument functions to run
before and after. Mocks and stubs are guaranteed to be released."
  (declare (indent defun)
           (debug t))
  ;; create an uninterned symbol to avoid overwriting
  ;; any internal bindings in :fixture
  (let ((resultvar (make-symbol "result")))
    `(let ((fixture ',fixture)
           (setup ',setup)
           (teardown ',teardown))
       (with-mocks-and-stubs
         (unwind-protect
             (save-excursion
               (let (,resultvar)
                 (when setup
                   (funcall setup))
                 (if fixture
                     (funcall fixture
                              (lambda ()
                                (setq ,resultvar (progn ,@body))))
                   (setq ,resultvar (progn ,@body)))
                 ,resultvar))
           (when teardown
             (funcall teardown)))))))

;;; Test macro: `deftest'

(defmacro deftest (test &rest body)
  "Define a test."
  (declare (indent defun)
           (debug (&define name
                           [&optional symbolp]
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let (debug doc dont-add keyword run suites)
    ;; collect docstring
    (when (stringp (car body))
      (setq doc (pop body)))
    ;; collect keywords
    (while (keywordp (car body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :debug)
        (setq debug (pop body)))
       ((eq keyword :run)
        (setq run (pop body))
        (unless run
          (setq dont-add t)))
       ((memq keyword '(:suite :suites))
        (if (listp (car body))
            (setq suites (append suites (pop body)))
          (add-to-list 'suites (pop body))))
       (t
        (pop body))))
    ;; macro expansion
    `(macrolet ,deftest-macros
       (add-to-list 'all-tests ',test)
       (dolist (suite ',suites)
         (add-to-suite suite ',test))
       (when (and (boundp 'suite-name) suite-name)
         (put ',test 'suite suite-name)
         (unless ,dont-add
           (add-to-suite suite-name ',test)))
       (defun ,test (&optional debug suite)
         ,doc
         (interactive "p")
         (let ((suite (or suite (default-suite ',test 'empty-suite)))
               (logged-tests logged-tests)
               (silent-tests silent-tests))
           ;; if `silent-tests' is t and the test is called
           ;; interactively, echo the result unlogged
           (when (and silent-tests
                      ,(if (version< emacs-version "23")
                           '(called-interactively-p)
                         '(called-interactively-p 'any)))
             (setq logged-tests nil
                   silent-tests nil))
           (cond
            ((or (eq debug 'batch)
                 (eq suite (and (boundp 'suite-name) suite-name)))
             (setq test-string ,doc
                   assert-name nil
                   assert-string nil)
             ,@body t)
            (t
             (funcall suite (or debug ',debug) ',test)))))
       (when ,run (,test))
       ',test)))

(defun default-suite (test &optional default)
  "Return the default suite of TEST."
  (or (when (symbolp test)
        (get test 'suite))
      default))

;; Currently, this produces a warning. Could it produce linkified
;; text in a separate buffer instead? (`with-output-to-temp-buffer')
(defun test-warning (suite message)
  "Display MESSAGE warning for SUITE."
  (display-warning suite message))

(defun test-message (string &rest args)
  "Conditionally echo a message.
If `silent-tests' is t, don't echo the message.
If `logged-tests' is nil, don't log the message
in the *Messages* buffer."
  (let ((message-log-max logged-tests))
    (unless silent-tests
      (apply 'message string args))))

;;; Assertion macro: `defassert'

;; The following is rather hairy. What it actually does is to
;; implement different ways of calling an assertion macro:
;;
;;     (assert-equal foo bar)
;;     (assert-equal
;;       "Is foo equal to bar?"
;;       foo bar)
;;     (assert-equal
;;       foo bar
;;       baz qux)
;;     (assert-equal
;;       "Is foo equal to bar and baz equal to qux?"
;;       foo bar
;;       baz qux)
;;
;; Assertions must be macros, not functions, to be able to use the
;; unevaluated expressions in the failure report. Thus we wind up
;; with a macro whose expansion is another macro definition.
(defmacro defassert (name args &rest body)
  "Define an assertion macro.

    (defassert assert-macro (doc arg1 arg2)
      ;; macro expansion
      )

The first argument in the argument list is the docstring.
The rest of the arguments are assumed to be repeatable, that is,
\(assert-macro \"test\" x1 y1 x2 y2 ...), unless a &rest argument
is specified."
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           def-body)))
  (let (aliases def docstring keyword param restvar shadow)
    (setq param (copy-sequence args))
    (when (stringp (car body))
      (setq docstring (pop body)))
    (while (keywordp (car body))
      (setq keyword (pop body))
      (cond
       ((memq keyword '(:alias :aliases))
        (setq aliases (pop body))
        (unless (listp aliases)
          (setq aliases (list aliases))))
       ((eq keyword :shadow)
        (setq shadow (pop body)))
       (t
        (pop body))))
    (if (memq '&rest param)
        ;; if &rest is specified, just make a `let' wrapper
        ;; for the documentation argument
        (setq restvar (car (last param))
              param (nbutlast param 2)
              def `(let ((name ',name)
                         (doc (when (stringp (car-safe ,restvar))
                                (pop ,restvar))))
                     `(progn (setq assert-name ',name
                                   assert-string ,doc)
                             (let (assert-name assert-string)
                               ,,@body t))))
      ;; repeatable argument list: iterate through the arguments
      (setq restvar 'body
            def
            ;; grab first argument as docstring if appropriate
            `(let* ((name ',name)
                    (doc (when (and (> (length ,restvar) 0)
                                    ,@(when (> (length param) 1)
                                        `((/= (mod (length ,restvar)
                                                   ,(length param)) 0)))
                                    (stringp ,(car-safe param)))
                           ,(car param)))
                    def)
               ;; then store the remainder in `restvar' for iteration
               (if doc
                   (setq ,restvar (append (list ,@(cdr param)) ,restvar))
                 (setq ,restvar (append (list ,@param) ,restvar)))
               ;; Go through `restvar' and bind the arguments to
               ;; successive values, evaluating the assertion's
               ;; definition each time. The concatenation of
               ;; these evaluations is the macro's expansion
               ;; (stored in `def').
               (while ,restvar
                 ,@(mapcar (lambda (var)
                             `(setq ,var (pop ,restvar))) param)
                 (add-to-list 'def (progn ,@body) t))
               `(progn (setq assert-name ',name
                             assert-string ,doc)
                       (let (assert-name assert-string)
                         ,@def t)))))
    `(progn
       ;; define assertion macro unless :shadow is t,
       ;; in which case just store it for macro-letting
       ,(if shadow
            `(eval-and-compile
               (add-to-list 'deftest-macros
                            '(,name (,@param &rest ,restvar) ,def))
               (put ',name 'lisp-indent-function 'defun))
          `(defmacro ,name (,@param &rest ,restvar)
             ,docstring
             (declare (indent defun))
             ,def))
       ;; define aliases
       ,@(mapcar (lambda (name)
                   `(defassert ,name ,args
                      ,docstring
                      ,@body))
                 aliases)
       ',name)))

;; assert { 2.0 }-like display of arguments and their evaluated
;; values. Is this sufficient, or should we use `macrolet' to remap
;; (assert (eq foo bar)) to (assert-eq foo bar)?
(defun make-eval-description (form eval &optional indent)
  "Return a string mapping FORM to EVAL.
EVAL is an evaluation tree as returned by `make-eval-tree'.
Example output when FORM is (= (+ (1+ 2) 2) 4):

    (= (+ (1+ 2) 2) 4) => nil
       (+ (1+ 2) 2) => 5
          (1+ 2) => 3"
  (let ((indent (or indent 0)) description val)
    (cond
     ((and (or (equal form eval)
               (eq (car-safe form) 'quote))
           ;; always print the root form
           (> indent 0))
      "")
     ((make-eval-tree-p (car-safe form))
      (setq val (car eval))
      ;; strip strings of text properties for readability
      (when (stringp val)
        (setq val (copy-sequence val))
        (set-text-properties 0 (length val) nil val))
      (setq description (format "%s%S => %S\n"
                                (make-string indent ?\ )
                                form val)
            indent (+ indent (length (format "%S" (car form))) 2))
      (while (cadr eval)
        (setq description
              (concat description
                      (make-eval-description
                       (cadr form) (cadr eval)
                       indent))
              form (cdr form)
              eval (cdr eval)))
      description)
     (t
      (when (stringp eval)
        (setq eval (copy-sequence eval))
        (set-text-properties 0 (length eval) nil eval))
      (format "%s%S => %S\n"
              (make-string indent ?\ )
              form eval)))))

(defun make-eval-tree (form)
  "Return evalution tree for FORM.
The tree is structurally similar to FORM, but all the
function symbols have been replaced with their return values.
For example, (+ 2 2) evaluates to (4 2 2).

Note that macros are not expanded beforehand. Instead, forms
listed in `evaluated-forms' are evaluated as regular functions.
Other forms are just replaced by their return value."
  (let (args elt rest sym tail)
    (cond
     ((make-eval-tree-p (car-safe form))
      (setq form (copy-sequence form)
            sym (car form)
            rest (cdr form)
            tail (mapcar 'make-eval-tree rest))
      (dolist (arg tail)
        (if (make-eval-tree-p (car-safe (pop rest)))
            (setq args (append args (list (list 'quote (car arg)))))
          (setq args (append args (list (list 'quote arg))))))
      (setq sym (eval (append (list sym) args)))
      (setcar form sym)
      (setcdr form tail)
      form)
     (t
      (eval form)))))

(defun make-eval-tree-p (sym)
  "Whether SYM may be evaluated as a function."
  (or (functionp sym)
      (memq sym evaluated-forms)))

(defvar evaluated-forms
  '(and
    if
    or
    prog1
    progn
    save-excursion
    save-restriction
    unless
    when)
  "Special forms whose arguments may be evaluated by `make-eval-tree'.
This might yield a more throughout evaluation than the regular
control flow: for example, `and' normally stops at the first nil
argument, skipping the rest. On the bright side, the upshot of
evaluating all arguments is more information.")

(defassert assert (form)
  "Verify that FORM returns non-nil."
  :alias (assert-that should-and should-all)
  :shadow t
  `(let* ((form ',form)
          (eval (make-eval-tree form))
          (retval (if (and (listp form) (listp eval))
                      (car-safe eval)
                    eval)))
     (without-mocks-and-stubs
       (unless retval
         (error (replace-regexp-in-string
                 "\\(\n\\)\\(?:.\\|\n\\)*\\'"
                 ", expected non-nil\n"
                 (make-eval-description form eval)
                 nil nil 1))))))

(defassert assert-not (form)
  "Verify that FORM returns non-nil."
  :alias (assert-nil should-nor should-all-not)
  `(let* ((form ',form)
          (eval (make-eval-tree form))
          (retval (if (and (listp form) (listp eval))
                      (car-safe eval)
                    eval)))
     (without-mocks-and-stubs
       (when retval
         (error (replace-regexp-in-string
                 "\\(\n\\)\\(?:.\\|\n\\)*\\'"
                 ", expected nil\n"
                 (make-eval-description form eval)
                 nil nil 1))))))

(defassert assert-progn (&rest body)
  "Verify that the last form in BODY returns non-nil."
  :alias should
  (let ((form (car (last body)))
        (body (butlast body 1)))
    `(let* ((form ',form)
            (eval (progn ,@body (make-eval-tree form)))
            (retval (if (and (listp form) (listp eval))
                        (car-safe eval)
                      eval)))
       (without-mocks-and-stubs
         (unless retval
           (error (replace-regexp-in-string
                   "\\(\n\\)\\(?:.\\|\n\\)*\\'"
                   ", expected non-nil\n"
                   (make-eval-description form eval)
                   nil nil 1)))))))

(defassert assert-not-progn (&rest body)
  "Verify that the last form in BODY returns nil."
  :alias should-not
  (let ((form (car (last body)))
        (body (butlast body 1)))
    `(let* ((form ',form)
            (eval (progn ,@body (make-eval-tree form)))
            (retval (if (and (listp form) (listp eval))
                        (car-safe eval)
                      eval)))
       (without-mocks-and-stubs
         (when retval
           (error (replace-regexp-in-string
                   "\\(\n\\)\\(?:.\\|\n\\)*\\'"
                   ", expected nil\n"
                   (make-eval-description form eval)
                   nil nil 1)))))))

;; xUnit discrepancy: xUnit favors "Yoda conditions"
;; (expected actual). We don't (actual expected).
(defassert assert-equal (actual expected)
  "Verify that ACTUAL is `equal' to EXPECTED."
  :alias should-equal
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (equal actual expected)
         (error "%S was %S, expected %S"
                ',actual actual expected)))))

(defassert assert-not-equal (actual expected)
  "Verify that ACTUAL is not `equal' to EXPECTED."
  :alias should-not-equal
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (when (equal actual expected)
         (error "%S was %S, expected not %S"
                ',actual actual expected)))))

(defassert assert-eq (actual expected)
  :alias should-eq
  "Verify that ACTUAL is `eq' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (eq actual expected)
         (error "%S was %S, expected %S"
                ',actual actual expected)))))

(defassert assert-not-eq (actual expected)
  :alias should-not-eq
  "Verify that ACTUAL is not `eq' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (when (eq actual expected)
         (error "%S was %S, expected not %S"
                ',actual actual expected)))))

(defassert assert-eql (actual expected)
  :alias should-eql
  "Verify that ACTUAL is `eql' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (eql actual expected)
         (error "%S was %S, expected %S"
                ',actual actual expected)))))

(defassert assert-not-eql (actual expected)
  :alias should-not-eql
  "Verify that ACTUAL is not `eql' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (when (eql actual expected)
         (error "%S was %S, expected not %S"
                ',actual actual expected)))))

(defassert assert-= (actual expected)
  :alias should-=
  "Verify that ACTUAL is `=' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (= actual expected)
         (error "%S was %S, expected %S"
                ',actual actual expected)))))

(defassert assert-/= (actual expected)
  "Verify that ACTUAL is `/=' to EXPECTED."
  :alias should-/=
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (/= actual expected)
         (error "%S was %S, expected not %S"
                ',actual actual expected)))))

(defassert assert-string= (actual expected)
  "Verify that ACTUAL is `string=' to EXPECTED."
  :alias should-string=
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (string= actual expected)
         (error "%S was %S, expected %S"
                ',actual actual expected)))))

(defassert assert-not-string= (actual expected)
  "Verify that ACTUAL is not `string=' to EXPECTED."
  :alias should-not-string=
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (when (string= actual expected)
         (error "%S was %S, expected not %S"
                ',actual actual expected)))))

(defassert assert-member (elt list)
  "Verify that ELT is `member' of LIST."
  :alias should-member
  `(let ((elt ,elt)
         (list ,list))
     (without-mocks-and-stubs
       (unless (member elt list)
         (error "expected %S in %S"
                elt list)))))

(defassert assert-memq (elt list)
  "Verify that ELT is `memq' of LIST."
  :alias should-memq
  `(let ((elt ,elt)
         (list ,list))
     (without-mocks-and-stubs
       (unless (memq elt list)
         (error "expected %S in %S"
                elt list)))))

(defassert assert-match (regexp string)
  "Verify that REGEXP matches STRING."
  :alias should-match
  `(let ((regexp ,regexp)
         (string ,string))
     (without-mocks-and-stubs
       (unless (string-match regexp string)
         (error "expected \"%S\" to match \"%S\""
                regexp string)))))

(defassert assert-error (&rest body)
  "Verify that BODY signals an error."
  :alias should-error
  (let ((body (if (> (length body) 1)
                  `(progn ,@body)
                (car-safe body)))
        (failvar (make-symbol "failed")))
    `(let (,failvar)
       (condition-case nil
           (progn
             ,body
             (setq ,failvar t))
         (error nil))
       (without-mocks-and-stubs
         (when ,failvar
           (error "%S expected to signal an error"
                  ',body))))))

(defassert assert-changed (form &rest body)
  "Verify that FORM changes after executing BODY.
FORM must be quoted."
  :alias should-change
  (let ((form (eval form)))
    `(let ((form ,form))
       (assert-not-equal
         (progn
           ,@body
           ,form)
         form))))

(defassert assert-not-changed (form &rest body)
  "Verify that FORM does not change after executing BODY.
FORM must be quoted."
  :alias should-not-change
  (let ((form (eval form)))
    `(let ((form ,form))
       (assert-equal
         (progn
           ,@body
           ,form)
         form))))

;;; Mocks/stubs

(defvar stubs nil
  "List of stubbed functions.")

(defvar stubs-global nil
  "List of all stubs in all contexts.")

(defvar mocks-alist nil
  "Alist of mocked functions.")

(defvar mocks-global-alist nil
  "Alist of all mocks in all contexts.")

;; stubs are made with advice, so the number of arguments is unchanged
(defmacro stub (func &rest body)
  "Stub out FUNC.
A stub is a temporary function advice shadowing the real
definition (so the argument list is retained). It is released
with `release-stubs'. You should stub inside `deftest' only,
which releases automatically."
  (declare (indent defun)
           (debug t))
  `(cond
    ((fboundp ',func)
     (add-to-list 'stubs ',func nil 'eq)
     (add-to-list 'stubs-global ',func nil 'eq)
     (defadvice ,func (around stub activate)
       (if stubs-global
           (setq ad-return-value
                 (progn ,@(or body '(nil))))
         ad-do-it)))
    (t
     (mock ,func (&rest args)))))

(defun release-stub (func)
  "Release stub for FUNC."
  (condition-case nil
      (progn
        (ad-remove-advice func 'around 'stub)
        (ad-update func))
    (error (release-mock func)))
  (setq stubs (delq func stubs))
  func)

(defun release-stubs (&rest funcs)
  "Release stubs for FUNCS.
Release stubs in `stubs' if unspecified."
  (setq funcs (or funcs stubs))
  (mapc 'release-stub funcs))

(defmacro with-stubs (&rest body)
  "Run BODY, releasing stubs afterwards.
Don't use this directly; see `with-mocks-and-stubs' instead."
  (declare (indent 0)
           (debug t))
  `(let ((stubbed-already stubs-global)
         stubs)
     (unwind-protect
         (progn ,@body)
       (release-stubs)
       (unless stubbed-already
         (setq stubs-global nil)))))

(defmacro without-stubs (&rest body)
  "Run BODY without stubs."
  (declare (indent 0)
           (debug t))
  `(let (stubs-global)        ; stubs are contingent on `stubs-global'
     ,@body))

;; mocks are temporary function rebindings
(defmacro mock (func args &rest body)
  "Mock FUNC.
A mock is a temporary function redefinition, released with
`release-mocks'. You should mock inside `deftest' only,
which releases automatically."
  (declare (indent defun)
           (debug t))
  `(mock-fset ',func (lambda ,args ,@(or body '(nil)))))

(defun mock-fset (symbol definition)
  "Mock SYMBOL with DEFINITION.
Don't use this function directly; see `mock' instead."
  (let ((olddef (when (fboundp symbol)
                  (symbol-function symbol)))
        (oldmsg (current-message))
        message-log-max)
    (add-to-list 'mocks-alist (cons symbol olddef))
    (unless (assq symbol mocks-global-alist)
      (add-to-list 'mocks-global-alist (cons symbol olddef)))
    (if definition
        (ad-safe-fset symbol definition)
      (fmakunbound symbol))
    (unless (eq symbol 'message)
      (if oldmsg (message "%s" oldmsg)
        (message nil)))
    symbol))

(defun release-mock (func)
  "Release mock for FUNC."
  (let ((oldmsg (current-message))
        message-log-max orig def)
    (when (setq orig (assq func mocks-alist))
      (setq def (cdr orig))
      (if def
          (ad-safe-fset func def)
        (fmakunbound func))
      (setq mocks-alist (assq-delete-all func mocks-alist))
      (if oldmsg (message "%s" oldmsg)
        (message nil)))
    func))

(defun release-mocks (&rest funcs)
  "Release mocks for FUNCS.
Release mocks in `mocks-alist' if unspecified."
  (setq funcs (or funcs (mapcar 'car mocks-alist)))
  (mapc 'release-mock funcs))

(defmacro with-mocks (&rest body)
  "Run BODY, releasing mocks afterwards.
Don't use this directly; see `with-mocks-and-stubs' instead."
  (declare (indent 0)
           (debug t))
  `(let ((mocked-already mocks-global-alist)
         mocks-alist)
     (unwind-protect
         (progn ,@body)
       (release-mocks)
       (unless mocked-already
         (setq mocks-global-alist nil)))))

(defmacro without-mocks (&rest body)
  "Run BODY without mocks."
  (declare (indent 0)
           (debug t))
  `(with-mocks
     (dolist (func mocks-global-alist)
       (mock-fset (car func) (cdr func)))
     ,@body))

(defmacro with-mocks-and-stubs (&rest body)
  "Run BODY, releasing mocks and stubs afterwards."
  (declare (indent 0)
           (debug t))
  `(with-mocks
     (with-stubs
       ,@body)))

;; This might seem paranoid, but could be useful for "jesting"
;; (where you replace `<' with `>=', `=' with `/=', etc.,
;; run the tests, and weep in despair as they still pass).
;; See http://jester.sourceforge.net/ for details.
(defmacro without-mocks-and-stubs (&rest body)
  "Run BODY without mocks and stubs."
  (declare (indent 0)
           (debug t))
  `(without-mocks
     (without-stubs
       ,@body)))

(defalias 'with-stubs-and-mocks 'with-mocks-and-stubs)
(defalias 'without-stubs-and-mocks 'without-mocks-and-stubs)

;;; Highlighting

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(deftest\\|defsuite\\|defassert\\)\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))
     ("(\\(\\(?:assert\\|should\\)\\(-[^ ]+\\)*\\|stub\\|mock\\)\\>" 1 font-lock-warning-face)
     ("(\\(with\\(out\\)?-\\(fixtures\\|stubs\\|mocks\\|mocks-and-stubs\
\\|stubs-and-mocks\\)\\)\\>"
      1 font-lock-keyword-face))))

;;; Worklog

;; 2011-02-19: Code cleanup and BDD aliases
;;
;;      Tests and test suites now return t if they pass and nil if
;;      they fail. Everything pertaining to the failure report is
;;      handled by global variables and by signaling an `error'.
;;      The error is caught by the calling suite, which formats it
;;      and presents it as a warning.
;;
;;      Thus, whether a test crashes (because of code changes) or just
;;      fails (because of a wrong assertion), it is handled in the
;;      same way. The calling suite wraps each test in a protective
;;      `condition-case' form, so that code execution is never halted
;;      and each test in the suite is given a go. (To halt execution,
;;      one has to explicitly ask for it with the :debug keyword.)
;;
;;      The upshot of these "nonlocal exits" is nesting of tests.
;;      For example, one might write a test for verifying that some
;;      behavior is properly initialized, and another test for
;;      checking that said behavior is initialized under the proper
;;      circumstances. It makes sense for the second test to call the
;;      first. Since any failure is nonlocal, and since the return
;;      value is simply t if a test passes, test reuse is trivial.
;;
;;      While on the subject of making things trivial, "abbreviated"
;;      test definitions have been removed. Previously, one could skip
;;      the word `deftest' inside suite definitions, yielding the more
;;      terse form shown to the right:
;;
;;          (defsuite suite-name    (defsuite suite-name
;;            (deftest test-name      (test-name
;;              (assert-=               (assert-=
;;                (+ 2 2) 4)))            (+ 2 2) 4)))
;;
;;      Although it looks nice, it has the unfortunate effect that one
;;      has to reevaluate the whole suite whenever one changes a
;;      single test. The `deftest' form to the left, by contrast, is
;;      a macro call by itself and can be evaluated as such.
;;
;;      This extends to debugging as well. With the abbreviated style,
;;      one has to instrument the whole suite at once, rather than
;;      just the single test one is interested in. Edebug is actually
;;      quite good at stepping through custom macros like `defsuite'
;;      and `deftest' (provided one tells it how with an appropriate
;;      `declare' statement in the macro definition). Add too much
;;      special syntax, however, and it is bound to fail. Hence,
;;      these macros should be made as simple as possible.
;;
;;      The assert { 2.0 }-like inspection of arguments is now
;;      implemented recursively. Furthermore, all assertions have
;;      Behavior Driven Development (BDD) aliases. An interesting
;;      article on BDD is at http://technomancy.us/73.
;;
;; 2010-09-14: Add `silent-tests' and `logged-tests' variables
;;
;;      By default, tests and suites log their results in the
;;      *Messages* buffer. If `logged-tests' is nil, results are not
;;      logged; if `silent-tests' is t, results are not even echoed.
;;
;;      Exception: if `silent-tests' is t and a test or suite is
;;      called interactively, the result is echoed, but not logged.
;;
;; 2010-09-13: Add DEBUG parameter to tests and suites
;;
;;      This provides verbose output useful on a per-test basis.
;;      Normally, tests are run in "batch mode", where the test code
;;      is wrapped in a protective `condition-case' form. This is
;;      usually what we want, since it doesn't halt code execution
;;      when tests are loaded in .emacs. Thus, if something fails, we
;;      will get a warning, but Emacs will still load to the best of
;;      its ability.
;;
;;      Otherwise, in "debug mode", the debugger is entered on error
;;      and execution is halted. This mode is enabled by running a
;;      test or suite interactively, or by passing t to the DEBUG
;;      parameter, e.g., (test t). The "debug mode" sets
;;      `debug-on-error' to t.
;;
;;      Incidentally, the DEBUG parameter is also used for selecting
;;      among the different "clauses" of a test function. In the
;;      "internal" cases, DEBUG is either `batch' or `debug', and the
;;      test silently returns t if it passes or returns an error
;;      message string if it fails (or it enters the debugger). When
;;      tests are run as part of a suite, they are run in this way.
;;      The suite collects all error messages and presents them to the
;;      user.
;;
;;      In the "user" cases, DEBUG is either nil (the default) or t,
;;      and the test looks for a suite to present it. The test calls a
;;      suite which in turn runs the test "internally". In sketch:
;;
;;      +--------+    +-------------------+    +---------------+
;;      | (test) | => | (suite ... 'test) | => | (test 'batch) |
;;      +--------+    +-------------------+    +---------------+
;;
;;      If DEBUG is nil (the default when called programmatically),
;;      the test is ultimately called in "batch mode". If DEBUG is t
;;      (the default when called interactively), the test is
;;      ultimately called in "debug mode". A :debug keyword argument
;;      may be specified to let a test be called in "debug mode" by
;;      default, but this should be used carefully.
;;
;; 2010-09-03: Fix long-standing compilation issues
;;
;;      To let `defsuite' be wrapped around `deftest', there were two
;;      strategies for making the two "communicate": either at compile
;;      time or at run time. The former was tried first, with partial
;;      results: tests would work uncompiled and the file would
;;      compile without warnings -- but the tests wouldn't work
;;      compiled.
;;
;;      Now, we make the communication happen at run time instead.
;;      The macro expansion of `defsuite' is a `let' form which the
;;      expansion of `deftest', running inside the form, can pick up
;;      on. The tests now compile properly, with (ahem) considerable
;;      speed gains.
;;
;;      We also try to disentangle the confusing relationship between
;;      suites and tests. Since the current suite is always included
;;      in the failure report, suites, not tests, are charged with the
;;      task of reporting failures. A test function consists of two
;;      clauses: either the test will call a suite to run itself (and
;;      report any failure), or it will execute its test code and
;;      return the result. If the test succeeds, the return value is
;;      t; otherwise it is an error message string, which the calling
;;      suite collects and reports.
;;
;;      Any suite can be called to run any test. If a test is not
;;      associated with a suite, it may default to `empty-suite'.

(provide 'test-framework)

;;; test-framework.el ends here
