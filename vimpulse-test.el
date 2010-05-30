;; vimpulse-test.el --- unit tests for Vimpulse -*- coding: utf-8 -*-

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Created: May 2010
;; Keywords: unit testing

;; This file is NOT part of vimpulse-big.el.

;;; Commentary:

;; This file is for developers only. It runs a couple of unit tests on
;; Vimpulse (BOTTOM OF FILE). To load it, add this line to .emacs:
;;
;;     (require 'vimpulse-test)
;;
;; Yeah, I am rolling my own framework. The pre-existing frameworks
;; all had their good points, but none offered every feature I needed.
;; Plus it's more fun this way. :)

;;; Tests

;; A simple test may look like this:
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
;; Alternatively, specify :run t when defining the test:
;;
;;     (deftest test-foo
;;       :run t
;;       (assert (= (+ 2 2) 4))
;;       (assert (= (* 3 3) 9))
;;       (assert (= (% 4 2) 0)))
;;
;; Let's simplify it a bit. Assertions of the same type can grouped
;; into a single statement:
;;
;;     (deftest test-foo
;;       :run t
;;       (assert
;;         (= (+ 2 2) 4)
;;         (= (* 3 3) 9)
;;         (= (% 4 2) 0)))
;;
;; This is Lisp, after all! Furthermore, `assert-=' removes the
;; `=' noise (and makes the failure report more specific):
;;
;;     (deftest test-foo
;;       :run t
;;       (assert-=
;;         (+ 2 2) 4
;;         (* 3 3) 9
;;         (% 4 2) 0))
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
;; If the test fails, these strings show up in the report.
;;
;; NOTE: xUnit frameworks often use "Yoda order", e.g.,
;; "assertEquals(4, 2 + 2);", where the expected value comes first.
;; Here, however, the expected value follows the actual. (Although
;; `=' is symmetrical, the failure reports depend on the order.)
;;
;; NOTE 2: `assert' only accepts multiple arguments inside `deftest'.
;; Outside `deftest' it's a different macro (defined by cl.el).
;;
;; NOTE 3: it's a good idea to name tests with a prefix, e.g.,
;; "test-". This avoids accidently overwriting other functions.

;;; Test suites

;; Tests can be grouped into suites with `defsuite'. The most
;; straightforward way is to just wrap it around them:
;;
;;     (defsuite test-foo-suite
;;       "Example suite."
;;       :run t
;;       (deftest test-foo
;;         "Example test."
;;         (assert-=
;;           "Elementary truth."
;;           (+ 2 2) 4))
;;       (deftest test-bar
;;         "Another example test."
;;         (assert-=
;;           "More elementary truth."
;;           (* 3 3) 9)))
;;
;; Like tests, the suite is executed with (test-foo-suite),
;; `M-x test-foo-suite' or :run t in the definition. Suites can also
;; have docstrings. For brevity, "deftest" can be omitted:
;;
;;     (defsuite test-foo-suite
;;       "Example suite."
;;       :run t
;;       (test-foo
;;        "Example test."
;;        (assert-=
;;          "Elementary truth."
;;          (+ 2 2) 4))
;;       (test-bar
;;        "Another example test."
;;        (assert-=
;;          "More elementary truth."
;;          (* 3 3) 9)))
;;
;; One can go even further and remove the test names themselves:
;;
;;     (defsuite test-foo-suite
;;       "Example suite."
;;       :run t
;;       ("Example test."
;;        (assert-=
;;          "Elementary truth."
;;          (+ 2 2) 4))
;;       ("Another example test."
;;        (assert-=
;;          "More elementary truth."
;;          (* 3 3) 9)))
;;
;; This might be suitable for small suites, but in general it's better
;; to give each test a name by which they can be called afterwards.
;;
;; A suite can include other suites simply by listing the suite names
;; in its `defsuite' form. Furthermore, `defsuite' forms may be
;; nested. One can also define the test suite first and then add tests
;; and suites to it, using the :suite keyword or `add-to-suite':
;;
;;     (defsuite test-foo-suite
;;       "Example suite.")
;;
;;     (deftest test-foo
;;       "Example test."
;;       :suite test-foo-suite
;;       (assert-=
;;         "Elementary truth."
;;         (+ 2 2) 4))
;;
;;     (add-to-suite 'test-foo-suite 'test-bar)
;;
;; NOTE: self-referencing suite definitions should be avoided,
;; although some safeguards exist to prohibit infinite loops.

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
;;       (test-foo
;;        ...)
;;       (test-bar
;;        ...))
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
;;       (test-foo
;;        ...)
;;       (test-bar
;;        ...))
;;
;; There's also the :wrap keyword argument, which specifies an
;; around-advice for the whole test, e.g., :wrap ((wobble) ad-do-it).
;; (See `defadvice' for more details.) The arguments are executed in
;; the following order:
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
;; Any single test may also specify these arguments. In that case, the
;; suite fixtures are wrapped around the test fixtures.
;;
;; NOTE: if defining a function to use as a fixture, make sure it's
;; defined before the tests are run (before the test if using :run t).
;;
;; NOTE 2: a test defined as part of a suite carries with it the
;; suite's fixtures even when called outside the suite. When the test
;; is called by a different suite, that suite's fixtures overwrites
;; the fixtures inherited from the original suite.

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

(eval-when-compile (require 'cl))

(defvar current-suites nil)
(defvar test-passed nil)
(defvar suite-passed nil)

;;; Test suite macro: `defsuite'

(defmacro defsuite (suite &rest body)
  "Define a test suite."
  (declare (indent defun))
  (let ((parents (and (boundp 'current-suite)
                      current-suite
                      (list current-suite)))
        doc form keyword result run
        suite-fixture suite-setup suite-teardown suite-wrap tests)
    ;; Collect docstring
    (when (stringp (car body))
      (setq doc (pop body)))
    ;; Collect keywords
    (while (keywordp (car body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :run)
        (setq run (pop body)))
       ((memq keyword '(:parent :suite))
        (if (listp (car body))
            (setq parents (append parents (pop body)))
          (add-to-list 'parents (pop body))))
       ((eq keyword :fixture)
        (setq suite-fixture (pop body)))
       ((eq keyword :setup)
        (setq suite-setup (pop body)))
       ((eq keyword :teardown)
        (setq teardown (pop body)))
       ((memq keyword '(:advice :wrap))
        (setq suite-wrap (pop body)))
       (t
        (pop body))))
    ;; Collect "abbreviated" forms -- that is, test definitions
    ;; lacking the `deftest' symbol and/or a test name
    (while (setq form (pop body))
      (cond
       ((symbolp form)
        (add-to-list 'tests form))
       ((not (cdr form))
        (add-to-list 'tests (car form)))
       ((and (symbolp (car form)) (fboundp (car form)))
        (add-to-list 'result form))
       (t
        (add-to-list 'result (append '(deftest) form)))))
    `(progn
       ;; Define suite variable
       (defvar ,suite ',tests ,doc)
       ;; Define suite function
       (defun ,suite ()
         ,doc
         (interactive)
         (let ((current-suites
                (and (boundp 'current-suites)
                     current-suites))
               (current-suite ',suite)
               (suite-passed t)
               stubs mocks-alist)
           (add-to-list 'current-suites ',suite)
           (with-mocks-and-stubs
             (dolist (func ,suite)
               (unless (memq func current-suites)
                 (funcall func ',suite-fixture
                          ',suite-setup ',suite-teardown))))
           (and suite-passed
                (called-interactively-p)
                (message "Test suite passed!"))))
       ;; :wrap function?
       ,@(when suite-wrap
           `((defadvice ,suite (around wrap activate)
               ,@(if (listp suite-wrap)
                     suite-wrap
                   `((,suite-wrap ad-do-it))))))
       ;; Add this suite to other suites?
       ,@(when parents
           `((mapc (lambda (suite)
                     (add-to-suite ',suite suite))
                   ',parents)))
       ;; The rest is just a `let' environment with `current-suite'
       ;; bound to the suite. `deftest' calls can pick up on this to
       ;; assign themselves to the containing suite.
       (let ((current-suite ',suite)
             (suite-fixture ',suite-fixture)
             (suite-setup ',suite-setup)
             (suite-teardown ',suite-teardown))
         ,@result)
       ;; :run suite
       ,@(when run
           `((,suite))))))

(defun add-to-suite (suite test)
  "Add TEST to SUITE."
  (eval `(defvar ,suite nil))
  ;; Suites are basically hooks
  (add-hook suite test))

(defun run-test (test &optional fixture setup teardown)
  "Run TEST with fixtures.
FIXTURE is a one-argument function with which to run TEST;
SETUP and TEARDOWN are zero-argument functions to run before and
after. Mocks and stubs are guaranteed to be released when the
test is done."
  (with-mocks-and-stubs
    (unwind-protect
        (save-excursion
          (when setup
            (funcall (if (functionp setup)
                         setup
                       `(lambda () ,@setup))))
          (if fixture
              (funcall (if (functionp fixture)
                           fixture
                         `(lambda () ,@fixture))
                       test)
            (funcall test)))
      (when teardown
        (funcall (if (functionp teardown)
                     teardown
                   `(lambda () ,@teardown)))))))

;;; Test macro: `deftest'

(defmacro deftest (test &rest body)
  "Define a test."
  (declare (indent defun))
  ;; A wee bit of duplication, here. Suites and tests have the same
  ;; keyword arguments, though the internals differ.
  (let ((suites (and (boundp 'current-suite)
                     current-suite
                     (list current-suite)))
        (suite-fixture (when (boundp 'suite-fixture)
                         suite-fixture))
        (suite-setup (when (boundp 'suite-setup)
                       suite-setup))
        (suite-teardown (when (boundp 'suite-teardown)
                          suite-teardown))
        doc keyword run fixture setup teardown wrap)
    ;; If TEST is not a name, move it into BODY
    ;; (a nil name creates an anonymous function).
    (unless (symbolp test)
      (setq body (append (list test) body)
            test nil))
    ;; Collect parent suite
    (when (symbolp (car body))
      (add-to-list 'suites (pop body)))
    ;; Collect docstring
    (when (stringp (car body))
      (setq doc (pop body)))
    ;; Collect keywords
    (while (keywordp (car body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :run)
        (setq run (pop body)))
       ((eq keyword :suite)
        (if (listp (car body))
            (setq suites (append suites (pop body)))
          (add-to-list 'suites (pop body))))
       ((eq keyword :fixture)
        (setq fixture (pop body)))
       ((eq keyword :setup)
        (setq setup (pop body)))
       ((eq keyword :teardown)
        (setq teardown (pop body)))
       ((memq keyword '(:advice :wrap))
        (setq wrap (pop body)))
       (t
        (pop body))))
    ;; Create function body
    (setq lambda
          `(lambda (&optional fixture setup teardown)
             ,@(when doc `(,doc))
             (interactive)
             (with-mocks-and-stubs
               (let* ((current-suite
                       (and (boundp 'current-suite)
                            current-suite))
                      test-passed)
                 ;; Run suite fixtures (defaulting to the fixtures of
                 ;; the parent suite, if any)
                 (run-test
                  (lambda ()
                    ;; Run test fixtures (hard-coded into the test
                    ;; function)
                    (run-test (lambda ()
                                (condition-case err
                                    (progn
                                      ,@body
                                      (setq test-passed t))
                                  (error (progn
                                           (setq suite-passed nil)
                                           (test-message
                                            ',test
                                            current-suite
                                            (error-message-string err))))))
                              ',fixture
                              ',setup
                              ',teardown))
                  (or fixture ',suite-fixture)
                  (or setup ',suite-setup)
                  (or teardown ',suite-teardown))
                 (when (and test-passed (called-interactively-p))
                   (message "Test passed!"))))))
    (if test
        `(macrolet ,deftest-macros
           ,@(when suites
               `((mapc (lambda (suite)
                         (add-to-suite suite ',test))
                       ',suites)))
           (defun ,test ,@(cdr lambda))
           ,@(when wrap
               `((defadvice ,test (around wrap activate)
                   ,@(if (listp wrap)
                         wrap
                       (list wrap)))))
           ,@(when run
               `((,test))))
      ;; If nil is passed for TEST, define an anonymous function.
      ;; This is useful for "abbreviated" suite format.
      `(macrolet ,deftest-macros
         ,@(when suites
             `((mapc (lambda (suite)
                       (add-to-suite suite ,lambda))
                     ',suites)))
         ,@(when run
             `((funcall ,lambda)))))))

;; Currently, this produces a warning. Could it produce linkified
;; text in a separate buffer instead? (`with-output-to-temp-buffer')
(defun test-message (test suite &rest strings)
  "Display warning for TEST in SUITE, consisting of STRINGS.
STRINGS are separated by a newline and a tab."
  (let (doc message)
    (if (and (functionp test)
             (stringp (setq doc (documentation test))))
        (add-to-list 'strings
                     (concat (if test (format "(%s) " test) "")
                             (substring doc 0
                                        (string-match "\n" doc))
                             "\n"))
      (when test
        (add-to-list 'strings (format "In `%s':\n" test))))
    (when (stringp (setq doc (get suite 'variable-documentation)))
      (add-to-list 'strings
                   (substring doc 0 (string-match "\n" doc))))
    (dolist (string strings)
      (when (and (stringp string)
                 (not (string= string "")))
        (if message
            (setq message (concat message "\n\t" string))
          (setq message string))))
    (display-warning (or suite 'test) message)))

;;; Assertion macro: `defassert'

;; This is hairy. What it does is to implement different ways of
;; calling an assertion macro:
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
;; Assertions must be macros, not functions, to use the unevaluated
;; expressions in the failure report.
(defmacro defassert (name args &rest body)
  "Define an assertion macro.

    (defassert assert-macro (doc arg1 arg2)
      ;; macro expansion
      )

The first argument in the argument list is the docstring.
The rest of the arguments are assumed to be repeatable, that is,
\(assert-macro \"test\" x1 y1 x2 y2 ...), unless a &rest argument
is specified."
  (declare (indent defun))
  (let* (body-var doc-var docstring keyword result shadow)
    (when (stringp (car body))
      (setq docstring (list (pop body))))
    (while (keywordp (car body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :shadow)
        (setq shadow (pop body)))
       (t
        (pop body))))
    (if (memq '&rest args)
        ;; If &rest is specified, just make a `let' wrapper for the
        ;; documentation argument
        (setq body-var (car (last args))
              args (nbutlast args 2)
              doc-var (or (pop args) 'doc)
              result
              ;; This is an expression for calculating the macro
              ;; definition, which in turn is evaluated to get the
              ;; macro expansion
              `(let ((,doc-var
                      (when (and (> (length ,body-var) 1)
                                 (stringp (car ,body-var)))
                        (pop ,body-var))))
                 ,@body))
      ;; Repeatable argument list: iterate through the arguments
      (setq body-var 'body
            doc-var (or (when (> (length args) 1)
                          (pop args)) 'doc)
            result
            ;; Like above, code for making code for making the
            ;; expansion
            `(let ((result '(progn)) ,doc-var)
               (if (and (> (length ,body-var) 0)
                        ,@(when (> (length args) 1)
                            `((/= (mod (length ,body-var)
                                       ,(length args))
                                  0)))
                        (stringp ,(car args)))
                   ;; Grab first argument as docstring if appropriate,
                   ;; then store the remaining in `body-var' for
                   ;; iteration
                   (setq ,doc-var ,(car args)
                         ,@(when (cdr args)
                             `(,body-var (append (list ,@(cdr args))
                                                 ,body-var))))
                 (setq ,body-var (append (list ,@args) ,body-var)))
               ;; Go through `body-var' and bind the arguments to
               ;; successive values, evaluating the assertion's
               ;; definition each time. The concatenation of these
               ;; evaluations is the macro's expansion (stored in
               ;; `result').
               (while ,body-var
                 ,@(mapcar (lambda (var)
                             `(setq ,var (pop ,body-var))) args)
                 (add-to-list 'result (progn ,@body) t))
               result)))
    `(progn
       ;; Define assertion macro unless :shadow is t
       ,@(unless shadow
           `((defmacro ,name (,@args &rest ,body-var)
               ,@docstring
               (declare (indent defun))
               ,result)))
       ;; Add to `deftest-macros' for good measure
       (add-to-list 'deftest-macros
                    '(,name (,@args &rest ,body-var) ,result)))))

;; Since `assert' is a standard macro defined by cl.el, don't redefine
;; it; just shadow it with `macrolet', using the :shadow keyword
(defvar deftest-macros nil
  "Macros that shadow global definitions inside `deftest'.")

(defvar assert-macros
  '((eq (obj1 obj2)
        `(progn (assert-eq doc ,obj1 ,obj2) t)))
  "Macros that shadow global definitions inside `assert'.")

;; assert { 2.0 }-like display of arguments and their evaluated
;; values. Is this sufficient, or should I use `macrolet' to remap
;; (assert (eq foo bar)) to (assert-eq foo bar)?
(defun assert-expand (form &optional prefix)
  "Return evalutions for arguments in function call FORM."
  (setq prefix (or prefix ""))
  (apply 'concat
         (mapcar
          (lambda (exp)
            (format "\n%s%s => %s"
                    prefix exp (eval exp)))
          (and (listp form)
               (and (functionp (car form)))
               (cdr form)))))

(defassert assert (doc form)
  "Verify that FORM returns non-nil."
  :shadow t
  `(let ((form ,form))
     (without-mocks-and-stubs
       (unless form
         (error "%sassert for %s failed:\n\texpected non-nil, was %s%s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',form form
                ,(assert-expand form "\t"))))))

(defassert assert-not (doc form)
  "Verify that FORM returns nil."
  :shadow t
  `(let ((form ,form))
     (without-mocks-and-stubs
       (when form
         (error "%sassert-not for %s failed:\n\texpected non-nil, was %s%s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',form form
                ,(assert-expand form "\t"))))))

(put 'assert 'lisp-indent-function 'defun)
(put 'assert-not 'lisp-indent-function 'defun)

;; `assert'-derivatives, on the other hand, can be defined
;; straightforwardly
(defassert assert-that (doc form)
  "Verify that FORM returns non-nil."
  `(let ((form ,form))
     (without-mocks-and-stubs
       (unless form
         (error "%sassert-that for %s failed:\n\texpected non-nil, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',form form)))))

(defassert assert-nil (doc form)
  "Verify that FORM returns nil."
  `(let ((form ,form))
     (without-mocks-and-stubs
       (when form
         (error "%sassert-nil for %s failed:\n\texpected nil, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',form form)))))

;; xUnit discrepancy: xUnit favors Yoda conditions (expected actual).
;; I don't (actual expected).
(defassert assert-equal (doc actual expected)
  "Verify that ACTUAL is `equal' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (equal actual expected)
         (error "%sassert-equal for %s failed:\n\texpected %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-not-equal (doc actual expected)
  "Verify that ACTUAL is not `equal' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (when (equal actual expected)
         (error "%sassert-not-equal for %s failed:\n\texpected not %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-eq (doc actual expected)
  "Verify that ACTUAL is `eq' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (eq actual expected)
         (error "%sassert-eq for %s failed:\n\texpected %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-not-eq (doc actual expected)
  "Verify that ACTUAL is not `eq' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (when (eq actual expected)
         (error "%sassert-not-eq for %s failed:\n\texpected not %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-eql (doc actual expected)
  "Verify that ACTUAL is `eql' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (eql actual expected)
         (error "%sassert-eql for %s failed:\n\texpected %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-not-eql (doc actual expected)
  "Verify that ACTUAL is not `eql' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (when (eql actual expected)
         (error "%sassert-not-eql for %s failed:\n\texpected not %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-= (doc actual expected)
  "Verify that ACTUAL is `=' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (= actual expected)
         (error "%sassert-= for %s failed:\n\texpected %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-/= (doc actual expected)
  "Verify that ACTUAL is `/=' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (/= actual expected)
         (error "%sassert-/= for %s failed:\n\texpected not %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-string= (doc actual expected)
  "Verify that ACTUAL is `string=' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (string= actual expected)
         (error "%sassert-string= for %s failed:\n\texpected %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-not-string= (doc actual expected)
  "Verify that ACTUAL is not `string=' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (when (string= actual expected)
         (error "%sassert-not-string= for %s failed:\n\texpected %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-member (doc elt list)
  "Verify that ELT is `member' of LIST."
  `(let ((elt ,elt)
         (list ,list))
     (without-mocks-and-stubs
       (unless (member elt list)
         (error "%sassert-member failed:\n\texpected %s in %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                elt list)))))

(defassert assert-memq (doc elt list)
  "Verify that ELT is `memq' of LIST."
  `(let ((elt ,elt)
         (list ,list))
     (without-mocks-and-stubs
       (unless (memq elt list)
         (error "%sassert-memq failed:\n\texpected %s in %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                elt list)))))

(defassert assert-match (doc regexp string)
  "Verify that REGEXP matches STRING."
  `(let ((regexp ,regexp)
         (string ,string))
     (without-mocks-and-stubs
       (unless (string-match regexp string)
         (error "%sassert-match failed:\n\texpected \"%s\" to match \"%s\"\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                regexp string)))))

(defassert assert-error (doc &rest body)
  "Verify that BODY signals an error."
  `(let (failed)
     (condition-case nil
         (progn
           ,@body
           (setq failed t))
       (error nil))
     (without-mocks-and-stubs
       (when failed
         (error "%sassert-error failed: %s expected to signal an error\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',body)))))

(defassert assert-changed (doc form &rest body)
  "Verify that FORM changes after executing BODY.
FORM must be quoted."
  `(let* ((form (eval ,form)))
     (assert-not-equal
       ,@(when doc `(,doc))
       (progn
         ,@body
         ,(eval form))
       form)))

(defassert assert-not-changed (doc form &rest body)
  "Verify that FORM does not change after executing BODY.
FORM must be quoted."
  `(let* ((form (eval ,form)))
     (assert-equal
       ,@(when doc `(,doc))
       (progn
         ,@body
         ,(eval form))
       form)))

;;; Mocks/stubs

(defvar stubs nil
  "List of stubbed functions.")

(defvar stubs-global nil
  "List of all stubs in all contexts.")

(defvar mocks-alist nil
  "Alist of mocked functions.")

(defvar mocks-global-alist nil
  "Alist of all mocks in all contexts.")

;; Stubs are made with advice, so the number of arguments is unchanged
(defmacro stub (func &rest body)
  "Stub out FUNC.
A stub is a temporary function advice shadowing the real
definition (so the argument list is retained). It is released
with `release-stubs'. You should stub inside `deftest' only,
which releases automatically."
  (declare (indent defun))
  `(cond
    ((fboundp ',func)
     (add-to-list 'stubs ',func)
     (unless (memq ',func stubs-global)
       (add-to-list 'stubs-global ',func))
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
  (setq stubs (delq func stubs)))

(defun release-stubs (&rest funcs)
  "Release stubs for FUNCS.
Release stubs in `stubs' if unspecified."
  (setq funcs (or funcs stubs))
  (mapc 'release-stub funcs))

(defmacro with-stubs (&rest body)
  "Run BODY, releasing stubs afterwards.
Don't use this directly; see `with-mocks-and-stubs' instead."
  (declare (indent 0))
  `(let ((stubbed-already stubs-global)
         stubs)
     (unwind-protect
         (progn ,@body)
       (release-stubs)
       (unless stubbed-already
         (setq stubs-global nil)))))

(defmacro without-stubs (&rest body)
  "Run BODY without stubs."
  (declare (indent 0))
  `(let (stubs-global)        ; stubs are contingent on `stubs-global'
     ,@body))

;; Mocks are temporary function rebindings
(defmacro mock (func args &rest body)
  "Mock FUNC.
A mock is a temporary function redefinition, released with
`release-mocks'. You should mock inside `deftest' only,
which releases automatically."
  (declare (indent defun))
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
      (message oldmsg))))

(defun release-mock (func)
  "Release mock for FUNC."
  (let ((oldmsg (current-message))
        message-log-max
        orig def)
    (when (setq orig (assq func mocks-alist))
      (setq def (cdr orig))
      (if def
          (ad-safe-fset func def)
        (fmakunbound func))
      (setq mocks-alist (assq-delete-all func mocks-alist))
      (message oldmsg))))

(defun release-mocks (&rest funcs)
  "Release mocks for FUNCS.
Release mocks in `mocks-alist' if unspecified."
  (setq funcs (or funcs (mapcar 'car mocks-alist)))
  (mapc 'release-mock funcs))

(defmacro with-mocks (&rest body)
  "Run BODY, releasing mocks afterwards.
Don't use this directly; see `with-mocks-and-stubs' instead."
  (declare (indent 0))
  `(let ((mocked-already mocks-global-alist)
         mocks-alist)
     (unwind-protect
         (progn ,@body)
       (release-mocks)
       (unless mocked-already
         (setq mocks-global-alist nil)))))

(defmacro with-mocks (&rest body)
  "Run BODY without mocks, restoring mocks afterwards."
  (declare (indent 0))
  `(let (mocks-alist)
     (unwind-protect
         (progn ,@body)
       (release-mocks))))

(defmacro without-mocks (&rest body)
  "Run BODY without mocks."
  (declare (indent 0))
  `(with-mocks
     (dolist (func mocks-global-alist)
       (mock-fset (car func) (cdr func)))
     ,@body))

(defmacro with-mocks-and-stubs (&rest body)
  "Run BODY, releasing mocks and stubs afterwards."
  (declare (indent 0))
  `(with-mocks
     (with-stubs
       ,@body)))

(defmacro without-mocks-and-stubs (&rest body)
  "Run BODY without mocks and stubs."
  (declare (indent 0))
  `(without-mocks
     (without-stubs
       ,@body)))

(fset 'with-stubs-and-mocks 'with-mocks-and-stubs)
(fset 'without-stubs-and-mocks 'without-mocks-and-stubs)

;;; Highlighting

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(deftest\\|defsuite\\|defassert\\)\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))
     ("(\\(assert\\(-[^ ]+\\)*\\)\\>" 1 font-lock-warning-face)
     ("(\\(with\\(out\\)?-\\(stubs\\|mocks\\|mocks-and-stubs\\|stubs-and-mocks\\)\\)\\>"
      1 font-lock-keyword-face))))

;;;; Okay, I'm done! Now on to test Vimpulse.

(defun vimpulse-test-buffer (body)
  "Execute BODY in a temporary buffer.
The buffer contains the familiar *scratch* message,
with point at position 1 and in vi (command) state."
  (with-temp-buffer
    (save-window-excursion
      (switch-to-buffer-other-window (current-buffer))
      (save-excursion
        (insert ";; This buffer is for notes you don't want to save, \
and for Lisp evaluation.\n;; If you want to create a file, visit \
that file with C-x C-f,\n;; then enter the text in that file's own \
buffer.\n"))
      (viper-mode)
      (funcall body))))

(defsuite test-utils-suite
  "Test suite for vimpulse-utils.el.
This line is not included in the report."
  :run t
  (test-augment-keymap
   "Test `vimpulse-augment-keymap'."
   (let (augment-alist map)
     (setq map (make-sparse-keymap))
     (define-key map "a" 'foo)
     (define-key map "b" 'bar)
     (define-key map "c" 'baz)
     (setq augment-alist
           '(([?a] . wibble)
             ([?b] . wobble)
             ([?c] . wubble)
             ([?d] . flob)))
     (vimpulse-augment-keymap map augment-alist)
     (assert-eq
       "Augment keymap carefully."
       (lookup-key map "a") 'foo
       (lookup-key map "b") 'bar
       (lookup-key map "c") 'baz
       (lookup-key map "d") 'flob)
     (vimpulse-augment-keymap map augment-alist t)
     (assert-eq
       "Augment keymap forcefully."
       (lookup-key map "a") 'wibble
       (lookup-key map "b") 'wobble
       (lookup-key map "c") 'wubble
       (lookup-key map "d") 'flob)))
  (test-truncate
   "Test `vimpulse-truncate'."
   (assert-equal
     "Positive numbers."
     (vimpulse-truncate [a b c] 0) []
     (vimpulse-truncate [a b c] 1) [a]
     (vimpulse-truncate [a b c] 2) [a b]
     (vimpulse-truncate [a b c] 3) [a b c]
     (vimpulse-truncate [a b c] 4) [a b c])
   (assert-equal
     "Negative numbers."
     (vimpulse-truncate [a b c] -1) [a b]
     (vimpulse-truncate [a b c] -2) [a]
     (vimpulse-truncate [a b c] -3) []
     (vimpulse-truncate [a b c] -4) [])
   (assert-equal
     "Limit cases."
     (vimpulse-truncate [] 0) []
     (vimpulse-truncate [] 3) []))
  (test-memq-recursive
   "Test `vimpulse-memq-recursive'."
   (assert
     "Find `a'."
     (vimpulse-memq-recursive 'a '(a b c))
     (vimpulse-memq-recursive 'a '(a b c))
     (vimpulse-memq-recursive 'a '(b (a) c))
     (vimpulse-memq-recursive 'a '((a) b c a)))
   (assert
     "Find nil."
     (vimpulse-memq-recursive nil '(nil b c a))
     (vimpulse-memq-recursive nil '(b (nil) c a))
     (vimpulse-memq-recursive nil '(t b ((())) c a)))
   (assert-not
     "Nothing in nil!"
     (vimpulse-memq-recursive nil nil))))

;; These tests are largely interactive, so don't run them
;; automatically; add (test-visual-suite) to .emacs and/or
;; run `M-x test-visual-suite' habitually.
(defsuite test-visual-suite
  "Test suite for vimpulse-visual-mode.el."
  :fixture vimpulse-test-buffer
  (test-visual-delete-word
   "Visually delete a word."
   (execute-kbd-macro "wved")
   (assert-string=
     (buffer-substring 1 47)
     ";;  buffer is for notes you don't want to save"))
  (test-visual-delete-line
   "Visually delete a line."
   (execute-kbd-macro "Vdw")
   (assert-string=
     (buffer-string)
     ";; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.\n"))
  (test-visual-delete-block
   "Visually delete `;; ' prefix."
   (execute-kbd-macro "\C-vjjlld")
   (assert-string=
     (buffer-string)
     "This buffer is for notes you don't want to save, and for \
Lisp evaluation.
If you want to create a file, visit that file with C-x C-f,
then enter the text in that file's own buffer.\n")))

(provide 'vimpulse-test)

;;; vimpulse-test.el ends here
