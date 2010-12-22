;; vimpulse-test.el --- unit tests for Vimpulse -*- coding: utf-8 -*-

;; This file is for developers. It runs a couple of unit tests on
;; Vimpulse. To load it, add this line to .emacs:
;;
;;     (require 'vimpulse-test)
;;
;; This file is NOT part of Vimpulse proper.

(require 'test-framework)

(defun vimpulse-test-buffer (body)
  "Execute BODY in a temporary buffer.
The buffer contains the familiar *scratch* message,
with point at position 1 and in vi (command) state."
  (let ((kill-ring kill-ring)
        (kill-ring-yank-pointer kill-ring-yank-pointer)
        x-select-enable-clipboard
        message-log-max)
    (with-temp-buffer
      (save-window-excursion
        (switch-to-buffer-other-window (current-buffer))
        (buffer-enable-undo)
        (save-excursion
          (insert ";; This buffer is for notes you don't want to save, \
and for Lisp evaluation.\n;; If you want to create a file, visit \
that file with C-x C-f,\n;; then enter the text in that file's own \
buffer.\n"))
        (viper-mode)
        (funcall body)))))

(defsuite test-utils-suite
  "Test suite for vimpulse-utils.el.
This line is not included in the report."
  :run t
  :setup ((require 'vimpulse))
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
  (test-filter-list
   "Test `vimpulse-filter-list'."
   (let* ((foo '(a nil b nil c))
          (bar (cdr foo))
          (baz (cdr bar)))
     (assert-equal
       "Filter whole list."
       (vimpulse-filter-list foo 'null) '(a b c))
     (assert-equal
       "Filter first element."
       (vimpulse-filter-list foo 'null bar) '(a nil b nil c))
     (assert-equal
       "Filter first and second element."
       (vimpulse-filter-list foo 'null baz) '(a b nil c)))))

;; These tests are largely interactive (and heavy), so don't run them
;; automatically; add (test-interactive-suite) to .emacs and/or run
;; `M-x test-interactive-suite' manually.
(defsuite test-interactive-suite
  "Interactive test suite for Vimpulse."
  :setup ((require 'vimpulse))
  :fixture vimpulse-test-buffer
  (test-visual-delete-word
   "Visually delete a word."
   (execute-kbd-macro "wved")
   (assert-string=
     (buffer-substring 1 47)
     ";;  buffer is for notes you don't want to save"))

  (test-visual-delete-line
   "Visually delete a line."
   (execute-kbd-macro "Vd")
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
then enter the text in that file's own buffer.\n"))

  (test-visual-delete-block-with-counts
   "Visually delete `;; ' prefix, using counts."
   (execute-kbd-macro "\C-v2l2jd")
   (assert-string=
     (buffer-string)
     "This buffer is for notes you don't want to save, and for \
Lisp evaluation.
If you want to create a file, visit that file with C-x C-f,
then enter the text in that file's own buffer.\n"))

  (test-end-of-Word
   "Test E (`vimpulse-end-of-Word')."
   (execute-kbd-macro "wr+d0yEP")
   (assert-string=
     "Yank and put +his before point."
     (buffer-substring 1 52)
     "+his+his buffer is for notes you don't want to save")
   (execute-kbd-macro "EEEwcwfoo")
   (assert-string=
     "Move to \"for\" and change to \"foo\"."
     (buffer-substring 1 52)
     "+his+his buffer is foo notes you don't want to save")
   (execute-kbd-macro "$Ewcw`foo'")
   (assert-string=
     "Move to next line and change \"If\" to \"`foo'\"."
     (buffer-substring 79 113)
     ";; `foo' you want to create a file")
   (execute-kbd-macro "By2EP")
   (assert-string=
     "Yank two words and put before point."
     (buffer-substring 79 122)
     ";; `foo' you`foo' you want to create a file")
   (execute-kbd-macro "EEEEEwcw(bar)")
   (assert-string=
     "Change a single-letter word."
     (buffer-substring 97 126)
     "you want to create (bar) file")
   (execute-kbd-macro "bldiW")
   (assert-string=
     "Delete inner Word."
     (buffer-substring 97 121)
     "you want to create  file"))

  (test-visual-replace
   "Replace with Visual selection."
   (execute-kbd-macro "wvjra")
   (assert-string=
     "Visually replace parts of two lines."
     (buffer-string)
     ";; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\
aaaaaaaaaaaaaaaaaaaaa
aaaaf you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.\n")
   (execute-kbd-macro "jjvral.l.")
   (assert-string=
     "Replace a character and repeat for subsequent characters."
     (buffer-substring 141 191)
     ";; aaan enter the text in that file's own buffer.\n")
   (execute-kbd-macro "$ra")
   (assert-string=
     "Replace at end of line."
     (buffer-substring 141 191)
     ";; aaan enter the text in that file's own buffera\n"))

  (test-change-undo
   "Change a word and undo."
   (let ((vimpulse-want-change-undo t))
     (execute-kbd-macro "wcwfoou")
     (assert-string=
       (buffer-substring 1 51)
       ";; This buffer is for notes you don't want to save"))))

(provide 'vimpulse-test)

;;; vimpulse-test.el ends here
