;; vimpulse-tests.el --- unit tests for Vimpulse -*- coding: utf-8 -*-

;; This file is for developers. It runs some unit tests on Vimpulse.
;; To load it, add the following line to .emacs:
;;
;;     (require 'vimpulse-tests)
;;
;; This file is NOT part of Vimpulse itself.

(require 'test-framework)
(require 'vimpulse)

(defvar vimpulse-tests-run t
  "Run Vimpulse tests.")

(defsuite vimpulse-test-states-suite
  :fixture with-temp-buffer
  :suite vimpulse-test-suite

  (defun vimpulse-test-local-mode-enabled ()
    "Verify that `vimpulse-local-mode' is enabled properly"
    (should "Set the mode variable to t"
      (eq vimpulse-local-mode t))
    (should "Refresh `emulation-mode-map-alist'"
      (memq 'vimpulse-mode-map-alist emulation-mode-map-alists))
    (should "Refresh the modeline"
      (memq 'vimpulse-modeline-tag global-mode-string))
    (should "Create a buffer-local value for `vimpulse-mode-map-alist'"
      (assq 'vimpulse-mode-map-alist (buffer-local-variables)))
    (should "Initialize buffer-local keymaps"
      (assq 'vimpulse-vi-state-local-map (buffer-local-variables))
      (keymapp vimpulse-vi-state-local-map)
      (assq 'vimpulse-emacs-state-local-map (buffer-local-variables))
      (keymapp vimpulse-emacs-state-local-map))
    (should "Refresh buffer-local entries in `vimpulse-mode-map-alist'"
      (rassq vimpulse-vi-state-local-map vimpulse-mode-map-alist)
      (rassq vimpulse-emacs-state-local-map vimpulse-mode-map-alist))
    (should-not "Don't add buffer-local entries to the default value"
      (rassq vimpulse-vi-state-local-map
             (default-value 'vimpulse-mode-map-alist))
      (rassq vimpulse-emacs-state-local-map
             (default-value 'vimpulse-mode-map-alist))))

  (defun vimpulse-test-local-mode-disabled ()
    "Verify that `vimpulse-local-mode' is disabled properly"
    (should "Set the mode variable to nil"
      (null vimpulse-local-mode))
    (should "Disable all states"
      (vimpulse-test-no-states))
    (should-fail (vimpulse-test-local-mode-enabled)))

  (defun vimpulse-test-no-states ()
    "Verify that all states are disabled"
    (should "Set `vimpulse-state' to nil"
      (null vimpulse-state))
    (expect "Disable all state keymaps"
      (dolist (state (mapcar 'car vimpulse-states-alist) t)
        (should-not
          (symbol-value (vimpulse-state-property state :mode))
          (memq (symbol-value (vimpulse-state-property state :keymap))
                (current-active-maps))
          (symbol-value (vimpulse-state-property state :local-mode))
          (memq (symbol-value (vimpulse-state-property state :local-keymap))
                (current-active-maps)))
        (dolist (map (vimpulse-state-auxiliary-keymaps state))
          (should-not (memq map (current-active-maps)))))))

  (deftest vimpulse-test-toggle-local-mode
    "Toggle `vimpulse-local-mode'"
    (expect "Enable `vimpulse-local-mode'"
      (vimpulse-local-mode 1)
      (should (vimpulse-test-local-mode-enabled)))
    (expect "Disable `vimpulse-local-mode'"
      (vimpulse-local-mode -1)
      (should (vimpulse-test-local-mode-disabled))))

  (defun vimpulse-test-change-state (state)
    "Change state to STATE and check keymaps"
    (let (mode keymap local-mode local-keymap tag)
      (vimpulse-change-state state)
      (setq mode (vimpulse-state-property state :mode)
            keymap (symbol-value (vimpulse-state-property
                                  state :keymap))
            local-mode (vimpulse-state-property state :local-mode)
            local-keymap (symbol-value (vimpulse-state-property
                                        state :local-keymap))
            tag (symbol-value (vimpulse-state-property
                               state :tag)))
      (should "Update `vimpulse-state'"
        (eq vimpulse-state state))
      (should "Ensure `vimpulse-local-mode' is enabled"
        (vimpulse-test-local-mode-enabled))
      (should "Enable state modes"
        (symbol-value mode)
        (symbol-value local-mode))
      (should "Push state keymaps to the top"
        (equal (nth 0 vimpulse-mode-map-alist)
               (cons local-mode local-keymap))
        (equal (nth 1 vimpulse-mode-map-alist)
               (cons mode keymap)))
      (should "Refresh modeline tag"
        (equal vimpulse-modeline-tag tag))))

  (deftest vimpulse-test-exit-vi-state
    "Enter vi state and then disable all states"
    (vimpulse-test-change-state 'vi)
    (vimpulse-vi-state -1)
    (should (vimpulse-test-no-states)))

  (deftest vimpulse-test-change-states
    "Change between vi state and emacs state"
    (vimpulse-test-change-state 'vi)
    (vimpulse-test-change-state 'emacs)
    (vimpulse-test-change-state 'vi)
    (vimpulse-test-change-state 'emacs))

  (deftest vimpulse-test-enter-vi-state-disabled
    "Enter vi state even if `vimpulse-local-mode' is disabled"
    (vimpulse-local-mode -1)
    (should
      (vimpulse-test-local-mode-disabled)
      (vimpulse-test-change-state 'vi))))

;;; TODO: Revive these tests

;; (defun vimpulse-test-buffer (body)
;;   "Execute BODY in a temporary buffer.
;; The buffer contains the familiar *scratch* message,
;; with point at position 1 and in vi (command) state."
;;   (let ((kill-ring kill-ring)
;;         (kill-ring-yank-pointer kill-ring-yank-pointer)
;;         x-select-enable-clipboard
;;         message-log-max)
;;     (with-temp-buffer
;;       (save-window-excursion
;;         (switch-to-buffer-other-window (current-buffer))
;;         (buffer-enable-undo)
;;         (save-excursion
;;           (insert ";; This buffer is for notes you don't want to save, \
;; and for Lisp evaluation.\n;; If you want to create a file, visit \
;; that file with C-x C-f,\n;; then enter the text in that file's own \
;; buffer.\n"))
;;         (viper-mode)
;;         (funcall body)))))
;;
;; (defsuite test-utils-suite
;;   "Test suite for vimpulse-utils.el.
;; This line is not included in the report."
;;   :run t
;;   :setup (require 'vimpulse)
;;   (deftest test-augment-keymap
;;     "Test `vimpulse-augment-keymap'."
;;     (let (augment-alist map)
;;       (setq map (make-sparse-keymap))
;;       (define-key map "a" 'foo)
;;       (define-key map "b" 'bar)
;;       (define-key map "c" 'baz)
;;       (setq augment-alist
;;             '(([?a] . wibble)
;;               ([?b] . wobble)
;;               ([?c] . wubble)
;;               ([?d] . flob)))
;;       (vimpulse-augment-keymap map augment-alist)
;;       (assert-eq
;;         "Augment keymap carefully."
;;         (lookup-key map "a") 'foo
;;         (lookup-key map "b") 'bar
;;         (lookup-key map "c") 'baz
;;         (lookup-key map "d") 'flob)
;;       (vimpulse-augment-keymap map augment-alist t)
;;       (assert-eq
;;         "Augment keymap forcefully."
;;         (lookup-key map "a") 'wibble
;;         (lookup-key map "b") 'wobble
;;         (lookup-key map "c") 'wubble
;;         (lookup-key map "d") 'flob)))
;;   (deftest test-truncate
;;     "Test `vimpulse-truncate'."
;;     (assert-equal
;;       "Positive numbers."
;;       (vimpulse-truncate [a b c] 0) []
;;       (vimpulse-truncate [a b c] 1) [a]
;;       (vimpulse-truncate [a b c] 2) [a b]
;;       (vimpulse-truncate [a b c] 3) [a b c]
;;       (vimpulse-truncate [a b c] 4) [a b c])
;;     (assert-equal
;;       "Negative numbers."
;;       (vimpulse-truncate [a b c] -1) [a b]
;;       (vimpulse-truncate [a b c] -2) [a]
;;       (vimpulse-truncate [a b c] -3) []
;;       (vimpulse-truncate [a b c] -4) [])
;;     (assert-equal
;;       "Limit cases."
;;       (vimpulse-truncate [] 0) []
;;       (vimpulse-truncate [] 3) []))
;;   (deftest test-filter-list
;;     "Test `vimpulse-filter-list'."
;;     (let* ((foo '(a nil b nil c))
;;            (bar (cdr foo))
;;            (baz (cdr bar)))
;;       (assert-equal
;;         "Filter whole list."
;;         (vimpulse-filter-list foo 'null) '(a b c))
;;       (assert-equal
;;         "Filter first element."
;;         (vimpulse-filter-list foo 'null bar) '(a nil b nil c))
;;       (assert-equal
;;         "Filter first and second element."
;;         (vimpulse-filter-list foo 'null baz) '(a b nil c)))))
;;
;; ;; These tests are largely interactive (and heavy), so don't run them
;; ;; automatically; add (test-interactive-suite) to .emacs and/or run
;; ;; `M-x test-interactive-suite' manually.
;; (defsuite test-interactive-suite
;;   "Interactive test suite for Vimpulse."
;;   :setup (require 'vimpulse)
;;   :fixture vimpulse-test-buffer
;;   (deftest test-visual-delete-word
;;     "Visually delete a word."
;;     (execute-kbd-macro "wved")
;;     (assert-string=
;;       (buffer-substring 1 47)
;;       ";;  buffer is for notes you don't want to save"))
;;
;;   (deftest test-visual-delete-line
;;     "Visually delete a line."
;;     (execute-kbd-macro "Vd")
;;     (assert-string=
;;       (buffer-string)
;;       ";; If you want to create a file, visit that file with C-x C-f,
;; ;; then enter the text in that file's own buffer.\n"))
;;
;;   (deftest test-visual-delete-block
;;     "Visually delete `;; ' prefix."
;;     (execute-kbd-macro "\C-vjjlld")
;;     (assert-string=
;;       (buffer-string)
;;       "This buffer is for notes you don't want to save, and for \
;; Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.\n"))
;;
;;   (deftest test-visual-delete-block-with-counts
;;     "Visually delete `;; ' prefix, using counts."
;;     (execute-kbd-macro "\C-v2l2jd")
;;     (assert-string=
;;       (buffer-string)
;;       "This buffer is for notes you don't want to save, and for \
;; Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.\n"))
;;
;;   (deftest test-end-of-Word
;;     "Test E (`vimpulse-end-of-Word')."
;;     (execute-kbd-macro "wr+d0yEP")
;;     (assert-string=
;;       "Yank and put +his before point."
;;       (buffer-substring 1 52)
;;       "+his+his buffer is for notes you don't want to save")
;;     (execute-kbd-macro "EEEwcwfoo")
;;     (assert-string=
;;       "Move to \"for\" and change to \"foo\"."
;;       (buffer-substring 1 52)
;;       "+his+his buffer is foo notes you don't want to save")
;;     (execute-kbd-macro "$Ewcw`foo'")
;;     (assert-string=
;;       "Move to next line and change \"If\" to \"`foo'\"."
;;       (buffer-substring 79 113)
;;       ";; `foo' you want to create a file")
;;     (execute-kbd-macro "By2EP")
;;     (assert-string=
;;       "Yank two words and put before point."
;;       (buffer-substring 79 122)
;;       ";; `foo' you`foo' you want to create a file")
;;     (execute-kbd-macro "EEEEEwcw(bar)")
;;     (assert-string=
;;       "Change a single-letter word."
;;       (buffer-substring 97 126)
;;       "you want to create (bar) file")
;;     (execute-kbd-macro "bldiW")
;;     (assert-string=
;;       "Delete inner Word."
;;       (buffer-substring 97 121)
;;       "you want to create  file"))
;;
;;   (deftest test-visual-replace
;;     "Replace with Visual selection."
;;     (execute-kbd-macro "wvjra")
;;     (assert-string=
;;       "Visually replace parts of two lines."
;;       (buffer-string)
;;       ";; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\
;; aaaaaaaaaaaaaaaaaaaaa
;; aaaaf you want to create a file, visit that file with C-x C-f,
;; ;; then enter the text in that file's own buffer.\n")
;;     (execute-kbd-macro "jjvral.l.")
;;     (assert-string=
;;       "Replace a character and repeat for subsequent characters."
;;       (buffer-substring 141 191)
;;       ";; aaan enter the text in that file's own buffer.\n")
;;     (execute-kbd-macro "$ra")
;;     (assert-string=
;;       "Replace at end of line."
;;       (buffer-substring 141 191)
;;       ";; aaan enter the text in that file's own buffera\n"))
;;
;;   (deftest test-change-undo
;;     "Change a word and undo."
;;     (let ((vimpulse-want-change-undo t))
;;       (execute-kbd-macro "wcwfoou")
;;       (assert-string=
;;         (buffer-substring 1 51)
;;         ";; This buffer is for notes you don't want to save"))))

(when vimpulse-tests-run
  (vimpulse-test-suite))

(provide 'vimpulse-tests)

;;; vimpulse-tests.el ends here
