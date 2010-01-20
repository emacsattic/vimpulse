;;;;
;;;; This file contains helper functions that
;;;; a) Can be useful for the end user
;;;; b) Can be useful for the contributor, thus avoiding
;;;;    duplication of functionalities.
;;;;

(defun vimpulse-is-whitespace (pos)
  "Returns true if the character at `pos' is whitespace, nil otherwhise"
  (equal (char-syntax (char-after pos)) 32))

;; Define a helper function that sets up the viper keys in a given map.
;; This function is useful for creating movement maps or altering existing
;; maps
(defun vimpulse-set-movement-keys-for-map (map)
  (define-key map "\C-d" 'viper-scroll-up)
  (define-key map "\C-u" 'viper-scroll-down)
  (define-key map "j" 'viper-next-line)
  (define-key map "k" 'viper-previous-line)
  (define-key map "l" 'viper-forward-char)
  (define-key map "h" 'viper-backward-char))

;; EXAMPLE, the following lines enable Vim style movement in help
;; and dired modes.
;; create a movement map and set the keys
;;
;; (setq vimpulse-movement-map (make-sparse-keymap))
;; (vimpulse-set-movement-keys-for-map vimpulse-movement-map)
;; (viper-modify-major-mode 'dired-mode 'emacs-state vimpulse-movement-map)
;; (viper-modify-major-mode 'help-mode 'emacs-state vimpulse-movement-map)

(defmacro vimpulse-region-command (function)
  "Convert Emacs region commands to visual selection commands.
See the comments in the source for an example on how to use it."
  `(lambda ()
     ,(format "Apply `%s' on visual selection." function)
     (interactive)
     (,function (vimpulse-visual-start) (vimpulse-visual-end))
     (vimpulse-visual-mode -1)))

;; The macro vimpulse-region-commands works with any Emacs command
;; that operates with a region and takes as arguments the beginning
;; and end of the region. For example, the `comment-region' and
;; `uncomment-region' commands:
;;
;;     (comment-region beg end &optional arg)
;;     (uncomment-region beg end &optional arg)
;;
;; You can define new bindings for commenting and uncommenting the
;; region as easily as:
;;
;;     (define-key viper-vi-global-user-map "\\\]"
;;       (vimpulse-region-command comment-region))
;;     (define-key viper-vi-global-user-map "\\,"
;;       (vimpulse-region-command uncomment-region))

(provide 'vimpulse-utils)

