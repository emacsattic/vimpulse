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
;(setq vimpulse-movement-map (make-sparse-keymap))
;(vimpulse-set-movement-keys-for-map vimpulse-movement-map)
;(viper-modify-major-mode 'dired-mode 'emacs-state vimpulse-movement-map) 
;(viper-modify-major-mode 'help-mode 'emacs-state vimpulse-movement-map)





(provide 'vimpulse-utils)
