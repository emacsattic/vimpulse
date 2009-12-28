(defvar vimpulse-experimental t
  "Define whether or not use experimental features. Turned on by default, so you will give feedback :P.")

;; Load advice.el.
(require 'advice)

;; Load cl.el. This only loads the macros, and is permitted under
;; GNU Emacs' inclusion policy. However, in the long run we should
;; purge the code of all cl.el dependencies.
(eval-when-compile (require 'cl))

;; Load redo.el if available.  Sadly we can't use APEL's require
;; function to get 'noerror functionality because GNU Emacs 21 doesn't
;; ship with APEL included.
(unless (featurep 'redo)
  (load "redo" 'noerror))

(provide 'vimpulse-dependencies)
