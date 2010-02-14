;;; Code:

;; Load Viper
(unless (boundp 'viper-mode)
  (setq viper-mode t))
(require 'viper)

;; Load redo.el if available. Sadly we can't use APEL's require
;; function to get 'noerror functionality because GNU Emacs 21
;; doesn't ship with APEL included.
(unless (featurep 'redo)
  (load "redo" t))

;; Load rect.el and rect-mark.el if available
(require 'rect)
(unless (featurep 'rect-mark)
  (load "rect-mark" t))

;; Load paren.el if available
(unless (featurep 'paren)
  (load "paren" t))

;; Customization group for Vimpulse
(defgroup vimpulse nil
  "Vim emulation within Emacs."
  :group  'emulations
  :link   '(custom-group-link "viper")
  :prefix 'vimpulse-)

(defcustom vimpulse-experimental t
  "Whether or not to use experimental features.
Turned on by default, so you will give feedback :P"
  :group 'vimpulse
  :type  'boolean)

;; The secrets discovered from untold diggings among
;; the ruins of Customize code
(defun vimpulse-custom-value-p (symbol)
  "Non-nil if SYMBOL has a customized value."
  (or (get symbol 'customized-value)
      (get symbol 'customized-face)
      (get symbol 'saved-value)))

(defmacro vimpulse-setq-custom (sym val)
  "Set the customized value of SYM to VAL."
  `(prog1 (setq ,sym ,val)              ; return VAL
     (when (get ',sym 'custom-autoload)
       (custom-load-symbol ',sym))
     (put ',sym 'customized-value (list (custom-quote ,val)))))

(defmacro vimpulse-setq-custom-default (symbol value)
  "Set the customized default value of SYMBOL to VALUE."
  `(prog1 ,value                        ; return VALUE
     (when (get ',symbol 'custom-autoload)
       (custom-load-symbol ',symbol))
     (put ',symbol 'standard-value (list (custom-quote ,value)))))

(defmacro vimpulse-setq (sym val)
  "Set SYM to VAL, defaults included, unless SYM is customized.
SYM is unquoted. Returns VAL."
  `(cond
    ;; Customized value: just set custom standard value
    ((vimpulse-custom-value-p ',sym)
     (vimpulse-setq-custom-default ,sym ,val))
    ;; Customized variable: set custom and regular values
    ((custom-variable-p ',sym)
     (vimpulse-setq-custom-default ,sym ,val)
     (vimpulse-setq-custom ,sym ,val)
     (setq-default ,sym ,val)
     (setq ,sym ,val))
    ;; Regular variable; set default and local values
    (t
     (setq-default ,sym ,val)
     (setq ,sym ,val))))

;; Carefully set Viper/woman variables
(defun vimpulse-initialize-variables ()
  "Set various non-Vimpulse variables, unless customized."
  ;; Fast paren-matching
  (vimpulse-setq show-paren-delay 0)
  ;; Can backspace past start of insert/line
  (vimpulse-setq viper-ex-style-editing nil)
  ;; Don't create new frame for manpages
  (vimpulse-setq woman-use-own-frame nil)
  ;; Don't prompt upon K key (manpage display)
  (vimpulse-setq woman-use-topic-at-point t)
  ;; Make cursor color consistent
  (vimpulse-setq viper-insert-state-cursor-color
                 viper-vi-state-cursor-color)
  ;; Cursor moves backwards when exiting Insert state
  (vimpulse-setq viper-ESC-moves-cursor-back t)
  ;; Not in Vim: C-h is indispensable in Emacs
  (vimpulse-setq viper-want-ctl-h-help t)
  ;; Refresh Viper settings
  (viper-change-state-to-vi))

(if (and (boundp 'after-init-time) after-init-time)
    (vimpulse-initialize-variables)
  (add-hook 'after-init-hook 'vimpulse-initialize-variables))

(provide 'vimpulse-dependencies)

