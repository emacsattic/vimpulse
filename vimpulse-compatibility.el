;;;; This code integrates Viper with the outside world

;;; undo-tree.el

(when (and (boundp 'undo-tree-visualizer-map)
           (fboundp 'undo-tree-visualizer-quit))
  (defun vimpulse-undo-quit ()
    "Quit the undo-tree visualizer and delete window."
    (interactive)
    (let ((w (selected-window)))
      (undo-tree-visualizer-quit)
      (when (eq w (selected-window))
        (delete-window))))
  (add-to-list 'viper-vi-state-mode-list 'undo-tree-visualizer-mode)
  (let ((map undo-tree-visualizer-map))
    (vimpulse-add-core-movement-cmds map)
    (vimpulse-inhibit-destructive-cmds map)
    (define-key map [remap viper-backward-char] 'undo-tree-visualize-switch-branch-left)
    (define-key map [remap viper-forward-char] 'undo-tree-visualize-switch-branch-right)
    (define-key map [remap viper-next-line] 'undo-tree-visualize-redo)
    (define-key map [remap viper-previous-line] 'undo-tree-visualize-undo)
    (define-key map [remap undo-tree-visualizer-scroll-left] 'viper-scroll-up)
    (define-key map [remap undo-tree-visualizer-scroll-left] 'viper-scroll-up-one)
    (define-key map [remap undo-tree-visualizer-scroll-right] 'viper-scroll-down)
    (define-key map [remap undo-tree-visualizer-scroll-right] 'viper-scroll-down-one)
    (define-key map [remap viper-intercept-ESC-key] 'vimpulse-undo-quit)
    (define-key map [remap undo-tree-visualizer-quit] 'vimpulse-undo-quit)
    (viper-modify-major-mode 'undo-tree-visualizer-mode 'vi-state map)
    (add-to-list 'ex-token-alist '("undolist" (undo-tree-visualize)))
    (add-to-list 'ex-token-alist '("ulist" (undo-tree-visualize)))))

;;; Add vi navigation to help buffers

;; Apropos
(eval-after-load 'apropos
  '(when vimpulse-want-vi-keys-in-apropos
     (add-to-list 'viper-vi-state-mode-list 'apropos-mode)
     (let ((map apropos-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'apropos-mode 'vi-state map))))

;; Buffer-menu
(eval-after-load "buff-menu"
  '(when vimpulse-want-vi-keys-in-buffmenu
     (setq viper-emacs-state-mode-list
           (delq 'Buffer-menu-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'Buffer-menu-mode)
     (let ((map Buffer-menu-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'Buffer-menu-mode 'vi-state map))))

;; Dired
(eval-after-load 'dired
  '(when vimpulse-want-vi-keys-in-dired
     (setq viper-emacs-state-mode-list
           (delq 'dired-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'dired-mode)
     (let ((map dired-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (add-to-list 'ex-token-alist '("e" (epa-dired-do-encrypt)))
       (add-to-list 'ex-token-alist '("s" (epa-dired-do-sign)))
       (add-to-list 'ex-token-alist '("v" (epa-dired-do-verify)))
       (add-to-list 'ex-token-alist '("d" (epa-dired-do-decrypt)))
       (viper-modify-major-mode 'dired-mode 'vi-state map))))

;; Info
(eval-after-load 'info
  '(when vimpulse-want-vi-keys-in-Info
     (setq viper-emacs-state-mode-list
           (delq 'Info-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'Info-mode)
     (let ((map Info-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (define-key map "\C-t" 'Info-history-back) ; l
       (define-key map "\C-o" 'Info-history-back)
       (define-key map "\M-h" 'Info-help) ; h
       (define-key map " " 'Info-scroll-up)
       (define-key map "\C-]" 'Info-follow-nearest-node)
       (define-key map [backspace] 'Info-scroll-down)
       (viper-modify-major-mode 'Info-mode 'vi-state map))))

;; Help
(eval-after-load 'help-mode
  '(when vimpulse-want-vi-keys-in-help
     (setq viper-emacs-state-mode-list
           (delq 'help-mode viper-emacs-state-mode-list))
     (add-to-list 'viper-vi-state-mode-list 'help-mode)
     (let ((map help-mode-map))
       (vimpulse-add-core-movement-cmds map)
       (vimpulse-inhibit-destructive-cmds map)
       (viper-modify-major-mode 'help-mode 'vi-state map))))

;;; ElDoc

(eval-after-load 'eldoc
  '(apply 'eldoc-add-command
          (append vimpulse-viper-movement-cmds
                  vimpulse-core-movement-cmds)))

;;; Folding

;; Almost all of this code is taken from extended-viper
;; coded by Brad Beveridge (bradbev at gmail.com)
;; - I changed the prefix of the custom functions to `vimpulse'
;;   to avoid multiple prefixes
(eval-after-load 'hideshow
  '(progn
     (defun vimpulse-hs-Open ()
       (interactive)
       (hs-show-block)
       (hs-hide-level -1))
     (add-hook 'hs-minor-mode-hook
               (lambda ()
                 (call-interactively 'hs-hide-all)
                 (define-key viper-vi-basic-map "za"
                   (lambda ()
                     (interactive)
                     (hs-toggle-hiding)
                     (hs-hide-level vimpulse-fold-level)))
                 (define-key viper-vi-basic-map "za" 'hs-toggle-hiding)
                 (define-key viper-vi-basic-map "zm" 'hs-hide-all)
                 (define-key viper-vi-basic-map "zr" 'hs-show-all)
                 (define-key viper-vi-basic-map "zo" 'hs-show-block)
                 (define-key viper-vi-basic-map "zc" 'hs-hide-block)))))

;; Load reveal.el if available
(unless (featurep 'reveal)
  (condition-case nil
      (require 'reveal)
    (error nil)))
(when (fboundp 'global-reveal-mode)
  (global-reveal-mode 1))

(provide 'vimpulse-compatibility)
