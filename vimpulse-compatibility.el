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

;;; Isearch

(defadvice isearch-message-prefix (around vimpulse-search activate)
  "Use vi prefix if appropriate."
  (if vimpulse-search-prompt
      (setq ad-return-value vimpulse-search-prompt)
    ad-do-it))

(defadvice isearch-update-ring (after vimpulse-search activate)
  "Update `viper-s-string'."
  (when (eq regexp viper-re-search)
    (setq viper-s-string string)))

(defadvice isearch-lazy-highlight-search (around vimpulse-search activate)
  "Deactivate `viper-search-wrap-around'."
  (let (viper-search-wrap-around)
    ad-do-it))

(defadvice viper-search (after vimpulse-search activate)
  "Update isearch history."
  (isearch-update-ring string viper-re-search))

;; If `viper-search-wrap-around' is t, we want the search to wrap
;; without warning
(defun vimpulse-search-fun-function (&optional regexp forward)
  "Return a wrapping search function.
Based on `viper-re-search' and `viper-s-forward'."
  (let* ((regexp (or regexp viper-re-search))
         (forward (or forward viper-s-forward))
         (search-fun (if regexp
                         (if forward
                             're-search-forward
                           're-search-backward)
                       (if forward
                           'search-forward
                         'search-backward))))
    (eval `(lambda (regexp &optional bound noerror count)
             (let ((orig (point)) retval)
               (setq retval (,search-fun regexp bound t count))
               (when (and (not retval) viper-search-wrap-around)
                 (goto-char ,@(if forward '((point-min))
                                '((point-max))))
                 (setq retval (,search-fun regexp bound t count))
                 (unless retval
                   (goto-char orig)))
               retval)))))

(defun vimpulse-search-backward (arg)
  (interactive "P")
  (let ((vimpulse-search-prompt "?")
        (lazy-highlight-initial-delay 0)
        (orig (point))
        (isearch-search-fun-function 'vimpulse-search-fun-function)
        (oldmsg (current-message))
        message-log-max
        search-nonincremental-instead)
    (setq viper-s-forward nil)
    (isearch-backward viper-re-search)
    (when (and (eq (point) orig)
               (not (string= "" isearch-string)))
      (isearch-repeat-backward)
      (isearch-exit))
    (message oldmsg)
    (vimpulse-flash-search-pattern t)
    (setq vimpulse-this-motion 'viper-search-next)))

(defun vimpulse-search-forward (arg)
  (interactive "P")
  (let ((vimpulse-search-prompt "/")
        (lazy-highlight-initial-delay 0)
        (orig (point))
        (isearch-search-fun-function 'vimpulse-search-fun-function)
        (oldmsg (current-message))
        message-log-max
        search-nonincremental-instead)
    (setq viper-s-forward t)
    (isearch-forward viper-re-search)
    (and isearch-other-end (goto-char isearch-other-end))
    (when (and (eq (point) orig)
               (not (string= "" isearch-string)))
      (isearch-repeat-forward)
      (isearch-exit))
    (and isearch-other-end (goto-char isearch-other-end))
    (message oldmsg)
    (vimpulse-flash-search-pattern t)
    (setq vimpulse-this-motion 'viper-search-next)))

(defun vimpulse-flash-search-pattern (&optional only-current)
  (let* ((isearch-string viper-s-string)
         (isearch-forward viper-s-forward)
         (isearch-regexp viper-re-search)
         (isearch-search-fun-function 'vimpulse-search-fun-function)
         isearch-lazy-highlight-last-string)
    (when (viper-has-face-support-p)
      (isearch-highlight (match-beginning 0) (match-end 0))
      (unless only-current
        (isearch-lazy-highlight-new-loop)
        (isearch-lazy-highlight-update))
      (sit-for vimpulse-incremental-delay)
      (isearch-dehighlight)
      (lazy-highlight-cleanup t))))

(when vimpulse-incremental-search
  (fset 'viper-search-backward 'vimpulse-search-backward)
  (fset 'viper-search-forward 'vimpulse-search-forward)
  (fset 'viper-flash-search-pattern 'vimpulse-flash-search-pattern))

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
