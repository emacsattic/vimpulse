;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This file contains all the code relative to visual mode.  ;;;;;
;;;; Visual mode is implemented as a minor mode.               ;;;;;
;;;; Currently, visual selection highlighting is done through  ;;;;;
;;;; the use of overlays for linewise and characterwise modes, ;;;;;
;;;; while for blockwise mode, rect-mark.el is needed.         ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Minor Mode code ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup vimpulse-visual nil
  "visual-mode for viper"
  :prefix "vimpulse-visual-"
  :group  'emulations)

(define-minor-mode vimpulse-visual-mode
  "Toggles visual mode in viper"
  :lighter " visual"
  :initial-value nil
  :global nil
  :group 'vimpulse-visual
  (cond
   ((eq t vimpulse-visual-mode)
    (vimpulse-visual-activate 'normal))
   ((not vimpulse-visual-mode)
    ;; Hide the selection
    (vimpulse-visual-hide-region)
    ;; The Emacs region does not include the element under the cursor.
    ;; This is contrary to the behaviour of Vim's visual selection.
    ;; After deactivating the visual selection, the `mark' is set so
    ;; that the Emacs region represents the selected area of the
    ;; visual selection, i.e., the mark-position is increased by 1.
    (if (not vimpulse-visual-region-normalized)
        (save-excursion
          (vimpulse-visual-normalize-region)))
    ;; Clean up local variables
    (setq deactivate-mark-hook
          (delq 'vimpulse-visual-deactivate deactivate-mark-hook))
    (and (boundp 'transient-mark-mode)
         (boundp 'vimpulse-visual-old-transient-mark-mode)
         (setq transient-mark-mode
               vimpulse-visual-old-transient-mark-mode))
    (and (boundp 'cua-mode)
         (boundp 'vimpulse-visual-old-cua-mode)
         (setq cua-mode vimpulse-visual-old-cua-mode))
    (and (boundp 'vimpulse-visual-overlays)
         (vimpulse-visual-delete-overlays vimpulse-visual-overlays))
    (and (boundp 'vimpulse-visual-old-global-variables)
         (mapcar #'kill-local-variable
                 vimpulse-visual-old-global-variables))
    (mapcar #'kill-local-variable
            vimpulse-visual-our-local-variables)
    (viper-deactivate-mark)
    (run-hook-with-args 'vimpulse-visual-deactivate-hook))))

(defcustom vimpulse-visual-mode-map (make-sparse-keymap)
  "Visual mode keymap.
This keymap is active when in visual mode."
  :type  'keymap
  :group 'vimpulse-visual)

(defun vimpulse-visual-face ()
  "Face of visual selection."
  (if (featurep 'xemacs) 'highlight 'region))

(defcustom vimpulse-visual-activate-hook nil
  "Hook run when visual mode is enabled."
  :type  'hook
  :group 'vimpulse-visual)

(defcustom vimpulse-visual-deactivate-hook nil
  "Hook run when visual mode is disabled."
  :type  'hook
  :group 'vimpulse-visual)

(defvar vimpulse-visual-mode nil
  "Current visual mode: may be nil, `normal', `line' or `block'.")

(defvar vimpulse-visual-old-transient-mark-mode nil
  "Last transient-mark-mode.
Visual mode changes this mode, so the original value is
stored here.")
(makunbound 'vimpulse-visual-old-transient-mark-mode)

(defvar vimpulse-visual-old-cua-mode nil
  "Last Cua-mode.
Visual mode changes this mode, so the original value is
stored here.")
(makunbound 'vimpulse-visual-old-cua-mode)

(defvar vimpulse-visual-old-global-variables nil
  "Values of old global variables changed in visual mode.")

(defvar vimpulse-visual-region-normalized nil
  "True if the region has been normalized.")

(viper-deflocalvar
 vimpulse-visual-last-mode nil
 "Last active visual mode: may be nil, `normal', `line' or `block'.")

(defconst vimpulse-visual-our-local-variables
  '(vimpulse-visual-mode
    vimpulse-visual-overlays
    vimpulse-visual-old-global-variables
    vimpulse-visual-old-transient-mark-mode
    vimpulse-visual-old-cua-mode
    vimpulse-visual-region-normalized)
  "Variables which must be buffer-local during visual mode only.")

(defconst vimpulse-visual-temporary-local-variables
  '(transient-mark-mode
    deactivate-mark-hook)
  "System variables which must be temporarily buffer-local.")

(defconst vimpulse-deactivate-mark-commands
  '(clear-rectangle
    copy-rectangle
    copy-rectangle-to-register
    kill-rectangle
    open-rectangle
    string-rectangle
    yank-rectangle
    viper-ex
    viper-exit-minibuffer
    keyboard-quit)
  "List of commands which would have disabled the mark
if Transient Mark Mode was enabled.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions related to visual selection activation, ;;;
;;; mode of operation change (character-wise,         ;;;
;;; line-wise, block-wise)                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-mode-to-insert-mode ()
  "Go from Visual mode to Insert mode."
  (interactive)
  (vimpulse-visual-mode -1)
  (goto-char vimpulse-visual-last-begin)
  (viper-change-state-to-insert))

(defun vimpulse-visual-deactivate ()
  "Deactivates the visual mode."
  (interactive)
  (vimpulse-visual-mode -1))

(defun vimpulse-visual-activate-normal ()
  "Activates normal visual mode."
  (interactive)
  (vimpulse-visual-activate 'normal))

(defun vimpulse-visual-activate-line ()
  "Activates linewise visual mode."
  (interactive)
  (vimpulse-visual-activate 'line))

(defun vimpulse-visual-activate-block ()
  "Activates block visual mode."
  (interactive)
  (vimpulse-visual-activate 'block))

(defun vimpulse-visual-toggle-normal ()
  "Activates normal visual mode if it is not active.
Deactivates visual mode otherwise."
  (interactive)
  (vimpulse-visual-toggle 'normal))

(defun vimpulse-visual-toggle-line ()
  "Activates linewise visual mode if it is not active.
Deactivates visual mode otherwise."
  (interactive)
  (vimpulse-visual-toggle 'line))

(defun vimpulse-visual-toggle-block ()
  "Activates block visual mode if it is not active.
Deactivates visual mode otherwise."
  (interactive)
  (vimpulse-visual-toggle 'block))

;; Currently we use the indices for `vimpulse-visual-last-begin' and
;; `vimpulse-visual-last-end'. Sometimes the command "gv" does not
;; work as expected (e.g., after ">" or "<", because tabs may be
;; inserted/deleted). Markers could do better in this case, but are
;; more sensitive to buffer-changes. This can be bad in visual
;; mode-operations that manipulate the buffer (e.g.,
;; `vimpulse-visual-yank' in rectangular mode kills the rectangle and
;; re-yanks it, which may invalidate the marks). Have to think about a
;; better solution (perhaps more visual mode type specific
;; information).
(viper-deflocalvar
 vimpulse-visual-last-begin (make-marker)
 "Upper-left position of the last selection.")

(viper-deflocalvar
 vimpulse-visual-last-end (make-marker)
 "Lower-right position of the last selection.
Notice that an Emacs region is exclusive the last element, so
this value is indeed (lower-right + 1), where `lower-right'
denotes the last element contained in the selection.")

(defun vimpulse-visual-activate (mode)
  "Activates visual mode. MODE is `normal', `line' or `block'."
  (unless (memq vimpulse-visual-mode '(normal line block))
    ;; If we activate visual mode, set the mark
    (set-mark (point))
    ;; Be careful if we are already marking a rectangle.
    ;; Make each of our state variables buffer local.
    (mapcar #'make-local-variable
            vimpulse-visual-our-local-variables)
    (setq vimpulse-visual-overlays nil
          vimpulse-visual-region-normalized nil
          vimpulse-visual-normalized-region nil
          vimpulse-visual-old-transient-mark-mode
          (and (boundp 'transient-mark-mode)
               transient-mark-mode)
          vimpulse-visual-old-cua-mode
          (and (boundp 'cua-mode)
               cua-mode)
          vimpulse-visual-old-global-variables
          ;; Remember which system variables weren't buffer local
          (let (variable list)
            (dolist (variable
                     vimpulse-visual-temporary-local-variables
                     list)
              (unless (assoc variable (buffer-local-variables))
                (add-to-list 'list variable)))))
    ;; Then make them all buffer local too
    (mapcar #'make-local-variable
            vimpulse-visual-temporary-local-variables)
    (if (boundp 'transient-mark-mode) (setq transient-mark-mode nil))
    (if (boundp 'cua-mode) (setq cua-mode nil))
    (add-hook 'post-command-hook 'vimpulse-visual-post-command)
    (add-hook 'deactivate-mark-hook 'vimpulse-visual-deactivate))
  (setq vimpulse-visual-last-mode mode)
  (setq vimpulse-visual-mode mode)
  (run-hook-with-args 'vimpulse-visual-activate-hook mode)
  (cond
   ((eq 'normal mode)
    (message "-- VISUAL --"))
   ((eq 'line mode)
    (message "-- VISUAL LINE --"))
   ((eq 'block mode)
    (message "-- VISUAL BLOCK --"))))

(defun vimpulse-visual-toggle (mode)
  "Activates visual-mode MODE if this is not the current visual mode.
Otherwise deactivates visual mode."
  (if (eq vimpulse-visual-mode mode)
      (vimpulse-visual-mode -1)
    (vimpulse-visual-activate mode)))

(defun vimpulse-visual-reactivate ()
  "Restores the last visual selection."
  (interactive)
  (when vimpulse-visual-last-mode
    (if (and vimpulse-visual-last-begin
             vimpulse-visual-last-end)
        (progn
          (goto-char vimpulse-visual-last-begin)
          (vimpulse-visual-activate vimpulse-visual-last-mode)
          (goto-char (1- vimpulse-visual-last-end)))
      (vimpulse-visual-activate-normal))))

;;;;;;;;;;;;;;;;;;;;
;;; Key bindings ;;;
;;;;;;;;;;;;;;;;;;;;

(define-key viper-vi-basic-map "v" 'vimpulse-visual-toggle-normal)
(define-key viper-vi-basic-map "V" 'vimpulse-visual-toggle-line)
(define-key viper-vi-basic-map "\C-v" 'vimpulse-visual-toggle-block)
(define-key viper-vi-basic-map "\C-p" 'yank-rectangle)
(define-key viper-vi-basic-map "g" nil)
(define-key viper-vi-basic-map "gv" 'vimpulse-visual-reactivate)

(define-key vimpulse-visual-mode-map "v" 'vimpulse-visual-toggle-normal)
(define-key vimpulse-visual-mode-map "V" 'vimpulse-visual-toggle-line)
(define-key vimpulse-visual-mode-map "\C-v" 'vimpulse-visual-toggle-block)
(define-key vimpulse-visual-mode-map "d" 'vimpulse-visual-delete)
(define-key vimpulse-visual-mode-map "x" 'vimpulse-visual-delete)
(define-key vimpulse-visual-mode-map "D" 'vimpulse-visual-delete)
(define-key vimpulse-visual-mode-map "d" 'vimpulse-visual-delete)
(define-key vimpulse-visual-mode-map "y" 'vimpulse-visual-yank)
(define-key vimpulse-visual-mode-map "i" 'vimpulse-visual-mode-to-insert-mode)
(define-key vimpulse-visual-mode-map "u" 'vimpulse-visual-mode)
(define-key vimpulse-visual-mode-map "R" 'vimpulse-visual-change)
(define-key vimpulse-visual-mode-map "r" 'vimpulse-visual-replace-region)
(define-key vimpulse-visual-mode-map "c" 'vimpulse-visual-change)
(define-key vimpulse-visual-mode-map "C" 'vimpulse-visual-change)
(define-key vimpulse-visual-mode-map "s" 'vimpulse-visual-change)
(define-key vimpulse-visual-mode-map "S" 'vimpulse-visual-change)
(define-key vimpulse-visual-mode-map "\"" 'vimpulse-visual-set-current-register)
(define-key vimpulse-visual-mode-map "o" 'vimpulse-visual-exchange-point-and-mark)
(define-key vimpulse-visual-mode-map "O" 'vimpulse-visual-jump-point)
(define-key vimpulse-visual-mode-map "I" 'vimpulse-visual-insert)
(define-key vimpulse-visual-mode-map "A" 'vimpulse-visual-append)
(define-key vimpulse-visual-mode-map "U" 'vimpulse-visual-make-upcase)
(define-key vimpulse-visual-mode-map "u" 'vimpulse-visual-make-downcase)
(define-key vimpulse-visual-mode-map "~" 'vimpulse-visual-toggle-case)
(define-key vimpulse-visual-mode-map "J" 'vimpulse-visual-join)
(define-key vimpulse-visual-mode-map "<" 'vimpulse-visual-shift-left)
(define-key vimpulse-visual-mode-map ">" 'vimpulse-visual-shift-right)
(define-key vimpulse-visual-mode-map "=" 'vimpulse-visual-indent)
(define-key vimpulse-visual-mode-map "a" 'vimpulse-select-text-object)
(define-key vimpulse-visual-mode-map "i" 'vimpulse-select-text-object)
;; Keys that have no effect in visual mode
(define-key vimpulse-visual-mode-map "." 'undefined)

;; Advice viper-intercept-ESC-key to exit visual mode with ESC
(defadvice viper-intercept-ESC-key
  (around vimpulse-ESC-exit-visual-mode activate)
  "Exit visual mode with ESC."
  (if (and vimpulse-visual-mode
           (not (input-pending-p)))
      (vimpulse-visual-mode -1)
    ad-do-it))

;; This thing is just to silence the byte compiler
;; and stop it bugging about free variable
;; `viper--key-maps' in Emacs 21 :)
;; Update: and to stop Emacs 23 bugging about the old macro.
(defmacro vimpulse-add-visual-maps-macro (keymap)
  `(defadvice viper-normalize-minor-mode-map-alist
     (after ,keymap activate)
     ,(format "Modifies `%s' to include visual keymaps." keymap)
     (add-to-list ',keymap
                  (cons 'vimpulse-visual-mode
                        vimpulse-visual-mode-map))))

(cond
 ((>= emacs-major-version 22)
  (vimpulse-add-visual-maps-macro viper--key-maps))
 (t
  (vimpulse-add-visual-maps-macro minor-mode-map-alist)))

(viper-normalize-minor-mode-map-alist)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual selection visualization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vimpulse-visual-overlays nil
  "List of overlays for current active region.")

;; Set functions for handling overlays (not yet provided by Viper)
(cond
 ((featurep 'xemacs)                    ; XEmacs
  (fset 'vimpulse-delete-overlay #'delete-extent)
  (fset 'vimpulse-mark-active #'region-exists-p))
 (t                                     ; GNU Emacs
  (fset 'vimpulse-delete-overlay #'delete-overlay)
  (fset 'vimpulse-mark-active #'(lambda () mark-active))))

(defun vimpulse-visual-normalize-region ()
  "Normalize visual selection.
 After a call to this function, `mark' points to the lower-right
 element of the selected area and `point' points to the upper-left
 element."
  (setq vimpulse-visual-region-normalized t)
  (cond
   ((eq 'normal vimpulse-visual-mode)
    (if (> (mark t) (point))
        (set-mark (1+ (mark)))
      (progn
        (forward-char)
        (let (deactivate-mark-hook)
          (exchange-point-and-mark)))))
   ((eq 'line vimpulse-visual-mode)
    (let ((start (save-excursion
                   (goto-char (region-beginning))
                   (line-beginning-position)))
          (end (save-excursion
                 (goto-char (region-end))
                 (1+ (line-end-position)))))
      (set-mark end)
      (goto-char start)))
   ((eq 'block vimpulse-visual-mode)
    (let ((beg (region-beginning))
          (end (region-end)))
      (let ((beg-col (save-excursion
                       (goto-char beg)
                       (current-column)))
            (end-col (save-excursion
                       (goto-char end)
                       (current-column))))

        (set-mark (save-excursion
                    (goto-char (max beg end))
                    (move-to-column (max beg-col end-col) t)
                    (1+ (point))))
        (goto-char (min beg end))
        (move-to-column (min beg-col end-col) t)))))
  ;; Set begin and end marker
  (set-marker vimpulse-visual-last-begin (region-beginning))
  (set-marker vimpulse-visual-last-end   (region-end))
  ;; Set Viper's command-pointer marker
  (viper-move-marker-locally 'viper-com-point
                             vimpulse-visual-last-begin
                             (current-buffer)))

(defun vimpulse-visual-start ()
  "Return beginning of visual selection."
  (apply 'min (mapcar 'overlay-start vimpulse-visual-overlays)))

(defun vimpulse-visual-end ()
  "Return end of visual selection."
  (apply 'max (mapcar 'overlay-end vimpulse-visual-overlays)))

(defun vimpulse-visual-bounds ()
  "Return bounds of visual selection as (START END)."
  (list (vimpulse-visual-start) (vimpulse-visual-end)))

(defun vimpulse-visual-highlight-region ()
  "Highlight visual selection, depending on region and visual mode."
  (let ((start (min (point) (mark t)))
        (end   (max (point) (mark t))))
    (cond
     ((eq 'normal vimpulse-visual-mode)
      (if (and vimpulse-visual-overlays
               (viper-overlay-p (car vimpulse-visual-overlays)))
          (progn
            (vimpulse-visual-delete-overlays
             (cdr vimpulse-visual-overlays))
            (setcdr vimpulse-visual-overlays nil)
            (viper-move-overlay (car vimpulse-visual-overlays)
                                start (1+ end)))
        (let ((ov (viper-make-overlay start (1+ end))))
          (viper-overlay-put ov 'face (vimpulse-visual-face))
          (viper-overlay-put ov 'priority 99)
          (setq vimpulse-visual-overlays (list ov)))))
     ((eq 'line vimpulse-visual-mode)
      (let ((start-line (save-excursion
                          (goto-char start)
                          (beginning-of-line)
                          (point)))
            (end-line (save-excursion
                        (goto-char end)
                        (end-of-line)
                        (point))))
        (if (and vimpulse-visual-overlays
                 (viper-overlay-p (car vimpulse-visual-overlays)))
            (progn
              (vimpulse-visual-delete-overlays
               (cdr vimpulse-visual-overlays))
              (setcdr vimpulse-visual-overlays nil)
              (viper-move-overlay (car vimpulse-visual-overlays)
                                  start-line (1+ end-line)))
          (let ((ov (viper-make-overlay start-line (1+ end-line))))
            (vimpulse-visual-hide-region)
            (viper-overlay-put ov 'face (vimpulse-visual-face))
            (viper-overlay-put ov 'priority 99)
            (setq vimpulse-visual-overlays (list ov))))))
     ((eq 'block vimpulse-visual-mode)
      (vimpulse-visual-highlight-block start end))
     (t
      (error "Error")))
    ;; This is a kludge. It's intended to emulate vim's behavior when
    ;; issuing : when visual selecting. To implement the kludge,
    ;; viper-ex is redefined, see viper-function-redefinitions.el.
    (set-marker vimpulse-visual-last-begin start)
    (set-marker vimpulse-visual-last-end   end)
    (set-register (viper-int-to-char (1+ (- ?y ?a)))
                  vimpulse-visual-last-begin)
    (set-register (viper-int-to-char (1+ (- ?z ?a)))
                  vimpulse-visual-last-end)))

(defun vimpulse-visual-highlight-block (start end)
  "Highlight rectangular region from START to END.
We do this by putting an overlay on each line within the
rectangle. Each overlay extends across all the columns of the
rectangle. We try to reuse overlays where possible because this
is more efficient and results in less flicker.
Adapted from: rm-highlight-rectangle"
  (save-excursion
    ;; Calculate the rectangular region represented by point and mark,
    ;; putting start in the north-west corner and end in the
    ;; south-east corner
    (let ((start-col (save-excursion
                       (goto-char start)
                       (current-column)))
          (end-col (save-excursion
                     (goto-char end)
                     (current-column))))
      (if (> start-col end-col)
          (setq start-col (prog1
                              end-col
                            (setq end-col start-col))
                start (save-excursion
                        (goto-char start)
                        (move-to-column start-col nil)
                        (point))
                end (save-excursion
                      (goto-char end)
                      (move-to-column end-col nil)
                      (point))))
      ;; Force a redisplay so we can do reliable window start/end
      ;; calculations
      (sit-for 0)
      (let* ((old vimpulse-visual-overlays)
             (new nil)
             overlay
             (window-start (max (window-start) start))
             (window-end (min (window-end) end))
             (nlines (count-lines window-start
                                  (min (1+ window-end)
                                       (point-max)))))
        ;; Iterate over those lines of the rectangle which are visible
        ;; in the currently selected window
        (goto-char window-start)
        (dotimes (i nlines)
          (let ((row-start (progn
                             (move-to-column start-col nil)
                             (point)))
                (row-end (progn
                           (move-to-column end-col nil)
                           (min (1+ (point))
                                (line-end-position)))))
            ;; Trim old leading overlays
            (while (and old
                        (setq overlay (car old))
                        (< (viper-overlay-start overlay) row-start)
                        (/= (viper-overlay-end overlay) row-end))
              (vimpulse-delete-overlay overlay)
              (setq old (cdr old)))
            ;; Reuse an overlay if possible, otherwise create one
            (if (and old
                     (setq overlay (car old))
                     (or (= (viper-overlay-start overlay) row-start)
                         (= (viper-overlay-end overlay) row-end)))
                (progn
                  (viper-move-overlay overlay row-start row-end)
                  (setq new (cons overlay new)
                        old (cdr old)))
              (setq overlay (viper-make-overlay row-start row-end))
              (viper-overlay-put overlay 'face (vimpulse-visual-face))
              (viper-overlay-put overlay 'priority 99)
              (setq new (cons overlay new))))
          (forward-line 1))
        ;; Trim old trailing overlays
        (vimpulse-visual-delete-overlays old)
        (setq vimpulse-visual-overlays (nreverse new))))))

(defun vimpulse-visual-hide-region ()
  "Remove the highlighting of the visual selection."
  (vimpulse-visual-delete-overlays vimpulse-visual-overlays)
  (setq vimpulse-visual-overlays nil))

(defun vimpulse-visual-delete-overlays (overlays)
  "Delete all overlays in OVERLAYS."
  (mapcar #'vimpulse-delete-overlay overlays))

(defun vimpulse-visual-post-command ()
  "Run after each command in visual mode."
  (cond
   (vimpulse-visual-mode
    (cond
     ((memq this-command vimpulse-deactivate-mark-commands)
      ;; An error in a post-command function can be fatal if it
      ;; re-occurs on each call, thus the condition-case safety
      ;; nets. We have to do things this way because deactivate-mark
      ;; doesn't (in general) get called if transient-mark-mode
      ;; isn't turned on.
      (condition-case nil
          (vimpulse-visual-mode -1)
        (error nil)))
     (t
      (condition-case info
          (vimpulse-visual-highlight-region)
        (error
         (ding)
         (message "visual-mode trouble: %s" info)
         (condition-case nil
             (vimpulse-visual-mode -1)
           (error nil)))))))
   ((boundp 'vimpulse-visual-overlays)
    (condition-case nil
        (vimpulse-visual-delete-overlays vimpulse-visual-overlays)
      (error nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Destructive commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-delete ()
  "Kills the visual selection to the kill-ring."
  (interactive)
  (let ((mode vimpulse-visual-mode) length)
    (vimpulse-visual-normalize-region)
    (setq length (abs (- (region-beginning) (region-end))))
    (cond
     ((eq 'normal mode)
      (viper-prefix-arg-com ?r 1 ?d)
      (viper-set-destructive-command
       (list 'viper-forward-char
             length ?d viper-use-register nil nil)))
     ((eq 'line mode)
      (goto-char vimpulse-visual-last-begin)
      (viper-line (cons (count-lines
                         vimpulse-visual-last-begin
                         vimpulse-visual-last-end)
                        ?D)))
     ((eq 'block mode)
      (kill-rectangle vimpulse-visual-last-begin
                      vimpulse-visual-last-end)
      (goto-char vimpulse-visual-last-begin)))
    (vimpulse-visual-mode -1)))

(defun vimpulse-visual-change ()
  "Change the visual selection to the kill-ring."
  (interactive)
  (let ((mode vimpulse-visual-mode) length)
    (vimpulse-visual-normalize-region)
    (setq length (abs (- (region-beginning) (region-end))))
    (vimpulse-visual-mode -1)
    (vimpulse-push-buffer-undo-list-mark)
    (cond
     ((eq 'normal mode)
      (viper-prefix-arg-com ?r 1 ?c)
      (viper-set-destructive-command
       (list 'viper-forward-char
             length ?c viper-use-register nil nil)))
     ((eq 'line mode)
      (goto-char vimpulse-visual-last-begin)
      (viper-line (cons (count-lines
                         vimpulse-visual-last-begin
                         vimpulse-visual-last-end)
                        ?C)))
     ((eq 'block mode)
      (vimpulse-visual-create-coords
       mode ?c
       vimpulse-visual-last-begin
       vimpulse-visual-last-end)
      (kill-rectangle vimpulse-visual-last-begin
                      vimpulse-visual-last-end)
      (goto-char vimpulse-visual-last-begin)
      (viper-insert nil)))
    (vimpulse-visual-mode -1)))

(defun vimpulse-visual-indent ()
  "Indent visual selection."
  (interactive)
  (indent-region (vimpulse-visual-start)
                 (vimpulse-visual-end))
  (vimpulse-visual-mode -1))

(defun vimpulse-visual-replace-region (&optional arg)
  "Replace all selected characters with ARG."
  (interactive "P")
  (let ((mode vimpulse-visual-mode))
    (vimpulse-visual-normalize-region)
    (cond
     ((memq mode '(normal line))
      (goto-char vimpulse-visual-last-begin)
      (viper-replace-char arg)
      (let ((c (char-after (point))))
        (dotimes
            (i (- vimpulse-visual-last-end
                  vimpulse-visual-last-begin))
          (cond
           ((member (char-after (point)) '(?\r ?\n))
            (forward-char))
           (t (delete-char 1)
              (insert c))))))
     ((eq 'block mode)
      (goto-char vimpulse-visual-last-begin)
      (viper-replace-char arg)
      (let* ((c (char-after (point)))
             (begin-col (current-column))
             (len (- (save-excursion
                       (goto-char vimpulse-visual-last-end)
                       (current-column))
                     begin-col)))
        (while (< (point) vimpulse-visual-last-end)
          (move-to-column begin-col nil)
          (let ((n 0))
            (while (and (< n len)
                        (not (member (char-after (point))
                                     '(?\r ?\n))))
              (delete-char 1)
              (insert c)
              (setq n (1+ n))))
          (forward-line))))
     (t
      (error "Viper not in visual-mode.")))
    (vimpulse-visual-mode -1)
    (goto-char vimpulse-visual-last-begin)))

(defun vimpulse-visual-make-upcase ()
  "Converts all selected characters to upper case."
  (interactive)
  (vimpulse-visual-change-case #'upcase-region))

(defun vimpulse-visual-make-downcase ()
  "Converts all selected characters to lower case."
  (interactive)
  (vimpulse-visual-change-case #'downcase-region))

(defun vimpulse-visual-toggle-case ()
  "Toggles the case of all selected characters."
  (interactive)
  (vimpulse-visual-change-case #'vimpulse-visual-toggle-case-region))

(defun vimpulse-visual-change-case (case-func)
  (let ((mode vimpulse-visual-mode))
    (vimpulse-visual-normalize-region)
    (cond
     ((memq mode '(normal line))
      (funcall case-func
               vimpulse-visual-last-begin
               vimpulse-visual-last-end))
     ((eq 'block mode)
      (let ((begin-col (save-excursion
                         (goto-char vimpulse-visual-last-begin)
                         (current-column)))
            (len  (- (save-excursion
                       (goto-char vimpulse-visual-last-end)
                       (current-column))
                     (save-excursion
                       (goto-char vimpulse-visual-last-begin)
                       (current-column)))))
        (goto-char vimpulse-visual-last-begin)
        (while (< (point) vimpulse-visual-last-end)
          (let ((from (save-excursion
                        (move-to-column begin-col nil)
                        (point)))
                (to (save-excursion
                      (move-to-column (+ begin-col len) nil)
                      (point))))
            (funcall case-func from to)
            (forward-line)))))
     (t
      (error "Viper not in visual mode.")))
    (vimpulse-visual-mode -1)
    (goto-char vimpulse-visual-last-begin)))

(defun vimpulse-visual-toggle-case-region (beg end)
  "Toggles the case of all characters from BEG to END (exclusive)."
  (save-excursion
    (goto-char beg)
    (while (< beg end)
      (setq c (following-char))
      (delete-char 1 nil)
      (if (eq c (upcase c))
          (insert-char (downcase c) 1)
        (insert-char (upcase c) 1))
      (setq beg (1+ beg)))))

(defun vimpulse-visual-join ()
  "Joins the selected lines."
  (interactive)
  (when vimpulse-visual-mode
    (vimpulse-visual-mode -1)
    (goto-char vimpulse-visual-last-begin)
    (viper-join-lines
     (count-lines vimpulse-visual-last-begin
                  vimpulse-visual-last-end))))

;; Currently, I don't know how to take the argument ARG
;; into the repeat-command
(defun vimpulse-visual-shift-left (arg)
  "Shift all selected lines to the left."
  (interactive "P")
  (let ((mode vimpulse-visual-mode))
    (vimpulse-visual-normalize-region)
    (vimpulse-visual-mode -1)
    (vimpulse-push-buffer-undo-list-mark)
    (let ((nlines (1- (count-lines vimpulse-visual-last-begin
                                   vimpulse-visual-last-end))))
      (dotimes (i (or arg 1))
        (goto-char vimpulse-visual-last-begin)
        (viper-next-line (cons nlines ?<)))
      (vimpulse-connect-undos))))

(defun vimpulse-visual-shift-right (arg)
  "Shift all selected lines to the right."
  (interactive "P")
  (let ((mode vimpulse-visual-mode))
    (vimpulse-visual-normalize-region)
    (vimpulse-visual-mode -1)
    (vimpulse-push-buffer-undo-list-mark)
    (let ((nlines (1- (count-lines vimpulse-visual-last-begin
                                   vimpulse-visual-last-end))))
      (dotimes (i (or arg 1))
        (goto-char vimpulse-visual-last-begin)
        (viper-next-line (cons nlines ?>)))
      (vimpulse-connect-undos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intermediate commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-set-current-register ()
  (interactive)
  (setq viper-use-register (read-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Non-destructive commands ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vimpulse-visual-yank ()
  "Save the visual selection in the kill-ring."
  (interactive)
  (let ((mode vimpulse-visual-mode))
    (vimpulse-visual-normalize-region)
    (cond
     ((memq mode '(normal line))
      (viper-prefix-arg-com ?r 1 ?y))
     ((eq 'block mode)
      (let ((pos vimpulse-visual-last-begin))
        (kill-rectangle vimpulse-visual-last-begin
                        vimpulse-visual-last-end)
        (goto-char pos)
        (yank-rectangle)))
     (t
      (error "Viper not in visual mode")))
    (vimpulse-visual-mode -1)
    (goto-char vimpulse-visual-last-begin)))

(defun vimpulse-visual-exchange-point-and-mark ()
  "Same as `exchange-point-and-mark', does not show region."
  (interactive)
  (let (deactivate-mark-hook)
    (if (featurep 'xemacs)
        (exchange-point-and-mark t)
      (exchange-point-and-mark))))

(defun vimpulse-visual-jump-point ()
  "Similar to `vimpulse-visual-exchange-point-and-mark',
but in block visual mode the cursor jumps to the other corner of
the selected region in the current line."
  (interactive)
  (cond
   ((memq vimpulse-visual-mode '(normal line))
    (vimpulse-visual-exchange-point-and-mark))
   ((eq 'block vimpulse-visual-mode)
    (let ((mark-col (save-excursion
                      (goto-char (mark t))
                      (current-column)))
          (point-col (current-column)))
      (set-mark (save-excursion
                  (goto-char (mark t))
                  (move-to-column point-col t)
                  (point)))
      (move-to-column mark-col t)))
   (t
    (error "Viper not in visual mode."))))

(defun vimpulse-select-text-object
  (count &optional char motion)
  "Visually select a text object."
  (interactive "p")
  (let* ((char    (or char last-command-event))
         (motion  (or motion (read-char)))
         (bounds  (vimpulse-unify-multiple-bounds
                   (point) char count motion))
         (start   (car bounds))
         (end     (cadr bounds)))
    (when bounds
      (unless (vimpulse-widen-selection start end)
        ;; We're stuck; move and try again
        (if (< (point) (mark t))
            (backward-char) (forward-char))
        (setq bounds (vimpulse-unify-multiple-bounds
                      (point) char count motion)
              start (car bounds)
              end   (cadr bounds))
        (vimpulse-widen-selection start end))
      (setq vimpulse-last-object-selection
            (list count char motion)))))

(defun vimpulse-widen-selection (start end)
  "Widen visual selection to START and END.
Returns nil if selection is unchanged (i.e., selection already
encompasses START and END). When called interactively, derives
START and END from previous text object selection."
  (interactive
   (let ((count  (nth 0 vimpulse-last-object-selection))
         (char   (nth 1 vimpulse-last-object-selection))
         (motion (nth 2 vimpulse-last-object-selection)))
     (when vimpulse-last-object-selection
       (vimpulse-select-text-object count char motion))
     '(nil nil))) ; that's it, we're done
  (let (widen-start-p widen-end-p)
    (cond
     ((or (not (numberp start)) (not (numberp end)))
      nil)
     ((< (point) (mark t))
      (setq widen-start-p
            (not (= (point) (min start end (point)))))
      (setq widen-end-p
            (not (= (mark t)
                    (max start end (mark t)))))
      (set-mark
       (max start end (mark t)))
      (goto-char (min start end (point))))
     (t
      (setq widen-start-p
            (not (= (mark t)
                    (min start end (mark t)))))
      (setq widen-end-p
            (not (= (point) (max start end (point)))))
      (set-mark
       (min start end (mark t)))
      (goto-char (max start end (point)))))
    ;; Was selection widened?
    (or widen-start-p widen-end-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual Block Mode Support ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(viper-deflocalvar
 vimpulse-undo-needs-adjust nil
 "If true, several commands in the undo-list should be connected.")

(defconst vimpulse-buffer-undo-list-mark 'vimpulse
  "Everything up to this mark is united in the undo-list.")

;; This variable holds the point and column of the first line
;; as well as the number of lines in the region
(defvar vimpulse-visual-insert-coords nil
  "A list with (I-COM UL-POS COL NLINES), where
I-COM is the insert command (?i, ?a, ?I or ?A),
UL-POS is the position of the upper left corner of the region,
COL is the column of insertion, and
NLINES is the number of lines in the region.")

(defun vimpulse-visual-create-coords
  (mode i-com upper-left lower-right)
  "Update the list of block insert coordinates with current rectangle.
I-COM should be ?c, ?i, ?a, ?I or ?A; the column for the
insertion will be chosen according to this command.
Returns the insertion point."
  (setq vimpulse-visual-insert-coords nil)
  (let ((nlines (count-lines upper-left lower-right))
        (col 0)) ; for ?I and ?A, trivial -- column is 0
    (when (or (eq i-com ?a) (eq i-com ?i) (eq i-com ?c))
      ;; For ?i and ?a, choose the left (the right) rectangle column
      (let ((start-col (save-excursion
                         (goto-char upper-left)
                         (current-column)))
            (end-col (save-excursion
                       (goto-char lower-right)
                       (current-column))))
        ;; Decide if we use the left or right column
        (setq col (max 0 (if (or (eq i-com ?i) (eq i-com ?c))
                             start-col
                           (1- end-col))))))
    ;; Save the information
    (setq vimpulse-visual-insert-coords
          (list mode i-com upper-left col nlines))
    (save-excursion
      (goto-char upper-left)
      (move-to-column col nil)
      (point))))

(defun vimpulse-filter-undos (undo-list)
  "Filters all `nil' marks from `undo-list' until the first
occurrence of `vimpulse-buffer-undo-list-mark'."
  (cond
   ((null undo-list)
    nil)
   ((eq (car undo-list) 'vimpulse)
    (cdr undo-list))
   ((null (car undo-list))
    (vimpulse-filter-undos (cdr undo-list)))
   (t
    (cons (car undo-list)
          (vimpulse-filter-undos (cdr undo-list))))))

(defun vimpulse-connect-undos ()
  "Connects all undo-steps from `buffer-undo-list' up to the
first occurrence of `vimpulse-buffer-undo-list-mark'."
  (when (and vimpulse-undo-needs-adjust
             (listp buffer-undo-list))
    (setq buffer-undo-list
          (vimpulse-filter-undos buffer-undo-list)))
  (setq vimpulse-undo-needs-adjust nil))

(defun vimpulse-push-buffer-undo-list-mark ()
  (setq vimpulse-undo-needs-adjust t)
  (push vimpulse-buffer-undo-list-mark buffer-undo-list))

;; Redefinitions of viper functions to handle visual block-mode,
;; that is, the "update all lines when we hit ESC" part.
;; This function is not in viper-functions-redefinitions.el
;; because its code is closely related to visual mode.
(defun viper-exit-insert-state ()
  (interactive)
  (viper-change-state-to-vi)
  (when vimpulse-visual-insert-coords
    ;; Get the saved info about the visual region
    (let ((mode   (nth 0 vimpulse-visual-insert-coords))
          (i-com  (nth 1 vimpulse-visual-insert-coords))
          (pos    (nth 2 vimpulse-visual-insert-coords))
          (col    (nth 3 vimpulse-visual-insert-coords))
          (nlines (nth 4 vimpulse-visual-insert-coords)))
      (goto-char pos)
      (save-excursion
        (dotimes (i (1- nlines))
          (forward-line 1)
          (let ((cur-col (move-to-column col)))
            ;; If we are in block mode, this line, but do not hit the
            ;; correct column, we check if we should convert tabs
            ;; and/or append spaces
            (if (and (eq mode 'block)
                     (or (/= col cur-col) ; wrong column or
                         (eolp)))         ; end of line
                (cond ((< col cur-col)    ; we are inside a tab
                       (move-to-column (1+ col) 'fill) ; convert to spaces
                       (move-to-column col 'fill) ; this is needed for ?a
                       (viper-repeat nil))
                      ((and (>= col cur-col) ; we are behind the end
                            (eq i-com ?a))   ; and I-COM is ?a
                       (move-to-column (1+ col) t) ; append spaces
                       (viper-repeat nil)))
              (viper-repeat nil)))))
      (setq vimpulse-visual-insert-coords nil)))
  ;; Update undo-list
  (vimpulse-connect-undos))

;; These 2 functions implement insertion at the beginning/end of a
;; visual block or linewise selection
(defun vimpulse-visual-insert (&optional arg)
  "Enter Insert at beginning of visual selection."
  (interactive "P")
  (let ((mode vimpulse-visual-mode))
    (vimpulse-visual-normalize-region)
    (vimpulse-visual-mode -1)
    (cond
     ((eq 'normal mode)
      (viper-insert arg))
     ((eq 'line mode)
      (goto-char (vimpulse-visual-create-coords
                  mode ?I
                  vimpulse-visual-last-begin
                  vimpulse-visual-last-end))
      (viper-Insert arg))
     ((eq 'block mode)
      (goto-char (vimpulse-visual-create-coords
                  mode ?i
                  vimpulse-visual-last-begin
                  vimpulse-visual-last-end))
      (viper-insert arg))
     (t
      (error "Viper not in visual mode!")))))

(defun vimpulse-visual-append (&optional arg)
  "Enter Insert at end of visual selection."
  (interactive "P")
  (let ((mode vimpulse-visual-mode))
    (vimpulse-visual-normalize-region)
    (vimpulse-visual-mode -1)
    (cond
     ((eq 'normal mode)
      (goto-char (mark t))
      (viper-insert arg))
     ((eq 'line mode)
      (goto-char (mark t))
      (unless (bobp) (backward-char))
      (viper-insert arg))
     ((eq 'block mode)
      (goto-char (vimpulse-visual-create-coords
                  mode ?a
                  vimpulse-visual-last-begin
                  vimpulse-visual-last-end))
      (viper-append arg))
     (t
      (error "Viper not in visual mode!")))))

;; Viper's larger movement commands use the mark to store the previous
;; position, which is fine and useful when the mark isn't active. When
;; it is, however, it has the effect of remaking the whole region.
(defadvice push-mark (around vimpulse-visual-mode activate)
  (unless (and vimpulse-visual-mode
               ;; Note: if you really need to call `push-mark'
               ;; in proximity with the commands below (i.e., in a hook),
               ;; do (let (this-command) (push-mark)).
               (memq this-command
                     '(vimpulse-goto-first-line
                       viper-backward-paragraph
                       viper-backward-sentence
                       viper-forward-paragraph
                       viper-forward-sentence
                       viper-goto-line
                       viper-window-bottom
                       viper-window-middle
                       viper-window-top)))
    ad-do-it))

;; CHECKME: is this still needed?
(defadvice viper-deactivate-mark
  (around vimpulse-deactivate-mark-wrap activate)
  (unless vimpulse-visual-mode
    ad-do-it))

(provide 'vimpulse-visual-mode)

