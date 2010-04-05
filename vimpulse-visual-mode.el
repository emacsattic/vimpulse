;;;; Visual mode

;; Visual mode is defined as another Viper state, just like vi state,
;; Insert state, Replace state etc. It inherits keybindings from
;; vi state (movement), but defines some bindings of its own
;; on top of that.
;;
;; Text selection in Emacs and Vim differs subtly by that in Vim,
;; the character under the cursor is included in the selection, while
;; Emacs' region excludes it. Vimpulse solves the problem by
;; "translating" a Visual selection to the equivalent Emacs region
;; when a command is about to be executed. Likewise, a Line selection
;; is translated to an Emacs region of whole lines.
;;
;; This is pretty transparent, except that we don't wish to do any
;; translating when the user is just moving around in the buffer.
;; To that end, the variable `vimpulse-movement-cmds' lists all of
;; Viper's movement commands, so that translation can be postphoned
;; until the user executes a non-movement command.
;;
;; Block selections are rectangle compatible. This means Emacs'
;; rectangular commands are applicable on the selection, and you can
;; write your own utilities using the rect.el library.

(vimpulse-define-state visual
  "Visual mode is a flexible and easy way to select text.
To use Visual mode, press v in vi (command) mode. Then use the
motion commands to expand the selection. Press d to delete, c to
change, r to replace, or y to copy. You can use p to paste.
For Line selection, press V instead of v; then you can copy and
paste whole lines. For Block selection, press C-v; now you can
copy and paste the selected rectangle. In Block selection, you
may use I or A to insert or append text before or after the
selection on each line."
  :id "<VIS> "
  :basic-minor-mode 'vimpulse-visual-mode
  :enable '((vimpulse-visual-mode (or vimpulse-visual-mode t))
            (vimpulse-operator-remap-minor-mode nil)
            operator-state
            vi-state)
  (cond
   ((eq 'visual-state new-state)
    (unless (memq vimpulse-visual-mode '(normal line block))
      (vimpulse-visual-mode 1)))
   (t
    (vimpulse-visual-mode -1))))

(defgroup vimpulse-visual nil
  "Visual mode for Viper."
  :prefix "vimpulse-visual-"
  :group  'vimpulse)

(define-minor-mode vimpulse-visual-mode
  "Toggles Visual mode in Viper."
  :initial-value nil
  :keymap vimpulse-visual-basic-map
  :global nil
  :group 'vimpulse-visual
  (cond
   (vimpulse-visual-mode
    (unless (memq vimpulse-visual-mode '(normal line block))
      (vimpulse-visual-activate 'normal)))
   (t
    ;; This is executed when we do (vimpulse-visual-mode -1).
    ;; It must run without error even if Visual mode is not active.
    (vimpulse-visual-highlight -1)
    ;; Clean up local variables
    (dolist (var vimpulse-visual-local-vars)
      (when (assq var vimpulse-visual-vars-alist)
        (set var (cdr (assq var vimpulse-visual-vars-alist))))
      (when (memq var vimpulse-visual-global-vars)
        (kill-local-variable var)))
    (vimpulse-visual-block-cleanup-whitespace)
    ;; Deactivate mark
    (when vimpulse-visual-vars-alist
      (vimpulse-deactivate-mark t))
    (vimpulse-transient-restore)
    (kill-local-variable 'vimpulse-visual-vars-alist)
    (kill-local-variable 'vimpulse-visual-global-vars)
    ;; If Viper state is not already changed,
    ;; change it to vi (command) state
    (when (eq viper-current-state 'visual-state)
      (cond
       ((eq 'emacs-state vimpulse-visual-previous-state)
        (viper-change-state-to-emacs))
       (t
        (viper-change-state-to-vi))))
    (kill-local-variable 'vimpulse-visual-previous-state))))

(viper-deflocalvar vimpulse-visual-mode nil
  "Current Visual mode: may be nil, `normal', `line' or `block'.")

(defcustom vimpulse-visual-block-untabify nil
  "Whether Block mode may change tabs to spaces for fine movement.
Off by default."
  :type  'boolean
  :group 'vimpulse-visual)

(viper-deflocalvar vimpulse-visual-global-vars nil
  "List of variables which were global.")

(viper-deflocalvar vimpulse-visual-local-vars
  '(cua-mode
    mark-active
    transient-mark-mode
    zmacs-regions
    vimpulse-visual-region-expanded)
  "System variables which are reset for each Visual session.")

(viper-deflocalvar vimpulse-visual-vars-alist nil
  "Alist of old variable values.")

(viper-deflocalvar vimpulse-visual-last nil
  "Last active Visual mode.
May be nil, `normal', `line', `block' or `insert'.")

(viper-deflocalvar vimpulse-visual-previous-state 'viper-state
  "Previous state before enabling Visual mode.
This lets us revert to Emacs state in non-vi buffers.")

(viper-deflocalvar vimpulse-visual-region-expanded nil
  "Whether region is expanded to the Visual selection.")

(viper-deflocalvar vimpulse-visual-point nil
  "Last value of `point' in Visual mode.")

(viper-deflocalvar vimpulse-visual-mark nil
  "Last value of `mark' in Visual mode.")

(viper-deflocalvar vimpulse-visual-overlay nil
  "Overlay for Visual selection.
In XEmacs, this is an extent.")

(viper-deflocalvar vimpulse-visual-block-overlays nil
  "Overlays for Visual Block selection.")

(viper-deflocalvar vimpulse-visual-norm-overlay nil
  "Overlay encompassing text inserted into the buffer
to make Block selection at least one column wide.")

;; Defined in rect.el
(defvar killed-rectangle nil)

(viper-deflocalvar vimpulse-undo-needs-adjust nil
  "If true, several commands in the undo-list should be connected.")

(defconst vimpulse-buffer-undo-list-mark 'vimpulse
  "Everything up to this mark is united in the undo-list.")

;; This variable holds the point and column of the first line
;; as well as the number of lines in the region
(defvar vimpulse-visual-insert-coords nil
  "List with (I-COM UL-POS COL NLINES), where
I-COM is the insert command (?i, ?a, ?I or ?A),
UL-POS is the position of the upper left corner of the region,
COL is the column of insertion, and
NLINES is the number of lines in the region.")

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

;;; Activation

(defun vimpulse-visual-activate (&optional mode)
  "Activate Visual mode. MODE is `normal', `line' or `block'.
May also be used to change the Visual mode."
  (unless (memq vimpulse-visual-mode '(normal line block))
    ;; We are activating Visual mode for the first time
    (kill-local-variable 'vimpulse-visual-vars-alist)
    (kill-local-variable 'vimpulse-visual-global-vars)
    (setq vimpulse-visual-previous-state viper-current-state)
    ;; Make global variables buffer-local
    (setq vimpulse-visual-vars-alist nil)
    (dolist (var vimpulse-visual-local-vars)
      (when (boundp var)
        ;; Remember old value
        (add-to-list 'vimpulse-visual-vars-alist
                     (cons var (eval var))))
      (unless (assoc var (buffer-local-variables))
        (make-local-variable var)
        (add-to-list 'vimpulse-visual-global-vars var)))
    ;; Re-add hooks in case they were cleared
    (add-hook 'pre-command-hook 'vimpulse-visual-pre-command)
    (add-hook 'post-command-hook 'vimpulse-visual-post-command)
    (if (featurep 'xemacs)
        (add-hook 'zmacs-deactivate-region-hook
                  'vimpulse-visual-deactivate-hook)
      (add-hook 'deactivate-mark-hook 'vimpulse-visual-deactivate-hook))
    ;; Remove nonsensical t value
    (and (boundp 'mark-active)
         (setq mark-active (vimpulse-mark-active)))
    ;; Activate mark at point
    (cond
     ((eq 'block mode)
      (set-mark (point))
      (vimpulse-deactivate-mark t)     ; `set-mark' activates the mark
      (vimpulse-transient-mark -1))
     (t
      (vimpulse-transient-mark 1)
      ;; Convert active Emacs region to Visual selection, if any.
      ;; To avoid confusion, do not move point, even if this means the
      ;; selection increases by one character when mark is before
      ;; point.
      (cond
       ((vimpulse-mark-active)
        (vimpulse-visual-contract-region t))
       (t
        (vimpulse-activate-mark (point))))
      (vimpulse-visual-highlight))))
  ;; Set the Visual mode
  (setq mode (or mode 'normal))
  (setq vimpulse-visual-mode mode
        vimpulse-visual-last mode)
  (viper-change-state 'visual-state)
  (viper-restore-cursor-type)           ; use vi cursor
  ;; Reactivate mark
  (cond
   ((eq 'block mode)
    (vimpulse-deactivate-mark t)
    (vimpulse-transient-mark -1))
   (t
    (vimpulse-transient-mark 1)
    (vimpulse-activate-mark))))

(defun vimpulse-visual-toggle (mode)
  "Enable Visual MODE if this is not the current mode.
Otherwise disable Visual mode."
  (if (eq vimpulse-visual-mode mode)
      (vimpulse-visual-mode -1)
    (vimpulse-visual-activate mode)))

(defun vimpulse-visual-activate-normal ()
  "Enable Visual selection."
  (interactive)
  (vimpulse-visual-activate 'normal)
  (message "-- VISUAL --"))

(defun vimpulse-visual-activate-line ()
  "Enable Visual Line selection."
  (interactive)
  (vimpulse-visual-activate 'line)
  (message "-- VISUAL LINE --"))

(defun vimpulse-visual-activate-block ()
  "Enable Visual Block selection."
  (interactive)
  (vimpulse-visual-activate 'block)
  (message "-- VISUAL BLOCK --"))

(defun vimpulse-visual-toggle-normal ()
  "Toggle Visual selection."
  (interactive)
  (vimpulse-visual-toggle 'normal)
  (when vimpulse-visual-mode
    (message "-- VISUAL --")))

(defun vimpulse-visual-toggle-line ()
  "Toggle Visual Line selection."
  (interactive)
  (vimpulse-visual-toggle 'line)
  (when vimpulse-visual-mode
    (message "-- VISUAL LINE --")))

(defun vimpulse-visual-toggle-block ()
  "Toggle Visual Block selection."
  (interactive)
  (vimpulse-visual-toggle 'block)
  (when vimpulse-visual-mode
    (message "-- VISUAL BLOCK --")))

;;; Visualization

(defun vimpulse-mark-active (&optional force)
  "Return t if mark is meaningfully active.
That is, `mark-active' is t, it's not about to be deactivated,
and there is a Transient Mark mode (or similar) to handle it."
  (cond
   ((featurep 'xemacs)
    (region-exists-p))
   (force
    (and (boundp 'mark-active)
         mark-active))
   (t
    (and (boundp 'transient-mark-mode)
         transient-mark-mode
         (or (not (boundp 'deactivate-mark))
             (not deactivate-mark))
         (boundp 'mark-active)
         mark-active))))

(defun vimpulse-deactivate-mark (&optional now)
  "Don't deactivate mark in Visual mode."
  (cond
   ((and vimpulse-visual-mode
         (not (eq 'block vimpulse-visual-mode)))
    nil)
   (t
    (vimpulse-deactivate-region now))))

(fset 'viper-deactivate-mark 'vimpulse-deactivate-mark)
(fset 'vimpulse-activate-mark 'vimpulse-activate-region)

(defun vimpulse-transient-mark (&optional arg)
  "Enable Transient Mark mode (and Cua mode) if not already enabled.
 Enable forcefully with positive ARG. Disable with negative ARG."
  (setq deactivate-mark nil)
  (let (deactivate-mark)
    (cond
     ;; Disable Transient Mark/Cua
     ((and (integerp arg) (> 1 arg))
      (and (fboundp 'cua-mode)
           cua-mode
           (cua-mode -1))
      (and (fboundp 'transient-mark-mode)
           transient-mark-mode
           (transient-mark-mode -1))
      (and (boundp 'zmacs-regions)
           (setq zmacs-regions nil)))
     ;; Enable Transient Mark/Cua
     (t
      (unless vimpulse-visual-vars-alist
        (when (boundp 'transient-mark-mode)
          (add-to-list 'vimpulse-visual-vars-alist
                       (cons 'transient-mark-mode
                             transient-mark-mode)))
        (when (boundp 'cua-mode)
          (add-to-list 'vimpulse-visual-vars-alist
                       (cons 'cua-mode cua-mode))))
      (cond
       ((and (fboundp 'cua-mode)
             (vimpulse-visual-before (eq cua-mode t))
             (or (not cua-mode) (numberp arg)))
        (cua-mode 1))
       ((and (fboundp 'transient-mark-mode)
             (or (not transient-mark-mode) (numberp arg)))
        (transient-mark-mode 1))
       ((and (boundp 'zmacs-regions)
             (or (not zmacs-regions) (numberp arg)))
        (setq zmacs-regions t)))))))

(defun vimpulse-transient-restore ()
  "Restore Transient Mark mode to what is was before Visual mode.
 Also restores Cua mode."
  (when vimpulse-visual-vars-alist
    (when (boundp 'transient-mark-mode)
      (if (vimpulse-visual-before transient-mark-mode)
          (transient-mark-mode 1)
        (transient-mark-mode -1)))
    (when (boundp 'cua-mode)
      (if (vimpulse-visual-before cua-mode)
          (cua-mode 1)
        (cua-mode -1)))
    (when (boundp 'zmacs-regions)
      (let ((oldval (vimpulse-visual-before zmacs-regions)))
        (setq zmacs-regions oldval)))))

(defmacro vimpulse-visual-before (&rest body)
  "Evaluate BODY with original system values from before Visual mode.
This is based on `vimpulse-visual-vars-alist'."
  `(let ,(mapcar (lambda (elt)
                   `(,(car elt) (quote ,(cdr elt))))
                 vimpulse-visual-vars-alist)
     ,@body))

(defun vimpulse-move-to-column (column &optional dir force)
  "Move point to column COLUMN in the current line.
Places point at left of the tab character (at the right
if DIR is non-nil) and returns point.
If `vimpulse-visual-block-untabify' is non-nil, then
tabs are changed to spaces. (FORCE untabifies regardless.)"
  (interactive "p")
  (if (or vimpulse-visual-block-untabify force)
      (move-to-column column t)
    (move-to-column column)
    (when (or (not dir) (and (numberp dir) (> 1 dir)))
      (when (< column (current-column))
        (unless (bolp)
          (backward-char)))))
  (point))

(defun vimpulse-visual-beginning (&optional mode force)
  "Return beginning of Visual selection,
based on `point', `mark' and `vimpulse-visual-mode'.
The Visual mode may be specified explicitly with MODE,
which must be one of `normal', `line' and `block'.

In Normal mode, return beginning of region.
In Line mode, return beginning of first line.
In Block mode, return upper opposite corner of rectangle.

If Emacs' region is already expanded to the Visual selection,
return beginning of region. This can be overridden with FORCE.

See also `vimpulse-visual-end'."
  (save-excursion
    (setq mode (or mode vimpulse-visual-mode))
    (cond
     ;; Region is already expanded
     ((and vimpulse-visual-region-expanded (not force))
      (min (point) (or (mark t) 1)))
     ;; Upper opposite corner of block selection
     ((eq 'block mode)
      (let* ((start (min (point) (or (mark t) 1)))
             (end   (max (point) (or (mark t) 1)))
             (start-col (progn
                          (goto-char start)
                          (current-column)))
             (end-col   (save-excursion
                          (goto-char end)
                          (current-column))))
        (if (or (< start-col end-col)
                (and (= start-col end-col)
                     (save-excursion
                       (goto-char end)
                       (not (eolp)))))
            start
          (if (eolp) start (1+ start)))))
     ;; Beginning of first line
     ((eq 'line mode)
      (when (mark t)
        (goto-char (min (point) (mark t))))
      (cond
       ((and (boundp 'visual-line-mode) visual-line-mode)
        (beginning-of-visual-line)
        (point))
       (t
        (line-beginning-position))))
     ;; Beginning of region
     (t
      (min (point) (or (mark t) 1))))))

(defun vimpulse-visual-end (&optional mode force)
  "Return end of Visual selection,
based on `point', `mark' and `vimpulse-visual-mode'.
The Visual mode may be specified explicitly with MODE,
which must be one of `normal', `line' and `block'.

In Normal mode, return end of region plus one character.
In Line mode, return end of last line, including newline.
In Block mode, return lower opposite corner of rectangle.

If Emacs' region is already expanded to the Visual selection,
return end of region. This can be overridden with FORCE.

See also `vimpulse-visual-beginning'."
  (save-excursion
    (setq mode (or mode vimpulse-visual-mode))
    (cond
     ;; Region is already expanded
     ((and vimpulse-visual-region-expanded (not force))
      (max (point) (or (mark t) 1)))
     ((eq 'block mode)
      ;; Lower opposite corner of block selection
      (let* ((start (min (point) (or (mark t) 1)))
             (end   (max (point) (or (mark t) 1)))
             (start-col (save-excursion
                          (goto-char start)
                          (current-column)))
             (end-col   (progn
                          (goto-char end)
                          (current-column))))
        (if (<= start-col end-col)
            (if (eolp) end (1+ end))
          end)))
     ;; End of last line (including newline)
     ((eq 'line mode)
      (when (mark t)
        (goto-char (max (point) (mark t))))
      (cond
       ((and (boundp 'visual-line-mode) visual-line-mode)
        (end-of-visual-line)
        (condition-case nil
            (forward-char)
          (error nil))
        (point))
       (t
        (line-beginning-position 2))))
     ;; End of region plus one character (but not at end of line)
     (t
      (if (save-excursion
            (goto-char (max (point) (or (mark t) 1)))
            (and (eolp)
                 (not (bolp))))
          (max (point) (or (mark t) 1))
        (1+ (max (point) (or (mark t) 1))))))))

(defun vimpulse-visual-range ()
  "Return Visual selection range (BEG END)."
  (if vimpulse-visual-mode
      (list (vimpulse-visual-beginning)
            (vimpulse-visual-end))
    (list (or (region-beginning) (point))
          (or (region-end) (point)))))

(defun vimpulse-visual-select (beg end &optional widen)
  "Visually select text from BEG to END.
Return nil if selection is unchanged. If WIDEN is non-nil, only
modify selection if it does not already encompass BEG and END.

Under the hood, this function changes Emacs' `point' and `mark'.
The boundaries of the Visual selection are deduced from these and
the current Visual mode via `vimpulse-visual-beginning' and
`vimpulse-visual-end'."
  (let (mark-active)
    ;; `vimpulse-visual-end' is always 1 larger than region's end
    ;; so that the character under the cursor is selected.
    ;; Therefore, subtract 1 from END (but avoid END < BEG).
    (vimpulse-set-region (min beg end)
                         (max (min beg end)
                              (1- (max beg end)))
                         widen)))

(defun vimpulse-visual-expand-region (&optional mode no-trailing-newline)
  "Expand Emacs region to Visual selection.
If NO-TRAILING-NEWLINE is t and selection ends with a newline,
exclude that newline from the region."
  (let (beg end mark-active)
    (setq beg (vimpulse-visual-beginning mode)
          end (vimpulse-visual-end mode))
    (when no-trailing-newline
      (save-excursion
        (goto-char end)
        (and (bolp) (not (bobp))
             (setq end (max beg (1- end))))))
    (setq vimpulse-visual-region-expanded t)
    (vimpulse-set-region beg end)))

(defun vimpulse-visual-contract-region (&optional keep-point)
  "Opposite of `vimpulse-visual-expand-region'.
I.e., the resulting Visual selection is equivalent to the former
Emacs region. If KEEP-POINT is t, does not move point.
Return nil if selection is unchanged."
  (let ((opoint (point)) (omark (mark t)))
    (setq vimpulse-visual-region-expanded nil)
    (vimpulse-visual-select (region-beginning) (region-end))
    (when keep-point (goto-char opoint))
    (not (and (= opoint (point))
              (= omark  (mark t))))))

(defun vimpulse-visual-restore ()
  "Restore previous selection."
  (interactive)
  (setq vimpulse-visual-region-expanded nil)
  (let ((last vimpulse-visual-last))
    (cond
     ;; If no previous selection, try a quick C-x C-x
     ((or (not vimpulse-visual-point)
          (not vimpulse-visual-mark))
      (vimpulse-activate-mark nil)
      (vimpulse-visual-mode 1))
     (t
      (unless vimpulse-visual-mode
        (cond
         ((eq 'line last)
          (vimpulse-visual-activate-line))
         ((eq 'block last)
          (vimpulse-visual-activate-block))
         (t                             ; normal
          (vimpulse-visual-activate-normal))))
      (set-mark vimpulse-visual-mark)
      (goto-char vimpulse-visual-point)
      (when (eq 'insert last)
        (vimpulse-visual-contract-region))
      (vimpulse-visual-highlight)))))

(defun vimpulse-visual-markers (&optional point mark)
  "Refresh `vimpulse-visual-point' and `vimpulse-visual-mark'."
  (setq mark  (or mark (mark t) 1)
        point (or point
                  ;; If the cursor has somehow gotten to the very end
                  ;; of the line, where it shouldn't be, fix it
                  (if (and (eolp) (not (bolp)))
                      (1- (point))
                    (point))))
  (viper-move-marker-locally 'vimpulse-visual-point point)
  (viper-move-marker-locally 'vimpulse-visual-mark  mark)
  (set-marker-insertion-type vimpulse-visual-point
                             (<= point mark))
  (set-marker-insertion-type vimpulse-visual-mark
                             (> point mark)))

(defun vimpulse-visual-highlight (&optional arg)
  "Highlight Visual selection, depending on region and Visual mode.
With negative ARG, removes highlighting."
  (cond
   ((and (numberp arg) (> 1 arg))
    (when (viper-overlay-live-p vimpulse-visual-overlay)
      (vimpulse-delete-overlay vimpulse-visual-overlay))
    (mapcar 'vimpulse-delete-overlay vimpulse-visual-block-overlays)
    (setq vimpulse-visual-block-overlays nil)
    ;; Clean up unreferenced overlays
    (dolist (overlay (vimpulse-overlays-at (point)))
      (when (eq (vimpulse-region-face) (viper-overlay-get overlay 'face))
        (vimpulse-delete-overlay overlay))))
   ((eq 'block vimpulse-visual-mode)
    ;; Remove any normal/line highlighting
    (when (viper-overlay-live-p vimpulse-visual-overlay)
      (vimpulse-delete-overlay vimpulse-visual-overlay))
    ;; Block highlighting isn't perfect
    (condition-case nil
        (vimpulse-visual-highlight-block
         (vimpulse-visual-beginning)
         (vimpulse-visual-end))
      (error nil)))
   (vimpulse-visual-mode                ; normal or line
    (let ((beg (vimpulse-visual-beginning))
          (end (vimpulse-visual-end)))
      ;; Remove any block highlighting
      (mapcar 'vimpulse-delete-overlay vimpulse-visual-block-overlays)
      (setq vimpulse-visual-block-overlays nil)
      ;; Reuse overlay if possible
      (if (viper-overlay-live-p vimpulse-visual-overlay)
          (viper-move-overlay vimpulse-visual-overlay beg end)
        (setq vimpulse-visual-overlay
              (vimpulse-make-overlay beg end nil t))
        (viper-overlay-put vimpulse-visual-overlay
                           'face (vimpulse-region-face))
        (viper-overlay-put vimpulse-visual-overlay
                           'priority 99))))))

(defun vimpulse-visual-highlight-block (beg end)
  "Highlight rectangular region from BEG to END.
We do this by putting an overlay on each line within the
rectangle. Each overlay extends across all the columns of the
rectangle. We try to reuse overlays where possible because this
is more efficient and results in less flicker.

Adapted from: `rm-highlight-rectangle' in rect-mark.el."
  (let ((opoint (point))                ; remember point
        (omark  (mark t))               ; remember mark
        (old vimpulse-visual-block-overlays)
        beg-col end-col new nlines overlay window-beg window-end)
    ;; Calculate the rectangular region represented by BEG and END,
    ;; but put BEG in the north-west corner and END in the south-east
    ;; corner if not already there
    (save-excursion
      (setq beg-col (save-excursion (goto-char beg)
                                    (current-column))
            end-col (save-excursion (goto-char end)
                                    (current-column)))
      (when (>= beg-col end-col)
        (if (= beg-col end-col)
            (setq end-col (1+ end-col))
          (setq beg-col (prog1 end-col
                          (setq end-col beg-col))))
        (setq beg (save-excursion (goto-char beg)
                                  (vimpulse-move-to-column beg-col)
                                  (point))
              end (save-excursion (goto-char end)
                                  (vimpulse-move-to-column end-col 1)
                                  (point))))
      ;; Force a redisplay so we can do reliable
      ;; windows BEG/END calculations
      (sit-for 0)
      (setq window-beg (max (window-start) beg)
            window-end (min (window-end) (1+ end))
            nlines (count-lines window-beg
                                (min window-end (point-max))))
      ;; Iterate over those lines of the rectangle which are
      ;; visible in the currently selected window
      (goto-char window-beg)
      (dotimes (i nlines)
        (let (row-beg row-end bstring astring)
          ;; Beginning of row
          (vimpulse-move-to-column beg-col)
          (when (> beg-col (current-column))
            ;; Prepend overlay with virtual spaces if we are unable to
            ;; move directly to the first column
            (setq bstring
                  (propertize
                   (make-string
                    (- beg-col (current-column)) ?\ )
                   'face
                   (or (get-text-property (1- (point)) 'face)
                       'default))))
          (setq row-beg (point))
          ;; End of row
          (vimpulse-move-to-column end-col)
          (when (> end-col (current-column))
            ;; Append overlay with virtual spaces if we are unable to
            ;; move directly to the last column
            (setq astring
                  (propertize
                   (make-string
                    (if (= row-beg (point))
                        (- end-col beg-col)
                      (- end-col (current-column)))
                    ?\ ) 'face (vimpulse-region-face)))
            ;; Place cursor on one of the virtual spaces
            ;; (only works in GNU Emacs)
            (if (= row-beg opoint)
                (put-text-property
                 0 (min (length astring) 1)
                 'cursor t astring)
              (put-text-property
               (max 0 (1- (length astring))) (length astring)
               'cursor t astring)))
          (setq row-end (min (point) (line-end-position)))
          ;; XEmacs bug: zero-length extents display
          ;; end-glyph before start-glyph
          (and (featurep 'xemacs)
               bstring astring
               (= row-beg row-end)
               (setq bstring (prog1 astring
                               (setq astring bstring))))
          ;; Trim old leading overlays
          (while (and old
                      (setq overlay (car old))
                      (< (viper-overlay-start overlay) row-beg)
                      (/= (viper-overlay-end overlay) row-end))
            (vimpulse-delete-overlay overlay)
            (setq old (cdr old)))
          ;; Reuse an overlay if possible, otherwise create one
          (cond
           ((and old (setq overlay (car old))
                 (or (= (viper-overlay-start overlay) row-beg)
                     (= (viper-overlay-end overlay) row-end)))
            (viper-move-overlay overlay row-beg row-end)
            (vimpulse-overlay-before-string overlay bstring)
            (vimpulse-overlay-after-string overlay astring)
            (setq new (cons overlay new)
                  old (cdr old)))
           (t
            (setq overlay (vimpulse-make-overlay row-beg row-end))
            (vimpulse-overlay-before-string overlay bstring)
            (vimpulse-overlay-after-string overlay astring)
            (viper-overlay-put overlay 'face (vimpulse-region-face))
            (viper-overlay-put overlay 'priority 99)
            (setq new (cons overlay new)))))
        (forward-line 1))
      ;; Trim old trailing overlays
      (mapcar 'vimpulse-delete-overlay old)
      (setq vimpulse-visual-block-overlays (nreverse new)))))

(defun vimpulse-visual-pre-command ()
  "Run before each command in Visual mode."
  (when vimpulse-visual-mode
    ;; Refresh Visual restore markers and marks
    (vimpulse-visual-markers)
    (set-register (viper-int-to-char (1+ (- ?y ?a)))
                  (vimpulse-visual-beginning))
    (set-register (viper-int-to-char (1+ (- ?z ?a)))
                  (vimpulse-visual-end))
    (cond
     ;; Movement command: don't expand region
     ((vimpulse-movement-cmd-p this-command)
      (setq vimpulse-visual-region-expanded nil))
     (t
      ;; Add whitespace if necessary for making a rectangle
      (and (eq 'block vimpulse-visual-mode)
           (vimpulse-visual-block-add-whitespace))
      (vimpulse-visual-expand-region
       ;; If in Line mode, don't include trailing newline
       ;; unless the command has real need of it
       nil (and (eq 'line vimpulse-visual-mode)
                (not (vimpulse-needs-newline-p this-command))))))))

(defun vimpulse-visual-post-command ()
  "Run after each command in Visual mode."
  (cond
   (vimpulse-visual-mode
    ;; Quitting: exit to vi (command) mode
    (cond
     (quit-flag                         ; C-g
      (vimpulse-visual-mode -1))
     ((eq 'keyboard-quit this-command)
      (vimpulse-visual-mode -1))
     ((and (not (vimpulse-mark-active))
           (not (eq 'block vimpulse-visual-mode)))
      (vimpulse-visual-mode -1))
     ;; Region was expanded, so contract it
     (vimpulse-visual-region-expanded
      (when (eq 'block vimpulse-visual-mode)
        (vimpulse-visual-block-cleanup-whitespace))
      (if (eq 'line vimpulse-visual-mode)
          (vimpulse-visual-restore)
        (vimpulse-visual-contract-region))
      (vimpulse-visual-highlight))
     (t
      (vimpulse-visual-highlight))))
   ;; Not in the Visual state, but maybe the mark
   ;; was activated in vi (command) state?
   ((and (vimpulse-mark-active)
         (eq 'vi-state viper-current-state)
         (if (boundp 'deactivate-mark) (not deactivate-mark) t))
    (vimpulse-visual-mode 1))))

(defun vimpulse-visual-deactivate-hook ()
  "Hook run when mark is deactivated in Visual mode."
  (when vimpulse-visual-mode
    (and (not (vimpulse-mark-active))
         (not (vimpulse-movement-cmd-p this-command))
         (vimpulse-visual-mode -1))))

(add-hook 'pre-command-hook 'vimpulse-visual-pre-command)
(add-hook 'post-command-hook 'vimpulse-visual-post-command)
(if (featurep 'xemacs)
    (add-hook 'zmacs-deactivate-region-hook
              'vimpulse-visual-deactivate-hook)
  (add-hook 'deactivate-mark-hook 'vimpulse-visual-deactivate-hook))

;; Advise viper-intercept-ESC-key to exit Visual mode with ESC
(defadvice viper-intercept-ESC-key
  (around vimpulse-ESC-exit-visual-mode activate)
  "Exit Visual mode with ESC."
  (let ((viper-ESC-moves-cursor-back (not (vimpulse-mark-active)))
        deactivate-mark)
    (if (and vimpulse-visual-mode
             (not (input-pending-p)))
        (vimpulse-visual-mode -1)
      ad-do-it)))

(defadvice viper-Put-back (around vimpulse-visual activate)
  "Delete selection before pasting in Visual mode."
  (let (inserted-text replaced-text mode)
    (setq yank-window-start (window-start))
    (cond
     (vimpulse-visual-mode
      (setq mode vimpulse-visual-mode)
      (unless (eq 'block mode)
        ;; Add replaced text to the kill-ring before the current kill
        (setq inserted-text (current-kill 0))
        (setq replaced-text
              (buffer-substring (region-beginning) (region-end)))
        (kill-new replaced-text t)
        (kill-new inserted-text))
      (vimpulse-visual-delete (region-beginning) (region-end) t)
      (when (and (eq 'normal mode)
                 (not (bolp))
                 (viper-end-with-a-newline-p inserted-text))
        (newline))
      (when (and (eq 'line mode)
                 (not (viper-end-with-a-newline-p inserted-text)))
        (save-excursion (newline))))
     ((vimpulse-mark-active)
      (delete-region (region-beginning) (region-end))))
    (if (and killed-rectangle
             kill-ring
             (eq (current-kill 0)
                 (get 'killed-rectangle 'previous-kill)))
        (yank-rectangle)
      ad-do-it)))

(defadvice viper-put-back (around vimpulse-visual activate)
  "Delete selection before pasting in Visual mode."
  (setq yank-window-start (window-start))
  (cond
   (vimpulse-visual-mode
    (viper-Put-back arg))
   ((vimpulse-mark-active)
    (viper-Put-back arg))
   (t
    (if (and killed-rectangle
             kill-ring
             (eq (current-kill 0)
                 (get 'killed-rectangle 'previous-kill)))
        (yank-rectangle)
      ad-do-it))))

;; Viper's larger movement commands use the mark to store the previous
;; position, which is fine and useful when the mark isn't active. When
;; it is, however, it has the effect of remaking the region.
(defadvice push-mark (around vimpulse-visual-mode activate)
  (unless (and vimpulse-visual-mode
               ;; Note: if you really need to call `push-mark'
               ;; in proximity with these commands (e.g., in a hook),
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

;; Block selection disables Transient Mark mode
(defadvice deactivate-mark (after vimpulse-visual activate)
  "Deactivate Visual Block mode."
  (when (eq 'block vimpulse-visual-mode)
    (vimpulse-visual-mode -1)))

(defmacro vimpulse-visual-mouse-advice (cmd)
  "Advise mouse command CMD to enable Visual mode."
  `(defadvice ,cmd (around vimpulse-visual activate)
     "Enable Visual mode in vi (command) state."
     (let ((w (posn-window (event-start (ad-get-arg 0)))))
       (cond
        ;; If Visual mode is enabled in the window clicked in,
        ;; adjust region afterwards
        ((with-selected-window w
           vimpulse-visual-mode)
         (vimpulse-visual-highlight -1)
         ad-do-it
         (when (eq w (selected-window))
           (vimpulse-visual-contract-region t)
           (vimpulse-visual-highlight)))
        ;; Otherwise, if in vi (command) state, enable Visual mode
        ((with-selected-window w
           (eq 'vi-state viper-current-state))
         ad-do-it
         (when (eq w (selected-window))
           (cond
            (vimpulse-visual-mode
             (vimpulse-visual-contract-region t))
            ((vimpulse-mark-active)
             (vimpulse-visual-mode 1)
             (setq vimpulse-visual-region-expanded nil)
             (vimpulse-visual-contract-region t)))))
        (t
         ad-do-it)))))

(vimpulse-visual-mouse-advice mouse-drag-region)
(vimpulse-visual-mouse-advice mouse-save-then-kill)

(defadvice mouse-show-mark (before vimpulse-visual activate)
  "Refresh highlighting of Visual selection."
  (when vimpulse-visual-mode
    (vimpulse-visual-highlight)))

;;; Lists

(defvar vimpulse-movement-cmds
  '(backward-char backward-list backward-paragraph backward-sentence
    backward-sexp backward-up-list backward-word beginning-of-buffer
    beginning-of-defun beginning-of-line beginning-of-visual-line
    cua-cancel down-list end-of-buffer end-of-defun end-of-line
    end-of-visual-line exchange-point-and-mark forward-char
    forward-list forward-paragraph forward-sentence forward-sexp
    forward-word keyboard-quit mouse-drag-region mouse-save-then-kill
    mouse-set-point mouse-set-region move-beginning-of-line
    move-end-of-line next-line previous-line scroll-down scroll-up
    undo up-list vimpulse-end-of-previous-word
    vimpulse-goto-definition vimpulse-goto-first-line
    vimpulse-goto-first-line vimpulse-visual-block-rotate
    vimpulse-visual-exchange-corners vimpulse-visual-restore
    vimpulse-visual-toggle-block vimpulse-visual-toggle-line
    vimpulse-visual-toggle-normal viper-backward-Word
    viper-backward-char viper-backward-paragraph
    viper-backward-sentence viper-backward-word
    viper-beginning-of-line viper-end-of-Word viper-end-of-word
    viper-exec-mapped-kbd-macro viper-find-char-backward
    viper-find-char-forward viper-forward-Word viper-forward-char
    viper-forward-paragraph viper-forward-sentence viper-forward-word
    viper-goto-char-backward viper-goto-char-forward viper-goto-eol
    viper-goto-line viper-insert viper-intercept-ESC-key
    viper-line-to-bottom viper-line-to-middle viper-line-to-top
    viper-next-line viper-previous-line viper-search-Next
    viper-search-backward viper-search-forward viper-search-next
    viper-window-bottom viper-window-middle viper-window-top)
  "List of commands that move point.
If listed here, the region is not expanded to the
Visual selection before the command is executed.")

(defvar vimpulse-newline-cmds
  '(cua-copy-region cua-cut-region cua-delete-region delete-region
    exchange-point-and-mark execute-extended-command kill-region
    kill-ring-save viper-put-back viper-Put-back
    vimpulse-visual-change vimpulse-visual-delete
    vimpulse-visual-exchange-corners vimpulse-visual-yank)
  "List of commands which needs the trailing newline in Visual Line mode.
In most cases, it's more useful not to include this newline in
the region acted on.")

(defun vimpulse-movement-cmd-p (command)
  "Whether COMMAND is a \"movement\" command.
That is, whether it is listed in `vimpulse-movement-cmds'."
  ;; We use `member' rather than `memq' to allow lambdas
  (member command vimpulse-movement-cmds))

(defun vimpulse-needs-newline-p (command)
  "Whether COMMAND needs trailing newline in Visual Line mode.
In most cases (say, when wrapping the selection in a skeleton),
it is more useful to exclude the last newline from the region."
  (member command vimpulse-newline-cmds))

;;; Destructive commands

(defun vimpulse-visual-delete (beg end &optional dont-save)
  "Kills the Visual selection to the kill-ring.
If DONT-SAVE is non-nil, just delete it."
  (interactive "r")
  (let ((length (- end beg)))
    (cond
     (dont-save
      (cond
       ((eq 'block vimpulse-visual-mode)
        (delete-rectangle beg end)
        (goto-char (min vimpulse-visual-point vimpulse-visual-mark)))
       (t
        (delete-region beg end)
        (goto-char beg)))
      (vimpulse-visual-mode -1))
     ((or (eq 'normal vimpulse-visual-mode)
          (and (boundp 'visual-line-mode) visual-line-mode
               (not (eq 'block vimpulse-visual-mode))))
      (viper-prefix-arg-com ?r 1 ?d)
      (viper-set-destructive-command
       (list 'viper-forward-char
             length ?d viper-use-register nil nil))
      (vimpulse-visual-mode -1))
     ((eq 'line vimpulse-visual-mode)
      (setq length (count-lines beg end))
      (goto-char (min vimpulse-visual-point vimpulse-visual-mark))
      (viper-line (cons length ?D))
      (vimpulse-visual-mode -1))
     ((eq 'block vimpulse-visual-mode)
      ;; Associate the rectangle with the last entry in the kill-ring
      (unless kill-ring
        (copy-region-as-kill beg end))
      (kill-rectangle beg end)
      (put 'killed-rectangle 'previous-kill (current-kill 0))
      (goto-char (min vimpulse-visual-point vimpulse-visual-mark))
      (vimpulse-visual-mode -1)))))

(defun vimpulse-visual-change (beg end &optional dont-save)
  "Change the Visual selection to the kill-ring.
If DONT-SAVE is non-nil, just delete it."
  (interactive "r")
  (let ((length (- end beg))
        (mode vimpulse-visual-mode))
    (vimpulse-visual-delete beg end dont-save)
    (setq length (min length (1- (- (buffer-size) (point)))))
    (cond
     ((or (eq 'normal mode)
          (and (boundp 'visual-line-mode) visual-line-mode
               (not (eq 'block mode))))
      (let (viper-d-com)
        (goto-char (max vimpulse-visual-point vimpulse-visual-mark))
        (viper-insert nil))
      (setcar (nthcdr 1 viper-d-com) length)
      (setcar (nthcdr 2 viper-d-com) ?c))
     ((eq 'line mode)
      (let (viper-d-com)
        (if (= (point) vimpulse-visual-point)
            (viper-Open-line nil)
          (viper-open-line nil))) ; if on last line, insert below
      (setcar (nthcdr 2 viper-d-com) ?C))
     ((eq 'block mode)
      (goto-char
       (vimpulse-visual-create-coords
        'block ?i
        (min vimpulse-visual-point vimpulse-visual-mark)
        (1+ (max vimpulse-visual-point vimpulse-visual-mark))))
      (viper-insert nil)))
    (setq vimpulse-visual-last 'insert)))

(defun vimpulse-visual-replace-region (beg end &optional arg)
  "Replace all selected characters with ARG."
  (interactive "r")
  (cond
   ((memq vimpulse-visual-mode '(normal line))
    (goto-char beg)
    (viper-replace-char arg)
    (let ((c (char-after (point))))
      (dotimes (i (- end beg))
        (cond
         ((member (char-after (point)) '(?\r ?\n))
          (forward-char))
         (t (delete-char 1)
            (insert c))))))
   ((eq 'block vimpulse-visual-mode)
    (goto-char beg)
    (viper-replace-char arg)
    (let* ((c (char-after (point)))
           (begin-col (current-column))
           (len (- (save-excursion
                     (goto-char end)
                     (current-column))
                   begin-col)))
      (while (< (point) end)
        (vimpulse-move-to-column begin-col)
        (let ((n 0))
          (while (and (< n len)
                      (not (member (char-after (point))
                                   '(?\r ?\n))))
            (delete-char 1)
            (insert c)
            (setq n (1+ n))))
        (forward-line))))
   (t
    (error "Not in Visual mode")))
  (vimpulse-visual-mode -1)
  (goto-char beg))

;; These two functions implement insertion at the beginning/end
;; of the Visual selection
(defun vimpulse-visual-insert (beg end &optional arg)
  "Enter Insert state at beginning of Visual selection."
  (interactive "r\nP")
  (let (deactivate-mark)
    (cond
     ((eq 'block vimpulse-visual-mode)
      (vimpulse-visual-block-rotate 'upper-left beg end)
      (setq beg (vimpulse-visual-beginning)
            end (vimpulse-visual-end))
      (vimpulse-visual-mode -1)
      (goto-char
       (vimpulse-visual-create-coords 'block ?i beg end))
      (viper-insert arg))
     (t
      (vimpulse-visual-mode -1)
      (push-mark end t t)
      (goto-char beg)
      (viper-insert arg))
     (t
      (error "Not in Visual mode")))))

(defun vimpulse-visual-append (beg end &optional arg)
  "Enter Insert state at end of Visual selection."
  (interactive "r\nP")
  (let (deactivate-mark)
    (cond
     ((eq 'block vimpulse-visual-mode)
      (vimpulse-visual-block-rotate 'upper-left beg end)
      (setq beg (vimpulse-visual-beginning)
            end (vimpulse-visual-end))
      (setq vimpulse-visual-norm-overlay nil)
      (vimpulse-visual-mode -1)
      (goto-char
       (vimpulse-visual-create-coords 'block ?a beg end))
      (viper-append arg))
     (t
      (vimpulse-visual-mode -1)
      (push-mark beg t t)
      (goto-char end)
      (viper-insert arg))
     (t
      (error "Not in Visual mode")))))

(defun vimpulse-visual-make-upcase (beg end)
  "Converts all selected characters to upper case."
  (interactive "r")
  (vimpulse-visual-change-case beg end 'upcase-region))

(defun vimpulse-visual-make-downcase (beg end)
  "Converts all selected characters to lower case."
  (interactive "r")
  (vimpulse-visual-change-case beg end 'downcase-region))

(defun vimpulse-visual-toggle-case (beg end)
  "Toggles the case of all selected characters."
  (interactive "r")
  (vimpulse-visual-change-case beg end 'vimpulse-visual-toggle-case-region))

(defun vimpulse-visual-change-case (beg end &optional case-func)
  (setq case-func (or case-func 'vimpulse-visual-toggle-case-region))
  (cond
   ((memq vimpulse-visual-mode '(normal line))
    (funcall case-func beg end))
   ((eq 'block vimpulse-visual-mode)
    (let ((begin-col (save-excursion
                       (goto-char beg)
                       (current-column)))
          (len  (- (save-excursion
                     (goto-char end)
                     (current-column))
                   (save-excursion
                     (goto-char beg)
                     (current-column)))))
      (goto-char beg)
      (while (< (point) end)
        (let ((from (save-excursion
                      (vimpulse-move-to-column begin-col)
                      (point)))
              (to (save-excursion
                    (vimpulse-move-to-column (+ begin-col len))
                    (point))))
          (funcall case-func from to)
          (forward-line)))))
   (t
    (error "Not in Visual mode")))
  (goto-char (vimpulse-visual-block-position 'upper-left beg end))
  (vimpulse-visual-mode -1))

(defun vimpulse-visual-toggle-case-region (beg end)
  "Toggles the case of all characters from BEG to END (exclusive)."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (< beg end)
      (setq c (following-char))
      (delete-char 1 nil)
      (if (eq c (upcase c))
          (insert-char (downcase c) 1)
        (insert-char (upcase c) 1))
      (setq beg (1+ beg)))))

(defun vimpulse-visual-join (beg end)
  "Joins the selected lines."
  (interactive "r")
  (when vimpulse-visual-mode
    (vimpulse-visual-mode -1)
    (goto-char beg)
    (viper-join-lines (count-lines beg end))))

;; Currently, I don't know how to take the argument ARG
;; into the Repeat-command
(defun vimpulse-visual-shift-left (beg end &optional arg)
  "Shift all selected lines to the left."
  (interactive "r\nP")
  (setq arg (viper-p-val arg))
  (vimpulse-visual-mode -1)
  (vimpulse-push-buffer-undo-list-mark)
  (let ((nlines (1- (count-lines beg end))))
    (dotimes (i arg)
      (goto-char beg)
      (viper-next-line (cons nlines ?<)))
    (vimpulse-connect-undos)))

(defun vimpulse-visual-shift-right (beg end &optional arg)
  "Shift all selected lines to the right."
  (interactive "r\nP")
  (setq arg (viper-p-val arg))
  (vimpulse-visual-mode -1)
  (vimpulse-push-buffer-undo-list-mark)
  (let ((nlines (1- (count-lines beg end))))
    (dotimes (i (or arg 1))
      (goto-char beg)
      (viper-next-line (cons nlines ?>)))
    (vimpulse-connect-undos)))

;;; Non-destructive commands

(defun vimpulse-visual-yank (beg end)
  "Save the Visual selection in the kill-ring."
  (interactive "r")
  (cond
   ((eq 'block vimpulse-visual-mode)
    (setq killed-rectangle (extract-rectangle beg end))
    ;; Associate the rectangle with the last entry in the kill-ring
    (unless kill-ring
      (copy-region-as-kill beg end))
    (put 'killed-rectangle 'previous-kill (current-kill 0))
    (vimpulse-visual-block-rotate 'upper-left beg end)
    (setq beg (vimpulse-visual-beginning)
          end (vimpulse-visual-end)))
   (vimpulse-visual-mode
    (viper-prefix-arg-com ?r 1 ?y))
   (t
    (error "Not in Visual mode")))
  (vimpulse-visual-mode -1)
  (goto-char beg))

;;; Block selection

(defun vimpulse-visual-block-position (corner &optional beg end)
  "Return position of Visual Block CORNER.
CORNER may be one of `upper-left', `upper-right', `lower-left'
and `lower-right', or a clockwise number from 0 to 3:

        0---1        upper-left +---+ upper-right
        |   |                   |   |
        3---2        lower-left +---+ lower-right

The rectangle is defined by mark and point, or BEG and END
if specified. The CORNER values `upper', `left', `lower'
and `right' return one of the defining corners.

        upper P---+                    +---M upper
         left |   | lower        lower |   | right
              +---M right         left P---+

Corners 0 and 3 are returned by their left side, corners 1 and 2
by their right side. To place point in one of the corners, use
`vimpulse-visual-block-rotate'.

To go the other way, use `vimpulse-visual-block-corner'."
  (save-excursion
    (setq beg (or beg (vimpulse-visual-beginning 'block))
          end (or end (vimpulse-visual-end 'block)))
    (when (> beg end) (setq beg (prog1 end (setq end beg))))
    (let ((beg-col (progn (goto-char beg)
                          (current-column)))
          (end-col (progn (goto-char end)
                          (current-column)))
          (upper beg) (left beg) (lower end) (right end)
          (upper-left 0) (upper-right 1)
          (lower-left 3) (lower-right 2))
      (when (> beg-col end-col)
        (setq beg-col (prog1 end-col
                        (setq end-col beg-col)))
        (setq left (prog1 right
                     (setq right left))))
      (if (memq corner '(upper left lower right))
          (eval corner)
        (setq corner (mod (eval corner) 4))
        (if (memq corner '(0 1))
            (goto-char beg)
          (goto-char end))
        (if (memq corner '(0 3))
            (vimpulse-move-to-column beg-col)
          (vimpulse-move-to-column end-col))
        (point)))))

(defun vimpulse-visual-block-corner (&optional symbolic pos)
  "Return the current Visual Block corner as a number from 0 to 3.
Corners are numbered clockwise, starting with the upper-left corner.
Return as one of `upper-left', `upper-right', `lower-left' and
`lower-right' if SYMBOLIC is non-nil.

        0---1        upper-left +---+ upper-right
        |   |                   |   |
        3---2        lower-left +---+ lower-right

Specify POS to compare that position, rather than point,
against the corners. The result can be passed to functions
like `vimpulse-visual-block-position' and
`vimpulse-visual-block-rotate'."
  (let ((upper-left 0)
        (upper-right 1)
        (lower-left 3)
        (lower-right 2)
        corner)
    (setq pos (or pos (point)))
    (or (dolist (i '(upper-left lower-left) corner)
          (when (eq pos (vimpulse-visual-block-position i))
            (setq corner i)))
        (progn
          (unless vimpulse-visual-region-expanded
            (setq pos (1+ pos)))
          (dolist (i '(upper-right lower-right) corner)
            (when (eq pos (vimpulse-visual-block-position i))
              (setq corner i)))))
    (if symbolic
        corner
      (eval corner))))

(defun vimpulse-visual-block-rotate (corner &optional beg end)
  "In Visual Block selection, rotate point and mark clockwise.
When called non-interactively, CORNER specifies the corner to
place point in; mark is placed in the opposite corner.

        0---1        upper-left +---+ upper-right
        |   |                   |   |
        3---2        lower-left +---+ lower-right

Corners are numbered clockwise from 0. For better readability,
you may use the symbolic values `upper-left', `upper-right',
`lower-left' and `lower-right'.

This function updates `vimpulse-visual-point' and
`vimpulse-visual-mark' so that \\[vimpulse-visual-restore]
restores the selection with the same rotation."
  (interactive
   (list (if (> 0 (prefix-numeric-value current-prefix-arg))
             (1- (vimpulse-visual-block-corner))
           (1+ (vimpulse-visual-block-corner)))))
  (let ((upper-left 0) (upper-right 1) (lower-left 3) (lower-right 2)
        newmark newpoint newmark-marker newpoint-marker mark-active)
    (setq corner (mod (eval corner) 4))
    (setq newpoint (vimpulse-visual-block-position corner beg end))
    (setq newmark (vimpulse-visual-block-position
                   (mod (+ 2 corner) 4) beg end))
    (if (memq corner '(0 3))
        (setq newmark-marker (1- newmark)
              newpoint-marker newpoint)
      (setq newpoint-marker (1- newpoint)
            newmark-marker newmark))
    (unless vimpulse-visual-region-expanded
      (setq newpoint newpoint-marker
            newmark  newmark-marker))
    (set-mark newmark)
    (goto-char newpoint)
    (vimpulse-visual-markers newpoint-marker newmark-marker)))

(defun vimpulse-visual-exchange-corners ()
  "Rearrange corners in Visual Block mode.

        M---+          +---M
        |   |    =>    |   |
        +---P          P---+

For example, if mark is in the upper left corner and point
in the lower right (see fig.), this function puts mark in
the upper right corner and point in the lower left."
  (interactive)
  (cond
   ((memq vimpulse-visual-mode '(normal line))
    (exchange-point-and-mark))
   ((eq 'block vimpulse-visual-mode)
    (let ((mark-col (save-excursion
                      (goto-char (mark t))
                      (forward-char)
                      (1- (current-column))))
          (point-col (current-column)))
      (set-mark (save-excursion
                  (goto-char (mark t))
                  (vimpulse-move-to-column
                   point-col (< (current-column) point-col))
                  (point)))
      (vimpulse-move-to-column
       mark-col (< (current-column) mark-col))
      (and (eolp) (not (bolp)) (backward-char))))
   (t
    (error "Not in Visual mode"))))

;; Insert whitespace into buffer to handle zero-width rectangles.
;; This isn't ideal and should be replaced with something else.
(defun vimpulse-visual-block-add-whitespace ()
  "Ensure rectangle is at least one column wide.
If the Block selection starts and ends on blank lines, the
resulting rectangle has width zero even if intermediate lines
contain characters. This function inserts a space after `mark'
so that a one-column rectangle can be made. The position of the
space is stored in `vimpulse-visual-norm-overlay' so it can be
removed afterwards with `vimpulse-visual-block-cleanup-whitespace'."
  (save-excursion
    (when (and (eq 'block vimpulse-visual-mode)
               (/= (vimpulse-visual-beginning)
                   (vimpulse-visual-end))
               (save-excursion
                 (goto-char (vimpulse-visual-beginning))
                 (and (bolp) (eolp)))
               (save-excursion
                 (goto-char (vimpulse-visual-end))
                 (and (bolp) (eolp))))
      (goto-char (mark t))
      (insert " ")
      (setq vimpulse-visual-norm-overlay
            (vimpulse-make-overlay (mark t) (1+ (mark t))
                                   nil t nil)))))

(defun vimpulse-visual-block-cleanup-whitespace ()
  "Clean up whitespace inserted by `vimpulse-visual-block-add-whitespace'."
  (when (viper-overlay-live-p vimpulse-visual-norm-overlay)
    (when (= 1 (- (viper-overlay-end   vimpulse-visual-norm-overlay)
                  (viper-overlay-start vimpulse-visual-norm-overlay)))
      (delete-region
       (viper-overlay-start vimpulse-visual-norm-overlay)
       (viper-overlay-end   vimpulse-visual-norm-overlay)))
    (vimpulse-delete-overlay vimpulse-visual-norm-overlay)
    (setq vimpulse-visual-norm-overlay nil)))

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
      (let ((beg-col (save-excursion
                       (goto-char upper-left)
                       (current-column)))
            (end-col (save-excursion
                       (goto-char lower-right)
                       (current-column))))
        ;; Decide if we use the left or right column
        (setq col (max 0 (if (or (eq i-com ?i) (eq i-com ?c))
                             beg-col
                           (1- end-col))))))
    ;; Save the information
    (setq vimpulse-visual-insert-coords
          (list mode i-com upper-left col nlines))
    (save-excursion
      (goto-char upper-left)
      (vimpulse-move-to-column col)
      (point))))

;; Redefinitions of Viper functions to handle Visual block selection,
;; that is, the "update all lines when we hit ESC" part.
;; This function is not in viper-functions-redefinitions.el
;; because its code is closely related to Visual mode.
(defun vimpulse-exit-insert-state ()
  (interactive)
  (viper-change-state-to-vi)
  (when vimpulse-visual-insert-coords
    ;; Get the saved info about the Visual selection
    (let ((mode   (nth 0 vimpulse-visual-insert-coords))
          (i-com  (nth 1 vimpulse-visual-insert-coords))
          (pos    (nth 2 vimpulse-visual-insert-coords))
          (col    (nth 3 vimpulse-visual-insert-coords))
          (nlines (nth 4 vimpulse-visual-insert-coords)))
      (goto-char pos)
      (save-excursion
        (dotimes (i (1- nlines))
          (forward-line 1)
          (let ((cur-col (vimpulse-move-to-column col)))
            ;; If we are in Block mode, this line, but do not hit the
            ;; correct column, we check if we should convert tabs
            ;; and/or append spaces
            (if (and (eq mode 'block)
                     (or (/= col cur-col) ; wrong column or
                         (eolp)))         ; end of line
                (cond ((< col cur-col)    ; we are inside a tab
                       (move-to-column (1+ col) t) ; convert to spaces
                       (move-to-column col t) ; this is needed for ?a
                       (viper-repeat nil))
                      ((and (>= col cur-col) ; we are behind the end
                            (eq i-com ?a))   ; and I-COM is ?a
                       (move-to-column (1+ col) t) ; append spaces
                       (viper-repeat nil)))
              (viper-repeat nil)))))
      (setq vimpulse-visual-insert-coords nil)))
  ;; Update undo-list
  (vimpulse-connect-undos))

(fset 'viper-exit-insert-state 'vimpulse-exit-insert-state)

;;; Key bindings

(define-key viper-vi-basic-map "v" 'vimpulse-visual-toggle-normal)
(define-key viper-vi-basic-map "V" 'vimpulse-visual-toggle-line)
(define-key viper-vi-basic-map "\C-v" 'vimpulse-visual-toggle-block)
(define-key viper-vi-basic-map "\C-p" 'yank-rectangle)
(define-key viper-vi-basic-map "gv" 'vimpulse-visual-restore)

(define-key vimpulse-visual-basic-map "v" 'vimpulse-visual-toggle-normal)
(define-key vimpulse-visual-basic-map "V" 'vimpulse-visual-toggle-line)
(define-key vimpulse-visual-basic-map "\C-v" 'vimpulse-visual-toggle-block)
(define-key vimpulse-visual-basic-map "d" 'vimpulse-visual-delete)
(define-key vimpulse-visual-basic-map "x" 'vimpulse-visual-delete)
(define-key vimpulse-visual-basic-map "D" 'vimpulse-visual-delete)
(define-key vimpulse-visual-basic-map "d" 'vimpulse-visual-delete)
(define-key vimpulse-visual-basic-map "y" 'vimpulse-visual-yank)
(define-key vimpulse-visual-basic-map "Y" 'vimpulse-visual-yank)
(define-key vimpulse-visual-basic-map "u" 'vimpulse-visual-mode)
(define-key vimpulse-visual-basic-map "R" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "r" 'vimpulse-visual-replace-region)
(define-key vimpulse-visual-basic-map "c" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "C" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "s" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "S" 'vimpulse-visual-change)
(define-key vimpulse-visual-basic-map "o" 'exchange-point-and-mark)
(define-key vimpulse-visual-basic-map "O" 'vimpulse-visual-exchange-corners)
(define-key vimpulse-visual-basic-map "I" 'vimpulse-visual-insert)
(define-key vimpulse-visual-basic-map "A" 'vimpulse-visual-append)
(define-key vimpulse-visual-basic-map "U" 'vimpulse-visual-make-upcase)
(define-key vimpulse-visual-basic-map "u" 'vimpulse-visual-make-downcase)
(define-key vimpulse-visual-basic-map "~" 'vimpulse-visual-toggle-case)
(define-key vimpulse-visual-basic-map "J" 'vimpulse-visual-join)
(define-key vimpulse-visual-basic-map "<" 'vimpulse-visual-shift-left)
(define-key vimpulse-visual-basic-map ">" 'vimpulse-visual-shift-right)
(define-key vimpulse-visual-basic-map "=" 'indent-region)
;; Keys that have no effect in Visual mode
(define-key vimpulse-visual-basic-map [remap viper-repeat] 'viper-nil)

(provide 'vimpulse-visual-mode)

