;;;; Operator-Pending mode

;; This provides a framework for combining "motions" and "operators".
;; A motion is any command moving point. An operator is a command
;; acting on the text moved over by a motion.
;;
;; Defining operator commands is similar to defining commands acting
;; on the region. That is, both must have two arguments, BEG and END,
;; and an `interactive' specification that stores the relevant range
;; in those arguments:
;;
;;     (defun foo-region (beg end)
;;       (interactive "r")
;;       ;; Do stuff from BEG to END
;;       )
;;
;;     (defun foo-operator (beg end)
;;       (interactive (vimpulse-range))
;;       ;; Do stuff from BEG to END
;;       )
;;
;; When the latter command is run, `vimpulse-range' will query the
;; user for a motion and determine the resulting range to pass on to
;; the command. (In Visual mode, however, it skips the querying and
;; returns the selection boundaries instead.)
;;
;; While a motion is read from the keyboard, a temporary Viper state,
;; Operator-Pending mode, is entered. This state inherits bindings
;; from the regular vi state, but it may also define its own, for
;; instance text objects. Text objects are like motions, but define a
;; starting point as well as an ending point. They are implemented
;; simply as selection commands.
;;
;; The benefit of a dedicated state when an "operator" is "pending" is
;; code separation. In the original scheme, every Viper motion must
;; manually do the work of deleting/changing/yanking the text moved
;; over, making that action repeatable, etc. The new framework handles
;; everything automatically and orthogonally, enabling the use of
;; plain Emacs movement commands (like S-exp navigation) as motions.
;;
;; In most cases, the motion range is determined by calling the motion
;; just as it would be in vi state. Occasionally, the operator-pending
;; behavior is slightly different. In those cases, a separate command
;; is bound in the Operator-Pending state, and associated with the
;; regular motion with a remap binding.
;;
;; What about all those Viper motions doing everything at once?
;; A couple of compatibility macros tries to separate the
;; operator-pending part from the rest. In the long run, Viper's
;; motions should be rewritten in a more modular way; I'll have to
;; contact Michael Kifer and hear what he thinks about this.
;; For what it's worth, the following code addresses "TODO item #1"
;; in viper.el.

(vimpulse-define-state operator
  "Operator-pending mode is when an operator is pending,
awaiting a motion (after \"d\", \"y\", \"c\", etc.)."
  :id "<OP> "
  :hook '(vimpulse-set-operator-cursor-type)
  :enable '(vimpulse-operator-remap-minor-mode
            (viper-vi-kbd-minor-mode nil)
            vi-state vimpulse-modal-minor-mode)
  (cond
   ((eq 'operator-state viper-current-state)
    (vimpulse-modal-minor-mode 1))
   (t
    (vimpulse-modal-minor-mode -1))))

;; This is a short-lived state, only used for calculating
;; motion ranges. If anything goes wrong and we enter the
;; command loop, exit to vi state immediately.
(defun vimpulse-operator-exit-hook ()
  "Exit Operator-Pending mode."
  (when (eq 'operator-state viper-current-state)
    (viper-change-state-to-vi)))

(add-hook 'pre-command-hook 'vimpulse-operator-exit-hook)
(add-hook 'post-command-hook 'vimpulse-operator-exit-hook)

;; We place all remap bindings in a keymap of their own.
;; This enables Visual mode only to inherit text object
;; bindings from Operator-Pending mode, not any remapping.
(defvar vimpulse-operator-remap-map (make-sparse-keymap))

(defvar vimpulse-operator-remap-alist nil
  "Association list of command remappings in Operator-Pending mode.")

(define-minor-mode vimpulse-operator-remap-minor-mode
  "Minor mode of bindings overwritten by `vimpulse-map' et al."
  :keymap vimpulse-operator-remap-map)

(put 'vimpulse-operator-remap-map
     'remap-alist 'vimpulse-operator-remap-alist)

(defvar vimpulse-this-operator nil
  "Current operator.
In general, motions and operators are orthogonal, with some exceptions:
\"cw\" and \"dw\" work on slightly different ranges, for example.
Motions can check this variable if they need to know what
operator receives their range. See also `vimpulse-this-motion'.")

(defvar vimpulse-this-motion nil
  "Current motion.
In general, motions and operators are orthogonal, with some exceptions:
\"cc\" may indent the current line while \"cw\" may not, for example.
Operators may check this variable if they need to know what
motion produced the current range. See also `vimpulse-this-operator'.")

(defvar vimpulse-this-count nil
  "Current count.")

(defvar vimpulse-this-motion-type nil
  "Current range type.
May be `block', `line', `normal' or nil.")

(defvar vimpulse-last-range-type nil
  "Last repeated range type.
May be `block', `line', `normal' or nil.")

(defvar vimpulse-last-operator nil
  "Last repeated operator.
Used by `vimpulse-operator-repeat'.")

(defvar vimpulse-last-motion nil
  "Last repeated motion.
Used by `vimpulse-operator-repeat'.")

(defcustom vimpulse-want-operator-pending-cursor t
  "Whether the cursor changes in Operator-Pending mode, on by default."
  :group 'vimpulse
  :type  'boolean)

;; XEmacs lacks a horizontal bar cursor option
(when (featurep 'xemacs)
  (setq vimpulse-want-operator-pending-cursor nil))

(defun vimpulse-set-operator-cursor-type ()
  "Change cursor appearance in Operator-Pending mode."
  (when vimpulse-want-operator-pending-cursor
    (vimpulse-half-height-cursor)))

(defun vimpulse-half-height-cursor ()
  "Change cursor to a half-height box.
\(This is really just a thick horizontal bar.)"
  (unless (featurep 'xemacs)
    (condition-case nil
        (let (height)
          (redisplay)
          (setq height (window-line-height))
          (setq height (+ (nth 0 height) (nth 3 height)))
          ;; Cut cursor height in half
          (setq height (/ height 2))
          (setq cursor-type (cons 'hbar height))
          ;; Ensure the cursor is redisplayed
          (force-window-update (selected-window))
          (redisplay))
      (error nil))))

(defun vimpulse-range
  (&optional no-repeat dont-move-point whole-lines keep-visual
             motion count)
  "Read a motion and return a range (BEG END).
In Visual mode, returns the beginning and end of the selection.
This can be used in the `interactive' form of a command:

    (defun foo (beg end)
      (interactive (vimpulse-range))
      ;; Do foo from BEG to END
      )

When called interactively, the command will read a motion, store
the resulting range in BEG and END, and do whatever it does on
the text between those buffer positions. The optional arguments
allow for some customization:

NO-REPEAT: don't let \\[viper-repeat] repeat the command.
DONT-MOVE-POINT: don't move to beginning of range in vi state.
WHOLE-LINES: extend range to whole lines.
KEEP-VISUAL: don't disable Visual selection.

After these arguments may follow a custom MOTION and COUNT
to use in vi (command) state. If specified, the command
will not read a motion in vi state."
  (let ((range (list (point) (point)))
        (beg (point)) (end (point))
        viper-ESC-moves-cursor-back)
    (setq vimpulse-this-motion-type nil
          vimpulse-this-count nil
          vimpulse-this-motion nil
          vimpulse-this-operator this-command)
    (cond
     ;; If text is selected, use selection boundaries as range
     ((or vimpulse-visual-mode (vimpulse-mark-active))
      (setq range (vimpulse-visual-range)
            beg (apply 'min range)
            end (apply 'max range)
            vimpulse-this-motion-type vimpulse-visual-mode
            motion nil)
      ;; Set up repeat
      (setq vimpulse-this-motion 'vimpulse-visual-reselect)
      ;; Disable selection
      (unless keep-visual
        (if vimpulse-visual-mode
            (vimpulse-visual-mode -1)
          (vimpulse-deactivate-region)))
      ;; Extend range to whole lines
      (when (and whole-lines
                 (not (eq 'line vimpulse-this-motion-type)))
        (setq range (list (save-excursion
                            (goto-char beg)
                            (line-beginning-position))
                          (save-excursion
                            (goto-char end)
                            (line-beginning-position 2)))
              vimpulse-visual-last 'line
              vimpulse-visual-height (count-lines beg end)))
      (if (eq 'block vimpulse-this-motion-type)
          (vimpulse-visual-block-rotate 'upper-left beg end)
        (goto-char beg)))
     ;; Not in Visual mode: use MOTION if specified,
     ;; or read motion and return motion range
     (t
      (if motion
          (setq vimpulse-this-motion motion
                vimpulse-this-count  count)
        (vimpulse-change-state-to-operator)
        (setq vimpulse-this-motion (vimpulse-keypress-parser t))
        (setq vimpulse-this-count (cadr vimpulse-this-motion)
              vimpulse-this-motion (car vimpulse-this-motion))
        ;; Return current line motion if operator calls itself
        (if (eq vimpulse-this-operator vimpulse-this-motion)
            (setq vimpulse-this-motion 'vimpulse-line
                  vimpulse-this-motion-type 'line)
          (setq vimpulse-this-motion
                (vimpulse-operator-remapping vimpulse-this-motion))))
      (cond
       ;; Quit if motion reading failed
       ((or (not vimpulse-this-motion)
            (eq 'viper-nil vimpulse-this-motion))
        (viper-change-state-to-vi)
        (setq quit-flag t))
       (t
        ;; Multiply operator count and motion count together
        (when (or current-prefix-arg vimpulse-this-count)
          (setq vimpulse-this-count
                (* (prefix-numeric-value current-prefix-arg)
                   (prefix-numeric-value vimpulse-this-count))))
        ;; Calculate motion range
        (setq range (vimpulse-motion-range
                     vimpulse-this-count vimpulse-this-motion)
              beg (apply 'min range)
              end (apply 'max range))
        ;; Extend range to whole lines
        (when (and whole-lines
                   (not (eq 'line vimpulse-this-motion-type)))
          (setq range (list (save-excursion
                              (goto-char beg)
                              (line-beginning-position))
                            (save-excursion
                              (goto-char end)
                              (line-beginning-position 2)))
                vimpulse-this-count (count-lines beg end)))
        (unless dont-move-point
          (goto-char beg)
          (when (and viper-auto-indent (bolp))
            (back-to-indentation)))
        (viper-change-state-to-vi)))))
    ;; Set up repeat
    (unless no-repeat
      (setq vimpulse-last-operator vimpulse-this-operator
            vimpulse-last-motion vimpulse-this-motion
            vimpulse-last-range-type vimpulse-this-motion-type)
      (viper-set-destructive-command
       (list 'vimpulse-operator-repeat
             vimpulse-this-count nil viper-use-register nil nil)))
    ;; Return range
    range))

(defun vimpulse-motion-range (count motion &optional pos)
  "Returns a range (BEG END) defined by point, MOTION and COUNT.
MOTION can move point or select some text (a text object).
It can be a command, a function or a list of Lisp expressions.
In Visual Mode, returns selection boundaries."
  (let ((current-prefix-arg count)
        (viper-intermediate-command 'viper-command-argument)
        (vimpulse-operator-basic-minor-mode t)
        (viper-current-state 'operator-state)
        retval)
    (save-excursion
      (when pos
        (goto-char pos))
      (viper-move-marker-locally 'viper-com-point (point))
      ;; Enable Transient Mark mode so we can reliably
      ;; detect mark setting
      (vimpulse-transient-mark)
      ;; Execute MOTION
      (cond
       ((listp motion)                  ; Lisp expression
        (setq retval (eval `(progn ,@motion))))
       ((commandp motion)
        (setq retval (call-interactively motion)))
       (t
        (setq retval (funcall motion count))))
      (cond
       ;; If text has been selected (i.e., it's a text object),
       ;; return the selection
       ((or vimpulse-visual-mode (vimpulse-mark-active))
        (prog1 (vimpulse-visual-range)
          (if vimpulse-visual-mode
              (vimpulse-visual-mode -1)
            (vimpulse-deactivate-region))
          (vimpulse-transient-restore)))
       ;; If a valid range was returned, use that
       ((vimpulse-range-p retval)
        retval)
       ;; Otherwise, range is defined by `viper-com-point'
       ;; and point (Viper type motion)
       (t
        (prog1 (list (min (point) (or viper-com-point (point)))
                     (max (point) (or viper-com-point (point))))
          (vimpulse-transient-restore)))))))

(defun vimpulse-range-p (object)
  "Return t if OBJECT is a range."
  (and (listp object)
       (eq 2 (length object))
       (numberp (car object))
       (numberp (cadr object))))

;; A keypress parser of some kind is unavoidable because we need to
;; know what we are executing beforehand (like when multiplying the
;; counts in "2d3w"). We try to avoid hard-coding where possible by
;; inspecting commands rather than the keys themselves.
(defun vimpulse-keypress-parser (&optional no-remap)
  "Read from keyboard and build a command description.
Returns (CMD COUNT), where COUNT is the numeric prefix argument
of CMD. Both COUNT and CMD may be nil."
  (let ((inhibit-quit t)
        char digit keys cmd count)
    (while (progn
             ;; Read a keypress, respecting Emacs version,
             ;; and convert it to ASCII representation
             (if (featurep 'xemacs)
                 (setq char (event-to-character
                             (next-command-event) nil t))
               (setq char (read-event))
               (when (symbolp char)
                 (setq char (or (get char 'ascii-character) char))))
             ;; This trick from simple.el's `digit-argument'
             ;; converts keystrokes like C-0 and C-M-1 to digits
             (setq digit (- (logand char ?\177) ?0))
             (if (keymapp cmd)
                 (setq keys (vconcat keys (vector char)))
               (setq keys (vector char)))
             (if no-remap              ; XEmacs doesn't have remapping
                 (setq cmd (key-binding keys t))
               (setq cmd (key-binding keys t t)))
             ;; This `cond' form determines whether
             ;; the reading loop will continue
             (cond
              ;; If calling itself ("cc"), return current command
              ((eq keys (vimpulse-strip-prefix
                         (vconcat (this-command-keys))))
               (setq cmd this-command)
               nil)
              ;; If CMD is a keymap, we need to read more
              ((keymapp cmd)
               t)
              ;; Numeric prefix argument
              ((or (memq cmd '(viper-digit-argument digit-argument))
                   ;; The 0 key runs `viper-beginning-of-line',
                   ;; so ignore it unless preceded by other digits
                   (and (eq 1 (length keys))
                        (not (keymapp cmd))
                        count
                        ;; Probably overkill: only 0 bound this way
                        (memq digit '(0 1 2 3 4 5 6 7 8 9))))
               ;; Store digits in a string, which is easily converted
               ;; to a number afterwards
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               t)
              ;; Catch middle digits like "da2w"
              ((and (not cmd)
                    (< 1 (length keys))
                    (memq digit '(0 1 2 3 4 5 6 7 8 9)))
               (setq count (concat (or count "")
                                   (number-to-string digit)))
               ;; Remove the digit from the key sequence
               ;; so we can see if the previous one goes anywhere
               (setq keys (vimpulse-truncate keys -1))
               (setq cmd (key-binding keys))
               t)
              ;; We might as well accept negative numbers using
              ;; Emacs' C--. Best of both worlds, right?
              ((eq 'negative-argument cmd)
               (unless count
                 (setq count "-")))
              ;; User pressed C-g, so return nil for CMD
              ((eq 'keyboard-quit cmd)
               (setq cmd nil))
              ;; We are done, exit the `while' loop
              (t
               nil))))
    ;; Determine COUNT
    (when (stringp count)
      (if (string= "-" count)
          (setq count nil)
        (setq count (string-to-number count))))
    ;; Return command description
    (list cmd count)))

;;; Repeat an operator/motion combination

;; This is used in `viper-d-com' (read by `viper-repeat').
(defun vimpulse-operator-repeat (arg)
  "Repeat an operator-motion combination.
ARG is a list of the form ((OPERATOR MOTION COUNT) . COM).
COM is discarded."
  (let ((val (viper-P-val arg)))
    (cond
     ((vimpulse-mark-active)
      (funcall operator (region-beginning) (region-end)))
     (t
      (vimpulse-operator-apply
       vimpulse-last-operator vimpulse-last-motion val
       vimpulse-last-range-type)))))

(defun vimpulse-operator-apply (operator motion count &optional type)
  "Apply OPERATOR on MOTION. COUNT is the motion count.
TYPE is the motion type."
  (let ((range (vimpulse-motion-range count motion))
        (vimpulse-this-operator operator)
        (vimpulse-this-motion motion)
        (vimpulse-this-motion-type type))
    (funcall operator (car range) (cadr range))))

(defun vimpulse-region-cmd-p (cmd)
  "Return t if CMD is a region command."
  (let ((spec (car (cdr (interactive-form cmd)))))
    (and (stringp spec)
         (not (not (string-match "r" spec))))))

(defun vimpulse-operator-cmd-p (cmd)
  "Return t if CMD is an operator command."
  (vimpulse-memq-recursive 'vimpulse-range
                           (interactive-form cmd)))

;;; Operators (yank, delete, change)

(defun vimpulse-yank (beg end)
  "Yank text from BEG to END."
  (interactive (vimpulse-range t t))
  (let ((length (abs (- beg end))))
    (cond
     ((eq 'block vimpulse-this-motion-type)
      (setq killed-rectangle (extract-rectangle beg end))
      ;; Associate the rectangle with the last entry in the kill-ring
      (unless kill-ring
        (copy-region-as-kill beg end))
      (put 'killed-rectangle 'previous-kill (current-kill 0))
      (vimpulse-visual-block-rotate 'upper-left beg end))
     (t
      (vimpulse-store-in-current-register beg end)
      (copy-region-as-kill beg end)
      (unless (eq 'line vimpulse-this-motion-type)
        (goto-char beg))
      (when (and (eolp) (not (bolp)))
        (backward-char))
      (when (< viper-change-notification-threshold length)
        (unless (or (viper-is-in-minibuffer)
                    (eq 'line vimpulse-this-motion-type))
          (message "Saved %d characters" length)))))))

(defun vimpulse-delete (beg end &optional dont-save)
  "Delete text from BEG to END.
If DONT-SAVE is t, just delete it."
  (interactive (vimpulse-range))
  (let ((length (abs (- end beg))))
    (cond
     (dont-save
      (cond
       ((eq 'block vimpulse-this-motion-type)
        (delete-rectangle beg end))
       (t
        (delete-region beg end))))
     ((eq 'block vimpulse-this-motion-type)
      (let ((orig (make-marker)))
        ;; Associate the rectangle with the last entry in the kill-ring
        (viper-move-marker-locally
         'orig (vimpulse-visual-block-position 'upper-left beg end))
        (unless kill-ring
          (copy-region-as-kill beg end))
        (kill-rectangle beg end)
        (put 'killed-rectangle 'previous-kill (current-kill 0))
        (goto-char orig)
        (set-marker orig nil)))
     (t
      (vimpulse-store-in-current-register beg end)
      (kill-region beg end)
      (when (and (eolp) (not (bolp)))
        (backward-char))
      (when (< viper-change-notification-threshold length)
        (unless (or (viper-is-in-minibuffer)
                    (eq 'line vimpulse-this-motion-type))
          (message "Deleted %d characters" length)))))))

(defun vimpulse-change (beg end &optional dont-save)
  "Change text from BEG to END.
If DONT-SAVE is non-nil, just delete it."
  (interactive (vimpulse-range))
  (cond
   ((eq 'block vimpulse-this-motion-type)
    (vimpulse-delete beg end dont-save)
    (goto-char
     (vimpulse-visual-create-coords
      'block ?i
      (min vimpulse-visual-point vimpulse-visual-mark)
      (1+ (max vimpulse-visual-point vimpulse-visual-mark))))
    (viper-insert nil))
   ((eq viper-intermediate-command 'viper-repeat)
    (if dont-save
        (delete-region beg end)
      (kill-region beg end))
    (when (eq 'line vimpulse-this-motion-type)
      (save-excursion (newline))
      (when viper-auto-indent
        (indent-according-to-mode)))
    (viper-yank-last-insertion))
   ((eq 'line vimpulse-this-motion-type)
    (setq viper-began-as-replace t)
    (if dont-save
        (delete-region beg end)
      (vimpulse-store-in-current-register beg end)
      (kill-region beg end))
    (save-excursion (newline))
    (when viper-auto-indent
      (indent-according-to-mode))
    (viper-change-state-to-insert))
   (t
    (if dont-save
        (delete-region beg end)
      (vimpulse-store-in-current-register beg end)
      (viper-change beg end)))))

(defun vimpulse-store-in-register (register start end)
  "Store text from START to END in REGISTER."
  (cond
   ((viper-valid-register register '(Letter))
    (viper-append-to-register
     (downcase register) start end))
   (t
    (copy-to-register register start end))))

(defun vimpulse-store-in-current-register (start end)
  "Store text from START to END in current register, if any.
Resets `viper-use-register'."
  (when viper-use-register
    (vimpulse-store-in-register viper-use-register start end)
    (setq viper-use-register nil)))

(defun vimpulse-read-register (&optional register command)
  "Use COMMAND with REGISTER.
If called interactively, read REGISTER and COMMAND from keyboard."
  (interactive)
  (setq register (or register (read-char)))
  (when (viper-valid-register register)
    (setq command (or command (key-binding (read-key-sequence nil))))
    (when (commandp command)
      (let ((this-command command)
            (viper-use-register register))
        (call-interactively command)))))

(define-key viper-vi-basic-map "y" 'vimpulse-yank)
(define-key viper-vi-basic-map "d" 'vimpulse-delete)
(define-key viper-vi-basic-map "c" 'vimpulse-change)
(define-key viper-vi-basic-map "\"" 'vimpulse-read-register)

;;; Remap non-motion commands to `viper-nil'

(defun vimpulse-operator-remap (from to)
  "Remap FROM to TO in Operator-Pending mode."
  (vimpulse-remap vimpulse-operator-remap-map from to))

(defun vimpulse-operator-remapping (cmd)
  "Return Operator-Pending remapping for CMD."
  (if (featurep 'xemacs)
      (or (cdr (assq cmd vimpulse-operator-remap-alist)) cmd)
    vimpulse-operator-remap-minor-mode
    (or (command-remapping cmd) cmd)))

(vimpulse-operator-remap 'redo 'viper-nil)
(vimpulse-operator-remap 'undo 'viper-nil)
(vimpulse-operator-remap 'viper-Put-back 'viper-nil)
(vimpulse-operator-remap 'viper-delete-backward-char 'viper-nil)
(vimpulse-operator-remap 'viper-delete-char 'viper-nil)
(vimpulse-operator-remap 'viper-insert 'viper-nil)
(vimpulse-operator-remap 'viper-intercept-ESC-key 'viper-nil)
(vimpulse-operator-remap 'viper-line-to-bottom 'viper-nil)
(vimpulse-operator-remap 'viper-line-to-middle 'viper-nil)
(vimpulse-operator-remap 'viper-line-to-top 'viper-nil)
(vimpulse-operator-remap 'viper-put-back 'viper-nil)
(vimpulse-operator-remap 'viper-repeat 'viper-nil)
(vimpulse-operator-remap 'viper-substitute 'viper-nil)

;;; Compatibility code allowing old-style Viper motions to work

(defadvice viper-execute-com (around vimpulse-operator activate)
  "Disable in Operator-Pending mode."
  (cond
   ((eq 'operator-state viper-current-state)
    (if (and com (eq com (upcase com)))
        (setq com ?R)
      (setq com ?r))
    ad-do-it
    (unless (eq 'viper-repeat viper-intermediate-command)
      (unless viper-d-com
        (setq viper-d-com (list nil nil nil nil nil nil)))
      (unless (eq vimpulse-this-motion
                  (vimpulse-operator-remapping m-com))
        (setq vimpulse-this-motion (vimpulse-operator-remapping m-com))
        (setcar (nthcdr 2 viper-d-com) com))
      (setq vimpulse-this-count val)
      (setcar (nthcdr 5 viper-d-com)
              (viper-array-to-string
               (if (arrayp viper-this-command-keys)
                   viper-this-command-keys
                 (this-command-keys))))))
   (t
    ad-do-it)))

;; The com code ?r runs a Viper motion in "range mode" without doing
;; anything else (it's associated with `viper-exec-dummy'). The
;; following sets ?R up similarly, but enables linewise ranges.
(aset viper-exec-array ?R 'vimpulse-exec-Dummy)

(defun vimpulse-exec-Dummy (mcom com)
  "Extend `viper-com-point'/point selection to whole lines."
  (cond
   ((< viper-com-point (point))
    (save-excursion
      (goto-char viper-com-point)
      (viper-move-marker-locally 'viper-com-point
                                 (line-beginning-position)))
    (goto-char (line-beginning-position 2)))
   (t
    (save-excursion
      (goto-char viper-com-point)
      (viper-move-marker-locally 'viper-com-point
                                 (line-beginning-position 2)))
    (goto-char (line-beginning-position)))))

(defadvice viper-getCom (around vimpulse-operator activate)
  "Translate ?r to ?R."
  (let ((com (viper-getcom arg)))
    (cond
     ((viper= com ?r)
      (setq ad-return-value ?R))
     (t
      ad-do-it))))

;; This separates the operator-pending part of a Viper motion from the
;; rest, defining a new command called vimpulse-operator-MOTION.
(defmacro vimpulse-operator-map-define (motion)
  "Define and map a new command for the operator-pending part of MOTION."
  (let ((motion-name (symbol-name motion))
        (docstring (documentation motion t)))
    (setq motion-name (replace-regexp-in-string
                       "^viper-\\\|^vimpulse-" "" motion-name))
    (setq motion-name
          (concat "vimpulse-operator-" motion-name))
    (setq motion-name (intern motion-name))
    (eval-after-load 'vimpulse-visual-mode
      `(add-to-list 'vimpulse-movement-cmds ',motion-name))
    (vimpulse-operator-remap motion motion-name)
    (eval `(defun ,motion-name (arg)
             ,(format "Operator-pending part of `%s'.\n\n%s"
                      motion (or docstring ""))
             (interactive "P")
             (let (com com-alist)
               (setq com-alist
                     '((vimpulse-change . ?c)
                       (vimpulse-delete . ?d)
                       (vimpulse-yank . ?y)))
               (setq com
                     (or (cdr (assq vimpulse-this-operator com-alist))
                         ?r))
               (,motion (if (vimpulse-mark-active)
                            arg
                          (cons arg com))))))
    `(quote ,motion-name)))

;; d%: when point is before the parenthetical expression,
;; include it in the resulting range
(defun vimpulse-operator-paren-match (arg)
  "Operator-pending part of `viper-paren-match'.

Go to the matching parenthesis."
  (interactive "p")
  (vimpulse-activate-mark (point))
  (viper-paren-match (cons arg ?r))
  (when (< (point) (mark t))
    (vimpulse-set-region (point) (1+ (mark t)))))

(vimpulse-operator-remap 'viper-paren-match 'vimpulse-operator-paren-match)

(defvar vimpulse-goto-line t
  "*Goto line with \"G\" like in Vim.")

(defun vimpulse-goto-line (arg)
  "Go to ARG's line; without ARG go to end of buffer.
Works like Vim's \"G\"."
  (interactive "P")
  (let ((val (viper-P-val arg))
        (com (viper-getCom arg)))
    (when (eq ?c com) (setq com ?C))
    (viper-move-marker-locally 'viper-com-point (point))
    (viper-deactivate-mark)
    (push-mark nil t)
    (cond
     ((null val)
      (goto-char (point-max)))
     (t
      (goto-line val)))
    (when com
      (viper-execute-com 'vimpulse-goto-line val com))))

(when vimpulse-goto-line
  (fset 'viper-goto-line 'vimpulse-goto-line)
  (define-key viper-vi-basic-map "G" 'vimpulse-goto-line))

(defun vimpulse-goto-first-line (arg)
  "Go to first line."
  (interactive "P")
  (let ((val (viper-P-val arg))
        (com (viper-getCom arg)))
    (when (eq ?c com) (setq com ?C))
    (viper-move-marker-locally 'viper-com-point (point))
    (viper-deactivate-mark)
    (push-mark nil t)
    (cond
     ((null val)
      (goto-char (point-min)))
     (t
      (goto-line val)))
    (when com
      (viper-execute-com 'vimpulse-goto-line val com))))

;; `viper-goto-eol' excludes last character on line unless
;; called in Insert state
(defun vimpulse-operator-goto-eol (arg)
  "Operator-Pending part of `viper-goto-eol'.

Goto end of line."
  (interactive "P")
  (let ((viper-current-state 'insert-state))
    (viper-goto-eol arg)))

(vimpulse-operator-remap 'viper-goto-eol 'vimpulse-operator-goto-eol)

;; Create Operator-Pending wrappers for remaining Viper motions
(vimpulse-operator-map-define vimpulse-goto-first-line)
(vimpulse-operator-map-define vimpulse-goto-line)
(vimpulse-operator-map-define viper-backward-Word)
(vimpulse-operator-map-define viper-backward-char)
(vimpulse-operator-map-define viper-backward-paragraph)
(vimpulse-operator-map-define viper-backward-sentence)
(vimpulse-operator-map-define viper-backward-word)
(vimpulse-operator-map-define viper-beginning-of-line)
(vimpulse-operator-map-define viper-end-of-Word)
(vimpulse-operator-map-define viper-end-of-word)
(vimpulse-operator-map-define viper-find-char-backward)
(vimpulse-operator-map-define viper-find-char-forward)
(vimpulse-operator-map-define viper-forward-Word)
(vimpulse-operator-map-define viper-forward-char)
(vimpulse-operator-map-define viper-forward-paragraph)
(vimpulse-operator-map-define viper-forward-sentence)
(vimpulse-operator-map-define viper-forward-word)
(vimpulse-operator-map-define viper-goto-char-backward)
(vimpulse-operator-map-define viper-goto-char-forward)
(vimpulse-operator-map-define viper-goto-line)
(vimpulse-operator-map-define viper-goto-mark)
(vimpulse-operator-map-define viper-goto-mark-and-skip-white)
(vimpulse-operator-map-define viper-next-line)
(vimpulse-operator-map-define viper-previous-line)
(vimpulse-operator-map-define viper-search-Next)
(vimpulse-operator-map-define viper-search-backward)
(vimpulse-operator-map-define viper-search-forward)
(vimpulse-operator-map-define viper-search-next)
(vimpulse-operator-map-define viper-window-bottom)
(vimpulse-operator-map-define viper-window-middle)
(vimpulse-operator-map-define viper-window-top)

(provide 'vimpulse-operator)

