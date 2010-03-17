;;;;
;;;; This file contains helper functions that
;;;; a) Can be useful for the end user
;;;; b) Can be useful for the contributor, thus avoiding
;;;;    duplication of functionalities.
;;;;

;; Utility code for autogenerating vi bindings
(defvar vimpulse-viper-movement-cmds
  '(viper-backward-Word viper-backward-char viper-backward-paragraph
    viper-backward-sentence viper-backward-word
    viper-beginning-of-line viper-command-argument
    viper-digit-argument viper-end-of-Word viper-end-of-word
    viper-exec-mapped-kbd-macro viper-find-char-backward
    viper-find-char-forward viper-forward-Word viper-forward-char
    viper-forward-paragraph viper-forward-sentence viper-forward-word
    viper-goto-char-backward viper-goto-char-forward viper-goto-eol
    viper-goto-line viper-line-to-bottom viper-line-to-middle
    viper-line-to-top viper-next-line viper-previous-line
    viper-scroll-down-one viper-scroll-down viper-scroll-up
    viper-scroll-up-one viper-window-bottom viper-window-middle
    viper-window-top vimpulse-end-of-previous-word
    vimpulse-goto-first-line vimpulse-goto-definition
    vimpulse-search-backward-for-symbol-at-point
    vimpulse-search-forward-for-symbol-at-point vimpulse-jump-backward
    vimpulse-jump-forward vimpulse-visual-toggle-normal
    vimpulse-visual-toggle-line vimpulse-visual-toggle-block)
  "List of Viper/Vimpulse movement commands.")

(defvar vimpulse-core-movement-cmds
  '(viper-backward-char
    viper-next-line
    viper-previous-line
    viper-forward-char)
  "List of Viper \"core\" movement commands.
These should be present in every mode, to avoid confusion.")

(defun vimpulse-augment-keymap
  (map augment-alist &optional replace)
  "Augment MAP with bindings from AUGMENT-ALIST.
If REPLACE is non-nil, bindings in MAP may be overwritten.
AUGMENT-ALIST has the format ((KEY . DEF) ...),
where KEY and DEF are passed to `define-key'."
  (let (key def num)
    (dolist (binding augment-alist)
      (setq key (car binding)
            def (cdr binding)
            num (lookup-key map key))
      (cond
       (replace
        (when (numberp num)
          (define-key map (vimpulse-truncate key num) nil))
        (define-key map key def))
       (t
        (when (numberp num)
          (setq num (lookup-key map (vimpulse-truncate key num))))
        (unless num
          (define-key map key def)))))))

(defun vimpulse-add-vi-bindings (map cmds &optional replace filter)
  "Add vi bindings for CMDS to MAP.
Add forcefully if REPLACE is t. Don't add keys matching FILTER,
which is a list of key vectors."
  (let (pmap keys)
    (unless filter
      (when (and (boundp 'viper-want-ctl-h-help)
                 viper-want-ctl-h-help)
        (add-to-list 'filter [?\C-h]))
      (unless (and (boundp 'vimpulse-want-C-u-like-Vim)
                   vimpulse-want-C-u-like-Vim)
        (add-to-list 'filter [?\C-u])))
    (setq pmap (make-sparse-keymap))
    (dolist (cmd cmds map)
      (dolist (vimap (list viper-vi-intercept-map
                           viper-vi-local-user-map
                           viper-vi-global-user-map
                           viper-vi-kbd-map
                           viper-vi-diehard-map
                           viper-vi-basic-map))
        (setq keys (where-is-internal cmd vimap))
        (dolist (key keys)
          (unless (let (match)
                    (dolist (entry filter match)
                      (when (equal key (vimpulse-truncate
                                        entry (length key)))
                        (setq match t))))
            (when (or (not (lookup-key pmap key))
                      (numberp (lookup-key pmap key)))
              (vimpulse-augment-keymap map
                                       `((,key . ,cmd))
                                       replace)
              ;; To prioritize between maps in `vimap',
              ;; we keep track of bindings by augmenting `pmap'.
              (vimpulse-augment-keymap pmap
                                       `((,key . ,cmd))))))))))

(defun vimpulse-add-movement-cmds (map &optional replace)
  "Add Viper/Vimpulse movement commands to MAP.
The commands are taken from `vimpulse-viper-movement-cmds' and looked
up in vi keymaps. If REPLACE is non-nil, may overwrite bindings
in MAP."
  (vimpulse-add-vi-bindings map vimpulse-viper-movement-cmds replace))

;; The default for this function is to replace rather than augment,
;; as core navigation should be present everywhere
(defun vimpulse-add-core-movement-cmds (map &optional augment)
  "Add \"core\" movement commands to MAP, forcefully.
The commands are taken from `vimpulse-core-movement-cmds'.
If AUGMENT is non-nil, don't overwrite bindings in MAP."
  (vimpulse-add-vi-bindings map
                            vimpulse-core-movement-cmds
                            (not augment)))

(defun vimpulse-inhibit-movement-cmds (map &optional replace)
  "Remap Viper movement commands to `viper-nil' in MAP.
The commands are taken from `vimpulse-viper-movement-cmds'.
If REPLACE is non-nil, may overwrite bindings in MAP."
  (dolist (cmd vimpulse-viper-movement-cmds)
    (eval `(vimpulse-augment-keymap
            map '(([remap ,cmd] . viper-nil))
            replace))))

(defun vimpulse-inhibit-destructive-cmds (map &optional replace)
  "Remap destructive Viper commands to `viper-nil' in MAP.
This isn't complete since `viper-command-argument' is left out so
that yanking may work, but as change and delete fail silently in
read-only buffers anyway, it does the job."
  (dolist (cmd '(viper-Append
                 viper-Insert
                 viper-append
                 viper-change-to-eol
                 viper-insert
                 viper-kill-line
                 viper-substitute
                 viper-substitute-line
                 vimpulse-visual-append
                 vimpulse-visual-change
                 vimpulse-visual-insert))
    (eval `(vimpulse-augment-keymap
            map '(([remap ,cmd] . viper-nil))
            replace))))

;; Makes dealing with vectors and key sequences a little easier
(defun vimpulse-truncate (vector length &optional offset)
  "Return a copy of VECTOR truncated to LENGTH.
If LENGTH is negative, skip last elements of VECTOR.
If OFFSET is specified, skip first elements of VECTOR."
  ;; If LENGTH is too large, trim it
  (when (> length (length vector))
    (setq length (length vector)))
  ;; If LENGTH is negative, convert it to the positive equivalent
  (when (> 0 length)
    (setq length (+ (length vector) length)))
  (when (> 0 length)
    (setq length 0))
  (if offset
      (setq length (- length offset))
    (setq offset 0))
  (let ((result (make-vector length t)))
    (dotimes (idx length result)
      (aset result idx (aref vector (+ idx offset))))))

(defun vimpulse-region-face ()
  "Return face of region."
  (if (featurep 'xemacs) 'zmacs-region 'region))

;; Set functions for handling overlays (not yet provided by Viper)
(cond
 ((featurep 'xemacs)                    ; XEmacs
  (fset 'vimpulse-delete-overlay 'delete-extent)
  (fset 'vimpulse-overlays-at 'extents-at))
 (t                                     ; GNU Emacs
  (fset 'vimpulse-delete-overlay 'delete-overlay)
  (fset 'vimpulse-overlays-at 'overlays-at)))

;; `viper-make-overlay' doesn't handle FRONT-ADVANCE
;; and REAR-ADVANCE properly in XEmacs
(defun vimpulse-make-overlay
  (beg end &optional buffer front-advance rear-advance)
  "Create a new overlay with range BEG to END in BUFFER.
In XEmacs, create an extent."
  (cond
   ((featurep 'xemacs)
    (let ((extent (make-extent beg end buffer)))
      (set-extent-property extent 'start-open front-advance)
      (set-extent-property extent 'end-closed rear-advance)
      (set-extent-property extent 'detachable nil)
      extent))
   (t
    (make-overlay beg end buffer front-advance rear-advance))))

(defun vimpulse-overlay-before-string (overlay string &optional face)
  "Set the `before-string' property of OVERLAY to STRING.
In XEmacs, change the `begin-glyph' property."
  (cond
   ((featurep 'xemacs)
    (setq face (or face (get-text-property 0 'face string)))
    (when (and string (not (glyphp string)))
      (setq string (make-glyph string)))
    (when face
      (set-glyph-face string face))
    (set-extent-begin-glyph overlay string))
   (t
    (viper-overlay-put overlay 'before-string string))))

(defun vimpulse-overlay-after-string (overlay string &optional face)
  "Set the `after-string' property of OVERLAY to STRING.
In XEmacs, change the `end-glyph' property."
  (cond
   ((featurep 'xemacs)
    (setq face (or face (get-text-property 0 'face string)))
    (when (and string (not (glyphp string)))
      (setq string (make-glyph string)))
    (when face
      (set-glyph-face string face))
    (set-extent-end-glyph overlay string))
   (t
    (viper-overlay-put overlay 'after-string string))))

(provide 'vimpulse-utils)

