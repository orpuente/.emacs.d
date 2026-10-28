;;; structural-insert.el --- Confine point to the symbol at point -*- lexical-binding: t; -*-

;; Keywords: convenience, editing

;;; Commentary:
;;
;; `structural-insert-mode' is a small minor mode meant as a building
;; block for a larger structural-editing system.  When turned on it:
;;
;;   1. sets `cursor-type' (buffer-locally) to `bar', and
;;   2. confines point to the symbol that was at point when the mode
;;      was enabled: any command that tries to move point past either
;;      edge of that symbol is effectively a no-op -- point simply
;;      stays put at the boundary it tried to cross.
;;
;; The symbol's bounds are tracked with markers, so editing text
;; inside the symbol (e.g. typing more characters at either end)
;; naturally grows the allowed region, while the mode still keeps you
;; from wandering into the surrounding text.
;;
;; Implementation note: point restriction is done via a buffer-local
;; `post-command-hook' that clamps point back inside the recorded
;; bounds after every command, rather than via `narrow-to-region'.
;; Narrowing would also hide the rest of the buffer, which is
;; undesirable here since the point is to constrain *editing*, not
;; visibility. The post-command-hook clamp is the same technique used
;; by packages like `iedit' or `symbol-overlay' to fence in point.
;;
;; Editing an existing symbol vs. inserting a new one:
;;
;; If a symbol touches point at all -- point is inside it, or right
;; at either of its edges -- that symbol is the target, and the mode
;; confines point to it as described above.
;;
;; Otherwise point is sitting at a seam where no symbol currently
;; exists, which covers the three situations described in the
;; requirements this mode was built against:
;;   1. between two symbols, separated by whitespace on both sides;
;;   2. between a boundary (a delimiter, or the start/end of a list,
;;      line, or file) and a symbol, with whitespace between them; or
;;   3. between two boundaries.
;; In all three, the mode seeds a zero-width region at point instead
;; of erroring out.  As the user types, that region grows to cover
;; exactly the new symbol being created, while still refusing to let
;; point cross into whatever boundary or symbol happens to sit next
;; to it.
;;
;; Note on case 2 with *no* whitespace at all, e.g. point between "("
;; and "foo" in "(foo)": `bounds-of-thing-at-point' still considers
;; point to be touching "foo" (its left edge), so this mode treats it
;; as editing/extending "foo" rather than starting a separate new
;; symbol immediately in front of it. Distinguishing "extend the
;; touching symbol" from "insert a new, separate symbol right here"
;; isn't possible from position alone once the two are flush against
;; each other with no gap; a caller that wants the latter should
;; leave (or insert) at least one character of separating whitespace
;; before turning the mode on.

;;; Code:

(require 'thingatpt)

(defgroup structural-insert nil
  "Confine point to a single symbol."
  :group 'convenience
  :prefix "structural-insert-")

(defvar-local structural-insert--beg nil
  "Marker at the start of the symbol point is confined to.")

(defvar-local structural-insert--end nil
  "Marker at the end of the symbol point is confined to.")

(defvar-local structural-insert--saved-cursor-type nil
  "`cursor-type' to restore once the mode is turned off.")

(defun structural-insert--target-bounds ()
  "Return the (BEG . END) region `structural-insert-mode' should confine to.

If a symbol touches point at all -- point inside it, or at either
of its edges -- that symbol's bounds are used, so the mode can
edit or extend it in place.

Otherwise point is at a seam where no symbol currently exists (see
the cases described in the Commentary section above), and a
zero-width region at point is returned instead; it will grow to
cover whatever new symbol the user types next."
  (or (bounds-of-thing-at-point 'symbol)
      (cons (point) (point))))

(defun structural-insert--enforce ()
  "Keep point within [`structural-insert--beg', `structural-insert--end'].
Installed on `post-command-hook'.  If the last command moved
point past either boundary, point is snapped back, which makes
crossing the boundary look like a no-op to the user."
  (when (and structural-insert--beg structural-insert--end)
    (let ((beg (marker-position structural-insert--beg))
          (end (marker-position structural-insert--end)))
      (cond
       ;; The symbol got entirely eaten (e.g. the user deleted it) --
       ;; nothing left to confine ourselves to, so bail out cleanly.
       ((> beg end) (structural-insert-mode -1))
       ((< (point) beg) (goto-char beg))
       ((> (point) end) (goto-char end))))))

(defun structural-insert--back-to-edit ()
  (interactive)
  (structural-insert-mode -1)
  (structural-editing--collapse-space)
  ;; Walk one char backwards to avoid jumping to the next sexp.
  (unless structural-editing-mode (goto-char (1- (point)))) ;; TODO: this doesn't work
  (structural-editing-mode 1))

(defvar structural-insert-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Conventional escape hatch, mirroring `iedit-mode' /
    ;; `multiple-cursors'. Drop this binding if the larger system
    ;; wants to own the exit path itself.
    (dolist (k '("C-g" "s-i" "C-<up>" "<up>"))
      (define-key map (kbd k) #'structural-insert--back-to-edit))
    map)
  "Keymap for `structural-insert-mode'.")

;;;###autoload
(define-minor-mode structural-insert-mode
  "Confine the cursor to a symbol and show a bar cursor.

If a symbol touches point -- inside it, or at either edge -- that
symbol is the target: the mode confines point to it so it can be
edited or extended in place.  If point is instead at a seam
between tokens with no symbol touching it at all -- flanked by
whitespace, a delimiter, or a list/line/file boundary -- a new,
initially empty symbol is started there, and the confined region
grows to track whatever is typed.

Either way, trying to move point past either edge of the target
is a no-op.  Turning the mode off restores the previous cursor
type and lifts the restriction."
  :init-value nil
  :lighter " SIns"
  :keymap structural-insert-mode-map
  (if structural-insert-mode
      (let ((bounds (structural-insert--target-bounds)))
        ;; `end' uses insertion-type t so that typing exactly at the
        ;; end of the target extends the marker forward (the new text
        ;; counts as part of the symbol, whether that symbol already
        ;; existed or is being created from scratch); `beg' keeps the
        ;; default insertion-type nil so typing exactly at the start
        ;; likewise ends up included rather than pushed outside the
        ;; region.
        (setq structural-insert--beg (copy-marker (car bounds)))
        (setq structural-insert--end (copy-marker (cdr bounds) t))
        (setq structural-insert--saved-cursor-type cursor-type)
        (setq cursor-type 'bar)
        (add-hook 'post-command-hook #'structural-insert--enforce nil t))
    (when structural-insert--beg (set-marker structural-insert--beg nil))
    (when structural-insert--end (set-marker structural-insert--end nil))
    (setq structural-insert--beg nil
          structural-insert--end nil)
    (setq cursor-type (or structural-insert--saved-cursor-type t))
    (remove-hook 'post-command-hook #'structural-insert--enforce t)))

(provide 'structural-insert)
;;; structural-insert.el ends here
