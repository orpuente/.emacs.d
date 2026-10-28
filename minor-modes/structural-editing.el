;;; structural-editing.el --- structural operations for lisp editing -*- lexical-binding: t; -*-

;; The central idea: the "cursor" is a NODE, not a character.
;;
;; A node is either an atom (symbol, number, string, keyword) or a list
;; form, including any expression prefix (' ` , #).
;;
;; INVARIANT: point rests on the first character of the current node.
;; Every command below restores that invariant before it returns.  Once
;; the invariant holds, "which sexp am I on?" is a pure function of
;; point, so there is no separate node state to keep in sync with the
;; buffer -- edits, undo, and other packages can move point freely and
;; the highlight still lands somewhere sensible.

(require 'cl-lib)
(require 'smartparens)

(defgroup structural-editing nil
  "Structural editing for lisp, where the cursor is a sexp."
  :group 'lisp
  :prefix "structural-editing-")

(defcustom structural-editing-hide-cursor t
  "When non-nil, hide the character cursor while `structural-editing-mode' is on.
The node overlay becomes the only cursor.  Set to nil if you want to
keep seeing where a self-inserting character would land."
  :type 'boolean)

(defface structural-editing-node
  '((((background dark))  :background "#2e3b4e" :extend nil)
    (((background light)) :background "#dde6f2" :extend nil))
  "Face for the current node.")

(defface structural-editing-node-blocked
  '((((background dark))  :background "#5a2f38" :extend nil)
    (((background light)) :background "#f5d6da" :extend nil))
  "Face flashed when a structural motion has nowhere to go.")


;;;; ---------------------------------------------------------------------
;;;; Node model

(defun structural-editing--beg-of (pos)
  "Start of the sexp whose body begins at POS, including prefix chars."
  (save-excursion (goto-char pos) (backward-prefix-chars) (point)))

(defun structural-editing--body-of (pos)
  "Start of the sexp body at POS, skipping any expression prefix."
  (save-excursion (goto-char pos) (skip-syntax-forward "'") (point)))

(defun structural-editing-node-bounds ()
  "Return (BEG . END) of the node at point, or nil.

Resolution order: the sexp at or after point in the current form,
else the enclosing form, else the last sexp before point.  This is
deliberately forgiving -- it means the highlight still shows something
useful after a command that left point somewhere unstructured."
  (save-excursion
    (let* ((here (point))
           (end (ignore-errors (scan-sexps here 1))))
      (cond
       (end (cons (structural-editing--beg-of (scan-sexps end -1)) end))
       ((ignore-errors (scan-lists here -1 1))
        (let ((open (scan-lists here -1 1)))
          (cons (structural-editing--beg-of open) (scan-sexps open 1))))
       (t (let ((beg (ignore-errors (scan-sexps here -1))))
            (when beg (cons (structural-editing--beg-of beg) (scan-sexps beg 1)))))))))

(defun structural-editing-normalize ()
  "Put point on the first character of the node at point.
Return the node bounds, or nil."
  (let ((b (structural-editing-node-bounds)))
    (when b (goto-char (car b)))
    b))

(defun structural-editing--container ()
  "Bounds of the form to act on for container operations.
The current node if it is a list, otherwise the enclosing form."
  (let* ((b (structural-editing-node-bounds))
         (body (and b (structural-editing--body-of (car b)))))
    (if (and body (eq (char-syntax (char-after body)) ?\())
        (cons body (scan-sexps body 1))
      (let ((open (ignore-errors (scan-lists (point) -1 1))))
        (and open (cons open (scan-sexps open 1)))))))


;;;; ---------------------------------------------------------------------
;;;; The overlay cursor

(defvar-local structural-editing--ov nil)
(defvar-local structural-editing--saved-cursor t)

(defun structural-editing--refresh ()
  "Move the node overlay to cover the node at point."
  (when (and (bound-and-true-p structural-editing-mode) (not (minibufferp)))
    (let ((b (structural-editing-node-bounds)))
      (cond
       ((null b) (when (overlayp structural-editing--ov) (delete-overlay structural-editing--ov)))
       (t (unless (overlayp structural-editing--ov)
            (setq structural-editing--ov (make-overlay 1 1 nil t nil))
            (overlay-put structural-editing--ov 'priority -50))
          (overlay-put structural-editing--ov 'face 'structural-editing-node)
          (move-overlay structural-editing--ov (car b) (cdr b) (current-buffer)))))))

(defun structural-editing--post-command ()
  (with-demoted-errors "structural-editing: %S" (structural-editing--refresh)))

(defun structural-editing--nope (fmt &rest args)
  "Report that a structural motion had nowhere to go.
Flashes the node overlay rather than beeping -- hitting the edge of a
form is a normal thing to do, not a mistake.  Always returns nil."
  (when (and (overlayp structural-editing--ov) (overlay-buffer structural-editing--ov))
    (overlay-put structural-editing--ov 'face 'structural-editing-node-blocked)
    (run-at-time 0.12 nil
                 (lambda (ov)
                   (when (and (overlayp ov) (overlay-buffer ov))
                     (overlay-put ov 'face 'structural-editing-node)))
                 structural-editing--ov))
  (message "%s" (apply #'format fmt args))
  nil)


;;;; ---------------------------------------------------------------------
;;;; Motion
;;
;; None of these wrap, and none of them move point when they fail.

(defun structural-editing--next-pos ()
  (let* ((b (structural-editing-node-bounds))
         (e (and b (ignore-errors (scan-sexps (cdr b) 1)))))
    (and e (structural-editing--beg-of (scan-sexps e -1)))))

(defun structural-editing--prev-pos ()
  (let* ((b (structural-editing-node-bounds))
         (s (and b (ignore-errors (scan-sexps (car b) -1)))))
    (and s (structural-editing--beg-of s))))

(defun structural-editing-next ()
  "Move to the next sibling node."
  (interactive)
  (let ((p (structural-editing--next-pos)))
    (if p (goto-char p) (structural-editing--nope "Last node in this form"))))

(defun structural-editing-prev ()
  "Move to the previous sibling node."
  (interactive)
  (let ((p (structural-editing--prev-pos)))
    (if p (goto-char p) (structural-editing--nope "First node in this form"))))

(defun structural-editing-last ()
  "Move to the last sibling in this form."
  (interactive)
  (let (p) (while (setq p (structural-editing--next-pos)) (goto-char p))))

(defun structural-editing-first ()
  "Move to the first sibling in this form."
  (interactive)
  (let (p) (while (setq p (structural-editing--prev-pos)) (goto-char p))))

(defun structural-editing-descend ()
  "Move to the first child of the current node."
  (interactive)
  (let* ((b (structural-editing-node-bounds))
         (target (and b (save-excursion
                          (goto-char (car b))
                          (ignore-errors
                            (down-list 1)
                            (let ((e (scan-sexps (point) 1)))
                              (and e (structural-editing--beg-of (scan-sexps e -1)))))))))
    ;; The `<' guard stops us descending into a list that merely FOLLOWS
    ;; an atom we are sitting on.
    (if (and target (< target (cdr b)))
        (goto-char target)
      (structural-editing--nope "No children here"))))

(defun structural-editing-ascend ()
  "Move to the enclosing form."
  (interactive)
  (let* ((b (structural-editing-node-bounds))
         (open (and b (ignore-errors (scan-lists (car b) -1 1)))))
    (if open (goto-char (structural-editing--beg-of open))
      (structural-editing--nope "Already at top level"))))

(defun structural-editing--node-name ()
  "Text used for same-name search: this atom, or this form's head symbol."
  (let* ((b (structural-editing-node-bounds))
         (body (and b (structural-editing--body-of (car b)))))
    (when b
      (if (eq (char-syntax (char-after body)) ?\()
          (save-excursion
            (goto-char body)
            (ignore-errors
              (down-list 1)
              (let ((e (scan-sexps (point) 1)))
                (and e (buffer-substring-no-properties
                        (scan-sexps e -1) e)))))
        (buffer-substring-no-properties body (cdr b))))))

(defun structural-editing-same-name (&optional backward)
  "Jump to the next node with the same name.  Does not wrap.
For a list, the name is its head symbol -- so this walks between calls
to the same function."
  (interactive "P")
  (let ((name (structural-editing--node-name))
        (b (structural-editing-node-bounds)))
    (if (not (and name b))
        (structural-editing--nope "No name here")
      (let ((regexp (concat "\\_<" (regexp-quote name) "\\_>"))
            (case-fold-search nil)
            (origin (point))
            (search (if backward #'re-search-backward #'re-search-forward)))
        (goto-char (if backward (car b) (cdr b)))
        (if (funcall search regexp nil t)
            (progn (goto-char (match-beginning 0))
                   (backward-prefix-chars)
                   (structural-editing-normalize))
          (goto-char origin)
          (structural-editing--nope "No %s `%s'" (if backward "previous" "next") name))))))

(defun structural-editing-same-name-backward ()
  "Jump to the previous node with the same name."
  (interactive)
  (structural-editing-same-name t))


;;;; ---------------------------------------------------------------------
;;;; Structural edits
;;
;; Every edit ends by putting the cursor back on a node -- usually the
;; same one you started on, so the highlight follows your node through
;; the transformation instead of teleporting.

(defun structural-editing--op (op anchor)
  "Run OP on the current container, then restore the node cursor.

ANCHOR names the delimiter OP leaves in place: `open' for the forward
operations, `close' for the backward ones.  That is the whole trick --
slurp and barf relocate exactly one delimiter, so anchoring a marker to
the other one always survives the edit.  When the cursor is on an atom
we anchor to the atom itself and simply follow it."
  (let* ((b (structural-editing-node-bounds))
         (c (or (structural-editing--container) (user-error "Not inside a form")))
         (on-container (and b (= (car c) (structural-editing--body-of (car b)))))
         (atom-m (and b (not on-container) (copy-marker (car b) nil)))
         (m (copy-marker (if (eq anchor 'open) (car c) (1- (cdr c))) nil)))
    (unwind-protect
        (progn
          (goto-char (1+ (car c)))
          (funcall op)
          (if atom-m
              (goto-char atom-m)
            (goto-char m)
            (unless (eq anchor 'open)
              (ignore-errors (backward-up-list 1 t t))))
          (backward-prefix-chars)
          (structural-editing-normalize))
      (set-marker m nil)
      (when atom-m (set-marker atom-m nil)))))

(defun structural-editing-slurp-forward ()
  "Pull the next sibling of this form inside it."
  (interactive) (structural-editing--op #'sp-forward-slurp-sexp 'open))

(defun structural-editing-barf-forward ()
  "Push this form's last child out to the right."
  (interactive) (structural-editing--op #'sp-forward-barf-sexp 'open))

(defun structural-editing-slurp-backward ()
  "Pull the previous sibling of this form inside it."
  (interactive) (structural-editing--op #'sp-backward-slurp-sexp 'close))

(defun structural-editing-barf-backward ()
  "Push this form's first child out to the left."
  (interactive) (structural-editing--op #'sp-backward-barf-sexp 'close))

(defun structural-editing-swap-next ()
  "Swap this node with its next sibling, following the node."
  (interactive)
  (let ((b (structural-editing-node-bounds)))
    (if (not (and b (ignore-errors (scan-sexps (cdr b) 1))))
        (structural-editing--nope "Last node in this form")
      (goto-char (cdr b))
      (transpose-sexps 1)
      (backward-sexp 1)
      (structural-editing-normalize))))

(defun structural-editing-swap-prev ()
  "Swap this node with its previous sibling, following the node."
  (interactive)
  (let ((b (structural-editing-node-bounds)))
    (if (not (and b (ignore-errors (scan-sexps (car b) -1))))
        (structural-editing--nope "First node in this form")
      ;; Swapping backward is swapping the predecessor forward, then
      ;; stepping back onto ourselves.
      (structural-editing-prev) (structural-editing-swap-next) (structural-editing-prev))))

(defun structural-editing-raise ()
  "Replace the enclosing form with this node."
  (interactive)
  (let ((b (or (structural-editing-node-bounds) (user-error "No node here"))))
    (goto-char (car b))
    (raise-sexp)
    (structural-editing-normalize)))

(defun structural-editing-splice-parent ()
  "Remove the enclosing form's delimiters, keeping the cursor here."
  (interactive)
  (let ((m (copy-marker (car (or (structural-editing-node-bounds)
                                 (user-error "No node here")))
                        nil)))
    (unwind-protect
        (progn (sp-splice-sexp) (goto-char m) (structural-editing-normalize))
      (set-marker m nil))))

(defun structural-editing-splice-node ()
  "Remove this node's own delimiters, leaving its children in place."
  (interactive)
  (structural-editing-descend)
  (structural-editing-splice-parent))

(defun structural-editing-wrap (&optional open)
  "Wrap this node in a new form and select it.
OPEN defaults to `('."
  (interactive)
  (let* ((b (or (structural-editing-node-bounds) (user-error "No node here")))
         (open (or open ?\())
         (close (matching-paren open)))
    (save-excursion
      (goto-char (cdr b)) (insert close)
      (goto-char (car b)) (insert open))
    (goto-char (car b))))

(defun structural-editing--collapse-space ()
  "Tidy horizontal whitespace at point after removing a node."
  (let ((beg (save-excursion (skip-chars-backward " \t") (point)))
        (end (save-excursion (skip-chars-forward " \t") (point))))
    (delete-region beg end)
    (when (and (not (bolp)) (not (eolp))
               (not (looking-at-p "\\s)"))
               (not (looking-back "\\s(" 1)))
      (insert " "))))

(defun structural-editing-copy ()
  "Copy this node."
  (interactive)
  (let ((b (or (structural-editing-node-bounds) (user-error "No node here"))))
    (copy-region-as-kill (car b) (cdr b))
    (message "Copied node")))

(defun structural-editing-kill ()
  "Kill this node and select a sensible neighbour.
Prefers the next sibling, then the previous one, then the parent."
  (interactive)
  (let* ((b (or (structural-editing-node-bounds) (user-error "No node here")))
         (has-next (ignore-errors (scan-sexps (cdr b) 1))))
    (kill-region (car b) (cdr b))
    (structural-editing--collapse-space)
    (if has-next
        (structural-editing-normalize)
      (let ((p (ignore-errors (scan-sexps (point) -1))))
        (if p (goto-char (structural-editing--beg-of p)) (structural-editing-ascend))))))

(defun structural-editing-yank ()
  "Yank as a new sibling after this node, and select it."
  (interactive)
  (let ((b (or (structural-editing-node-bounds) (user-error "No node here"))))
    (goto-char (cdr b))
    (insert " ")
    (let ((start (point)))
      (yank)
      (goto-char start)
      (structural-editing-normalize))))


;;;; ---------------------------------------------------------------------
;;;; Leaving the structural world
;;
;; These are the escape hatches into ordinary character editing.  Each
;; positions point and turns the mode off, so the real cursor comes back
;; and self-insertion behaves normally.  `structural-editing-mode' turns it back on.

(defun structural-editing--edit-at (pos &optional add-space backwards)
  (structural-editing-mode -1)
  (goto-char pos)
  (when add-space
    (insert-char ?\s)
    (when backwards (goto-char pos)))
  (structural-insert-mode 1))

(defun structural-editing-insert-before ()
  "Edit just before this node."
  (interactive)
  (structural-editing--edit-at (with-bounds s) t t))

(defun structural-editing-insert-after ()
  "Edit just after this node."
  (interactive)
  (structural-editing--edit-at (with-bounds e) t))

(defun structural-editing-insert-head ()
  "Edit just inside the head of this form."
  (interactive)
  (structural-editing--edit-at (1+ (structural-editing--body-of (with-bounds s))) t t))

(defun structural-editing-insert-tail ()
  "Edit just inside the tail of this form."
  (interactive)
  (structural-editing--edit-at (1- (with-bounds e)) t))

(defun structural-editing-new-sibling ()
  "Open a new empty form after this node and edit inside it."
  (interactive)
  (goto-char (with-bounds e))
  (insert " ()")
  (structural-editing--edit-at (1- (point))))


;;;; ---------------------------------------------------------------------
;;;; Structural Editing DSL

(defun structural-editing--state ()
  (cond ((thing-at-point 'symbol) 'symbol)
     ((thing-at-point 'sexp) 'list)
     ((thing-at-point 'whitespace) 'placeholder)
     (t (structural-editing--nope "bad thing at point") nil)))

(defmacro state-case (&rest cases)
  `(cl-ecase (structural-editing--state)
     ,@cases))

(defmacro with-bounds (&rest body)
  `(let* ((b (structural-editing-node-bounds)) 
          (s (car b))
          (e (cdr b)))
     ,@body))



(defun tap ()
  (interactive)
  (message "%s" (structural-editing--state)))

(cl-labels ((prev-char () (with-bounds (goto-char (max s (1- (point))))))
            (next-char () (with-bounds (goto-char (min e (1+ (point))))))
            
            (prev-sexp ()
              (let ((p (structural-editing--prev-pos)))
                (if p (goto-char p) (structural-editing--nope "First node in this form"))))
            (next-sexp ()
              (let ((p (structural-editing--next-pos)))
                (if p (goto-char p) (structural-editing--nope "Last node in this form"))))
            
            (prev-placeholder ())
            (next-placeholder ())
            
            (first-char () (with-bounds (goto-char s)))
            (last-char () (with-bounds (goto-char e)))

            (first-sexp () (let (p) (while (setq p (structural-editing--prev-pos)) (goto-char p))))
            (last-sexp () (let (p) (while (setq p (structural-editing--next-pos)) (goto-char p))))
            
            (beg-of-line? () (sp--looking-back-p (concat "^\\s-*" (regexp-quote id))))
            (end-of-line? () (sp--looking-at-p "\\s-*$"))
            (has-parent? ()
              (let* ((b (structural-editing-node-bounds))
                     (open (and b (ignore-errors (scan-lists (car b) -1 1)))))
                (not open)))
            
            (ascend-sexp ()
              (let* ((b (structural-editing-node-bounds))
                     (open (and b (ignore-errors (scan-lists (car b) -1 1)))))
                (if open (goto-char (structural-editing--beg-of open))
                  (structural-editing--nope "Already at top level"))))
            (descend-sexp ()
              (let* ((b (structural-editing-node-bounds))
                     (target (and b (save-excursion
                                      (goto-char (car b))
                                      (ignore-errors
                                        (down-list 1)
                                        (let ((e (scan-sexps (point) 1)))
                                          (and e (structural-editing--beg-of (scan-sexps e -1)))))))))
                ;; The `<' guard stops us descending into a list
                ;; that merely follows an atom we are sitting on.
                (if (and target (< target (cdr b)))
                    (goto-char target)
                  (structural-editing--nope "No children here")))))

  (defun structural-editing-prev~ ()
    (interactive)
    (structural-editing-insert-before))
  
  (defun structural-editing-next~ ()
    (interactive)
    (structural-editing-insert-after))

  (defun structural-editing-prev ()
    (interactive)
    (prev-sexp))

  (defun structural-editing-next ()
    (interactive)
    (next-sexp))

  (defun structural-editing-ascend ()
    (interactive)
    (ascend-sexp))

  (defun structural-editing-descend ()
    (interactive)
    (state-case
     (symbol (structural-editing--edit-at (with-bounds e)))
     (list (descend-sexp))))

  (defun structural-editing-first-in-line ()
    (interactive))

  (defun structural-editing-last-in-line ()
    (interactive))

  (defun structural-editing-first-in-list ()
    (interactive)
    (first-sexp))

  (defun structural-editing-last-in-list ()
    (interactive)
    (last-sexp))

  (defun structural-editing-same-name-prev ()
    (interactive)
    (structural-editing-same-name t))

  (defun structural-editing-same-name-next ()
    (interactive)
    (structural-editing-same-name))

  (defun structural-editing-swap-prev ()
    "Swap this sexp with its previous sibling."
    (interactive)
    (with-bounds
      (if (not (and b (ignore-errors (scan-sexps s -1))))
          (structural-editing--nope "First node in this form")
        (structural-editing-prev)
        (structural-editing-swap-next)
        (structural-editing-prev))))

  (defun structural-editing-swap-next ()
    "Swap this sexp with its next sibling."
    (interactive)
    (with-bounds
     (if (not (and b (ignore-errors (scan-sexps e 1))))
         (structural-editing--nope "Last node in this form")
       (goto-char e)
       (transpose-sexps 1)
       (backward-sexp 1)
       (structural-editing-normalize))))

  (defun structural-editing-raise ()
    "Replace the enclosing form with this node."
    (interactive)
    (with-bounds 
     (goto-char s)
     (raise-sexp)
     (structural-editing-normalize)))

  (defun structural-editing-splice-parent ()
    (interactive))

  (defun structural-editing-splice-node ()
    (interactive))

  ;; TODO: don't do anything on failure
  (defun structural-editing-slurp-forward ()
    "Pull the next sibling of this form inside it."
    (interactive)
    (structural-editing--op #'sp-forward-slurp-sexp 'open))

  (defun structural-editing-barf-forward ()
    "Push this form's last child out to the right."
    (interactive)
    (structural-editing--op #'sp-forward-barf-sexp 'open))

  ;; TODO: don't do anything on failure
  (defun structural-editing-slurp-backward ()
    "Pull the previous sibling of this form inside it."
    (interactive)
    (structural-editing--op #'sp-backward-slurp-sexp 'close))

  (defun structural-editing-barf-backward ()
    "Push this form's first child out to the left."
    (interactive)
    (structural-editing--op #'sp-backward-barf-sexp 'close))

  (defun structural-editing-ret ()
    (interactive))

  (defun structural-editing-delete ()
    (interactive)
    (my/kill-whitespace-or-sexp))

  (defun structural-editing-backspace ()
    (interactive)
    (my/kill-whitespace-or-sexp-backward))

  (defun structural-editing-copy ()
    "Copy this sexp."
    (interactive)
    (let ((b (structural-editing-node-bounds)))
      (copy-region-as-kill (car b) (cdr b))
      (message "Copied sexp")))

  (defun structural-editing-yank ()
    (interactive)
    (let ((b (structural-editing-node-bounds)))
      (goto-char (cdr b))
      (insert " ")
      (let ((start (point)))
        (yank)
        (goto-char start)
        (structural-editing-normalize))))

  (defun structural-editing-wrap (&optional open)
    "Wrap this node in a new form and select it.
OPEN defaults to `('."
    (interactive)
    (with-bounds 
     (let* ((open (or open ?\())
            (close (matching-paren open)))
       (save-excursion
         (goto-char e) (insert close)
         (goto-char s) (insert open))
       (goto-char s)))))


;;;; ---------------------------------------------------------------------
;;;; Keymap

(defvar structural-editing-mode-map (make-sparse-keymap))

(defun structural-editing-bind (keymap)
  "Install the structural bindings into KEYMAP."
  (cl-flet ((setk (ks f)
              (dolist (k (if (listp ks) ks (list ks)))
                (define-key keymap (kbd k) f))))

    ;; Tree motion. Left/right walk siblings, up/down walk depth.
    (setk '("C-j" "C-<left>"  "j" "<left>")   #'structural-editing-prev)
    (setk '("C-l" "C-<right>" "l" "<right>")  #'structural-editing-next)
    (setk '("s-i" "C-<up>"    "i" "<up>")     #'structural-editing-ascend)
    (setk '("C-k" "C-<down>"  "k" "<down>")   #'structural-editing-descend)
    
    (setk '("<home>")     #'structural-editing-first-in-line)
    (setk '("<end>")      #'structural-editing-last-in-line)

    (setk '("C-<home>")   #'structural-editing-first-in-list)
    (setk '("C-<end>")    #'structural-editing-last-in-list)

    ;; Same name
    (setk '("C-M-j" "C-M-<left>")  #'structural-editing-same-name-prev)
    (setk '("C-M-l" "C-M-<right>") #'structural-editing-same-name-next)

    ;; Reorder siblings: the cursor follows the node it moved.
    (setk '("M-<left>"  "M-j") #'structural-editing-swap-prev)
    (setk '("M-<right>" "M-l") #'structural-editing-swap-next)

    ;; Change nesting depth: up loses a level, down gains one.
    (setk '("M-<up>"   "M-i") #'structural-editing-splice-parent)
    (setk '("M-<down>" "M-k") #'structural-editing-wrap)

    ;; Raise & new lists
    (setk "C-r" #'structural-editing-raise)
    (setk "(" #'structural-editing-wrap)
    (setk ")" #'structural-editing-splice-node)

    ;; Slurp & barf
    (setk "M-]" #'structural-editing-slurp-forward)
    (setk "M-[" #'structural-editing-barf-forward)
    (setk "M-o" #'structural-editing-slurp-backward)
    (setk "M-p" #'structural-editing-barf-backward)

    ;; Smart return & spacebar
    (setk "<return>"    #'structural-editing-ret)
    (setk "SPC"     #'structural-editing-next~)
    (setk "S-SPC"   #'structural-editing-prev~)

    ;; Deletion
    (setk "<delete>"    #'structural-editing-delete)
    (setk "<backspace>" #'structural-editing-backspace)

    ;; Copy & Paste
    (setk "M-w"         #'structural-editing-copy)
    (setk "C-y"         #'structural-editing-yank)

    ;; Escape hatches into character editing.
    ;; ...
   
    keymap))

(structural-editing-bind structural-editing-mode-map)


;;;; ---------------------------------------------------------------------
;;;; Mode

;;;###autoload
(define-minor-mode structural-editing-mode
  "Structural editing where the cursor is a sexp, not a character."
  :lighter " S-Edt"
  :keymap structural-editing-mode-map
  (cond
   (structural-editing-mode
    (when structural-editing-hide-cursor
      (setq structural-editing--saved-cursor cursor-type)
      (setq-local cursor-type nil))
    (add-hook 'post-command-hook #'structural-editing--post-command nil t)
    (structural-editing-normalize)
    (structural-editing--refresh))
   (t
    (with-bounds
     (remove-hook 'post-command-hook #'structural-editing--post-command t)
     (when (overlayp structural-editing--ov) (delete-overlay structural-editing--ov))
     (setq structural-editing--ov nil)
     (setq-local cursor-type structural-editing--saved-cursor)
     (goto-char e)))))

;; Getting back in from character editing.
(dolist (map (list emacs-lisp-mode-map lisp-mode-shared-map))
  (define-key map (kbd "<escape>") (lambda ()
                                     (interactive)
                                     (structural-insert-mode -1)
                                     ;; Walk one char backwards to avoid jumping to the next sexp.
                                     (unless structural-editing-mode (goto-char (1- (point))))
                                     (structural-editing-mode 'toggle))))

(provide 'structural-editing)
;;; structural-editing.el ends here
