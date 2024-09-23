
;;; =====================================================================
;;;              Global variables and customization options


(defgroup dev-tree nil
  "Like a jupyter notebook but with pure functions and a tree instead of cells.")

(defcustom dev-tree-mode-lighter " Dev-Tree"
  "Lighter displayed in mode line when `dev-tree-mode' is enabled."
  :group 'dev-tree
  :type 'string)

(defcustom dev-tree-visualizer-node-names t
  "When non-nil, display the name of each node in dev-tree visualizer."
  :group 'dev-tree
  :type 'boolean)

(defcustom dev-tree-visualizer-lazy-drawing 100
  "When non-nil, use lazy dev-tree drawing in visualizer.

Setting this to a number causes the visualizer to switch to lazy
drawing when the number of nodes in the tree is larger than this
value.

Lazy drawing means that only the visible portion of the tree will
be drawn initially, and the tree will be extended later as
needed. For the most part, the only visible effect of this is to
significantly speed up displaying the visualizer for very large
trees.

There is one potential negative effect of lazy drawing. Other
branches of the tree will only be drawn once the node from which
they branch off becomes visible. So it can happen that certain
portions of the tree that would be shown with lazy drawing
disabled, will not be drawn immediately when it is
enabled. However, this effect is quite rare in practice."
  :group 'dev-tree
  :type '(choice (const :tag "never" nil)
		 (const :tag "always" t)
		 (integer :tag "> size")))

(defface dev-tree-visualizer-default-face
  '((((class color)) :foreground "gray"))
  "Face used to draw dev-tree in visualizer."
  :group 'dev-tree)

(defface dev-tree-visualizer-current-face
  '((((class color)) :foreground "red"))
  "Face used to highlight current dev-tree node in visualizer."
  :group 'dev-tree)

(defface dev-tree-visualizer-active-branch-face
  '((((class color) (background dark))
     (:foreground "white" :weight bold))
    (((class color) (background light))
     (:foreground "black" :weight bold)))
  "Face used to highlight active dev-tree branch in visualizer."
  :group 'dev-tree)

(defface dev-tree-visualizer-register-face
  '((((class color)) :foreground "yellow"))
  "Face used to highlight dev-tree nodes saved to a register
in visualizer."
  :group 'dev-tree)

(defface dev-tree-visualizer-unmodified-face
  '((((class color)) :foreground "cyan"))
  "Face used to highlight nodes corresponding to unmodified buffers
in visualizer."
  :group 'dev-tree)

(defvar dev-tree-visualizer-parent-buffer nil
  "Parent buffer in visualizer.")
(put 'dev-tree-visualizer-parent-buffer 'permanent-local t)
(make-variable-buffer-local 'dev-tree-visualizer-parent-buffer)

(defvar dev-tree-visualizer-spacing nil
  "Stores current horizontal spacing needed for drawing dev-tree.")
(put 'dev-tree-visualizer-spacing 'permanent-local t)
(make-variable-buffer-local 'dev-tree-visualizer-spacing)

(defsubst dev-tree-visualizer-calculate-spacing ()
  "Calculate horizontal spacing required for drawing tree with current settings."
  (if dev-tree-visualizer-node-names 13 3))

(defvar dev-tree-visualizer-initial-node nil
  "Holds node that was current when visualizer was invoked.")
(put 'dev-tree-visualizer-initial-node 'permanent-local t)
(make-variable-buffer-local 'dev-tree-visualizer-initial-node)

(defvar dev-tree-visualizer-selected-node nil
  "Holds currently selected node in visualizer selection mode.")
(put 'dev-tree-visualizer-selected-node 'permanent-local t)
(make-variable-buffer-local 'dev-tree-visualizer-selected)

(defvar dev-tree-visualizer-needs-extending-down nil
  "Used to store nodes at edge of currently drawn portion of tree.")
(put 'dev-tree-visualizer-needs-extending-down 'permanent-local t)
(make-variable-buffer-local 'dev-tree-visualizer-needs-extending-down)

(defvar dev-tree-visualizer-needs-extending-up nil
  "Used to store nodes at edge of currently drawn portion of tree.")
(put 'dev-tree-visualizer-needs-extending-up 'permanent-local t)
(make-variable-buffer-local 'dev-tree-visualizer-needs-extending-up)

(defvar dev-tree-inhibit-kill-visualizer nil
  "Dynamically bound to t when deving from visualizer, to inhibit
`dev-tree-kill-visualizer' hook function in parent buffer.")

(defvar dev-tree-insert-face nil
  "Can be let-bound to a face name, used in drawing functions.")
 
(defconst dev-tree-visualizer-buffer-name " *dev-tree*"
  "Visualizer buffer names.")



;;; =================================================================
;;;                          Default keymaps



;;; =====================================================================
;;;                     Dev-tree data structure


(cl-defstruct
  (dev-tree
   :named
   (:constructor nil)
   (:constructor make-dev-tree
                 (&aux
                  (root (dev-tree-make-node nil nil))
                  (current root)
                  (size 0)
		  (count 0)
		  (object-pool (make-hash-table :test 'eq :weakness 'value))))
   (:copier nil))
  root current size count object-pool)

(defun dev-tree-copy (tree)
  ;; Return a copy of dev-tree TREE.
  (unwind-protect
      (let ((new (make-dev-tree)))
	(dev-tree-decircle tree)
	(let ((max-lisp-eval-depth (* 100 (dev-tree-count tree)))
	      (max-specpdl-size (* 100 (dev-tree-count tree))))
	  (setf (dev-tree-root new)
		(dev-tree-node-copy (dev-tree-root tree)
				     new (dev-tree-current tree))))
	(setf (dev-tree-size new)
	      (dev-tree-size tree))
	(setf (dev-tree-count new)
	      (dev-tree-count tree))
	(setf (dev-tree-object-pool new)
	      (copy-hash-table (dev-tree-object-pool tree)))
	(dev-tree-recircle new)
	new)
    (dev-tree-recircle tree)))


(cl-defstruct
  (dev-tree-node
   (:type vector)   ; create unnamed struct
   (:constructor nil)
   (:constructor dev-tree-make-node
                 (previous undo
		  &optional redo
                  &aux
                  (timestamp (current-time))
                  (branch 0)))
   (:constructor dev-tree-make-node-backwards
                 (next-node undo
		  &optional redo
                  &aux
                  (next (list next-node))
                  (timestamp (current-time))
                  (branch 0)))
   (:constructor dev-tree-make-empty-node ())
   (:constructor dev-tree-copy-node-save-data
		 (node
		  &aux
		  (undo (let ((changeset (dev-tree-node-undo node)))
			  (run-hook-wrapped
			   'dev-tree-pre-save-element-functions
			   (lambda (fun)
			     (setq changeset (delq nil (mapcar fun changeset)))
			     nil))
			  changeset))
		  (redo (let ((changeset (dev-tree-node-redo node)))
			  (run-hook-wrapped
			   'dev-tree-pre-save-element-functions
			   (lambda (fun)
			     (setq changeset (delq nil (mapcar fun changeset)))
			     nil))
			  changeset))
		  (timestamp (dev-tree-node-timestamp node))
		  (branch (dev-tree-node-branch node))
		  (meta-data (dev-tree-node-meta-data node))))
   (:copier nil))
  previous next undo redo timestamp branch meta-data)


(defmacro dev-tree-node-p (n)
  (let ((len (length (dev-tree-make-node nil nil))))
    `(and (vectorp ,n) (= (length ,n) ,len))))

(defun dev-tree-node-copy (node &optional tree current)
  ;; Return a deep copy of dev-tree NODE, sans previous link or meta-data.
  ;; If TREE and CURRENT are supplied, set (dev-tree-current TREE) to the
  ;; copy of CURRENT node, if found.
  (let* ((new (dev-tree-make-empty-node))
	 (stack (list (cons node new)))
	 n)
    (while (setq n (pop stack))
      (setf (dev-tree-node-undo (cdr n))
	    (copy-tree (dev-tree-node-undo (car n)) 'copy-vectors))
      (setf (dev-tree-node-redo (cdr n))
	    (copy-tree (dev-tree-node-redo (car n)) 'copy-vectors))
      (setf (dev-tree-node-timestamp (cdr n))
	    (copy-sequence (dev-tree-node-timestamp (car n))))
      (setf (dev-tree-node-branch (cdr n))
	    (dev-tree-node-branch (car n)))
      (setf (dev-tree-node-next (cdr n))
	    (mapcar (lambda (_) (dev-tree-make-empty-node))
		    (make-list (length (dev-tree-node-next (car n))) nil)))
    ;; set (dev-tree-current TREE) to copy if we've found CURRENT
    (when (and tree (eq (car n) current))
      (setf (dev-tree-current tree) (cdr n)))
    ;; recursively copy next nodes
    (let ((next0 (dev-tree-node-next (car n)))
	  (next1 (dev-tree-node-next (cdr n))))
      (while (and next0 next1)
	(push (cons (pop next0) (pop next1)) stack))))
    new))


(cl-defstruct
  (dev-tree-region-data
   (:type vector)   ; create unnamed struct
   (:constructor nil)
   (:constructor dev-tree-make-region-data
		 (&optional undo-beginning undo-end
			     redo-beginning redo-end))
   (:constructor dev-tree-make-undo-region-data
		 (undo-beginning undo-end))
   (:constructor dev-tree-make-redo-region-data
		 (redo-beginning redo-end))
   (:copier nil))
  undo-beginning undo-end redo-beginning redo-end)


(defmacro dev-tree-region-data-p (r)
  (let ((len (length (dev-tree-make-region-data))))
    `(and (vectorp ,r) (= (length ,r) ,len))))

(defmacro dev-tree-node-clear-region-data (node)
  `(setf (dev-tree-node-meta-data ,node)
	 (delq nil
	       (delq :region
		     (plist-put (dev-tree-node-meta-data ,node)
				:region nil)))))

(cl-defstruct
  (dev-tree-visualizer-data
   (:type vector)   ; create unnamed struct
   (:constructor nil)
   (:constructor dev-tree-make-visualizer-data
		 (&optional lwidth cwidth rwidth marker))
   (:copier nil))
  lwidth cwidth rwidth marker)


(defmacro dev-tree-visualizer-data-p (v)
  (let ((len (length (dev-tree-make-visualizer-data))))
    `(and (vectorp ,v) (= (length ,v) ,len))))

(defun dev-tree-node-clear-visualizer-data (node)
  (let ((plist (dev-tree-node-meta-data node)))
    (if (eq (car plist) :visualizer)
	(setf (dev-tree-node-meta-data node) (nthcdr 2 plist))
      (while (and plist (not (eq (cadr plist) :visualizer)))
	(setq plist (cdr plist)))
      (if plist (setcdr plist (nthcdr 3 plist))))))

(defmacro dev-tree-node-lwidth (node)
  `(let ((v (plist-get (dev-tree-node-meta-data ,node) :visualizer)))
     (when (dev-tree-visualizer-data-p v)
       (dev-tree-visualizer-data-lwidth v))))

(defmacro dev-tree-node-cwidth (node)
  `(let ((v (plist-get (dev-tree-node-meta-data ,node) :visualizer)))
     (when (dev-tree-visualizer-data-p v)
       (dev-tree-visualizer-data-cwidth v))))

(defmacro dev-tree-node-rwidth (node)
  `(let ((v (plist-get (dev-tree-node-meta-data ,node) :visualizer)))
     (when (dev-tree-visualizer-data-p v)
       (dev-tree-visualizer-data-rwidth v))))

(defmacro dev-tree-node-marker (node)
  `(let ((v (plist-get (dev-tree-node-meta-data ,node) :visualizer)))
     (when (dev-tree-visualizer-data-p v)
       (dev-tree-visualizer-data-marker v))))


(gv-define-setter dev-tree-node-lwidth (val node)
  `(let ((v (plist-get (dev-tree-node-meta-data ,node) :visualizer)))
     (unless (dev-tree-visualizer-data-p v)
       (setf (dev-tree-node-meta-data ,node)
	     (plist-put (dev-tree-node-meta-data ,node) :visualizer
			(setq v (dev-tree-make-visualizer-data)))))
     (setf (dev-tree-visualizer-data-lwidth v) ,val)))

(gv-define-setter dev-tree-node-cwidth (val node)
  `(let ((v (plist-get (dev-tree-node-meta-data ,node) :visualizer)))
     (unless (dev-tree-visualizer-data-p v)
       (setf (dev-tree-node-meta-data ,node)
	     (plist-put (dev-tree-node-meta-data ,node) :visualizer
			(setq v (dev-tree-make-visualizer-data)))))
     (setf (dev-tree-visualizer-data-cwidth v) ,val)))

(gv-define-setter dev-tree-node-rwidth (val node)
  `(let ((v (plist-get (dev-tree-node-meta-data ,node) :visualizer)))
     (unless (dev-tree-visualizer-data-p v)
       (setf (dev-tree-node-meta-data ,node)
	     (plist-put (dev-tree-node-meta-data ,node) :visualizer
			(setq v (dev-tree-make-visualizer-data)))))
     (setf (dev-tree-visualizer-data-rwidth v) ,val)))

(gv-define-setter dev-tree-node-marker (val node)
  `(let ((v (plist-get (dev-tree-node-meta-data ,node) :visualizer)))
     (unless (dev-tree-visualizer-data-p v)
       (setf (dev-tree-node-meta-data ,node)
	     (plist-put (dev-tree-node-meta-data ,node) :visualizer
			(setq v (dev-tree-make-visualizer-data)))))
     (setf (dev-tree-visualizer-data-marker v) ,val)))



(cl-defstruct
  (dev-tree-register-data
   (:type vector)
   (:constructor nil)
   (:constructor dev-tree-make-register-data (buffer node)))
  buffer node)

(defun dev-tree-register-data-p (data)
  (and (vectorp data)
       (= (length data) 2)
       (dev-tree-node-p (dev-tree-register-data-node data))))

(defun dev-tree-register-data-print-func (data)
  (princ (format "an dev-tree state for buffer %s"
		 (dev-tree-register-data-buffer data))))

(defmacro dev-tree-node-register (node)
  `(plist-get (dev-tree-node-meta-data ,node) :register))

(gv-define-setter dev-tree-node-register (val node)
  `(setf (dev-tree-node-meta-data ,node)
	 (plist-put (dev-tree-node-meta-data ,node) :register ,val)))



;;; =====================================================================
;;;              Basic dev-tree data structure functions


(defun dev-tree-grow (undo)
  "Add an UNDO node to current branch of `buffer-dev-tree'."
  (let* ((current (dev-tree-current buffer-dev-tree))
         (new (dev-tree-make-node current undo)))
    (push new (dev-tree-node-next current))
    (setf (dev-tree-current buffer-dev-tree) new)))


(defun dev-tree-grow-backwards (node undo &optional redo)
  "Add new node *above* dev-tree NODE, and return new node.
Note that this will overwrite NODE's \"previous\" link, so should
only be used on a detached NODE, never on nodes that are already
part of `buffer-dev-tree'."
  (let ((new (dev-tree-make-node-backwards node undo redo)))
    (setf (dev-tree-node-previous node) new)
    new))


(defun dev-tree-splice-node (node splice)
  "Splice NODE into undo tree, below node SPLICE.
Note that this will overwrite NODE's \"next\" and \"previous\"
links, so should only be used on a detached NODE, never on nodes
that are already part of `buffer-dev-tree'."
  (setf (dev-tree-node-next node) (dev-tree-node-next splice)
	(dev-tree-node-branch node) (dev-tree-node-branch splice)
	(dev-tree-node-previous node) splice
	(dev-tree-node-next splice) (list node)
	(dev-tree-node-branch splice) 0)
  (dolist (n (dev-tree-node-next node))
    (setf (dev-tree-node-previous n) node)))


(defun dev-tree-snip-node (node)
  "Snip NODE out of undo tree."
  (let* ((parent (dev-tree-node-previous node))
	 position p)
    ;; if NODE is only child, replace parent's next links with NODE's
    (if (= (length (dev-tree-node-next parent)) 0)
	(setf (dev-tree-node-next parent) (dev-tree-node-next node)
	      (dev-tree-node-branch parent) (dev-tree-node-branch node))
      ;; otherwise...
      (setq position (dev-tree-position node (dev-tree-node-next parent)))
      (cond
       ;; if active branch used do go via NODE, set parent's branch to active
       ;; branch of NODE
       ((= (dev-tree-node-branch parent) position)
	(setf (dev-tree-node-branch parent)
	      (+ position (dev-tree-node-branch node))))
       ;; if active branch didn't go via NODE, update parent's branch to point
       ;; to same node as before
       ((> (dev-tree-node-branch parent) position)
	(cl-incf (dev-tree-node-branch parent)
		 (1- (length (dev-tree-node-next node))))))
      ;; replace NODE in parent's next list with NODE's entire next list
      (if (= position 0)
	  (setf (dev-tree-node-next parent)
		(nconc (dev-tree-node-next node)
		       (cdr (dev-tree-node-next parent))))
	(setq p (nthcdr (1- position) (dev-tree-node-next parent)))
	(setcdr p (nconc (dev-tree-node-next node) (cddr p)))))
    ;; update previous links of NODE's children
    (dolist (n (dev-tree-node-next node))
      (setf (dev-tree-node-previous n) parent))))


(defun dev-tree-mapc (--dev-tree-mapc-function-- node)
  ;; Apply FUNCTION to NODE and to each node below it.
  (let ((stack (list node))
	n)
    (while (setq n (pop stack))
      (funcall --dev-tree-mapc-function-- n)
      (setq stack (append (dev-tree-node-next n) stack)))))


(defmacro dev-tree-num-branches ()
  "Return number of branches at current undo tree node."
  '(length (dev-tree-node-next (dev-tree-current buffer-dev-tree))))


(defun dev-tree-position (node list)
  "Find the first occurrence of NODE in LIST.
Return the index of the matching item, or nil of not found.
Comparison is done with `eq'."
  (let ((i 0))
    (catch 'found
      (while (progn
               (when (eq node (car list)) (throw 'found i))
               (cl-incf i)
               (setq list (cdr list))))
      nil)))


(defvar *dev-tree-id-counter* 0)
(make-variable-buffer-local '*dev-tree-id-counter*)

(defmacro dev-tree-generate-id ()
  ;; Generate a new, unique id (uninterned symbol).
  ;; The name is made by appending a number to "dev-tree-id".
  ;; (Copied from CL package `gensym'.)
  `(let ((num (prog1 *dev-tree-id-counter*
		(cl-incf *dev-tree-id-counter*))))
     (make-symbol (format "dev-tree-id%d" num))))


(defun dev-tree-decircle (dev-tree)
  ;; Nullify PREVIOUS links of DEV-TREE nodes, to make DEV-TREE data
  ;; structure non-circular.
  (dev-tree-mapc
   (lambda (node)
     (dolist (n (dev-tree-node-next node))
       (setf (dev-tree-node-previous n) nil)))
   (dev-tree-root dev-tree)))


(defun dev-tree-recircle (dev-tree)
  ;; Recreate PREVIOUS links of DEV-TREE nodes, to restore circular DEV-TREE
  ;; data structure.
  (dev-tree-mapc
   (lambda (node)
     (dolist (n (dev-tree-node-next node))
       (setf (dev-tree-node-previous n) node)))
   (dev-tree-root dev-tree)))



;;; =====================================================================
;;;                   Visualizer utility functions


(defun dev-tree-compute-widths (node)
  "Recursively compute widths for nodes below NODE."
  (let ((stack (list node))
        res)
    (while stack
      ;; try to compute widths for node at top of stack
      (if (dev-tree-node-p
           (setq res (dev-tree-node-compute-widths (car stack))))
          ;; if computation fails, it returns a node whose widths still need
          ;; computing, which we push onto the stack
          (push res stack)
        ;; otherwise, store widths and remove it from stack
        (setf (dev-tree-node-lwidth (car stack)) (aref res 0)
              (dev-tree-node-cwidth (car stack)) (aref res 1)
              (dev-tree-node-rwidth (car stack)) (aref res 2))
        (pop stack)))))


(defun dev-tree-node-compute-widths (node)
  ;; Compute NODE's left-, centre-, and right-subtree widths. Returns widths
  ;; (in a vector) if successful. Otherwise, returns a node whose widths need
  ;; calculating before NODE's can be calculated.
  (let ((num-children (length (dev-tree-node-next node)))
        (lwidth 0) (cwidth 0) (rwidth 0) p)
    (catch 'need-widths
      (cond
       ;; leaf nodes have 0 width
       ((= 0 num-children)
        (setf cwidth 1
              (dev-tree-node-lwidth node) 0
              (dev-tree-node-cwidth node) 1
              (dev-tree-node-rwidth node) 0))

       ;; odd number of children
       ((= (mod num-children 2) 1)
        (setq p (dev-tree-node-next node))
        ;; compute left-width
        (dotimes (_ (/ num-children 2))
          (if (dev-tree-node-lwidth (car p))
              (cl-incf lwidth (+ (dev-tree-node-lwidth (car p))
                              (dev-tree-node-cwidth (car p))
                              (dev-tree-node-rwidth (car p))))
            ;; if child's widths haven't been computed, return that child
            (throw 'need-widths (car p)))
          (setq p (cdr p)))
        (if (dev-tree-node-lwidth (car p))
            (cl-incf lwidth (dev-tree-node-lwidth (car p)))
          (throw 'need-widths (car p)))
        ;; centre-width is inherited from middle child
        (setf cwidth (dev-tree-node-cwidth (car p)))
        ;; compute right-width
        (cl-incf rwidth (dev-tree-node-rwidth (car p)))
        (setq p (cdr p))
        (dotimes (_ (/ num-children 2))
          (if (dev-tree-node-lwidth (car p))
              (cl-incf rwidth (+ (dev-tree-node-lwidth (car p))
                              (dev-tree-node-cwidth (car p))
                              (dev-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p))))

       ;; even number of children
       (t
        (setq p (dev-tree-node-next node))
        ;; compute left-width
        (dotimes (_ (/ num-children 2))
          (if (dev-tree-node-lwidth (car p))
              (cl-incf lwidth (+ (dev-tree-node-lwidth (car p))
                              (dev-tree-node-cwidth (car p))
                              (dev-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p)))
        ;; centre-width is 0 when number of children is even
        (setq cwidth 0)
        ;; compute right-width
        (dotimes (_ (/ num-children 2))
          (if (dev-tree-node-lwidth (car p))
              (cl-incf rwidth (+ (dev-tree-node-lwidth (car p))
                              (dev-tree-node-cwidth (car p))
                              (dev-tree-node-rwidth (car p))))
            (throw 'need-widths (car p)))
          (setq p (cdr p)))))

      ;; return left-, centre- and right-widths
      (vector lwidth cwidth rwidth))))


(defun dev-tree-clear-visualizer-data (tree)
  ;; Clear visualizer data below NODE.
  (dev-tree-mapc
   (lambda (n) (dev-tree-node-clear-visualizer-data n))
   (dev-tree-root tree)))


(defun dev-tree-node-unmodified-p (node &optional mtime)
  ;; Return non-nil if NODE corresponds to a buffer state that once upon a
  ;; time was unmodified. If a file modification time MTIME is specified,
  ;; return non-nil if the corresponding buffer state really is unmodified.
  (let* ((changeset
	  (or (dev-tree-node-redo node)
	      (and (setq changeset (car (dev-tree-node-next node)))
		   (dev-tree-node-undo changeset))))
	 (ntime
	  (let ((elt (car (last changeset))))
	    (and (consp elt) (eq (car elt) t) (consp (cdr elt))
		 (cdr elt)))))
    (and ntime
	 (or (null mtime)
	     ;; high-precision timestamps
	     (if (listp (cdr ntime))
		 (equal ntime mtime)
	       ;; old-style timestamps
	       (and (= (car ntime) (car mtime))
		    (= (cdr ntime) (cadr mtime))))))))



;;; =====================================================================
;;;                        Dev-Tree commands


(defvar dev-tree-timer nil)

;;;###autoload
(define-minor-mode dev-tree-mode
  "Toggle dev-tree mode.
With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

dev-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `dev-tree-mode':

  \\{dev-tree-map}

Within the dev-tree visualizer, the following keys are available:

  \\{dev-tree-visualizer-mode-map}"

  nil                       ; init value
  dev-tree-mode-lighter    ; lighter
  dev-tree-map             ; keymap

  (cond
   (dev-tree-mode  ; enabling `dev-tree-mode'
    (set (make-local-variable 'undo-limit)
	 (if dev-tree-limit
	     (max undo-limit dev-tree-limit)
	   most-positive-fixnum))
    (set (make-local-variable 'undo-strong-limit)
	 (if dev-tree-limit
	     (max undo-strong-limit dev-tree-strong-limit)
	   most-positive-fixnum))
    (set (make-local-variable 'undo-outer-limit)  ; null `undo-outer-limit' means no limit
	 (when (and dev-tree-limit undo-outer-limit undo-outer-limit)
	   (max undo-outer-limit dev-tree-outer-limit)))
    (when (null dev-tree-limit)
      (setq dev-tree-timer
	    (run-with-idle-timer 5 'repeat #'undo-list-transfer-to-tree)))
    (add-hook 'post-gc-hook #'dev-tree-post-gc nil))

   (t  ; disabling `dev-tree-mode'
    ;; rebuild `buffer-undo-list' from tree so Emacs undo can work
    (undo-list-rebuild-from-tree)
    (setq buffer-dev-tree nil)
    (remove-hook 'post-gc-hook #'dev-tree-post-gc 'local)
    (when (timerp dev-tree-timer) (cancel-timer dev-tree-timer))
    (kill-local-variable 'undo-limit)
    (kill-local-variable 'undo-strong-limit)
    (kill-local-variable 'undo-outer-limit))))


(defun turn-on-dev-tree-mode (&optional print-message)
  "Enable `dev-tree-mode' in the current buffer, when appropriate.
Some major modes implement their own undo system, which should
not normally be overridden by `dev-tree-mode'. This command does
not enable `dev-tree-mode' in such buffers. If you want to force
`dev-tree-mode' to be enabled regardless, use (dev-tree-mode 1)
instead.

The heuristic used to detect major modes in which
`dev-tree-mode' should not be used is to check whether either
the `undo' command has been remapped, or the default undo
keybindings (C-/ and C-_) have been overridden somewhere other
than in the global map. In addition, `dev-tree-mode' will not be
enabled if the buffer's `major-mode' appears in
`dev-tree-incompatible-major-modes'."
  (interactive "p")
  (if (or (key-binding [remap undo])
	  (dev-tree-overridden-undo-bindings-p)
	  (memq major-mode dev-tree-incompatible-major-modes))
      (when print-message
	(message "Buffer does not support dev-tree-mode;\
 dev-tree-mode NOT enabled"))
    (dev-tree-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-dev-tree-mode
  dev-tree-mode turn-on-dev-tree-mode)

(defun dev-tree-switch-branch (branch)
  "Switch to a different BRANCH of the undo tree.
This will affect which branch to descend when *redoing* changes
using `dev-tree-redo'."
  (interactive (list (or (and prefix-arg (prefix-numeric-value prefix-arg))
                         (and (not (eq buffer-undo-list t))
			      (undo-list-transfer-to-tree)
			      (let ((b (dev-tree-node-branch
					(dev-tree-current
					 buffer-dev-tree))))
				(cond
				 ;; switch to other branch if only 2
				 ((= (dev-tree-num-branches) 2) (- 1 b))
				 ;; prompt if more than 2
				 ((> (dev-tree-num-branches) 2)
				  (read-number
				   (format "Branch (0-%d, on %d): "
					   (1- (dev-tree-num-branches)) b)))
				 ))))))
  (unless dev-tree-mode
    (user-error "dev-tree mode not enabled in buffer"))
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t)
    (user-error "No undo information in this buffer"))
  ;; sanity check branch number
  (when (<= (dev-tree-num-branches) 1)
    (user-error "Not at undo branch point"))
  (when (or (< branch 0) (> branch (1- (dev-tree-num-branches))))
    (user-error "Invalid branch number"))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-dev-tree'
  (undo-list-transfer-to-tree)
  ;; switch branch
  (setf (dev-tree-node-branch (dev-tree-current buffer-dev-tree))
	branch)
  (message "Switched to branch %d" branch))


(defun dev-tree-set (node &optional preserve-timestamps)
  ;; Set buffer to state corresponding to NODE. Returns intersection point
  ;; between path back from current node and path back from selected NODE.
  ;; Non-nil PRESERVE-TIMESTAMPS disables updating of timestamps in visited
  ;; dev-tree nodes. (This should *only* be used when temporarily visiting
  ;; another undo state and immediately returning to the original state
  ;; afterwards. Otherwise, it could cause history-discarding errors.)
  (let ((path (make-hash-table :test 'eq))
        (n node))
    (puthash (dev-tree-root buffer-dev-tree) t path)
    ;; build list of nodes leading back from selected node to root, updating
    ;; branches as we go to point down to selected node
    (while (progn
             (puthash n t path)
             (when (dev-tree-node-previous n)
               (setf (dev-tree-node-branch (dev-tree-node-previous n))
                     (dev-tree-position
                      n (dev-tree-node-next (dev-tree-node-previous n))))
               (setq n (dev-tree-node-previous n)))))
    ;; work backwards from current node until we intersect path back from
    ;; selected node
    (setq n (dev-tree-current buffer-dev-tree))
    (while (not (gethash n path))
      (setq n (dev-tree-node-previous n)))
    ;; ascend tree until intersection node
    (while (not (eq (dev-tree-current buffer-dev-tree) n))
      (dev-tree-undo-1 nil nil preserve-timestamps))
    ;; descend tree until selected node
    (while (not (eq (dev-tree-current buffer-dev-tree) node))
      (dev-tree-redo-1 nil nil preserve-timestamps))
    n))

(defun dev-tree-save-state-to-register (register)
  "Store current dev-tree state to REGISTER.
The saved state can be restored using
`dev-tree-restore-state-from-register'.
Argument is a character, naming the register."
  (interactive "cdev-tree state to register: ")
  (unless dev-tree-mode
    (user-error "dev-tree mode not enabled in buffer"))
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t)
    (user-error "No undo information in this buffer"))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-dev-tree'
  (undo-list-transfer-to-tree)
  ;; save current node to REGISTER
  (set-register
   register (registerv-make
	     (dev-tree-make-register-data
	      (current-buffer) (dev-tree-current buffer-dev-tree))
	     :print-func 'dev-tree-register-data-print-func))
  ;; record REGISTER in current node, for visualizer
  (setf (dev-tree-node-register (dev-tree-current buffer-dev-tree))
	register))



(defun dev-tree-restore-state-from-register (register)
  "Restore dev-tree state from REGISTER.
The state must be saved using `dev-tree-save-state-to-register'.
Argument is a character, naming the register."
  (interactive "*cRestore dev-tree state from register: ")
  (unless dev-tree-mode
    (user-error "dev-tree mode not enabled in buffer"))
  ;; throw error if undo is disabled in buffer, or if register doesn't contain
  ;; an dev-tree node
  (let ((data (registerv-data (get-register register))))
    (cond
     ((eq buffer-undo-list t)
      (user-error "No undo information in this buffer"))
     ((not (dev-tree-register-data-p data))
      (user-error "Register doesn't contain dev-tree state"))
     ((not (eq (current-buffer) (dev-tree-register-data-buffer data)))
      (user-error "Register contains dev-tree state for a different buffer")))
    ;; transfer entries accumulated in `buffer-undo-list' to `buffer-dev-tree'
    (undo-list-transfer-to-tree)
    ;; restore buffer state corresponding to saved node
    (dev-tree-set (dev-tree-register-data-node data))))


(defun dev-tree-make-history-save-file-name (file)
  "Create the undo history file name for FILE.
Normally this is the file's name with \".\" prepended and
\".~dev-tree~\" appended.

A match for FILE is sought in `dev-tree-history-directory-alist'
\(see the documentation of that variable for details\). If the
directory for the backup doesn't exist, it is created."
  (let* ((backup-directory-alist dev-tree-history-directory-alist)
	 (name (make-backup-file-name-1 file)))
    (concat (file-name-directory name) "." (file-name-nondirectory name)
	    ".~dev-tree~")))


(defun dev-tree-serialize (tree)
  "Serialise dev-tree TREE to current buffer."
  ;; write root
  (let ((data (dev-tree-copy-node-save-data (dev-tree-root tree))))
    (when (eq (dev-tree-root tree) (dev-tree-current tree))
      (setf (dev-tree-node-next data) 'current))
    (prin1 data (current-buffer)))
  (terpri (current-buffer))
  ;; Note: We serialise in breadth-first order, as dev-trees are typically
  ;;       much deeper than they are wide, so this is more memory-efficient.
  (let ((queue (make-queue)))
    (queue-enqueue queue (dev-tree-root tree))
    (while (not (queue-empty queue))
      (prin1 (mapcar
	      (lambda (n)
		(queue-enqueue queue n)
		(let ((data (dev-tree-copy-node-save-data n)))
		  ;; use empty next field to mark current node
		  (when (eq n (dev-tree-current tree))
		    (setf (dev-tree-node-next data) 'current))
		  data))
	      (dev-tree-node-next (queue-dequeue queue)))
	     (current-buffer))
      (terpri (current-buffer)))))


(defun dev-tree-deserialize ()
  "Deserialize and return dev-tree from current buffer."
  (let ((tree (make-dev-tree))
	(queue (make-queue))
	node)
    ;; read root
    (setf (dev-tree-root tree) (read (current-buffer)))
    (queue-enqueue queue (dev-tree-root tree))
    ;; reconstruct tree in breadth-first order
    (while (not (queue-empty queue))
      (setq node (queue-dequeue queue))
      (when (eq (dev-tree-node-next node) 'current)
	(setf (dev-tree-current tree) node))
      (setf (dev-tree-node-next node) (read (current-buffer)))
      (mapc (lambda (n) (queue-enqueue queue n))
	    (dev-tree-node-next node)))
    ;; restore parent links
    (dev-tree-recircle tree)
    tree))

(defun dev-tree-save-history (&optional filename overwrite)
  "Store dev-tree history to file.

If optional argument FILENAME is omitted, default save file is
\".<buffer-file-name>.~dev-tree\" if buffer is visiting a file.
Otherwise, prompt for one.

If OVERWRITE is non-nil, any existing file will be overwritten
without asking for confirmation."
  (interactive)
  (unless dev-tree-mode
    (user-error "dev-tree mode not enabled in buffer"))
  (when (eq buffer-undo-list t)
    (user-error "No undo information in this buffer"))
  (undo-list-transfer-to-tree)
  (when (and buffer-dev-tree (not (eq buffer-dev-tree t)))
    ;; (dev-tree-kill-visualizer)
    ;; ;; should be cleared already by killing the visualizer, but writes
    ;; ;; unreasable data if not for some reason, so just in case...
    ;; (dev-tree-clear-visualizer-data buffer-dev-tree)
    (let ((buff (current-buffer))
	  (tree buffer-dev-tree))
      ;; get filename
      (unless filename
	(setq filename
	      (if buffer-file-name
		  (dev-tree-make-history-save-file-name buffer-file-name)
		(expand-file-name (read-file-name "File to save in: ") nil))))
      (when (or (not (file-exists-p filename))
		overwrite
		(yes-or-no-p (format "Overwrite \"%s\"? " filename)))

	;; print dev-tree to file
	;; Note: We use `with-temp-buffer' instead of `with-temp-file' to
	;;       allow `auto-compression-mode' to take effect, in case user
	;;       has overridden or advised the default
	;;       `dev-tree-make-history-save-file-name' to add a compressed
	;;       file extension.
	(with-temp-buffer
	  ;; write version number; (original save file format (version 0) has no version string)
	  (unless (= dev-tree-save-format-version 0)
	    (prin1 (cons 'dev-tree-save-format-version dev-tree-save-format-version)
		   (current-buffer))
	    (terpri (current-buffer)))
	  ;; write hash
	  (prin1 (sha1 buff) (current-buffer))
	  (terpri (current-buffer))
	  ;; write tree
	  (cl-case dev-tree-save-format-version
	    (0 (dev-tree-serialize-old-format tree))
	    (1 (dev-tree-serialize tree))
	    (t (error "Unknown `dev-tree-save-format-version'; dev-tree history *not* saved")))
	  ;; write file
	  (with-auto-compression-mode
	    (write-region nil nil filename)))))))


(defmacro dev-tree--catch-load-history-error (error-fmt &rest body)
  `(condition-case nil
       (progn ,@body)
     (error
      (kill-buffer nil)
      (funcall (if noerror #'message #'user-error) ,error-fmt filename)
      (throw 'load-error nil))))


(defun dev-tree-load-history (&optional filename noerror)
  "Load dev-tree history from file, for the current buffer.

If optional argument FILENAME is null, default load file is
\".<buffer-file-name>.~dev-tree\" if buffer is visiting a file.
Otherwise, prompt for one.

If optional argument NOERROR is non-nil, return nil instead of
signaling an error if file is not found.

Note this will overwrite any existing undo history."
  (interactive)
  (unless dev-tree-mode
    (user-error "dev-tree mode not enabled in buffer"))
  ;; get filename
  (unless filename
    (setq filename
	  (if buffer-file-name
	      (dev-tree-make-history-save-file-name buffer-file-name)
	    (expand-file-name (read-file-name "File to load from: ") nil))))

  ;; attempt to read dev-tree
  (catch 'load-error
    (unless (file-exists-p filename)
      (if noerror
	  (throw 'load-error nil)
	(user-error "File \"%s\" does not exist; could not load dev-tree history"
		    filename)))

    ;; read file contents
    (let ((buff (current-buffer))
	  version hash tree)
	(with-temp-buffer
	  (with-auto-compression-mode (insert-file-contents filename))
	  (goto-char (point-min))

	  (dev-tree--catch-load-history-error
	   "Error reading dev-tree history from \"%s\""
	   ;; read version number
	   (setq version (read (current-buffer)))
	   ;; read hash
	   (cond
	    ((eq (car-safe version) 'dev-tree-save-format-version)
	     (setq version (cdr version))
	     (setq hash (read (current-buffer))))
	    ;; original save file format (version 0) has no version string
	    ((stringp version)
	     (setq hash version
		   version 0))
	    (t (error "Error"))))

	  ;; check hash
	  (dev-tree--catch-load-history-error
	    "Buffer has been modified since dev-tree history was saved to
	  \"%s\"; could not load dev-tree history"
	    (unless (string= (sha1 buff) hash) (error "Error")))

	  ;; read tree
	  (dev-tree--catch-load-history-error
	   "Error reading dev-tree history from \"%s\""
	   (setq tree
		 (cl-case version
		   (0 (dev-tree-deserialize-old-format))
		   (1 (dev-tree-deserialize))
		   (t (error "Error")))))
	  (kill-buffer nil))

	(setq buffer-dev-tree tree
	      buffer-undo-list (list nil 'dev-tree-canary)))))




;;; =====================================================================
;;;                    Visualizer drawing functions


(defun dev-tree-visualize ()
  "Visualize the current buffer's undo tree."
  (interactive "*")
  (unless dev-tree-mode
    (user-error "dev-tree mode not enabled in buffer"))
  (deactivate-mark)
  ;; throw error if undo is disabled in buffer
  (when (eq buffer-undo-list t)
    (user-error "No undo information in this buffer"))
  ;; transfer entries accumulated in `buffer-undo-list' to `buffer-dev-tree'
  (undo-list-transfer-to-tree)
  ;; add hook to kill visualizer buffer if original buffer is changed
  (add-hook 'before-change-functions 'dev-tree-kill-visualizer nil t)
  ;; prepare *dev-tree* buffer, then draw tree in it
  (let ((dev-tree buffer-dev-tree)
        (buff (current-buffer))
	(display-buffer-mark-dedicated 'soft))
    (switch-to-buffer-other-window
     (get-buffer-create dev-tree-visualizer-buffer-name))
    (setq dev-tree-visualizer-parent-buffer buff)
    (setq dev-tree-visualizer-parent-mtime
	  (and (buffer-file-name buff)
	       (nth 5 (file-attributes (buffer-file-name buff)))))
    (setq dev-tree-visualizer-initial-node (dev-tree-current dev-tree))
    (setq dev-tree-visualizer-spacing
	  (dev-tree-visualizer-calculate-spacing))
    (setq buffer-dev-tree dev-tree)
    (dev-tree-visualizer-mode)
    (setq buffer-dev-tree dev-tree)
    (set (make-local-variable 'dev-tree-visualizer-lazy-drawing)
	 (or (eq dev-tree-visualizer-lazy-drawing t)
	     (and (numberp dev-tree-visualizer-lazy-drawing)
		  (>= (dev-tree-count dev-tree)
		      dev-tree-visualizer-lazy-drawing))))
    (when dev-tree-visualizer-diff (dev-tree-visualizer-show-diff))
    (let ((inhibit-read-only t)) (dev-tree-draw-tree dev-tree))))


(defun dev-tree-kill-visualizer (&rest _dummy)
  ;; Kill visualizer. Added to `before-change-functions' hook of original
  ;; buffer when visualizer is invoked.
  (unless (or dev-tree-inhibit-kill-visualizer
	      (null (get-buffer dev-tree-visualizer-buffer-name)))
    (with-current-buffer dev-tree-visualizer-buffer-name
      (dev-tree-visualizer-quit))))



(defun dev-tree-draw-tree (dev-tree)
  ;; Draw dev-tree in current buffer starting from NODE (or root if nil).
  (let ((inhibit-read-only t)
	(node (if dev-tree-visualizer-lazy-drawing
		  (dev-tree-current dev-tree)
		(dev-tree-root dev-tree))))
    (erase-buffer)
    (setq dev-tree-visualizer-needs-extending-down nil
	  dev-tree-visualizer-needs-extending-up nil)
    (dev-tree-clear-visualizer-data dev-tree)
    (dev-tree-compute-widths node)
    ;; lazy drawing starts vertically centred and displaced horizontally to
    ;; the left (window-width/4), since trees will typically grow right
    (if dev-tree-visualizer-lazy-drawing
	(progn
	  (dev-tree-move-down (/ (window-height) 2))
	  (dev-tree-move-forward (max 2 (/ (window-width) 4)))) ; left margin
      ;; non-lazy drawing starts in centre at top of buffer
      (dev-tree-move-down 1)  ; top margin
      (dev-tree-move-forward
       (max (/ (window-width) 2)
	    (+ (dev-tree-node-char-lwidth node)
	       ;; add space for left part of left-most time-stamp
	       (if dev-tree-visualizer-timestamps
		   (/ (- dev-tree-visualizer-spacing 4) 2)
		 0)
	       2))))  ; left margin
    ;; link starting node to its representation in visualizer
    (setf (dev-tree-node-marker node) (make-marker))
    (set-marker-insertion-type (dev-tree-node-marker node) nil)
    (move-marker (dev-tree-node-marker node) (point))
    ;; draw dev-tree
    (let ((dev-tree-insert-face 'dev-tree-visualizer-default-face)
	  node-list)
      (if (not dev-tree-visualizer-lazy-drawing)
	  (dev-tree-extend-down node t)
	(dev-tree-extend-down node)
	(dev-tree-extend-up node)
	(setq node-list dev-tree-visualizer-needs-extending-down
	      dev-tree-visualizer-needs-extending-down nil)
	(while node-list (dev-tree-extend-down (pop node-list)))))
    ;; highlight active branch
    (let ((dev-tree-insert-face 'dev-tree-visualizer-active-branch-face))
      (dev-tree-highlight-active-branch
       (or dev-tree-visualizer-needs-extending-up
	   (dev-tree-root dev-tree))))
    ;; highlight current node
    (dev-tree-draw-node (dev-tree-current dev-tree) 'current)))


(defun dev-tree-extend-down (node &optional bottom)
  ;; Extend tree downwards starting from NODE and point. If BOTTOM is t,
  ;; extend all the way down to the leaves. If BOTTOM is a node, extend down
  ;; as far as that node. If BOTTOM is an integer, extend down as far as that
  ;; line. Otherwise, only extend visible portion of tree. NODE is assumed to
  ;; already have a node marker. Returns non-nil if anything was actually
  ;; extended.
  (let ((extended nil)
	(cur-stack (list node))
	next-stack)
    ;; don't bother extending if BOTTOM specifies an already-drawn node
    (unless (and (dev-tree-node-p bottom) (dev-tree-node-marker bottom))
      ;; draw nodes layer by layer
      (while (or cur-stack
		 (prog1 (setq cur-stack next-stack)
		   (setq next-stack nil)))
	(setq node (pop cur-stack))
	;; if node is within range being drawn...
	(if (or (eq bottom t)
		(and (dev-tree-node-p bottom)
		     (not (eq (dev-tree-node-previous node) bottom)))
		(and (integerp bottom)
		     (>= bottom (line-number-at-pos
				 (dev-tree-node-marker node))))
		(and (null bottom)
		     (pos-visible-in-window-p (dev-tree-node-marker node)
					      nil t)))
	    ;; ...draw one layer of node's subtree (if not already drawn)
	    (progn
	      (unless (and (dev-tree-node-next node)
			   (dev-tree-node-marker
			    (nth (dev-tree-node-branch node)
				 (dev-tree-node-next node))))
		(goto-char (dev-tree-node-marker node))
		(dev-tree-draw-subtree node)
		(setq extended t))
	      (setq next-stack
		    (append (dev-tree-node-next node) next-stack)))
	  ;; ...otherwise, postpone drawing until later
	  (push node dev-tree-visualizer-needs-extending-down))))
    extended))


(defun dev-tree-extend-up (node &optional top)
  ;; Extend tree upwards starting from NODE. If TOP is t, extend all the way
  ;; to root. If TOP is a node, extend up as far as that node. If TOP is an
  ;; integer, extend up as far as that line. Otherwise, only extend visible
  ;; portion of tree. NODE is assumed to already have a node marker. Returns
  ;; non-nil if anything was actually extended.
  (let ((extended nil) parent)
    ;; don't bother extending if TOP specifies an already-drawn node
    (unless (and (dev-tree-node-p top) (dev-tree-node-marker top))
      (while node
	(setq parent (dev-tree-node-previous node))
	;; if we haven't reached root...
	(if parent
	    ;; ...and node is within range being drawn...
	    (if (or (eq top t)
		    (and (dev-tree-node-p top) (not (eq node top)))
		    (and (integerp top)
			 (< top (line-number-at-pos
				 (dev-tree-node-marker node))))
		    (and (null top)
			 ;; NOTE: we check point in case window-start is outdated
			 (< (min (line-number-at-pos (point))
				 (line-number-at-pos (window-start)))
			    (line-number-at-pos
			     (dev-tree-node-marker node)))))
		;; ...and it hasn't already been drawn
		(when (not (dev-tree-node-marker parent))
		  ;; link parent node to its representation in visualizer
		  (dev-tree-compute-widths parent)
		  (dev-tree-move-to-parent node)
		  (setf (dev-tree-node-marker parent) (make-marker))
		  (set-marker-insertion-type
		   (dev-tree-node-marker parent) nil)
		  (move-marker (dev-tree-node-marker parent) (point))
		  ;; draw subtree beneath parent
		  (setq dev-tree-visualizer-needs-extending-down
			(nconc (delq node (dev-tree-draw-subtree parent))
			       dev-tree-visualizer-needs-extending-down))
		  (setq extended t))
	      ;; ...otherwise, postpone drawing for later and exit
	      (setq dev-tree-visualizer-needs-extending-up (when parent node)
		    parent nil))

	  ;; if we've reached root, stop extending and add top margin
	  (setq dev-tree-visualizer-needs-extending-up nil)
	  (goto-char (dev-tree-node-marker node))
	  (dev-tree-move-up 1)  ; top margin
	  (delete-region (point-min) (line-beginning-position)))
	;; next iteration
	(setq node parent)))
    extended))


(defun dev-tree-expand-down (from &optional to)
  ;; Expand tree downwards. FROM is the node to start expanding from. Stop
  ;; expanding at TO if specified. Otherwise, just expand visible portion of
  ;; tree and highlight active branch from FROM.
  (when dev-tree-visualizer-needs-extending-down
    (let ((inhibit-read-only t)
	  node-list extended)
      ;; extend down as far as TO node
      (when to
	(setq extended (dev-tree-extend-down from to))
	(goto-char (dev-tree-node-marker to))
	(redisplay t))  ; force redisplay to scroll buffer if necessary
      ;; extend visible portion of tree downwards
      (setq node-list dev-tree-visualizer-needs-extending-down
	    dev-tree-visualizer-needs-extending-down nil)
      (when node-list
	(dolist (n node-list)
	  (when (dev-tree-extend-down n) (setq extended t)))
	;; highlight active branch in newly-extended-down portion, if any
	(when extended
	  (let ((dev-tree-insert-face
		 'dev-tree-visualizer-active-branch-face))
	    (dev-tree-highlight-active-branch from)))))))


(defun dev-tree-expand-up (from &optional to)
  ;; Expand tree upwards. FROM is the node to start expanding from, TO is the
  ;; node to stop expanding at. If TO node isn't specified, just expand visible
  ;; portion of tree and highlight active branch down to FROM.
  (when dev-tree-visualizer-needs-extending-up
    (let ((inhibit-read-only t)
	  extended node-list)
      ;; extend up as far as TO node
      (when to
	(setq extended (dev-tree-extend-up from to))
	(goto-char (dev-tree-node-marker to))
	;; simulate auto-scrolling if close to top of buffer
	(when (<= (line-number-at-pos (point)) scroll-margin)
	  (dev-tree-move-up (if (= scroll-conservatively 0)
				 (/ (window-height) 2) 3))
	  (when (dev-tree-extend-up to) (setq extended t))
	  (goto-char (dev-tree-node-marker to))
	  (unless (= scroll-conservatively 0) (recenter scroll-margin))))
      ;; extend visible portion of tree upwards
      (and dev-tree-visualizer-needs-extending-up
	   (dev-tree-extend-up dev-tree-visualizer-needs-extending-up)
	   (setq extended t))
      ;; extend visible portion of tree downwards
      (setq node-list dev-tree-visualizer-needs-extending-down
	    dev-tree-visualizer-needs-extending-down nil)
      (dolist (n node-list) (dev-tree-extend-down n))
      ;; highlight active branch in newly-extended-up portion, if any
      (when extended
	(let ((dev-tree-insert-face
	       'dev-tree-visualizer-active-branch-face))
	  (dev-tree-highlight-active-branch
	   (or dev-tree-visualizer-needs-extending-up
	       (dev-tree-root buffer-dev-tree))
	   from))))))



(defun dev-tree-highlight-active-branch (node &optional end)
  ;; Draw highlighted active branch below NODE in current buffer. Stop
  ;; highlighting at END node if specified.
  (let ((stack (list node)))
    ;; draw active branch
    (while stack
      (setq node (pop stack))
      (unless (or (eq node end)
		  (memq node dev-tree-visualizer-needs-extending-down))
	(goto-char (dev-tree-node-marker node))
	(setq node (dev-tree-draw-subtree node 'active)
	      stack (nconc stack node))))))


(defun dev-tree-draw-node (node &optional current)
  ;; Draw symbol representing NODE in visualizer. If CURRENT is non-nil, node
  ;; is current node.
  (goto-char (dev-tree-node-marker node))
  (when dev-tree-visualizer-timestamps
    (dev-tree-move-backward (/ dev-tree-visualizer-spacing 2)))

  (let* ((dev-tree-insert-face (and dev-tree-insert-face
				     (or (and (consp dev-tree-insert-face)
					      dev-tree-insert-face)
					 (list dev-tree-insert-face))))
	 (register (dev-tree-node-register node))
	 (unmodified (if dev-tree-visualizer-parent-mtime
			 (dev-tree-node-unmodified-p
			  node dev-tree-visualizer-parent-mtime)
		       (dev-tree-node-unmodified-p node)))
	node-string)
    ;; check node's register (if any) still stores appropriate dev-tree state
    (unless (and register
		 (dev-tree-register-data-p
		  (registerv-data (get-register register)))
		 (eq node (dev-tree-register-data-node
			   (registerv-data (get-register register)))))
      (setq register nil))
    ;; represent node by different symbols, depending on whether it's the
    ;; current node, is saved in a register, or corresponds to an unmodified
    ;; buffer
    (setq node-string
	    (cond
	     (dev-tree-visualizer-timestamps
	        (dev-tree-timestamp-to-string
	         (dev-tree-node-timestamp node)
		 dev-tree-visualizer-relative-timestamps
		 current register))
	     (register (char-to-string register))
	     (unmodified "s")
	     (current "x")
	     (t "o"))
	  dev-tree-insert-face
	    (nconc
	     (cond
	      (current    (list 'dev-tree-visualizer-current-face))
	      (unmodified (list 'dev-tree-visualizer-unmodified-face))
	      (register   (list 'dev-tree-visualizer-register-face)))
	     dev-tree-insert-face))
    ;; draw node and link it to its representation in visualizer
    (dev-tree-insert node-string)
    (dev-tree-move-backward (if dev-tree-visualizer-timestamps
				 (1+ (/ dev-tree-visualizer-spacing 2))
			       1))
    (move-marker (dev-tree-node-marker node) (point))
    (put-text-property (point) (1+ (point)) 'dev-tree-node node)))


(defun dev-tree-draw-subtree (node &optional active-branch)
  ;; Draw subtree rooted at NODE. The subtree will start from point.
  ;; If ACTIVE-BRANCH is non-nil, just draw active branch below NODE. Returns
  ;; list of nodes below NODE.
  (let ((num-children (length (dev-tree-node-next node)))
        node-list pos trunk-pos n)
    ;; draw node itself
    (dev-tree-draw-node node)

    (cond
     ;; if we're at a leaf node, we're done
     ((= num-children 0))

     ;; if node has only one child, draw it (not strictly necessary to deal
     ;; with this case separately, but as it's by far the most common case
     ;; this makes the code clearer and more efficient)
     ((= num-children 1)
      (dev-tree-move-down 1)
      (dev-tree-insert ?|)
      (dev-tree-move-backward 1)
      (dev-tree-move-down 1)
      (dev-tree-insert ?|)
      (dev-tree-move-backward 1)
      (dev-tree-move-down 1)
      (setq n (car (dev-tree-node-next node)))
      ;; link next node to its representation in visualizer
      (unless (markerp (dev-tree-node-marker n))
        (setf (dev-tree-node-marker n) (make-marker))
        (set-marker-insertion-type (dev-tree-node-marker n) nil))
      (move-marker (dev-tree-node-marker n) (point))
      ;; add next node to list of nodes to draw next
      (push n node-list))

     ;; if node has multiple children, draw branches
     (t
      (dev-tree-move-down 1)
      (dev-tree-insert ?|)
      (dev-tree-move-backward 1)
      (move-marker (setq trunk-pos (make-marker)) (point))
      ;; left subtrees
      (dev-tree-move-backward
       (- (dev-tree-node-char-lwidth node)
          (dev-tree-node-char-lwidth
           (car (dev-tree-node-next node)))))
      (move-marker (setq pos (make-marker)) (point))
      (setq n (cons nil (dev-tree-node-next node)))
      (dotimes (_ (/ num-children 2))
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (dev-tree-node-branch node)
                           (dev-tree-node-next node))))
          (dev-tree-move-forward 2)
          (dev-tree-insert ?_ (- trunk-pos pos 2))
          (goto-char pos)
          (dev-tree-move-forward 1)
          (dev-tree-move-down 1)
          (dev-tree-insert ?/)
          (dev-tree-move-backward 2)
          (dev-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (dev-tree-node-marker (car n)))
            (setf (dev-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (dev-tree-node-marker (car n)) nil))
          (move-marker (dev-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (goto-char pos)
        (dev-tree-move-forward
         (+ (dev-tree-node-char-rwidth (car n))
            (dev-tree-node-char-lwidth (cadr n))
            dev-tree-visualizer-spacing 1))
        (move-marker pos (point)))
      ;; middle subtree (only when number of children is odd)
      (when (= (mod num-children 2) 1)
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (dev-tree-node-branch node)
                           (dev-tree-node-next node))))
          (dev-tree-move-down 1)
          (dev-tree-insert ?|)
          (dev-tree-move-backward 1)
          (dev-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (dev-tree-node-marker (car n)))
            (setf (dev-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (dev-tree-node-marker (car n)) nil))
          (move-marker (dev-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (goto-char pos)
        (dev-tree-move-forward
         (+ (dev-tree-node-char-rwidth (car n))
            (if (cadr n) (dev-tree-node-char-lwidth (cadr n)) 0)
            dev-tree-visualizer-spacing 1))
        (move-marker pos (point)))
      ;; right subtrees
      (move-marker trunk-pos (1+ trunk-pos))
      (dotimes (_ (/ num-children 2))
        (setq n (cdr n))
        (when (or (null active-branch)
                  (eq (car n)
                      (nth (dev-tree-node-branch node)
                           (dev-tree-node-next node))))
          (goto-char trunk-pos)
          (dev-tree-insert ?_ (- pos trunk-pos 1))
          (goto-char pos)
          (dev-tree-move-backward 1)
          (dev-tree-move-down 1)
          (dev-tree-insert ?\\)
          (dev-tree-move-down 1)
          ;; link node to its representation in visualizer
          (unless (markerp (dev-tree-node-marker (car n)))
            (setf (dev-tree-node-marker (car n)) (make-marker))
            (set-marker-insertion-type (dev-tree-node-marker (car n)) nil))
          (move-marker (dev-tree-node-marker (car n)) (point))
          ;; add node to list of nodes to draw next
          (push (car n) node-list))
        (when (cdr n)
          (goto-char pos)
          (dev-tree-move-forward
           (+ (dev-tree-node-char-rwidth (car n))
              (if (cadr n) (dev-tree-node-char-lwidth (cadr n)) 0)
              dev-tree-visualizer-spacing 1))
          (move-marker pos (point))))
      ))
    ;; return list of nodes to draw next
    (nreverse node-list)))


(defun dev-tree-node-char-lwidth (node)
  ;; Return left-width of NODE measured in characters.
  (if (= (length (dev-tree-node-next node)) 0) 0
    (- (* (+ dev-tree-visualizer-spacing 1) (dev-tree-node-lwidth node))
       (if (= (dev-tree-node-cwidth node) 0)
           (1+ (/ dev-tree-visualizer-spacing 2)) 0))))


(defun dev-tree-node-char-rwidth (node)
  ;; Return right-width of NODE measured in characters.
  (if (= (length (dev-tree-node-next node)) 0) 0
    (- (* (+ dev-tree-visualizer-spacing 1) (dev-tree-node-rwidth node))
       (if (= (dev-tree-node-cwidth node) 0)
           (1+ (/ dev-tree-visualizer-spacing 2)) 0))))


(defun dev-tree-insert (str &optional arg)
  ;; Insert character or string STR ARG times, overwriting, and using
  ;; `dev-tree-insert-face'.
  (unless arg (setq arg 1))
  (when (characterp str)
    (setq str (make-string arg str))
    (setq arg 1))
  (dotimes (_ arg) (insert str))
  (setq arg (* arg (length str)))
  (dev-tree-move-forward arg)
  ;; make sure mark isn't active, otherwise `backward-delete-char' might
  ;; delete region instead of single char if transient-mark-mode is enabled
  (setq mark-active nil)
  (backward-delete-char arg)
  (when dev-tree-insert-face
    (put-text-property (- (point) arg) (point) 'face dev-tree-insert-face)))


(defun dev-tree-move-down (&optional arg)
  ;; Move down, extending buffer if necessary.
  (let ((row (line-number-at-pos))
        (col (current-column))
        line)
    (unless arg (setq arg 1))
    (forward-line arg)
    (setq line (line-number-at-pos))
    ;; if buffer doesn't have enough lines, add some
    (when (/= line (+ row arg))
      (cond
       ((< arg 0)
	(insert (make-string (- line row arg) ?\n))
	(forward-line (+ arg (- row line))))
       (t (insert (make-string (- arg (- line row)) ?\n)))))
    (dev-tree-move-forward col)))


(defun dev-tree-move-up (&optional arg)
  ;; Move up, extending buffer if necessary.
  (unless arg (setq arg 1))
  (dev-tree-move-down (- arg)))


(defun dev-tree-move-forward (&optional arg)
  ;; Move forward, extending buffer if necessary.
  (unless arg (setq arg 1))
  (let (n)
    (cond
     ((>= arg 0)
      (setq n (- (line-end-position) (point)))
      (if (> n arg)
	  (forward-char arg)
	(end-of-line)
	(insert (make-string (- arg n) ? ))))
     ((< arg 0)
      (setq arg (- arg))
      (setq n (- (point) (line-beginning-position)))
      (when (< (- n 2) arg)  ; -2 to create left-margin
	;; no space left - shift entire buffer contents right!
	(let ((pos (move-marker (make-marker) (point))))
	  (set-marker-insertion-type pos t)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (insert-before-markers (make-string (- arg -2 n) ? ))
	    (forward-line 1))
	  (goto-char pos)))
      (backward-char arg)))))


(defun dev-tree-move-backward (&optional arg)
  ;; Move backward, extending buffer if necessary.
  (unless arg (setq arg 1))
  (dev-tree-move-forward (- arg)))


(defun dev-tree-move-to-parent (node)
  ;; Move to position of parent of NODE, extending buffer if necessary.
  (let* ((parent (dev-tree-node-previous node))
	 (n (dev-tree-node-next parent))
	 (l (length n)) p)
    (goto-char (dev-tree-node-marker node))
    (unless (= l 1)
      ;; move horizontally
      (setq p (dev-tree-position node n))
      (cond
       ;; node in centre subtree: no horizontal movement
       ((and (= (mod l 2) 1) (= p (/ l 2))))
       ;; node in left subtree: move right
       ((< p (/ l 2))
	(setq n (nthcdr p n))
	(dev-tree-move-forward
	 (+ (dev-tree-node-char-rwidth (car n))
	    (/ dev-tree-visualizer-spacing 2) 1))
	(dotimes (_ (- (/ l 2) p 1))
	  (setq n (cdr n))
	  (dev-tree-move-forward
	   (+ (dev-tree-node-char-lwidth (car n))
	      (dev-tree-node-char-rwidth (car n))
	      dev-tree-visualizer-spacing 1)))
	(when (= (mod l 2) 1)
	  (setq n (cdr n))
	  (dev-tree-move-forward
	   (+ (dev-tree-node-char-lwidth (car n))
	      (/ dev-tree-visualizer-spacing 2) 1))))
       (t ;; node in right subtree: move left
	(setq n (nthcdr (/ l 2) n))
	(when (= (mod l 2) 1)
	  (dev-tree-move-backward
	   (+ (dev-tree-node-char-rwidth (car n))
	      (/ dev-tree-visualizer-spacing 2) 1))
	  (setq n (cdr n)))
	(dotimes (_ (- p (/ l 2) (mod l 2)))
	  (dev-tree-move-backward
	   (+ (dev-tree-node-char-lwidth (car n))
	      (dev-tree-node-char-rwidth (car n))
	      dev-tree-visualizer-spacing 1))
	  (setq n (cdr n)))
	(dev-tree-move-backward
	 (+ (dev-tree-node-char-lwidth (car n))
	    (/ dev-tree-visualizer-spacing 2) 1)))))
    ;; move vertically
    (dev-tree-move-up 3)))


(defun dev-tree-timestamp-to-string
  (timestamp &optional relative current register)
  ;; Convert TIMESTAMP to string (either absolute or RELATVE time), indicating
  ;; if it's the CURRENT node and/or has an associated REGISTER.
  (if relative
      ;; relative time
      (let ((time (floor (float-time
			  (time-subtract (current-time) timestamp))))
	    n)
	(setq time
	      ;; years
	      (if (> (setq n (/ time 315360000)) 0)
		  (if (> n 999) "-ages" (format "-%dy" n))
		(setq time (% time 315360000))
		;; days
		(if (> (setq n (/ time 86400)) 0)
		    (format "-%dd" n)
		  (setq time (% time 86400))
		  ;; hours
		  (if (> (setq n (/ time 3600)) 0)
		      (format "-%dh" n)
		    (setq time (% time 3600))
		    ;; mins
		    (if (> (setq n (/ time 60)) 0)
			(format "-%dm" n)
		      ;; secs
		      (format "-%ds" (% time 60)))))))
	(setq time (concat
		    (if current "*" " ")
		    time
		    (if register (concat "[" (char-to-string register) "]")
		      "   ")))
	(setq n (length time))
	(if (< n 9)
	    (concat (make-string (- 9 n) ? ) time)
	  time))
    ;; absolute time
    (concat (if current " *" "  ")
	    (format-time-string "%H:%M:%S" timestamp)
	    (if register
		(concat "[" (char-to-string register) "]")
	      "   "))))



;;; =====================================================================
;;;                        Visualizer modes


(define-derived-mode
  dev-tree-visualizer-mode special-mode "dev-tree-visualizer"
  "Major mode used in dev-tree visualizer.

The dev-tree visualizer can only be invoked from a buffer in
which `dev-tree-mode' is enabled. The visualizer displays the
undo history tree graphically, and allows you to browse around
the undo history, undoing or redoing the corresponding changes in
the parent buffer.

Within the dev-tree visualizer, the following keys are available:

  \\{dev-tree-visualizer-mode-map}"
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t)
  (setq cursor-type nil)
  (setq dev-tree-visualizer-selected-node nil)
  (make-local-variable 'dev-tree-visualizer-timestamps)
  (make-local-variable 'dev-tree-visualizer-diff))




;;; =====================================================================
;;;                        Visualizer commands


(defun dev-tree-visualize-undo (&optional arg)
  "Undo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (unless (eq major-mode 'dev-tree-visualizer-mode)
    (user-error "dev-tree mode not enabled in buffer"))
  (let ((old (dev-tree-current buffer-dev-tree))
	current)
    ;; undo in parent buffer
    (switch-to-buffer-other-window dev-tree-visualizer-parent-buffer)
    (deactivate-mark)
    (unwind-protect
	(let ((dev-tree-inhibit-kill-visualizer t)) (dev-tree-undo-1 arg))
      (setq current (dev-tree-current buffer-dev-tree))
      (switch-to-buffer-other-window dev-tree-visualizer-buffer-name)
      ;; unhighlight old current node
      (let ((dev-tree-insert-face 'dev-tree-visualizer-active-branch-face)
	    (inhibit-read-only t))
	(dev-tree-draw-node old))
      ;; when using lazy drawing, extend tree upwards as required
      (when dev-tree-visualizer-lazy-drawing
	(dev-tree-expand-up old current))
      ;; highlight new current node
      (let ((inhibit-read-only t)) (dev-tree-draw-node current 'current))
      ;; update diff display, if any
      (when dev-tree-visualizer-diff (dev-tree-visualizer-update-diff)))))


(defun dev-tree-visualize-redo (&optional arg)
  "Redo changes. A numeric ARG serves as a repeat count."
  (interactive "p")
  (unless (eq major-mode 'dev-tree-visualizer-mode)
    (user-error "dev-tree mode not enabled in buffer"))
  (let ((old (dev-tree-current buffer-dev-tree))
	current)
    ;; redo in parent buffer
    (switch-to-buffer-other-window dev-tree-visualizer-parent-buffer)
    (deactivate-mark)
    (unwind-protect
	(let ((dev-tree-inhibit-kill-visualizer t)) (dev-tree-redo-1 arg))
      (setq current (dev-tree-current buffer-dev-tree))
      (switch-to-buffer-other-window dev-tree-visualizer-buffer-name)
      ;; unhighlight old current node
      (let ((dev-tree-insert-face 'dev-tree-visualizer-active-branch-face)
	    (inhibit-read-only t))
	(dev-tree-draw-node old))
      ;; when using lazy drawing, extend tree downwards as required
      (when dev-tree-visualizer-lazy-drawing
	(dev-tree-expand-down old current))
      ;; highlight new current node
      (let ((inhibit-read-only t)) (dev-tree-draw-node current 'current))
      ;; update diff display, if any
      (when dev-tree-visualizer-diff (dev-tree-visualizer-update-diff)))))


(defun dev-tree-visualize-switch-branch-right (arg)
  "Switch to next branch of the undo tree.
This will affect which branch to descend when *redoing* changes
using `dev-tree-redo' or `dev-tree-visualizer-redo'."
  (interactive "p")
  (unless (eq major-mode 'dev-tree-visualizer-mode)
    (user-error "dev-tree mode not enabled in buffer"))
  ;; un-highlight old active branch below current node
  (goto-char (dev-tree-node-marker (dev-tree-current buffer-dev-tree)))
  (let ((dev-tree-insert-face 'dev-tree-visualizer-default-face)
	(inhibit-read-only t))
    (dev-tree-highlight-active-branch (dev-tree-current buffer-dev-tree)))
  ;; increment branch
  (let ((branch (dev-tree-node-branch (dev-tree-current buffer-dev-tree))))
    (setf (dev-tree-node-branch (dev-tree-current buffer-dev-tree))
	  (cond
	   ((>= (+ branch arg) (dev-tree-num-branches))
	    (1- (dev-tree-num-branches)))
	   ((<= (+ branch arg) 0) 0)
	   (t (+ branch arg))))
    (let ((inhibit-read-only t))
      ;; highlight new active branch below current node
      (goto-char (dev-tree-node-marker (dev-tree-current buffer-dev-tree)))
      (let ((dev-tree-insert-face 'dev-tree-visualizer-active-branch-face))
	(dev-tree-highlight-active-branch (dev-tree-current buffer-dev-tree)))
      ;; re-highlight current node
      (dev-tree-draw-node (dev-tree-current buffer-dev-tree) 'current))))


(defun dev-tree-visualize-switch-branch-left (arg)
  "Switch to previous branch of the undo tree.
This will affect which branch to descend when *redoing* changes
using `dev-tree-redo' or `dev-tree-visualizer-redo'."
  (interactive "p")
  (dev-tree-visualize-switch-branch-right (- arg)))


(defun dev-tree-visualizer-quit ()
  "Quit the dev-tree visualizer."
  (interactive)
  (unless (eq major-mode 'dev-tree-visualizer-mode)
    (user-error "dev-tree mode not enabled in buffer"))
  (dev-tree-clear-visualizer-data buffer-dev-tree)
  ;; remove kill visualizer hook from parent buffer
  (unwind-protect
      (with-current-buffer dev-tree-visualizer-parent-buffer
	(remove-hook 'before-change-functions 'dev-tree-kill-visualizer t))
    ;; kill diff buffer, if any
    (when dev-tree-visualizer-diff (dev-tree-visualizer-hide-diff))
    (let ((parent dev-tree-visualizer-parent-buffer)
	  window)
      ;; kill visualizer buffer
      (kill-buffer nil)
      ;; switch back to parent buffer
      (unwind-protect
	  (if (setq window (get-buffer-window parent))
	      (select-window window)
	    (switch-to-buffer parent))))))


(defun dev-tree-visualizer-abort ()
  "Quit the dev-tree visualizer and return buffer to original state."
  (interactive)
  (unless (eq major-mode 'dev-tree-visualizer-mode)
    (user-error "dev-tree mode not enabled in buffer"))
  (let ((node dev-tree-visualizer-initial-node))
    (dev-tree-visualizer-quit)
    (dev-tree-set node)))


(defun dev-tree-visualizer-set (&optional pos)
  "Set buffer to state corresponding to undo tree node
at POS, or point if POS is nil."
  (interactive)
  (unless (eq major-mode 'dev-tree-visualizer-mode)
    (user-error "dev-tree mode not enabled in buffer"))
  (unless pos (setq pos (point)))
  (let ((node (get-text-property pos 'dev-tree-node)))
    (when node
      ;; set parent buffer to state corresponding to node at POS
      (switch-to-buffer-other-window dev-tree-visualizer-parent-buffer)
      (let ((dev-tree-inhibit-kill-visualizer t)) (dev-tree-set node))
      (switch-to-buffer-other-window dev-tree-visualizer-buffer-name)
      ;; re-draw undo tree
      (let ((inhibit-read-only t)) (dev-tree-draw-tree buffer-dev-tree))
      (when dev-tree-visualizer-diff (dev-tree-visualizer-update-diff)))))


(defun dev-tree-visualizer-mouse-set (pos)
  "Set buffer to state corresponding to undo tree node
at mouse event POS."
  (interactive "@e")
  (unless (eq major-mode 'dev-tree-visualizer-mode)
    (user-error "dev-tree mode not enabled in buffer"))
  (dev-tree-visualizer-set (event-start (nth 1 pos))))


(defun dev-tree-visualize-undo-to-x (&optional x)
  "Undo to last branch point, register, or saved state.
If X is the symbol `branch', undo to last branch point. If X is
the symbol `register', undo to last register. If X is the symbol
`saved', undo to last saved state. If X is null, undo to first of
these that's encountered.

Interactively, a single \\[universal-argument] specifies
`branch', a double \\[universal-argument] \\[universal-argument]
specifies `saved', and a negative prefix argument specifies
`register'."
  (interactive "P")
  (unless (eq major-mode 'dev-tree-visualizer-mode)
    (user-error "dev-tree mode not enabled in buffer"))
  (when (and (called-interactively-p 'any) x)
    (setq x (prefix-numeric-value x)
	  x (cond
	     ((< x 0)  'register)
	     ((<= x 4) 'branch)
	     (t        'saved))))
  (let ((current (if dev-tree-visualizer-selection-mode
		     dev-tree-visualizer-selected-node
		   (dev-tree-current buffer-dev-tree)))
	(diff dev-tree-visualizer-diff)
	r)
    (dev-tree-visualizer-hide-diff)
    (while (and (dev-tree-node-previous current)
		(or (if dev-tree-visualizer-selection-mode
			(progn
			  (dev-tree-visualizer-select-previous)
			  (setq current dev-tree-visualizer-selected-node))
		      (dev-tree-visualize-undo)
		      (setq current (dev-tree-current buffer-dev-tree)))
		    t)
		         ;; branch point
		(not (or (and (or (null x) (eq x 'branch))
			      (> (dev-tree-num-branches) 1))
			 ;; register
			 (and (or (null x) (eq x 'register))
			      (setq r (dev-tree-node-register current))
			      (dev-tree-register-data-p
			       (setq r (registerv-data (get-register r))))
			      (eq current (dev-tree-register-data-node r)))
			 ;; saved state
			 (and (or (null x) (eq x 'saved))
			      (dev-tree-node-unmodified-p current))
			 ))))
    ;; update diff display, if any
    (when diff
      (dev-tree-visualizer-show-diff
       (when dev-tree-visualizer-selection-mode
	 dev-tree-visualizer-selected-node)))))


(defun dev-tree-visualize-redo-to-x (&optional x)
  "Redo to last branch point, register, or saved state.
If X is the symbol `branch', redo to last branch point. If X is
the symbol `register', redo to last register. If X is the sumbol
`saved', redo to last saved state. If X is null, redo to first of
these that's encountered.

Interactively, a single \\[universal-argument] specifies
`branch', a double \\[universal-argument] \\[universal-argument]
specifies `saved', and a negative prefix argument specifies
`register'."
  (interactive "P")
  (unless (eq major-mode 'dev-tree-visualizer-mode)
    (user-error "dev-tree mode not enabled in buffer"))
  (when (and (called-interactively-p 'any) x)
    (setq x (prefix-numeric-value x)
	  x (cond
	     ((< x 0)  'register)
	     ((<= x 4) 'branch)
	     (t        'saved))))
  (let ((current (if dev-tree-visualizer-selection-mode
		     dev-tree-visualizer-selected-node
		   (dev-tree-current buffer-dev-tree)))
	(diff dev-tree-visualizer-diff)
	r)
    (dev-tree-visualizer-hide-diff)
    (while (and (dev-tree-node-next current)
		(or (if dev-tree-visualizer-selection-mode
			(progn
			  (dev-tree-visualizer-select-next)
			  (setq current dev-tree-visualizer-selected-node))
		      (dev-tree-visualize-redo)
		      (setq current (dev-tree-current buffer-dev-tree)))
		    t)
		         ;; branch point
		(not (or (and (or (null x) (eq x 'branch))
			      (> (dev-tree-num-branches) 1))
			 ;; register
			 (and (or (null x) (eq x 'register))
			      (setq r (dev-tree-node-register current))
			      (dev-tree-register-data-p
			       (setq r (registerv-data (get-register r))))
			      (eq current (dev-tree-register-data-node r)))
			 ;; saved state
			 (and (or (null x) (eq x 'saved))
			      (dev-tree-node-unmodified-p current))
			 ))))
    ;; update diff display, if any
    (when diff
      (dev-tree-visualizer-show-diff
       (when dev-tree-visualizer-selection-mode
	 dev-tree-visualizer-selected-node)))))


(defun dev-tree-visualizer-toggle-timestamps ()
  "Toggle display of time-stamps."
  (interactive)
  (unless (eq major-mode 'dev-tree-visualizer-mode)
    (user-error "dev-tree mode not enabled in buffer"))
  (setq dev-tree-visualizer-timestamps (not dev-tree-visualizer-timestamps))
  (setq dev-tree-visualizer-spacing (dev-tree-visualizer-calculate-spacing))
  ;; redraw tree
  (let ((inhibit-read-only t)) (dev-tree-draw-tree buffer-dev-tree)))


(defun dev-tree-visualizer-scroll-left (&optional arg)
  (interactive "p")
  (unless (eq major-mode 'dev-tree-visualizer-mode)
    (user-error "dev-tree mode not enabled in buffer"))
  (scroll-left (or arg 1) t))


(defun dev-tree-visualizer-scroll-right (&optional arg)
  (interactive "p")
  (unless (eq major-mode 'dev-tree-visualizer-mode)
    (user-error "dev-tree mode not enabled in buffer"))
  (scroll-right (or arg 1) t))


(defun dev-tree-visualizer-scroll-up (&optional arg)
  (interactive "P")
  (unless (eq major-mode 'dev-tree-visualizer-mode)
    (user-error "dev-tree mode not enabled in buffer"))
  (if (or (and (numberp arg) (< arg 0)) (eq arg '-))
      (dev-tree-visualizer-scroll-down arg)
    ;; scroll up and expand newly-visible portion of tree
    (unwind-protect
	(scroll-up-command arg)
      (dev-tree-expand-down
       (nth (dev-tree-node-branch (dev-tree-current buffer-dev-tree))
	    (dev-tree-node-next (dev-tree-current buffer-dev-tree)))))
    ;; signal error if at eob
    (when (and (not dev-tree-visualizer-needs-extending-down) (eobp))
      (scroll-up))))


(defun dev-tree-visualizer-scroll-down (&optional arg)
  (interactive "P")
  (unless (eq major-mode 'dev-tree-visualizer-mode)
    (user-error "dev-tree mode not enabled in buffer"))
  (if (or (and (numberp arg) (< arg 0)) (eq arg '-))
      (dev-tree-visualizer-scroll-up arg)
    ;; ensure there's enough room at top of buffer to scroll
    (let ((scroll-lines
	   (or arg (- (window-height) next-screen-context-lines)))
	  (window-line (1- (line-number-at-pos (window-start)))))
      (when (and dev-tree-visualizer-needs-extending-up
		 (< window-line scroll-lines))
	(let ((inhibit-read-only t))
	  (goto-char (point-min))
	  (dev-tree-move-up (- scroll-lines window-line)))))
    ;; scroll down and expand newly-visible portion of tree
    (unwind-protect
	(scroll-down-command arg)
      (dev-tree-expand-up
       (dev-tree-node-previous (dev-tree-current buffer-dev-tree))))
    ;; signal error if at bob
    (when (and (not dev-tree-visualizer-needs-extending-down) (bobp))
      (scroll-down))))


;;; =========================================================================

(provide 'dev-tree)

;;; dev-tree.el ends here
