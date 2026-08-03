(defun modes-in-buffer (mode)
  (interactive (list major-mode))
  (defun iter (mode)
    (and mode (cons mode (iter (get mode 'derived-mode-parent)))))
  (message "%s" (iter mode)))

(defun add-to-hook (hook procedures)
  "Adds each procedure in 'procedures' to 'hook'."
  (dolist (procedure procedures)
    (add-hook hook (symbol-function procedure))))

(defun add-to-hooks (hooks procedures)
  "Adds each procedure in 'procedures' to each hook in 'hooks'."
  (dolist (hook hooks)
    (dolist (procedure procedures)
      (add-hook hook (symbol-function procedure)))))

(defun lerp (start end amount)
  (cond ((<= amount 0) start)
     ((>= amount 1) end)
     (t (+ (* (- 1 amount) start)
	   (* amount end)))))

(defun color-lerp (start end percent)
  (let* ((start-vals (color-name-to-rgb start))
	 (end-vals (color-name-to-rgb end))
	 (vals (cl-mapcar (lambda (s e) (lerp s e (/ percent 100.0))) start-vals end-vals)))
    (apply 'color-rgb-to-hex vals)))

(defun sly-eval-last-sexp-in-popup-buffer ()
  (interactive)
  (let ((string (sly-last-expression)))
    (if string
        ;; code adapted from `sly-eval-print'
        (sly-eval-async `(slynk:eval-and-grab-output ,string)
          (lambda (result)
            (cl-destructuring-bind (output values) result
              (sly-with-popup-buffer ("*sly-evaluation-result*"
                                      :connection t
                                      :mode 'lisp-mode
                                      :select t)
		(sly-popup-buffer-mode)
		(read-only-mode -1)
		(unless (string-empty-p output)
		  (insert (string-remove-prefix "\n" output) "\n\n"))
		(if (cl-find ?\n values)
		    (insert "=> \n" values)
		  (insert "=> " values))
		(read-only-mode 1)))))
      (user-error "No sexp at point!"))))
