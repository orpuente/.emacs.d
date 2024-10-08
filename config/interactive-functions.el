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
