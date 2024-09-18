(defun add-to-hook (hook procedures)
  "Adds each procedure in 'procedures' to 'hook'."
  (dolist (procedure procedures)
    (add-hook hook (symbol-function procedure))))

(defun add-to-hooks (hooks procedures)
  "Adds each procedure in 'procedures' to each hook in 'hooks'."
  (dolist (hook hooks)
    (dolist (procedure procedures)
      (add-hook hook (symbol-function procedure)))))
