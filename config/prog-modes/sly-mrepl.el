;; (with-eval-after-load 'sly
;;  (orpl-lisp-editing-keybinds sly-mrepl-mode-map))

(defun enable-ansi-colors-in-sly-mrepl ()
  (add-to-list 'sly-mrepl-output-filter-functions 'ansi-color-apply))

(add-to-hook 'sly-mrepl-mode-hook
	     '(shared-lisp-pretty-symbols
	       enable-ansi-colors-in-sly-mrepl))
