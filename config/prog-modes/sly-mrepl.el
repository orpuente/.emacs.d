(with-eval-after-load 'sly
  (orpl-lisp-editing-keybinds sly-mrepl-mode-map))

(add-to-hook 'sly-mrepl-mode-hook
	     '(shared-lisp-pretty-symbols))
