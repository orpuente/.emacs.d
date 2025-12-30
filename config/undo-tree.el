(use-package undo-tree
  :ensure t
  :defer 5
  :config (progn 
	    (global-undo-tree-mode)
	    (define-key undo-tree-map (kbd "C-/") nil)
	    (define-key undo-tree-map (kbd "C-_") nil)
	    (keymap-unset undo-tree-map "C-/")
	    (global-set-key (kbd "C-z") #'undo-tree-undo)
	    (global-set-key (kbd "C-y") #'undo-tree-redo)))
