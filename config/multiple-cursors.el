(use-package multiple-cursors
  :ensure t
  :demand t
  :config (progn
	    (keymap-unset mc/keymap "C-v")
	    (keymap-unset mc/keymap "<return>")
	    (global-set-key (kbd "C-M-<up>") #'mc/mark-previous-like-this)
	    (global-set-key (kbd "C-M-<down>") #'mc/mark-next-like-this)
	    (global-set-key (kbd "C-M-i") #'mc/mark-previous-like-this)
	    (global-set-key (kbd "C-M-k") #'mc/mark-next-like-this)))


