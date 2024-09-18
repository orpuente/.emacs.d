(define-key mc/keymap (kbd "<return>") nil)

(global-set-key (kbd "C-M-<up>") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-<down>") #'mc/mark-next-like-this)

(global-set-key (kbd "C-M-i") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-k") #'mc/mark-next-like-this)
