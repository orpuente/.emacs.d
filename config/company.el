(define-key company-mode-map   (kbd "C-SPC") #'company-complete)
(define-key company-active-map (kbd "M-i") #'company-select-previous)
(define-key company-active-map (kbd "s-i") #'company-select-previous)
(define-key company-active-map (kbd "C-b") #'company-select-previous)
(define-key company-active-map (kbd "M-k") #'company-select-next)
(define-key company-active-map (kbd "s-k") #'company-select-next)
(define-key company-active-map (kbd "C-f") #'company-select-next)
(define-key company-active-map (kbd "<tab>") #'company-complete-selection)

;; (keymap-unset company-active-map "<enter>" t)
;; (keymap-unset company-active-map "RET" t)
;; (keymap-unset company-active-map "C-h" t)

(define-key company-active-map (kbd "<enter>") nil)
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map (kbd "C-h") nil)
