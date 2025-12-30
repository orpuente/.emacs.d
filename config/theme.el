;; Documentation can be found here: `https://protesilaos.com/emacs/ef-themes'
(use-package ef-themes
  :ensure t
  :demand t
  :config (progn (setq ef-themes-disable-other-themes t)
		 (load-theme 'ef-bio :no-confirm)
		 ;; (load-theme 'ef-autumn :no-confirm)
		 ;; (load-theme 'ef-elea-light :heno-confirm)
		 ;; Toggle between themes
		 (setq ef-themes-to-toggle (list 'ef-bio 'ef-elea-light))))

;; Sebas' red theme.
; (set-face-attribute 'default nil :foreground "#ff6666" :background "#000")

;; doom-emacs
(use-package doom-themes
  :defer 5
  :config (progn (doom-themes-neotree-config)
		 ;; (load-theme 'doom-one t)
		 (global-set-key (kbd "C-b") #'neotree-toggle)))
