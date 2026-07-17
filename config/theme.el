(defvar *theme* 'light)

(defun update-rainbow-delimiters-theme ()
  (if (eq *theme* 'light)
	  (rainbow-delimiters-light-theme)
	(rainbow-delimiters-dark-theme)))

(defun themes-toggle ()
  (interactive)
  (setq *theme* (if (eq *theme* 'light) 'dark 'light))
  (update-rainbow-delimiters-theme)
  (ef-themes-toggle))

;; Documentation can be found here: `https://protesilaos.com/emacs/ef-themes'
(use-package ef-themes
  :ensure t
  :demand t
  :config (progn (setq ef-themes-disable-other-themes t)
				 (load-theme 'ef-orange :no-confirm)
				 ;; (load-theme 'ef-autumn :no-confirm)
				 ;; (load-theme 'ef-elea-light :no-confirm)
				 ;; Toggle between themes
				 (setq ef-themes-to-toggle (list 'ef-orange 'ef-bio))
				 (global-set-key (kbd "C-n") #'themes-toggle)
				 (global-set-key (kbd "C-S-n") #'ef-themes-rotate)))

;; doom-emacs
(use-package doom-themes
  :disabled t
  :defer t
  :config (progn (doom-themes-neotree-config)
				 ;; (load-theme 'doom-one t)
				 (global-set-key (kbd "C-b") #'neotree-toggle)))
