(require 'ef-themes)

;; Sebas' red theme.
; (set-face-attribute 'default nil :foreground "#ff6666" :background "#000")

;; Documentation can be found here: `https://protesilaos.com/emacs/ef-themes'
(setq ef-themes-disable-other-themes t)
;;(load-theme 'ef-autumn :no-confirm)
;;(load-theme 'ef-elea-light :no-confirm)
(load-theme 'ef-bio :no-confirm)

;; Toggle between themes
(setq ef-themes-to-toggle (list 'ef-bio 'ef-elea-light))

;; doom-emacs
;; (load-theme 'doom-one t)
(doom-themes-neotree-config)
(global-set-key (kbd "C-b") #'neotree-toggle)
