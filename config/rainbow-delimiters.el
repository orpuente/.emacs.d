(defun rainbow-delimiters-restart-mode ()
  (if rainbow-delimiters-mode
      (progn
	(rainbow-delimiters-mode 0)
	(rainbow-delimiters-mode 1))))

;; Main color scheme.
(defun rainbow-delimiters-main-colors ()
  (interactive)
  (setq rainbow-delimiters-max-face-count 8)
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "dark orange")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "deep pink")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "chartreuse")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "deep sky blue")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "yellow")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "orchid")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "spring green")
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "sienna1")
  (rainbow-delimiters-restart-mode))

;; Alternative color scheme.
(defun rainbow-delimiters-alternative-colors ()
  (interactive)
  (setq rainbow-delimiters-max-face-count 6)
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "royalblue3")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "gray50")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "springgreen3")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "mediumpurple4")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "darkslategray")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "deepskyblue4")
  (rainbow-delimiters-restart-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook prog-mode
  :config (rainbow-delimiters-main-colors))
