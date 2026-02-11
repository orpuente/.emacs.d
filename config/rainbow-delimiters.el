(defun lerp (start end amount)
  (cond ((<= amount 0) start)
     ((>= amount 1) end)
     (t (+ (* (- 1 amount) start)
	   (* amount end)))))

(defun color-lerp (start end percent)
  (let* ((start-vals (color-name-to-rgb start))
	 (end-vals (color-name-to-rgb end))
	 (vals (cl-mapcar (lambda (s e) (lerp s e (/ percent 100.0))) start-vals end-vals)))
    (apply 'color-rgb-to-hex vals)))

(defun rainbow-delimiters-restart-mode ()
  (if rainbow-delimiters-mode
      (progn
	(rainbow-delimiters-mode 0)
	(rainbow-delimiters-mode 1))))

;; Main color scheme.
(defun rainbow-delimiters-dark-theme ()
  (interactive)
  (setq rainbow-delimiters-max-face-count 7)
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "dark orange")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "deep pink")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "chartreuse")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "deep sky blue")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "yellow")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "orchid")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "spring green")
  (rainbow-delimiters-restart-mode))

(defun rainbow-delimiters-dark-theme-dimmed (percent)
  (interactive (list (read-number "Dim Percent: " 75)))
  (setq rainbow-delimiters-max-face-count 6)
  (let ((percent (or percent 75))
	(bc (background-color-at-point)))
    (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground (color-lerp "dark orange" (background-color-at-point) percent))
    (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground (color-lerp "deep pink" (background-color-at-point) percent))
    (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground (color-lerp "chartreuse" (background-color-at-point) percent))
    (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground (color-lerp "deep sky blue" (background-color-at-point) percent))
    (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground (color-lerp "yellow" (background-color-at-point) percent))
    (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground (color-lerp "orchid" (background-color-at-point) percent))
    (rainbow-delimiters-mode 0))
  (rainbow-delimiters-mode 1))

;; Alternative color scheme.
(defun rainbow-delimiters-light-theme ()
  (interactive)
  (setq rainbow-delimiters-max-face-count 8)
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "dark orange")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#d8007e")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#00d812")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#007ed8")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#d81200")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#a200d8")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#00d8a2")
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "#1200d8")
  (rainbow-delimiters-restart-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode sly-mrepl-mode)
  :config (rainbow-delimiters-dark-theme))
