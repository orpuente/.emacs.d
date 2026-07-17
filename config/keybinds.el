(defun my/translate-keys (&optional frame) ; Reason:
  (keyboard-translate ?\C-i ?\s-i)       ; C-i = TAB
  (keyboard-translate ?\C-\[ ?\s-\[))    ; C-[ = M-w 
(add-hook 'after-make-frame-functions #'my/translate-keys)
(my/translate-keys)

;; Unbinds
(keymap-unset lisp-interaction-mode-map "C-M-i")
(keymap-unset emacs-lisp-mode-map "C-M-i")
(dolist (key '("C-a" "C-n" "C-o" "C-p"
			   "C-t" "C-]" "H-[" "M-e"
			   "C-k" "C-l" "H-i" "C-j"
			   "s-j" "s-l" "s-i" "s-k" "s-["))
  (keymap-global-unset key t)
  (keymap-unset lisp-interaction-mode-map key t))

;; window movement
(global-set-key (kbd "C-0") #'other-window)
(global-set-key (kbd "C-<tab>") #'other-window)

;; basic text manipulation
(global-set-key (kbd "C-v") #'yank)
(global-set-key (kbd "C-a") #'mark-whole-buffer)

(defun my/backward-transpose-sexp ()
  (interactive)
  (sp-transpose-sexp -1))

(defun my/kill-whitespace ()
  (interactive)
  (let ((start (point)))
	(skip-chars-forward " \t\n\r\f")
	(when (> (point) start)
	  (delete-region start (point))
	  t)))

(defun my/kill-whitespace-backward ()
  (interactive)
  (let ((start (point)))
	(skip-chars-backward " \t\n\r\f")
	(when (< (point) start)
	  (delete-region (point) start)
	  t)))

(defun my/kill-whitespace-or-sexp ()
  (interactive)
  (unless (my/kill-whitespace)
	(sp-kill-sexp)))

(defun my/kill-whitespace-or-sexp-backward ()
  (interactive)
  (unless (my/kill-whitespace-backward)
	(sp-backward-kill-sexp)))

(defun my/lisp-editing-keybinds (keymap)
  ;; C-M-[key] (Ctrl + Alt + [key])
  (define-key keymap (kbd "M-<left>") #'my/backward-transpose-sexp)
  (define-key keymap (kbd "M-<right>") #'sp-transpose-sexp)
  (define-key keymap (kbd "M-<up>") #'my/backward-transpose-sexp)
  (define-key keymap (kbd "M-<down>") #'sp-transpose-sexp)

  ;; C-[Arrows]
  (define-key keymap (kbd "C-<left>")  #'backward-sexp)
  (define-key keymap (kbd "C-<right>") #'forward-sexp)
  (define-key keymap (kbd "C-<up>")    #'backward-sexp)
  (define-key keymap (kbd "C-<down>")  #'forward-sexp)
  
  ;;; ast-actions
  (define-key keymap (kbd "H-[") #'sp-backward-slurp-sexp)
  (define-key keymap (kbd "C-]") #'sp-backward-barf-sexp)
  (define-key keymap (kbd "M-[") #'sp-forward-barf-sexp)
  (define-key keymap (kbd "M-]") #'sp-forward-slurp-sexp)
  (define-key keymap (kbd "C-r") #'raise-sexp)
  (define-key keymap (kbd "C-(") #'sp-backward-unwrap-sexp)
  (define-key keymap (kbd "C-)") #'sp-unwrap-sexp)
  (define-key keymap (kbd "C-<delete>") #'my/kill-whitespace-or-sexp)
  (define-key keymap (kbd "C-<backspace>") #'my/kill-whitespace-or-sexp-backward))


;; Quick Help
(global-set-key (kbd "M-h") (kbd "C-h o <return>"))

;; Just in case
(global-set-key (kbd "M-x") #'execute-extended-command)
(global-set-key (kbd "M-w") #'kill-ring-save)
