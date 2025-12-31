(defun orpl-translate-keys (&optional frame) ; Reason:
  (keyboard-translate ?\C-i ?\s-i)       ; C-i = TAB
  (keyboard-translate ?\C-\[ ?\s-\[))    ; C-[ = M-w 
(add-hook 'after-make-frame-functions #'orpl-translate-keys)
(orpl-translate-keys)

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

(defun orpl-backward-transpose-sexp ()
  (interactive)
  (sp-transpose-sexp -1))

(defun orpl-lisp-editing-keybinds (keymap)
  ;;; jlik
  ;; Ctrl
  (define-key keymap (kbd "M-j") #'backward-char)
  (define-key keymap (kbd "M-l") #'forward-char)
  (define-key keymap (kbd "M-i") #'previous-line)
  (define-key keymap (kbd "M-k") #'next-line)

  ;; Super (WindowsKey)
  (define-key keymap (kbd "s-j") #'sp-backward-up-sexp)
  (define-key keymap (kbd "s-l") #'sp-up-sexp)
  (define-key keymap (kbd "s-i") #'sp-backward-down-sexp)
  (define-key keymap (kbd "s-k") #'sp-down-sexp)

  ;; Meta (AltKey)
  (define-key keymap (kbd "C-j") #'backward-sexp)
  (define-key keymap (kbd "C-l") #'forward-sexp)
  (define-key keymap (kbd "H-i") #'backward-sexp) 
  (define-key keymap (kbd "C-k") #'forward-sexp)

  ;; C-M-[key] (Ctrl + Alt + [key])
  ;; (define-key keymap (kbd "M-SPC-j") nil)
  ;; (define-key keymap (kbd "M-SPC-l") nil)
  (define-key keymap (kbd "M-SPC M-k") #'sp-transpose-sexp)
  (define-key keymap (kbd "M-SPC M-i") #'orpl-backward-transpose-sexp)

  ;; Arrows
  (define-key keymap (kbd "C-<left>")  #'backward-sexp)
  (define-key keymap (kbd "C-<right>") #'forward-sexp)
  (define-key keymap (kbd "C-<up>")    #'backward-sexp)
  (define-key keymap (kbd "C-<down>")  #'forward-sexp)

  ;;; ast-actions
  (define-key keymap (kbd "H-[") #'sp-backward-slurp-sexp)
  (define-key keymap (kbd "C-]") #'sp-backward-barf-sexp)
  (define-key keymap (kbd "M-[") #'sp-forward-barf-sexp)
  (define-key keymap (kbd "M-]") #'sp-forward-slurp-sexp)
  (define-key keymap (kbd "C-r") #'raise-sexp))

;; Quick Help
(global-set-key (kbd "M-h") (kbd "C-h o <return>"))

;; Just in case
(global-set-key (kbd "M-x") #'execute-extended-command)
(global-set-key (kbd "M-w") #'kill-ring-save)
