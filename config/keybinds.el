;;                                   Reason:
(keyboard-translate ?\C-i ?\s-i)   ; C-i = TAB
(keyboard-translate ?\C-\[ ?\s-\[) ; C-[ = M-w 

;; Unbinds
(define-key undo-tree-map (kbd "C-/") nil)
(define-key undo-tree-map (kbd "C-_") nil)
(keymap-unset undo-tree-map "C-/")
(keymap-unset lisp-interaction-mode-map "C-M-i")
(keymap-unset emacs-lisp-mode-map "C-M-i")
(keymap-unset mc/keymap "C-v")

(global-set-key (kbd "M-w") #'kill-ring-save)
(global-set-key (kbd "C-d") #'delete-char)

(dolist (key '("C-a" "C-k" "C-l" "C-n" "C-o" "C-p"
	       "C-t" "C-]" "s-[" "M-e"))
  (global-unset-key (kbd key)))

;; window movement
(global-set-key (kbd "C-0") #'other-window)

;; basic text manipulation
(global-set-key (kbd "C-z") #'undo-tree-undo)
(global-set-key (kbd "C-y") #'undo-tree-redo)
(global-set-key (kbd "C-v") #'yank)
(global-set-key (kbd "C-a") #'mark-whole-buffer)
(global-set-key (kbd "M-k") #'next-line)
(global-set-key (kbd "M-i") #'previous-line)

;;; ast-movement in lisp modes
;; p: previous
;; (define-key lisp-mode-shared-map (kbd "H-i") #')
;; (define-key lisp-mode-shared-map (kbd "M-i") #'sp-transpose-sexp) ; doesn't exist

;; n: next
;; (define-key lisp-mode-shared-map (kbd "H-k") #')
;; (define-key lisp-mode-shared-map (kbd "M-k") #'sp-transpose-sexp)

;; b: backwards
(define-key lisp-mode-shared-map (kbd "M-j") #'backward-char)
(define-key lisp-mode-shared-map (kbd "M-b") #'backward-char)
;; (define-key lisp-mode-shared-map (kbd "H-j") )
(define-key lisp-mode-shared-map (kbd "C-j") #'backward-sexp)

;; f: forward
(define-key lisp-mode-shared-map (kbd "M-l") #'forward-char)
(define-key lisp-mode-shared-map (kbd "M-f") #'forward-char)
;; (define-key lisp-mode-shared-map (kbd "H-l") #')
(define-key lisp-mode-shared-map (kbd "C-l") #'forward-sexp)
(define-key lisp-mode-shared-map (kbd "C-f") #'forward-sexp)

;; Arrows
(define-key lisp-mode-shared-map (kbd "C-<left>")  #'backward-sexp)
(define-key lisp-mode-shared-map (kbd "C-<right>") #'forward-sexp)
(define-key lisp-mode-shared-map (kbd "C-<up>")    #'sp-backward-up-sexp)
(define-key lisp-mode-shared-map (kbd "C-<down>")  #'sp-backward-down-sexp)
(define-key lisp-mode-shared-map (kbd "M-<up>")    #'sp-up-sexp)
(define-key lisp-mode-shared-map (kbd "M-<down>")  #'sp-down-sexp)

;;; ast-actions
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
(define-key lisp-mode-shared-map (kbd "s-[") #'sp-backward-slurp-sexp)
(define-key lisp-mode-shared-map (kbd "C-]") #'sp-backward-barf-sexp)
(define-key lisp-mode-shared-map (kbd "M-[") #'sp-forward-barf-sexp)
(define-key lisp-mode-shared-map (kbd "M-]") #'sp-forward-slurp-sexp)

;;; Lisp key-swaps
;;(define-key lisp-mode-shared-map (kbd ":") (lambda () (interactive) (insert ";")))
;;(define-key lisp-mode-shared-map (kbd ";") (lambda () (interactive) (insert ":")))
;; --

;;; Lisp key-shortcuts
(define-key lisp-mode-shared-map (kbd "M-SPC") (lambda () (interactive) (insert "-")))
(define-key lisp-mode-shared-map (kbd "C-r") #'raise-sexp)
(define-key lisp-mode-shared-map (kbd "C-x C-p") #'sly-eval-last-sexp-in-popup-buffer)

;; Quick Help
(global-set-key (kbd "M-h") (kbd "C-h o <return>"))

;; Just in case
(global-set-key (kbd "M-x") #'execute-extended-command)
