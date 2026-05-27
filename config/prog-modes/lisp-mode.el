;;;; CL HyperSpec
(setq browse-url-browser-function 'eww-browse-url
      common-lisp-hyperspec-root "file:///home/orpuente/.emacs.d/lisp/documentation/HyperSpec-7-0/HyperSpec/")

;;;; Keybinds
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
(define-key lisp-mode-shared-map (kbd "C-x C-p") #'sly-eval-last-sexp-in-popup-buffer)
(orpl-lisp-editing-keybinds lisp-mode-shared-map)

;;;; Hooks
(add-hook 'sly-mode-hook
          (lambda ()
            (unless (sly-connected-p)
	      (define-key sly-mode-map (kbd "M-e") (lambda () (interactive) (sly-compile-defun) (sleep-for 0.1) (sly-disassemble-definition)))
              (save-excursion (sly)))))
