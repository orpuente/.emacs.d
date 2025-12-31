;;;; CL HyperSpec
(setq browse-url-browser-function 'eww-browse-url
      common-lisp-hyperspec-root "file:///home/orpuente/.emacs.d/lisp/documentation/HyperSpec-7-0/HyperSpec/")

;;;; keybinds
(define-key lisp-mode-shared-map (kbd "C-x C-p") #'sly-eval-last-sexp-in-popup-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
(orpl-lisp-editing-keybinds lisp-mode-shared-map)
