(use-package org
  :config (progn
	    (org-babel-do-load-languages
	     'org-babel-load-languages
	     '((C . t)
	       (python . t)
	       (lisp . t)
	       (rust . t)
	       ))))

(use-package org-modern
  :ensure t
  :demand t
  :config (progn
	    (setq
	     ;; Edit settings
	     org-auto-align-tags nil
	     org-tags-column 0
	     org-catch-invisible-edits 'show-and-error
	     org-special-ctrl-a/e t
	     org-insert-heading-respect-content t

	     ;; Org styling, hide markup etc.
	     org-hide-emphasis-markers t
	     org-pretty-entities t
	     org-agenda-tags-column 0
	     org-ellipsis "…")

	    (global-org-modern-mode)))

;; Skip confirmation when evaluating org-babel code blocks.
(defun my-org-confirm-babel-evaluate (lang body) nil)
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)

;; Change org-babel lisp-eval to sly-eval (default is slime).
(setq org-babel-lisp-eval-fn #'sly-eval)

;; Hide leading stars on headers.
(setq org-hide-leading-stars t)

;; Add minor modes.
(add-to-hook 'org-mode-hook
	     '(toggle-word-wrap))
