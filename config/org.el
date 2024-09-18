;; Org-babel languages.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (python . t)
   (lisp . t)
   (rust . t)))

;; Skip confirmation when evaluating org-babel code blocks.
(defun my-org-confirm-babel-evaluate (lang body) nil)
(setq org-confirm-babel-evaluate #'my-org-confirm-babel-evaluate)

;; Change org-babel lisp-eval to sly-eval (default is slime).
(setq org-babel-lisp-eval-fn #'sly-eval)

;; Hide leading stars on headers.
(setq org-hide-leading-stars t)

;; Doom-org.
(doom-themes-org-config)

;; Add minor modes.
(add-to-hook 'org-mode-hook
	     '(toggle-word-wrap
	       toggle-word-wrap
	       toggle-word-wrap))
