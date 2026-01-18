(use-package smartparens
  :ensure smartparens
  :hook (((prog-mode sly-mrepl-mode) . smartparens-mode)
         ((prog-mode sly-mrepl-mode) . smartparens-strict-mode))
  :config (progn
	    (require 'smartparens-config)
	    (setq sp-highlight-pair-overlay nil)
	    (setq sp-highlight-wrap-overlay nil)
	    (setq sp-highlight-wrap-tag-overlay nil)))
