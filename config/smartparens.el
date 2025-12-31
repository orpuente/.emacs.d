(use-package smartparens
  :ensure smartparens
  :hook (((prog-mode sly-mrepl-mode) . smartparens-mode)
         ((prog-mode sly-mrepl-mode) . smartparens-strict-mode))
  :config (require 'smartparens-config))


