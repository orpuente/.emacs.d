(use-package smartparens
  :ensure smartparens
  :hook ((prog-mode . smartparens-mode)
         (prog-mode . smartparens-strict-mode))
  :config (require 'smartparens-config))


