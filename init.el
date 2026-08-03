;;; init.el --- Minimal Literate Bootstrap -*- lexical-binding: t; -*-

;; ---------------------------------------------------------------------------
;; 1. Startup Optimization (Garbage Collector)
;; ---------------------------------------------------------------------------
;; Temporarily raise the GC threshold to 50MB to speed up initial loading
(setq gc-cons-threshold (* 50 1024 1024))

;; ---------------------------------------------------------------------------
;; 2. Custom Variables Isolation
;; ---------------------------------------------------------------------------
;; Redirect UI/Custom modifications (custom-set-variables / custom-set-faces)
;; to a separate file so they don't clutter your main init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;; ---------------------------------------------------------------------------
;; 3. Package Repositories & Defaults
;; ---------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Uncomment to profile `init.el'.
;; (setq use-package-compute-statistics t)

;; Uncomment to debug use-package loads.
;; (setq use-package-verbose t)

;; Uncomment if MELPA Stable is desired:
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Default behavior for use-package
(setq use-package-always-defer t)

;; ---------------------------------------------------------------------------
;; 4. Load Literate Configuration (config.org)
;; ---------------------------------------------------------------------------
(require 'org)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

;; ---------------------------------------------------------------------------
;; 5. Restore Garbage Collector & Report Startup Time
;; ---------------------------------------------------------------------------
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1024 1024))
            (message "Emacs successfully loaded in %s" (emacs-init-time))))
(put 'narrow-to-region 'disabled nil)
