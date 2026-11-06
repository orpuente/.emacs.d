;;; init.el --- Minimal Literate Bootstrap -*- lexical-binding: t; -*-

;; ---------------------------------------------------------------------------
;; 1. Startup Optimization (Garbage Collector)
;; ---------------------------------------------------------------------------
;; GC is disabled for startup in early-init.el and restored in section 6.

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
;; Don't `require' package.el at startup: packages are already activated
;; (via package-quickstart, see early-init.el) and loading package.el costs
;; ~30-45 ms. Configure it whenever something does load it.
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Uncomment if MELPA Stable is desired:
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; ---------------------------------------------------------------------------
;; 4. Recompile Files
;; ---------------------------------------------------------------------------
;; Byte-compile local Lisp whenever the source is newer than its .elc (or the
;; .elc is missing). If a compile fails, no .elc is written, and
;; `load-prefer-newer' makes Emacs load the newer .el source instead of a
;; stale .elc.
(setq load-prefer-newer t)

(require 'bytecomp)
(defun my/recompile-if-needed (file)
  "Byte-compile FILE (relative to `user-emacs-directory') if its .elc is stale."
  (byte-recompile-file (expand-file-name file user-emacs-directory) nil 0))

;; config.org -> config.el (tangle only when config.org changed)
(let ((org-file (expand-file-name "config.org" user-emacs-directory))
      (el-file  (expand-file-name "config.el"  user-emacs-directory)))
  (when (file-newer-than-file-p org-file el-file)
    (require 'org)
    (org-babel-tangle-file org-file el-file
                           (rx string-start (or "emacs-lisp" "elisp") string-end))
    ;; Mark config.el as up to date. This is needed for the case in which,
    ;; we edit non-source blocks in the .org file, in which case the file
    ;; would not be re-tangled, but the .org would be newer, and therefore
    ;; the load function would prefer the .el (source) file.
    (set-file-times el-file)))

;; Compile minor modes before config.el, because config.el loads them.
(mapc #'my/recompile-if-needed
      '("ob-racket/ob-racket.el"
        "minor-modes/window-mode.el"
        "minor-modes/structural-insert.el"
        "minor-modes/structural-editing.el"
        "config.el"))

;; ---------------------------------------------------------------------------
;; 5. Load Literate Configuration (config.org)
;; ---------------------------------------------------------------------------
(require 'use-package)                  ; compiled config calls into use-package/bind-key
(load (expand-file-name "config" user-emacs-directory) nil 'nomessage)

;; ---------------------------------------------------------------------------
;; 6. Restore Garbage Collector & Report Startup Time
;; ---------------------------------------------------------------------------
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1)
            (message "Emacs successfully loaded in %s" (emacs-init-time))))
(put 'narrow-to-region 'disabled nil)
