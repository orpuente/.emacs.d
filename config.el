(use-package cl-lib :demand t)

(use-package no-littering
  :ensure t
  :demand t
  :config
  ;; Redirect auto-save files (#filename#) to var/auto-save/
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  ;; Redirect backup files (filename~) to var/backup/
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/")))))

;; Hide UI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode -1)

;; Disable ring-bell.
(setq ring-bell-function #'ignore)

;; Editing and navigation behavior
(delete-selection-mode 1)                        ; Replace selected text when typing
(setq isearch-allow-scroll 'unlimited)           ; Allow scrolling during isearch
(setq-default cursor-in-non-selected-windows nil)  ; Hide cursor in inactive windows

(defun my/translate-keys (&optional frame)
  "Translate raw C-i and C-[ sequences to avoid collisions with TAB and ESC."
  (keyboard-translate ?\C-i ?\s-i)
  (keyboard-translate ?\C-\[ ?\s-\[))

(add-hook 'after-make-frame-functions #'my/translate-keys)
(my/translate-keys)

;; Unbind mode-specific C-M-i bindings
(keymap-unset lisp-interaction-mode-map "C-M-i" t)
(keymap-unset emacs-lisp-mode-map "C-M-i" t)

;; Global and mode-specific unbinds
(dolist (key '("C-z" "C-a" "C-n" "C-o" "C-p"
               "C-t" "C-]" "H-[" "M-e" "M-f"
               "C-k" "C-l" "H-i" "C-j" "C-_" "C-/"
               "s-j" "s-l" "s-i" "s-k" "s-["))
  (unbind-key key)
  (keymap-global-unset key t)
  (keymap-unset lisp-interaction-mode-map key t))

(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font Mono 16")) ;; standalone
(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono 16")        ;; emacsclient

;; Prevent Emacs from using proportional default fonts for symbols
(setq use-default-font-for-symbols nil)

;; Set a monospace font that contains math symbols (e.g., JuliaMono or DejaVu Sans Mono)
;; https://github.com/cormullion/juliamono/releases
(defvar my/fallback-font "JuliaMono")

(set-fontset-font t 'greek             (font-spec :family my/fallback-font))
(set-fontset-font t 'mathematical      (font-spec :family my/fallback-font))

(dolist (range '((#x2100 . #x214f)     ; ℒ
                 (#x2200 . #x22ff)     ; ∅ and math comparisons
                 (#x1d400 . #x1d7ff))) ; 𝒹
  (set-fontset-font t range (font-spec :family my/fallback-font)))

(setq inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(setq-default tab-width 4
	          indent-tabs-mode nil)

(setq backward-delete-char-untabify-method nil)

(setq help-window-select t)

(define-advice quit-window (:before (&optional kill window) always-kill)
  "Ensure `quit-window' always kills the buffer when called interactively."
  (when (called-interactively-p 'interactive)
    (setq kill t)))

;;(setq native-comp-async-report-warnings-errors nil)
;;(setq warning-minimum-level :error)

(use-package ef-themes
  :ensure t
  :demand t
  :init
  ;; Load the default theme before displaying frames
  ;; (load-theme 'ef-bio :no-confirm)
  (load-theme 'ef-maris-dark :no-confirm)
  :bind
  (("C-S-n" . ef-themes-load-random-dark)
   ("C-S-p" . (lambda ()
                (interactive)
                (load-theme 'ef-maris-dark :no-confirm)))))

(use-package doom-modeline
  :ensure t
  :demand t
  :init (doom-modeline-mode 1))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode))

(use-package idle-highlight-mode
  :ensure t
  :hook prog-mode
  :config
  (setq idle-highlight-idle-time 0.02))

(defun cl-pretty-symbols ()
  "This function enables pretty symbols for cl-like lisps."
  (setq prettify-symbols-alist
	    '(("lambda"       . ?λ)
	      ("defun"        . ?󰊕)
	      ("defmacro"     . ?󰡷)
	      ("defparameter" . ?)
	      ("defvar"       . ?󰕷)
	      ("cond"         . ?󰘬)
	      ("!="           . ?≠)
	      ("<="           . ?≤)
	      (">="           . ?≥)
	      ("list"         . ?ℒ)
	      ("nil"          . ?∅)))
  (prettify-symbols-mode 1))

(defun cl3-pretty-symbols ()
  "This function enables pretty symbols for CL3."
  (setq prettify-symbols-alist
	    '(("lambda"       . ?λ)
	      ("def"          . ?󰊕)
	      ("defm"         . ?)
	      ("defstruct"    . ?)
	      ("defmacro"     . ?󰡷)
	      ("defprm"       . ?)
	      ("defvar"       . ?󰕷)
	      ("cond"         . ?󰘬)
	      ("self"         . ?○)
	      ("!="           . ?≠)
	      ("<="           . ?≤)
	      (">="           . ?≥)
	      ("for"          . ?∀)
	      ("list"         . ?ℒ)
	      ("nil"          . ?∅)))
  (prettify-symbols-mode 1))

(defun scheme-pretty-symbols ()
  "This function enables pretty symbols for scheme-like lisps."
  (setq prettify-symbols-alist
	    '(("lambda"             . ?λ)
	      ("define"             . ?𝒹)
          ("define-syntax-rule" . ?󰡷)
	      ("cond"               . ?󰘬)
	      ("!="                 . ?≠)
	      ("<="                 . ?≤)
	      (">="                 . ?≥)
	      ("list"               . ?ℒ)
	      ("null"               . ?∅)))
  (prettify-symbols-mode 1))

;; Pretty symbols hooks.
(add-hook 'emacs-lisp-mode-hook       #'cl-pretty-symbols)
(add-hook 'lisp-interaction-mode-hook #'cl-pretty-symbols)
(add-hook 'lisp-mode-hook             #'cl-pretty-symbols)
(add-hook 'cl3-mode-hook              #'cl3-pretty-symbols)
(add-hook 'racket-mode-hook           #'scheme-pretty-symbols)
(add-hook 'scheme-mode-hook           #'scheme-pretty-symbols)
(add-hook 'geiser-repl-mode-hook      #'scheme-pretty-symbols)
(add-hook 'racket-repl-mode-hook      #'scheme-pretty-symbols)
(add-hook 'sly-mrepl-mode-hook        #'cl-pretty-symbols)

(blink-cursor-mode -1)

(use-package beacon
  :ensure t
  :init
  (beacon-mode 1)
  :custom
  ;;(beacon-color "#15B046")
  (beacon-blink-when-window-scrolls t)
  (beacon-blink-when-window-changes t))

(use-package ultra-scroll
  ;;:vc (:url "https://github.com/jdtsmith/ultra-scroll") ; if desired (emacs>=v30)
  :ensure t
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :hook (dired-mode . dired-hide-details-mode)
  :bind ("C-d" . dired-jump)
  :custom (dired-listing-switches "-agho --group-directories-first"))

;; TODO: Figure out how to use use nerd-icons-dired instead
;; (use-package nerd-icons-dired :hook (dired-mode . nerd-icons-dired-mode))

;; File and folder icons in Dired
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; Colorize permissions, dates, and file sizes
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(defun my/dired-hide-permissions ()
  "Hide permissions field in dired buffer."
  (save-excursion
	(goto-char (point-min))
	(while (re-search-forward dired-re-perms nil t)
	  (let ((ov (make-overlay (match-beginning 0) (1+ (match-end 0)))))
	(overlay-put ov 'invisible t)
	(overlay-put ov 'evaporate t)))))

(add-hook 'dired-after-readin-hook #'my/dired-hide-permissions)

;; Vertico: Modern completion UI
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t)) ;; Cycle candidates infinitely

;; Marginalia: Rich contextual annotations next to commands
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode 1))

;; Vertico-Posframe: Renders Vertico as a centered floating overlay
(use-package vertico-posframe
  :ensure t
  :after vertico
  :init
  (vertico-posframe-mode 1)
  :custom
  (vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center)
  (vertico-posframe-min-width 50)
  (vertico-posframe-border-width 2)
  (vertico-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8))))

;; (ido-mode 1)
;; (ido-everywhere 1)
;; (setq ido-enable-flex-matching t)

;; Orderless: Match search terms in any order (fuzzy searching)
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Consult: Powerful search and navigation commands (Telescope alternative)
(use-package consult
  :ensure t
  :bind
  ;; File and Buffer Search
  (("C-b"     . consult-buffer)           ; Enhanced buffer switcher with preview
   ("C-c f"   . consult-fd)               ; Find files using 'fd' (very fast)
   ("C-c g"   . consult-ripgrep)          ; Live grep inside files with ripgrep
   ("C-s"     . consult-line)             ; Search text inside current buffer with preview
   :map minibuffer-local-map
   ("M-s"     . consult-history))
  :init
  ;; Live preview setup
  (setq consult-preview-key 'any))

(use-package company
  :ensure t
  :defer t
  :hook ((prog-mode . company-mode)
         (geiser-repl-mode . company-mode))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  :bind
  (("C-SPC" . company-complete)
   :map company-active-map
   ("M-j" . company-select-next)
   ("M-k" . company-select-previous)
   ("<tab>" . company-complete-selection)
   ("TAB" . company-complete-selection)
   ("<enter>" . nil)
   ("RET" . nil)
   ("C-h" . nil)))

(use-package undo-tree
  :ensure t
  :defer 1
  :bind (:map undo-tree-map
              ("C-/" . nil)
              ("C-_" . nil)
              ("C-z" . undo-tree-undo)
              ("C-y" . undo-tree-redo))
  :config (global-undo-tree-mode))

(use-package smartparens
  :ensure t
  :hook (((prog-mode geiser-repl-mode racket-repl-mode sly-mrepl-mode) . smartparens-strict-mode))
  :config
  (require 'smartparens-config)
  
  ;; Disable single-quote auto-pairing specifically for programming and Lisp modes
  (sp-with-modes '(prog-mode emacs-lisp-mode lisp-interaction-mode geiser-repl-mode sly-mrepl-mode)
    (sp-local-pair "'" nil :actions nil))

  ;; Enable auto-pairing for Org-specific formatting tags
  (sp-with-modes 'org-mode
    (sp-local-pair "*" "*")
    (sp-local-pair "=" "=")
    (sp-local-pair "~" "~")
    (sp-local-pair "/" "/"))

  ;; Disable overlay highlights
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode geiser-repl-mode racket-repl-mode sly-mrepl-mode) . rainbow-delimiters-mode))

(use-package multiple-cursors
  :ensure t
  :demand t
  :config (progn
	    (keymap-unset mc/keymap "C-v")
	    (keymap-unset mc/keymap "<return>")
	    (global-set-key (kbd "C-M-<up>") #'mc/mark-previous-like-this)
	    (global-set-key (kbd "C-M-<down>") #'mc/mark-next-like-this)
	    (global-set-key (kbd "C-M-i") #'mc/mark-previous-like-this)
	    (global-set-key (kbd "C-M-k") #'mc/mark-next-like-this)))

(setq browse-url-browser-function 'eww-browse-url
      common-lisp-hyperspec-root "file:///home/orpuente/.emacs.d/lisp/documentation/HyperSpec-7-0/HyperSpec/")

(defun enable-ansi-colors-in-sly-mrepl ()
  (add-to-list 'sly-mrepl-output-filter-functions 'ansi-color-apply))

(use-package sly
  :ensure t
  :hook ((sly-mrepl-mode-hook . enable-ansi-colors-in-sly-mrepl)))

(use-package cl3-mode
  :vc (:url "https://gitlab.com/sebbb/cl3-mode"))

(defvar cl-custom-indents
  '((defshader (4 4 (&lambda &body)))
	(clsl (&lambda &body))))

(defun cl-put-custom-indent (symbol indent-spec)
  (put symbol 'common-lisp-indent-function indent-spec))

(defun cl-activate-custom-indents ()
  (interactive)
  (mapc (lambda (indent) (apply #'cl-put-custom-indent indent))
		cl-custom-indents)
  'done)

(dolist (hook '(sly-mode-hook sly-mrepl-hook))
  (add-hook hook #'cl-activate-custom-indents))

(use-package geiser
  :ensure t
;;  :hook ((scheme-mode-hook . geiser-mode))
  :config (setq geiser-mode-start-repl-p t))

(use-package racket-mode
  :ensure t
;;  :hook racket-repl
  :config
  (my/lisp-editing-keybinds racket-repl-mode-map)
  (my/lisp-editing-keybinds racket-mode-map))

(use-package org
  :ensure nil
  :hook (toggle-word-wrap visual-line-mode)
  :bind (:map org-mode-map
              ("s-i" . (lambda () (interactive) (insert "#+begin_src \n#+end_src") (forward-line -1) (end-of-line))))
  :custom
  (org-support-shift-select t)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)

  :load-path "ob-racket"
  
  :config
  ;; Load Org Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
	 (python . t)
	 (lisp . t)
	 (racket . t))))

(use-package ob-racket
  :after org
  :pin manual)

(use-package org-modern
  :ensure t
  :demand t
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-agenda-tags-column 0)
  (org-ellipsis " ")
  :config
  (global-org-modern-mode 1))

(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-enable))

(use-package polymode
  :ensure t
  :defer t
  :config
  ;; Fixes occasional indentation jumps when shifting modes.
  (setq polymode-move-to-inner-on-create nil))

(use-package poly-org
  :ensure t
  :after (org polymode)
  :hook org-mode
  :config
  ;; Stops the mode from changing src blocks faces.
  (oset poly-org-innermode :adjust-face '(:inherit org-block :extend t))
  (oset poly-org-innermode :protect-font-lock t))

;; (mapcar #'eieio-slot-descriptor-name (eieio-class-slots (class-of poly-org-hostmode)))

(bind-keys
 ;; Common editing shortcuts
 ("C-a" . mark-whole-buffer)
 ("C-v" . yank))

;; Quick Help
(global-set-key (kbd "M-h") (kbd "C-h o <return>"))

;; Fold code
(use-package hideshow
  :ensure nil
  :bind (("C-f" . #'hs-toggle-hiding)
         ("M-f" . #'hs-toggle-hiding))
  :hook (prog-mode . hs-minor-mode))

(use-package hydra
  :ensure t)

(load "~/.emacs.d/minor-modes/window-mode.el")
(use-package window-mode
  :config (window-mode 1))

(global-set-key (kbd "C-0") #'other-window)

(defun my/backward-transpose-sexp ()
  (interactive)
  (sp-transpose-sexp -1))

(defun my/kill-whitespace ()
  (interactive)
  (let ((start (point)))
	(skip-chars-forward " \t\n\r\f")
	(when (> (point) start)
	  (delete-region start (point))
	  t)))

(defun my/kill-whitespace-backward ()
  (interactive)
  (let ((start (point)))
	(skip-chars-backward " \t\n\r\f")
	(when (< (point) start)
	  (delete-region (point) start)
	  t)))

(defun my/kill-whitespace-or-sexp ()
  (interactive)
  (unless (my/kill-whitespace)
	(sp-kill-sexp)))

(defun my/kill-whitespace-or-sexp-backward ()
  (interactive)
  (unless (my/kill-whitespace-backward)
	(sp-backward-kill-sexp)))

(defun my/lisp-editing-keybinds (keymap)
  ;; C-M-[key] (Ctrl + Alt + [key])
  (define-key keymap (kbd "M-<left>") #'my/backward-transpose-sexp)
  (define-key keymap (kbd "M-<right>") #'sp-transpose-sexp)
  (define-key keymap (kbd "M-<up>") #'my/backward-transpose-sexp)
  (define-key keymap (kbd "M-<down>") #'sp-transpose-sexp)

  ;; C-[Arrows]
  (define-key keymap (kbd "C-<left>")  #'backward-sexp)
  (define-key keymap (kbd "C-<right>") #'forward-sexp)
  (define-key keymap (kbd "C-<up>")    #'backward-sexp)
  (define-key keymap (kbd "C-<down>")  #'forward-sexp)
  
  ;;; ast-actions
  (define-key keymap (kbd "H-[") #'sp-backward-slurp-sexp)
  (define-key keymap (kbd "C-]") #'sp-backward-barf-sexp)
  (define-key keymap (kbd "M-[") #'sp-forward-barf-sexp)
  (define-key keymap (kbd "M-]") #'sp-forward-slurp-sexp)
  (define-key keymap (kbd "C-r") #'raise-sexp)
  (define-key keymap (kbd "C-(") #'sp-backward-unwrap-sexp)
  (define-key keymap (kbd "C-)") #'sp-unwrap-sexp)
  (define-key keymap (kbd "C-<delete>") #'my/kill-whitespace-or-sexp)
  (define-key keymap (kbd "C-<backspace>") #'my/kill-whitespace-or-sexp-backward))

(my/lisp-editing-keybinds lisp-mode-shared-map)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode-unless-remote))
  :config
  ;; Refresh indicators cleanly when using Magit
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
