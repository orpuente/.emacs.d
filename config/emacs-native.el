;;;; Set some basic config vars.

;; Change font
(set-frame-font "FiraCode Nerd Font 9")

;; Fonts for emacsclient.
(add-to-list 'default-frame-alist '(font . "FiraCode Nerd Font 9"))

;; Disable ring-bell.
(setq ring-bell-function #'ignore)

;; Disable tool-bar.
(tool-bar-mode -1)

;; Enable line numbers in all buffers.
(global-display-line-numbers-mode -1)

;; Disable scroll-bar, line-numbers are good enouugh.
;; A minimap like in VS-Code would be more useful.
(scroll-bar-mode -1)

;; Disable menu-bar. I never used it in the first two weeks of using Emacs.
(menu-bar-mode -1)

;; Inserting text while the mark is active
;; causes the selected text to be deleted first.
(delete-selection-mode 1)

;; Set common-lisp backend to SBCL
;; To start sly faster, we can point sbcl to a custom core with Slynk dependencies preloaded.
;; Reference: https://joaotavora.github.io/sly/#Loading-Slynk-faster
(setq inferior-lisp-program "sbcl --dynamic-space-size 4096")

;; Actually close help buffers (and others) when you press 'q'.
(defadvice quit-window (before quit-window-always-kill)
  "When running `quit-window', always kill the buffer."
  (when (called-interactively-p 'interactive) (ad-set-arg 0 t)))
(ad-activate 'quit-window)

;; Move cursor to recently open help window
(setq help-window-select t)

;; Hide cursor in non-selected window
(setq-default cursor-in-non-selected-windows nil)

;; Hide title bar
;; (setq default-frame-alist '((undecorated . t)))
;; (add-to-list 'default-frame-alist '(drag-internal-border . 1))
;; (add-to-list 'default-frame-alist '(internal-border-width . 5))

;; Allow scrolling during isearch
(setq isearch-allow-scroll 'unlimited)
