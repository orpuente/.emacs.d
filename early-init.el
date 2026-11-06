;;; early-init.el --- Pre-GUI, pre-package startup settings -*- lexical-binding: t; -*-

;; Runs before package activation and before the first frame is created.
;; Anything that shapes the initial frame or speeds up package loading
;; belongs here; everything else lives in config.org.

;; ---------------------------------------------------------------------------
;; 1. Garbage Collector
;; ---------------------------------------------------------------------------
;; Disable GC for the whole startup (package activation included). init.el
;; restores a sane threshold in `emacs-startup-hook'.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1.0)

;; ---------------------------------------------------------------------------
;; 2. File-name handlers
;; ---------------------------------------------------------------------------
;; Every `load'/`require' matches the file name against these regexps
;; (TRAMP, jka-compr, ...). Skip them during startup and restore afterwards,
;; keeping anything that was added while init ran.
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist
                  (delete-dups (append file-name-handler-alist
                                       my/file-name-handler-alist))))
          -90)

;; ---------------------------------------------------------------------------
;; 3. Initial frame
;; ---------------------------------------------------------------------------
;; Create the first frame with its final font and without tool/menu/scroll
;; bars, instead of building a default frame and changing it afterwards.
;; Applies to `emacsclient -c' frames too.
(setq default-frame-alist
      (append '((font . "FiraCode Nerd Font Mono 16")
                (menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars . nil)
                (horizontal-scroll-bars . nil))
              default-frame-alist))
;; Keep the mode variables consistent with the frame parameters above.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
;; Don't resize the frame when the font or UI elements change.
(setq frame-inhibit-implied-resize t)

;; ---------------------------------------------------------------------------
;; 4. Package quickstart
;; ---------------------------------------------------------------------------
;; Load one precomputed autoloads file (package-quickstart.el) instead of
;; many separate *-autoloads.el files. package.el regenerates it on install,
;; upgrade and delete; run `M-x package-quickstart-refresh' if you change
;; elpa/ by hand.
(setq package-quickstart t)

;; ---------------------------------------------------------------------------
;; 5. Native-comp trampolines without loading the compiler
;; ---------------------------------------------------------------------------
;; Advising a C primitive (doom-modeline advises `rename-buffer') makes
;; Emacs look for a cached native-comp trampoline .eln. Computing its
;; name calls `comp-c-func-name', which lives in comp.el, so the first
;; such advice loads the whole native compiler (~30 ms) even when the
;; .eln already exists. Compute the name here instead (Emacs 30+).
;; If the computed file isn't found -- because it isn't cached yet, or
;; because this formula has drifted from upstream -- or comp.el is
;; already loaded, defer to the original, so behavior stays correct.
(defun my/trampoline-filename (subr)
  "Return SUBR's trampoline file name, like `comp-trampoline-filename'."
  (let* ((name (symbol-name subr))
         (hex (mapconcat (lambda (c) (substring (format "%x" c) 0 2)) name))
         (readable (replace-regexp-in-string
                    (rx (not (any "0-9a-z_"))) ""
                    (string-replace "-" "_" name))))
    (format "subr--trampoline-%s_%s_0.eln" hex readable)))

(defun my/fast-trampoline-filename (orig-fun subr)
  "Return SUBR's trampoline file name without loading comp.el if possible.
Around advice for `comp-trampoline-filename'; ORIG-FUN is the original."
  (or (and (not (featurep 'comp))
           (let ((file (my/trampoline-filename subr)))
             (and (locate-file file (comp-eln-load-path-eff))
                  file)))
      (funcall orig-fun subr)))

(advice-add 'comp-trampoline-filename :around #'my/fast-trampoline-filename)
