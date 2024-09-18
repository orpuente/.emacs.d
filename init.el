(require 'cl-lib)
(require 'display-line-numbers)
(require 'ef-themes)
(require 'multiple-cursors)
(require 'org-tempo)
(require 'package)
(require 'rainbow-delimiters)
(require 'smartparens-config)
(require 'undo-tree)
(when (display-graphic-p)
  (require 'all-the-icons))
(require 'doom-themes)
(require 'doom-modeline)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-support-shift-select t)
 '(package-selected-packages
   '(yasnippet lsp-ui lsp-mode rust-mode ob-rust doom-modeline org-superstar all-the-icons neotree doom-themes minimap multiple-cursors company sly-quicklisp zoom ef-themes rainbow-delimiters undo-tree sly smartparens)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and
	 (eq isdir nil)
	 (string= (substring path -3) ".el")
	 (not (string= (substring path 0 2) ".#")))
        (load (file-name-sans-extension fullpath)))))))

(load-directory "~/.emacs.d/config-utils")
(load-directory "~/.emacs.d/config")

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
