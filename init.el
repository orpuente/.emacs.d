;; If setting up the config from scratch remember to.
;; 1. (package-refresh-contents)
;; 2. (package-upgrade-all)

;; Since we are using `use-package' we don't want to 'require` all packages.
;; So, we DO NOT call `package-initialize'. We also don't use `require'.

;; These packages are not added to 'load-path` for some reason, so we add them manually.
(add-to-list 'load-path "/home/orpuente/.emacs.d/elpa/queue-0.2/")
(add-to-list 'load-path "/home/orpuente/.emacs.d/elpa/smartparens-20250612.1050/")
(add-to-list 'load-path "/home/orpuente/.emacs.d/elpa/undo-tree-0.8.2/")

;; Alwas set (:defer t) when using `use-package'.
(setq use-package-always-defer t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; We pass the &optional async-download argument to not block emacs.
;; Additionally, we run this function after emacs has initialized.
;; Before doing these two things this was by far the slowest initialization step.
(add-hook 'emacs-startup-hook (lambda () (package-refresh-contents t)))

;; Some global packages.
(use-package cl-lib :demand t)
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t
  :demand t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-support-shift-select t)
 '(package-selected-packages
   '(all-the-icons company doom-modeline doom-themes ef-themes
		   multiple-cursors rainbow-delimiters smartparens
		   undo-tree)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)


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

(message "Init time: %s" (emacs-init-time))
