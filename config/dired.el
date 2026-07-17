(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-d" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(defun my/dired-hide-permissions ()
  "Hide permissions field in dired buffer."
  (save-excursion
	(goto-char (point-min))
	(while (re-search-forward dired-re-perms nil t)
	  (let ((ov (make-overlay (match-beginning 0) (1+ (match-end 0)))))
		(overlay-put ov 'invisible t)
		(overlay-put ov 'evaporate t)))))

(add-hook 'dired-after-readin-hook #'my/dired-hide-permissions)
