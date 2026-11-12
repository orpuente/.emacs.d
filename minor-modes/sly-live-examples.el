;;; sly-live-examples.el --- Live inline example results for SLY  -*- lexical-binding: t; -*-

;;; Commentary:
;; Write example forms as comments starting with `;;>' anywhere in a
;; Lisp buffer.  With `sly-live-examples-mode' on, each example is
;; evaluated in the connected Lisp after every compilation, and its
;; result is shown at the end of the line.  Optionally, the toplevel
;; form at point is recompiled automatically when Emacs is idle.

;;; Code:

(require 'sly)
(require 'cl-lib)

(defvar sly-live-examples-mode)

(defgroup sly-live-examples nil
  "Live inline evaluation of example forms."
  :group 'sly)

(defcustom sly-live-examples-marker ";;>"
  "Comment prefix that marks a line as an example form."
  :type 'string)

(defcustom sly-live-examples-idle-delay 0.6
  "Seconds of idle time before recompiling the form at point.
Nil means only refresh after explicit compilation (e.g. \\[sly-compile-defun])."
  :type '(choice (const :tag "Only on explicit compile" nil) number))

(defface sly-live-examples-result-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for successful example results.")

(defface sly-live-examples-error-face
  '((t :inherit error :weight normal))
  "Face for example results that signalled a condition.")

(defvar-local sly-live-examples--overlays nil)
(defvar-local sly-live-examples--last-tick nil)
(defvar sly-live-examples--timer nil)

;;;; Collecting examples

(defun sly-live-examples--regexp ()
  (concat "^[ \t]*" (regexp-quote sly-live-examples-marker) "[ \t]*\\(.+?\\)[ \t]*$"))

(defun sly-live-examples--collect ()
  "Return a list of (EOL-POSITION . FORM-STRING) for every example line."
  (let (acc)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (sly-live-examples--regexp) nil t)
        (push (cons (line-end-position) (match-string-no-properties 1)) acc)))
    (nreverse acc)))

(defun sly-live-examples--on-example-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at (sly-live-examples--regexp))))

;;;; Overlays

(defun sly-live-examples--clear ()
  (mapc #'delete-overlay sly-live-examples--overlays)
  (setq sly-live-examples--overlays nil))

(defun sly-live-examples--show (ov status text)
  (when (overlay-buffer ov)
    (overlay-put ov 'after-string
                 (propertize (concat "   ⇒ " (replace-regexp-in-string "\n" " " text))
                             'face (if (eq status :ok)
                                       'sly-live-examples-result-face
                                     'sly-live-examples-error-face)))))

;;;; Evaluation in the Lisp image

(defun sly-live-examples--remote-form (strings)
  "Build a form that evaluates each string in STRINGS safely in the image."
  `(cl:let ((cl:*package* slynk::*buffer-package*)
            (cl:*print-length* 20)
            (cl:*print-level* 5))
     (cl:mapcar
      (cl:lambda (s)
        (cl:handler-case
            (cl:list :ok
                     (cl:format nil "~{~S~^, ~}"
                                (cl:multiple-value-list
                                 (cl:eval (cl:read-from-string s)))))
          (cl:serious-condition (c)
            (cl:list :error (cl:format nil "~A: ~A" (cl:type-of c) c)))))
      ',strings)))

(defun sly-live-examples-refresh ()
  "Re-evaluate all example forms in the buffer and update their results."
  (interactive)
  (sly-live-examples--clear)
  (when (sly-connected-p)
    (let* ((examples (sly-live-examples--collect))
           (ovs (mapcar (lambda (ex)
                          (let ((ov (make-overlay (car ex) (car ex) nil t nil)))
                            (overlay-put ov 'sly-live-examples t)
                            (sly-live-examples--show ov :ok "…")
                            ov))
                        examples)))
      (setq sly-live-examples--overlays ovs)
      (when examples
        (sly-eval-async (sly-live-examples--remote-form (mapcar #'cdr examples))
          (lambda (results)
            (cl-loop for ov in ovs
                     for (status text) in results
                     do (sly-live-examples--show ov status text))))))))

;;;; Triggers

(defun sly-live-examples--after-compile (&rest args)
  "Refresh examples in the buffer that was just compiled."
  (let ((buf (or (cl-find-if #'bufferp args) (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when sly-live-examples-mode
          (sly-live-examples-refresh))))))

(defun sly-live-examples--complete-toplevel-form-p ()
  "Non-nil if point is inside a toplevel form whose parens balance."
  (let ((orig (point)))
    (save-excursion
      (goto-char (line-end-position))
      (and (ignore-errors (beginning-of-defun) t)
           (ignore-errors (forward-sexp) t)
           (>= (point) orig)))))

(defun sly-live-examples--idle ()
  (when (and sly-live-examples-mode
             (sly-connected-p)
             (not (eql sly-live-examples--last-tick (buffer-chars-modified-tick))))
    (setq sly-live-examples--last-tick (buffer-chars-modified-tick))
    (cond ((sly-live-examples--on-example-line-p)
           (sly-live-examples-refresh))
          ((sly-live-examples--complete-toplevel-form-p)
           (sly-compile-defun)))))   ; compilation hook then refreshes

;;;; Commands

(defun sly-live-examples-insert ()
  "Add a new example line below the current toplevel definition."
  (interactive)
  (let ((name (save-excursion
                (goto-char (line-end-position))
                (beginning-of-defun)
                (ignore-errors
                  (down-list) (forward-sexp 2)
                  (thing-at-point 'symbol t)))))
    (end-of-defun)
    (skip-chars-backward " \t\n")
    (end-of-line)
    (insert "\n" sly-live-examples-marker " (" (or name "") " )")
    (backward-char)))

;;;; Mode

(defvar sly-live-examples-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c ;") #'sly-live-examples-refresh)
    (define-key map (kbd "C-c :") #'sly-live-examples-insert)
    map))

;;;###autoload
(define-minor-mode sly-live-examples-mode
  "Show the results of `;;>' example forms inline, updated as you edit."
  :lighter " Live"
  :keymap sly-live-examples-mode-map
  (if sly-live-examples-mode
      (progn
        (add-hook 'sly-compilation-finished-hook #'sly-live-examples--after-compile)
        (when (and sly-live-examples-idle-delay (not sly-live-examples--timer))
          (setq sly-live-examples--timer
                (run-with-idle-timer sly-live-examples-idle-delay t
                                     #'sly-live-examples--idle)))
        (sly-live-examples-refresh))
    (sly-live-examples--clear)
    ;; Tear down global state once no buffer uses the mode.
    (unless (cl-some (lambda (b) (buffer-local-value 'sly-live-examples-mode b))
                     (buffer-list))
      (remove-hook 'sly-compilation-finished-hook #'sly-live-examples--after-compile)
      (when sly-live-examples--timer
        (cancel-timer sly-live-examples--timer)
        (setq sly-live-examples--timer nil)))))

(provide 'sly-live-examples)
;;; sly-live-examples.el ends here
