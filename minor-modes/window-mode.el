;;; window-mode.el --- Jupyter-style window manipulation mode -*- lexical-binding: t; -*-

;; Author: Oscar Puente <orpuente98@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (hydra "0.15.0"))
;; Keywords: convenience, frames
;; URL: https://github.com/orpuente/window-mode

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Inspired by the cell mode of Jupyter notebooks: enable
;; `window-mode' and press ESC to enter a transient Window mode in
;; which the arrow keys move between windows, shift+arrows swap
;; them, and single keys split, close, and resize them. Press RET
;; (or q) to settle into the selected window.
;;
;; While active, the selected window is framed with the theme's
;; mode-line color (top bar + side fringes + the mode line itself)
;; and the cursor is hidden.
;;
;; Usage:
;;
;;   (use-package window-mode
;;     :config (window-mode 1))
;;
;; Requires GUI Emacs; in terminals ESC is the Meta prefix.

;;; Code:

(require 'hydra)

;;;; Options

(defgroup window-mode nil
  "Jupyter-style window manipulation mode."
  :group 'windows
  :prefix "window-mode-")

(defcustom window-mode-accent-color nil
  "Accent color for the window frame.
When nil, use the background of `mode-line-active', which matches
themes that give the mode line a distinct color (e.g. ef-themes)."
  :type '(choice (const :tag "Follow theme" nil) color))

(defcustom window-mode-fringe-width 8
  "Width in pixels of the side accents while Window mode is active."
  :type 'natnum)

;;;; Internal state

(defface window-mode-border '((t))
  "Face for the top accent bar. Its color is set dynamically.")

(defconst window-mode--bar
  (propertize " " 'face 'window-mode-border
              'display '(space :align-to right))
  "A single space stretched to fill the header line with color.")

(defvar window-mode--win nil
  "The window currently carrying the top accent bar.")

(defvar window-mode--cursor nil
  "Saved `cursor-type' to restore on exit.")

(defvar window-mode--active nil
  "Non-nil while the Window mode hydra is active.")

;;;; Highlighting

(defun window-mode--accent-color ()
  "Return the accent color for the current theme."
  (or window-mode-accent-color
      (face-attribute 'mode-line-active :background nil t)))

(defun window-mode--paint (&rest _)
  "Color the accent faces from the current theme.
Called on entry and after any theme switch while active. The
fringe uses a face override spec: it layers on top of the theme
without polluting the face's defaults, and is cleanly removed on
exit."
  (when window-mode--active
    (let ((color (window-mode--accent-color)))
      (set-face-attribute 'window-mode-border nil :background color)
      (face-spec-set 'fringe `((t :background ,color))
                     'face-override-spec))))

(defun window-mode--follow (&optional _frame)
  "Move the accents to the selected window.
The top bar is a window parameter, so it is inherently
per-window. Fringe color is global, so the selected window gets
thick fringes while all others get zero-width ones -- the color
then shows only around the selection."
  (when (window-live-p window-mode--win)
    (set-window-parameter window-mode--win 'header-line-format nil))
  (setq window-mode--win (selected-window))
  (set-window-parameter window-mode--win 'header-line-format window-mode--bar)
  (walk-windows
   (lambda (win)
     (if (eq win window-mode--win)
         (set-window-fringes win window-mode-fringe-width
                             window-mode-fringe-width)
       (set-window-fringes win 0 0)))
   'no-minibuf))

;;;; Entry and exit

(defun window-mode--enter ()
  "Activate Window mode visuals."
  (setq window-mode--active t)
  ;; Hide the cursor. The save is non-destructive so a double
  ;; entry can never clobber the real value.
  (unless window-mode--cursor
    (setq window-mode--cursor (or (default-value 'cursor-type) t)))
  (setq-default cursor-type nil)
  ;; Paint and place the accents, then keep them in sync.
  (window-mode--paint)
  (window-mode--follow)
  (add-hook 'window-selection-change-functions #'window-mode--follow)
  (add-hook 'enable-theme-functions #'window-mode--paint))

(defun window-mode--exit ()
  "Restore everything Window mode changed."
  (setq window-mode--active nil)
  (setq-default cursor-type (or window-mode--cursor t))
  (setq window-mode--cursor nil)
  (remove-hook 'window-selection-change-functions #'window-mode--follow)
  (remove-hook 'enable-theme-functions #'window-mode--paint)
  (when (window-live-p window-mode--win)
    (set-window-parameter window-mode--win 'header-line-format nil))
  (setq window-mode--win nil)
  ;; Drop the fringe override (back to pure theme control) and
  ;; restore default fringe widths everywhere.
  (face-spec-set 'fringe nil 'face-override-spec)
  (walk-windows (lambda (win) (set-window-fringes win nil)) 'no-minibuf))

;;;; The hydra

(defhydra window-mode-hydra (:color amaranth :hint nil
                             :body-pre (window-mode--enter) ; once, on entry
                             :post     (window-mode--exit)) ; once, on exit
  "
Window Mode
  arrows: move   shift+arrows: swap
  _s_: split below  _v_: split right  _d_: close
  _o_: close others  _+_/_-_: resize  _=_: balance
  _RET_/_q_: exit
"
  ;; navigate
  ("<left>"  windmove-left)
  ("<down>"  windmove-down)
  ("<up>"    windmove-up)
  ("<right>" windmove-right)
  ;; swap
  ("S-<left>"  windmove-swap-states-left)
  ("S-<down>"  windmove-swap-states-down)
  ("S-<up>"    windmove-swap-states-up)
  ("S-<right>" windmove-swap-states-right)
  ;; split / close
  ("s" split-window-below)
  ("v" split-window-right)
  ("d" delete-window)
  ("o" delete-other-windows)
  ;; resize
  ("+" enlarge-window)
  ("-" shrink-window)
  ("=" balance-windows)
  ;; exit into the current window
  ("RET" nil :color blue)
  ("q" nil :color blue))

;;;; Minor mode

(defvar window-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<escape>") #'window-mode-hydra/body)
    map)
  "Keymap for `window-mode'.")

;;;###autoload
(define-minor-mode window-mode
  "Jupyter-style window manipulation, entered with ESC.
While the hydra is active, arrow keys move between windows,
shift+arrows swap them, and RET settles into the selected
window."
  :global t
  :keymap window-mode-map
  :lighter " Win")

(provide 'window-mode)
;;; window-mode.el ends here
