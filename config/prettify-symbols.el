(defun prog-mode-pretty-symbols ()
  "This function enables pretty symbols for all
programming modes. It is to be added to the `'prog-mode-hook'."
  (setq prettify-symbols-alist
	'(("lambda" . ?λ)
	  ("fn"     . ?󰊕)
	  ("struct" . ?)
	  ("enum"   . ?)
	  ("self"   . ?○)
	  ("Self"   . ?○)
	  ("!="     . ?≠)
	  ("<="     . ?≤)
	  (">="     . ?≥)
	  ("for"    . ?∀)
	  ("..."    . ?…)))
  (prettify-symbols-mode 1))

(defun shared-lisp-pretty-symbols ()
  "This function enables pretty symbols for all
lisp modes. It is to be added to the lisp mode hooks."
  (setq prettify-symbols-alist
	'(("lambda"       . ?λ)
	  ("defun"        . ?󰊕)
	  ("defmethod"    . ?)
	  ("defstruct"    . ?)
	  ("defmacro"     . ?󰡷)
	  ("defparameter" . ?)
	  ("defvar"       . ?󰕷)
	  ("cond"         . ?󰘬)
	  ("self"         . ?○)
	  ("!="           . ?≠)
	  ("<="           . ?≤)
	  (">="           . ?≥)
	  ("for"          . ?∀)
	  ("..."          . ?…)
	  ("list"         . ?ℒ)
	  ("nil"          . ?∅)
	  ("union"        . ?∪)
	  ("sqrt"         . ?√)))
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
  "This function enables pretty symbols for CL3."
  (setq prettify-symbols-alist
	'(("lambda"       . ?λ)
	  ("define"       . ?𝒹)
	  ("cond"         . ?󰘬)
	  ("!="           . ?≠)
	  ("<="           . ?≤)
	  (">="           . ?≥)
	  ("list"         . ?ℒ)
	  ("null"         . ?∅)))
  (prettify-symbols-mode 1))

;; Pretty symbols hooks.
(add-hook 'prog-mode-hook             #'prog-mode-pretty-symbols)
(add-hook 'emacs-lisp-mode-hook       #'shared-lisp-pretty-symbols)
(add-hook 'lisp-mode-hook             #'shared-lisp-pretty-symbols)
(add-hook 'cl3-mode-hook              #'cl3-pretty-symbols)
(add-hook 'scheme-mode-hook           #'scheme-pretty-symbols)
(add-hook 'lisp-interaction-mode-hook #'shared-lisp-pretty-symbols)

