(cl-macrolet
    ((sub-keyword (group prefix kw sub clr)
       `(list (rx ,prefix symbol-start (group ,kw) symbol-end)
	   ,group
	   '(progn
	      (put-text-property (match-beginning ,group) (match-end ,group) 'display ,sub)
	      ,clr)))
     
     (clr-keyword (group prefix (&rest kws) clr)
       `(list (rx ,prefix symbol-start (group (| ,@kws)) symbol-end)
	   ,group
	   ',clr)))
  
  (defvar-local *7-billion-humans--keywords*
      (list
       ;; objects
       (clr-keyword 0 "'" ("nothing" "hole" "worker" "myitem" "datacube" "wall" "printer" "shredder") 'font-lock-number-face)

       ;; directions
       (clr-keyword 0 "'" ("c" "n" "ne" "e" "se" "s" "sw" "w" "nw") 'font-lock-number-face)

       ;; memory
       (clr-keyword 0 "'" ("mem1" "mem2" "mem3" "mem4") 'font-lock-warning-face)

       ;; operators
       (clr-keyword 1 "(" ("==" "!=" "<." "<=" ">." ">=" "&&" "||") 'all-the-icons-orange)

       ;; actions
       (clr-keyword 1 "(" ("step" "rand-step" "pickup" "giveto" "takefrom" "deliver" "drop" "write!") 'all-the-icons-lgreen)

       ;; smart-actions
       (clr-keyword 1 "(" ("nearest" "set!" "calc") 'all-the-icons-dred)

       ;; control flow
       (sub-keyword 1 "(" "if*" "if" 'all-the-icons-purple-alt)
       (clr-keyword 1 "(" ("ins-list" "if-else" "jump" "jump-label" "loop*" "while" "while-else" "do-while" "repeat" "end" "match")
		    'all-the-icons-purple-alt)

       ;; meta
       (clr-keyword 1 "(" ("compile-program" "def" "map-procedure") 'font-lock-builtin-face))))

(defvar-local *7-billion-humans--pretty-symbols*
    '(;; control-flow
      ("jump-label"   . ?󰌖)
      ("loop*"        . ?⟳)
      ("match"        . ?󰘬)
      ;; objects
      ("'nothing"     . ?󰝣)
      ("'hole"        . ?)
      ("'worker"      . ?)
      ("'myitem"      . ?󱅝)
      ("'datacube"    . ?)
      ("'wall"        . ?󰟾)
      ("'printer"     . ?󱞆)
      ("'shredder"    . ?)
      ;; directions
      ("'c"           . ?)
      ("'n"           . ?)
      ("'ne"          . ?)
      ("'e"           . ?)
      ("'se"          . ?)
      ("'s"           . ?)
      ("'sw"          . ?)
      ("'w"           . ?)
      ("'nw"          . ?)
      ;; operators
      ("=="           . ?=)
      (">."           . ?>)
      ("<."           . ?<)))

(defun 7-billion-humans-mode--add-keywords ()
  ;; Ensure `display` is managed by font-lock
  (add-to-list 'font-lock-extra-managed-props 'display)
  (font-lock-add-keywords nil *7-billion-humans--keywords*)
  (font-lock-flush))

(defun 7-billion-humans-mode--remove-keywords ()
  (font-lock-remove-keywords nil *7-billion-humans--keywords*)
  ;; Remove display properties left behind
  (remove-text-properties (point-min) (point-max) '(display nil))
  (font-lock-flush))

(defun 7-billion-humans-mode--add-pretty-symbols ()
  (setq prettify-symbols-alist 
	(map-merge 'list prettify-symbols-alist *7-billion-humans--pretty-symbols*))
  (prettify-symbols-mode 1))

(defun 7-billion-humans-mode--remove-pretty-symbols ()
  (dolist (pair *7-billion-humans--pretty-symbols*)
    (assoc-delete-all (car pair) prettify-symbols-alist))
  (prettify-symbols-mode 1))

(defun 7-billion-humans-mode--add-custom-indentation ()
  (put 'if* 'sly-common-lisp-indent-function 1)
  (put 'do-while 'sly-common-lisp-indent-function 1)
  (put 'repeat 'sly-common-lisp-indent-function 1)
  (put 'compile-program 'sly-common-lisp-indent-function 0))

(defun 7-billion-humans-mode--remove-custom-indentation ()
  (put 'if* 'sly-common-lisp-indent-function 'defun))

(defun 7-billion-humans-mode--enable ()
  (7-billion-humans-mode--add-keywords)
  (7-billion-humans-mode--add-pretty-symbols)
  (7-billion-humans-mode--add-custom-indentation))

(defun 7-billion-humans-mode--disable ()
  (7-billion-humans-mode--remove-keywords)
  (7-billion-humans-mode--remove-pretty-symbols)
  (7-billion-humans-mode--remove-custom-indentation))

(define-minor-mode 7-billion-humans-mode
  "A minor mode for better lisp editing for 7 Billion Humans, the videogame."
  :lighter " 7BH"
  (if 7-billion-humans-mode
      (7-billion-humans-mode--enable)
    (7-billion-humans-mode--disable)))

(provide '7-billion-humans-mode)
