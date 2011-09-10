;;; pcl-cvs-util.el --- Utitlity functions for pcl-cvs

;; Copyright (C) 1998-2000  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: pcl-cvs
;; Version: v2_9_9
;; Revision: pcl-cvs-util.el,v 1.26 2000/03/05 21:32:21 monnier Exp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:


;;; Code:

(eval-when-compile (require 'cl))
(require 'cvs-compat)

;;;;
;;;; list processing
;;;l

(defsubst cvs-car (x) (if (consp x) (car x) x))
(defalias 'cvs-cdr 'cdr-safe)
(defsubst cvs-append (&rest xs)
  (apply 'append (mapcar (lambda (x) (if (listp x) x (list x))) xs)))

(defsubst cvs-every (-cvs-every-f -cvs-every-l)
  (eval (cons 'and (mapcar (lambda (x) (list -cvs-every-f x)) -cvs-every-l))))

(defun cvs-union (xs ys)
  (let ((zs ys))
    (dolist (x xs zs)
      (unless (member x ys) (push x zs)))))
      

(defun cvs-map (-cvs-map-f &rest -cvs-map-ls)
  (unless (cvs-every 'null -cvs-map-ls)
    (cons (apply -cvs-map-f (mapcar 'car -cvs-map-ls))
	  (apply 'cvs-map -cvs-map-f (mapcar 'cdr -cvs-map-ls)))))

(defun cvs-first (l &optional n)
  (if (null n) (car l)
    (when l
      (let* ((nl (list (pop l)))
	     (ret nl))
	(while (and l (> n 1))
	  (setcdr nl (list (pop l)))
	  (setq nl (cdr nl))
	  (decf n))
	ret))))

(defun cvs-partition (p l)
  "Partition a list L into two lists based on predicate P.
The function returns a `cons' cell where the `car' contains
elements of L for which P is true while the `cdr' contains
the other elements.  The ordering among elements is maintained."
  (let (car cdr)
    (dolist (x l)
      (if (funcall p x) (push x car) (push x cdr)))
    (cons (nreverse car) (nreverse cdr))))

;;;; 
;;;; frame, window, buffer handling
;;;; 

(defun cvs-pop-to-buffer-same-frame (buf)
  "Pop to BUF like `pop-to-buffer' but staying on the same frame.
If `pop-to-buffer' would have opened a new frame, this function would
try to split the a new window instead."
  (let ((pop-up-windows (or pop-up-windows pop-up-frames))
	(pop-up-frames nil))
    (or (let ((buf (get-buffer-window buf))) (and buf (select-window buf)))
	(and pop-up-windows
	     (ignore-errors (select-window (split-window-vertically)))
	     (switch-to-buffer buf))
	(pop-to-buffer (current-buffer)))))

(defun cvs-bury-buffer (buf &optional mainbuf)
  "Hide the buffer BUF that was temporarily popped up.
BUF is assumed to be a temporary buffer used from the buffer MAINBUF."
  (interactive (list (current-buffer)))
  (save-current-buffer
    (let ((win (if (eq buf (window-buffer (selected-window))) (selected-window)
		 (get-buffer-window buf t))))
      (when win
	(if (window-dedicated-p win)
	    (condition-case ()
		(delete-window win)
	      (error (iconify-frame (window-frame win))))
	  (if (and mainbuf (get-buffer-window mainbuf))
	      (delete-window win)))))
    (with-current-buffer buf
      (bury-buffer (unless (and (eq buf (window-buffer (selected-window)))
				(not (window-dedicated-p (selected-window))))
		     buf)))
    (when mainbuf
      (let ((mainwin (or (get-buffer-window mainbuf)
			 (get-buffer-window mainbuf 'visible))))
	(when mainwin (select-window mainwin))))))
	      
(defun cvs-get-buffer-create (name &optional noreuse)
  "Create a buffer NAME unless such a buffer already exists.
If the NAME looks like an absolute file name, the buffer will be created
with `create-file-buffer' and will probably get another name than NAME.
In such a case, the search for another buffer with the same name doesn't
use the buffer name but the buffer's `list-buffers-directory' variable.
If NOREUSE is non-nil, always return a new buffer."
  (or (and (not (file-name-absolute-p name)) (get-buffer-create name))
      (unless noreuse
	(dolist (buf (buffer-list))
	  (with-current-buffer buf
	    (when (equal name list-buffers-directory)
	      (return buf)))))
      (with-current-buffer (create-file-buffer name)
	(set (make-local-variable 'list-buffers-directory) name)
	(current-buffer))))

;;;;
;;;; string processing
;;;;

(defun cvs-file-to-string (file &optional oneline args)
  "Read the content of FILE and return it as a string.
If ONELINE is t, only the first line (no \\n) will be returned.
If ARGS is non-nil, the file will be executed with ARGS as its
arguments.  If ARGS is not a list, no argument will be passed."
  (with-temp-buffer
    (condition-case nil
	(progn
	  (if args
	      (apply 'call-process
		     file nil t nil (when (listp args) args))
	    (insert-file-contents file))
	  (buffer-substring (point-min)
			    (if oneline
				(progn (goto-char (point-min)) (end-of-line) (point))
			      (point-max))))
      (file-error nil))))

(defun cvs-string-prefix-p (str1 str2)
  "Tell whether STR1 is a prefix of STR2."
  (let ((length1 (length str1)))
    (and (>= (length str2) length1)
	 (string= str1 (substring str2 0 length1)))))

;; (string->strings (strings->string X)) == X
(defun cvs-strings->string (strings &optional separator)
  "Concatenate the STRINGS, adding the SEPARATOR (default \" \").
This tries to quote the strings to avoid ambiguity such that
  (cvs-string->strings (cvs-strings->string strs)) == strs
Only some SEPARATOR will work properly."
  (let ((sep (or separator " ")))
    (mapconcat
     (lambda (str)
       (let ((newstr (string-replace-match "[\\\"]" str "\\\\\\&" nil t)))
	 (if (or (and newstr (not (equal newstr str)))
		 (string= str "")
		 (string-match "\\(\\s-\\|[']\\)" str))
	     (concat "\"" (or newstr str) "\"")
	   str)))
     strings sep)))

;; (string->strings (strings->string X)) == X
(defun cvs-string->strings (string &optional separator)
  "Split the STRING into a list of strings.
It understands elisp style quoting within STRING such that
  (cvs-string->strings (cvs-strings->string strs)) == strs
The SEPARATOR regexp defaults to \"\\s-+\"."
  (let ((sep (or separator "\\s-+"))
	(i (string-match "[\"]" string)))
    (if (null i) (split-string string sep)	; no quoting:  easy
      (append (unless (eq i 0) (split-string (substring string 0 i) sep))
	      (let ((rfs (read-from-string string i)))
		(cons (car rfs)
		      (cvs-string->strings (substring string (cdr rfs)) sep)))))))
      

(defun cvs-string-fill (str n &optional filling truncate)
  "Add FILLING (defaults to the space char) to STR to reach size N.
If STR is longer than N, truncate if TRUNCATE is set, else don't do anything."
  (let ((l (length str)))
    (if (> l n)
	(if truncate (substring str 0 n) str)
      (concat str (make-string (- n l) (or filling ? ))))))

;;;; 
;;;; defmap
;;;; 

(defun cvs-custom-create-map (m bs args)
  (let (inherit dense suppress)
    (while args
      (let ((key (first args))
	    (val (second args)))
	(cond
	 ((eq key :dense) (setq dense val))
	 ((eq key :inherit) (setq inherit val))
	 ((eq key :group) )
	 ;;((eq key :suppress) (setq suppress val))
	 (t (message "Uknown argument %s in defmap" key))))
      (setq args (cddr args)))
    (unless (keymapp m)
      (setq bs (append m bs))
      (setq m (if dense (make-keymap) (make-sparse-keymap))))
    (dolist (b bs)
      (let ((keys (car b))
	    (binding (cdr b)))
	(dolist (key (if (consp keys) keys (list keys)))
	  (cond
	   ((symbolp key)
	    (substitute-key-definition key binding m global-map))
	   ((null binding)
	    (unless (keymapp (lookup-key m key)) (define-key m key binding)))
	   ((let ((o (lookup-key m key)))
	      (or (null o) (numberp o) (eq o 'undefined)))
	    (define-key m key binding))))))
    (cond
     ((keymapp inherit) (set-keymap-parent m inherit))
     ((consp inherit) (set-keymap-parents m inherit)))
    m))

(defmacro cvs-defmap (m bs doc &rest args)
  `(defconst ,m
     (cvs-custom-create-map (if (boundp ',m) ,m) ,bs ,(cons 'list args))
     ,doc))

;;;; 
;;;; Defining a new major mode
;;;; 

(defconst cvs-define-major-mode-locals
  '((set-syntax-table . "mode-syntax-table")
    (local-abbrev-table . "mode-abbrev-table")
    (font-lock-defaults . "font-lock-defaults")
    (outline-regexp . "outline-regexp")
    (imenu-create-index-function . "imenu-create-index")
    (imenu-generic-expression . "imenu-generic-expression"))
  "Alist of variables or functions and the corresponding end-of-var-name.
Describe the conventional name to be used for each case.
The actual variable name is mabe by concatenating the mode name,
a `-' and the right-hand side of each cons cell.  For example,
for `sml-mode', the variable containing the table to be passed to
`set-syntax-table' should be `sml-mode-syntax-table'.
If the left hand side is function, the var's content should be passed
to it as its only argument, else it is assumed to be a variable
that is made local to the buffer before being assigned to.")

(defmacro cvs-define-major-mode (lighter doc &rest body)
  "Define a major mode following the usual convention.
LIGHTER is the text that should appear in the modeline.
DOC is the doc string.  The keymap is automatically appended.
The mode's main function is (concat (downcase LIGHTER) \"-mode\").
The function begins by killing all local variables and setting the
mode and the local keymap.  Then it tries to setup some other
standard features (using `cvs-define-major-mode-locals') if provided.
Finaly, it runs BODY and finishes by running <foo-mode>-hook."
  (let* ((base (downcase lighter))
	 (name (concat base "-mode"))
	 (mode (intern name))
	 (keymap-name (concat name "-map"))
	 (hook-name (concat name "-hook")))
    `(progn
       ;; shut up the compiler
       ,@(mapcar (lambda (x) `(defvar ,(intern (concat base "-" (cdr x)))))
		 cvs-define-major-mode-locals)
       
       (defun ,mode ()
	 ,(concat "\\<" keymap-name ">" doc "\nThis mode runs `"
		  hook-name "' just before exiting."
		  "\n\\{" keymap-name "}")
	 (interactive)
	 (kill-all-local-variables)
	 (setq major-mode ',mode)
	 (setq mode-name ,lighter)
	 (use-local-map ,(intern keymap-name))
	 (mapcar (lambda (x)
		   (let* ((v (car x)) (tail (cdr x))
			  (sym (intern (concat ,base "-" tail))))
		     (if (boundp sym)
			 (if (fboundp v) (funcall v (symbol-value sym))
			   (set (make-local-variable v) (symbol-value sym))))))
		 (or (and (boundp 'cvs-define-major-mode-locals)
			  cvs-define-major-mode-locals)
		     ',cvs-define-major-mode-locals))
	 ,@body
	 (run-hooks ',(intern hook-name))))))

;;;; 
;;;; file names
;;;; 

(defsubst cvs-expand-dir-name (d)
  (file-name-as-directory (expand-file-name d)))

;;;;
;;;; (interactive <foo>) support function
;;;;

(defstruct (cvs-qtypedesc
	    (:constructor nil) (:copier nil)
	    (:constructor cvs-qtypedesc-create
			  (str2obj obj2str &optional complete hist-sym require)))
  str2obj
  obj2str
  hist-sym
  complete
  require)


(defconst cvs-qtypedesc-string1 (cvs-qtypedesc-create 'identity 'identity t))
(defconst cvs-qtypedesc-string (cvs-qtypedesc-create 'identity 'identity))
(defconst cvs-qtypedesc-strings
  (cvs-qtypedesc-create 'cvs-string->strings 'cvs-strings->string nil))

(defun cvs-query-read (default prompt qtypedesc &optional hist-sym)
  (let* ((qtypedesc (or qtypedesc cvs-qtypedesc-strings))
	 (hist-sym (or hist-sym (cvs-qtypedesc-hist-sym qtypedesc)))
	 (complete (cvs-qtypedesc-complete qtypedesc))
	 (completions (and (functionp complete) (funcall complete)))
	 (initval (funcall (cvs-qtypedesc-obj2str qtypedesc) default)))
    (funcall (cvs-qtypedesc-str2obj qtypedesc)
	     (cond
	      ((null complete) (read-string prompt initval hist-sym))
	      ((functionp complete)
	       (completing-read prompt completions
				nil (cvs-qtypedesc-require qtypedesc)
				initval hist-sym))
	      (t initval)))))

;;;; 
;;;; Flags handling
;;;; 

(defstruct (cvs-flags
	    (:constructor nil)
	    (:constructor -cvs-flags-make
			  (desc defaults &optional qtypedesc hist-sym)))
  defaults persist desc qtypedesc hist-sym)

(defmacro cvs-flags-define (sym defaults
				&optional desc qtypedesc hist-sym docstring)
  `(defconst ,sym
     (let ((bound (boundp ',sym)))
       (if (and bound (cvs-flags-p ,sym)) ,sym
	 (let ((defaults ,defaults))
	   (-cvs-flags-make ,desc
			    (if bound (cons ,sym (cdr defaults)) defaults)
			    ,qtypedesc ,hist-sym))))
     ,docstring))

(defun cvs-flags-query (sym &optional desc arg)
  "Query flags based on SYM.
Optional argument DESC will be used for the prompt
If ARG (or a prefix argument) is nil, just use the 0th default.
If it is a non-negative integer, use the corresponding default.
If it is a negative integer query for a new value of the corresponding
  default and return that new value.
If it is \\[universal-argument], just query and return a value without
  altering the defaults.
If it is \\[universal-argument] \\[universal-argument], behave just
  as if a negative zero was provided."
  (let* ((flags (symbol-value sym))
	 (desc (or desc (cvs-flags-desc flags)))
	 (qtypedesc (cvs-flags-qtypedesc flags))
	 (hist-sym (cvs-flags-hist-sym flags))
	 (arg (if (eq arg 'noquery) 0 (or arg current-prefix-arg 0)))
	 (numarg (prefix-numeric-value arg))
	 (defaults (cvs-flags-defaults flags))
	 (permstr (if (< numarg 0) (format " (%sth default)" (- numarg)))))
    ;; special case for universal-argument
    (when (consp arg)
      (setq permstr (if (> numarg 4) " (permanent)" ""))
      (setq numarg 0))

    ;; sanity check
    (unless (< (abs numarg) (length defaults))
      (error "There is no %sth default." (abs numarg)))

    (if permstr
	(let* ((prompt (format "%s%s: " desc permstr))
	       (fs (cvs-query-read (nth (- numarg) (cvs-flags-defaults flags))
				   prompt qtypedesc hist-sym)))
	  (when (not (equal permstr ""))
	    (setf (nth (- numarg) (cvs-flags-defaults flags)) fs))
	  fs)
      (nth numarg defaults))))

(defsubst cvs-flags-set (sym index value)
  "Set SYM's INDEX'th setting to VALUE."
  (setf (nth index (cvs-flags-defaults (symbol-value sym))) value))

;;;; 
;;;; Prefix keys
;;;; 

(defconst cvs-prefix-number 10)

(defsubst cvs-prefix-sym (sym) (intern (concat (symbol-name sym) "-cps")))

(defmacro cvs-prefix-define (sym docstring desc defaults
				 &optional qtypedesc hist-sym)
  (let ((cps (cvs-prefix-sym sym)))
    `(progn
       (defvar ,sym nil ,(cons (or docstring "") "
See `cvs-prefix-set' for further description of the behavior."))
       (defconst ,cps
	 (let ((defaults ,defaults))
	   ;; sanity ensurance
	   (unless (>= (length defaults) cvs-prefix-number)
	     (setq defaults (append defaults
				    (make-list (1- cvs-prefix-number)
					       (first defaults)))))
	   (-cvs-flags-make ,desc defaults ,qtypedesc ,hist-sym))))))

(defun cvs-prefix-make-local (sym)
  (let ((cps (cvs-prefix-sym sym)))
    (make-local-variable sym)
    (set (make-local-variable cps) (copy-cvs-flags (symbol-value cps)))))

(defun cvs-prefix-set (sym arg)
  ;; we could distinguish between numeric and non-numeric prefix args instead of
  ;; relying on that magic `4'.
  "Set the cvs-prefix contained in SYM.
If ARG is between 0 and 9, it selects the corresponding default.
If ARG is negative (or \\[universal-argument] which corresponds to negative 0),
  it queries the user and sets the -ARG'th default.
If ARG is greater than 9 (or \\[universal-argument] \\[universal-argument]),
  the (ARG mod 10)'th prefix is made persistent.
If ARG is NIL toggle the PREFIX's value between its 0th default and NIL
  and reset the persistence."
  (let* ((prefix (symbol-value (cvs-prefix-sym sym)))
	 (numarg (if (integerp arg) arg 0))
	 (defs (cvs-flags-defaults prefix)))

    ;; set persistence if requested
    (when (> (prefix-numeric-value arg) 9)
      (setf (cvs-flags-persist prefix) t)
      (setq numarg (mod numarg 10)))

    ;; set the value
    (set sym
	 (cond
	  ((null arg)
	   (setf (cvs-flags-persist prefix) nil)
	   (unless (symbol-value sym) (first (cvs-flags-defaults prefix))))

	  ((or (consp arg) (< numarg 0))
	   (setf (nth (- numarg) (cvs-flags-defaults prefix))
		 (cvs-query-read (nth (- numarg) (cvs-flags-defaults prefix))
				 (format "%s: " (cvs-flags-desc prefix))
				 (cvs-flags-qtypedesc prefix)
				 (cvs-flags-hist-sym prefix))))
	  (t (nth numarg (cvs-flags-defaults prefix)))))
    (force-mode-line-update)))

(defun cvs-prefix-get (sym &optional read-only)
  "Return the current value of the prefix SYM.
and reset it unless READ-ONLY is non-nil."
  (prog1 (symbol-value sym)
    (unless (or read-only
		(cvs-flags-persist (symbol-value (cvs-prefix-sym sym))))
      (set sym nil)
      (force-mode-line-update))))

(provide 'pcl-cvs-util)

;;; pcl-cvs-util.el ends here
