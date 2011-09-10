;;; jka-load.el - Support for loading remote or compressed emacs lisp files.
;;; bugs/comments to jka@ece.cmu.edu
;;; Version 0.1
;;; Last modified 4/23/93


;;; Copyright (C) 1993  Jay K. Adams
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; LCD Archive Entry:
;;; jka-load|Jay Adams|jka@ece.cmu.edu|
;;; Load remote or compressed GNU Emacs lisp files.|
;;; 23-April-1993|0.1|~/misc/jka-load.el.Z|


;;; This package modifies the low-level elisp load function so that
;;; files are read in through insert-file-contents.  What this means
;;; is that if you have a package installed that makes
;;; insert-file-contents do special things like access remote files or
;;; automatically uncompress files (ange-ftp, jka-compr, or jam-zcat,
;;; for instance), then their action will also apply to loading lisp
;;; files.  I have found that using jka-load with a package like
;;; jka-compr (thus allowing you to compress all your emacs lisp
;;; files) is really useful since emacs lisp files are rarely accessed
;;; (usually only once per invocation of emacs) and they compress
;;; really well.


;;; Instructions:
;;; 
;;; Load the jka-load package.  Edit as usual.  The operation of this
;;; package should be completely invisible to the user.  
;;;
;;; After the package is loaded, all lisp file loading (whether done
;;; explicitly or through autoload or require) will be done using
;;; insert-file-contents.  Any package that modifies makes 
;;; insert-file-contents do special things to input files will now
;;; work with emacs lisp files.

;;; History
;;;
;;; 4/20/93    Added code to insert a set-buffer command at the
;;;            beginning of the lisp code buffer before evaluating the
;;;            buffer.  This is so that the current-buffer will be
;;;            correct when reading the lisp code.
;;;
;;; 4/23/93    Version 0.1


(defvar jka-load-regular-file-regexp "^/usr/gnu/lisp"
  "*Regular expression that matches ordinary emacs lisp files.
If the path name of a load file matches this regexp, it is loaded
using jka-compr-real-load file.  Otherwise, it is loaded using
jka-load-load.")


(defvar jka-load-lisp-file-extensions '(".elc" ".el" "")
  "List of extensions to try adding to emacs lisp load files.")


;;; This is sort of like the openp routine in lread.c except there is
;;; no exec_only arg and the suffix arg is a list instead of a string.
;;; In fact, if the lisp code looks a little strange here its because
;;; I pretty much transliterated the C version.
(defun jka-load-openp (path str suffix)
  "Duplicate the function of the openp routing in lread.c."
  (catch 'result
    (let ((absolute (file-name-absolute-p str))
	  filename suf try)
      (while path
	(catch 'continue
	  (setq filename (expand-file-name str (car path)))
	  (if (not (file-name-absolute-p filename))
	      (progn
		(setq filename (expand-file-name str default-directory))
		(if (not (file-name-absolute-p filename))
		    (throw 'continue nil))))

	  (setq suf suffix)
	  (while suf
	    (setq try (concat filename (car suf)))
	    (and (file-readable-p try)
		 (not (file-directory-p try))
		 (throw 'result try))
	    (setq suf (cdr suf))))

	(if absolute
	    (throw 'result nil)
	  (setq path (cdr path)))))

    nil))
      
   
(defun jka-load-load (file &optional noerror nomessage nosuffix)
  "Documented as original."
  (let* ((filename (jka-load-openp load-path file 
				   (if nosuffix
				       (cons "" nil)
				     jka-load-lisp-file-extensions))))
    (if (not filename)

	(or noerror
	    (error "Cannot open load file %s" file))

      (if (string-match jka-load-regular-file-regexp filename)

	  (jka-load-real-load filename noerror nomessage nosuffix)

	(let ((cbuf (current-buffer))
	      (lbufname (concat " *jka-load-temp:" filename))
	      lbuf)

	  (or nomessage
	      (message "Loading %s..." filename))

	  (unwind-protect
	      (progn
		(setq lbuf (get-buffer lbufname))
		(if lbuf
		    (set-buffer lbuf)
		  (setq lbuf (get-buffer-create lbufname))
		  (set-buffer lbuf)
		  (insert-file-contents filename))
		(set-buffer cbuf)
		(jka-load-eval-buffer lbuf))
	    (and
	     lbuf
	     (kill-buffer lbuf)))

	  (or nomessage
	      (message "Loading %s...done." filename)))))))


;;; Eval the contents of buffer BUF being careful not to change the
;;; current-buffer.  One way to do this is to write the buffer out to
;;; a temp file and load it back in.  That's sort of a drag because
;;; you would have to uniquely name each temp file so that loads
;;; within load files would work.  
;;;
;;; This function is sort of a hack.  It just puts a line at the
;;; beginning of the lisp code buffer that sets the current-buffer.
;;; Then it goes to the lisp code buffer and does eval-current-buffer.
;;; I think this is OK since buffers are gauranteed to have unique
;;; names.
(defun jka-load-eval-buffer (buf)
  (let* ((cbuf (current-buffer))
	 (set-buf-expr (list 'set-buffer
			     (list 'get-buffer (buffer-name cbuf)))))
    (print set-buf-expr buf)
    (set-buffer buf)    
    (eval-current-buffer)
    (and
     (buffer-name cbuf)
     (set-buffer cbuf))))


;;; This is just a lisp rewrite of the require as it appears in fns.c.
;;; It has to be rewritten so that the new version of load will be called.
(defun jka-load-require (feature &optional file)
  "Documented as original."
  (or (featurep feature)
      (progn
	(load (or file (symbol-name feature)) nil t nil)
	(or (featurep feature)
	    (error "Required feature %s was not provided." feature))))
  feature)


;;; See if sym's function binding is an autoload.
(defun jka-load-is-autoload (sym)
  "Return t if SYM's function definition is an autoload.
SYM must have a function binding.  It is assumed to be an autoload if
either the car of the definition is an autoload, or the definition
seems to have been created by jka-load-autoload."

  (let ((fun (symbol-function sym))
	(argdecl-pos 1)
	(da-pos 4))

    (while (symbolp fun)
      (setq fun (symbol-function fun)))

    ;; If it's a macro, move all the arg positions down one.
    (and (listp fun)
	 (eq (car fun) 'macro)
	 (setq argdecl-pos 2)
	 (setq da-pos 5))

    ;; See if it looks like its still and autoload.  Detecting
    ;; jka-load-autoload's is a little messy.
    (and (listp fun)
	 (or (eq (car fun) 'autoload)
	     (and (equal (nth argdecl-pos fun)
			 '(&rest jka-load-autoload-args))
		  (eq (car (nth da-pos fun)) 'jka-load-do-autoload))))))

      
  
;;; This seems to work in all useful cases.
;;; It differs from regular autoload in the following way: regular autoload has
;;; the ability to make a function (before it is loaded) appear to be both a
;;; macro and interactive.  Since jka-load-autoload binds the function to a
;;; lambda expression, it cannot make them interactive and macros at the same
;;; time.  The upshot is that if you autoload a funciton and declare it to be
;;; both a macro and interactive, it will not (before it is loaded) be
;;; interactive.
(defun jka-load-autoload (fun file &optional doc int macro)
  "Documented as original."
  (and 
   (or (not (fboundp fun))
       (jka-load-is-autoload fun))
   (fset fun
	 (append (and macro '(macro))
		 '(lambda (&rest jka-load-autoload-args))
		 ;; if doc and/or int are nil, put nils in the
		 ;; function definition.  This makes it easier to find
		 ;; out if a function definition was created by
		 ;; jka-load-autoload.
		 (list doc)
		 (list (and int '(interactive)))
		 (list (list 'jka-load-do-autoload
			     (list 'quote fun)
			     file 
			     (and int '(interactive-p))
			     macro
			     'jka-load-autoload-args))))))


;;; Use verbose formal parameters for this procedure 'cause any symbol binding
;;; it does will be seen by the function that is being autoloaded (when it is
;;; executed for the first time).
(defun jka-load-do-autoload (jka-load-autoload-fun
			     jka-load-autoload-file
			     jka-load-autoload-interactive
			     jka-load-autoload-macro
			     jka-load-autoload-args)
  "Procedure called when autoloading a function defined by jka-load-autoload."

  ;; Load it
  (load jka-load-autoload-file nil (not jka-load-autoload-interactive) nil)

  ;; Make sure it got defined
  (and 
   (or (not (fboundp jka-load-autoload-fun))
       (jka-load-is-autoload jka-load-autoload-fun))
   (error "Autoloading failed to define function %s." 
	  jka-load-autoload-fun))

  ;; Run it
  (if jka-load-autoload-interactive
      (call-interactively jka-load-autoload-fun)
    (if jka-load-autoload-macro
	(macroexpand (cons jka-load-autoload-fun jka-load-autoload-args))
      (apply jka-load-autoload-fun jka-load-autoload-args))))


;;; This function was lifted from ange-ftp.  I added some args to make
;;; it a little more general. - jka
(defun jka-load-overwrite-fn (fun saved-prefix new-prefix overwrite-msg)
  "Replace FUN's function definition with NEW-PREFIX-FUN's, saving the
original definition as SAVED-PREFIX-FUN.  The original documentation is
placed on the new definition suitably augmented.  Third arg, OVERWRITE-MSG,
is tacked on to the doc string of the new fun."

  (let* ((name (symbol-name fun))
	 (saved (intern (concat saved-prefix name)))
	 (new (intern (concat new-prefix name)))
	 (nfun (symbol-function new))
	 (exec-directory (if (or (equal (nth 3 command-line-args) "dump")
				 (equal (nth 4 command-line-args) "dump"))
			     "../etc/"
			   exec-directory)))			 
    
    (while (symbolp nfun)
      (setq nfun (symbol-function nfun)))
    
    (or (fboundp saved)
	(progn
	  (fset saved (symbol-function fun))
	  (fset fun new)))
    
    (let* ((doc-str (jka-load-safe-documentation saved))
	   (ndoc-str (concat doc-str (and doc-str "\n")
			     overwrite-msg)))
      
      (cond ((listp nfun)
	     ;; Probe to test whether function is in preloaded read-only
	     ;; memory, and if so make writable copy:
	     (condition-case nil
		 (setcar nfun (car nfun))
	       (error
		(setq nfun (copy-sequence nfun)) ; shallow copy only
		(fset new nfun)))
	     (let ((ndoc-cdr (nthcdr 2 nfun)))
	       (if (stringp (car ndoc-cdr))
		   ;; Replace the existing docstring.
		   (setcar ndoc-cdr ndoc-str)
		 ;; There is no docstring.  Insert the overwrite msg.
		 (setcdr ndoc-cdr (cons (car ndoc-cdr) (cdr ndoc-cdr)))
		 (setcar ndoc-cdr overwrite-msg))))
	    (t
	     ;; it's an emacs19 compiled-code object
	     (let ((new-code (append nfun nil))) ; turn it into a list
	       (if (nthcdr 4 new-code)
		   (setcar (nthcdr 4 new-code) ndoc-str)
		 (setcdr (nthcdr 3 new-code) (cons ndoc-str nil)))
	       (fset new (apply 'make-byte-code new-code))))))))


;;; Also lifted from ange-ftp.
(defun jka-load-safe-documentation (fun)
  "A documentation function that isn't quite as fragile."
  (condition-case ()
      (documentation fun)
    (error nil)))


;;; do the overwirtes

(defvar jka-load-overwrite-list
  '(
    load
    require
    autoload
    )
  "List of functions overwritten by the jka-load package.")


(mapcar
 (function
  (lambda (fn)
    (jka-load-overwrite-fn
     fn
     "jka-load-real-"
     "jka-load-"
     "Note: This function has been modified to load lisp files through
insert-file-contents.  It is defined in the jka-load package."
     ))
  )
 jka-load-overwrite-list)


(provide 'jka-load)
