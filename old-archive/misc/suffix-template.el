;;; suffix-template.el
;; AUTHOR:
;;   Ralf Fassel <ralf@physik3.gwdg.de>
;;
;; DESCRIPTION:
;;   For a new file the default behaviour is to put the filename in a
;;   comment at the start of the buffer and an end of file marker at the
;;   end if a comment syntax is defined for the current mode.  This may
;;   be altered/added to by lisp code in a template file based on the
;;   file suffix.  The template files (one file per suffix) are searched
;;   for in the directory that `suffix-template-dir' points to.  For
;;   details see docstring of `suffix-template' below.
;;
;; INSTALLATION:
;;   Add this to your .emacs:
;;         (require 'suffix-template)
;;         (add-hook 'find-file-hooks 'suffix-template)
;;   and put this elisp file somewhere on your load-path.  This assumes
;;   emacs version 19.
;;
;; CUSTOMIZATION:
;;    Variable suffix-template-dir:
;;         Directory containing template files for `suffix-template'.
;;    Varibale suffix-template-ignore-files:
;;         List of filenames that `suffix-template' should not work on.
;;
;; EXAMPLE TEMPLATE FILES:
;;    See end of file.
;;
;; CREDITS:
;;    Thanks to Martin Hansen who served as beta tester victim.
;;    This is inspired by (though not derived from) the auto-template
;;    package by Kevin Broadey.
;;
;; LCD Archive Entry:
;; suffix-template|Ralf Fassel|ralf@physik3.gwdg.de|
;; Load a template file based on file name suffixes.|
;; 18-Jan-94|$Revision: 1.5 $|~/misc/suffix-template.el.Z|
;;
;; COPYRIGHT:
;; Copyright (C) 1993 Ralf Fassel <ralf@physik3.gwdg.de>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You don't know the GNU General Public License?
;; Write to the Free Software Foundation, Inc.,
;; 675 Mass Ave, Cambridge, MA 02139, USA.
;;

;; $Log: suffix-template.el,v $
; Revision 1.5  1994/08/30  18:41:02  ralf
; Add "TAGS" to `suffix-template-ignore-files'.
;
; Revision 1.4  1994/04/29  09:47:09  ralf
; `suffix-template' had an extra ')'.  Removed.
;
; Revision 1.3  1994/04/24  13:23:13  ralf
; added variable suffix-template-ignore-files: don't do anything for
; filenames that match any element in the list unless called interactively.
;
; Revision 1.2  1994/03/08  11:20:28  ralf
; make `suffix-template' interactive:
; Call interactively to force it on empty buffers.
;
; Revision 1.1  1994/02/04  09:37:16  ralf
; Initial revision
;;;-------------------------------------------------------------

;;; Code:
;;; user definable variables
(defvar suffix-template-dir "~/templates/"
  "Directory containing template files for `suffix-template'.")

(defvar suffix-template-ignore-files
  '("/Changelog\\'" "/RMAIL\\'" "/README\\'" "/TAGS\\'")
  "List of filename regexps which `suffix-template' should not work on.
The complete buffer-file-name is matched against each element in this
list.  This list is meant to avoid trouble with packages like RMAIL and
the Version Control stuff that rely on an empty new file.")

;;;
(defun suffix-template ()
  "Load the template file for the current buffers visited file if the
buffer is writeable and the buffer size is 0.  `suffix-template' by
default does not work on files which already exist or have names that
match any element in `suffix-template-ignore-files'.  Erase the buffer
and call it interactively to force it.

The template file is named after the file-name-suffix \(\"el\" for
\"foo.el\"\) and is searched for in directory `suffix-template-dir'.
Files without suffix load their template from file \"no.suffix\".

The template file should contain lisp code to either set
`text-before-point' and `text-after-point' to strings or do all
insertion by its own and set them to nil.  `text-before/after-point' are
inserted in the buffer and point is left in between if they are still
strings after loading the template file.  The buffer is marked
unmodified after loading the template.

The default is to put the filename in a comment at the start of the
buffer and an end of file marker at the end if a comment syntax is
defined for the current mode.

While loading the template file the following variables are bound:
filename -- filename without directory,
filename-base -- filename without suffix,
filename-suffix -- suffix only.
Note that `buffer-file-name' by default holds the complete filename."
  ;; Said enough, let's do it.
  (interactive)
  ;; First check/make sure:
  (cond (buffer-read-only nil)
	((> (buffer-size) 0) nil)
	((and (not (interactive-p))
	      (or (file-exists-p buffer-file-name)
		  (let ((list suffix-template-ignore-files) elt)
		    (catch 'match
		      (while list
			(setq elt (car list))
			(if (and (stringp elt)
				 (string-match elt buffer-file-name))
			    (throw 'match buffer-file-name)
			  (setq list (cdr list)))))))) nil)
	;; ok, do our stuff...
	('default
	  (let* ((filename (file-name-nondirectory buffer-file-name))
		 (index (string-match "\\.[^.]+$" filename))
		 filename-base filename-suffix template
		 text-before-point text-after-point)
	    ;; is there a suffix?
	    (if index
		(progn
		  (setq filename-base (substring filename 0 index))
		  (setq filename-suffix (substring filename (1+ index)))
		  (setq template (concat suffix-template-dir filename-suffix)))
	      ;; else: no suffix
	      (setq filename-base filename)
	      (setq template (concat suffix-template-dir "no.suffix")))
	    ;; setup the default texts. 
	    (if comment-start
		;; C/C++ has a blank, elisp not, neither TeX nor FORTRAN nor...
		(let ((blankflag
		       (and (> (length comment-start) 0)
			    (not (string= " " (substring comment-start -1))))))
		  (setq text-before-point
			(concat comment-start (if blankflag " ")
				filename comment-end "\n"))
		  (setq text-after-point
			(concat "\n\n\n" comment-start (if blankflag " ")
				"End of file" comment-end "\n"))
		  ))
	    ;; When using `load' the template file might actually be one
	    ;; of 'xxx.elc', 'xxx.el' or 'xxx'.
	    ;; So just try to load it and catch errors.
	    (condition-case errmsg
		(load template t t) ; ignore if not found
	      ;; warn if wrong code or other error
	      (error (ding)
		     (message "Note: error %s in template %s" errmsg template)))
	    ;; insert if they're still strings
	    (if (stringp text-before-point)
		(insert text-before-point))
	    (if (stringp text-after-point)
		(save-excursion (insert text-after-point)))
	    ;; typo in filename? no problem ...
	    (set-buffer-modified-p nil)
	    ))))

(provide 'suffix-template)

;; EXAMPLE TEMPLATES:
;;;;
;; To make your C/C++ header files automagically include-once-only,
;; put the following to the template file 'h' and make a link to 'H':
;; 1---cut here---1
;; -*- emacs-lisp -*-
;; C/C++ header files template
;;(let ((cpp-filename
;;       (upcase
;;	(concat "_"
;;		;; make sure only word constituents are used in #ifdef's
;;		(mapcar '(lambda (val) (if (eq (char-syntax val) ?w) val ?_))
;;			filename)))))
;;  (setq text-before-point
;;	(concat
;;	 text-before-point
;;	 "#ifndef " cpp-filename "\n"
;;	 "#define " cpp-filename "\t1\n\n"))
;;  (setq text-after-point
;;	(concat "\n\n\n#endif " comment-start cpp-filename comment-end "\n")))
;; 1---cut here---1
;;;;;;
;; Result is:
;;  /* very-random.header.h */
;;  #ifndef _VERY_RANDOM_HEADER_H
;;  #define _VERY_RANDOM_HEADER_H	1
;;
;;  *
;;
;;
;;  #endif /* _VERY_RANDOM_HEADER_H */
;;
;; '*' denotes the location of point.  Note how `text-before-point' is
;; added to and `text-after-point' is overriden in the template.  When
;; loading a .H file, the C-comments will be replaced by C++ comments
;; since .H has c++-mode by default.
;;;;;;;;;;;
;; Here is an example to also set the compile-command when loading the
;; template.  Put it to c, make a link to cc and f and el and...:
;; 2---cut here---2
;;;;  -*- emacs-lisp -*-
;;(cond
;; ((eq major-mode 'c-mode)
;;  (setq text-before-point
;;	(concat text-before-point "#include <stdio.h>\n#include <stdlib.h>\n\n")))
;; ((eq major-mode 'c++-mode)
;;  (setq text-before-point
;;	(concat text-before-point "#include <iostream.h>\n\n")))
;; )
;;;; set compile command
;;(if (or (file-exists-p "makefile") (file-exists-p "Makefile"))
;;    (progn (make-local-variable 'compile-command)
;;	   (setq compile-command (concat "make -k " filename-base))))
;; 2---cut here---2
;;;;;;
;; Result is:
;;// f.cc                   /* f.c */                ; f.el
;;#include <iostream.h>     #include <stdio.h>       *
;;
;;*                         *
;;                                                   ; End of file
;;
;;// End of file            /* End of file */
;;;;;;;
;; And a last example for interpreter files:
;; save to awk, make a link to gawk and add '("\\.gawk$" . awk-mode)
;; to your auto-mode-list: :
;; 3---cut here---3
;;;;  -*- emacs-lisp -*-
;;(setq text-before-point
;;      (concat (if (equal filename-suffix "awk")
;;		  "#!/usr/bin/awk" "#!/usr/local/bin/gawk")
;;	      " -f\n" text-before-point))
;; 3---cut here---3
;;;;;;;
;; Result is:
;;  #!/usr/bin/awk -f   #!/usr/local/bin/gawk -f
;;  # f.awk             # f.gawk
;;  *                   *
;;
;;
;;  # End of file       # End of file
;;;;;;;
;; Happy hacking!
;; End of suffix-template.el
