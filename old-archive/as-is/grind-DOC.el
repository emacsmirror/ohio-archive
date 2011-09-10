;;; Grind GNU Emacs DOC file into troff input.
;;; Copyright (C) 1987 Kyle E. Jones

;;; This file may be redistributed provided the above copyright
;;; notice appears on all copies and that the further free redistribution
;;; of this file is not in any way restricted by those who
;;; redistribute it.

;;; This software is distributed 'as is', without warranties of any kind.

;;; This file is not part of GNU Emacs.

;;; The document that is the output from the (grind-DOC) function is
;;; part of GNU Emacs.


(defvar grind-DOC-macro-package 'me
  "*Should be bound to a symbol indicating what troff macro package
should be used to format the DOC file.  If this variable is set to nil
only basic troff commands will be used.  Since the author isn't a troff
expert the document will look terrible without macros.  Currently only
me macros are supported.")

(defvar grind-DOC-point-size 10 "*The point size for the formatted DOC file.")

(defvar grind-DOC-double-sided t
  "*Should be non-nil if the formatted DOC will be printed on both sides
of the pages.  This will cause the appropriate changes to the headers and
footers.")

(defconst grind-DOC-known-macros '(me)
  "A list of symbols representing the troff macro packages that
(grind-DOC) knows how to use.")

(defconst grind-DOC-title-tag "The Pleasure Principle")

V(defun abs (n) (cond ((< n 0) (- n)) (t n)))

(defun grind-DOC () (interactive)
  "Reads the etc/DOC-xx.xx.x file into a buffer and converts it to a form
suitable as troff or nroff input."
  ;
  ; Make sure we can deal with the macro package and the point size.
  ;
  (cond
   ((not (symbolp grind-DOC-macro-package))
    (error "grind-DOC-macro-package must be a symbol"))
  ((and grind-DOC-macro-package
	(not (memq grind-DOC-macro-package grind-DOC-known-macros)))
   (error "I don't know how to use %s %s"
	  (symbol-name grind-DOC-macro-package) "macros"))
  ((not (numberp grind-DOC-point-size))
   (error "grind-DOC-point-size must be a number"))
  ((not (memq grind-DOC-point-size '(6 7 8 9 10 11 12 14 16 18 20 24 28 36)))
   (error "point size must be 6, 7, 8, 9, 10, 11, 12, 14, 16, 18, 20, 24, 28, or 36")))
  ;
  ; Select the DOC file.
  ;
  (find-file (expand-file-name (concat "DOC-" emacs-version) exec-directory))
  (setq buffer-read-only nil)
  (auto-save-mode 0)
  (set-visited-file-name (concat (buffer-file-name) "."
				 (cond
				  ((null grind-DOC-macro-package) "t")
				  (t (symbol-name grind-DOC-macro-package)))))
  (delete-other-windows)
  ;
  ; Save-excursion just in case the DOC file was already selected.
  ;
  (save-excursion
    (let (case-fold-search mode-line-format varstart-point bufstring name odot)
      ;
      ; The first thing we must do is convert the \[COMMAND] sequences
      ; into the keys that the COMMANDs are bound to.
      ;
      (setq mode-line-format
	    "                     Grinding the DOC file... be patient.")
      (sit-for 0)
      (setq bufstring (substitute-command-keys (buffer-string)))
      (erase-buffer)
      (insert bufstring)
      ;
      ; Here we make each docstring begin and end with C-_ for
      ; easier manipulation.  This is undone later.
      ;
      (goto-char (1+ (point-min)))
      (replace-string "\C-_" "\C-_\C-_" nil)
      (goto-char (point-max))
      (insert "\C-_")
      ;
      ; Sort the docstrings.  This implicitly separates function
      ; documentation from the variable documentation.
      ;
      (sort-regexp-fields nil "\C-_\\([FV].*\\)[^\C-_]*\C-_" "\\1"
			  (point-min) (point-max))
      ;
      ; now add the indentation commands and put ( ...) around the functions
      ;
      (save-restriction
	(goto-char (point-min))
	(search-forward "\C-_V" (point-max) nil 1)
	(backward-char 2)
	(narrow-to-region (point-min) (dot))
	(goto-char (point-min))
	(while (search-forward "\C-_F" (point-max) t 1)
	  (delete-char -2)
	  (insert ".ul\n(")
	  (end-of-line 1)
	  (insert "...)\n.in +5")
	  (search-forward "\C-_" (point-max) nil 1)
	  (delete-char -1)
	  (insert "\n.in -5\n"))
	(insert ".bp\n"))
      (while (search-forward "\C-_V" (point-max) t 1)
	(delete-char -2)
	(insert ".ul\n")
	(end-of-line 1)
	(insert "\n.in +5")
	(search-forward "\C-_" (point-max) nil 1)
	(delete-char -1)
	(insert "\n.in -5\n"))
      ;
      ; try to make those parameters that are in all-caps look better
      ;
      (goto-char (point-min))
      (replace-regexp "[A-Z][A-Z]+" "\\\\fB\\\\s-2\\&\\\\s+2\\\\fR" nil)
      ;
      ; Handle point size.  Vertical spacing should be 1.2 * point size.
      ; Since Emacs Lisp has no floating point we multiply by 12,
      ; add 5 (for rounding). and divide by 10.
      ;
      (goto-char (point-min))
      (insert ".ps " (int-to-string grind-DOC-point-size) "\n")
      (insert ".vs "
	      (int-to-string (/ (+ (* grind-DOC-point-size 12) 5) 10)) "\n")
      ;
      ; Insert a tag so that the macro specific functions know where to
      ; put the title page.
      ;
      (insert grind-DOC-title-tag "\n")
      ;
      ; Insert the GNU Emacs copyright notice.
      ;
      (insert
       ".bp\n.ce 2\n.fi\n.ul 1000\n\\(co " (substring emacs-build-time 20)
       " Free Software Foundation, Inc.

This document is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.\n.ul 0\n.pn 1\n")
      ;
      ; header for function documentation
      ;
      (insert ".bp\n")
      (insert ".nf\n.ce\n\\fBGNU Emacs Functions\\fR\n.sp\n")
      ;
      ; add header and info for variable documentation
      ;
      (search-forward ".bp\n" (point-max))
      (insert ".ce\n\\fBGNU Emacs Variables\\fR\n"
	      ".sp\n.fi\nVariables whose documentation begins with an\n"
	      "asterisk `*' are user definable options.  These variables are\n"
	      "used to customize Emacs.  Other variables are generally of\n"
	      "interest only to Emacs Lisp programmers.\n.nf\n.sp\n")
      ;
      ; Now do macro-package specific things
      ;
      (cond
       ((eq grind-DOC-macro-package 'me) (grind-DOC-for-me))
       (t (grind-DOC-no-macros)))
      (message "Grinding completed.  Behold!"))))

(defun grind-DOC-for-me ()
  "Adds -me macro commands to the formatted DOC.  Should only be called
from (grind-DOC).  In particular -me macros make possible the building
of an index."
  (random t)
  ;
  ; Title page
  ;
  (goto-char (point-min))
  (search-forward grind-DOC-title-tag (point-max))
  (delete-char 1)				; delete the newline
  (delete-char (- (length grind-DOC-title-tag))); delete the tag
  (insert ".tp\n")
  (insert ".ps 28\n.vs 34\n.sv 2.5i\n.ce\nGNU EMACS Lisp Reference\n.sp\n"
	  ".ps\n.vs\n"
	  ".ce 5\n\\s-2(gouged with a blunt instrument from "
	  (concat "DOC-" emacs-version)
	  ")\\s+2\n.sp\n"
	  "\\s-1writing by\\s+1\n"
	  ".bi \"Richard M. Stallman\"\n.sp\n"
	  "\\s-1gouging by\\s+1\n"
	  ".bi \"Kyle E. Jones\"\n")

  ;
  ; Origins
  ;
  (search-forward "\n.pn 1" (point-max))
  (beginning-of-line)
  (insert ".sp\nThis document is a reformatted version of the GNU Emacs "
	  emacs-version " online documentation.\nThe program that gouges this document from the etc/DOC-" emacs-version " file was written by\n"
	  (nth (% (abs (random)) 6)
	       '("a mild mannered youth"
		 "a singularly handsome man"
		 "a moody and arrogant surrealist"
		 "a 60's rabblerouser"
		 "a lout"
		 "several caribou, oddly enough, all"))
	  " named Kyle Jones, currently living in exile.\n")
  ;
  ; Headers and footers
  ;
  (forward-line 1)
  (if grind-DOC-double-sided
      (insert ".oh 'Version " emacs-version "'GNU EMACS Lisp Reference'%'\n"
	      ".eh '%'GNU EMACS Lisp Reference'Version " emacs-version "'\n")
    (insert ".he 'GNU EMACS Lisp Reference''Version " emacs-version "'\n"
	    ".fo ''- % -''\n"))
  ;
  ; Use bold-italics on variable and function names and wrap docstrings
  ; in a keep.
  ;
  (goto-char (point-min))
  (let (name odot)
    (while (search-forward "\n.in +5\n" (point-max) t 1)
      (search-backward ".ul\n" (point-min))
      (delete-char 4)
      (insert ".(b\n.bi \"")
      (setq odot (dot))
      (if (looking-at "(") (forward-char 1))
      (setq name (symbol-name (read (current-buffer))))
      (goto-char odot)
      (end-of-line 1)
      (insert "\"")
      (search-forward "\n.in -5\n" (point-max))
      (insert ".)b\n.(x\n\t" name "\n.)x\n")))

  ;
  ; Make index entries where the variable and function documentation
  ; sections are.
  ;
  (goto-char (point-min))
  (search-forward "\n.bp\n" (point-max) nil 2)
  (insert ".(x\nFunction Documentation\n.)x\n")
  (search-forward "\\fBGNU Emacs" (point-max) nil 2)
  (beginning-of-line)
  (insert ".(x\nVariable Documentation\n.)x\n")

  ;
  ; Force out index.
  ;
  (goto-char (point-max))
  (insert ".he ''''\n.eh ''''\n.oh ''''\n.fo ''''\n"
	  (cond (grind-DOC-double-sided ".bp\n.bp\n")
		(t ".bp\n"))
	  ".ce\n\\fB\\(if INDEX \\(if\\fR\n.xp\n.bp\n"))

(defun grind-DOC-no-macros ()
  "Add title page to formatted DOC using straight troff commands.  Should
only be called from (grind-DOC)."
  ;
  ; Title page
  ;
  (goto-char (point-min))
  (search-forward grind-DOC-title-tag (point-max))
  (delete-char 1)				; delete the newline
  (delete-char (- (length grind-DOC-title-tag))); delete the tag
  (insert ".ps 28\n.vs 34\n.sv 2.5i\n.ce\nGNU EMACS Lisp Reference\n.sp\n"
	  ".ps\n.vs\n"
	  ".ce 5\n\\s-2(gouged with a blunt instrument from "
	  (concat "DOC-" emacs-version)
	  ")\\s+2\n.sp\n"
	  "\\s-1writing by\\s+1\n"
	  "\\fBRichard M. Stallman\\fR\n.sp\n"
	  "\\s-1gouging by\\s+1\n"
	  "\\fBKyle E. Jones\\fR\n"))
