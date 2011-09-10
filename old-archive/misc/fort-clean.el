;;;   fort-clean.el - code for cleaning and stylizing FORTRAN subprograms
;;;   Copyright (C) 1992 Free Software Foundation, Inc.
;;;
;;;   This program is free software; you can redistribute it and/or modify
;;;   it under the terms of the GNU General Public License as published by
;;;   the Free Software Foundation; either version 1, or (at your option)
;;;   any later version.
;;;
;;;   This program is distributed in the hope that it will be useful,
;;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;   GNU General Public License for more details.
;;;
;;;   You should have received a copy of the GNU General Public License
;;;   along with this program; if not, write to the Free Software
;;;   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;;   Title:  fortran clean
;;;
;;;   Description:  cleans around operators and logicals in a FORTRAN 
;;;                 subprogram or all subprograms in a buffer. Filling and
;;;                 indenting of the resulting FORTRAN code are *not*
;;;                 attempted. The buffer can be toggle-read-only, fort-clean
;;;                 will still work.
;;;
;;;   Created:  Thu May 28 10:59:30 1992
;;;   Author:   Lawrence R. Dodd
;;;             <dodd@mycenae.cchem.berkeley.edu>
;;;             <dodd@roebling.poly.edu>
;;;   
;;;   Bug Reports, Comments, Suggestions, Smart Remarks: 
;;;         dodd@roebling.poly.edu 
;;;         also M-x fortran-clean-submit-report
;;;
;;;   $LastEditDate:     "Sat Nov 14 14:00:27 1992"$
;;;   $Date: 1992/11/14 19:04:31 $
;;;
;;;   $Author: dodd $
;;;   $Id: fort-clean.el,v 3.41 1992/11/14 19:04:31 dodd Exp $
;;;   $Source: /home/dodd/lisp/RCS/fort-clean.el,v $ 
;;;   $Revision: 3.41 $ 

;;;   INSTALLATION: 
;;; 
;;;     o  save as fort-clean.el in the GNU emacs load-path.
;;;  
;;;     o  stick:
;;;
;;; 	 ;; autoload fortran-clean
;;; 
;;;   	 (autoload 'fortran-clean "fort-clean"
;;;   	    "Code for cleaning and stylizing FORTRAN code in the 
;;;      current subprogram." t)
;;; 	 
;;; 	 ;; autoload fortran-clean-buffer
;;; 
;;;   	 (autoload 'fortran-clean-buffer "fort-clean"
;;;   	    "Code for cleaning and stylizing FORTRAN code in all the 
;;;      subprograms of the current buffer." t)
;;;
;;;        inside your .emacs, or more preferably inside fortran-mode-hook:
;;;
;;;        (setq fortran-mode-hook 
;;;           '(lambda () 
;;;                    .
;;;                    . other stuff
;;;                    .
;;; 	         ;; autoload fortran-clean
;;; 
;;;   	         (autoload 'fortran-clean "fort-clean"
;;;   		    "Code for cleaning and stylizing FORTRAN code in the 
;;;              current subprogram." t)
;;; 	 
;;; 	         ;; autoload fortran-clean-buffer
;;; 
;;;   	         (autoload 'fortran-clean-buffer "fort-clean"
;;;   		    "Code for cleaning and stylizing FORTRAN code in all the 
;;;              subprograms of the current buffer." t)))

;;;   USAGE INSTRUCTIONS: 
;;; 
;;;     o type `M-x fortran-clean' to clean the current subprogram
;;;     o type `M-x fortran-clean-buffer' to clean the current all subprograms 
;;;       in the current buffer

;;;   KNOWN BUGS: 
;;; 
;;;     o  will NOT fill properly -- statements may exceed the 72 column 
;;;     afterwards
;;; 
;;;     o  will NOT make 'if (a .eq. b  ) b = d' into 'if (a .eq. b) b = d' 
;;;     because there is no `then' at the end
;;;   
;;;     o  will NOT make (file ='foobar') into (file = 'foobar') because of 
;;;     the fortran-clean-inside-re check
;;;
;;;     o  definition of fortran-subprogram can be fragile and as a result 
;;;     sometimes fortran-clean fails 
;;; 
;;;     o  if more than on fortran-clean-statement replacement is done on one
;;;     line then the last match is not replaced.  WHY?  if
;;;     fortran-clean-statement is run again it cleans up properly. this has
;;;     something to do with where the point is left after a replace-match is
;;;     performed. it is placed at the end of the replacement string so I have
;;;     fortran-statement-replace-regexp step back a single space.

;;;   SEE ALSO:
;;;
;;;     /hallc1.cebaf.gov:/emacs for the most recent version of fortran-mode
;;;     and also for Ralph Finch's fortran-beautifier stored in
;;;     fortran-misc.el

;; LCD Archive Entry:
;; fort-clean|Lawrence R. Dodd|dodd@roebling.poly.edu|
;; Code for cleaning and stylizing fortran code|
;; 92-11-14|3.41|~/misc/fort-clean.el.Z|

(defconst fortran-clean-version "$Revision: 3.41 $"
  "$Id: fort-clean.el,v 3.41 1992/11/14 19:04:31 dodd Exp $")


;;; ** define some useful regular expressions

(defvar fortran-relational-regexp
      "\\.lt\\.\\|\\.le\\.\\|\\.eq\\.\\|\\.ne\\.\\|\\.gt\\.\\|\\.ge\\."
      "Regular expression that will match relational expressions in a fortran
program.")

(defvar fortran-logical-regexp
      "\\.and\\.\\|\\.or\\.\\|\\.neqv\\.\\|\\.xor\\.\\|\\.eqv\\.\\|\\.not\\."
      "Regular expression that will match logical expressions in a fortran
program.")

(defvar fortran-log-rel-regexp
      (concat "\\(" fortran-logical-regexp "\\|" fortran-relational-regexp
              "\\)")
      "Regular expression that will match both relational and logical
expressions in a fortran program.")

(defvar fortran-prefix-char "\\([a-z0-9A-Z()=]\\)"
  "Standard prefix character regular expression.")

(defvar fortran-suffix-char "\\([(a-z0-9A-Z]\\)"
  "Standard suffix character regular expression.")


;;; ** clean all in buffer

(defun fortran-clean-buffer ()
  "Cleans around equal signs, minus signs, plus signs, as well as in and
around if-then-elseif constructs, read statements, and write statements for
all fortran subprograms within buffer. Works only on lines that do not contain
a comment character in the zeroth column."
  (interactive)
  (let (originally_locked)

    ;; if the buffer is set to read only, unlock it and set a flag
    (if buffer-read-only
	(progn (toggle-read-only)
	       (setq originally_locked t))
      (setq originally_locked nil))

    (save-excursion
      ;; go to top of the buffer
      (goto-char (point-min))

      ;; loop through all the subprograms in this buffer. The `while' loop
      ;; uses as its `CONDITION' the first half of the funtion
      ;; end-of-subprogram from fortran.el. The value returned by
      ;; `re-search-forward' is used to abort the iteration. This was done
      ;; because `end-of-subprogram' does not return `nil' if there are no
      ;; more subprograms in the current buffer and the `while' loop would
      ;; iterate endlessly (I know because it happened to me).

      (while (progn 
	       (let ((case-fold-search t))
		 (beginning-of-line 2)
		 (re-search-forward "^[ \t0-9]*end\\b[ \t]*[^ \t=(a-z]" nil 'move)))
	;; another subprogram exists, move into position
	(goto-char (match-beginning 0))
	(forward-line 1)
	;; fire!
	(fortran-clean-subprogram)))

    (message "cleaning buffer...done")

    ;; if the buffer was originally locked, relock it
    (if originally_locked (toggle-read-only))
    )
  )



;;; ** clean all

(defun fortran-clean ()
  "Cleans around equal signs, minus signs, plus signs, as well as in and around
if-then-elseif constructs, read statements, and write statements in a fortran
subprogram. Works only on lines that do not contain a comment character in the
zeroth column."
  (interactive)
  (let (originally_locked)

    ;; if the buffer is set to read only, unlock it and set a flag
    (if buffer-read-only
	(progn (toggle-read-only)
	       (setq originally_locked t))
      (setq originally_locked nil))

    (fortran-clean-subprogram)

    ;; if the buffer was originally locked, relock it
    (if originally_locked (toggle-read-only))
    )
  )



;;; ** clean subprogram

(defun fortran-clean-subprogram ()
  "This function cleans the current subprogram and is either called once by
fortran-clean or called as many times as needed by fortran-clean-buffer."
    (message "cleaning subprogram...=")
    (fortran-clean-equal-sign)
    (message "cleaning subprogram...=,-")
    (fortran-clean-minus-sign)
    (message "cleaning subprogram...=,-,+")
    (fortran-clean-plus-sign)
    (message "cleaning subprogram...=,-,+,if")
    (fortran-clean-statements)
    (message "cleaning subprogram...fixing...")
    (fortran-clean-exponential)
    (fortran-clean-for-clean)
    (message "cleaning subprogram...done."))


;;; ** clean the equal signs

(defun fortran-clean-equal-sign ()
  "Places single space before and after all equal signs in fortran statements
but not in fortran comment lines. Removes tabs surrounding plus signs. Will
not put a space after a equal sign if it is followed by a minus sign. Will not
put space around equal sign unless it is preceded or followed by an
alphanumeric character or parenthesis. Example: '= -a' will result not '= -
a'"

  ;; NOT ENOUGH SPACE
  ;; replace 'a=-b-c' with 'a =-b-c'
  (fortran-statement-replace-regexp "\\([a-z0-9A-Z)]\\)=" "\\1 =")
  ;; replace 'a = -b-c' with 'a = -b-c'
  (fortran-statement-replace-regexp "\\([^=]\\)=\\([(a-z0-9A-Z---]\\)"
				    "\\1= \\2")
  ;; TOO MUCH SPACE
  (fortran-statement-replace-regexp " [ \t]+=" " =")
  (fortran-statement-replace-regexp "\t[ \t]+=" " =")
  (fortran-statement-replace-regexp "= [ \t]+" "= ")
  (fortran-statement-replace-regexp "=\t[ \t]+" "= ")
  )

;;; ** clean the minus signs

(defun fortran-clean-minus-sign ()
  "Places single space before and after all minus signs in fortran statements
but not in fortran comment lines. Removes tabs surrounding minus signs. Will
not put a space after a minus sign if it is preceded by an equal sign or by a
relational or logical expression. Example: '= -a' will result not '= - a'"

  ;; NOT ENOUGH SPACE
  ;; replace 'a=b -c' with 'a=b - c'
  (fortran-statement-replace-regexp
   (concat "\\([^---=.]\\)-" fortran-suffix-char) "\\1- \\2")
  ;; replace 'a=b-c' with 'a=b -c'
  (fortran-statement-replace-regexp
   (concat fortran-prefix-char "-\\([^---]\\)") "\\1 -\\2")

  ;; TOO MUCH SPACE
  (fortran-statement-replace-regexp " [ \t]+-" " -")
  (fortran-statement-replace-regexp "\t[ \t]+-" " -")
  (fortran-statement-replace-regexp "- [ \t]+" "- ")
  (fortran-statement-replace-regexp "-\t[ \t]+" "- ")
  )


;;; ** clean the plus signs

(defun fortran-clean-plus-sign ()
  "Places single space before and after all plus signs in fortran statements
but not in fortran comment lines. Removes tabs surrounding plus signs. Will
not put a space after a plus sign if it is preceded by an equal sign or by a
relational or logical expression. Example: '= +a' will result not '= + a'"

  ;; NOT ENOUGH SPACE
  ;; replace 'a=b +c' with 'a=b + c'
  (fortran-statement-replace-regexp
   (concat "\\([^+=.]\\)\\+" fortran-suffix-char) "\\1\\+ \\2")
  ;; replace 'a=b+c' with 'a=b +c'
  (fortran-statement-replace-regexp
   (concat fortran-prefix-char "\\+\\([^+]\\)") "\\1 \\+\\2")

  ;; TOO MUCH SPACE
  (fortran-statement-replace-regexp " [ \t]+\\+" " \\+")
  (fortran-statement-replace-regexp "\t[ \t]+\\+" " \\+")
  (fortran-statement-replace-regexp "\\+ [ \t]+" "\\+ ")
  (fortran-statement-replace-regexp "\\+\t[ \t]+" "\\+ ")
  )


;;; ** clean the if-then-elseif constructs, read, and write statements

(defun fortran-clean-statements ()
  "Places a single space after an 'if', before a 'then', around relational
and logical expressions, and around 'read' and 'write' statements."

  ;;; replace 'if   ( a)' with 'if (a)'
  (fortran-statement-replace-regexp "\\(^  +if\\)[ \t]*([ \t]*" "\\1 (")
  ;;; replace 'else if   ( a)' with 'else if (a)'
  (fortran-statement-replace-regexp "\\(^  +else[ \t]*if\\)[ \t]*([ \t]*" "\\1 (")
  ;;; replace '(a)  then' with (a) then
  (fortran-statement-replace-regexp ")[ \t]*then$" ") then")
  ;;; replace '(a ) then' with '(a) then'
  (fortran-statement-replace-regexp "[ \t]*) then$" ") then")
  ;;; replace 'a.gt.' with 'a .gt.'
  (fortran-statement-replace-regexp
   (concat "[ \t]*" fortran-log-rel-regexp) " \\1")
  ;;; replace 'a.gt.' with 'a .gt.' (REPEAT) this is a patch! why does this 
  ;;; screw up? KLUDGE!
  (fortran-statement-replace-regexp
   (concat "[ \t]*" fortran-log-rel-regexp) " \\1")
  ;;; replace '.gt.b' with '.gt. b'
  (fortran-statement-replace-regexp
   (concat fortran-log-rel-regexp "[ \t]*") "\\1 ")
  ;;; replace 'read   ( a)' with 'read (a)'
  (fortran-statement-replace-regexp "\\(^  +read\\)[ \t]*([ \t]*" "\\1 (")
  ;;; replace 'write   ( a)' with 'write (a)'
  (fortran-statement-replace-regexp "\\(^  +write\\)[ \t]*([ \t]*" "\\1 (")
    )

;;; ** clean exponential constants

(defun fortran-clean-exponential ()
  "Removes spaces found in exponential constants. For example, we want to
replace occurrences of '1.0D - 9' with '1.0D-9'"

  ;; exponential notation: 
  ;;   OPTIONAL integer(s), OPTIONAL decimal point, OPTIONAL integer(s), 
  ;;   one letter (e,d, or q), OPTIONAL sign (+ or -), integer(s)
  (fortran-statement-replace-regexp
   "\\([0-9=+---/*(, ]\\.?[0-9]*[edqEDQ]\\) \\([+---]\\) \\([0-9]\\)"
   "\\1\\2\\3")
  )

;;; ** clean fortran-clean

(defun fortran-clean-for-clean ()
  "Cleans up after fortran-clean. Corrects some substitutions made by the
previous routines that are not desired and that are hard to avoid."

  ;; CORRECTION SEARCH-AND-REPLACES

  ;; replace 'a   = -  b -c' with 'a = -b - c'
  (fortran-statement-replace-regexp
   (concat "\\([a-z0-9A-Z) ]=\\) \\([+---]\\) " fortran-suffix-char)
   "\\1 \\2\\3")

  ;; replace '( - a - c)' with '( -a - c)'
  (fortran-statement-replace-regexp
   (concat "( \\([+---]\\) " fortran-suffix-char)
   "( \\1\\2")

  ;; replace ',- a' with ', -a'
  (fortran-statement-replace-regexp
   (concat ",[ \t]*\\([+---]\\) " fortran-suffix-char)
   ", \\1\\2")

  ;; replace '.and. - a .lt. - c' with '.and. -a .lt. -c'
  (fortran-statement-replace-regexp
   (concat fortran-log-rel-regexp " \\([+---]\\) " fortran-suffix-char)
   "\\1 \\2\\3")

  )


;;; ** general search and replace routine for non-comment fortran statements

(defun fortran-statement-replace-regexp (regstring replacement)
  "Replaces REGSTRING with REPLACEMENT for in fortran statements, excluding
comment lines and anything between (single or double) quote marks, within a
fortran subprogram. We define a fortran statement as anything without a \"C\"
in the zeroth column. In this way, comments, FORMAT statements, filenames, and
character strings should be uneffected. Also will not search and replace
following \"!\" comment characters."
  (interactive
   "sReplace regexp: \nsReplace regexp %s in fortran statement with: \n")
;;; 
;;; logic: 
;;;   o do a regular expression forward search for 'regstring'
;;;   o when a match is found go to the beginning of the line 
;;;   o if there is not a comment character at the beginning of the line then 
;;;     search and replace in that line 'regstring' with 'replacement' 
;;;      oo search in the current line up to the first ! that is not in 
;;;         quotes or the end of line if there are no non-quoted !'s
;;;      oo only replace if string is not between quotes
;;;      oo after each replacement move back one character to avoid endless 
;;;         looping
;;;   o go forward one line
;;; 
  (save-excursion
    (beginning-of-fortran-subprogram)
    (let
	;; VARLIST

	((eop
	  (save-excursion (end-of-fortran-subprogram) (end-of-line) (point))))

      ;; BODY

      ;; search for regstring from the current point to the end of the 
      ;; subprogram
      (while (and (< (point) eop) (re-search-forward regstring eop t))
        (beginning-of-line) ; regstring found - go to beginning of line
        ;; if there is a blank or tab in the first column then search and 
	;; replace the rest of the current line
        (if (not (looking-at "[cC*]")) ; not comment?
            (progn
              (let*
		  ;; VARLIST

		  ( 
		   ;; end of line
		   (eol (save-excursion (end-of-line) (point)))
		   
		   ;; final point of search/replace
		   (finalpt
		    (save-excursion
		      
		      ;; go to beginning of line
		      (beginning-of-line)
		      
		      ;; search forward from bol to eol for a "!" that does
		      ;; not reside in a set of single or double quotes. if a
		      ;; "!"  is *not* found, then search-forward will leave
		      ;; the point at eol.
		      
		      (while (and (search-forward "!" eol 0)
				  (or (fortran-clean-inside-re "'" "'")
				      (fortran-clean-inside-re "\"" "\""))
				  ))
		      
		      ;; get value of point
		      (point)
		      
		      ) ; end excursion 
		    ) ; finalpt defined
		   
		   ) ; end of VARLIST
		
		;; BODY

                (while (re-search-forward regstring finalpt t)

		  ;; if we are not inside a single or double quote then 
		  ;; replace `regstring' with `replacement' 
		  ;; (the `unwind-protect' contruct from section in Elisp 
		  ;; Reference Manual on saving match data)

		  (if (let ((data (match-data)))
			(unwind-protect
			    (not (or (fortran-clean-inside-re "'" "'")
				     (fortran-clean-inside-re "\"" "\"")
				     (first-non-continuation-char regstring)))
			  (store-match-data data)))
		      (progn (replace-match replacement) (backward-char))
		    ) ; end of if

		  ) ; end of while
		) ; end of inner let
	      
	      ) ; end of if-then FORM
	  ) ; end of if
	(forward-line 1) ; go to beginning of next line
	) ; end while
      )   ; end of outer let
    )     ; end save-excusion
  )

(defun first-non-continuation-char (regstring)
  "Returns t if REGSTRING is the first string in a continuation fortran line."
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "^     [^ ]" regstring))))


;; this page is provided for reports.
;; adopted from Barry A. Warsaw's c++-mode.el

(defvar fortran-mailer 'mail
  "*Mail package to use to generate report mail buffer.")

(defconst fortran-clean-help-address "dodd@roebling.poly.edu"
  "Address accepting submission of reports.")

(defun fortran-clean-submit-report ()
  "Submit via mail a report using the mailer in fortran-mailer."
  (interactive)
  (funcall fortran-mailer)
  (insert fortran-clean-help-address)
  (if (re-search-forward "^subject:[ \t]+" (point-max) 'move)
      (insert "Report on fortran-clean.el " fortran-clean-version))
  (if (not (re-search-forward mail-header-separator (point-max) 'move))
      (progn (goto-char (point-max))
	     (insert "\n" mail-header-separator "\n")
	     (goto-char (point-max)))
    (forward-line 1))
  (set-mark (point))			;user should see mark change
  (insert "\n\n")
  (insert (emacs-version) "\n")
  (insert "fort-clean.el " fortran-clean-version)
  (exchange-point-and-mark))


;;; Based on code from Ralph Finch's fortran-beautifier (rfinch@water.ca.gov)

(defun fortran-clean-inside-re (start-re end-re)
  "Returns t if inside a starting regexp and an ending regexp
on the same line."
  (and (save-excursion
	 (re-search-backward start-re
			     (save-excursion (beginning-of-line) (point)) t))
       (save-excursion
	 (re-search-forward end-re
			    (save-excursion (end-of-line) (point)) t))))
