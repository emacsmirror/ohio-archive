;; @(#) tinycom.el -- smart comment setting utility

;; @(#) $Id: tinycom.el,v 1.21 1995/09/12 18:04:55 jaalto Exp jaalto $
;; @(#) $Keywords: language, comment handling $
;; $KnownCompatibility: 18.57 , 19.28 $
;; $outlineRegexp: '@+' $
;; $bookMarkRegexp: '[-~+=*#.,'`^]+ &+\\([^ \t]+\\) [-~+=*#.,'`^]+' $
;; $PackageInstallRe: '^[ \t]*;;+[*] ' $

;; This file is *NOT* part of GNU emacs

;;{{{ Id

;; Copyright (C) 1994,1995 Jari Aalto
;; Author:       Jari Aalto <jari.aalto@ntc.nokia.com>
;; Maintainer:   Jari Aalto <jari.aalto@ntc.nokia.com>
;; Created:      Sep 29 1994, on a rainy day of fall.
;;
;; To get information on this program use ident(1) or do M-x tic-version
;; Look at the code with folding.el , tinyfold.el , tinybm.el


;; LCD Archive Entry:
;; tinycom|Jari Aalto|jari.aalto@ntc.nokia.com|
;; Smart comment setting utility, better than std emacs M-; |
;; 12-Sep-1995|1.21|~/misc/tinycom.el.Z|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;}}}
;;{{{ Briefly

;; ........................................................ &t-briefly ...
;; o Replaces M-; commenting function. Suitable for any major-mode
;; o Determines comment variables on the fly, no matter where you
;;   are or what mode you are using _ONLY_ when comments vars are _empty_
;; o There is no-list that tells not to touch this mode's commenting.
;;   This is for modes that has comment-end. TIC can't handle those.
;; o Repetitive M-; converts single comments into 'bigger classes'
;; o Smartly places NEW comment on empty line by looking at previous
;;   comment.
;; o If there is CODE + COMMENT, the comment will be adjusted to standard
;;   position even if comment-column is crowded. [this is very nice feature]
;; o You can set col position for those comments that are not allowed to move
;;   This handy for long lines or special comments.
;; o If there are multiple 'comments' , like '$#var # comment' in perl-mode,
;;   it picks up the latest # char and treats it as a comment.
;;   This is something standard emacs commenting cannot do!

;;}}}
;;{{{ Installation

;; ........................................................ &t-install ...
;; - Put this file on your Emacs-Lisp load path, add following into your
;;   ~/.emacs startup file
;;
;;   normal load
;;     (require		  'tinycom)
;;     (tic-install-keys  "g")
;;     (setq-default	  comment-column 48)
;;
;;   or use atoload, your emacs starts faster
;;     (global-set-key	  "\M-;"	  'tic-indent-for-comment)
;;     (autoload	  'tic-indent-for-comment  "tinycom" "" t)

;;}}}

;;{{{ Documentation

;; PREFACE
;; ..................................................... &t-commentary ...
;; - In 18 Oct 94 Era Eriksson asked for help in gnu.emacs.help.
;;   He hated 'modes' because they redefined his tab key strangely.
;;   What he wanted was tab 8 in _every possible case_. *huh*,
;;   I thought when reading his post: "..but if the mode screws with my
;;   tab key, I don't want it.". Whatever the language, he was going
;;   to use _hard_ tabs...we'll I don't think he had met Lisp yet :-/
;; - Anyway, he also wanted his comments always to be positioned in
;;   column 56 (tab #7), And I started thinking how could he add
;;   comments if the mode, which _knows_ comment syntax, wasn't
;;   turned on? (He always programmed in fundamental-mode, what a
;;   cool dude...)
;; - As a result this packet was eventually born. It really became
;;   part of my daily life use! I couldn't live do without it anymore.
;; - The original .el to Era was posted under name 'general-comment.el'.

;; WHAT's THIS ALL ABOUT
;; ========================================
;; - Let see...I'm in C/C++ mode, and I want to switch to better mode
;;   before I start adjusting my comments nicely. But wait,
;;   the new mode doesn't know about C++-comments!
;; - Or I'm editing my ~/.Xdefauls, there is no mode for it, no-one to
;;   know the comment syntax. Boom :-(
;; - Now it's time for giving this packet a run.
;;   It hooks itself directly to "\M-;" replacing any previous function.
;; - It defines comment syntax and position on the fly when it can identify
;;   the file name. If the file isn't known then it passes control
;;   to 'mode' to handle the commenting-> very handy in temporary buffers
;;   that has no filename, eg. *scratch* buffer.
;;
;; LIMITATIONS
;; - This isn't designed for modes that have comment-end, you get
;;   only ready '/* */' string eg. in C-mode ie. ready comment string,
;;   the comment classes don't apply.
;;
;; RESERVED prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tic-" in front of
;;   every function & variable. It stands for '(ti)ny (c)omment.
;; - variable names contain letters 'tic-:', excecpt version/hook/mode vars.
;;
;; Examples
;; - At the end of file there is my favorite 'general editing mode' :-)
;;   setup. I use it for perl, shells, awk, C++ [sometimes]




;;}}}
;;{{{ History

;; ........................................................ &t-history ...
;; [jari]   = jari.aalto@ntc.nokia.com(work), ssjaaa@uta.fi(University)
;;
;; Sep 9	1995	[jari]		19.28	v1.22		Release_11
;; - One debugging garbage commented.
;;
;; Sep 9	1995	[jari]		19.28	v1.21		NotReleased
;; - Small bug corrected which added extra comment at the end of line.
;;   Also corrects comment class disablement.
;;
;; Sep 8	1995	[jari]		19.28	v1.20		Release_10
;; - ** warning: all variable names have been renamed! "-v-" --> '-:'
;; - Thre was bug in positioning the cursor if one were programming in
;;   mode that used comment-char for other purposes. Eg in csh:
;;
;;	if ( $#argv == 0 ) goto USAGE	#
;;	      *
;;
;;   The comment were added ok, but the cursor stayed at the point where
;;   use was when M-; was pressed. Corrected so, that the point is after
;;   comment.
;; - Code layout changed a little. Added load-hook, added ###autoload.
;; - Also corrrected very old "misfortune" feature:
;;
;;   If you have code where the COMMENT-START is used for other purposes,
;;   like in csh or perl
;;
;;
;;       if ( $#argv == 0 ) goto USAGE
;;
;;   Don't be suprised if the line indent's like this after you hit M-;
;;
;;       if ( $				#argv == 0 ) goto USAGE
;;
;;   It's just natural that program thinks that '#' is valid comment
;;   character in a line and adjusts position according to comment-column.
;;   The solution is firt add comment by hand, and the adjusting it with M-;
;;   and last comment-start will always be used for referencensing.
;;
;; - Now there is new variable that tells "This is not valid comment
;;   position". See tic-:not-comment-re
;;
;; Mar 2	1995	[jari]		19.28	v1.20		NotReleased
;; - Internal error if comment-col/start was nil and tic-adj-comment
;;   called. fixed.
;;
;; Mar 1	1995	[jari]		19.28	v1.19		NotReleased
;; - If the comment-start weren't recognized in any way
;;   the program hung up, this was due to bad  entry test in
;;   tic-adj-comment. Fixed, now indents allthough comment not set.
;;
;; Feb 28	1995	[jari]		19.28	v1.18		Release_8
;; - Small error in tic-install-keys. Example updated.
;;
;; Feb 27	1995	[jari]		19.28	v1.17		Release_7
;; - Corrected LCD entry. Now supports Autoload.
;;
;; Jan 17	1995	[jari]		19.28	v1.16		NotReleased
;; - Converting classes on it's line: now looks back until prev com isn't
;;   in comment-column of position isn't within line CODE len
;; - Added initializing comment variables, so that user's mistakes
;;   don't signal error while calling directly indent-for-comment {bug}
;; - Now I let the major mode do the code level indent for
;;   full comment. See new variable tic-v-tab-call-no-alist
;;
;; Jan 9	1995	[jari]		19.28	v1.15		Release_6
;; - Made 18.57 compatible again, uses it's own skip-syntax func
;; - Lots of improvements for comment positioning. Bugs found
;; - new conf variable tic-v-def-com-pos, installation changed
;;
;; Dec 30	1994	[jari]		19.28   v1.12   	Release_5
;; - Fine tuning and total rewrite of tic-set-com. Found bugs from it
;; - new: 'extra' handling for lines with CODE + COMMENT. New funcs/vars
;;
;; Dec 28	1994	[jari]		19.28   v1.9    	Release_4
;; - Lots of bugs found from comment class function
;; - Corrected comment class func + added various new features
;; - Now uses own indent func and doesn't rely on indent-for-comment.
;; - added 'brief' commentary.
;;
;; Nov 27	1994	[jari]		18.57	v1.5		Release_3
;; - Polished few edges, included right GPL licence, no new features.
;;
;; Nov. 15	1994 	[jari]		18.57	v1.2-1.4	Release_2
;; - Now has smart code indent finder, and supports 'comment classes'
;; - Looks real good... :-)
;;
;; Sep. 29	1994 	[jari]		18.57	v1.1		Release_1
;; - First implementation
;; - Just A quick hack to get body ready. Seems working well.


;; To do:
;; ========================================
;; - Some of these functions are currently transferred to general
;;   tinylibXXX.el lisp libraries and when the libraries are commonly
;;   available, the overlapping functions are removed from this file.
;;
;; - Change the preceddence so, that newest code is loaded, if .el
;;   is never that .el.gz, then use .el, Now is simpli always uses .gz
;;   if one exists


;;}}}

;;; Code:

;;{{{ setup: user configurable

;;; ......................................................... &require ...

;;; .......................................................... &v-bind ...
;;; - It's better to have these behind a function, so that
;;;   user can call them within hooks, if the binding gets lost
;;;   for some reason.


;; install keys only if user wants them
(defun tic-set-keys (key-func)
  "Installs keys."
  (funcall key-func  "\M-;" 'tic-indent-for-comment)
  )

(defun tic-install-keys (env)
  "Changes ECS-; keybinding  to tic-indent-for-comment.
ENV parameter has values 'local' 'global'"
  (interactive "slocal, global, both [lgb]: ")
  (cond
   ((string-equal env "l")
    (tic-set-keys 'local-set-key))
   ((string-equal env "g")
    (tic-set-keys 'global-set-key))
   ((string-equal env "b")
    (tic-set-keys 'local-set-key)
    (tic-set-keys 'global-set-key))
   ))


;;; ......................................................... &v-hooks ...

(defvar tinycom-load-hook nil
  "*Hook run when package has been loaded.")

;;; ... private variables ................................. &v-private ...

;;; ... user configurable .................................. &v-config ...

(defvar tic-:comment-file-alist
  '(
    (".c"     . ("/*"    "*/"48))
    (".h"     . ("//"    "" 	48))	;our company used .h for C++
    (".hh"    . ("//"    "" 	48))
    (".cc"    . ("//" 	 ""	48))
    (".sh"    . ("#"	 "" 	48))
    (".pas"   . ("{" 	 "}"	48))
    (".pl"    . ("#"	 "" 	48))	;perl library, not executable
    (".pls"   . ("#"	 "" 	48))	;perl #!/perl exe, my convention
    (".pm"    . ("#"	 "" 	48))	;perl ..
    (".el"    . (";"	 ""	40))
    (".awk"   . ("#"	 ""	40))
    (".tex"   . ("%"	 ""	48))
    (".texi"  . ("%"	 ""	48))
    (".html"  . ("<!---" "-->" 	48))	;strange syntax !
    )
  "*Comments according to file ext. The format is as follows:
   \(EXT . \(COM_START COM_END DEF_COLUMN\)\)
The values are only used if they are not yet defined.")

(defvar tic-:not-comment-re ".*[$]\\(#\\)[a-zA-Z]"
  "*When searching for comment position the position found will be
rejected, if comment subexpression 1 match's position is the same as
initially found comment position. The test will be done with looking-at at
the beginnning of line.

      $#variable_in_csh
0123456789 123456789 12345       ;columns
       *                         ;found comment pos, reject it.
")

(defvar tic-:comment-re-file-alist
  '(
    ("\\.ema"	   ";" ""  48)            ;.emacs , .emacs.dired ...
    ("\/\\.[Xx]"   "!" ""  1)		;.Xdefauls, .xinirc
    ("\\csh"	   "#" ""  48)		;like .cshrc or myScript.csh
    )
  "*Comments according to regexp. This is used only if normal ext search fails
NOTE 1: Remember that this regexp is compared against buffer-name, which
        contains user's path in front of the filename.
NOTE 2: The regexps are scanned in order given. So make most restrictive
        the first ones.")


(defvar tic-:tab-call-no-alist
  '((fundamental-mode . 1)
    (text-mode 	      . 1)
    )
  "*List of modes which enables using TIC's own indent-for-code algorithm

Most programming modes supply function that knows how to indent the
statement.  But in some cases mode does not supply it's own indent
function placed on variable indent-line-function, which is called by
tab key.")


(defvar tic-:adj-no-alist
  '((lisp-interaction-mode . 1)
    (lisp-mode 		   . 1)
    (emacs-lisp-mode 	   . 1)
    (c-mode		   . 1)    ;; we use use // in c++, not /* */
    (pascal-mode	   . 1)
    )
  "*List of modes which disables converting to bigger comment class, when
called from tic-indent-for-comment. It's assoc list format:
   \(major-mode . 1\)
Where the dot is required and any value unused.

The function tic-adj-comment isn't suitable for all possible
modes. Eg. lisp has it's own idea about placing comments according
to comment used.
      ;     --> comment column
      ;;    --> right to code level
      ;;;+  --> left margin.

This defines list of mode names where tic-adj-comment is suppressed
and the normal indent-for-comment is used.")


(defvar tic-:comment-notify nil
  "*If this flag is non-nil the user will be notified every time
When the comment syntax wasn't found according to file name.
The message is _not_ displayed when buffer-name contains '*'.

You may want to set this to 't for awhile, so that you can add all
those files that are missing from the list. When you're satisfied,
you can turn off the warning.")

(defvar tic-:def-com-pos 'code
  "*This variable determines the default comment position for totally
empty lines. Possible choices are:
  'code            ,code level indent [usually as previous code line]
  'cpos            ,normal comment-column

Note that 'cpos doesn't always put comment where you would expect, because
it looks back until it finds code. In spite of that, it normally works
well _inside_ functions")


(defvar tic-:comment-extra-arg 1
  "*See documentation of tic-:comment-extra")

(defvar tic-:comment-extra-stop 63	;TAB position 64
  "*See documentation of tic-:comment-extra.
The comment movement is not done if current-column > this variable.")


(defvar tic-:comment-extra 'tab
  "*This affects function tic-set-com. Let's see an example:

    abcd abcd abcd abcd abcd abcd[x] abcd abcd # COMMENT

You have line, where line exeeds the comment column[x] and your
comment is at the end. This variable determines how such
line is handled when you now hit M-;

Current choices are:
    'tab      Insert tab between code and comment, so that they get
              separated. Any previous whitespace is deleted.
    'spc      Same, but insert space instead. The number or spaces inserted
              is told in variable  tic-:comment-extra-arg

None of these actions are carried out if the comment was placed in
column tic-:comment-extra-stop +1 or further. Such comment is
left untouched, because adjusting may push it out of the window edge.")

;;}}}
;;{{{ setup: -- version notice

;;; ... version info ...................................... &v-version ...

(defconst tic-version
  "$Revision: 1.21 $"
  "Latest version number.")


(defconst tic-version-id
  "$Id: tinycom.el,v 1.21 1995/09/12 18:04:55 jaalto Exp jaalto $"
  "Latest modification time and version number.")

(defconst tic-version-doc
  "tinycom.el -- smart comment setting utility

First created: Sep 29 1994
Author       : Jari Aalto <jari.aalto@ntc.nokia.com
Maintainer   : Jari Aalto <jari.aalto@ntc.nokia.com

Suggestions and enchancements welcome!"
  "Brief version and contact info")


;;; ----------------------------------------------------------------------
(defun tic-version ()
  "tinycom.el information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tic-version-doc
       "\n\ncurrent version:\n" tic-version-id)
      (pop-to-buffer  ob)
    ))

;;}}}

;;; ########################################################### &Funcs ###


;;{{{ code: misc




;;; ----------------------------------------------------------------------
;;;
(defun tic-get-syntax-re (direction syntax)
  "Emulate some features of 19.xx emacs for 18.xx"
  (if (eq direction 'back)
      (cond
       ((equal syntax " ") "[^ \t]")
       ((equal syntax "^ ") "[ \t]")
       ;;   others are not used in this prg.
       (t nil))
    ))

;;; ----------------------------------------------------------------------
;;;
(defun tic-skip-syntax-backward (arg &optional pos)
  "Offer compatibility for 18.xx, because it does not have skip syntax.
This is only a shadow of the original. Very limited."
  (let (re)
    (if (boundp 'skip-syntax-backward)
	(skip-syntax-backward arg pos)
      (setq re (tic-get-syntax-re 'back arg))
      (if (re-search-backward re pos t) (forward-char 1))
      )
    ))

;;; ----------------------------------------------------------------------
;;;
(defun tic-find-prev-com-col (com-start &optional not-this-col CF)
  "Looks upward to find previous comment placement column.
if NOT-THIS-COL is given it scans backward until different column
is found. variable CF says that comment searched must reside further in
in the line than this column.

Returns:
  nil	,unable to find previous comment
  col
"
  ;; (interactive "sComment start: ")
  (let* (
	 (len	(length re))
	 (re	com-start)
	 (loop	t)
	 ret found
	 )
    (save-excursion

      (while loop
	(beginning-of-line)
	(setq ret nil found nil)
	(if (null (setq found (re-search-backward re nil t))) nil
	  (setq ret (current-column)))

	(setq loop nil)				;default

	(if (or (null found)
		(null not-this-col))
	    nil
	  (if (not (= ret not-this-col))
	      (setq loop nil)			;this will do !
	    (setq loop t found nil)		;keep searching
	    ))

	(if (or (null found)
		(null CF))
	    nil
	  (if (> ret CF)			;this IS suitable !
	      (setq loop nil)
	    (setq loop t found nil)))			;keep going
	))
    ret))

;;; ----------------------------------------------------------------------
;;;
(defun tic-find-com-col ()
  "Looks Current line to find comment-start.

Returns:
   nil
   nbr	,column.
"
  (let (
	(no-com   (or tic-:not-comment-re   "dummy"))
	(max	  (save-excursion (end-of-line) (point)))
	(clen	  (length comment-start))
	(re	  comment-start)
	ret found
	)
    (save-excursion
      (beginning-of-line)

      ;;  is there restrictions ?
      (setq not-pos (if (and (looking-at no-com)
			     (match-beginning 1))
			(match-beginning 1)
		      nil))

      (while (re-search-forward re  max t) ; find last comment
	(setq found t))

      (if (null found)
	  nil
	(backward-char clen)
	(setq cp (point))
	(if (eq cp not-pos)
	    nil				;cannot use this
	  (setq ret (current-column))))
      ret
      )))

;;}}}
;;{{{ positions

;;; ----------------------------------------------------------------------
;;;
(defun tic-check-line (mode &optional arg)
  "Few commands to use to determine line data.

Returns:
  depends heavily on the MODE
"
  (let* (ret
	 re re2
	 bp ep
	 p p2
	 )
    (if (null (symbolp mode)) nil	;handle literals
      (cond

       ;;  ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ((and (eq 'code-last-col mode) comment-start)
	(setq re comment-start)
	(save-excursion
	  (beginning-of-line)
	  (setq bp (point))
	  (end-of-line) (setq ep (point))
	  (if (null (re-search-backward comment-start bp t))
	      nil			;not moved anywhere
	    (goto-char (match-beginning 0)))
	  (tic-skip-syntax-backward " " bp)
	  (setq ret (current-column)))
	)

       ;;  ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ;;   find *last* comment start pos
       ;;   like '// i = 1000;   // temporarily commented out'
       ((and (eq 'com-last-col mode) comment-start)
	;;  the '.*' always matches up till last one
	(setq re (concat ".*\\(" comment-start  "\\)"  ))
	(save-excursion
	  (beginning-of-line)
	  (if (not (looking-at re))
	      nil
	    (goto-char (match-beginning 1))
	    (setq ret (current-column))
	    )))

       ;;  ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ;;  whole line is 'alone' comment, so that there is no
       ;;  double comments, return it's starting position
       ;;  Double comment is like the previous cond-case
       ((and (eq 'com-alone-col mode) comment-start)
	(setq re (concat "[ \t]*\\(" comment-start  "\\)"  ))
	;;  notice COM + SPC in re2
	;;  - user may write '// this is separator /////////////////////'
	;;    But that's one comment
	;;  - '// i = MaxPos; // commented'. There is SPC in second
	;;    comment, so it has to be Double Commented line.
	(setq re2 (concat ".*\\(" comment-start  " \\)"  ))
	(save-excursion
	  (beginning-of-line)
	  (if (not (looking-at re))
	      nil
	    (setq p (match-beginning 1))
	    (if (not (looking-at re2))
		(progn				;only single comment
		  (goto-char p)
		  (setq ret (current-column)))
	       (setq p2 (match-beginning 1))
	       (if (not (eq p p2))		;Double Commented
		   nil
		 (goto-char p)			;same comment hit twice
		 (setq ret (current-column)))
	    ))))

       ;;  ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ((eq 'eolpos mode)
	(save-excursion (end-of-line) (setq ret (point))))

       ;;  ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ((eq 'bolpos mode)
	(save-excursion (beginning-of-line) (setq ret (point))))

       ;;  ... ... ... ... ... ... ... ... ... ... ... ... ... ... ... ...
       ((eq 'emptyEol mode)
	;; If the rest of line empty ?
	(setq ret (looking-at "[ \t]*")))



       ))
      ret))




;;; ----------------------------------------------------------------------
;;;
(defun tic-find-code-col (com-start)
  "Looks upward to find possible code indent column.
Eg.

   echo something
                     # ridiculous comment
   <now press ESC ;>
                     # inserts comment here

The problem is that the first comment is considered as indent
for code by normal lisp functions, when it should be the 'echo' line.
We look upward till there is code line that isn't full comment line.

NOTE:
  This doesn't work on C/C++, or any mode that has comment-end,
  because we can't keep track of multiline comments.

Returns:
  nbr		,found code, proposed column returned.
  nil  		,unable to find proper code indent
"
  ;; (interactive "sComment start: ")
  (let* ((re-com  (concat
		   "^[ \t]*" (regexp-quote com-start) "+"
		   "\\|^[ \t]*$"	;skip empty lines
		   ))
	 (move t)
	 p				;point mark
	 ret
	 )
    (save-excursion
      (while (and move (eq ret nil))	;while RET is not set
	(backward-to-indentation 1)
	(beginning-of-line)
	(if (looking-at re-com)
	    (if (eq (point) p)
		(setq move nil))	;we're stucked .. :-C
	  ;; Not rexp? Maybe this real code line?
	  (back-to-indentation)
	  (setq ret (current-column)))
	(setq p (point))
	))
    ret))

;;}}}



;;; ######################################################### &comment ###
;;; The real engine parts to do the job


;;{{{ tic-set-com

;;; ----------------------------------------------------------------------
;;; See simple.el (funcall comment-indent-function)
;;; - Funny thing is that you just can't set comment-column to NBR
;;;   and expect it to be comment place, because the indent-for-comment
;;;   might decide to move the position to another place!
;;; - This function instead, will always put comment there where
;;;   user want's it.
;;;
(defun tic-set-com (&optional new)
  "Lighter version of function indent-for-comment. Moves current comment to
comment-position. Doesn't signal any errors.

Features:
-  if there is multiple comments on the line, like in shell or
   perl code '#', the last one is considered as comment, *unless*
   optional artgument NEW is given. In that case, nothing is considered
   as old comments.
-  If line is too long for comment column it inserts additional SPC or TAB
   between the code and comment. See variable tic-:comment-extra

Return
  t		,position changed OR new comment added
  nil		,position not allowed
"
  (interactive)
  (let* (
	 (xtra	   tic-:comment-extra)
	 (x-spc	   (make-string tic-:comment-extra-arg ?\ ))
	 (x-spc    (if (eq 'tab xtra) "\t" x-spc)) ; what's the insert type ?
	 (stop-col tic-:comment-extra-stop)
	 (ep	   (save-excursion (end-of-line) (point)))
	 (skip	   (or comment-start-skip  comment-start))

	 found
	 bp p				;BEG of line point
	 com-c code-c com-p
	 ret
	 )
    (beginning-of-line)
    (setq bp (point))

    (if (null new)			;did user say new vomment ?
	(while (re-search-forward skip ep t) ;last comment
	  (setq found t)))

;;;    (d! new found)
    (if (null found)
	(progn
	  (end-of-line)
	  (indent-to comment-column)
	  (insert comment-start)
	  (setq ret t)
	  )

      ;;  first set comment position
      (goto-char (match-beginning 0)) (setq p (point))
      (tic-skip-syntax-backward "^ " (1- (match-beginning 0)))
      (setq com-p (point))
      (setq com-c (current-column))	;comment column

      ;; Now where is the code position
      (tic-skip-syntax-backward " " bp) (backward-char 1)
      (setq code-c (current-column))

      (goto-char com-p)
;;;      (d! "#set" (current-column) comment-column com-c)

      (if (= comment-column com-c)
	  nil				;nothing to do
	(setq ret t)
	(if (< code-c comment-column)
	    (progn			;we can indent ok
	      (delete-horizontal-space)
	      (indent-to comment-column))
	  ;;  line is too long to get position to comment-col
;;;	  (d! "Too long line..." (point)  com-c stop-col)
	  (if (> com-c stop-col) nil	;do not touch this
	    (delete-horizontal-space)
;;;	    (d! "del ok" )
	    (insert x-spc))
	  ))
      )
;;;    (d! "#set end" ret (current-column))
    ret
    ))

;;}}}
;;{{{ tic-adj-com

;;; ----------------------------------------------------------------------
;;; Original idea in asm-mode.el by :
;;;   Martin Neitzel,  Techn. Univ. Braunschweig, W.Germany
;;;   BITNET/EARN:   neitzel@dbsinf6.bitnet    (mail via bitnet preferred)
;;
;;; - Thank you Martin! I'm Afraid the code isn't like yours any more,
;;;   but the same principle 'converting to bigger class'
;;;   is preserved.
;;; - This is self standing function.
;;;
;;; - I really should write this again some day, divide into more smaller
;;;   blocks of funcs...
;;;
(defun tic-adj-comment ()
  "Introduce a comment or convert an already existing comment into a
comment of a `bigger' class.  These are the known comment classes:

        1-- Left margin             ;; omitted if there is CODE on the line
        2-- indented like code
        3-- on comment column

Suggested usage: while writing your code, trigger this command repeatedly
until you are satisfied with the comment.


Comment on it's own line note:
- Following lines has two comment chars '#', I call it double commented line.
              # comment text # more text
              # set myVariable = 100;      # temporarily commented
  Because It's hard to know if the line is 'full comment' as in case 1, or
  has 'code temporarily commented out' as line 2, we always consider
  line as 'full comment' if line starts with comment-start.
- In this case whole line moves when converting to classes.

Code note:
-  tic-set-com is used instead of standard indent-for-comment.
-  Considered adding 4th choice: indent like previous comment,
   but I decided 4th choice or 4 taps was too much...3 seemed ideal,
   so I left it out from 'full comment line'.
"
  (interactive)
  (let* (
	 save-excur
	 (def-place tic-:def-com-pos)
	 (tab-alist tic-:tab-call-no-alist)
	 (ci	    (current-indentation))
	 (cc	    (current-column))
	 (com	    comment-start)
	 (col	    comment-column)
	 (ccol	    col)		;copy of the col
	 (clen	    (length comment-start))

	 ;;    handle ONLY lines that have only comment, no code.
	 (re-com    (concat "^[ \t]*\\(" (regexp-quote com) "+\\)"))
	 (re-com2   (concat ".*" (regexp-quote com)))
	 (op	     (point))		;original point

	 cur-code-c prev-cc cur-cc	;various com-col points
	 class bp prev-code-ind ans cont
	 ep				;end point
	 D				;just for debugging
	 tmp
	 )

    (catch 'done
      (if (or (null cc) (null com))
	  (error "comment-[column/start] not set"))

      (if (and com (string-match "[^ \t]" com)) nil
	;;  Should we move the cursor ?
	(if (and (tic-check-line 'emptyEol) (integerp col))
	    (indent-to col))
	(message "tic-adj: no comment-start defined.")
	(throw 'done t))

      (setq cur-code-c (tic-check-line 'code-last-col))
      (setq prev-cc (tic-find-prev-com-col com col cur-code-c));previous c col
      (setq cur-cc (tic-find-com-col))	         ;current comment column

      ;;  - If we do NOT already have a comment, indent for a new one.
      (beginning-of-line)  (setq bp (point))



      (if (looking-at re-com)		;comment on it's own line ?
	  nil ;; (d! "full comment line" )

;;	(d! (looking-at re-com2) re-com2)
	;; .............................................................
	(cond
	 ;;  no comment at all or not suitable comment ?
	 ((or (null (setq tmp (looking-at re-com2)))
	      (and tmp
		   (null (tic-find-com-col))))     ;it isn't suitable
	  (if (not (looking-at "[ \t]*$"))       ; CODE + no COM
	      (progn
		(tic-set-com 'new)	;Normal column, but it'll be changed
		(setq cur-cc (tic-find-com-col))
		(setq class 3)		;indent like prev line by DEF
		)
	    ;;  empty line
	    (insert com)		;see cont, it passes thru
	    ;;  User propably want CODE level indent on empty line by DEF
	    (if (eq def-place 'code ) (setq cont t))
	    ))

	 ;;   There is existing comment
	 ((and cur-cc (= cur-cc col))	;change class if possible
	  (setq class 3))

	 ((and cur-cc prev-cc		;make sure these are set
	       (or (null prev-cc) (= cur-cc prev-cc)))
	  ;;   possibly change class to standard column
	  (tic-set-com))		; no prev comment or position was same

	 (t
	  ;;  add New comment
;;;	  (d! "new")

	  (tic-set-com)
	  ;;(tic-set-com 'new)

	  ))

;;;	(d! "CLASS " class cur-cc col)
	;;   Determine how CODE + COM line is handled
	(if (not (eq class 3))
	    nil				;return current comment point
	  (setq prev-code-ind (tic-find-code-col com))
;;;	  (d! "#1 prev" (point) prev-cc "cur" cur-cc "ind" prev-code-ind
;;;	      "code col" cur-code-c)
	  (if (or (null prev-code-ind)	;No code?? Just beeing paranoid...
		  (>= cur-code-c col)	;cannot put to comment-col
		  (null prev-cc)	;cannot convert to class, isn't set
		  (>= cur-code-c prev-cc)	;cannot use prev com class
		  (not (= cur-cc col))	;convert to com-col CLASS
		  )
	      (progn
;;;		(d! "#1.1 com-col" col)
		(tic-set-com)
		)

	    ;;   Convert to previous comment column class then
;;;	    (d! "#2 prev" prev-cc "cur" cur-cc "code" code-c)
	    (setq comment-column prev-cc); change temporarily
	    (tic-set-com)
	    (setq comment-column col)	;restore value
	    ))

;;;	(d! "cont" cont)
	(if cont
	    nil				;do we continue forward ?
	  (if (not (eolp))
	      (forward-char (length comment-start)))

	  (throw 'done t))
	)

      ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
      ;;  There is a comment, convert it between classes.
      ;;  Also correct the search position due to S-FWD command

      (beginning-of-line)
      (if (re-search-forward comment-start (tic-check-line 'eolpos))
	  (goto-char (match-beginning 0)))

      (setq cc (current-column))	;at front of comment
;;;      (d! "cc" cc)

      ;;   First select where to convert?
      (cond
       ((= cc 0)			;BEG of line ?
	(setq class 1))
       ((and (not (eq ci 0))		; prallel to code ?
	     (not (eq cc col)))		; this is rough guess...
	(setq class 2))
       ((and (not (null col))
	     (= cc col))		; in Comment column ?
	(setq class 0)))


      ;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
;;;   (d! "TIC-ADJ" (point) cc (bolp) class)
      ;;  Now issue the converting procedure
      (cond
       ((eq class 0)			;BEG of line
	(delete-horizontal-space))

       ((eq class 1)			;code level
	(beginning-of-line) (delete-horizontal-space)
	;; Are we allowed to indent this by ourself ?
	(if (null (assoc major-mode tab-alist))
	    (indent-according-to-mode)		;let mode place the staetment
	(if (null (setq col (tic-find-code-col com))) 	    nil
	  (if (and (= cc 0) (= col 0)) ;if suggested POS is same
	      (indent-relative))
	  (indent-to col)))
	)

       ((eq class 2)			;column level
	(indent-to col))
       )
      (forward-char clen)		;after class change
      );; Throw

    ;;  do we need to restore the point ? [experimental]
;;    (if save-excur (goto-char op))
    ))

;;}}}
;;{{{ code: main

;;; ----------------------------------------------------------------------
(defun tic-set-c-vars (&optional cs ce cc css)
  "Sets comment variables CS:comment-start etc... If some ARG is
nil, it will be reset to harmless value.
"
  (if (null cs)			(setq comment-start ""))
  (if (null ce)			(setq comment-end ""))
  (if (not (integerp cc))	(setq comment-column 48))
  (if (null css)		(setq comment-start-skip ""))
  )


;;; ----------------------------------------------------------------------
;;;
(defun tic-indent-for-comment ()
  "Alternative to standard indent-for-comment. Relies on file extension
and doesn't need specific mode to be turned on. Temporary buffers
that has no file name cannot be identified by this function, so
it passes control directly to mode. There is a chance you might not
even notice that this function is working on the background.

Define comment syntax in tic-:comment-file-alist
Verbose warnings are enabled by tic-:comment-notify
Special cases are handled by tic-:comment-extra* variables
Version notice is on command tic-version.


"
  (interactive)
  (let* ((bp	      (current-buffer))
         (bn	      (buffer-file-name bp))
         (bb	      (buffer-name bp))		;; the displayed name
         (warn	      tic-:comment-notify)	;; shorter name, globals here
	 (c-li	      tic-:comment-file-alist)  ;; easier to maintain
	 (c2-li	      tic-:comment-re-file-alist)
	 (no-li	      tic-:adj-no-alist)
         (com-s	      "")		;empty by default if not recognized
	 (com-e	      "")
	 (i	      0)
         (com-col     48)		;default comment column if not set
	 (loop	      t)
         ext re el ass ptr
         )

      (if (null (assoc major-mode no-li))
	  nil
	;;   let mode handle comment classes
	;;   but let's correct some user mistakes first...
	(tic-set-c-vars
	 comment-start comment-end comment-column comment-start-skip)
	(indent-for-comment))


    (if (or (null bn)			;skip, it's *temp* or the kind
	    (assoc major-mode no-li))	;let mode handle comment
	(progn				;just to ease adding debug calls
	  (tic-set-c-vars
	   comment-start comment-end comment-column comment-start-skip)
	  (indent-for-comment)		;mode call...
	  )
      ;; .................................................................
      (if (null (string-match
                 "[a-zA-Z0-9]\\.[a-zA-Z]*$" bn)) nil ;skip, no extension
        (setq ext (substring bn (1+ (match-beginning 0)) (length bn)))
        (setq ass (assoc ext c-li))
;;;	(d! "extension found " ext (nth 0 ass))
        (if (null ass) nil		; skip, if not found
          (setq com-s (nth 1 ass)) (setq com-e (nth 2 ass))
	  (setq com-col (nth 3 ass)))
        ) ;; ----------------------------- string-match

      (if (not (equal "" com-s)) nil    ; skip, found already
        ;;  handle some special files that has no extension
	(setq ptr c2-li loop t)
        (while (and loop ptr)
	  (setq re (nth 0 (setq el (car ptr))))
          (if (null (string-match re bn)) nil
	    (setq loop nil)		; OK raise found flag
	    (setq com-s (nth 1 el))
	    (setq com-e (nth 2 el))
	    (setq com-col (nth 3 el)))
;;;	  (d! "~tic regexp search" loop re  com-col com-s )
	  (setq ptr (cdr ptr)))		;advance
	)

;;;      (d! "result" re com-col com-s com-e)
      ;;   the result could be "" if no match was found --> gives no errors
      (setq comment-start com-s)
      (setq comment-end com-e)

      ;;   if the position is NOT set use default comment position
      (if (integerp comment-column) t ;skip, is set ok
        (setq comment-column com-col))


      ;;  - The indent-for-comment WON'T work if this is nill
      ;;    See simple.el for function def.
      ;;  - We don't set _right_ value, just sufficent replacement.
      (setq comment-start-skip (concat com-s "+"))

      (if (and tic-:comment-notify
	       (equal "" com-s))
	  (message "TIC: unknown file, no comment syntax available"))

;;;      (d! "adj-com" comment-column comment-start comment-end)

      (tic-adj-comment)
      )))

;;}}}

;;{{{ example setup

;;; ......................................................... &example ...
;;; - Copy this, and use M-% to remove comment prefixes ';;* '
;;; - Code can be easily extracted with function tinylib.el/ti::m-pkg-rip-magic

;;* ;;; My personal general 'editing' mode.
;;* ;;; - I don't want use hooks, because then I can't turn on
;;* ;;;   'plain raw' mode. See, the hooks are always run :-(
;;* ;;;
;;* _
;;* (defun mm-fundamental ()                    ;my mode fundamental
;;*   "my fundamental-mode"
;;*   (interactive)
;;*   (fundamental-mode)
;;*   (tii-mode t)				;tinyindent.el
;;*   (setq tii-tt-mode nil)
;;*   (folding-mode)			;folding.el, just great !!
;;*   (recenter)				;I want visible notification
;;*   ;; delete possible comment settings
;;*   (setq comment-start nil
;;* 	comment-end nil
;;*         comment-column nil
;;* 	;;   very important to restore this !! See simple.el
;;*         comment-indent-hook '(lambda () comment-column)
;;*   ))
;;* (global-set-key "\C-cf" 'mm-fundamental)
;;* _
;;* _
;;* (setq c++-mode-hook  'c++-my-hook)	; I want to use this in C++ mode too.
;;* _
;;* (defun c++-my-hook ()
;;*   (setq comment-column nil
;;* 	comment-start nil
;;* 	comment-end nil)
;;*   (tic-install-keys "l"))


;;}}}

(provide 'tinycom)
(run-hooks 'tinycom-load-hook)
;;; ................ end of tinycom.el ...................................
