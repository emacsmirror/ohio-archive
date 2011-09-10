;;;Organization: Freelance Software Consultant, Washington DC.
;;;Date:         Sun, 8 Oct 89 21:39:49 GMT
;;;From:         Wolfgang Rupprecht <apple!bloom-beacon!mgm.mit.edu!wolfgang@BBN.COM>
;;;Subject:      Re: HP-UX f77 and GNU Emacs.

;;;In article <MATS.89Oct8142939@heron.qz.se> mats@heron.qz.se (Mats Pettersson QZ)
;;; writes:
;;;>HP-UX f77 produces a somewhat nonstandard(?) error listing which
;;;>causes a little problem when executing f77 from whithin Emacs (ie
;;;>M-X compile). Emacs can't correctly parse the resulting error
;;;>list but reports 'No more errors', or something equivalent, regardless
;;;>of how many errors there actually where.

;;;Here is a patch to compile.el which may be easier to add new
;;;error-rexexps to.  I have been using this n various forms for quite a
;;;while.

;;;Key features:
;;;	* table driven.  New error parser expressions easily added.
;;;	* No pre-parsing of *compilations* buffer.  Much faster at
;;;	  starting up. (try running normal next-error on a grep of the
;;;	  Unix kernel source tree sometime)
;;;	* the next-error to be parsed is the next line after cursor.
;;;	  Errors to be examined are easily chosen by moving cursor in
;;;	  "*compilations*" buffer.
;;;	* understands all error outputs that I have been subjected to
;;;	  in the last 4 years.  Even understands make/makefiles.
;;;	* Three window output for lint's "n-th arg used inconsistently"
;;;	* It's Free Software (tm) ;-)

;;;To install:
;;;	* make sure load-path includes the directory that this file
;;;	  will be put into.
;;;	* set a variable old-load-path to be the original load path
;;;	  that emacs was compiled with.
;;;	  eg.
;;;		load-path => ("/usr/src/gnuemacs/lisp-patches/"
;;;			      "/usr/src/gnuemacs/lisp/")
;;;		old-load-path => ("/usr/src/gnuemacs/lisp/")

;;;enjoy,
;;;-wolfgang

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;									     ;;
;;	File:     compile.el	 					     ;;
;;	Author:   Wolfgang Rupprecht					     ;;
;;	Created:  March 1987				     	             ;;
;;	Contents: I did significant hacking to the error parser		     ;;
;;		for next-error.  The parser now has a table that	     ;;
;;		it scans for applicable rexexp templates.  If		     ;;
;;		one of them fits, it uses that one to parse the		     ;;
;;		line. If it doesn't fit, the scanner tries the		     ;;
;;		next template. If all templates fail, the line		     ;;
;;		is deemed a useless line and discarded.			     ;;
;;									     ;;
;;	Copyright (c) 1989, 1987 Wolfgang Rupprecht.			     ;;
;;									     ;;
;;	$Header$							     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file is not really part of GNU Emacs, but is (hopefully)
;; useful in conjunction with it.  It is meant as a patch to the
;; distributed GnuEmacs lisp file by the same name.

;; GNU Emacs and this compile.el is distributed in the hope that it
;; will be useful, but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General
;; Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights
;; and responsibilities.  It should be in a file named COPYING.  Among
;; other things, the copyright notice and this notice must be
;; preserved on all copies.

;; First we load the real lib.  The original load-path should be stored
;; in the variable old-load-path.

(let ((load-path old-load-path))
  (require 'compile))

;; Now we patch it.

(define-key ctl-x-map "`" 'wsr:next-error)

;; let's hope that nobody is stupid enough to put a colon or
;; parenthesis in their filenames, (these regexps desperately need to
;; cue off of them) -wsr

;; for forced updating of the defvar definitions
;; (defmacro defvar-f (name val doc) (list 'setq name val))

(defvar error-parsing-regexp-list
  ;; rule 0: 4.3bsd grep, cc, lint(part1 warnings)
  ;; /users/wolfgang/foo.c(8): warning: w may be used before set
  '(("^\\([^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t]" 1 2)
    ;; rule 1: 4.3bsd lint part2: inconsistant type warnings
    ;; strcmp: variable # of args.	llib-lc(359)  ::  /users/wolfgang/foo.c(8)
    ("^[^\n]*[ \t]+\\([^:( \t\n]+\\)[:(]+[ \t]*\\([0-9]+\\)[:) \t]+\\([^:(
 \t\n]+\\)[:(]+[ \t]*\\([0-9]+\\)[:) \t]+$"
     3 4 1 2)

;;;    ("[^\n]*[ \t:]+\\([^:( \t\n]+\\)[ \t]*[:(]+[ \t]*\\([0-9]+\\)[:) \t]*$"
   1 2)
    ;; rule 2: 4.3bsd lint part3: defined, but unused messages
    ;; linthemorrhoids defined( /users/wolfgang/foo.c(4) ), but never used
    ;; foo used( file.c(144) ), but not defined
    ("[^\n]*\\(defined\\|used\\)[ \t(]+\\([^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:)
 \t]+"
     2 3)
    ;; rule 3: 4.3bsd compiler
    ;; "foo.c", line 18: cc_cardiac_arrest undefined
    ("^[\* \t]*[Ll]ine[ \t]+\\([0-9]+\\)[ \t]+of[ \t]+\"\\([^\"]+\\)\":" 2 1)
    ;; rule 4: apollo cc warnings, yuk -wsr
    ("^[\* \t]*\"\\([^\"]+\\)\",?[ \t]+[Ll]ine[ \t]+\\([0-9]+\\):" 1 2)
    ;; rule 5: as on a sun 3 under sunos 3.4
    ;;(as) "spl.i", line 23:  Error:  syntax error.
    ("^(.+)[ \t]+\"\\([^\"]+\\)\",[ \t]+line[ \t]+\\([0-9]+\\):" 1 2)
    ;; rule 6: m88kcc
    ;; "./foo.h" , line 128: redeclaration of bar
    ;; note the extra space before the comma (after filename) : grotty
    ("^\\((.+)[ \t]+\\)?\"\\([^\"]+\\)\" ?,[ \t]+line[ \t]+\\([0-9]+\\):" 2 3)
    ;; rule 7: Make
    ;; Make: line 20: syntax error.  Stop.
    ;; Make: Must be a separator on rules line 84.  Stop.
    ("^[\* \t]*[Mm]ake: [^\n]*[Ll]ine[ \t]+\\([0-9]+\\)[.:]"
     scan-make 1)
    ;; rule 8: there is no rule 8
    ;; (add other rules and explanations here)
    )
  "a list of lists consisting of:
\((rexexp filename-match-index linenum-match-index)(...)(...))
for parsing error messages")

(defun test-parse (pos)
  "Test the line parsing code, attempts to parse the current line for
filename and line number. Answer is returned in minibuffer."
  (interactive "d")
  (forward-line 0)
  (let (filename linenum filename-2 linenum-2) ; set by compilation-parse-line
    (let ((parserule (compilation-parse-line)))
      (if parserule
	  (if filename-2
	      (message "Parses as: '%s(%d)' and '%s(%d)' [rule %d]"
		       filename linenum
		       filename-2 linenum-2 parserule)
	    (message "Parses as: '%s(%d)' [rule %d]"
		     filename linenum parserule))
	(message "Couldn't parse that line")))))

(defvar compilations-window-height 4 "*Height of compilations buffer window.")

;; parse error msgs, find file or files, and position cursor on the
;; appropriate lines.
;; The "primary" file is always at the bottom of the screen.
;; The *compilations* buffer is always at the top, and reduced to
;;  a smaller height.

(defun wsr:next-error (&optional argp)
  "This is the *new* NEXT ERROR: Visit next compilation error message
and corresponding source code.  This operates on the output from the
\\[compile] command.  This command uses the line after current point
as the starting point of the next error search."
  (interactive)
  (pop-to-buffer "*compilation*")
  (let ((opoint (point))
	(pwd default-directory)
	filename linenum filename-2 linenum-2)
    (while (and (zerop (forward-line 1))
		(null (compilation-parse-line))))
    (if (null filename)
	(error (concat compilation-error-message
		       (if (and compilation-process
				(eq (process-status compilation-process)
				    'run))
			   " yet" "")))
      (recenter 0)
      (if (file-exists-p filename)
	  (progn
	    (delete-other-windows)
	    (let ((wh (window-height)))
	      (find-file-other-window filename)
	      (shrink-window (- compilations-window-height (/ wh 2)))
	      )
	    (goto-line linenum)
	    (recenter (/ (window-height) 2))
	    (if filename-2	; a two file match
		(let ((default-directory pwd)) ; get the pwd right!
		      (if (file-exists-p filename-2)
			  (progn
			    (split-window-vertically nil)
			    (find-file filename-2)
			    (goto-line linenum-2)
			    (recenter (/ (window-height) 2))
			    ;; now back to file # 1
			    (other-window 1)
			    ;; needed if both windows are on the same file
			    (recenter (/ (window-height) 2))
			    )
			(message "Can't find file '%s(%d)'"
				 filename-2 linenum-2)))))
	(error "Can't find file '%s(%d)'" filename linenum)))))

(defun compilation-parse-line ()
  "Parse this line, setq-ing filename and linenum."
  (let ((parse-list error-parsing-regexp-list)
	(rule-num 0))
    (while parse-list
      (let ((rule-list (car parse-list)))
	(if (looking-at (car rule-list))
	    (let ((file-index (nth 1 rule-list))
		  (line-index (nth 2 rule-list))
		  (file-2-index (nth 3 rule-list))
		  (line-2-index (nth 4 rule-list)))
 	      (setq linenum (string-to-int
			     (buffer-substring (match-beginning line-index)
					       (match-end line-index))))
	      (if file-2-index
		  (progn
		    (setq filename-2 (buffer-substring
				      (match-beginning file-2-index)
				      (match-end file-2-index)))
		    (setq linenum-2 (string-to-int
				     (buffer-substring
				      (match-beginning line-2-index)
				      (match-end line-2-index))))))
	      (setq filename
		    (cond ((integerp file-index)
			   (buffer-substring (match-beginning file-index)
					     (match-end file-index)))
			  ;; careful! this next funcall may mash
			  ;; the match-data, so it must be done
			  ;; after all the line numbers and names have been
			  ;; extracted
			  ((symbolp file-index) (funcall file-index))
			  ((stringp file-index) file-index)
			  (t (error "Parsing error: unknown action type: %s"
				    file-index))))
	      (setq parse-list nil))	;we're done
	  (setq parse-list (cdr parse-list)
		rule-num (1+ rule-num)))))
    (and linenum filename rule-num)))	; return matching rule number

(defun scan-make ()
  "Attempt to find the name of the Makefile used by this make run.
This routine shouln't be used for anything drastic, since it just isn't
that robust."
  (cond ((save-excursion
	   (re-search-backward "make[^\n]+-f[ \t]+\\(\\sw\\|\\s_\\)+" nil t))
	 (buffer-substring (match-beginning 1)(match-end 1)))
	((file-exists-p "makefile") "makefile")
	((file-exists-p "Makefile") "Makefile")
	(t nil)
      ))
;;;Wolfgang Rupprecht	ARPA:  wolfgang@mgm.mit.edu (IP 18.82.0.114)
;;;TEL: (703) 768-2640	UUCP:  mit-eddie!mgm.mit.edu!wolfgang
  