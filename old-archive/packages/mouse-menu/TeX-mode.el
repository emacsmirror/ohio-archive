;; TeX mode commands.
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.
;; Rewritten following contributions by William F. Schelter
;; and Dick King (king@kestrel).
;; Modified August 1986 by Stephen Gildea <mit-erl!gildea> and
;; Michael Prange <mit-erl!prange> to add LaTeX support and enhance
;; TeX-region.
;; Added TeX-directory and reorganized somewhat  gildea 21 Nov 86
;; Added TeX-screen-print and TeX-screen-print-command for bit-map screens.
;;	Russell A. Ritchie <russell@uk.ac.strath.hci> Fri Aug  7 16:20:51 1987
;; Added TeX-spell-{buffer,region} using 'detex' and 'delatex' from
;; '=contributions' in the tex82 distribution. See note below for details.
;; 	Russell A. Ritchie <russell@uk.ac.strath.hci> Wed Sep 22 09:26:54 1976
;; Modified TeX-spell-{buffer,region} to use Kamal's "detex" program from the
;; TeX 2.5 distribution.
;; Added LaTeX-{chapter,section,label,reference,typestyle,verbatim}
;; and the building blocks for them: LaTeX-{block,wrap,insert}.
;; Should any of these be TeX functions, I never use the raw thing so..?
;; 	Russell A. Ritchie <russell@uk.ac.strath.hci> Tue Oct 12 11:47:41 1976
;; Added TeX-bufferp and TeX-spell-all-TeX-buffers.
;; 	Russell A. Ritchie <russell@uk.ac.strath.hci> Tue Apr  5 12:02:31 1988
;; Added SUN mouse menu facility for TeX commands. See 'sun-menus.el'.
;; 	Russell A. Ritchie <russell@uk.ac.strath.hci> Fri Apr  8 10:01:01 1988
;; Modded TeX-screen-print to prompt for filename if TeX-zap-file is nil.
;; 	Russell A. Ritchie <russell@uk.ac.strath.hci> Thu Sep  8 14:50:53 1988
;; Added (provide 'TeX-mode).
;; 	Russell A. Ritchie, <russell@uk.ac.strath.hci> Tue Feb 14 10:22:11 1989
;; Modified TeX-screen-print-command to conditionalise on window-system.
;; 	Russell A. Ritchie, <russell@uk.ac.strath.hci> Tue Feb 14 10:23:10 1989
;; Modified TeX-screen-print-command to default to "xdvi", and modded
;; display algorithm to work around the fact that XtInitialise thinks
;; that command line arguments starting with a "#" are geometry
;; specifications and strips them out.
;; 	Russell A. Ritchie, <russell@uk.ac.strath.hci> Mon Mar 13 14:52:03 1989
;; Added TeX-BibTeX-command and TeX-BibTeX-buffer.
;; 	Russell A. Ritchie, <russell@uk.ac.strath.hci> Mon Mar 13 14:52:03 1989
;; Added interactive personal word-list maintenance to TeX-spell-region.
;; Define env var SPELLDICT to point to a spell word hash list file
;; (usually created using "cp /usr/dict/hlistb $SPELLDICT" for we Brits.)
;; and use in association with a tex-spell-command consisting of:
;;
;; #!/bin/sh
;; # Run detex on ARG then pipe output to spell
;; if ( `test -w $SPELLDICT` ) then
;;	detex -i < $1 | spell -d $SPELLDICT
;; else
;;	detex -i < $1 | spell -b
;; fi
;; 	Russell A. Ritchie, <russell@uk.ac.strath.hci> Wed Mar 15 16:22:32 1989
;; Added miscellaneous functions to support severely augmented Menu interface.
;; 	Russell Ritchie, <russell@uk.ac.strath.hci> Thu May 10 14:50:06 1990



;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Still to do:
;;  Make TAB indent correctly for TeX code.  Then we can make linefeed
;;  do something more useful.
;;
;;  Have spell understand TeX instead of assuming the entire world
;;  uses nroff.
;;  Done! See TeX-spell-{region,buffer}. <russell@uk.ac.strath.hci>.
;;  TeX commands inside words cause problems still - they are stripped out and
;;  the misspellings found ok but the replace-string can't find them in the
;;  original document, because they're not there...  Use recursive edit as a
;;  workaround OR don't try to print things fancy-fashion until you know how
;;  to spell them...
;;
;;  The code for finding matching $ needs to be fixed.

(provide 'TeX-mode)

(defvar TeX-directory "/tmp/"
  "*Directory in which to run TeX subjob.  Temporary files are
created in this directory.")
(defvar TeX-dvi-print-remove-command "lprm"
  "*Command used to remove a job from the print queue (see lpr-switches).")
(defvar TeX-dvi-print-command "lpr -d"
  "*Command string used by \\[TeX-print] to print a .dvi file.")
(defvar TeX-show-queue-command "lpq -Plw"
  "*Command string used by \\[TeX-show-print-queue] to show the print queue
that \\[TeX-print] put your job on.")
(defvar TeX-default-mode 'plain-TeX-mode
  "*Mode to enter for a new file when it can't be determined whether
the file is plain TeX or LaTeX or what.")
(defvar TeX-screen-print-command
  (if (eq window-system 'x)
      "xdvi"
    "texsun")
  "*Command used by \\[TeX-screen-print] to show a .dvi file on the screen.")
(defvar TeX-BibTeX-command "bibtex"
  "*The command used to generate \".bbl\" files for TeX and LaTeX, usually bibtex.")


(defvar TeX-command nil
  "The command to run TeX on a file.  The name of the file will be appended
to this string, separated by a space.")
(defvar TeX-trailer nil
  "String appended after the end of a region sent to TeX by \\[TeX-region].")
(defvar TeX-start-of-header nil
  "String used by \\[TeX-region] to delimit the start of the file's header.")
(defvar TeX-end-of-header nil
  "String used by \\[TeX-region] to delimit the end of the file's header.")
(defvar TeX-shell-cd-command "cd"
  "Command to give to shell running TeX to change directory.  The value of
TeX-directory will be appended to this, separated by a space.")
(defvar TeX-zap-file nil
  "Temporary file name used for text being sent as input to TeX.
Should be a simple file name with no extension or directory specification.")
(defvar TeX-spell-command "texspell"
  "The command used to strip TeX (or LaTeX) contructs prior to 'spell'ing
usually it will just be a shell script that does 'detex -i < $1 | spell'")

(defvar TeX-mode-syntax-table nil
  "Syntax table used while in TeX mode.")

(defvar TeX-mode-map nil)

(defun TeX-define-common-keys (keymap)
  "Define the keys that we want defined both in TeX-mode
and in the TeX-shell."
  (define-key keymap "\C-c\C-k" 'TeX-kill-job)
  (define-key keymap "\C-c\C-l" 'TeX-recenter-output-buffer)
  (define-key keymap "\C-c\C-q" 'TeX-show-print-queue)
  (define-key keymap "\C-c\C-p" 'TeX-print)
  (define-key keymap "\C-c\C-s" 'TeX-screen-print)
  )

(if TeX-mode-map 
    nil
  (setq TeX-mode-map (make-sparse-keymap))
  (TeX-define-common-keys TeX-mode-map)
  (define-key TeX-mode-map "\"" 'TeX-insert-quote)
  (define-key TeX-mode-map "\n" 'TeX-terminate-paragraph)
  (define-key TeX-mode-map "\e}" 'up-list)
  (define-key TeX-mode-map "\e{" 'TeX-insert-braces)
  (define-key TeX-mode-map "\C-c\C-r" 'TeX-region)
  (define-key TeX-mode-map "\C-c\C-b" 'TeX-buffer)
  (define-key TeX-mode-map "\C-c\C-c" 'TeX-spell-buffer) ; For "C"heck spelling.
  (define-key TeX-mode-map "\C-c\C-f" 'TeX-close-LaTeX-block)
  )

;(fset 'TeX-mode 'tex-mode) in loaddefs.
(defun tex-mode ()
  "Major mode for editing files of input for TeX or LaTeX.
Trys to intuit whether this file is for plain TeX or LaTeX and
calls plain-tex-mode or latex-mode.  If it cannot be determined
\(e.g., the file is empty), the value of TeX-default-mode is used."
  (interactive)
  (let ((mode
	 (save-excursion
	   (goto-char (point-min))
	   (while (and (search-forward "\\" nil t)
		       (let ((search-end (point)))
			 (save-excursion (beginning-of-line)
					 (search-forward "%" search-end t)))))
	   (if (not (eobp))
	       (if (looking-at "documentstyle")
		   'latex-mode
		 'plain-tex-mode)))))
    (if mode (funcall mode)
      (funcall TeX-default-mode))))

(fset 'plain-TeX-mode 'plain-tex-mode)
(fset 'LaTeX-mode 'latex-mode)

(defun plain-tex-mode ()
  "Major mode for editing files of input for plain TeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[TeX-region] to run TeX on the current region, plus a \"header\"
copied from the top of the file (containing macro definitions, etc.),
running TeX under a special subshell.  \\[TeX-buffer] does the whole buffer.
\\[TeX-print] prints the .dvi file made by either of these.
\\[TeX-screen-print] shows the .dvi file on the screen.

Use \\[validate-TeX-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Use \\[TeX-spell-buffer] or \\[TeX-spell-region] to check/fix spelling, and
\\[TeX-spell-all-TeX-buffers] if you've got lots of TeX buffers on the go.       

Special commands:
\\{TeX-mode-map}

Mode variables:
TeX-directory
	Directory in which to create temporary files for TeX jobs
	run by \\[TeX-region] or \\[TeX-buffer].
TeX-dvi-print-command
	Command string used by \\[TeX-print] to print a .dvi file.
TeX-show-queue-command
	Command string used by \\[TeX-show-print-queue] to show the print
	queue that \\[TeX-print] put your job on.
TeX-screen-print-command
	Command string used by \\[TeX-screen-print] to print a .dvi file on the screen.

Entering plain-TeX mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of plain-TeX-mode-hook."
  (interactive)
  (TeX-common-initialization)
  (setq mode-name "TeX")
  (setq major-mode 'plain-TeX-mode)
  (make-local-variable 'TeX-command)
  (setq TeX-command "tex")
  (make-local-variable 'TeX-start-of-header)
  (setq TeX-start-of-header "%**start of header")
  (make-local-variable 'TeX-end-of-header)
  (setq TeX-end-of-header "%**end of header")
  (make-local-variable 'TeX-trailer)
  (setq TeX-trailer "\\bye\n")
  (run-hooks 'text-mode-hook 'TeX-mode-hook 'plain-TeX-mode-hook))

(defun latex-mode ()
  "Major mode for editing files of input for LaTeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[TeX-region] to run LaTeX on the current region, plus the preamble
copied from the top of the file (containing \\documentstyle, etc.),
running LaTeX under a special subshell.  \\[TeX-buffer] does the whole buffer.
\\[TeX-print] prints the .dvi file made by either of these.
\\[TeX-screen-print] shows the .dvi file on the screen.

Use \\[validate-TeX-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Use \\[TeX-spell-buffer] or \\[TeX-spell-region] to check/fix spelling and
\\[TeX-spell-all-TeX-buffers] if you've got lots of LaTeX buffers. Do a 
\\[describe-function] TeX-spell-all-TeX-buffers for more details.

Special commands:
\\{TeX-mode-map}

Mode variables:
TeX-directory
	Directory in which to create temporary files for TeX jobs
	run by \\[TeX-region] or \\[TeX-buffer].
TeX-dvi-print-command
	Command string used by \\[TeX-print] to print a .dvi file.
TeX-show-queue-command
	Command string used by \\[TeX-show-print-queue] to show the print
	queue that \\[TeX-print] put your job on.
TeX-screen-print-command
	Command string used by \\[TeX-screen-print] to print a .dvi file on the screen.

Entering LaTeX mode calls the value of text-mode-hook,
then the value of TeX-mode-hook, and then the value
of LaTeX-mode-hook."
  (interactive)
  (TeX-common-initialization)
  (setq mode-name "LaTeX")
  (setq major-mode 'LaTeX-mode)
  (make-local-variable 'TeX-command)
  (setq TeX-command "latex")
  (make-local-variable 'TeX-start-of-header)
  (setq TeX-start-of-header "\\documentstyle")
  (make-local-variable 'TeX-end-of-header)
  (setq TeX-end-of-header "\\begin{document}")
  (make-local-variable 'TeX-trailer)
  (setq TeX-trailer "\\end{document}\n")
  (run-hooks 'text-mode-hook 'TeX-mode-hook 'LaTeX-mode-hook))

(defun TeX-common-initialization ()
  (kill-all-local-variables)
  (use-local-map TeX-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (if (null TeX-mode-syntax-table)
      (progn
	(setq TeX-mode-syntax-table (make-syntax-table))
	(set-syntax-table TeX-mode-syntax-table)
	(modify-syntax-entry ?\\ ".")
	(modify-syntax-entry ?\f ">")
	(modify-syntax-entry ?\n ">")
	(modify-syntax-entry ?$ "$$")
	(modify-syntax-entry ?% "<")
	(modify-syntax-entry ?\" ".")
	(modify-syntax-entry ?& ".")
	(modify-syntax-entry ?_ ".")
	(modify-syntax-entry ?@ "_")
	(modify-syntax-entry ?~ " ")
	(modify-syntax-entry ?' "w"))
    (set-syntax-table TeX-mode-syntax-table))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^[ \t]*$\\|^[\f\\\\]")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "[^\\]\\(\\\\\\\\\\)*%+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'TeX-comment-indent))

(defun TeX-comment-indent ()
  (if (looking-at "%%%")
      (current-column)
    (skip-chars-backward " \t")
    (max (if (bolp) 0 (1+ (current-column)))
	 comment-column)))

(defun TeX-insert-quote (arg)
  "Insert ``, '' or \" according to preceding character.
With prefix argument, always insert \" characters."
  (interactive "P")
  (if arg
      (let ((count (prefix-numeric-value arg)))
	(if (listp arg)
	    (self-insert-command 1)	;C-u always inserts just one
	  (self-insert-command count)))
    (insert
     (cond
      ((or (bobp)
	   (save-excursion
	     (forward-char -1)
	     (looking-at "[ \t\n]\\|\\s(")))
       "``")
      ((= (preceding-char) ?\\)
       ?\")
      (t "''")))))

(defun validate-TeX-buffer ()
  "Check current buffer for paragraphs containing mismatched $'s.
As each such paragraph is found, a mark is pushed at its beginning,
and the location is displayed for a few seconds."
  (interactive)
  (let ((opoint (point)))
    (goto-char (point-max))
    ;; Does not use save-excursion
    ;; because we do not want to save the mark.
    (unwind-protect
	(while (and (not (input-pending-p)) (not (bobp)))
	  (let ((end (point)))
	    (search-backward "\n\n" nil 'move)
	    (or (TeX-validate-paragraph (point) end)
		(progn
		  (push-mark (point))
		  (message "Mismatch found in pararaph starting here")
		  (sit-for 4)))))
      (goto-char opoint))))

(defun TeX-validate-paragraph (start end)
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (forward-sexp (- end start))
	  t))
    (error nil)))

(defun TeX-terminate-paragraph (inhibit-validation)
  "Insert two newlines, breaking a paragraph for TeX.
Check for mismatched braces/$'s in paragraph being terminated.
A prefix arg inhibits the checking."
  (interactive "P")
  (or inhibit-validation
      (TeX-validate-paragraph
       (save-excursion
	 (search-backward "\n\n" nil 'move)
	 (point))
       (point))
      (message "Paragraph being closed appears to contain a mismatch"))
  (insert "\n\n"))

(defun TeX-insert-braces ()
  "Make a pair of braces and be poised to type inside of them."
  (interactive)
  (insert ?\{)
  (save-excursion
    (insert ?})))

;;; Like TeX-insert-braces, but for LaTeX.
;;; By Michael Prange and Stephen Gildea.
(defun TeX-close-LaTeX-block ()
  "Creates an \\end{...} to match \\begin{...} on the current line and
puts point on the blank line between them."
  (interactive "*")
  (let ((fail-point (point)))
    (end-of-line)
    (if (re-search-backward "\\\\begin{\\([^}\n]*\\)}"
			    (save-excursion (beginning-of-line) (point)) t)
	(let ((text (buffer-substring (match-beginning 1) (match-end 1)))
	      (indentation (current-column)))
	  (end-of-line)
	  (delete-horizontal-space)
	  (insert "\n\n")
	  (indent-to indentation)
	  (insert "\\end{" text "}")
	  (forward-line -1))
      (goto-char fail-point)
      (ding))))

;;; Invoking TeX in an inferior shell.

;;; Why use a shell instead of running TeX directly?  Because if TeX
;;; gets stuck, the user can switch to the shell window and type at it.

;;; It's kludge that we have to create a special buffer just 
;;; to write out the TeX-trailer.  It would nice if there were a
;;; function like write-region that would write literal strings.

(defun TeX-region (beg end)
  "Run TeX on the current region.  A temporary file (TeX-zap-file) is
written in directory TeX-directory, and TeX is run in that directory.
If the buffer has a header, it is written to the temporary file before
the region itself.  The buffer's header is all lines between the
strings defined by TeX-start-of-header and TeX-end-of-header
inclusive.  The header must start in the first 100 lines.  The value
of TeX-trailer is appended to the temporary file after the region."
  (interactive "r")
  (if (get-buffer "*TeX-shell*")
      (TeX-kill-job)
    (TeX-start-shell))
  (or TeX-zap-file (setq TeX-zap-file (make-temp-name "#tz")))
  (let ((tex-out-file (concat TeX-zap-file ".tex"))
	(temp-buffer (get-buffer-create " TeX-Output-Buffer"))
	(zap-directory (expand-directory-name TeX-directory)))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(forward-line 100)
	(let ((search-end (point))
	      (hbeg (point-min)) (hend (point-min))
	      (default-directory zap-directory))
	  (goto-char (point-min))
	  ;; Initialize the temp file with either the header or nothing
	  (if (search-forward TeX-start-of-header search-end t)
	      (progn
		(forward-line -1)
		(setq hbeg (point))	;mark beginning of header
		(if (search-forward TeX-end-of-header nil t)
		    (progn (forward-line 1)
			   (setq hend (point)))	;mark end of header
		  (setq hbeg (point-min))))) ;no header
	  (write-region (min hbeg beg) hend tex-out-file nil 'quietly)
	  (write-region (max beg hend) end tex-out-file t  'quietly))
	(let ((local-tex-trailer TeX-trailer))
	  (set-buffer temp-buffer)
	  (erase-buffer)
	  ;; make sure trailer isn't hidden by a comment
	  (insert-string "\n")
	  (if local-tex-trailer (insert-string local-tex-trailer))
	  (set-buffer-directory temp-buffer zap-directory)
	  (write-region (point-min) (point-max) tex-out-file t 'quietly))))
    (set-buffer-directory "*TeX-shell*" zap-directory)
    (send-string "TeX-shell" (concat TeX-shell-cd-command " "
				     zap-directory "\n"))
    (send-string "TeX-shell" (concat TeX-command " \""
				     tex-out-file "\"\n")))
  (TeX-recenter-output-buffer 0))

(defun TeX-start-shell ()
  (require 'shell)
  (save-excursion
    (set-buffer (make-shell "TeX-shell" "csh" "/dev/null" "-v"))
    (TeX-define-common-keys (current-local-map))))

(defun set-buffer-directory (buffer directory)
  "Set BUFFER's default directory to be DIRECTORY."
  (setq directory (expand-directory-name directory))
  (if (not (file-directory-p directory))
      (error "%s is not a directory" directory)
    (save-excursion
      (set-buffer buffer)
      (setq default-directory directory))))

;;; On Emacs version 18, this should use file-name-as-directory.
(defun expand-directory-name (name)
  "Like expand-file-name, but returns a directory name."
  (file-name-as-directory name))	; ok!?

(defun TeX-buffer ()
  "Run TeX on current buffer.  See  TeX-region  for more information."
  (interactive)
  (TeX-region (point-min) (point-max)))

(defun TeX-kill-job ()
  "Kill the currently running TeX job."
  (interactive)
  (quit-process "TeX-shell" t))

(defun TeX-recenter-output-buffer (linenum)
  "Redisplay buffer showing current TeX job so that most recent
output can be seen.  The last line of the buffer is displayed on
line LINE of the window, or centered if LINE is nil."
  (interactive "P")
  (let ((tex-shell (get-buffer "*TeX-shell*"))
	(old-buffer (current-buffer)))
    (if (null tex-shell)
	(message "No TeX output buffer")
      (pop-to-buffer tex-shell)
      (bury-buffer tex-shell)
      (goto-char (point-max))
      (recenter (if linenum
		    (prefix-numeric-value linenum)
		  (/ (window-height) 2)))
      (pop-to-buffer old-buffer)
      )))

(defun TeX-print ()
  "Print the .dvi file made by \\[TeX-region] or \\[TeX-buffer].
Runs the shell command defined by TeX-dvi-print-command."
  (interactive)
  (let ((dvi-files
	 (mapcar (function list) (directory-files default-directory nil ".*\.dvi$"))))
    (if (and (null TeX-zap-file) (null dvi-files))
	(error "You must run %s to produce a \".dvi\" file to print." mode-name)
      (send-string "TeX-shell"
		   (format
		    "%s \"%s\"\n"
		    TeX-dvi-print-command
		    (if TeX-zap-file
			(concat TeX-zap-file ".dvi")
		      (completing-read "Show which dvi file: " dvi-files (function identity) t))))
      (TeX-recenter-output-buffer nil))))

(defun TeX-show-print-queue ()
  "Show the print queue that \\[TeX-print] put your job on.
Runs the shell command defined by TeX-show-queue-command."
  (interactive)
  (if (not (get-buffer "*TeX-shell*"))
      (TeX-start-shell))
  (send-string "TeX-shell" (concat TeX-show-queue-command "\n"))
  (TeX-recenter-output-buffer nil))

(defun TeX-screen-print ()
  "Show the .dvi file made by \\[TeX-region] or \\[TeX-buffer] on the screen.
Runs the shell command defined by TeX-screen-print-command.
Tries to start at page number specified by absolute value of prefix argument PAGENUMBER"
  (interactive)
  (send-string
   "TeX-shell"
   (format "%s \"%s\"\n" 
	   TeX-screen-print-command
	   (concat
	    "./"
	    ;; Hack to get aroung Xdvi's gobbling of
	    ;; command-line args that begin with "#" as
	    ;; geometry specifications, even though it realises
	    ;; that they are invalid and ignores them, it
	    ;; doesn't put them back into the argv.
	    (if TeX-zap-file
		(concat TeX-zap-file ".dvi")
	      (completing-read
	       "Show which dvi file: "
	       (mapcar (function list) (directory-files default-directory nil ".*\.dvi$"))
	       (function identity)
	       t))))))

(defun TeX-spell-buffer ()
  "Run TeX-spell-filter-command on buffer, then spell"
  (interactive)
  (TeX-spell-region (point-min)(point-max) (buffer-name)))

(defun TeX-spell-region (beg end &optional description)
  "Run TeX-spell-filter-command on current region, then spell"
  (interactive "r")
  (let ((buf (get-buffer-create " *temp*")))
    (save-excursion
     (set-buffer buf)
     (widen)
     (erase-buffer))
    (message "Filtering out the %s constructs in %s..."
	     mode-name (or description "the region"))
    (if (= ?\n (char-after (1- end)))
	(call-process-region beg end TeX-spell-command nil buf)
      (let ((oldbuf (current-buffer)))
	(save-excursion
	 (set-buffer buf)
	 (insert-buffer-substring oldbuf beg end)
	 (insert ?\n)
	 (call-process-region (point-min) (point-max) TeX-spell-command t buf))))
    (message "Checking spelling of %s...%s"
	     (or description "region")
	     (if (save-excursion
		  (set-buffer buf)
		  (> (buffer-size) 0))
		 "not correct"
	       "correct"))
    (let (word newword
	  (case-fold-search t)
	  (case-replace t)
	  (new-word-buf (get-buffer-create "*New Words*")))
      (save-excursion
	(set-buffer new-word-buf)
	(widen)
	(erase-buffer))
      (while (save-excursion
	      (set-buffer buf)
	      (> (buffer-size) 0))
	(save-excursion
	 (set-buffer buf)
	 (goto-char (point-min))
	 (setq word (buffer-substring (point)
				      (progn (end-of-line) (point))))
	 (forward-char 1)
	 (delete-region (point-min) (point))
	 (setq newword (read-input (concat "Replacement for " word ": ")
				   word))
	 (flush-lines (concat "^" (regexp-quote word) "$")))
	(if (not (equal word newword))
	    (progn
	     (goto-char (point-min))
	     (query-replace-regexp (concat "\\b" (regexp-quote word) "\\b")
				   newword))
	  ;; If user has a local dictionary (filename specified by (getenv "SPELLDICT")) 
	  ;; Save words that user says don't need changing, then offer to add them
	  ;; to local dictionary. 
	  (save-excursion
	    (set-buffer new-word-buf)
	    (insert newword "\n")))))
    (save-excursion
      (let ((new-word-buf (get-buffer-create "*New Words*"))
	    (pop-up-windows t))
	(save-window-excursion
	  (pop-to-buffer new-word-buf)
	  (if (and (> (buffer-size) 0) (getenv "SPELLDICT"))
	      ;; We have some new words and the user has a personal
	      ;; dictionary, offer to save the new spellings.
	      (let* ((users-dict (getenv "SPELLDICT"))
		     (users-dict-full-path (expand-file-name users-dict)))
		(if (yes-or-no-p (format "Add these words to %s? " users-dict))
		    (let ((message (format "Merging new words with words in %s..." users-dict)))
		      (message message)
		      (call-process-region
		       (point-min) (point-max) "spellin" t t nil users-dict)
		      (write-region (point-min) (point-max) users-dict-full-path nil nil)
		      (message (concat message "done")))))))
	(kill-buffer new-word-buf)))))

(defun TeX-bufferp (&optional buffer)
  "Return buffer-name if it is a TeX (or LaTeX) buffer, or nil otherwise.
One optional arg, BUFFER, the buffer to test (or name of one), default is current."
  (if buffer
      ;; Pop to the desired buffer and test it instead of the current one.
      (save-excursion
	(set-buffer buffer)
	(TeX-bufferp))
    ;; This is the desired buffer, is it TeX or LaTeX?
    (if (or (eq major-mode 'tex-mode)
	    (eq major-mode 'TeX-mode)
	    (eq major-mode 'latex-mode)
	    (eq major-mode 'LaTeX-mode))
	(buffer-name))))

(defun TeX-spell-all-TeX-buffers ()
  "Apply TeX-spell-buffer to all TeX (or LaTeX) buffers that can be found.

 This should really grap the contents of all the TeX buffers into a single
 temporary buffer (with appropriate 'invisible' begin--end marks) and run
 TeX-spell-buffer on that. This would save having to repeat corrections 
 for regularly misspelt words like misspelled (or vice versa ?!).
 Afterwards the altered regions would be put back in the correct buffers.
 The 'detex' program used by TeX-spell understands about '\include{file}',
 and runs itself recursively on the specified files. Some extra brainpower
 should be added here, to avoid duplicated effort. A combination of (pwd)
 and (buffer-file-name) should allow the '\include' files to be visited.
 The corresponding '\include{file}' command should either be diked out of
 the temporary buffer, or an appropriate flag passed to 'detex' to avoid
 the recursive runs. At the moment (Tue Apr 5 14:58:18 1988) the flag that
 is passed to 'detex' to stop this is '-i'. For extra hack-value it should
 also figure out what 'bib' files are being used (if any) and dike out any
 defined citation references from the misspelt word list. Any volunteers?
"
  (interactive)
  (let ((TeX-buffer-names
	  (apply (function nconc)
		 (mapcar (function (lambda (x) (if (TeX-bufferp x) (list x))))
			 (buffer-list)))))
    (save-excursion
      (while TeX-buffer-names
	(let ((this-buffer (car TeX-buffer-names)))
	  (switch-to-buffer this-buffer t) ; Show the one we're doing.
	  (TeX-spell-buffer)		; and do it...
	  (setq TeX-buffer-names (cdr TeX-buffer-names)))))))

(defun TeX-BibTeX-buffer ()
  "Run the command specified by  TeX-BibTeX-command  on the current buffer."
  (interactive)
  (if (null TeX-zap-file)
      (error "You must run %s on the whole buffer before you can use BibTeX."
	     mode-name)
    (send-string "TeX-shell"
		 (concat TeX-BibTeX-command " \"" TeX-zap-file "\"\n"))
    (TeX-recenter-output-buffer nil)))

;;; To do: Modify wrappers to work with regions for sectioning/typestyling...
;;; Use one LaTeX-wrap-region function that takes wrap-thing & in/prefix flag.

(defun LaTeX-block(block)
  "Surround the word at (or before) point with \\BLOCK{ and } and move past }"
  (LaTeX-insert (concat "\\" block "{")))

(defun LaTeX-wrap(type)
  "Wrap {\TYPE and } around the word at or before point, and move past }"
  (LaTeX-insert (concat "{\\" type " ")))

(defun LaTeX-insert(start)
  "Put START before word at point, move to end of word and add \"} \""
  (forward-word -1)
  (insert start)
  (forward-word 1)
  (insert "} "))

(defun LaTeX-cite()
  "Make word at point a bibliographic citation, bound to \\[LaTeX-cite]"
  (interactive)
  (LaTeX-block "cite")
  (forward-word -2)
  (forward-char -1)
  (delete-horizontal-space)
  (insert "~")
  (forward-word 2)
  (forward-char 1))

(defun LaTeX-section(arg)
  "Make word at point chapter name, bound to \\[LaTeX-section]
One \C-u makes it a section, two make it a subsection, three a subsubsection."
  (interactive "p")
  (cond ((eq arg 1) (LaTeX-block "chapter"))
	((eq arg 4) (LaTeX-block "section"))
	((eq arg 16) (LaTeX-block "subsection"))
	(t (LaTeX-block "subsubsection"))))

(defun LaTeX-label()
  "Make word at point a label, bound to \\[LaTeX-label]"
  (interactive)
  (LaTeX-block "label"))

(defun LaTeX-index()
  "Make word at point an index entry, bound to \\[LaTeX-index]"
  (interactive)
  (LaTeX-block "index"))

(defun LaTeX-reference(arg)
  "Make current word a LaTeX reference, according to table:

       __________________________________________________
       | Prefix Argument Value        | Reference       |
       |-------------------------------------------------
       | 1 (default if no prefix arg) |  bibliographic	|
       | >3 (use \C-u once)	      |  page number	|
       | >15 (use \C-u twice)	      |  figure		|
       | >63 (use \C-u thrice)	      |  table		|
       --------------------------------------------------"
  (interactive "p")
  (if (eq arg 4) (LaTeX-block "pageref") (LaTeX-block "ref"))
  (save-excursion
    (forward-word -2)
    (forward-char -1)
    (delete-horizontal-space)
    (insert (concat (cond ((eq arg 1) "")
			  ((eq arg 4) " page")
			  ((eq arg 16) " figure")
			  ((eq arg 64) " table")) "~"))))

(defun LaTeX-typestyle(arg)
  "Make current word change type style depending on prefix arg table below:
       ________________________________________________
       | Prefix Argument Value        | Type Style    |
       |-----------------------------------------------
       | 1 (default if no prefix arg) |  bold         |
       | >3 (use \C-u once)	      |  emphasise    |
       | >15 (use \C-u twice)	      |  italics      |
       | >63 (use \C-u thrice)	      |  sans-serif   |
       ------------------------------------------------"
  (interactive "p")
  (cond ((eq arg 1) (LaTeX-wrap "bf"))
	((eq arg 4) (LaTeX-wrap "em"))
	((eq arg 16) (LaTeX-wrap "it"))
	((eq arg 64) (LaTeX-wrap "sf"))))

(defun LaTeX-verbatim(char)
  "Make region delimited by (prompted for) CHAR an in-text verbatim entry."
  (interactive "sDelimited by what?: ")
  (save-excursion
    (let ((point (point)))
      (search-backward char (point-min) t 2)
      (if (eq point (point))		; We haven't moved.
	  (error (concat "I couldn't find 2 '" char "'s"))
	(insert "\\verb")))))

(defun TeX-typeface-region (typeface)
  "Insert \"{\\TYPEFACE\" and \"}\" around current region.
Where TYPEFACE is a known LaTeX type style."
  (save-excursion
    (goto-char (region-beginning))
    (insert "{\\" typeface)
    (goto-char (region-end)))
    (insert "}"))

(defun TeX-new-section (type text)
  (interactive "sSection type: \nsSection text: ")
  (insert-string "\n\\" type "{" text "}\n"))

(defun TeX-new-heading (type)
  (interactive)
  (TeX-new-section type (read-string "Heading text: ")))

(defun TeX-remove-from-print-queue (jobnumber)
  (interactive "nRemove Printer Job number: ")
  (apply 'start-process "Print Queue Removal" nil
	 TeX-dvi-print-remove-command
	 (append lpr-switches (list (format "%s" jobnumber))))
  (message
   "Removing Job %s from Printer Queue %s..." jobnumber
   (mapconcat 
    'identity
    (append (list "using" TeX-dvi-print-remove-command) lpr-switches) " ")))

(defun TeX-suspend-job ()
  "Stop the *TeX-shell*'s current subjob."
  (save-excursion
    (set-buffer "*TeX-shell*")
    (stop-shell-subjob)))

(defun TeX-continue-job ()
  "Continue the *TeX-shell*'s current subjob."
  (save-excursion
    (set-buffer "*TeX-shell*")
    (continue-shell-subjob)))

