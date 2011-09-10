;;;; igrep.el --- An improved interface to `grep`.

;;; Description:
;;; 
;;; Define the \[igrep] command, which is like \[grep] except that it
;;; takes three required arguments (PROGRAM, EXPRESSION, and FILES) and
;;; an optional argument (OPTIONS) instead of just one argument (COMMAND).
;;; Also define the analogous \[egrep] and \[fgrep] commands for convenience.
;;; 
;;; Define the \[igrep-find] command, which is like \[igrep] except that
;;; it uses `find` to recursively `grep` a directory.  Also define the
;;; analogous \[egrep-find] and \[fgrep-find] commands for convenience.
;;; 
;;; \[igrep] and \[igrep-find] (and their analogues) provide defaults
;;; for the EXPRESSION and FILES arguments when called interactively,
;;; and there are global variables that control the syntax of the `grep`
;;; and `find` shell commands that are executed.  A user option controls
;;; whether the corresponding GNU (gzip) "zPROGRAM" script is used, to
;;; `grep` compressed files.
;;; 
;;; \[agrep] and \[agrep-find] are also defined as convenient interfaces
;;; to the approximate `grep` utility, which is distributed with the
;;; `glimpse' indexing and query tool (available from
;;; <URL:http://glimpse.cs.arizona.edu:1994/>).
;;; 
;;; \[grep] itself is advised to provide the \[igrep] interface when
;;; called interactively (when called programmatically, it still uses
;;; the original argument list).  \[grep-find] is defined as an alias
;;; for \[igrep-find].
;;; 
;;; When run interactively from Dired mode, the various \[igrep]
;;; commands provide defaults for the EXPRESSION and FILES arguments
;;; that are based on the visited directory (including any inserted
;;; subdirectories) and the current file.  In addition, the
;;; \[dired-do-igrep] and \[dired-do-igrep-find] commands are defined
;;; that respect the `dired-do-*' command calling conventions: a prefix
;;; argument is interpreted as the number of succeeding files to `grep`,
;;; otherwise all the marked files are `grep`ed.  \[dired-do-grep] and
;;; \[dired-do-grep-find] are defined as aliases for \[dired-do-igrep]
;;; and \[dired-do-igrep-find].

;;; Copyright:
;;; 
;;; Copyright © 1994,1995,1996,1997 Kevin Rodgers
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;; 
;;; Neither my former nor current employer (Martin Marietta and
;;; Information Handling Services, respectively) has disclaimed any
;;; copyright interest in igrep.el.
;;; 
;;; Kevin Rodgers <kevinr@ihs.com>		Project Engineer
;;; Information Handling Services		Electronic Systems Development
;;; 15 Inverness Way East, M/S A201		
;;; Englewood CO 80112 USA			(303)397-2807[voice]/-2779[fax]

;;; Installation:
;;; 
;;; 1. Put this file in a directory that is a member of load-path, and
;;;    byte-compile it (e.g. with `M-x byte-compile-file') for better
;;;    performance.  You can ignore any warnings about references to free
;;;    variables and "not known to be defined" functions.
;;; 2. Put these forms in default.el or ~/.emacs:
;;;    (autoload (function igrep) "igrep"
;;;       "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
;;;    (autoload (function igrep-find) "igrep"
;;;       "*Run `grep` via `find`..." t)
;;;    (autoload (function dired-do-igrep) "igrep"
;;;       "*Run `grep` on the marked (or next prefix ARG) files." t)
;;;    (autoload (function dired-do-igrep-find) "igrep"
;;;       "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
;;; 2. a. For completeness, you can add these forms as well:
;;;    (autoload (function grep) "igrep"
;;;       "*Run `grep` PROGRAM to match EXPRESSION in FILES..." t)
;;;    (autoload (function egrep) "igrep"
;;;       "*Run `egrep`..." t)
;;;    (autoload (function fgrep) "igrep"
;;;       "*Run `fgrep`..." t)
;;;    (autoload (function agrep) "igrep"
;;;       "*Run `agrep`..." t)
;;;    (autoload (function grep-find) "igrep"
;;;       "*Run `grep` via `find`..." t)
;;;    (autoload (function egrep-find) "igrep"
;;;       "*Run `egrep` via `find`..." t)
;;;    (autoload (function fgrep-find) "igrep"
;;;       "*Run `fgrep` via `find`..." t)
;;;    (autoload (function agrep-find) "igrep"
;;;       "*Run `agrep` via `find`..." t)
;;;    (autoload (function dired-do-grep) "igrep"
;;;       "*Run `grep` on the marked (or next prefix ARG) files." t)
;;;    (autoload (function dired-do-grep-find) "igrep"
;;;       "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
;;; 3. If you are running Windows 95/NT, you should install findutils
;;;    and grep from release 17.1 (or higher) of the Cygnus GNU-Win32
;;;    distribution.  See <URL:http://www.cygnus.com/misc/gnu-win32/>.

;;; Usage:
;;; 
;;; M-x igrep			M-x igrep-find
;;; M-x  grep			M-x  grep-find
;;; M-x egrep			M-x egrep-find
;;; M-x fgrep			M-x fgrep-find
;;; M-x agrep			M-x agrep-find
;;; M-x dired-do-igrep		M-x dired-do-igrep-find
;;; M-x  dired-do-grep		M-x  dired-do-grep-find
;;; (Each of the 10 igrep commands accepts 1, 2, or 3 `C-u' prefix arguments.
;;; The 2 Dired commands interpret a prefix argument like the regular `dired-do'
;;; commands.)
;;; C-x ` (M-x next-error)	C-c C-c (M-x compile-goto-error) [in *igrep*]

;;; Customization examples:
;;; 
;;; To ignore case by default:
;;; 	(setq igrep-options "-i")
;;; To search subdirectories by default:
;;; 	(setq igrep-find t)
;;; To search files with the GNU (gzip) zgrep script:
;;; 	(setq igrep-use-zgrep t)
;;; or define new igrep commands (this works for zegrep and zfgrep as well):
;;; 	(igrep-define zgrep)		; M-x zgrep
;;; 	(igrep-find-define zgrep)	; M-x zgrep-find
;;; To avoid exceeding some shells' limit on command argument length
;;; (this only works when grep'ing files in the current directory):
;;; 	(setq igrep-find t
;;; 	      igrep-find-prune-option "\\! -name .")

;;; To do:
;;; 
;;; 1. Delete the *-program variables (except for igrep-program) and
;;;    replace igrep-options with a table that maps igrep-program to the
;;;    appropriate options.
;;; 2. Generalize support for the -prune find clause (e.g. -fstype nfs).
;;; 3. Provide support for `glimpse`.
;;; 4. Add a menu interface.

;;; LCD Archive Entry:
;;; igrep|Kevin Rodgers|kevinr@ihs.com|
;;; An improved interface to grep/egrep/fgrep; plus recursive `find` versions.|
;;; 10-Feb-1997|2.56|~/misc/igrep.el.gz|


;;; Package interface:

(provide 'igrep)

(require 'backquote)			; igrep-with-default-in-history
(require 'compile)			; compile-internal and grep-regexp-
					; alist

(defconst igrep-version "2.56"
  "Version of igrep.el")


;;; User options:

(defvar igrep-options nil
  "*The options passed by \\[igrep] to `igrep-program', or nil.

`-n' will automatically be passed to `igrep-program', to generate the
output expected by \\[next-error] and \\[compile-goto-error].
`-e' will automatically be passed to `igrep-program', if it supports
that option.")

(defvar igrep-read-options nil
  "*If non-nil, `\\[igrep]' always prompts for options;
otherwise, it only prompts when 1 or 3 `C-u's are given as a prefix arg.")

(defvar igrep-read-multiple-files nil
  "*If non-nil, `\\[igrep]' always prompts for multiple-files;
otherwise, it only prompts when 2 or 3 `C-u's are given as a prefix arg.")

(defvar igrep-verbose-prompts t
  "*If t, \\[igrep] prompts for arguments verbosely;
if not t but non-nil, \\[igrep] prompts for arguments semi-verbosely;
if nil, \\[igrep] prompts for arguments tersely.")

(defvar igrep-save-buffers 'query
  "*If t, \\[igrep] first saves each modified file buffer;
if not t but non-nil, \\[igrep] offers to save each modified file buffer.")

(defvar igrep-program-table		; referenced by igrep-use-zgrep
  (let ((exec-directories exec-path)
	(program-obarray (make-vector 11 0)))
    (while exec-directories
      (if (and (car exec-directories)
	       (file-directory-p (car exec-directories)))
	  (let ((grep-programs
		 (directory-files (car exec-directories)
				  nil "grep\\(\\.exe\\)?\\'")))
	    (while grep-programs
	      ;; Check `(file-executable-p (car grep-programs))'?
	      (if (save-match-data
		    (string-match "\\.exe\\'" (car grep-programs)))
		  (intern (substring (car grep-programs) 0 -4) program-obarray)
		(intern (car grep-programs) program-obarray))
	      (setq grep-programs (cdr grep-programs)))))
      (setq exec-directories (cdr exec-directories)))
    program-obarray)
  "An obarray of available `grep` programs, passed by `igrep-read-program'
to `completing-read' when `igrep-program' is nil.")

(defvar igrep-use-zgrep
  (if (intern-soft "zgrep" igrep-program-table)
      'files)
  "If t, \\[igrep] searches files using the GNU (gzip) `zPROGRAM` script;
If not t but non-nil, \\[igrep] searches compressed FILES using `zPROGRAM`;
if nil, \\[igrep] searches files with `PROGRAM`.")


;;; User variables:

(defvar igrep-program "grep"
  "The default shell program run by \\[igrep] and \\[igrep-find].
It must take a `grep` expression argument and one or more file names.
If nil, \\[igrep] prompts for the program to run.")

(defvar igrep-expression-quote-char
  (if (memq system-type '(ms-dos windows-95 windows-nt emx))
      ?\"
    ?')
  "The character used to delimit the EXPRESSION argument to \\[igrep],
to protect it from shell filename expansion.")

(defvar igrep-expression-option
  (if (or (eq system-type 'berkeley-unix)
	  (save-match-data
	    (string-match "-sco" system-configuration)))
      "-e")
  "If non-nil, the option used to specify the EXPRESSION argument to \\[igrep],
to protect an initial `-' from option processing.")

(defvar igrep-parenthesis-escape-char
  (if (memq system-type '(ms-dos windows-95 windows-nt emx))
      nil
    ?\\)
  "If non-nil, the character used by \\[igrep] to escape parentheses,
to protect them from shell interpretation.")

(defvar igrep-find nil
  "If non-nil, \\[igrep] searches directories using `find`.
See \\[igrep-find].")

(defvar igrep-find-prune-options
  (if (not (save-match-data
	     (string-match "-sco" system-configuration)))
      "-name SCCS -o -name RCS")
  "The `find` clause used to prune directories, or nil;
see \\[igrep-find].")

(defvar igrep-find-file-options "-type f -o -type l"
  "The `find` clause used to filter files passed to `grep`, or nil;
see \\[igrep-find].")

(defvar igrep-find-use-xargs
  (if (equal (call-process "find" nil nil nil
			   (if (boundp 'grep-null-device)
			       grep-null-device
			     "/dev/null")
			   "-print0")
	     0)
      'gnu)
  "If `gnu', \\[igrep-find] executes
	`find ... -print0 | xargs -0 -e grep ...`;
if not `gnu' but non-nil, \\[igrep-find] executes
	`find ... -print | xargs -e grep ...`;
if nil, \\[igrep-find] executes
	`find ... -exec grep ...`.")

(defvar igrep-program-default nil
  "The default `grep` program, passed by `igrep-read-program'
to `completing-read' when `igrep-program' is nil.")

(defvar igrep-expression-history '()
  "The minibuffer history list for \\[igrep]'s EXPRESSION argument.")

(defvar igrep-files-history '()
  "The minibuffer history list for \\[igrep]'s FILES argument.")


;;; Commands:

(defadvice grep (around igrep-interface first (&rest grep-args) activate)
  "If called interactively, use the \\[igrep] interface instead,
where GREP-ARGS is (PROGRAM EXPRESSION FILES OPTIONS);
if called programmatically, GREP-ARGS is still (COMMAND)."
  (interactive (igrep-read-args))
  (if (interactive-p)
      (apply (function igrep) grep-args)
    ad-do-it))

(defalias 'grep-find 'igrep-find)

(defun igrep (program expression files &optional options)
  "*Run `grep` PROGRAM to match EXPRESSION in FILES.
The output is displayed in the *igrep* buffer, which \\[next-error] and
\\[compile-goto-error] parse to find each line of matched text.

PROGRAM may be nil, in which case it defaults to `igrep-program'.

EXPRESSION is automatically delimited by `igrep-expression-quote-char'.

FILES is either a file name pattern (expanded by the shell named by
`shell-file-name') or a list of file name patterns.

Optional OPTIONS is also passed to PROGRAM; it defaults to `igrep-options'.

If a prefix argument \
\(\\[universal-argument]\) \
is given when called interactively,
or if `igrep-read-options' is set, OPTIONS is read from the minibuffer.

If two prefix arguments \
\(\\[universal-argument] \\[universal-argument]\) \
are given when called interactively,
or if `igrep-read-multiple-files' is set, FILES is read from the minibuffer
multiple times.

If three prefix arguments \
\(\\[universal-argument] \\[universal-argument] \\[universal-argument]\) \
are given when called interactively,
or if `igrep-read-options' and `igrep-read-multiple-files' are set,
OPTIONS is read and FILES is read multiple times.

If `igrep-find' is non-nil, the directory or directories
containing FILES is recursively searched for files whose name matches
the file name component of FILES \(and whose contents match
EXPRESSION\)."
  (interactive
   (igrep-read-args))
  (if (null program)
      (setq program (or igrep-program "grep")))
  (if (null options)
      (setq options igrep-options))
  (if (not (listp files))		; (stringp files)
      (setq files (list files)))
  (if (string-match "^[rj]?sh$" (file-name-nondirectory shell-file-name))
      ;; (restricted, job-control, or standard) Bourne shell doesn't expand ~:
      (setq files
	    (mapcar 'expand-file-name files)))
  (let* ((win32-quote-process-args nil)	; work around NT Emacs hack
	 (use-zgrep (cond ((eq igrep-use-zgrep t))
			  (igrep-use-zgrep
			   (let ((files files)
				 (compressed-p nil))
			     (while (and files (not compressed-p))
			       (if (save-match-data
				     (string-match "\\.g?[zZ]\\'" (car files)))
				   (setq compressed-p t))
			       (setq files (cdr files)))
			     compressed-p))
			  (t nil)))
	 (command (format "%s -n %s %s %c%s%c %s %s"
			  (if (and use-zgrep
				   (save-match-data
				     (not (string-match "\\`z" program))))
			      (setq program (concat "z" program))
			    program)
			  (or options "")
			  (or igrep-expression-option
			      (progn
				(if (save-match-data
				      (string-match "\\`-" expression))
				    (setq expression (concat "\\" expression)))
				""))
			  igrep-expression-quote-char
			  expression
			  igrep-expression-quote-char
			  (if igrep-find
			      (if igrep-find-use-xargs
				  ""
				"\"{}\"")
			    (mapconcat (function identity) files " "))
			  (if (boundp 'grep-null-device)
			      grep-null-device
			    "/dev/null"))))
    (if igrep-find
	(setq command
	      (igrep-format-find-command command files)))
    (cond ((eq igrep-save-buffers t) (save-some-buffers t))
	  (igrep-save-buffers (save-some-buffers)))
    (compile-internal command
		      (format "No more %c%s%c matches"
			      igrep-expression-quote-char
			      program
			      igrep-expression-quote-char)
		      "igrep" nil grep-regexp-alist)))

;; Analogue commands:

(defmacro igrep-define (analogue-command &rest igrep-bindings)
  "Define ANALOGUE-COMMAND as an `igrep' analogue command.
Optional (VARIABLE VALUE) arguments specify temporary bindings for the command."
;;;  (interactive "SCommand: ") ; C-u => read bindings?
  (let ((analogue-program (symbol-name analogue-command)))
    (` (defun (, analogue-command) (&rest igrep-args)
	 (, (format "*Run `%s` via \\[igrep].
All arguments \(including prefix arguments, when called interactively\)
are handled by `igrep'."
		    analogue-program))
	 (interactive
	  (let ((igrep-program (if igrep-program (, analogue-program)))
		(igrep-program-default (, analogue-program)))
	    (igrep-read-args)))
	 (let ( (,@ igrep-bindings))
	   (apply (function igrep)
		  (cond ((interactive-p) (car igrep-args))
			((car igrep-args))
			(t (, analogue-program)))
		  (cdr igrep-args)))))))

(igrep-define egrep)
(igrep-define fgrep)
(igrep-define agrep
  (igrep-use-zgrep nil)
  (igrep-expression-option "-e"))


;; Recursive (`find`) commands:

(defun igrep-find (&rest igrep-args)
  "*Run `grep` via `find`; see \\[igrep] and `igrep-find'.
All arguments \(including prefix arguments, when called interactively\)
are handled by `igrep'."
  (interactive
   (let ((igrep-find t))
     (igrep-read-args)))
  (let ((igrep-find t))
    (apply (function igrep) igrep-args)))

;; Analogue recursive (`find`) commands:

(defmacro igrep-find-define (analogue-command &rest igrep-bindings)
  "Define ANALOGUE-COMMAND-find as an `igrep' analogue `find` command.
Optional (VARIABLE VALUE) arguments specify temporary bindings for the command."
;;;  (interactive "SCommand: ") ; C-u => read bindings?
  (let ((analogue-program (symbol-name analogue-command)))
    (setq analogue-command
	  (intern (format "%s-find" analogue-command)))
    (` (defun (, analogue-command) (&rest igrep-args)
	 (, (format "*Run `%s` via \\[igrep-find].
All arguments \(including prefix arguments, when called interactively\)
are handled by `igrep'."
		    analogue-program))
	 (interactive
	  (let ((igrep-program (if igrep-program (, analogue-program)))
		(igrep-program-default (, analogue-program))
		(igrep-find t))
	    (igrep-read-args)))
	 (let ( (,@ igrep-bindings))
	   (apply (function igrep-find)
		  (cond ((interactive-p) (car igrep-args))
			((car igrep-args))
			(t (, analogue-program)))
		  (cdr igrep-args)))))))

(igrep-find-define egrep)
(igrep-find-define fgrep)
(igrep-find-define agrep
  (igrep-use-zgrep nil)
  (igrep-expression-option "-e"))


;; Dired commands:

(defun dired-do-igrep (program expression &optional options arg)
  "*Run `grep` PROGRAM to match EXPRESSION (with optional OPTIONS)
on the marked (or next prefix ARG) files."
  (interactive
   (let* ((current-prefix-arg nil)
	  (igrep-args (igrep-read-args t)))
     ;; Delete FILES:
     (setcdr (nthcdr 1 igrep-args) (nthcdr 3 igrep-args))
     ;; Append ARG:
     (nconc igrep-args (list current-prefix-arg))))
  (igrep program
	 expression
	 (funcall (cond ((fboundp 'dired-get-marked-files) ; GNU Emacs
			 'dired-get-marked-files)
			((fboundp 'dired-mark-get-files) ; XEmacs
			 'dired-mark-get-files))
		  t arg)
	 options))

(defalias 'dired-do-grep 'dired-do-igrep)

;; Dired recursive (`find`) commands:

(defun dired-do-igrep-find (program expression &optional options arg)
  "*Run `grep` PROGRAM to match EXPRESSION (with optional OPTIONS)
on the marked (or next prefix ARG) directories."
  (interactive
   (let* ((current-prefix-arg nil)
	  (igrep-find t)
	  (igrep-args (igrep-read-args t)))
     ;; Delete FILES:
     (setcdr (nthcdr 1 igrep-args) (nthcdr 3 igrep-args))
     ;; Append ARG:
     (nconc igrep-args (list current-prefix-arg))))
  (let ((igrep-find t))
    (dired-do-igrep program expression options arg)))

(defalias 'dired-do-grep-find 'dired-do-igrep-find)


;;; Utilities:

(defsubst igrep-file-directory (name)
  ;; Return the directory component of NAME, or "." if it has no
  ;; directory component.
  (directory-file-name (or (file-name-directory name)
			   (file-name-as-directory "."))))

(defsubst igrep-file-pattern (name)
  ;; Return the file component of NAME, or "*" if it has no file
  ;; component.
  (let ((pattern (file-name-nondirectory name)))
       (if (string= pattern "")
	   "*"
	 pattern)))

(defun igrep-format-find-command (command files)
  ;; Format `grep` COMMAND to be invoked via `find` on FILES.
  (let ((directories '())
	(patterns '()))
    (while files
      (let ((dir (igrep-file-directory (car files)))
	    (pat (igrep-file-pattern (car files))))
	(if (and (not (string= dir "."))
		 (file-symlink-p dir))
	    (setq dir (concat dir "/.")))
	(if (not (member dir directories))
	    (setq directories (cons dir directories)))
	(cond ((equal pat "*")
	       (setq patterns t))
	      ((and (listp patterns)
		    (not (member pat patterns)))
	       (setq patterns (cons pat patterns)))))
      (setq files (cdr files)))
    (format (cond ((eq igrep-find-use-xargs 'gnu)
		   ;; | \\\n
		   "find %s %s %s %s -print0 | xargs -0 -e %s")
		  (igrep-find-use-xargs
		   ;; | \\\n
		   "find %s %s %s %s -print | xargs -e %s")
;;;		  ((memq system-type '(ms-dos windows-95 windows-nt emx))
;;;		   "find %s %s %s %s -exec %s ;")
		  (t
		   "find %s %s %s %s -exec %s \\;"))
	    (mapconcat (function identity) (nreverse directories)
		       " ")
	    (if igrep-find-prune-options
		(format "-type d %c( %s %c) -prune -o"
			(or igrep-parenthesis-escape-char ? )
			igrep-find-prune-options
			(or igrep-parenthesis-escape-char ? ))
	      "")
	    (if igrep-find-file-options
		(format "%c( %s %c)"
			(or igrep-parenthesis-escape-char ? )
			igrep-find-file-options
			(or igrep-parenthesis-escape-char ? ))
	      "")
	    (if (listp patterns)
		(if (cdr patterns)	; (> (length patterns) 1)
		    (format "%c( %s %c)"
			    (or igrep-parenthesis-escape-char " ")
			    (mapconcat (function (lambda (pat)
						   (format "-name \"%s\"" pat)))
				       (nreverse patterns)
				       " -o ")
			    (or igrep-parenthesis-escape-char " "))
		  (format "-name \"%s\"" (car patterns)))
	      "")
	    command)))

(defun igrep-read-args (&optional no-files)
  ;; Read and return a list: (PROGRAM EXPRESSION FILES OPTIONS).
  ;; If NO-FILES is non-nil, then FILES is not read and nil is returned
  ;; in its place.
  (let* ((program (igrep-read-program (if igrep-verbose-prompts
					  (if igrep-find
					      "[find] "))))
	 (prompt-prefix (if igrep-verbose-prompts
			    (apply (function concat)
				   (if igrep-find
				       "[find] ")
				   (if (eq igrep-verbose-prompts t)
				       (list program " ")))))
	 (options (igrep-read-options prompt-prefix)))
    (if (eq igrep-verbose-prompts t)
	(setq prompt-prefix
	      (concat prompt-prefix options " ")))
    (list program
	  (igrep-read-expression prompt-prefix)
	  (if (not no-files)
	      (igrep-read-files prompt-prefix))
	  options)))

(defsubst igrep-prefix (prefix string)
  ;; If PREFIX is non-nil, concatenate it and STRING; otherwise, return STRING.
  (if prefix
      (concat prefix string)
    string))

(defun igrep-read-program (&optional prompt-prefix)
  ;; If igrep-program is nil, read and return a program name from the
  ;; minibuffer; otherwise, return igrep-program.
  ;; Optional PROMPT-PREFIX is prepended to the "Program: " prompt.
  (or igrep-program
      (let ((prompt "Program: "))
	(completing-read (igrep-prefix prompt-prefix prompt) igrep-program-table
			 nil t (or igrep-program-default "grep")))))

(defun igrep-read-options (&optional prompt-prefix)
  ;; If current-prefix-arg is '(4) or '(64), read and return an options
  ;; string from the minibuffer; otherwise, return igrep-options.
  ;; Optional PROMPT-PREFIX is prepended to the "Options: " prompt.
  (if (or igrep-read-options
	  (and (consp current-prefix-arg)
	       (memq (prefix-numeric-value current-prefix-arg)
		     '(4 64))))
      (let ((prompt "Options: "))
	(read-string (igrep-prefix prompt-prefix prompt)
		     (or igrep-options "-")))
    igrep-options))

(defun igrep-read-expression (&optional prompt-prefix)
  ;; Read and return a `grep` expression string from the minibuffer.
  ;; Optional PROMPT-PREFIX is prepended to the "Expression: " prompt.
  (let ((default-expression (igrep-default-expression)))
    (if (string= default-expression "")
	(read-from-minibuffer (igrep-prefix prompt-prefix "Expression: ")
			      nil nil nil 'igrep-expression-history)
      (let ((expression
	     (igrep-read-string-with-default-in-history
	      (igrep-prefix prompt-prefix (format "Expression (default %s): "
						  default-expression))
	      default-expression
	      'igrep-expression-history)))
	(if (string= expression "")
	    default-expression
	  expression)))))

(defun igrep-default-expression ()
  (if (eq major-mode 'dired-mode)
      (let ((dired-file (dired-get-filename nil t)))
	(save-excursion
	  (set-buffer (or (and dired-file (get-file-buffer dired-file))
			  (other-buffer (current-buffer) t)))
	  (current-word)))
    (current-word)))

(defsubst igrep-default-key ()
  ;; Return the key bound to `exit-minibuffer', preferably "\r".
  (if (eq (lookup-key minibuffer-local-completion-map "\r")
	  (function exit-minibuffer))
      "\r"
    (where-is-internal (function exit-minibuffer)
		       minibuffer-local-completion-map
		       t)))

(defun igrep-read-files (&optional prompt-prefix)
  ;; Read and return a file name pattern from the minibuffer.  If
  ;; current-prefix-arg is '(16) or '(64), read multiple file name
  ;; patterns and return them in a list.  Optional PROMPT-PREFIX is
  ;; prepended to the "File(s): " prompt.
  (let* ((dired-subdirectory (if (eq major-mode 'dired-mode)
				 (dired-current-directory t)))
	 (default-files (concat dired-subdirectory
				(igrep-default-file-pattern)))
	 (prompt (format "File(s) (default %s): " default-files))
	 (insert-default-directory nil)	; use relative path names
	 (file (igrep-read-file-name-with-default-in-history
		(igrep-prefix prompt-prefix prompt)
		default-files
		nil
		'igrep-files-history)))
    (if (or igrep-read-multiple-files
	    (and (consp current-prefix-arg)
		 (memq (prefix-numeric-value current-prefix-arg)
		       '(16 64))))
	(let ((files (list file)))
	  (setq prompt
		(igrep-prefix prompt-prefix
			      (if igrep-verbose-prompts
				  (format "File(s): [Type `%s' when done] "
					  (key-description (igrep-default-key)))
				"File(s): ")))
	  (while (not (string= (setq file
				     (igrep-read-file-name prompt
							   nil "" nil nil
							   'igrep-files-history))
			       ""))
	    (setq files (cons file files)))
	  (nreverse files))
      file)))

(defmacro igrep-with-default-in-history (default history &rest body)
  ;; Temporarily append DEFAULT to HISTORY, and execute BODY forms.
  (` (progn
       ;; Append default to history:
       (set history
	    (cons (, default)
		  (if (boundp (, history))
		      (symbol-value (, history))
		    '())))
       (unwind-protect			; Make sure the history is restored.
	   ;; Execute body:
	   (progn (,@ body))
	 ;; Delete default from history (undo the append above):
	 (setcdr (symbol-value (, history))
		 (nthcdr 2 (symbol-value (, history))))))))

(defun igrep-read-string-with-default-in-history (prompt default history)
  ;; Read a string from the minibuffer, prompting with string PROMPT.
  ;; DEFAULT can be inserted into the minibuffer with `previous-
  ;; history-element'; HISTORY is a symbol whose value (if bound) is a
  ;; list of previous results, most recent first.
  (let ((string (igrep-with-default-in-history default history
		  (read-from-minibuffer prompt nil nil nil history))))
    ;; Replace empty string in history with default:
    (if (string= string "")
	(setcar (symbol-value history) default))
    string))

(defun igrep-read-file-name-with-default-in-history
  (prompt &optional default initial history)
  ;; Read a file name from the minibuffer, prompting with string PROMPT.
  ;; DEFAULT can be inserted into the minibuffer with `previous-
  ;; history-element'; HISTORY is a symbol whose value (if any) is a
  ;; list of previous results, most recent first.
  (igrep-with-default-in-history default history
    (igrep-read-file-name prompt nil default nil initial history)))

(defun igrep-read-file-name (prompt
  &optional directory default existing initial history)
  ;; Just like read-file-name, but with optional HISTORY.
  ;; Also: convert DIRECTORY to DIRECTORY/* file name pattern.
  (let ((file-name
	 (if history
	     (let ((file-name-history (symbol-value history)))
	       (prog1 (read-file-name prompt directory default existing initial)
		 (set history file-name-history)))
	   (read-file-name prompt directory default existing initial))))
    (if (and (not (string-equal file-name ""))
	     (file-directory-p file-name))
	(expand-file-name "*" file-name)
      file-name)))

(defun igrep-default-file-pattern ()
  ;; Return a shell file name pattern that matches files with the same
  ;; extension as the file being visited in the current buffer.
  ;; (Based on other-possibly-interesting-files in ~/as-is/unix.el, by
  ;; Wolfgang Rupprecht <wolfgang@mgm.mit.edu>.)
  (if (eq major-mode 'dired-mode)
      (cond ((stringp dired-directory)
	     (if (file-directory-p dired-directory)
		 "*"
	       (file-name-nondirectory dired-directory))) ; wildcard
	    ((consp dired-directory)	; (DIR FILE ...)
	     (mapconcat 'identity (cdr dired-directory) " ")))
    (if buffer-file-name
	(let ((file-name (file-name-nondirectory buffer-file-name)))
	  (concat "*"
		  (save-match-data
		    (if (string-match "\\.[^.]+\\(\\.g?[zZ]\\)?$" file-name)
			(substring file-name
				   (match-beginning 0) (match-end 0))))))
      "*")))

;;; Local Variables:
;;; eval: (put 'igrep-with-default-in-history 'lisp-indent-hook 2)
;;; eval: (put 'igrep-define 'lisp-indent-hook 1)
;;; eval: (put 'igrep-find-define 'lisp-indent-hook 1)
;;; End:

;;;; igrep.el ends here
