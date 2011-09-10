;;; shelldb.el --- customisable context-based database for the shell.

;; Author: Simon Marshall <Simon.Marshall@mail.esrin.esa.it>
;; Keywords: processes completion
;; Version: 1.03

;; LCD Archive Entry:
;; shelldb|Simon Marshall|Simon.Marshall@mail.esrin.esa.it|
;; Customisable context-based database for shell commands.|
;; 06-Dec-1994|1.03|~/packages/shelldb.el.Z|
;; The archive is archive.cis.ohio-state.edu in /pub/gnu/emacs/elisp-archive.

;;; This file is not part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Purpose:
;; 
;; Shelldb is a customisable general and context-based information package.
;; Shelldb already knows about many command-line options, command-line option
;; arguments, specific command arguments, and general command arguments.
;; Functions are provided to customise the databases is a relatively painless
;; way.

;; Installation:
;; 
;; To use, put in your package:
;;
;; (require 'shelldb)

;; Help:
;;
;; Try M-x shelldb-help.

;; Customisation:
;; 
;; Package	Requires	Comments
;; ============================================================================
;; shelldb	shell		Database of shell entities (usernames,
;;				hostnames, etc., and contextual information)
;; ----------------------------------------------------------------------------
;; shellc	shelldb		Completion of shell entities
;; ----------------------------------------------------------------------------
;; shells	shelldb		Spelling correction of shell entities
;;		fuzzy-match
;; ----------------------------------------------------------------------------
;; shellt	shell		Tracking (directories and relevent shell
;;				entities).  Useful if shellc or shells is used.
;; ----------------------------------------------------------------------------
;; fuzzy-match			Fuzzy string matcher (not shell related)
;; ----------------------------------------------------------------------------

;; Feedback:
;;
;; Please send me bug reports, bug fixes, and extensions, so that I can
;; merge them into the master source.
;;     - M-x shelldb-submit-feedback
;;     - Simon Marshall (Simon.Marshall@mail.esrin.esa.it)
;; Please send me the lisp forms (see shelldb-help) plus your email address,
;; name, machine/OS, and the version of this file so that I can keep track of
;; what has been added.  I don't want patches to shelldb-command-arguments.

;; History:
;;
;; 1.00--1.01:
;; - See shellc.
;; 1.02--1.03:
;; - Commented out `reporter-dump-variable' from `shelldb-submit-feedback'.
;; - Made `build-hostnames' use ~/.netrc (from Ralf Fassel) and ~/.ftptoolrc.

(require 'shell)

(eval-when-compile
  (require 'reporter))

(defconst shelldb-version "1.02"
  "Version number of the shelldb package.")

;;; HELP!!!

(defun shelldb-help ()
  "This package is intended to provide customisable general and context-based
information for the shell.

Contextual information is of three types:
- Command options (e.g., \"---verbose \", \"--version\", etc.)
- Command option arguments (e.g., option arg of \"-user\" might be a username)
- Command arguments generally (e.g., the command accepts host names only)

Provided with this package are general databases for command names (including
aliases and builtins) host names, user names, shell variables, environment
variables, mail names, errr...  You can build your own databases.

General Information:
~~~~~~~~~~~~~~~~~~~~
- Database creation see `shelldb-make' and `shelldb-initialise'
- Database value see `shelldb-db' and `shelldb-elements'
- Database modification see `shelldb-set', `shelldb-add' and `shelldb-remove'
- Database evaluation see `shelldb-get'

General databases are of the of lists of entities.  Each entity is of the form
\"name\" or \"name=value\".

For example, a general database of user names:

 (shelldb-make 'usernames
  (shelldb-shell-strings \"(ypcat passwd; cat /etc/passwd) 2>/dev/null\"
                         \"^[^+][^:]*\" 0)

Or if you want to defer the calculation of the initial value of the database
for speed:

 (shelldb-make 'usernames)

 (shelldb-initialise 'usernames
   '(shelldb-shell-strings \"(ypcat passwd; cat /etc/passwd) 2>/dev/null\"
			   \"^[^+][^:]*\" 0))

Evalutating:

 (shelldb-db 'usernames)

returns the database `usernames'.  If the database is nil, as it would be the
first time it is referred to in this example, then it is initialised by calling
the form specified to `shelldb-initialise'.

By default, the following databases are provided by shelldb: `hostnames',
`usernames', `shell-variables', `environment-variables', `command-builtins',
and `command-aliases'.

Databases `shell-variables', `environment-variables' and `command-builtins' are
buffer-local.

Contextual Information:
~~~~~~~~~~~~~~~~~~~~~~~
Information:
- Command entry see `shelldb-command-arguments'
Modification:
- Command options see `shelldb-add-command-options'
- Command option arguments see `shelldb-add-command-option-arguments'
- Command general arguments see `shelldb-add-command-arguments'
Also:
- General command general arguments see `shelldb-add-general-option-arguments'
- Command context removal see `shelldb-remove-command'
- Command context option removal see `shelldb-remove-command-option'

For example, a common command is \"make\".  Maybe your \"make\" has a parallel
option:

 (shelldb-add-command-options \"make\" '(\"-fork\"))

Maybe the option can have a number of arguments:

 (shelldb-add-command-option-arguments \"make\" \"-fork\" t \"fork type\"
                                      '(\"numtargets\" \"maxprocs\"))

Maybe it would be good if your \"make\" completes general arguments as
executables, though not if it follows the \"-fork\" option directly:

 (shelldb-add-command-arguments \"make\"
  (list nil (shelldb-option-preceding-argument \"-fork\")
        \"target\" 'filespec nil nil nil nil t))

Even more fun, making real commands from scratch:

 ;; Add the commands and options.
 (shelldb-add-command-options '(\"rsh\" \"rlogin\") '(\"-n\" \"-l\"))

 ;; If it's after option \"-l\", complete as username.
 (shelldb-add-command-option-arguments '(\"rsh\" \"rlogin\") \"-l\" t
  \"username\" 'username)

 ;; If it's the first argument, and not an option, complete as hostname.
 (shelldb-add-command-arguments '(\"rsh\" \"rlogin\")
  (list (shelldb-argumentn 1) (shelldb-last-argument \"-\\\\S *\")
        \"hostname\" 'hostname))

 ;; If it's not after \"-l\", try completing as hostname.
 (shelldb-add-command-arguments '(\"rsh\" \"rlogin\")
  (list nil (shelldb-option-preceding-argument \"-l\") nil 'hostname))

 ;; If it's not the first argument or after \"-l\", try completing as command.
 (shelldb-add-command-arguments \"rsh\"
  (list nil (format \"%s\\\\|%s\" (shelldb-argumentn 1)
		    (shelldb-option-preceding-argument \"-l\"))
        nil 'command))

Phew!

It is, of course, up to the user of the database to decide how to interpret
entity types.  This package uses the types `alias', `command', `filespec',
`hostname', `mailname', `username', `variable', and `shell-string'.

For the destructive rather than the constructive:

 (shelldb-remove-command '(\"dos\" \"msdos\" \"dos2unix\" \"unix2dos\"))

Maybe there's a bug in shelldb's command completion entries:

 (shelldb-remove-command-option \"ftp\" '(\"-rwxr-xr-x\" \"-rwx------\"))

To extract an entry for a command, use `shelldb-command-arguments':

 (shelldb-command-arguments \"ping\")
      => (\"ping\"
          (\"-R\" \"-l\" \"-r\" \"-v\")
          ((nil \"\\\\s +-\\\\S *$\" \"hostname\" hostname)))

You shouldn't need to add, or remove, many things, but if you do please let me
know.  I'd be especially grateful for GNU command information.  Don't, please,
send me the nasty lists, or even patches to them, just send me the functions
\(in terms of those above) that you used to add or remove the commands/command
options/command option arguments/command arguments.

Submit comments, bug reports, etc. with M-x shelldb-submit-feedback.

Enjoy."
  (interactive)
  (let ((buf "*Help Shelldb*"))
    (with-output-to-temp-buffer buf
      (princ (documentation 'shelldb-help) (get-buffer buf)))))

;;; Some useful functions for messing about with shelldb-command-arguments.

(defun shelldb-command-arguments (command)
  "Return the COMMAND entry."
  (assoc command shelldb-command-arguments))

(defun shelldb-add-command-options (command options)
  "For COMMAND, use OPTIONS for completion.
If COMMAND is a list, use for each command in the list represented by COMMAND.
If OPTIONS is a string, use the options for the command represented by OPTIONS.

For example:
 (shelldb-add-command-options \"mycommand\" '(\"-option1\" \"-option2\"))
 (shelldb-add-command-options \"mycommand\" \"emacs\")
 (shelldb-add-command-options '(\"gzip\" \"gunzip\") '(\"--version\"))

See completion modification functions `shelldb-add-command-option-arguments',
`shelldb-add-command-arguments', `shelldb-add-general-option-arguments' and
`shelldb-remove-command'.

See final argument regexp generating functions `shelldb-last-argument',
`shelldb-option-preceding-argument' and `shelldb-argumentn'.

See variable `shelldb-command-arguments'."
  (cond ((listp command)
	 (while command
	   (shelldb-add-command-options (car command) options)
	   (setq command (cdr command))))
	((stringp options)
	 (shelldb-add-command-options
	  command (nth 1 (assoc options shelldb-command-arguments))))
	(t
	 (let* ((old-entry (assoc command shelldb-command-arguments))
		(new-opts
		 (uniq (sort (append options (nth 1 old-entry))
			     '(lambda (f s)
				(cond ((and (stringp f) (stringp s))
				       (string-lessp f s))
				      ((stringp f) t)
				      ((stringp s) nil)
				      (t (string-lessp (car f) (car s))))))
		       'equal)))
	   (setq shelldb-command-arguments
		 (cons (list command new-opts (nth 2 old-entry))
		       (delete old-entry shelldb-command-arguments)))))))


(defun shelldb-add-command-option-arguments (command option gap barf
					     &rest arguments)
  "For COMMAND's OPTION, use ARGUMENTS for option completion.
If COMMAND is a list, use for each command in the list represented by COMMAND.
If OPTION is a list, use each option in the list represented by OPTION.
If GAP non-nil, whitespace is expected between the option and it argument.

For example:
 (shelldb-add-command-option-arguments \"find\" \"-user\" t \"username\" 'username)

 (shelldb-add-command-option-arguments \"find\" \"-fstype\" t
  \"file system type\" '(\"nfs\" \"4.2\"))

 (shelldb-add-command-option-arguments \"find\" '(\"-exec\" \"-ok\") t
  \"command\" 'command)

 (shelldb-add-command-option-arguments '(\"cc\" \"cpp\" \"gcc\" \"lint\" \"ld\" \"as\")
  \"-I\" nil \"dirname\" 'filespec nil nil \"^-I\" t nil)

See `shelldb-add-command-options' and `shelldb-command-arguments'."
  (cond ((listp command)
	 (while command
	   (apply 'shelldb-add-command-option-arguments (car command) option
		  gap barf arguments)
	   (setq command (cdr command))))
	((listp option)
	 (while option
	   (apply 'shelldb-add-command-option-arguments command (car option)
		  gap barf arguments)
	   (setq option (cdr option))))
	(t
	 (let* ((old-entry (assoc command shelldb-command-arguments))
		(old-opts (nth 1 old-entry))
		(new-opts (delete option (delete (assoc option old-opts)
						 old-opts))))
	   (setq shelldb-command-arguments
		 (cons (list command new-opts (nth 2 old-entry))
		       (delete old-entry shelldb-command-arguments)))
	   (shelldb-add-command-options
	    command (list (append (list option gap barf) arguments)))))))


(defun shelldb-add-command-arguments (command arguments)
  "For COMMAND, use ARGUMENTS for completion.
If COMMAND is a list, use for each command in the list represented by COMMAND.
If ARGUMENTS is a string, use the arguments for the command ARGUMENTS.

For example:
 (shelldb-add-command-arguments \"cd\"
  '(nil nil \"dirname\" filespec nil nil nil t nil))

 (shelldb-add-command-arguments \"mail\"
  (list nil (shelldb-last-argument \"\\\\S +@\\\\S *\") \"mailname\" 'mailname))

 (shelldb-add-command-arguments \"mail\"
  (list (shelldb-last-argument \"\\\\S +@\\\\S *\") nil \"hostname\" 'hostname))

 (shelldb-add-command-arguments \"xon\"
  (list (shelldb-argumentn 1) nil \"command\" 'hostname))

 (shelldb-add-command-arguments \"xon\"
  (list nil (shelldb-option-preceding-argument '(\"-name\" \"-screen\" \"-user\"))
        \"command\" 'command))

 (shelldb-add-command-arguments \"ypcat\"
  '(nil nil \"NIS name\" shell-string \"ypcat -x\" \"^\\\\S +\\\\s +\\\"\\\\(\\\\S +\\\\)\\\"\" 1))

See `shelldb-command-arguments' for a description of the format of ARGUMENTS.

See `shelldb-add-command-options' and `shelldb-add-command-option-arguments'."
  (cond ((listp command)
	 (while command
	   (shelldb-add-command-arguments (car command) arguments)
	   (setq command (cdr command))))
	((stringp arguments)
	 (let ((arguments (nth 2 (assoc arguments shelldb-command-arguments))))
	   (while arguments
	     (shelldb-add-command-arguments command (car arguments))
	     (setq arguments (cdr arguments)))))
	(t
	 (let* ((old-entry (assoc command shelldb-command-arguments))
		(old-opts (nth 1 old-entry)) (old-args (nth 2 old-entry)))
	   (setq shelldb-command-arguments
		 (cons (list command old-opts (cons arguments old-args))
		       (delete old-entry shelldb-command-arguments)))))))


(defun shelldb-add-general-option-arguments (option gap &rest arguments)
  "For each command with OPTION, use ARGUMENTS for option completion.

For example:
 (shelldb-add-general-option-arguments \"-display\" t \"hostname\" 'hostname)

And maybe even:
 (shelldb-add-general-option-arguments \"-I\" nil
  \"dirname\" 'filespec nil nil \"^-I\" t nil)

See `shelldb-add-command-options' and `shelldb-command-arguments'."
  (let ((shelldb-command-args shelldb-command-arguments) (cmds '()))
    ;; We make a list of commands, rather than calling within the loop, as we
    ;; don't want to delete the list while traversing it.
    (while shelldb-command-args
      (if (member option (nth 1 (car shelldb-command-args)))
	  (setq cmds (cons (nth 0 (car shelldb-command-args)) cmds)))
      (setq shelldb-command-args (cdr shelldb-command-args)))
    (apply 'shelldb-add-command-option-arguments cmds option gap arguments)))


(defun shelldb-remove-command (command)
  "Remove COMMAND's completion entry.
If COMMAND is a list, do for each command in the list represented by COMMAND.
See `shelldb-add-command-options' and `shelldb-command-arguments'."
  (if (listp command)
      (while command
	(shelldb-remove-command (car command))
	(setq command (cdr command)))
    (setq shelldb-command-arguments
	  (delete (assoc command shelldb-command-arguments)
		  shelldb-command-arguments))))


(defun shelldb-remove-command-option (command option)
  "Remove COMMAND's OPTION completion entry.
If COMMAND is a list, do for each command in the list represented by COMMAND.
If OPTION is a list, use each option in the list represented by OPTION.
See `shelldb-add-command-options' and `shelldb-command-arguments'."
  (cond ((listp command)
	 (while command
	   (shelldb-remove-command-option (car command) option)
	   (setq command (cdr command))))
	 ((listp option)
	  (while option
	    (shelldb-remove-command-option command (car option))
	    (setq option (cdr option))))
	 (t
	  (let* ((entry (assoc command shelldb-command-arguments))
		 (options (nth 1 entry)) (arguments (nth 2 entry))
		 (new-opts (delete option (delete (assoc option options)
						  options))))
	    (setq shelldb-command-arguments
		  (cons (list command new-opts arguments)
			(delete entry shelldb-command-arguments)))))))

;;; Functions to mess about with databases.

(defsubst shelldb-symbol (db) (intern (concat "shelldb-" (symbol-name db))))

(defun shelldb-make (db &optional value)
  "Make the database DB.  Initialises the DB to VALUE or nil."
  (set (shelldb-symbol db) value))

(defun shelldb-initialise (db form)
  "Make the database DB initialise by evaluating FORM."
  (eval (list 'defun (intern (concat "build-" (symbol-name db))) nil
	      (list 'setq (shelldb-symbol db) form))))

(defun shelldb-db (db)
  "Return the database DB.
If the value of the database is nil, attempt to build it by calling the
function `build-DB'."
  (let* ((name (symbol-name db))
	 (symbol (intern-soft (concat "shelldb-" name))))
    (or (symbol-value (if (boundp symbol) symbol))
	(funcall (or (intern-soft (concat "build-" name)) 'ignore))
	(symbol-value (if (boundp symbol) symbol)))))

(defun shelldb-elements (db)
  "Return the elements of database DB.
If the database contains items of the form \"element=value\", then only the
\"element\" part is returned.
Otherwise, this function is the same as `shelldb-db'."
  (mapcar (function (lambda (e) (substring e 0 (string-match "=" e))))
	  (shelldb-db db)))

(defun shelldb-set (db variable &optional value)
  "In database DB, set the value of the named VARIABLE to VALUE.
If VALUE is not provided or nil, the VARIABLE is removed from the DB.
See `shelldb-get' and `process-environment' for the database element format."
  (let* ((db (shelldb-symbol db))
	 (process-environment (symbol-value db)))
    (setenv variable value)
    (set db process-environment)))

(defun shelldb-get (db variable)
  "In database DB, return the value of the named VARIABLE as a string.
See `shelldb-set'."
  (let* ((db (shelldb-symbol db))
	 (process-environment (symbol-value db)))
    (getenv variable)))

(defun shelldb-add (db item)
  "In database DB, add ITEM.
ITEM is only added if not already present.  If ITEM is a list, add each element
of the list."
  (let ((item (if (listp item) item (list item)))
	(db-name (shelldb-symbol db)))
    (set db-name (uniq (sort (nconc item (shelldb-db db)) 'string-lessp)
		       'string-equal))))

(defun shelldb-remove (db item)
  "In database DB, remove ITEM.
If ITEM is a list, remove each element of the list."
  (let ((item (if (listp item) item (list item)))
	(db-name (shelldb-symbol db)))
    (while item
      (set db-name (delete (car item) (shelldb-db db)))
      (setq item (cdr item)))))

;;; Functions to generate regexps matching options and arguments.

(defun shelldb-option-preceding-argument (&optional opt arg)
  "Return regexp option OPT preceding last argument ARG.
If OPT and/or ARG nil, use any non-whitespace string.
If OPT and/or ARG is a list, use each item in the list.
See `shelldb-add-command-options'."
  (cond ((and opt (listp opt))
	 (mapconcat (function (lambda (o)
			(shelldb-option-preceding-argument o arg))) opt "\\|"))
	((and arg (listp arg))
	 (mapconcat (function (lambda (a)
			(shelldb-option-preceding-argument opt a))) arg "\\|"))
	(t
	 (concat "\\s +" (or opt "-\\S +") (shelldb-last-argument arg)))))

(defun shelldb-last-argument (&optional arg)
  "Return regexp ARG as last argument.
If ARG nil, use any non-whitespace string.
If ARG is a list, use each item in the list.
See `shelldb-add-command-options'."
  (if (or (null arg) (stringp arg))
      (concat "\\s +" (or arg "\\S *") "$")
    (mapconcat (function (lambda (o) (shelldb-last-argument o))) arg "\\|")))

(defun shelldb-argumentn (n)
  "Return regexp matching N arguments.
The last argument may be empty, but must be preceded by whitespace.
See `shelldb-add-command-options'."
  (concat "^" (mapconcat 'identity (make-list n "\\S +\\s +") "") "\\S *$"))

;;; Misc fns.

(defun shelldb-submit-feedback (command)
  "Sumbit feedback on the shelldb package by electronic mail."
  (interactive "sRelevant shell command (RET if none)? ")
  (require 'reporter)
  (reporter-submit-bug-report
   "Simon.Marshall@mail.esrin.esa.it" (concat "shelldb-" shelldb-version) nil
   nil nil (if (string-equal command "") nil (concat "Entry for " command)))
  ;(let ((entry (assoc command shelldb-command-arguments)))
  ;  (reporter-dump-variable 'entry (current-buffer)))
  )

(defun shelldb-word (word-chars prefix-regexp)
  "Return the word of WORD-CHARS at point after PREFIX-REGEXP.
See `comint-word'."
  (save-match-data
    (let ((word (or (comint-word word-chars) "")))
      (if (and (stringp prefix-regexp) (string-match prefix-regexp word))
	  (substring word (match-end 0))
	word))))

(defun shelldb-input ()
  "Return the input before point.
See `shell-backward-command'."
  (buffer-substring
   (save-excursion (shell-backward-command 1) (point)) (point)))

(defun shelldb-command (file)
  "Return basename of FILE.
Strips off directory name, and leading \"\\\"."
  (save-match-data
    (file-name-nondirectory (substring file (string-match "[^\\]" file)))))


(defun shelldb-files-matching-filespec (dir incl excl dir-p exec-p)
  "Return list of files in DIR matching INCL but not EXCL.
If INCL and/or EXCL are nil, respective restrictions are not imposed.
If DIR-P non-nil, return directories only.
If EXEC-P non-nil, return executables only."
  (let ((incl-files (delete "." (delete ".." (directory-files dir))))
	(path (file-name-as-directory dir))
	(fign
	 (and comint-completion-fignore
	      (mapconcat (function (lambda (x) (concat (regexp-quote x) "$")))
			 comint-completion-fignore "\\|")))
	(files '()) (file ""))
    (while incl-files
      (setq file (car incl-files) incl-files (cdr incl-files))
      (if (or (file-directory-p (concat path file))
	      (and (or (null incl) (string-match incl file))
		   (or (null excl) (not (string-match excl file)))
		   (or (null fign) (not (string-match fign file)))
		   (not dir-p)
		   (or (null exec-p) (file-executable-p (concat path file)))))
	  (setq files (cons file files))))
    files))


;;;(defun shelldb-shell-strings (command match &optional shell)
;;;  "Return a list of strings by executing COMMAND.
;;;The regexp MATCH should match each successive string to be retrieved.
;;;Uses \"/bin/sh\" unless SHELL is given."
;;;  (save-window-excursion
;;;    (let ((default-major-mode 'fundamental-mode)
;;;	  (strings '()) (buf " *shell-strings*"))
;;;      (buffer-disable-undo (set-buffer (get-buffer-create buf)))
;;;      (erase-buffer)
;;;      (call-process (or shell "/bin/sh") nil t nil "-c" command)
;;;      (goto-char (point-min))
;;;      (while (re-search-forward match nil t)
;;;	(setq strings (cons (current-word) strings)))
;;;      (kill-buffer buf)
;;;      (unique strings 'delete))))


(defun shelldb-shell-strings (command regexp subexp &optional shell)
  "Return a list of strings by executing COMMAND in SHELL.
For each line, if REGEXP matches the line, the buffer substring matching SUBEXP
is added to the list.
If SHELL nil, \"/bin/sh\" is used.  See also `match-string'."
  (save-window-excursion
    (let ((default-major-mode 'fundamental-mode)
	  (strings '()) (buf " *shell-strings*"))
      (buffer-disable-undo (set-buffer (get-buffer-create buf)))
      (erase-buffer)
      (call-process (or shell "/bin/sh") nil t nil "-c" command)
      (goto-char (point-min))
      (while (and (= (forward-line 1) 0) (not (eobp)))
	(if (looking-at regexp)
	    (setq strings (cons (match-string subexp) strings))))
      (kill-buffer buf)
      (uniq (sort strings 'string-lessp) 'string-equal))))

;;; Really aught to be part of emacs?
(or (fboundp 'match-string)
    (defun match-string (n &optional string)
      "Return the matched grouping N from STRING.
If STRING is not given, use the current buffer.  See `string-match'."
      (if (stringp string)
	  (substring string (match-beginning n) (match-end n))
	(buffer-substring (match-beginning n) (match-end n)))))

(or (fboundp 'uniq)
    (defun uniq (list predicate)
      "Uniquify LIST, comparing adjacent elements using PREDICATE.
Return the list with adjacent duplicate items removed by side effects.
PREDICATE is called with two elements of LIST, and should return non-nil if the
first element is \"equal to\" the second.
This function will only work as expected if LIST is sorted, as with the Un*x
command of the same name.  See also `sort'."
      (let ((list list))
	(while list
	  (while (funcall predicate (car list) (nth 1 list))
	    (setcdr list (nthcdr 2 list)))
	  (setq list (cdr list))))
      list))

;;; Default databases.

(defvar shelldb-hostnames nil
  "List of host names.
Items in the list are of the form \"hostname\".

Note that these are generated by looking at /etc/hosts (directly and ypcat).")

(defun build-hostnames ()
  (let ((hosts
	 (append
	  (shelldb-shell-strings
	   "(ypcat hosts; cat /etc/hosts) 2>/dev/null"
	   "\\s *[0-9][0-9.]*\\s +\\([a-zA-Z]\\S *\\)" 1)
	  (shelldb-shell-strings
	   (concat "cat " (expand-file-name "~/.netrc") " 2>/dev/null")
	   "machine\\s +\\(\\S +\\)" 1)
	  (shelldb-shell-strings
	   (concat "cat " (expand-file-name "~/.ftptoolrc") " 2>/dev/null")
	   "host:\\s *\\(\\S +\\)" 1))))
    (setq shelldb-hostnames (uniq (sort hosts 'string-lessp) 'string-equal))))

(defvar shelldb-usernames nil
  "List of user names.
Items in the list are of the form \"username\".

Note that these are generated by looking at /etc/passwd (directly and ypcat).")

(defun build-usernames ()
  (setq shelldb-usernames
	(shelldb-shell-strings "(ypcat passwd; cat /etc/passwd) 2>/dev/null"
	 "^[^+][^:]*" 0)))

(defvar shelldb-shell-variables nil
  "List of shell variable names.
Items in the list are of the form \"shellvariable\".

Note that these are generated by looking at the output from \"set\".")

(defun build-shell-variables ()
  (setq shelldb-shell-variables
	(mapcar (function (lambda (s)
		    (if (not (string-match "\\s +" s))
			(concat s "=")
		      (concat (substring s 0 (match-beginning 0)) "="
			      (substring s (match-end 0))))))
		(shelldb-shell-strings "set" "^\\s *\\(.*\\)$" 0
				       (or explicit-shell-file-name
					   (getenv "ESHELL")
					   (getenv "SHELL"))))))

(defvar shelldb-environment-variables nil
  "List of environment variable names.
Items in the list are of the form \"envvariable=value\".

Note that these are generated by looking at the output from \"env\".")

(defun build-environment-variables ()
  (setq shelldb-environment-variables
	(shelldb-shell-strings "env" "^\\s *\\(.*\\)$" 1
	 (or explicit-shell-file-name (getenv "ESHELL") (getenv "SHELL")))))

(defvar shelldb-command-builtins
  (let ((shell (or explicit-shell-file-name (getenv "ESHELL") (getenv "SHELL")
		   "/bin/sh")))
    (cond ((string-match "/sh$" shell) nil)
	  ((string-match "/bash$" shell)
	   '("alias" "bg" "bind" "break" "builtin" "cd" "command" "continue"
	     "declare" "typeset" "dirs" "echo" "enable" "eval" "exec" "exit"
	     "bye" "export" "fc" "fg" "getopts" "hash" "help" "history" "jobs"
	     "kill" "let" "local" "logout" "popd" "pushd" "pwd" "read"
	     "readonly" "return" "set" "shift" "suspend" "test" "times" "trap"
	     "type" "ulimit" "umask" "unalias" "unset" "wait"))
	  ((string-match "/tcsh$" shell)
	   '("ls-F" "which" "where" "echotc" "complete" "uncomplete"
	     ;; Guessed:
	     "alias" "bg" "cd" "continue" "echo" "eval" "exec" "exit" "fg"
	     "history" "jobs" "logout" "popd" "pushd" "pwd" "set" "ulimit"
	     "umask" "unalias" "unset" "wait"))
	  (t
	   '(;; Guessed:
	     "alias" "bg" "cd" "continue" "echo" "eval" "exec" "exit" "fg"
	     "history" "jobs" "logout" "popd" "pushd" "pwd" "set" "ulimit"
	     "umask" "unalias" "unset" "wait"))))
  "List of command builtins.
Items in the list are of the form \"builtin\".")

(defvar shelldb-command-aliases nil
  "List of command aliases.
Items in the list are of the form \"alias\".

Note that these are generated by looking at the output from \"alias\".")

(defun build-command-aliases ()
  (setq shelldb-command-aliases
	(shelldb-shell-strings "alias" "^\\s *\\(\\S +\\)" 1
	 (or explicit-shell-file-name (getenv "ESHELL") (getenv "SHELL")))))

(make-local-variable 'shelldb-shell-variables)
(make-local-variable 'shelldb-environment-variables)
(make-local-variable 'shelldb-command-aliases)

;;; Shell scripts I used to get the command/options from manpages.
;;; The list was produced by ./extract-options-from-manpages > command-options
;;; then evaled the sexp in the file command-options.

;;; The executable ./extract-options-from-manpages:
;;; #! /bin/sh
;;; 
;;; #MANPATH=$HOME/Slash/usr/man
;;; tmpfile=/tmp/fubar.$$
;;; 
;;; /bin/rm -f $tmpfile
;;; touch $tmpfile
;;; 
;;; echo "Starting manpages in $MANPATH" 1>&2
;;; for manpagedir in `echo $MANPATH | sed 's/:/ /g'`; do
;;; 	for manpage in $manpagedir/*/*; do
;;; 		echo "$manpage..." 1>&2
;;; 		extract-options-from-manpage $manpage >> $tmpfile
;;; 	done
;;; done
;;; echo "Finished manpages in $MANPATH" 1>&2
;;; 
;;; echo ";; list with elements of the form (\"command\" . (\"option1\" ...))"
;;; echo "(setq options (list"
;;; cat $tmpfile
;;; #sort -u -z1048576 $tmpfile
;;; echo " ))"
;;; 
;;; /bin/rm -f $tmpfile
;;; The executable ./extract-options-from-manpage:
;;; #! /bin/sh
;;; 
;;; if test $# -ne 1; then
;;; 	echo "Usage: "`basename $0`" command"
;;; 	exit
;;; fi
;;; 
;;; # Strip off trailing `.suffix'.
;;; command=`basename $1 | sed 's/\..*$//'`
;;; 
;;; # Get the options out.
;;; options=`man $command | egrep '^[ \t]*-' \
;;; 	| awk '{ for (i = 1; i <= NF; ++i) print $i }' \
;;; 	| egrep -v '^[^-]' \
;;; 	| sed 's/^\(-[a-zA-z0-9+-]*\).*/\"\1\"/' \
;;; 	| sed 's/\[//g' | sed 's/\]//g' | sed 's/_\"$/\"/' \
;;; 	| egrep -v '^"[-+]*"$'| egrep -v '^"\-[0-9]*"$' \
;;; 	| egrep -v '^"-HUGE"$'| egrep -v '^"-fnonstd"$' \
;;; 	| sort | uniq`
;;; 
;;; # Format them for emacs lisp.
;;; if test `echo $options | wc -w` -gt 0; then
;;; 	echo " '(\"$command\" ("$options") nil)"
;;; fi

;;; Functions I used to insert the command/option list.  I put point where I
;;; wanted the list to begin, and evaled
;;; (setq shelldb-command-arguments options)
;;; (shelldb-remove-command (shelldb-single-letter-options))
;;; (shelldb-insert-options shelldb-command-arguments)

;;;(defun shelldb-insert-options (options)
;;;  "Insert OPTIONS and indent etc."
;;;  (let ((list (uniq (sort options 'string-lessp) 'string-equal)))
;;;    (insert "'(")
;;;    (while list
;;;      (shelldb-insert (car list))
;;;      (sit-for 0)
;;;      (setq list (cdr list)))
;;;    (newline-and-indent)
;;;    (insert ")\n")))
;;;
;;;(defun shelldb-insert (options)
;;;  (cond ((and options (listp options))
;;;	 (if (/= (preceding-char) ?\()
;;;	     (newline-and-indent))
;;;	 (insert "(")
;;;	 (while options
;;;	   (shelldb-insert (car options))
;;;	   (setq options (cdr options)))
;;;	 (delete-horizontal-space)
;;;	 (insert ") "))
;;;	(t
;;;	 (let ((string (prin1-to-string options)))
;;;	   (if (> (+ (current-column) (length string)) 75)
;;;	       (newline-and-indent))
;;;	   (insert string " ")))))
;;;
;;;(defun shelldb-single-letter-options ()
;;;  "Return a list of all commands with single letter arguments only.
;;;Those commands that have option arguments or general command arguments are
;;;not included."
;;;  (let ((f (function (lambda (i) (= (length i) 2)))))
;;;    (delete nil
;;;     (mapcar (function (lambda (entry)
;;;			 (if (and (eval (cons 'and (mapcar f (nth 1 entry))))
;;;				  (null (nth 2 entry)))
;;;			     (nth 0 entry))))
;;;	     shelldb-command-arguments))))

;;; Don't look further unless you feel brave...

(defvar shelldb-command-arguments
;; Created by inserting the shell-created list of commands and options...
'(("Xserver"
   ("-I" "-a" "-ac" "-auth" "-broadcast" "-bs" "-c" "-cc" "-class" "-co"
    "-cookie" "-displayID" "-dpi" "-f" "-fc" "-fn" "-fp" "-help"
    "-indirect" "-ld" "-lf" "-logo" "-ls" "-once" "-p" "-port" "-query"
    "-r" "-s" "-su" "-t" "-to" "-v" "-wm" "-x") nil)
  ("Xsun"
   ("-ar1" "-ar2" "-debug" "-dev" "-mono" "-zaphod") nil)
  ("XsunMono"
   ("-ar1" "-ar2" "-debug" "-dev" "-mono" "-zaphod") nil)
  ("acc"
   ("-B" "-Bdynamic" "-C" "-D" "-E" "-H" "-I" "-L" "-M" "-O" "-P" "-PIC"
    "-Qdir" "-Qoption" "-Qpath" "-Qproduce" "-R" "-S" "-U" "-V" "-X" "-Xa"
    "-Xc" "-Xs" "-Xt" "-a" "-bsdmalloc" "-c" "-cg87" "-cg89" "-cg92"
    "-dalign" "-dryrun" "-fast" "-fsingle" "-g" "-help" "-keeptmp" "-l"
    "-lc" "-libmieee" "-libmil" "-misalign" "-native" "-nolib" "-nolibmil"
    "-noqueue" "-o" "-p" "-pg" "-pic" "-qdir" "-qoption" "-qpath"
    "-qproduce" "-s" "-sb" "-sbfast" "-strconst" "-sys5" "-temp" "-time"
    "-u" "-unroll" "-v" "-vc" "-w" "-xlicinfo") nil)
  ("acct"
   ("-p" "-print" "-u") nil)
  ("acctdisk"
   ("-p" "-print" "-u") nil)
  ("acctdusg"
   ("-p" "-print" "-u") nil)
  ("acctwtmp"
   ("-p" "-print" "-u") nil)
  ("admin"
   ("-a" "-b" "-d" "-e" "-f" "-h" "-i" "-l" "-la" "-m" "-n" "-r" "-t" "-y"
    "-z") nil)
  ("alias" nil
   (("^\\S +\\s +\\S *$" nil "command alias" alias)))
  ("alint"
   ("-C" "-D" "-F" "-I" "-L" "-O" "-R" "-U" "-V" "-W" "-Xa" "-Xc" "-Xs"
    "-Xt" "-a" "-b" "-c" "-g" "-h" "-k" "-l" "-m" "-n" "-o" "-p" "-s" "-u"
    "-v" "-x" "-y") nil)
  ("arc"
   ("-l300" "-l300s" "-l4014" "-l450" "-lplot" "-lplot2648" "-lplot7221"
    "-lplotaed" "-lplotbg" "-lplotdumb" "-lplotgigi" "-lplotimagen") nil)
  ("as"
   ("-D" "-I" "-J" "-L" "-O" "-P" "-R" "-SC" "-U" "-V" "-d2" "-h" "-i386"
    "-j" "-k" "-o" "-pic") nil)
  ("atobm"
   ("-bd" "-bg" "-bw" "-chars" "-fg" "-fn" "-geometry" "-help" "-hl" "-ms"
    "-name" "-nodashed" "-xhot" "-yhot"
    ("-display" t "hostname" hostname)) nil)
  ("attraction"
   ("-color-mode" "-color-shift" "-delay" "-glow" "-mode" "-mono" "-noglow"
    "-orbit" "-points" "-radius" "-root" "-segments" "-size" "-threshold"
    "-vmult" "-vx" "-vy" "-window") nil)
  ("automount"
   ("-D" "-M" "-T" "-f" "-hosts" "-m" "-n" "-null" "-tl" "-tm" "-tw" "-v")
   nil)
  ("bash"
   ("-C" "-G" "-H" "-L" "-O" "-S" "-a" "-b" "-c" "-d" "-e" "-f" "-g" "-h"
    "-i" "-k" "-l" "-login" "-m" "-n" "-nobraceexpansion" "-nolineediting"
    "-noprofile" "-norc" "-o" "-p" "-path" "-posix" "-q" "-quiet" "-r"
    "-rcfile" "-s" "-t" "-u" "-v" "-version" "-w" "-x" "-z") nil)
  ("bdraw"
   ("-cbreak") nil)
  ("binder"
   ("-network" "-system" "-user") nil)
  ("bitmap"
   ("-bd" "-bg" "-bw" "-chars" "-fg" "-fn" "-geometry" "-help" "-hl" "-ms"
    "-name" "-nodashed" "-xhot" "-yhot"
    ("-display" t "hostname" hostname)) nil)
  ("blitspin"
   ("-bitmap" "-delay" "-delay2" "-mono" "-root" "-window") nil)
  ("bmtoa"
   ("-bd" "-bg" "-bw" "-chars" "-fg" "-fn" "-geometry" "-help" "-hl" "-ms"
    "-name" "-nodashed" "-xhot" "-yhot"
    ("-display" t "hostname" hostname)) nil)
  ("bsd"
   ("-lbsd") nil)
  ("calctool"
   ("-Wn" "-a" "-c" "-l" "-m" "-r" "-v") nil)
  ("canvas"
   ("-anchor" "-arrow" "-arrowshape" "-background" "-bitmap" "-capstyle"
    "-colormap" "-colormode" "-extent" "-file" "-fill" "-font" "-fontmap"
    "-foreground" "-height" "-joinstyle" "-justify" "-outline"
    "-pageanchor" "-pageheight" "-pagewidth" "-pagex" "-pagey" "-rotate"
    "-smooth" "-splinesteps" "-start" "-stipple" "-style" "-tags" "-text"
    "-width" "-window" "-x" "-y") nil)
  ("cc"
   ("-B" "-C" "-D" "-E" "-J" "-M" "-O" "-P" "-PIC" "-Qoption" "-Qpath"
    "-Qproduce" "-R" "-S" "-U" "-a" "-align" "-c" "-dalign" "-dryrun"
    "-f68881" "-ffpa" "-fsingle" "-fsky" "-fsoft" "-fstore" "-fswitch" "-g"
    "-go" "-help" "-l" "-misalign" "-o" "-p" "-pg" "-pic" "-pipe" "-sb"
    "-target" "-temp" "-time" "-w"
    ("-I" nil "dirname" filespec nil nil "^-I" t nil)
    ("-L" nil "dirname" filespec nil nil "^-L" t nil)) nil)
  ("cd" nil
   ((nil "\\s +\\([0-9+-]+\\|\\S *~[^/]*\\)$" "dirname" filespec nil nil
	 nil t nil)))
  ("cdraw"
   ("-cbreak") nil)
  ("ce_db_build"
   ("-db_file" "-from_ascii" "-to_ascii") nil)
  ("ce_db_merge"
   ("-db_file" "-from_ascii") nil)
  ("cflow"
   ("-d" "-i" "-ix" "-r") nil)
  ("circle"
   ("-l300" "-l300s" "-l4014" "-l450" "-lplot" "-lplot2648" "-lplot7221"
    "-lplotaed" "-lplotbg" "-lplotdumb" "-lplotgigi" "-lplotimagen") nil)
  ("closepl"
   ("-l300" "-l300s" "-l4014" "-l450" "-lplot" "-lplot2648" "-lplot7221"
    "-lplotaed" "-lplotbg" "-lplotdumb" "-lplotgigi" "-lplotimagen") nil)
  ("cmap_alloc"
   ("-allscreens" "-depth" "-force" "-help" "-verbose" "-visual"
    ("-display" t "hostname" hostname)) nil)
  ("cmap_compact"
   (("-display" t "hostname" hostname)) nil)
  ("compress"
   ("-b" "-c" "-f" "-v")
   ((nil "\\s +-\\S *$" "filename" filespec nil "\\(\\.gz\\|\\.Z\\)$" nil
	 nil nil)))
  ("cont"
   ("-l300" "-l300s" "-l4014" "-l450" "-lplot" "-lplot2648" "-lplot7221"
    "-lplotaed" "-lplotbg" "-lplotdumb" "-lplotgigi" "-lplotimagen") nil)
  ("convertfont"
   ("-M" "-S" "-a" "-b" "-c" "-d" "-f" "-n" "-o" "-s" "-t" "-ta" "-tv" "-v"
    "-vf" "-x") nil)
  ("cpp"
   ("-B" "-C" "-D" "-H" "-M" "-P" "-R" "-T" "-U" "-Y" "-p" "-undef"
    ("-I" nil "dirname" filespec nil nil "^-I" t nil)
    ("-L" nil "dirname" filespec nil nil "^-L" t nil)) nil)
  ("ctags"
   ("--append" "--backward-search" "--c++" "--cxref" "--defines"
    "--forward-search" "--help" "--ignore-indentation" "--include"
    "--no-defines" "--no-warn" "--output" "--typedefs" "--typedefs-and-c++"
    "--update" "--version" "--vgrind" "-B" "-C" "-D" "-F" "-H" "-S" "-T"
    "-V" "-a" "-d" "-i" "-o" "-t" "-u" "-v" "-w" "-x") nil)
  ("dbx"
   ("-I" "-c" "-e" "-i" "-kbd" "-q" "-r" "-s" "-sr")
   ((nil "\\s +-\\S *$" "command" command)))
  ("dbxtool"
   ("-I" "-d" "-i" "-k" "-kbd") nil)
  ("debugger"
   ("-I" "-Wfsdb" "-kbd") nil)
  ("decayscreen"
   ("-delay" "-mono" "-root" "-window") nil)
  ("dis"
   ("-F" "-L" "-V" "-d" "-da" "-g" "-l" "-o" "-t") nil)
  ("dkctl"
   ("-wchk") nil)
  ("dos"
   ("-b" "-c" "-p" "-q" "-s" "-update" "-w") nil)
  ("dos2unix"
   ("-ascii" "-iso") nil)
  ("draw"
   ("-cbreak") nil)
  ("dumpfont"
   ("-M" "-S" "-a" "-b" "-c" "-d" "-f" "-n" "-o" "-s" "-t" "-ta" "-tv" "-v"
    "-vf" "-x") nil)
  ("emacs"
   ("-b" "-batch" "-bd" "-bg" "-cr" "-d" "-f" "-fg" "-fn" "-font"
    "-geometry" "-i" "-ib" "-kill" "-l" "-ms" "-name" "-nw" "-q" "-r" "-rn"
    "-t" "-u"
    ("-display" t "hostname" hostname)) nil)
  ("eqnchar"
   ("-wig") nil)
  ("etags"
   ("--append" "--backward-search" "--c++" "--cxref" "--defines"
    "--forward-search" "--help" "--ignore-indentation" "--include"
    "--no-defines" "--no-warn" "--output" "--typedefs" "--typedefs-and-c++"
    "--update" "--version" "--vgrind" "-B" "-C" "-D" "-F" "-H" "-S" "-T"
    "-V" "-a" "-d" "-i" "-o" "-t" "-u" "-v" "-w" "-x") nil)
  ("etherfind"
   ("-apple" "-c" "-d" "-decnet" "-i" "-ip" "-l" "-n" "-p" "-proto" "-r"
    "-t" "-u" "-v" "-x") nil)
  ("exec" nil
   ((nil "\\s +-\\S *$" "command" command)))
  ("extract_patch"
   ("-DEFAULT" "-d" "-p" "-r") nil)
  ("extract_unbundled"
   ("-DEFAULT" "-d" "-r") nil)
  ("f77"
   ("-B" "-Bdynamic" "-Bstatic" "-C" "-D" "-F" "-I" "-J" "-L" "-Nc"
    "-Ncdlnqsx" "-Nd" "-Nl" "-Nn" "-Nq" "-Ns" "-Nx" "-O" "-O1" "-O1234"
    "-O2" "-O3" "-O4" "-PIC" "-Qoption" "-Qpath" "-Qproduce" "-S" "-U" "-V"
    "-a" "-align" "-ansi" "-c" "-cg" "-cg87" "-cg89" "-dalign" "-dryrun"
    "-e" "-f" "-f68881" "-fast" "-ffpa" "-ffpaplus" "-fsoft" "-fstore"
    "-fswitch" "-g" "-help" "-i2" "-i4" "-l" "-libmil" "-misalign"
    "-native" "-nolibmil" "-o" "-onetrip" "-p" "-pg" "-pic" "-pipe" "-r4"
    "-r8" "-sb" "-temp" "-time" "-u" "-v" "-w" "-w66" "-xld") nil)
  ("filemerge"
   ("-a" "-b" "-f1" "-f2" "-l" "-r") nil)
  ("find"
   ("-cpio" "-depth" "-gilds" "-ls" "-name" "-ncpio" "-newer" "-nogroup"
    "-nouser" "-print" "-prune" "-xdev"
    ("-atime" t "day count")
    ("-ctime" t "day count")
    ("-exec" t "command" command)
    ("-fstype" t "file system type"
     ("nfs" "4.2"))
    ("-group" t "group name" shell-string "ypcat group; cat /etc/group"
     "^[^+][^:]*" 0)
    ("-inum" t "inode number")
    ("-links" t "link count")
    ("-mtime" t "day count")
    ("-ok" t "command" command)
    ("-perm" t "permission flag")
    ("-size" t "size")
    ("-type" t "file system type"
     ("b" "c" "d" "f" "p" "l" "s"))
    ("-user" t "username" username)) nil)
  ("finger"
   ("-b" "-f" "-h" "-i" "-l" "-m" "-p" "-q" "-s" "-w")
   ((nil "\\s +\\(-\\S *\\|\\S *@\\S *\\)$" "username" username)))
  ("fish_props"
   ("-bn" "-d" "-fnn" "-in" "-rn" "-s") nil)
  ("flame"
   ("-delay" "-delay2" "-iterations" "-mono" "-ncolors" "-points" "-root"
    "-window") nil)
  ("foption"
   ("-f" "-fswitch") nil)
  ("fparel"
   ("-p" "-p0" "-v") nil)
  ("fpversion"
   ("-foption") nil)
  ("frame"
   ("-geometry") nil)
  ("fs"
   ("-config" "-ls" "-port") nil)
  ("fslsfonts"
   ("-C" "-ll[l]" "-m" "-n" "-server" "-u" "-w") nil)
  ("fstobdf"
   ("-fn" "-server") nil)
  ("ftp"
   ("-d" "-g" "-i" "-n" "-t" "-v")
   ((nil "\\s +-\\S *$" "hostname" hostname)))
  ("gcc"
   ("-A" "-B" "-C" "-D" "-E" "-G" "-H" "-I-" "-M" "-MD" "-MM" "-MMD" "-O"
    "-O0" "-O1" "-O2" "-P" "-Qn" "-Qy" "-S" "-U" "-V" "-W" "-Wa"
    "-Waggregate-return" "-Wall" "-Wcast-align" "-Wcast-qual"
    "-Wchar-subscript" "-Wchar-subscripts" "-Wcomment" "-Wconversion"
    "-Wenum-clash" "-Werror" "-Wformat" "-Wid-clash-" "-Wimplicit"
    "-Winline" "-Wl" "-Wmissing-prototypes" "-Wnested-externs"
    "-Wno-import" "-Woverloaded-virtual" "-Wparentheses" "-Wpointer-arith"
    "-Wredundant-decls" "-Wreturn-type" "-Wshadow" "-Wstrict-prototypes"
    "-Wswitch" "-Wtemplate-debugging" "-Wtraditional" "-Wtrigraphs"
    "-Wuninitialized" "-Wunused" "-Wwrite-strings" "-Xlinker" "-YP" "-Ym"
    "-a" "-ansi" "-b" "-c" "-d" "-dD" "-dJ" "-dL" "-dM" "-dN" "-dR" "-dS"
    "-da" "-dc" "-dd" "-df" "-dg" "-dj" "-dk" "-dl" "-dm" "-dp" "-dr" "-ds"
    "-dt" "-dx" "-dy" "-f" "-fPIC" "-fall-virtual" "-fcall-saved-"
    "-fcall-used-" "-fcaller-saves" "-fcond-mismatch" "-fcse-follow-jumps"
    "-fcse-skip-blocks" "-fdelayed-branch" "-fdollars-in-identifiers"
    "-felide-constructors" "-fenum-int-equiv" "-fexpensive-optimizations"
    "-fexternal-templates" "-ffast-math" "-ffixed-" "-ffloat-store"
    "-fforce-addr" "-fforce-mem" "-finhibit-size-directive"
    "-finline-functions" "-fkeep-inline-functions" "-fmemoize-lookups"
    "-fno-asm" "-fno-builtin" "-fno-common" "-fno-default-inline"
    "-fno-defer-pop" "-fno-function-cse" "-fno-gnu-linker" "-fno-ident"
    "-fno-inline" "-fno-peephole" "-fno-signed-bitfields"
    "-fno-strict-prototype" "-fno-unsigned-bitfields" "-fnonnull-objects"
    "-fomit-frame-pointer" "-fpcc-struct-return" "-fpic" "-fpretend-float"
    "-freg-struct-return" "-freg-struct-returno" "-frerun-cse-after-loop"
    "-fsave-memoized" "-fschedule-insns" "-fschedule-insns2"
    "-fshared-data" "-fshort-double" "-fshort-enums" "-fsigned-bitfields"
    "-fsigned-char" "-fstrength-reduce" "-fsyntax-only"
    "-fthis-is-variable" "-fthread-jumps" "-funroll-all-loops"
    "-funroll-loops" "-funsigned-bitfields" "-funsigned-char"
    "-fverbose-asm" "-fvolatile" "-fvolatile-global" "-fwritable-strings"
    "-g" "-gcoff" "-gdwarf" "-gdwarf+" "-ggdb" "-gstabs" "-gstabs+"
    "-gxcoff" "-gxcoff+" "-idirafter" "-imacros" "-include" "-iprefix"
    "-iwithprefix" "-l" "-lobjc" "-m" "-m29000" "-m29050" "-m486" "-m68000"
    "-m68020" "-m68020-40" "-m68030" "-m68040" "-m68881" "-m88000"
    "-m88100" "-m88110" "-mabicalls" "-margcount" "-masm-compat"
    "-mbig-pic" "-mbitfield" "-mbw" "-mc1" "-mc2" "-mc68000" "-mc68020"
    "-mcall-lib-mul" "-mcheck-zero-division" "-mcode-align"
    "-mcomplex-addr" "-mcpu" "-mdisable-fpregs" "-mdisable-indexing" "-mdw"
    "-mepilogue" "-mfp-arg-in-fpregs" "-mfp-arg-in-gregs" "-mfp-in-toc"
    "-mfp-reg" "-mfp-regs" "-mfp32" "-mfp64" "-mfpa" "-mfpu"
    "-mfull-fp-blocks" "-mg" "-mgas" "-mgnu" "-mgpopt" "-mhalf-pic"
    "-mhandle-large-shift" "-mhard-float" "-mhc-struct-return"
    "-mic-compat" "-mic2" "-mic3" "-midentify-revision" "-min-line-mul"
    "-mint64" "-mintel-asm" "-mips2" "-mips3" "-mkernel"
    "-mkernel-registers" "-mlarge" "-mleaf-procedures" "-mlong-calls"
    "-mlong64" "-mlonglong128" "-mmemcpy" "-mminimum-fp-blocks" "-mmips-as"
    "-mmips-tfile" "-mnbw" "-mno-486" "-mno-abicalls"
    "-mno-check-zero-division" "-mno-code-align" "-mno-complex-addr"
    "-mno-epilogue" "-mno-fop-in-toc" "-mno-fp-in-toc" "-mno-fp-regs"
    "-mno-fp-ret-in-387" "-mno-fpu" "-mno-gpopt" "-mno-half-pic"
    "-mno-leaf-procedures" "-mno-memcpy" "-mno-mips-tfile"
    "-mno-ocs-debug-info" "-mno-ocs-frame-position" "-mno-old-align"
    "-mno-optimize-arg-area" "-mno-rnames" "-mno-serialize-volatile"
    "-mno-seriazlize-volatile" "-mno-shared-libs" "-mno-soft-float"
    "-mno-stats" "-mno-strict-align" "-mno-tail-call" "-mno-underscores"
    "-mnoargcount" "-mnobitfield" "-mnodw" "-mnohc-struct-return"
    "-mnumerics" "-mocs-debug-info" "-mocs-frame-position" "-mold-align"
    "-moptimize-arg-area" "-mpa-risc-1-0" "-mpa-risc-1-1" "-mrnames"
    "-mrtd" "-mserialize-volatile" "-mshared-libs" "-mshort"
    "-mshort-data-" "-msmall" "-msoft-float" "-msparclite" "-mstack-check"
    "-mstats" "-mstrict-align" "-msvr3" "-msvr4" "-mtail-call"
    "-mtrailing-colon" "-mtrap-large-shift" "-munix"
    "-muse-div-instruction" "-muser-registers" "-mv8" "-mversion-03"
    "-mwarn-passed-structs" "-nocpp" "-nostartfiles" "-nostdinc"
    "-nostdinc++" "-nostdlib" "-o" "-p" "-pedantic" "-pedantic-errors"
    "-pg" "-pipe" "-print-libgcc-file-name" "-r" "-save-temps" "-shared"
    "-static" "-symbolic" "-traditional" "-traditional-cpp" "-trigraphs"
    "-u" "-undef" "-v" "-w" "-x" "-z"
    ("-I" nil "dirname" filespec nil nil "^-I" t nil)
    ("-L" nil "dirname" filespec nil nil "^-L" t nil)) nil)
  ("gdb" nil
   ((nil "\\s +-\\S *$" "command" command)))
  ("generic_args"
   ("-WH") nil)
  ("get"
   ("-G" "-a" "-b" "-c" "-e" "-g" "-i" "-k" "-lp" "-m" "-n" "-p" "-r" "-s"
    "-t" "-x") nil)
  ("gprof"
   ("-C" "-D" "-E" "-F" "-a" "-b" "-c" "-e" "-f" "-pg" "-s" "-xpg" "-z")
   nil)
  ("greynetic"
   ("-delay" "-mono" "-root" "-window") nil)
  ("gtconfig"
   ("-DEGAMMA8" "-E" "-G" "-I" "-M" "-c" "-d" "-degamma8" "-f" "-g" "-i"
    "-m" "-s0" "-s1" "-v" "-w") nil)
  ("gunzip"
   ("--ascii" "--best" "--decompress" "--fast" "--force" "--help"
    "--license" "--list" "--name" "--no-name" "--quiet" "--recursive"
    "--stdout" "--suffix" "--test" "--to-stdout" "--uncompress" "--verbose"
    "--version" "-L" "-N" "-S" "-V" "-a" "-c" "-d" "-f" "-h" "-l" "-n" "-q"
    "-r" "-t" "-v")
   ((nil "\\s +-\\S *$" "filename" filespec "\\(\\.gz\\|\\.Z\\)$" nil nil
	 nil nil)))
  ("gzip"
   ("--ascii" "--best" "--decompress" "--fast" "--force" "--help"
    "--license" "--list" "--name" "--no-name" "--quiet" "--recursive"
    "--stdout" "--suffix" "--test" "--to-stdout" "--uncompress" "--verbose"
    "--version" "-L" "-N" "-S" "-V" "-a" "-c" "-d" "-f" "-h" "-l" "-n" "-q"
    "-r" "-t" "-v")
   ((nil "\\s +-\\S *$" "filename" filespec nil "\\(\\.gz\\|\\.Z\\)$" nil
	 nil nil)))
  ("halo"
   ("-animate" "-count" "-delay" "-mono" "-root" "-window") nil)
  ("helix"
   ("-mono" "-root" "-window") nil)
  ("help_viewer"
   ("-A" "-dir") nil)
  ("hopalong"
   ("-count" "-delay" "-mono" "-ncolors" "-root" "-timeout" "-window") nil)
  ("hypercube"
   ("-color0" "-color1" "-color2" "-color3" "-color4" "-color5" "-color6"
    "-color7" "-delay" "-mono" "-observer-z" "-root" "-window" "-xw" "-xy"
    "-xz" "-yw" "-yz" "-zw") nil)
  ("ico"
   ("-colors" "-d" "-dbl" "-faces" "-i" "-noedges" "-obj" "-objhelp" "-r"
    "-sleep") nil)
  ("ico2"
   ("-colors" "-d" "-dbl" "-faces" "-i" "-noedges" "-obj" "-objhelp" "-r"
    "-sleep") nil)
  ("ifconfig"
   ("-arp" "-private" "-trailers") nil)
  ("imake"
   ("-D" "-DCURDIR" "-DTOPDIR" "-I" "-T" "-e" "-f" "-s" "-v") nil)
  ("imsmap"
   ("-cycle" "-iterations" "-mode" "-mono" "-ncolors" "-no-cycle" "-root"
    "-timeout" "-window") nil)
  ("indent"
   ("-T" "-bacc" "-bad" "-bap" "-bbb" "-bc" "-bl" "-br" "-bs" "-c" "-cd"
    "-cdb" "-ce" "-ci" "-cli" "-d" "-di" "-eei" "-fc1" "-i" "-ip" "-l"
    "-lc" "-lp" "-nbad" "-npcs" "-npro" "-npsl" "-nv" "-pcs" "-psl" "-sc"
    "-sob" "-st" "-troff" "-v") nil)
  ("infocmp"
   ("-A" "-B" "-C" "-I" "-L" "-V" "-c" "-d" "-n" "-r" "-sc" "-sd" "-si"
    "-sl" "-u" "-v" "-w") nil)
  ("inline"
   ("-f68881" "-ffpa" "-fsky" "-fsoft" "-fswitch" "-i" "-mc68010"
    "-mc68020" "-o" "-v" "-w") nil)
  ("join"
   ("-a" "-e" "-j1" "-o" "-t") nil)
  ("kill"
   ("-ABRT" "-ALRM" "-BUS" "-CHLD" "-CONT" "-EMT" "-FPE" "-HUP" "-ILL"
    "-INT" "-IO" "-KILL" "-LOST" "-PIPE" "-PROF" "-QUIT" "-SEGV" "-STOP"
    "-SYS" "-TERM" "-TRAP" "-TSTP" "-TTIN" "-TTOU" "-URG" "-USR1" "-USR2"
    "-VTALRM" "-WINCH" "-XCPU" "-XFSZ" "-l") nil)
  ("ld"
   ("-A" "-B" "-Bdynamic" "-Bstatic" "-Bsymbolic" "-D" "-M" "-N" "-S"
    "-Tdata" "-Ttext" "-X" "-align" "-assert" "-d" "-dc" "-dp" "-e" "-k"
    "-l" "-n" "-o" "-p" "-r" "-s" "-t" "-u" "-x" "-y" "-z"
    ("-I" nil "dirname" filespec nil nil "^-I" t nil)
    ("-L" nil "dirname" filespec nil nil "^-L" t nil)) nil)
  ("ldd"
   ("-lc") nil)
  ("lex"
   ("-Qy" "-V" "-c" "-e" "-n" "-t" "-v" "-w") nil)
  ("linemod"
   ("-l300" "-l300s" "-l4014" "-l450" "-lplot" "-lplot2648" "-lplot7221"
    "-lplotaed" "-lplotbg" "-lplotdumb" "-lplotgigi" "-lplotimagen") nil)
  ("lint"
   ("-C" "-D" "-O" "-U" "-a" "-b" "-c" "-g" "-h" "-host" "-i" "-l" "-lcore"
    "-lcurses" "-lkvm" "-llwp" "-lm" "-lmp" "-lpixrect" "-lplot" "-lport"
    "-lsuntool" "-lsunwindow" "-ltermcap" "-ltermlib" "-n" "-o" "-p" "-q"
    "-target" "-u" "-v" "-x" "-z"
    ("-I" nil "dirname" filespec nil nil "^-I" t nil)
    ("-L" nil "dirname" filespec nil nil "^-L" t nil)) nil)
  ("listen"
   ("-d" "-e" "-i" "-k" "-l" "-q" "-qz" "-r" "-s" "-t" "-v" "-x" "-z") nil)
  ("listres"
   ("-all" "-format" "-nosuper" "-top" "-variable") nil)
  ("lpq"
   ("-l"
    ("-P" nil "printer" shell-string "cat /etc/printcap"
     "^[a-zA-Z][[a-zA-Z0-9]+" 0 "^-P"))
   nil)
  ("lpr"
   ("-C" "-J" "-T" "-c" "-d" "-f" "-g" "-h" "-i" "-l" "-m" "-n" "-p" "-r"
    "-s" "-t" "-v" "-w"
    ("-P" nil "printer" shell-string "cat /etc/printcap"
     "^[a-zA-Z][[a-zA-Z0-9]+" 0 "^-P"))
   ((nil nil "filename" filespec "\\.ps$" nil nil nil nil)))
  ("mail"
   ("-F" "-H" "-N" "-T" "-U" "-d" "-e" "-f" "-h" "-i" "-n" "-r" "-s" "-u"
    "-v")
   ((nil "\\s +\\(-\\S *\\|\\S *@\\S *\\)$" "mailname" mailname)))
  ("mailtool"
   ("-Mi" "-Mx") nil)
  ("makedepend"
   ("-Dname" "-Iincludedir" "-fmakefile" "-oobjsuffix" "-sstring" "-wwidth")
   nil)
  ("maze"
   ("-S" "-d" "-dead-color" "-g" "-grid-size" "-live-color" "-post-delay"
    "-pre-delay" "-r" "-root" "-solve-delay" "-window") nil)
  ("menu"
   ("-accelerator" "-activebackground" "-background" "-bitmap" "-command"
    "-font" "-label" "-menu" "-offvalue" "-onvalue" "-state" "-underline"
    "-value" "-variable") nil)
  ("menubar"
   ("-underline") nil)
  ("mike"
   ("-c" "-d" "-g" "-l" "-m" "-n" "-p" "-q" "-r" "-s" "-s0" "-u") nil)
  ("modload"
   ("-A" "-conf" "-entry" "-exec" "-nolink" "-o") nil)
  ("modstat"
   ("-id") nil)
  ("modunload"
   ("-exec") nil)
  ("mp"
   ("-F" "-PS" "-a" "-d" "-f" "-l" "-m" "-o" "-p" "-s" "-tm" "-ts" "-v")
   nil)
  ("mpeg_play"
   ("-cb_range" "-cr_range" "-dither" "-eachstat" "-l_range" "-loop"
    "-no_display" "-nob" "-nop" "-quiet" "-shmem_off"
    ("-display" t "hostname" hostname)) nil)
  ("muncher"
   ("-geometry" "-q" "-r" "-s" "-v"
    ("-display" t "hostname" hostname)) nil)
  ("netfind"
   ("-T" "-dslf") nil)
  ("netfone"
   ("-c" "-d" "-g" "-l" "-m" "-n" "-p" "-q" "-r" "-s" "-s0" "-u") nil)
  ("newsclock"
   ("-alarm" "-date" "-face" "-hour" "-r" "-seconds" "-timezone" "-v") nil)
  ("nl"
   ("-b" "-ba" "-d" "-f" "-fa" "-h" "-ha" "-i" "-l" "-n" "-p" "-s" "-v"
    "-w") nil)
  ("nlsadmin"
   ("-d" "-e" "-i" "-k" "-l" "-q" "-qz" "-r" "-s" "-t" "-v" "-x" "-z") nil)
  ("noseguy"
   ("-filename" "-font" "-mode" "-program" "-root" "-text" "-window") nil)
  ("oclock"
   ("-backing" "-bd" "-bg" "-bw" "-fg" "-geometry" "-hour" "-jewel"
    "-minute" "-noshape"
    ("-display" t "hostname" hostname)) nil)
  ("olmh"
   ("-file") nil)
  ("olwm"
   ("-2d" "-3d" "-all" "-background" "-basiclocale" "-bd" "-bg"
    "-bordercolor" "-c" "-click" "-debug" "-depth" "-displaylang" "-f"
    "-fg" "-fn" "-follow" "-font" "-foreground" "-multi" "-name" "-numeric"
    "-orphans" "-single" "-synchronize" "-syncpid" "-syncsignal" "-visual"
    "-xrm"
    ("-display" t "hostname" hostname)) nil)
  ("openpl"
   ("-l300" "-l300s" "-l4014" "-l450" "-lplot" "-lplot2648" "-lplot7221"
    "-lplotaed" "-lplotbg" "-lplotdumb" "-lplotgigi" "-lplotimagen") nil)
  ("openwin"
   ("-nosunview") nil)
  ("options"
   ("-foreground") nil)
  ("othertools"
   ("-8bit_color_only" "-B" "-F" "-P" "-S" "-WH" "-WI" "-WL" "-WP" "-WT"
    "-Wb" "-Wf" "-Wg" "-Wh" "-Wi" "-Wl" "-Wn" "-Wp" "-Ws" "-Wt" "-Ww" "-b"
    "-background" "-d" "-f" "-i" "-k" "-m" "-n" "-overlay_only" "-p"
    "-pattern" "-s" "-toggle_enable") nil)
  ("owplaces"
   ("-all" "-ampersand" "-host" "-local" "-multi" "-output" "-pointer"
    "-remote" "-script" "-silent" "-single" "-timeout"
    ("-display" t "hostname" hostname)) nil)
  ("pageview"
   ("-aa" "-dir" "-dpi" "-h" "-left" "-mcd" "-mono" "-page" "-usage" "-v"
    "-verbose" "-w" "-width") nil)
  ("patch"
   ("--batch" "--context" "--debug" "--directory" "--ed" "--force"
    "--forward" "--fuzz" "--ifdef" "--ignore-whitespace" "--normal"
    "--output" "--prefix" "--quiet" "--reject-file" "--remove-empty-files"
    "--reverse" "--silent" "--skip" "--strip" "--suffix" "--unified"
    "--version" "--version--control" "-B" "-D" "-E" "-F" "-N" "-R" "-S"
    "-V" "-b" "-c" "-d" "-e" "-f" "-l" "-n" "-o" "-p1" "-pnumber" "-r" "-s"
    "-t" "-u" "-v" "-x") nil)
  ("pax"
   ("-a" "-b" "-c" "-d" "-f" "-i" "-l" "-m" "-n" "-o" "-p" "-r" "-rw" "-s"
    "-t" "-u" "-v" "-w" "-x" "-y") nil)
  ("ping"
   ("-R" "-l" "-r" "-v")
   ((nil "\\s +-\\S *$" "hostname" hostname)))
  ("place"
   ("-anchor" "-bordermode" "-height" "-in" "-relheight" "-relwidth"
    "-relx" "-rely" "-width" "-x" "-y") nil)
  ("plaid"
   ("-b" "-bd" "-bg" "-bw" "-fg" "-geometry"
    ("-display" t "hostname" hostname)) nil)
  ("plbpex"
   ("-bd" "-bg" "-buff" "-bw" "-geometry" "-h" "-hlhsr"
    ("-display" t "hostname" hostname)) nil)
  ("point"
   ("-l300" "-l300s" "-l4014" "-l450" "-lplot" "-lplot2648" "-lplot7221"
    "-lplotaed" "-lplotbg" "-lplotdumb" "-lplotgigi" "-lplotimagen") nil)
  ("ppmpat"
   ("-anticamo" "-camo" "-gingham2" "-gingham3" "-madras" "-poles" "-squig"
    "-tartan") nil)
  ("printcore"
   ("-d" "-foo") nil)
  ("psh"
   ("-i" "-s"
    ("-display" t "hostname" hostname)) nil)
  ("psindent"
   ("-ba" "-bb" "-bs" "-cba" "-cbb" "-cea" "-ceb" "-da" "-db" "-dba" "-dbb"
    "-dea" "-deb" "-ea" "-eb" "-gra" "-grb" "-gsa" "-gsb" "-ia" "-ib"
    "-lba" "-lbb" "-lca" "-lcb" "-rba" "-rbb" "-rca" "-rcb") nil)
  ("psping"
   ("-usage"
    ("-display" t "hostname" hostname)) nil)
  ("purify"
   ("-LEAKS_AT_EXIT" "-LeaksA-" "-always-use-cache-dir" "-append-logfile"
    "-auto-mount-prefix" "-cache-dir" "-chain-length" "-collector"
    "-continue-without-license" "-copy-fd-output-to-logfile"
    "-dynamic-linker" "-fds" "-first-only" "-free-queue-length"
    "-free-queue-threshold" "-g++" "-handle" "-handle-signals" "-ignore"
    "-ignore-runtime-environment" "-inuse-at-exit" "-leak-threshold"
    "-leaks-at-exit" "-linker" "-logfile" "-mail-to-user" "-option_name"
    "-output-limit" "-pointer-mask" "-pointer-offset" "-program-name"
    "-search-mmaps" "-user-path" "-watchpoints-file") nil)
  ("pushd" nil
   ((nil "\\s +\\([0-9+-]+\\|\\S *~[^/]*\\)$" "dirname" filespec nil nil
	 nil t nil)))
  ("puzzle"
   ("-colormap" "-geometry" "-picture" "-size" "-speed"
    ("-display" t "hostname" hostname)) nil)
  ("pyro"
   ("-count" "-frequency" "-mono" "-root" "-scatter" "-window") nil)
  ("qix"
   ("-additive" "-color-shift" "-count" "-delay" "-hollow" "-linear"
    "-mono" "-non-transparent" "-random" "-root" "-segments" "-size"
    "-solid" "-spread" "-subtractive" "-transparent" "-window" "-xor") nil)
  ("rasfilter8to1"
   ("-d" "-rgba") nil)
  ("realxfishdb"
   ("-bn" "-d" "-fnn" "-in" "-rn" "-s") nil)
  ("reservecolors"
   ("-discard" "-invramp" "-nokeep" "-svmono"
    ("-display" t "hostname" hostname)) nil)
  ("rlogin"
   ("-L" "-e"
    ("-l" t "username" username))
   ((nil "\\s +-l\\s +\\S *$" nil hostname)
    ("\\s +-l\\s +\\S *$" nil "username" username)
    ("^\\S +\\s +\\S *$" "\\s +-\\S *$" "hostname" hostname)))
  ("rocks"
   ("-count" "-delay" "-norotate" "-root" "-speed" "-window") nil)
  ("rorschach"
   ("-iterations" "-mono" "-offset" "-root" "-window" "-xsymmetry"
    "-ysymmetry") nil)
  ("rsh"
   ("-n"
    ("-l" t "username" username))
   ((nil "^\\S +\\s +\\S *$\\|\\s +-l\\s +\\S *$" nil command)
    (nil "\\s +-l\\s +\\S *$" nil hostname)
    ("\\s +-l\\s +\\S *$" nil "username" username)
    ("^\\S +\\s +\\S *$" "\\s +-\\S *$" "hostname" hostname)))
  ("sbbuild"
   ("-help" "-verbose" "-version") nil)
  ("sbquery"
   ("-break_lock" "-files_only" "-help" "-help_filter" "-help_focus"
    "-literal" "-max_memory" "-no_case" "-no_secondaries" "-no_source"
    "-no_update" "-o" "-pattern" "-reg_expr" "-sh_pattern" "-show_db_dirs"
    "-symbols_only" "-version") nil)
  ("sbrowser"
   ("-b" "-sb" "-xsb") nil)
  ("sccs-admin"
   ("-a" "-b" "-d" "-e" "-f" "-h" "-i" "-l" "-la" "-m" "-n" "-r" "-t" "-y"
    "-z") nil)
  ("sccs-get"
   ("-G" "-a" "-b" "-c" "-e" "-g" "-i" "-k" "-lp" "-m" "-n" "-p" "-r" "-s"
    "-t" "-x") nil)
  ("sendmail"
   ("-C" "-F" "-M" "-R" "-ba" "-bd" "-bi" "-bm" "-bp" "-bs" "-bt" "-bv"
    "-bz" "-d" "-f" "-h" "-n" "-o" "-q" "-r" "-t" "-v") nil)
  ("server"
   ("-DERROR_MAIL_ANALYSIS" "-DHAVE_SETJMP_H" "-D_POSIX_SOURCE" "-I"
    "-lbsd") nil)
  ("set" nil
   (("^\\S +\\s +\\S *$" nil "shell variable" variable shell)))
  ("setenv" nil
   (("^\\S +\\s +\\S *$" nil "environment variable" variable environment)))
  ("showfont"
   ("-L" "-M" "-bitmap_pad" "-end" "-extents_only" "-l" "-m" "-pad"
    "-server" "-start" "-unit") nil)
  ("slidescreen"
   ("-delay" "-grid-size" "-ibw" "-increment" "-root" "-window") nil)
  ("space"
   ("-l300" "-l300s" "-l4014" "-l450" "-lplot" "-lplot2648" "-lplot7221"
    "-lplotaed" "-lplotbg" "-lplotdumb" "-lplotgigi" "-lplotimagen") nil)
  ("speaker"
   ("-c" "-d" "-g" "-l" "-m" "-n" "-p" "-q" "-r" "-s" "-s0" "-u") nil)
  ("spider"
   ("-save_file") nil)
  ("stty"
   ("-a" "-evenp" "-g" "-litout" "-oddp" "-parity" "-pass8") nil)
  ("suntools"
   ("-8bit_color_only" "-B" "-F" "-P" "-S" "-WH" "-WI" "-WL" "-WP" "-WT"
    "-Wb" "-Wf" "-Wg" "-Wh" "-Wi" "-Wl" "-Wn" "-Wp" "-Ws" "-Wt" "-Ww" "-b"
    "-background" "-d" "-f" "-i" "-k" "-m" "-n" "-overlay_only" "-p"
    "-pattern" "-s" "-toggle_enable") nil)
  ("sunview"
   ("-8bit_color_only" "-B" "-F" "-P" "-S" "-WH" "-WI" "-WL" "-WP" "-WT"
    "-Wb" "-Wf" "-Wg" "-Wh" "-Wi" "-Wl" "-Wn" "-Wp" "-Ws" "-Wt" "-Ww" "-b"
    "-background" "-d" "-f" "-i" "-k" "-m" "-n" "-overlay_only" "-p"
    "-pattern" "-s" "-toggle_enable") nil)
  ("svenv"
   ("-csh" "-env" "-exec" "-warn"
    ("-display" t "hostname" hostname)) nil)
  ("svid2"
   ("-lsvidii" "-lsvidii-3" "-n") nil)
  ("svidii"
   ("-lsvidii" "-lsvidii-3" "-n") nil)
  ("tbl"
   ("-ms") nil)
  ("tdl"
   ("-add" "-all" "-category" "-date" "-deadline" "-f" "-help" "-include"
    "-list" "-order" "-postscript" "-print" "-printer" "-scale") nil)
  ("telnet" nil
   ((nil "\\s +-\\S *$" "hostname" hostname)))
  ("termcap"
   ("-ltermcap") nil)
  ("terminfo"
   ("-am" "-na" "-nam" "-rv" "-screen" "-w") nil)
  ("text"
   ("-background" "-bgstipple" "-borderwidth" "-fgstipple" "-font"
    "-foreground" "-relief" "-underline") nil)
  ("textedit"
   ("-EL" "-ES" "-ET" "-EU" "-Ea" "-Ec" "-Ei" "-Em" "-En" "-Eo" "-Er" "-Es"
    "-Et" "-Eu" "-adjust_is_pending_delete" "-auto_indent" "-checkpoint"
    "-history_limit" "-lower_context" "-margin" "-multi_click_space"
    "-multi_click_timeout" "-number_of_lines" "-okay_to_overwrite"
    "-read_only" "-scratch_window" "-tab_width" "-upper_context") nil)
  ("tgetent"
   ("-ltermcap") nil)
  ("tgetflag"
   ("-ltermcap") nil)
  ("tgetnum"
   ("-ltermcap") nil)
  ("tgetstr"
   ("-ltermcap") nil)
  ("tgoto"
   ("-ltermcap") nil)
  ("toolplaces"
   ("-O" "-S" "-Wl" "-d" "-help" "-o" "-r" "-u") nil)
  ("toolwait"
   ("-help" "-timeout"
    ("-display" t "hostname" hostname)) nil)
  ("toplevel"
   ("-class" "-screen") nil)
  ("tputs"
   ("-ltermcap") nil)
  ("traceroute" nil
   ((nil "\\s +-\\S *$" "hostname" hostname)))
  ("troff"
   ("-a" "-b" "-f" "-i" "-m" "-n" "-o" "-pN" "-q" "-r" "-s" "-t" "-w" "-z")
   nil)
  ("ttdbck"
   ("-F" "-I" "-T" "-Z" "-a" "-b" "-f" "-i" "-imp" "-k" "-m" "-p" "-t" "-x")
   nil)
  ("tttar"
   ("-h" "-rename" "-v") nil)
  ("twm"
   ("-f" "-s" "-v"
    ("-display" t "hostname" hostname)) nil)
  ("unalias" nil
   (("^\\S +\\s +\\S *$" nil "command alias" alias)))
  ("uncompress"
   ("-b" "-c" "-f" "-v")
   ((nil "\\s +-\\S *$" "filename" filespec "\\(\\.gz\\|\\.Z\\)$" nil nil
	 nil nil)))
  ("unifdef"
   ("-c" "-iD" "-iU" "-l" "-t") nil)
  ("unix2dos"
   ("-ascii" "-iso") nil)
  ("unset"
   ("-h" "-l" "-p" "-r")
   (("^\\S +\\s +\\S *$" nil "shell variable" variable shell)))
  ("unsetenv" nil
   (("^\\S +\\s +\\S *$" nil "environment variable" variable environment)))
  ("viewres"
   ("-top" "-variable" "-vertical") nil)
  ("vkbd"
   ("-nopopup") nil)
  ("vtwm"
   ("-f" "-s" "-v"
    ("-display" t "hostname" hostname)) nil)
  ("where" nil
   ((nil "\\s +-\\S *$" "command" command)))
  ("which" nil
   ((nil "\\s +-\\S *$" "command" command)))
  ("winsysck"
   ("-a" "-v"
    ("-display" t "hostname" hostname)) nil)
  ("wish"
   ("-file" "-geometry" "-help" "-name" "-sync"
    ("-display" t "hostname" hostname)) nil)
  ("worms"
   ("-field" "-trail") nil)
  ("x11perf"
   ("-all" "-atom" "-bg" "-bigosrect1" "-bigosrect10" "-bigosrect100"
    "-bigosrect500" "-bigostrap10" "-bigostrap100" "-bigsrect1"
    "-bigsrect10" "-bigsrect100" "-bigsrect500" "-bigstrap10"
    "-bigstrap100" "-bigtilerect1" "-bigtilerect10" "-bigtilerect100"
    "-bigtilerect500" "-bigtiletrap10" "-bigtiletrap100" "-circle1"
    "-circle10" "-circle100" "-circle500" "-circulate" "-complex10"
    "-complex100" "-copypixpix10" "-copypixpix100" "-copypixpix500"
    "-copypixwin10" "-copypixwin100" "-copypixwin500" "-copyplane10"
    "-copyplane100" "-copyplane500" "-copywinpix10" "-copywinpix100"
    "-copywinpix500" "-copywinwin10" "-copywinwin100" "-copywinwin500"
    "-create" "-dcircle100" "-ddcircle100" "-ddellipse100" "-ddline100"
    "-ddseg100" "-dellipse100" "-depth" "-destroy" "-dline10" "-dline100"
    "-dot" "-dseg10" "-dseg100" "-ellipse10" "-ellipse100" "-ellipse500"
    "-eschertilerect1" "-eschertilerect10" "-eschertilerect100"
    "-eschertilerect500" "-eschertiletrap10" "-eschertiletrap100"
    "-f14itext16" "-f14text16" "-f8itext" "-f8text" "-f9itext" "-f9text"
    "-fcircle1" "-fcircle10" "-fcircle100" "-fcircle500" "-fcpcircle10"
    "-fcpcircle100" "-fcpellipse10" "-fcpellipse100" "-fellipse10"
    "-fellipse100" "-fellipse500" "-fg" "-fitext" "-fspcircle10"
    "-fspcircle100" "-fspellipse10" "-fspellipse100" "-ftext" "-gc"
    "-getimage10" "-getimage100" "-getimage500" "-hseg10" "-hseg100"
    "-hseg500" "-labels" "-line1" "-line10" "-line100" "-line500" "-map"
    "-move" "-movetree" "-noop" "-orect10" "-orect100" "-orect500"
    "-osrect1" "-osrect10" "-osrect100" "-osrect500" "-ostrap10"
    "-ostrap100" "-pack" "-pcircle10" "-pcircle100" "-pellipse10"
    "-pellipse100" "-pm" "-polytext" "-popup" "-prop" "-putimage10"
    "-putimage100" "-putimage500" "-range" "-rect1" "-rect10" "-rect100"
    "-rect500" "-repeat" "-resize" "-rop" "-scroll10" "-scroll100"
    "-scroll500" "-seg1" "-seg10" "-seg100" "-seg100c1" "-seg100c2"
    "-seg100c3" "-seg500" "-shmput10" "-shmput100" "-shmput500" "-srect1"
    "-srect10" "-srect100" "-srect500" "-strap10" "-strap100" "-sync"
    "-tilerect1" "-tilerect10" "-tilerect100" "-tilerect500" "-tiletrap10"
    "-tiletrap100" "-time" "-tr10itext" "-tr10text" "-tr24itext"
    "-tr24text" "-trap10" "-trap100" "-triangle1" "-triangle10"
    "-triangle100" "-ucirculate" "-ucreate" "-umove" "-unmap" "-uresize"
    "-vseg10" "-vseg100" "-vseg500" "-wcircle10" "-wcircle100"
    "-wcircle500" "-wdcircle100" "-wddcircle100" "-wddellipse100"
    "-wddline100" "-wdellipse100" "-wdline100" "-wellipse10" "-wellipse100"
    "-wellipse500" "-whseg10" "-whseg100" "-whseg500" "-wline10"
    "-wline100" "-wline500" "-worect10" "-worect100" "-worect500"
    "-wpcircle10" "-wpcircle100" "-wpellipse10" "-wpellipse100" "-wvseg10"
    "-wvseg100" "-wvseg500"
    ("-display" t "hostname" hostname)) nil)
  ("x11perfcomp"
   ("-l" "-r" "-ro") nil)
  ("x3270"
   ("-activeicon" "-apl" "-charset" "-efont" "-keymap" "-model" "-mono"
    "-port" "-tn") nil)
  ("xalarm"
   ("-alarmaudio" "-confirm" "-daemon" "-date" "-file" "-geometry" "-help"
    "-kill" "-list" "-noalarmaudio" "-noconfirm" "-nopester" "-nowarn"
    "-nowarningaudio" "-nowarnwords" "-pester" "-quiet" "-reset"
    "-restartonly" "-snooze" "-time" "-version" "-volume" "-warn"
    "-warningaudio" "-warnwords" "-xrm"
    ("-display" t "hostname" hostname)) nil)
  ("xarchie"
   ("-D" "-N" "-c" "-color" "-debug" "-e" "-ec" "-er" "-es" "-gray" "-help"
    "-host" "-maxHits" "-mono" "-nice" "-noscroll" "-offset" "-r" "-s"
    "-search" "-sort" "-t" "-w") nil)
  ("xautolock"
   ("-bell" "-cornerdelay" "-corners" "-cornersize" "-help" "-locker"
    "-noclose" "-notify" "-time" "-version") nil)
  ("xbiff"
   ("-bd" "-bg" "-bw" "-fg" "-file" "-geometry" "-help" "-rv" "-shape"
    "-update" "-volume" "-xrm"
    ("-display" t "hostname" hostname)) nil)
  ("xcalc"
   ("-rpn" "-stipple") nil)
  ("xclipboard"
   ("-nw" "-w") nil)
  ("xclock"
   ("-analog" "-chime" "-digital" "-hd" "-help" "-hl" "-padding" "-update")
   nil)
  ("xcmsdb"
   ("-color" "-format" "-query" "-remove") nil)
  ("xcolor"
   ("-dump" "-geometry" "-half" "-iconwin" "-nobw" "-noinst"
    ("-display" t "hostname" hostname)) nil)
  ("xconsole"
   ("-daemon" "-exitOnFail" "-file" "-nonotify" "-notify" "-verbose") nil)
  ("xcutsel"
   ("-cutbuffer" "-selection") nil)
  ("xditview"
   ("-backingStore" "-bd" "-bg" "-bw" "-fg" "-fn" "-geometry" "-help"
    "-page" "-rv" "-xrm"
    ("-display" t "hostname" hostname)) nil)
  ("xdm"
   ("-config" "-daemon" "-debug" "-error" "-nodaemon" "-resources"
    "-server" "-session" "-udpPort" "-xrm") nil)
  ("xdpr"
   ("-P" "-device" "-help"
    ("-display" t "hostname" hostname)) nil)
  ("xearth"
   ("-day" "-gif" "-label" "-night" "-nofork" "-nogeostatic" "-noshade"
    "-pos" "-ppm" "-size" "-sunpos" "-version" "-wait") nil)
  ("xev"
   ("-bs" "-bw" "-geometry" "-id" "-name" "-rv" "-s"
    ("-display" t "hostname" hostname)) nil)
  ("xeyes"
   ("-backing" "-bd" "-bg" "-bw" "-center" "-fg" "-geometry" "-outline"
    "-shape"
    ("-display" t "hostname" hostname)) nil)
  ("xfd"
   ("-bc" "-box" "-center" "-fn" "-start") nil)
  ("xfig"
   ("-Landscape" "-Portrait" "-bg" "-bold" "-but_per_row" "-button"
    "-centimeters" "-debug" "-exportLanguage" "-fg" "-flushleft"
    "-geometry" "-iconGeometry" "-imperial" "-inches" "-internalBW"
    "-inverse" "-keyFile" "-latexfonts" "-left" "-metric" "-monochrome"
    "-normal" "-noscalablefonts" "-notrack" "-pheight" "-pwidth" "-right"
    "-scalablefonts" "-showallbuttons" "-specialtext" "-startfontsize"
    "-startlatexFont" "-startpsFont" "-textoutline" "-track" "-userscale"
    "-userunit") nil)
  ("xhost"
   (("-display" t "hostname" hostname))
   ((nil nil "hostname" hostname "^[+-]?")))
  ("xhtalk"
   ("-all" "-buttonFont" "-color" "-dIdle" "-dLight" "-debug" "-f" "-h"
    "-help" "-idle" "-idleOneCol" "-idleThreeCol" "-idleTwoCol" "-info"
    "-prog" "-timeout" "-tty" "-update") nil)
  ("xkill"
   ("-all" "-button" "-frame" "-id"
    ("-display" t "hostname" hostname)) nil)
  ("xload"
   ("-highlight" "-hl" "-jumpscroll" "-label" "-nolabel" "-scale" "-update")
   nil)
  ("xlock"
   ("-From" "-batchcount" "-bg" "-delay" "-fg" "-font" "-info" "-invalid"
    "-mode" "-name" "-nice" "-password" "-resources" "-saturation"
    "-timeout" "-username" "-v" "-validate"
    ("-display" t "hostname" hostname)) nil)
  ("xlsatoms"
   ("-format" "-name" "-range"
    ("-display" t "hostname" hostname)) nil)
  ("xlsclients"
   ("-a" "-l" "-m"
    ("-display" t "hostname" hostname)) nil)
  ("xlsfonts"
   ("-C" "-l" "-m" "-n" "-w"
    ("-display" t "hostname" hostname)) nil)
  ("xlswins"
   ("-format" "-indent" "-l"
    ("-display" t "hostname" hostname)) nil)
  ("xmag"
   ("-bd" "-bg" "-bw" "-fn" "-geometry" "-mag" "-source" "-z"
    ("-display" t "hostname" hostname)) nil)
  ("xman"
   ("-bothshown" "-geometry" "-helpfile" "-notopbox" "-pagesize") nil)
  ("xmh"
   ("-file") nil)
  ("xmkmf"
   ("-DCURDIR" "-DTOPDIR") nil)
  ("xmodmap"
   ("-e" "-grammar" "-help" "-n" "-pk" "-pm" "-pp" "-quiet" "-verbose"
    ("-display" t "hostname" hostname)) nil)
  ("xnetload"
   ("-highlight" "-hl" "-jumpscroll" "-label" "-nolabel" "-scale" "-update")
   nil)
  ("xnews"
   (("-display" t "hostname" hostname)) nil)
  ("xon"
   ("-access" "-debug" "-name" "-nols" "-screen"
    ("-user" t "username" username))
   ((nil
     "^\\S +\\s +\\S *$\\|\\s +-name\\s +\\S *$\\|\\s +-screen\\s +\\S *$\\|\\s +-user\\s +\\S *$"
     "command" command)
    ("^\\S +\\s +\\S *$" nil "command" hostname)))
  ("xpr"
   ("-append" "-compact" "-cutoff" "-density" "-device" "-gamma" "-gray"
    "-header" "-height" "-landscape" "-left" "-noff" "-noposition"
    "-output" "-plane" "-portrait" "-psfig" "-render" "-rv" "-scale"
    "-slide" "-split" "-top" "-trailer" "-width") nil)
  ("xprop"
   ("-f" "-font" "-frame" "-fs" "-grammar" "-help" "-id" "-len" "-name"
    "-notype" "-remove" "-root" "-spy"
    ("-display" t "hostname" hostname)) nil)
  ("xrdb"
   ("-D" "-I" "-U" "-backup" "-cpp" "-edit" "-help" "-load" "-merge" "-n"
    "-nocpp" "-query" "-quiet" "-remove" "-retain" "-symbols"
    ("-display" t "hostname" hostname)) nil)
  ("xrefresh"
   ("-black" "-geometry" "-none" "-root" "-solid" "-white"
    ("-display" t "hostname" hostname)) nil)
  ("xroger"
   ("-mono" "-root" "-window") nil)
  ("xscope"
   ("-d" "-h" "-i" "-o" "-q" "-v"
    ("-display" t "hostname" hostname)) nil)
  ("xscreensaver"
   ("-cycle" "-demo" "-lock" "-lock-timeout" "-nice" "-no-lock" "-no-xidle"
    "-silent" "-timeout" "-verbose" "-visual" "-xidle") nil)
  ("xscreensaver-command"
   ("-activate" "-cycle" "-deactivate" "-demo" "-exit" "-lock" "-next"
    "-prev" "-restart") nil)
  ("xset"
   ("-fp"
    ("-display" t "hostname" hostname)) nil)
  ("xsetroot"
   ("-bg" "-bitmap" "-cursor" "-cursor_name" "-def" "-fg" "-gray" "-grey"
    "-help" "-mod" "-name" "-rv" "-solid"
    ("-display" t "hostname" hostname)) nil)
  ("xsol"
   ("-nodrag"
    ("-display" t "hostname" hostname)) nil)
  ("xsplinefun"
   ("-backwards" "-forwards" "-id" "-loops" "-maxcolors") nil)
  ("xstdcmap"
   ("-all" "-best" "-blue" "-default" "-delete" "-gray" "-green" "-help"
    "-red" "-verbose"
    ("-display" t "hostname" hostname)) nil)
  ("xterm"
   ("-C" "-S" "-T" "-ah" "-aw" "-b" "-bd" "-bg" "-borderwidth" "-bw" "-cc"
    "-cn" "-cr" "-cu" "-fb" "-fg" "-fn" "-geometry" "-help" "-iconic" "-j"
    "-l" "-lf" "-ls" "-mb" "-mc" "-ms" "-n" "-name" "-nb" "-r" "-rv" "-rw"
    "-s" "-sb" "-sf" "-si" "-sk" "-sl" "-t" "-title" "-tm" "-tn" "-ut"
    "-vb" "-w" "-wf" "-xrm"
    ("-display" t "hostname" hostname)
    ("-e" t "command" command)) nil)
  ("xv_get_sel"
   ("-D" "-rank" "-t") nil)
  ("xview"
   ("-position") nil)
  ("xwd"
   ("-add" "-frame" "-help" "-nobdrs" "-out" "-xy"
    ("-display" t "hostname" hostname)) nil)
  ("xwininfo"
   ("-all" "-bits" "-english" "-events" "-frame" "-help" "-id" "-int"
    "-metric" "-name" "-root" "-size" "-stats" "-tree" "-wm"
    ("-display" t "hostname" hostname)) nil)
  ("xwud"
   ("-bg" "-fewcolors" "-fg" "-geometry" "-help" "-in" "-new" "-noclick"
    "-plane" "-raw" "-rv" "-std" "-vis"
    ("-display" t "hostname" hostname)) nil)
  ("yacc"
   ("-Qy" "-V" "-d" "-l" "-t" "-v") nil)
  ("ypbind"
   ("-d" "-s" "-v" "-ypset" "-ypsetme") nil)
  ("ypcat"
   ("-d" "-k" "-t" "-x")
   ((nil nil "NIS name" shell-string "ypcat -x"
	 "^\\S +\\s +\"\\(\\S +\\)\"" 1)))
  ("ypserv"
   ("-d" "-s" "-v" "-ypset" "-ypsetme") nil)
  ("ypset"
   ("-V1" "-V2" "-d" "-h") nil)
  ("ypwhich"
   ("-V1" "-V2" "-d" "-m" "-t" "-x") nil)
  ("ypxfrd"
   ("-d" "-s" "-v" "-ypset" "-ypsetme") nil)
  ("zcat"
   ("--ascii" "--best" "--decompress" "--fast" "--force" "--help"
    "--license" "--list" "--name" "--no-name" "--quiet" "--recursive"
    "--stdout" "--suffix" "--test" "--to-stdout" "--uncompress" "--verbose"
    "--version" "-L" "-N" "-S" "-V" "-a" "-c" "-d" "-f" "-h" "-l" "-n" "-q"
    "-r" "-t" "-v") 
   ((nil "\\s +-\\S *$" "filename" filespec "\\(\\.gz\\|\\.Z\\)$" nil nil
	 nil nil)))
)
"List of shell commands and their arguments.
Contains command names, option names, and general arguments.
For additions, see `shelldb-add-command-options' and `shelldb-help'.

Each item is of the form:
 (\"command\" (option-spec ...) (arg-spec ...))

option-spec is of the form:
 \"option\" or
 (\"options\" gap barf (\"option arg\" ...)) or
 (\"options\" gap barf type arguments) or
 (\"options\" gap barf)

arg-spec is of the form:
 (\"incl-regexp\" \"excl-regexp\" barf (\"command arg\" ...)) or
 (\"incl-regexp\" \"excl-regexp\" barf type arguments) or
 (\"incl-regexp\" \"excl-regexp\" barf)

If gap is nil, the text that immediately follows the option should be used.

If barf is non-nil, then failure is deemed to be \"fatal\" (that is, no further
action should be taken).  If barf is a string, it is used to display an
appropriate message.

The type indicates how the option or argument should be interpreted.
Currently, types used include `alias', `command', `filespec', `hostname',
`mailname', `shell-string', `username', and `variable'.  Any symbol can be
used, of course.  The arguments should be passed to whatever function is used
to interpret the type.

Note that these may not be correct for your shell, and in fact you might not
even have some commands present in this list.")

;;; Add some stuff.
(let
 ((args
   '(
     ;; For example, add xalarm (ahem) and its args...
     ;;("xalarm" ("-alarmaudio" "-confirm" "-daemon" "-date" "-display" "-file"
     ;;		"-geometry" "-help" "-kill" "-list" "-noalarmaudio"
     ;;		"-noconfirm" "-nopester" "-nowarn" "-nowarningaudio"
     ;;		"-nowarnwords" "-pester" "-quiet" "-reset" "-restartonly"
     ;;		"-snooze" "-time" "-version" "-volume" "-warn"
     ;;		"-warningaudio" "-warnwords" "-xrm"))

     ;; GNU configure...
     ("configure" ("--help" "--quiet" "--silent" "--version" "--verbose"
		   "--with-x" "--with-x11" "--with-x10" "--with-x-toolkit"
		   "--with-gcc" "--run-in-place"))
     )))
 (while args
   (apply 'shelldb-add-command-options (car args))
   (setq args (cdr args))))

;;; Option arguments.
(let
 ((args
   '(
     ;; For example, add commands and command option arguments..
     ;;("find" "-user" t "username" username)
     ;;("find" "-fstype" t "file system type" ("nfs" "4.2"))
     ;;("find" "-type" t "file system type" ("b" "c" "d" "f" "p" "l" "s"))
     ;;("find" "-group" t "group name" shell-string
     ;; "ypcat group; cat /etc/group" "^[^+][^:]*" 0)
     ;;("find" ("-exec" "-ok") t "command" command)
     ;;("find" "-perm" t "permission flag")
     ;;(("cc" "cpp" "gcc" "lint" "ld") "-I" nil "dirname"
     ;; filespec nil nil "^-I" t nil)
     ;;(("cc" "cpp" "gcc" "lint" "ld") "-L" nil "dirname"
     ;; filespec nil nil "^-L" t nil)

     ;; GNU configure...
     ("configure" "--x-includes=" nil "dirname"
      filespec nil nil "^--x-includes=" t nil)
     ("configure" "--x-libraries=" nil "dirname"
      filespec nil nil "^--x-libraries=" t nil)
     ("configure" "--srcdir=" nil "dirname"
      filespec nil nil "^--srcdir=" t nil)
     ("configure" "--prefix=" nil "dirname"
      filespec nil nil "^--prefix=" t nil)
     ("configure" "--exec-prefix=" nil "dirname"
      filespec nil nil "^--exec-prefix=" t nil)
     )))
 (while args
   (apply 'shelldb-add-command-option-arguments (car args))
   (setq args (cdr args))))

;;; General arguments.
(let
 ((args
   (list

;;    (list "xon"
;;	  (list (shelldb-argumentn 1) nil "command" 'hostname))

;;    (list "xon"
;;	  (list nil (format "%s\\|%s" (shelldb-argumentn 1)
;;			    (shelldb-option-preceding-argument
;;			     '("-name" "-screen" "-user")))
;;		"command" 'command))

;;    (list '("rsh" "rlogin")
;;	  (list (shelldb-argumentn 1) (shelldb-last-argument "-\\S *") "hostname"
;;		'hostname))

;;    (list '("rsh" "rlogin")
;;	  (list (shelldb-option-preceding-argument "-l") nil "username"
;;		'username))

;;    (list '("rsh" "rlogin")
;;	  (list nil (shelldb-option-preceding-argument "-l") nil 'hostname))

;;    (list "rsh"
;;	  (list nil (format "%s\\|%s" (shelldb-argumentn 1)
;;			    (shelldb-option-preceding-argument "-l"))
;;		nil 'command))

;;    (list '("gzip" "compress")
;;	  (list nil (shelldb-last-argument "-\\S *") "filename" 'filespec
;;		nil "\\(\\.gz\\|\\.Z\\)$" nil nil nil))

;;    (list '("gunzip" "uncompress")
;;	  (list nil (shelldb-last-argument "-\\S *") "filename" 'filespec
;;		"\\(\\.gz\\|\\.Z\\)$" nil nil nil nil))

;;    '("ypcat" (nil nil "NIS name" shell-string "ypcat -x"
;;		   "^\\S +\\s +\"\\(\\S +\\)\"" 1))

;;    '("xhost" (nil nil "hostname" hostname "^[+-]?"))

    )))
 (while args
   (apply 'shelldb-add-command-arguments (car args))
   (setq args (cdr args))))

;;; General general stuff.
;;(shelldb-add-general-option-arguments "-display" t "hostname" 'hostname)

;;; Install ourselves.

(provide 'shelldb)

;;; shelldb.el ends here
