;;; shells.el --- spell checker for the shell.

;; Author: Simon Marshall <Simon.Marshall@mail.esrin.esa.it>
;; Keywords: processes spell check
;; Version: 1.01

;; LCD Archive Entry:
;; shells|Simon Marshall|Simon.Marshall@mail.esrin.esa.it|
;; Spelling correction for the emacs-19 shell buffer.|
;; 10-Aug-1994|1.01|~/packages/shells.el.Z|
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
;; Shells is a spell-checker package for the Emacs-19 shell.  It will also
;; serve as a general spell-checking package whenever file-type input is used.
;; Shells already knows how to spell-check paths, commands, user names, host
;; names, variables, many command-line options, but can also spell-check
;; command-line option arguments, specific command arguments, and general
;; command arguments...

;; Installation:
;; 
;; To use, put in your ~/.emacs:
;;
;; (eval-after-load "shell" '(define-key shell-mode-map "\M-#"
;;                             'shells-dynamic-spell))
;;
;; (autoload 'shells-dynamic-spell
;;   "shells" "Dynamically perform spell checking at point." t)
;;
;; Note that you will also need the shelldb and fuzzy-match packages:
;;
;; In a shell buffer, use M-# to correct the spelling before point.

;; Help:
;;
;; Try M-x shells-help and M-x shelldb-help.

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
;;     - M-x shells-submit-feedback
;;     - Simon Marshall (Simon.Marshall@mail.esrin.esa.it)

(require 'shelldb)
(require 'fuzzy-match)

(eval-when-compile
  (require 'reporter))

(defconst shells-version "1.01"
  "Version number of the shells package.")

;;; HELP!!!

(defun shells-help ()
  "This package is intended to provide context-based spell-check for the shell.

Knowledge about general shell and contextual information is contained within
the shelldb package.  For further information on shelldb, try M-x shelldb-help.
This package requires the fuzzy matching package `fuzzy-match'.  See that
package's source file for futher information.

Spell checking is automatic; typing M-# in a shell buffer should spell-check
the entity before point depending on the entity's context.  For example:

% find -user sminoM-#

etc., might result in spell-checking to leave the command line as:

% find -user simon

Provided with this package is general spell-checking for command names
\(including aliases and builtins) host names, user names, shell variables,
environment variables, mail names and file specs, and basically anything if the
list of candidates can be extracted from a shell command.  See the functions
`shells-dynamic-spell-*'.  See also the variables `shells-fuzzy-tolerance',
`shells-fuzzy-brownie-points' and `shells-prompt-before-correction'.

Submit comments, bug reports, etc., with M-x shells-submit-feedback.

Enjoy."
  (interactive)
  (let ((buf "*Help Shells*"))
    (with-output-to-temp-buffer buf
      (princ (documentation 'shells-help) (get-buffer buf)))))

(defvar shells-fuzzy-tolerance 2
  "The amount of fuzziness tolerated when spelling.
Each unit of tolerance represents an incorrect character.
Was called `shells-fuzzy-money', since the analogy was \"paying\" each time
a comparison was made.

See `shells-fuzzy-brownie-points' and `shells-dynamic-spell'.")

(defvar shells-fuzzy-brownie-points 1
  "The amount of fuzziness given when spelling paths.
After each directory constituent, this amount is used to increase the tolerance
allowed for the next constituent.
Was called `shells-fuzzy-interest', see `shells-fuzzy-tolerance'.

See also `shells-dynamic-spell'.")

(defvar shells-prompt-before-correction nil
  "If non-nil, ask a `y-or-n-p' question before replacement.
A question will only be asked if there is only one candidate for correction.")

(defvar shells-dynamic-spell-functions
  '(shells-dynamic-spell-command-option
    shells-dynamic-spell-command-option-argument-extended
    shells-dynamic-spell-command-option-argument
    shells-dynamic-spell-command-argument
    shells-dynamic-spell-command
    shells-dynamic-spell-variable
    shells-dynamic-spell-username
    shells-dynamic-spell-hostname
    shells-dynamic-spell-path)
  "List of functions called to perform spell checking.
Functions should return non-nil if correction was performed successfully.
See also `shells-dynamic-spell'.")

(defun shells-dynamic-spell ()
  "Dynamically perform spell checking at point.
Calls the functions in `shells-dynamic-spell-functions' to perform spell
checking until a function returns non-nil, at which point correction is
assumed to have occurred.

See `shells-fuzzy-tolerance' and `shells-fuzzy-brownie-points'."
  (interactive)
  (let ((functions shells-dynamic-spell-functions))
    (while (and functions (null (funcall (car functions))))
      (setq functions (cdr functions)))))


(defun shells-dynamic-spell-command-option ()
  "Dynamically spell check the command option.
Options are assumed to begin with \"-\"."
  (let ((input (shelldb-input)))
    (if (string-match "^\\(\\S +\\)\\s +\\(\\S +\\s +\\)*\\(-\\S *\\)$" input)
	(let* ((command (shelldb-command (match-string 1 input)))
	       (option (match-string 3 input))
	       (options (mapcar (function (lambda (o) (or (car-safe o) o)))
				(nth 1 (shelldb-command-arguments command)))))
	  (message "Spell checking command option name...")
	  (comint-word "!-~")
	  (FM-offer-corrections option
	   (shells-sort (shells-things-matching-thing option options))
	   shells-prompt-before-correction)))))


(defun shells-dynamic-spell-command-option-argument ()
  "Dynamically spell the command option argument.
Options are assumed to begin with \"-\" and there is a gap between option and
argument."
  (let ((input (shelldb-input)))
    (if (string-match
	 "^\\(\\S +\\)\\s +\\(\\S +\\s +\\)*\\(-\\S *\\)\\s +\\(\\S *\\)$"
	 input)
	(let* ((arglist
		(assoc (match-string 3 input)
		       (nth 1 (shelldb-command-arguments
			       (shelldb-command (match-string 1 input))))))
	       (argument (match-string 4 input))
	       (spelt (and arglist
			   (shells-perform-spell
			    "Spell checking command option argument%s..."
			    argument arglist))))
	  ;; Any error to report?
	  (if (stringp spelt)
	      (message "No candidates%s as %s"
	       (if (string-equal argument "") "" (concat " of " argument))
	       spelt))
	  spelt))))



(defun shells-dynamic-spell-command-option-argument-extended ()
  "Dynamically spell the extended command option argument.
Options are assumed to begin with \"-\" and there is no gap between option and
argument."
  (let ((input (shelldb-input)))
    (if (string-match "^\\(\\S +\\)\\s +\\(\\S +\\s +\\)*\\(-\\S *\\)$" input)
	(let* ((command (shelldb-command (match-string 1 input)))
	       (option (match-string 3 input))
	       (options (nth 1 (shelldb-command-arguments command)))
	       (spelt nil) (argument nil) (optext '()))
	  (while (and options (not spelt))
	    (setq optext (car options) options (cdr options))
	    (if (and (listp optext) (not (nth 1 optext))
		     (string-match (concat "^" (nth 0 optext)) option))
		(setq argument (substring option (match-end 0))
		      spelt (shells-perform-spell
			     "Spell checking command option%s..."
			     argument optext))))
	  ;; Any error to report?
	  (if (stringp spelt)
	      (message "No candidates%s as %s"
		(if (string-equal argument "") "" (concat " of " argument))
		spelt))
	  spelt))))


(defun shells-dynamic-spell-command-argument ()
  "Dynamically spell the command argument."
  (let ((input (shelldb-input)))
    (if (string-match "^\\(\\S +\\)\\s +\\(\\S +\\s +\\)*\\(\\S *\\)$" input)
	(let* ((command (shelldb-command (match-string 1 input)))
	       (argument (match-string 3 input))
	       (arguments (nth 2 (shelldb-command-arguments command)))
	       (arglist "") (spelt nil))
	  (while (and arguments (not spelt))
	    (setq arglist (car arguments) arguments (cdr arguments))
	    (if (and (or (not (stringp (nth 0 arglist)))
			 (string-match (nth 0 arglist) input))
		     (or (not (stringp (nth 1 arglist)))
			 (not (string-match (nth 1 arglist) input))))
		(setq spelt (shells-perform-spell
			     "Spell checking command argument%s..."
			     argument arglist))))
	  ;; Any error to report?
	  (if (stringp spelt)
	      (message "No candidates%s as %s"
		(if (string-equal argument "") "" (concat " of " argument))
		spelt))
	  spelt))))


(defvar shells-spell-functions
  '((alias . shells-dynamic-spell-as-command-alias)
    (command . shells-dynamic-spell-as-command)
    (filespec . shells-dynamic-spell-as-filespec)
    (hostname . shells-dynamic-spell-as-hostname)
    (mailname . shells-dynamic-spell-as-mailname)
    (shell-string . shells-dynamic-spell-as-shell-string)
    (username . shells-dynamic-spell-as-username)
    (variable . shells-dynamic-spell-as-variable))
  "Cons pair of the form (TYPE . SPELL-FUNCTION).
Used to map between entity type and the function required to spell check it.
See `shells-perform-spell'.")

(defun shells-perform-spell (mformat item clist)
  "Perform spell checking for ITEM using CLIST.
MFORMAT is the message format to use.  The message is displayed with \"barf\"
>from CLIST.  See `shelldb-command-arguments' for details on CLIST.
See `shells-spell-functions' for the functions used to spell check ITEM."
  (let ((barf (nth 2 clist)) (spell (nth 3 clist)))
    (message mformat (if (stringp barf) (concat " as " barf) ""))
    (or (cond ((null spell)
	       nil)
	      ((symbolp spell)
	       (let ((entry (assq spell shells-spell-functions)))
		 (apply (or (cdr-safe entry) 'ignore) (nthcdr 4 clist))))
	      (t
	       (comint-word "!-~")
	       (FM-offer-corrections item
		(shells-sort (shells-things-matching-thing item spell))
		shells-prompt-before-correction)))
	barf)))

(defun shells-dynamic-spell-path ()
  "Spell check before point as a path.
A listing is shown in a buffer if there are multiple candidates.
See `shells-pathnames-matching-pathname'."
  (interactive)
  (let ((path (comint-word "~/A-Za-z0-9+@:_.$#,={}-")))
    (if path
	(prog2 (message "Spell checking path name...")
	    (FM-offer-corrections path
	     (shells-sort (save-match-data
			    (shells-pathnames-matching-pathname path)))
	     shells-prompt-before-correction)))))


(defun shells-dynamic-spell-command ()
  "Spell check the command before point.
See `shells-dynamic-spell-as-command'."
  (interactive)
  (let ((filename (comint-word "~/A-Za-z0-9+:_.#,=-")))
    (if (and filename
	     (save-match-data (not (string-match "[~/]" filename)))
	     (eq (match-beginning 0)
		 (save-excursion (shell-backward-command 1) (point))))
	(prog2 (message "Spell checking command name...")
	    (shells-dynamic-spell-as-command)))))


(defun shells-dynamic-spell-as-command ()
  "Spell check before point as a command.
A listing is shown in a buffer if there are multiple candidates.
See `shells-commands-matching-command' and `shells-fuzzy-tolerance'."
  (interactive)
  (let ((command (comint-word "A-Za-z0-9+:_.#,=-")))
    (FM-offer-corrections command
     (save-match-data (shells-sort (shells-commands-matching-command command)))
     shells-prompt-before-correction)))


(defun shells-dynamic-spell-hostname ()
  "Spell check before point as a host name.
See `shells-dynamic-spell-as-hostname'."
  (interactive)
  (let ((hostname (comint-word "@A-Za-z0-9+_.-")))
    (if (and hostname (string-match "@" hostname))
	(prog2 (message "Spell checking host name...")
	    (shells-dynamic-spell-as-hostname)))))

(defun shells-dynamic-spell-as-hostname ()
  "Spell check before point as a host name.
A listing is shown in a buffer if there are multiple candidates.
See `shells-hostnames-matching-hostname' and `shells-fuzzy-tolerance'."
  (let* ((hostname (comint-word "A-Za-z0-9+_.-"))
	 (hostnames (shells-things-matching-thing
		     hostname (shelldb-elements 'hostnames))))
    (FM-offer-corrections hostname (shells-sort hostnames)
			  shells-prompt-before-correction)))


(defun shells-dynamic-spell-username ()
  "Spell check before point as a user name.
See `shells-dynamic-spell-as-username'."
  (interactive)
  (let ((username (comint-word "~A-Za-z0-9+_.-")))
    (if (and username (string-match "^~[^/]*$" username))
	(prog2 (message "Spell checking user name...")
	    (shells-dynamic-spell-as-username)))))

(defun shells-dynamic-spell-as-username ()
  "Spell check before point as a user name.
A listing is shown in a buffer if there are multiple candidates.
See `shells-usernames-matching-username' and `shells-fuzzy-tolerance'."
  (let ((username (comint-word "A-Za-z0-9+_.-")))
    (FM-offer-corrections username
     (shells-sort (shells-usernames-matching-username username))
     shells-prompt-before-correction)))


(defun shells-dynamic-spell-variable ()
  "Spell check before point as a user name.
See `shells-dynamic-spell-as-username'."
  (interactive)
  (let ((variable (shell-match-partial-variable)))
    (if (and variable (string-match "^\\$" variable))
	(prog2 (message "Spell checking variable name...")
	    (shells-dynamic-spell-as-variable)))))


(defun shells-dynamic-spell-as-variable ()
  "Spell check before point as a user name.
A listing is shown in a buffer if there are multiple candidates.
See `shells-usernames-matching-username' and `shells-fuzzy-tolerance'."
  (let ((var (shell-match-partial-variable)))
    (FM-offer-corrections var
     (save-match-data (shells-sort (shells-variables-matching-variable var)))
     shells-prompt-before-correction)))


(defun shells-dynamic-spell-as-mailname ()
  "Spell check before point as a mail name.
A listing is shown in a buffer if there are multiple candidates.
See `shells-mailnames-matching-mailname' and `shells-fuzzy-tolerance'."
  (require 'mailalias)
  (if (eq mail-aliases t)
      (build-mail-aliases))
  (let* ((mailname (or (comint-word "A-Za-z0-9+_.-") ""))
	 (candidates (append (mapcar 'car mail-aliases)
			     (shelldb-elements 'usernames)))
	 (mailnames (shells-things-matching-thing mailname candidates)))
    (FM-offer-corrections mailname (shells-sort mailnames)
			  shells-prompt-before-correction)))


(defun shells-dynamic-spell-as-command-alias ()
  "Spell check before point as a command alias.
A listing is shown in a buffer if there are multiple candidates.
See `shells-fuzzy-tolerance'."
  (let* ((alias (or (comint-word "A-Za-z0-9+_.-") ""))
	 (aliases (shells-things-matching-thing
		   alias (shelldb-elements 'command-builtins))))
    (FM-offer-corrections alias (shells-sort aliases)
			  shells-prompt-before-correction)))


(defun shells-dynamic-spell-as-shell-string (command regexp subexp
					     &optional prefix shell)
  "Spell check before point as string using COMMAND.
If the regexp PREFIX matches the current word before point, only the text
following the match is used for spell checking.
See `shelldb-word' and `shelldb-shell-strings'."
  (let* ((shell-string (shelldb-word "A-Za-z0-9+-" prefix))
	 (shell-strings (shells-shell-strings-matching-shell-strings
			 shell-string command regexp subexp shell)))
    (FM-offer-corrections shell-string (shells-sort shell-strings)
			  shells-prompt-before-correction)))



(defun shells-dynamic-spell-as-filespec (incl-regexp excl-regexp pref-regexp
					 dir-p exec-p)
  "Dynamically spell check at point according to the filespec.
Only the text after PREF-REGEXP is completed.  All other arguments are ignored.
See `shells-pathnames-matching-pathname'."
  (let* ((filespec (shelldb-word "~/A-Za-z0-9+@:_.$#,={}-" pref-regexp))
	 (paths (shells-pathnames-matching-pathname filespec)))
    (FM-offer-corrections filespec (save-match-data (shells-sort paths))
			  shells-prompt-before-correction)))

(defsubst shells-concat (item1 item2)
  "Return path comprising ITEM1 and ITEM2."
  (if (string-equal item1 "")
      item2
    (concat (file-name-as-directory item1) item2)))


(defun shells-pathnames-matching-pathname (path &optional cash interest)
  "Return list of paths that fuzzily match PATH.
If CASH is nil, use `shells-fuzzy-tolerance' for fuzzy-money.
If INTEREST is nil, use `shells-fuzzy-brownie-points' for fuzzy-interest.

CASH is the \"tolerance\" or \"lenience\" used when matching path constituents.
Before matching a path constituent, INTEREST is added to CASH, though CASH will
not grow to be greater than the initial amount of CASH (like my bank, really).

Matching is fairly exhaustive.  For example, if you match with the path
\"/user/spool/mail\", this function can return the correct \"/usr/spool/mail\"
even if the directory \"/user\" exists (but \"/user/spool/mail\" does not).
It also resolves usernames and variables.

For example:

 (shells-pathnames-matching-pathname \"$HME/emacs\" 2 1)
      => ((1 . \"$HOME/Emacs\") (1 . \"$HOME/.emacs\"))

 (shells-pathnames-matching-pathname \"/user/spool/male\" 2 1)
      => ((1 . \"/usr/spool/mail\"))

Though you need a lot of tolerance to match this one:

 (shells-pathnames-matching-pathname \"/user/spoilt/male\" 3 2)
      => ((1 . \"/usr/spool/mail\"))

Matches can be sorted:

 (shells-sort (shells-pathnames-matching-pathname \"~/Emacs/shels.el\" 2 1))
      => (\"~/Emacs/shell.el\" \"~/Emacs/shells.el\" \"~/Emacs/shellt.el\"
          \"~/Emacs/shellc.el\")

 (shells-sort (shells-pathnames-matching-pathname \"~tim/~duck/~hurry\" 2 1))
      => (\"~tom/~dick/~harry\")

 (shells-sort (shells-pathnames-matching-pathname \"/usr/passwd\" 3 0))
      => (\"/bin/passwd\" \"/etc/passwd\")

Note that path constituent delimiters (\"/\"s) and home directory symbols (\"~\"s)
are assumed to be correct.

See also `shells-things-matching-thing' and `shells-spell-check'."
  (let* ((pathlist (let ((comint-delimiter-argument-list '(?/)))
		     (nreverse (delete "/" (comint-delim-arg path)))))
	 (cash (or cash shells-fuzzy-tolerance))
	 (interest (or interest shells-fuzzy-brownie-points))
	 (newlist (list (cons cash (if (string-match "^/" path) "/" ""))))
	 (targetdepth (1- (length pathlist))) (targetitem nil)
	 (depth 0) (oldlist ()))
    ;; We build a list (newlist/oldlist) of elements of the form (money . item)
    ;; Each element is expanded with the next level down the pathlist, and
    ;; those whose money falls below zero are not added to the new list.
    (while (<= depth targetdepth)
      ;; Using available matches in oldlist, make newlist of elements that
      ;; match(-ish) targetitem.
      (setq oldlist newlist newlist () targetitem (nth depth pathlist))
      (while oldlist
	(let* ((money (min cash (+ (car (car oldlist)) interest)))
	       (directory (cdr (car oldlist)))
	       (matches 
		;; Get all the elements that fuzzily match targetitem.
		(cond ((string-equal targetitem "~")		; Special case
		       (list (cons money targetitem)))
		      ((string-match "^~" targetitem)		; User name
		       (shells-usernames-matching-username targetitem money))
		      ((string-match "^\\$" targetitem)		; Variable
		       (shells-variables-matching-variable targetitem money))
		      (t					; File name
		       (shells-filenames-matching-filename
			targetitem directory money (= depth targetdepth))))))
	  ;; Prepend the directory to each match, and add them to newlist.
	  (mapcar (function (lambda (i)
			      (setcdr i (shells-concat directory (cdr i)))))
		  matches)
	  (setq newlist (nconc matches newlist) oldlist (cdr oldlist))))
      (setq depth (1+ depth)))
    newlist))


(defun shells-things-matching-thing (thing candidates &optional cash)
  "Return a list of things matching THING given CANDIDATES and CASH.
Returned list contains elements in the form (fuzzy-money . thing) and may be
reduced to a list of things using `shells-sort'.
If CASH is nil, the value of `shells-fuzzy-tolerance' used for fuzzy-money.

The fuzzy-money is spent by `shells-balance', and only items still in credit
are returned.

For example:

 (shells-things-matching-thing \"pigs\" '(\"pugs\" \"pugz\" \"snout\" \"trough\") 2)
      => ((0 . \"pugz\") (1 . \"pugs\"))

 (shells-sort
  (shells-things-matching-thing \"pigs\" '(\"pugs\" \"pugz\" \"snout\" \"trough\") 2))
      => (\"pugs\" \"pugz\")

See also `shells-spell-check' and `shells-fuzzy-tolerance'."
  (let ((thing (FM-string-to-char-list thing))
	(oldmoney (or cash shells-fuzzy-tolerance))
	candidate money newthing things)
    (while candidates
      (setq candidate (car candidates) candidates (cdr candidates)
	    money (shells-balance thing candidate oldmoney))
      (if (>= money 0)
	  (setq things (cons (cons money candidate) things))))
    things))


(defun shells-commands-matching-command (command &optional cash)
  "Return list of commands that fuzzily match COMMAND.
See `shells-things-matching-thing'."
  (let ((commands (uniq (sort (append (shelldb-elements 'command-aliases)
				      (shelldb-elements 'command-builtins)
				      (directory-files "." nil nil t)
				      (shelldb-elements 'command-executables))
			      'string-lessp) 'string-equal)))
    (shells-things-matching-thing command commands cash)))


;;(defun shells-hostnames-matching-hostname (hostname &optional cash)
;;  "Return list of host names that fuzzily match HOSTNAME.
;;See `shells-things-matching-thing'."
;;  (shells-things-matching-thing hostname (shelldb-elements 'hostnames) cash))


(defun shells-usernames-matching-username (username &optional cash)
  "Return list of user names that fuzzily match USERNAME.
USERNAME can be prefixed with \"~\".  See `shells-things-matching-thing'."
  (let ((usernames (if (save-match-data (string-match "^[^~]" username))
		       (shelldb-elements 'usernames)
		     (mapcar (function (lambda (user) (concat "~" user)))
			     (shelldb-elements 'usernames)))))
    (shells-things-matching-thing username usernames cash)))


(defun shells-shell-strings-matching-shell-strings (shell-string
						    command regexp subexp shell
						    &optional cash)
  "Return list shell-strings that fuzzily match SHELL-STRING.
See `shells-things-matching-thing'."
  (let ((shell-strings (save-match-data
			 (shelldb-shell-strings command regexp subexp shell))))
    (shells-things-matching-thing shell-string shell-strings cash)))


(defun shells-variables-matching-variable (variable &optional cash)
  "Return list of variables that fuzzily match VARIABLE.
VARIABLE can be prefixed with \"$\", \"$(\" or \"${\", and suffixed with \")\" or \"}\".
See `shells-things-matching-thing'."
  (let* ((dollar (if (string-match "^\\$" variable) "$" ""))
	 (prefix (cond ((string-match "[{}]" variable) "{")
		       ((string-match "[()]" variable) "(")
		       (t "")))
	 (suffix (cond ((string-equal prefix "{") "}")
		       ((string-equal prefix "(") ")")
		       (t "")))
	 (cash
	  (+ (or cash shells-fuzzy-tolerance)
	     (if (string-match "[{(].+[^})]$\\|[^{(].+[})]$" variable) 1 0)))
	 (variables
	  (mapcar (function (lambda (name) (concat dollar prefix name suffix)))
		  (append (shelldb-elements 'shell-variables)
			  (shelldb-elements 'environment-variables)))))
    (shells-things-matching-thing variable variables cash)))


(defun shells-filenames-matching-filename (filename directory cash file-p)
  "Return list of files that fuzzily match FILENAME in DIRECTORY.
DIRECTORY may contain variable or usernames, they are expanded without error.
If FILE-P non-nil, then returned files need not be directories.
See `shells-things-matching-thing'."
  (let* ((cash (or cash shells-fuzzy-tolerance))
	 (directory (if (string-equal directory "")
			default-directory
		      (condition-case nil
			  (let ((process-environment
				 (append (shelldb-db 'shell-variables)
					 (shelldb-db 'environment-variables))))
			    (substitute-in-file-name directory))
			(error ""))))
	 (files (and (not (string-equal directory ""))
		     (file-accessible-directory-p directory)
		     (directory-files directory nil nil t)))
	 file money newlist)
    (while files
      (setq file (car files) files (cdr files))
      (if (and (or file-p (file-directory-p (shells-concat directory file)))
	       (<= 0 (setq money (shells-balance filename file cash))))
	  (setq newlist (cons (cons money file) newlist))))
    newlist))

(defun shells-balance (string1 string2 money)
  "Return the balance for STRING1 and STRING2 given MONEY.
That is, given MONEY, return the cash left after spending on STRING2 to make it
like STRING1.  A negative value just means that an overspend is required; it
need not indicate by how much.

See `shells-things-matching-thing' and `FM-matchiness'."
  (let* ((len1 (length string1)) (len2 (length string2))
	 (debit (abs (- len1 len2))))
    (if (< money debit)
	-1				; There's no chance anyway
      (- money (- len1 (FM-matchiness string1 string2)) debit))))


(defun shells-sort (items)
  "Return sorted ITEMS according to their fuzzy money.
See `shells-things-matching-thing'."
  (nreverse (mapcar 'cdr (sort items 'car-less-than-car))))


;;;(uniq (sort (apply 'nconc (mapcar 'directory-files (cdr (reverse exec-path))))
;;;            'string-lessp) 'string-equal)
;;; would be quicker, but we aught to do it properly.
(defun build-command-executables ()
  "Builds the database `command-executables' by searching `exec-path'."
  (let* ((paths (delete "." (delete nil (cdr (reverse exec-path)))))
	 (cwd (file-name-as-directory (expand-file-name default-directory)))
	 (ignored-extensions
	  (and comint-completion-fignore
	       (mapconcat (function (lambda (x) (concat (regexp-quote x) "$")))
			  comint-completion-fignore "\\|")))
	 path comps-in-path file filepath executables)
    (message "Building command name list...")
    (while paths
      (setq path (file-name-as-directory (comint-directory (car paths)))
	    comps-in-path (and (file-accessible-directory-p path)
			       (directory-files path)))
      (while comps-in-path
	(setq file (car comps-in-path)
	      filepath (concat path file))
	(if (and (or (null ignored-extensions)
		     (not (string-match ignored-extensions file)))
		 (or (string-equal path cwd)
		     (not (file-directory-p filepath)))
		 (or (null shell-completion-execonly)
		     (file-executable-p filepath)))
	    (setq executables (cons file executables)))
	(setq comps-in-path (cdr comps-in-path)))
      (setq paths (cdr paths)))
    (message "Building command name list...done")
    (shelldb-make 'command-executables
		  (uniq (sort executables 'string-lessp) 'string-equal))))


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

(defun shells-submit-feedback ()
  "Sumbit feedback on the shells package by electronic mail."
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   "Simon.Marshall@mail.esrin.esa.it" (concat "shells-" shells-version)
   '(shells-fuzzy-tolerance shells-fuzzy-brownie-points
     shells-prompt-before-correction shells-dynamic-spell-functions features)))

;;; Install ourselves.

(provide 'shells)

;;; shells.el ends here
