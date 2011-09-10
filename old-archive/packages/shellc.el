;;; shellc.el --- customisable context-based completion for the shell.

;; Author: Simon Marshall <Simon.Marshall@mail.esrin.esa.it>
;; Keywords: processes completion
;; Version: 1.03

;; LCD Archive Entry:
;; shellc|Simon Marshall|Simon.Marshall@mail.esrin.esa.it|
;; Customisable context-based completion for the emacs-19 shell buffer.|
;; 06-Dec-94|1.03|~/packages/shellc.el.Z|
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
;; Shellc is a customisable context-based completion package for the emacs-19
;; shell.  Shellc already knows how to complete many command-line options, but
;; can also complete command-line option arguments, specific command arguments,
;; and general command arguments.  Functions are provided to customise
;; completion is a relatively painless way.

;; Installation:
;; 
;; To use, put in your ~/.emacs:
;;
;; (eval-after-load "shell" '(load "shellc"))
;;
;; In a shell buffer, use TAB as usual.  Provided with this package is general
;; completion for command names (including aliases and builtins) host names,
;; user names, shell and environment variables, mail names and file specs, and
;; basically anything if the list of completions can be extracted from a shell
;; command, on top of the completion provided by shell.el.

;; Help:
;;
;; Try M-x shellc-help and M-x shelldb-help.

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
;;     - M-x shellc-submit-feedback
;;     - Simon Marshall (Simon.Marshall@mail.esrin.esa.it)

;; History:
;;
;; - 1.00--1.01:
;;   Option argument structure goes back into option list, and from hairy
;;   regexps to strings.  This solves the gapless-option-argument problem and
;;   knowing exactly what options are available.
;;   There are now completion functions for option arguments.
;;   Edited command options, of course.  Deleted single letter option commands.
;;   Added version number and support for reporter.
;;   Uses separate variables for shell and environment variable completion.
;;   Added command and alias builtin completion.
;;   Kevin Broadey <KevinB@bartley.demon.co.uk> fixed shellc-hostnames regexp
;;   for brain-dead versions of `current-word'.
;;   Split up shellc into shellc and shelldb.

(require 'shelldb)

(eval-when-compile
  (require 'mailalias)
  (require 'reporter))

(defconst shellc-version "1.02"
  "Version number of the shellc package.")

;;; HELP!!!

(defun shellc-help ()
  "This package is intended to provide context-based completion for the shell.

Knowledge about general shell and contextual information is contained within
the shelldb package.  For further information on shelldb, try M-x shelldb-help.
For futher information on default shell completion and shell completion
behaviour, see `shell-mode'.

Completion is automatic; typing TAB in a shell buffer should complete the
entity before point depending on the entity's context.  For example:

% find -uTAB siTAB -grTAB stTAB

etc., might result in completion to leave the command line as:

% find -user simon -group staff

Provided with this package is general completion for command names (including
aliases and builtins) host names, user names, shell variables, environment
variables, mail names and file specs, and basically anything if the list of
completions can be extracted from a shell command, on top of the completion
provided by shell.el.  See the functions `shellc-dynamic-complete-*'.

Submit comments, bug reports, etc., with M-x shellc-submit-feedback.

Enjoy."
  (interactive)
  (let ((buf "*Help Shellc*"))
    (with-output-to-temp-buffer buf
      (princ (documentation 'shellc-help) (get-buffer buf)))))

;;; Functions for context completion

(defun shellc-dynamic-complete-command-option ()
  "Dynamically complete the command option.
Options are assumed to begin with \"-\"."
  (let ((input (shelldb-input)))
    (if (string-match "^\\(\\S +\\)\\s +\\(\\S +\\s +\\)*\\(-\\S *\\)$" input)
	(let* ((command (shelldb-command (match-string 1 input)))
	       (option (match-string 3 input))
	       (options (mapcar (function (lambda (o) (or (car-safe o) o)))
				(nth 1 (shelldb-command-arguments command))))
	       (completed
		(if options
		    (let ((comint-completion-addsuffix nil))
		      (message "Completing command option name...")
		      (comint-dynamic-simple-complete option options)))))
	  (if (and (memq completed '(sole shortest))
		   comint-completion-addsuffix)
	      (let* ((input (shelldb-input))
		     (option (and (string-match "^\\(\\S +\\)\\s +\\(\\S +\\s +\\)*\\(-\\S *\\)$" input)
				  (match-string 3 input)))
		     (options (nth 1 (shelldb-command-arguments command))))
		(if (or (member option options)
			(and (assoc option options)
			     (nth 1 (assoc option options))))
		    (insert " "))))
	  completed))))


(defun shellc-dynamic-complete-command-option-argument ()
  "Dynamically complete the command option argument.
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
	       (completed (and arglist
			       (shellc-perform-completion
				"Completing command option argument%s..."
				argument arglist))))
	  ;; Any error to report?
	  (if (stringp completed)
	      (message "No completions%s as %s"
		(if (string-equal argument "") "" (concat " of " argument))
		completed))
	  completed))))


(defun shellc-dynamic-complete-command-option-extended ()
  "Dynamically complete the extended command option argument.
Options are assumed to begin with \"-\" and there is no gap between option and
argument.  See `shelldb-command-arguments'."
  (let ((input (shelldb-input)))
    (if (string-match "^\\(\\S +\\)\\s +\\(\\S +\\s +\\)*\\(-\\S *\\)$" input)
	(let* ((command (shelldb-command (match-string 1 input)))
	       (option (match-string 3 input))
	       (options (nth 1 (shelldb-command-arguments command)))
	       (completed nil) (argument nil) (optext '()))
	  (while (and options (not completed))
	    (setq optext (car options) options (cdr options))
	    (if (and (listp optext) (not (nth 1 optext))
		     (string-match (concat "^" (nth 0 optext)) option))
		(setq argument (substring option (match-end 0))
		      completed (shellc-perform-completion
				 "Completing command option%s..."
				 argument optext))))
	  ;; Any error to report?
	  (if (stringp completed)
	      (message "No completions%s as %s"
		(if (string-equal argument "") "" (concat " of " argument))
		completed))
	  completed))))


(defun shellc-dynamic-complete-command-argument ()
  "Dynamically complete the command argument.
See `shelldb-command-arguments'."
  (let ((input (shelldb-input)))
    (if (string-match "^\\(\\S +\\)\\s +\\(\\S +\\s +\\)*\\(\\S *\\)$" input)
	(let* ((command (shelldb-command (match-string 1 input)))
	       (argument (match-string 3 input))
	       (arguments (nth 2 (shelldb-command-arguments command)))
	       (arglist "") (completed nil))
	  (while (and arguments (not completed))
	    (setq arglist (car arguments) arguments (cdr arguments))
	    (if (and (or (not (stringp (nth 0 arglist)))
			 (string-match (nth 0 arglist) input))
		     (or (not (stringp (nth 1 arglist)))
			 (not (string-match (nth 1 arglist) input))))
		(setq completed (shellc-perform-completion
				 "Completing command argument%s..."
				 argument arglist))))
	  ;; Any error to report?
	  (if (stringp completed)
	      (message "No completions%s as %s"
		(if (string-equal argument "") "" (concat " of " argument))
		completed))
	  completed))))

(defvar shellc-completion-functions
  '((alias . shellc-dynamic-complete-as-command-alias)
    (command . shellc-dynamic-complete-as-command)
    (filespec . shellc-dynamic-complete-as-filespec)
    (hostname . shellc-dynamic-complete-as-hostname)
    (mailname . shellc-dynamic-complete-as-mailname)
    (username . shellc-dynamic-complete-as-username)
    (variable . shellc-dynamic-complete-as-variable)
    (shell-string . shellc-dynamic-complete-as-shell-string))
  "Cons pairs of the form (TYPE . COMPLETION-FUNCTION).
Used to map between entity type and the function required to complete it.
See `shellc-perform-completion'.")

(defun shellc-perform-completion (mformat item clist)
  "Perform the completion for ITEM using CLIST.
MFORMAT is the message format to use.  The message is displayed with \"barf\"
>from CLIST.  See `shelldb-command-arguments' for details on CLIST.
See `shellc-completion-functions' for the functions used to complete ITEM."
  (let ((barf (nth 2 clist)) (complete (nth 3 clist)))
    (message mformat (if (stringp barf) (concat " as " barf) ""))
    (or (cond ((null complete)
	       nil)
	      ((symbolp complete)
	       (let ((entry (assq complete shellc-completion-functions)))
		 (apply (or (cdr-safe entry) 'ignore) (nthcdr 4 clist))))
	      (t
	       (comint-dynamic-simple-complete item complete)))
	barf)))

;;; Functions for completion of specific entities

(defun shellc-dynamic-complete-hostname ()
  "Dynamically complete the host name at point.
This function is similar to `comint-dynamic-complete-filename', except that it
searches `shellc-hostnames' for completion candidates.

Returns non-nil if successful."
  (interactive)
  (let ((hostname (comint-word "@A-Za-z0-9+_.-")))
    (if (and hostname (string-match "@" hostname))
	(prog2 (message "Completing host name...")
	    (shellc-dynamic-complete-as-hostname)))))


(defun shellc-dynamic-complete-as-hostname (&optional prefix)
  "Dynamically complete at point as a host name.
This function is similar to `comint-dynamic-complete-filename', except that it
searches `shellc-hostnames' for completion candidates.

Returns non-nil if successful."
  (comint-dynamic-simple-complete (shelldb-word "A-Za-z0-9+_.-" prefix)
				  (shelldb-elements 'hostnames)))


(defun shellc-dynamic-complete-username ()
  "Dynamically complete the user name at point.
Completes if after a username beginning with \"~\".
See `shellc-dynamic-complete-as-username'."
  (interactive)
  (let ((username (comint-word "~A-Za-z0-9+_.-")))
    (if (and username (string-match "^~[^/]*$" username))
	(prog2 (message "Completing user name...")
	    (let ((completed (let ((comint-completion-addsuffix nil))
			       (shellc-dynamic-complete-as-username))))
	      (if (and comint-completion-addsuffix
		       (memq completed '(sole shortest)))
		  (insert "/"))
	      completed)))))


(defun shellc-dynamic-complete-as-username (&optional prefix)
  "Dynamically complete at point as a user name.
See `comint-dynamic-simple-complete' and `shellc-usernames'.
Returns non-nil if successful."
  (comint-dynamic-simple-complete (shelldb-word "A-Za-z0-9+_.-" prefix)
				  (shelldb-elements 'usernames)))


(defun shellc-dynamic-complete-variable ()
  "Dynamically complete the variable at point.
Completes if after a variable beginning with \"$\".
See `shellc-dynamic-complete-as-variable'."
  (interactive)
  (let ((variable (shell-match-partial-variable)))
    (if (and variable (string-match "^\\$" variable))
	(prog2 (message "Completing variable name...")
	    (shellc-dynamic-complete-as-variable nil)))))


(defun shellc-dynamic-complete-as-variable (type)
  "Dynamically complete at point as a variable.
If TYPE is `shell' use `shellc-shell-variables', if `environment' use
`shellc-environment-variables'.  Otherwise use both.
See `comint-dynamic-simple-complete'.

Returns non-nil if successful."
  (let* ((variables (cond ((eq type 'shell)
			   (shelldb-elements 'shell-variables))
			  ((eq type 'environment)
			   (shelldb-elements 'environment-variables))
			  (t (append
			      (shelldb-elements 'shell-variables)
			      (shelldb-elements 'environment-variables)))))
	 (var (or (shell-match-partial-variable) ""))
	 (variable (substring var (or (string-match "[^$({]\\|$" var) 0)))
	 (success (let ((comint-completion-addsuffix nil))
		    (comint-dynamic-simple-complete variable variables))))
    (if (memq success '(sole shortest))
	(let* ((var (shell-match-partial-variable))
	       (variable (substring var (string-match "[^$({]" var)))
	       (value (or (shelldb-get 'shell-variables variable)
			  (shelldb-get 'environment-variables variable)))
	       (protection (cond ((string-match "{" var) "}")
				 ((string-match "(" var) ")")
				 (t "")))
	       (suffix (cond ((or (not comint-completion-addsuffix)
				  (not (string-match "^\\$" var))) "")
			     ((and value (file-directory-p
					  (comint-directory value))) "/") 
			     (t " "))))
	  (insert protection suffix)))
    success))


(defun shellc-dynamic-complete-mailname ()
  "Dynamically complete the mail name at point.
See `shellc-dynamic-complete-as-mailname'.

Returns non-nil if successful."
  (interactive)
  (shellc-dynamic-complete-as-mailname))


(defun shellc-dynamic-complete-as-mailname ()
  "Dynamically complete at point as a mail name.
See `shellc-usernames' and `mail-aliases'.

Returns non-nil if successful."
  (require 'mailalias)
  (if (eq mail-aliases t)
      (build-mail-aliases))
  (comint-dynamic-simple-complete
   (or (comint-word "A-Za-z0-9+_.-") "")
   (append (mapcar 'car mail-aliases) (shelldb-elements 'usernames))))


(defun shellc-dynamic-complete-command ()
  "Dynamically complete the command at point.
See `shellc-dynamic-complete-as-command'.

Returns non-nil if successful."
  (interactive)
  (let ((command (comint-word "A-Za-z0-9+_.-")))
    (if (and command
	     (save-match-data (not (string-match "[~/]" command)))
	     (eq (match-beginning 0)
		 (save-excursion (shell-backward-command 1) (point))))
	(prog2 (message "Completing command...")
	    (shellc-dynamic-complete-as-command)))))


(defun shellc-dynamic-complete-as-command ()
  "Dynamically complete at point as a command.
This function uses (in order) and `shell-dynamic-complete-as-command',
`shellc-dynamic-complete-as-command-builtin', and
`shellc-dynamic-complete-as-command-alias' to complete the command.

Returns non-nil if successful."
  (or (shell-dynamic-complete-as-command)
      (shellc-dynamic-complete-as-command-builtin)
      (shellc-dynamic-complete-as-command-alias)))


(defun shellc-dynamic-complete-as-command-alias ()
  "Dynamically complete at point as a command alias.
See `shelldb-command-aliases'.

Returns non-nil if successful."
  (comint-dynamic-simple-complete (or (comint-word "A-Za-z0-9+_.-") "")
				  (shelldb-elements 'command-aliases)))


(defun shellc-dynamic-complete-as-command-builtin ()
  "Dynamically complete at point as a command builtin.
See `shelldb-command-builtins'.

Returns non-nil if successful."
  (comint-dynamic-simple-complete (or (comint-word "A-Za-z0-9+_.-") "")
				  (shelldb-elements 'command-builtins)))


(defun shellc-dynamic-complete-as-filespec (incl-regexp excl-regexp
					    pref-regexp dir-p exec-p)
  "Dynamically complete at point according to the filespec.
Only the text after PREF-REGEXP is completed.
See `shellc-files-matching-filespec'."
  (let* ((filename (substitute-in-file-name
		    (shelldb-word "~/A-Za-z0-9+@:_.$#,={}-" pref-regexp)))
         (pathdir (file-name-directory filename))
         (pathnondir (file-name-nondirectory filename))
         (directory (if pathdir (comint-directory pathdir) default-directory))
	 (completed (let ((comint-completion-addsuffix nil))
		      (comint-dynamic-simple-complete
		       pathnondir
		       (shellc-files-matching-filespec
			directory incl-regexp excl-regexp dir-p exec-p)))))
    (if (and comint-completion-addsuffix (memq completed '(sole shortest)))
	(insert (if (file-directory-p
		     (substitute-in-file-name
		      (shelldb-word "~/A-Za-z0-9+@:_.$#,={}-" pref-regexp)))
		    "/" " ")))
    completed))


(defun shellc-dynamic-complete-as-shell-string (command regexp subexp
						&optional prefix shell)
  "Dynamically complete at point as string using COMMAND.
If the regexp PREFIX matches the current word before point, only the text
following the match is used for completion.
See `shelldb-word' and `shelldb-shell-strings'."
  (comint-dynamic-simple-complete
   (shelldb-word "A-Za-z0-9+-" prefix)
   (shelldb-shell-strings command regexp subexp shell)))

;;; Misc fns.

(defun shellc-submit-feedback (command)
  "Sumbit feedback on the shellc package by electronic mail."
  (interactive "sRelevant shell command (RET if none)? ")
  (require 'reporter)
;  (let ((func (function (lambda (command)
;			  (let ((entry (shelldb-command-arguments command)))
;			    (reporter-dump-variable 'entry))))))
    (reporter-submit-bug-report
     "Simon.Marshall@mail.esrin.esa.it" (concat "shellc-" shellc-version)
;     (list (cons command func) 'features) nil nil
     '(features) nil nil
     (if (string-equal command "") nil (concat "Entry for " command)))
;    )
    )


(defun shellc-files-matching-filespec (dir incl excl dir-p exec-p)
  "Return list of files in DIR matching INCL but not EXCL.
Directories are always returned regardless.
If INCL and/or EXCL are nil, respective restrictions are not imposed.
If DIR-P non-nil, return directories only.
If EXEC-P non-nil, return executables only."
  (let ((incl-files (delete "." (delete ".." (directory-files dir))))
	(path (file-name-as-directory dir))
	(fign
	 (and comint-completion-fignore
	      (mapconcat (function (lambda (x) (concat (regexp-quote x) "$")))
			 comint-completion-fignore "\\|")))
	(files ()) (file ""))
    (while incl-files
      (setq file (car incl-files) incl-files (cdr incl-files))
      (if (or (file-directory-p (concat path file))
	      (and (or (null fign) (not (string-match fign file)))
		   (or (null incl) (string-match incl file))
		   (or (null excl) (not (string-match excl file)))
		   (not dir-p)
		   (or (null exec-p) (file-executable-p (concat path file)))))
	  (setq files (cons file files))))
    files))


;;(defun shellc-lessp (first second)
;;  (cond ((and (stringp first) (stringp second)) (string-lessp first second))
;;	((stringp first) t)
;;	((stringp second) nil)
;;	(t (string-lessp (car first) (car second)))))

;;; Really aught to be part of emacs?
(or (fboundp 'match-string)
    (defun match-string (n &optional string)
      "Return the matched grouping N from STRING.
If STRING is not given, use the current buffer.  See `string-match'."
      (if (stringp string)
	  (substring string (match-beginning n) (match-end n))
	(buffer-substring (match-beginning n) (match-end n)))))

;;; Install ourselves.  Additional hook order is important.
(let ((rem-hooks '(comint-replace-by-expanded-history
		   shell-dynamic-complete-command
		   shell-dynamic-complete-environment-variable))
      (add-hooks '(shellc-dynamic-complete-command
		   shellc-dynamic-complete-hostname
		   shellc-dynamic-complete-username
		   shellc-dynamic-complete-variable
		   shellc-dynamic-complete-command-argument
		   shellc-dynamic-complete-command-option-argument
		   shellc-dynamic-complete-command-option-extended
		   shellc-dynamic-complete-command-option
		   comint-replace-by-expanded-history
		   )))
  (while rem-hooks
    (remove-hook 'shell-dynamic-complete-functions (car rem-hooks))
    (setq rem-hooks (cdr rem-hooks)))
  (while add-hooks
    (add-hook 'shell-dynamic-complete-functions (car add-hooks))
    (setq add-hooks (cdr add-hooks))))

;;; Add to the menu-bar.
(let ((completion-map (lookup-key shell-mode-map [menu-bar completion])))
  (define-key-after completion-map [complete-shell-variable]
    '("Complete Shell Variable Name" . shellc-dynamic-complete-shell-variable)
    'complete-env-variable)
  (define-key-after completion-map [complete-user]
    '("Complete User Name" . shellc-dynamic-complete-username)
    'complete-file)
  (define-key-after completion-map [complete-host]
    '("Complete Host Name" . shellc-dynamic-complete-hostname)
    'complete-user)
  (define-key-after completion-map [complete-option]
    '("Complete Command Option" . shellc-dynamic-complete-command-option)
    'complete-shell-variable)
  (define-key-after completion-map [complete-option-argument]
    '("Complete Command Option Argument" .
      shellc-dynamic-complete-command-option-argument)
    'complete-option)
  (define-key-after completion-map [complete-argument]
    '("Complete Command Argument" . shellc-dynamic-complete-command-argument)
    'complete-option-argument))

(provide 'shellc)

;;; shellc.el ends here
