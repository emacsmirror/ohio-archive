;;; shellt.el --- active tracking for the shell.

;; Author: Simon Marshall <Simon.Marshall@mail.esrin.esa.it>
;; Keywords: processes tracking
;; Version: 1.01

;; LCD Archive Entry:
;; shellt|Simon Marshall|Simon.Marshall@mail.esrin.esa.it|
;; Directory etc. tracking for the emacs-19 shell buffer.|
;; 10-Aug-1994|1.01|~/packages/shellt.el.Z|
;; The archive is archive.cis.ohio-state.edu in /pub/gnu/emacs/elisp-archive.

;;; This file is not part of GNU Emacs.

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; This program Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Purpose:
;; 
;; Shellt is an active directory tracker for the Emacs-19 shell.  Shellt
;; appends directory querying commands to the input when input commands match
;; suspect commands such as "cd", "pushd", etc.  The returning information is
;; then extracted from the shell output and used to set Emacs' idea of the
;; current directory and directory stack.
;; 
;; Shellt is a passive shell variable and alias tracker.  It watches the input
;; for changes, and updates Emacs' idea of the list of variables and aliases
;; for completion purposes.  This is only enabled if shelldb is provided.

;; Caveat:
;;
;; Directory tracking is *active*, not *passive* like shell.el, so when it
;; screws up it does so actively, by putting unwanted text in your input to the
;; shell.  Generally it will only be a problem if you do something like:
;;
;; prompt% mail myboss
;; Subject: My Pay Rise
;; I do loads of work and what do I get?  Peanuts and a tacky clock after fifty
;; years.  I want money.  In fact, I want all your money.  And I want it now,
;; cd (that's cash on delivery, get-face).
;; Love, Si.
;; ^D
;;
;; Note that the final line of the message begins (i.e., the word interpreted
;; as a command) with the word "cd".  Then:
;;
;; prompt% lpr -Pboss resignation-letter.ps
;;
;; Since under Emacs you generally don't need to invoke interactive commands
;; from the shell prompt, you shouldn't get hurt.

;; Installation:
;; 
;; To use, put in your ~/.emacs:
;;
;; (eval-after-load "shell" '(load "shellt"))

;; Help:
;;
;; Try M-x shellt-help.

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
;;     - M-x shellt-submit-feedback
;;     - Simon Marshall (Simon.Marshall@mail.esrin.esa.it)

(require 'shell)

(eval-when-compile
  (require 'reporter))

(defconst shellt-version "1.01"
  "Version number of the shellt package.")

;;; HELP!!!

(defun shellt-help ()
  "This package is intended to provide tracking for the shell.

The basic tracking is of directory information, and is active.  If the shelldb
package exists, shellt will also passively track variable and alias changes in
the input if `shellt-track-entities' is non-nil and with the shelldb package.

Note that because the directory tracker is active, if it makes a mistake it
does so actively.  You may find text inserted into the input to the process.
This problem occurs when you are running an interactive program from the shell
in the shell buffer, since the query is sent to that program and not the shell.

The moment when shellt actively queries the shell for directory information is
controlled by `shellt-directory-commands'.  The form of the query is controlled
by `shellt-directory-tag'.

Submit comments, bug reports, etc., with M-x shellt-submit-feedback.

Enjoy."
  (interactive)
  (let ((buf "*Help Shellt*"))
    (with-output-to-temp-buffer buf
      (princ (documentation 'shellt-help) (get-buffer buf)))))

;; Include "pwd" and "dirs" to force display of the directory stack.
(defvar shellt-directory-commands "cd\\|chdir\\|popd\\|pushd\\|pwd\\|dirs"
  "*Regexp to match directory changing commands.
If a command in the input matches this regexp, then it has a directory
information command appended to it when sent to the shell.  Strings in this
regexp need not actually be directory changing commands, of course.

Set to \"\" if you want the shell to query directory information on each input.
See `shellt-input-sender'.")

(defvar shellt-directory-tag "IgnoreThisLineOfShelltDirectoryInfo"
  "String to tag directory information.
Should be a string that is unlikely to appear otherwise.")

(defvar shellt-track-entities t
  "Non-nil if shellt should track changes to shell entities.
The package shelldb must also be loaded.")

(defun shell-mode ()
  "Major mode for interacting with an inferior shell.
Return after the end of the process' output sends the text from the 
    end of process to the end of the current line.
Return before end of process output copies the current line (except
    for the prompt) to the end of the buffer and sends it.
M-x send-invisible reads a line of text without echoing it, and sends it to
    the shell.  This is useful for entering passwords.

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

cd, pushd and popd commands given to the shell are watched by Emacs to keep
this buffer's default directory the same as the shell's working directory.
M-x dirs queries the shell and resyncs Emacs' idea of what the current 
    directory stack is.
M-x dirtrack-toggle turns directory tracking on and off.

set, unset, alias and unalias commands given to the shell are watched by Emacs
to keep this buffer's `shell-variables', `environment-variables' and
`command-aliases' the same as the shell's.  This option is only enabled if
`shelldb' is provided.

\\{shell-mode-map}
Customization: Entry to this mode runs the hooks on `comint-mode-hook' and
`shell-mode-hook' (in that order).  Before each input, the hooks on
`comint-input-filter-functions' are run.  After each shell output, the hooks
on `comint-output-filter-functions' are run.

Variable `shellt-directory-commands' controls which commands cause the shell to
be queried for the state of the directory stack.

Functions `shellt-shell-variable-tracker' and `shellt-command-alias-tracker'
are added to the hook `comint-input-filter-functions'.

Variables `comint-completion-autolist', `comint-completion-addsuffix',
`comint-completion-recexact' and `comint-completion-fignore' control the
behavior of file name, command name and variable name completion.  Variable
`shell-completion-execonly' controls the behavior of command name completion.
Variable `shell-completion-fignore' is used to initialise the value of
`comint-completion-fignore'.

Variables `comint-input-ring-file-name' and `comint-input-autoexpand' control
the initialisation of the input ring history, and history expansion.

Variables `comint-output-filter-functions', a hook, and
`comint-scroll-to-bottom-on-input' and `comint-scroll-to-bottom-on-output'
control whether input and output cause the window to scroll to the end of the
buffer."
  (interactive)
  (comint-mode)
  (setq major-mode 'shell-mode)
  (setq mode-name "Shell")
  (use-local-map shell-mode-map)
  (setq comint-prompt-regexp shell-prompt-pattern)
  (setq comint-completion-fignore shell-completion-fignore)
  (setq comint-delimiter-argument-list shell-delimiter-argument-list)
  (setq comint-dynamic-complete-functions shell-dynamic-complete-functions)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start comint-prompt-regexp)
  (make-local-variable 'shell-dirstack)
  (setq shell-dirstack nil)
  (setq shell-last-dir nil)
  (make-local-variable 'shell-dirtrackp)
  (setq shell-dirtrackp t)
  (setq comint-input-sender 'shellt-input-sender)
  (if (and shellt-track-entities (featurep 'shelldb))
      (progn
	(add-hook 'comint-input-filter-functions 'shellt-variable-tracker)
	(add-hook 'comint-input-filter-functions 'shellt-command-alias-tracker)
	))
  (add-hook 'comint-output-filter-functions 'shell-directory-tracker)
  (setq comint-input-autoexpand shell-input-autoexpand)
  (let ((process (get-buffer-process (current-buffer))))
    ;; Process-dependent assignments.
    (set-process-sentinel process 'shellt-process-sentinel)
    ;; shell-dependent assignments.
    (let ((shell (file-name-nondirectory (car (process-command process)))))
      (setq comint-input-ring-file-name
	    (or (getenv "HISTFILE")
		(cond ((string-equal shell "bash") "~/.bash_history")
		      ((string-equal shell "ksh") "~/.sh_history")
		      (t "~/.history"))))
      (setq shell-dirstack-query
	    (if (string-match "^k?sh$" shell) "pwd" "dirs"))))
  (run-hooks 'shell-mode-hook)
  (comint-read-input-ring t))

;;; Directory tracking
;;; ===========================================================================
;;; We replace `comint-input-sender' with our own, and we prepend the
;;; function `shell-directory-tracker' to `comint-output-filter-functions'.
;;; The former forces the shell to print directory information when input
;;; commands match `shellt-directory-commands', and the latter extracts it
;;; from the output.
;;;
;;; It's not perfect, since output may occur in non-line aligned chunks, and
;;; therefore directory information may not be recognised.  The fact that the
;;; directory stack will not be updated under these circumstances is no big
;;; deal, since changes don't happen that often either, but the directory
;;; information will remain in the output.  This contravenes the Principal of
;;; Least Astonishment.
;;;
;;; But, in the words of Olin Shivers:
;;; The solution is to relax, not stress out about it, and settle for
;;; a hack that works pretty well in typical circumstances. Remember
;;; that a half-assed solution is more in keeping with the spirit of Unix, 
;;; anyway. Blech.

(defun shellt-input-sender (process string)
  "Function for sending to PROCESS input STRING.
This sends STRING, plus a directory information command using the shell command
`shell-dirstack-query' if `shellt-input-matches-directory-commands' returns
non-nil, plus a newline.
The directory information command is constructed by:

 (format \" ; echo %s `%s`\" shellt-directory-tag shell-dirstack-query)

Lines matching this string are assumed to contain directory information."
  (let ((dir-cmd (format " ; echo %s `%s`"
			 shellt-directory-tag shell-dirstack-query)))
    (if (or (null shell-dirstack-query)
	    (string-match "^\\s *$" shell-dirstack-query)
	    (not (shellt-input-matches-directory-commands)))
	(comint-send-string process string)
      (comint-send-string process (concat string dir-cmd)))
    (comint-send-string process "\n")))

(defun shellt-input-matches-directory-commands ()
  "Return non-nil if the input matches directory commands.
Input must not be split-line (i.e., the line must not end with \"\\\").
The input assumed to lie on the previous line, and is parsed using
`shell-forward-command' and matched using `shellt-directory-commands'."
  (save-excursion
    (end-of-line 0)
    (if (= (preceding-char) ?\\)
	nil
      (comint-bol nil)
      (while (and (not (eolp))
		  (not (string-match shellt-directory-commands
				     (comint-arguments (shellt-input) 0 0))))
	(shell-forward-command 1))
      (string-match shellt-directory-commands
		    (comint-arguments (shellt-input) 0 0)))))

(defun shellt-input ()
  "Return the input after point.
See `shell-forward-command'."
  (buffer-substring
   (point) (save-excursion (shell-forward-command 1) (point))))


(defun shellt-process-sentinel (process string)
  "Insert in PROCESS's buffer an appropriate message from STRING.
Also calls function `comint-write-input-ring' if STRING matches \"finished\".

This function should be PROCESS's `process-sentinel'."
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert (format "\n\nProcess %s %s" (process-name process) string))
    (if (string-match "finished" string)
	(comint-write-input-ring))))

(defun shell-directory-tracker (output)
  "Tracks directory information within the output in the buffer.
Any text that matches directory information is removed from the buffer.
See `shellt-input-sender'.

Note that we parse the buffer, not OUTPUT, since we will remove any directory
information string from the buffer that we find."
  (if shell-dirtrackp
      (let ((dir-cmd (format "^%s \\(.*\\)$" shellt-directory-tag)))
	(save-excursion
	  ;; comint-last-input-end would be more reliable, but less efficient.
	  (goto-char comint-last-output-start)
	  ;; Look for the dir-cmd.
	  (while (re-search-forward dir-cmd nil t)
	    (let ((dir-info (match-string 1)))
	      ;; Delete line (and preceding newline) and process dir-info.
	      (delete-region (1- (match-beginning 0)) (match-end 0))
	      (shellt-set-dirstack dir-info t)
	      (shell-dirstack-message)))))))

(defun shell-resync-dirs ()
  "Resync the buffer's idea of the current directory stack.
This command queries the shell with the command bound to `shell-dirstack-query'
(usually \"dirs\"), reads the next line output and parses it to form the new
directory stack.  DON'T issue this command unless the buffer is at a shell
prompt.  Also, note that if some other subprocess decides to do output
immediately after the query, its output will be taken as the new directory
stack -- you lose. If this happens, just do the command again."
  (interactive)
  (if (and shell-dirstack-query
	   (not (string-match "\\`\\s *\\'" shell-dirstack-query)))
      (let* ((proc (get-buffer-process (current-buffer)))
	     (pmark (process-mark proc)))
	(goto-char pmark)
	(insert shell-dirstack-query) (insert "\n")
	(sit-for 0)  	                ; force redisplay
	(comint-send-string proc shell-dirstack-query) 
	(comint-send-string proc "\n")
	(set-marker pmark (point))
	(let ((pt (point)))             ; wait for 1 line
	  ;; Extra newline prevents the user's pending input from spoofing us.
	  (insert "\n") (backward-char 1)
	  (while (not (looking-at ".+\n"))
	    (accept-process-output proc)
	    (goto-char pt)))
	(goto-char pmark) (delete-char 1) ; remove the extra newline
	;; That's the dirlist. grab it & parse it.
	(shellt-set-dirstack (substring (match-string 0) 0 -1) nil))))

(defun shellt-set-dirstack (dl quiet)
  (let* ((dl-len (length dl))
	 (ds '())			; new dir stack
	 (i 0))
    (while (< i dl-len)
      ;; regexp = optional whitespace, (non-whitespace), optional whitespace
      (string-match "\\s *\\(\\S +\\)\\s *" dl i) ; pick off next dir
      (setq ds (cons (file-name-as-directory (match-string 1 dl)) ds))
      (setq i (match-end 0)))
    (let ((ds (nreverse ds)))
      (condition-case nil
	  (progn (cd (car ds))
		 (setq shell-dirstack (cdr ds))
		 (if (not quiet)
		     (shell-dirstack-message)))
	(error (message "Couldn't cd."))))))

(defun shellt-variable-tracker (input)
  "Tracks shell variable changes in the buffer.
If the command is \"rehash\", the next reference to the shell database
`command-executables' causes it to be reinitialised.
If the variable is \"path\" or \"PATH\", the other variable is also set, and
the next reference to the shell database `command-executables' causes it to be
reinitialised.
Note that we parse the buffer, not INPUT, since we parse multiple commands."
  (save-excursion
    (end-of-line 0)
    (let ((end (point)))
      (comint-bol nil)
      (while (< (point) end)
	(let* ((input (buffer-substring
		       (point) (progn (shell-forward-command 1) (point))))
	       (comint-delimiter-argument-list '(?=))
	       (command (comint-arguments input 0 0))
	       (variable (comint-arguments input 1 1)))
	  (cond ((string-equal command "set")
		 (shelldb-set 'shell-variables variable
			      (comint-arguments input 3 nil)))
		((string-equal command "unset")
		 (shelldb-set 'shell-variables variable))
		((string-equal command "setenv")
		 (shelldb-set 'environment-variables variable
			      (comint-arguments input 2 nil)))
		((string-equal command "unsetenv")
		 (shelldb-set 'environment-variables variable))
		((string-equal command "rehash")
		 (shelldb-make 'command-executables nil))))))))

(defun shellt-command-alias-tracker (input)
  "Tracks command alias changes in the buffer.
Note that we parse the buffer, not INPUT, since we parse multiple commands."
  (save-excursion
    (end-of-line 0)
    (let ((end (point)))
      (comint-bol nil)
      (while (< (point) end)
	(let* ((input (buffer-substring
		       (point) (progn (shell-forward-command 1) (point))))
	       (command (comint-arguments input 0 0)))
	  (cond ((string-equal command "alias")
		 (shelldb-add 'command-aliases (comint-arguments input 1 1)))
		((string-equal command "unalias")
		 (shelldb-remove 'command-aliases
				 (comint-arguments input 1 1)))))))))

;;; Miscellaneous.

(defun shellt-submit-feedback ()
  "Sumbit feedback on the shellt package by electronic mail."
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   "Simon.Marshall@mail.esrin.esa.it" (concat "shellt-" shellt-version)
   '(shellt-directory-commands shellt-directory-tag features)))

;;; Make sure we have a few functions even if we don't have shelldb.
(or (fboundp 'match-string)
    (defun match-string (n &optional string)
      "Return the matched grouping N from STRING.
If STRING is not given, use the current buffer.  See `string-match'."
      (if (stringp string)
	  (substring string (match-beginning n) (match-end n))
	(buffer-substring (match-beginning n) (match-end n)))))

;;; Make obsolete, for what it's worth.
(mapcar (function (lambda (v)
		    (make-obsolete-variable v "Variable is no longer used.")))
	'(shell-popd-regexp shell-pushd-regexp shell-pushd-tohome
	  shell-pushd-dextract shell-pushd-dunique shell-cd-regexp))

(mapcar (function (lambda (f)
		    (make-obsolete f "Function is no longer used.")))
	'(shell-process-popd shell-prefixed-directory-name shell-process-cd
	  shell-process-pushd shell-extract-num))

;;; Install ourselves.

(provide 'shellt)

;;; shellt.el ends here
