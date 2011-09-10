;; -*-Emacs-Lisp-*-
;; $Id: j-shell.el,v 2.1 1994/07/12 13:51:02 jthompso Exp $

;; Jim's Pretty-Good Shell Mode for GNU Emacs (J-Shell)
;; Copyright (C) 1992, 1993, 1994 James C. Thompson, jimt@sugar.neosoft.com

;; J-Shell is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; J-Shell is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; -----------------------------------------------------------
;;	  ***  CAUTION  ***  CAUTION  ***  CAUTION  ***
;; This package redefines the Emacs functions view-lossage and
;; recent-keys, as a means of protecting passwords entered in
;; jsh-mode.  Any packages or lisp code that depends on these
;; functions will probably lose.  This feature may be disabled
;; at the user's option; see the variable jsh-secure-mode.
;; -----------------------------------------------------------

;; NOTE: Version 2.1 fixes a problem in running J-Shell under FSF
;;       Emacs version 19.24 and later.  J-Shell 2.1 should also work
;;       with earlier versions of FSF Emacs 19, Lucid Emacs 19
;;       (XEmacs), and Emacs 18.

(defconst jsh-version  (substring "$Revision: 2.1 $" 11 -2)
  "$Id: j-shell.el,v 2.1 1994/07/12 13:51:02 jthompso Exp $

Report bugs to: James C. Thompson <jimt@neosoft.com>")

;; NOTE: To report bugs use the C-c C-b binding.  See file DOC, section BUGS.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User-configurable variables begin here:

(defvar jsh-prompt-pattern "[>%\\$][ \t]*"
  "*Matches the user's prompt.")

(defvar jsh-password-pattern "[Pp]assword: *$"
  "*The pattern that invokes password-parsing.")

(defvar jsh-home (concat "\\(" (getenv "HOME") "\\)")
  "*If non-nil, indicates string to substitute with ~ in mode-line dir.")

(defvar jsh-history-stack-size 256
  "*The number of commands to save in the history stack.")

;; Shell- and mode-startup

(defvar explicit-shell-file-name nil
  "*If non-nil, is file name to use for explicitly requested inferior shell.")

(defvar jsh-mode-hooks nil
  "*The hooks that are called when a buffer enters jsh-mode.")

(defvar jsh-start-hooks nil
  "*The hooks that are called when J-Shell starts the shell program.")

;; Filename Completion

(defvar jsh-completion-separator "\\s \\|[(|&;$=:<>]"
  "*These characters delimit strings for completion")

;; Command Completion

(defvar jsh-complete-commands t
  "*Non-nil means try to complete commands.")

(defvar jsh-command-preceders "[(|&;]"
  "*Characters that sometimes precede a command")

(defvar jsh-complete-command-threshold 2
  "*Only strings longer than this are completed as commands")

(defvar jsh-builtin-commands
  '(("pushd") ("popd") ("reorient"))
  "*Names of commands that don't exist as executable files.

This is a list of shell built-in commands and shell functions for the
command completion functions to add to the command completion list.")

;; Environment completion and substitution

(defvar jsh-complete-environment t
  "*Non-nil means attempt to complete environment variable names.")

(defvar jsh-complete-env-threshold 2
  "*Only strings longer than this are completed as environmant variables.")
  
(defvar jsh-expand-environment t
  "*Non-nil means expand environment variables after completing them.")

;; "Security"

(defvar jsh-secure-mode t
  "*Non-nil means disable view-lossage function to hide passwords")

(defvar jsh-secure-message
  "The functions which allow you to view recent keystrokes have been
hidden by j-shell, to protect passwords entered in shell buffers."
  "The message printed for view-lossage in \"secure\" mode.")

;; Signals

(defvar jsh-send-char-signals t
  "*If non-nil, send characters instead of signals to the shell;
for example, send C-c for interrupt instead of SIGINT.

If you prefer to run inferior shells via a pipe rather than a pty,
(that is, you set process-connection-type nil), or your system's ptys
are in short supply, then you should set this variable to nil so that
key bindings such as C-c C-c will work correctly.

If you prefer ptys, and they are in good supply on your system, then
the default value (t) is best.  Sending characters works correctly
with remote logins, whereas signals cannot be sent to remote shells.")

(defvar jsh-interrupt-char "\C-c"
  "*Character to send for interrupt.")

(defvar jsh-stop-char "\C-z"
  "*Character to send for stop")

(defvar jsh-quit-char "\C-\\"
  "*Character to send for quit")

(defvar jsh-eof-char "\C-d"
  "*Character to send for end-of-file")

;; Bang-regurgitation

(defvar jsh-regurgitate-bang-commands t
  "*Non-nil means replace ! commands in the command history
with the next line of shell output.")

(defvar jsh-dont-regurg-regex "\\([Ee]vent not found\\)\\|\\([Nn]o such event\\)"
  "*Shell output matching this expression is not regurgitated, even
though the previous input may have contained a shell-history character
(hard-coded for now to the bang character, '!').")

;; Alternate mode-line

(defvar jsh-use-alternate-modeline nil
  "*Non-nil means use mode-line containing host and directory.")

(defvar jsh-alternate-modeline '("--%b--  " global-mode-string "  " jsh-default-dir)
  "*A modeline to display buffer name and default directory.")

(defvar jsh-modeline-dir-len 64
  "*How much of the current directory to display in the mode line.")

(defvar jsh-chop-keep-beginning t
  "*Non-nil means keep initial \"/\" or \"~/\" in chopping directory names,
as a cue to whether the directory is beneath $HOME.")

(defvar jsh-chop-at-slash t
  "*Non-nil means chop directory names at \"/\" boundaries.")

;; Miscellany

(defvar jsh-buffer-base-name "j-shell"
  "*The name used for jsh buffers.")

(defvar jsh-greeting-file "/etc/motd"
  "*Specifies the file to be inserted at the top of the jsh buffer.
If nil, no file is inserted.")

;; User-configurable variables end here.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jsh-message-intro-pattern "\033EmAcS"
  "Matches the first part of any message from the shell to Emacs")

(defvar jsh-chdir-message-pattern "\\(\033EmAcScd \\(.*\\)\n\\)"
  "Matches shell message to indicate a new working directory.
This regular expression should have two levels of escaped parentheses:
the outer enclosing the whole expression, the inner enclosing the
expression that matches the directory name.")

(defvar jsh-rlogin-message-pattern "\\(\033EmAcShost \\(.*\\)\n\\)"
  "Matches shell message to indicate a new host.
This regular expression should have two levels of escaped parentheses:
the outer enclosing the whole expression, the inner enclosing the
expression that matches the host name.")

(defvar jsh-setenv-message-pattern "\\(\033EmAcSenv \\(.*\\)\n\\)"
  "Matches shell message to indicate a changed environment variable.
This regular expression should have three levels of escaped parentheses:
the outer enclosing the whole expression, the first inner pair enclosing
the expression that matches the variable name, and the second inner pair
enclosing the variable's value.")

(defvar jsh-eval-message-pattern "\\(\033EmAcSeval \\(.*\\)\n\\)"
  "Matches shell message indicating a lisp exression to evaluate.
This regular expression should have two levels of escaped parentheses:
the outer enclosing the whole expression, the inner enclosing the
expression to evaluate.")

(defconst jsh-version-string (concat "J-Shell, version " jsh-version))

(defconst jsh-help-address "jimt@neosoft.com")

(defconst jsh-running-epoch (boundp 'epoch::version))
(defconst jsh-running-lemacs (and (string-match "Lucid" emacs-version) t))

(defconst jsh-nonsense "'Twas Brynly, and the Summer toves...")

(defun j-shell (shell-program-name &optional new-buffer-name)
  "Run a shell inside a new J-Shell buffer.

Optional argument PROGRAM specifies name of shell or program to run.
Argument NAME specifies name of new buffer.  For example,

   (j-shell \"/usr/local/bin/tcsh\" \"tcsh\")

will run tcsh inside a new j-shell buffer named \"tcsh\".

If PROGRAM is nil or not specified, then the user is prompted for
a shell to run.  The default value for the prompt is determined by
evaluating the following, in order, until one evaluates non-nil:
explicit-shell-file-name, the environment variable ESHELL, and the
environment variable SHELL; if none of these is non-nil, then
\"/bin/csh\" is the default.

If NAME is nil or not specified, then jsh-buffer-base-name is used as
follows to determine the name of the buffer:  if jsh-buffer-base-name
is t, then the buffer name is taken from the name of the shell (using
file-name-nondirectory); if jsh-buffer-base-name is a string, then its
value is the name of the new buffer; if jsh-buffer-base-name is nil,
then the name of the new shell is \"j-shell\".

Use key sequence C-c C-b to submit a bug report via email."

  (interactive "P")

  (setq default-program
	(or explicit-shell-file-name
	    (getenv "ESHELL")
	    (getenv "SHELL")
	    "/bin/csh"))
  
  ;; Pick the program to run
  (setq program-name
	(if shell-program-name
	    (if (stringp shell-program-name)
		shell-program-name
	      (let ((prompt (format "Start shell: (default %s) "
				    (file-name-nondirectory default-program))))
		(read-file-name prompt (file-name-directory default-program)
				default-program t)))
	  default-program))
	      

  (setq temp-program-name (expand-file-name program-name))
  (if (file-exists-p temp-program-name)
      (setq program-name temp-program-name))

  ;; Pick the name of the new buffer.
  (setq buffer-name
	(if new-buffer-name
	    new-buffer-name
	  (if jsh-buffer-base-name
	      (if (eq jsh-buffer-base-name t)
		  (file-name-nondirectory program-name)
		jsh-buffer-base-name)
	    "j-shell")))
	    


  ;; Generate a new buffer
  (setq jshell (generate-new-buffer buffer-name))
  (switch-to-buffer jshell)

  ;; Insert the greeting file if it's readable
  (if (and jsh-greeting-file (file-readable-p jsh-greeting-file))
      (progn
	(insert-file-contents jsh-greeting-file)
	(goto-char (point-max))))
  
  (jsh-mode program-name)

  (message jsh-version-string))

(defun jsh-mode (program-name &rest args)
  "Jim's Pretty-Good Shell Mode, a major mode for running shells.

The following commands are available:
\\[jsh-beginning-of-line]		jsh-beginning-of-line
\\[jsh-complete]		jsh-complete
\\[jsh-edit]		jsh-edit
\\[jsh-send-input]		jsh-send-input

\\[jsh-expand]		jsh-expand
\\[jsh-completion-help]		jsh-completion-help
\\[jsh-send-quit]		jsh-send-quit
\\[jsh-beginning-of-command]		jsh-beginning-of-command
\\[jsh-submit-bug-report]		jsh-submit-bug-report
\\[jsh-send-interrupt]		jsh-send-interrupt
\\[jsh-send-eof]		jsh-send-eof
\\[jsh-kill-output]		jsh-kill-output
\\[jsh-password]		jsh-password
\\[jsh-show-output]		jsh-show-output
\\[jsh-start-program]		jsh-start-program
\\[jsh-kill-input]		jsh-kill-input
\\[jsh-send-stop]		jsh-send-stop

\\[jsh-hist-next]		jsh-hist-next
\\[jsh-hist-prev]		jsh-hist-prev
\\[jsh-move-next]		jsh-move-next
\\[jsh-move-prev]		jsh-move-prev
\\[jsh-send-input]		jsh-send-input

Entry to this mode calls the functions in jsh-mode-hooks.

J-Shell expects the shell to send strings specifying what working
directory the shell is in.  J-Shell tracks these strings to update the
buffer's default directory.  Example scripts are distributed with
J-Shell for ksh, csh, tcsh, bash, zsh, and the tcl shell.

Bug reports may be submitted via email using the key sequence C-c C-b.

Also see the function j-shell."

  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'jsh-mode)
  (setq mode-name "J-Shell")

  ;; Setup the mode-line
  (if jsh-use-alternate-modeline
      (setq mode-line-format jsh-alternate-modeline))

  ;; Make all the local variables...
  (make-local-variable 'jsh-program-name)
  (setq jsh-program-name program-name)

  (make-local-variable 'jsh-host)
					;  (setq jsh-host (getenv "HOST"))
  (setq jsh-host (system-name))

  (make-local-variable 'jsh-host)
  (setq jsh-host (system-name))

  (make-local-variable 'jsh-dir)
  (setq jsh-dir default-directory)

  (make-local-variable 'jsh-default-dir)
  (update-jsh-dir default-directory)

  (make-local-variable 'jsh-grab-history)
  (setq jsh-grab-history nil)

  (make-local-variable 'jshmark)
  (setq jshmark (make-marker))		;jshmark's position will get
					;set in jsh-start-program.

  (make-local-variable 'jsh-history)
  (setq jsh-history (make-vector jsh-history-stack-size nil))

  (make-local-variable 'jsh-hist-tos)
  (setq jsh-hist-tos (- jsh-history-stack-size 1))

  (make-local-variable 'jsh-hist-bos)
  (setq jsh-hist-bos 0)

  (make-local-variable 'jsh-hist-sp)
  (setq jsh-hist-sp jsh-hist-tos)

  (make-local-variable 'jsh-hist-interact)
  (setq jsh-hist-interact 0)

  (make-local-variable 'jsh-hist-at-tos)
  (setq jsh-hist-at-tos t)
  (make-local-variable 'jsh-hist-at-bos)
  (setq jsh-hist-at-bos t)

  (make-local-variable 'jsh-parsing-password)
  (setq jsh-parsing-password nil)

  (make-local-variable 'jsh-environment)
  (setq jsh-environment (jsh-make-environment))

  (make-local-variable 'jsh-message)
  (setq jsh-message nil)

  ;; PATH isn't cumulative, so it probably needs to be buffer-local
  (make-local-variable 'jsh-exec-path)
  (setq jsh-exec-path (jsh-make-exec-path (jsh-getenv "PATH")))

  (if jsh-running-lemacs
      (jsh-setup-lemacs-keymap)
    (jsh-setup-keymap))

  (run-hooks 'jsh-mode-hooks)
  
  (if (and (featurep 'dir-hist)
	   (not (dirhist-p 'jsh-dirhist)))
      (dh-make-dirhist 'jsh-dirhist))
  
  (jsh-start-program))

(defun jsh-start-program ()
  "Start or restart the shell program in the current buffer.  Called
automatically by jsh-mode to start the first shell; may be called
manually through C-c C-s to restart the shell if it dies or you kill
it accidentally.  Runs the hooks in jsh-start-hooks."

  (interactive)

  (make-local-variable 'jshproc)

  (if (and (setq jshproc (get-buffer-process (current-buffer)))
	   (setq jsh-status (process-status jshproc))
	   (or (eq jsh-status 'run) (eq jsh-status 'stop)))
      (error "A process is already running (or runnable) in this buffer.")

    ;; If a processs was running in this buffer, clean it up...
    (if jshproc (delete-process jshproc))

    ;; Start the new process
    (setq buff-name (buffer-name))
    (setq jshproc
	  (let* ((frame-width (if (fboundp 'frame-width)
				  (frame-width)
				(screen-width)))
		 (process-environment
		  (nconc
		   ;; the following stolen from Emacs 19.24's comint.el
		   (if (and (boundp 'system-uses-terminfo)
			    system-uses-terminfo)
		       (list "JSHELL=t" "TERM=unknown"
			     (format "COLUMNS=%d" frame-width))
		     (list "JSHELL=t" "TERM=emacs"
			   (format "TERMCAP=emacs:co#%d:tc=unknown"
				   frame-width)))
		   process-environment)))
	    (start-process buff-name jshell
			   jsh-program-name "-i")))

    ;; I cannot explain why the following call is necessary, unless it
    ;; is because there is an error in Emacs.  If two buffers, named
    ;; "tcsh" and "tcsh<2>", are present and we attempt to restart the
    ;; shell in the first buffer, the process will magically get
    ;; associated with the second, even though the call to start-
    ;; process explicitly specified the first.  This call negates that
    ;; odd behavior.
    (set-process-buffer jshproc (current-buffer))

    (set-process-filter jshproc 'jsh-filter)
    (set-marker jshmark (point))
    (run-hooks 'jsh-start-hooks))
  jshproc)

(defun jsh-setup-keymap ()
  ;; Set up the keyboard map for jsh mode.
  (if (and (boundp 'jsh-mode-map) jsh-mode-map)
      nil

    ;;Make a copy of the global map and make substitutions for all the
    ;;"printing" keys and for Del.  The substituted functions handle
    ;;the reading of passwords.
    (setq jsh-mode-map (copy-keymap global-map))
    (substitute-key-definition 'self-insert-command 'jsh-self-insert
			       jsh-mode-map)
    (substitute-key-definition 'delete-backward-char 'jsh-del-back
			       jsh-mode-map)

    ;;Install new keymaps into the mode map; this is necessary because
    ;;copy-keymap isn't fully recursive.  If we didn't make these
    ;;substitutions, our C-c and Meta (ESC) key definitions would
    ;;"leak" into other buffers.
    (define-key jsh-mode-map "\C-c" (make-sparse-keymap))
    (define-key jsh-mode-map "\C-[" (make-sparse-keymap))

    (fill-out-keymap))
  (use-local-map jsh-mode-map))

(defun jsh-substitute-key-definition (olddef newdef keymap &optional oldmap)
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
In other words, OLDDEF is replaced with NEWDEF where ever it appears.
Prefix keymaps reached from KEYMAP are not checked recursively;
perhaps they ought to be."
  (map-keymap (function (lambda (key binding)
			  (if (eq binding olddef)
			      (if oldmap
				  (define-key oldmap key newdef)
				(define-key keymap key newdef)))))
	      keymap))

(defun jsh-setup-lemacs-keymap ()
  ;; Set up the keyboard map for jsh mode.
  (if (and (boundp 'jsh-mode-map) jsh-mode-map)
      nil

    (setq jsh-mode-map (make-keymap))
    (jsh-substitute-key-definition 'self-insert-command 'jsh-self-insert
				   global-map jsh-mode-map)
    (jsh-substitute-key-definition 'delete-backward-char 'jsh-del-back
				   global-map jsh-mode-map)
    (fill-out-keymap)
    (define-key jsh-mode-map '[(meta return)] 'jsh-send-input))

  (use-local-map jsh-mode-map))

(defun fill-out-keymap ()
  ;;Fill out the Control-C map
  (define-key jsh-mode-map "\C-c\?"   'jsh-completion-help)
  (define-key jsh-mode-map "\C-c\C-\\" 'jsh-send-quit)
  (define-key jsh-mode-map "\C-c\C-a" 'jsh-beginning-of-command)
  (define-key jsh-mode-map "\C-c\C-b" 'jsh-submit-bug-report)
  (define-key jsh-mode-map "\C-c\C-c" 'jsh-send-interrupt)
  (define-key jsh-mode-map "\C-c\C-d" 'jsh-send-eof)
  (define-key jsh-mode-map "\C-c\C-o"	'jsh-kill-output)
  (define-key jsh-mode-map "\C-c\C-p" 'jsh-password)
  (define-key jsh-mode-map "\C-c\C-r"	'jsh-show-output)
  (define-key jsh-mode-map "\C-c\C-s" 'jsh-start-program)
  (define-key jsh-mode-map "\C-c\C-u" 'jsh-kill-input)
  (define-key jsh-mode-map "\C-c\C-w" 'backward-kill-word)
  (define-key jsh-mode-map "\C-c\C-z" 'jsh-send-stop)
  (define-key jsh-mode-map "\C-c\t"   'jsh-expand)
  (define-key jsh-mode-map "\C-c "	'jsh-display-dirhist)

  ;;Fill out the Escape map
  (define-key jsh-mode-map "\eC-m" 'jsh-send-input)
  (define-key jsh-mode-map "\eP" 'jsh-move-prev)
  (define-key jsh-mode-map "\eN" 'jsh-move-next)
  (define-key jsh-mode-map "\ep" 'jsh-hist-prev)
  (define-key jsh-mode-map "\en" 'jsh-hist-next)

  ;;Fill out the rest of the mode map
  (define-key jsh-mode-map "\C-m" 'jsh-send-input)
  (define-key jsh-mode-map "\t"   'jsh-complete)
  (define-key jsh-mode-map "\C-a" 'jsh-beginning-of-line)
  (define-key jsh-mode-map "\C-j" 'jsh-edit))

(defun jsh-output (output)
  (save-excursion
    (goto-char (marker-position jshmark))
    (insert-before-markers output)))

(defun jsh-filter (process output)

  (if (= (length output) 0)		;Skip empty strings
      nil

    ;; Set to nil the string to read and evaluate
    (setq jsh-eval-string nil)
    
    ;; Make a record of what the current buffer and case-fold are, so we
    ;; can set them back after this function is done.
    (setq current (current-buffer))
    (setq current-case-fold case-fold-search)

    ;; Use an unwind-protect form to ensure that we set the buffer and
    ;; search mode back when we're done.
    (unwind-protect
	(progn 
	  (setq case-fold-search nil)	;Set this to nil to force a
					;case-sensitive search.

	  ;; Set the buffer to the buffer that jsh is running in, and
	  ;; make searches case-sensitive
	  (set-buffer (process-buffer process))

	  ;; The very first thing to do is: if we have a partial message
	  ;; prepend it onto the output; eventually, this will form a whole
	  ;; message.  This is a pretty stupid, brute-force way to do
	  ;; things, but it's also simple.  With luck, we won't have too
	  ;; many overflowing messages.
	  (if jsh-message
	      (setq output (concat jsh-message output)))
	  (setq jsh-message nil)
	  
	  ;; All non-empty output turns off password mode (it keeps us
	  ;; from erroneously going into password mode); the
	  ;; exception, of course, is output that ends with the
	  ;; password pattern.  We check for that later.
	  (jsh-unpassword)

	  ;; Commands with ! in them are regurgitated by some shells
	  ;; before executing them; we grab such commands and place
	  ;; the regurgitated form into the history stack in place of
	  ;; the original form.

	  (if (and jsh-regurgitate-bang-commands jsh-grab-history)
	      (progn			;We're expecting a
					;regurgitation

		;; Before proceeding, attempt to avoid false
		;; regurgitations by comparing the last recorded
		;; command with the current line of output.  If they
		;; match up to the position of the bang, then do the
		;; regurgitation.  This won't catch all false matches,
		;; but ought to catch most.
		(setq last-command
		      (aref jsh-history (jsh-hist-inc jsh-hist-tos)))
		(setq bang-pos (string-match "!" last-command))
		(if (and bang-pos
			 (< bang-pos (length output))
			 (string= (substring last-command 0 bang-pos)
				  (substring output 0 bang-pos))

			 ;; This last clause is a bit of hackage for
			 ;; [t]csh, bash, and zsh 
			 (not (string-match jsh-dont-regurg-regex output)))
		    (progn
		      ;; Match the first line of shell output--it's the
		      ;; reguritation.
		      (string-match "^.*$" output)
		      (jsh-output (setq regurgitated-command
					(substring output
						   (match-beginning 0)
						   (match-end 0))))

		      ;; pop the last command off the stack and push the
		      ;; regurgitated command in its place
		      (setq jsh-hist-tos (jsh-hist-inc jsh-hist-tos))
		      (jsh-hist-push regurgitated-command)
		      (setq output (substring output (match-end 0)))))
		(setq jsh-grab-history nil)))
	  
	  ;; Look for any strings in the output indicating that the
	  ;; shell has changed its host or working directory, or that
	  ;; it has an expression to evaluate.

	  ;; The next two if constructs are written so as to handle
	  ;; messages that arrive pieces; the first one handles whole
	  ;; messages and can detect messages that are incomplete, but
	  ;; have an intact message intro ("ESC EmAcS").  The second
	  ;; is an attempt to handle messages that have only a piece
	  ;; of the intro (say, the ESC, but not the EmAcS); the piece
	  ;; is put back into jsh-message, so that processing the next
	  ;; time around might be a whole message.  These schemes seem
	  ;; to work, but probably need some serious rethinking; at a
	  ;; minimum, the two should probably be combined.

	  ;; If there's at least one message...
	  (if (string-match jsh-message-intro-pattern output)

	      ;; Do all the messages
	      (while
		  (setq from (string-match jsh-message-intro-pattern output))

		;; Make sure there's a complete message
		(if (setq to (string-match "\n" output from))
		    (progn
		      (setq jsh-eval-string (jsh-handle-message
					     output))

		      (setq output (concat (substring output 0 from)
					   (substring output (+ to 1)))))

		  (setq jsh-message (concat jsh-message
					    (substring output from)))
		  (setq output (substring output 0 from)))))

	  (if (and (setq from (string-match "\033" output))
		   (not (string-match "\n" output)))
	      (progn
		(setq jsh-message (concat jsh-message output))
		(setq output "")))
	    
	  (jsh-output output)
	  ;; Check to see whether the buffer contents now end with a
	  ;; string matching the password pattern (bound the search
	  ;; with the beginning of the line, to keep things from
	  ;; getting too slow); if they do, then enter password minor-
	  ;; mode.
	  ;; tcsh test: echo -n pass ; sleep 2 ; echo -n word: ; echo `line`

	  (setq pass-search-bound (save-excursion (beginning-of-line) (point)))
	  (if (and
	       (save-excursion
		 (re-search-backward jsh-password-pattern pass-search-bound t))
	       (= (match-end 0) (point)))
	      (jsh-password)))

      ;; Set the buffer and search folding back to what they were when
      ;; we entered the filter function.
      (set-buffer current)
      (setq case-fold-search current-case-fold))
    
    (if jsh-eval-string
	(unwind-protect
	    (let ((read-at 0)
		  (read-limit (length jsh-eval-string)))
	      (while (< read-at read-limit)
		(setq read-cons (read-from-string jsh-eval-string read-at))
		(eval (car read-cons))
		(setq read-at (cdr read-cons))))
	  (setq jsh-eval-string nil)))))

(defun jsh-handle-message (message)
  (let (jsh-eval-string)
    (if (string-match jsh-eval-message-pattern message)
	(let* ((eval-expr (substring message (match-beginning 2)
				     (match-end 2))))
	  (setq jsh-eval-string (concat jsh-eval-string eval-expr))))

    (if (string-match jsh-rlogin-message-pattern message)
	(let* ((new-host (substring message (match-beginning 2)
				    (match-end 2))))
	  (setq jsh-host new-host)
	  (jsh-set-default-dir jsh-host jsh-dir)
	  (update-jsh-dir default-directory)))

    (if (string-match jsh-chdir-message-pattern message)
	(let* ((newdir (substring message (match-beginning 2)
				  (match-end 2))))
		    
	  (setq jsh-dir newdir)
	  (jsh-set-default-dir jsh-host jsh-dir)
	  (update-jsh-dir default-directory)))

    (if (string-match jsh-setenv-message-pattern message)
	(let* ((env (substring message (match-beginning 2)
			       (match-end 2))))

	  ;; What a hack--because of some bogosity in csh,
	  ;; it's best to allow environment variables to be
	  ;; set with "NAME value" as well as "NAME=value"
	  (if (or (string-match "\\([^=]+\\)=\\(.*\\)" env)
		  (string-match "\\([^ ]+\\) \\(.*\\)" env))
	      (let* ((variable (substring env
					  (match-beginning 1)
					  (match-end 1)))
		     (value (substring env
				       (match-beginning 2)
				       (match-end 2))))
		(jsh-setenv variable value)))))
    jsh-eval-string))

(defun jsh-set-default-dir (host dir)
  (if (string= host (system-name))
      (setq default-directory (expand-file-name dir))
    (setq default-directory (concat "/" host ":" dir)))

  (if (string= (substring default-directory -1 nil) "/")
      nil
    (setq default-directory (concat default-directory "/")))

  (if (and (featurep 'dir-hist)
	   (dirhist-p 'jsh-dirhist))
      (dh-add-dir 'jsh-dirhist default-directory)))
	

(defun update-jsh-dir (dir)

  (setq targlen jsh-modeline-dir-len)

  (if jsh-home
      (if (string-match jsh-home dir)
	  (if (= (match-beginning 0) 0)
	      (setq dir (concat "~" (substring dir (match-end 0)))))))

  (setq jsh-default-dir (jsh-shorten-dir dir))
  (setq jsh-default-dir
	(concat jsh-default-dir
		(make-string (- targlen (length jsh-default-dir)) ? )))

  (save-excursion (set-buffer (other-buffer)))) ;force mode-line update

(defun jsh-shorten-dir (dir)
  (if (< (length dir) targlen)		;If the string doesn't need
      dir				;shortening, don't try

    ;; Determine how many characters to keep from the beginning
    (if jsh-chop-keep-beginning
	(if (string= "~/" (substring dir 0 2))
	    (setq start 2)
	  (if (string= "/" (substring dir 0 1))
	      (setq start 1)
	    (setq start 0)))
      (setq start 0))
    
    ;; Determine the point at which to start searching for a "/"
    (setq hack (- (length dir) (- targlen 3 start)))

    ;; Look for a "/" at which to chop the string; if there isn't one,
    ;; return as many characters as possible
    (if (and jsh-chop-at-slash (string-match "/" dir hack))
	(if (< (setq chop-from (match-beginning 0)) (- (length dir) 1))
	    nil
	  (setq chop-from hack))
      (setq chop-from hack))
    (concat (substring dir 0 start) "..." (substring dir chop-from))))

(defun jsh-hist-push (command)
  ;; If the command to push is the same as the command that's already
  ;; on the top of the stack, don't push it again.
  (if (string= command (aref jsh-history (jsh-hist-inc jsh-hist-tos)))
      nil				;Don't push

    ;; Push the command onto the stack, and adjust the top- and
    ;; bottom-of-stack values.
    (aset jsh-history jsh-hist-tos command)
    (setq jsh-hist-tos (jsh-hist-dec jsh-hist-tos))
    (aset jsh-history jsh-hist-tos "")
    (if (= (jsh-hist-inc jsh-hist-tos) jsh-hist-bos)
	(setq jsh-hist-bos jsh-hist-tos)))

  ;; Strictly speaking, this doesn't belong here, but we always do it
  ;; after a push, so for now it stays
  (setq jsh-hist-sp jsh-hist-tos)
  (setq jsh-hist-at-tos t)
  (setq jsh-hist-at-bos (= (jsh-hist-inc jsh-hist-sp)
			   jsh-hist-bos))

  ;; Return top-of-stack
  jsh-hist-tos)

(defun jsh-hist-dec (arg)
  (if (= 0 arg)
      (- jsh-history-stack-size 1)
    (- arg 1)))

(defun jsh-hist-inc (arg)
  (setq arg (+ 1 arg))
  (if (= arg jsh-history-stack-size)
      0
    arg))

(defun hist-find-prev (s)
  (setq sp (jsh-hist-inc jsh-hist-sp))
  (setq targ (concat "^" (regexp-quote s)))
  (setq hist-current (aref jsh-history jsh-hist-sp))

  (catch 'found				;Catch 'found as a means of
					;breaking out of the loop

    ;; search down through the stack looking for a history that
    ;; matches the target regex *and* is different from the current
    ;; history
    (while (not (= sp jsh-hist-bos))
      (if (string-match targ		;Matches regex?
			(setq hist-at-sp (aref jsh-history sp)))

	  (if (string= hist-current hist-at-sp)	;Yes, different?
	      nil			;No, keep going...

	    ;;We've found a history that meets both criteria--break
	    ;;the loop by throwing 'found.  The value that we throw
	    ;;will end up as this function's return value.
	    (throw 'found sp)))

      (setq sp (jsh-hist-inc sp)))	;increment the stack-pointer
					;(bump down in the stack).

    ;; If we reach this point, then the loop has terminated without
    ;; finding a history that meets our criteria--leave nil as our
    ;; return value
    nil))
    

(defun hist-find-next (s)
  ;; This function is just like hist-find-prev except that it searches
  ;; in the opposite direction.
  (setq sp (jsh-hist-dec jsh-hist-sp))
  (setq targ (concat "^" (regexp-quote s)))
  (setq hist-current (aref jsh-history jsh-hist-sp))

  (catch 'found
    (while (not (= sp jsh-hist-tos))
      (if (string-match targ
			(setq hist-at-sp (aref jsh-history sp)))
	  (if (string= hist-current hist-at-sp)
	      nil
	    (throw 'found sp)))
      (setq sp (jsh-hist-dec sp)))
    nil))

(defun jsh-hist-prev ()
  "Get and insert prev shell command"
  (interactive)

  (if jsh-hist-at-bos
      (error "No more history for this jsh.")

    (setq jsh-hist-prev-wrap nil)
    (save-excursion
      (goto-char (point-max))
      (setq key (buffer-substring (marker-position jshmark) (point)))
      (if jsh-hist-at-tos
	  (aset jsh-history jsh-hist-tos key)
	(setq last-key  (aref jsh-history jsh-hist-sp))
	(if (string= last-key key)
	    nil
	  (setq jsh-hist-sp jsh-hist-tos)
	  (setq jsh-hist-at-tos t)
	  (aset jsh-history jsh-hist-tos key)))
      
      (setq tos (aref jsh-history jsh-hist-tos))
      (if (string= tos "")
	  (setq jsh-hist-sp (jsh-hist-inc jsh-hist-sp))
	(if (setq sp (hist-find-prev tos))
	    (setq jsh-hist-sp sp)
	  (error "No more history matching \"%s\"." tos)))
      
      (delete-region (marker-position jshmark) (point))
      (insert (aref jsh-history jsh-hist-sp))
      (setq jsh-hist-at-bos (= (jsh-hist-inc jsh-hist-sp) jsh-hist-bos)))
    
    (goto-char (point-max))
    (setq jsh-hist-at-tos (= jsh-hist-sp jsh-hist-tos))))

(defun jsh-hist-next ()
  "Get and insert next shell command"
  (interactive)

  (if jsh-hist-at-tos
      (error "No more recent history for this jsh.")

    (save-excursion
      (goto-char (point-max))

      (setq tos (aref jsh-history jsh-hist-tos))
      (if (string= tos "")
	  (setq jsh-hist-sp (jsh-hist-dec jsh-hist-sp))
	(if (setq sp (hist-find-next tos))
	    (setq jsh-hist-sp sp)
	  (message "No more recent history matching \"%s\"." tos)
	  (setq jsh-hist-sp jsh-hist-tos)))

      (delete-region (marker-position jshmark) (point))
      (insert (aref jsh-history jsh-hist-sp))
      (setq jsh-hist-at-tos (= jsh-hist-sp jsh-hist-tos)))
    (goto-char (point-max))
    (setq jsh-hist-at-bos nil)))

(defun jsh-send-input ()
  "Send a line of input to the shell."
  (interactive)
  
  (if (and others-at-completion (other-windows))
      (if (eq others-at-completion 'no)
	  (delete-windows-on (get-buffer " *Completions*"))
	(unwind-protect			;Use this form to force a
					;return to the j-shell buffer.

	    (condition-case nil		;Use this form to catch an
					;error if the buffer we're
					;going to restore hase been
					;deleted since we put up the
					;*completions* buffer.
		(progn
		  (other-window 1)	;Go to the other window.

					;Put its buffer back to what
					;it was.
		  (switch-to-buffer others-at-completion))
	      (error nil))		;Stupid-handle the error

	  (other-window -1))))		;Return to the j-shell buffer
					;even if an error occured.
  (setq others-at-completion nil)
  
  (or (get-buffer-process (current-buffer))
      (error "Current buffer has no process"))

  (if jsh-parsing-password
      (progn
	(goto-char (point-max))
	(move-marker jshmark (point))
	(process-send-string jshproc jsh-password)
	(process-send-string jshproc "\n")
	(jsh-unpassword))

    ;; We're not parsing a password; what we do next depends on
    ;; whether the point is in the input-editing area or above it.
    (if (>= (point) (marker-position jshmark))
	(progn
	  (goto-char (point-max))
	  (setq preceding (char-after (- (point) 1)))
	  (insert ?\n)

	  ;;If the character before the \n is a backslash \, don't
	  ;;actually send anything to the shell, and don't move the
	  ;;marker. 
	  (if (and preceding (= ?\\ preceding))
	      nil
	    (setq from (marker-position jshmark))
	    (move-marker jshmark (point))
	    (jsh-send-region jshproc from (point))))

      ;; Exclude the shell prompts, if any.
      (beginning-of-line)
      (while
	  (re-search-forward jsh-prompt-pattern
			     (save-excursion (end-of-line) (point))
			     t))

      (let ((copy (buffer-substring (point)
				    (progn (forward-line 1) (point)))))
	(goto-char (point-max))
	(delete-region (marker-position jshmark) (point))
	(insert copy)
	(setq from (marker-position jshmark))
	(move-marker jshmark (point))
	(jsh-send-region jshproc from (point))))))

(defun jsh-send-region (process from to)
  ;; Push the region onto the command history
  (save-excursion
    (progn
      (goto-char from)
      (if (re-search-forward "[^ \t\n]" to t)
	  (jsh-hist-push
	   (buffer-substring from (- to 1))))))	;Don't save the linefeed!

  ;; If there's an unescaped ! in the region, set a flag so the filter
  ;; function can replace the command history when it's regurgitated
  ;; by the shell.
  (save-excursion
    (goto-char from)
    (if (or (looking-at "!") (re-search-forward "[^\\]!" to t))
	(setq jsh-grab-history t)))

  ;; Rule 42: all persons more than a mile high to leave the court.
  ;; This loop does away with the ^G problem by sending user input to
  ;; the shell in chunks of 250 characters.  The ^G problem occurs
  ;; when shell modes send more than 256 characters to the shell, thus
  ;; overrunning the shell's input buffer; the shell complains by
  ;; sending ^G (the bell character).
  (while (> (- to from) 250)

    (process-send-region process from (+ from 250))
    (setq from (+ from 250))

    ;; Make sure the output buffer gets flushed;  what we really need
    ;; is to change its buffering--currently it's line-buffered; we
    ;; need to make it unbuffered (Emacs is actually the buffer).
    (if (= (char-after from) ?\n)
	nil
      (process-send-eof)));;Flushes IO buffered for process.  What a travesty!

  (process-send-region process from to)) ;Send whatever's left

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for parsing passwords

(defun jsh-password ()
  "Enter a password, without having it echo."
  (interactive)
  (jsh-hide-lossage)
  (setq jsh-parsing-password t)
  (make-local-variable 'jsh-password)
  (setq jsh-password ""))

(defun jsh-self-insert (arg)
  "Insert the character you type.
Whichever character you type to run this command is inserted."
  (interactive "P")
  (setq number (prefix-numeric-value arg))
  (if jsh-parsing-password
      (setq jsh-password
	    (concat jsh-password (make-string number last-input-char)))
    (self-insert-command number)))

(defun jsh-del-back (arg)
  "Delete the previous characters (following, with negative ARG)."
  (interactive "P")
  (if jsh-parsing-password
      (if (string= "" jsh-password)
	  (beep)
	(setq jsh-password
	      (substring jsh-password 0 -1)))
    (backward-delete-char (prefix-numeric-value arg))))

(defun jsh-unpassword ()
  "Stop entering password--make characters echo."
  (interactive)
  (setq jsh-parsing-password nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for making passwords a little bit more secure.

(defvar jsh-view-lossage
  '(lambda ()
     (interactive)
     (with-output-to-temp-buffer "*Help*"
       (princ jsh-secure-message)
       (print-help-return-message))))

(defvar jsh-recent-keys
  '(lambda ()
     (interactive)
     jsh-secure-message))

(defun jsh-hide-lossage ()
  (if jsh-secure-mode
      (let ((rk (intern-soft "recent-keys"))
	    (vl (intern-soft "view-lossage")))

	(if (and rk (not (eq (symbol-function rk)
			     jsh-recent-keys)))
	    (progn
	      (set rk jsh-secure-message)
	      (fset rk jsh-recent-keys)))

	(if (and vl (not (eq (symbol-function vl)
			     jsh-view-lossage)))
	    (progn
      	      (set vl jsh-secure-message)
	      (fset vl jsh-view-lossage)))

	;; Close the dribble file--wouldn't want to record a password
	(if (string-lessp emacs-version "18.58")
	    (if (string-match "unix" (symbol-name system-type))
		;; Emacs 18.57 and earlier versions don't like nil as a
		;; way to close the dribble file, so on unix systems, open
		;; /dev/null instead.
		(open-dribble-file "/dev/null")
	      (beep)
	      (message "*** Security risk: your password may be written into the dribble file"))
	  (open-dribble-file nil)))))	;nil means close

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some miscellaneous functions

(defun jsh-edit ()
  "Copy a line to the input area for editing."
  (interactive)
  (save-excursion
    (end-of-line)
    (setq jsh-end (point))
    (jsh-beginning-of-line)
    (copy-region-as-kill (point) jsh-end))
  (goto-char (point-max))
  (yank))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This section implements some commands for campatibility with
;;; Emacs's shell mode.

(defun jsh-kill-output ()
  "Kill the output of the previous shell command."
  (interactive)

  ;; Search backward for a shell prompt; if we find one that's
  ;; different from the one we might happen to be on, then there's
  ;; output to flush
  (setq top (jsh-prev-command (point-min) (point)))
  (if (< top (point))
      (progn
	
	;; Find the output associated with this prompt, and delete it
	(save-excursion
	  (goto-char top)
	  (end-of-line)
	  (forward-char 1)
	  (setq from (point))
	  (if (re-search-forward jsh-prompt-pattern (point-max) t)
	      (progn
		(goto-char (match-beginning 0))
		(if (= from (point))
		    nil
		  (kill-region from (point))
		  (insert "*** output flushed ***\n")))))

	;; If the prompt that we deleted from is not visible in the
	;; window, make it visible by moving the window start to the
	;; beginning of the line that contains the prompt 
	(save-excursion
	  (goto-char top)
	  (beginning-of-line)
	  (setq start (window-start (selected-window)))
	  (if (< (point) start)
	      (set-window-start (selected-window) (point)))))))

(defun jsh-show-output ()
  "Search backward for and show the previous shell prompt."
  (interactive)
  (jsh-move-prev)
  (setq show-at (save-excursion (beginning-of-line) (point)))
  (set-window-start (selected-window) show-at))
		
(defun jsh-kill-input ()
  "Kill pending shell input."
  (interactive)
  (jsh-beginning-of-command)
  (kill-region (point) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This section implements some commands for moving about with the shell

(defun jsh-move-prev ()
  "Search backwards through the buffer for the previous shell command."
  (interactive)
  (goto-char
   (jsh-prev-command (point-min) (point)))
  (end-of-line))

(defun jsh-move-next ()
  "Search forward through the buffer for the next shell command"
  (interactive)
  (goto-char
   (jsh-next-command (point) (point-max)))
  (end-of-line))

(defun jsh-prev-command (min max)
  (save-excursion
    (setq from (point))
    (goto-char max)
    (beginning-of-line)

    (setq done nil)
    (while (not done)
      (if (re-search-backward jsh-prompt-pattern min t)
	  (progn
	    (goto-char (match-end 0))
	    (if (looking-at "\n")
		(beginning-of-line)
	      (setq done t)))
	(setq done t)
	(end-of-line)))
    (if (looking-at "$")
	from
      (point))))

(defun jsh-next-command (min max)
  (save-excursion
    (goto-char min)
    (while (and (looking-at jsh-prompt-pattern) (< (point) max))
      (forward-char 1))
    (setq done nil)
    (while (not done)
      (if (re-search-forward jsh-prompt-pattern max t)
	  (progn
	    (goto-char (match-end 0))
	    (if (looking-at "\n")
		nil
	      (setq done t)))
	(setq done t)))
    (point)))

(defun jsh-beginning-of-line ()
  "Go to beginning of line, skipping over any prompt."
  (interactive)
  (setq limit (point))
  (beginning-of-line)
  (if (looking-at jsh-prompt-pattern)
      (goto-char (match-end 0)))
  (if (= limit (point))
      (beginning-of-line)))

(defun jsh-beginning-of-command ()
  "Go to beginning of command."
  (interactive)
  (goto-char (marker-position jshmark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This section is related to signals.

(defun jsh-send-eof ()
  "Send EOF (end-of-file) to the shell"
  (interactive)
  (if jsh-send-char-signals
      (process-send-string jshproc jsh-eof-char)
    (process-send-eof)))

(defun jsh-send-interrupt ()
  "Send an INT (interrupt) signal or character to the shell"
  (interactive)
  (if jsh-send-char-signals
      (process-send-string jshproc jsh-interrupt-char)
    (interrupt-process nil t)))

(defun jsh-send-stop ()
  "Send a STOP signal or character to the shell"
  (interactive)
  (if jsh-send-char-signals
      (process-send-string jshproc jsh-stop-char)
    (stop-process nil t)))

(defun jsh-send-quit ()
  "Send a QUIT signal or character to the shell"
  (interactive)
  (if jsh-send-char-signals
      (process-send-string jshproc jsh-quit-char)
    (quit-process nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This section implements filename completion.
;;;
;;; Filename completion is based almost entirely upon the package
;;; written by Shinichirou Sugou.  The changes that have been added
;;; allow j-shell to remove the completion buffer upon sending input
;;; to the shell.  The rationale for this modification is that having
;;; given jsh a command, the user no longer needs to see the
;;; completions.

;;; The logic for removing the completion buffer attempts to restore
;;; the windows to the way they were before displaying the completion
;;; buffer.  If the jsh window was the only window displayed when the
;;; completion window appeared, the the completion window is deleted;
;;; if some window was already displaying another buffer, the
;;; completions buffer is replaced with the original.

;;; A note: I have tried replacing my logic for restoring the other
;;; window with the more standard combination of current-window-
;;; configuration, set-window-configuration; it works, but I find that
;;; I prefer my version.  The problem with set-window-configuration is
;;; that it restores the window-start position of the J-Shell buffer,
;;; and this is sometimes annoying.

(defvar others-at-completion nil)
(defvar no nil)
(defvar yes nil)

(defun other-windows ()
  (setq this-window (get-buffer-window (current-buffer)))
  (not (eq (next-window this-window) this-window)))

;; File-completion-in-shell-mode by Shinichirou Sugou 90/6/8
;;        shin%sgtp.apple.juice.or.jp@uunet.uu.net

(defun jsh-complete ()
  "Perform completion on the preceding string.

The string may be a filename, environment variable, or (depending on
context) a command."

  (interactive)

  ;; The following if-construct and its contents have been added for
  ;; j-shell.
  (if others-at-completion		;If others-at-completion
      nil				;already has a value, do
					;nothing.

    ;; If there are other windows, set others-at-completion to the
    ;; buffer in the other window, the one that will be supplanted by
    ;; the completions buffer.  If there are no other windows, set
    ;; others-at-completion to 'no.
    (if (other-windows)			
	(progn			       
	  (other-window 1)
	  (setq others-at-completion (current-buffer))
	  (other-window -1))
      (setq others-at-completion 'no)))

  (let* ((beg  (save-excursion
                 (re-search-backward jsh-completion-separator)
                 (1+ (point))))
         (end (point))
	 (jsh-at-command (jsh-at-command (marker-position jshmark) beg))
         (file (file-name-nondirectory (buffer-substring beg end)))
         (dir (or (file-name-directory (buffer-substring beg end)) ""))
         (akin (jsh-all-completions file dir))
	 (lpc (try-completion file (jsh-make-alist-from-list 
				    (append akin nil)))))
    (cond ((eq lpc t)
           (message "[Sole completion]")
	   (if (and jsh-expand-environment
		    (= (char-after (setq from (- beg 1))) ?$)
		    (setq expansion (jsh-getenv file)))
	       (progn
		 (message "[Sole completion--expanded]")
		 (delete-region (- beg 1) end)
		 (insert expansion))
	     (sit-for 2)))
          ((eq lpc nil)
           (ding t)
           (message "[No match]")
           (sit-for 2))
          ((and (string= lpc file) (my-member lpc akin))
           (message "[Complete, but not unique]")
           (sit-for 2))
          ((string= lpc file)
           (jsh-completion-help akin))
          (t
           (delete-region beg end)
           (insert dir lpc)))))

;; Expand-file-name
(defun jsh-expand ()
  "Expand the relative filename or path preceding point.
The absolute filename or path is substituted."
  (interactive)
  (let* ((beg  (save-excursion
                 (re-search-backward jsh-completion-separator)
                 (1+ (point))))
         (end (point))
	 (filename (buffer-substring beg end)))
    (setq filename (expand-file-name (substitute-in-file-name filename)))
    (if (file-exists-p filename)
	(progn
	  (delete-region beg end)
	  (insert filename)))))

(defun my-member (item list &optional testf)
  "Compare using TESTF predicate, or use 'eql' if TESTF is nil."
  (setq testf (or testf 'eql))
  (catch 'bye
    (while (not (null list))
      (if (funcall testf item (car list))
          (throw 'bye list))
      (setq list (cdr list)))
    nil))

(defun jsh-show (buf)
  (if (other-windows)
      nil
    (split-window-vertically))
  (other-window 1)
  (switch-to-buffer buf t)
  (other-window -1))


(defun jsh-completion-help (&optional akin)
  "Display all completions of the string preceding point."

  (interactive)
  (make-local-variable 'temp-buffer-show-hook)
  (setq temp-buffer-show-hook 'jsh-show)
  (if (null akin)
      (let* ((beg  (save-excursion
                     (re-search-backward "\\s ")
                     (1+ (point))))
             (end (point))
             (file (file-name-nondirectory (buffer-substring beg end)))
             (dir (or (file-name-directory (buffer-substring beg end)) "")))
        (message "Making completion list...")
        (setq akin (file-name-all-completions file dir))))
  (if akin
      (with-output-to-temp-buffer " *Completions*"
        (display-completion-list (sort akin 'string-lessp)))
    (ding t)
    (message "[No completion]")))

(defun jsh-make-alist-from-list (list)
  (let ((list list))
    (while list
      (setcar list (cons (car list) nil))
      (setq list (cdr list))))
  list)

(defvar jsh-environment nil
  "Emacs's environment, represented as an association list.")

(defun jsh-make-environment ()
  (if (boundp 'process-environment)
      (let ((env process-environment)
	    jsh-env)
	(while env
	  (setq var (car env))
	  (setq env (cdr env))
	  (if (string-match "\\([^=]+\\)=\\(.*\\)" var)
	      (setq jsh-env
		    (cons (cons (substring var
					   (match-beginning 1)
					   (match-end 1))
				(substring var
					   (match-beginning 2)
					   (match-end 2)))
			  jsh-env))))
	(nreverse jsh-env))
    (getenv t)))

(defvar jsh-exec-path nil
  "A copy of exec-path, changed when the shell's PATH changes.")

(defun jsh-make-exec-path (path)
  (let ((start 0)
	(limit (length path))
	(exec-path nil))
    (while (and (< start limit)
		(string-match "[^:]+" path start))
      (if (setq dir (substring path (match-beginning 0) (match-end 0)))
	  (setq exec-path (cons dir exec-path)))
      (setq start (+ 1 (match-end 0))))
    (nreverse exec-path)))

(defun jsh-setenv (variable value)
  (if (setq env (assoc variable jsh-environment))
      (setcdr env value)
    (setq jsh-environment (cons (cons variable value) jsh-environment)))

  (if (string= "PATH" variable)
      (setq jsh-exec-path (jsh-make-exec-path value))))

(defun jsh-getenv (variable)
  (if (setq env (assoc variable jsh-environment))
      (if (setq value (cdr env))
	  value
	"")))

(if (not (fboundp 'file-executable-p))
    (defun file-executable-p (filename)
      (not (= 0 (logand (file-modes filename) 73)))))

(defun jsh-at-command (from to)
  (if (> from to)
      nil
    (if (= from to)
	t
      (save-excursion
	(save-restriction
	  (narrow-to-region from to)
	  (goto-char to)
	  ;; Look backward for the first non-whitespace character
	  ;; preceding the completion point.
	  (if (re-search-backward "[^ \t][ \t]*" nil t)
	      (progn
		(goto-char (match-beginning 0))
		(looking-at jsh-command-preceders))
	    t))))))

(defun jsh-all-command-completions (file)
  (let (commands
	(path jsh-exec-path))
    (while path
      (let ((dir (file-name-as-directory (car path))))
	(if (file-exists-p dir)
	    (if (string= dir ".")
		nil
	      (setq files (file-name-all-completions file dir))
	      (while files
		(setq filename (concat dir (car files)))
		(if (file-executable-p filename)
		    (setq commands (cons (car files) commands)))
		(setq files (cdr files))))))
      (setq path (cdr path)))
    commands))

(defun jsh-all-completions (file dir)

  (let (completions)

    (if (and jsh-complete-commands
	     (string= dir "")
	     jsh-at-command)
	(if (and (boundp 'jsh-complete-command-threshold)
		 jsh-complete-command-threshold
		 (<= (length file) jsh-complete-command-threshold))
	    nil
	  (setq completions (append (jsh-all-command-completions file)
				    completions))
	  (if (and (boundp 'jsh-builtin-commands)
		   jsh-builtin-commands)
	      (setq completions (append (all-completions
					 file jsh-builtin-commands)
					completions)))))

    (if (and jsh-complete-environment
	     (string= dir ""))
	(if (and (and (boundp 'jsh-complete-env-threshold)
		      jsh-complete-env-threshold
		      (<= (length file) jsh-complete-env-threshold)))
	    nil 
	  (setq completions (append (all-completions file jsh-environment)
				    completions))))

    (setq completions (append (file-name-all-completions file dir) completions))

    completions))

(defun jsh-try-completion (file dir)

  (try-completion
   file (jsh-make-alist-from-list (jsh-all-completions file dir))))

(defun jsh-submit-bug-report ()
  "Submit via mail a bug report on J-Shell."
  (interactive)
  (require 'reporter)
  (let ((curbuf (current-buffer)))
    (reporter-submit-bug-report
     jsh-help-address
     (concat "j-shell.el " jsh-version " running "
	     jsh-program-name "\nin " (pwd) "\non system " (system-name))
     (append
      (list
       'default-directory
       'case-fold-search)
      (list-vars-matching "^jsh-"))
     nil
     (function
      (lambda ()
	(insert
	 (save-excursion
	   (set-buffer curbuf)
	   (goto-char (point-max))
	   (forward-line -100)
	   (concat "\nLast "
		   (count-lines (point) (point-max))
		   " lines of buffer\n"
		   "=========================\n"
		   (buffer-substring (point) (point-max))
		   "\n=========================\n"))))))))
  
(defun list-vars-matching (regex)
  (setq syms nil)
  (mapatoms
   (function
    (lambda (sym)
      (if (boundp sym)
	  (let ((name (symbol-name sym)))
	    (if (string-match regex name)
		(setq syms (cons sym syms))))))))
  syms)

(defun jsh-display-dirhist ()
  (interactive)
  (if (and (featurep 'dir-hist)
	   (dirhist-p 'jsh-dirhist))
      (progn
	(dh-own-history 'jsh-dirhist (current-buffer))
	(dh-display 'jsh-dirhist)
	(set-buffer (get 'jsh-dirhist 'dh-buffer)))))

(provide 'j-shell)
