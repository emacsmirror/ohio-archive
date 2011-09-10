;;; empclient.el --- Simple, extensible, self contained empire client

;; Copyright (C) 1994 Markus Armbruster

;; Author: Markus Armbruster <armbru@pond.sub.org>
;; Version: $Id: empclient.el,v 1.5 1994/07/25 09:30:32 armbru Exp $
;; Keywords: games

;; Empclient is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Empclient is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Empclient; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file is a simple, extensible, self contained empire-client-in-
;; a-buffer package (empire client mode).  Simple means there is
;; essentially everything the stock emp_client provides plus what we
;; get for free from Emacs, but nothing more.  Extensible means there
;; are hooks to plug in other tools.  Self contained means you don't
;; need anything but this file and a *recent* Emacs.  GNU Emacs 19.25
;; works fine, alas, 19.22 doesn't.  I don't care for version 18
;; anymore.

;; Since this mode is built on top of Emacs's general command-
;; interpreter-in-a-buffer mode (comint mode), it shares a common
;; base functionality, and a common set of bindings, with all modes
;; derived from comint mode.  This makes these modes easier to use.

;; For documentation on the functionality provided by comint mode, and
;; the hooks available for customising it, see the file comint.el that
;; comes with Emacs.


;;; Summary of Commands & Bindings

;; empire-client		Connect to an empire-server
;; empire-client-mode		Major mode
;; empire-pop-to-tool	TAB	Pop to current tool

;; Replacements for comint commands, bindings as in comint:
;; empire-delchar-or-maybe-eof	Replaces comint-delchar-or-maybe-eof
;; empire-send-eof		Replaces comint-send-eof
;; empire-send-intr		Replaces comint-interrupt-subjob


;;; Summary of User Options

;; empire-host			Name of the server host
;; empire-port			Port to use on empire-host
;; empire-country		Your empire country
;; empire-passwd		Your empire password
;; empire-prompt-format		To customize `[#:#] Command: ' prompts
;; empire-keep-redir-buffers	Keep or kill buffers created for redirections
;; empire-client-mode-hook	Major mode hook


;;; Plugging in Tools

;; Empclient filters each line of input through the functions in
;; `empire-input-filter-functions.  They may perform arbitrary
;; substitutions.  The function `empire-filter-input' may be used for
;; recursive substitutions.  Then, Empclient calls the function in
;; `empire-input-split-function' to split the input line into a list
;; of commands.  Each command is sent to the server on a separate
;; line.  Filtering and splitting is inhibited while the variable
;; `empire-verbatim-input' is non-nil.

;; Tools may insert commands as if they were typed in by the user.
;; See functions `empire-insert-input' and `empire-send-command'.

;; Empclient calls the function in `empire-output-filter' for each
;; line of output.  This function may snoop data from the empire
;; dialog.  Empclient runs `empire-after-output-hook' after each chunk
;; of server output.

;; A set of tools including a map display is currently under
;; development.


;;; Installation

;; You need this file somewhere in your load-path.
;; You may byte compile it for speed
;;   M-x byte-compile whereever/empclient.el
;; To just give it a try, use
;;   M-x load-library empclient
;;   M-x empire-client
;; To install it, add the following line to ~/.emacs
;;   (autoload 'empire-client "empclient" "Connect to an empire server, with I/O through buffer *empire*." t)


;;; How to get

;; anonymous ftp
;;   i44ftp.ira.uka.de:pub/misc/empire/emacs/empclient.el
;; or from the Elisp Archive,
;;   archive.cis.ohio-state.edu:pub/gnu/emacs/elisp-archive/games/empclient.el.Z
;; or one of its mirrors.
;; or from the Empire archive,
;;   ftp.cis.ksu.edu:/pub/Games/Empire/...

;; LCD Archive Entry:
;; empclient|Markus Armbruster|armbru@pond.sub.org|
;; Simple, extensible, self contained empire client.|
;; 25-Jul-1994|1.5|~/games/empclient.el.Z|


;;; Bugs

;; The empire command `wait' confuses Empclient.  If this happens, use
;; `empire-send-intr' with an argument to reset Empclient.


;;; Restrictions

;; Currently, you can't have multiple empires per Emacs.


;;; To do

;; Do not busy wait in empire-send
;; Clean up comint's menus.
;; Allow multiple connections (not that I needed them :)


;;; Code:

(require 'comint)
(provide 'empclient)

;;; User options

(defvar empire-host (getenv "EMPIREHOST")
  "*Name of the server host, or its IP address.")

(defvar empire-port (getenv "EMPIREPORT")
  "*Port to use on empire-host.")

(defvar empire-country (getenv "COUNTRY")
  "*Your empire country")

(defvar empire-passwd (getenv "PLAYER")
  "*Your empire password, the so called representative.")

(defvar empire-prompt-format "[%s:%s] Command: "
  "*The format string for empire prompts.
The first %s will print as your current number of BTUs, the second as
your login time.")

(defvar empire-client-prompt "Client> "
  "*The prompt string for client generated prompts.")

(defvar empire-keep-redir-buffers nil
  "*Non-nil means buffers created for redirections should be kept.
Nil means silently kill buffers created for redirections.
Already existing buffers are never killed.")

(defvar empire-client-mode-hook nil
  "*Hook for customising Empire Client mode.")


;;; Variables

;; Empire client mode keymap

(defvar empire-client-mode-map nil
  "Keymap used in Empire Client mode.")
(cond ((not empire-client-mode-map)
       (setq empire-client-mode-map (copy-keymap comint-mode-map))
       (define-key empire-client-mode-map "\t" 'empire-pop-to-tool)
       (define-key empire-client-mode-map "\C-c\C-v" 'empire-toggle-verbatim)
       (substitute-key-definition 'comint-send-eof
				  'empire-send-eof
				  empire-client-mode-map)
       (substitute-key-definition 'comint-delchar-or-maybe-eof
				  'empire-delchar-or-maybe-eof
				  empire-client-mode-map)
       (substitute-key-definition 'comint-interrupt-subjob
				  'empire-send-intr
				  empire-client-mode-map)))


;; Variables provided for external tools to interface with empire client

(defconst empclient-version "1.4"
  "Empclient's version number.
This is really the oldest version that looks like this one for tools.
When interface changes, it gets bumped up to the `real' version number.")

(defvar empire-input-filter-functions nil
  "A list of functions to filter lines of user input.
For each line the functions are called in list order.  The line is
passed to the first one, subsequent ones are passed their
predecessor's result, and the last result is sent to the server.  If a
function returns nil, filtering is terminated and the line of input is
removed.")

(defvar empire-input-split-function 'list
  "Function to split the filtered input into a list of commands.
It is called with one argument, a string or nil.
It should return a list of strings and optionally the following symbols:
`at-prompt-or-abort' requests to force the session to command level,
`at-flush-or-ignore' requests to ignore the immedialtely following
string if the session is at command level.")

(defvar empire-verbatim-input nil
  "If non-nil, send input verbatim.
Else it is filtered through `empire-input-filter-functions' and split
with `empire-input-split-function'.")

;; To avoid interference with user commands, tools should not inject
;; commands while `empire-busy' is non-nil.
(defvar empire-busy nil
  "Non-nil if session is at command-level and there are no unprocessed commands.")

(defvar empire-output-filter (function (lambda (tag string) string))
  "Function to filter lines of server output.
Called inside of an excursion with two arguments.  First argument is
one of the symbols `exit', `flush', `prompt', `abort', `cmderr',
`badcmd', `data'.  Second argument is the line of output.  Returns the filtered string.")

;; The function in `empire-output-filter' might want to know this:
(defvar empire-current-input nil
  "Empclient currently receives this input line's output.")

;; This hook is intented for output triggered actions that move point.
;; They can't be called from `empire-output-filter', as it runs inside
;; of an excursion.
(defvar empire-after-output-hook nil
  "Hook run after each chunk of server output.")

(defvar empire-tool-buffer nil
  "If non-nil, \\[empire-pop-to-tool] pops to this buffer.")


;; Internal variables

(defvar empire-process-mark nil
  "If non-nil, overrides process mark.
Comint abuses the process mark as mark for the start of the current
input.  Sometimes the real process mark needs to be somewhere else.")

(defvar empire-sent-ahead nil
  "Number of lines sent to server but not acknowledged by a prompt.")

(defconst empire-server-tag-alist
  '((cmdok   . ?0)
    (data    . ?1)
    (init    . ?2)
    (exit    . ?3)
    (flush   . ?4)
    (noecho  . ?5)
    (prompt  . ?6)
    (abort   . ?7)
    (redir   . ?8)
    (pipe    . ?9)
    (cmderr  . ?A)
    (badcmd  . ?B)
    (execute . ?C))
  "Alist associating symbols with server message tags.")

(defvar empire-consumer nil
  "Function that `consumes' a line of server output.
Called with one argument, the line of output.")

(defvar empire-partial-line nil
  "Trailing partial line of server output.")

(defvar empire-login-proto nil
  "Description of the remaining login protocol.
A list of lists `(EXPECTED-TAG ERROR NEXT-COMMAND)' where EXPECTED-TAG
 is a symbol, ERROR a function or nil, NEXT-COMMAND a string or nil.
If we get EXPECTED-TAG, the NEXT-COMMAND is sent to the server if
 non-nil and the first element is deleted from empire-login-proto.  If
 this exhausts the list, login was successful.
On receiving an unexpected tag, ERROR is called if non-nil with three
 arguments, the expected tag, the received tag and the received
 string.  The connection is then closed.")

;; Good choice: ?\240 is ?\   with msb set --- gets displayed as
;; highlighted blank if we can't make it invisible.
(defvar empire-magic-prompt-cookie "\240"
  "String to append to strange prompts.
We need to match prompts with `comint-prompt-regexp'.  If we can't,
we append this magic cookie to make it recognizable.")

(defvar empire-redir-buffer nil
  "Buffer that holds the current redirection.
If this is not a buffer, ignore output.")

(defvar empire-pipe-process nil
  "The process we are currently piping to.")


;;; Commands

(defun empire-client-mode ()
  "Major mode for interacting with an empire server.
Return after the end of the process's output sends the text from the 
 end of process to the end of the current line.
Return before end of process output copies the current line (except
 for the prompt) to the end of the buffer and sends it.

\\{empire-client-mode-map}

Entry to this mode runs the hooks `comint-mode-hook' and
`empire-client-mode-hook' (in that order)."
  (interactive)
  (comint-mode)
  (setq comint-input-filter 'empire-save-on-history-p)
  (setq comint-input-sender 'empire-send)
  (setq comint-process-echoes t)	; not really, but we fake it
  (setq comint-prompt-regexp
	(concat "^"
		(format (regexp-quote empire-prompt-format) "[0-9]*" "[0-9]*")
		"\\|^"
		empire-client-prompt
		"\\|^.*"
		empire-magic-prompt-cookie))
  (setq major-mode 'empire-client-mode)
  (setq mode-name "Empire-Client")
  ;; hack mode line to display `Verb' while empire-verbatim-input is non-nil
  (setq mode-line-format (copy-sequence mode-line-format))
  (let ((tail (memq 'mode-line-process mode-line-format)))
    (setcdr tail (cons '(empire-verbatim-input " Verb") (cdr tail))))
  (use-local-map empire-client-mode-map)
  ;; arrange display table to interpret characters' msb as highlight
  (setq buffer-display-table (make-display-table))
  (let ((highlight-face-id (if (fboundp 'face-id) (face-id 'highlight) 0))
	(char 128))
    (while (< char 256)
      (aset buffer-display-table char
	    (vector (+ char -128 (* 256 highlight-face-id))))
      (setq char (1+ char))))
  (run-hooks 'empire-client-mode-hook))

(defun empire-client (&optional kill)
  "Connect to an empire server, with I/O through buffer *empire*.
If there is already a connection, just switch to its buffer.
With prefix arg, try to kill an existing connection instead of
starting a new one."
  (interactive "P")
  (let* ((buffer (get-buffer-create "*empire*"))
	 (process (get-buffer-process buffer)))
    (save-excursion
      (set-buffer buffer)
      (if (and process (eq (process-status process) 'open))
	  nil				; reuse old connection
	(empire-client-mode)
	;; outdated emacs versions may choke on this, but 19.25 works:
	(add-text-properties 0 (length empire-magic-prompt-cookie)
			     '(invisible t rear-nonsticky t)
			     empire-magic-prompt-cookie)
	;; open connection
	(or empire-host (setq empire-host (read-string "Empire host? ")))
	(or empire-port (setq empire-port (read-string "Empire port? ")))
	(and (stringp empire-port)
	     (string-match "\\`[0-9]+\\'" empire-port)
	     (setq empire-port (string-to-number empire-port)))
	(or empire-country (setq empire-country (read-string "Country? ")))
	(or empire-passwd  (setq empire-passwd (read-string "Player? ")))
	(if process (delete-process process))
	(setq process
	      (open-network-stream "empire" buffer empire-host empire-port))
	(goto-char (point-max))
	(set-marker (process-mark process) (point))
	(setq empire-process-mark (make-marker))
	(set-process-filter process 'empire-process-filter)
	(setq empire-consumer 'empire-login)
	(setq empire-partial-line "")
	(setq empire-login-proto
	      (list (list 'init nil (concat "user " (user-login-name)))
		    (list 'cmdok nil (concat "coun " empire-country))
		    (list 'cmdok
			  (function (lambda (exp rec args)
				      (setq empire-country nil)))
			  (concat "pass " empire-passwd))
		    (list 'cmdok
			  (function (lambda (exp rec args)
				      (setq empire-country nil
					    empire-passwd nil)))
			  (if kill "kill" "play"))))
	(setq empire-sent-ahead 1)	; expecting one initial prompt
	(setq empire-busy t)
	(setq empire-redir-buffer nil)
	(setq empire-pipe-process nil))
      (switch-to-buffer buffer)
      buffer)))

(defun empire-delchar-or-maybe-eof (arg)
  "Delete ARG characters forward, or (if at eob) send an end-of-file cookie."
  (interactive "p")
  (if (eobp)
      (empire-send-eof)
    (delete-char arg)))

(defun empire-send-eof ()
  "Send end-of-file cookie to empire server."
  (interactive)
  (let ((empire-verbatim-input t))
    (empire-send-command "ctld")))

(defun empire-send-intr (&optional arg)
  "Send interrupt cookie to empire server.
With argument, reset Empclient to sane state."
  (interactive "P")
  (cond (arg
	 (setq empire-sent-ahead 0
	       empire-partial-line "")
	 (empire-close-pipe)
	 (empire-unredirect)
	 (set-marker empire-process-mark nil)))
  (let ((empire-verbatim-input t))
    (empire-send-command "aborted")))

(defun empire-pop-to-tool ()
  "Select `empire-tool-buffer' in some window, preferably a different one."
  (interactive)
  (if empire-tool-buffer
      (pop-to-buffer empire-tool-buffer)
    (message "No current tool")))

(defun empire-toggle-verbatim (&optional arg)
  "Change whether input is sent verbatim or filtered and split.
With arg, set verbatim iff arg is positive."
  (interactive "P")
  (setq empire-verbatim-input
	(if (not arg)
	    (not empire-verbatim-input)
	  (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))


;;; Sending input

(defun empire-save-on-history-p (string)
  "Return non-nil if STRING should be saved on the input history list.
Don't save all whitespace, end-of-file or interrupt cookies.
This usually serves as `comint-input-filter'."
  (not (string-match "\\`\\( *\\|ctld\\|aborted\\)\\'" string)))

(defun empire-send (process string)
  "Sent to PROCESS input STRING and a newline.
Input is filtered through `empire-input-filter-functions' and split
with `empire-input-split-function' unless `empire-verbatim-input' is
non-nil."
  (let ((pmark (process-mark process))
	cmds)
    (set-marker pmark (point))		; comint should do that
    (insert string)			; fake echo
    (insert "\n")
    (let ((opoint (point)))
      (setq cmds
	    (if empire-verbatim-input
		(list string)
	      (condition-case err
		  (funcall empire-input-split-function
			   (empire-filter-input string
						empire-input-filter-functions))
		(error (message "Input filtering failed: %s" err)
		       nil))))
      (if (and (= opoint (point))
	       (not (cdr cmds))
	       (string= string (car cmds)))
	  ;; no need to echo both string and cmds, undo former
	  (delete-region pmark (point))))
    (or cmds
	;; nothing will be sent, will get no server prompt, fake one
	(insert empire-client-prompt))
    (while cmds
      (cond ((eq (car cmds) 'at-prompt-or-abort)
	     ;; TODO: no busy wait
	     (empire-wait-until-non-busy t)
	     (if empire-busy		; not at prompt, abort command
		 (empire-send-intr)))
	    ((eq (car cmds) 'at-flush-or-ignore)
	     ;; TODO: no busy wait
	     (empire-wait-until-non-busy t)
	     (or empire-busy		; not at flush, ignore next cmd
		 (setq cmds (cdr cmds))))
	    (t
	     (let ((cmd (concat (car cmds) "\n")))
	       (and (zerop empire-sent-ahead)
		    (setq empire-current-input cmd))
	       (save-excursion		; echo cmd
		 (insert cmd)
		 (set-marker pmark (point)))
	       (while (< (point) pmark)
		 (and (/= 0 empire-sent-ahead)
		      (not (marker-position empire-process-mark))
		      (set-marker empire-process-mark (point)))
		 (setq empire-sent-ahead (1+ empire-sent-ahead))
		 (setq empire-busy t)
		 (forward-line)
		 ;; mark end of server input line
		 (add-text-properties (1- (point)) (point)
				      '(empire-input t rear-nonsticky t)))
	       (comint-send-string process cmd))))
      (setq cmds (cdr cmds)))))

(defun empire-filter-input (input filters)
  "Filter INPUT through FILTERS."
  (while (and filters input)
    (setq input (funcall (car filters) input))
    (setq filters (cdr filters)))
  input)


;;; Functions for tools to insert & send input

(defun empire-insert-input (string)
  "Insert STRING into *empire* buffer as if it were typed in by the user.
If STRING contains newlines, send the current input up to the last newline."
  (let ((saved-buffer (window-buffer (selected-window)))
	(pop-up-windows nil)
	(pop-up-frames nil))
    (or (zerop (length string))
	;; doesn't work if current buffer is *empire*
	(unwind-protect
	    (progn
	      (pop-to-buffer (get-buffer "*empire*"))
	      (let ((this-command 'yank))
		(comint-preinput-scroll-to-bottom))
	      (let ((opoint (point)))
		(insert string)
		(save-excursion
		  (cond ((search-backward "\n" opoint t)
			 (comint-send-input)
			 (delete-char 1))))))
	  (pop-to-buffer saved-buffer)))))

(defun empire-send-command (string)
  "Insert and send empire command STRING.
Use function `empire-wait-until-non-busy' to wait for the command's completion."
  (let ((saved-buffer (window-buffer (selected-window)))
	(pop-up-windows nil)
	(pop-up-frames nil)
	proc-mark point-mark)
    (unwind-protect
	(progn
	  (pop-to-buffer (get-buffer "*empire*"))
	  (setq proc-mark (process-mark (get-buffer-process (current-buffer))))
	  (setq point-mark (and (/= (point) proc-mark) (point-marker)))
	  (goto-char proc-mark)
	  (insert string)
	  (let ((comint-eol-on-send nil)) ; send exactly up to point
	    (comint-send-input))
	  (goto-char (or point-mark proc-mark))
	  (if point-mark (set-marker point-mark nil)))
      (pop-to-buffer saved-buffer))))

(defun empire-wait-until-non-busy (&optional partial)
  "Wait until empire session is not busy.
If optional argument PARTIAL is non-nil, only wait for a prompt for
which no input is pending."
  (let ((process (get-buffer-process "*empire*")))
    (while (if partial
	       (/= 0 empire-sent-ahead)
	     empire-busy)
      (cond ((not (accept-process-output process 1))
	     ;; no progress for one second, better tell user what's up
	     (message "Waiting for empire server sending prompt")
	     (accept-process-output process)))
      (sit-for 0)
      (or (eq (process-status process) 'open)
	  (error "Empire session died")))
    (message "")))


;;; Receiving output

(defun empire-process-filter (process output)
  ;; Process filter for empire connection PROCESS.
  ;; Split OUTPUT in lines and feed it to value of `empire-consumer'.
  ;; Save a remaining partial line in variable `empire-partial-line'.
  (if (process-buffer process)
      (save-excursion
	(save-restriction
	  (save-match-data
	    (widen)
	    (let ((start 0)
		  (debug-on-error t) (debug-on-quit t) (inhibit-quit nil)) ; debugging on
	      (set-buffer (process-buffer process))
	      (while (string-match "\n" output start)
		(let* ((msg (concat empire-partial-line
				    (substring output start (match-end 0))))
		       (tag (car (rassq (upcase (aref msg 0))
					empire-server-tag-alist))))
		  (setq start (match-end 0))
		  (if (and tag (= (aref msg 1) ?\ ))
		      (funcall empire-consumer
			       process
			       tag
			       (substring msg 2))
		    (empire-message (format "Ignored strange line `%s'"
					    (substring msg 0 -1))))
		  (setq empire-partial-line "")))
	      (setq empire-partial-line (substring output start))))))
    (delete-process process))
  (run-hooks 'empire-after-output-hook))

(defun empire-login (process tag args)
  ;; Empire connection PROCESS sent TAG with ARGS.
  ;; Check TAG, insert ARGS into buffer, send next command as
  ;; described by empire-login-proto.
  ;; This is in `empire-consumer' during initial login.
  (let ((expected (nth 0 (car empire-login-proto)))
	(handler  (nth 1 (car empire-login-proto)))
	(command  (nth 2 (car empire-login-proto))))
    (cond ((eq tag expected)
	   (empire-insert-output args)
	   (and command (process-send-string process (concat command "\n")))
	   (setq empire-login-proto (cdr empire-login-proto))
	   (or empire-login-proto
	       (setq empire-consumer 'empire-play)))
	  (t
	   (if handler (funcall handler expected tag args))
	   (if (> (length args) 0)
	       (setq args (substring args 0 -1)))
	   (empire-abort (if (and (eq tag 'cmderr)
				  (> (length args) 0))
			     args
			   (format "expected %s, got %s %s"
				   expected tag args)))))))


(defun empire-play (process tag args)
  ;; Empire connection PROCESS sent TAG with ARGS.
  ;; Do the right thing.
  ;; This is in `empire-consumer' during play.
  (cond ((eq tag 'init)
	 (let ((good-proto 2))
	   (if (/= (string-to-number args) good-proto)
	       (empire-abort (format "server protocol %s, expected %s"
				     args good-proto)))
	   (setq args nil)))
	((eq tag 'exit)
	 (setq args (concat "Exit: " args)))
	((eq tag 'flush)
	 (setq args (substring args 0 -1)))
	((eq tag 'prompt)
	 (empire-close-pipe)
	 (empire-unredirect)
	 (setq args
	       (apply 'format
		      empire-prompt-format
		      (if (string-match "\\`\\s-*\\([0-9]+\\)\\s-+\\([0-9]+\\)"
					args)
			  (list (substring args
					   (match-beginning 1)
					   (match-end 1))
				(substring args
					   (match-beginning 2)
					   (match-end 2)))
			(empire-message (format "Got bad prompt `%s'"
						(substring args 0 -1)))
			(list "" "")))))
	((eq tag 'abort)
	 (setq args (concat "Aborted\n" args)))
	((eq tag 'redir)
	 (setq args (substring args 0 -1))
	 (if (and (string-match "\\`\\s-*>\\([>!]?\\)\\s-*" args)
		  (< (match-end 0) (length args)))
	     (empire-redirect (substring args (match-end 0))
			      (cdr (assoc (substring args
						     (match-beginning 1)
						     (match-end 1))
					  '((""  . excl)
					    (">" . append)
					    ("!" . trunc)))))
	   (empire-message (format "Bad redirect `%s'" args)))
	 (setq args nil))
	((eq tag 'pipe)
	 (setq args (substring args 0 -1))
	 (if (and (string-match "\\`\\s-*|\\s-*" args)
		  (< (match-end 0) (length args)))
	     (empire-open-pipe (substring args (match-end 0)))
	   (empire-message (concat "Bad pipe `%s'" args)))
	 (setq args nil))
	((memq tag '(cmderr badcmd))
	 (setq args (concat "Error; " args)))
        ((eq tag 'execute)
	 (setq args (substring args 0 -1))
	 (if (string-match "\\S-" args)
	     (empire-execute (substring args (match-beginning 0)))
	   (empire-message (format "Bad execute `%s'" args)))
	 (setq args nil))
	((not (eq tag 'data))
	 (empire-message (format "Ignored %s `%s'" tag (substring args 0 -1)))
	 (setq args nil)))
  (cond ((> (length args) 0)
	 (empire-out (funcall empire-output-filter tag args))))
  (cond ((memq tag '(flush prompt))
	 ;; if prompt doesn't match regexp append magic cookie
	 (cond ((not (string-match comint-prompt-regexp args))
		(empire-insert-output empire-magic-prompt-cookie)))
	 ;; update variables
	 (if (>= empire-sent-ahead 1)
	     (setq empire-sent-ahead (1- empire-sent-ahead))
	   (message "Unexpected prompt")
	   (setq empire-sent-ahead 0))
	 (setq empire-busy (not (and (zerop empire-sent-ahead)
				     (eq tag 'prompt))))
	 ;; set current command, skip over type ahead
	 (if (>= empire-sent-ahead 1)
	     (let* ((start (or (marker-position empire-process-mark)
			       (marker-position (process-mark process))))
		    (end (if (get-text-property start 'empire-input)
			     start
			   (next-single-property-change start
							'empire-input))))
	       (setq empire-current-input
		     (buffer-substring start (or end (point-max))))
	       (set-marker empire-process-mark
			   (if end (1+ end) (point-max)))))
	 ;; if in sync, clear empire-process-mark
	 (if (<= empire-sent-ahead 1)
	     (set-marker empire-process-mark nil)))
	((eq tag 'exit)
	 (delete-process process))))

(defun empire-out (string)
  ;; If it is piped, send STRING to pipe.
  ;; else if empire connection is redirected, append STRING to redirection,
  ;; else insert string into buffer.
  (cond (empire-pipe-process
	 (if (memq (process-status empire-pipe-process) '(run stop))
	     ;; process may exit asynchronously, have to catch SIGPIPE
	     (condition-case err
		 (process-send-string empire-pipe-process string)
	       (error (or (string-match "SIGPIPE" (nth 1 err))
			  (signal (car err) (cdr err)))))))
	((bufferp empire-redir-buffer)
	 (save-excursion
	   (set-buffer empire-redir-buffer)
	   (save-restriction
	     (widen)
	     (goto-char (point-max))
	     (insert string))))
	((not empire-redir-buffer)
	 (empire-insert-output string))))

(defun empire-redirect (file mode)
  ;; Redirect connection output to FILE.
  ;; MODE excl means don't overwrite FILE if it exists already.
  ;; MODE append appends to FILE.
  ;; Another MODE overwrites FILE.
  (let ((buffer (get-file-buffer file))
	new-buffer)
    (cond ((string= file "/dev/null")	; handled specially for speed
	   (setq empire-redir-buffer t)) ; ignore output
	  ((and (eq mode 'excl)
		(or buffer (file-nlinks file)))
	   (setq empire-redir-buffer t)	; ignore output
	   (empire-message (format "Redirect: `%s' exists" file)))
	  ((not (file-writable-p file))
	   (setq empire-redir-buffer t)	; ignore output
	   (empire-message (format "Redirect: `%s' unwriteable" file)))
	  (t
	   (or buffer
	       (setq buffer (find-file-noselect file t)
		     new-buffer t))
	   (setq empire-redir-buffer buffer)
	   (put 'empire-redir-buffer 'empire-keep
		(or (not new-buffer) empire-keep-redir-buffers))
	   (or (eq mode 'append)
	       (save-excursion
		 (set-buffer buffer)
		 (erase-buffer)))))))

(defun empire-unredirect ()
  ;; Terminate redirection of empire connection PROCESS.
  (if (bufferp empire-redir-buffer)
      (save-excursion
	(set-buffer empire-redir-buffer)
	(save-buffer)
	(or (get 'empire-redir-buffer 'empire-keep)
	    (kill-buffer empire-redir-buffer))))
  (setq empire-redir-buffer nil))

(defun empire-open-pipe (command)
  ;; Pipe empire connection's output through COMMAND.
  (let ((process-connection-type nil))
    (string-match "\\S-+" command)
    (setq empire-pipe-process
	  (start-process-shell-command (substring command 0 (match-end 0))
				       nil command))
    (process-kill-without-query empire-pipe-process)
    ;; Set the process mark so that empire-pipe-filter can find the
    ;; buffer.  The actual position is irrelevant.
    (set-marker (process-mark empire-pipe-process) 1)
    (set-process-filter empire-pipe-process 'empire-pipe-filter)))

(defun empire-pipe-filter (process string)
  ;; Process filter for a pipe on behalf of empire connection PROCESS.
  ;; Insert STRING into buffer.
  (let ((buffer (marker-buffer (process-mark process)))
	(empire-pipe-process nil))
    (save-excursion
      (set-buffer buffer)
      (empire-out string))))

(defun empire-close-pipe ()
  ;; Finish piping empire connection through a command.
  ;; Send EOF and wait for command to terminate.
  (let ((pipe empire-pipe-process))
    (cond (pipe
	   (setq empire-pipe-process nil)
	   (cond ((memq (process-status pipe) '(run stop))
		  (process-send-eof pipe)
		  (condition-case err
		      (while (memq (process-status pipe) '(run stop))
			(let ((inhibit-quit nil))
			  (cond ((not (accept-process-output pipe 1))
				 ;; no progress for one second, better
				 ;; tell user what's up
				 (message (substitute-command-keys "Waiting for %s, \\[keyboard-quit] to quit")
					  (process-name pipe))
				 (accept-process-output pipe)))))
		    (quit (empire-message "killing...")
			  (delete-process pipe)))
		  (message nil)))
	   (empire-process-status pipe)
	   (delete-process pipe)))))

(defun empire-execute (script)
  ;; Feed SCRIPT to empire connection.
  (let ((process (get-buffer-process (current-buffer)))
	buffer)
    (unwind-protect
	(if (file-readable-p script)
	    (save-excursion
	      (setq buffer (generate-new-buffer " *empire-exec*"))
	      (set-buffer buffer)
	      (insert-file-contents script)
	      (comint-send-region process 1 (point-max)))
	  (empire-message (format "Execute: `%s' unreadable" script)))
      (process-send-string process "ctld\n")
      (if buffer (kill-buffer buffer)))))


;; Inserting output into buffer

(defun empire-process-status (process)
  "Insert any unusual status of PROCESS."
  (let ((name (process-name process))
	(status (process-status process))
	(code (process-exit-status process))
	event)
    (cond ((eq status 'run)
	   (setq event "%s is running"))
	  ((eq status 'stop)
	   (setq event "%s is stopped"))
	  ((eq status 'exit)
	   (setq event (and (/= code 0) "%s exited abnormally with code %d")))
	  ((eq status 'signal)
	   (setq event "%s died with signal %d"))
	  (t
	   (setq event (format "is %s" status))))
    (if event (empire-message (format event name code)))))

(defun empire-abort (message)
  "Abort empire connection with MESSAGE."
  (setq empire-consumer 'ignore)
  (empire-message (concat message " --- session died"))
  (delete-process (get-buffer-process (current-buffer))))

(defun empire-message (message)
  "Insert client MESSAGE."
  (empire-insert-output (concat (if (looking-at "^") "" "\n")
				"Client: "
				message
				"\n")))

(defun empire-insert-output (string)
  "Insert STRING as empire connection output."
  (let ((process (get-buffer-process (current-buffer))))
    (if (marker-position empire-process-mark)
	;; set process mark temporarily to empire-process-mark
	(let* ((pmark (process-mark process))
	       (saved-pmark (copy-marker pmark)))
	  (unwind-protect
	      (progn
		(set-marker pmark (marker-position empire-process-mark))
		(comint-output-filter process string)
		(empire-interpret-backspace comint-last-output-start pmark))
	    (set-marker pmark (marker-position saved-pmark))
	    (set-marker saved-pmark nil)))
      (comint-output-filter process string))))

(defun empire-interpret-backspace (from to)
  "Try to convert backspaces between FROM and TO into text properties."
  (save-excursion
    (let ((underline-props '(face underline rear-nonsticky t))
	  (overstrike-props '(face bold rear-nonsticky t)))
      (goto-char from)
      (while (search-forward "\b" to t)
	(let ((char1 (char-after (- (point) 2)))
	      (char2 (following-char)))
	  (cond ((and char1 (= char2 ?_))
		 (forward-char -1)
		 (delete-char 2)
		 (add-text-properties (1- (point)) (point) underline-props))
		((and char2 (= char1 ?_))
		 (delete-backward-char 2)
		 (add-text-properties (point) (1+ (point)) underline-props)
		 (forward-char))
		((and char1 (= char1 char2))
		 (delete-backward-char 2)
		 (add-text-properties (point) (1+ (point)) overstrike-props)
		 (forward-char))))))))

;;; empclient.el ends here
