;; $Header: /home/abel/cvs/src/misc/emacs/erc/erc.el,v 1.185 1998/08/11 22:28:11 abel Exp $

;; erc.el - an Emacs IRC client

;; Author: Alexander L. Belikoff (abel@bfr.co.il)
;; Version: 1.1 ($Revision: 1.185 $)
;; Keywords: IRC, client, Internet

;; Copyright (C) 1997 Alexander L. Belikoff

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:

;; ERC is an Emacs IRC client. For more information, see the supplied erc.txt


(defvar erc-version-string "1.1"
  "ERC version. This is used by `erc-version' function")

(defvar erc-official-location "mailto://abel@bfr.co.il"
  "Location of the ERC client on the Internet")


;; tunable connection and authentication parameters

(defvar erc-server nil
  "IRC server to use.

See a function `erc-compute-server' for more details on connection
parameters and authentication")


(defvar erc-port nil
  "IRC port to use")


(defvar erc-nick nil
  "Nickname to use.

See a function `erc-compute-nick' for more details on connection
parameters and authentication")


(defvar erc-user-full-name nil
  "User full name.

See a function `erc-compute-full-name' for more details on connection
parameters and authentication")


(defvar erc-password nil
  "ERC password to use in authentication (not necessary)")


;; tunable GUI stuff

(defvar erc-prompt "ERC>"
  "Prompt used by ERC. Trailing whitespace is not required")


(defvar erc-notice-prefix "*** "
  "Prefix for all notices")


(defvar erc-notice-highlight-type 'all
  "Determines how to highlight notices. The following values are
allowed:

    'prefix - highlight notice prefix only
    'all    - highlight the entire notice

Any other value disables notice's highlighting altogether.")


(defvar erc-pal-highlight-type 'nick
  "Determines how to highlight pal's messages. The following values are
allowed:

    'nick - highlight pal's nickname only
    'all  - highlight the entire message from pal

Any other value disables pal highlighting altogether.")


(defvar erc-uncontrol-input-line t
  "Determines whether to interpret the control characters on the input line
before rewriting it.

The \"uncontrolled\" line usually looks much nicer (with bold and other
fonts), but all control character information gets lost. That is, if you
press RET on this line again, it will be parsed without any control
characters")


;; other tunable parameters

(defvar erc-auto-discard-away t
  "If non-NIL, then sending anything, while being AWAY, automatically
discards the AWAY state")


(defvar erc-paranoid nil
  "Paranoid mode. If non-nil, then all incoming CTCP requests will be shown")


(defvar erc-pals ()
  "List of pals on IRC")


(defvar erc-startup-file-list
  '(".ercrc.el" ".ercrc" "~/.ercrc.el" "~/.ercrc" "~/.ircrc")
  "List of files to try for a startup script. The first existant and
readable one will get executed.

If the filename ends with `.el' is is presumed to be an emacs-lisp
script and it gets (load)ed. Otherwise is is treated as a bunch of
regular IRC commands")


;; variables available for IRC scripts

(defvar erc-user-information "ERC User"
  "USER_INFORMATION IRC variable")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; requirements

(require 'edo-tools)


;; mode-specific tables

(defvar erc-mode-syntax-table nil
  "Syntax table used while in ERC mode.")

(cond ((not erc-mode-syntax-table)
       (setq erc-mode-syntax-table (make-syntax-table))
       (modify-syntax-entry ?\" ".   " erc-mode-syntax-table)
       (modify-syntax-entry ?\\ ".   " erc-mode-syntax-table)
       (modify-syntax-entry ?' "w   " erc-mode-syntax-table)))

(defvar erc-mode-abbrev-table nil
  "Abbrev table used while in ERC mode.")

(define-abbrev-table 'erc-mode-abbrev-table ())

(defvar erc-mode-map nil)

(cond ((not erc-mode-map)
       (setq erc-mode-map (make-sparse-keymap))
       (define-key erc-mode-map "\C-m" 'erc-send-current-line)
;       (define-key erc-mode-map "\M-p" 'erc-previous-command)
;       (define-key erc-mode-map "\M-n" 'erc-next-command)
;       (define-key erc-mode-map "\t" 'erc-complete)
       (define-key erc-mode-map "\C-ca" 'erc-input-action)))


;; faces

;; Honestly, I have a horrible sense of color and the "defaults" below
;; are supposed to be really bad. But colors ARE required in IRC to
;; convey different parts of conversation. If you think you know better
;; defaults - send them to me.

(edo-make-face-once 'erc-default-face)
(edo-make-face-once 'erc-direct-msg-face nil "IndianRed")
(edo-make-face-once 'erc-input-face 'italic "Green4")
(edo-make-face-once 'erc-bold-face 'bold)
(edo-make-face-once 'erc-inverse-face nil "White" "Black")
(edo-make-face-once 'erc-underline-face 'underline)
(edo-make-face-once 'erc-prompt-face 'bold "Black" "Tan")
(edo-make-face-once 'erc-notice-face 'italic "SlateBlue")
(edo-make-face-once 'erc-action-face 'bold)
(edo-make-face-once 'erc-error-face nil "White" "Red")
(edo-make-face-once 'erc-pal-face 'bold "White" "Magenta")


;; debugging support

(defvar erc-log-p nil
  "Do debug logging if non-NIL")

(defvar erc-debug-log-file
  (concat (expand-file-name ".") "/ERC.debug")

  "Debug log file name")


(defmacro erc-log (string)
  "Logs STRING if logging is on (see `erc-log-p')"

  `(if erc-log-p
       (erc-log-aux ,string)))


(defun erc-log-aux (string)
  "Do the debug logging of STRING"

  (let ((cb (current-buffer)))
    (set-buffer (process-buffer proc))
    (set-buffer dbuf)
    (goto-char (point-max))
    (insert (concat "** " string "\n"))
    (set-buffer cb)))


;; now the mode activation routines

(defun erc-mode ()
  "Major mode for Emacs IRC
Special commands:

\\{erc-mode-map}

Turning on text-mode runs the hook `erc-mode-hook'."
  
  (interactive)
  (kill-all-local-variables)

  (use-local-map erc-mode-map)
  (setq mode-name "ERC")
  (setq major-mode 'erc-mode)
  (setq local-abbrev-table erc-mode-abbrev-table)
  (set-syntax-table erc-mode-syntax-table)
  (run-hooks 'erc-mode-hook))


;; activation

(defconst erc-default-server "irc.funet.fi"
  "IRC server to use if it cannot be detected otherwise")

(defconst erc-default-port "irc"
  "IRC port to use if it cannot be detected otherwise")


(defun erc (&optional server port nick full-name)
  "Run ERC"

  (let* ((bufname (concat "*ERC: " server "*"))
	 (buffer (get-buffer-create bufname)))
    (set-buffer buffer)
    (switch-to-buffer buffer)
    (erc-mode)
    ;; go to the end of the buffer and open a new line
    ;; (the buffer may have existed)
    (goto-char (point-max))
    (open-line 1)
    (goto-char (point-max))

    ;; make local variables

    ;; connection parameters
    (make-variable-buffer-local 'erc-session-server)
    (make-variable-buffer-local 'erc-session-port)
    (make-variable-buffer-local 'erc-session-user-full-name)
    (make-variable-buffer-local 'proc)
    ;; stack of default recepients
    (make-variable-buffer-local 'def-rcpts)
    (setq def-rcpts ())
    ;; stack for user's nicknames
    (make-variable-buffer-local 'nick-stk)
    (setq nick-stk ())
    ;; assoc list of pairs (TIME-OF-PING-REQUEST-SENT . DESTINATION)
    (make-variable-buffer-local 'pings)
    (setq pings ())
    ;; last incomplete line read
    (make-variable-buffer-local 'prev-rd)
    (setq prev-rd "")
    ;; last peers (sender and receiver)
    (make-variable-buffer-local 'last-peers)
    (setq last-peers '(nil . nil))
    ;; last invitation channel
    (make-variable-buffer-local 'invitation)
    (setq invitation nil)
    ;; away flag
    (make-variable-buffer-local 'away)
    (setq away nil)
    ;; time of last command sent
    (make-variable-buffer-local 'last-sent-time)
    (setq last-sent-time (erc-current-time))
    ;; user requested quit
    (make-variable-buffer-local 'quitting)
    (setq quitting nil)
    ;; login-time 'nick in use' error
    (make-variable-buffer-local 'bad-nick)
    (setq bad-nick nil)
    ;; debug output buffer
    (make-variable-buffer-local 'dbuf)
    (setq dbuf 
	  (if erc-log-p
	      (get-buffer-create (concat "*ERC-DEBUG: " server "*"))
	    nil))

    (erc-determine-parameters server port nick full-name)
    (erc-connect)))


;; interactive startup

(defvar erc-server-history-list nil
  "IRC server interactive selection history list")

(defvar erc-nick-history-list nil
  "Nickname interactive selection history list")


(defun erc-select ()
  "Interactively select connection parameters and run ERC"

  (interactive "")
  (let
      ((server (read-from-minibuffer
		"IRC server: " erc-server nil nil 'erc-server-history-list))
       (port
	(erc-string-to-port
	 (read-from-minibuffer "IRC port: "
			       (erc-port-to-string (or erc-port "irc")))))
       (nick (read-from-minibuffer
	      "Nickname: " (or erc-nick (user-login-name))
	      nil nil 'erc-nick-history-list)))
    (erc server port nick nil)))


;; process management

(defun erc-connect ()
  "Perform the connection and login"

  (message "Connecting to %s:%s..." erc-session-server erc-session-port)
  (setq proc (open-network-stream
	      (format "erc-%s-%s" erc-session-server erc-session-port)
	      (current-buffer)
	      erc-session-server
	      erc-session-port))
  (message "Connecting to %s:%s... done" erc-session-server erc-session-port)
  (set-process-sentinel proc 'erc-process-sentinel)
  (set-process-filter proc 'erc-process-filter)
  (set-marker (process-mark proc) (point))
  (erc-log "\n\n\n********************************************\n")
  (message "Logging in as \'%s\'..." (erc-current-nick))
  (erc-login)
  (message "Logging in as \'%s\'... done" (erc-current-nick))

  ;; execute a startup script
  (let ((f (erc-select-startup-file)))
    (cond
     ((not f)
      nil)

     (t
      (message "Loading \'%s\'..." f)
      (erc-load-script f)
      (message "Loading \'%s\'... done" f))))
  (erc-update-mode-line)
  (erc-display-prompt))


(defun erc-process-filter (proc string)
  "Filter messages from server"

  (erc-log (concat "erc-process-filter: " string))
  (let ((ob (current-buffer))
	(nb (process-buffer proc)))
    (set-buffer nb)
    (mapcar (lambda (s)
	      (if (not (null s))
		  (erc-display-line (concat s "\n"))))
	    (mapcar 'erc-parse-line-from-server
		    (erc-split-multiline string)))
    (set-buffer ob)))


(defun erc-process-sentinel (cproc event)
  "Sentinel function for ERC process"

  (erc-log (format
	    "SENTINEL: proc: %S  status: %S  event: %S (quitting: %S)"
	    proc (process-status proc) event quitting))

  (save-excursion
    (set-buffer (process-buffer cproc))
    (goto-char (point-max))

    (if (string= event "exited abnormally with code 256\n")

	;; Sometimes (eg on a /LIST command) ERC happens to die with
	;; an exit code 256. The icrii client also shows this behavior
	;; and it restarts itself. So do I.

	(cond
	 ((not quitting)
	  (open-line 1)
	  (goto-char (point-max))
	  (insert
	   (erc-highlight-error
	    "Connection failed! Re-establishing connection...\n"))
	  (erc-connect))

	 (t
	  (let* ((wd (window-width))
		(msg "*** ERC finished ***")
		(off (/ (- wd (length msg)) 2))
		(s ""))
	    (if (> off 0)
		(setq s (make-string off ?\ )))
	    (insert (concat "\n\n" s msg "\n")))))
      (insert (concat "\n\n*** ERC terminated: " event "\n"))))
  (goto-char (point-max))
  (erc-update-mode-line))


;;; I/O interface
  
;; send interface

(defun erc-send-command (l)
  "Send command line L to IRC server.

The command line must contain neither prefix nor trailing `\\n'"
  
  (erc-log (concat "erc-send-command: " l))
  (if (and away erc-auto-discard-away)
      (erc-process-away nil))
  (process-send-string proc (concat l "\n"))
  (setq last-sent-time (erc-current-time)))


(defun erc-send-ctcp-message (tgt l)
  "Send CTCP message L for TGT. If TGT is nil the message is not sent.

The command must contain neither prefix nor trailing `\\n'"

  (cond
   (tgt
    (erc-log (format "erc-send-CTCP-message: [%s] %s" tgt l))
    (erc-send-command (format "PRIVMSG %s :\C-a%s\C-a" tgt l)))))


(defun erc-send-ctcp-notice (tgt l)
  "Send CTCP notice L for TGT. If TGT is nil the message is not sent.

The command must contain neither prefix nor trailing `\\n'"
  
  (cond
   (tgt
    (erc-log (format "erc-send-CTCP-notice: [%s] %s" tgt l))
    (erc-send-command (format "NOTICE %s :\C-a%s\C-a" tgt l)))))


(defun erc-send-action (tgt l)
  "Send CTCP ACTION information described by the L string to TGT"

  (erc-send-ctcp-message tgt (format "ACTION %s" l)))


;; command tables

(defconst erc-command-table
  ;; MSG and NOTICE are put first to ease the search.
  '(("MSG" erc-cmd-msg "")
    ("NOTICE" erc-cmd-msg "")
    ("AWAY" erc-cmd-away "")
    ("BYE" erc-cmd-quit "")
    ("CHANNEL" erc-cmd-join "")
    ("COMMENT" nil "")
    ("CTCP" erc-cmd-ctcp "")
    ("DATE" erc-cmd-time "")
    ("DESCRIBE" erc-cmd- "")
    ("EXIT" erc-cmd-quit "")
    ("JOIN" erc-cmd-join "")
    ("KICK" erc-cmd-kick "")
    ("LEAVE" erc-cmd-part "")
    ("LOAD" erc-cmd-load "")
    ("ME" erc-cmd-me "")
    ("NICK" erc-cmd-nick "")
    ("PART" erc-cmd-part "")
    ("PING" erc-cmd-ping "")
    ("QUERY" erc-cmd-query "")
    ("QUIT" erc-cmd-quit "")
    ("SERVER" erc-cmd-server "")
    ("SET" erc-cmd-not-implemented "")
    ("SIGNOFF" erc-cmd-quit "")
    ("TIME" erc-cmd-time "")
    ("TOPIC" erc-cmd-topic ""))

  "Table of input commands supported by ERC. Each entry has a form
\(NAME FUNCTION HELP\)

NAME is a command name in uppercase. FUNCTION is a command handler or NIL to
ignore the command. This function must take two strings as arguments:
a command name and the rest of the command line. HELP is a character string
containing one-line help for the command.

There may be a default handler for all other commands. It must be represented
by an entry with NAME as an empty string")












;; display interface

(defun erc-display-line (string)
  "Display STRING in the ERC. All screen output must be done through this
function.

The line gets filtered to interpret the control characters. If STRING is NIL,
the function does nothing"
  
  (erc-log (concat "erc-display-line: " string))
  (if string
      (save-excursion
	(set-buffer (process-buffer proc))
	(goto-char (process-mark proc))
	(insert (erc-interpret-controls string))
	(set-marker (process-mark proc) (point))
	(goto-char (point-max)))))


(defun erc-send-current-line ()
  "Parse current line and send it to IRC"
  
  ;; Plan of attack:
  ;; save the string (without prompt) to s
  ;; return if s is empty
  ;; if not last line - go to the last one and one more
  ;; clear current line
  ;; print prompt in prompt face
  ;; uncontrol & print the line
  ;; send line to the parser
  ;; set the process marker
  ;; open new line and print the prompt there

  (interactive)
  (let* ((l (erc-parse-current-line))
	 (s (concat (car l) (cdr l)))
	 (s2 (concat (car l) (cdr l)))) ; distinct copy of s to stuff w/ faces
    (cond
     ((string-match "^\\s-*\n*$" s)
      (message "Blank line - ignoring...")
      (ding))

     (t
      (cond ((> (count-lines (point) (point-max)) 0)
	     (goto-char (point-max))
	     (open-line 1)
	     (goto-char (point-max))))
      (delete-region
       (progn (beginning-of-line) (point))
       (progn (end-of-line) (point)))
      (erc-display-prompt)
      ;; s2 is a copy of s used for insertion, It'll be stuffed with faces
      (put-text-property 0 (length s2) 'face 'erc-input-face s2)
      (insert (if erc-uncontrol-input-line
		  (erc-interpret-controls s2)
		s2))
      (goto-char (point-max))
      (open-line 1)
      (goto-char (point-max))
      (set-marker (process-mark proc) (point))
      (erc-display-prompt)
      (erc-process-input-line s)))))


(defun erc-process-input-line (line)
  "Translate LINE to an RFC1459 command and send it based on a table
in `erc-command-table'.

If the command in the LINE is not in the table, it is passed to
`erc-cmd-default'. If LINE is not a command (ie. doesn't start with /<WORD>)
then it is sent as a message.

See documentation for `erc-command-table' for more details."

  (cond					; invoke method in the table
   ((string-match "^\\s-*/\\([A-Za-z]+\\)\\(\\s-+.*\\|\\s-*\\)$" line)
    (let* ((cmd (upcase (match-string 1 line)))
	   (args (match-string 2 line))
	   (e (assoc cmd erc-command-table))
	   (fn (if e
		   (nth 1 e)
		 #'erc-cmd-default)))
      (erc-log (format "proc-input: lookup: [%S] [%S] -> %S" cmd args e))
      (if (not (funcall fn cmd args))
	  (erc-display-line (erc-highlight-error "Bad syntax\n")))))
   (t					; send as text
    (let ((r (erc-default-target)))
      (if r
	  (erc-send-command (format "PRIVMSG %s :%s" r line))
	(erc-display-line (erc-highlight-error "No target\n")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    Input commands handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erc-cmd-not-implemented (cmd line)
  (erc-log (format "cmd: NOT IMPLEMENTED: %s %s" cmd line))
  (erc-display-line (erc-highlight-error "Not implemented\n"))
  t)


(defun erc-cmd-default (cmd line)
  (erc-log (format "cmd: DEFAULT: %s %s" cmd line))
  (erc-send-command (concat cmd line))
  t)


(defun erc-cmd-away (cmd line)
  (cond
   ((string-match "^\\s-*\\(.*\\)$" line)
    (let ((reason (match-string 1 line)))
      (erc-log (format "cmd: AWAY: %s" reason))
      (erc-send-command
       (if (string= reason "")
	   "AWAY"
	 (concat "AWAY :" reason))))
    t)
   (t
    nil)))


(defun erc-cmd-ctcp (cmd line)
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-+\\(.*\\)$" line)
    (let ((nick (match-string 1 line))
	  (cmd (match-string 2 line)))
      (erc-log (format "cmd: CTCP [%s]: [%s]" nick cmd))
      (erc-send-ctcp-message nick cmd))
    t)
   (t
    nil)))

     
(defun erc-cmd-describe (cmd line)
  (cond
   ((string-match
     "^\\s-*\\(\\S-+\\)\\s-\\(.*\\)$" line)
    (let ((dst (match-string 1 line))
	  (s (match-string 2 line)))
      (erc-log (format "cmd: DESCRIBE: [%s] %s" dst s))
      (erc-send-action dst s))
    t)
   (t
    nil)))


(defun erc-cmd-join (cmd line)
  (cond
   ((string-match "^\\s-*\\(\\S-*\\)\\s-*$" line)
    (let ((s (match-string 1 line))
	  (chnl nil))
      (erc-log (format "cmd: JOIN: %s" s))
      (cond
       ((string= (upcase s) "-INVITE")
	(if invitation
	    (setq chnl invitation
		  invitation nil)
	  (message "You've got no invitation")))
       (t
	(setq chnl s)))
      (cond
       (chnl
	(erc-send-command (format "JOIN %s" chnl))
	(erc-send-command (format "MODE %s" chnl)))))
    t)
   (t
    nil)))

     
(defun erc-cmd-kick (cmd line)
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-\\(.*\\)$" line)
    (let ((ch (match-string 1 line))
	  (nick (match-string 2 line))
	  (reason (match-string 3 line)))
      (erc-log (format "cmd: KICK: %s/%s: %s" nick ch reason))
      (erc-send-command (format "KICK %s %s :%s" ch nick reason)))
    t)
   (t
    nil)))


(defun erc-cmd-load (cmd line)
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)" line)
    (let ((file (match-string 1 line)))
      (erc-log (format "cmd: LOAD: %s" file))
      (cond
       ((not (file-readable-p file))
	(erc-display-line
	 (erc-highlight-error (concat "Cannot read file " file))))
       (t
	(message "Loading \'%s\'..." file)
	(erc-load-script file))))
    t)
   (t
    nil)))


(defun erc-cmd-me (cmd line)
  (cond
   ((string-match "^\\s-\\(.*\\)$" line)
    (let ((s (match-string 1 line)))
      (erc-log (format "cmd: ME: %s" s))
      (erc-send-action (erc-default-target) s))
    t)
   (t
    nil)))

   
(defun erc-cmd-msg (cmd line)
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\(\\s-*$\\|\\s-+.*\\)" line)
    (let ((tgt (match-string 1 line))
	  (s (match-string 2 line)))
      (erc-log (format "cmd: MSG(%s): [%s] %s" cmd tgt s))
      (cond
       ((string= tgt ",")
	(if (car last-peers)
	    (setq tgt (car last-peers))
	  (setq tgt nil)))
       ((string= tgt ".")
	(if (cdr last-peers)
	    (setq tgt (cdr last-peers))
	  (setq tgt nil))))
      (cond
       (tgt
	(setcdr last-peers tgt)
	(erc-send-command
	 (if (string= cmd "MSG")
	     (format "PRIVMSG %s :%s" tgt s)
	   (format "NOTICE %s :%s" tgt s))))
       (t
	(erc-display-line (erc-highlight-error "No target\n")))))
    t)
   (t
    nil)))


(defun erc-cmd-nick (cmd line)
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-*$" line)
    (let ((nick (match-string 1 line)))
      (erc-log (format "cmd: NICK: %s (bad-nick: %S)" nick bad-nick))
      (erc-send-command (format "NICK %s" nick))
      (cond (bad-nick
	     (erc-push-nick nick)
	     (erc-update-mode-line)
	     (setq bad-nick nil))))
    t)
   (t
    nil)))


(defun erc-cmd-part (cmd line)
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-*\\(.*\\)$" line)
    (let ((ch (match-string 1 line))
	  (msg (match-string 2 line)))
      (erc-log (format "cmd: PART: %s: %s" ch msg))
      (erc-send-command
       (if (string= msg "")
	   (format "PART %s" ch)
	 (format "PART %s :%s" ch msg))))
    t)
   (t
    nil)))


(defun erc-cmd-ping (cmd line)
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-*$" line)
    (let ((s (match-string 1 line))
	  (ct (erc-current-time)))
      (erc-log (format "cmd: PING: %s" s))
      (erc-send-ctcp-message s (format "PING %d" ct))
      (setq pings (cons (cons ct (erc-trim-string s)) pings)))
    t)
   (t
    nil)))


(defun erc-cmd-query (cmd line)
  (cond
   ((string-match "^\\s-*\\(\\S-*\\)\\s-*$" line)
    (let ((s (match-string 1 line)))
      (erc-log (format "cmd: QUERY [%s]" s))
      (if (string= s "")
	  (erc-delete-query)
	(erc-add-query s))
      (erc-update-mode-line))
    t)
   (t
    nil)))


(defun erc-cmd-quit (cmd line)
  (cond
   ((string-match "^\\s-*\\(.*\\)$" line)
    (let ((s (match-string 1 line)))
      (erc-log (format "cmd: QUIT: %s" s))
      (setq quitting t)
      (erc-send-command
       (if (or (not s) (string= s ""))
	   (format
	    "QUIT :\C-bERC\C-b v%s (IRC client for Emacs) - \C-b%s\C-b"
	    erc-version-string erc-official-location)
	 (format "QUIT :%s" s))))
    t)
   (t
    nil)))


(defun erc-cmd-server (cmd line)
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-*$" line)
    (let ((nserver (match-string 1 line)))
      (erc-log (format "cmd: SERVER: %s" nserver))
      (let ((quitting t))
	(delete-process proc)
	(setq erc-session-server nserver)
	(erc-connect))
      (erc-update-mode-line))
    t)
   (t
    nil)))


(defun erc-cmd-time (cmd line)
  (cond
   ((string-match "^\\s-*\\(.*\\)$" line)
    (let ((args (match-string 1 line)))
      (erc-log (format "cmd: TIME: %s" args))
      (erc-send-command (concat "TIME" args)))
    t)
   (t
    nil)))


(defun erc-cmd-topic (cmd line)
  (cond
   ((string-match "^\\s-*\\(\\S-+\\)\\s-\\(.*\\)$" line)
    (let ((ch (match-string 1 line))
	  (topic (match-string 2 line)))
      (erc-log (format "cmd: TOPIC [%s]: %s" ch topic))
      (erc-send-command (format "TOPIC %s :%s" ch topic)))
    t)
   (t
    nil)))


(defun erc-reformat-command (line)
  "Re-format command in the line

Removes leading [and trailing?] spaces and converts a command to uppercase.
Returns a string untouched, if it doesn't contain a command"

  (if (string-match "^\\s-*/\\([A-Za-z]+\\)\\(\\s-+.*\\|\\s-*\\)$" line)
      (concat "/" (upcase (match-string 1 line)) " " (match-string 2 line))
    line))
  

(defun erc-interpret-controls (line)
  "Translate control characters in the line to faces and beeps"

  (cond
   ((not (null line))
    (let ((res "")
	  (i 0)
	  (j 0)
	  (len (length line))
	  (hl-start nil)
	  (hl-face nil))
      (while (< i len)
	(let ((c (string-to-char (substring line i))))
;;	(erc-log (format "intp-c: i %d c [%c] st %S fc %S"
;;			 i c hl-start hl-face))
	  (cond ((char-equal c ?\^g)	; beep
		 (ding t))
		
		;; bold
		((char-equal c ?\^b)
;;	       (erc-log	(format "intp-c: BOLD i %d j %d st %S" i j hl-start))
		 (cond ((not hl-start)
			(setq hl-start j
			      hl-face 'erc-bold-face))
		       (t
			(put-text-property hl-start j 'face hl-face res)
			(if (eq hl-face 'erc-bold-face)
			    (setq hl-start nil
				  hl-face nil)
			  (setq hl-start j
				hl-face 'erc-bold-face)))))
		
		;; underline
		((char-equal c ?\^_)
;;	       (erc-log	(format "intp-c: UNDL i %d j %d st %S" i j hl-start))
		 (cond ((not hl-start)
			(setq hl-start j
			      hl-face 'erc-underline-face))
		       (t
			(put-text-property hl-start j 'face hl-face res)
			(if (eq hl-face 'erc-underline-face)
			    (setq hl-start nil
				  hl-face nil)
			  (setq hl-start j
				hl-face 'erc-underline-face)))))

		;; inverse
		((char-equal c ?\^v)
;;	       (erc-log	(format "intp-c: INV i %d j %d st %S" i j hl-start))
		 (cond ((not hl-start)
			(setq hl-start j
			      hl-face 'erc-inverse-face))
		       (t
			(put-text-property hl-start j 'face hl-face res)
			(if (eq hl-face 'erc-inverse-face)
			    (setq hl-start nil
				  hl-face nil)
			  (setq hl-start j
				hl-face 'erc-inverse-face)))))
		
		(t
		 (setq res (concat res (char-to-string c)))
		 (put-text-property j (1+ j) 'face
				    (get-char-property i 'face line) res)
		 (setq j (1+ j)))))
	(setq i (1+ i)))
      (if hl-start
	  (put-text-property hl-start (1- j) 'face hl-face res))
      res))

   (t nil)))


(defun erc-display-prompt ()
  "Display ERC prompt (defined by `erc-prompt' variable) using
`erc-prompt-face' at the current position"

  (if (> (length erc-prompt) 0)
      (let ((s (concat erc-prompt " ")))
	(put-text-property 0 (length erc-prompt) 'face 'erc-prompt-face s)
	(put-text-property (length erc-prompt) (length s)
			   'face 'erc-input-face s)
	(insert s))))


;; interactive operations

(defvar erc-action-history-list ()
  "History list for interactive action input")


(defun erc-input-action ()
  "Interactively input a user action and send it to IRC"

  (interactive "")
  (let ((action (read-from-minibuffer 
		 "Action: " nil nil nil erc-action-history-list)))
    (if (not (string-match "^\\s-*$" action))
	(erc-send-action (erc-default-target) action))))


(defun erc-complete ()
  (interactive)
  (let ((l (erc-parse-current-line)))
    (erc-log (concat "erc-complete: (" (car l) " . " (cdr l) ")"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                        IRC SERVER INPUT HANDLING
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun erc-parse-line-from-server (string)
  "parse the message received from server.

Returns the valuable part of the message"

  (erc-log (concat "parse: input: " string))
  (cond

   ;; numeric replies
;;   ((string-match (concat "^:\\S-+\\s-+\\([0-9][0-9][0-9]\\)\\s-+"
;;			  (erc-current-nick) "\\s-+\\(.*\\)$") string)
   ((string-match
     "^:\\S-+\\s-+\\([0-9][0-9][0-9]\\)\\s-+\\S-+\\s-+\\(.*\\)$" string)
    (erc-process-num-reply
     (string-to-number (match-string 1 string)) (match-string 2 string)))

   ;; closing message
   ((string-match "^ERROR\\s-+:Closing\\s-+Link" string)
    (erc-update-mode-line)
    nil)

   ;; ping messages
   ((string-match "^\\s-*PING\\s-+:\\(\\S-+\\)" string)
    (let ((pinger (match-string 1 string)))
      (erc-log (format "PING: %s" pinger))
      (erc-send-command (format "PONG :%s" pinger))
      nil))

   ;; pong messages
   ((string-match
     (concat "^:"
	     erc-session-server
	     "\\s-+PONG\\s-+\\(\\S-+\\)\\s-+:\\s-*\\(\\S-+\\)")
     string)
    nil)

   ;; server NOTICE messages
;;   ((string-match (concat "^\\s-*:" erc-session-server "\\s-+NOTICE\\s-+"
;;			  (erc-current-nick) "\\s-*:\\s-*") string)
   ((or (string-match (concat "^\\s-*:" erc-session-server "\\s-+NOTICE\\s-+"
			      (erc-current-nick) "\\s-*:\\s-*") string)
	(string-match "^\\s-*NOTICE\\s-+\\S-+\\s-*:\\s-*" string))
    
    (erc-highlight-notice
     (format "%s%s" erc-notice-prefix (substring string (match-end 0)))))
   
   ;; PRIVMSG's and CTCP messages
   ((string-match
     "^:\\(\\S-+\\)\\s-+\\(PRIVMSG\\|NOTICE\\)\\s-+\\(\\S-+\\)\\s-+:[ \t]*\\(.*\\)$" string)
    (let* ((sspec (match-string 1 string))
	   (cmd (match-string 2 string))
	   (tgt (match-string 3 string))
	   (msg (match-string 4 string))
	   (sndr (erc-parse-user sspec))
	   (nick (nth 0 sndr))
	   (login (nth 1 sndr))
	   (host (nth 2 sndr))
	   (msgp (string= cmd "PRIVMSG"))
	   (privp (string= (downcase tgt) (erc-current-nick)))
	   (mark-s (if msgp (if privp ?* ?<) ?-))
	   (mark-e (if msgp (if privp ?* ?>) ?-))
	   (s nil))
      (erc-log (format "MSG: %s TO %s FROM %S <%s>" cmd tgt sndr msg))
      (cond
       ((string-match "^\C-a\\(.*\\)\C-a$" msg)
	(setq s
	      (if (string= cmd "PRIVMSG")
		  (erc-process-ctcp-request nick (match-string 1 msg))
		(erc-process-ctcp-response nick (match-string 1 msg)))))
       (t
	(setcar last-peers nick)
	(cond

	 ;; private message/notice
	 (privp
	  (setq s
		(if away
		    (format "%c%s%c <%s>  %s"
			    mark-s nick mark-e
			    (format-time-string "%a %b %d %H:%M"
						(current-time))
			    msg)
		  (format "%c%s%c %s" mark-s nick mark-e msg)))
	  (put-text-property 0 (length s) 'face 'erc-direct-msg-face s)
	  (if (not (get-buffer-window (process-buffer proc)))
	      (message "IRC message from %s: %s" nick msg)))

	 ;; channel message/notice
	 (t
	  (setq s (format "%c%s%c %s" mark-s nick mark-e msg))
	  (put-text-property 0 (length s) 'face 'erc-default-face s)))

	(erc-log (format "DEBUG: apres cond: s: %s" s))

	;; mark the pal
	(if (erc-pal-p nick)
	    (cond
	     ((equal erc-pal-highlight-type 'nick)
	      (put-text-property 0 (+ 2 (length nick)) 'face 'erc-pal-face s))
	     ((equal erc-pal-highlight-type 'all)
	      (put-text-property 0 (length s) 'face 'erc-pal-face s))
	     (t nil)))))
      s))

   ;; MODE messages
   ((string-match
     "^:\\(\\S-+\\)\\s-+MODE\\s-+\\(\\S-+\\)\\s-+\\(.*\\)\\s-*$"
     string)
    (let* ((sspec (match-string 1 string))
	   (tgt (match-string 2 string))
	   (mode (match-string 3 string))
	   (sndr (erc-parse-user sspec))
	   (nick (nth 0 sndr))
	   (login (nth 1 sndr))
	   (host (nth 2 sndr)))
      (erc-log (format "MODE: %s -> %s: %s" nick tgt mode))
      ;; dirty hack
      (if (or (string= login "") (string= host ""))
	  (erc-highlight-notice
	   (format "%s%s has changed mode for %s to %s"
		   erc-notice-prefix nick tgt mode))
	(erc-highlight-notice
	 (format "%s%s (%s@%s) has changed mode for %s to %s"
		 erc-notice-prefix nick login host tgt mode)))))

   ;; NICK messages
   ((string-match
     "^:\\(\\S-+\\)\\s-+NICK\\s-+:\\s-*\\(.*\\)\\s-*$" string)
    (let* ((sspec (match-string 1 string))
	   (nn (match-string 2 string))
	   (sndr (erc-parse-user sspec))
	   (nick (nth 0 sndr))
	   (login (nth 1 sndr))
	   (host (nth 2 sndr)))
      (erc-log (format "NICK: %s -> %s" nick nn))
      (cond
       ((string= nick (erc-current-nick))
	(erc-push-nick nn)
	(erc-update-mode-line)
	(erc-highlight-notice
	 (format "%sYour new nickname is %s" erc-notice-prefix nn)))
       (t
	(erc-handle-user-status-change 'nick
				       (list nick login host)
				       (list nn))
	(erc-highlight-notice
	 (format "%s%s (%s@%s) has renamed himself to %s"
		 erc-notice-prefix nick login host nn))))))

   ;; TOPIC messages
   ((string-match
     "^:\\(\\S-+\\)\\s-+TOPIC\\s-+\\(\\S-+\\)\\s-+:\\s-*\\(.*\\)\\s-*$" string)
    (let* ((sspec (match-string 1 string))
	   (ch (match-string 2 string))
	   (topic (match-string 3 string))
	   (sndr (erc-parse-user sspec))
	   (nick (nth 0 sndr))
	   (login (nth 1 sndr))
	   (host (nth 2 sndr)))
      (erc-log (format "TOPIC: %s for %s -> %s" nick ch topic))
      (erc-highlight-notice
       (format "%s%s (%s@%s) has set the topic for %s: %s"
	       erc-notice-prefix nick login host ch topic))))

   ;; KICK messages
   ((string-match
     "^:\\(\\S-+\\)\\s-+KICK\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+:\\s-*\\(.*\\)\\s-*$" string)
    (let* ((sspec (match-string 1 string))
	   (ch (match-string 2 string))
	   (tgt (match-string 3 string))
	   (reason (match-string 4 string))
	   (sndr (erc-parse-user sspec))
	   (nick (nth 0 sndr))
	   (login (nth 1 sndr))
	   (host (nth 2 sndr)))
      (erc-log (format "KICK: %s by %s: %s" tgt nick reason))
      (erc-highlight-notice
       (cond
	((string= tgt (erc-current-nick))
	 (format "%sYou have been kicked by %s off channel %s: %s"
		 erc-notice-prefix nick ch reason))
	((string= nick (erc-current-nick))
	 (format "%sYou have kicked %s off channel %s: %s"
		 erc-notice-prefix tgt ch reason))
	(t
	 (format "%s%s (%s@%s) has kicked %s off channel %s: %s"
		 erc-notice-prefix nick login host tgt ch reason))))))

   ;; INVITE messages
   ((string-match
     (concat "^:\\(\\S-+\\)\\s-+INVITE\\s-+"
	     (erc-current-nick)
	     "\\s-+:\\s-*\\(.*\\)\\s-*$")
     string)
    (let* ((sspec (match-string 1 string))
	   (chnl (match-string 2 string))
	   (sndr (erc-parse-user sspec))
	   (nick (nth 0 sndr))
	   (login (nth 1 sndr))
	   (host (nth 2 sndr)))
      (erc-log (format "INVITE: by %s to %s" sspec chnl))
      (setq invitation chnl)
      (erc-highlight-notice
       (format "%s%s (%s@%s) invites you to channel %s"
	       erc-notice-prefix nick login host chnl))))

   ;; JOIN messages
   ((string-match "^:\\(\\S-+\\)\\s-+JOIN\\s-+:\\s-*\\(\\S-+\\)"
		  string)
    (let* ((sspec (match-string 1 string))
	   (chnl (match-string 2 string))
	   (sndr (erc-parse-user sspec))
	   (nick (nth 0 sndr))
	   (login (nth 1 sndr))
	   (host (nth 2 sndr)))
      (erc-log (format "JOIN: %S" sndr))
      ;; strip the stupid combined JOIN facility (IRC 2.9)
      (if (string-match "^\\(.*\\)?\^g.*$" chnl)
	  (setq chnl (match-string 1 chnl)))
      (cond
       ((string= nick (erc-current-nick))
	(erc-add-default-channel chnl)
	(erc-update-mode-line)
	(erc-highlight-notice
	 (format "%sYou have joined channel %s" erc-notice-prefix chnl)))
       (t
	(erc-highlight-notice
	 (format "%s%s (%s@%s) has joined channel %s"
		 erc-notice-prefix nick login host chnl))))))

   ;; PART messages
   ((string-match
     "^:\\(\\S-+\\)\\s-+PART\\s-+\\(\\S-+\\)\\s-*\\(:\\s-*\\(.*\\)\\)?$"
     string)
    (let* ((sspec (match-string 1 string))
	   (chnl (match-string 2 string))
	   (reason (match-string 4 string))
	   (sndr (erc-parse-user sspec))
	   (nick (nth 0 sndr))
	   (login (nth 1 sndr))
	   (host (nth 2 sndr)))
      (erc-log (format "PART: %s by %s: %s" chnl sspec reason))
      (cond
       ((string= nick (erc-current-nick))
	(erc-delete-default-channel chnl)
	(erc-update-mode-line)
	(erc-highlight-notice
	 (format "%sYou have left channel %s" erc-notice-prefix chnl)))
       (t
	(erc-highlight-notice
	 (format "%s%s (%s@%s) has left channel %s%s"
		 erc-notice-prefix nick login host chnl
		 (if (and reason (not (string= reason "")))
		     (concat ": " reason)
		   "")))))))

   ;; QUIT messages
   ((string-match "^:\\(\\S-+\\)\\s-+QUIT\\s-+:\\s-*\\(.*\\)$"
		  string)
    (let* ((sspec (match-string 1 string))
	   (reason (match-string 2 string))
	   (sndr (erc-parse-user sspec))
	   (nick (nth 0 sndr))
	   (login (nth 1 sndr))
	   (host (nth 2 sndr)))
      (erc-highlight-notice
       (format "%s%s (%s@%s) has quit: %s" erc-notice-prefix nick login host reason))))

   ;; other
   (t string)))


(defun erc-process-num-reply (n s)
  "Process numeric reply from server. N is a numeric code,
S is a trailing message"
  
  (erc-log (format "process-num-reply: [%d] %s" n s))
  (cond

   ;; endings to ignore
   ((or (= n 315)
	(= n 318)
	(= n 365)
	(= n 366)
	(= n 369)
	(= n 374)
	(= n 376))
    nil)

   ;; error replies
   
   ;; login-time 'nickname in use' message
   ((and (= n 433)
	 (string-match "^\\(\\S-+\\)\\s-*:" s))
    (let* ((nick (match-string 1 s)))
      (setq bad-nick t)
      (erc-highlight-error (format "%sNickname \'%s\' is already in use"
				   erc-notice-prefix nick))))

   ;; other error replies
   ((and (>= n 400)
	(< n 500))
    (erc-highlight-error (concat erc-notice-prefix s)))

   ;; "good" numeric replies

   ;; MOTD, INFO and other weird messages
   ((and
     (or (= n 371) (= n 372) (= n 375) (= n 376)
	 (and (>= n 1) (<= n 4))
	 (and (>= n 251) (<= n 255)))
     (string-match "^:\\s-*\\(.*\\)$" s))
    (let ((msg (match-string 1 s)))
      (erc-log (format "MOTD/%d: %s" n msg))
      (erc-highlight-notice (format "%s%s" erc-notice-prefix msg))))

   ;; LUSEROP, LUSERUNKNOWN, LUSERCHANNELS
   ((and (>= n 252) (<= n 254)
	 (string-match "^\\s-*\\([0-9]+\\)\\s-*:\\s-?\\(.*\\)$" s))
    (let ((nops (match-string 1 s))
	  (msg (match-string 2 s)))
      (erc-highlight-notice
       (format "%s%s %s" erc-notice-prefix nops msg))))
   
   ;; ADMIN messages
   ((and (>= n 256) (<= n 259)
	 (string-match "^:\\s-*\\(.*\\)\\s-*$" s))
    (let ((msg (match-string 1 s)))
      (erc-highlight-notice (concat erc-notice-prefix msg))))
   
   ;; AWAY notice
   ((and (= n 301)
	 (string-match "^\\(\\S-+\\)\\s-+:\\s-*\\(.*\\)$" s))
    (let ((nick (match-string 1 s))
	  (msg (match-string 2 s)))
      (erc-highlight-notice
       (format "%s%s is AWAY: %s" erc-notice-prefix nick msg))))

   ;; AWAY messages
   ((and
     (or (= n 305) (= n 306))
     (string-match "^:\\s-*\\(.*\\)\\s-*$" s))
    (erc-process-away (= n 306))
    (let ((msg (match-string 1 s)))
      (erc-highlight-notice (concat erc-notice-prefix msg))))
   
   ;; WHOIS/WAS notices
   ((and (or (= n 311) (= n 314))
	 (string-match
	  "^\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\*\\s-+:\\s-*\\(.*\\)$"
	  s))
    (let ((nick (match-string 1 s))
	  (user (match-string 2 s))
	  (host (match-string 3 s))
	  (fname (match-string 4 s)))
      (erc-highlight-notice
       (format "%s%s %s %s (%s@%s)"
	       erc-notice-prefix nick
	       (if (= n 311)
		   "is"
		 "was")
	       fname user host))))

   ;; WHOISOPERATOR
   ((and (>= n 313)
	 (string-match "^\\s-*\\(\\S-+\\)\\s-*:\\s-?\\(.*\\)$" s))
    (let ((nick (match-string 1 s))
	  (msg (match-string 2 s)))
      (erc-highlight-notice
       (format "%s%s %s" erc-notice-prefix nick msg))))
   
   ;; IDLE notice
   ((and (= n 317)
	 (string-match
	  "^\\(\\S-+\\)\\s-+\\([0-9]+\\)\\s-*:\\s-*seconds idle.*$"
	  s))
    (let* ((nick (match-string 1 s))
	  (nsec (string-to-number (match-string 2 s))))
      (erc-highlight-notice 
       (format "%s%s is idle for %s"
	       erc-notice-prefix
	       nick
	       (erc-sec-to-time nsec)))))

   ;; server notice
   ((and (= n 312)
	 (string-match
	  "^\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+:\\s-*\\(.*\\)\\s-*$" s))
    (let* ((nick (match-string 1 s))
	   (saddr (match-string 2 s))
	   (srvr (match-string 3 s)))
      (erc-highlight-notice
       (format "%s%s is/was on server %s (%s)"
	       erc-notice-prefix nick saddr srvr))))

   ;; channels notice
   ((and (= n 319)
	 (string-match "^\\(\\S-+\\)\\s-+:\\s-*\\(.*\\)\\s-*$" s))
    (let* ((nick (match-string 1 s))
	   (chnls (match-string 2 s)))
      (erc-highlight-notice
       (format "%s%s is on channel(s): %s" erc-notice-prefix nick chnls))))

   ;; LIST header
   ((= n 321)
    (erc-highlight-notice (format "%s%s" erc-notice-prefix s)))

   ;; LIST notice
   ((and (= n 322)
	 (string-match
	  "^\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+:\\s-*\\(.*\\)\\s-*$" s))
    (let* ((chnl (match-string 1 s))
	   (nv (match-string 2 s))
	   (topic (match-string 3 s)))
      (erc-highlight-notice
       (if (string= topic "")
	   (format "%s%s [%s]" erc-notice-prefix chnl nv)
	 (format "%s%s [%s]: %s" erc-notice-prefix chnl nv topic)))))

   ;; LIST footer
   ((and (= n 323)
	 (string-match
	  "^\\s-*:\\(.*\\)$" s))
    (let* ((msg (match-string 1 s)))
      (erc-highlight-notice (format "%s%s" erc-notice-prefix msg))))

   ;; WHO notice
   ((and (= n 352)
	 (string-match
	  "^\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+:\\([0-9]+\\)\\s-+\\(.*\\)\\s-*$" s))
    (let* ((chnl (match-string 1 s))
	   (usr (match-string 2 s))
	   (hst (match-string 3 s))
	   (srv (match-string 4 s))
	   (nick (match-string 5 s))
	   (fl (match-string 6 s))
	   (hc (match-string 7 s))
	   (fn (match-string 8 s)))
      (erc-highlight-notice
       (format "%-11s %-10s %-4s %s@%s (%s)" chnl nick fl usr hst fn))))

   ;; users on the channel
   ((and (= n 353)
	 (string-match "^=\\s-+\\(\\S-+\\)\\s-+:\\s-*\\(.*\\)\\s-*$" s))
    (let ((chnl (match-string 1 s))
	  (users (match-string 2 s)))
      (erc-highlight-notice
       (format "%sUsers on %s: %s" erc-notice-prefix chnl users))))

   ;; INVITE response
   ((and (= n 341)
	 (string-match "^\\s-*\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-*$" s))
    (let ((nick (match-string 1 s))
	  (chnl (match-string 2 s)))
      (erc-highlight-notice
       (format "%s%s was invited to channel %s" erc-notice-prefix nick chnl))))

   ;; TOPIC notice
   ((and (= n 332)
	 (string-match "^\\(\\S-+\\)\\s-+:\\s-*\\(.*\\)$" s))
    (let ((chnl (match-string 1 s))
	  (topic (match-string 2 s)))
      (erc-highlight-notice
       (format "%s%s topic: %s" erc-notice-prefix chnl topic))))

   ;; channel modes
   ((and (= n 324)
	 (string-match "^\\(\\S-+\\)\\s-+\\(.*\\)$" s))
    (let ((chnl (match-string 1 s))
	  (modes (match-string 2 s))
	  (s nil))
      (setq s (format "%s%s modes: %s" erc-notice-prefix chnl modes))
      (erc-highlight-notice s)))

   ;; links
   ((and (= n 364)
	 (string-match
	  "^\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-+:\\s-*\\([0-9]+\\)\\s-+\\(.*\\)\\s-*$" s))
    (let ((mask (match-string 1 s))
	   (svr (match-string 2 s))
	   (hc (match-string 3 s))
	   (sinfo (match-string 4 s)))
      (erc-highlight-notice
       (format "%s%-20s %-20s %2s %s" erc-notice-prefix mask svr hc sinfo))))

   ;; time
   ((and (= n 391)
	 (string-match "^\\(\\S-+\\)\\s-*:\\s-*\\(.*\\)$" s))
    (let ((svr (match-string 1 s))
	  (tm (match-string 2 s)))
      (erc-highlight-notice (format "%s%s: %s" erc-notice-prefix svr tm))))

   ;; other
   (t
    (erc-highlight-notice (format "%s[%d] %s" erc-notice-prefix n s)))))


(defun erc-process-ctcp-request (sndr msg)
  "Process incoming CTCP request. SNDR is sender's nickname. MSG is
the message contents. The function returns a string to be displayed or NIL"
  
  (erc-log (format "process-ctcp-request: [%s] %s" sndr msg))
  (if erc-paranoid
      (erc-display-line
       (erc-highlight-error
	(format "==> CTCP request from %s: %s\n" sndr msg))))
      
  (cond

   ;; ACTION
   ((string-match "^ACTION\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (match-string 1 msg)))
      (put-text-property 0 (length s) 'face 'erc-action-face s)
      (concat sndr "> " s)))

   ;; CLIENTINFO
   ((string-match "^CLIENTINFO\\(\\s-*\\|\\s-+.*\\)$" msg)
    (let ((s (erc-client-info (erc-trim-string (match-string 1 msg)))))
      (erc-send-ctcp-notice sndr (format "CLIENTINFO %s" s)))
    nil)

   ;; ECHO
   ((string-match "^ECHO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (match-string 1 msg)))
      (erc-send-ctcp-notice sndr (format "ECHO %s" s))
      (put-text-property 0 (length s) 'face 'erc-action-face s)
      (concat sndr " [ECHO]> " s)))

   ;; FINGER
   ((string= "FINGER" msg)
    (let ((s (format "FINGER %s (%s@%s)."
		     (user-full-name)
		     (user-login-name)
		     (system-name)))
	  (ns (erc-time-diff last-sent-time (erc-current-time))))
      (if (> ns 0)
	  (setq s (concat s " Idle for " (erc-sec-to-time ns))))
      (erc-send-ctcp-notice sndr s))
    nil)

   ;; PING
   ((string-match "^PING\\s-+\\([0-9]+\\)" msg)
    (erc-send-ctcp-notice sndr (format "PING %s" (match-string 1 msg)))
    nil)

   ;; TIME
   ((string= msg "TIME")
    (erc-send-ctcp-notice sndr (format "TIME %s" (current-time-string)))
    nil)

   ;; USERINFO
   ((string= msg "USERINFO")
    (erc-send-ctcp-notice sndr (format "USERINFO %s" erc-user-information))
    nil)

   ;; VERSION request
   ((string= "VERSION" msg)
    (erc-send-ctcp-notice
     sndr
     (format "VERSION \C-bERC\C-b v%s - an IRC client for emacs (\C-b%s\C-b)"
	     erc-version-string
	     erc-official-location))
    nil)

   (t
    (erc-highlight-notice
     (format "%sUnknown CTCP request from %s: %s"
	     erc-notice-prefix sndr msg)))))
   

(defun erc-process-ctcp-response (sndr msg)
  "Process incoming CTCP response. SNDR is sender's nickname. MSG is
the message contents"
  
  (erc-log (format "process-ctcp-response: [%s] %s" sndr msg))
  (cond

   ;; ECHO
   ((string-match "^ECHO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [ECHO]> %s" nick (match-string 1 msg))))
      (put-text-property 0 (length s) 'face 'erc-action-face s)
      s))
   
   ;; CLIENTINFO
   ((string-match "^CLIENTINFO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [CLIENTINFO]> %s" nick (match-string 1 msg))))
      (put-text-property 0 (length s) 'face 'erc-action-face s)
      s))
   
   ;; FINGER
   ((string-match "^FINGER\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [FINGER]> %s" nick (match-string 1 msg))))
      (put-text-property 0 (length s) 'face 'erc-action-face s)
      s))
   
   ;; PING
   ((string-match "^PING\\s-+\\([0-9]+\\)" msg)
    (let* ((pt (string-to-int (match-string 1 msg)))
	   (ct (erc-current-time))
	   (ns (erc-time-diff pt ct))
	   (pair (assoc pt pings)))
      (cond
       (pair
	(setq pings (delete pair pings))
	(erc-highlight-notice
	 (format "%sPing time to %s is %s"
		 erc-notice-prefix
		 (cdr pair)
		 (erc-sec-to-time ns))))
       (t
	(erc-highlight-error
	 (format "Unexpected PING response from %s (time %s)" nick pt))))))

   ;; TIME
   ((string-match "^TIME\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [TIME]> %s" nick (match-string 1 msg))))
      (put-text-property 0 (length s) 'face 'erc-action-face s)
      s))
   
   ;; VERSION response
   ((string-match "^VERSION\\s-+\\(.*\\)\\s-*$" msg)
    (let ((s (format "%s [VERSION]> %s" nick (match-string 1 msg))))
      (put-text-property 0 (length s) 'face 'erc-action-face s)
      s))
   
   (t
    (erc-highlight-notice
     (format "%sUnknown CTCP message from %s: %s"
	     erc-notice-prefix nick msg)))))


(defun erc-process-away (away-p)
  "Do additional processing of user going away (if AWAY-P is non-nil),
or coming back"
  
  (cond
   (away-p
    (setq away (current-time)))

   (t
    (let ((away-time away))
      ;; away must be set to NIL BEFORE sending anything to prevent
      ;; an infinite recursion
      (setq away nil)
      (erc-send-action
       (erc-default-target)
       (if away-time
	   (format "is back (gone for %s)"
		   (erc-sec-to-time (erc-time-diff
				     (erc-emacs-time-to-erc-time away-time)
				     (erc-current-time))))
	 "is back")))))
  (erc-update-mode-line))


(defun erc-handle-user-status-change (typ nlh &optional l)
  "Handle changes in any user's status. So far, only nick change is handled.

Generally, the TYP argument is a symbol describing the change type, NLH is
a list containing the original nickname, login name and hostname for the user,
and L is a list containing additional TYP-specific arguments.

So far the following TYP/L pairs are supported:

       event                    TYP                    L

    nickname change            'nick                (NEW-NICK)

"
  (erc-log (format "user-change: type: %S  nlh: %S  l: %S" typ nlh l))
  (cond
   ;; nickname change
   ((equal typ 'nick)
    t)

   (t
    nil)))


(defun erc-highlight-notice (s)
  "Highlight notice message S and return it

See also variable `erc-notice-highlight-type'"

  (cond
   ((equal erc-notice-highlight-type 'prefix)
    (put-text-property 0 (length erc-notice-prefix) 'face 'erc-notice-face s)
    s)

   ((equal erc-notice-highlight-type 'all)
    (put-text-property 0 (length s) 'face 'erc-notice-face s)
    s)

   (t s)))


(defun erc-highlight-error (s)
  "Highlight error message S and return it"

  (put-text-property 0 (length s) 'face 'erc-error-face s)
  s)


(defun erc-parse-user (string)
  "Parse IRC-type user specification (nick!login@host) to three separate
tokens and return them as a list"

  (erc-log (concat "parse-user: input: " string))
  (if (string-match "\\([^!]*\\)!\\([^@]*\\)@\\(.*\\)" string)
     (list (match-string 1 string)
	   (match-string 2 string)
	   (match-string 3 string))
    (list string "" "")))


(defun erc-parse-current-line ()
  "parse current input line

Returns a pair (PART1 . PART2), where PART1 is the input part before the point
and PART2 is the part after the point"

  (save-excursion
    (let* ((p1 (point))
	   (p0 (progn (beginning-of-line) (point)))
	   (p2 (progn (end-of-line) (point)))
	   (l0 (buffer-substring-no-properties p0 p2))
	   (l1 (buffer-substring-no-properties p0 p1))
	   (l2 (buffer-substring-no-properties p1 p2)))
;;      (erc-log (format "parse-line: l0: %S  l1: %S  l2: %S\n" l0 l1 l2))
      (cond ((string-match (concat "^" erc-prompt "[ \t]*") l0)
	     (let ((i1 (match-end 0)))
	       (if (>= i1 (length l1))
		   (cons nil (substring l0 i1))
		   (cons (substring l1 i1) l2))))
	    (t (cons l1 l2))))))


(defun erc-split-multiline (string)
  "Split STRING, containing multiple lines and return them in a list"

  (interactive "")
  (let ((l ())
	(i0 0)
	(doit t))
    (while doit
      (let ((i (string-match "\r?\n" string i0))
	    (s (substring string i0)))
	(cond (i
	       (setq l
		     (cons
		      (concat prev-rd (substring string i0 i))
		      l))
	       (setq prev-rd "")
	       (setq i0 (match-end 0)))

	      ((> (length s) 0)
	       (setq prev-rd (concat prev-rd s))
	       (setq doit nil))

	      (t (setq doit nil)))))
    (reverse l)))


;; command history

(defun erc-previous-command ()
  "Replace current command with the previous one from the history"

  (beep)
  (message "Not implemented yet"))


(defun erc-next-command ()
  "Replace current command with the next one from the history"

  (beep)
  (message "Not implemented yet"))


;; nick handling

(defun erc-push-nick (nick)
  "Push new nickname to a nickname stack"

  (setq nick-stk (cons nick nick-stk)))


(defun erc-pop-nick ()
  "Remove topmost nickname from a stack"

  (if (null nick-stk)
      (error "Nickname stack empty")
    (setq nick-stk (cdr nick-stk))))


(defun erc-current-nick ()
  "Return current nickname (top of the nickname stack)"

  (if (null nick-stk)
      nil
    (car nick-stk)))


;; default target handling

(defun erc-default-target ()
  "Returns current default target (as a character string) or NIL if none."

  (let ((tgt (car def-rcpts)))
    (cond
     ((not tgt) nil)
     ((listp tgt) (cdr tgt))
     (t tgt))))


(defun erc-add-default-channel (ch)
  "Add channel to the default channel list. If the current default
recepient is of QUERY type, then push the new default channel *after*
the head"

  (let ((d1 (car def-rcpts))
	(d2 (cdr def-rcpts))
	(chl (downcase ch)))
    (if (and (listp d1)
	     (equal (car d1) 'QUERY))
	(setq def-rcpts
	      (cons d1 (cons chl d2)))
      (setq def-rcpts
	    (cons chl def-rcpts)))))


(defun erc-delete-default-channel (ch)
  "Delete channel from the default channel list."

  (setq def-rcpts (delete (downcase ch) def-rcpts)))


(defun erc-add-query (nick)
  "Add QUERY'd nickname to the default channel list. The previous
default target of QUERY type gets removed"

  (let ((d1 (car def-rcpts))
	(d2 (cdr def-rcpts))
	(qt (cons 'QUERY (downcase nick))))
    (if (and (listp d1)
	     (equal (car d1) 'QUERY))
	(setq def-rcpts (cons qt d2))
      (setq def-rcpts (cons qt def-rcpts)))))


(defun erc-delete-query ()
  "Delete the topmost target if it is a QUERY"

  (let ((d1 (car def-rcpts))
	(d2 (cdr def-rcpts)))
    (if (and (listp d1)
	     (equal (car d1) 'QUERY))
	(setq def-rcpts d2)
      (error "Current target is not a QUERY"))))


;; pal stuff

(defun erc-pal-p (nick)
  "check whether NICK is in the pals list"

  (member (downcase nick) erc-pals))


(defun erc-add-pal ()
  "Add pal interactively"

  (interactive "")
  (let ((pal (downcase (read-from-minibuffer "Pal\'s nickname: "))))
    (if (erc-pal-p pal)
	(error (format "pal \"%s\" already on the list" pal))
      (setq erc-pals (cons pal erc-pals)))))


(defun erc-delete-pal ()
  "Delete pal interactively"

  (interactive "")
  (let ((pal (downcase (read-from-minibuffer "Pal\'s nickname: "))))
    (if (not (erc-pal-p pal))
	(error (format "pal \"%s\" is not on the list" pal))
      (setq erc-pals (delete pal erc-pals)))))


;; script execution and startup

(defun erc-select-startup-file ()
  "Select startup file with a script to execute. See also
the variable `erc-startup-file-list'"

  (let ((l erc-startup-file-list)
	(f nil))
    (while (and (not f) l)
      (if (file-readable-p (car l))
	  (setq f (car l)))
      (setq l (cdr l)))
    f))


(defun erc-load-script (file)
  "Load a script from FILE. If the filename ends with `.el', then load it
as a emacs-lisp program. Otherwise, trieat it as a regular IRC script"

  (erc-log (concat "erc-load-script: " file))
  (cond
   ((string-match "\\.el$" file)
    (load file))

   (t
    (erc-load-irc-script file))))


(defun erc-load-irc-script (file)
  "Load IRC script from FILE"

  (erc-log (concat "erc-load-script: " file))
  (let ((cb (current-buffer))
	(b (find-file-read-only file))
	(s ""))
    (set-buffer cb)
    (while s
      (erc-log (concat "erc-load-script: CMD: " s))
      (if (not (string-match "^\\s-*$" s))
	  (erc-process-input-line s))
      (setq s (erc-get-command-from-buffer b)))
    (kill-buffer b)))


(defun erc-get-command-from-buffer (b)
  "Read the current line from buffer B, move the point down
and return the line"

  ;; There may be a possible problem with a non-saved excursion
  ;; but if we do save it, we won't be able to remember the new
  ;; position in the command buffer

  (let ((cb (current-buffer))
	(s nil))
    (set-buffer b)
    (cond
     ((not (equal (point) (point-max)))
      (setq s (buffer-substring-no-properties
	       (progn (beginning-of-line) (point))
	       (progn (end-of-line) (point))))
      (beginning-of-line 2)))
    (set-buffer cb)
    s))


;; authentication

(defun erc-login ()
  "Perform user authentication at the IRC server"
  
  (erc-log (format "login: nick: %s, user: %s %s %s :%s"
		   (erc-current-nick)
		   (user-login-name)
		   (system-name)
		   erc-session-server
		   erc-session-user-full-name))
  
  (if erc-password
      (erc-send-command (format "PASS %s" erc-password)))
  (erc-send-command (format "NICK %s" (erc-current-nick)))
  (erc-send-command
   (format "USER %s %s %s :%s"
	   (user-login-name)
	   (system-name)
	   erc-session-server
	   erc-session-user-full-name)))


;; connection properties' heuristics

(defun erc-determine-parameters (&optional server port nick name)
  "Determine the connection and authentication parameters and
sets the buffer local variables:

      erc-session-server
      erc-session-port
      nick-stk
      erc-session-full-name"

  (setq erc-session-server (erc-compute-server server)
	erc-session-port (or port erc-default-port)
	erc-session-user-full-name (erc-compute-full-name name))

  (erc-push-nick (erc-compute-nick nick)))

  
(defun erc-compute-server (server)
  "return the IRC server to use using the following order until a non-NIL
one is found:

- argument
- erc-server value
- value of IRCSERVER environment variable
- erc-default-server value"
  
  (or server
      erc-server
      (getenv "IRCSERVER")
      erc-default-server))


(defun erc-compute-nick (nick)
  "return the user's nick using the following order until a non-NIL
one is found:

- argument
- erc-nick value
- value of IRCNICK environment variable
- user's login name"
  
  (or nick
      erc-nick
      (getenv "IRCNICK")
      (user-login-name)))


(defun erc-compute-full-name (name)
  "return the user's full name using the following order until a non-NIL
one is found:

- argument
- erc-user-full-name value
- value of IRCNAME environment variable
- user's full name from the system databases"
  
  (or name
      erc-user-full-name
      (getenv "IRCNAME")
      (user-full-name)))


;; time routines

(defun erc-emacs-time-to-erc-time (tm)
  "Convert Emacs time to a number of seconds since the epoch"

  (round (+ (* (nth 0 tm) 65536) (nth 1 tm))))


(defun erc-current-time ()
  "Return current time as a number of seconds since the epoch"

  (erc-emacs-time-to-erc-time (current-time)))


(defun erc-time-diff (t1 t2)
  "Return time difference in seconds between T1 and T2 (T2 >= T1)"

  (- t2 t1))


(defun erc-time-gt (t1 t2)
  "Check whether T1 > T2"

  (> t1 t2))


(defun erc-sec-to-time (ns)
  "Convert seconds to a time string HH:MM.SS"

  (format "%02d:%02d.%02d"
	  (/ ns 3600)
	  (/ (% ns 3600) 60)
	  (% ns 60)))


;; info

(defconst erc-clientinfo-alist
  '(("ACTION" . "is used to inform about one's current activity")
    ("CLIENTINFO" . "gives help on CTCP commands supported by client")
    ("ECHO" . "echoes its arguments back")
    ("FINGER" . "shows user's name, location, and idle time")
    ("PING" . "measures delay between peers")
    ("TIME" . "shows client-side time")
    ("USERINFO" . "shows information provided by a user")
    ("VERSION" . "shows client type and version"))
  
  "Alist of CTCP CLIENTINFO for ERC commands")


(defun erc-client-info (s)
  "Return CTCP CLIENTINFO on command S. Is S is NIL or an empty string
then return general CLIENTINFO"

  (if (or (not s) (string= s ""))
      (concat
       (apply #'concat
	      (mapcar (lambda (e)
			(concat (car e) " "))
		      erc-clientinfo-alist))
       ": use CLIENTINFO <COMMAND> to get more specific information")
    (let ((h (assoc s erc-clientinfo-alist)))
      (if h
	  (concat s " " (cdr h))
	(concat s ": unknown command")))))


;; miscellaneous

(defun erc-update-mode-line ()
  "Update the mode line in the ERC buffer"

;;  (erc-log (format "MODE LINE: proc: %S" (process-status proc)))
  (let ((nick (erc-current-nick))
	(tgt (or (erc-default-target) "0")))
    (setq mode-line-format
	  (list "-----ERC: "
		erc-session-server "/" (erc-port-to-string erc-session-port)
		(cond
		 ((not (equal (process-status proc) 'open))
		  "  (CLOSED)  ")
		 (away
		  (concat "  (AWAY since "
			  (format-time-string "%a %b %d %H:%M" away)
			  ")  "))
		 (t
		  (concat "  (" nick " -> " tgt	")  ")))
	      global-mode-string
	      "   %[(" 
	      mode-name 
	      mode-line-process 
	      minor-mode-alist 
	      "%n" ")%]--" 
	      (line-number-mode "L%l--") 
	      '(-3 . "%p") 
	      "-%-"))))


(defun erc-port-to-string (p)
  "Convert port P to string. P may be an integer or a service name"

  (if (integerp p)
      (int-to-string p)
    p))


(defun erc-string-to-port (s)
  "Convert string S to either integer port number or a service name"

  (let ((n (string-to-number s)))
    (if (= n 0)
	s
      n)))


(defun erc-version ()
  "display ERC version"

  (interactive "")
  (message "ERC version %s" erc-version-string))


(defun erc-trim-string (s)
  "Trim leading and trailing spaces off the string"

  (cond
   ((string-match "^\\s-*$" s)
    "")
   ((string-match "^\\s-*\\(.*\\S-\\)\\s-*$" s)
    (match-string 1 s))
   (t
    s)))


(provide 'erc)

;; end of $Source: /home/abel/cvs/src/misc/emacs/erc/erc.el,v $
