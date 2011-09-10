;;; BSD compatible TALK for emacs 18/19

;;;
;;; Copyright (C) 1994 Free Software Foundation
;;;
;;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;;; Version: 0.8
;;; Keywords: talk, comm, games
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;

;;; Commentary: 
;;;   etalk is an emulation of the standard BSD talk program, which is
;;;   compatible with the BSD talk deamon, and the sun talk deamon.
;;;   The lisp sources support user visible protocols, and the C
;;;   binary supports machine compatibility problems.
;;;
;;;   General supported features are: Multiple connections, multiple
;;;   entry points with different connection methods, several
;;;   communication features (for extended editing), game playing, and
;;;   a large library of games, with emacs19 X support.
;;;
;;;   The major entry point is the command "etalk" which querys for a
;;;   use to talk to.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Q&D instructions:  Load this file, type "M-x etalk RET SPC"
;;;                    to get a list of users.  Type/complete one,
;;;                    and hit RET.  Type to them.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change from EMACS TALK 0.6B to ETALK
;;;   For those familiar with TALK 0.*B versions of this program, the
;;; name change from TALK to ETALK represents a distinct change in
;;; approch to handling this program.  The changes are:
;;;   O New binary written in an more object oriented approach.
;;;   O No TTY ioctrl to hamper portability.
;;;   O One etalk process for all connections streamline running.
;;;   O A command line buffer for status reading and the like.
;;;   O configure script for building and installing.
;;;   O Re-written info file which is hopefully organized better
;;;   O Lisp fixed to produce fewer errors with the optimizing byte compiler
;;;   O All games are in a subdirectory "games" to prevent unwanted
;;;     loading
;;;   O Lisp sources have been reorganized to supply maximum re-use
;;;     in some areas.
;;;   O ETALK version is now sent as a message to signal types, and
;;;     future backward compatability for features.  0.8 will be the
;;;     baseline for future additions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General features.
;;;   O Four connection generation entry points:
;;;     etalk, etalk-batch, etalk-mail, etalk-mail-reply.
;;;   O Minibuffer completion for usernames and host names including
;;;     parsing routines for finger version of several types,
;;;     including VMS
;;;   O Local config file containing host names and daemon types to
;;;     speed up connecting routines, and provide completion on a
;;;     system and personal basis.
;;;   O Multiple daemon support for NTALK and OTALK (sun talk)
;;;   O Multiple talk connection protocols including vanilla talk (no
;;;     features) and emacs talk (tons o features.)
;;;   O Preferred name transfer for more personable messages.
;;;   O For users with completely incompatible daemons (or no daemon
;;;     at all), etalk-mail can still let users connect over TCP.
;;;
;;;   O Infinite conversation scrollback.
;;;   O Multiple simultaneous connections (all can be started with
;;;     different methods.)
;;;   O Peer to peer connection propagation allowing user 1 to
;;;     generate connections to everyone user 2 is talking to.
;;;   O Copy/Yank from/into etalk conversation buffers.
;;;   O Buffer logging facility to always save your conversations.
;;;   O auto-wrap minor mode compatible (even with vanilla talk)
;;;   O Make a file popup in a window on both sides of a connection.
;;;   O Insert a file into the conversation (you may wish to stick to
;;;     small files. ;)
;;;   O Extended editing capabilities to go backwards over lines when
;;;     everyone uses emacs.
;;;   O Minibuffer message over connection.
;;;   O Two person games over talk connection with a wide selection of
;;;     games to choose from.
;;;   O Some game(s) can be used with tyrant-ai to be played against
;;;     the computer.
;;;   O Etalk window management commands.
;;;
;;;   O Bundled finger mode, allows browsing of finger information
;;;     which also allows you to do dired style actions to users on a
;;;     given line such as mail, more-finger-info, and etalk.
;;;   O Bundled sformat, a super-format extender I wrote to aid in
;;;     writing the multiple *-format functions through lists of
;;;     extension characters and their values and types.
;;;   O Extended X support in games allows colors to spice things up
;;;

(defvar etalk-version-number '(0 . 8)
  "The current version of etalk in a float notation.")

(defvar etalk-program-name "ETALK"
  "The name of this package.")

(defvar etalk-emacs-version emacs-version
  "Version of emacs this was loaded/byte compiled under.")

(defvar etalk-version (format "%s %d.%dB" 
			      etalk-program-name
			      (car etalk-version-number)
			      (cdr etalk-version-number))
  "Current working version of the etalk program.")

(defun etalk-version ()
  "Return a string which verbally describes this version of emacs"
  (interactive)
  (message "Emacs Talk [%s] parsed under emacs V %s"
	   etalk-version etalk-emacs-version)
)

(defconst etalk-18-p (= (string-to-int etalk-emacs-version) 18)
  "A simply way to determine version during load.")

;;; Since I have never used abbrev, I havn't the slightest notion
;;; of what to do with this now that I have one.
(defvar etalk-mode-abbrev-table nil
  "Abbreviation table")

(define-abbrev-table 'etalk-mode-abbrev-table ())

(defvar etalk-log-mode-map nil
  "Keymap used within the log buffer...")

(if etalk-log-mode-map 
    ()
  (setq etalk-log-mode-map (make-keymap))
  (suppress-keymap etalk-log-mode-map)
  (if etalk-18-p
      (fillarray etalk-log-mode-map 'etalk-send-command-key)
    (fillarray (nth 1 etalk-log-mode-map) 'etalk-send-command-key))
  (define-key etalk-log-mode-map "\C-c" nil)
  (define-key etalk-log-mode-map "\C-x" nil)
  (define-key etalk-log-mode-map "\C-f" nil)
  (define-key etalk-log-mode-map "\C-n" nil)
  (define-key etalk-log-mode-map "\C-b" nil)
  (define-key etalk-log-mode-map "\C-p" nil)
  (define-key etalk-log-mode-map "\C-v" nil)
  (define-key etalk-log-mode-map "\C-g" nil)
  (define-key etalk-log-mode-map "\C-l" nil)
  (define-key etalk-log-mode-map "\C-e" nil)
  (define-key etalk-log-mode-map "\C-a" nil)
  (define-key etalk-log-mode-map "\C-s" nil)
  (define-key etalk-log-mode-map "\e" nil)
  (define-key etalk-log-mode-map "\C-c\C-c" 'etalk-kill-process)
  (define-key etalk-log-mode-map "\C-r" 'etalk-setup-windows-with-log)
  (define-key etalk-log-mode-map "\r" 'etalk-setup-windows-with-log)
  )

(defvar etalk-mode-c-map nil
  "Keymap used on C-c in talk mode.")

(if etalk-mode-c-map 
    ()
  (setq etalk-mode-c-map (make-sparse-keymap))
  (define-key etalk-mode-c-map "c" (lookup-key global-map "\C-c"))
  (define-key etalk-mode-c-map "\C-c" 'etalk-nuke-connection)
  (define-key etalk-mode-c-map "\C-h" 'etalk-hug-remote)
  (define-key etalk-mode-c-map "g"    'etalk-initiate-special-function)
  (define-key etalk-mode-c-map "m"    'etalk-send-minibuffer-message)
  (define-key etalk-mode-c-map "o"    'etalk-remote-multilist)
  (define-key etalk-mode-c-map "r"    'etalk-setup-windows-plus)
  (define-key etalk-mode-c-map "\C-r" 'etalk-setup-windows-with-log)
  )

(defvar etalk-mode-map nil 
  "Keymap used in talk mode")

(if etalk-mode-map 
    ()
  (setq etalk-mode-map (make-keymap))
  (suppress-keymap etalk-mode-map)
  (if etalk-18-p
      (fillarray etalk-mode-map 'etalk-insert-char)
    (fillarray (nth 1 etalk-mode-map) 'etalk-insert-char))
  (define-key etalk-mode-map "\C-@" 'set-mark-command)
  (define-key etalk-mode-map "\C-b" nil) ;move-back
  (define-key etalk-mode-map "\C-c"  etalk-mode-c-map)
  (define-key etalk-mode-map "\C-f" nil) ;move forward
  (define-key etalk-mode-map "\C-g" 'etalk-beep)
  (define-key etalk-mode-map "\C-h" 'etalk-brief-help) ;help facility
  (define-key etalk-mode-map "\C-k" 'etalk-delete-line)
  (define-key etalk-mode-map "\C-l" 'etalk-clear-window)
  (define-key etalk-mode-map "\C-m" 'etalk-RET)
  (define-key etalk-mode-map "\C-n" nil) ;next line
  (define-key etalk-mode-map "\C-p" nil) ;prev line
  (define-key etalk-mode-map "\C-r" 'etalk-setup-windows)
  (define-key etalk-mode-map "\C-w" 'etalk-delete-word-backwards)
  (define-key etalk-mode-map "\C-x" nil)
  ;; the following is to allow files to be inserted just as yank works.
  (define-key etalk-mode-map "\C-xi" 'etalk-insert-file)
  (define-key etalk-mode-map "\C-xI" 'etalk-insert-file-buffer)
  (define-key etalk-mode-map "\C-y" 'etalk-yank-text)
  (define-key etalk-mode-map "\C-z" 'suspend-emacs)
  (define-key etalk-mode-map "\e" nil)
  (define-key etalk-mode-map "\177" 'etalk-delete-backwards)
)

(defvar etalk-mode-local-hooks
  '(lambda ()
     (auto-fill-mode 1))
  "Default hook to set auto fill on a talk buffer through hooks...")

(defvar etalk-hangup-redo-windows t
  "*Redo the windows whenever there is a change in process status.")

(defvar etalk-process-file "etalk"
  "*This string declares where the talk program executable
lives on the system. A path name, or just file name if on your path")

(defvar etalk-log-buffer-name "*ETALK LOG*"
  "*This string represents the buffer name of the talk log buffer within
which etalk logs all process transactions.")

(defvar etalk-log-mode-name "Etalk-Log"
  "Mode name used in mode line when describing an etalk log buffer.")

(defvar etalk-local-buffer-name "%P"
  "*The name given to a local talk buffer.  Follows rules for talk-format.")

(defvar etalk-remote-buffer-name "%u@%:1m%:1t"
  "*The name given to a local talk buffer.  Follows rules for talk-format.")

(defvar etalk-remote-display-preferred-name " {%:2p}"
  "*Non-nil, display the remote's preferred name in the minibuffer
after connection.  This string is simply appended to 
etalk-remote-buffer-name, and follows the rules for talk-format.")

(defvar etalk-inhibit-startup-message nil
  "*If non-nil, don't print startup message about etalk.")

(defvar etalk-goofy-message-delay 5
  "*Initial length of time between goofy messages. 0 means turn off.")

(defvar etalk-buffer-logging-directory nil
  "*Where conversations are logged.  Nil is no logging, NAME logs files in
this directory where the file name = username.")

(defvar etalk-clear-buffer-on-call nil
  "*Clear buffers if non-nil for remote.")

(defvar etalk-announce-as (user-login-name)
  "*This string contains a name passed to the remote talk deamon as
your username.  Changing this means you announce yourself with a
different name.  Most sysadmins would probably be upset if you did
this to unknowing users.")

(defvar etalk-preferred-name (user-full-name)
  "*The user name you wish to use as your prefered name.")

(defvar etalk-C-x4t-to-global-map t
  "*Should i add talk-setup-windows to the keystroke C-x4t???")

;; Ok! here is a brave thing.  Modify the global-key-map so that the
;; keystroke C-x 4 t redoes the talk windows. ;)
(if etalk-C-x4t-to-global-map
    (define-key global-map "\C-x4t" 'etalk-setup-windows))

(defvar etalk-remote-mode-string "ETALK-remote"
  "The mode name of a buffer attached to a talk process.")

(defvar etalk-local-mode-string "ETALK"
  "Mode name of the local talk buffer.")

(defvar etalk-message-to-minibuffer t
  "Dictates whether messages from ETALK are displayed in minibuffer,
or in the talk remote buffer.")

(defvar etalk-log-all-minibuffer-messages t
  "*Dictate whether message from tcp processes are logged in the log buffer.")

(defvar etalk-remote-process-list nil
  "This list contains the group of active talk buffers
for local talk.  Input to talk window is sent to all remote windows.")

(defvar etalk-edit-characters-mine ""
  "*This string represents the characters used when sending
edit characters to remote system.  This variable must be changed if
you wish to change keybindings.")

(require 'etalk-mini)			;minibuffer support, address parse
(require 'etalk-proc)			;process control
(require 'etalk-tcp)			;tcp specific things
(require 'etalk-edit)			;editing control
(require 'etalk-spec)                   ;special things
(require 'etalk-yorn)			;yes or no queries
(if etalk-18-p 
    (require 'etalk-18)			;18 specific things
  (if (equal window-system 'x)
      (require 'etalk-x)		;x specific menuy things (19 only)
    ))

(autoload 'etalk-tyrannical-mode "etalk-tyrn"
  "tyrant mode setup for minor mode." nil)

(autoload 'etalk-mail-portnumber "etalk-mail"
  "contact that individual directly via port number" nil)

(autoload 'etalk-mail-extract-portnumber "etalk-mail"
  "Take the mail buffer, and read in parameters to create an ETALK connection."
  nil)

;; Define all local variables globally, otherwise, when we byte compile
;; we will get lots of errors about free variables.
(defvar etalk-remote-who nil
  "Local: User name of who we are talking as per remote etalk buffer")

(defvar etalk-remote-where nil
  "Local: Machine name of who we are talking as per remote etalk buffer")

(defvar etalk-remote-tty nil
  "Local:  tty of who we are talking as per remote etalk buffer")

(defvar etalk-remote-preferred-name nil
  "Local: Preferred name of who we are talking as per remote etalk buffer")

(defvar etalk-edit-chars nil
  "Local: Edit characters of who we are talking as per remote etalk buffer")

(defvar etalk-filter-message nil
  "When a message initiator (C-c) this variable is filled until the
terminator (\n) is encountered, it is then parsed.")

(defvar etalk-filter-message-type nil
  "The identifier which follows the the message initiator.  This
determines how the message is parsed")

(defvar etalk-special-request-function nil
  "Contains a function when user is waiting for a response from a
remote query")

(defvar etalk-inserted-file nil
  "When an etalk-insert-file-buffer is called, this is set to the name
of the transferred file.")

(defvar etalk-remote-type nil
  "The ETALK version of remote talk program.  NIL is vanilla talk.")

(defvar etalk-point nil
  "Marker which always points to the end of the talk buffer, unless
otherwise specified.")

(defvar etalk-tag nil
  "Set to t for any buffer which is related to etalk.  These buffers
are being tagged so etalk can delete all buffers related to itself")

(defvar etalk-remote-is-emacs nil
  "Set to T when this remote user has emacs running as well.  In local
buffer, it means that all remote connections are using emacs.  Emacs
talk buffers allow delete around line terminators.")

;; ----------------------------------------------------------------- ;;
;; Ok, now that the preliminaries are over, define the major modes.  ;;
;; This is broken down into 6 functions.                             ;;
;;                                                                   ;;
;; etalk              : find/start local talk buffer and remote.     ;;
;;  etalk-batch       : read command line arguments to get talk args ;;
;;  etalk-mail        : intiate a talk conversation using mail       ;;
;;  etalk-mail-reply  : reply to a talk-mail message to connect      ;;
;; -Modes-                                                           ;;
;; etalk-mode-local:  : setup the local talk buffer with marker.     ;;
;; etalk-mode-remote: : setup names, marker, process on remote.      ;;
;; etalk-mode:        : setup keymaps etc common to both buffers.    ;;
;; ----------------------------------------------------------------- ;;

;;;###autoload
(defun etalk (somebody-else &optional socket)
  "This function initialized the talk buffers in your emacs session.
Parameter is a talk style address like: 
   joe@some.big.computer.edu tty01 or
   some.big.computer.edu!joe tty01
This function also is subordinate to talk-mail, and talk-mail-reply
which allows connections based on passed information on socket
numbers."
  (interactive (cons (save-window-excursion
		       (etalk-blurb-buffer)
		       (etalk-read-username)) '()))

  ;; make sure its a username always at least 1 letter... right?
  (if (not (string-match "[A-Za-z]" somebody-else))
      (error "No name specified."))

  ;; check for duplicate calls local buffers
  (if (not (get-buffer (etalk-format etalk-local-buffer-name)))
      (progn
	(set-buffer (get-buffer-create (etalk-format etalk-local-buffer-name)))
	(etalk-mode-local)))
  ;; build new talk buffer, talk-mode-remote will recycle
  (set-buffer (get-buffer-create "etalk-temp"))
  (etalk-mode-remote somebody-else socket)
  (etalk-setup-windows)
  (run-hooks 'etalk-hooks))

;;;###autoload
(defun etalk-batch ()
  "Filler procedure to allow etalk to run from a single command line."
  (interactive)
  (etalk-blurb-buffer)
  (let ((ind 0)
	(tname ""))
    (while (not (equal (nth ind command-line-args) "etalk-batch"))
      (setq ind (+ ind 1)))
    (setq ind (+ ind 1))
    (if (nth ind command-line-args)
	(setq tname (concat tname (nth ind command-line-args)))
      (error "Out of arguments at key point!"))
    (setq ind (+ ind 1))
    (if (nth ind command-line-args)
	(setq tname (concat tname " " (nth ind command-line-args))))
    (etalk tname))
  (error "Canceling rest of loadup to preserve etalk windows..."))

;;;###autoload
(defun etalk-mail (somebody-else)
  "Does a call to somebody else with socket set to 0, which is
me-server mode."

  (interactive (cons (save-window-excursion
		       (etalk-blurb-buffer)
		       (etalk-read-username)) '()))
  (etalk somebody-else 0)		;start the wait...
  (let ((namestuff (etalk-parse-address somebody-else)))
    (etalk-mail-portnumber (etalk-format "%u@%m"
					 (nth 0 namestuff)
					 (nth 1 namestuff)
					 (nth 2 namestuff))))
  (run-hooks 'etalk-mail-hooks))

;;;###autoload
(defun etalk-mail-reply (somebody-else socket)
  "Reply to mail talk request and require socket parameter too!"

  (interactive (save-window-excursion
		 (let ((tmp 0))
		   (if (setq tmp (etalk-mail-extract-portnumber))
		       tmp
		     (cons (progn
			     (etalk-read-username))
			   (cons (string-to-int 
				  (read-string "Enter port number: "))
				 '()))))))
  (etalk somebody-else socket)
  (run-hooks 'etalk-mail-hooks))

(defun etalk-mode-local ()
  "Creates and sets up a local talk buffer. See etalk-mode for details."

  (etalk-mode 'local)
  (setq mode-name etalk-local-mode-string)
  (run-hooks 'etalk-mode-local-hooks))
  
(defun etalk-mode-remote (in-string &optional socket)
  "This takes a buffer and turns it into a remote talk window,
complete with TALK TCP process."

  ;; parse the command line parameter.  
  (let* ((addresslist (etalk-parse-address in-string))
	 (somebody-else (nth 0 addresslist))
	 (somewhere-else (nth 1 addresslist))
	 (sometty-else (nth 2 addresslist))
	 (tname nil))

    (if (not sometty-else) 
	(setq sometty-else ""))
    
    (setq tname (etalk-format etalk-remote-buffer-name somebody-else
			     somewhere-else sometty-else))

    ;; I don't recall what this was for, but it doesn't make sense
    ;;(if (get-buffer tname)
    ;;(progn
    ;;(kill-buffer (current-buffer))
    ;;(set-buffer (get-buffer tname)))

    (rename-buffer tname)

    (if etalk-clear-buffer-on-call
	(delete-region (point-min) (point-max)))

    (etalk-mode)

    (setq mode-name etalk-remote-mode-string)

    (make-local-variable 'etalk-remote-who) 
    (setq etalk-remote-who somebody-else)

    (make-local-variable 'etalk-remote-where) 
    (setq etalk-remote-where somewhere-else)

    (make-local-variable 'etalk-remote-tty) 
    (setq etalk-remote-tty sometty-else))

  (make-local-variable 'etalk-remote-preferred-name)
  (setq etalk-remote-preferred-name nil)

  (make-local-variable 'etalk-edit-chars) 
  (setq etalk-edit-chars "")

  (make-local-variable 'etalk-filter-message)
  (setq etalk-filter-message nil)

  (make-local-variable 'etalk-filter-message-type)
  (setq etalk-filter-message-type 0)

  (make-local-variable 'etalk-special-request-function)
  (setq etalk-special-request-function nil)

  ;; upon closer inspection, I couldn't figure out what these were for.
  ;;(make-local-variable 'etalk-special-activation-function)
  ;;(setq etalk-special-activation-function nil)

  (make-local-variable 'etalk-inserted-file)
  (setq etalk-inserted-file nil)
  
  (make-local-variable 'etalk-remote-type)
  (setq etalk-remote-type '["talk" 1 0]) ;default to vanilla talk.
  
  ;; socket is null if using normal methods, a process if there is a process
  ;; if we want to get that info from another user, and an integer if gotten
  ;; from a mail message

  (etalk-startup-tcp-connection socket)
  (run-hooks 'etalk-mode-remote-hooks))

(defun etalk-mode (&optional local)
  "Major mode for using the standard TALK interface via Emacs buffers.
Editing commands are as follows:

When connected to a regular talk server, the following keys work.
\\<etalk-mode-map>
  \\[set-mark-command]     Set mark.
  \\[etalk-nuke-connection] Hangup
  \\[etalk-delete-line]     Kill line. (backwards)
  \\[etalk-clear-window]     Refresh screen.
  \\[etalk-setup-windows]     Redo the windows.
  \\[etalk-setup-windows-plus]   Redo windows with another buffer on the top.
  \\[etalk-setup-windows-with-log] Redo windows with the log buffer on top.
  \\[etalk-delete-word-backwards]     Delete word backwards.
  \\[etalk-yank-text]     Yank kill ring into your talk window.
  \\[etalk-insert-file]   Insert a file into the conversation.
  \\[etalk-brief-help]     Display short/long help message.

When connected to someone else using Emacs Talk, these additional keys work.

  \\[etalk-insert-file-buffer]   Insert a file into a buffer on boths sides.
  \\[etalk-initiate-special-function]   initaite special function (game)
  \\[etalk-remote-multilist]   Create connections to more people through remote.
  \\[etalk-send-minibuffer-message]   Send message to other emacs minibuffer.
  \\[etalk-hug-remote] Send string \"*HUG*\" to remote minibuffer.
"
  (kill-all-local-variables)


  ;; set the visited file name for logging purposes.
  (if etalk-buffer-logging-directory
      (let ((obn (buffer-name)))
	(set-visited-file-name 
	 (concat (expand-file-name 
		  (concat etalk-buffer-logging-directory 
			  (if (string-match "/$" etalk-buffer-logging-directory)
			      "" "/" )
			  (if local
			      (concat (getenv "USER") ".local")
			    (if (string-match "\\( \\)" obn)
				(substring obn 0 (match-beginning 1))
			      obn))))))
	(rename-buffer obn)))

  ;; set the talk-point marker if it exists, or make one.
  (if (not (markerp 'etalk-point)) 
      (progn 
	(make-local-variable 'etalk-point)
	(setq etalk-point (point-max-marker))))

  (set-marker etalk-point (point-max))
  ;; this is a tag saying that the buffer is related to talk
  (make-local-variable 'etalk-tag)
  (setq etalk-tag t)

  (use-local-map etalk-mode-map)
  (setq major-mode 'etalk-mode)

  (setq mode-line-buffer-identification (list etalk-version ": %15b"))
  (setq local-abbrev-table etalk-mode-abbrev-table)

  (make-local-variable 'etalk-remote-is-emacs) ;for local too for ease of use.
  (setq etalk-remote-is-emacs nil)	;set to nil till we really know
  (run-hooks 'etalk-mode-hooks))

(defun etalk-log-buffer ()
  "This function returns the current etalk log buffer.  If it doesn't
exist, it is created."
  
  (if (get-buffer etalk-log-buffer-name)
      ;; just return the buffer if it exists..
      (get-buffer etalk-log-buffer-name)
    ;; set up the buffer so it is usable...
    (set-buffer (get-buffer-create etalk-log-buffer-name))
    (kill-all-local-variables)

    (use-local-map etalk-log-mode-map)
  
    (setq major-mode 'etalk-log)
    (setq mode-name etalk-log-mode-name)

    (make-local-variable 'etalk-point)
    (setq etalk-point (point-max-marker))

    (make-local-variable 'etalk-tag)
    (setq etalk-tag t)

    (make-local-variable 'etalk-filter-message)
    (setq etalk-filter-message nil)

    (make-local-variable 'etalk-filter-message-type)
    (setq etalk-filter-message-type 0)

    (run-hooks 'etalk-log-mode-hooks)

    (current-buffer))
  )
  
(defun etalk-setup-windows (&optional extrabuff)
  "Function: Talkes the list of active etalk buffers and
sets up windows for them on the screen."
  (interactive)

  ;; take over the screen.
  (etalk-zorch-dead-processes)

  ;; get window data type stuff.
  (let ((nw (+ 1 (length etalk-tcp-list))))
    (if (and extrabuff (bufferp (get-buffer extrabuff)))
	(progn
	  (switch-to-buffer extrabuff)
	  (delete-other-windows (selected-window))
	  (split-window (selected-window) (/ (window-height) (+ nw 1)))
	  (other-window 1)
	  (switch-to-buffer (etalk-format etalk-local-buffer-name))
	  )
      (switch-to-buffer (etalk-format etalk-local-buffer-name))
      (delete-other-windows (selected-window)))

    (let ((l etalk-tcp-list))
      (while (> nw 1)
	(split-window (selected-window) (/ (window-height) nw))
	(switch-to-buffer (process-buffer (car (car l))))
	(setq nw (- nw 1))
	(setq l (cdr l))
	(other-window 1))
      (select-window (get-buffer-window 
		      (get-buffer (etalk-format etalk-local-buffer-name)))))))

(defun etalk-setup-windows-with-log ()
  "Run setup-windows with the log buffer as the extra buffer on the top
of the screen."
  (interactive)
  (etalk-setup-windows etalk-log-buffer-name))

(defun etalk-setup-windows-plus (extrabuff)
  "Redraw all the talk windows with a spare one on the bottom with
some random extra buffer on the bottom"

  (interactive "BOther buffer:")
  (etalk-setup-windows extrabuff))

(defun etalk-pop-kill-ring ()
  "Remove the first element from the kill ring."
  ;;doesn't seem to always work.  Will redo someday...
  (interactive)
  (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer)))

(defun etalk-blurb-buffer ()
  "You have the right to remain silent, anything you say can and will
be typecast against you."

  (or etalk-inhibit-startup-message
      (save-excursion
	(setq etalk-inhibit-startup-message t)
	(get-buffer-create "Your Rights with ETALK")
	(set-buffer (get-buffer "Your Rights with ETALK"))
	(delete-region (point-min) (point-max))
	(insert "
     Thanks for using Emacs talk!  [" etalk-version "]

     This program is free under the GNU general public license.
     See the GNU COPYING file for more details.

     Please report Bugs/Problems/Suggestions to: Eric Ludlam via
                                                 zappo@gnu.ai.mit.edu")
    (display-buffer "Your Rights with ETALK"))))



;;; End of talk lisp code.
;;;

;;; At load time, simply startup a process.  Anyone loading this will
;;; be talking anyway, so why not get it overwith right away?
;; (etalk-start-one-process)


(provide 'etalk)
