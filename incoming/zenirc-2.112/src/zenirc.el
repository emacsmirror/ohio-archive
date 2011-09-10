;;; zenirc.el --- Waste time on Internet Relay Chat (ZenIRC client)

;; Copyright (C) 1993, 1994 Ben A. Mesander
;; Copyright (C) 1993, 1994, 1995 Noah S. Friedman
;; Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998 Per Persson

;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;;         Noah Friedman <friedman@prep.ai.mit.edu>
;;         Per Persson <pp@sno.pp.se>
;; Major contributors:
;;         Charles Hannum <mycroft@gnu.ai.mit.edu>
;;         Richard Todd <rmtodd@essex.ecn.uoknor.edu>
;;         Eric Prestemon <ecp@io.com>
;;         Mark Bailen <msbailen@msbdcolka.cr.usgs.gov>
;;         Jason Bastek <jason@marilyn.oit.umass.edu>
;;         Ray Jones <rjones@pobox.com>
;; Maintainer: pp@sno.pp.se
;; Keywords: extensions, zenirc
;; Created: 1993-06-03

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;;       <vuori> The first Lisp-interpreter of the People's Republic
;;               of China, Lisp-130, was written for a Chinese
;;               minicomputer at the Shenyang Science Academy's
;;               processing automation department in 1980. It was an
;;               implementation of Lisp 1.5, which include 94
;;               functions programmed in assembler.
;;
;;               -- Eero Hyvänen, Juko Seppänen: Lisp-world 2, the
;;                  development of Lisp-languages and systems.
;;
;;          <fn> 1960-03-14  LISP introduced, 

;;; Code:

;; Current version of ZenIRC.
(defconst zenirc-version "2.112")

(and (string= (substring emacs-version 0 2) "18")
     (require 'zenirc-18))


;;; User options

(defvar zenirc-buffer-name "*zenirc*"
  "*Basic buffer name for Zen Internet Relay Chat.")

(defvar zenirc-userinfo "Oink."
  "*Reply to USERINFO ctcp.")

(defvar zenirc-ignore-list
  '(;; Ignore messages with more than four CTCP strings
    "\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a"
    )
  "*Patterns of messages from server to ignore.
This should be a list of regular expressions that match IRC protocol messages.
For example, if you wanted to ignore all messages from `foo@bar.com', put
\"PRIVMSG [^!]+!foo@bar\\\\.com \" in the list.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	BEEP.  (Female voice:)  Hi Tony, this is Sheila.  I can't stop
;;;	thinking about you.  When can we get together?  I want to grab
;;;	you and undress you and then BEEP
;;;
;;;	-- From the "Canonical List of Anwering Machine Messages."
;;;       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar zenirc-signal-list '()
  "*List of regular expressions which cause signal notification.")

(defvar zenirc-beep-on-signal nil
  "*If t, beep on signals when not seen.
If 'always, beep on all signals.")

(defvar zenirc-send-confirmation t
  "*If nil, don't confirm sent PRIVMSG/NOTICE.
If t, confirm sent PRIVMSG/NOTICE in the process buffer.
If 'message, confirm sent PRIVMSG/NOTICE in the echo area.

The confirmation looks like \"(sent to #emacs)\".")

(defvar zenirc-timestamp nil
  "*If nil, don't timestamp messages.
If t, timestamp messages.")

(defvar zenirc-timestamp-prefix "["
  "*What to add before the timestamp string")

(defvar zenirc-timestamp-suffix "]"
  "*What do add after the timestamp string")

(defvar zenirc-always-show-channelname t
  "*If nil, don't show channelname in PRIVMSG/NOTICE when it's the same as 
zenirc-current-victim.
If t, always show channelnames in PRIVMSG/NOTICE (when appropriate).")

(defvar zenirc-delete-preceding-whitespaces nil
  "*Whether ZenIRC should delete any whitespaces before the first word 
before sending it off to the server.")

(defvar zenirc-whowas-on-401 nil
  "*Wheter ZenIRC should issue a WHOWAS command if WHOIS returns no nick.")

(defvar zenirc-send-ctcp-errmsg-on-unknown t
  "*If non-`nil', reply to unknown CTCP queries with an ERRMSG reply.

The IRC protocol requires that each query requires a separate error reply,
yet most server implementations will close your connection if you send too
many messages at once (\"flooding\").  This gives malicious users a way to
disconnect you from IRC, but setting this variable to `nil' will prevent it
by simply not replying to invalid CTCP requests.

See also `zenirc-send-ctcp-errmsg-on-unbalanced'.")

(defvar zenirc-send-ctcp-errmsg-on-unbalanced t
  "*If non-`nil', reply to unbalanced CTCP queries with an ERRMSG reply.
See the documentation for `zenirc-send-ctcp-errmsg-on-unknown' for further
information on why it may be useful to set this to `nil'.")

(defvar zenirc-verbose-ctcp t
  "*Should ZenIRC tell you when you send CTCP replies to people?")

(defvar zenirc-fingerdata
  (format "%s <%s@%s>" (user-full-name) (user-real-login-name) (system-name))
  "*CTCP FINGER reply data.")

(defvar zenirc-command-char ?/
  "*Char that begins a command at the beginning of a line")

;;; IRC connection-related variables.

(defvar zenirc-server-alist nil
  "*Association list of port/password/nick info for each server.
This is initialized via `zenirc-ircserver-string->alist' the first time you
start a zenirc session.")

(defvar zenirc-ircserver-environment-variable-name "IRCSERVER"
  "*Name of environment variable containing server/port info.
This variable is used by `zenirc-ircserver-string->alist'.
It is user-settable so that you can potentially define different
environment variables for different clients.")

;; Give a default for this since there's no easy way of guessing a server
;; name if you don't know any.
(defvar zenirc-server-default "irc.stealth.net"
  "*Server to use if no other is specified.
See `zenirc-server-alist' and `zenirc-establish-server-connection'.")

(defvar zenirc-nick-default nil
  "*Nickname to use if no other is specified.
See `zenirc-server-alist' and `zenirc-establish-server-connection'.")

(defvar zenirc-password-default nil
  "*Default server password to use if no other is specified.
See `zenirc-server-alist' and `zenirc-establish-server-connection'.")

(defvar zenirc-port-default nil
  "*Default server port to use if no other is specified.
See `zenirc-server-alist' and `zenirc-establish-server-connection'.")

(defvar zenirc-user-full-name-default nil
  "*Default full name used to describe yourself on irc.
See `zenirc-establish-server-connection'.")

(defvar zenirc-user-login-name-default nil
  "*Default user name to use if no other is specified.
See `zenirc-server-alist' and `zenirc-establish-server-connection'.")

(defvar zenirc-process-connect-function 'open-network-stream
  "*Function used to establish server connection.
This is called by `zenirc-establish-server-connection' and should take the
same arguments normally given to `open-network-stream'.
This function can be used to make proxy connections.")


;;; Misc variables of interest.
;;; Most of these are reasonable for users to modify.

(defconst zenirc-message-length-limit 450
  "Maximum length of messages that can be sent on a single line.

Actually, this isn't really the length of the message the client is allowed
to send; it includes cruft that might be added by the server and over which
you have little knowledge or control.  For example, if your system is not
configured so that gethostname returns the FQDN for your host, the
calculations in zenirc-send-multi-line may be off from what the server
considers is your hostname.  Another way in which it can fail is if your
host name is shorter than the corresponding IP address and the server
failed to resolve your hostname.

In light of these possibilities, this constant is set to 450 even though
the theoretical maximum allowed is 512 according to RFC1459.")

(defvar zenirc-mode-map '()
  "*Sparse keymap for zenirc-mode")
(cond
 ((null zenirc-mode-map)
  (setq zenirc-mode-map (make-sparse-keymap))
  (define-key zenirc-mode-map "\n" 'zenirc-send-line)
  (define-key zenirc-mode-map "\C-m" 'zenirc-send-line)
  (define-key zenirc-mode-map "\C-c\C-t" 'zenirc-toggle-channel)
  (define-key zenirc-mode-map "\C-c\C-r" 'zenirc-send-privmsg-last-rec)
  (define-key zenirc-mode-map "\C-c\C-s" 'zenirc-send-privmsg-last-sent)
  (define-key zenirc-mode-map ":" 'zenirc-self-insert-or-send-privmsg-last-rec)
  (define-key zenirc-mode-map ";" 
    'zenirc-self-insert-or-send-privmsg-last-sent)))

;; These strings should be in the format "ftp-server:directory:file".
(defvar zenirc-source-list
  '("ftp.splode.com:/pub/zenirc:zenirc.tar.gz")
  "Where to retrieve ZenIRC from.")

;; Existing client messages are recycled here where possible, as it makes
;; it more likely that other clients will format them correctly.
(defvar zenirc-clientinfo-list
  '((ACTION . "ACTION contains action descriptions for atmosphere")
    (CLIENTINFO
     . "CLIENTINFO gives information about available CTCP commands")
    (ECHO . "ECHO returns string sent by other person")
    (ERRMSG . "ERRMSG returns error messages")
    (FINGER . "FINGER shows real name, and login name of user")
    (PING . "PING returns the arguments it receives")
    ;; sojge sure is a wordy bastard.
    ;; The description is made up out of zero or more lines followed by an
    ;; end marker.
    ;; Every line is a CTCP reply with the SOURCE keyword, a space, the
    ;; name of a FTP-server, a colon, a directory name, a colon, and 0 or
    ;; more file names.
    ;; If no file names are given, all the files in the named directory are
    ;; needed.  The end marker contains just the keyword.
    (SOURCE . "SOURCE Where to find the source code for this client")
    (TIME . "TIME tells you the time on the user's host")
    (USERINFO . "USERINFO returns user settable information")
    (VERSION . "VERSION shows client type, version, and environment"))
  "*Association list of CLIENTINFO CTCP help strings")

(defvar zenirc-clientinfo-string "ACTION CLIENTINFO ECHO ERRMSG FINGER PING SOURCE TIME USERINFO VERSION :Use CLIENTINFO <COMMAND> to get more specific information"
  "*CLIENTINFO Help string, showing list of CTCP commands supported")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;	"I wish this video had some explosions. That would be cool."
;;;	"Heh heh henh hmm heh. It does have some explosions. Heh henh hmm."
;;;	"Faries grant wishes. Huh huh heh huh hunh."
;;;
;;;	-- Beavis & Butthead
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; debugging variables.  for the adventurous.
(defvar zenirc-debug-mainloop nil)
(defvar zenirc-debug-ignore nil)
(defvar zenirc-debug-signal nil)
(defvar zenirc-debug-ctcp nil)
(defvar zenirc-debug-commands nil)
(defvar zenirc-debug-timer nil)
(defvar zenirc-bug-address "zenirc-bug@splode.com")


;;; local state variables.
;;; It's probably not useful for the user to change these, unless necessary
;;; for particular extensions.

(defvar zenirc-server nil)
(make-variable-buffer-local 'zenirc-server)

(defvar zenirc-port nil)
(make-variable-buffer-local 'zenirc-port)

(defvar zenirc-password nil)
(make-variable-buffer-local 'zenirc-password)

(defvar zenirc-nick nil)
(make-variable-buffer-local 'zenirc-nick)

(defvar zenirc-user-login-name nil)
(make-variable-buffer-local 'zenirc-user-login-name)

(defvar zenirc-user-full-name nil)
(make-variable-buffer-local 'zenirc-user-full-name)

;; The name the current IRC server calls itself.
;; This can differ from `zenirc-server' if one is just a network alias of
;; the other.
(defvar zenirc-current-server-name nil)
(make-variable-buffer-local 'zenirc-current-server-name)

;; current channel or luser, or nil
(defvar zenirc-current-victim nil)
(make-variable-buffer-local 'zenirc-current-victim)

;; variables to store the nick you last sent to or that last sent to you
(defvar zenirc-privmsg-last-rec "")
(make-variable-buffer-local 'zenirc-privmsg-last-rec)

(defvar zenirc-privmsg-last-sent "")
(make-variable-buffer-local 'zenirc-privmsg-last-sent)

;; remember last person we saw a privmsg from.
(defvar zenirc-privmsg-last-seen nil)
(make-variable-buffer-local 'zenirc-privmsg-last-seen)

;; a list of channels the client is on
(defvar zenirc-channel-list '())
(make-variable-buffer-local 'zenirc-channel-list)

;; We use this marker instead of the process mark, because the latter goes
;; away when a process exits, which is a gratuitous nuisance.
(defvar zenirc-process-mark nil)
(make-variable-buffer-local 'zenirc-process-mark)

;; unprocessed data read from socket
(defvar zenirc-unprocessed-output nil)
(make-variable-buffer-local 'zenirc-unprocessed-output)

;; standard vector into which parsed messages are stashed, to avoid
;; consing new vectors each time.
(defvar zenirc-message-vector (make-vector 12 nil))
(make-variable-buffer-local 'zenirc-message-vector)

;; allowed server modes (set in 004 reply)
(defvar zenirc-server-modes nil)
(make-variable-buffer-local 'zenirc-server-modes)

;; server version (set in 004 reply)
(defvar zenirc-server-version nil)
(make-variable-buffer-local 'zenirc-server-version)

(defvar zenirc-time-last-event nil)
(make-variable-buffer-local 'zenirc-time-last-event)

;; allowed user modes (set in 004 reply)
(defvar zenirc-user-modes nil)
(make-variable-buffer-local 'zenirc-user-modes)


;;; Standard hooks

;; TODO: implement "filters" and turn this into one.
(defvar zenirc-format-nickuserhost-hook
  '(identity)
  "*List of filters used to format nicknames in displayed messages.")

(defvar zenirc-mode-hook nil
  "*Hook to run at the end of zenirc-mode.")

(defvar zenirc-startup-hook nil
  "*Hook run before establishing a server connection.")

(defvar zenirc-exit-hook nil
  "*Hook to run when zenirc exits.")

(defvar zenirc-connect-hook nil
  "*Hook to run registering with an IRC server.")

(defvar zenirc-timer-hook nil
  "*Timer hook variable.")

(defvar zenirc-signal-hook '(zenirc-signal)
  "*Signal hook variable.")

(defvar zenirc-message-hook nil
  "*Hook to run whenever a message is inserted in the zenirc buffer.
The buffer is narrowed to the region containing the newly-inserted text,
and is called with two arguments: the process (if known) and the unmodified
string.  This string may not match exactly what is currently in the buffer,
since functions on this hook can easily modify the latter.")

;; Hooks for various server commands.
;; These are commands that the user types, e.g. "/quit" (the best command
;; of all!).  For any given command CMD, the hook zenirc-command-CMD-hook
;; is run.  If the user types a command for which there is no hook, the
;; command is passed directly to the server.
(defvar zenirc-command-away-hook '(zenirc-command-away))
(defvar zenirc-command-action-hook '(zenirc-command-action))
(defvar zenirc-command-command-char-hook '(zenirc-command-command-char))
(defvar zenirc-command-ctcp-hook '(zenirc-command-ctcp))
(defvar zenirc-command-kick-hook '(zenirc-command-kick))
(defvar zenirc-command-kill-hook '(zenirc-command-kill))
(defvar zenirc-command-language-hook '(zenirc-command-language))
(defvar zenirc-command-m-hook '(zenirc-command-m))
(defvar zenirc-command-me-hook '(zenirc-command-me))
(defvar zenirc-command-msg-hook '(zenirc-command-msg))
(defvar zenirc-command-notice-hook '(zenirc-command-notice))
(defvar zenirc-command-oper-hook '(zenirc-command-oper))
(defvar zenirc-command-part-hook '(zenirc-command-part))
(defvar zenirc-command-ping-hook '(zenirc-command-ping))
(defvar zenirc-command-privmsg-hook '(zenirc-command-privmsg))
(defvar zenirc-command-query-hook '(zenirc-command-query))
(defvar zenirc-command-quit-hook '(zenirc-command-quit))
(defvar zenirc-command-quote-hook '(zenirc-command-quote))
(defvar zenirc-command-server-hook '(zenirc-command-server))
(defvar zenirc-command-squit-hook '(zenirc-command-squit))
(defvar zenirc-command-topic-hook '(zenirc-command-topic))

;; Hooks run after various kinds of messages are sent
;; These hooks get several args: a process, a format specifier to use if
;; the message sent had to be sent in multiple chunks, and format specifier
;; to use if the entire message fit in one line, the recipient, and the
;; number of chunks actually sent.
;; See zenirc-send-confirmation-generic for an example.
(defvar zenirc-send-confirmation-privmsg-hook
  '(zenirc-send-confirmation-generic))
(defvar zenirc-send-confirmation-notice-hook
  '(zenirc-send-confirmation-generic))
(defvar zenirc-send-confirmation-me-hook '(zenirc-send-confirmation-generic))
(defvar zenirc-send-line-hook nil
  "*Hook run after a line of input is sent to the server.
Functions on this hook get three args: two integers specifying the
beginning and ending points in the buffer containing the text sent, and a
string representing the formatted text actually sent to the server (the
main difference is that embedded newlines are mapped to spaces).")

;; Hooks run to generate replies to CTCP queries.
(defvar zenirc-ctcp-reply-PING-hook '(zenirc-ctcp-reply-PING))
(defvar zenirc-ctcp-query-ACTION-hook '(zenirc-ctcp-query-ACTION))
(defvar zenirc-ctcp-query-CLIENTINFO-hook '(zenirc-ctcp-query-CLIENTINFO))
(defvar zenirc-ctcp-query-ECHO-hook '(zenirc-ctcp-query-ECHO))
(defvar zenirc-ctcp-query-ERRMSG-hook '(zenirc-ctcp-query-ERRMSG))
(defvar zenirc-ctcp-query-FINGER-hook '(zenirc-ctcp-query-FINGER))
(defvar zenirc-ctcp-query-PING-hook '(zenirc-ctcp-query-PING))
(defvar zenirc-ctcp-query-SOURCE-hook '(zenirc-ctcp-query-SOURCE))
(defvar zenirc-ctcp-query-TIME-hook '(zenirc-ctcp-query-TIME))
(defvar zenirc-ctcp-query-USERINFO-hook '(zenirc-ctcp-query-USERINFO))
(defvar zenirc-ctcp-query-VERSION-hook '(zenirc-ctcp-query-VERSION))

;; Hooks run in response to messages from the server.
;; For any message of type TYPE, the hook zenirc-server-TYPE-hook is run.
(defvar zenirc-server-ERROR-hook '(zenirc-server-ERROR))
(defvar zenirc-server-INVITE-hook '(zenirc-server-INVITE))
(defvar zenirc-server-JOIN-hook '(zenirc-server-JOIN))
(defvar zenirc-server-KICK-hook '(zenirc-server-KICK))
(defvar zenirc-server-KILL-hook '(zenirc-server-KILL))
(defvar zenirc-server-MODE-hook '(zenirc-server-MODE))
(defvar zenirc-server-NICK-hook '(zenirc-server-NICK))
(defvar zenirc-server-NOTICE-hook '(zenirc-server-NOTICE))
(defvar zenirc-server-PART-hook '(zenirc-server-PART))
(defvar zenirc-server-PING-hook '(zenirc-server-PING))
(defvar zenirc-server-PONG-hook '(zenirc-server-PONG))
(defvar zenirc-server-PRIVMSG-hook '(zenirc-server-PRIVMSG))
(defvar zenirc-server-QUIT-hook '(zenirc-server-QUIT))
(defvar zenirc-server-TOPIC-hook '(zenirc-server-TOPIC))
(defvar zenirc-server-WALLOPS-hook '(zenirc-server-WALLOPS))
(defvar zenirc-server-001-hook '(zenirc-server-001))
(defvar zenirc-server-002-hook '(zenirc-server-002))
(defvar zenirc-server-003-hook '(zenirc-server-003))
(defvar zenirc-server-004-hook '(zenirc-server-004))
(defvar zenirc-server-200-hook '(zenirc-server-200))
(defvar zenirc-server-201-hook '(zenirc-server-201))
(defvar zenirc-server-202-hook '(zenirc-server-202))
(defvar zenirc-server-203-hook '(zenirc-server-203))
(defvar zenirc-server-204-hook '(zenirc-server-204))
(defvar zenirc-server-205-hook '(zenirc-server-205))
(defvar zenirc-server-206-hook '(zenirc-server-206))
(defvar zenirc-server-208-hook '(zenirc-server-208))
(defvar zenirc-server-209-hook '(zenirc-server-209))
(defvar zenirc-server-211-hook '(zenirc-server-211))
(defvar zenirc-server-212-hook '(zenirc-server-212))
(defvar zenirc-server-213-hook '(zenirc-server-213))
(defvar zenirc-server-214-hook '(zenirc-server-214))
(defvar zenirc-server-215-hook '(zenirc-server-215))
(defvar zenirc-server-216-hook '(zenirc-server-216))
(defvar zenirc-server-217-hook '(zenirc-server-217))
(defvar zenirc-server-218-hook '(zenirc-server-218))
(defvar zenirc-server-219-hook '(zenirc-server-219))
(defvar zenirc-server-221-hook '(zenirc-server-221))
(defvar zenirc-server-241-hook '(zenirc-server-241))
(defvar zenirc-server-242-hook '(zenirc-server-242))
(defvar zenirc-server-243-hook '(zenirc-server-243))
(defvar zenirc-server-244-hook '(zenirc-server-244))
(defvar zenirc-server-249-hook '(zenirc-server-249))
(defvar zenirc-server-251-hook '(zenirc-server-251))
(defvar zenirc-server-252-hook '(zenirc-server-252))
(defvar zenirc-server-253-hook '(zenirc-server-253))
(defvar zenirc-server-254-hook '(zenirc-server-254))
(defvar zenirc-server-255-hook '(zenirc-server-255))
(defvar zenirc-server-256-hook '(zenirc-server-256))
(defvar zenirc-server-257-hook '(zenirc-server-257))
(defvar zenirc-server-258-hook '(zenirc-server-258))
(defvar zenirc-server-259-hook '(zenirc-server-259))
(defvar zenirc-server-261-hook '(zenirc-server-261))
(defvar zenirc-server-262-hook '(zenirc-server-262))
(defvar zenirc-server-301-hook '(zenirc-server-301))
(defvar zenirc-server-302-hook '(zenirc-server-302))
(defvar zenirc-server-303-hook '(zenirc-server-303))
(defvar zenirc-server-305-hook '(zenirc-server-305))
(defvar zenirc-server-306-hook '(zenirc-server-306))
(defvar zenirc-server-311-hook '(zenirc-server-311))
(defvar zenirc-server-312-hook '(zenirc-server-312))
(defvar zenirc-server-313-hook '(zenirc-server-313))
(defvar zenirc-server-314-hook '(zenirc-server-314))
(defvar zenirc-server-315-hook '(zenirc-server-315))
(defvar zenirc-server-317-hook '(zenirc-server-317))
(defvar zenirc-server-318-hook '(zenirc-server-318))
(defvar zenirc-server-319-hook '(zenirc-server-319))
(defvar zenirc-server-321-hook '(zenirc-server-321))
(defvar zenirc-server-322-hook '(zenirc-server-322))
(defvar zenirc-server-323-hook '(zenirc-server-323))
(defvar zenirc-server-324-hook '(zenirc-server-324))
(defvar zenirc-server-331-hook '(zenirc-server-331))
(defvar zenirc-server-332-hook '(zenirc-server-332))
(defvar zenirc-server-333-hook '(zenirc-server-333))
(defvar zenirc-server-341-hook '(zenirc-server-341))
(defvar zenirc-server-342-hook '(zenirc-server-342))
(defvar zenirc-server-351-hook '(zenirc-server-351))
(defvar zenirc-server-352-hook '(zenirc-server-352))
(defvar zenirc-server-353-hook '(zenirc-server-353))
(defvar zenirc-server-364-hook '(zenirc-server-364))
(defvar zenirc-server-365-hook '(zenirc-server-365))
(defvar zenirc-server-366-hook '(zenirc-server-366))
(defvar zenirc-server-367-hook '(zenirc-server-367))
(defvar zenirc-server-368-hook '(zenirc-server-368))
(defvar zenirc-server-369-hook '(zenirc-server-369))
(defvar zenirc-server-371-hook '(zenirc-server-371))
(defvar zenirc-server-372-hook '(zenirc-server-372))
(defvar zenirc-server-374-hook '(zenirc-server-374))
(defvar zenirc-server-375-hook '(zenirc-server-375))
(defvar zenirc-server-376-hook '(zenirc-server-376))
(defvar zenirc-server-381-hook '(zenirc-server-381))
(defvar zenirc-server-382-hook '(zenirc-server-382))
(defvar zenirc-server-391-hook '(zenirc-server-391))
(defvar zenirc-server-392-hook '(zenirc-server-392))
(defvar zenirc-server-393-hook '(zenirc-server-393))
(defvar zenirc-server-394-hook '(zenirc-server-394))
(defvar zenirc-server-395-hook '(zenirc-server-395))
(defvar zenirc-server-401-hook '(zenirc-server-401))
(defvar zenirc-server-402-hook '(zenirc-server-402))
(defvar zenirc-server-403-hook '(zenirc-server-403))
(defvar zenirc-server-404-hook '(zenirc-server-404))
(defvar zenirc-server-405-hook '(zenirc-server-405))
(defvar zenirc-server-406-hook '(zenirc-server-406))
(defvar zenirc-server-407-hook '(zenirc-server-407))
(defvar zenirc-server-409-hook '(zenirc-server-409))
(defvar zenirc-server-411-hook '(zenirc-server-411))
(defvar zenirc-server-412-hook '(zenirc-server-412))
(defvar zenirc-server-413-hook '(zenirc-server-413))
(defvar zenirc-server-414-hook '(zenirc-server-414))
(defvar zenirc-server-415-hook '(zenirc-server-415))
(defvar zenirc-server-421-hook '(zenirc-server-421))
(defvar zenirc-server-422-hook '(zenirc-server-422))
(defvar zenirc-server-423-hook '(zenirc-server-423))
(defvar zenirc-server-424-hook '(zenirc-server-424))
(defvar zenirc-server-431-hook '(zenirc-server-431))
(defvar zenirc-server-432-hook '(zenirc-server-432))
(defvar zenirc-server-433-hook '(zenirc-server-433))
(defvar zenirc-server-436-hook '(zenirc-server-436))
(defvar zenirc-server-437-hook '(zenirc-server-437))
(defvar zenirc-server-441-hook '(zenirc-server-441))
(defvar zenirc-server-442-hook '(zenirc-server-442))
(defvar zenirc-server-443-hook '(zenirc-server-443))
(defvar zenirc-server-444-hook '(zenirc-server-444))
(defvar zenirc-server-445-hook '(zenirc-server-445))
(defvar zenirc-server-446-hook '(zenirc-server-446))
(defvar zenirc-server-451-hook '(zenirc-server-451))
(defvar zenirc-server-461-hook '(zenirc-server-461))
(defvar zenirc-server-462-hook '(zenirc-server-462))
(defvar zenirc-server-463-hook '(zenirc-server-463))
(defvar zenirc-server-464-hook '(zenirc-server-464))
(defvar zenirc-server-465-hook '(zenirc-server-465))
(defvar zenirc-server-467-hook '(zenirc-server-467))
(defvar zenirc-server-471-hook '(zenirc-server-471))
(defvar zenirc-server-472-hook '(zenirc-server-472))
(defvar zenirc-server-473-hook '(zenirc-server-473))
(defvar zenirc-server-474-hook '(zenirc-server-474))
(defvar zenirc-server-475-hook '(zenirc-server-475))
(defvar zenirc-server-477-hook '(zenirc-server-477))
(defvar zenirc-server-481-hook '(zenirc-server-481))
(defvar zenirc-server-482-hook '(zenirc-server-482))
(defvar zenirc-server-483-hook '(zenirc-server-483))
(defvar zenirc-server-491-hook '(zenirc-server-491))
(defvar zenirc-server-501-hook '(zenirc-server-501))
(defvar zenirc-server-502-hook '(zenirc-server-502))


(defun zenirc-mode ()
  "Major mode for wasting major time on IRC."
  (kill-all-local-variables)

  (setq mode-name "ZenIRC")
  (setq major-mode 'zenirc-mode)
  (use-local-map zenirc-mode-map)
  (setq mode-line-process '(":%s"))
  (setq mode-line-format
        '( ""
           mode-line-modified
           mode-line-buffer-identification
           " "
           global-mode-string
           " "
           (-3 . "%p")
           " %[("
           mode-name
           mode-line-process
           "%n"
           minor-mode-alist
           ")%] "
           zenirc-nick
           (zenirc-current-victim ("->" zenirc-current-victim))
           " "
           "%-"))
  (zenirc-run-hook 'zenirc-mode-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code to handle connection to server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun zenirc (&optional prefix)
  "Waste time on IRC.

If an irc session already exists, switch to that session.
With prefix arg, start a new session even if another exists.

If buffer exists but zenirc process is not running, make new process.
If buffer exists and zenirc process is running, just switch to that buffer.
If an explicit numeric prefix argument is given (or this function is called
from lisp with a numeric argument), switch to the buffer named
\"*zenirc*<prefix>\", e.g. \"*zenirc*<2>\".  If there is no process in that
buffer, start one.
If a prefix argument is given but it is not a number, create a new buffer
and start a process in it.  This is the same as calling the function from
lisp with an argument of `t'."
  (interactive "P")
  (let* ((zenirc-buffer (if prefix
                            (generate-new-buffer zenirc-buffer-name)
                          (get-buffer-create zenirc-buffer-name)))
         (process (get-buffer-process zenirc-buffer)))
    (pop-to-buffer zenirc-buffer)

    (cond
     ((and process
	   (memq (process-status process) '(open run))))
     (t
      (zenirc-mode)

      (or zenirc-server-alist
          (setq zenirc-server-alist (zenirc-ircserver-string->alist)))

      (setq zenirc-unprocessed-output "")
      (setq zenirc-current-victim nil)

      ;; Time of last event in zenirc - set it to "now"
      (setq zenirc-time-last-event
            (zenirc-time-to-int (current-time-string)))

      ;; note the semantics here that the current buffer when
      ;; zenirc-startup-hook is run is zenirc-buffer.
      (zenirc-run-hook 'zenirc-startup-hook)

      ;; Do this before opening network stream, if currently unset.
      ;; If already set, preserve so that user can save input.
      (or zenirc-process-mark
          (setq zenirc-process-mark
                (set-marker (make-marker) (point-max) zenirc-buffer)))

      (or (setq process
                (zenirc-establish-server-connection zenirc-buffer))
          (error "zenirc: could not establish any server connection."))

      (set-process-buffer process zenirc-buffer)
      (set-process-filter process 'zenirc-filter)
      (set-process-sentinel process 'zenirc-sentinel)
      (zenirc-login process)
      (zenirc-run-hook 'zenirc-connect-hook process)))))

(defun zenirc-select (&optional server port nick)
  "Run manually or by issuing /server from a ZenIRC buffer.

This function starts a new ZenIRC buffer and connects to a given server.
Variables not already given are queried for, using zenirc-server-alist 
for default values. zenirc-server-alist is also updated each time this
function is issued."

  (interactive)
  (if (integerp port) (setq port (int-to-string port)))
  (let ((new-server) (new-port) (new-nick))
    (setq 
     new-server
     ; server to connect to
     (or server
     ; server is not given, query user
	 (completing-read "Server: " zenirc-server-alist nil nil 
			  (or
			   (car (car zenirc-server-alist))
			   ; last resort default
			   zenirc-server-default)))
     new-port
     ; port to connect to
     (or
      port
      ; port is not given, query user
      (read-string "Port: " 
		   (or
		    (if (car (cdr (assoc new-server zenirc-server-alist)))
			(int-to-string
			 (car (cdr (assoc new-server zenirc-server-alist)))))
		    (if zenirc-port (int-to-string zenirc-port))
		    ; last resort default
		    (getenv "IRCPORT")
		    "6667")))
     new-nick
     ; nickname to use
     (or 
      nick
      ; nickname is not given, query user
      (read-string "Nickname: " 
		   (or 
		    (car (nthcdr 3 (assoc new-server zenirc-server-alist)))
		    zenirc-nick
		    (getenv "IRCNICK")
		    ; last resort default
		    (user-login-name)))))
    ; update zenirc-server-alist
    (let ((new-list (list new-server (string-to-int new-port) nil new-nick)))
      (if (not (member new-list zenirc-server-alist))
	  ; a new entry is given
	  (setq zenirc-server-alist 
		(cons new-list zenirc-server-alist))
	; move old entry to the top of zenirc-server-alist
	(setq zenirc-server-alist (delete new-list zenirc-server-alist)
	      zenirc-server-alist (cons new-list zenirc-server-alist)))
      ; make sure we don't try to connect to anything else then the
      ; given server
      (let ((zenirc-server-alist (list new-list)))
	; run the actual connection, at last
	(zenirc t)))))

(defun zenirc-establish-server-connection (buffer &optional alist)
  "Waste time by connecting to an irc server.
This function takes two arguments: a buffer and an optional alist
of the same form as that returned by `zenirc-ircserver-string->alist'.
If none is specified, the default is `zenirc-server-alist'.

For each server in the alist, attempt to connect to it on the appropriate
port and with the appropriate nicknames, etc.

If any of the elements in the list for a server is unspecified, one of the
following defaults is used, in the specified order of priority (names in
caps preceded with `$' are environment variables):

   port:     zenirc-port-default, $IRCPORT
   password: zenirc-password-default
   nickname: zenirc-nick-default, $IRCNICK, (user-login-name)
   username: zenirc-user-login-name-default, $USER, (user-login-name)

Finally, if zenirc-server-alist is nil and no other alist is specified,
connect to `zenirc-server-default', or $IRCSERVER, using defaults as
described above."
  (save-excursion
    (set-buffer buffer)
    (or alist
        (setq alist zenirc-server-alist)
        (setq alist (list (list zenirc-server-default))))
    (let ((procname (concat "zenirc:" (buffer-name)))
          ent server port proc)
      (while alist
	(setq ent (car alist))
	(setq alist (cdr alist))
	
	;; Note that we check the environment variable before the
	;; -default variable.  This is pretty much the only exception.
	(setq server (or (car ent)
			 (let ((server (getenv "IRCSERVER")))
			   (and server
				(substring server 0
					   (string-match " \\|:" server))))
			 zenirc-server-default
			 (error "no server specified.")))
	
	(setq port (or (nth 1 ent)
		       (let ((p (getenv "IRCPORT")))
			 (and p (string-to-int p)))
		       zenirc-port-default
		       6667))
	
	(condition-case data
	    (progn
	      (zenirc-message buffer 'connect-try server port)
	      ;; Do a redisplay before connecting, in case the server is
	      ;; slow to respond.
	      (sit-for 0)
	      (setq proc (funcall zenirc-process-connect-function
				  procname buffer server port))
	      ;; Update connection status in modeline.
	      (force-mode-line-update)
	      (setq alist nil)
	      (setq zenirc-server          server)
	      ;; This might get reset later, but initialize it.
	      (setq zenirc-current-server-name server)
	      
	      (setq zenirc-port            port)
	      (setq zenirc-password        (or (nth 2 ent)
					       zenirc-password-default))
	      (setq zenirc-nick            (or (nth 3 ent)
					       zenirc-nick-default
					       (getenv "IRCNICK")
					       (user-login-name)
					       "Thoth")) ; it -is- funny
	      (setq zenirc-user-full-name  (or (nth 4 ent)
					       zenirc-user-full-name-default
					       (getenv "IRCNAME")
					       (user-full-name)
					       "Thoth"))
	      (setq zenirc-user-login-name (or (nth 5 ent)
					       zenirc-user-login-name-default
					       (getenv "USER")
					       (user-login-name)
					       "Thoth")))
	  (quit
	   (setq alist nil)
	   (zenirc-message buffer 'connect-abort))
	  
	  (file-error
	   ;; file-error "connection failed" "connection timed out" host proc
	   ;; file-error "connection failed" "connection refused" host proc
	   (if (string= (nth 1 data) "connection failed")
	       (zenirc-message buffer 'connect-failed server port
			       (nth 2 data))
	     (signal 'file-error data)))
	  (error
	   ;; data == (error "Unknown host \"foo\"")
	   (if (string-match "^Unknown host" (nth 1 data))
	       (zenirc-message buffer 'connect-failed server port
			       (nth 1 data))
	     (apply 'signal data)))))
      proc)))

(defun zenirc-ircserver-string->alist (&optional str)
  "*Create association list of server to port/password/nick/username.

This function takes a string of the form

      \"SERVER1:PORT1:PASSWORD1:NICKNAME1:USERNAME1  SERVER2:...\"

If more than one entry is desired, separate each entry in the string
variable with any nonzero amount of whitespace composed of spaces, tabs,
and/or newlines.

If no string is specified, the value of the environment variable
specified by `zenirc-ircserver-environment-variable-name'.

The alist returned consist of lists containing the following elements,
and satisfy the corresponding type predicates:

    SERVER:   `stringp'
    PORT:     `natnump' or `null'
    PASSWORD: `stringp' or `null'
    NICKNAME: `stringp' or `null'
    USERNAME: `stringp' or `null'

These alists specify a list of servers and related data with which zenirc
should attempt to connect to servers; generally, each one is tried until a
successful connection is made.  See `zenirc-establish-server-connection'."
  (or str (setq str (getenv zenirc-ircserver-environment-variable-name)))
  (cond
   ((null str) nil)
   ((let ((len (length str))
          (pos 0)
          (result nil)
          tmp tmplen tmppos
          tmplist)
      (save-match-data
        (and (string-match "^[ \t\r\n]+" str pos)
             (setq pos (match-end 0)))

        (while (< pos len)
          (cond ((string-match "[ \t\r\n]+" str pos)
                 (setq tmplen (- (match-beginning 0) pos))
                 (setq tmp (substring str pos (+ pos tmplen)))
                 (setq pos (match-end 0)))
                (t
                 (setq tmplen (- len pos))
                 (setq tmp (if (zerop pos)
                               str
                             (substring str pos)))
                 (setq pos len)))

          (setq tmppos 0)
          (setq tmplist nil)
          (while (< tmppos tmplen)
            (cond ((string-match ":" tmp tmppos)
                   (setq tmplist
                         (cons (substring tmp tmppos (match-beginning 0))
                               tmplist))
                   (and (string= (car tmplist) "")
                        (setcar tmplist nil))
                   (setq tmppos (match-end 0)))
                  (t
                   (and (string= tmp "")
                        (setq tmp nil))
                   (setq tmplist (cons (if (zerop tmppos)
                                           tmp
                                         (substring tmp tmppos))
                                       tmplist))
                   (setq tmppos tmplen))))
          (and tmplist
               (progn
                 (setq tmplist (nreverse tmplist))
                 (and (stringp (nth 1 tmplist))
                      ;; convert port number to int
                      (setcar (nthcdr 1 tmplist)
                              (string-to-int (nth 1 tmplist))))
                 (setq result (cons tmplist result))))))
      (nreverse result)))))

;; send nick, user@host information
;; NICK zenirc-nick
;; USER zenirc-user-login-name (system-name) zenirc-server 
;;                                  :zenirc-user-full-name
(defun zenirc-login (proc)
  (and zenirc-password
       (process-send-string proc (format "PASS %s\n" zenirc-password)))
  ;; Send user info first; some servers reject connections otherwise.
  (process-send-string proc (format "USER %s %s %s :%s\n"
                                    zenirc-user-login-name
                                    (system-name)
                                    zenirc-server
                                    zenirc-user-full-name))
  (process-send-string proc (format "NICK %s\n" zenirc-nick)))
    
(defun zenirc-sentinel (proc str)
  (save-excursion
    (set-buffer (process-buffer proc))
    (zenirc-run-hook 'zenirc-exit-hook proc str)
    (zenirc-message proc 'sentinel (current-time-string))))


;; This function takes a chunk of text from the server, and any text
;; left over from the last chunk, and passes it to zenirc-parse-output
;; to be interpreted.
(defun zenirc-filter (proc string)
  (let ((orig-buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer (process-buffer proc))
          (setq zenirc-unprocessed-output
                (zenirc-parse-output proc string zenirc-unprocessed-output)))
      (set-buffer orig-buffer))))

;; This routine takes a bunch of text from the server, and any remnants
;; from the last bunch, and splits it into lines. The lines are passed to
;; zenirc-parse-server-message to be parsed and then whatever needs to be
;; done for that server message is done.
(defun zenirc-parse-output (proc string unparsed-output)
  (let* ((unparsed (concat unparsed-output string))
         (proc-window (get-buffer-window (process-buffer proc)))
	 (ignored nil)
         eol line parsed)
    (save-match-data
      (while (setq eol (string-match "\n" unparsed))

        ;; Somewhere around ircd 2.8.16.0, server messages start coming in
        ;; with a C-m (ascii 13, carriage return) at the end.
        ;; (Incidentally, that is the correct thing to do; all textually
        ;; based network protocols should use CRLF rather than just LF, for
        ;; the sake of consistency.  --friedman)
        (if (= (aref unparsed (1- eol)) ?\C-m)
            (setq line (substring unparsed 0 (1- eol)))
          (setq line (substring unparsed 0 eol)))
        (setq unparsed (substring unparsed (1+ eol)))
        (cond
         ((zenirc-ignore-p line)
          (and zenirc-debug-ignore
               (zenirc-message proc 'debug (concat "Ignored: " line))
	       (setq ignored t)))
         (t
          (let* ((parsed (zenirc-parse-server-message line))
                 (hook-name (concat "zenirc-server-" (aref parsed 0) "-hook"))
                 (hook (intern-soft hook-name)))
            (cond
             (zenirc-debug-mainloop
              (zenirc-message proc 'debug (concat "Hook: " hook-name))
              (zenirc-message proc 'debug
                              (concat "Parsed: "
                                      (prin1-to-string parsed)))))

            (zenirc-timer-handler proc)
            (if (and hook (boundp hook))
                (zenirc-run-hook hook proc parsed)
              (zenirc-message proc 'server line))
            (if (and (not ignored)
		     (zenirc-signal-p line))
                (zenirc-run-hook 'zenirc-signal-hook proc parsed))))))
      ;; return the unprocessed partial line, if any.
      unparsed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utility subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns a string indicating emacs variant.
(defun zenirc-emacs-variant ()
  (let ((case-fold-search t)
        (alist '(("^Nemacs\\b"   . "Nemacs")
                 ("^Epoch\\b"    . "Epoch")
                 ("\\bXEmacs\\b" . "XEmacs")
                 ("\\bLucid\\b"  . "Lucid Emacs")
                 ("^GNU Emacs"   . "GNU Emacs")))
        (version (cond
                  ((fboundp 'nemacs-version)
                   (nemacs-version))
                  (t
                   (emacs-version))))
        result)
    (save-match-data
      (while alist
        (cond
         ((string-match (car (car alist)) version)
          (setq result (cdr (car alist)))
          (setq alist nil))
         (t
          (setq alist (cdr alist))))))
    result))

;; Update the modeline, or whatever it takes to actually update the modeline
;; depending on which version of Emacs we're using.
;;
;; ``Consistency is the last refuge of the unimaginative'' 
;;		-- Oscar Wilde
;;
(defun zenirc-update-modeline ()
  (let ((version (emacs-version)))
    (cond ((string-match "Emacs 19" version)
	   (force-mode-line-update))
	  ((string-match "Xemacs" version)
	   (redraw-modeline))
	  (t
	   (redraw-display)))))

(defun zenirc-match-string (n &optional s)
  "Return string matched by last search.
N specifies the nth parenthesized expression in the last regexp.
N=0 means the entire text matched by the whole regexp or whole string.
S should be given if the last search was by `string-match' on string S.

Return value is nil if there is no Nth match."
  (and (match-beginning n)
       (if s
           (substring s (match-beginning n) (match-end n))
         (buffer-substring (match-beginning n) (match-end n)))))

(defun zenirc-string-match-list (msg regexp-list)
  (let ((match-data (match-data))
        (found nil))
    (while (and (not found) regexp-list)
      (setq found (string-match (car regexp-list) msg))
      (setq regexp-list (cdr regexp-list)))
    (or found
        (store-match-data match-data))
    found))

;; t if we are at the beginning of the input area
(defun zenirc-beginning-of-input-p (&optional proc)
  (or proc
      (setq proc (get-buffer-process (current-buffer))))
  ;; Note you can compare markers and positions safely; `=' looks at the
  ;; marker's position.
  (= (point) zenirc-process-mark))

;; t if point is in the "input area" (i.e. beyond the process mark)
(defun zenirc-in-input-p ()
  ;; Note you can compare markers and positions safely; `=' looks at the
  ;; marker's position.
  (>= (point) zenirc-process-mark))

;; return t if arg is a channel name, else nil
(defun zenirc-channel-p (arg)
  (memq (aref arg 0) '(?# ?& ?+)))

;; ignore processing - msg is a server message sent to the client.
;; Return non-nil if it is to be ignored, nil if it is not to be ignored.
(defun zenirc-ignore-p (msg)
  (zenirc-string-match-list msg zenirc-ignore-list))

;; Return t if both names are equivalent, ignoring differences in case.
;; This uses zenirc-downcase-name to handle weird chars.
;; 
;; This function is used to check whether a given nickname matches a
;; nickname fed to us by the server. As the function is used to check
;; this on recieved PRIVMSGs and NOTICEs we also need to check if the
;; sending side used one of the more obscure addressing schemes:
;;	nickname@servername
;;	username%hostname
;; As @ and % are illegal characters in a nickname, we can safely
;; check if n1 contains one of them to see if the recieved string
;; matches whatever the server thinks we are. --pp
;;
;; Because of the fact mentioned above we need to define what n1 and
;; n2 should be. I hereby declare that n1 is whatever the server has
;; fed us and n2 is our own given string. This only needs to be true
;; when the optional argument "recieving-privmsg" is set, but it
;; should be the standard way to address the function. --pp
(defun zenirc-names-equal-p (n1 n2 &optional recieving-privmsg)
  ;; I (Noah) have checked the emacs source code, and the size of strings
  ;; is stored in the Lisp_Object structure, so it can be referenced in
  ;; constant time.  Checking size first avoids the need for extra string
  ;; consing and regexp searching in zenirc-downcase-name if we know the
  ;; names can't possibly be equal.
  (or
   (and (= (length n1) (length n2))
	(string= (zenirc-downcase-name n1) (zenirc-downcase-name n2)))
   (and recieving-privmsg
	(or (string-match "@" n1)
	    (string-match "%" n1)))))
    
;  (and (= (length n1) (length n2))
;       (string= (zenirc-downcase-name n1) (zenirc-downcase-name n2))))

;; RFC1459 says that, because of IRC's scandanavian origin, the
;; characters {}| are considered to be the lower case equivalents of the
;; characters []\, respectively.  This is a critical issue when determining
;; the equivalence of two nicknames or channel names.
(defun zenirc-downcase-name (s)
  (setq s (downcase s))
  (let ((c '((?\[ . ?\{) (?\] . ?\}) (?\\ . ?\|)))
        (p 0))
    (save-match-data
      (while (string-match "[][\\]" s p)
        (aset s (match-beginning 0)
              (cdr (assq (aref s (match-beginning 0)) c)))
        (setq p (match-end 0)))))
  s)

;; determine if an event is worthy of a signal
(defun zenirc-signal-p (msg)
  (zenirc-string-match-list msg zenirc-signal-list))

;; returns nil if nick is actually a server name.
(defun zenirc-extract-nick (nickuserhost)
  (save-match-data
    (cond ((string-match "[!.]" nickuserhost)
           (if (= (aref nickuserhost (match-beginning 0)) ?.)
               nil
             (substring nickuserhost 0 (match-beginning 0))))
          (t nickuserhost))))

(defun zenirc-extract-userhost (nickuserhost)
  (save-match-data
    (and (string-match "!" nickuserhost)
         (substring nickuserhost (match-end 0)))))

(defun zenirc-extract-host (nickuserhost)
  (save-match-data
    (and (string-match "@" nickuserhost)
         (substring nickuserhost (match-end 0)))))

;; Parse a line into its constituent parts (words separated by
;; whitespace).  Return a list of the words.
(defun zenirc-parse-words (line)
  (let ((list '())
	(posn 0))
    (save-match-data
      (while (string-match "[^ \t\n]+" line posn)
	(setq list (cons (zenirc-match-string 0 line) list))
        (setq posn (match-end 0))))
    (nreverse list)))

;; Parse the first n words in line, returning a list consisting of each
;; word, plus any remaining portion of the string.
(defun zenirc-parse-n-words (n line)
  (let ((i 0)
        (len (length line))
        (posn 0)
        (result nil))
    (save-match-data
      (while (and (< i n)
                  (string-match "[^ \t\n]+" line posn))
        (setq result (cons (zenirc-match-string 0 line) result))
        (setq posn (match-end 0))
        (setq i (1+ i)))
      (and (string-match "[ \t\n]+" line posn)
           (setq posn (match-end 0)))
      (and (< posn len)
           (setq result (cons (substring line posn) result)))
      (nreverse result))))

;; parse a line into the first word and the rest.
;;
;; This returns ("word" . "rest"), where word means adjacent non-space
;; characters. Any amount of whitespace is skipped after the first word,
;; and "rest" is the rest of the line. If there is no "rest", a "rest"
;;  of "" is constructed.
(defun zenirc-parse-firstword (str)
  (let ((cell (cons nil nil)))
    (save-match-data
      (cond ((string-match "[^ \t\n]+" str)
	     (setcar cell (zenirc-match-string 0 str))
             (if (string-match "[^ \t\n]+" str (match-end 0))
                 (setcdr cell (substring str (match-beginning 0)))
               (setcdr cell ""))
             cell)))))

;; parse a server message into the zenirc-message-vector
;; the result looks like ["msgtype" "sender" "to" "arg1" ... "argn"]
(defun zenirc-parse-server-message (string)
  (save-match-data
    (let ((posn (if (eq (aref string 0) ?:)
                    (string-match " " string)
                  0))
          (msg zenirc-message-vector)
          (n 2))
      (fillarray msg nil)

      (aset msg 1 (if (eq posn 0)
                      (or zenirc-current-server-name zenirc-server)
                    (substring string 1 posn)))

      (aset msg 0 (let* ((bposn (string-match "[^ ]" string posn))
                         (eposn (string-match " " string bposn)))
                    (setq posn (and eposn
                                    (string-match "[^ ]" string eposn)))
                    (substring string bposn eposn)))

      (while (and posn
                  (not (eq (aref string posn) ?:)))
        (aset msg n (let* ((bposn posn)
                           (eposn (string-match " " string bposn)))
                      (setq posn (and eposn
                                      (string-match "[^ ]" string eposn)))
                      (substring string bposn eposn)))
        (setq n (1+ n)))
      (if posn
          (aset msg n (substring string (1+ posn))))
      msg)))

;; Try matching msg in regexp-list.
;; If no match is found, preserve old match data and return nil.
;; Otherwise, return value of sucessful string-match and leave modified
;; match-data intact.
;; do a signal (pop up buffer, beep, whatever)
(defun zenirc-signal (proc msg)
  (cond ((and proc-window
	      (pos-visible-in-window-p zenirc-process-mark proc-window)
	      (not (string-equal zenirc-beep-on-signal 'always))))
	(t
	 (and zenirc-beep-on-signal (ding t))
	 (zenirc-message nil 'signal (buffer-name)))))

(defun zenirc-message (proc-or-buffer string &rest args)
  (let ((proc nil)
        (buffer nil)
        (sym nil))

    (cond ((processp proc-or-buffer)
           (setq buffer (process-buffer proc-or-buffer))
           (setq proc proc-or-buffer))
          ((or (bufferp proc-or-buffer)
               (stringp proc-or-buffer))
           (setq buffer (get-buffer proc-or-buffer))
           (setq proc (get-buffer-process buffer))))

    (cond
     ((symbolp string)
      (setq sym string)
      (setq string (zenirc-lang-retrieve-catalog-entry string))))
    (and args
         (if string
             (setq string (apply 'format string args))
	   (setq string (format "[raw] %s" args))))
    (cond
     ((null proc-or-buffer)
      (message "%s" string))
     (t
      (setq string (concat string "\n"))
      (let ((orig-buffer (current-buffer))
            region-begin
            window
            window-point
            current-point-mark)
        (unwind-protect
            (progn
              (set-buffer buffer)
              (setq window (get-buffer-window buffer))
              (setq region-begin (marker-position zenirc-process-mark))
              (setq current-point-mark (point-marker))

              ;; If process mark is at window start, insert-before-markers
              ;; will insert text off-window since it's also inserting before
              ;; the start window mark.  Preserve window start's point in
              ;; that case.
              (and window
                   (= zenirc-process-mark (window-start window))
                   (setq window-point region-begin))

              (goto-char zenirc-process-mark)
              (insert-before-markers string)
              (goto-char region-begin)
              (while (search-forward "\C-m" zenirc-process-mark t)
                (delete-char -1))
              (and zenirc-message-hook
                   (save-restriction
                     (narrow-to-region region-begin zenirc-process-mark)
                     (zenirc-run-hook 'zenirc-message-hook proc sym string)))
              (goto-char current-point-mark)
              (and window-point
                   (set-window-start window window-point 'noforce)))
          (set-buffer orig-buffer)))))))

;; Insert the string "(sent to foo)" for arbitrary foo in the zenirc buffer.
;; This might be a useful thing to put in your zenirc-command-msg-hook.
;; `data' is either a simple string or a parsed message list.  The
;; structure of a partially-parsed message differs a little; the rest of
;; the string is directly in the cdr, instead of each substring being in
;; its own cons.  --friedman
(defun zenirc-display-recipient-confirmation (proc data &optional n)
  (or n (setq n 1))
  (let ((to (cond
             ((stringp data)
              data)
             ((and (consp data)
                   (consp (cdr data))
                   (> (length data) 1))
              (car (cdr data)))
             ((listp data)
              (car (zenirc-parse-firstword (cdr data))))
             ((signal 'wrong-type-argument (list 'string-or-list-p data))))))
    (if (string-equal 'message zenirc-send-confirmation)
 	(setq proc nil))
    (if (> n 1)
 	(zenirc-message proc 'send-multi to n)
      (zenirc-message proc 'send to))))

;; Handle a zenirc / command typed by the user.  Check to see if there's a
;; hook for the command and if so, execute the hook, otherwise just send the
;; command line unaltered to the server.
(defun zenirc-do-command (proc cmdline)
  (let* ((parsedcmd (zenirc-parse-firstword cmdline))
	 (cmdname (car parsedcmd))
         (hook-name (concat "zenirc-command-" cmdname "-hook"))
	 (hook (intern-soft hook-name)))
    (cond
     (zenirc-debug-commands
      (zenirc-message proc 'debug (concat "Hook: " hook-name))
      (zenirc-message proc 'debug
                      (concat "Parsed: " (prin1-to-string parsedcmd)))))
    ;; Call the hook, if it's bound and non-nil.
    ;; Otherwise, just send the unparsed command to the server.
    (if (and hook
             (boundp hook)
             (symbol-value hook))
        (zenirc-run-hook hook proc parsedcmd)
      (process-send-string proc (concat cmdline "\n")))))

(defun zenirc-send-line ()
  "Send current line to IRC server."
  (interactive)
  (cond
   ((zenirc-in-input-p)
    (end-of-line)
    (let* ((proc (get-buffer-process (current-buffer)))
           (input-start (copy-marker zenirc-process-mark))
	   (input-end (point))
           (string (buffer-substring input-start input-end))
           (posn 0))
      (if (= (point) (point-max))
          (insert "\n")
        ;; skip over line already present
        (goto-char (1+ (point))))
      (set-marker zenirc-process-mark (point))
      (zenirc-timer-handler proc)
      (save-match-data
        (cond
         ;; Ignore lines composed only of whitespace
         ((not (string-match "\\`\\s-*\\'" string))
          ;; convert newlines in input to spaces (decimal ascii 32)
          (while (string-match "\n" string posn)
            (aset string (match-beginning 0) 32)
            (setq posn (match-end 0)))
	  ;; Remove preceding whitespaces, if user wants us to.
	  (if (and zenirc-delete-preceding-whitespaces
		   (string-match (concat "^\\( \\)*"
					 (char-to-string zenirc-command-char))
				 string))
	      (setq string 
		    (substring string (string-match "[^ ]" string))))
          ;; Run this hook after string has been formatted, but before
          ;; invoking any hooks since they may do unpredictable things like
          ;; change the current buffer.
          (zenirc-run-hook 'zenirc-send-line-hook
                           input-start input-end string)
          (cond ((= (aref string 0) zenirc-command-char)
                 (zenirc-do-command proc (substring string 1)))
                ((string= zenirc-current-victim nil)
                 (zenirc-message proc 'nosend))
                (t
                 (let ((n (zenirc-send-multi-line
                           proc
                           (concat "PRIVMSG " zenirc-current-victim)
                           string)))
 		   (zenirc-run-hook 'zenirc-send-confirmation-privmsg-hook
 				    proc 'send-multi 'send
 				    zenirc-current-victim n)))))))))
   (t
    (goto-char (point-max)))))

;; Returns the number of chunks required to send the message
(defun zenirc-send-multi-line (proc prefix string)
  (let* ((maxlen (- zenirc-message-length-limit
                    (length prefix)
                    ;; when the server sends your message, it prepends a
                    ;; string of the form ":nick!user@host "
                    ;; Plus we add a newline and a separator ourselves.
                    (length zenirc-nick)
                    (length zenirc-user-login-name)
                    (length (system-name))
                    ;;(length ": !@ :\r\n")
                    8))
         (strlen (length string))
         (posn 0)
         (n 0)
         (count 0))
    (while (< posn strlen)
      (setq n (min maxlen (- strlen posn)))
      (process-send-string
         proc (concat prefix " :" (substring string posn (+ posn n)) "\r\n"))
      (setq posn (+ posn n))
      (setq count (1+ count)))
    count))

;; This is the default action for most zenirc-send-confirmation-FOO-hooks.
(defun zenirc-send-confirmation-generic (proc multi single to n)
  (if zenirc-send-confirmation
      (progn
	(and (string-equal 'message zenirc-send-confirmation)
	     (setq proc nil))
	(let ((msg (if (> n 1) multi single)))
	  (zenirc-message proc msg to n)))))

;; Delete a cell from a list, case-insensitively.
(defun zenirc-delete-case-insensitive (elt list)
  "Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `equal'.
If the first member of LIST is ELT, deleting it is not a side effect;
it is simply using a different list.
Therefore, write `(setq foo (delete element foo))'
to be sure of changing the value of `foo'.

This function compares things case-insensitively (according to RFC1459)."
  (let ((p list)
        (l (cdr list)))
    (while l
      (if (equal (zenirc-downcase-name elt) (zenirc-downcase-name (car l)))
          (setcdr p (cdr l))
        (setq p (cdr p)))
      (setq l (cdr l))))
  (if (equal (zenirc-downcase-name elt) (zenirc-downcase-name (car list)))
      (cdr list)
    list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ZenIRC message catalogs
;;;
;;;      "Hello, hello, obarrayter?"
;;;      "This is the obarrayter."
;;;      "Could you please call an internist?"
;;;      "What's the problem, sir?"
;;;      "Oh, just some bad hash.  But I'd like to consult a professional
;;;       just to be safe."
;;;      "I understand, sir."
;;;      ...dum dee dum...
;;;      'Tis a gift to be symbol
;;;      'tis a gift to be freed...
;;;                -- Karl Fogel
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar zenirc-lang-catalogs (make-vector 13 0)
  "Obarray used to store message for all languages, indexed by symbol.")

;; 211 buckets should be more than enough for message catalogs (remember to
;; use a prime number to get good hashing characteristics).
;; This is not the total number of messages you can store, but just the number
;; of "buckets" in which they can go.  Even if the catalog eventually
;; contains more entries than this, it isn't really necessary to increase
;; the size of this table.  Note that right now, there are about 175
;; messages in the english catalog with a few extra libraries loaded.
(defconst zenirc-lang-obarray-size 211
  "*The default hash table size for newly created message catalogs")

(defvar zenirc-lang-current-language 'english
  "Current language in use in zenirc.")

;; This works on existing catalogs, but will overwrite any entry already in
;; the catalog.
(defun zenirc-lang-define-catalog (lang alist)
  (let* ((catalog-name (if (stringp lang)
                           lang
                         (symbol-name lang)))
         (catalog-sym (intern catalog-name zenirc-lang-catalogs))
         catalog)

    (or (boundp catalog-sym)
        (set catalog-sym (make-vector zenirc-lang-obarray-size 0)))
    (setq catalog (symbol-value catalog-sym))

    (while alist
      (set (intern (symbol-name (car (car alist))) catalog) (cdr (car alist)))
      (setq alist (cdr alist)))))

;; This creates a new catalog if none exists for the language specified.
;; It is more efficient to use zenirc-lang-define-catalog if defining many
;; entries at once.
(defun zenirc-lang-store-catalog-entry (sym str lang)
  (or lang (setq lang zenirc-lang-current-language))
  (let* ((catalog-name (if (stringp lang)
                           lang
                         (symbol-name lang)))
         (sym-name (if (stringp sym)
                       sym
                     (symbol-name sym)))
         (catalog-sym (intern catalog-name zenirc-lang-catalogs)))
    (or (boundp catalog-sym)
        (set catalog-sym (make-vector zenirc-lang-obarray-size 0)))
    (set (intern sym-name (symbol-value catalog-sym)) str)))

;; This returns nil for any undefined entry type, or if there is no
;; catalog for the language specified.
(defun zenirc-lang-retrieve-catalog-entry (sym &optional lang)
  (if (not lang) (setq lang zenirc-lang-current-language))
  (or (zenirc-lang-retrieve-catalog-entry-1 sym lang)
      ;; For now, if a message entry isn't defined for the
      ;; current language, default to english.  There are
      ;; many new message types and the other catalogs
      ;; aren't completely up to date.
      (and (not (string-equal lang 'english))
 	   (zenirc-lang-retrieve-catalog-entry-1 sym 'english))))

(defun zenirc-lang-retrieve-catalog-entry-1 (sym lang)
  (or lang (setq lang zenirc-lang-current-language))
  (let* ((catalog-name (if (stringp lang)
                           lang
                         (symbol-name lang)))
         (catalog-sym (intern-soft catalog-name zenirc-lang-catalogs))
         catalog
         (sym-name (if (stringp sym)
                       sym
                     (symbol-name sym)))
         msg-sym)
    (cond ((or (null catalog-sym)
               (not (boundp catalog-sym)))
           nil)
          (t
           (setq catalog (symbol-value catalog-sym))
           (setq msg-sym (intern-soft sym-name catalog))
           (and msg-sym
                (boundp msg-sym)
                (symbol-value msg-sym))))))

;; If called interactively and language is undefined, signal an error.
(defun zenirc-lang-set-current-language (lang)
  (interactive (list (completing-read "Switch to language: "
                                      zenirc-lang-catalogs 'boundp t)))
  (let* ((name (if (stringp lang)
                   lang
                 (symbol-name lang)))
         (catalog (intern-soft name zenirc-lang-catalogs)))

    (cond ((and catalog (boundp catalog))
           ;; Set the current language to a symbol interned in the global
           ;; obarray.  This makes it more convenient to compare against
           ;; other symbols with eq.
           (setq zenirc-lang-current-language (intern name))
           (zenirc-message (current-buffer) 'newcatalog name))
          (t
           (zenirc-message (current-buffer) 'nocatalog name)
           nil))))


;; English is the default catalog.  Other catalogs are available in
;; separate files.
(defun zenirc-lang-define-english-catalog ()
  (zenirc-lang-define-catalog 'english
    '((s001 . "[info] You are wasting time.")
      (s002 . "[info] Your IRC server is %s running ircd version %s")
      (s003 . "[info] This server was created %s")
      (s200 . "[info] %s (%s) Link -> %s") ; Version reply from /trace
      (s201 . "[info] %s Try  -> %s")
      (s202 . "[info] %s H.S. -> %s")
      (s203 . "[info] %s Hmmm -> IP address: %s") ; Unknown connection
      (s204 . "[info] %s Oper -> %s") ; Operator connection
      (s205 . "[info] %s User -> %s") ; User connection
      (s206 . "[info] %s Serv -> %s %s %s %s ") ; Server connection
      (s208 . "[info] %s %s -> %s") ; New type connection
      (s209 . "[info] %s Clas -> %s = %s") ; What the classes means
      (s211 . "[info] %s link up %s sec\nSent: %s/%s, Rcvd: %s/%s, SendQ: %s")
      (s212 . "[info] %s\t->\ttimes: %s\tbytes: %s") ; Command stats
      (s213 . "[info] C hst/nme/prt/cls: %s/%s/%s/%s")      ; C-lines
      (s214 . "[info] N hst/nme/prt/cls: %s/%s/%s/%s") ; N-lines
      (s215 . "[info] %s host/name/class:\t%s/%s/%s") ; I-lines
      (s216 . "[info] K host/username:\t%s/%s") ; K-lines
      (s217 . "[info] Q %s/%s/%s/%s/%s") ; Q-lines
      (s218 . "[info] Class: %s Ping freq: %s Conn.freq: %s Max Links: %s Sendq: %s") ; Y-lines
      (s219 . "[info] End of /stats.")
      (s221 . "[info] Your current user mode is: %s")
      (s241 . "[info] LEAF hostmask/depth:\t\t%s/%s") ; L-lines
      (s242 . "[info] %s") ; Uptime of server
      ;; O-lines and o-lines; the latter are for local ops
      (s243 . "[info] %s nickname/user@host:\t%s/%s")
      (s244 . "[info] HUB  hostmask/servername:\t%s/%s") ; H-lines
      (s249 . "[info] %s; %s") ; /stats Z info.
      (s251 . "[info] There are %s/%s visible/invisible users on %s servers.")
      (s251-29 . "[info] There are %s users and %s services on %s servers.")
      (s252 . "[info] There are %s major dweebs online.")
      (s253 . "[info] There are %s unknown connections.")
      (s254 . "[info] There are %s channels")
      (s255 . "[info] There are %s clients and %s servers connected to this server")
      (s255-29 . "[info] There are %s clients, %s services and %s servers connected to this server")
      (s256 . "[info] Administrative information for %s:") ; /admin line 1
      (s257 . "[info] %s") ; /admin line 2
      (s258 . "[info] %s") ; /admin line 3
      (s259 . "[info] %s") ; /admin line 4
      (s261 . "[info] %s File -> %s %s") ; Logfile trace
      (s262 . "[info] %s Vers -> %s")
      (s301 . "[info] %s is away: %s")
      (s302 . "[info] userhost: %s") ; userhost reply
      (s303 . "[info] Currently wasting time: %s") ; ison reply
      (s305 . "[info] You are no longer away")
      (s306 . "[info] You are away")
      (s311 . "[info] %s (%s@%s) is %s") ; user part of /whois list
      (s312 . "[info] %s iswas using server %s (%s)")
      (s313 . "[info] %s is a major dweeb.") ; /whois operator status
      (s314 . "[info] %s (%s@%s) was %s") ; user part of /whowas list
      (s315 . "[info] End of /who.")
      (s317 . "[info] %s has been idle %s") ; /whois idle time
      (s318 . "[info] End of /whois.")
      (s319 . "[info] %s is on: %s") ; channel part of whois data
      (s321 . "[info] Channel         Users Topic") ; header for LIST cmd
      (s322 . "[info] %-15s %-5s %s")  ; each channel in LIST cmd
      (s323 . "[info] End of /list.")  ; trailer for LIST cmd
      (s324 . "[info] Mode for %s is %s %s") ; channel mode
      (s331 . "[info] %s has no topic") ; no topic message
      (s332 . "[info] %s topic: %s")   ; topic message
      (s333 . "[info] %s topic set by %s at %s") ; topic set time
      (s341 . "[info] You are inviting %s to %s") ; invite reply
      (s342 . "[info] You are asking %s to waste time") ; summon reply
      (s351 . "[info] Version: %s %s %s") ; version reply
      (s352_header . "[info] Nickname  Stat Name of Channel User@host (Hop count  Name)") ; header for /who list reply
      (s352 . "[info] %-9s %-3s  %-15s %s@%s (%s)") ; /who list reply
      (s353 . "[info] Users on %s: %s") ; displayed after channel join
      (s364 . "[info] %s %s %s")       ; /links reply
      (s365 . "[info] end of /links")  ; end of /links reply
      (s367 . "[info] %s ban %s")      ; banlist reply
      (s368 . "[info] end of banlist") ; end of banlist reply
      (s371 . "[info] %s")             ; info reply
      (s372 . "[motd] %s")		; message of the day
      (s375 . "[motd] Message Of The Day:") ; start of motd
      (s376 . "[motd] End of motd")    ; displayed at end of motd
      (s381 . "[info] You are now a major dweeb") ; irc op status
      (s382 . "[info] Rehashing: %s")  ; rehash server msg
      (s391 . "[info] Time for server %s: %s") ; TIME reply
      (s392 . "[info] Userid   Terminal  Host") ; header for users rpl
      (s393 . "[info] %s")             ; body of users rpl
      (s395 . "[info] Nobody logged on") ; nobody for users rpl
      (s401 . "[info] No such nick/channel: %s") ; there is no such nick/chan
      (s402 . "[info] No such nick/server: %s") ; there is no such server
      (s403 . "[info] No such channel: %s") ; there is no such channel
      (s404 . "[info] You cannot send to %s.") ; you can't send to channel
      (s405 . "[info] Too many channels: %s") ; too many channels
      (s406 . "[info] Server has no record of nickname: %s") ; no whowas data
      (s407 . "[info] Duplicate recipients. No message sent: %s") ; user@host
      (s409 . "[info] No origin specified.") ; ping error reply
      (s411 . "[info] No recipient given.") ; no recipient given
      (s412 . "[info] No text to send.") ; you didn't send anything.
      (s413 . "[info] No toplevel domain: %s") ; no toplevel domain spec
      (s414 . "[info] Wildcard in toplevel domain: %s") ; wild toplevel
      (s415 . "[info] Bad server/host mask: %s") ; wild toplevel
      (s421 . "[info] This looks like spam to me: %s") ; you sent server spam
      (s422 . "[info] No motd (flame major dweeb listed in /admin)")
      (s423 . "[info] No admin info.  Ignorant major dweeb running server.")
      (s431 . "[info] No nickname given") ; you didn't provide a nick
      (s432 . "[info] Invalid nickname: %s")
      (s433 . "[info] Nickname already in use: %s")
      (s436 . "[info] Nick collision kill: %s")
      (s437 . "[info] Nick/channel temporarily unavailable: %s")
      (s441 . "[info] %s is not on %s") ; can't do it to those not present
      (s442 . "[info] You are not on %s.") ; you can't do that dave.
      (s443 . "[info] %s is already on channel %s.") ; invite error
      (s444 . "[info] %s is not logged in") ; SUMMON reply
      (s445 . "[info] Some major dweeb won't let you do summon")
      (s446 . "[info] Some major dweeb won't let you do /users")
      (s451 . "[info] You have not registered") ; gotta do the USER NICK thing
      (s461 . "[info] Not enough parameters: %s") ; as 421
      (s462 . "[info] You may not reregister") ; cannot USER twice
      (s463 . "[info] Some fascist major dweeb will not let you connect")
      (s464 . "[info] Password is incorrect") ; bad PASS command
      (s465 . "[info] You are not allowed to use this server.") ; creep
      (s467 . "[info] Key for %s is already set.") ; chan key set already
      (s471 . "[info] Cannot join %s (user limit reached).") ; too many ppl
      (s472 . "[info] %s is an unknown mode character.") ; duh
      (s473 . "[info] Cannot join %s (invite only).") ; fascist nerds
      (s474 . "[info] Cannot join %s (ban).") ; you're banned
      (s475 . "[info] Cannot join %s (channel key).") ; bad key
      (s477 . "[info] Channel %s doesn't support modes.")
      (s481 . "[info] You are not a big enough dweeb to do that.") ; oper only
      (s482 . "[info] You are not a powermonger for %s.") ; chanop needed
      (s483 . "[info] Duh.  You cannot kill a server") ; can't kill a server
      (s491 . "[info] No major dweebs allowed from your host") ; no o-line
      (s501 . "[info] Unknown user mode flag") ; you did something silly
      (s502 . "[info] Cannot change mode for other users") ; as above
      (action . "(sent to %s)") ; ctcp action sent
      (connect-failed . "[error] Couldn't connect to %s port %d, reason: %s")
      (connect-try . "[info] Connecting to %s port %d...")
      (connect-abort . "[info] Aborted attempt to connect to an irc server.")
      (ctcp_action . "[action->%s] %s %s") ; ctcp ACTION display
      (ctcp_action_nochannel . "[action] %s %s")
      (ctcp_clientinfo . "[query] CLIENTINFO from %s to %s")
      (ctcp_echo . "[query] ECHO from %s to %s containing: %s")
      (ctcp_errmsg . "[query] ERRMSG from %s to %s")
      (ctcp_finger . "[query] FINGER from %s to %s")
      (ctcp_ping . "[query] PING from %s to %s")
      (ctcp_ping_reply . "[reply] PING: %s is %s seconds away")
      (ctcp_source . "[query] SOURCE from %s to %s")
      (ctcp_time . "[query] TIME from %s to %s")
      (ctcp_userinfo . "[query] USERINFO from %s to %s")
      (ctcp_version . "[query] VERSION from %s to %s")
      (debug  . "[debug] %s")          ; displayed by debugging code
      (error . "[%s] %s")              ; server error message
      (invite . "[info] %s invites you to %s.") ; invite
      (join_you . "[info] Joining channel: %s")
      (join . "[info] %s has joined %s")
      (join_mode . "[info] %s joined %s (+%s).")
      (kick . "[info] %s has been kicked from %s by %s") ; someone was peeved
      (kick_you . "[info] You have been kicked from %s by %s") ; loser
      (kill . "[info] You have been killed: %s") ; your time is up.
      (mode . "[info] %s has changed mode for %s: %s") ; MODE change
      (nick . "[info] %s has changed nick to %s") ; nick change
      (newcatalog . "[info] Current message catalog set to %s")
      (nocatalog . "[error] No message catalog defined for %s")
      (nosend . "[info] you have no current victim to send to") ; msg not sent
      (notice . "{%s%s} %s")           ; NOTICE
      (notice_nochannel . "{%s} %s")           ; NOTICE
      (notice_you . "{%s} %s")         ; NOTICE sent to your nick
      (now_querying . "[info] Current victim is %s.") ; /query foo
      (part_you . "[info] Leaving: %s (%s)") ; your part from channel message
      (part . "[info] %s has left %s (%s)") ; part from channel message
      (pong . "[info] %s says ojnk.")  ; pong message from server
      (privmsg . "<%s%s> %s")          ; PRIVMSG
      (privmsg_nochannel . "<%s> %s")          ; PRIVMSG
      (privmsg_you . "*%s* %s")        ; PRIVMSG sent to your nick
      (protocol_violation . "[error] The following line is in violation of the IRC protocol.\n[error] Please tell the server administrator:\n%s: %s")
      (query . "[query] from %s to %s content %s") ; ctcp query
      (query_unknown . "is an unknown CTCP query")
      (query_unbalanced . "[UNBALANCED query] from %s to %s content %s")
      (query_unbalanced_reply . "is an unbalanced CTCP query")
      (quit . "[info] %s stopped wasting time: %s") ; user signoff
      (reply . "[reply] from %s to %s content %s") ; ctcp reply
      (reply_unbalanced . "[UNBALANCED reply] from %s to %s content %s")
      (send . "(sent to %s)") ; you sent a message/notice
      (send-multi . "(sent to %s in %d parts)") ; sent a long message/notice
      (sentinel . "\nZenIRC ended at %s") ; process sentinel message
      (server . "[server] %s")         ; unknown server message
      (signal . "[signal in %s]")        ; signal in echo area
      (topic . "[info] %s changed the topic on %s to: %s") ; topic message
      (wallops . "-%s- %s")            ; WALLOPS notice
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ZenIRC hook handling functions
;;;
;;; ZenIRC uses a somewhat nonstandard hook mechanism. Hook symbols
;;; are manipulated with zenirc-add-hook and zenirc-delete-hook, and
;;; are executed with zenirc-run-hook. A hook symbol is a list of
;;; symbols that are function names. When a hook is run with
;;; zenirc-run-hook, each symbol in the list is run in turn - unless
;;; one of the hooks sets the variable zenirc-run-next-hook to nil. In
;;; this case, zenirc-run-hook immediatelly returns to the caller.
;;; Unlike emacs 19 hooks, ZenIRC hooks are called with arguments.
;;; ZenIRC hooks return the value of the last hook run.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zenirc-run-hook (hooksym &rest args)
  "Take hook name HOOKSYM and run it, passing optional args ARGS.
HOOKSYM should be a symbol, a hook variable.
If the hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with args ARGS.
If it is a list, the elements are called, in order, with ARGS, if
zenirc-run-next-hook is t (the default). Otherwise, the hooks after
the one that set zenirc-run-next-hook are not called, and control is
returned to the caller. (zenirc-run-hook) returns the value returned
from the last hook run."
      (let ((zenirc-run-next-hook t)
            (result))
        (and (boundp hooksym)
             (symbol-value hooksym)
             (let ((value (symbol-value hooksym)))
               (if (and (listp value)
                        (not (eq (car value) 'lambda)))
                   (while (and value zenirc-run-next-hook)
                     (setq result (apply (car value) args))
                     (setq value (cdr value)))
                 (setq result (apply value args)))))
        result))

(defun zenirc-add-hook (hook function &optional append)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (or (boundp hook) (set hook nil))
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value hook)))
    (if (or (not (listp old)) (eq (car old) 'lambda))
	(set hook (list old))))
  (or (if (consp function)
	  (member function (symbol-value hook))
	(memq function (symbol-value hook)))
      (set hook
	   (if append
	       (nconc (symbol-value hook) (list function))
	     (cons function (symbol-value hook))))))

(defun zenirc-remove-hook (hook function)
  "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `add-hook'."
  (if (or (not (boundp hook))		;unbound symbol, or
	  (null (symbol-value hook))	;value is nil, or
	  (null function))		;function is nil, then
      nil				;Do nothing.
    (let ((hook-value (symbol-value hook)))
      (if (consp hook-value)
	  (setq hook-value (delete function hook-value))
	(if (equal hook-value function)
	    (setq hook-value nil)))
      (set hook hook-value))))

(fset 'zenirc-delete-hook 'zenirc-remove-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ZenIRC time handling functions
;;;
;;; These functions are used to implement time handling in ZenIRC.
;;; Much of this code was lifted from the Kiwi 4.30 irc client.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zenirc-time-to-int (timestr)
  "Convert from time in string format as returned by current-time-string
to a double integer format, as returned by file-attributes.

Written by Stephen Ma <ma_s@maths.su.oz.au>"
  (let* ((norm+ '(lambda (num1 num2)
		  (let ((sumh (+ (car num1) (car num2)))
			(suml (+ (car (cdr num1)) (car (cdr num2)))))
		    (list (+ sumh (/ suml 65536)) (% suml 65536)))))
	 (norm* '(lambda (num1 num2)
		  (let ((prodh (* num1 (car num2)))
			(prodl (* num1 (car (cdr num2)))))
		    (list (+ prodh (/ prodl 65536)) (% prodl 65536)))))
	 (seconds (string-to-int (substring timestr 17 19)))
	 (minutes (string-to-int (substring timestr 14 16)))
	 (hours (string-to-int (substring timestr 11 13)))
	 (partdays (1- (string-to-int (substring timestr 8 10))))
	 (years (string-to-int (substring timestr 20 24)))
	 (days (+ partdays
		  (cond ((and (= (% years 4) 0)
			      (/= (% years 100) 0))
			 (cdr (assoc (substring timestr 4 7)
				     '(("Jan" . 0)
				       ("Feb" . 31)
				       ("Mar" . 60)
				       ("Apr" . 91)
				       ("May" . 121)
				       ("Jun" . 152)
				       ("Jul" . 182)
				       ("Aug" . 213)
				       ("Sep" . 244)
				       ("Oct" . 274)
				       ("Nov" . 305)
				       ("Dec" . 335)))))
			(t (cdr (assoc (substring timestr 4 7)
				       '(("Jan" . 0)
					 ("Feb" . 31)
					 ("Mar" . 59)
					 ("Apr" . 90)
					 ("May" . 120)
					 ("Jun" . 151)
					 ("Jul" . 181)
					 ("Aug" . 212)
					 ("Sep" . 243)
					 ("Oct" . 273)
					 ("Nov" . 304)
					 ("Dec" . 334))))))
		  (* (- years 1970) 365)
		  (/ (- years 1969) 4)
		  (- (/ (- years 1901) 100)))))
    (funcall norm+
	     (funcall norm*
		      60
		      (funcall norm+
			       (funcall norm*
					60
					(funcall norm+
						 (funcall norm*
							  24
							  (list 0 days))
						 (list 0 hours)))
			       (list 0 minutes)))
	     (list 0 seconds))))

(defun zenirc-time= (a b)
  "Compare two times, and return true if they are equal."
  (and (= (nth 0 a) (nth 0 b))
       (= (nth 1 a) (nth 1 b))))

(defun zenirc-time< (a b)
  "Compare two times, and return t if the first is earlier than the second."
  (or (< (nth 0 a) (nth 0 b))
      (and (= (nth 0 a) (nth 0 b))
	   (< (nth 1 a) (nth 1 b)))))

(defun zenirc-time-diff (a b)
  "Return the difference between two times. This function requires
the second argument to be earlier in time than the first argument."
  (cond ((= (nth 0 a) (nth 0 b)) (list 0 (- (nth 1 a) (nth 1  b))))
	((> (nth 1 b) (nth 1 a)) (list (- (nth 0 a) (nth 0 b) 1)
				       (- (+ 65536 (nth 1 a)) (nth 1 b))))
	(t (list (- (nth 0 a) (nth 0 b))
		 (- (nth 1 a) (nth 1 b))))))

;; Convert a number of seconds since the epoch (in ASCII) into an
;; ASCII string representing the time.
(defun zenirc-epoch-seconds-to-time (seconds)
  (save-match-data
    (let (millions units high low)
      (if (string-match "^\\(.*\\)\\(......\\)$" seconds)
          (setq millions (string-to-int (substring seconds
                                                   (match-beginning 1)
                                                   (match-end 1)))
                units (string-to-int (substring seconds
                                                (match-beginning 2)
                                                (match-end 2))))
        (setq millions 0
              units (string-to-int seconds)))
      (setq high (+ (* millions 15) (/ (* millions 265) 1024) (/ units 65536))
            low (+ (% (+ (* (% millions 4) 16384) (* millions 576)) 65536)
                   (% units 65536)))
      (if (> low 65535)
          (setq low (- low 65536)
                high (1+ high)))
      (list high low))))

(defun zenirc-timer-handler (proc)
  "Call zenirc-timer-hook as often as possible. The maximum delay between
calls of zenirc-timer-hook is how often a server pings the client."
  (let ((now (zenirc-time-to-int (current-time-string))))
    (if (zenirc-time< '(0 0) (zenirc-time-diff now zenirc-time-last-event))
	(progn
	  (and zenirc-debug-timer
               (zenirc-message proc
                               "[debug] timer: %s\n" (current-time-string)))
	  (zenirc-run-hook 'zenirc-timer-hook proc now)
	  (setq zenirc-time-last-event now)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; command handling subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zenirc-insert-at-proc-mark (&rest args)
    (cond ((= (point) zenirc-process-mark)
           (apply 'insert args))
          (t
           (let ((point (point-marker)))
             (goto-char zenirc-process-mark)
             (apply 'insert args)
             (goto-char point)))))

(defun zenirc-send-privmsg-last-rec ()
  (interactive)
  (zenirc-insert-at-proc-mark (concat (char-to-string zenirc-command-char) 
				      "msg ") zenirc-privmsg-last-rec " "))

(defun zenirc-send-privmsg-last-sent ()
  (interactive)
  (zenirc-insert-at-proc-mark (concat (char-to-string zenirc-command-char) 
				      "msg ") zenirc-privmsg-last-sent " "))

(defun zenirc-self-insert-or-send-privmsg-last-rec ()
  (interactive)
  (if (zenirc-beginning-of-input-p)
      (zenirc-send-privmsg-last-rec)
    (if (string-match "Xemacs" emacs-version)
	(insert last-command-char)
      (insert (this-command-keys)))))

(defun zenirc-self-insert-or-send-privmsg-last-sent ()
  (interactive)
  (if (zenirc-beginning-of-input-p)
      (zenirc-send-privmsg-last-sent)
    (if (string-match "Xemacs" emacs-version)
	(insert last-command-char)
      (insert (this-command-keys)))))

;; change zenirc-current-victim by toggling between all channels in
;; zenirc-channel-list, original code by <vuori@sci.fi>
(defun zenirc-toggle-channel ()
  (interactive)
  (let ((list zenirc-channel-list)
	(orig-victim zenirc-current-victim))
    (while list
      (if (string-equal (zenirc-downcase-name zenirc-current-victim) 
			(car list))
	  ; this is the current victim cell of the list
	  (if (cdr list)
	      ; it's not the last cell
	      (setq zenirc-current-victim (or (car (cdr list))
					      zenirc-current-victim)
		    list nil)
	    ; it is the last cell, use the first cell
	    (setq zenirc-current-victim (car zenirc-channel-list))))
      (setq list (cdr list)))
    ; if the victim wasn't in the zenirc-channel-list, default to
    ; first cell of zenirc-channel-list
    (if (string-equal orig-victim zenirc-current-victim)
	(setq zenirc-current-victim (car zenirc-channel-list)))
    (zenirc-update-modeline)))
    
;; /action victim text
;; send a ctcp action to the specified victim
(defun zenirc-command-action (proc parsed)
  (let* ((l (zenirc-parse-firstword (cdr parsed)))
         (n (zenirc-send-multi-line proc
                                   (concat "PRIVMSG " (car l))
                                   (concat "\^AACTION " (cdr l) "\^A"))))
    (zenirc-run-hook 'zenirc-send-confirmation-me-hook
		     proc 'send-multi 'send (car l) n)))

;; /away [message]
;; set your away message (or remove it if not present)
(defun zenirc-command-away (proc parsedcmd)
  (process-send-string proc (concat "AWAY :" (cdr parsedcmd) "\n")))

;; /command-char command-char
;; No string does nothing.
(defun zenirc-command-command-char (proc parsedcmd)
  (if (not (string= "" (cdr parsedcmd)))
	   (setq zenirc-command-char (string-to-char (cdr parsedcmd)))))

;; /ctcp victim query [text]
;; does the ^A ctcp things and uppercases the argument.
(defun zenirc-command-ctcp (proc parsedcmd)
  (let* ((parsedarg (zenirc-parse-firstword (cdr parsedcmd)))
	 (argument (zenirc-parse-firstword (cdr parsedarg))))
    (process-send-string
     proc
     (concat "PRIVMSG "
	     (car parsedarg)
	     " :\C-a"
	     (upcase (car argument))
	     (if (not (string-equal "" (cdr argument)))
		 " ")
	     (cdr argument)
	     "\C-a\n"))))

;; /kick nick #channel [reason]
(defun zenirc-command-kick (proc parsedcmd)
  (let* ((l (zenirc-parse-n-words 2 (cdr parsedcmd)))
         (n (length l))
         (v [nil nil "KICK %s %s\n" "KICK %s %s :%s\n"]))
    (cond ((< n 2)
           ;; Invalid format for command
           (zenirc-message proc 's421
                           (format "/%s %s" (car parsedcmd) (cdr parsedcmd))))
          (t
           (process-send-string proc (apply 'format (aref v n) l))))))

;; /kill nick reason
(defun zenirc-command-kill (proc parsedcmd)
  (let* ((l (zenirc-parse-n-words 1 (cdr parsedcmd)))
         (n (length l))
         (v [nil "KILL %s\n" "KILL %s :%s\n"]))
    (cond ((< n 1)
           ;; Invalid format for command
           (zenirc-message proc 's421
                           (format "/%s %s" (car parsedcmd) (cdr parsedcmd))))
          (t
           (process-send-string proc (apply 'format (aref v n) l))))))

;; /language [lang]
;; switches message catalogs
;; note that you must have the catalog in question loaded already, before
;; you can switch to it.
(defun zenirc-command-language (proc parsedcmd)
  (let ((lang (car (zenirc-parse-firstword (cdr parsedcmd)))))
    (zenirc-lang-set-current-language lang)))

;; /m victim message
;; send a message to someone who is not the current victim
(defun zenirc-command-m (proc parsedmsg)
  (zenirc-command-privmsg proc parsedmsg))

;; /me message
;; send a ctcp action to the current victim
(defun zenirc-command-me (proc parsed)
  (let ((n (zenirc-send-multi-line proc
                                   (concat "PRIVMSG " zenirc-current-victim)
                                   (concat "\^AACTION " (cdr parsed) "\^A"))))
    (zenirc-run-hook 'zenirc-send-confirmation-me-hook
 		     proc 'send-multi 'send
 		     zenirc-current-victim n)))

;; /msg victim message
;; send a message to someone who is not the current victim
(defun zenirc-command-msg (proc parsedmsg)
  (zenirc-command-privmsg proc parsedmsg))

;; /oper handler
;;
;; Always remember, the lame deserve to lose.
;;
;; I did not add this, but neither do I have any intention of removing it.
;; --friedman
;; Neither have I, removing this would be like removing the spirit of ZenIRC.
;; --pp
(defun zenirc-command-oper (proc parsedmsg)
  (process-send-string proc (concat "QUIT :" (cdr parsedmsg) "\n")))

;; /notice nick message
(defun zenirc-command-notice (proc parsedmsg)
  (let* ((pair (zenirc-parse-firstword (cdr parsedmsg)))
         (n (zenirc-send-multi-line proc
                                    (concat "NOTICE " (car pair))
                                    (cdr pair))))
    (setq zenirc-privmsg-last-sent (car pair))
    (zenirc-run-hook 'zenirc-send-confirmation-notice-hook
 		     proc 'send-multi 'send (car pair) n)))

;; /part channel [message]
;; exit channel, displaying optional message
(defun zenirc-command-part (proc parsedcmd)
  (let* ((parsedtext (zenirc-parse-firstword (cdr parsedcmd))))
    (if (not (string= (cdr parsedtext) ""))
	(process-send-string proc (concat "PART " (car parsedtext)
					  " :" (cdr parsedtext) "\n"))
      (process-send-string proc (concat 
				 "PART " (car parsedtext) 
				 " :Started wasting time elsewhere\n")))))

;; /ping victim
;; TODO: Rewrite this code.
(defun zenirc-command-ping (proc parsedmsg)
  (process-send-string
   proc
   (concat "PRIVMSG " (cdr parsedmsg) " :\C-aPING "
	   (car (cdr (zenirc-time-to-int (current-time-string)))) "\C-a\n")))

;; /privmsg victim message
(defun zenirc-command-privmsg (proc parsedmsg)
  (let* ((pair (zenirc-parse-firstword (cdr parsedmsg)))
         (n (zenirc-send-multi-line proc
                                    (concat "PRIVMSG " (car pair))
                                    (cdr pair))))
    (setq zenirc-privmsg-last-sent (car pair))
    (zenirc-run-hook 'zenirc-send-confirmation-privmsg-hook
 		     proc 'send-multi 'send (car pair) n)))

;; /query [victim]
;; If we gave it an argument, set zenirc-current-victim to that arg.
;; If not, just display what zenirc-current-victim is.
(defun zenirc-command-query (proc parsedmsg)
  (if (not (string= (cdr parsedmsg) ""))
      (setq zenirc-current-victim
            (car (zenirc-parse-firstword (cdr parsedmsg))))
    ;; no arguments, just display who we're querying.
    (zenirc-message proc 'now_querying zenirc-current-victim))
  (zenirc-update-modeline))

;; /quit [message]
;; exit irc, displaying optional message
(defun zenirc-command-quit (proc parsedcmd)
  (if (string= "" (cdr parsedcmd))
      (process-send-string proc "QUIT :Started wasting time elsewhere\n")
    (process-send-string proc (concat "QUIT :" (cdr parsedcmd) "\n"))))

;; /quote [raw irc command]
;;
;; send raw text to irc server
(defun zenirc-command-quote (proc parsedcmd)
  (process-send-string proc (concat (cdr parsedcmd) "\n")))

;; /server [server [port [nickname]]]
(defun zenirc-command-server (proc parsedcmd)
  (if (string= "" (cdr parsedcmd))
      (zenirc-select)
    (let* ((parsedarg (cdr parsedcmd)))
      (zenirc-select (car (zenirc-parse-n-words 1 parsedarg))
		     (or (car (cdr (zenirc-parse-n-words 2 parsedarg)))
			 zenirc-port)
		     (or (car (cdr (cdr  (zenirc-parse-n-words 3 parsedarg))))
			 zenirc-nick)))))

;; /squit server [reason]
(defun zenirc-command-squit (proc parsedcmd)
  (let* ((l (zenirc-parse-n-words 1 (cdr parsedcmd)))
         (n (length l))
         (v [nil "SQUIT %s\n" "SQUIT %s :%s\n"]))
    (cond ((< n 1)
           ;; Invalid format for command
           (zenirc-message proc 's421
                           (format "/%s %s" (car parsedcmd) (cdr parsedcmd))))
          (t
           (process-send-string proc (apply 'format (aref v n) l))))))

;; /topic channel [topic_string]
;;
;; set the topic of a channel to `topic string'
(defun zenirc-command-topic (proc parsedcmd)
  (let* ((parsedtext (zenirc-parse-firstword (cdr parsedcmd))))
    (if (not (string= (cdr parsedtext) ""))
	(process-send-string proc (concat "TOPIC " (car parsedtext)
					  " :" (cdr parsedtext) "\n"))
      (process-send-string proc (concat "TOPIC " (car parsedtext) "\n")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; server message handling subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; handle ERROR server message
(defun zenirc-server-ERROR (proc parsedmsg)
  (zenirc-message proc 'error (aref parsedmsg 1) (aref parsedmsg 2)))

;; INVITE - user invites you to channel
(defun zenirc-server-INVITE (proc parsedmsg)
  (zenirc-message proc 'invite
                  (zenirc-run-hook 'zenirc-format-nickuserhost-hook
                                   (aref parsedmsg 1))
                  (aref parsedmsg 3)))

;; NICK change server message
(defun zenirc-server-NICK (proc parsedmsg)
  (let ((from (aref parsedmsg 1))
        (to (aref parsedmsg 2)))
    (and (zenirc-names-equal-p (zenirc-extract-nick from) zenirc-nick)
         (progn
           (setq zenirc-nick to)
           (force-mode-line-update)))
    (zenirc-message proc 'nick
                    (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)
                    to)))

;; NOTICE server message
(defun zenirc-server-NOTICE (proc parsedmsg)
  (zenirc-privmsg-or-notice proc parsedmsg))

;; JOIN server message
;; zenirc-current-victim is the current channel your msgs will go to.
(defun zenirc-server-JOIN (proc parsedmsg)
  (let ((who (aref parsedmsg 1))
	(channel (aref parsedmsg 2))
	(mode nil))
    (if (zenirc-names-equal-p (zenirc-extract-nick who) zenirc-nick)
	(progn
	  (setq zenirc-current-victim (aref parsedmsg 2))
	  (setq zenirc-channel-list (cons (zenirc-downcase-name 
					   (aref parsedmsg 2))
					  zenirc-channel-list))
	  (zenirc-update-modeline)
	  (zenirc-message proc 'join_you zenirc-current-victim))
      
      (if (string-match "" channel) ; <dl> are people stupid or what?
	  (setq channel (substring (aref parsedmsg 2) 0 (- (match-end 0) 1))
		mode (substring (aref parsedmsg 2) (match-end 0)
				(length (aref parsedmsg 2)))))
      (if mode
	  (zenirc-message proc 'join_mode
			  (zenirc-run-hook 'zenirc-format-nickuserhost-hook
					   who) channel mode)
	(zenirc-message proc 'join
			(zenirc-run-hook 'zenirc-format-nickuserhost-hook who)
			channel)))))

;; KICK - you have been removed from a channel
;; (KICK kicker #chan kickee [reason])
(defun zenirc-server-KICK (proc parsedmsg)
  (if (not (zenirc-names-equal-p (aref parsedmsg 3) zenirc-nick))
      ;; someone else got kicked
      (zenirc-message proc 'kick
                      (aref parsedmsg 3)
                      (aref parsedmsg 2)
                      (format "%s - %s"
                              (zenirc-run-hook 'zenirc-format-nickuserhost-hook
                                               (aref parsedmsg 1))
                              (aref parsedmsg 4)))
    ;; you got kicked
    (zenirc-message proc 'kick_you
                    (aref parsedmsg 2)
                    (format "%s - %s"
                            (zenirc-extract-nick (aref parsedmsg 1))
                            (aref parsedmsg 4)))
    (if (zenirc-names-equal-p (aref parsedmsg 2) zenirc-current-victim)
	(setq zenirc-current-victim nil))))

;; KILL - you have been killed
(defun zenirc-server-KILL (proc parsedmsg)
  (zenirc-message proc 'kill (aref parsedmsg 3)))

;; MODE - channel or user mode change
;;
;; MODE from <channel> {[+|-]|o|p|s|i|t|n|b|v} [<limit>] [<user>] [<ban mask>]
;; MODE from <nickname> {[+|-]|i|w|s|o}
;;
;; Sum of channel `b' and `o' mode changes <= 3, so at most 5 args appear
;; after the the channel mode flags (three `b' or `o's, a `k', and an `l').
;; Current actual server implementation seems to limit this to 4 args.
(defun zenirc-server-MODE (proc parsedmsg)
  (zenirc-message proc 'mode
                  (zenirc-run-hook 'zenirc-format-nickuserhost-hook
                                   (aref parsedmsg 1))
                  (aref parsedmsg 2)
                  (format "%s %s %s %s %s %s"
                          (or (aref parsedmsg 3) "")
                          (or (aref parsedmsg 4) "")
                          (or (aref parsedmsg 5) "")
                          (or (aref parsedmsg 6) "")
                          (or (aref parsedmsg 7) "")
                          (or (aref parsedmsg 8) ""))))

;; PART - channel leave message
(defun zenirc-server-PART (proc parsedmsg)
  (let ((channel (aref parsedmsg 2))
	(who (aref parsedmsg 1))
	; As of ircd 2.9.1, you can send comments in your PART
	; message just like with QUIT. Thanks to this you can
	; always manage to get the last word in a serious
	; discussion.
	;
	; :Omnion!~pp@sno.pp.se PART #twilight_zone :grow up, piker
	(reason (or (aref parsedmsg 3)
		    ; on a 2.8 PART message, use nickname as comment
		    (zenirc-extract-nick (aref parsedmsg 1)))))
    (if (zenirc-names-equal-p (zenirc-extract-nick who) zenirc-nick)
	(progn
	  (zenirc-message proc 'part_you channel reason)
	  (setq zenirc-channel-list (delete (zenirc-downcase-name channel)
					    zenirc-channel-list))
	  (and zenirc-current-victim
	      (zenirc-names-equal-p channel zenirc-current-victim)
              (setq zenirc-current-victim (car zenirc-channel-list))))
      (zenirc-message proc 'part
                      (zenirc-run-hook 'zenirc-format-nickuserhost-hook who)
                      channel reason))))

;; PONG - server "is alive" message
(defun zenirc-server-PONG (proc parsedmsg)
  (zenirc-message proc 'pong (aref parsedmsg 1)))

;; PING - server "are you alive" message
(defun zenirc-server-PING (proc parsedmsg)
  (process-send-string proc (concat "PONG " (aref parsedmsg 2) "\n")))

;; PRIVMSG server message
(defun zenirc-server-PRIVMSG (proc parsedmsg)
  (zenirc-privmsg-or-notice proc parsedmsg))

;; QUIT - someone (thankfully) left irc.
(defun zenirc-server-QUIT (proc parsedmsg)
  (zenirc-message proc 'quit
                  (zenirc-run-hook 'zenirc-format-nickuserhost-hook
                                   (aref parsedmsg 1))
                  (aref parsedmsg 2)))

;; TOPIC - someone set the topic on a channel
(defun zenirc-server-TOPIC (proc parsedmsg)
  (zenirc-message proc 'topic
                  (zenirc-run-hook 'zenirc-format-nickuserhost-hook
                                   (aref parsedmsg 1))
                  (aref parsedmsg 2)
                  (aref parsedmsg 3)))

;; WALLOPS - notice to operators
(defun zenirc-server-WALLOPS (proc parsedmsg)
  (zenirc-message proc 'wallops (aref parsedmsg 1) (aref parsedmsg 2)))

;; 001 - welcome to irc
(defun zenirc-server-001 (proc parsedmsg)
  (setq zenirc-nick (aref parsedmsg 2))
  (zenirc-message proc 's001))

;; 002 - who is your server
;; hostname regexp [-a-zA-Z.0-9]
(defun zenirc-server-002 (proc parsedmsg)
  (let ((str (aref parsedmsg 3)))
    (if (string-match
	 "Your host is \\([-a-zA-Z.0-9]+\\), running version \\(.*\\)"
	 str)
	(zenirc-message proc 's002
			(zenirc-match-string 1 str)
			(zenirc-match-string 2 str))
      (zenirc-message proc 'protocol_violation
                      (aref parsedmsg 1)
                      (aref parsedmsg 3)))))

;; 003 - when this server was built
(defun zenirc-server-003 (proc parsedmsg)
  (let ((str (aref parsedmsg 3)))
    (if (string-match "This server was created \\(.*\\)" str)
	(zenirc-message proc 's003 (zenirc-match-string 1 str))
      (zenirc-message proc 'protocol_violation
                      (aref parsedmsg 1)
                      (aref parsedmsg 3)))))

;; 004 - version and allowed modes information
(defun zenirc-server-004 (proc parsedmsg)
  (setq zenirc-current-server-name (aref parsedmsg 3))
  (setq zenirc-server-version (aref parsedmsg 4))
  (setq zenirc-user-modes (aref parsedmsg 5))
  (setq zenirc-server-modes (aref parsedmsg 6)))

;; 200 RPL_TRACELINK - {<server>} Link -> Version: <version>
;; 200 RPL_TRACELINK - {<server>} (version) Link to: -> <server>
(defun zenirc-server-200 (proc parsedmsg)
  (zenirc-message proc 's200
                  (aref parsedmsg 1)
                  (aref parsedmsg 4)
                  (aref parsedmsg 6)))

;; 201 RPL_TRACECONNECTING
(defun zenirc-server-201 (proc parsedmsg)
  (zenirc-message proc 's201 (aref parsedmsg 1) (aref parsedmsg 5)))

;; 202 RPL_TRACEHANDSHAKE
(defun zenirc-server-202 (proc parsedmsg)
  (zenirc-message proc 's202 (aref parsedmsg 1) (aref parsedmsg 5)))

;; 203 RPL_TRACEUNKNOWN - {<server>} Unknown -> IP address : <ip#>
(defun zenirc-server-203 (proc parsedmsg)
  (zenirc-message proc 's203 (aref parsedmsg 1) (aref parsedmsg 5)))

;; 204 RPL_TRACEOPERATOR {<server>} Oper -> <nick[host]>
(defun zenirc-server-204 (proc parsedmsg)
  (zenirc-message proc 's204 (aref parsedmsg 1) (aref parsedmsg 5)))

;; 205 RPL_TRACEUSER {<server>} Luser -> <nick[host]>
(defun zenirc-server-205 (proc parsedmsg)
  (zenirc-message proc 's205 (aref parsedmsg 1) (aref parsedmsg 5)))

;; 206 RPL_TRACESERVER {<server>} Server -> <server> Class: <#> S: <#> C: <#>
(defun zenirc-server-206 (proc parsedmsg)
  (zenirc-message proc 's206
                  (aref parsedmsg 1)
                  (aref parsedmsg 4)
                  (aref parsedmsg 5)
                  (aref parsedmsg 6)
                  (aref parsedmsg 7)))

;; 208 RPL_TRACENEWTYPE
(defun zenirc-server-208 (proc parsedmsg)
  (zenirc-message proc 's208
                  (aref parsedmsg 1)
                  (aref parsedmsg 3)
                  (aref parsedmsg 5)))

;; 209 RPL_TRACECLASS {<server}> Class -> type = blah
(defun zenirc-server-209 (proc parsedmsg)
  (zenirc-message proc 's209
                  (aref parsedmsg 1)
                  (aref parsedmsg 4)
                  (aref parsedmsg 5)))

;; 211 RPL_STATLINKINFO
(defun zenirc-server-211 (proc parsedmsg)
  (zenirc-message proc 's211
                  (aref parsedmsg 3)
                  (aref parsedmsg 9)
                  (aref parsedmsg 5)
                  (aref parsedmsg 6)
                  (aref parsedmsg 7)
                  (aref parsedmsg 8)
                  (aref parsedmsg 4)))

;; 212 RPL_STATSCOMMANDS
(defun zenirc-server-212 (proc parsedmsg)
  (zenirc-message proc 's212
                  (aref parsedmsg 3)
                  (aref parsedmsg 4)
                  (aref parsedmsg 5)))

;; 213 RPL_STATSCLINE
(defun zenirc-server-213 (proc parsedmsg)
  (zenirc-message proc 's213
                  (aref parsedmsg 4)
                  (aref parsedmsg 6)
                  (aref parsedmsg 7)
                  (aref parsedmsg 8)))

;; 214 RPL_STATSNLINE
(defun zenirc-server-214 (proc parsedmsg)
  (zenirc-message proc 's214
                  (aref parsedmsg 4)
                  (aref parsedmsg 6)
                  (aref parsedmsg 7)
                  (aref parsedmsg 8)))

;; 215 RPL_STATSILINE
(defun zenirc-server-215 (proc parsedmsg)
  (zenirc-message proc 's215
		  (aref parsedmsg 3)
		  (aref parsedmsg 4)
                  (aref parsedmsg 6)
                  (aref parsedmsg 8)))

;; 216 RPL_STATSKLINE
(defun zenirc-server-216 (proc parsedmsg)
  (zenirc-message proc 's216 (aref parsedmsg 4) (aref parsedmsg 6)))

;; 217 RPL_STATSQLINE
(defun zenirc-server-217 (proc parsedmsg)
  (zenirc-message proc 's217
                  (aref parsedmsg 4)
                  (aref parsedmsg 5)
                  (aref parsedmsg 6)
                  (aref parsedmsg 7)
                  (aref parsedmsg 8)))

;; 218 RPL_STATSYLINE
(defun zenirc-server-218 (proc parsedmsg)
  (zenirc-message proc 's218
                  (aref parsedmsg 4)
                  (aref parsedmsg 5)
                  (aref parsedmsg 6)
                  (aref parsedmsg 7)
                  (aref parsedmsg 8)))

;; 219 RPL_ENDOFSTATS
(defun zenirc-server-219 (proc parsedmsg)
  (zenirc-message proc 's219))

;; 221 RPL_UMODEIS
(defun zenirc-server-221 (proc parsedmsg)
  (zenirc-message proc 's221 (aref parsedmsg 3)))

;; 241 RPL_STATSLLINE
(defun zenirc-server-241 (proc parsedmsg)
  (zenirc-message proc 's241 (aref parsedmsg 4) (aref parsedmsg 6)))

;; 242 RPL_STATSUPTIME
(defun zenirc-server-242 (proc parsedmsg)
  (zenirc-message proc 's242 (aref parsedmsg 3)))

;; 243 RPL_STATSOLINE
(defun zenirc-server-243 (proc parsedmsg)
  (zenirc-message proc 's243
                  (aref parsedmsg 3)
                  (aref parsedmsg 6)
                  (aref parsedmsg 4)))

;; 244 RPL_STATSHLINE
(defun zenirc-server-244 (proc parsedmsg)
  (zenirc-message proc 's244 (aref parsedmsg 4) (aref parsedmsg 6)))

;; 249 RPL_STATSZLINE
(defun zenirc-server-249 (proc parsedmsg)
  (zenirc-message proc 's249 (aref parsedmsg 1) (aref parsedmsg 3)))

;; 251 - :server 251 ZenIRC :There are x users and y invisible on z servers
;;       :server 251 ZenIRC :There are x users and y services on z servers
(defun zenirc-server-251 (proc parsedmsg)
  (let ((str (aref parsedmsg 3)))
    (if (string-match "There are \\([0-9]+\\) users and \\([0-9]+\\) invisible on \\([0-9]+\\) servers" str)
	(zenirc-message proc 's251
			(zenirc-match-string 1 str)
			(zenirc-match-string 2 str)
			(zenirc-match-string 3 str))
      (if (string-match "There are \\([0-9]+\\) users and \\([0-9]+\\) services on \\([0-9]+\\) servers" str)
	  (zenirc-message proc 's251-29
			  (zenirc-match-string 1 str)
			  (zenirc-match-string 2 str)
			  (zenirc-match-string 3 str))
	(zenirc-message proc 'protocol_violation
			(aref parsedmsg 1) (aref parsedmsg 3))))))

;; 252 - number of irc operators online
(defun zenirc-server-252 (proc parsedmsg)
  (zenirc-message proc 's252 (aref parsedmsg 3)))

;; 253 - number of "unknown" connections
(defun zenirc-server-253 (proc parsedmsg)
  (zenirc-message proc 's253 (aref parsedmsg 3)))

;; 254 - number of channels
(defun zenirc-server-254 (proc parsedmsg)
  (zenirc-message proc 's254 (aref parsedmsg 3)))

;; 255 - :server 255 ZenIRC :I have x clients and y servers
;;       :server 255 ZenIRC :I have x clients, y services and z servers
(defun zenirc-server-255 (proc parsedmsg)
  (let ((str (aref parsedmsg 3)))
    (if (string-match "I have \\([0-9]+\\) clients and \\([0-9]+\\) servers" str)
	(zenirc-message proc 's255
                        (substring str (match-beginning 1) (match-end 1))
                        (substring str (match-beginning 2) (match-end 2)))
      (if (string-match "I have \\([0-9]+\\) clients, \\([0-9]+\\) services and \\([0-9]+\\) servers" str)
	  (zenirc-message proc 's255-29
			  (substring str (match-beginning 1) (match-end 1))
			  (substring str (match-beginning 2) (match-end 2))
			  (substring str (match-beginning 3) (match-end 3)))
	(zenirc-message proc 'protocol_violation
			(aref parsedmsg 1) (aref parsedmsg 3))))))

;; 256 - line 1 of /admin information
(defun zenirc-server-256 (proc parsedmsg)
  (zenirc-message proc 's256 (aref parsedmsg 1)))

;; 257 - line 2 of /admin information
(defun zenirc-server-257 (proc parsedmsg)
  (zenirc-message proc 's257 (aref parsedmsg 3)))

;; 258 - line 2 of /admin information
(defun zenirc-server-258 (proc parsedmsg)
  (zenirc-message proc 's258 (aref parsedmsg 3)))

;; 259 - line 4 of /admin information
(defun zenirc-server-259 (proc parsedmsg)
  (zenirc-message proc 's259 (aref parsedmsg 3)))

;; 261 RPL_TRACELOG
(defun zenirc-server-261 (proc parsedmsg)
  (zenirc-message proc 's261
                  (aref parsedmsg 1)
                  (aref parsedmsg 4)
                  (aref parsedmsg 5)))

;; 262 - RPL_TRACEEND
(defun zenirc-server-262 (proc parsedmsg)
  (zenirc-message proc 's262
		  (aref parsedmsg 1)
		  (aref parsedmsg 4)))

;; 301 - someone is /away
(defun zenirc-server-301 (proc parsedmsg)
  (zenirc-message proc 's301 (aref parsedmsg 3) (aref parsedmsg 4)))

;; 302 - Userhost reply - RPL_USERHOST  ":[<reply>{<space><reply>}]"
(defun zenirc-server-302 (proc parsedmsg)
  (zenirc-message proc 's302 (aref parsedmsg 3)))

;; 303 - Ison reply - RPL_ISON ":[<nick> {<space><nick>}]"
(defun zenirc-server-303 (proc parsedmsg)
  (zenirc-message proc 's303 (aref parsedmsg 3)))

;; 305 - you are not /away
(defun zenirc-server-305 (proc parsedmsg)
  (zenirc-message proc 's305))

;; 306 - you are /away
(defun zenirc-server-306 (proc parsedmsg)
  (zenirc-message proc 's306))

;; 311 - userinfo for /whois list
(defun zenirc-server-311 (proc parsedmsg)
  (zenirc-message proc 's311
                  (aref parsedmsg 3)
                  (aref parsedmsg 4)
                  (aref parsedmsg 5)
                  (aref parsedmsg 7)))

;; 312 - server part of /whois list
(defun zenirc-server-312 (proc parsedmsg)
  (zenirc-message proc 's312
                  (aref parsedmsg 3)
                  (aref parsedmsg 4)
                  (aref parsedmsg 5)))

;; 313 - /whois list reply indicating irc operator
(defun zenirc-server-313 (proc parsedmsg)
  (zenirc-message proc 's313 (aref parsedmsg 3)))

;; 314 - /whowas reply
(defun zenirc-server-314 (proc parsedmsg)
  (zenirc-message proc 's314
                  (aref parsedmsg 3)
                  (aref parsedmsg 4)
                  (aref parsedmsg 5)
                  (aref parsedmsg 7)))

;; 315 - end of /who list
(defun zenirc-server-315 (proc parsedmsg)
  (zenirc-message proc 's315))

;; 317 - /whois list idle time reply
(defun zenirc-server-317 (proc parsedmsg)
  (let ((hours (/ (string-to-int (aref parsedmsg 4)) 3600)))
    (let ((minutes (- (/ (string-to-int (aref parsedmsg 4)) 60)
		      (* hours 60))))
      (let ((seconds (- (string-to-int (aref parsedmsg 4))
			(* hours 3600)
			(* minutes 60))))
	(let ((hours (int-to-string hours))
	      (minutes (int-to-string minutes))
	      (seconds (int-to-string seconds)))
	  (if (= (length minutes) 1)
	      (setq minutes (concat "0" minutes)))
	  (if (= (length seconds) 1)
	      (setq seconds (concat "0" seconds)))
	  (let ((idle-string (concat
			      hours ":" minutes ":" seconds)))
	    (zenirc-message proc 's317
			    (aref parsedmsg 3)
			    idle-string)))))))

;; 318 - end of /whois list
(defun zenirc-server-318 (proc parsedmsg)
  (zenirc-message proc 's318))

;; 319 - what channels part of /whois list
(defun zenirc-server-319 (proc parsedmsg)
  (zenirc-message proc 's319 (aref parsedmsg 3) (aref parsedmsg 4)))

;; 321 - header for /list command
;; This reply was removed as of ircd 2.9.1.
(defun zenirc-server-321 (proc parsedmsg)
  (zenirc-message proc 's321))

;; 322 - element returned by /list
(defun zenirc-server-322 (proc parsedmsg)
  (zenirc-message proc 's322
                  (aref parsedmsg 3)
                  (aref parsedmsg 4)
                  (aref parsedmsg 5)))

;; 323 - trailer for /list command
(defun zenirc-server-323 (proc parsedmsg)
  (zenirc-message proc 's323))

;; 324 - RPL_CHANNELMODEIS "<channel> <mode> <mode params>"
(defun zenirc-server-324 (proc parsedmsg)
  (zenirc-message proc 's324
                  (aref parsedmsg 3)
                  (aref parsedmsg 4)
                  (or (aref parsedmsg 5) "")))

;; 331 - RPL_NOTOPIC "<channel> :No topic is set"
(defun zenirc-server-331 (proc parsedmsg)
  (zenirc-message proc 's331 (aref parsedmsg 3)))

;; 332 - channel topic on join, etc.
(defun zenirc-server-332 (proc parsedmsg)
  (zenirc-message proc 's332 (aref parsedmsg 3) (aref parsedmsg 4)))

;; 333 - user who set topic and when it was set
;;       :server 333 to channel who-set-topic time-when-set
(defun zenirc-server-333 (proc parsedmsg)
  (zenirc-message proc 's333
                  (aref parsedmsg 3)
                  (aref parsedmsg 4)
                  (current-time-string (zenirc-epoch-seconds-to-time
                                        (aref parsedmsg 5)))))

;; 341 - invite reply
(defun zenirc-server-341 (proc parsedmsg)
  (zenirc-message proc 's341 (aref parsedmsg 3) (aref parsedmsg 4)))

;; 342 - RPL_SUMMONING "<user> :Summoning user to IRC"
(defun zenirc-server-342 (proc parsedmsg)
  (zenirc-message proc 's342 (aref parsedmsg 3)))

;; 351 - RPL_VERSION "<version>.<debuglevel> <server> :<comments>"
(defun zenirc-server-351 (proc parsedmsg)
  (zenirc-message proc 's351
                  (aref parsedmsg 3)
                  (aref parsedmsg 4)
                  (aref parsedmsg 5)))

;; 352 - WHO reply
(defun zenirc-server-352 (proc parsedmsg)
  (if (string= (aref parsedmsg 3) "Channel")
      ;; this is the header
      (zenirc-message proc 's352_header)
    ;; this is a reply
    (zenirc-message proc 's352
                    (aref parsedmsg 7)
                    (aref parsedmsg 8)
                    (aref parsedmsg 3)
                    (aref parsedmsg 4)
                    (aref parsedmsg 5)
                    (aref parsedmsg 9))))

;; 353 - name list after channel join or NAMES command
(defun zenirc-server-353 (proc parsedmsg)
  (zenirc-message proc 's353 (aref parsedmsg 4) (aref parsedmsg 5)))

;; 364 - RPL_LINKS "<mask> <server> :<hopcount> <server info>"
(defun zenirc-server-364 (proc parsedmsg)
  (zenirc-message proc 's364
                  (aref parsedmsg 3)
                  (aref parsedmsg 4)
                  (aref parsedmsg 5)))

;; 365 - RPL_ENDOFLINKS "<mask> :End of /LINKS list"
(defun zenirc-server-365 (proc parsedmsg)
  (zenirc-message proc 's365 (aref parsedmsg 3)))

;; 366 - after all ppl on channel displayed
(defun zenirc-server-366 (proc parsedmsg) ())

;; 367 - RPL_BANLIST "<channel> <banid>"
(defun zenirc-server-367 (proc parsedmsg)
  (zenirc-message proc 's367 (aref parsedmsg 3) (aref parsedmsg 4)))

;; 368 - RPL_RPL_ENDOFBANLIST
(defun zenirc-server-368 (proc parsedmsg)
  (zenirc-message proc 's368))

;; 369 - end of whowas
(defun zenirc-server-369 (proc parsedmsg) ())

;; 371 - RPL_INFO ":<string>"
(defun zenirc-server-371 (proc parsedmsg)
  (zenirc-message proc 's371 (aref parsedmsg 3)))

;; 372 - motd line
(defun zenirc-server-372 (proc parsedmsg)
  (zenirc-message proc 's372 (aref parsedmsg 3)))

;; 374 - RPL_ENDOFINFO ":End of /INFO list"
(defun zenirc-server-374 (proc parsedmsg) ())

;; 375 - start of /MOTD
(defun zenirc-server-375 (proc parsedmsg)
  (zenirc-message proc 's375))

;; 376 - end of /MOTD
(defun zenirc-server-376 (proc parsedmsg)
  (zenirc-message proc 's376))

;; 381 - RPL_YOUREOPER ":You are now an IRC operator"
(defun zenirc-server-381 (proc parsedmsg)
  (zenirc-message proc 's381))

;; 382 - RPL_REHASHING "<config file> :Rehashing"
(defun zenirc-server-382 (proc parsedmsg)
  (zenirc-message proc 's382 (aref parsedmsg 3)))

;; 391 - RPL_TIME "<server> :<string showing server's local time>"
(defun zenirc-server-391 (proc parsedmsg)
  (zenirc-message proc 's391 (aref parsedmsg 3) (aref parsedmsg 4)))

;; 392 - RPL_USERSSTART ":UserID   Terminal  Host"
(defun zenirc-server-392 (proc parsedmsg)
  (zenirc-message proc 's392))

;; 393 - RPL_USERS ":%-8s %-9s %-8s"
(defun zenirc-server-393 (proc parsedmsg)
  (zenirc-message proc 's393 (aref parsedmsg 3)))

;; 394 - RPL_ENDOFUSERS ":End of users"
(defun zenirc-server-394 (proc parsedmsg) ())

;; 395 - RPL_NOUSERS ":Nobody logged in"
(defun zenirc-server-395 (proc parsedmsg)
  (zenirc-message proc 's395))

;; 401 - no such nick/channel
(defun zenirc-server-401 (proc parsedmsg)
  (zenirc-message proc 's401 (aref parsedmsg 3))
  (if zenirc-whowas-on-401
      (process-send-string proc (concat "WHOWAS " (aref parsedmsg 3) "\n"))))

;; 402 - no such server
(defun zenirc-server-402 (proc parsedmsg)
  (zenirc-message proc 's402 (aref parsedmsg 3)))

;; 403 - ERRNOSUCHCHANNEL "<channel> :No such channel"
(defun zenirc-server-403 (proc parsedmsg)
  (zenirc-message proc 's403 (aref parsedmsg 3)))

;; 404 - ERR_CANNOTSENDTOCHAN "<channel> :Cannot send to channel"
(defun zenirc-server-404 (proc parsedmsg)
  (zenirc-message proc 's404 (aref parsedmsg 3)))

;; 405 - ERR_TOOMANYCHANNELS  "<channel_name> :You have joined too many channels"
(defun zenirc-server-405 (proc parsedmsg)
  (zenirc-message proc 's405 (aref parsedmsg 3)))

;; 406 - ERR_WASNOSUCHNICK  "<channel_name> :There was no such nickname"
(defun zenirc-server-406 (proc parsedmsg)
  (zenirc-message proc 's406 (aref parsedmsg 3)))

;; 407 - ERR_TOOMANYTARGETS "<target> :Duplicate recipients. No message delivered"
(defun zenirc-server-407 (proc parsedmsg)
  (zenirc-message proc 's407 (aref parsedmsg 3)))

;; 409 - ERR_NOORIGIN ":No origin specified"
(defun zenirc-server-409 (proc parsedmsg)
  (zenirc-message proc 's409))

;; 411 - ERR_NORECIPIENT ":No recipient given (<command>)"
(defun zenirc-server-411 (proc parsedmsg)
  (zenirc-message proc 's411))

;; 412 - ERR_NOTEXTTOSEND ":No text to send"
;; you sent a message w/o any text
(defun zenirc-server-412 (proc parsedmsg)
  (zenirc-message proc 's412))

;; 413 - ERR_NOTOPLEVEL "<mask> :No toplevel domain specified"
(defun zenirc-server-413 (proc parsedmsg)
  (zenirc-message proc 's413 (aref parsedmsg 3)))

;; 414 - ERR_WILDTOPLEVEL "<mask> :Wildcard in toplevel domain"
(defun zenirc-server-414 (proc parsedmsg)
  (zenirc-message proc 's414 (aref parsedmsg 3)))

;; 415 - ERR_BADMASK "<server/host> :Bad Server/host mask"
(defun zenirc-server-415 (proc parsedmsg)
  (zenirc-message proc 's415 (aref parsedmsg 3)))

;; 421 - server detected error in what you sent
(defun zenirc-server-421 (proc parsedmsg)
  (zenirc-message proc 's421 (aref parsedmsg 3)))

;; 422 - ERR_NOMOTD ":MOTD File is missing"
(defun zenirc-server-422 (proc parsedmsg)
  (zenirc-message proc 's422))

;; 423 - ERR_NOADMININFO "<server> :No administrative info available"
(defun zenirc-server-423 (proc parsedmsg)
  (zenirc-message proc 's423 (aref parsedmsg 3)))

;; 424 ERR_FILEERROR ":File error doing <file op> on <file>"
(defun zenirc-server-424 (proc parsedmsg)
  (zenirc-message proc (aref parsedmsg 3)))

;; 431     ERR_NONICKNAMEGIVEN ":No nickname given"
(defun zenirc-server-431 (proc parsedmsg)
  (zenirc-message proc 's431))

;; 432 -  ERR_ERRONEUSNICKNAME "<nick> :Erroneus nickname"
(defun zenirc-server-432 (proc parsedmsg)
  (zenirc-message proc 's432 (aref parsedmsg 3)))

;; 433 - ERR_NICKNAMEINUSE "<nick> :Nickname is already in use"
(defun zenirc-server-433 (proc parsedmsg)
  (zenirc-message proc 's433 (aref parsedmsg 3)))

;; 436 - ERR_NICKCOLLISION "<nick> :Nickname collision KILL"
(defun zenirc-server-436 (proc parsedmsg)
  (zenirc-message proc 's436 (aref parsedmsg 3)))

;; 437 - ERR_UNAVAILRESOURCE "<nick>/<channel> :Nick/channel is temporarily
;;                                            unavailable"
(defun zenirc-server-437 (proc parsedmsg)
  (zenirc-message proc 's437 (aref parsedmsg 3)))

;; 441 - ERR_USERNOTINCHANNEL "<nick> <channel> :They aren't on that channel"
(defun zenirc-server-441 (proc parsedmsg)
  (zenirc-message proc 's441 (aref parsedmsg 3) (aref parsedmsg 4)))

;; 442 - You are not on that channel
(defun zenirc-server-442 (proc parsedmsg)
  (zenirc-message proc 's442 (aref parsedmsg 3)))

;; 443 - already on channel invite error
(defun zenirc-server-443 (proc parsedmsg)
  (zenirc-message proc 's443 (aref parsedmsg 3) (aref parsedmsg 4)))

;; 444 - ERR_NOLOGIN "<user> :User not logged in"
(defun zenirc-server-444 (proc parsedmsg)
  (zenirc-message proc 's444 (aref parsedmsg 3)))

;; 445 - ERR_SUMMONDISABLED ":SUMMON has been disabled"
(defun zenirc-server-445 (proc parsedmsg)
  (zenirc-message proc 's445))

;; 446 - ERR_USERSDISABLED ":USERS has been disabled"
(defun zenirc-server-446 (proc parsedmsg)
  (zenirc-message proc 's446))

;; 451 - ERR_NOTREGISTERED ":You have not registered"
(defun zenirc-server-451 (proc parsedmsg)
  (zenirc-message proc 's451))

;; 461 - server detected error - not enough parameters
(defun zenirc-server-461 (proc parsedmsg)
  (zenirc-message proc 's461 (aref parsedmsg 3)))

;; 462 - ERR_ALREADYREGISTRED  ":You may not reregister"
(defun zenirc-server-462 (proc parsedmsg)
  (zenirc-message proc 's462))

;; 463 - ERR_NOPERMFORHOST ":Your host isn't among the privileged"
(defun zenirc-server-463 (proc parsedmsg)
  (zenirc-message proc 's463))

;; 464 - ERR_PASSWDMISMATCH ":Password incorrect"
(defun zenirc-server-464 (proc parsedmsg)
  (zenirc-message proc 's464))

;; 465 - ERR_YOUREBANNEDCREEP ":You are banned from this server"
(defun zenirc-server-465 (proc parsedmsg)
  (zenirc-message proc 's465))

;; 467 - ERR_KEYSET "<channel> :Channel key already set"
(defun zenirc-server-467 (proc parsedmsg)
  (zenirc-message proc 's467 (aref parsedmsg 3)))

;; 471 - ERR_CHANNELISFULL "<channel> :Cannot join channel (+l)"
(defun zenirc-server-471 (proc parsedmsg)
  (zenirc-message proc 's471 (aref parsedmsg 3)))

;; 472 - ERR_UNKNOWNMODE "<char> :is unknown mode char to me"
(defun zenirc-server-472 (proc parsedmsg)
  (zenirc-message proc 's472 (aref parsedmsg 3)))

;; 473 - ERR_INVITEONLYCHAN "<channel> :Cannot join channel (+i)"
(defun zenirc-server-473 (proc parsedmsg)
  (zenirc-message proc 's473 (aref parsedmsg 3)))

;; 474 - ERR_BANNEDFROMCHAN "<channel> :Cannot join channel (+b)"
(defun zenirc-server-474 (proc parsedmsg)
  (zenirc-message proc 's474 (aref parsedmsg 3)))

;; 475 - ERR_BADCHANNELKEY "<channel> :Cannot join channel (+k)"
(defun zenirc-server-475 (proc parsedmsg)
  (zenirc-message proc 's475 (aref parsedmsg 3)))

;; 477 - ERR_NOCHANMODES "<channel> :Channel doesn't support modes"
(defun zenirc-server-477 (proc parsedmsg)
  (zenirc-message proc 's477 (aref parsedmsg 3)))

;; 481 - ERR_NOPRIVILEGES ":Permission Denied- You're not an IRC operator"
(defun zenirc-server-481 (proc parsedmsg)
  (zenirc-message proc 's481))

;; 482 - ERR_CHANOPRIVSNEEDED "<channel> :You're not channel operator"
(defun zenirc-server-482 (proc parsedmsg)
  (zenirc-message proc 's482 (aref parsedmsg 3)))

;; 483 - ERR_CANTKILLSERVER ":You cant kill a server!"
(defun zenirc-server-483 (proc parsedmsg)
  (zenirc-message proc 's483))

;; 491 - ERR_NOOPERHOST ":No O-lines for your host"
(defun zenirc-server-491 (proc parsedmsg)
  (zenirc-message proc 's491))

;; 501 - ERR_UMODEUNKNOWNFLAG ":Unknown MODE flag"
(defun zenirc-server-501 (proc parsedmsg)
  (zenirc-message proc 's501))

;; 502 - ERR_USERSDONTMATCH ":Cant change mode for other users"
(defun zenirc-server-502 (proc parsedmsg)
  (zenirc-message proc 's502))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PRIVMSG/NOTICE and CTCP handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun zenirc-privmsg-or-notice (proc parsedmsg)
  (let ((from (aref parsedmsg 1))	; who the message is from
	(to (aref parsedmsg 2))		; who the message is to
	(text (aref parsedmsg 3))	; the text of the message
	(type (aref parsedmsg 0)))	; privmsg or notice
    ;; see if privmsg or notice contains ctcp
    (if (not (string-match "\C-a" text))
	(zenirc-format-privmsg-or-notice type from to text proc)
      ;; message contains ctcp. break it up into ctcp and non-ctcp parts
      ;; and handle each.
      (let ((index 0) (ctcp nil) (str "") (len (length text)))
	(while (< index len)
	  (if (char-equal (aref text index) ?\C-a)
	      ;; we hit a control-a - deal with it.
	      (progn
		(if (not (string= str ""))
		    ;; we have a string
		    (if ctcp
			;; we are in a ctcp message
			(progn
			  (zenirc-handle-ctcp type from to str proc)
			  (setq str ""))
		      ;; we are in a regular message
		      (zenirc-format-privmsg-or-notice type from to str proc)
		      (setq str ""))
		  ;; we do not have a string
		  (if (and (not (= index 0)) ctcp)
		      ;; some leper sent a zero length message
		      (zenirc-handle-ctcp type from to str proc)))
		(setq ctcp (not ctcp))) ; toggle ctcp state
	    ;; we have a regular character
	    (setq str (concat str (char-to-string (aref text index)))))
	  (setq index (1+ index))
	  (if (and (= index len) ctcp)
	      ;; someone sent us an ill-formed ctcp message
	      (zenirc-unbalanced-ctcp type from to str proc)))))))

;; handle a ctcp message
(defun zenirc-handle-ctcp (type from to str proc)
  (let* ((parsedctcp (zenirc-parse-firstword str))
         (fmt (if (string= type "PRIVMSG") 'query 'reply))
         (hook-name (format "zenirc-ctcp-%s-%s-hook" fmt (car parsedctcp)))
         (hook (intern-soft hook-name))
         (sender (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)))
    (and zenirc-debug-ctcp
         (zenirc-message proc 'debug hook-name))
    (cond ((and hook
                (boundp hook))
           (zenirc-run-hook hook proc parsedctcp from to))
          ;; Don't ever reply to notices, just privmsgs
          (t
           (and (string= type "PRIVMSG")
                zenirc-send-ctcp-errmsg-on-unknown
                (zenirc-ctcp-errmsg
                 type sender to str
		 (zenirc-lang-retrieve-catalog-entry 'query_unknown)
                 proc))
           (zenirc-message proc fmt sender to str)))))

;; spew a ctcp error
(defun zenirc-ctcp-errmsg (type from to str whine proc)
  (let ((nick (zenirc-extract-nick from))
        (fmt "NOTICE %s :\C-aERRMSG %s :%s\C-a\n"))
    (process-send-string proc (format fmt nick str whine))))

;; handle an unbalanced ctcp message
(defun zenirc-unbalanced-ctcp (type from to str proc)
  (let ((sender (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)))
    (cond
     ((string= type "PRIVMSG")
      (and zenirc-send-ctcp-errmsg-on-unbalanced
           (zenirc-ctcp-errmsg type from to str
	    (zenirc-lang-retrieve-catalog-entry 'query_unbalanced_reply)
            proc))
      (zenirc-message proc 'query_unbalanced sender to str))
     (t
      (zenirc-message proc 'reply_unbalanced sender to str)))))

;; format a PRIVMSG or NOTICE and insert it in the zenirc process buffer
(defun zenirc-format-privmsg-or-notice (type origfrom to text proc)
  (let ((timestr (zenirc-timestamp-string))
	(from (zenirc-run-hook 'zenirc-format-nickuserhost-hook origfrom))
        msgtype)
    (setq zenirc-privmsg-last-seen origfrom)
    (cond
     ((not 
       (zenirc-channel-p to))
      
      (or (and zenirc-current-server-name
               (zenirc-names-equal-p from zenirc-current-server-name t))
          (setq zenirc-privmsg-last-rec (zenirc-extract-nick from)))

      (if (string= type "PRIVMSG")
          (setq msgtype 'privmsg_you)
        (setq msgtype 'notice_you))
      (zenirc-message proc msgtype 
		      (if zenirc-timestamp
			  (concat from zenirc-timestamp-prefix 
			      timestr zenirc-timestamp-suffix) 
			from)
		      text))
     ((and
       (zenirc-names-equal-p to zenirc-current-victim t)
       (not zenirc-always-show-channelname))
      
      (if (string= type "PRIVMSG")
          (setq msgtype 'privmsg_nochannel)
        (setq msgtype 'notice_nochannel))

      (zenirc-message proc msgtype 
		      (if zenirc-timestamp
			  (concat from zenirc-timestamp-prefix
				  timestr zenirc-timestamp-suffix)
			from)
			text))
     (t
      (if (string= type "PRIVMSG")
          (setq msgtype 'privmsg)
        (setq msgtype 'notice))
      (zenirc-message proc msgtype from 
		      (if zenirc-timestamp
			  (concat to zenirc-timestamp-prefix
				  timestr zenirc-timestamp-suffix)
			to)
		      text)))))

(defun zenirc-timestamp-string ()
  (substring (current-time-string) 11 16))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ctcp handlers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; query handlers

;; handler for a ctcp ACTION query
;;
;; Order of args is recipient, sender, message.
;; E.g. [ACTION->#oink_worship] noah splodes
(defun zenirc-ctcp-query-ACTION (proc parsedctcp from to)
  (if (and (not zenirc-always-show-channelname)
	   (zenirc-names-equal-p to zenirc-current-victim t))
      (zenirc-message proc 'ctcp_action_nochannel
		      (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)
		      (cdr parsedctcp))
    (zenirc-message proc 'ctcp_action
		    to
		    (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)
		    (cdr parsedctcp))))

;; handler for a ctcp PING query
(defun zenirc-ctcp-query-PING (proc parsedctcp from to)
  (process-send-string
   proc (concat "NOTICE "
		(zenirc-extract-nick from)
		" :\C-aPING " (cdr parsedctcp) "\C-a\n"))
  (if zenirc-verbose-ctcp
      (zenirc-message proc 'ctcp_ping
                      (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)
                      to)))

;; handler for a ctcp VERSION query
(defun zenirc-ctcp-query-VERSION (proc parsedctcp from to)
  (process-send-string
   proc (format "NOTICE %s :\C-aVERSION ZenIRC : %s : in %s %s\C-a\n"
                (zenirc-extract-nick from)
                zenirc-version
                (zenirc-emacs-variant)
                emacs-version))
  (and zenirc-verbose-ctcp
       (zenirc-message proc 'ctcp_version
                       (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)
                       to)))

;; handler for a ctcp USERINFO query
(defun zenirc-ctcp-query-USERINFO (proc parsedctcp from to)
  (process-send-string
   proc (concat "NOTICE "
		(zenirc-extract-nick from)
		" :\C-aUSERINFO :" zenirc-userinfo "\C-a\n"))
  (if zenirc-verbose-ctcp
      (zenirc-message proc 'ctcp_userinfo
                      (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)
                      to)))

;; handler for a ctcp SOURCE query
(defun zenirc-ctcp-query-SOURCE (proc parsedctcp from to)
  (let ((src zenirc-source-list)
        (fromnick (zenirc-extract-nick from)))
    (while src
      (process-send-string proc
                           (concat "NOTICE "
                                   fromnick
                                   " :\C-aSOURCE " (car src) "\C-a\n"))
      (setq src (cdr src)))
    (process-send-string proc (concat "NOTICE " fromnick " :\C-aSOURCE\C-a\n"))
  (if zenirc-verbose-ctcp
      (zenirc-message proc 'ctcp_source
                      (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)
                      to))))

;; handler for a ctcp CLIENTINFO query
(defun zenirc-ctcp-query-CLIENTINFO (proc parsedctcp from to)
  (let ((replyto (zenirc-extract-nick from))
        (sender (zenirc-run-hook 'zenirc-format-nickuserhost-hook from))
        help)
    (if (string= (cdr parsedctcp) "")
        (process-send-string proc (format "NOTICE %s :\C-aCLIENTINFO %s\C-a\n"
                                          replyto zenirc-clientinfo-string))
      (setq help (cdr (assq (intern-soft (cdr parsedctcp))
                            zenirc-clientinfo-list)))
      (if help
	  (process-send-string
	   proc (format "NOTICE %s :\C-aCLIENTINFO %s\C-a\n" replyto help))
	(zenirc-ctcp-errmsg "PRIVMSG" from to "CLIENTINFO"
                            (concat (cdr parsedctcp)
                                    " is not a valid function")
                            proc)))
    (and zenirc-verbose-ctcp
	 (zenirc-message proc 'ctcp_clientinfo sender to))))

;; handler for a ctcp ECHO query
(defun zenirc-ctcp-query-ECHO (proc parsedctcp from to)
  (process-send-string
   proc (concat "NOTICE "
		(zenirc-extract-nick from)
		" :\C-aECHO"
		(if (string= "" (cdr parsedctcp))
		    ""
		  (concat " " (cdr parsedctcp)))
		"\C-a\n"))
  (if zenirc-verbose-ctcp
      (zenirc-message proc 'ctcp_echo
                      (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)
                      to (cdr parsedctcp))))
  

;; handler for a ctcp ERRMSG query
(defun zenirc-ctcp-query-ERRMSG (proc parsedctcp from to)
  (process-send-string
   proc (concat "NOTICE "
                (zenirc-extract-nick from)
                " :\C-aERRMSG"
                (if (string= "" (cdr parsedctcp))
                    ""
                  (concat " " (cdr parsedctcp) " :No error"))
                "\C-a\n"))
  (if zenirc-verbose-ctcp
      (zenirc-message proc 'ctcp_errmsg
                      (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)
                      to)))

;; handler for a ctcp FINGER query
(defun zenirc-ctcp-query-FINGER (proc parsedctcp from to)
  (process-send-string
   proc (concat "NOTICE "
		(zenirc-extract-nick from)
		" :\C-aFINGER :" zenirc-fingerdata "\C-a\n"))
  (if zenirc-verbose-ctcp
      (zenirc-message proc 'ctcp_finger
                      (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)
                      to)))

;; handler for a ctcp TIME query
(defun zenirc-ctcp-query-TIME (proc parsedctcp from to)
  (process-send-string
   proc (concat "NOTICE "
		(zenirc-extract-nick from)
		" :\C-aTIME :" (current-time-string) "\C-a\n"))
  (if zenirc-verbose-ctcp
      (zenirc-message proc 'ctcp_time
                      (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)
                      to)))

;;; reply handlers

;; handler for a ctcp PING reply
(defun zenirc-ctcp-reply-PING (proc parsedctcp from to)
  (let ((current (car (cdr (zenirc-time-to-int (current-time-string))))))
    (if (< current
	   (string-to-int (cdr parsedctcp)))
 	(setq current (+ 65536 current)))
    (zenirc-message proc 'ctcp_ping_reply
                    (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)
                    (- current (string-to-int (cdr parsedctcp))))))


(defun zenirc-bug ()
  "Send a bug report to the ZenIRC maintainers."
  (interactive)
  (require 'sendmail)
  (mail nil zenirc-bug-address
	(format "Found bug in zenirc %s" zenirc-version))
  (goto-char (point-max))
  (insert
   (emacs-version) "\n\n"
   "Describe the bug you encountered as well as you can.\n"
   (substitute-command-keys
    "When you're done press \\[mail-send-and-exit] to send the message.\n\n")))


(provide 'zenirc)

(zenirc-lang-define-english-catalog)

;;; zenirc.el ends here
