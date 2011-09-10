;;;
;;;
;;; zenirc-example.el --- Example customizations for zenirc.el

;;; Copyright (C) 1993, 1994 Ben A. Mesander
;;; Copyright (C) 1993, 1994, 1996, 1997, 1998 Per Persson

;;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;;;         Per Persson <pp@sno.pp.se>
;;; Maintainer: Per Persson <pp@sno.pp.se>
;;; Keywords: extensions
;;; Created: 1993/06/03

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
;;; program's maintainer or write to: The Free Software Foundation,
;;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; A good way to use this is to add something like
;; (autoload 'zenirc "/home/pp/.zenirc-example" "Major mode to waste time" t)
;; to your ~/.emacs file. Then edit and copy this file there. If you do that
;; you also need to uncomment the last line of this file.

;; if the autoload says ".zenirc-example", the file needs to be named
;; ".zenirc-example.el", this is probably the best way for you to name
;; it. (sure, go ahead and rename it, don't blame me).

;; Also take a look on all the different scripts not mentioned in this file,
;; they might give you something you'll love.

;;; Code:

;; this is a list of IRC servers you use
;; it consists of servername, portnumber, password, nickname, username
(setq zenirc-server-alist 
      '(("irc.funet.fi")
	("cs-pub.bu.edu" 6666)
	("irc.stealth.net" 6667 nil "ben" "oedipus rpc.rexd")))

;; this is what you reply to CTCP USERINFO
(setq zenirc-userinfo "Oink.")

;; this is a list of annoying things to ignore. This list ignores
;; messages from nickserv, anything with the word "fnord" in it,
;; messages from the major dweeb craig and everything with more then
;; four CTCPs in it.
(setq zenirc-ignore-list 
      '("^:NickServ!Nickserv@hpsystem2.informatik.tu-muenchen.de" "fnord"
	"^:craig!craig@netsys1.netsys.com"     
	"\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a[^\C-a]*\C-a"))

;; zenirc can beep when it notices something, 
;; nil -> never beep
;; t -> beep when message not seen
;; 'always -> beep on all signals
(setq zenirc-beep-on-signal nil)

;; with zenirc-beep-on-signal turned on, 
;; this will make zenirc beep when it sees a ^G ala ircII and when you recieve
;; a private PRIVMSG
;(setq zenirc-signal-list '("" 
;                            "^\\([^ ]\\)* PRIVMSG [^#&+]\\([^ ]\\)* :"))

;; this is how you want ZenIRC to send confirmations
;; "nil" is no confirmation
;; "t" is confirmation in buffer
;; "'message" is confirmation in echo area
(setq zenirc-send-confirmation t)

;; if you want timestamps on PRIVMSG/NOTICE or not
;; with default prefix and suffix it looks like
;;	*ben[13:31]* lets have some fun
;;	<ben#twilight_zone[13:32]> SLUGS AND KNIGHTS! SLUGS AND KNIGTS!
(setq zenirc-timestamp nil
      zenirc-timestamp-prefix "["
      zenirc-timestamp-suffix "]")

;; in ircII, the channelname isn't shown in PRIVMSGs if you're currently
;; talking to the channel, setting zenirc-always-show-channelname to nil
;; gives you the same effect.
(setq zenirc-always-show-channelname nil)

;; if WHOIS returns no-such-nick, setting this variable to t will make 
;; the client automaticall issue an WHOWAS command
(setq zenirc-whowas-on-401 nil)

;; setting this variable to t will make ZenIRC remove preceding whitespaces
;; before a command, that is;
;;   /whois omnion
;; will turn in to;
;; /whois omnion
;; but;
;;   oink!
;; will turn in to;
;;   oink!
;; yeah, there's no difference there.
(setq zenirc-delete-preceding-whitespaces nil)

;; if you want ZenIRC to send out ERRMSG on bogus CTCP queries
(setq zenirc-send-ctcp-errmsg-on-unknown t)
;; if you awnt ZenIRC to send out ERRMSG on unbalanced CTCP queries
(setq zenirc-send-ctcp-errmsg-on-unbalanced t)

;; if you want ZenIRC to tell you when send out CTCP replies
(setq zenirc-verbose-ctcp t)

;; what ZenIRC replies on CTCP FINGER
(setq zenirc-fingerdata
      (format "%s <%s@%s>" (user-full-name) (user-real-login-name) 
	      (system-name)))

;; commandkey in ZenIRC
(setq zenirc-command-char ?/)

;;; use the following to surpress AWAY info if seen more then once

;(load-library "zenirc-away")

;;; use the following to make ZenIRC queue commands for you, to get around
;;; stupid flood controls

;(load-library "zenirc-command-queue")

;;; use the following to have tab-completion in ZenIRC

;(load-library "zenirc-complete")

;;; use the following to be able to act upon DCC CHAT and DCC SEND requests

;(load-library "zenirc-dcc")

;;; use the following to make ZenIRC fill incoming messages for you

;; if ZenIRC should fill incoming lines
;(setq zenirc-fill-mode t)
;; if ZenIRC should fill outgoing lines
;(setq zenirc-fill-outgoing-mode t)
;; how ZenIRC should fill things
;; (zenirc-wrap-region) adds zenirc-fill-prefix on each line
;(setq zenirc-fill-region-function 'zenirc-wrap-region)
;(setq zenirc-fill-prefix "   ")
;; (zenirc-wrap-region-dynamic) adds spaces at beginning of line, 
;; depending on length of first word
;(setq zenirc-fill-region-function 'zenric-wrap-region-dynamic)
;; (zenirc-wrap-region-static) adds spaces uses zenirc-fill-static 
;; columns to the left to display <nick#channel> and the rest to 
;; actual messages.
;(setq zenirc-fill-region-function 'zenirc-wrap-region-static)
;(setq zenirc-fill-static 26)
;(load-library "zenirc-fill")

;;; use the following to make ZenIRC format things like no-other-client 
;;; is able to. the first time a nick is seen, it's shown as 
;;; `nick!user@host'... after the first time it's just shown as `nick'
;;; until the `user@host' part changes.

;(load-library "zenirc-format")

;;; use the following to get history functions on C-cC-p and C-cC-n

;(load-library "zenirc-history")

;;; use the following to get an ircII like /ignore command

;(load-library "zenirc-ignore")

;;; use the following to make ZenIRC output netsplits/netjoins nicer

;(load-library "zenirc-netsplit")

;;; use the following to get an ircII like /notify command

;; a list of notificated people
;(setq zenirc-notify-list 
;      '("oddy" "piker" "flashman"))
;(load-library "zenirc-notify")

;;; use the following to make ZenIRC popup buffers when things happen

;(load-library "zenirc-popup")

;;; use the following if you want ZenIRC to act automatically when it
;;; sees a certain string in a PRIVMSG. if this sounds interesting, read 
;;; the comments in zenirc-trigger.el and take a look at zenirc-yow.el,
;;; zenirc-oink.el, zenirc-meditate.el, zenirc-8ball.el, zenirc-shop.el
;;; and zenirc-fortran.el

;(load-library "zenirc-trigger")

;; the following is an example of how to do something during initializing a
;; server connection. 001 is the first thing the server sends to a client
;; after the client sends USER and NICK.
;;
;; :pfawww.pp.se 001 Omnion :Welcome to the Internet Relay Network Omnion
;;
;; current code from Eric Prestemon <ecp@io.com>

(defvar zenirc-startup-channels-alist '(("debian" . "#debian")
					("uoknor.edu" . nil)
					(".*" . "#perl"))
  "*Pairs of server and channels.
server is a regexp
channels is a comma separated string of channels to join during 
 startup where  nil means none")

(defun zenirc-startup-join (proc parsedmsg)
  (let ((channelpairs (copy-alist zenirc-startup-channels-alist)))
    (while channelpairs
      (if (string-match (car (car channelpairs)) zenirc-server)
	  (progn
	    (if (cdr (car channelpairs))
		(process-send-string proc
				     (concat "JOIN " 
					     (cdr (car channelpairs)) 
					     "\n")))
	    (setq channelpairs nil))
	(setq channelpairs (cdr channelpairs))))))      

(zenirc-add-hook 'zenirc-server-001-hook 'zenirc-startup-join)

;;
;; the following is an example of adding a new ctcp reply type to zenirc.
;; in this case, it is "BOZOS", which returns a list of people who have
;; been bozotic with me
;;

;; create a hook to be called and assign it a default value
(defvar zenirc-ctcp-query-BOZOS-hook '(zenirc-ctcp-query-BOZOS))

(setq zenirc-clientinfo-list
'((ACTION . "ACTION contains action descriptions for atmosphere")
;; this is the addition to the default list
  (BOZOS . "BOZOS returns a list of people who have been bozotic with me")
  (CLIENTINFO . "CLIENTINFO gives information about available CTCP commands")
  (ERRMSG . "ERRMSG returns error messages")
  (PING . "PING returns the arguments it receives")
  (FINGER . "FINGER shows real name, and login name of user (idle time is not yet implemented in ZenIRC)")
;; sojge sure is a wordy bastard.
  (SOURCE . "takes 0 arguments and returns a description of where to find the source code of the client. The description is made up out of zero or more lines followed by an end marker. Every line is a CTCP reply with the SOURCE keyword, a space, the name of a FTP-server, a colon, a directory name, a colon, and 0 or more file names. If no file names are given, all the files in the named directory are needed. The end marker contains just the keyword.")
  (TIME . "TIME tells you the time on the user's host")
  (USERINFO . "USERINFO returns user settable information")
  (VERSION . "VERSION shows client type, version, and environment")))

;; note addition of BOZOS--------------*****
(setq zenirc-clientinfo-string "ACTION BOZOS CLIENTINFO ERRMSG FINGER PING SOURCE TIME USERINFO VERSION :Use CLIENTINFO <COMMAND> to get more specific information")

;;
;; this is the handler for a CTCP BOZOS query
;;
(defun zenirc-ctcp-query-BOZOS (proc parsedctcp from to)
  ;; if verbose ctcp is on, tell the user we got the query
  (if zenirc-verbose-ctcp
      (zenirc-message 
       proc 
       (format "[info] responding to BOZOS query from %s to %s\n" 
	       (zenirc-run-hook 'zenirc-format-nickuserhost-hook from) to)))
  ;; send a reply to the BOZOS query
  (process-send-string 
   proc 
   (concat "NOTICE  " (zenirc-extract-nick from)
	   " :\C-aBOZOS phone poxav veep noah rmtodd dmarcher lila neil notused laura w jason belladona mycroft amazin CHRISTIAN omnion fn vuori nap\C-a\n")))

;; this is the second last line of the file, the next line is the last one
(require 'zenirc)