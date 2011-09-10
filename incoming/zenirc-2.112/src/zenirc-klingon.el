;;; zenirc-klingon.el 
;;; Waste time on Internet Relay Chat with badly translated Klingon.

;;; Copyright (C) 1993, 1994 Ben A. Mesander
;;; Copyright (C) 1998 Per Persson

;; Author: David M. Archer <dmarcher@gnu.ai.mit.edu>
;; Maintainer: pp@sno.pp.se
;; Keywords: extensions, zenirc
;; Created: 1995-05-07

;; $Id: zenircrcsblahblahhere Exp $

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

;;; Code:

(require 'zenirc)

(zenirc-lang-define-catalog 
 'klingon
 '((s001 . "[De'] batlhlIj bIchIlqu'lI'")  ; wasting time
	 (s002 . "[De'] %s tengchaH QumwI'qoqvam lo' %s") ; server is
	 (s003 . "[De'] %s ghItlh tengchaH QumwI'qoqvam") ; ??
	 (s200 . "[De'] %s (%s) rarwI' -> %s") ; Version jangwI' from /trace
	 (s202 . "[De'] %s H.S. -> %s")
	 (s203 . "[De'] %s Sovbe'wI' -> IP address: %s") ; Unknown connection
	 (s204 . "[De'] %s DenIb Qatlh -> %s") ; Operator connection
	 (s205 . "[De'] %s lo'wI' -> %s") ; User connection
	 (s206 . "[De'] %s tengchaH QumwI'qoq -> %s %s %s %s ") ; Server connection
	 (s208 . "[De'] %s %s -> %s") ; New type connection
	 (s209 . "[De'] %s buv -> %s = %s") ; What the classes means
	 (s211 . "[De'] %s rartaH lup %s\nSent: %s/%s, Rcvd: %s/%s, SendQ: %s")
	 (s212 . "[De'] %s\t->\ttimes: %s\tbytes: %s") ; Command stats
	 (s213 . "[De'] C hst/nme/prt/cls: %s/%s/%s/%s")      ; C-lines
	 (s214 . "[De'] N hst/nme/prt/cls: %s/%s/%s/%s") ; N-lines
	 (s215 . "[De'] %s host/name/class:\t%s/%s/%s") ; I-lines
	 (s216 . "[De'] K host/username:\t%s/%s") ; K-lines
	 (s217 . "[De'] Q %s/%s/%s/%s/%s") ; Q-lines
	 (s218 . "[De'] Class: %s Ping freq: %s Conn.freq: %s Max Links: %s Sendq: %s") ; Y-lines
	 (s219 . "[De'] /stats pItlh")
	 (s221 . "[De'] %s soHbeHwI'")
	 (s241 . "[De'] LEAF hostmask/depth:\t\t%s/%s") ; L-lines
	 (s242 . "[De'] %s") ; Uptime of server
	 (s243 . "[De'] O pongname/user@host:\t%s/%s") ; O-lines
	 (s244 . "[De'] HUB  hostmask/servername:\t%s/%s") ; H-lines
	 (s251 . "[De'] naDev tengchaHmay QumwI'meyqoq %s lo'lI' lo'wI'pu' So'be' je %s lo'wI'pu' So' %s tu'lu'")
	 (s252 . "[De'] naDev DenIbya'pu' Qatlh %s tu'lu'") ; # opers
	 (s253 . "[De'] naDev rarwI' Sovbe' %s tu'lu'") ; # links ?
	 (s254 . "[De'] naDev Se'mey %s tu'lu'") ; # channels
	 (s255 . "[De'] naDev lo'WI'pu' %s je tengchaHmay QumwI'meyqoq %s tengchaH QumwI'qoqvam rartaH tu'lu'")
	 (s256 . "[De'] De' loH %s:") ; /admin line 1
	 (s257 . "[De'] %s") ; /admin line 2
	 (s258 . "[De'] %s") ; /admin line 3
	 (s259 . "[De'] %s") ; /admin line 4
	 (s261 . "[De'] %s File -> %s %s") ; Logfile trace
	 (s301 . "[De'] %s naDevDaq ghaHbe': %s") ; away
	 (s302 . "[De'] userhost: %s") ; userhost jangwI'
	 (s303 . "[De'] batlhlIj chIlqu'lI' %s") ; ison (losing honor)
	 (s305 . "[De'] naDevDaq soHbe'") ; you are not here
	 (s306 . "[De'] naDevDaq soH") ; you are here
	 (s311 . "[De'] %s (%s@%s) %s ghaH") ; user part of /whois list
	 (s312 . "[De'] %s tengchaH Qumwl'qoq lo'lI' %s (%s) ghaH")
	 (s313 . "[De'] %s DenIbya' Qatlh ghaH") ; /whois operator status
	 (s314 . "[De'] %s (%s@%s) %s ghaHta'law'") ; user part of /whowas list
	 (s315 . "[De'] /who pItlh")
	 (s317 . "[De'] %s has been idle %s") ; /whois idle time
	 (s318 . "[De'] /whois pItlh")
	 (s319 . "[De'] %s lo'lI' Se'mey %sghaH") ; channel part of whois data
	 (s321 . "[De'] Se'           lo'wI'pu'  qech") ; header for LIST cmd
	 (s322 . "[De'] %-15s %-5s %s")  ; each channel in LIST cmd
	 (s323 . "[De'] /list pItlh")  ; trailer for LIST cmd
	 (s324 . "[De'] %s 'oHbeHwI' %s %s") ; channel mode
	 (s331 . "[De'] %s Se' 'oHbe' qech") ; no topic message
	 (s332 . "[De'] %s qech: %s")   ; topic message
	 (s333 . "[De'] %s qech %s ghaHbeHta' %s") ; topic set time
	 (s341 . "[De'] %s DarI'meH %s") ; invite 
	 (s342 . "[De'] %s Datlhob batlh chIl") ; summon (ask to lose honor)
	 (s351 . "[De'] tengchaH QumwI'qoq lo': %s %s %s") ; version jangwI'
	 (s352_header . "[De'] pong     Dotlh  Se'      lo'wI'@'ejyo' (toghwI' chuq  pong)") ; header for /who list jangwI\'
	 (s352 . "[De'] %-9s %-3s  %-15s %s@%s (%s)") ; /who list jangwI\'
	 (s353 . "[De'] %s lo'wI'pu' %s") ; displayed after Bahnhof eintritt
	 (s364 . "[De'] %s %s %s")       ; /links 
	 (s365 . "[De'] /links pItlh")  ; end of /links 
	 (s367 . "[De'] %s ghImwI' %s")      ; banlist 
	 (s368 . "[De'] QonoS ghImwI' pItlh") ; end of banlist 
	 (s371 . "[De'] %s")             ; De' 
	 (s372 . "[motd] %s")		; message of the day
	 (s375 . "[motd] jabbI'ID'a' tengchaH QumwI'qoqvam:") ; start of motd
	 (s376 . "[motd] motd pItlh")    ; displayed at end of motd
	 (s381 . "[De'] DenIbya' Qatlh soH") ; you are an oper
	 (s382 . "[De'] Hujqa'lI': %s")  ; rehash server msg
	 (s391 . "[De'] %s tengchaH QumwI'qoqvam poH: %s") ; server's time
	 (s392 . "[De'] png     QumwI'    'ejDo'") ; header for users rpl
	 (s393 . "[De'] %s")             ; body of users rpl
	 (s395 . "[De'] lo'wI'pu' pagh") ; nobody for users rpl
	 (s401 . "[De'] pong joq Se' not: %s") ; there is no such pong/chan
	 (s402 . "[De'] tengchaH QumwI'qoq not: %s") ; there is no such server
	 (s403 . "[De'] Se' not: %s") ; there is no such Bahnhof
	 (s404 . "[De'] %s DangeHlaHbe'") ; can't send to channel.
	 (s405 . "[De'] tlhaqlIj chu'Ha'lu'pu': %s") ; you're on too many channels
	 (s406 . "[De'] %s pong Sovbe'wI'") ; no whowas data
	 (s407 . "[De'] HevwI' rap  ngeHHa'lu'pu': %s") ; user@host
	 (s409 . "[De'] mung nobHa'ta'") ; ping error 
	 (s411 . "[De'] HevwI' nobHa'ta'") ; no recipient given
	 (s412 . "[De'] mu' nobHa'ta'") ; you didn't send anything.
	 (s413 . "[De'] No toplevel domain: %s") ; no toplevel domain spec
	 (s414 . "[De'] Wildcard in toplevel domain: %s") ; wild toplevel
	 (s421 . "[De'] jIHtaHbogh naDev vISovbe': %s") ; you sent server spam
	 (s422 . "[De'] motd not (DenIbya' Qatlh tIbach /admin)")
	 (s423 . "[De'] loH De' not. (DenIbya' Qatlh yIchup HoH'egh)")
	 (s431 . "[De'] pong not") ; you didn't provide a name
	 (s432 . "[De'] pong qab: %s")
	 (s433 . "[De'] pong lo'lI'vIS: %s")
	 (s436 . "[De'] pong paw': %s")
	 (s441 . "[De'] %s not %s") ; can't do it to those not present
	 (s442 . "[De'] %s Se' Dalo'be'") ; not on channel
	 (s443 . "[De'] %s lo'pa' Se' %s.") ; invite error
	 (s444 . "[De'] %s naDev not") ; SUMMON jangwI\'
	 (s445 . "[De'] DenIbya' Qatlh Duchaw'Ha' /summon")
	 (s446 . "[De'] DenIbya' Qatlh Duchaw'Ha' /users")
	 (s451 . "[De'] SuSovbe'pu'") ; gotta do the USER pong thing
	 (s461 . "[De'] jIHtaHbogh naDev vISovbe': %s") ; as 421
	 (s462 . "[De'] bItlhobbe' cha'logh") ; cannot USER twice
	 (s463 . "[De'] DenIbya' Qatlh Dararbe'qu'") ; cannot connect
	 (s464 . "[De'] jISaHbe'") ; bad PASS command
	 (s465 . "[De'] DenIbya' Qatlh Dararbe'qu'") ; creep
	 (s467 . "[De'] %s ngoq naDev") ; chan key set already
	 (s471 . "[De'] %s Damuvbe'qu' (lo'wI' vuS)") ; too many ppl
	 (s472 . "[De'] bIlughbe': %s") ; duh
	 (s473 . "[De'] %s Damuvbe'qu' (rI'Se'neH)") ; fascist nerds
	 (s474 . "[De'] %s Damuvbe'qu' (ghImwI')") ; you're banned
	 (s475 . "[De'] %s Damuvbe'qu' (ngoq qab)") ; bad key
	 (s481 . "[De'] DenIbya' Qatlh soHbe'") ; oper only
	 (s482 . "[De'] verengan soHbe'") ; chanop needed
	 (s483 . "[De'] nuqneH") ; can't kill a server
	 (s491 . "[De'] DenIbya' Qatlhpu' naDevbe'") ; no o-line
	 (s501 . "[De'] bIlughbe'") ; you did something silly
	 (s502 . "[De'] bIlughbe'") ; as above
	 (action . "(%s Davengta')") ; ctcp action sent
	 (action-echo . "(%s Davengta')") ; ctcp action sent
	 (connect-failed . "[Qagh] rarwI' mevlu'ta' %s %d not  QIj: %s")
	 (connect-try . "[De'] jol chu'lu' %s %d")
	 (connect-abort . "[De'] rarwI' mevlu'ta'")
	 (ctcp_action . "[vang->%s] %s %s") ; ctcp ACTION display
	 (ctcp_clientinfo . "[yu'wI'] CLIENTINFO %s lIghaH %s")
	 (ctcp_errmsg . "[yu'wI'] ERRMSG %s lIghaH %s")
	 (ctcp_finger . "[yu'wI'] FINGER %s lIghaH %s")
	 (ctcp_ping . "[yu'wI'] PING %s lIghaH %s")
	 (ctcp_ping_reply . "[jangwI'] PING %s Duchuqlaw' lup %s")
	 (ctcp_source . "[yu'wI'] SOURCE %s lIghaH %s")
	 (ctcp_time . "[yu'wI'] TIME %s lIghaH %s")
	 (ctcp_username . "[yu'wI'] USERNAME %s lIghaH %s")
	 (ctcp_version . "[yu'wI'] VERSION %s lIghaH %s")
	 (debug  . "[reghuluS 'Iwghargh DaHoHlI'] %s"); displayed by debug code
	 (error . "[%s] %s")              ; server error message
	 (invite . "[De'] %s DurI'lI' %s.") ; invite (you are being hailed)
	 (join_you . "[De'] %s Damuvta'")
	 (join . "[De'] %s muvta' %s")
	 (kick . "[De'] %s: %s vo'ta' %s") ; someone was peeved
	 (kick_you . "[De'] %s Duvo'ta' %s") ; loser
	 (kill . "[De'] HovDaq lupqu'ta': %s") ; you've been killed (transported into star!)
	 (mode . "[De'] %s ghaHbeHchoH %s: %s") ; MODE change
	 (nick . "[De'] %s pong ghaHchoH %s") ; pong change
	 (newcatalog . "[De'] vIjatlhlaH %s")
	 (nocatalog . "[Qagh] vIjatlhlaHbe' %s")
	 (nosend . "[De'] romuluSngan Daghajbe'") ; msg not sent
	 (notice . "{%s%s} %s")           ; NOTICE
	 (notice_you . "{%s} %s")         ; NOTICE sent to your pong
	 (notify_list . "[De'] QonoS ghoq: %s")
	 (notify_on . "[De'] %s batlhlchaj chIlqu'lI'")
	 (notify_off . "[De'] %s batlhlchaj chIlqu'lI'be'")
	 (now_querying . "[De'] %s romuluSnganlI' DaghajchoH") ; /query foo
	 (part_you . "[De'] %s Damejta' (%s)") ;
	 (part . "[De'] %s mejta' %s (%s)") ; part from Bahnhof message
	 (pong . "[De'] %s chuS")  ; pong message from server
	 (privmsg . "<%s%s> %s")          ; PRIVMSG
	 (privmsg_you . "*%s* %s")        ; PRIVMSG sent to your pong
	 (protocol_violation . "[error] The following line is in violation of the IRC protocol.\n[error] Please tell the server administrator:\n%s: %s")
	 (query . "[yu'wI'] %s lIghaH %s ngaSwI': %s") ; ctcp yu'wI'
	 (query_unknown . "CTCP yu'wI' not")
	 (query_unbalanced . "[tlhaQ Yu'WI'] %s lIghaH %s ngaSwI': %s")
	 (query_unbalanced_reply . "CTCP tlhaQ Yu'WI'")
	 (quit . "[De'] %s lupta': %s") ; user signoff
	 (reply . "[jangwI'] %s lIghaH %s ngaSwI': %s") ; ctcp jangwI'
	 (reply_unbalanced . "[tlhaQ JangwI'] %s lIghaH %s ngaSwI': %s")
	 (send . "(%s DangeHta')") ; you sent a message/notice
	 (send-echo . "(%s DangeHta')") ; you sent a message/notice
	 (sentinel . "\nZenIRC Hegh: %s") ; process sentinel message
	 (server . "[QumwI'] %s")         ; unknown server message
	 (signal . "[%s DurI'lI']")        ; signal in echo area
	 (topic . "[De'] %s choHta' qech %s: %s") ; topic message
	 (wallops . "-%s- %s")            ; WALLOPS notice
	 ))

(provide 'zenirc-klingon)

;;; zenirc-klingon.el ends here

;;; notes:

; lupta'  transport (done)
; Se'  frequency
; Se'mey frequencies
; DenIbya' Qatlh / Denlbya'pu' Qatlh
; naDev DenIbya'pu' Qatlh tu'lu' here (Denlbya'pu') notice(indef)
; tengchaH space station
; QumwI' communications device
; qoq (so-called)

; rarwI' "that which connects" link
; Sovbe'wI' "that which is not known" unknown

; tengchaH QumwI'qoq        server
; tengchaH QumwI'qoqvam     this server
; tengchaHmay Qumwl'meyqoq  servers

; ngeHHa'lu'pu'  nothing sent.

; ghaH him/her
; lo'wI'  user (one who uses)  lo'wI'pu'  (users)
; pItlh done
; ghImwI'  ban (that which exiles)
; soHbeHwI'    your mode 

; muv   join   muvta'    (has joined)


;HIja'	yes
;ghobe'	no
;toH	Well! Aha!
;chay'	How did this happen? / What's going on?
;jIyajbe'	I don't understand.
;jISaHbe'	I don't care.
;qay'be' 	No problem!
;tlhIngan Hol DajatIh'a' Do you speak Klingon?
;tlhIngan Hol vIjatIhlaHbe'	I cannot speak Klingon.
;tlhIngan Hol vIjatIhlaH	I can speak Klingon.
;jIleSnIS	I need rest.
;bIleSnIS	You need rest.
;lu'	Ok.
;naDev qaS wanI' ramqu' There's nothing happening here.
;naDev vo' yIghoS	Go away!
;nuqneH	What do you want? (greeting)
;bIjatlh' e' ylmev Shut up.
;naDev tlhInganpu' tu'lu' There are Klingons around here.
