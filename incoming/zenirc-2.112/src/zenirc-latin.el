;;; zenirc-latin.el --- Latin message catalog for ZenIRC

;; Copyright (C) 1993, 1994 Ben A. Mesander
;; Copyright (C) 1998 Per Persson

;; Author: Richard Todd <rmtodd@servalan.servalan.com>
;; Maintainer: pp@sno.pp.se
;; Keywords: extensions
;; Created: 1994-02-28

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

;; "i like latin-mode, it's like operating a trojan horse or something."
;;   -- Dave Archer <dmarcher@acsu.buffalo.edu>

;; "with zenirc-latin-mode, a netsplit reminds me of the part in carmina
;;  burana where everyone's drinking." -- Dave Archer

;; "god, i love the latin mode.. makes me feel like caligula" -- James Price

;;; Code:

(require 'zenirc)

(zenirc-lang-define-catalog 'latin
 '((join_you . "[info] Apud %s iniis.") ; your channel join message
   (join . "[info] %s apud %s iniit.") ; channel join message
   (s001 . "[info] Tempus tuum perdis.") ; welcome to irc message
   (s002 . "[info] Servum IRC tuum %s (%s) est.") ; server name & version #
   (s003 . "[info] Diem %s servum factum est.") ; when the server was built
   (s211 . "[info] %s connectio %s momentibus erat.\nMissi : %s/%s, Recepti: %s/%s, Mittendi: %s")
   (s212 . "[info] %s\t->\ttempus: %s\tbytes: %s") ; command stats
   (s215 . "[info] %s machina/nomen:\t%s/%s") ; I-lines
   (s216 . "[info] K machina/nomen clientis:\t%s/%s") ; K-lines
   (s219 . "[info] /stats perfectus est.") ; end of /stats
   (s221 . "[info] Tuus modus  %s nunc est.") ; user mode
   (s241 . "[info] LEAF hostmask/altitudo:\t\t%s/%s") ; L-lines
   (s243 . "[info] %s cognomen/login@machina:\t%s/%s") ; O-lines or o-lines
   (s244 . "[info] HUB  hostmask/nomen servi:\t%s/%s") ; H-lines
   ;; # users on connect message
   (s251 . "[info] %s clientes visibiles et %s clientes invisibles apud %s serva sunt.")
   (s252 . "[info] %s nothi pravi adsunt.") ; irc operators msg
   (s253 . "[info] %s conjunctiones incognitae sunt.") ; unk connects msg
   (s254 . "[info] %s fora sunt.")    ; number of channels
   ;; # of clients and servers
   (s255 . "[info] %s clientes et %s serva huic servo loquiuntur.")
   (s256 . "[info] Descriptio procurationis %s:") ; admin info
   (s301 . "[info] %s abest: %s")     ; someone is away
   (s302 . "[info] identificatio: %s.") ; userhost reply
   (s303 . "[info] Tempus perdit nunc: %s") ; ison reply
   (s305 . "[info] Nuntio te non abesse.")
   (s306 . "[info] Nuntio te abesse.")
   (s311 . "[info] %s (%s@%s) %s est.") ; user part of /whois list
   (s312 . "[info] %s apud servum %s (%s)") ; server part of /whois list
   (s313 . "[info] %s nothus pravus est.") ; /whois operator status
   (s314 . "[info] %s (%s@%s)  %s erat.") ; user part of /whowas list
   (s315 . "[info] /who perfectus est.") ; end of /who list replies
   (s318 . "[info] /whois perfectus est.") ; end of /whois list replies
   (s317 . "[info] %s per %s quietus erat.") ; /whois idle time
   (s319 . "[info] %s adest in: %s")  ; channel part of whois data
   (s321 . "[info] Forum\tQuot Clientes?\tRes") ; header for LIST cmd
   (s322 . "[info] %s\t%s\t%s")       ; each channel in LIST cmd
   (s323 . "[info] /list perfectus est.") ; trailer for LIST cmd
   (s324 . "[info] Modus %s: %s %s.") ; channel mode
   (s331 . "[info] %s nullam rem habet.") ; no topic message
   (s332 . "[info] Res %s : %s")      ; topic message
   (s341 . "[info] %s in %s invitas.") ; invite reply
   (s342 . "[info] %s tempus perdere rogas.") ; summon reply
   (s351 . "[info] Version: %s %s %s.") ; version reply
   ;; header for /who list reply
   ;; Bleah, Latin doesn't really have a good short word for 'nickname', so
   ;; we'll invent one.
   (s352_header . "[info] Cognomen Status Nomen Fori     Login et Machina        Nomen")
   (s352 . "[info] %-9s %-3s  %-15s %s@%s (%s)") ; /who list reply
   (s353 . "[info] Homines in %s: %s") ; displayed after channel join
   (s364 . "[info] %s %s %s")         ; /links reply
   (s365 . "[info] /links perfectus est.") ; end of /links reply
   (s367 . "[info] %s interdicit %s.") ; banlist reply
   (s368 . "[info] Tabula interdictionum perfecta est.") ; end of banlist
   (s371 . "[info] %s")               ; info reply
   (s372 . "[motd] %s")               ; message of the day
   (s375 . "[motd] Nuntius hodiernus:") ; start of motd
   (s376 . "[motd] Nuntius hodiernus perfectus est.") ; end of motd
   (s381 . "[info] Nunc nothus pravus est.") ; irc op status
   ;; Yeah, right, like there's a word for `rehash' in Latin.
   (s382 . "[info] Rehashit: %s")
   (s391 . "[info] Dies apud servum %s: %s.") ; TIME reply
   (s392 . "[info] Login Terminal  Machina") ; header for users rpl
   (s393 . "[info] %s")               ; body of users rpl
   (s395 . "[info] Nemo adest.")      ; nobody for users rpl
   (s401 . "[info] %s nec cognomen nec forum est.") ; no such nick/chan
   (s402 . "[info] Servum %s nescio.") ; there is no such server
   (s403 . "[info] Forum %s nescio.") ; there is no such channel
   (s404 . "[info] %s loqui non potes.") ; you can't send to channel
   (s405 . "[info] Plura fora quam loqui potes: %s.") ; too many channels
   (s406 . "[info] %s servum nescit.") ; no whowas data
   (s407 . "[info] Receptores duplices  ; nuntium non misit: %s") ; user@host
   (s411 . "[info] Nullus receptor adfuit.") ; no recipient given
   (s412 . "[info] Nullus nuntius adfuit.") ; you didn't send anything.
   (s413 . "[info] Nullum regnum supremum adfuit: %s") ; no toplevel domain
   (s414 . "[info] Regnum vitiosum est: %s.") ; wild toplevel
   (s421 . "[info] Stercus est: %s.") ; you sent server spam
   (s422 . "[info] Quidam nothus pravus imperitus est; nuntium hodiernum non fecit.")
   (s423 . "[info] Quidam nothus pravus imperitus est; procurationem non descripsit.")
   (s431 . "[info] Nullus cognomen adfuit.") ; you didn't provide a nick
   (s432 . "[info] Cognomen vitiosum est: %s.") ; invalid nick
   (s433 . "[info] Aliquis cognomen %s iam utebatur.") ; invalid nick
   (s436 . "[info] Concursus cognominum: %s.") ; nickicide
   (s441 . "[info] %s in %s non est.") ; can't do it to those not present
   (s442 . "[info] In %s non es.")    ; you can't do that dave.
   (s443 . "[info] %s iam in %s est.") ; invite error
   (s444 . "[info] %s non conjunctus est.") ; SUMMON reply
   ;; disabled summon
   (s445 . "[info] Te /summon uti aliqui nothus pravus non permittit.")
   ;; disabld users
   (s446 . "[info] Te /users uti aliqui nothus pravus non permittit.")
   (s451 . "[info] Nondum perscripsit.") ; gotta do the USER NICK thing
   (s461 . "[info] Parum parameteres: %s")
   (s462 . "[info] Non perscriptere iterum potes.") ; cannot USER twice
   ;; server refuses this client
   (s463 . "[info] Te connectere aliqui nothus pravus cerritus non permittit.")
   (s464 . "[info] Tessera vitiosa est.") ; bad PASS command
   (s465 . "[info] Hoc servam uti non tibi licet.") ; creep
   (s467 . "[info] Tessera huius fori iam adest.") ; chan key set already
   (s471 . "[info] Apud %s inire non potes (plures clientes).")
   (s472 . "[info] Modum %s nescio.") ; duh
   (s473 . "[info] Apud %s inire non potes (nulla invitatio est).")
   (s474 . "[info] Apud %s inire non potes. Interdiceris.") ; you're banned
   (s475 . "[info] Apud %s inire non potes (tesseram non habes).")
   (s481 . "[info] Hic solum nothos pravos licet.") ; oper only
   (s482 . "[info] Censor %s non es.")
   (s483 . "[info] Eh! Servum caedere non potes!") ; can't kill a server
   (s491 . "[info] Nulli nothi pravi ab tua machina permissi sunt.")
   (s501 . "[info] Hanc modum nescio.") ; you did something silly
   (s502 . "[info] Modum alii mutare non potes.") ; as above
   (ctcp_action . "[actio->%s] %s %s")
   (ctcp_clientinfo . "[quaestio] CLIENTINFO ab %s ad %s.")
   (ctcp_errmsg . "[quaestio] ERRMSG ab %s ad %s.")
   (ctcp_finger . "[quaestio] FINGER ab %s ad %s.")
   (ctcp_ping . "[quaestio] PING ab %s ad %s.")
   (ctcp_ping_reply . "[reponsum] PING %s %s momentibus abest.")
   (ctcp_source . "[quaestio] SOURCE ab %s ad %s.")
   (ctcp_time . "[quaestio] TIME ab %s ad %s.")
   (ctcp_userinfo . "[quaestio] USERINFO ab %s ad %s.")
   (ctcp_version . "[quaestio] VERSION ab %s ad %s.")
   (debug . "[debug] %s")             ; displayed by debugging code
   (error . "[%s] %s")                ; server error message
   (invite . "[info] %s te apud %s invitat.") ; invite
   (kick . "[info] %s de %s ejectum est per %s.") ; someone was peeved
   (kick_you . "[info] De %s ejectum es per %s.") ; loser
   (kill . "[info] Caesus es: %s.")     ; your time is up.
   (mode . "[info] %s modum de %s mutavit: %s") ; MODE change
   (nick . "[info] %s cognomen mutavit ad %s.") ; nick change
   (nosend . "[info] Nulla victima in nuntios habes.") ;
   (notice . "{%s%s} %s")             ; NOTICE
   (notice_you . "{%s} %s")           ; NOTICE sent to your nick
   (notify_list . "[info] Ei quos exspectas: %s") ;
   (notify_on . "[info] %s tempus perdere videbam.") ;
   (notify_off . "[info] %s tempus perdere non iam video.") ;
   (now_querying . "[info] Victima: %s.") ; /query foo
   (part_you . "[info] Exis de: %s (%s)") ; your part from channel message
   (part . "[info] %s exiit de %s (%s)")  ;part from channel message
   (privmsg . "<%s%s> %s")            ; PRIVMSG
   (privmsg_you . "*%s* %s")          ; PRIVMSG sent to your nick
   (query . "[quaestio] ab %s ad %s, res %s.") ; ctcp query
   (query_unknown . "Mandatum CTCP ignotum.") ; we don't grok this
   (query_unbalanced . "[mala quaestio] ab %s ad %s, res %s.")
   (query_unbalanced_reply . "Malum reponsum CTCP.") ; odd number of ^A's
   (quit . "[info] %s tempus perdere intermisit: %s") ; user signoff
   (reply . "[reponsum] ab %s ad %s, res %s.") ; ctcp reply
   (reply_unbalanced . "[malum reponsum] ab %s ad %s, res %s.") ; weird
   (send . "(ad %s missus est)")      ; you sent a message/notice
   (send-echo . "(ad %s missus est)")      ; you sent a message/notice
   (server . "[servum] %s")           ; unknown server message
   (signal . "[signum in %s]")          ; signal in echo area
   (topic . "[info] %s res %s mutavit: %s") ; topic message
   (wallops . "-%s- %s")              ; WALLOPS notice
   ))

(provide 'zenirc-latin)

;; zenirc-latin.el ends here
