;;; zenirc-swedish.el --- Swedish message catalog for ZenIRC

;; Copyright (C) 1993, 1994 Ben A. Mesander
;; Copyright (C) 1993, 1994, 1995, 1996, 1998 Per Persson

;; Author: Per Persson <pp@sno.pp.se>
;;         Ben A. Mesander <ben@gnu.ai.mit.edu>
;;         Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: pp@sno.pp.se
;; Keywords: extensions
;; Created: 1993/06/03

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

;; "it's like operating a nazi sub or something."
;;    --Ben A. Mesander <ben@gnu.ai.mit.edu>

;; If you're using emacs 19, loading iso-ascii.el or doing
;; M-x standard-display-european on X displays makes 8-bit characters
;; easier to see.

;;; Code:

(require 'zenirc)

(zenirc-lang-define-catalog
 'swedish
 '((join_you . "[info] Du deltar nu i mötet: %s")
   (join . "[info] %s deltar nu i mötet %s")
   (s001 . "[info] Du kastar bort tid.") ; welcome to irc message
   ;; server name & version # msg
   (s002 . "[info] Din IRK server är %s och kör IRKD version %s")
   ;; when the server was built
   (s003 . "[info] Den här IRK servern skapades %s")
   ;; # users on connect message
   (s251 . "[info] Det finns %s synliga och %s osynliga klienter på %s serverar.")
   (s200 . "[info] %s Länk -> version: %s") ; Version reply from /trace
   (s202 . "[info] %s H.S. -> %s")    ; Trace handshake
   (s203 . "[info] %s Hmmm -> IP address: %s") ; Unknown connection
   (s204 . "[info] %s Oper -> %s")    ; Operator connection
   (s205 . "[info] %s Klie -> %s")    ; User connection
   (s206 . "[info] %s Serv -> %s %s %s %s ") ; Server connection
   (s208 . "[info] %s %s -> %s")      ; New type connection
   (s209 . "Klas -> %s = %s")         ; What the classes means
   ;; Linkinfo
   (s211 . "[info] %s länk har varit uppe %s sekunder\nSänt %s/%s, Mottagit %s/%s, SändKö: %s")
   (s212 . "[info] %s\t->\tgånger: %s\tbytes: %s") ; Command stats
   (s213 . "[info] C msk/nam/prt/kls: %s/%s/%s/%s") ; C-lines
   (s214 . "[info] N msk/nam/prt/kls: %s/%s/%s/%s") ; N-lines
   (s215 . "[info] %s maskin/namn:\t%s/%s") ; I-lines
   (s216 . "[info] K maskin/användarnamn:\t%s/%s") ; K-lines
   (s217 . "[info] Q %s/%s/%s/%s/%s") ; Q-lines
   ;; Y-lines
   (s218 . "[info] Klass: %s Ping frek: %s Länk frek: %s Max länkar: %s SändKö: %s")
   (s219 . "[info] Slut på /stats.") ; End of /stats I guess
   (s221 . "[info] Din nuvarande status är: %s") ; user mode
   (s241 . "[info] LÖV  maskinmask/djup:\t\t%s/%s") ; L-lines
   (s242 . "[info] %s")               ; Uptime of server
   (s243 . "[info] %s smeknamn/användare@maskin:\t%s/%s") ; O- or o-lines
   (s244 . "[info] GREN maskinmask/servernamn:\t%s/%s") ; H-lines
   ;; # users on connect message
   (s251 . "[info] Det finns %s synliga och %s osynliga klienter på %s serverar.")
   (s252 . "[info] Det finns %s verkliga nollor aktiva.") ; irc operators msg
   (s253 . "[info] Det finns %s okända uppkopplingar.") ; unk connects msg
   (s254 . "[info] Det finns %s möten.") ; number of channels
   ;; # of clients and servers
   (s255 . "[info] Det finns %s klienter och %s servrar uppkopplade till den här servern.")
   (s256 . "[info] Administrativ information för %s:") ; /admin line 1
   (s257 . "[info] %s")               ; line 2 of admin information
   (s258 . "[info] %s")               ; line 3 of admin information
   (s259 . "[info] %s")               ; line 4 of admin information
   (s261 . "[info] %s Fil -> %s %s")  ; Logfile trace
   (s301 . "[info] %s är borta: %s") ; someone is away
   (s302 . "[info] användaraddress: %s") ; userhost reply
   (s303 . "[info] ` %s' slösar för närvarande tid.")
   (s305 . "[info] Du är nu tillbaka.")
   (s306 . "[info] Du är borta.")
   (s311 . "[info] %s (%s@%s) är %s") ; user part of /whois list
   ;; server part of /whois list
   (s312 . "[info] %s använder servern %s (%s)")
   (s313 . "[info] %s är en verklig nolla.") ; /whois operator status
   (s314 . "[info] %s (%s@%s) var %s") ; user part of /whowas list
   (s315 . "[info] Slut på /who")  ; end of /who list replies
   (s318 . "[info] Slut på /whois") ; end of /whois list replies
   (s317 . "[info] %s har varit inaktiv %d") ; /whois idle time
   (s319 . "[info] %s är på: %s") ; channel part of whois data
   (s321 . "[info] Möte\tAnvändare\tRubrik") ; header for LIST cmd
   (s322 . "[info] %s\t%s\t%s")       ; each channel in LIST cmd
   (s323 . "[info] Slut på /list") ; trailer for LIST cmd
   (s324 . "[info] Status för %s är %s %s") ; channel mode
   (s331 . "[info] %s har ingen rubrik.") ; no topic message
   (s332 . "[info] %s rubrik: %s")    ; topic message
   (s341 . "[info] Du inbjuder %s till %s") ; invite reply
   (s342 . "[info] Du ber %s att kasta bort tid.") ; summon reply
   (s351 . "[info] Version: %s %s %s") ; version reply
   ;; header for /who list reply
   (s352_header . "[info] Smeknamn  Stat Namn av Möte  Användare@adress        Namn")
   (s352 . "[info] %-9s %-3s  %-15s %s@%s (%s)") ; /who list reply
   (s353 . "[info] Klienter på %s: %s") ; displayed after channel join
   (s364 . "[info] %s %s %s")         ; /links reply
   (s365 . "[info] slut på /links") ; end of /links reply
   (s367 . "[info] %s portningar %s") ; banlist reply
   (s368 . "[info] slut på portningslistan.") ; end of banlist reply
   (s371 . "[info] %s")               ; info reply
   (s372 . "[motd] %s")		; message of the day
   (s375 . "[motd] Dagens meddelande:") ; start of motd
   (s376 . "[motd] Slut på dm.")   ; displayed at end of motd
   (s381 . "[info] Du är nu en verklig nolla.") ; irc op status
   (s382 . "[info] Åter brädgårdar: %s") ; rehash server msg
   (s391 . "[info] Lokal tid på servern %s är: %s") ; TIME reply
   (s392 . "[info] Användar# Terminal  Address") ; header for users rpl
   (s393 . "[info] %s")               ; body of users rpl
   (s395 . "[info] Ingen är påloggad.") ; nobody for users rpl
   ;; there is no such nick/chan
   (s401 . "[info] Det finns inget sådant smeknamn/möte: %s")
   (s402 . "[info] Ingen server med det namnet: %s") ; no such server

   (s403 . "[info] Inget möte med det namnet: %s") ; no such channel
   (s404 . "[info] Du kan inte sända till %s") ; can't send to channel
   (s405 . "[info] Du deltar redan i tio möten: %s") ; too many channels
   ;; no whowas data
   (s406 . "[info] Något sådant smeknamn finns inte i serverns databas: %s")
   ;; user@host
   (s407 . "[info] Dubbla mottagare. Inget meddelande sänt: %s")
   (s409 . "[info] Ingen sändare given.") ; ping error reply
   (s411 . "[info] Ingen mottagare given.") ; no recipient given
   (s412 . "[info] Ingen text att sända.") ; you didn't send anything.
   (s413 . "[info] Ingen toppnivå domain: %s") ; no toplevel domain spec
   (s414 . "[info] Jokertecken i toppnivå domain: %s")
   (s421 . "[info] Det här ser ut som nonsens för mig: %s")
   (s422 . "[info] Någon verklig nolla har inte tillräckligt hög IQ för att ha en dm fil.")
   (s423 . "[info] Någon verklig nolla på %s är ignorant nog att inte lägga upp administrations info.")
   (s431 . "[info] Inget smeknamn givet.") ; you didn't provide a nick
   (s432 . "[info] Ogiltigt smeknamn: %s") ; invalid nick
   (s433 . "[info] Smeknamnet %s används redan.") ; invalid nick
   (s436 . "[info] Smeknamns kollision: %s") ; nickicide
   ;; can't do it to those not present
   (s441 . "[info] %s är deltar inte i %s")
   (s442 . "[info] Du är deltar inte i %s") ; you can't do that dave.
   (s443 . "[info] %s deltar redan i mötet %s") ; invite error
   (s444 . "[info] %s är inte påloggad.") ; SUMMON reply
   (s445 . "[info] Någon verklig nolla låter dig inte använda /summon")
   (s446 . "[info] Någon verklig nolla låter dig inte använda /users")
   (s451 . "[info] Du har inte registrerat dig.")
   (s461 . "[info] Inte nog med parametrar: %s") ; as 421
   (s462 . "[info] Du får inte registrera dig flera gånger.")
   (s463 . "[info] Någon fascistisk nolla låter dig inte koppla upp dig.")
   (s464 . "[info] Passordet är inkorrekt.") ; bad PASS command
   (s465 . "[info] Du är inte omtyckt på den här server, byt genast ditt kräk.")
   ;; chan key set already
   (s467 . "[info] Nyckeln är redan satt för %s")
   ;; too many ppl
   (s471 . "[info] Du kan inte delta i %s (användar antalet överskridit).")
   (s472 . "[info] %s är en okänd status flagga.") ; duh
   (s473 . "[info] Du kan inte delta i %s (måste vara inbjuden).")
   (s474 . "[info] Du kan inte delta i %s (portad).") ; you're banned
   (s475 . "[info] Du kan inte delta i %s (fel kanal nyckel).") ; bad key
   (s481 . "[info] Du är inte en verklig nolla.") ; oper only
   (s482 . "[info] Du är inte nog mäktig att %s") ; chanop needed
   ;; can't kill a server
   (s483 . "[info] Blä, du kan inte döda en server.")
   ;; no o-line
   (s491 . "[info] Inga verkliga nollor tillåtna från din address.")
   (s501 . "[info] Okänd klient status flagga.")
   (s502 . "[info] Kan inte ändra status för andra klienter.")
   (action . "(skickat till %s=")
   (action-echo . "(skickat till %s)")
   (ctcp_action . "[action->%s] %s %s")
   (ctcp_clientinfo . "[fråga] CLIENTINFO från %s till %s")
   (ctcp_errmsg . "[fråga] ERRMSG från %s till %s")
   (ctcp_finger . "[fråga] FINGER från %s till %s")
   (ctcp_ping . "[fråga] PING från %s till %s")
   (ctcp_source . "[fråga] SOURCE från %s till %s")
   (ctcp_time . "[fråga] TIME från %s till %s")
   (ctcp_userinfo . "[fråga] USERINFO från %s till %s")
   (ctcp_version . "[fråga] VERSION från %s till %s")
   (debug . "[debug] %s")             ; displayed by debugging code
   (error . "[%s] %s")                ; server error message
   (invite . "[info] %s tycker att du borde komma till %s") ; invite
   (kick . "[info] %s har blivit sparkad från %s av %s")
   (kick_you . "[info] Du har blivit sparkad från %s av %s")
   (kill . "[info] Du har blivit mördad: %s") ; your time is up.
   (mode . "[info] %s har ändrat statusen för %s: %s") ; MODE change
   (nick . "[info] %s har bytt smeknamn till %s") ; nick change
   (nosend . "[info] du har inget nuvarande offer att sända till.")
   (notice . "{%s%s} %s")             ; NOTICE
   (notice_you . "{%s} %s")           ; NOTICE sent to your nick
   (notify_list . "[info] Din nuvarande radar lista: %s")
   (notify_on . "[info] Aha! %s slösar viktig tid.")
   (notify_off . "[info] Aha! %s slutade slösa viktig tid.")
   (now-querying . "[info] Nuvarande offer är %s") ; /query foo
   (part_you . "[info] Lämnar: %s (%s)") ; your part from channel message
   (part . "[info] %s har lämnat %s (%s)") ; part from channel message
   (pong . "[info] %s säger ojnk.")
   (privmsg . "<%s%s> %s")            ; PRIVMSG
   (privmsg_you . "*%s* %s")          ; PRIVMSG sent to your nick
   (query . "[fråga] från %s till %s innehållande %s") ; ctcp query
   (query_unknown . "är en okänd CTCP fråga.")
   (query_unbalanced . "[OBALANCERAD fråga] från %s till %s innehållande %s")
   (query_unbalanced_reply . "är en obalancerad CTCP fråga.")
   (quit . "[info] %s slutade slösa viktig tid: %s") ; user signoff
   (reply . "[svar] från %s till %s innehållande %s") ; ctcp reply
   (reply_unbalanced . "[OBALANCERAT svar] från %s till %s innehållande %s")
   (send . "(skickat till %s)")       ; you sent a message/notice
   (send-echo . "(skickat till %s)")       ; you sent a message/notice
   (server . "[server] %s")           ; unknown server message
   (signal . "[signal i %s]")           ; signal in echo area
   (topic . "[info] %s bytte rubriken för %s till: %s") ; topic message
   (wallops . "-%s- %s")              ; WALLOPS notice
   ))

(provide 'zenirc-swedish)

;;; zenirc-swedish.el ends here
