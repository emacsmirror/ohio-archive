;;; zenirc-finnish.el --- Finnish message catalog for ZenIRC

;; Copyright (C) 1995 Valtteri Vuorikoski
;; Copyright (C) 1998 Per Persson

;; Author: Valtteri Vuorikoski <vuori@sci.fi>
;; Maintainer: pp@sno.pp.se
;; Keywords: extensions
;; Created: 1995/05/23

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

;; If you're using emacs 19, loading iso-ascii.el or doing
;; M-x standard-display-european on X displays makes 8-bit characters
;; easier to see.

;;; Code:

(require 'zenirc)

(zenirc-lang-define-catalog
 'finnish
 '((s001 . "[info] Hukkaat aikaasi.")
   (s002 . "[info] Irkkiserverisi on %s ajaen ircd-versiota %s")
   (s003 . "[info] T�m� serveri luotiin %s")
   (s200 . "[info] %s (%s) Linkki -> %s") ; Version reply from /trace
   (s202 . "[info] %s H.S. -> %s")
   (s203 . "[info] %s Hmmm -> IP-osoite: %s") ; Unknown connection
   (s204 . "[info] %s Oper -> %s") ; Operator connection
   (s205 . "[info] %s K�ytt�j� -> %s") ; User connection
   (s206 . "[info] %s Serveri -> %s %s %s %s ") ; Server connection
   (s208 . "[info] %s %s -> %s") ; New type connection
   (s209 . "[info] %s Luokka -> %s = %s") ; What the classes means
   (s211 . "[info] %s linkki ylh��ll� %s sek\nL�hetetty: %s/%s, Vastaanotettu: %s/%s, SendQ: %s")
   (s212 . "[info] %s\t->\ttimes: %s\tbytes: %s") ; Command stats
   (s213 . "[info] C kone/nimi/port/luok: %s/%s/%s/%s")      ; C-lines
   (s214 . "[info] N kone/nimi/port/luok: %s/%s/%s/%s") ; N-lines
   (s215 . "[info] %s kone/nimi/luokka:\t%s/%s/%s") ; I-lines
   (s216 . "[info] K kone/k�ytt�j�:\t%s/%s") ; K-lines
   (s217 . "[info] Q %s/%s/%s/%s/%s") ; Q-lines
   (s218 . "[info] Luokka: %s Ping-taajuus: %s Connectaustaajuus: %s Max Linkkkej�: %s SendQ: %s") ; Y-lines
   (s219 . "[info] /statsien loppu.")
   (s221 . "[info] User-moodisi on: %s")
   (s241 . "[info] LEAF-serverin hostmaski/syvyys:\t\t%s/%s") ; L-lines
   (s242 . "[info] %s") ; Uptime of server
   (s243 . "[info] %s nickki/k�ytt�j�@kone:\t%s/%s") ; O-lines
   (s244 . "[info] HUB  hostmaski/serveri:\t%s/%s") ; H-lines
   (s251 . "[info] %s/%s n�kyv��/n�kym�t�nt� k�ytt�j�� %s:ll� serverill�.")
   (s252 . "[info] %s isoa moloa linjalla.")
   (s253 . "[info] %s tuntematonta yhteytt�.")
   (s254 . "[info] %s kannua")
   (s255 . "[info] %s clientti� and %s serveri� yhteydess� t�h�n serveriin")
   (s256 . "[info] Administratiivinen informaatio %s:lle:") ; /admin line 1
   (s257 . "[info] %s") ; /admin line 2
   (s258 . "[info] %s") ; /admin line 3
   (s259 . "[info] %s") ; /admin line 4
   (s261 . "[info] %s Tiedosto -> %s %s") ; Logfile trace
   (s301 . "[info] %s on pois: %s")
   (s302 . "[info] userhost: %s") ; userhost reply
   (s303 . "[info] T�ll� hetkell� aikaa hukkaamassa: %s") ; ison reply
   (s305 . "[info] Et ole en�� poissa")
   (s306 . "[info] Olet poissa")
   (s311 . "[info] %s (%s@%s) on %s") ; user part of /whois list
   (s312 . "[info] %s k�ytt�� serveri� %s (%s)")
   (s313 . "[info] %s on Iso Molo.") ; /whois operator status
   (s314 . "[info] %s (%s@%s) oli %s") ; user part of /whowas list
   (s315 . "[info] /who:n loppu.")
   (s317 . "[info] %s on iDLaillut %s") ; /whois idle time
   (s318 . "[info] /whois:in loppu.")
   (s319 . "[info] %s on kannuilla: %s") ; channel part of whois data
   (s321 . "[info] Kannu            Populaa Otsikko") ; header for LIST cmd
   (s322 . "[info] %-15s %-5s %s")  ; each channel in LIST cmd
   (s323 . "[info] /listin loppu.")  ; trailer for LIST cmd
   (s324 . "[info] Moodi kannulle %s on %s %s") ; channel mode
   (s331 . "[info] %s:ll� ei ole otsikkoa") ; no topic message
   (s332 . "[info] %s otsikko: %s")   ; topic message
   (s333 . "[info] %s:n asetti %s kello %s") ; topic set time
   (s341 . "[info] Kutsuit %s:n kanavalle %s") ; invite reply
   (s342 . "[info] Pyyd�t %s:�� hukkaamaan aikaansa") ; summon reply
   (s351 . "[info] Versio: %s %s %s") ; version reply
   (s352_header . "[info] Nickki  Stat Kanavan nimi K�ytt�j�@kone (Hopseja  Nimi)") ; header for /who list reply
   (s352 . "[info] %-9s %-3s  %-15s %s@%s (%s)") ; /who list reply
   (s353 . "[info] K�ytt�ji� kannulla %s: %s") ; displayed after channel join
   (s364 . "[info] %s %s %s")       ; /links reply
   (s365 . "[info] /linksin loppu")  ; end of /links reply
   (s367 . "[info] %s banni %s")      ; banlist reply
   (s368 . "[info] Bannilistan loppu") ; end of banlist reply
   (s371 . "[info] %s")             ; info reply
   (s372 . "[motd] %s")		; message of the day
   (s375 . "[motd] P�iv�n viesti:") ; start of motd
   (s376 . "[motd] P�iv�n viestin loppu")    ; displayed at end of motd
   (s381 . "[info] Olet nyt Iso Molo") ; irc op status
   (s382 . "[info] Poltan hashista: %s")  ; rehash server msg
   (s391 . "[info] Aika serverilt� %s: %s") ; TIME reply
   (s392 . "[info] Tili   Terminaali  Kone") ; header for users rpl
   (s393 . "[info] %s")             ; body of users rpl
   (s395 . "[info] Ei ket��n kotona") ; nobody for users rpl
   (s401 . "[info] Ei moista nickki�/kannua: %s") ; there is no such nick/chan
   (s402 . "[info] Ei moista serveri�: %s") ; there is no such server
   (s403 . "[info] Ei moista kanavaa: %s") ; there is no such channel
   (s404 . "[info] Et voi l�hett�� %s:lle.") ; you can't send to channel
   (s405 . "[info] Liian monta kanavaa: %s") ; too many channels
   (s406 . "[info] Serverill� ei ole tietoa nickist�: %s") ; no whowas data
   (s407 . "[info] Liian monta vastaanottajaa. Viesti� ei l�hetetty: %s") ; user@host
   (s409 . "[info] Alkuper�� ei m��ritelty.") ; ping error reply
   (s411 . "[info] Ei vastaanottajaa.") ; no recipient given
   (s412 . "[info] Ei mit��n l�hetett�v��.") ; you didn't send anything.
   (s413 . "[info] Ei toplevel-domainia: %s") ; no toplevel domain spec
   (s414 . "[info] Villikortti toplevel-domainissa: %s") ; wild toplevel
   (s421 . "[info] T�m� n�ytt�� minusta purkitetulta lihalta: %s") ; you sent server spam
   (s422 . "[info] Ei motdia (liekit� /adminissa mainittua Isoa Moloa)")
   (s423 . "[info] Ei admin-infoa. Vihjeet�n Iso Molo ajaa serveri�.")
   (s431 . "[info] Ei nickki� annettu") ; you didn't provide a nick
   (s432 . "[info] Invalidi nickki: %s")
   (s433 . "[info] Nickki jo k�yt�ss�: %s")
   (s436 . "[info] Nickkit�rm�ysmurhatappo: %s")
   (s441 . "[info] %s ei ole kannulla %s") ; can't do it to those not present
   (s442 . "[info] Et ole kannulla %s.") ; you can't do that dave.
   (s443 . "[info] %s on jo kannulla %s.") ; invite error
   (s444 . "[info] %s ei ole loggautunut sis��n") ; SUMMON reply
   (s445 . "[info] Iso Molo ei anna sinun summonoida")
   (s446 . "[info] Iso Molo ei anna sinun n�hd� /usereita")
   (s451 . "[info] Et ole rekister�itynyt") ; gotta do the USER NICK thing
   (s461 . "[info] Ei tarpeeksi parametrej�: %s") ; as 421
   (s462 . "[info] Et voi rekister�ity� uudelleen") ; cannot USER twice
   (s463 . "[info] Fasistisika-Iso Molo ei anna sinun k�ytt�� t�t� serveri�")
   (s464 . "[info] V��r� salasana") ; bad PASS command
   (s465 . "[info] Sinulla ei ole lupaa k�ytt�� t�t� serveri�.") ; creep
   (s467 . "[info] Avain kanavalle %s on jo asetettu") ; chan key set already
   (s471 . "[info] Et voi joinata %s:lle (luuserilimiitti saavutettu).") ; too many ppl
   (s472 . "[info] %s on tuntematon moodi.") ; duh
   (s473 . "[info] Et voi joinata %s:lle (hommaa invite).") ; fascist nerds
   (s474 . "[info] Et voi joinata %s:lle %s (bannattu).") ; you're banned
   (s475 . "[info] Et voi joinata %s:lle (v��r� avainsana).") ; bad key
   (s481 . "[info] Et ole tarpeeksi Iso Molo tehd�ksesi tuota.") ; oper only
   (s482 . "[info] Et ole voimansiirtokone %s:ll�.") ; chanop needed
   (s483 . "[info] Et voi tappaa serveri�. Doh!") ; can't kill a server
   (s491 . "[info] Isoja Moloja ei sallita sinun koneeltasi") ; no o-line
   (s501 . "[info] Tuntematon moodilippu") ; you did something silly
   (s502 . "[info] Et voi vaihtaa toisten k�ytt�jien moodia") ; as above
   (action . "(l�hetetty %s:lle)") ; ctcp action sent
   (action-echo . "(l�hetetty %s:lle)") ; ctcp action sent
   (connect-failed . "[error] En voinut connectata %s:n porttiin %d, koska: %s")
   (connect-try . "[info] Connectailen %s:n porttiin %d...")
   (connect-abort . "[info] Konnektaus serveriin abortoitu.")
   (ctcp_action . "[action->%s] %s %s") ; ctcp ACTION display
   (ctcp_clientinfo . "[query] CLIENTINFO %s:lt� %s:lle")
   (ctcp_errmsg . "[query] ERRMSG %s:lt� %s:lle")
   (ctcp_finger . "[query] FINGER %s:lt� %s:lle")
   (ctcp_ping . "[query] PING %s:lt� %s:lle")
   (ctcp_ping_reply . "[reply] PING: %s on %s sekunnin matkan p��ss�")
   (ctcp_source . "[query] SOURCE %s:lt� %s:lle")
   (ctcp_time . "[query] TIME %s:lt� %s:lle")
   (ctcp_userinfo . "[query] USERINFO %s:lt� %s:lle")
   (ctcp_version . "[query] VERSION %s:lt� %s:lle")
   (debug  . "[debug] %s")          ; displayed by debugging code
   (error . "[%s] %s")              ; server error message
   (invite . "[info] %s kutsuu sinut %s:lle.") ; invite
   (join_you . "[info] Joinaan kannulle: %s")
   (join . "[info] %s on joinannut %s:lle")
   (kick . "[info] %s on potkittu kanavalta %s %s:n toimesta") ; someone was peeved
   (kick_you . "[info] Sinut on potkittu %s:lt� %s:n toimesta") ; loser
   (kill . "[info] Sinut on raukkamaisesti tapettu: %s") ; your time is up.
   (mode . "[info] %s on vaihtanut moodia %s:lle: %s") ; MODE change
   (nick . "[info] %s on vaihtanut nickki�: %s") ; nick change
   (newcatalog . "[info] Viestikatalogi asetettu %s:ksi")
   (nocatalog . "[error] Ei viestikatalogia m��ritelty %s:lle")
   (nosend . "[info] Sinulla ei ole uhria jolle l�hett��") ; msg not sent
   (notice . "{%s%s} %s")           ; NOTICE
   (notice_you . "{%s} %s")         ; NOTICE sent to your nick
   (notify_list . "[info] T�m�nhetkinen ajanhukkaajalistasi: %s")
   (notify_on . "[info] tunnistettu %s aikaa hukkaamassa.")
   (notify_off . "[info] tunnen, ett� %s on lopettanut ajanhukkaamisen.")
   (now_querying . "[info] T�m�nhetkinen uhrisi on %s.") ; /query foo
   (part_you . "[info] H�ivyn: %s (%s)") ; your part from channel message
   (part . "[info] %s on h�ipynyt %s:lt� (%s)") ; part from channel message
   (pong . "[info] %s sanoo ojnk.")  ; pong message from server
   (privmsg . "<%s%s> %s")          ; PRIVMSG
   (privmsg_you . "*%s* %s")        ; PRIVMSG sent to your nick
   (protocol_violation . "[error] T�m� rivi ei ole IRC-protokollan mukainen.\n[error] Valita serverin administraattorille:\n%s: %s")
   (query . "[query] %s:lt� %s:lle sis�lt� %s") ; ctcp query
   (query_unknown . "on tuntematon CTCP-viesti")
   (query_unbalanced . "[ep�tasapainoinen ctcp] %s:lt� %s:lle sis�lt� %s")
   (query_unbalanced_reply . "on ep�tasapainoinen CTCP-viesti")
   (quit . "[info] %s lopetti ajanhukkaamisen: %s") ; user signoff
   (reply . "[reply] %s:lt� %s:lle sis�lt� %s") ; ctcp reply
   (reply_unbalanced . "[ep�tasapainoinen vastaus] %s:lt� %s:lle sis�lt� %s")
   (send . "(l�hetetty %s:lle)") ; you sent a message/notice
   (send-echo . "(l�hetetty %s:lle)") ; you sent a message/notice
   (sentinel . "\nZenIRC lopetettu aikaan %s") ; process sentinel message
   (server . "[server] %s")         ; unknown server message
   (signal . "[signaali %s]")        ; signal in echo area
   (topic . "[info] %s vaihtoi otsikon kanavalla %s: %s") ; topic message
   (wallops . "-%s- %s")            ; WALLOPS notice
   ))

(provide 'zenirc-finnish)

;;; zenirc-finnish.el ends here