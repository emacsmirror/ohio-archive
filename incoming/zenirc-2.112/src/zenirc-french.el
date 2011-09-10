;;; zenirc-french.el --- French message catalog for ZenIRC

;; Copyright (C) 1993, 1994 Ben A. Mesander
;; Copyright (C) 1998 Per Persson

;; Author: Nicolas Pioch <Nicolas.Pioch@enst.fr> (Nap)
;; Maintainer: pp@sno.pp.se
;; Keywords: extensions
;; Created: 1994/02/28

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

(zenirc-lang-define-catalog 'french
 '((join_you . "[info] Entr�e dans %s.")
   (join . "[info] %s entre dans %s.")
   (s001 . "[info] Vous perdez votre temps.") ; welcome to irc message
   ;; server name & version # msg
   (s002 . "[info] Votre serveur IRC est %s, version %s.")
   (s003 . "[info] Serveur cr�e %s.") ; when the server was built
   ;; # users on connect message
   (s251 . "[info] %s louzeurs visibles et %s invisibles sur %s serveurs.")
   (s252 . "[info] %s cr�tins sont en ligne.") ; irc operators msg
   (s253 . "[info] %s connexions non identifi�es.") ; unk connects msg
   (s254 . "[info] %s groupes.")      ; number of channels
   ;; # of clients and servers
   (s255 . "[info] %s clients et %s serveurs connect�s sur ce serveur.")
   (s301 . "[info] %s est absent: %s") ; someone is away
   (s302 . "[info] identification: %s.") ; userhost reply
   (s305 . "[info] Vous n'�tes plus marqu� absent.")
   (s306 . "[info] Vous �tes marqu� absent.")
   (s311 . "[info] %s (%s@%s) est %s") ; user part of /whois list
   (s312 . "[info] %s sur le serveur %s (%s)") ; server part of /whois list
   (s313 . "[info] %s est un cr�tin.") ; /whois operator status
   (s314 . "[info] %s (%s@%s) �tait %s.") ; user part of /whowas list
   (s315 . "[info] Fin de /who.")     ; end of /who list replies
   (s318 . "[info] Fin de /whois.")   ; end of /whois list replies
   (s317 . "[info] %s a �t� inactif %s") ; /whois idle time
   (s319 . "[info] %s participe �: %s") ; channel part of whois data
   (s321 . "[info] Groupes\tLouzeurs\tSujet") ; header for LIST cmd
   (s322 . "[info] %s\t%s\t%s")       ; each channel in LIST cmd
   (s323 . "[info] Fin de /list.")    ; trailer for LIST cmd
   (s324 . "[info] Mode de %s: %s %s.") ; channel mode
   (s331 . "[info] %s n'a pas de sujet.") ; no topic message
   (s332 . "[info] %s sujet: %s")     ; topic message
   (s341 . "[info] Vous invitez %s dans %s.") ; invite reply
   (s342 . "[info] Vous incitez %s � perdre son temps.") ; summon reply
   (s351 . "[info] Version: %s %s %s.") ; version reply
                                        ; header for /who list reply
   (s352_header . "[info] Pseudo  Etat Nom du Groupe  login et machine        Nom")
   (s352 . "[info] %-9s %-3s  %-15s %s@%s (%s)") ; /who list reply
   (s353 . "[info] Louzeurs sur %s: %s") ; displayed after channel join
   (s364 . "[info] %s %s %s")         ; /links reply
   (s365 . "[info] Fin de /links.")   ; end of /links reply
   (s367 . "[info] %s interdit � %s.") ; banlist reply
   (s368 . "[info] Fin de liste d'interdiction.") ; end of banlist reply
   (s371 . "[info] %s")               ; info reply
   (s372 . "[motd] %s")		; message of the day
   (s375 . "[motd] Message du jour:")	; start of motd
   (s376 . "[motd] Fin de message.")  ; displayed at end of motd
   (s381 . "[info] Vous etes maintenant un cr�tin total.") ; irc op status
   (s382 . "[info] Rechargement: %s") ; rehash server msg
   (s391 . "[info] Heure sur le serveur %s: %s.") ; TIME reply
   (s392 . "[info] Login Terminal  Machine") ; header for users rpl
   (s393 . "[info] %s")               ; body of users rpl
   (s395 . "[info] Personne n'est connect�.") ; nobody for users rpl
   (s401 . "[info] Louzeur ou groupe inconnu: %s.") ; no such nick/chan
   (s402 . "[info] Serveur inconnu: %s.") ; there is no such server
   (s403 . "[info] Groupe inconnu: %s.") ; there is no such channel
   (s404 . "[info] Impossible d'�crire � %s.") ; can't send to channel
   (s405 . "[info] Trop de groupes: %s.") ; too many channels
   (s406 . "[info] Le serveur ne se souvient plus de %s.")
   ;; user@host
   (s407 . "[info] Destinataires dupliqu�s, message non envoy�: %s")
   (s411 . "[info] Destinataire manquant.") ; no recipient given
   (s412 . "[info] Texte manquant.")  ; you didn't send anything.
   (s413 . "[info] Domaine manquant: %s") ; no toplevel domain spec
   (s414 . "[info] Domaine invalide: %s.") ; wild toplevel
   (s421 . "[info] Charabia: %s.")
   (s422 . "[info] Certains cr�tins sont trop ignorants pour cr�er un mot de bienvenue.")
   (s423 . "[info] Les cr�tins de %s sont trop ignorants pour fournir les informations administratives.")
   (s431 . "[info] Pseudonyme manquant.") ; you didn't provide a nick
   (s432 . "[info] Pseudonyme invalide: %s.") ; invalid nick
   (s433 . "[info] Pseudonyme %s d�ja utilis�.") ; invalid nick
   (s436 . "[info] Collision de pseudonymes: %s.") ; nickicide
   (s441 . "[info] %s n'est pas sur %s.") ; can't do it to those not present
   (s442 . "[info] Vous n'�tes pas sur %s.") ; you can't do that dave.
   (s443 . "[info] %s est d�j� sur %s.") ; invite error
   (s444 . "[info] %s n'est pas connect�.") ; SUMMON reply
   (s445 . "[info] Un quelconque cr�tin vous refuse l'usage de /summon.")
   ;; disabld users
   (s446 . "[info] Un quelconque cr�tin vous refuse l'usage de /users.")
   ;; gotta do the USER NICK thing
   (s451 . "[info] Vous n'�tes pas encore enregistr�.")
   (s461 . "[info] Param�tres insuffisants: %s") ; same as 421
   (s462 . "[info] Vous ne pouvez pas vous enregistrer � nouveau.")
   (s463 . "[info] Un quelconque cr�tin fasciste vous emp�che de vous connecter.")
   (s464 . "[info] Le mot de passe ne semble pas valable.") ; bad PASS command
   (s465 . "[info] Vous n'�tes pas autoris� � utiliser ce serveur.")
   ;; chan key set already
   (s467 . "[info] Il y a d�j� une cl� sur %s.")
   (s471 . "[info] Impossible de rentrer sur %s (limite atteinte).")
   (s472 . "[info] Mode %s inconnu.") ; duh
   (s473 . "[info] Impossible de rentrer sur %s (sur invitation).")
   (s474 . "[info] Impossible de rentrer sur %s (interdit d'acces).")
   ;; bad key
   (s475 . "[info] Impossible de rentrer sur %s (cl� invalide).")
   (s481 . "[info] Vous n'�tes pas encore assez cr�tin pour cela.")
   ;; chanop needed
   (s482 . "[info] Vous n'�tes pas assez puissant sur %s.")
   (s483 . "[info] Hep! Vous ne pouvez pas tuer un serveur.")
   (s491 . "[info] Pas de cr�tins connus sur votre site.") ; no o-line
   (s501 . "[info] Mode utilisateur inconnu.") ; you did something silly
   (s502 . "[info] Vous ne pouvez pas changer le mode de quelqu'un d'autre.")
   (ctcp_action . "[action->%s] %s %s") ; ctcp ACTION display
   (ctcp_clientinfo . "[interrogation] CLIENTINFO de %s sur %s.")
   (ctcp_errmsg . "[interrogation] ERRMSG de %s sur %s.")
   (ctcp_finger . "[interrogation] FINGER de %s sur %s.")
   (ctcp_ping . "[interrogation] PING de %s sur %s.")
   (ctcp_source . "[interrogation] SOURCE de %s sur %s.")
   (ctcp_time . "[interrogation] TIME de %s sur %s.")
   (ctcp_userinfo . "[interrogation] USERINFO de %s sur %s.")
   (ctcp_version . "[interrogation] VERSION de %s sur %s.")
   (debug . "[debug] %s")             ; displayed by debugging code
   (error . "[%s] %s")                ; server error message
   (invite . "[info] %s vous invite � perdre du temps sur %s.") ; invite
   (kick . "[info] %s a �t� �ject� de %s par %s.")
   (kick_you . "[info] Vous avez �t� �ject� de %s par %s.")
   (kill . "[info] Vous avez �t� tu�: %s.") ; your time is up.
   (mode . "[info] %s a chang� le mode sur %s: %s.") ; MODE change
   (nick . "[info] %s a chang� de pseudo pour %s.") ; nick change
   (nosend . "[info] Pas de victime pour ce message.") ; msg not sent
   (notice . "{%s%s} %s")             ; NOTICE
   (notice_you . "{%s} %s")           ; NOTICE sent to your nick
   (now-querying . "[info] Victime: %s.") ; /query foo
   (part_you . "[info] D�part de: %s (%s)") ; your part from channel message
   (part . "[info] %s quitte %s (%s)")    ;part from channel message
   (privmsg . "<%s%s> %s")            ; PRIVMSG
   (privmsg_you . "*%s* %s")          ; PRIVMSG sent to your nick
   (query . "[interrogation] de %s sur %s, contenu %s.") ; ctcp query
   (query_unknown . "commande CTCP inconnue.") ; we don't grok this
   (query_unbalanced . "[mauvaise interrogation] de %s vers %s, contenu %s.")
   (query_unbalanced_reply . "mauvaise r�ponse CTCP.") ; odd number of ^A's
   (quit . "[info] %s arr�te de perdre du temps: %s") ; user signoff
   (reply . "[reponse] de %s sur %s, contenu %s.") ; ctcp reply
   (reply_unbalanced . "[mauvaise r�ponse] de %s sur %s, contenu %s.")
   (send . "(envoy� sur %s)")      ; you sent a message/notice
   (send-action . "(envoy� sur %s)")      ; you sent a message/notice
   (server . "[serveur] %s")          ; unknown server message
   (signal . "[signal dans %s]")        ; signal in echo area
   (topic . "[info] %s a chang� le sujet sur %s pour: %s") ; topic message
   (wallops . "-%s- %s")              ; WALLOPS notice
   ))

(provide 'zenirc-french)

;;; zenirc-french.el ends here
