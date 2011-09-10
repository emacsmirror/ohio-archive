;Return-Path: <@relay.cs.net:davis@scr.slb.com>
;Date: Tue, 25 Apr 89 04:16 EST
;From: Paul Davis <davis@scr.slb.com>
;Subject: mail logging with Emacs
;To: INFO-GNU-EMACS@prep.ai.mit.edu
;X-Vms-To: prep.ai.mit.edu::info-gnu-emacs
;
;
;
;Another piece of code I find useful: I really do not have a need for
;an archive of complete mail messages I send to others, but I do find
;it a help to have a log of when I send mail and to whom. The following
;function performs this, writing the date, and the visible mail header
;to a file. Its a bit naughty in that it inserts and then deletes the
;date into your *mail* buffer without telling you, but then I didn't
;want to have to set up a new buffer, farm off the header, add the
;date, write the buffer to a file and then kill the new buffer.
;
;I load this as part of my mail-mode-hook:
;
;(setq mail-mode-hook '(lambda ()
;			(require 'mail-log)
;			(setq mail-log-outgoing t))
;
;
;Here's a sample of my mail log file:
;
;----------------------
;Date: Mon Apr 24 08:32:01 1989
;To: info-gnu-emacs@prep.ai.mit.edu
;Subject: generic completion functions/function expansion
;----------------------
;Date: Mon Apr 24 08:35:59 1989
;To: mansfiel
;Subject: see Blair in FIN re: travellers cheques
;----------------------
;Date: Mon Apr 24 08:42:23 1989
;To: dingwall%sifvx3%sifvx7.psi@prsrtr.psi
;In-reply-to: Schlumberger Technologies Instruments, Farnborough, 
;	Computer Systems Department.'s message of 
;	Mon, 24 Apr 89 16:35:58 PDT <8904242335.AA07489@indigo.scr.slb.com>
;Subject: RE: "UKnet"
;----------------------
;
;enjoy
;
;Paul
;                             Paul Davis at Schlumberger Cambridge Research
;                                <davis%scrsu1%sdr.slb.com@relay.cs.net>
;
