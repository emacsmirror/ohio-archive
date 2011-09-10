;;; zenirc-fortran.el --- emulate F-BOT FORTRAN bot program with ZenIRC

;; Copyright (C) 1993, 1994 Ben A. Mesander
;; Copyright (C) 1997 Noah Friedman

;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;; Maintainer: ben@gnu.ai.mit.edu
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

;; Example hook code to override something internal to zenirc.
;; Also a good example of a server message hook.

;; From ben@gnu.ai.mit.edu Tue May 11 08:12:27 1993
;; Newsgroups: alt.irc
;; From: ben@gnu.ai.mit.edu (Ben A. Mesander)
;; Subject: Re: introducing ZENBOT 1.0!!!!  (Bot-lovers)
;; Summary: FORTRAN BOTZ RULE!!!!!!111
;; In-Reply-To: christian@hopper.Virginia.EDU's message of Mon, 10 May 1993 23:43:42 GMT
;; Date: Tue, 11 May 1993 04:44:42 GMT
;; Nntp-Posting-Host: gnu.ai.mit.edu
;; Organization: The phedz.
;;
;; Hah. Leper.
;;
;; ircII bots are for weenies. Even C bots are slow on supercomputer
;; architectures. On a Cray, FORTRAN is much faster than C, due to the
;; architecture of the machine. Therefore I - piglet3 - the GENIUS
;; MASTERMIND of IRC have written a FORTRAN bot. This bot is guaranteed
;; to be faster than any C bot if run on a Cray II or above. I am sure
;; the irCOPS want to keep this technology secret, but I - piglet3 - am
;; going to reveal the SECRET OF FAST BOTS ON IRC RIGHT IN THIS POSTING!
;;
;; Unlike Christian's RESTRICTIVE licensing agreement, my bot is in the
;; PUBLIC DOMAIN! If you want to use it and claim you wrote it, PLEASE DO!
;;
;; The source comes in three files, a Makefile (yes, Christian's bot had
;; NO MAKEFILE! IT WAS HARD TO INSTALL!), a FORTRAN source code file, and
;; a small C code interface to the UNIX operating system. Those of you
;; running operating systems besides UNIX should have no trouble writing
;; the network interface routines in FORTRAN instead of C.
;;
;; Future releases of this program will be installable via GNU Configure,
;; thus making installation even easier. The FORTRAN-C calling interface
;; is assumed to be BSD-style, for those of you who really want to compile
;; it.
;;
;; ---Makefile---cut here---
;; CC=gcc
;; F77=f77
;;
;; bot: bot.o sock.o
;; 	$(F77) bot.o sock.o -o bot
;;
;; bot.o: bot.f
;; 	$(F77) -c bot.f
;;
;; sock.o: sock.c
;; 	$(CC) -c sock.c
;; ---bot.f---cut here---
;; CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
;; C
;; C FORTRAN BOT PROGRAM FOR IRC II
;; C
;; C BY BEN MESANDER
;; C BEN@GNU.AI.MIT.EDU
;; C
;; C THIS PROGRAM IS IN THE PUBLIC DOMAIN
;; CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
;;
;;       PROGRAM FBOT
;;       CHARACTER*80 SERVER, CHANEL, NICK, NAME, USER, HOST
;;
;; C YOU MUST CUSTOMIZE THESE VARIABLES
;;       NICK='F-BOT'
;;       NAME='EXPERIMENTAL FORTRAN IRC-II BOT PROGRAM'
;;       USER='BEN'
;;       HOST='GNU.AI.MIT.EDU'
;;
;;       PRINT *,'ENTER NAME OF IRC-SERVER TO USE:'
;;       READ(*,'(A)') SERVER
;;       PRINT *,'ENTER NAME OF CHANNEL TO JOIN:'
;;       READ(*,'(A)') CHANEL
;;
;;       I=OPSOCK(SERVER)
;;       IF (I.EQ.1) THEN
;;          PRINT *,'UNABLE TO CONNECT TO IRC-SERVER'
;;          STOP
;;       ENDIF
;;
;;       I=WRSOCK('NICK '//NICK)
;;       IF (I.EQ.1) THEN
;;          PRINT *,'UNABLE TO WRITE IRC-NICK TO SERVER'
;;          STOP
;;       ENDIF
;;
;;       I=WRSOCK('USER '//USER(1:INDEX(USER,' ')-1)//'@'//HOST(1:
;;      +     INDEX(HOST,' ')-1)//' 1 1 :'//NAME)
;;       IF (I.EQ.1) THEN
;;          PRINT *,'UNABLE TO WRITE IRC-NAME TO SERVER'
;;          STOP
;;       ENDIF
;;
;;       CALL EATMTD()
;;       CALL BOTTY(CHANEL,NICK,USER,HOST)
;;       CALL CLSOCK()
;;       STOP
;;       END
;;
;;       SUBROUTINE EATMTD()
;; C PURPOSE - TO EAT THE MOTD
;;       CHARACTER*513 INLINE
;;
;;  10   CONTINUE
;;       I=RDSOCK(INLINE)
;;       IF (I.EQ.1) THEN
;;          PRINT *,'ERROR WHILE READING MOTD'
;;          STOP
;;       ENDIF
;;       IF ((INDEX(INLINE,'End of /MOTD').EQ.0).OR.
;;      +     (INDEX(INLINE,'376').NE.0)) GOTO 10
;;       RETURN
;;       END
;;
;;       SUBROUTINE BOTTY(CHANEL, NICK, USER, HOST)
;;       CHARACTER*80 CHANEL, NICK, USER, HOST
;; C PURPOSE - TO BE BOTTY
;;       CHARACTER*513 INLINE
;;
;;       I=WRSOCK('JOIN '//CHANEL)
;;       IF (I.EQ.1) THEN
;;          PRINT *,'ERROR WHILE JOINING CHANNEL'
;;          STOP
;;       ENDIF
;;
;;  20   CONTINUE
;;       INLINE=''
;;       I=RDSOCK(INLINE)
;;          IF (I.EQ.1) THEN
;;          PRINT *,'ERROR READING FROM IRC-SERVER'
;;          STOP
;;       ENDIF
;;
;;       IF (INDEX(INLINE,'PING').EQ.1) THEN
;;          I=WRSOCK('PONG '//HOST)
;;          IF (I.EQ.1) THEN
;;             PRINT *,'ERROR WHILE PONGING'
;;             STOP
;;          ENDIF
;;          GOTO 20
;;       ENDIF
;;
;;       IF (INDEX(INLINE,'FORTRAN').NE.0) THEN
;;          I=WRSOCK('PRIVMSG '//CHANEL(:INDEX(CHANEL,' ')-1)//
;;      +        ' :YES I AM REALLY WRITTEN IN 3L33T FORTRAN-77!!!!!!!')
;;          IF (I.EQ.1) THEN
;;             PRINT *,'ERROR WRITING 3L33T MESSAGE TO IRC-SERVER'
;;             STOP
;;          ENDIF
;;          GOTO 20
;;       ENDIF
;;
;;       IF (INDEX(INLINE,'PHONE IS A SLUT').NE.0) RETURN
;;
;;       GOTO 20
;;       RETURN
;;       END
;;
;; ---sock.c---cut here---
;; /*
;;  * rudimentary C socket I/O routines for FORTRAN bot.
;;  *
;;  * Ben Mesander
;;  * ben@gnu.ai.mit.edu
;;  *
;;  * this code is in the public domain
;;  */
;;
;; #include <stdio.h>
;; #include <sys/types.h>
;; #include <sys/socket.h>
;; #include <netinet/in.h>
;; #include <netdb.h>
;; #define IRCPORT 6667
;;
;; int sock;		/* socket to connect to server */
;;
;; /*
;;  * open a socket to the irc server on "hostname"
;;  */
;;
;; int opsock_(char *fhostname, int fhostnamelen)
;; {
;; 	char *hostname;
;; 	int i;
;; 	struct sockaddr_in addr;
;; 	struct hostent *h;
;;
;; 	if (NULL == (hostname = (char *)malloc(fhostnamelen+1))) {
;; 		return 1;
;; 	}
;; 	strncpy(hostname, fhostname, fhostnamelen+1);
;; 	for(i=0;i<strlen(hostname);i++) {
;; 		if (' ' == *(hostname+i)) {
;; 			*(hostname+i)='\0';
;; 			break;
;; 		}
;; 	}
;;
;; 	if (NULL == (h = gethostbyname(hostname))) {
;; 		return 1;
;; 	}
;; 	addr.sin_family = AF_INET;
;; 	addr.sin_port = IRCPORT;
;; 	bcopy(h->h_addr, (char *)&addr.sin_addr, h->h_length);
;; 	bzero((char *)addr.sin_zero, sizeof(addr.sin_zero));
;;
;; 	if ( 0 > (sock = socket(AF_INET, SOCK_STREAM, 0))) {
;; 		return 1;
;; 	}
;;
;; 	if ( 0 > connect(sock, &addr, sizeof(addr))) {
;; 		return 1;
;; 	}
;;
;; 	return(0);
;; }
;;
;; int clsock_(void)
;; {
;; 	close(sock);
;; }
;;
;; int rdsock_(char *buf, int buflen)
;; {
;; 	int i;
;;
;; 	for (i=0; i<513; i++) {
;; 		if (1 != read(sock,buf+i,1)) {
;; 			return 1;
;; 		}
;; 		if ('\n' == *(buf+i)) {
;; 			return 0;
;; 		}
;; 	}
;; 	return 0;
;; }
;;
;; int wrsock_(char *buf, int buflen)
;; {
;; 	int i;
;;
;; 	for (i=0; i<buflen; i++) {
;; 		if (1 != write(sock, buf+i, 1)) {
;; 			return 1;
;; 		}
;; 	}
;; 	if (1 != write(sock, "\n", 1)) {
;; 		return 1;
;; 	}
;; 	return 0;
;; }
;;
;; ---end of file---
;;
;; --Ben
;; ben@gnu.ai.mit.edu
;; oink.

;; Thanks to zenirc-trigger.el, this elisp module has been reduced to a
;; one-liner, but the real live fortran above is too eleet to stop
;; distributing.

;;; Code:

(require 'zenirc-trigger)

(zenirc-trigger-register "fortran"
    "YES I AM REALLY WRITTEN IN 3L33T FORTRAN 77!!!!!!!!!111"
    "\\bfortran\\b")

(provide 'zenirc-fortran)

;; zenirc-fortran.el ends here
