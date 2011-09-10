/* -*-C-*-
 Header file for the GNU Emacs server and client C code.

 This file is part of GNU Emacs.

 Copying is permitted under those conditions described by the GNU
 General Public License.

 Copyright (C) 1989 Free Software Foundation, Inc.

 Author: Andy Norman (ange@hplb.hpl.hp.com), based on 
         'etc/server.c' and 'etc/emacsclient.c' from the 18.52 GNU
         Emacs distribution.

 Please mail bugs and suggestions to the author at the above address.
*/

static char header_rcsid [] = "$Header: gnuserv.h,v 1.9 90/01/31 10:37:41 ange Exp $";

#define NO_SHORTNAMES

#define PATCHLEVEL 2

#include "../src/config.h"
#undef read
#undef write
#undef open
#undef close

/* Define the communication method between server and clients */

#define INTERNET_DOMAIN_SOCKETS
#define UNIX_DOMAIN_SOCKETS
/* #define SYSV_IPC */

#if !defined(SYSV_IPC) && !defined(UNIX_DOMAIN_SOCKETS) && !defined(INTERNET_DOMAIN_SOCKETS)

#ifdef HAVE_SYSVIPC
#define SYSV_IPC		/* SYSV systems use SYSV IPC by default */
#endif /* HAVE_SYSVIPC */

#ifdef BSD
#define UNIX_DOMAIN_SOCKETS	/* BSD systems use Unix Domain sockets by default */
#endif /* BSD */

#endif /* No communication method pre-defined */

#include <sys/types.h>
#include <sys/param.h>
#include <stdio.h>
#include <signal.h>

#ifdef SYSV_IPC
#include <sys/ipc.h>
#include <sys/msg.h>

#define send_string(s,str) \
  if (strlen(msgp->mtext) + strlen(str) < BUFSIZ) \
     strcat(msgp->mtext,str); \
  else \
  { \
    fprintf(stderr,"%s: not enough message buffer space\n",progname); \
     exit(1); \
  } \

#endif /* SYSV_IPC */

#if defined(INTERNET_DOMAIN_SOCKETS) || defined(UNIX_DOMAIN_SOCKETS)
#include <sys/socket.h>
#endif /* INTERNET_DOMAIN_SOCKETS || UNIX_DOMAIN_SOCKETS */

#ifdef INTERNET_DOMAIN_SOCKETS
#include <netinet/in.h>
#include <netdb.h>
#define TABLE_SIZE 101		/* The number of entries in the hash table */
#define HASH(host) host		/* Rather simplistic hash function */
#define DEFAULT_PORT 21490	/* default port number to use */
#endif /* INTERNET_DOMAIN_SOCKETS */

#ifdef UNIX_DOMAIN_SOCKETS
#include <sys/un.h>
#endif /* UNIX_DOMAIN_SOCKETS */

#define HOSTNAMSZ 255		/* max size of a hostname */
#define REPLYSIZ 300		/* max size of reply from server to client */
#define FALSE 0
#define TRUE 1

extern char *getenv();
extern char *optarg;
extern int optind;
extern char *progname;

#ifndef BSD
extern char *getcwd();
#endif

#define max2(x,y) (((x) > (y)) ? (x) : (y))

#ifndef _NFILE			/* rough guess at maximum number of open files */
#define _NFILE 20
#endif
