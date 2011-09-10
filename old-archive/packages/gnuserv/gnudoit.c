/* -*-C-*-
 Client code to locally and remotely evaluate lisp forms using GNU Emacs.

 This file is part of GNU Emacs.

 Copying is permitted under those conditions described by the GNU
 General Public License.

 Copyright (C) 1989 Free Software Foundation, Inc.

 Author: Andy Norman (ange@hplb.hpl.hp.com).

 Please mail bugs and suggestions to the author at the above address.
*/

static char rcsid [] = "$Header: gnudoit.c,v 1.5 89/07/24 12:47:53 ange Exp $";

#include "gnuserv.h"

#if !defined(SYSV_IPC) && !defined(UNIX_DOMAIN_SOCKETS) && !defined(INTERNET_DOMAIN_SOCKETS)
main ()
{
  fprintf (stderr,"Sorry, the Emacs server is only supported on systems that have\n");
  fprintf (stderr,"Unix Domain sockets, Internet Domain sockets or System V IPC.\n");
  exit (1);
} /* main */
#else /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */

main(argc,argv)
     int argc;
     char *argv[];
{
  int starting_line = 1;			/* line to start editing at */
  int qflg = 0;					/* don't wait around for gnu emacs to eval cmd */
  int errflg = 0;				/* option error */
  int c;					/* char from getopt */
  int s;					/* socket / msqid to server */
#ifdef INTERNET_DOMAIN_SOCKETS
  char remotehost[HOSTNAMSZ];			/* remote hostname */
  int hflg = 0;					/* hostname given on command line */
  u_short port = 0;				/* port number */
  char *ptr;					/* return from getenv */
#endif /* INTERNET_DOMAIN_SOCKETS */
#ifdef SYSV_IPC
  struct msgbuf *msgp;				/* message */
#endif /* SYSV_IPC */

  progname = argv[0];

  while ((c = getopt(argc, argv,
#ifdef INTERNET_DOMAIN_SOCKETS
		     "qh:p:"
#else /* !INTERNET_DOMAIN_SOCKETS */
		     "q"
#endif /* !INTERNET_DOMAIN_SOCKETS */
		     )) != EOF)
    switch (c) {
#ifdef INTERNET_DOMAIN_SOCKETS
    case 'h':					/* host name specified */
      strcpy(remotehost,optarg);
      hflg++;
      break;
    case 'p':					/* port number specified */
      port = atoi(optarg);
      break;
#endif /* INTERNET_DOMAIN_SOCKETS */
    case 'q':					/* quick mode specified */
      qflg++;
      break;
    case '?':
      errflg++;
    }; /* switch */

  if (errflg) {
    fprintf(stderr,
#ifdef INTERNET_DOMAIN_SOCKETS
	    "usage: %s [-q] [-h hostname] [-p port] [sexpr]...\n",
#else /* !INTERNET_DOMAIN_SOCKETS */
	    "usage: %s [-q] [sexpr]...\n",
#endif /* !INTERNET_DOMAIN_SOCKETS */
	    progname);
    exit (1);
  }; /* if */

#ifdef INTERNET_DOMAIN_SOCKETS
  if (!hflg) {
    if((ptr=getenv("GNU_HOST")) != NULL)
      strcpy(remotehost,ptr);
    else
      gethostname(remotehost,HOSTNAMSZ);	/* use this host by default */
  }; /* if */

  if (port == 0 && (ptr=getenv("GNU_PORT")) != NULL)
      port = atoi(ptr);

#ifdef UNIX_DOMAIN_SOCKETS
  if (!strcmp(remotehost,"unix"))
    s = connect_to_unix_server();
  else
#endif /* UNIX_DOMAIN_SOCKETS */
    s = connect_to_internet_server(remotehost,port);
#else /* !INTERNET_DOMAIN_SOCKETS */
#ifdef UNIX_DOMAIN_SOCKETS
  s = connect_to_unix_server();
#endif /* UNIX_DOMAIN_SOCKETS */
#ifdef SYSV_IPC
  if ((msgp = (struct msgbuf *) malloc(sizeof *msgp + BUFSIZ)) == NULL) {
    fprintf(stderr,"%s: not enough memory for message buffer\n",progname);
    exit(1);
  }; /* if */

  msgp->mtext[0] = '\0';			/* ready for later strcats */
  s = connect_to_ipc_server();
#endif /* SYSV_IPC */
#endif /* !INTERNET_DOMAIN_SOCKETS */

  if (qflg) {
    send_string(s,"(server-eval-quickly '(progn ");
  }
  else {
    send_string(s,"(server-eval '(progn ");
  };

  for (; optind < argc; optind++)
    send_string(s,argv[optind]);

  send_string(s,"))");

#ifdef SYSV_IPC
  disconnect_from_ipc_server(s,msgp,!qflg);
#else /* !SYSV_IPC */
  disconnect_from_server(s,!qflg);
#endif /* !SYSV_IPC */

  exit(0);

} /* main */

#endif /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */
