/* -*-C-*-
 Client code to allow local and remote editing of files by GNU Emacs.

 This file is part of GNU Emacs. 

 Copying is permitted under those conditions described by the GNU
 General Public License.

 Copyright (C) 1989 Free Software Foundation, Inc.

 Author: Andy Norman (ange@hplb.hpl.hp.com), based on 
         'etc/emacsclient.c' from the GNU Emacs 18.52 distribution.

 Please mail bugs and suggestions to the author at the above address.
*/

static char rcsid [] = "$Header: gnuclient.c,v 1.8 89/07/24 12:46:46 ange Exp $";

#include "gnuserv.h"

#if !defined(SYSV_IPC) && !defined(UNIX_DOMAIN_SOCKETS) && !defined(INTERNET_DOMAIN_SOCKETS)
main ()
{
  fprintf (stderr,"Sorry, the Emacs server is only supported on systems that have\n");
  fprintf (stderr,"Unix Domain sockets, Internet Domain sockets or System V IPC.\n");
  exit (1);
} /* main */
#else /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */

static char cwd[MAXPATHLEN+2];			/* current working directory when calculated */
static char *cp = NULL;				/* ptr into valid bit of cwd above */


/*
  get_current_working_directory -- return the cwd.
*/
char *get_current_working_directory()
{
  if (cp == NULL) {				/* haven't calculated it yet */
#ifdef BSD
    if (getwd(cwd) == 0) {
#else /* !BSD */
    if (getcwd(cwd,MAXPATHLEN) == NULL) {
#endif /* !BSD */
      perror(progname);
      fprintf(stderr,"%s: unable to get current working directory\n",progname);
      exit(1);
    }; /* if */
    
    /* on some systems, cwd can look like '@machine/' ... */
    /* ignore everything before the first '/' */
    for (cp = cwd; *cp && *cp != '/'; ++cp)
      ;				           

  }; /* if */

  return cp;
    
} /* get_current_working_directory */


/*
  filename_expand -- try to convert the given filename into a fully-qualified
  		     pathname.
*/
void filename_expand(fullpath,filename)
     char *fullpath;				/* returned full pathname */
     char *filename;				/* filename to expand */
{
  int len;

  fullpath[0] = '\0';
  
  if(filename[0] && filename[0] != '/') {	/* relative filename */
    
    strcat(fullpath,get_current_working_directory());
    len = strlen(fullpath);
     
    if (len > 0 && fullpath[len-1] == '/')	/* trailing slash already? */
      ;						/* yep */
    else
      strcat(fullpath,"/");			/* nope, append trailing slash */
  }; /* if */

  strcat(fullpath,filename);

} /* filename_expand */


main(argc,argv)
     int argc;
     char *argv[];
{
  int starting_line = 1;			/* line to start editing at */
  char command[MAXPATHLEN+50];			/* emacs command buffer */
  char fullpath[MAXPATHLEN+1];			/* full pathname to file */
  int qflg = 0;					/* quick edit, don't wait for user to finish */
  int errflg = 0;				/* option error */
  int c;					/* char from getopt */
  int s;					/* socket / msqid to server */
#ifdef INTERNET_DOMAIN_SOCKETS
  char thishost[HOSTNAMSZ];			/* this hostname */
  char remotehost[HOSTNAMSZ];			/* remote hostname */
  char remotepath[MAXPATHLEN+1];		/* remote pathname */
  int hflg = 0;					/* hostname given on command line */
  int rflg = 0;					/* pathname given on command line */
  u_short port = 0;				/* port to server */
  char *ptr;					/* return from getenv */
#endif /* INTERNET_DOMAIN_SOCKETS */
#ifdef SYSV_IPC
  struct msgbuf *msgp;				/* message */
#endif /* SYSV_IPC */

  progname = argv[0];

  while ((c = getopt(argc, argv,

#ifdef INTERNET_DOMAIN_SOCKETS
		     "h:p:r:q"
#else /* !INTERNET_DOMAIN_SOCKETS */
		     "q"
#endif /* !INTERNET_DOMAIN_SOCKETS */

		     )) != EOF)
    switch (c) {
    case 'q':					/* quick mode specified */
      qflg++;
      break;

#ifdef INTERNET_DOMAIN_SOCKETS
    case 'h':					/* server host name specified */
      strcpy(remotehost,optarg);
      hflg++;
      break;
    case 'r':					/* remote path from server specifed */
      strcpy(remotepath,optarg);
      rflg++;
      break;
    case 'p':					/* port number specified */
      port = atoi(optarg);
      break;
#endif /* INTERNET_DOMAIN_SOCKETS */

    case '?':
      errflg++;
    }; /* switch */

  if (errflg) {
    fprintf(stderr,
#ifdef INTERNET_DOMAIN_SOCKETS
	    "usage: %s [-q] [-h hostname] [-p port] [-r pathname] [[+line] path] ...\n",
#else /* !INTERNET_DOMAIN_SOCKETS */
	    "usage: %s [-q] [[+line] path] ...\n",
#endif /* !INTERNET_DOMAIN_SOCKETS */
	    progname);
    exit (1);
  }; /* if */

#ifdef INTERNET_DOMAIN_SOCKETS
  gethostname(thishost,HOSTNAMSZ);

  if (!hflg) {					/* attempt to find the server host */
    if((ptr=getenv("GNU_HOST")) != NULL)	/* user specified a host */
      strcpy(remotehost,ptr);
    else					/* use this host by default */
      strcpy(remotehost,thishost);
  }; /* if */

  if(!rflg) {					/* attempt to generate a path to this machine */
    if((ptr=getenv("GNU_NODE")) != NULL)	/* user specified a path */
      strcpy(remotepath,ptr);

#if defined(hp9000s300) || defined(hp9000s800)
    else if (strcmp(thishost,remotehost)) {	/* try /net/thishost */
      strcpy(remotepath,"/net/"); 		/* (this fails using internet addresses) */
      strcat(remotepath,thishost);
    }
#endif

    else					/* same machines, no need for path */
      remotepath[0] = '\0';			/* default is the empty path */
  }; /* if */

  if (port == 0 && (ptr=getenv("GNU_PORT")) != NULL)
    port = atoi(ptr);

#ifdef UNIX_DOMAIN_SOCKETS
  if (!strcmp(remotehost,"unix"))
    s = connect_to_unix_server();
  else
#endif /* UNIX_DOMAIN_SOCKETS */
    s = connect_to_internet_server(remotehost,port);
#else  /* !INTERNET_DOMAIN_SOCKETS */

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
    send_string(s,"(server-edit-files-quickly '(");
  }
  else {
    send_string(s,"(server-edit-files '(");
  };

  for (; optind < argc; optind++) {
    if (*argv[optind] == '+')
      starting_line = atoi(argv[optind]);
    else {

      filename_expand(fullpath,argv[optind]);
      sprintf(command,"(%d . \"%s%s\")",starting_line,

#ifdef INTERNET_DOMAIN_SOCKETS
	      remotepath,
#else /* !INTERNET_DOMAIN_SOCKETS */
	      "",
#endif
	      fullpath);
      send_string(s,command);
      starting_line = 1;
    }; /* else */
  }; /* for */

  send_string(s,"))");

#ifdef SYSV_IPC
  disconnect_from_ipc_server(s,msgp,FALSE);
#else /* !SYSV_IPC */
  disconnect_from_server(s,FALSE);
#endif /* !SYSV_IPC */

  exit(0);

} /* main */

#endif /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */
