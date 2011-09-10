/* -*-C-*-
 Server code for handling requests from clients and forwarding them
 on to the GNU Emacs process.

 This file is part of GNU Emacs.

 Copying is permitted under those conditions described by the GNU
 General Public License.

 Copyright (C) 1989 Free Software Foundation, Inc.

 Author: Andy Norman (ange@hplb.hpl.hp.com), based on 'etc/server.c'
         from the 18.52 GNU Emacs distribution.

 Please mail bugs and suggestions to the author at the above address.
*/

static char rcsid [] = "$Header: gnuserv.c,v 1.8 89/09/28 12:47:01 ange Exp $";

#include "gnuserv.h"

#if !defined(SYSV_IPC) && !defined(UNIX_DOMAIN_SOCKETS) && !defined(INTERNET_DOMAIN_SOCKETS)
main ()
{
  fprintf (stderr,"Sorry, the Emacs server is only supported on systems that have\n");
  fprintf (stderr,"Unix Domain sockets, Internet Domain sockets or System V IPC\n");
  exit (1);
} /* main */
#else /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */

#ifdef SYSV_IPC

int ipc_qid = 0;		/* ipc message queue id */
int ipc_wpid = 0;		/* watchdog task pid */


/*
  ipc_exit -- clean up the queue id and queue, then kill the watchdog task
              if it exists. exit with the given status.
*/
void ipc_exit(stat)
     int stat;
{
  msgctl(ipc_qid,IPC_RMID,0);
  
  if (ipc_wpid != 0)
    kill(ipc_wpid,SIGKILL);

  exit(stat);
} /* ipc_exit */


/*
  ipc_handle_signal -- catch the signal given and clean up.
*/
void ipc_handle_signal(sig)
     int sig;
{
  ipc_exit(0);
} /* ipc_handle_signal */


/* 
  ipc_spawn_watchdog -- spawn a watchdog task to clean up the message queue should the
			server process die.
*/
int ipc_spawn_watchdog()
{
  if ((ipc_wpid = fork()) == 0) { /* child process */
    int ppid = getppid();	/* parent's process id */

    setpgrp();			/* gnu kills process group on exit */
    
    while (1) {
      if (kill(ppid,0) < 0) {	/* ppid is no longer valid, parent may have died */
	ipc_exit(0);
      }; /* if */

      sleep(10);		/* have another go later */
    }; /* while */
  }; /* if */

} /* ipc_spawn_watchdog */


/*
  ipc_init -- initialize server, setting the global msqid that can be listened on.
*/
void ipc_init(msgpp)
     struct msgbuf **msgpp;
{
  key_t key;			/* messge key */
  char buf[BUFSIZ];		/* pathname for key */
  int p;			/* child process id */

  sprintf(buf,"/tmp/gsrv%d",geteuid());
  creat(buf,0600);
  key = ftok(buf,1);

  if ((ipc_qid = msgget(key,0600|IPC_CREAT)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to create msg queue\n",progname);
    ipc_exit(1);
  }; /* if */

  ipc_spawn_watchdog();

  signal(SIGTERM,ipc_handle_signal);
  signal(SIGINT,ipc_handle_signal);

  if ((*msgpp = (struct msgbuf *) malloc(sizeof **msgpp + BUFSIZ)) == NULL) {
    fprintf(stderr,"%s: unable to allocate space for message buffer\n",progname);
    ipc_exit(1);
  }; /* if */

} /* ipc_init */


/*
  handle_ipc_request -- accept a request from a client, pass the request on
  			to the GNU Emacs process, then wait for its reply and
			pass that on to the client.
*/
void handle_ipc_request(msgp)
     struct msgbuf *msgp;	/* message buffer */
{
  struct msqid_ds msg_st;	/* message status */
  char buf[BUFSIZ];
  int len;			/* length of message / read */
  int junk;			/* junk value */

  if ((len = msgrcv(ipc_qid,msgp,BUFSIZ-1,1,0)) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to receive\n",progname);
    ipc_exit(1);
  }; /* if */

  msgctl(ipc_qid,IPC_STAT,&msg_st);
  strncpy(buf,msgp->mtext,len);
  buf[len] = '\0';		/* terminate */
  
  printf("%d %s",ipc_qid,buf);
  fflush(stdout);

  if ((len = read(0,buf,BUFSIZ)) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to read\n",progname);
    ipc_exit(1);
  }; /* if */
      
  /* parse the response from gnu */
  msgp->mtext[0] = '\0';
  sscanf(buf,"%d:%[^\n]\n",&junk,msgp->mtext);

  /* Send a response back to the client. */
  msgp->mtype = msg_st.msg_lspid;
  msgsnd(ipc_qid,msgp,strlen(msgp->mtext)+1,0);

} /* handle_ipc_request */
#endif /* SYSV_IPC */


#if defined(INTERNET_DOMAIN_SOCKETS) || defined(UNIX_DOMAIN_SOCKETS)
/*
  echo_request -- read request from a given socket descriptor, and send the information
                  to stdout (the gnu process).
*/
void echo_request(s)
int s;				/* socket */
{
  char buf[BUFSIZ];
  int len;

  printf("%d ",s);
  
  /* read until we get a newline or no characters */
  while ((len = recv(s,buf,BUFSIZ,0)) > 0) {
    buf[len] = '\0';
    printf("%s",buf);

    if (buf[len-1] == '\n')
      break;			/* end of message */

  }; /* while */

  if (len < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to recv\n",progname);
    exit(1);
  }; /* if */
  
} /* echo_request */


/*
  handle_response -- accept a response from stdin (the gnu process) and pass the
                     information on to the relevant client.
*/
void handle_response()
{
  char buf[BUFSIZ];
  char response[BUFSIZ];
  int s;
  int len;

  if ((len = read(0,buf,BUFSIZ)) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to read\n",progname);
    exit(1);
  }; /* if */
      
  /* parse the response from gnu */
  response[0] = '\0';
  sscanf(buf,"%d:%[^\n]\n", &s, response);

  /* Send a response back to the client. */
  send_string(s,response);
  close(s);

} /* handle_response */
#endif /* INTERNET_DOMAIN_SOCKETS || UNIX_DOMAIN_SOCKETS */


#ifdef INTERNET_DOMAIN_SOCKETS
struct entry {
  u_long host_addr;
  struct entry *next;
};

struct entry *permitted_hosts[TABLE_SIZE];


/*
  permitted -- return whether a given host is allowed to connect to the server.
*/
int permitted(host_addr)
     u_long host_addr;
{
  int key;
  struct entry *entry;
  
  /* First find the hash key */
  key = HASH(host_addr) % TABLE_SIZE;
  
  /* Now check the chain for that hash key */
  for(entry=permitted_hosts[key]; entry != NULL; entry=entry->next)
    if (host_addr == entry->host_addr) 
      return(TRUE);

  return(FALSE);

} /* permitted */


/* 
  add_host -- add the given host to the list of permitted hosts, provided it isn't
              already there.
*/	
void add_host(host_addr)
     u_long host_addr;
{
  int key;
  struct entry *new_entry;
  
  if (!permitted(host_addr)) {
    if ((new_entry = (struct entry *) malloc(sizeof(struct entry))) == NULL) {
      fprintf(stderr,"%s: unable to malloc space for permitted host entry\n",
	      progname);
      exit(1);
    }; /* if */

    new_entry->host_addr = host_addr;
    key = HASH(host_addr) % TABLE_SIZE;
    new_entry->next = permitted_hosts[key];
    permitted_hosts[key] = new_entry;
  }; /* if */

} /* add_host */


/*
  setup_table -- initialise the table of hosts allowed to contact the server. 
                 Put in the local machine, and, if a security file is specifed,
                 add each host that is named in the file.
*/
void setup_table()
{
  FILE *host_file;
  char *file_name;
  char hostname[HOSTNAMSZ];
  u_long host_addr;
  int i;
  
  /* Make sure every entry is null */
  for (i=0; i<TABLE_SIZE; i++)
    permitted_hosts[i] = NULL;

  gethostname(hostname,HOSTNAMSZ);

  if ((host_addr = internet_addr(hostname)) == -1) {
    fprintf(stderr,"%s: unable to find %s in /etc/hosts or from YP", 
	    progname,hostname);
    exit(1);
  }; /* if */

  add_host(host_addr);					/* add local host */

  if (((file_name = getenv("GNU_SECURE")) != NULL &&    /* security file  */
       (host_file = fopen(file_name,"r")) != NULL)) {	/* opened ok */
    while ((fscanf(host_file,"%s",hostname) != EOF))	/* find a host */
      if ((host_addr = internet_addr(hostname)) != -1)	/* get its internet addr */
	add_host(host_addr);				/* add the addr */

    fclose(host_file);
  }; /* if */

} /* setup_table */


/*
  internet_init -- initialize server, returning an internet socket that can
                    be listened on.
*/
int internet_init()
{
  int ls;			/* socket descriptor */
  struct servent *sp;		/* pointer to service information */
  struct sockaddr_in server;	/* for local socket address */
  char *ptr;			/* ptr to return from getenv */

  setup_table();

  /* clear out address structure */
  bzero((char *)&server,sizeof(struct sockaddr_in));
  
  /* Set up address structure for the listen socket. */
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = INADDR_ANY;

  /* Find the information for the gnu server
   * in order to get the needed port number.
   */
  if ((ptr=getenv("GNU_PORT")) != NULL)
    server.sin_port = htons(atoi(ptr));
  else if ((sp = getservbyname ("gnuserv", "tcp")) == NULL)
    server.sin_port = htons(DEFAULT_PORT+getuid());
  else
    server.sin_port = sp->s_port;
  
  /* Create the listen socket. */
  if ((ls = socket (AF_INET,SOCK_STREAM, 0)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to create socket\n",progname);
    exit(1);
  }; /* if */
  
  /* Bind the listen address to the socket. */
  if (bind(ls,&server,sizeof(struct sockaddr_in)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to bind socket\n",progname);
    exit(1);
  }; /* if */

  /* Initiate the listen on the socket so remote users
   * can connect. 
   */
  if (listen(ls,20) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to listen\n",progname);
    exit(1);
  }; /* if */

  return(ls);

} /* internet_init */


/*
  handle_internet_request -- accept a request from a client and send the information
                             to stdout (the gnu process).
*/
void handle_internet_request(ls)
int ls;				/* listen socket */
{
  int s;
  int addrlen = sizeof(struct sockaddr_in);
  struct sockaddr_in peer;	/* for peer socket address */

  bzero((char *)&peer,sizeof(struct sockaddr_in));

  if ((s = accept(ls,&peer,&addrlen)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to accept\n",progname);
    exit(1);
  }; /* if */
    
  /* Check that access is allowed - if not return crud to the client */
  if (!permitted(peer.sin_addr.s_addr)) {
    send_string(s,"gnudoit: Connection refused\ngnudoit: unable to connect to remote");
    close(s);
    return;
  }; /* if */

  echo_request(s);
  
} /* handle_internet_request */
#endif /* INTERNET_DOMAIN_SOCKETS */


#ifdef UNIX_DOMAIN_SOCKETS
/*
  unix_init -- initialize server, returning an unix-domain socket that can
               be listened on.
*/
int unix_init()
{
  int ls;			/* socket descriptor */
  struct sockaddr_un server; 	/* unix socket address */

  if ((ls = socket(AF_UNIX,SOCK_STREAM, 0)) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to create socket\n",progname);
    exit(1);
  }; /* if */

  /* Set up address structure for the listen socket. */
  sprintf(server.sun_path,"/tmp/gsrv%d",geteuid());
  unlink(server.sun_path);	/* remove old file if it exists */

  server.sun_family = AF_UNIX;
 
  if (bind(ls,&server,strlen(server.sun_path)+2) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to bind socket\n",progname);
    exit(1);
  }; /* if */

  chmod(server.sun_path,0700);	/* only this user can send commands */

  if (listen(ls,20) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to listen\n",progname);
    exit(1);
  }; /* if */

  signal(SIGPIPE,SIG_IGN);	/* in case user kills client */

  return(ls);

} /* unix_init */


/*
  handle_unix_request -- accept a request from a client and send the information
                         to stdout (the gnu process).
*/
void handle_unix_request(ls)
int ls;				/* listen socket */
{
  int s;
  int len = sizeof(struct sockaddr_un);
  struct sockaddr_un server; 	/* for unix socket address */

  server.sun_family = AF_UNIX;

  if ((s = accept(ls,&server,&len)) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to accept\n",progname);
  }; /* if */

  echo_request(s);
  
} /* handle_unix_request */
#endif /* UNIX_DOMAIN_SOCKETS */


main(argc,argv)
     int argc;
     char *argv[];
{
  int ils = -1;			/* internet domain listen socket */
  int uls = -1;			/* unix domain listen socket */
  int chan;			/* temporary channel number */
#ifdef SYSV_IPC
  struct msgbuf *msgp;		/* message buffer */
#endif /* SYSV_IPC */

  progname = argv[0];

  for(chan=3; chan < _NFILE; close(chan++)) /* close unwanted channels */
    ;

#ifdef SYSV_IPC
  ipc_init(&msgp);		/* get a msqid to listen on, and a message buffer */
#endif /* SYSV_IPC */

#ifdef INTERNET_DOMAIN_SOCKETS
  ils = internet_init();	/* get a internet domain socket to listen on */
#endif /* INTERNET_DOMAIN_SOCKETS */

#ifdef UNIX_DOMAIN_SOCKETS
  uls = unix_init();		/* get a unix domain socket to listen on */
#endif /* UNIX_DOMAIN_SOCKETS */

  while (1) {
#ifdef SYSV_IPC
    handle_ipc_request(msgp);
#else /* NOT SYSV_IPC */
    int rmask = 1 
#ifdef UNIX_DOMAIN_SOCKETS
      + (1 << uls) 
#endif /* UNIX_DOMAIN_SOCKETS */

#ifdef INTERNET_DOMAIN_SOCKETS
      + (1 << ils)
#endif /* INTERNET_DOMAIN_SOCKETS */
      ;
    
    if (select(max2(uls,ils) + 1,&rmask,0,0,0) < 0) {
      perror(progname);
      fprintf(stderr,"%s: unable to select\n",progname);
      exit(1);
    }; /* if */

#ifdef UNIX_DOMAIN_SOCKETS    
    if (rmask & (1 << uls))	/* from unix domain socket (client process) */
      handle_unix_request(uls);
#endif /* UNIX_DOMAIN_SOCKETS */

#ifdef INTERNET_DOMAIN_SOCKETS
    if (rmask & (1 << ils))	/* from internet domain socket (client process) */
      handle_internet_request(ils);
#endif /* INTERNET_DOMAIN_SOCKETS */

    if (rmask & 1) 		/* from stdin (gnu process) */
      handle_response();
#endif /* NOT SYSV_IPC */
  }; /* while */

} /* main */

#endif /* SYSV_IPC || UNIX_DOMAIN_SOCKETS || INTERNET_DOMAIN_SOCKETS */
