/* -*-C-*-
 Common library code for the GNU Emacs server and client.

 This file is part of GNU Emacs.

 Copying is permitted under those conditions described by the GNU
 General Public License.

 Copyright (C) 1989 Free Software Foundation, Inc.

 Author: Andy Norman (ange@hplb.hpl.hp.com), based on 
         'etc/server.c' and 'etc/emacsclient.c' from the 18.52 GNU
         Emacs distribution.

 Please mail bugs and suggestions to the author at the above address.
*/

static char rcsid [] = "$Header: gnuslib.c,v 1.6 89/09/28 12:47:34 ange Exp $";

#include "gnuserv.h"

char *progname = NULL;

#ifdef SYSV_IPC
/*
  connect_to_ipc_server -- establish connection with server process via SYSV IPC
  			   Returns msqid for server if successful.
*/
int connect_to_ipc_server()
{
  int s;			/* connected msqid */
  key_t key;			/* message key */
  char buf[BUFSIZ];		/* buffer for filename */

  sprintf(buf,"/tmp/gsrv%d",geteuid());
  creat(buf,0600);
  key = ftok(buf,1);

  if ((s = msgget(key,0600)) == -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to access msg queue\n",progname);
    exit(1);
  }; /* if */

  return(s);

} /* connect_to_ipc_server */


/*
  disconnect_from_ipc_server -- inform the server that sending has finished,
                                and wait for its reply.
*/
void disconnect_from_ipc_server(s,msgp,echo)
     int s;
     struct msgbuf *msgp;
     int echo;
{
  int len;			/* length of received message */

  send_string(s,"\n");		/* newline terminates this message */
  msgp->mtype = 1;

  if(msgsnd(s,msgp,strlen(msgp->mtext)+1,0) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to send message to server\n",progname);
    exit(1);
  }; /* if */
  
  if((len = msgrcv(s,msgp,BUFSIZ,getpid(),0)) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to receive message from server\n",progname);
    exit(1);
  }; /* if */

  if (echo) {
    msgp->mtext[len] = '\0';	/* string terminate message */
    printf("%s\n",msgp->mtext);
  }; /* if */

} /* disconnect_from_ipc_server */  
#endif SYSV_IPC


#if defined(INTERNET_DOMAIN_SOCKETS) || defined(UNIX_DOMAIN_SOCKETS)
/*
  send_string -- send string to socket.
*/
void send_string(s,msg)
     int s;
     char *msg;
{
  if (send(s,msg,strlen(msg),0) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to send\n",progname);
    exit(1);
  }; /* if */ 
  
} /* send_string */
#endif /* INTERNET_DOMAIN_SOCKETS || UNIX_DOMAIN_SOCKETS */


#ifdef UNIX_DOMAIN_SOCKETS
/*
  connect_to_unix_server -- establish connection with server process via a unix-
  			    domain socket. Returns socket descriptor for server
			    if successful.
*/
int connect_to_unix_server()
{
  int s;			/* connected socket descriptor */
  struct sockaddr_un server; 	/* for unix connections */

  if ((s = socket(AF_UNIX,SOCK_STREAM,0)) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to create socket\n",progname);
    exit(1);
  }; /* if */
  
  server.sun_family = AF_UNIX;
  sprintf(server.sun_path,"/tmp/gsrv%d",geteuid());

  if (connect(s,&server,strlen(server.sun_path)+2) < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to connect to local\n",progname);
    exit(1);
  }; /* if */

  return(s);

} /* connect_to_unix_server */
#endif /* UNIX_DOMAIN_SOCKETS */


#ifdef INTERNET_DOMAIN_SOCKETS
/*
  internet_addr -- return the internet addr of the hostname or
                   internet address passed. Return -1 on error.
*/
u_long internet_addr(host)
     char *host;
{
  struct hostent *hp;		/* pointer to host info for remote host */
  u_long host_addr;		/* host address */

  if ((host_addr = inet_addr(host)) != -1)
    return host_addr;
  else if ((hp = gethostbyname(host)) != NULL)
    return ((struct in_addr *)(hp->h_addr))->s_addr;
  else
    return -1;

} /* internet_addr */


/*
  connect_to_internet_server -- establish connection with server process via 
  				an internet domain socket. Returns socket
				descriptor for server if successful.
*/
int connect_to_internet_server(serverhost,port)
     char *serverhost;
     u_short port;
{
  int s;				/* connected socket descriptor */
  struct servent *sp;			/* pointer to service information */
  struct sockaddr_in peeraddr_in;	/* for peer socket address */

  /* clear out address structures */
  bzero((char *)&peeraddr_in,sizeof(struct sockaddr_in));
  
  /* Set up the peer address to which we will connect. */
  peeraddr_in.sin_family = AF_INET;

  /* look up the server host's internet address */
  if ((peeraddr_in.sin_addr.s_addr = internet_addr(serverhost)) == -1) {
    fprintf(stderr,"%s: unable to find %s in /etc/hosts or from YP\n",
	    progname,serverhost);
    exit(1);
  }; /* if */
  
  if (port == 0) {
    if ((sp = getservbyname ("gnuserv","tcp")) == NULL)
      peeraddr_in.sin_port = htons(DEFAULT_PORT+getuid());
    else
      peeraddr_in.sin_port = sp->s_port;
  } /* if */
  else
    peeraddr_in.sin_port = htons(port);
  
  /* Create the socket. */
  if ((s = socket (AF_INET,SOCK_STREAM, 0))== -1) {
    perror(progname);
    fprintf(stderr,"%s: unable to create socket\n",progname);
    exit(1);
  }; /* if */
  
  /* Try to connect to the remote server at the address
   * which was just built into peeraddr.
   */
  if (connect(s, &peeraddr_in, sizeof(struct sockaddr_in)) == -1) {
    perror(progname);
    fprintf(stderr, "%s: unable to connect to remote\n",progname);
    exit(1);
  }; /* if */
  
  return(s);

} /* connect_to_internet_server */
#endif /* INTERNET_DOMAIN_SOCKETS */


#if defined(INTERNET_DOMAIN_SOCKETS) || defined(UNIX_DOMAIN_SOCKETS)
/*
  disconnect_from_server -- inform the server that sending has finished, and wait for
                            its reply.
*/
void disconnect_from_server(s,echo)
     int s;
     int echo;
{
  char buffer[REPLYSIZ];
  int length;

  send_string(s,"\n");		/* make sure server gets string */

  if (shutdown(s,1) == -1) {
    perror(progname);
    fprintf(stderr, "%s: unable to shutdown socket\n",progname);
    exit(1);
  }; /* if */

  while((length = recv(s,buffer,REPLYSIZ,0)) > 0) {
    buffer[length] = '\0';
    if (echo) printf("%s",buffer);
  }; /* while */
  
  if (echo) putchar('\n');

  if(length < 0) {
    perror(progname);
    fprintf(stderr,"%s: unable to read the reply from the server\n",progname);
    exit(1);
  }; /* if */

} /* disconnect_from_server */  
#endif /* INTERNET_DOMAIN_SOCKETS || UNIX_DOMAIN_SOCKETS */
