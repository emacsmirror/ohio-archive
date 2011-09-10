/*
 * TCP/IP stream emulation for GNU Emacs.
 * Copyright (C) 1988, 1989 Fujitsu Laboratories LTD.
 * Copyright (C) 1988, 1989 Masanobu UMEDA
 * $Header: tcp.c,v 1.3 89/06/19 13:39:12 umerin Locked $

 * This file is part of GNU Emacs.

 * GNU Emacs is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY.  No author or distributor
 * accepts responsibility to anyone for the consequences of using it
 * or for whether it serves any particular purpose or works at all,
 * unless he says so in writing.  Refer to the GNU Emacs General Public
 * License for full details.

 * Everyone is granted permission to copy, modify and redistribute
 * GNU Emacs, but only under the conditions described in the
 * GNU Emacs General Public License.   A copy of this license is
 * supposed to have been given to you along with GNU Emacs so you
 * can know your rights and responsibilities.  It should be in a
 * file named COPYING.  Among other things, the copyright notice
 * and this notice must be preserved on all copies.
 
 * If you modify the source for your system, please send me the diffs.
 * I'll includes some of them in the future releases.
 *
 * Yasunari,Itoh at PFU limited contributed for Fujitsu UTS and SX/A.
 *
 * Thu Apr  6 13:47:37 JST 1989
 * USG fixes by Sakaeda <saka@mickey.trad.pf.fujitsu.junet>
 *
 * For Fujitsu UTS compile with:
 *	cc -O -o tcp tcp.c -DFUJITSU_UTS -lu -lsocket
 */

#ifndef lint
static char *rcsId = "$Header: tcp.c,v 1.3 89/06/19 13:39:12 umerin Locked $";
#endif

#include <stdio.h>
#include <fcntl.h>
#include <ctype.h>
#include <sys/types.h>

#ifdef FUJITSU_UTS
#define USG
#include <sys/ucbtypes.h>
#include <sys/tisp/socket.h>
#include <netdb.h>
#include <sys/tisp/in.h>
#else
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#endif

#ifdef USG
#include <sys/stat.h>
#include <signal.h>
#endif

#ifdef FUJITSU_UTS
#define bcopy(f,t,n)    memcpy(t,f,n)
#define bcmp(b1,b2,n)   (memcmp(b1,b2,n)!=0)
#define bzero(b,n)      memset(b,0,n)
#endif

#ifdef USG
int selectable = 1;

sigout()
{
  fcntl(fileno(stdin),F_SETFL,0);
  exit(-1);
}
#endif

main(argc, argv)
int	argc;
char	*argv[];
{
  struct hostent	*host;
  struct sockaddr_in	sockin, sockme;
  struct servent	*serv;
  char	*hostname;
  char	*service = "nntp";
  int	port;
  int	readfds;
  int   writefds;
  int	server;			/* NNTP Server */
  int	emacsIn = fileno(stdin); /* Emacs intput */
  int	emacsOut = fileno(stdout); /* Emacs output */
  char	buffer[1024];
  int	nbuffer;		/* Number of bytes in buffer */
  int   wret;
  char  *retry;                  /* retry bufferp */

  while(--argc > 0){
    switch(**(++argv)){
    case '-':
      {
	char	*p = &argv[0][1];
	if(strcmp(p,"s")==0){	/* Service name */
	  service = *(++argv);
	  --argc;
	} else if(strcmp(p,"h")==0){ /* Host name */
	  hostname = *(++argv);
	  --argc;
	} else {
	  fprintf(stderr, "Usage: tcp -h HOST -s SERVICE\n");
	  exit(1);
	}
      }
      break;
    default:
      fprintf(stderr, "Usage: tcp -h HOST [-s SERVICE]\n");
      exit(1);
      break;
    }
  }

  if((host = gethostbyname(hostname)) == NULL){
    perror("gethostbyname");
    exit(1);
  }
  if(isdigit(service[0]))
    port = atoi(service);
  else {
    serv = getservbyname(service, "tcp");
    if(serv == NULL){
      perror("getservbyname");
      exit(1);
    }
    port = serv->s_port;
  }

  bzero(&sockin, sizeof(sockin));
  sockin.sin_family = host->h_addrtype;
  bcopy(host->h_addr, &sockin.sin_addr, host->h_length);
  sockin.sin_port = htons(port);
  if((server = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    perror("socket");
    exit(1);
  }
  if(setsockopt(server, SOL_SOCKET, SO_REUSEADDR, 0, 0)) {
    perror("setsockopt");
    exit(1);
  }
  bzero(&sockme, sizeof(sockme));
  sockme.sin_family = sockin.sin_family;
  sockme.sin_addr.s_addr = INADDR_ANY;
  if(bind(server, &sockme, sizeof(sockme)) < 0){
    perror("bind");
    exit(1);
  }
  if(connect(server, &sockin, sizeof (sockin)) < 0){
    perror("connect");
    close(server);
    exit(1);
  }

#ifdef O_NDELAY
  fcntl(server, F_SETFL, O_NDELAY);

#ifdef USG
  /* USG pipe cannot not select emacsIn */
  {
    struct stat statbuf;
    fstat (emacsIn,&statbuf);
    if (statbuf.st_mode & 010000)
      selectable = 0;
    if (!selectable){
      signal(SIGINT,sigout);
      fcntl(emacsIn, F_SETFL, O_NDELAY);
    }
  }
#endif
#endif

  /* Connection established. */
  while(1){
    readfds = (1 << server) | (1 << emacsIn);
    if(select(32, &readfds, NULL, NULL, (struct timeval *)NULL) == -1){
      perror("select");
      exit(1);
    }
    if(readfds & (1 << emacsIn)){
      /* From Emacs */
      nbuffer = read(emacsIn, buffer, sizeof buffer -1);

#ifdef USG
      if (selectable && nbuffer == 0){
	goto finish;
      } else if (!(readfds & (1 << server)) && nbuffer == 0){
	sleep (1);
      } else 
#else
      if(nbuffer == 0)
	goto finish;
#endif
      for(retry = buffer; nbuffer > 0; nbuffer -= wret, retry += wret){
	writefds = 1 << server;
	if(select(server+1,NULL,&writefds,NULL,(struct timeval*)NULL) == -1){
	  perror("select");
	  exit(1);
	}
	wret = write(server, retry, nbuffer);
	if(wret < 0) goto finish;
      }
    }
    if(readfds & (1 << server)){
      /* From NNTP server */
      nbuffer = read(server, buffer, sizeof buffer -1);
      if(nbuffer == 0)
	goto finish;
      for(retry = buffer; nbuffer > 0; nbuffer -= wret, retry += wret){
	writefds = 1 << emacsOut;
#ifdef USG
	if(selectable)
#endif
	  if(select(emacsOut+1,NULL,&writefds,NULL,(struct timeval*)NULL) == -1){
	    perror("select");
	    exit(1);
	  }
	wret = write(emacsOut, retry, nbuffer);
	if(wret < 0) goto finish;
      }
    }
  }

  /* End of communication. */
 finish:
  close(server);
#ifdef USG
  if (!selectable) fcntl(emacsIn, F_SETFL,0);
#endif
  close(emacsIn);
  close(emacsOut);
  exit(0);
}
