/*
 * Copyright (C) 1994 Free Software Foundation
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, you can either send email to this
 * program's author (see below) or write to:
 *
 *              The Free Software Foundation, Inc.
 *              675 Mass Ave.
 *              Cambridge, MA 02139, USA. 
 *
 * Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
 *
 * etl_tcp.c
 *
 * Purpose:
 *   This file contains the TCP routines used for creating sockets to
 * wait for connection, and connect to remotes.  It allows access of
 * addresses for use in UDP messages as well when leaving invitations.
 *
 * ::Header:: etalklib.h
 */

#include "etalklib.h"


/*
 * Function: TCP_listen
 *
 * Create, and set up a TCP socket to listen for connecting parties
 * 
 * Parameters: None
 *
 * History:
 * eml 3/1/94
 */
struct InputDevice *TCP_listen()
{
  struct sockaddr_in ssaddr;
  int                ss;
  int                t;
  struct InputDevice *new;
  
  /* 
   * pick any port
   */
  ssaddr.sin_port   = 0;
  ssaddr.sin_addr.s_addr = INADDR_ANY;
  /*
   * find a socket to use.
   */
  if( (ss = socket( AF_INET, SOCK_STREAM, 0 )) < 0) 
    {
      perror( "Can't allocate socket:" ); 
      return NULL;
    }
  /*
   * bind the socket to <any> address
   */
  if( bind( ss,(struct sockaddr *) &ssaddr, sizeof(ssaddr) ) < 0) 
    {
      perror( "tcp bind failure" ); 
      exit( 1 );
    }
  /*
   * Now get the socket name into the address structure.
   * This will be sent to the talk daemon which will
   * forward it to those doing lookups to our process
   */
  t = sizeof(ssaddr);
  if( getsockname( ss, (struct sockaddr *)&ssaddr, &t ) != 0) 
    {
      perror( "getsockname failure:" );
      exit( 1 );
    }
  /*
   * Now listen for any information to be available here
   */
  if( listen( ss, 1 ) < 0) 
    {
      perror( "listen failure: " ); 
      exit( 1 );
    }

  /*
   * This always comes back "0", meaning local-host, so we must
   * say otherwise.
   */
  ssaddr.sin_addr = HOST_gen_local_host()->addr.sin_addr;

  new = ET_gen_iodev(IO_TCP, ss, &ssaddr);

  new->state = LISTENING;

  return new;
}

/*
 * Function: TCP_accept
 *
 * Create and set up a socket by accepting it from a listening socket
 * 
 * Parameters: listening - the socket listening for new commers
 *
 * History:
 * eml 3/1/94
 */
struct InputDevice *TCP_accept(listening)
     struct InputDevice *listening;
{
  int ss;
  int t;
  struct sockaddr     addr;
  struct InputDevice *new;

  if(verbose)
    printf("Accepting new input device on socket %s\n",
	   ET_dev_name(listening));

  /*
   * accept information on that socket
   */
  t = sizeof(struct sockaddr);
  if( (ss = accept(listening->fd, (struct sockaddr *)&addr, &t)) < 0) 
    {
      perror("Can't allocate socket:"); 
      return NULL;
    }

  new = ET_gen_iodev(IO_TCP, ss, (struct sockaddr_in *)&addr);

  /*
   * Now get the socket name into the address structure.
   */
  t = sizeof(struct sockaddr_in);		/* how big is it */
  if(getsockname(ss, (struct sockaddr *)&new->raddr, &t) != 0) 
    {
      perror("getsockname failure:");
      exit(1);
    }
  
  /*
   * Update statistics
   */
  listening->recvc += 1;

  /*
   * this is technically inefficient and unnecessary, but save a wee
   * bit of space, and at this point, speed is not really vital. 
   */
  new->host = HOST_gen_host_by_addr((struct sockaddr *)&new->raddr, t);

  return new;
}

/*
 * Function: TCP_connect
 *
 * Create a TCP connection to the given addr, usually received from
 * a talk daemon.
 * 
 * Parameters: addr - sockaddr to connect to.
 *
 * History:
 * eml 4/6/94
 */
struct InputDevice *TCP_connect(addr)
     struct sockaddr *addr;
{
  int ss;
  struct InputDevice *new;

  /*
   * find a socket to use.
   */
  if( (ss = socket( AF_INET, SOCK_STREAM, 0 )) < 0) 
    {
      perror( "Can't allocate socket:" ); 
      return NULL;
    }

  new = ET_gen_iodev(IO_TCP, ss, (struct sockaddr_in *)addr);
  /*
   * now connect to the remote hosts.
   */
  if(connect(ss, (struct sockaddr*)&new->raddr, sizeof(new->raddr)) < 0)
    {
      perror("Can't connect socket:"); 
      return NULL;
    }

  new->host = HOST_gen_host_by_addr( (struct sockaddr *)&new->raddr, 
				    sizeof(new->raddr));
    
  return new;
}
