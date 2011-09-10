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
 * etl_udp.c
 *
 * Purpose:
 *   This file contains UDP allocation and the like.  It only manages
 * UDP in relation to talk.  The static variable 'host_port'
 * represents the udp port hosting within this instantiation of etalk.
 * Other functions create addresses used in sends.
 *
 * ::Header:: ./etalklib.h
 */

#include "etalklib.h"

/*
 * this is the local port representing our attachment.
 */
static struct sockaddr_in  host_port;
static int                 udp_sd = 0;



/*
 * Function: receive_port()
 *
 * Returns the sockaddr of our port.  Used for filling in talk request
 * messages.
 * 
 * Parameters:
 *
 * History:
 * eml 4/1/94
 */
struct sockaddr_in *UDP_receive_port()
{
  return &host_port;
}

/*
 * Function: setup_localport
 *
 * Local function which allocats a single port for local use in UDP,
 * plus sets up the stuff we need for sending our address.
 *
 * Parameters:
 *
 * History:
 * eml 4/1/94
 */
void UDP_setup_localport()
{
  int t;			/* temp variable for addr size. */

  if(udp_sd) return;

  /* 
   * This is not in net-order because it is used in BIND
   */
  host_port.sin_family = AF_INET;
  host_port.sin_port = 0;
  host_port.sin_addr.s_addr = INADDR_ANY;  
  /*
   * allocate ourselves a socket to use
   */
  if((udp_sd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) 
    {
      perror("Socket for datagram failure");
      exit(1);
    }
  /*
   * now bind up the datagram socket to ourselves
   */
  if(bind(udp_sd, (struct sockaddr*)&host_port, sizeof(host_port)) < 0) 
    {
      perror("bind for datagram failure.");
      exit(1);
    }
  /*
   * and do the get name thing.
   */
  t = sizeof(host_port);
  if(getsockname(udp_sd, (struct sockaddr *)&host_port, &t) != 0) 
    {
      perror("getsockname");
      exit(1);
    }
  /* 
   * Finally, take this static variable used in our for
   * our port (always the same) and set it up for copies into
   * daemon messages!
   */
  host_port.sin_family = htons(AF_INET);
  /* Look to see if this step is needed. */
  host_port.sin_addr = HOST_gen_local_host()->addr.sin_addr;
}

/*
 * Function: UDP_host
 *
 * Create an inputdevice with the address field set to this machine.
 * 
 * Parameters: machine - character string of remote machine name.
 *
 * History:
 * eml 4/1/94
 */
struct InputDevice *UDP_host(machine)
     char *machine;
{
  struct HostObject  *host;	/* my host object        */
  struct InputDevice *new;	/* new input device      */
  struct sockaddr_in  hisdg;	/* target port address   */
  char               *servicename; /* service name used  */
  struct servent     *sp;
  char                serv_found = 0;

  memset(&hisdg, 0, sizeof(hisdg));
  /*
   * Get the host stuff now.
   */
  host = HOST_gen_host(machine);

  /*
   * sin port already set, and in network order, now lets
   * bind up to the service.  To use ntalk, both myself, and
   * remote needs to be of NTALK or above.  In the very first
   * case, both hosts are the same, but that's ok, since we need
   * to broadcast that addr struct with targeted TALK messages.
   */
  do {
    if((host->type > OTALKD))
      /* If we check local host too, then we won't be able to use sun
       * otalk to talk to BSD ntalk nodes, and thus, vice-versa.  Such
       * connections are only possible under etalk on both sides.
       *
       * && (HOST_gen_local_host()->type > OTALKD)) 
       * EML 7/1/94
       */
      servicename = "ntalk";
    else
      servicename = "talk";
    
    if((sp = getservbyname(servicename, "udp")) == NULL) 
      {
	fprintf(stderr, "getservbyname - Error finding service\n");
	serv_found = 0;
	if(!host->type)
	  {
	    fprintf(stderr, "No more daemon types to check for! Exiting.\n");
	    exit(1);
	  }
	/* Since the types are kept as ints, when we drop below
	 * OTALK we get -1, and break from loop.
	 */
	host->type -= 1;
      }
    else
      serv_found = 1;

  } while(! serv_found );

  hisdg            = host->addr; /* his host address */
  hisdg.sin_family = AF_INET;	/* he is using INET  */
  hisdg.sin_port   = sp->s_port; /* the port to use  */
  /*
   * now that we have all that, lets save it.
   */
  new = ET_gen_iodev(IO_UDP, udp_sd,  &hisdg);

  if(!new->host)
    new->host = host;

  new->state = IDLE;

  if(verbose)
    printf("Servent port %d looked up for %s.\n", ntohs(sp->s_port),
	   ET_dev_name(new));

  return new;
}

/*
 * Function: UDP_daemon_change
 *
 * If the daemon is changing for a port, we need a new socket pointer.
 * therefore, read new type from host, and use that.
 * 
 * Parameters: machine - character string of remote machine name.
 *
 * History:
 * eml 4/25/94
 */
int UDP_daemon_change(io)
     struct InputDevice *io;
{
  struct sockaddr_in  hisdg;	/* target port address   */
  char               *servicename; /* service name used  */
  struct servent     *sp;

  memset(&hisdg, 0, sizeof(hisdg));

  /* Set the new port in address by taking the host, and reading in
   * a new service name.
   */
  if((io->host->type > OTALKD))
    /* && (HOST_gen_local_host()->type > OTALKD))
     * Removed for same reasons as above.
     */
    servicename = "ntalk";
  else
    servicename = "talk";
  
  if((sp = getservbyname(servicename, "udp")) == NULL) 
    {
      perror("getservbyname - Error finding service");
      return Fail;
    }

  hisdg            = io->host->addr; /* his host address */
  hisdg.sin_family = AF_INET;	/* he is using INET  */
  hisdg.sin_port   = sp->s_port; /* the port to use  */
  /*
   * now that we have all that, lets save it.
   */
  io->raddr = hisdg;

  if(verbose)
    printf("Servent port %d looked up for %s during change.\n",
	   ntohs(sp->s_port), ET_dev_name(io));

  return Success;
}

#ifdef DAEMON

/*****************************************************************/

/*
 * The servent allocator is for use with a daemon to come later.
 */
struct InputDevice *UDP_udp_servent( servent_name )
     const char *servent_name;
{
  struct InputDevice *new;
  struct servent     *sp;
  int                 sd;
  int                 t;

  return NULL;			/* do nothing for now. */

  if((sp = getservbyname(servent_name, "udp")) == NULL) 
    {
      perror("udp_servent: getservbyname:");
      exit(1);
    }
  /* 
   * This is not in net-order because it is used in BIND.
   */
  host_port.sin_family = AF_INET;
  host_port.sin_port = ntohs(sp->s_port);
  host_port.sin_addr.s_addr = INADDR_ANY;

  /*
   * allocate ourselves a socket to use
   */
  if((udp_sd = socket(AF_INET,SOCK_DGRAM,0)) < 0) 
    {
      perror("Socket for datagram failure");
      exit(1);
    }
  /*
   * now bind up the datagram socket to ourselves
   */
  if(bind(udp_sd, (struct sockaddr*)&host_port, sizeof(host_port)) < 0) 
    {
      perror("bind for datagram failure.");
      exit(1);
    }
  /*
   * and do the get name thing.
   */
  t = sizeof(host_port);
  if(getsockname(udp_sd, (struct sockaddr *)&host_port, &t) != 0) 
    {
      perror("getsockname");
      exit(1);
    }

  /*
   * now that we have all that, lets save it.
   */
  new = ET_gen_iodev(IO_UDP, udp_sd, NULL);

  new->host = HOST_gen_local_host();

  new->state = IDLE;

  return new;
}
#endif
