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
 * et_strm.c
 *
 * Purpose:
 *   This file contains the functions used while running a stream socket.
 * The basic two are REMOTESTREAM and LOCALSTREAM functions.  A remote
 * stream is connected to a remote person, and is associated with a
 * USER struct to a local stream, which is directly attached to an
 * emacs process via a TCP socket
 *
 * ::Header:: etalk.h
 */
#include "etalklib.h"
#include "etalk.h"


/*
 * Function: STREAM_local_read
 *
 * When talk is connected, use this to forward characters from emacs process
 * 
 * Parameters: Ctxt - context
 *             io - device to read from
 * History:
 * eml 4/6/94
 */
void STREAM_local_read(Ctxt, io)
     struct TalkContext *Ctxt;
     struct InputDevice *io;
{
  struct UserObject *u;
  char   buffer[100];
  int    rsize;

  /*
   * Always do one read.
   */
  rsize = ET_recv(io, buffer, 100);
  if(rsize > 0)
    {
      u = USER_iofind(io);

      if(!u)
	{
	  printf("Stream is bound to local_read but is not in user struct!\n");
	  return;
	}
      if(!u->remote)
	{
	  /*
	   * If there is no remote, then check to see timeout on remote_connect
	   * and put hyper speed on it so we can reannounce.
	   */
	  if(Ctxt->remote_connect->timeout)
	    {
	      Ctxt->remote_connect->timeout = 1;
	    }
	  return;
	}
      
      /* if the etalk version is wimpy, then make sure we send a little
       * slower than usual so it can keep up with us.
       */
      if(u->type < ETALK)
	{
	  int i;
	  for(i = 0; i< rsize; i++)
	    ET_send(u->remote, &buffer[i], 1);
	}
      else
	ET_send(u->remote, buffer, rsize);
    }
  else
    {
      printf("Connection to %s closed by emacs!\n",
	     u->name);
      
      ET_clean_dev(io);

      if((u = USER_iofind(io)) != NULL)
	{
	  printf("\03%c%d\n", TTY_DELETED, u->id);
	  /* delete if userstruct is new... */
	  if(u->state == USER_NEW)
	    PROTOCOL_delete_all(Ctxt, TRUE);

	  u->state = USER_CLOSED;

	  ET_clean_dev(u->remote);
	}
    }
}

/*
 * Function: STREAM_remote_read
 *
 * When talk is connected, use this to forward characters to emacs process
 * 
 * Parameters: Ctxt - context
 *             io - input device to read from
 * History:
 * eml 4/6/94
 */
void STREAM_remote_read(Ctxt, io)
     struct TalkContext *Ctxt;
     struct InputDevice *io;
{
  struct UserObject *u;
  char  *errmsg = "etalk error:  user struct binding error!\n";
  char   buffer[100];
  int    rsize;

  rsize = ET_recv(io, buffer, 100);

  u = USER_iofind(io);

  if(rsize > 0)
    {
      if(!u)
	{
	  printf("Stream is bound to local_read but is not in user struct!\n");
	  return;
	}
      if(!u->local)
	{
	  ET_send(io, errmsg, strlen(errmsg));
	  printf("\03%s", errmsg);
	  return;
	}
      
      ET_send(u->local, buffer, rsize);
    }
  else
    {
      if(u) {
	printf("\03Connection to %s closed by remote!\n",
	       u->name);
      }

      ET_clean_dev(io);

      if(u)
	{
	  printf("\03%c%d\n", TTY_DELETED, u->id);
	  u->state = USER_CLOSED;
	  
	  ET_clean_dev(u->local);
	}
    }  
}

