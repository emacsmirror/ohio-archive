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
 * et_local.c
 *
 * Purpose:
 *   This file contains the functions which manage basic functioning of
 * the TCP streams which are connected to the host (parent) program.
 * It handles the creation of the TCP streams as associated with user
 * structs. 
 * 
 * ::Header:: etalk.h
 */
#include "etalklib.h"
#include "etalk.h"


/*
 * Function: LOCAL_new_tcp
 *
 * When emacs_local receives a connect request, this function will be
 * called, and ACCEPT will link that TCP connection into a user struct,
 * which will have a specific ID.  That ID will be reported, and used
 * to index that user.  The user struct then waits for a CALL command
 * to use this user struct.
 * 
 * Parameters: Ctxt - the talk context
 *             io   - the id to talk to
 * History:
 * eml 4/11/94
 */
void LOCAL_new_tcp(Ctxt, io)
     struct TalkContext *Ctxt;
     struct InputDevice *io;
{
  struct UserObject *new;
  /*
   * Being called indicates a new connection.  USER structs will always
   * be created here since we must have a local connect to entice a remote
   * connect.
   */
  new = USER_alloc();
  
  if(!new)
    {
      printf("Error creating new user!\n");
      return;
    }
  /*
   * Now set new's connection TCP.
   */
  new->local = TCP_accept(Ctxt->emacs_connect);

  if(!new->local)
    {
      printf("ACCEPT Connection Failed!\n");
      new->state = USER_CLOSED;
      return;
    }

  /* 
   * and finally report the connection is active...
   */
  printf("\03%c%d\n", TTY_USERID, new->id);
  /*
   * and now, set some state variables on the new connection.
   */
  new->local->state   = WAITING;
  new->local->readme  = STREAM_local_read;
  new->local->timeout = 0;
  new->local->timefn  = NULL;

  if(verbose)
    printf("New user struct built and returned from dev %s.\n", 
	   ET_dev_name(io));
}
