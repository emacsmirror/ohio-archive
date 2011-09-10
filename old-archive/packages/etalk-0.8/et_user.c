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
 * et_user.c
 *
 * Purpose:
 *   This file creates/delets and otherwise manages user structures
 * which are run-time objects used while dealing with live connections
 * to other people.
 * 
 * ::Header:: etalk.h */
#include "etalklib.h"
#include "etalk.h"

static struct UserObject *Q_first = NULL;
static struct UserObject *Q_last = NULL;
/*
 * Start our ids at 1 so 0 can be an unfilled flag...
 */
static int user_id = 1;


/*
 * Function: USER_alloc
 *
 * Create and add one user to the user structure list.  There are no
 * parameters since those parts are added later on...
 * 
 * Parameters: None
 *
 * History:
 * eml  4/11/94
 */
struct UserObject *USER_alloc()
{
  struct UserObject *new;

  new = (struct UserObject *)malloc(sizeof(struct UserObject));

  if(!new)
    {
      perror("malloc failure!! : ");
      return NULL;
    }

  memset(new, 0, sizeof(struct UserObject));

  new->state = USER_NEW;
  new->type  = VanillaTalk;
  new->id    = user_id;

  user_id++;			/* setup for next user id. */

  if(Q_last) {
    Q_last->next = new;
  } else {
    Q_first = new;
  }
  new->prev = Q_last;
  new->next = NULL;
  Q_last = new;

  return new;
}

/*
 * Function: USER_clean
 *
 * Clean out all user structs which are labelled as CLOSED
 * 
 * Parameters:
 *
 * History:
 * eml 4/21/94
 */
void USER_clean()
{
  struct UserObject *u;

  u = Q_first;
  while(u)
    {
      if(u->state == USER_CLOSED)
	{
	  if(verbose)
	    printf("Freeing user %s ...\n", u->name?u->name:"Unknown");

	  if(u == Q_first)
	    {
	      if(u == Q_last)
		{
		  Q_first = Q_last = NULL; /* nothing left */
		}
	      else
		{
		  u->next->prev = NULL; /* just the first guy */
		  Q_first = u->next;
		}
	    }
	  else
	    {
	      if(u == Q_last)
		{
		  u->prev->next = NULL; /* the last guy */
		  Q_last = u->prev;
		}
	      else
		{
		  u->next->prev = u->prev; /* somewhere in the middle */
		  u->prev->next = u->next;
		}
	    }
	  {
	    struct UserObject *t = u;
	    u = u->next;
	    free(t);		/* free him up. */
	  }
	}
      else
	u = u->next;
    }  
}

/*
 * Function: USER_find
 *
 * When given the id, return a pointer to the structure containing that users
 * information
 * 
 * Parameters: id - the id of the user
 *
 * History:
 * eml 4/11/94
 */
struct UserObject *USER_find(id)
     int id;
{
  struct UserObject *u;

  u = Q_first;

  while(u && (u->id != id)) u = u->next;

  return u;
}

/*
 * Function: USER_iofind
 *
 * Find a user struct based on an io device.
 * 
 * Parameters: io - the io device
 *
 * History:
 * eml 4/12/94
 */
struct UserObject *USER_iofind(io)
     struct InputDevice *io;
{
  struct UserObject *u;

  u = Q_first;

  /* Look at both local and remotes.  We should never get
   * multiple instances of these things...
   */
  while(u && (u->remote != io) && (u->local != io)) u = u->next;

  return u;
}

/*
 * Function: USER_hangup
 *
 * Hangup on a selected user.
 * 
 * Parameters: id - user id or -1 for everyone.
 *
 * History:
 * eml 4/14/94
 */
void USER_hangup(Ctxt, id)
     struct TalkContext *Ctxt;
     int                 id;
{
  struct UserObject *u;
  int start, end, i;

  if(id == -1) {
    start = 1;
    end = user_id;
  } else {
    start = id;
    end = id + 1;
  }

  for(i = start; i < end; i++)
    {
      u = USER_find(i);		/* get the user struct      */

      if(!u) continue;		/* if error, keep on trukin */

      if(u->state == USER_CLOSED) continue;

      printf("\03%c%d\n", TTY_DELETED, u->id);

      ET_clean_dev(u->local);

      ET_clean_dev(u->remote);

      if(u->state == USER_NEW)
	PROTOCOL_delete_all(Ctxt, TRUE);

      u->state = USER_CLOSED;
    }
}

/*
 * Function: USER_print
 *
 * Print out a formatted list of the users we are keeping track of,
 * including all new and all dead.
 * 
 * Parameters: none
 *
 * History:
 * eml 4/14/94
 */
void USER_print()
{
  static char *us[] = { "NEW", "CONNECT", "DEAD" };
  static char *tp[] = { "Unknown", "Vanilla", "ETALK", "YTALK" };
  struct UserObject *u;

  if(Q_first)
    {
      printf("Id Num\tName\tState\tTalkPrg\tC Sent\tC Recv\tSend P\tRecv P\tHost\n");

      u = Q_first;
      while(u) 
	{
	  printf("%d\t%-7s\t%s\t%s\t%-7ld\t%-7ld\t%d\t%d\t%s\n",
		 u->id,
		 u->name?u->name:"Unknown", 
		 ((u->state<3)&&(u->state>=0))?us[u->state]:"bad val",
		 ((u->type<=3)&&(u->type>=0))?tp[u->type]:"bad val",
		 u->remote->sendc, 
		 u->local->sendc,
		 u->local->fd,
		 u->remote?u->remote->fd:-1,
		 ET_dev_name(u->remote));
	  u = u->next;
	}
    }
  else
    {
      printf("No users currently active.\n");
    }
}

