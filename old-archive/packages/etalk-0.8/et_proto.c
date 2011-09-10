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
 * et_proto.c
 *
 * History:
 * interran@uluru.Stanford.EDU (John Interrante)
 * Spelling error in PROTOCOL_test.
 * eml
 * Increased the number of things tested against for etalk.
 *
 * Purpose:
 *  This file implements the etalk protocol.  It is dependant on the
 * recursive nature of the select_all fn with match exiting.  This
 * will allow us to detect timeouts and all that kind of goo stuff,
 * and view it logically in a linear manor.
 * 
 * ::Header:: etalk.h
 */
#include "etalklib.h"
#include "etalk.h"
#include "gtalk.h"		/* this contains retry limits and such */
#include "talk.h"
#include "otalk.h"

/* The current objects used during user aquisition
 */
static struct UserObject  *uobj = NULL;
static struct InputDevice *io = NULL;
static int                 announce_vers;
static int                 version_check = FALSE;

/*
 * This macro will simply encompass a loop to try to get a UDP message.
 * for the following attach function...
 */
#define GET_MSG(fn, io) \
{ int cnt = 0; if(!uobj || !(io) || !io) return; do \
    { if(uobj->state == USER_CLOSED) \
	{ PROTOCOL_delete_all(Ctxt, TRUE); return; } \
	if(cnt >= (UDP_RETRY)) \
	{ printf("\03%c%d\n", TTY_DELETED, uobj->id); \
	    printf("\03Retry limit reached. Abandoning Call.\n"); \
	    PROTOCOL_delete_all(Ctxt, TRUE); \
	    (io)->readme = NULL; return; } \
	if(!(fn)) \
	{ printf("\03%c%d\n", TTY_DELETED, uobj->id); \
	    printf("\03Error running function, no more actions taken.\n"); \
	    PROTOCOL_delete_all(Ctxt, TRUE); \
	    (io)->readme = NULL; return; } \
	(io)->timeout = (UDP_LIFE); (io)->readme = no_action; cnt++; } \
    while(ET_select_all(Ctxt, io) == Fail); \
    (io)->timeout = 0; (io)->readme = NULL; }

/* similar to previous macro, but instead worries about version checks
 */
#define GET_MSG_RETRY(fn, io) \
{ int cnt = 0; if(!uobj || !(io) || !io) return; do \
    { if(uobj->state == USER_CLOSED) \
	{ PROTOCOL_delete_all(Ctxt, TRUE); return; } \
	if(cnt >= (UDP_RETRY)) \
	{ printf("\03%c%d\n", TTY_DELETED, uobj->id); \
	    printf("\03Retry limit reached. Checking Version.\n"); \
	    version_check = TRUE; \
	    (io)->readme = NULL; break; } \
	if(!(fn)) \
	{ printf("\03%c%d\n", TTY_DELETED, uobj->id); \
	    printf("\03Error running function, no more actions taken.\n"); \
	    PROTOCOL_delete_all(Ctxt, TRUE); \
	    (io)->readme = NULL; return; } \
	(io)->timeout = (UDP_LIFE); (io)->readme = no_action; cnt++; } \
    while(ET_select_all(Ctxt, io) == Fail); \
    (io)->timeout = 0; (io)->readme = NULL; }


/*
 * Function: no_action
 *
 * Do nothing so that the readme flag gets set...
 * 
 * Parameters: Ctxt - talk context
 *             io   - the input device
 * History:
 * eml 4/6/94
 */
static void no_action(Ctxt, io)
     struct TalkContext *Ctxt;
     struct InputDevice *io;
{ /* Do nothing so README fn can be set while waiting about... */ }

/*
 * Function: display_timeout
 *
 * display a message discussing timeout till next announcement.
 * 
 * Parameters: Ctxt - context
 *             io - the io device in question
 * History:
 * eml 4/15/94
 */
void display_timeout(Ctxt, io)
     struct TalkContext *Ctxt;
     struct InputDevice *io;
{
  /* if the calling user object is blank, ignore. */
  if(!uobj) 
    {
      printf("protocol display_timeout called with no active user obect.\n");
      return;
    }
  
  printf("\03%cCalling %s: %d Announces, %d seconds till next ring.\n", 
	 TTY_NOLOG,
	 uobj->name,
	 announce_vers,
	 io->timeout);
}

/*
 * Function: PROTOCOL_connect
 *
 * If we mysteriously get a socket number, use that to connect directly
 * to some remote.
 * 
 * Parameters: Ctxt   - talk context
 *             userid - the user id of the emacs socket available
 *             sock   - the socket id on remote's machine
 *             user   - the user name
 *             node   - the remote node name
 *             tty    - the tty of remote
 * History:
 * eml 4/18/94
 */
void PROTOCOL_connect(Ctxt, userid, sock, user, node, tty)
     struct TalkContext *Ctxt;
     int userid;
     int sock;
     char *user;
     char *node;
     char *tty;
{
  struct HostObject *host;
  struct InputDevice *newtcp;
  struct sockaddr_in addr;

  /* now get the user struct from our list...
   */
  uobj = USER_find(userid);

  if(!uobj) {
    printf("Bad user id structure...\n");
    uobj = NULL;
    io = NULL;
    return;
  }

  if(uobj->state != USER_NEW)
    {
      printf("Attempt to call out on a used user struct!");
      uobj = NULL;
      io = NULL;
      return;
    }
  announce_vers = 1;

  uobj->name = (char *)malloc(strlen(user) + 1);
  strcpy(uobj->name, user);

  uobj->local->name = (char *)malloc(strlen(user) + 8);
  sprintf(uobj->local->name, "TCP to %s", uobj->name);

  /* Ok, now just try to connect...
   */
  host = HOST_gen_host(node);

  if(!host)
    {
      printf("Cannot connect to %s because host does not exist.\n",
	     node);
      uobj->state = USER_CLOSED;
      ET_clean_dev(uobj->local);
      return;
    }

  addr = host->addr;
  addr.sin_port = htons(sock);

  newtcp = TCP_connect((struct sockaddr *)&addr);
  
  if(newtcp)
    {
      newtcp->state = CONNECTED;
      newtcp->readme = STREAM_remote_read;
      newtcp->timeout = 0;
      newtcp->timefn = 0;
	  
      uobj->local->state = CONNECTED;
      uobj->remote = newtcp;
      uobj->state = USER_CONNECTED;
	  
      uobj->remote->name = (char *)malloc(strlen(user) + 10);
      sprintf(uobj->remote->name, "TCP from %s", uobj->name);
	  
      ET_send(newtcp, Ctxt->editkeys, NUMEDITKEYS);
	  
    }
  else
    {
      printf("\03Unable to connect to %s.\n", node);
      uobj->state = USER_CLOSED;
      ET_clean_dev(uobj->local);
    }
  uobj = NULL;
}

/*
 * Function: PROTOCOL_wait
 *
 * Wait for a connection to a given user id.
 * 
 * Parameters: Ctxt - talk context
 *             userid - the user id of the emacs socket available
 *             user - the user name
 *             node - the remote node name
 *             tty - the tty of remote
 * History:
 * eml 4/18/94
 */
void PROTOCOL_wait(Ctxt, userid, user, node, tty)
     struct TalkContext *Ctxt;
     int                 userid;
     char               *user;
     char               *node;
     char               *tty;
{
  struct InputDevice *newtcp;

  if(io || uobj)
    {
      printf("\03You may only make one outgoing call at a time!\n");
      return;
    }
  uobj = USER_find(userid);

  if(!uobj) {
    printf("Bad user id structure...\n");
    uobj = NULL;
    io = NULL;
    return;
  }

  if(uobj->state != USER_NEW)
    {
      printf("Attempt to call out on a used user struct!");
      uobj = NULL;
      io = NULL;
      return;
    }
  
  /*
   * Wait for connection from the remote...
   */
  Ctxt->remote_connect->readme  = no_action;
  Ctxt->remote_connect->timeout = RING_WAIT;
  Ctxt->remote_connect->timefn  = NULL;

  while(ET_select_all(Ctxt, Ctxt->remote_connect) == Fail)
    {
      /* first, check to see if we are even checking anymore...
       */
      if(!uobj) return;
      Ctxt->remote_connect->timeout = RING_WAIT;
    }
  
  Ctxt->remote_connect->readme  = NULL;
  Ctxt->remote_connect->timeout = 0;
  Ctxt->remote_connect->timemsg = NULL;
  /*
   * now run accept on the socket to get the new user and do the final
   * setup
   */
  newtcp = TCP_accept(Ctxt->remote_connect);
  
  newtcp->state = CONNECTED;
  newtcp->readme = STREAM_remote_read;
  newtcp->timeout = 0;
  newtcp->timefn = 0;

  uobj->local->state = CONNECTED;
  uobj->remote = newtcp;
  uobj->state = USER_CONNECTED;

  uobj->remote->name = (char *)malloc(strlen(user) + 10);
  sprintf(uobj->remote->name, "TCP from %s", uobj->name);

  ET_send(newtcp, Ctxt->editkeys, NUMEDITKEYS); 

  /* cleanup static varialbes used herin
   */
  uobj = NULL;
}


/*
 * Function: PROTOCOL_test
 *
 * Test the local talk daemon by looking up USER.  Don't bother
 * reading the message in since we'll just quit in a minute anyway.
 *
 * Parameters:  Ctxt - talk context
 *
 * History:
 * eml	May 27, 1994	Created
 * eml  Aug 10, 1994    Spelling, OTALKD and NTALKD not OTALK and NTALK
 * eml  Aug 19, 1994    Added much more comprehensive test of the
 *                      whole protocol exchange between daemon and server.
 */
int PROTOCOL_test(Ctxt)
     struct TalkContext *Ctxt;
{
  Ctxt->myname = "testing";

  printf("Checking for daemon: Sending test lookup message [V %d].\n", 
	 Ctxt->local_daemon->host->type);
  DMN_Lookup(Ctxt, Ctxt->local_daemon, "test", "");

  Ctxt->tty->readme = NULL;	/* turn off command line from intrusion */
  Ctxt->local_daemon->timeout = (UDP_LIFE); 
  Ctxt->local_daemon->readme = no_action;

  if(ET_select_all(Ctxt, Ctxt->local_daemon))
    {
      printf("Brief daemon check successful!\n");
      /* Read in the text sent back.  We don't care what it says. */
      DMN_get_lookup_response(Ctxt->local_daemon);
    }
  else
    {
      printf("Daemon check failed!\n");
      if(Ctxt->local_daemon->host->type != 0)
	{
	  printf("You may need to install ntalk or reconfigure for otalk!\n");
	  printf("Try configure --enable-OTALK_ONLY\n");
	  printf("OR putting the line:\n");
	  printf("0 %s OTALKD\n", Ctxt->local_daemon->host->name);
	  printf("into the file %s to force use of OTALKD protocol.\n",
		 LOCAL_RC);
	}
      else
	{
	  printf("You may need to install ntalk or reconfigure for ntalk!\n");
	  printf("Try putting the line:\n");
	  printf("0 %s NTALKD\n", Ctxt->local_daemon->host->name);
	  printf("into the file %s to force use of NTALKD protocol.\n",
		 LOCAL_RC);
	}
      printf("\nRead the etalk info file under `Setup Problems' for help.\n");
      return 1;
    }

  Ctxt->myname = "testing";

  printf("Initiating protocol test.\n");
  printf("Sending invitation...\n");
  DMN_LeaveInvite(Ctxt, Ctxt->myname, "");

  Ctxt->local_daemon->timeout = (UDP_LIFE); 
  Ctxt->local_daemon->readme = no_action;

  if(ET_select_all(Ctxt, Ctxt->local_daemon))
    {
      int r = DMN_get_invite_response(Ctxt->local_daemon);

      printf("Received response %s to invitation!\n",
	     DMN_last_response(Ctxt->local_daemon));
      if(r == Fail)
	{
	  printf("Invitation in protocol test failed.  See info file.\n");
	  exit(1);
	}
    }
  else
    {
      printf("Invitation in protocol test failed.  See info file.\n");
      exit(1);
    }

  printf("Sending lookup message for previous invite...\n");
  DMN_Lookup(Ctxt, Ctxt->local_daemon, Ctxt->myname, "");

  Ctxt->local_daemon->timeout = (UDP_LIFE); 
  Ctxt->local_daemon->readme = no_action;

  if(ET_select_all(Ctxt, Ctxt->local_daemon))
    {
      struct sockaddr_in *r;

      r = DMN_get_lookup_response(Ctxt->local_daemon);

      /* convert address out of network order. */
      r->sin_family = ntohs(r->sin_family);

      printf("Received response %s to lookup request!\n",
	     DMN_last_response(Ctxt->local_daemon));
      if(DMN_last_response_numeric(Ctxt->local_daemon) != SUCCESS)
	{
	  printf("Lookup in  protocol test failed.  See info file.\n");
	  exit(1);
	}
      printf("Our remote connect socket is:");
      print_sockaddr(&Ctxt->remote_connect->raddr);
      printf("\nDaemon thinks it is         :");
      print_sockaddr(r);
      printf("\n");

      if((r->sin_family != Ctxt->remote_connect->raddr.sin_family) ||
	 (r->sin_port != Ctxt->remote_connect->raddr.sin_port) ||
	 (r->sin_addr.s_addr != Ctxt->remote_connect->raddr.sin_addr.s_addr))
	{
	  printf("Response from daemon garbled our address!\n");
	  exit(0);
	}
      else
	{
	  printf("Address to our socket transferred correctly.\n");
	}
    }
  else
    {
      printf("Lookup in protocol test failed.  See info file.\n");
      exit(1);
    }

  printf("Sending deletion message for previous invite...\n");
  DMN_Delete(Ctxt, Ctxt->local_daemon, DMN_invite);

  Ctxt->local_daemon->timeout = (UDP_LIFE); 
  Ctxt->local_daemon->readme = no_action;

  if(ET_select_all(Ctxt, Ctxt->local_daemon))
    {
      int r = DMN_get_delete_response(Ctxt->local_daemon);

      printf("Received response %s to delete request!\n",
	     DMN_last_response(Ctxt->local_daemon));
      if(r == Fail)
	{
	  printf("Deletion in  protocol test failed.  See info file.\n");
	  exit(1);
	}
    }
  else
    {
      printf("Deletion in protocol test failed.  See info file.\n");
      exit(1);
    }
  printf("Protocol test successful.\n");

  return Success;
}

/*
 * Function: PROTOCOL_attach
 *
 * Simply do the "talk thing" to connect to a remote user.
 * 
 * Parameters: Ctxt - talk context
 *             uid  - userobject id number.
 *             user - the user
 *             node - the node to attach to
 *             tty  - the users tty.
 * History:
 * eml 4/6/94
 */
void PROTOCOL_attach(Ctxt, userid, user, node, tty)
     struct TalkContext *Ctxt;
     int                 userid;
     char               *user;
     char               *node;
     char               *tty;
{
  struct sockaddr_in *addr;	/* address used for TCP creation */
  struct InputDevice *newtcp;	/* new TCP socket                */

  if(io || uobj)
    {
      printf("\03You may only make one outgoing call at a time!\n");
      return;
    }

  /* First, make sure we have an IO device for talking to remote daemon.
   */
  if(!node) {
    io = Ctxt->local_daemon;	/* use local daemon for remote daemon */
    node = "";
  } else {
    io = UDP_host(node);
  }
  if(!tty) tty = "";
  
  /* now get the user struct from our list...
   */
  uobj = USER_find(userid);

  if(!uobj) {
    printf("Bad user id structure...\n");
    uobj = NULL;
    io = NULL;
    return;
  }

  if(uobj->state != USER_NEW)
    {
      printf("Attempt to call out on a used user struct!");
      uobj = NULL;
      io = NULL;
      return;
    }
  announce_vers = 1;

  uobj->name = (char *)malloc(strlen(user) + 1);
  strcpy(uobj->name, user);

  uobj->local->name = (char *)malloc(strlen(user) + 8);
  sprintf(uobj->local->name, "TCP to %s", uobj->name);

  do
    {
      version_check = FALSE;
      /* Now, lets find out if we have been invited or not by asking that
       * remote machine...
       */
      if(verbose)
	printf("Sending lookup message [V %d].\n", io->host->type);

      GET_MSG_RETRY(DMN_Lookup(Ctxt, io, user, tty), io);
      if(!version_check)
	addr = DMN_get_lookup_response(io);
      else
	addr = NULL;

      if(addr == (struct sockaddr_in *)-1) version_check = TRUE;
      
      if(addr && (addr != (struct sockaddr_in *)-1))
	{
	  addr->sin_family = ntohs(addr->sin_family);
	  
	  newtcp = TCP_connect((struct sockaddr *)addr);
	  
	  if(newtcp)
	    {
	      newtcp->state = CONNECTED;
	      newtcp->readme = STREAM_remote_read;
	      newtcp->timeout = 0;
	      newtcp->timefn = 0;
	      
	      uobj->local->state = CONNECTED;
	      uobj->remote = newtcp;
	      uobj->state = USER_CONNECTED;
	      
	      uobj->remote->name = (char *)malloc(strlen(user) + 10);
	      sprintf(uobj->remote->name, "TCP from %s", uobj->name);
	      
	      ET_send(newtcp, Ctxt->editkeys, NUMEDITKEYS);
	      
	      /* make sure we don't delete non-existing thingies
	       */
	      uobj = NULL;
	      io = NULL;
	      
	      return;
	    }
	  else
	    {
	      /*
	       * If we get crap from the daemon, just call them!!
	       */
	      printf("\03Invite information failed.  Calling out...\n");
	    }
	}
      else
	{
	  /* check for potential errors, and we may need to downgrade the
	   * supposed talk daemon we are connecting to.
	   */
	  if(version_check)
	    {
	      /* The daemon may or may not respond to this query,
	       * so slowly drop to nothing...
	       */
	      io->host->type--;
	      if(io->host->type < 0)
		{
		  printf("\03Host %s has no apparent talk daemon!!\n",
			 io->host->name);
		  io = NULL;
		  uobj = NULL;
		  return;
		}
	      UDP_daemon_change(io);
	    }
	  /*
	   * If we do get a response, and no addr, then
	   * check version error.
	   */
	  else if(DMN_last_response_numeric(io) == BADVERSION)
	    {
	      if(io->host->type == OTALKD)
		{
		  printf("\03Host %s has no apparent talk daemon!!\n",
			 io->host->name);
		  io->state = DEAD;
		  io = NULL;
		  uobj = NULL;
		  return;
		}
	      /* In this case, we must degrade down to the level descibed.
	       */
	      printf("\03Downgrading daemon versions...\n");
	      io->host->type = DMN_last_response_version(io);
	      UDP_daemon_change(io);
	    }
	  /* and if that is all ok, then just call out!!
	   */
	}
    } while((DMN_last_response_numeric(io) == BADVERSION) || version_check);

  /* if we have not yet connected, then we must:
   * 1) prepare TCP listener for a connection.
   * do {
   *   2) leave an invitation
   *   3) announce to terminal
   * } while we don't hear from him.
   *
   *************
   *
   * Loop on the RING-WAIT thing after doing the invite/announce thing.
   */
  if(verbose)
    printf("Sending invitation message.\n");

  GET_MSG(DMN_LeaveInvite(Ctxt, user, tty), Ctxt->local_daemon);
  if(DMN_get_invite_response(Ctxt->local_daemon) == Fail) 
    {
      printf("\03Error leaving invitation for %s is [%s]\n", 
	     user, DMN_last_response(Ctxt->local_daemon));
      PROTOCOL_delete_all(Ctxt, TRUE);
      return;
    }
  printf("\03Announcing...\n");
  GET_MSG(DMN_Announce(Ctxt, io, user, tty), io);
  if(DMN_get_announce_response(io) == Fail) 
    {
      printf("\03Response to announcement to %s is [%s]\n",
	     user, DMN_last_response(io));
      PROTOCOL_delete_all(Ctxt, TRUE);
      return;
    }
  
  /*
   * now wait for the connect.  If connect occurs during this time,
   * reads will be buffered... I hope anyway... so prepare rc for cnct
   */
  Ctxt->remote_connect->readme  = no_action;
  Ctxt->remote_connect->timeout = RING_WAIT;
  Ctxt->remote_connect->timefn  = NULL;
  Ctxt->remote_connect->timemsg = display_timeout;

  while(ET_select_all(Ctxt, Ctxt->remote_connect) == Fail)
    {
      /*
       * Check for a behind the back cancel of this opperation.
       */
      if(!uobj || !io) return;
      /* buffer connects until we finish doing out UDP thing 
       * by simply removing it from the Q of Input devices to read from.
       */
      Ctxt->remote_connect->readme  = NULL;
      Ctxt->remote_connect->timeout = 0;
      /*
       * There was no connect, so delete the old ones, and put in some
       * new ones...
       */ 
      PROTOCOL_delete_all(Ctxt, 0);
      /*
       * setup for receiving again.
       */
      Ctxt->remote_connect->readme  = no_action;
      Ctxt->remote_connect->timeout = RING_WAIT;
      /*
       * Now, reannounce, and leave a new copy of the invitation...
       */
      if(verbose)
	printf("Sending invite message.\n");

      GET_MSG(DMN_LeaveInvite(Ctxt, user, tty), Ctxt->local_daemon);
      if(DMN_get_invite_response(Ctxt->local_daemon) == Fail)
	{
	  printf("\03Error leaving invitation for %s is  [%s]\n", 
		 user, DMN_last_response(Ctxt->local_daemon));
	  PROTOCOL_delete_all(Ctxt, TRUE);
	  return;
	}

      printf("\03Announcing again [%d]...\n", announce_vers++);

      GET_MSG(DMN_Announce(Ctxt, io, user, tty), io);
      if(DMN_get_announce_response(io) == Fail)
	{
	  printf("\03Response to announcement to %s is [%s]\n",
		 user, DMN_last_response(io));
	  PROTOCOL_delete_all(Ctxt, TRUE);
	  return;
	}
  
    }
  /* there shouldn't be any more remote buffer connects until we
   * finish doing out UDP thing by simply removing it from the Q of
   * Input devices to read from.
   */
  Ctxt->remote_connect->readme  = NULL;
  Ctxt->remote_connect->timeout = 0;
  Ctxt->remote_connect->timemsg = NULL;
  /*
   * we have a connection, so delete the announces and invites, and
   * put in some new ones...
   */ 
  PROTOCOL_delete_all(Ctxt, 0);
  /*
   * now run accept on the socket to get the new user and do the final
   * setup
   */
  newtcp = TCP_accept(Ctxt->remote_connect);
  
  newtcp->state = CONNECTED;
  newtcp->readme = STREAM_remote_read;
  newtcp->timeout = 0;
  newtcp->timefn = 0;

  uobj->local->state = CONNECTED;
  uobj->remote = newtcp;
  uobj->state = USER_CONNECTED;

  uobj->remote->name = (char *)malloc(strlen(user) + 10);
  sprintf(uobj->remote->name, "TCP from %s", uobj->name);

  ET_send(newtcp, Ctxt->editkeys, NUMEDITKEYS); 

  /* cleanup static varialbes used herin
   */
  ET_reset_ids();
  io = NULL;
  uobj = NULL;
}

/*
 * Function: PROTOCOL_delete_all
 *
 * Delete the last announcement, and the last invitation.
 * 
 * Parameters: uobj - user object of current pass
 *
 * History:
 * eml 4/15/94
 */
void PROTOCOL_delete_all(Ctxt, terminate)
     struct TalkContext *Ctxt;
     int                 terminate;
{
  if(!io && !uobj) return;

  /*
   * We cannot use the nifty macro or we get a nasty little recursive 
   * problem.
   */
  if(verbose)
    printf("Sending delete announce message.\n");

  {
    int cnt = 0; 
    do {
      if(cnt >= (UDP_RETRY))
	{
	  printf("\03Retry limit reached. Can't delete announce.\n"); 
	  ET_clean_dev(uobj->local);
	  uobj->state = USER_CLOSED;
	  io->readme = NULL;
	  break;
	}
      else
	{
	  if(!DMN_Delete(Ctxt, io, DMN_announce))
	    { 
	      printf("\03Error running delete invite function.\n");
	      close(uobj->local->fd);
	      ET_clean_dev(uobj->local);
	      uobj->state = USER_CLOSED;
	      io->readme = NULL;
	      break;
	    }
	  io->timeout = (UDP_LIFE);
	  io->readme = no_action;
	  cnt++; 
	}
    }
    while(ET_select_all(Ctxt, io) == Fail);
    io->timeout = 0; 
    io->readme = NULL;
  }
  if(DMN_get_delete_response(io) == Fail)
    {
      printf("\03Delete announce error  %s\n", DMN_last_response(io));
    }
  if(verbose)
    printf("Sending delete invite message.\n");

  {
    int cnt = 0; 
    do {
      if(cnt >= (UDP_RETRY))
	{
	  printf("\03Retry limit reached. Can't delete invite.\n"); 
	  ET_clean_dev(uobj->local);
	  uobj->state = USER_CLOSED;
	  Ctxt->local_daemon->readme = NULL;
	  break;
	}
      else
	{
	  if(!DMN_Delete(Ctxt, Ctxt->local_daemon, DMN_invite))
	    { 
	      printf("\03Error running delete invite function.\n");
	      ET_clean_dev(uobj->local);
	      uobj->state = USER_CLOSED;
	      Ctxt->local_daemon->readme = NULL;
	      break;
	    }
	  Ctxt->local_daemon->timeout = (UDP_LIFE);
	  Ctxt->local_daemon->readme = no_action; 
	  cnt++; 
	}
    }
    while(ET_select_all(Ctxt, Ctxt->local_daemon) == Fail);
    Ctxt->local_daemon->timeout = 0; 
    Ctxt->local_daemon->readme = NULL;
  }
  if(DMN_get_delete_response(Ctxt->local_daemon) == Fail)
    {
      printf("\03Delete invite error  %s\n", 
	     DMN_last_response(Ctxt->local_daemon));
    }

  Ctxt->remote_connect->readme = NULL;
  Ctxt->remote_connect->timeout = 0;

  if(terminate)
    {
      if(verbose)
	printf("Executing terminate call information...\n");

      io = NULL;
      if(uobj)
	uobj->state = USER_CLOSED;
      if(uobj && uobj->local)
	{
	  ET_clean_dev(uobj->local);
	}
      uobj = NULL;
      ET_reset_ids();
      ET_end_recursion();	/* now cancel any lazy calls */
    }
}

/*
 * Function: PROTOCOL_abort
 *
 * Abort any currently active call.
 * 
 * Parameters: Ctxt - the current talk context
 *
 * History:
 * eml 4/19/94
 */
void PROTOCOL_abort(Ctxt)
     struct TalkContext *Ctxt;
{
  if(!io && !uobj)
    {
      printf("No active calls waiting...\n");
      return;
    }
  /* Basically, null out any active call structures.
   */
  if(io)
    {
      io->readme = NULL;
      io = NULL;
    }
  if(uobj)
    {
      if(verbose)
	printf("Call to %s UID %d deleted.\n",
	       uobj->name?uobj->name:"Unknown", uobj->id);

      ET_clean_dev(uobj->local);
      uobj->state = USER_CLOSED;
      uobj = NULL;
    }
  if(Ctxt->remote_connect->timemsg)
    {
      Ctxt->remote_connect->timemsg = NULL;
      Ctxt->remote_connect->timeout = 1; /* to free a waitfor */
      Ctxt->remote_connect->readme = NULL;
    }
}
