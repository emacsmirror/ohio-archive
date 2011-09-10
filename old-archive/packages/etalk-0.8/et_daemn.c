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
 * et_daemn.c
 *
 * Purpose:
 *   This file contains all those functions needed for talking to the
 * talk daemon.
 *   Local static variables include the announce_id and invitation_id,
 * with the understanding that only one call will be made at a time
 * over the single available udp port kept open.
 * 
 * History:
 * eml 8/17/94
 * Added check against messages sizes to aid in initial debugging.
 *
 * ::Header:: etalk.h
 */
#include "etalklib.h"
#include "etalk.h"
#include "talk.h"
#include "otalk.h"
#include "gtalk.h"

static long invite_id = 0;
static long announce_id = 0;

static int Response_Display();

/*
 * Special unions so one pointer may point to all types of responses
 */
union ctl_msg {
  CTL_MSG_OLD otalk;
  CTL_MSG     talk;
  CTL_MSG_GNU gtalk;
};

union ctl_response {
  CTL_RESPONSE_OLD otalk;
  CTL_RESPONSE     talk;
  CTL_RESPONSE_GNU gtalk;
};

static union ctl_msg      Control;
static union ctl_response Response;

static char *msg_types[] = {
  "Leave Invite",
  "Look up",
  "Delete",
  "Announce",
};
static char *msg_responses[] = {
  "Success",
  "Not Here",
  "Failed",
  "Machine Unknown",
  "Permision Denied",
  "Unknown Request",
  "Bad Version",
  "Bad Address",
  "Bad Control Address"
};

/*
 * Function: control_size, response_size
 *
 * These functions return the size of each portion of the
 * previously declaired unions.  This allows a send based
 * on message size which, in turn, is based on daemon type.
 * 
 * Parameters: io - device we are sending to.  need to know type.
 *
 * History:
 * eml 3/1/94
 */
static int control_size(io)
     struct InputDevice *io;
{
  switch(io->host->type) {
  case GTALKD: return sizeof(Control.gtalk);
  case NTALKD: return sizeof(Control.talk);
  case OTALKD: return sizeof(Control.otalk);
  default: return 0;
  }
}
static int response_size(io)
     struct InputDevice *io;
{
  switch(io->host->type) {
  case GTALKD: return sizeof(Response.gtalk);
  case NTALKD: return sizeof(Response.talk);
  case OTALKD: return sizeof(Response.otalk);
  default: return 0;
  }
}


/*
 * Function: ET_control_print
 *
 * the following debugging command will print the contents of the 
 * control message.
 * 
 * Parameters: io - the device to print (for type of msg)
 *
 * History:
 * eml 3/1/94
 */
void ET_control_print(io)
     struct InputDevice *io;
{
  printf("Control message is:\n");
  switch(io->host->type) {
  case GTALKD:
  case NTALKD:
    printf("Control.talk.vers     : %d\n", Control.talk.vers);
    if(Control.talk.type > ANNOUNCE)
      printf("Control.talk.type     : Unknown %d\n", Control.talk.type);
    else
      printf("Control.talk.type     : %s\n", msg_types[Control.talk.type]);
    printf("Control.talk.addr     : "); 
    print_swapped_sockaddr(&Control.talk.addr);
    printf("\nControl.talk.ctl_addr : ");
    print_swapped_sockaddr(&Control.talk.ctl_addr);
    printf("\nControl.talk.pid      : %ld\n", htonl(Control.talk.pid));
    printf("Control.talk.id_num   : %d\n", htonl(Control.talk.id_num));
    printf("Control.talk.l_name   : %s\n", Control.talk.l_name);
    printf("Control.talk.r_name   : %s\n", Control.talk.r_name);
    printf("Control.talk.r_tty    : %s\n", Control.talk.r_tty);
    break;
  case OTALKD:
    if(Control.otalk.type > ANNOUNCE)
      printf("Control.otalk.type     : Unknown %d\n", Control.otalk.type);
    else
      printf("Control.otalk.type     : %s\n", msg_types[Control.otalk.type]);
    printf("Control.otalk.addr     : ");
    print_swapped_sockaddr(&Control.otalk.addr);
    printf("\nControl.otalk.ctl_addr : ");
    print_swapped_sockaddr(&Control.otalk.ctl_addr);
    printf("\nControl.otalk.pid      : %d\n", htonl(Control.otalk.pid));
    printf("Control.otalk.id_num   : %d\n", htonl(Control.otalk.id_num));
    printf("Control.otalk.l_name   : %s\n", Control.otalk.l_name);
    printf("Control.otalk.r_name   : %s\n", Control.otalk.r_name);
    printf("Control.otalk.r_tty    : %s\n", Control.otalk.r_tty);
    break;
  default:
    printf("print_control: Input device host has daemon type %d.\n",
	   io->host->type);
  }
}


/*
 * Function: DMN_check_compile
 *
 * Checks the compiled sizes of talk daemon messages against those
 * stored in config.h
 *
 * Returns:     int  - status
 * Parameters:  None
 *
 * History:
 * eml     Aug 17, 1994    
 */
int DMN_check_compile()
{
#define SOC sizeof(CTL_MSG_OLD)
#define SOR sizeof(CTL_RESPONSE_OLD)
#define NOC sizeof(CTL_MSG)
#define NOR sizeof(CTL_RESPONSE)
#define GOC sizeof(CTL_MSG_GNU)
#define GOR sizeof(CTL_RESPONSE_GNU)

  if((SOC != OTALK_CM_SIZE) || (SOR != OTALK_CMR_SIZE) ||
     (NOC != NTALK_CM_SIZE) || (NOR != NTALK_CMR_SIZE) ||
     (GOC != GTALK_CM_SIZE) || (GOR != GTALK_CMR_SIZE))
    {
      printf("\nERROR in compile sizes!\n\n");
      printf("Old control %d, Old response %d\n", SOC, SOR);
      printf("New control %d, New response %d\n", NOC, NOR);
      printf("GNU control %d, GNU response %d\n", GOC, GOR);
      printf("\nSee config.h for desired sizes.\n");
      return Fail;
    }
  else
    {
      return Success;
    }
}

/*
 * Function: setup_ctl_msg
 *
 * Local function used to fill in the parts of a control message based
 * on address type of input device.
 * 
 * Parameters: Ctxt - context of the program
 *             io - the iodevice sending to (for type)
 *             target - address to save at daemone side.
 *             type - type of message to send
 *             id_num - id of message for bookeeping
 *             r_user - remote usename
 *             r_tty - remote tty name
 * History:
 * eml 3/1/94
 */
static int setup_ctl_msg(Ctxt,io, target, type, id_num, r_user, r_tty)
     struct TalkContext *Ctxt;
     struct InputDevice *io;
     struct sockaddr    *target;
     u_char              type;
     long                id_num;
     char               *r_user;
     char               *r_tty;
{
  if(! io->host) {
    printf("setup_ctl_msg: Input device has no associated host.\n");
    return Fail;
  }

  switch(io->host->type) {
  case GTALKD:
    Control.gtalk.vers    = TALK_VERSION_GNU;
    /* no such thing but its close to ntalk */
  case NTALKD:
    /* They are practically the same, so just do one check
     * in here to make above flow down easilly...
     */
    if(io->host->type == NTALKD)
      Control.talk.vers     =  TALK_VERSION;
    Control.talk.type     =  type;
    Control.talk.addr     = *target;
    ((struct sockaddr_in *)&Control.talk.addr)->sin_family = 
      htons(AF_INET);
    Control.talk.ctl_addr = *(struct sockaddr *)UDP_receive_port();
    Control.talk.pid      =  htonl( Ctxt->pid );
    Control.talk.id_num   =  htonl( id_num );
    strcpy( Control.talk.l_name, Ctxt->myname );
    strcpy( Control.talk.r_name, r_user );
    if(r_tty)
      strcpy( Control.talk.r_tty,  r_tty );
    else
      Control.talk.r_tty[0] = 0;
    break;
  case OTALKD:
    Control.otalk.type     =  type;
    Control.otalk.addr     = *target;
    ((struct sockaddr_in *)&Control.otalk.addr)->sin_family = 
      htons(AF_INET);
    Control.otalk.ctl_addr = *(struct sockaddr *)UDP_receive_port();
    Control.otalk.pid      =  htonl( Ctxt->pid );
    Control.otalk.id_num   =  htonl( id_num );
    strcpy( Control.otalk.l_name, Ctxt->myname );
    strcpy( Control.otalk.r_name, r_user );
    if(r_tty)
      strcpy( Control.otalk.r_tty,  r_tty );  
    else
      Control.otalk.r_tty[0] = 0;
    break;
  default:
    printf("setup_ctl_msg: Input device host has daemon type %d.\n",
	   io->host->type);
    return Fail;
  }

  return Success;
}

/*
 * Function: ET_reset_ids
 *
 * IDs must be reset after a connection, therefore supply this function.
 * 
 * Parameters:
 *
 * History:
 * eml 4/15/94
 */
void ET_reset_ids()
{
  announce_id = 0;
  invite_id = 0;
}

/*
 * Function: send_control, receive_response
 *
 * The following send/recv functions are specifically for sending the
 * variable size control/response messages.  They are variable size in
 * support of the different version of talk available.
 * 
 * Parameters: io - the io device to send/receive from.
 *
 * History:
 * eml 3/1/94
 */
static int send_control(io)
     struct InputDevice *io;
{
  int result;

  result = ET_send(io,		/* talk daemon           */
		   &Control,	/* Control psuedo global */
		   control_size(io)); /* msg size to io        */
  if(verbose)
    {
      printf("Sending a control message over %s...\n", ET_dev_name(io));
      ET_control_print(io);
    }

  if(result == Fail) {
    printf("Error sending control message\n");
    return Fail;
  } else {
    return Success;
  }
}
static int recv_response(io)
     struct InputDevice *io;
{
  int result = 
    ET_recv(io,			/* talk daemon            */
	    &Response,		/* responce psuedo global */
	    response_size(io)); /* msg size to io         */

  if(result == Fail) {
    printf("Error receiving response message\n");
    return Fail;
  } 

  if(verbose)
    printf("Receiving response message on %s...\n", ET_dev_name(io));

  if(verbose)
    Response_Display(&Response, io);

  /*
   * Take the response, and host to network convert it right now before
   * anyone gets confused somewhere else!!!!!!!!!!!!!!!!!
   */
  switch( io->host->type ) {
  case GTALKD:
  case NTALKD:
    Response.talk.id_num = ntohl( Response.talk.id_num );
    break;
  case OTALKD:
    Response.otalk.id_num = ntohl( Response.otalk.id_num );
    break;
  default:
    printf("recv_response: IO device has wrong host daemon type %d.\n",
	   io->host->type);
    return Fail;
  }

  return Success;
}

/*
 * The following batch of functions are for Sending of a control
 * message, and take care of all the icky setup.  The messages we like
 * to send are Lookups, Announces, and Deletes.
 */

/*
 * Function: DMN_LeaveInvite
 *
 * Leave an invitation on my machine.  All needed info is in the
 * context structure.
 * 
 * Parameters: Ctxt - context
 *             r_user - remote user to ask about
 *             r_tty - remote users tty.
 * History:
 * eml 4/15/94
 */
int DMN_LeaveInvite(Ctxt, r_user, r_tty)
     struct TalkContext *Ctxt;
     char               *r_user;
     char               *r_tty;
{
  int result;

  /* Only set up if using new data...
   */
  if(r_user)
    {
      /*
       * Load in the local-global variable Control with this control message
       */
      result = setup_ctl_msg(Ctxt,
			     Ctxt->local_daemon, /* to local daemon */
			     (struct sockaddr *)&Ctxt->remote_connect->raddr,
			     LEAVE_INVITE,
			     ++invite_id,
			     r_user,
			     r_tty);
      if(result == Fail)
	{
	  printf("Error creating control message for leaving invite\n");
	  return Fail;
	}
    }

  result = send_control(Ctxt->local_daemon);

  return result;
}

/*
 * Function: DMN_Lookup
 *
 * Look for an invitation on remote machine to see if they are waiting
 * for us.
 * 
 * Parameters: Ctxt - context
 *             io - the input device of remote daemon
 *             r_user - the remote user
 *             r_tty - remote users tty
 * History:
 * eml 4/15/94
 */
int DMN_Lookup(Ctxt, io, r_user, r_tty)
     struct TalkContext *Ctxt;
     struct InputDevice *io;
     char               *r_user;
     char               *r_tty;
{
  int result;

  /*
   * Only do a setup if we get a new user. Otherwise, we are doing a
   * udp resend (because of lossage) and must use the same info.
   */
  if(r_user) {
    result = setup_ctl_msg(Ctxt,
			   io,
			   (struct sockaddr *)UDP_receive_port(),
			   LOOK_UP, 
			   0,
			   r_user,
			   r_tty);
    if(result == Fail)
      {
	printf("Error creating control message for lookup\n");
	return Fail;
      }
  }

  result = send_control(io);

  return result;
}

/*
 * Function: DMN_Delete
 *
 * delete an id from a given daemon.  Deamon is defined by IO structure
 * passed in.
 * 
 * Parameters: Ctxt - context
 *             io - io of machine we are deleting stuff from
 *             type - either DMN_invite, or DMN_announce
 * History:
 * eml 4/15/94
 */
int DMN_Delete(Ctxt, io, type)
     struct TalkContext    *Ctxt;
     struct InputDevice    *io;
     enum DMN_deletion_type type;
{
  int  result;
  long id;

  if(type == DMN_invite)
    id = invite_id;
  else
    id = announce_id;

  if(verbose)
    printf("Deleting type %d with id %ld\n", type, id);
  
  result = setup_ctl_msg(Ctxt,
			 io,
			 (struct sockaddr *)UDP_receive_port(),
			 DELETE, 
			 id,
			 "",
			 "");
  if(result == Fail)
    {
      printf("Error creating control message for Delete\n");
      return Fail;
    }

  result = send_control(io);

  return result;
}

/*
 * Function: DMN_Announce
 *
 * Send an announcement message
 * 
 * Parameters: Ctxt - context
 *             io - the io device to announce to
 *             r_user - remote user
 *             r_tty - remote users tty
 * History:
 * eml 4/15/94
 */
int DMN_Announce(Ctxt, io, r_user, r_tty)
     struct TalkContext *Ctxt;
     struct InputDevice *io;
     char               *r_user;
     char               *r_tty;
{
  int result;

  result = setup_ctl_msg(Ctxt,
			 io,
			 (struct sockaddr *)UDP_receive_port(),
			 ANNOUNCE, 
			 ++announce_id,
			 r_user,
			 r_tty);
  if(result == Fail)
    {
      printf("Error creating control message for announce.\n");
      return Fail;
    }

  result = send_control(io);

  return result;
}

/*
 * this next batch of functions all deal in the receiving of messages.
 * usually, all we care about are simple things like a lookup with a
 * YES/NO response, and invite/deletes which have ID numbers
 * associated which we need to return.  Lookups will also contain a
 * persons ADDR struct so use that as return value.
 *
 * Function: DMN_get_lookup_response
 *
 * Receive the lookup response, and return the address contained therin.
 * 
 * Parameters: io - the device to get it from
 *
 * History:
 * eml 4/15/94
 */
struct sockaddr_in *DMN_get_lookup_response(io)
     struct InputDevice *io;
{

  if(! recv_response(io))
    {
      printf("\03Error receiving lookup response.\n");
      return (struct sockaddr_in *)-1;
    }
    
  switch(io->host->type)
    {
    case GTALKD:
    case NTALKD:
      if(Response.talk.answer == SUCCESS)
	return (struct sockaddr_in *)&Response.talk.addr;
      else
	if(Response.talk.answer != NOT_HERE)
	  printf("\03lookup response: %s\n",
		 msg_responses[Response.talk.answer]);
      break;
    case OTALKD:
      if(Response.otalk.answer == SUCCESS)
	return  (struct sockaddr_in *)&Response.otalk.addr;
      else
	if(Response.otalk.answer != NOT_HERE)
	  printf("\03lookup response: %s\n",
		 msg_responses[Response.otalk.answer]);
      break;
    default:
      printf("\03get_lookup_response: IO device has wrong host daemon type %d.\n",
	     io->host->type);
    }
  return NULL;
}


/*
 * Function: DMN_get_invite_response
 *
 * Receive a response to an invitation, and record the id returned for
 * deletion later.  The return type on local machines will say if that
 * person is logged on or not.
 * 
 * Parameters: io - the device to read from
 *
 * History:
 * eml 4/15/94 */
int DMN_get_invite_response(io)
     struct InputDevice *io;
{
  if(! recv_response(io))
    {
      printf("\03Error receiving invite response.\n");
      return Fail;
    }

  switch(io->host->type)
    {
    case GTALKD:
    case NTALKD:
      /* This should be important, but talkd actually returns NOT_HERE
       * because the invitation is NOT THERE because we are sending the
       * first one (always because we delete as we go backwards.
       if(Response.talk.answer == NOT_HERE)
	{
	  return Fail;
	}
      */
      invite_id = Response.talk.id_num;
      return Success;
    case OTALKD:
      /*
      if(Response.otalk.answer == NOT_HERE)
	{
	  return Fail;
	}
	*/
      invite_id = Response.otalk.id_num;
      return Success;
    default:
      printf("\03get_invite_response: IO device has wrong host daemon type %d.\n",
	     io->host->type);
    }

  return Fail;
}

/*
 * Function: DMN_get_announce_response
 *
 * Receive the response to an announce message, and save the id for
 * deletion later.
 * 
 * Parameters: io - the input device to read from
 *
 * History:
 * eml 4/15/94
 */
int DMN_get_announce_response(io)
     struct InputDevice *io;
{
  if(! recv_response(io))
    {
      printf("\03Error receiving announce response.\n");
      return Fail;
    }

  switch(io->host->type)
    {
    case GTALKD:
    case NTALKD:
      if(Response.talk.answer != SUCCESS)
	return Fail;
      announce_id = Response.talk.id_num;
      return Success;
    case OTALKD:
      if(Response.otalk.answer != SUCCESS)
	return Fail;
      announce_id = Response.otalk.id_num;
      return Success;
    default:
      printf("\03get_announce_response: IO device has wrong host daemon type %d.\n",
	     io->host->type);
    }

  return Fail;
}


/*
 * Function: DMN_get_delete_response
 *
 * Receive the response to a deletion request.  It's presense is
 * usually a good sign, so don't worry if it really didn't succede
 * since we'd be wasting our time trying to figure out why.
 * 
 * Parameters: io - the device to read from
 *
 * History:
 * eml 4/15/94
 */

int DMN_get_delete_response(io)
     struct InputDevice *io;
{
  if(! recv_response(io))
    {
      printf("\03Error receiving delete response.\n");
      return Fail;
    }

  if(verbose)
    printf("Delete response is %s\n", DMN_last_response(io));

  /* reguardless of success, report success because we received it. */
  switch(io->host->type)
    {
    case GTALKD:
    case NTALKD:
      return Success;
    case OTALKD:
      return Success;
    default:
      printf("\03get_delete_response: IO device has wrong host daemon type %d.\n",
	     io->host->type);
    }

  return Fail;
}


/*
 * Function: DMN_last_response, DMN_last_response_numeric, 
 *           DMN_last_response_version
 *
 * Return a string representation of the response from the last
 * message.  Good for error messages.
 * Return the integer response from the last message.
 * Return the version from the last response message.
 * 
 * Parameters: io - the device received on for parsing message contents.
 *
 * History:
 * eml 4/15/94
 */
char *DMN_last_response(io)
     struct InputDevice *io;
{
  switch(io->host->type)
    {
    case GTALKD:
    case NTALKD:
      return msg_responses[Response.talk.answer];
    case OTALKD:
      return msg_responses[Response.otalk.answer];
    default:
      return "Unknown io type";
    }
}

int DMN_last_response_numeric(io)
     struct InputDevice *io;
{
  switch(io->host->type)
    {
    case GTALKD:
    case NTALKD:
      return Response.talk.answer;
    case OTALKD:
      return Response.otalk.answer;
    default:
      return -1;
    }
}

int DMN_last_response_version(io)
     struct InputDevice *io;
{
  switch(io->host->type)
    {
    case GTALKD:
    case NTALKD:
      return Response.talk.vers;
    case OTALKD:
      return 0;			/* we have no method of reading vers */
    default:
      return -1;
    }
}

/*
 * Function: Response_Display
 *
 * Print out the contents of the last response message.
 * 
 * Parameters: io - device received on for printing correct form.
 *
 * History:
 * eml 4/15/94
 */
static int Response_Display(response, io)
     union ctl_response *response; /* the response (saved?)    */
     struct InputDevice *io;	/* where the message came from */
{
  printf("Response message is:\n");
  switch(io->host->type)
    {
    case GTALKD:
    case NTALKD:
      printf("Response.talk.vers    : %d\n", response->talk.vers);
      printf("Response.talk.type    : %s\n", msg_types[response->talk.type]);
      printf("Response.talk.answer  : %s\n", msg_responses[response->talk.answer]);
      printf("Response.talk.id_num  : %d\n", response->talk.id_num);
      printf("Response.talk.addr    : ");
      print_swapped_sockaddr(&response->talk.addr);
      printf("\n");
      break;
    case OTALKD:
      printf("Response.otalk.type   : %s\n", msg_types[response->otalk.type]);
      printf("Response.otalk.answer : %s\n", msg_responses[response->otalk.answer]);
      printf("Response.otalk.id_num : %d\n", response->otalk.id_num);
      printf("Response.otalk.addr   : ");
      print_swapped_sockaddr(&response->otalk.addr);
      printf("\n");
      break;
    default:
      printf("Response_Display: IO device has wrong host daemon type %d.\n",
	     io->host->type);
    }
  printf("invite_id             : %ld\n", invite_id);
  printf("announce_id           : %ld\n", announce_id);

  return Success;
}


/*
 * Function: DMN_get_and_display
 *
 * Read in the response message, and print it out.  Good for debugging
 * with the few innocuous command line fns like ANNOUNCE and LOOKUP
 * 
 * Parameters: Ctxt - context
 *             io - the io device pending
 * History:
 * eml 4/15/94
 */
void DMN_get_and_display(Ctxt, io)
  struct TalkContext *Ctxt;
  struct InputDevice *io;
{
  int result;

  result = recv_response(io);

  /* If verbosity is on, then response is already printed!
   */
  if(! verbose)
    Response_Display(&Response, io);

  io->readme = NULL;
}
