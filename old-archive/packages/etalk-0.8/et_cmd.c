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
 * et_cmd.c
 *
 * Purpose:
 *   This file contains the main wait loop, and subordinate functions
 * used to parse input based on descriptor type in the object lists.
 *
 * History:
 * eml Aug 10, 1994 Modified cmd_test to return error status.
 *
 * ::Header:: etalk.h
 */

#include "etalklib.h"
#include "etalk.h"

#define BUFSIZE 300

struct command_associate {
  char *command;
  char *description;
#ifdef PROTOTYPES
  int (*parse)(struct TalkContext *Ctxt, char *cmdline);
#else
  int (*parse)();
#endif
};

#ifdef PROTOTYPES
static int ETC_abort(struct TalkContext *Ctxt, char *cmd);
#ifdef TALKDTEST
static int ETC_announce(struct TalkContext *Ctxt, char *cmd);
#endif
static int ETC_call(struct TalkContext *Ctxt, char *cmd);
static int ETC_clean(struct TalkContext *Ctxt, char *cmd);
static int ETC_client(struct TalkContext *Ctxt, char *cmd);
static int ETC_connect(struct TalkContext *Ctxt, char *cmd);
#ifdef TALKDTEST
static int ETC_delete(struct TalkContext *Ctxt, char *cmd);
#endif
static int ETC_device(struct TalkContext *Ctxt, char *cmd);
static int ETC_echo(struct TalkContext *Ctxt, char *cmd);
static int ETC_editc(struct TalkContext *Ctxt, char *cmd);
static int ETC_hangup(struct TalkContext *Ctxt, char *cmd);
static int ETC_help(struct TalkContext *Ctxt, char *cmd);
static int ETC_host(struct TalkContext *Ctxt, char *cmd);
#ifdef TALKDTEST
static int ETC_leave(struct TalkContext *Ctxt, char *cmd);
static int ETC_lookup(struct TalkContext *Ctxt, char *cmd);
#endif
static int ETC_name(struct TalkContext *Ctxt, char *cmd);
static int ETC_quit(struct TalkContext *Ctxt, char *cmd);
static int ETC_read(struct TalkContext *Ctxt, char *cmd);
static int ETC_status(struct TalkContext *Ctxt, char *cmd);
static int ETC_test(struct TalkContext *Ctxt, char *cmd);
static int ETC_users(struct TalkContext *Ctxt, char *cmd);
static int ETC_wait(struct TalkContext *Ctxt, char *cmd);
static int ETC_verbose(struct TalkContext *Ctxt, char *cmd);
static int ETC_version(struct TalkContext *Ctxt, char *cmd);
#else
static int ETC_abort();
#ifdef TALKDTEST
static int ETC_announce();
#endif
static int ETC_call();
static int ETC_clean();
static int ETC_client();
static int ETC_connect();
#ifdef TALKDTEST
static int ETC_delete();
#endif
static int ETC_device();
static int ETC_echo();
static int ETC_editc();
static int ETC_hangup();
static int ETC_help();
static int ETC_host();
#ifdef TALKDTEST
static int ETC_leave();
static int ETC_lookup();
#endif
static int ETC_name();
static int ETC_quit();
static int ETC_read();
static int ETC_status();
static int ETC_test();
static int ETC_users();
static int ETC_wait();
static int ETC_verbose();
static int ETC_version();
#endif

static struct command_associate Commands[] =
{
  { "ABORT",    "Abort any currently active call",            ETC_abort },
#ifdef TALKDTEST
  { "ANNOUNCE", "Announce USER[@MACHINE] to send an announce message", ETC_announce },
#endif
  { "CALL",     "Call UID USER[@MACHINE[ TTY]] to make connection", ETC_call},
  { "CLEAN",    "Clean all users and devs marked DEAD",       ETC_clean },
  { "CLIENT",   "Client UID <DEFAULT | ETALK | YTALK>",       ETC_client },
  { "CONNECT",  "Connect UID Socket USER[@MACHINE[ TTY]] to make a direct connection", ETC_connect },
#ifdef TALKDTEST
  { "DELETE",   "Delete <ANNOUNCE | INVITE> [host] to delete id from host", ETC_delete},
#endif
  { "DEVICES",  "Get a list of all device structures",        ETC_device },
  { "ECHO",     "Echo command line back (Debug)",              ETC_echo },
  { "EDITCHAR", "Editchar 123 to set the edit characters",    ETC_editc },
  { "HANGUP",   "hangup UID to hangup on this user",          ETC_hangup },
  { "HELP",     "These help messages",                        ETC_help },
  { "HOSTS",    "Print a list of all hosts we have accessed", ETC_host },
#ifdef TALKDTEST
  { "LEAVE",    "leave USER[@MACHINE] to leave an invitation",ETC_leave },
  { "LOOKUP",   "Lookup USER[@MACHINE] to lookup an invite",  ETC_lookup },
#endif
  { "NAME",	"Name MYANNOUNCENAME to set your used name",  ETC_name },
  { "QUIT",     "Quit etalk binary",                          ETC_quit },
  { "READ",     "Re-read the RC files for host names.",       ETC_read },
  { "STATUS",   "Display compile, runtime statistics.",       ETC_status },
  { "TEST",	"Test [VERBOSE] Run diagnostics within etalk and exit.", ETC_test },
  { "USERS",    "Get a list of user structs",                 ETC_users },
  { "WAIT",     "Wait UID USER[@MACHINE[ TTY]] to wait for user to connect", ETC_wait },
  { "VERBOSE",  "Toggle verbosity.",                          ETC_verbose },
  { "VERSION",  "Version of etalk running.",                  ETC_version },
};

/*
 * Function: ETC_help
 *
 * Print out a help screen
 * 
 * Parameters: None
 *
 * History:
 * eml 4/19/94
 */
static int ETC_help(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  int i;

  printf("Available messages:\n");
  for(i = 0; i < (sizeof(Commands) / sizeof(struct command_associate)); i++)
    printf(" %-10s: %s\n", Commands[i].command, Commands[i].description);

  return Success;
}

/*
 * Function: ETC_c2str
 *
 * Locally defined function which takes a character and returns a
 * string.  The string will place ^ for control characters.
 *
 * Returns:     static char * - A local static string value.
 * Parameters:  c - the character to convert.
 *
 * History:
 * eml	Jul 12, 1994	Created
 */
static char *ETC_c2str(c)
     char c;
{
  static char cs[3] = { 0, 0, 0};

  if(c < ' ')
    {
      cs[0] = '^';
      cs[1] = c + '@';
      return cs;
    }
  else if(c == '')
    {
      cs[0] = '^';
      cs[1] = '?';
      return cs;
    }
  else
    {
      cs[0] = c;
      cs[1] = 0;
      return cs;
    }
}

/*
 * Function: ETC_scmp
 *
 * this compares s1 and s2 for len characters.  Case is ignored.
 * Success is returned when they are the same.
 *
 * Returns:     int - Success means they are the same
 * Parameters:  s1  - String 1
 *              s2  - String 2
 *              len - length of string
 * History:
 * eml	Jul 30, 1994	Created
 */
int ETC_scmp(s1, s2, len)
     char *s1, *s2;
     int   len;
{
  int i;
  for(i = 0; (i<len) && s1[i] && s2[i]; i++)
    {
      if((s1[i] >= 'a') && (s1[i] <= 'z'))
	s1[i] = s1[i] - 'a' + 'A';

      if((s2[i] >= 'a') && (s2[i] <= 'z'))
	s2[i] = s2[i] - 'a' + 'A';

      if(s1[i] != s2[i])	/* not equal */
	return Fail;
    }
  if(i == len)
    return Success;		/* all match */
  else
    return Fail;		/* to short */
}
/*
 * Function: CMD_parse_command
 *
 * Reads in a command line, and parses it, then does some appropriate action.
 * 
 * Parameters: Ctxt - context of the program
 *             dev - device to read from (should be stdio)
 * History:
 * eml 3/1/94
 */
void ETC_parse_command(Ctxt, dev)
     struct TalkContext *Ctxt;
     struct InputDevice *dev;
{
  char  buff[BUFSIZE];
  char *s;
  int   i;

  /*
   * There will always be a ^M or ^L so make sure it's nuked.
   */
  memset(buff, 0, sizeof(buff));
  ET_recv(dev, buff, BUFSIZE);

  s = buff;
  while(*s && (*s != ' ')) s++;

  /*
   * Now that the first command has been parsed, lets find the string
   * we have matched (case insensitive) and execute its function.
   */
  if(s-buff)
    for(i = 0; i < (sizeof(Commands) / sizeof(struct command_associate)); i++)
      {
	/*
	 * case insensitive comparison of beginning of string.  Allow
	 * shortest version of word, but not shortest unique.  Close enough
	 * for something to be interfaced into some other program.
	 */
	if(ETC_scmp(Commands[i].command, buff, s - buff) == Success)
	  {
	    if(!Commands[i].parse(Ctxt, ++s) && verbose)
	      printf("That command did not succeed.\n");
	    return;
	  }
      }
  printf("Invalid command %s\n", buff);
}


/*
 * Function: ETC_call
 *
 * Initiate a call using the BSD talk daemons.
 * 
 * Parameters: Ctxt - context
 *             cmd  - end of command line
 * History:
 * 4/18/93
 */
static int ETC_call(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  char *idstr;
  int   id;
  char *user;			/* user name             */
  char *host;			/* host name             */
  char *tty;			/* tty of user           */

  if(!Ctxt->myname)
    {
      printf("You must use the NAME command before making a call.\n");
      return Fail;
    }

  idstr = strtok(cmd, " ");
  user  = strtok(NULL, "!@");
  if(!idstr)
    {
      printf("Usage: CALL UID USER[[@]HOSTNAME[ TTY]]\n");
      return Fail;
    }
  id    = atoi(idstr);
  host  = strtok(NULL, " \t\n");
  if(host) {
    tty = strtok(NULL, " \t\n");
  }

  if(verbose)
    printf("\03Calling %s on %s id %d\n", user, host, id);

  PROTOCOL_attach(Ctxt, id, user, host, tty);

  return Success;
}


/*
 * Function: ETC_connect
 *
 * Complete a call by connecting to a known socket.
 * 
 * Parameters: Ctxt - context
 *             cmd  - end of command line
 * History:
 * 4/18/93
 */
static int ETC_connect(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  char *idstr;
  int   id;
  char *sockstr;
  int   sock;
  char *user;			/* user name             */
  char *host;			/* host name             */
  char *tty;			/* tty of user           */

  idstr = strtok(cmd, " ");
  sockstr = strtok(NULL, " ");
  user  = strtok(NULL, "!@");
  if(!idstr || !sockstr)
    {
      printf("Usage: CONNECT UID USER[[@]HOSTNAME[ TTY]]\n");
      return Fail;
    }
  id    = atoi(idstr);
  sock  = atoi(sockstr);
  host  = strtok(NULL, " \t\n");
  if(host) {
    tty = strtok(NULL, " \t\n");
  }

  if(verbose)
    printf("\03Connecting %s on socket %d host %s id %d\n",
	   user, sock, host, id);

  PROTOCOL_connect(Ctxt, id, sock, user, host, tty);

  return Success;
}

/*
 * Function: ETC_wait
 *
 * Wait for someone to connect to a know socket ID.
 * 
 * Parameters: Ctxt - context
 *             cmd  - end of command line
 * History:
 * 4/18/93
 */
static int ETC_wait(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  char *idstr;
  int   id;
  char *user;			/* user name             */
  char *host;			/* host name             */
  char *tty;			/* tty of user           */

  idstr = strtok(cmd, " ");
  user  = strtok(NULL, "!@");
  if(!idstr)
    {
      printf("Usage: WAIT UID USER[[@]HOSTNAME[ TTY]]\n");
      return Fail;
    }
  id    = atoi(idstr);
  host  = strtok(NULL, " \t\n");
  if(host) {
    tty = strtok(NULL, " \t\n");
  }

  /* This is non-verbose because nothing happens after this. */
  printf("Waiting for %s on host %s id %d\n",
	 user, host, id);

  PROTOCOL_wait(Ctxt, id, user, host, tty);

  return Success;
}

/*
 * Function: ETC_editc
 *
 * Read in three characters, (with a preceeding ^ indicating control)
 * and set the editcharacters for the system.  These characters are sent
 * first thing whenever a new connection is created.
 * 
 * Parameters: Ctxt - context
 *             cmd - tail of command line
 * History:
 * eml 4/14/94
 */
static int ETC_editc(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  char *s;
  int   curkey = 0;

  s = cmd;

  while(*s && (curkey < 3))
    {
      if(*s == '^')
	{
	  s++;
	  if(*s == '?')
	    Ctxt->editkeys[curkey] = 127; /* delete is special somehow. */
	  else
	    Ctxt->editkeys[curkey] = *s - '@'; /* convert to control */
	}
      else
	Ctxt->editkeys[curkey] = *s;
      s++;
      curkey++;
    }
  
  if(verbose)
    {
      printf("Edit characters set to Delchar(%s)",
	     ETC_c2str(Ctxt->editkeys[0]));
      printf(" DelLine(%s)",
	     ETC_c2str(Ctxt->editkeys[1]));
      printf(" DelWord(%s)\n", 
	     ETC_c2str(Ctxt->editkeys[2]));
    }
  return Success;
}

/*
 * Function: ETC_hangup
 *
 * Cleanly hangup on the userid specified on the command line
 * 
 * Parameters: Ctxt - context
 *             cmd - tail of command line
 * History:
 * eml 4/14/94
 */
static int ETC_hangup(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  int uid;
  uid = atoi(cmd);		/* grab the id number */

  if(uid == 0)
    {
      printf("HANGUP: id %s failed to parse to integer.\n", cmd);
      return Fail;
    }

  USER_hangup(Ctxt, uid);

  return Success;
}

/*
 * Function: ETC_client
 *
 * Locally defined function which sets USERS type to one of the
 * various client types.
 *
 * Parameters:  Ctxt - Context of program
 *              cmd  - command line
 * History:
 * eml	Jul 11, 1994	Created
 */
static int ETC_client(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  static char       *tp[] = { "DEFAULT", "ETALK", "YTALK" };
  char              *clientt;
  int                uid;
  struct UserObject *u;
  int                i;
  

  uid = atoi(strtok(cmd, " "));		/* grab the id number */

  u = USER_find(uid);

  if(! uid || ! u)
    {
      printf("No UID %d!\n", uid);
      printf("Usage: CLIENT UID <DEFAULT | ETALK | YTALK>\n");
      return Fail;
    }

  clientt = strtok(NULL, " \n\t");

  if(!clientt)
    {
      printf("Usage: CLIENT UID <DEFAULT | ETALK | YTALK>\n");
      return Fail;
    }

    for(i = 0; i < (sizeof(tp) / sizeof(char *)); i++)
      {	/*
	 * case insensitive comparison of beginning of string.  Allow
	 * shortest version of word, but not shortest unique.  Close enough
	 * for something to be interfaced into some other program.
	 */
	if(ETC_scmp(tp[i], clientt, strlen(clientt)) == Success)
	  {
	    /* Position in array is one less than the types in enum.
	     */
	    u->type = i+1;

	    if(verbose)
	      printf("Client of %s is set to %s\n", 
		     u->name?u->name:"unknown", tp[i]);

	    return Success;
	  }
      }
  
  printf("Usage: CLIENT UID <DEFAULT | ETALK | YTALK>\n");
  printf("       String \"%s\" is not known.\n", clientt);

  return Fail;
}


/*
 * Function: ETC_name
 *
 * Locally defined function which provides sets the user name, or the
 * name which is used to announce to other users
 *
 * Parameters:  Ctxt - Context
 *              cmd  - tail of command line
 * History:
 * eml	May 27, 1994	Created
 */
static int ETC_name(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  char *name;

  name = strtok(cmd, " ");

  name = (char *)malloc(strlen(name) + 1);

  strcpy(name, strtok(cmd, " "));

  if(Ctxt->myname) {
    if(verbose)
      printf("Replacing old name %s with ", Ctxt->myname);
    free(Ctxt->myname);
  } else {
    if(verbose)
      printf("Setting name to ");
  }

  Ctxt->myname = name;
  if(verbose)
    printf("%s\n", name);

  return Success;
}
/*
 * Function: ETC_clean
 *
 * Call a clean routine for users structures and devices to free up
 * any dead structures.  Mabee link to garbage collecting or something
 * equally goofy later.
 * 
 * Parameters: Ctxt - context
 *             cmd - tail of command line
 * History:
 * eml 4/15/94
 */
static int ETC_clean(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  /* shut off protocol things before closing sockets. ;)
   */
  ET_clean();			/* clean up devices. */
  USER_clean();			/* clean up users    */
  
  if(verbose)
    printf("Cleaning complete.\n");

  return Success;
}

/*
 * Function: ETC_abort
 *
 * Abort any currently active call outwards (waits or otherwise)
 * 
 * Parameters: Ctxt - context
 *             cmd - tail of command line
 * History:
 * eml 4/19/94
 */
static int ETC_abort(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  /* shut off any active call
   */
  PROTOCOL_abort(Ctxt);

  return Success;
}
/*
 * Function: ETC_users
 *
 * Print a list of all users.
 * 
 * Parameters: Ctxt - context
 *             cmd - tail of command line
 * History:
 * eml 4/14/94
 */

static int ETC_users(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  printf("\nList of user statistics:\n");
  USER_print();

  return Success;
}
/*
 * Function: ETC_host
 *
 * Print a list of all hosts.
 * 
 * Parameters: Ctxt - context
 *             cmd - tail of command line
 * History:
 * eml 4/21/94
 */
static int ETC_host(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  printf("\nList of host statistics:\n");
  HOST_print();

  return Success;
}

/*
 * Function: ETC_status
 *
 * Locally defined function which displays compile time, and run time
 * statistics about the program.
 *
 * Parameters:  Ctxt - Context of program
 *              cmd  - command line
 * History:
 * eml	Jul 11, 1994	Created
 */
static int ETC_status(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  printf("Compile time parameters:\n");
#if TALKDTEST == 1
  printf("TALKDTEST  : TRUE\n");
#else
  printf("TALKDTEST  : FALSE\n");
#endif

#if OTALK_ONLY == 1
  printf("OTALK_ONLY : TRUE\n");
#else
  printf("OTALK_ONLY : FALSE\n");
#endif

#ifdef DEBUG_2
  printf("DEBUG_2    : TRUE\n");
#else
  printf("DEBUG_2    : FALSE\n");
#endif

#ifdef SYSTEM_RC
  printf("SYSTEM_RC  : %s\n", SYSTEM_RC);
#else
  printf("SYSTEM_RC  : NONE\n");
#endif
  
#ifdef LOCAL_RC
  printf("LOCAL_RC   : %s\n", LOCAL_RC);
#else
  printf("LOCAL_RC   : NONE\n");
#endif
  
  printf("Run time parameters:\n");
  printf("Local Name : %s\n", Ctxt->myname?Ctxt->myname:"Undefined");
  printf("Verbosity  : %d\n", verbose);
  printf("Editkeys   : CHAR(%s)",
	 ETC_c2str(Ctxt->editkeys[0]));
  printf(" LINE(%s)",
	 ETC_c2str(Ctxt->editkeys[1]));
  printf(" WORD(%s)\n",
	 ETC_c2str(Ctxt->editkeys[2]));
  printf("PID        : %ld\n", Ctxt->pid);
  printf("Msg Delay  : %d (unused)\n", Ctxt->message_delay);

  return Success;
}

/*
 * Function: ETC_device
 *
 * Print out a list of all devices.
 * 
 * Parameters: Ctxt - context
 *             cmd - tail of command line
 * History:
 * eml 4/15/94
 */

static int ETC_device(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  printf("\nList of device statistics:\n");
  ET_print_q_list();

  return Success;
}
/*
 * Function: ETC_quit
 *
 * Quit etalk binary
 * 
 * Parameters: Ctxt - context
 *             cmd - tail of command line
 * History:
 * eml 3/1/94
 */
static int ETC_quit(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  /* shut off protocol things before closing sockets. ;)
   */
  PROTOCOL_delete_all(Ctxt, TRUE);
  ET_close_all(Ctxt);
  printf("Clean shutdown complete...\n");
  exit(0);
}

/*
 * Function: ETC_read
 *
 * Locally defined function which re-reads the system RC files for
 * hosts and their daemon types.
 *
 * Parameters:  Ctxt - Context of talk program
 *              cmd  - command line
 * History:
 * eml	Jul 11, 1994	Created
 */
static int ETC_read(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  RC_load_all_hosts();

  return Success;
}

/*
 * Function: ETC_test
 *
 * Locally defined function which tests the active etalk system as
 * best it can.
 *
 * Parameters:  Ctxt - Context
 *              cmd  -  tail of command line
 * History:
 * eml	May 27, 1994	Created
 * eml  Aug 10, 1994    Returns error code on failure.
 */
static int ETC_test(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  if((strlen(cmd) > 0) &&
     (ETC_scmp(cmd, "VERBOSE", strlen(cmd)) == Success))
    {
      verbose = 0;
      ETC_verbose(Ctxt, "");
      ETC_status(Ctxt, "");
    }

  if(PROTOCOL_test(Ctxt) == Fail)
    {
      /* Return error code for makefile */
      exit(1);
    }
  else
    {
      /* Quit peacefully */
      ETC_quit(Ctxt, cmd);
    }

  /* If we get this far, something bad happened! */
  return Fail;
}

/*
 * Function: ETC_verbose
 *
 * Locally defined function which toggles the verbosity variable.
 *
 * Parameters:  Ctxt - Context of talk program
 *              cmd  - the command line
 * History:
 * eml	Jul 11, 1994	Created
 */
static int ETC_verbose(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  /* Toggle the verbosity
   */
  verbose = !verbose;

  if(verbose)
    printf("Verbosity is now set.\n");
  else
    printf("Verbosity is now off.\n");

  return Success;
}

/*
 * Function: ETC_version
 *
 * Locally defined function which prints out the ETALK version string
 *
 * Parameters:  Ctxt - Context of the program
 *              cmd  - the rest command line
 * History:
 * eml	Jul 11, 1994	Created
 */
static int ETC_version(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  /* print out the current version string
   */
  printf("%s\n", ETALK_);

  return Success;
}
/*
 * Echo back the command line.
 */
static int ETC_echo(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  printf("[%s]\n", cmd);
  return Success;
}

#ifdef TALKDTEST
/*
 * Function: ETC_delete, ETC_leave, ETC_lookup, ETC_announce
 * 
 * Each of these function test one section of the talk protocol by
 * sending one message to the destined talk daemon, and printing the
 * response message to the minibuffer.
 * 
 * Parameters: Ctxt - conext
 *             cmd - tail of the command line.
 * History:
 * eml 3/18/94
 */
static int ETC_delete(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  char *id;			/* id type name          */
  char *host;			/* host name             */
  enum DMN_deletion_type t;	/* deletion type         */
  struct InputDevice *io;	/* io of thier daemon    */

  if(!Ctxt->myname)
    {
      printf("You must use the NAME command before deleting things.\n");
      return;
    }

  id = strtok(cmd, " ");
  host = strtok(NULL, " \t\n");

  if(!id) {
    id = cmd;
  }

  if(ETC_scmp(id, "INVITE", strlen(id)) == Success)
    {
      t = DMN_invite;
    } 
  else if(ETC_scmp(id, "ANNOUNCE", strlen(id)) == Success)
    {
      t = DMN_announce;
    }
  else
    {
      printf("usage: DELETE <ANNOUNCE | INVITE> [hostname]\n");
      return Fail;
    }

  printf("Deleting id %s from %s\n", id, host);

  /*
   * If no host, then make sure that we use local host 127.0.0
   */
  if(host) {
    io = UDP_host(host);
    if(io == NULL) return Fail;
  }
  else
    io = Ctxt->local_daemon;

  printf("IO device addr: ");
  print_sockaddr((struct sockaddr *)&io->raddr);
  printf("\n");

  if(! DMN_Delete(Ctxt, io, t))
    {
      printf("Error, continuation action not taken.\n");
      return Fail;
    }

  /* Info is printed already if verbosity is on.
   */
  if(! verbose)
    ET_control_print(io);

  io->readme = (void *)DMN_get_and_display;

  return Success;
}
static int ETC_leave(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  char *user;			/* user name             */
  char *host;			/* host name             */
  char *tty;			/* tty of user           */
  struct InputDevice *io;	/* io of thier daemon    */

  if(!Ctxt->myname)
    {
      printf("You must use the NAME command before leaving an invite.\n");
      return Fail;
    }

  user = strtok(cmd, "!@");
  host = strtok(NULL, " \t\n");
  if(!host) {
    user  = strtok(cmd, " ");
  }
  tty = strtok(NULL, " \t\n");

  if(!tty) tty = "";

  if(!user) {
    printf("usage: LEAVE user[[@]hostname][ TTY]\n");
    return Fail;
  }

  printf("leaving an invite for %s on %s\n", user, host);

  /*
   * If no host, then make sure that we use local host 127.0.0
   */
  if(host) {
    io = UDP_host(host);
    if(io == NULL) return Fail;
  }
  else
    io = Ctxt->local_daemon;

  printf("IO device addr: ");
  print_sockaddr((struct sockaddr *)&io->raddr);
  printf("\n");

  if(! DMN_LeaveInvite(Ctxt, user, tty))
    {
      printf("Error, continuation action not taken.\n");
      return Fail;
    }

  if(! verbose)
    ET_control_print(io);

  io->readme = (void *)DMN_get_and_display;

  return Success;
}
static int ETC_lookup(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  char *user;			/* user name             */
  char *host;			/* host name             */
  char *tty;			/* tty of user           */
  struct InputDevice *io;	/* io of thier daemon    */

  if(!Ctxt->myname)
    {
      printf("You must use the NAME command before requesting lookup.\n");
      return Fail;
    }

  user = strtok(cmd, "!@");
  host = strtok(NULL, " \t\n");
  if(!host) {
    user = strtok(cmd, " ");
  }
  tty = strtok(NULL, " \t\n");

  if(!tty) tty = "";

  if(!user) {
    printf("usage: LOOKUP user[[@]hostname][ TTY]\n");
    return Fail;
  }

  printf("Looking for invite from %s on %s\n", user, host);

  /*
   * If no host, then make sure that we use local host 127.0.0
   */
  if(host) {
    io = UDP_host(host);
    if(io == NULL) return Fail;
  }
  else
    io = Ctxt->local_daemon;

  printf("IO device addr: ");
  print_sockaddr((struct sockaddr *)&io->raddr);
  printf("\n");

  if(! DMN_Lookup(Ctxt, io, user, tty))
    {
      printf("Error, continuation action not taken.\n");
      return Fail;
    }

  if(! verbose)
    ET_control_print(io);

  io->readme = (void *)DMN_get_and_display;

  return Success;
}
static int ETC_announce(Ctxt, cmd)
     struct TalkContext *Ctxt;
     char               *cmd;
{
  char *user;			/* user name             */
  char *host;			/* host name             */
  char *tty;			/* tty of user           */
  struct InputDevice *io;	/* io of thier daemon    */

  if(!Ctxt->myname)
    {
      printf("You must use the NAME command before making announcing.\n");
      return Fail;
    }

  user = strtok(cmd, "!@");
  host = strtok(NULL, " \t\n");
  if(!host) {
    user = strtok(cmd, " ");
  }
  tty = strtok(NULL, " \t\n");

  if(!tty) tty = "";

  if(!user) {
    printf("usage: ANNOUNCE user[[@]hostname][ TTY]\n");
    return Fail;
  }

  printf("Announcing to %s on %s\n", user, host);

  /*
   * If no host, then make sure that we use local host 127.0.0
   */
  if(host) {
    io = UDP_host(host);
    if(io == NULL) return Fail;
  }
  else
    io = Ctxt->local_daemon;

  printf("IO device addr: ");
  print_sockaddr((struct sockaddr *)&io->raddr);
  printf("\n");

  if(! DMN_Announce(Ctxt, io, user, tty))
    {
      printf("Error, continuation action not taken.\n");
      return Fail;
    }

  if(! verbose)
    ET_control_print(io);

  io->readme = (void *)DMN_get_and_display;

  return Success;
}
#endif /* TALKDTEST */
