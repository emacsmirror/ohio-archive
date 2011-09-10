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
 * etalk.c
 *
 * Purpose:
 *  This file contains the startup code for initializing the udp and
 * connection sockets.  It then starts up main_loop which waits on all
 * registered file descriptors.
 *
 * General Instructions:
 * To Build:
 *    If there is a "configure" script, type configure
 *    then type make
 * For use with emacs:
 *    Set the elisp directory as your default (dired) and load in the
 *    lisp file etalkcomp.el.  This will automatically byte-compile
 *    the lisp files with the correct defaults.  Next, make sure the
 *    etalk lisp directory is in your load-path (see file autoloads)
 *    Then do a M-x etalk RET username@machine RET
 * To use with your own talk program:
 *    pipe-exec this binary with no parameters.
 *    Wait for the string ("\03%c1111\n", TTY_LOCALPORT)
 *    Wait for the string ("\03%c1111\n", TTY_REMOTEPORT)
 *    Ignore all other strings not starting with "\03" always
 *    --> TTY_*PORT are defined in etalklib, and 1111 is an ascii number
 *    Create a TCP connection to the LOCALPORT received.
 *    Wait for the string ("\03%c1\n", TTY_USERID)
 *    --> TTY_USERID is defined in etalklib, and 1 is an ascii number
 *    representing a USERSTRUCT.  ie.  some connection.
 *    Send the string EDITCHARS ^?^U^W with ^ and ? being seperate
 *    characteres.   This sets the edit characters.
 *    Send the string NAME zappo where zappo is the username of the
 *    person using the program. (Used in announces.)
 *    Send string "CALL id user[@machine][ tty]" to the etalk subprocess.
 *    Display all strings starting with ("\03%cstuf\n", TTY_NOLOG) as a message
 *    Any output to the TTY re-rings until a connection is made.
 *    When EDITCHARS are recieved, on TCP_LOCAL the connection is established
 *    If you support some sub/super set of the extensions, send the
 *    string "\03\010NAME major.minor\n" where \010 is 8 and where
 *    name is the name of your interface, and major.minor are the 0.8
 *    version of your program.  See elisp for other message codes.  
 *
 * History:
 * eml 8/17/94
 * Added call to check for sizes of net messages.  Will serv as a
 * preliminary check to make everything is kool.
 */
#include "etalklib.h"
#include "etalk.h"

int verbose = 0;		/* verbosity variable */

int main(argc, argv)
     int   argc;
     char *argv[];
{
  struct TalkContext Ctxt;

  printf("%s\n", ETALK_);

  Ctxt.myname        = NULL;
  Ctxt.message_delay = 30;

  if(DMN_check_compile() == Fail)
    /* Return error status on this check. */
    return 1;

  RC_load_all_hosts();

  printf("Initializing Network Interface.\n");

  UDP_setup_localport();

  Ctxt.me             = HOST_gen_local_host();
  /* We must set readme by hand here because the device library does not
   * not provide the reader.
   */
  Ctxt.tty            = ET_tty();
  Ctxt.tty->readme    = ETC_parse_command;

  Ctxt.emacs_connect  = TCP_listen(); /* we must set host by hand */
  Ctxt.emacs_connect->name = "emacs_connect";
  Ctxt.emacs_connect->host = Ctxt.me;
  Ctxt.emacs_connect->readme = LOCAL_new_tcp;
  Ctxt.emacs_connect->timeout = 0;

  printf("\03%c%d\n", TTY_LOCALPORT,
	 ntohs(Ctxt.emacs_connect->raddr.sin_port));

  Ctxt.remote_connect = TCP_listen();
  Ctxt.remote_connect->name = "remote_connect";
  Ctxt.remote_connect->host = Ctxt.me;
  printf("\03%c%d\n", TTY_REMOTEPORT,
	 ntohs(Ctxt.remote_connect->raddr.sin_port));

  Ctxt.udp_port           = UDP_host(Ctxt.me->name);
  Ctxt.udp_port->name     = "udp_port";
  Ctxt.local_daemon       = UDP_host(Ctxt.me->name);
  Ctxt.local_daemon->name = "local_daemon";

  Ctxt.editkeys[0] = '';
  Ctxt.editkeys[1] = '';
  Ctxt.editkeys[2] = '';

  Ctxt.pid            = getpid();

  printf("Initialization complete.\n");

  /* Select loop.  This should be the only occurance with NULL as the
   * reton parameter...
   */
  ET_select_all(&Ctxt, NULL);

  return 1;
}

