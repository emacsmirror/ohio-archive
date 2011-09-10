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
 * The structures in this program are almost identical to those found
 * in the BSD include file protocol/talkd.h.  This is to provide
 * compatibility with older talk protocols.  Please see the comment
 * after this one from talk.h (included with this distribution) to see
 * thier copyright information.
 *
 * gtalk.h
 *
 * Purpose:
 *   These definitions will be my replacement for ntalk and otalk
 * internally.  The conversion from otalk and ntalk ctr structures
 * will ease the conflicts when passing all the gobbldy gook about.
 * Someday this structure will define some form of gtalk daemon
 * structure which will have greater flexibility. 
 *
 *   The purpose of some fields, when going to V2 may change when/if I
 * get around to writing my talk daemon.
 */

#define GNAME_SIZE 12
#define GTTY_SIZE 16

/* The following comment is the copyright pertaining to Berkeley's    
 * format for the talk daemon.  This is in reference to the structure 
 * of the messages only, (which match those found in talk.h included  
 * with this file.
 */
/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

typedef struct GNU_control_message {
  u_char          vers;		       /* talk protocol version    */
  u_char          type;		       /* request type             */
  u_char          answer;	       /*  */
  u_char          pad;		       /*  */
#if SIZEOF_LONG == 4
  u_long          id_num;	       /* message id number (dels) */
#else
  u_int           id_num;
#endif /* sizeof_long == 4 */
  struct sockaddr addr;		       /* target address           */
  struct sockaddr ctl_addr;	       /* reply to address         */
  u_long          pid;		       /* caller's process id      */
  char            l_name[GNAME_SIZE];  /* caller's name            */
  char            r_name[GNAME_SIZE];  /* callee's name            */
  char            r_tty[GTTY_SIZE];    /* callee's tty             */
} CTL_MSG_GNU;

typedef struct GNU_response_message {
  u_char           vers;	       /* protocol version         */
  u_char           type;	       /* type of request message  */
  u_char           answer;	       /* response to request      */
  u_char           pad;		       /* padding                  */
#if SIZEOF_LONG == 4
  u_long           id_num;	       /* message id number        */
#else
  u_int            id_num;
#endif
  struct  sockaddr addr;	       /* address for connection   */
} CTL_RESPONSE_GNU;

#define TALK_VERSION_GNU 2	       /* GNU protocol version number */

/* additional message type values, others from talk.h                   */
#define REPLY_QUERY    4	       /* request reply data from local */
				       /* daemon                        */
/* additional answer values */
#define NO_CALLER      9	       /* no-one calling answer from REPLY   */

/* additional time constants we would like to use.                           */
#define UDP_LIFE       5	       /* seconds to wait before retransmit  */
#define UDP_RETRY      2               /* Only retry X time before giving up */
