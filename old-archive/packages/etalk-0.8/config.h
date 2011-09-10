/* 
 *  Copyright (C) 1994 Free Software Foundation
 * 
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 * 
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 * 
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, you can either send email to this
 *  program's author (see below) or write to:
 * 
 *               The Free Software Foundation, Inc.
 *               675 Mass Ave.
 *               Cambridge, MA 02139, USA. 
 * 
 *  Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
 * 
 * Configuration variables to for setting up etalk to be compiled
 */
#ifndef CONFIG_H
#define CONFIG_H
/*
 * SYSTEM_RC is a string representing where the system wide etalk_rc file
 * will reside.  Not defining this will mean there will never be a system
 * RC file.  Defineing it as "/etc/hosts.talk" is safe, and there is no
 * need for this file to exist.
 */
#ifndef SYSTEM_RC
#define SYSTEM_RC "/etc/hosts.talk"
#endif

/* LOCAL_RC is a string representing the local users rc file.  This should
 * always be ".hosts.talk" since that is what the documentation points to.
 * Internally, start at the root of the user directory.
 */
#ifndef LOCAL_RC
#define LOCAL_RC ".hosts.talk"
#endif

/* The following lists of sizes are defined as a checkup.  The headers
 * otalk.h, talk.h, and gtalk.h must declair the messages to be these
 * sizes, or conflicts will occur, and the binary will not initialize.
 *
 * DO NOT OVERRIDE!!  These are static on all systems.
 */
#define OTALK_CM_SIZE 76
#define OTALK_CMR_SIZE 24
#define NTALK_CM_SIZE 84
#define NTALK_CMR_SIZE 24
#define GTALK_CM_SIZE 84
#define GTALK_CMR_SIZE 24

/*
 * Define PROTOTYPES if your system can handle C prototypes of the form
 * type functioname(int parameter);
 * Leaving it uncommented simply reduces some of the debugging available.
 */
/* #define PROTOTYPES */

/* 
 * Define DEBUG_2 if you want tons o debug messages.
#define DEBUG_2
 */


/* The following includes are the defaults needed to do most things
 * within the etalk binary.  This gives me just one place where I must
 * do all this ick stuff.
 */

/* malloc, free, atoi : malloc is cast, and unknowns default to return  */
/*                      int, so there is no need to add those defs here */
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

/* printf, FILE etc: What do you do without this stuff? */
#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

/* This from autoconf docs.
 */
#if STDC_HEADERS || HAVE_STRING_H
#include <string.h>
/* An ANSI string.h and pre-ANSI memory.h might conflict.  */
#if !STDC_HEADERS && HAVE_MEMORY_H
#include <memory.h>
#endif /* not STDC_HEADERS and HAVE_MEMORY_H */
#define bcopy(s, d, n) memcpy ((d), (s), (n))
#define bzero(s, n) memset ((s), 0, (n))
#else /* not STDC_HEADERS and not HAVE_STRING_H */
#include <strings.h>
/* memory.h and strings.h conflict on some systems.  */
#endif /* not STDC_HEADERS and not HAVE_STRING_H */

#if HAVE_SYS_TYPES_H == 1
# include <sys/types.h>
#else
typedef unsigned char u_char;
typedef unsigned short u_short;
typedef unsigned int u_int;
typedef unsigned long u_long;
#endif

#if HAVE_SYS_TIME_H
# include <sys/time.h>
#else
# if HAVE_TIME_H
#  include <time.h>
# endif /* TIME_H */
#endif /* SYS_TIME_H */

#if HAVE_UNISTD_H == 1
#include <unistd.h>
#else
/* If we don't have any of these, we must devine timeval ourselves. */
#if ! defined HAVE_TIME_H && ! defined HAVE_SYS_TIME_H
struct timeval {
  int tv_sec;
  int tv_usec;
};
#endif /* not TIME_H and not SYS_TIME_H */

/* Select.h is an aix thing, and includes the FD_* routines needed
 * to do selecting on that system
 */
#ifdef HAVE_SELECT_H
#include <select.h>
#endif /*SELECT_H */

#endif /* UNISTD_H */

/* If you don't have these, or the equivalent thereof, then you can't */
/* run this program, therefore, no checks are going to be made        */
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#endif /* CONFIG_H */
