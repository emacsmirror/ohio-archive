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
 */
#ifndef ETALKLIB_

#define ETALKLIB_ "ETALK library routines 0.8B (c) 1994 Free Software Foundation"

#include "config.h"

/*
 * First, lets make some of those standard define we all love
 * to use.
 */
#define TRUE 1
#define Success 1
#define FALSE 0
#define Fail 0

/*
 * Define an object which will be the input interface for
 * multiple sets of ports and types of input devices.
 */
enum InputDeviceType { IO_TTY = 0, IO_TCP = 1, IO_UDP = 2 };

enum InputState { EMPTY = 0, CONNECTED = 1, LISTENING = 2, WAITING = 3,
		    IDLE = 4, DEAD = 5 };

enum DMN_deletion_type { DMN_invite, DMN_announce };

struct InputDevice {
  enum InputDeviceType type;	/* type of IO port                   */
  enum InputState      state;	/* current state                     */
  char                *name;	/* logical name to keep trak of this */
  struct sockaddr_in   raddr;	/* socket address of remote /listing */
  int                  fd;	/* file descriptor                   */
  struct HostObject   *host;	/* host we are attached to           */
  long                 sendc;	/* number of send char/units         */
  long                 recvc;	/* number of recieve char/units      */
  void               (*readme)();/* what to do when there is input   */
  long                 timeout;	/* when to timeout (global seconds)  */
  void               (*timefn)(); /* what to do when there is no input */
  void               (*timemsg)(); /* msg to print when there is no input */
				
  struct InputDevice  *next;	/* closure                           */
  struct InputDevice  *prev;	/* closure                           */
};

/*
 * Describe hosts structures.  Host structures are only used
 * when attempting to connect to a hosts TALKD, and that is 
 * what all states represent.
 */
				/* GTALKD is stub for later */
enum DaemonType { OTALKD = 0, NTALKD = 1, GTALKD = 2 };
enum RCFileType { RC_system, RC_local, RC_new, RC_changed };

struct HostObject {
  char               *name;	/* official name of the machine     */
  enum DaemonType     type;	/* type of the daemon               */
  enum RCFileType     rc;	/* where was daemon info from       */
  struct sockaddr_in  addr;	/* host address structure.          */
  int                 addr_len;	/* length of address from host ent  */
  struct HostObject  *next;	/* closure                          */
  struct HostObject  *prev;	/* closure                          */
};

/*
 * The following defines are the MESSAGETYPE sent to the emacs
 * over the TTY.  Any character not listed (ie, A-z) means a message
 * to print in the minibuffer.
 */
/* socket number of emacs_connect */
#define TTY_LOCALPORT 0x01
/* socket number of remote_connect */
#define TTY_REMOTEPORT 0x02
/* user id of new user struct */
#define TTY_USERID 0x03
/* id of user struct just turned off. */
#define TTY_DELETED 0x04
/* a minibuff message which should not be logged. */
#define TTY_NOLOG 0x05

/* There needs to be a verbose variable defined in main.
 */
extern int verbose;

/*
 * Now lets advertise some function prototypes used in
 * controlling objects.
 */
#ifdef PROTOTYPES

/* etl_dev.c */
extern struct InputDevice *ET_gen_iodev(enum InputDeviceType type, int fd,
					struct sockaddr_in  *raddr);
extern struct sockaddr_in ET_portable_address(struct InputDevice *dev);
extern int ET_select_all(void *Ctxt, struct InputDevice *reton);
extern void ET_close_all(void *Ctxt);
extern struct InputDevice *ET_tty();
extern int ET_send(struct InputDevice *dev, void *buffer, int size);
extern int ET_recv(struct InputDevice *dev, void *buffer, int size);
void print_sockaddr(struct sockaddr *addr);
extern char *ET_dev_name(struct InputDevice *io);
extern void ET_end_recursion();
extern void ET_print_q_list();
extern void ET_clean_dev(struct InputDevice *io);
extern void ET_clean();
extern void print_swapped_sockaddr(struct sockaddr *addr);
extern void ET_print_device(struct InputDevice *io);

/* etl_tcp.c */
extern struct InputDevice *TCP_listen();
extern struct InputDevice *TCP_connect(struct sockaddr *addr);
extern struct InputDevice *TCP_accept(struct InputDevice *listening);

/* etl_udp.c */
extern struct sockaddr_in *UDP_receive_port();
extern void                UDP_setup_localport();
extern struct InputDevice *UDP_host(char *machine);
extern struct InputDevice *UDP_udp_servent(const char *servent_name );
extern int UDP_daemon_change( struct InputDevice *io );

/* etl_host.c */
extern struct HostObject *HOST_gen_local_host();
extern struct HostObject *HOST_gen_host( char *machine );
extern struct HostObject *HOST_gen_host_by_addr( struct sockaddr *addr, int size );
extern void HOST_print();

#else

/* etl_dev.c */
extern struct InputDevice *ET_gen_iodev();
extern struct sockaddr_in ET_portable_address();
extern int ET_select_all();
extern void ET_close_all();
extern struct InputDevice *ET_tty();
extern int ET_send();
extern int ET_recv();
extern void print_sockaddr();
extern char *ET_dev_name();
extern void ET_end_recursion();
extern void ET_print_q_list();
extern void ET_clean_dev();
extern void ET_clean();
extern void print_swapped_sockaddr();
extern void ET_print_device();

/* etl_tcp.c */
extern struct InputDevice *TCP_listen();
extern struct InputDevice *TCP_connect();
extern struct InputDevice *TCP_accept();

/* etl_udp.c */
extern struct sockaddr_in *UDP_receive_port();
extern void                UDP_setup_localport();
extern struct InputDevice *UDP_host();
extern struct InputDevice *UDP_udp_servent();
extern int UDP_daemon_change();

/* etl_host.c */
extern struct HostObject *HOST_gen_local_host();
extern struct HostObject *HOST_gen_host();
extern struct HostObject *HOST_gen_host_by_addr();
extern void HOST_print();



#endif

#endif /* etalklib_ */
