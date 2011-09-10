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

#ifndef ETALK_

#define ETALK_ "ETALK version 0.8 (c) 1994 Free Software Foundation"

#define NUMEDITKEYS 3

/*
 * lets define the context used thoughout
 * this talk program.
 */
struct TalkContext {
  struct HostObject  *me;		/* The current host.            */
  struct InputDevice *tty;		/* TTY to emacs                 */
  struct InputDevice *emacs_connect;	/* emacs network port           */
  struct InputDevice *remote_connect;	/* remote connect port          */
  struct InputDevice *udp_port;		/* my udp to daemon port        */
  struct InputDevice *local_daemon;	/* The local daemons address    */
  char                editkeys[3];      /* My editkeys                  */
  pid_t               pid;		/* pid, (autoconf makes pid_t)  */
  char               *myname;		/* my name                      */
  int                 message_delay;	/* delay between goofy messages */
};

/*
 * Describe a USER.  USERS represent different connections to
 * multiple talk programs.
 */
enum TalkClientType { Unknowntalk, VanillaTalk, ETALK, YTALK };
enum TalkUserState  { USER_NEW = 0, USER_CONNECTED = 1, USER_CLOSED = 2 };

struct UserObject {
  char               *name;	/* name of user                 */
  char                editkey[3]; /* thier edit keys            */
  int                 id;	/* unique user id.              */
  enum TalkClientType type;	/* thier client type            */
  enum TalkUserState  state;	/* state of this structure      */
  struct InputDevice *remote;	/* thier TCP io device          */
  struct InputDevice *local;	/* thier device to emacs window */
  struct UserObject  *next;	/* closure                      */
  struct UserObject  *prev;
};

#ifdef PROTOTYPES

/* et_cmd.c */
extern void ETC_parse_command(struct TalkContext *Ctxt, struct InputDevice *dev);

/* et_daemn.c */
extern void ET_control_print(struct InputDevice *io);
extern int DMN_check_compile();
extern int DMN_Lookup(struct TalkContext *Ctxt, struct InputDevice *io,
		      char *r_user, char *r_tty);
extern struct sockaddr_in *DMN_get_lookup_response(struct InputDevice *io);
extern int DMN_Announce(struct TalkContext *Ctxt, struct InputDevice *io,
		 char *r_user, char *r_tty);
extern void DMN_get_and_display(struct TalkContext *Ctxt,
				struct InputDevice *io);
extern int DMN_Delete(struct TalkContext *Ctxt, struct InputDevice *io,
		      enum DMN_deletion_type type);
extern int DMN_LeaveInvite(struct TalkContext *Ctxt, char *r_user, char *r_tty);
extern int DMN_get_invite_response(struct InputDevice *io);
extern int DMN_get_announce_response(struct InputDevice *io);
extern int DMN_get_delete_response(struct InputDevice *io);
extern char *DMN_last_response(struct InputDevice *io);
extern int DMN_last_response_numeric(struct InputDevice *io);
extern int DMN_last_response_version(struct InputDevice *io);
extern void ET_reset_ids();

/* et_proto.c */
extern void PROTOCOL_attach(struct TalkContext *Ctxt, int userid,
		     char *user, char *node, char *tty);
extern void PROTOCOL_delete_all(struct TalkContext *Ctxt, int terminate);
extern void PROTOCOL_connect(struct TalkContext *Ctxt, int userid, int sock,
		      char *user, char *node, char *tty);
extern void PROTOCOL_wait(struct TalkContext *Ctxt, int userid,
		   char *user, char *node, char *tty);
extern void PROTOCOL_abort(struct TalkContext *Ctxt);
extern int PROTOCOL_test(struct TalkContext *Ctxt);

/* et_strm.c */
extern void STREAM_local_read(struct TalkContext *Ctxt, struct InputDevice *io);
extern void STREAM_remote_read(struct TalkContext *Ctxt, struct InputDevice *io);

/* et_local.c */
extern void LOCAL_new_tcp(struct TalkContext *Ctxt, struct InputDevice *io);

/* et_user.c */
extern struct UserObject *USER_alloc();
extern struct UserObject *USER_find(int id);
extern struct UserObject *USER_iofind(struct InputDevice *io);
extern void USER_print();
extern void USER_hangup(struct TalkContext *Ctxt, int id);
extern void USER_clean();

/* et_rc.c */
extern void RC_load_all_hosts();

#else /* PROTOTYPES */

/* et_cmd.c */
extern void ETC_parse_command();

/* et_daemn.c */
extern void ET_control_print();
extern int DMN_check_compile();
extern int DMN_Lookup();
extern struct sockaddr_in *DMN_get_lookup_response();
extern int DMN_Announce();
extern void DMN_get_and_display();
extern int DMN_Delete();
extern int DMN_LeaveInvite();
extern int DMN_get_invite_response();
extern int DMN_get_announce_response();
extern int DMN_get_delete_response();
extern char *DMN_last_response();
extern int DMN_last_response_numeric();
extern int DMN_last_response_version();
extern void ET_reset_ids();

/* et_proto.c */
extern void PROTOCOL_attach();
extern void PROTOCOL_delete_all();
extern void PROTOCOL_connect();
extern void PROTOCOL_wait();
extern void PROTOCOL_abort();

/* et_strm.c */
extern void STREAM_local_read();
extern void STREAM_remote_read();

/* et_local.c */
extern void LOCAL_new_tcp();

/* et_user.c */
extern struct UserObject *USER_alloc();
extern struct UserObject *USER_find();
extern struct UserObject *USER_iofind();
extern void USER_print();
extern void USER_hangup();
extern void USER_clean();

/* et_rc.c */
extern void RC_load_all_hosts();

#endif /* PROTOTYPES */


#endif /* ETALK_ */
