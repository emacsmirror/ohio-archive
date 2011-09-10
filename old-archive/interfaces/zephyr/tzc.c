/*
   tzc.c  trivial zephyr client (once upon a time)
   Copyright (C) 1992, 1993 Scott Draves (spot@cs.cmu.edu)

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   Now at version 2.0.  Somewhat (he made me say that) incompatible with
   previous versions.

   this program is a replacement for zwgc and zwrite that talks to emacs
   in printed sexps.  you can get the whole thing from
   hopeless.mess.cs.cmu.edu:/usr/spot/pub/zephyr.tar.Z


   CHANGES

   Mon Jan 18  received -a option from hsw@cs.cmu.edu, it restarts the
               process if no messages have been received after arg seconds.
   Wed Jan  6  added p: to options string, i forgot it in the merge
   Fri Jan  1  replace ((tzcspew . sent) (to "spot"))
               with    ((tzcspew . sent) (to "PERSONAL" . "spot"))
   Sun Dec 20  removed silly old code that was forcing recip to be "" for
               non-personal instances.
   sometime    recognizes (auth . nil) to disable kerberos authentication
   Wed Dec 16  added some more error checking to send_zgram so that it
               handles garbled input without dying.
   Sun Dec 13: merged in lisp reading stuff.  no more calls to zwrite.
   Sun Oct 25: if we get a USR1 signal, then restart ourselves. added -p
               flag to write our pid into a file. this is so that kauthd
	       users can restart tzc automatically when they get new
	       tickets.
   Tue Jul 28: added wait() to subscribe_with_zctl() to prevent zombies.

   TODO

   if you get a USR1 in the middle of writing out a msg, then the output
   will not be properly formatted.  so should either disable the sig for
   this time, or set a flag in the handler and check at the main loop.  the
   same probably applies to the alarm.

   write a man page
   
  */

#include <strings.h>
#include <stdio.h>
#include <sys/types.h>
#include <ctype.h>
#include <zephyr/zephyr.h>
#include <netdb.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <signal.h>
#include "lread.h"

#define ZCTL_BINARY "/usr/misc/.zephyr/bin/zctl"

typedef struct PendingReply PendingReply;
struct PendingReply {
   char *instance;
   char *recipient;
   ZUnique_Id_t	uid;
   PendingReply *next;
};

struct Globals {
   char		*program;
   int          argc;
   char         **argv;

   u_short	port;
   int		stdin_flags;
   int		zfd;

   int		ebufsiz;
   char *	ebuf;
   char *	ebufptr;

   /* linked list of messages sent which are waiting for replies */
   PendingReply *pending_replies;

   struct {
      Value *sym_class;
      Value *sym_instance;
      Value *sym_recipients;
      Value *sym_message;
      Value *sym_send;
      Value *sym_tzcfodder;
      Value *sym_auth;
   } constants;
};

struct Globals global_storage, *globals;

void usage() {
   fprintf(stderr, "usage: %s [-s] [-e exposure] [-p filename] [-a nseconds]\n",
	   globals->program);
}

/* I'm paranoid about leaving stdin non-blocking, though I doubt it's a */
/* problem in this case. - nix */
void bail(int code) {
   if (-1 == fcntl(0, F_SETFL, globals->stdin_flags)) {
      perror("unable to reset stdin");
      code = 1;
   }
   exit(code);
}

Code_t check(Code_t e, char *s) {
   if (e) {
      com_err(__FILE__, e, s);
      bail(1);
   }
   return e;
}

Code_t warn(Code_t e, char *s) {
   if (e)
      com_err(__FILE__, e, s);
   return e;
}

#if 0
/* useful for debugging */
void list_subs() {
   int i, n, one = 1;
   ZSubscription_t sub;
   ZRetrieveSubscriptions(0, &n);
   for (i = 0; i < n; i++) {
      ZGetSubscriptions(&sub, &one);
      printf("recip(%s) class(%s) inst(%s)\n",
	     sub.recipient, sub.class, sub.classinst);
   }
}
#endif

void subscribe() {
   static ZSubscription_t sub[] =
   {{"%me%", "MESSAGE", "*"},
    {"*", "MESSAGE", "*"},
    {"*", "*", "*"}};
   int n = sizeof(sub) / sizeof(ZSubscription_t);
   sub[0].recipient = ZGetSender();
   check(ZSubscribeTo(sub, n, 0), "ZSubscribeTo");
}


void subscribe_with_zctl() {
  int pid;
  FILE *fd;
  char st[1024];
  char *portfile;
  extern char *getenv();

  portfile = getenv("WGFILE");

  if(!portfile) {
    sprintf(st,"/tmp/wg.%d",getuid());
    portfile = st;
  }

  fd = fopen(portfile,"w");
  if(!fd) {
    perror(portfile);
    bail(1);
  }
  fprintf(fd, "%d\n", globals->port);
  fclose(fd);

  if(!(pid = vfork())) {
      char fn[1024];
      sprintf(fn,"%s/.zephyr.subs.tzc",getenv("HOME"));
      execl(ZCTL_BINARY,"zctl","load",fn,0);
      perror("zwgc exec");
      fprintf(stderr,"Unable to load subscriptions.\n");
      bail(0);
    }
  if(pid == -1) {
      perror("zwgc exec");
      fprintf(stderr,"Unable to fork and load subscriptions.\n");
    }
  while (pid != wait(0));
}


/*
  like fputs, but quotes the string as necc for 
  reading as a string by gnu-emacs
  */
void fputqs(char *s, FILE *o) {
   char c;
   putc('\"', o);
   while (c = *s++) {
      if (c == '\1') {
	 putc('\\', o);
	 putc('1', o);
      } else {
	 if (c == '\"' || c == '\\')
	    putc('\\', o);
	 putc(c, o);
      }
   }
   putc('\"', o);
}

/* like fputs, but quotes for reading as a symbol by gnu-emacs */
void fputqqs(char *s, FILE *o) {
   char c;
   if ('\0' == *s)
      fputs("nil", o);
   else
      while (c = *s++) {
	 if (!(isalnum(c) || strchr("-+*/_~!@$%^&=:<>{}", c)))
	    putc('\\', o);
	 putc(c, o);
      }
}

void
emacs_put_open()
{
   fputs("\001(", stdout);
}

void
emacs_put_close()
{
   putc(')', stdout);
   putc('\0', stdout);
   putc('\n', stdout);
   fflush(stdout);
}

void
emacs_put_pair_open(char *tag)
{
   putc('(', stdout);
   fputqqs(tag, stdout);
   fputs(" . ", stdout);
}

void
emacs_put_pair_close()
{
   fputs(") ", stdout);
}

void
emacs_put_sym(char *tag, char *sym)
{
   putc('(', stdout);
   fputqqs(tag, stdout);
   fputs(" . ", stdout);
   if (sym[0])
      fputqqs(sym, stdout);
   else
      fputs("nil", stdout);
   fputs(") ", stdout);
}

void
emacs_put_str(char *tag, char *str)
{
   putc('(', stdout);
   fputqqs(tag, stdout);
   fputs(" . ", stdout);
   fputqs(str, stdout);
   fputs(") ", stdout);
}

void
emacs_put_int(char *tag, int i)
{
   putc('(', stdout);
   fputqqs(tag, stdout);
   fputs(" . ", stdout);
   printf("%d", i);
   fputs(") ", stdout);
}

void
emacs_put_str_str(char *tag, char *a, char *b)
{
   putc('(', stdout);
   fputqqs(tag, stdout);
   putc(' ', stdout);
   fputqs(a, stdout);
   fputs(" . ", stdout);
   fputqs(b, stdout);
   fputs(") ", stdout);
}   

void
emacs_error(char *err)
{
   static int reentry = 0;
   if (reentry) {
      /* die a horrible death */
      printf("Looping emacs_error.  Aieee!  I perish in flames!\n");
      exit(5);
   }
   reentry = 1;
   emacs_put_open();
   emacs_put_sym("tzcspew", "error");
   emacs_put_str("message", err);
   emacs_put_close();
   reentry = 0;
}

char *auth_string(int n) {
   switch (n) {
    case ZAUTH_YES    : return "yes";
    case ZAUTH_FAILED : return "failed";
    case ZAUTH_NO     : return "no";
    default           : return "bad-auth-value";
   }
}
 
char *kind_string(int n) {
   switch (n) {
    case UNSAFE:    return "unsafe";
    case UNACKED:   return "unacked";
    case ACKED:     return "acked";
    case HMACK:     return "hmack";
    case HMCTL:     return "hmctl";
    case SERVACK:   return "servack";
    case SERVNAK:   return "servnak";
    case CLIENTACK: return "clientack";
    case STAT:      return "stat";
    default:        return "bad-kind-value";
   }
}

void
send_zgram(Value *spec)
{
    ZNotice_t notice;
    int retval;
    char bfr[BUFSIZ];
    int (*auth)();
    Value *v;
    Value *recip_list;
    char *class;
    Value *messageval;
    Value *message_list;

    bzero((char *) &notice, sizeof(notice));

    /* emacs sends something of the form:
     * ((class . class) (recipients . recip-list))
     */
    /* pull class and inst, and recip_list out of spec */
    v = assqv(globals->constants.sym_class, spec);
    if (VTAG(v) != cons) {
       emacs_error("class not defined");
       goto fail;
    }
    class = vextract_string_c(VCDR(v));
    v = assqv(globals->constants.sym_recipients, spec);
    if (VTAG(v) != cons) {
       emacs_error("recipients not defined");
       goto fail;
    }
    recip_list = VCDR(v);
    v = assqv(globals->constants.sym_message, spec);
    if (VTAG(v) != cons) {
       emacs_error("message not defined");
       goto fail;
    }
    message_list = VCDR(v);

    auth = ZAUTH;
    v = assqv(globals->constants.sym_auth, spec);
    if (VTAG(v) == cons &&
	VTAG(VCDR(v)) == nil)
       auth = ZNOAUTH;

    notice.z_kind = ACKED;
    notice.z_port = 0;
    notice.z_class = class;
    notice.z_opcode = "";
    notice.z_sender = 0;
    notice.z_message = NULL;

    if (auth == ZAUTH) {
       notice.z_default_format = "Class $class, Instance $instance:\nTo: @bold($recipient)\n$message";
    } else {
       notice.z_default_format = "@bold(UNAUTHENTIC) Class $class, Instance $instance:\n$message";
    }

    if (1) {
       /* cdr through the message list, determining total length */
       Value *chase_message_list = message_list;
       char *buffer_mark;
       notice.z_message_len = 0;
       while (VTAG(chase_message_list) == cons) {
	  Value *one_field = VCAR(chase_message_list);
	  notice.z_message_len += VSLENGTH(one_field) + 1;
	  chase_message_list = VCDR(chase_message_list);
       }

       buffer_mark = notice.z_message =
	  (char *) malloc(notice.z_message_len);
       
       /* cdr through again, this time copying strings into the buffer */
       chase_message_list = message_list;
       while (VTAG(chase_message_list) == cons) {
	  Value *one_field = VCAR(chase_message_list);
	  bcopy(VSDATA(one_field), buffer_mark, VSLENGTH(one_field));
	  buffer_mark += VSLENGTH(one_field);
	  *buffer_mark++ = 0;
	  chase_message_list = VCDR(chase_message_list);
       }
    }
    
    /* cdr through the recipient list */
    while (VTAG(recip_list) == cons) {
       Value *recip_pair = VCAR(recip_list);

       if (VTAG(recip_pair) != cons) {
	  emacs_error("bad recipient, not a pair");
	  goto bailout;
       }

       /* these strings are freed when the reply comes back in */
       notice.z_class_inst = vextract_string_c(VCAR(recip_pair));
       notice.z_recipient = vextract_string_c(VCDR(recip_pair));

       if ((retval = ZSendNotice(&notice, auth)) != ZERR_NONE) {
	  (void) sprintf(bfr, "while sending notice to %s", 
			 notice.z_recipient);
	  com_err("foo", retval, bfr);
	  break;
       }
       else {
	  /* prepare for reply */
	  PendingReply *reply = (PendingReply *) malloc(sizeof(PendingReply));
	  reply->instance = notice.z_class_inst;
	  reply->recipient = notice.z_recipient;
	  reply->uid = notice.z_uid;
	  /* any other info to save? */
	  reply->next = globals->pending_replies;
	  globals->pending_replies = reply;
       }

       recip_list = VCDR(recip_list);
    }

  bailout:
    free(notice.z_message);
  fail:
}

void
process_sexp(Value *v)
{
   Value *pair;
   Value *key;

#if 0
   printf("read: ");
   prin(stdout, v);
   fputc('\n', stdout);
#endif

   if (VTAG(v) == cons &&
       NULL != (pair = assqv(globals->constants.sym_tzcfodder, v)) &&
       NULL != (key = VCDR(pair))) {
      if (eqv(key, globals->constants.sym_send))
	 send_zgram(v);
      else
	 emacs_error("bad tzcfodder key");
   }
   else
      emacs_error("no tzcfodder key");

/*
	 printf("parsed: ");
	 prin(stdout, v);
	 fputc('\n', stdout);
 */
#if 0
	 if (destructure(pattern, v)) {
	    printf("match_data = ");
	    prin(stdout, match_data);
	    fputc('\n', stdout);
	 }
	 else {
	    printf("destructure failed\n");
	 }
#endif
}

void
read_emacs()
{
   int ret;
   char *c;
   Value *v;
   
   /* if the buffer is full, expand it*/
   if (globals->ebufptr - globals->ebuf >= globals->ebufsiz) {
      char *new_buf;
      globals->ebufsiz *= 2;
      new_buf = (char *) malloc(globals->ebufsiz);
      bcopy(globals->ebuf, new_buf, globals->ebufptr - globals->ebuf);
      globals->ebufptr = new_buf + (globals->ebufptr - globals->ebuf);
      free(globals->ebuf);
      globals->ebuf = new_buf;
   }

   /* read up to size of buffer */
   ret = read(0, globals->ebufptr,
	      globals->ebufsiz - (globals->ebufptr - globals->ebuf));
   if (ret < 0) {
      if (errno != EWOULDBLOCK) {
	 perror("reading from emacs");
	 bail(1);
      }
   }
   else
      globals->ebufptr += ret;

   /* parse & process all available input from emacs */
   do {
      ret = parse(globals->ebufptr - globals->ebuf,
		  globals->ebuf, &v);
      if (ret > 0) {
	 bcopy(globals->ebuf + ret, globals->ebuf,
	       (globals->ebufptr - globals->ebuf) - ret);
	 globals->ebufptr -= ret;

	 process_sexp(v);

	 free_value(v);
      }
   } while (ret > 0);
   
}

char *time_str()
{
    char *now_name;
    time_t now;
    now = time(0);
    now_name = ctime(&now);
    now_name[24] = '\0';	/* dump newline at end */
    return(now_name);
}

void say_hi() {
   printf("; tzc is free software and you are welcome to distribute copies\n");
   printf("; of it under certain conditions; see the source code for details.\n");
   printf("; Copyright (C) 1992, 1993 Scott Draves\n");
   printf("; Started: %s\n\n", time_str());
   fflush(stdout);
}

void
setup(int use_zctl)
{
   /* set stdin up for nonblocking reads, saving previous flags so we can */
   /* restore them later */
   if (-1 == (globals->stdin_flags = fcntl(0, F_GETFL, 0))) {
      perror("fcntl(0, F_GETFL)");
      exit(1);
   }
   if (-1 == (fcntl(0, F_SETFL, globals->stdin_flags | FNDELAY))) {
      perror("fcntl(0, F_SETFL)");
      bail(1);
   }

   /* if there were an easy way to block-buffer stdout
      i'd do it here */

   check(ZInitialize(), "ZInitialize");
   globals->port = 0;
   check(ZOpenPort(&globals->port), "ZOpenPort");

   if ((globals->zfd= ZGetFD()) < 0) {
      perror("ZGetFD");
      bail(1);
   }

   if (use_zctl)
      subscribe_with_zctl();
   else
      subscribe();

   globals->ebufsiz = 256;
   globals->ebuf = (char *) malloc(globals->ebufsiz);
   globals->ebufptr = globals->ebuf;

   globals->pending_replies = NULL;

   globals->constants.sym_class = vmake_symbol_c("class");
   globals->constants.sym_instance = vmake_symbol_c("instance");
   globals->constants.sym_recipients = vmake_symbol_c("recipients");
   globals->constants.sym_message = vmake_symbol_c("message");
   globals->constants.sym_send = vmake_symbol_c("send");
   globals->constants.sym_tzcfodder = vmake_symbol_c("tzcfodder");
   globals->constants.sym_auth = vmake_symbol_c("auth");
}

void
await_input(int *emacs_in, int *zephyr_in)
{
   *emacs_in = *zephyr_in = 0;

   if (ZPending() > 0)
      *zephyr_in = 1;

    while (!(*emacs_in || *zephyr_in)) {
      fd_set fdset;

      FD_ZERO(&fdset);
      FD_SET(0, &fdset);		/* stdin from emacs */
      FD_SET(globals->zfd, &fdset);	/* Zephyr incoming */

      if (select(globals->zfd + 1, &fdset, NULL, NULL, NULL) < 0) {
	 perror("select");
	 break;
      }
      *emacs_in = FD_ISSET(0, &fdset);
      *zephyr_in = FD_ISSET(globals->zfd, &fdset);
      /* this didn't work - we'll just assume that a whole packet
       * will arrive soon after anything shows up on the input
       if (*zephyr_in)
       if (warn(ZPending(), "ZPending") < 1)
       *zephyr_in = 0;
       */
   }
}

void
report_zgram(ZNotice_t *notice, int auth)
{
      int i, len, forced_termination;
      char *from_host, *p;
      struct hostent *hst;
      char *now_name;

      now_name = time_str();

      /* convert IP# of sender into ascii name */
      hst = gethostbyaddr((char *)&notice->z_sender_addr,
			  sizeof(notice->z_sender_addr), AF_INET);
      from_host = hst ? hst->h_name : inet_ntoa(notice->z_sender_addr);

      /* abbreviate sender by throwing away realm if it's local */
      p = strchr(notice->z_sender, '@');
      if (p && !strcmp(p+1, ZGetRealm()))
	 *p = '\0';

      /* ditto for recipient */
      p = strchr(notice->z_recipient, '@');
      if (p && !strcmp(p+1, ZGetRealm()))
	 *p = '\0';

      emacs_put_open();
      emacs_put_sym("tzcspew", "message");
      emacs_put_sym("kind", kind_string(notice->z_kind));
      emacs_put_sym("class", notice->z_class);
      emacs_put_str("instance", notice->z_class_inst);
      emacs_put_sym("opcode", notice->z_opcode);
      emacs_put_str("sender", notice->z_sender);
      emacs_put_str("recipient", notice->z_recipient);
      emacs_put_int("port", notice->z_port);
      emacs_put_sym("auth", auth_string(auth));
      emacs_put_str("time", now_name);
      emacs_put_str("fromhost", from_host);

      emacs_put_pair_open("message");
      putc('(', stdout);
      p = notice->z_message;
      len = notice->z_message_len;
      forced_termination = 0;
      if (p[len-1]) {
	 /* force null termination */
	 /* this code has not been tested */
	 p = (char *) malloc(len+1);
	 bcopy(notice->z_message, p, len);
	 p[len] = '\0';
	 len += 1;
	 forced_termination = 1;
      }
      for (i = 0; i < len; i++) {
	 fputqs(p+i, stdout);
	 putc(' ', stdout);
	 i += strlen(p+i);
      }
      putc(')', stdout);
      emacs_put_pair_close();

      if (forced_termination) {
	 emacs_put_sym("forced-termination", "t");
	 free(p);
      }

      emacs_put_close();

#if 0
      fputs("(kind . ", stdout);
      fputs(kind_string(notice->z_kind), stdout);
      fputs(") (class . ", stdout);
      fputqqs(notice->z_class, stdout);
      fputs(") (instance . ", stdout);
      fputqs(notice->z_class_inst, stdout);
      fputs(") (opcode . ", stdout);
      fputqqs(notice->z_opcode, stdout);
      fputs(") (sender . ", stdout);
      fputqs(notice->z_sender, stdout);
      fputs(") (recipient . ", stdout);
      if (notice->z_recipient[0])
	 fputqs(notice->z_recipient, stdout);
      else
	 fputs("nil", stdout);
      fprintf(stdout,
	      ") (port . %d) (auth . %s) (time . \"%s\") (fromhost . ",
	      notice->z_port, auth_string(auth), now_name);
      fputqs(from_host, stdout);
      fputs(") (message . (", stdout);
#endif
}

void
process_reply(ZNotice_t *notice, int auth)
{
   PendingReply **link;
   PendingReply *reply = NULL;

   /* remove from the pending reply list */
   link = &globals->pending_replies;
   while (*link != NULL) {
      if (ZCompareUID(&notice->z_uid, &(*link)->uid)) {
	 reply = *link;
	 *link = (*link)->next;
	 break;
      }
      link = &(*link)->next;
   }

   if (NULL == reply)
      /* who called us, anyway ? */
      return;

   /* genuine reply, deal with it */
   switch (notice->z_kind) {
    case SERVACK:
      if (!strcmp(notice->z_message, ZSRVACK_SENT)) {
	 /* tell emacs message sent successfully */
	 emacs_put_open();
	 emacs_put_sym("tzcspew", "sent");
	 emacs_put_str_str("to", reply->instance, reply->recipient);
	 emacs_put_close();
      }
      else if (!strcmp(notice->z_message, ZSRVACK_NOTSENT)) {
	 emacs_put_open();
	 emacs_put_sym("tzcspew", "not-sent");
	 emacs_put_str_str("to", reply->instance, reply->recipient);
	 emacs_put_close();
      }
      else {
	 /* oh nooooo! */
	 emacs_put_open();
	 emacs_put_sym("tzcspew", "error");
	 emacs_put_str("message", "bad reply code");
	 emacs_put_int("code", notice->z_kind);
	 emacs_put_close();
      }
      break;

    case SERVNAK:
      /* tell emacs authorization failure */
      emacs_error("authorization failure");
      break;

    default:
      /* server failure when receiving acknowledgement */
      emacs_error("server failure when processing acknowledgement");
      break;
   }

   free(reply->instance);
   free(reply->recipient);
   free(reply);
}

void
read_zephyr()
{
   ZNotice_t notice;
   struct sockaddr_in from;
   int auth;
   int expected_reply;
   PendingReply *reply;

   while (ZPending() > 0) {
      expected_reply = 0;

      warn(ZReceiveNotice(&notice, &from), "ZReceiveNotice");
      auth = ZCheckAuthentication(&notice, &from);

      if (notice.z_kind != ACKED) {
	 /* check to see whether this is an ack/nak or whether it is new
	  * incoming info */
	 reply = globals->pending_replies;
	 while (reply != NULL) {
	    if (ZCompareUID(&notice.z_uid, &reply->uid)) {
	       expected_reply = 1;
	       break;
	    }
	    reply = reply->next;
	 }
      }

      if (expected_reply)
	 process_reply(&notice, auth);
      else
	 report_zgram(&notice, auth);

      (void) ZFreeNotice(&notice);
   }
}

void restart_tzc(int sig) {
   /* unblock sigusr1 */
   int mask = sigsetmask(0);
   (void) sigsetmask(mask & ~sigmask(SIGUSR1) & ~sigmask(SIGALRM));
   execvp(globals->argv[0], globals->argv);
}

void reset_alarm(long alarm_rate) {
   if (alarm_rate > 0) {
      struct itimerval value;
      value.it_interval.tv_usec = 0;
      value.it_interval.tv_sec = alarm_rate;
      value.it_value.tv_usec = 0;
      value.it_value.tv_sec = alarm_rate;
      setitimer(ITIMER_REAL, &value, (struct itimerval *)0);
   }
}

int main(int argc, char **argv) {
   char *program;
   int use_zctl = 0, sw;
   extern char *optarg;
   char *location = NULL;
   long restart_rate = 0;	/* In seconds; zero disables auto restart */

   globals = &global_storage;
   program = strrchr(argv[0], '/');
   if (program == NULL)
      program = argv[0];
   else
      program++;
   globals->program = program;
   globals->argc = argc;
   globals->argv = argv;

   say_hi();

   signal(SIGUSR1, restart_tzc);
   signal(SIGALRM, restart_tzc);

   while ((sw = getopt(argc, argv, "sa:e:p:")) != EOF)
      switch (sw) {
       case 's':
	 use_zctl = 1;
	 break;
       case 'e':
	 location = optarg;
	 break;
       case 'a':
	 restart_rate = atoi(optarg);
	 break;
       case 'p':
	 if (1) {
	    FILE *f = fopen(optarg, "w");
	    if (f) {
	       fprintf(f, "%d\n", getpid());
	       fclose(f);
	    } else {
	       perror(optarg);
	       exit(1);
	    }
	 }
	 break;
       case '?':
       default:
	 usage();
	 bail(1);
      }

   setup(use_zctl);
   if (location != NULL) {
      check(ZSetLocation(location), "ZSetLocation");
   }

   reset_alarm(restart_rate);

   while (1) {
      int emacs_in, zephyr_in;

      await_input(&emacs_in, &zephyr_in);

      /* if emacs input ready, add it to a buffer */
      if (emacs_in)
	 read_emacs();

      if (zephyr_in) {
	 reset_alarm(restart_rate);
	 read_zephyr();
      }
   }
}
