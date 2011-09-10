/*
 * Omega source	file
 *
 * Copyright 1986,1987 by Daniel R. Grayson.
 *
 * This	material contains trade	secrets	and may	be registered with the U.S.
 * Copyright Office as an unpublished work, pursuant to	Title 17, U.S. Code,
 * Section 408.	 Unauthorized copying, adaptation, distribution	or display is
 * prohibited.
 *
 */

/* emacsfrontend.c - derived from frontend.c, output sexps readable
   by GNU Emacs Lisp. */

#include <stdio.h>
#include "interrupts.h"
#include "trace.h"
#include "lib.h"
#define FRONTENDVERSION
#include "mathtalk.h"

#include "mathdescemacs.h"

static int column = 1;          /* counter for newline insertion */

/* end of state */

#define LINEWIDTH 80
#define outc_bump(c) (outc(c), \
		      LINEWIDTH == ++column ? (outc('\n'), column = 1) : 0)


inc()
{
  return twgetc(leftin);
}

outc(c)
{
  return twputc(c,leftout);
}

outflush()
{
  twfflush(leftout);
}

     
char *sprintf();

#define put(x) fputs(x,stdout)
#define send(x) (void)twwrite(LEFTOUT,x,strlen(x))

int hadint, busywriting;

onint(){
/*     if (!permissiontointerrupt) {
	  puts("\n$ Sorry, we can't interrupt the kernel now.");
	  if (permissiontospeak) userprompt();
	  return;
	  } */
     if (busywriting) {
	  hadint = YES;
	  }
     else {
     	  sendinterrupt();
	  }
     (void)signal(SIGINT,onint);
     }

#define BUFSZ 4096

char indata[BUFSZ];

/* modified to wrap in double quotes and escape double quotes. */
putn(s,n)
char *s;
{
  putchar('"');
  while (n-->0)
    {
      if (('"' == *s) || ('\\' == *s))
	putchar('\\'); /* escape double quote */
      putchar(*s++);
    }
  putchar('"'); putchar(' ');
}


char *
  actionname(ac)
{
  if (ac >=0 && ac <= aTOTAL) return action_name[ac];
  return NULL;
}


userprompt(){
     switch(current_menu) { /* this needs to be changed - maybe do nothing */
	  case INTERRUPT_MENU: put("(interrupt) "); break;
	  case DEBUG_MENU: put("(debug) "); break;
	  case INSPECTOR_MENU: put("(inspector) "); break;
	  case TOPLEVEL_MENU: break;
	  }
     }

/*ARGSUSED*/
main(argc,argv)
char **argv;
{
  int dir, c, ac, blksiz;
  char *blk;
  stack *user = newstack(char,256,256);
  setprogname(argc,argv);
  twinit(RIGHTEND);
  mtinit();
  (void)signal(SIGINT,onint);
  sendaction(aRESPOND); /* remove this later */
     
  while(1)
    {
      c = twbigetchar(&dir);
      if (c == EOF) {
	twshutdown();
	trace("exiting");
	exit(0);
      }
      
      if (dir == LEFTWARD) {
	/* user input */
	/* putchar(c); /* echo it */
	outc_bump(c);
	if (c == '\n') {
	  twfflush(rightout);
	  outflush();
	}

/*	  busywriting = YES;
	  senddata(stackcontents(user),stacksize(user));
	  emptystack(user);
	  busywriting = NO;
	  if (hadint) {
	    hadint = NO;
	    sendinterrupt();
	  }
	}
	else {
	  pushstack(user,c,char);
	} */
      }

      else			/* dir == RIGHTWARD */
	{
	  /* kernel output */
	  ac = interpret(c);
	  if (ac != 0) {
	    if (withdata(ac)) {
	      getblock(&blk,&blksiz);
	      putn(blk,blksiz);
	      if (blksiz!=0 && blk[blksiz-1]!='\n') {
		/* for readability only */
		putchar('\n');
	      }
	      Free(blk);
	    }
	    puts(actionname(ac));
	  }
	  fflush(stdout);
	  if (permissiontospeak) /* userprompt(); */
	    ;
	}
    }
}
