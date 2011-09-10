/*
 * emacs-console.c - make the current pty the console
 *
 * ben@piglet.cr.usgs.gov
 *
 * $Header: /z/piglet/home/ben/src/console-buffer/RCS/emacs-console.c,v 1.2 1993/09/29 18:10:19 ben Exp $
 *
 * $Log: emacs-console.c,v $
 * Revision 1.2  1993/09/29  18:10:19  ben
 * Added support for ULTRIX and AUX like UNIXen
 *
 * Revision 1.1  1993/09/26  00:45:54  ben
 * Initial Revision
 *
 *
 */

#include <stdio.h>
#include <sys/ioctl.h>
#include <unistd.h>

/* define -only one- of the following */

/*#define USE_OLDMSGS /* read console messages from /dev/oldmsgs ala A/UX */
/*#define USE_XCONS /* read console messages via /dev/xcons ala ULTRIX */
/*#define USE_TIOCCONS /* redirect console to pty with TIOCCONS ala DG/UX */

void main(int argc, char **argv)
{
#ifdef USE_OLDMSGS
	execlp("tail","tail","-f","/dev/oldmsgs",NULL);
#endif
#ifdef USE_XCONS
	execlp("cat","cat","/dev/xcons",NULL);
#endif
#ifdef USE_TIOCCONS
	if (0 == ioctl(fileno(stdout), TIOCCONS, &argc)) {
		pause();
	} else {
		perror("emacs-console");
	}
#endif
}
 
