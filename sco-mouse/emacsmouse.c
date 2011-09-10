/*
 * emacsmouse.c - emacs mouse driver
 * copyright 1990 Ronald Florence (ron@mlfarm, 5.7.90)
 */

#include <fcntl.h>
#include <sys/termio.h>
#include <stdio.h>
#include <signal.h>
#include <sys/machdep.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/sysmacros.h>
#include <sys/page.h>
#include <sys/event.h>
#include <mouse.h>

#define BUTTONS	0x7
#define RATIO	0x40		/* You may need to change this if your */
				/* mouse is overactive or lazy. */

main(argc, argv)
int argc;
char **argv;
{
  int quit(), sensitivity, ratio = RATIO;
  EVENT *evp;
  dmask_t dmask = D_REL | D_BUTTON;

  if (argc <= 1 || (sensitivity = atoi(*++argv)) < 1 || sensitivity > 9)
    sensitivity = 5;
  ratio <<= sensitivity;

  signal(SIGINT, quit);
   
  if (ev_init() < 0)
    quit();
  if (ev_open(&dmask) < 0)
    {
#ifdef DEBUG
      printf("cannot open event queue\n");
#endif
      quit();
    }
  if (dmask != (D_REL | D_BUTTON)) 
    {
#ifdef DEBUG
      printf("cannot attach mouse\n");
#endif
      quit();
    }

  while (!ev_block()) 
    if ( (evp = ev_read()) != (EVENT *) NULL ) 
      {
	char button;

	switch (EV_BUTTONS(*evp) & BUTTONS)
	  {
	  case LT_BUTTON: 
	    button = 'l'; 
	    break;
	  case MD_BUTTON:
	    button = 'c';
	    break;
	  case RT_BUTTON: 
	    button = 'r'; 
	    break;
	  case (LT_BUTTON | RT_BUTTON): 
	    button = 'b'; 
	    break;
	  case (LT_BUTTON | RT_BUTTON | MD_BUTTON):
	    button = 'a';
	    break;
	  case (LT_BUTTON | MD_BUTTON):
	    button = 'L';
	    break;
	  case (RT_BUTTON | MD_BUTTON):
	    button = 'R';
	    break;
	  default: 
	    button = 'n'; 
	    break;
	  }
	if (button != 'n' || EV_DX(*evp) || EV_DY(*evp))
	  {
	    EV_DX(*evp) *= ratio;
	    EV_DY(*evp) *= ratio;
	    EV_DX(*evp) >>= LOC_RSHIFT;
	    EV_DY(*evp) >>= LOC_RSHIFT;
#ifdef DEBUG
	    printf("%c %+.2d %+.2d\n", button, EV_DX(*evp), EV_DY(*evp));
#else
	    printf("%c%+.2d%+.2d", button, EV_DX(*evp), EV_DY(*evp));
	    fflush(stdout);
#endif
	  }
	ev_pop();
      }
  return 0;
}


quit()
{
  ev_close();
  exit(0);
}
