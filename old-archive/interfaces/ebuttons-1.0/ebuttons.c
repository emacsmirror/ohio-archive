#ifndef lint
static char *rcsid = "$Header: /tmp_mnt/vida/disks/disk5/Users/terry/s/ebuttons/RCS/ebuttons.c,v 1.1 1992/11/28 22:41:25 terry Exp $";
#endif


/*
 * X interface to Emacs.
 * Copyright (C) 1992  Terry Jones
 * (Based (heavily) on the taglist facility written by Brad Mears)
 *
 * This file is part of ebuttons
 *
 * Ebuttons is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * The GNU General Public License can be obtained via anonymous ftp from
 * prep.ai.mit.edu as pub/gnu/COPYING or pub/gnu/COPYING-2.
 *
 * Ebuttons is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

/*
 * This provides a point-and-click command interface to an 
 * emacs session.  This won't be much use without ebuttons.el.
 * See the README for details on use.
 */

#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#ifdef HAVE_XAW3D
#include <X11/Xaw3d/Box.h>
#include <X11/Xaw3d/Command.h>
#else
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#endif

static void create_buttons();
static void button_press();
void toggle_visibility();

typedef struct {
    String labels[MAX_BUTTONS];
    String commands[MAX_BUTTONS];
} AppResources;

static AppResources appResources;
static Widget toplevel;            

#include "ebuttons.h"

int 
main(argc, argv)
int argc;
char **argv;
{
   XtAppContext appContext;

   toplevel = XtAppInitialize(&appContext, "ebuttons", NULL, 0, &argc, argv, NULL, NULL, 0);
   XtGetApplicationResources(toplevel, (XtPointer)&appResources, resources, XtNumber(resources), NULL, 0);
   create_buttons(toplevel);
   XtAppAddInput(appContext, fileno(stdin), XtInputReadMask, toggle_visibility, NULL);
   XtRealizeWidget(toplevel);
   XtAppMainLoop(appContext);
   return 0;
}


/*
 * Callback for the configurable command buttons.  The quit button
 * has no associated command.
 */
static void 
button_press(w, command, dummy)
Widget  w;
XtPointer command; 
XtPointer dummy;
{
   if (!command){
      exit(0);
  }

   printf("%s", (String)command);
   fflush(stdout);
   return;
}


static void 
create_buttons(parent)
Widget parent;
{
    Widget cmds[MAX_BUTTONS];
    Widget cmdbox;              
    Widget quit;
    int i;
    Dimension width;
    Dimension max_width = 0;
    
    cmdbox = XtCreateManagedWidget("cmdbox", boxWidgetClass, parent, NULL, 0);
    
    for (i = 0; i < MAX_BUTTONS; i++) {
	if (appResources.labels[i] && appResources.commands[i]){
	    cmds[i] = XtCreateManagedWidget(appResources.labels[i], commandWidgetClass, cmdbox, NULL, 0);
	    XtAddCallback(cmds[i], XtNcallback, button_press, (XtPointer)appResources.commands[i]);
	
	    XtVaGetValues(cmds[i], XtNwidth, &width, NULL);
	    
	    if (max_width < width){
		max_width = width;
	    }
	}
    }
    
    quit = XtCreateManagedWidget("Quit", commandWidgetClass, cmdbox, NULL, 0);
    XtAddCallback(quit, XtNcallback, button_press, NULL);
    
    XtVaGetValues(quit, XtNwidth, &width, NULL);
    
    if (max_width < width){
	max_width = width;
    }
    
    /* Set all the buttons to the same width */
    for (i = 0; i < MAX_BUTTONS; i++) {
	if (appResources.labels[i] && appResources.commands[i]){
	    XtVaSetValues(cmds[i], XtNwidth, max_width, NULL);
	}
    }
    
    XtVaSetValues(quit, XtNwidth, max_width, NULL);
    
    return;
}


void 
toggle_visibility(client_data, dummy1, dummy2)
XtPointer client_data;
int *dummy1;
XtInputId *dummy2;
{
   char buf[BUFSIZ];
   static int visible = 1;

    /* Read input from stdin.  We know it is ready since we have been called. */
   
   if (!fgets(buf, BUFSIZ, stdin)){
       /* This goes to stdout, not stderr. */
       printf("could not read from stdin!\n");
       exit(0);
   }

   if (visible) {
       XtUnmapWidget(toplevel);
       visible = 0;
   }
   else {
       XtMapWidget(toplevel);
       visible = 1;
   }
   
   return;
}

