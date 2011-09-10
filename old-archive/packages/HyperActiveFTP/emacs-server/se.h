/**************************************************************************
***
***  se.h  -  include file for emacs-server and send-emacs (se)
***  ----------------------------------------------------------
***
***  SCCS ID: 92/04/07 11:51:43 2.2 se.h
***
***  Copyright (C) 1991 Claus Bo Nielsen
**************************************************************************/

#define TRUE          1
#define FALSE         0

#define DEBUG         0                   /* 0: no, 1: yes               */

#define SOCKET_PATH  "/tmp/emacs-server"  /* default using UNIX socket   */
#define SOCKET_PORT   9000                /* we use port 9000            */
#define PATH_LENGTH   80                  /* max path string length      */
#define ELISP_EXP     -100                /* Code to emacs lisp exp.     */
#define WAIT_TIME     500000              /* time to wait for emacs      */


