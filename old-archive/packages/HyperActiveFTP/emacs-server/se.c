/**************************************************************************
***
***  se.c  -  connect to emacs server and send file path(s)
***  ------------------------------------------------------
***
***  SCCS ID: 92/04/07 11:51:40 2.2 se.c
***
***  Copyright (C) 1991 Claus Bo Nielsen
**************************************************************************/

#ifndef lint
static char      *_Version = "@(#)92/04/07 11:51:40, 2.2 se.c";
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <malloc.h>

#include "se.h"				  /* here we defind mutual things */

static int   debug = DEBUG;		  /* are we debugging?            */
static char  hostname[20] = "";		  /* which host to connect to     */
static int   file_socket = FALSE;	  /* use unix file socket         */


/***************************************************************************/
int   main(argc, argv)
int   argc;
char *argv[];
/***************************************************************************/
{
   int                ch;
   int                sock;
   int                length;
   struct sockaddr_in server;
   struct sockaddr_un server_un;
   char               filename[PATH_LENGTH];
   char               filepath[PATH_LENGTH];
   char               pwd[PATH_LENGTH];
   char              *elisp = NULL;
   char              *elisp_after = NULL;
   struct hostent    *hp, *gethostbyname();
   extern char       *optarg;
   extern int         optind;
   char              *port_path = NULL;
   
   while ((ch = getopt(argc, argv, "dfp:h:e:a:u")) != -1)
      switch(ch)
      {
      case 'd':
	 debug = TRUE;
	 break;
      case 'f':
	 file_socket = TRUE;
	 break;
      case 'p':
	 port_path = optarg;
	 break;
      case 'h':
	 strcpy(hostname, optarg);
	 break;
      case 'e':
	 elisp = (char *)calloc(strlen(optarg)+1, 1);
	 strcpy(elisp, optarg);
	 break;
      case 'a':
	 elisp_after = (char *)calloc(strlen(optarg)+1, 1);
	 strcpy(elisp_after, optarg);
	 break;
      case 'u':
      case '?':
	 PrintUsage();
	 exit(0);
	 break;
      }

   if (strlen(hostname) < 1)		  /* no -h specified */
   {
      if (getenv("HOST") == NULL)
         err("101 Can't find the HOST environment variable");
      else
         strcpy(hostname, getenv("HOST"));
   }

   if (file_socket)			  /* use unix socket */
   {
      if ((sock = socket(AF_UNIX, SOCK_STREAM, 0)) == -1)
	 err("109 Can't get a socket");
      
      server_un.sun_family = AF_UNIX;

      if (port_path == NULL)
	  strcpy(server_un.sun_path, SOCKET_PATH);
      else
      {
	 if (debug)
	    fprintf(stderr, "Using file %s for unix socket\n", port_path);
	 
	 strcpy(server_un.sun_path, port_path);
      }

      if (connect(sock, (struct sockaddr *)&server_un, sizeof(server_un)) == -1)
	 err("124 Connect failed");
   }
   else					  /* use inet socket */
   {
      if (debug)
	 fprintf(stderr,"Connect to emacs-server daemon on %s\n",
		 hostname);
      
      if ((sock = socket(AF_INET, SOCK_STREAM, 0)) == -1)
	 err("133 Can't get a socket");
      
      server.sin_family = AF_INET;
      
      if ((hp = gethostbyname(hostname)) == NULL)
	 err("138 gethostbyname failed - can't resolve host");
      
      bcopy((char *)hp->h_addr, (char *)&server.sin_addr, hp->h_length);
      
      if (port_path == NULL)
	 server.sin_port = SOCKET_PORT;
      else
	 server.sin_port = atoi(port_path);
      
      if (connect(sock, (struct sockaddr *)&server, sizeof(server)) == -1)
	 err("148 Connect failed");
   }
   
   if (debug)
      fprintf(stderr,"Connected to emacs-server daemon on %s\n", hostname);

   if (elisp != NULL)
      SendElisp(sock, elisp);
   
   while(optind < argc)
   {
      if (strlen(argv[optind]) >= PATH_LENGTH)
	 err("160 File path too long");

      strcpy(filename, argv[optind]);	  /* get the filename */

      /* We have to find out if the file specified is an absolute path */
      /* if is is everything is OK else append the filename to 'pwd'.  */
      if (filename[0] != '/')
      {
	 strcpy(filepath, (char *)getwd(pwd));
	 strcat(filepath, "/");
	 strcat(filepath, filename);
      }
      else
	 strcpy(filepath, filename);

      RmDoubleSlash(filepath);
      
      length = strlen(filepath);
      if (debug)
	 fprintf(stderr,"Sending file path: %s\n", filepath);

      if (write(sock, &length, 4) != 4)
	 err("182 Transmission error");
      
      if (write(sock, filepath, length) != length)
	 err("185 Transmission error");
      else
	 optind++;
   }

   if (elisp_after != NULL)
      SendElisp(sock, elisp_after);

   length = -1;				  /* send finished */
   if (write(sock, &length, 4) != 4)
      err("195 Transmission error");
}


/***************************************************************************/
int RmDoubleSlash(str)			  /* rm "//" from a file path      */
char *str;				  /* emacs don't like file names   */
					  /* like /etc///hosts (unix       */
                                          /* don't care)                   */
/***************************************************************************/
{
   char *new_str,
        *str_ch,
        *ch;

   str_ch = str;
   new_str = ch = (char *)strdup(str);

   while(*str_ch != '\0')
   {
      if (*str_ch == '/')
      {
	 *ch++ = *str_ch++;
      
	 while(*str_ch == '/')
	    str_ch++;
      }
      else
	 *ch++ = *str_ch++;
   }
   *ch = '\0';
   
   strcpy(str, new_str);
   free(new_str);

   return(0);
}


/***************************************************************************/
int SendElisp(sock, exp)		  /* send elisp expression         */
int   sock;
char *exp;
/***************************************************************************/
{
   int length;
   
   length = ELISP_EXP;
   if (write(sock, &length, 4) != 4)
      err("244 Transmission error");
   
   length = strlen(exp);
   if (write(sock, &length, 4) != 4)
      err("248 Transmission error");
   
   if (write(sock, exp, length) != length)
      err("251 Transmission error");
   
   if (debug)
      fprintf(stderr,"Sending elisp string of length %d\n", length);
}


/***************************************************************************/
int err(s)				  /* send error message to console */
char *s;
/***************************************************************************/
{
   fprintf(stderr, "Error - se.c:%s\n", s);
   exit(1);
}

      
/***************************************************************************/
int PrintUsage()			  /* print usage on stdout         */
/***************************************************************************/
{
   fprintf(stderr,"se (SendEmacs) version: 2.2 (92/04/07 11:51:40)\n");
   fprintf(stderr,"Usage: se [-d][-f][-h<host>][-p<port/path>][-e<elisp>][-a<elisp>] <file> ...\n");
   fprintf(stderr," -d             : debug information\n");
   fprintf(stderr," -f             : use UNIX socket (other path in -p)\n");
   fprintf(stderr," -h <host>      : connect to <host>, default $HOST\n");
   fprintf(stderr," -p <port/path> : port no or file path to use, default %d or %s\n", SOCKET_PORT, SOCKET_PATH);
   fprintf(stderr," -e <elisp>     : emacs will evaluate the <elisp> expression(s)\n");
   fprintf(stderr," -a <elisp>     : as -e but will be send expression(s) after file(s)\n");
}

   
/** End of se.c **/
