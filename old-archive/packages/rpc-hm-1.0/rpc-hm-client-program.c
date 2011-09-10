/*
       Copyright (C) 1991, 1992 Eyvind Ness. All rights reserved.
*/

/* 
	Author: Eyvind Ness (eyvind) 
	Date:   Monday, June 29 1992 08:21 GMT
	File:   /usr/local/gnu/emacs/elisp/rpc-hm-1.0/rpc-hm-client-program.c
*/

/*
  GNU Emacs Note: To make this client work with rpc-hm.el put the
  executable of this program in a place where it can be found somewhere
  along your exec-path, e.g. /usr/local/gnu/bin/rpc-hm-client-program

  To generate the executable, just do M-x compile in Emacs, or type the
  compile-command at the end of this file manually to a Unix shell.
  

Dependencies: 

  - Sun RPC, UDP, IP network software
  - Ethernet LAN

Description:

  An RPC client for evaluating LISP expression on a remote Lisp host
  computer. It sends a string converted to the network standard
  representation as defined by XDR to a LISP server process on the
  server host. It expects a string back containing the result of the
  requested operation. 

Notes:

  The RPC portmapper daemon must be running on both computers, see
  portmap(1m).

*/

#include <stdio.h>
#include <rpc/rpc.h>

#define LISP_SERVICE 0x20000ffe
#define VERSION_NO 1
#define PROC_NO 1

/* 8K is the maximum packet size in UDP: */
#define MAXBUFSZ 8192
  
int xdr_net_string ();
void print_reply ();
void prompt_if ();

main (argc, argv)
     int argc;
     char *argv[];
{
  char *reply_ptr, *query_ptr;
  char reply[MAXBUFSZ], query[MAXBUFSZ];
  int status, i=0;
  int promptp = (argc == 3);
  char prompt[64], nextch;
  
  query_ptr = query;
  reply_ptr = reply;
/*
  A NULL ptr would force XDR to allocate memory for the reply.
*/

  if (argc < 2)
    {
      fprintf(stderr,
	      "Usage: %s <hostname> &optional (promptp nil)\n",
	      argv[0]);
      exit (1);
    }
  strncpy(prompt,argv[1],64);

  prompt_if(promptp, prompt);
  fflush(stdout);

  nextch=fgetc(stdin);
  while (!feof(stdin) && (i<MAXBUFSZ-1) )
    {
      query[i]=nextch;
      nextch=fgetc(stdin);
      i++;
    }
  query[i]='\0';		/* Null terminate */
      
  status = callrpc(argv[1],
		   LISP_SERVICE,
		   VERSION_NO,
		   PROC_NO,
		   xdr_net_string,
		   &query_ptr,
		   xdr_net_string,
		   &reply_ptr);
  if (status != 0) 
    {
      fprintf(stderr,"\ncallrpc error: ");
      clnt_perrno(status);
      fprintf(stderr,"\n");
      exit (1);
    }

  print_reply ( reply, promptp );
  exit(0);
}
/* End main */


int xdr_net_string (xdrs, sp)
/* 
          this  function  converts  both  to  and  from  the
          network  standard representation  of    strings as
          defined by XDR.
*/
     XDR *xdrs;
     char **sp;
{
  return ( xdr_string(xdrs,sp,MAXBUFSZ) );
}
  
void print_reply(str, end_of_data_flagp)
     char str[];
     int end_of_data_flagp;
{
  if (end_of_data_flagp)
    fprintf(stdout, "%s\n",str);
  else
    fprintf(stdout, "%s\nEOF\n",str);
    
  fflush(stdout);
}

void prompt_if (cond, prompt)
     int cond;
     char *prompt;
{
  if (cond)
    fprintf(stdout, "%s> ", prompt);
}


/*
   Local Variables:
   compile-command: "cc -O -o rpc-hm-client-program rpc-hm-client-program.c"
   End:
*/
