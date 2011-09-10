/*LINTLIBRARY*/

/* PCmail mail repository version III
   written by Mark L. Lambert

   Copyright 1987, 1988 by the Massachusetts Institute of Technology
   See permission and disclaimer notice in file "notice.h"
 */


/* NNTP client library */

#include <stdio.h>
#include "nntp.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>

/* local procedures */
int nntp_get_socket(), nntp_parse_response(), nntp_read(), nntp_getc();
void nntp_inc_ptr();

char *nntp_errors[] = 
{
  "no error",
  "I/O error",
  "socket error",
  "host not responding",
  "service unavailable",
  "host name unknown",
  "connection reset by foreign host",
  "protocol violation"
};

char *nntp_errstring(nnp)
     register Nntp_stream *nnp;
{
  return(nntp_errors[nntp_errno(nnp)]);
}

int nntp_open_connection(server, nnp)

char *server;
register Nntp_stream *nnp;
{
  memchr(nnp, 0, sizeof(Nntp_stream));                              /* zorch */
  nnp->nn_new_data_begin = nnp->nn_old_data_begin = nnp->nn_inbuf;
  nnp->nn_nbytes = 0;
  if((nnp->nn_skt = nntp_get_socket(nnp, server)) == ERROR)
    return(ERROR);
  else if(nntp_in(nnp) == ERROR)                        /* get server banner */
    goto CommErr;
  else if(nntp_parse_response(nnp) == ERROR) 
    goto CommErr;
  return(OK);

 CommErr:
  if(nnp->nn_skt) (void) close(nnp->nn_skt);
  return(ERROR);
}

int nntp_get_socket(nnp, host)
     
register Nntp_stream *nnp;
char *host;
{
  int s;
  struct sockaddr_in address;
  struct servent *svc_info;
  struct hostent *host_info;
  
  if(! (svc_info = getservbyname("nntp", "tcp"))) 
  {
    nnp->nn_error = NN_ERR_NOSVC;
    return(ERROR);
  }
  else if(! (host_info = gethostbyname(host))) 
  {
    nnp->nn_error = NN_ERR_NOHOST;
    return(ERROR);
  }
  memchr((char *) &address, 0, sizeof(address));
  memcpy((char *) &(address.sin_addr), host_info->h_addr, host_info->h_length);
  address.sin_family = host_info->h_addrtype;
  address.sin_port = svc_info->s_port;
  if((s = socket(host_info->h_addrtype, SOCK_STREAM, 0)) == ERROR) 
  {
    nnp->nn_error = NN_ERR_SKT;
    return(ERROR);
  }
  else if(connect(s, &address, sizeof(address)) == ERROR) 
  {
    nnp->nn_error = NN_ERR_CONN;
    return(ERROR);
  }
  else return(s);
}

int nntp_out(nnp, str)

register Nntp_stream *nnp;
register char *str;
{
  register char *bptr = nnp->nn_outbuf;
  register int nchrs;
  register int len;

  if(*str == '.' && strlen(str) > 1)             /* stuff a dot if necessary */
    *bptr++ = '.';
  memcpy(bptr, str, strlen(str));
  bptr += strlen(str);
  memcpy(bptr, "\r\n", 2);
  bptr += 2;

  /* we've stuffed the string into the output buffer,complete with dot-stuffing
     and CRLF.  Now find length and reset bptr */
  len = bptr - nnp->nn_outbuf;
  bptr = nnp->nn_outbuf;
  while(len)
  {
    if((nchrs = send(nnp->nn_skt, bptr, len, 0)) == ERROR)
    {
      if(errno == 0)
        nnp->nn_error = NN_ERR_RESET;
      else
        nnp->nn_error = NN_ERR_IO;
      return(ERROR);
    }
    bptr += nchrs;
    len -= nchrs;
  }
  return(OK);
}

/* return 0 if end of list (dot by itself), 1 otherwise, -1 if error */

int nntp_in(nnp)

register Nntp_stream *nnp;
{
  if(nntp_read(nnp) == ERROR)
    return(ERROR);
  if(nnp->nn_reply[0] == '.')                 /* de-stuff a dot if necessary */
    if(strcmp(nnp->nn_reply, ".") == 0) return(0);
    else if(nnp->nn_reply[1] == '.')
      (void) strcpy(nnp->nn_reply, nnp->nn_reply + 1);
  return(1);
}

/* return FALSE if the current server reply contains an error response, TRUE
   else, ERROR if error */

int nntp_parse_response(nnp)

register Nntp_stream *nnp;
{
  char reply_code[512];             /* protocol may lose, so use huge buffer */
  
  if(sscanf(nnp->nn_reply, "%s", reply_code) != 1) 
  {
    nnp->nn_error == NN_ERR_PROTO;
    return(ERROR);
  }
  nnp->nn_reply_code = atoi(reply_code);
  if(*reply_code == CHAR_ERR || *reply_code == CHAR_FATAL || 
     *reply_code < CHAR_INF) 
    return(FALSE);
  return(TRUE);
}

int nntp_command(nnp, msg)

register Nntp_stream *nnp;
register char *msg;
{
  if(nntp_out(nnp, msg) == ERROR) return(ERROR);
  else if(nntp_in(nnp) == ERROR) return(ERROR);
  else return(nntp_parse_response(nnp));
}

void nntp_close(nnp)

register Nntp_stream *nnp;
{
  (void) nntp_command(nnp, "QUIT");
  if(nnp->nn_skt) (void) close(nnp->nn_skt);
}

/* read from the nntp server, if result is an end-of-list (dot by itself),
   return TRUE else FALSE, ERROR if error */
int nntp_list_end_p(nnp)
     register Nntp_stream *nnp;
{
  int endp;

  if((endp = nntp_in(nnp)) == 0) return(TRUE);
  else if(endp == 1) return(FALSE);
  else return(ERROR);
}

/* read into an NNTP buffer, not including the carriage-return-newline pair
   that terminate every NNTP command and response.  Assumes the supplied
   buffer is at least 512 bytes long, since that is the maximum length of
   an NNTP command/response.  This is gross since VMS won't let us use
   stdio to do buffering.  Instead, we need to do our own buffering.  Ick. */

#define NNTP_INC(nnp)  (nnp)->nn_new_data_begin++; (nnp)->nn_nbytes--;
#define NNTP_NOCRLF 0
#define NNTP_CR 1

int nntp_read(nnp)
     register Nntp_stream *nnp;
{
  register char *bptr = nnp->nn_reply;
  register int nchrs;
  register int crlf_state = NNTP_NOCRLF;

  while(TRUE)
  {
    if(nnp->nn_nbytes == 0)                        /* empty buffer?  Fill it */
    {
      nnp->nn_new_data_begin = nnp->nn_inbuf;
      if((nchrs = recv(nnp->nn_skt, nnp->nn_inbuf, sizeof(nnp->nn_inbuf), 
		       0)) == ERROR)
      {
	if(errno != 0)
          nnp->nn_error = NN_ERR_IO;
        else
          nnp->nn_error = NN_ERR_RESET;
	return(ERROR);
      }
      nnp->nn_nbytes += nchrs;
    }

    /* look for CRLF */
    while(nnp->nn_nbytes)
    {
      if(*(nnp->nn_new_data_begin) == '\r')
      {
	NNTP_INC(nnp);                                        /* punt the CR */
	crlf_state = NNTP_CR;
      }
      else
      {
	if(crlf_state == NNTP_CR)
	{
	  if(*(nnp->nn_new_data_begin) == '\n')
	  {
	    NNTP_INC(nnp);                                    /* punt the LF */
	    *bptr = '\0';
	    return(TRUE);
	  }
	  else
	  {
	    *bptr++ = '\r';                                    /* replace CR */
	  }
	}
	*bptr++ = *(nnp->nn_new_data_begin);
	NNTP_INC(nnp);
	crlf_state = NNTP_NOCRLF;
      }
    }
  }
}
