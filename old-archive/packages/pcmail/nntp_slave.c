#include <stdio.h>
#include "nntp.h"
#include <errno.h>

#define SLV_ART_DELIM "\014\n"
#define SLV_M_NAME 50
#define SLV_M_PATH 256
#define SLV_M_BUF 512
#define MAX(x, y) ((x) > (y) ? (x) : (y))

void slv_err_report(), slv_osd_bboard();
char *strerror();

/* nntp_slave <server> <newsgroup name> <output file> <control file> */

void main(argc, argv)

int argc;
char *argv[];
{
  char server[SLV_M_NAME], bbname[SLV_M_NAME], ctlname[SLV_M_PATH],
       outfile[SLV_M_PATH];
  int slv_cmdline(), slv_new_articles(), ret;
  Nntp_stream nnp;

  if(slv_cmdline(argc, argv, server, bbname, outfile, ctlname) == ERROR)
    exit(1);
  if(nntp_open_connection(server, &nnp) == ERROR)
  {
    slv_err_report(&nnp);
    exit(1);
  }
  ret = slv_new_articles(&nnp, bbname, outfile, ctlname);
  nntp_close(&nnp);
  (ret == ERROR) ? exit(1) : exit(0);
}

int slv_cmdline(argc, argv, server, bbname, outfile, ctlname)

int argc;
char *argv[];
char *server;
char *bbname;
char *outfile;
char *ctlname;
{
  if(argc != 5) 
  {
    printf("nntp-slave: usage is nntp_slave <server-name> <newsgroup-name> <outfile> <controlfile>\n");
    return(ERROR);
  }
  strncpy(server, argv[1], SLV_M_NAME);
  strncpy(bbname, argv[2], SLV_M_NAME);
  strncpy(outfile, argv[3], SLV_M_PATH);
  strncpy(ctlname, argv[4], SLV_M_PATH);
  return(OK);
}

int slv_new_articles(nnp, bbname, outfile, ctl_path)

Nntp_stream *nnp;
char *bbname;
char *outfile;
char *ctl_path;
{
  char ctlinfo[SLV_M_BUF], ctlbboard[SLV_M_NAME], 
       cmdbuf[SLV_M_BUF], junk2[SLV_M_NAME], ctl_path_new[SLV_M_PATH];
  int slv_deliver(), ctllast = 0, ret, bb_first, bb_last, junk, newlast,
      slv_open_ctl_outfile();
  FILE *fin = NULL, *fout = NULL;

  /* open control file, read all information, write out all but desired 
     bboard, store desired bboard information (last article read) */

  (void) sprintf(ctl_path_new, "%snew", ctl_path);
  fin = fopen(ctl_path, "r");
  if(slv_open_ctl_outfile(ctl_path_new, &fout) == ERROR)
    goto EndComm;
  if(fin)
  {
    while(fgets(ctlinfo, sizeof(ctlinfo), fin))
    {
      if(sscanf(ctlinfo, "%s %d", ctlbboard, &junk) != 2)
      {
	printf("nntp-slave: Illegal format in newsgroup control file\n");
	goto EndComm;
      }
      if(strcmp(ctlbboard, bbname) != 0)
      {
        fputs(ctlinfo, fout);
	if(ferror(fout))
	{
	  printf("nntp-slave: Newsgroup control file write error (%s)\n", 
		 strerror(errno));
	  goto EndComm;
	}
      }
      else
        ctllast = junk;
    }
    if(fclose(fin) == ERROR)
    {
      printf("nntp-slave: Newsgroup control file close error (%s)\n", 
	     strerror(errno));
      fin = NULL;
      goto EndComm;
    }
  }
  /* if newsgroup is already subscribed to, ctllast is last article number 
     read, otherwise it is zero.  Set group to desired bboard and read 
     information.  If group exists, set target article count to be max 
     articles on group, and set start article count to be 
     MAX(ctllast, group-first).  Get articles, update control file, and bung 
     out */
  (void) sprintf(cmdbuf, "group %s", bbname);
  if((ret = nntp_command(nnp, cmdbuf)) == ERROR)
  {
    slv_err_report(nnp);
    goto EndComm;
  }
  else if(ret == 0)
  {
    if(nntp_current_reply_code(nnp) == ERR_NOGROUP)
      printf("nntp-slave: No newsgroup named \"%s\"\n", bbname);
    else
      printf("nntp-slave: Unexpected response \"%s\"", 
	     nntp_current_reply(nnp));
    goto EndComm;
  }
  if(sscanf(nntp_current_reply(nnp), "%d %d %d %d %s", &junk, &junk, 
	    &bb_first, &bb_last, junk2) != 5)
  {
    printf("nntp-slave: Protocol error in GROUP reply \"%s\"\n", 
	   nntp_current_reply(nnp));
    goto EndComm;
  }

  /* figure the first article to read: if we've never seen this bboard,
     start with article 1 or 20 less than the limit, whatever is greater.
     Otherwise start with the first article the server knows about or
     the control file's last article, whatever is greater */
  if(ctllast == 0) ctllast = MAX(1, bb_last - 20);
  else ctllast = MAX(ctllast, bb_first);

  ret = slv_deliver(nnp, bbname, ctllast, bb_last, &newlast, outfile);
  (void) fprintf(fout, "%s %d\n", bbname, newlast);
  if(ferror(fout))
  {
    printf("nntp-slave: Newsgroup control file write error (%s)\n", 
	   strerror(errno));
    goto EndComm;
  }
  if(fclose(fout) == ERROR)
  {
    printf("nntp-slave: Newsgroup control file close error (%s)\n", 
	   strerror(errno));
    fout = NULL;
    goto EndComm;
  }
  if(rename(ctl_path_new, ctl_path) == ERROR)
  {
    printf("nntp-slave: Newsgroup control file rename error (%s)\n", 
	   strerror(errno));
    goto EndComm;
  }    
  return(ret);

 EndComm:
  if(fout) (void) fclose(fout);
  if(fin) (void) fclose(fin);
  return(ERROR);
}

int slv_deliver(nnp, bbname, first, last, newlast, inbox_path)

Nntp_stream *nnp;
char *bbname;
int first, last, *newlast;
char *inbox_path;
{
  FILE *fout = NULL;
  char cmdbuf[SLV_M_BUF];
  int ret;

  if(! (fout = fopen(inbox_path, "w")))
  {
    printf("nntp-slave: %s open error (%s)\n", inbox_path, strerror(errno));
    goto EndComm;
  }
  for(*newlast = first; *newlast <= last; ++(*newlast))
  {
    (void) sprintf(cmdbuf, "article %d", *newlast);
    if((ret = nntp_command(nnp, cmdbuf)) == ERROR)
    {
      slv_err_report(nnp);
      goto EndComm;
    }
    else if(nntp_current_reply_code(nnp) == ERR_NOARTIG ||
	    nntp_current_reply_code(nnp) == ERR_NOART) 
      continue;                                     /* skip missing articles */
    else if(nntp_current_reply_code(nnp) != OK_ARTICLE) 
    {
      printf("nntp-slave: Unexpected response \"%s\"", 
	     nntp_current_reply(nnp));
      goto EndComm;
    }
    (void) fputs(SLV_ART_DELIM, fout);
    if(ferror(fout))
    {
      printf("nntp-slave: Inbox file write error (%s)\n", strerror(errno));
      goto EndComm;
    }
    while((ret = nntp_list_end_p(nnp)) == 0)
    {
      (void) fprintf(fout, "%s\n", nntp_current_reply(nnp));
      if(ferror(fout))
      {
	printf("nntp_slave: Inbox file write error (%s)\n", strerror(errno));
	goto EndComm;
      }
    }
    if(ret == ERROR)
    {
      slv_err_report(nnp);
      goto EndComm;
    }
  }
  if(fclose(fout) == ERROR)
  {
    fout = NULL;
    printf("nntp_slave: Error closing inbox file (%s)\n", strerror(errno));
    goto EndComm;
  }
  return(OK);

 EndComm:
  if(fout) (void) fclose(fout);
  return(ERROR);
}

void slv_err_report(nnp)
register Nntp_stream *nnp;
{
  printf("nntp-slave: NNTP error (%s)\n", nntp_errstring(nnp));
  if(nntp_os_errorp(nnp)) 
    printf("nntp-slave: OS error %d (%s)\n", errno, strerror(errno));
}

int slv_open_ctl_outfile(ctl_path, fout) 

char *ctl_path;
FILE **fout;
{
  if(! (*fout = fopen(ctl_path, "w")))
  {
    printf("nntp-slave: Newsgroup control file open error (%s)\n", 
	   strerror(errno));
    return(ERROR);
  }
  return(OK);
}

/* system-dependent stuff * follows */

extern char *sys_errlist[];

char *strerror(n)
int n;
{
  return(sys_errlist[errno]);
}
