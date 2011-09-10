/*
 * Copyright (C) 1994 Free Software Foundation
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, you can either send email to this
 * program's author (see below) or write to:
 *
 *              The Free Software Foundation, Inc.
 *              675 Mass Ave.
 *              Cambridge, MA 02139, USA. 
 *
 * Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
 *
 * et_rc.c
 *
 * Purpose:
 *  To read a .etc.hosts file, which will contain the type of daemon
 * on the remote machine.
 *
 * Format is:
 * number.id machine.name daemon_type
 *
 * where:
 *  number.id    is optional, and is the numeric address. "128.119.28.53"
 *  machine.name is required, and follows at least one space.
 *                  and is of the form "choochoo.dmc.com"
 *  daemon_type is the string GTALKD NTALKD or OTALKD,
 *                  where GTALKD has not been written,
 *                        NTALKD is BSD talk, and
 *                        OTALKD is the old version of talk found on suns.
 *
 * ::Header:: etalk.h 
 */
#include "etalklib.h"
#include "etalk.h"

static char *names[] = { "OTALKD", "NTALKD", "GTALKD" };


/*
 * Function: parse_file
 *
 * Read in file stream, and just load in those host entities into the
 * host's table.
 * 
 * Parameters: file   - the file to parse from
 *             rctype - the type of RC file read from.
 * History:
 * eml 5/2/94
 */
static void parse_file(file, rctype)
     FILE           *file;
     enum RCFileType rctype;
{
  char  buffer[100];		/* buffer to read into               */
  char *s;			/* search pointer                    */
  char *hostname;		/* pointer to the hostname in buffer */
  char *daemon;			/* pointer to daemone type in buffer */
  int   i;			/* index                             */
  int   linenum = 0;		/* currently read line number        */
  struct HostObject *host;	/* new host to be created            */
  
  while(fgets(buffer, 100, file))
    {
      linenum++;
      s = buffer;		/* initialize parts.                 */
      hostname = NULL;
      daemon = NULL;

      if(*s == '#')
	{
#ifdef DEBUG_2
	  printf("Line %d: comment.\n", linenum);
#endif
	  continue;	/* # means comment.                  */
	}
      /* Pass over the first string (the number part)
       */
      
      if(!strtok(buffer, " \t"))
	{
#ifdef DEBUG_2
	  printf("Line %d: blank.\n", linenum);
#endif
	  continue;	/* EOL */
	}
      
      /* Save this place, and read over it, and place a NULL
       */
      hostname = strtok(NULL, " \n");
      daemon = strtok(NULL, " \n");

      /*
       * if we don't have hostname or daemon types, then there is no reason
       * to load that host in.
       */
      if(hostname && daemon)
	{
#ifdef DEBUG_2
	  printf("Line %d: Loading new host %s type %s\n", 
		 linenum, hostname, daemon);
#endif
	  /*
	   * If the host already exists, then that host is returned by
	   * this function, therefore, changes in daemon type will hold.
	   */
	  host = HOST_gen_host( hostname );

	  if(host)
	    {
	      i = 0;
	      while(i < (sizeof(names) / (sizeof(char *))))
		{
		  if(!strcmp(daemon, names[i]))
		    {
		      host->type = i;
		      host->rc   = rctype;
		      break;
		    }
		  i++;
		}
	      if(i ==  (sizeof(names) / (sizeof(char *))))
		{
		  printf("Line %d: Daemon type %s is unknown.\n", 
			 linenum, daemon);
		}
	    }
	  else
	    {
	      printf("Line %d: Host %s is unknown.\n", linenum, hostname);
	    }
	}
      else if(verbose)
	{
	  if(hostname)
	    {
	      printf("Line %d: Incomplete line encountered.\n", linenum);
	    }
	}
    }
}


/*
 * Function: RC_load_all_hosts
 *
 * Read in every host in the available hosts RC files, and define them.
 * These machines should be small in number, and represent those machines
 * which are different only.  Normal machines need no such special treatment.
 * 
 * Parameters: None
 *
 * History:
 * eml 5/2/94
 */
void RC_load_all_hosts()
{
  FILE *stream;			/* open file stream    */
#ifdef LOCAL_RC
  char  lfile[100];
#endif

#ifdef SYSTEM_RC
  if(verbose)
    printf("Reading hosts from system file.\n");

  stream = fopen(SYSTEM_RC, "ra");
  if(stream)
    {
      parse_file(stream, RC_system);
      fclose(stream);
    }
#ifdef DEBUG_2
  else
    {
      printf("Failed to open host RC file %s\n", SYSTEM_RC);
    }
#endif /* debug_2 */
#endif /* system_rc */

#ifdef LOCAL_RC
  if(verbose)
    printf("Reading hosts from user file.\n");

  strcpy(lfile, (char *)getenv("HOME"));
  if(lfile[strlen(lfile)] != '/')
    {
      strcat(lfile, "/");
    }
  strcat(lfile, LOCAL_RC);

  stream = fopen(lfile, "ra");
  if(stream)
    {
      parse_file(stream, RC_local);
      fclose(stream);
    }
  else if(verbose)
    {
      printf("Failed to open host RC file %s\n", lfile);
    }
#endif /* local_rc */
    return;
}
