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
 * etl_host.c
 *
 * Purpose:
 *   This file contains the functions used in managing the list of
 * hosts used at run time.  Hosts are managed to remember the talk
 * daemon type, and addresses of thier talk daemons.  Eventually,
 * place some in/out routines for handling a .etalk_hostsrc type file. 
 *
 * ::Header:: etalklib.h
 */

#include "etalklib.h"

extern int h_errno;

static struct HostObject *Q_first = NULL;
static struct HostObject *Q_last  = NULL;

/*
 * Print h_errno in human readable fassion...
 */
static void print_hosterr(machine)
     char *machine;
{
  switch(h_errno) {
  case HOST_NOT_FOUND:
    printf("Host %s unknown\n", machine);
    break;
  case NO_ADDRESS:
    printf("Host %s exists but has no address.\n", machine);
    break;
  case NO_RECOVERY:
    printf("Name server error while looking for %s\n", machine);
    break;
  case TRY_AGAIN:
    printf("Name server error looking for %s, try again later.\n", machine);
    break;
  default:
    printf("Unknown error looking for %s\n", machine);
    }
}

/*
 * Function: append_host
 *
 * Take the host structure and tac it onto the end of the local
 * static list.  Be sure to recycle duplicate definitions of hosts
 * 
 * Parameters: host - the host to append
 *
 * History:
 * eml 3/1/94
 */
static struct HostObject *append_host(host)
     struct hostent *host;
{
  struct HostObject *new;
  /*
   * first, quick search to see if we know him or not
   */
  new = Q_first;
  while(new && strcmp(new->name, host->h_name))
    new = new->next;
  
  if(new) {
    if(verbose)
      printf("Recycling host %s\n", new->name);
    return new;
  }

  new = (struct HostObject *)malloc(sizeof(struct HostObject));
  
#if OTALK_ONLY == 1
  new->type     = OTALKD;
#else
  new->type     = NTALKD;	/* assume this first */
#endif
  new->rc       = RC_new;	/* assume a new host */
  new->next     = NULL;
  
  new->name     = (char *)malloc(strlen(host->h_name) + 1);
  strcpy(new->name, host->h_name);
  new->addr.sin_family = AF_INET;
  new->addr.sin_port = 0;
  bcopy(host->h_addr, (char *)&new->addr.sin_addr, host->h_length);
  new->addr_len = host->h_length;

  if(!Q_first)
    Q_first    = new;
  else
    Q_last->next = new;
  Q_last       = new;
    
  if(verbose)
    printf("Returning new host %s\n", new->name);
  
  return new;
}

/*
 * Function: HOST_gen_host
 *
 * Create a host object based of some machine name.  Important part here
 * is the address field that is filled in, and keeping track of OTALK hosts.
 * 
 * Parameters: machine - character name of machine
 *
 * History:
 * eml 3/1/94
 */
struct HostObject *HOST_gen_host( machine )
     char *machine ;
{
  struct hostent *host;

  host = gethostbyname( machine );

  if(!host) 
    {
      print_hosterr(machine);
      return NULL;
    }

  return append_host(host);
}     

/*
 * Function: HOST_gen_host_by_addr
 *
 * Create a host object based of some machine name.  Important part here
 * is the address field that is filled in, and keeping track of OTALK hosts.
 * 
 * Parameters: machine - character name of machine
 *             size    - size of the address
 *
 * History:
 * eml 3/1/94
 */
struct HostObject *HOST_gen_host_by_addr(addr, size)
     struct sockaddr *addr;
     int size ;
{
  struct hostent *host;
  struct in_addr  addr_in;

  addr_in = ((struct sockaddr_in *)addr)->sin_addr;

  /* Potential bug in linux:
   *  Technically, addr_in is always 4 bytes (long) however,
   * for expandability, INET makes their fns pass it about.  The size
   * on linux  0.99 didn't give back the right number, and as a result,
   * this call always failed until the constant was put in.
   */
  host = gethostbyaddr( (const char *)&addr_in, sizeof(addr_in), AF_INET );

  if(!host) 
    {
      print_hosterr("<sockaddr structure>");
      print_sockaddr(addr);
      printf(": size %d\n", size);
      return NULL;
    }

  return append_host(host);
}     

/*
 * Function: HOST_gen_local_host
 *
 * Special function to return a host struct of the machine we are running
 * on.  Use this with short circut to access information about ourselves.
 * 
 * Parameters:
 *
 * History:
 * eml 3/1/94
 */

struct HostObject *HOST_gen_local_host()
{
  static struct HostObject *new = NULL;
  char                      buffer[100];
  int                       result;

  if(!new)
    {
      result = gethostname(buffer, 100);

      if(result < 0) {
	perror("ET_gen_local_host: gethostname:");
	return 0;
      }
      new = HOST_gen_host(buffer);
    }
  return new;
}


/*
 * Function: HOST_print
 *
 * Print a formatted list of all hosts currently in our table.
 * 
 * Parameters:
 *
 * History:
 * eml 4/21/94
 */
void HOST_print()
{
  static char *types[] = { "None", "OTALK", "NTALK", "GTALK" };
  static char *rcf[] = { "System", "Local", "New", "Change" };
  struct HostObject *t;

  printf("Name\t\t\tType\tRC\tAddress\n");

  for(t = Q_first; t; t = t->next)
    {
      printf("%-23.23s\t%s\t%s\t",
	     t->name?t->name:"Unknown",
	     types[t->type+1],
	     rcf[t->rc]);
      print_sockaddr((struct sockaddr*)&t->addr);
      printf("\n");
    }
}
