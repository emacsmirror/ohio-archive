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
 * etl_dev.c
 *
 * Purpose:
 *   This file contains device functions for handling IO on all types
 * of descriptors.  It manages a static list of all descriptors and
 * thier attributes, has a generic selector, and then reads in that
 * data and calls the appropriate object functions.  Also available
 * are the functions sending output to a given object, reguardless of
 * it's type.
 *
 * ::Header:: etalklib.h
 */

#include "etalklib.h"

static struct InputDevice *Q_first = NULL;
static struct InputDevice *Q_last  = NULL;

/*
 * This variable will break from read loops when new devices are created
 * as a result of a previous read.
 */
static int new_device = 0;

static int recursive_exit = FALSE;


/*
 * Function: ET_select_all
 *
 * Main loop to the whole program.  Simply wait for input on any of
 * our great list of sockets, and when something shows up, run the
 * associated readme function.
 * Timeouts are handled on a one second granularity basis, and the IO
 * struct is modified over time untill a timeout occurs.
 * Return value simply is Success if something comes in, and Fail if not.
 * 
 * Parameters: Ctxt  - context of the program to pass to READMEs Must
 *                     be NULL because lib fns don't include etalk.
 *             reton - return on a match of this value.  Good for UDP stuff.
 * History:
 * eml 3/1/94
 * eml 4/6/94  Decrementing timeouts added, and reton match.
 */
int ET_select_all(Ctxt, reton)
     void               *Ctxt;
     struct InputDevice *reton;
{
  struct InputDevice *Q;	/* Q loop variable                       */
  fd_set              mask;	/* mask used in select function          */
  int                 rval;	/* return value of some functions        */
  int                 tocnt;	/* count of timeouts encountered         */
  struct timeval      timeout;	/* timeout passed into select            */
  int                 returnflag = FALSE; /* flag for returning on match */

  while(1) {			/* loop forever on devices   */
    if(reton && recursive_exit)	/* check recursive exit flag */
      return Fail;		/* return that instant!      */
    else
      recursive_exit = FALSE;

    new_device = 0;		/* rezero new device flag  */
    FD_ZERO(&mask);		/* init mask               */
    rval = 0;			/* init return val/ fd #   */
    tocnt = 0;			/* init timeout count      */
    Q = Q_first;		/* setup for loop          */
#ifdef DEBUG_2
    printf(" * Adding to mask: ");
#endif
    while(Q) {
      /* Only add to mask if dev has a reader function and is not dead.
       */
      if((Q->readme) && (Q->state != DEAD)) {
	FD_SET(Q->fd, &mask);
#ifdef DEBUG_2
	printf("%s, ", ET_dev_name(Q));
#endif
	/* get greatest file descriptor for select.
	 */
	if(rval <= Q->fd) 
	  rval = Q->fd + 1;
	if(Q->timeout > 0)
	  tocnt++;
      }
      Q = Q->next;
    }
#ifdef DEBUG_2
    printf("\n");
#endif
    
    if(tocnt)
      {
	timeout.tv_sec = 1;
	timeout.tv_usec = 0;
	/* Select with one second delay...
	 */
	rval = select(rval, &mask, NULL, NULL, &timeout);
      }
    else
      /* Select with infinite delay
       */
      rval = select(rval, &mask, NULL, NULL, NULL);

    if(rval < 0) {
      perror("Selecting main loop");
      return Fail;
    }
    if(rval == 0) {
#ifdef DEBUG_2
      printf(" * Select Timeout\n");
#endif
      /* Loop on each item, and decrement their timeout counters...
       */
      Q = Q_first;
      while(Q) {
	if((Q->timeout > 0) && (Q->state != DEAD))
	  {
#ifdef DEBUG_2
	    printf(" * * Timeout for %s\n", ET_dev_name(Q));
#endif
	    Q->timeout--;
	    if(!Q->timeout) 
	      {
		if(Q->timefn)
		  {
		    Q->timefn(Ctxt, Q);
		  } 
		/* Check for match on reton variable
		 */
		if(Q == reton) returnflag = TRUE;
	      }
	    /* No timeout, see if there is something to do each sec.
	     */
	    else
	      {
		if(Q->timemsg)
		  Q->timemsg(Ctxt, Q);
	      }
	  }
	Q = Q->next;
      }
#ifdef DEBUG_2
      if(reton)
	{
	  printf(" * * * Return flag failing is %d\n", returnflag);
	}
#endif
      if(returnflag) return Fail;
    } else {
      Q = Q_first;
      while(Q) {
	if((Q->readme) && (Q->state != DEAD))
	  {
#ifdef DEBUG_2
	    printf(" * Output check for host %s : ", ET_dev_name(Q));
#endif
	    
	    if(FD_ISSET(Q->fd, &mask)) { /* check for each socket w/ io avail*/
#ifdef DEBUG_2
	      printf("Executing README.\n");
#endif
	      Q->readme(Ctxt, Q);		/* and run the catch program.*/
	      if(Q == reton) returnflag = TRUE;	/* return if waiting...      */
	      if(new_device) break; /* break when new dev created. */
	    } 
#ifdef DEBUG_2
	    else {
	      printf("No output waiting.\n");
	    }
#endif
	  }
	Q = Q->next;
      }
#ifdef DEBUG_2
      if(reton)
	{
	  printf(" * * * Return flag success is %d\n", returnflag);
	}
#endif
      if(returnflag) return Success;
    }
  }
}


/*
 * Function: ET_end_recusion
 *
 * Set a local variable so that any recursive calls to select_all
 * are canceled right away upon return.
 * 
 * Parameters: none
 *
 * History:
 * eml 4/15/94
 */
void ET_end_recursion()
{
  recursive_exit = TRUE;
}

/*
 * Function: ET_clean_dev
 *
 * Do bookeeping on a device when it is shut down
 * 
 * Parameters: io - the device
 *
 * History:
 * eml 4/15/94
 */
void ET_clean_dev(io)
     struct InputDevice *io;
{
  /* ignore any nulls, make other procs much simpler */
  if(!io) return;
  /* never close the TTY parts by accident */
  if(io->fd <= 2) return;

  io->state = DEAD;
  close(io->fd);
  io->fd = 1;			/* if anyone write, goes to stdout */
  io->readme = NULL;		/* and remove any functions on it. */
}

/*
 * Function: ET_close_all
 *
 * Close all ports in the Q list.  There should be several duplicates,
 * but close doesn't care too much.
 * 
 * Parameters: Ctxt - unused context field.
 *
 * History:
 * eml 3/1/94
 */
void ET_close_all(Ctxt)
     void *Ctxt;
{
  struct InputDevice *Q;

  Q = Q_first;
  while(Q) {
    close(Q->fd);
    Q = Q->next;
  }
}

/*
 * Function: ET_clean
 *
 * Go through all devices and free all ports labelled as DEAD
 * 
 * Parameters:
 *
 * History:
 * eml 4/21/94
 */
void ET_clean()
{
  struct InputDevice *io;

  io = Q_first;
  while(io)
    {
      if(io->state == DEAD)
	{
	  if(verbose)
	    printf("Freeing io %s ...\n", ET_dev_name(io));

	  if(io == Q_first)
	    {
	      if(io == Q_last)
		{
		  Q_first = Q_last = NULL; /* nothing left */
		}
	      else
		{
		  io->next->prev = NULL; /* just the first guy */
		  Q_first = io->next;
		}
	    }
	  else
	    {
	      if(io == Q_last)
		{
		  io->prev->next = NULL; /* the last guy */
		  Q_last = io->prev;
		}
	      else
		{
		  io->next->prev = io->prev; /* somewhere in the middle */
		  io->prev->next = io->next;
		}
	    }
	  {
	    struct InputDevice *t = io;
	    io = io->next;
	    free(t);		/* free him up. */
	  }
	}
      else
	io = io->next;
    }
}

/*
 * Function: ET_gen_iodev
 *
 * Malloc space for one InputDevice, and fill in the fields.
 * 
 * Parameters: type - udp,tcp,tty
 *             fd - the file descriptor
 *             raddr - remote address (to be sent to.)
 * History:
 * eml 3/1/93 
 */
struct InputDevice *ET_gen_iodev(type, fd, raddr)
     enum InputDeviceType type;
     int                  fd;
     struct sockaddr_in  *raddr;
{
  struct InputDevice *new;
  
  /* first, don't keep allocating new structures.  Only for UDP for now
   */
  if(type == IO_UDP)
    {
      new = Q_first;
      while(new)
	{
	  if((new->type == type) && (new->fd == fd) && raddr &&
	     (!memcmp(&new->raddr, raddr, sizeof(struct sockaddr_in))))
	    break;
	  new = new->next;
	}
      if(new) 
	{
	  if(verbose)
	    printf("Recycling device %s\n", ET_dev_name(new));

	  return new;
	}
    }

  new_device++;

  new = (struct InputDevice *)malloc(sizeof(struct InputDevice));

  if(!new)
    {
      perror("malloc failure!!! ");
      return Fail;
    }

  if(verbose)
    printf("Creating IO device on descriptor %d\n", fd);

  new->type     =  type;
  new->state    =  EMPTY;
  new->name     =  NULL;
  new->sendc    =  0;
  new->recvc    =  0;
  if(raddr)
    new->raddr  = *raddr;
  else
    bzero(&new->raddr, sizeof(new->raddr));
  new->fd       =  fd;
  new->host     =  NULL;
  new->readme   =  NULL;
  new->timeout  =  0;
  new->timefn   =  NULL;
  new->timemsg  =  NULL;
  new->next     =  NULL;

  if(!Q_last) 
    Q_first    = new;
  else
    Q_last->next = new;
  new->next    = NULL;
  new->prev    = Q_last;
  Q_last       = new;

  return new;
}     


/*
 * Function: ET_portable_address
 *
 * Return a network sendable address struct from a file descriptor.
 * 
 * Parameters: dev - The input device with the desciptor attached.
 *
 * History:
 * eml 4/1/94
 */
struct sockaddr_in ET_portable_address(dev)
     struct InputDevice *dev;
{
  int                t;
  struct sockaddr_in addr;
  /*
   * and do the get name thing.
   */
  t = sizeof(addr);
  if(getsockname(dev->fd, (struct sockaddr *)&addr, &t) != 0) 
    {
      perror("getsockname");
      exit(1);
    }
  return addr;
}


/*
 * Function: ET_tty
 *
 * Simply generate an Input struct from the TTY. No addressing and
 * the like should be used.
 * 
 * Parameters:
 *
 * History:
 * eml 3/1/94
 */
struct InputDevice *ET_tty()
{
  static struct InputDevice *tty = NULL;

  if(tty) return tty;

  tty = ET_gen_iodev(IO_TTY, 0, NULL);
  
  tty->state = CONNECTED;
  tty->name = "TTY";

  return tty;
}


/*
 * Function: ET_send
 *
 * Take an Input device, which contains a type, and send something to
 * them based on the type (udp, tcp etc).
 * 
 * Parameters: dev - Input device
 *             buffer - the buffer to send.
 *             size - the size of the buffer to send.
 * History:
 * eml 3/1/94
 */
int ET_send(dev, buffer, size)
     struct InputDevice *dev;
     void *buffer;
     int size;
{
#ifdef DEBUG_2
  printf(" * Sending on descriptor %d\n", dev->fd);
#endif

  switch(dev->type) {
  case IO_TTY:
    if(write(1, buffer, size) < 0) {
      perror("ET_send: write:");
      return Fail;
    }
    dev->sendc += size;
    return Success;
  case IO_TCP:
    if(write(dev->fd, buffer, size) < 0) {
      perror("ET_send: write:");
      return Fail;
    }
    dev->sendc += size;
    return Success;
  case IO_UDP:
    if(sendto(dev->fd, buffer, size, 0, (struct sockaddr *)&dev->raddr, 
	      sizeof(struct sockaddr_in)) < 0) {
      perror("ET_send: sendto");
      return Fail;
    }
    dev->sendc += 1;		/* count in terms of packets */
    return Success;
  default:
    printf("ET_recv: device has wrong type %d\n", dev->type);
  }
  return Fail;
}

/*
 * Function: ET_recv
 *
 * Universal read function for input devices.  Allows some special
 * dealins with UDP which can receive from someone other than the
 * selected individual.
 * 
 * Parameters: dev - the input device
 *             buffer - the buffer to receive into
 *             size - size of said buffer
 * History:
 * em; 3/1/94
 */
int ET_recv(dev, buffer, size)
     struct InputDevice *dev;
     void *buffer;
     int size;
{
  int sinsize;
  int retsize;

#ifdef DEBUG_2
  printf(" * Receiving on descriptor %d\n", dev->fd);
#endif

  switch(dev->type) {
  case IO_TTY:
    {
      if((buffer = gets(buffer)) == 0) {
	perror("ET_recv: gets:");
	return Fail;
      }
      dev->recvc += strlen(buffer);
      return strlen(buffer);
    }
  case IO_TCP:
    if((retsize = read(dev->fd, buffer, size)) < 0) {
      perror("ET_recv: read:");
      return Fail;
    }
    dev->recvc += retsize;
    return retsize;
  case IO_UDP:
    sinsize = sizeof(struct sockaddr_in);

    if((retsize = recvfrom(dev->fd, buffer, size, 0, 
			  (struct sockaddr *)&dev->raddr, 
			  &sinsize)) < 0) {
      perror("ET_recv: recvfrom:");
      return Fail;
    }
    dev->recvc += 1;		/* in terms of packets. */
    return retsize;
  default:
    printf("ET_recv: device has wrong type %d\n", dev->type);
  }
  return Fail;
}


/*
 * Function: ET_dev_name
 *
 * Take an input device, and return the appropriate name based on info
 * available.
 * 
 * Parameters: io - the input device.
 *
 * History:
 * eml 4/12/94
 */
char *ET_dev_name(io)
     struct InputDevice *io;
{
  static char *typestr[] = { "TTY", "TCP", "UDP" };
  static char short_term_buffer[80];

  if(!io) return "<NULL>";

  if(io->name) return io->name;

  if(io->host && io->host->name) 
    {
      sprintf(short_term_buffer, "%s to %s",
	      typestr[io->type], io->host->name);
      return short_term_buffer;
    }
  
  sprintf(short_term_buffer, "%s on descriptor %d",
	  typestr[io->type], io->fd);
  return short_term_buffer;
}


/*
 * Function: ET_print_device
 *
 * Prints out the given input device
 *
 * Returns:     void  - 
 * Parameters:  io - the input device to print
 *
 * History:
 * eml	May 19, 1994	Created
 */
void ET_print_device(io)
     struct InputDevice *io;
{
  static char *typestr[] = { "TTY", "TCP", "UDP" };
  static char *state[] = { "EMPTY", "CONNECT", "LISTEN", "WAIT", "IDLE",
			     "DEAD" };

  printf("%-8.15s\t%s\t%s\t%d\t%s\t%ld\t%ld\t%s\n",
	 ET_dev_name(io),
	 typestr[io->type],
	 state[io->state],
	 io->fd, io->readme?"TRUE":"FALSE",
	 io->sendc, io->recvc,
	 (io->host && io->host->name)?
	 io->host->name:"<NULL>");  
}

/*
 * Function: print_q_list
 * 
 * print a formatted list of all open input devices with thier names.
 * 
 * Parameters: None
 *
 * History:
 * eml 4/15/94
 */
void ET_print_q_list()
{
  struct InputDevice *Q;

  printf("Name\t\tType\tState\tDescptr\tActive\tSent\tRecv\tHostname\n");

  Q = Q_first;
  while(Q) {
    ET_print_device(Q);
    Q = Q->next;
  }
}

/*
 * Function: print_sockaddr
 *
 * print out a socket address.  For debugging purposes.
 * 
 * Parameters: addr - the addr to print
 *
 * History:
 * eml 3/1/94
 */
void print_sockaddr(addr)
     struct sockaddr *addr;
{
  printf("F:(%d) P:(%d) IN:(%ld)", 
	 ((struct sockaddr_in *)addr)->sin_family, 
	 /* ports are alwas in net order...
	  */
	 ntohs(((struct sockaddr_in *)addr)->sin_port),
	 ((struct sockaddr_in *)addr)->sin_addr.s_addr);
}

/*
 * Function: print_swapped_sockaddr
 *
 * Prints out the sockaddr which is in network order by rotating the ints.
 *
 * Returns:     void  - 
 * Parameters:  addr - address to print
 *
 * History:
 * eml	May 19, 1994	Created
 */
void print_swapped_sockaddr(addr)
     struct sockaddr *addr;
{
  printf("F:(%d) P:(%d) IN:(%ld)", 
	 ntohs(((struct sockaddr_in *)addr)->sin_family),
	 ntohs(((struct sockaddr_in *)addr)->sin_port),
	 ((struct sockaddr_in *)addr)->sin_addr.s_addr);
}
