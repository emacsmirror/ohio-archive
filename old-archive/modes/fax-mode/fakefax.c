/* fakefax -- invoke yet another fax sending program.

   Copyright (C) 1995 Ralph Schleicher	*/

/* This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of
   the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include <sys/types.h>

#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#else
extern int waitpid ();
#endif
#ifndef WEXITSTATUS
#define WEXITSTATUS(status) ((unsigned) (status) >> 8)
#endif

#if STDC_HEADERS
#include <stdlib.h>
#include <string.h>
#else /* not STDC_HEADERS */
#if !HAVE_STRCHR
#define strchr index
#define strrchr rindex
#endif
extern char *strchr ();
extern char *strrchr ();
#if !HAVE_STRERROR
extern char *sys_errlist[];
#define strerror(error) sys_errlist[error]
#endif
#endif /* not STDC_HEADERS */

#include <stdio.h>
#if HAVE_FCNTL_H
#include <fcntl.h>
#else
extern int open ();
#endif
#if HAVE_UNISTD_H
#include <unistd.h>
#else
extern int read ();
extern int write ();
extern int close ();
#endif

#include <errno.h>

#ifndef O_BINARY
#define O_BINARY 0
#endif

#define CHUNK 4096


int
main (arg_count, arg_vec)
int arg_count;
char **arg_vec;
{
  char *prog_name;
  int arg_index;
  char file_name[L_tmpnam];
  int file_desc;
  char buffer[CHUNK];
  int child;
  int status;
  int result;

  if (arg_count < 2)
    exit (EXIT_FAILURE);

  prog_name = strrchr (arg_vec[0], '/');
  if (prog_name)
    ++prog_name;
  else
    prog_name = arg_vec[0];

  if (!tmpnam (file_name))
    {
      if (errno == 0)
	errno = EINVAL;

      fprintf (stderr, "%s: %s\n",
	       prog_name, strerror (errno));

      exit (EXIT_FAILURE);
    }

  file_desc = open (file_name, O_CREAT | O_WRONLY | O_BINARY, 0644);
  if (!file_desc)
    {
      fprintf (stderr, "%s:%s: %s\n",
	       prog_name, file_name, strerror (errno));

      exit (EXIT_FAILURE);
    }

  while (1)
    {
#ifdef EINTR
      do
	{
	  result = read (STDIN_FILENO, buffer, CHUNK);
	}
      while (result < 0 && errno == EINTR);
#else
      result = read (STDIN_FILENO, buffer, CHUNK);
#endif
      if (result == 0)
	break;

      if (result < 0)
	{
	  fprintf (stderr, "%s:%s: %s\n",
		   prog_name, file_name, strerror (errno));

	  close (file_desc);
	  unlink (file_name);

	  exit (EXIT_FAILURE);
	}

      if (write (file_desc, buffer, result) != result)
	{
	  fprintf (stderr, "%s:%s: %s\n",
		   prog_name, file_name, strerror (errno));

	  close (file_desc);
	  unlink (file_name);

	  exit (EXIT_FAILURE);
	}
    }

  close (file_desc);

  ++arg_vec;
  --arg_count;

  for (arg_index = 1; arg_index < arg_count; ++arg_index)
    if (arg_vec[arg_index][0] == '-' && arg_vec[arg_index][1] == 0)
      arg_vec[arg_index] = file_name;

  child = fork ();
  switch (child)
    {
    case 0:

      if (arg_vec[0][0] == '/'
	  || (arg_vec[0][0] == '.'
	      && (arg_vec[0][1] == '/'
		  || (arg_vec[0][1] == '.' && arg_vec[0][2] == '/'))))
	execv (arg_vec[0], arg_vec);
      else
	execvp (arg_vec[0], arg_vec);

      /* Fall through. */

    case -1:

      fprintf (stderr, "%s:%s: %s\n",
	       prog_name, arg_vec[0], strerror (errno));

      unlink (file_name);

      exit (EXIT_FAILURE);
    }

#ifdef EINTR
  do
    {
      result = waitpid (child, &status, 0);
    }
  while (result < 0 && errno == EINTR);
#else
  waitpid (child, &status, 0);
#endif

  unlink (file_name);

  /* Return the exit status of the child process to the caller.	 */

  exit (WEXITSTATUS (status));
}


/*
 * local variables:
 * compile-command: "gcc -Wall -O -o fakefax fakefax.c"
 * end:
 */
