/* $Id: cwdcmd.c,v 1.0 1993/03/07 04:42:04 jimt Exp $ */

# include <stdio.h>
# include <string.h>
# include <malloc.h>
# include <sys/param.h>

# ifdef ROBUST_VERSION
main(argc, argv)
     int argc;
     char *argv[];
{
  char *intro = "\033EmAcScd ";
  char path[MAXPATHLEN];
  char *message;
  
  if (getwd(path) == NULL)
    {
      fprintf(stderr, "%s: %s\n", argv[0], path);
      exit(1);
    }
  
  if ((message = malloc(strlen(intro) + strlen(path) + 2)) == NULL)
    {
      fprintf(stderr, "%s: out of memory\n", argv[0]);
      exit(1);
    }
  
  strcpy(message, intro);
  strcat(message, path);
  strcat(message, "\n");
  write(1, message, strlen(message));
}
# else
main()
{
  static char message[MAXPATHLEN + 12] = "\033EmAcScd ";
  
  getwd(message + 9);
  strcat(message, "\n");
  write(1, message, strlen(message));
}
# endif
