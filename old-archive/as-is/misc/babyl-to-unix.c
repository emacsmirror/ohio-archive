/*To: unix-emacs@bbn.com
/*Date: 10 Nov 88 18:38:23 GMT
/*From: Edward Wilkinson <munnari!vuwcomp!dsiramd!pnamd!csvaxa!edward@uunet.uu.net>
/*Subject: C code to convert Babyl mailfiles to Unix mailfiles
/*
/*
/*Sometimes I need  to have old mailboxes  in Unix mail format to fiddle
/*with. Unfortunately, Gnumacs converts them  all to  Babyl format. So I
/*wrote  a  small  program,  b2m, to go  the  other way.  It  even saves
/*labels! I  don't know if  it's  any  use to anyone else -  here is is,
/*anyway. No man  page or Makefile, as it's  only small. As I don't know
/*the exact format of Babyl files, I'm sure the program can be improved.
/*
/*----------cut-here-------------cut-here-------------cut-here----------
/*
 * b2m - a filter for Babyl -> Unix mail files
 *
 * usage:	b2m < babyl > mailbox
 *
 * I find this useful whenever I have to use a
 * system which - shock horror! - doesn't run
 * Gnu emacs. At least now I can read all my
 * Gnumacs Babyl format mail files!
 *
 * it's not much but it's free!
 *
 *   Ed Wilkinson
 *   E.Wilkinson@massey.ac.nz
 *   Mon Nov 7 15:54:06 PDT 1988
 */

#include <stdio.h>
#include <time.h>
#include <strings.h>

#define TRUE  (1)
#define FALSE (0)

int header = FALSE, printing;
long ltoday;
char from[256], labels[256], data[256], *p, *today;

main(argc, argv)
char **argv;
{
  ltoday = time(0);
  today = ctime(&ltoday);

  if (gets(data))
    if (strcmp(data, "BABYL OPTIONS:")) {
      fprintf(stderr, "b2m: not a Babyl mailfile!\n");
      exit(-1);
    } else
      printing = FALSE;
  else
    exit(-1);
  if (printing)
    puts(data);

  while (gets(data)) {
    if (!strcmp(data, ""))
      exit(0);

    if (!strcmp(data, "*** EOOH ***") && !printing) {
      printing = header = TRUE;
      printf("From b2m %s", today);
      continue;
    }
    
    if (!strcmp(data, "")) {
      /* save labels */
      gets(data);
      p = strtok(data, " ,\r\n\t");
      strcpy(labels, "X-Babyl-Labels: ");

      while (p = strtok(NULL, " ,\r\n\t")) {
	strcat(labels, p);
	strcat(labels, ", ");
      }

      labels[strlen(labels) - 2] = '\0';
      printing = header = FALSE;
      continue;
    }

    if (!strlen(data) && header) {
      header = FALSE;
      if (strcmp(labels, "X-Babyl-Labels"))
	puts(labels);
    }
    
    if (printing)
      puts(data);
  }
}
/*----------cut-here-------------cut-here-------------cut-here----------
/*
/*Any suggestions and/or   bugfixes gratefully received at the   address
/*below.
/*-- 
/*Ed Wilkinson @ Computer Centre, Massey University, Palmerston North, NZ
/*uucp: {uunet,watmath!cantuar}!vuwcomp!csvaxa!edward   DTE: 530163000005
/*Janet/Greybook: E.Wilkinson@nz.ac.massey      Phone: +64 63 69099 x8587
/*CSnet/ACSnet/Internet: E.Wilkinson@massey.ac.nz    New Zealand = GMT+12
/*
/**/