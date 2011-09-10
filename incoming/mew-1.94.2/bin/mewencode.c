/*
 * mewencode, mewdecode, and mewcat --- MIME encoding for Mew
 *
 * Author:  Kazu Yamamoto <Kazu@Mew.org>
 * Created: Dec  8, 1994
 * Revised: Dec 12, 1998
 *
 * Code:
 */

static char version_message[] =	"mewencode version 0.14 981219 Kazu Yamamoto";

#include "getopt.h"
#include <stdio.h>
#include <string.h>
#if defined(_WIN32) || defined(OS2)
#include <fcntl.h>
#endif

#define ENCODE_NAME "mewencode"
#define DECODE_NAME "mewdecode"
#define CAT_NAME    "mewcat"

#define ENCODE   1
#define DECODE   2

#define SUCCESS 0
#define ERROR 1

#define FILESEP    '/'
#define FILESEPLEN 1

#define LINELEN 71

#define BASE64	'b'
#define QP     	'q'
#define GZIP64	'z'

#define ON  1
#define YES 1
#define OFF 0
#define NO  0

#define CR 13
#define LF 10

#define OOB -1
#define EOP -2

#define PADDING '='

/*
 * "b" is ignored on UNIX.
 */
#define FDREAD	"rb"
#define FDWRITE	"wb"

#define STRCMP(str1, str2) strncmp(str1, str2, sizeof(str2) - 1)

/*
 * long name convention for option
 */

struct option longopts [] = {
	{"encode", 	     0, 0, 'e'},
	{"decode", 	     0, 0, 'd'},
	{"base64",           0, 0, 'b'},
	{"quoted-printable", 0, 0, 'q'},
	{"gzip64",	     0, 0, 'z'},
	{"length", 	     1, 0, 'l'},
	{"text",	     0, 0, 't'},
	{"help",	     0, 0, 'h'},
	{"version",	     0, 0, 'v'},    
	{0, 0, 0, 0}
};

void
usage(progname)
	char* progname;
{
	fprintf(stderr, "usage: %s [-e|-d] [-b|-q|-g] [-l length] [-t] [infile [outfile]]\n", progname);
}

char *help_message[] = {
	" -e  --encode           Encoding <infile>",
	" -d  --decode           Decoding <infile> instead of encoding",
	"                        Decoding is the default when called with decoding", 
	"                        program name.", 
	" -b  --base64           MIME base64 en/decoding.",
	" -q  --quoted-printable MIME quoted-printable en/decoding.",
	" -g  --gzip64           MIME gzip64 en/decoding(not yet specified in RFC).",
	" -z                     Same as -g.",
	" -l  --length           Line length into which base64/quoted-printable/gzip64 ",
	"                        encoding truncate. The default value is 71.",
	" -t  --text             On base64/gzip64 encoding, local newline is treated",
	"                        as CRLF.",
	"                        On base/gzip64 decoding,any newline is translated",
	"                        into local newline.",
	"                        Specify this option only when the input is a line",
	"                        based object(e.g. Content-Type: is text/plain or",
	"                        application/postscript).",
	" -h  --help             Display this help message.",
	" -v  --version          Display the version.",
	"",
	"Default is Encoding, Base64, Length = 71, Binary.",
	"If <infile> is \"-\", it means the standard input.",
	"If <outfile> is \"-\", it means the standard output.",
	0
};

void
help(progname)
	char *progname;
{
	char **p = help_message;

	fprintf(stderr, "help: %s\n\n", progname);
	fprintf(stderr, " A MIME encoder/decoder.\n\n");
	usage(progname);
	while (*p) fprintf(stderr, "%s\n", *p++);
}

void
version(progname)
	char *progname;
{
	fprintf(stderr, "version of %s: %s\n", progname, version_message);
}

/*
 * utils
 */

char *
mewbasename(filename)
	char *filename;
{
	char *p;

	if ((p = strrchr(filename, FILESEP)) != NULL)
		filename = p + FILESEPLEN;
	return filename;
}

/*
 * basic input/output
 */

/*
 * 8bit input with line canonicalization
 */

int
Getc(stream, text)
	FILE *stream;
	int text;
{
	static int Incr = OFF, Ineof = OFF;
	int c;
    
	if (!text)
		return(getc(stream));
    
	if (Ineof)
		return(EOF);
	c = getc(stream);
    
	if (Incr) {
		Incr  = OFF;	
		switch (c) {
		case EOF:
			Ineof = ON;
			return(LF);
			break;
		case LF:
			return(LF);
			break;
		default:
			ungetc(c, stream);
			return(LF);
			break;
		}
	}
	if (c == CR) {
		Incr = ON;
		return(CR);
	}
	if (c == LF) {
		ungetc(c, stream);
		Incr = ON;
		return(CR);
	}
	if (c == EOF)
		Ineof = ON;
	return(c);
}

/*
 * lineless 'ascii' input
 */

static char base256[] = {
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
    
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
      /*                                                -                / */
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB, 62, OOB,OOB,OOB, 63,
      /*  0   1   2   3    4   5   6   7    8   9                =        */
	 52, 53, 54, 55,  56, 57, 58, 59,  60, 61,OOB,OOB, OOB,OOB,OOB,OOB,
      /*      A   B   C    D   E   F   G    H   I   J   K    L   M   N   O*/
	OOB,  0,  1,  2,   3,  4,  5,  6,   7,  8,  9, 10,  11, 12, 13, 14,
      /*  P   Q   R   S    T   U   V   W    X   Y   Z                     */
	 15, 16, 17, 18,  19, 20, 21, 22,  23, 24, 25,OOB, OOB,OOB,OOB,OOB,
      /*      a   b   c    d   e   f   g    h   i   j   k    l   m   n   o*/
	OOB, 26, 27, 28,  29, 30, 31, 32,  33, 34, 35, 36,  37, 38, 39, 40,
      /*  p   q   r   s    t   u   v   w    x   y   z                     */
	 41, 42, 43, 44,  45, 46, 47, 48,  49, 50, 51,OOB, OOB,OOB,OOB,OOB, 
};

int
GetChar(FILE *stream, int cannot_be_eof)
{
	int c, ret;
	static int Ineof = OFF;

	if (Ineof == ON)
		return EOF;
    
	do {
		c = getc(stream);
	} while ( c == CR || c == LF);

	if (c == EOF) {
		if (cannot_be_eof == YES) {
			fprintf(stderr, "Error: base64 decoder saw premature EOF.");
			exit(ERROR);
		}
		Ineof = ON;
		return(EOF);
	}

	if (c == PADDING)
		return(EOP);
	
	if ((ret = base256[c]) == OOB) {
		fprintf(stderr, "base64 decoder saw an illegal character.");
		exit(ERROR);
	}
	
	return(ret);
}

void
PutChar(c, stream, text)
	int c;
	FILE *stream;
	int text;
{
	static int Outcr = OFF, Outeof = OFF;

	if (!text) {
		if (c != EOF)
			putc(c, stream);
		return;
	}

	/* text */
    
	if (Outeof) return;
    
	if (c == EOF) {
		Outeof = ON;
		Outcr = OFF; /* xxx */
		return;
	}

	if (Outcr) {
		Outcr = OFF;
		switch (c) {
		case LF : 
			break;
		case CR : 
			putc(LF, stream);
			Outcr = ON;
			break;
		default:
			putc(c, stream);
			break;
		}
		return;
	}

	switch (c) {
	case CR : 
		putc(LF, stream);
		Outcr = ON;
		break;
	default:
		putc(c, stream);
		break;
	}
    
}

/*
 * Base 64
 */

static char base64[] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

void
base256to64 (c1, c2, c3, padding, outfile)
	int c1, c2, c3, padding;
	FILE *outfile;
{
	putc(base64[c1>>2], outfile);
	putc(base64[((c1 & 0x3)<< 4) | ((c2 & 0xF0) >> 4)], outfile);
	switch (padding) {
	case 0:
		putc(base64[((c2 & 0xF) << 2) | ((c3 & 0xC0) >>6)], outfile);
		putc(base64[c3 & 0x3F], outfile);
		break;
	case 1:
		putc(base64[((c2 & 0xF) << 2) | ((c3 & 0xC0) >>6)], outfile);
		putc(PADDING, outfile);
		break;
	case 2:
		putc(PADDING, outfile);
		putc(PADDING, outfile);
		break;
	}
}

void
base64_encode(infile, outfile, text, length)
	FILE *infile, *outfile;
	int text, length;
{
	int c1, c2, c3, len = 0;
    
	while ((c1 = Getc(infile, text)) != EOF) {
		if ((c2 = Getc(infile, text)) == EOF)
			base256to64(c1, 0, 0, 2, outfile);
		else if ((c3 = Getc(infile, text)) == EOF)
			base256to64(c1, c2, 0, 1, outfile);
		else
			base256to64(c1, c2, c3, 0, outfile);
		len += 4;
		if (len > length) {
			putc('\n', outfile);
			len = 0;
		}
	}
	if (len) putc('\n', outfile);
	fflush(outfile);
    
}

void
base64_decode(infile, outfile, text)
	FILE *infile, *outfile;
	int text;
{
	int c1, c2, c3, c4;

	while ((c1 = GetChar(infile, NO)) != EOF) {
		if (c1 == EOP)
			break;
		if ((c2 = GetChar(infile, YES)) == EOP)
			break;
		PutChar(((c1 << 2) | ((c2 & 0x30) >> 4)), outfile, text);

		if ((c3 = GetChar(infile, YES)) == EOP)
			break;
		PutChar((((c2 & 0x0f) << 4) | ((c3 & 0x3c) >> 2)), outfile, text);
		
		if ((c4 = GetChar(infile, YES)) == EOP)
			break;
		PutChar((((c3 & 0x03) << 6) | c4), outfile, text);
	}
}

/*
 * Quoted_Printable
 */

static char base16[] = "0123456789ABCDEF";
static char From[] = "\nFrom ";

#define EQ   '='
#define TAB   9
#define SP   32
#define DOT  '.'
#define DEL 127
#define softbreak(stream) {putc(EQ, stream); putc(LF, stream);}

void
quoted_printable_encode(infile, outfile, length)
	FILE *infile, *outfile;
	int length;
{
	int c, len = 0, sp = OFF, lfdot = OFF, Fromix = 1;

	while ((c = getc(infile)) != EOF) {
		if ((c == TAB) || (c == SP)) {
			if (From[Fromix] == c) { /* SP */
				putc(EQ, outfile);
				putc(base16[c >> 4], outfile);
				putc(base16[c & 0x0f], outfile);
				len += 3;
				Fromix = 0;
				continue;
			}
			Fromix = 0;
			sp = ON;
			putc(c, outfile);
			if ((++len) > length) {
				sp = OFF;
				len = 0;
				lfdot = LF;
				Fromix = 1;
				softbreak(outfile);
			}
			continue;
		}
		if (c == LF) {
			if (sp || (lfdot == DOT))
				softbreak(outfile);
			len = 0;
			sp = OFF;
			lfdot = LF;
			Fromix = 1;
			putc(LF, outfile);
			continue;
		}
		if ((c < SP) || (c == EQ) || (c >= DEL)) {
			/* exclusive TAB, SP */
			sp = OFF;
			putc(EQ, outfile);
			putc(base16[c >> 4], outfile);
			putc(base16[c & 0x0f], outfile);
			len += 3;
			if (len > length) {
				len = 0;
				lfdot = LF;
				Fromix = 1;
				softbreak(outfile);
			} else {
				Fromix = 0;
				lfdot = OFF;
			}
			continue;
		}
		sp = OFF;

		if (From[Fromix] == c)
			Fromix++;
		else
			Fromix = 0;
	
		if (c == DOT && lfdot == LF)
			lfdot = DOT; 
		else
			lfdot = OFF;

		putc(c, outfile);
		if ((++len) > length) {
			len = 0;
			lfdot = LF;
			Fromix = 1;
			softbreak(outfile);
		}
	}
	if (len > 0)
		softbreak(outfile); /* ignored by decoder */
}

static char base128[] = {
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
	  0,  1,  2,  3,   4,  5,  6,  7,   8,  9,OOB,OOB, OOB,OOB,OOB,OOB,
	OOB, 10, 11, 12,  13, 14, 15,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
	OOB, 10, 11, 12,  13, 14, 15,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
	OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB, OOB,OOB,OOB,OOB,
};

int
puthexchar(c1, c2, stream)
	int c1, c2;
	FILE *stream;
{
	int a1, a2;
	static int warned = OFF;

	if (((a1 = base128[c1]) != OOB) && ((a2 = base128[c2]) != OOB)) {
		putc(((a1 << 4) | a2), stream);
		return (SUCCESS);
	} else {
		if (warned == OFF) { /* warn just once */
			fprintf(stderr, "Error: can't translate hex to character: %c%c\n",
				c1, c2);
			warned = ON;
		}
		return (ERROR);	
	}
}

void
quoted_printable_decode(infile, outfile)
	FILE *infile, *outfile;
{
	int c1, c2, c3;

	/* if an error occurs, print input sequence as it is, anyway, sigh */
    
	while((c1 = getc(infile)) != EOF) {
	skipgetc:
		if (c1 == EQ) {
			if ((c2 = getc(infile)) == EOF) {
				fprintf(stderr, "Error: end of file after =.\n");
				putc(EQ, outfile);
				exit(ERROR);
			}
			if (c2 == LF) continue;
			if ((c3 = getc(infile)) == EOF) {
				fprintf(stderr, "Error: end of file after =.\n");
				putc(EQ, outfile);
				putc(c2, outfile);
				exit(ERROR);
			}
			if (puthexchar(c2, c3, outfile) == ERROR) {
				putc(EQ, outfile);
				if ((c1 = c2) == EOF) exit(ERROR);
				ungetc(c3, infile);
				goto skipgetc;
			} else
				continue;
		}
		putc(c1, outfile);
	}
}

/*
 * Gzip 64
 */

#define READ  0
#define WRITE 1

void
gzip64_encode(infile, outfile, text, length)
	FILE *infile, *outfile;
	int text, length;
{
	int pipes[2];
	int childpid;
    
	if (pipe(pipes) != 0) {
		fprintf(stderr, "Can't open pipe\n");
		exit(ERROR);
	}

	childpid = fork ();

	if (childpid < 0) {
		fprintf(stderr, "Can't fork.\n");
		exit(ERROR);
	}

	if (childpid > 0) { /* I'm the parent. */
		close(pipes[WRITE]);
		if ((infile = fdopen(pipes[READ], FDREAD)) == NULL) {
			fprintf(stderr, "Can't open read pipe\n");
			exit(ERROR);
		}

		base64_encode(infile, outfile, OFF, length);

		exit(SUCCESS);
	}

	/* I'm the child. */

	close(WRITE);
	dup(pipes[WRITE]);
	close(pipes[READ]);
	
	if (text == OFF) {
	
		if (fileno(infile) != READ) {
			close(READ);
			dup(fileno(infile));
		}
	
		execlp("gzip", "gzip", "-q", "-n", "-c", (char *) 0);
	
		exit(SUCCESS);

	} else {

		int pipes2[2];
		int childpid2;
	
		if (pipe(pipes2) != 0) {
			fprintf(stderr, "Can't open pipe\n");
			exit(ERROR);
		}
	
		childpid2 = fork ();
	
		if (childpid2 < 0) {
			fprintf(stderr, "Can't fork.\n");
			exit(ERROR);
		}
	
		if (childpid2 > 0) { /* I'm the child. */
			close(pipes2[WRITE]);
			close(READ);
			dup(pipes2[READ]);

			execlp("gzip", "gzip", "-q", "-n", "-c", (char *) 0);
	    
			exit(SUCCESS);
		}

		/* I'm the grandchild */
#ifdef OS2
		_setmode(pipes2[WRITE],  O_BINARY);
#endif
		close(WRITE);
		dup(pipes2[WRITE]);
		close(pipes2[READ]);
	
		{
			int c;
			while ((c = Getc(infile, ON)) != EOF)
				putchar(c);
		}

		exit(SUCCESS);
	
	}
    
}

void
gzip64_decode(infile, outfile, text)
	FILE *infile, *outfile;
	int text;
{
	int pipes[2];
	int childpid;
    
	if (pipe(pipes) != 0) {
		fprintf(stderr, "Can't open pipe\n");
		exit(ERROR);
	}

	childpid = fork ();

	if (childpid < 0) {
		fprintf(stderr, "Can't fork.\n");
		exit(ERROR);
	}

	if (childpid > 0) { /* I'm the parent. */
		close(READ);
		dup(pipes[READ]);

		close(pipes[WRITE]);

		if (text == ON) {
			int c;
			while ((c = getchar()) != EOF) PutChar(c, outfile, ON);
		} else {
			if (fileno(outfile) != WRITE) {
				close(WRITE);
				dup(fileno(outfile));
			}
			execlp ("gzip", "gzip", "-q", "-d", "-n", "-c", (char *) 0);
		}
	
		exit(SUCCESS);

	}

	/* I'm the child. */
    
	close(pipes[READ]);

	if (text == ON) {
		int pipes2[2];
		int childpid2;
	
		if (pipe(pipes2) != 0) {
			fprintf(stderr, "Can't open pipe\n");
			exit(ERROR);
		}

		childpid2 = fork ();

		if (childpid2 < 0) {
			fprintf(stderr, "Can't fork.\n");
			exit(ERROR);
		}

		if (childpid2 > 0) { /* I'm the child. */
			close(pipes2[WRITE]);
			close(WRITE);
			dup(pipes[WRITE]);
			close(READ);
			dup(pipes2[READ]);

			execlp("gzip", "gzip", "-q", "-d", "-n", "-c", (char *) 0);
	
			exit(SUCCESS);	    
		}

		/* I'm the grandchild */

		close(pipes2[READ]);

		if ((outfile = fdopen(pipes2[WRITE], FDWRITE)) == NULL) {
			fprintf(stderr, "Can't open write pipe\n");
			exit(ERROR);
		}
	
		base64_decode(infile, outfile, OFF);

		exit(SUCCESS);

	} else {

		if ((outfile = fdopen(pipes[WRITE], FDWRITE)) == NULL) {
			fprintf(stderr, "Can't open write pipe\n");
			exit(ERROR);
		}

		base64_decode(infile, outfile, OFF);
	
		exit(SUCCESS);
	}
}

/*
 * main
 */

int main (argc, argv)
	int argc;
	char **argv;
{
	int optc;
	FILE *infile;
	FILE *outfile;
	int file_count = 0;
	/*
	 * default option values
	 */
	int  action = ENCODE;
	char encode = BASE64;	/* -b -q -g */
	int  length = LINELEN;	/* -l num */
	int  text   = OFF;	/* -t */
	char *progname = mewbasename(argv[0]), *p;

	for (p = progname; *p; p++) {
		*p = tolower(*p);
	}

#ifdef _WIN32
	_setmode(_fileno(stdin),  O_BINARY);
	_setmode(_fileno(stdout), O_BINARY);	
#endif
#ifdef OS2
	_setmode(fileno(stdin),  O_BINARY);
	_setmode(fileno(stdout), O_BINARY);	
#endif
	while((optc = getopt_long(argc, argv, "esdbqgzl:thv", longopts,
				  (int *)0)) != EOF) {
		switch (optc) {
		case 'e':
			action = ENCODE;
			break;
		case 'd':
			action = DECODE;
			break;
		case 'b':
			encode = BASE64;
			break;
		case 'q':
			encode = QP;
			break;
		case 'g':
		case 'z':
			encode = GZIP64;
			break;
		case 'l':
			length = atoi(optarg);
			break;
		case 't':
			text = ON;
			break;
		case 'h':
			help(progname);
			exit(SUCCESS);
			break;
		case 'v':
			version(progname);
			exit(SUCCESS);
			break;
		default:
			usage(progname);
			exit(ERROR);
		}
	}

	file_count = argc - optind;

	switch(file_count) {
	case 0:
		infile  = stdin;
		outfile = stdout;
		break;
	case 1:
		if (STRCMP(argv[optind], "-") == 0) {
			infile = stdin;
		} else if ((infile = fopen(argv[optind], FDREAD)) == NULL) {
			fprintf(stderr, "Can't open file %s.\n", argv[optind]);
			exit(ERROR);
		}
		outfile = stdout;
		break;
	case 2:
		if (STRCMP(argv[optind], "-") == 0) {
			infile  = stdin;
		} else if ((infile = fopen(argv[optind], FDREAD)) == NULL) {
			fprintf(stderr, "Can't open file %s.\n", argv[optind]);
			exit(ERROR);
		}
		optind++;
		if (STRCMP(argv[optind], "-") == 0) {
			outfile  = stdout;
		} else if ((outfile = fopen(argv[optind], FDWRITE)) == NULL) {
			fprintf(stderr, "Can't open file %s.\n", argv[optind]);
			exit(ERROR);
		}
		break;
	default:
		usage(progname);
		exit(ERROR);
		break;
	}

	/* Override argments by progname. */

	if (STRCMP(progname, DECODE_NAME) == 0)
		action = DECODE;
	if (STRCMP(progname, CAT_NAME) == 0) {
		action = DECODE;
		outfile = stdout;
	}

	switch (action) {
	case ENCODE:
		switch (encode) {
		case BASE64:
			base64_encode(infile, outfile, text, length);
			break;
		case QP:
			quoted_printable_encode(infile, outfile, length);
			break;
		case GZIP64:
			gzip64_encode(infile, outfile, text, length);
			break;
		}
		break;
	case DECODE:
		switch (encode) {
		case BASE64:
			base64_decode(infile, outfile, text);
			break;
		case QP:
			quoted_printable_decode(infile, outfile);
			break;
		case GZIP64:
			gzip64_decode(infile, outfile, text);
			break;
		}
		break;
	}

	exit(SUCCESS);
}

/* 
 * Copyright (C) 1994, 1995, 1996, 1997 Mew developing team.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the team nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * mewencode.c ends here
 */
