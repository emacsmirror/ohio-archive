;/* 
sc rxnq.c LINK NOSTARTUP NOSTKCHK STRINGMERGE STREQ IGNORE=73
quit
*/
/*
 *  NAME
 *	rxnq.c
 *
 *  DESCRIPTION
 *	This is a small HACK which removes the quotes from a commandline
 *	and executes RX. This is due to a bug in GNUEmacs Amiga port 1.26
 *	which quotes the arguments to commands executed with
 *	(start-process ...)
 *
 *	To compile under SAS/C 6.x, execute this file.
 *
 *  STATUS
 *	This file is distributed under the GNU General Publc License.
 *
 *  AUTHOR
 *	Anders Lindgren, d91ali@csd.uu.se
 */

#include <proto/exec.h>
#include <proto/dos.h>

#include <string.h>

__asm __saveds int
not_main(register __a0 const char * arg, register __d0 int len)
{
    register struct Library * DOSBase;
    register UBYTE * str;
    register int rc = 20;

    if (DOSBase = OpenLibrary("dos.library", 0)) {
	if (str = AllocMem(len+4, 0)) {
	    strcpy(str, "RX ");
	    if (len >= 3 && arg[0] == '\"' && arg[len-2] == '\"') {
		strncat(str, arg+1, len-3);
	    }
	    else {
		strncat(str, arg, len-1);
	    }
	    rc = (Execute(str, NULL, Output()) ? 0 : 10);
	    
	    FreeMem(str, len+4);
	}
	CloseLibrary(DOSBase);
    }
    return(rc);
}
