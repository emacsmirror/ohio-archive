
%{
#include <stdio.h>
extern char * nbval;
extern char * nboldval;
%}


%token OPENNOTEBOOK CLOSENOTEBOOK
%token NBOPENHEAD OPENHEAD CLOSEHEAD
%token OUTPUTSUBHEAD SETTINGSSUBHEAD
%token CLOSECOMMENT OPENCOMMENT
%token CONTENTS
%token SEMI EQUAL COMMA TOKEN

%%

notebook	: OPENNOTEBOOK { printf("(setq free-cells ())\n");
				printf("(setq notebook-control '\n("); } 
		  NBOPENHEAD control.list CLOSEHEAD 
			{ printf("))\n\n");
			  printf("(setq cell-vector [\n"); } 
			cell.list 
			{ printf("\n])\n"); }
			CLOSENOTEBOOK
		;

cell.list	: OPENHEAD { printf("\n("); } control.list 
		  CLOSEHEAD { printf(" (contents . \""); } 
			 cell.contents { printf("))"); }
			 cell.list
		| ;

cell.contents	: CLOSECOMMENT content.list OPENCOMMENT
		| content.list
		;

content.list	: content.lines /* only for non-subheadings */
		| content.lines
		  OUTPUTSUBHEAD
		{ printf("\n)\n(output-form . \""); }  content.lines
		| content.lines
		  SETTINGSSUBHEAD
		{ printf("\n)\n(styles . \""); } content.lines
		;

content.lines	: CONTENTS { printcontents(nbval); } content.lines
		| { printf("\" ");  } /* last line */
		;

control.list	: control SEMI control.list
		| ;

control		: atoken { printf(" t\) "); }
		|
		| atoken EQUAL { printf(" "); }
			token.list { printf(")"); } ;

atoken		: TOKEN  { printf(" (%s", nbval); } ;

token.list	: tokens COMMA token.list
		| tokens ;

tokens		: TOKEN { printf("%s ", nboldval); } tokens
		| TOKEN  { printf("%s ", nbval); } ; /* look ahead */

%%

#define output(c) putc(c, stdout)

printcontents(p)
	register char *p;
{
	while (*p)
	{ 
		if ('"' == *p) output('\\');
		output(*p++);
	}
}

	
main() {
	return (yyparse () );
      }

yyerror(s)
	char *s;
{
	fprintf(stderr, "%s\n", s);
}

