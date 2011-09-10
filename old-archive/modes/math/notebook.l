
%{
#include "notebook.h"
char * nbval = "";
char * nboldval = "";

char tokbuf[512];
char tokoldbuf[512];

#ifdef LEXER
#define result(tok) {\
printf("%s ", "tok");\
 return(tok); }
#else
#define result(tok) {\
 return(tok); }
#endif
%}

whitespace	[ \t\n\r]*
token		([a-zA-Z0-9]+|\"[^"\n]+\")

%Start	Head Body

%%
^"(*^"{whitespace}	{ BEGIN Body; result (OPENNOTEBOOK) ; }

^"^*)"{whitespace}	result (CLOSENOTEBOOK) ;

^"(*"{whitespace}	result (OPENCOMMENT) ;
^"*)"{whitespace}	result (CLOSECOMMENT) ;

<Body>^"::["		{ BEGIN Head;
			  result (NBOPENHEAD) ; }

<Body>^":["		{ BEGIN Head;
			  result (OPENHEAD) ; }

<Head>"]"{whitespace}\n		{ BEGIN Body;
			  result (CLOSEHEAD) ; }

<Body>^";[o]"		result (OUTPUTSUBHEAD) ;
<Body>^";[s]"		result (SETTINGSSUBHEAD) ;

<Head>";"		result (SEMI) ;
<Head>"="		result (EQUAL) ;
<Head>","		result (COMMA) ;

<Head>{whitespace}	;


<Body>^([^:;\n].*)|\n	{
			  strcpy(tokoldbuf, tokbuf);
			  nboldval = tokoldbuf;
			  strcpy(tokbuf, yytext);
			 nbval = tokbuf;
#ifdef LEXER
printf("\"%s\" ", nbval);
#endif
			result (CONTENTS) ;}

<Head>{token}(" "{token})*	{ 
			  strcpy(tokoldbuf, tokbuf);
			  nboldval = tokoldbuf;
			  strcpy(tokbuf, yytext);
			  nbval = tokbuf;
#ifdef LEXER
printf("\"%s\" ", nbval);
#endif
			 result (TOKEN) ; }

<Body>.		{ printf("\\%3o", yytchar);  /* last char read */
		  result (CONTENTS); }

<Head>.		{ printf(" Illegal Character: \\%3o\n", yytchar);
		  result (-1); }

%% 

#ifdef LEXER
main ()
{
	int t;
	
	t = yylex();
	while (t)
	 {
		t = yylex();
	}
}

int yywrap ()
{
  return (1);
}

#endif
