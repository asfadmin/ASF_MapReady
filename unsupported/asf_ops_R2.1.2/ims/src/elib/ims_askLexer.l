%{
static char *sccs = "@(#)ims_askLexer.l	5.1  16 Mar 1996";
/**************************************************************************
**
** File:      ims_askLexer.l
**  
** Creator:   H. Sayah
**
** Date:      August 5, 1991
**
** Function:  Lexical scanner to identify tokens for the ims_parseKwBuffer()
**            routine. 
**            This file is an input to the UNIX's lex program.
**
** Updated:   H. Sayah   March 1995
**            ASF adoption of AMMOS-CDB inherited code.
**
**************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <ims_const.h>
#include <ims_keyword.h>
#include <ims_dbms.h>
#include <ims_askLexer.h>
#include <ims_askParser.h>

/*
** These variables are flags indicating where input is comming from
** It can either come from memory of from file(fp).  The program that
** will call the parser will have to set the flag before a call to the
** parser is made.  If lexinput is set to FILE_PTR then, input will be
** from fp else from memory.  The variables lexinput,lexinput_fp,and
** lexinput_memptr must be set appropriately by the calling program
*/
enum LEX_INPUT_SOURCE lexinput;
FILE		*lexinput_fp;
char		*lexinput_memptr;


#undef getc
#define	getc(x) ((lexinput == FILE_PTR) ? \
	(fgetc(lexinput_fp)):(*(lexinput_memptr++)))

#define	yywrap()	1

/*
** Function to save a string 
*/
char	*saveString(string)
char	*string;	/*passed in string*/
{
	char	*new;
	if (!string)
	{
		return((char *)NULL);
	}
	if ( new = malloc((unsigned)strlen(string)+1))
	{
		(void) strcpy(new, string);
		return(new);
	}
	else
	{
		return((char *)NULL);
	}
}
%}

WHITESPACE		[ \t\n\r]
CHARACTER		[^ \t\n\r\(\)\[\]\"\']
LETTER			[a-zA-Z]
STRING			{CHARACTER}+

%%


\/\*(\n|.)*\*\/		;	/* comment - ignored */

"<=" 			{ yylval.string = saveString(yytext); return(BI_OPERATOR); }
">=" 			{ yylval.string = saveString(yytext); return(BI_OPERATOR); }
"!=" 			{ yylval.string = saveString(yytext); return(BI_OPERATOR); }
">" 			{ yylval.string = saveString(yytext); return(BI_OPERATOR); }
"<"			{ yylval.string = saveString(yytext); return(BI_OPERATOR); }
"="			{ yylval.string = saveString(yytext); return(BI_OPERATOR); }
"like"		{ yylval.string = saveString(yytext); return(BI_OPERATOR); }
"max"			{ yylval.string = saveString(yytext); return(UN_OPERATOR); }
"min"			{ yylval.string = saveString(yytext); return(UN_OPERATOR); }
"and"			{ yylval.string = saveString(yytext); return(BL_OPERATOR); }
"or"			{ yylval.string = saveString(yytext); return(BL_OPERATOR); }
"where"		{ yylval.string = saveString(yytext); return(WHERE); }
"("			{ yylval.string = saveString(yytext); return(OPEN_PARAN); }
")"			{ yylval.string = saveString(yytext); return(CLOSE_PARAN); }

{WHITESPACE}		;

{STRING}		{
				yylval.string = saveString(yytext);
				return(T_STRING);
			}
\"([^"])*\"	{
				yylval.string = saveString(yytext);
				return(T_QSTRING);
			}
\'([^'"])*\'	{
				yylval.string = saveString(yytext);
				return(T_QSTRING);
			}
.			{ return(yytext[0]); }

