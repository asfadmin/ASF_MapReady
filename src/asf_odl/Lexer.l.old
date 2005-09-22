/*============================================================================
 |  @(#)odl_scan.l	1.3 95/02/03 15:13:52
 |
 |  Object Description Language (ODL) Lexical Analyzer.
 |  Alaska SAR Facility (ASF) Project.
 |
 |  Copyright (C) Jet Propulsion Laboratory
 |
 *===========================================================================*/
%{
#include "Parser.h"

#include "Label.h"
#include "Group.h"
#include "Object.h"

#include "Ident.h"
#include "odl_String.h"
#include "Symbol.h"
#include "Int.h"
#include "Double.h"
#include "odl_Time.h"
#include "Unit.h"

#include "Array.h"
#include "Range.h"
#include "Set.h"
#include "Seq.h"
#include "Seq2.h"

static char sccsid_Lexer_l[] =
	"@(#)odl_scan.l	1.3 95/02/03 15:13:52";

#ifdef	_NO_PROTO
#define YY_DECL			int yylex(yylval, yylp)		\
				YYSTYPE *yylval;		\
				yylex_t* yylp;
#else
#define YY_DECL			int yylex(YYSTYPE *yylval, yylex_t* yylp)
#endif

#undef  YY_INPUT
#define YY_INPUT(buffer, byte_read, max_size)			\
{                                               		\
    byte_read = max_size < ((ODLparse_t*) yylp)->len		\
		? max_size : ((ODLparse_t*) yylp)->len;		\
    if (byte_read > 0) {                        		\
	memcpy(buffer, ((ODLparse_t*) yylp)->buf, byte_read);	\
	((ODLparse_t*) yylp)->buf += byte_read;			\
	((ODLparse_t*) yylp)->len -= byte_read;			\
    }								\
}
int yywrap()			{ return 1; }

#include "y.tab.h"
%}

DIGIT				[0-9]
EDIGIT				[0-9A-Fa-f]
LETTER				[A-Za-z]
WORD				{LETTER}({LETTER}|{DIGIT}|_)*
UINT				{DIGIT}+
XINT				{EDIGIT}+
REAL				({UINT}"."{UINT}?)|("."{UINT})
DATE				{UINT}[-/]{UINT}([-/]{UINT})?
TIME				{UINT}":"{UINT}(":"({UINT}|{REAL}))?
ZONE				("Z")|([+-]{UINT})|([+-]{UINT}":"{UINT})
BEGIN				[Bb][Ee][Gg][Ii][Nn]
OBJECT				[Oo][Bb][Jj][Ee][Cc][Tt]
GROUP				[Gg][Rr][Oo][Uu][Pp]
END				[Ee][Nn][Dd]

%x CMT
%%

{END}				{ return END; }
{END}_{GROUP}			{
#ifdef	DEBUG
				  printf("END_GROUP");
#endif
				  return END_GROUP; }
{END}_{OBJECT}			{
#ifdef	DEBUG
				  printf("END_OBJECT");
#endif
				  return END_OBJECT; }
{GROUP}|({BEGIN}"_"{GROUP})	{
#ifdef	DEBUG
				  printf("GROUP");
#endif
				  return GROUP; }
{OBJECT}|({BEGIN}"_"{OBJECT})	{ 
#ifdef	DEBUG
				  printf("OBJECT");
#endif
				  return OBJECT; }
{WORD}			{ *yylval = (YYSTYPE) NewIdent(YYTEXT, YYLENG);
				  return IDENT; }
'[^']*'			{ *yylval = (YYSTYPE) NewSymbol(YYTEXT+1, YYLENG-2);
				  return SYMBOL; }
\"[^"]*\"		{ *yylval = (YYSTYPE) NewString(YYTEXT+1, YYLENG-2);
				  return STRING; }
\<[^>\n]*\>		{ *yylval = (YYSTYPE) NewUnit(YYTEXT+1, YYLENG-2);
				  return UNIT; }
[+-]?{UINT}			{ *yylval = (YYSTYPE) NewInt(YYTEXT, YYLENG);
				  return INT; }
{UINT}#[+-]?{XINT}#		{ *yylval = (YYSTYPE) NewInt(YYTEXT, YYLENG);
				  return INT; }
[+-]?{REAL}([Ee][+-]?{UINT})?	{ *yylval = (YYSTYPE) NewDouble(YYTEXT, YYLENG);
				  return DOUBLE; }
{DATE}				{ *yylval = (YYSTYPE) NewTime(YYTEXT, YYLENG);
				  return DATE; }
{TIME}({ZONE})?                 { *yylval = (YYSTYPE) NewTime(YYTEXT, YYLENG);
				  return TIME; }
{DATE}[T-]{TIME}({ZONE})?	{ *yylval = (YYSTYPE) NewTime(YYTEXT, YYLENG);
				  return DATETIME; }
[Nn]"/"[Aa]			{ *yylval = (YYSTYPE) NewIdent(YYTEXT, YYLENG);
				  return IDENT; }
".."				{ return RANGE; }
[ \t\r\f\v]+			; /* Ignore white space */
"--".*\n			{ ++yylp->yylineno; }
"***".*\n			{ ++yylp->yylineno; }
\n				{
#ifdef	DEBUG
				  printf("\n");
#endif
				  ++yylp->yylineno; }
.				{
#ifdef	DEBUG
				  printf(" %c ", YYTEXT[0]);
#endif
				  return YYTEXT[0]; }
"/*"				{ BEGIN (CMT); }
<CMT>.				;
<CMT>\n				{ ++yylp->yylineno; }
<CMT>"*/"			{ BEGIN (INITIAL); }
%%
