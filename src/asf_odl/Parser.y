/*============================================================================*
 |  @(#)Parser.y	1.4 96/02/23 18:17:03
 |  
 |  Object Description Language (ODL) Parser.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
%{
#include <stdio.h>
#include <ctype.h>
#include <time.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <limits.h>

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


static
#ifdef	_NO_PROTO
int yyerror ();
#else
int yyerror (char *error_msg, yylex_t* yylp);
#endif
%}

%pure_parser

%token	OBJECT GROUP END END_OBJECT END_GROUP 
%token	IDENT STRING SYMBOL INT DOUBLE DATE TIME DATETIME UNIT RANGE
%%

odl	: stmtList end_opt	{ ((ODLparse_t*) yylp)->tree = (Obj_t*) $1; }
	;
end_opt	: /* optional */
	| END
	;
stmtList 
        : stmtList stmt		{ $$ = (YYSTYPE) Elem($1, $2); }
	| stmt			{ $$ = (YYSTYPE) Elem(NewArray(63), $1); }
	;
stmtList_opt
	: /* optional */	{ $$ = (YYSTYPE) NewArray(0); }
	| stmtList		{ $$ = $1; }
	;
stmt	: OBJECT id stmtList_opt END_OBJECT id_opt
	  {
	    if (! $5 || !strcmp(Val($2), Val($5))) {
		Destroy($5); free($5);
		$$ = (YYSTYPE) NewObject($2, $3);
	    }
	    else {
		yyerror("OBJECT & END_OBJECT names mismatched ", yylp);
		Destroy($2); free($2);
		Destroy($3); free($3);
		Destroy($5); free($5);
		YYERROR;
	    }
	  }
	| GROUP id stmtList_opt END_GROUP id_opt
	  {
	    if (! $5 || !strcmp(Val($2), Val($5))) {
		Destroy($5); free($5);
		$$ = (YYSTYPE) NewGroup($2, $3);
	    }
	    else {
		yyerror("GROUP & END_GROUP names mismatched", yylp);
		Destroy($2); free($2);
		Destroy($3); free($3);
		Destroy($5); free($5);
		YYERROR;
	    }
	  }
	| IDENT '=' value	{ $$ = (YYSTYPE) NewLabel($1, $3); }
	;
id	: '=' IDENT		{ $$ = $2; }
	;
id_opt	: /* optional */	{ $$ =  0; }
	| id			{ $$ = $1; }
	;
value   : scalar		{ $$ = $1; }
	| seq			{ $$ = $1; }
	| set			{ $$ = $1; }
	| '(' seqList ')'	{ $$ = (YYSTYPE) NewSeq2($2); }
	| INT RANGE INT	
	  { $$ = (YYSTYPE) NewRange(Elem(Elem(NewArray(2), $1), $3)); }
	;
seqList : seqList seq		{ $$ = (YYSTYPE) Elem($1, $2); }
	| seq			{ $$ = (YYSTYPE) Elem(NewArray(511), $1); }
	;
seq	: '('  values_opt  ')'	{ $$ = (YYSTYPE) NewSeq($2); }
	;
set	: '{'  values_opt  '}'	{ $$ = (YYSTYPE) NewSet($2); }
	;
values_opt
	: /* optional */	{ $$ = (YYSTYPE) NewArray(0); }
	| values		{ $$ = $1; }
	;
values	: values ',' scalar	{ $$ = (YYSTYPE) Elem($1, $3); }
	| scalar 		{ $$ = (YYSTYPE) Elem(NewArray(511), $1); }
	;
scalar	: STRING		{ $$ = $1; }
	| INT unit_opt		{ $$ = (YYSTYPE) SetUnit($1, $2); }
	| DATE			{ $$ = $1; }
	| TIME			{ $$ = $1; }
	| IDENT			{ $$ = $1; }
	| SYMBOL		{ $$ = $1; }
	| DOUBLE unit_opt	{ $$ = (YYSTYPE) SetUnit($1, $2); }
	| DATETIME unit_opt	{ $$ = (YYSTYPE) SetUnit($1, $2); }
	;
unit_opt: /* optional */	{ $$ =  0; }
	| UNIT			{ $$ = $1; }
	;
%%

static
#ifdef	_NO_PROTO
int yyerror (error_msg, yylp)
char* error_msg;
yylex_t* yylp;

#else
int yyerror (char *error_msg, yylex_t* yylp)
#endif
{
    sprintf(((ODLparse_t*) yylp)->err, "Line %d: %s\n",
	    yylp->yylineno, error_msg);
}

/*--------------------------------------------------------------------------
 |  If string_len == 0, 'string' specifies the name of a file to parse.
 |  otherwise, it points to a character buffer of 'string_len' bytes.
 |  The character buffer does not have to be null-terminated.  'err' is
 |  the buffer space for any errors returned, and must be >= 256 bytes.
 *-------------------------------------------------------------------------*/

#ifdef _NO_PROTO
Obj_t* ODLparse (str, len, err)
char*  str;
size_t len;
char*  err;
#else

Obj_t* ODLparse (char* str, size_t len, char* err)
#endif
{
    ODLparse_t parse;
    size_t n;
    int fd = -1;
    if (!str) return NULL;

    if (!len) {
	struct stat fs;
	char* file = str;

	if ((fd = open(file, O_RDONLY)) == -1 ||
	    fstat(fd, &fs) == -1) {
	    strcpy(err, strerror(errno));
	    return NULL;
	}
	if ((str = (char *) malloc(len = fs.st_size)) == NULL) {
	    strcpy(err, strerror(errno));
	    close(fd);
	    return NULL;
	}
	if (read(fd, str, len) != len) {
	    strcpy(err, errno ? strerror(errno) : "read error in ODLparse");
	    free(str);
	    close(fd);
	    return NULL;
	}
	close(fd);
    }
    *err = 0;
    parse.buf = str;
    parse.len = len;
    parse.tree = NULL;

    if (yyparse((yylex_t*) &parse))
	strcpy(err, parse.err);

    yy_delete_buffer(parse.state.yy_current_buffer, &parse.state);

    if (fd != -1) free(str);
    return (parse.tree);
}
