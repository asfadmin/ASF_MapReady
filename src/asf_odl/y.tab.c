
/*  A Bison parser, made from Parser.y with Bison version GNU Bison version 1.22

ASF Hand modifications:
1.1  7/98  O. Lawlor  Removed alloca calls.
     According to its own man page, 
    "alloca() is machine-, compiler-, and most  of  all,  system-
     dependent.  Its use is strongly discouraged."
     Alloca is not implimented on several systems, 
     including the Cray T3E.  Hence, I'm removing it, and 
     replacing it with malloc().  This introduces a memory leak
     in the parser, but because we don't parse too many ODL 
     entries, this shouldn't cause any problems.
*/

#define YYBISON 1  /* Identify Bison output.  */

#define	OBJECT	258
#define	GROUP	259
#define	END	260
#define	END_OBJECT	261
#define	END_GROUP	262
#define	IDENT	263
#define	STRING	264
#define	SYMBOL	265
#define	INT	266
#define	DOUBLE	267
#define	DATE	268
#define	TIME	269
#define	DATETIME	270
#define	UNIT	271
#define	RANGE	272

#line 9 "Parser.y"

#include "asf.h"
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

static char sccsid_Parser_y[] =
"@(#)Parser.y	1.4 96/02/23 18:17:03";

static
#ifdef	_NO_PROTO
int yyerror ();
#else
int yyerror (char *error_msg, yylex_t* yylp);
#endif

#ifndef YYLTYPE
typedef
struct yyltype
{
	int timestamp;
	int first_line;
	int first_column;
	int last_line;
	int last_column;
	char *text;
}  yyltype;

#define YYLTYPE yyltype
#endif

#ifndef YYSTYPE
#define YYSTYPE int
#endif
#include "asf.h"

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		58
#define	YYFLAG		-32768
#define	YYNTBASE	24

#define YYTRANSLATE(x) ((unsigned)(x) <= 272 ? yytranslate[x] : 39)

static const char yytranslate[] = {     
	0,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,    19,
	    20,     2,     2,    23,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	    18,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,    21,     2,    22,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
	     2,     2,     2,     2,     2,     1,     2,     3,     4,     5,
	     6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
	    16,    17
};

#if YYDEBUG != 0
static const short yyprhs[] = {     
	0,
	     0,     3,     4,     6,     9,    11,    12,    14,    20,    26,
	    30,    33,    34,    36,    38,    40,    42,    46,    50,    53,
	    55,    59,    63,    64,    66,    70,    72,    74,    77,    79,
	    81,    83,    85,    88,    91,    92
};

static const short yyrhs[] = {    
	26,
	    25,     0,     0,     5,     0,    26,    28,     0,    28,     0,
	     0,    26,     0,     3,    29,    27,     6,    30,     0,     4,
	    29,    27,     7,    30,     0,     8,    18,    31,     0,    18,
	     8,     0,     0,    29,     0,    37,     0,    33,     0,    34,
	     0,    19,    32,    20,     0,    11,    17,    11,     0,    32,
	    33,     0,    33,     0,    19,    35,    20,     0,    21,    35,
	    22,     0,     0,    36,     0,    36,    23,    37,     0,    37,
	     0,     9,     0,    11,    38,     0,    13,     0,    14,     0,
	     8,     0,    10,     0,    12,    38,     0,    15,    38,     0,
	     0,    16,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 
	0,
	    57,    59,    60,    63,    64,    67,    68,    70,    84,    98,
	   100,   102,   103,   105,   106,   107,   108,   109,   112,   113,
	   115,   117,   120,   121,   123,   124,   126,   127,   128,   129,
	   130,   131,   132,   133,   135,   136
};

static const char * const yytname[] = {   
	"$","error","$illegal.","OBJECT","GROUP",
	"END","END_OBJECT","END_GROUP","IDENT","STRING","SYMBOL","INT","DOUBLE","DATE",
	"TIME","DATETIME","UNIT","RANGE","'='","'('","')'","'{'","'}'","','","odl","end_opt",
	"stmtList","stmtList_opt","stmt","id","id_opt","value","seqList","seq","set",
	"values_opt","values","scalar","unit_opt",""
};
#endif

static const short yyr1[] = {     
	0,
	    24,    25,    25,    26,    26,    27,    27,    28,    28,    28,
	    29,    30,    30,    31,    31,    31,    31,    31,    32,    32,
	    33,    34,    35,    35,    36,    36,    37,    37,    37,    37,
	    37,    37,    37,    37,    38,    38
};

static const short yyr2[] = {     
	0,
	     2,     0,     1,     2,     1,     0,     1,     5,     5,     3,
	     2,     0,     1,     1,     1,     1,     3,     3,     2,     1,
	     3,     3,     0,     1,     3,     1,     1,     2,     1,     1,
	     1,     1,     2,     2,     0,     1
};

static const short yydefact[] = {     
	0,
	     0,     0,     0,     2,     5,     0,     6,     6,     0,     3,
	     1,     4,    11,     7,     0,     0,    31,    27,    32,    35,
	    35,    29,    30,    35,    23,    23,    10,    15,    16,    14,
	    12,    12,    36,     0,    28,    33,    34,    35,    23,     0,
	    20,     0,    24,    26,     0,    13,     8,     9,    18,    17,
	    19,    21,     0,    22,    25,     0,     0,     0
};

static const short yydefgoto[] = {    
	56,
	    11,    14,    15,     5,    46,    47,    27,    40,    28,    29,
	    42,    43,    44,    35
};

static const short yypact[] = {    
	42,
	    -8,    -8,    31,    26,-32768,     4,    42,    42,    -6,-32768,
	-32768,-32768,-32768,    42,    45,    47,-32768,-32768,-32768,    10,
	    36,-32768,-32768,    36,     9,    28,-32768,-32768,-32768,-32768,
	    -8,    -8,-32768,    44,-32768,-32768,-32768,    36,    28,    13,
	-32768,    33,    34,-32768,    37,-32768,-32768,-32768,-32768,-32768,
	-32768,-32768,    28,-32768,-32768,    56,    58,-32768
};

static const short yypgoto[] = {
	-32768,
	-32768,    60,    53,    21,    46,    30,-32768,-32768,   -24,-32768,
	    38,-32768,    -9,   -10
};


#define	YYLAST		64


static const short yytable[] = {    
	30,
	    41,    17,    18,    19,    20,    21,    22,    23,    24,     6,
	    36,    13,    25,    37,    26,    51,    17,    18,    19,    38,
	    21,    22,    23,    24,    12,    33,    34,    39,     1,     2,
	    10,    39,    50,     3,    12,    17,    18,    19,    38,    21,
	    22,    23,    24,    55,     1,     2,     7,     8,     9,     3,
	    31,    33,    52,    32,    49,    57,    53,    58,    54,     4,
	    16,    48,     0,    45
};

static const short yycheck[] = {     
	9,
	    25,     8,     9,    10,    11,    12,    13,    14,    15,    18,
	    21,     8,    19,    24,    21,    40,     8,     9,    10,    11,
	    12,    13,    14,    15,     4,    16,    17,    19,     3,     4,
	     5,    19,    20,     8,    14,     8,     9,    10,    11,    12,
	    13,    14,    15,    53,     3,     4,     1,     2,    18,     8,
	     6,    16,    20,     7,    11,     0,    23,     0,    22,     0,
	     8,    32,    -1,    26
};
#define YYPURE 1

/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
/* Modified for the Alaska SAR Facility Project SAR Processing System */
/* @(#)bison.simple	1.2 95/11/16 11:47:49 */
#line 3 "../../lib//usr/people/src/cp/src/toolkit/lib/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */




/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#ifdef YYPURE
#define YYCONTEXT	, yylp
#else
#define YYCONTEXT
#endif
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up" YYCONTEXT); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		yylex(&yylval, &yylloc YYCONTEXT)
#else
#define YYLEX		yylex(&yylval YYCONTEXT)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPURE
int yyparse (yylex_t* yylp);
#else
int yyparse (void);
#endif
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (from, to, count)
char *from;
char *to;
int count;
{
	register char *f = from;
	register char *t = to;
	register int i = count;

	while (i-- > 0)
		*t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_bcopy (char *from, char *to, int count)
{
	register char *f = from;
	register char *t = to;
	register int i = count;

	while (i-- > 0)
		*t++ = *f++;
}

#endif
#endif

#line 184 "../../lib//usr/people/src/cp/src/toolkit/lib/bison.simple"
int
#ifdef YYPURE
#ifdef __GNUC__
yyparse (yylex_t* yylp)
#else
yyparse (yylp)
yylex_t* yylp;
#endif
#else
yyparse()
#endif
{
	register int yystate;
	register int yyn;
	register short *yyssp;
	register YYSTYPE *yyvsp;
	int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
	int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

	short	yyssa[YYINITDEPTH];	/*  the state stack			*/
	YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

	short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
	YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
	YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
	YYLTYPE *yyls = yylsa;
	YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

	int yystacksize = YYINITDEPTH;

#ifdef YYPURE
	int yychar;
	YYSTYPE yylval;
	int yynerrs;
#ifdef YYLSP_NEEDED
	YYLTYPE yylloc;
#endif
#endif

	YYSTYPE yyval;		/*  the variable used to return		*/
	/*  semantic values from the action	*/
	/*  routines				*/

	int yylen;

#if YYDEBUG != 0
	if (yydebug)
		fprintf(stderr, "Starting parse\n");
#endif

	yylex_init(yylp);
	yystate = 0;
	yyerrstatus = 0;
	yynerrs = 0;
	yychar = YYEMPTY;		/* Cause a token to be read.  */

	/* Initialize stack pointers.
	     Waste one element of value and location stack
	     so that they stay on the same level as the state stack.
	     The wasted elements are never initialized.  */

	yyssp = yyss - 1;
	yyvsp = yyvs;
#ifdef YYLSP_NEEDED
	yylsp = yyls;
#endif

	/* Push a new state, which is found in  yystate  .  */
	/* In all cases, when you get here, the value and location stacks
	   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

	*++yyssp = yystate;

	if (yyssp >= yyss + yystacksize - 1)
	{
		/* Give user a chance to reallocate the stack */
		/* Use copies of these so that the &'s don't force the real ones into memory. */
		YYSTYPE *yyvs1 = yyvs;
		short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
		YYLTYPE *yyls1 = yyls;
#endif

		/* Get the current used size of the three stacks, in elements.  */
		int size = yyssp - yyss + 1;

#ifdef yyoverflow
		/* Each stack pointer address is followed by the size of
			 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
		/* This used to be a conditional around just the two extra args,
			 but that might be undefined if yyoverflow is a macro.  */
		yyoverflow("parser stack overflow",
		    &yyss1, size * sizeof (*yyssp),
		    &yyvs1, size * sizeof (*yyvsp),
		    &yyls1, size * sizeof (*yylsp),
		    &yystacksize);
#else
		yyoverflow("parser stack overflow",
		    &yyss1, size * sizeof (*yyssp),
		    &yyvs1, size * sizeof (*yyvsp),
		    &yystacksize);
#endif

		yyss = yyss1; 
		yyvs = yyvs1;
#ifdef YYLSP_NEEDED
		yyls = yyls1;
#endif
#else /* no yyoverflow */
		/* Extend the stack our own way.  */
		if (yystacksize >= YYMAXDEPTH)
		{
			yyerror("parser stack overflow" YYCONTEXT);
			return 2;
		}
		yystacksize *= 2;
		if (yystacksize > YYMAXDEPTH)
			yystacksize = YYMAXDEPTH;
		yyss = (short *) malloc (yystacksize * sizeof (*yyssp));
		__yy_bcopy ((char *)yyss1, (char *)yyss, size * sizeof (*yyssp));
		yyvs = (YYSTYPE *) malloc (yystacksize * sizeof (*yyvsp));
		__yy_bcopy ((char *)yyvs1, (char *)yyvs, size * sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
		yyls = (YYLTYPE *) malloc (yystacksize * sizeof (*yylsp));
		__yy_bcopy ((char *)yyls1, (char *)yyls, size * sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

		yyssp = yyss + size - 1;
		yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
		yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
		if (yydebug)
			fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

		if (yyssp >= yyss + yystacksize - 1)
			YYABORT;
	}

#if YYDEBUG != 0
	if (yydebug)
		fprintf(stderr, "Entering state %d\n", yystate);
#endif

	goto yybackup;
yybackup:

	/* Do appropriate processing given the current state.  */
	/* Read a lookahead token if we need one and don't already have one.  */
	/* yyresume: */

	/* First try to decide what to do without reference to lookahead token.  */

	yyn = yypact[yystate];
	if (yyn == YYFLAG)
		goto yydefault;

	/* Not known => get a lookahead token if don't already have one.  */

	/* yychar is either YYEMPTY or YYEOF
	     or a valid token in external form.  */

	if (yychar == YYEMPTY)
	{
#if YYDEBUG != 0
		if (yydebug)
			fprintf(stderr, "Reading a token: ");
#endif
		yychar = YYLEX;
	}

	/* Convert token to internal form (in yychar1) for indexing tables with */

	if (yychar <= 0)		/* This means end of input. */
	{
		yychar1 = 0;
		yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
		if (yydebug)
			fprintf(stderr, "Now at end of input.\n");
#endif
	}
	else
	{
		yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
		if (yydebug)
		{
			fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
			/* Give the individual parser a way to print the precise meaning
				     of a token, for further debugging info.  */
#ifdef YYPRINT
			YYPRINT (stderr, yychar, yylval);
#endif
			fprintf (stderr, ")\n");
		}
#endif
	}

	yyn += yychar1;
	if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
		goto yydefault;

	yyn = yytable[yyn];

	/* yyn is what to do for this token type in this state.
	     Negative => reduce, -yyn is rule number.
	     Positive => shift, yyn is new state.
	       New state is final state => don't bother to shift,
	       just return success.
	     0, or most negative number => error.  */

	if (yyn < 0)
	{
		if (yyn == YYFLAG)
			goto yyerrlab;
		yyn = -yyn;
		goto yyreduce;
	}
	else if (yyn == 0)
		goto yyerrlab;

	if (yyn == YYFINAL)
		YYACCEPT;

	/* Shift the lookahead token.  */

#if YYDEBUG != 0
	if (yydebug)
		fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

	/* Discard the token being shifted unless it is eof.  */
	if (yychar != YYEOF)
		yychar = YYEMPTY;

	*++yyvsp = yylval;
#ifdef YYLSP_NEEDED
	*++yylsp = yylloc;
#endif

	/* count tokens shifted since error; after three, turn off error status.  */
	if (yyerrstatus) yyerrstatus--;

	yystate = yyn;
	goto yynewstate;

	/* Do the default action for the current state.  */
yydefault:

	yyn = yydefact[yystate];
	if (yyn == 0)
		goto yyerrlab;

	/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
	yylen = yyr2[yyn];
	if (yylen > 0)
		yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
	if (yydebug)
	{
		int i;

		fprintf (stderr, "Reducing via rule %d (line %d), ",
		    yyn, yyrline[yyn]);

		/* Print the symbols being reduced, and their result.  */
		for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
			fprintf (stderr, "%s ", yytname[yyrhs[i]]);
		fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
	}
#endif


	switch (yyn) {

	case 1:
#line 57 "Parser.y"
		{ 
			((ODLparse_t*) yylp)->tree = (Obj_t*) yyvsp[-1]; 
			;
			break;
		}
	case 4:
#line 63 "Parser.y"
		{ 
			yyval = (YYSTYPE) Elem(yyvsp[-1], yyvsp[0]); 
			;
			break;
		}
	case 5:
#line 64 "Parser.y"
		{ 
			yyval = (YYSTYPE) Elem(NewArray(63), yyvsp[0]); 
			;
			break;
		}
	case 6:
#line 67 "Parser.y"
		{ 
			yyval = (YYSTYPE) NewArray(0); 
			;
			break;
		}
	case 7:
#line 68 "Parser.y"
		{ 
			yyval = yyvsp[0]; 
			;
			break;
		}
	case 8:
#line 71 "Parser.y"
		{
			if (! yyvsp[0] || !strcmp(Val(yyvsp[-3]), Val(yyvsp[0]))) {
				Destroy(yyvsp[0]); 
				free(yyvsp[0]);
				yyval = (YYSTYPE) NewObject(yyvsp[-3], yyvsp[-2]);
			}
			else {
				yyerror("OBJECT & END_OBJECT names mismatched ", yylp);
				Destroy(yyvsp[-3]); 
				free(yyvsp[-3]);
				Destroy(yyvsp[-2]); 
				free(yyvsp[-2]);
				Destroy(yyvsp[0]); 
				free(yyvsp[0]);
				YYERROR;
			};

			break;
		}
	case 9:
#line 85 "Parser.y"
		{
			if (! yyvsp[0] || !strcmp(Val(yyvsp[-3]), Val(yyvsp[0]))) {
				Destroy(yyvsp[0]); 
				free(yyvsp[0]);
				yyval = (YYSTYPE) NewGroup(yyvsp[-3], yyvsp[-2]);
			}
			else {
				yyerror("GROUP & END_GROUP names mismatched", yylp);
				Destroy(yyvsp[-3]); 
				free(yyvsp[-3]);
				Destroy(yyvsp[-2]); 
				free(yyvsp[-2]);
				Destroy(yyvsp[0]); 
				free(yyvsp[0]);
				YYERROR;
			};

			break;
		}
	case 10:
#line 98 "Parser.y"
		{ 
			yyval = (YYSTYPE) NewLabel(yyvsp[-2], yyvsp[0]); 
			;
			break;
		}
	case 11:
#line 100 "Parser.y"
		{ 
			yyval = yyvsp[0]; 
			;
			break;
		}
	case 12:
#line 102 "Parser.y"
		{ 
			yyval =  0; 
			;
			break;
		}
	case 13:
#line 103 "Parser.y"
		{ 
			yyval = yyvsp[0]; 
			;
			break;
		}
	case 14:
#line 105 "Parser.y"
		{ 
			yyval = yyvsp[0]; 
			;
			break;
		}
	case 15:
#line 106 "Parser.y"
		{ 
			yyval = yyvsp[0]; 
			;
			break;
		}
	case 16:
#line 107 "Parser.y"
		{ 
			yyval = yyvsp[0]; 
			;
			break;
		}
	case 17:
#line 108 "Parser.y"
		{ 
			yyval = (YYSTYPE) NewSeq2(yyvsp[-1]); 
			;
			break;
		}
	case 18:
#line 110 "Parser.y"
		{ 
			yyval = (YYSTYPE) NewRange(Elem(Elem(NewArray(2), yyvsp[-2]), yyvsp[0])); 
			;
			break;
		}
	case 19:
#line 112 "Parser.y"
		{ 
			yyval = (YYSTYPE) Elem(yyvsp[-1], yyvsp[0]); 
			;
			break;
		}
	case 20:
#line 113 "Parser.y"
		{ 
			yyval = (YYSTYPE) Elem(NewArray(511), yyvsp[0]); 
			;
			break;
		}
	case 21:
#line 115 "Parser.y"
		{ 
			yyval = (YYSTYPE) NewSeq(yyvsp[-1]); 
			;
			break;
		}
	case 22:
#line 117 "Parser.y"
		{ 
			yyval = (YYSTYPE) NewSet(yyvsp[-1]); 
			;
			break;
		}
	case 23:
#line 120 "Parser.y"
		{ 
			yyval = (YYSTYPE) NewArray(0); 
			;
			break;
		}
	case 24:
#line 121 "Parser.y"
		{ 
			yyval = yyvsp[0]; 
			;
			break;
		}
	case 25:
#line 123 "Parser.y"
		{ 
			yyval = (YYSTYPE) Elem(yyvsp[-2], yyvsp[0]); 
			;
			break;
		}
	case 26:
#line 124 "Parser.y"
		{ 
			yyval = (YYSTYPE) Elem(NewArray(511), yyvsp[0]); 
			;
			break;
		}
	case 27:
#line 126 "Parser.y"
		{ 
			yyval = yyvsp[0]; 
			;
			break;
		}
	case 28:
#line 127 "Parser.y"
		{ 
			yyval = (YYSTYPE) SetUnit(yyvsp[-1], yyvsp[0]); 
			;
			break;
		}
	case 29:
#line 128 "Parser.y"
		{ 
			yyval = yyvsp[0]; 
			;
			break;
		}
	case 30:
#line 129 "Parser.y"
		{ 
			yyval = yyvsp[0]; 
			;
			break;
		}
	case 31:
#line 130 "Parser.y"
		{ 
			yyval = yyvsp[0]; 
			;
			break;
		}
	case 32:
#line 131 "Parser.y"
		{ 
			yyval = yyvsp[0]; 
			;
			break;
		}
	case 33:
#line 132 "Parser.y"
		{ 
			yyval = (YYSTYPE) SetUnit(yyvsp[-1], yyvsp[0]); 
			;
			break;
		}
	case 34:
#line 133 "Parser.y"
		{ 
			yyval = (YYSTYPE) SetUnit(yyvsp[-1], yyvsp[0]); 
			;
			break;
		}
	case 35:
#line 135 "Parser.y"
		{ 
			yyval =  0; 
			;
			break;
		}
	case 36:
#line 136 "Parser.y"
		{ 
			yyval = yyvsp[0]; 
			;
			break;
		}
	}
	/* the action file gets copied in in place of this dollarsign */
#line 465 "../../lib//usr/people/src/cp/src/toolkit/lib/bison.simple"
	
	    yyvsp -= yylen;
	yyssp -= yylen;
#ifdef YYLSP_NEEDED
	yylsp -= yylen;
#endif

#if YYDEBUG != 0
	if (yydebug)
	{
		short *ssp1 = yyss - 1;
		fprintf (stderr, "state stack now");
		while (ssp1 != yyssp)
			fprintf (stderr, " %d", *++ssp1);
		fprintf (stderr, "\n");
	}
#endif

	*++yyvsp = yyval;

#ifdef YYLSP_NEEDED
	yylsp++;
	if (yylen == 0)
	{
		yylsp->first_line = yylloc.first_line;
		yylsp->first_column = yylloc.first_column;
		yylsp->last_line = (yylsp-1)->last_line;
		yylsp->last_column = (yylsp-1)->last_column;
		yylsp->text = 0;
	}
	else
	{
		yylsp->last_line = (yylsp+yylen-1)->last_line;
		yylsp->last_column = (yylsp+yylen-1)->last_column;
	}
#endif

	/* Now "shift" the result of the reduction.
	     Determine what state that goes to,
	     based on the state we popped back to
	     and the rule number reduced by.  */

	yyn = yyr1[yyn];

	yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
	if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
		yystate = yytable[yystate];
		else
		yystate = yydefgoto[yyn - YYNTBASE];

	goto yynewstate;

yyerrlab:   /* here on detecting error */

	if (! yyerrstatus)
	/* If not already recovering from an error, report this error.  */
	{
		++yynerrs;

#ifdef YYERROR_VERBOSE
		yyn = yypact[yystate];

		if (yyn > YYFLAG && yyn < YYLAST)
		{
			int size = 0;
			char *msg;
			int x, count;

			count = 0;
			/* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
			for (x = (yyn < 0 ? -yyn : 0);
			    x < (sizeof(yytname) / sizeof(char *)); x++)
				if (yycheck[x + yyn] == x)
					size += strlen(yytname[x]) + 15, count++;
			msg = (char *) malloc(size + 15);
			if (msg != 0)
			{
				strcpy(msg, "parse error");

				if (count < 5)
				{
					count = 0;
					for (x = (yyn < 0 ? -yyn : 0);
					    x < (sizeof(yytname) / sizeof(char *)); x++)
						if (yycheck[x + yyn] == x)
						{
							strcat(msg, count == 0 ? ", expecting `" : " or `");
							strcat(msg, yytname[x]);
							strcat(msg, "'");
							count++;
						}
				}
				yyerror(msg YYCONTEXT);
				free(msg);
			}
			else
				yyerror ("parse error; also virtual memory exceeded" YYCONTEXT);
		}
		else
#endif /* YYERROR_VERBOSE */
			yyerror("parse error" YYCONTEXT);
	}

	goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

	if (yyerrstatus == 3)
	{
		/* if just tried and failed to reuse lookahead token after an error, discard it.  */

		/* return failure if at end of input */
		if (yychar == YYEOF)
			YYABORT;

#if YYDEBUG != 0
		if (yydebug)
			fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

		yychar = YYEMPTY;
	}

	/* Else will try to reuse lookahead token
	     after shifting the error token.  */

	yyerrstatus = 3;		/* Each real token shifted decrements this */

	goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
	/* This is wrong; only states that explicitly want error tokens
	     should shift them.  */
	yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
	if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

	if (yyssp == yyss) YYABORT;
	yyvsp--;
	yystate = *--yyssp;
#ifdef YYLSP_NEEDED
	yylsp--;
#endif

#if YYDEBUG != 0
	if (yydebug)
	{
		short *ssp1 = yyss - 1;
		fprintf (stderr, "Error: state stack now");
		while (ssp1 != yyssp)
			fprintf (stderr, " %d", *++ssp1);
		fprintf (stderr, "\n");
	}
#endif

yyerrhandle:

	yyn = yypact[yystate];
	if (yyn == YYFLAG)
		goto yyerrdefault;

	yyn += YYTERROR;
	if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
		goto yyerrdefault;

	yyn = yytable[yyn];
	if (yyn < 0)
	{
		if (yyn == YYFLAG)
			goto yyerrpop;
		yyn = -yyn;
		goto yyreduce;
	}
	else if (yyn == 0)
		goto yyerrpop;

	if (yyn == YYFINAL)
		YYACCEPT;

#if YYDEBUG != 0
	if (yydebug)
		fprintf(stderr, "Shifting error token, ");
#endif

	*++yyvsp = yylval;
#ifdef YYLSP_NEEDED
	*++yylsp = yylloc;
#endif

	yystate = yyn;
	goto yynewstate;
}
#line 138 "Parser.y"


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
			strcpy(err, "bad open!");/*strerror(errno));*/
			return NULL;
		}
		if ((str = (char *) malloc(len = fs.st_size)) == NULL) {
			strcpy(err, "bad malloc!\n");/*strerror(errno));*/
			close(fd);
			return NULL;
		}
		if (read(fd, str, len) != len) {
			strcpy(err, "read error!\n");
			/*errno ? strerror(errno) : "read error in ODLparse");*/
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
