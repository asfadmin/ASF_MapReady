# line 2 "odl2.y"

/*****************************************************************************

 Description: This file contains the parser for the Object Description
              Langauge (ODL).  The parser is produced using Yacc and
              all changes to the parsing scheme should be made by
              modifying the Yacc input file rather than the
              C-language code produced from by Yacc.

 Author:  Randy Davis, University of Colorado LASP

 Creation Date: 17 April 1990
 Last Modified: 18 May 1991

 History:

   Creation - This module was introduced in the Version 1 ODLC library.

   Version 2.0 - 30 August 1990 - R. Davis, U. of Colorado LASP
     a) Modified to comply with ODL Version 2.  This includes adding
        support for GROUP statements.

   Version 2.0.1 - 26 November 1990 - R. Davis, U. of Colorado LASP
     a) Changed parsing scheme to provide better error reporting and
        recovery.

   Version 2.1 - 13 March 1991
     a) Modified calls to parser action routines to pass pointers to
        value structures rather than copying in the entire structure.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    Added include file odlinter.h.

*****************************************************************************/

#include "odldef.h"
#include "odlinter.h"

#ifdef PDS_TOOLBOX
    extern int pds_watch_ends;
#else
    int pds_watch_ends = TRUE;
#endif


# line 51 "odl2.y"
typedef union         {
               struct Value_Data  item;
               int                flag;
	      } YYSTYPE;
# define _OBJECT 257
# define _END_OBJECT 258
# define _GROUP 259
# define _END_GROUP 260
# define _END 261
# define _sequence_opening 262
# define _sequence_closing 263
# define _set_opening 264
# define _set_closing 265
# define _units_opening 266
# define _units_closing 267
# define _list_separator 268
# define _point_operator 269
# define _assignment_operator 270
# define _multiply_operator 271
# define _divide_operator 272
# define _exponentiate_operator 273
# define _range_operator 274
# define _date 275
# define _date_time 276
# define _date_timeV0 277
# define _integer 278
# define _name 279
# define _real 280
# define _symbol 281
# define _text_string 282
# define _time 283
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
# define YYERRCODE 256

# line 310 "odl2.y"



/* Error handling routine.  This routine is called, explicitly or
   implicitly, whenever a syntax error is detected.                         */

yyerror (error_msg)
  char *error_msg;                 /* Error message text                    */

{
  ODLPrintError (error_msg);

  return;
}
int yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 2,
	0, 1,
	-2, 0,
-1, 25,
	262, 25,
	264, 25,
	275, 25,
	276, 25,
	277, 25,
	278, 25,
	279, 25,
	280, 25,
	281, 25,
	282, 25,
	283, 25,
	-2, 0,
	};
# define YYNPROD 83
# define YYLAST 187
int yyact[]={

    67,    87,    38,    37,    36,    35,    77,    73,    27,    67,
    52,    63,   105,    34,    33,    97,    73,    31,    30,    54,
    56,    57,    52,    58,    53,    59,    60,    55,    54,    56,
    57,    52,    58,    53,    59,    60,    55,    61,    96,    51,
    67,    29,    28,    98,    85,    72,    84,    77,    90,    64,
    54,    56,    57,    52,    58,    53,    59,    60,    55,    54,
    56,    57,    52,    58,    53,    59,    60,    55,    54,    56,
    57,    52,    58,    53,    59,    60,    55,     7,    19,    20,
    21,    22,    12,   111,    26,    94,   103,    89,    66,    83,
    18,    49,    68,    74,    88,    82,    39,    99,    25,    80,
    17,   101,   102,     3,    81,   110,    23,    80,    75,    50,
    71,    24,   104,    93,    70,    69,    48,    47,    46,    45,
    43,    40,    42,    41,    62,    44,    32,    15,    13,    11,
    10,     9,     8,     6,     5,     4,     2,   100,   109,    95,
    14,    16,     1,     0,   106,    65,     0,     0,     0,     0,
   108,    40,     0,    76,     0,    44,    79,     0,   112,    78,
    86,     0,     0,     0,     0,     0,     0,    91,     0,    92,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   107 };
int yypact[]={

  -179, -1000,  -179, -1000, -1000, -1000, -1000,  -179, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000,  -172,  -271,  -228,
  -229,  -252,  -253, -1000, -1000,  -242, -1000,  -257,  -274,  -275,
  -276,  -277,  -225, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000,  -263, -1000, -1000, -1000, -1000, -1000,
 -1000,  -216, -1000, -1000, -1000, -1000, -1000,  -221, -1000, -1000,
 -1000,  -256,  -225,  -268, -1000,  -161, -1000, -1000, -1000,  -222,
  -222, -1000,  -278, -1000,  -169,  -215, -1000,  -247, -1000, -1000,
  -207, -1000, -1000, -1000, -1000,  -241, -1000,  -224, -1000, -1000,
 -1000, -1000, -1000,  -170, -1000,  -261, -1000,  -222, -1000, -1000,
  -241, -1000, -1000,  -222,  -173, -1000, -1000, -1000, -1000, -1000,
 -1000,  -222, -1000 };
int yypgo[]={

     0,   142,    92,   141,   140,   139,   138,   137,   136,   103,
   135,   134,   133,   132,   131,   130,   129,   128,   127,   126,
    96,   124,    88,   123,   122,   120,   119,   118,   117,   116,
   115,    89,   114,    46,   113,    85,   112,   110,    91,   109,
    93,   108 };
int yyr1[]={

     0,     1,     8,     8,     9,     9,     9,     9,    10,    10,
    11,    11,    12,    13,    13,    17,    17,     4,     4,    14,
    14,    18,    18,     3,     3,    19,    15,    15,    15,    21,
    16,    20,    20,    20,    20,    22,    22,    22,    22,    22,
    30,     2,    32,    26,    31,    31,    33,    34,    34,    35,
    35,     5,     5,     7,     7,     7,    36,     6,     6,    27,
    27,    27,    27,    37,    37,    28,    28,    29,    23,    23,
    38,    38,    38,    40,    40,    40,    39,    41,    41,    24,
    24,    24,    25 };
int yyr2[]={

     0,     3,     2,     4,     2,     2,     2,     4,     2,     2,
     2,     2,     3,     2,     2,     3,     7,     3,     7,     2,
     2,     3,     7,     3,     7,     1,     8,     7,     5,     1,
    10,     3,     3,     3,     3,     2,     2,     2,     2,     2,
     1,     6,     1,     6,     0,     2,     6,     2,     7,     3,
     7,     2,     5,     3,     3,     5,     2,     2,     5,     3,
     3,     3,     5,     0,     6,     3,     3,     3,     2,     2,
     5,     7,     7,     2,     6,     3,     6,     2,     4,     4,
     6,     7,     7 };
int yychk[]={

 -1000,    -1,    -8,    -9,   -10,   -11,   -12,   256,   -13,   -14,
   -15,   -16,   261,   -17,    -4,   -18,    -3,   279,   269,   257,
   258,   259,   260,    -9,    -9,   270,   256,   279,   270,   270,
   270,   270,   -19,   256,   270,   279,   279,   279,   279,   -20,
   -22,   -23,   -24,   -25,    -2,   -26,   -27,   -28,   -29,   -38,
   -39,   264,   278,   280,   275,   283,   276,   277,   279,   281,
   282,   262,   -21,   274,   265,   -40,   -22,   256,    -2,   -30,
   -32,   -37,   266,   263,   -40,   -41,   -38,   262,   -20,    -2,
   268,   265,   256,   -31,   -33,   266,   -31,   279,   263,   256,
   263,   -38,   -22,   -34,   -35,    -5,   279,   256,   267,   267,
    -7,   271,   272,   256,   -36,   273,   -33,   -35,   -33,    -6,
   278,   256,   -33 };
int yydef[]={

     0,    -2,    -2,     2,     4,     5,     6,     0,     8,     9,
    10,    11,    12,    13,    14,    19,    20,     0,     0,    15,
    17,    21,    23,     3,     7,    -2,    28,     0,     0,     0,
     0,     0,     0,    27,    29,    16,    18,    22,    24,    26,
    31,    32,    33,    34,    35,    36,    37,    38,    39,    68,
    69,     0,    40,    42,    59,    60,    61,    63,    65,    66,
    67,     0,     0,     0,    79,     0,    73,    75,    35,    44,
    44,    62,     0,    70,     0,     0,    77,     0,    30,    82,
     0,    80,    81,    41,    45,     0,    43,     0,    71,    72,
    76,    78,    74,     0,    47,    49,    51,     0,    64,    46,
     0,    53,    54,     0,     0,    56,    52,    48,    55,    50,
    57,     0,    58 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"_OBJECT",	257,
	"_END_OBJECT",	258,
	"_GROUP",	259,
	"_END_GROUP",	260,
	"_END",	261,
	"_sequence_opening",	262,
	"_sequence_closing",	263,
	"_set_opening",	264,
	"_set_closing",	265,
	"_units_opening",	266,
	"_units_closing",	267,
	"_list_separator",	268,
	"_point_operator",	269,
	"_assignment_operator",	270,
	"_multiply_operator",	271,
	"_divide_operator",	272,
	"_exponentiate_operator",	273,
	"_range_operator",	274,
	"_date",	275,
	"_date_time",	276,
	"_date_timeV0",	277,
	"_integer",	278,
	"_name",	279,
	"_real",	280,
	"_symbol",	281,
	"_text_string",	282,
	"_time",	283,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"label : statement_list",
	"statement_list : statement",
	"statement_list : statement_list statement",
	"statement : aggregation_stmt",
	"statement : assignment_stmt",
	"statement : end_statement",
	"statement : error statement",
	"aggregation_stmt : object_stmt",
	"aggregation_stmt : group_stmt",
	"assignment_stmt : attribute_stmt",
	"assignment_stmt : pointer_stmt",
	"end_statement : _END",
	"object_stmt : object_opening",
	"object_stmt : object_closing",
	"object_opening : _OBJECT",
	"object_opening : _OBJECT _assignment_operator _name",
	"object_closing : _END_OBJECT",
	"object_closing : _END_OBJECT _assignment_operator _name",
	"group_stmt : group_opening",
	"group_stmt : group_closing",
	"group_opening : _GROUP",
	"group_opening : _GROUP _assignment_operator _name",
	"group_closing : _END_GROUP",
	"group_closing : _END_GROUP _assignment_operator _name",
	"attribute_stmt : _name _assignment_operator",
	"attribute_stmt : _name _assignment_operator value",
	"attribute_stmt : _name _assignment_operator error",
	"attribute_stmt : _name error",
	"pointer_stmt : _point_operator _name _assignment_operator",
	"pointer_stmt : _point_operator _name _assignment_operator value",
	"value : scalar_value",
	"value : sequence_value",
	"value : set_value",
	"value : range_value",
	"scalar_value : integer_value",
	"scalar_value : real_value",
	"scalar_value : date_time_value",
	"scalar_value : symbolic_value",
	"scalar_value : text_string_value",
	"integer_value : _integer",
	"integer_value : _integer units_part",
	"real_value : _real",
	"real_value : _real units_part",
	"units_part : /* empty */",
	"units_part : units_expression",
	"units_expression : _units_opening units_expr _units_closing",
	"units_expr : units_factor",
	"units_expr : units_expr units_mult_op units_factor",
	"units_factor : units",
	"units_factor : units units_exp_op units_exponent",
	"units : _name",
	"units : error units_expression",
	"units_mult_op : _multiply_operator",
	"units_mult_op : _divide_operator",
	"units_mult_op : error units_expression",
	"units_exp_op : _exponentiate_operator",
	"units_exponent : _integer",
	"units_exponent : error units_expression",
	"date_time_value : _date",
	"date_time_value : _time",
	"date_time_value : _date_time",
	"date_time_value : _date_timeV0 time_zoneV0",
	"time_zoneV0 : /* empty */",
	"time_zoneV0 : _units_opening _name _units_closing",
	"symbolic_value : _name",
	"symbolic_value : _symbol",
	"text_string_value : _text_string",
	"sequence_value : sequence_1D",
	"sequence_value : sequence_2D",
	"sequence_1D : _sequence_opening _sequence_closing",
	"sequence_1D : _sequence_opening value_list _sequence_closing",
	"sequence_1D : _sequence_opening value_list error",
	"value_list : scalar_value",
	"value_list : value_list _list_separator scalar_value",
	"value_list : error",
	"sequence_2D : _sequence_opening sequence_1D_list _sequence_closing",
	"sequence_1D_list : sequence_1D",
	"sequence_1D_list : sequence_1D_list sequence_1D",
	"set_value : _set_opening _set_closing",
	"set_value : _set_opening value_list _set_closing",
	"set_value : _set_opening value_list error",
	"range_value : integer_value _range_operator integer_value",
};
#endif /* YYDEBUG */
#line 1 "/usr/lib/yaccpar"
/*	@(#)yaccpar 1.10 89/04/04 SMI; from S5R3 1.10	*/

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	{ free(yys); free(yyv); return(0); }
#define YYABORT		{ free(yys); free(yyv); return(1); }
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-1000)

/*
** static variables used by the parser
*/
static YYSTYPE *yyv;			/* value stack */
static int *yys;			/* state stack */

static YYSTYPE *yypv;			/* top of value stack */
static int *yyps;			/* top of state stack */

static int yystate;			/* current state */
static int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */

int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */


/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */
	unsigned yymaxdepth = YYMAXDEPTH;

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yyv = (YYSTYPE*)malloc(yymaxdepth*sizeof(YYSTYPE));
	yys = (int*)malloc(yymaxdepth*sizeof(int));
	if (!yyv || !yys)
	{
		yyerror( "out of memory" );
		return(1);
	}
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

	goto yystack;
	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			(void)printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			yymaxdepth += YYMAXDEPTH;
			yyv = (YYSTYPE*)realloc((char*)yyv,
				yymaxdepth * sizeof(YYSTYPE));
			yys = (int*)realloc((char*)yys,
				yymaxdepth * sizeof(int));
			if (!yyv || !yys)
			{
				yyerror( "yacc stack overflow" );
				return(1);
			}
			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			(void)printf( "Received token " );
			if ( yychar == 0 )
				(void)printf( "end-of-file\n" );
			else if ( yychar < 0 )
				(void)printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				(void)printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				(void)printf( "Received token " );
				if ( yychar == 0 )
					(void)printf( "end-of-file\n" );
				else if ( yychar < 0 )
					(void)printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					(void)printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
				yynerrs++;
			skip_init:
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						(void)printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					(void)printf( "Error recovery discards " );
					if ( yychar == 0 )
						(void)printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						(void)printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						(void)printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			(void)printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 1:
# line 97 "odl2.y"
{
                            /* End-of-file hit before END statement found */
                            ODLEndLabel ();
                            if (pds_watch_ends)
                            {
                               yyerror ("END statement is missing");
                               YYABORT;
			    }
                            else
                               YYACCEPT;
                          } break;
case 12:
# line 126 "odl2.y"
{
                            /* This is the normal termination of parsing */
                            if (ODLEndLabel ())
                              {
                                YYACCEPT;
                              }
                            else
                              {
                                YYABORT;
                              }
                          } break;
case 15:
# line 144 "odl2.y"
{ yyerror ("Missing '=' operator after OBJECT"); } break;
case 16:
# line 146 "odl2.y"
{ ODLBeginAggregate (KA_OBJECT, &yypvt[-0].item); } break;
case 17:
# line 149 "odl2.y"
{ 
                             yyval.item.value.string = NULL;
                             ODLEndAggregate (KA_OBJECT, &yyval.item);
                           } break;
case 18:
# line 154 "odl2.y"
{ ODLEndAggregate (KA_OBJECT, &yypvt[-0].item); } break;
case 21:
# line 162 "odl2.y"
{ yyerror ("Missing '=' operator after GROUP"); } break;
case 22:
# line 164 "odl2.y"
{ ODLBeginAggregate (KA_GROUP, &yypvt[-0].item); } break;
case 23:
# line 167 "odl2.y"
{ 
                             yyval.item.value.string = NULL;
                             ODLEndAggregate (KA_GROUP, &yyval.item);
                           } break;
case 24:
# line 172 "odl2.y"
{ ODLEndAggregate (KA_GROUP, &yypvt[-0].item); } break;
case 25:
# line 176 "odl2.y"
{ ODLBeginParameter (KP_ATTRIBUTE, &yypvt[-1].item); } break;
case 27:
# line 179 "odl2.y"
{ 
                             yyerror ("Bad value in assignment statement");
                             yyclearin;
                           } break;
case 28:
# line 184 "odl2.y"
{ yyerror ("Expected '=' after name"); } break;
case 29:
# line 188 "odl2.y"
{ ODLBeginParameter (KP_POINTER, &yypvt[-1].item); } break;
case 31:
# line 193 "odl2.y"
{ ODLMarkParameter (KV_SCALAR); } break;
case 32:
# line 195 "odl2.y"
{ ODLMarkParameter (KV_SEQUENCE); } break;
case 33:
# line 197 "odl2.y"
{ ODLMarkParameter (KV_SET); } break;
case 34:
# line 199 "odl2.y"
{ ODLMarkParameter (KV_SEQUENCE); } break;
case 40:
# line 210 "odl2.y"
{ ODLStoreValue (&yypvt[-0].item); } break;
case 42:
# line 215 "odl2.y"
{ ODLStoreValue (&yypvt[-0].item); } break;
case 48:
# line 228 "odl2.y"
{ ODLMarkUnits (yypvt[-1].flag); } break;
case 49:
# line 231 "odl2.y"
{ ODLStoreUnits1 (&yypvt[-0].item); } break;
case 50:
# line 233 "odl2.y"
{ ODLStoreUnits2 (&yypvt[-2].item, &yypvt[-0].item); } break;
case 52:
# line 237 "odl2.y"
{ yyerror ("Units designator must be a name"); } break;
case 53:
# line 240 "odl2.y"
{ yyval.flag = 1; } break;
case 54:
# line 242 "odl2.y"
{ yyval.flag = -1; } break;
case 55:
# line 244 "odl2.y"
{ yyerror ("Expected a '*', '/' or '**' operator");} break;
case 58:
# line 251 "odl2.y"
{ yyerror("Exponent in units expr must be integer");} break;
case 59:
# line 254 "odl2.y"
{ ODLStoreValue (&yypvt[-0].item); } break;
case 60:
# line 256 "odl2.y"
{ ODLStoreValue (&yypvt[-0].item); } break;
case 61:
# line 258 "odl2.y"
{ ODLStoreValue (&yypvt[-0].item); } break;
case 62:
# line 260 "odl2.y"
{ ODLStoreValue (&yypvt[-1].item); } break;
case 65:
# line 267 "odl2.y"
{ ODLStoreValue (&yypvt[-0].item); } break;
case 66:
# line 269 "odl2.y"
{ ODLStoreValue (&yypvt[-0].item); } break;
case 67:
# line 272 "odl2.y"
{ ODLStoreValue (&yypvt[-0].item); } break;
case 70:
# line 279 "odl2.y"
{ yyerror("Sequences with no values not allowed"); } break;
case 71:
# line 281 "odl2.y"
{ ODLCheckSequence (); } break;
case 72:
# line 283 "odl2.y"
{ 
                             yyerror("')' at end of a sequence is missing");
                             ODLCheckSequence ();
                           } break;
case 75:
# line 291 "odl2.y"
{ yyerror ("Error in value list"); } break;
case 81:
# line 303 "odl2.y"
{ yyerror ("The '}' is missing at end of set"); } break;
case 82:
# line 307 "odl2.y"
{ ODLCheckRange (&yypvt[-2].item, &yypvt[-0].item); } break;
	}
	goto yystack;		/* reset registers in driver code */
}
