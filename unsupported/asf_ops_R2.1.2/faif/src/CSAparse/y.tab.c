# define Begin 257
# define END 258
# define ARRAY 259
# define VALUE 260
# define KEYWORD 261
# define DATE 262
# define COMMENT 263

# line 23 "CSAparse.y"
#include "CSAparse.h"
char *val;                       /* string a value is stored in           */
char *big;                       /* string a text string is stored in     */
int array_flag = 0;              /* flag that an array is being processed */
int block_flag = 0;              /* flag that a block is being processed  */
struct node *root = NULL;        /* pointer to the root of the tree       */
struct node *curr = NULL;        /* pointer to the current tree node      */

struct block_ptr *curr_block = NULL;  /* pointer to current block node     */
struct block_ptr *head_block = NULL;  /* pointer to list of nested blocks  */
struct block_ptr *last_block = NULL;  /* pointer to last block in list     */
struct array_node *array_list = NULL; /* pointer to a list of values       */
struct array_node *curr_array = NULL; /* pointer to the current array node */
int count = 0;
extern yyout;

#ifdef __STDC__
#include <stdlib.h>
#include <string.h>
#else
#include <malloc.h>
#include <memory.h>
#endif

#include <values.h>

#ifdef __cplusplus

#ifndef yyerror
	void yyerror(const char *);
#endif

#ifndef yylex
#ifdef __EXTERN_C__
	extern "C" { int yylex(void); }
#else
	int yylex(void);
#endif
#endif
	int yyparse(void);

#endif
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval;
YYSTYPE yyval;
typedef int yytabelem;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#if YYMAXDEPTH > 0
int yy_yys[YYMAXDEPTH], *yys = yy_yys;
YYSTYPE yy_yyv[YYMAXDEPTH], *yyv = yy_yyv;
#else	/* user does initial allocation */
int *yys;
YYSTYPE *yyv;
#endif
static int yymaxdepth = YYMAXDEPTH;
# define YYERRCODE 256

# line 283 "CSAparse.y"


struct node *mknode()
{
  struct node *temp;
  
  temp = (NODEPTR) util_do_malloc(sizeof(struct node));
  temp->keyword    = NULL;
  temp->value      = NULL;
  temp->block      = NULL;
  temp->Array      = NULL;
  temp->vector[0]  = NULL;
  temp->vector[1]  = NULL;
  temp->vector[2]  = NULL;
  temp->vector[3]  = NULL;
  temp->vector[4]  = NULL;
  temp->vector[5]  = NULL;
  temp->vector[6]  = NULL;
  temp->next       = NULL;
  temp->prev       = NULL;

  return(temp);
}

void tree (int data_type, char *keyword, char *value) 
{

  struct node *temp;
  temp = mknode();

  switch (data_type) {

  case HDR_ASSIGN:

    temp->data_type = HDR_ASSIGN;
    temp->keyword   = keyword;
    temp->value     = value;

    /*  Added "|| (root == NULL)" for PR 859 */
    if ((curr == NULL) || (root == NULL))
      root = curr = temp;
    else
      {
	curr->next = temp;
	temp->prev = curr;
	curr = temp;
      }
    break;

  case ASSIGN:
	
    temp->data_type = ASSIGN;
    temp->keyword   = keyword;
    temp->value     = value;

    if (block_flag == 1)
      {
	curr->block = temp;
	temp->prev = curr;
	block_flag = 0;
      }
    else
      {
	curr->next = temp;
	temp->prev = curr;
      }
    curr = temp;

    break;

  case ARRAY_:

    temp->data_type = ARRAY_;
    temp->keyword   = keyword;
    temp->value     = value;
    temp->Array     = array_list;
    
    curr_array = array_list = NULL;
    
    if (block_flag == 1)
      {
	curr->block = temp;
	block_flag = 0;
      }
    else
      {
	curr->next = temp;
	temp->prev = curr;
      }
    curr = temp;
  
    break;

  case COMMENT_:
    temp->data_type = COMMENT_;
    temp->keyword   = keyword;

    /*  Added "|| (root == NULL)" for PR 859 */
    if ((curr == NULL) || (root == NULL))
      root = curr = temp;
    else if (block_flag == 1)
      {
	curr->block = temp;
	temp->prev = curr;
	block_flag = 0;
      }
    else
      {
	curr->next = temp;
	temp->prev = curr;
	curr = temp;
      }
  }    
} 

yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 33
# define YYLAST 83
yytabelem yyact[]={

     8,    24,     9,    13,    14,    13,    11,     8,    14,     9,
    35,    14,    24,    42,    38,    26,    46,    33,    25,     2,
    10,    23,    34,    45,    21,    22,    12,    20,     6,     5,
     7,     0,     1,     0,    32,    15,    16,    17,    18,    19,
    27,    28,     0,    43,    37,     4,    36,    36,    28,    28,
    28,     3,    44,    31,    48,    36,     0,     0,     0,    51,
     0,     0,    52,    36,    50,    47,    36,    29,    49,    39,
    40,    41,     0,    30,    29,    29,    29,     0,     0,     0,
    30,    30,    30 };
yytabelem yypact[]={

  -257,-10000000,  -257,  -257,  -257,  -257,  -257,  -253,-10000000,-10000000,
  -248,-10000000,   -43,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,  -250,  -253,  -248,-10000000,  -259,  -259,  -244,  -250,  -250,
  -250,  -245,  -248,-10000000,  -259,-10000000,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,  -259,  -248,-10000000,  -259,-10000000,-10000000,  -248,-10000000,
-10000000,  -248,-10000000 };
yytabelem yypgo[]={

     0,    32,    19,    51,    45,    29,    28,    10,    20,    26,
    22,    17,    30,    25,    16,    40,    24,    23 };
yytabelem yyr1[]={

     0,     1,     1,     1,     1,     1,     1,     7,     8,     9,
     6,    10,    10,    11,    11,    12,    12,    12,     2,     2,
    13,    14,     4,     4,    15,    15,    15,    15,    16,    17,
     3,     3,     5 };
yytabelem yyr2[]={

     0,     4,     4,     4,     4,     4,     0,     3,     3,     3,
     3,     3,     3,     5,     3,     7,     7,     5,     4,     2,
     1,     1,    10,    12,     4,     4,     4,     0,     1,     1,
    13,    10,    15 };
yytabelem yychk[]={

-10000000,    -1,    -2,    -3,    -4,    -5,    -6,   -12,   257,   259,
    -8,   263,    -9,   262,   261,    -1,    -1,    -1,    -1,    -1,
    -2,   -16,   -13,    -7,   260,    61,    58,   -15,    -2,    -4,
    -3,   -12,    -7,   -11,   -10,    -7,    -8,   -11,   258,   -15,
   -15,   -15,   258,    -7,   -11,   -17,   -14,   -10,    -7,   -10,
   -14,    -7,    -7 };
yytabelem yydef[]={

     6,    -2,     6,     6,     6,     6,     6,    19,    28,    20,
     0,    10,     0,     8,     9,     1,     2,     3,     4,     5,
    18,    27,     0,     0,     7,    17,     0,     0,    27,    27,
    27,     0,     0,    15,    14,    11,    12,    16,    29,    24,
    25,    26,    21,     0,    13,    31,    22,    21,     0,    30,
    23,     0,    32 };
typedef struct
#ifdef __cplusplus
	yytoktype
#endif
{ char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"Begin",	257,
	"END",	258,
	"ARRAY",	259,
	"VALUE",	260,
	"KEYWORD",	261,
	"DATE",	262,
	"COMMENT",	263,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"start : multassign start",
	"start : block start",
	"start : array start",
	"start : vector start",
	"start : comment start",
	"start : /* empty */",
	"Value : VALUE",
	"Date : DATE",
	"Keyword : KEYWORD",
	"comment : COMMENT",
	"value : Value",
	"value : Date",
	"multvalue : value multvalue",
	"multvalue : value",
	"assign : Keyword '=' multvalue",
	"assign : Keyword ':' multvalue",
	"assign : Keyword '='",
	"multassign : assign multassign",
	"multassign : assign",
	"$ACT1 : /* empty */",
	"$ACT0 : /* empty */",
	"array : ARRAY $ACT1 assign END $ACT0",
	"array : ARRAY $ACT1 assign END value $ACT0",
	"datastruct : multassign datastruct",
	"datastruct : array datastruct",
	"datastruct : block datastruct",
	"datastruct : /* empty */",
	"$ACT3 : /* empty */",
	"$ACT4 : /* empty */",
	"block : Begin $ACT3 datastruct END $ACT4 value",
	"block : Begin $ACT3 datastruct END $ACT4",
	"vector : Date Value Value Value Value Value Value",
};
#endif /* YYDEBUG */
# line	1 "/usr/ccs/bin/yaccpar"
/*
 * Copyright (c) 1993 by Sun Microsystems, Inc.
 */

#pragma ident	"@(#)yaccpar	6.12	93/06/07 SMI"

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	return(0)
#define YYABORT		return(1)
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
#define YYNEW(type)	malloc(sizeof(type) * yynewmax)
#define YYCOPY(to, from, type) \
	(type *) memcpy(to, (char *) from, yynewmax * sizeof(type))
#define YYENLARGE( from, type) \
	(type *) realloc((char *) from, yynewmax * sizeof(type))
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
#define YYFLAG		(-10000000)

/*
** global variables used by the parser
*/
YYSTYPE *yypv;			/* top of value stack */
int *yyps;			/* top of state stack */

int yystate;			/* current state */
int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */
int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */



#ifdef YYNMBCHARS
#define YYLEX()		yycvtok(yylex())
/*
** yycvtok - return a token if i is a wchar_t value that exceeds 255.
**	If i<255, i itself is the token.  If i>255 but the neither 
**	of the 30th or 31st bit is on, i is already a token.
*/
#if defined(__STDC__) || defined(__cplusplus)
int yycvtok(int i)
#else
int yycvtok(i) int i;
#endif
{
	int first = 0;
	int last = YYNMBCHARS - 1;
	int mid;
	wchar_t j;

	if(i&0x60000000){/*Must convert to a token. */
		if( yymbchars[last].character < i ){
			return i;/*Giving up*/
		}
		while ((last>=first)&&(first>=0)) {/*Binary search loop*/
			mid = (first+last)/2;
			j = yymbchars[mid].character;
			if( j==i ){/*Found*/ 
				return yymbchars[mid].tvalue;
			}else if( j<i ){
				first = mid + 1;
			}else{
				last = mid -1;
			}
		}
		/*No entry in the table.*/
		return i;/* Giving up.*/
	}else{/* i is already a token. */
		return i;
	}
}
#else/*!YYNMBCHARS*/
#define YYLEX()		yylex()
#endif/*!YYNMBCHARS*/

/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
#if defined(__STDC__) || defined(__cplusplus)
int yyparse(void)
#else
int yyparse()
#endif
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */

#if defined(__cplusplus) || defined(lint)
/*
	hacks to please C++ and lint - goto's inside switch should never be
	executed; yypvt is set to 0 to avoid "used before set" warning.
*/
	static int __yaccpar_lint_hack__ = 0;
	switch (__yaccpar_lint_hack__)
	{
		case 1: goto yyerrlab;
		case 2: goto yynewstate;
	}
	yypvt = 0;
#endif

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

#if YYMAXDEPTH <= 0
	if (yymaxdepth <= 0)
	{
		if ((yymaxdepth = YYEXPAND(0)) <= 0)
		{
			yyerror("yacc initialization error");
			YYABORT;
		}
	}
#endif

	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */
	goto yystack;	/* moved from 6 lines above to here to please C++ */

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

			printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
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
			int yynewmax;
#ifdef YYEXPAND
			yynewmax = YYEXPAND(yymaxdepth);
#else
			yynewmax = 2 * yymaxdepth;	/* double table size */
			if (yymaxdepth == YYMAXDEPTH)	/* first time growth */
			{
				char *newyys = (char *)YYNEW(int);
				char *newyyv = (char *)YYNEW(YYSTYPE);
				if (newyys != 0 && newyyv != 0)
				{
					yys = YYCOPY(newyys, yys, int);
					yyv = YYCOPY(newyyv, yyv, YYSTYPE);
				}
				else
					yynewmax = 0;	/* failed */
			}
			else				/* not first time */
			{
				yys = YYENLARGE(yys, int);
				yyv = YYENLARGE(yyv, YYSTYPE);
				if (yys == 0 || yyv == 0)
					yynewmax = 0;	/* failed */
			}
#endif
			if (yynewmax <= yymaxdepth)	/* tables not expanded */
			{
				yyerror( "yacc stack overflow" );
				YYABORT;
			}
			yymaxdepth = yynewmax;

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
		if ( ( yychar < 0 ) && ( ( yychar = YYLEX() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			printf( "Received token " );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
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
			if ( ( yychar < 0 ) && ( ( yychar = YYLEX() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				printf( "Received token " );
				if ( yychar == 0 )
					printf( "end-of-file\n" );
				else if ( yychar < 0 )
					printf( "-none-\n" );
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
					printf( "%s\n", yytoks[yy_i].t_name );
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
			skip_init:
				yynerrs++;
				/* FALLTHRU */
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
						printf( _POP_, *yy_ps,
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

					printf( "Error recovery discards " );
					if ( yychar == 0 )
						printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						printf( "token -none-\n" );
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
						printf( "token %s\n",
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
			printf( "Reduce by (%d) \"%s\"\n",
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
		
case 7:
# line 45 "CSAparse.y"
{
  /* dynamically allocate an array to store the value string */

  val = big = (char *) calloc((strlen(yylval) + 1), sizeof(char));
  strcpy(val, yylval);
  yyval = (int) val;
} break;
case 8:
# line 55 "CSAparse.y"
{
  /* dynamically allocate an array to store the date string */

  val = (char *) calloc((strlen(yylval) + 1), sizeof(char));
  strcpy(val, yylval);
  yyval = (int) val;
} break;
case 9:
# line 65 "CSAparse.y"
{
  /* dynamically allocate an array to store the keyword string */

  val = (char *) calloc((strlen(yylval) + 1), sizeof(char));
  strcpy(val, yylval);
  yyval = (int) val;
} break;
case 10:
# line 75 "CSAparse.y"
{
  /* dynamically allocate an array to store the comment string */

  val = (char *) calloc((strlen(yylval) + 1), sizeof(char));
  strcpy(val, yylval);
  tree(COMMENT_, val, NULL);
} break;
case 11:
# line 85 "CSAparse.y"
{
  struct array_node *temp;

  /* if we're processing an array, put all the values into a linked list */

  if (array_flag == 1)  
    {
      temp = (ARRAYPTR) util_do_malloc(sizeof (struct array_node));
      temp->value = (char *) yypvt[-0];
      temp->next = NULL;

      if (array_list == NULL)           /* if this is a new array */
	array_list = curr_array = temp;

      else                            /* if we're adding to the current array */
	{
	  curr_array->next = temp;
	  curr_array = temp;
	}
    }
} break;
case 12:
# line 107 "CSAparse.y"
{
  struct array_node *temp;

  /* if we're processing an array, put all the dates into a linked list    */
  /* NOTE: dates and values can go into the same list (a date is a value)  */

  if (array_flag == 1)
    {
      temp = (ARRAYPTR) util_do_malloc(sizeof (struct array_node));
      temp->value = (char *) yypvt[-0];
      temp->next = NULL;

      if (array_list == NULL)            /* if this is a new array */
	array_list = curr_array = temp;

      else                           /* if we're adding to the current array */
	{
	  curr_array->next = temp;
	  curr_array = temp;
	}
    } 
} break;
case 13:
# line 132 "CSAparse.y"
{
  char *temp;
  char *temp2;
  temp2 = (char *) yypvt[-0];
  count++;
  
  /*  if we're not processing an array, concatenate all the values into one
   *  string 
   */

  if (array_flag == 0)
    {
      temp = (char *) calloc((strlen(yypvt[-0])+1), sizeof(char));

      /* You must prepend the new value to the existing string */

      strcpy(temp, big);
      free(big);
      big = (char *)util_do_malloc((strlen(yypvt[-1]) + strlen(temp) + 1)*sizeof(char));
      strcpy(big, yypvt[-1]);
      strcat(big, " ");
      strcat(big, temp);
      free(temp);
      free((char *) yypvt[-1]);
      yyval = (int) big;
    }
  
} break;
case 14:
# line 161 "CSAparse.y"
{
} break;
case 15:
# line 166 "CSAparse.y"
{

  if (array_flag == 1)     /* insert an array into the tree structure */
    tree(ARRAY_, (char *) yypvt[-2], (char *) yypvt[-0]);
  else                     /* insert an assignment into the tree structure */
    tree(ASSIGN, (char *) yypvt[-2], (char *)yypvt[-0]);
} break;
case 16:
# line 174 "CSAparse.y"
{
  /* insert a header assignment into the tree structure */

  tree(HDR_ASSIGN, (char *) yypvt[-2], (char *) yypvt[-0]);

} break;
case 17:
# line 181 "CSAparse.y"
{
  /*  insert an empty assignment into the tree structure
   *  (empty meaning keyword with missing value)
   */

  if (array_flag == 1) 
    tree(ARRAY_, (char *) yypvt[-1], NULL);
  else
    tree(ASSIGN, (char *) yypvt[-1], NULL);
} break;
case 20:
# line 196 "CSAparse.y"
{
  array_flag = 1;  /* set the flag to signal we're processing an array */
} break;
case 21:
# line 202 "CSAparse.y"
{
  
  array_flag = 0;  /* reset the flag to zero */
} break;
case 28:
# line 213 "CSAparse.y"
{

  /* This is called when a "begin" is found */

  struct node *temp;         /* temporary tree node pointer */
  struct block_ptr *btemp;   /* temporary block node pointer */

  temp = mknode();
  temp->data_type = 3; 
  btemp = (BLOCKPTR) util_do_malloc(sizeof(struct block_ptr));
  curr->next = temp;
  temp->prev = curr;
  btemp->block = curr = temp;
  
  if (curr_block == NULL)
    {
      btemp->next = btemp->prev = NULL;
      last_block = curr_block = btemp;
      if (!head_block)
	head_block = btemp;
    }
  else
    {
      btemp->next = NULL;
      btemp->prev = curr_block;
      curr_block = last_block = btemp;
    }
  
  block_flag = 1;
} break;
case 29:
# line 246 "CSAparse.y"
{
   curr = curr_block->block; 
   curr_block = curr_block->prev; 
} break;
case 30:
# line 253 "CSAparse.y"
{
  char *temp;
  temp = (char *) yypvt[-0];

  if (yypvt[-0])
    free((char *) yypvt[-0]);
} break;
case 32:
# line 264 "CSAparse.y"
{
  struct node *temp;
  temp = mknode();
  
  temp->vector[0] = (char *)yypvt[-6];
  temp->vector[1] = (char *)yypvt[-5];
  temp->vector[2] = (char *)yypvt[-4];
  temp->vector[3] = (char *)yypvt[-3];
  temp->vector[4] = (char *)yypvt[-2];
  temp->vector[5] = (char *)yypvt[-1];
  temp->vector[6] = (char *)yypvt[-0];
  
  temp->data_type = 5;
  curr->next = temp;
  temp->prev = curr;
  curr = temp;
} break;
# line	532 "/usr/ccs/bin/yaccpar"
	}
	goto yystack;		/* reset registers in driver code */
}

