
# line 56 "ims_askParser.y"
static char *sccs = "@(#)ims_askParser.y	5.1  16 Mar 1996";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_keyword.h>
#include <ims_msg.h>
#include <ims_timeConv.h>
#include <ims_ask.h>
#include <ims_askLexer.h> 
#include <ims_util.h>

#define FTS_ONLINE_STATUS 1
#define MAX_TABLE_COUNTER 16

/*
** global variables
*/
static char *sPtr = (char *)NULL;     /* pointer to the sqlBuffer */
static int unFlag;                    /* unary operator flag */
static char unOperator[IMS_COL255_LEN+1];   /* storage for unary operator */
static char workArea[IMS_COL255_LEN + IMS_COL255_LEN + 1];
static ASK_KEYWORD_LIST *glbKeywordList = (ASK_KEYWORD_LIST *)NULL;
static ASK_KEYWORD_LIST *glbCurrKey = (ASK_KEYWORD_LIST *)NULL;
static ASK_KEYWORD_POLICY *glbKeywordPolicy = (ASK_KEYWORD_POLICY *)NULL;
static int status;
static IMS_MSG_STRUCT *glbMsgDesc;    /* global msgDesc pointer */
static int tableCounter;              /* table counter; Maximum 16 */
static int keywordCounter;            /* keyword counter */

/*
** Static function declarations.
*/
static int processKeyword ();
static int validateKeyword ();
static int processValue ();
static int processBiOperator ();
static void processUnOperator ();
static void processBlOperator ();
static void yyerror ();

/*
** External variables
*/
/* 
** token that is passed to the parser from the lexical analyzer 
*/
extern	char	yytext[];	

/************************************************************************
** 
** The following routines are called by only the parser
**
*************************************************************************/

/************************************************************************
**
** yyerror - routine for parser to call when it encountered an error. 
**
*************************************************************************/
static	void	yyerror(yyErrBuf)
char *yyErrBuf;
{
extern	int	yylineno;
/*extern	char	yysbuf[];*/

	(void) ims_msg(glbMsgDesc, IMS_ERROR,
		"Error '%s': Keyword search syntax error detected at line %d near '%s'",
		yyErrBuf, yylineno,yytext);
	return;
}

/*************************************************************************
**
** validateKeyword - validate extracted keyword from the command
**                   buffer.
**
**************************************************************************/
static int validateKeyword (keyword)
char *keyword;
{
	glbCurrKey = glbKeywordList;
	while (glbCurrKey != (ASK_KEYWORD_LIST *)NULL)
	{
		if (strcmp (keyword, glbCurrKey->keyword) == 0)
		{
			if ((glbCurrKey->significance == IMS_MANDATORY_INDEXED) || 
				(glbCurrKey->significance == IMS_MANDATORY_NOT_INDEXED) ||
				(glbCurrKey->significance == IMS_OPTIONAL_INDEXED) ||
				(glbCurrKey->significance == IMS_OPTIONAL_NOT_INDEXED))

			{
				return (IMS_OK);
			}
			return (IMS_ERROR);
		}
		glbCurrKey = glbCurrKey->next;
	}
	return (IMS_ERROR);
}

/************************************************************************
**
** processKeyword - process keyword token extracted from the 
**                  command buffer.
**
*************************************************************************/
static int processKeyword (keyword)
char *keyword;
{
	/*
	** validate the keyword here before proceeding furthur.
	*/
	if (validateKeyword (keyword) < IMS_OK)
	{
		(void) ims_msg (glbMsgDesc, IMS_ERROR,
			"Keyword '%s' is not a valid search key for '%s, %s, %s'.",
			keyword, glbKeywordPolicy->platform, glbKeywordPolicy->sensor, 
			glbKeywordPolicy->dataset);
		return (IMS_ERROR);
	}

	/* increment keywordCounter */
	keywordCounter++;

	if (unFlag)
	{
		(void) sprintf (sPtr, 
			"%s = (select %s(%s) from %s where ",
			keyword, unOperator, keyword, glbKeywordPolicy->granules_table);
		unFlag = 0;
		if ((tableCounter += 1) > MAX_TABLE_COUNTER)
		{
			(void) ims_msg (glbMsgDesc, IMS_ERROR,
				"The criteria expression is not acceptable; too many table names are required.  The number of 'min' and 'max' expressions must not exceed '%d'.",
				(int)(MAX_TABLE_COUNTER - 1));
			return (IMS_ERROR);
		}
	}
	else
	{
		(void) sprintf (sPtr, "%s ", keyword);
	}
	sPtr = sPtr + strlen (sPtr);
	return (IMS_OK);
}

/**************************************************************************
**
** processValue - process keyword value extracted from the command 
**                buffer.
**
** CR 3623 - Allow valid null strings ("NULL", "UNK", and "N/A") to be
**	     used as values for keywords.  For each case use the Sybase
**	     'NULL' as the query value.
**
**************************************************************************/
static int processValue (value)
char *value;
{
	int length, maxLength, status;
	char tempValue[IMS_COL255_LEN + IMS_COL255_LEN +1];
	IMS_NUMERIC_DATE numericDate;
	char *p;

	switch (glbCurrKey->data_type)
	{
		case IMS_INT1_TYPE: /* INTEGER */
		case IMS_INT2_TYPE: 
		case IMS_INT4_TYPE: 
			if (!ims_isInteger (value))
			{
				if (ims_isNullStr (ims_removeQuotes (value)))
				{
					(void) sprintf (sPtr, "%s ", "NULL");
				}
				else
				{
					(void) ims_msg (glbMsgDesc, IMS_ERROR,
						"Keyword '%s' does not have a valid integer type value.",
						glbCurrKey->keyword);
					return (IMS_ERROR);
				}
			}
			else
			{
				(void) sprintf (sPtr, "%s ", value);
			}
			sPtr = sPtr + strlen(sPtr);
			break;
		case IMS_FLOAT4_TYPE: /* FLOAT */
		case IMS_FLOAT8_TYPE: 
			if (!ims_isReal (value))
			{
				if (ims_isNullStr (ims_removeQuotes (value)))
				{
					(void) sprintf (sPtr, "%s ", "NULL");
				}
				else
				{
					(void) ims_msg (glbMsgDesc, IMS_ERROR,
						"Keyword '%s' does not have a valid real type value.",
						glbCurrKey->keyword);
					return (IMS_ERROR);
				}
			}
			else
			{
				(void) sprintf (sPtr, "%s ", value);
			}
			sPtr = sPtr + strlen(sPtr);
			break;
		case IMS_CHAR_TYPE:   /* CHARACTER */
		case IMS_SYMBOL_TYPE: /* SYMBOL */
		case IMS_STRING_TYPE: /* STRING */
			if (glbCurrKey->max_len == 0 || glbCurrKey->max_len > IMS_COL255_LEN) 
			{
				maxLength = IMS_COL255_LEN;
			}
			else
			{
				maxLength = glbCurrKey->max_len;
			}

			/*
			** Remove any quotes and determine length.
			*/

			length = strlen (ims_removeQuotes (value));

			/*
			** Check for maximum allowable length.
			*/

			if (length > maxLength)
			{
				(void) ims_msg (glbMsgDesc, IMS_ERROR,
					"Exceeded maximum value length of '%d' for keyword '%s'.",
					maxLength, glbCurrKey->keyword);
				return (IMS_ERROR);
			}

			/*
			** Check for a valid 'NULL' string.
			*/

			if (ims_isNullStr (value))
			{
				(void) sprintf (sPtr, "%s ", "NULL");
			}
			else
			{
				/*
				** Prepare for sybase character string
				** data entry.
				*/

				(void) ims_formatQuotedString (value, tempValue);
				(void) sprintf (sPtr, "'%s' ", tempValue);
			}
			sPtr = sPtr + strlen(sPtr);
			break;

		case IMS_DATETIME_TYPE: 
			length = glbCurrKey->max_len;
			if ((length > 0) && ((int) strlen (value) > length))
			{
				(void) ims_msg (glbMsgDesc, IMS_ERROR,
					"Exceeded maximum value length of '%d' for keyword '%s'.",
					length, glbCurrKey->keyword);
				return (IMS_ERROR);
			}

			/*
			** Check for valid 'NULL' string.
			*/

			(void) strcpy (tempValue, value);

			if (ims_isNullStr (ims_removeQuotes (tempValue)))
			{
				(void) sprintf (sPtr, "%s ", "NULL");
			}
			else
			{
				/*
				** Convert time type into dbms representation
				*/	

				if ((status = ims_timeToNumericDate 
					(glbMsgDesc, tempValue, &numericDate)) < IMS_OK)
				{
					(void) ims_msg (glbMsgDesc, status,
						"Invalid date/time format value for keyword '%s'.",
						glbCurrKey->keyword);
					return (status);
				}
				else
				{
					(void) ims_numericDateToDBMSA (&numericDate, tempValue);
					(void) sprintf (sPtr, "'%s' ", tempValue);
				}
			}
			sPtr = sPtr + strlen(sPtr);
			break;
		case IMS_DOYTIME_TYPE: 
			length = glbCurrKey->max_len;
			if ((length > 0) && ((int) strlen (value) > length))
			{
				(void) ims_msg (glbMsgDesc, IMS_ERROR,
					"Exceeded maximum value length of '%d' for keyword '%s'.",
					length, glbCurrKey->keyword);
				return (IMS_ERROR);
			}

			/*
			** Check for valid 'NULL' string.
			*/

			(void) strcpy (tempValue, value);

			if (ims_isNullStr (ims_removeQuotes (tempValue)))
			{
				(void) sprintf (sPtr, "%s ", "NULL");
			}
			else
			{
				/*
				** Convert time type into dbms hex
				** representation.
				*/	

				if ((status = ims_timeToNumericDate 
					(glbMsgDesc, tempValue, &numericDate)) < IMS_OK)
				{
					(void) ims_msg (glbMsgDesc, status,
						"Invalid date/time format value for keyword '%s'.",
						glbCurrKey->keyword);
					return (status);
				}
				else
				{
					(void) sprintf (sPtr, "'%s' ", tempValue);
				}
			}
			sPtr = sPtr + strlen(sPtr);
			break;
		default:
			(void) ims_msg (glbMsgDesc, IMS_FATAL,
				"keyword '%s' has an invalid data type.",
				glbCurrKey->keyword);
			return (IMS_FATAL);
	}
	return (IMS_OK);
}

/*************************************************************************
**
** processUnOperator - process unary operator extracted from the
**                     command buffer.
**
**************************************************************************/
static void processUnOperator (operator)
char *operator;
{
	unFlag = 1;
	(void) sprintf (unOperator, "%s", operator);
}

/**************************************************************************
**
** processBiOperator - process binary operator extracted from the
**                     command buffer.
**
***************************************************************************/
static int processBiOperator (operator)
char *operator;
{
	/*
	** We must make sure that like operator is used only for 
	** keywords with chacacter data type values.
	*/
	if ( strcmp (operator, "like") == 0)
	{
		if ((glbCurrKey->data_type != IMS_CHAR_TYPE) &&
			(glbCurrKey->data_type != IMS_STRING_TYPE) &&
			(glbCurrKey->data_type != IMS_SYMBOL_TYPE))
		{
			(void) ims_msg (glbMsgDesc, IMS_ERROR, 
				"Binary operator 'like' must have character type operands.");
			return (IMS_ERROR);
		}
	}
	(void) sprintf (sPtr, "%s ", operator);
	sPtr = sPtr + strlen (sPtr);
	return (IMS_OK);
}

/**************************************************************************
**
** processBlOperator - process boolean operator extracted from the
**                     command buffer.
**
***************************************************************************/
static void processBlOperator (operator)
char *operator;
{
	(void) sprintf (sPtr, "%s ", operator);
	sPtr = sPtr + strlen (sPtr);
}


# line 472 "ims_askParser.y"
typedef union
#ifdef __cplusplus
	YYSTYPE
#endif
	{
	STRING	string;
} YYSTYPE;
# define T_STRING 257
# define T_QSTRING 258
# define BL_OPERATOR 259
# define UN_OPERATOR 260
# define WHERE 261
# define BI_OPERATOR 262
# define OPEN_PARAN 263
# define CLOSE_PARAN 264

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

# line 552 "ims_askParser.y"


/************************************************************************
** 
** The following routines are avaiable to other submodules
**
*************************************************************************/
/************************************************************************
**
** ims_parseKwBuffer 
** This routine call the parsing yacc parser to parse the keywordlist.
** The parsed keyword value pairs are put in the keywordTable.
**
*************************************************************************/
int ims_parseKwBuffer(msgDesc, kwPolicy, kwBuffer, sqlBuffer)
IMS_MSG_STRUCT *msgDesc;
ASK_KEYWORD_POLICY *kwPolicy;
char *kwBuffer;
char *sqlBuffer;
{
	char *kPtr; /* temporary pointer */
	ASK_KEYWORD_LIST *currKeyword;

	/* 
	** defined and declared in adLexer.l - indicate if input to 
	** the keywordlist parser is comming from memory of file (fp) 
	*/
	extern	enum LEX_INPUT_SOURCE	lexinput;	

	/* 
	** if input to the parser comes from memory this variable must be 
	** set to the address of the memory of keyword list input 
	*/
	extern	char	*lexinput_memptr; 

	/* 
	** current line # of input during parsing 
	*/
	extern	int	yylinno;	

	/* Terminate sqlBuffer area. */
	*sqlBuffer = '\0';

	/* initialize */
	glbMsgDesc = msgDesc;

	/* Check for policy information */
	if (kwPolicy == (ASK_KEYWORD_POLICY *)NULL)
	{
		(void) ims_msg (glbMsgDesc, IMS_FATAL,
			"Stopped parsing: keyword policy information is not setup.");
		return (IMS_FATAL);
	}

	/* Initialize varriables. */
	tableCounter = 1;
	keywordCounter = 0;
	sPtr = sqlBuffer;
	kPtr = kwBuffer;
	glbKeywordList = kwPolicy->keywordList;
	glbKeywordPolicy = kwPolicy;

	(void) sprintf (sPtr, 
		"select dataset_idx, granule_idx, name, version, data_kbytes, metadata_kbytes from %s where ", 
		kwPolicy->granules_table); 
	sPtr = sPtr + strlen(sPtr);

	/* 
	** Keyword buffer is empty.
	*/
	if (kPtr == (char *)NULL)
	{
		(void) sprintf (sPtr, "(status = %d) order by name ", FTS_ONLINE_STATUS);
		sPtr = sPtr + strlen (sPtr);
		return (IMS_OK);
	}
	/* Ignore leading blanks. */
	SKIPBLANKS(kPtr);
	if ((int) strlen(kPtr) <= 0)
	{
		(void) sprintf (sPtr, "(status = %d) order by name ", FTS_ONLINE_STATUS);
		sPtr = sPtr + strlen (sPtr);
		return (IMS_OK);
	}

	/*
	** Parses the keyword list.  If successful, each keyword value pair
	** will be put in linked list
	** But we must first set the input of adLexer to come from keywordList
	*/
	lexinput = MEM_PTR;
	lexinput_memptr = kPtr;
	if (yyparse() != 0)
	{
		/* Skip over remaining tokens; empty token buffer. */
		while (yylex () != 0) (void) free (yylval.string);
		return(IMS_ERROR);
	}
	else
	{
		if (keywordCounter > 0)
		{
			(void) sprintf (sPtr, "and (status = %d) order by name ",
				FTS_ONLINE_STATUS);
		}
		else
		{
			(void) sprintf (sPtr, "(status = %d) order by name ",
				FTS_ONLINE_STATUS);
		}
		sPtr = sPtr + strlen(sPtr);
		return(IMS_OK);
	}
}

yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
	};
# define YYNPROD 20
# define YYLAST 176
yytabelem yyact[]={

    10,     5,     8,    13,    23,    18,    15,    10,     7,    30,
    21,    22,     8,     9,     4,    17,     6,     1,    24,    12,
    26,    20,    11,     3,    14,     2,     0,    16,     0,    19,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    28,
    29,     0,     0,    28,    28,    29,    29,    33,    27,    31,
    32,     0,    27,    27,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     8,     0,     0,
     0,     0,    25,     7,     0,    10 };
yytabelem yypact[]={

   -90,  -252,-10000000,-10000000,   -90,  -257,  -256,-10000000,-10000000,   -90,
-10000000,  -259,  -245,-10000000,  -247,-10000000,  -252,-10000000,-10000000,   -89,
-10000000,-10000000,-10000000,-10000000,  -255,-10000000,   -84,-10000000,  -255,  -255,
-10000000,  -259,  -252,-10000000 };
yytabelem yypgo[]={

     0,    17,    25,    23,    13,    14,    15,    16,    24,    21,
    20,    19,    18 };
yytabelem yyr1[]={

     0,     1,     1,     1,     1,     3,    10,    10,    10,     2,
     2,     8,    11,     4,     7,     9,     9,    12,     5,     6 };
yytabelem yyr2[]={

     0,     2,     2,     6,     6,     6,     2,     6,     6,     9,
    13,     3,     3,     3,     3,     3,     3,     3,     3,     3 };
yytabelem yychk[]={

-10000000,    -1,    -2,    -3,    -5,    91,    -7,   263,   257,    -4,
   259,    -1,   -11,   260,    -8,   262,    -1,    -6,   264,    -7,
    -9,   257,   258,    93,   -12,   261,   -10,    -3,    -5,    -4,
    93,   -10,   -10,    -6 };
yytabelem yydef[]={

     0,    -2,     1,     2,     0,     0,     0,    18,    14,     0,
    13,     0,     0,    12,     0,    11,     3,     4,    19,     0,
     5,    15,    16,     9,     0,    17,     0,     6,     0,     0,
    10,     0,     7,     8 };
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
	"T_STRING",	257,
	"T_QSTRING",	258,
	"BL_OPERATOR",	259,
	"UN_OPERATOR",	260,
	"WHERE",	261,
	"BI_OPERATOR",	262,
	"OPEN_PARAN",	263,
	"CLOSE_PARAN",	264,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"phrase : un_phrase",
	"phrase : bi_phrase",
	"phrase : phrase bl_operator phrase",
	"phrase : open_pr phrase close_pr",
	"bi_phrase : keyword bi_operator value",
	"bii_phrase : bi_phrase",
	"bii_phrase : bii_phrase bl_operator bii_phrase",
	"bii_phrase : open_pr bii_phrase close_pr",
	"un_phrase : '[' un_operator keyword ']'",
	"un_phrase : '[' un_operator keyword where bii_phrase ']'",
	"bi_operator : BI_OPERATOR",
	"un_operator : UN_OPERATOR",
	"bl_operator : BL_OPERATOR",
	"keyword : T_STRING",
	"value : T_STRING",
	"value : T_QSTRING",
	"where : WHERE",
	"open_pr : OPEN_PARAN",
	"close_pr : CLOSE_PARAN",
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
		
case 9:
# line 505 "ims_askParser.y"
{ sprintf (sPtr, "(status = %d)) ", FTS_ONLINE_STATUS); 
								sPtr = sPtr + strlen(sPtr); } break;
case 10:
# line 508 "ims_askParser.y"
{ sprintf (sPtr, ") and (status = %d)) ", FTS_ONLINE_STATUS); 
								sPtr = sPtr + strlen(sPtr); } break;
case 11:
# line 513 "ims_askParser.y"
{ status = processBiOperator (yypvt[-0].string); (void) free(yypvt[-0].string); 
							if (status < IMS_OK) return (status); } break;
case 12:
# line 518 "ims_askParser.y"
{ processUnOperator (yypvt[-0].string); (void) free(yypvt[-0].string); } break;
case 13:
# line 522 "ims_askParser.y"
{ processBlOperator (yypvt[-0].string); (void) free(yypvt[-0].string); } break;
case 14:
# line 526 "ims_askParser.y"
{ status = processKeyword (yypvt[-0].string); (void) free(yypvt[-0].string);
							if (status < IMS_OK) return (status); } break;
case 15:
# line 531 "ims_askParser.y"
{ status = processValue (yypvt[-0].string); (void) free(yypvt[-0].string);
							if (status < IMS_OK) return (status); } break;
case 16:
# line 534 "ims_askParser.y"
{ status = processValue (yypvt[-0].string); (void) free(yypvt[-0].string);
							if (status < IMS_OK) return (status); } break;
case 17:
# line 539 "ims_askParser.y"
{ sprintf (sPtr, "( "); sPtr = sPtr + strlen(sPtr); 
							(void) free(yypvt[-0].string); } break;
case 18:
# line 544 "ims_askParser.y"
{ sprintf (sPtr, "( "); sPtr = sPtr + strlen(sPtr); 
							(void) free(yypvt[-0].string); } break;
case 19:
# line 549 "ims_askParser.y"
{ sprintf (sPtr, ") "); sPtr = sPtr + strlen(sPtr);
							(void) free(yypvt[-0].string); } break;
# line	532 "/usr/ccs/bin/yaccpar"
	}
	goto yystack;		/* reset registers in driver code */
}

