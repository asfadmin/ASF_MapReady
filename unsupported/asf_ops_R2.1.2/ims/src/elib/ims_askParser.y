/**************************************************************************
**
** File:      ims_askParser.y
** 
** Date:      August 4, 1991
**
** Creator:   H. Sayah
**
** Function:  Creates a parser to validate the syntax of the keyword-value
**            pair search criteria specified by the caller program.  The
**            valid langauge for the search criteria is specifed below in 
**            BNF langauge.  The created parser also accepts as input the
**            keyword policy infomation for a given file type and ensures
**            that all policy criteria are enforced.
**
** Modified:  February 16, 1993 - S. Hardman
**            CR 3623 - Modified the processValue() function to allow
**            the strings "NULL", "UNK", and "N/A" to be entered as
**            keyword values.
**
**            March 1995 - H. Sayah - R1B
**            ASF adaptation of code inherited from AMMOS-CDB.
**
** Input:     Input to the parser routine is a list of keyword policy 
**            infomation and a string buffer specifing the keyword search
**            criteria.
** 
** Output:    Output from the parser routine is a dynamically created SQL
**            statement to query files from the catalog database based on
**            keyword and values specified.
**
** Note:       
**            The parser works with a lexical analyzer lexer.c obtained 
**            by inputting rules for tokenizing input lexer.l through 
**            UNIX's lex utility.
**
**	           Binary operators are: "=",">","<",">=", "<=", "like".
**            Unary operators are: "max", "min".
**            Boolean operators are: "and", "or".
** 
**	           White spaces between the attributes, operators, and values 
**            are ignored. 
**
**            Memory is allocated for the tokens identified by the lexer
**            program and thus must be freed here in the parser program.
**	
***************************************************************************/


/*
** The chunk of codes following the "%{" string upto "}%" will be 
** included into the parser generated code by yacc.
*/

%{
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

%}

%union	{
	STRING	string;
};

%token	<string>	T_STRING
%token	<string>	T_QSTRING
%left 	<string>	BL_OPERATOR
%token	<string>	UN_OPERATOR
%token	<string> WHERE
%token	<string>	BI_OPERATOR
%token	<string> OPEN_PARAN
%token 	<string> CLOSE_PARAN

%%

/*
** Language syntax specification.
*/
phrase			:	un_phrase
					|	bi_phrase
					|	phrase bl_operator phrase
					|	open_pr phrase close_pr 
					;

bi_phrase		:	keyword  bi_operator value 
					;

bii_phrase		:	bi_phrase
					|	bii_phrase bl_operator bii_phrase
					|	open_pr bii_phrase close_pr
					;

un_phrase		:	'[' un_operator keyword ']' 
							{ sprintf (sPtr, "(status = %d)) ", FTS_ONLINE_STATUS); 
								sPtr = sPtr + strlen(sPtr); }
					|	'[' un_operator keyword where bii_phrase ']'
							{ sprintf (sPtr, ") and (status = %d)) ", FTS_ONLINE_STATUS); 
								sPtr = sPtr + strlen(sPtr); }
					;

bi_operator		:	BI_OPERATOR 
						{ status = processBiOperator ($1); (void) free($1); 
							if (status < IMS_OK) return (status); }
					;

un_operator		:	UN_OPERATOR 
						{ processUnOperator ($1); (void) free($1); }
					;

bl_operator		:	BL_OPERATOR 
						{ processBlOperator ($1); (void) free($1); }
					;

keyword			:	T_STRING
						{ status = processKeyword ($1); (void) free($1);
							if (status < IMS_OK) return (status); }
					;

value				:	T_STRING
						{ status = processValue ($1); (void) free($1);
							if (status < IMS_OK) return (status); }
					|	T_QSTRING
						{ status = processValue ($1); (void) free($1);
							if (status < IMS_OK) return (status); }
					;

where				:	WHERE 
						{ sprintf (sPtr, "( "); sPtr = sPtr + strlen(sPtr); 
							(void) free($1); }
					;

open_pr			:	OPEN_PARAN
						{ sprintf (sPtr, "( "); sPtr = sPtr + strlen(sPtr); 
							(void) free($1); }
					;

close_pr			:	CLOSE_PARAN
						{ sprintf (sPtr, ") "); sPtr = sPtr + strlen(sPtr);
							(void) free($1); }
					;
%%

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

