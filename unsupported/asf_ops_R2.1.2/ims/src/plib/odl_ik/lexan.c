# include <stdio.h>
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin, *yyout;
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;

/*****************************************************************************

 Description: This file contains the lexical analyzer for the Object
              Description Language. The lexical analyzer is created
              using Lex and modifications to the lexical analysis of
              ODL should be made by modifying the Lex input file and
              not the C-language version of the analyzer produced by
              Lex.

 Author:  Randy Davis, University of Colorado LASP

 Creation Date: 17 April 1990
 Last Modified: 18 May 1991
 
 History:

   Creation - This routine was introduced in Version 1 of the ODLC library.

   Version 2.0 - 30 August 1990 - R. Davis, U. of Colorado LASP
     a) Upgraded to ODL Version 2.  The biggest change is the support
        of groups.  Support for ODL Version 1 features not found in
        Version 2 -- like the range operator -- remain so that the
        lexical analyzer can handle older labels as well as new ones.
     b) Added support for ODL Version 0 date and time format. 

   Version 2.1 - 13 March 1991 - R. Davis, U. of Colorado LASP
     a) Changed to a more general way to turn tokens into 'value data'
        structures by converting from the older ODLxToken routines to
        ODLConvertX routines.  Processing of string tokens was moved into
        a new action routine named yyGetStringToken.
     b) Added recognition of two values that are often entered incorrectly
        by users: file names without quotation marks (which are turned into
        strings); and the symbol N/A (for Not Applicable, which is turned 
        into a quoted symbol).
     c) Saved comments so they can be attached as annotation to the ODL tree.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    Removed ODL function prototypes which are now in include files.
    Added include file odlinter.h.

  Version 2.3 - 13 October 1991 - M. DeMore, Jet Propulsion Laboratory
     Removed code in yyGetStringToken which used to process '\t' and
     `\n`.  They are now transferred exactly as is to the output string
     and are handled by the output routines instead.  This was done to
     prevent the lexer from eating backslashes in DOS file names.

******************************************************************************/

#include "odldef.h"
#include "odlparse.h"
#include "odlinter.h"

/* This was moved from rdvalue.c so that other modules could be independent
   of rdvalue */

int nc;

/* The following are for the dynamic string allocation routines */

# define ODL_BUFF_INCREMENT BUFSIZ

int ODL_buf_pos = 0;
long ODL_buf_size = 0;
char *temp_buf = NULL;
char *ODL_buffer = NULL;

char *ODLNewString ();
char ODLPeekString ();
char ODLBackupString ();
char *ODLStoreString ();
void ODLKillString ();
int ODLStringLength ();


/* The following are warning messages */

#define MESSAGE1 "Value is assumed to be a file name -- converted to a string"
#define MESSAGE2 "Value N/A is not a name -- will appear within single quotes"
#define MESSAGE3 "BEGIN_GROUP statement found.  Will be converted to GROUP"
#define MESSAGE4 "BEGIN_OBJECT statement found.  Will be converted to OBJECT"

/* The following global variable is defined in the module containing the
   parser action routines */

/* The following global variable is defined in the module containing the
   parser action routines */

/* >>>>>>>>>>>>>>>>>>>>>>>>>> BEGIN CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */
/* >>>>> MDD 5/19/91  Added extern statement to the following      >>>>> */
/* >>>>>              variable because it is declared in parsact   >>>>> */
/* >>>>>              and the PC compiler is unhappy.              >>>>> */
/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

extern char       *ODLcurrent_comment;  /* Most recently recognized comment */

/* >>>>>>>>>>>>>>>>>>>>>>>>>> END OF CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */

/* The following routine processes string tokens.  This routine is used
   because ODL strings can be very long, rendering LEX's regular
   expression mechanism for getting tokens inefficient */

char *yyGetStringToken ();

/* The following routine processes comments within an ODL label */

void  yyGetComment ();


# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
                             {
                                     return (_END);
                                    }
break;
case 2:
                    {
                                     return (_END_GROUP);
                                    }
break;
case 3:
                   {
                                     return (_END_OBJECT);
                                    }
break;
case 4:
                            {
                                     return (_GROUP);
                                    }
break;
case 5:
                  {
                                     ODLPrintWarning(MESSAGE3);
                                     return (_GROUP);
                                    }
break;
case 6:
                           {
                                     return (_OBJECT);
                                    }
break;
case 7:
                 {
                                     ODLPrintWarning(MESSAGE4);
                                     return (_OBJECT);
                                    }
break;
case 8:
{
                                     yylval.item =
                                        ODLConvertSymbol (yytext, yyleng, 1);
                                     return (_name);
                                    }
break;
case 9:
                          {
                                     yylval.item =
                                       ODLConvertSymbol(&yytext[1],yyleng-2,2);
                                     return (_symbol);
                                    }
break;
case 10:
                                 {
                                     temp_buf = yyGetStringToken ();
                                     yylval.item = 
                                        ODLConvertString (temp_buf, 
                                              ODLStringLength ());
                                     ODLKillString ();
                                     return (_text_string);
                                    }
break;
case 11:
          {
                                       /* This is the low part of an ODL
                                          Version 1 range value */
                                     yylval.item =
                                        ODLConvertInteger (yytext, yyleng);
                                     return (_integer);
                                    }
break;
case 12:
               {
                                     yylval.item =
                                        ODLConvertInteger (yytext, yyleng);
                                     return (_integer);
                                    }
break;
case 13:
{
                                     yylval.item =
                                        ODLConvertInteger (yytext, yyleng);
                                     return (_integer);
                                    }
break;
case 14:
 {
                                     yylval.item = 
                                        ODLConvertReal (yytext, yyleng);
                                     return (_real);
                                    }
break;
case 15:
                             {
                                     yylval.item =
                                        ODLConvertDate (yytext, yyleng);
                                     return (_date);
                                    }
break;
case 16:
                           {
                                     yylval.item =
                                        ODLConvertDate (yytext, yyleng);
                                     return (_date);
                                    }
break;
case 17:
                    {
                                     yylval.item =
                                        ODLConvertTime (yytext, yyleng);
                                     return (_time);
                                    }
break;
case 18:
          {
                                     yylval.item =
                                        ODLConvertDateTime (yytext, yyleng);
                                     return (_date_time);
                                    }
break;
case 19:
                  {
                                     yylval.item =
                                        ODLConvertDateTime (yytext, yyleng);
                                     return (_date_timeV0);
                                    }
break;
case 20:
{
                                     ODLPrintWarning (MESSAGE1);
                                     yylval.item =
                                        ODLConvertString (yytext, yyleng);
                                     return (_text_string);
                                    }
break;
case 21:
                        {
                                     ODLPrintWarning (MESSAGE2);
                                     yylval.item =
                                        ODLConvertSymbol (yytext, yyleng, 2);
                                     return (_symbol);
                                    }
break;
case 22:
                                {
                                     return (_sequence_opening);
                                    }
break;
case 23:
                                {
                                     return (_sequence_closing);
                                    }
break;
case 24:
                                {
                                     return (_set_opening);
                                    }
break;
case 25:
                                {
                                     return (_set_closing);
                                    }
break;
case 26:
                                {
                                     return (_units_opening);
                                    }
break;
case 27:
                                {
                                     return (_units_closing);
                                    }
break;
case 28:
                                {
                                     return (_list_separator);
                                    }
break;
case 29:
                                {
                                     return (_point_operator);
                                    }
break;
case 30:
                                {
                                     return (_assignment_operator);
                                    }
break;
case 31:
                                {
                                     return (_multiply_operator);
                                    }
break;
case 32:
                                {
                                     return (_divide_operator);
                                    }
break;
case 33:
                               {
                                     return (_exponentiate_operator);
                                    }
break;
case 34:
                               {
                                     return (_range_operator);
                                    }
break;
case 35:
               {
                                     /* This is a comment line */

                                     yyGetComment ();
                                    }
break;
case 36:
                             {
                                     /* This is a comment at the end of a
                                        line of ODL code -- ignore it */
                                    }
break;
case 37:
                       {}
break;
case 38:
                        {}
break;
case 39:
                                  { /* Return other characters verbatim */
                                     return (yytext[0]);
                                    }
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */

/*****************************************************************************

  Routine: yywrap
 
  Description:  Required wrap-up routine for lexical processing.  No
                special wrap-up is required for ODL parsing.
 
  Input:  None.
           
  Output: Return value is set to TRUE to indicate parsing completed OK.
 
*****************************************************************************/

yywrap ()
{
 return(1);
}


/*****************************************************************************

  Routine: yyGetStringToken
 
  Description:  Get a text string token.  The opening delimiter (") was
                recognized by the lexical analyzer before this routine
                was called.  This routine will read in the remainder of
                the string token up to the end delimiter and it will
                reformat the text string as it goes into an ODL string
                value.
 
  Input:  No arguments required.  The text of the string token is gotten
          using the lexical analyzer input function (yyinput).
           
  Output: The text string is placed in the lexical analyzer's token buffer
          (pointed to by yytext) and the token character count (yyleng) is
          set to reflect the string length.
 
  MDD - October 22, 1991
        Modified to use dynamic memory allocation routines rather than
        a static array.

*****************************************************************************/

char *yyGetStringToken ()
{
  char    c;                       /* Current input character               */
  int     newline;                 /* New line flag:                        */
                                   /*   0 - Not at the start of a line      */
                                   /*  +1 - Newline found in input string   */
                                   /*  -1 - Newline placed in output string */

  newline = 1;

  ODLNewString();
  
  while ((c = yyinput()) != '"' && c != '\0')
   {
    switch (c)
     {
      case '\n':

       /* End of current line : get rid of any trailing blanks on the line */
       
       while (ODLPeekString (1) == ' ') ODLBackupString ();

       /* If the last non-blank character on the line is a '-', then this
          is a word hyphen and we can delete it. If it is an '&' then this
          indicates that all characters to the left are to be left intact,
          although we do delete the '&'.  Otherwise we add a blank to
          separate the last word on the current input line from the first
          word on the next input line. If there are two or more newlines
          in a row, then we retain the newlines to separate paragraphs */
   
       if (newline > 0)
       { 
           if (ODLPeekString (1) && 
                  !(ODLPeekString (1) == 'n' && ODLPeekString (2) == '\\'))
           {
              ODLStoreString ('\\');
              ODLStoreString ('n');
           }
           ODLStoreString ('\\');
           ODLStoreString ('n');
       }
       else if (newline < 0)
       {
          newline = 1;
       }
       else
       {
           newline = 1;
           if (ODLPeekString (1) == '-' || ODLPeekString (1) == '&')
           {
              ODLBackupString ();
           }
           else if (ODLPeekString (1) && 
                    !(ODLPeekString (1) == 'n' && ODLPeekString (2) == '\\'))
           {
              ODLStoreString (' ');
           }
       }
       break;

      case ' ':  case '\t':

       /* Ignore a blank or tab at the beginning of an input line; otherwise
          copy a blank character into the output string */
       
       if (newline == 0)
       {
         ODLStoreString (' ');
       }
       break;

      case '\\':
         ODLStoreString (c);
         ODLStoreString (yyinput ());
         if (ODLPeekString (1) == 'n' && ODLPeekString (2) == '\\')
            newline = -1;
         else if (ODLPeekString (1) && 
              !(ODLPeekString (1) == 't' && ODLPeekString (2) == '\\'))
            newline = 0;
         break;

      default:

       /* Copy the input character to the token buffer */
       ODLStoreString (c);
       newline = 0;
        
     } 
   }     

  /* Terminate the token buffer */

  return (ODLStoreString ('\0'));
}


/*****************************************************************************

  Routine: yyGetComment
 
  Description:  Get a comment and either attach it to the ODL tree or
                put it where other routines can get at it and do so.
 
  Input:  No arguments required.  The text and length of the comment
          come from the global variables yyinput and yyleng.
           
  Output: No output parameters.  The comment is copied and saved for
          later processing.
 
*****************************************************************************/


void yyGetComment ()
{
  int   ib;                   /* Index to first character in comment text   */
  int   ie;                   /* Index to last character in comment text    */
  int   il;                   /* Count of characters in comment text        */
  char *string;               /* Pointer to space allocated for comment     */

#include  <ctype.h>


  /* Skip over any whitespace prior to the start of the comment */

  ib = 0;
  ie = yyleng - 1;

  for ( ; ib <= ie && isspace (yytext[ib]) ; ib++);

  /* Skip over the slash and asterisk that introduce the comment */

  ib += 2;

  /* Skip backward over any white space or ending comment delimiter */

  for ( ; ib <= ie && isspace (yytext[ie]) ; ie--);

  if (ie > ib && yytext[ie] == '/')
    {
      if (yytext[ie-1] == '*')
        {
          ie -= 2;
        }
    }

  /* Eliminate any trailing whitespace */

  for ( ; ie >= ib && isspace (yytext[ie]); ie--);

  /* Get the number of characters in the comment string */

  yytext[ie+1] = '\0';
  il = (ie >= ib)? ie-ib+1 : 0;

  if (ODLcurrent_comment == NULL)
    {
      /* There is no comment currently.  Allocate space for a new
         comment and copy the text */

      string = (char *) malloc (il+1);
      if (string != NULL)
        {
          ODLcurrent_comment = strcpy (string, &yytext[ib]);
        }
    }
  else 
    {
      /* There is already some comment there: put in a newline character
         to end the previous comment line and append the current text
         to the comment */

      string = (char *) realloc (ODLcurrent_comment, 
                                 strlen (ODLcurrent_comment)+il+2);
      if (string != NULL)
        {
          strcat (string, "\n");
          ODLcurrent_comment = strcat (string, &yytext[ib]);
        }
    }

return;
}


char *ODLStoreString (c)
char c;
{
   if (ODL_buffer != NULL)
   {
      if (ODL_buf_pos < ODL_buf_size)   
         *(ODL_buffer + ODL_buf_pos++) = c;
      else
      {
         ODL_buf_size = ODL_buf_size + ODL_BUFF_INCREMENT;
         ODL_buffer = (char *) realloc (ODL_buffer, ODL_buf_size); 
         if (ODL_buffer == NULL)
         {
            printf ("Out of memory for string storage.");
            exit (1);
         }
         *(ODL_buffer + ODL_buf_pos++) = c;
      }
   }
   return (ODL_buffer);
}

char *ODLNewString ()
{
   ODLKillString ();
   ODL_buffer = (char *) malloc (ODL_BUFF_INCREMENT);
   if (ODL_buffer == NULL)
   {
      printf ("Out of memory for string storage.");
      exit (1);
   }
   ODL_buf_size = ODL_BUFF_INCREMENT;
}

char ODLBackupString ()
{
   if (ODL_buf_pos > 0) 
   {
       ODL_buf_pos--;
       return (*(ODL_buffer + ODL_buf_pos));
   }
   else
      return (0);
}

char ODLPeekString (pos)
int pos;
{
   if (pos != 0 && ODL_buffer != NULL && pos <= ODL_buf_pos) 
      return (*(ODL_buffer + (ODL_buf_pos - pos)));
   else
      return (0);
}

void ODLKillString ()
{
   if (ODL_buffer != NULL) free (ODL_buffer);
   ODL_buffer = NULL;
   ODL_buf_pos = 0;
   ODL_buf_size = 0;
}      

int ODLStringLength ()
{
   return (ODL_buf_pos - 1);
}
int yyvstop[] = {
0,

39,
0,

37,
39,
0,

38,
0,

38,
39,
0,

10,
39,
0,

39,
0,

22,
39,
0,

23,
39,
0,

31,
39,
0,

39,
0,

28,
39,
0,

39,
0,

32,
39,
0,

12,
39,
-11,
0,

26,
39,
0,

30,
39,
0,

27,
39,
0,

8,
39,
0,

8,
39,
0,

8,
39,
0,

8,
39,
0,

8,
39,
0,

8,
39,
0,

29,
39,
0,

8,
38,
39,
0,

24,
39,
0,

25,
39,
0,

37,
39,
0,

32,
39,
0,

8,
39,
0,

9,
0,

33,
0,

12,
-11,
0,

34,
0,

14,
0,

36,
0,

14,
0,

12,
-11,
0,

20,
0,

8,
0,

8,
0,

8,
0,

8,
0,

8,
0,

35,
36,
0,

8,
0,

15,
0,

11,
0,

14,
0,

16,
0,

17,
0,

8,
0,

8,
0,

8,
0,

8,
0,

8,
0,

21,
0,

8,
0,

35,
0,

1,
8,
0,

14,
0,

13,
0,

17,
0,

8,
0,

8,
0,

8,
0,

14,
0,

15,
0,

16,
0,

17,
0,

17,
0,

8,
0,

8,
0,

8,
0,

4,
8,
0,

8,
0,

17,
0,

17,
0,

8,
0,

8,
0,

6,
8,
0,

18,
0,

19,
0,

17,
0,

8,
0,

8,
0,

8,
0,

8,
0,

18,
0,

8,
0,

8,
0,

8,
0,

8,
0,

18,
0,

18,
0,

19,
0,

8,
0,

8,
0,

2,
8,
0,

8,
0,

18,
0,

18,
0,

19,
0,

19,
0,

8,
0,

8,
0,

3,
8,
0,

18,
0,

5,
8,
0,

8,
0,

7,
8,
0,
0};
# define YYTYPE int
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,4,	1,5,	
33,0,	1,6,	30,55,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,7,	
0,0,	30,55,	0,0,	0,0,	
1,8,	1,9,	1,10,	1,11,	
1,12,	1,13,	11,35,	1,14,	
1,15,	1,16,	15,40,	24,53,	
30,56,	31,57,	56,75,	61,79,	
0,0,	37,43,	65,82,	0,0,	
65,83,	1,17,	1,18,	1,19,	
0,0,	2,30,	1,20,	1,21,	
1,20,	1,20,	1,22,	1,20,	
1,23,	1,20,	1,20,	1,20,	
95,82,	21,50,	0,0,	1,24,	
1,25,	1,20,	22,51,	1,20,	
93,104,	1,20,	1,20,	94,105,	
23,52,	25,54,	2,7,	32,58,	
96,106,	114,123,	128,137,	1,26,	
2,9,	2,10,	2,11,	8,33,	
2,13,	62,80,	2,14,	2,31,	
132,141,	92,81,	39,59,	8,33,	
8,0,	21,50,	8,33,	50,70,	
51,71,	52,72,	22,51,	53,73,	
2,17,	2,18,	2,19,	1,27,	
23,52,	25,54,	71,88,	32,58,	
1,28,	2,32,	1,29,	36,39,	
36,39,	36,39,	36,39,	36,39,	
36,39,	36,39,	36,39,	36,39,	
36,39,	8,34,	39,59,	76,88,	
62,81,	8,33,	54,74,	50,70,	
51,71,	52,72,	8,33,	53,73,	
99,109,	12,36,	2,26,	12,37,	
12,37,	12,37,	12,37,	12,37,	
12,37,	12,37,	12,37,	12,37,	
12,37,	0,0,	0,0,	8,33,	
8,33,	8,33,	8,33,	8,33,	
8,33,	8,33,	8,33,	8,33,	
8,33,	0,0,	54,74,	0,0,	
8,33,	8,33,	8,33,	2,28,	
8,33,	2,29,	8,33,	8,33,	
14,38,	0,0,	14,39,	14,39,	
14,39,	14,39,	14,39,	14,39,	
14,39,	14,39,	14,39,	14,39,	
42,62,	42,62,	42,62,	42,62,	
42,62,	42,62,	42,62,	42,62,	
42,62,	42,62,	0,0,	16,41,	
0,0,	0,0,	0,0,	58,76,	
70,87,	72,89,	74,90,	87,99,	
8,33,	16,42,	16,43,	16,44,	
16,45,	16,45,	16,45,	16,45,	
16,45,	16,45,	16,45,	16,45,	
16,45,	16,45,	16,46,	20,47,	
0,0,	20,48,	20,48,	20,48,	
20,48,	20,48,	20,48,	20,48,	
20,48,	20,48,	20,48,	58,76,	
70,87,	72,89,	74,90,	87,99,	
0,0,	89,102,	20,48,	20,48,	
20,48,	20,48,	20,48,	20,48,	
20,48,	20,48,	20,48,	20,48,	
20,48,	20,48,	20,48,	20,48,	
20,48,	20,48,	20,48,	20,48,	
20,48,	20,48,	20,48,	20,48,	
20,48,	20,48,	20,48,	20,48,	
98,84,	90,103,	98,84,	98,108,	
20,49,	89,102,	20,48,	20,48,	
20,48,	20,48,	20,48,	20,48,	
20,48,	20,48,	20,48,	20,48,	
20,48,	20,48,	20,48,	20,48,	
20,48,	20,48,	20,48,	20,48,	
20,48,	20,48,	20,48,	20,48,	
20,48,	20,48,	20,48,	20,48,	
40,40,	90,103,	103,112,	0,0,	
0,0,	0,0,	0,0,	0,0,	
40,40,	40,0,	0,0,	40,40,	
107,84,	0,0,	107,84,	98,86,	
0,0,	0,0,	0,0,	0,0,	
43,63,	0,0,	43,64,	43,64,	
43,64,	43,64,	43,64,	43,64,	
43,64,	43,64,	43,64,	43,64,	
0,0,	0,0,	103,112,	0,0,	
0,0,	0,0,	40,40,	0,0,	
0,0,	0,0,	40,40,	43,59,	
0,0,	0,0,	0,0,	40,40,	
44,65,	44,65,	44,65,	44,65,	
44,65,	44,65,	44,65,	44,65,	
44,65,	44,65,	0,0,	107,86,	
0,0,	66,84,	0,0,	66,84,	
40,40,	40,40,	40,40,	40,40,	
40,40,	40,40,	40,40,	40,40,	
40,40,	40,40,	0,0,	43,59,	
66,85,	40,40,	40,40,	40,40,	
41,60,	40,40,	41,60,	40,40,	
40,40,	41,61,	41,61,	41,61,	
41,61,	41,61,	41,61,	41,61,	
41,61,	41,61,	41,61,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	41,61,	41,61,	
41,61,	41,61,	41,61,	41,61,	
66,86,	130,120,	0,0,	130,120,	
130,139,	0,0,	0,0,	0,0,	
0,0,	40,40,	46,66,	46,66,	
46,66,	46,66,	46,66,	46,66,	
46,66,	46,66,	46,66,	46,66,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	41,61,	41,61,	
41,61,	41,61,	41,61,	41,61,	
47,47,	47,47,	47,47,	47,47,	
47,47,	47,47,	47,47,	47,47,	
47,47,	47,47,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
130,122,	47,47,	47,47,	47,47,	
47,47,	47,47,	47,47,	47,47,	
47,47,	47,47,	47,47,	47,47,	
47,47,	47,47,	47,47,	47,47,	
47,47,	47,47,	47,47,	47,47,	
47,47,	47,47,	47,47,	47,47,	
47,47,	47,47,	47,47,	138,120,	
0,0,	138,120,	0,0,	47,47,	
0,0,	47,47,	47,47,	47,47,	
47,47,	47,47,	47,47,	47,47,	
47,47,	47,47,	47,47,	47,47,	
47,47,	47,47,	47,47,	47,47,	
47,47,	47,47,	47,47,	47,47,	
47,47,	47,47,	47,47,	47,47,	
47,47,	47,47,	47,47,	49,67,	
49,67,	49,67,	49,67,	49,67,	
49,67,	49,67,	49,67,	49,67,	
49,67,	0,0,	0,0,	0,0,	
0,0,	0,0,	138,122,	0,0,	
49,68,	49,68,	49,68,	49,68,	
49,68,	49,68,	49,68,	49,68,	
49,68,	49,68,	49,68,	49,68,	
49,68,	49,68,	49,68,	49,68,	
49,68,	49,68,	49,68,	49,68,	
49,68,	49,68,	49,68,	49,68,	
49,68,	49,68,	0,0,	0,0,	
0,0,	0,0,	49,69,	0,0,	
49,68,	49,68,	49,68,	49,68,	
49,68,	49,68,	49,68,	49,68,	
49,68,	49,68,	49,68,	49,68,	
49,68,	49,68,	49,68,	49,68,	
49,68,	49,68,	49,68,	49,68,	
49,68,	49,68,	49,68,	49,68,	
49,68,	49,68,	57,57,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	57,57,	57,0,	
59,77,	57,57,	59,77,	0,0,	
0,0,	59,78,	59,78,	59,78,	
59,78,	59,78,	59,78,	59,78,	
59,78,	59,78,	59,78,	0,0,	
0,0,	64,64,	64,64,	64,64,	
64,64,	64,64,	64,64,	64,64,	
64,64,	64,64,	64,64,	0,0,	
57,57,	0,0,	0,0,	0,0,	
57,57,	0,0,	0,0,	0,0,	
0,0,	57,57,	64,59,	77,91,	
77,91,	77,91,	77,91,	77,91,	
77,91,	77,91,	77,91,	77,91,	
77,91,	0,0,	0,0,	113,120,	
0,0,	113,120,	57,57,	57,57,	
57,57,	57,57,	57,57,	57,57,	
57,57,	57,57,	57,57,	57,57,	
0,0,	0,0,	113,121,	57,57,	
57,57,	57,57,	64,59,	57,57,	
0,0,	57,57,	57,57,	60,61,	
60,61,	60,61,	60,61,	60,61,	
60,61,	60,61,	60,61,	60,61,	
60,61,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
60,61,	60,61,	60,61,	60,61,	
60,61,	60,61,	113,122,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	57,57,	
0,0,	0,0,	0,0,	0,0,	
0,0,	67,67,	67,67,	67,67,	
67,67,	67,67,	67,67,	67,67,	
67,67,	67,67,	67,67,	0,0,	
60,61,	60,61,	60,61,	60,61,	
60,61,	60,61,	67,69,	67,69,	
67,69,	67,69,	67,69,	67,69,	
67,69,	67,69,	67,69,	67,69,	
67,69,	67,69,	67,69,	67,69,	
67,69,	67,69,	67,69,	67,69,	
67,69,	67,69,	67,69,	67,69,	
67,69,	67,69,	67,69,	67,69,	
0,0,	0,0,	0,0,	0,0,	
67,69,	0,0,	67,69,	67,69,	
67,69,	67,69,	67,69,	67,69,	
67,69,	67,69,	67,69,	67,69,	
67,69,	67,69,	67,69,	67,69,	
67,69,	67,69,	67,69,	67,69,	
67,69,	67,69,	67,69,	67,69,	
67,69,	67,69,	67,69,	67,69,	
68,68,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	68,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	68,68,	
68,68,	68,68,	68,68,	69,69,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	69,69,	69,69,	
69,69,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	0,0,	0,0,	
0,0,	0,0,	69,69,	0,0,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	75,75,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	75,75,	75,0,	
0,0,	75,75,	78,78,	78,78,	
78,78,	78,78,	78,78,	78,78,	
78,78,	78,78,	78,78,	78,78,	
80,92,	80,92,	80,92,	80,92,	
80,92,	80,92,	80,92,	80,92,	
80,92,	80,92,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
75,75,	0,0,	0,0,	0,0,	
75,75,	0,0,	0,0,	0,0,	
0,0,	75,75,	81,93,	81,93,	
81,93,	81,93,	81,93,	81,93,	
81,93,	81,93,	81,93,	81,93,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	75,75,	75,75,	
75,75,	75,75,	75,75,	75,75,	
75,75,	75,75,	75,75,	75,75,	
0,0,	0,0,	0,0,	75,75,	
75,75,	75,75,	0,0,	75,75,	
0,0,	75,75,	75,75,	82,94,	
82,94,	82,94,	82,94,	82,94,	
82,94,	82,94,	82,94,	82,94,	
82,94,	83,95,	83,95,	83,95,	
83,95,	83,95,	83,95,	83,95,	
83,95,	83,95,	83,95,	84,96,	
84,96,	84,96,	84,96,	84,96,	
84,96,	84,96,	84,96,	84,96,	
84,96,	0,0,	85,97,	75,75,	
85,98,	85,98,	85,98,	85,98,	
85,98,	85,98,	85,98,	85,98,	
85,98,	85,98,	88,67,	88,67,	
88,67,	88,67,	88,67,	88,67,	
88,67,	88,67,	88,67,	88,67,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	88,68,	
88,68,	88,68,	88,68,	88,68,	
88,68,	88,100,	88,68,	88,68,	
88,68,	88,68,	88,68,	88,68,	
88,68,	88,101,	88,68,	88,68,	
88,68,	88,68,	88,68,	88,68,	
88,68,	88,68,	88,68,	88,68,	
88,68,	0,0,	0,0,	0,0,	
0,0,	88,69,	0,0,	88,68,	
88,68,	88,68,	88,68,	88,68,	
88,68,	88,100,	88,68,	88,68,	
88,68,	88,68,	88,68,	88,68,	
88,68,	88,101,	88,68,	88,68,	
88,68,	88,68,	88,68,	88,68,	
88,68,	88,68,	88,68,	88,68,	
88,68,	97,107,	97,107,	97,107,	
97,107,	97,107,	97,107,	97,107,	
97,107,	97,107,	97,107,	100,68,	
100,68,	100,68,	100,68,	100,68,	
100,68,	100,68,	100,68,	100,68,	
100,68,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
100,68,	100,68,	100,68,	100,68,	
100,68,	100,68,	100,68,	100,68,	
100,68,	100,68,	100,68,	100,68,	
100,68,	100,68,	100,68,	100,68,	
100,68,	100,110,	100,68,	100,68,	
100,68,	100,68,	100,68,	100,68,	
100,68,	100,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
100,68,	100,68,	100,68,	100,68,	
100,68,	100,68,	100,68,	100,68,	
100,68,	100,68,	100,68,	100,68,	
100,68,	100,68,	100,68,	100,68,	
100,68,	100,110,	100,68,	100,68,	
100,68,	100,68,	100,68,	100,68,	
100,68,	100,68,	101,68,	101,68,	
101,68,	101,68,	101,68,	101,68,	
101,68,	101,68,	101,68,	101,68,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	101,68,	
101,111,	101,68,	101,68,	101,68,	
101,68,	101,68,	101,68,	101,68,	
101,68,	101,68,	101,68,	101,68,	
101,68,	101,68,	101,68,	101,68,	
101,68,	101,68,	101,68,	101,68,	
101,68,	101,68,	101,68,	101,68,	
101,68,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	101,68,	
101,111,	101,68,	101,68,	101,68,	
101,68,	101,68,	101,68,	101,68,	
101,68,	101,68,	101,68,	101,68,	
101,68,	101,68,	101,68,	101,68,	
101,68,	101,68,	101,68,	101,68,	
101,68,	101,68,	101,68,	101,68,	
101,68,	104,113,	104,113,	104,113,	
104,113,	104,113,	104,113,	104,113,	
104,113,	104,113,	104,113,	105,114,	
105,114,	105,114,	105,114,	105,114,	
105,114,	105,114,	105,114,	105,114,	
105,114,	106,115,	106,115,	106,115,	
106,115,	106,115,	106,115,	106,115,	
106,115,	106,115,	106,115,	108,84,	
0,0,	108,84,	0,0,	0,0,	
108,108,	108,108,	108,108,	108,108,	
108,108,	108,108,	108,108,	108,108,	
108,108,	108,108,	120,128,	120,128,	
120,128,	120,128,	120,128,	120,128,	
120,128,	120,128,	120,128,	120,128,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	109,67,	109,67,	
109,67,	109,67,	109,67,	109,67,	
109,67,	109,67,	109,67,	109,67,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	108,86,	109,68,	
109,68,	109,68,	109,68,	109,68,	
109,68,	109,116,	109,68,	109,68,	
109,68,	109,68,	109,68,	109,68,	
109,68,	109,117,	109,68,	109,68,	
109,68,	109,68,	109,68,	109,68,	
109,68,	109,68,	109,68,	109,68,	
109,68,	0,0,	0,0,	0,0,	
0,0,	109,69,	0,0,	109,68,	
109,68,	109,68,	109,68,	109,68,	
109,68,	109,116,	109,68,	109,68,	
109,68,	109,68,	109,68,	109,68,	
109,68,	109,117,	109,68,	109,68,	
109,68,	109,68,	109,68,	109,68,	
109,68,	109,68,	109,68,	109,68,	
109,68,	110,68,	110,68,	110,68,	
110,68,	110,68,	110,68,	110,68,	
110,68,	110,68,	110,68,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	110,68,	110,68,	
110,68,	110,68,	110,68,	110,68,	
110,68,	110,68,	110,68,	110,68,	
110,68,	110,68,	110,68,	110,68,	
110,118,	110,68,	110,68,	110,68,	
110,68,	110,68,	110,68,	110,68,	
110,68,	110,68,	110,68,	110,68,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	110,68,	110,68,	
110,68,	110,68,	110,68,	110,68,	
110,68,	110,68,	110,68,	110,68,	
110,68,	110,68,	110,68,	110,68,	
110,118,	110,68,	110,68,	110,68,	
110,68,	110,68,	110,68,	110,68,	
110,68,	110,68,	110,68,	110,68,	
111,68,	111,68,	111,68,	111,68,	
111,68,	111,68,	111,68,	111,68,	
111,68,	111,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	111,68,	111,68,	111,68,	
111,68,	111,68,	111,68,	111,68,	
111,68,	111,68,	111,119,	111,68,	
111,68,	111,68,	111,68,	111,68,	
111,68,	111,68,	111,68,	111,68,	
111,68,	111,68,	111,68,	111,68,	
111,68,	111,68,	111,68,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	111,68,	111,68,	111,68,	
111,68,	111,68,	111,68,	111,68,	
111,68,	111,68,	111,119,	111,68,	
111,68,	111,68,	111,68,	111,68,	
111,68,	111,68,	111,68,	111,68,	
111,68,	111,68,	111,68,	111,68,	
111,68,	111,68,	111,68,	116,68,	
116,68,	116,68,	116,68,	116,68,	
116,68,	116,68,	116,68,	116,68,	
116,68,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
116,68,	116,68,	116,68,	116,68,	
116,68,	116,68,	116,68,	116,68,	
116,68,	116,68,	116,68,	116,68,	
116,68,	116,68,	116,68,	116,68,	
116,68,	116,124,	116,68,	116,68,	
116,68,	116,68,	116,68,	116,68,	
116,68,	116,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
116,68,	116,68,	116,68,	116,68,	
116,68,	116,68,	116,68,	116,68,	
116,68,	116,68,	116,68,	116,68,	
116,68,	116,68,	116,68,	116,68,	
116,68,	116,124,	116,68,	116,68,	
116,68,	116,68,	116,68,	116,68,	
116,68,	116,68,	117,68,	117,68,	
117,68,	117,68,	117,68,	117,68,	
117,68,	117,68,	117,68,	117,68,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	117,68,	
117,125,	117,68,	117,68,	117,68,	
117,68,	117,68,	117,68,	117,68,	
117,68,	117,68,	117,68,	117,68,	
117,68,	117,68,	117,68,	117,68,	
117,68,	117,68,	117,68,	117,68,	
117,68,	117,68,	117,68,	117,68,	
117,68,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	117,68,	
117,125,	117,68,	117,68,	117,68,	
117,68,	117,68,	117,68,	117,68,	
117,68,	117,68,	117,68,	117,68,	
117,68,	117,68,	117,68,	117,68,	
117,68,	117,68,	117,68,	117,68,	
117,68,	117,68,	117,68,	117,68,	
117,68,	118,68,	118,68,	118,68,	
118,68,	118,68,	118,68,	118,68,	
118,68,	118,68,	118,68,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	118,68,	118,68,	
118,68,	118,68,	118,68,	118,68,	
118,68,	118,68,	118,68,	118,68,	
118,68,	118,68,	118,68,	118,68,	
118,68,	118,68,	118,68,	118,68,	
118,68,	118,68,	118,126,	118,68,	
118,68,	118,68,	118,68,	118,68,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	118,68,	118,68,	
118,68,	118,68,	118,68,	118,68,	
118,68,	118,68,	118,68,	118,68,	
118,68,	118,68,	118,68,	118,68,	
118,68,	118,68,	118,68,	118,68,	
118,68,	118,68,	118,126,	118,68,	
118,68,	118,68,	118,68,	118,68,	
119,68,	119,68,	119,68,	119,68,	
119,68,	119,68,	119,68,	119,68,	
119,68,	119,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	119,68,	119,68,	119,68,	
119,68,	119,127,	119,68,	119,68,	
119,68,	119,68,	119,68,	119,68,	
119,68,	119,68,	119,68,	119,68,	
119,68,	119,68,	119,68,	119,68,	
119,68,	119,68,	119,68,	119,68,	
119,68,	119,68,	119,68,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	119,68,	119,68,	119,68,	
119,68,	119,127,	119,68,	119,68,	
119,68,	119,68,	119,68,	119,68,	
119,68,	119,68,	119,68,	119,68,	
119,68,	119,68,	119,68,	119,68,	
119,68,	119,68,	119,68,	119,68,	
119,68,	119,68,	119,68,	121,129,	
0,0,	121,130,	121,130,	121,130,	
121,130,	121,130,	121,130,	121,130,	
121,130,	121,130,	121,130,	123,131,	
0,0,	123,132,	123,132,	123,132,	
123,132,	123,132,	123,132,	123,132,	
123,132,	123,132,	123,132,	124,68,	
124,68,	124,68,	124,68,	124,68,	
124,68,	124,68,	124,68,	124,68,	
124,68,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
124,68,	124,68,	124,68,	124,68,	
124,68,	124,68,	124,68,	124,68,	
124,68,	124,68,	124,68,	124,68,	
124,68,	124,68,	124,133,	124,68,	
124,68,	124,68,	124,68,	124,68,	
124,68,	124,68,	124,68,	124,68,	
124,68,	124,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
124,68,	124,68,	124,68,	124,68,	
124,68,	124,68,	124,68,	124,68,	
124,68,	124,68,	124,68,	124,68,	
124,68,	124,68,	124,133,	124,68,	
124,68,	124,68,	124,68,	124,68,	
124,68,	124,68,	124,68,	124,68,	
124,68,	124,68,	125,68,	125,68,	
125,68,	125,68,	125,68,	125,68,	
125,68,	125,68,	125,68,	125,68,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	125,68,	
125,68,	125,68,	125,68,	125,68,	
125,68,	125,68,	125,68,	125,68,	
125,134,	125,68,	125,68,	125,68,	
125,68,	125,68,	125,68,	125,68,	
125,68,	125,68,	125,68,	125,68,	
125,68,	125,68,	125,68,	125,68,	
125,68,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	125,68,	
125,68,	125,68,	125,68,	125,68,	
125,68,	125,68,	125,68,	125,68,	
125,134,	125,68,	125,68,	125,68,	
125,68,	125,68,	125,68,	125,68,	
125,68,	125,68,	125,68,	125,68,	
125,68,	125,68,	125,68,	125,68,	
125,68,	126,68,	126,68,	126,68,	
126,68,	126,68,	126,68,	126,68,	
126,68,	126,68,	126,68,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	126,68,	126,68,	
126,68,	126,68,	126,68,	126,68,	
126,68,	126,68,	126,68,	126,68,	
126,68,	126,68,	126,68,	126,68,	
126,68,	126,135,	126,68,	126,68,	
126,68,	126,68,	126,68,	126,68,	
126,68,	126,68,	126,68,	126,68,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	126,68,	126,68,	
126,68,	126,68,	126,68,	126,68,	
126,68,	126,68,	126,68,	126,68,	
126,68,	126,68,	126,68,	126,68,	
126,68,	126,135,	126,68,	126,68,	
126,68,	126,68,	126,68,	126,68,	
126,68,	126,68,	126,68,	126,68,	
127,68,	127,68,	127,68,	127,68,	
127,68,	127,68,	127,68,	127,68,	
127,68,	127,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	127,68,	127,68,	127,136,	
127,68,	127,68,	127,68,	127,68,	
127,68,	127,68,	127,68,	127,68,	
127,68,	127,68,	127,68,	127,68,	
127,68,	127,68,	127,68,	127,68,	
127,68,	127,68,	127,68,	127,68,	
127,68,	127,68,	127,68,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	127,68,	127,68,	127,136,	
127,68,	127,68,	127,68,	127,68,	
127,68,	127,68,	127,68,	127,68,	
127,68,	127,68,	127,68,	127,68,	
127,68,	127,68,	127,68,	127,68,	
127,68,	127,68,	127,68,	127,68,	
127,68,	127,68,	127,68,	129,138,	
129,138,	129,138,	129,138,	129,138,	
129,138,	129,138,	129,138,	129,138,	
129,138,	131,140,	131,140,	131,140,	
131,140,	131,140,	131,140,	131,140,	
131,140,	131,140,	131,140,	133,68,	
133,68,	133,68,	133,68,	133,68,	
133,68,	133,68,	133,68,	133,68,	
133,68,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
133,68,	133,68,	133,68,	133,68,	
133,68,	133,68,	133,68,	133,68,	
133,68,	133,68,	133,68,	133,68,	
133,68,	133,68,	133,68,	133,68,	
133,68,	133,68,	133,68,	133,68,	
133,142,	133,68,	133,68,	133,68,	
133,68,	133,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
133,68,	133,68,	133,68,	133,68,	
133,68,	133,68,	133,68,	133,68,	
133,68,	133,68,	133,68,	133,68,	
133,68,	133,68,	133,68,	133,68,	
133,68,	133,68,	133,68,	133,68,	
133,142,	133,68,	133,68,	133,68,	
133,68,	133,68,	134,68,	134,68,	
134,68,	134,68,	134,68,	134,68,	
134,68,	134,68,	134,68,	134,68,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	134,68,	
134,68,	134,68,	134,68,	134,143,	
134,68,	134,68,	134,68,	134,68,	
134,68,	134,68,	134,68,	134,68,	
134,68,	134,68,	134,68,	134,68,	
134,68,	134,68,	134,68,	134,68,	
134,68,	134,68,	134,68,	134,68,	
134,68,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	134,68,	
134,68,	134,68,	134,68,	134,143,	
134,68,	134,68,	134,68,	134,68,	
134,68,	134,68,	134,68,	134,68,	
134,68,	134,68,	134,68,	134,68,	
134,68,	134,68,	134,68,	134,68,	
134,68,	134,68,	134,68,	134,68,	
134,68,	135,68,	135,68,	135,68,	
135,68,	135,68,	135,68,	135,68,	
135,68,	135,68,	135,68,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	135,68,	135,68,	
135,68,	135,68,	135,68,	135,68,	
135,68,	135,68,	135,68,	135,68,	
135,68,	135,68,	135,68,	135,68,	
135,68,	135,68,	135,68,	135,68,	
135,68,	135,68,	135,68,	135,68,	
135,68,	135,68,	135,68,	135,68,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	135,68,	135,68,	
135,68,	135,68,	135,68,	135,68,	
135,68,	135,68,	135,68,	135,68,	
135,68,	135,68,	135,68,	135,68,	
135,68,	135,68,	135,68,	135,68,	
135,68,	135,68,	135,68,	135,68,	
135,68,	135,68,	135,68,	135,68,	
136,68,	136,68,	136,68,	136,68,	
136,68,	136,68,	136,68,	136,68,	
136,68,	136,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	136,68,	136,68,	136,68,	
136,68,	136,68,	136,68,	136,68,	
136,68,	136,68,	136,68,	136,68,	
136,68,	136,68,	136,68,	136,68,	
136,68,	136,68,	136,68,	136,68,	
136,144,	136,68,	136,68,	136,68,	
136,68,	136,68,	136,68,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	136,68,	136,68,	136,68,	
136,68,	136,68,	136,68,	136,68,	
136,68,	136,68,	136,68,	136,68,	
136,68,	136,68,	136,68,	136,68,	
136,68,	136,68,	136,68,	136,68,	
136,144,	136,68,	136,68,	136,68,	
136,68,	136,68,	136,68,	137,145,	
137,145,	137,145,	137,145,	137,145,	
137,145,	137,145,	137,145,	137,145,	
137,145,	139,120,	0,0,	139,120,	
0,0,	0,0,	139,139,	139,139,	
139,139,	139,139,	139,139,	139,139,	
139,139,	139,139,	139,139,	139,139,	
141,141,	141,141,	141,141,	141,141,	
141,141,	141,141,	141,141,	141,141,	
141,141,	141,141,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
142,68,	142,68,	142,68,	142,68,	
142,68,	142,68,	142,68,	142,68,	
142,68,	142,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
139,122,	142,68,	142,68,	142,68,	
142,68,	142,68,	142,68,	142,68,	
142,68,	142,68,	142,68,	142,68,	
142,68,	142,68,	142,68,	142,68,	
142,146,	142,68,	142,68,	142,68,	
142,68,	142,68,	142,68,	142,68,	
142,68,	142,68,	142,68,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	142,68,	142,68,	142,68,	
142,68,	142,68,	142,68,	142,68,	
142,68,	142,68,	142,68,	142,68,	
142,68,	142,68,	142,68,	142,68,	
142,146,	142,68,	142,68,	142,68,	
142,68,	142,68,	142,68,	142,68,	
142,68,	142,68,	142,68,	143,68,	
143,68,	143,68,	143,68,	143,68,	
143,68,	143,68,	143,68,	143,68,	
143,68,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
143,68,	143,68,	143,147,	143,68,	
143,68,	143,68,	143,68,	143,68,	
143,68,	143,68,	143,68,	143,68,	
143,68,	143,68,	143,68,	143,68,	
143,68,	143,68,	143,68,	143,68,	
143,68,	143,68,	143,68,	143,68,	
143,68,	143,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
143,68,	143,68,	143,147,	143,68,	
143,68,	143,68,	143,68,	143,68,	
143,68,	143,68,	143,68,	143,68,	
143,68,	143,68,	143,68,	143,68,	
143,68,	143,68,	143,68,	143,68,	
143,68,	143,68,	143,68,	143,68,	
143,68,	143,68,	144,68,	144,68,	
144,68,	144,68,	144,68,	144,68,	
144,68,	144,68,	144,68,	144,68,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	144,68,	
144,68,	144,68,	144,68,	144,68,	
144,68,	144,68,	144,68,	144,68,	
144,68,	144,68,	144,68,	144,68,	
144,68,	144,68,	144,68,	144,68,	
144,68,	144,68,	144,68,	144,68,	
144,68,	144,68,	144,68,	144,68,	
144,68,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	144,68,	
144,68,	144,68,	144,68,	144,68,	
144,68,	144,68,	144,68,	144,68,	
144,68,	144,68,	144,68,	144,68,	
144,68,	144,68,	144,68,	144,68,	
144,68,	144,68,	144,68,	144,68,	
144,68,	144,68,	144,68,	144,68,	
144,68,	146,68,	146,68,	146,68,	
146,68,	146,68,	146,68,	146,68,	
146,68,	146,68,	146,68,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	146,68,	146,68,	
146,68,	146,68,	146,68,	146,68,	
146,68,	146,68,	146,68,	146,68,	
146,68,	146,68,	146,68,	146,68,	
146,68,	146,68,	146,68,	146,68,	
146,68,	146,68,	146,68,	146,68,	
146,68,	146,68,	146,68,	146,68,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	146,68,	146,68,	
146,68,	146,68,	146,68,	146,68,	
146,68,	146,68,	146,68,	146,68,	
146,68,	146,68,	146,68,	146,68,	
146,68,	146,68,	146,68,	146,68,	
146,68,	146,68,	146,68,	146,68,	
146,68,	146,68,	146,68,	146,68,	
147,68,	147,68,	147,68,	147,68,	
147,68,	147,68,	147,68,	147,68,	
147,68,	147,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	147,68,	147,68,	147,68,	
147,68,	147,68,	147,68,	147,68,	
147,68,	147,68,	147,68,	147,68,	
147,68,	147,68,	147,68,	147,68,	
147,68,	147,68,	147,68,	147,68,	
147,148,	147,68,	147,68,	147,68,	
147,68,	147,68,	147,68,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	147,68,	147,68,	147,68,	
147,68,	147,68,	147,68,	147,68,	
147,68,	147,68,	147,68,	147,68,	
147,68,	147,68,	147,68,	147,68,	
147,68,	147,68,	147,68,	147,68,	
147,148,	147,68,	147,68,	147,68,	
147,68,	147,68,	147,68,	148,68,	
148,68,	148,68,	148,68,	148,68,	
148,68,	148,68,	148,68,	148,68,	
148,68,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
148,68,	148,68,	148,68,	148,68,	
148,68,	148,68,	148,68,	148,68,	
148,68,	148,68,	148,68,	148,68,	
148,68,	148,68,	148,68,	148,68,	
148,68,	148,68,	148,68,	148,68,	
148,68,	148,68,	148,68,	148,68,	
148,68,	148,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
148,68,	148,68,	148,68,	148,68,	
148,68,	148,68,	148,68,	148,68,	
148,68,	148,68,	148,68,	148,68,	
148,68,	148,68,	148,68,	148,68,	
148,68,	148,68,	148,68,	148,68,	
148,68,	148,68,	148,68,	148,68,	
148,68,	148,68,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-56,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+0,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+6,
yycrank+0,	0,		yyvstop+8,
yycrank+0,	0,		yyvstop+11,
yycrank+-98,	0,		yyvstop+14,
yycrank+0,	0,		yyvstop+16,
yycrank+0,	0,		yyvstop+19,
yycrank+4,	0,		yyvstop+22,
yycrank+103,	0,		yyvstop+25,
yycrank+0,	0,		yyvstop+27,
yycrank+138,	0,		yyvstop+30,
yycrank+8,	0,		yyvstop+32,
yycrank+172,	0,		yyvstop+35,
yycrank+0,	0,		yyvstop+39,
yycrank+0,	0,		yyvstop+42,
yycrank+0,	0,		yyvstop+45,
yycrank+185,	0,		yyvstop+48,
yycrank+8,	yysvec+20,	yyvstop+51,
yycrank+4,	yysvec+20,	yyvstop+54,
yycrank+6,	yysvec+20,	yyvstop+57,
yycrank+4,	yysvec+20,	yyvstop+60,
yycrank+23,	yysvec+20,	yyvstop+63,
yycrank+0,	0,		yyvstop+66,
yycrank+0,	yysvec+20,	yyvstop+69,
yycrank+0,	0,		yyvstop+73,
yycrank+0,	0,		yyvstop+76,
yycrank+5,	0,		yyvstop+79,
yycrank+11,	0,		yyvstop+82,
yycrank+13,	yysvec+20,	yyvstop+85,
yycrank+-2,	yysvec+8,	0,	
yycrank+0,	0,		yyvstop+88,
yycrank+0,	0,		yyvstop+90,
yycrank+79,	0,		0,	
yycrank+11,	yysvec+12,	yyvstop+92,
yycrank+0,	0,		yyvstop+95,
yycrank+37,	yysvec+36,	yyvstop+97,
yycrank+-307,	0,		yyvstop+99,
yycrank+345,	0,		0,	
yycrank+148,	0,		0,	
yycrank+282,	0,		yyvstop+101,
yycrank+308,	0,		0,	
yycrank+0,	yysvec+16,	yyvstop+103,
yycrank+378,	0,		0,	
yycrank+400,	0,		yyvstop+106,
yycrank+0,	yysvec+20,	yyvstop+108,
yycrank+475,	yysvec+20,	0,	
yycrank+40,	yysvec+20,	yyvstop+110,
yycrank+44,	yysvec+20,	yyvstop+112,
yycrank+34,	yysvec+20,	yyvstop+114,
yycrank+50,	0,		0,	
yycrank+68,	yysvec+20,	yyvstop+116,
yycrank+0,	yysvec+30,	0,	
yycrank+12,	0,		0,	
yycrank+-597,	0,		yyvstop+118,
yycrank+143,	yysvec+20,	yyvstop+121,
yycrank+565,	0,		0,	
yycrank+635,	0,		0,	
yycrank+20,	yysvec+60,	0,	
yycrank+56,	yysvec+42,	yyvstop+123,
yycrank+0,	0,		yyvstop+125,
yycrank+577,	0,		yyvstop+127,
yycrank+13,	yysvec+44,	yyvstop+129,
yycrank+326,	yysvec+46,	yyvstop+131,
yycrank+673,	yysvec+20,	yyvstop+133,
yycrank+748,	yysvec+20,	yyvstop+135,
yycrank+823,	yysvec+20,	0,	
yycrank+139,	yysvec+20,	yyvstop+137,
yycrank+27,	yysvec+20,	yyvstop+139,
yycrank+128,	yysvec+20,	yyvstop+141,
yycrank+0,	0,		yyvstop+143,
yycrank+145,	yysvec+20,	yyvstop+145,
yycrank+-945,	0,		yyvstop+147,
yycrank+44,	yysvec+20,	yyvstop+149,
yycrank+599,	0,		0,	
yycrank+910,	0,		yyvstop+152,
yycrank+0,	0,		yyvstop+154,
yycrank+920,	0,		0,	
yycrank+946,	0,		0,	
yycrank+983,	0,		0,	
yycrank+993,	0,		0,	
yycrank+1003,	0,		0,	
yycrank+1016,	0,		0,	
yycrank+0,	0,		yyvstop+156,
yycrank+137,	yysvec+20,	yyvstop+158,
yycrank+1026,	yysvec+20,	0,	
yycrank+169,	yysvec+20,	yyvstop+160,
yycrank+210,	yysvec+20,	yyvstop+162,
yycrank+0,	yysvec+77,	yyvstop+164,
yycrank+21,	yysvec+80,	yyvstop+166,
yycrank+26,	yysvec+81,	0,	
yycrank+29,	yysvec+82,	0,	
yycrank+31,	yysvec+83,	yyvstop+168,
yycrank+34,	yysvec+84,	yyvstop+170,
yycrank+1101,	0,		0,	
yycrank+233,	yysvec+85,	yyvstop+172,
yycrank+53,	yysvec+20,	yyvstop+174,
yycrank+1111,	yysvec+20,	yyvstop+176,
yycrank+1186,	yysvec+20,	yyvstop+178,
yycrank+0,	yysvec+20,	yyvstop+180,
yycrank+226,	yysvec+20,	yyvstop+183,
yycrank+1261,	0,		0,	
yycrank+1271,	0,		0,	
yycrank+1281,	0,		0,	
yycrank+277,	yysvec+97,	yyvstop+185,
yycrank+1296,	0,		yyvstop+187,
yycrank+1322,	yysvec+20,	0,	
yycrank+1397,	yysvec+20,	yyvstop+189,
yycrank+1472,	yysvec+20,	yyvstop+191,
yycrank+0,	yysvec+20,	yyvstop+193,
yycrank+616,	yysvec+104,	yyvstop+196,
yycrank+35,	yysvec+105,	yyvstop+198,
yycrank+0,	yysvec+106,	yyvstop+200,
yycrank+1547,	yysvec+20,	yyvstop+202,
yycrank+1622,	yysvec+20,	yyvstop+204,
yycrank+1697,	yysvec+20,	yyvstop+206,
yycrank+1772,	yysvec+20,	yyvstop+208,
yycrank+1306,	0,		0,	
yycrank+1849,	0,		0,	
yycrank+0,	0,		yyvstop+210,
yycrank+1861,	0,		0,	
yycrank+1871,	yysvec+20,	yyvstop+212,
yycrank+1946,	yysvec+20,	yyvstop+214,
yycrank+2021,	yysvec+20,	yyvstop+216,
yycrank+2096,	yysvec+20,	yyvstop+218,
yycrank+36,	yysvec+120,	yyvstop+220,
yycrank+2171,	0,		0,	
yycrank+374,	yysvec+121,	yyvstop+222,
yycrank+2181,	0,		0,	
yycrank+58,	yysvec+123,	yyvstop+224,
yycrank+2191,	yysvec+20,	yyvstop+226,
yycrank+2266,	yysvec+20,	yyvstop+228,
yycrank+2341,	yysvec+20,	yyvstop+230,
yycrank+2416,	yysvec+20,	yyvstop+233,
yycrank+2491,	0,		0,	
yycrank+448,	yysvec+129,	yyvstop+235,
yycrank+2506,	0,		yyvstop+237,
yycrank+0,	yysvec+131,	yyvstop+239,
yycrank+2516,	0,		yyvstop+241,
yycrank+2532,	yysvec+20,	yyvstop+243,
yycrank+2607,	yysvec+20,	yyvstop+245,
yycrank+2682,	yysvec+20,	yyvstop+247,
yycrank+0,	yysvec+137,	yyvstop+250,
yycrank+2757,	yysvec+20,	yyvstop+252,
yycrank+2832,	yysvec+20,	yyvstop+255,
yycrank+2907,	yysvec+20,	yyvstop+257,
0,	0,	0};
struct yywork *yytop = yycrank+3029;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,014 ,014 ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,01  ,01  ,01  ,01  ,01  ,047 ,
01  ,01  ,01  ,'+' ,01  ,'+' ,01  ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,014 ,01  ,01  ,01  ,01  ,
01  ,'A' ,'B' ,'C' ,'D' ,'E' ,'F' ,'G' ,
'H' ,'I' ,'J' ,'H' ,'H' ,'H' ,'N' ,'O' ,
'P' ,'H' ,'R' ,'H' ,'T' ,'U' ,'H' ,'H' ,
'H' ,'H' ,'H' ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'B' ,'C' ,'D' ,'E' ,'F' ,'G' ,
'H' ,'I' ,'J' ,'H' ,'H' ,'H' ,'N' ,'O' ,
'P' ,'H' ,'R' ,'H' ,'T' ,'U' ,'v' ,'H' ,
'H' ,'H' ,'H' ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,1,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
#ifndef lint
static	char ncform_sccsid[] = "@(#)ncform 1.6 88/02/08 SMI"; /* from S5R2 1.2 */
#endif

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
yylook(){
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
yyback(p, m)
	int *p;
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	}
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}
