%a 2600
%o 3050
%{

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


%}

DIGIT       [0-9]
EDIGIT      [0-9A-Fa-f]
LETTER      [A-Za-z]
SIGN        [+-]
WHITESPACE  [ \t]
FORMATTER   [\r\n\f\v;]

WORD        {LETTER}({LETTER}|{DIGIT})*
ALPHANUMER  ({LETTER}|{DIGIT}|"_")*

U_INTEGER   {DIGIT}+
X_INTEGER   {EDIGIT}+
U_REAL      ({U_INTEGER}"."{U_INTEGER})|({U_INTEGER}".")|("."{U_INTEGER})

DATE        {U_INTEGER}"-"{U_INTEGER}("-"{U_INTEGER})?
DATEV0      {U_INTEGER}"/"{U_INTEGER}("/"{U_INTEGER})?
TIME        {U_INTEGER}":"{U_INTEGER}(":"({U_INTEGER}|{U_REAL}))?
ZONE        ("Z")|({SIGN}{U_INTEGER})|({SIGN}{U_INTEGER}":"{U_INTEGER})

BEGIN       [Bb][Ee][Gg][Ii][Nn]
OBJECT      [Oo][Bb][Jj][Ee][Cc][Tt]
GROUP       [Gg][Rr][Oo][Uu][Pp]
END         [Ee][Nn][Dd]


%%


^{END}                              {
                                     return (_END);
                                    }

{END}"_"{GROUP}                     {
                                     return (_END_GROUP);
                                    }

{END}"_"{OBJECT}                    {
                                     return (_END_OBJECT);
                                    }

{GROUP}                             {
                                     return (_GROUP);
                                    }

{BEGIN}"_"{GROUP}                   {
                                     ODLPrintWarning(MESSAGE3);
                                     return (_GROUP);
                                    }

{OBJECT}                            {
                                     return (_OBJECT);
                                    }

{BEGIN}"_"{OBJECT}                  {
                                     ODLPrintWarning(MESSAGE4);
                                     return (_OBJECT);
                                    }

{WORD}("_"{WORD})*("_"{U_INTEGER})? {
                                     yylval.item =
                                        ODLConvertSymbol (yytext, yyleng, 1);
                                     return (_name);
                                    }

'[^'\n]*'                           {
                                     yylval.item =
                                       ODLConvertSymbol(&yytext[1],yyleng-2,2);
                                     return (_symbol);
                                    }

\"                                  {
                                     temp_buf = yyGetStringToken ();
                                     yylval.item = 
                                        ODLConvertString (temp_buf, 
                                              ODLStringLength ());
                                     ODLKillString ();
                                     return (_text_string);
                                    }

({SIGN})?{U_INTEGER}/".."           {
                                       /* This is the low part of an ODL
                                          Version 1 range value */
                                     yylval.item =
                                        ODLConvertInteger (yytext, yyleng);
                                     return (_integer);
                                    }

({SIGN})?{U_INTEGER}                {
                                     yylval.item =
                                        ODLConvertInteger (yytext, yyleng);
                                     return (_integer);
                                    }

{U_INTEGER}"#"({SIGN})?{X_INTEGER}"#" {
                                     yylval.item =
                                        ODLConvertInteger (yytext, yyleng);
                                     return (_integer);
                                    }

({SIGN})?({U_REAL})(([Ee]{U_INTEGER})|([Ee]{SIGN}{U_INTEGER}))?  {
                                     yylval.item = 
                                        ODLConvertReal (yytext, yyleng);
                                     return (_real);
                                    }

{DATE}                              {
                                     yylval.item =
                                        ODLConvertDate (yytext, yyleng);
                                     return (_date);
                                    }

{DATEV0}                            {
                                     yylval.item =
                                        ODLConvertDate (yytext, yyleng);
                                     return (_date);
                                    }

{TIME}({ZONE})?                     {
                                     yylval.item =
                                        ODLConvertTime (yytext, yyleng);
                                     return (_time);
                                    }

{DATE}"T"{TIME}({ZONE})?           {
                                     yylval.item =
                                        ODLConvertDateTime (yytext, yyleng);
                                     return (_date_time);
                                    }

{DATEV0}"-"{TIME}                   {
                                     yylval.item =
                                        ODLConvertDateTime (yytext, yyleng);
                                     return (_date_timeV0);
                                    }

{LETTER}{ALPHANUMER}"."{ALPHANUMER} {
                                     ODLPrintWarning (MESSAGE1);
                                     yylval.item =
                                        ODLConvertString (yytext, yyleng);
                                     return (_text_string);
                                    }

[Nn]"/"[Aa]                         {
                                     ODLPrintWarning (MESSAGE2);
                                     yylval.item =
                                        ODLConvertSymbol (yytext, yyleng, 2);
                                     return (_symbol);
                                    }

"("                                 {
                                     return (_sequence_opening);
                                    }

")"                                 {
                                     return (_sequence_closing);
                                    }

"{"                                 {
                                     return (_set_opening);
                                    }

"}"                                 {
                                     return (_set_closing);
                                    }

"<"                                 {
                                     return (_units_opening);
                                    }

">"                                 {
                                     return (_units_closing);
                                    }

","                                 {
                                     return (_list_separator);
                                    }

"^"                                 {
                                     return (_point_operator);
                                    }

"="                                 {
                                     return (_assignment_operator);
                                    }

"*"                                 {
                                     return (_multiply_operator);
                                    }

"/"                                 {
                                     return (_divide_operator);
                                    }

"**"                                {
                                     return (_exponentiate_operator);
                                    }

".."                                {
                                     return (_range_operator);
                                    }

^{WHITESPACE}*"/*".*                {
                                     /* This is a comment line */

                                     yyGetComment ();
                                    }

"/*".*                              {
                                     /* This is a comment at the end of a
                                        line of ODL code -- ignore it */
                                    }

{WHITESPACE}                        {} /* Ignore white space */

{FORMATTER}                         {} /* Ignore format effectors */

.                                   { /* Return other characters verbatim */
                                     return (yytext[0]);
                                    }

%%

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

  /* Skip backward over ending comment delimiter */

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
