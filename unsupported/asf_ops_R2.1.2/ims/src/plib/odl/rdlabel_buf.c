/*****************************************************************************

  Routine: ReadLabel_buf
 
  Description: Reads in a PDS label and places the label information
               into an ODL tree.
 
  Input:
          input_file - Pointer to the file containing the label.
          root       - Pointer to the root node on the ODL parse tree.
 
  Output: A value of one is returned as the function value if parsing was
          completed successfully; otherwise a value of zero is returned
          and an summary of errors and warnings is given.
 
  Author:  Nadia Adhami, Nadia.Adhami@Jpl.Nasa.Gov

  Creation Date:  20 May 1995

  History:
	1. set the global buffer pointer yybuf to the input buffer

*****************************************************************************/
#include <ctype.h>
#include <errno.h>
#include <math.h>

#include "odldef.h"
#include "odlinter.h"

/* The following statements duplicate two lexical analyzer constants for
   use in this module */

#define YYLMAX BUFSIZ
#define YYNEWLINE 10

/* The following variables are defined in the module containing the
   parser action routines. */

extern AGGREGATE  ODLroot_node;         /* Pointer to root node of ODL tree */
extern AGGREGATE  ODLcurrent_aggregate; /* Pointer to current aggregate node*/

/* The following variables are defined with the output routines
   ODLPrintError and ODLPrintWarning */

extern int        ODLerror_count;       /* Cumulative count of errors       */
extern int        ODLwarning_count;     /* Cumulative count of warnings     */
extern int        ODLlinenumber_flag;   /* Controls output of line nos.     */

/* The following variables are defined in the lexical analyzer module */

extern FILE *yyin;                  /* Pointer to file for parser input     */
extern FILE *yyout;                 /* Pointer to file for parser output    */

extern char  yysbuf[YYLMAX];        /* Lexical analysis rescan buffer       */
extern char *yysptr;                /* Pointer into lexical rescan buffer   */

extern int   yylineno;              /* Current input line number            */
extern int   yyprevious;            /* Previous lexical analysis state      */

char *yybuf = NULL;                 /* Pointer into lexical buffer   */

#ifdef PDS_TOOLBOX

extern int  pds_line_offset;        /* SFDU offset for PDS toolbox          */

#endif

int ReadLabel_buf (input_file,root)

     char     *input_file;
     AGGREGATE  root;

{
  char  warning[120];               /* String to contain warning message    */

  /* set the global buffer pointer to the input buffer */

  yybuf  = input_file;

  /* Set pointers for the files to be used by the parser */

  yyin  = NULL;
  yyout = NULL;

  /* Reset other variables used by the lexical analyzer */

  yyprevious = YYNEWLINE;
  yysptr = yysbuf;
  yylineno = 1;

#ifdef PDS_TOOLBOX

   yylineno += pds_line_offset;

#endif

  /* Initialize the error and warning counters and indicate that line
     numbers are to appear in all error and warning messages */

  ODLerror_count       = 0;
  ODLwarning_count     = 0;
  ODLlinenumber_flag   = 1;

  /* Initialize the node pointers */

  ODLroot_node         = root;
  ODLcurrent_aggregate = root;
  
  /* Call the parser to read in and process the label */

  if (yyparse() != 0)
  {
    sprintf (warning,
             "Label reading complete with %d errors, %d warnings.",
             ODLerror_count,
             ODLwarning_count);
    ODLPrintWarning (warning);
    return (0);
  }
  else
  {
    return (1);
  }
}
