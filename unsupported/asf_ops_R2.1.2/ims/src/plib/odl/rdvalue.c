/*****************************************************************************

  Routine: ReadValue
 
  Description: Parses an ODL value given as ASCII text and assigns it to a
               specified parameter. The input text string may contain any
               ODL scalar, sequence or set value.  This routine is used to
               process values entered in text format as input to label
               editors and similar programs.  To do its job, this routine
               forms an ODL parameter assignment statement and then calls
               the ODL parser to parse the statement and place the results
               onto an ODL tree.

  Input:
          node         -   Pointer to the aggregate node to which the
                           parameter and value are to be attached.
          parameter_name - Character string containing the parameter name.
          value_string -   Character string containing the value in ASCII
                           text format.
 
  Output: A value of one is returned as the function value if parsing was
          completed successfully; otherwise a value of zero is returned.
 
  Author:  Randy Davis, University of Colorado LASP

  Creation Date:  09 October 1990
  Last Modified:  18 May 1991

  History:

    Creation - This routine first appeared in Version 2.0 of the ODLC library.

    Version 2.1 - 13 March 1991 - Randy Davis, U. of Colorado LASP
      a) Set global variable ODLlinenumber_flag to FALSE to indicate that
         line numbers shouldn't be included in error messages generated
         by the parser when called by this routine.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    a) Removed include statements that were Unix specific and placed them
       in odldef.h.

*****************************************************************************/
#include <ctype.h>
#include <errno.h>
#include <math.h>

#include "odldef.h"

/* The following statements duplicate two lexical analyzer constants for
   use in this module */

#define YYLMAX BUFSIZ
#define YYNEWLINE 10

/* The following statements define a routine for placing characters into
   the lexical analyzer's rescan buffer */

#define unput(X) for (nc=strlen(X); nc > 0; yyunput(X[--nc]))

extern int nc;                                 /* Number of characters to unput    */

/* The following variables are defined in the module containing the
   parser action routines. */

extern AGGREGATE  ODLroot_node;         /* Pointer to root node of ODL tree */
extern AGGREGATE  ODLcurrent_aggregate; /* Pointer to current aggregate node*/

extern int        ODLerror_count;       /* Cumulative count of errors       */
extern int        ODLwarning_count;     /* Cumulative count of warnings     */
extern int        ODLlinenumber_flag;   /* Flag to control line no. output  */

/* The following variables are defined in the lexical analyzer module */

extern FILE *yyin;                  /* Pointer to file for parser input     */
extern FILE *yyout;                 /* Pointer to file for parser output    */

extern char  yysbuf[YYLMAX];        /* Lexical analysis rescan buffer       */
extern char *yysptr;                /* Pointer into lexical rescan buffer   */

extern int   yylineno;              /* Current input line number            */
extern int   yyprevious;            /* Previous lexical analysis state      */


int ReadValue (node,parameter_name,value_string)

     AGGREGATE    node;
     char        *parameter_name;
     char        *value_string;

{

  /* Reset pointers for the files to be used by the parser */

  yyin  = NULL;
  yyout = NULL;

  /* Reset other variables used by the lexical analyzer */

  yyprevious = YYNEWLINE;
  yysptr = yysbuf;
  yylineno = 1;
  
  /* Initialize the error and warning counters, and reset the line number
     output flag so that line numbers won't be reported in any error
     messages */

  ODLerror_count       = 0;
  ODLwarning_count     = 0;
  ODLlinenumber_flag   = 0;

  /* Initialize the node pointers */

  ODLroot_node         = node;
  ODLcurrent_aggregate = node;
  
  /* Place the parameter name and value string into the lexical analyzer's
     rescan buffer, along with an END statement to terminate the mini-label
     we're creating */

  unput ("\nEND\n");
  unput (value_string);
  unput ("=");
  unput (parameter_name);

  /* Call the parser to read in and process the value. The result will be
     a new parameter node (along with the associated value information)
     attached to the specified aggregate node */

  if (yyparse() != 0)
  {
    return (0);
  }
  else
  {
    return (1);
  }
}


