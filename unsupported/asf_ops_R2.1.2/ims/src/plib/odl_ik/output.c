/*****************************************************************************

  Description: Routines for writing error messages, warning messages and the
               statements written by PrintLabel and WriteLabel.
 
  Author:  Randy Davis, University of Colorado LASP

  Creation Date:  25 August 1990
  Last Modified:  18 May, 1991

  History:

    Creation - This set of routines was introduced in Version 2 of the
    ODLC library.  These routines are 'standard' versions of output
    routines for the error and warning messages generated while reading
    and parsing a label and for the statements written by PrintLabel and
    WriteLabel.  The error and warning routines put out messages to the
    'stderr' file.  ODLPrintStmt writes to the 'stdout' file.  ODLWriteStmt
    writes to a file specified by the first argument to the routine.  These
    routines were introduced in the Version 2.0 release of the ODLC library.
    Software developers using the ODLC package can customize their
    program's output by providing their own versions of these routines.
    For example, user-provided versions of the print routines might be
    used to place output into special windows on a workstation screen.

    Version 2.1 - 14 March 1991 - R. Davis, U. of Colorado LASP
      a) Defined the ODLerror_count and ODLwarning_count global variables in
         this module rather than in the file with the parser action routines.
         Also added a new global variable, ODLlinenumber_flag, to make the
         reporting of line numbers in error/warning messages optional.  This
         allows the error routines to be used for other purposes than
         printing and writing messages from the label parser.
      b) Added routine ODLPrintInfo to print information messages.

    Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
       Removed include statements that were Unix specific and placed them
       in odldef.h. Added include file odlinter.h.

*****************************************************************************/

#include "odldef.h"
#include "odlinter.h"


/* The following external variable is defined and set by the lexical
   analyzer and is used here in error reporting.                            */

extern int yylineno;       /* Current source line number                    */

/* The following external variables are defined here and used in other
   modules */

int  ODLerror_count;       /* Cumulative count of errors                    */
int  ODLwarning_count;     /* Cumulative count of warnings                  */
int  ODLlinenumber_flag;   /* TRUE if line number to be reported in message */



/*****************************************************************************

  Routine: ODLPrintError
 
  Description: Prints a message reporting an error detected during the
               parsing of an ODL label.
 
  Input:
          error_msg - Character string with text of error message.
 
  Output: The error message string is printed to the stderr output file.

*****************************************************************************/

#ifndef EOSDIS			/* GMS: if we are the EOSDIS project, the 
				 * output has been redirected to IK_Syslog */
void ODLPrintError (error_msg)
     char  error_msg[];
{

  if (ODLlinenumber_flag)
    {
      /* Include the line number in the error message */

      fprintf (stderr, "**Error at line %d:\n", yylineno);
    }
  else
    {
      fprintf (stderr, "**Error:");
    }

  /* Put out the error message text and increment the error count */

  fprintf (stderr, "  %s\n", error_msg);
  ODLerror_count++;

  return;
}



/*****************************************************************************

  Routine: ODLPrintWarning
 
  Description: Prints a message to warn of a non-fatal problem detected
               during the parsing of an ODL label.
 
  Input:
          warning - Character string with text of warning message.
 
  Output: The warning message string is printed to the stderr output file.

*****************************************************************************/

void ODLPrintWarning (warning)
     char warning[];
{

  if (ODLlinenumber_flag)
    {
      /* Include the line number in the warning message */

      fprintf (stderr, "**Warning at line %d:\n", yylineno);
    }
  else
    {
      fprintf (stderr, "**Warning:");
    }

  /* Put out the warning message text and increment the warning count */

  fprintf (stderr, "  %s\n", warning);
  ODLwarning_count++;

  return;
}



/*****************************************************************************

  Routine: ODLPrintInfo
 
  Description: Prints a message to inform the user of a situation that
               is important but not an error or warning.
 
  Input:
          info_message - Character string with text of the message.
 
  Output: The message string is printed to the stdout output file.

*****************************************************************************/


void ODLPrintInfo (info_message)
     char info_message[];
{

  /* Put out the message text */

  fprintf (stdout, "**Note: %s\n", info_message);

  return;
}

#endif



/*****************************************************************************

  Routine: ODLPrintStmt
 
  Description: Prints an ODL statement.
 
  Input:
          stmt - Character string with statement to be printed.
 
  Output: The statement is printed to the stdout output file.

*****************************************************************************/


void ODLPrintStmt (statement)
     char statement[];
{

  fputs (statement, stdout);

  return;
}




/*****************************************************************************

  Routine: ODLWriteStmt
 
  Description: Writes an ODL label statement to the specified file.
 
  Input:
          output_file - Pointer to file to which statement is to be written.
          statement - Character string with statement to be written.
 
  Output: The statement is printed to the output file.

*****************************************************************************/


void ODLWriteStmt (output_file,statement)
     FILE *output_file;
     char  statement[];
{

  fputs (statement, output_file);

  return;
}
