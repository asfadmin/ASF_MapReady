#include "pdsdef.h"
#include "odlinter.h"

extern int yylineno;

int  ODLerror_count;
int  ODLwarning_count;
int  ODLlinenumber_flag;


void ODLPrintError (error_msg)

char  error_msg[];
{
    char *temp_str = {NULL};

    temp_str = (char *) malloc (50 + String_Size(error_msg));
    if (temp_str == 0) 
        exit (1);
    sprintf (temp_str, "ERROR:   Line %d -- %s", yylineno, error_msg);
    err_append_message (CONTINUE, temp_str);
    ODLerror_count++;
    free (temp_str);
    return;
}

void ODLPrintWarning (warning)

char warning[];
{
    char *temp_str = {NULL};

    temp_str = (char *) malloc (50 + String_Size(warning));
    if (temp_str == 0) 
        exit (1);

    sprintf (temp_str, "WARNING: Line %d -- %s", yylineno, warning);
    err_append_message (CONTINUE, temp_str);
    ODLwarning_count++;
    free (temp_str);
    return;
}


void ODLPrintInfo (info_message)

char info_message [];
{
    char *temp_str = {NULL};

    temp_str = (char *) malloc (50 + String_Size(info_message));
    if (temp_str == 0) 
        exit (1);

    sprintf (temp_str, "INFO:    %s", info_message);
    err_append_message (CONTINUE, temp_str);
    free (temp_str);
    return;
}




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
