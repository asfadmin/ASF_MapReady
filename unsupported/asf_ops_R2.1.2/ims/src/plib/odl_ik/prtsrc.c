/*****************************************************************************
                                                                           
  PrintSource:  Prints the text of a PDS label with line numbers.
                Note: For this routine to work properly, the source
                label must end in the standard PDS fashion: with the
                word END (in all uppercase) at the beginning of a
                line followed immediately by a new line.
  Input:
         source_file  - Pointer to the file descriptor for the file
                        containing the label to be printed.
         first_line   - First line number to be printed.
         last_line    - Last line number to be printed.

  Output:  No function value is returned.  The text of the label with
           line numbers is printed using output routine ODLPrintStmt.

  Author:  Randy Davis, University of Colorado LASP

  Creation Date: 13 March 1991
  Last Modified: 18 May 1991

  History:

  Creation - This routine was introduced in Version 2.1 of the ODLC library.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    a) Removed include statements that were Unix specific and placed them
       in odldef.h.
    b) Corrected argument declarations from FILE to FILE *.
    c) Added include file oldinter.h and remove prototypes of ODL functions

  Version 2.3 - 6 February 1992 - M. DeMore, Jet Propulsion Laboratory
    a) changed to use long ints due to PC bug

****************************************************************************/


#include "odldef.h"
#include "odlinter.h"

/* The following define the maximum number of characters in a source line,
   the width of the line number, and the maximum length of an output line */

#define  MAXSRCLINE 132
#define  LINOWIDTH  5
#define  MAXOUTLINE MAXSRCLINE+LINOWIDTH+1


void PrintSource (source_file, first_line, last_line)

/* >>>>>>>>>>>>>>>>>>>>>>>>>> BEGIN CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */
/* >>>>> MDD 4/30/91  Changed FILE to FILE *                       >>>>> */
/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

       FILE  *source_file;

/* >>>>>>>>>>>>>>>>>>>>>>>>>> END OF CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>> */

       long   first_line;
       long  last_line;
{
  char  src_line[MAXSRCLINE]; /* Buffer to hold input line of the label   */
  char  out_line[MAXOUTLINE]; /* Buffer to hold output line               */
  long   line_no;              /* Line number                              */

  line_no = 0;

  while (fgets (src_line, MAXSRCLINE, source_file) != NULL)
    {
      /* Increment the line number, convert it to a character string
	 representation and place in line buffer */

      line_no++;
      if (line_no > last_line)
	{
	  break;
	}
      else if (line_no >= first_line)
	{
	  sprintf (out_line, "%0*ld %s", LINOWIDTH, line_no, src_line);
	  ODLPrintStmt (out_line);
	}

      /* See if we have hit the END statement yet and if so, stop
	 processing */

      if (strcmp (src_line, "END\n") == 0)
	{
	  break;
	}
    }

  return;
}
