/*****************************************************************************
                                                                           
  WriteSource:  Writes the text of a PDS label to a file with line numbers.

  Input:
         source_file  - Pointer to the file descriptor for the source file
                        containing the label to be printed.
         output_file  - Pointer to the file descriptor for the output file.

  Output:  No function value is returned.  The text of the label with
           line numbers is written using output routine ODLWriteStmt.

  Author:  Randy Davis, University of Colorado LASP

  Creation Date: 18 May 1991

  History:

  Creation - This routine was introduced in Version 2.1 of the ODLC library.

  Version 2.2 - 18 May 1991 - M. DeMore, Jet Propulsion Laboratory
    a) Removed include statements that were Unix specific and placed them
       in odldef.h. Removed prototypes of ODL functions and added include
       file odlinter.h.
    b) Corrected argument declarations from FILE to FILE *.

  Version 2.3 - 6 February 1991 - M. DeMore, Jet Propulsion Laboratory
    a) Changed line number count to long int for PC.
 ****************************************************************************/
#include <ctype.h>
#include <errno.h>
#include <math.h>

#include "odldef.h"
#include "odlinter.h"

/* The following define the maximum number of characters in a source line,
   the width of the line number, and the maximum length of an output line */

#define  MAXSRCLINE 132
#define  LINOWIDTH  5
#define  MAXOUTLINE MAXSRCLINE+LINOWIDTH+1


void WriteSource (source_file, output_file)

/* >>>>>>>>>>>>>>>>>>>>>>>>>> BEGIN CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>>> */
/* >>>>> MDD 4/30/91  Changed FILE to FILE *                       >>>>> */
/* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> */

       FILE  *source_file;
       FILE  *output_file;

/* >>>>>>>>>>>>>>>>>>>>>>>>>> END OF CN CHANGES >>>>>>>>>>>>>>>>>>>>>>>> */
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
      sprintf (out_line, "%0*ld %s", LINOWIDTH, line_no, src_line);
      ODLWriteStmt (output_file, out_line);

      /* See if we have hit the END statement yet and if so, stop
         processing */

      if (strcmp (src_line, "END\n") == 0)
        {
          break;
        }
    }

  return;
}
