/******************************************************************************
FUNCTION:	chkwindow

PURPOSE:	Checks to see if the specified window is outside the actual
		window for a LAS file.

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
	   06/85    T. Butzer		Original implementation
	   12/87    B. Ailts		change include directory structure
 	   12/87    B. Ailts		implemented new DDR structure
 	   02/91    B. Ailts		Standardized error messages

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 
Check the window specifications against the DDR window specs.
If the window specifications are outside the DDR window specs
   Return a fatal error

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"

#include "las.h"
#include "locinc.h"

lasErr FUNCTION check_window(struct FDESC *fd,struct DDR *ddr)
	
{
/*lasErr status;*/

char *errtxt;

if ((fd->sl + fd->nl - 1) > ddr->nl)
	return(E_FAIL);

if ((fd->ss + fd->ns - 1) > ddr->ns)
	return (E_FAIL);

if ((fd->sl != 1) || (fd->ss != 1) || 
    (fd->nl != ddr->nl) || (fd->ns != ddr->ns))
   {
   errtxt = (char *)MALLOC(ERRLEN);

   sprintf (errtxt,"Window specified for file %s is (%d %d %d %d)",
	    fd->fname,
            fd->sl,fd->ss,fd->nl,fd->ns);
   c_errmsg(errtxt,"chkwindow-window",NON_FATAL);

   free(errtxt);
   }
return(E_SUCC);
} 
