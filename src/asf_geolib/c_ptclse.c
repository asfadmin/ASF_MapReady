/******************************************************************************
FUNCTION:			c_ptclse

PURPOSE:  Closes a tie point file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  5.0	   11/88    D. Steinwand	Original development
  5.1      07/90    D. Etrheim		Standardized error message handling

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	Has to be run under TAE

PROJECT	  LAS

ALGORITHM 
	Close file
	If not successful, return an error
	Return

ALGORITHM REFERENCES	LAS 5.0 imageio routines	
*******************************************************************************/
#include "worgen.h"
#include "geompak.h"
#include "ptio.h"

int c_ptclse(ptdesc)

struct PTDESC **ptdesc;		/* File descriptor array */
{
register struct PTDESC *fd;
int action = 0;		/* Normal close */

fd = *ptdesc;	
if (c_lsclos(&(fd->fptr), fd->name, &action) != E_SUCC)
   {
   c_errmsg("Error closing tie point file","ptclse-close",NON_FATAL);
   return(E_FAIL);
   }
return(E_SUCC);
}
