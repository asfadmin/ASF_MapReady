/*********************   Label Services Subsystem   ***************************
FUNCTION:	lsopen

PURPOSE:	Open an associated file.

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
           04/86    C. Greenhagen	initial development
	   07/86    K. Gacke		newLAS development
           10/86    T. Butzer		delete file on write if exists (VMS)
	   12.87    B. Ailts		change include file directory spec
					replace DESC arguments with char strings
					use raw 'C' types
					place bridge routines in a seperate file
  5.0      04/88    D. Hollaren		changed include spec to las
  5.1     11/88	   B. Ailts		Replaced las.h with worgen.h so these
					routines can be placed in world
					Replaced c_delfil with delete so this
					routine can be placed in world
  5.2     07/90    D. VanderZee         Standardized error handling
  5.3     01/91    K. Gacke		Changed i/o to buffered i/o for sun --
					sun has a problem with direct i/o when
					the close takes to int to be executed
  5.4     03/93    D. Etrheim		Changed pmode to 666 for file open.
					This will allow users to use umask 
					appropriately. Execute permision is not
					needed for any files created by LAS.
  6.0	  6/97     O. Lawlor            Changed to standard I/O.
  
COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 
Set the open flags according to the specified mode
In VMS
    If writing to the file and the file already exists
	delete the file
Open the file

ALGORITHM REFERENCES	none
*******************************************************************************/

#include "asf.h"

#include "worgen.h"
#include "vll.h"


lasErr FUNCTION c_lsopen ( FILE **fd, const char *hname, int *rwmode)
{
FILE 	*fp;
char    msgtxt[ERRLEN + 1];
char 	*flags;

if (*rwmode == READ)				/* set flags:		      */
    flags = "r";				/* read only */
else if (*rwmode == WRITE)			/* write only - append	      */
    flags = "w";
else if (*rwmode == APPEND)			/* write only - append	      */
    flags = "a";
else if (*rwmode == RW)				/* read/write		      */
    flags = "r+";
else
    {
    sprintf(msgtxt,"Invalid open mode specified: %d",*rwmode);
    c_errmsg(msgtxt,"lsopen-mode",NON_FATAL);
    return(E_FAIL);
    }
fp = FOPEN(hname,flags);
*fd = fp;
return(E_SUCC);
}
