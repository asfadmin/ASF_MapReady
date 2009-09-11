/*********************   Label Services Subsystem   *********************
FUNCTION:	lsstat

PURPOSE:	Ascertain file presence and requested access permissions.

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
           04/86    C. Greenhagen	initial development
	   07/86    K. Gacke		newLAS development
	   12/87    B. Ailts		place bridge routines in seperate file
					use raw 'C' types
					change include directory specifications
					replace DESC arguments with char strings
  5.0      04/88    D. Hollaren		change include spec to las
					return E_SUCC/E_FAIL
  5.1     11/88	   B. Ailts		Replaced las.h with worgen.h so these
					routines can be placed in world

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 
Set the return status to E_FAIL if the specified mode can not be done

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"

#include <unistd.h>
#include "worgen.h"
#include "vll.h"

lasErr FUNCTION c_lsstat (const char *hostname, int *mode)
{
#ifndef macOS
if (access(hostname,*mode) != 0)		/* success   */
   return(E_FAIL);
#else
	FILE *f=fopen(hostname,"r");
	if (f==NULL)
		return(E_FAIL);
#endif
return(E_SUCC);
}
