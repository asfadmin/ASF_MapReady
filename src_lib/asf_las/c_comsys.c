/******************************************************************************
NAME:	COMSYS

FUNCTION:
        COMSYS compares the system that this software is being run on and
	the system that created the input ddr.

PROGRAM HISTORY:
  Version	Date       Author       Request
  -------	----	   ------       -------
    1.0         10/87       B.Ailts     initial development
    1.1		12/87       B.Ailts     Change include directory specifications
					Place the bridge routine in a seperate
					file
					Replace DESC arguments with char. string
    1.2		04/88       B.Ailts     Replaced newlas.h with las.h
    5.0         03/89       D. Hollaren Added IEEE, PRIME, and IBM machines to
                                        the table.  Made table into static array
    5.1		01/90	    D. Akkerman Modified to call c_getddr directly
					rather than using label services.
    5.2         12/90       B. Ailts    Updated the error messages
    6.0         6/98	O. Lawlor: Tore out guts; moved to c_getddr.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:	
		Run under TAE

PROJECT: 	LAS

ALGORITHM:

ALGORITHM REFERENCES:

******************************************************************************/

#include "asf.h"
#include "las.h"
#include "sysdef.h"

#define SAME		0
#define NOT_REAL	1
#define ONLY_BYTE	2
#define NONE		3

lasErr FUNCTION c_comsys(char *hostname,int *flag)
{
	*flag=SAME;
	return(E_SUCC);
}
