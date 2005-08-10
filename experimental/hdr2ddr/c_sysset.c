/******************************************************************************
FUNCTION:	SYSSET

PURPOSE:	Set a system string or system numerical code.  Given the system
		string return the system numerical code or given the system
		numerical code return the system string.

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0		    K. Gacke		Original development
  5.0      02/89    D. Hollaren		Conversion to 5.0 standards
  5.1	   07/90     D. VanderZee	Standardized error handling
  5.2	   04/91    T. Baltzer		Add Silicon Graphics, Data General

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 
	 Convert the input system string to lower case
	 Find the input system string in the list of valid system strings
	 If the input system string was not found
	     display an error message and
	     return bad status

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"


#include "worgen.h"
#include "sysdef.h"

lasErr FUNCTION c_sysset(const char *system,int *nsys)
	/* character coded system (e.g. "gould-utx") */
	/* system numerical code (e.g. SYS_GOULD_UTX)*/
{
	/*int len;*/			/* Length of input system string	      */
	int index;			/* Looping variable			      */
	lasErr status = E_FAIL;		/* Return status			      */

	static char *sys_name[] = 	/* Array of valid system strings	      */
		{IEEE,IEEE_LIL,MVS,UNICOS,MSC,NULL};

	/*Find the input system string in the list of valid system strings*/
	for (index = 0; sys_name[index] ; index++)
		if (strcmp(system,sys_name[index]) == 0)
		{
			*nsys = index;
			status=E_SUCC;
			break;
		}

	return(status);
}

