/******************************************************************************
FUNCTION:	c_dkdel

PURPOSE:	Delete the specified file

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0      				Original development
  5.0      05/88    D. Hollaren		added calles to c_errmsg
  5.0      11/88    B. Davis   		added sys_errlist, allowed dir deletion
  5.0	   10/90    T. Baltzer		fix return status checking, errmsg calls
  5.1	   09/91    B. Ailts		fixed a spelling error

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 
  Dynamically allocate space for the symbolic link name and file status struct
  Assume the file name is a symbolic link and try to find it
  	If an error is found looking for the link
		return an error
	
  Otherwise; get the status information about the symbolic link file
	If an error occurs getting the status
		return an error
	else
		delete the linked file

  Delete the file name
  Free the space for the symbolic link name and file status structure

ALGORITHM REFERENCES	none
*******************************************************************************/

#include "asf.h"

#include <unistd.h>
#include "las.h"
#include "diskio.h"

lasErr c_dkdel(char *filename)
{
	int status;		/* status code				*/

	/*char msg[ERRLEN];*/	/* Error message string			*/
	/*char taenm[CMLEN];*/	/* Error message name			*/

/* Delete the file			*/
	status = unlink(filename);

	return((lasErr)status);
}
