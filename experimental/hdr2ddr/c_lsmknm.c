/*********************   Label Services Subsystem   *********************
FUNCTION:	lsmknm

PURPOSE:	Given an image name, make a label services file name.

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
           08/85    T. Bodoh     	initial development
	   07/86    K. Gacke		newLAS development
	   03/87    D. Gordon		added end of directory check for vax
	   12/87    B. Ailts		change include directory specifications
					place bridge routines in a seperate file
					replace DESC arguments with char strings
					use raw 'C' types
  5.0      04/88    D. Hollaren		changed include spec to las

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 
Make sure the name isn't to long
Otherwise, append the specified extention

ALGORITHM REFERENCES	none
*******************************************************************************/

#include "asf.h"
#include "las.h"

void FUNCTION c_lsmknm(const char *inname,const char *ext,char *outname)
{
char *newName=appendExt(inname,ext);
strcpy(outname,newName);
free(newName);
return;
}
