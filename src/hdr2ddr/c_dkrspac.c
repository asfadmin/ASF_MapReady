/******************************************************************************
FUNCTION:	c_dkrspac

PURPOSE:	Returns how much space is presently software reserved

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0					Original development
  5.0      05/88    D Hollaren		added c_errmsg and las include
  5.1      03/89    D Hollaren          Now returns the correct amount of space
					used
  5.2	   10/90    T. Baltzer		check return status's, add errmsg calls
  5.3	   11/90    T. Baltzer		enable recovery if DISKRSVD file gone
  5.5      05/90    B. Ailts		Added c_sigblk and c_sigunblk in order
 					that the interupts produced by the 
					processing message does corrupt the I/O.
  5.6      09/91    D. Etrheim          Modify to open the DISKRSVD file
                                        here instead of depending on cmlock
                                        to do it correctly. Ran thru Saber-C.
  5.7      04/92    D. Etrheim          Modify to close file ptr from cmlock
                                        if DISKRSVD file is opened here.
  5.8	   05/92    T. Mittan		Declare thisdev as an int for DG.
					The corresponding sturcture element
					statp.st_dev is defined as an int on DG.
  5.9      12/93    D. Etrheim		Modify calling sequence for cmlock so
                                        this is backword compatible for 
					Solaris 2.3.
  7.0	    4/95    T. Logan		Removed references to sprintf that were
					not compiling properly.
  8.0		6/97    O. Lawlor       Laid waste to wasteful, unused disk I/O code.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 
Get status information on the specified file
Get the device the file is on
Open and lock the software disk reserve file

Read each record of the file
    If the record has an active file
	get the device the file is on from the status information
	if the device is the same as the device for the input file
	    accumilate the software reserved space on that device
read next record until End Of File    

Close and unlock the software disk reserve file

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"
#include "las.h"
#include "diskio.h"

/******************************************************************************
FUNCTION:	c_dkrspac

PURPOSE:	Returns how much space is presently software reserved

******************************************************************************/
int c_dkrspac(char	*path)
{
	return(0);		/* No op				*/
}
