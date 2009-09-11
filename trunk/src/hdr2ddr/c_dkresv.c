/******************************************************************************
FUNCTION:	c_dkresv

PURPOSE:	Reserves space on a given pack

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
 1.0   					Original development
 5.0	   05/88    D Hollaren		added calles to cerrmsg
 5.1	   10/90    T. Baltzer		check return status's, add errmsg calls
 5.2	   11/90    T. Baltzer		enable recovery if DISKRSVD file gone
 5.5       05/90    B. Ailts		Added c_sigblk and c_sigunblk in order
 					that the interupts produced by the 
					processing message does corrupt the I/O.
 5.6       09/91    D. Etrheim          Modify to open the DISKRSVD file
                                        here instead of depending on cmlock
                                        to do it correctly. Ran thru Saber-C.
 5.7       04/92    D. Etrheim          Modify to close file ptr from cmlock
                                        if DISKRSVD file is opened here.
 6.0        6/97    O. Lawlor          Removed bloated, buggy, unportable I/O code.
  7.2      2/98     O. Lawlor (ASF) Removed superfluous calls to c_sigblk.
 
COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 
  Get the current process id
  Initailize the file name
  Fill the disk reserve file record with the path name, process id, size, and
  active flag.
  open and lock the disk reserve file
  write the record
  close and unlock the disk reserve file

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"
#include "las.h"
#include "diskio.h"

/******************************************************************************
FUNCTION:	c_dkresv

PURPOSE:	Reserves space on a given pack
******************************************************************************/
int c_dkresv(char	*path, int	size)
{
	return(E_SUCC);		/* No op				*/
}
