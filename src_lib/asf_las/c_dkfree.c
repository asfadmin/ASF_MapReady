/******************************************************************************
FUNCTION:	c_dkfree

PURPOSE:	Free reserved space on a given pack

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0      				Original development
  5.0      05/88    D. Hollaren		Broke into a seperate routine & document
  5.1	   10/90    T. Baltzer		Check return status's, add errmsg calls
  5.2	   11/90    T. Baltzer		enable recovery if DISKRSVD file gone
  5.5      05/90    B. Ailts		Added c_sigblk and c_sigunblk in order
 					that the interupts produced by the 
					processing message does corrupt the I/O.
  5.6      09/91    D. Etrheim		Modify to open the DISKRSVD file
					here instead of depending on cmlock
					to do it correctly. Ran thru Saber-C.
  5.5      04/92    T. Mittan	        Modify to close file ptr from cmlock
					if DISKRSVD file is opened here.	
  5.6      12/92    D. Etrheim		Remove the O_APPEND flag for opening 
					the file.  We want to overwrite the
					record, but the append flag puts us to
					the end of the file on write.
 6.0	   6/97     O. Lawlor   Massive destruction of complex, unused code.
  7.2      2/98     O. Lawlor (ASF) Removed superfluous calls to c_sigblk.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:
	None.

PROJECT			LAS

ALGORITHM 
  Get the current process ID
  Initailze the symbolic link name
  Open and lock the disk reserved file
  Loop until the end of the file
	Read each record of the disk reserved file
	If the record is label active
	    If the process ID matches
		If the file name matches
		    Flag the record as Done or inactive
		    rewrite the record
  End loop until EOF

  Close and unlock the disk reserved file

ALGORITHM REFERENCES	none
*******************************************************************************/
#include "asf.h"
#include "las.h"
#include "diskio.h"

/******************************************************************************
FUNCTION:	c_dkfree

PURPOSE:	Free reserved space on a given pack
******************************************************************************/
lasErr c_dkfree(char	*path)
{
	return(E_SUCC);		/* No op			    */
}
