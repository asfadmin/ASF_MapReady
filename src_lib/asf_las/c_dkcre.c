/******************************************************************************
FUNCTION:	c_dkcre

PURPOSE:	Create and reserve space for a file.  The file will be
		created and space reserved for the file in one of the
		specified directory paths.  The disk space available
		that can be best utilized for the creation of the file
		is used.

PROGRAM HISTORY:
Version    Date     Author       	Change Request
-------    -----    -------------	--------------
  1.0	            			Original development
  5.0      05/88    D. Hollaren		Put in calles to c_errmsg
  5.1	   07/88    D. Hollaren		Fixed bug that created two versions of
					an image on the vax
  5.2 	   10/90    T. Baltzer		Fix return status's, errmsg calls
  5.5      05/90    B. Ailts		Added c_sigblk and c_sigunblk in order
 					that the interupts produced by the 
					processing message does corrupt the I/O.
  5.6	   08/91    B. Davis		longs changed to doubles for large disk
  5.7      09/91    D. Etrheim		Modified to do a first fit instead of
					a best fit for SG. PRF #6022
  5.8      02/92    D. Etrheim          Change to use bfree for DG.  This seems
					to better represent what can actually
					be allocated
  5.9	   05/92    T. Mittan		Change to use bavail for DG.  This gives
					a better representation of availible
					disk space
  6.0	   07/92    T. Mittan		type cast longs to doubles
  6.1      07/93    D. Etrheim          Modifed to do a first fit for all 
                                        systems
  6.2s	   10/94    R. White		For Solaris  #undefined
					v_count defined by TAE #includes
					to avoid conflict with
					<sys/vnode.h>, #included several
					files defining system functions,
					replaced statfs() by statvfs(),
					changed <strings.h> to	<string.h>,
					replaced getwd() by getcwd(),
					and removed non-ANSI declarations
					of standard C library functions. 
  7.0	   4/95	    T. Logan (ASF)	Removed TAE dependencies
  7.1      6/97     O. Lawlor (ASF) Added Prototypes, massively simplified logic.
  7.2      2/98     O. Lawlor (ASF) Removed superfluous calls to c_sigblk.

COMPUTER HARDWARE AND/OR SOFTWARE LIMITATIONS:  None.

PROJECT			LAS

ALGORITHM 
  Make sure file dosen't already exist
  Allocate space for the current working directory,
    the DATAPATH,
    file status structure, and
    file systems statistics

  Get the current working directories
  Set an array of pointers to the directory paths
  Get the directory paths to be search for the creation of the file
    (if no directory paths were specified the current working directory is used)

  Loop until the file is created or try once if wait flag is not set
	Search the paths to determine the best location to create the file
	Try to create the file with that path
	If the the file was created break from the loop
	If the wait flag was not set and not enough disk space was avaliable
	   break with an error
	Send a message to the operators that there is not enough disk space
	Wait for the specified amount of time
  End of loop; try to create the file again.

  Free up space allocated for;
	the current working directory,
	datapath, and
	file status structure

  return status

ALGORITHM REFERENCES
  The DATAPATH contains directories seperated by a colon ":"
  ie) "/software/test:/production/job:/production/batch/proj1"
*******************************************************************************/
#include "asf.h"
#include <unistd.h>
#include "las.h"
#include "diskio.h"

lasErr c_dkcre(char	*filename,int filesize)
{
/* Check to see if the file already exists				*/
	return (E_SUCC);
}
