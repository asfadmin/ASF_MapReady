/*==============================================================================
Filename:	sendfile.c

Description:	
	This module contains the server end functions for sending local
files to external systems via FTP or NFS.

External Functions:
	alloc_sendprms_record
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None

Notes:
==============================================================================*/

static char SccsFile[] = "%M%" ;
static char SccsRevision[] = "%R%" ;
static char SccsDate[] = "%G%";
static char SccsLastChanger[] = "%W%";
static char SccsState[] = "%I%";

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <unistd.h>
#include <syslog.h>
#include "faifdefs.h"
#include "sendfile.h"


#ifdef __STDC__
File_Transfer_Params *alloc_sendprms_record(void) ;
#else
File_Transfer_Params *alloc_sendprms_record() ;
#endif


extern void *util_do_malloc() ;



/*==============================================================================
Function:	File_Transfer_Params *alloc_sendprms_record(void)

Description:	
	Allocate space for a params record.  Once the record is
allocated, initialize its fields and return the allocated record.
NULL is returned if allocation does not succeed.

Parameters:	None
Returns:	pointer to allocated params record	
Creator:	Norbert Piega	
Creation Date:	08/02/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
File_Transfer_Params *
alloc_sendprms_record(void)
#else
File_Transfer_Params *
alloc_sendprms_record()
#endif
{
   File_Transfer_Params *sendprm ;

   sendprm = 
      (File_Transfer_Params *) util_do_malloc(sizeof(File_Transfer_Params)) ;
   if (sendprm != (File_Transfer_Params *)NULL)
   {
      sendprm->out_file = NULL ;
      sendprm->src_host = NULL ;
      sendprm->src_dir = NULL ;
      sendprm->dest_host = NULL ;
      sendprm->dest_dir = NULL ;
      sendprm->send_cmd = NULL ;
      sendprm->user = NULL ;
      sendprm->mode = NULL ;
      sendprm->rootpath = NULL ;
   }

   return(sendprm) ;
} /* alloc_sendprms_record */


/* End of file */
