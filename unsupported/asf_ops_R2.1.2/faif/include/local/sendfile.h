/*==============================================================================
Filename:	sendfile.h

Description:	
	This file contains the data structure used to pass file transfer
information to the get and send functions (get/send via FTP, NFS or FTAM).

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _SENDFILE_
#define _SENDFILE_

#ifndef ERROR
#include "faifdefs.h"
#endif

#define FTPSEND_SCRIPT_SUBPATH ""
#define FTPGET_SCRIPT_SUBPATH ""

#define NFS_SEND_CMD_PATH "/usr/bin/cp"
#define NFS_SEND_CMD "cp"

#define MAX_DEST_STRLEN  132

#define BINARY  "binary"
#define ASCII   "ascii"

typedef struct file_transfer_params
{
   char *out_file ;
   char *src_host ;
   char *src_dir ;
   char *dest_host ;
   char *dest_dir ;
   char *send_cmd ;
   char *user ;
   char *mode ;
   char *rootpath ;
} File_Transfer_Params ;

#endif /* _SENDFILE_ */

/* End of File */
