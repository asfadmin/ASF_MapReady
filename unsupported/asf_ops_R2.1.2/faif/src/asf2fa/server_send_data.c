/*************************************************************************
 * Copyright (c)1995 California Institute of Technology. U.S. Government *
 * Sponsorship acknowledged.                                             *
 *************************************************************************/

/*==============================================================================
Filename:	server_send_data.c

Description:	
	This module contains the DCE remote procedure "send_file_data" - 
the DCE RPC server function

External Functions:
	send_file_data

Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:

==============================================================================*/

static char SccsFile[] = "server_send_data.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "30 Aug 1996";
static char SccsLastChanger[] = "@(#)server_send_data.c	1.3";
static char SccsState[] = "1.3";

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <syslog.h>
#include <sys/file.h>
#include <dirent.h>
#include "faifdefs.h"
#include "sendtoFA.h"
#include "server_send_data.h"

#define MAX_BUF_ELEMENTS  1000


#ifdef __STDC__
short int send_file_data(handle_t, Custom_File_Handle, Pipe_Char_Type,
   error_status_t *) ;
#else
short int send_file_data() ;
#endif

extern char *FAxmitserver_configfile ;

extern void *util_do_malloc() ;
extern int   send_file_to_FA() ;
extern char *get_config_val() ;


/*==============================================================================
Function:	short int send_file_data(handle_t bind_handle,
			 Custom_File_Handle custom_handle,
		         Pipe_Char_Type ASF_pipe_data,
			 error_status_t *dce_error)

Description:
	This is the remote procedure which is actually called by the
DCE client part of the send file to FA service.  This remote procedure
does the actual data pull from the client input pipe.  The DCE
bind_handle is passed for DCE client-server connection; the
custom_handle passes client file info such as filename and destination
FA; and ASF_pipe_data is the pipe type structure mainly referenced to
be able to execute the pipe pull operation.

	This function basically performs the pipe pull then writes
pulled data into a local file.  The local file once completed is then
sent out to the appropriate flight agency by calling send_file_to_FA.

	In case the status returned by any of the functions called is
an error status, syslog is called to log the error.  Failure of this 
remote procedure to perform file transfer to the destination flight 
agency causes the return of an error status.

Parameters:
	ASF_pipe_data - Data transferred from the client through the 
pipe are defined as a certain pipe data type.  In this case, the pipe 
data is type character data pipe.  Note the type Pipe_Char_Type is 
defined as follows:

	typedef pipe char Pipe_Char_Type ;

The pipe data structure includes fields for specifying the buffer
allocation routine and the data pull routine names to be used by the
pipe and the pipe state record it will use.  The pipe state record
contains file specification information - file handle, filename.

        bind_handle - DCE RPC binding handle obtained via client
initialization.  The binding handle contains the information the 
client needs to connect to the server.  This binding handle is 
obtained via RPC library calls.

        custom_handle - Custom binding handle for DCE pipe.  This
data structure contains file info such as file name, file descriptor,
file destination info and possibly other file handle related data.
 
The client code must initialize the information encapsulated in this
structure prior to passing it in the RPC.

Returns:
	OK - send operation completed successfully
	ERROR - failure status returned by RPC server

Creator:	Norbert Piega	
Creation Date:  06/13/1994	
Notes:		
==============================================================================*/
#ifdef __STDC__
short int
send_file_data(handle_t bind_handle,
			 Custom_File_Handle custom_handle,
		         Pipe_Char_Type ASF_pipe_data,
			 error_status_t *dce_error)
#else
short int
send_file_data(bind_handle, custom_handle, ASF_pipe_data, dce_error)
   handle_t bind_handle ;
   Custom_File_Handle custom_handle ;
   Pipe_Char_Type ASF_pipe_data ;
   error_status_t *dce_error ;
#endif
{
   int             cls; /* integer for closing of the directory */
   int             status, status_rm ;
   int             file_h ;
   idl_char        buf[MAX_BUF_ELEMENTS] ;   /* pipe data buffer          */
   idl_ulong_int   element_count ;           /* number of elements pulled */
   char            logmsg[MAX_SYSLOG_MSGLEN+1] ;     /* syslog msg string */
   char           *rootpath ;
   char           *tempdir ;
   char           *realtempdir ;
   char           *tempfile ;
   char           *slash ;
   char           *sourcefile ;
   DIR            *dirp;

   syslog(LOG_NOTICE,"FAxmitserver called with following parameters;");
   syslog(LOG_NOTICE,"Sourcefile=%s",custom_handle.sourcefile);
   syslog(LOG_NOTICE,"File_type =%s",custom_handle.file_type);
   syslog(LOG_NOTICE,"Dest_id   =%s",custom_handle.dest_id);

   /* Local copy of file transferred from client to server
   -- will be in the temp dir.  Obtain name of temp file,
   -- temp dir.  Note: The rootpath has to be used.
   */

   if ((rootpath = getenv(FAIF_ROOTPATH_EV)) == (char *)NULL)
   {
      syslog(LOG_WARNING, 
	 "ERROR, FAIF rootpath not set.\n");
      return (ERROR);
   }

   if ((tempdir = get_config_val(FAxmitserver_configfile, 
				 FAXMIT_TMPDIR_EV)) == (char *)NULL)
   {
     syslog(LOG_WARNING, 
	 "ERROR, FAxmitserver temp dir not set.\n");
     return(ERROR);
   }
   realtempdir = (char *)util_do_malloc(sizeof(char)*
					(strlen(rootpath) + 
					 strlen(tempdir)+2)) ;
   strcpy(realtempdir, rootpath) ;
   strcat(realtempdir, "/") ;
   strcat(realtempdir, tempdir) ;

   if ((dirp=opendir(realtempdir)) == (DIR *)NULL)
   {
      syslog(LOG_ERR, "ERROR, Unable to access FAxmitserver temp dir %s\n", realtempdir) ;
      free(tempdir) ;
      free(realtempdir) ;
      return(ERROR) ;
   }
   else if (access(realtempdir, W_OK) !=0)
   {
      syslog(LOG_ERR, 
	 "ERROR, No write permission for FAxmitserver temp dir\n") ;
      free(tempdir) ;
      free(realtempdir) ;
      return(ERROR) ;
   }

   /* Use filename portion only of input source pathname
   */
   slash = strrchr((char *)custom_handle.sourcefile, '/') ;
   if (slash != (char *)NULL)
      sourcefile = slash + 1 ;
   else
      sourcefile = (char *)custom_handle.sourcefile ;
   tempfile = (char *)util_do_malloc(sizeof(char)*(strlen(realtempdir)+
			  strlen(sourcefile)+2)) ;
   strcpy(tempfile, realtempdir) ;
   strcat(tempfile, "/") ;
   strcat(tempfile, sourcefile) ;

   /* Open local file on server for write (create local copy of pulled data)
   */
   file_h = open((char *)tempfile, O_CREAT | O_TRUNC | O_WRONLY, 0777) ;

   /* If can't open file, need to discard the pipe data and return ERROR
   */
   if (file_h < 0)      
   {
      file_h = open("/dev/null", O_WRONLY) ;
      syslog(LOG_ERR, "ERROR, Unable to open for write temporary local copy %s\n", tempfile) ;
      free(tempdir) ;
      free(realtempdir) ;
      free(tempfile) ;
      return(ERROR) ;
   }

   while(true)               /* entire pipe must be processed             */
   { 
      (ASF_pipe_data.pull)(  /* pull routine is used for an input pipe    */
         ASF_pipe_data.state,/* state is controlled by the stub           */
         buf,                /* the buffer to be filled                   */
         MAX_BUF_ELEMENTS,   /* maximum number of data elements in buffer */
         &element_count      /* actual number of elements in the buffer   */
      ) ;

      if(element_count == 0) 
	 break ;             /* 0 count signals pipe is empty */

      /* Write obtained data to local file copy
      */
      write(file_h, buf, (sizeof(idl_char)*element_count)) ;
   }

   close(file_h) ;

   /* Send local copy to FA via FTP, FTAM
   */
   if (*(custom_handle.file_type) == NULL)
      status = send_file_to_FA(tempfile, custom_handle.dest_id, (char *)NULL) ;
   else
      status = send_file_to_FA(tempfile, custom_handle.dest_id, 
			       custom_handle.file_type) ;

   if (status != ERROR)
      /* Delete local copy after successful send
      */
      status_rm = remove((char *)tempfile) ;
   else
   {
      syslog(LOG_ERR, "ERROR, failed to send %s\n", tempfile) ;
   }

   cls = closedir(dirp) ;
   free(realtempdir) ;
   free(tempdir) ;
   free(tempfile) ;

   return(status) ;

} /* send_file_data */

/* End of file */

