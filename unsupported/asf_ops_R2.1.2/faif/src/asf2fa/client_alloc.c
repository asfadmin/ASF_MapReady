/*************************************************************************
 * Copyright (c)1995 California Institute of Technology. U.S. Government *
 * Sponsorship acknowledged.                                             *
 *************************************************************************/

/*==============================================================================
Filename:	client_alloc.c

Description:	
	This module contains the function which performs the assignment of 
the client buffer to be used by the pipe between DCE client and server.

External Functions:
	client_alloc
        pipe_data_pull

Static Functions:
	None

External Variables Defined:
	client_buffer

File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFile[] = "client_alloc.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "17 Sep 1996";
static char SccsLastChanger[] = "@(#)client_alloc.c	1.1";
static char SccsState[] = "1.1";

#include <stdio.h>
#include "sendtoFA.h"
#include "In_Pipe_State.h"

/*pipe_data_pull*/
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <syslog.h>
#include "faifdefs.h"

#ifdef __STDC__
void client_alloc(In_Pipe_State *, idl_ulong_int, idl_char **, idl_ulong_int *);
#else
void client_alloc() ;
#endif

#define BUFFER_SIZE 2048

/* Client pipe buffer
*/
idl_char client_buffer[BUFFER_SIZE];


/*==============================================================================
Function:	void client_alloc() 

Description:	
	This function assigns the client buffer and buffer size for the
the buffer to be used by the DCE pipe between the DCE client and server.

Parameters:
	state - coordinates pipe procedure calls; currently not used
	bsize - desired size of buffer in bytes
	buf - pointer to the pipe buffer; the allocated buffer 
	bcount - allocated buffer size in bytes

Returns:	None	
Creator:	Norbert Piega	
Creation Date:	06/13/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
void
client_alloc(In_Pipe_State *state, idl_ulong_int  bsize,
	     idl_char **buf, idl_ulong_int *bcount)
#else
void
client_alloc(state, bsize, buf, bcount)
   In_Pipe_State   *state;
   idl_ulong_int    bsize;
   idl_char       **buf;
   idl_ulong_int   *bcount;
#endif
{
    *buf = client_buffer;
    *bcount = BUFFER_SIZE;

    return;

} /* client_alloc */


/* Definition of a state structure for pipe data */
#include "In_Pipe_State.h"

#ifdef __STDC__
int pipe_data_pull(In_Pipe_State *, idl_char *, idl_ulong_int, idl_ulong_int *);
#else
int pipe_data_pull() ; 
#endif

/*==============================================================================
Function:	pipe_data_pull()

Description:
	Pipe data pull routine opens the client file represented by
pipe_state->filehandle, reads the file contents into a client allocated
buffer pull_buffer which will be transferred through the pipe to the
server, then closes the client file.  Note that the data_count is
passed back after the actual number of items pulled from the pipe is
known.  Syslog is invoked by this function if the file I/O operations
fail; the errors are logged and an error status is returned by this
function.

Parameters:
	pipe_state - coordinates pipe procedure calls
	pull_buffer - buffer of data pulled
	max_count - maximum element count in buffer
	data_count - actual element count in buffer

Returns:
	OK - No errors
	ERROR - file I/O error encountered

Creator:        Norbert Piega 
Creation Date: 06/13/1994 
Notes:
==============================================================================*/
#ifdef __STDC__ 
int 
pipe_data_pull(In_Pipe_State *pipe_state, idl_char *pull_buffer,
		    idl_ulong_int max_count, idl_ulong_int *data_count)
#else
int 
pipe_data_pull(pipe_state, pull_buffer, max_count, data_count)
   In_Pipe_State *pipe_state ;
   idl_char      *pull_buffer ;
   idl_ulong_int  max_count ;
   idl_ulong_int *data_count;
#endif
{
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   /* For this application, open local source file if not open already 
   */
   if (pipe_state->filehandle == -1) 
   { 
      pipe_state->filehandle = open(pipe_state->filename, O_RDONLY) ;
      if(pipe_state->filehandle == -1) 
      {
	 syslog(LOG_ERR, "ERROR, Cannot open file %s for reading\n", pipe_state->filename);
         return(ERROR);
      }
   }

   /* Process buffer for your application
   */
   *data_count = 
      (idl_ulong_int) read(pipe_state->filehandle, pull_buffer, 
	(idl_ulong_int) max_count); 
   if (*data_count < 0)
   {
      syslog(LOG_ERR, "ERROR, Pipe read error encountered.\n") ;
      return(ERROR) ;
   }

   /* To signal the end of data, pull procedure must set the count to 0.  
   */
   if (*data_count == 0) 
   {      
      /* End of data reached, do application cleanup 
      */
      close(pipe_state->filehandle);  
   }

   return(OK) ;

} /* pipe_data_pull */

/* End of File */
