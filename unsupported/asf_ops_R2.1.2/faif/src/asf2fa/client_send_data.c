/*************************************************************************
 * Copyright (c)1995 California Institute of Technology. U.S. Government *
 * Sponsorship acknowledged.                                             *
 *************************************************************************/

/*==============================================================================
Filename:	client_send_data.c

Description:	
	This module contains the definition of the high level function called 
by user client applications "send_file_from_ASF".  This function in turn 
executes as a DCE RPC client to the send_file_data RPC server.

Internal Function:
        connect_via_cds_binding

External Functions:
	send_file_from_ASF
	
Static Functions:
	None
	
External Variables Defined:
	FA_Names_Table
	
File Scope Static Variables:
	None
	
Notes:

==============================================================================*/

static char SccsFile[] = "client_send_data.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "17 Sep 1996";
static char SccsLastChanger[] = "@(#)client_send_data.c	1.1";
static char SccsState[] = "1.1";

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <syslog.h>
#include <dce/dce_error.h>
#include "faifdefs.h"
#include "In_Pipe_State.h"
#include "sendtoFA.h"
#include "check_status.h"  /* header with the CHECK_STATUS macro */
#include <dce/nbase.h>
#include <dce/rpc.h>

#ifndef FAXMIT_CDS_ENTRY_EV
#define FAXMIT_CDS_ENTRY_EV  "FAXMIT_CDS_ENTRY"
#endif

#ifdef __STDC__
int send_file_from_ASF(char *, char *, char *) ;
#else
int send_file_from_ASF() ;
#endif

#ifdef __STDC__
static int  connect_via_cds_binding (char *, rpc_binding_handle_t *);
#else
static int  connect_via_cds_binding ();
#endif

/* If debug is set to 1, the function connect_via_cds_binding will give
   messages about connecting to dce. */
#define debug 0

/* Table of valid Flight Agency Names that
-- can be passed to the send server function */
extern Names_Table_Entry FA_Names_Table[] ;



/*==============================================================================
Function:       static int connect_via_cds_binding(char *CDS_entry_name, rpc_binding_handle_t *BindHandleRet)

Description:
         Checks for open bindings and then connects to the open one.
It initializes the client by connecting to the server.

Parameters:

Returns:

Creator: Samah Sohrab
Creation Date: 03/20/1997
Notes:
==============================================================================*/

#ifdef __STDC__
static int  connect_via_cds_binding (char *CDS_entry_name, rpc_binding_handle_t *BindHandleRet)
#else
static int  connect_via_cds_binding (CDS_entry_name, BindHandleRet)
   char *CDS_entry_name ;
   rpc_binding_handle_t *BindHandleRet ;
#endif
{
   int                      error = 0;
   rpc_binding_handle_t     bind_handle;
   rpc_ns_handle_t          import_context;
   unsigned32               status;
   boolean32                ret;
   unsigned_char_t         *string;

   if (debug) printf("connect_via_cds_binding(%s)\n", CDS_entry_name);

   /* connect to server */

   rpc_ns_binding_import_begin(
              rpc_c_ns_syntax_dce,
              (unsigned_char_t *)CDS_entry_name,
              sendtoFA_v1_0_c_ifspec,
              NULL,
              &import_context,
              &status);

   if (status != rpc_s_ok)
   {
      return 0;
   }

   rpc_ns_mgmt_handle_set_exp_age(
              import_context,
              (unsigned32)0,
              &status);

   if (status != rpc_s_ok)
   {
      return 0;
   }

   for(;;)
   {
      rpc_ns_binding_import_next(
              import_context,
              &bind_handle,
              &status);

      if (status != rpc_s_ok)
      {
         error = 1;
         break;
      }

      if (debug)
      {
         rpc_binding_to_string_binding(bind_handle,&string,&status);
         printf("Client got a binding:\n%s\n",string);
         rpc_string_free(&string,&status);
      }

      rpc_ep_resolve_binding(
              bind_handle,
              sendtoFA_v1_0_c_ifspec,
              &status);

      if (status != rpc_s_ok)
      {
         if (debug)
	 {
            int  error_status;
            char error_string[dce_c_error_string_len];
            printf("rpc_ep_resolve_binding() - failed\n");
            printf("%s\n",error_string);
	 }

         continue;
      }

      if (debug)
      {
         rpc_binding_to_string_binding(bind_handle,&string,&status);
         printf("Resolved binding:\n%s\n",string);
         rpc_string_free(&string,&status);
      }

      if (debug) printf("Pinging server\n");

      ret = rpc_mgmt_is_server_listening(bind_handle,&status);

      if (ret) break;

      if (debug) printf("Server ping failed\n");

      rpc_binding_free(&bind_handle,&status);

      if (status != rpc_s_ok)
      {
         error = 1;
         break;
      }
   }

   rpc_ns_binding_import_done(
              &import_context,
              &status);

   if (status != rpc_s_ok)
   {
      error = 1;
   }

   if (error) return 1;

   /* printf("Found active server via CDS\n"); */

   /* return binding handle */
   *BindHandleRet = bind_handle;

   return 1;
}



/*==============================================================================
Function:	int send_file_from_ASF(char *ASF_file, char *FA, 
				       char *file_type)

Description:
	This is the ASF to FA send file DCE client.  This represents
the component associated with the client side of the send file to FA
operation.  This is the user level function called to execute a file
send that activates the DCE data send RPC.  What needs to be 
specified are the client file ASF_file, the FA "ESA", "CSA", or 
"NASDA" and the FA file type.  This function takes care of the DCE
client initialization as well as the DCE RPC to the DCE remote 
procedure send_file_data.  In case an error status is returned by 
called functions causing the failure of data transfer from client to 
server, syslog is invoked to report the error and an error status is 
returned to the calling function.  A separate function 
report_dce_err_status is used to report the appropriate DCE comm or 
fault status message in case the error status parameter of the RPC 
specifies an error.

Parameters:
	ASF_file - File path of ASF file to be sent out to a flight
agency.  This is the client file to be sent out to the server side.
	   
        FA -  Flight agency code denoting which flight agency the ASF
file is to be sent.

	file_type - file type string corresponding to the type of the
file to send

Returns:	
	status returned by send_data RPC
	or
	ERROR - bad input, send failure

Creator:	Norbert Piega	
Creation Date:	06/13/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
send_file_from_ASF(char *ASF_file, char *FA, char *file_type)
#else
int
send_file_from_ASF(ASF_file, FA, file_type)
   char *ASF_file ;
   char *FA ;
   char *file_type ;
#endif
{
   int status ;
   int index ;
   handle_t bind_handle ;
   Custom_File_Handle custom_handle ;      /* customized binding handle  */
   In_Pipe_State state;
   Pipe_Char_Type  ASF_pipe_data;       /* a pipe structure is allocated */
   void client_alloc(), pipe_data_pull();
   error_status_t err_status = rpc_s_ok ;
   char *CDS_entry_name ; 
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
 
   if (ASF_file == (char *)NULL || FA == (char *)NULL)
   {
      syslog(LOG_ERR, "ERROR, NULL input(s) to send_file_from_ASF server\n") ;
      return(ERROR) ;
   }

   /* Check if file to send is accessible
   */
   if (access(ASF_file, F_OK) != 0 || access(ASF_file, R_OK) != 0)
   {
      syslog(LOG_ERR, "ERROR, Unable to access file %s in send_file_from_ASF\n", ASF_file) ;
      return(ERROR) ;
   }

   /* Check FA id passed
   */
   for (index=0; FA_Names_Table[index].name_id != SENTINEL; index++)
      if (strcmp(FA, FA_Names_Table[index].name_identifier) == 0)
	 break ;
   if (FA_Names_Table[index].name_id == SENTINEL)
   {
      syslog(LOG_ERR, "ERROR, Invalid FA specified in send_file_from_ASF\n") ;
      return(ERROR) ;
   }

   /* Obtain server's CDS entry name to be used in
   */
   CDS_entry_name = getenv(FAXMIT_CDS_ENTRY_EV) ;
   if (CDS_entry_name == (char *)NULL)
   {
      syslog(LOG_ERR, "ERROR, Unable to obtain server's CDS entry name from environment.\nThe environment variable %s must be set.\n", FAXMIT_CDS_ENTRY_EV) ;
      return(ERROR) ;
   }

   /* Get client DCE binding handle via function connect_via_cds_binding
   */
   if ((status = connect_via_cds_binding(CDS_entry_name, (rpc_binding_handle_t *) &bind_handle)) == 0)
   {
      syslog(LOG_ERR, "ERROR, Unable to obtain binding handle for server\n") ;
      return(ERROR) ;
   }

   /* Initialize customized binding handle structure 
   */
   strcpy((char *)custom_handle.dest_id, FA) ;
   if (file_type != (char *)NULL)
      strcpy((char *)custom_handle.file_type, file_type) ;
   else
      *custom_handle.file_type = NULL ;
   strcpy((char *)custom_handle.sourcefile, ASF_file) ;
   
   /* Initialize pipe structure 
   */
   state.filehandle = -1 ;
   state.filename = ASF_file ;

   /* Initialize pipe state 
   -- Initialize alloc procedure for a pipe 
   -- Initialize pull procedure for input pipe 
   */
   ASF_pipe_data.state = (rpc_ss_pipe_state_t)&state;
   ASF_pipe_data.alloc = client_alloc;     
   ASF_pipe_data.pull = pipe_data_pull; 

   /* RPC! The Remote Procedure Call 
   */
   err_status = rpc_s_ok ;
   status = (int) send_file_data(bind_handle, custom_handle, 
			   ASF_pipe_data, &err_status) ;
   CHECK_STATUS(err_status, "send_file_data RPC", RESUME) ;
   if (status == ERROR || err_status != rpc_s_ok) 
   {
      syslog(LOG_ERR, "ERROR, Error sending %s\n", ASF_file) ;
      return(ERROR) ;
   }    

   return(status) ;

} /* send_file_from_ASF */


/* End of file */
