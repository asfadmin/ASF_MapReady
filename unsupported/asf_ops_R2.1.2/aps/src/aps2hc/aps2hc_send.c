#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		aps2hc_send.c

Description:	This module contains the DCE client initialization routine.
				It includes the procedures performed to obtain binding
				information that will be used to contact the server.

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)aps2hc_send.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/aps2hc/SCCS/s.aps2hc_send.c"

#include <time.h>

#include "aps2hc.h"
#include "aps2hc_status.h"


int
aps2hc_send(CLIENT_INPUTS *client_inputs,char *filetype,char *progName,int hostFlag)
{
	error_status_t		status ;
	int 				result;
	time_t				aps2hc_send_time;
	char				msg[MSG_LEN];

	aps2hc_send_time = time(NULL); 

	if((strcmp(filetype, "AWOS") == 0))
	{
		downlink_ingest_put_wos(
				client_inputs->handle,
				client_inputs->source,
				aps2hc_send_time,
				client_inputs->count,		
				client_inputs->data_file_path,
				&status);
		result = check_aps_status(status,progName,hostFlag);
	}
	else if((strcmp(filetype , "AE1E") ==0) ||
			(strcmp(filetype , "AE2E") ==0) ||
			(strcmp(filetype , "AJ1E") ==0) ||
			(strcmp(filetype , "AA1E") ==0) ||
			(strcmp(filetype , "AR1E") ==0))

	{
		downlink_ingest_put_ephemeris(
				client_inputs->handle,
				client_inputs->source,
				aps2hc_send_time,
				client_inputs->count,           
				client_inputs->data_file_path,
				&status);
		result = check_aps_status(status,progName, hostFlag);
	}
	else
	{
		(void)sprintf(msg,"file type incorrect !!");
		aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
		return(0);
	}
	return(result);

} /* aps2hc_send */
