#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		aps2hc_status.c

Description:	This module contains the DCE client status routines.  

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)aps2hc_status.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/aps2hc/SCCS/s.aps2hc_status.c"

#include "aps2hc.h"
#include "aps2hc_status.h"



/*==============================================================================
Function:       check_aps_status

Description:    

Parameters:     

Returns:         1	- if rpc status is ok
				-1	- if server is ok, but transaction had errors
				 0	- server errors occurred during transaction

Creator:        Your_Name

Creation Date:  03/15/96

Notes:		
==============================================================================*/
/*************************************************************************
*
*       check_aps_status()
*
*
**************************************************************************/

int
check_aps_status(error_status_t input_status, char * progName, int hostFlag)
{
	int		result = -1 ;
	char	msg[MSG_LEN];

	sprintf (msg, "Host Controller (HC) return status = %d", input_status);
	aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);

	switch(input_status)
	{
		case rpc_s_ok :
		{
			(void)sprintf(msg,"Program completed successfully!");
			aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
			result = 1 ;
			break;
		}
		case hst_s_syntax :
		{
			(void)sprintf(msg,"The file has a syntax problem");
			aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
			aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
			(void)sprintf(msg, "Program terminated abnormally.");
			aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
			break;
		}
		case hst_s_semantic :
		{
			(void)sprintf(msg,"The file has a semantic problem");
			aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
			aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
			(void)sprintf(msg, "Program terminated abnormally.");
			aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
			break;
		}
		case hst_s_misscomp :
		{
			(void)sprintf(msg,"The file is missing a mandatory component");
			aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
			aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
			(void)sprintf(msg, "Program terminated abnormally.");
			aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
			break;
		}
		case hst_s_data_transfer_failed:
		{
			(void)sprintf(msg,"The ftp (file transfer) failed");
			aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
			aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
			(void)sprintf(msg, "Program terminated abnormally.");
			aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
			break;
		}
		default :
		{
			check_rpc_status(input_status, ABORT, progName, hostFlag);
			result = 0;
			break;
		}
	} /* switch */

	return(result);

} /* check_aps_status */ 


/*==============================================================================
Function:       check_init_status

Description:    

Parameters:     

Returns:        1 if can continue with current host
				0 if need to try next host

Creator:        Teresa McKillop

Creation Date:  5/22/96

Notes:		
==============================================================================*/
int
check_init_status(	error_status_t	input_status,
					int				action,
					char *			progName,
					int				hostFlag )
{
	int		result = 0 ;	/* default to "error" */

	/* might ABORT the program: if doesn't, set result based on input_status */
	check_rpc_status( input_status, action, progName, hostFlag ) ;

	if (input_status == rpc_s_ok)
		result = 1 ;

	return (result) ;
}


/*==============================================================================
Function:       check_rpc_status()

Description:    

Parameters:     hostFlag	set to 1 if working with main DCE server
							set to 0 if working with the backup

Returns:        

Creator:        Unknown

Creation Date:  early 1996

Notes:		
==============================================================================*/
void
check_rpc_status(	error_status_t	input_status,
					int				action,
					char *			progName,
					int				hostFlag)
{
   	static int				error_stat;
   	static unsigned char	error_string[dce_c_error_string_len];
    char					msg[MSG_LEN];
 
   	if(input_status != rpc_s_ok)
   	{
      	dce_error_inq_text(input_status, error_string, &error_stat);
        if (hostFlag)
        { 	/* If main DCE server failed, ... */
            (void) sprintf( msg, "HC main DCE server: %s", error_string ) ;
        }
        else
        {
            (void) sprintf( msg, "HC backup DCE server: %s", error_string ) ;
            aps_log_msg(progName, APS_ERROR, msg, DO_SYSLOG, DO_PRINT);
        }
		aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);

      	if(action == ABORT)
		{
			if (hostFlag)
			{
				(void) sprintf(msg, "HC main DCE server not in service, "
									"connecting backup server....");
			}
			else
			{
				(void)sprintf(msg, "Program terminated abnormally.");
			}
			aps_log_msg(progName, APS_INFO, msg, DO_SYSLOG, DO_PRINT);
    /*    	exit(APS_EXIT_ERROR);*/
		}
   	}
}/* check_rpc_status */
 
