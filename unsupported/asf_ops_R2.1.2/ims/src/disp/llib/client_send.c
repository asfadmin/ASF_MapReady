static char *sccs = "@(#)client_send.c	5.3  01/03/97";
/*************************************************************************
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
 *************************************************************************/
/*************************************************************************
	Modification: 
					02/27/96     jwang    removed cases of CP_STATUS_MESSAGE, and
					                      CP_SCAN, CP_FRAME which are not utilized
					                      in Order Dispatcher.  
					                      Changed return value to IMS_FATAL, IMS_ERROR
					                      or IMS_OK to make the return code be
					                      consistant with other IMS applications.

					03/07/96     jwang    added DEBUG flags.
 *************************************************************************/

#include <ims_query.h>

#ifdef DCEON
#include <stdio.h>
#include <messages.h>
#include <check_status.h>
#include <ims_ppsDefs.h>
#include <PPSerr.h>

#define NO_RPC -645

#endif 

/*==============================================================================
Function:	int client_send(char *filename, int filetype, 
				char *err_msg, int *pps_ret_code, 
				int kill_flag)
Description:	
Parameters:
Returns:	
Creator:	Nadia Adhami
Creation Date:	Wed Sep  6 17:21:40 PDT 1995
Notes:		
==============================================================================*/

#define	BUF_SIZE	2048

int client_send(char *filename, int filetype, char *err_msg, int *pps_ret_code, int kill_flag)
{
#ifdef DCEON
	FILE	*infp ;
	int  	nread ;
	long	buf_size = c_msg_size;
	long	spec_size = c_msg_size;
  unsigned char 	buffer[c_msg_size];
  unsigned char 	job_spec[c_msg_size];
	error_status_t status = rpc_s_ok ;

	infp = fopen( filename , "r");
	if (! infp)
	{
		sprintf(err_msg,"ERROR in opening input file\n") ;
		return(IMS_FATAL);
	}
	nread = fread(buffer, sizeof(char), sizeof(buffer), infp);
	if (nread == 0)
	{
		sprintf(err_msg,"ERROR in reading input file\n") ;
		fclose(infp);
		return(IMS_FATAL);
	}
	else
		buffer[nread] = '\0';
	*pps_ret_code = NO_RPC;

	switch  (filetype)
	{
	    	case IMS_CANCEL :
#ifdef DEBUG
                	printf("\nCLIENT : call send_IMS_CancelReq_to_PPS(%s)\n",filename);
#endif 
			*pps_ret_code = send_IMS_CancelReq_to_PPS(buffer, nread, &status);
			CHECK_STATUS_MSG(status, "Error in send_IMS_CancelReq_to_PPS\n", RESUME, err_msg);
			break;

	    	case IMS_SCAN :
#ifdef DEBUG
                	printf("\nCLIENT : call send_IMS_ScanReq_to_PPS(%s)\n",filename);
#endif
			*pps_ret_code = send_IMS_ScanReq_to_PPS(buffer, nread, &status);
			CHECK_STATUS_MSG(status, "Error in send_IMS_ScanReq_to_PPS\n", RESUME, err_msg);
			break;

	    	case IMS_L1PR :
#ifdef DEBUG
                	printf("\nCLIENT : call send_IMS_L1PReq_to_PPS(%s)\n",filename);
#endif 
			*pps_ret_code = send_IMS_L1PReq_to_PPS(buffer, nread, &status);
			CHECK_STATUS_MSG(status, "Error in send_IMS_L1PReq_to_PPS\n", RESUME, err_msg);
			break;

	    	case IMS_SV_AVAIL :
#ifdef DEBUG
                	printf("\nCLIENT : call send_IMS_SvAvail_to_PPS(%s)\n",filename);
#endif 
			*pps_ret_code = send_IMS_SvAvail_to_PPS(buffer, nread, &status);
			CHECK_STATUS_MSG(status, "Error in send_IMS_SvAvail_to_PPS\n", RESUME, err_msg);
			break;

		default:
			sprintf(err_msg, "Unknown msg type\n");
			fclose(infp);
			return (IMS_FATAL);
			break;
	}

	if ( status != rpc_s_ok )
	{
		fclose(infp);
		return(IMS_ERROR);
	}

	if (*pps_ret_code == NO_RPC)
		printf("RPC not invoked\n");

	if (*pps_ret_code != ER_NO_ERROR)	
	{
		fclose(infp);
		return (IMS_FATAL);
	}
	
	fclose(infp);
#endif

	return (IMS_OK);

} /* client_send */

/* End of File */
