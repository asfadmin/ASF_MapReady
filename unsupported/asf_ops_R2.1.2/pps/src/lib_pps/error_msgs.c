/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:       error_msg.c
 
Description:
        This module contains the error_msgs returned by the RPC calls used
between PPS server and its clients.
 
External Functions:
	print_pps_err_msg
	get_pps_err_msg

Static Functions:
	None
 
External Variables Defined:
	pps_err_msgs
 
File Scope Static Variables:
	None
 
Notes:
==============================================================================*/

#include <stdio.h>
#include "messages.h"
#include "PPSdefs.h"

static char SccsFileId[] = "@(#)error_msgs.c	1.3    10/31/97";

const char *
pps_err_msgs[] = 
{ 
	"No error",
	"NULL input to the function",
	"ERROR in parsing IMS Message",
	"Msg is not a CANCEL Request",
	"Msg is not a DUB Request",
	"Msg is not a L1 Product Request",
	"Msg is not a SCAN Request",
	"Msg is not a SV Available Message",
	"Error in processing CANCEL Request",
	"Error in processing DUB Request",
	"Error in processing L1 Product Request",
	"Error in processing SCAN Request",
	"Error in processing SV Available Message",
	"Error in parsing CP Message",
	"Unknown CP Job type",
	"Error in processing CP Request",
	"Msg is not a CP Status Message",           /* ER_NOT_CP_STATUS_MSG */
	"No database login available at this time",
	"PPS Server is shutting down - have to reject rpc",
	"IMS GHA Query failed",
	"IMS GHA Query returned no data",
	"IMS TCE Query failed",
	"IMS TCE Query returned no data",
	"IMS State Vector Query failed",
	"IMS State Vector Query returned no data",
	"Can not insert record into 'Scan Orders' Table",
	"Can not insert record into 'Jobs' Table",
	"Jobid not found in the 'Jobs' Table",
	"Malloc error in IMS function",
	"Malloc error in PPS function",
	"Database login failed (check userid,password,dbname,servername)",
	"Can not insert record into 'L1 Orders' Table",
	"Can not insert record into 'L1 Proc Parms' Table",
	"Can not send order status message to IMS",
	"IMS Cal Params Query failed",
	"IMS Cal Params Query returned no data",
	"IMS Scan Results Query failed",
	"IMS Scan Results Query returned no data",
	"Can not execute stored procedure get_next_job_id()",
	"Can not cancel job, it is already submitted",
	"Can not find scan order",
	"Can not find L1 order",
	"IMS can't open connection to IMS database server",
	"Can not make SCAN job available for processing",
	"Can not make L1 job available for processing",
	"Can not make DUB job available for processing",
	"No Scan Job with scansar mode is available for processing",
	"No Scan Job with continuous mode is available for processing",
	"No Dub Job is available for processing",
	"Error in parsing common header",
	"Can not allocate memory for common header structure",
	"Can not extract common header ",
	"Unknown IMS Message type",
	"ODL function: ReadLabel_buf() returned error",
	"Job not found in the Jobs Table",
	"Job not found in the L1 Proc Params Table",
	"Can not update Jobs Table",
	"IMS Frame Query failed",
	"IMS Frame Query returned no data",
	"Can not update L1 ProcParms Table",
	"Can not update job state ",
	"Can not update L1 record using matching scan ",
	"Can not remove completed/canceled scan order",
	"Can not remove completed/canceled L1 order",
	"Can not remove L1 order",
	"Buffer was not big enough to hold the job",
	"Can not update Scan Orders Table",
	"No Frame Job with scansar mode is available for processing",
	"No Frame Job with continuous mode is available for processing",
	"Failed to convert to/from ODL time", 
	"Keyword Value is too long",
	"Invalid Keyword Value Type",
	"Keyword not found",
	"Insufficient media info to perform IMS queries",
	"Failed to access data from the PPS database",
	"The job is now READY",
	"The job is now AVAILABLE (PLANNED)",
	"The job is now PENDING" ,
	"The job is still PENDING",
	"The job is still READY",
	"Failed to complete request due to deadlock",
	"FATAL database error occured - Please EXIT",
	"Failed to complete request - please try again",
	"Errors in config file" ,
	"Order not found in the IMS database",
	"The frame for the selected L1 request has status of MISSED",
	"The frame for the selected L1 request has status of REJECTED"
};

void print_pps_err_msg(int) ;

/*==============================================================================
Function:	void print_pps_err_msg(int errcode)
Description:	Print PPS error message	
Parameters:	PPS error code corresponding to a specific error string
Returns:	None	
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 14:31:49 PDT 1995
Notes:		
==============================================================================*/
void print_pps_err_msg(int errcode)
{
	fprintf(stderr, pps_err_msgs[errcode]);
}

/*==============================================================================
Function:	get_pps_err_msg(int errcode)
Description:	get PPS error message string
Parameters:	PPS error code corresponding to a specific error string
Returns:	constant error message string
Notes:		
==============================================================================*/
const char*
get_pps_err_msg(int errcode)
{
	return(pps_err_msgs[errcode]);

} /* get_pps_err_msg */
