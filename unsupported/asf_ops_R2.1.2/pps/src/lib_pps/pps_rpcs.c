/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:       pps_rpcs.c

Description:
        This module contains the remote procedure calls used to communicate
	between PPS server and its clients.

External Functions:
	send_IMS_CancelReq_to_PPS
	send_IMS_L1PReq_to_PPS
	send_IMS_ScanReq_to_PPS
	send_IMS_SvAvail_to_PPS
 
	send_status_to_PPS
	recv_job_from_PPS

Static Functions:
	None

External Variables Defined:
	None

File Scope Static Variables:
	None

Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)pps_rpcs.c	1.3    04/23/97";

#include <stdio.h>

#include "ppscount.h"
#include "ppsmx.h"
#include "messages.h"
#include "defs.h"
#include "PPSdefs.h"
#include "PPShdr.h"
#include "PPSerr.h"
#include "PPSextern.h"
#include "db_sybint.h"

extern MX_VAR 	g_dying, g_numrpcs;
extern char	*pps_err_msgs[];
extern DB_Proc  *g_PPS_dbproc_table;



short
send_IMS_CancelReq_to_PPS(
unsigned char    msg[MSG_SIZE],
long             size, 
error_status_t   *status)
{
	Common_Header_Record 	*common_hdr = NULL;
	IMS_CancelReq_Record 	*rec = NULL;
	int 			msg_type;
	int			table_index;
	int			ret_code;


#ifdef ODL_DEBUG
	fprintf(stdout, "IMS -> PPS (CancelReq):\n");
	fprintf(stdout, (char *) msg);
#endif

        /* shutdown logic */
        if (test_mx_var(&g_dying)) {
                fprintf(stderr, "PPS rpc rejected - shutting down\n");
                return(ER_SHUTDOWN);
        }
        inc_mx_var(&g_numrpcs);

	/* maintain statistics */
	pps_count(PPS_CANCEL_ORDER);

	if ( !msg || (size == 0) )
	{
		pps_logMsg(ProgName, PPS_ERROR,
		               "Input IMS message is null.") ;
		dec_mx_var(&g_numrpcs);
		return(ER_INPUT_NULL) ;
	}

	if (ingest_IMS_msg_mx("IMS_message", msg, &msg_type,
                                      &common_hdr, &rec) != OK)
	{
		dec_mx_var(&g_numrpcs);
                if (rec)
                        free (rec);
                if (common_hdr)
                        free (common_hdr);
		return(ER_PARSE_IMS);
	}
	if (msg_type != IMS_CANCEL)
	{
		if (rec)
			free (rec);
	        if (common_hdr)
       		        free (common_hdr);

		pps_logMsg(ProgName, PPS_ERROR,
 		               "IMS message is not a Cancel Request.");
		dec_mx_var(&g_numrpcs);
		return(ER_NOT_CANCEL);
	}
	/* get a dbproc for the server and the commit service */
        if ((get_PPS_dbproc(&table_index)) != OK || (table_index == -1))
	{
		if (rec)
			free (rec);
	        if (common_hdr)
       		        free (common_hdr);

		pps_logMsg(ProgName, PPS_CRITICAL,
                               "Unable to get a database login.");
		dec_mx_var(&g_numrpcs);
                return(ER_NO_LOGIN_AVAIL);
        }
	/* process the scan request */
	ret_code = process_IMS_CancelReq(
		g_PPS_dbproc_table[table_index].dbproc_server, 
		g_PPS_dbproc_table[table_index].dbproc_commit, 
		&(g_PPS_dbproc_table[table_index].ims_aux_query),
		rec);

	/* free the database connections */
        free_PPS_dbproc (table_index);

	/* decrement number of active rpcs */
	dec_mx_var (&g_numrpcs);

	if (rec)
		free (rec);
        if (common_hdr)
                free (common_hdr);

	fflush(stdout); fflush(stderr);
	*status = 0;
	return(ret_code);

} /* send_IMS_CancelReq_to_PPS */

short 
send_IMS_ScanReq_to_PPS(
unsigned char    msg[MSG_SIZE], 
long             size, 
error_status_t   *status)
{
	int 			msg_type;
	int 			table_index;
	int			ret_code;
	IMS_ScanReq_Record 	*rec = NULL;
	Common_Header_Record 	*common_hdr = NULL;

#ifdef ODL_DEBUG
        fprintf(stdout, "IMS -> PPS (ScanReq):\n");
        fprintf(stdout, (char *) msg);
#endif

        /* shutdown logic */
	/* if the server is shutting down, *
	 * reject incoming rpcs            */
        if (test_mx_var(&g_dying)) {
                fprintf(stderr, "PPS rpc rejected - shutting down\n");
                return(ER_SHUTDOWN);
        }

	/* increment the number of active rpcs */
        inc_mx_var(&g_numrpcs);

	/* maintain statistics */
	pps_count(PPS_SCAN_ORDER);

	if ( !msg || (size == 0) )
	{
		pps_logMsg(ProgName, PPS_ERROR,
		              "Input IMS message is null.") ;
		dec_mx_var(&g_numrpcs);
		return(ER_INPUT_NULL) ;
	}

	/* parse the scan request message */
	if (ingest_IMS_msg_mx("IMS_message", msg, &msg_type, &common_hdr, &rec) != OK)
	{
		if (rec)
			free (rec);
	        if (common_hdr)
       		        free (common_hdr);

		dec_mx_var(&g_numrpcs);
		return(ER_PARSE_IMS);
	}
	if (msg_type != IMS_SCAN)
	{
		if (rec)
			free (rec);
	        if (common_hdr)
       		        free (common_hdr);

		pps_logMsg(ProgName, PPS_ERROR,
		              "IMS message is not a Scan Request.");
		dec_mx_var(&g_numrpcs);
		return(ER_NOT_SCAN);
	}

	/* get a dbproc for the server and the commit service */
        if ((get_PPS_dbproc(&table_index)) != OK || (table_index == -1))
	{
		if (rec)
			free (rec);
	        if (common_hdr)
       		        free (common_hdr);

		pps_logMsg(ProgName, PPS_ERROR,
                              "Unable to get a database login");
		dec_mx_var(&g_numrpcs);
                return(ER_NO_LOGIN_AVAIL);
        }
	/* process the scan request */
	ret_code = process_IMS_ScanReq(
		g_PPS_dbproc_table[table_index].dbproc_server, 
		g_PPS_dbproc_table[table_index].dbproc_commit, 
		&(g_PPS_dbproc_table[table_index].ims_aux_query),
		rec);

	/* free the database connections */
        free_PPS_dbproc (table_index);

	/* decrement number of active rpcs */
	dec_mx_var (&g_numrpcs);

	if (rec)
		free (rec);
        if (common_hdr)
                free (common_hdr);

	fflush(stdout); fflush(stderr);
	*status = 0;
	return(ret_code);

} /* send_IMS_ScanReq_to_PPS */

short 
send_IMS_L1PReq_to_PPS(
unsigned char     msg[MSG_SIZE], 
long              size, 
error_status_t    *status)
{
	int 			msg_type;
	int			table_index;
	int			ret_code;
	IMS_L1PReq_Record 	*rec = NULL;
	Common_Header_Record 	*common_hdr = NULL;

#ifdef ODL_DEBUG
        fprintf(stdout, "IMS -> PPS (L1Req):\n");
        fprintf(stdout, (char *) msg);
#endif

        /* shutdown logic */
        if (test_mx_var(&g_dying)) {
                fprintf(stderr, "PPS rpc rejected - shutting down\n");
                return(ER_SHUTDOWN);
        }
        inc_mx_var(&g_numrpcs);

	/* maintain statistics */
	pps_count(PPS_L1_ORDER);

	if ( !msg || (size == 0) )
	{
		pps_logMsg(ProgName, PPS_ERROR,
		               "Input IMS message is null.") ;
		dec_mx_var(&g_numrpcs);
		return(ER_INPUT_NULL) ;
	}

	if (ingest_IMS_msg_mx("IMS_message", msg, &msg_type, &common_hdr, &rec) != OK)
	{
		dec_mx_var(&g_numrpcs);
                if (rec)
                        free (rec);
                if (common_hdr)
                        free (common_hdr);
		return(ER_PARSE_IMS);
	}
	if (msg_type != IMS_L1PR)
	{
		pps_logMsg(ProgName, PPS_ERROR,
		               "IMS message is not a L1 Product Request.");
		dec_mx_var(&g_numrpcs);
                if (rec)
                        free (rec);
                if (common_hdr)
                        free (common_hdr);
		return(ER_NOT_L1PR);
	}
	/* get a dbproc for the server and the commit service */
        if ((get_PPS_dbproc(&table_index)) != OK || (table_index == -1))
	{
		if (rec)
			free (rec);
	        if (common_hdr)
       		        free (common_hdr);

		pps_logMsg(ProgName, PPS_ERROR,
                                "Unable to get a database login.");
		dec_mx_var(&g_numrpcs);
                return(ER_NO_LOGIN_AVAIL);
        }
	/* process the scan request */
	ret_code = process_IMS_L1PReq(
		g_PPS_dbproc_table[table_index].dbproc_server, 
		g_PPS_dbproc_table[table_index].dbproc_commit, 
		&(g_PPS_dbproc_table[table_index].ims_aux_query),
		rec);

	/* free the database connections */
        free_PPS_dbproc (table_index);

	/* decrement number of active rpcs */
	dec_mx_var (&g_numrpcs);

	if (rec)
		free (rec);
        if (common_hdr)
                free (common_hdr);

	fflush(stdout); fflush(stderr);
	*status = 0;
	return(ret_code);

} /* send_IMS_L1PReq_to_PPS */

short 
send_IMS_SvAvail_to_PPS(
unsigned char    msg[MSG_SIZE],
long             size, 
error_status_t   *status)
{
	int			table_index;
	int			ret_code;
	Common_Header_Record    *common_hdr = NULL;
	IMS_SVecAvail_Record 	*rec = NULL;
	int msg_type;

#ifdef ODL_DEBUG
        fprintf(stdout, "IMS -> PPS (SvAvail):\n");
        fprintf(stdout, (char *) msg);
#endif

        /* shutdown logic */
        if (test_mx_var(&g_dying)) {
                fprintf(stderr, "PPS rpc rejected - shutting down\n");
                return(ER_SHUTDOWN);
        }
        inc_mx_var(&g_numrpcs);

	/* maintain statistics */
	pps_count(PPS_SVEC_AVAIL);

	if ( !msg || (size == 0) )
	{
		pps_logMsg(ProgName, PPS_ERROR,
                                  "Input IMS message is null.") ;
		dec_mx_var(&g_numrpcs);
		return(ER_INPUT_NULL) ;
	}

	if (ingest_IMS_msg_mx("IMS_message", msg, &msg_type, &common_hdr, &rec) != OK)
	{
		dec_mx_var(&g_numrpcs);
                if (rec)
                        free (rec);
                if (common_hdr)
                        free (common_hdr);

		return(ER_PARSE_IMS);
	}
	if (msg_type != IMS_SV_AVAIL)
	{
		pps_logMsg(ProgName, PPS_ERROR,
		    "IMS message is not a State Vector Avail message");
		dec_mx_var(&g_numrpcs);
                if (rec)
                        free (rec);
                if (common_hdr)
                        free (common_hdr);
		return(ER_NOT_SVAVAIL);
	}
        /* get a dbproc for the server and the commit service */
        if ((get_PPS_dbproc(&table_index)) != OK || (table_index == -1))
        {
		if (rec)
			free (rec);
	        if (common_hdr)
       		        free (common_hdr);

		pps_logMsg(ProgName, PPS_ERROR,
                                  "Unable to get a database login");
                dec_mx_var(&g_numrpcs);
                return(ER_NO_LOGIN_AVAIL);
        }
        /* process the scan request */
        ret_code = process_IMS_SvAvail(
                g_PPS_dbproc_table[table_index].dbproc_server,
                g_PPS_dbproc_table[table_index].dbproc_commit,
                &(g_PPS_dbproc_table[table_index].ims_aux_query),
                rec);
 
        /* free the database connections */
        free_PPS_dbproc (table_index);

	/* decrement number of active rpcs */
	dec_mx_var(&g_numrpcs);

	
	if (rec)
		free (rec);
        if (common_hdr)
                free (common_hdr);

	fflush(stdout); fflush(stderr);
	*status = 0;
	return(ret_code);

} /* send_IMS_SvAvail_to_PPS */


short 
send_status_to_PPS(
unsigned char    msg[MSG_SIZE],
long             size, 
error_status_t   *status)
{
	Common_Header_Record    *common_hdr = NULL;
	CP_JobStatus_Record 	*rec = NULL;
	int 			msg_type;
	int			table_index;
	int			ret_code;

#ifdef ODL_DEBUG
        fprintf(stdout, "CP -> PPS (Status):\n");
        fprintf(stdout, (char *) msg);
#endif

        /* shutdown logic */
        if (test_mx_var(&g_dying)) {
                fprintf(stderr, "PPS rpc rejected - shutting down\n");
                return(ER_SHUTDOWN);
        }
        inc_mx_var(&g_numrpcs);

	if ( !msg || (size == 0) )
	{
		pps_logMsg(ProgName, PPS_ERROR,
		                "Input CP message is null.") ;
		dec_mx_var(&g_numrpcs);
		return(ER_INPUT_NULL) ;
	}

	if (ingest_CP_msg_mx("CP_message", msg, &msg_type,
			&common_hdr, &rec) != OK)
	{
		dec_mx_var(&g_numrpcs);
                if (rec)
                        free (rec);
                if (common_hdr)
                        free (common_hdr);

		return(ER_PARSE_CP);
	}
	if (msg_type != CP_STATUS_MESSAGE)
	{
		pps_logMsg(ProgName, PPS_ERROR,
       		                 "CP message is not a CP Status Message.");
		dec_mx_var(&g_numrpcs);
                if (rec)
                        free (rec);
                if (common_hdr)
                        free (common_hdr);

		return(ER_NOT_CP_STATUS_MSG);
	}

        /* Get a dbproc for the server and the commit service */
        if ((get_PPS_dbproc(&table_index)) != OK || (table_index == -1))
        {
		if (rec)
			free (rec);
	        if (common_hdr)
       		        free (common_hdr);

		pps_logMsg(ProgName, PPS_ERROR,
                              "Unable to get a database login");
                dec_mx_var(&g_numrpcs);
                return(ER_NO_LOGIN_AVAIL);
        }

	/* If Scan job status */
	if (strcmp(rec->request_type, SCAN_KEYWD) == 0)
	{
	   /* If Scan complete */
	   if (strcmp(rec->status, COMPLETED) == 0)
	   {
           	ret_code = process_CP_ScanComplete(
                	g_PPS_dbproc_table[table_index].dbproc_server,
                	g_PPS_dbproc_table[table_index].dbproc_commit,
                	&(g_PPS_dbproc_table[table_index].ims_aux_query),
                	rec);
	   }
	   else if (strcmp(rec->status, CANCELFAIL) == 0)
	   /* else if Scan Failed */
	   {
           	ret_code = process_CP_ScanCancel(
                	g_PPS_dbproc_table[table_index].dbproc_server,
                	g_PPS_dbproc_table[table_index].dbproc_commit,
                	&(g_PPS_dbproc_table[table_index].ims_aux_query),
                	rec);
	   }
	   else if (strcmp(rec->status, RETRY) == 0)
	   {
		ret_code = process_CP_ScanRetry(
                	g_PPS_dbproc_table[table_index].dbproc_server,
                	g_PPS_dbproc_table[table_index].dbproc_commit,
                	&(g_PPS_dbproc_table[table_index].ims_aux_query),
                	rec);
	   }
	}
	else if (strcmp(rec->request_type, FRAME_KEYWD) == 0)
	/* else if Frame job status */
	{
	   if (strcmp(rec->status, RETRY) == 0)
	   {
		ret_code = process_CP_FrameRetry(
                	g_PPS_dbproc_table[table_index].dbproc_server,
                	g_PPS_dbproc_table[table_index].dbproc_commit,
                	&(g_PPS_dbproc_table[table_index].ims_aux_query),
                	rec);
	   }
	   else
	   {
		/* If Frame Completed or If Frame Failed */
		ret_code = process_CP_FrameFinal(
			g_PPS_dbproc_table[table_index].dbproc_server,
			g_PPS_dbproc_table[table_index].dbproc_commit,
			&(g_PPS_dbproc_table[table_index].ims_aux_query),
			rec);
	   }
	}

        /* free the database connections */
        free_PPS_dbproc (table_index);
 
        /* decrement number of active rpcs */
        dec_mx_var (&g_numrpcs);

	if (rec)
		free (rec);
        if (common_hdr)
                free (common_hdr);

	fflush(stdout); fflush(stderr);
	*status = 0;

	return(ret_code);

} /* send_status_to_PPS */

short 
recv_job_from_PPS(
unsigned char   job_request[MSG_SIZE], 
long            request_size, 
unsigned char   job_spec[MSG_SIZE],
long            *spec_size, 
error_status_t  *status)
{
	Common_Header_Record *common_hdr = NULL;
	CP_JobReq_Record *rec = NULL;
	int 	msg_type;
	int	table_index;
	int	ret_code;
	char    logmsg[MAX_SYSLOG_MSGLEN+1] ;

#ifdef ODL_DEBUG
        fprintf(stdout, "CP -> PPS (Job Request):\n");
        fprintf(stdout, (char *) job_request);
#endif

        /* shutdown logic */
        if (test_mx_var(&g_dying)) {
                fprintf(stderr, "PPS rpc rejected - shutting down\n");
                return(ER_SHUTDOWN);
        }
        inc_mx_var(&g_numrpcs);

	if ( !job_request || (request_size == 0) )
	{
		pps_logMsg(ProgName, PPS_ERROR, "Input message is null.") ;
		dec_mx_var(&g_numrpcs);
		return(ER_INPUT_NULL) ;
	}

	if (ingest_CP_msg_mx("CP_message", job_request, &msg_type,
		&common_hdr, &rec) != OK)
	{
		dec_mx_var(&g_numrpcs);
                if (rec)
                        free (rec);
                if (common_hdr)
                        free (common_hdr);
		return(ER_PARSE_CP);
	}
	
	if (msg_type != CP_JOB_REQUEST)
	{
		pps_logMsg(ProgName, PPS_ERROR,
		            "CP message is not a Frame or Scan Request.");
		dec_mx_var(&g_numrpcs);
                if (rec)
                        free (rec);
                if (common_hdr)
                        free (common_hdr);

		return(ER_UNKNOWN_CP_JOB);
	}

       /* get a dbproc for the server and the commit service */
        if ((get_PPS_dbproc(&table_index)) != OK || (table_index == -1))
        {
		if (rec)
			free (rec);
	        if (common_hdr)
       		        free (common_hdr);

		pps_logMsg(ProgName, PPS_CRITICAL,
                                  "Unable to get a database login");
                dec_mx_var(&g_numrpcs);
                return(ER_NO_LOGIN_AVAIL);
        }
	if (! strcmp(rec->request_type, SCAN_KEYWD))
                    /* process the request */
                    ret_code = process_CP_ScanReq(
                        g_PPS_dbproc_table[table_index].dbproc_server,
                        g_PPS_dbproc_table[table_index].dbproc_commit,
                        &(g_PPS_dbproc_table[table_index].ims_aux_query),
                        rec, job_spec, spec_size);
	else if (! strcmp(rec->request_type, FRAME_KEYWD))
                    /* process the request */
                        ret_code = process_CP_FrameReq(
                        g_PPS_dbproc_table[table_index].dbproc_server,
                        g_PPS_dbproc_table[table_index].dbproc_commit,
                        &(g_PPS_dbproc_table[table_index].ims_aux_query),
                        rec, job_spec, spec_size);
	else
	{
		sprintf(logmsg, "Unknow CP request_type[%s]",
					rec->request_type);
		pps_logMsg(ProgName, PPS_ERROR, logmsg);
		ret_code = ER_UNKNOWN_CP_JOB;
	}

#ifdef ODL_DEBUG
	if (job_spec)
	{
        	fprintf(stdout, "PPS -> CP (Job Request):\n");
        	fprintf(stdout, (char *) job_spec);
	}
#endif

        /* free the database connections */
        free_PPS_dbproc (table_index);

	/* decrement number of active rpcs */
	dec_mx_var(&g_numrpcs);

	if (rec)
		free (rec);
        if (common_hdr)
                free (common_hdr);

	fflush(stdout); fflush(stderr);
	*status = 0;
	return(ret_code);

} /* recv_job_from_PPS*/
