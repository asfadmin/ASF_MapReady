/******************************************************************************
** File:        pps.c
**
** Function:    CP-PPS interface program for retrieving scan or L1 job
**              from PPS, then return job status to PPS.
**
** Author:      J. Ho
**
** Date:        12/29/95
**
** Notes:
**
**              This program is the main interface routine for CP and PPS.
**              It retrieves scan or L1 job from PPS, then passes job to CP
**              for processing the image.  Once it finishs, the return
**              status will send back to PPS to complete the job.
**
** CP-PPS v 1.4   4/25/96 use CP/PPS to PPS, CP-PPS/CP to CP
** CP-PPS v 1.4   4/26/96 use SUBSYSTEM_COMPLETED, not SUBSYSTEM_STATUS
** CP-PPS v 1.4.1 4/29/96 fixed PPS as destination
** CP-PPS v 1.4.2 5/22/96 added more debug messgaes
** CP-PPS v 1.4.3 5/28/96 run under PPS_servere down & more messages
**                        memory leaks, -asfn, add more error msgs
** CP-PPS v 1.5   6/4/96  added error-handling, etc.
** CP-PPS v 1.5.7 1/8/97  add COMPLETED or CANCEL/FAIL in status
**
******************************************************************************/
static char sccsid_pps_c[] = "@(#)pps.c	1.14 97/01/28 15:23:27";

#include <stdio.h>
#include <sys/types.h>
#include <syslog.h>
#include <dce/dce_error.h>
#include <dce/pthread.h>
#include <dce/rpcexc.h>
#include "asf.h" 
#include "asfcommon.h"
#include "asf_syslog.h"
#include "odl.h"
#include "PPSerr.h"
#include "messages.h"
#include "local_messages.h"
#include "version.h"

#define JOB_REQUEST             0
#define JOB_STATUS              1
#define INVALID_JOB_ID		-1
#define BUF_SIZE                5000
#define SUBSYSTEM_ACK           "SUBSYSTEM_ACK" 
#define SUBSYSTEM_COMPLETED     "SUBSYSTEM_COMPLETED" 
#define SUBSYSTEM_STATUS        "SUBSYSTEM_STATUS" 
#define SUBSYSTEM_HEARTBEAT     "SUBSYSTEM_HEARTBEAT" 
#define SPS_JOB_STATUS     	"SPS_JOB_STATUS" 

char *programName = NULL;

/* typedef unsigned long int error_status_t; */

extern char   *pps_err_msgs[];

void fill_in_status(char *source, char *destination, int job_id,
    char *status_type, char *status, char *comment, ODL sps_job_status);

int handle_invalid_job_id(ODL msg, ODL completed, char *error_string);

int send_completed_to_cp(ODL msg, int status, char *comment);

ODL StringToODL (char* str, size_t len, char *err);

static void cleanup_callback (ODL msg)
{
    static char STATUS[]        = "SPS_JOB_REQUEST.BODY.STATUS";
    ODL ack;
    char *s;

    if (! (ack = GetAckMsg(msg, SUBSYSTEM_COMPLETED))) {
        printfLLog(LOG_ERR, "Cannot create %s message", SUBSYSTEM_COMPLETED);
        return;
    }

    if (! ODLSetVal(ack, STATUS, 0)) {
        printfLLog(LOG_ERR, "Cannot set %s in %s message",
	    STATUS, SUBSYSTEM_COMPLETED);
        ODLFree(ack);
        return;
    }

/*    sleep(2);*/
    WriteMsgToServer(ack);
    ODLFree(ack);
} /* end of cleanup_callback */


static void subsys_callback (ODL msg)
{
    ODL    ack, jobODL;
    ODL    completed;
    ODL    sps_job_status;
    int    requestType, pps_ret_code, err;
    long   buf_size = BUF_SIZE;
    long   spec_size = BUF_SIZE;
    char   *cp_ppsPtr, *ppsPtr;
    char   *bufPtr, *msgType;
    static int error_stat;
    static unsigned char error_string[dce_c_error_string_len];
    static int job_id;
    unsigned char job_spec[BUF_SIZE];
    unsigned char cp_job_spec[BUF_SIZE];
    unsigned char scratch[BUF_SIZE+3];
    unsigned char msg_from_cp[BUF_SIZE];
    error_status_t status = 0;
    FILE *fd;

    if (!(ack = GetAckMsg(msg, SUBSYSTEM_ACK))) {
        printfLLog(LOG_ERR, ERR_CREATE_MESSAGE, SUBSYSTEM_ACK);
        return;
    }
    WriteMsgToServer(ack);
    ODLFree(ack);
#ifdef DEBUG_PPS
    printfLLog(LOG_INFO, "Sent %s to CP", SUBSYSTEM_ACK);
#endif

    /* Create SUBSYSTEM_COMPLETED message */
    if (! (completed = GetAckMsg(msg, SUBSYSTEM_COMPLETED))) {
        printfLLog(LOG_ERR, ERR_CREATE_MESSAGE, SUBSYSTEM_COMPLETED);
        return;
    }

    /* Convert message from CP from ODL to a string. */
    bufPtr = ODLToStr(msg, NULL);
    if (bufPtr == NULL) {
        printfLLog(LOG_ERR, ERR_ODL_TO_STRING);
	(void) send_completed_to_cp(completed, -1, ERR_ODL_TO_STRING);
        ODLFree(completed);
        return;
    } else {
	if (strlen(bufPtr) > BUF_SIZE) {
	    printfLLog(LOG_ERR, ERR_MESSAGE_TOO_LONG, BUF_SIZE);
	    (void) sprintf(scratch, ERR_MESSAGE_TOO_LONG, BUF_SIZE);
	    (void) send_completed_to_cp(completed, -1, scratch);
	    ODLFree(completed);
	    free(bufPtr);
	    return;
	}
	if ((cp_ppsPtr = strstr(bufPtr, programName)) == (char *) NULL) {
	    printfLLog(LOG_ERR, ERR_BAD_DESTINATION_CP);
	    (void) send_completed_to_cp(completed, -1, ERR_BAD_DESTINATION_CP);
	    ODLFree(completed);
	    free(bufPtr);
	    return;
	}
	if ((cp_ppsPtr = strstr(cp_ppsPtr, "\"")) == (char *) NULL) {
	    printfLLog(LOG_ERR, ERR_BAD_FORMAT_CP);
	    (void) send_completed_to_cp(completed, -1, ERR_BAD_FORMAT_CP);
	    ODLFree(completed);
	    free(bufPtr);
	    return;
	}
	strcpy(msg_from_cp, bufPtr);
	ppsPtr = strstr(msg_from_cp, programName);
	(void) strcpy(ppsPtr, "PPS");
	ppsPtr = strchr(ppsPtr, 0);
	strcpy(ppsPtr, cp_ppsPtr);
	free(bufPtr);
    }
    
    /* get MSG_TYPE if SPS_JOB_REQUEST */
    msgType = ODLGetStr(msg, "SPS_JOB_REQUEST.COMMON_HEADER.MSG_TYPE");
    if (msgType == NULL) {
        /* get MSG_TYPE if SPS_JOB_STATUS */
        msgType = ODLGetStr(msg, "SPS_JOB_STATUS.COMMON_HEADER.MSG_TYPE");
        if (msgType == NULL) {
	    printfLLog(LOG_ERR, ERR_UNRECOGNIZED_MSG);
	    (void) send_completed_to_cp(completed, -1, ERR_UNRECOGNIZED_MSG);
            ODLFree(completed);
            return;
        }
    }

    /* set proper request type */
    if (!strcmp(msgType, "SPS_JOB_REQUEST")) {
        requestType = JOB_REQUEST;
    } else if (!strcmp(msgType, "SPS_JOB_STATUS")) {
        requestType = JOB_STATUS;
	job_id = ODLGetInt(msg, "SPS_JOB_STATUS.BODY.JOB_ID", &error_stat);
	if (error_stat != 0) {
	    printfLLog(LOG_ERR, ERR_BAD_JOB_ID_CP);
	    (void) send_completed_to_cp(completed, -1, ERR_BAD_JOB_ID_CP);
	    ODLFree(completed);
	    return;
	}
    } else {
	printfLLog(LOG_ERR, ERR_UNRECOGNIZED_MSG_TYPE);
	(void) send_completed_to_cp(completed, -1, ERR_UNRECOGNIZED_MSG_TYPE);
	ODLFree(completed);
	return;
    }

    buf_size = strlen(msg_from_cp);
    if (requestType == JOB_REQUEST) {
#ifdef READ_JOB_FROM_FILE
	printfLLog(LOG_DEBUG, "Reading job from /tmp/CP-PPS.test ....");
	if ((fd = fopen("/tmp/CP-PPS.test", "r")) != (FILE *) NULL) {
	    spec_size = fread(job_spec, sizeof(char), BUF_SIZE, fd);
	    status = 0;
	    printfLLog(LOG_DEBUG, "Job read from /tmp/CP-PPS.test");
	} else {
	    printfLLog(LOG_DEBUG, ERR_NO_TEST_FILE);
	    (void) send_completed_to_cp(completed, -1, ERR_NO_TEST_FILE);
	    ODLFree(completed);
	    return;
	}
#else
	printfLLog(LOG_DEBUG, "Sending SPS_JOB_REQUEST to PPS");
        pps_ret_code = recv_job_from_PPS(msg_from_cp, buf_size, 
                                         job_spec, &spec_size, &status);
	printfLLog(LOG_DEBUG, "Requesting job from PPS");
#endif /* READ_JOB_FROM_FILE */
    } else { /* JOB_STATUS */
	printfLLog(LOG_DEBUG, "Sending status for job %d to PPS",
	    job_id);
        pps_ret_code = send_status_to_PPS(msg_from_cp, buf_size, &status);
	printfLLog(LOG_DEBUG, "Status for job %d sent to PPS",
	    job_id);
    }

    /* check return message for calling PPS */
    if (status != 0) {
        dce_error_inq_text(status, error_string, &error_stat);
	/*
	 * According to the man page, this routine always returns
	 * a message even if the call fails.
	 */
	if (status == rpc_s_no_more_bindings) {
	    /* The PPS server is probably down -- use our own message */
	    printfLLog(LOG_ERR, ERR_NO_MORE_BINDINGS);
	    (void) sprintf(scratch, ERR_NO_MORE_BINDINGS);
	} else if (status == rpc_s_comm_failure) {
	    /* The PPS server is probably down -- use our own message */
	    printfLLog(LOG_ERR, ERR_COMM_FAILURE);
	    (void) sprintf(scratch, ERR_COMM_FAILURE);
	} else {
	    if (requestType == JOB_REQUEST) {
		printfLLog(LOG_ERR, ERR_RECEIVING_JOB_RPC, error_string);
		(void) sprintf(scratch, ERR_RECEIVING_JOB_RPC, error_string);
	    } else if (requestType == JOB_STATUS) {
		printfLLog(LOG_ERR, ERR_SENDING_STATUS_RPC, error_string);
		(void) sprintf(scratch, ERR_SENDING_STATUS_RPC, error_string);
	    }
	}

#ifdef DEBUG_PPS
	printfLLog(LOG_ERR, "Status from rpc = %d\n", status);
	printfLLog(LOG_ERR, "Error string from rpc:  %s\n", error_string);
	printfLLog(LOG_ERR,
	    "Status from dce_error_inq_text = %d\n", error_stat);
#endif /* DEBUG_PPS */

	(void) send_completed_to_cp(completed, -1, scratch);
	ODLFree(completed);
	return;
    }
    if (pps_ret_code == 0) {
        if (spec_size == 0) {
            printfLLog(LOG_INFO, ERR_JOB_NOT_AVAILABLE);
	    (void) send_completed_to_cp(completed, 0, ERR_JOB_NOT_AVAILABLE);
	    ODLFree(completed);
	    return;
        } else if (requestType == JOB_REQUEST) {

#ifdef DEBUG_PPS
	    /*
	     * Print the job received from the PPS verbatim.
	     */
	    (void) printf("Job received from PPS is\n%s\n", job_spec);
	    (void) fflush(stdout);
#endif
	    /*
	     * Change the value of SOURCE in the message to the logical
	     * name expected by CP.
	     */
	    if ((ppsPtr = strstr(job_spec, "PPS")) == (char *) NULL) {
		printfLLog(LOG_ERR, ERR_BAD_SOURCE_PPS);
		(void) send_completed_to_cp(completed, -1, ERR_BAD_SOURCE_PPS);
		ODLFree(completed);
		return;
	    }
	    strcpy(scratch, job_spec);
	    cp_ppsPtr = strstr(scratch, "PPS");
	    (void) strcpy(cp_ppsPtr, "CP-");
	    strcat(cp_ppsPtr, ppsPtr);

	    /*
	     * Convert the job from PPS to ODL format for transmission
	     * to CP.
	     */
	    jobODL = StringToODL(scratch, strlen(scratch), error_string);
	    if (jobODL == NULL) {
		/*
		 * Could not parse the job due to a syntax error.
		 * Send a SUBSYSTEM_COMPLETED error message back to CP.
		 * If the job ID can be extracted, send an SPS_JOB_STATUS
		 * message back to PPS with the job ID.  Otherwise, send
		 * an SPS_JOB_STATUS message with a job ID of INVALID_JOB_ID.
		 */
		ppsPtr = strstr(scratch, "JOB_ID");
		if (ppsPtr == (char *) NULL) {
		    /* No job ID could be found */
		    (void) sprintf(scratch, ERR_BAD_FORMAT_PPS_WT_ID,
			error_string);
		    (void) handle_invalid_job_id(msg, completed, scratch);
		    ODLFree(completed);
		    return;
		}
		ppsPtr = strstr(ppsPtr, "=");
		if (ppsPtr == (char *) NULL) {
		    /* No job ID could be found */
		    (void) sprintf(scratch, ERR_BAD_FORMAT_PPS_WT_ID,
			error_string);
		    (void) handle_invalid_job_id(msg, completed, scratch);
		    ODLFree(completed);
		    return;
		}

		if (sscanf(ppsPtr, "=%d", &job_id) != 1) {
		    /* No job ID could be found */
		    (void) sprintf(scratch, ERR_BAD_FORMAT_PPS_WT_ID,
			error_string);
		    (void) handle_invalid_job_id(msg, completed, scratch);
		    ODLFree(completed);
		    return;
		}

		/* Found a job ID -- reference it */
		printfLLog(LOG_ERR, ERR_BAD_FORMAT_PPS_W_ID,
		    job_id, error_string);
		(void) sprintf(scratch, ERR_BAD_FORMAT_PPS_W_ID,
		    job_id, error_string);
		(void) send_completed_to_cp(completed, -1, scratch);
		ODLFree(completed);

		if (! (sps_job_status = GetAckMsg(msg, SPS_JOB_STATUS))) {
		    printfLLog(LOG_ERR, ERR_CREATE_MESSAGE, SPS_JOB_STATUS);
		    return;
		}
		fill_in_status("CP", "PPS", job_id, "FINAL",
		    "CANCEL/FAIL", scratch, sps_job_status);
		bufPtr = ODLToStr(sps_job_status, NULL);
		if (bufPtr != (char *) NULL) {
		    pps_ret_code = send_status_to_PPS(
			bufPtr, strlen(bufPtr), &status);
#ifdef DEBUG_PPS
		    /*
		     * Print the message sent to the PPS
		     * verbatim.
		     */
		    (void) printf("Status sent to PPS is\n%s\n", bufPtr);
		    (void) fflush(stdout);
#endif
		    printfLLog(LOG_DEBUG, OK_BAD_STATUS_SENT_W_ID,
			job_id);
		    /*
		     * Log the status, but don't respond further
		     * to CP -- the job was bad anyway.
		     */
		    errCode2Msg(pps_ret_code, JOB_STATUS, job_id,
			error_string);
		    printfLLog(LOG_ERR, error_string);
		    free(bufPtr);
		}
		ODLFree(sps_job_status);
		return;
	    }
	    /*
	     * Job is a legal ODL message -- try to get the job ID.
	     */
	    job_id = ODLGetInt(jobODL, "SCAN_REQUEST.BODY.JOB_ID",
		&error_stat);
	    if (error_stat != 0) {
		job_id = ODLGetInt(jobODL, "FRAME_REQUEST.BODY.JOB_ID",
		    &error_stat);
		if (error_stat != 0) {
		    /*
		     * We can't get the job ID from the message.
		     * The job ID must be either missing or malformed.
		     * Send a SUBSYSTEM_COMPLETED error message back to CP.
		     * Since no job ID can be obtained, use a job ID of
		     * INVALID_JOB_ID and send a status message to PPS.
		     */
		    (void) handle_invalid_job_id(msg, completed,
			ERR_BAD_JOB_ID_PPS);
		    ODLFree(jobODL);
		    ODLFree(completed);
		    return;
		}
	    }
	    printfLLog(LOG_DEBUG, OK_JOB_ID, job_id);
	    /*
	     * Send the job to CP.
	     */
/*    sleep(2);*/
/*** 
    Don't call this routine here because there is no malloc for 
    sps_job_status. You need to call this routine if you want to 
    report status to PPS. In this case, you need to call GetAckMsg
    first and call ODLFree last.

            fill_in_status("CP", "PPS", job_id, "FINAL",
		"COMPLETED", NULL, sps_job_status);
***/
	    WriteMsgToServer(jobODL);
	    ODLFree(jobODL);
	    ODLFree(completed);
	    return;
        } else { /* JOB_STATUS */
            printfLLog(LOG_DEBUG, OK_STATUS_SENT, job_id);
	    (void) sprintf(scratch, OK_STATUS_SENT, job_id);
	    (void) send_completed_to_cp(completed, 0, scratch);
	    ODLFree(completed);
	    return;
        }
    } else { /* error */
        errCode2Msg(pps_ret_code, requestType, job_id, error_string);
        printfLLog(LOG_ERR, error_string);
	(void) send_completed_to_cp(completed, -1, error_string);
	ODLFree(completed);
	return;
    }
} /* end of subsys_callback */


errCode2Msg(int errCode, int requestType, int job_id, char *errMsg)
{
    struct {
        char *str;
    } errorMessage[79] = {"ER_NO_ERROR",             /* 0 */
                          "ER_INPUT_NULL",
                          "ER_PARSE_IMS",
                          "ER_NOT_CANCEL",
                          "ER_NOT_DUB",
                          "ER_NOT_L1PR",
                          "ER_NOT_SCAN",
                          "ER_NOT_SVAVAIL",
                          "ER_PROCESS_CANCEL",
                          "ER_PROCESS_DUB",
                          "ER_PROCESS_L1PR",         /* 10 */
                          "ER_PROCESS_SCAN",
                          "ER_PROCESS_SVAVAIL",
                          "ER_PARSE_CP", 
                          "ER_UNKNOWN_CP_JOB", 
                          "ER_PROCESS_CP", 
                          "ER_NOT_CP_STATUS_MSG",
                          "ER_NO_LOGIN_AVAIL",
                          "ER_SHUTDOWN", 
                          "ER_IMS_GHA_QUERY_FAIL", 
                          "ER_IMS_GHA_QUERY_NODATA", /* 20 */
                          "ER_IMS_TCE_QUERY_FAIL",
                          "ER_IMS_TCE_QUERY_NODATA",
                          "ER_IMS_SV_QUERY_FAIL",
                          "ER_IMS_SV_QUERY_NODATA",
                          "ER_INSERT_SCAN",
                          "ER_INSERT_JOBS",
                          "ER_JOBID_NOT_FOUND",
                          "ER_IMS_MALLOC",
                          "ER_PPS_MALLOC",
                          "ER_DBLOGIN_FAIL",         /* 30 */
                          "ER_INSERT_L1PR",
                          "ER_INSERT_PROC_PARMS",
                          "ER_IMS_SEND_STATUS_FAIL",
                          "ER_IMS_CAL_QUERY_FAIL",
                          "ER_IMS_CAL_QUERY_NODATA",
                          "ER_IMS_SCAN_QUERY_FAIL",
                          "ER_IMS_SCAN_QUERY_NODATA",
                          "ER_SP_GET_NEXT_JOB_ID",
                          "ER_JOB_SUBMITTED",
                          "ER_FIND_SCAN_ORDER",      /* 40 */
                          "ER_FIND_L1_ORDER",        
                          "ER_IMS_OPEN_DB_CONNECT",
                          "ER_SCAN_AVAIL_FAIL",
                          "ER_L1_AVAIL_FAIL",
                          "ER_DUB_AVAIL_FAIL",
                          "ER_NO_AVAIL_SCAN_SCANSAR",
                          "ER_NO_AVAIL_SCAN_CONT",
                          "ER_NO_AVAIL_DUB",
                          "ER_PARSE_COMMON_HDR",
                          "ER_MALLOC_COMMON_HDR",      /* 50 */
                          "ER_COMMON_HDR_EXTRACT",
                          "ER_UNKNOWN_IMS_MSG",
                          "ER_ODL_READLABEL_BUF",
                          "ER_FIND_JOB",
                          "ER_FIND_L1PP",
                          "ER_UPDATE_JOBS",
                          "ER_IMS_FRAME_QUERY_FAIL",
                          "ER_IMS_FRAME_QUERY_NODATA",
                          "ER_UPDATE_L1_PROC_PARMS",
                          "ER_UPDATE_JOBSTATE",        /* 60 */
                          "ER_UPDATE_L1_USING_SCAN",
                          "ER_REMOVE_SCAN_COMPLETE",
                          "ER_REMOVE_L1_COMPLETE",
                          "ER_REMOVE_L1_ORDER",
                          "ER_BUFFER_TOO_SMALL",
                          "ER_UPDATE_SCAN",
                          "ER_NO_AVAIL_FRAME_SCANSAR",
                          "ER_NO_AVAIL_FRAME_CONT",
                          "ER_BAD_ODL_TIME_FORMAT",
                          "ER_KEYWORD_VALUE_TOO_LONG",	/* 70 */
                          "ER_INVALID_KEYWORD_VALUE_TYPE",
                          "ER_KEYWORD_NOT_FOUND",
                          "ER_INSUFFICIENT_MEDIA_INFO",
                          "ER_DB_ACCESS",
                          "ER_JOB_IS_NOW_READY",
                          "ER_JOB_IS_NOW_AVAIL",
                          "ER_JOB_IS_STILL_PENDING",
                          "ER_DEADLOCK"			/* 78 */
                          };

    /*
     * This routine will work as long as a one-to-one correspondence
     * between the error codes is maintained.
     */
    if (errCode == 0) {
	/*
	 * Should never be executed, since this routine is only supposed
	 * to be called if errCode != 0
	 */
	sprintf(errMsg, "RPC completed successfully");
    } else if (0 < errCode && errCode < sizeof(errorMessage)/sizeof(char *)) {
	if (requestType == JOB_STATUS &&
	    (errCode == ER_FIND_SCAN_ORDER || errCode == ER_FIND_L1_ORDER)) {
	    sprintf(errMsg, ERR_UNRECOGNIZED_JOB, job_id);
	} else if (requestType == JOB_STATUS) {
	    sprintf(errMsg, ERR_SENDING_STATUS, errorMessage[errCode].str);
	} else if (requestType == JOB_REQUEST) {
	    sprintf(errMsg, ERR_RECEIVING_JOB, errorMessage[errCode].str);
	}
    } else {
	if (requestType == JOB_STATUS) {
	    sprintf(errMsg, ERR_SENDING_STATUS_CODE_UNKNOWN, errCode);
	} else if (requestType == JOB_STATUS) {
	    sprintf(errMsg, ERR_RECEIVING_JOB_CODE_UNKNOWN, errCode);
	}
    }
} /* end of errCode2Msg */


/*--------------------------------------------------------------------------*
** NAME:
**  main
**
** DESCRIPTION:
**  This program is the main part of CP - PPS interface routine.
**  It sets up the communication with CP first, and establish
**  the callback events to handle JOB REQUEST from CP to PPS.
**
** Notes:
**
**-------------------------------------------------------------------------*/
main(int argc, char *argv[])
{
    AsfApp app;
    register int i;

    /*
     * Open syslog files temporarily, just in case errors occur before
     * initASFlog() is called.
     */
    openlog("CP", LOG_PID|LOG_CONS|LOG_NDELAY, LOG_CP);
    for (i = 1; argv[i] != NULL; i++) {
        if (strcmp(argv[i], "-asfn") == 0) {
            if (argv[++i] == NULL || argv[i][0] == '-') {
	        syslog(LOG_ERR, "%s:  No argument specified for -asfn flag",
		    argv[0]);
#ifdef DEBUG_PPS
                fprintf(stderr, "No argument specified for -asfn flag\n");
		fflush(stderr);
#endif /* DEBUG_PPS */
                exit(-1);
            }
            programName = argv[i];
        }
    }

    if ((app = AsfAppInitialize(programName, &argc, argv)) == NULL) {
        syslog(LOG_ERR, "can't initialize application.  Goodbye!");
#ifdef DEBUG_PPS
	fprintf(stderr, "can't initialize application.  Goodbye!\n");
	fflush(stderr);
#endif /* DEBUG_PPS */
        exit (-1);
    }

    AsfAddCallback(app, "SPS_JOB_REQUEST", subsys_callback, 0, 0);
    AsfAddCallback(app, "SPS_JOB_STATUS", subsys_callback, 0, 0);
    AsfAddCallback(app, "CLEANUP_REQUEST", cleanup_callback, 0, 0);

    AsfAppMainLoop(app);

} /* end of main */

void fill_in_status(char *source, char *destination, int job_id,
    char *status_type, char *status, char *comment, ODL sps_job_status)
{
    if (source != (char *) NULL)
	ODLSetVal(sps_job_status,
	    "SPS_JOB_STATUS.COMMON_HEADER.SOURCE", source);

    if (destination != (char *) NULL)
	ODLSetVal(sps_job_status,
	    "SPS_JOB_STATUS.COMMON_HEADER.DESTINATION", destination);

    ODLSetVal(sps_job_status,
	"SPS_JOB_STATUS.BODY.JOB_ID", job_id);

    if (status_type != (char *) NULL)
	ODLSetVal(sps_job_status,
	    "SPS_JOB_STATUS.BODY.STATUS_TYPE", status_type);

    if (status != (char *) NULL)
	ODLSetVal(sps_job_status,
	    "SPS_JOB_STATUS.BODY.STATUS", status);

    if (comment != (char *) NULL)
	ODLSetVal(sps_job_status,
	    "SPS_JOB_STATUS.BODY.COMMENT", comment);
}

int handle_invalid_job_id(ODL msg, ODL completed, char *error_string)
{
    int job_id;
    unsigned char scratch[BUF_SIZE+3];
    ODL sps_job_status;
    int pps_ret_code;
    char *bufPtr;
    error_status_t status = 0;

    job_id = INVALID_JOB_ID;
    printfLLog(LOG_ERR, error_string);
    (void) send_completed_to_cp(completed, -1, error_string);

    if (! (sps_job_status = GetAckMsg(msg, SPS_JOB_STATUS))) {
	printfLLog(LOG_ERR, ERR_CREATE_MESSAGE, SPS_JOB_STATUS);
	return -1;
    }
    fill_in_status("CP", "PPS", job_id, "FINAL",
	"CANCEL/FAIL", error_string, sps_job_status);
    bufPtr = ODLToStr(sps_job_status, NULL);
    if (bufPtr != (char *) NULL) {
	pps_ret_code = send_status_to_PPS(bufPtr, strlen(bufPtr), &status);
#ifdef DEBUG_PPS
	/*
	 * Print the message sent to the PPS verbatim.
	 */
	(void) printf("Status sent to PPS is\n%s\n", bufPtr);
	(void) fflush(stdout);
#endif
	printfLLog(LOG_DEBUG, OK_BAD_STATUS_SENT_WT_ID, job_id);
	/*
	 * Log the status, but don't respond further to CP --
	 * the job was bad anyway.
	 */
	errCode2Msg(pps_ret_code, JOB_STATUS, job_id, scratch);
	printfLLog(LOG_ERR, scratch);
	free(bufPtr);
    }
    ODLFree(sps_job_status);
    return 0;
}

int send_completed_to_cp(ODL msg, int status, char *comment)
{
    ODLSetVal(msg, "SUBSYSTEM_COMPLETED.BODY.STATUS", 
	      (status ? "CANCEL/FAIL" : "COMPLETED"));

    if (comment != (char *) NULL)
	ODLSetVal(msg, "SUBSYSTEM_COMPLETED.BODY.COMMENT", comment);
    else
	ODLSetVal(msg, "SUBSYSTEM_COMPLETED.BODY.COMMENT", "");

/*    sleep(2);*/
    WriteMsgToServer(msg);
    return 0;
}
