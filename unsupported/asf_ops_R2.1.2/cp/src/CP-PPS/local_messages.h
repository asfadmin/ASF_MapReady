/*
 * Messages used internally by CP-PPS
 */

static char sccsid_local_messages_h[] =
    "@(#)local_messages.h	1.1 96/06/04 12:52:43";

#ifndef _LOCAL_MESSAGES_H
#define _LOCAL_MESSAGES_H

#define ERR_BAD_DESTINATION_CP	"ERROR:  DESTINATION not set correctly in message from CP"
#define ERR_BAD_FORMAT_CP	"ERROR:  Badly formatted message from CP"
#define ERR_BAD_JOB_ID_CP	"ERROR:  JOB_ID not set correctly in message from CP"
#define ERR_BAD_JOB_ID_PPS	"ERROR:  JOB_ID not set correctly in message from PPS"
#define ERR_BAD_FORMAT_PPS_W_ID	"ERROR in job %d from PPS (near %s)"
#define ERR_BAD_FORMAT_PPS_WT_ID	"ERROR in job from PPS (near %s) -- cannot read job ID"
#define ERR_BAD_SOURCE_PPS	"ERROR:  SOURCE not set correctly in message from PPS"
#define ERR_COMM_FAILURE	"ERROR:  communications failure (PPS server or RPC daemon may be down)"
#define ERR_CREATE_MESSAGE	"ERROR:  Cannot create %s message"
#define ERR_MESSAGE_TOO_LONG	"ERROR:  Message from CP exceeds maximum of %d characters"
#define ERR_JOB_NOT_AVAILABLE	"ERROR:  The requested type of job is not available"
#define ERR_NO_MORE_BINDINGS	"ERROR:  no more bindings (PPS server may be down)"
#define ERR_NO_TEST_FILE	"ERROR:  Cannot open /tmp/CP-PPS.test for reading"
#define ERR_ODL_TO_STRING	"ERROR:  Cannot convert message from CP to a string"
#define ERR_RECEIVING_JOB	"ERROR receiving job from PPS:  PPS returned code %s"
#define ERR_RECEIVING_JOB_CODE_UNKNOWN	"ERROR receiving job from PPS:  PPS returned code %d"
#define ERR_RECEIVING_JOB_RPC	"ERROR receiving job from PPS:  PPS returned RPC message %s"
#define ERR_SENDING_STATUS	"ERROR sending status to PPS:  PPS returned code %s"
#define ERR_SENDING_STATUS_CODE_UNKNOWN	"ERROR sending status to PPS:  PPS returned code %d"
#define ERR_SENDING_STATUS_RPC	"ERROR sending status to PPS:  PPS returned RPC message %s"
#define ERR_SET_VALUE		"ERROR:  Cannot set %s in %s message"
#define ERR_UNRECOGNIZED_JOB	"ERROR sending status to PPS for job %d:  unknown job ID"
#define ERR_UNRECOGNIZED_MSG	"ERROR:  Unrecognized message from CP"
#define ERR_UNRECOGNIZED_MSG_TYPE	"ERROR:  Cannot find MSG_TYPE in message from CP"
#define OK_BAD_STATUS_SENT_W_ID	"Status for job %d sent to PPS (CANCEL/FAIL)"
#define OK_BAD_STATUS_SENT_WT_ID	"Status for invalid job sent to PPS (CANCEL/FAIL, id %d)"
#define OK_JOB_ID		"Sending job %d to CP"
#define OK_STATUS_SENT		"Status for job %d sent to PPS"

#endif /* !_LOCAL_MESSAGES_H */
