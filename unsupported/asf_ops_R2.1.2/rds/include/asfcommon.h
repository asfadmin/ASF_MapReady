/*
 * @(#)asfcommon.h	2.8 96/02/19 07:54:45
 */

#ifndef _ASFCOMMON_H
#define _ASFCOMMON_H

static char sccsid_asfcommon_h[] =
	"@(#)asfcommon.h	2.8 96/02/19 07:54:45";

/* subsystem names & id's ******/
#define ASF_CP		"CP"
#define ASF_RDS		"ASF_RDS"
#define ASF_SSP1	"ASF_SSP1"
#define ASF_SSP2	"ASF_SSP2"
#define ASF_ASP		"ASF_ASP"
#define ASF_PPS		"ASF_PPS"
#define ASF_IMS		"ASF_IMS"
#define CP_PPS		"CP_PPS"
#define CP_IMS          "CP_IMS"
#define CP_QC		"CP_QC"
#define CP_SCAN_QC      "CP_SCAN_QC"
#define CP_PRE_QC       "CP_PRE_QC"
#define CP_GPR	        "CP_GPR"
#define ASF_LOG_BROWSER	"LOG_BROWSER"
/* end subsystem names ......*/


/* message types **********/
#define ACK_MSG              "SUBSYSTEM_ACK"      /* may be obsoleted */
#define STATUS_MSG           "SUBSYSTEM_STATUS"   /* may be obsoleted */

/* end message types .....*/

/* message fields ***********/
#define MSG_TYPE        	"COMMON_HEADER.MSG_TYPE"
#define SEQ_NO        		"COMMON_HEADER.SEQ_NO" 
#define DESTINATION		"COMMON_HEADER.DESTINATION"
#define SOURCE			"COMMON_HEADER.SOURCE"
#define TIME			"COMMON_HEADER.TIME"
#define NUMBER_OF_RECORDS	"COMMON_HEADER.NUMBER_OF_RECORDS"

/* end message fields........*/

#endif   /* !_ASFCOMMON_H */
