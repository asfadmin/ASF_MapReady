/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:       PPShdr.h
Description:
		PPS error numbers
 
Creator:        Nadia Adhami (nadia.adhami@jpl.nasa.gov)
Notes:
==============================================================================*/
 
#ifndef _PPSERR_
#define _PPSERR_
 
#pragma ident "@(#)PPSerr.h	1.3  10/31/97"

/* ERROR NUMBERS */

#define ER_NO_ERROR			0
#define ER_INPUT_NULL			1
#define ER_PARSE_IMS			2
#define ER_NOT_CANCEL			3
#define ER_NOT_DUB			4
#define ER_NOT_L1PR			5
#define ER_NOT_SCAN			6
#define ER_NOT_SVAVAIL			7
#define ER_PROCESS_CANCEL		8
#define ER_PROCESS_DUB			9
#define ER_PROCESS_L1PR			10
#define ER_PROCESS_SCAN			11
#define ER_PROCESS_SVAVAIL		12
#define ER_PARSE_CP			13
#define ER_UNKNOWN_CP_JOB		14
#define ER_PROCESS_CP			15
#define ER_NOT_CP_STATUS_MSG		16
#define ER_NO_LOGIN_AVAIL		17
#define ER_SHUTDOWN			18
#define ER_IMS_GHA_QUERY_FAIL		19
#define ER_IMS_GHA_QUERY_NODATA		20
#define ER_IMS_TCE_QUERY_FAIL		21
#define ER_IMS_TCE_QUERY_NODATA		22
#define ER_IMS_SV_QUERY_FAIL		23
#define ER_IMS_SV_QUERY_NODATA		24
#define ER_INSERT_SCAN			25
#define ER_INSERT_JOBS			26
#define ER_JOBID_NOT_FOUND		27
#define ER_IMS_MALLOC			28
#define ER_PPS_MALLOC			29
#define ER_DBLOGIN_FAIL			30
#define ER_INSERT_L1PR			31
#define ER_INSERT_PROC_PARMS		32
#define ER_IMS_SEND_STATUS_FAIL		33
#define ER_IMS_CAL_QUERY_FAIL		34
#define ER_IMS_CAL_QUERY_NODATA		35
#define ER_IMS_SCAN_QUERY_FAIL		36
#define ER_IMS_SCAN_QUERY_NODATA	37
#define ER_SP_GET_NEXT_JOB_ID		38
#define ER_JOB_SUBMITTED		39
#define ER_FIND_SCAN_ORDER		40
#define ER_FIND_L1_ORDER		41
#define ER_IMS_OPEN_DB_CONNECT		42
#define ER_SCAN_AVAIL_FAIL		43
#define ER_L1_AVAIL_FAIL		44
#define ER_DUB_AVAIL_FAIL		45
#define ER_NO_AVAIL_SCAN_SCANSAR 	46
#define ER_NO_AVAIL_SCAN_CONT		47
#define ER_NO_AVAIL_DUB			48
#define ER_PARSE_COMMON_HDR		49
#define ER_MALLOC_COMMON_HDR		50
#define ER_COMMON_HDR_EXTRACT		51
#define ER_UNKNOWN_IMS_MSG		52
#define ER_ODL_READLABEL_BUF		53
#define ER_FIND_JOB			54
#define ER_FIND_L1PP			55
#define ER_UPDATE_JOBS			56
#define ER_IMS_FRAME_QUERY_FAIL 	57
#define ER_IMS_FRAME_QUERY_NODATA 	58
#define ER_UPDATE_L1_PROC_PARMS		59
#define ER_UPDATE_JOBSTATE		60
#define ER_UPDATE_L1_USING_SCAN		61
#define ER_REMOVE_SCAN_COMPLETE		62
#define ER_REMOVE_L1_COMPLETE		63
#define ER_REMOVE_L1_ORDER		64
#define ER_BUFFER_TOO_SMALL		65
#define ER_UPDATE_SCAN			66
#define ER_NO_AVAIL_FRAME_SCANSAR 	67
#define ER_NO_AVAIL_FRAME_CONT		68
#define ER_BAD_ODL_TIME_FORMAT 		69
#define ER_KEYWORD_VALUE_TOO_LONG	70
#define ER_INVALID_KEYWORD_VALUE_TYPE	71
#define ER_KEYWORD_NOT_FOUND		72
#define ER_INSUFFICIENT_MEDIA_INFO	73
#define ER_DB_ACCESS			74
#define ER_JOB_IS_NOW_READY             75
#define ER_JOB_IS_NOW_AVAIL             76
#define ER_JOB_IS_NOW_PENDING           77
#define ER_JOB_IS_STILL_PENDING         78
#define ER_JOB_IS_STILL_READY           79
#define ER_DEADLOCK                     80
#define ER_DB_FATAL                     81
#define ER_DB_NONFATAL                  82
#define ER_CONFIG_FILE                  83
#define ER_IMS_ORDER_NOT_FOUND          84
#define ER_MISSED_FRAME 		85
#define ER_REJECTED_FRAME		86

#endif _PPSERR_
