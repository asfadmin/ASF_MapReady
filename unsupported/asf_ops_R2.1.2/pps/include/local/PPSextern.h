/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:       PPSextern.h
Description:
		PPS extern definitions
 
Creator:        Nadia Adhami (nadia.adhami@jpl.nasa.gov)
Notes:
 
==============================================================================*/
 
#ifndef _PPSEXTERN_
#define _PPSEXTERN_

#pragma ident "@(#)PPSextern.h	1.2  12/16/96"

#include "PPSdefs.h"

/* structures */
 
extern char *pps_err_msgs[];
extern CONFIG_VALUES	configValues;
extern DB_Proc          *g_PPS_dbproc_table;

extern char ProgName[];

/* system functions */

extern void free() ;
extern char *strcpy() ;
extern char *strncpy() ;
extern void *malloc() ;

/* lib pps functions */


extern void 	WriteLabelBuf() ;
extern int	ReadLabel_buf();
extern void 	create_ODL_common_hdr() ;
extern void 	create_ODL_order_status();
extern void 	create_ODL_order_status_body() ;
extern int	db_max_value();
extern void     check_auto_submit();
extern void 	fill_scanorder_status() ;
extern void 	send_status_to_IMS() ;
extern void 	pps_start_xact() ;
extern void 	pps_commit_xact() ;
extern void 	assign_default_config() ;
extern void 	init_config_null() ;
extern void 	set_num_threads () ;
extern void 	*util_do_malloc() ;
extern void 	convert_ODLdate_to_timerec() ;
extern void 	abortall() ;
extern void 	scanreq_erase_svec() ;
extern void 	l1req_erase_svec() ;
extern void 	fill_L1order_status() ;
extern void 	free_PPS_dbproc() ;
extern void 	pps_count() ;
extern int      get_jobid();
extern int      match_L1request();
extern int      match_svec_2_scan();
extern int      insert_into_schedule();
extern int      get_next_job_id();
extern int      get_jobstate();
extern int  	get_num_threads () ;
extern int      (*msg_handler)();
extern int 	ReadLabel_buf() ;
extern int 	parse_common_hdr() ;
extern int 	extract_common_header() ;
extern int 	extract_IMS_CancelReq_Record() ;
extern int 	extract_IMS_L1PReq_Record() ;
extern int 	extract_IMS_ScanReq_Record() ;
extern int 	extract_IMS_SVecAvail_Record() ;
extern int 	extract_CP_JobReq_Record();
extern int	process_IMS_L1PReq() ;
extern int	process_IMS_SvAvail() ;
extern int	process_IMS_ScanReq() ;
extern int	process_IMS_CancelReq() ;
extern int	process_CP_JobReq() ;
extern int 	ingest_IMS_msg() ;
extern int	ingest_CP_msg() ;
extern int	get_PPS_dbproc() ;
extern int 	convert_date_string() ;
extern int 	is_datetime() ;
extern char 	odl_timestring_to_seconds() ;
extern char 	seconds_to_odl_timestring() ;
extern int 	query_IMS_GHA() ;
extern int 	query_IMS_TCE() ;
extern int 	query_IMS_SVEC() ;
extern void 	get_current_time(char *) ;

extern int 	ingest_IMS_msg_mx() ;
extern int	ingest_CP_msg_mx() ;
extern void 	create_order_status_buf_mx() ;

extern void pps_logMsg();

extern Common_Header_Record 	*alloc_common_header_record();
extern IMS_CancelReq_Record 	*alloc_IMS_CancelReq_Record();
extern IMS_L1PReq_Record    	*alloc_IMS_L1PReq_Record();
extern IMS_ScanReq_Record   	*alloc_IMS_ScanReq_Record();
extern IMS_SVecAvail_Record   	*alloc_IMS_SVecAvail_Record();
extern CP_JobReq_Record 	*alloc_CP_JobReq_Record();
 

#endif _PPSEXTERN_
