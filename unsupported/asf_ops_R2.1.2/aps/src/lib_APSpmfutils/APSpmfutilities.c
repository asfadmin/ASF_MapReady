#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		PMF.c

Description:	Contains the functions for building the Product Metadata File
				information (PMF)

External Functions Defined:
				create_APSodl_common_hdr
				create_APSodl_catalog_metadata
				create_APSodl_detailed_metadata
				PMFcurrtime
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)APSpmfutilities.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APSpmfutils/SCCS/s.APSpmfutilities.c"

#include <stdio.h>          /* for fprintf ...                   */
#include <stdlib.h>         /* for free, getopt, ...                      */
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>         /* for strcmp, strncmp argument checks  */

#include "GENconversions.h" /* for fa global variables				*/
#include "APSpmfutilities.h"   /* for PMF definitions 					*/

   char PMF_start_rev[MAXLINE] ;
   char PMF_stop_rev[MAXLINE] ;
   char	PMF_source[MAXLINE] ;
   char PMF_dest[MAXLINE] ;
   char PMF_msg_type[MAXLINE] ;
   char PMF_antenna_id[MAXLINE] ;
   char PMF_response_to[MAXLINE] ;

   char PMF_file_name[MAXLINE] ;                 /* Name of file to archive */
   char PMF_format[MAXLINE] ;
   char PMF_file_source[MAXLINE] ;               /* File Source               */
   char PMF_number_of_records[MAXLINE];
   char PMF_file_dest[MAXLINE] ;                 /* File Destination          */
   char PMF_fa_file_type[MAXLINE] ;              /* FA File Type              */
   char PMF_gen_file_type[MAXLINE] ;             /* Generated File Type       */
   char PMF_satellite[MAXLINE] ;                 /* Satellite                 */

   char PMF_fa_createtime[TIME_STRING_LEN+1] ;   /* Time file created at FA   */
   char PMF_faif_arrivaltime[TIME_STRING_LEN+1] ;/* File Arrival Time         */
   char PMF_valid_starttime[TIME_STRING_LEN+1] ; /* Start of time bracket     */
   char PMF_valid_stoptime[TIME_STRING_LEN+1] ;  /* Start of time bracket     */
   char PMF_first_aos_time[TIME_STRING_LEN+1] ;  /* Earliest AOS time for file*/
   char PMF_last_los_time[TIME_STRING_LEN+1] ;   /* Latest LOS time for file  */



APSPMF_metadata PMF_struct_wreq[]=
{
/*
-- PMF HEADER instructions
--
*/
/* PMF_SOURCE      0*/  {PMF_source,    "APS",          NULL,   NULL},
/* PMF_DESTINATION 1*/  {PMF_dest,      "IMS",          NULL,   NULL},
/* MSG_TYPE        2
-- deposited in
-- commonrecord->file_type */
						{PMF_msg_type,	FAIF_PMF_NAME,  NULL,   NULL},
/* NUMBER_OF_RECORDS 3*/{PMF_number_of_records,
										"1",            NULL,   NULL},

/*
--
-- PMF AGGREGATE instructions
--
-- NOTE: A full instruction has a value assigned to each field.  
-- If field  = NULL , indicates there are no more instructions to process.
-- If string = NULL , the field will not be populated by APS_create_pmf()
--					  WARNING !! WARNING !! 
--					  This field MUST be populated elsewhere to prevent error.
-- If keyword= NULL , the entire instruction will not be processed.
-- If conversion = NULL, no effect.
*/

/* FILE_NAME      (4) is populated  elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
*/  
						{PMF_file_name,		NULL, 
                                        PMFKEYWD_FILENAME,  ODLConvertSymbol},
/* FILE_FORMAT     5*/  {PMF_format,        "ASF",
                                        PMFKEYWD_FORMAT,    ODLConvertSymbol},
/* FILE_SOURCE     6*/  {PMF_file_source,   "APS",
                                        PMFKEYWD_SRC,       ODLConvertSymbol},
/* FILE_DESTINATION7*/  {PMF_file_dest,     "WFF",
                                        PMFKEYWD_DEST,      ODLConvertSymbol},
/* FA_FILE_TYPE    8*/  {PMF_fa_file_type, "WALPS_AVAIL_REQ",
                                        PMFKEYWD_FA_FTYPE,  ODLConvertSymbol},
/* GEN_FILE_TYPE   9*/  {PMF_gen_file_type, "LONG_TERM_USER_REQUESTS",
                                        PMFKEYWD_GEN_FTYPE, ODLConvertSymbol},
/*
-- FILE_CREATION_TIME (10) is populated elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
--		(ie:keyword=PMFKEYWD_FILE_CREAT_TIME, conversion=ODLConvertDateTime)
*/
						{PMF_fa_createtime, NULL,
                                        PMFKEYWD_FILE_CREAT_TIME,
                                                            ODLConvertDateTime},
/* 
-- PLATFORM    (11) is optional for the WFF Request for Availability
*/
                        {PMF_satellite,		NULL,   NULL,	NULL},
/*
-- ANTENNA_ID  (12) is optional for the WFF Request for Availability
*/
                        {PMF_antenna_id,	NULL,   NULL,	NULL},
/*
-- ARRIVAL_TIME (13) at FAIF is not filled in here.  Otherwise, we would use:
                        {PMF_faif_arrivaltime, <string>,
                                    PMFKEYWD_ARRIVE_TIME,   ODLConvertDateTime},
*/
                        {PMF_faif_arrivaltime,	NULL,	NULL,	NULL},
 
/* RESPONSE_TO   (14) not used, because WFF Request for Availability
-- is not a response file 
*/
						{PMF_response_to,		NULL,    NULL,   NULL},

/* VALID_START_TIME (15)
   VALID_END_TIME   (16)
*/
						{PMF_valid_starttime,  fa_file_start_time,
                                        PMFKEYWD_VALID_START,
                                                            ODLConvertDateTime},
						{PMF_valid_stoptime,   fa_file_stop_time,
                                        PMFKEYWD_VALID_END, ODLConvertDateTime},

/* START_REV      (17) is optional for WFF Request for Availability
   END_REV        (18) is optional for WFF Request for Availability
*/
                        {PMF_start_rev,			NULL,   NULL,	NULL},
                        {PMF_stop_rev, 			NULL,   NULL,	NULL},

/* FIRST_AOS		(19) is optional for WFF Request for Availability
   LAST_LOS			(20) is optional for WFF Request for Availability
*/
                        {PMF_first_aos_time,	NULL,   NULL,	NULL},
                        {PMF_last_los_time, 	NULL,   NULL,	NULL},

                        {NULL,  NULL,   NULL,   NULL }
} ;


APSPMF_metadata PMF_struct_wstv[]=
{
/*
-- PMF HEADER instructions
--
*/
/* PMF_SOURCE      0*/  {PMF_source,    "APS",          NULL,   NULL},
/* PMF_DESTINATION 1*/  {PMF_dest,      "IMS",          NULL,   NULL},
/* MSG_TYPE        2
-- deposited in
-- commonrecord->file_type */
						{PMF_msg_type,	FAIF_PMF_NAME,  NULL,   NULL},
/* NUMBER_OF_RECORDS 3*/{PMF_number_of_records,
										"1",            NULL,   NULL},

/*
--
-- PMF AGGREGATE instructions
--
-- NOTE: A full instruction has a value assigned to each field.  
-- If field  = NULL , indicates there are no more instructions to process.
-- If string = NULL , the field will not be populated by APS_create_pmf()
--					  WARNING !! WARNING !! 
--					  This field MUST be populated elsewhere to prevent error.
-- If keyword= NULL , the entire instruction will not be processed.
-- If conversion = NULL, no effect.
*/

/* FILE_NAME      (4) is populated  elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
*/  
						{PMF_file_name,		NULL, 
                                        PMFKEYWD_FILENAME,  ODLConvertSymbol},
/* FILE_FORMAT     5*/  {PMF_format,        "ASF",
                                        PMFKEYWD_FORMAT,    ODLConvertSymbol},
/* FILE_SOURCE     6*/  {PMF_file_source,   "APS",
                                        PMFKEYWD_SRC,       ODLConvertSymbol},
/* FILE_DESTINATION7*/  {PMF_file_dest,     "WFF",
                                        PMFKEYWD_DEST,      ODLConvertSymbol},
/* FA_FILE_TYPE    8*/  {PMF_fa_file_type, "WALPS_STVEC",
                                        PMFKEYWD_FA_FTYPE,  ODLConvertSymbol},
/* GEN_FILE_TYPE   9*/  {PMF_gen_file_type, "STATE_VECTORS",
                                        PMFKEYWD_GEN_FTYPE, ODLConvertSymbol},
/*
-- FILE_CREATION_TIME (10) is populated elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
--		(ie:keyword=PMFKEYWD_FILE_CREAT_TIME, conversion=ODLConvertDateTime)
*/
						{PMF_fa_createtime, NULL,
                                        PMFKEYWD_FILE_CREAT_TIME,
                                                            ODLConvertDateTime},
/* 
-- PLATFORM    (11) is optional for the WFF State Vector File
*/
                        {PMF_satellite,		NULL,   NULL,	NULL},
/*
-- ANTENNA_ID  (12) is optional for the WFF State Vector File
*/
                        {PMF_antenna_id,	NULL,   NULL,	NULL},
/*
-- ARRIVAL_TIME (13) at FAIF is not filled in here.  Otherwise, we would use:
                        {PMF_faif_arrivaltime, <string>,
                                    PMFKEYWD_ARRIVE_TIME,   ODLConvertDateTime},
*/
                        {PMF_faif_arrivaltime,	NULL,	NULL,	NULL},
 
/* RESPONSE_TO   (14) not used, because WFF State Vector File  
-- is not a response file 
*/
						{PMF_response_to,		NULL,    NULL,   NULL},

/* VALID_START_TIME (15) is optional for WFF State Vectors file
   VALID_END_TIME   (16) is optional for WFF State Vectors file
*/
						{PMF_valid_starttime,		NULL,    NULL,   NULL},
						{PMF_valid_stoptime,		NULL,    NULL,   NULL},

/* START_REV      (17) is optional for WFF State Vectors file
   END_REV        (18) is optional for WFF State Vectors file
*/
                        {PMF_start_rev,			NULL,   NULL,	NULL},
                        {PMF_stop_rev, 			NULL,   NULL,	NULL},

/* FIRST_AOS		(19)
   LAST_LOS			(20)
*/
                        {PMF_first_aos_time,	NULL,   NULL,	NULL},
                        {PMF_last_los_time, 	NULL,   NULL,	NULL},

                        {NULL,  NULL,   NULL,   NULL }
} ;


APSPMF_metadata PMF_struct_wwos[]=
{
/*
-- PMF HEADER instructions
--
*/
/* PMF_SOURCE      0*/  {PMF_source,    "APS",          NULL,   NULL},
/* PMF_DESTINATION 1*/  {PMF_dest,      "IMS",          NULL,   NULL},
/* MSG_TYPE        2
-- deposited in
-- commonrecord->file_type */
						{PMF_msg_type,	FAIF_PMF_NAME,  NULL,   NULL},
/* NUMBER_OF_RECORDS 3*/{PMF_number_of_records,
										"1",            NULL,   NULL},

/*
--
-- PMF AGGREGATE instructions
--
-- NOTE: A full instruction has a value assigned to each field.  
-- If field  = NULL , indicates there are no more instructions to process.
-- If string = NULL , the field will not be populated by APS_create_pmf()
--					  WARNING !! WARNING !! 
--					  This field MUST be populated elsewhere to prevent error.
-- If keyword= NULL , the entire instruction will not be processed.
-- If conversion = NULL, no effect.
*/

/* FILE_NAME      (4) is populated  elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
*/  
						{PMF_file_name,		NULL, 
                                        PMFKEYWD_FILENAME,  ODLConvertSymbol},
/* FILE_FORMAT     5*/  {PMF_format,        "ASF",
                                        PMFKEYWD_FORMAT,    ODLConvertSymbol},
/* FILE_SOURCE     6*/  {PMF_file_source,   "APS",
                                        PMFKEYWD_SRC,       ODLConvertSymbol},
/* FILE_DESTINATION7*/  {PMF_file_dest,     "WFF",
                                        PMFKEYWD_DEST,      ODLConvertSymbol},
/* FA_FILE_TYPE    8*/  {PMF_fa_file_type, "WALPS_WOS",
                                        PMFKEYWD_FA_FTYPE,  ODLConvertSymbol},
/* GEN_FILE_TYPE   9*/  {PMF_gen_file_type, "WEEKLY_OPERATIONS_SCHEDULE",
                                        PMFKEYWD_GEN_FTYPE, ODLConvertSymbol},
/*
-- FILE_CREATION_TIME (10) is populated elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
--		(ie:keyword=PMFKEYWD_FILE_CREAT_TIME, conversion=ODLConvertDateTime)
*/
						{PMF_fa_createtime, NULL,
                                        PMFKEYWD_FILE_CREAT_TIME,
                                                            ODLConvertDateTime},
/* 
-- PLATFORM    (11) is optional for the WFF WOS Weekly Operation Schedule
*/
                        {PMF_satellite,		NULL,   NULL,	NULL},
/*
-- ANTENNA_ID  (12) is optional for WFF WOS Weekly Operation Schedule 
*/
                        {PMF_antenna_id,	NULL,   NULL,	NULL},
/*
-- ARRIVAL_TIME (13) at FAIF is not filled in here.  Otherwise, we would use:
                        {PMF_faif_arrivaltime, <string>,
                                    PMFKEYWD_ARRIVE_TIME,   ODLConvertDateTime},
*/
                        {PMF_faif_arrivaltime,	NULL,	NULL,	NULL},
 
/* RESPONSE_TO   (14) is not used, because WFF WOS is not a response file 
*/
						{PMF_response_to,		NULL,    NULL,   NULL},

/* VALID_START_TIME (15)	comes from fa_argument_start_time	
   VALID_END_TIME   (16)	comes from fa_argument_stop_time
*/
						{PMF_valid_starttime,  fa_argument_start_time,
                                        PMFKEYWD_VALID_START,
                                                            ODLConvertDateTime},
						{PMF_valid_stoptime,   fa_argument_stop_time,
                                        PMFKEYWD_VALID_END, ODLConvertDateTime},

/* START_REV      (17) is optional for WFF WOS Weekly Operation Schedule
   END_REV        (18) is optional for WFF WOS Weekly Operation Schedule
						{PMF_start_rev,         (char *)&fa_file_start_rev,
                                        PMFKEYWD_START_REV, ODLConvertInteger},
						{PMF_stop_rev, 			(char *)&fa_file_stop_rev,
                                        PMFKEYWD_END_REV,   ODLConvertInteger},
*/
                        {PMF_start_rev,			NULL,   NULL,	NULL},
                        {PMF_stop_rev, 			NULL,   NULL,	NULL},

/* FIRST_AOS		(19)
   LAST_LOS			(20)
*/
                        {PMF_first_aos_time,	NULL,   NULL,	NULL},
                        {PMF_last_los_time, 	NULL,   NULL,	NULL},

                        {NULL,  NULL,   NULL,   NULL }
} ;


APSPMF_metadata PMF_struct_hstv[]=
{
/*
-- PMF HEADER instructions
--
*/
/* PMF_SOURCE      0*/  {PMF_source,    "APS",          NULL,   NULL},
/* PMF_DESTINATION 1*/  {PMF_dest,      "IMS",          NULL,   NULL},
/* MSG_TYPE        2
-- deposited in
-- commonrecord->file_type */
						{PMF_msg_type,	FAIF_PMF_NAME,  NULL,   NULL},
/* NUMBER_OF_RECORDS 3*/{PMF_number_of_records,
										"1",            NULL,   NULL},

/*
--
-- PMF AGGREGATE instructions
--
-- NOTE: A full instruction has a value assigned to each field.  
-- If field  = NULL , indicates there are no more instructions to process.
-- If string = NULL , the field will not be populated by APS_create_pmf()
--					  WARNING !! WARNING !! 
--					  This field MUST be populated elsewhere to prevent error.
-- If keyword= NULL , the entire instruction will not be processed.
-- If conversion = NULL, no effect.
*/

/* FILE_NAME      (4) is populated  elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
*/  
						{PMF_file_name,		NULL, 
                                        PMFKEYWD_FILENAME,  ODLConvertSymbol},
/* FILE_FORMAT     5*/  {PMF_format,        "ASF",
                                        PMFKEYWD_FORMAT,    ODLConvertSymbol},
/* FILE_SOURCE     6*/  {PMF_file_source,   "APS",
                                        PMFKEYWD_SRC,       ODLConvertSymbol},
/* FILE_DESTINATION7*/  {PMF_file_dest,     "HC",
                                        PMFKEYWD_DEST,      ODLConvertSymbol},
/* FA_FILE_TYPE    8*/  {PMF_fa_file_type, "ASF_STVEC",
                                        PMFKEYWD_FA_FTYPE,  ODLConvertSymbol},
/* GEN_FILE_TYPE   9*/  {PMF_gen_file_type, "STATE_VECTORS",
                                        PMFKEYWD_GEN_FTYPE, ODLConvertSymbol},
/*
-- FILE_CREATION_TIME (10) is populated elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
--		(ie:keyword=PMFKEYWD_FILE_CREAT_TIME, conversion=ODLConvertDateTime)
*/
						{PMF_fa_createtime, NULL,
                                        PMFKEYWD_FILE_CREAT_TIME,
                                                            ODLConvertDateTime},
/* 
-- PLATFORM    (11) is optional for the ASF State Vector File
*/
                        {PMF_satellite,		NULL,   NULL,	NULL},
/*
-- ANTENNA_ID  (12) is optional for the ASF State Vector File
*/
                        {PMF_antenna_id,	NULL,   NULL,	NULL},
/*
-- ARRIVAL_TIME (13) at FAIF is not filled in here.  Otherwise, we would use:
                        {PMF_faif_arrivaltime, <string>,
                                    PMFKEYWD_ARRIVE_TIME,   ODLConvertDateTime},
*/
                        {PMF_faif_arrivaltime,	NULL,	NULL,	NULL},
 
/* RESPONSE_TO   (14) not used, because ASF State Vector File  
-- is not a response file 
*/
						{PMF_response_to,		NULL,    NULL,   NULL},

/* VALID_START_TIME (15) is optional for ASF State Vectors file
   VALID_END_TIME   (16) is optional for ASF State Vectors file
*/
						{PMF_valid_starttime,		NULL,    NULL,   NULL},
						{PMF_valid_stoptime,		NULL,    NULL,   NULL},

/* START_REV      (17) is optional for ASF State Vectors file
   END_REV        (18) is optional for ASF State Vectors file
*/
                        {PMF_start_rev,			NULL,   NULL,	NULL},
                        {PMF_stop_rev, 			NULL,   NULL,	NULL},

/* FIRST_AOS		(19)
   LAST_LOS			(20)
*/
                        {PMF_first_aos_time,	NULL,   NULL,	NULL},
                        {PMF_last_los_time, 	NULL,   NULL,	NULL},

                        {NULL,  NULL,   NULL,   NULL }
} ;


APSPMF_metadata PMF_struct_hwos[]=
{
/*
-- PMF HEADER instructions
--
*/
/* PMF_SOURCE      0*/  {PMF_source,    "APS",          NULL,   NULL},
/* PMF_DESTINATION 1*/  {PMF_dest,      "IMS",          NULL,   NULL},
/* MSG_TYPE        2
-- deposited in
-- commonrecord->file_type */
						{PMF_msg_type,	FAIF_PMF_NAME,  NULL,   NULL},
/* NUMBER_OF_RECORDS 3*/{PMF_number_of_records,
										"1",            NULL,   NULL},

/*
--
-- PMF AGGREGATE instructions
--
-- NOTE: A full instruction has a value assigned to each field.  
-- If field  = NULL , indicates there are no more instructions to process.
-- If string = NULL , the field will not be populated by APS_create_pmf()
--					  WARNING !! WARNING !! 
--					  This field MUST be populated elsewhere to prevent error.
-- If keyword= NULL , the entire instruction will not be processed.
-- If conversion = NULL, no effect.
*/

/* FILE_NAME      (4) is populated  elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
*/  
						{PMF_file_name,		NULL, 
                                        PMFKEYWD_FILENAME,  ODLConvertSymbol},
/* FILE_FORMAT     5*/  {PMF_format,        "ASF",
                                        PMFKEYWD_FORMAT,    ODLConvertSymbol},
/* FILE_SOURCE     6*/  {PMF_file_source,   "APS",
                                        PMFKEYWD_SRC,       ODLConvertSymbol},
/* FILE_DESTINATION7*/  {PMF_file_dest,     "HC",
                                        PMFKEYWD_DEST,      ODLConvertSymbol},
/* FA_FILE_TYPE    8*/  {PMF_fa_file_type, "ASF_WOS",
                                        PMFKEYWD_FA_FTYPE,  ODLConvertSymbol},
/* GEN_FILE_TYPE   9*/  {PMF_gen_file_type, "WEEKLY_OPERATIONS_SCHEDULE",
                                        PMFKEYWD_GEN_FTYPE, ODLConvertSymbol},
/*
-- FILE_CREATION_TIME (10) is populated elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
--		(ie:keyword=PMFKEYWD_FILE_CREAT_TIME, conversion=ODLConvertDateTime)
*/
						{PMF_fa_createtime, NULL,
                                        PMFKEYWD_FILE_CREAT_TIME,
                                                            ODLConvertDateTime},
/* 
-- PLATFORM    (11) is optional for the ASF WOS Weekly Operation Schedule
*/
                        {PMF_satellite,		NULL,   NULL,	NULL},
/*
-- ANTENNA_ID  (12) is optional for ASF WOS Weekly Operation Schedule 
*/
                        {PMF_antenna_id,	NULL,   NULL,	NULL},
/*
-- ARRIVAL_TIME (13) at FAIF is not filled in here.  Otherwise, we would use:
                        {PMF_faif_arrivaltime, <string>,
                                    PMFKEYWD_ARRIVE_TIME,   ODLConvertDateTime},
*/
                        {PMF_faif_arrivaltime,	NULL,	NULL,	NULL},
 
/* RESPONSE_TO   (14) is not used, because ASF WOS is not a response file 
*/
						{PMF_response_to,		NULL,    NULL,   NULL},

/* VALID_START_TIME (15)	
   VALID_END_TIME   (16)
*/
						{PMF_valid_starttime,  fa_argument_start_time,
                                        PMFKEYWD_VALID_START,
                                                            ODLConvertDateTime},
						{PMF_valid_stoptime,   fa_argument_stop_time,
                                        PMFKEYWD_VALID_END, ODLConvertDateTime},

/* START_REV      (17) is optional for ASF WOS Weekly Operation Schedule
   END_REV        (18) is optional for ASF WOS Weekly Operation Schedule
						{PMF_start_rev,         (char *)&fa_file_start_rev,
                                        PMFKEYWD_START_REV, ODLConvertInteger},
						{PMF_stop_rev, 			(char *)&fa_file_stop_rev,
                                        PMFKEYWD_END_REV,   ODLConvertInteger},
*/
                        {PMF_start_rev,			NULL,   NULL,	NULL},
                        {PMF_stop_rev, 			NULL,   NULL,	NULL},

/* FIRST_AOS		(19)
   LAST_LOS			(20)
*/
						{PMF_first_aos_time,  	fa_first_track_start,
                                    PMFKEYWD_FIRST_AOS,		ODLConvertDateTime},
						{PMF_last_los_time,  	fa_last_track_end,
                                    PMFKEYWD_LAST_LOS,		ODLConvertDateTime},

                        {NULL,  NULL,   NULL,   NULL }
} ;


APSPMF_metadata PMF_struct_crar[]=
{
/*
-- PMF HEADER instructions
--
*/
/* PMF_SOURCE      0*/  {PMF_source,    "APS",          NULL,   NULL},
/* PMF_DESTINATION 1*/  {PMF_dest,      "IMS",          NULL,   NULL},
/* MSG_TYPE        2
-- deposited in
-- commonrecord->file_type */
						{PMF_msg_type,	FAIF_PMF_NAME,  NULL,   NULL},
/* NUMBER_OF_RECORDS 3*/{PMF_number_of_records,
										"1",            NULL,   NULL},

/*
--
-- PMF AGGREGATE instructions
--
-- NOTE: A full instruction has a value assigned to each field.  
-- If field  = NULL , indicates there are no more instructions to process.
-- If string = NULL , the field will not be populated by APS_create_pmf()
--					  WARNING !! WARNING !! 
--					  This field MUST be populated elsewhere to prevent error.
-- If keyword= NULL , the entire instruction will not be processed.
-- If conversion = NULL, no effect.
*/

/* FILE_NAME      (4) is populated  elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
*/  
						{PMF_file_name,		NULL, 
                                        PMFKEYWD_FILENAME,  ODLConvertSymbol},
/* FILE_FORMAT     5*/  {PMF_format,        "ASF",
                                        PMFKEYWD_FORMAT,    ODLConvertSymbol},
/* FILE_SOURCE     6*/  {PMF_file_source,   "APS",
                                        PMFKEYWD_SRC,       ODLConvertSymbol},
/* FILE_DESTINATION7*/  {PMF_file_dest,     "CSA",
                                        PMFKEYWD_DEST,      ODLConvertSymbol},
/* FA_FILE_TYPE    8*/  {PMF_fa_file_type, "CSA_RECAVAILRPT",
                                        PMFKEYWD_FA_FTYPE,  ODLConvertSymbol},
/* GEN_FILE_TYPE   9*/  {PMF_gen_file_type, "STATION_STATUS",
                                        PMFKEYWD_GEN_FTYPE, ODLConvertSymbol},
/*
-- FILE_CREATION_TIME (10) is populated elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
--		(ie:keyword=PMFKEYWD_FILE_CREAT_TIME, conversion=ODLConvertDateTime)
*/
						{PMF_fa_createtime, NULL,
                                        PMFKEYWD_FILE_CREAT_TIME,
                                                            ODLConvertDateTime},
/* PLATFORM      (11) */
						{PMF_satellite,     fa_sat_id,
                                        PMFKEYWD_PLATFORM,  ODLConvertSymbol},
/*
-- ANTENNA_ID  (12) is optional for CSA Reception Availability Report.
*/
                        {PMF_antenna_id,		NULL,   NULL,	NULL},
/*
-- ARRIVAL_TIME (13) at FAIF is not filled in here.  Otherwise, we would use:
                        {PMF_faif_arrivaltime, <string>,
                                    PMFKEYWD_ARRIVE_TIME,   ODLConvertDateTime},
*/
                        {PMF_faif_arrivaltime,	NULL,	NULL,	NULL},
 
/* RESPONSE_TO   (14) is not used, because CSA Reception Availability Report 
-- is not a response file 
*/
						{PMF_response_to,		NULL,    NULL,   NULL},

/* VALID_START_TIME (15)	
   VALID_END_TIME   (16)
*/
						{PMF_valid_starttime,  fa_file_start_time,
                                        PMFKEYWD_VALID_START,
                                                            ODLConvertDateTime},
						{PMF_valid_stoptime,   fa_file_stop_time,
                                        PMFKEYWD_VALID_END, ODLConvertDateTime},

/* START_REV      (17) is optional for CSA Reception Availability Report
   END_REV        (18) is optional for CSA Reception Availability Report
						{PMF_start_rev,         (char *)&fa_file_start_rev,
                                        PMFKEYWD_START_REV, ODLConvertInteger},
						{PMF_stop_rev, 			(char *)&fa_file_stop_rev,
                                        PMFKEYWD_END_REV,   ODLConvertInteger},
*/
                        {PMF_start_rev,			NULL,   NULL,	NULL},
                        {PMF_stop_rev, 			NULL,   NULL,	NULL},

/* FIRST_AOS		(19)
   LAST_LOS			(20)
*/
                        {PMF_first_aos_time,	NULL,   NULL,	NULL},
                        {PMF_last_los_time, 	NULL,   NULL,	NULL},

                        {NULL,  NULL,   NULL,   NULL }
} ;


APSPMF_metadata PMF_struct_stgs[]=
{
/*
-- PMF HEADER instructions
--
*/
/* PMF_SOURCE      0*/  {PMF_source,    "APS",          NULL,   NULL},
/* PMF_DESTINATION 1*/  {PMF_dest,      "IMS",          NULL,   NULL},
/* MSG_TYPE        2
-- deposited in
-- commonrecord->file_type */
						{PMF_msg_type,	FAIF_PMF_NAME,  NULL,   NULL},
/* NUMBER_OF_RECORDS 3*/{PMF_number_of_records,
										"1",            NULL,   NULL},

/*
--
-- PMF AGGREGATE instructions
--
-- NOTE: A full instruction has a value assigned to each field.  
-- If field  = NULL , indicates there are no more instructions to process.
-- If string = NULL , the field will not be populated by APS_create_pmf()
--					  WARNING !! WARNING !! 
--					  This field MUST be populated elsewhere to prevent error.
-- If keyword= NULL , the entire instruction will not be processed.
-- If conversion = NULL, no effect.
*/

/* FILE_NAME      (4) is populated  elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
*/  
						{PMF_file_name,		NULL, 
                                        PMFKEYWD_FILENAME,  ODLConvertSymbol},
/* FILE_FORMAT     5*/  {PMF_format,        "ASF",
                                        PMFKEYWD_FORMAT,    ODLConvertSymbol},
/* FILE_SOURCE     6*/  {PMF_file_source,   "APS",
                                        PMFKEYWD_SRC,       ODLConvertSymbol},
/* FILE_DESTINATION7*/  {PMF_file_dest,     "ADEOS",
                                        PMFKEYWD_DEST,      ODLConvertSymbol},
/* FA_FILE_TYPE    8*/  {PMF_fa_file_type, "STGS",
                                        PMFKEYWD_FA_FTYPE,  ODLConvertSymbol},
/* GEN_FILE_TYPE   9*/  {PMF_gen_file_type, "REPLY_TO_FLIGHT_AGENCY_REQUEST",
                                        PMFKEYWD_GEN_FTYPE, ODLConvertSymbol},
/*
-- FILE_CREATION_TIME (10) is populated elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
--		(ie:keyword=PMFKEYWD_FILE_CREAT_TIME, conversion=ODLConvertDateTime)
*/
						{PMF_fa_createtime, NULL,
                                        PMFKEYWD_FILE_CREAT_TIME,
                                                            ODLConvertDateTime},
/* PLATFORM      (11) */
						{PMF_satellite,     fa_sat_id,
                                        PMFKEYWD_PLATFORM,  ODLConvertSymbol},
/*
-- ANTENNA_ID  (12) is optional for STGS Response to REQR File.
*/
                        {PMF_antenna_id,		NULL,   NULL,	NULL},
/*
-- ARRIVAL_TIME (13) at FAIF is not filled in here.  Otherwise, we would use:
                        {PMF_faif_arrivaltime, <string>,
                                    PMFKEYWD_ARRIVE_TIME,   ODLConvertDateTime},
*/
                        {PMF_faif_arrivaltime,	NULL,	NULL,	NULL},
 
/* RESPONSE_TO   (14) is populated elsewhere.
--              However, we still carry the information fields needed
--              by the create_APSodl_catalog_metadata routine
*/
						{PMF_response_to,		NULL,
                                        PMFKEYWD_RESPONSE,  ODLConvertSymbol},

/* VALID_START_TIME 15	is optional for STGS Response to REQR File
   VALID_END_TIME   16	is optional for STGS Response to REQR File	
						{PMF_valid_starttime,  fa_file_start_time,
                                        PMFKEYWD_VALID_START,
                                                            ODLConvertDateTime},
						{PMF_valid_stoptime,   fa_file_stop_time,
                                        PMFKEYWD_VALID_END, ODLConvertDateTime},
*/
                        {PMF_valid_starttime,	NULL,	NULL,	NULL},
                        {PMF_valid_stoptime,	NULL,	NULL,	NULL},

/* START_REV      (17) is optional for STGS Response to REQR File
   END_REV        (18) is optional for STGS Response to REQR File
						{PMF_start_rev,         (char *)&fa_file_start_rev,
                                        PMFKEYWD_START_REV, ODLConvertInteger},
						{PMF_stop_rev, 			(char *)&fa_file_stop_rev,
                                        PMFKEYWD_END_REV,   ODLConvertInteger},
*/
                        {PMF_start_rev,			NULL,   NULL,	NULL},
                        {PMF_stop_rev, 			NULL,   NULL,	NULL},

/* FIRST_AOS		(19)
   LAST_LOS			(20)
*/
                        {PMF_first_aos_time,	NULL,   NULL,	NULL},
                        {PMF_last_los_time, 	NULL,   NULL,	NULL},

                        {NULL,  NULL,   NULL,   NULL }
} ;


APSPMF_metadata PMF_struct_msge[]=
{
/*
-- PMF HEADER instructions
--
*/
/* PMF_SOURCE      0*/  {PMF_source,    "APS",          NULL,   NULL},
/* PMF_DESTINATION 1*/  {PMF_dest,      "IMS",          NULL,   NULL},
/* MSG_TYPE        2
-- deposited in
-- commonrecord->file_type */
						{PMF_msg_type,	FAIF_PMF_NAME,  NULL,   NULL},
/* NUMBER_OF_RECORDS 3*/{PMF_number_of_records,
										"1",            NULL,   NULL},

/*
--
-- PMF AGGREGATE instructions
--
-- NOTE: A full instruction has a value assigned to each field.  
-- If field  = NULL , indicates there are no more instructions to process.
-- If string = NULL , the field will not be populated by APS_create_pmf()
--					  WARNING !! WARNING !! 
--					  This field MUST be populated elsewhere to prevent error.
-- If keyword= NULL , the entire instruction will not be processed.
-- If conversion = NULL, no effect.
*/

/* FILE_NAME      (4) is populated  elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
*/  
						{PMF_file_name,		NULL, 
                                        PMFKEYWD_FILENAME,  ODLConvertSymbol},
/* FILE_FORMAT     5*/  {PMF_format,        "ASF",
                                        PMFKEYWD_FORMAT,    ODLConvertSymbol},
/* FILE_SOURCE     6*/  {PMF_file_source,   "APS",
                                        PMFKEYWD_SRC,       ODLConvertSymbol},
/* FILE_DESTINATION7*/  {PMF_file_dest,     "NASDA",
                                        PMFKEYWD_DEST,      ODLConvertSymbol},
/* FA_FILE_TYPE    8*/  {PMF_fa_file_type, "MSGE",
                                        PMFKEYWD_FA_FTYPE,  ODLConvertSymbol},
/* GEN_FILE_TYPE   9*/  {PMF_gen_file_type, "REPLY_TO_FLIGHT_AGENCY_REQUEST",
                                        PMFKEYWD_GEN_FTYPE, ODLConvertSymbol},
/*
-- FILE_CREATION_TIME (10) is populated elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
--		(ie:keyword=PMFKEYWD_FILE_CREAT_TIME, conversion=ODLConvertDateTime)
*/
						{PMF_fa_createtime, NULL,
                                        PMFKEYWD_FILE_CREAT_TIME,
                                                            ODLConvertDateTime},
/* PLATFORM      (11) */
						{PMF_satellite,     fa_sat_id,
                                        PMFKEYWD_PLATFORM,  ODLConvertSymbol},
/*
-- ANTENNA_ID  (12) is optional for MSGE Response to REQM File.
*/
                        {PMF_antenna_id,		NULL,   NULL,	NULL},
/*
-- ARRIVAL_TIME (13) at FAIF is not filled in here.  Otherwise, we would use:
                        {PMF_faif_arrivaltime, <string>,
                                    PMFKEYWD_ARRIVE_TIME,   ODLConvertDateTime},
*/
                        {PMF_faif_arrivaltime,	NULL,	NULL,	NULL},
 
/* RESPONSE_TO   (14) is populated elsewhere.
--              However, we still carry the information fields needed
--              by the create_APSodl_catalog_metadata routine
*/
						{PMF_response_to,		NULL,
                                        PMFKEYWD_RESPONSE,  ODLConvertSymbol},

/* VALID_START_TIME 15	is optional for MSGE Response to REQM File
   VALID_END_TIME   16	is optional for MSGE Response to REQM File	
						{PMF_valid_starttime,  fa_file_start_time,
                                        PMFKEYWD_VALID_START,
                                                            ODLConvertDateTime},
						{PMF_valid_stoptime,   fa_file_stop_time,
                                        PMFKEYWD_VALID_END, ODLConvertDateTime},
*/
                        {PMF_valid_starttime,	NULL,	NULL,	NULL},
                        {PMF_valid_stoptime,	NULL,	NULL,	NULL},

/* START_REV      (17) is optional for MSGE Response to REQM File
   END_REV        (18) is optional for MSGE Response to REQM File
						{PMF_start_rev,         (char *)&fa_file_start_rev,
                                        PMFKEYWD_START_REV, ODLConvertInteger},
						{PMF_stop_rev, 			(char *)&fa_file_stop_rev,
                                        PMFKEYWD_END_REV,   ODLConvertInteger},
*/
                        {PMF_start_rev,			NULL,   NULL,	NULL},
                        {PMF_stop_rev, 			NULL,   NULL,	NULL},

/* FIRST_AOS		(19)
   LAST_LOS			(20)
*/
                        {PMF_first_aos_time,	NULL,   NULL,	NULL},
                        {PMF_last_los_time, 	NULL,   NULL,	NULL},

                        {NULL,  NULL,   NULL,   NULL }
} ;


APSPMF_metadata PMF_struct_msgf[]=
{
/*
-- PMF HEADER instructions
--
*/
/* PMF_SOURCE      0*/  {PMF_source,    "APS",          NULL,   NULL},
/* PMF_DESTINATION 1*/  {PMF_dest,      "IMS",          NULL,   NULL},
/* MSG_TYPE        2
-- deposited in
-- commonrecord->file_type */
						{PMF_msg_type,	FAIF_PMF_NAME,  NULL,   NULL},
/* NUMBER_OF_RECORDS 3*/{PMF_number_of_records,
										"1",            NULL,   NULL},

/*
--
-- PMF AGGREGATE instructions
--
-- NOTE: A full instruction has a value assigned to each field.  
-- If field  = NULL , indicates there are no more instructions to process.
-- If string = NULL , the field will not be populated by APS_create_pmf()
--					  WARNING !! WARNING !! 
--					  This field MUST be populated elsewhere to prevent error.
-- If keyword= NULL , the entire instruction will not be processed.
-- If conversion = NULL, no effect.
*/

/* FILE_NAME      (4) is populated  elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
*/  
						{PMF_file_name,		NULL, 
                                        PMFKEYWD_FILENAME,  ODLConvertSymbol},
/* FILE_FORMAT     5*/  {PMF_format,        "ASF",
                                        PMFKEYWD_FORMAT,    ODLConvertSymbol},
/* FILE_SOURCE     6*/  {PMF_file_source,   "APS",
                                        PMFKEYWD_SRC,       ODLConvertSymbol},
/* FILE_DESTINATION7*/  {PMF_file_dest,     "NASDA",
                                        PMFKEYWD_DEST,      ODLConvertSymbol},
/* FA_FILE_TYPE    8*/  {PMF_fa_file_type, "MSGF",
                                        PMFKEYWD_FA_FTYPE,  ODLConvertSymbol},
/* GEN_FILE_TYPE   9*/  {PMF_gen_file_type, "STATION_STATUS",
                                        PMFKEYWD_GEN_FTYPE, ODLConvertSymbol},
/*
-- FILE_CREATION_TIME (10) is populated elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
--		(ie:keyword=PMFKEYWD_FILE_CREAT_TIME, conversion=ODLConvertDateTime)
*/
						{PMF_fa_createtime, NULL,
                                        PMFKEYWD_FILE_CREAT_TIME,
                                                            ODLConvertDateTime},
/* PLATFORM      (11) is optional for MSGF Station Status File
						{PMF_satellite,     fa_sat_id,
                                        PMFKEYWD_PLATFORM,  ODLConvertSymbol},
*/
                        {PMF_satellite,			NULL,   NULL,	NULL},
/*
-- ANTENNA_ID  (12) is not reported in the MSGF Station Status file.
*/
                        {PMF_antenna_id,		NULL,   NULL,	NULL},
/*
-- ARRIVAL_TIME (13) at FAIF is not filled in here.  Otherwise, we would use:
                        {PMF_faif_arrivaltime, <string>,
                                    PMFKEYWD_ARRIVE_TIME,   ODLConvertDateTime},
*/
                        {PMF_faif_arrivaltime, NULL,	NULL,	NULL},
 
/* RESPONSE_TO   (14) is not used, because MSGF is not a response file 
						{PMF_response_to,      (char *) &fa_filename,
                                        PMFKEYWD_RESPONSE,  ODLConvertSymbol},
*/
						{PMF_response_to,		NULL,    NULL,   NULL},

/* VALID_START_TIME 15*/{PMF_valid_starttime,  fa_file_start_time,
                                        PMFKEYWD_VALID_START,
                                                            ODLConvertDateTime},
/* VALID_END_TIME  16*/ {PMF_valid_stoptime,   fa_file_stop_time,
                                        PMFKEYWD_VALID_END, ODLConvertDateTime},

/* START_REV      (17) is optional for MSGF Station Status File
   END_REV        (18) is optional for MSGF Station Status File
						{PMF_start_rev,         (char *)&fa_file_start_rev,
                                        PMFKEYWD_START_REV, ODLConvertInteger},
						{PMF_stop_rev, 			(char *)&fa_file_stop_rev,
                                        PMFKEYWD_END_REV,   ODLConvertInteger},
*/
                        {PMF_start_rev,			NULL,   NULL,	NULL},
                        {PMF_stop_rev, 			NULL,   NULL,	NULL},

/* FIRST_AOS		(19)
   LAST_LOS			(20)
*/
                        {PMF_first_aos_time,	NULL,   NULL,	NULL},
                        {PMF_last_los_time, 	NULL,   NULL,	NULL},

                        {NULL,  NULL,   NULL,   NULL }
} ;


APSPMF_metadata PMF_struct_reqw[]=
{
/*
-- PMF HEADER instructions
--
*/
/* PMF_SOURCE      0*/  {PMF_source,    "APS",          NULL,   NULL},
/* PMF_DESTINATION 1*/  {PMF_dest,      "IMS",          NULL,   NULL},
/* MSG_TYPE        2
-- deposited in
-- commonrecord->file_type */
						{PMF_msg_type,	FAIF_PMF_NAME,  NULL,   NULL},
/* NUMBER_OF_RECORDS 3*/{PMF_number_of_records,
										"1",            NULL,   NULL},

/*
--
-- PMF AGGREGATE instructions
--
-- NOTE: A full instruction has a value assigned to each field.  
-- If field  = NULL , indicates there are no more instructions to process.
-- If string = NULL , the field will not be populated by APS_create_pmf()
--					  WARNING !! WARNING !! 
--					  This field MUST be populated elsewhere to prevent error.
-- If keyword= NULL , the entire instruction will not be processed.
-- If conversion = NULL, no effect.
*/

/* FILE_NAME      (4) is populated  elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
*/  
						{PMF_file_name,		NULL, 
                                        PMFKEYWD_FILENAME,  ODLConvertSymbol},
/* FILE_FORMAT     5*/  {PMF_format,        "ASF",
                                        PMFKEYWD_FORMAT,    ODLConvertSymbol},
/* FILE_SOURCE     6*/  {PMF_file_source,   "APS",
                                        PMFKEYWD_SRC,       ODLConvertSymbol},
/* FILE_DESTINATION7*/  {PMF_file_dest,     "NASDA",
                                        PMFKEYWD_DEST,      ODLConvertSymbol},
/* FA_FILE_TYPE    8*/  {PMF_fa_file_type, "REQW",
                                        PMFKEYWD_FA_FTYPE,  ODLConvertSymbol},
/* GEN_FILE_TYPE   9*/  {PMF_gen_file_type, "SHORT_TERM_USER_REQUESTS",
                                        PMFKEYWD_GEN_FTYPE, ODLConvertSymbol},
/*
-- FILE_CREATION_TIME (10) is populated elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
--		(ie:keyword=PMFKEYWD_FILE_CREAT_TIME, conversion=ODLConvertDateTime)
*/
						{PMF_fa_createtime, NULL,
                                        PMFKEYWD_FILE_CREAT_TIME,
                                                            ODLConvertDateTime},
/* PLATFORM      (11) */{PMF_satellite,     fa_sat_id,
                                        PMFKEYWD_PLATFORM,  ODLConvertSymbol},
/*
-- ANTENNA_ID  (12) is not reported in the REQW Weekly Request File.
*/
                        {PMF_antenna_id,		NULL,   NULL,	NULL},
/*
-- ARRIVAL_TIME (13) at FAIF is not filled in here.  Otherwise, we would use:
                        {PMF_faif_arrivaltime, <string>,
                                    PMFKEYWD_ARRIVE_TIME,   ODLConvertDateTime},
*/
                        {PMF_faif_arrivaltime, NULL,	NULL,	NULL},
 
/* RESPONSE_TO   (14) is not used, because REQW is not a response file 
						{PMF_response_to,      (char *) &fa_filename,
                                        PMFKEYWD_RESPONSE,  ODLConvertSymbol},
*/
						{PMF_response_to,		NULL,    NULL,   NULL},

/* VALID_START_TIME 15*/{PMF_valid_starttime,  fa_file_start_time,
                                        PMFKEYWD_VALID_START,
                                                            ODLConvertDateTime},
/* VALID_END_TIME  16*/ {PMF_valid_stoptime,   fa_file_stop_time,
                                        PMFKEYWD_VALID_END, ODLConvertDateTime},

/* START_REV      (17)*/{PMF_start_rev,         (char *)&fa_file_start_rev,
                                        PMFKEYWD_START_REV, ODLConvertInteger},
/* END_REV		  (18)*/{PMF_stop_rev, 			(char *)&fa_file_stop_rev,
                                        PMFKEYWD_END_REV,   ODLConvertInteger},

/* FIRST_AOS		(19)
   LAST_LOS			(20)
*/
                        {PMF_first_aos_time,	NULL,   NULL,	NULL},
                        {PMF_last_los_time, 	NULL,   NULL,	NULL},

                        {NULL,  NULL,   NULL,   NULL }
} ;


APSPMF_metadata PMF_struct_reqq[]=
{
/*
-- PMF HEADER instructions
--
*/
/* PMF_SOURCE      0*/  {PMF_source,    "APS",          NULL,   NULL},
/* PMF_DESTINATION 1*/  {PMF_dest,      "IMS",          NULL,   NULL},
/* MSG_TYPE        2
-- deposited in
-- commonrecord->file_type */
						{PMF_msg_type,	FAIF_PMF_NAME,  NULL,   NULL},
/* NUMBER_OF_RECORDS 3*/{PMF_number_of_records,
										"1",            NULL,   NULL},

/*
--
-- PMF AGGREGATE instructions
--
-- NOTE: A full instruction has a value assigned to each field.  
-- If field  = NULL , indicates there are no more instructions to process.
-- If string = NULL , the field will not be populated by APS_create_pmf()
--					  WARNING !! WARNING !! 
--					  This field MUST be populated elsewhere to prevent error.
-- If keyword= NULL , the entire instruction will not be processed.
-- If conversion = NULL, no effect.
*/

/* FILE_NAME      (4) is populated  elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
*/  
						{PMF_file_name,		NULL, 
                                        PMFKEYWD_FILENAME,  ODLConvertSymbol},
/* FILE_FORMAT     5*/  {PMF_format,        "ASF",
                                        PMFKEYWD_FORMAT,    ODLConvertSymbol},
/* FILE_SOURCE     6*/  {PMF_file_source,   "APS",
                                        PMFKEYWD_SRC,       ODLConvertSymbol},
/* FILE_DESTINATION7*/  {PMF_file_dest,     "NASDA",
                                        PMFKEYWD_DEST,      ODLConvertSymbol},
/* FA_FILE_TYPE    8*/  {PMF_fa_file_type, "REQQ",
                                        PMFKEYWD_FA_FTYPE,  ODLConvertSymbol},
/* GEN_FILE_TYPE   9*/  {PMF_gen_file_type, "LONG_TERM_USER_REQUESTS",
                                        PMFKEYWD_GEN_FTYPE, ODLConvertSymbol},
/*
-- FILE_CREATION_TIME (10) is populated elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
--		(ie:keyword=PMFKEYWD_FILE_CREAT_TIME, conversion=ODLConvertDateTime)
*/
						{PMF_fa_createtime, NULL,
                                        PMFKEYWD_FILE_CREAT_TIME,
                                                            ODLConvertDateTime},
/* PLATFORM      (11) */{PMF_satellite,     fa_sat_id,
                                        PMFKEYWD_PLATFORM,  ODLConvertSymbol},
/*
-- ANTENNA_ID  (12) is not reported in the REQQ Quarterly Request File.
*/
                        {PMF_antenna_id,		NULL,   NULL,	NULL},
/*
-- ARRIVAL_TIME (13) at FAIF is not filled in here.  Otherwise, we would use:
                        {PMF_faif_arrivaltime, <string>,
                                    PMFKEYWD_ARRIVE_TIME,   ODLConvertDateTime},
*/
                        {PMF_faif_arrivaltime, NULL,	NULL,	NULL},
 
/* RESPONSE_TO   (14) is not used, because REQQ is not a response file 
						{PMF_response_to,      (char *) &fa_filename,
                                        PMFKEYWD_RESPONSE,  ODLConvertSymbol},
*/
						{PMF_response_to,		NULL,    NULL,   NULL},

/* VALID_START_TIME 15*/{PMF_valid_starttime,  fa_file_start_time,
                                        PMFKEYWD_VALID_START,
                                                            ODLConvertDateTime},
/* VALID_END_TIME  16*/ {PMF_valid_stoptime,   fa_file_stop_time,
                                        PMFKEYWD_VALID_END, ODLConvertDateTime},

/* START_REV      (17)*/{PMF_start_rev,         (char *)&fa_file_start_rev,
                                        PMFKEYWD_START_REV, ODLConvertInteger},
/* END_REV		  (18)*/{PMF_stop_rev, 			(char *)&fa_file_stop_rev,
                                        PMFKEYWD_END_REV,   ODLConvertInteger},

/* FIRST_AOS		(19)
   LAST_LOS			(20)
*/
                        {PMF_first_aos_time,	NULL,   NULL,	NULL},
                        {PMF_last_los_time, 	NULL,   NULL,	NULL},

                        {NULL,  NULL,   NULL,   NULL }
} ;


APSPMF_metadata PMF_struct_reug[]=
{
/*
-- PMF HEADER instructions
--
*/
/* PMF_SOURCE      0*/  {PMF_source,    "APS",          NULL,   NULL},
/* PMF_DESTINATION 1*/  {PMF_dest,      "IMS",          NULL,   NULL},
/* MSG_TYPE        2
-- deposited in
-- commonrecord->file_type */
						{PMF_msg_type,	FAIF_PMF_NAME,  NULL,   NULL},
/* NUMBER_OF_RECORDS 3*/{PMF_number_of_records,
										"1",            NULL,   NULL},

/*
--
-- PMF AGGREGATE instructions
--
-- NOTE: A full instruction has a value assigned to each field.  
-- If field  = NULL , indicates there are no more instructions to process.
-- If string = NULL , the field will not be populated by APS_create_pmf()
--					  WARNING !! WARNING !! 
--					  This field MUST be populated elsewhere to prevent error.
-- If keyword= NULL , the entire instruction will not be processed.
-- If conversion = NULL, no effect.
*/

/* FILE_NAME      (4) is populated  elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
*/  
						{PMF_file_name,		NULL, 
                                        PMFKEYWD_FILENAME,  ODLConvertSymbol},
/* FILE_FORMAT     5*/  {PMF_format,        "ASF",
                                        PMFKEYWD_FORMAT,    ODLConvertSymbol},
/* FILE_SOURCE     6*/  {PMF_file_source,   "APS",
                                        PMFKEYWD_SRC,       ODLConvertSymbol},
/* FILE_DESTINATION7*/  {PMF_file_dest,     "ESA",
                                        PMFKEYWD_DEST,      ODLConvertSymbol},
/* FA_FILE_TYPE    8*/  {PMF_fa_file_type, "REUG",
                                        PMFKEYWD_FA_FTYPE,  ODLConvertSymbol},
/* GEN_FILE_TYPE   9*/  {PMF_gen_file_type, "STATION_STATUS",
                                        PMFKEYWD_GEN_FTYPE, ODLConvertSymbol},
/*
-- FILE_CREATION_TIME (10) is populated elsewhere.
-- 				However, we still carry the information fields needed 
--				by the create_APSodl_catalog_metadata routine 
--		(ie:keyword=PMFKEYWD_FILE_CREAT_TIME, conversion=ODLConvertDateTime)
*/
						{PMF_fa_createtime, NULL,
                                        PMFKEYWD_FILE_CREAT_TIME,
                                                            ODLConvertDateTime},
/* PLATFORM       (11) is not required in an unavailability report, which 
						reports on unavailability of entire station.
						The  following would otherwise be used:
						{PMF_satellite,     fa_sat_id,
                                        PMFKEYWD_PLATFORM,  ODLConvertSymbol},
*/	
						{PMF_satellite,     	NULL,	NULL ,	NULL },
/*
-- ANTENNA_ID  (12) is not reported in the REUG Unavailability Report.
-- Originally this report was to be generated for a single antenna.
-- A format for multi-antenna REUG will need to be decided by ASF and the FA
*/
                        {PMF_antenna_id,		NULL,   NULL,	NULL},
/*
-- ARRIVAL_TIME (13) at FAIF is not filled in here.  Otherwise, we would use:
                        {PMF_faif_arrivaltime, <string>,
                                    PMFKEYWD_ARRIVE_TIME,   ODLConvertDateTime},
*/
                        {PMF_faif_arrivaltime, NULL,	NULL,	NULL},
 
/* RESPONSE_TO   (14) is not used, because REUG is not a response file 
						{PMF_response_to,      (char *) &fa_filename,
                                        PMFKEYWD_RESPONSE,  ODLConvertSymbol},
*/
						{PMF_response_to,		NULL,    NULL,   NULL},

/* VALID_START_TIME 15*/{PMF_valid_starttime,  fa_file_start_time,
                                        PMFKEYWD_VALID_START,
                                                            ODLConvertDateTime},
/* VALID_END_TIME  16*/ {PMF_valid_stoptime,   fa_file_stop_time,
                                        PMFKEYWD_VALID_END, ODLConvertDateTime},

/* START_REV      (17) not used for REUG  
   END_REV		  (18) not used for REUG
						{PMF_start_rev,         (char *)&fa_file_start_rev,
                                        PMFKEYWD_START_REV, ODLConvertInteger},
						{PMF_stop_rev, 			(char *)&fa_file_stop_rev,
                                        PMFKEYWD_END_REV,   ODLConvertInteger},
*/
						{PMF_start_rev,			NULL,    NULL,   NULL}, 
						{PMF_stop_rev, 			NULL,    NULL,   NULL},	

/* FIRST_AOS		(19)
   LAST_LOS			(20)
*/
                        {PMF_first_aos_time,	NULL,   NULL,	NULL},
                        {PMF_last_los_time, 	NULL,   NULL,	NULL},

                        {NULL,  NULL,   NULL,   NULL }
} ;


PMF_FILENAME PMF_files[] =
{
	{"NASDA",   "MSGF", PMF_struct_msgf},  /* ASF Station Unavailabily		*/
	{"NASDA",   "REQM", PMF_struct_msge},  /* MSGE is a response to REQM	*/
	{"NASDA",   "REQR", PMF_struct_stgs},  /* STGS is a response to REQR	*/
	{"NASDA",   "REQQ", PMF_struct_reqq},  /* Quarterly Request File		*/
	{"NASDA",   "REQW", PMF_struct_reqw},  /* Weekly Request File			*/
	{"ESA",		"REUG", PMF_struct_reug},  /* ASF Station Unavailabity 		*/
	{"CSA",		"CRAR", PMF_struct_crar},  /* ASF Reception Avail. Report   */
	{"CSA",		"MRAR", PMF_struct_crar},  /* MCM Reception Avail. Report   */

	{"ASF",		"HC",	PMF_struct_hstv},  /* Host Controler State Vector File*/
	{"ASF",		"AWOS", PMF_struct_hwos},  /* Host Controler WOS			*/
	{"ASF",		"WFF",	PMF_struct_wstv},  /* Wallops State Vector File		*/
	{"ASF",		"MWOS", PMF_struct_wwos},  /* Wallops WOS          			*/
	{"ASF",		"AREQ", PMF_struct_wreq},  /* Wallops Request for Avail'ty	*/
    {NULL, NULL, NULL}
} ;

/*==============================================================================
Function:	create_APSodl_common_hdr
Description:	Creates the common header aggregate for the PMF
Parameters:     APSodl_Common_Header *record - C structure containing the values
                                            of the fields in the common header
		AGGREGATE parent - the parent aggregate, which should be the PMF
		                   aggregate
Returns:	
Creator:	Philip Yurchuk (phil@orca.jpl.nasa.gov)
Creation Date:	6/25/95
Notes:		
==============================================================================*/
void create_APSodl_common_hdr(APSodl_Common_Header *record, AGGREGATE parent)
{
  AGGREGATE  common_hdr ;       /* common header aggregate */
  PARAMETER  curr_param ;       /* the current parameter   */
  VALUE_DATA curr_value ;       /* the current value       */
  VALUE      value_node_ptr ;   /* not currently needed    */
  char tempdate[TIME_STRING_LEN+1] ;

  /* create the COMMON_HDR aggregate, and connect it to parent */
  
  common_hdr = NewAggregate(parent, KA_OBJECT, PMFKEYWD_COMMON_HDR, NULL) ;

  /* create the TIME parameter, assign it to common_hdr aggregate, and attach
  -- the file_creation_time value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, PMFKEYWD_TIME) ;
  curr_param->value_kind = KV_SCALAR ;
  strcpy(tempdate, record->file_creation_time) ;
  curr_value = ODLConvertDateTime (tempdate, sizeof(tempdate)-1) ;
  value_node_ptr = NewValue(curr_param, &curr_value) ;
  
  /* create the SOURCE parameter, assign it to common_hdr aggregate, and attach
  -- the file_source value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, PMFKEYWD_SRC) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol(
		record->file_source, strlen(record->file_source), 2) ;
  value_node_ptr = NewValue(curr_param, &curr_value) ;
  
  /* create the DESTINATION parameter, assign it to common_hdr aggregate, and 
  -- attach the file_source value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, PMFKEYWD_DEST) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol(
		record->file_dest, strlen(record->file_dest), 2) ;
  value_node_ptr = NewValue(curr_param, &curr_value) ;

  /* create the NUMBER_OF_RECORDS parameter, assign it to common_hdr aggregate, and 
  -- attach the #_recs value to it
  */
  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, PMFKEYWD_NUMRECS) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertInteger(
		record->number_of_records, strlen(record->number_of_records)) ;
  value_node_ptr = NewValue(curr_param, &curr_value) ;
 
  /* create the MSG_TYPE parameter, assign it to common_hdr aggregate, and 
  -- attach the file_type value to it
  */

  curr_param = NewParameter(common_hdr, KP_ATTRIBUTE, PMFKEYWD_MSG_TYPE) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertSymbol (
		record->file_type, strlen(record->file_type), 2) ;
  value_node_ptr = NewValue(curr_param, &curr_value) ;
 
} /* create_APSodl_common_hdr */



 

/*==============================================================================
Function:	create_APSodl_catalog_metadata
Description:	Creates the catalog metadata aggregate for the PMF
Parameters:     APSodl_Catalog_Metadata *record - C 
								structure containing the values
                                of the fields in the catalog metadata object
		AGGREGATE parent - the parent aggregate, which should be the PMF
		                   aggregate
Returns:	
Creator:        Philip Yurchuk   (phil@orca.jpl.nasa.ogv)
Creation Date:	7/5/95
Notes: modified by Miguel A. Siu		
			Modify this code to make use of the APSPMF_metadata model, which
			could include a pointer to the conversion function, as well as
			a pointer to the keyword to be used.
==============================================================================*/
void create_APSodl_catalog_metadata(
	APSPMF_metadata PMF_struct[], 
	AGGREGATE parent)
{
  AGGREGATE catalog_metadata ;
  PARAMETER  curr_param ;
  VALUE_DATA curr_value ;
  VALUE      value_node_ptr ;
  char tempdate[TIME_STRING_LEN+1] ;
  int	i ;
 
  /* create the CATALOG_METADATA aggregate, and connect it to parent */

  catalog_metadata = NewAggregate(parent, KA_OBJECT, PMFKEYWD_CATMETA, NULL) ;

/*
-- Now, process all the fields for aggregate 
*/
  for (i=0; i<NUMBER_OF_PMF_FIELDS; i++)
  {
	if (PMF_struct[i].field[0] && PMF_struct[i].keyword)
    {
      curr_param = 
		NewParameter(catalog_metadata, KP_ATTRIBUTE, PMF_struct[i].keyword) ;
      curr_param->value_kind = KV_SCALAR ;

      if (PMF_struct[i].conversion == ODLConvertDateTime)
	  {
		strcpy(tempdate, PMF_struct[i].field) ;
		curr_value = PMF_struct[i].conversion (tempdate, sizeof(tempdate)-1) ;
	  }
	  else
	  	curr_value = PMF_struct[i].conversion ( 
						PMF_struct[i].field, strlen(PMF_struct[i].field), 2) ;

	  value_node_ptr = NewValue(curr_param, &curr_value) ;
    }
  }

#ifdef MIGUEL_COMMENT_OUT

	  9999     99    9    9  99999   9       999999   9999
	 9        9  9   99  99  9    9  9       9       9
	  9999   9    9  9 99 9  9    9  9       99999    9999
	      9  999999  9    9  99999   9       9            9
	 9    9  9    9  9    9  9       9       9       9    9
	  9999   9    9  9    9  9       999999  999999   9999

  if (record->valid_end_time[0])
    {
      curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_VALID_END) ;
      curr_param->value_kind = KV_SCALAR ;
      strcpy(tempdate, record->valid_end_time) ;
      curr_value = ODLConvertDateTime (tempdate, sizeof(tempdate)-1) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->start_rev[0])
    {
      curr_param = 
	 NewParameter(catalog_metadata, KP_ATTRIBUTE, PMFKEYWD_START_REV) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertInteger (record->start_rev, sizeof(record->start_rev)-1) ;
      NewValue(curr_param, &curr_value) ;
    }

#endif /* for MIGUEL_COMMENT_OUT */

} /* create_APSodl_catalog_metadata */



 

/*==============================================================================
Function:	create_APSodl_detailed_metadata
Description:	Creates DETAILED_METADATA object part of PMF	
Parameters:
	record - contains detailed metadata info to put in PMF ODL object
	parent - ODL tree to add detailed metadata object

Returns:	None
Creator:	Philip Yurchuk
Creation Date:	Thu Jun 29 15:27:37 PDT 1995
Notes:		
==============================================================================*/
void create_APSodl_detailed_metadata(APSodl_Detailed_Metadata *record, AGGREGATE parent)
{
 
  AGGREGATE detailed_metadata ;
  PARAMETER  curr_param ;
  VALUE_DATA curr_value ;
  VALUE      value_node_ptr ;
 
  detailed_metadata =
     NewAggregate(parent, KA_OBJECT, PMFKEYWD_DETAILEDMETA, NULL) ;
  curr_param = 
     NewParameter(detailed_metadata, KP_ATTRIBUTE, PMFKEYWD_FILESIZE) ;
  curr_param->value_kind = KV_SCALAR ;
  curr_value = ODLConvertInteger (record->file_size, sizeof(record->file_size)-1) ;
  value_node_ptr = NewValue(curr_param, &curr_value) ;

} /* create_APSodl_detailed_metadata */





/*==============================================================================
Function:	PMFcurrtime	
Description:	Gets current time to be used in the PMF
Parameters:	None
Returns:	time string for current time	
Creator:	Phil Yurchuk
Creation Date:	Thu Jun 29 15:27:57 PDT 1995
Notes:		
==============================================================================*/
char *PMFcurrtime()
{
  char *s ;
  time_t now ;
  
  s = (char *)util_do_malloc(sizeof(char)*(TIME_STRING_LEN+1)) ;

  now = time(NULL) ;
  strftime(s, TIME_STRING_LEN+1, "%Y-%jT%X.000",localtime(&now)) ;

  return (s) ;

} /* PMFcurrtime */





/*==============================================================================
Function:	PMFfiletime
Description:	Get last modification time of a file (Part of PMF)
Parameters:
	fname - name of file to get mod time for
Returns:	Time string for mod time of fname	
Creator:	Phil Yurchuk
Creation Date:	Thu Jun 29 15:27:57 PDT 1995
Notes:		
==============================================================================*/
char *PMFfiletime(char *fname)
{
  char *s ;
  struct stat buf ;

  stat(fname, &buf) ;

  s = (char *)util_do_malloc(sizeof(char)*(TIME_STRING_LEN+1)) ;

  strftime(s, TIME_STRING_LEN+1, "%Y-%jT%X.000",localtime(&buf.st_mtime)) ;
  /*
  strftime(s, TIME_STRING_LEN+1, "%Y-%jT%X.000",localtime(&buf.st_mtim.tv_sec)) ;
  */

  return (s) ;

} /* PMFfiletime */

  
 


/*==============================================================================
Function:	create_PMF	
Description:	Create PMF object from metadata record supplied	
Parameters:
	catrecord - metadata record containing info to be put in PMFcatalog_meta

Returns:	Root of PMF ODL	
Creator:	Phil Yurchuk
Creation Date:	Thu Jun 29 15:28:21 PDT 1995

Notes:		modified to use the new APSPMF_metadata structure
==============================================================================*/
AGGREGATE create_PMF(APSPMF_metadata catrecord[])
{
  
  AGGREGATE root, parent ;
  APSodl_Common_Header *commonrecord ;
  APSodl_Common_Header commonhdrrec ;

  commonrecord  = &commonhdrrec ;

  strcpy(commonrecord->file_creation_time, PMFcurrtime()) ;
  strcpy(catrecord[FILE_CREATION_TIME].field, PMFcurrtime()) ;
  strcpy(commonrecord->number_of_records, catrecord[NUMBER_OF_RECORDS].field) ;
  strcpy(commonrecord->file_source, catrecord[PMF_SOURCE].field) ;
  strcpy(commonrecord->file_dest, "IMS") ;
  strcpy(commonrecord->file_type, FAIF_PMF_NAME) ;

  root   = NewAggregate(NULL, KA_OBJECT, "root", NULL) ;
  parent = NewAggregate(root, KA_OBJECT, FAIF_PMF_NAME, NULL) ;

  create_APSodl_common_hdr(commonrecord, parent) ;
  create_APSodl_catalog_metadata(catrecord, parent) ;

  return(root) ;

} /* create_PMF */


/*==============================================================================
Function:       APS_create_pmf

Description:    Receive a Flight Agency file PMF description and use it to
				create a PMF file.

Parameters:     

Returns:        

Creator:        Miguel Siu

Creation Date:  Mon Nov 20 14:55:35 PST 1995

Notes:	All data will come from global variables populated by the calling
		program, and from values which have been pre-determined for each
		Flight Agency file.
==============================================================================*/
int APS_create_pmf (char *metafilename, APSPMF_metadata *PMF_struct)
{
int				i ;
FILE            *PMF_file_ptr ;
AGGREGATE     	root ;

#ifdef PRINT_DIAG
printf ("about to clear out PMF_struct.  And populate it.\n") ;
#endif

	for (i=0; i<NUMBER_OF_PMF_FIELDS; i++)
	{
		if (PMF_struct[i].string == NULL) continue;	

		PMF_struct[i].field[0] = NULL ;
		strncpy (PMF_struct[i].field,	PMF_struct[i].string,
									strlen(PMF_struct[i].string) ) ;
	}

#ifdef PRINT_DIAG
printf ("about to create the PMF with new structure\n") ;
#endif

  /* Create the PMF */

  root = (AGGREGATE)create_PMF(PMF_struct) ;

#ifdef PRINT_DIAG
  PrintLabel(root) ;    
#endif

       PMF_file_ptr = fopen(metafilename, "w");
       WriteLabel (PMF_file_ptr, root);
       fclose(PMF_file_ptr);

}
/* End of File */
