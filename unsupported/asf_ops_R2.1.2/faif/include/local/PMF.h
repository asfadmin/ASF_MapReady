/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
 
/*==============================================================================
Filename:	 PMF.h

Description:	
	 This contains the data structure definition for a FA catalog 
metadata.
	 
Creator:        Philip Yurchuk (phil@orca.jpl.nasa.gov)
Notes:
	FAIF PMF Document
1.  Oct. '96 - RH
    Added keywords for ASF_Data_Seq_Msg_Record

SCCS Info:
   @(#)PMF.h	1.1
==============================================================================*/

#ifndef _PMF_
#define _PMF_

#ifndef ERROR
#include "faifdefs.h"
#endif

#include "odldef.h"
#include "odlinter.h"

#define NUMRECS_IN_PMF "1"

/* Catalog Metadata record structure
*/
typedef struct ODL_catalog_metadata
{
   char file_name[MAXLINE] ;                     /* Name of file to be archived
						 */
   char format[MAXLINE] ;

   char file_source[MAXLINE] ;                   /* File Source               */
   char file_arrival_time[TIME_STRING_LEN+1] ;   /* File Arrival Time         */
   char number_of_records[MAXLINE];
   char file_dest[MAXLINE] ;                     /* File Destination          */
   char FA_file_type[MAXLINE] ;                  /* FA File Type              */
   char gen_file_type[MAXLINE] ;                 /* Generated File Type       */
   char satellite[MAXLINE] ;                     /* Satellite                 */
   char file_creation_time[TIME_STRING_LEN+1] ;  /* catchall                  */
   char valid_start_time[TIME_STRING_LEN+1] ;    /* Start of time bracket     */
   char valid_end_time[TIME_STRING_LEN+1] ;      /* Endt of time bracket      */
   char start_rev[MAXLINE] ;                     /* Start of rev bracket      */
   char end_rev[MAXLINE] ;                       /* End of rev bracket        */

} ODL_Catalog_Metadata ;

typedef struct ODL_detailed_metadata
{
   char file_size[MAXLINE] ;                     /* Size of file in kilobytes */
} ODL_Detailed_Metadata ;

#define FAIF_PMF_NAME      "DAPPS_FILE_METADATA"

/* PMF Keywords
*/
#define PMF_SRC   "FAIF"
#define PMF_DEST  "ASF"

#define PMFKEYWD_COMMON_HDR  "COMMON_HEADER"
#define PMFKEYWD_TIME        "TIME"
#define PMFKEYWD_MSG_TYPE    "MSG_TYPE"
#define PMFKEYWD_DEST        "DESTINATION"
#define PMFKEYWD_SRC         "SOURCE"
#define PMFKEYWD_NUMRECS     "NUMBER_OF_RECORDS"

#define PMFKEYWD_CATMETA      "CATALOG_METADATA"
#define PMFKEYWD_DETAILEDMETA "DETAILED_METADATA"

#define PMFKEYWD_FILENAME    "FILE_NAME"
#define PMFKEYWD_FORMAT      "FILE_FORMAT"
#define PMFKEYWD_ARRIVE_TIME "ARRIVAL_TIME"
#define PMFKEYWD_FA_FTYPE    "FA_FILE_TYPE"
#define PMFKEYWD_GEN_FTYPE   "GEN_FILE_TYPE"
#define PMFKEYWD_PLATFORM    "PLATFORM"
#define PMFKEYWD_FILE_CREAT_TIME  "FILE_CREATION_TIME"
#define PMFKEYWD_VALID_START "VALID_START_TIME"
#define PMFKEYWD_VALID_END   "VALID_END_TIME"
#define PMFKEYWD_START_REV   "START_REV"
#define PMFKEYWD_END_REV     "END_REV"

#define PMFKEYWD_FILESIZE    "FILE_SIZE"

#define PMFKEYWD_SYS_ACT_ID      "SYSTEM_ACTIVITY_ID"
#define PMFKEYWD_SYS_ACT_TYPE    "SYSTEM_ACTIVITY_TYPE"
#define PMFKEYWD_STATUS          "STATUS"
#define PMFKEYWD_SENSOR          "SENSOR"
#define PMFKEYWD_REVOLUTION      "REVOLUTION"
#define PMFKEYWD_SEQUENCE        "SEQUENCE"
#define PMFKEYWD_MEDIA_ID_ALIAS  "MEDIA_ID_ALIAS"
#define PMFKEYWD_MEDIA_ID        "MEDIA_ID"
#define PMFKEYWD_MEDIA_ID_TYP    "MEDIA_ID_TYPE_NAME"
#define PMFKEYWD_MEDIA_DOG_TAG   "MEDIA_DOG_TAG"
#define PMFKEYWD_ACT_ID          "ACTIVITY_ID"
#define PMFKEYWD_GENERATION      "GENERATION"
#define PMFKEYWD_RECORDER_ID     "RECORDER_ID"
#define PMFKEYWD_RECORDER_TYPE   "RECORDER_TYPE"
#define PMFKEYWD_RECORDER_MTYPE  "RECORDER_MEDIA_TYPE"
#define PMFKEYWD_DATA_DIRECTION  "DATA_DIRECTION"
#define PMFKEYWD_START_ADDRESS   "START_ADDRESS"
#define PMFKEYWD_STOP_ADDRESS    "STOP_ADDRESS"
#define PMFKEYWD_START_TIME      "START_TIME"
#define PMFKEYWD_END_TIME        "END_TIME"
#define PMFKEYWD_CHANNELIZATION  "CHANNELIZATION"
#define PMFKEYWD_BIT_RATE        "BIT_RATE"
#define PMFKEYWD_TDRS_NUM_IDS    "TDRS_NUMBER_OF_IDS"

/* General file type used in PMF 
*/
#define GFTYPE_FASKED    "FA_SCHEDULE"
#define GFTYPE_PSV       "PREDICTED_STATE_VECTORS"
#define GFTYPE_RSV       "RESTITUTED_STATE_VECTORS"
#define GFTYPE_RQST_DCP  "REQUEST_FOR_DATA_COPY"
#define GFTYPE_FADCPRMSG "FA_DATA_COPY_RECEIVED_MESSAGE"
#define GFTYPE_FARTLTUR  "FA_REPLY_TO_LONG_TERM_USER_REQUESTS"
#define GFTYPE_TIME_COR  "TIME_CORRELATION"
#define GFTYPE_LTUSRRQV  "LONG_TERM_USER_RQST_VALIDATION"
#define GFTYPE_FAPLAN    "FA_PLAN"
#define GFTYPE_LTUSRREQ  "LONG_TERM_USER_REQUESTS"
#define GFTYPE_ACQ_REP   "ACQUISTION_REPORT"
#define GFTYPE_SHIP_REP  "SHIPMENT_REPORT"
#define GFTYPE_ARCH_DEX  "ARCHIVED_DATA_EXTRACTED"
#define GFTYPE_STN_STAT  "STATION_STATUS"
#define GFTYPE_SVTIMCOR  "STATE_VECTORS"
#define GFTYPE_FARQSTS   "FA_REQUESTS"
#define GFTYPE_TAPE_RD   "TAPE_READABILITY"
#define GFTYPE_SAT_STAT  "SATELLITE_STATUS"
#define GFTYPE_STUSRREQ  "SHORT_TERM_USER_REQUESTS"
#define GFTYPE_CAT_REP   "CATALOG_REPORT"
#define GFTYPE_RFA_RQST  "REPLY_TO_FLIGHT_AGENCY_REQUEST"
#define GFTYPE_CAL_RQST  "CALIBRATION_REQUEST"
#define GFTYPE_CAL_SKED  "CALIBRATION_SCHEDULE"
#define GFTYPE_SARPROCP  "SAR_PROCESSING_PARAMETERS"
#define GFTYPE_FAOPRES   "FA_OPERATION_RESULT"
#define GFTYPE_RGSRESP   "RGS_RESPONSE"
#define GFTYPE_DATAMSG   "DATATAKE_MESSAGE"
#define GFTYPE_RGSRQST   "RGS_REQUEST"
#define GFTYPE_WOS       "WEEKLY_OPERATIONS_SCHEDULE"
#define GFTYPE_SV        "STATE_VECTORS"

#define ASF_STR          "ASF"
#define ORIG_STR         "ORIGINAL"
#define NA_STR           "NA"
#define NO_STR           "NO"
#define YES_STR          "YES"

#define E_SAT            "E#"
#define E1_SAT           "E1"
#define E2_SAT           "E2"
#define J_SAT            "J1"
#define A_SAT            "A1" 
#define R_SAT            "R1" 

#define PMF_FILE_EXT     "M"


#endif /* _PMF_ */

/* End of File */
