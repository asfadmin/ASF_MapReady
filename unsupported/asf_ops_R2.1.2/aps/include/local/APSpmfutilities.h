#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	 APSpmfutilities.h

Description:	
	 This contains the data structure definition for a FA catalog 
metadata.
	 
Creator:        Philip Yurchuk (phil@orca.jpl.nasa.gov)
Notes:
	FAIF PMF Document

==============================================================================*/
#pragma ident	"@(#)APSpmfutilities.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.APSpmfutilities.h"


#ifndef _PMF_
#define _PMF_

#ifndef ERROR
#endif
#include "faifdefs.h"

#include "APSodlcommonhdr.h"
#include <odldef.h>
#include <odlinter.h>

#define NUMRECS_IN_PMF "1"


typedef struct APSPMF_catalog_metadata_
{
	char *field;
	char *string;
	char *keyword;
	VALUE_DATA  (*conversion)();

} APSPMF_metadata;

typedef struct APSodl_detailed_metadata
{
   char file_size[MAXLINE] ;                     /* Size of file in kilobytes */
} APSodl_Detailed_Metadata ;

#define PMF_SOURCE                  0
#define PMF_DESTINATION             1
#define MSG_TYPE                    2
#define NUMBER_OF_RECORDS           3
#define FILE_NAME                   4
#define FILE_FORMAT                 5
#define FILE_SOURCE                 6
#define FILE_DESTINATION            7
#define FA_FILE_TYPE                8
#define GEN_FILE_TYPE               9
#define FILE_CREATION_TIME          10
#define PLATFORM                    11
#define ANTENNA_ID                  12
#define ARRIVAL_TIME                13
#define RESPONSE_TO                 14
#define VALID_START_TIME            15
#define VALID_END_TIME              16
#define START_REV                   17
#define END_REV                     18
#define FIRST_AOS                   19
#define LAST_LOS                    20
#define NUMBER_OF_PMF_FIELDS        21


#define PMFKEYWD_ANTENNA	"ANTENNA_ID"
#define PMFKEYWD_RESPONSE	"RESPONSE_TO"

/* ============================================================================= */

#define FAIF_PMF_NAME  "DAPPS_FILE_METADATA"

/* PMF Keywords
*/
#define PMF_SRC   "APS"
#define PMF_DEST  "IMS"

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
#define PMFKEYWD_APS_PFLAG   "APS_PROCESSED_FLAG"
#define PMFKEYWD_VALID_START "VALID_START_TIME"
#define PMFKEYWD_VALID_END   "VALID_END_TIME"
#define PMFKEYWD_START_REV   "START_REV"
#define PMFKEYWD_END_REV     "END_REV"
#define PMFKEYWD_FIRST_AOS   "FIRST_AOS"
#define PMFKEYWD_LAST_LOS    "LAST_LOS"

#define PMFKEYWD_FILESIZE    "FILE_SIZE"

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
#define GFTYPE_SVTIMCOR  "STATE_VECTORS_AND_TIME_CORRELATION"
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

typedef
    struct _PMF_NAMES
    {
        char        *flight_agency ;
        char        *file_type ;
        APSPMF_metadata
					*file_descriptor ;
    } PMF_FILENAME ;

/*
-- extern defintions
-- used by the various PMF_struct_<filename>[] declarations
*/
extern PMF_FILENAME PMF_files[] ;

extern char PMF_start_rev[] ;
extern char PMF_stop_rev[] ;
extern char PMF_source[] ;
extern char PMF_dest[] ;
extern char PMF_msg_type[] ;
extern char PMF_antenna_id[] ;
extern char PMF_response_to[] ;

extern char PMF_satellite[] ;
extern char PMF_file_name[] ;
extern char PMF_format[] ;
extern char PMF_file_source[] ;     /* File Source               */
extern char PMF_number_of_records[];
extern char PMF_file_dest[] ;       /* File Destination          */
extern char PMF_fa_file_type[] ;    /* File Destination          */
extern char PMF_gen_file_type[] ;   /* Generated File Type       */

extern char PMF_fa_createtime[] ;
extern char PMF_faif_arrivaltime[] ;   /* File Arrival Time         */
extern char PMF_valid_starttime[] ;
extern char PMF_valid_stoptime[] ;
extern char PMF_first_aos_time[] ;
extern char PMF_last_los_time[] ;


#endif /* _PMF_ */

/* End of File */
