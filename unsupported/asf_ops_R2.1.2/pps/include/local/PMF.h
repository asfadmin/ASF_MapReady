/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
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
==============================================================================*/

#ifndef _PMF_
#define _PMF_

#pragma ident "@(#)PMF.h	1.1  11/21/96"

#ifndef ERROR
#include "defs.h"
#endif

#include "ODL/odldef.h"
#include "ODL/odlinter.h"

/* Catalog Metadata record structure
*/
typedef struct ODL_catalog_metadata
{
   char file_name[MAXLINE] ;                     /* Name of file to be archived
						 */
   char format[MAXLINE] ;
   char file_source[MAXLINE] ;                   /* File Source               */
   char file_arrival_time[TIME_STRING_LEN+1] ;   /* File Arrival Time         */
   char file_dest[MAXLINE] ;                     /* File Destination          */
   char FA_file_type[MAXLINE] ;                  /* FA File Type              */
   char gen_file_type[MAXLINE] ;                 /* Generated File Type       */
   char satellite[MAXLINE] ;                     /* Satellite                 */
   char file_creation_time[TIME_STRING_LEN+1] ;  /* Time ESA file was created */
   char APS_processed_flag[MAXLINE] ;            /* APS Processed Flag        */
   char valid_start_time[TIME_STRING_LEN+1] ;    /* Start of time bracket     */
   char valid_end_time[TIME_STRING_LEN+1] ;      /* Endt of time bracket      */
   char start_rev[TIME_STRING_LEN+1] ;           /* Start of rev bracket      */
   char end_rev[TIME_STRING_LEN+1] ;             /* End of rev bracket        */

} ODL_Catalog_Metadata ;

typedef struct ODL_detailed_metadata
{
   char file_size[MAXLINE] ;                     /* Size of file in kilobytes */
} ODL_Detailed_Metadata ;


/* General file type used in PMF */

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
#define J_SAT            "J1"
#define A_SAT            "AD" 

#endif /* _PMF_ */

/* End of File */




