/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
 
/*==============================================================================
Filename:	WALPS.h
Description:	
	Header file for WALLOPS functions.  Includes WALLOPS keyword
#define's, keyword string lengths, etc.
	 
Creator:        Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:
	Refer to the FAIF Software Requirements Document (JPL-D11909) for a
list of file types supported through this header file.
1.  Oct. '96 - RH
    Updated to Version 2.3 of ASF/WFF ISD
    Added ASF_Data_Seq_Msg_Record
    Added WALPS_SR* and WALPS_TPR* items

SCCS Info:
   @(#)WALPS.h	1.1
==============================================================================*/

#ifndef _WALLOPS_
#define _WALLOPS_

#ifndef ERROR
#include "faifdefs.h"
#endif

#include "odldef.h"

/* Wallops Keyword Table entry record structure
*/
typedef struct wallops_keywords
{
   int id ;                                      /* Keyword Id */
   char *keyword ;                               /* Keyword string */

} WALPS_Keywords ;

/* Wallops bound Wallops file types 
*/
#define WALPS_RES        300  /* Availability Response */
#define WALPS_WOS        301  /* Weekly Operations Schedule */
#define WALPS_EPH        302  /* Ephemeris (State Vectors) */
#define WALPS_DNL        303  /* Downlink (Data Take) */
#define WALPS_MSH        304  /* MGS Shipment Report */
#define WALPS_REQ        305  /* Availability Request */
#define WALPS_TPR        306  /* TDRS Playback Report */

/* Some WALPS delimiters
*/
#define WALPS_FN_DELIM '_'
#define WALPS_TM_DELIM 'T'

/* Wallops FA file type strings
*/
#define WALPS_RES_STR  "WALPS_AVAIL_RESP"
#define WALPS_WOS_STR  "WALPS_WOS"
#define WALPS_EPH_STR  "WALPS_STVEC"
#define WALPS_DNL_STR  "WALPS_DT"
#define WALPS_MSH_STR  "WALPS_SHIP_RPT"
#define WALPS_REQ_STR  "WALPS_AVAIL_REQ"
#define WALPS_TPR_STR  "WALPS_TDRS_PLAYBACK"

/* Wallops defined filename prefix strings
*/
#define WALPS_REQ_PREFIX  "REQ"
#define WALPS_RES_PREFIX  "RES"
#define WALPS_WOS_PREFIX  "WOS"
#define WALPS_EPH_PREFIX  "EPH"
#define WALPS_DNL_PREFIX  "DNL"
#define WALPS_MSH_PREFIX  "MSH"
#define WALPS_TPR_PREFIX  "TPR"

/*
-- Note : Modify these as necessary.  
*/
 
#define COMMON_HDR        0
#define TIME              1
#define MSG_TYPE          2
#define DESTINATION       3
#define SOURCE            4
#define NUM_RECS          5
#define REQ               6
#define RES               7
#define WOS               8
#define EPH               9
#define DNL              10
#define MSH              11
#define TPR              12

/* Wallops defined identifiers
*/
#define COMMON_HDR_KEYWD  "COMMON_HEADER"
#define TIME_KEYWD        "TIME"
#define MSG_TYPE_KEYWD    "MSG_TYPE"
#define DESTINATION_KEYWD "DESTINATION"
#define SOURCE_KEYWD      "SOURCE"
#define NUM_RECS_KEYWD    "NUMBER_OF_RECORDS"
#define REQ_KEYWD         "AVAILABILITY_REQUEST"
#define RES_KEYWD         "AVAILABILITY_RESPONSE"
#define WOS_KEYWD         "WOS"
#define EPH_KEYWD         "EPHEMERMIS"
#define DNL_KEYWD         "DOWNLINK"
#define MSH_KEYWD         "MGS_SHIPMENT_REPORT"
#define TPR_KEYWD         "TDRS_PLAYBACK_REPORT"

/* Some valid Wallops header values
*/
#define WALPS_SRCDEST_ASF    "ASF"
#define WALPS_SRCDEST_WALPS  "WFF"

/* Valid WALLOPS parameters (Keywords inside ODL Object Aggregations)
*/
#define SATELLITE      0
#define SENSOR         1
#define MODE           2
#define REVOLUTION     3
#define DATATAKES      4
#define ACTIVITY_ID    5
#define AGENCY         6
#define RECORDED       7
#define MEDIA_ID       8
#define START_ADDRESS  9
#define END_ADDRESS   10
#define START_TIME    11
#define END_TIME      12
#define RECORDER_ID   13
#define TIMEON        14
#define TIMEOFF       15
#define TRANSMITTERID 16
#define SITENAME      17


/* Associated keyword strings for WALLOPS parameters
*/
#define SATELLITE_KEYWD      "PLATFORM"
#define SENSOR_KEYWD         "SENSOR"
#define MODE_KEYWD           "MODE"
#define REVOLUTION_KEYWD     "REVOLUTION"
#define SEQUENCE_KEYWD       "SEQUENCE"
#define DATATAKES_KEYWD      "DATATAKES"
#define ACTIVITY_ID_KEYWD    "ACTIVITY_ID"
#define AGENCY_KEYWD         "AGENCY"
#define RECORDED_KEYWD       "RECORDED"
#define MEDIA_ID_KEYWD       "MEDIA_ID"
#define START_ADDRESS_KEYWD  "START_ADDRESS"
#define END_ADDRESS_KEYWD    "END_ADDRESS"
#define START_TIME_KEYWD     "START_TIME"
#define END_TIME_KEYWD       "END_TIME"
#define RECORDER_ID_KEYWD    "RECORDER_ID"
#define TIMEON_KEYWD         "TIME_ON"
#define TIMEOFF_KEYWD        "TIME_OFF"
#define TRANSMITTERID_KEYWD  "TRANSMITTER_ID"
#define SITENAME_KEYWD       "SITENAME"


#define WALPS_SATNAME_E1  "E-ERS-1"
#define WALPS_SATNAME_E2  "E-ERS-2"
#define WALPS_SATNAME_J1  "J-ERS-1"
#define WALPS_SATNAME_R1  "RADARSAT"
#define WALPS_SATNAME_A1  "ADEOS"

#define WALPS_SENSOR_DUMP "DUMP"
#define WALPS_SENSOR_SAR  "SAR"
#define WALPS_SENSOR_OPS  "OPS"
#define WALPS_SENSOR_VNIR "VNIR"

#define WALPS_SMODE_STD1  "STANDARD-1"
#define WALPS_SMODE_STD2  "STANDARD-2"
#define WALPS_SMODE_STD3  "STANDARD-3"
#define WALPS_SMODE_STD4  "STANDARD-4"
#define WALPS_SMODE_STD5  "STANDARD-5"
#define WALPS_SMODE_STD6  "STANDARD-6"
#define WALPS_SMODE_STD7  "STANDARD-7"
#define WALPS_SMODE_WIDE1 "WIDE-1"
#define WALPS_SMODE_WIDE2 "WIDE-2"
#define WALPS_SMODE_WIDE3 "WIDE-3"
#define WALPS_SMODE_FINE1 "FINE-1"
#define WALPS_SMODE_FINE2 "FINE-2"
#define WALPS_SMODE_FINE3 "FINE-3"
#define WALPS_SMODE_FINE4 "FINE-4"
#define WALPS_SMODE_FINE5 "FINE-5"
#define WALPS_SMODE_ELOW1 "EXT-LOW-1"
#define WALPS_SMODE_EHI1  "EXT-HIGH-1"
#define WALPS_SMODE_EHI2  "EXT-HIGH-2"
#define WALPS_SMODE_EHI3  "EXT-HIGH-3"
#define WALPS_SMODE_EHI4  "EXT-HIGH-4"
#define WALPS_SMODE_EHI5  "EXT-HIGH-5"
#define WALPS_SMODE_EHI6  "EXT-HIGH-6"
#define WALPS_SMODE_SCNS1 "SCANSAR-1"
#define WALPS_SMODE_SCNS2 "SCANSAR-2"
#define WALPS_SMODE_SCNS3 "SCANSAR-3"
#define WALPS_SMODE_SCNS4 "SCANSAR-4"

#define WALPS_ACT_DWNLNK  "RT-SAR"
#define WALPS_ACT_DUMP    "DUMP"

#define WALPS_AGENCY_ASF   "ASF"
#define WALPS_AGENCY_ESA   "ESA"
#define WALPS_AGENCY_CSA   "CSA"
#define WALPS_AGENCY_ESF   "ESA-ASF"
#define WALPS_AGENCY_CSF   "CSA-ASF"
#define WALPS_AGENCY_NASDA "NASDA"
#define WALPS_AGENCY_NSF   "NASDA-ASF"

#define WALPS_DT_TAKEN_Y   "YES"
#define WALPS_DT_TAKEN_N   "NO"

#define WALPS_XMITTER_J8150  "JERS-1_8150"
#define WALPS_XMITTER_J8350  "JERS-1_8350"
#define WALPS_XMITTER_R8105  "RADARSAT_8105"
#define WALPS_XMITTER_R8230  "RADARSAT_8230"

#define WALPS_SRRECORD_NAME "MGS_SHIPMENT_REPORT"
#define WALPS_SR_SUFFIX     "WALPS_SHIP_RPT"
#define MAX_SRKEYVAL_STRLEN  256
#define WALPS_MSH_TAPE_STR  "MGS_TAPE_RECORD"

#define WALPS_TPRRECORD_NAME   "TDRS_PLAYBACK_REPORT"
#define WALPS_TPR_SUFFIX       "WALPS_TDRS_PLAYBACK"
#define MAX_TPRKEYVAL_STRLEN    256
#define WALPS_TPR_TAPE_STR     "TDRS_PLAYBACK_REPORT_RECORD"
#define TDRS_ID_BLOCKS         33.12764

/*  FAIF translates WALPS DNL messages and the tape_downlink_records within
    the WALPS MSH and TPR messages into ASF Data Sequence Messages.  The
    ASF_Data_Seq_Msg_Record is used during this translation. */

typedef struct data_seq_msg_record
{
   char  destination[4];                  /* always "IMS" */
   int   number_of_records;               /* always 1 */
   char  msg_type[22];                    /* always "DATA_SEQUENCE_MESSAGE" */
   char  source[5];                       /* always "FAIF" */
   char  time[TIME_STRING_LEN + 1];
   char  system_activity_id[7];           /* always "FAIF_0" */
   char  system_activity_type[9];         /* always "DOWNLINK" */
   char  status[14];                      /* either "HST_S_OK" or
                                             "HST_S_PROBLEM" */
   char  platform[3];                     /* "E1" or "E2" or .... */
   char  sensor[2];                       /* "S" or "O" or "V" */
   int   revolution;
   int   sequence;
   char  media_id_alias[13];              /* "MCMURDO-nnnn" */
   char  media_id[13];                    /* "MCMURDO-nnnn" */
   char  media_id_type_name[15];          /* always "ARCHIVE_SIGNAL" */
   int   media_dog_tag;                   /* always 0 */
   int   generation;                      /* always 1 */
   char  recorder_id[6];                  /* "MM" or "MM_xx" */
   char  recorder_type[6];                /* always "DCRSI" */
   char  recorder_media_type[14];         /* always "AMPEX_HED_733" */
   char  data_direction[8];               /* always "UNKNOWN" */
   int   start_address;
   int   stop_address;
   struct ODLDate  start_time;
   struct ODLDate  end_time;
   char  channelization[4];               /* always "N/A" */
   char  bit_rate[10];                    /* always "105000000" */
   int   tdrs_number_of_ids;
}  ASF_Data_Seq_Msg_Record;

#endif /* _WALLOPS_ */

/* End of File */
