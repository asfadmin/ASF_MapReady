/*==============================================================================
Filename:	ASF.h

Description:	
	This header file contains the #define's used to represent 
identifiers used in ASF standard formatted files. 

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		
1.  Oct. '96 - RH
    Add ASF_SATNAME_A1

SCCS Info:
   @(#)ASF.h	1.1
==============================================================================*/

#ifndef _ASF_
#define _ASF_

#define ASFHDR_YEAR   4
#define ASFHDR_TIME  16 
#define ASFHDR_MSGT   2
#define ASFHDR_DEST   3
#define ASFHDR_ORGN   3
#define ASFHDR_SPARE 17
#define ASFHDR_LEN ((ASFHDR_YEAR)+1 + (ASFHDR_TIME)+1 + \
		    (ASFHDR_MSGT)+1 + (ASFHDR_DEST)+1 + \
		    (ASFHDR_ORGN)+1 + (ASFHDR_SPARE))

#define ASF_MSGTYPE_DT   "DT"
#define ASF_MSGTYPE_SV   "SV"
#define ASF_MSGTYPE_WOS  "WO"
#define ASF_MSGTYPE_AREQ "AR"

#define ASF_DT_FNAME_LEN  15
#define ASF_DT_FEXT_LEN    4
#define ASF_DT_FEXT      ".DTM"

#define ASF_SATNAME_RS  "R1"
#define ASF_SATNAME_E1  "E1"
#define ASF_SATNAME_E2  "E2"
#define ASF_SATNAME_J1  "J1"
#define ASF_SATNAME_A1  "A1"

#define ASF_SENSOR_DUMP "D"
#define ASF_SENSOR_SAR  "S"
#define ASF_SENSOR_OPS  "O"
#define ASF_SENSOR_VNIR "V"

#define ASF_SMODE_STD1  "STANDARD-1"
#define ASF_SMODE_STD2  "STANDARD-2"
#define ASF_SMODE_STD3  "STANDARD-3"
#define ASF_SMODE_STD4  "STANDARD-4"
#define ASF_SMODE_STD5  "STANDARD-5"
#define ASF_SMODE_STD6  "STANDARD-6"
#define ASF_SMODE_STD7  "STANDARD-7"
#define ASF_SMODE_WIDE1 "WIDE-1"
#define ASF_SMODE_WIDE2 "WIDE-2"
#define ASF_SMODE_WIDE3 "WIDE-3"
#define ASF_SMODE_FINE1 "FINE-1"
#define ASF_SMODE_FINE2 "FINE-2"
#define ASF_SMODE_FINE3 "FINE-3"
#define ASF_SMODE_FINE4 "FINE-4"
#define ASF_SMODE_FINE5 "FINE-5"
#define ASF_SMODE_ELOW1 "EXT-LOW-1"
#define ASF_SMODE_EHI1  "EXT-HIGH-1"
#define ASF_SMODE_EHI2  "EXT-HIGH-2"
#define ASF_SMODE_EHI3  "EXT-HIGH-3"
#define ASF_SMODE_EHI4  "EXT-HIGH-4"
#define ASF_SMODE_EHI5  "EXT-HIGH-5"
#define ASF_SMODE_EHI6  "EXT-HIGH-6"
#define ASF_SMODE_SCNS1 "SCANSAR-1"
#define ASF_SMODE_SCNS2 "SCANSAR-2"
#define ASF_SMODE_SCNS3 "SCANSAR-3"
#define ASF_SMODE_SCNS4 "SCANSAR-4"

#define ASF_ACT_RTSAR   "RTS"
#define ASF_ACT_DUMP    "DMP"

#define ASF_AGENCY_ASF  "ASF"
#define ASF_AGENCY_ESA  "ESA"
#define ASF_AGENCY_CSA  "CSA"
#define ASF_AGENCY_ESF  "ESF"
#define ASF_AGENCY_CEF  "CEF"
#define ASF_AGENCY_NAS  "NAS"
#define ASF_AGENCY_NSF  "NSF"

#define ASF_DT_TAKEN_Y  "YES"
#define ASF_DT_TAKEN_N  "NOT"

#define ASF_MTYPE_AS    "AS"
#define ASF_MTYPE_WS    "WS"

#define ASF_SRCDEST_ASF   "ASF"
#define ASF_SRCDEST_APS   "APS"
#define ASF_SRCDEST_WALPS "WFF"
#define ASF_SRCDEST_CSA   "CSA"
#define ASF_SRCDEST_RGS   "RGS"
#define ASF_SRCDEST_FAIF  "FAI"

#define ASF_XMITTER_E1    "00"
#define ASF_XMITTER_E2    "01"
#define ASF_XMITTER_J8150 "F1"
#define ASF_XMITTER_J8350 "F2"
#define ASF_XMITTER_R8105 "F3"
#define ASF_XMITTER_R8230 "F4"

#endif /* _ASF_ */

/* End of File */
