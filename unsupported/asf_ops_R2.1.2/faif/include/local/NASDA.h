/*==============================================================================
Filename:	NASDA.h

Description:
	Header file for NASDA functions.  Includes NASDA keyword #define's,
keyword string lengths, etc.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		
	Refer to the FAIF Software Requirements Document (JPL-D11909) for a
list of file types supported through this header file.

SCCS Info:
   @(#)NASDA.h	1.3
==============================================================================*/

#ifndef _NASDA_
#define _NASDA_

#define NASDA_ID_STRLEN       4
 
/* ASF bound NASDA file types 
*/
#define NASDA_ELMF 100 /* Orbit Data : State Vector and Time Correlation */
#define NASDA_OPLN 101 /* JERS-1 Operations Plan */
#define NASDA_REQA 102 /* Reply to quarterly request */
#define NASDA_REQM 103 /* Request for Mission Data Recorder (MDR) tape dump */
#define NASDA_MSGC 104 /* HDDT readability report */
#define NASDA_MSGN 105 /* Satellite status information */

/* NASDA bound files
*/
#define NASDA_REQQ 106 /* Quarterly Request */
#define NASDA_REQW 107 /* Weekly Request */
#define NASDA_REAC 108 /* Acquisition Result */
#define NASDA_CATA 109 /* Catalogue Data */
#define NASDA_MSGM 110 /* HDDT shipment report */
#define NASDA_MSGF 111 /* Station Status information */
#define NASDA_MSGE 112 /* Station Unavailability */

/* NASDA file type strings
*/
#define NASDA_ELMF_STR "NASDA_ELMF" 
#define NASDA_OPLN_STR "NASDA_OPLN"
#define NASDA_REQA_STR "NASDA_REQA"
#define NASDA_REQM_STR "NASDA_REQM"
#define NASDA_MSGC_STR "NASDA_MSGC"
#define NASDA_MSGN_STR "NASDA_MSGN"
#define NASDA_REQQ_STR "NASDA_REQQ" 
#define NASDA_REQW_STR "NASDA_REQW"
#define NASDA_REAC_STR "NASDA_REAC"
#define NASDA_CATA_STR "NASDA_CATA"
#define NASDA_MSGM_STR "NASDA_MSGM"
#define NASDA_MSGF_STR "NASDA_MSGF"
#define NASDA_MSGE_STR "NASDA_MSGE"

/* NASDA defined header file type strings
*/
#define NASDA_ELMF_HSTR "ELMF" 
#define NASDA_TCE_HSTR  "TIME CORRELATION ELEMENTS"
#define NASDA_SV_HSTR   "STATE VECTORS"
#define NASDA_OPLN_HSTR "OPLN"
#define NASDA_REQA_HSTR "REQA"
#define NASDA_REQM_HSTR "REQM"
#define NASDA_MSGC_HSTR "MSGC"
#define NASDA_MSGN_HSTR "MSGN"
#define NASDA_REQQ_HSTR "REQQ" 
#define NASDA_REQW_HSTR "REQW"
#define NASDA_REAC_HSTR "REAC"
#define NASDA_CATA_HSTR "CATA"
#define NASDA_MSGM_HSTR "MSGM"
#define NASDA_MSGF_HSTR "MSGF"
#define NASDA_MSGE_HSTR "MSGE"

/* defines for NASDA header info
*/
#define NASDA_HEADER_SIZE       128
#define NASDA_MAX_FILENAME_LEN    8
#define NASDA_MAX_PROJNAME_LEN    4
#define NASDA_GRND_STA_NAME_LEN   4
#define NASDA_FILE_FORM_DATE      8
#define NASDA_FILE_FORM_TIME      8
#define NASDA_DATAREC_FIELD_LEN   4
#define NASDA_RECNUM_FIELD_LEN    5
#define NASDA_EXTRA_HEADER_BYTES 82
 
#endif /* _NASDA_ */

/* End of File */
