/*==============================================================================
Filename:	CSA.h
Description:	
	Header file for CSA functions.  Includes CSA header record and
other type definitions, CSA keywords table, #defines.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		
	Refer to FAIF Software Requirements Document (JPL-D11909) for a
list of file types supported through this header file.

SCCS Info:
   @(#)CSA.h	1.5
==============================================================================*/

#ifndef _CSA_
#define _CSA_

#ifndef ERROR
#include "faifdefs.h"
#endif

/* CSA Keyword Table entry record structure
*/
typedef struct csa_keywords
{
   int id ;                                      /* Keyword Id */
   char *keyword ;                               /* Keyword string */

} CSA_Keywords ;

#define CSA_ID_STRLEN       3

/* ASF bound CSA file types 
*/
#define CSA_PREDORBIT      200 /* Predicted Orbit */
#define CSA_DEFVORBIT      201 /* Definitive Orbit */
#define CSA_RECRQST        202 /* Reception request */
#define CSA_RECSCHED       203 /* Reception schedule */
#define CSA_CALIBRQST      204 /* Calibration request */
#define CSA_CALIBSCHED     205 /* Calibration schedule */
#define CSA_SARPROCPRM     206 /* SAR Processing parameters */
#define CSA_RRQ_MCM        207 /* Reception request for McMurdo */
#define CSA_RSH_MCM        208 /* Reception schedule for McMurdo */
#define CSA_CRQ_MCM        209 /* Calibration request for McMurdo */
#define CSA_CSH_MCM        210 /* Calibration schedule for McMurdo */

/* CSA bound file types
*/
#define CSA_ARCHSTRGRPT    211 /* Archive Storage Report */
#define CSA_RECRPT         212 /* Reception Report */
#define CSA_RECAVAILRPT    213 /* Reception Availability Report */
#define CSA_CALIBRPT       214 /* Calibration Report */
#define CSA_CALIBAVAILRPT  215 /* Calibration Availability Report */

#define CSA_RRP_MCM        216 /* Reception Report */
#define CSA_RAR_MCM        217 /* Reception Availability Report */
#define CSA_CRP_MCM        218 /* Calibration Report */
#define CSA_CAR_MCM        219 /* Calibration Availability Report */

/* CSA file type strings
*/
#define CSA_PREDORBIT_STR      "CSA_PREDORBIT"
#define CSA_DEFVORBIT_STR      "CSA_DEFVORBIT"
#define CSA_RECRQST_STR        "CSA_RECRQST" 
#define CSA_RECSCHED_STR       "CSA_RECSCHED"
#define CSA_CALIBRQST_STR      "CSA_CALIBRQST"
#define CSA_CALIBSCHED_STR     "CSA_CALIBSCHED"
#define CSA_SARPROCPRM_STR     "CSA_SARPROCPRM"
#define CSA_RRQ_MCM_STR        "CSA_RRQ_MCM"
#define CSA_RSH_MCM_STR        "CSA_RSH_MCM"
#define CSA_CRQ_MCM_STR        "CSA_CRQ_MCM"
#define CSA_CSH_MCM_STR        "CSA_CSH_MCM"

#define CSA_ARCHSTRGRPT_STR    "CSA_ARCHSTRGRPT" 
#define CSA_RECRPT_STR         "CSA_RECRPT"
#define CSA_RECAVAILRPT_STR    "CSA_RECAVAILRPT"
#define CSA_CALIBRPT_STR       "CSA_CALIBRPT"
#define CSA_CALIBAVAILRPT_STR  "CSA_CALIBAVAILRPT"

#define CSA_RRP_MCM_STR        "CSA_RRP_MCM"
#define CSA_RAR_MCM_STR        "CSA_RAR_MCM"
#define CSA_CRP_MCM_STR        "CSA_CRP_MCM"
#define CSA_CAR_MCM_STR        "CSA_CAR_MCM"

/*
-- Note : Modify these as necessary.  
--        Ex. MAX_KEYTABLE_SIZE when 
--            adding entries.
*/
#define HEADER_KEYWDS       8 
#define MAX_KEYTABLE_SIZE  32 
 
#define HEADER_START        0
#define FILENAME            1
#define SPACECRAFT_ID       2
#define FILE_CREATION_TIME  3
#define FILE_SOURCE         4
#define FILE_DEST           5
#define FILE_TYPE           6
#define ENDFILE_LINE        7

#define SATELLITE_NAME      0
#define GENTIME_KEYWORD     1
#define ORBITNUM_KEYWORD    2
#define GREENWCHA_KEYWORD   3


/* Some expected identifiers/lex items defined by CSA
*/
#define HEADER_START_STR       ";###"
#define FILENAME_STR           "FILENAME"
#define SPACECRAFT_ID_STR      "SPACECRAFT_IDENTIFIER"
#define FILE_CREAT_TIME_STR    "FILE_CREATION_TIME"
#define FILE_SOURCE_STR        "FILE_SOURCE"
#define FILE_DEST_STR          "FILE_DEST"
#define FILE_TYPE_STR          "FILE_TYPE"
#define ENDFILE_LINE_STR       ";###END_OF_FILE"

#define CSA_FSRC_MCS         "MCS"
#define CSA_FDEST_UFDROC     "UFDROC"

#define CSA_SPACECRAFT_ID    "RADARSAT_1"
#define CSA_RECREP_FILESRC   "FBDROC"

#define CSA_FTYPE_ORBDATA    "ORBIT_DATA"
#define CSA_FTYPE_RECRQST    "CSA_RECRQST"
#define CSA_FTYPE_RECSCHED   "CSA_RECSCHED"
#define CSA_FTYPE_CALRQST    "CSA_CALIBRQST"
#define CSA_FTYPE_CALSCHED   "CSA_CALIBSCHED"
#define CSA_FTYPE_SARPROCPRM "CSA_SARPROCPRM"

#define CSA_FTYPE_ASR        "ARCHIVE_STORAGE_REPORT"
#define CSA_FTYPE_RECRPT     "RECEPTION_REPORT"
#define CSA_FTYPE_RECRQST_M  "RECEPTION_REQUEST"
#define CSA_FTYPE_RECSCHED_M "RECEPTION_SCHEDULE"
#define CSA_FTYPE_RAR        "CSA_RECAVAILRPT"
#define CSA_FTYPE_CALRPT     "CSA_CALIBRPT"
#define CSA_FTYPE_CAR        "CSA_CALIBAVAILRPT"

#define CSA_NUM_VECTORS      15

/* Filename extensions defined by CSA
-- Used to identify inbound CSA files
*/
#define CSA_FEXT_ORBDATA    "ORB"
#define CSA_FEXT_RECRQST    "RRQ"
#define CSA_FEXT_RECSCHED   "RSH"
#define CSA_FEXT_CALRQST    "CRQ"
#define CSA_FEXT_CALSCHED   "CSH"
#define CSA_FEXT_SARPROCPRM "PAP"

#define CSA_FEXT_ASR        "ASR"
#define CSA_FEXT_RECRPT     "RRP"
#define CSA_FEXT_RAR        "RAR"
#define CSA_FEXT_CALRPT     "CRP"
#define CSA_FEXT_CAR        "CAR"

/* Dataset names */
#define CSA_SV_HSTR  "STATE VECTORS"

#endif /* _CSA_ */

/* End of File */
