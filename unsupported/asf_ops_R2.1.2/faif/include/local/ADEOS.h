/*==============================================================================
Filename:	ADEOS.h
Description:	ADEOS specific definitions
Creator:	Phil Yurchuk
Notes:		

SCCS Info:
   @(#)ADEOS.h	1.2
==============================================================================*/

#ifndef _ADEOS_
#define _ADEOS_

#define ADEOS_ID_STRLEN       4

/* Incoming ADEOS files
*/
#define ADEOS_REQR 400
#define ADEOS_RDRD 401 
#define ADEOS_ELMD 402
#define ADEOS_ELMP 403
#define ADEOS_OPL1 404
#define ADEOS_RPLN 405
#define ADEOS_ORST 406
#define ADEOS_STAD 407
#define ADEOS_TMDF 408

/* Outgoing
*/
#define ADEOS_STGS 409
#define ADEOS_REAC 410
#define ADEOS_SRRD 411

#define ADEOS_REQR_STR  "ADEOS_REQR"
#define ADEOS_RDRD_STR  "ADEOS_RDRD"
#define ADEOS_ELMD_STR  "ADEOS_ELMD"
#define ADEOS_ELMP_STR  "ADEOS_ELMP"
#define ADEOS_OPL1_STR  "ADEOS_OPL1"
#define ADEOS_RPLN_STR  "ADEOS_RPLN"
#define ADEOS_ORST_STR  "ADEOS_ORST"
#define ADEOS_STAD_STR  "ADEOS_STAD"
#define ADEOS_STGS_STR  "ADEOS_STGS"
#define ADEOS_REAC_STR  "ADEOS_REAC"
#define ADEOS_SRRD_STR  "ADEOS_SRRD"
#define ADEOS_TMDF_STR  "ADEOS_TMDF"

#define ADEOS_REQR_HSTR  "REQR"
#define ADEOS_RDRD_HSTR  "RDRD"
#define ADEOS_ELMD_HSTR  "ELMD"
#define ADEOS_ELMP_HSTR  "ELMP"
#define ADEOS_OPL1_HSTR  "OPL1"
#define ADEOS_RPLN_HSTR  "RPLN"
#define ADEOS_ORST_HSTR  "ORST"
#define ADEOS_STAD_HSTR  "STAD"
#define ADEOS_STGS_HSTR  "STGS"
#define ADEOS_REAC_HSTR  "REAC"
#define ADEOS_SRRD_HSTR  "SRRD"
#define ADEOS_TMDF_HSTR  "TMDF"

#define ADEOS_HEADER_SIZE       128
#define ADEOS_MAX_FILENAME_LEN    8
#define ADEOS_MAX_PROJNAME_LEN    4
#define ADEOS_GRND_STA_NAME_LEN   4
#define ADEOS_FILE_FORM_DATE      8
#define ADEOS_FILE_FORM_TIME      8
#define ADEOS_DATAREC_FIELD_LEN   4
#define ADEOS_RECNUM_FIELD_LEN    5
#define ADEOS_EXTRA_HEADER_BYTES 82


#endif

/* End of File */
