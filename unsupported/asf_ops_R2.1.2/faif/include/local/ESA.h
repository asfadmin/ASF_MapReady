/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
 
/*==============================================================================
Filename:	ESA.h

Description:
	Header file for ESA functions.  Includes ESA header record and
other type definitions, ESA keywords table, #defines.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		
	Refer to the FAIF Software Requirements Document (JPL-D11909) for
list of file types supported through this header file.
        1.  R. Hoffman - Sept. 96
            Added SHAQP

SCCS Info:
   @(#)ESA.h	1.3
==============================================================================*/

#ifndef _ESA_
#define _ESA_

#ifndef ERROR
#include "faifdefs.h"
#endif

#define ESA_ID_STRLEN       4

/* ESA file types : Inbound 
*/
#define ESA_ORPD  0 /* Predicted Orbit */
#define ESA_ORRE  1 /* Restituted Orbit */
#define ESA_ODMC  2 /* Medium Copy Order */
#define ESA_ODMR  3 /* Medium Release Order */
#define ESA_SHAQ  4 /* Acquisition Schedule */
#define ESA_RQST  5 /* Acquisition Status */
#define ESA_PATC  6 /* Time Correlation */
#define ESA_RQVR  7 /* RQVR */
#define ESA_MPSG  8 /* Global Activity Plan */
#define ESA_SHAQP 9 /* Preliminary Acquisition Schedule */

/* ESA file types : Outbound
*/
#define ESA_RQUS  9 /* Acquisition Request */
#define ESA_REAQ 10 /* Acquisition Report */
#define ESA_RESM 11 /* Shipment Report */
#define ESA_REEX 12 /* Extracted Data */
#define ESA_REUG 13 /* Unavailability Report */
 
/* ESA file type strings
*/
#define ESA_ORPD_STR  "ESA_ORPD"
#define ESA_ORRE_STR  "ESA_ORRE"
#define ESA_ODMC_STR  "ESA_ODMC"
#define ESA_ODMR_STR  "ESA_ODMR"
#define ESA_SHAQ_STR  "ESA_SHAQ"
#define ESA_RQST_STR  "ESA_RQST"
#define ESA_PATC_STR  "ESA_PATC"
#define ESA_RQVR_STR  "ESA_RQVR"
#define ESA_MPSG_STR  "ESA_MPSG"
#define ESA_SHAQP_STR "ESA_SHAQP"

#define ESA_RQUS_STR  "ESA_RQUS"
#define ESA_REAQ_STR  "ESA_REAQ"
#define ESA_RESM_STR  "ESA_RESM"
#define ESA_REEX_STR  "ESA_REEX"
#define ESA_REUG_STR  "ESA_REUG"


/* defines for ESA header info
-- Mostly unused in R1A
*/
#define ESA_HEADER_SIZE       30
#define ESA_FILE_ID_SIZE       5 /* 4 characters plus "_" */
#define ESA_DATE_SIZE          6
#define ESA_ORIGINATOR         2
#define ESA_FILE_DEST          2
#define ESA_CYCLIC_COUNTER     4
#define ESA_SATELLITE_ID_SIZE  2
#define ESA_TIME_SIZE          8


/* ESA defined file type strings in file header
*/
#define ESA_ORPD_HSTR  "ORPD"
#define ESA_ORRE_HSTR  "ORRE"
#define ESA_SV_HSTR    "STATE VECTORS"
#define ESA_ODMC_HSTR  "ODMC"
#define ESA_ODMR_HSTR  "ODMR"
#define ESA_SHAQ_HSTR  "SHAQ"
#define ESA_RQST_HSTR  "RQST"
#define ESA_PATC_HSTR  "PATC"
#define ESA_TCE_HSTR   "TIME CORRELATION ELEMENTS"
#define ESA_RQVR_HSTR  "RQVR"
#define ESA_MPSG_HSTR  "MPSG"
#define ESA_SHAQP_HSTR "SHAQ"  /* same IMS dataset as SHAQ */

#define ESA_RQUS_HSTR  "RQUS"
#define ESA_REAQ_HSTR  "REAQ"
#define ESA_RESM_HSTR  "RESM"
#define ESA_REEX_HSTR  "REEX"
#define ESA_REUG_HSTR  "REUG"

#endif /* _ESA_ */
 
/* End of File */
