/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
 
/*==============================================================================
Filename:	WALPSar.h
Description:	
	Header file for WALLOPS availability response file functions.
	 
Creator:        Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:
	Refer to the ASF to Wallops Flight Facility (WFF) Interface
Specification Document (ISD)

1.  October '96 - RH
    Updated for ISD Version 2.3

SCCS Info:
   @(#)WALPSar.h	1.2
==============================================================================*/

#ifndef _WALLOPSAR_
#define _WALLOPSAR_

#ifndef ERROR
#include "faifdefs.h"
#endif

#include "WALPS.h"
#include "odldef.h"
#include "odlinter.h"

#define WALPS_ARRECORD_NAME "AVAILABILITY_RESPONSE_RECORD"
#define WALPS_RES_SUFFIX     "WALPS_AVAIL_RESP"
#define MAX_RESKEYVAL_STRLEN  256
 
typedef struct wallops_ar_record
{
   char file_name[MAX_RESKEYVAL_STRLEN*10] ;
   char sensor[MAX_RESKEYVAL_STRLEN] ;
   struct ODLDate time_on ;
   struct ODLDate time_off ;
} WALPS_AR_Record ;


#endif /* _WALLOPSAR_ */

/* End of File */
