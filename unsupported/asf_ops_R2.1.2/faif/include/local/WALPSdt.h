/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
 
/*==============================================================================
Filename:	WALPSdt.h
Description:	
	Header file for WALLOPS datatake file functions.
	 
Creator:        Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:
	Refer to the ASF to Wallops Flight Facility (WFF) Interface
Specification Document (ISD)
1.  Oct. '96 - RH
    Updated to ISD Version 2.3

SCCS Info:
   @(#)WALPSdt.h	1.1
==============================================================================*/

#ifndef _WALLOPSDT_
#define _WALLOPSDT_

#ifndef ERROR
#include "faifdefs.h"
#endif

#include "WALPS.h"
#include "odldef.h"
#include "odlinter.h"

#define WALPS_DTRECORD_NAME "DOWNLINK_RECORD"
#define WALPS_DT_SUFFIX     "WALPS_DT"
#define WALPS_DT_ASF_SUFFIX "DATATAKE MESSAGE"
#define MAX_DTKEYVAL_STRLEN  256
 
typedef struct wallops_dt_record
{
   char file_name[MAX_DTKEYVAL_STRLEN*10] ;
   char satellite[MAX_DTKEYVAL_STRLEN] ;

} WALPS_DT_Record ;


#endif /* _WALLOPSDT_ */

/* End of File */
