/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
 
/*==============================================================================
Filename:	WALPSsv.h
Description:	
	Header file for WALLOPS state vector file functions.
	 
Creator:        Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:
	Refer to the ASF to Wallops Flight Facility (WFF) Interface
Specification Document (ISD)
1.  October '96 - RH
    Updated for ISD Version 2.3

SCCS Info:
   @(#)WALPSsv.h	1.1
==============================================================================*/

#ifndef _WALPSSV_
#define _WALPSSV_

#ifndef ERROR
#include "faifdefs.h"
#endif

#include "WALPS.h"
#include "odldef.h"
#include "odlinter.h"

#define MAX_EPHKEYVAL_STRLEN  256
 
typedef struct wallops_sv_record
{
   char file_name[MAX_EPHKEYVAL_STRLEN*10] ;
   char satellite[MAX_EPHKEYVAL_STRLEN] ;
   char type[MAX_EPHKEYVAL_STRLEN] ;
   char coordinate[MAX_EPHKEYVAL_STRLEN] ;
   long xpos, ypos, zpos ;
   long xvel, yvel, zvel ;
   long rev ;
   struct ODLDate sv_time ;

} WALPS_SV_Record ;

#define WALPS_EPHRECORD_NAME     "EPHEMERIS_DATA"
#define WALPS_EPHMETARECORD_NAME "EPHEMERIS_METADATA"

#define TYPE         18
#define COORDINATE   19
#define XPOSITION    20
#define YPOSITION    21
#define ZPOSITION    22
#define XVELOCITY    23
#define YVELOCITY    24
#define ZVELOCITY    25

#define TYPE_KEYWD         "TYPE"
#define COORDINATE_KEYWD   "COORDINATE_SYSTEM"
#define XPOSITION_KEYWD    "X_POSITION"
#define YPOSITION_KEYWD    "Y_POSITION"
#define ZPOSITION_KEYWD    "Z_POSITION"
#define XVELOCITY_KEYWD    "X_VELOCITY"
#define YVELOCITY_KEYWD    "Y_VELOCITY"
#define ZVELOCITY_KEYWD    "Z_VELOCITY"

#define WALPS_EPHTYPE_PRED  "PREDICTED"
#define WALPS_EPHTYPE_REST  "RESTITUTED"

#define WALPS_EPHPRECIS_TE  "TRUE_EQUATORIAL"
#define WALPS_EPHPRECIS_MD  "MEAN_OF_DATE"

#endif /* _WALPSSV_ */

/* End of File */
