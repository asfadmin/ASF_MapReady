/*************************************************************************
 * Copyright (c)1995, 1996 California Institute of Technology.           *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
 
/*==============================================================================
Filename:	WALPSwos.h

Description:	
	Header file for WALLOPS WOS file functions.  
	 
Creator:        Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:
	Refer to the ASF to Wallops Flight Facility (WFF) Interface
Specification Document (ISD)
1.  October '96 - RH
    Updated for ISD Version 2.3

SCCS Info:
   @(#)WALPSwos.h	1.1
==============================================================================*/

#ifndef _WALPSWOS_
#define _WALPSWOS_

#ifndef ERROR
#include "faifdefs.h"
#endif

#include "WALPS.h"
#include "odldef.h"
#include "odlinter.h"

#define WALPS_WOSRECORD_NAME  "WOS_RECORD"
#define WALPS_REQRECORD_NAME "AVAILABILITY_REQUEST_RECORD"
#define MAX_WOSKEYVAL_STRLEN  256
 
typedef struct wallops_wos_record
{
   char file_name[MAX_WOSKEYVAL_STRLEN*10] ;
   char satellite[MAX_WOSKEYVAL_STRLEN] ;
   char sensor[MAX_WOSKEYVAL_STRLEN] ;
   char mode[MAX_WOSKEYVAL_STRLEN] ;
   char activity_id[MAX_WOSKEYVAL_STRLEN] ;
   char agency[MAX_WOSKEYVAL_STRLEN] ;
   char sitename[MAX_WOSKEYVAL_STRLEN] ;
   char transmitter_id[MAX_WOSKEYVAL_STRLEN] ;
   long start_addr ;
   long end_addr ;
   int rev ;
   int num_datatakes ;
   struct ODLDate start_time ;
   struct ODLDate end_time ;

} WALPS_WOS_Record ;


#endif /* _WALPSWOS_ */

/* End of File */
