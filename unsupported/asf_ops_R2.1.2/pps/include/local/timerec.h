/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	timerec.h

Description:	
	This header file contains the typedef for the time record structure.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
==============================================================================*/

#ifndef _TIMEREC_
#define _TIMEREC_

#pragma ident "@(#)timerec.h	1.1  11/21/96"

#include "defs.h"

#define IS_LEAP_YEAR(x) (((x)%400==0) || (((x)%100!=0) && ((x)%4==0)))

/* Enumeration of units supported */
enum time_unit { unit_year, unit_day, unit_hour, unit_minute, unit_seconds } ;

/* Type to support time_unit enumerated type */
typedef enum time_unit Time_Unit ;

/* Enumeration of comparison values */
enum time_compare_t { lessthan, equal, greaterthan } ;

/* Type to support enumerated type */
typedef enum time_compare_t Time_Compare_Result ;

/* Time record structure
*/
typedef struct time_record
{
   char time_string[TIME_STRING_LEN+1] ;      /* Time string */
   int year, day, hour, minute ;              /* Time component values */
   double seconds ;

} Time_Record ;

#endif /* _TIMEREC_ */

/* End of File */
