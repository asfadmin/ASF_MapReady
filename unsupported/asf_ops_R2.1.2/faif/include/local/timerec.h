/*==============================================================================
Filename:	timerec.h

Description:	
	This header file contains the typedef for the time record structure.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _TIMEREC_
#define _TIMEREC_

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
