/*==============================================================================
Filename:	ASFhdr.h

Description:	
	This header file contains the data structure definition for
the ASF header record.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		

SCCS Info:
   %W%
==============================================================================*/

#ifndef _ASFHDR_
#define _ASFHDR_

#include "ASF.h"

/* ASF Header Record structure
*/
typedef struct asf_header_rec
{
   char year[ASFHDR_YEAR+1] ;
   char time[ASFHDR_TIME+1] ;
   char msg_type[ASFHDR_MSGT+1] ;
   char dest[ASFHDR_DEST+1] ;
   char origin[ASFHDR_ORGN+1] ;
   char spare[ASFHDR_SPARE+1] ;

} ASF_Header_Record ;

#endif /* _ASFHDR_ */

/* End of File */
