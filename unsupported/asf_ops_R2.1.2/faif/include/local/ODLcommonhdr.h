/*==============================================================================
Filename:	 ODLcommonhdr.h

Description:	
	 This contains the data structure definition for a common file
header record.
	 
Creator:        Philip Yurchuk (phil@orca.jpl.nasa.gov)
Notes:
	Refer to the FAIF Software Requirements Document (JPL-D11909) for a
list of file types supported through this header file.

SCCS Info:
   %W%
==============================================================================*/
#ifndef _ODLCOMMONHDR_
#define _ODLCOMMONHDR_

#ifndef ERROR
#include "faifdefs.h"
#endif

#include "WALPS.h"
#include "odldef.h"
#include "odlinter.h"

/* Common Header record structure
*/
typedef struct ODL_common_header
{
   char file_creation_time[TIME_STRING_LEN+1] ;  /* File Creation Time */
   struct ODLDate date_time ;                    /* ODL structure used to
						 ** store creation time
						 */
   char file_source[MAXLINE] ;                   /* File Source */
   char file_dest[MAXLINE] ;                     /* File Destination */
   char file_type[MAXLINE] ;                     /* File Type */
   char number_of_records[MAXLINE] ;


} ODL_Common_Header ;

#endif /* _ODLCOMMONHDR_ */

/* End of File */
