/*==============================================================================
Filename:	WALPShdr.h

Description:	
	 This contains the data structure definition for the Wallops file
header record.
	 
Creator:        Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:
	Refer to the FAIF Software Requirements Document (JPL-D11909) for a
list of file types supported through this header file.

SCCS Info:
   %W%
==============================================================================*/

#ifndef _WALPSHDR_
#define _WALPSHDR_

#ifndef ERROR
#include "faifdefs.h"
#endif

#include "WALPS.h"
#include "odldef.h"
#include "odlinter.h"

/* Wallops Header record structure
*/
typedef struct wallops_header_record
{
   int  num_records ;                            /* Number of records */
   char file_name[MAX_FILENAME_LEN] ;             /* File name */
   char spacecraft_id[MAXLINE] ;                 /* Spacecraft Identifier */
   char file_creation_time[TIME_STRING_LEN+1] ;  /* File Creation Time */
   struct ODLDate date_time ;                    /* ODL structure used to
						 ** store creation time
						 */
   char file_source[MAXLINE] ;                   /* File Source */
   char file_dest[MAXLINE] ;                     /* File Destination */
   char file_type[MAXLINE] ;                     /* File Type */
   int file_type_id ;                            /* Filt Type Id Number */

} WALPS_Header_Record ;

#endif /* _WALPSHDR_ */

/* End of File */
