/*==============================================================================
Filename:	CSAhdr.h

Description:	
	This file contains the data structure definition for the CSA
file header record.

Creator:	Norbert Piega (norbert@orca.jpl.nasa.gov)
Notes:		
	Refer to FAIF Software Requirements Document (JPL-D11909) for a
list of file types supported through this header file.

SCCS Info:
   %W%
==============================================================================*/

#ifndef _CSAHDR_
#define _CSAHDR_

#ifndef ERROR
#include "faifdefs.h"
#endif

#include "CSA.h"

/* CSA Header record structure
*/
typedef struct csa_header_record
{
   char filename[MAX_FILENAME_LEN] ;             /* File name */
   char spacecraft_id[MAXLINE] ;                 /* Spacecraft Identifier */
   char file_creation_time[TIME_STRING_LEN+1] ;  /* File Creation Time */
   char file_source[MAXLINE] ;                   /* File Source */
   char file_dest[MAXLINE] ;                     /* File Destination */
   char file_type[MAXLINE] ;                     /* File Type */

} CSA_Header_Record ;

#endif /* _CSAHDR_ */

/* End of File */
