#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	 ODLcommonhdr.h

Description:	
	 This contains the data structure definition for a common file
header record.
	 
Creator:        Philip Yurchuk (phil@orca.jpl.nasa.gov)
Notes:
	Refer to the FAIF Software Requirements Document (JPL-D11909) for a
list of file types supported through this header file.

==============================================================================*/
#pragma ident	"@(#)APSodlcommonhdr.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/local/SCCS/s.APSodlcommonhdr.h"

#ifndef _APSODLCOMMONHDR_
#define _APSODLCOMMONHDR_

#ifndef ERROR
#include "faifdefs.h"
#endif

/* #include "WALPS.h"  mas 12/6 */
#include <odldef.h>
#include <odlinter.h>

/* Common Header record structure
*/
typedef struct APSodl_common_header
{
   char file_creation_time[TIME_STRING_LEN+1] ;  /* File Creation Time */
   struct ODLDate date_time ;                    /* ODL structure used to
						 ** store creation time
						 */
   char file_source[MAXLINE] ;                   /* File Source */
   char file_dest[MAXLINE] ;                     /* File Destination */
   char file_type[MAXLINE] ;                     /* File Type */
   char number_of_records[MAXLINE] ;


} APSodl_Common_Header ;

#endif /* _APSODLCOMMONHDR_ */

/* End of File */
