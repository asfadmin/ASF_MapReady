/*==============================================================================
Filename:	filerec.h
Description:	Type definition for File_Record 
Creator:	Norbert Piega	
Notes:		

SCCS Info:
   %W%
==============================================================================*/
#ifndef _FILEREC_
#define _FILEREC_

#ifndef ERROR
#include "faifdefs.h"
#endif

/* File to Route Record Structure
*/
typedef struct file_rec
{
   char file_name[MAX_FILENAME_LEN] ;
   int file_type_status ;
   char *flight_agency ;
   char file_dest[MAX_DIRNAME_LEN] ;
   char time_received[TIME_STRING_LEN+1] ;
   char time_forwarded[TIME_STRING_LEN+1] ;
   LOGICAL routed_flag ;
   LOGICAL logged_flag ;
   LOGICAL translated_flag ;
   struct file_rec *next_filerec ;

} File_Record ;

#endif /* _FILEREC_ */

/* End of File */
