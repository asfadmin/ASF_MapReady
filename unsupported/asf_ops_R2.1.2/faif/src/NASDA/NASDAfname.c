/*==============================================================================
Filename:	NASDAfname.c

Description:
	This module contains the function(s) used for determining if a
filename is a NASDA type file name - if it conforms to NASDA's file 
naming convention.

External Functions:
	parse_NASDA_filename
	
Static Functions:
	None
	
External Variables Defined:
	NASDA_File_Id_Table

File Scope Static Variables:
	None
	
Notes:

==============================================================================*/

static char SccsFile[] = "%M%" ;
static char SccsRevision[] = "%R%" ;
static char SccsDate[] = "%G%";
static char SccsLastChanger[] = "%W%";
static char SccsState[] = "%I%";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <syslog.h>
#include "faifdefs.h"
#include "NASDA.h"


#ifdef __STDC__
int parse_NASDA_filename(char *) ;
#else
int parse_NASDA_filename() ;
#endif

extern void *util_do_malloc() ;
 
File_Identifier NASDA_File_Id_Table[] =
{
   { NASDA_ELMF, NASDA_ELMF_HSTR }, /* Orbit Data : State Vector and
				    -- Time Correlation 
				    */
   { NASDA_OPLN, NASDA_OPLN_HSTR }, /* JERS-1 Operations Plan */
   { NASDA_REQA, NASDA_REQA_HSTR }, /* Reply to quarterly request */
   { NASDA_REQM, NASDA_REQM_HSTR }, /* Request for Mission Data Recorder (MDR)
				    -- tape dump 
				    */
   { NASDA_MSGC, NASDA_MSGC_HSTR }, /* HDDT readability report */
   { NASDA_MSGN, NASDA_MSGN_HSTR }, /* Satellite status info */
   { NASDA_REQQ, NASDA_REQQ_HSTR }, /* Quarterly Request */
   { NASDA_REQW, NASDA_REQW_HSTR }, /* Weekly Request */
   { NASDA_REAC, NASDA_REAC_HSTR }, /* Acquisition Result */
   { NASDA_CATA, NASDA_CATA_HSTR }, /* Catalogue Data */
   { NASDA_MSGM, NASDA_MSGM_HSTR }, /* HDDT shipment report */
   { NASDA_MSGF, NASDA_MSGF_HSTR }, /* Station Status information */
   { NASDA_MSGE, NASDA_MSGE_HSTR }, /* Station Unavailability */
   { SENTINEL,   NULL            }
} ;




/*==============================================================================
Function:	int parse_NASDA_filename(filename)

Description:
	Parse and validate the input string filename as a NASDA
filename.  This is an initial means of determining the file type of a
specific flight agency file.  If filename conforms to the NASDA file
naming convention, then it is further validated as an NASDA file.  The
table NASDA_File_Id_Table is consulted to validate file id information
that are obtained from the filename.  The return status of this
function is the file id code for a valid file type or ERROR if the
filename is invalid.

Parameters:
	filename - Filename part of a file's filepath specification.
Note that the names used must be valid Unix filenames.

Returns:	valid NASDA file type or ERROR
Creator:	Norbert Piega	
Creation Date:	06/20/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
parse_NASDA_filename(char *filename)
#else
int
parse_NASDA_filename(filename)
   char *filename ;
#endif
{
   int i, type_matched = ERROR ;
   char file_id[NASDA_ID_STRLEN+1] ;
   char *start, *fname ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   /* Precaution.  Skip past the last '/' in the file specification
   */
   if ((start = strrchr(filename, '/')) != NULL)
   {
      fname = (char *)util_do_malloc(sizeof(char)*(strlen(start+1)+1)) ;
      strcpy(fname, start+1) ;
   }
   else
      fname = filename ;

   if ((int)strlen(fname) < NASDA_ID_STRLEN)
   {
      sprintf(logmsg, "WARNING, %s is an invalid NASDA file name\n", fname) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }

   /* Compare file id from filename to table of
   -- file types and associated ids
   */
   file_id[0] = toupper(fname[0]) ;
   file_id[1] = toupper(fname[1]) ;
   file_id[2] = toupper(fname[2]) ;
   file_id[3] = toupper(fname[3]) ;
   file_id[4] = '\0' ;
   i = 0 ;
   while (NASDA_File_Id_Table[i].file_id_number != SENTINEL)
      if (strcmp(NASDA_File_Id_Table[i].file_identifier, file_id) == 0)
      {
         type_matched = NASDA_File_Id_Table[i].file_id_number ;
         break ; 
      }
      else
         i++ ;

   return(type_matched) ;

} /* parse_NASDA_filename */


/* End of File */
