/*==============================================================================
Filename:	ESAfname.c

Description:	
	This module contains the function parse_ESA_filename which checks
if a filename is a valid ESA filename - if conforms to the ESA file naming
convention.

External Functions:
	parse_ESA_filename

Static Functions:
	None

External Variables Defined:
	ESA_File_Id_Table

File Scope Static Variables:
	None

Notes:
1.  Nov. '96 - RH
    Modified to handle SHAQP
2.  April '97 - Rich Norman, RH
    Further modifications for SHAQP
==============================================================================*/

static char SccsFile[] = "ESAfname.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "22 Jan 1996";
static char SccsLastChanger[] = "@(#)ESAfname.c	1.1";
static char SccsState[] = "1.1";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <syslog.h>
#include "ESA.h"


#ifdef __STDC__
int parse_ESA_filename(char *) ;
#else
int parse_ESA_filename() ;
#endif

extern void *util_do_malloc() ;

File_Identifier ESA_File_Id_Table[] =
{
   { ESA_ORPD, ESA_ORPD_HSTR }, /* Predicted Orbit */
   { ESA_ORRE, ESA_ORRE_HSTR }, /* Restituted Orbit */
   { ESA_ODMC, ESA_ODMC_HSTR }, /* Medium Copy Order */
   { ESA_ODMR, ESA_ODMR_HSTR }, /* Medium Release Order */
   { ESA_SHAQ, ESA_SHAQ_HSTR }, /* Acquisition Schedule */
   { ESA_RQST, ESA_RQST_HSTR }, /* Acquisition Status */
   { ESA_PATC, ESA_PATC_HSTR }, /* Time Correlation */
   { ESA_RQVR, ESA_RQVR_HSTR }, /* Validation Result */
   { ESA_MPSG, ESA_MPSG_HSTR }, /* Global Activity Plan */
   { ESA_RQUS, ESA_RQUS_HSTR }, /* Acquisition Request */
   { ESA_REAQ, ESA_REAQ_HSTR }, /* Acquisition Report */
   { ESA_RESM, ESA_RESM_HSTR }, /* Shipment Report */
   { ESA_REEX, ESA_REEX_HSTR }, /* Extracted Data */
   { ESA_REUG, ESA_REUG_HSTR }, /* Unavailability Report */
   { SENTINEL, NULL          }
} ;



/*==============================================================================
Function:	int parse_ESA_filename(filename)

Description:
	Parse and validate the input string filename as a ESA
filename.  This is an initial means of determining the file type of a
specific flight agency file.  If filename conforms to the ESA file
naming convention, then it is further validated as an ESA file.  The
table ESA_File_Id_Table is consulted to validate file id information
that are obtained from the filename.  The return status of this
function is the file id code for a valid file type or ERROR if the
filename is invalid.


Parameters:  
   filename  - Filename part of a file's filepath specification.
Note that the names used must be valid Unix filenames.

Returns:  	
	ERROR - Unable to recognize file type from filename
	TYPE_MATCHED - type of ESA file type determined from filename

Creator:        Norbert Piega 
Creation Date:	06/20/1994 
Notes:
==============================================================================*/
#ifdef __STDC__
int
parse_ESA_filename(char *filename)
#else
int
parse_ESA_filename(filename)
   char *filename ; 
#endif 
{
   int i, type_matched = ERROR ;
   char file_id[ESA_ID_STRLEN+1] ;
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

   if ((int)strlen(fname) < ESA_ID_STRLEN)
   {
      sprintf(logmsg, "ERROR, %s is an invalid ESA file name\n", fname) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }

   if (islower(fname[0]))
   {
      sprintf (logmsg, "ERROR: Uncapitalized filename %s.\n", fname);
      syslog (LOG_ERR, logmsg);
      return (ERROR);
   }

   /* Handle SHAQP. */
   if (strncmp(fname, "SHQP_", 5) == 0) return (ESA_SHAQP);

   /* Just in case */
   if (strncmp(fname, "SHAQP", 5) == 0)
   {
      syslog(LOG_ERR, "ERROR: SHAQP needs renaming (to SHQP_).\n"); 
      return (ERROR);
   }

   /* Compare file id from filename to table of
   -- file types and associated ids */
   file_id[0] = fname[0] ;
   file_id[1] = fname[1] ;
   file_id[2] = fname[2] ;
   file_id[3] = fname[3] ;
   file_id[4] = '\0' ;

   i = 0 ;
   while (ESA_File_Id_Table[i].file_id_number != SENTINEL )
      if (strcmp(ESA_File_Id_Table[i].file_identifier, file_id) == 0)
      {
         type_matched = ESA_File_Id_Table[i].file_id_number ;
         break ; 
      }
      else
         i++ ;

   return(type_matched) ;

} /* parse_ESA_filename */


/* End of File */
