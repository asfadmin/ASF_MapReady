/*==============================================================================
Filename:	CSAfname.c

Description:
	This module contains the function(s) used for determining if a
filename is a CSA type file name - if it conforms to CSA's file naming
convention documented in Mission Control System to U.S. And Foreign Data
Reception Operation Centres (UFDROC) Interface Control Document,
Radarsat Mission Control System Project CDRL SE-7 of Contract S.700035.

External Functions:
	parse_CSA_filename
	
Static Functions:
	None
	
External Variables Defined:
	CSA_File_Id_Table
	
File Scope Static Variables:
	None
	
Notes:
1.  Feb. '96 -- R. Hoffman --
    (a) Use get_CSAstvec_precision to distinguish between .orb files (as in
        CSAhdr.c)
    (b) Explicitly check for ERROR return from get_CSAstvec_precision.
2.  March '96 -- R. Hoffman 
    parse_CSA_hdr() is no longer used.  But we need the portion of it
    that distinguishes between ASF and MCM files here in parse_CSA_filename()
==============================================================================*/

static char SccsFile[] = "CSAfname.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "23 Feb 1996";
static char SccsLastChanger[] = "@(#)CSAfname.c	1.2";
static char SccsState[] = "1.2";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <syslog.h>
#include "CSA.h"


#ifdef __STDC__
int parse_CSA_filename(char *) ;
#else
int parse_CSA_filename() ;
#endif

extern void *util_do_malloc() ;

File_Identifier CSA_File_Id_Table[] =
{
   { CSA_PREDORBIT,     CSA_FEXT_ORBDATA    }, /* Predicted Orbit */
   { CSA_DEFVORBIT,     CSA_FEXT_ORBDATA    }, /* Definitive Orbit */
   { CSA_RECRQST,       CSA_FEXT_RECRQST    }, /* Reception request */
   { CSA_RECSCHED,      CSA_FEXT_RECSCHED   }, /* Reception schedule */
   { CSA_CALIBRQST,     CSA_FEXT_CALRQST    }, /* Calibration request */
   { CSA_CALIBSCHED,    CSA_FEXT_CALSCHED   }, /* Calibration schedule */
   { CSA_SARPROCPRM,    CSA_FEXT_SARPROCPRM }, /* SAR Processing parameters */
   { CSA_ARCHSTRGRPT,   CSA_FEXT_ASR        }, /* Archive Storage Report */
   { CSA_RECRPT,        CSA_FEXT_RECRPT     }, /* Reception Report */
   { CSA_RECAVAILRPT,   CSA_FEXT_RAR        }, /* Reception Avail. Report */
   { CSA_CALIBRPT,      CSA_FEXT_CALRPT     }, /* Calib. Report */
   { CSA_CALIBAVAILRPT, CSA_FEXT_CAR        }, /* Calib. Avail. Report */
   { SENTINEL,          NULL                }
} ;



/*==============================================================================
Function:	int parse_CSA_filename(filename)

Description:
	Parse and validate the input string filename as a CSA
filename.  This is an initial means of determining the file type of a
specific flight agency file.  If filename conforms to the CSA file
naming convention, then it is further validated as a CSA file.  The
table CSA_File_Id_Table is consulted to validate file id information
that are obtained from the filename.  The return status of this
function is the file id code for a valid file type or ERROR if the
filename is invalid.

Parameters:
        filename - Filename part of a file's filepath specification.
Note that the names used must be valid Unix filenames.

Returns:	
	ERROR - unable to recognize file from filename specified
	TYPE_MATCHED - type of file determined from filename

Creator:	Norbert Piega	
Creation Date:	06/20/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
parse_CSA_filename(char *filename)
#else
int
parse_CSA_filename(filename)
   char *filename ;
#endif
{
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
   int i, type_matched = ERROR ;
   char file_id[CSA_ID_STRLEN+1] ;
   char *start, *fname, *dot ;
   int status;

   /* Skip past the '/' in the file specification
   */
   if ((start = strrchr(filename, '/')) != (char *)NULL)
   {
      fname = (char *)util_do_malloc(sizeof(char)*(strlen(start+1)+1)) ;
      strcpy(fname, start+1) ;
   }
   else
      fname = filename ;

   /* Look for the filename extension (ext in basename.ext)
   */
   if ((dot = strchr(fname, '.')) == (char *)NULL)
   {
      sprintf(logmsg, "WARNING, %s missing extension. Invalid CSA file name\n", fname) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }

   /* Check extension name length
   */
   dot++ ;
   if ((int)strlen(dot) < CSA_ID_STRLEN)
   {
      sprintf(logmsg, "WARNING, invalid CSA file extension name %s\n", dot) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }

   /* Compare extension name to table of
   -- file types and associated extensions
   */
   file_id[0] = toupper(dot[0]) ;
   file_id[1] = toupper(dot[1]) ;
   file_id[2] = toupper(dot[2]) ;
   file_id[3] = '\0' ;
   i = 0 ;
   while (CSA_File_Id_Table[i].file_id_number != SENTINEL)
      if (strcmp(CSA_File_Id_Table[i].file_identifier, file_id) == 0)
      {
         type_matched = CSA_File_Id_Table[i].file_id_number ;
         if (strcmp(file_id, CSA_FEXT_ORBDATA) == 0)
         {
            status = get_CSAstvec_precision(filename) ;
            /* if status == ERROR (because file can't be opened, for example),
               call it predicted anyway. 
            */
            if ((status == ERROR) || (status == TRUE)) 
              type_matched = CSA_PREDORBIT ;
            else type_matched = CSA_DEFVORBIT ;
	 }
         break ; 
      }
      else
         i++ ;

   /* Distinguish between ASF and MCM filetypes */
   if (type_matched == 207 && strncmp(fname + 8,"_f.", 3) == 0)
      type_matched = 202;
   if (type_matched == 202 && strncmp(fname + 8,"_m.", 3) == 0)
      type_matched = 207;
   if (type_matched == 208 && strncmp(fname + 8,"_f.", 3) == 0)
      type_matched = 203;
   if (type_matched == 203 && strncmp(fname + 8,"_m.", 3) == 0)
      type_matched = 208;

   return(type_matched) ;

} /* parse_CSA_filename */


/* End of File */
