/*==============================================================================
Filename:	ESAhdr.c

Description:
	ESA file header parser function.  This module contains the 
function for parsing ESA headers.  The specification of the ESA 
file format is described in the Earthnet ERS-1 Central Facility to
National and Foreign Stations Interface Specification document.

External Functions:
	parse_ESA_hdr

Static Functions:
	None

External Variables Defined:
	None	

File Scope Static Variables:
	None

Notes:
1.  R. Hoffman - Sept. 96
    Added SHAQP handling

==============================================================================*/

static char SccsFile[] = "ESAhdr.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "22 Jan 1996";
static char SccsLastChanger[] = "@(#)ESAhdr.c	1.1";
static char SccsState[] = "1.1";

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <syslog.h>
#include "ESA.h"

#ifdef __STDC__
int parse_ESA_hdr(char *) ;
#else
int parse_ESA_hdr() ;
#endif


extern File_Identifier ESA_File_Id_Table[] ;




/*==============================================================================
Function:	int parse_ESA_hdr(char *filepath)

Description:
	Parse the header portion of the input ESA file specified by
filepath to determine its file type.  Return the obtained file type if
the parse is successful otherwise, return an ERROR.  The table
ESA_File_Id_Table is used to determine what file type status is
returned.

Parameters:
	char *filepath - file path of input ESA file to parse

Returns:
	ERROR - Unable to recognize file from filename
	TYPE_MATCHED - type of ESA file determined from filename 

Creator:	Norbert Piega	
Creation Date:	06/20/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
parse_ESA_hdr(char *filepath)
#else
int
parse_ESA_hdr(filepath)
   char *filepath ;
#endif
{
   FILE *fp ;
   char header[ESA_HEADER_SIZE+1] ;
   char *esa_filename;
   int ftype = ERROR ;
   int i ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   /*  SHAQP does not have a standard ESA header.  Catch it here. */
   esa_filename = strrchr (filepath, '/');
   if (esa_filename == NULL) esa_filename = filepath;
   else esa_filename++;    /* begin after the slash */
   if (strncmp(esa_filename, "SHQP_", 5) == 0) return (ESA_SHAQP);

   /* Open the ESA file
   */
   if ((fp = fopen(filepath, "r")) == (FILE *)NULL)
   {
      sprintf(logmsg, 
         "WARNING, unable to open file %s for header parse.\n", filepath) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }

   /* Read out the file header portion of the file
   */
   fread(header, sizeof(char), ESA_HEADER_SIZE, fp) ;
   header[ESA_HEADER_SIZE] = '\0' ;

   fclose(fp) ;

   /* Search file id table for the matching file id
   */
   for (i=0; ESA_File_Id_Table[i].file_id_number != SENTINEL; i++)
      if (strncmp(header, ESA_File_Id_Table[i].file_identifier,
               ESA_ID_STRLEN) == 0)
      {
         ftype = ESA_File_Id_Table[i].file_id_number ;
         break ;
      }

   if (ftype == ERROR) 
   {
      syslog(LOG_ERR, "WARNING, invalid file header contents.\n") ;
   }

   return(ftype) ;

} /* parse_ESA_hdr */

/* End of File */
