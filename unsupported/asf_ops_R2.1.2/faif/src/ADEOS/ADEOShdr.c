/*==============================================================================
Filename:	ADEOShdr.c

Description:
	ADEOS file header parser function.  This module contains the
function for parsing ADEOS headers.  The specification of the ADEOS
file format is described in the ADEOS Operation Interface Specification 
document.

External Functions:
	parse_ADEOS_hdr
	
Static Functions:
	None
	
External Variables Defined:
	None
	
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
#include <syslog.h>
#include "faifdefs.h"
#include "ADEOS.h"

#ifdef __STDC__
int parse_ADEOS_hdr(char *, int) ;
#else
int parse_ADEOS_hdr() ;
#endif


extern File_Identifier ADEOS_File_Id_Table[] ;
 



/*==============================================================================
Function:	int parse_ADEOS_hdr(filepath)

Description:
	Parse the header portion of the input ADEOS file specified by
filepath to determine its file type.  Return the obtained file type if
the parse is successful otherwise, return ERROR.  The table
ADEOS_File_Id_Table is used to determine what file id code is returned
as status.

Parameters:
	char *filepath - file path of input ADEOS file to parse

Returns:	ADEOS file type or NULL	
Creator:	Norbert Piega	
Creation Date:	10/04/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int
parse_ADEOS_hdr(char *fname, int ftype)
#else
int
parse_ADEOS_hdr(fname, ftype)
   char *fname ;
   int ftype ;
#endif
{
   FILE *fp ;
   char header[ADEOS_HEADER_SIZE+1] ;
   int i ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   /* Open the ADEOS file
   */
   if ((fp = fopen(fname, "r")) == (FILE *)NULL) 
   {
      sprintf(logmsg, 
         "WARNING, unable to open file %s for header parse\n", fname) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }

   /* Read out the file header portion of the file
   */
   fread(header, sizeof(char), ADEOS_HEADER_SIZE, fp) ;
   header[ADEOS_HEADER_SIZE] = '\0' ;
   fclose(fp) ;

   /* Search file id table for the matching file id
   */
   for (i=0; ADEOS_File_Id_Table[i].file_id_number != SENTINEL; i++)
      if (strncmp(header, ADEOS_File_Id_Table[i].file_identifier,
               ADEOS_ID_STRLEN) == 0)
      {
         ftype = ADEOS_File_Id_Table[i].file_id_number ;
         break ;
      }

   if (ftype == ERROR)
      syslog(LOG_ERR, "WARNING, invalid file header contents.\n") ;

   return(ftype) ;
 
} /* parse_ADEOS_hdr */

/* End of File */
