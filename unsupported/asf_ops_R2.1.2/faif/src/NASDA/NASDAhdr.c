/*==============================================================================
Filename:	NASDAhdr.c

Description:
	NASDA file header parser function.  This module contains the
function for parsing NASDA headers.  The specification of the NASDA
file format is described in the JERS-1 Operation Interface
Specification document.

External Functions:
	parse_NASDA_hdr
	
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
#include "NASDA.h"

#ifdef __STDC__
int parse_NASDA_hdr(char *, int) ;
#else
int parse_NASDA_hdr() ;
#endif


extern File_Identifier NASDA_File_Id_Table[] ;
 



/*==============================================================================
Function:	int parse_NASDA_hdr(filepath)

Description:
	Parse the header portion of the input NASDA file specified by
filepath to determine its file type.  Return the obtained file type if
the parse is successful otherwise, return ERROR.  The table
NASDA_File_Id_Table is used to determine what file id code is returned
as status.

Parameters:
	char *filepath - file path of input NASDA file to parse

Returns:	NASDA file type or NULL	
Creator:	Norbert Piega	
Creation Date:	06/20/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
parse_NASDA_hdr(char *fname, int ftype)
#else
int
parse_NASDA_hdr(fname, ftype)
   char *fname ;
   int ftype ;
#endif
{
   FILE *fp ;
   char header[NASDA_HEADER_SIZE+1] ;
   int i ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   /* Open the NASDA file
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
   fread(header, sizeof(char), NASDA_HEADER_SIZE, fp) ;
   header[NASDA_HEADER_SIZE] = '\0' ;
   fclose(fp) ;

   /* Search file id table for the matching file id
   */
   for (i=0; NASDA_File_Id_Table[i].file_id_number != SENTINEL; i++)
      if (strncmp(header, NASDA_File_Id_Table[i].file_identifier,
               NASDA_ID_STRLEN) == 0)
      {
         ftype = NASDA_File_Id_Table[i].file_id_number ;
         break ;
      }

   if (ftype == ERROR)
      syslog(LOG_ERR, "WARNING, invalid file header contents.\n") ;

   return(ftype) ;
 
} /* parse_NASDA_hdr */

/* End of File */
