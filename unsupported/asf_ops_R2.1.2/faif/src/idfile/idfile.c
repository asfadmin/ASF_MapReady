/*==============================================================================
Filename:	idfile.c

Description:
	This module contains the FAIF file identifier function.
The file identifier function called identify_FA_file returns the file
type code for the file specified through the input parameter.  Note that
the file type codes are found in the following header files: ESA.h,
NASDA.h, CSA.h, WALPS.h, ADEOS.h.

External Functions:
	check_FA_filename
	identify_FA_file                    // no longer used

Static Functions:
	None
	
External Variables Defined:
	None

File Scope Static Variables:
	Hdr_Parse_Func_Table
	
Notes:
1.  Nov. '96 - RH
    Uncommented some error logmsg's.
2.  April '97 - RH
    Omit call to identify_FA_file; use check_FA_filename only
==============================================================================*/

static char SccsFile[] = "idfile.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "26 Mar 1996";
static char SccsLastChanger[] = "@(#)idfile.c	1.2";
static char SccsState[] = "1.2";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <time.h>
#include <syslog.h>
#include "faifdefs.h"
#include "filerec.h"
#include "dapps_list.h"
#include "nmalloc.h"

#ifdef __STDC__
int identify_FA_file(char *, int) ;
int check_FA_filename(char *, int) ;
int assign_filerec_ftype(File_Record *, int) ;
#else
int identify_FA_file() ;
int check_FA_filename() ;
int assign_filerec_ftype() ;
#endif

extern int parse_ESA_hdr() ;
extern int parse_ESA_filename() ;

extern int parse_NASDA_hdr() ;
extern int parse_NASDA_filename() ;

extern int parse_CSA_hdr() ;
extern int parse_CSA_filename() ;

extern int parse_WALPS_hdr() ;
extern int parse_WALPS_filename() ;

extern int parse_ADEOS_hdr() ;
extern int parse_ADEOS_filename() ;

extern void *util_do_malloc() ;

/*
-- Table of Header Parse Functions
--
--   An entry for each FA header parser
-- function is stored in this table.
*/
static Function_Table_Entry Hdr_Parse_Func_Table[] =
{
   { ESA,      "parse_ESA_hdr",     parse_ESA_hdr   },
   { NASDA,    "parse_NASDA_hdr",   parse_NASDA_hdr },
   { CSA,      "parse_CSA_hdr",     parse_CSA_hdr   },
   { WALPS,    "parse_WALPS_hdr",   parse_WALPS_hdr },
   { ADEOS,    "parse_ADEOS_hdr",   parse_ADEOS_hdr },
   { SENTINEL,  NULL,               NULL            }
} ;



/*==============================================================================
Function:	int check_FA_filename(filepath, fa_code)

Description:
	Determine the file type of the file with the specified file
path filepath.  Identify the file by examining its filename.  The file
type obtained is returned.  In case the file type cannot be
determined an error status is returned and the ASF error handling routine
is called to log an ERROR message.  Note that the message generated may 
be based on error status returned by the called functions.

Parameters:
   filepath - file path of input FA file to be identified

Returns:	a valid flight agency file type or ERROR
Creator:	Norbert Piega	
Creation Date:	06/13/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
check_FA_filename(char *filepath, int fa_code)
#else
int
check_FA_filename(filepath, fa_code)
   char *filepath ;
   int fa_code ;
#endif
{
   int fname_ftype ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   /* Parse filename to find out if it conforms to the
   -- ESA, NASDA, CSA, or WALLOPS file naming convention.
   */
   if (fa_code == ESA)
      fname_ftype = parse_ESA_filename(filepath) ;
  
   else if (fa_code == NASDA)
      fname_ftype = parse_NASDA_filename(filepath) ;

   else if (fa_code == CSA)
      fname_ftype = parse_CSA_filename(filepath) ;

   else if (fa_code == WALPS)
      fname_ftype = parse_WALPS_filename(filepath) ;

   else if (fa_code == ADEOS)
      fname_ftype = parse_ADEOS_filename(filepath) ;

   else
   {
      sprintf(logmsg, 
	 "ERROR, Invalid FA id in check_FA_filename for file %s\n",
	  filepath) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }

   if (fname_ftype == ERROR)
   {
      sprintf(logmsg, 
	 "ERROR, Unable to determine file type from filename %s\n",
	  filepath) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }

   return(fname_ftype) ;

} /* check_FA_filename */




/*==============================================================================
Function:	int identify_FA_file(filepath, fa_code)

  **  No longer used ** 

Description:
	Determine the file type of the file with the specified file
path filepath.  Identify the file by its filename through the call to
check_FA_filename.  Based on the return status of the check_FA_filename,
the Hdr_Parse_Func_Table is consulted to determine which of the
parse_XXX_header functions to call and then the call is made.  The 
status returned by the parse_XXX_header function called gives positive
identification of the file therefore the file type obtained is returned.
In case the file type cannot be determined an error status is returned 
and syslog is called to log a WARNING message.  Note that the message 
generated may be based on error status returned by the called functions.

Parameters:
   char *filepath - file path of input FA file to be identified
   int fa_code - FA id checked as the FA type of a file

Returns:	a valid flight agency file type or ERROR
Creator:	Norbert Piega	
Creation Date:	06/13/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
identify_FA_file(char *filepath, int fa_code)
#else
int
identify_FA_file(filepath, fa_code)
   char *filepath ;
   int fa_code ;
#endif
{
   int fname_ftype ;
   int fhdr_ftype ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
   int index ;

   if ((fname_ftype = check_FA_filename(filepath, fa_code)) != ERROR)
   {
      /* Now, try to parse the header to verify file type.
      -- The index to the table is the fa_code.
      */
      for (index=0; Hdr_Parse_Func_Table[index].func_id != SENTINEL; index++)
	 if (Hdr_Parse_Func_Table[index].func_id == fa_code)
	    break ;
      if (Hdr_Parse_Func_Table[index].func_id == SENTINEL)
      {
         syslog(LOG_DEBUG, 
            "WARNING, Unrecognized FA code in identify_FA_file function\n") ;
         return(ERROR) ;
      }
      fhdr_ftype = (*Hdr_Parse_Func_Table[index].func_entry)(filepath) ;
      if (fhdr_ftype != ERROR)
      {
         /* Check if type specified in header
         -- matches type obtained from filename
         */
         if (fhdr_ftype != fname_ftype)
         {
	    sprintf(logmsg, 
	       "WARNING, header and file name file type mismatch for file %s\n",
	       filepath) ; 
/*            syslog(LOG_WARNING, logmsg) ;*/
         }
         return(fhdr_ftype) ;
      }
      else
      {
         sprintf(logmsg, 
	    "WARNING, unable to determine file type of %s from header info \
            \nUsing file type specified in file name\n", filepath) ; 
         syslog(LOG_WARNING, logmsg) ;
	 return(fname_ftype) ;
      }

   } /* Endif */

   return(ERROR) ;

} /* identify_FA_file */




/*==============================================================================
Function:	int assign_filerec_ftype(File_Record *filerecp)

Description:	
	This function assigns the file type and flight agency fields of
a file record.  The value assigned is based on what is obtained via the
check_FA_filename function.

Parameters:
	filerecp - pointer to the file record to be modified.

Returns:
	OK - assignment succeeded
	ERROR - error identifying file

Creator:	Norbert Piega	
Creation Date:	08/08/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
assign_filerec_ftype(File_Record *filerecp, int fa_code)
#else
int
assign_filerec_ftype(filerecp, fa_code)
   File_Record *filerecp ;
   int fa_code ;
#endif
{
   int status ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   /* Identify file type.  Assign type to file_type_status field */
   status = check_FA_filename(filerecp->file_name, fa_code);
   if (status == ERROR) return (ERROR);

   /* Assign file type */
   filerecp->file_type_status = status ; 

   /* Assign flight agency */
   switch(fa_code)
   {
      case ESA:
	    filerecp->flight_agency = 
	       (char *)util_do_malloc(sizeof(char)*(strlen(ESA_STR)+1)) ;
            strcpy(filerecp->flight_agency, ESA_STR) ;
	    break ;
      case CSA:
	    filerecp->flight_agency = 
	       (char *)util_do_malloc(sizeof(char)*(strlen(CSA_STR)+1)) ;
            strcpy(filerecp->flight_agency, CSA_STR) ;
	    break ;
      case NASDA:
	    filerecp->flight_agency = 
	       (char *)util_do_malloc(sizeof(char)*(strlen(NASDA_STR)+1)) ;
            strcpy(filerecp->flight_agency, NASDA_STR) ;
	    break ;
      case WALPS:
	    filerecp->flight_agency = 
	       (char *)util_do_malloc(sizeof(char)*(strlen(WALPS_STR)+1)) ;
            strcpy(filerecp->flight_agency, WALPS_STR) ;
	    break ;
      case ADEOS:
	    filerecp->flight_agency = 
	       (char *)util_do_malloc(sizeof(char)*(strlen(ADEOS_STR)+1)) ;
            strcpy(filerecp->flight_agency, ADEOS_STR) ;
	    break ;
      default:
	    filerecp->flight_agency = 
	       (char *)util_do_malloc(sizeof(char)*(strlen("UNKNOWN")+1)) ;
            strcpy(filerecp->flight_agency,"UNKNOWN") ;
   }

   return(OK) ;

} /* assign_filerec_ftype */

/* End of file */
