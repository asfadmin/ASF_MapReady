/*==============================================================================
Filename:	CSAhdr.c

Description:
	CSA file header parser related functions.  This module contains
the functions for handling CSA headers : parse, allocate header record
data structure, etc.  The specification of the CSA state vector file is
described in the CSA RADARSAT Mission Control System to US and Foreign
Data Reception Operation Centers Interface Control Document [MCS to
UFDROC].

External Functions:
	alloc_CSA_header_record
	extract_CSA_header
	parse_CSA_hdr
	
Static Functions:
	None

External Variables Defined:
	CSA_hdr_keyword_table
	
File Scope Static Variables:
	CSA_File_Type_Table
	
Notes:

==============================================================================*/

static char SccsFile[] = "CSAhdr.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "28 Mar 1996";
static char SccsLastChanger[] = "@(#)CSAhdr.c	1.3";
static char SccsState[] = "1.3";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <ctype.h>
#include "CSAhdr.h"


#ifdef __STDC__
CSA_Header_Record * alloc_CSA_header_record(void) ;
int                 extract_CSA_header(FILE *,CSA_Header_Record *) ;
int                 parse_CSA_hdr(char *) ;
int                 write_CSA_hdr_record(FILE *, CSA_Header_Record *) ;
#else
CSA_Header_Record * alloc_CSA_header_record() ;
int                 extract_CSA_header() ;
int                 parse_CSA_hdr() ;
int                 write_CSA_hdr_record() ;
#endif


extern void * util_do_malloc() ;
extern int    get_CSAstvec_precision() ;

/*
-- CSA Valid Header Keywords Table
--  Note: End of file line designator added here since
--  it is common to all CSA files also.
*/
CSA_Keywords CSA_hdr_keyword_table[MAX_KEYTABLE_SIZE] =
{
   { HEADER_START,       HEADER_START_STR    },
   { FILENAME,           FILENAME_STR        },
   { SPACECRAFT_ID,      SPACECRAFT_ID_STR   },
   { FILE_CREATION_TIME, FILE_CREAT_TIME_STR },
   { FILE_SOURCE,        FILE_SOURCE_STR     },
   { FILE_DEST,          FILE_DEST_STR       },
   { FILE_TYPE,          FILE_TYPE_STR       },
   { ENDFILE_LINE,       ENDFILE_LINE_STR    },
   { SENTINEL,           NULL                }
} ;

/*
-- Table of File Type Identifiers used
-- by CSA.  These identifiers are found
-- in the CSA file headers as values of
-- the keyword FILE_TYPE.
*/
static File_Identifier CSA_File_Type_Table[] =
{
   { CSA_PREDORBIT,     CSA_FTYPE_ORBDATA    }, /* Predicted Orbit */
   { CSA_DEFVORBIT,     CSA_FTYPE_ORBDATA    }, /* Definitive Orbit */
   { CSA_RECRQST,       CSA_FTYPE_RECRQST    }, /* Reception request */
   { CSA_RECSCHED,      CSA_FTYPE_RECSCHED   }, /* Reception schedule */
   { CSA_CALIBRQST,     CSA_FTYPE_CALRQST    }, /* Calibration request */
   { CSA_CALIBSCHED,    CSA_FTYPE_CALSCHED   }, /* Calibration schedule */
   { CSA_SARPROCPRM,    CSA_FTYPE_SARPROCPRM }, /* SAR Processing parameters */
   { CSA_RRQ_MCM,       CSA_FTYPE_RECRQST_M  }, /* Reception request */
   { CSA_RSH_MCM,       CSA_FTYPE_RECSCHED_M }, /* Reception schedule */
   { CSA_CRQ_MCM,       CSA_FTYPE_RAR        }, /* Calibration request */
   { CSA_CSH_MCM,       CSA_FTYPE_CALRPT     }, /* Calib. schedule*/
   { CSA_CALIBAVAILRPT, CSA_FTYPE_CAR        }, /* Calib. Avail. Report */ 
   { SENTINEL,          NULL                 }
} ;
			    



/*==============================================================================
Function:	CSA_Header_Record *alloc_CSA_header_record(void)

Description:	
	Allocate space for a CSA header record.  The header record
contains various fields storing information obtained from a CSA file
header.  Once the header record is allocated successfully, the field
values are initialized and a pointer to the record is returned.  If the
allocation did not succeed, NULL is returned.

Parameters:
	None

Returns:	pointer to newly allocated CSA header record or NULL 
Creator:	Norbert Piega	
Creation Date:	06/20/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
CSA_Header_Record *
alloc_CSA_header_record(void)
#else
CSA_Header_Record *
alloc_CSA_header_record()
#endif
{
   CSA_Header_Record *header = NULL ;

   header = (CSA_Header_Record *)util_do_malloc(sizeof(CSA_Header_Record)) ;
   if (header != NULL)
   {
      *header->filename = NULL ;
      *header->spacecraft_id = NULL ;
      *header->file_creation_time = NULL ;
      *header->file_source = NULL ;
      *header->file_dest = NULL ;
      *header->file_type = NULL ;
   }

   return(header) ;

} /* alloc_CSA_header_record */


 


/*==============================================================================
Function:	int extract_CSA_header(CSA_fp, CSA_header)

Description:	
	This function parses the input file represented by the file
pointer CSA_fp to obtain and store header information.  The table
CSA_Hdr_Keyword_Table is consulted during the parsing to identify valid
CSA identifiers and expected symbols (ex.  start of a comment line).
The header information obtained are stored in the passed state vector
record CSA_header.  If the header record was filled successfully,
ACCEPT is returned, otherwise, REJECT is returned.  In case of errors
during parsing, ERROR is returned.

Parameters:
	FILE *CSA_fp - pointer to CSA input file stream
	CSA_Header_Record *CSA_header - CSA header record where extracted
header information will be stored

Returns:	
	ACCEPT - all header information obtained successfully
	REJECT - default
	ERROR - memory allocation errors, unable to complete header record

Creator:	Norbert Piega	
Creation Date:	06/20/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
extract_CSA_header(FILE *CSA_fp, CSA_Header_Record *CSA_header)
#else
int
extract_CSA_header(CSA_fp, CSA_header)
   FILE *CSA_fp ;
   CSA_Header_Record *CSA_header ;
#endif
{
   int i,
       status = REJECT ;                            /* status returned */
   char inline[MAXLINE+1] ;                    /* input line from file */
   char *start,                                 /* start of input line */
	*ptr, *keyword, *value ;
   LOGICAL filled_header_record = FALSE ;     /* flag - header filled? */

   while (fgets(inline, MAXLINE, CSA_fp) != NULL) 
   {
      /* Skip blanks
      */
      start = inline ;
      while (isspace(*start))
         start++ ;

      /* BLANK line 
      */
      if (*start == '\0')
         continue ;

      /* COMMENT line 
      */
      else if (*start == ';')
      {
	 /* Check for header delimiters
	 */
         if (strncmp(start, CSA_hdr_keyword_table[HEADER_START].keyword,
            strlen(CSA_hdr_keyword_table[HEADER_START].keyword)) == 0) 
         {
            if (strncmp(start, CSA_hdr_keyword_table[ENDFILE_LINE].keyword,
               strlen(CSA_hdr_keyword_table[ENDFILE_LINE].keyword)) == 0) 
            {
               /* END_OF_FILE line, check if header complete */
               if (filled_header_record == FALSE)
               {
		  syslog(LOG_ERR, 
		    "WARNING, unexpected end of file keyword encountered\n") ;
                  status = ERROR ;
                  break ;
               }
            }

            else /* Parse header information */
            {
               ptr = strchr(start, '#') ;
               value = NULL ;

	       /* Try to match a header keyword
	       */
               for (i=0 ; i<HEADER_KEYWDS; i++)
               {
                  keyword = ptr+3 ;
                  if (strncmp(CSA_hdr_keyword_table[i].keyword, keyword,
                     strlen(CSA_hdr_keyword_table[i].keyword)) == 0)
                  {
                     ptr = strchr(start, ':') ;
                     if (ptr != NULL)
                        ptr++ ;
                     else
                        break ;

                     while (isspace(*ptr))
                        ptr++ ;
                     value = ptr ;
                     while (!isspace(*ptr))
                        ptr++ ;
                     *ptr = '\0' ;
                     break ;
                  }
               }

               /* Store value for a specified header keyword
	       */
               switch(i)
               {
                  case FILENAME:
                     strcpy(CSA_header->filename, value) ;
                     break ;

                  case SPACECRAFT_ID:
                     strcpy(CSA_header->spacecraft_id, value) ;
                     break ;

                  case FILE_CREATION_TIME:
                     strcpy(CSA_header->file_creation_time, value) ;
                     break ;

                  case FILE_SOURCE:
                     strcpy(CSA_header->file_source, value) ;
                     break ;

                  case FILE_DEST:
                     strcpy(CSA_header->file_dest, value) ;
                     break ;

                  case FILE_TYPE:
                     strcpy(CSA_header->file_type, value) ;
                     break ;

                  default:
                     break ;

               } /* endswitch */

               filled_header_record = 
		 (*CSA_header->filename && 
		  *CSA_header->spacecraft_id &&
                  *CSA_header->file_creation_time && 
		  *CSA_header->file_source &&
                  *CSA_header->file_dest && 
		  *CSA_header->file_type) ;
            }
         }
         else if (filled_header_record == TRUE)
          
            break ; /* Plain comment line. 
		    -- Break out if record filled else Ignore
                    -- Note: Breaking out of the loop is essential to prevent
                    --    reading past the header section.
		    */
      }
      else
         break ;   /* Not a header line, break out of the loop */

   } /* endwhile */

   if (filled_header_record == FALSE)
      status = ERROR ;
   else
      status = ACCEPT ;

   return(status) ;

} /* extract_CSA_header */




/*==============================================================================
Function:	int parse_CSA_hdr(filepath)

Description:
	Parse the header portion of the input CSA file specified by
filepath to determine its file type.  Start by allocating and
initializing a CSA header record; the header record is required by the
extract header function which is called next.  Among the data to be
stored in the header record structure is the file type of the file.
parse_CSA_header calls extract_CSA_header.  extract_CSA_header fills
the CSA_header structure as it parses the CSA file represented by the
file pointer CSA_fp.  The extract function returns success or failure.
If the extract succeeded, the file type is obtained from the header
record and returned otherwise an error status is returned.

Parameters:
	char *filepath - file path of input CSA file

Returns:	
	ERROR - parse error found
	TYPE_MATCHED - the file type id for the file type recognized from
the header parse

Creator:	Norbert Piega	
Creation Date:	06/20/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
parse_CSA_hdr(char *filepath)
#else
int
parse_CSA_hdr(filepath)
   char *filepath ;
#endif
{
   FILE *fp ;                                      /* input file pointer */
   CSA_Header_Record *header ;                      /* CSA header record */
   int i,
       type_matched = ERROR ;                          /* returned value */
   LOGICAL predicted_flag ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;               /* syslog msg string */
 
   /* Open the CSA file
   */
   if ((fp = fopen(filepath, "r")) == (FILE *)NULL) 
   {
      sprintf(logmsg, 
         "WARNING, unable to open file %s for header parse\n", filepath) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }
 
   /* Allocate new CSA header record 
   */
   if ((header = (CSA_Header_Record *)alloc_CSA_header_record()) 
        == (CSA_Header_Record *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, Unable to allocate CSA Header record.\n") ;
      return(ERROR) ;
   }
 
   /* Read file header information and store in header record 
   */
   if (extract_CSA_header(fp, header) != ACCEPT)
   {
      sprintf(logmsg,
         "WARNING, error extracting CSA file header info from file %s\n",
	  filepath) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }
 
   fclose(fp) ;

   /* Determine file type using the table CSA_File_Type_Table
   */
   i = 0 ;
   while (CSA_File_Type_Table[i].file_id_number != SENTINEL )
      if (strcmp(CSA_File_Type_Table[i].file_identifier, 
		 header->file_type) == 0)
      {
         type_matched = CSA_File_Type_Table[i].file_id_number ;
         if (strcmp(header->file_type, CSA_FTYPE_ORBDATA) == 0)
         {
            predicted_flag = get_CSAstvec_precision(header->filename) ;
	    if (predicted_flag == TRUE)
	       type_matched = CSA_PREDORBIT ;
            else
	       type_matched = CSA_DEFVORBIT ;
	 }
         break ; 
      }
      else
         i++ ;

   /* Check for FB files which are incompetantly identified as MCM */

/*   if (type_matched == 207 && strncmp(header->file_dest,"FB",2) == 0)
      type_matched = 202;
   if (type_matched == 202 && strncmp(header->file_dest,"MM",2) == 0)
      type_matched = 207;
   if (type_matched == 208 && strncmp(header->file_dest,"FB",2) == 0)
      type_matched = 203;
   if (type_matched == 203 && strncmp(header->file_dest,"MM",2) == 0)
      type_matched = 208;*/

   if (type_matched == 207 && strncmp(filepath + 8,"_f.", 3) == 0)
      type_matched = 202;
   if (type_matched == 202 && strncmp(filepath + 8,"_m.", 3) == 0)
      type_matched = 207;
   if (type_matched == 208 && strncmp(filepath + 8,"_f.", 3) == 0)
      type_matched = 203;
   if (type_matched == 203 && strncmp(filepath + 8,"_m.", 3) == 0)
      type_matched = 208;

   return(type_matched) ;

} /* parse_CSA_hdr */
 


/*==============================================================================
Function:	int write_CSA_hdr_record(FILE *outfp, 
		    CSA_Header_Record *csa_hdr)

Description:	
	This function writes the contents of a CSA header structure to
a file thus generating a CSA file header.

Parameters:
	FILE *outfp - output file pointer to write CSA header to
	CSA_Header_Record *csa_hdr - pointer to CSA header record
structure containing the data to be written to a file

Returns:	OK, ERROR
Creator:	Norbert Piega	
Creation Date:	01/09/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int
write_CSA_hdr_record(FILE *outfp, CSA_Header_Record *csa_hdr)
#else
int
write_CSA_hdr_record(outfp, csa_hdr)
   FILE *outfp ;
   CSA_Header_Record *csa_hdr ;
#endif
{
   if (csa_hdr == (CSA_Header_Record *)NULL)
   {
      syslog(LOG_DEBUG, 
	 "WARNING, NULL CSA header record input in CSA header write\n") ;
      return(ERROR) ;
   }

   if (outfp == (FILE *)NULL)
   {
      syslog(LOG_DEBUG, "WARNING, NULL file input writing CSA header\n");
      return(ERROR) ;
   }

   fprintf(outfp, "%s", HEADER_START_STR) ;
   fprintf(outfp, "%s: ", FILENAME_STR) ;
   fprintf(outfp, "%s\n", csa_hdr->filename) ;

   fprintf(outfp, "%s", HEADER_START_STR) ;
   fprintf(outfp, "%s: ", SPACECRAFT_ID_STR) ;
   fprintf(outfp, "%s\n", csa_hdr->spacecraft_id) ;

   fprintf(outfp, "%s", HEADER_START_STR) ;
   fprintf(outfp, "%s: ", FILE_CREAT_TIME_STR) ;
   fprintf(outfp, "%s\n", csa_hdr->file_creation_time) ;

   fprintf(outfp, "%s", HEADER_START_STR) ;
   fprintf(outfp, "%s: ", FILE_SOURCE_STR) ;
   fprintf(outfp, "%s\n", csa_hdr->file_source) ;

   fprintf(outfp, "%s", HEADER_START_STR) ;
   fprintf(outfp, "%s: ", FILE_DEST_STR) ;
   fprintf(outfp, "%s\n", csa_hdr->file_dest) ;

   fprintf(outfp, "%s", HEADER_START_STR) ;
   fprintf(outfp, "%s: ", FILE_TYPE_STR) ;
   fprintf(outfp, "%s\n", csa_hdr->file_type) ;

   return(OK) ;

} /* write_CSA_hdr_record */

/* End of file */
