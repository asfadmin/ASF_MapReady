/*==============================================================================
Filename:	WALPShdr.c

Description:
	WALLOPS file header parser function.  This module contains the 
function for parsing WALLOPS headers.  The specification of the WALLOPS 
file format is described in the Alaska SAR Facility (ASF) to Wallops 
Flight Facility (WFF) Interface Specification Document (ISD), JPL-D11871.

External Functions:
	alloc_WALPS_header_record
	parse_WALPS_hdr
	extract_WALPS_header
	extract_param_value
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	WALPS_Keyword_Table
	WALPS_MsgType_Table
	
Notes:
1.  Oct. '96 - RH
    Update for ISD Version 2.3:
      Change keyword tables
      Removed WALPS_fp from parameter list in extract_WALPS_header()
      Amended extract_param_value() to fix return value in some cases
==============================================================================*/

static char SccsFile[] = "WALPShdr.c" ;
static char SccsRevision[] = "1" ;
static char SccsDate[] = "22 Jan 1996";
static char SccsLastChanger[] = "@(#)WALPShdr.c	1.1";
static char SccsState[] = "1.1";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include "odldef.h"
#include "odlinter.h"
#include "WALPShdr.h"

#ifdef __STDC__
WALPS_Header_Record *alloc_WALPS_header_record(void) ;
int                  parse_WALPS_hdr(char *) ;
int                  extract_WALPS_header(WALPS_Header_Record *,
					  AGGREGATE top) ;
int                  extract_param_value(AGGREGATE, void *, char *) ;
#else
WALPS_Header_Record *alloc_WALPS_header_record() ;
int                  parse_WALPS_hdr() ;
int                  extract_WALPS_header() ;
int                  extract_param_value() ;
#endif

extern void *       util_do_malloc() ;


/* WALLOPS Valid Keywords Table
*/
WALPS_Keywords WALPS_Keyword_Table[] =
{
   { COMMON_HDR,  COMMON_HDR_KEYWD  },
   { TIME,        TIME_KEYWD        },
   { MSG_TYPE,    MSG_TYPE_KEYWD    },
   { DESTINATION, DESTINATION_KEYWD }, 
   { SOURCE,      SOURCE_KEYWD      },
   { NUM_RECS,    NUM_RECS_KEYWD    },
   { REQ,         REQ_KEYWD         },
   { RES,         RES_KEYWD         },
   { WOS,         WOS_KEYWD         },
   { EPH,         EPH_KEYWD         },
   { DNL,         DNL_KEYWD         },
   { MSH,         MSH_KEYWD         },
   { TPR,         TPR_KEYWD         },
   { SENTINEL,    NULL              }
} ;


/* WALLOPS Valid Message Type Table
*/
WALPS_Keywords WALPS_MsgType_Table[] =
{
   { WALPS_REQ,         REQ_KEYWD  },
   { WALPS_RES,         RES_KEYWD  },
   { WALPS_WOS,         WOS_KEYWD  },
   { WALPS_EPH,         EPH_KEYWD  },
   { WALPS_DNL,         DNL_KEYWD  },
   { WALPS_MSH,         MSH_KEYWD  },
   { WALPS_TPR,         TPR_KEYWD  },
   { SENTINEL,          NULL       }
} ;




/*==============================================================================
Function:	WALPS_Header_Record *alloc_WALPS_header_record(void)

Description:	
	Allocate space for a WALPS header record.  The header record
contains various fields storing information obtained from a WALPS file
header.  Once the header record is allocated successfully, the field
values are initialized and a pointer to the record is returned.  If the
allocation did not succeed, NULL is returned.

Parameters: 	None
Returns:	pointer to newly allocated WALPS header record or NULL 
Creator:	Norbert Piega	
Creation Date:	10/19/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
WALPS_Header_Record *
alloc_WALPS_header_record(void)
#else
WALPS_Header_Record *
alloc_WALPS_header_record()
#endif
{
   WALPS_Header_Record *header = NULL ;

   header = (WALPS_Header_Record *)util_do_malloc(sizeof(WALPS_Header_Record)) ;
   if (header != NULL)
   {
      header->num_records = 0 ;
      *header->file_name = NULL ;
      *header->spacecraft_id = NULL ;
      *header->file_creation_time = NULL ;
      *header->file_source = NULL ;
      *header->file_dest = NULL ;
      *header->file_type = NULL ;
      header->file_type_id = -1 ;
   }

   return(header) ;

} /* alloc_WALPS_header_record */
    




/*==============================================================================
Function:	int parse_WALPS_hdr(char *filepath)

Description:
	Parse the header portion of the input WALLOPS file specified by
filepath to determine its file type.  Return the obtained file type if
the parse is successful otherwise, return an ERROR.

Parameters:
	filepath - file path of input WALLOPS file to parse

Returns:	WALLOPS file type or ERROR
Creator:	Norbert Piega	
Creation Date:	06/20/1994
Notes:		
==============================================================================*/
#ifdef __STDC__
int
parse_WALPS_hdr(char *filepath)
#else
int
parse_WALPS_hdr(filepath)
   char *filepath ;
#endif
{
   int ftype = ERROR ;
   FILE *tfp ;
   AGGREGATE top ;
   AGGREGATE hdrobj ;
   PARAMETER parameter = NULL ; 
   VALUE param_value = NULL ;
   int index ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   /* Open Wallops file
   */
   if ((tfp = fopen(filepath, "r")) == (FILE *)NULL)
   {
      sprintf(logmsg, "ERROR, unable to open file %s\n", filepath) ;
      syslog(LOG_ERR, logmsg) ;
      return(ERROR) ;
   }

   /* Allocate root aggregate node for Wallops label file
   */
   top = NewAggregate(NULL, KA_GROUP, "root", NULL);

   /* Read Wallops label. Obtain associated tree
   */
   if (ReadLabel(tfp, top) == 0)
   {
      syslog(LOG_DEBUG, "ERROR, ODL routine ReadLabel returned error\n") ;
      return(ERROR) ;
   }
   else
      if (top == NULL)
      {
	 syslog(LOG_DEBUG, "ERROR, ODL top node is NULL\n") ;
	 return(ERROR) ;
      }
      else
      {
	 /* Search tree for a specific type of object
	 */
         if (FindObject(top, WALPS_Keyword_Table[RES].keyword, 
		   NULL) != NULL)
	    ftype = WALPS_RES ;

         else if (FindObject(top, WALPS_Keyword_Table[WOS].keyword, 
	           NULL) != NULL)
	    ftype = WALPS_WOS ;

         else if (FindObject(top, WALPS_Keyword_Table[EPH].keyword, 
		   NULL) != NULL)
	    ftype = WALPS_EPH ;

         else if (FindObject(top, WALPS_Keyword_Table[DNL].keyword, 
	           NULL) != NULL)
	    ftype = WALPS_DNL ;

         else if (FindObject(top, WALPS_Keyword_Table[MSH].keyword, 
	           NULL) != NULL)
	    ftype = WALPS_MSH ;

         else if (FindObject(top, WALPS_Keyword_Table[TPR].keyword, 
	           NULL) != NULL)
	    ftype = WALPS_TPR ;

         else
	 {
	    sprintf(logmsg, 
	       "ERROR, File %s is an unsupported file type\n", filepath) ;
            syslog(LOG_ERR, logmsg) ;
	    return(ERROR) ;
	 }

	 /* Find COMMON HEADER Object
	 -- Note: ODL converts names to uppercase so it only
	 -- recognizes searches of names in uppercase
	 */
         if ((hdrobj = FindObject(top, 
				  WALPS_Keyword_Table[COMMON_HDR].keyword, 
	           NULL)) == NULL)
         {
	    sprintf(logmsg, 
	       "ERROR, unable to find common header for %s\n", filepath) ;
	    syslog(LOG_ERR, logmsg) ;
	    return(ERROR) ;
         }
         else
	 {
	    if ((parameter = FindParameter(hdrobj,
			     WALPS_Keyword_Table[MSG_TYPE].keyword))
                 != NULL)
            {
	       if ((param_value = FirstValue(parameter)) != NULL)
	       {
		  if (param_value->item.type == TV_SYMBOL)
		  {
		     for (index=0; WALPS_MsgType_Table[index].id != SENTINEL;
			  index++)
		        if (strcmp(param_value->item.value.string,
				WALPS_MsgType_Table[index].keyword) == 0)
                        {
			   if (ftype != WALPS_MsgType_Table[index].id)
			   {
			      syslog(LOG_ERR,
		                 "ERROR, File object name and message type value mismatch\n") ;
			      return(ERROR) ;
			      break ;
			   }
		        }
		  }
		  else
		  {
		     sprintf(logmsg,
		        "ERROR, Value type of %s must be string\n",
		         WALPS_Keyword_Table[MSG_TYPE].keyword) ;
                     syslog(LOG_ERR, logmsg) ;
		     return(ERROR) ;
		  }
	       }
	       else
	       {
		  sprintf(logmsg,
		     "ERROR, Invalid value for %s keyword\n",
		      WALPS_Keyword_Table[MSG_TYPE].keyword) ;
                  syslog(LOG_ERR, logmsg) ;
		  return(ERROR) ;
	       }
	    }
	    else
	    {
               sprintf(logmsg, 
		  "ERROR, Missing expected statement with keyword %s\n",
		   WALPS_Keyword_Table[MSG_TYPE].keyword) ;
               syslog(LOG_ERR, logmsg) ;
               return(ERROR) ;
	    }
         }

      } /* endElse top not NULL */

   fclose(tfp) ;

   return(ftype) ;

} /* parse_WALPS_hdr */





/*==============================================================================
Function:	int extract_WALPS_header(WALPS_header, top)

Description:	
	This function obtains and stores header information.  The table
WALPS_Hdr_Keyword_Table is consulted during the parsing to identify valid
WALPS identifiers and expected symbols (ex.  start of a comment line).
The header information obtained are stored in the passed state vector
record WALPS_header.  If the header record was filled successfully,
ACCEPT is returned, otherwise, REJECT is returned.  In case of errors
during parsing, ERROR is returned.

Parameters:
	WALPS_Header_Record *WALPS_header - WALPS header record where 
	   extracted header information will be stored
        AGGREGATE top - top aggregate from input ODL file

Returns:	
	ACCEPT - all header information obtained successfully
	REJECT - default
	ERROR - memory allocation errors, unable to complete header record

Creator:	Norbert Piega	
Creation Date:	10/19/1994
Notes:		
	It is assumed that the Wallops header field file_name 
WALPS_header->file_name is defined.
==============================================================================*/
#ifdef __STDC__
int
extract_WALPS_header(WALPS_Header_Record *WALPS_header, AGGREGATE top)
#else
int
extract_WALPS_header(WALPS_header, top)
   WALPS_Header_Record *WALPS_header ;
   AGGREGATE top ;
#endif
{
   AGGREGATE hdrobj ;
   PARAMETER parameter = NULL ; 
   VALUE param_value = NULL ;
   int i, status = REJECT ;                   /* status returned */
   int index ;
   LOGICAL filled_header_record = FALSE ;     /* flag - header filled? */
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   if (top == NULL)
   {
      syslog(LOG_DEBUG, "ERROR, ODL top node is NULL\n") ;
      status = ERROR ;
   }
   else
   {
      /* Search tree for a specific type of object
      -- Note: This must be the same as the value of
      -- MSG_TYPE checked later.
      */
      for (index=REQ; WALPS_Keyword_Table[index].id != SENTINEL;
	   index++)
         if (FindObject(top, WALPS_Keyword_Table[index].keyword, NULL) 
	     != NULL)
         {
	    if (WALPS_Keyword_Table[index].id == REQ)
               WALPS_header->file_type_id = WALPS_REQ ;

            else if (WALPS_Keyword_Table[index].id == RES)
               WALPS_header->file_type_id = WALPS_RES ;

            else if (WALPS_Keyword_Table[index].id == WOS)
               WALPS_header->file_type_id = WALPS_WOS ;

            else if (WALPS_Keyword_Table[index].id == EPH)
               WALPS_header->file_type_id = WALPS_EPH ;

            else if (WALPS_Keyword_Table[index].id == DNL)
               WALPS_header->file_type_id = WALPS_DNL ;

            else if (WALPS_Keyword_Table[index].id == MSH)
               WALPS_header->file_type_id = WALPS_MSH ;

            else if (WALPS_Keyword_Table[index].id == TPR)
               WALPS_header->file_type_id = WALPS_TPR ;


            strcpy(WALPS_header->file_type, 
		   WALPS_Keyword_Table[index].keyword) ;
            break ;
         }
      if (WALPS_Keyword_Table[index].id == SENTINEL)
      {
         sprintf(logmsg, "ERROR, File %s is an unsupported file type\n", 
		          WALPS_header->file_name) ;
         syslog(LOG_ERR, logmsg) ;
	 status = ERROR ;
      }

      /* Find COMMON HEADER Object
      -- Note: ODL converts names to uppercase so it only
      -- recognizes searches of names in uppercase
      */
      if ((hdrobj = FindObject(top, 
			       WALPS_Keyword_Table[COMMON_HDR].keyword, NULL)) 
                     == NULL)
      {
         sprintf(logmsg, "ERROR, unable to find common header for %s\n", 
		          WALPS_header->file_name) ;
	 syslog(LOG_ERR, logmsg) ;
	 status = ERROR ;
      }
      else
      {
         /* Look for the keywords expected inside the
	 -- COMMON_HEADER aggregation. ie. TIME, MSG_TYPE,
	 -- DESTINATION, SOURCE, NUMBER_OF_RECORDS
	 */
         status = extract_param_value(hdrobj, &(WALPS_header->num_records),
                 WALPS_Keyword_Table[NUM_RECS].keyword) ;

         status = extract_param_value(hdrobj, WALPS_header->file_source,
                 WALPS_Keyword_Table[SOURCE].keyword) ;

         if (strcmp(WALPS_header->file_source, "WALLOPS") == 0)
            strcpy(WALPS_header->file_source, WALPS_SRCDEST_WALPS) ;

         

         status = extract_param_value(hdrobj, WALPS_header->file_dest,
                 WALPS_Keyword_Table[DESTINATION].keyword) ;

         status = extract_param_value(hdrobj, WALPS_header->file_type,
                 WALPS_Keyword_Table[MSG_TYPE].keyword) ;

         status = extract_param_value(hdrobj, &(WALPS_header->date_time),
                 WALPS_Keyword_Table[TIME].keyword) ;

	 sprintf(WALPS_header->file_creation_time,"%04d-%03dT%02d:%02d:%02d.%03d",
            WALPS_header->date_time.year,
            WALPS_header->date_time.doy,
            WALPS_header->date_time.hours,
            WALPS_header->date_time.minutes,
            WALPS_header->date_time.seconds,
            WALPS_header->date_time.nanoseconds/1000000);

	 filled_header_record = TRUE ;
      }

   }  /* Else top not NULL */

   if (filled_header_record == FALSE)
      status = ERROR ;
   else
      status = ACCEPT ;

   return(status) ;

} /* extract_WALPS_header */





/*==============================================================================
Function:	int extract_param_value(AGGREGATE object, void *record_field, 
					char *keyword)

Description:	
	This function searches for the keyword inside the ODL aggregation
represented by object.  If the keyword is found, the value of the keyword
is obtained and assigned to record_field.  Note that record field has t
be cast to the proper type for this assignment to work.

Parameters:
	AGGREGATE object - the ODL aggregate inside which the keyword is
searched
	void *record_field - the variable to assign the keyword value
obtained
	char *keyword - the keyword string to search for in the specified
object/aggregate

Returns:	OK, ERROR
Creator:	Norbert Piega	
Creation Date:	10/21/1994
Notes:		
	Note that record field is treated as a pointer to an integer if
the value to assign is an integer, a pointer to a char if the value to
assign is a string, and a pointer to a struct ODLDate if the value to
assign is a struct ODLDate.  These are the only types expected.
==============================================================================*/
#ifdef __STDC__
int
extract_param_value(AGGREGATE object, void *record_field, char *keyword)
#else
int
extract_param_value(object, record_field, keyword)
   AGGREGATE object ;
   void *record_field ;
   char *keyword ;
#endif
{
   PARAMETER parameter ;
   VALUE param_value ;
   int status = OK ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;

   /* Look for the keywords expected inside the
   -- object aggregation. ex. For object DATATAKE_RECORD,
   -- the parameters/keywords expected are SATELLITE, SENSOR,
   -- MODE, REVOLUTION, DATATAKES, etc.
   */
   if ((parameter = FindParameter(object, keyword)) != NULL)
   {
      if ((param_value = FirstValue(parameter)) != NULL)
      {
	 switch(param_value->item.type)
	 {
	    case TV_INTEGER:
               *((int *)record_field) = param_value->item.value.integer.number ;
	       break ;

            case TV_SYMBOL:
	       strcpy((char *)record_field, param_value->item.value.string) ;
	       break ;

            case TV_DATE_TIME:
	       *((struct ODLDate *)record_field) = 
		  param_value->item.value.date_time ;
	       break ;
    
	    default:
               sprintf(logmsg, "ERROR, Invalid value for %s keyword\n",
                       keyword) ;
	       syslog(LOG_ERR, logmsg) ;
	       status = ERROR ;
	 }
      }
      else
      {
         sprintf(logmsg, "ERROR, No value for keyword %s\n", keyword) ;
	 syslog(LOG_ERR, logmsg) ;
	 status = ERROR ;
      }
   }
   else
   {
      sprintf(logmsg, "ERROR, Unable to find keyword %s\n", keyword) ;
      syslog(LOG_ERR, logmsg) ;
      status = ERROR ;
   }

   return(status) ;

} /* extract_param_value */

/* End of file */
