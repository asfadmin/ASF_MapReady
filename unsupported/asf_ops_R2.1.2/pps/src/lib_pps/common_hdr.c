/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	ODLcommon_hdr.c

Description:
	ODL common header parser function.  This module contains the 
function for parsing IMS/CP message headers.  The specification of the ODL
objects exchanged between PPS and the IMS is described in Chapter 9 of the
ASF SIS Document. The ODL objects exchanged between the PPS and the CP are
described in Chapter 2.

External Functions:
	alloc_common_header_record
	parse_common_hdr
	extract_common_header
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	IMS_Keyword_Table
	IMS_MsgType_Table
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)common_hdr.c	1.2    12/16/96";

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "defs.h"
#include "odldef.h"
#include "odlinter.h"
#include "PPSdefs.h"
#include "PPSextern.h"
#include "PPS_Extern.h"
#include "PPSerr.h"


/* IMS Valid Keywords Table */

IMS_Keywords IMS_Keyword_Table[] =
{
   { COMMON_HDR,  COMMON_HDR_KEYWD  },
   { TIME,        TIME_KEYWD        },
   { MSG_TYPE,    MSG_TYPE_KEYWD    },
   { DESTINATION, DESTINATION_KEYWD }, 
   { SOURCE,      SOURCE_KEYWD      },
   { NUM_RECS,    NUM_RECS_KEYWD    },
   { L1PR,        L1PR_ORDER_KEYWD  },
   { SCAN,        SCAN_ORDER_KEYWD  },
   { STVEC,	  STVEC_KEYWD       },
   { CANCEL_ORDER,CANCEL_ORDER_KEYWD},
   { JOB_REQUEST, JOB_REQUEST_KEYWD },
   { STATUS_MESSAGE, STATUS_MESSAGE_KEYWD },
   { SENTINEL,    NULL              }
} ;


/* IMS Valid Message Type Table */

IMS_Keywords IMS_MsgType_Table[] =
{
   { IMS_L1PR,		L1PR_ORDER_KEYWD	},
   { IMS_SCAN,		SCAN_ORDER_KEYWD	},
   { IMS_SV_AVAIL,	STVEC_KEYWD		},
   { IMS_CANCEL,	CANCEL_ORDER_KEYWD	},
   { CP_JOB_REQUEST,	JOB_REQUEST_KEYWD	},
   { CP_STATUS_MESSAGE,	STATUS_MESSAGE_KEYWD	},
   { SENTINEL,		NULL              	}
} ;




/*==============================================================================
Function:	Common_Header_Record *alloc_common_header_record(void)

Description:	
	Allocate space for a common header record.  

Parameters: 	None
Returns:	pointer to newly allocated & initialized record or NULL
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
Common_Header_Record *
alloc_common_header_record(void)
#else
Common_Header_Record *
alloc_common_header_record()
#endif
{
   Common_Header_Record *header = NULL ;

   header = (Common_Header_Record *)util_do_malloc(sizeof(Common_Header_Record)) ;
   if (header != NULL)
   {
      header->num_records = 0 ;
      *header->file_name = NULL ;
      *header->file_creation_time = NULL ;
      *header->file_type = NULL ;
      header->file_type_id = -1 ;
   }
   return(header) ;
} 

    

/*==============================================================================
Function:	int parse_common_hdr(char *filepath, char *buffer)

Description:
	Parse the header portion of the input ODL object specified by
filepath to determine its file type.  Return the obtained file type if
the parse is successful otherwise, return an ERROR.

Parameters:
	filepath - file path of input IMS file to parse

Returns:	IMS file type or ERROR
Creator:	Nadia Adhami	
Creation Date:	05/20/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int
parse_common_hdr(char *filepath, char *buffer)
#else
int
parse_common_hdr(filepath, buffer)
   char *filepath ;
   char *buffer ;
#endif
{
   int ftype = ERROR ;
   AGGREGATE top ;
   AGGREGATE hdrobj ;
   PARAMETER parameter = NULL ; 
   VALUE param_value = NULL ;
   int index ;
   char logmsg[MAX_SYSLOG_MSGLEN] ;

   /* Allocate root aggregate node */
   top = NewAggregate(NULL, KA_GROUP, "root", NULL);

   /* Read label. Obtain associated tree */
   if (ReadLabel_buf(buffer, top) == 0)
   {
      pps_logMsg(ProgName, PPS_WARNING, "ODL routine ReadLabel returned error");
      (void) RemoveAggregate(top);
      return(ERROR) ;
   }
   else
      if (top == NULL)
      {
         pps_logMsg(ProgName, PPS_WARNING, "ODL top node is NULL");
         (void) RemoveAggregate(top);
	 return(ERROR) ;
      }
      else
      {
	 /* Search tree for a specific type of object */

         if (FindObject(top, IMS_Keyword_Table[L1PR].keyword, 
		   NULL) != NULL)
	    ftype = IMS_L1PR ;

         else if (FindObject(top, IMS_Keyword_Table[SCAN].keyword, 
	           NULL) != NULL)
	    ftype = IMS_SCAN ;

         else if (FindObject(top, IMS_Keyword_Table[STVEC].keyword, 
	           NULL) != NULL)
	    ftype = IMS_SV_AVAIL ;

         else if (FindObject(top, IMS_Keyword_Table[CANCEL_ORDER].keyword, 
	           NULL) != NULL)
	    ftype = IMS_CANCEL ;

         else if (FindObject(top, IMS_Keyword_Table[JOB_REQUEST].keyword, 
	           NULL) != NULL)
	    ftype = CP_JOB_REQUEST ;

         else if (FindObject(top, IMS_Keyword_Table[STATUS_MESSAGE].keyword, 
	           NULL) != NULL)
	    ftype = CP_STATUS_MESSAGE ;

         else
	 {
	    sprintf(logmsg, "File %s is an unsupported file type", filepath) ;
            pps_logMsg(ProgName, PPS_ERROR, logmsg);
            (void) RemoveAggregate(top);
	    return(ERROR) ;
	 }

	 /* Find COMMON HEADER Object
	    Note: ODL converts names to uppercase so it only
	    recognizes searches of names in uppercase */

         if ((hdrobj = FindObject(top, 
				  IMS_Keyword_Table[COMMON_HDR].keyword, 
	           NULL)) == NULL)
         {
	    sprintf(logmsg, "unable to find common header for %s", filepath) ;
            pps_logMsg(ProgName, PPS_ERROR, logmsg);
	    (void) RemoveAggregate(top);
	    return(ERROR) ;
         }
         else
	 {
	    if ((parameter = FindParameter(hdrobj,
			     IMS_Keyword_Table[MSG_TYPE].keyword))
                 != NULL)
            {
	       if ((param_value = FirstValue(parameter)) != NULL)
	       {
		  if (param_value->item.type == TV_SYMBOL ||
		      param_value->item.type == TV_STRING )
		  {
		     for (index=0; IMS_MsgType_Table[index].id != SENTINEL;
			  index++)
                     {
		        if (strcmp(param_value->item.value.string,
				IMS_MsgType_Table[index].keyword) == 0)
                        {
			   if (ftype != IMS_MsgType_Table[index].id)
			   {
                              pps_logMsg(ProgName, PPS_ERROR,
				"File object name and message type value mismatch");
			      (void) RemoveAggregate(top);
			      return(ERROR) ;
			   }
		        }
                     }
		  }
		  else
		  {
		     sprintf(logmsg,
		        " Value type of %s must be string",
		         IMS_Keyword_Table[MSG_TYPE].keyword) ;
                     pps_logMsg(ProgName, PPS_ERROR, logmsg);
		     (void) RemoveAggregate(top);
		     return(ERROR) ;
		  }
	       }
	       else
	       {
		  sprintf(logmsg,
		     "Invalid value for %s keyword",
		      IMS_Keyword_Table[MSG_TYPE].keyword) ;
                  pps_logMsg(ProgName, PPS_ERROR, logmsg);
		  (void) RemoveAggregate(top);
		  return(ERROR) ;
	       }
	    }
	    else
	    {
               sprintf(logmsg, 
		  "Missing expected statement with keyword %s",
		   IMS_Keyword_Table[MSG_TYPE].keyword) ;
               pps_logMsg(ProgName, PPS_ERROR, logmsg);
               (void) RemoveAggregate(top);
               return(ERROR) ;
	    }
         }

      } /* endElse top not NULL */

   (void) RemoveAggregate(top);

   return(ftype) ;

} /* parse_common_hdr */





/*==============================================================================
Function:	int extract_common_header(common_header, top)

Description:	
	This function parses the message contained in the buffer
to obtain and store header information.  The table
IMS_Hdr_Keyword_Table is consulted during the parsing to identify valid
IMS identifiers and expected symbols (ex.  start of a comment line).
If the header record was filled successfully,
OK is returned, otherwise, ERROR is returned.

Parameters:
	Common_Header_Record *common_header - IMS header record where 
	   extracted header information will be stored
        AGGREGATE top - top aggregate from input ODL file

Returns:	
	OK - all header information obtained successfully
	ERROR - memory allocation errors, unable to complete header record

Creator:	Nadia Adhami 	
Creation Date:	05/20/1995 
Notes:		
	It is assumed that the Wallops header field file_name 
common_header->file_name is defined.
==============================================================================*/
#ifdef __STDC__
int
extract_common_header(Common_Header_Record *common_header, AGGREGATE top)
#else
int
extract_common_header(common_header, top)
   Common_Header_Record *common_header ;
   AGGREGATE top ;
#endif
{
   AGGREGATE hdrobj ;
   int status = OK ;                   	/* status returned */
   int index ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
   char tempbuf[20];
   int  rc; /* return code */

   if (top == NULL)
   {
      pps_logMsg(ProgName, PPS_DEBUG, "ODL top node is NULL");
      status = ERROR ;
   }
   else
   {
      /* Search tree for a specific type of object
      -- Note: This must be the same as the value of
      -- MSG_TYPE checked later.
      */
      for (index=L1PR; IMS_Keyword_Table[index].id != SENTINEL;
	   index++)
         if (FindObject(top, IMS_Keyword_Table[index].keyword, NULL) 
	     != NULL)
         {
	    if (IMS_Keyword_Table[index].id == L1PR)
               common_header->file_type_id = IMS_L1PR ;

            else if (IMS_Keyword_Table[index].id == SCAN)
               common_header->file_type_id = IMS_SCAN ;

            else if (IMS_Keyword_Table[index].id == STVEC)
               common_header->file_type_id = IMS_SV_AVAIL ;

            else if (IMS_Keyword_Table[index].id == CANCEL_ORDER)
               common_header->file_type_id = IMS_CANCEL ;

            strcpy(common_header->file_type, 
		   IMS_Keyword_Table[index].keyword) ;
            break ;
         }

      if (IMS_Keyword_Table[index].id == SENTINEL)
      {
         sprintf(logmsg, "File %s is an unsupported file type", 
		          common_header->file_name) ;
         pps_logMsg(ProgName, PPS_ERROR, logmsg);
	 status = ERROR ;
         return( status );
      }

      /* Find COMMON HEADER Object
      -- Note: ODL converts names to uppercase so it only
      -- recognizes searches of names in uppercase
      */
      if ((hdrobj = FindObject(top, 
			       IMS_Keyword_Table[COMMON_HDR].keyword, NULL)) 
                     == NULL)
      {
         sprintf(logmsg, "Unable to find common header for %s", 
		          common_header->file_name) ;
         pps_logMsg(ProgName, PPS_ERROR, logmsg);
	 status = ERROR ;
      }
      else
      {
         /* extract keywords inside the COMMON_HEADER aggregation */

         if ((rc=PPS_extract_param_value(hdrobj, &(common_header->num_records),
                 IMS_Keyword_Table[NUM_RECS].keyword, 0)) != ER_NO_ERROR)
         {
             sprintf(logmsg, "Searching keyword[%s] from file[%s] failed: %s",
	                 IMS_Keyword_Table[NUM_RECS].keyword,
	                 common_header->file_name,
                         pps_err_msgs[rc]) ;
             pps_logMsg(ProgName, PPS_ERROR, logmsg);
             status = ERROR ;
         }

         if ((rc=PPS_extract_param_value(hdrobj, common_header->file_type,
             IMS_Keyword_Table[MSG_TYPE].keyword, MAXLINE -1)) != ER_NO_ERROR)
         {
             sprintf(logmsg, "Searching keyword[%s] from file[%s] failed: %s",
	                 IMS_Keyword_Table[MSG_TYPE].keyword,
	                 common_header->file_name,
                         pps_err_msgs[rc]) ;
             pps_logMsg(ProgName, PPS_ERROR, logmsg);
             status = ERROR ;
         }

         if ((rc=PPS_extract_param_value(hdrobj, &(common_header->date_time),
                 IMS_Keyword_Table[TIME].keyword, 0)) != ER_NO_ERROR)
         {
             sprintf(logmsg, "Searching keyword[%s] from file[%s] failed: %s",
	                 IMS_Keyword_Table[TIME].keyword,
	                 common_header->file_name,
                         pps_err_msgs[rc]) ;
             pps_logMsg(ProgName, PPS_ERROR, logmsg);
             status = ERROR ;
         }

         if ((rc=PPS_extract_param_value(hdrobj, tempbuf,
                 IMS_Keyword_Table[SOURCE].keyword, 20)) != ER_NO_ERROR)
         {
             sprintf(logmsg, "Searching keyword[%s] from file[%s] failed: %s",
	                 IMS_Keyword_Table[SOURCE].keyword,
	                 common_header->file_name,
                         pps_err_msgs[rc]) ;
             pps_logMsg(ProgName, PPS_ERROR, logmsg);
             status = ERROR ;
         }

         if ((rc=PPS_extract_param_value(hdrobj, tempbuf,
                 IMS_Keyword_Table[DESTINATION].keyword, 20)) != ER_NO_ERROR)
         {
             sprintf(logmsg, "Searching keyword[%s] from file[%s] failed",
	                 IMS_Keyword_Table[DESTINATION].keyword,
	                 common_header->file_name,
                         pps_err_msgs[rc]) ;
             pps_logMsg(ProgName, PPS_ERROR, logmsg);
             status = ERROR ;
         }

      }

   }  /* Else top not NULL */

   return(status) ;

} /* extract_common_header */
