/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	ingestCPmsg.c

Description:
	This module contains the routines used for ingesting ODL format 
        CP messages. 

External Functions:
	ingest_CP_msg
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)ingestCPmsg.c	1.1    11/21/96";

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "odldef.h"
#include "odlinter.h"
#include "defs.h"
#include "PPSdefs.h"
#include "PPSextern.h"




/*==============================================================================
Function:	int ingest_CP_msg(char *infile, char *buffer, 
		int *msg_type,
		Common_Header_Record **p_hdr, void *p_rec))

Description:	
	This function ingests an CP msg file in ODL (Object
Description Language).

Parameters:
	char *infile - input CP msg filename
	char *buffer - input buffer containing CP message

Returns:	OK, ERROR
Creator:	Nadia Adhami	
Creation Date:	5/01/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int ingest_CP_msg(char *infile, char buffer[MSG_SIZE], int *msg_type,
    Common_Header_Record **p_hdr, void **p_rec)
#else
int ingest_CP_msg(infile, buffer, msg_type, p_hdr, p_rec)
   char *infile ;
   char buffer[MSG_SIZE] ;
   int  *msg_type ;
   Common_Header_Record **p_hdr;
   void **p_rec)
#endif
{
   AGGREGATE top ;
   int    status = OK ;

   CP_JobReq_Record 	*jobreq = NULL ;
   CP_JobStatus_Record 	*jobstat = NULL ;
   Common_Header_Record *common_hdr = NULL ;

   if (! buffer)
   {
      pps_logMsg(ProgName, PPS_ERROR, "Input CP message is null.") ;
      return(ERROR) ;
   }


   if ((*msg_type = parse_common_hdr(infile, buffer)) == ERROR)
   {
      pps_logMsg(ProgName, PPS_ERROR,
	 "Input CP file is an unknown Message.");
      return(ERROR) ;
   }


   /* Allocate root aggregate node */
   top = NewAggregate(NULL, KA_OBJECT, "root", NULL);

   /* Read CP label. Obtain associated tree */
   if (ReadLabel_buf(buffer, top) == 0)
   {
      pps_logMsg(ProgName, PPS_DEBUG, "ODL routine ReadLabel returned error.");
      (void) RemoveAggregate(top);
      return(ERROR) ;
   }

   /* Allocate CP header record
      Extract header data from input file */

   if ((common_hdr = (Common_Header_Record *)alloc_common_header_record()) 
	== NULL)
   {
      pps_logMsg(ProgName, PPS_DEBUG,
                       "Unable to allocate Wallops header record") ;
      (void) RemoveAggregate(top);
      return(ERROR) ;
   }
   strcpy(common_hdr->file_name, infile) ;
   if ((status = extract_common_header(common_hdr, top)) == ERROR)
   {
      pps_logMsg(ProgName, PPS_DEBUG, "CP header extraction error.") ;
      free(common_hdr) ;
      (void) RemoveAggregate(top);
      return(ERROR) ;
   }

   /*--------------------------------------------------*/
   /* allocate space to hold body record               */
   /*--------------------------------------------------*/
   switch (*msg_type)
   {
     case CP_JOB_REQUEST:
       if (! (jobreq = (CP_JobReq_Record *)alloc_CP_JobReq_Record()) )
       {
           pps_logMsg(ProgName, PPS_DEBUG,
                          "Unable to allocate JobRequest record");
           status = ERROR;
       }
       break;
     case CP_STATUS_MESSAGE:
       if (! (jobstat = (CP_JobStatus_Record *)alloc_CP_JobStatus_Record()) )
       {
           pps_logMsg(ProgName, PPS_DEBUG,
                          "Unable to allocate JobStatus record");
           status = ERROR;
       }
       break;
     default:
       status = ERROR;
       pps_logMsg(ProgName, PPS_ERROR, "CP message type not known.") ;
       break;
   } 

   if (status == ERROR)
   {
           free(common_hdr) ;
           (void) RemoveAggregate(top);
           return (ERROR);
   }

   /*--------------------------------------------------*/
   /* extract the body record from ODL object and      */
   /* return it in p_rec                               */
   /*--------------------------------------------------*/
   switch (*msg_type)
   {
     case CP_JOB_REQUEST:
       if ((status = extract_CP_JobReq_Record(jobreq, top)) == OK)
           *p_rec = (void *)jobreq;
       else
           free(jobreq);
       break;

     case CP_STATUS_MESSAGE:
       if ((status = extract_CP_JobStatus_Record(jobstat, top)) == OK)
           *p_rec = (void *)jobstat;
       else
           free(jobstat);
       break;

     default:
       status = ERROR;
       pps_logMsg(ProgName, PPS_ERROR, "CP message type not known.") ;
       break;
   } 

   if (status == ERROR)
   {
           free(common_hdr) ;
           (void) RemoveAggregate(top);
           return (ERROR);
   }


   /* return the common header object record in p_hdr */
   *p_hdr = common_hdr;

   /* ODL object is no longer needed, remove it */
   (void) RemoveAggregate(top);

   return(OK) ;

} /* ingest_CP_msg */


/* End of File */
