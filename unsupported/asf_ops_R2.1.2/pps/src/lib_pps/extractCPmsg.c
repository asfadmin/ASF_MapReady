/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	extractCPmsg.c

Description:
	This module contains the function for parsing IMS messages.  

External Functions:
	alloc_CP_JobReq_Record
	extract_CP_JobReq_Record
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)extractCPmsg.c	1.2    04/23/97";

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


/*==============================================================================
Function:	CP_JobStatus_Record *alloc_CP_JobReq_Record(void)
Description:	Allocate space for a CP Job Status record.  
Parameters: 	None
Returns:	pointer to newly allocated & initialized record or NULL
Creator:	Norbert Piega
Creation Date:	11/28/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
CP_JobStatus_Record * alloc_CP_JobStatus_Record(void)
#else
CP_JobStatus_Record * alloc_CP_JobStatus_Record()
#endif
{
   CP_JobStatus_Record *rec = NULL ;

   rec = (CP_JobStatus_Record *)util_do_malloc(sizeof(CP_JobStatus_Record)) ;
   if (rec != NULL)
   {
      rec->job_id = -1 ;
      *rec->dataset = NULL ;
      *rec->platform = NULL ;
      *rec->sensor = NULL ;
      *rec->status = NULL ;
      *rec->status_type = NULL ;
      *rec->comment = NULL ;
      *rec->product_filename = NULL ;
   }
   return(rec) ;

} /* alloc_CP_JobStatus_Record */





/*==============================================================================
Function:	CP_JobReq_Record *alloc_CP_JobReq_Record(void)
Description:	Allocate space for a IMS Dub Request record.  
Parameters: 	None
Returns:	pointer to newly allocated & initialized record or NULL
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
CP_JobReq_Record * alloc_CP_JobReq_Record(void)
#else
CP_JobReq_Record * alloc_CP_JobReq_Record()
#endif
{
   CP_JobReq_Record *rec = NULL ;

   rec = (CP_JobReq_Record *)util_do_malloc(sizeof(CP_JobReq_Record)) ;
   if (rec != NULL)
   {
      *rec->request_type = NULL ;
      *rec->processor_mode = NULL ;
   }
   return(rec) ;
} 





/*==============================================================================
Function:	
	int extract_CP_JobStatus_Record(CP_JobStatus_Record *rec, 
					AGGREGATE top)

Description:	
	Get job status info from CP ODL message and store in the CP job
status record data structure.

Parameters:
	rec - data structure where extracted data information will be stored
        top - topmost aggregate object for input file

Returns:	
	ACCEPT - message was parsed successfully
	ERROR -  unable to complete data extraction

Creator:	Norbert Piega	
Creation Date:	11/28/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int
extract_CP_JobStatus_Record(CP_JobStatus_Record *rec, AGGREGATE top)
#else
int
extract_CP_JobStatus_Record(rec, top)
   CP_JobStatus_Record *rec ;
   AGGREGATE top ;
#endif
{
   AGGREGATE object ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
   int  rc;                        /* return code */

   if (top == NULL)
   {
      pps_logMsg(ProgName, PPS_DEBUG, "ODL top node is NULL");
      return(ERROR) ;
   }

   if ((object = FindObject(top, BODY_KEYWD, NULL)) == NULL)
   {
      sprintf(logmsg, "Unable to find %s object", BODY_KEYWD) ;
      pps_logMsg(ProgName, PPS_ERROR, logmsg);
      return(ERROR) ;
   }

   /* extract fields inside the BODY aggregation */
   /* ignore xxx_job_start and xxx_job_end keywords */

   if ((rc=PPS_extract_param_value(object, &(rec->job_id), JOB_ID_KEYWD, 0))
                                        != ER_NO_ERROR)
   {
       sprintf(logmsg, "Keyword[%s] for CP job status: %s",
                              JOB_ID_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR) ;
   }

   if ((rc=PPS_extract_param_value(object, rec->product_filename,
                    PRODUCT_ID_KEYWD, MAX_FILENAME_LEN -1)) != ER_NO_ERROR)
   {
       sprintf(logmsg, "Keyword[%s] for CP job status: %s",
                              PRODUCT_ID_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR) ;
   }

   if ((rc=PPS_extract_param_value(object, rec->dataset, DATASET_KEYWD,
                    MAXLINE -1)) != ER_NO_ERROR)
   {
       sprintf(logmsg, "Keyword[%s] for CP job status: %s",
                              DATASET_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR) ;
   }

   if ((rc=PPS_extract_param_value(object, rec->platform, PLATFORM_KEYWD,
                    MAXLINE -1)) != ER_NO_ERROR)
   {
       sprintf(logmsg, "Keyword[%s] for CP job status: %s",
                              PLATFORM_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR) ;
   }

   if ((rc=PPS_extract_param_value(object, rec->sensor, SENSOR_KEYWD,
                    MAXLINE -1)) != ER_NO_ERROR)
   {
       sprintf(logmsg, "Keyword[%s] for CP job status: %s",
                              SENSOR_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR) ;
   }

   if ((rc=PPS_extract_param_value(object, rec->status, STATUS_KEYWD,
                    MAXLINE -1)) != ER_NO_ERROR)
   {
       sprintf(logmsg, "Keyword[%s] for CP job status: %s",
                              STATUS_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR) ;
   }
   else
   {
       /* Validate this keyword value because send_status_to_PPS function
          expects it to be correct */ 
      if (strcmp(rec->status, COMPLETED)  != 0 && 
          strcmp(rec->status, CANCELFAIL) != 0 &&
          strcmp(rec->status, RETRY) != 0)
      {
         sprintf(logmsg, "Keyword[%s] for CP job status: %s",
                 STATUS_KEYWD, pps_err_msgs[ER_INVALID_KEYWORD_VALUE_TYPE]);
         pps_logMsg(ProgName, PPS_ERROR, logmsg);
         return(ERROR) ;
      }
   }

   if ((rc=PPS_extract_param_value(object, rec->status_type, STATUS_TYPE_KEYWD,
                    MAXLINE -1)) != ER_NO_ERROR)
   {
       sprintf(logmsg, "Keyword[%s] for CP job status: %s",
                              STATUS_TYPE_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR) ;
   }

   if ((rc=PPS_extract_param_value(object, rec->comment, COMMENT_KEYWD,
                    MAXLINE -1)) != ER_NO_ERROR)
   {
       sprintf(logmsg, "Keyword[%s] for CP job status: %s",
                              COMMENT_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR) ;
   }

   if ((rc=PPS_extract_param_value(object, rec->request_type,
                    REQUEST_TYPE_KEYWD, MAXLINE -1)) != ER_NO_ERROR)
   {
       sprintf(logmsg, "Keyword[%s] for CP job status: %s",
                              REQUEST_TYPE_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR) ;
   }
   else
   {
       /* Validate this keyword value because send_status_to_PPS function
          expects it to be correct */ 
      if (strcmp(rec->request_type, SCAN_KEYWD)  != 0 && 
          strcmp(rec->request_type, FRAME_KEYWD) != 0) 
      {   
         sprintf(logmsg, "Keyword[%s] for CP job status: %s",
              REQUEST_TYPE_KEYWD, pps_err_msgs[ER_INVALID_KEYWORD_VALUE_TYPE]);
         pps_logMsg(ProgName, PPS_ERROR, logmsg);
         return(ERROR) ;
      }
   }


   return(OK);

} /* extract_CP_JobStatus_Record */





/*==============================================================================
Function:	
	int extract_CP_JobReq_Record(CP_JobReq_Record *rec, AGGREGATE top)

Description:	
	This function parses the input file represented by the file
pointer fp to obtain and store datatake record information.  The 
table IMS_Hdr_Keyword_Table is consulted during the parsing to identify 
valid IMS identifiers and expected symbols (ex.  start of a comment line).

Parameters:
	rec - data structure where extracted data information will be stored
        top - topmost aggregate object for input file

Returns:	
	ACCEPT - message was parsed successfully
	ERROR -  unable to complete data extraction

Creator:	Nadia Adhami	
Creation Date:	5/17/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int
extract_CP_JobReq_Record(CP_JobReq_Record *rec, AGGREGATE top)
#else
int
extract_CP_JobReq_Record(rec, top)
   CP_JobReq_Record *rec ;
   AGGREGATE top ;
#endif
{
   AGGREGATE object ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
   int  rc;                        /* return code */

   if (top == NULL)
   {
      pps_logMsg(ProgName, PPS_DEBUG, "ODL top node is NULL");
      return(ERROR) ;
   }

   if ((object = FindObject(top, BODY_KEYWD, NULL)) == NULL)
   {
      sprintf(logmsg, "Unable to find %s object", BODY_KEYWD) ;
      pps_logMsg(ProgName, PPS_ERROR, logmsg);
      return(ERROR) ;
   }

   /* extract fields inside the BODY aggregation */

   if ((rc=PPS_extract_param_value(object, rec->request_type,
                     REQUEST_TYPE_KEYWD, REQUEST_TYPE_STRLEN)) != ER_NO_ERROR)
   {
       sprintf(logmsg, "Keyword[%s] for CP job request: %s",
                              REQUEST_TYPE_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR) ;
   }

   if ((rc=PPS_extract_param_value(object, rec->processor_mode,
                 PROCESSOR_MODE_KEYWD, PROCESSOR_MODE_STRLEN)) != ER_NO_ERROR)
   {
       sprintf(logmsg, "Keyword[%s] for CP job request: %s",
                              PROCESSOR_MODE_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR) ;
   }

   return(OK);

} /* extract_CP_JobReq_Record */
