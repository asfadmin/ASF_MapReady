/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	ingestIMSmsg.c

Description:
	This module contains the routines used for ingesting ODL format 
        IMS messages. 

External Functions:
	ingest_IMS_msg
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)ingestIMSmsg.c	1.2    12/16/96";

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "odldef.h"
#include "odlinter.h"
#include "defs.h"
#include "PPSdefs.h"
#include "PPSextern.h"
#include "PPSerr.h"


/*==============================================================================
Function:	int ingest_IMS_msg(char *infile, char *buffer, int *filetype,
		Common_Header_Record **p_hdr, void *p_rec))

Description:	
	This function ingests an IMS msg file in ODL (Object
Description Language).

Parameters:
	char *infile - input IMS msg filename
	char *buffer - input buffer containing IMS message

Returns:
        OK if successful
        ERROR otherwise

Creator:	Nadia Adhami	
Creation Date:	5/01/1995
Notes:		
==============================================================================*/
#ifdef __STDC__
int ingest_IMS_msg(char *infile, char buffer[MSG_SIZE], int *filetype,
    Common_Header_Record **p_hdr, void **p_rec)
#else
int ingest_IMS_msg(infile, buffer, filetype, p_hdr, p_rec)
   char                   *infile ;
   char                   buffer[MSG_SIZE] ;
   int                    *filetype ;             /* RETURN */
   Common_Header_Record   **p_hdr;                /* RETURN */
   void                   **p_rec)                /* RETURN */
#endif
{
   AGGREGATE top ;
   int    status = OK;

   Common_Header_Record *common_hdr = NULL ;
   IMS_CancelReq_Record *cancel_rec = NULL ;
   IMS_L1PReq_Record *l1p_rec = NULL ;
   IMS_ScanReq_Record *scan_rec = NULL ;
   IMS_SVecAvail_Record *svavail_rec = NULL ;

   /* Open error log */

   if (! buffer)
   {
      pps_logMsg(ProgName, PPS_ERROR, pps_err_msgs[ER_INPUT_NULL]);
      return(ERROR);
   }

   if ((*filetype = parse_common_hdr(infile, buffer)) == ERROR)
   {
      pps_logMsg(ProgName, PPS_ERROR, pps_err_msgs[ER_PARSE_COMMON_HDR]);
      return(ERROR) ;
   }

   /* Allocate root aggregate node */
   top = NewAggregate(NULL, KA_OBJECT, "root", NULL);

   /* Read IMS label. Obtain associated tree */
   if (ReadLabel_buf(buffer, top) == 0)
   {
      pps_logMsg(ProgName, PPS_DEBUG, "ODL routine ReadLabel returned error.");
      (void) RemoveAggregate(top);
      return(ERROR) ;
   }

   /* Allocate IMS header record
      Extract header data from input file */

   if ((common_hdr = (Common_Header_Record *)alloc_common_header_record()) 
	== NULL)
   {
      pps_logMsg(ProgName, PPS_DEBUG, pps_err_msgs[ER_MALLOC_COMMON_HDR]) ;
      (void) RemoveAggregate(top);
      return(ERROR) ;
   }
   strcpy(common_hdr->file_name, infile) ;
   if (extract_common_header(common_hdr, top) != OK)
   {
      pps_logMsg(ProgName, PPS_ERROR, pps_err_msgs[ER_COMMON_HDR_EXTRACT]) ;
      free(common_hdr) ;
      (void) RemoveAggregate(top);
      return(ERROR) ;
   }

   /*-------------------------------------------------*/
   /* allocate the data structure to hold the message */
   /*-------------------------------------------------*/
   switch (*filetype)
   {
     case IMS_SCAN:
       if (! (scan_rec = (IMS_ScanReq_Record *)alloc_IMS_ScanReq_Record()) )
       {
           pps_logMsg(ProgName, PPS_DEBUG,
                            "Unable to allocate IMS Cancel Request record");
           status = ERROR;
       }
       break;
     case IMS_CANCEL:
       if (! (cancel_rec =
                       (IMS_CancelReq_Record *)alloc_IMS_CancelReq_Record()) )
       {
           pps_logMsg(ProgName, PPS_DEBUG,
                       "Unable to allocate IMS Cancel Request record");
           status = ERROR;
       }
       break;
     case IMS_L1PR:
       if (! (l1p_rec = (IMS_L1PReq_Record *)alloc_IMS_L1PReq_Record()))
       {
           pps_logMsg(ProgName, PPS_DEBUG,
                       "Unable to allocate L1PRequest record");
           status = ERROR;
       }
       break;
     case IMS_SV_AVAIL:
       if (!(svavail_rec=(IMS_SVecAvail_Record *)alloc_IMS_SVecAvail_Record()))
       {
           pps_logMsg(ProgName, PPS_DEBUG,
                       "Unable to allocate State Vector Avail record");
           status = ERROR;
       }
       break;
     default:
       pps_logMsg(ProgName, PPS_DEBUG, pps_err_msgs[ER_UNKNOWN_IMS_MSG]);
       status = ERROR ;
       break;
   } 

   if (status != OK)
   {
           free(common_hdr) ;
           (void) RemoveAggregate(top);
           return(ERROR) ;
   }


   /* return the body object record in p_rec */
   switch (*filetype)
   {
     case IMS_SCAN:
       strcpy(scan_rec->file_name, infile) ;
       if ((status = extract_IMS_ScanReq_Record(scan_rec, top)) == OK)
           *p_rec = (void *)scan_rec;
       else
           free(scan_rec);
       break;

     case IMS_CANCEL:
       strcpy(cancel_rec->file_name, infile) ;
       if ((status = extract_IMS_CancelReq_Record(cancel_rec, top)) == OK)
           *p_rec = (void *)cancel_rec;
       else
           free(cancel_rec);
       break;

     case IMS_L1PR:
       strcpy(l1p_rec->file_name, infile) ;
       if ((status = extract_IMS_L1PReq_Record(l1p_rec, top)) == OK)
           *p_rec = (void *)l1p_rec;
       else
           free(l1p_rec);
       break;

     case IMS_SV_AVAIL:
       strcpy(svavail_rec->file_name, infile) ;
       if ((status = extract_IMS_SVecAvail_Record(svavail_rec, top)) == OK)
           *p_rec = (void *)svavail_rec;
       else
           free(svavail_rec);
       break;

     default:
       break;
   }

   if (status != OK)
   {
           free(common_hdr) ;
           (void) RemoveAggregate(top);
           return(ERROR) ;
   }

   /* return the common header object record in p_hdr */
   *p_hdr = common_hdr;

   /* ODL object is no longer needed, remove it */
   (void) RemoveAggregate(top);

   return(OK) ;

} /* ingest_IMS_msg */

/* End of File */
