/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	extractIMSmsg.c

Description:
	This module contains the function for parsing IMS messages.  

External Functions:
	alloc_IMS_CancelReq_Record
	alloc_IMS_L1PReq_Record
	alloc_IMS_ScanReq_Record
	alloc_IMS_StateVec_Record

	extract_IMS_CancelReq_Record
	extract_IMS_L1PReq_Record
	extract_IMS_ScanReq_Record
	extract_IMS_StateVec_Record
	
Static Functions:
	None
	
External Variables Defined:
	None
	
File Scope Static Variables:
	None
	
Notes:
==============================================================================*/

static char SccsFileId[] = "@(#)extractIMSmsg.c	1.2    12/16/96";

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

struct PPS_ExtractMaps
{
    int      offset;
    char*    keyword;
    short    length;
};

#define EXTRACT_L1_PARAMS_CHECK_ERROR(obj,record,keyword,length,order,item) \
   if ((rc=PPS_extract_param_value(object, record, keyword, length)) \
                                        != ER_NO_ERROR) \
   { \
       sprintf(logmsg, \
              "Keyword[%s] for IMS L1 Order/Item[%d/%d]: %s", \
              keyword, order, item, pps_err_msgs[rc]); \
       pps_logMsg(ProgName, PPS_ERROR, logmsg); \
       return(ERROR); \
   }

#define EXTRACT_SCAN_PARAMS_CHECK_ERROR(obj,record,keyword,length,order,item) \
   if ((rc=PPS_extract_param_value(object, record, keyword, length)) \
                                        != ER_NO_ERROR) \
   { \
       sprintf(logmsg, \
              "Keyword[%s] for IMS Scan Order/Item[%d/%d]: %s", \
              keyword, order, item, pps_err_msgs[rc]); \
       pps_logMsg(ProgName, PPS_ERROR, logmsg); \
       return(ERROR); \
   }

/*-------------------------------------------------------*/
/* extraction map for L1 request orders' Body Object     */
/* which don't require special processing                */
/*-------------------------------------------------------*/
static struct PPS_ExtractMaps L1BodyExtractTable[] =
{
    { StructOffset(IMS_L1PReq_Record*, order_type), ORDER_TYPE_KEYWD,
                                                    ORDER_TYPE_STRLEN },
    { StructOffset(IMS_L1PReq_Record*, priority), PRIORITY_KEYWD,
                                                    PRIORITY_STRLEN },
    { StructOffset(IMS_L1PReq_Record*, platform), PLATFORM_KEYWD, 
                                                    PLATFORM_STRLEN },
    { StructOffset(IMS_L1PReq_Record*, sensor), SENSOR_KEYWD, 
                                                    SENSOR_STRLEN },
    { StructOffset(IMS_L1PReq_Record*, rev), REVOLUTION_KEYWD, 0 },
    { StructOffset(IMS_L1PReq_Record*, mode), MODE_KEYWD, MODE_STRLEN },
    { StructOffset(IMS_L1PReq_Record*, quicklook_flag), QUICKLOOK_FLAG_KEYWD, 
                                                    LOGICAL_STRLEN },
    { StructOffset(IMS_L1PReq_Record*, datatake_seq), SEQUENCE_KEYWD, 0 },
    { StructOffset(IMS_L1PReq_Record*, activity_id), ACTIVITY_ID_KEYWD, 
                                                    ACTIVITY_ID_STRLEN },
    { StructOffset(IMS_L1PReq_Record*, product_type), PRODUCT_TYPE_KEYWD, 
                                                    PRODUCT_TYPE_STRLEN },
    { StructOffset(IMS_L1PReq_Record*, pixel_spacing), PIXEL_SPACING_KEYWD, 0 },
    { StructOffset(IMS_L1PReq_Record*, frame_id), FRAME_ID_KEYWD, 0 },
    { StructOffset(IMS_L1PReq_Record*, frame_mode), FRAME_MODE_KEYWD, 
                                                    FRAME_MODE_STRLEN },
    { StructOffset(IMS_L1PReq_Record*, output_format), OUTPUT_FORMAT_KEYWD, 
                                                    OUTPUT_FMT_STRLEN },
    { StructOffset(IMS_L1PReq_Record*, site_name), SITE_NAME_KEYWD, 
                                                    SITE_NAME_STRLEN },
    { StructOffset(IMS_L1PReq_Record*, projection), PROJECTION_KEYWD, 
                                                    PROJECTION_STRLEN },
    { StructOffset(IMS_L1PReq_Record*, processing_gain), PROCESSING_GAIN_KEYWD, 
                                                    0 },
    { StructOffset(IMS_L1PReq_Record*, deskew), DESKEW_KEYWD, LOGICAL_STRLEN },
    { StructOffset(IMS_L1PReq_Record*, compensation_flag), 
                                                    COMPENSATION_KEYWD, 
                                                    LOGICAL_STRLEN },
    { StructOffset(IMS_L1PReq_Record*, avg_terrain_ht), AVG_TERRAIN_HT_KEYWD,
                                                    0 },
    { StructOffset(IMS_L1PReq_Record*, terrain_correction),
                                                    TERRAIN_CORRECTION_KEYWD,
                                                    LOGICAL_STRLEN }
}; /*L1BodyExtractTable*/

/*-------------------------------------------------------*/
/* extraction map for Scan request orders' Body Object   */
/* which don't require special processing                */
/*-------------------------------------------------------*/
static struct PPS_ExtractMaps ScanBodyExtractTable[] =
{
    { StructOffset(IMS_ScanReq_Record*, order_type), ORDER_TYPE_KEYWD,
                                                    ORDER_TYPE_STRLEN },
    { StructOffset(IMS_ScanReq_Record*, priority), PRIORITY_KEYWD,
                                                    PRIORITY_STRLEN },
    { StructOffset(IMS_ScanReq_Record*, platform), PLATFORM_KEYWD, 
                                                    PLATFORM_STRLEN },
    { StructOffset(IMS_ScanReq_Record*, sensor), SENSOR_KEYWD, 
                                                    SENSOR_STRLEN },
    { StructOffset(IMS_ScanReq_Record*, rev), REVOLUTION_KEYWD, 0 },
    { StructOffset(IMS_ScanReq_Record*, mode), MODE_KEYWD, MODE_STRLEN },
    { StructOffset(IMS_ScanReq_Record*, quicklook_flag), QUICKLOOK_FLAG_KEYWD, 
                                                    LOGICAL_STRLEN },
    { StructOffset(IMS_ScanReq_Record*, datatake_seq), SEQUENCE_KEYWD, 0 },
    { StructOffset(IMS_ScanReq_Record*, activity_id), ACTIVITY_ID_KEYWD, 
                                                    ACTIVITY_ID_STRLEN },
    { StructOffset(IMS_ScanReq_Record*, frame_mode), FRAME_MODE_KEYWD, 
                                                    FRAME_MODE_STRLEN },
    { StructOffset(IMS_ScanReq_Record*, site_name), SITE_NAME_KEYWD, 
                                                    SITE_NAME_STRLEN }
}; /*ScanBodyExtractTable*/

/*==============================================================================
Function:	init_tc(), init_gha(), init_sv()
Description:	Initialize corresponding data structures
Parameters: 	pointer to a data structure
Returns:	None
Creator:	Nadia Adhami
Creation Date:	5/1/1995
Notes:		
==============================================================================*/
void init_tc( struct Time_Correlation *tc)
{
	tc->rev = 0 ;
	tc->sat_time = 0 ;
	tc->clock_cycle = 0 ;
}
void init_gha( struct GHA_Correction *gha)
{
	gha->angle = 0 ;
}
void init_sv( struct State_Vector *sv)
{
        memset(sv->precision, 0, SVEC_TYPE_STRLEN+1);
        memset(sv->coord_sys, 0, COORD_SYS_STRLEN+1);
        memset(sv->platform, 0, PLATFORM_STRLEN+1);
	sv->rev = 0;
	sv->x_pos = 0; sv->y_pos = 0; sv->z_pos = 0;
	sv->x_vel = 0; sv->y_vel = 0; sv->z_vel = 0;
}

/*==============================================================================
Function:	IMS_L1PReq_Record * alloc_IMS_L1PReq_Record(void)
Description:	Allocate IMS L1 Request Record	
Parameters:	None
Returns:	Pointer to newly allocated record	
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 14:37:26 PDT 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
IMS_L1PReq_Record * alloc_IMS_L1PReq_Record(void)
#else
IMS_L1PReq_Record * alloc_IMS_L1PReq_Record()
#endif
{
   IMS_L1PReq_Record *rec = NULL ;

   rec = (IMS_L1PReq_Record *)util_do_malloc(sizeof(IMS_L1PReq_Record)) ;
   if (rec != NULL)
   {
      *rec->order_type = '\0' ;
      rec->order_id = 0 ;
      rec->item_id = 0 ;
      *rec->priority = '\0' ;
      *rec->platform = '\0' ;
      *rec->sensor = '\0' ;
      rec->rev = 0 ;
      *rec->mode = '\0' ;

      rec->datatake_seq = 0 ;
      *rec->activity_id = '\0' ;
      *rec->product_type = '\0' ;
      rec->pixel_spacing = 0 ;
      rec->frame_id = 0 ;
      rec->subframe_id = 0 ;
      *rec->frame_mode = '\0' ;
      *rec->output_format = '\0' ;
      *rec->site_name = '\0' ;
      *rec->projection = '\0' ;

      rec->processing_gain = 0 ;
      *rec->deskew = '\0' ;
      rec->avg_terrain_ht = 0.0 ;
      rec->ps_reference_lat = 0.0 ;
      rec->ps_reference_lon = 0.0 ;
      rec->lambert_latitude_n = 0.0 ;
      rec->lambert_latitude_s = 0.0 ;
      rec->utm_zone = 0 ;
      *rec->terrain_correction = '\0' ;
      *rec->compensation_flag = '\0' ;

      *rec->cal_params_file = '\0' ;
      *rec->cal_params_file2 = '\0' ;
      *rec->scan_results_file = '\0' ;

      *rec->media_id = '\0' ;
      *rec->media_type = '\0' ;
      *rec->media_location = '\0' ;
      *rec->data_direction = '\0' ;

      *rec->recorder_id = '\0' ;
      *rec->station_id = '\0' ;

      *rec->comment = '\0' ;

      init_gha( &rec->gha );
      init_tc( &rec->tc );
      init_sv( &rec->sv );

   }
   return(rec) ;

} /* alloc_IMS_L1PReq_Record */





/*==============================================================================
Function:	IMS_ScanReq_Record * alloc_IMS_ScanReq_Record(void)
Description:	Allocate IMS Scan Request Record
Parameters:	None
Returns:	Pointer to newly allocated record
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 14:38:50 PDT 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
IMS_ScanReq_Record * alloc_IMS_ScanReq_Record(void)
#else
IMS_ScanReq_Record * alloc_IMS_ScanReq_Record()
#endif
{
   IMS_ScanReq_Record*rec = NULL ;

   rec = (IMS_ScanReq_Record *)util_do_malloc(sizeof(IMS_ScanReq_Record)) ;
   if (rec != NULL)
   {
      *rec->order_type = '\0' ;
      rec->order_id = 0 ;
      rec->item_id = 0 ;
      *rec->priority = '\0' ;
      *rec->platform = '\0' ;
      *rec->sensor = '\0' ;
      rec->rev = 0 ;
      *rec->mode = '\0' ;
 
      rec->datatake_seq = 0 ;
      *rec->activity_id = '\0' ;
      *rec->frame_mode = '\0' ;
      *rec->site_name = '\0' ;

      *rec->media_id = '\0' ;
      *rec->media_type = '\0' ;
      *rec->media_location = '\0' ;
      *rec->data_direction = '\0' ;
 
      rec->start_address = 0 ;
      rec->end_address = 0 ;
      *rec->recorder_id = '\0' ;
      *rec->station_id = '\0' ;
      *rec->comment = '\0' ;

      init_gha( &rec->gha );
      init_tc( &rec->tc );
      init_sv( &rec->sv );
   }
   return(rec) ;

} /* alloc_IMS_ScanReq_Record */
 




/*==============================================================================
Function:	IMS_CancelReq_Record * alloc_IMS_CancelReq_Record(void)
Description:	Allocate IMS Cancel Request Record	
Parameters:	None
Returns:	Pointer to newly allocated record	
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 14:41:35 PDT 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
IMS_CancelReq_Record * alloc_IMS_CancelReq_Record(void)
#else
IMS_CancelReq_Record * alloc_IMS_CancelReq_Record()
#endif
{
   IMS_CancelReq_Record *rec = NULL ;

   rec = (IMS_CancelReq_Record *)util_do_malloc(sizeof(IMS_CancelReq_Record)) ;
   if (rec != NULL)
   {
      rec->order_id = 0 ;
      rec->item_id = 0 ;
      *rec->request_type = NULL ;

   }
   return(rec) ;

} /* alloc_IMS_CancelReq_Record */




/*==============================================================================
Function:	IMS_SVecAvail_Record * alloc_IMS_SVecAvail_Record(void)
Description:	Allocate IMS State Vector Available Record
Parameters:	None
Returns:	Pointer to newly allocated record
Creator:	Nadia Adhami
Creation Date:	Tue Oct 17 14:43:10 PDT 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
IMS_SVecAvail_Record * alloc_IMS_SVecAvail_Record(void)
#else
IMS_SVecAvail_Record * alloc_IMS_SVecAvail_Record()
#endif
{
   IMS_SVecAvail_Record *rec = NULL ;

   rec = (IMS_SVecAvail_Record *)util_do_malloc(sizeof(IMS_SVecAvail_Record)) ;
   if (rec != NULL)
   {
      *rec->platform = NULL ;
      *rec->type = NULL ;
      rec->start_rev = 0 ;
      rec->end_rev = 0 ;

   }
   return(rec) ;

} /* alloc_IMS_SVecAvail_Record */





/*==============================================================================
Function:	
	int extract_IMS_CancelReq_Record(IMS_CancelReq_Record *rec, AGGREGATE top)
	int extract_IMS_L1PReq_Record(IMS_L1PReq_Record *rec, AGGREGATE top)
	int extract_IMS_ScanReq_Record(IMS_ScanReq_Record *rec, AGGREGATE top)
	int extract_IMS_SVecAvail_Record(IMS_ScanReq_Record *rec, AGGREGATE top)

Description:	
	This function parses the input file represented by the file
pointer fp to obtain and store datatake record information.  The 
table IMS_Hdr_Keyword_Table is consulted during the parsing to identify 
valid IMS identifiers and expected symbols (ex.  start of a comment line).

Parameters:
	rec - data structure where extracted data information will be stored
        top - topmost aggregate object for input file

Returns:	
	OK     - message was parsed successfully
	ERROR -  unable to complete data extraction

Creator:	Nadia Adhami	
Creation Date:	5/17/1995
Notes:		
==============================================================================*/

#ifdef __STDC__
int
extract_IMS_L1PReq_Record(IMS_L1PReq_Record *rec, AGGREGATE top)
#else
int
extract_IMS_L1PReq_Record(rec, top)
   IMS_L1PReq_Record *rec ;
   AGGREGATE top ;
#endif
{
   AGGREGATE            object ;
   char                 logmsg[MAX_SYSLOG_MSGLEN+1] ;
   struct ODLDate 	odl_date;
   int                  k;      /* index */
   int                  rc;     /* return code */

   if (top == NULL)
   {
      pps_logMsg(ProgName, LOG_DEBUG, "ODL top node is NULL");
      return(ERROR) ;
   }

   if ((object = FindObject(top, BODY_KEYWD, NULL)) == NULL)
   {
      sprintf(logmsg, "Unable to find %s object for %s",
                   BODY_KEYWD, rec->file_name) ;
      pps_logMsg(ProgName, PPS_ERROR, logmsg);
      return(ERROR) ;
   }

   /*-------------------------------------------------*/
   /* extract fields inside the BODY aggregation      */
   /*-------------------------------------------------*/
   /*-------------------------------------------------*/
   /* extract order and item id first                 */
   /*-------------------------------------------------*/
   if ((rc=PPS_extract_param_value(object, &(rec->order_id),
                                ORDER_ID_KEYWD, 0)) != ER_NO_ERROR)
   {
       sprintf(logmsg,
                    "Keyword[%s] for IMS L1 order: %s",
                    ORDER_ID_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR);
   }
   if ((rc=PPS_extract_param_value(object, &(rec->item_id),
                                ITEM_ID_KEYWD, 0)) != ER_NO_ERROR)
   {
       sprintf(logmsg,
                    "Keyword[%s] for IMS L1 order[%d]: %s",
                    ITEM_ID_KEYWD, rec->order_id, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR);
   }
   /*-------------------------------------------------*/
   /* next: do the ones don't need special processing */
   /*-------------------------------------------------*/
   for (k=0; k < ElementNumber(L1BodyExtractTable); k++)
   {
       EXTRACT_L1_PARAMS_CHECK_ERROR(obj,
                              (char*)rec + L1BodyExtractTable[k].offset,
                              L1BodyExtractTable[k].keyword,
                              L1BodyExtractTable[k].length,
                              rec->order_id,
                              rec->item_id);
   }

   /*----------------------------------------------------------------*/
   /* extract "sub_frameid" if product_type == COMPLEX               */
   /*----------------------------------------------------------------*/
   if (rec->product_type[0] && strcmp(rec->product_type, COMPLEX_KEYWD) == 0)
       EXTRACT_L1_PARAMS_CHECK_ERROR(obj, &(rec->subframe_id),
                    SUBFRAME_ID_KEYWD, 0, rec->order_id, rec->item_id);

   /*----------------------------------------------------------------*/
   /* extract "PS" or "LAMBERT" or "UTM" depending on the projection */
   /*----------------------------------------------------------------*/
   if (rec->projection[0] && strcmp(rec->projection, PS_KEYWD) == 0)
   {
       EXTRACT_L1_PARAMS_CHECK_ERROR(obj, &(rec->ps_reference_lat),
                    PS_REFERENCE_LAT_KEYWD, 0, rec->order_id, rec->item_id);
       EXTRACT_L1_PARAMS_CHECK_ERROR(obj, &(rec->ps_reference_lon),
                    PS_REFERENCE_LON_KEYWD, 0, rec->order_id, rec->item_id);
   }
   else if (rec->projection[0] && strcmp(rec->projection, LAMBERT_KEYWD) == 0)
   {
       EXTRACT_L1_PARAMS_CHECK_ERROR(obj, &(rec->lambert_latitude_n),
                    LAMBERT_LATITUDE_N_KEYWD, 0, rec->order_id, rec->item_id);
       EXTRACT_L1_PARAMS_CHECK_ERROR(obj, &(rec->lambert_latitude_s),
                    LAMBERT_LATITUDE_S_KEYWD, 0, rec->order_id, rec->item_id);
   }
   else if (rec->projection[0] && strcmp(rec->projection, UTM_KEYWD) == 0)
   {
       EXTRACT_L1_PARAMS_CHECK_ERROR(obj, &(rec->utm_zone),
                    UTM_ZONE_KEYWD, 0, rec->order_id, rec->item_id);
   }

   /*---------------------------------*/
   /* SOURCE_MEDIA object is optional */
   /*---------------------------------*/
   if ((object = FindObject(top, SOURCE_MEDIA_KEYWD, NULL)) == NULL)
   {
      sprintf(logmsg, "Unable to find %s object for %s", 
                         SOURCE_MEDIA_KEYWD, rec->file_name) ;
      pps_logMsg(ProgName, PPS_DEBUG, logmsg);
      return(OK) ;
   }

   EXTRACT_L1_PARAMS_CHECK_ERROR(obj, rec->media_type,
          MEDIA_TYPE_KEYWD, MEDIA_TYPE_STRLEN, rec->order_id, rec->item_id);
   EXTRACT_L1_PARAMS_CHECK_ERROR(obj, rec->media_id,
          MEDIA_ID_KEYWD, MEDIA_ID_STRLEN, rec->order_id, rec->item_id);
 
   /*---------------------------------------------------------*/
   /* if media_type = DISK, need media_location               */ 
   /*---------------------------------------------------------*/
   if (strcmp(rec->media_type, DISK_KEYWD) == 0)
   {
       EXTRACT_L1_PARAMS_CHECK_ERROR(obj, rec->media_location,
            MEDIA_LOCATION_KEYWD, MEDIA_LOCATION_STRLEN,
            rec->order_id, rec->item_id);
   }
   /*---------------------------------------------------------*/
   /* for all other media types, don't need media_location    */ 
   /* if media_location doesn't exist, don't complain         */
   /*---------------------------------------------------------*/
   else
   {
       rc = PPS_extract_param_value(object, rec->media_location,
                         MEDIA_LOCATION_KEYWD, MEDIA_LOCATION_STRLEN);
       if (rc != ER_NO_ERROR && rc != ER_KEYWORD_NOT_FOUND)
       {
           sprintf(logmsg,
                    "Keyword[%s] for IMS L1 Order/Item[%d/%d]: %s",
                    ITEM_ID_KEYWD, rec->order_id,
                    rec->item_id, pps_err_msgs[rc]);
           pps_logMsg(ProgName, PPS_ERROR, logmsg);
           return(ERROR);
       }
   }
 
   EXTRACT_L1_PARAMS_CHECK_ERROR(obj, &odl_date, CENTER_TIME_KEYWD, 0,
            rec->order_id, rec->item_id);
   /* save the center time in the record */
   convert_ODLdate_to_timerec (&odl_date , &(rec->center_time)) ;

   EXTRACT_L1_PARAMS_CHECK_ERROR(obj, &odl_date, START_TIME_KEYWD, 0,
            rec->order_id, rec->item_id);
   /* save the start time in the record */
   convert_ODLdate_to_timerec (&odl_date , &(rec->start_time)) ;

   EXTRACT_L1_PARAMS_CHECK_ERROR(obj, &odl_date, END_TIME_KEYWD, 0,
            rec->order_id, rec->item_id);
   /* save the end time in the record */
   convert_ODLdate_to_timerec (&odl_date , &(rec->end_time)) ;

   EXTRACT_L1_PARAMS_CHECK_ERROR(obj, rec->recorder_id, RECORDER_ID_KEYWD,
            RECORDER_ID_STRLEN, rec->order_id, rec->item_id);
   EXTRACT_L1_PARAMS_CHECK_ERROR(obj, rec->station_id, STATION_ID_KEYWD,
            STATION_ID_STRLEN, rec->order_id, rec->item_id);
   EXTRACT_L1_PARAMS_CHECK_ERROR(obj, rec->data_direction, DATADIRECTION_KEYWD,
            DATA_DIRECTION_STRLEN, rec->order_id, rec->item_id);
   return(OK) ;

} /* extract_IMS_L1PReq_Record */


#ifdef __STDC__
int
extract_IMS_ScanReq_Record(IMS_ScanReq_Record *rec, AGGREGATE top)
#else
int
extract_IMS_ScanReq_Record(rec, top)
   IMS_ScanReq_Record *rec ;
   AGGREGATE top ;
#endif
{
   AGGREGATE            object ;
   char                 logmsg[MAX_SYSLOG_MSGLEN+1] ;
   struct ODLDate 	odl_date;
   int                  k;          /* index */
   int                  rc;         /* return code */

   if (top == NULL)
   {
      pps_logMsg(ProgName, PPS_DEBUG, "ODL top node is NULL");
      return(ERROR) ;
   }

   if ((object = FindObject(top, BODY_KEYWD, NULL)) == NULL)
   {
      sprintf(logmsg, "Unable to find %s object for %s", 
         BODY_KEYWD, rec->file_name) ;
      pps_logMsg(ProgName, PPS_ERROR, logmsg);
      return(ERROR) ;
   }

   /*-------------------------------------------------*/
   /* extract fields inside the BODY aggregation      */
   /*-------------------------------------------------*/
   /*-------------------------------------------------*/
   /* extract order and item id first                 */
   /*-------------------------------------------------*/
   if ((rc=PPS_extract_param_value(object, &(rec->order_id),
                                    ORDER_ID_KEYWD, 0)) != ER_NO_ERROR)
   {
       sprintf(logmsg,
                   "Keyword[%s] for IMS Scan order: %s",
                   ORDER_ID_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR);
   }
   if ((rc=PPS_extract_param_value(object, &(rec->item_id),
                                    ITEM_ID_KEYWD, 0)) != ER_NO_ERROR)
   {
       sprintf(logmsg,
                   "Keyword[%s] for IMS Scan order[%d]: %s",
                   ITEM_ID_KEYWD, rec->order_id, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR);
   }
   /*-------------------------------------------------*/
   /* next: do the ones don't need special processing */
   /*-------------------------------------------------*/
   for (k=0; k < ElementNumber(ScanBodyExtractTable); k++)
   {
       EXTRACT_SCAN_PARAMS_CHECK_ERROR(obj,
                              (char*)rec + ScanBodyExtractTable[k].offset,
                              ScanBodyExtractTable[k].keyword,
                              ScanBodyExtractTable[k].length,
                              rec->order_id, rec->item_id);
   }

   /*---------------------------------*/
   /* SOURCE_MEDIA object is optional */
   /*---------------------------------*/
   if ((object = FindObject(top, SOURCE_MEDIA_KEYWD, NULL)) == NULL)
   {
      sprintf(logmsg, "Unable to find %s object for %s", 
                         SOURCE_MEDIA_KEYWD, rec->file_name) ;
      pps_logMsg(ProgName, PPS_DEBUG, logmsg);
      return(OK) ;
   }

   EXTRACT_SCAN_PARAMS_CHECK_ERROR(obj, rec->media_type,
         MEDIA_TYPE_KEYWD, MEDIA_TYPE_STRLEN, rec->order_id, rec->item_id);
   EXTRACT_SCAN_PARAMS_CHECK_ERROR(obj, rec->media_id,
         MEDIA_ID_KEYWD, MEDIA_ID_STRLEN, rec->order_id, rec->item_id);
 
   /*---------------------------------------------------------*/
   /* if media_type = DISK, need media_location               */ 
   /*---------------------------------------------------------*/
   if (strcmp(rec->media_type, DISK_KEYWD) == 0)
   {
       EXTRACT_SCAN_PARAMS_CHECK_ERROR(obj, rec->media_location,
                         MEDIA_LOCATION_KEYWD, MEDIA_LOCATION_STRLEN,
                         rec->order_id, rec->item_id);
   }
   /*---------------------------------------------------------*/
   /* for all other media types,    don't need media_location */ 
   /* if media_location doesn't exist, don't complain         */
   /*---------------------------------------------------------*/
   else
   {
       rc = PPS_extract_param_value(object, rec->media_location,
                         MEDIA_LOCATION_KEYWD, MEDIA_LOCATION_STRLEN);
       if (rc != ER_NO_ERROR && rc != ER_KEYWORD_NOT_FOUND)
       {
           sprintf(logmsg,
                    "Keyword[%s] for IMS Scan Order/Item[%d/%d]: %s",
                    ITEM_ID_KEYWD, rec->order_id,
                    rec->item_id, pps_err_msgs[rc]);
           pps_logMsg(ProgName, PPS_ERROR, logmsg);
           return(ERROR);
	}
   }
 
   EXTRACT_SCAN_PARAMS_CHECK_ERROR(obj, &(rec->start_address),
              START_ADDRESS_KEYWD, 0, rec->order_id, rec->item_id);
   EXTRACT_SCAN_PARAMS_CHECK_ERROR(obj, &(rec->end_address),
              END_ADDRESS_KEYWD, 0, rec->order_id, rec->item_id);

   EXTRACT_SCAN_PARAMS_CHECK_ERROR(obj, &odl_date, START_TIME_KEYWD, 0,
              rec->order_id, rec->item_id);
   /* save the start time in the record */
   convert_ODLdate_to_timerec (&odl_date , &(rec->start_time)) ;

   EXTRACT_SCAN_PARAMS_CHECK_ERROR(obj, &odl_date, END_TIME_KEYWD, 0,
              rec->order_id, rec->item_id);
   /* save the end time in the record */
   convert_ODLdate_to_timerec (&odl_date , &(rec->end_time)) ;

   EXTRACT_SCAN_PARAMS_CHECK_ERROR(obj, rec->recorder_id,
          RECORDER_ID_KEYWD, RECORDER_ID_STRLEN, rec->order_id, rec->item_id);
   EXTRACT_SCAN_PARAMS_CHECK_ERROR(obj, rec->station_id,
          STATION_ID_KEYWD, STATION_ID_STRLEN, rec->order_id, rec->item_id);
   EXTRACT_SCAN_PARAMS_CHECK_ERROR(obj, rec->data_direction,
      DATADIRECTION_KEYWD, DATA_DIRECTION_STRLEN, rec->order_id, rec->item_id);

   return(OK) ;

} /* extract_IMS_ScanReq_Record */

#ifdef __STDC__
int
extract_IMS_CancelReq_Record(IMS_CancelReq_Record *rec, AGGREGATE top)
#else
int
extract_IMS_CancelReq_Record(rec, top)
   IMS_CancelReq_Record *rec ;
   AGGREGATE top ;
#endif
{
   AGGREGATE object ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
   int  rc;          /* return code */

   if (top == NULL)
   {
      pps_logMsg(ProgName, PPS_DEBUG, "ODL top node is NULL");
      return(ERROR) ;
   }

   if ((object = FindObject(top, BODY_KEYWD, NULL)) == NULL)
   {
      sprintf(logmsg, "Unable to find %s object for %s", 
         BODY_KEYWD, rec->file_name) ;
      pps_logMsg(ProgName, PPS_ERROR, logmsg);
      return(ERROR) ;
   }

   /* extract fields inside the BODY aggregation */

   if ((rc=PPS_extract_param_value(object, &(rec->order_id),
                                      ORDER_ID_KEYWD, 0)) != ER_NO_ERROR)
   {
       sprintf(logmsg,
                     "Keyword[%s] for IMS Cancel request: %s",
                     ORDER_ID_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR);
   }

   if ((rc=PPS_extract_param_value(object, &(rec->item_id),
                                       ITEM_ID_KEYWD, 0)) != ER_NO_ERROR)
   {
       sprintf(logmsg,
                     "Keyword[%s] for IMS Cancel on Order[%d]: %s",
                     ITEM_ID_KEYWD, rec->order_id, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR);
   }

   if ((rc=PPS_extract_param_value(object, rec->request_type,
                   REQUEST_TYPE_KEYWD, REQUEST_TYPE_STRLEN)) != ER_NO_ERROR)
   {
       sprintf(logmsg,
              "Keyword[%s] for IMS Cancel order/item[%d/%d]: %s",
              REQUEST_TYPE_KEYWD, rec->order_id,
              rec->item_id, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR);
   }

   return(OK) ;

} /* extract_IMS_CancelReq_Record */


#ifdef __STDC__
int
extract_IMS_SVecAvail_Record(IMS_SVecAvail_Record *rec, AGGREGATE top)
#else
int
extract_IMS_SVecAvail_Record(rec, top)
   IMS_SVecAvail_Record *rec ;
   AGGREGATE top ;
#endif
{
   AGGREGATE object ;
   char logmsg[MAX_SYSLOG_MSGLEN+1] ;
   struct ODLDate 	odl_date;
   int                  rc;     /* return code */

   if (top == NULL)
   {
      pps_logMsg(ProgName, PPS_DEBUG, "ODL top node is NULL");
      return(ERROR) ;
   }

   if ((object = FindObject(top, BODY_KEYWD, NULL)) == NULL)
   {
      sprintf(logmsg, "Unable to find %s object for %s",
                            BODY_KEYWD, rec->file_name);
      pps_logMsg(ProgName, PPS_ERROR, logmsg);
      return(ERROR) ;
   }

   /* extract fields inside the BODY aggregation */

   if ((rc=PPS_extract_param_value(object, rec->platform,
                           PLATFORM_KEYWD, PLATFORM_STRLEN)) != ER_NO_ERROR)
   {
       sprintf(logmsg,
              "Keyword[%s] for IMS State Vector Available: %s",
              PLATFORM_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR);
   }

   if ((rc=PPS_extract_param_value(object, &(rec->start_rev),
                                START_REV_KEYWD, 0)) != ER_NO_ERROR)
   {
       sprintf(logmsg,
              "Keyword[%s] for IMS State Vector Available: %s",
              START_REV_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR);
   }

   if ((rc=PPS_extract_param_value(object, &(rec->end_rev),
                        END_REV_KEYWD, 0)) != ER_NO_ERROR)
   {
       sprintf(logmsg,
              "Keyword[%s] for IMS State Vector Available: %s",
              END_REV_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR);
   }

   if ((rc=PPS_extract_param_value(object, &odl_date,
                        START_TIME_KEYWD, 0)) != ER_NO_ERROR)
   {
       sprintf(logmsg,
              "Keyword[%s] for IMS State Vector Available: %s",
              START_TIME_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR);
   }

   /* save the start time in the record */
   convert_ODLdate_to_timerec (&odl_date , &(rec->start_time)) ;

   if ((rc=PPS_extract_param_value(object, &odl_date,
                        END_TIME_KEYWD, 0)) != ER_NO_ERROR)
   {
       sprintf(logmsg,
              "Keyword[%s] for IMS State Vector Available: %s",
              END_TIME_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR);
   }

   /* save the end time in the record */
   convert_ODLdate_to_timerec (&odl_date , &(rec->end_time)) ;

   if ((rc=PPS_extract_param_value(object, rec->type,
             STATE_VECTOR_PRECISION_KEYWD, SVEC_TYPE_STRLEN)) != ER_NO_ERROR)
   {
       sprintf(logmsg,
              "Keyword[%s] for IMS State Vector Available: %s",
              STATE_VECTOR_PRECISION_KEYWD, pps_err_msgs[rc]);
       pps_logMsg(ProgName, PPS_ERROR, logmsg);
       return(ERROR);
   }


   return(OK) ;

} /* extract_IMS_SVecAvail_Record */
