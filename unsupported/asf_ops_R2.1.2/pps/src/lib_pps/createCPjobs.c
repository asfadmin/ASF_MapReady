/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*==============================================================================
Filename:	createCPjobs.c

Description:	Contains the functions for building the CP job ODL objects

External Functions:
	
Static Functions:
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:

==============================================================================*/

static char SccsFileId[] = "@(#)createCPjobs.c	1.8    04 Jun 1996";

#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "odldef.h"
#include "odlinter.h"
#include "ODLcommonhdr.h"
#include "PPSdefs.h"
#include "PPSextern.h"

#ifdef __STDC__
void create_ODL_TCE_body(Time_Correlation *, AGGREGATE) ;
void create_ODL_SVData_body(State_Vector *, AGGREGATE) ;
void create_ODL_SVmeta_body(State_Vector *, AGGREGATE) ;
void create_ODL_GHA_body(GHA_Correction *, AGGREGATE) ;
void create_ODL_CP_Framejob_body(IMS_L1PReq_Record *, AGGREGATE) ;
void create_ODL_CP_Framejob(IMS_L1PReq_Record *, AGGREGATE *) ;
void create_CP_Framejob_buf(IMS_L1PReq_Record *, char *, int*) ;
void create_ODL_CP_Scanjob_body(IMS_ScanReq_Record *, AGGREGATE) ;
void create_ODL_CP_Scanjob(IMS_ScanReq_Record *, AGGREGATE *) ;
void create_CP_Scanjob_buf(IMS_ScanReq_Record *, char *, int*) ;
#else
void create_ODL_TCE_body() ;
void create_ODL_SVData_body() ;
void create_ODL_SVmeta_body() ;
void create_ODL_GHA_body() ;
void create_ODL_CP_Framejob_body() ;
void create_ODL_CP_Framejob() ;
void create_CP_Framejob_buf() ;
void create_ODL_CP_Scanjob_body() ;
void create_ODL_CP_Scanjob() ;
void create_CP_Scanjob_buf() ;
#endif



/*==============================================================================
Function:	void create_ODL_TCE_body(Time_Correlation *record, AGGREGATE parent)
Description:	Create ODL object for TCE (Time Correlation Elements) info
Parameters:	record containing TCE info
Returns:	None	
Creator:	Norbert Piega
Creation Date:	Wed Nov 15 12:51:35 PST 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void create_ODL_TCE_body(Time_Correlation *record, AGGREGATE parent)
#else
void create_ODL_TCE_body(record, parent)
   Time_Correlation *record ;
   AGGREGATE parent ;
#endif
{
  AGGREGATE 	tcebody ;
  PARAMETER  	curr_param ;
  VALUE_DATA 	curr_value ;
  char tempstr[MAXLINE] ;

 
  /* create the TIME_CORRELATION aggregate, and connect it to parent */

  tcebody = NewAggregate(parent, KA_OBJECT, TIME_CORRELATION_KEYWD, NULL) ;

  if (record->time.time_string[0])
  {
     curr_param = NewParameter(tcebody, KP_ATTRIBUTE, TIME_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol (record->time.time_string, 
			   strlen(record->time.time_string), 1) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->rev != -1)
  {
     curr_param = NewParameter(tcebody, KP_ATTRIBUTE, REVOLUTION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->rev) ;
     curr_value = ODLConvertInteger (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->sat_time != -1)
  {
     unsigned int sat_time = (unsigned int) record->sat_time; 
     curr_param = NewParameter(tcebody, KP_ATTRIBUTE, PLATFORM_TIME_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%u", sat_time);
     curr_value = ODLConvertSymbol (tempstr, strlen(tempstr),1);
     NewValue(curr_param, &curr_value) ;
  }

  if (record->clock_cycle != -1)
  {
     curr_param = NewParameter(tcebody, KP_ATTRIBUTE, CLOCK_CYCLE_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->clock_cycle) ;
     curr_value = ODLConvertInteger (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

} /* create_ODL_TCE_body */




/*==============================================================================
Function:	void create_ODL_SVData_body(State_Vector *record, AGGREGATE parent)
Description:	Create ODL object for state vector data info
Parameters:	record containing state vector data info
Returns:	None	
Creator:	Norbert Piega
Creation Date:	Wed Nov 15 12:51:35 PST 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void create_ODL_SVData_body(State_Vector *record, AGGREGATE parent)
#else
void create_ODL_SVData_body(record, parent)
   State_Vector *record ;
   AGGREGATE parent ;
#endif
{
  AGGREGATE 	svdatabody ;
  PARAMETER  	curr_param ;
  VALUE_DATA 	curr_value ;
  char tempstr[MAXLINE] ;
 
  /* create the STATE VECTOR DATA aggregate, and connect it to parent */

  svdatabody = NewAggregate(parent, KA_OBJECT, STATE_VECTOR_DATA_KEYWD, NULL) ;

  if (record->time.time_string[0])
  {
     curr_param = NewParameter(svdatabody, KP_ATTRIBUTE, TIME_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol (record->time.time_string, 
			   strlen(record->time.time_string), 1) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->x_pos != 0.0)
  {
     curr_param = NewParameter(svdatabody, KP_ATTRIBUTE, X_POSITION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%f", record->x_pos) ;
     curr_value = ODLConvertReal (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->y_pos != 0.0)
  {
     curr_param = NewParameter(svdatabody, KP_ATTRIBUTE, Y_POSITION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%f", record->y_pos) ;
     curr_value = ODLConvertReal (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->z_pos != 0.0)
  {
     curr_param = NewParameter(svdatabody, KP_ATTRIBUTE, Z_POSITION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%f", record->z_pos) ;
     curr_value = ODLConvertReal (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->x_vel != 0.0)
  {
     curr_param = NewParameter(svdatabody, KP_ATTRIBUTE, X_VELOCITY_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%f", record->x_vel) ;
     curr_value = ODLConvertReal (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->y_vel != 0.0)
  {
     curr_param = NewParameter(svdatabody, KP_ATTRIBUTE, Y_VELOCITY_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%f", record->y_vel) ;
     curr_value = ODLConvertReal (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->z_vel != 0.0)
  {
     curr_param = NewParameter(svdatabody, KP_ATTRIBUTE, Z_VELOCITY_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%f", record->z_vel) ;
     curr_value = ODLConvertReal (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

} /* create_ODL_SVData_body */




/*==============================================================================
Function:	void create_ODL_SVmeta_body(State_Vector *record, AGGREGATE parent)
Description:	Create ODL object for state vector metadata info
Parameters:	record containing state vector metadata info
Returns:	None	
Creator:	Norbert Piega
Creation Date:	Wed Nov 15 12:51:35 PST 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void create_ODL_SVmeta_body(State_Vector *record, AGGREGATE parent)
#else
void create_ODL_SVmeta_body(record, parent)
   State_Vector *record ;
   AGGREGATE parent ;
#endif
{
  AGGREGATE 	svmetabody ;
  PARAMETER  	curr_param ;
  VALUE_DATA 	curr_value ;
  char tempstr[MAXLINE] ;
 
  /* create the STATE VECTOR METADATA aggregate, and connect it to parent */

  svmetabody = 
     NewAggregate(parent, KA_OBJECT, STATE_VECTOR_METADATA_KEYWD, NULL) ;

  if (record->platform[0])
  {
     curr_param = NewParameter(svmetabody, KP_ATTRIBUTE, PLATFORM_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol (record->platform, 
			   strlen(record->platform), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->precision[0])
  {
     curr_param = 
	NewParameter(svmetabody, KP_ATTRIBUTE, STATE_VECTOR_PRECISION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol (record->precision, 
			   strlen(record->precision), 2) ;
     NewValue(curr_param, &curr_value) ;
  }
     curr_param = 
	NewParameter(svmetabody, KP_ATTRIBUTE, COORDINATE_SYSTEM_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol (record->coord_sys, 
			   strlen(record->coord_sys), 2) ;
     NewValue(curr_param, &curr_value) ;

  if (record->rev != -1)
  {
     curr_param = NewParameter(svmetabody, KP_ATTRIBUTE, REVOLUTION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->rev) ;
     curr_value = ODLConvertInteger (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

} /* create_ODL_SVmeta_body */




/*==============================================================================
Function:	void create_ODL_GHA_body(GHA_Correction *record, AGGREGATE parent)
Description:	Create ODL object for GHA info
Parameters:	record containing GHA info
Returns:	None	
Creator:	Norbert Piega
Creation Date:	Wed Nov 15 12:51:35 PST 1995
Notes:		
==============================================================================*/
#ifdef __STDC__
void create_ODL_GHA_body(GHA_Correction *record, AGGREGATE parent)
#else
void create_ODL_GHA_body(record, parent)
   GHA_Correction *record ;
   AGGREGATE parent ;
#endif
{
  AGGREGATE 	ghabody ;
  PARAMETER  	curr_param ;
  VALUE_DATA 	curr_value ;
  char tempstr[MAXLINE] ;
 
  /* create the GHA CORRECTION aggregate, and connect it to parent */

  ghabody = NewAggregate(parent, KA_OBJECT, GHA_CORRECTION_KEYWD, NULL) ;

  if (record->time.time_string[0])
  {
     curr_param = NewParameter(ghabody, KP_ATTRIBUTE, TIME_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol (record->time.time_string, 
			   strlen(record->time.time_string), 1) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->angle != 0.0)
  {
     curr_param = NewParameter(ghabody, KP_ATTRIBUTE, ANGLE_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%f", record->angle) ;
     curr_value = ODLConvertReal (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

} /* create_ODL_GHA_body */



/*==============================================================================
Function:	create_ODL_CP_Framejob_body
Description:	Creates the Frame job ODL object to be sent by PPS to CP
Parameters:     IMS_Order_Status *record - C structure containing the values
                                   of the fields in the order status message
		AGGREGATE parent - the parent aggregate, 
Returns:	
Creator:        Nadia Adhami   (Nadia.Adhami@jpl.nasa.ogv)
Creation Date:	8/15/95
Notes:		
==============================================================================*/
#ifdef __STDC__
void create_ODL_CP_Framejob_body(IMS_L1PReq_Record *record, AGGREGATE parent)
#else
void create_ODL_CP_Framejob_body(record, parent)
   IMS_L1PReq_Record *record ;
   AGGREGATE parent ;
#endif
{
  AGGREGATE 	body ;
  AGGREGATE 	child ;
  PARAMETER  	curr_param ;
  VALUE_DATA 	curr_value ;
  char tempstr[MAXLINE] ;
 
  /* create the CATALOG_METADATA aggregate, and connect it to parent */

  body = NewAggregate(parent, KA_OBJECT, BODY_KEYWD, NULL) ;

  if (record->job_id != -1)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, JOB_ID_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->job_id) ;
     curr_value = ODLConvertInteger (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->priority[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, PRIORITY_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol(record->priority, strlen(record->priority), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->insert_top_flag[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, INSERT_TOP_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol (record->insert_top_flag, 
			  strlen(record->insert_top_flag), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->platform[0])
    {
      curr_param = NewParameter(body, KP_ATTRIBUTE, PLATFORM_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = 
	 ODLConvertSymbol(record->platform, strlen(record->platform), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->sensor[0])
    {
      curr_param = NewParameter(body, KP_ATTRIBUTE, SENSOR_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = 
	 ODLConvertSymbol(record->sensor, strlen(record->sensor), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->rev != -1)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, REVOLUTION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->rev) ;
     curr_value = ODLConvertInteger (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->datatake_seq != -1)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, SEQUENCE_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->datatake_seq) ;
     curr_value = ODLConvertInteger (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->activity_id[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, ACTIVITY_ID_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
      curr_value = 
	 ODLConvertSymbol (record->activity_id, strlen(record->activity_id), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->media_id[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, MEDIA_ID_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value =
        ODLConvertSymbol(record->media_id,
                         strlen(record->media_id), 2) ;
     NewValue(curr_param, &curr_value) ;
  }
 
  if (record->media_type[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, MEDIA_TYPE_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value =
        ODLConvertSymbol(record->media_type,
                         strlen(record->media_type), 2) ;
     NewValue(curr_param, &curr_value) ;
  }
 
  if (record->media_location[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, MEDIA_LOCATION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value =
        ODLConvertSymbol(record->media_location,
                         strlen(record->media_location), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->data_direction[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, DATADIRECTION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value =
        ODLConvertSymbol(record->data_direction,
                         strlen(record->data_direction), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->quicklook_flag[0])  
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, QUICKLOOK_FLAG_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value =
        ODLConvertSymbol(record->quicklook_flag,
                         strlen(record->quicklook_flag), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

   if (record->compensation_flag[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, COMPENSATION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value =
        ODLConvertSymbol(record->compensation_flag,
                         strlen(record->compensation_flag), 2) ;
     NewValue(curr_param, &curr_value) ;
  }
 
  if (record->site_name[0])
    {
      curr_param = NewParameter(body, KP_ATTRIBUTE, SITE_NAME_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = 
	 ODLConvertSymbol(record->site_name, 
	                  strlen(record->site_name), 2) ;
      NewValue(curr_param, &curr_value) ;
    }

  if (record->scan_results_file[0])
    {
      curr_param = NewParameter(body, KP_ATTRIBUTE, SCAN_RESULTS_FILE_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertString(record->scan_results_file, 
			  strlen(record->scan_results_file));
      NewValue(curr_param, &curr_value) ;
    }

  if (record->cal_params_file[0])
    {
      curr_param = NewParameter(body, KP_ATTRIBUTE, PRIM_CALPARMS_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      curr_value = ODLConvertString(record->cal_params_file, 
		          strlen(record->cal_params_file));
      NewValue(curr_param, &curr_value) ;
    }

  /* CP requires that we do not pass blank if cal_params_file2 is not
     available - so we need special processing to satisfy this requirement.
     (Sybase interprets null string as one blank for "insert" )*/
  if (record->cal_params_file2[0])
  {
      curr_param = NewParameter(body, KP_ATTRIBUTE, SEC_CALPARMS_KEYWD) ;
      curr_param->value_kind = KV_SCALAR ;
      if (strlen(record->cal_params_file2) == 1 &&
                 record->cal_params_file2[0] == ' ')
      {
	 record->cal_params_file2[0] = '\0'; 
      }
      curr_value = ODLConvertString(record->cal_params_file2,
                          strlen(record->cal_params_file2));
      NewValue(curr_param, &curr_value) ;
  }

  if (record->frame_mode[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, FRAME_MODE_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value =
        ODLConvertSymbol (record->frame_mode,
                          strlen(record->frame_mode), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->mode[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, MODE_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol(record->mode, strlen(record->mode), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->product_type[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, PRODUCT_TYPE_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol(record->product_type, 
			 strlen(record->product_type), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->pixel_spacing != -1.0)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, PIXEL_SPACING_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%f", record->pixel_spacing) ;
     curr_value = ODLConvertReal(tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->frame_id != -1)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, FRAME_ID_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->frame_id) ;
     curr_value = ODLConvertInteger (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  /* print subframe_id only for COMPLEX product_types */
  if (record->subframe_id != -1 && (!strcmp(record->product_type,COMPLEX_KEYWD)))
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, SUBFRAME_ID_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->subframe_id) ;
     curr_value = ODLConvertInteger (tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->output_format[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, OUTPUT_FORMAT_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol (record->output_format, 
			  strlen(record->output_format), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->projection[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, PROJECTION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol(record->projection, 
			 strlen(record->projection), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  /* print utm_zone for complex prodects only */
  if (! strcmp(record->projection, UTM_KEYWD))
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, UTM_ZONE_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->utm_zone) ;
     curr_value = ODLConvertInteger(tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }
  
  if (record->processing_gain != -1)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, PROCESSING_GAIN_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->processing_gain) ;
     curr_value = ODLConvertInteger(tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }
  
  if (record->avg_terrain_ht != -1.0)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, AVG_TERRAIN_HT_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%f", record->avg_terrain_ht) ;
     curr_value = ODLConvertReal(tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->deskew[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, DESKEW_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol(record->deskew, 
			 strlen(record->deskew), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->terrain_correction[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, TERRAIN_CORRECTION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol(record->terrain_correction, 
			 strlen(record->terrain_correction), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->ps_reference_lat != -1.0)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, PS_REFERENCE_LAT_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%f", record->ps_reference_lat) ;
     curr_value = ODLConvertReal(tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->ps_reference_lon != -1.0)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, PS_REFERENCE_LON_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%f", record->ps_reference_lon) ;
     curr_value = ODLConvertReal(tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->lambert_latitude_n != -1.0)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, LAMBERT_LATITUDE_N_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%f", record->lambert_latitude_n) ;
     curr_value = ODLConvertReal(tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->lambert_latitude_s != -1.0)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, LAMBERT_LATITUDE_S_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%f", record->lambert_latitude_s) ;
     curr_value = ODLConvertReal(tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  create_ODL_GHA_body(&(record->gha), body) ;

  child = NewAggregate(body, KA_OBJECT, STATE_VECTOR_RECORD_KEYWD, NULL) ;
  create_ODL_SVData_body(&(record->sv), child) ;
  create_ODL_SVmeta_body(&(record->sv), child) ;

  /* TCE not needed for R1 */
  if (strcmp(record->platform, R1_SATNAME) != 0) 
     create_ODL_TCE_body(&record->tc, body) ;

} /* create_ODL_CP_Framejob_body */





/*==============================================================================
Function:       create_ODL_CP_Framejob
Description:    create ODL tree containing CP Frame job info
Parameters:     status record, ODL tree basenode
Returns:        void
Creator:        Norbert Piega
Creation Date:  11/15/1995
Notes:
==============================================================================*/
#ifdef __STDC__
void    create_ODL_CP_Framejob(
        IMS_L1PReq_Record       *record,
        AGGREGATE               *base_node)
#else
void    create_ODL_CP_Framejob(record, base_node)
        IMS_L1PReq_Record        *record;
        AGGREGATE               *base_node;
#endif
{
  AGGREGATE             root ;
  AGGREGATE             parent ;
  ODL_Common_Header     *commonrecord ;
  ODL_Common_Header     commonhdrrec ;
 
  commonrecord  = &commonhdrrec ;
 
  get_current_time(commonrecord->file_creation_time) ;
  strcpy(commonrecord->file_source, PPS_KEYWD) ;
  strcpy(commonrecord->file_dest,   CP_KEYWD) ;
  strcpy(commonrecord->file_type,   FRAME_JOB_KEYWD) ;
  strcpy(commonrecord->number_of_records, "1") ;
 
  root   = NewAggregate(NULL, KA_OBJECT, "root", NULL) ;
  parent = NewAggregate(root, KA_OBJECT, FRAME_JOB_KEYWD, NULL) ;
 
  create_ODL_common_hdr(commonrecord, parent) ;
  create_ODL_CP_Framejob_body(record, parent) ;
 
  *base_node = root;
 
} /* create_ODL_CP_Framejob */



/*==============================================================================
Function:	create_CP_Framejob_buf
Description:	create buffer containing requested CP job
Parameters: 	job info structure, buffer, size of buffer
Returns:	void
Creator:	Norbert Piega
Creation Date:	Mon Nov  6 14:18:53 PST 1995
Notes:		Had to create this function since a file
		can not contain both ODL and IMS header files.
		This function hides the ODL definition from the calling routine.
==============================================================================*/
#ifdef __STDC__
void  	create_CP_Framejob_buf(
	IMS_L1PReq_Record       *Frame_order,
	char			*buffer,
	int			*sizeof_buffer)
#else
void  	create_CP_Framejob_buf(Frame_order, buffer, sizeof_buffer)
	IMS_L1PReq_Record       *Frame_order;
	char			*buffer;
	int			*sizeof_buffer;
#endif
{
        AGGREGATE       root;
 
        /* create ODL format message */
        create_ODL_CP_Framejob(Frame_order, &root);

        /* write ODL message tree into a buffer */
        WriteLabelBuf (buffer, root, *sizeof_buffer);

	/* free the ODL message tree */
	(void) RemoveAggregate(root);

} /* create_CP_Framejob_buf */





/*==============================================================================
Function:	create_ODL_CP_Scanjob_body
Description:	Creates the body of the scan job ODL object to be sent to CP
Parameters:     IMS_ScanReq_Record *record - C structure containing the values
                                   of the fields in the scan request
		AGGREGATE parent - the parent aggregate, 
Returns:	
Creator:        Norbert Piega
Creation Date:	11/15/95
Notes:		
==============================================================================*/
#ifdef __STDC__
void create_ODL_CP_Scanjob_body(IMS_ScanReq_Record *record, AGGREGATE parent)
#else
void create_ODL_CP_Scanjob_body(record, parent)
   IMS_ScanReq_Record *record ;
   AGGREGATE parent ;
#endif
{
  AGGREGATE 	body, child ;
  PARAMETER  	curr_param ;
  VALUE_DATA 	curr_value ;
  char tempstr[MAXLINE] ;
 
  /* create the SCAN_REQUEST aggregate, and connect it to parent */

  body = NewAggregate(parent, KA_OBJECT, BODY_KEYWD, NULL) ;

  if (record->job_id != -1)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, JOB_ID_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->job_id) ;
     curr_value = ODLConvertInteger(tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->priority[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, PRIORITY_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol(record->priority, strlen(record->priority), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->insert_top_flag[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, INSERT_TOP_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol(record->insert_top_flag, 
			 strlen(record->insert_top_flag), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->platform[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, PLATFORM_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol(record->platform, strlen(record->platform), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->sensor[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, SENSOR_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol(record->sensor, strlen(record->sensor), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->rev != -1)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, REVOLUTION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->rev) ;
     curr_value = ODLConvertInteger(tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->datatake_seq != -1)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, SEQUENCE_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->datatake_seq) ;
     curr_value = ODLConvertInteger(tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->activity_id[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, ACTIVITY_ID_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = ODLConvertSymbol(record->activity_id, 
		         strlen(record->activity_id), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->media_id[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, MEDIA_ID_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value =
        ODLConvertSymbol(record->media_id,
                         strlen(record->media_id), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->media_type[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, MEDIA_TYPE_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol(record->media_type, 
		         strlen(record->media_type), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->media_location[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, MEDIA_LOCATION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol(record->media_location, 
                         strlen(record->media_location), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->data_direction[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, DATADIRECTION_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value =
        ODLConvertSymbol(record->data_direction,
                         strlen(record->data_direction), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->start_address != -1)
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, START_ADDRESS_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->start_address) ;
     curr_value = ODLConvertInteger(tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->end_address != -1)  
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, END_ADDRESS_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     sprintf(tempstr, "%d", record->end_address) ;
     curr_value = ODLConvertInteger(tempstr, strlen(tempstr)) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->start_time.time_string[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, START_TIME_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value =
         ODLConvertSymbol(record->start_time.time_string,
                          strlen(record->start_time.time_string), 1) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->end_time.time_string[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, END_TIME_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value =
         ODLConvertSymbol(record->end_time.time_string,
                          strlen(record->end_time.time_string), 1) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->site_name[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, SITE_NAME_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	 ODLConvertSymbol(record->site_name, 
	                  strlen(record->site_name), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->recorder_id[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, RECORDER_ID_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	 ODLConvertSymbol(record->recorder_id, 
			  strlen(record->recorder_id), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->station_id[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, STATION_ID_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol(record->station_id, 
		         strlen(record->station_id), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->mode[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, MODE_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol(record->mode, 
		         strlen(record->mode), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  if (record->frame_mode[0])
  {
     curr_param = NewParameter(body, KP_ATTRIBUTE, FRAME_MODE_KEYWD) ;
     curr_param->value_kind = KV_SCALAR ;
     curr_value = 
	ODLConvertSymbol (record->frame_mode, 
		          strlen(record->frame_mode), 2) ;
     NewValue(curr_param, &curr_value) ;
  }

  create_ODL_GHA_body(&record->gha, body) ;

  child = NewAggregate(body, KA_OBJECT, STATE_VECTOR_RECORD_KEYWD, NULL) ;
  create_ODL_SVData_body(&record->sv, child) ;
  create_ODL_SVmeta_body(&record->sv, child) ;

  /* TCE not needed for R1 */
  if (strcmp(record->platform, R1_SATNAME) != 0)
     create_ODL_TCE_body(&record->tc, body) ;

} /* create_ODL_CP_Scanjob_body */





/*==============================================================================
Function:       create_ODL_CP_Scanjob
Description:    create ODL tree containing CP Scan job info
Parameters:     status record, ODL tree basenode
Returns:        void
Creator:        Norbert Piega
Creation Date:  11/15/1995
Notes:
==============================================================================*/
#ifdef __STDC__
void    create_ODL_CP_Scanjob(
        IMS_ScanReq_Record       *record,
        AGGREGATE               *base_node)
#else
void    create_ODL_CP_Scanjob(record, base_node)
        IMS_ScanReq_Record        *record;
        AGGREGATE               *base_node;
#endif
{
  AGGREGATE             root ;
  AGGREGATE             parent ;
  ODL_Common_Header     *commonrecord ;
  ODL_Common_Header     commonhdrrec ;
 
  commonrecord  = &commonhdrrec ;
 
  get_current_time(commonrecord->file_creation_time) ;
  strcpy(commonrecord->file_source, PPS_KEYWD) ;
  strcpy(commonrecord->file_dest,   CP_KEYWD) ;
  strcpy(commonrecord->file_type,   SCAN_JOB_KEYWD) ;
  strcpy(commonrecord->number_of_records, "1") ;
 
  root   = NewAggregate(NULL, KA_OBJECT, "root", NULL) ;
  parent = NewAggregate(root, KA_OBJECT, SCAN_JOB_KEYWD, NULL) ;
 
  create_ODL_common_hdr(commonrecord, parent) ;
  create_ODL_CP_Scanjob_body(record, parent) ;
 
  *base_node = root;
 
} /* create_ODL_CP_Scanjob */





/*==============================================================================
Function:	create_CP_Scanjob_buf
Description:	create buffer containing requested CP job
Parameters: 	job info structure, buffer, size of buffer
Returns:	void
Creator:	Norbert Piega
Creation Date:	Mon Nov  6 14:19:19 PST 1995
Notes:		Had to create this function since a file
		can not contain both ODL and IMS header files.
		This function hides the ODL definition from the calling routine.
==============================================================================*/
#ifdef __STDC__
void  	create_CP_Scanjob_buf(
	IMS_ScanReq_Record	*Scan_order,
	char			*buffer,
	int			*sizeof_buffer)
#else
void  	create_CP_Scanjob_buf(Scan_order, buffer, sizeof_buffer)
	IMS_ScanReq_Record	*Scan_order;
	char			*buffer;
	int			*sizeof_buffer;
#endif
{
        AGGREGATE       	root;
 
        /* create ODL format message */
        create_ODL_CP_Scanjob(Scan_order, &root);

        /* write ODL message tree into a buffer */
        WriteLabelBuf (buffer, root, *sizeof_buffer);

	/* free the ODL message tree */
	(void) RemoveAggregate(root);

} /* create_CP_Scanjob_buf */


/* End of File */
