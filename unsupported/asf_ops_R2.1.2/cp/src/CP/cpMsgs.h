#define FLEXIBLE_PPS /* */
#define EXPECT_FRAME_MODE /* if FRAME_REQUEST should contain FRAME_MODE */

#ifndef _cpMsgs
#define _cpMsgs

static char sccsid_cpmsgs_h[] = "@(#)cpMsgs.h	1.19 96/12/18 09:45:01";


/*----------------------------------------------------------
 * NAME:
 *  cpMsgs.h
 *
 * DESCRIPTION:
 *
 *
 * NOTES:
 *
 *
 *---------------------------------------------------------*/


typedef struct msgTag {
   char *str;
   int *ptr;
} msgInfo;


static msgInfo sps_job_status[] = {
{ "SPS_JOB_STATUS.BODY.PLATFORM", (void *) &GLOBAL_state.dtm_satBuf } ,
{ "SPS_JOB_STATUS.BODY.SENSOR", (void *) &GLOBAL_state.sensorBuf } ,
{ NULL}
};  /* sps_job_status */


static msgInfo scan_request[] = {
{ "COMMON_HEADER.MSG_TYPE", (void *) &GLOBAL_state.rt_buf } , 
{ "BODY.JOB_ID", (void *) &GLOBAL_state.jobId } ,
{ "BODY.PRIORITY", (void *) &GLOBAL_state.priorityStr } ,
{ "BODY.INSERT_TOP", (void *) &GLOBAL_state.insertTop } ,
{ "BODY.PLATFORM", (void *) &GLOBAL_state.dtm_satBuf } ,
{ "BODY.SENSOR", (void *) &GLOBAL_state.sensorBuf } ,
{ "BODY.REVOLUTION", (void *) &GLOBAL_state.rev } ,
{ "BODY.SEQUENCE", (void *) &GLOBAL_state.seq } ,
{ "BODY.ACTIVITY_ID", (void *) &GLOBAL_state.activityBuf } ,
{ "BODY.MEDIA_ID", (void *) &GLOBAL_state.mediaId } ,
{ "BODY.MEDIA_TYPE", (void *) &GLOBAL_state.mediaType } ,
{ "BODY.MEDIA_LOCATION", (void *) &GLOBAL_state.tapeBuf } ,
{ "BODY.DATA_DIRECTION", (void *) &GLOBAL_state.dataDirection } ,
#ifdef E_BEAM
{ "BODY.EXPERIMENTAL_BEAM", (void *) &GLOBAL_state.experimentalBeam } ,
#endif
{ "BODY.START_ADDRESS", (void *) &GLOBAL_state.startAddr } ,
{ "BODY.END_ADDRESS", (void *) &GLOBAL_state.endAddr } ,
{ "BODY.START_TIME", (void *) &GLOBAL_state.startTime } ,
{ "BODY.END_TIME", (void *) &GLOBAL_state.endTime } ,
{ "BODY.RECORDER_ID", (void *) &GLOBAL_state.recorderBuf } ,
{ "BODY.STATION_ID", (void *) &GLOBAL_state.stationID } ,
{ "BODY.FRAME_MODE", (void *) &GLOBAL_state.frame_mode_buf } ,
{ "BODY.SITE_NAME", (void *) &GLOBAL_state.site_nameBuf } ,
{ "BODY.MODE", (void *) &GLOBAL_state.mode_buf } ,
{ "BODY.GHA_CORRECTION.TIME", (void *) &GLOBAL_state.gha_time_val } ,
{ "BODY.GHA_CORRECTION.ANGLE", (void *) &GLOBAL_state.gha_angle } ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.PLATFORM", 
       (void *) &GLOBAL_state.sv_satBuf } ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.STATE_VECTOR_PRECISION",
       (void *) &GLOBAL_state.sv_typeBuf } ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.COORDINATE_SYSTEM",
       (void *) &GLOBAL_state.sv_coordSysBuf } ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.REVOLUTION",
       (void *) &GLOBAL_state.sv_rev } ,

{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.TIME",
       (void *) &GLOBAL_state.sv_time_val } ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_POSITION",
       (void *) &GLOBAL_state.sv_x_pos } ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_POSITION",
       (void *) &GLOBAL_state.sv_y_pos } ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_POSITION",
       (void *) &GLOBAL_state.sv_z_pos } ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_VELOCITY",
       (void *) &GLOBAL_state.sv_x_vel } ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_VELOCITY",
       (void *) &GLOBAL_state.sv_y_vel } ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_VELOCITY",
       (void *) &GLOBAL_state.sv_z_vel } ,

#ifndef FLEXIBLE_PPS /* use request_flexible_tc */
{ "BODY.TIME_CORRELATION.REVOLUTION", (void *) &GLOBAL_state.time_corr_rev } ,
{ "BODY.TIME_CORRELATION.TIME", (void *) &GLOBAL_state.time_corr_time_val } ,
{ "BODY.TIME_CORRELATION.PLATFORM_TIME", (void *) &GLOBAL_state.sat_time } ,
{ "BODY.TIME_CORRELATION.CLOCK_CYCLE", (void *) &GLOBAL_state.clock_cycle } ,
#endif
{ NULL}
};  /* scan_request */

static msgInfo frame_request[] = {
{ "COMMON_HEADER.MSG_TYPE", (void *) &GLOBAL_state.rt_buf} , 
{ "BODY.JOB_ID", (void *) &GLOBAL_state.jobId} ,
{ "BODY.PRIORITY", (void *) &GLOBAL_state.priorityStr} ,
{ "BODY.INSERT_TOP", (void *) &GLOBAL_state.insertTop} ,
{ "BODY.PLATFORM", (void *) &GLOBAL_state.dtm_satBuf} ,
{ "BODY.SENSOR", (void *) &GLOBAL_state.sensorBuf} ,
{ "BODY.REVOLUTION", (void *) &GLOBAL_state.rev} ,
{ "BODY.SEQUENCE", (void *) &GLOBAL_state.seq} ,
{ "BODY.MEDIA_ID", (void *) &GLOBAL_state.mediaId} ,
{ "BODY.MEDIA_TYPE", (void *) &GLOBAL_state.mediaType} ,
{ "BODY.MEDIA_LOCATION", (void *) &GLOBAL_state.tapeBuf} ,
{ "BODY.DATA_DIRECTION", (void *) &GLOBAL_state.dataDirection } ,
#ifdef E_BEAM
{ "BODY.EXPERIMENTAL_BEAM", (void *) &GLOBAL_state.experimentalBeam } ,
#endif
{ "BODY.SITE_NAME", (void *) &GLOBAL_state.site_nameBuf} ,
#ifdef EXPECT_FRAME_MODE
{ "BODY.FRAME_MODE", (void *) &GLOBAL_state.frame_mode_buf } , 
#endif
{ "BODY.SCAN_RESULTS_FILE", (void *) &GLOBAL_state.scanResultsFile} ,
{ "BODY.CALPARMS_FILE_PRIMARY", (void *) &GLOBAL_state.calParamsFile} ,
{ "BODY.CALPARMS_FILE_SECONDARY", (void *) &GLOBAL_state.calParamsFile_2} ,
{ "BODY.COMPENSATION_FLAG", (void *) &GLOBAL_state.compensated} ,
{ "BODY.QUICKLOOK_FLAG", (void *) &GLOBAL_state.quicklook} ,
{ "BODY.MODE", (void *) &GLOBAL_state.mode_buf} ,
{ "BODY.GHA_CORRECTION.TIME", (void *) &GLOBAL_state.gha_time_val} ,
{ "BODY.GHA_CORRECTION.ANGLE", (void *) &GLOBAL_state.gha_angle} ,
#ifndef FLEXIBLE_PPS /*** moved to request_flexible_tc */
{ "BODY.TIME_CORRELATION.REVOLUTION", (void *) &GLOBAL_state.time_corr_rev} , 
{ "BODY.TIME_CORRELATION.TIME", (void *) &GLOBAL_state.time_corr_time_val} , 
{ "BODY.TIME_CORRELATION.PLATFORM_TIME", (void *) &GLOBAL_state.sat_time} , 
{ "BODY.TIME_CORRELATION.CLOCK_CYCLE", (void *) &GLOBAL_state.clock_cycle} , 
#endif
{ "BODY.PRODUCT_TYPE", (void *) &GLOBAL_state.productT_buf} ,
{ "BODY.PIXEL_SPACING", (void *) &GLOBAL_state.pixelSpacing} ,
{ "BODY.FRAME_ID", (void *) &GLOBAL_state.frameId} ,
#ifndef FLEXIBLE_PPS /* moved to frame_request_flexible_sub */
{ "BODY.SUBFRAME_ID", (void *) &GLOBAL_state.subframe_id} ,
#endif
{ "BODY.OUTPUT_FORMAT", (void *) &GLOBAL_state.output_format_buf} ,
{ "BODY.PROJECTION", (void *) &GLOBAL_state.projection_buf} ,
{ "BODY.PROCESSING_GAIN", (void *) &GLOBAL_state.processingGain} ,
{ "BODY.AVG_TERRAIN_HT", (void *) &GLOBAL_state.avgTerrainHt} ,
{ "BODY.PS_REFERENCE_LAT", (void *) &GLOBAL_state.standardLat} ,
{ "BODY.PS_REFERENCE_LON", (void *) &GLOBAL_state.standardLon} ,
#ifndef FLEXIBLE_PPS /* moved to frame_request_flexible_utm */
{ "BODY.UTM_ZONE", (void *) &GLOBAL_state.UTM_zone} ,
#endif
{ "BODY.DESKEW", (void *) &GLOBAL_state.skew_buf} ,
{ "BODY.TERRAIN_CORRECTION", (void *) &GLOBAL_state.terrain_correction_buf} ,
{ "BODY.LAMBERT_LATITUDE_N", (void *) &GLOBAL_state.lambert_lat_n} ,
{ "BODY.LAMBERT_LATITUDE_S", (void *) &GLOBAL_state.lambert_lat_s} ,

{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.PLATFORM" ,
       (void *) &GLOBAL_state.sv_satBuf} ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.STATE_VECTOR_PRECISION",
       (void *) &GLOBAL_state.sv_typeBuf} ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.COORDINATE_SYSTEM",
       (void *) &GLOBAL_state.sv_coordSysBuf} ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.REVOLUTION",
       (void *) &GLOBAL_state.sv_rev} ,

{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_POSITION",
       (void *) &GLOBAL_state.sv_x_pos} ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_POSITION",
       (void *) &GLOBAL_state.sv_y_pos} ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_POSITION",
       (void *) &GLOBAL_state.sv_z_pos} ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_VELOCITY",
       (void *) &GLOBAL_state.sv_x_vel} ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_VELOCITY",
       (void *) &GLOBAL_state.sv_y_vel} ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_VELOCITY",
       (void *) &GLOBAL_state.sv_z_vel} ,
{ "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.TIME",
       (void *) &GLOBAL_state.sv_time_val} ,

{ NULL}  };  /* frame_request */


#ifdef FLEXIBLE_PPS
static msgInfo frame_request_flexible_subframe[] = {
{ "BODY.SUBFRAME_ID", (void *) &GLOBAL_state.subframe_id } ,
{ NULL } }; 

static msgInfo frame_request_flexible_utm[] = {
{ "BODY.UTM_ZONE", (void *) &GLOBAL_state.UTM_zone } ,
{ NULL } };  

/* this one is needed for both scan and frame requests */
static msgInfo request_flexible_tc[] = {
{ "BODY.TIME_CORRELATION.REVOLUTION", (void *) &GLOBAL_state.time_corr_rev } ,
{ "BODY.TIME_CORRELATION.TIME", (void *) &GLOBAL_state.time_corr_time_val } ,
{ "BODY.TIME_CORRELATION.PLATFORM_TIME", (void *) &GLOBAL_state.sat_time } ,
{ "BODY.TIME_CORRELATION.CLOCK_CYCLE", (void *) &GLOBAL_state.clock_cycle } ,
{ NULL } };
#endif


static msgInfo sps_scan_request[] ={
{ "SPS_SCAN_REQUEST.BODY.JOB_ID", (void *) &GLOBAL_state.jobId} ,
{ "SPS_SCAN_REQUEST.BODY.REVOLUTION", (void *) &GLOBAL_state.rev} , 
{ "SPS_SCAN_REQUEST.BODY.PLATFORM", (void *) &GLOBAL_state.dtm_satBuf} ,
{ "SPS_SCAN_REQUEST.BODY.PLATFORM", (void *) &GLOBAL_state.dtm_satBuf} ,
{ "SPS_SCAN_REQUEST.BODY.PLATFORM", (void *) &GLOBAL_state.dtm_satBuf} ,
{ "SPS_SCAN_REQUEST.BODY.PLATFORM", (void *) &GLOBAL_state.dtm_satBuf} ,
{ "SPS_SCAN_REQUEST.BODY.SENSOR", (void *) &GLOBAL_state.sensorBuf} ,  
{ "SPS_SCAN_REQUEST.BODY.SEQUENCE", (void *) &GLOBAL_state.seq} ,
{ "SPS_SCAN_REQUEST.BODY.ACTIVITY_ID", (void *) &GLOBAL_state.activityBuf} ,
{ "SPS_SCAN_REQUEST.BODY.MEDIA_ID", (void *) &GLOBAL_state.mediaId} ,
{ "SPS_SCAN_REQUEST.BODY.MEDIA_TYPE", (void *) &GLOBAL_state.mediaType} ,
{ "SPS_SCAN_REQUEST.BODY.MEDIA_LOCATION", (void *) &GLOBAL_state.tapeBuf} ,
{ "SPS_SCAN_REQUEST.BODY.DATA_DIRECTION", 
                                    (void *) &GLOBAL_state.dataDirection } ,
#ifdef E_BEAM
{ "SPS_SCAN_REQUEST.BODY.EXPERIMENTAL_BEAM", 
                                    (void *) &GLOBAL_state.experimentalBeam } ,
#endif
{ "SPS_SCAN_REQUEST.BODY.START_ADDRESS", (void *) &GLOBAL_state.startAddr} ,
{ "SPS_SCAN_REQUEST.BODY.END_ADDRESS", (void *) &GLOBAL_state.endAddr} ,
{ "SPS_SCAN_REQUEST.BODY.RECORDER_ID", (void *) &GLOBAL_state.recorderBuf} ,
{ "SPS_SCAN_REQUEST.BODY.STATION_ID", (void *) &GLOBAL_state.stationID} ,
{ "SPS_SCAN_REQUEST.BODY.FRAME_MODE", (void *) &GLOBAL_state.frame_mode_buf} ,
{ "SPS_SCAN_REQUEST.BODY.SITE_NAME", (void *) &GLOBAL_state.site_nameBuf} ,
{ "SPS_SCAN_REQUEST.BODY.MODE", (void *) &GLOBAL_state.mode_buf} ,
{ "SPS_SCAN_REQUEST.BODY.GHA_CORRECTION.ANGLE", (void *) &GLOBAL_state.gha_angle} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.PLATFORM" ,
       (void *) &GLOBAL_state.sv_satBuf} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.STATE_VECTOR_PRECISION",
       (void *) &GLOBAL_state.sv_typeBuf} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.COORDINATE_SYSTEM",
       (void *) &GLOBAL_state.sv_coordSysBuf} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.REVOLUTION",
       (void *) &GLOBAL_state.sv_rev} ,

{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_POSITION",
       (void *) &GLOBAL_state.sv_x_pos} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_POSITION",
       (void *) &GLOBAL_state.sv_y_pos} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_POSITION",
       (void *) &GLOBAL_state.sv_z_pos} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_VELOCITY",
       (void *) &GLOBAL_state.sv_x_vel} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_VELOCITY",
       (void *) &GLOBAL_state.sv_y_vel} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_VELOCITY",
       (void *) &GLOBAL_state.sv_z_vel} ,

{ "SPS_SCAN_REQUEST.BODY.TIME_CORRELATION.REVOLUTION", (void *) &GLOBAL_state.time_corr_rev} ,
{ "SPS_SCAN_REQUEST.BODY.TIME_CORRELATION.PLATFORM_TIME", (void *) &GLOBAL_state.sat_time} ,
{ "SPS_SCAN_REQUEST.BODY.TIME_CORRELATION.CLOCK_CYCLE", (void *) &GLOBAL_state.clock_cycle} ,
{ NULL} 
};  /* sps_scan_request */

static msgInfo sps_scan_request_time[] ={
{ "SPS_SCAN_REQUEST.BODY.START_TIME", (void *) &GLOBAL_state.startTime} ,
{ "SPS_SCAN_REQUEST.BODY.END_TIME", (void *) &GLOBAL_state.endTime} ,
{ "SPS_SCAN_REQUEST.BODY.GHA_CORRECTION.TIME", (void *) &GLOBAL_state.gha_time_val} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.TIME",
       (void *) &GLOBAL_state.sv_time_val} ,
{ "SPS_SCAN_REQUEST.BODY.TIME_CORRELATION.TIME", (void *) &GLOBAL_state.time_corr_time_val} ,
{ NULL} 
};  /* sps_scan_request_time */

static msgInfo sps_scan_request_double[] ={
{ "SPS_SCAN_REQUEST.BODY.GHA_CORRECTION.ANGLE", (void *) &GLOBAL_state.gha_angle} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_POSITION",
       (void *) &GLOBAL_state.sv_x_pos} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_POSITION",
       (void *) &GLOBAL_state.sv_y_pos} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_POSITION",
       (void *) &GLOBAL_state.sv_z_pos} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_VELOCITY",
       (void *) &GLOBAL_state.sv_x_vel} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_VELOCITY",
       (void *) &GLOBAL_state.sv_y_vel} ,
{ "SPS_SCAN_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_VELOCITY",
       (void *) &GLOBAL_state.sv_z_vel} ,

{ NULL}
};  /* sps_scan_request_double */



/************** sps_decode_request ***************************************/
/*   this block below only lists the items that are received in an       */
/*   incoming frame_request message.  other items that are retrieved     */
/*   from the scan results file are listed in a separate structure.      */
/*************************************************************************/

static msgInfo sps_decode_request[] ={
{ "SPS_DECODE_REQUEST.BODY.JOB_ID", (void *) &GLOBAL_state.jobId } ,
{ "SPS_DECODE_REQUEST.BODY.PLATFORM", (void *) &GLOBAL_state.dtm_satBuf } ,
{ "SPS_DECODE_REQUEST.BODY.SENSOR", (void *) &GLOBAL_state.sensorBuf } ,
{ "SPS_DECODE_REQUEST.BODY.REVOLUTION", (void *) &GLOBAL_state.rev } ,
{ "SPS_DECODE_REQUEST.BODY.SEQUENCE", (void *) &GLOBAL_state.seq } ,
{ "SPS_DECODE_REQUEST.BODY.MEDIA_ID", (void *) &GLOBAL_state.mediaId} ,
{ "SPS_DECODE_REQUEST.BODY.MEDIA_TYPE", (void *) &GLOBAL_state.mediaType} ,
{ "SPS_DECODE_REQUEST.BODY.MEDIA_LOCATION", (void *) &GLOBAL_state.tapeBuf} ,
{ "SPS_DECODE_REQUEST.BODY.DATA_DIRECTION", 
                                    (void *) &GLOBAL_state.dataDirection } ,
#ifdef E_BEAM
{ "SPS_DECODE_REQUEST.BODY.EXPERIMENTAL_BEAM", 
                                    (void *) &GLOBAL_state.experimentalBeam } ,
#endif
{ "SPS_DECODE_REQUEST.BODY.MODE", (void *) &GLOBAL_state.mode_buf } ,
{ "SPS_DECODE_REQUEST.BODY.GHA_CORRECTION.ANGLE", (void *) &GLOBAL_state.gha_angle } ,

{ "SPS_DECODE_REQUEST.BODY.TIME_CORRELATION.REVOLUTION", (void *) &GLOBAL_state.time_corr_rev } ,
{ "SPS_DECODE_REQUEST.BODY.TIME_CORRELATION.PLATFORM_TIME", (void *) &GLOBAL_state.sat_time } ,
{ "SPS_DECODE_REQUEST.BODY.TIME_CORRELATION.CLOCK_CYCLE", (void *) &GLOBAL_state.clock_cycle } ,

{ "SPS_DECODE_REQUEST.BODY.SCAN_RESULTS_FILE", (void *) &GLOBAL_state.scanResultsFile } ,

{ "SPS_DECODE_REQUEST.BODY.FRAME_ID", (void *) &GLOBAL_state.frameId } ,
{ "SPS_DECODE_REQUEST.BODY.OUTPUT_FORMAT", (void *) &GLOBAL_state.output_format_buf } ,

{ "SPS_DECODE_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.PLATFORM" ,
       (void *) &GLOBAL_state.sv_satBuf} ,
{ "SPS_DECODE_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.STATE_VECTOR_PRECISION",
       (void *) &GLOBAL_state.sv_typeBuf} ,
{ "SPS_DECODE_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.COORDINATE_SYSTEM",
       (void *) &GLOBAL_state.sv_coordSysBuf} ,
{ "SPS_DECODE_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.REVOLUTION",
       (void *) &GLOBAL_state.sv_rev} ,

{ NULL}
}; /* sps_decode_request */

static msgInfo sps_decode_request_time[] ={
{ "SPS_DECODE_REQUEST.BODY.START_TIME", (void *) &GLOBAL_state.startTime} ,
{ "SPS_DECODE_REQUEST.BODY.END_TIME", (void *) &GLOBAL_state.endTime} ,
{ "SPS_DECODE_REQUEST.BODY.GHA_CORRECTION.TIME", (void *) &GLOBAL_state.gha_time_val} ,
{ "SPS_DECODE_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.TIME",
       (void *) &GLOBAL_state.sv_time_val} ,
{ "SPS_DECODE_REQUEST.BODY.TIME_CORRELATION.TIME", (void *) &GLOBAL_state.time_corr_time_val} ,
{ NULL}
};  /* sps_decode_request_time */

static msgInfo sps_decode_request_double[] ={
{ "SPS_DECODE_REQUEST.BODY.GHA_CORRECTION.ANGLE", (void *) &GLOBAL_state.gha_angle } ,
{ "SPS_DECODE_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_POSITION",
       (void *) &GLOBAL_state.sv_x_pos} ,
{ "SPS_DECODE_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_POSITION",
       (void *) &GLOBAL_state.sv_y_pos} ,
{ "SPS_DECODE_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_POSITION",
       (void *) &GLOBAL_state.sv_z_pos} ,
{ "SPS_DECODE_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_VELOCITY",
       (void *) &GLOBAL_state.sv_x_vel} ,
{ "SPS_DECODE_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_VELOCITY",
       (void *) &GLOBAL_state.sv_y_vel} ,
{ "SPS_DECODE_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_VELOCITY",
       (void *) &GLOBAL_state.sv_z_vel} ,
{ NULL}
};  /* sps_decode_request_double */




/************** sps_frame_request ****************************************/
/*   this block below only lists the items that are received in an       */
/*   incoming frame_request message.  other items that are retrieved     */
/*   from the scan results file are listed in a separate structure.      */
/*************************************************************************/

static msgInfo sps_frame_request[] ={
{ "SPS_FRAME_REQUEST.BODY.JOB_ID", (void *) &GLOBAL_state.jobId } ,
{ "SPS_FRAME_REQUEST.BODY.PLATFORM", (void *) &GLOBAL_state.dtm_satBuf } ,
{ "SPS_FRAME_REQUEST.BODY.SENSOR", (void *) &GLOBAL_state.sensorBuf } ,
{ "SPS_FRAME_REQUEST.BODY.REVOLUTION", (void *) &GLOBAL_state.rev } ,
{ "SPS_FRAME_REQUEST.BODY.SEQUENCE", (void *) &GLOBAL_state.seq } ,
{ "SPS_FRAME_REQUEST.BODY.MEDIA_ID", (void *) &GLOBAL_state.mediaId } ,
{ "SPS_FRAME_REQUEST.BODY.MEDIA_TYPE", (void *) &GLOBAL_state.mediaType } ,
{ "SPS_FRAME_REQUEST.BODY.MEDIA_LOCATION", (void *) &GLOBAL_state.tapeBuf } ,
{ "SPS_FRAME_REQUEST.BODY.DATA_DIRECTION", 
                                    (void *) &GLOBAL_state.dataDirection } ,
#ifdef E_BEAM
{ "SPS_FRAME_REQUEST.BODY.EXPERIMENTAL_BEAM", 
                                    (void *) &GLOBAL_state.experimentalBeam } ,
#endif
{ "SPS_FRAME_REQUEST.BODY.COMPENSATION_FLAG", (void *) &GLOBAL_state.compensated} ,
{ "SPS_FRAME_REQUEST.BODY.QUICKLOOK_FLAG", (void *) &GLOBAL_state.quicklook} ,

{ "SPS_FRAME_REQUEST.BODY.GHA_CORRECTION.ANGLE", 
                                    (void *) &GLOBAL_state.gha_angle } ,

{ "SPS_FRAME_REQUEST.BODY.MODE", (void *) &GLOBAL_state.mode_buf } ,

{ "SPS_FRAME_REQUEST.BODY.TIME_CORRELATION.PLATFORM_TIME", 
         (void *) &GLOBAL_state.sat_time } ,
{ "SPS_FRAME_REQUEST.BODY.TIME_CORRELATION.CLOCK_CYCLE", 
         (void *) &GLOBAL_state.clock_cycle } ,
{ "SPS_FRAME_REQUEST.BODY.TIME_CORRELATION.REVOLUTION", 
         (void *) &GLOBAL_state.time_corr_rev } ,

{ "SPS_FRAME_REQUEST.BODY.PRODUCT_TYPE", (void *) &GLOBAL_state.productT_buf } ,
{ "SPS_FRAME_REQUEST.BODY.PIXEL_SPACING", 
         (void *) &GLOBAL_state.pixelSpacing } ,
{ "SPS_FRAME_REQUEST.BODY.FRAME_ID", (void *) &GLOBAL_state.frameId } ,
{ "SPS_FRAME_REQUEST.BODY.SUBFRAME_ID", (void *) &GLOBAL_state.subframe_id } ,
{ "SPS_FRAME_REQUEST.BODY.OUTPUT_FORMAT", 
         (void *) &GLOBAL_state.output_format_buf } ,
{ "SPS_FRAME_REQUEST.BODY.SITE_NAME", (void *) &GLOBAL_state.site_nameBuf } ,

{ "SPS_FRAME_REQUEST.BODY.PROJECTION", (void *) &GLOBAL_state.projection_buf } ,
{ "SPS_FRAME_REQUEST.BODY.PROCESSING_GAIN", 
         (void *) &GLOBAL_state.processingGain } ,

{ "SPS_FRAME_REQUEST.BODY.AVG_TERRAIN_HT", 
         (void *) &GLOBAL_state.avgTerrainHt } ,
{ "SPS_FRAME_REQUEST.BODY.PS_REFERENCE_LAT", 
         (void *) &GLOBAL_state.standardLat } ,
{ "SPS_FRAME_REQUEST.BODY.PS_REFERENCE_LON", 
         (void *) &GLOBAL_state.standardLon } ,
{ "SPS_FRAME_REQUEST.BODY.UTM_ZONE", (void *) &GLOBAL_state.UTM_zone } ,
{ "SPS_FRAME_REQUEST.BODY.CALPARMS_FILE_PRIMARY", 
         (void *) &GLOBAL_state.calParamsFile } ,
{ "SPS_FRAME_REQUEST.BODY.SCAN_RESULTS_FILE", 
         (void *) &GLOBAL_state.scanResultsFile } ,

{ "SPS_FRAME_REQUEST.BODY.DESKEW", (void *) &GLOBAL_state.skew_buf } ,
{ "SPS_FRAME_REQUEST.BODY.TERRAIN_CORRECTION", 
         (void *) &GLOBAL_state.terrain_correction_buf } ,
{ "SPS_FRAME_REQUEST.BODY.LAMBERT_LATITUDE_N", 
         (void *) &GLOBAL_state.lambert_lat_n } ,
{ "SPS_FRAME_REQUEST.BODY.LAMBERT_LATITUDE_S", 
         (void *) &GLOBAL_state.lambert_lat_s } ,

{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.PLATFORM" ,
       (void *) &GLOBAL_state.sv_satBuf} ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.STATE_VECTOR_PRECISION",
       (void *) &GLOBAL_state.sv_typeBuf} ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.COORDINATE_SYSTEM",
       (void *) &GLOBAL_state.sv_coordSysBuf} ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.REVOLUTION",
       (void *) &GLOBAL_state.sv_rev} ,

{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_POSITION",
       (void *) &GLOBAL_state.sv_x_pos} ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_POSITION",
       (void *) &GLOBAL_state.sv_y_pos} ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_POSITION",
       (void *) &GLOBAL_state.sv_z_pos} ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_VELOCITY",
       (void *) &GLOBAL_state.sv_x_vel} ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_VELOCITY",
       (void *) &GLOBAL_state.sv_y_vel} ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_VELOCITY",
       (void *) &GLOBAL_state.sv_z_vel} ,

{ NULL } }; /* sps_frame_request */

/************** sps_frame_request_double ***********************************/
/*   this block below only lists the double items that are received in an  */
/*   incoming frame_request message.                                       */
/***************************************************************************/

static msgInfo sps_frame_request_double[] ={
{ "SPS_FRAME_REQUEST.BODY.GHA_CORRECTION.ANGLE", 
       (void *) &GLOBAL_state.gha_angle } ,
{ "SPS_FRAME_REQUEST.BODY.PIXEL_SPACING", 
       (void *) &GLOBAL_state.pixelSpacing } ,

{ "SPS_FRAME_REQUEST.BODY.AVG_TERRAIN_HT", 
       (void *) &GLOBAL_state.avgTerrainHt } ,
{ "SPS_FRAME_REQUEST.BODY.PS_REFERENCE_LAT", 
       (void *) &GLOBAL_state.standardLat } ,
{ "SPS_FRAME_REQUEST.BODY.PS_REFERENCE_LON", 
       (void *) &GLOBAL_state.standardLon } ,

{ "SPS_FRAME_REQUEST.BODY.LAMBERT_LATITUDE_N", 
       (void *) &GLOBAL_state.lambert_lat_n } ,
{ "SPS_FRAME_REQUEST.BODY.LAMBERT_LATITUDE_S", 
       (void *) &GLOBAL_state.lambert_lat_s } ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_POSITION",
       (void *) &GLOBAL_state.sv_x_pos} ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_POSITION",
       (void *) &GLOBAL_state.sv_y_pos} ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_POSITION",
       (void *) &GLOBAL_state.sv_z_pos} ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_VELOCITY",
       (void *) &GLOBAL_state.sv_x_vel} ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_VELOCITY",
       (void *) &GLOBAL_state.sv_y_vel} ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_VELOCITY",
       (void *) &GLOBAL_state.sv_z_vel} ,

{ NULL } }; /* sps_frame_request_double */



/************** sps_frame_request_time ***********************************/
/*   this block below only lists the time items that are received in an  */
/*   incoming frame_request message.                                     */
/*************************************************************************/

static msgInfo sps_frame_request_time[] ={
{ "SPS_FRAME_REQUEST.BODY.GHA_CORRECTION.TIME", 
       (void *) &GLOBAL_state.gha_time_val } ,
{ "SPS_FRAME_REQUEST.BODY.TIME_CORRELATION.TIME", 
       (void *) &GLOBAL_state.time_corr_time_val } ,
{ "SPS_FRAME_REQUEST.BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.TIME",
       (void *) &GLOBAL_state.sv_time_val } ,
{ NULL } }; /* sps_frame_request_time */



#endif
