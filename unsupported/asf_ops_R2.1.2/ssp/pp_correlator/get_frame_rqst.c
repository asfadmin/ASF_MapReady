/* SccsId[]= @(#)get_frame_rqst.c	2.41 3/24/98 */
static char sccsid_get_frame_rqst[]= "@(#)PPget_frame_rqst.c:2.41";

#include "pp.h"
#include "ssp_req_odl.h"

int get_frame_rqst(char filename[], struct Frame_Request *request,
		   char error_msg[])
{
  ODL msg;
  struct stat stbuf;
  char *buf;
  int fd, ierr;

  /* open frame_request file, read the entire file into a buffer */
  if ((fd = open(filename, O_RDONLY)) == -1) {
    sprintf(error_msg, "get_frame_rqst: open file %s failed.", filename);
    return ierr_2;
  }
  if (fstat(fd, &stbuf) == -1) {
    sprintf(error_msg, "get_frame_rqst: checking file size failed.");
    return ierr_2;
  }
  if ((buf = (char *) malloc(stbuf.st_size)) == NULL) {
    sprintf(error_msg, "get_frame_rqst: allocate memory failed.");
    return ierr_5;
  }

  if (read(fd, buf, stbuf.st_size) != stbuf.st_size) {
      sprintf(error_msg, "get_frame_rqst: read file failed.");
      return ierr_4;
  }
  close(fd);


  /* convert content of buffer to ODL */
  if ((msg= StrToODL(buf, stbuf.st_size)) == NULL) { 
      sprintf(error_msg, "get_frame_rqst: can't parse string.");
      return ierr_11;
  }
  free(buf); 



  /* extracting information from msg */
  request->job_id = ODLGetInt(msg, JOB_ID, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get JOB_ID");
    return(ierr_12);
  }

  request->revolution = ODLGetInt(msg, REV, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get REV");
    return(ierr_12);
  }


  strcpy(request->platform , ODLGetString(msg,PLATFORM, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get PLATFORM");
    return(ierr_12);
  }

  strcpy(request->start_time , ODLGetTime(msg, START_TIME, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get START_TIME");
    return(ierr_12);
  }

  strcpy(request->gha_corr_time , ODLGetTime(msg, GHA_CORR_TIME, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get GHA_CORR_TIME");
    return(ierr_12);
  }

  request->gha_corr_angle = ODLGetDouble(msg,  GHA_CORR_ANGLE, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get GHA_CORR_ANGLE");
    return(ierr_12);
  }

  strcpy(request->instr_mode , ODLGetString(msg, INSTRUMENT_MODE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get INSTRUMENT_MODE");
    return(ierr_12);
  }

  strcpy(request->sv_time , ODLGetTime(msg, SV_TIME, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get State_Vector_Date");
    return(ierr_12);
  }

  request->sv_x_pos = ODLGetDouble(msg, SV_X_POS, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get SV_X_POS");
    return(ierr_12);
  }

  request->sv_y_pos = ODLGetDouble(msg, SV_Y_POS, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get SV_Y_POS");
    return(ierr_12);
  }

  request->sv_z_pos = ODLGetDouble(msg, SV_Z_POS, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get SV_Z_POS");

    return(ierr_12);
  }

  request->sv_x_vel = ODLGetDouble(msg, SV_X_VEL, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get SV_X_VEL");
    return(ierr_12);
  }

  request->sv_y_vel = ODLGetDouble(msg, SV_Y_VEL, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get SV_Y_VEL");
    return(ierr_12);
    }

  request->sv_z_vel = ODLGetDouble(msg, SV_Z_VEL, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get SV_Z_VEL");
    return(ierr_12);
  }

  strcpy(request->prod_type , ODLGetString(msg, PROD_TYPE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get PROD_TYPE");
    return(ierr_12);
  }

  request->pixel_spacing = ODLGetDouble(msg, PIXEL_SPACING, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get PIXEL_SPACING");
    return(ierr_12);
  }

  request->frame_id = ODLGetInt(msg, FRAME_ID, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get FRAME_ID");
    return(ierr_12);
  }

  strcpy(request->frame_mode , ODLGetString(msg, FRAME_MODE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get FRAME_MODE");
    return(ierr_12);
  }

  request->proc_gain = ODLGetInt(msg, PROC_GAIN, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get PROC_GAIN");
    return(ierr_12);
  }

  request->avg_terrain= ODLGetDouble(msg, AVG_TERRAIN, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get AVG_TERRAIN");
    return(ierr_12);
  }

  strcpy(request->image_file,ODLGetString(msg, CEOS_DATA, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get CEOS_DATA");
    return(ierr_12);
  }

  strcpy(request->echo_file,ODLGetString(msg, ECHO_FILE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get ECHO_FILE");
    return(ierr_12);
  }

  strcpy(request->aux_file , ODLGetString(msg, AUX_FILE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get AUXILIARY_FILE");
    return(ierr_12);
  }

  strcpy(request->eph_file , ODLGetString(msg, EPH_FILE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_frame_rqst: Can't get EPHEMERIS_FILE");
    return(ierr_12);
  }

  strcpy(request->rep_file , ODLGetString(msg, REP_FILE, &ierr));
  if (ierr !=0) {
    strcpy(error_msg , "get_frame_rqst: Can't get REPLICA_FILE");
    return(ierr_12);
  }

  strcpy(request->comp_flag , ODLGetString(msg, COMP_FLAG, &ierr));
  if (ierr !=0) {
    strcpy(error_msg , "get_frame_rqst: Can't get COMP_FLAG");
    return(ierr_12);
  }
 
  ODLFree(msg);
  return(0);
}
