/* SccsId[]= @(#)get_frame_rqst.c	2.41 3/24/98 */
static char sccsid_get_frame_rqst[]= "@(#)PPget_frame_rqst.c:2.41";

#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "odl.h"
#include "ssp_req_odl.h"
#include "odl_file.h"
#include "error.h"

int get_frame_rqst(char *filename, struct ODL_FILE *odl_file)
{
  ODL msg;
  struct stat stbuf;
  char *buf;
  int fd, ierr;

  /* open frame_request file, read the entire file into a buffer */
  if (!(fd = open(filename, O_RDONLY))) {
    sprintf(odl_file->error_msg, "get_frame_rqst: open file failed.");
    return ierr_2;
  }
  if (fstat(fd, &stbuf) == -1) {
    sprintf(odl_file->error_msg, "get_frame_rqst: checking file size failed.");
    return ierr_2;
  }
  if (!(buf = (char *) malloc(stbuf.st_size))) {
    sprintf(odl_file->error_msg, "get_frame_rqst: failed to allocate memory.");
    return ierr_5;
  }

  if (read(fd, buf, stbuf.st_size) != stbuf.st_size) {
      sprintf(odl_file->error_msg, "get_frame_rqst: failed to read file.");
      return ierr_4;
  }
  close(fd);


  /* convert content of buffer to ODL */
  if ((msg= StrToODL(buf, stbuf.st_size)) == NULL) { 
      sprintf(odl_file->error_msg, "get_frame_rqst: can't parse string.");
      return ierr_11;
  }
  free(buf); 



  /* extracting information from msg */
  odl_file->job_id = ODLGetInt(msg, JOB_ID, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get JOB_ID");
    return(ierr_12);
  }


  strcpy(odl_file->product_id , ODLGetString(msg, PRODUCT_ID, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get PRODUCT_ID");
    return(ierr_12);
  }

  strcpy(odl_file->platform , ODLGetString(msg,PLATFORM, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get PLATFORM");
    return(ierr_12);
  }

  strcpy(odl_file->start_time , ODLGetTime(msg, START_TIME, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get START_TIME");
    return(ierr_12);
  }

  strcpy(odl_file->gha_corr_time , ODLGetTime(msg, GHA_CORR_TIME, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get GHA_CORR_TIME");
    return(ierr_12);
  }

  odl_file->gha_corr_angle = ODLGetDouble(msg,  GHA_CORR_ANGLE, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get GHA_CORR_ANGLE");
    return(ierr_12);
  }

  strcpy(odl_file->instr_mode , ODLGetString(msg, INSTRUMENT_MODE, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get INSTRUMENT_MODE");
    return(ierr_12);
  }

  strcpy(odl_file->sv_time , ODLGetTime(msg, SV_TIME, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get State_Vector_Date");
    return(ierr_12);
  }

  odl_file->sv_x_pos = ODLGetDouble(msg, SV_X_POS, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get SV_X_POS");
    return(ierr_12);
  }

  odl_file->sv_y_pos = ODLGetDouble(msg, SV_Y_POS, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get SV_Y_POS");
    return(ierr_12);
  }

  odl_file->sv_z_pos = ODLGetDouble(msg, SV_Z_POS, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get SV_Z_POS");

    return(ierr_12);
  }

  odl_file->sv_x_vel = ODLGetDouble(msg, SV_X_VEL, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get SV_X_VEL");
    return(ierr_12);
  }

  odl_file->sv_y_vel = ODLGetDouble(msg, SV_Y_VEL, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get SV_Y_VEL");
    return(ierr_12);
    }

  odl_file->sv_z_vel = ODLGetDouble(msg, SV_Z_VEL, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get SV_Z_VEL");
    return(ierr_12);
  }

  strcpy(odl_file->prod_type , ODLGetString(msg, PROD_TYPE, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get PROD_TYPE");
    return(ierr_12);
  }

  odl_file->pixel_spacing = ODLGetDouble(msg, PIXEL_SPACING, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get PIXEL_SPACING");
    return(ierr_12);
  }

  odl_file->frame_id = ODLGetInt(msg, FRAME_ID, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get FRAME_ID");
    return(ierr_12);
  }

  strcpy(odl_file->frame_mode , ODLGetString(msg, FRAME_MODE, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get FRAME_MODE");
    return(ierr_12);
  }

  odl_file->proc_gain = ODLGetInt(msg, PROC_GAIN, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get PROC_GAIN");
    return(ierr_12);
  }

  odl_file->avg_terrain= ODLGetDouble(msg, AVG_TERRAIN, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get AVG_TERRAIN");
    return(ierr_12);
  }

  strcpy(odl_file->deskew,ODLGetString(msg, DESKEW, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get DESKEW");
    return(ierr_12);
  }

  strcpy(odl_file->image_file,ODLGetString(msg, CEOS_DATA, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get CEOS_DATA");
    return(ierr_12);
  }


  strcpy(odl_file->pmf_file,ODLGetString(msg, PMF_FILE, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get PMF_FILE");
    return(ierr_12);
  }


  strcpy(odl_file->ceos_leader_file,ODLGetString(msg, CEOS_LEADER, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get CEOS_LEADER");
    return(ierr_12);
  }

  strcpy(odl_file->echo_file,ODLGetString(msg, ECHO_FILE, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get ECHO_FILE");
    return(ierr_12);
  }

  strcpy(odl_file->aux_file , ODLGetString(msg, AUX_FILE, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get AUXILIARY_FILE");
    return(ierr_12);
  }

  strcpy(odl_file->eph_file , ODLGetString(msg, EPH_FILE, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get EPHEMERIS_FILE");
    return(ierr_12);
  }

  strcpy(odl_file->rep_file , ODLGetString(msg, REP_FILE, &ierr));
  if (ierr !=0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get REPLICA_FILE");
    return(ierr_12);
  }

  odl_file->revolution = ODLGetInt(msg, REV, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get REVOLUTION");
    return(ierr_12);
  }


  strcpy(odl_file->media_id , ODLGetString(msg, MEDIA_ID, &ierr));
  if (ierr !=0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get MEDIA_ID");
    return(ierr_12);
  }



  strcpy(odl_file->cali_file , ODLGetString(msg, CALI_FILE, &ierr));
  if (ierr !=0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get CALI_FILE");
    return(ierr_12);
  }


  strcpy(odl_file->frame_file , ODLGetString(msg, FRAME_FILE, &ierr));
  if (ierr !=0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get FRAME_FILE");
    return(ierr_12);
  }

  strcpy(odl_file->time , ODLGetTime(msg, TIME, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get TIME");
    return(ierr_12);
  }


  strcpy(odl_file->sensor , ODLGetString(msg, SENSOR, &ierr));
  if (ierr !=0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get SENSOR");
    return(ierr_12);
  }


  odl_file->seq = ODLGetInt(msg, SEQ, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get SEQ");
    return(ierr_12);
  }



  strcpy(odl_file->activity , ODLGetString(msg, ACTIVITY, &ierr));
  if (ierr !=0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get ACTIVITY");
    return(ierr_12);
  }



  strcpy(odl_file->media_type , ODLGetString(msg, MEDIA_TYPE, &ierr));
  if (ierr !=0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get MEDIA_TYPE");
    return(ierr_12);
  }

  strcpy(odl_file->media_location , ODLGetString(msg, MEDIA_LOCATION, &ierr));
  if (ierr !=0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get MEDIA_LOCATION");
    return(ierr_12);
  }


  strcpy(odl_file->recordid , ODLGetString(msg, RECORDID, &ierr));
  if (ierr !=0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get RECORDID");
    return(ierr_12);
  }


  strcpy(odl_file->station_id , ODLGetString(msg, STATION_ID, &ierr));
  if (ierr !=0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get STATION_ID");
    return(ierr_12);
  }


  strcpy(odl_file->sitename , ODLGetString(msg, SITENAME, &ierr));
  if (ierr !=0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get SITENAME");
    return(ierr_12);
  }


  odl_file->file_version = ODLGetInt(msg, FILE_VERSION, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get FILE_VERSION");
    return(ierr_12);
  }


  odl_file->subframe_id = ODLGetInt(msg, SUBFRAME_ID, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get SUBFRAME_ID");
    return(ierr_12);
  }



  odl_file->start_address = ODLGetInt(msg, START_ADDRESS, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get START_ADDRESS");
    return(ierr_12);
  }

  odl_file->end_address = ODLGetInt(msg, END_ADDRESS, &ierr);
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get END_ADDRESS");
    return(ierr_12);
  }


  strcpy(odl_file->sv_type_precision , ODLGetString(msg, SV_TYPE_PRECISION, &ierr));
  if (ierr !=0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get SV_TYPE_PRECISION");
    return(ierr_12);
  }

  strcpy(odl_file->end_time , ODLGetTime(msg, END_TIME, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get END_TIME");
    return(ierr_12);
  }


  strcpy(odl_file->comp_flag , ODLGetString(msg, COMP_FLAG, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get COMPENSATION_FLAG");
    return(ierr_12);
  }

  strcpy(odl_file->projection , ODLGetString(msg, PROJECTION, &ierr));
  if (ierr != 0) {
    strcpy(odl_file->error_msg , "get_frame_rqst: Can't get PROJECTION");
    return(ierr_12);
  }


  ODLFree(msg);
  return(0);
}
