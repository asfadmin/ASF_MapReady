/* SccsId[]= @(#)get_auxiliary.c	2.41 3/24/98 */
static char sccsid_get_auxiliary[]= "@(#)PPget_auxiliary.c:2.41";

#include "pp.h"
#include "ssp_aux_odl.h"

#define WINDOW_POS_CHANGES "AUXILIARY_FILE.BODY.BURST_INFO.WINDOW_POS_CHANGES"
#define WINDOW_POS_VALUES "AUXILIARY_FILE.BODY.BURST_INFO.WINDOW_POS_VALUES"
#define AGC_CHANGES "AUXILIARY_FILE.BODY.BURST_INFO.AGC_CHANGES"
#define AGC_VALUES "AUXILIARY_FILE.BODY.BURST_INFO.AGC_VALUES"


int get_auxiliary(char filename[],
		  struct Auxiliary *auxiliary,
		  char error_msg[])
{
  ODL msg;
  struct stat stbuf;
  char *buf;
  int fd, ierr, maximun_rows, maximum_cols, i;

  /* open frame_request file, read the entire file into a buffer */
  if ((fd = open(filename, O_RDONLY)) == -1) {
    sprintf(error_msg, "get_auxiliary: open file %s failed.",filename);
    return ierr_2;
  }
  if (fstat(fd, &stbuf) == -1) {
    sprintf(error_msg, "get_auxiliary: checking file size failed.");
    return ierr_2;
  }
  if ((buf = (char *) malloc(stbuf.st_size)) == NULL) {
    sprintf(error_msg, "get_auxiliary: failed to allocate memory.");
    return ierr_5;
  }

  if (read(fd, buf, stbuf.st_size) != stbuf.st_size) {
      sprintf(error_msg, "get_auxiliary: read file failed.");
      return ierr_4;
  }
  close(fd);


  /* convert content of buffer to ODL */
  if ((msg= StrToODL(buf, stbuf.st_size)) == NULL) { 
      sprintf(error_msg, "get_auxiliary: can't parse string.");
      return ierr_11;
  }
  free(buf); 


  /* extract information from buffer */
  auxiliary->aux_frame_id = ODLGetInt(msg, FRAME_ID, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_auxiliary: Can't get FRAME_ID");
    return(ierr_12);
  }

  strcpy(auxiliary->aux_frame_mode, ODLGetString(msg, FRAME_MODE, &ierr)); 
  if (ierr != 0) {
    strcpy(error_msg , "get_auxiliary: Can't get FRAME_MODE");
    return(ierr_12);
  }

  strcpy(auxiliary->aux_instr_mode, ODLGetString(msg, INSTRUMENT, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_auxiliary: Can't get INSTRUMENT");
    return(ierr_12);
  }

  strcpy(auxiliary->platform , ODLGetString(msg,SATELLITE, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_auxiliary: Can't get SATELLITE");
    return(ierr_12);
  }

  strcpy(auxiliary->aux_sc_time, ODLGetTime(msg, SC_TIME, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_auxiliary: Can't get SC_TIME");
    return(ierr_12);
  }

  auxiliary->aux_echo_samples = ODLGetInt(msg, ECHO_SAMPLES, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_auxiliary: Can't get ECHO_SAMPLES");
    return(ierr_12);
  }

  auxiliary->aux_echo_pulses = ODLGetInt(msg, ECHO_PULSES, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_auxiliary: Can't get ECHO_PULSES");
    return(ierr_12);
  }
 
  auxiliary->aux_sampling_rate = ODLGetDouble(msg, SAMP_RATE, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_auxiliary: Can't get SAMP_RATE");
    return(ierr_12);
  }

  auxiliary->aux_pulse_period = ODLGetDouble(msg, PULSE_PRD, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_auxiliary: Can't get PULSE_PRD");
    return(ierr_12);
  }

  auxiliary->aux_window_st_time = ODLGetDouble(msg, WINDOW_ST_TIME, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_auxiliary: Can't get WINDOW_ST_TIME");
    return(ierr_12);
  }
 
  auxiliary->aux_window_duration = ODLGetDouble(msg, WINDOW_DURATION, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_auxiliary: Can't get WINDOW_DURATION");
    return(ierr_12);
  }

  /* allow 10 position changes but only the first two are used */
  maximun_rows = 1;
  maximum_cols = 10;
  auxiliary->aux_window_pos_changes[0] = 0;
  auxiliary->aux_window_pos_changes[1] = 0;
  if(! ODLGetArrayInt(msg, WINDOW_POS_CHANGES,
		      auxiliary->aux_window_pos_changes, 
		      &maximun_rows, &maximum_cols)) {
    strcpy(error_msg , "get_auxiliary: Can't get WINDOW_POS_CHANGES");
    return(ierr_12);
  }

  /* allow 10 position values but only the first two are used */
  maximun_rows = 1;
  maximum_cols = 10;
  auxiliary->aux_window_pos_values[0] = 0.0;
  auxiliary->aux_window_pos_values[1] = 0.0;
  if(! ODLGetArrayDouble(msg, WINDOW_POS_VALUES,
			 auxiliary->aux_window_pos_values, 
			 &maximun_rows, &maximum_cols)) {
    strcpy(error_msg , "get_auxiliary: Can't get WINDOW_POS_VALUES");
    return(ierr_12);
  }

  maximun_rows = 1;
  maximum_cols = auxiliary->aux_echo_pulses+1;
  auxiliary->aux_agc_changes = (int*) malloc(maximum_cols*sizeof(int));
  for(i = 0; i < maximum_cols; i++) auxiliary->aux_agc_changes[i] = 0;
  if(! ODLGetArrayInt(msg, AGC_CHANGES,
		      auxiliary->aux_agc_changes,
		      &maximun_rows, &maximum_cols)) {
    strcpy(error_msg , "get_auxiliary: Can't get AGC_CHANGES");
    return(ierr_12);
  }
  auxiliary->aux_agc_changes[maximum_cols] = auxiliary->aux_echo_pulses;

  maximun_rows = 1;
  maximum_cols = auxiliary->aux_echo_pulses+1;
  auxiliary->aux_agc_values = (double*) malloc(maximum_cols*sizeof(double));
  for(i = 0; i < maximum_cols; i++) auxiliary->aux_agc_values[i] = 0.0;
  if(! ODLGetArrayDouble(msg, AGC_VALUES,
			 auxiliary->aux_agc_values,
			 &maximun_rows, &maximum_cols)) {
    strcpy(error_msg , "get_auxiliary: Can't get AGC_VALUES");
    return(ierr_12);
  }

  ODLFree(msg);
  return(0);
}
