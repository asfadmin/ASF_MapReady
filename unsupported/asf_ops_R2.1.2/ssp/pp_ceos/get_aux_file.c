/* SccsId[]= @(#)get_aux_file.c	2.41 3/24/98 */
static char sccsid_get_aux_file[]= "@(#)PPget_aux_file.c:2.41";

#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "odl.h"
#include "ssp_aux_odl.h"
#include "aux_file.h"
#include "error.h"

#define AGC_CHANGES "AUXILIARY_FILE.BODY.BURST_INFO.AGC_CHANGES"
#define AGC_VALUES "AUXILIARY_FILE.BODY.BURST_INFO.AGC_VALUES"

int get_aux_file(char *filename, struct AUX_FILE *aux_file)
{
  ODL msg;
  struct stat stbuf;
  char *buf;
  int fd, ierr, maximum_rows, maximum_cols, i, j;

  /* open auxillary file, read the entire file into a buffer */
  if (!(fd = open(filename, O_RDONLY))) {
    sprintf(aux_file->error_msg, "get_aux_file: open file failed.");
    return ierr_2;
  }
  if (fstat(fd, &stbuf) == -1) {
    sprintf(aux_file->error_msg, "get_aux_file: checking file size failed.");
    return ierr_2;
  }
  if (!(buf = (char *) malloc(stbuf.st_size))) {
    sprintf(aux_file->error_msg, "get_aux_file: failed to allocate memory.");
    return ierr_5;
  }

  if (read(fd, buf, stbuf.st_size) != stbuf.st_size) {
      sprintf(aux_file->error_msg, "get_aux_file: failed to read file.");
      return ierr_4;
  }
  close(fd);


  /* convert content of buffer to ODL */
  if ((msg= StrToODL(buf, stbuf.st_size)) == NULL) { 
      sprintf(aux_file->error_msg, "get_aux_file: can't parse string.");
      return ierr_11;
  }
  free(buf); 



  /* extracting information from msg */
  aux_file->echo_samples = ODLGetInt(msg, ECHO_SAMPLES, &ierr);
  if (ierr != 0) {
    strcpy(aux_file->error_msg , "get_aux_file: Can't get ECHO_SAMPLES");
    return(ierr_12);
  }

  aux_file->echo_pulses = ODLGetInt(msg, ECHO_PULSES, &ierr);
  if (ierr != 0) {
    strcpy(aux_file->error_msg , "get_aux_file: Can't get ECHO_PULSES");
    return(ierr_12);
  }

  aux_file->replica_agc = ODLGetDouble(msg, REPLICA_AGC, &ierr);
  if (ierr != 0) {
     strcpy(aux_file->error_msg, "get_aux_file: Can't get REPLICA_AGC");
     return(ierr_12);
  }

  aux_file->lna_temp = ODLGetDouble(msg, LNA_TEMP, &ierr);
  if (ierr != 0) {
     strcpy(aux_file->error_msg, "get_aux_file: Can't get LNA_TEMP");
     return(ierr_12);
  }

  aux_file->subsystem_temp = ODLGetDouble(msg, SUBSYSTEM_TEMP, &ierr);
  if (ierr != 0) {
     strcpy(aux_file->error_msg, "get_aux_file: Can't get SUBSYSTEM_TEMP");
     return(ierr_12);
  }

  aux_file->protector_temp = ODLGetDouble(msg, PROTECTOR_TEMP, &ierr);
  if (ierr != 0) {
     strcpy(aux_file->error_msg, "get_aux_file: Can't get PROTECTOR_TEMP");
     return(ierr_12);
  }

  aux_file->calibration_temp = ODLGetDouble(msg, CALI_TEMP, &ierr);
  if (ierr != 0) {
     strcpy(aux_file->error_msg, "get_aux_file: Can't get CALI_TEMP");
     return(ierr_12);
  }

  aux_file->rx_agc = ODLGetDouble(msg, RX_AGC, &ierr);
  if (ierr != 0) {
     strcpy(aux_file->error_msg, "get_aux_file: Can't get RX_AGC");
     return(ierr_12);
  }

  maximum_rows = 1;
  maximum_cols = aux_file->echo_pulses+1;
  aux_file->agc_changes = (int*) malloc(maximum_cols*sizeof(int));
  for(i = 0; i < maximum_cols; i++) aux_file->agc_changes[i] = 0;
  if(! ODLGetArrayInt(msg, AGC_CHANGES,
                      aux_file->agc_changes,
                      &maximum_rows, &maximum_cols)) {
    strcpy(aux_file->error_msg , "get_auxiliary: Can't get AGC_CHANGES");
    return(ierr_12);
  }
  aux_file->agc_changes[maximum_cols] = aux_file->echo_pulses;

  maximum_rows = 1;
  maximum_cols = aux_file->echo_pulses+1;
  aux_file->agc_values = (double*) malloc(maximum_cols*sizeof(double));
  for(i = 0; i < maximum_cols; i++) aux_file->agc_values[i] = 0.0;
  if(! ODLGetArrayDouble(msg, AGC_VALUES,
                         aux_file->agc_values,
                         &maximum_rows, &maximum_cols)) {
    strcpy(aux_file->error_msg , "get_auxiliary: Can't get AGC_VALUES");
    return(ierr_12);
  }
  aux_file->max_agc_value = 0.0;
  for (i=0; i<maximum_rows; i++) {
    for (j=0; j<maximum_cols; j++) { 
      if (aux_file->agc_values[j] > aux_file->max_agc_value)
        aux_file->max_agc_value = aux_file->agc_values[j];
    }
  }
      

  strcpy(aux_file->sc_time , ODLGetTime(msg, SC_TIME, &ierr));
  if (ierr != 0) {
    strcpy(aux_file->error_msg , "get_auxillary: Can't get SC_TIME");
    return(ierr_12);
  }


  aux_file->sampling_rate = ODLGetDouble(msg, SAMP_RATE, &ierr);
  if (ierr != 0) {
     strcpy(aux_file->error_msg, "get_aux_file: Can't get SAMPLING RATE");
     return(ierr_12);
  }
  
  aux_file->bit_error_rate = ODLGetDouble(msg, BIT_ERROR_RATE, &ierr);
  if (ierr != 0) {
     strcpy(aux_file->error_msg, "get_aux_file: Can't get BIT_ERROR_RATE");
     return(ierr_12);
  }

  ODLFree(msg);
  return(0);
}
