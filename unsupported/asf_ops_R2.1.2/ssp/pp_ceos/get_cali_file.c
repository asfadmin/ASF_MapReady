/* SccsId[]= @(#)get_cali_file.c	2.41 3/24/98 */
static char sccsid_get_cali_file[]= "@(#)PPget_cali_file.c:2.41";

#include <stdio.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "odl.h"
#include "ssp_cali_odl.h" 
#include "cali_file.h"
#include "error.h"

int get_cali_file(char *filename, struct CALI_FILE *cali_file)
{
  ODL msg;
  struct stat stbuf;
  char *buf;
  int fd, ierr;

  /* open cali file, read the entire file into a buffer */
  if (!(fd = open(filename, O_RDONLY))) {
    sprintf(cali_file->error_msg, "get_cali_file: open file failed.");
    return ierr_2;
  }
  if (fstat(fd, &stbuf) == -1) {
    sprintf(cali_file->error_msg, "get_cali_file: checking file size failed.");
    return ierr_2;
  }
  if (!(buf = (char *) malloc(stbuf.st_size))) {
    sprintf(cali_file->error_msg, "get_cali_file: failed to allocate memory.");
    return ierr_5;
  }

  if (read(fd, buf, stbuf.st_size) != stbuf.st_size) {
      sprintf(cali_file->error_msg, "get_cali_file: failed to read file.");
      return ierr_4;
  }
  close(fd);


  /* convert content of buffer to ODL */
  if ((msg= StrToODL(buf, stbuf.st_size)) == NULL) { 
      sprintf(cali_file->error_msg, "get_cali_file: can't parse string.");
      return ierr_11;
  }
  free(buf); 



  /* extracting information from msg */


/*
  strcpy(cali_file->cali_date, ODLGetString(msg, CALI_DATE, &ierr));
  if (ierr != 0) {
    strcpy(cali_file->error_msg , "get_cali_file: Can't get CALI_DATE");
    return(ierr_12);
  }
*/
/*
  cali_file->islr = ODLGetDouble(msg, ISLR, &ierr);
  if (ierr != 0) {
    strcpy(cali_file->error_msg , "get_cali_file: Can't get ISLR");
    return(ierr_12);
  }


  cali_file->pslr = ODLGetDouble(msg, PSLR, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get PSLR");
     return(ierr_12);
  }
*/
  cali_file->islr = -99.0;
  cali_file->pslr = -99.0;
/*
  cali_file->azi_ambig = ODLGetDouble(msg, AZI_AMBIG, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get AZI_AMBIG");
     return(ierr_12);
  }

  cali_file->rng_ambig = ODLGetDouble(msg, RNG_AMBIG, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get RNG_AMBIG");
     return(ierr_12);
  }
*/
  cali_file->azi_ambig = -99.0;

/*

  cali_file->snr = ODLGetDouble(msg, SNR, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get SNR");
     return(ierr_12);
  }

  cali_file->ber = ODLGetDouble(msg, BER, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get BER");
     return(ierr_12);
  }

*/
/*
  cali_file->rng_res = ODLGetDouble(msg, RNG_RES, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get RNG_RES");
     return(ierr_12);
  }

  cali_file->azi_res = ODLGetDouble(msg, AZI_RES, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get AZI_RES");
     return(ierr_12);
  }
*/
  cali_file->rng_res = -99.0;
  cali_file->azi_res = -99.0;

/*

  cali_file->rad_res = ODLGetDouble(msg, RAD_RES, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get RAD_RES");
     return(ierr_12);
  }

  cali_file->dyn_rng = ODLGetDouble(msg, DYN_RNG, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get DYN_RNG");
     return(ierr_12);
  }

  cali_file->abs_rad_unc_db = ODLGetDouble(msg, ABS_RAD_UNC_DB, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get ABS_RAD_UNC_DB");
     return(ierr_12);
  }

  cali_file->rel_rad_unc_db = ODLGetDouble(msg, REL_RAD_UNC_DB, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get REL_RAD_UNC_DB");
     return(ierr_12);
  }

*/

  cali_file->alt_locerr = ODLGetDouble(msg, ALT_LOCERR, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get ALT_LOCERR");
     /* return(ierr_12); */
     cali_file->alt_locerr = -99.0;
  }

  cali_file->crt_locerr = ODLGetDouble(msg, CRT_LOCERR, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get CRT_LOCERR");
     /* return(ierr_12); */
     cali_file->crt_locerr = -99.0;
  }

  cali_file->alt_scale = ODLGetDouble(msg, ALT_SCALE, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get ALT_SCALE");
     /* return(ierr_12); */
     cali_file->alt_scale = -99.0;
  }


  cali_file->crt_scale = ODLGetDouble(msg, CRT_SCALE, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get CRT_SCALE");
     /* return(ierr_12); */
     cali_file->crt_scale = -99.0;
  }

  cali_file->dis_skew = ODLGetDouble(msg, DIS_SKEW, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get DIS_SKEW");
     /* return(ierr_12); */
     cali_file->dis_skew = -99.0;
  }

  cali_file->ori_err = ODLGetDouble(msg, ORI_ERR, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get ORI_ERR");
     /* return(ierr_12); */
     cali_file->ori_err = -99.0;
  }

/*

  cali_file->alt_m = ODLGetDouble(msg, ALT_M, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get ALT_M");
     return(ierr_12);
  }

  cali_file->crt_m = ODLGetDouble(msg, CRT_M, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get CRT_M");
     return(ierr_12);
  }

  cali_file->nesz = ODLGetDouble(msg, NESZ, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get NESZ");
     return(ierr_12);
  }

  cali_file->enl = ODLGetDouble(msg, ENL, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get ENL");
     return(ierr_12);
  }

*/

  strcpy(cali_file->cal_status, ODLGetString(msg, CAL_STATUS, &ierr));
  if (ierr != 0) {
    strcpy(cali_file->error_msg, "get_cali_file: Can't get CAL_STATUS");
    /* return(ierr_12); */
    strcpy(cali_file->cal_status, "");
  }

  strcpy(cali_file->cal_comment, ODLGetString(msg, CAL_COMMENT, &ierr));
  if (ierr != 0) {
    strcpy(cali_file->error_msg, "get_cali_file: Can't get CAL_COMMENT");
    /* return(ierr_12); */
    strcpy(cali_file->cal_comment, "");
  }
  cali_file->rel_radio_acc = ODLGetDouble(msg, REL_RADIO_ACC, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get REL_RADIO_ACC");
     /*return(ierr_12);*/
     cali_file->rel_radio_acc = -99.0;
  }

  cali_file->noise_fact = ODLGetDouble(msg, NOISE_FACT, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get NOISE_FACT");
     return(ierr_12);
  }

  cali_file->linear_conv_fact = ODLGetDouble(msg, LINEAR_CONV_FACT, &ierr);
  if (ierr != 0) {
     strcpy(cali_file->error_msg, "get_cali_file: Can't get LINEAR_CONV_FACT");
     return(ierr_12);
  }

  ODLFree(msg);

  return(0);
}
