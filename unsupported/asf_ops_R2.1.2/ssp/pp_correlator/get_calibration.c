/* SccsId[]= @(#)get_calibration.c	2.41 3/24/98 */
static char sccsid_get_calibration[]= "@(#)PPget_calibration.c:2.41";

#include "pp.h"
#include "ssp_cali_odl.h"

#define ELEV_INCR "CAL_PARAM.DETAILED_METADATA.ANTPTN_OBJ.ELEV_INCR"
#define FIRST_ELEV "CAL_PARAM.DETAILED_METADATA.ANTPTN_OBJ.FIRST_ELEV"
#define NO_REC "CAL_PARAM.DETAILED_METADATA.ANTPTN_OBJ.NO_REC"
#define GAIN_VEC "CAL_PARAM.DETAILED_METADATA.ANTPTN_OBJ.GAIN_VEC"

int get_calibration(char filename[],
		    struct Calibration *calibration,
		    char error_msg[])
{
  ODL msg;
  struct stat stbuf;
  char *buf;
  int fd, ierr, maximun_rows, maximum_cols, i;

  /* open frame_request file, read the entire file into a buffer */
  if ((fd = open(filename, O_RDONLY)) == -1) {
    sprintf(error_msg, "get_calibration: open file %s failed.", filename);
    return ierr_2;
  }
  if (fstat(fd, &stbuf) == -1) {
    sprintf(error_msg, "get_calibration: checking file size failed.");
    return ierr_2;
  }
  if ((buf = (char *) malloc(stbuf.st_size)) == NULL) {
    sprintf(error_msg, "get_calibration: failed to allocate memory.");
    return ierr_5;
  }

  if (read(fd, buf, stbuf.st_size) != stbuf.st_size) {
      sprintf(error_msg, "get_calibration: read file failed.");
      return ierr_4;
  }
  close(fd);


  /* convert content of buffer to ODL */
  if ((msg= StrToODL(buf, stbuf.st_size)) == NULL) { 
      sprintf(error_msg, "get_calibration: can't parse string.");
      return ierr_11;
  }
  free(buf); 


  /* extract information from buffer */
  calibration->elev_incr = ODLGetDouble(msg, ELEV_INCR, &ierr);
  if (ierr != 0) {
    strcpy(error_msg , "get_calibration: Can't get ELEV_INCR");
    return(ierr_12);
  }

  calibration->first_elev = ODLGetDouble(msg, FIRST_ELEV, &ierr); 
  if (ierr != 0) {
    strcpy(error_msg , "get_calibration: Can't get FIRST_ELEV");
    return(ierr_12);
  }
  
  calibration->no_rec = ODLGetInt(msg, NO_REC, &ierr); 
  if (ierr != 0) {
    strcpy(error_msg , "get_calibration: Can't get NO_REC");
    return(ierr_12);
  }


  maximun_rows = 1;
  maximum_cols = calibration->no_rec;

  calibration->gain_vec = (double*) malloc(maximum_cols*sizeof(double));
  if(calibration == NULL) {
    sprintf(error_msg , "get_calibration: Can't allocate %d bytes of memory",maximum_cols*sizeof(double));
    return(ierr_12);
  }    
  if(! ODLGetArrayDouble(msg, GAIN_VEC, calibration->gain_vec, 
			 &maximun_rows, &maximum_cols)) {
    strcpy(error_msg , "get_calibration: Can't get GAIN_VEC");
    return(ierr_12);
  }
  calibration->gain_vec_length = maximum_cols;

  ODLFree(msg);
  return(0);
}

