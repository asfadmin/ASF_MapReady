/* SccsId[]= @(#)get_ephemeris.c	2.41 3/24/98 */
static char sccsid_get_ephemeris[]= "@(#)PPget_ephemeris.c:2.41";

#include "pp.h"
#include "ssp_eph_odl.h"

int get_ephemeris(char filename[],
		  struct Ephemeris *ephemeris, 
		  char error_msg[])
{
  ODL msg;
  struct stat stbuf;
  char *buf;
  int fd, ierr;

  /* open frame_request file, read the entire file into a buffer */
  if ((fd = open(filename, O_RDONLY)) == -1) {
    sprintf(error_msg, "get_ephemeris: open file %s failed.",filename);
    return ierr_2;
  }
  if (fstat(fd, &stbuf) == -1) {
    sprintf(error_msg, "get_ephemeris: checking file size failed.");
    return ierr_2;
  }
  if ((buf = (char *) malloc(stbuf.st_size)) == NULL) {
    sprintf(error_msg, "get_ephemeris: allocating memory failed.");
    return ierr_5;
  }

  if (read(fd, buf, stbuf.st_size) != stbuf.st_size) {
      sprintf(error_msg, "get_ephemeris: reading file failed.");
      return ierr_4;
  }
  close(fd);


  /* convert content of buffer to ODL */
  if ((msg= StrToODL(buf, stbuf.st_size)) == NULL) { 
      sprintf(error_msg, "get_ephemeris: can't parse string.");
      return ierr_11;
  }
  free(buf); 


  /********* extract information from buffer *********/
  strcpy(ephemeris->eph_time, ODLGetTime(msg, BURST_TIME, &ierr));
  if (ierr != 0) {
    strcpy(error_msg , "get_ephemeris: Can't get BURST_TIME");
    return(ierr_12);
  }

  ODLFree(msg);
  return(0);
}
