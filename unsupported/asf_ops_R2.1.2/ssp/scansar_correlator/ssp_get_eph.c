/* SccsId[]= @(#)ssp_get_eph.c	2.41 3/24/98 */
/* Lawrence H. Chen 9/21/94

   This subroutine extract ephemeris data from disk file

*/

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "asf.h"
/*#include "asfcommon.h"*/
/*#include "ephemeris.h"*/
#include "ssp_eph_odl.h"
#include "error.h"

int ssp_get_eph(filename)
char *filename;
{
  struct stat stbuf;
  char *buf, **body_key;
  ODL msg, odl, *burst;
  int fd, nb, ierr;
  int dummy = 3;
  int t_burst, i;
  char filename_c[60];
/*************************************/
  char eph_hdr_msg_type[60];
  char eph_hdr_destination[60];
  char eph_hdr_source[60];
  char eph_hdr_date[60];
  int eph_hdr_number_records;
  int eph_hdr_proc_id;
  int eph_hdr_frame_id;
  char eph_hdr_frame_mode[60];
  char eph_hdr_instr_mode[60];
  char eph_hdr_satellite[60];
  char eph_hdr_sensor[60];
  int eph_hdr_rev;
  int eph_hdr_seq;
  int eph_hdr_start_burst;
  int eph_hdr_end_burst;
  int eph_hdr_burst_size;
  int eph_hdr_data_size;
  char eph_data_valid[1600][60];
  int eph_burst[1600];
  int eph_beam_select[1600];
  int eph_revolution[1600];
  char eph_time[1600][60];
  double eph_greenwich_angle[1600];
  double eph_a[1600];
  double eph_h[1600];
  double eph_k[1600];
  double eph_p[1600];
  double eph_q[1600];
  double eph_l[1600];
  double eph_drag[1600];
/*************************************/
  char buff[300];



  str_f_to_c(filename_c,filename,60);



  if ((fd = open(filename_c, 0)) == -1) {
      printf ("can't open %s\n", filename_c);
      sprintf(buff,"cannot open %s during parsing %s",filename_c,filename_c);
      printclog(3,buff);
      printerr(filename_c);
      return(ierr_2);
  }
  if (fstat(fd, &stbuf) == -1) {
      printf("can't stat %s\n", filename_c);
      sprintf(buff,"cannot open %s during parsing %s",filename_c,filename_c);
      printclog(3,buff);
      printerr(filename_c);
      return(ierr_2);
  }
  if ((buf = (char *) malloc(stbuf.st_size)) == NULL) {
      printf("can't malloc %d bytes\n", stbuf.st_size);
      sprintf(buff,"cannot malloc during parsing %s",filename_c);
      printclog(3,buff);
      return(ierr_5);
  }
  if (read(fd, buf, stbuf.st_size) != stbuf.st_size) {
      printf("can't read %d\n", stbuf.st_size);
      sprintf(buff,"cannot read during parsing %s",filename_c);
      printclog(3,buff);
      printerr(filename_c);
      return(ierr_4);
  }
  close(fd);



  if ((msg= StrToODL(buf, stbuf.st_size)) == NULL) {
      printf("can't parse string\n");
      sprintf(buff,"cannot parse %s",filename_c);
      printclog(3,buff);
      return (ierr_11);
  }
  free(buf);

  strcpy(eph_hdr_msg_type, ODLGetString(msg, MSG_TYPE, &ierr));
     if (ierr != 0)
      return(ierr_12);
  strcpy(eph_hdr_destination, ODLGetString(msg, DESTINATION, &ierr));
     if (ierr != 0)
      return(ierr_12);
  strcpy(eph_hdr_source, ODLGetString(msg, SOURCE, &ierr));
     if (ierr != 0)
      return(ierr_12);
  strcpy(eph_hdr_date, ODLGetTime(msg, DATE, &ierr));
     if (ierr != 0)
      return(ierr_12);
  eph_hdr_number_records = ODLGetInt(msg, NUMBER_RECORDS, &ierr);
     if (ierr != 0)
      return(ierr_12);
  eph_hdr_proc_id = ODLGetInt(msg, PROC_ID, &ierr);
     if (ierr != 0)
      return(ierr_12);
  eph_hdr_frame_id = ODLGetInt(msg, FRAME_ID, &ierr);
     if (ierr != 0)
      return(ierr_12);
  strcpy(eph_hdr_frame_mode, ODLGetString(msg, FRAME_MODE, &ierr));
     if (ierr != 0)
      return(ierr_12);
  strcpy(eph_hdr_instr_mode, ODLGetString(msg, INSTRUMENT, &ierr));
     if (ierr != 0)
      return(ierr_12);
  strcpy(eph_hdr_satellite, ODLGetString(msg, SATELLITE, &ierr));
     if (ierr != 0)
      return(ierr_12);
  strcpy(eph_hdr_sensor, ODLGetString(msg, SENSOR, &ierr));
     if (ierr != 0)
      return(ierr_12);
  eph_hdr_rev = ODLGetInt(msg, REV, &ierr);
     if (ierr != 0)
      return(ierr_12);
  eph_hdr_seq = ODLGetInt(msg, SEQ, &ierr);
     if (ierr != 0)
      return(ierr_12);
  eph_hdr_start_burst = ODLGetInt(msg, START_BURST, &ierr);
     if (ierr != 0)
      return(ierr_12);
  eph_hdr_end_burst = ODLGetInt(msg, END_BURST, &ierr);
     if (ierr != 0)
      return(ierr_12);
/*  eph_hdr_burst_size = ODLGetInt(msg, BURST_SIZE, &ierr);
     if (ierr != 0)
      return(ierr_12);
  eph_hdr_data_size = ODLGetInt(msg, DATA_SIZE, &ierr);
     if (ierr != 0)
      return(ierr_12);*/

  if (! (burst = (ODL*) Val(Lookup(msg, "EPHEMERIS_FILE.BODY")))) {
     return(ierr_12);
  }

  for (i = 0; (odl = burst[i]) != NULL; ++i)
  {
     char *p;
     if (strcasecmp(Name(odl), "BURST_INFO"))
        continue;

     t_burst = ODLGetInt(odl, "BURST", &ierr) - 1;
     if (ierr != 0)
      return(ierr_12);


     eph_burst[t_burst] = ODLGetInt(odl,"BURST", &ierr);
     if (ierr != 0)
      return(ierr_12);

     strcpy(&eph_data_valid[t_burst][0],ODLGetString(odl,"DATA_VALID", &ierr));
     if (ierr != 0)
      return(ierr_12);

     eph_beam_select[t_burst] = ODLGetInt(odl,"BEAM_SELECT", &ierr);
     if (ierr != 0)
      return(ierr_12);

     eph_revolution[t_burst] = ODLGetInt(odl,"REVOLUTION", &ierr);
     if (ierr != 0)
      return(ierr_12);


     strcpy(&eph_time[t_burst][0],ODLGetTime(odl,"TIME", &ierr));
     if (ierr != 0)
      return(ierr_12);


     eph_greenwich_angle[t_burst] = 
       ODLGetDouble(odl,"GREENWICH_ANGLE", &ierr);
     if (ierr != 0)
      return(ierr_12);


     eph_a[t_burst] = ODLGetDouble(odl,"SC_EPHEMERIS_A", &ierr);
     if (ierr != 0)
      return(ierr_12);

     eph_h[t_burst] = ODLGetDouble(odl,"SC_EPHEMERIS_H", &ierr);
     if (ierr != 0)
      return(ierr_12);


     eph_k[t_burst] = ODLGetDouble(odl,"SC_EPHEMERIS_K", &ierr);
     if (ierr != 0)
      return(ierr_12);

     eph_p[t_burst] = ODLGetDouble(odl,"SC_EPHEMERIS_P", &ierr);
     if (ierr != 0)
      return(ierr_12);

     eph_q[t_burst] = ODLGetDouble(odl,"SC_EPHEMERIS_Q", &ierr);
     if (ierr != 0)
      return(ierr_12);

     eph_l[t_burst] = ODLGetDouble(odl,"SC_EPHEMERIS_L", &ierr);
     if (ierr != 0)
      return(ierr_12);

     eph_drag[t_burst] = ODLGetDouble(odl,"DRAG", &ierr);
     if (ierr != 0)
      return(ierr_12);
  }


  ODLFree(msg);
  return(0);
}
