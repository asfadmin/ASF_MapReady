/* SccsId[]= @(#)ssp_get_rep.c	2.41 3/24/98 */
/* Lawrence H. Chen 9/21/94

   This subroutine get replica data

*/

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "asf.h"
/*#include "asfcommon.h"*/
/*#include "replica.h"*/
#include "ssp_rep_odl.h"
#include "error.h"

#define HDR_BUF_SIZE 1024

union i4b4 {
      int i4;
      char b[4];
};


int  ssp_get_rep(filename,dk_rep)
char *filename;
int     dk_rep[1600];
{
  struct stat stbuf;
  char *buf, *bp, *ip;
  ODL hdr,ack;
  FILE *fp;
  int fd;
  int nb;
  int ierr,i,j,number_records;
  union i4b4 burst_number, samples, pulses;
  char filename_c[60];
  char sub_hdr[32];
  int sub_hdr_size;
  int dummy_size,dummy_ptr;
  int dk_ptr;
  char keyword[13];
  int  burst_num,length;
  int nbytes;
/*************************************/
  int       rep_hdr_size;
  int       rep_number_records;
  int       rep_prod_id;
  int       rep_proc_id;
  int       rep_frame_id;
  int       rep_rev;
  int       rep_seq;
  int       rep_start_burst;
  int       rep_end_burst;
  int       rep_burst_size;
  int       rep_data_size;
  char    rep_msg_type[60];
  char    rep_source[60];
  char    rep_destination[60];
  char    rep_date[60];
  char    rep_frame_mode[60];
  char    rep_instr_mode[60];
  char    rep_satellite[60];
  char    rep_sensor[60];
/*************************************/
  char buff[300];

  str_f_to_c(filename_c,filename,60);

  if ((fp = fopen(filename_c, "r")) == NULL) {
      printf("can't open %s\n", filename_c);
      sprintf(buff,"cannot open %s during parsing %s",filename_c,filename_c);
      printclog(3,buff);
      printerr(filename_c);
      return(ierr_2);
  }
  if (fstat(fileno(fp), &stbuf) == -1) {
      printf("can't stat %s\n", filename_c);
      sprintf(buff,"cannot open %s during parsing %s",filename_c,filename_c);
      printclog(3,buff);
      printerr(filename_c);
      return(ierr_2);
  }
  if ((buf = (char *)malloc(HDR_BUF_SIZE)) == NULL) {
      printf("can't malloc %d bytes\n", HDR_BUF_SIZE);
      sprintf(buff,"cannot malloc during parsing %s",filename_c);
      printclog(3,buff);
      return(ierr_5);
  }

  /* Read Replica Header */

  nb = HDR_BUF_SIZE;
  bp = buf;
  while (nb > 0 && fgets(bp, nb, fp) != NULL)
  {
     int n = strlen(bp);
     bp += n;
     nb -= n;
     if (strcmp(bp-n, "END\n") == 0)
	break;
  }
/*  ODLinit(); */

  rep_hdr_size = bp-buf;

  if ((hdr = StrToODL(buf, bp-buf)) == NULL) {
      printf("can't parse string\n");
      sprintf(buff,"cannot parse through %s",filename_c);
      printclog(3,buff);
      return (ierr_11);

  }

  strcpy(rep_msg_type, ODLGetString(hdr, MSG_TYPE, &ierr));
     if (ierr != 0)
      return(ierr_12);
  strcpy(rep_destination, ODLGetString(hdr, DESTINATION, &ierr));
     if (ierr != 0)
      return(ierr_12);
  strcpy(rep_source, ODLGetString(hdr, SOURCE, &ierr));
     if (ierr != 0)
      return(ierr_12);
  strcpy(rep_date, ODLGetTime(hdr, DATE, &ierr));
     if (ierr != 0)
      return(ierr_12);
  rep_number_records = ODLGetInt(hdr, NUMBER_RECORDS, &ierr);
     if (ierr != 0)
      return(ierr_12);
  rep_proc_id = ODLGetInt(hdr, PROC_ID, &ierr);
     if (ierr != 0)
      return(ierr_12);
  rep_frame_id = ODLGetInt(hdr, FRAME_ID, &ierr);
     if (ierr != 0)
      return(ierr_12);
  strcpy(rep_frame_mode, ODLGetString(hdr, FRAME_MODE, &ierr));
     if (ierr != 0)
      return(ierr_12);
  strcpy(rep_instr_mode, ODLGetString(hdr, INSTRUMENT, &ierr));
     if (ierr != 0)
      return(ierr_12);
  strcpy(rep_satellite, ODLGetString(hdr, SATELLITE, &ierr));
     if (ierr != 0)
      return(ierr_12);
  strcpy(rep_sensor, ODLGetString(hdr, SENSOR, &ierr));
     if (ierr != 0)
      return(ierr_12);
  rep_rev = ODLGetInt(hdr, REV, &ierr);
     if (ierr != 0)
      return(ierr_12);
  rep_seq = ODLGetInt(hdr, SEQ, &ierr);
     if (ierr != 0)
      return(ierr_12);
  rep_start_burst = ODLGetInt(hdr, START_BURST, &ierr);
     if (ierr != 0)
      return(ierr_12);
  rep_end_burst = ODLGetInt(hdr, END_BURST, &ierr);
     if (ierr != 0)
      return(ierr_12);
/*  rep_burst_size = ODLGetInt(hdr, BURST_SIZE, &ierr);
     if (ierr != 0)
      return(ierr_12);
  rep_data_size = ODLGetInt(hdr, DATA_SIZE, &ierr);
     if (ierr != 0)
      return(ierr_12); */



  /* read in binary data */
/*  --------------------

  iprintfLog("nb = %d\n",nb);
  if (fread(bp, 1, nb, fp) != nb) {
     iprintfLog("replica raw data file has improper format\n");
     exit(-1);
  }
*/ 

/* extract rep raw data */
/* --------------------
  ip = bp;
  for (j = 0; j < number_records; j++) {
      for (i = 0; i < 12; i++)
           iprintfLog("%1c", *(ip+i));
      iprintfLog("\n");
      for (i = 0; i < 4; i++) {
           burst_number.b[i] = *(ip+12+i);
           samples.b[i] = *(ip+16+i);
	   pulses.b[i] = *(ip+20+i);
      }

      iprintfLog("burst_number %d\n", burst_number.i4);
      iprintfLog("samples %d\n", samples.i4);
      iprintfLog("pulses %d\n", pulses.i4);

      ip += 32 + samples.i4 * pulses.i4;
  }
*/

  fclose(fp);
  free(buf);
  ODLFree(hdr);

/*  pass_rep_(filename);*/
  if ((fd = open(filename_c, 0)) == NULL) {
      printf("can't open %s, %s\n", filename_c);
      return(ierr_2);
  }

  sub_hdr_size = 32;
  dk_ptr = rep_hdr_size;

  for (i=rep_start_burst;i<=rep_end_burst;i++)
   {
     dummy_size=12;
     nbytes = cread(&fd, keyword, &dummy_size, &dk_ptr);
     dummy_size=4;
     dummy_ptr =dk_ptr+12;
     nbytes = cread(&fd, &burst_num, &dummy_size, &dummy_ptr);
     dummy_size=4;
     dummy_ptr =dummy_ptr+4;
     nbytes = cread(&fd, &length, &dummy_size, &dummy_ptr);
/*     printf("%s %d %d\n",keyword,burst_num,length);*/
     dk_ptr=dk_ptr+sub_hdr_size;
     dk_rep[burst_num-1]=dk_ptr;
/*     printf("%d %d %d\n",(burst_num-1),dk_ptr,length);*/
     dk_ptr=dk_ptr+length;
   

      
   }

  close(fd);

  return(0);

}  
