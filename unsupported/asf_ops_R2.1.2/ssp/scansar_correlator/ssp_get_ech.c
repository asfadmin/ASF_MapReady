/* SccsId[]= @(#)ssp_get_ech.c	2.41 3/24/98 */
/* Lawrence H. Chen 9/21/94

   This subroutine get echo data

*/

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "asf.h"
/*#include "asfcommon.h"*/
#include "echo.h"
#include "ssp_ech_odl.h"

#define HDR_BUF_SIZE 1024

union i4b4 {
      int i4;
      char b[4];
};


int  ssp_get_ech(char *filename,char *error_msg) 
/* void ssp_get_ech(char *filename) */
{
  struct stat stbuf;
  char *buf, *bp, *ip;
  ODL hdr , ack;
  FILE *fp;
  int nb;
  int ierr,i,j,number_records;
  union i4b4 burst_number, samples, pulses;
  extern int pass_ech_();

  iprintfLog("SSP_GET_ECH (c) : \n");
  iprintfLog("filename : %s\n", filename);

  if ((fp = fopen(filename, "r")) == NULL) {
      iprintfLog("can't open %s, %s\n", filename, strerror(errno));
      error_msg= "Disk Open Error:RDS-Echo";
      return  -1 ;
  }
  if (fstat(fileno(fp), &stbuf) == -1) {
      iprintfLog("can't stat %s, %s\n", filename, strerror(errno));
      error_msg= "Read Disk File Status  :RDS-Echo";
      return  -1 ;
  }
  if ((buf = (char *)malloc(HDR_BUF_SIZE)) == NULL) {
      iprintfLog("can't malloc %d bytes\n", HDR_BUF_SIZE);
      error_msg= "Memo Alloc Error :RDS-Echo";
      return  -1 ;
  }

  /* Read  Header */

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

  echo_hdr_.hdr_size = bp-buf;

  if ((hdr = StrToODL(buf, bp-buf)) == NULL) {
     iprintfLog("can't convert header to ODL\n");
     exit(-1);
  }

  strcpy(echo_hdr_.msg_type, ODLGetString(hdr, MSG_TYPE, &ierr));
  strcpy(echo_hdr_.destination, ODLGetString(hdr, DESTINATION, &ierr));
  strcpy(echo_hdr_.source, ODLGetString(hdr, SOURCE, &ierr));
  strcpy(echo_hdr_.date, ODLGetTime(hdr, DATE, &ierr));
  echo_hdr_.number_records = ODLGetInt(hdr, NUMBER_RECORDS, &ierr);
  echo_hdr_.proc_id = ODLGetInt(hdr, PROC_ID, &ierr);
  echo_hdr_.frame_id = ODLGetInt(hdr, FRAME_ID, &ierr);
  strcpy(echo_hdr_.frame_mode, ODLGetString(hdr, FRAME_MODE, &ierr));
  strcpy(echo_hdr_.instr_mode, ODLGetString(hdr, INSTRUMENT, &ierr));
  strcpy(echo_hdr_.satellite, ODLGetString(hdr, SATELLITE, &ierr));
  strcpy(echo_hdr_.sensor, ODLGetString(hdr, SENSOR, &ierr));
  echo_hdr_.rev = ODLGetInt(hdr, REV, &ierr);
  echo_hdr_.seq = ODLGetInt(hdr, SEQ, &ierr);
  echo_hdr_.start_burst = ODLGetInt(hdr, START_BURST, &ierr);
  echo_hdr_.end_burst = ODLGetInt(hdr, END_BURST, &ierr);

/*
  printf("-------------->\n");
  printf("start_burst %d\n", echo_hdr_.start_burst);
  printf("end_burst %d\n", echo_hdr_.end_burst);
*/

  number_records = ODLGetInt(hdr, NUMBER_RECORDS, &ierr);

  /* read in binary data */
/*
  iprintfLog("nb = %d\n",nb);
  if (fread(bp, 1, nb, fp) != nb) {
     iprintfLog("echo raw data file has improper format\n");
     ssp_error(ack, "Disk Read Error :RDS-Aux");
      exit( -1 );
  }
*/
/* extract echo raw data for debug
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

  pass_ech_(filename);

  iprintfLog("END of SSP_GET_ECH\n");  
}  
