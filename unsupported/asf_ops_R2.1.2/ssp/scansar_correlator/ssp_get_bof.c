/* SccsId[]= @(#)ssp_get_bof.c	2.41 3/24/98 */
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
#include "ssp_bof_odl.h"
#include "error.h"

int ssp_get_bof(filename,nbursts,nbeams,dk_ptr_skip,dk_size)
char *filename;
int *nbursts,*nbeams;
int *dk_ptr_skip;
int dk_size [1600];
{
  struct stat stbuf;
  char *buf, **body_key;
  ODL msg, odl, *burst;
  int fd, nb, ierr;
  int dummy = 3;
  int t_burst, i;
  char filename_c[60];
  ODL byte_offset_odl;
  ODL *vector_offset;
/*************************************/
  char buff[300];
  


  str_f_to_c(filename_c,filename,60);
  printf("FILENAME IS %s\n",filename_c);



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



  *nbursts = ODLGetInt(msg, N_BURSTS, &ierr);
  *nbeams  = ODLGetInt(msg, N_BEAMS , &ierr);

  byte_offset_odl  = Lookup(msg,BYTE_OFFSETS);

  vector_offset    = (ODL *) Val(byte_offset_odl);

  for (i=0;i< *nbursts;i++)
    {
      dk_size [i]=  *(int *)Val(vector_offset[i+1]) -  *(int *)Val(vector_offset[i]);
    }

   *dk_ptr_skip  =  *(int *)Val(vector_offset[0]); 


  ODLFree(msg);
  return(0);
}
