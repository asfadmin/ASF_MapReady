/* SccsId[]= @(#)dump_odl_to_disk.c	2.41 3/24/98 */
#include <stdio.h>
#include "odl.h"
#include "error.h"


int dump_odl_to_disk(filename,odl)
char *filename;
ODL odl;
{
/*******************************/
    char *sss,*sss1,*sss2,*sss3;
    FILE *fp;
    char buff[300];


  if ((fp=fopen(filename,"w"))== NULL)
    {
     printf("cannot open %s\n",filename);
     fflush(stdout);
     sprintf(buff,"cannot open %s\n",filename);
     printclog(1,buff);
     printerr(filename);
     return(ierr_2);
    }
/***************************************/
/*  sss= (char *)malloc(sizeof(char)*10000);*/
  sss=  ODLToStr(odl,NULL);
/*  sss1=strstr(sss,"SPS_FRAME_REQUEST");
  sss2=strstr(&sss1[28],"OBJECT = BODY");
  sss3=strstr(&sss2[13],"END_OBJECT = BODY");
  sss3[17]=NULL;

  fprintf(fp,"%s\n",sss2);*/



  sss1=strstr(sss,"OBJECT = SPS_FRAME_REQUEST");
  sss2=strstr(sss,"END_OBJECT = SPS_FRAME_REQUEST");
  sss2[30]=NULL;
  fprintf(fp,"%s\n",sss1);


  fclose(fp);
  printf("FINISHED DUMP ODL FILE\n");
  fflush(stdout);










/***************************************/





 return(0);
}
