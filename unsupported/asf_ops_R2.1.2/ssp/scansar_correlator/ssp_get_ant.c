/* SccsId[]= @(#)ssp_get_ant.c	2.41 3/24/98 */
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "asf.h"
#include "ssp_ant_odl.h"
#include "error.h"


int ssp_get_ant(file_name,
         w1_elev,w2_elev,w3_elev,s5_elev,s6_elev,s7_elev,
         w1_gain,w2_gain,w3_gain,s5_gain,s6_gain,s7_gain,
         w1_num,w2_num,w3_num,s5_num,s6_num,s7_num,
         w1_1st_elev,w2_1st_elev,w3_1st_elev,s5_1st_elev,s6_1st_elev,s7_1st_elev,
         w1_elev_inc,w2_elev_inc,w3_elev_inc,s5_elev_inc,s6_elev_inc,s7_elev_inc,
         w1_beamctr,w2_beamctr,w3_beamctr,s5_beamctr,s6_beamctr,s7_beamctr,
         azm_num,azm_inc,azm_gain,ns_ant_rg,az_peak_coef1,az_peak_coef2,az_peak_coef3)
  double *w1_elev,*w2_elev,*w3_elev,*s5_elev,*s6_elev,*s7_elev;
  double *w1_gain,*w2_gain,*w3_gain,*s5_gain,*s6_gain,*s7_gain;
  int *w1_num,*w2_num,*w3_num,*s5_num,*s6_num,*s7_num;
  double *w1_1st_elev,*w2_1st_elev,*w3_1st_elev,*s5_1st_elev,*s6_1st_elev,*s7_1st_elev;
  double *w1_elev_inc,*w2_elev_inc,*w3_elev_inc,*s5_elev_inc,*s6_elev_inc,*s7_elev_inc;
  double *w1_beamctr,*w2_beamctr,*w3_beamctr,*s5_beamctr,*s6_beamctr,*s7_beamctr;
  double *az_peak_coef1,*az_peak_coef2,*az_peak_coef3;
  int    *azm_num;
  double *azm_inc;
  double *azm_gain;
  char *file_name;
  int *ns_ant_rg;

{
  struct stat stbuf;
  char *buf;
  ODL odl,*bodyODL,msg;
  int fd, nb, ierr;
  char filename_c[60];
  int i,ii;
/***************************************/
  char ant_mode[60];
  int num_vec;
  int np_maxrow,np_maxcol;
  char buff[300];
  double az_dummy[3];
  double *vec_dummy;


 F_to_C (filename_c,file_name,60);


  if ((fd = open(filename_c, 0)) == -1) {
      printf ("can't open %s\n", filename_c);
      fflush(stdout);
      sprintf(buff,"cannot open %s during parsing %s",filename_c,filename_c);
      printclog(3,buff);
      printerr(filename_c);
      return(ierr_2);
  }
  if (fstat(fd, &stbuf) == -1) {
      printf("can't stat %s\n", filename_c);
      fflush(stdout);
      sprintf(buff,"cannot open %s during parsing %s",filename_c,filename_c);
      printclog(3,buff);
      printerr(filename_c);
      return(ierr_2);
  }
  if ((buf = (char *) malloc(stbuf.st_size)) == NULL) {
      printf("can't malloc %s\n", filename_c);
      fflush(stdout);
      sprintf(buff,"cannot malloc during parsing %s",filename_c);
      printclog(3,buff);
      return(ierr_5);
  }

  if (read(fd, buf, stbuf.st_size) != stbuf.st_size) {
      printf("can't read %d\n", stbuf.st_size);
      fflush(stdout);
      sprintf(buff,"cannot read during parsing %s",filename_c);
      printclog(3,buff);
      printerr(filename_c);
      return(ierr_4);
  }
  close(fd);

  if ((msg= StrToODL(buf, stbuf.st_size)) == NULL) {
      printf("can't parse string\n");
      fflush(stdout);
      sprintf(buff,"cannot parse %s",filename_c);
      printclog(3,buff);
      return(ierr_11);
  }
  free(buf);



     np_maxrow = 1;
     np_maxcol = *ns_ant_rg;

   for (i=0;i<np_maxcol;i++)
    {
    w1_elev[i]=0.0;
    w2_elev[i]=0.0;
    w3_elev[i]=0.0;
    s5_elev[i]=0.0;
    s6_elev[i]=0.0;
    s7_elev[i]=0.0;
    w1_gain[i]=0.0;
    w2_gain[i]=0.0;
    w3_gain[i]=0.0;
    s5_gain[i]=0.0;
    s6_gain[i]=0.0;
    s7_gain[i]=0.0;
    *w1_num    =0;
    *w2_num    =0;
    *w3_num    =0;
    *s5_num    =0;
    *s6_num    =0;
    *s7_num    =0;
     } 
    if ((bodyODL = (ODL*) Val(Lookup(msg,DETAILED_METADATA ))) == NULL)
    { 
     ODLFree(odl);
      return(ierr_12);
     }

   for (ii=0; ( odl = bodyODL[ii] ) !=NULL; ++ii) {
     if (strcasecmp(Name(odl), "ANTPTN_OBJ"))
       continue;
         az_dummy[0]= ODLGetDouble(odl, "AZ_PEAK_COEF0", &ierr);
         if (ierr != 0) return(ierr_12);
         az_dummy[1]= ODLGetDouble(odl, "AZ_PEAK_COEF1", &ierr);
         if (ierr != 0) return(ierr_12);
         az_dummy[2]= ODLGetDouble(odl, "AZ_PEAK_COEF2", &ierr);
         if (ierr != 0) return(ierr_12);
     strcpy(ant_mode,
     ODLGetString(odl, "MODE", &ierr));
     printf("ANTPTN_OBJ %s %lf %lf %lf\n",ant_mode,az_dummy[0],
     az_dummy[1],az_dummy[2]);
     fflush(stdout);
     

     if (strcmp(ant_mode, "WD1")==0)
       {
         az_peak_coef1[0]= az_dummy[0];
         az_peak_coef2[0]= az_dummy[1];
         az_peak_coef3[0]= az_dummy[2];
         if (ierr != 0) return(ierr_12);
         *w1_beamctr =         ODLGetDouble (odl, "BEAMCTR"  , &ierr);
         if (ierr != 0) return(ierr_12);
         *w1_elev_inc=         ODLGetDouble (odl, "ELEV_INCR", &ierr);
         if (ierr != 0) return(ierr_12);
         *w1_1st_elev=         ODLGetDouble (odl, "FIRST_ELEV", &ierr);
         if (ierr != 0) return(ierr_12);
         *w1_num      = ODLGetInt(odl, "NO_REC", &ierr);
         if (ierr != 0) return(ierr_12);
         np_maxrow = 1;
         np_maxcol = *w1_num;
         vec_dummy= (double *)malloc(sizeof(double)*np_maxcol);
         if (! ODLGetArrayDouble(odl, "ELEVANG_VEC",
          vec_dummy, &np_maxrow,&np_maxcol))
         return(ierr_12);
         for (i=0;i< *ns_ant_rg;i++)
          w1_elev[i]= vec_dummy[i];
         if (! ODLGetArrayDouble(odl, "GAIN_VEC",
          vec_dummy, &np_maxrow,&np_maxcol))
         return(ierr_12);
         for (i=0;i< *ns_ant_rg;i++)
          w1_gain[i]= vec_dummy[i];
         printf("load  %s %d \n",ant_mode,np_maxcol);
      fflush(stdout);
      free(vec_dummy);
  
       }
     if (strcmp(ant_mode, "WD2")==0)
       {
         az_peak_coef1[1]= az_dummy[0];
         az_peak_coef2[1]= az_dummy[1];
         az_peak_coef3[1]= az_dummy[2];
         *w2_beamctr =         ODLGetDouble (odl, "BEAMCTR"  , &ierr);
         if (ierr != 0) return(ierr_12);
         *w2_elev_inc=         ODLGetDouble (odl, "ELEV_INCR", &ierr);
         if (ierr != 0) return(ierr_12);
         *w2_1st_elev=         ODLGetDouble (odl, "FIRST_ELEV", &ierr);
         if (ierr != 0) return(ierr_12);
         *w2_num      = ODLGetInt(odl, "NO_REC", &ierr);
         if (ierr != 0) return(ierr_12);
         np_maxrow = 1;
         np_maxcol = *w2_num;
         vec_dummy= (double *)malloc(sizeof(double)*np_maxcol);
         if (! ODLGetArrayDouble(odl, "ELEVANG_VEC",
          vec_dummy, &np_maxrow,&np_maxcol))
         return(ierr_12);
         for (i=0;i< *ns_ant_rg;i++)
          w2_elev[i]= vec_dummy[i];
         if (! ODLGetArrayDouble(odl, "GAIN_VEC",
          vec_dummy, &np_maxrow,&np_maxcol))
         return(ierr_12);
         for (i=0;i< *ns_ant_rg;i++)
          w2_gain[i]= vec_dummy[i];
         printf("load  %s %d \n",ant_mode,np_maxcol);
      fflush(stdout);
      free(vec_dummy);
       }
     if (strcmp(ant_mode, "WD3")==0)
       {
         az_peak_coef1[2]= az_dummy[0];
         az_peak_coef2[2]= az_dummy[1];
         az_peak_coef3[2]= az_dummy[2];
         *w3_beamctr =         ODLGetDouble (odl, "BEAMCTR"  , &ierr);
         if (ierr != 0) return(ierr_12);
         *w3_elev_inc=         ODLGetDouble (odl, "ELEV_INCR", &ierr);
         if (ierr != 0) return(ierr_12);
         *w3_1st_elev=         ODLGetDouble (odl, "FIRST_ELEV", &ierr);
         if (ierr != 0) return(ierr_12);
         *w3_num      = ODLGetInt(odl, "NO_REC", &ierr);
         if (ierr != 0) return(ierr_12);
         np_maxrow = 1;
         np_maxcol = *w3_num;
         vec_dummy= (double *)malloc(sizeof(double)*np_maxcol);
         if (! ODLGetArrayDouble(odl, "ELEVANG_VEC",
          vec_dummy, &np_maxrow,&np_maxcol))
         return(ierr_12);
         for (i=0;i< *ns_ant_rg;i++)
          w3_elev[i]= vec_dummy[i];
         if (! ODLGetArrayDouble(odl, "GAIN_VEC",
          vec_dummy, &np_maxrow,&np_maxcol))
         return(ierr_12);
         for (i=0;i< *ns_ant_rg;i++)
          w3_gain[i]= vec_dummy[i];
         printf("load  %s %d \n",ant_mode,np_maxcol);
      fflush(stdout);
      free(vec_dummy);
       }
     if (strcmp(ant_mode, "ST5")==0)
       {
         az_peak_coef1[4]= az_dummy[0];
         az_peak_coef2[4]= az_dummy[1];
         az_peak_coef3[4]= az_dummy[2];
         *s5_beamctr =         ODLGetDouble (odl, "BEAMCTR"  , &ierr);
         if (ierr != 0) return(ierr_12);
         *s5_elev_inc=         ODLGetDouble (odl, "ELEV_INCR", &ierr);
         if (ierr != 0) return(ierr_12);
         *s5_1st_elev=         ODLGetDouble (odl, "FIRST_ELEV", &ierr);
         if (ierr != 0) return(ierr_12);
         *s5_num      = ODLGetInt(odl, "NO_REC", &ierr);
         if (ierr != 0) return(ierr_12);
         np_maxrow = 1;
         np_maxcol = *s5_num;
         vec_dummy= (double *)malloc(sizeof(double)*np_maxcol);
         if (! ODLGetArrayDouble(odl, "ELEVANG_VEC",
          vec_dummy, &np_maxrow,&np_maxcol))
         return(ierr_12);
         for (i=0;i< *ns_ant_rg;i++)
          s5_elev[i]= vec_dummy[i];
         if (! ODLGetArrayDouble(odl, "GAIN_VEC",
          vec_dummy, &np_maxrow,&np_maxcol))
         return(ierr_12);
         for (i=0;i< *ns_ant_rg;i++)
          s5_gain[i]= vec_dummy[i];
         printf("load  %s %d \n",ant_mode,np_maxcol);
      fflush(stdout);
      free(vec_dummy);
       }
     if (strcmp(ant_mode, "ST6")==0)
       {
         az_peak_coef1[5]= az_dummy[0];
         az_peak_coef2[5]= az_dummy[1];
         az_peak_coef3[5]= az_dummy[2];
         *s6_beamctr =         ODLGetDouble (odl, "BEAMCTR"  , &ierr);
         if (ierr != 0) return(ierr_12);
         *s6_elev_inc=         ODLGetDouble (odl, "ELEV_INCR", &ierr);
         if (ierr != 0) return(ierr_12);
         *s6_1st_elev=         ODLGetDouble (odl, "FIRST_ELEV", &ierr);
         if (ierr != 0) return(ierr_12);
         *s6_num      = ODLGetInt(odl, "NO_REC", &ierr);
         if (ierr != 0) return(ierr_12);
         np_maxrow = 1;
         np_maxcol = *s6_num;
         vec_dummy= (double *)malloc(sizeof(double)*np_maxcol);
         if (! ODLGetArrayDouble(odl, "ELEVANG_VEC",
          vec_dummy, &np_maxrow,&np_maxcol))
         return(ierr_12);
         for (i=0;i< *ns_ant_rg;i++)
          s6_elev[i]= vec_dummy[i];
         if (! ODLGetArrayDouble(odl, "GAIN_VEC",
          vec_dummy, &np_maxrow,&np_maxcol))
         return(ierr_12);
         for (i=0;i< *ns_ant_rg;i++)
          s6_gain[i]= vec_dummy[i];
         printf("load  %s %d \n",ant_mode,np_maxcol);
      fflush(stdout);
      free(vec_dummy);
       }
     if (strcmp(ant_mode, "ST7")==0)
       {
         az_peak_coef1[6]= az_dummy[0];
         az_peak_coef2[6]= az_dummy[1];
         az_peak_coef3[6]= az_dummy[2];
         *s7_beamctr =         ODLGetDouble (odl, "BEAMCTR"  , &ierr);
         if (ierr != 0) return(ierr_12);
         *s7_elev_inc=         ODLGetDouble (odl, "ELEV_INCR", &ierr);
         if (ierr != 0) return(ierr_12);
         *s7_1st_elev=         ODLGetDouble (odl, "FIRST_ELEV", &ierr);
         if (ierr != 0) return(ierr_12);
         *s7_num      = ODLGetInt(odl, "NO_REC", &ierr);
         if (ierr != 0) return(ierr_12);
         np_maxrow = 1;
         np_maxcol = *s7_num;
         vec_dummy= (double *)malloc(sizeof(double)*np_maxcol);
         if (! ODLGetArrayDouble(odl, "ELEVANG_VEC",
          vec_dummy, &np_maxrow,&np_maxcol))
         return(ierr_12);
         for (i=0;i< *ns_ant_rg;i++)
          s7_elev[i]= vec_dummy[i];
         if (! ODLGetArrayDouble(odl, "GAIN_VEC",
          vec_dummy, &np_maxrow,&np_maxcol))
         return(ierr_12);
         for (i=0;i< *ns_ant_rg;i++)
          s7_gain[i]= vec_dummy[i];
         printf("load  %s %d \n",ant_mode,np_maxcol);
      fflush(stdout);
      free(vec_dummy);
       }

}

   for (i=0; ( odl = bodyODL[i] ) !=NULL; ++i) {
     if (strcasecmp(Name(odl), "AZM_BEAM_OBJ"))
        continue;
     *azm_inc = ODLGetDouble (odl, "AZM_INCR"  , &ierr);
     if (ierr != 0) return(ierr_12);
     *azm_num = ODLGetInt(odl, "NO_REC", &ierr);
     if (ierr != 0) return(ierr_12);
     np_maxrow = 1;
     np_maxcol = *azm_num;
     vec_dummy= (double *)malloc(sizeof(double)*np_maxcol);
     if (! ODLGetArrayDouble(odl, "AZM_GAIN_VEC",
          vec_dummy, &np_maxrow,&np_maxcol))
         return(ierr_12);
     for (i=0;i< *ns_ant_rg;i++)
       azm_gain[i]= vec_dummy[i];
     printf("load AZMOBJ %d\n",np_maxcol);
      fflush(stdout);
     free(vec_dummy);
   }

 return(0);
}

int F_to_C (str1,str2,len)
char *str1;
char *str2;
int len;
{
  int i;
  char *dummy_char;

  dummy_char= (char *)malloc(sizeof(char)*(len+1));

  for (i=0;i<len;i++)
  {
    if (*(str2 + i) == ' ')
    {
     *(dummy_char +i)= '\0';
     break;
    }
  *(dummy_char + i) = *(str2 +i);
  }

   *(dummy_char + i)= '\0';


  strcpy(str1,dummy_char);



 free(dummy_char);



return(1);
}

