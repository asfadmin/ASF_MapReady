static char sccsid_l_dhr_cmp_c[] =
    "@(#)l_dhr_cmp.c	1.3 96/04/09 20:48:48";

/* l_dhr_cmp(sp,pp,fp1,cp) -----------------------------------

********************************************************************
*l_dhr_cmp.c fills in the CEOS leader data histogram record        *
* modified: 10/2/91, dtc, correct AR for SIS error not to CEOS spcs*
* modified: 08/17/92, dtc, correct min & max sample values for CPX *
*           and CSD.
********************************************************************

*/

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <errno.h>
#include "l_dhr.h"
#include "procfil.h"
#include "scene_file.h"
#include "procdec.h"

extern char DEF_PATH[80];	/* default files */

#define L_DHR_SIZE	4628	

extern DPP_FILE 	dpf;
extern SP_FILE 		spf;
extern EM_FILE 		emf;
extern SCENE_FILE	sf;

extern RQST_PTR 	Cur_Rqst;
extern SV		sv1,sv2;
extern TC_FILE 		tcf;

extern int	vbose;

l_dhr_cmp(sp,pp,fp1,cp)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
    FILE		*fp1;
    CEOS_INFO_PTR	cp;
{

FILE *fp0;
L_DHR_FILE l_dhr_buf;

int	i,k,index,cntr,mrkr;
int	i015,i016,i017,i018,i019,i020;
char	str_buf[100];
char	filename[80];
char	img_type[33];
float	f021,f022,f025;


/*Open L_DHR_FILE0 */
sprintf(filename,"%sL_DHR_FILE0",DEF_PATH);
if((fp0 = fopen(filename,"rb+")) == NULL){
  printf("Cannot open %s\n",filename);
  return (FAIL);
  }

/* read in the initial values for updating */
if(fread(&l_dhr_buf,L_DHR_SIZE,1,fp0)!=1){
  if(feof(fp0))
    return (FAIL);
  printf("File L_DHR_FILE0 read error\n");
  }

fclose(fp0);


/* determine the type of product*/

   if (strcmp(Cur_Rqst->type,"CSD") == 0) {
	strcpy(img_type,"SIGNAL DATA                     ");
	i015 = 5632;
	i016 = 26624;
	i017 = 5632;
	i018 = 26624;
	i019 = 5632;
	i020 = 26624;
	f021 = 0.0;
	f022 = 255.0;
	f025 = 1.0;
   }
   if (strcmp(Cur_Rqst->type,"CPX") == 0) {
	strcpy(img_type,"COMPLEX IMAGE                   ");
	i015 = 2048;
	i016 = 12800;
	i017 = 2048;
	i018 = 2048;
	i019 = 2048;
	i020 = 2048;
	f021 = -32768.0;
	f022 = 32767.0;
	f025 = 256.0;
   }
   if ((strcmp(Cur_Rqst->type,"STD") == 0) ||
       (strcmp(Cur_Rqst->type,"QLK") == 0) ||
       (strcmp(Cur_Rqst->type,"RPR") == 0))  {
	strcpy(img_type,"IMAGE AMPLITUDE                 ");
	i015 = 8192;
	i016 = 8192;
	i017 = 8192;
	i018 = 8192;
	i019 = 8192;
	i020 = 8192;
	f021 = 0.0;
	f022 = 255.0;
	f025 = 1.0;
   } 


/* For field 'i'*/
	/*HISTOGRAM TABLE DATA SET DESCRIPTION*/

/*STD & RPR = IMAGE$AMPLITUDE*/
/*CPX       = COMPLEX$IMAGE*/
/*CCSD      = SIGNAL$DATA*/
/*dhr011[32];*/		/*Histogram descriptor*/
   for(index=0;index<=31;index++)
      l_dhr_buf.dhr011[index] = img_type[index];

/*STD&RPR=8192,CPX=2048,CCSD=5632*/
/* load into dhr015[8] Total # of data samples in line directn (P)*/
sprintf(str_buf,"%8d",i015);
   for(index=0;index<=7;index++)
      l_dhr_buf.dhr015[index] = str_buf[index];

/*STD&RPR=8192,CPX=12800,CCSD=26624*/
/*dhr016[8];*/		/*Total # of data samples across lines (L)*/
sprintf(str_buf,"%8d",i016);
   for(index=0;index<=7;index++)
      l_dhr_buf.dhr016[index] = str_buf[index];

/*STD&RPR=8192,CPX=2048,CCSD=5632*/
/*dhr017[8];*/		/*Data samples group size in line directn (M)*/
sprintf(str_buf,"%8d",i017);
   for(index=0;index<=7;index++)
      l_dhr_buf.dhr017[index] = str_buf[index];

/*STD&RPR=8192,CPX=2048,CCSD=26624*/
/*dhr018[8];*/		/*Data samples group size across lines (N)*/
sprintf(str_buf,"%8d",i018);
   for(index=0;index<=7;index++)
      l_dhr_buf.dhr018[index] = str_buf[index];

/*STD&RPR=8192,CPX=2048,CCSD=5632*/
/*dhr019[8];*/		/*# of samples per group in line directn (k)*/
sprintf(str_buf,"%8d",i019);
   for(index=0;index<=7;index++)
      l_dhr_buf.dhr019[index] = str_buf[index];

/*STD&RPR=8192,CPX=2048,CCSD=26624*/
/*dhr020[8];*/		/*# of samples per group across lines (l)*/
sprintf(str_buf,"%8d",i020);
   for(index=0;index<=7;index++)
      l_dhr_buf.dhr020[index] = str_buf[index];

	/*DATA STATISTICS*/
/*STD&RPR=0,CPX=-32768,CCSD=0*/
/*dhr021[16];*/		/*Min sample value of 1st. histogram table bin*/
sprintf(str_buf,"%16.7f",f021);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr021[index] = str_buf[index];

/*STD&RPR=255,CPX=32767,CCSD=255*/
/*dhr022[16];*/		/*Max sample value of last histogram table bin*/
sprintf(str_buf,"%16.7f",f022);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr022[index] = str_buf[index];

/*convert & load into dhr023[16] Mean sample value*/
sprintf(str_buf,"%16.7f",cp->meansamp);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr023[index] = str_buf[index];

/*convert & load into dhr024[16] Standard deviation of sample value*/
sprintf(str_buf,"%16.7f",cp->stdvsamp);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr024[index] = str_buf[index];

/*STD&RPR=1,CPX=256,CCSD=1*/
/*dhr025[16];*/		/*Sample value increment*/
sprintf(str_buf,"%16.7f",f025);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr025[index] = str_buf[index];


	/*DATA HISTOGRAM STATISTICS*/

/*convert & load into dhr026[16] Minimum histogram table value*/
sprintf(str_buf,"%16.7f",cp->minfreq);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr026[index] = str_buf[index];

/*convert & load into dhr027[16] Maximum histogram table value*/
sprintf(str_buf,"%16.7f",cp->maxfreq);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr027[index] = str_buf[index];

/*convert & load into dhr028[16] Mean histogram table value*/
sprintf(str_buf,"%16.7f",cp->meanfreq);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr028[index] = str_buf[index];

/*convert & load into dhr029[16] Standard deviation of histogram table*/
sprintf(str_buf,"%16.7f",cp->stdvfreq);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr029[index] = str_buf[index];

/*dhr030[8];*/		/*Histogram table size=256*/

/*dhr031[2048];*/	/*histogram table values*/
mrkr = 0;
for(cntr=0;cntr<cp->nbins;cntr++){
      sprintf(str_buf,"%8u",cp->hstgrm[cntr]);
         for(index=0;index<=7;index++) 
            l_dhr_buf.dhr031[mrkr+index] = str_buf[index];
	 mrkr = mrkr + 8; 
 }




/* For field 'q' -- process only CSD data */


if (strcmp(Cur_Rqst->type,"CSD") == 0) {

	/*HISTOGRAM TABLE DATA SET DESCRIPTION*/

/*STD & RPR = IMAGE$AMPLITUDE*/
/*CPX       = COMPLEX$IMAGE*/
/*CCSD      = SIGNAL$DATA*/
/*dhr032[32];*/		/*Histogram descriptor*/
   for(index=0;index<=31;index++)
      l_dhr_buf.dhr032[index] = img_type[index];

/*STD&RPR=8192,CPX=2048,CCSD=5632*/
/* load into dhr036[8] Total # of data samples in line directn (P)*/
sprintf(str_buf,"%8d",i015);
   for(index=0;index<=7;index++)
      l_dhr_buf.dhr036[index] = str_buf[index];

/*STD&RPR=8192,CPX=12800,CCSD=26624*/
/*dhr037[8];*/		/*Total # of data samples across lines (L)*/
sprintf(str_buf,"%8d",i016);
   for(index=0;index<=7;index++)
      l_dhr_buf.dhr037[index] = str_buf[index];

/*STD&RPR=8192,CPX=2048,CCSD=5632*/
/*dhr038[8];*/		/*Data samples group size in line directn (M)*/
sprintf(str_buf,"%8d",i017);
   for(index=0;index<=7;index++)
      l_dhr_buf.dhr038[index] = str_buf[index];

/*STD&RPR=8192,CPX=2048,CCSD=26624*/
/*dhr039[8];*/		/*Data samples group size across lines (N)*/
sprintf(str_buf,"%8d",i018);
   for(index=0;index<=7;index++)
      l_dhr_buf.dhr039[index] = str_buf[index];

/*STD&RPR=8192,CPX=2048,CCSD=5632*/
/*dhr040[8];*/		/*# of samples per group in line directn (k)*/
sprintf(str_buf,"%8d",i019);
   for(index=0;index<=7;index++)
      l_dhr_buf.dhr040[index] = str_buf[index];

/*STD&RPR=8192,CPX=2048,CCSD=26624*/
/*dhr041[8];*/		/*# of samples per group across lines (l)*/
sprintf(str_buf,"%8d",i020);
   for(index=0;index<=7;index++)
      l_dhr_buf.dhr041[index] = str_buf[index];

	/*DATA STATISTICS*/
/*STD&RPR=0,CPX=-32768,CCSD=0*/
/*dhr042[16];*/		/*Min sample value of 1st. histogram table bin*/
sprintf(str_buf,"%16.7f",f021);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr042[index] = str_buf[index];

/*STD&RPR=255,CPX=32767,CCSD=255*/
/*dhr043[16];*/		/*Max sample value of last histogram table bin*/
sprintf(str_buf,"%16.7f",f022);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr043[index] = str_buf[index];

/*convert & load into dhr044[16] Mean sample value*/
sprintf(str_buf,"%16.7f",cp->q_meansamp);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr044[index] = str_buf[index];

/*convert & load into dhr045[16] Standard deviation of sample value*/
sprintf(str_buf,"%16.7f",cp->q_stdvsamp);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr045[index] = str_buf[index];

/*STD&RPR=1,CPX=256,CCSD=1*/
/*dhr046[16];*/		/*Sample value increment*/
sprintf(str_buf,"%16.7f",f025);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr046[index] = str_buf[index];


	/*DATA HISTOGRAM STATISTICS*/

/*convert & load into dhr047[16] Minimum histogram table value*/
sprintf(str_buf,"%16.7f",cp->q_minfreq);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr047[index] = str_buf[index];

/*convert & load into dhr048[16] Maximum histogram table value*/
sprintf(str_buf,"%16.7f",cp->q_maxfreq);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr048[index] = str_buf[index];

/*convert & load into dhr049[16] Mean histogram table value*/
sprintf(str_buf,"%16.7f",cp->q_meanfreq);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr049[index] = str_buf[index];

/*convert & load into dhr050[16] Standard deviation of histogram table*/
sprintf(str_buf,"%16.7f",cp->q_stdvfreq);
   for(index=0;index<=15;index++)
      l_dhr_buf.dhr050[index] = str_buf[index];

/*dhr051[8];*/		/*Histogram table size =256 */

/*dhr052[2048];*/	/*histogram table values*/
mrkr = 0;
for (cntr=0; cntr<cp->q_nbins; cntr++){
      sprintf(str_buf,"%8u",cp->q_hstgrm[cntr]);
         for(index=0;index<=7;index++) 
            l_dhr_buf.dhr052[mrkr+index] = str_buf[index];
	 mrkr = mrkr + 8; 
 }

} /* end if CSD -- process "q" for CSD only */



/*write L_DHR_FILE1*/
if(fwrite(&l_dhr_buf,L_DHR_SIZE,1,fp1)!=1)
  printf("File write error\n");


return (PASS);

}
