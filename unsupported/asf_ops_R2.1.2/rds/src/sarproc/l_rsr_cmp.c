static char sccsid_l_rsr_cmp_c[] =
    "@(#)l_rsr_cmp.c	1.3 96/04/09 20:48:50";

/* l_rsr_cmp(sp,pp,fp1) -----------------------------------

**********************************************************************
*   l_rsr_cmp.c fills in the CEOS leader range spectra record        *
**********************************************************************

* add code to use the output of the 2dfft for 2 pass processing *
* dtc,sept30,92,jpl *

*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include "l_rsr.h"
#include "scene_file.h"
#include "procfil.h"
#include "procdec.h"

extern char DEF_PATH[80];	/* default files */


#define L_RSR_SIZE	2220	

extern DPP_FILE 	dpf;
extern SP_FILE 		spf;
extern EM_FILE 		emf;
extern SCENE_FILE	sf;

extern RQST_PTR 	Cur_Rqst;
extern SV		sv1,sv2;
extern TC_FILE 		tcf;


l_rsr_cmp(sp,pp,fp1)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
    FILE		*fp1;
{


FILE *fp0;
L_RSR_FILE l_rsr_buf;

int	index,cntr,mrkr,i,j,k;
short int	idata[128];
char	str_buf[100];
char	filename[80];
float   sdata[2048 * 3], minavg, maxavg;

/*Open L_RSR_FILE0 */
sprintf(filename,"%sL_RSR_FILE0",DEF_PATH);
if((fp0 = fopen(filename,"r")) == NULL){
  printf("Cannot open %s\n",filename);
  return (FAIL);
  }

/* read in the initial values for updating */
if(fread(&l_rsr_buf,L_RSR_SIZE,1,fp0)!=1){
  if(feof(fp0))
    return (FAIL);
  perror("File L_RSR_FILE0 read error\n");
  }

fclose(fp0);

/* read in the recorded spectral values */
for (i = 0; i < 128; i++)
   sf.spectra_val[i] = 0.0;
minavg = 1.0e9;
maxavg = -1.0e9;
if ((fp0 = fopen("rng_spectra","r")) == NULL){
  printf("Cannot open rng_spectra file\n");
  if(p_get_mem_file(idata,128,"rng_spec","") == FAIL) {
     printf("...Cannot open rng_spec file (for 2dfft)\n");
     for (i = 0; i < 2048 * 3; i++)
        sf.spectra_val[i] = 0.0;
  }
  else {
    for (k = 0; k < 128; k++) {
      sf.spectra_val[k] = 20.0 * log10((double)(idata[k])/512.0);
      if (sf.spectra_val[k] > maxavg)
	maxavg = sf.spectra_val[k];
      if (sf.spectra_val[k] < minavg)
	minavg = sf.spectra_val[k];
    }
  }
}
else {
if(fread(sdata,2048 * 3, sizeof(float),fp0)!=sizeof(float)){
  if(feof(fp0))
    return (FAIL);
  perror("Read error on rng_spectra file");
  }
fclose(fp0);

/* Calculate 128 values of range spectra data */
k = 0;
for (i = 0; i < 2048; i+=16) {
    for (j = 0; j < 16; j++) {
	sf.spectra_val[k] += sdata[i+j] + sdata[i+j+2048] + sdata[i+j+4096];
    }
    sf.spectra_val[k] /= 48;
    if (sf.spectra_val[k] > maxavg)
	maxavg = sf.spectra_val[k];
    if (sf.spectra_val[k] < minavg)
	minavg = sf.spectra_val[k];
    k++;
}

}




/*STD&RPR=256,CPX=256,CCSD=$*/
/*char	rsr015[8];*/	/*# of range lines integrated for spectra*/

/*char	rsr018[16];*/	/*Minimum spectral power (dB)*/
sprintf(str_buf,"%-16.7f",minavg);
   for(index=0;index<=15;index++) 
      l_rsr_buf.rsr018[index] = str_buf[index];
/*char	rsr019[16];*/	/*Maximum spectral power (dB)*/
sprintf(str_buf,"%-16.7f",maxavg);
   for(index=0;index<=15;index++) 
      l_rsr_buf.rsr019[index] = str_buf[index];

	/*SPECTRAL DATA TABLE VALUES*/
/*read in 128 (F16.7)  table values from sf->spectra_val[128]*/
/*load into rsr023[2048]*/
mrkr = 0;
for(cntr=0;cntr<128;cntr++){
      sprintf(str_buf,"%-16.7f",sf.spectra_val[cntr]);
         for(index=0;index<=15;index++) 
            l_rsr_buf.rsr023[mrkr+index] = str_buf[index];
	 mrkr = mrkr + 16; 
 }


/*write L_RSR_FILE1*/
if(fwrite(&l_rsr_buf,L_RSR_SIZE,1,fp1)!=1)
  printf("File write error\n");


return (PASS);

}
