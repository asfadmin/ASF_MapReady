static char sccsid_l_rdr_cmp_c[] =
    "@(#)l_rdr_cmp.c	1.3 96/04/09 20:48:50";

/* l_rdr_cmp(sp,pp,fp1,cp) -------------------------------------------

**********************************************************************
* l_rdr_cmp.c fills in the CEOS leader radiometric data record       *
*             mod. 1/14/92 - to compute the average of the rc_funct  *
**********************************************************************

*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include "l_rdr.h"
#include "scene_file.h"
#include "procfil.h"
#include "procdec.h"
#include "procpar.h"

extern char DEF_PATH[80];	/* default files */

#define L_RDR_SIZE	4232	

extern DPP_FILE 	dpf;
extern SP_FILE 		spf;
extern EM_FILE 		emf;
extern SCENE_FILE	sf;

extern RQST_PTR 	Cur_Rqst;
extern SV		sv1,sv2;
extern TC_FILE 		tcf;


l_rdr_cmp(sp,pp,fp1,cp)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
    FILE		*fp1;
    CEOS_INFO_PTR	cp;
{


FILE *fp0;
L_RDR_FILE l_rdr_buf;

int	index,cntr,mrkr,kount;
char	str_buf[100];
char	filename[80];
double	val_dbl;
float	val_flt;
float	a1,a2,a3;
short int d1[8192];
int	g,i,sr;
double  ga,gb,gc,val,spacing,sum;

/*Open L_RDR_FILE0 */
sprintf(filename,"%sL_RDR_FILE0",DEF_PATH);
if((fp0 = fopen(filename,"r")) == NULL){
  printf("Cannot open %s\n",filename);
  return (FAIL);
  }

/* read in the initial values for updating */
if(fread(&l_rdr_buf,L_RDR_SIZE,1,fp0)!=1){
  if(feof(fp0))
    return (FAIL);
  printf("File L_RDR_FILE0 read error\n");
  }

fclose (fp0);


/*rdr015[16];*/		/*Noise scale factor(a1)*/
sprintf(str_buf,"%-16.7e",sf.noise_scl_fac);
    for(index=0;index<=15;index++) 
        l_rdr_buf.rdr015[index] = str_buf[index];

/*rdr016[16];*/		/*Linear conversion factor(a2)*/
sprintf(str_buf,"%-16.7e",sf.lnr_conv_fac);
    for(index=0;index<=15;index++) 
        l_rdr_buf.rdr016[index] = str_buf[index];

/*rdr017[16];*/		/*Offset conversion factor(a3)*/
sprintf(str_buf,"%-16.7e",sf.off_conv_fac);
    for(index=0;index<=15;index++) 
        l_rdr_buf.rdr017[index] = str_buf[index];


	/*RADIOMETRIC DATA SET*/

/* read the radiometric compensation vector */
p_get_mem_file(d1,8192,"rc_funct","");

/* extract the desired values and convert to real */
if (strcmp(Cur_Rqst->type,"CPX") && strcmp(Cur_Rqst->type,"CSD")) {
    spacing = spf.srspace / 1000.0;
    gc = (sf.r_near - pp->r_close / 1000.0) / spacing;
    ga = ((sf.r_far - 2.0 * sf.r_mid + sf.r_near) 
	    / spacing + gc)
	    * (2.0 / (ap_rmlines * ap_rmlines));
    gb = (((sf.r_mid - sf.r_near) / spacing) - gc 
	    - (ga * ap_rmlines * ap_rmlines) / 4.0)
	    * (2.0 / ap_rmlines);
    kount = 0;
    sum = 0.0;
    for (i = g = 0; i < 256; g += 32, i++) {
	sr = g * (ga * g + gb) + gc;
	val = d1[sr] / 32768.0;
	if(val != 0.0)  {
		kount++;
		sum += val * val;
	}
	sf.radio_val[i] = val * val;
    }
    if (kount !=0) {
	sf.rcf_avg = sum/kount;
    	sf.snr = 10.0*log10( (cp->meansamp * cp->meansamp)/
	     		     (sf.noise_scl_fac * sf.rcf_avg) );
	printf("avg of rc_funct = %-16.7e\n",sf.rcf_avg);
	printf("histogram mean  = %-16.7e\n",cp->meansamp);
	printf("noise cal coeff = %-16.7e\n",sf.noise_scl_fac);
	printf("snr             = %-16.7e\n",sf.snr);
    }
}
/* for CPX, use slant range and use every 8th value */
if (strcmp(Cur_Rqst->type,"CPX") == 0) {
    kount = 0;
    sum = 0.0;
    for (i = g = 0; i < 256; g += 8, i++) {
	val = d1[g] / 32768.0;
	if(val != 0.0) {
		kount++;
		sum += val * val;
	}
	sf.radio_val[i] = val * val;
    }
    if (kount !=0) {
	sf.rcf_avg = sum/kount;
    	sf.snr = 10.0*log10( (2 * cp->meansamp * cp->meansamp)/
	     		     (sf.noise_scl_fac * sf.rcf_avg) );
    }
}
/* for CSD, leave the table empty */
if (strcmp(Cur_Rqst->type,"CSD") == 0) {
    for (i = 0; i < 256; i++) {
	sf.radio_val[i] = 0.;
    }
}


	/*LOOK UP TABLE VALUES/*
/*read in the 256  table values(F16.7) from SCENE_xxxx file*/
/*convert & load into rdr019*/
mrkr = 0;
for(cntr=0;cntr<256;cntr++){
      sprintf(str_buf,"%16.7f",sf.radio_val[cntr]);
         for(index=0;index<=15;index++) 
            l_rdr_buf.rdr019[mrkr+index] = str_buf[index];
	 mrkr = mrkr + 16; 
 }


/*write L_RDR_FILE1*/
if(fwrite(&l_rdr_buf,L_RDR_SIZE,1,fp1)!=1)
  printf("File write error\n");


return (PASS);

}
