static char sccsid_l_dqs_cmp_c[] =
    "@(#)l_dqs_cmp.c	1.3 96/04/09 20:48:48";

/* l_dqs_cmp(sp,pp,fp1) ------------------------------------------

*********************************************************************
*l_dqs_cmp.c fills in the CEOS leader data quality summary record   *
*********************************************************************

*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "l_dqs.h"
#include "procfil.h"
#include "scene_file.h"
#include "procdec.h"

extern char DEF_PATH[80];	/* default files */

#define L_DQS_SIZE	830	

extern DPP_FILE 	dpf;
extern SP_FILE 		spf;
extern EM_FILE 		emf;
extern SCENE_FILE	sf;

extern RQST_PTR 	Cur_Rqst;
extern SV		sv1,sv2;
extern TC_FILE 		tcf;


l_dqs_cmp(sp,pp,fp1)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
    FILE		*fp1;
{

FILE *fp0;
L_DQS_FILE l_dqs_buf;

int	index;
char	str_buf[32];
char	filename[80];
double	val_dbl;
float	val_flt;


/*Open L_DQS_FILE0 */
sprintf(filename,"%sL_DQS_FILE0",DEF_PATH);
if((fp0 = fopen(filename,"r")) == NULL){
  printf("Cannot open %s\n",filename);
  return (FAIL);
  }

/* read in the initial values for updating */
if(fread(&l_dqs_buf,L_DQS_SIZE,1,fp0)!=1){
  if(feof(fp0))
    return (FAIL);
  printf("File L_DQS_FILE0 read error\n");
  }

fclose(fp0);

/*to be manually entered during operations phase*/
/*dqs009[6];*/		/*Date of last calibration update as YYMMDD*/
			/*YY = last two digits of year*/
			/*MM = month of the year*/
			/*DD = day of the month*/

/*ABSOLUTE RADIOMETRIC DATA QUALITY*/
/*extract pp->islr*/
/*convert & load into dqs011[16] Nominal ISLR (dB)*/
sprintf(str_buf,"%-16.7f",pp->islr);
   for(index=0;index<=15;index++)
      l_dqs_buf.dqs011[index] = str_buf[index];



/*extract pp->pslr*/
/*convert & load into dqs012[16] Nominal PSLR (dB)*/
sprintf(str_buf,"%-16.7f",pp->pslr);
   for(index=0;index<=15;index++)
      l_dqs_buf.dqs012[index] = str_buf[index];

/*extract sf.snr*/
/*convert & load into dqs015[16] Estimate of SNR (from range spectra)*/
sprintf(str_buf,"%-16.7f",sf.snr);
   for(index=0;index<=15;index++)
      l_dqs_buf.dqs015[index] = str_buf[index];

/*extract pp->ber*/
/*convert & load into dqs016[16] Actual Bit Error Rate (BER)*/
sprintf(str_buf,"%-16.7f",pp->ber);
   for(index=0;index<=15;index++)
      l_dqs_buf.dqs016[index] = str_buf[index];


/*write L_DQS_FILE1*/
if(fwrite(&l_dqs_buf,L_DQS_SIZE,1,fp1)!=1)
  printf("File write error\n");


return (PASS);

}
