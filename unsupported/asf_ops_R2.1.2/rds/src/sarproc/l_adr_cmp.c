static char sccsid_l_adr_cmp_c[] =
    "@(#)l_adr_cmp.c	1.3 96/04/09 20:48:47";

/*  l_adr_cmp(sp,pp,fp1)  ---------------------------------

******************************************************************
*l_adr_cmp.c fills in the CEOS leader file attitude data record  *
******************************************************************

*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "l_adr.h"
#include "procfil.h"
#include "scene_file.h"
#include "procdec.h"

#define L_ADR_SIZE	376	

extern char DEF_PATH[80];	/* default files */

extern DPP_FILE 	dpf;
extern SP_FILE 		spf;
extern EM_FILE 		emf;
extern SCENE_FILE	sf;

extern RQST_PTR 	Cur_Rqst;
extern SV 		sv1,sv2;
extern TC_FILE 		tcf;


l_adr_cmp(sp,pp,fp1)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
    FILE		*fp1;
{

FILE *fp0;
L_ADR_FILE l_adr_buf;

int	index,mlsc;
char	str_buf[32];
char	filename[80];
double	val_dbl;
float	val_flt;


/*Open L_ADR_FILE0 */
sprintf(filename,"%sL_ADR_FILE0",DEF_PATH);
if((fp0 = fopen(filename,"r")) == NULL){
  printf("Cannot open L_ADR_FILE0\n");
  return (FAIL);
  }

/* read in the initial values for updating */
if(fread(&l_adr_buf,L_ADR_SIZE,1,fp0)!=1){
  if(feof(fp0))
    return (FAIL);
  printf("File L_ADR_FILE0 read error\n");
  }

fclose(fp0);


	/*Attitude data set*/
/*extract pp->sv.gmt.day*/
/*load into adr008[4] Day of the year (GMT)*/
sprintf(str_buf,"%4u",pp->sv.gmt.day);
   for(index=0;index<=3;index++)
      l_adr_buf.adr008[index] = str_buf[index];


/*extract L_PPD_FILE_PTR->ppd019*/
/*calculate & load into adr009[8] Millisecond of day (GMT)*/
mlsc = pp->sv.gmt.hr*3600000 + pp->sv.gmt.min*60000 +
       (int) (pp->sv.gmt.second*1000.0);
sprintf(str_buf,"%8u",mlsc);
   for(index=0;index<=7;index++)
      l_adr_buf.adr009[index] = str_buf[index];

/*extract pp->att.pitch*/
/*convert & load into adr013[14] Pitch (degrees)*/
sprintf(str_buf,"%-14.6f",pp->att.pitch);
   for(index=0;index<=13;index++)
      l_adr_buf.adr013[index] = str_buf[index];


/*extract pp->att.roll*/
/*convert & load into adr014[14] Roll (degrees)*/
sprintf(str_buf,"%-14.6f",pp->att.roll);
   for(index=0;index<=13;index++)
      l_adr_buf.adr014[index] = str_buf[index];


/*extract pp->att.yaw*/
/*convert & load into adr015[14] Yaw (degrees)*/
sprintf(str_buf,"%-14.6f",pp->att.yaw);
   for(index=0;index<=13;index++)
      l_adr_buf.adr015[index] = str_buf[index];


/*extract sf.pitch_rate*/
/*convert & load into adr019[14] Pitch rate (degrees/sec)*/
sprintf(str_buf,"%-14.6f",sf.pitch_rate);
   for(index=0;index<=13;index++)
      l_adr_buf.adr019[index] = str_buf[index];


/*extract sf->roll_rate*/
/*convert & load into adr020[14] Roll rate (degrees/sec)*/
sprintf(str_buf,"%-14.6f",sf.roll_rate);
   for(index=0;index<=13;index++)
      l_adr_buf.adr020[index] = str_buf[index];

/*extract sf->yaw_rate*/
/*convert & load into adr021[14] Yaw rate (degrees/sec)*/
sprintf(str_buf,"%-14.6f",sf.yaw_rate);
   for(index=0;index<=13;index++)
      l_adr_buf.adr021[index] = str_buf[index];


/*write L_ADR_FILE1*/
if(fwrite(&l_adr_buf,L_ADR_SIZE,1,fp1)!=1)
  printf("File write error\n");

return (PASS);

}
