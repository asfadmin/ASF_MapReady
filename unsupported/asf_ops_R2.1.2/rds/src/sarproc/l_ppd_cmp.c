static char sccsid_l_ppd_cmp_c[] =
    "@(#)l_ppd_cmp.c	1.3 96/04/09 20:48:49";

/* l_ppd_cmp(sp,pp,fp1) ----------------------------------------

**********************************************************************
* l_ppd_cmp.c fills in the CEOS leader file platform position data   *
* record 		             	                             *
**********************************************************************

*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "l_ppd.h"
#include "scene_file.h"
#include "procdec.h"
#include "procfil.h"

extern char DEF_PATH[80];	/* default files */

#define L_PPD_SIZE	782

extern DPP_FILE 	dpf;
extern SP_FILE 		spf;
extern EM_FILE 		emf;
extern SCENE_FILE	sf;

extern RQST_PTR 	Cur_Rqst;
extern SV		sv1,sv2;
extern TC_FILE 		tcf;


l_ppd_cmp(sp,pp,fp1)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
    FILE		*fp1;
{

FILE *fp0;
L_PPD_FILE l_ppd_buf;

int	index,istat;
char	str_buf[32];
char	filename[80];
double	val_dbl;
float	val_flt,seconds,mlsc;
int	year,day,hour,minute,month,day_month;
double	get_gha(),ghangle,d_seconds;
double	ctok(),kepler[6];
GMT	agmt;


/*Open L_PPD_FILE0 */
sprintf(filename,"%sL_PPD_FILE0",DEF_PATH);
if((fp0 = fopen(filename,"r")) == NULL){
  printf("Cannot open %s\n",filename);
  return (FAIL);
  }

/* read in the initial values for updating */
if(fread(&l_ppd_buf,L_PPD_SIZE,1,fp0)!=1){
  if(feof(fp0))
    return (FAIL);
  printf("File L_PPD_FILE0 read error\n");
  }

fclose(fp0);

/*compute the Keplerian elements from the first state vector*/
ctok(sf.x_begin,sf.y_begin,sf.z_begin,sf.vx_begin,sf.vy_begin,
	sf.vz_begin,kepler);

/*convert & load into ppd008[16] 1st orbital element*/
sprintf(str_buf,"%-16.7e",kepler[0]);
   for(index=0;index<=15;index++)
      l_ppd_buf.ppd008[index] = str_buf[index];

/*convert & load into ppd009[16] 2nd orbital element*/
sprintf(str_buf,"%-16.7e",kepler[1]);
   for(index=0;index<=15;index++)
      l_ppd_buf.ppd009[index] = str_buf[index];

/*convert & load into ppd010[16] 3rd orbital element*/
sprintf(str_buf,"%-16.7e",kepler[2]);
   for(index=0;index<=15;index++)
      l_ppd_buf.ppd010[index] = str_buf[index];

/*convert & load into ppd011[16] 4th orbital element*/
sprintf(str_buf,"%-16.7e",kepler[3]);
   for(index=0;index<=15;index++)
      l_ppd_buf.ppd011[index] = str_buf[index];

/*convert & load into ppd012[16] 5th orbital element*/
sprintf(str_buf,"%-16.7e",kepler[4]);
   for(index=0;index<=15;index++)
      l_ppd_buf.ppd012[index] = str_buf[index];

/*convert & load into ppd013[16] 6th orbital element*/
sprintf(str_buf,"%-16.7e",kepler[5]);
   for(index=0;index<=15;index++)
      l_ppd_buf.ppd013[index] = str_buf[index];

/* extract time from pp_regions and convert to desired form */
strncpy(str_buf, sf.tme_scene_ctr, 4);
str_buf[4] = '\0';
	agmt.yr  = atoi(str_buf);
strncpy(str_buf, &sf.tme_scene_ctr[4], 3);
str_buf[3] = '\0';
	agmt.day = atoi(str_buf);
strncpy(str_buf, &sf.tme_scene_ctr[8], 2);
str_buf[2] = '\0';
	agmt.hr  = atoi(str_buf);
strncpy(str_buf, &sf.tme_scene_ctr[11], 2);
str_buf[2] = '\0';
	agmt.min = atoi(str_buf);
strncpy(str_buf, &sf.tme_scene_ctr[14], 2);
str_buf[2] = '\0';
	agmt.second = (float) (atoi(str_buf));
strncpy(str_buf, &sf.tme_scene_ctr[17], 3);
str_buf[3] = '\0';
	mlsc = (float) (atoi(str_buf));
	agmt.second += mlsc/1000.0;

/* subtract half image length to get time of first vector */
d_seconds = 102400.0 /(2.0 * pp->swth_speed);
add_seconds(&agmt,-d_seconds);
time_brk(&agmt,&year,&day,&hour,&minute,&seconds);
get_gha_(&year,&day,&hour,&minute,&seconds,&ghangle);
get_month_day(year,day,&month,&day_month);
d_seconds = hour*3600.0 + minute*60.0 + seconds; /* seconds in day */

/*convert & load into ppd015[4] Year of data point. (YYYY)*/
sprintf(str_buf,"%4u",year);
   for(index=0;index<=3;index++)
      l_ppd_buf.ppd015[index] = str_buf[index];

/*convert & load into ppd016[4] Month of data point. (MM)*/
sprintf(str_buf,"%4u",month);
   for(index=0;index<=3;index++)
      l_ppd_buf.ppd016[index] = str_buf[index];

/*convert & load into ppd017[4] Day (in month) of data point. (DD)*/
sprintf(str_buf,"%4u",day_month);
   for(index=0;index<=3;index++)
      l_ppd_buf.ppd017[index] = str_buf[index];

/*convert & load into ppd018[4] Day in the year (GMT)*/
sprintf(str_buf,"%4u",day);
   for(index=0;index<=3;index++)
      l_ppd_buf.ppd018[index] = str_buf[index];

/*convert & load into ppd019[22] Seconds of day (GMT) of data*/
sprintf(str_buf,"%-22.15f",d_seconds);
   for(index=0;index<=21;index++)
      l_ppd_buf.ppd019[index] = str_buf[index];

/*extract time interval from pp->swth_speed*/
/*convert & load into ppd020[22] time interval between points*/
d_seconds = 102400.0 /(2.0 * pp->swth_speed);
if(strcmp(Cur_Rqst->type,"CPX") == 0) d_seconds /= 2.0;
sprintf(str_buf,"%-22.15f",d_seconds);
   for(index=0;index<=21;index++)
      l_ppd_buf.ppd020[index] = str_buf[index];

/*ppd022[22];*/	/*Greenwich mean hour angle (degrees)*/
sprintf(str_buf,"%-22.15f",ghangle);
   for(index=0;index<=21;index++)
      l_ppd_buf.ppd022[index] = str_buf[index];

/*STD=60,QL=920*/
/*ppd023[16];*/	/*Along track position error (meters)*/

/*STD=15,QL=15*/
/*ppd024[16];*/	/*Across track position error (meters)*/

/*STD=25,QL=30*/
/*ppd025[16];*/	/*Radial position error (meters/sec)*/

/*STD=0.027,QL=0.029*/
/*ppd026[16];*/	/*Along track velocity error (meters/sec)*/

/*STD=0.015,QL=0.016*/
/*ppd027[16];*/	/*Across track velocity error (meters/sec)*/

/*STD=0.040,QL=0.995*/
/*ppd028[16];*/	/*Radial velocity error (degrees/sec)*/


	/*First positional data point-(format D22.15)*/
/*extract sf.x_begin,sf.y_begin,sf.z_begin*/
/*convert & load into ppd029[66] 1st data point position vector*/
sprintf(str_buf,"%-22.15f",sf.x_begin);
   for(index=0;index<=21;index++)
      l_ppd_buf.ppd029[index] = str_buf[index];
sprintf(str_buf,"%-22.15f",sf.y_begin);
   for(index=22;index<=43;index++)
      l_ppd_buf.ppd029[index] = str_buf[index-22];
sprintf(str_buf,"%-22.15f",sf.z_begin);
   for(index=44;index<=65;index++)
      l_ppd_buf.ppd029[index] = str_buf[index-44];

/*extract sf.vx_begin,sf.vy_begin,sf.vz_begin*/
/*convert & load into ppd030[66] 1st data point velocity vector */
sprintf(str_buf,"%-22.15f",sf.vx_begin * 1000.0);
   for(index=0;index<=21;index++)
      l_ppd_buf.ppd030[index] = str_buf[index];
sprintf(str_buf,"%-22.15f",sf.vy_begin * 1000.0);
   for(index=22;index<=43;index++)
      l_ppd_buf.ppd030[index] = str_buf[index-22];
sprintf(str_buf,"%-22.15f",sf.vz_begin * 1000.0);
   for(index=44;index<=65;index++)
      l_ppd_buf.ppd030[index] = str_buf[index-44];


	/*Second positional data point*/
/*extract sf.x_mid,sf.y_mid,sf.z_mid*/
/*convert & load into ppd031[66] second data point position vector*/
sprintf(str_buf,"%-22.15f",sf.x_mid);
   for(index=0;index<=21;index++)
      l_ppd_buf.ppd031[index] = str_buf[index];
sprintf(str_buf,"%-22.15f",sf.y_mid);
   for(index=22;index<=43;index++)
      l_ppd_buf.ppd031[index] = str_buf[index-22];
sprintf(str_buf,"%-22.15f",sf.z_mid);
   for(index=44;index<=65;index++)
      l_ppd_buf.ppd031[index] = str_buf[index-44];

/*extract sf.vx_mid,sf.vy->mid,sf.vz_mid*/
/*convert & load into ppd032[66] second data point velocity vector*/
sprintf(str_buf,"%-22.15f",sf.vx_mid * 1000.0);
   for(index=0;index<=21;index++)
      l_ppd_buf.ppd032[index] = str_buf[index];
sprintf(str_buf,"%-22.15f",sf.vy_mid * 1000.0);
   for(index=22;index<=43;index++)
      l_ppd_buf.ppd032[index] = str_buf[index-22];
sprintf(str_buf,"%-22.15f",sf.vz_mid * 1000.0);
   for(index=44;index<=65;index++)
      l_ppd_buf.ppd032[index] = str_buf[index-44];


	/*Third positional data point*/
/*extract sf.x_end,sf.y_end,sf.z_end*/
/*convert & load into ppd033[66] third data point position vector*/
sprintf(str_buf,"%-22.15f",sf.x_end);
   for(index=0;index<=21;index++)
      l_ppd_buf.ppd033[index] = str_buf[index];
sprintf(str_buf,"%-22.15f",sf.y_end);
   for(index=22;index<=43;index++)
      l_ppd_buf.ppd033[index] = str_buf[index-22];
sprintf(str_buf,"%-22.15f",sf.z_end);
   for(index=44;index<=65;index++)
      l_ppd_buf.ppd033[index] = str_buf[index-44];

/*extract sf.vx_end,sf.vy->end,sf.vz_end*/
/*convert & load into ppd034[66] third data point velocity vector*/
sprintf(str_buf,"%-22.15f",sf.vx_end * 1000.0);
   for(index=0;index<=21;index++)
      l_ppd_buf.ppd034[index] = str_buf[index];
sprintf(str_buf,"%-22.15f",sf.vy_end * 1000.0);
   for(index=22;index<=43;index++)
      l_ppd_buf.ppd034[index] = str_buf[index-22];
sprintf(str_buf,"%-22.15f",sf.vz_end * 1000.0);
   for(index=44;index<=65;index++)
      l_ppd_buf.ppd034[index] = str_buf[index-44];

/*write L_PPD_FILE1*/
if(fwrite(&l_ppd_buf,L_PPD_SIZE,1,fp1)!=1)
  printf("File write error\n");


return (PASS);

}
