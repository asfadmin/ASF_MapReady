static char sccsid_l_dss_cmp_c[] =
    "@(#)l_dss_cmp.c	1.3 96/04/09 20:48:48";

/* l_dss_cmp(sp,pp,fp1) -----------------------------------

*********************************************************************
* l_dss_cmp.c Leader File-Data Set Summary Record 		    *
*********************************************************************

*/

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "l_dss.h"
#include "scene_file.h"
#include "procdec.h"
#include "procfil.h"
#include "procpar.h"

extern char DEF_PATH[80];	/* default files */

#define L_DSS_SIZE	2014

extern DPP_FILE 	dpf;
extern SP_FILE 		spf;
extern EM_FILE 		emf;
extern SCENE_FILE	sf;

extern RQST_PTR 	Cur_Rqst;
extern SV		sv1,sv2;
extern TC_FILE 		tcf;

extern int	sw_clutter_lock;
extern int	sw_auto_focus;


l_dss_cmp(sp,pp,fp1)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
    FILE		*fp1;
{

FILE *fp0;
L_DSS_FILE l_dss_buf;

int	index;
char	str_buf[32];
char	filename[80];
double	val_dbl;
float	val_flt;
int     year,month,day;
double  xx,yy,zz,dd1,dd2,len,sc_dist,heading;

double  pi = 3.141592653589793;
double  c = 2.998E8;

/* Open initialized L_DSS_FILE0 for reading */
sprintf(filename,"%sL_DSS_FILE0",DEF_PATH);
if((fp0 = fopen(filename,"r")) == NULL){
  printf("Cannot open %s\n",filename);
  return (FAIL);
  }
if(fread(&l_dss_buf,L_DSS_SIZE,1,fp0)!=1){
  if(feof(fp0))
    return (FAIL);
  printf("File L_DSS_FILE0 read error\n");
  }

fclose (fp0);


/*extract sf.image_id*/
/*load into dss009[16]	Scene identifier*/
strncpy(l_dss_buf.dss009,sf.image_id,strlen(sf.image_id));

/*extract sf.tme_scene_ctr*/
/*load into dss011[32]	Input scene center time*/
strncpy(l_dss_buf.dss011,sf.tme_scene_ctr,4);
l_dss_buf.dss011[4] = '\0';
year = atoi(l_dss_buf.dss011);
strncpy(str_buf,&sf.tme_scene_ctr[4],3);
str_buf[3] = '\0';
day = atoi(str_buf);
get_month_day(year,day,&month,&day);
sprintf(&l_dss_buf.dss011[4],"%.2d%.2d",month,day);
strncpy(&l_dss_buf.dss011[8],&sf.tme_scene_ctr[8],2);
strncpy(&l_dss_buf.dss011[10],&sf.tme_scene_ctr[11],2);
strncpy(&l_dss_buf.dss011[12],&sf.tme_scene_ctr[14],2);
strncpy(&l_dss_buf.dss011[14],&sf.tme_scene_ctr[17],3);
			/*YYYYMMDDhhmmssttt$$$$$$$$$$$$$ where:*/
			/*YYYY = year*/
			/*MM = month */
			/*DD = day */
			/*hh = hours (00 to 23) */
			/*mm = minutes (00 to 59)*/
			/*ss = seconds (00 to 59)*/
			/*ttt = milliseconds (000 to 999)*/

/*extract sf.lat_scene_ctr*/
/*convert & load into dss013[16] scene center geodetic latitude*/
sprintf(str_buf,"%-16.7e",sf.lat_scene_ctr);
   for(index=0;index<=15;index++)
      l_dss_buf.dss013[index] = str_buf[index];

/*extract sf.lon_scene_ctr*/
/*convert & load into dss014[16] scene center geodetic longitude*/
sprintf(str_buf,"%-16.7e",sf.lon_scene_ctr);
   for(index=0;index<=15;index++)
      l_dss_buf.dss014[index] = str_buf[index];

/*convert & load into  dss015[16] Scene Center true heading(deg.)*/
xx = sf.x_begin * sf.x_begin;
yy = sf.y_begin * sf.y_begin;
zz = sf.z_begin * sf.z_begin;
dd1 = (xx + yy) / (xx + yy + zz);
dd1 = acos(sqrt(dd1));
len = sqrt(xx+yy+zz);
xx = sf.x_end * sf.x_end;
yy = sf.y_end * sf.y_end;
zz = sf.z_end * sf.z_end;
dd2 = (xx + yy) / (xx + yy + zz);
dd2 = acos(sqrt(dd2));
xx = sf.x_begin - sf.x_end;
yy = sf.y_begin - sf.y_end;
zz = sf.z_begin - sf.z_end;
sc_dist = sqrt( xx*xx + yy*yy + zz*zz );
heading = (180.0 / pi) * asin(len * fabs(dd2 - dd1) / sc_dist);
if (dd2 > dd1 || (dd2 == dd1 && sf.z_mid > 0.0)) {
    heading = 270.0 + heading; 
}
else {
    heading = 270.0 - heading;
}
sprintf(str_buf,"%-16.7e",heading);
   for(index=0;index<=15;index++)
      l_dss_buf.dss015[index] = str_buf[index];

/*extract emf.r_e*/
/*convert & load into dss017[16] Ellipsoid semimajor axis (km)-{Re}*/
sprintf(str_buf,"%-16.7e",emf.r_e);
   for(index=0;index<=15;index++)
      l_dss_buf.dss017[index] = str_buf[index];

/*extract emf.r_pole*/
/*convert & load into dss018[16] Ellipsoid semiminor axis (km)*/
sprintf(str_buf,"%-16.7e",emf.r_pole);
   for(index=0;index<=15;index++)
      l_dss_buf.dss018[index] = str_buf[index];

/*extract Cur_Rqst->ave_hght*/
/*convert & load into dss024[16] Average terrain height(km)*/
sprintf(str_buf,"%-16.7e",Cur_Rqst->ave_hght);
   for(index=0;index<=15;index++)
      l_dss_buf.dss024[index] = str_buf[index];

/*extract sf.lnum_scene_ctr*/
/*convert & load into dss025[16] Scene center line number*/
val_dbl = sf.lnum_scene_ctr;
sprintf(str_buf,"%-16.7e",val_dbl);
   for(index=0;index<=15;index++)
      l_dss_buf.dss025[index] = str_buf[index];

/*extract sf.pnum_scene_ctr*/
/*convert & loan into dss026[16] Scene center pixel number*/
val_dbl = sf.pnum_scene_ctr;
sprintf(str_buf,"%-16.7e",val_dbl);
   for(index=0;index<=15;index++)
      l_dss_buf.dss026[index] = str_buf[index];

/*convert & load into dss027[16] Processed scene length (km)*/
val_dbl = 0.0125 * (sf.lnum_scene_ctr * 2);
sprintf(str_buf,"%-16.7e",val_dbl);
   for(index=0;index<=15;index++)
      l_dss_buf.dss027[index] = str_buf[index];

/*=0.0125 * (sf.pnum_scene_ctr * 2)*/
/*convert & load into dss028[16] Processed scene width (km)*/
val_dbl = 0.0125 * (sf.pnum_scene_ctr * 2);
sprintf(str_buf,"%-16.7e",val_dbl);
   for(index=0;index<=15;index++)
      l_dss_buf.dss028[index] = str_buf[index];

/*sf.sp_id*/
/*convert & load into dss033[16] Sensor platform mission identifier*/
strncpy(l_dss_buf.dss033,sf.sp_id,strlen(sf.sp_id));

/*sf.snsr_mode*/
/*extract & laod into dss034[32] Sensor ID & Mode of operation */
strncpy(l_dss_buf.dss034,sf.snsr_mode,strlen(sf.snsr_mode));
			/*Sensor and mode of operation.		      */
			/*AAAAAA-BB-CCDD-EFbbbbbbbbbbbbbbb	      */
                       	/*AAAAAA= SAR, Sensor ID 		      */
                        /*BB 	= Lb, L-band; 			      */
                        /*  	= Cb, C-band (b is a trailing  blank) */
                        /*CC    = HR, High res,= LR, Low res          */
                        /*DD    = Code for imaging mode               */
			/*       (NM=near, FM=far, BM=browse, SM=scan)*/
                        /*E 	= H, Horizontal,= V, Vertical         */
                        /*F 	= H, Horizontal,= V, Vertical 	      */

/*convert & load into dss035[8] Orbit number*/
/*   extract Cur_Rqst->take_id positions 6 thru 10   */
/*      into positions 4 thru 8 in the CEOS record   */
/* note in C language index begins at zero, hence the offset by -1 */
   for(index=5;index<=9;index++)
      l_dss_buf.dss035[index-2] = Cur_Rqst->take_id[index];

/*need equations to calculate the geodetic lat/lon from x,y,z of S/C*/
/*%dss036[8];*/		/*Sensor Platform geodetic Latitude*/
/*%dss037[8];*/		/*Sensor Platform geodetic Longitude*/

/*%dss038[8];*/		/*Sensor Platform Heading(deg.)*/
sprintf(str_buf,"%-8.4f",sf.trk_ang);
   for(index=0;index<=7;index++)
      l_dss_buf.dss038[index] = str_buf[index];

/* extract spf.gamma_nom*/
/*convert & load into dss039[8] Sensor clock angle*/
sprintf(str_buf,"  90.000");
   for(index=0;index<=7;index++)
      l_dss_buf.dss039[index] = str_buf[index];

/* extract sf.inc_image_ctr */
/*%dss040[8];*/		/*Incidence angle at scene center*/
sprintf(str_buf,"%-8.4f",sf.inc_image_ctr);
   for(index=0;index<=7;index++)
      l_dss_buf.dss040[index] = str_buf[index];

/*dss041[8] Radar frequency  (GHz)*/
/* for J-ERS-1, L-band, something will have to be done here or  */
/*    in the static files of the default directory */

/*extract spf.xlambda*/
/*convert & load into dss042[16] Radar wavelength (meters)*/
sprintf(str_buf,"%-16.7e",spf.xlambda);
   for(index=0;index<=15;index++)
      l_dss_buf.dss042[index] = str_buf[index];

/* calculate range pulse amplitude coefficient # 2 (Hz/sec) */
val_dbl = (spf.bw_ra / spf.tau) * 1.0e12;
/*convert & load into dss046[16] Range pulse amplitude coeff #2*/
sprintf(str_buf,"%-16.7e",val_dbl);
   for(index=0;index<=15;index++)
      l_dss_buf.dss046[index] = str_buf[index];

/*extract spf.csr*/
/*convert & load into dss057[16] Sampling rate (MHz)*/
val_dbl = spf.csr / 1000000.0;
sprintf(str_buf,"%-16.7e",val_dbl);
   for(index=0;index<=15;index++)
      l_dss_buf.dss057[index] = str_buf[index];

/*extract wndw_pos*/
/*convert & load into dss058[16] Range gate at early edge(usec)*/
sprintf(str_buf,"%-16.7e",sf.wndw_pos);
   for(index=0;index<=15;index++)
      l_dss_buf.dss058[index] = str_buf[index];

/*extract spf.tau*/
/*convert & load into dss059[16] Range pulse length (usec)*/
sprintf(str_buf,"%-16.7f",spf.tau);
   for(index=0;index<=15;index++)
      l_dss_buf.dss059[index] = str_buf[index];

/*auxiliary data value + 47db .....see sheet4.25*/
/*%dss062[16];*/		/*Receiver gain for like polarized(dB)*/

/*extract spf.nbits*/
/*convert & load into dss064[8] Quantization in bits per channel*/
sprintf(str_buf,"%8u",spf.nbits);
   for(index=0;index<=7;index++)
      l_dss_buf.dss064[index] = str_buf[index];

/*convert & load into dss066[16] DC Bias for I-component*/
val_dbl = (pp->imean_cor)-(pp->imean);
sprintf(str_buf,"%-16.7e",val_dbl);
   for(index=0;index<=15;index++)
      l_dss_buf.dss066[index] = str_buf[index];

/*=(pp->qmean_cor)-(pp->qmean)*/
/*convert & load into dss067[16] DC Bias for Q-component*/
val_dbl = (pp->qmean_cor)-(pp->qmean);
sprintf(str_buf,"%-16.7e",val_dbl);
   for(index=0;index<=15;index++)
      l_dss_buf.dss067[index] = str_buf[index];

/*extract sf.iq_gain_imb*/
/*convert & load into dss068[16] Gain imbalance for I & Q*/
sprintf(str_buf,"%-16.7e",sf.iq_gain_imb);
   for(index=0;index<=15;index++)
      l_dss_buf.dss068[index] = str_buf[index];

/* extract spf.gamma_nom*/
/*convert & load into dss071[16] Antenna electronic boresight relative*/
/*to platform vertical axis at the start of the image (degrees)*/
sprintf(str_buf,"%-16.7e",spf.gamma_nom);
   for(index=0;index<=15;index++)
      l_dss_buf.dss071[index] = str_buf[index];

/* extract spf.gamma_nom*/
/*convert & load into dss072[16] Antenna mechanical boresight relative*/
/*to platform vertical axis at the start of the image, positive to the*/
/*right,negative to the left (degrees)*/
sprintf(str_buf,"%-16.7e",spf.gamma_nom);
   for(index=0;index<=15;index++)
      l_dss_buf.dss072[index] = str_buf[index];

/*sp->pre->prf*/
/*convert & load into dss074[16] Nominal PRF (Hz)*/
sprintf(str_buf,"%-16.7e",sp->pre->prf);
   for(index=0;index<=15;index++)
      l_dss_buf.dss074[index] = str_buf[index];

/*calculate antenna azimuth beam width*/
val_dbl = (180.0 / pi) * (0.88 * spf.xlambda) / spf.ant_len;
/*convert & load into dss076[16] Antenna azimuth beam width (degrees)*/
sprintf(str_buf,"%-16.7f",val_dbl);
   for(index=0;index<=15;index++)
      l_dss_buf.dss076[index] = str_buf[index];

	/*Sensor specific parameters*/
/*extract tcf.bt*/
/*convert & load into dss077[16] Satellite encoded binary time code*/
sprintf(str_buf,"%16u",tcf.bt);
   for(index=0;index<=15;index++)
      l_dss_buf.dss077[index] = str_buf[index];

/*extract tcf.gmt*/
get_month_day(tcf.gmt.yr,tcf.gmt.day,&month,&day);
/*load into dss078[32] Satellite clock time<YYYYMMDDhhmmssttt$$$..$>*/
sprintf(str_buf,"%.4d%.2d%.2d%.2d%.2d%6.3f",tcf.gmt.yr,month,day,
	tcf.gmt.hr,tcf.gmt.min,tcf.gmt.second);
   for(index=0;index<=17;index++)
      l_dss_buf.dss078[index] = str_buf[index];

/*extract tcf.delta*/
/*convert & load into dss079[8] Satellite clock increment (nano-secs)*/
sprintf(str_buf,"%8u",tcf.delta);
   for(index=0;index<=7;index++)
      l_dss_buf.dss079[index] = str_buf[index];

/*extract pp->img_name*/
/*load into dss084[16] Processing facility process code*/
strncpy(l_dss_buf.dss084,&pp->img_name[1],strlen(&pp->img_name[1]));

/*extract ap_looks*/
/*convert & load into dss088[16] Nominal azimuth # of looks */
sprintf(str_buf,"%8u.0000000",ap_looks);
   for(index=0;index<=15;index++)
      l_dss_buf.dss088[index] = str_buf[index];

/*extract pp->bw_az4_act*/
/*convert & load into dss090[16] Bandwidth per look in Azimuth ( Hz )*/
sprintf(str_buf,"%-16.7f",pp->bw_az4_act/ap_looks);
   for(index=0;index<=15;index++)
      l_dss_buf.dss090[index] = str_buf[index];

/*extract spf.bw_ra*/
/*convert & load into dss091[16] Processor bandwidth per look in range*/
sprintf(str_buf,"%-16.7f",spf.bw_ra);
   for(index=0;index<=15;index++)
      l_dss_buf.dss091[index] = str_buf[index];

/*extract spf.pbw*/
/*convert & load into dss092[16] Total processor bandwidth in Azimuth*/
sprintf(str_buf,"%-16.7f",spf.pbw);
   for(index=0;index<=15;index++)
      l_dss_buf.dss092[index] = str_buf[index];

/*extract spf.bw_ra*/
/*convert & load into dss093[16] Total processor bandwidth in range*/
sprintf(str_buf,"%-16.7f",spf.bw_ra);
   for(index=0;index<=15;index++)
      l_dss_buf.dss093[index] = str_buf[index];

/*extract sp->pre->dcrs_id*/
/*convert & load into dss096[16] Data input source(eg:HDDT identifier)*/
strncpy(l_dss_buf.dss096,sp->pre->dcrs_id,strlen(sp->pre->dcrs_id));

/*extract sf.fdga,sf.fdgb,sf.fdgc*/

/*convert & load fda_gnd into dss101[16] */
/*Along track Doppler frequency constant term(Hz)*/ 
sprintf(str_buf,"%-16.7e",sf.fdga);
   for(index=0;index<=15;index++)
      l_dss_buf.dss101[index] = str_buf[index];

/*convert & load fda_gnd into dss105[16] */
/*Cross track Doppler frequency constant term*/ 
/*at early edge of image (Hz)*/
sprintf(str_buf,"%-16.7e",sf.fdga);
   for(index=0;index<=15;index++)
      l_dss_buf.dss105[index] = str_buf[index];

/*convert & load fdb_gnd into dss106[16] */
/*Cross track Doppler frequency linear term*/
/*at early edge of the image (Hz/pixel)*/
sprintf(str_buf,"%-16.7e",sf.fdgb);
   for(index=0;index<=15;index++)
      l_dss_buf.dss106[index] = str_buf[index];

/*convert & load fdc_gnd into dss107[16] */
/*Cross track Doppler frequency quadratic term*/
/*at early edge of the image (Hz/pixeI/pixel)*/
sprintf(str_buf,"%-16.7e",sf.fdgc);
   for(index=0;index<=15;index++)
      l_dss_buf.dss107[index] = str_buf[index];

/*extract sf.fdotga,sf.fdotgb,sf.fdotgc*/

/*convert & load fdota_gnd into dss110[16] */
/*Cross track Doppler freq constant term*/
sprintf(str_buf,"%-16.7e",sf.fdotga);
   for(index=0;index<=15;index++)
      l_dss_buf.dss110[index] = str_buf[index];

/*convert & load fdota_gnd into dss114[16] */
/*Cross track Doppler freq rate constant term*/
sprintf(str_buf,"%-16.7e",sf.fdotga);
   for(index=0;index<=15;index++)
      l_dss_buf.dss114[index] = str_buf[index];

/*convert & load fdotb_gnd into dss115[16] */
/*Cross track Doppler freq rate linear term*/
sprintf(str_buf,"%-16.7e",sf.fdotgb);
   for(index=0;index<=15;index++)
      l_dss_buf.dss115[index] = str_buf[index];

/*convert & load fdotc_gnd into dss116[16] */
/*Cross track Doppler freq rate quadratic term*/
sprintf(str_buf,"%-16.7e",sf.fdotgc);
   for(index=0;index<=15;index++)
      l_dss_buf.dss116[index] = str_buf[index];

/*if "CSD" NOT else YES or check config.asp switch for clutterlock*/
/*load into dss119[4] Clutter lock applied flag ( "YES$"/"NOT$" )*/
if (strcmp(Cur_Rqst->type,"CSD") == 0 || sw_clutter_lock == 0)
    strncpy(l_dss_buf.dss119,"NOT ",4);

/*if "CSD" NOT else NOT or check config.asp switch for autofocus*/
/*load into dss120[4] Autofocussing applied flag ( "YES$"/"NOT$" )*/
if (strcmp(Cur_Rqst->type,"CSD") == 0 || sw_auto_focus == 0)
    strncpy(l_dss_buf.dss120,"NOT ",4);

/*12.5m high res/ 100m low res/ calc if CPX or CSD*/
/*convert & load into dss121[16] Line spacing*/
if (strcmp(Cur_Rqst->type,"CPX") == 0 || 
	strcmp(Cur_Rqst->type,"CSD") == 0) {
    val_dbl = pp->swth_speed / sp->pre->prf;
    sprintf(str_buf,"%-16.7f",val_dbl);
       for(index=0;index<=15;index++)
	  l_dss_buf.dss121[index] = str_buf[index];
}

/*12.5m high res/ 100m low res/ calc if in slant range*/
/*convert & load into dss122[16] Pixel spacing in range*/
if (strcmp(Cur_Rqst->type,"CPX") == 0
	|| strcmp(Cur_Rqst->type,"CSD") == 0
	|| (strcmp(Cur_Rqst->type,"RPR") == 0 
	    && Cur_Rqst->gnd_slnt_rg[0] == 'S')) {
    val_dbl = c / (2.0 * spf.csr);
    sprintf(str_buf,"%-16.7f",val_dbl);
       for(index=0;index<=15;index++)
	  l_dss_buf.dss122[index] = str_buf[index];
}
  


/*write L_DSS_FILE1					      */
if(fwrite(&l_dss_buf,L_DSS_SIZE,1,fp1)!=1)
  printf("File write error\n");


return (PASS);

}
