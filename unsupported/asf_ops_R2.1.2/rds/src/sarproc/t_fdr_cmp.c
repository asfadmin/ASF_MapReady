
static char sccsid_t_fdr_cmp_c[] =
    "@(#)t_fdr_cmp.c	1.3 96/04/09 20:48:51";

/* t_fdr_cmp(sp,pp,fp1) -----------------------------------------

**********************************************************************
*t_fdr_cmp.c fills in the CEOS trailer file facility data record     *
**********************************************************************

*/

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <math.h>
#include <time.h>
#include "t_fdr.h"
#include "scene_file.h"
#include "procdec.h"
#include "procfil.h"
#include "procpar.h"

extern char DEF_PATH[80];	/* default files */


#define T_FDR_SIZE	1513	

#define STD 1
#define RPR 2
#define CPX 3
#define CSD 4
#define QLK 5

extern DPP_FILE 	dpf;
extern SP_FILE 		spf;
extern RTF_FILE 	rtf;
extern EM_FILE 		emf;
extern SCENE_FILE	sf;

extern RQST_PTR 	Cur_Rqst;
extern SV		sv1,sv2;
extern TC_FILE 		tcf;

extern int sw_auto_focus;	/* 1 = perform auto focus   */
extern int sw_clutter_lock;	/* 1 = perform clutter lock */

t_fdr_cmp(sp,pp,fp1)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
    FILE		*fp1;
{

FILE *fp0;
T_FDR_FILE t_fdr_buf;

int	index;
char	str_buf[100];
char	filename[80];
double	val_dbl;
float	val_flt;
double  lk_angle,spacing,delta;
double  pi = 3.141592653589793;
double  c = 2.998E8;
int     ptype;

struct tm *gmtime(),*tp;
GMT now;

/* Open initialized T_FDR_FILE0 for reading */
sprintf(filename,"%sT_FDR_FILE0",DEF_PATH);
if((fp0 = fopen(filename,"r")) == NULL){
  printf("Cannot open %s\n",filename);
  return (FAIL);
  }
if(fread(&t_fdr_buf,T_FDR_SIZE,1,fp0)!=1){
  if(feof(fp0))
    return (FAIL);
  printf("File T_FDR_FILE0 read error\n");
  }

fclose(fp0);

/*set processing request type*/
if (strcmp(Cur_Rqst->type,"STD") == 0)
    ptype = STD;
if (strcmp(Cur_Rqst->type,"RPR") == 0)
    ptype = RPR;
if (strcmp(Cur_Rqst->type,"CPX") == 0)
    ptype = CPX;
if (strcmp(Cur_Rqst->type,"CSD") == 0)
    ptype = CSD;
if (strcmp(Cur_Rqst->type,"QLK") == 0)
    ptype = QLK;

/*extract Cur_Rqst->take_id*/
/*load into fdr007[14] DataTakeID SS/X/rrrrr.nn format*/
/*SS = Satellite, X = Sensor, rrrrr = Rev No. */
/*nn = Datatakes within a Rev.*/
strncpy(t_fdr_buf.fdr007,Cur_Rqst->take_id,strlen(Cur_Rqst->take_id));

/*extract sf->image_id[12]*/
/*convert & load into fdr008[11] ImageID NNNNNNNTss format where*/
/*T	= 1 ,Full-res image */
/*	= 2, Lo-res image */
/*	= 3, Geocoded full-res */
/*    	= 4, Geocoded lo-res */
/*   	= 5, Complex */
/*    	= 6, CCSD */
strncpy(t_fdr_buf.fdr008,sf.image_id,strlen(sf.image_id));

/*Greenwich Mean Time of correlation*/
/*get current time */
index = 0;
index = time(&index);
tp = gmtime(&index);

/*convert & load into fdr009[5] Year of Correlation*/
sprintf(str_buf,"%-4d",tp->tm_year + 1900);
   for(index=0;index<=3;index++)
      t_fdr_buf.fdr009[index] = str_buf[index];

/*convert & load into fdr010[17]*/ 
/*date in ddd:hh:mm:ss.ccc format where:*/
/*ddd = Julian days*/
/*hh = Hours, mm = Minutes*/
/*ss = Seconds, ccc = Milliseconds*/
sprintf(str_buf,"%.3d:%.2d:%.2d:%6.3f",tp->tm_yday+1,
	tp->tm_hour,tp->tm_min,(float)tp->tm_sec);
   strncpy(t_fdr_buf.fdr010,str_buf,16);

/*extract Cur_Rqst->site*/
/*convert & load into fdr011[33] name of the site covered by the data.*/
strncpy(t_fdr_buf.fdr011,Cur_Rqst->site,strlen(Cur_Rqst->site));

/*Greenwich Mean Time at image center*/ 
/*extract sf.tme_scene_ctr */
/*load into fdr012[5] and fdr013[17]*/
/*Format: yyyy ddd:hh:mm:ss.ccc, where: */
/*yyyy = year*/
/*ddd = Julian days, hh = Hours*/
/*mm = Minutes, ss = Seconds  */
/*ccc = Milliseconds*/
strncpy(t_fdr_buf.fdr012,sf.tme_scene_ctr,4);
strncpy(t_fdr_buf.fdr013,&sf.tme_scene_ctr[4],strlen(&sf.tme_scene_ctr[4]));

/*extract sf.lat_scene_ctr*/
/*convert & load into fdr014[17] Latitude of image center(deg)*/
sprintf(str_buf,"%-16.7f",sf.lat_scene_ctr);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr014[index] = str_buf[index];

/*extract sf.lon_scene_ctr*/
/*convert & load into fdr015[17] Longitude of image center*/
sprintf(str_buf,"%-16.7f",sf.lon_scene_ctr);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr015[index] = str_buf[index];

/*extract sf.lat_a*/
/*convert & load into fdr016[17] Latitude at the start  of the image*/
/*frame in near swath*/
sprintf(str_buf,"%-16.7f",sf.lat_a);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr016[index] = str_buf[index];

/*extract sf.lon_a*/
/*convert & load into fdr017[17] Longitude at the start of the image*/
/*frame in the near swath*/
sprintf(str_buf,"%-16.7f",sf.lon_a);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr017[index] = str_buf[index];

/*extract sf.lat_d*/
/*convert & load into fdr018[17] Latitude at the end of the image */
/*frame in the near swath*/
sprintf(str_buf,"%-16.7f",sf.lat_d);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr018[index] = str_buf[index];

/*extract sf.lon_d*/
/*convert & load into fdr019[17] Longitude at the end of the image */
/*frame in the near swath*/
sprintf(str_buf,"%-16.7f",sf.lon_d);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr019[index] = str_buf[index];

/*extract sf.lat_b*/
/*convert & load into fdr020[17] Latitude at the start of the image */
/*frame in the far swath*/
sprintf(str_buf,"%-16.7f",sf.lat_b);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr020[index] = str_buf[index];

/*extract sf.lon_b*/
/*convert & load into fdr021[17] Longitude at the start of the image */
/*frame in the far swath*/
sprintf(str_buf,"%-16.7f",sf.lon_b);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr021[index] = str_buf[index];

/*extract sf.lat_e*/
/*convert & load into fdr022[17] Latitude at the end of the image */
/*frame in the far swath*/
sprintf(str_buf,"%-16.7f",sf.lat_e);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr022[index] = str_buf[index];

/*extract sf.lon_e*/
/*convert & load into fdr023[17] Longitude at the end of the image */
/*frame in the far swath*/
sprintf(str_buf,"%-16.7f",sf.lon_e);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr023[index] = str_buf[index];

/*=0.0125 x (sf.nl)*/
/*convert & load into fdr024[17] The actual swath width (km) coverage */
/*in the azimuth direction*/
val_flt = 0.0125 * sf.nl;
if (ptype == CPX)
  val_flt = (pp->swth_speed / (1000.0 * sp->pre->prf)) * sf.nl;
if (ptype == CSD)
  val_flt = 100.0;
sprintf(str_buf,"%-16.7f",val_flt);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr024[index] = str_buf[index];

/*fdr025[17]; calculated below, after fdr063*/

/*convert & load into fdr026[9] The actual number of pixels per line */
/*Format: I8 */
sprintf(str_buf,"%-8u",sf.np_nflr);
   for(index=0;index<=7;index++)
      t_fdr_buf.fdr026[index] = str_buf[index];

/*convert & load into fdr027[9] The actual number of lines */
/*Format: I8 */
sprintf(str_buf,"%-8u",sf.nl_nflr);
   for(index=0;index<=7;index++)
      t_fdr_buf.fdr027[index] = str_buf[index];

/*extract sf.np*/
/*convert & load into fdr028[9] The total (with filler data) number */
/*of pixels per line.Format: I8 */
sprintf(str_buf,"%-8u",sf.np);
   for(index=0;index<=7;index++)
      t_fdr_buf.fdr028[index] = str_buf[index];

/*extract sf.nl*/
/*convert & load into fdr029[9] The total (with filler data) number */
/*of image lines.Format: I8 */
sprintf(str_buf,"%-8u",sf.nl);
   for(index=0;index<=7;index++)
      t_fdr_buf.fdr029[index] = str_buf[index];

/*extract sf.media_labl*/
/*load into fdr030[7] Identification label of the media that the*/
/*ID image was written to in format: ccnnnn*/ 
/* cc 	=AI, Archive Image tape */
/*      =QL, Quick Look tape    */
strncpy(t_fdr_buf.fdr030,sf.media_labl,strlen(sf.media_labl));

/*extract sf.dcrs_start*/
/*convert & load into fdr031[17] block# on DCRSi where data begins*/
sprintf(str_buf,"%-16u",sf.dcrs_start);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr031[index] = str_buf[index];

/*extract sf.dcrs_end*/
/*convert & load into fdr032[17] Location on DCRSi where data ends*/
sprintf(str_buf,"%-16u",sf.dcrs_end);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr032[index] = str_buf[index];

/*fdr033[17];*/		/*The name of the platform for the sensor*/
			/*that transmitted the SAR data.Format:A16*/
			/*= E-ERS1, European Space Agency Remote Earth*/
			/*          Sensing Satellite 1 */
                        /*= J-ERS1,Japanese Earth Resource Satellite 1*/
                        /*= RADARSAT*/
strncpy(t_fdr_buf.fdr033,sf.sp_id,strlen(sf.sp_id));


/*Sensor and mode of operation load into fdr034[33];*/		
			/*AAAAAA-BB-CCDD-EFbbbbbbbbbbbbbbb*/
                       	/*AAAAAA= TBD, Sensor ID */
                        /*BB 	= Lb, L-band; */
                        /*  	= Cb, C-band (b is a trailing  blank) */
                        /*CC    = HI, High resolution */
                        /*    	= LO, Low resolution */
                        /*DD    = TBD, Code for imaging mode */
			/*(e.g., near,  far,  browse, scan, etc.) */
                        /*E 	= H, Horizontal */
                        /*   	= V, Vertical */
                        /*F 	= H, Horizontal */
                        /*   	= V, Vertical */
strncpy(t_fdr_buf.fdr034,sf.snsr_mode,strlen(sf.snsr_mode));

/*extract sp->pre->prf*/
/*convert & load into fdr035[17] The pulse repetition frequency (PRF)*/
sprintf(str_buf,"%-16.7f",sp->pre->prf);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr035[index] = str_buf[index];

/*fdr036[17];  calculated below, after fdr041*/

/*fdr037[17];*/		/*Data rate.			Format:F16.7 */

/*sf->wndw_pos*/
/*convert & load into fdr038[17] Data window position*/
sprintf(str_buf,"%-16.7f",sf.wndw_pos);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr038[index] = str_buf[index];

/*Range gate delay.Format:F16.7 */
/*calculate from (data_window_pos + (8/PRF)x10**6)*/
/* load into fdr039[17]*/
sprintf(str_buf,"%-16.7f",sf.rnge_gate);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr039[index] = str_buf[index];

/*fdr040[17];*/		/*Track angle to True North.	Format:F16.7 */
sprintf(str_buf,"%-16.7f",sf.trk_ang);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr040[index] = str_buf[index];

/*fdr041[2];*/		/*Flag indicating whether the pass is*/
			/*=A,ascending or =D,descending*/
			/* (done above by track angle calculation) */
t_fdr_buf.fdr041[0] = sf.asc_dsc[0];

/*convert & load into fdr036[17] The SAR antenna look angle*/
sprintf(str_buf,"%-16.7f",sf.lk_angle);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr036[index] = str_buf[index];

/*extract sf->altitude*/
/*convert & load into fdr042[17] Altitude of the S/C at image center*/
sprintf(str_buf,"%-16.7f",sf.altitude);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr042[index] = str_buf[index];
  
/*extract sf.x_mid*/
/*convert & load into fdr043[23] The S/C X-position at image center*/
sprintf(str_buf,"%-22.15f",sf.x_mid);
   for(index=0;index<=21;index++)
      t_fdr_buf.fdr043[index] = str_buf[index];

/*extract sf.y_mid*/
/*convert & load into fdr044[23] The S/C Y-position at image center*/
sprintf(str_buf,"%-22.15f",sf.y_mid);
   for(index=0;index<=21;index++)
      t_fdr_buf.fdr044[index] = str_buf[index];
  
/*extract sf.z_mid*/
/*convert & load into fdr045[23] The S/C Z-position at image center*/ 
sprintf(str_buf,"%-22.15f",sf.z_mid);
   for(index=0;index<=21;index++)
      t_fdr_buf.fdr045[index] = str_buf[index];
  
/*extract sf.vx_mid*/
/*convert & load into fdr046[23] The S/C X-velocity at image center*/
sprintf(str_buf,"%-22.15f",sf.vx_mid * 1000.0);
   for(index=0;index<=21;index++)
      t_fdr_buf.fdr046[index] = str_buf[index];
  
/*extract sf.vy_mid*/
/*convert & load into fdr047[23] The S/C Y-velocity at image center*/
sprintf(str_buf,"%-22.15f",sf.vy_mid * 1000.0);
   for(index=0;index<=21;index++)
      t_fdr_buf.fdr047[index] = str_buf[index];
  
/*extract sf.vz_mid*/
/*convert & load into fdr048[23] The S/C Z-velocity at image center*/
sprintf(str_buf,"%-22.15f",sf.vz_mid * 1000.0);
   for(index=0;index<=21;index++)
      t_fdr_buf.fdr048[index] = str_buf[index];

/*extract pp->att.roll*/
/*convert & load into fdr049[15] The S/C roll at image center*/
sprintf(str_buf,"%-14.6e",pp->att.roll);
   for(index=0;index<=13;index++)
      t_fdr_buf.fdr049[index] = str_buf[index];

/*extract pp->att.yaw*/
/*convert & load into fdr050[15] The S/C yaw at image center*/
sprintf(str_buf,"%-14.6e",pp->att.yaw);
   for(index=0;index<=13;index++)
      t_fdr_buf.fdr050[index] = str_buf[index];

/*extract pp->att.pitch*/
/*convert & load into fdr051[15] The S/C pitch at image center*/
sprintf(str_buf,"%-14.6e",pp->att.pitch);
   for(index=0;index<=13;index++)
      t_fdr_buf.fdr051[index] = str_buf[index];

/*3 attitude quality flags, 052-054 */

/*extract sf.roll_rate (Format E14.6)*/
/*convert & load into fdr055[15] The S/C roll rate at image center*/
sprintf(str_buf,"%-14.6e",sf.roll_rate);
   for(index=0;index<=13;index++)
      t_fdr_buf.fdr055[index] = str_buf[index];

/*extract sf.yaw_rate*/
/*convert & load into fdr056[15] The S/C yaw rate at image center*/
sprintf(str_buf,"%-14.6e",sf.yaw_rate);
   for(index=0;index<=13;index++)
      t_fdr_buf.fdr056[index] = str_buf[index];

/*extract sf.pitch_rate*/
/*convert & load into fdr057[15] The S/C pitch rate at image center*/
sprintf(str_buf,"%-14.6e",sf.pitch_rate);
   for(index=0;index<=13;index++)
      t_fdr_buf.fdr057[index] = str_buf[index];

/*3 attitude rate quality flags, 058-060 */

/*extract sf.re_nadir*/
/*convert & load into fdr061[17] Radius of the Earth at nadir*/
sprintf(str_buf,"%-16.7f",sf.re_nadir);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr061[index] = str_buf[index];

/*extract sf.re_image_ctr*/
/*convert & load into fdr062[17] Radius of the Earth at image center*/
sprintf(str_buf,"%-16.7f",sf.re_image_ctr);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr062[index] = str_buf[index];

/*convert & load into fdr063[17] Incidence angle at image center.*/
sprintf(str_buf,"%-16.7f",sf.inc_image_ctr);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr063[index] = str_buf[index];

/*=0.0125 x ap_rmlines*/
/*convert & load into fdr025[17] The actual swath width (km) coverage */
/*in the range direction*/
val_flt = 0.0125 * ap_rmlines;
if (ptype == CPX ||
	(ptype == RPR && Cur_Rqst->gnd_slnt_rg[0] == 'S'))
   val_flt = (ap_rmlines * spf.srspace) 
	     / (1000.0 * sin((pi / 180.0) * sf.inc_image_ctr));
if (ptype == CSD)
   val_flt = 100.0;
sprintf(str_buf,"%-16.7f",val_flt);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr025[index] = str_buf[index];

/*extract sf->asp_ver*/
/*convert & load into fdr064[8] The version number of the ASP H/W*/
strncpy(t_fdr_buf.fdr064,sf.asp_ver,strlen(sf.asp_ver));

/* image processing type */
/*=SF,Standrd,full-res,=QF,Quick-look,full-res*/
/*sf->proc_type load into fdr065[3]*/
strncpy(t_fdr_buf.fdr065,sf.proc_type,strlen(sf.proc_type));

/* An identifier describing the type of */
/*ephemeris used. read from sv1.precision */
/*convert & load into fdr066[2]*/
			/*1=Given/predicts*/
			/*2=Restituted*/
			/*3=Preliminary*/
			/*4=Precise*/
sprintf(str_buf,"%-1d",sv1.precision);
    t_fdr_buf.fdr066[0] = str_buf[0];

/*=1 if CPX,else 4*/
/*convert & load into fdr067[17] effective no. of looks in azimuth.*/
sprintf(str_buf,"%-16.7f",(ptype == CPX) ? 1.0 : 4.0);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr067[index] = str_buf[index];

/*extract pp->h_az*/
/*convert & load into fdr069[17] weighting pedestal height in azimuth.*/
sprintf(str_buf,"%-16.7f",pp->h_az);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr069[index] = str_buf[index];

/*extract rtf.h*/
/*convert & load into fdr070[17] weighting pedestal height in range.*/
sprintf(str_buf,"%-16.7f",rtf.h);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr070[index] = str_buf[index];

/*=NOT if CPX,else YES*/
/*convert & load into fdr071[4] look energy normalization flag*/

/*extract sp->pre->rec_gain*/
/*convert & load into fdr074[17] Receiver gain*/
sprintf(str_buf,"%-16.7f",sp->pre->rec_gain);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr074[index] = str_buf[index];

/*extract pp->swth_speed*/
/*convert & load into fdr075[17] Swath velocity*/
sprintf(str_buf,"%-16.7f",pp->swth_speed);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr075[index] = str_buf[index];

/*Squint angle sf.squint */
/* load into fdr076[17];(Format F16.7)*/
sprintf(str_buf,"%-16.7f",sf.squint);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr076[index] = str_buf[index];

/*extract Cur_Rqst->ave_hght*/
/*convert & load into fdr077[17] Ave. terrain height above Geoid at*/
/*image center*/
sprintf(str_buf,"%-16.7f",Cur_Rqst->ave_hght);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr077[index] = str_buf[index];
  
/*extract pp->pro_gain and Cur_Rqst->proc_gain */
/*convert & load into fdr078[4] Processor gain*/
sprintf(str_buf,"%-3d",(pp->pro_gain + Cur_Rqst->proc_gain));
   for(index=0;index<=2;index++)
      t_fdr_buf.fdr078[index] = str_buf[index];
	
/*extract Cur_Rqst->deskew load into fdr079[4] */
/*A flag to indicate whether Doppler Skew was removed.*/
			/*= NOT, Deskew not applied*/
			/*= YES, Deskew applied*/
if (ptype == RPR)
    strncpy(t_fdr_buf.fdr079,Cur_Rqst->deskew,strlen(Cur_Rqst->deskew));
if (ptype == CPX)
    strncpy(t_fdr_buf.fdr079,"NOT ",4);

/*extract Cur_Rqst->gnd_slnt_rg*/
/*load into fdr080[7] =GROUND,ground range,=SLANT,slant range*/
if (ptype == STD || ptype == QLK || 
	(ptype == RPR && Cur_Rqst->gnd_slnt_rg[0] == 'G'))
    strncpy(t_fdr_buf.fdr080,"GROUND",6);
  
/*extract sf.r_near*/
/*convert & load into fdr081[17] Slant range to the first image pixel.*/
sprintf(str_buf,"%-16.7f",sf.r_near);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr081[index] = str_buf[index];

/*extract sf.r_far*/
/*convert & load into fdr082[17] Slant range to the last image pixel.*/
sprintf(str_buf,"%-16.7f",sf.r_far);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr082[index] = str_buf[index];

/*=something else if CPX*/
/*fdr083[9];  start sample processed = 1*/

/*=NOT if CSD or sw_clutter_lock == 0,else YES*/
/*convert & load into fdr084[4] flag to indicate whether Clutterlock*/
if (ptype == CSD || sw_clutter_lock == 0)
    strncpy(t_fdr_buf.fdr084,"NOT ",4);

/*extract sf.fdga*/
/*convert & load into fdr085[17] Doppler frequency at the near range.*/
sprintf(str_buf,"%-16.7f",sf.fdga);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr085[index] = str_buf[index];

/*extract sf.fdgb*/
/*convert & load into fdr086[17] Doppler frequency slope*/
sprintf(str_buf,"%-16.7f",sf.fdgb);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr086[index] = str_buf[index];

/*extract sf.fdgc*/
/*convert & load into fdr087[17] Doppler frequency quadratic term.*/
sprintf(str_buf,"%-16.7f",sf.fdgc);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr087[index] = str_buf[index];

/*NOT if CSD or sw_auto_focus == 0,else YES*/
/*convert & load into fdr088[4] flag to indicate whether Autofocusing.*/
if (ptype == CSD || sw_auto_focus == 0)
    strncpy(t_fdr_buf.fdr088,"NOT ",4);

/*extract sf.fdotga*/
/*convert & load into fdr089[17] Doppler frequency rate at near range.*/
sprintf(str_buf,"%-16.7f",sf.fdotga);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr089[index] = str_buf[index];

/*extract sf.fdotgb*/
/*convert & load into fdr090[17] Doppler frequency rate slope*/
sprintf(str_buf,"%-16.7f",sf.fdotgb);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr090[index] = str_buf[index];

/*extract sf.fdotgc*/
/*convert & load into fdr091[17] Doppler frequency rate quadratic term*/
sprintf(str_buf,"%-16.7f",sf.fdotgc);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr091[index] = str_buf[index];

/*30m-high res,240m-low res*/ 
/*convert & load into fdr092[17] Nominal image resolution in azimuth.*/
if (ptype == CPX) {
sprintf(str_buf,"%-16.7f",10.0);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr092[index] = str_buf[index];
}

/*30m-high res,240m-low res*/ 
/*convert & load into fdr093[17] Nominal image resolution in range.*/
if (ptype == CPX) {
sprintf(str_buf,"%-16.7f",10.0);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr093[index] = str_buf[index];
}

/*12.5m high res/ 100m low res*/
/*convert & load into fdr094[17] Pixel spacing in azimuth*/
if (ptype == CPX || ptype == CSD) {
    spacing = pp->swth_speed / sp->pre->prf;
    sprintf(str_buf,"%-16.7f",spacing);
       for(index=0;index<=15;index++)
	  t_fdr_buf.fdr094[index] = str_buf[index];
}
  
/*12.5m high res/ 100m low res*/
/*convert & load into fdr095[17] Pixel spacing in range*/
if (t_fdr_buf.fdr080[0] == 'S') {
    spacing = c / (2.0 * spf.csr);
    sprintf(str_buf,"%-16.7f",spacing);
       for(index=0;index<=15;index++)
	  t_fdr_buf.fdr095[index] = str_buf[index];
}
  
/*NOT*/
/*load into fdr096[4] On-board range compression flag.*/
/*= NOT, No on-board range compression*/
/*= YES, On-board range compression applied*/

/*extract spf.nbits*/
/*convert & load into fdr097[5] Bits per sample of SAR signal data*/
sprintf(str_buf,"%-4d",spf.nbits);
   for(index=0;index<=3;index++)
      t_fdr_buf.fdr097[index] = str_buf[index];
  
/*fdr098[17];  calibrator estimate (= 0)*/

/*extract pp->ber*/
/*convert & load into fdr099[17] Data transfer bit error rate*/
sprintf(str_buf,"%-16.7f",pp->ber);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr099[index] = str_buf[index];

/*extract pp->snr -- use the new value for snr calculated */
/*  in write_ceos.c based on IOM#3340-92-003 */
/*convert & load into fdr100[17] Signal to noise ratio*/
sprintf(str_buf,"%-16.7f",sf.snr);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr100[index] = str_buf[index];

/*extract spf.est_noise_floor*/
/*convert & load into fdr101[17] Estimated noise floor*/
sprintf(str_buf,"%-16.7f",spf.est_noise_floor);
   for(index=0;index<=15;index++)
      t_fdr_buf.fdr101[index] = str_buf[index];

/*fdr102[17];   radiometric resolution (=0.1)*/

/*The number of saturated points determined from image histogram.*/
/*sf.sat_pnt = bin value 256 from histogram*/
/* convert & load into fdr103[9];*/
sprintf(str_buf,"%-8u",sf.sat_pnt);
   for(index=0;index<=7;index++)
      t_fdr_buf.fdr103[index] = str_buf[index];

/*flag to indicate whether image is  within specification.*/
/*sf->spec_flg load into fdr104[4]*/

/*comments field reserved for documenting image anomalies.*/
/*sf->comment load into fdr105[100]*/
strncpy(t_fdr_buf.fdr105,sf.comment,strlen(sf.comment));


/*write T_FDR_FILE1 */
if(fwrite(&t_fdr_buf,T_FDR_SIZE,1,fp1)!=1)
  printf("File write error\n");


return (PASS);

}
