#ifndef _SCENE_FILE_H
#define _SCENE_FILE_H

static char sccsid_scene_file_h[] = 
    "@(#)scene_file.h	1.2 96/04/09 20:29:11";

/*scene_file structure*/

typedef struct {	
char sw_id[13];		/*software id*/
char file_name[17];	/*file name*/
char tme_scene_ctr[33];	/*Input scene center time*/
			/*YYYYMMDDhhmmssttt$$$$$$$$$$$$$ where:*/
			/*YYYY = year*/
			/*MM = month */
			/*DD = day */
			/*hh = hours (00 to 23) */
			/*mm = minutes (00 to 59)*/
			/*ss = seconds (00 to 59)*/
			/*ttt = milliseconds (000 to 999)*/
double lat_scene_ctr;	/*Processed scene center geodetic latitude*/
double lon_scene_ctr;	/*Processed scene center geodetic longitude*/
int lnum_scene_ctr;	/*Scene center line number*/
int pnum_scene_ctr;	/*Scene center pixel number*/
char sp_id[17];		/*Sensor platform mission identifier*/
float wndw_pos;		/*Data window position*/
float iq_gain_imb;	/*Gain imbalance for I & Q*/

	/*First(beginning) positional data point*/
double x_begin;	
double y_begin;
double z_begin;
double vx_begin;
double vy_begin;
double vz_begin;

	/*Second (mid) positional data point*/
double x_mid;	
double y_mid;
double z_mid;
double vx_mid;
double vy_mid;
double vz_mid;

	/*Third (end) positional data point*/
double x_end;
double y_end;
double z_end;
double vx_end;
double vy_end;
double vz_end;

float pitch_rate;	/*Pitch rate (degrees/sec)*/
float roll_rate;	/*Roll rate (degrees/sec)*/
float yaw_rate;		/*Yaw rate (degrees/sec)*/


/* meta data */
char image_id[12];	/*ImageID NNNNNNNTss format where*/
                   	/*T	= 1.Full-res image */
			/*	= 2, Lo-res image */
                	/*	= 3, Geocoded full-res */
                	/*    	= 4, Geocoded lo-res */
                	/*   	= 5, Complex */
                	/*    	= 6, CCSD */
double lat_a;		/*Latitude at the start  of the image frame*/
			/*in near swath*/
double lon_a;		/*Longitude at the start of the image frame*/
			/*in the near swath*/
double lat_d;		/*Latitude at the end of the image frame in*/
			/*the near swath*/
double lon_d;		/*Longitude at the end of the image frame*/
			/*in the near swath*/
double lat_b;		/*Latitude at the start of the image frame in*/
			/*the far swath*/
double lon_b;		/*Longitude at the start of the image frame in*/
			/*the far swath*/
double lat_e;		/*Latitude at the end of the image frame in */
			/*the far swath*/
double lon_e;		/*Longitude at the end of the image frame in*/
			/*the far swath*/
int np;    		/*The total (with filler data) number of */
			/*pixels per line.*/
int nl;			/*The total (with filler data) number of */
			/*image lines.*/
char media_labl[7];	/*Identification label of the media that the*/
			/*ID image was written to in format: ccnnnn*/ 
			/* cc 	=FE, ESA tape; */
                	/*    	=FN, NASDA tape; */
                	/*   	=FC, CCRS tape; */
                	/*  	=AS, Archive signal DCRSi; */
                	/*	=WS, Working signal DCRSi */
int dcrs_start;		/*block # on DCRSi where data begins. */
int dcrs_end;		/*block # on DCRSi where data ends. */
float altitude;		/*altitude of spacecraft at image center*/ 
float re_nadir;		/*Radius of the Earth at nadir*/
float re_image_ctr;	/*Radius of the Earth at center of the image.*/
char asp_ver[8];	/*The version number of the ASP.*/
char proc_type[3];	/*image processing type*/
			/*SF=standard full res,QF=guick look full res*/
char spec_flg[4]; 	/*A flag to indicate whether image is  within*/
			/*specification.*/
char comment[101];  	/*A comments field reserved for documenting*/
			/*image anomalies.*/
double r_near;		/*slant range to first image pixel*/
double r_mid;		/*slant range to middle image pixel*/
double r_far;		/*slant range to last image pixel*/
double fdga;		/*ground Doppler coef */
double fdgb;		/*ground Doppler coef */
double fdgc;		/*ground Doppler coef */
double fdotga;		/*ground Doppler rate coef */
double fdotgb;		/*ground Doppler rate coef */
double fdotgc;		/*ground Doppler rate coef */

/**********************************************************************/

float tru_hd;		/*Processed scene center true heading(deg.)   */
char snsr_mode[33];	/*Sensor and mode of operation.		      */
			/*AAAAAA-BB-CCCCCCDDDDDD-EFbbbbbbb	      */
                       	/*AAAAAA= TBD, Sensor ID 		      */
                        /*BB 	= Lb, L-band; 			      */
                        /*  	= Cb, C-band (b is a trailing  blank) */
                        /*CCCCCC= HIGH, High res,= LOW, Low res       */
                        /*DDDDDD= TBD, Code for imaging mode          */
			/*(e.g., near,  far,  browse, scan, etc.)     */
                        /*E 	= H, Horizontal,= V, Vertical         */
                        /*F 	= H, Horizontal,= V, Vertical 	      */
double sp_lat;		/*Sensor Platform geodetic Lattitude	      */
double sp_lon;		/*Sensor Platform geodetic Longitude	      */
float sp_hd;		/*Sensor Platform Heading(deg.)		      */
float inc_scene_ctr;	/*Incidence angle at scene center	      */
float rnge_gate;	/*Range gate at early edge(usec)	      */
float rec_gain_lp;	/*Receiver gain for like polarized(dB)	      */
float bw_lk_range;	/*Bandwidth per look in Range ( Hz )	      */
float bw_ttl_range;	/*Total processor bandwidth in Range	      */
float gmh_ang;		/*Greenwich mean hour angle (degrees)	      */
float al_pos_err;	/*Along track position error (meters)	      */
float ac_pos_err;	/*Across track position error (meters)	      */
float rad_pos_err;	/*Radial position error (meters/sec)	      */
float al_vel_err;	/*Along  track vel. error (meters/sec)	      */
float ac_vel_err;	/*Across track vel. error (meters/sec)	      */
float rad_vel_err;	/*Radial vel. error(degrees/sec)	      */
float noise_scl_fac;	/*Noise scale factor(a1)		      */
float lnr_conv_fac;	/*Linear conversion factor(a2)		      */
float off_conv_fac;	/*Offset conversion factor(a3)		      */

char cal_update[7];	/*Date of last calibration update as YYMMDD   */
			/*YY = last two digits of year		      */
			/*MM = month of the year		      */
			/*DD = day of the month			      */
float al_loc_err;	/*Nom. abs loc error along track (meters)     */
float ac_loc_err;	/*Abs loc error cross track (meters)	      */
float lne_dist;	/*Nom.geometrc distortn scale in line directn	      */
float pxl_dist;	/*Nom.geometrc distortn scale in pixel directn	      */
float dist_skew;	/*Nom.geometrc distortn skew		      */
	
	/*HISTOGRAM TABLE DATA SET DESCRIPTION*/
char htgm_dscpt[33];	/*Histogram descriptor			      */
int htgm_seq;		/*Seq. no. in the full histogram table	      */
int lne_smpl_num;	/*Total # of data samples in line directn (P) */
int ac_smpl_num;	/*Total # of data samples across lines (L)    */
int lne_grp_size;	/*Data samples group size in line directn (M) */
int ac_grp_size;	/*Data samples group size across lines (N)    */
int lne_num_smpl;	/*# of samples per group in line directn (k)  */
int ac_num_smpl;	/*# of samples per group across lines (l)     */

	/*Data Statistics for i component*/
int imin_frst;		/*Min sample value of 1st. histogram table bin*/
int imax_last;		/*Max sample value of last histogram table bin*/
int ival_inc;		/*Sample value increment		      */


	/*Data Statistics for q component*/
int qmin_frst;		/*Min sample value of 1st. histogram table bin*/
int qmax_last;		/*Max sample value of last histogram table bin*/
int qval_inc;		/*Sample value increment		      */


int num_smpl_off;	/*# of smpl offset from 1st smpl in range line*/
int num_rl;	     	/*# of range lines integrated for spectra     */
float frst_ctr_freq;	/*Center frequency of first spectra bin (Hz)  */
float lst_ctr_freq;	/*Center frequency of last spectra bin (Hz)   */
float min_sptrl_pwr;	/*Minimum spectral power (dB)		      */
float max_sptrl_pwr;	/*Maximum spectral power (dB)		      */

int np_nflr;		/*The actual (without filler data) number of  */
			/*pixels per line			      */
int nl_nflr;		/*The actual (without filler data) number of  */
			/*image lines				      */
float trk_ang;		/*Track angle to True North		      */
char asc_dsc[2];	/*Flag indicating whether the pass is	      */
			/*ascending(A) or descending(D)		      */
int roll_qlty;		/*quality flag for S/C roll  at image center  */
int yaw_qlty;		/*quality flag for S/C yaw   at image center  */
int pitch_qlty;		/*quality flag for S/C pitch at image center  */
int roll_rate_qlty;	/*quality flag for roll  rate at image center */
int yaw_rate_qlty;	/*quality flag for yaw   rate at image center */
int pitch_rate_qlty;	/*quality flag for pitch rate at image center */
float inc_image_ctr;	/*Incidence angle at the center of the image. */
float az_dist;		/*processing induced distortions in azimuth   */
float rg_dist;		/*processing induced distortions in range     */
float squint;		/*squint angle				      */
float cal_est;		/*Calibrator estimate			      */
float rad_res;		/*Radiometric resolution		      */
int sat_pnt;  		/*The number of saturated points determined   */
			/*from image histogram.			      */
float rcf_avg;		/*average of radiometic compensation function */
float snr;		/*more accurate SNR from image avg & noise    */
float lk_angle;		/*look angle				      */

	/*RADIOMETRIC DATA SET-LOOK UP TABLE VALUES		      */
float radio_val[256];

	/*SPECTRAL DATA TABLE VALUES*/
float spectra_val[128];

	/*HISTOGRAM TABLE VALUES*/
	/*256 table values- i component*/
int i_hstgrm[256];

	/*256 table values- q component*/
int q_hstgrm[256];

}SCENE_FILE,*SCENE_FILE_PTR;

#endif /* ! _SCENE_FILE_H */
