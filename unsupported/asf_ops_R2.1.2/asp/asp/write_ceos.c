/* Alaska SAR Processor (ASP) %W% %E% %U% */
/* CLW 12-18-95 Hardcode 29.8 for mech_sight for RADARSAT (Ming Chen) */
/* CLW 12-14-95 Fill in Cal powers from scan_result file */
/* CLW 12-08-95 Fill in product_type for DSS record */
/* EYC 02-12-95 Calculate radar frequency from wavelength */

#include <stdio.h>
#include <stdarg.h>
#include <math.h>
#include <strings.h>
#include <ctype.h>
#include <time.h>
#include <defs.h>
#include <extern.h>
#include <scene_file.h>
#include <procfil.h>

/* These are global variables that ceos s/w needed to be defined */

struct ceos_struct CEOS;
int Debug = FALSE;
SARL_ptr *Global_tree;
unsigned char* work;

#define PASS 0
#define FAIL -1

extern SCENE_FILE sf;
extern RQST_PTR Cur_Rqst;              /* current request */
extern TC_FILE tcf;                    /* time correlation element */
extern SV               sv1,sv2;

/* parameter files */

extern DPP_FILE dpf;                   /* default processing parameters file */
extern SP_FILE spf;                    /* sensor parameters file */
extern EM_FILE emf;                    /* Earth model file */
extern RTF_FILE rtf;                   /* range transfer function file */

extern int ap_looks;          /* 1 or 4 look data */
extern int ap_rmlines;		/* # of range migration output lines */
extern int      sw_clutter_lock;
extern int      sw_auto_focus;
extern int	vbose;
extern char	PROC_PATH[80];
extern int ucs_product;
extern char scan_result_filename[50];
extern char cal_params_filename[50];
extern char scan_result_version[30];
extern char cal_status[50];
extern char cal_comment[300];
extern double noise_fct;
extern double linear_conv_fct;
extern double offset_conv_fct;
extern double islr_rng, pslr_rng;
extern double range_res, azimuth_res;
extern double range_ambig, azimuth_ambig;
extern double ori_error, dist_skew;
extern double crosst_scale, alongt_scale;
extern double crosst_locerr, alongt_locerr;
extern double rel_radio_acc;

enum { LOW_RES, FULL_RES, BOTH_IMAGE };
extern int Res_type;

SARL_ptr* Rd_LDR_template();
static TAPE_SEG_PTR sp;
static PP_BLOCK_PTR pp;
float nesz;

void Fill_str( char *string, int len, char *fmt, ... )
{
	va_list args;
	int l;

	va_start(args,fmt);
	vsprintf(string,fmt,args);
	l = strlen(string);
	if ( l < len ) memset(&string[l],' ',len-l);
	string[len-1] = '\0';
	va_end(args);
}
int write_ceos( sp_in, pp_in, outfile )
	TAPE_SEG_PTR sp_in;
	PP_BLOCK_PTR pp_in;
	char *outfile;
{
	SARL_ptr* tree;

	CEOS.out=TRUE;
	CEOS.debug = 0;
	CEOS.inputType = FROM_LDR;

	sprintf(CEOS.infile,"%sceos_templates/ceos_ldr_init",PROC_PATH);
	if ( !strcmp(Cur_Rqst->type,"CPX") ) strcat(CEOS.infile,".X.");	
	else if ( !strcmp(Cur_Rqst->type,"CSD") ) strcat(CEOS.infile,".C.");	
	else strcat(CEOS.infile,".S.");
	strncat(CEOS.infile,Cur_Rqst->take_id,2);
	strcpy(CEOS.outfile,outfile);

        if (CEOS.inputType == FROM_LDR) {
           if ( (tree = Rd_LDR_template()) == NULL ){
		return(FAIL);
	   };
	   sp = sp_in;
	   pp = pp_in;
	   Fill_LDR_values( tree );
	   Write_CEOS_SARL( CEOS.outfile, tree);
        }
	Free_SARL( tree );
	free( tree );
	return(PASS);
}
SARL_ptr* Rd_LDR_template() 
{

   FILE *in;
   SARL_ptr* tree;
   int ret;
   
   /* open the leader file */
   if ( ( in = Open_LDR(CEOS.infile, READ_ONLY) ) == NULL) {
      printf("\n Could not open the leader file for reading --- %s",CEOS.infile);
      return(NULL);
   }
 
   /* allocate memory for a SARL structure */

   if ( ( tree = Allocate_SARL() ) == NULL) {
      printf("\n Rd_LDR_template Failed to Allocate_SARL");
      return(NULL);
   }

   SetCurrentSARL( tree );

   /* read the File Descriptor Record, transfer to "descript" structure */

   if ( !Read_FDR_SARL( in, tree ) ) {
      printf("\n Error in Read_FDR_SARL\n");
	free( tree );
      return(NULL);
   }

   /* based on the contents of the leader file FDR, allocate space for 
      each record */

   if ( Allocate_SARL_ODR(tree) ) {
      ret=Read_ALL_SARL( in, tree ); 
      if ( ret == END_OF_FILE ){
		fclose(in);
   		return(tree);
      }
      else {
              	printf("\n Aborting the read of the input file\n");
		Free_SARL( tree );
		free( tree );
		fclose(in);
   		return(NULL);
      }
   } else {
	Free_SARL( tree );
	free( tree );
      fclose(in);
      return(NULL);
   }
}
int Fill_LDR_values( SARL_ptr* d )
{
	int status;
	CEOS_INFO cip;

	/* call to compute some last minute stuff to be put into CEOS */
	if (leader_info(sp,pp,&cip) == FAIL) {
	  printf("...Failed in making leader info.\n");
	  return (FAIL);
	}
	if (trailer_info(sp,pp) == FAIL) {
	  printf("...Failed in making trailer info.\n");
	  return (FAIL);
	}

	Fill_FDR_values( &(d->descript) );
	Fill_DSS_values( d->data_sum );
	Fill_PP_values( d->platform );
/*	d->data_sum->plat_long -= d->platform->hr_angle;
	if( d->data_sum->plat_long < -180.0 ) d->data_sum->plat_long += 360.0;
*/
	Fill_AT_values( d->attitude );
	if ( d->radio_data != NULL ) Fill_RD_values( d->radio_data, &cip );
	if ( d->data_qual != NULL ) Fill_DQS_values( d->data_qual );
	Fill_DH_values( d->histogram, &cip );
	Fill_RS_values( d->spectra );	
	Fill_FR_values( d->facility, d->data_sum, d->histogram );

	return(PASS);
}
int Fill_FDR_values( Sarl_Desc_Rec* d )
{
	Fill_str( d->software_id, sizeof(d->software_id),
			"%s%s", sf.sw_id, sf.asp_ver );

	/* PnRRRRRFFFMDSvvv */
	Fill_str( d->product_id, sizeof(d->product_id), 
		"%.2s%.5s%.9s", Cur_Rqst->take_id,
		&Cur_Rqst->take_id[5], Cur_Rqst->site );
	return(PASS);
}
int Fill_DSS_values( Dataset_Sum* d )
{
	double  val_dbl;
	double xx,yy,zz,dd1,dd2,len,sc_dist,heading;
	GMT     tempgmt;
	int     year,month,day,i;
	double  pi = 3.141592653589793;
	double  c = 2.998E8;
	char str_buf[80];

	/* PnRRRRRFFFMDSvvv */
	Fill_str( d->product_id, sizeof(d->product_id), 
		"%.2s%.5s%.9s", Cur_Rqst->take_id,
		&Cur_Rqst->take_id[5], Cur_Rqst->site );

	strncpy( str_buf, sf.tme_scene_ctr, 4 );
	str_buf[4] = '\0';
	year = atoi(str_buf);
	strncpy(str_buf,&sf.tme_scene_ctr[4],3);
	str_buf[3] = '\0';
	day = atoi(str_buf);
	get_month_day(year,day,&month,&day);
	Fill_str( d->inp_sctim, sizeof(d->inp_sctim), 
		"%.4d%.2d%.2d%.2s%.2s%.2s%.3s",
		year,month,day,&sf.tme_scene_ctr[8],
		&sf.tme_scene_ctr[11], &sf.tme_scene_ctr[14],
		&sf.tme_scene_ctr[17]);
	if ( sf.asc_dsc [0] == 'A' )
		Fill_str( d->asc_des, sizeof(d->asc_des), "ASCENDING" );
	else
		Fill_str( d->asc_des, sizeof(d->asc_des), "DESCENDING" );

	d->pro_lat = sf.lat_scene_ctr;
	d->pro_long = sf.lon_scene_ctr;
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

/* fix the heading angle near equator CV */
	if ((Cur_Rqst->take_id[0]=='J') && (sf.z_begin>0.0) && (sf.z_end<0.0))
      	    heading = (180.0 / pi) * asin(len * fabs(dd2 + dd1) / sc_dist);
	else
   	    heading = (180.0 / pi) * asin(len * fabs(dd2 - dd1) / sc_dist);

/* Ming's cheap fix for heading angle, 2-25-94 */

	if (vbose) printf("dd1=%g, dd2=%g\n",dd1, dd2);

	if ((Cur_Rqst->take_id[0]=='J') && (sf.z_begin>0.0) && (sf.z_end<0.0)){
           printf("JERS near the equator\n");
           heading = 270.0 - heading;
	}
	else if (Cur_Rqst->take_id[0] == 'J' && sf.z_end < 0.0) {
    	   if (vbose) printf("JERS in SOUTH atmosphere!\n");
    	   if (dd2 > dd1 || (dd2 == dd1 && sf.z_mid > 0.0))
       		heading = 270.0 - heading;
    	   else
       		heading = 270.0 + heading;
	   }
	else {
    	   if (dd2 > dd1 || (dd2 == dd1 && sf.z_mid > 0.0))
       		heading = 270.0 + heading;
    	   else
       		heading = 270.0 - heading;
	}
	d->pro_head = (float) heading;

	d->ellip_maj = emf.r_e;
	d->ellip_min = emf.r_pole;

	d->terrain_h = Cur_Rqst->ave_hght;
	d->sc_lin = sf.lnum_scene_ctr;
	d->sc_pix = sf.pnum_scene_ctr;
	d->scene_len = 0.0125 * (sf.lnum_scene_ctr * 2);
	d->scene_wid = 0.0125 * (sf.pnum_scene_ctr * 2);

	/*strncpy(&d->sensor_id[10],"HR",2);*/
	Fill_str( d->revolution,sizeof(d->revolution), "   %5.5s",
				&Cur_Rqst->take_id[5] );

	d->plat_lat = (float)sf.sp_lat;
	d->plat_long = (float)sf.sp_lon;
	d->plat_head_scene = sf.trk_ang;
	d->incident_ang = sf.inc_image_ctr;
	d->wave_length = spf.xlambda;
	if( spf.xlambda>0.0 ) d->frequency = 0.299792458 / spf.xlambda;

	d->ampl_coef[1] = 0.0000000; /* CV for Tom 6/6/97 */
	d->phas_coef[2] = (spf.bw_ra / spf.tau) * 1.0e12 * 2 * pi;
	d->rng_samp_rate = spf.csr / 1000000.0;
	d->rng_gate = sf.wndw_pos;
	d->rng_length = spf.tau;
	d->chn_bits = spf.nbits;
/*	d->i_bias = (pp->imean_cor)-(pp->imean);
	d->q_bias = (pp->qmean_cor)-(pp->qmean);
*/
	if(Cur_Rqst->take_id[0] == 'J') { /* CV 6/6/97 */
	    d->i_bias = (pp->imean_cor)-(pp->imean);
	    d->q_bias = (pp->qmean_cor)-(pp->qmean);
	} else if(Cur_Rqst->take_id[0] == 'E') {
	    d->i_bias = 15.5; 
	    d->q_bias = 15.5;
	} else if(Cur_Rqst->take_id[0] == 'R') {
	    d->i_bias = 7.5; 
	    d->q_bias = 7.5;
	}	
	d->iq_ratio = sf.iq_gain_imb;
	d->ele_sight = spf.gamma_nom;
	if(Cur_Rqst->take_id[0] == 'R')
	     d->mech_sight = 29.8;
	else d->mech_sight = spf.gamma_nom;
	d->prf = sp->pre->prf;
	d->azi_beam = (180.0 / pi) * (0.88 * spf.xlambda) / spf.ant_len;
	Fill_str( d->sat_bintim, sizeof(d->sat_bintim), "%16u", tcf.bt );

	/* for J-ERS-1 need to add the offset back */
	tempgmt = tcf.gmt;
	if(Cur_Rqst->take_id[0] == 'J') {
	        val_dbl = (double) (tcf.delta) / 1000.0;
	        add_seconds(&tempgmt,val_dbl);
	}
	get_month_day(tempgmt.yr,tempgmt.day,&month,&day);
	/*load Satellite clock time<YYYYMMDDhhmmssttt$$$..$>*/
	Fill_str( d->sat_clktim, sizeof(d->sat_clktim),
			"%.4d%.2d%.2d%.2d%.2d%6.3f",tempgmt.yr,month,day,
	        	tempgmt.hr,tempgmt.min,tempgmt.second);
	d->sat_clkinc = tcf.delta;
	Fill_str( d->ver_id, sizeof(d->ver_id), "%s", sf.asp_ver );
	Fill_str( d->fac_code, sizeof(d->fac_code), "%s", &pp->img_name[1] );
	if ( !strcmp(Cur_Rqst->type,"CPX") )
	    Fill_str(d->product_type, sizeof(d->product_type), "COMPLEX");
	if ( strcmp(Cur_Rqst->type,"CSD") ){
	    d->n_azilok = ap_looks;
	    d->bnd_azilok = pp->bw_az4_act/ap_looks;
	    d->bnd_rnglok = spf.bw_ra;
	    d->bnd_azi = spf.pbw;
	    d->bnd_rng = spf.bw_ra;
	    d->rng_res = range_res;	/* CV 4/3/97 */
	    d->azi_res = azimuth_res;	/* CV 4/3/97 */
	    d->alt_dopcen[0] = sf.fdga;
	    d->crt_dopcen[0] = sf.fdga;	
	    d->crt_dopcen[1] = sf.fdgb;
	    d->crt_dopcen[2] = sf.fdgc;
	    d->alt_rate[0] = sf.fdotga;
	    d->crt_rate[0] = sf.fdotga;
	    d->crt_rate[1] = sf.fdotgb;
	    d->crt_rate[2] = sf.fdotgc;
	} 

	Fill_str( d->data_inpsrc, sizeof(d->data_inpsrc), "%s",
			Cur_Rqst->tape_id );
	if (!strcmp(Cur_Rqst->type,"CSD")){
		if (!sw_clutter_lock) Fill_str( d->clutterlock_flg, 
			sizeof(d->clutterlock_flg), "NOT" );
		if (!sw_auto_focus) Fill_str( d->auto_focus, 
			sizeof(d->auto_focus), "NOT" );
	}
	if ( !strcmp(Cur_Rqst->type,"CPX") || !strcmp(Cur_Rqst->type,"CSD") ) {
		d->line_spacing = pp->swth_speed / sp->pre->prf;
	}
	if (!strcmp(Cur_Rqst->type,"CPX") ||
		!strcmp(Cur_Rqst->type,"CSD") ||
		(!strcmp(Cur_Rqst->type,"RPR") && 
		 Cur_Rqst->gnd_slnt_rg[0] == 'S') ){
		d->pixel_spacing = c / (2.0 * spf.csr);
	}
	if ( Cur_Rqst->take_id[0] == 'R' ){
		Fill_str(d->beam1,sizeof(d->beam1),"ST%1.1d",
					sp->aux.beam_seq );
		d->prf1 = sp->pre->prf;
		d->rng_gate1 = sf.rnge_gate;
		d->az_ovlp_nxt_img = d->rg_off_nxt_img = 0; /* TBD */
	}	

	/*  Convert the following parameters to low-res */

	if ( Res_type == LOW_RES ){
	   /*strncpy(&d->sensor_id[10],"LR",2);*/
	   Fill_str(d->product_type, sizeof(d->product_type), "LOW");
	   d->sc_lin = d->sc_pix = 512;
	   printf("rng_res=%g,azi=%g\n",d->rng_res,d->azi_res);
	   d->n_azilok = d->n_azilok * 100.0 / d->azi_res;
	   d->n_rnglok = d->n_rnglok * 100.0 / d->rng_res;
	   d->bnd_azilok = d->bnd_azilok / d->n_azilok;
	   d->bnd_rnglok = d->bnd_rnglok / d->n_rnglok;
	   d->azi_res = azimuth_res * 8;
	   d->rng_res = range_res * 8;
	   d->crt_dopcen[1] *= 8.0;
	   d->crt_dopcen[2] *= 8.0*8.0;
	   d->crt_rate[1] *= 8.0;
	   d->crt_rate[2] *= 8.0*8.0;
	   d->line_spacing *= 8.0;
	   d->pixel_spacing *= 8.0;
	} 

	Fill_str(d->cal_params_file, sizeof(d->cal_params_file), "%s",
		cal_params_filename);
	Fill_str(d->scan_results_file, sizeof(d->scan_results_file), "%s",
		 scan_result_filename);
	Fill_str(d->scanner_version, sizeof(d->scanner_version), "%s",
		scan_result_version);
	Fill_str(d->decode_version, sizeof(d->decode_version), " ");
	
/* blank fill the remaining spares at the end */

	for( i=0; i<sizeof(d->spare_dss_16); i++ ) d->spare_dss_16[i] = ' ';

	return(PASS);
}
int Fill_PP_values( Pos_Data* d )
{
	int	i;
	double  kepler[6];
	char	str_buf[32];
	float   seconds,mlsc;
	int     year,day,hour,minute,month,day_month;
	double  ghangle,d_seconds;
	GMT     agmt;
	char	cur_dir[80];

	ctok(sf.x_begin,sf.y_begin,sf.z_begin,
		sf.vx_begin,sf.vy_begin,sf.vz_begin,kepler);
	for ( i = 0; i < 6; i++ ) d->orbit_ele[i] = kepler[i];

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
	strcpy( cur_dir, getcwd(cur_dir,80));
	chdir(PROC_PATH);
	get_gha_(&year,&day,&hour,&minute,&seconds,&ghangle);
	chdir( cur_dir );
	get_month_day(year,day,&month,&day_month);
	d_seconds = hour*3600.0 + minute*60.0 + seconds; /* seconds in day */

	d->year = year;
	d->month = month;
	d->day = day_month;
	d->gmt_day = day;
	d->gmt_sec = d_seconds;

	d_seconds = 102400.0 /(2.0 * pp->swth_speed);
	if(strcmp(Cur_Rqst->type,"CPX") == 0) d_seconds /= 2.0;
	d->data_int = d_seconds;
	d->hr_angle = ghangle;
	
	d->pos_vect[0].pos[0] = sf.x_begin;
	d->pos_vect[0].pos[1] = sf.y_begin;
	d->pos_vect[0].pos[2] = sf.z_begin;
	d->pos_vect[0].vel[0] = sf.vx_begin * 1000.0;
	d->pos_vect[0].vel[1] = sf.vy_begin * 1000.0;
	d->pos_vect[0].vel[2] = sf.vz_begin * 1000.0;
	d->pos_vect[1].pos[0] = sf.x_mid;
	d->pos_vect[1].pos[1] = sf.y_mid;
	d->pos_vect[1].pos[2] = sf.z_mid;
	d->pos_vect[1].vel[0] = sf.vx_mid * 1000.0;
	d->pos_vect[1].vel[1] = sf.vy_mid * 1000.0;
	d->pos_vect[1].vel[2] = sf.vz_mid * 1000.0;
	d->pos_vect[2].pos[0] = sf.x_end;
	d->pos_vect[2].pos[1] = sf.y_end;
	d->pos_vect[2].pos[2] = sf.z_end;
	d->pos_vect[2].vel[0] = sf.vx_end * 1000.0;
	d->pos_vect[2].vel[1] = sf.vy_end * 1000.0;
	d->pos_vect[2].vel[2] = sf.vz_end * 1000.0;

/* blank fill the remaining spares at the end */

	for( i=0; i<sizeof(d->spare_ppr_1); i++ ) d->spare_ppr_1[i] = ' ';

	return(PASS);
}
int Fill_AT_values( Att_Data* d )
{
	int mlsc;
	int i;

	/* Assuming there is only one point */

	d->att_vect->gmt_day = pp->sv.gmt.day;
	mlsc = pp->sv.gmt.hr*3600000 + pp->sv.gmt.min*60000 +
       		(int) (pp->sv.gmt.second*1000.0);
	d->att_vect->gmt_msec = mlsc;
	d->att_vect->pitch = pp->att.pitch;
	d->att_vect->roll = pp->att.roll;
	d->att_vect->yaw = pp->att.yaw;
	d->att_vect->pitch_rate = sf.pitch_rate;
	d->att_vect->roll_rate = sf.roll_rate;
	d->att_vect->yaw_rate = sf.yaw_rate;
	d->next = NULL;

/* blank fill the remaining spares at the end */

	for( i=0; i<sizeof(d->spare_adr_1); i++ ) d->spare_adr_1[i] = ' ';

	return(PASS);
}
int Fill_RD_values( Radi_Data* d, CEOS_INFO_PTR cp )
{
	short int d1[8192];
	int i,kount,g,sr;
	double ga,gb,gc,val,spacing,sum;

	d->noise_fact = sf.noise_scl_fac;
	d->linear_conv_fact = sf.lnr_conv_fac;
	d->offset_conv_fact = sf.off_conv_fac;

	/* read the radiometric compensation vector */
	p_get_mem_file(d1,8192,"rc_funct","");

	/* extract the desired values and convert to real */
	if (strcmp(Cur_Rqst->type,"CPX") && 
	    strcmp(Cur_Rqst->type,"CSD")) {
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
		} else val = 1.0;
		sf.radio_val[i] = val * val;
	    }
	    if (kount !=0) {
		sf.rcf_avg = sum/kount;
		if ( !cp->meansamp ) sf.snr = FAIL;
		else {
		   if (noise_fct <= 0.0) {
			sf.snr = noise_fct;	/* -9999.0000 */
			nesz = noise_fct;
		   } else {
	    	        sf.snr = 10.0*log10(((cp->meansamp * cp->meansamp) -
		     		     (sf.noise_scl_fac * sf.rcf_avg) ) /
		     		     (sf.noise_scl_fac * sf.rcf_avg) );
			nesz = 10.0*log10(sf.noise_scl_fac * sf.rcf_avg);
		   }
		}
/*
		printf("avg of rc_funct = %-16.7e\n",sf.rcf_avg);
		printf("histogram mean  = %-16.7e\n",cp->meansamp);
		printf("noise cal coeff = %-16.7e\n",sf.noise_scl_fac);
		printf("snr             = %-16.7e\n",sf.snr);
*/
	    }
	}
	/* for CPX, use slant range and use every 8th value */
	if (!strcmp(Cur_Rqst->type,"CPX")) {
	    kount = 0;
	    sum = 0.0;
	    for (i = g = 0; i < 256; g += 8, i++) {
		val = d1[g] / 32768.0;
		if(val != 0.0) {
			kount++;
			sum += val * val;
		} else val = 1.0;
		sf.radio_val[i] = val * val;
	    }
	    if (kount !=0) {
		sf.rcf_avg = sum/kount;
		if (noise_fct <= 0.0) {
		      sf.snr = noise_fct;	/* -9999.0000 */
		      nesz = noise_fct;		/* -9999.0000 */
		} else {
	    	        sf.snr = 10.0*log10(((2*cp->meansamp * cp->meansamp) -
		     		     (sf.noise_scl_fac * sf.rcf_avg) ) /
		     		     (sf.noise_scl_fac * sf.rcf_avg) );
			nesz = 10.0*log10(sf.noise_scl_fac * sf.rcf_avg);
	   	}
	    }
	}
	if ( ucs_product ) {
	     for ( i = 0; i < 256; i++ ) 
		 d->lookup_tab[i] = 1.0;
	}
	else {
	     for ( i = 0; i < 256; i++ ) 
		d->lookup_tab[i] = sf.radio_val[i];
	}
	return(PASS);
}
int Fill_DQS_values( Qual_Sum* d )
{
	int i;

	d->islr = islr_rng;
	d->pslr = pslr_rng;
	d->azi_ambig = azimuth_ambig;
	d->rng_ambig = range_ambig;
	d->snr = sf.snr;
	d->ber = pp->ber;
	d->rng_res = range_res;
	d->azi_res = azimuth_res;
	d->rad_res = 0.1;
	if ( Res_type == LOW_RES ){
		d->rng_res = range_res * 8;
		d->azi_res = azimuth_res * 8;
	}
/*	if ( Res_type == LOW_RES ){
		d->rng_res = d->azi_res = 210.;
	}
*/
/***
	d->dyn_rng = ;
	d->abs_rad_unc_db = ;
	d->rel_rad_unc[0][0] = ;
***/
	d->abs_rad_unc_db = 2.0;
	d->abs_rad_unc_deg = 0.0;
	d->rel_rad_unc[0][0] = rel_radio_acc;
	d->rel_rad_unc[1][0] = 0.0;

	if( strcmp(Cur_Rqst->type,"CSD") ) {
	    d->alt_locerr = alongt_locerr;
	    d->crt_locerr = crosst_locerr;
	}
	if( strcmp(Cur_Rqst->type,"CPX") && strcmp(Cur_Rqst->type,"CSD") ) {
	    d->alt_scale = alongt_scale;
	    d->crt_scale = crosst_scale;
	    d->dis_skew = dist_skew;
	    d->ori_err = ori_error;
	}
	d->nesz = nesz;
/***
	if( strcmp(Cur_Rqst->type,"CSD") ) {
	    d->misreg[0][0] = ;
	    d->misreg[1][0] = ;
	}
	d->nesz = ;
	d->enl = ;
	strcpy( d->tb_update, "      " );
***/
	Fill_str(d->cal_status, sizeof(d->cal_status), "%s", cal_status);

/* blank fill the remaining spares at the end */

	for( i=0; i<sizeof(d->spare_dqs_2); i++ ) d->spare_dqs_2[i] = ' ';

	Fill_str(d->cal_comment, sizeof(d->cal_comment), "%s", cal_comment);

	return(PASS);
}
int Fill_DH_values( Data_Hist* d_orig, CEOS_INFO_PTR cp )
{
	int     i;
	Data_Hist* d;
	Hist_Data_Set* hd_ptr;
	int	lr_hist[256];
	float	value[256];
	extern int in_linelen, in_vlines, in_scenelen;

/* Fill in signal histogram record */

	d = d_orig;

	/* I Component */
	hd_ptr = d->data_set;
        hd_ptr->ns_lin = in_linelen;
        hd_ptr->ns_pix = in_vlines * in_scenelen;
        hd_ptr->ngrp_lin = hd_ptr->ns_lin;
        hd_ptr->ngrp_pix = hd_ptr->ns_pix;
        hd_ptr->nsamp_lin = hd_ptr->ns_lin;
        hd_ptr->nsamp_pix = hd_ptr->ns_pix;
	for ( i = 0; i < hd_ptr->nbin; i++ ){
		hd_ptr->data_values_hist[i] = pp->ihist_cor[i];
		value[i] = (float) i;
	}
	hist_stat(hd_ptr->nbin,hd_ptr->data_values_hist,value,
		&hd_ptr->min_smp, &hd_ptr->max_smp,
		&hd_ptr->min_hist, &hd_ptr->max_hist,
		&hd_ptr->mean_smp, &hd_ptr->std_smp,
		&hd_ptr->mean_hist, &hd_ptr->std_hist);
	d->ltab = (long) ( hd_ptr->nhist*8 + 248 );

	/* Q Component */
	hd_ptr = hd_ptr->next;
        hd_ptr->ns_lin = in_linelen;
        hd_ptr->ns_pix = in_vlines * in_scenelen;
        hd_ptr->ngrp_lin = hd_ptr->ns_lin;
        hd_ptr->ngrp_pix = hd_ptr->ns_pix;
        hd_ptr->nsamp_lin = hd_ptr->ns_lin;
        hd_ptr->nsamp_pix = hd_ptr->ns_pix;
	for ( i = 0; i < hd_ptr->nbin; i++ ){
		hd_ptr->data_values_hist[i] = pp->qhist_cor[i];
		value[i] = (float) i;
	}
	hist_stat(hd_ptr->nbin,hd_ptr->data_values_hist,value,
		&hd_ptr->min_smp, &hd_ptr->max_smp,
		&hd_ptr->min_hist, &hd_ptr->max_hist,
		&hd_ptr->mean_smp, &hd_ptr->std_smp,
		&hd_ptr->mean_hist, &hd_ptr->std_hist);

	if ( !strcmp(Cur_Rqst->type,"CSD") ) return(PASS);

/* Fill in processed data historgram record */

	d = d->next;
	hd_ptr = d->data_set;

        hd_ptr->ns_lin = sf.nl_nflr;	/* use actual lines */
        hd_ptr->ns_pix = sf.np_nflr;	/* use actual pixels */
	if( hd_ptr->ns_pix==8192 ) hd_ptr->ns_pix = 8191;
        hd_ptr->ngrp_lin = hd_ptr->ns_lin;
        hd_ptr->ngrp_pix = hd_ptr->ns_pix;
        hd_ptr->nsamp_lin = hd_ptr->ns_lin;
        hd_ptr->nsamp_pix = hd_ptr->ns_pix;

	if ( Res_type == LOW_RES ){
	    hd_ptr->ns_lin = hd_ptr->ns_lin / 8;
	    hd_ptr->ns_pix = hd_ptr->ns_pix / 8;
            hd_ptr->ngrp_lin = hd_ptr->ns_lin;
            hd_ptr->ngrp_pix = hd_ptr->ns_pix;
            hd_ptr->nsamp_lin = hd_ptr->ns_lin;
            hd_ptr->nsamp_pix = hd_ptr->ns_pix;
	    if ( p_get_mem_file(lr_hist,512,"hstgrm",".lr") == PASS){
		for ( i = 0; i < hd_ptr->nbin; i++ ){
		    hd_ptr->data_values_hist[i] = lr_hist[i];
		    value[i] = (float) i;
		}
	    }
	} else {	/*  Full-Res  */
	    for ( i = 0; i < hd_ptr->nbin; i++ ){
		hd_ptr->data_values_hist[i] = cp->hstgrm[i];
		value[i] = (float) i;
	    }
	    hd_ptr->data_values_hist[0] -=	/* remove invalid zeros */
		sf.nl*sf.np - hd_ptr->ns_lin*hd_ptr->ns_pix;
	}
	if( hd_ptr->data_values_hist[0]<0 ) hd_ptr->data_values_hist[0] = 0;
	hist_stat(hd_ptr->nbin,hd_ptr->data_values_hist,value,
		&hd_ptr->min_smp, &hd_ptr->max_smp,
		&hd_ptr->min_hist, &hd_ptr->max_hist,
		&hd_ptr->mean_smp, &hd_ptr->std_smp,
		&hd_ptr->mean_hist, &hd_ptr->std_hist);
	return(PASS);
}
int Fill_RS_values( Rng_Spec* d )
{
	int i,j,k;
	short int       idata[128];
	float   sdata[2048 * 3], minavg, maxavg;
	FILE *fp0;

	for (i = 0; i < 128; i++) sf.spectra_val[i] = 0.0;
	minavg = 1.0e9;
	maxavg = -1.0e9;
	if ((fp0 = fopen("rng_spectra","r")) == NULL){
	  printf("Cannot open rng_spectra file\n");
	  if(p_get_mem_file(idata,128,"rng_spec","") == FAIL) {
	     printf("...Cannot open rng_spec file (for 2dfft)\n");
	     for (i = 0; i < 128; i++)
	        sf.spectra_val[i] = 0.0;
	  }
	  else {
	    for (k = 0; k < 128; k++) {
	      if (idata[k] > 0)
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
	      if (sf.spectra_val[k] > maxavg) maxavg = sf.spectra_val[k];
	      if (sf.spectra_val[k] < minavg) minavg = sf.spectra_val[k];
	      k++;
	  }
	}
	d->last_freq = spf.csr;
	d->min_power = minavg;
	d->max_power = maxavg;
	for ( i=0; i<128; i++ ) d->data_values_spec[i] = sf.spectra_val[i];
	for( ; i<256; i++ ) d->data_values_spec[i] = 0.0;

/* blank fill the remaining spares at the end */

	for( i=0; i<sizeof(d->spare_rsr_3); i++ ) d->spare_rsr_3[i] = ' ';

	return(PASS);
}
int Fill_FR_values( Fac_Related* d, Dataset_Sum* dss, Data_Hist* hist )
{
	int     ptype;
	float   val_flt;
	struct tm *gmtime(), *tp;
	GMT now;
	int index;
	double  pi = 3.141592653589793;
	double  c = 2.998E8;

#define STD 1
#define RPR 2
#define CPX 3
#define CSD 4
#define QLK 5

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

	/* SSrrrrrnn */
	Fill_str( d->datatake_ID, sizeof(d->datatake_ID), 
			"%.2s%.7s",
			Cur_Rqst->take_id, &Cur_Rqst->id[1]);

	/* FFFMDSvvv */
	Fill_str( d->image_ID, sizeof(d->image_ID), "%.9s", Cur_Rqst->site );

	index = 0;
	index = time(&index);
	tp = gmtime(&index);
	Fill_str( d->corr_year, sizeof(d->corr_year), "%d",
				tp->tm_year + 1900 );
	Fill_str( d->corr_GMT, sizeof(d->corr_GMT),
			"%.3d:%.2d:%.2d:%6.3f",tp->tm_yday+1,
			tp->tm_hour,tp->tm_min,(float)tp->tm_sec);
	Fill_str(d->site_name,sizeof(d->site_name),"%s",Cur_Rqst->site);
	Fill_str(d->data_year, sizeof(d->data_year), "%4.4s", sf.tme_scene_ctr);
	Fill_str(d->center_GMT,sizeof(d->center_GMT), "%s",
			&sf.tme_scene_ctr[4]);
	d->center_LAT = sf.lat_scene_ctr;
	d->center_LON = sf.lon_scene_ctr;
	d->near_start_LAT = sf.lat_a;
	d->near_start_LON = sf.lon_a;
	d->near_end_LAT = sf.lat_d;
	d->near_end_LON = sf.lon_d;
	d->far_start_LAT = sf.lat_b;
	d->far_start_LON = sf.lon_b;
	d->far_end_LAT = sf.lat_e;
	d->far_end_LON = sf.lon_e;

	d->actual_azimuth = 0.0125 * sf.nl;
	if (ptype == CPX)
	   d->actual_azimuth = (pp->swth_speed/(1000.0*sp->pre->prf)) * sf.nl;
	else if (ptype == CSD)
  		d->actual_azimuth = 100.0;

	d->actual_pixels = sf.np_nflr;
	if( d->actual_pixels==8192 ) d->actual_pixels = 8191;
	d->actual_lines = sf.nl_nflr;
	d->total_pixels = sf.np;
	d->total_lines = sf.nl;
	Fill_str(d->media_id,sizeof(d->media_id),"%s",sf.media_labl);
	d->start_address = sf.dcrs_start;
	d->end_address = sf.dcrs_end;
	Fill_str(d->platform_name,sizeof(d->platform_name), "%s",sf.sp_id);
	/*strncpy(d->sensor_mode,sf.snsr_mode,strlen(sf.snsr_mode));*/
	Fill_str(d->sensor_mode,sizeof(d->sensor_mode),"%s",dss->sensor_id);
	d->PRF = sp->pre->prf;
	d->ant_look_angle = sf.lk_angle;
	/* d->data_rate may need TBD */
	d->data_win_pos = sf.wndw_pos;
	d->range_gate_del = sf.rnge_gate;
	d->track_angle = sf.trk_ang;
	Fill_str( d->ASC_DESC, sizeof(d->ASC_DESC), "%s", sf.asc_dsc );
	d->S_C_altitude = sf.altitude;
	d->X_position = sf.x_mid;
	d->Y_position = sf.y_mid;
	d->Z_position = sf.z_mid;
	d->X_velocity = sf.vx_mid * 1000.;
	d->Y_velocity = sf.vy_mid * 1000.;
	d->Z_velocity = sf.vz_mid * 1000.;
	d->roll = pp->att.roll;
	d->yaw = pp->att.yaw;
	d->pitch = pp->att.pitch;
	d->roll_rate = sf.roll_rate;
	d->yaw_rate = sf.yaw_rate;
	d->pitch_rate = sf.pitch_rate;
	d->nadir_radius = sf.re_nadir;
	d->image_radius = sf.re_image_ctr;
	d->incidence_angle = sf.inc_image_ctr;

	d->actual_range = 0.0125 * ap_rmlines;
	if (ptype == CPX ||
	        (ptype == RPR && Cur_Rqst->gnd_slnt_rg[0] == 'S'))
	   d->actual_range = (ap_rmlines * spf.srspace)
	             / (1000.0 * sin((pi / 180.0) * sf.inc_image_ctr));
	else if (ptype == CSD)
	   d->actual_range = 100.0;
	Fill_str(d->proc_version,sizeof(d->proc_version),"%s", sf.asp_ver);
	Fill_str(d->proc_type,sizeof(d->proc_type),"%s",sf.proc_type);
	switch (sv1.precision) {
	    case 1:
	    case 4:
		strcpy( d->type_ephemeris, "P " );
		break;
	    case 2:
		strcpy( d->type_ephemeris, "R " );
		break;
	    case 3:
		strcpy( d->type_ephemeris, "D " );
		break;
	    default:
		strcpy( d->type_ephemeris, "  " );
		break;
	}
	d->looks_azimuth = (ptype == CPX) ? 1.0 : 4.0;
	d->azi_weight_fac = pp->h_az;
	d->range_weight_fac = rtf.h;

	if ( Cur_Rqst->take_id[0] == 'E' ){
		d->induced_azimuth = 50.;
		d->induced_range = 100.;
	} else if ( Cur_Rqst->take_id[0] == 'J' ){
		d->induced_azimuth = 120.;
		d->induced_range = 50.;
	} else if ( Cur_Rqst->take_id[0] == 'R' ){
		d->induced_azimuth = 0.;	/* TBD */
		d->induced_range = 0.;		/* TBD */
	}

	d->gain = Cur_Rqst->proc_gain;
	d->swath_velocity = pp->swth_speed;
	d->squint_angle = sf.squint;
	d->avg_terrain_ht = Cur_Rqst->ave_hght;
	d->processor_gain = pp->pro_gain;  /* Modify 9/6/96 */
/*	d->processor_gain = (pp->pro_gain + Cur_Rqst->proc_gain);*/

	if (ptype == RPR)
	    Fill_str(d->deskew,sizeof(d->deskew),"%s",
			Cur_Rqst->deskew);
	else if (ptype == CPX)
	    Fill_str(d->deskew,sizeof(d->deskew),"NOT");

	if (ptype == STD || ptype == QLK ||
		(ptype == RPR && Cur_Rqst->gnd_slnt_rg[0] == 'G'))
		Fill_str(d->gnd_slant_flag,sizeof(d->gnd_slant_flag),
				"GROUND");

	d->sl_rng_1st_pix = sf.r_near;
	d->sl_rng_last_pix = sf.r_far;

	if (ptype == CSD || sw_clutter_lock == 0)
    		Fill_str(d->clutterlock_flg,sizeof(d->clutterlock_flg),"NOT");
	d->dop_frq_const = sf.fdga;
	d->dop_frq_slope = sf.fdgb;
	d->dop_frq_quad = sf.fdgc;

	if (ptype == CSD || sw_auto_focus == 0)
    		Fill_str(d->autofocus_flag,sizeof(d->autofocus_flag),"NOT");
	d->dop_frq_r_cnst = sf.fdotga;
	d->dop_frq_r_slope = sf.fdotgb;
	d->dop_frq_r_quad = sf.fdotgc;

	if (ptype == CPX) {
		d->azi_res = 10.0;
		d->rng_res = 10.0;
	} else if ( Cur_Rqst->take_id[0] == 'E' ){
		d->azi_res = 27.;
		d->rng_res = 29.;
	} else if ( Cur_Rqst->take_id[0] == 'J' ){
		d->azi_res = 29.;
		d->rng_res = 25.;
	} else if ( Cur_Rqst->take_id[0] == 'R' ){
		d->azi_res = azimuth_res;	/* CV 4/3/97 */
		d->rng_res = range_res;		/* CV 4/3/97 */
	}
	if (ptype == CPX || ptype == CSD) 
		d->azimuth_pixel = pp->swth_speed / sp->pre->prf;
	if (d->gnd_slant_flag[0] == 'S') 
		d->range_pixel = c / (2.0 * spf.csr);
	d->bits_sample = spf.nbits;
	d->bit_err_rate = pp->ber;
	d->SNR = sf.snr;
	d->est_noise_flr = spf.est_noise_floor;
	d->satur_points = sf.sat_pnt;

/* CLW notes: These values are from aux data scanned internal from ASP */
/* In order to transfer these values to ceos, ASP must scan & process */
/* These values are not saved into scan results file then
	transfer between scan job to frame job. They are LOST.  */

/* aux data are LOST between scan job & frame job */
/* Add r1_get_aux_data in preprocessing */

	if ( Cur_Rqst->take_id[0] == 'R' ){
	  d->repl_agc = sp->aux.rep_AGC;
	  d->temp_rx_lna = sp->aux.temp.rec_lna_temp;
	  d->temp_rx_sub = sp->aux.temp.rec_sub_temp;
	  d->temp_rx_prot = sp->aux.temp.rec_proc_temp;
	  d->temp_cal_sys = sp->aux.temp.cal_sub_temp;
	  d->rx_agc = sp->aux.AGC_set;
	  d->pre_cal1_pow = 0.0;
	  d->pre_cal2_pow = 0.0;
	  d->post_cal1_pow = 0.0;
	  d->post_cal2_pow = 0.0;
	  d->repl_pow = 0.;  /* TBD */
	  d->ssar_roll_ang = 0.;  /* TBD */
	}
	
	Fill_str(d->comment,sizeof(d->comment),"%s",sf.comment);

	if ( Res_type == LOW_RES ){
		d->actual_pixels = d->actual_pixels / 8.;
		d->actual_lines = d->actual_lines / 8.;
		d->total_pixels = d->total_lines = 1024;
		d->looks_azimuth = dss->n_azilok;
		d->looks_range = dss->n_rnglok;
		d->dop_frq_slope = dss->crt_dopcen[1];
		d->dop_frq_quad = dss->crt_dopcen[2];
		d->dop_frq_r_slope = dss->crt_rate[1];
		d->dop_frq_r_quad = dss->crt_rate[2];
		d->azi_res = dss->azi_res;
		d->rng_res = dss->rng_res;
		d->azimuth_pixel = dss->line_spacing;
		d->range_pixel = dss->pixel_spacing;
		d->satur_points = hist->next->data_set->data_values_hist[255];
	}

	return(PASS);
}
/*==========================================================================
	Below are inherited modules from asp R1a version 
		leader_info, hist_stat, trailer_info
	to fill values into sp, pp, scene_file, ceos aux. data structures
*/
/* leader_info(sp,pp,cp) ----------------------------------

********************************************************************
* leader_info computes some auxilary info for the CEOS format      *
********************************************************************

*/
leader_info(sp,pp,cp)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
    CEOS_INFO_PTR	cp;
{

int	i,k,grand_total,expected;
float	peak;
float	value[256];
float	meansamp,stdvsamp,meanfreq,stdvfreq,minsmp,maxsmp,minfreq,maxfreq;


/* determine the type of product, and set up the histogram   */
/* and the corresponding values for the histogram.           */

   if (strcmp(Cur_Rqst->type,"CPX") == 0) {
	/* rearrange the histogram table to account for two-s */
	/* compliment to go from -128 to +127.                */
	cp->nbins = 256;
	k = 0;
	for (i=128; i<256; i++) cp->hstgrm[i] = sf.i_hstgrm[k++];
	for (i=0;   i<128; i++) cp->hstgrm[i] = sf.i_hstgrm[k++];
	for (i=0; i<cp->nbins; i++) value[i]  = (float)(i-128)*256.;
   }
   else if ((strcmp(Cur_Rqst->type,"STD") == 0) ||
       (strcmp(Cur_Rqst->type,"QLK") == 0) ||
       (strcmp(Cur_Rqst->type,"RPR") == 0))  {
	cp->nbins = 256;
	grand_total = 0;
	for (i=0; i<cp->nbins; i++) {
	   cp->hstgrm[i] = sf.i_hstgrm[i];
	   value[i]  = (float)i;
	   grand_total += cp->hstgrm[i];
	}
	expected = grand_total - sf.nl * ap_rmlines;
	if (expected < 0) 
	  printf("...histogram total count is lower than expected: %d\n",grand_total);
	if (expected > 0) 
	   cp->hstgrm[0] -= expected;
   	if (cp->hstgrm[0] < 0) cp->hstgrm[0] = 0;
   } 
   if (strcmp(Cur_Rqst->type,"CSD")) {
	sf.sat_pnt = cp->hstgrm[255];
	hist_stat(cp->nbins,cp->hstgrm,value,&minsmp,&maxsmp,&minfreq,&maxfreq,
		&meansamp,&stdvsamp,&meanfreq,&stdvfreq);
	     cp->minfreq = minfreq;
	     cp->maxfreq = maxfreq;
	     cp->meansamp = meansamp;
	     cp->stdvsamp = stdvsamp;
	     cp->meanfreq = meanfreq;
	     cp->stdvfreq = stdvfreq;
   }

/* compute the three coefficients for the radiometric data record */

/* substitute the dpf.noise_fctr, dpf.linear_fctr, dpf.offset_fctr
(from default directory) by the value reading from CPF file (noise_fct,
linear_conv_fct, & offset_conv_fct) 10/16/96 */

  if (Cur_Rqst->take_id[0]=='R') {	/* from Tom 3/13/97 */
  	sf.noise_scl_fac = noise_fct * 
		 pow(10.,((Cur_Rqst->proc_gain)/10.0));
  	sf.lnr_conv_fac  = linear_conv_fct * 
		 pow(10.,(- (Cur_Rqst->proc_gain)/10.0));
  } else { 
  	sf.noise_scl_fac = noise_fct * pp->noise *
		 pow(10.,((Cur_Rqst->proc_gain)/10.0));
  	sf.lnr_conv_fac  = linear_conv_fct * 
		 pow(10.,(- (Cur_Rqst->proc_gain)/10.0));
  }
  sf.off_conv_fac  = offset_conv_fct;

/*  sf.noise_scl_fac = dpf.noise_fctr * pp->noise *
		 pow(10.,((Cur_Rqst->proc_gain)/10.0));*/

/* -- The linear term no longer is to use the peak power as per
      IOM#3340-92-003  but is left here in comments for record   
  peak = pp->peak;
  if (peak ==  0.0) peak = dpf.peak_ref;
  sf.lnr_conv_fac  = (dpf.peak_ref/peak) * dpf.linear_fctr * 
		 pow(10.,(- (pp->pro_gain + Cur_Rqst->proc_gain)/10.0));
-- */


  return (PASS);

}


/* hist_stat(nbins,hstgrm,value,minsmp,maxsmp,minfreq,maxfreq,meansamp, -------
	     stdvsamp,meanfreq,stdvfreq)

 compute some statics for the histogram.

 inputs:	nbins 	-- number of bins in the histogram
		hstgrm 	-- the histogram 
		value	-- the values of each bin 
 output:	minsmp  -- the minimum sample value
		maxsmp  -- the maximum sample value
 		minfreq	-- the minimum histogram table value
		maxfreq	-- the maximum histogram table value
		meansamp - the mean sample value
		stdvsamp - the std dev. of sample value
		meanfreq - the mean histogram table value
		stdvfreq - the std dev of histogram table value

*/


hist_stat(nbins,hstgrm,value,minsmp,maxsmp,minfreq,maxfreq,meansamp,
	     stdvsamp,meanfreq,stdvfreq)

long nbins,hstgrm[];
float	value[],*meansamp,*stdvsamp,*minsmp,*maxsmp,
	*meanfreq,*stdvfreq,*minfreq,*maxfreq;
{

int	i,temp,mintemp,maxtemp;
double ftemp,total,total2,sum,sum2,val;

if (nbins <= 0) return;

mintemp = 9999999;
maxtemp = 0;

for (total=0, total2=0., sum=0., sum2=0., i=0; i < nbins; i++) {
	temp   = hstgrm[i];
	ftemp  = (double)temp;
	val    = (double)value[i];
	total  += ftemp;
	total2 += ftemp*ftemp;
	ftemp *= val;
	sum    += ftemp;
	sum2   += ftemp*val;
	if ( mintemp > temp) mintemp = temp;
	if ( maxtemp < temp) maxtemp = temp;
 }

*minsmp = 1000.0;
for( i=0; i<nbins && *minsmp==1000.0 ; i++ )
	if( hstgrm[i]>0 ) *minsmp = (float) i;
*maxsmp = -1.0;
for( i=nbins-1; i>=0 && *maxsmp==-1.0 ; i-- )
	if( hstgrm[i]>0 ) *maxsmp = (float) i;

if (total != 0.) {
  *meansamp = sum/total;
  *stdvsamp = sqrt((sum2/total) - ((*meansamp) * (*meansamp)));
} else {
  *meansamp = 0.0;
  *stdvsamp = 0.0;
}
*meanfreq = total/nbins;
*stdvfreq = sqrt ((total2/nbins) - (*meanfreq) * (*meanfreq));
*minfreq  = (float)mintemp;
*maxfreq  = (float)maxtemp;
if (vbose) {
	printf("mean and stdv (sample) = %g,%g\n",*meansamp,*stdvsamp);
	printf("mean and stdv (freque) = %g,%g\n",*meanfreq,*stdvfreq);
 }
return;

}


/* trailer_info(sp,pp) -----------------------------------------

**********************************************************************
* computes some last minute stuff for trailer facility data record   *
**********************************************************************

*/

trailer_info(sp,pp)
    TAPE_SEG_PTR 	sp;
    PP_BLOCK_PTR 	pp;
{

double  ff,xx,yy,zz,dd1,dd2,len,trk_angle;
double  r,gclat_sc,gdlat_sc,dlat;
double  lk_angle,spacing,delta;
double  pi = 3.141592653589793;
double  c = 2.998E8;
char    str_buf[32];
float   seconds,mlsc;
int     year,day,hour,minute,month,day_month;
double  ghangle;
GMT     agmt;
char    cur_dir[80];

/*extract ap_rmlines */
sf.np_nflr = ap_rmlines;

/*extract sf.nl*/
sf.nl_nflr = sf.nl;

/*Range gate delay. */
/*calculate from (data_window_pos + (8/PRF)x10**6)*/
sf.rnge_gate = sf.wndw_pos + ((8.0/sp->pre->prf)*1000000.0); 

/*calculate track angle*/
ff = 1.0 / emf.f;
xx = sf.x_begin * sf.x_begin;
yy = sf.y_begin * sf.y_begin;
zz = sf.z_begin * sf.z_begin;
dd1 = (xx + yy) / (xx + yy + zz);
dd1 = acos(sqrt(dd1));
xx = sf.x_end * sf.x_end;
yy = sf.y_end * sf.y_end;
zz = sf.z_end * sf.z_end;
dd2 = (xx + yy) / (xx + yy + zz);
dd2 = acos(sqrt(dd2));
len = (strcmp(Cur_Rqst->type,"CPX") == 0) ? 51.2 : 102.4;
if ((Cur_Rqst->take_id[0]=='J') && (sf.z_begin>0.0) && (sf.z_end<0.0)){
     trk_angle = (180.0 / pi)
            * asin(sf.re_nadir * fabs(dd2 + dd1) / len);
} else {
     trk_angle = (180.0 / pi)
            * asin(sf.re_nadir * fabs(dd2 - dd1) / len);
}
/* Ming's cheap fix for track angle, 2-17-94 */

if (vbose) printf("dd1=%g, dd2=%g\n", dd1,dd2);

if ((Cur_Rqst->take_id[0]=='J') && (sf.z_begin>0.0) && (sf.z_end<0.0)){
       printf("JERS near the equator\n");
        trk_angle = 270.0 - trk_angle;
        sf.asc_dsc[0] = 'D';
}
else if (Cur_Rqst->take_id[0] == 'J' && sf.z_end < 0.0) {
    if (vbose) printf("JERS in SOUTH atmosphere!\n");
    if (dd2 > dd1 || (dd2 == dd1 && sf.z_mid > 0.0)) {
        trk_angle = 270.0 - trk_angle;
        sf.asc_dsc[0] = 'D';
    }
    else {
        trk_angle = 270.0 + trk_angle;
        sf.asc_dsc[0] = 'A';
    }
}
else {
    if (dd2 > dd1 || (dd2 == dd1 && sf.z_mid > 0.0)) {
        trk_angle = 270.0 + trk_angle;
        sf.asc_dsc[0] = 'A';
    }
    else {
        trk_angle = 270.0 - trk_angle;
        sf.asc_dsc[0] = 'D';
    }
}
sf.trk_ang = trk_angle;

/* calculate spacecraft altitude at image center*/
xx = sf.x_mid * sf.x_mid;
yy = sf.y_mid * sf.y_mid;
zz = sf.z_mid * sf.z_mid;
r = sqrt(xx + yy + zz);
gclat_sc = acos(sqrt(xx + yy) / r);
gdlat_sc = atan(tan(gclat_sc) / ((1.0 - ff) * (1.0 - ff)));
dlat = gdlat_sc - gclat_sc;
sf.altitude = (r - sf.re_nadir) * cos(dlat);

sf.sp_lat= (gdlat_sc -(sin(gdlat_sc-gclat_sc)*sf.altitude)/sf.re_nadir)*180.0/pi;

/* extract time from pp_regions and convert to desired form & calculate
hour angle (ghangle) in the center  CV 4/4/97 */
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

time_brk(&agmt,&year,&day,&hour,&minute,&seconds);
strcpy( cur_dir, getcwd(cur_dir,80));
chdir(PROC_PATH);
get_gha_(&year,&day,&hour,&minute,&seconds,&ghangle);
chdir( cur_dir );

/* sf.sp_lat = (180.0 / pi) * gdlat_sc; */

sf.sp_lon = (atan2( sf.y_mid , sf.x_mid)) *180/pi - ghangle;
if (sf.sp_lon < -180.0) 
   sf.sp_lon += 360;
if (sf.sp_lon > 180.0) 
   sf.sp_lon -= 360;
/* calculate spherical coordinates EYC 8/20/96 
sf.sp_lat = asin( sf.z_mid / r );
sf.sp_lon = atan2( sf.y_mid / sf.x_mid );*/

/* calculate antenna look angle */
delta = (sf.r_mid * sf.r_mid - sf.altitude * sf.altitude)
	/ (2.0 * r);
lk_angle = (180.0 / pi) * (acos((sf.altitude + delta) / sf.r_mid));
sf.lk_angle = lk_angle;
/* need to save lk_angle in sf.??? */

/*calculate incidence angle*/
sf.inc_image_ctr = lk_angle + (180.0 / pi) * acos(1.0 - (delta / emf.r_e));

/*Squint angle sf.squint =pitch cos 20 - yaw sin 20*/
lk_angle = (pi / 180.0) * lk_angle;
sf.squint = pp->att.pitch * cos(lk_angle) - pp->att.yaw * sin(lk_angle);
if(Cur_Rqst->take_id[0] == 'J') 
    sf.squint = asin((pp->fda/pp->fdota)*(pp->swth_speed/pp->r_close));

return (PASS);

}
