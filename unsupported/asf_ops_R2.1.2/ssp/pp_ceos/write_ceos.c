/* SccsId[]= @(#)write_ceos.c	2.41 3/24/98 */
static char sccsid_write_ceos[]= "@(#)PPwrite_ceos.c:2.41";



#include <stdio.h>
#include <stdarg.h>
#include <math.h>
#include <strings.h>
#include <ctype.h>
#include <sys/file.h>
#include <time.h>
#include <defs.h> 
#include <extern.h>
#include <sys/stat.h>

#include "odl_file.h"
#include "input_file.h"
#include "aux_file.h"
#include "cali_file.h"
#include "externs.h"
#include "error.h"

struct ceos_struct CEOS;
int Debug = FALSE;
SARL_ptr *Global_tree;


char PROC_PATH[80];

#define PASS 0
#define FAIL -1


enum {LOW_RES, FULL_RES, BOTH_IMAGE};
int Res_type;

SARL_ptr* Rd_LDR_template();

char* version(char* file);

double SNR;
double i_mean_hist, q_mean_hist;

double  ctok (double rx, double ry, double rz, double vx, double vy, double vz, 
              double kepler[]);

/*************************************************************************/
/*************************************************************************/


int write_ceos()
{
	SARL_ptr* tree;

	CEOS.out = TRUE;
	CEOS.debug = 0; 
	CEOS.inputType = FROM_LDR;
/*
	sprintf(CEOS.infile, "%sceos_templates/ceos_ldr_init", PROC_PATH);
*/
	if (!strcmp(odl_file->prod_type, "STANDARD") ||
		!strcmp(odl_file->prod_type, "RAMP"))
		strcpy(CEOS.infile, "/home/tmpdisk/ANT/ceos_template.pp");
	else
		strcpy(CEOS.infile, "/home/tmpdisk/ANT/ceos_template.complex.pp");
	sprintf(CEOS.outfile, "/home/tmpdisk/ceos_ldr.%d", odl_file->job_id);
/*
	strcpy(CEOS.outfile, outfile);
*/
	

	if (CEOS.inputType == FROM_LDR) {
		if ((tree = Rd_LDR_template()) == NULL) {
			return (FAIL);
		}
		Fill_LDR_values(tree);
		Write_CEOS_SARL(CEOS.outfile, tree);
	}
	Free_SARL(tree);
	free(tree);
	return (PASS);
}

/*************************************************************************/
SARL_ptr* Rd_LDR_template()
{
	FILE *in;
	SARL_ptr* tree;
	int ret;

	/* open the leader file */
	if ((in=Open_LDR(CEOS.infile, READ_ONLY)) == NULL) {
		printf("\n Could not open the leader file template -- %s", CEOS.infile);
		fflush(stdout);
		exit(ierr_2);
		/* return NULL; */
	}

	/* allocate memory for a SARL structure */
	if ((tree = Allocate_SARL()) == NULL) {
		printf("\n Failed to allocate tree");
		fflush(stdout);
		exit(ierr_5);
		/* return NULL; */
	}

	SetCurrentSARL(tree);

	/* read the File Descriptor Record, transfer to "descript" */
	if (!Read_FDR_SARL(in, tree)) {
		printf("\n Error in Read_FDR_SARL\n");
		free(tree);
		return NULL;
	}


	/* based on the contents of the leader file FDR, allocate space
           for each record */

	if (Allocate_SARL_ODR(tree)) {
		ret=Read_ALL_SARL(in, tree);
		if (ret == END_OF_FILE) {
			fclose(in);
			return (tree);
		}
		else {
			printf("\n Aborting the read of the input file\n");
			Free_SARL(tree);
			free(tree);
			fclose(in);
			return NULL;
		}
	}
	else {
		Free_SARL(tree);
		free(tree);
		fclose(in);
		return NULL;
	}
}
/************************************************************************/
int Fill_LDR_values(SARL_ptr* d)
{
	Fill_FDR_values(&(d->descript));
	/* Fill_DSS_values(d->data_sum); */
	Fill_PP_values(d->platform);
	Fill_AT_values(d->attitude);
	Fill_RD_values(d->radio_data);
	/* Fill_DQS_values(d->data_qual); */

	Fill_DH_values(d->histogram);
	Fill_DSS_values(d->data_sum);

	Fill_RS_values(d->spectra);
	Fill_FR_values(d->facility, d->data_sum, d->histogram, d->radio_data);
	Fill_DQS_values(d->data_qual);

	return (PASS);
}
/************************************************************************/
int Fill_FDR_values(Sarl_Desc_Rec* d)
{
	Fill_str(d->software_id, sizeof(d->software_id),
		"PP2.41");
	
	/* PnRRRRRFFFMDSvvv */
	Fill_str(d->product_id, sizeof(d->product_id), 
		"%s", odl_file->product_id);
	d->n_map_proj = 0L;
	d->l_map_proj = 0L;
	d->n_radi_data = 1L;
	d->l_radi_data = 4232L;
	d->n_qual_sum = 1L;
	d->l_qual_sum = 1620L;
	d->n_data_hist = 2L;
	d->n_dem_desc = 0L;

	return (PASS);
}
/************************************************************************/
int Fill_DSS_values(Dataset_Sum* d)
{

	char file[256];
	char *ptr;
	int i;
	

	/* PPRRRRRFFFMDSvvv */
	Fill_str(d->product_id, sizeof(d->product_id), 
                "%s", odl_file->product_id);

	Fill_str(d->inp_sctim, sizeof(d->inp_sctim),
		"%s", data_set.inp_sctim);

	if (facility_record.z_velocity > 0)
		Fill_str( d->asc_des, sizeof(d->asc_des), "ASCENDING" );
	else
		Fill_str( d->asc_des, sizeof(d->asc_des), "DESCENDING" );

	d->pro_lat = data_set.pro_lat;
	d->pro_long = data_set.pro_long;
	d->pro_head = data_set.pro_head;


	d->terrain_h = data_set.terrain_h;

	if (!strcmp(odl_file->prod_type, "STANDARD") ||
			!strcmp(odl_file->prod_type, "RAMP"))
  {
		d->sc_lin = num_rec / 2;
		d->sc_pix = rec_length / 2;
	}
	else {    /* COMPLEX */
		d->sc_lin = (num_rec - 1) / 2;
		d->sc_pix = (rec_length - 192) / 2;
	}

	d->scene_len = data_set.scene_len;
	d->scene_wid = data_set.scene_wid;

	if (!strcmp(odl_file->platform, "R1"))
		Fill_str(d->mission_id, sizeof(d->mission_id), "RSAT-1");
	else if (!strcmp(odl_file->platform, "E1"))
		Fill_str(d->mission_id, sizeof(d->mission_id), "ERS-1");
	else if (!strcmp(odl_file->platform, "E2"))
		Fill_str(d->mission_id, sizeof(d->mission_id), "ERS-2");
	else
		Fill_str(d->mission_id, sizeof(d->mission_id), "JERS-1");

	Fill_str(d->sensor_id, sizeof(d->sensor_id), "RSAT-1-C -    -HH");
	Fill_str(d->revolution, sizeof(d->revolution), "%d", odl_file->revolution);

	d->plat_lat = data_set.plat_lat;
	d->plat_long = data_set.plat_long;
	d->plat_head_scene = data_set.plat_head_scene;

	if (!strcmp(odl_file->platform, "R1") && 
		!strcmp(odl_file->frame_mode, "ANTARCTIC"))
		d->clock_ang = -90.0;
	else
		d->clock_ang = 90.0;

	d->incident_ang = data_set.incident_ang;
	d->wave_length = data_set.wave_length;
	if (data_set.wave_length > 0.0) d->frequency = 0.3 / data_set.wave_length;
	d->ampl_coef[0] = data_set.ampl_coef0;
	d->ampl_coef[1] = data_set.ampl_coef1;
	d->ampl_coef[2] = data_set.ampl_coef2;
	d->ampl_coef[3] = data_set.ampl_coef3;
	d->ampl_coef[4] = data_set.ampl_coef4;

  d->phas_coef[0] = data_set.phas_coef0;
  d->phas_coef[1] = data_set.phas_coef1;
  d->phas_coef[2] = data_set.phas_coef2;
  d->phas_coef[3] = data_set.phas_coef3;
  d->phas_coef[4] = data_set.phas_coef4;

	d->chirp_ext_ind = data_set.chirp_ext_ind;

	d->rng_samp_rate = data_set.rng_samp_rate;
	d->rng_gate = data_set.rng_gate;
	d->rng_length = data_set.rng_length;

	d->chn_bits = 4;

        /* QDN 2/20/98 hardcode the i_bias and q_bias to -7.5 
	d->i_bias = i_mean_hist;
	d->q_bias = q_mean_hist;
        */
	d->i_bias = -7.5;
	d->q_bias = -7.5;


	d->iq_ratio = 0.0;  

	d->ele_sight = data_set.ele_sight;
	d->mech_sight = 29.4;
	Fill_str(d->echo_track, sizeof(d->echo_track), "OFF");
	d->prf = data_set.prf;
	d->elev_beam = 0.0;
	d->azi_beam = data_set.azi_beam;

 	Fill_str(d->sat_bintim, sizeof(d->sat_bintim), "");

	Fill_str(d->sat_clktim, sizeof(d->sat_clktim), "");
	d->sat_clkinc = 0;
	Fill_str(d->sys_id, sizeof(d->sys_id), "PREC");
	Fill_str(d->ver_id, sizeof(d->ver_id), "VERS2.41");

	Fill_str(d->fac_code, sizeof(d->fac_code), "");
	Fill_str(d->lev_code, sizeof(d->lev_code), "");

	if (!strcmp(odl_file->prod_type, "STANDARD")) {
		if (odl_file->pixel_spacing == 100)
			Fill_str(d->product_type, sizeof(d->product_type), "LOW");
		else
			Fill_str(d->product_type, sizeof(d->product_type), "FULL");
	}
	else if (!strcmp(odl_file->prod_type, "RAMP"))
		Fill_str(d->product_type, sizeof(d->product_type), "RAMP");
	else
		Fill_str(d->product_type, sizeof(d->product_type), "COMPLEX");

	Fill_str(d->algor_id, sizeof(d->algor_id), "%s", data_set.algor_id);

	d->n_azilok = data_set.n_azilok;
	d->n_rnglok = data_set.n_rnglok;
	d->bnd_azilok = data_set.bnd_azilok;
	d->bnd_rnglok = data_set.bnd_rnglok;
	d->bnd_azi = data_set.bnd_azi;
	d->bnd_rng = data_set.bnd_rng;
	Fill_str(d->azi_weight, sizeof(d->azi_weight), "KAISER");
	Fill_str(d->rng_weight, sizeof(d->rng_weight), "KAISER");
	Fill_str(d->data_inpsrc, sizeof(d->data_inpsrc), "%s", odl_file->media_id);
	d->rng_res = data_set.rng_res;            
	d->azi_res = data_set.azi_res;
	d->radi_stretch[0] = 0.0;
	d->radi_stretch[1] = 0.0;
	d->alt_dopcen[0] = data_set.alt_dopcen0;
	d->alt_dopcen[1] = data_set.alt_dopcen1;
	d->alt_dopcen[2] = data_set.alt_dopcen2;
	d->crt_dopcen[0] = data_set.crt_dopcen0;
	d->crt_dopcen[1] = data_set.crt_dopcen1;
	d->crt_dopcen[2] = data_set.crt_dopcen2;
	Fill_str(d->time_dir_pix, sizeof(d->time_dir_pix), "INCREASE");
	if (!strcmp(odl_file->frame_mode, "ANTARCTIC"))
		Fill_str(d->time_dir_lin, sizeof(d->time_dir_lin), "INCREASE");
	else
		Fill_str(d->time_dir_lin, sizeof(d->time_dir_lin), "DECREASE");
	d->alt_rate[0] = data_set.alt_rate0;
	d->alt_rate[1] = data_set.alt_rate1;
	d->alt_rate[2] = data_set.alt_rate2;
	d->crt_rate[0] = data_set.crt_rate0;
	d->crt_rate[1] = data_set.crt_rate1;
	d->crt_rate[2] = data_set.crt_rate2;
	Fill_str(d->line_cont, sizeof(d->line_cont), "RANGE");
	Fill_str(d->clutterlock_flg, sizeof(d->clutterlock_flg), "%s",
				data_set.clutterlock_flg);

	Fill_str(d->auto_focus, sizeof(d->auto_focus), "%s", data_set.auto_focus);
	d->line_spacing = data_set.line_spacing;
	d->pixel_spacing = data_set.pixel_spacing;
	Fill_str(d->rngcmp_desg, sizeof(d->rngcmp_desg), "SYNTHETIC CHIRP");
	d->no_beams = 1;
	Fill_str(d->beam1, sizeof(d->beam1), "%s", odl_file->instr_mode);
	Fill_str(d->beam2, sizeof(d->beam2), "");
	Fill_str(d->beam3, sizeof(d->beam3), "");
	Fill_str(d->beam4, sizeof(d->beam4), "");
	d->prf1 = data_set.prf1;
	d->prf2 = 0.0;
	d->prf3 = 0.0;
	d->prf4 = 0.0;
	d->rng_gate1 = data_set.rng_gate1;
	d->rng_gate2 = 0.0;
	d->rng_gate3 = 0.0;
	d->rng_gate4 = 0.0;
	d->tot_pls_burst = 0;
	d->val_pls_burst = 0;

	d->az_ovlp_nxt_img = 0;
	d->rg_off_nxt_img = 0;

	ptr = strrchr(odl_file->cali_file, '/');
	Fill_str(d->cal_params_file, sizeof(d->cal_params_file), "%s", ++ptr);
	ptr = strrchr(odl_file->frame_file, '/');
	Fill_str(d->scan_results_file, sizeof(d->scan_results_file), "%s", ++ptr);

	sprintf(file, "/home/tmpdisk/scan.dat.%d", odl_file->job_id);
	Fill_str(d->scanner_version, sizeof(d->scanner_version), "%s", version(file));

	sprintf(file, "/home/tmpdisk/aux.dat.%d", odl_file->job_id);
	Fill_str(d->decode_version, sizeof(d->decode_version), "%s", version(file));

/* blank fill the remaining spares at the end */

	
	for (i=0; i<sizeof(d->spare_dss_16); i++) d->spare_dss_16[i] = ' ';
	
	return (PASS);
}
/************************************************************************/
int Fill_PP_values(Pos_Data* d)
{
	int i,j,k;
        double sec;
	char str[100];
	int yearday, month, day;
        double kepler[6], t_gha, t_start;

/*  EYC 2/18/1998       Use the correct time for PP record
	strcpy(str, aux_file->sc_time);
	d->year = atoi(strtok(str, "-")); 
	yearday = atoi(strtok(NULL, "T"));
*/
        sscanf( facility_record.start_gmt, "%4d-%3dT%2d:%2d:%lf",
                &k, &yearday, &i, &j, &sec );
        d->year = k;
        d->gmt_sec = i*3600.0 + j*60.0 + sec;
	month_day(d->year, yearday, &month, &day);
	d->month = month;
	d->day = day;
	d->gmt_day = yearday;
/*
	strtok(NULL, ":");
	strtok(NULL, ":");
	d->gmt_sec = atoi(strtok(NULL, "."));
*/

/*  EYC 2/18/1998     Use sscanf to get the decimals of seconds
        d->gmt_sec = 3600*atoi(strtok(NULL, ":")) + 60*atoi(strtok(NULL, ":")) 
                     + atoi(strtok(NULL, "."));
*/

	d->data_int = platform_position.data_int;
	Fill_str(d->ref_coord, sizeof(d->ref_coord), "INERTIAL");
/*  EYC 2/19/1998       Correct GHA to start time
	d->hr_angle = odl_file->gha_corr_angle;
*/
        gmt2sec( odl_file->gha_corr_time, &t_gha );
        gmt2sec( facility_record.start_gmt, &t_start );
        d->hr_angle = fmod( odl_file->gha_corr_angle + 360.0 +
                ( t_start - t_gha )*0.00417807462193866, 360.0 );

	d->alt_poserr = -99.0;
	d->crt_poserr = -99.0;
	d->rad_poserr = -99.0;
	d->alt_velerr = -99.0;
	d->crt_velerr = -99.0;
	d->rad_velerr = -99.0;

	ctok(platform_position.x_position_begin,
	     platform_position.y_position_begin,
	     platform_position.z_position_begin,
	     platform_position.x_velocity_begin/1000.0,
	     platform_position.y_velocity_begin/1000.0,
	     platform_position.z_velocity_begin/1000.0,
	     kepler);

	for (i=0; i<6; i++) d->orbit_ele[i] = kepler[i];

	d->pos_vect[0].pos[0] = platform_position.x_position_begin;
	d->pos_vect[0].pos[1] = platform_position.y_position_begin;
	d->pos_vect[0].pos[2] = platform_position.z_position_begin;
	d->pos_vect[0].vel[0] = platform_position.x_velocity_begin;
	d->pos_vect[0].vel[1] = platform_position.y_velocity_begin;
	d->pos_vect[0].vel[2] = platform_position.z_velocity_begin;
	d->pos_vect[1].pos[0] = platform_position.x_position_middle;
	d->pos_vect[1].pos[1] = platform_position.y_position_middle;
	d->pos_vect[1].pos[2] = platform_position.z_position_middle;
	d->pos_vect[1].vel[0] = platform_position.x_velocity_middle;
	d->pos_vect[1].vel[1] = platform_position.y_velocity_middle;
	d->pos_vect[1].vel[2] = platform_position.z_velocity_middle;
	d->pos_vect[2].pos[0] = platform_position.x_position_end;
	d->pos_vect[2].pos[1] = platform_position.y_position_end;
	d->pos_vect[2].pos[2] = platform_position.z_position_end;
	d->pos_vect[2].vel[0] = platform_position.x_velocity_end;
	d->pos_vect[2].vel[1] = platform_position.y_velocity_end;
	d->pos_vect[2].vel[2] = platform_position.z_velocity_end;


/* blank fill the remaining spares at the end */
	for (i=0; i<sizeof(d->spare_ppr_1); i++) d->spare_ppr_1[i] = ' ';

	return (PASS);
}
/************************************************************************/
int Fill_AT_values(Att_Data* d)
{ 
	int i;

	d->npoint = 3;
	d->att_vect->gmt_day = -99;
	d->att_vect->gmt_msec = -99;
	d->att_vect->pitch_flag = -99;
	d->att_vect->roll_flag = -99;
	d->att_vect->yaw_flag = -99;
	d->att_vect->pitch = -99.0;
	d->att_vect->roll = -99.0;
	d->att_vect->yaw = -99.0;
	d->att_vect->pitch_rate_flag = 0;
	d->att_vect->roll_rate_flag = 0;
	d->att_vect->yaw_rate_flag = 0;
	d->att_vect->pitch_rate = -99.0;
	d->att_vect->roll_rate = -99.0;
	d->att_vect->yaw_rate = -99.0;
	d->next = NULL;

	
/* blank fill the remaining spares at the end */
	for (i=0; i<sizeof(d->spare_adr_1); i++) d->spare_adr_1[i] = ' ';

	return (PASS);
}
/************************************************************************/
int Fill_RD_values(Radi_Data* d)
{
	int i, j, fd, num_samp;
	char file[256];
	double *buf;
	struct stat stbuf;

	d->noise_fact = cali_file->noise_fact;
	d->linear_conv_fact = cali_file->linear_conv_fact;
	d->offset_conv_fact = 0.0;

	sprintf(file, "/home/tmpdisk/scale_factor.dat.%d", odl_file->job_id);
	if ((fd=open(file, O_RDONLY)) == -1)
		info_handler(ierr_2, file, "can't open file %s", file);

	stat(file, &stbuf);
	for (i=0; i<256; i++) d->lookup_tab[i] = 0.0;

	buf = (double *) malloc(stbuf.st_size);
	read(fd, buf, stbuf.st_size);
	num_samp = stbuf.st_size/(256*sizeof(double));
	if (num_samp > 4) num_samp = 32;
	for (i=0, j=0; i<stbuf.st_size/sizeof(double) && j<256; i += num_samp, j++) 
		d->lookup_tab[j] = ((double *) buf)[i] / 10.0; 

	close(fd);
	free(buf);

	return (PASS);
}
/************************************************************************/
int Fill_DQS_values(Qual_Sum* d)
{
	int i, j;

	Fill_str(d->cali_date, sizeof(d->cali_date), "");
	d->islr = cali_file->islr;
	d->pslr = cali_file->pslr;
	d->azi_ambig = cali_file->azi_ambig;
	d->rng_ambig = cali_file->rng_ambig;
	d->snr = SNR;
	d->ber = aux_file->bit_error_rate;
	d->rng_res = cali_file->rng_res;
	d->azi_res = cali_file->azi_res;
	d->rad_res = -99.0;
	d->dyn_rng = -99.0;
	d->abs_rad_unc_db = -99.0;
	d->abs_rad_unc_deg = 0.0;

	for (i=0; i<2; i++)
		for (j=0; j<16; j++)
			d->rel_rad_unc[i][j] = -99.0;
/*
	d->rel_rad_unc_db = -99.0;
	d->rel_rad_unc_deg = 0.0;
*/
	d->alt_locerr = cali_file->alt_locerr;
	d->crt_locerr = cali_file->crt_locerr;
	d->alt_scale = cali_file->alt_scale;
	d->crt_scale = cali_file->crt_scale;
	d->dis_skew = cali_file->dis_skew;
	d->ori_err = cali_file->ori_err;
/*
	d->alt_m = -99.0;
	d->crt_m = -99.0;
*/
	d->nesz = -99.0;
	d->enl = -99.0;
	Fill_str(d->tb_update, sizeof(d->tb_update), "");
	Fill_str(d->cal_status, sizeof(d->cal_status), "%s", 
					cali_file->cal_status);
	Fill_str(d->cal_comment, sizeof(d->cal_comment), "%s",
					cali_file->cal_comment);

	return (PASS);
}
/************************************************************************/
int Fill_DH_values(Data_Hist* d_orig)
{
	int i;
	Data_Hist* d;
	Hist_Data_Set *hd_ptr, *tmp_ptr;
	long ihstgrm[256];
	long qhstgrm[256];
	long hstgrm[256];
	long hstgrm_img[256];
	float value[256];


/* Fill in signal histogram record */

	d = d_orig;

	/* I Component */
	hd_ptr = d->data_set;

	hd_ptr->nbin = 64; 

	iqhistogram(hd_ptr->nbin, ihstgrm, qhstgrm);

	hd_ptr->ns_lin = aux_file->echo_samples;
	hd_ptr->ns_pix = aux_file->echo_pulses;
	hd_ptr->ngrp_lin = hd_ptr->ns_lin;
	hd_ptr->ngrp_pix = hd_ptr->ns_pix;
	hd_ptr->nsamp_lin = hd_ptr->ns_lin;
	hd_ptr->nsamp_pix = hd_ptr->ns_pix;
	hd_ptr->min_smp = -16;
	hd_ptr->max_smp = 15;
	for (i=0; i<hd_ptr->nbin; i++) {
		hd_ptr->data_values_hist[i] = ihstgrm[i];
		value[i] = (float) (i-32);
	}

	hist_stat(hd_ptr->nbin,hd_ptr->data_values_hist,value,
               &hd_ptr->min_smp, &hd_ptr->max_smp,
               &hd_ptr->min_hist, &hd_ptr->max_hist,
               &hd_ptr->mean_smp, &hd_ptr->std_smp,
               &hd_ptr->mean_hist, &hd_ptr->std_hist);

	d->ltab = (long) (284-36+hd_ptr->nhist*8);

	i_mean_hist = hd_ptr->mean_hist;

	/* Q Component */
	hd_ptr = hd_ptr->next;

	hd_ptr->nbin = 64; 

	hd_ptr->ns_lin = aux_file->echo_samples;
	hd_ptr->ns_pix = aux_file->echo_pulses;
	hd_ptr->ngrp_lin = hd_ptr->ns_lin;
	hd_ptr->ngrp_pix = hd_ptr->ns_pix;
	hd_ptr->nsamp_lin = hd_ptr->ns_lin;
	hd_ptr->nsamp_pix = hd_ptr->ns_pix;
	hd_ptr->min_smp = -16;
	hd_ptr->max_smp = 15;
	for (i=0; i<hd_ptr->nbin; i++) {
		hd_ptr->data_values_hist[i] = qhstgrm[i];
		value[i] = (float) (i-32);
	}

	hist_stat(hd_ptr->nbin,hd_ptr->data_values_hist,value,
               &hd_ptr->min_smp, &hd_ptr->max_smp,
               &hd_ptr->min_hist, &hd_ptr->max_hist,
               &hd_ptr->mean_smp, &hd_ptr->std_smp,
               &hd_ptr->mean_hist, &hd_ptr->std_hist);

	
	q_mean_hist = hd_ptr->mean_hist;

/* Fill in the processed data histogram record */

	d = d->next;
	hd_ptr = d->data_set;
	hd_ptr->nbin = 256;
	histogram(hd_ptr->nbin, hstgrm, hstgrm_img);
	for (i=0; i<hd_ptr->nbin; i++) {
		hd_ptr->data_values_hist[i] = hstgrm [i];
/*
		printf("hd_ptr->data_values_hist[%d] = %d\n", i, hd_ptr->data_values_hist[i]);
*/
		if (!strcmp(odl_file->prod_type, "COMPLEX"))
			value[i] = (float) (i-128);
		else 
			value[i] = (float) i;
	}
	fflush(stdout);
	if (!strcmp(odl_file->prod_type, "STANDARD")) {
		Fill_str(hd_ptr->hist_desc, sizeof(hd_ptr->hist_desc),
				"DETECTED DATA");
		hd_ptr->min_smp = 0.0;
		hd_ptr->max_smp = 255.0;
		hd_ptr->smp_inc = 1.0;
		hd_ptr->ns_lin = num_rec;
		hd_ptr->ns_pix = rec_length;
	}
	else if (!strcmp(odl_file->prod_type, "RAMP")) {
		Fill_str(hd_ptr->hist_desc, sizeof(hd_ptr->hist_desc),
				"DETECTED DATA");
		hd_ptr->min_smp = 0.0;
		hd_ptr->max_smp = 65535.0;
		hd_ptr->smp_inc = 256.0;
		hd_ptr->ns_lin = num_rec;
		hd_ptr->ns_pix = rec_length / 2;
	}
	else {
		Fill_str(hd_ptr->hist_desc, sizeof(hd_ptr->hist_desc),
				"REAL COMPONENT");
		hd_ptr->min_smp = -32768.0;
		hd_ptr->max_smp = 32767.0;
		hd_ptr->smp_inc = 256.0;
		hd_ptr->ns_lin = num_rec - 1;
		hd_ptr->ns_pix = (rec_length - 192) / 4;
	}
	hd_ptr->ngrp_lin = hd_ptr->ns_lin;
	hd_ptr->ngrp_pix = hd_ptr->ns_pix;
	hd_ptr->nsamp_lin = hd_ptr->ns_lin;
	hd_ptr->nsamp_pix = hd_ptr->ns_pix;

	hist_stat(hd_ptr->nbin, hd_ptr->data_values_hist, value,
		&hd_ptr->min_smp, &hd_ptr->max_smp,
		&hd_ptr->min_hist, &hd_ptr->max_hist,
		&hd_ptr->mean_smp, &hd_ptr->std_smp,
		&hd_ptr->mean_hist, &hd_ptr->std_hist); 

	if (!strcmp(odl_file->prod_type, "RAMP") || 
			!strcmp(odl_file->prod_type, "COMPLEX")) {
	hd_ptr->mean_smp = hd_ptr->mean_smp * 256.0;
	hd_ptr->std_smp = hd_ptr->std_smp * 256.0;
	}
	

	if (!strcmp(odl_file->prod_type, "COMPLEX")) {
/*
		printf("inside if\n");
		fflush(stdout);
		tmp_ptr = (Hist_Data_Set *) malloc(sizeof(Hist_Data_Set));
		tmp_ptr->next = NULL;
		hd_ptr->next = tmp_ptr;
		hd_ptr = hd_ptr->next;
		printf("before loop\n");
		fflush(stdout);
		printf("hd_ptr->nbin = %d\n", hd_ptr->nbin);
		fflush(stdout);
		hd_ptr->data_values_hist = (long *)malloc(hd_ptr->nbin * sizeof(long));
*/
		hd_ptr = hd_ptr->next;
		hd_ptr->nbin = 256;
		for (i=0; i<hd_ptr->nbin; i++) {
			hd_ptr->data_values_hist[i] = hstgrm_img[i];
/*
			printf("hd_ptr->data_values_hist[%d] = %d\n", i, hd_ptr->data_values_hist[i]);
			fflush(stdout);
*/
			value[i] = (float) (i-128);
		}
		Fill_str(hd_ptr->hist_desc, sizeof(hd_ptr->hist_desc),
				"IMAGINARY COMPONENT");
		hd_ptr->min_smp = -32768.0;
		hd_ptr->ns_lin = num_rec - 1;
		hd_ptr->ns_pix = (rec_length - 192) / 4;
		hd_ptr->ngrp_lin = hd_ptr->ns_lin;
		hd_ptr->ngrp_pix = hd_ptr->ns_pix;
		hd_ptr->nsamp_lin = hd_ptr->ns_lin;
		hd_ptr->nsamp_pix = hd_ptr->ns_pix;
		hd_ptr->max_smp = 32767.0;
		hd_ptr->smp_inc = 256.0;
		hist_stat(hd_ptr->nbin, hd_ptr->data_values_hist, value,
			&hd_ptr->min_smp, &hd_ptr->max_smp,
			&hd_ptr->min_hist, &hd_ptr->max_hist,
			&hd_ptr->mean_smp, &hd_ptr->std_smp,
			&hd_ptr->mean_hist, &hd_ptr->std_hist); 

		hd_ptr->mean_smp = hd_ptr->mean_smp * 256.0;
		hd_ptr->std_smp = hd_ptr->std_smp * 256.0;

	}
	return (PASS);
}
/************************************************************************/
int Fill_RS_values(Rng_Spec* d)
{
	int i;
 
	d->sar_chan = 1;
	d->n_dset = 1;
	d->dset_size = 4032;
	d->req_recs = 1;
	d->table_no = 1;
	d->n_pixels = -99;
	d->pixel_offset = 0;
	d->n_lines = -99;
	d->first_freq = -99.0;
	d->last_freq = -99.0;
	d->min_power = -99.0;
	d->max_power = -99.0;
	d->n_bins = 256;
	
	for (i=0; i<256; i++) d->data_values_spec[i] = range_spectra.spectrum[i];

/* blank fill the remaining spares at the end */

	for (i=0; i<sizeof(d->spare_rsr_3); i++) d->spare_rsr_3[i] = ' ';

	return (PASS);
}
/************************************************************************/
int Fill_FR_values(Fac_Related* d, 
                   Dataset_Sum* dss, 
                   Data_Hist* hist,
                   Radi_Data* rd)
{
	char year[60];
	double sum, noise_scl_fact, rcf_avg;
	int i;

	/* PPRRRRRNN */
	Fill_str(d->datatake_ID, sizeof(d->datatake_ID), "%.7s%.2d", 
                   odl_file->product_id, odl_file->seq);

	/* FFFMDSvvv */
	Fill_str(d->image_ID, sizeof(d->image_ID), "%s", &odl_file->product_id[7]);
	
	Fill_str(d->corr_year, sizeof(d->corr_year), "");
	Fill_str(d->corr_GMT, sizeof(d->corr_GMT), "");
	Fill_str(d->site_name, sizeof(d->site_name), "%s", odl_file->sitename);
	strcpy(year, facility_record.center_gmt);
	year[4] = '\0';
	Fill_str(d->data_year, sizeof(d->data_year), "%s", year);
	Fill_str(d->center_GMT, sizeof(d->center_GMT), "%s",
			&facility_record.center_gmt[5]);
	d->center_LAT = facility_record.center_lat;
	d->center_LON = facility_record.center_lon;
	if (!strcmp(odl_file->frame_mode, "ANTARCTIC")) {
		d->near_start_LAT = facility_record.near_start_lat;
		d->near_start_LON = facility_record.near_start_lon;
		d->near_end_LAT = facility_record.near_end_lat;  
		d->near_end_LON = facility_record.near_end_lon;
		d->far_start_LAT = facility_record.far_start_lat;
		d->far_start_LON = facility_record.far_start_lon;
		d->far_end_LAT = facility_record.far_end_lat;
		d->far_end_LON = facility_record.far_end_lon;
	}
	else {
		d->near_start_LAT = facility_record.near_end_lat;
		d->near_start_LON = facility_record.near_end_lon;
		d->near_end_LAT = facility_record.near_start_lat;  
		d->near_end_LON = facility_record.near_start_lon;
		d->far_start_LAT = facility_record.far_end_lat;
		d->far_start_LON = facility_record.far_end_lon;
		d->far_end_LAT = facility_record.far_start_lat;
		d->far_end_LON = facility_record.far_start_lon;
	}
	
	d->actual_azimuth = -99.0;
	d->actual_range = -99.0;

	if (!strcmp(odl_file->prod_type, "STANDARD")) {
		d->actual_pixels = rec_length;
		d->actual_lines = num_rec;
	}
	else if (!strcmp(odl_file->prod_type, "RAMP")) {
		d->actual_pixels = rec_length / 2;
		d->actual_lines = num_rec;
	}
	else {
		d->actual_pixels = (rec_length - 192) / 4;
		d->actual_lines = num_rec - 1;
	}
/*
	d->actual_lines = num_rec;
*/
	d->total_pixels = d->actual_pixels;
	d->total_lines = d->actual_lines;

	Fill_str(d->media_id, sizeof(d->media_id), "%s", odl_file->media_id);
	d->start_address = odl_file->start_address;
	d->end_address = odl_file->end_address;

	if (!strcmp(odl_file->platform, "R1"))
		Fill_str(d->platform_name, sizeof(d->platform_name), "RADARSAT-1");
	else if (!strcmp(odl_file->platform, "E1"))
		Fill_str(d->platform_name, sizeof(d->platform_name), "ERS-1");
	else if (!strcmp(odl_file->platform, "E2"))
		Fill_str(d->platform_name, sizeof(d->platform_name), "ERS-2");
	else
		Fill_str(d->platform_name, sizeof(d->platform_name), "JERS-1");

	Fill_str(d->sensor_mode, sizeof(d->sensor_mode), "%s",
				dss->sensor_id);
	d->PRF = dss->prf;
	d->ant_look_angle = dss->ele_sight;
	d->data_rate = -99.0;
	d->data_win_pos = -99.0;
	d->range_gate_del = -99.0;
	d->track_angle = facility_record.track_angle;
	if (facility_record.z_velocity > 0)
		Fill_str(d->ASC_DESC, sizeof(d->ASC_DESC), "A");
	else
		Fill_str(d->ASC_DESC, sizeof(d->ASC_DESC), "D");
	d->S_C_altitude = facility_record.s_c_altitude;
	d->X_position = facility_record.x_position;
	d->Y_position = facility_record.y_position;
	d->Z_position = facility_record.z_position;
	d->X_velocity = facility_record.x_velocity;
	d->Y_velocity = facility_record.y_velocity;
	d->Z_velocity = facility_record.z_velocity;
	d->roll = 0.0;
	d->yaw = 0.0;
	d->pitch = 0.0;
	d->roll_flag = 0;
	d->yaw_flag = 0;
	d->pitch_flag = 0;
	d->roll_rate = 0.0;
	d->yaw_rate = 0.0;
	d->pitch_rate = 0.0;
	d->roll_rate_flag = 0;
	d->yaw_rate_flag = 0;
	d->pitch_rate_flag = 0;
	d->nadir_radius = facility_record.nadir_radius;
	d->image_radius = facility_record.image_radius;
	d->incidence_angle = dss->incident_ang;
	Fill_str(d->proc_version, sizeof(d->proc_version), "2.41");
	Fill_str(d->proc_type, sizeof(d->proc_type), "");
	Fill_str(d->type_ephemeris, sizeof(d->type_ephemeris), "");
	d->looks_azimuth = -99.0;
	d->looks_range = -99.0;
	d->azi_weight_fac = -99.0;
	d->range_weight_fac = -99.0;
	Fill_str(d->look_energy_eq, sizeof(d->look_energy_eq), "NOT");
	d->induced_azimuth = -99.0;
	d->induced_range = -99.0;
	d->gain = odl_file->proc_gain;
	d->swath_velocity = facility_record.swath_velocity;
	d->squint_angle = facility_record.squint_angle;
	d->avg_terrain_ht = odl_file->avg_terrain;
	d->processor_gain = odl_file->proc_gain;
	Fill_str(d->deskew, sizeof(d->deskew), "%s", odl_file->deskew);
/*
	if (strstr(odl_file->projection, "GROUND") != NULL)
		Fill_str(d->gnd_slant_flag, sizeof(d->gnd_slant_flag), "GROUND");
	else if (strstr(odl_file->projection, "SLANT") != NULL)
		Fill_str(d->gnd_slant_flag, sizeof(d->gnd_slant_flag), "SLANT");
	else
		Fill_str(d->gnd_slant_flag, sizeof(d->gnd_slant_flag), "");
*/
	if (!strcmp(odl_file->prod_type, "RAMP") ||
		!strcmp(odl_file->prod_type, "COMPLEX"))
		Fill_str(d->gnd_slant_flag, sizeof(d->gnd_slant_flag), "SLANT");
	else
		Fill_str(d->gnd_slant_flag, sizeof(d->gnd_slant_flag), "GROUND");

	d->sl_rng_1st_pix = facility_record.sl_rng_1st_pix;
	d->sl_rng_last_pix = facility_record.sl_rng_last_pix;
	
	d->start_sample = 0;
	Fill_str(d->clutterlock_flg, sizeof(d->clutterlock_flg), "YES");
	d->dop_frq_const = facility_record.dop_frq_const;
	d->dop_frq_slope = facility_record.dop_frq_slope;
	d->dop_frq_quad = facility_record.dop_frq_quad;
	Fill_str(d->autofocus_flag, sizeof(d->autofocus_flag), "%s",
				dss->auto_focus);
	d->dop_frq_r_cnst = -99.0;
	d->dop_frq_r_slope = -99.0;
	d->dop_frq_r_quad = -99.0;
	d->azi_res = dss->azi_res;
	d->rng_res = dss->rng_res;
	d->azimuth_pixel = -99.0;
	d->range_pixel = -99.0;
	Fill_str(d->OBRC_flag, sizeof(d->OBRC_flag), "NOT");
	d->bits_sample = 4;
	d->calib_est = -99.0;
	d->bit_err_rate = aux_file->bit_error_rate;

	if (cali_file->noise_fact <= 0) {
		d->SNR = -99.0;
	}
	else {
	sum = 0.0;
	for (i=0; i<256; i++)
		sum += rd->lookup_tab[i];
	rcf_avg = sum/256.0;
	noise_scl_fact = cali_file->noise_fact * 1 * pow(10.0, odl_file->proc_gain/10.0);
	d->SNR = 10.0*log10((hist->data_set->mean_smp * hist->data_set->mean_smp)/(noise_scl_fact * rcf_avg));
	printf("sum = %lf rcf_avg = %lf noise_scl_fact = %lf\n", sum, rcf_avg, noise_scl_fact);
	printf("noise_fact = %lf proc_gain = %lf mean_samp = %lf\n", cali_file->noise_fact, odl_file->proc_gain, hist->data_set->mean_smp);

	}
	SNR = d->SNR;


	printf("SNR = %lf\n", d->SNR);
        fflush(stdout);

	d->est_noise_flr = -99.0;
	d->radio_m_resol = -99.0;
	d->satur_points = -99;
	Fill_str(d->spec_flag, sizeof(d->spec_flag), "");
	d->repl_agc = aux_file->replica_agc;
	d->temp_rx_lna = aux_file->lna_temp;
	d->temp_rx_sub = aux_file->subsystem_temp;
	d->temp_rx_prot = aux_file->protector_temp;
	d->temp_cal_sys = aux_file->calibration_temp;
	d->rx_agc = aux_file->rx_agc;
	d->pre_cal1_pow = -99.0;
	d->pre_cal2_pow = -99.0;
	d->post_cal1_pow = -99.0;
	d->post_cal2_pow = -99.0;
	d->repl_pow = -99.0;
	d->ssar_roll_ang = -99.0;
	Fill_str(d->comment, sizeof(d->comment), "");
	
	return (PASS);
}
/************************************************************************/
char* version(char* file)
{
	FILE *fp;
	char line[500];
	static char string[20];
	int i,j,len;

	if ((fp = fopen(file, "r")) == NULL) return "";
	if ((fgets(line, sizeof(line), fp)) == NULL) { 
		fclose(fp);
		return "";
	}
	fclose(fp);

	len = strlen(line);
	for (i=0; i<len && !isdigit(line[i]); i++)
		;

	if (i == len) return "";
	j = 0;
  do {
  	string[j++] = line[i++];
  } while (isdigit(line[i]) || (line[i] == '.'));

  string[j] = '\0';

	return string;
}


/************************************************************************/
int histogram (long nbins, long hstgrm[], long hstgrm_img[])
{
	int i, j, fd;
	int pix_val, pix_val_img;
	unsigned char* buf;
	unsigned short* sbuf;
	signed short* complex_buf;
	char file[256];
	int num_pix;
	
	printf("start histogram\n");
	fflush(stdout);

	for (i=0; i<nbins; i++) {
		hstgrm[i] = 0;
		hstgrm_img[i] = 0;
	}

	sprintf(file, "/home/tmpdisk/image.dat.%d", odl_file->job_id);

	if ((fd = open(file, O_RDONLY)) == -1)
		info_handler(ierr_2, file, "can't open file %s", file);


	if (!strcmp(odl_file->prod_type, "STANDARD")) {
		buf = (unsigned char *) malloc(rec_length+192);
		read(fd, buf, rec_length+192);
		for (i=1; i<=num_rec; i++) {
			read(fd, buf, rec_length+192);
			for (j=192; j<(rec_length+192); j++) {
				pix_val = buf[j];
				if (pix_val < 0) pix_val = 0;
				if (pix_val > nbins) pix_val = nbins - 1;
				hstgrm[pix_val]++;
			}
		}
		hstgrm[0] = 0;
		free(buf);
	}
	else if (!strcmp(odl_file->prod_type, "RAMP")) {
		num_pix = (rec_length+192)/ 2;
		sbuf = (unsigned short *) malloc(rec_length + 192);
		read(fd, sbuf, rec_length+192);
		for (i=1; i<=num_rec; i++) {
			read(fd, sbuf, rec_length+192);
			for (j=96; j<num_pix; j++) {
				pix_val = sbuf[j]/256;
				if (pix_val < 0) pix_val = 0;
				if (pix_val > nbins) pix_val = nbins - 1;
				hstgrm[pix_val]++;
			}
		}
		hstgrm[0] = 0;
		free(sbuf);
	}
	else {  /* Complex data */
		num_pix = rec_length / 4;
		complex_buf = (signed short *) malloc(rec_length);
		read(fd, complex_buf, rec_length);
		for (i=1; i<num_rec; i++) {
			read(fd, complex_buf, rec_length);
			for (j=48; j<num_pix; j++) {
				pix_val = ((short *) &(((int *)complex_buf)[j]))[0] / 256;
				pix_val_img = ((short *) &(((int *)complex_buf)[j]))[1] / 256;
				if (pix_val > 127) pix_val = 127;
				if (pix_val_img > 127) pix_val_img = 127;
				if (pix_val < -128) pix_val = -128;
				if (pix_val_img < -128) pix_val_img = -128;
				if (!((pix_val == 0) && (pix_val_img == 0))) {
					hstgrm[pix_val+128]++;
					hstgrm_img[pix_val_img+128]++;
				}
			}
		}
		free(complex_buf);
	}
	close(fd);
	printf("end histogram\n");
	fflush(stdout);

	return 0;
}
/************************************************************************/
int iqhistogram(long nbins, long ihstgrm[], long qhstgrm[])
{
	int i, j, k, offset, echo_file_fd, counter;
	char file[256];
	char tmp_buf[1024];
	signed char *buf;
	signed char val, i_val, j_val;
	float agc_factor, agc_factor_max;
	float wt[] = {0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 
			-7.5, -6.5, -5.5, -4.5, -3.5, -2.5, -1.5, -0.5};
	
	printf("start iqhistogram\n");
	fflush(stdout);
	for (i=0; i<nbins; i++) {
		ihstgrm[i] = 0;
		qhstgrm[i] = 0;
	}

	sprintf(file, "/home/tmpdisk/echo.dat.%d", odl_file->job_id);
	if ((echo_file_fd = open(file, O_RDONLY)) == -1)
		info_handler(ierr_2, file, "can't open file %s", file);

	read(echo_file_fd, tmp_buf, 1024);
	for (i=0; i<1024-7; i++) {
		if (tmp_buf[i] == 'E' &&
		    tmp_buf[i+1] == 'C' &&
		    tmp_buf[i+2] == 'H' &&
		    tmp_buf[i+3] == 'O' &&
		    tmp_buf[i+4] == 'R' &&
		    tmp_buf[i+5] == 'A' &&
		    tmp_buf[i+6] == 'W') {
			offset = i + 32;
			break;
		}
	}
	lseek(echo_file_fd, offset, 0);
	counter = 0;

	agc_factor_max = sqrt(pow(10, (aux_file->max_agc_value/10.0)));

	for (i=1; i<=aux_file->echo_pulses; i += 10) {

		while (1) {
			if ((i >= aux_file->agc_changes[counter]) &&
				(i < aux_file->agc_changes[counter+1])) {
				agc_factor = sqrt(pow(10.0, (aux_file->agc_values[counter]/10.0)));
				break;
			}
			else {
				if (counter >= aux_file->echo_pulses) {
					counter = 0;
					break;
				}
				else
					counter++;
			}
		}
		if (!strcmp(odl_file->platform, "E1") || !strcmp(odl_file->platform, "E2")) {
			buf = (signed char *) malloc(aux_file->echo_samples*2);
			read(echo_file_fd, buf, aux_file->echo_samples*2);
			for (j=0; j<aux_file->echo_samples*2; j+=2) {
				i_val = buf[j] - 15;
				j_val = buf[j+1] - 15;

				i_val = (signed char) (agc_factor/agc_factor_max)*i_val;
				j_val = (signed char) (agc_factor/agc_factor_max)*j_val;

				if (i_val < -16) i_val = -16;
				if (j_val < -16) j_val = -16;

				ihstgrm[i_val+16]++;
				qhstgrm[j_val+16]++;
			}
		}
		else {
			buf = (signed char *) malloc(aux_file->echo_samples);
			read(echo_file_fd, buf, aux_file->echo_samples);
			for (j=0; j<aux_file->echo_samples; j++) {
				i_val = (buf[j] >> 4) & 15;  /* higher 4 bits */
				j_val = buf[j] & 15;  /* lower 4 bits */
			

			i_val = (signed char) (((wt[i_val]*agc_factor)/(15.0*agc_factor_max)) * 64.0);
			j_val = (signed char) (((wt[j_val]*agc_factor)/(15.0*agc_factor_max)) * 64.0);

			if (i_val >= 32) i_val = 31;
			if (j_val >= 32) j_val = 31;

			ihstgrm[i_val+32]++;
			qhstgrm[j_val+32]++;
			}
		}
	}
	
	close(echo_file_fd);
	free(buf);
	printf("end iqhistogram\n");
	fflush(stdout);
	return 0;
}
/************************************************************************/
/* hist_stat(nbins,hstgrm,value,minsmp,maxsmp,minfreq,maxfreq,meansamp,
             stdvsamp,meanfreq,stdvfreq)

 compute some statics for the histogram.

 inputs:        nbins   -- number of bins in the histogram
                hstgrm  -- the histogram
                value   -- the values of each bin
 output:        minsmp  -- the minimum sample value
                maxsmp  -- the maximum sample value
                minfreq -- the minimum histogram table value
                maxfreq -- the maximum histogram table value
                meansamp - the mean sample value
                stdvsamp - the std dev. of sample value
                meanfreq - the mean histogram table value
                stdvfreq - the std dev of histogram table value

*/


hist_stat(nbins,hstgrm,value,minsmp,maxsmp,minfreq,maxfreq,meansamp,
             stdvsamp,meanfreq,stdvfreq)

long nbins,hstgrm[];
float   value[],*meansamp,*stdvsamp,*minsmp,*maxsmp,
        *meanfreq,*stdvfreq,*minfreq,*maxfreq;
{

int     i,temp,mintemp,maxtemp;
double ftemp,total,total2,sum,sum2,val;

if (nbins <= 0) return;

mintemp = 9999999;
maxtemp = 0;

for (total=0., total2=0., sum=0., sum2=0., i=0; i < nbins; i++) {
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

/*
*minsmp = 1000.0;
for( i=0; i<nbins && *minsmp==1000.0 ; i++ )
        if( hstgrm[i]>0 ) *minsmp = (float) i;
*maxsmp = -1.0;
for( i=nbins-1; i>=0 && *maxsmp==-1.0 ; i-- )
        if( hstgrm[i]>0 ) *maxsmp = (float) i;
*/

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

return;

}
/************************************************************************/
/* month: set month, day from day of year */
int month_day(int year, int yearday, int *pmonth, int *pday)
{
	char daytab[2][13] = {
		{0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
		{0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31}
	};
		
	int i, leap;

	leap = year%4 == 0 && year%100 != 0 || year%400 == 0;
	for (i = 1; yearday > daytab[leap][i]; i++)
		yearday -= daytab[leap][i];
	*pmonth = i;
	*pday = yearday;
}
/************************************************************************/
int write_pmf()
{

	FILE *fp;
	char file[256];
	char local_archive_path[256];
	char *ptr;

	sprintf(file, "/home/tmpdisk/pmf.%d", odl_file->job_id);

	if ((fp = fopen(file, "w")) == NULL)
		info_handler(ierr_2, file, "can't create file %s", file);

	fprintf(fp, "OBJECT = SAR_FRAME_METADATA\n");
	fprintf(fp, "  OBJECT = COMMON_HEADER\n");
	fprintf(fp, "    TIME = %s\n", odl_file->time);
	fprintf(fp, "    MSG_TYPE = \"SAR_FRAME_METADATA\"\n");
	fprintf(fp, "    DESTINATION = \"CP\"\n");
	fprintf(fp, "    SOURCE = \"PP\"\n");
	fprintf(fp, "    NUMBER_OF_RECORDS = 1\n");
	fprintf(fp, "  END_OBJECT = COMMON_HEADER\n");
	
	fprintf(fp, "  OBJECT = CATALOG_METADATA\n");
	fprintf(fp, "    JOB_ID = %d\n", odl_file->job_id);
	fprintf(fp, "    PLATFORM = \"%s\"\n", odl_file->platform);
	fprintf(fp, "    SENSOR = \"%s\"\n", odl_file->sensor);
	fprintf(fp, "    REVOLUTION = %d\n", odl_file->revolution);
	fprintf(fp, "    SEQUENCE = %d\n", odl_file->seq);
	fprintf(fp, "    ACTIVITY_ID = \"%s\"\n", odl_file->activity);
	fprintf(fp, "    MEDIA_ID = \"%s\"\n", odl_file->media_id);
	fprintf(fp, "    MEDIA_TYPE = \"%s\"\n", odl_file->media_type);
	fprintf(fp, "    MEDIA_LOCATION = \"%s\"\n", odl_file->media_location);
	fprintf(fp, "    RECORDER_ID = \"%s\"\n", odl_file->recordid);
	fprintf(fp, "    STATION_ID = \"%s\"\n", odl_file->station_id);
	fprintf(fp, "    FRAME_MODE = \"%s\"\n", odl_file->frame_mode);
	fprintf(fp, "    SITE_NAME = \"%s\"\n", odl_file->sitename);
	fprintf(fp, "    MODE = \"%s\"\n", odl_file->instr_mode);
	fprintf(fp, "    SEGMENT_ID = 1\n");

	if (!strcmp(odl_file->comp_flag, "NO"))
		fprintf(fp, "    DATASET = \"%s\"\n",
			"RADARSAT-1 STANDARD BEAM UNCOMPENSATED PRODUCT");
	else if (!strcmp(odl_file->prod_type, "STANDARD") ||
			!strcmp(odl_file->prod_type, "RAMP"))
		fprintf(fp, "    DATASET = \"%s\"\n", 
			"RADARSAT-1 STANDARD BEAM STANDARD GRF PRODUCT");
	else if (!strcmp(odl_file->prod_type, "COMPLEX"))
		fprintf(fp, "    DATASET = \"%s\"\n",
			"RADARSAT-1 STANDARD BEAM COMPLEX PRODUCT");



	fprintf(fp, "    FILE_VERSION = %d\n", odl_file->file_version);

	strcpy(local_archive_path, odl_file->image_file);
	ptr = strrchr(local_archive_path, '/');
	*++ptr = '\0';
	
	fprintf(fp, "    LOCAL_ARCHIVE_PATH = \"%s\"\n", local_archive_path);
	fprintf(fp, "    PRODUCT_ID = \"%s\"\n", odl_file->product_id);
	fprintf(fp, "    IMAGE_FILE = \"%s\"\n", odl_file->image_file);
	fprintf(fp, "    CEOS_LEADER_FILE = \"%s\"\n", odl_file->ceos_leader_file);
	fprintf(fp, "    FRAME_ID = %d\n", odl_file->frame_id);
	fprintf(fp, "    SUBFRAME_ID = %d\n", odl_file->subframe_id);
	fprintf(fp, "    START_ADDRESS = %d\n", odl_file->start_address);
	fprintf(fp, "    END_ADDRESS = %d\n", odl_file->end_address);
	if (!strcmp(odl_file->prod_type, "STANDARD") ||
		!strcmp(odl_file->prod_type, "RAMP")) {
	fprintf(fp, "    IMAGE_RECORD_COUNT = %d\n", num_rec + 1);
	fprintf(fp, "    IMAGE_FIRST_RECORD_LENGTH = %d\n", rec_length + 192);
	fprintf(fp, "    IMAGE_MAX_RECORD_LENGTH = %d\n", rec_length + 192);
	}
	else {
	fprintf(fp, "    IMAGE_RECORD_COUNT = %d\n", num_rec);
	fprintf(fp, "    IMAGE_FIRST_RECORD_LENGTH = %d\n", rec_length);
	fprintf(fp, "    IMAGE_MAX_RECORD_LENGTH = %d\n", rec_length);
	}
	fprintf(fp, "    LEADER_MAX_RECORD_LENGTH = %d\n", 5120);
	fprintf(fp, "    LEADER_RECORD_COUNT = %d\n", 10);
	fprintf(fp, "    PRODUCT_CREATOR = \"PP\"\n");
	if (!strcmp(odl_file->prod_type, "STANDARD") ||
		!strcmp(odl_file->prod_type, "RAMP"))
		fprintf(fp, "    CEOS_PRODUCT_TYPE = \"GRF\"\n");
	else
		fprintf(fp, "    CEOS_PRODUCT_TYPE = \"CPX\"\n");
	fprintf(fp, "    START_TIME = %s\n", facility_record.start_gmt);
	fprintf(fp, "    END_TIME = %s\n", facility_record.end_gmt);
	fprintf(fp, "    CENTER_LAT = %lf\n", facility_record.center_lat);
	fprintf(fp, "    CENTER_LON = %lf\n", facility_record.center_lon);
	fprintf(fp, "    CENTER_TIME = \"%s\"\n", facility_record.center_gmt);
	if (!strcmp(odl_file->frame_mode, "ARCTIC")) {
		fprintf(fp, "    NEAR_START_LAT = %lf\n", facility_record.near_end_lat);
		fprintf(fp, "    NEAR_START_LON = %lf\n", facility_record.near_end_lon);
		fprintf(fp, "    NEAR_END_LAT = %lf\n", facility_record.near_start_lat);
		fprintf(fp, "    NEAR_END_LON = %lf\n", facility_record.near_start_lon);
		fprintf(fp, "    FAR_START_LAT = %lf\n", facility_record.far_end_lat);
		fprintf(fp, "    FAR_START_LON = %lf\n", facility_record.far_end_lon);
		fprintf(fp, "    FAR_END_LAT = %lf\n", facility_record.far_start_lat);
		fprintf(fp, "    FAR_END_LON = %lf\n", facility_record.far_start_lon);
	}
	else {
		fprintf(fp, "    NEAR_START_LAT = %lf\n", facility_record.near_start_lat);
		fprintf(fp, "    NEAR_START_LON = %lf\n", facility_record.near_start_lon);
		fprintf(fp, "    NEAR_END_LAT = %lf\n", facility_record.near_end_lat);
		fprintf(fp, "    NEAR_END_LON = %lf\n", facility_record.near_end_lon);
		fprintf(fp, "    FAR_START_LAT = %lf\n", facility_record.far_start_lat);
		fprintf(fp, "    FAR_START_LON = %lf\n", facility_record.far_start_lon);
		fprintf(fp, "    FAR_END_LAT = %lf\n", facility_record.far_end_lat);
		fprintf(fp, "    FAR_END_LON = %lf\n", facility_record.far_end_lon);
	}

	if (facility_record.z_velocity > 0)
		fprintf(fp, "    ASC_DESC = \"A\"\n");
	else
		fprintf(fp, "    ASC_DESC = \"D\"\n");

	fprintf(fp, "    PROC_VERSION = \"VERS2.41\"\n");
	fprintf(fp, "    STATE_VECTOR_PRECISION = \"%s\"\n", odl_file->sv_type_precision);
	fprintf(fp, "    SIGNAL_TO_NOISE_RATIO = %lf\n", SNR);
	fprintf(fp, "    RADIOMETRIC_ACCURACY = %lf\n", cali_file->rel_radio_acc);
	fprintf(fp, "    PRODUCT_CREATION_TIME = %s\n", odl_file->time);
	fprintf(fp, "  END_OBJECT = CATALOG_METADATA\n");
	fprintf(fp, "END_OBJECT = SAR_FRAME_METADATA\n");
	fprintf(fp, "END\n");

	
	fclose(fp);
}


/************************************************************************/

/*  double  ctok (rx, ry, rz, vx, vy, vz, kepler)  --------------

**********************************************************************
*                                                                    *
* 'ctok.c'  converts the Cartesian State Vector to Kepler elements   *
*                                                                    *
**********************************************************************

INPUTS:
variables:      rx,ry,rz
type:           double
description:    Cartesian coordinates in the x,y,z axis in Km.

variables:      vx,vy,vz
type:           double
description:    Velocity in the x,y,z direction in Km/sec.

OUTPUTS:
variable:       kepler[6]
type:           double
description:    the resultant Kepler transformation consisting of the
                six Kepler elements,namely,
                'a' the semi-major axis in Km
                'e' the eccentricity
                'i' angle of inclination (deg)
                'Omega' longitude of ascending node (deg.)
                'w' argument of periapsis (deg.)
                'M' mean anomaly (deg.)
*/

double  ctok (rx, ry, rz, vx, vy, vz, kepler)
double  rx, ry, rz, vx, vy, vz, kepler[6];

{
    double  a, u, r, v, es, ec, e, E, M_deg, V2, H, i, i_deg;
    double  w_deg, cu, su, somega, comega, omega;
    double  pi, mu;
    double tanfix();

    pi = 3.141592653589793;
    mu = 3.9860045e+5;


 /* determine semi major axis 'a' */
    r = sqrt ((rx * rx) + (ry * ry) + (rz * rz));
    V2 = (vx * vx) + (vy * vy) + (vz * vz);
    a = (mu * r) / ((2.0 * mu) - (r * V2));

 /* determine eccentricity 'e' */
    es = ((rx * vx) + (ry * vy) + (rz * vz)) / sqrt (mu * a);
    ec = 1.0 - (r / a);
    e = sqrt ((es * es) + (ec * ec));
		printf("e = %lf\n", e);

/* determine mean anomaly'M' */
    E = 2.0 * tanfix ((e - ec), es);
		printf("E = %lf\n", E);
    M_deg = (180.0 / pi) * (E - es);
    if (M_deg < 0.0)
        M_deg = M_deg + 360.0;

/* determine angle of inclination 'i' */
    H = sqrt (mu * a * (1.0 - (e * e)));
    i = acos (((rx * vy) - (ry * vx)) / H);
    i_deg = i * (180.0 / pi);

/* determine omega */
    somega = ((ry * vz) - (rz * vy)) / (sin (i) * H);
    comega = ((rx * vz) - (rz * vx)) / (sin (i) * H);
    omega = (180.0 / pi) * 2.0 * tanfix ((1.0 - comega), somega);
    if (omega < 0.0)
        omega = omega + 360.0;

/* determine w_deg */
    su = rz / (r * sin (i));
    cu = ((ry / r) * somega) + ((rx / r) * comega);
    if (rz == 0.0)
        cu = 1.0;
    u = 2 * tanfix ((1.0 - cu), su);
    v = 2 * atan (sqrt ((1.0 + e) / (1.0 - e)) * tan (E / 2.0));
    w_deg = (180.0 / pi) * (u - v);
    if (w_deg < 0.0)
        w_deg = w_deg + 360.0;


    kepler[0] = a;
    kepler[1] = e;
    kepler[2] = i_deg;
    kepler[3] = omega;
    kepler[4] = w_deg;
    kepler[5] = M_deg;

		printf("a = %lf e = %lf i_deg = %lf omega = %lf w_deg = %lf M_deg = %lf\n",
		        a, e, i_deg, omega, w_deg, M_deg);

    return;
}

/* tanfix(a,b) --------------------------------------------------------

        This routine calculates the tangent of a/b, protecting
        against b=0.
*/

double tanfix(a,b)
    double a,b;
{
    double pi = 3.141592653589793;

    if (b == 0.0) {
        if (a < 0.0)
            return (-pi / 2.0);
        else if (a > 0.0)
            return (pi / 2.0);
        else
            return (0.0);
    }
    return (atan (a/b));
}

