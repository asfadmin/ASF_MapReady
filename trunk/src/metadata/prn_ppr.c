#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

char *sprn_ppr(struct proc_parm_rec *p)
{
  int i, j;
  char *ret = MALLOC(sizeof(char)*1);
  strcpy(ret, "");

  add(&ret, "\n*********** begin of record *******************\n");
  add(&ret, "\n Record sequence number\t%d", p->seq_num);
  add(&ret, "\n Input media\t%s", p->inp_media);
  add(&ret, "\n Number of input tape id\t%d", p->n_tape_id);
  for (i=0; i<p->n_tape_id; i++)
    add(&ret, "\n Tape identifier [%2d]\t%s", i+1, p->tape_id[i]);
  add(&ret, "\n Expected ingest start time\t%s", p->exp_ing_start);
  add(&ret, "\n Expected ingest stop time\t%s", p->exp_ing_stop);
  add(&ret, "\n Actual ingest start time\t%s", p->act_ing_start);
  add(&ret, "\n Actual ingest stop time\t%s", p->act_ing_stop);
  add(&ret, "\n Processing start time\t%s", p->proc_start);
  add(&ret, "\n Processing stop time\t%s", p->proc_stop);
  for (i=0; i<10; i++)
    add(&ret, "\n Mean signal levels across range [%d]\t%16.7lf", 
	i+1, p->mn_sig_lev[i]);
  add(&ret, "\n Source data quality indicator\t%d", p->src_data_ind);
  add(&ret, "\n Number of missing lines\t%d", p->miss_ln);
  add(&ret, "\n Number of rejected lines\t%d", p->rej_ln);
  add(&ret, "\n Number of time inconsistencies\t%d", p->large_gap);
  add(&ret, "\n Measured bit error rate\t%16.7lf", p->bit_error_rate);
  add(&ret, "\n Percent of frames with CRC errors\t%16.7lf", p->fm_crc_err);
  add(&ret, "\n Number of date inconsistencies\t%d", p->date_incons);
  add(&ret, "\n Number of unexpected PRF changes\t%d", p->prf_changes);
  add(&ret, "\n Number of delay changes\t%d", p->delay_changes);
  add(&ret, "\n Number of skippped frames\t%d", p->skipd_frams);
  add(&ret, "\n Range lines reject before start time\t%d", p->rej_bf_start);
  add(&ret, "\n Range lines reject due to too few frames\t%d", 
      p->rej_few_fram);
  add(&ret, "\n Range lines reject due to too many frames\t%d", 
      p->rej_many_fram);
  add(&ret, "\n Frames reject due to master channel error\t%d", 
      p->rej_mchn_err);
  add(&ret, "\n Frames reject due to virtual channel error\t%d",
      p->rej_vchn_err);
  add(&ret, "\n Frames reject due to incorrect record type\t%d",
      p->rej_rec_type);
  add(&ret, "\n Sensor configuration\t%s", p->sens_config);
  add(&ret, "\n Sensor orientation\t%s", p->sens_orient);
  add(&ret, "\n Frame synch marker\t%s", p->sych_marker);
  add(&ret, "\n Range reference function source\t%s", p->rng_ref_src);
  for (i=0; i<4; i++)
    add(&ret, "\n Range reference amplitude coefficient [%d]\t%16.7lf",
	i+1, p->rng_amp_coef[i]);
  for (i=0; i<4; i++)
    add(&ret, "\n Range reference phase coefficient [%d]\t%16.7lf",
	i+1, p->rng_phas_coef[i]);
  for (i=0; i<4; i++)
    add(&ret, "\n Error function amplitude coefficient [%d]\t%16.7lf",
	i+1, p->err_amp_coef[i]);
  for (i=0; i<4; i++)
    add(&ret, "\n Error function phase coefficient [%d]\t%16.7lf",
	i+1, p->err_phas_coef[i]);
  add(&ret, "\n Pulse bandwidth code [10^-2 MHz]\t%d", p->pulse_bandw);
  add(&ret, "\n ADC sampling rate [10^-3 Msamp/s]\t%s", p->adc_samp_rate);
  add(&ret, "\n Replica AGC attenuation\t%16.7lf", p->rep_agc_attn);
  add(&ret, "\n Gain correction factor [dB]\t%16.7lf", p->gn_corctn_fctr);
  add(&ret, "\n Replica energy gain correction\t%16.7lf", p->rep_energy_gn);
  add(&ret, "\n Orbit data source\t%s", p->orb_data_src);
  add(&ret, "\n Pulse count 1\t%d", p->pulse_cnt_1);
  add(&ret, "\n Pulse count 2\t%d", p->pulse_cnt_2);
  add(&ret, "\n Beam edge detection requested\t%s", p->beam_edge_rqd);
  add(&ret, "\n Beam edge confidence measure\t%16.7lf", p->beam_edge_conf);
  add(&ret, "\n Number of pixels in beam overlap\t%d", p->pix_overlap);
  add(&ret, "\n Number of beams\t%d", p->n_beams);
  for (i=0; i<p->n_beams; i++) {  
    add(&ret, "\n Beam type [%d]\t%s", i+1, p->beam_info[i].beam_type);
    add(&ret, "\n Elevation beam look angle source [%d]\t%s",
	i+1, p->beam_info[i].beam_look_src);
    add(&ret, "\n Applied elev beam look angle [deg] [%d]\t%16.7lf",
	i+1, p->beam_info[i].beam_look_ang);
    add(&ret, "\n Actual PRF [Hz] [%d]\t%16.7lf", i+1, p->beam_info[i].prf);
  }
  add(&ret, "\n Number of pixel count updates\t%d", p->n_pix_updates);
  for (i=0; i<p->n_pix_updates; i++) {
    add(&ret, "\n Pixel count update time [%d]\t%s", 
	i+1, p->pix_count[i].pix_update);
    for (j=0; j<4; j++)
      add(&ret, "\n Count of image pixels in beam %d\t%d", 
	  j+1, p->pix_count[i].n_pix[j]);
  }
  add(&ret, "\n Processing window start time [sec]\t%16.7lf", p->pwin_start);
  add(&ret, "\n Processing window end time [sec]\t%16.7lf", p->pwin_end);
  add(&ret, "\n Recording type\t%s", p->recd_type);
  add(&ret, "\n Time increment between temperature setttings [sec]\t%16.7lf",
      p->temp_set_inc);
  add(&ret, "\n Number of temperature settings\t%d", p->n_temp_set);
  for (i=0; i<p->n_temp_set; i++)
    for (j=0; j<4; j++)
      add(&ret, "\n Temperature settings [%2d][%d]\t%d", 
	  i+1, j+1, p->temp[i].temp_set[j]);
  add(&ret, "\n Number of image pixels\t%d", p->n_image_pix);
  add(&ret, "\n Percent zero pixels\t%16.7lf", p->prc_zero_pix);
  add(&ret, "\n Percent saturated pixels\t%16.7lf", p->prc_satur_pix);
  add(&ret, "\n Image histogram mean intensity\t%16.7lf", p->img_hist_mean);
  for (i=0; i<3; i++)
    add(&ret, "\n Image cumulative distribution [%d]\t%16.7lf", 
	i+1, p->img_cumu_dist[i]);
  add(&ret, "\n Pre-image calibration gain factor\t%16.7lf", p->pre_img_gn);
  add(&ret, "\n Post-image calibration gain factor\t%16.7lf", p->post_img_gn);
  add(&ret, "\n Time increment between DopCen estimate [sec]\t%16.7lf",
      p->dopcen_inc);
  add(&ret, "\n Number of Doppler Centroid estimates\t%d", p->n_dopcen);
  for (i=0; i<p->n_dopcen; i++) {
    add(&ret, "\n Doppler Centroid confidence measure [%2d]\t%16.7lf", 
	i+1, p->dopcen_est[i].dopcen_conf);
    add(&ret, "\n Doppler Centroid reference time [%2d]\t%16.7lf",
	i+1, p->dopcen_est[i].dopcen_ref_tim);
    for (j=0; j<4; j++)
    add(&ret, "\n Doppler Centroid coefficients [%2d][%d]\t%16.7lf",
	i+1, j+1, p->dopcen_est[i].dopcen_coef[j]);
  }
  add(&ret, "\n Doppler ambiguity error\t%d", p->dopamb_err);
  add(&ret, "\n Doppler ambiguity confidence measure\t%16.7lf",
      p->dopamb_conf);
  for (i=0; i<7; i++)
    add(&ret, "\n Ephemeris orbit data [%d]\t%16.7lf", 
	i+1, p->eph_orb_data[i]);
  add(&ret, "\n Application type\t%s", p->appl_type);
  for (i=0; i<5; i++)
    add(&ret, "\n Slow time coefficient [%d]\t%16.7lf", 
	i+1, p->slow_time_coef[i]);
  add(&ret, "\n Number of SRGR coefficient sets\t%d", p->n_srgr);
  for (i=0; i<p->n_srgr; i++) {
    add(&ret, "\n SRGR update date/time [%2d]\t%s", 
	i+1, p->srgr_coefset[i].srgr_update);
    for (j=0; j<6; j++)
    add(&ret, "\n SRGR coefficient [%2d][[%d]\t%16.7g", 
	i+1, j+1, p->srgr_coefset[i].srgr_coef[j]);
  }
  add(&ret, "\n SGF product pixel spacing\t%16.lf", p->pixel_spacing);
  add(&ret, "\n GICS product required\t%s", p->pics_reqd);
  add(&ret, "\n Work order identifier\t%s", p->wo_number);
  add(&ret, "\n Work order entry date\t%s", p->wo_date);
  add(&ret, "\n Satellite identifier\t%s", p->satellite_id);
  add(&ret, "\n User id\t%s", p->user_id);
  add(&ret, "\n Completion message required flag\t%s", p->complete_msg);
  add(&ret, "\n SGF product scene identifier\t%s", p->scene_id);
  add(&ret, "\n Density of SGF product media\t%s", p->density_in);
  add(&ret, "\n SGF product identifer\t%s", p->media_id);
  add(&ret, "\n Incidence angle of first pixel\t%16.7lf", p->angle_first);
  add(&ret, "\n Incidence angle of last pixel\t%16.7lf", p->angle_last);
  add(&ret, "\n GICS output product type\t%s", p->prod_type);
  add(&ret, "\n Map system identifier\t%s", p->map_system);
  add(&ret, "\n GICS output, centre latitude\t%16.7lf", p->centre_lat);
  add(&ret, "\n GICS output, centre longitude\t%16.7lf", p->centre_long);
  add(&ret, "\n GICS output, size eastings [km]\t%22.15lf", p->span_x);
  add(&ret, "\n GICS output, size northings [km]\t%22.15lf", p->span_y);
  add(&ret, "\n DTM correction to be applied flag\t%s", p->apply_dtm);
  add(&ret, "\n GICS output product density\t%s", p->density_out);
  add(&ret, "\n Time of the first state vector\t%s", p->state_time);
  add(&ret, "\n Number of state vectors\t%d", p->num_state_vectors);
  add(&ret, "\n Time increment between state vectors\t%16.7lf", 
      p->state_time_inc);
  add(&ret, "\n Scene output coordinate system\t%s", p->coord_sys);
  add(&ret, "\n*********** end of record ********************\n");
  return ret;
}

void prn_ppr(FILE *fp, struct proc_parm_rec *p)
{
    char *rec = sprn_ppr(p);
    fprintf(fp, "%s", rec);
    FREE(rec);
}
