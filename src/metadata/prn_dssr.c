/********************************************************************
NAME:   prn_dssr.c

PURPOSE:   Reads the input (.ldr) file given and prints all of the
           fields of the dataset summary record.

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0           4/96   T. Logan (ASF)
*********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

char *sprn_dssr(struct dataset_sum_rec *ds, int era)
{
  int i;

  char *ret = MALLOC(sizeof(char)*1);
  strcpy(ret, "");

  add(&ret, "\n*********** begin of Dataset Summary record *****************\n");
  add(&ret, "\n DSS SEQ NUM\t\t%d",ds->seq_num);
  add(&ret, "\n SAR CHNL INDTR\t\t%d",ds->sar_chan);
  add(&ret, "\n SCENE INDICATOR\t%s",ds->product_id);
  add(&ret, "\n SCENE DESIGNATOR\t%s",ds->scene_des);
  add(&ret, "\n INPT SCN CTR TIME\t%s",ds->inp_sctim);
  add(&ret, "\n ASC/DESCENDING\t\t%s",ds->asc_des);
  add(&ret, "\n LAT @ SCN CTR\t\t%16.7f",ds->pro_lat);
  add(&ret, "\n LONG @ SCN CTR\t\t%16.7f",ds->pro_long);
  add(&ret, "\n SCN CTR HEADING\t%16.7f",ds->pro_head);
  add(&ret, "\n ELLIP DESIGNATOR\t%s",ds->ellip_des);
  add(&ret, "\n ELLIP SEMIMAJOR\t%16.7f",ds->ellip_maj);
  add(&ret, "\n ELLIP SEMIMINOR\t%16.7f",ds->ellip_min);
  add(&ret, "\n EARTH MASS\t\t%16.7f",ds->earth_mass);
  add(&ret, "\n GRAVITATIONAL CNST\t%16.7f",ds->grav_const);
  add(&ret, "\n ELLIP PARM 1\t\t%16.7f",ds->ellip_j[0]);
  add(&ret, "\n ELLIP PARM 2\t\t%16.7f",ds->ellip_j[1]);
  add(&ret, "\n ELLIP PARM 3\t\t%16.7f",ds->ellip_j[2]);
  add(&ret, "\n AVG TERRAIN HT\t\t%16.7f",ds->terrain_h);
  add(&ret, "\n IMG CTR LINE NUM\t%f",ds->sc_lin);
  add(&ret, "\n IMG CTR PIX NUM\t%f",ds->sc_pix);
  add(&ret, "\n IMAGE LENGTH\t\t%16.7f",ds->scene_len);
  add(&ret, "\n IMAGE WIDTH\t\t%16.7f",ds->scene_wid);
  add(&ret, "\n NUM SAR CHANNELS\t%d",ds->nchn);
  add(&ret, "\n MISSION ID\t\t%s",ds->mission_id);
  add(&ret, "\n SENSOR ID\t\t%s",ds->sensor_id);
  add(&ret, "\n ORBIT NUMBER\t\t%s",ds->revolution);
  add(&ret, "\n PLAT LAT @ NADIR\t%8.3f",ds->plat_lat);
  add(&ret, "\n PLAT LONG @ NADIR\t%8.3f",ds->plat_long);
  add(&ret, "\n PLAT HEADING\t\t%8.3f",ds->plat_head_scene);
  add(&ret, "\n SNSR CLK ANGLE\t\t%8.3f",ds->clock_ang);
  add(&ret, "\n INCIDENCE ANGLE\t\t%8.3f",ds->incident_ang);
  add(&ret, "\n RADAR FREQUENCY\t%8.3f",ds->frequency);
  add(&ret, "\n RDR WAVELENGTH\t\t%16.7f",ds->wave_length);
  add(&ret, "\n MOTION COMP IND\t%s",ds->motion_comp);
  add(&ret, "\n RNG PULSE CODE\t\t%s",ds->pulse_code);
  add(&ret, "\n RNG CHIRP 1\t\t%16.7E",ds->ampl_coef[0]);
  add(&ret, "\n RNG CHIRP 2\t\t%16.7E",ds->ampl_coef[1]);
  add(&ret, "\n RNG CHIRP 3\t\t%16.7E",ds->ampl_coef[2]);
  add(&ret, "\n RNG CHIRP 4\t\t%16.7E",ds->ampl_coef[3]);
  add(&ret, "\n RNG CHIRP 5\t\t%16.7E",ds->ampl_coef[4]);
  add(&ret, "\n RNG PHASE 1\t\t%16.7E",ds->phas_coef[0]);
  add(&ret, "\n RNG PHASE 2\t\t%16.7E",ds->phas_coef[1]);
  add(&ret, "\n RNG PHASE 3\t\t%16.7E",ds->phas_coef[2]);
  add(&ret, "\n RNG PHASE 4\t\t%16.7E",ds->phas_coef[3]);
  add(&ret, "\n RNG PHASE 5\t\t%16.7E",ds->phas_coef[4]);
  add(&ret, "\n CHRP EXTRACTION IND\t%d",ds->chirp_ext_ind);
  add(&ret, "\n RNG CMPLX SAMPLE RATE\t%16.7f",ds->rng_samp_rate);
  add(&ret, "\n RNG GATE\t\t%16.7f",ds->rng_gate);
  add(&ret, "\n RNG PULSE LEN\t\t%16.7f",ds->rng_length);
  add(&ret, "\n BASEBAND FLAG\t\t%s",ds->baseband_f);
  add(&ret, "\n RNG COMPRESS FLAG\t%s",ds->rngcmp_f);
  add(&ret, "\n RCVR GAIN POLAR\t%16.7f",ds->gn_polar);
  add(&ret, "\n RCVR GAIN CROSS\t%16.7f",ds->gn_cross);
  add(&ret, "\n QUANT BITS/CHNL\t%d",ds->chn_bits);
  add(&ret, "\n QUANTZR DESCRPT\t%s",ds->quant_desc);
  add(&ret, "\n I CHNL DC BIAS\t\t%16.7f",ds->i_bias);
  add(&ret, "\n Q CHNL DC BIAS\t\t%16.7f",ds->q_bias);
  add(&ret, "\n I/Q CHNL RATIO\t\t%16.7f",ds->iq_ratio);
  add(&ret, "\n SPARE_DSS_7\t\t\t%16.7f",ds->spare_dss_7);
  add(&ret, "\n SPARE_DSS_8\t\t\t%16.7f",ds->spare_dss_8);
  add(&ret, "\n ELCTRNC BORESITE\t%16.7f",ds->ele_sight);
  add(&ret, "\n MECHNCL BORESITE\t%16.7f",ds->mech_sight);
  add(&ret, "\n ECHO TRK FLAG\t\t%s",ds->echo_track);
  add(&ret, "\n NOMINAL PRF\t\t%16.7f",ds->prf);
  add(&ret, "\n ANT ELEV BEAM WD\t%16.7f",ds->elev_beam);
  add(&ret, "\n ANT AZI BEAM WD\t%16.7f",ds->azi_beam);
  add(&ret, "\n SATLT BINARY TIME\t%s",ds->sat_bintim);
  add(&ret, "\n SATLT CLOCK TIME\t%s",ds->sat_clktim);
  add(&ret, "\n SATLT CLOCK INC\t%d",ds->sat_clkinc);
  add(&ret, "\n PROCESSING FACILITY\t%s",ds->fac_id);
  add(&ret, "\n PROCESSING SYSTEM\t%s",ds->sys_id);
  add(&ret, "\n PROCESSING VERSION\t%s",ds->ver_id);
  add(&ret, "\n FAC PROCESS CODE\t%s",ds->fac_code);
  add(&ret, "\n PRODUCT CODE\t\t%s",ds->lev_code);
  add(&ret, "\n PRODUCT TYPE\t\t%s",ds->product_type);
  add(&ret, "\n PROCESSING ALGTHM\t%s",ds->algor_id);
  add(&ret, "\n NUM LOOKS IN AZI\t%16.7f",ds->n_azilok);
  add(&ret, "\n NUM LOOKS IN RNG\t%16.7f",ds->n_rnglok);
  add(&ret, "\n BNDWDTH/LOOK IN AZI\t%16.7f",ds->bnd_azilok);
  add(&ret, "\n BNDWDTH/LOOK IN RNG\t%16.7f",ds->bnd_rnglok);
  add(&ret, "\n PROC BNDWDTH AZI\t%16.7f",ds->bnd_azi);
  add(&ret, "\n PROC BNDWDTH RNG\t%16.7f",ds->bnd_rng);
  add(&ret, "\n AZI WEIGHT FUNC\t%s",ds->azi_weight);
  add(&ret, "\n RNG WEIGHT FUNC\t%s",ds->rng_weight);
  add(&ret, "\n DATA INPUT SRC\t\t%s",ds->data_inpsrc);
  add(&ret, "\n NOM RESOLUTION RNG\t%16.7f",ds->rng_res);
  add(&ret, "\n NOM RESOLUTION AZI\t%16.7f",ds->azi_res);
  add(&ret, "\n RADIO STRETCH BIAS\t%16.7f",ds->radi_stretch[0]);
  add(&ret, "\n RADIO STRETCH GAIN\t%16.7f",ds->radi_stretch[1]);
  add(&ret, "\n ALT DOPPLER FREQ 1\t%16.7f",ds->alt_dopcen[0]);
  add(&ret, "\n ALT DOPPLER FREQ 2\t%16.7f",ds->alt_dopcen[1]);
  add(&ret, "\n ALT DOPPLER FREQ 3\t%16.7f",ds->alt_dopcen[2]);
  add(&ret, "\n CRT DOPPLER FREQ 1\t%16.7f",ds->crt_dopcen[0]);
  add(&ret, "\n CRT DOPPLER FREQ 2\t%16.7f",ds->crt_dopcen[1]);
  add(&ret, "\n CRT DOPPLER FREQ 3\t%16.7f",ds->crt_dopcen[2]);
  add(&ret, "\n TIME DIRECT RNG\t%s",ds->time_dir_pix);
  add(&ret, "\n TIME DIRECT AZI\t%s",ds->time_dir_lin);
  add(&ret, "\n ALT DOPPLER RATE 1\t%16.7f",ds->alt_rate[0]);
  add(&ret, "\n ALT DOPPLER RATE 2\t%16.7f",ds->alt_rate[1]);
  add(&ret, "\n ALT DOPPLER RATE 3\t%16.7f",ds->alt_rate[2]);
  add(&ret, "\n CRT DOPPLER RATE 1\t%16.7f",ds->crt_rate[0]);
  add(&ret, "\n CRT DOPPLER RATE 2\t%16.7f",ds->crt_rate[1]);
  add(&ret, "\n CRT DOPPLER RATE 3\t%16.7f",ds->crt_rate[2]);
  add(&ret, "\n LINE CONTENT IND\t%s",ds->line_cont);
  add(&ret, "\n CLUTTER LOCK FLAG\t%s",ds->clutterlock_flg);
  add(&ret, "\n AUTOFOCUS FLAG\t\t%s",ds->auto_focus);
  add(&ret, "\n LINE SPACING\t\t%16.7f",ds->line_spacing);
  add(&ret, "\n PIXEL SPACING\t\t%16.7f",ds->pixel_spacing);
  add(&ret, "\n RNG COMPRESS DESG\t%s",ds->rngcmp_desg);
  if (strncmp(ds->fac_id, "ES", 2)==0 || 
      strncmp(ds->fac_id, "D-PAF", 5)==0 ||
      strncmp(ds->fac_id, "I-PAF", 5)==0) {
    for (i=0; i<3; i++) 
      add(&ret, "\n Zero-Doppler range time (%d)\t%16.7f", i, ds->rng_time[i]);
    add(&ret, "\n Zero-Doppler azimuth time first pixel\t%s", ds->az_time_first);
    add(&ret, "\n Zero-Doppler azimuth time center pixel\t%s", 
	    ds->az_time_center);
    add(&ret, "\n Zero-Doppler azimuth time last pixel\t%s", ds->az_time_last);
  }
  else if (strncmp(ds->fac_id, "ASF", 3)==0) {
    if (era==0)
      {
	add(&ret, "\n NUM ANNOTATION PTS\t%d",ds->annot_pts);
	for (i=0; i<ds->annot_pts; i++) {
	  add(&ret, "\n LINE NUM ANNOT START\t\t%d",ds->annot_line[i]);
	  add(&ret, "\n PIXEL NUM ANNOT START\t\t%d",ds->annot_pixel[i]);
	  add(&ret, "\n ANNOTATION TEXT\t\t%d",ds->annot_text[i][0]);
	}
      }
    else
      {
	add(&ret, "\n NUM OF BEAMS\t%d",ds->no_beams);
	add(&ret, "\n BEAM 1 IDENTIFIER\t%s",ds->beam1);
	add(&ret, "\n BEAM 2 IDENTIFIER\t%s",ds->beam2);
	add(&ret, "\n BEAM 3 IDENTIFIER\t%s",ds->beam3);
	add(&ret, "\n BEAM 4 IDENTIFIER\t%s",ds->beam4);
	add(&ret, "\n PRF OF BEAM 1 HZ\t%8.3f",ds->prf1);
	add(&ret, "\n PRF OF BEAM 2 HZ\t%8.3f",ds->prf2);
	add(&ret, "\n PRF OF BEAM 3 HZ\t%8.3f",ds->prf3);
	add(&ret, "\n PRF OF BEAM 4 HZ\t%8.3f",ds->prf4);
	add(&ret, "\n RANGE GATE OF BEAM 1\t%8.3f",ds->rng_gate1);
	add(&ret, "\n RANGE GATE OF BEAM 2\t%8.3f",ds->rng_gate2);
	add(&ret, "\n RANGE GATE OF BEAM 3\t%8.3f",ds->rng_gate3);
	add(&ret, "\n RANGE GATE OF BEAM 4\t%8.3f",ds->rng_gate4);
	add(&ret, "\n TOTAL PULSES PER BURST\t%d",ds->tot_pls_burst);
	add(&ret, "\n VALID PULSES PER BURST\t%d",ds->val_pls_burst);
	add(&ret, "\n RANGE LNS OVERLAP IN AZI WITH NEXT\t%d",
		ds->az_ovlp_nxt_img);
	add(&ret, "\n PIXEL OFFSET IN RANGE WITH NEXT\t%d",ds->rg_off_nxt_img);
	add(&ret, "\n CALIBRATION PARAMETER FILE USED\t%s",ds->cal_params_file);
	add(&ret, "\n SCAN RESULTS FILE USED         \t%s",ds->scan_results_file);
	add(&ret, "\n SOFTWARE VERSION OF SCANNER    \t%s",ds->scanner_version);
	add(&ret, "\n SOFTWARE VERSION OF DECODER    \t%s",ds->decode_version);
      }
  }
  add(&ret, "\n*********** end of Dataset Summary  record ********************\n");
  return ret;
}

void prn_dssr(FILE *fp, struct dataset_sum_rec *ds, int era)
{
    char *rec = sprn_dssr(ds, era);
    fprintf(fp, "%s", rec);
    FREE(rec);
}
