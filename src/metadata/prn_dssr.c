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

void prn_dssr(FILE *fp, struct dataset_sum_rec *ds, int era)
{
  int i;
  fprintf(fp, "\n*********** begin of Dataset Summary record *****************\n");
  fprintf(fp, "\n DSS SEQ NUM\t\t%d",ds->seq_num);
  fprintf(fp, "\n SAR CHNL INDTR\t\t%d",ds->sar_chan);
  fprintf(fp, "\n SCENE INDICATOR\t%s",ds->product_id);
  fprintf(fp, "\n SCENE DESIGNATOR\t%s",ds->scene_des);
  fprintf(fp, "\n INPT SCN CTR TIME\t%s",ds->inp_sctim);
  fprintf(fp, "\n ASC/DESCENDING\t\t%s",ds->asc_des);
  fprintf(fp, "\n LAT @ SCN CTR\t\t%16.7f",ds->pro_lat);
  fprintf(fp, "\n LONG @ SCN CTR\t\t%16.7f",ds->pro_long);
  fprintf(fp, "\n SCN CTR HEADING\t%16.7f",ds->pro_head);
  fprintf(fp, "\n ELLIP DESIGNATOR\t%s",ds->ellip_des);
  fprintf(fp, "\n ELLIP SEMIMAJOR\t%16.7f",ds->ellip_maj);
  fprintf(fp, "\n ELLIP SEMIMINOR\t%16.7f",ds->ellip_min);
  fprintf(fp, "\n EARTH MASS\t\t%16.7f",ds->earth_mass);
  fprintf(fp, "\n GRAVITATIONAL CNST\t%16.7f",ds->grav_const);
  fprintf(fp, "\n ELLIP PARM 1\t\t%16.7f",ds->ellip_j[0]);
  fprintf(fp, "\n ELLIP PARM 2\t\t%16.7f",ds->ellip_j[1]);
  fprintf(fp, "\n ELLIP PARM 3\t\t%16.7f",ds->ellip_j[2]);
  fprintf(fp, "\n AVG TERRAIN HT\t\t%16.7f",ds->terrain_h);
  fprintf(fp, "\n IMG CTR LINE NUM\t%f",ds->sc_lin);
  fprintf(fp, "\n IMG CTR PIX NUM\t%f",ds->sc_pix);
  fprintf(fp, "\n IMAGE LENGTH\t\t%16.7f",ds->scene_len);
  fprintf(fp, "\n IMAGE WIDTH\t\t%16.7f",ds->scene_wid);
  fprintf(fp, "\n NUM SAR CHANNELS\t%d",ds->nchn);
  fprintf(fp, "\n MISSION ID\t\t%s",ds->mission_id);
  fprintf(fp, "\n SENSOR ID\t\t%s",ds->sensor_id);
  fprintf(fp, "\n ORBIT NUMBER\t\t%s",ds->revolution);
  fprintf(fp, "\n PLAT LAT @ NADIR\t%8.3f",ds->plat_lat);
  fprintf(fp, "\n PLAT LONG @ NADIR\t%8.3f",ds->plat_long);
  fprintf(fp, "\n PLAT HEADING\t\t%8.3f",ds->plat_head_scene);
  fprintf(fp, "\n SNSR CLK ANGLE\t\t%8.3f",ds->clock_ang);
  fprintf(fp, "\n INCIDENCE ANGLE\t\t%8.3f",ds->incident_ang);
  fprintf(fp, "\n RADAR FREQUENCY\t%8.3f",ds->frequency);
  fprintf(fp, "\n RDR WAVELENGTH\t\t%16.7f",ds->wave_length);
  fprintf(fp, "\n MOTION COMP IND\t%s",ds->motion_comp);
  fprintf(fp, "\n RNG PULSE CODE\t\t%s",ds->pulse_code);
  fprintf(fp, "\n RNG CHIRP 1\t\t%16.7E",ds->ampl_coef[0]);
  fprintf(fp, "\n RNG CHIRP 2\t\t%16.7E",ds->ampl_coef[1]);
  fprintf(fp, "\n RNG CHIRP 3\t\t%16.7E",ds->ampl_coef[2]);
  fprintf(fp, "\n RNG CHIRP 4\t\t%16.7E",ds->ampl_coef[3]);
  fprintf(fp, "\n RNG CHIRP 5\t\t%16.7E",ds->ampl_coef[4]);
  fprintf(fp, "\n RNG PHASE 1\t\t%16.7E",ds->phas_coef[0]);
  fprintf(fp, "\n RNG PHASE 2\t\t%16.7E",ds->phas_coef[1]);
  fprintf(fp, "\n RNG PHASE 3\t\t%16.7E",ds->phas_coef[2]);
  fprintf(fp, "\n RNG PHASE 4\t\t%16.7E",ds->phas_coef[3]);
  fprintf(fp, "\n RNG PHASE 5\t\t%16.7E",ds->phas_coef[4]);
  fprintf(fp, "\n CHRP EXTRACTION IND\t%d",ds->chirp_ext_ind);
  fprintf(fp, "\n RNG CMPLX SAMPLE RATE\t%16.7f",ds->rng_samp_rate);
  fprintf(fp, "\n RNG GATE\t\t%16.7f",ds->rng_gate);
  fprintf(fp, "\n RNG PULSE LEN\t\t%16.7f",ds->rng_length);
  fprintf(fp, "\n BASEBAND FLAG\t\t%s",ds->baseband_f);
  fprintf(fp, "\n RNG COMPRESS FLAG\t%s",ds->rngcmp_f);
  fprintf(fp, "\n RCVR GAIN POLAR\t%16.7f",ds->gn_polar);
  fprintf(fp, "\n RCVR GAIN CROSS\t%16.7f",ds->gn_cross);
  fprintf(fp, "\n QUANT BITS/CHNL\t%d",ds->chn_bits);
  fprintf(fp, "\n QUANTZR DESCRPT\t%s",ds->quant_desc);
  fprintf(fp, "\n I CHNL DC BIAS\t\t%16.7f",ds->i_bias);
  fprintf(fp, "\n Q CHNL DC BIAS\t\t%16.7f",ds->q_bias);
  fprintf(fp, "\n I/Q CHNL RATIO\t\t%16.7f",ds->iq_ratio);
  fprintf(fp, "\n SPARE_DSS_7\t\t\t%16.7f",ds->spare_dss_7);
  fprintf(fp, "\n SPARE_DSS_8\t\t\t%16.7f",ds->spare_dss_8);
  fprintf(fp, "\n ELCTRNC BORESITE\t%16.7f",ds->ele_sight);
  fprintf(fp, "\n MECHNCL BORESITE\t%16.7f",ds->mech_sight);
  fprintf(fp, "\n ECHO TRK FLAG\t\t%s",ds->echo_track);
  fprintf(fp, "\n NOMINAL PRF\t\t%16.7f",ds->prf);
  fprintf(fp, "\n ANT ELEV BEAM WD\t%16.7f",ds->elev_beam);
  fprintf(fp, "\n ANT AZI BEAM WD\t%16.7f",ds->azi_beam);
  fprintf(fp, "\n SATLT BINARY TIME\t%s",ds->sat_bintim);
  fprintf(fp, "\n SATLT CLOCK TIME\t%s",ds->sat_clktim);
  fprintf(fp, "\n SATLT CLOCK INC\t%d",ds->sat_clkinc);
  fprintf(fp, "\n PROCESSING FACILITY\t%s",ds->fac_id);
  fprintf(fp, "\n PROCESSING SYSTEM\t%s",ds->sys_id);
  fprintf(fp, "\n PROCESSING VERSION\t%s",ds->ver_id);
  fprintf(fp, "\n FAC PROCESS CODE\t%s",ds->fac_code);
  fprintf(fp, "\n PRODUCT CODE\t\t%s",ds->lev_code);
  fprintf(fp, "\n PRODUCT TYPE\t\t%s",ds->product_type);
  fprintf(fp, "\n PROCESSING ALGTHM\t%s",ds->algor_id);
  fprintf(fp, "\n NUM LOOKS IN AZI\t%16.7f",ds->n_azilok);
  fprintf(fp, "\n NUM LOOKS IN RNG\t%16.7f",ds->n_rnglok);
  fprintf(fp, "\n BNDWDTH/LOOK IN AZI\t%16.7f",ds->bnd_azilok);
  fprintf(fp, "\n BNDWDTH/LOOK IN RNG\t%16.7f",ds->bnd_rnglok);
  fprintf(fp, "\n PROC BNDWDTH AZI\t%16.7f",ds->bnd_azi);
  fprintf(fp, "\n PROC BNDWDTH RNG\t%16.7f",ds->bnd_rng);
  fprintf(fp, "\n AZI WEIGHT FUNC\t%s",ds->azi_weight);
  fprintf(fp, "\n RNG WEIGHT FUNC\t%s",ds->rng_weight);
  fprintf(fp, "\n DATA INPUT SRC\t\t%s",ds->data_inpsrc);
  fprintf(fp, "\n NOM RESOLUTION RNG\t%16.7f",ds->rng_res);
  fprintf(fp, "\n NOM RESOLUTION AZI\t%16.7f",ds->azi_res);
  fprintf(fp, "\n RADIO STRETCH BIAS\t%16.7f",ds->radi_stretch[0]);
  fprintf(fp, "\n RADIO STRETCH GAIN\t%16.7f",ds->radi_stretch[1]);
  fprintf(fp, "\n ALT DOPPLER FREQ 1\t%16.7f",ds->alt_dopcen[0]);
  fprintf(fp, "\n ALT DOPPLER FREQ 2\t%16.7f",ds->alt_dopcen[1]);
  fprintf(fp, "\n ALT DOPPLER FREQ 3\t%16.7f",ds->alt_dopcen[2]);
  fprintf(fp, "\n CRT DOPPLER FREQ 1\t%16.7f",ds->crt_dopcen[0]);
  fprintf(fp, "\n CRT DOPPLER FREQ 2\t%16.7f",ds->crt_dopcen[1]);
  fprintf(fp, "\n CRT DOPPLER FREQ 3\t%16.7f",ds->crt_dopcen[2]);
  fprintf(fp, "\n TIME DIRECT RNG\t%s",ds->time_dir_pix);
  fprintf(fp, "\n TIME DIRECT AZI\t%s",ds->time_dir_lin);
  fprintf(fp, "\n ALT DOPPLER RATE 1\t%16.7f",ds->alt_rate[0]);
  fprintf(fp, "\n ALT DOPPLER RATE 2\t%16.7f",ds->alt_rate[1]);
  fprintf(fp, "\n ALT DOPPLER RATE 3\t%16.7f",ds->alt_rate[2]);
  fprintf(fp, "\n CRT DOPPLER RATE 1\t%16.7f",ds->crt_rate[0]);
  fprintf(fp, "\n CRT DOPPLER RATE 2\t%16.7f",ds->crt_rate[1]);
  fprintf(fp, "\n CRT DOPPLER RATE 3\t%16.7f",ds->crt_rate[2]);
  fprintf(fp, "\n LINE CONTENT IND\t%s",ds->line_cont);
  fprintf(fp, "\n CLUTTER LOCK FLAG\t%s",ds->clutterlock_flg);
  fprintf(fp, "\n AUTOFOCUS FLAG\t\t%s",ds->auto_focus);
  fprintf(fp, "\n LINE SPACING\t\t%16.7f",ds->line_spacing);
  fprintf(fp, "\n PIXEL SPACING\t\t%16.7f",ds->pixel_spacing);
  fprintf(fp, "\n RNG COMPRESS DESG\t%s",ds->rngcmp_desg);
  if (strncmp(ds->fac_id, "ES", 2)==0 || 
      strncmp(ds->fac_id, "D-PAF", 5)==0 ||
      strncmp(ds->fac_id, "I-PAF", 5)==0) {
    int i;
    for (i=0; i<3; i++) 
      fprintf(fp, "\n Zero-Doppler range time (%d)\t%16.7f", i, ds->rng_time[i]);
    fprintf(fp, "\n Zero-Doppler azimuth time first pixel\t%s", ds->az_time_first);
    fprintf(fp, "\n Zero-Doppler azimuth time center pixel\t%s", 
	    ds->az_time_center);
    fprintf(fp, "\n Zero-Doppler azimuth time last pixel\t%s", ds->az_time_last);
  }

  else if (strncmp(ds->mission_id, "ALOS", 4)==0) {
    fprintf(fp, "\n Calibration data indicator\t%d", ds->cal_data_indicator);
    fprintf(fp, "\n Start line number of calibration at upper image\t%d",
	    ds->start_cal_up);
    fprintf(fp, "\n Stop line number of calibration at upper image\t%d", 
	    ds->stop_cal_up);
    fprintf(fp, "\n Start line number of calibration at bottom image\t%d", 
	    ds->start_cal_bottom);
    fprintf(fp, "\n Stop line number of calibration at bottom image\t%d",
	    ds->stop_cal_bottom);
    fprintf(fp, "\n PRF switching indicator\t%d", ds->prf_switch);
    fprintf(fp, "\n Line locator of PRF switching\t%d", ds->line_prf_switch);
    fprintf(fp, "\n Direction of a beam center in a scene scenter\t%16.7f", 
	    ds->beam_center_dir);
    fprintf(fp, "\n Yaw steering mode flag\t%d", ds->yaw_steering);
    fprintf(fp, "\n Parameter table number of automatically setting\t%d", 
	    ds->param_table);
    fprintf(fp, "\n Nominal offnadir angle\t%16.7f", ds->off_nadir_angle);
    fprintf(fp, "\n Antenna beam number\t%d", ds->ant_beam_num);
    int i;
    for (i=0; i<6; i++)
      fprintf(fp, "\n Incidence angle parameter (a%d)\t%16.7f", i, ds->incid_a[i]);
  }
  else if (strncmp(ds->fac_id, "ASF", 3)==0) {
    if (era==0)
      {
	fprintf(fp, "\n NUM ANNOTATION PTS\t%d",ds->annot_pts);
	for (i=0; i<ds->annot_pts; i++) {
	  fprintf(fp, "\n LINE NUM ANNOT START\t\t%d",ds->annot_line[i]);
	  fprintf(fp, "\n PIXEL NUM ANNOT START\t\t%d",ds->annot_pixel[i]);
	  fprintf(fp, "\n ANNOTATION TEXT\t\t%d",ds->annot_text[i][0]);
	}
      }
    else
      {
	fprintf(fp, "\n NUM OF BEAMS\t%d",ds->no_beams);
	fprintf(fp, "\n BEAM 1 IDENTIFIER\t%s",ds->beam1);
	fprintf(fp, "\n BEAM 2 IDENTIFIER\t%s",ds->beam2);
	fprintf(fp, "\n BEAM 3 IDENTIFIER\t%s",ds->beam3);
	fprintf(fp, "\n BEAM 4 IDENTIFIER\t%s",ds->beam4);
	fprintf(fp, "\n PRF OF BEAM 1 HZ\t%8.3f",ds->prf1);
	fprintf(fp, "\n PRF OF BEAM 2 HZ\t%8.3f",ds->prf2);
	fprintf(fp, "\n PRF OF BEAM 3 HZ\t%8.3f",ds->prf3);
	fprintf(fp, "\n PRF OF BEAM 4 HZ\t%8.3f",ds->prf4);
	fprintf(fp, "\n RANGE GATE OF BEAM 1\t%8.3f",ds->rng_gate1);
	fprintf(fp, "\n RANGE GATE OF BEAM 2\t%8.3f",ds->rng_gate2);
	fprintf(fp, "\n RANGE GATE OF BEAM 3\t%8.3f",ds->rng_gate3);
	fprintf(fp, "\n RANGE GATE OF BEAM 4\t%8.3f",ds->rng_gate4);
	fprintf(fp, "\n TOTAL PULSES PER BURST\t%d",ds->tot_pls_burst);
	fprintf(fp, "\n VALID PULSES PER BURST\t%d",ds->val_pls_burst);
	fprintf(fp, "\n RANGE LNS OVERLAP IN AZI WITH NEXT\t%d",
		ds->az_ovlp_nxt_img);
	fprintf(fp, "\n PIXEL OFFSET IN RANGE WITH NEXT\t%d",ds->rg_off_nxt_img);
	fprintf(fp, "\n CALIBRATION PARAMETER FILE USED\t%s",ds->cal_params_file);
	fprintf(fp, "\n SCAN RESULTS FILE USED         \t%s",ds->scan_results_file);
	fprintf(fp, "\n SOFTWARE VERSION OF SCANNER    \t%s",ds->scanner_version);
	fprintf(fp, "\n SOFTWARE VERSION OF DECODER    \t%s",ds->decode_version);
      }
  }
  fprintf(fp, "\n*********** end of Dataset Summary  record ********************\n");
  return;
 }
