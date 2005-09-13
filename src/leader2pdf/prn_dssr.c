#include "asf.h"
#include "ceos.h"

void prn_dssr(char *file, struct dataset_sum_rec *ds, int era)
{
  FILE *fp;
  int i;

  fp = FOPEN(file, "w");
  fprintf(fp, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n");
  fprintf(fp, "\"http://www.w3.org/TR/html4/loose.dtd\">\n<html>\n<head>\n");
  fprintf(fp, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\n");
  fprintf(fp, "<title>Data Set Summary record</title>\n</head>\n<body>\n");
  fprintf(fp, "<h2>Data Set Summary record</h2>\n");
  fprintf(fp, "<strong>DSS SEQ NUM: </strong>%d<br>\n",ds->seq_num);
  fprintf(fp, "<strong>SAR CHNL INDTR: </strong>%d<br>\n",ds->sar_chan);
  fprintf(fp, "<strong>SCENE INDICATOR: </strong>%s<br>\n",ds->product_id);
  fprintf(fp, "<strong>SCENE DESIGNATOR: </strong>%s<br>\n",ds->scene_des);
  fprintf(fp, "<strong>INPT SCN CTR TIME: </strong>%s<br>\n",ds->inp_sctim);
  fprintf(fp, "<strong>ASC/DESCENDING: </strong>%s<br>\n",ds->asc_des);
  fprintf(fp, "<strong>LAT @ SCN CTR: </strong>%16.7f<br>\n",ds->pro_lat);
  fprintf(fp, "<strong>LONG @ SCN CTR: </strong>%16.7f<br>\n",ds->pro_long);
  fprintf(fp, "<strong>SCN CTR HEADING: </strong>%16.7f<br>\n",ds->pro_head);
  fprintf(fp, "<strong>ELLIP DESIGNATOR: </strong>%s<br>\n",ds->ellip_des);
  fprintf(fp, "<strong>ELLIP SEMIMAJOR: </strong>%16.7f<br>\n",ds->ellip_maj);
  fprintf(fp, "<strong>ELLIP SEMIMINOR: </strong>%16.7f<br>\n",ds->ellip_min);
  fprintf(fp, "<strong>EARTH MASS: </strong>%16.7f<br>\n",ds->earth_mass);
  fprintf(fp, "<strong>GRAVITATIONAL CNST: </strong>%16.7f<br>\n",ds->grav_const);
  fprintf(fp, "<strong>ELLIP PARM 1: </strong>%16.7f<br>\n",ds->ellip_j[0]);
  fprintf(fp, "<strong>ELLIP PARM 2: </strong>%16.7f<br>\n",ds->ellip_j[1]);
  fprintf(fp, "<strong>ELLIP PARM 3: </strong>%16.7f<br>\n",ds->ellip_j[2]);
  fprintf(fp, "<strong>AVG TERRAIN HT: </strong>%16.7f<br>\n",ds->terrain_h);
  fprintf(fp, "<strong>IMG CTR LINE NUM: </strong>%f<br>\n",ds->sc_lin);
  fprintf(fp, "<strong>IMG CTR PIX NUM: </strong>%f<br>\n",ds->sc_pix);
  fprintf(fp, "<strong>IMAGE LENGTH: </strong>%16.7f<br>\n",ds->scene_len);
  fprintf(fp, "<strong>IMAGE WIDTH: </strong>%16.7f<br>\n",ds->scene_wid);
  fprintf(fp, "<strong>NUM SAR CHANNELS: </strong>%d<br>\n",ds->nchn);
  fprintf(fp, "<strong>MISSION ID: </strong>%s<br>\n",ds->mission_id);
  fprintf(fp, "<strong>SENSOR ID: </strong>%s<br>\n",ds->sensor_id);
  fprintf(fp, "<strong>ORBIT NUMBER: </strong>%s<br>\n",ds->revolution);
  fprintf(fp, "<strong>PLAT LAT @ NADIR: </strong>%8.3f<br>\n",ds->plat_lat);
  fprintf(fp, "<strong>PLAT LONG @ NADIR: </strong>%8.3f<br>\n",ds->plat_long);
  fprintf(fp, "<strong>PLAT HEADING: </strong>%8.3f<br>\n",ds->plat_head_scene);
  fprintf(fp, "<strong>SNSR CLK ANGLE: </strong>%8.3f<br>\n",ds->clock_ang);
  fprintf(fp, "<strong>INCIDENCE ANGLE: </strong>%8.3f<br>\n",ds->incident_ang);
  fprintf(fp, "<strong>RADAR FREQUENCY: </strong>%8.3f<br>\n",ds->frequency);
  fprintf(fp, "<strong>RDR WAVELENGTH: </strong>%16.7f<br>\n",ds->wave_length);
  fprintf(fp, "<strong>MOTION COMP IND: </strong>%s<br>\n",ds->motion_comp);
  fprintf(fp, "<strong>RNG PULSE CODE: </strong>%s<br>\n",ds->pulse_code);
  fprintf(fp, "<strong>RNG CHIRP 1: </strong>%16.7E<br>\n",ds->ampl_coef[0]);
  fprintf(fp, "<strong>RNG CHIRP 2: </strong>%16.7E<br>\n",ds->ampl_coef[1]);
  fprintf(fp, "<strong>RNG CHIRP 3: </strong>%16.7E<br>\n",ds->ampl_coef[2]);
  fprintf(fp, "<strong>RNG CHIRP 4: </strong>%16.7E<br>\n",ds->ampl_coef[3]);
  fprintf(fp, "<strong>RNG CHIRP 5: </strong>%16.7E<br>\n",ds->ampl_coef[4]);
  fprintf(fp, "<strong>RNG PHASE 1: </strong>%16.7E<br>\n",ds->phas_coef[0]);
  fprintf(fp, "<strong>RNG PHASE 2: </strong>%16.7E<br>\n",ds->phas_coef[1]);
  fprintf(fp, "<strong>RNG PHASE 3: </strong>%16.7E<br>\n",ds->phas_coef[2]);
  fprintf(fp, "<strong>RNG PHASE 4: </strong>%16.7E<br>\n",ds->phas_coef[3]);
  fprintf(fp, "<strong>RNG PHASE 5: </strong>%16.7E<br>\n",ds->phas_coef[4]);
  fprintf(fp, "<strong>CHRP EXTRACTION IND: </strong>%d<br>\n",ds->chirp_ext_ind);
  fprintf(fp, "<strong>RNG CMPLX SAMPLE RATE: </strong>%16.7f<br>\n",ds->rng_samp_rate);
  fprintf(fp, "<strong>RNG GATE: </strong>%16.7f<br>\n",ds->rng_gate);
  fprintf(fp, "<strong>RNG PULSE LEN: </strong>%16.7f<br>\n",ds->rng_length);
  fprintf(fp, "<strong>BASEBAND FLAG: </strong>%s<br>\n",ds->baseband_f);
  fprintf(fp, "<strong>RNG COMPRESS FLAG: </strong>%s<br>\n",ds->rngcmp_f);
  fprintf(fp, "<strong>RCVR GAIN POLAR: </strong>%16.7f<br>\n",ds->gn_polar);
  fprintf(fp, "<strong>RCVR GAIN CROSS: </strong>%16.7f<br>\n",ds->gn_cross);
  fprintf(fp, "<strong>QUANT BITS/CHNL: </strong>%d<br>\n",ds->chn_bits);
  fprintf(fp, "<strong>QUANTZR DESCRPT: </strong>%s<br>\n",ds->quant_desc);
  fprintf(fp, "<strong>I CHNL DC BIAS: </strong>%16.7f<br>\n",ds->i_bias);
  fprintf(fp, "<strong>Q CHNL DC BIAS: </strong>%16.7f<br>\n",ds->q_bias);
  fprintf(fp, "<strong>I/Q CHNL RATIO: </strong>%16.7f<br>\n",ds->iq_ratio);
  fprintf(fp, "<strong>SPARE_DSS_7: </strong>: </strong>%16.7f<br>\n",ds->spare_dss_7);
  fprintf(fp, "<strong>SPARE_DSS_8: </strong>: </strong>%16.7f<br>\n",ds->spare_dss_8);
  fprintf(fp, "<strong>ELCTRNC BORESITE: </strong>%16.7f<br>\n",ds->ele_sight);
  fprintf(fp, "<strong>MECHNCL BORESITE: </strong>%16.7f<br>\n",ds->mech_sight);
  fprintf(fp, "<strong>ECHO TRK FLAG: </strong>%s<br>\n",ds->echo_track);
  fprintf(fp, "<strong>NOMINAL PRF: </strong>%16.7f<br>\n",ds->prf);
  fprintf(fp, "<strong>ANT ELEV BEAM WD: </strong>%16.7f<br>\n",ds->elev_beam);
  fprintf(fp, "<strong>ANT AZI BEAM WD: </strong>%16.7f<br>\n",ds->azi_beam);
  fprintf(fp, "<strong>SATLT BINARY TIME: </strong>%s<br>\n",ds->sat_bintim);
  fprintf(fp, "<strong>SATLT CLOCK TIME: </strong>%s<br>\n",ds->sat_clktim);
  fprintf(fp, "<strong>SATLT CLOCK INC: </strong>%d<br>\n",ds->sat_clkinc);
  fprintf(fp, "<strong>PROCESSING FACILITY: </strong>%s<br>\n",ds->fac_id);
  fprintf(fp, "<strong>PROCESSING SYSTEM: </strong>%s<br>\n",ds->sys_id);
  fprintf(fp, "<strong>PROCESSING VERSION: </strong>%s<br>\n",ds->ver_id);
  fprintf(fp, "<strong>FAC PROCESS CODE: </strong>%s<br>\n",ds->fac_code);
  fprintf(fp, "<strong>PRODUCT CODE: </strong>%s<br>\n",ds->lev_code);
  fprintf(fp, "<strong>PRODUCT TYPE: </strong>%s<br>\n",ds->product_type);
  fprintf(fp, "<strong>PROCESSING ALGTHM: </strong>%s<br>\n",ds->algor_id);
  fprintf(fp, "<strong>NUM LOOKS IN AZI: </strong>%16.7f<br>\n",ds->n_azilok);
  fprintf(fp, "<strong>NUM LOOKS IN RNG: </strong>%16.7f<br>\n",ds->n_rnglok);
  fprintf(fp, "<strong>BNDWDTH/LOOK IN AZI: </strong>%16.7f<br>\n",ds->bnd_azilok);
  fprintf(fp, "<strong>BNDWDTH/LOOK IN RNG: </strong>%16.7f<br>\n",ds->bnd_rnglok);
  fprintf(fp, "<strong>PROC BNDWDTH AZI: </strong>%16.7f<br>\n",ds->bnd_azi);
  fprintf(fp, "<strong>PROC BNDWDTH RNG: </strong>%16.7f<br>\n",ds->bnd_rng);
  fprintf(fp, "<strong>AZI WEIGHT FUNC: </strong>%s<br>\n",ds->azi_weight);
  fprintf(fp, "<strong>RNG WEIGHT FUNC: </strong>%s<br>\n",ds->rng_weight);
  fprintf(fp, "<strong>DATA INPUT SRC: </strong>%s<br>\n",ds->data_inpsrc);
  fprintf(fp, "<strong>NOM RESOLUTION RNG: </strong>%16.7f<br>\n",ds->rng_res);
  fprintf(fp, "<strong>NOM RESOLUTION AZI: </strong>%16.7f<br>\n",ds->azi_res);
  fprintf(fp, "<strong>RADIO STRETCH BIAS: </strong>%16.7f<br>\n",ds->radi_stretch[0]);
  fprintf(fp, "<strong>RADIO STRETCH GAIN: </strong>%16.7f<br>\n",ds->radi_stretch[1]);
  fprintf(fp, "<strong>ALT DOPPLER FREQ 1: </strong>%16.7f<br>\n",ds->alt_dopcen[0]);
  fprintf(fp, "<strong>ALT DOPPLER FREQ 2: </strong>%16.7f<br>\n",ds->alt_dopcen[1]);
  fprintf(fp, "<strong>ALT DOPPLER FREQ 3: </strong>%16.7f<br>\n",ds->alt_dopcen[2]);
  fprintf(fp, "<strong>CRT DOPPLER FREQ 1: </strong>%16.7f<br>\n",ds->crt_dopcen[0]);
  fprintf(fp, "<strong>CRT DOPPLER FREQ 2: </strong>%16.7f<br>\n",ds->crt_dopcen[1]);
  fprintf(fp, "<strong>CRT DOPPLER FREQ 3: </strong>%16.7f<br>\n",ds->crt_dopcen[2]);
  fprintf(fp, "<strong>TIME DIRECT RNG: </strong>%s<br>\n",ds->time_dir_pix);
  fprintf(fp, "<strong>TIME DIRECT AZI: </strong>%s<br>\n",ds->time_dir_lin);
  fprintf(fp, "<strong>ALT DOPPLER RATE 1: </strong>%16.7f<br>\n",ds->alt_rate[0]);
  fprintf(fp, "<strong>ALT DOPPLER RATE 2: </strong>%16.7f<br>\n",ds->alt_rate[1]);
  fprintf(fp, "<strong>ALT DOPPLER RATE 3: </strong>%16.7f<br>\n",ds->alt_rate[2]);
  fprintf(fp, "<strong>CRT DOPPLER RATE 1: </strong>%16.7f<br>\n",ds->crt_rate[0]);
  fprintf(fp, "<strong>CRT DOPPLER RATE 2: </strong>%16.7f<br>\n",ds->crt_rate[1]);
  fprintf(fp, "<strong>CRT DOPPLER RATE 3: </strong>%16.7f<br>\n",ds->crt_rate[2]);
  fprintf(fp, "<strong>LINE CONTENT IND: </strong>%s<br>\n",ds->line_cont);
  fprintf(fp, "<strong>CLUTTER LOCK FLAG: </strong>%s<br>\n",ds->clutterlock_flg);
  fprintf(fp, "<strong>AUTOFOCUS FLAG: </strong>%s<br>\n",ds->auto_focus);
  fprintf(fp, "<strong>LINE SPACING: </strong>%16.7f<br>\n",ds->line_spacing);
  fprintf(fp, "<strong>PIXEL SPACING: </strong>%16.7f<br>\n",ds->pixel_spacing);
  fprintf(fp, "<strong>RNG COMPRESS DESG: </strong>%s<br>\n",ds->rngcmp_desg);
  if (era==0)
    {
      fprintf(fp, "<strong>NUM ANNOTATION PTS: </strong>%d<br>\n",ds->annot_pts);
      for (i=0; i<ds->annot_pts; i++) {
	fprintf(fp, "<strong>LINE NUM ANNOT START: </strong>%d<br>\n",ds->annot_line[i]);
	fprintf(fp, "<strong>PIXEL NUM ANNOT START: </strong>%d<br>\n",ds->annot_pixel[i]);
	fprintf(fp, "<strong>ANNOTATION TEXT: </strong>%d<br>\n",ds->annot_text[i][0]);
      }
    }
  else
    {
      fprintf(fp, "<strong>NUM OF BEAMS: </strong>%d<br>\n",ds->no_beams);
      fprintf(fp, "<strong>BEAM 1 IDENTIFIER: </strong>%s<br>\n",ds->beam1);
      fprintf(fp, "<strong>BEAM 2 IDENTIFIER: </strong>%s<br>\n",ds->beam2);
      fprintf(fp, "<strong>BEAM 3 IDENTIFIER: </strong>%s<br>\n",ds->beam3);
      fprintf(fp, "<strong>BEAM 4 IDENTIFIER: </strong>%s<br>\n",ds->beam4);
      fprintf(fp, "<strong>PRF OF BEAM 1 HZ: </strong>%8.3f<br>\n",ds->prf1);
      fprintf(fp, "<strong>PRF OF BEAM 2 HZ: </strong>%8.3f<br>\n",ds->prf2);
      fprintf(fp, "<strong>PRF OF BEAM 3 HZ: </strong>%8.3f<br>\n",ds->prf3);
      fprintf(fp, "<strong>PRF OF BEAM 4 HZ: </strong>%8.3f<br>\n",ds->prf4);
      fprintf(fp, "<strong>RANGE GATE OF BEAM 1: </strong>%8.3f<br>\n",ds->rng_gate1);
      fprintf(fp, "<strong>RANGE GATE OF BEAM 2: </strong>%8.3f<br>\n",ds->rng_gate2);
      fprintf(fp, "<strong>RANGE GATE OF BEAM 3: </strong>%8.3f<br>\n",ds->rng_gate3);
      fprintf(fp, "<strong>RANGE GATE OF BEAM 4: </strong>%8.3f<br>\n",ds->rng_gate4);
      fprintf(fp, "<strong>TOTAL PULSES PER BURST: </strong>%d<br>\n",ds->tot_pls_burst);
      fprintf(fp, "<strong>VALID PULSES PER BURST: </strong>%d<br>\n",ds->val_pls_burst);
      fprintf(fp, "<strong>RANGE LNS OVERLAP IN AZI WITH NEXT: </strong>%d<br>\n",ds->az_ovlp_nxt_img);
      fprintf(fp, "<strong>PIXEL OFFSET IN RANGE WITH NEXT: </strong>%d<br>\n",ds->rg_off_nxt_img);
      fprintf(fp, "<strong>CALIBRATION PARAMETER FILE USED: </strong>%s<br>\n",ds->cal_params_file);
      fprintf(fp, "<strong>SCAN RESULTS FILE USED: </strong>%s<br>\n",ds->scan_results_file);
      fprintf(fp, "<strong>SOFTWARE VERSION OF SCANNER: </strong>%s<br>\n",ds->scanner_version);
      fprintf(fp, "<strong>SOFTWARE VERSION OF DECODER: </strong>%s<br>\n",ds->decode_version);
    }
  fprintf(fp, "</body>\n</html>\n");
  FCLOSE(fp);

  return;
}
