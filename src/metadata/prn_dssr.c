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

void prn_dssr(struct dataset_sum_rec *ds, int era)
{
 int i;
 printf("\n*********** begin of Dataset Summary record *******************\n");
 printf("\n DSS SEQ NUM\t\t%d",ds->seq_num);
 printf("\n SAR CHNL INDTR\t\t%d",ds->sar_chan);
 printf("\n SCENE INDICATOR\t%s",ds->product_id);
 printf("\n SCENE DESIGNATOR\t%s",ds->scene_des);
 printf("\n INPT SCN CTR TIME\t%s",ds->inp_sctim);
 printf("\n ASC/DESCENDING\t\t%s",ds->asc_des);
 printf("\n LAT @ SCN CTR\t\t%16.7f",ds->pro_lat);
 printf("\n LONG @ SCN CTR\t\t%16.7f",ds->pro_long);
 printf("\n SCN CTR HEADING\t%16.7f",ds->pro_head);
 printf("\n ELLIP DESIGNATOR\t%s",ds->ellip_des);
 printf("\n ELLIP SEMIMAJOR\t%16.7f",ds->ellip_maj);
 printf("\n ELLIP SEMIMINOR\t%16.7f",ds->ellip_min);
 printf("\n EARTH MASS\t\t%16.7f",ds->earth_mass);
 printf("\n GRAVITATIONAL CNST\t%16.7f",ds->grav_const);
 printf("\n ELLIP PARM 1\t\t%16.7f",ds->ellip_j[0]);
 printf("\n ELLIP PARM 2\t\t%16.7f",ds->ellip_j[1]);
 printf("\n ELLIP PARM 3\t\t%16.7f",ds->ellip_j[2]);
 printf("\n AVG TERRAIN HT\t\t%16.7f",ds->terrain_h);
 printf("\n IMG CTR LINE NUM\t%f",ds->sc_lin);
 printf("\n IMG CTR PIX NUM\t%f",ds->sc_pix);
 printf("\n IMAGE LENGTH\t\t%16.7f",ds->scene_len);
 printf("\n IMAGE WIDTH\t\t%16.7f",ds->scene_wid);
 printf("\n NUM SAR CHANNELS\t%d",ds->nchn);
 printf("\n MISSION ID\t\t%s",ds->mission_id);
 printf("\n SENSOR ID\t\t%s",ds->sensor_id);
 printf("\n ORBIT NUMBER\t\t%s",ds->revolution);
 printf("\n PLAT LAT @ NADIR\t%8.3f",ds->plat_lat);
 printf("\n PLAT LONG @ NADIR\t%8.3f",ds->plat_long);
 printf("\n PLAT HEADING\t\t%8.3f",ds->plat_head_scene);
 printf("\n SNSR CLK ANGLE\t\t%8.3f",ds->clock_ang);
 printf("\n INCIDENCE ANGLE\t\t%8.3f",ds->incident_ang);
 printf("\n RADAR FREQUENCY\t%8.3f",ds->frequency);
 printf("\n RDR WAVELENGTH\t\t%16.7f",ds->wave_length);
 printf("\n MOTION COMP IND\t%s",ds->motion_comp);
 printf("\n RNG PULSE CODE\t\t%s",ds->pulse_code);
 printf("\n RNG CHIRP 1\t\t%16.7E",ds->ampl_coef[0]);
 printf("\n RNG CHIRP 2\t\t%16.7E",ds->ampl_coef[1]);
 printf("\n RNG CHIRP 3\t\t%16.7E",ds->ampl_coef[2]);
 printf("\n RNG CHIRP 4\t\t%16.7E",ds->ampl_coef[3]);
 printf("\n RNG CHIRP 5\t\t%16.7E",ds->ampl_coef[4]);
 printf("\n RNG PHASE 1\t\t%16.7E",ds->phas_coef[0]);
 printf("\n RNG PHASE 2\t\t%16.7E",ds->phas_coef[1]);
 printf("\n RNG PHASE 3\t\t%16.7E",ds->phas_coef[2]);
 printf("\n RNG PHASE 4\t\t%16.7E",ds->phas_coef[3]);
 printf("\n RNG PHASE 5\t\t%16.7E",ds->phas_coef[4]);
 printf("\n CHRP EXTRACTION IND\t%d",ds->chirp_ext_ind);
 printf("\n RNG CMPLX SAMPLE RATE\t%16.7f",ds->rng_samp_rate);
 printf("\n RNG GATE\t\t%16.7f",ds->rng_gate);
 printf("\n RNG PULSE LEN\t\t%16.7f",ds->rng_length);
 printf("\n BASEBAND FLAG\t\t%s",ds->baseband_f);
 printf("\n RNG COMPRESS FLAG\t%s",ds->rngcmp_f);
 printf("\n RCVR GAIN POLAR\t%16.7f",ds->gn_polar);
 printf("\n RCVR GAIN CROSS\t%16.7f",ds->gn_cross);
 printf("\n QUANT BITS/CHNL\t%d",ds->chn_bits);
 printf("\n QUANTZR DESCRPT\t%s",ds->quant_desc);
 printf("\n I CHNL DC BIAS\t\t%16.7f",ds->i_bias);
 printf("\n Q CHNL DC BIAS\t\t%16.7f",ds->q_bias);
 printf("\n I/Q CHNL RATIO\t\t%16.7f",ds->iq_ratio);
 printf("\n SPARE_DSS_7\t\t\t%16.7f",ds->spare_dss_7);
 printf("\n SPARE_DSS_8\t\t\t%16.7f",ds->spare_dss_8);
 printf("\n ELCTRNC BORESITE\t%16.7f",ds->ele_sight);
 printf("\n MECHNCL BORESITE\t%16.7f",ds->mech_sight);
 printf("\n ECHO TRK FLAG\t\t%s",ds->echo_track);
 printf("\n NOMINAL PRF\t\t%16.7f",ds->prf);
 printf("\n ANT ELEV BEAM WD\t%16.7f",ds->elev_beam);
 printf("\n ANT AZI BEAM WD\t%16.7f",ds->azi_beam);
 printf("\n SATLT BINARY TIME\t%s",ds->sat_bintim);
 printf("\n SATLT CLOCK TIME\t%s",ds->sat_clktim);
 printf("\n SATLT CLOCK INC\t%d",ds->sat_clkinc);
 printf("\n PROCESSING FACILITY\t%s",ds->fac_id);
 printf("\n PROCESSING SYSTEM\t%s",ds->sys_id);
 printf("\n PROCESSING VERSION\t%s",ds->ver_id);
 printf("\n FAC PROCESS CODE\t%s",ds->fac_code);
 printf("\n PRODUCT CODE\t\t%s",ds->lev_code);
 printf("\n PRODUCT TYPE\t\t%s",ds->product_type);
 printf("\n PROCESSING ALGTHM\t%s",ds->algor_id);
 printf("\n NUM LOOKS IN AZI\t%16.7f",ds->n_azilok);
 printf("\n NUM LOOKS IN RNG\t%16.7f",ds->n_rnglok);
 printf("\n BNDWDTH/LOOK IN AZI\t%16.7f",ds->bnd_azilok);
 printf("\n BNDWDTH/LOOK IN RNG\t%16.7f",ds->bnd_rnglok);
 printf("\n PROC BNDWDTH AZI\t%16.7f",ds->bnd_azi);
 printf("\n PROC BNDWDTH RNG\t%16.7f",ds->bnd_rng);
 printf("\n AZI WEIGHT FUNC\t%s",ds->azi_weight);
 printf("\n RNG WEIGHT FUNC\t%s",ds->rng_weight);
 printf("\n DATA INPUT SRC\t\t%s",ds->data_inpsrc);
 printf("\n NOM RESOLUTION RNG\t%16.7f",ds->rng_res);
 printf("\n NOM RESOLUTION AZI\t%16.7f",ds->azi_res);
 printf("\n RADIO STRETCH BIAS\t%16.7f",ds->radi_stretch[0]);
 printf("\n RADIO STRETCH GAIN\t%16.7f",ds->radi_stretch[1]);
 printf("\n ALT DOPPLER FREQ 1\t%16.7f",ds->alt_dopcen[0]);
 printf("\n ALT DOPPLER FREQ 2\t%16.7f",ds->alt_dopcen[1]);
 printf("\n ALT DOPPLER FREQ 3\t%16.7f",ds->alt_dopcen[2]);
 printf("\n CRT DOPPLER FREQ 1\t%16.7f",ds->crt_dopcen[0]);
 printf("\n CRT DOPPLER FREQ 2\t%16.7f",ds->crt_dopcen[1]);
 printf("\n CRT DOPPLER FREQ 3\t%16.7f",ds->crt_dopcen[2]);
 printf("\n TIME DIRECT RNG\t%s",ds->time_dir_pix);
 printf("\n TIME DIRECT AZI\t%s",ds->time_dir_lin);
 printf("\n ALT DOPPLER RATE 1\t%16.7f",ds->alt_rate[0]);
 printf("\n ALT DOPPLER RATE 2\t%16.7f",ds->alt_rate[1]);
 printf("\n ALT DOPPLER RATE 3\t%16.7f",ds->alt_rate[2]);
 printf("\n CRT DOPPLER RATE 1\t%16.7f",ds->crt_rate[0]);
 printf("\n CRT DOPPLER RATE 2\t%16.7f",ds->crt_rate[1]);
 printf("\n CRT DOPPLER RATE 3\t%16.7f",ds->crt_rate[2]);
 printf("\n LINE CONTENT IND\t%s",ds->line_cont);
 printf("\n CLUTTER LOCK FLAG\t%s",ds->clutterlock_flg);
 printf("\n AUTOFOCUS FLAG\t\t%s",ds->auto_focus);
 printf("\n LINE SPACING\t\t%16.7f",ds->line_spacing);
 printf("\n PIXEL SPACING\t\t%16.7f",ds->pixel_spacing);
 printf("\n RNG COMPRESS DESG\t%s",ds->rngcmp_desg);
 if (era==0)
  {
   printf("\n NUM ANNOTATION PTS\t%d",ds->annot_pts);
   for (i=0; i<ds->annot_pts; i++) {
     printf("\n LINE NUM ANNOT START\t\t%d",ds->annot_line[i]);
     printf("\n PIXEL NUM ANNOT START\t\t%d",ds->annot_pixel[i]);
     printf("\n ANNOTATION TEXT\t\t%d",ds->annot_text[i][0]);
   }
  }
 else
  {
   printf("\n NUM OF BEAMS\t%d",ds->no_beams);
   printf("\n BEAM 1 IDENTIFIER\t%s",ds->beam1);
   printf("\n BEAM 2 IDENTIFIER\t%s",ds->beam2);
   printf("\n BEAM 3 IDENTIFIER\t%s",ds->beam3);
   printf("\n BEAM 4 IDENTIFIER\t%s",ds->beam4);
   printf("\n PRF OF BEAM 1 HZ\t%8.3f",ds->prf1);
   printf("\n PRF OF BEAM 2 HZ\t%8.3f",ds->prf2);
   printf("\n PRF OF BEAM 3 HZ\t%8.3f",ds->prf3);
   printf("\n PRF OF BEAM 4 HZ\t%8.3f",ds->prf4);
   printf("\n RANGE GATE OF BEAM 1\t%8.3f",ds->rng_gate1);
   printf("\n RANGE GATE OF BEAM 2\t%8.3f",ds->rng_gate2);
   printf("\n RANGE GATE OF BEAM 3\t%8.3f",ds->rng_gate3);
   printf("\n RANGE GATE OF BEAM 4\t%8.3f",ds->rng_gate4);
   printf("\n TOTAL PULSES PER BURST\t%d",ds->tot_pls_burst);
   printf("\n VALID PULSES PER BURST\t%d",ds->val_pls_burst);
   printf("\n RANGE LNS OVERLAP IN AZI WITH NEXT\t%d",ds->az_ovlp_nxt_img);
   printf("\n PIXEL OFFSET IN RANGE WITH NEXT\t%d",ds->rg_off_nxt_img);
   printf("\n CALIBRATION PARAMETER FILE USED\t%s",ds->cal_params_file);
   printf("\n SCAN RESULTS FILE USED         \t%s",ds->scan_results_file);
   printf("\n SOFTWARE VERSION OF SCANNER    \t%s",ds->scanner_version);
   printf("\n SOFTWARE VERSION OF DECODER    \t%s",ds->decode_version);
  }
  printf("\n*********** end of Dataset Summary  record ********************\n");
  return;
 }
