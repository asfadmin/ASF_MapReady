#include <stdio.h>
#ifdef SGI
#include <strings.h>
#else
#include <string.h>
#endif
#include <stdlib.h>
#include <ctype.h>
#include "defs.h"
#include "extern.h"


static char sccsid_odl_lib_c[] =
            "@(#)odl_lib.c	1.7 96/10/04 14:42:01";

/* Added by Dan F. */
extern int create_ODL_file() ;

extern FILE *ODL_w_fp;

#define ODL_WRITE(format,  args)	fprintf(ODL_w_fp, format, args)

int CEOS_to_ODL()
{
    SARL_ptr* current;

    if (!CEOS.odl_out) return(0);
    if ( !(create_ODL_file(CEOS.odl_w_file, "")) ) return(0);
    
    /* grab the current CEOS SARL structure */
    if ( (current = GetCurrentSARL())  == NULL) {
	printf("\nEmpty CEOS SARL structure");
	return(0);
    }
    
    /* write the File Descriptor record */
    FDR_to_ODL( Get_L_FDR( current ) );
    DSS_to_ODL( Get_L_DSS( current ) );
    MP_to_ODL( Get_L_MP( current) );
    PP_to_ODL( Get_L_PP( current) );
    ATT_to_ODL( Get_L_AT( current) );
    RD_to_ODL( Get_L_RD( current) );
    RC_to_ODL( Get_L_RC( current) );
    DQS_to_ODL( Get_L_DQS( current) );
    DH_to_ODL( Get_L_DH( current) );
    RS_to_ODL( Get_L_RS( current) );
    DE_to_ODL( Get_L_DE( current) );
    DP_to_ODL( Get_L_DP( current) );
    FR_to_ODL( Get_L_FR( current) );
    ODL_WRITE("END\n", "");
    return(1);
}


int FDR_to_ODL( Sarl_Desc_Rec* t ) {
	
    if (t == (Sarl_Desc_Rec*) NULL) return(FALSE);
	
	ODL_WRITE("OBJECT            = Sarl_Desc_Rec\n", "");
	ODL_WRITE("  ASCII_FLAG        = \"%s\"\n", t->ascii_flag);
	ODL_WRITE("  FORMAT_DOC        = \"%s\"\n", t->format_doc);
	ODL_WRITE("  FORMAT_REV        = \"%s\"\n", t->format_rev);
	ODL_WRITE("  DESIGN_REV        = \"%s\"\n", t->design_rev );
	ODL_WRITE("  SOFTWARE_ID       = \"%s\"\n", t->software_id);
	ODL_WRITE("  FILE_NUM          = %d\n", t->file_num);
	ODL_WRITE("  PRODUCT_ID        = \"%s\"\n", t->product_id);
	ODL_WRITE("  REC_SEQ_FLAG      = \"%s\"\n", t->rec_seq_flag);
	ODL_WRITE("  SEQ_LOC           = %ld\n", t->seq_loc);
	ODL_WRITE("  SEQ_LEN           = %d\n", t->seq_len);
	ODL_WRITE("  REC_CODE          = \"%s\"\n", t->rec_code);
	ODL_WRITE("  CODE_LOC          = %ld\n", t->code_loc);
	ODL_WRITE("  CODE_LEN          = %d\n", t->code_len);
	ODL_WRITE("  REC_LEN           = \"%s\"\n", t->rec_len);
	ODL_WRITE("  RLEN_LOC          = %ld\n", t->rlen_loc);
	ODL_WRITE("  RLEN_LEN          = %d\n", t->rlen_len);
	ODL_WRITE("  SPARE_FDR_3       = \"%s\"\n", t->spare_fdr_3);
	ODL_WRITE("  N_DATASET         = %ld\n", t->n_dataset);
	ODL_WRITE("  L_DATASET         = %ld\n", t->l_dataset);
	ODL_WRITE("  N_MAP_PROJ        = %ld\n", t->n_map_proj);
	ODL_WRITE("  L_MAP_PROJ        = %ld\n", t->l_map_proj);
	ODL_WRITE("  N_PLAT_POS        = %ld\n", t->n_plat_pos);
	ODL_WRITE("  L_PLAT_POS        = %ld\n", t->l_plat_pos);
	ODL_WRITE("  N_ATT_DATA        = %ld\n", t->n_att_data);
	ODL_WRITE("  L_ATT_DATA        = %ld\n", t->l_att_data);
	ODL_WRITE("  N_RADI_DATA       = %ld\n", t->n_radi_data);
	ODL_WRITE("  L_RADI_DATA       = %ld\n", t->l_radi_data);
	ODL_WRITE("  N_RADI_COMP       = %ld\n", t->n_radi_comp);
	ODL_WRITE("  L_RADI_COMP       = %ld\n", t->l_radi_comp);
	ODL_WRITE("  N_QUAL_SUM        = %ld\n", t->n_qual_sum);
	ODL_WRITE("  L_QUAL_SUM        = %ld\n", t->l_qual_sum);
	ODL_WRITE("  N_DATA_HIST       = %ld\n", t->n_data_hist);
	ODL_WRITE("  L_DATA_HIST       = %ld\n", t->l_data_hist);
	ODL_WRITE("  N_RANG_SPEC       = %ld\n", t->n_rang_spec);
	ODL_WRITE("  L_RANG_SPEC       = %ld\n", t->l_rang_spec);
	ODL_WRITE("  N_DEM_DESC        = %ld\n", t->n_dem_desc);
	ODL_WRITE("  L_DEM_DESC        = %ld\n", t->l_dem_desc);
	ODL_WRITE("  N_RADAR_PAR       = %ld\n", t->n_radar_par);
	ODL_WRITE("  L_RADAR_PAR       = %ld\n", t->l_radar_par);
	ODL_WRITE("  N_ANNO_DATA       = %ld\n", t->n_anno_data);
	ODL_WRITE("  L_ANNO_DATA       = %ld\n", t->l_anno_data);
	ODL_WRITE("  N_DET_PROC        = %ld\n", t->n_det_proc);
	ODL_WRITE("  L_DET_PROC        = %ld\n", t->l_det_proc);
	ODL_WRITE("  N_CAL             = %ld\n", t->n_cal);
	ODL_WRITE("  L_CAL             = %ld\n", t->l_cal);
	ODL_WRITE("  N_GCP             = %ld\n", t->n_gcp);
	ODL_WRITE("  L_GCP             = %ld\n", t->l_gcp);
	ODL_WRITE("  SPARE_FDR_4       = \"%s\"\n", t->spare_fdr_4);
	ODL_WRITE("  N_FAC_DATA        = %ld\n", t->n_fac_data);
	ODL_WRITE("  L_FAC_DATA        = %ld\n", t->l_fac_data);
	ODL_WRITE("END_OBJECT        = Sarl_Desc_Rec\n", "");
     return(TRUE);
  }
  
int DSS_to_ODL( Dataset_Sum* ds ) {

	int i;
   
   if (ds == (Dataset_Sum*) NULL) return(FALSE);
   
   	ODL_WRITE("OBJECT          = Dataset_Sum\n", "");
        ODL_WRITE("  SEQ_NUM         = %d\n", ds->seq_num );
	ODL_WRITE("  SAR_CHAN        = %d\n", ds->sar_chan );
	ODL_WRITE("  PRODUCT_ID      = \"%s\"\n", ds->product_id );
	ODL_WRITE("  SCENE_DES       = \"%s\"\n", ds->scene_des );
	ODL_WRITE("  INP_SCTIM       = \"%s\"\n", ds->inp_sctim );
	ODL_WRITE("  ASC_DES         = \"%s\"\n", ds->asc_des );
	ODL_WRITE("  PRO_LAT         = %16.7f\n", ds->pro_lat );
	ODL_WRITE("  PRO_LONG        = %16.7f\n", ds->pro_long );
	ODL_WRITE("  PRO_HEAD        = %16.7f\n", ds->pro_head );
	ODL_WRITE("  ELLIP_DES       = \"%s\"\n", ds->ellip_des );
	ODL_WRITE("  ELLIP_MAJ       = %16.7f\n", ds->ellip_maj );
	ODL_WRITE("  ELLIP_MIN       = %16.7f\n", ds->ellip_min );
	ODL_WRITE("  EARTH_MASS      = %16.7f\n", ds->earth_mass );
	ODL_WRITE("  GRAV_CONST      = %16.7f\n", ds->grav_const );
	ODL_WRITE("  ELLIP_J         = ( %16.7f,", ds->ellip_j[0] );
	ODL_WRITE(" %16.7f, ", ds->ellip_j[1] );
	ODL_WRITE(" %16.7f )\n", ds->ellip_j[2] );
	ODL_WRITE("  TERRAIN_H       = %16.7f\n", ds->terrain_h );
	ODL_WRITE("  SC_LIN          = %ld\n", ds->sc_lin );
	ODL_WRITE("  SC_PIX          = %ld\n", ds->sc_pix );
	ODL_WRITE("  SCENE_LEN       = %16.7f\n", ds->scene_len );
	ODL_WRITE("  SCENE_WID       = %16.7f\n", ds->scene_wid );
        ODL_WRITE("  NCHN            = %d\n", ds->nchn );
	ODL_WRITE("  MISSION_ID      = \"%s\"\n", ds->mission_id );
	ODL_WRITE("  SENSOR_ID       = \"%s\"\n", ds->sensor_id );
	ODL_WRITE("  REVOLUTION      = \"%s\"\n", ds->revolution );
        ODL_WRITE("  PLAT_LAT        = %8.3f\n", ds->plat_lat );
	ODL_WRITE("  PLAT_LONG       = %8.3f\n", ds->plat_long );
	ODL_WRITE("  PLAT_HEAD_SCENE = %8.3f\n", ds->plat_head_scene );
	ODL_WRITE("  CLOCK_ANG       = %8.3f\n", ds->clock_ang );
	ODL_WRITE("  INCIDENT_ANG    = %8.3f\n", ds->incident_ang );	
        ODL_WRITE("  FREQUENCY       = %8.3f\n", ds->frequency );
	ODL_WRITE("  WAVE_LENGTH     = %16.7f\n", ds->wave_length );
	ODL_WRITE("  MOTION_COMP     = \"%s\"\n", ds->motion_comp );
	ODL_WRITE("  PULSE_CODE      = \"%s\"\n", ds->pulse_code );	
        ODL_WRITE("  AMPL_COEF       = ( %16.7e,", ds->ampl_coef[0] );
	ODL_WRITE(" %16.7e, ", ds->ampl_coef[1] );
	ODL_WRITE(" %16.7e, ", ds->ampl_coef[2] );
	ODL_WRITE(" %16.7e, ", ds->ampl_coef[3] );	
        ODL_WRITE(" %16.7e )\n", ds->ampl_coef[4] );
	ODL_WRITE("  PHAS_COEF       = ( %16.7e,", ds->phas_coef[0] );
	ODL_WRITE(" %16.7e, ", ds->phas_coef[1] );
	ODL_WRITE(" %16.7e, ", ds->phas_coef[2] );	
        ODL_WRITE(" %16.7e, ", ds->phas_coef[3] );
	ODL_WRITE(" %16.7e )\n", ds->phas_coef[4] );
	ODL_WRITE("  CHIRP_EXT_IND   = %ld\n", ds->chirp_ext_ind );
	ODL_WRITE("  RNG_SAMP_RATE   = %16.7f\n", ds->rng_samp_rate );
	ODL_WRITE("  RNG_GATE        = %16.7f\n", ds->rng_gate );
	ODL_WRITE("  RNG_LENGTH      = %16.7f\n", ds->rng_length );
	ODL_WRITE("  BASEBAND_F      = \"%s\"\n", ds->baseband_f );
	ODL_WRITE("  RNGCMP_F        = \"%s\"\n", ds->rngcmp_f );
	ODL_WRITE("  GN_POLAR        = %16.7f\n", ds->gn_polar );
	ODL_WRITE("  GN_CROSS        = %16.7f\n", ds->gn_cross );
	ODL_WRITE("  CHN_BITS        = %ld\n", ds->chn_bits );
	ODL_WRITE("  QUANT_DESC      = \"%s\"\n", ds->quant_desc );
	ODL_WRITE("  I_BIAS          = %16.7f\n", ds->i_bias );
	ODL_WRITE("  Q_BIAS          = %16.7f\n", ds->q_bias );
	ODL_WRITE("  IQ_RATIO        = %16.7f\n", ds->iq_ratio );
        ODL_WRITE("  SPARE_DSS_7     = %16.7f\n", ds->spare_dss_7 );
        ODL_WRITE("  SPARE_DSS_8     = %16.7f\n", ds->spare_dss_8 );
	ODL_WRITE("  ELE_SIGHT       = %16.7f\n", ds->ele_sight );
	ODL_WRITE("  MECH_SIGHT      = %16.7f\n", ds->mech_sight );
	ODL_WRITE("  ECHO_TRACK      = \"%s\"\n", ds->echo_track );
	ODL_WRITE("  PRF             = %16.7f\n", ds->prf );
	ODL_WRITE("  ELEV_BEAM       = %16.7f\n", ds->elev_beam );
	ODL_WRITE("  AZI_BEAM        = %16.7f\n", ds->azi_beam );	
	ODL_WRITE("  SAT_BINTIM      = \"%s\"\n", ds->sat_bintim );
	ODL_WRITE("  SAT_CLKTIM      = \"%s\"\n", ds->sat_clktim );
	ODL_WRITE("  SAT_CLKINC      = %ld\n", ds->sat_clkinc );	
	ODL_WRITE("  FAC_ID          = \"%s\"\n", ds->fac_id );
	ODL_WRITE("  SYS_ID          = \"%s\"\n", ds->sys_id );
	ODL_WRITE("  VER_ID          = \"%s\"\n", ds->ver_id );	
	ODL_WRITE("  FAC_CODE        = \"%s\"\n", ds->fac_code );
	ODL_WRITE("  LEV_CODE        = \"%s\"\n", ds->lev_code );
	ODL_WRITE("  PRODUCT_TYPE   = \"%s\"\n", ds->product_type );	
	ODL_WRITE("  ALGOR_ID        = \"%s\"\n", ds->algor_id );
	ODL_WRITE("  N_AZILOK        = %16.7f\n", ds->n_azilok );
	ODL_WRITE("  N_RNGLOK        = %16.7f\n", ds->n_rnglok );	
	ODL_WRITE("  BND_AZILOK      = %16.7f\n", ds->bnd_azilok );
	ODL_WRITE("  BND_RNGLOK      = %16.7f\n", ds->bnd_rnglok );
	ODL_WRITE("  BND_AZI         = %16.7f\n", ds->bnd_azi );	
	ODL_WRITE("  BND_RNG         = %16.7f\n", ds->bnd_rng );
	ODL_WRITE("  AZI_WEIGHT      = \"%s\"\n", ds->azi_weight );
	ODL_WRITE("  RNG_WEIGHT      = \"%s\"\n", ds->rng_weight );	
	ODL_WRITE("  DATA_INPSRC     = \"%s\"\n", ds->data_inpsrc );	
	ODL_WRITE("  RNG_RES         = %16.7f\n", ds->rng_res );
	ODL_WRITE("  AZI_RES         = %16.7f\n", ds->azi_res );
	ODL_WRITE("  RADI_STRETCH    = ( %16.7f,", ds->radi_stretch[0] );	
	ODL_WRITE(" %16.7f )\n", ds->radi_stretch[1] );	
	ODL_WRITE("  ALT_DOPCEN      = ( %16.7f,", ds->alt_dopcen[0] );
	ODL_WRITE(" %16.7f,", ds->alt_dopcen[1] );
	ODL_WRITE(" %16.7f )\n", ds->alt_dopcen[2] );	
	ODL_WRITE("  CRT_DOPCEN      = ( %16.7f,", ds->crt_dopcen[0] );	
	ODL_WRITE(" %16.7f,", ds->crt_dopcen[1] );
	ODL_WRITE(" %16.7f )\n", ds->crt_dopcen[2] );
	ODL_WRITE("  TIME_DIR_PIX    = \"%s\"\n", ds->time_dir_pix );	
	ODL_WRITE("  TIME_DIR_LIN    = \"%s\"\n", ds->time_dir_lin );	
	ODL_WRITE("  ALT_RATE        = ( %16.7f,", ds->alt_rate[0] );
	ODL_WRITE(" %16.7f,", ds->alt_rate[1] );
	ODL_WRITE(" %16.7f )\n", ds->alt_rate[2] );
	ODL_WRITE("  CRT_RATE        = ( %16.7f,", ds->crt_rate[0] );	
	ODL_WRITE(" %16.7f,", ds->crt_rate[1] );
	ODL_WRITE(" %16.7f )\n", ds->crt_rate[2] );
	ODL_WRITE("  LINE_CONT       = \"%s\"\n", ds->line_cont );
	ODL_WRITE("  CLUTTERLOCK_FLG = \"%s\"\n", ds->clutterlock_flg );
	ODL_WRITE("  AUTO_FOCUS      = \"%s\"\n", ds->auto_focus );	
	ODL_WRITE("  LINE_SPACING    = %16.7f\n", ds->line_spacing );
	ODL_WRITE("  PIXEL_SPACING   = %16.7f\n", ds->pixel_spacing );
	ODL_WRITE("  RNGCMP_DESG     = \"%s\"\n", ds->rngcmp_desg );
#ifdef PRE_RADARSAT
        ODL_WRITE("  SPARE12         = \"%s\"\n", ds->spare12 );	
	ODL_WRITE("  ANNOT_PTS       = %d\n", ds->annot_pts );	
        ODL_WRITE("  SPARE13         = \"%s\"\n", ds->spare13 );
	if (ds->annot_pts) {
	    ODL_WRITE("  OBJECT             = annot_rec_t\n", "");
	    ODL_WRITE("    ANNOT_LINE       = ( %d", ds->annot_line[0] );
	    for (i=1; i<ds->annot_pts; i++) {
	        if (!(i % 4) ) {
		   ODL_WRITE("\n                        ", "");
		   ODL_WRITE("%d", ds->annot_line[i] );
		}
		else
		   ODL_WRITE(", %d", ds->annot_line[i] );
		if ( i==(ds->annot_pts-1) ) ODL_WRITE(" )\n", "");
	    }
	    
	    ODL_WRITE("    ANNOT_PIXEL      = ( %d", ds->annot_pixel[0] );
	    for (i=1; i<ds->annot_pts; i++) {
	        if (!(i % 4) ) {
		   ODL_WRITE(",\n                        ", "");
		   ODL_WRITE("%d", ds->annot_pixel[i] );
		}
		else
		   ODL_WRITE(", %d", ds->annot_pixel[i] );
	        if ( i==(ds->annot_pts-1) ) ODL_WRITE(" )\n", "");
	    }
	    
	    ODL_WRITE("    ANNOT_TEXT       = ( %s", ds->annot_text[0][0] );
	    for (i=1; i<ds->annot_pts; i++) {
		ODL_WRITE(", \"%s\"\n", ds->annot_text[i][0] );
		if ( i==(ds->annot_pts-1) ) ODL_WRITE(" )\n", "");
	    }
	    
	    ODL_WRITE("  END_OBJECT        = annot_rec_t\n", "");	
        }
        ODL_WRITE("  SPARE14         = \"%s\"\n", ds->spare14 );
#else
	ODL_WRITE("  NO_BEAMS        = %d\n", ds->no_beams );	
        ODL_WRITE("  BEAM1           = \"%s\"\n", ds->beam1 );
        ODL_WRITE("  BEAM2           = \"%s\"\n", ds->beam2 );
        ODL_WRITE("  BEAM3           = \"%s\"\n", ds->beam3 );
        ODL_WRITE("  BEAM4           = \"%s\"\n", ds->beam4 );
	ODL_WRITE("  PRF1            =  %8.3f\n", ds->prf1 );	
	ODL_WRITE("  PRF2            =  %8.3f\n", ds->prf2 );	
	ODL_WRITE("  PRF3            =  %8.3f\n", ds->prf3 );	
	ODL_WRITE("  PRF4            =  %8.3f\n", ds->prf4 );	
	ODL_WRITE("  RNG_GATE1       =  %8.3f\n", ds->rng_gate1 );	
	ODL_WRITE("  RNG_GATE2       =  %8.3f\n", ds->rng_gate2 );	
	ODL_WRITE("  RNG_GATE3       =  %8.3f\n", ds->rng_gate3 );	
	ODL_WRITE("  RNG_GATE4       =  %8.3f\n", ds->rng_gate4 );	
	ODL_WRITE("  TOT_PLS_BURST   = %d\n", ds->tot_pls_burst );	
	ODL_WRITE("  VAL_PLS_BURST   = %d\n", ds->val_pls_burst );	
	ODL_WRITE("  AZ_OVLP_NXT_IMG = %d\n", ds->az_ovlp_nxt_img );	
	ODL_WRITE("  RN_OFF_NXT_IMG  = %d\n", ds->rg_off_nxt_img );	
        ODL_WRITE("  CAL_PARAMS_FILE    = \"%s\"\n", ds->cal_params_file );
        ODL_WRITE("  SCAN_RESULTS_FILE  = \"%s\"\n", ds->scan_results_file );
        ODL_WRITE("  SCANNER_VERSION    = \"%s\"\n", ds->scanner_version );
        ODL_WRITE("  DECODE_VERSION     = \"%s\"\n", ds->decode_version );
#endif
        ODL_WRITE("END_OBJECT      = Dataset_Sum\n", "");
   
    if (ds->next != (Dataset_Sum *) NULL) DSS_to_ODL( ds->next);
    return(TRUE);
}

int MP_to_ODL( Map_Proj* mp ) 
{

   if (mp== (Map_Proj*) NULL) return(FALSE);
       ODL_WRITE("OBJECT              = Map_Proj\n", "");
       ODL_WRITE("  MAP_DESC            = \"%s\"\n", mp->map_desc );
       ODL_WRITE("  N_PIXEL             = %ld\n", mp->n_pixel );
       ODL_WRITE("  N_LINE              = %ld\n", mp->n_line );
       ODL_WRITE("  PIXEL_SPACING       = %16.7f\n", mp->pixel_spacing );
       ODL_WRITE("  LINE_SPACING        = %16.7f\n", mp->line_spacing );
       ODL_WRITE("  OSC_ORIENT          = %16.7f\n", mp->osc_orient );
       ODL_WRITE("  ORB_INCL            = %16.7f\n", mp->orb_incl );
       ODL_WRITE("  ASC_NODE            = %16.7f\n", mp->asc_node );
       ODL_WRITE("  ISC_DIST            = %16.7f\n", mp->isc_dist );
       ODL_WRITE("  GEO_ALT             = %16.7f\n", mp->geo_alt );
       ODL_WRITE("  ISC_VEL             = %16.7f\n", mp->isc_vel );
       ODL_WRITE("  PLAT_HEAD           = %16.7f\n", mp->plat_head );
       ODL_WRITE("  REF_ELLIP           = \"%s\"\n", mp->ref_ellip );
       ODL_WRITE("  SEMI_MAJOR          = %16.7f\n", mp->semi_major );
       ODL_WRITE("  SEMI_MINOR          = %16.7f\n", mp->semi_minor );
       ODL_WRITE("  DATUM_SHIFT         = ( %16.7f,", mp->datum_shift[0] );
       ODL_WRITE(" %16.7f,", mp->datum_shift[1] );
       ODL_WRITE(" %16.7f  )\n", mp->datum_shift[2] );
       ODL_WRITE("  AUX_DATUM_SHIFT     = ( %16.7f,", mp->aux_datum_shift[0] );
       ODL_WRITE(" %16.7f, ", mp->aux_datum_shift[1] );
       ODL_WRITE(" %16.7f  )\n", mp->aux_datum_shift[2] );
       ODL_WRITE("  SCAL_ELLIP          = %16.7f\n", mp->scal_ellip );
       ODL_WRITE("  PROJECTION          = \"%s\"\n", mp->projection );
       ODL_WRITE("  UTM_DESC            = \"%s\"\n", mp->utm_desc );
       ODL_WRITE("  UTM_ZONE_SIG        = \"%s\"\n", mp->utm_zone_sig );
       ODL_WRITE("  UTM_EAST_ORIG       = %16.7f\n", mp->utm_east_orig );
       ODL_WRITE("  UTM_NORTH_ORIG      = %16.7f\n", mp->utm_north_orig  );
       ODL_WRITE("  UTM_CENT_LONG       = %16.7f\n", mp->utm_cent_long );
       ODL_WRITE("  UTM_CENT_LAT        = %16.7f\n", mp->utm_cent_lat );
       ODL_WRITE("  UTM_STAND_PAR       = ( %16.7f,", mp->utm_stand_par[0] );
       ODL_WRITE(" %16.7f )\n", mp->utm_stand_par[1] );
       ODL_WRITE("  UTM_SCALE           = %16.7f\n", mp->utm_scale );
       ODL_WRITE("  UPS_DESC            = \"%s\"\n", mp->ups_desc );
       ODL_WRITE("  UPS_CENT_LONG       = %16.7f\n", mp->ups_cent_long );
       ODL_WRITE("  UPS_CENT_LAT        = %16.7f\n", mp->ups_cent_lat );
       ODL_WRITE("  UPS_SCALE           = %16.7f\n", mp->ups_scale );
       ODL_WRITE("  NSP_DESC            = \"%s\"\n", mp->nsp_desc );
       ODL_WRITE("  NSP_EAST_ORIG       = %16.7f\n", mp->nsp_east_orig );
       ODL_WRITE("  NSP_NORTH_ORIG      = %16.7f\n", mp->nsp_north_orig );
       ODL_WRITE("  NSP_CENT_LONG       = %16.7f\n", mp->nsp_cent_long );
       ODL_WRITE("  NSP_CENT_LAT        = %16.7f\n", mp->nsp_cent_lat );
       ODL_WRITE("  NSP_STAND_PAR       = ( %16.7f,", mp->nsp_stand_par[0] );
       ODL_WRITE(" %16.7f,", mp->nsp_stand_par[1] );
       ODL_WRITE(" %16.7f,", mp->nsp_stand_par[2] );
       ODL_WRITE(" %16.7f )\n", mp->nsp_stand_par[3] );
       ODL_WRITE("  NSP_STAND_MER       = ( %16.7f,", mp->nsp_stand_mer[0] );
       ODL_WRITE(" %16.7f,", mp->nsp_stand_mer[1] );
       ODL_WRITE(" %16.7f )\n", mp->nsp_stand_mer[2] );
       ODL_WRITE("  CORNER_NE           = ( %16.7f,", mp->corner_ne[0] );
       ODL_WRITE(" %16.7f,", mp->corner_ne[1] );
       ODL_WRITE(" %16.7f,", mp->corner_ne[2] );
       ODL_WRITE(" %16.7f,\n", mp->corner_ne[3] );
       ODL_WRITE("                          %16.7f,", mp->corner_ne[4] );
       ODL_WRITE(" %16.7f,", mp->corner_ne[5] );
       ODL_WRITE(" %16.7f,", mp->corner_ne[6] );
       ODL_WRITE(" %16.7f )\n", mp->corner_ne[7] );
       ODL_WRITE("  CORNER_LL           = ( %16.7f,", mp->corner_ll[0] );
       ODL_WRITE(" %16.7f, ", mp->corner_ll[1] );
       ODL_WRITE(" %16.7f, ", mp->corner_ll[2] );
       ODL_WRITE(" %16.7f,\n", mp->corner_ll[3] );
       ODL_WRITE("                          %16.7f, ", mp->corner_ll[4] );
       ODL_WRITE(" %16.7f,", mp->corner_ll[5] );
       ODL_WRITE(" %16.7f,", mp->corner_ll[6] );
       ODL_WRITE(" %16.7f )\n", mp->corner_ll[7] );
       ODL_WRITE("  TERR_HEIGHT         = ( %16.7f,", mp->terr_height[0] );
       ODL_WRITE(" %16.7f,", mp->terr_height[1] );
       ODL_WRITE(" %16.7f,", mp->terr_height[2] );
       ODL_WRITE(" %16.7f )\n", mp->terr_height[3] );
       ODL_WRITE("  LP_CONV_COEF        = ( %16.7f,", mp->lp_conv_coef[0] );
       ODL_WRITE(" %16.7f,", mp->lp_conv_coef[1] );
       ODL_WRITE(" %16.7f,", mp->lp_conv_coef[2] );
       ODL_WRITE(" %16.7f,\n", mp->lp_conv_coef[3] );
       ODL_WRITE("                          %16.7f, ", mp->lp_conv_coef[4] );
       ODL_WRITE(" %16.7f,", mp->lp_conv_coef[5] );
       ODL_WRITE(" %16.7f,", mp->lp_conv_coef[6] );
       ODL_WRITE(" %16.7f )\n", mp->lp_conv_coef[7] );
       ODL_WRITE("  MP_CONV_COEF        = ( %16.7f,", mp->mp_conv_coef[0] );
       ODL_WRITE(" %16.7f,", mp->mp_conv_coef[1] );
       ODL_WRITE(" %16.7f,", mp->mp_conv_coef[2] );
       ODL_WRITE(" %16.7f,\n", mp->mp_conv_coef[3] );
       ODL_WRITE("                          %16.7f,", mp->mp_conv_coef[4] );
       ODL_WRITE(" %16.7f,", mp->mp_conv_coef[5] );
       ODL_WRITE(" %16.7f,", mp->mp_conv_coef[6] );
       ODL_WRITE(" %16.7f )\n", mp->mp_conv_coef[7] );

       ODL_WRITE("END_OBJECT          = Map_Proj\n", "");
    if (mp->next != (Map_Proj *) NULL) MP_to_ODL( mp->next );
    return(TRUE);
}

int PP_to_ODL( Pos_Data* p) 
{
   Pos_Vect_Rec *pv;

   if (p == (Pos_Data*) NULL) return(FALSE);
       ODL_WRITE("OBJECT            = Pos_Data\n", "");
       ODL_WRITE("  SEQ_NUM           = %d\n", p->seq_num );
       ODL_WRITE("  ORBIT_ELE_DESG    = \"%s\"\n", p->orbit_ele_desg );
       ODL_WRITE("  ORBIT_ELE         = ( %16.7f,", p->orbit_ele[0] );
       ODL_WRITE(" %16.7f,", p->orbit_ele[1] );
       ODL_WRITE(" %16.7f,\n", p->orbit_ele[2] );
       ODL_WRITE("                        %16.7f,", p->orbit_ele[3] );
       ODL_WRITE(" %16.7f,", p->orbit_ele[4] );
       ODL_WRITE(" %16.7f )\n", p->orbit_ele[5] );
       ODL_WRITE("  NDATA             = %d\n", p->ndata );
       ODL_WRITE("  YEAR              = %d\n", p->year );
       ODL_WRITE("  MONTH             = %d\n", p->month );
       ODL_WRITE("  DAY               = %d\n", p->day );
       ODL_WRITE("  GMT_DAY           = %d\n", p->gmt_day );
       ODL_WRITE("  GMT_SEC           = %22.15f\n", p->gmt_sec );
       ODL_WRITE("  DATA_INT          = %22.15f\n", p->data_int );
       ODL_WRITE("  REF_COORD         = \"%s\"\n", p->ref_coord );
       ODL_WRITE("  HR_ANGLE          = %22.15f\n", p->hr_angle );
       ODL_WRITE("  ALT_POSERR        = %16.7f\n", p->alt_poserr );
       ODL_WRITE("  CRT_POSERR        = %16.7f\n", p->crt_poserr );
       ODL_WRITE("  RAD_POSERR        = %16.7f\n", p->rad_poserr );
       ODL_WRITE("  ALT_VELERR        = %16.7f\n", p->alt_velerr );
       ODL_WRITE("  CRT_VELERR        = %16.7f\n", p->crt_velerr );
       ODL_WRITE("  RAD_VELERR        = %16.7f\n", p->rad_velerr );


       pv = p->pos_vect;
       while(pv != NULL) {
             ODL_WRITE("  OBJECT            = Pos_Vect_Rec\n", "");
             ODL_WRITE("    PV.POS            = ( %22.15f, ", pv->pos[0] );
             ODL_WRITE(" %22.15f,", pv->pos[1] );
             ODL_WRITE(" %22.15f )\n", pv->pos[2] );
             ODL_WRITE("    PV.VEL            = ( %22.15f, ", pv->vel[0] );
             ODL_WRITE(" %22.15f,", pv->vel[1] );
             ODL_WRITE(" %22.15f )\n", pv->vel[2] );
             ODL_WRITE("  END_OBJECT        = Pos_Vect_Rec\n", "");
	     pv = pv->next;
       }

       ODL_WRITE("END_OBJECT        = Pos_Data\n", "");

    if (p->next != (Pos_Data *) NULL ) PP_to_ODL( p->next );
    return(TRUE);
}

int ATT_to_ODL( Att_Data * a ) 
{
   Att_Vect_Rec *av;

   if (a == (Att_Data*) NULL) return(FALSE);

       ODL_WRITE("OBJECT            = Att_Data\n", "");
       ODL_WRITE("  SEQ_NUM           = %d\n", a->seq_num );
       ODL_WRITE("  NPOINT            = %d\n", a->npoint );
       av = a->att_vect;
       while (av!=NULL) {
          ODL_WRITE("  OBJECT            = Att_Vect_Rec\n", "");
          ODL_WRITE("    GMT_DAY           = %d\n", av->gmt_day );
          ODL_WRITE("    GMT_MSEC          = %ld\n", av->gmt_msec );
          ODL_WRITE("    PITCH_FLAG        = %d\n", av->pitch_flag );
          ODL_WRITE("    ROLL_FLAG         = %d\n", av->roll_flag );
          ODL_WRITE("    YAW_FLAG          = %d\n", av->yaw_flag );
          ODL_WRITE("    PITCH             = %14.6f\n", av->pitch );
          ODL_WRITE("    ROLL              = %14.6f\n", av->roll );
          ODL_WRITE("    YAW               = %14.6f\n", av->yaw );
          ODL_WRITE("    PITCH_RATE_FLAG   = %d\n", av->pitch_rate_flag );
          ODL_WRITE("    ROLL_RATE_FLAG    = %d\n", av->roll_rate_flag );
          ODL_WRITE("    YAW_RATE_FLAG     = %d\n", av->yaw_rate_flag );
          ODL_WRITE("    PITCH_RATE        = %14.6f\n", av->pitch_rate );
          ODL_WRITE("    ROLL_RATE         = %14.6f\n", av->roll_rate );
          ODL_WRITE("    YAW_RATE          = %14.6f\n", av->yaw_rate );
          ODL_WRITE("  END_OBJECT        = Att_Vect_Rec\n", "");
          av=av->next;
       }
       ODL_WRITE("END_OBJECT        = Att_Data\n", "");

    if (a->next != (Att_Data *) NULL) ATT_to_ODL( a->next);
    return(TRUE);
}

int RD_to_ODL( Radi_Data* r ) 
{
    int i;

    if (r == (Radi_Data*) NULL) return(FALSE);

       ODL_WRITE("OBJECT             = Radi_Data\n", "");
       ODL_WRITE("  SEQ_NUM            = %d\n", r->seq_num );
       ODL_WRITE("  N_DATA             = %d\n", r->n_data );
       ODL_WRITE("  FIELD_SIZE         = %ld\n", r->field_size );
       ODL_WRITE("  CHAN_IND           = \"%s\"\n", r->chan_ind );
       ODL_WRITE("  TABLE_DESIG        = \"%s\"\n", r->table_desig );
       ODL_WRITE("  N_SAMP             = %ld\n", r->n_samp );
       ODL_WRITE("  SAMP_TYPE          = \"%s\"\n", r->samp_type );
       ODL_WRITE("  NOISE_FACT         = %16.7f\n", r->noise_fact );
       ODL_WRITE("  LINEAR_CONV_FACT   = %16.7f\n", r->linear_conv_fact );
       ODL_WRITE("  OFFSET_CONV_FACT   = %16.7f\n", r->offset_conv_fact );

       for (i=0; i<r->n_samp; i++) {
          if ( !i ) {
	     ODL_WRITE("  LOOKUP_TAB         = (%16.7f",r->lookup_tab[i]);
	  }
	  else {
	     if ( !(i%4) ) {
	        ODL_WRITE(",\n                        ", "");
		ODL_WRITE("%16.7f",r->lookup_tab[i]);
	     }
	     else 
	        ODL_WRITE(", %16.7f",r->lookup_tab[i]);
	  }
	  if ( i==(r->n_samp-1) ) ODL_WRITE(" )\n",  "" );
       }
       ODL_WRITE("END_OBJECT        = Radi_Data\n", "");

    if (r->next != (Radi_Data *) NULL) RD_to_ODL( r->next );
    return(TRUE);
}

int RC_to_ODL( Radi_Comp* r ) 
{
    Rad_Comp_Set *rc;
    Radio_Comp_Tbl *rt;


    if (r == (Radi_Comp*) NULL) return(FALSE);

       ODL_WRITE("OBJECT             = Radi_Comp\n", "");
       ODL_WRITE("  SEQ_NUM            = %d\n", r->seq_num );
       ODL_WRITE("  SAR_CHAN           = %d\n", r->sar_chan);
       ODL_WRITE("  N_DSET             = %ld\n", r->n_dset);
       ODL_WRITE("  DSET_SIZE          = %ld\n", r->dset_size);
       
       rc = r->set;
       while (rc!=NULL) {
           ODL_WRITE("  OBJECT                = Rad_Comp_Set\n", "");
           ODL_WRITE("    COMP_DATA_TYPE        = \"%s\"\n", rc->comp_data_type);
           ODL_WRITE("    DATA_DESCR            = \"%s\"\n", rc->data_descr);
           ODL_WRITE("    REQ_RECS              = %d\n", rc->req_recs);
           ODL_WRITE("    TABLE_SEQ_NUM         = %d\n", rc->table_seq_num);
           ODL_WRITE("    NUM_PAIRS             = %ld\n", rc->num_pairs);
           ODL_WRITE("    FIRST_PIXEL           = %ld\n", rc->first_pixel);
           ODL_WRITE("    LAST_PIXEL            = %ld\n", rc->last_pixel);
           ODL_WRITE("    PIXEL_SIZE            = %ld\n", rc->pixel_size);
           ODL_WRITE("    MIN_SAMP_INDEX        = %f\n", rc->min_samp_index);
           ODL_WRITE("    MIN_COMP_VALUE        = %f\n", rc->min_comp_value);
           ODL_WRITE("    MAX_SAMP_INDEX        = %f\n", rc->min_samp_index);
           ODL_WRITE("    MAX_COMP_VALUE        = %f\n", rc->max_comp_value);
           ODL_WRITE("    N_TABLE_ENTRIES       = %ld\n", rc->n_table_entries);

           rt = rc->tbl;
           while (rt!=NULL) {
               ODL_WRITE("    OBJECT             = Radio_Comp_Tbl\n", "");
               ODL_WRITE("      SAMPLE_OFFSET    = %16.7f\n", rt->sample_offset);
               ODL_WRITE("      SAMPLE_GAIN      = %16.7f\n", rt->sample_gain);
               ODL_WRITE("    END_OBJECT         = Radio_Comp_Tbl\n", "");
               rt=rt->next;
           }
           ODL_WRITE("  END_OBJECT                = Rad_Comp_Set\n", "");
           rc = rc->next;
       }
       ODL_WRITE("END_OBJECT         = Radi_Comp\n", "");

    if (r->next != (Radi_Comp *) NULL ) RC_to_ODL( r->next ) ;
    return(TRUE);
}


int DQS_to_ODL( Qual_Sum * q )
{
    int i;

    if (q == (Qual_Sum*) NULL) return(FALSE);

       ODL_WRITE("OBJECT             = Qual_Sum\n", "");
       ODL_WRITE("  SEQ_NUM            = %d\n", q->seq_num );
       ODL_WRITE("  CHAN_IND           = \"%s\"\n", q->chan_ind );
       ODL_WRITE("  CALI_DATE          = \"%s\"\n", q->cali_date );
       ODL_WRITE("  NCHN               = %d\n", q->nchn );
       ODL_WRITE("  ISLR               = %16.7f\n", q->islr );
       ODL_WRITE("  PSLR               = %16.7f\n", q->pslr );
       ODL_WRITE("  AZI_AMBIG          = %16.7f\n", q->azi_ambig );
       ODL_WRITE("  RNG_AMBIG          = %16.7f\n", q->rng_ambig );
       ODL_WRITE("  SNR                = %16.7f\n", q->snr );
       ODL_WRITE("  BER                = %e\n", q->ber );
       ODL_WRITE("  RNG_RES            = %16.7f\n", q->rng_res );
       ODL_WRITE("  AZI_RES            = %16.7f\n", q->azi_res );
       ODL_WRITE("  RAD_RES            = %16.7f\n", q->rad_res );
       ODL_WRITE("  DYN_RNG            = %16.7f\n", q->dyn_rng );
       ODL_WRITE("  ABS_RAD_UNC_DB     = %16.7f\n", q->abs_rad_unc_db );
       ODL_WRITE("  ABS_RAD_UNC_DEG    = %16.7f\n", q->abs_rad_unc_deg );
  
       for (i=0; i<q->nchn; i++) {
           if (!i) {
	      ODL_WRITE("  OBJECT             = rel_rad_unc\n", "");
	      ODL_WRITE("    REL_RAD_UNC.DB     = ( %16.7f", q->rel_rad_unc[0][i] );     
	   }
	   else {
	      if ( !(i%4) ){
	         ODL_WRITE(", \n                       ", "");
		 ODL_WRITE("%16.7f", q->rel_rad_unc[0][i] );
	      }
	      else
	         ODL_WRITE(", %16.7f", q->rel_rad_unc[0][i] );
	   }
	   if (i==(q->nchn-1)) ODL_WRITE(" )\n", "" );
       }
       for (i=0; i<q->nchn; i++) {
           if (!i) {
	      ODL_WRITE("    REL_RAD_UNC.DEG    = ( %16.7f", q->rel_rad_unc[1][i] );     
	   }
	   else {
	      if ( !(i%4) ) {
	         ODL_WRITE(", \n                       ", "");
		 ODL_WRITE("%16.7f", q->rel_rad_unc[1][i] );
	      }
	      else
	         ODL_WRITE(", %16.7f", q->rel_rad_unc[1][i] );
	   }
	   if (i==(q->nchn-1)) ODL_WRITE(" )\n", "" );
       }
       if (q->nchn)  ODL_WRITE("  END_OBJECT         = rel_rad_unc\n", "");

       ODL_WRITE("  ALT_LOCERR         = %16.7f\n", q->alt_locerr );
       ODL_WRITE("  CRT_LOCERR         = %16.7f\n", q->crt_locerr );
       ODL_WRITE("  ALT_SCALE          = %16.7f\n", q->alt_scale );
       ODL_WRITE("  CRT_SCALE          = %16.7f\n", q->crt_scale );
       ODL_WRITE("  DIS_SKEW           = %16.7f\n", q->dis_skew );
       ODL_WRITE("  ORI_ERR            = %16.7f\n", q->ori_err );

       for (i=0; i<q->nchn; i++) {
           if (!i) {
	      ODL_WRITE("  OBJECT             = misreg\n", "");
	      ODL_WRITE("    MISREG ALT_M       = ( %16.7f", q->misreg[0][i] );
	   }
	   else {
	      if ( !(i%4) ) {
	         ODL_WRITE(",\n                       ", "");
		 ODL_WRITE("%16.7f", q->misreg[0][i] );
	      }
	      else
                 ODL_WRITE(", %16.7f", q->misreg[0][i] );
	   }
	   if (i==(q->nchn-1)) ODL_WRITE(" )\n", "" );
       }
       
       for (i=0; i<q->nchn; i++) {
           if (!i) {
	      ODL_WRITE("    MISREG CRT_M       = ( %16.7f", q->misreg[1][i] );
	   }
	   else {
	      if ( !(i%4) ) {
	         ODL_WRITE(",\n                       ", "");
		 ODL_WRITE("%16.7f", q->misreg[1][i] );
	      }
	      else 
                 ODL_WRITE(", %16.7f", q->misreg[1][i] );
	   }
	   if (i==(q->nchn-1)) ODL_WRITE(" )\n", "" );
       }
       if (q->nchn)  ODL_WRITE("  END_OBJECT         = misreg\n", "");
#ifndef PRE_RADARSAT
       ODL_WRITE("  NESZ               = %16.7f\n", q->nesz );
       ODL_WRITE("  ENL                = %16.7f\n", q->enl );
       ODL_WRITE("  TB_UPDATE          = \"%s\"\n", q->tb_update );
       ODL_WRITE("  CAL_STATUS         = \"%s\"\n", q->cal_status );
       ODL_WRITE("  CAL_COMMENT        = \"%s\"\n", q->cal_comment );
#endif       
       ODL_WRITE("END_OBJECT         = Qual_Sum\n", "");

    if (q->next != (Qual_Sum *) NULL) DQS_to_ODL( q->next );
    return(TRUE);
}

int DH_to_ODL( Data_Hist* h)
{
    Hist_Data_Set *ht;
    int i;

    if (h == (Data_Hist*) NULL) return(FALSE);

       ODL_WRITE("OBJECT             = Data_Hist\n", "");
       ODL_WRITE("  SEQ_NUM            = %d\n", h->seq_num );
       ODL_WRITE("  SAR_CHAN           = %d\n", h->sar_chan );
       ODL_WRITE("  NTAB               = %ld\n", h->ntab );
       ODL_WRITE("  LTAB               = %ld\n", h->ltab );
 
       ht=h->data_set;
       while (ht!=NULL) {

           ODL_WRITE("  OBJECT             = Hist_Data_Set\n", "");
           ODL_WRITE("    HIST_DESC          = \"%s\"\n", ht->hist_desc );
           ODL_WRITE("    NREC               = %d\n", ht->nrec );
           ODL_WRITE("    TAB_SEQ            = %d\n", ht->tab_seq );
           ODL_WRITE("    NBIN               = %ld\n", ht->nbin );
           ODL_WRITE("    NS_LIN             = %ld\n", ht->ns_lin );
           ODL_WRITE("    NS_PIX             = %ld\n", ht->ns_pix );
           ODL_WRITE("    NGRP_LIN           = %ld\n", ht->ngrp_lin );
           ODL_WRITE("    NGRP_PIX           = %ld\n", ht->ngrp_pix );
           ODL_WRITE("    NSAMP_LIN          = %ld\n", ht->nsamp_lin );
           ODL_WRITE("    NSAMP_PIX          = %ld\n", ht->nsamp_pix );
           ODL_WRITE("    MIN_SMP            = %16.7f\n", ht->min_smp );
           ODL_WRITE("    MAX_SMP            = %16.7f\n", ht->max_smp );
           ODL_WRITE("    MEAN_SMP           = %16.7f\n", ht->mean_smp );
           ODL_WRITE("    STD_SMP            = %16.7f\n", ht->std_smp );
           ODL_WRITE("    SMP_INC            = %16.7f\n", ht->smp_inc );
           ODL_WRITE("    MIN_HIST           = %16.7f\n", ht->min_hist );
           ODL_WRITE("    MAX_HIST           = %16.7f\n", ht->max_hist );
           ODL_WRITE("    MEAN_HIST          = %16.7f\n", ht->mean_hist );
           ODL_WRITE("    STD_HIST           = %16.7f\n", ht->std_hist );
           ODL_WRITE("    NHIST              = %ld\n", ht->nhist );

           for (i=0; i<ht->nhist; i++) {
	       if (!i) ODL_WRITE("    DATA_VALUES_HIST   = ( %ld", ht->data_values_hist[i] );
	       else {
	          if (!(i%4)) {
		     ODL_WRITE(",\n                           ", "");
		     ODL_WRITE("%ld", ht->data_values_hist[i] );
		  }
		  else
                     ODL_WRITE(", %ld", ht->data_values_hist[i] );
	       }
	       if ( i==(ht->nhist-1) ) ODL_WRITE(" )\n",  "");
           }
           ODL_WRITE("  END_OBJECT         = Hist_Data_Set\n", "" );
           ht=ht->next;
       }
       ODL_WRITE("END_OBJECT         = Data_Hist\n", "" );

    if (h->next != (Data_Hist *) NULL) DH_to_ODL( h->next );
    return(TRUE);

}

int RS_to_ODL( Rng_Spec* r)
{
    int i;

    if (r == (Rng_Spec*) NULL) return(FALSE);

       ODL_WRITE("OBJECT           = Rng_Spec\n", "");
       ODL_WRITE("  SEQ_NUM          = %d\n", r->seq_num );
       ODL_WRITE("  SAR_CHAN         = %d\n", r->sar_chan );
       ODL_WRITE("  N_DSET           = %ld\n", r->n_dset );
       ODL_WRITE("  DSET_SIZE        = %ld\n", r->dset_size );
       ODL_WRITE("  REQ_RECS         = %d\n", r->req_recs );
       ODL_WRITE("  TABLE_NO         = %d\n", r->table_no );
       ODL_WRITE("  N_PIXELS         = %ld\n", r->n_pixels );
       ODL_WRITE("  PIXEL_OFFSET     = %ld\n", r->pixel_offset );
       ODL_WRITE("  N_LINES          = %ld\n", r->n_lines );
       ODL_WRITE("  FIRST_FREQ       = %16.7f\n", r->first_freq );
       ODL_WRITE("  LAST_FREQ        = %16.7f\n", r->last_freq );
       ODL_WRITE("  MIN_POWER        = %16.7f\n", r->min_power );
       ODL_WRITE("  MAX_POWER        = %16.7f\n", r->max_power );
       ODL_WRITE("  N_BINS           = %ld\n", r->n_bins );

       for (i=0; i<r->n_bins; i++) {
	   if (!i) ODL_WRITE("  DATA_VALUES_SPEC = ( %16.7f", r->data_values_spec[i] );
	   else {
	       if (!(i%4)) {
	          ODL_WRITE(",\n                  ", "");
		  ODL_WRITE("%16.7f", r->data_values_spec[i] );
	       }
	       else
                  ODL_WRITE(", %16.7f", r->data_values_spec[i] );
	   }
	   if ( i==(r->n_bins-1) ) ODL_WRITE(" )\n",  "");
       }

       ODL_WRITE("END_OBJECT     = Rng_Spec\n", "" );

    if (r->next != (Rng_Spec *) NULL ) RS_to_ODL( r->next );
    return(TRUE);

}

int DE_to_ODL( Digital_Elev* e) 
{
    Dem_Desc *set;
    Corner_Pts *pts;
    int i;

    if (e == (Digital_Elev*) NULL) return(FALSE);

       ODL_WRITE("OBJECT             = Digital_Elev\n", "");
       ODL_WRITE("  SEQ_NUM            = %d\n", e->seq_num );
       ODL_WRITE("  TTL_NUM_SETS       = %ld\n", e->ttl_num_sets );
       ODL_WRITE("  DEM_SEQ_NUM        = %d\n", e->DEM_seq_num );
       ODL_WRITE("  SOURCE_DEM         = \"%s\"\n", e->source_DEM );
       ODL_WRITE("  HT_REF_NAME        = \"%s\"\n", e->HT_ref_name );
       ODL_WRITE("  GEN_METHOD         = \"%s\"\n", e->gen_method );
       ODL_WRITE("  RASTER_UNIT        = \"%s\"\n", e->raster_unit );
       ODL_WRITE("  PRESENTATION_PROJ  = \"%s\"\n", e->presentation_proj );
       ODL_WRITE("  NS_RASTER          = %16.7f\n", e->NS_raster );
       ODL_WRITE("  EW_RASTER          = %16.7f\n", e->EW_raster );
       ODL_WRITE("  RESAMPLE           = \"%s\"\n", e->resample );
       ODL_WRITE("  HEIGHT_ERR         = %16.7f\n", e->height_err );
       ODL_WRITE("  NS_LOC_ERR         = %16.7f\n", e->NS_loc_err );
       ODL_WRITE("  EW_LOC_ERR         = %16.7f\n", e->EW_loc_err );
       ODL_WRITE("  MAX_HEIGHT         = %16.7f\n", e->max_height );
       ODL_WRITE("  MIN_HEIGHT         = %16.7f\n", e->min_height );
       ODL_WRITE("  MEAN_HEIGHT        = %16.7f\n", e->MEAN_height );
       ODL_WRITE("  STD_HEIGHT         = %16.7f\n", e->STD_height );
       ODL_WRITE("  NUM_POLYS          = %d\n", e->num_polys );

       set= e->set;
       while (set!=NULL) {
           ODL_WRITE("  OBJECT             = Dem_Desc\n", "");
           ODL_WRITE("    POLY_SEQ_NUM     = %d\n", set->poly_seq_num );
           ODL_WRITE("    NUM_CRNR_PTS     = %d\n", set->num_crnr_pts );

           pts = set->pts ;
           i=1;
           while (pts!=NULL) {
               ODL_WRITE("    OBJECT             = Corner_Pts\n", "" );
               ODL_WRITE("      CP_LAT_1         = %16.7f\n", pts->cp_lat_1 );
               ODL_WRITE("      CP_LON_1         = %16.7f\n", pts->cp_lon_1 );
               ODL_WRITE("    END_OBJECT         = Corner_Pts\n", "" );
               pts=pts->next;
               i++;
           }
           set=set->next;
           ODL_WRITE("  END_OBJECT         = Dem_Desc\n", "");
       }

       ODL_WRITE("END_OBJECT       = Digital_Elev\n", "" );

    if (e->next != (Digital_Elev *) NULL) DE_to_ODL( e->next );
    return(TRUE);
}

int DP_to_ODL( Proc_Parm* e) 
{
    Beam_Info *bi;
    Pix_Count *pc;
    Temp_Rec *tr;
    Dopcen_Est *de;
    SRGR_Coefset *sc;
    int i;

    if (e == (Proc_Parm*) NULL) return(FALSE);

       ODL_WRITE("OBJECT             = Proc_Parm\n", "");
       ODL_WRITE("  SEQ_NUM            = %d\n", e->seq_num );
       ODL_WRITE("  SEQ NUM        = %d\n", e->seq_num);
       ODL_WRITE("  SPARE_DPP_1     = %s\n", e->spare_dpp_1);
       ODL_WRITE("  INP_MEDIA      = %s\n", e->inp_media);
       ODL_WRITE("  N_TAPE_ID      = %d\n", e->n_tape_id);
       for (i=0; i<10; i++) {
	   if (!i) 
              ODL_WRITE("  TAPE_ID        = ( %s", e->tape_id[i]);
	   else {
	       if (!(i%4)) {
                  ODL_WRITE(",\n                  ", "");
                  ODL_WRITE("%s", e->tape_id[i] );
	       }
	       else
                  ODL_WRITE(", %s", e->tape_id[i] );
	   }
	   if ( i==9 )   
              ODL_WRITE(" )\n",  "");
       }
       ODL_WRITE("  EXP_ING_START  = %s\n", e->exp_ing_start);
       ODL_WRITE("  EXP_ING_STOP   = %s\n", e->exp_ing_stop);
       ODL_WRITE("  ACT_ING_START  = %s\n", e->act_ing_start);
       ODL_WRITE("  ACT_ING_STOP   = %s\n", e->act_ing_stop);
       ODL_WRITE("  PROC_START     = %s\n", e->proc_start); 
       ODL_WRITE("  PROC_STOP      = %s\n", e->proc_stop);
       for (i=0;i<10; i++) {
	   if (!i) 
              ODL_WRITE("  MN_SIG_LEV     = ( %16.7f", e->mn_sig_lev[i]);
	   else {
	       if (!(i%4)) {
                  ODL_WRITE(",\n                  ", "");
                  ODL_WRITE("%16.7f", e->mn_sig_lev[i] );
	       }
	       else
                  ODL_WRITE(", %16.7f", e->mn_sig_lev[i] );
	   }
	   if ( i==9 )   
              ODL_WRITE(" )\n",  "");
       }
       ODL_WRITE("  SRC_DATA_IND   = %d\n", e->src_data_ind);
       ODL_WRITE("  MISS_LN        = %ld\n", e->miss_ln);
       ODL_WRITE("  REJ_LN         = %ld\n", e->rej_ln); 
       ODL_WRITE("  LARGE_GAP      = %ld\n", e->large_gap); 
       ODL_WRITE("  BIT_ERROR_RATE = %16.7f\n", e->bit_error_rate);
       ODL_WRITE("  FM_CRC_ERR     = %16.7f\n", e->fm_crc_err);
       ODL_WRITE("  DATE_INCONS    = %ld\n", e->date_incons);
       ODL_WRITE("  PRF_CHANGES    = %ld\n", e->prf_changes);
       ODL_WRITE("  DELAY_CHANGES  = %ld\n", e->delay_changes);
       ODL_WRITE("  SKIPD_FRAMS    = %ld\n", e->skipd_frams); 
       ODL_WRITE("  REJ_BF_START   = %ld\n", e->rej_bf_start); 
       ODL_WRITE("  REJ_FEW_FRAM   = %ld\n", e->rej_few_fram); 
       ODL_WRITE("  REJ_MANY_FRAM  = %ld\n", e->rej_many_fram);
       ODL_WRITE("  REJ_MCHN_ERR   = %ld\n", e->rej_mchn_err); 
       ODL_WRITE("  REJ_VCHN_ERR   = %ld\n", e->rej_vchn_err);
       ODL_WRITE("  REJ_REC_TYPE   = %ld\n", e->rej_rec_type); 
       ODL_WRITE("  PRD_QUAL_IND   = %ld\n", e->prd_qual_ind);
       ODL_WRITE("  QC_RATING      = %s\n", e->qc_rating); 
       ODL_WRITE("  QC_COMMENT     = %s\n", e->qc_comment);
       ODL_WRITE("  SENS_CONFIG    = %s\n", e->sens_config); 
       ODL_WRITE("  SENS_ORIENT    = %s\n", e->sens_orient);
       ODL_WRITE("  SYCH_MARKER    = %s\n", e->sych_marker);
       ODL_WRITE("  RNG_REF_SRC    = %s\n", e->rng_ref_src);
       for (i=0;i<4; i++) {
	   if (!i) 
              ODL_WRITE("  RNG_AMP_COEF   = ( %16.7f", e->rng_amp_coef[i]);
	   else {
              ODL_WRITE(", %16.7f", e->rng_amp_coef[i] );
	   }
	   if ( i==3 )   
              ODL_WRITE(" )\n",  "");
       }
       for (i=0;i<4; i++) {
	   if (!i) 
              ODL_WRITE("  RNG_PHAS_COEF  = ( %16.7f", e->rng_phas_coef[i]);
	   else {
              ODL_WRITE(", %16.7f", e->rng_phas_coef[i] );
	   }
	   if ( i==3 )   
              ODL_WRITE(" )\n",  "");
       }
       for (i=0;i<4; i++) {
	   if (!i) 
              ODL_WRITE("  ERR_AMP_COEF   = ( %16.7f", e->err_amp_coef[i]);
	   else {
              ODL_WRITE(", %16.7f", e->err_amp_coef[i] );
	   }
	   if ( i==3 )   
              ODL_WRITE(" )\n",  "");
       }
       for (i=0;i<4; i++) {
	   if (!i) 
              ODL_WRITE("  ERR_PHAS_COEF  = ( %16.7f", e->err_phas_coef[i]);
	   else {
                  ODL_WRITE(", %16.7f", e->err_phas_coef[i] );
	   }
	   if ( i==3 )   
              ODL_WRITE(" )\n",  "");
       }
       ODL_WRITE("  PULSE_BANDW    = %ld\n", e->pulse_bandw); 
       ODL_WRITE("  ADC_SAMP_RATE  = %s\n", e->adc_samp_rate);
       ODL_WRITE("  REP_AGC_ATTN   = %16.7f\n", e->rep_agc_attn);
       ODL_WRITE("  GN_CORCTN_FCTR = %16.7f\n", e->gn_corctn_fctr);
       ODL_WRITE("  REP_ENERGY_GN  = %16.7f\n", e->rep_energy_gn);
       ODL_WRITE("  ORB_DATA_SRC   = %s\n", e->orb_data_src);
       ODL_WRITE("  PULSE_CNT_1    = %ld\n", e->pulse_cnt_1);
       ODL_WRITE("  PULSE_CNT_2    = %ld\n", e->pulse_cnt_2);
       ODL_WRITE("  BEAM_EDGE_RQD  = %s\n", e->beam_edge_rqd);
       ODL_WRITE("  BEAM_EDGE_CONF = %16.7f\n", e->beam_edge_conf);
       ODL_WRITE("  PIX_OVERLAP    = %ld\n", e->pix_overlap);
       ODL_WRITE("  N_BEAMS        = %ld\n", e->n_beams);
    
       bi = e->beam_info;
       while ( bi != NULL) {
           ODL_WRITE("  OBJECT         = Beam_Info\n", "");
           ODL_WRITE("    BEAM_TYPE      = %s\n", bi->beam_type);
           ODL_WRITE("    BEAM_LOOK_SRC  = %s\n", bi->beam_look_src);
           ODL_WRITE("    BEAM_LOOK_ANG  = %16.7f\n", bi->beam_look_ang);
           ODL_WRITE("    PRF            = %16.7f\n", bi->prf);
           ODL_WRITE("  END_OBJECT     = Beam_Info\n", "");
	   bi =  bi->next;
       }
    
       ODL_WRITE("  N_PIX_UPDATES  = %ld\n", e->n_pix_updates);
       pc = e->pix_count;
       while ( pc != NULL
       ) {
           ODL_WRITE("  OBJECT         = Pix_Count\n", "");
           ODL_WRITE("    PIX_UPDATE     = %s\n", pc->pix_update);
	   for (i=0;i<4;i++) {
	       if (!i) 
                  ODL_WRITE("    N_PIX         = ( %ld", pc->n_pix[i]);
	       else {
                  ODL_WRITE(", %ld", pc->n_pix[i] );
	       }
	       if ( i==3 )   
                 ODL_WRITE(" )\n",  "");
           }
           ODL_WRITE("  END_OBJECT     = Pix_Count\n", "");
	   pc = pc->next;
       }
       ODL_WRITE("  PWIN_START     = %16.7f\n", e->pwin_start);
       ODL_WRITE("  PWIN_END       = %16.7f\n", e->pwin_end);
       ODL_WRITE("  RECD_TYPE      = %s\n", e->recd_type);
       ODL_WRITE("  TEMP_SET_INC   = %16.7f\n", e->temp_set_inc);
    
       ODL_WRITE("  N_TEMP_SET     = %ld\n", e->n_temp_set);
       tr = e->temp;
       while ( tr != NULL) {
           ODL_WRITE("  OBJECT         = Temp_Rec\n", "");
           for (i=0;i<4;i++) {
	       if (!i) 
                  ODL_WRITE("    TEMP_SET      = ( %ld", tr->temp_set[i]);
	       else {
                  ODL_WRITE(", %ld", tr->temp_set[i] );
	       }
	       if ( i==3 )   
                 ODL_WRITE(" )\n",  "");
           }
           ODL_WRITE("  END_OBJECT     = Temp_Rec\n", "");
	   tr= tr->next;
       }
    
       ODL_WRITE("  N_IMAGE_PIX    = %ld\n", e->n_image_pix); 
       ODL_WRITE("  PRC_ZERO_PIX   = %16.7f\n", e->prc_zero_pix); 
       ODL_WRITE("  PRC_SATUR_PIX  = %16.7f\n", e->prc_satur_pix); 
       ODL_WRITE("  IMG_HIST_MEAN  = %16.7f\n", e->img_hist_mean); 

       for (i=0;i<3;i++) {
	   if (!i) 
              ODL_WRITE("  IMG_CUMU_DIST  = ( %16.7f", e->err_phas_coef[i]);
	   else {
              ODL_WRITE(", %16.7f", e->err_phas_coef[i] );
	   }
	   if ( i==2 )   
              ODL_WRITE(" )\n",  "");
       }

       ODL_WRITE("  PRE_IMG_GN     = %16.7f\n", e->pre_img_gn); 
       ODL_WRITE("  POST_IMG_GN    = %16.7f\n", e->post_img_gn); 
       ODL_WRITE("  DOPCEN_INC     = %16.7f\n", e->dopcen_inc); 
    
       ODL_WRITE("  N_DOPCEN       = %ld\n", e->n_dopcen);
       de = e->dopcen_est;
       while ( de != NULL) {
           ODL_WRITE("  OBJECT         = Dopcen_Est\n", "");
           ODL_WRITE("    DOPCEN_CONF    = %16.7f\n", de->dopcen_conf);
           ODL_WRITE("    DOPCEN_REF_TIM = %16.7f\n", de->dopcen_ref_tim);
           for (i=0;i<4;i++) {
	       if (!i) 
                  ODL_WRITE("    DOPCEN_COEF  = ( %16.7f", de->dopcen_coef[i]);
	       else {
                  ODL_WRITE(", %16.7f", de->dopcen_coef[i] );
	       }
	       if ( i==3 )   
                 ODL_WRITE(" )\n",  "");
           }
           ODL_WRITE("  END_OBJECT     = Dopcen_Est\n", "");
	   de = de->next;
       }

       ODL_WRITE("  DOPAMB_ERR     = %ld\n", e->dopamb_err);
       ODL_WRITE("  DOPAMB_CONF    = %16.7f\n", e->dopamb_conf); 

       for (i=0;i<7;i++) {
	   if (!i) 
              ODL_WRITE("  EPH_ORB_DATA   = ( %16.7f", e->eph_orb_data[i]);
	   else {
	       if (!(i%4)) {
                  ODL_WRITE(",\n                  ", "");
                  ODL_WRITE("%16.7f", e->eph_orb_data[i] );
	       }
	       else
                  ODL_WRITE(", %16.7f", e->eph_orb_data[i] );
	   }
	   if ( i==6 )   
              ODL_WRITE(" )\n",  "");
       }

       ODL_WRITE("  APPL_TYPE      = %s\n", e->appl_type); 
       ODL_WRITE("  FIRST_LNTIM    = %22.15f\n", e->first_lntim);
       ODL_WRITE("  LNTIM_INC      = %22.15f\n", e->lntim_inc);
    
       ODL_WRITE("  N_SRGR         = %ld\n", e->n_srgr);
       sc = e->srgr_coefset;
       while( sc != NULL) {
           ODL_WRITE("  OBJECT         = SRGR_Coefset\n", "");
           ODL_WRITE("    SRGR_UPDATE   = %s\n", sc->srgr_update);
           for (i=0;i<6;i++) {
	       if (!i) 
                  ODL_WRITE("    SRGR_COEF      = ( %16.7f", sc->srgr_coef[i]);
	       else {
	          if (!(i%4)) {
                     ODL_WRITE(",\n                  ", "");
                     ODL_WRITE("%16.7f", sc->srgr_coef[i] );
	          }
	          else
                     ODL_WRITE(", %16.7f", sc->srgr_coef[i] );
	       }
	       if ( i==5 )   
                  ODL_WRITE(" )\n",  "");
           }
           ODL_WRITE("  END_OBJECT     = SRGR_Coefset\n", "");
	   sc = sc->next;
       }
    
       ODL_WRITE("  PIXEL_SPACING  = %16.7f\n", e->pixel_spacing);
       ODL_WRITE("  PICS_REQD      = %s\n", e->pics_reqd);
       ODL_WRITE("  WO_NUMBER      = %s\n", e->wo_number); 
       ODL_WRITE("  WO_DATE        = %s\n", e->wo_date);
       ODL_WRITE("  SATELLITE_ID   = %s\n", e->satellite_id); 
       ODL_WRITE("  USER_ID        = %s\n", e->user_id);
       ODL_WRITE("  COMPLETE_MSG   = %s\n", e->complete_msg);
       ODL_WRITE("  SCENE_ID       = %s\n", e->scene_id); 
       ODL_WRITE("  DENSITY_IN     = %s\n", e->density_in); 
       ODL_WRITE("  MEDIA_ID       = %s\n", e->media_id);
       ODL_WRITE("  ANGLE_FIRST    = %16.7f\n", e->angle_first);
       ODL_WRITE("  ANGLE_LAST     = %16.7f\n", e->angle_last);
       ODL_WRITE("  PROD_TYPE      = %s\n", e->prod_type); 
       ODL_WRITE("  MAP_SYSTEM     = %s\n", e->map_system); 
       ODL_WRITE("  CENTRE_LAT     = %22.15f\n", e->centre_lat); 
       ODL_WRITE("  CENTRE_LONG    = %22.15f\n", e->centre_long);  
       ODL_WRITE("  SPAN_X         = %22.15f\n", e->span_x);  
       ODL_WRITE("  SPAN_Y         = %22.15f\n", e->span_y); 
       ODL_WRITE("  APPLY_DTM      = %s\n", e->apply_dtm);
       ODL_WRITE("  DENSITY_OUT    = %s\n", e->density_out); 
       ODL_WRITE("  SPARE_DPP_2    = %s\n", e->spare_dpp_2);
      



       ODL_WRITE("END_OBJECT       = Proc_Parm\n", "" );

    if (e->next != (Proc_Parm *) NULL) DP_to_ODL( e->next );
    return(TRUE);
}




int FR_to_ODL( Fac_Related* r) 
{

    if (r == (Fac_Related*) NULL) return(FALSE);

       ODL_WRITE("OBJECT             = Fac_Related\n", "");
       ODL_WRITE("  SEQ_NUM            = %d\n", r->seq_num );
       ODL_WRITE("  DATATAKE_ID        = \"%s\"\n", r->datatake_ID );
       ODL_WRITE("  IMAGE_ID           = \"%s\"\n", r->image_ID );
       ODL_WRITE("  CORR_YEAR          = \"%s\"\n", r->corr_year );
       ODL_WRITE("  CORR_GMT           = \"%s\"\n", r->corr_GMT );
       ODL_WRITE("  SITE_NAME          = \"%s\"\n", r->site_name );
       ODL_WRITE("  DATA_YEAR          = \"%s\"\n", r->data_year );
       ODL_WRITE("  CENTER_GMT         = \"%s\"\n", r->center_GMT );
       ODL_WRITE("  CENTER_LAT         = %17.7f\n", r->center_LAT );
       ODL_WRITE("  CENTER_LON         = %17.7f\n", r->center_LON );
       ODL_WRITE("  NEAR_START_LAT     = %17.7f\n", r->near_start_LAT );
       ODL_WRITE("  NEAR_START_LON     = %17.7f\n", r->near_start_LON );
       ODL_WRITE("  NEAR_END_LAT       = %17.7f\n", r->near_end_LAT );
       ODL_WRITE("  NEAR_END_LON       = %17.7f\n", r->near_end_LON );
       ODL_WRITE("  FAR_START_LAT      = %17.7f\n", r->far_start_LAT );
       ODL_WRITE("  FAR_START_LON      = %17.7f\n", r->far_start_LON );
       ODL_WRITE("  FAR_END_LAT        = %17.7f\n", r->far_end_LAT );
       ODL_WRITE("  FAR_END_LON        = %17.7f\n", r->far_end_LON );
       ODL_WRITE("  ACTUAL_AZIMUTH     = %17.7f\n", r->actual_azimuth );
       ODL_WRITE("  ACTUAL_RANGE       = %17.7f\n", r->actual_range );
       ODL_WRITE("  ACTUAL_PIXELS      = %ld\n", r->actual_pixels );
       ODL_WRITE("  ACTUAL_LINES       = %ld\n", r->actual_lines );
       ODL_WRITE("  TOTAL_PIXELS       = %ld\n", r->total_pixels );
       ODL_WRITE("  TOTAL_LINES        = %ld\n", r->total_lines );
       ODL_WRITE("  MEDIA_ID           = \"%s\"\n", r->media_id );
       ODL_WRITE("  START_ADDRESS      = %ld\n", r->start_address );
       ODL_WRITE("  END_ADDRESS        = %ld\n", r->end_address );
       ODL_WRITE("  PLATFORM_NAME      = \"%s\"\n", r->platform_name );
       ODL_WRITE("  SENSOR_MODE        = \"%s\"\n", r->sensor_mode );
       ODL_WRITE("  PRF                = %17.7f\n", r->PRF );
       ODL_WRITE("  ANT_LOOK_ANGLE     = %17.7f\n", r->ant_look_angle );
       ODL_WRITE("  DATA_RATE          = %17.7f\n", r->data_rate );
       ODL_WRITE("  DATA_WIN_POS       = %17.7f\n", r->data_win_pos );
       ODL_WRITE("  RANGE_GATE_DEL     = %17.7f\n", r->range_gate_del );
       ODL_WRITE("  TRACK_ANGLE        = %17.7f\n", r->track_angle );
       ODL_WRITE("  ASC_DESC           = \"%s\"\n", r->ASC_DESC );
       ODL_WRITE("  S_C_ALTITUDE       = %17.7f\n", r->S_C_altitude );
       ODL_WRITE("  X_POSITION         = %23.10E\n", r->X_position );
       ODL_WRITE("  Y_POSITION         = %23.10E\n", r->Y_position );
       ODL_WRITE("  Z_POSITION         = %23.10E\n", r->Z_position );
       ODL_WRITE("  X_VELOCITY         = %23.10E\n", r->X_velocity );
       ODL_WRITE("  Y_VELOCITY         = %23.10E\n", r->Y_velocity );
       ODL_WRITE("  Z_VELOCITY         = %23.10E\n", r->Z_velocity );
       ODL_WRITE("  ROLL               = %15.7f\n", r->roll );
       ODL_WRITE("  YAW                = %15.7f\n", r->yaw );
       ODL_WRITE("  PITCH              = %15.7f\n", r->pitch );
       ODL_WRITE("  ROLL_FLAG          = %d\n", r->roll_flag );
       ODL_WRITE("  YAW_FLAG           = %d\n", r->yaw_flag );
       ODL_WRITE("  PITCH_FLAG         = %d\n", r->pitch_flag );
       ODL_WRITE("  ROLL_RATE          = %15.7f\n", r->roll_rate );
       ODL_WRITE("  YAW_RATE           = %15.7f\n", r->yaw_rate );
       ODL_WRITE("  PITCH_RATE         = %15.7f\n", r->pitch_rate );
       ODL_WRITE("  ROLL_RATE_FLAG     = %d\n", r->roll_rate_flag );
       ODL_WRITE("  YAW_RATE_FLAG      = %d\n", r->yaw_rate_flag );
       ODL_WRITE("  PITCH_RATE_FLAG    = %d\n", r->pitch_rate_flag );
       ODL_WRITE("  NADIR_RADIUS       = %17.7f\n", r->nadir_radius );
       ODL_WRITE("  IMAGE_RADIUS       = %17.7f\n", r->image_radius );
       ODL_WRITE("  INCIDENCE_ANGLE    = %17.7f\n", r->incidence_angle );
       ODL_WRITE("  PROC_VERSION       = \"%s\"\n", r->proc_version );
       ODL_WRITE("  PROC_TYPE          = \"%s\"\n", r->proc_type );
       ODL_WRITE("  TYPE_EPHEMERIS     = \"%s\"\n", r->type_ephemeris );
       ODL_WRITE("  LOOKS_AZIMUTH      = %17.7f\n", r->looks_azimuth );
       ODL_WRITE("  LOOKS_RANGE        = %17.7f\n", r->looks_range );
       ODL_WRITE("  AZI_WEIGHT_FAC     = %17.7f\n", r->azi_weight_fac );
       ODL_WRITE("  RANGE_WEIGHT_FAC   = %17.7f\n", r->range_weight_fac );
       ODL_WRITE("  LOOK_ENERGY_EQ     = \"%s\"\n", r->look_energy_eq );
       ODL_WRITE("  INDUCED_AZIMUTH    = %17.7f\n", r->induced_azimuth );
       ODL_WRITE("  INDUCED_RANGE      = %17.7f\n", r->induced_range );
       ODL_WRITE("  GAIN               = %17.7f\n", r->gain );
       ODL_WRITE("  SWATH_VELOCITY     = %17.7f\n", r->swath_velocity );
       ODL_WRITE("  SQUINT_ANGLE       = %17.7f\n", r->squint_angle );
       ODL_WRITE("  AVG_TERRAIN_HT     = %17.7f\n", r->avg_terrain_ht );
       ODL_WRITE("  PROCESSOR_GAIN     = %d\n", r->processor_gain );
       ODL_WRITE("  DESKEW             = \"%s\"\n", r->deskew );
       ODL_WRITE("  GND_SLANT_FLAG     = \"%s\"\n", r->gnd_slant_flag );
       ODL_WRITE("  SL_RNG_1ST_PIX     = %17.7f\n", r->sl_rng_1st_pix );
       ODL_WRITE("  SL_RNG_LAST_PIX    = %17.7f\n", r->sl_rng_last_pix );
       ODL_WRITE("  START_SAMPLE       = %ld\n", r->start_sample );
       ODL_WRITE("  CLUTTERLOCK_FLG    = \"%s\"\n", r->clutterlock_flg );
       ODL_WRITE("  DOP_FRQ_CONST      = %17.7f\n", r->dop_frq_const );
       ODL_WRITE("  DOP_FRQ_SLOPE      = %17.7f\n", r->dop_frq_slope );
       ODL_WRITE("  DOP_FRQ_QUAD       = %17.7f\n", r->dop_frq_quad );
       ODL_WRITE("  AUTOFOCUS_FLAG     = \"%s\"\n", r->autofocus_flag );
       ODL_WRITE("  DOP_FRQ_R_CNST     = %17.7f\n", r->dop_frq_r_cnst );
       ODL_WRITE("  DOP_FRQ_R_SLOPE    = %17.7f\n", r->dop_frq_r_slope );
       ODL_WRITE("  DOP_FRQ_R_QUAD     = %17.7f\n", r->dop_frq_r_quad );
       ODL_WRITE("  AZI_RES            = %17.7f\n", r->azi_res );
       ODL_WRITE("  RNG_RES            = %17.7f\n", r->rng_res );
       ODL_WRITE("  AZIMUTH_PIXEL      = %17.7f\n", r->azimuth_pixel );
       ODL_WRITE("  RANGE_PIXEL        = %17.7f\n", r->range_pixel );
       ODL_WRITE("  OBRC_FLAG          = \"%s\"\n", r->OBRC_flag );
       ODL_WRITE("  BITS_SAMPLE        = %d\n", r->bits_sample );
       ODL_WRITE("  CALIB_EST          = %17.7f\n", r->calib_est );
       ODL_WRITE("  BIT_ERR_RATE       = %17.7f\n", r->bit_err_rate );
       ODL_WRITE("  SNR                = %17.7f\n", r->SNR );
       ODL_WRITE("  EST_NOISE_FLR      = %17.7f\n", r->est_noise_flr );
       ODL_WRITE("  RADIO_M_RESOL      = %17.7f\n", r->radio_m_resol );
       ODL_WRITE("  SATUR_POINTS       = %ld\n", r->satur_points );
       ODL_WRITE("  SPEC_FLAG          = \"%s\"\n", r->spec_flag );
#ifndef PRE_RADARSAT
       ODL_WRITE("  REPL_AGC           = %17.7f\n", r->repl_agc );
       ODL_WRITE("  TEMP_RX_LNA        = %17.7f\n", r->temp_rx_lna );
       ODL_WRITE("  TEMP_RX_SUB        = %17.7f\n", r->temp_rx_sub );
       ODL_WRITE("  TEMP_RX_PROT       = %17.7f\n", r->temp_rx_prot );
       ODL_WRITE("  TEMP_CAL_SYS       = %17.7f\n", r->temp_cal_sys );
       ODL_WRITE("  RX_AGC             = %17.7f\n", r->rx_agc );
       ODL_WRITE("  PRE_CAL1_POW       = %17.7f\n", r->pre_cal1_pow );
       ODL_WRITE("  PRE_CAL2_POW       = %17.7f\n", r->pre_cal2_pow );
       ODL_WRITE("  POST_CAL1_POW      = %17.7f\n", r->post_cal1_pow );
       ODL_WRITE("  POST_CAL2_POW      = %17.7f\n", r->post_cal2_pow );
       ODL_WRITE("  REPL_POW           = %17.7f\n", r->repl_pow );
       ODL_WRITE("  SSAR_ROLL_ANG      = %17.7f\n", r->ssar_roll_ang );
       ODL_WRITE("  COMMENT            = \"%s\"\n", r->comment );
#endif
       ODL_WRITE("END_OBJECT         = Fac_Related\n", "");

    if (r->next != (Fac_Related *) NULL) FR_to_ODL( r->next);
    return(TRUE);
}


