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


static char sccsid_process_odl_c[] =
        "@(#)process_odl.c	1.9 96/10/01 14:02:03";

/* Added by Dan F. */
extern int Debug;
extern int open_ODL_file() ;
extern int process_ODL_file() ;
extern void set_separators() ;
extern int next_head() ;
extern int next_token() ;
extern int get_int() ;
extern int get_float() ;
extern int get_double() ;
extern int get_array() ;


extern FILE *ODL_r_fp;

int Record_count;

typedef	struct
  {
  char	*token;
  int	format_type;
  }
User_token_def;

typedef	struct
  {
  char	*token;
  int  (*func) (SARL_ptr*);
  }
User_func_def;

int Read_FDR_ODL(SARL_ptr *tree);
int Read_ALL_ODL(SARL_ptr *tree);
int Decode_ODL_DSR(SARL_ptr *tree);
int Decode_ODL_MPR(SARL_ptr *tree);
int Decode_ODL_PDR(SARL_ptr *tree);
int Decode_ODL_ADR(SARL_ptr *tree);
int Decode_ODL_RDR(SARL_ptr *tree);
int Decode_ODL_RCR(SARL_ptr *tree);
int Decode_ODL_QSR(SARL_ptr *tree);
int Decode_ODL_DHR(SARL_ptr *tree);
int Decode_ODL_RSR(SARL_ptr *tree);
int Decode_ODL_DER(SARL_ptr *tree);
int Decode_ODL_DPR(SARL_ptr *tree);
int Decode_ODL_FRR(SARL_ptr *tree);


int process_from_odl( void ) {

   SARL_ptr* tree;
   int ret;
   
   /* open the ODL file */
   if ( !( open_ODL_file(CEOS.odl_r_file, "") )) {
      printf("\n Could not open the leader file for reading --- %s",CEOS.odl_r_file);
      return(0);
   }
 
   /* allocate memory for a SARL structure */

   if ( ( tree = Allocate_SARL() ) == NULL) {
      printf("\n Failed to Allocate_SARL");
      return(0);
   }

   SetCurrentSARL( tree );

   /* read the File Descriptor Record, transfer to "descript" structure */

   if ( !Read_FDR_ODL( tree ) ) {
      printf("\n Error in Read_FDR_ODL\n");
      return(0);
   }

   /* based the contents of the ODL file FDR, allocate space for 
      each of record */

   if ( Allocate_SARL_ODR(tree) ) {
      printf("\n Decoding from the ODL file\n");
      ret=Read_ALL_ODL( tree );
   }
   
   fclose(ODL_r_fp);

   return(ret);
}

int Read_FDR_ODL( SARL_ptr* tree ) 
{
    int ret;
    char token[80];
    static int multi;
    char Object_tokens[] = {"Sarl_Desc_Rec"};
    Sarl_Desc_Rec *t = &(tree->descript);
    desc_rec *d = &(t->desc);
    int la_int;
    float la_float;
    double la_double;
    int len;
    static User_token_def token_list[] = {
	{ "ASCII_FLAG",   1 },
	{ "FORMAT_DOC",   1 }, { "FORMAT_REV",   1 }, 
	{ "DESIGN_REV",   1 }, { "SOFTWARE_ID",  1 }, 
	{ "FILE_NUM",     2 }, { "PRODUCT_ID",   1 }, 
	{ "REC_SEQ_FLAG", 1 }, { "SEQ_LOC",      2 }, 
	{ "SEQ_LEN",      2 }, { "REC_CODE",     1 }, 
	{ "CODE_LOC",     2 }, { "CODE_LEN",     2 }, 
	{ "REC_LEN",      1 }, { "RLEN_LOC",     2 }, 
	{ "RLEN_LEN",     2 },
	{ "SPARE_FDR_3",  1 }, 
	{ "N_DATASET",    2 }, { "L_DATASET",    2 }, 
	{ "N_MAP_PROJ",   2 }, { "L_MAP_PROJ",   2 }, 
	{ "N_PLAT_POS",   2 }, { "L_PLAT_POS",   2 }, 
	{ "N_ATT_DATA",   2 }, { "L_ATT_DATA",   2 }, 
	{ "N_RADI_DATA",  2 }, { "L_RADI_DATA",  2 }, 
	{ "N_RADI_COMP",  2 }, { "L_RADI_COMP",  2 }, 
	{ "N_QUAL_SUM",   2 }, { "L_QUAL_SUM",   2 }, 
	{ "N_DATA_HIST",  2 }, { "L_DATA_HIST",  2 }, 
	{ "N_RANG_SPEC",  2 }, { "L_RANG_SPEC",  2 },
	{ "N_DEM_DESC",   2 }, { "L_DEM_DESC",   2 },
	{ "N_RADAR_PAR",  2 }, { "L_RADAR_PAR",  2 },
	{ "N_ANNO_DATA",  2 }, { "L_ANNO_DATA",  2 },
	{ "N_DET_PROC",   2 }, { "L_DET_PROC",   2 },
	{ "N_CAL",        2 }, { "L_CAL",        2 },
	{ "N_GCP",        2 }, { "L_GCP",        2 },
	{ "SPARE_FDR_4",  1 }, { "N_FAC_DATA",   2 },
	{ "L_FAC_DATA",   2 },
	{ "NULL",        NULL }, 
    };
    int i;
    static int nspare4;
    Record_count=1;

    /* First OBJECT token must be "Sarl_Desc_Rec" */
    
    set_separators("= ");
    next_head( token ); 
    if ( strcmp(token, "OBJECT") ) {
       printf("\n Expecting OBJECT as first token - Read_FDR_ODL\n");
       return(0);
    }
    multi++;
    
    next_token( token );
    if ( strcmp(token, "Sarl_Desc_Rec") ) {
       printf("\n Expecting but did not find FDR as first OBJECT\n");
       return(0);
    }
    
    while (multi) {
          if ( !(ret=next_token( token )) ) {
	     printf("\n End of File");
	     return(0);
	  }
	  
	  if ( !(strcmp( token, "END_OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     /* compare END_OBJECT to OBJECT tokens */
	     if (!(strcmp( token, Object_tokens )) ) {
	        multi--;
	     }
	  }
	  else {
	     /* find a match for Object token in token list */
	     i=0;
	     while (strcmp(token_list[i].token, "NULL")) {
		 if ( !(strcmp(token_list[i].token, token)) ) {
		     switch (token_list[i].format_type) {
			 case 1:
			    next_token( token );
			    /* remove quote */
			    len = strlen(token);
			    if (len >2) {
			       strncpy(token, &token[1],  len-2);
			       token[len-2]='\0';
			    }
			    else
			       strcpy(token, "");
			    break;
			 case 2:
			    get_int(&la_int);
			    break;
			 case 3:
			    get_float(&la_float);
			    break;
			 case 4:
			    get_double(&la_double);
			    break;
			 default:
			    break;
		     }
		     switch ( i+1) {
			 case 1:
			    strncpy(t->ascii_flag, token, 2);
			    break;
			 case 2:
			    strncpy(t->format_doc, token, 12);
			    break;
			 case 3:
			    strncpy(t->format_rev, token, 2);
			    break;
			 case 4:
			    strncpy(t->design_rev, token, 2);
			    break;
			 case 5:
			    strncpy(t->software_id, token, 12);
			    break;
			 case 6:
			    t->file_num = la_int;
			    break;
			 case 7:
			    strncpy(t->product_id, token, 16);
			    break;
			 case 8:
			    strncpy(t->rec_seq_flag, token, 4);
			    break;
			 case 9:
			    t->seq_loc = la_int;
			    break;
			 case 10:
			    t->seq_len = la_int;
			    break;
			 case 11:
			    strncpy(t->rec_code, token, 4);
			    break; 
			 case 12:
			    t->code_loc = la_int;
			    break;
			 case 13:
			    t->code_len = la_int;
			    break;
			 case 14:
			    strncpy(t->rec_len, token, 4);
			    break;
			 case 15:
			    t->rlen_loc = la_int;
			    break;
			 case 16:
			    t->rlen_len = la_int;
			    break;
			 case 17:
			    strncpy(t->spare_fdr_3, token, 64);
			    break;
			 case 18:
			    t->n_dataset = la_int;
			    break;
			 case 19:
			    t->l_dataset = la_int;
			    break;
			 case 20:
			    t->n_map_proj = la_int;
			    break;
			 case 21:
			    t->l_map_proj = la_int;
			    break;
			 case 22:
			    t->n_plat_pos = la_int;
			    break;
			 case 23:
			    t->l_plat_pos = la_int;
			    break;
			 case 24:
			    t->n_att_data = la_int;
			    break;
			 case 25:
			    t->l_att_data = la_int;
			    break;
			 case 26:
			    t->n_radi_data = la_int;
			    break;
			 case 27:
			    t->l_radi_data = la_int;
			    break;
			 case 28:
			    t->n_radi_comp = la_int;
			    break;
			 case 29:
			    t->l_radi_comp = la_int;
			    break;
			 case 30:
			    t->n_qual_sum = la_int;
			    break;
			 case 31:
			    t->l_qual_sum = la_int;
			    break;
			 case 32:
			    t->n_data_hist = la_int;
			    break;
			 case 33:
			    t->l_data_hist = la_int;
			    break;
			 case 34:
			    t->n_rang_spec = la_int;
			    break;
			 case 35:
			    t->l_rang_spec = la_int;
			    break;
			 case 36:
			    t->n_dem_desc = la_int;
			    break;
			 case 37:
			    t->l_dem_desc = la_int;
			    break;
			 case 38:
			    t->n_radar_par = la_int;
			    break;
			 case 39:
			    t->l_radar_par = la_int;
			    break;
			 case 40:
			    t->n_anno_data = la_int;
			    break;
			 case 41:
			    t->l_anno_data = la_int;
			    break;
			 case 42:
			    t->n_det_proc = la_int;
			    break;
			 case 43:
			    t->l_det_proc = la_int;
			    break;
			 case 44:
			    t->n_cal = la_int;
			    break;
			 case 45:
			    t->l_cal = la_int;
			    break;
			 case 46:
			    t->n_gcp = la_int;
			    break;
			 case 47:
			    t->l_gcp = la_int;
			    break;
			 case 48:
			    if (nspare4>=11) break;
			    nspare4++;
			    t->spare_fdr_4[nspare4-1] = la_int;
			    break;
			 case 49:
			    t->n_fac_data = la_int;
			    break;
			 case 50:
			    t->l_fac_data = la_int;
			    break;
			 default:
			    break;
		     }
		     break;
		 }
		 i++;
	     }
	  }
    }
    
    /* Fill in the record type info */
    d->rec_seq  = Record_count++;
    d->rec_sub1 = LD1SUB;
    d->rec_type = LDTYPE;
#ifdef PRE_RADARSAT
    d->rec_sub2 = COM2SUB;
#else
    d->rec_sub2 = LD2SUB;
#endif
    d->rec_sub3 = LD3SUB;
    d->length = BUF_FDR_SIZE;
    
    return(1);
}

int Read_ALL_ODL( SARL_ptr* tree )
{
    static User_func_def token_list[] = {
        { "Dataset_Sum",  Decode_ODL_DSR }, 
	{ "Map_Proj",     Decode_ODL_MPR }, 
	{ "Pos_Data",     Decode_ODL_PDR }, 
	{ "Att_Data",     Decode_ODL_ADR }, 
	{ "Radi_Data",    Decode_ODL_RDR }, 
	{ "Radi_Comp",    Decode_ODL_RCR }, 
	{ "Qual_Sum",     Decode_ODL_QSR }, 
	{ "Data_Hist",    Decode_ODL_DHR }, 
	{ "Rng_Spec",     Decode_ODL_RSR }, 
	{ "Digital_Elev", Decode_ODL_DER }, 
	{ "Proc_Parm",    Decode_ODL_DPR }, 
	{ "Fac_Related",  Decode_ODL_FRR }, 
    	{ "NULL",         NULL }, 
    };
    int i;
    char token[80];
    int ret;
    
    while ( (ret=next_token( token )) ) {
        
	if ( strcmp(token, "OBJECT")) {
	   if ( !strcmp(token, "END") ) return(1);
           printf("\n Expecting OBJECT as first token - Read_ALL_ODL\n");
           return(0);
        }
	else {
	   if ( !(ret=next_token( token )) ) {
	      printf("\n End of File");
	      return(0);
	   }
	}

       /* find a match for Object token in token list */
       i=0;
       while (strcmp(token_list[i].token, "NULL")) {
	   if ( !(strcmp(token_list[i].token, token)) ) {
	      if ( (ret=(*token_list[i].func)(tree)) != DECODE_OK) {
		 printf("\n Error decoding ODL file\n");
		 return(0);
	      }
	      break;
	   }
	   i++;
       }
   }  
   return(1);
}

int Decode_ODL_DSR(SARL_ptr *tree)
{
    char Object_tokens[] = {"Dataset_Sum"};
    Sarl_Desc_Rec *t = &(tree->descript);
    Dataset_Sum *ds = tree->data_sum;
    desc_rec *d;
    int n_data = t->n_dataset;
    int la_int;
    float la_float;
    double la_double;
    int len;
    int multi=1;
    static User_token_def token_list[] = {
	{ "SEQ_NUM",         2 }, { "SAR_CHAN",       2 }, 
	{ "PRODUCT_ID",      1 }, { "SCENE_DES",      1 }, 
	{ "INP_SCTIM",       1 }, { "ASC_DES",        1 }, 
	{ "PRO_LAT",         3 }, { "PRO_LONG",       3 }, 
	{ "PRO_HEAD",        3 }, { "ELLIP_DES",      1 }, 
	{ "ELLIP_MAJ",       3 }, { "ELLIP_MIN",      3 }, 
	{ "EARTH_MASS",      3 }, { "GRAV_CONST",     3 }, 
	{ "ELLIP_J",         5 },
        { "TERRAIN_H",       3 }, { "SC_LIN",         2 },
        { "SC_PIX",          2 }, { "SCENE_LEN",      3 },
        { "SCENE_WID",       3 },
	{ "NCHN",            2 },
        { "MISSION_ID",      1 }, { "SENSOR_ID",      1 }, 
	{ "REVOLUTION",      1 }, { "PLAT_LAT",       3 }, 
	{ "PLAT_LONG",       3 }, { "PLAT_HEAD_SCENE",3 }, 
	{ "CLOCK_ANG",       3 }, { "INCIDENT_ANG",   3 }, 
	{ "FREQUENCY",       3 }, { "WAVE_LENGTH",    3 }, 
	{ "MOTION_COMP",     1 }, { "PULSE_CODE",     1 }, 
	{ "AMPL_COEF",       5 }, { "PHAS_COEF",      5 }, 
	{ "CHIRP_EXT_IND",   2 }, 
	{ "RNG_SAMP_RATE",   3 }, { "RNG_GATE",       3 }, 
	{ "RNG_LENGTH",      3 }, { "BASEBAND_F",     1 }, 
	{ "RNGCMP_F",        1 }, { "GN_POLAR",       3 }, 
	{ "GN_CROSS",        3 }, { "CHN_BITS",       2 }, 
	{ "QUANT_DESC",      1 }, { "I_BIAS",         3 }, 
	{ "Q_BIAS",          3 }, { "IQ_RATIO",       3 }, 
	{ "SPARE_DSS_7",     3 }, { "SPARE_DSS_8",    3 }, 
	{ "ELE_SIGHT",       3 }, { "MECH_SIGHT",     3 }, 
	{ "ECHO_TRACK",      1 }, { "PRF",            3 },
        { "ELEV_BEAM",       3 }, { "AZI_BEAM",       3 },
        { "SAT_BINTIM",      1 }, { "SAT_CLKTIM",     1 }, 
        { "SAT_CLKINC",      2 },
	{ "FAC_ID",          1 }, { "SYS_ID",         1 }, 
	{ "VER_ID",          1 }, { "FAC_CODE",       1 }, 
	{ "LEV_CODE",        1 }, { "PRODUCT_TYPE",   1 }, 
	{ "ALGOR_ID",        1 }, { "N_AZILOK",       3 }, 
	{ "N_RNGLOK",        3 }, { "BND_AZILOK",     3 },  
	{ "BND_RNGLOK",      3 }, { "BND_AZI",        3 }, 
	{ "BND_RNG",         3 }, { "AZI_WEIGHT",     1 }, 
	{ "RNG_WEIGHT",      1 }, { "DATA_INPSRC",    1 }, 
	{ "RNG_RES",         3 }, { "AZI_RES",        3 }, 
	{ "RADI_STRETCH",    5 }, { "ALT_DOPCEN",     5 },
        { "CRT_DOPCEN",      5 }, { "TIME_DIR_PIX",   1 }, 
	{ "TIME_DIR_LIN",    1 }, { "ALT_RATE",       5 },
        { "CRT_RATE",        5 }, { "LINE_CONT",      1 }, 
	{ "CLUTTERLOCK_FLG", 1 }, { "AUTO_FOCUS",     1 }, 
	{ "LINE_SPACING",    3 }, { "PIXEL_SPACING",  3 }, 
	{ "RNGCMP_DESG",     1 }, 
#ifdef PRE_RADARSAT
	{ "ANNOT_PTS",       2 }, { "ANNOT_LINE",     5 }, 
	{ "ANNOT_PIXEL",     5 }, { "ANNOT_TEXT",     5 },
#else
	{ "NO_BEAMS",        2 }, 
	{ "BEAM1",           1 }, { "BEAM2",          1 }, 
	{ "BEAM3",           1 }, { "BEAM4",          1 }, 
	{ "PRF1",            3 }, { "PRF2",           3 }, 
	{ "PRF3",            3 }, { "PRF4",           3 }, 
	{ "RNG_GATE1",       3 }, { "RNG_GATE2",      3 }, 
        { "RNG_GATE3",       3 }, { "RNG_GATE4",      3 }, 
	{ "TOT_PLS_BURST",   2 }, { "VAL_PLS_BURST",  2 }, 
	{ "AZ_OVLP_NXT_IMG", 2 }, { "RN_OFF_NXT_IMG", 2 },
        { "CAL_PARAMS_FILE", 1 }, { "SCAN_RESULTS_FILE", 1 },
        { "SCANNER_VERSION", 1 }, { "DECODE_VERSION", 1 },
#endif
	{ "NULL",         NULL } 
    };
    int i=1;
    char token[MAX_LINE];
    int ret;
    
    if (ds == NULL) {
       printf("\nError: File Descriptor Record did not have a Dataset Summary record listed\n");
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (ds->seq_num !=0) {
           if (i<=n_data) {
              ds=ds->next;
           }
           else
              return(DECODE_ERROR);
           i++;
       }
    }

    printf("\n Decode Data Set Summary record \n");
    
    d = &(ds->desc);
    
    while (multi) {
          if ( !(ret=next_token( token )) ) {
	     printf("\n End of File - No token found");
	     return(0);
	  }
	  
	  if ( !(strcmp( token, "END_OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of Object - No token found");
	        return(0);
	     }
	     /* compare END_OBJECT to OBJECT tokens */
	     if (!(strcmp( token, Object_tokens )) ) {
	        multi--;
	     }
	  }
	  else {
	     /* find a match for Object token in token list */
	     i=0;
	     while (strcmp(token_list[i].token, "NULL")) {
		 if ( !(strcmp(token_list[i].token, token)) ) {
		     switch (token_list[i].format_type) {
			 case 1:
			    next_token( token );
			    /* remove quote */
			    len = strlen(token);
			    if (len >2) {
			       strncpy(token, &token[1],  len-2);
			       token[len-2]='\0';
			    }
			    else
			       strcpy(token, "");
			    break;
			 case 2:
			    get_int(&la_int);
			    break;
			 case 3:
			    get_float(&la_float);
			    break;
			 case 4:
			    get_double(&la_double);
			    break;
			 case 5:
			 default: /* arrays handled on an individual basis */
			    break;
		     }
		     switch ( i+1 ) {
			 case 1:
			    ds->seq_num = la_int;
			    break;
			 case 2:
			    ds->sar_chan = la_int;
			    break;
			 case 3:
			    strncpy( ds->product_id,  token, 16 );
			    break;
			 case 4:
			    strncpy( ds->scene_des,  token, 32 );
			    break;
			 case 5:
			    strncpy( ds->inp_sctim,  token, 32 );
			    break;
			 case 6:
			    strncpy( ds->asc_des,  token, 16 );
			    break;
			 case 7:
			    ds->pro_lat = la_float;
			    break;
			 case 8:
			    ds->pro_long = la_float;
			    break;
			 case 9:
			    ds->pro_head = la_float;
			    break;
			 case 10:
			    strncpy( ds->ellip_des,  token, 16 );
			    break;
			 case 11:
			    ds->ellip_maj = la_float;
			    break;
			 case 12:
			    ds->ellip_min = la_float;
			    break;
			 case 13:
			    ds->earth_mass = la_float;
			    break;
			 case 14:
			    ds->grav_const = la_float;
			    break;
			 case 15:
			    get_array( ds->ellip_j,  3,  2);
			    break;
			 case 16:
			    ds->terrain_h = la_float;
			    break;
			 case 17:
			    ds->sc_lin = la_int;
			    break;
			 case 18:
			    ds->sc_pix = la_int;
			    break;
			 case 19:
			    ds->scene_len = la_float;
			    break;
			 case 20:
			    ds->scene_wid = la_float;
			    break;
			 case 21:
			    ds->nchn = la_int;
			    break;
			 case 22:
			    strncpy( ds->mission_id,  token, 16 );
			    break;
			 case 23:
			    strncpy( ds->sensor_id,  token, 32 );
			    break;
			 case 24:
			    strncpy( ds->revolution,  token, 8 );
			    break;
			 case 25:
			    ds->plat_lat = la_float;
			    break;
			 case 26:
			    ds->plat_long = la_float;
			    break;
			 case 27:
			    ds->plat_head_scene = la_float;
			    break;
			 case 28:
			    ds->clock_ang = la_float;
			    break;
			 case 29:
			    ds->incident_ang = la_float;
			    break;
			 case 30:
			    ds->frequency = la_float;
			    break;
			 case 31:
			    ds->wave_length = la_float;
			    break;
			 case 32:
			    strncpy( ds->motion_comp,  token, 2 );
			    break;
			 case 33:
			    strncpy(ds->pulse_code,  token, 16 );
			    break;
			 case 34:
			    get_array( ds->ampl_coef,  5,  2);
			    break;
			 case 35:
			    get_array( ds->phas_coef,  5,  2);
			    break;
			 case 36:
			    ds->chirp_ext_ind = la_int;
			    break;
			 case 37:
			    ds->rng_samp_rate = la_float;
			    break;
			 case 38:
			    ds->rng_gate = la_float;
			    break;
			 case 39:
			    ds->rng_length = la_float;
			    break;
			 case 40:
			    strncpy( ds->baseband_f,  token, 4 );
			    break;
			 case 41:
			    strncpy( ds->rngcmp_f,  token, 4 );
			    break;
			 case 42:
			    ds->gn_polar = la_float;
			    break;
			 case 43:
			    ds->gn_cross = la_float;
			    break;
			 case 44:
			    ds->chn_bits = la_int;
			    break;
			 case 45:
			    strncpy( ds->quant_desc,  token, 12 );
			    break;
			 case 46:
			    ds->i_bias = la_float;
			    break;
			 case 47:
			    ds->q_bias = la_float;
			    break;
			 case 48:
			    ds->iq_ratio = la_float;
			    break;
			 case 49:
			    ds->spare_dss_7 = la_float;
			    break;
			 case 50:
			    ds->spare_dss_8 = la_float;
			    break;
			 case 51:
			    ds->ele_sight = la_float;
			    break;
			 case 52:
			    ds->mech_sight = la_float;
			    break;
			 case 53:
			    strncpy( ds->echo_track,  token, 4 );
			    break;
			 case 54:
			    ds->prf = la_float;
			    break;
			 case 55:
			    ds->elev_beam = la_float;
			    break;
			 case 56:
			    ds->azi_beam = la_float;
			    break;
			 case 57:
			    strncpy( ds->sat_bintim,  token, 16 );
			    break;
			 case 58:
			    strncpy( ds->sat_clktim,  token, 32 );
			    break;
			 case 59:
			    ds->sat_clkinc = la_int;
			    break;
			 case 60:
			    strncpy( ds->fac_id,  token, 16 );
			    break;
			 case 61:
			    strncpy( ds->sys_id,  token, 8 );
			    break;
			 case 62:
			    strncpy( ds->ver_id,  token, 8 );
			    break;
			 case 63:
			    strncpy( ds->fac_code,  token, 16 );
			    break;
			 case 64:
			    strncpy( ds->lev_code,  token, 16 );
			    break;
			 case 65:
			    strncpy( ds->product_type,  token, 32 );
			    break;
			 case 66:
			    strncpy( ds->algor_id,  token, 32 );
			    break;
			 case 67:
			    ds->n_azilok = la_float;
			    break;
			 case 68:
			    ds->n_rnglok = la_float;
			    break;
			 case 69:
			    ds->bnd_azilok = la_float;
			    break;
			 case 70:
			    ds->bnd_rnglok = la_float;
			    break;
			 case 71:
			    ds->bnd_azi = la_float;
			    break;
			 case 72:
			    ds->bnd_rng = la_float;
			    break;
			 case 73:
			    strncpy( ds->azi_weight,  token, 32 );
			    break;
			 case 74:
			    strncpy( ds->rng_weight,  token, 32 );
			    break;
			 case 75:
			    strncpy( ds->data_inpsrc,  token, 16 );
			    break;
			 case 76:
			    ds->rng_res = la_float;
			    break;
			 case 77:
			    ds->azi_res = la_float;
			    break;
			 case 78:
			    get_array( ds->radi_stretch,  2,  2 );
			    break;
			 case 79:
			    get_array( ds->alt_dopcen,  3,  2 );
			    break;
			 case 80:
			    get_array( ds->crt_dopcen,  3,  2 );
			    break;
			 case 81:
			    strncpy( ds->time_dir_pix,  token, 8 );
			    break;
			 case 82:
			    strncpy( ds->time_dir_lin,  token, 8 );
			    break;
			 case 83:
			    get_array( ds->alt_rate,  3,  2 );
			    break;
			 case 84:
			    get_array( ds->crt_rate,  3,  2 );
			    break;
			 case 85:
			    strncpy( ds->line_cont,  token, 8 );
			    break;
			 case 86:
			    strncpy( ds->clutterlock_flg, token, 4 );
			    break;
			 case 87:
			    strncpy( ds->auto_focus,  token, 4 );
			    break;
			 case 88:
			    ds->line_spacing = la_float;
			    break;
			 case 89:
			    ds->pixel_spacing = la_float;
			    break;
			 case 90:
			    strncpy( ds->rngcmp_desg,  token, 16 );
			    break;
#ifdef PRE_RADARSAT
			 case 91:
			    ds->annot_pts = la_int;
			    break;
			 case 92:
			    get_array( ds->annot_line,  ds->annot_pts, 1 );
			    break;
			 case 93:
			    get_array( ds->annot_pixel,  ds->annot_pts,  1 );
			    break;
			 case 94:
			    get_array( ds->annot_text,  ds->annot_pts,  0 );
			    break;
#else
			 case 91:
			    ds->no_beams = la_int;
			    break;
			 case 92:
			    strncpy( ds->beam1,  token, 4 );
			    break;
			 case 93:
			    strncpy( ds->beam2,  token, 4 );
			    break;
			 case 94:
			    strncpy( ds->beam3,  token, 4 );
			    break;
			 case 95:
			    strncpy( ds->beam4,  token, 4 );
			    break;
			 case 96:
			    ds->prf1 = la_float;
			    break;
			 case 97:
			    ds->prf2 = la_float;
			    break;
                         case 98:
			    ds->prf3 = la_float;
                            break;
			 case 99:
			    ds->prf4 = la_float;
			    break;
			 case 100:
			    ds->rng_gate1 = la_float;
			    break;
			 case 101:
			    ds->rng_gate2 = la_float;
			    break;
			 case 102:
			    ds->rng_gate3 = la_float;
			    break;
			 case 103:
			    ds->rng_gate4 = la_float;
			    break;
			 case 104:
			    ds->tot_pls_burst = la_int;
			    break;
			 case 105:
			    ds->val_pls_burst = la_int;
			    break;
			 case 106:
			    ds->az_ovlp_nxt_img = la_int;
			    break;
			 case 107:
			    ds->rg_off_nxt_img = la_int;
			    break;
			 case 108:
			    strncpy(ds->cal_params_file, token, 32);
			    break;
			 case 109:
			    strncpy(ds->scan_results_file, token, 32);
			    break;
			 case 110:
			    strncpy(ds->scanner_version, token, 16);
			    break;
			 case 111:
			    strncpy(ds->decode_version, token, 16);
			    break;
#endif
			 default :
			    break;
		     }
		     break;
		 }
		 i++;
	     }
	  }
    }
    
    /* Fill in the record type info */
    d->rec_seq  = Record_count++;
    d->rec_sub1 = LS1SUB;
    d->rec_type = LSTYPE;
#ifdef PRE_RADARSAT
    d->rec_sub2 = COM2SUB;
#else
    d->rec_sub2 = LS2SUB;
#endif
    d->rec_sub3 = LS3SUB;
    d->length = t->l_dataset;
    
    return(1);
}

int Decode_ODL_MPR(SARL_ptr *tree)
{
    char Object_tokens[] = {"Map_Proj"};
    Map_Proj *mp = tree->map_proj;
    Sarl_Desc_Rec *t = &(tree->descript);
    int n_data = tree->descript.n_map_proj;
    desc_rec *d;
    char token[MAX_LINE];
    int la_int;
    float la_float;
    double la_double;
    int len;
    int multi=1;
    int ret;
    int i=1;
    
    static User_token_def token_list[] = {
        { "SEQ_NUM",          2 }, { "MAP_DESC",        1 }, 
	{ "N_PIXEL",          2 }, { "N_LINE",          2 }, 
	{ "PIXEL_SPACING",    3 }, { "LINE_SPACING",    3 }, 
	{ "OSC_ORIENT",       3 }, { "ORB_INCL",        3 }, 
	{ "ASC_NODE",         3 }, { "ISC_DIST",        3 }, 
	{ "GEO_ALT",          3 }, { "ISC_VEL",         3 }, 
	{ "PLAT_HEAD",        3 }, { "REF_ELLIP",       1 }, 
	{ "SEMI_MAJOR",       3 }, { "SEMI_MINOR",      3 }, 
	{ "DATUM_SHIFT",      5 }, { "AUX_DATUM_SHIFT", 5 }, 
	{ "SCAL_ELLIP",       3 }, { "PROJECTION",      1 }, 
	{ "UTM_DESC",         1 }, { "UTM_ZONE_SIG",    1 }, 
	{ "UTM_EAST_ORIG",    3 }, { "UTM_NORTH_ORIG",  3 }, 
	{ "UTM_CENT_LONG",    3 }, { "UTM_CENT_LAT",    3 }, 
	{ "UTM_STAND_PAR",    5 }, { "UTM_SCALE",       3 }, 
	{ "UPS_DESC",         1 }, { "UPS_CENT_LONG",   3 }, 
	{ "UPS_CENT_LAT",     3 }, { "UPS_SCALE",       3 }, 
	{ "NSP_DESC",         1 }, { "NSP_EAST_ORIG",   3 }, 
	{ "NSP_NORTH_ORIG",   3 }, { "NSP_CENT_LONG",   3 }, 
	{ "NSP_CENT_LAT",     3 }, { "NSP_STAND_PAR",   5 }, 
	{ "NSP_STAND_MER",    5 }, { "CORNER_NE",       5 }, 
	{ "CORNER_LL",        5 }, { "TERR_HEIGHT",     5 }, 
	{ "LP_CONV_COEF",     5 }, { "MP_CONV_COEF",    5 },
	{ "NULL",          NULL }
    };

    if (mp == NULL) {
       printf("\nError: File Descriptor Record did not have a Map Projection record listed\n");
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (mp->seq_num !=0) {
           if (i<=n_data) {
              mp=mp->next;
           }
           else
              return(DECODE_ERROR);
           i++;
       }
    }
    
    printf("\n Decode Map Projection record \n");
    
    /* process the record */
    d = &(mp->desc);

    while (multi) {
          if ( !(ret=next_token( token )) ) {
	     printf("\n End of File");
	     return(0);
	  }
	  
	  if ( !(strcmp( token, "END_OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     /* compare END_OBJECT to OBJECT tokens */
	     if (!(strcmp( token, Object_tokens )) ) {
	        multi--;
	     }
	  }
	  else {
	     /* find a match for Object token in token list */
	     i=0;
	     while (strcmp(token_list[i].token, "NULL")) {
		 if ( !(strcmp(token_list[i].token, token)) ) {
		     switch (token_list[i].format_type) {
			 case 1:
			    next_token( token );
			    /* remove quote */
			    len = strlen(token);
			    if (len >2) {
			       strncpy(token, &token[1],  len-2);
			       token[len-2]='\0';
			    }
			    else
			       strcpy(token, "");
			    break;
			 case 2:
			    get_int(&la_int);
			    break;
			 case 3:
			    get_float(&la_float);
			    break;
			 case 4:
			    get_double(&la_double);
			    break;
			 case 5:
			 default:
			    break;
		     }
		     switch ( i+1) {
			 case 1:
			    mp->seq_num = la_int;
			    break;
			 case 2:
			    strncpy(mp->map_desc, token, 32);
			    break;
			 case 3:
			    mp->n_pixel = la_int;
			    break;
			 case 4:
			    mp->n_line = la_int;
			    break;
			 case 5:
			    mp->pixel_spacing = la_float;
			    break;
			 case 6:
			    mp->line_spacing = la_float;
			    break;
			 case 7:
			    mp->osc_orient = la_float;
			    break;
			 case 8:
			    mp->orb_incl = la_float;
			    break;
			 case 9:
			    mp->asc_node = la_float;
			    break;
			 case 10:
			    mp->isc_dist = la_float;
			    break;
			 case 11:
			    mp->geo_alt = la_float;
			    break;
			 case 12:
			    mp->isc_vel = la_float;
			    break; 
			 case 13:
			    mp->plat_head = la_float;
			    break;
			 case 14:
			    strncpy(mp->ref_ellip, token, 32);
			    break;
			 case 15:
			    mp->semi_major = la_float;
			    break;
			 case 16:
			    mp->semi_minor = la_float;
			    break;
			 case 17:
			    get_array( mp->datum_shift,  3,  2);
			    break;
			 case 18:
			    get_array( mp->aux_datum_shift,  3,  2);
			    break;
			 case 19:
			    mp->scal_ellip = la_float;
			    break;
			 case 20:
			    strncpy(mp->projection, token, 32);
			    break;
			 case 21:
			    strncpy(mp->utm_desc, token, 32);
			    break;
			 case 22:
			    strncpy(mp->utm_zone_sig, token, 4);
			    break;
			 case 23:
			    mp->utm_east_orig = la_float;
			    break;
			 case 24:
			    mp->utm_north_orig = la_float;
			    break;
			 case 25:
			    mp->utm_cent_long = la_float;
			    break;
			 case 26:
			    mp->utm_cent_lat = la_float;
			    break;
			 case 27:
			    get_array( mp->utm_stand_par,  2,  2);
			    break;
			 case 28:
			    mp->utm_scale = la_float;
			    break;
			 case 29:
			    strncpy(mp->ups_desc, token, 32);
			    break;
			 case 30:
			    mp->ups_cent_long = la_float;
			    break;
			 case 31:
			    mp->ups_cent_lat = la_float;
			    break;
			 case 32:
			    mp->ups_scale = la_float;
			    break;
			 case 33:
			    strncpy(mp->nsp_desc, token, 32);
			    break;
			 case 34:
			    mp->nsp_east_orig = la_float;
			    break;
			 case 35:
			    mp->nsp_north_orig = la_float;
			    break;
			 case 36:
			    mp->nsp_cent_long = la_float;
			    break;
			 case 37:
			    mp->nsp_cent_lat = la_float;
			    break;
			 case 38:
			    get_array( mp->nsp_stand_par,  4,  2);
			    break;
			 case 39:
			    get_array( mp->nsp_stand_mer,  3,  2);
			    break;
			 case 40:
			    get_array( mp->corner_ne,  8,  2);
			    break;
			 case 41:
			    get_array( mp->corner_ll,  8,  2);
			    break;
			 case 42:
			    get_array( mp->terr_height,  4,  2);
			    break;
			 case 43:
			    get_array( mp->lp_conv_coef,  8,  3);
			    break;
			 case 44:
			    get_array( mp->mp_conv_coef,  8,  3);
			    break;
                         default:
                            break;
		     }
		     break;
		 }
		 i++;
	     }
	  }
    }
    /* Fill in the record type info */
    d->rec_seq  = Record_count++;
    d->rec_sub1 = LM1SUB;
    d->rec_type = LMTYPE;
#ifdef PRE_RADARSAT
    d->rec_sub2 = COM2SUB;
#else
    d->rec_sub2 = LM2SUB;
#endif
    d->rec_sub3 = LM3SUB;
    d->length = t->l_map_proj;
    return(1);
}

int Decode_ODL_PDR(SARL_ptr *tree)
{
    char *Object_tokens[] = { "Pos_Data",  "Pos_Vect_Rec",  "NULL" };
    Sarl_Desc_Rec *t = &(tree->descript);
    Pos_Data *p = tree->platform;
    Pos_Vect_Rec *pv;
    int n_data = tree->descript.n_plat_pos;
    desc_rec *d;
    char token[MAX_LINE];
    int i=1;
    int la_int;
    float la_float;
    double la_double;
    int len;
    int multi=1;
    int ret;
    int found;
    int object_set[] = { 1, 0 };
    
    static User_token_def token_list[] = {
    	{ "SEQ_NUM",          2 },    { "ORBIT_ELE_DESG",       1 }, 
	{ "ORBIT_ELE",        5 },    { "NDATA",                2 }, 
	{ "YEAR",             2 },    { "MONTH",                2 }, 
	{ "DAY",              2 },    { "GMT_DAY",              2 }, 
	{ "GMT_SEC",          4 },    { "DATA_INT",             4 }, 
	{ "REF_COORD",        1 },    { "HR_ANGLE",             4 }, 
	{ "ALT_POSERR",       3 },    { "CRT_POSERR",           3 }, 
	{ "RAD_POSERR",       3 },    { "ALT_VELERR",           3 }, 
	{ "CRT_VELERR",       3 },    { "RAD_VELERR",           3 }, 
        { "PV.POS",           5 },    { "PV.VEL",               5 },    
	{ "NULL",          NULL }
    };
    
    if (p == NULL) {
       printf("\nError: File Descriptor Record did not have a Platform Position Data record listed\n");
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (p->seq_num !=0) {
           if (i<=n_data) {
              p=p->next;
           }
           else
              return(DECODE_ERROR);
           i++;
       }
    }
    printf("\n Decode Position Data record \n");
    
    /* process the record */
    d = &(p->desc);
    
    while (multi) {
          if ( !(ret=next_token( token )) ) {
	     printf("\n End of File");
	     return(0);
	  }
	  
          /* is it an embedded new object? */
	  if ( !(strcmp( token, "OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     i=0; found = 0;
	     while( strcmp( Object_tokens[i], "NULL") ) {
	        /* which one is it? */
	        if (!(strcmp( token, Object_tokens[i] )) ) {
		   object_set[i]++;
		   found=1;
		}
		i++;
	     }
	     if (!found) {
		printf("\n OBJECT token not in Object list - %s", token);
		return(0);
	     }
	     /* keep track of the OBJECT depth */
	     multi++;
	  }
	  
	  if ( !(strcmp( token, "END_OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     /* compare END_OBJECT to OBJECT tokens */
	     i=0; found=0;
	     while( strcmp( Object_tokens[i], "NULL") ) {
	     	/* which one is it? */
	        if (!(strcmp( token, Object_tokens[i] )) ) {
	           multi--;
		   found=1;
		   object_set[i]--;
		   if (object_set[i]) {
		       printf("\n Object never set - %s",  token);
		       return(0);
		   }
		   switch (i) {
		       case 0:
		            break;
		       case 1:
		            if (pv!= NULL) pv=pv->next;
			    break;
		   }
		}
		i++;
	     }
	     if (!found) {
		printf("\n END_OBJECT token not in Object list - %s", token);
		return(0);
	     }
	  }
	  else {
	     /* find a match for Object token in token list */
	     i=0;
	     while (strcmp(token_list[i].token, "NULL")) {
		 if ( !(strcmp(token_list[i].token, token)) ) {
		     switch (token_list[i].format_type) {
			 case 1:
			    next_token( token );
			    /* remove quote */
			    len = strlen(token);
			    if (len >2) {
			       strncpy(token, &token[1],  len-2);
			       token[len-2]='\0';
			    }
			    else
			       strcpy(token, "");
			    break;
			 case 2:
			    get_int(&la_int);
			    break;
			 case 3:
			    get_float(&la_float);
			    break;
			 case 4:
			    get_double(&la_double);
			    break;
			 case 5:
			 default: /* arrays handled on an individual basis */
			    break;
		     }
		     switch ( i+1 ) {
			 case 1:
			    p->seq_num = la_int;
			    break;
			 case 2:
			    strncpy( p->orbit_ele_desg,  token, 32 );
			    break;
			 case 3:
			    get_array( p->orbit_ele,  6,  2 );
			    break;
			 case 4:
			    p->ndata = la_int;
			    pv = Allocate_Position_Velocity_Sets( p );
			    break;
			 case 5:
			    p->year = la_int;
			    break;
			 case 6:
			    p->month = la_int;
			    break;
			 case 7:
			    p->day = la_int;
			    break;
			 case 8:
			    p->gmt_day = la_int;
			    break;
			 case 9:
			    p->gmt_sec = la_double;
			    break;
			 case 10:
			    p->data_int = la_double;
			    break;
			 case 11:
			    strncpy( p->ref_coord,  token, 64 );
			    break;
			 case 12:
			    p->hr_angle = la_double;
			    break;
			 case 13:
			    p->alt_poserr = la_float;
			    break;
			 case 14:
			    p->crt_poserr = la_float;
			    break;
			 case 15:
			    p->rad_poserr = la_float;
			    break;
			 case 16:
			    p->alt_velerr = la_float;
			    break;
			 case 17:
			    p->crt_velerr = la_float;
			    break;
			 case 18:
			    p->rad_velerr = la_float;
			    break;
                         case 19:
			    get_array( pv->pos,  3,  3 );
                            break;
                         case 20:
			    get_array( pv->vel,  3,  3 );
			    break;
			 default :
			    break;
		     }
		     break;
		 }
		 i++;
	     }
	  }
    }
    
    /* Fill in the record type info */
    d->rec_seq  = Record_count++;
    d->rec_sub1 = LP1SUB;
    d->rec_type = LPTYPE;
#ifdef PRE_RADARSAT
    d->rec_sub2 = COM2SUB;
#else
    d->rec_sub2 = LP2SUB;
#endif
    d->rec_sub3 = LP3SUB;
    d->length = t->l_plat_pos;
    return(1);
}
int Decode_ODL_ADR(SARL_ptr *tree)
{
    Att_Data *a = tree->attitude;
    Att_Vect_Rec *av;
    Sarl_Desc_Rec *t = &(tree->descript);
    int n_data = tree->descript.n_att_data;
    desc_rec *d;
    char token[MAX_LINE];
    char *Object_tokens[] = {"Att_Data", "Att_Vect_Rec", "NULL"};
    int i=1;
    int la_int;
    float la_float;
    double la_double;
    int len;
    int multi=1;
    int ret;
    int found;
    int object_set[] = { 1, 0 };

    static User_token_def token_list[] = {
        { "SEQ_NUM",         2 }, { "NPOINT",           2 },
        { "GMT_DAY",         2 }, { "GMT_MSEC",         2 },
        { "PITCH_FLAG",      2 }, { "ROLL_FLAG",        2 },
        { "YAW_FLAG",        2 }, { "PITCH",            3 },
        { "ROLL",            3 }, { "YAW",              3 },
        { "PITCH_RATE_FLAG", 2 }, { "ROLL_RATE_FLAG",   2 },
        { "YAW_RATE_FLAG",   2 }, { "PITCH_RATE",       3 },
        { "ROLL_RATE",       3 }, { "YAW_RATE",         3 },
        { "NULL",         NULL }
    };

    if (a == NULL) {
       printf("\nError: File Descriptor Record did not have an Attitude Data record listed\n");
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (a->seq_num !=0) {
           if (i<=n_data) {
              a=a->next;
           }
           else
              return(DECODE_ERROR);
           i++;
       }
    }
    
    printf("\n Decode Attitude Data record \n");
       
    /* process the record */
    d = &(a->desc);
     
    while (multi) {
          if ( !(ret=next_token( token )) ) {
	     printf("\n End of File");
	     return(0);
	  }
	  
          /* is it an embedded new object? */
	  if ( !(strcmp( token, "OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     i=0; found = 0;
	     while( strcmp( Object_tokens[i], "NULL") ) {
	        /* which one is it? */
	        if (!(strcmp( token, Object_tokens[i] )) ) {
		   object_set[i]++;
		   found=1;
		}
		i++;
	     }
	     if (!found) {
		printf("\n OBJECT token not in Object list - %s", token);
		return(0);
	     }
	     /* keep track of the OBJECT depth */
	     multi++;
	  }
	  
	  if ( !(strcmp( token, "END_OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     /* compare END_OBJECT to OBJECT tokens */
	     i=0; found=0;
	     while( strcmp( Object_tokens[i], "NULL") ) {
	     	/* which one is it? */
	        if (!(strcmp( token, Object_tokens[i] )) ) {
	           multi--;
		   found=1;
		   object_set[i]--;
		   if (object_set[i]) {
		       printf("\n Object never set - %s",  token);
		       return(0);
		   }
		   switch (i) {
		       case 0:
		            break;
		       case 1:
		            if (av!= NULL) av=av->next;
			    break;
		   }
		}
		i++;
	     }
	     if (!found) {
		printf("\n END_OBJECT token not in Object list - %s", token);
		return(0);
	     }
	  }
	  else {
	     /* find a match for Object token in token list */
	     i=0;
	     while (strcmp(token_list[i].token, "NULL")) {
		 if ( !(strcmp(token_list[i].token, token)) ) {
		     switch (token_list[i].format_type) {
			 case 1:
			    next_token( token );
			    /* remove quote */
			    len = strlen(token);
			    if (len >2) {
			       strncpy(token, &token[1],  len-2);
			       token[len-2]='\0';
			    }
			    else
			       strcpy(token, "");
			    break;
			 case 2:
			    get_int(&la_int);
			    break;
			 case 3:
			    get_float(&la_float);
			    break;
			 case 4:
			    get_double(&la_double);
			    break;
			 case 5:
			 default:
			    break;
		     }
		     switch ( i+1) {
			 case 1:
			    a->seq_num = la_int;
			    break;
			 case 2:
			    a->npoint = la_int;
			    av = Allocate_Attitude_Sets( a );
			    break;
			 case 3:
			    av->gmt_day = la_int;
			    break;
                         case 4:
			    av->gmt_msec = la_int;
			    break;
                         case 5:
			    av->pitch_flag = la_int;
			    break;
                         case 6:
			    av->roll_flag = la_int;
			    break;
                         case 7:
			    av->yaw_flag = la_int;
			    break;
                         case 8:
			    av->pitch = la_float;
			    break;
                         case 9:
			    av->roll = la_float;
			    break;
                         case 10:
			    av->yaw = la_float;
			    break;
                         case 11:
			    av->pitch_rate_flag = la_int;
			    break;
                         case 12:
			    av->roll_rate_flag = la_int;
			    break;
                         case 13:
			    av->yaw_rate_flag = la_int;
			    break;
                         case 14:
			    av->pitch_rate = la_float;
			    break;
                         case 15:
			    av->roll_rate = la_float;
			    break;
                         case 16:
			    av->yaw_rate = la_float;
			    break;
                         default:
                            break;
		     }
                     break;
		 }
		 i++;
	     }
	  }
    }

    /* Fill in the record type info */
    d->rec_seq  = Record_count++;
    d->rec_sub1 = LA1SUB;
    d->rec_type = LATYPE;
#ifdef PRE_RADARSAT
    d->rec_sub2 = COM2SUB;
#else
    d->rec_sub2 = LA2SUB;
#endif
    d->rec_sub3 = LA3SUB;
    d->length = t->l_att_data;
    return(1);
}
int Decode_ODL_RDR(SARL_ptr *tree)
{
    Radi_Data *r = tree->radio_data;
    Sarl_Desc_Rec *t = &(tree->descript);
    int n_data = tree->descript.n_radi_data;
    desc_rec *d;
    char token[MAX_LINE];
    char Object_tokens[] = {"Radi_Data"};
    int i=1;
    int la_int;
    float la_float;
    double la_double;
    int len;
    int multi=1;
    int ret;

    static User_token_def token_list[] = {
	{ "SEQ_NUM",          2 }, { "N_DATA",           2 }, 
	{ "FIELD_SIZE",       2 }, { "CHAN_IND",         1 }, 
	{ "TABLE_DESIG",      1 }, { "N_SAMP",           2 }, 
	{ "SAMP_TYPE",        1 }, { "NOISE_FACT",       3 }, 
	{ "LINEAR_CONV_FACT", 3 }, { "OFFSET_CONV_FACT", 3 },
	{ "LOOKUP_TAB",       5 }, { "NULL",          NULL } 
    };
    
    if (r == NULL) {
       printf("\nError: File Descriptor Record did not have a Radiometric Data record listed\n");
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (r->seq_num !=0) {
           if (i<=n_data) {
              r=r->next;
           }
           else
              return(DECODE_ERROR);
           i++;
       }
    }
    
    printf("\n Decode Radiometric Data record \n");
        
    /* process the record */
    d = &(r->desc);
    
    while (multi) {
          if ( !(ret=next_token( token )) ) {
	     printf("\n End of File");
	     return(0);
	  }
	  
	  if ( !(strcmp( token, "END_OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     /* compare END_OBJECT to OBJECT tokens */
	     if (!(strcmp( token, Object_tokens )) ) {
	        multi--;
	     }
	  }
	  else {
	     /* find a match for Object token in token list */
	     i=0;
	     while (strcmp(token_list[i].token, "NULL")) {
		 if ( !(strcmp(token_list[i].token, token)) ) {
		     switch (token_list[i].format_type) {
			 case 1:
			    next_token( token );
			    /* remove  */
			    len = strlen(token);
			    if (len >2) {
			       strncpy(token, &token[1],  len-2);
			       token[len-2]='\0';
			    }
			    else
			       strcpy(token, "");
			    break;
			 case 2:
			    get_int(&la_int);
			    break;
			 case 3:
			    get_float(&la_float);
			    break;
			 case 4:
			    get_double(&la_double);
			    break;
			 case 5:
			 default:
			    break;
		     }
		     switch ( i+1) {
			 case 1:
			    r->seq_num = la_int;
			    break;
			 case 2:
			    r->n_data = la_int;
			    break;
			 case 3:
			    r->field_size = la_int;
			    break;
			 case 4:
			    strncpy(r->chan_ind, token, 4 );
			    break;
			 case 5:
			    strncpy(r->table_desig, token, 24);
			    break;
			 case 6:
			    r->n_samp = la_int;
			    break;
			 case 7:
			    strncpy(r->samp_type, token, 16);
			    break;
			 case 8:
			    r->noise_fact = la_float;
			    break;
			 case 9:
			    r->linear_conv_fact = la_float;
			    break;
			 case 10:
			    r->offset_conv_fact = la_float;
			    break;
			 case 11:
			    get_array(r->lookup_tab,  r->n_samp,  2);
			    break;
                         default:
                            break;
		     }
		     break;
		 }
		 i++;
	     }
	  }
    }

    /* Fill in the record type info */
    d->rec_seq  = Record_count++;
    d->rec_sub1 = LR1SUB;
    d->rec_type = LRTYPE;
#ifdef PRE_RADARSAT
    d->rec_sub2 = COM2SUB;
#else
    d->rec_sub2 = LR2SUB;
#endif
    d->rec_sub3 = LR3SUB;
    d->length = t->l_radi_data;
    return(1);
}
int Decode_ODL_RCR(SARL_ptr *tree)
{
    Radi_Comp *r = tree->radio_comp;
    Rad_Comp_Set *rs;
    Radio_Comp_Tbl *rt;
    Sarl_Desc_Rec *t = &(tree->descript);
    desc_rec *d;
    char token[MAX_LINE];
    char *Object_tokens[] = {"Radi_Comp", "Radi_Comp_Set", "Radio_Comp_Tbl",  "NULL"};
    int n_data = tree->descript.n_radi_comp;
    int i=1;
    int la_int;
    float la_float;
    double la_double;
    int len;
    int multi=1;
    int ret;
    int Comp_Sets;
    int found;
    int object_set[] = { 1,  0,  0 };

    static User_token_def token_list[] = {
        { "SEQ_NUM",             2 }, { "SAR_CHAN",            2 },
        { "N_DSET",              2 }, { "DSET_SIZE",           2 },
        { "COMP_DATA_TYPE",      1 }, { "DATA_DESCR",          1 },
        { "REQ_RECS",            2 }, { "TABLE_SEQ_NUM",       2 },
        { "NUM_PAIRS",           2 }, { "FIRST_PIXEL",         2 },
        { "LAST_PIXEL",          2 }, { "PIXEL_SIZE",          2 },
        { "MIN_SAMP_INDEX",      3 }, { "MIN_COMP_VALUE",      3 },
        { "MAX_SAMP_INDEX",      3 }, { "MAX_COMP_VALUE",      3 },
        { "N_TABLE_ENTRIES",     2 }, { "SAMPLE_OFFSET",       3 }, 
	{ "SAMPLE_GAIN",         3 }, { "NULL",             NULL }
    };
    
    if (r == NULL) {
       printf("\nError: File Descriptor Record did not have a Radiometric Compensation record listed\n");
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (r->seq_num !=0) {
           if (i<=n_data) {
              r=r->next;
           }
           else
              return(DECODE_ERROR);
           i++;
       }
    }
    
    printf("\n Decode Data Radiometric Compensation record \n");
       
    /* process the record */
    d = &(r->desc);
     
    while (multi) {
          if ( !(ret=next_token( token )) ) {
	     printf("\n End of File");
	     return(0);
	  }
	  
          /* is it an embedded new object? */
	  if ( !(strcmp( token, "OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     i=0; found = 0;
	     while( strcmp( Object_tokens[i], "NULL") ) {
	        /* which one is it? */
	        if (!(strcmp( token, Object_tokens[i] )) ) {
		   object_set[i]++;
		   found=1;
		}
		i++;
	     }
	     if (!found) {
		printf("\n OBJECT token not in Object list - %s", token);
		return(0);
	     }
	     /* keep track of the OBJECT depth */
	     multi++;
	  }
	  
	  if ( !(strcmp( token, "END_OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     /* compare END_OBJECT to OBJECT tokens */
	     i=0; found=0;
	     while( strcmp( Object_tokens[i], "NULL") ) {
	     	/* which one is it? */
	        if (!(strcmp( token, Object_tokens[i] )) ) {
	           multi--;
		   found=1;
		   object_set[i]--;
		   if (object_set[i]) {
		       printf("\n Object never set - %s",  token);
		       return(0);
		   }
		   switch (i) {
		       case 0:
		            break;
		       case 1:
		            if (rs!= NULL) rs=rs->next;
			    break;
		       case 2:
                            if (rt!= NULL) rt=rt->next;
		            break;
		   }
		}
		i++;
	     }
	     if (!found) {
		printf("\n END_OBJECT token not in Object list - %s", token);
		return(0);
	     }
	  }
	  else {
	     /* find a match for Object token in token list */
	     i=0;
	     while (strcmp(token_list[i].token, "NULL")) {
		 if ( !(strcmp(token_list[i].token, token)) ) {
		     switch (token_list[i].format_type) {
			 case 1:
			    next_token( token );
			    /* remove quote */
			    len = strlen(token);
			    if (len >2) {
			       strncpy(token, &token[1],  len-2);
			       token[len-2]='\0';
			    }
			    else
			       strcpy(token, "");
			    break;
			 case 2:
			    get_int(&la_int);
			    break;
			 case 3:
			    get_float(&la_float);
			    break;
			 case 4:
			    get_double(&la_double);
			    break;
			 case 5:
			 default:
			    break;
		     }
		     switch ( i+1) {
			 case 1:
			    r->seq_num = la_int;
			    break;
			 case 2:
			    r->sar_chan = la_int;
			    break;
			 case 3:
			    r->n_dset = la_int;
			    rs = Allocate_Comp_Sets( r );
			    Comp_Sets=TRUE;
			    break;
                         case 4:
			    r->dset_size = la_int;
			    break;
                         case 5:
			    strncpy(rs->comp_data_type, token, 7);
			    break;
                         case 6:
			    strncpy(rs->data_descr, token, 32);
			    break;
                         case 7:
			    rs->req_recs = la_int;
			    break;
                         case 8:
			    rs->table_seq_num = la_int;
			    break;
                         case 9:
			    rs->num_pairs = la_int;
			    break;
                         case 10:
			    rs->first_pixel = la_int;
			    break;
                         case 11:
			    rs->last_pixel = la_int;
			    break;
                         case 12:
			    rs->pixel_size = la_int;
			    break;
                         case 13:
			    rs->min_samp_index = la_float;
			    break;
                         case 14:
			    rs->min_comp_value = la_float;
			    break;
                         case 15:
			    rs->max_samp_index = la_float;
			    break;
                         case 16:
			    rs->max_comp_value = la_float;
			    break;
                         case 17:
			    rs->n_table_entries = la_int;
			    rt = Allocate_Comp_Tbl( rs );
			    break;
                         case 18:
			    rt->sample_offset = la_float;
			    break;
                         case 19:
			    rt->sample_gain = la_float;
			    break;
                         default:
                            break;
		     }
                     break;
		 }
		 i++;
	     }
	  }
    }

    /* Fill in the record type info */
    d->rec_seq  = Record_count++;
    d->rec_sub1 = LC1SUB;
    d->rec_type = LCTYPE;
#ifdef PRE_RADARSAT
    d->rec_sub2 = COM2SUB;
#else
    d->rec_sub2 = LC2SUB;
#endif
    d->rec_sub3 = LC3SUB;
    d->length = t->l_radi_comp;
    return(1);
}

int Decode_ODL_QSR(SARL_ptr *tree)
{
    Qual_Sum *q = tree->data_qual;
    Sarl_Desc_Rec *t = &(tree->descript);
    int n_data = tree->descript.n_qual_sum;
    desc_rec *d;
    char token[MAX_LINE];
    char Object_tokens[] = {"Qual_Sum"};
    int i=1;
    int la_int;
    float la_float;
    double la_double;
    int len;
    int multi=1;
    int ret;
    
    static User_token_def token_list[] = {
        { "SEQ_NUM",         2 }, { "CHAN_IND",         1 },
        { "CALI_DATE",       1 }, { "NCHN",             2 },
        { "ISLR",            3 }, { "PSLR",             3 },
        { "AZI_AMBIG",       3 }, { "RNG_AMBIG",        3 },
        { "SNR",             3 }, { "BER",              3 },
        { "RNG_RES",         3 }, { "AZI_RES",          3 },
        { "RAD_RES",         3 }, { "DYN_RNG",          3 },
        { "ABS_RAD_UNC_DB",  3 }, { "ABS_RAD_UNC_DEG",  3 },
        { "REL_RAD_UNC.DB",  3 }, { "REL_RAD_UNC.DEG",  3 },
        { "ALT_LOCERR",      3 }, { "CRT_LOCERR",       3 }, 
	{ "ALT_SCALE",       3 }, { "CRT_SCALE",        3 },
        { "DIS_SKEW",        3 }, { "ORI_ERR",          3 }, 
	{ "MISREG ALT_M",    3 }, { "MISREG CRT_M",     3 }, 
#ifndef PRE_RADARSAT
	{ "NESZ",            3 }, { "ENL",              3 }, 
	{ "TB_UPDATE",       1 }, { "CAL_STATUS",       1 }, 
        { "CAL_COMMENT",     1 },
#endif
	{ "NULL",          NULL }
    };

    if (q == NULL) {
       printf("\nError: File Descriptor Record did not have a Data Quality Summary record listed\n");
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (q->seq_num !=0) {
           if (i<=n_data) {
              q=q->next;
           }
           else
              return(DECODE_ERROR);
           i++;
       }
    }
    
    printf("\n Decode Quality Sum record \n");
        
    /* process the record */
    d = &(q->desc);
    
    while (multi) {
          if ( !(ret=next_token( token )) ) {
	     printf("\n End of File");
	     return(0);
	  }
	  
	  if ( !(strcmp( token, "END_OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     /* compare END_OBJECT to OBJECT tokens */
	     if (!(strcmp( token, Object_tokens )) ) {
	        multi--;
	     }
	  }
	  else {
	     /* find a match for Object token in token list */
	     i=0;
	     while (strcmp(token_list[i].token, "NULL")) {
		 if ( !(strcmp(token_list[i].token, token)) ) {
		     switch (token_list[i].format_type) {
			 case 1:
			    next_token( token );
			    /* remove quote */
			    len = strlen(token);
			    if (len >2) {
			       strncpy(token, &token[1],  len-2);
			       token[len-2]='\0';
			    }
			    else
			       strcpy(token, "");
			    break;
			 case 2:
			    get_int(&la_int);
			    break;
			 case 3:
			    get_float(&la_float);
			    break;
			 case 4:
			    get_double(&la_double);
			    break;
			 case 5:
			 default:
			    break;
		     }
		     switch ( i+1) {
			 case 1:
			    q->seq_num = la_int;
			    break;
			 case 2:
			    strncpy(q->chan_ind, token, 4);
			    break;
			 case 3:
			    strncpy(q->cali_date, token, 6);
			    break;
			 case 4:
			    q->nchn = la_int;
			    break;
			 case 5:
			    q->islr = la_float;
			    break;
			 case 6:
			    q->pslr = la_float;
			    break;
			 case 7:
			    q->azi_ambig = la_float;
			    break;
			 case 8:
			    q->rng_ambig = la_float;
			    break;
			 case 9:
			    q->snr = la_float;
			    break;
			 case 10:
			    q->ber = la_float;
			    break;
			 case 11:
			    q->rng_res = la_float;
			    break;
			 case 12:
			    q->azi_res = la_float;
			    break; 
			 case 13:
			    q->rad_res = la_float;
			    break;
			 case 14:
			    q->dyn_rng = la_float;
			    break;
			 case 15:
			    q->abs_rad_unc_db = la_float;
			    break;
			 case 16:
			    q->abs_rad_unc_deg = la_float;
			    break;
			 case 17:
			    get_array(q->rel_rad_unc[0],  q->nchn,  2);
			    break;
			 case 18:
			    get_array(q->rel_rad_unc[1],  q->nchn,  2);
			    break;
			 case 19:
			    q->alt_locerr = la_float;
			    break;
			 case 20:
			    q->crt_locerr = la_float;
			    break; 
			 case 21:
			    q->alt_scale = la_float;
			    break;
			 case 22:
			    q->crt_scale = la_float;
			    break;
			 case 23:
			    q->dis_skew = la_float;
			    break;
			 case 24:
			    q->ori_err = la_float;
			    break;
			 case 25:
			    get_array(q->misreg[0],  q->nchn,  2);
			    break;
			 case 26:
			    get_array(q->misreg[1],  q->nchn,  2);
			    break;
#ifndef PRE_RADARSAT
			 case 27:
			    q->nesz  = la_float;
			    break;
			 case 28:
			    q->enl = la_float;
			    break;
			 case 29:
			    strncpy(q->tb_update, token, 8);
			    break;
			 case 30:
			    strncpy(q->cal_status, token, 16);
			    break;
			 case 31:
			    strncpy(q->cal_comment, token, 200);
			    break;
#endif
                         default:
                            break;
		     }
		     break;
		 }
		 i++;
	     }
	  }
    }

    /* Fill in the record type info */
    d->rec_seq  = Record_count++;
    d->rec_sub1 = LQ1SUB;
    d->rec_type = LQTYPE;
#ifdef PRE_RADARSAT
    d->rec_sub2 = COM2SUB;
#else
    d->rec_sub2 = LQ2SUB;
#endif
    d->rec_sub3 = LQ3SUB;
    d->length = t->l_qual_sum;
    return(1);
}
int Decode_ODL_DHR(SARL_ptr *tree)
{
    Data_Hist *h = tree->histogram;
    Hist_Data_Set *hd;
    Sarl_Desc_Rec *t = &(tree->descript);
    desc_rec *d;
    char token[MAX_LINE];
    char *Object_tokens[] = {"Data_Hist", "Hist_Data_Set", "NULL"};
    int n_data = tree->descript.n_data_hist;
    int i=1;
    int la_int;
    float la_float;
    double la_double;
    int len;
    int multi=1;
    int ret;
    long *dv;
    int object_set[] = { 1,  0};
    int found;

    static User_token_def token_list[] = {
        { "SEQ_NUM",           2 }, { "SAR_CHAN",        2 },
        { "NTAB",              2 }, { "LTAB",            2 },
        { "HIST_DESC",         1 }, { "NREC",            2 },
        { "TAB_SEQ",           2 }, { "NBIN",            2 },
        { "NS_LIN",            2 }, { "NS_PIX",          2 },
        { "NGRP_LIN",          2 }, { "NGRP_PIX",        2 },
        { "NSAMP_LIN",         2 }, { "NSAMP_PIX",       2 },
        { "MIN_SMP",           3 }, { "MAX_SMP",         3 },
        { "MEAN_SMP",          3 }, { "STD_SMP",         3 },
        { "SMP_INC",           3 }, { "MIN_HIST",        3 },
        { "MAX_HIST",          3 }, { "MEAN_HIST",       3 },
        { "STD_HIST",          3 }, { "NHIST",           2 },
        { "DATA_VALUES_HIST",  5 }, { "NULL",         NULL }
    };
    
    if (h == NULL) {
       printf("\nError: File Descriptor Record did not have a Data Histogram record listed\n");
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (h->seq_num !=0) {
           if (i<=n_data) {
              h=h->next;
           }
           else
              return(DECODE_ERROR);
           i++;
       }
    }
    
    printf("\n Decode Data Histogram record \n");
    
    /* process the record */
    d = &(h->desc); 
    
    while (multi) {
    
          if ( !(ret=next_token( token )) ) {
	     printf("\n End of File");
	     return(0);
	  }
	  
          /* is it an embedded new object? */
	  if ( !(strcmp( token, "OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     i=0; found = 0;
	     while( strcmp( Object_tokens[i], "NULL") ) {
	        /* which one is it? */
	        if (!(strcmp( token, Object_tokens[i] )) ) {
		   object_set[i]++;
		   found=1;
		}
		i++;
	     }
	     if (!found) {
		printf("\n OBJECT token not in Object list - %s", token);
		return(0);
	     }
	     /* keep track of the OBJECT depth */
	     multi++;
	  }
	  else if ( !(strcmp( token, "END_OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     /* compare END_OBJECT to OBJECT tokens */
	     i=0; found=0;
	     while( strcmp( Object_tokens[i], "NULL") ) {
	     	/* which one is it? */
	        if (!(strcmp( token, Object_tokens[i] )) ) {
	           multi--;
		   found=1;
		   object_set[i]--;
		   if (object_set[i]) {
		       printf("\n Object never set - %s",  token);
		       return(0);
		   }
		   switch (i) {
		       case 0:
		            break;
		       case 1:
		       	    if (hd!=NULL) hd=hd->next;
			    break;
		   }
		}
		i++;
	     }
	     if (!found) {
		printf("\n END_OBJECT token not in Object list - %s", token);
		return(0);
	     }
	  }
	  else {
	     /* find a match for Object token in token list */
	     i=0;
	     while (strcmp(token_list[i].token, "NULL")) {
		 if ( !(strcmp(token_list[i].token, token)) ) {
		     switch (token_list[i].format_type) {
			 case 1:
			    next_token( token );
			    /* remove quote */
			    len = strlen(token);
			    if (len >2) {
			       strncpy(token, &token[1],  len-2);
			       token[len-2]='\0';
			    }
			    else
			       strcpy(token, "");
			    break;
			 case 2:
			    get_int(&la_int);
			    break;
			 case 3:
			    get_float(&la_float);
			    break;
			 case 4:
			    get_double(&la_double);
			    break;
			 case 5:
			 default:
			    break;
		     }
		     switch ( i+1) {
			 case 1:
			    h->seq_num = la_int;
			    break;
			 case 2:
			    h->sar_chan = la_int;
			    break;
			 case 3:
			    h->ntab = la_int;
			    hd = Allocate_Hist_Data_Set( h );
			    break;
                         case 4:
			    h->ltab = la_int;
			    break;
                         case 5:
			    strncpy(hd->hist_desc, token, 32);
			    break;
                         case 6:
			    hd->nrec = la_int;
			    break;
                         case 7:
			    hd->tab_seq = la_int;
			    break;
                         case 8:
			    hd->nbin = la_int;
			    break;
                         case 9:
			    hd->ns_lin = la_int;
			    break;
                         case 10:
			    hd->ns_pix = la_int;
			    break;
                         case 11:
			    hd->ngrp_lin = la_int;
			    break;
                         case 12:
			    hd->ngrp_pix = la_int;
			    break;
                         case 13:
			    hd->nsamp_lin = la_int;
			    break;
                         case 14:
			    hd->nsamp_pix = la_int;
			    break;
                         case 15:
			    hd->min_smp = la_float;
			    break;
                         case 16:
			    hd->max_smp = la_float;
			    break;
                         case 17:
			    hd->mean_smp = la_float;
			    break;
                         case 18:
			    hd->std_smp = la_float;
			    break;
                         case 19:
			    hd->smp_inc = la_float;
			    break;
                         case 20:
			    hd->min_hist = la_float;
			    break;
                         case 21:
			    hd->max_hist = la_float;
			    break;
                         case 22:
			    hd->mean_hist = la_float;
			    break;
                         case 23:
			    hd->std_hist = la_float;
			    break;
                         case 24:
			    hd->nhist = la_int;
			    dv = Allocate_DH_table( hd );
			    break;
                         case 25:
			    get_array(dv, hd->nhist,  4);
			    break;
                         default:
                            break;
		     }
                     break;
		 }
		 i++;
	     }
	  }
    }

    /* Fill in the record type info */
    d->rec_seq  = Record_count++;
    d->rec_sub1 = LH1SUB;
    d->rec_type = LHTYPE;
#ifdef PRE_RADARSAT
    d->rec_sub2 = COM2SUB;
#else
    d->rec_sub2 = LH2SUB;
#endif
    d->rec_sub3 = LH3SUB;
    d->length = t->l_data_hist;
    return(1);
}
int Decode_ODL_RSR(SARL_ptr *tree)
{
    Rng_Spec *r = tree->spectra;
    Sarl_Desc_Rec *t = &(tree->descript);
    desc_rec *d;
    char token[MAX_LINE];
    char Object_tokens[] = {"Rng_Spec"};
    int n_data = tree->descript.n_rang_spec;
    int i=1;
    int la_int;
    float la_float;
    double la_double;
    int len;
    int multi=1;
    int ret;

    static User_token_def token_list[] = {
        { "SEQ_NUM",         2 }, { "SAR_CHAN",        2 },
        { "N_DSET",          2 }, { "DSET_SIZE",       2 },
        { "REQ_RECS",        2 }, { "TABLE_NO",        2 },
        { "N_PIXELS",        2 }, { "PIXEL_OFFSET",    2 },
        { "N_LINES",         2 }, { "FIRST_FREQ",      3 },
        { "LAST_FREQ",       3 }, { "MIN_POWER",       3 },
        { "MAX_POWER",       3 }, { "SPARE_RSR_1",     1 },
        { "SPARE_RSR_2",     1 }, { "N_BINS",          2 },
        { "DATA_VALUES_SPEC",5 }, { "NULL",         NULL }
    };
    
    if (r == NULL) {
       printf("\nError: File Descriptor Record did not have a Range Spectra record listed\n");
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (r->seq_num !=0) {
           if (i<=n_data) {
              r=r->next;
           }
           else
              return(DECODE_ERROR);
           i++;
       }
    }
    /* process the record */
    d = &(r->desc);
    printf("\n Decode Range Spectra record \n");
        
    while (multi) {
          if ( !(ret=next_token( token )) ) {
	     printf("\n End of File");
	     return(0);
	  }
	  
	  if ( !(strcmp( token, "END_OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     /* compare END_OBJECT to OBJECT tokens */
	     if (!(strcmp( token, Object_tokens )) ) {
	        multi--;
	     }
	  }
	  else {
	     /* find a match for Object token in token list */
	     i=0;
	     while (strcmp(token_list[i].token, "NULL")) {
		 if ( !(strcmp(token_list[i].token, token)) ) {
		     switch (token_list[i].format_type) {
			 case 1:
			    next_token( token );
			    /* remove */
			    len = strlen(token);
			    if (len >2) {
			       strncpy(token, &token[1],  len-2);
			       token[len-2]='\0';
			    }
			    else
			       strcpy(token, "");
			    break;
			 case 2:
			    get_int(&la_int);
			    break;
			 case 3:
			    get_float(&la_float);
			    break;
			 case 4:
			    get_double(&la_double);
			    break;
			 case 5:
			 default:
			    break;
		     }
		     switch ( i+1) {
			 case 1:
			    r->seq_num = la_int;
			    break;
			 case 2:
			    r->sar_chan = la_int;
			    break;
			 case 3:
			    r->n_dset = la_int;
			    break;
			 case 4:
			    r->dset_size = la_int;
			    break;
			 case 5:
			    r->req_recs = la_int;
			    break;
			 case 6:
			    r->table_no = la_int;
			    break;
			 case 7:
			    r->n_pixels = la_int;
			    break;
			 case 8:
			    r->pixel_offset = la_int;
			    break;
			 case 9:
			    r->n_lines = la_int;
			    break;
			 case 10:
			    r->first_freq = la_float;
			    break;
			 case 11:
			    r->last_freq = la_float;
			    break;
			 case 12:
			    r->min_power = la_float;
			    break; 
			 case 13:
			    r->max_power = la_float;
			    break;
			 case 14:
			    strncpy(r->spare_rsr_1, token, 16);
			    break;
			 case 15:
			    strncpy(r->spare_rsr_2, token, 16);
			    break;
			 case 16:
                            r->n_bins = la_int;
			    break;
			 case 17:
			    get_array(r->data_values_spec, r->n_bins,  2);
			    break;
                         default:
                            break;
		     }
		     break;
		 }
		 i++;
	     }
	  }
    }

    /* Fill in the record type info */
    d->rec_seq  = Record_count++;
    d->rec_sub1 = LZ1SUB;
    d->rec_type = LZTYPE;
#ifdef PRE_RADARSAT
    d->rec_sub2 = COM2SUB;
#else
    d->rec_sub2 = LZ2SUB;
#endif
    d->rec_sub3 = LZ3SUB;
    d->length = t->l_rang_spec;
    return(1);
}

int Decode_ODL_DER(SARL_ptr *tree)
{
    Digital_Elev *e = tree->elevation;
    Dem_Desc *ed;
    Corner_Pts *ec;
    Sarl_Desc_Rec *t = &(tree->descript);
    desc_rec *d;
    char token[MAX_LINE];
    char *Object_tokens[] = {"Digital_Elev", "Dem_Desc", "Corner_Pts", "NULL"};
    int object_set[] = { 1,  0,  0 };
    int n_data = tree->descript.n_dem_desc;
    int i=1;
    int la_int;
    float la_float;
    double la_double;
    int len;
    int multi=1;
    int ret;
    int found;
    
    static User_token_def token_list[] = {
    	{ "SEQ_NUM",          2 },    { "TTL_NUM_SETS",       2 }, 
	{ "DEM_SEQ_NUM",      2 },    { "SOURCE_DEM",         1 }, 
	{ "HT_REF_NAME",      1 },    { "GEN_METHOD",         1 }, 
	{ "RASTER_UNIT",      1 },    { "PRESENTATION_PROJ",  1 }, 
	{ "NS_RASTER",        3 },    { "EW_RASTER",          3 }, 
	{ "RESAMPLE",         1 },    { "HEIGHT_ERR",         3 }, 
	{ "NS_LOC_ERR",       3 },    { "EW_LOC_ERR",         3 }, 
	{ "MAX_HEIGHT",       3 },    { "MIN_HEIGHT",         3 }, 
	{ "MEAN_HEIGHT",      3 },    { "STD_HEIGHT",         3 }, 
	{ "NUM_POLYS",        2 },    { "POLY_SEQ_NUM",       2 },    
	{ "NUM_CRNR_PTS",     2 },    { "CP_LAT_1",           3 },    
	{ "CP_LON_1",         3 },    { "NULL",            NULL }
    };

    if (e == NULL) {
       printf("\nError: File Descriptor Record did not have a Digital Elevation record listed\n");
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (e->seq_num !=0) {
           if (i<=n_data) {
              e=e->next;
           }
           else
              return(DECODE_ERROR);
           i++;
       }
    }
    
    printf("\n Decode Digital Elevation record \n");
      
    /* process the record */
    d = &(e->desc);  
    
    while (multi) {
    
          if ( !(ret=next_token( token )) ) {
	     printf("\n End of File");
	     return(0);
	  }
	  
          /* is it an embedded new object? */
	  if ( !(strcmp( token, "OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     i=0; found = 0;
	     while( strcmp( Object_tokens[i], "NULL") ) {
	        /* which one is it? */
	        if (!(strcmp( token, Object_tokens[i] )) ) {
		   object_set[i]++;
		   found=1;
		}
		i++;
	     }
	     if (!found) {
		printf("\n OBJECT token not in Object list - %s", token);
		return(0);
	     }
	     /* keep track of the OBJECT depth */
	     multi++;
	  }
	  
	  if ( !(strcmp( token, "END_OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     /* compare END_OBJECT to OBJECT tokens */
	     i=0; found=0;
	     while( strcmp( Object_tokens[i], "NULL") ) {
	     	/* which one is it? */
	        if (!(strcmp( token, Object_tokens[i] )) ) {
	           multi--;
		   found=1;
		   object_set[i]--;
		   if (object_set[i]) {
		       printf("\n Object never set - %s",  token);
		       return(0);
		   }
		   switch (i) {
		       case 0:
		            break;
		       case 1:
		            if (ed!= NULL) ed=ed->next;
			    break;
		       case 2:
                            if (ec!= NULL) ec=ec->next;
		            break;
		   }
		}
		i++;
	     }
	     if (!found) {
		printf("\n END_OBJECT token not in Object list - %s", token);
		return(0);
	     }
	  }
	  else {
	     /* find a match for Object token in token list */
	     i=0;
	     while (strcmp(token_list[i].token, "NULL")) {
		 if ( !(strcmp(token_list[i].token, token)) ) {
		     switch (token_list[i].format_type) {
			 case 1:
			    next_token( token );
			    /* remove quote */
			    len = strlen(token);
			    if (len >2) {
			       strncpy(token, &token[1],  len-2);
			       token[len-2]='\0';
			    }
			    else
			       strcpy(token, "");
			    break;
			 case 2:
			    get_int(&la_int);
			    break;
			 case 3:
			    get_float(&la_float);
			    break;
			 case 4:
			    get_double(&la_double);
			    break;
			 case 5:
			 default: /* arrays handled on an individual basis */
			    break;
		     }
		     switch ( i+1 ) {
			 case 1:
			    e->seq_num = la_int;
			    break;
			 case 2:
			    e->ttl_num_sets = la_int;
			    break;
			 case 3:
			    e->DEM_seq_num = la_int;
			    break;
			 case 4:
			    strncpy( e->source_DEM,  token, 32 );
			    break;
			 case 5:
			    strncpy( e->HT_ref_name,  token, 32 );
			    break;
			 case 6:
			    strncpy( e->gen_method,  token, 32 );
			    break;
			 case 7:
			    strncpy( e->raster_unit,  token, 12 );
			    break;
			 case 8:
			    strncpy( e->presentation_proj,  token, 32 );
			    break;
			 case 9:
			    e->NS_raster = la_float;
			    break;
			 case 10:
			    e->EW_raster = la_float;
			    break;
			 case 11:
			    strncpy( e->resample,  token, 32 );
			    break;
			 case 12:
			    e->height_err = la_float;
			    break;
			 case 13:
			    e->NS_loc_err = la_float;
			    break;
			 case 14:
			    e->EW_loc_err = la_float;
			    break;
			 case 15:
			    e->max_height = la_float;
			    break;
			 case 16:
			    e->min_height = la_float;
			    break;
			 case 17:
			    e->MEAN_height = la_float;
			    break;
			 case 18:
			    e->STD_height = la_float;
			    break;
			 case 19:
                            e->num_polys = la_int;
			    ed= Allocate_DEM_sets( e );
                            break;
                         case 20:
                            ed->poly_seq_num = la_int;
			    break;
                         case 21:
                            ed->num_crnr_pts = la_int;
			    ec = Allocate_DEM_pts( ed );
			    break;
                         case 22:
                            ec->cp_lat_1 = la_float;
			    break;
                         case 23:
                            ec->cp_lon_1 = la_float;
			    break;
			 default :
			    break;
		     }
		     break;
		 }
		 i++;
	     }
	  }
    }

    /* Fill in the record type info */
    d->rec_seq  = Record_count++;
    d->rec_sub1 = LE1SUB;
    d->rec_type = LETYPE;
#ifdef PRE_RADARSAT
    d->rec_sub2 = COM2SUB;
#else
    d->rec_sub2 = LE2SUB;
#endif
    d->rec_sub3 = LE3SUB;
    d->length = t->l_dem_desc;
    return(1);
}

int Decode_ODL_DPR(SARL_ptr *tree)
{
    Proc_Parm *e = tree->detail;
    Sarl_Desc_Rec *t = &(tree->descript);
    desc_rec *d;
    char token[MAX_LINE];
    char *Object_tokens[] = {"Proc_Parm", "Beam_Info", "Pix_Count", 
                             "Temp_Rec",  "Dopcen_Est",  "SRGR_Coefset", 
			     "NULL"};
    int object_set[] = { 1,  0,  0,  0, 0, 0};
    int n_data = tree->descript.n_det_proc;
    Beam_Info    *bi;
    Pix_Count    *pc;
    Temp_Rec     *tr;
    Dopcen_Est   *de;
    SRGR_Coefset *sc;
    int i=1;
    int la_int;
    float la_float;
    double la_double;
    int len;
    int multi=1;
    int ret;
    int found;
    
    static User_token_def token_list[] = {
    	{ "SEQ_NUM",            2 },     { "SPARE_DPP_1",          1 }, 
	{ "INP_MEDIA",          1 },     { "N_TAPE_ID",            2 }, 
	{ "TAPE_ID",            1 }, 
	{ "EXP_ING_START",      1 },     { "EXP_ING_STOP",         1 }, 
	{ "ACG_ING_START",      1 },     { "ACG_ING_STOP",         1 }, 
	{ "PROC_START",         1 },     { "PROC_STOP",            1 }, 
	{ "MN_SIG_LEV",         5 },     { "SRC_DATA_IND",         2 }, 
	{ "MISS_LN",            2 },     { "REJ_LN",               2 }, 
	{ "LARGE_GAP",          2 },     { "BIT_ERROR_RATE",       3 }, 
	{ "FM_CRC_ERR",         3 },     { "DATE_INCONS",          2 }, 
	{ "PRF_CHANGES",        2 },     { "DELAY_CHANGES",        2 }, 
	{ "SKIPD_FRAMS",        2 },     { "REJ_BF_START",         2 }, 
	{ "REJ_FEW_FRAM",       2 },     { "REJ_MANY_FRAM",        2 }, 
	{ "REJ_MCHN_ERR",       2 },     { "REJ_VCHN_ERR",         2 }, 
	{ "REJ_REC_TYPE",       2 },     { "PRD_QUAL_IND",         2 }, 
	{ "QC_RATING",          1 },     { "QC_COMMENT",           1 }, 
	{ "SENS_CONFIG",        1 },     { "SENS_ORIENT",          1 }, 
	{ "SYCH_MARKER",        1 },     { "RNG_REF_SRC",          1 }, 
	{ "RNG_AMP_COEF",       5 },     { "RNG_PHAS_COEF",        5 }, 
	{ "ERR_AMP_COEF",       5 },     { "ERR_PHAS_COEF",        5 }, 
	{ "PULSE_BANDW",        2 },     { "ADC_SAMP_RATE",        1 }, 
	{ "REP_AGC_ATTN",       3 },     { "GN_CORCTN_FCTR",       3 }, 
	{ "REP_ENERGY_GN",      3 },     { "ORB_DATA_SRC",         1 }, 
	{ "PULSE_CNT_1",        2 },     { "PULSE_CNT_2",          2 }, 
	{ "BEAM_EDGE_RQD",      1 },     { "BEAM_EDGE_CONF",       3 }, 
	{ "PIX_OVERLAP",        2 },     { "N_BEAMS",              2 }, 
	{ "BEAM_TYPE",          1 },     { "BEAM_LOOK_SRC",        1 }, 
	{ "BEAM_LOOK_ANG",      3 }, 
	{ "PRF",                3 },     { "N_PIX_UPDATES",        2 }, 
	{ "PIX_UPDATES",        1 },     { "N_PIX",                5 }, 
	{ "PWIN_START",         3 },     { "PWIN_END",             3 }, 
	{ "RECD_TYPE",          1 },     { "TEMP_SET_INC",         3 }, 
	{ "N_TEMP_SET",         2 },     { "TEMP_SET",             5 }, 
	{ "N_IMAGE_PIX",        2 },     { "PRC_ZERO_PIX",         3 }, 
	{ "PRC_SATUR_PIX",      3 },     { "IMG_HIST_MEAN",        3 }, 
	{ "IMG_CUMU_DIST",      5 },     { "PRE_IMG_GN",           3 }, 
	{ "POST_IMG_GN",        3 },     { "DOPCEN_INC",           3 }, 
	{ "N_DOPCEN",           2 },     { "DOPCEN_CONF",          3 }, 
	{ "DOPCEN_REF_TIM",     3 },     { "DOPCEN_COEF",          5 }, 
	{ "DOPAMB_ERR",         2 },     { "DOPAMB_CONF",          3 }, 
	{ "EPH_ORB_DATA",       5 },     { "APPL_TYPE",            3 }, 
	{ "FIRST_LNTIM",        4 },     { "LNTIM_INC",            4 }, 
	{ "N_SRGR",             2 },     { "SRGR_UPDATE",          1 }, 
	{ "SRGR_COEF",          5 },     { "PIXEL_SPACING",        3 }, 
	{ "PICS_REQD",          1 },     { "WO_NUMBER",            1 }, 
	{ "WO_DATE",            1 },     { "SATELLITE_ID",         1 }, 
	{ "USER_ID",            1 },     { "COMPLETE_MSG",         1 }, 
	{ "SCENE_ID",           1 },     { "DENSITY_IN",           1 }, 
	{ "MEDIA_ID",           1 },     { "ANGLE_FIRST",          3 }, 
	{ "ANGLE_LAST",         3 },     { "PROD_TYPE",            1 }, 
	{ "MAP_SYSTEM",         1 },     { "CENTRE_LAT",           4 },
	{ "CENTRE_LONG",        1 },     { "SPAN_X",               1 }, 
	{ "SPAN_Y",             1 },     { "APPLY_DIM",            1 }, 
	{ "DENSITY_OUT",        1 },     { "SPARE_DPP_2",          1 },  
	{ "NULL",            NULL }
    };

    if (e == NULL) {
       printf("\nError: File Descriptor Record did not have a Detail Processing record listed\n");
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (e->seq_num !=0) {
           if (i<=n_data) {
              e=e->next;
           }
           else
              return(DECODE_ERROR);
           i++;
       }
    }
    
    printf("\n Decode Detail Processing record \n");
      
    /* process the record */
    d = &(e->desc);  
    
    while (multi) {
          if ( !(ret=next_token( token )) ) {
	     printf("\n End of File");
	     return(0);
	  }
	  
          /* is it an embedded new object? */
	  if ( !(strcmp( token, "OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     i=0; found = 0;
	     while( strcmp( Object_tokens[i], "NULL") ) {
	        /* which one is it? */
	        if (!(strcmp( token, Object_tokens[i] )) ) {
		   object_set[i]++;
		   found=1;
		}
		i++;
	     }
	     if (!found) {
		printf("\n OBJECT token not in Object list - %s", token);
		return(0);
	     }
	     /* keep track of the OBJECT depth */
	     multi++;
	  }
	  
	  if ( !(strcmp( token, "END_OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     /* compare END_OBJECT to OBJECT tokens */
	     i=0; found=0;
	     while( strcmp( Object_tokens[i], "NULL") ) {
	     	/* which one is it? */
	        if (!(strcmp( token, Object_tokens[i] )) ) {
	           multi--;
		   found=1;
		   object_set[i]--;
		   if (object_set[i]) {
		       printf("\n Object never set - %s",  token);
		       return(0);
		   }
		   switch (i) {
		       case 0:
		            break;
		       case 1:
		            if (bi!=NULL) bi=bi->next;
			    break;
		       case 2:
		            if (pc!=NULL) pc = pc->next;
			    break;
		       case 3:
		            if (tr!=NULL) tr = tr->next;
			    break;
		       case 4:
		            if (de!=NULL) de = de->next;
			    break;
		       case 5:
		            if (sc!=NULL) sc = sc->next;
			    break;
		   }
		}
		i++;
	     }
	     if (!found) {
		printf("\n END_OBJECT token not in Object list - %s", token);
		return(0);
	     }
	  }
	  else {
	     /* find a match for Object token in token list */
	     i=0;
	     while (strcmp(token_list[i].token, "NULL")) {
		 if ( !(strcmp(token_list[i].token, token)) ) {
		     switch (token_list[i].format_type) {
			 case 1:
			    next_token( token );
			    /* remove quote */
			    len = strlen(token);
			    if (len >2) {
			       strncpy(token, &token[1],  len-2);
			       token[len-2]='\0';
			    }
			    else
			       strcpy(token, "");
			    break;
			 case 2:
			    get_int(&la_int);
			    break;
			 case 3:
			    get_float(&la_float);
			    break;
			 case 4:
			    get_double(&la_double);
			    break;
			 case 5:
			 default: /* arrays handled on an individual basis */
			    break;
		     }
		     switch ( i+1 ) {
			 case 1:
			    e->seq_num = la_int;
			    break;
			 case 2:
			    strncpy(e->spare_dpp_1,  token, 4);
			    break;
			 case 3:
			    strncpy(e->inp_media,  token, 3);
			    break;
			 case 4:
			    e->n_tape_id = la_int;
			    break;
			 case 5:
			    for (i=0; i<e->n_tape_id;i++) {
				strncpy( e->tape_id[i],  token, 8 );
			    }
			    break;
			 case 6:
			    strncpy( e->exp_ing_start,  token, 21 );
			    break;
			 case 7:
			    strncpy( e->exp_ing_stop,  token, 21 );
			    break;
			 case 8:
			    strncpy( e->act_ing_start,  token, 21 );
			    break;
			 case 9:
			    strncpy( e->act_ing_stop,  token, 21 );
			    break;
			 case 10:
			    strncpy( e->proc_start,  token, 21 );
			    break;
			 case 11:
			    strncpy( e->proc_stop,  token, 21 );
			    break;
			 case 12:
			    get_array (e->mn_sig_lev,  10,  2);
			    break;
			 case 13:
			    e->src_data_ind = la_int;
			    break;
			 case 14:
			    e->miss_ln = la_int;
			    break;
			 case 15:
			    e->rej_ln = la_int;
			    break;
			 case 16:
			    e->large_gap = la_int;
			    break;
			 case 17:
			    e->bit_error_rate = la_float;
			    break;
			 case 18:
			    e->fm_crc_err = la_float;
			    break;
                         case 19:
                            e->date_incons = la_int;
			    break;
                         case 20:
                            e->prf_changes = la_int;
			    break;
                         case 21:
                            e->delay_changes = la_int;
			    break;
                         case 22:
                            e->skipd_frams = la_int;
			    break;
                         case 23:
                            e->rej_bf_start = la_int;
			    break;
                         case 24:
                            e->rej_few_fram = la_int;
			    break;
                         case 25:
                            e->rej_many_fram = la_int;
			    break;
                         case 26:
                            e->rej_mchn_err = la_int;
			    break;
                         case 27:
                            e->rej_vchn_err = la_int;
			    break;
                         case 28:
                            e->rej_rec_type = la_int;
			    break;
                         case 29:
                            e->prd_qual_ind = la_int;
			    break;
			 case 30:
			    strncpy( e->qc_rating,  token, 6 );
			    break;
			 case 31:
			    strncpy( e->qc_comment,  token, 80 );
			    break;
			 case 32:
			    strncpy( e->sens_config,  token, 10 );
			    break;
			 case 33:
			    strncpy( e->sens_orient,  token, 9 );
			    break;
			 case 34:
			    strncpy( e->sych_marker,  token, 8 );
			    break;
			 case 35:
			    strncpy( e->rng_ref_src,  token, 12 );
			    break;
                         case 36:
                            get_array( e->rng_amp_coef,  4,  2);
			    break;
                         case 37:
                            get_array( e->rng_phas_coef,  4,  2);
			    break;
                         case 38:
                            get_array( e->err_amp_coef,  4,  2);
			    break;
                         case 39:
                            get_array( e->err_phas_coef,  4,  2);
			    break;
                         case 40:
                            e->pulse_bandw = la_int;
			    break;
			 case 41:
			    strncpy( e->adc_samp_rate,  token, 5 );
			    break;
                         case 42:
                            e->rep_agc_attn = la_float;
			    break;
                         case 43:
                            e->gn_corctn_fctr = la_float;
			    break;
                         case 44:
                            e->rep_energy_gn = la_float;
			    break;
			 case 45:
			    strncpy( e->orb_data_src,  token, 11 );
			    break;
                         case 46:
                            e->pulse_cnt_1 = la_int;
			    break;
                         case 47:
                            e->pulse_cnt_2 = la_int;
			    break;
			 case 48:
			    strncpy( e->beam_edge_rqd,  token, 3 );
			    break;
                         case 49:
                            e->beam_edge_conf = la_float;
			    break;
                         case 50:
                            e->pix_overlap = la_int;
			    break;
                         case 51:
                            e->n_beams = la_int;
			    bi = Allocate_Beam_Info( e );
			    break;
			 case 52:
			    strncpy( bi->beam_type,  token, 3 );
			    break;
			 case 53:
			    strncpy( bi->beam_look_src,  token, 9 );
			    break;
                         case 54:
                            bi->beam_look_ang = la_float;
			    break;
                         case 55:
                            bi->prf = la_float;
			    break;
                         case 56:
                            e->n_pix_updates = la_int;
			    pc = Allocate_Pix_Count( e );
			    break;
			 case 57:
			    strncpy(pc->pix_update,  token, 21 );
			    break;
                         case 58:
                            get_array( pc->n_pix,  4,  1);
			    break;
                         case 59:
                            e->pwin_start = la_float;
			    break;
                         case 60:
                            e->pwin_end = la_float;
			    break;
			 case 61:
			    strncpy( e->recd_type,  token, 8 );
			    break;
                         case 62:
                            e->temp_set_inc = la_float;
			    break;
                         case 63:
                            e->n_temp_set = la_int;
			    tr = Allocate_Temp_Rec( e );
			    break;
                         case 64:
                            get_array( tr->temp_set, 4, 1);
			    break;
                         case 65:
                            e->n_image_pix = la_int;
			    break;
                         case 66:
                            e->prc_zero_pix = la_float;
			    break;
                         case 67:
                            e->prc_satur_pix = la_float;
			    break;
                         case 68:
                            e->img_hist_mean = la_float;
			    break;
                         case 69:
                            get_array( e->img_cumu_dist,  3,  2);
			    break;
                         case 70:
                            e->pre_img_gn = la_float;
			    break;
                         case 71:
                            e->post_img_gn = la_float;
			    break;
                         case 72:
                            e->dopcen_inc = la_float;
			    break;
                         case 73:
                            e->n_dopcen = la_int;
			    de = Allocate_Dopcen_Est( e );
			    break;
                         case 74:
                            de->dopcen_conf = la_float;
			    break;
                         case 75:
                            de->dopcen_ref_tim = la_float;
			    break;
                         case 76:
                            get_array( de->dopcen_coef,  4,  2);
			    break;
                         case 77:
                            e->dopamb_err = la_int;
			    break;
                         case 78:
                            e->dopamb_conf = la_float;
			    break;
                         case 79:
                            get_array( e->eph_orb_data,  7,  2);
			    break;
			 case 80:
			    strncpy( e->appl_type,  token, 12 );
			    break;
                         case 81:
                            e->first_lntim = la_double;
			    break;
                         case 82:
                            e->lntim_inc = la_double;
			    break;
                         case 83:
                            e->n_srgr = la_int;
			    sc = Allocate_SRGR_Coefset( e );
			    break;
			 case 84:
			    strncpy( sc->srgr_update,  token, 21 );
			    break;
                         case 85:
                            get_array( sc->srgr_coef,  6,  2);
			    break;
                         case 86:
                            e->pixel_spacing = la_float;
			    break;
			 case 87:
			    strncpy( e->pics_reqd,  token, 3 );
			    break;
			 case 88:
			    strncpy( e->wo_number,  token, 8 );
			    break;
			 case 89:
			    strncpy( e->wo_date,  token, 20 );
			    break;
			 case 90:
			    strncpy( e->satellite_id,  token, 10 );
			    break;
			 case 91:
			    strncpy( e->user_id,  token, 20 );
			    break;
			 case 92:
			    strncpy( e->complete_msg,  token, 3 );
			    break;
			 case 93:
			    strncpy( e->scene_id,  token, 15 );
			    break;
			 case 94:
			    strncpy( e->density_in,  token, 4 );
			    break;
			 case 95:
			    strncpy( e->media_id,  token, 8 );
			    break;
                         case 96:
                            e->angle_first = la_float;
			    break;
                         case 97:
                            e->angle_last = la_float;
			    break;
			 case 98:
			    strncpy( e->prod_type,  token, 3 );
			    break;
			 case 99:
			    strncpy( e->map_system,  token, 16 );
			    break;
                         case 100:
                            e->centre_lat = la_double;
			    break;
                         case 101:
                            e->centre_long = la_double;
			    break;
                         case 102:
                            e->span_x = la_double;
			    break;
                         case 103:
                            e->span_y = la_double;
			    break;
			 case 104:
			    strncpy( e->apply_dtm,  token, 3 );
			    break;
			 case 105:
			    strncpy( e->density_out,  token, 4 );
			    break;
			 case 106:
			    strncpy( e->spare_dpp_2,  token, 247 );
			    break;
			 default :
			    break;
		     }
		     break;
		 }
		 i++;
	     }
	  }
    }

    /* Fill in the record type info */
    d->rec_seq  = Record_count++;
    d->rec_sub1 = LY1SUB;
    d->rec_type = LYTYPE;
#ifdef PRE_RADARSAT
    d->rec_sub2 = COM2SUB;
#else
    d->rec_sub2 = LY2SUB;
#endif
    d->rec_sub3 = LY3SUB;
    d->length = t->l_det_proc;
    return(1);
}


int Decode_ODL_CDR(SARL_ptr *tree)
{
    Calib_Data *e = tree->calibration;
    Sarl_Desc_Rec *t = &(tree->descript);
    desc_rec *d;
    char token[MAX_LINE];
    char *Object_tokens[] = {"Calib_Data", "NULL"};
    int object_set[] = { 1};
    int n_data = tree->descript.n_cal;
    int i=1;
    int la_int;
    float la_float;
    double la_double;
    int multi=1;
    int ret;
    int len;
    int found;
    
    static User_token_def token_list[] = {
    	{ "SEQ_NUM",            2 },     { "SPARE_CDR_1",          1 }, 
	{ "SPARE_CDR_2",        1 },     { "NULL",              NULL }
    };

    if (e == NULL) {
       printf("\nError: File Descriptor Record did not have a Calibration Data record listed\n");
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (e->seq_num !=0) {
           if (i<=n_data) {
              e=e->next;
           }
           else
              return(DECODE_ERROR);
           i++;
       }
    }
    
    printf("\n Decode Calibration Data record \n");
      
    /* process the record */
    d = &(e->desc);  
    
    while (multi) {
          if ( !(ret=next_token( token )) ) {
	     printf("\n End of File");
	     return(0);
	  }
	  
          /* is it an embedded new object? */
	  if ( !(strcmp( token, "OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     i=0; found = 0;
	     while( strcmp( Object_tokens[i], "NULL") ) {
	        /* which one is it? */
	        if (!(strcmp( token, Object_tokens[i] )) ) {
		   object_set[i]++;
		   found=1;
		}
		i++;
	     }
	     if (!found) {
		printf("\n OBJECT token not in Object list - %s", token);
		return(0);
	     }
	     /* keep track of the OBJECT depth */
	     multi++;
	  }
	  
	  if ( !(strcmp( token, "END_OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     /* compare END_OBJECT to OBJECT tokens */
	     i=0; found=0;
	     while( strcmp( Object_tokens[i], "NULL") ) {
	     	/* which one is it? */
	        if (!(strcmp( token, Object_tokens[i] )) ) {
	           multi--;
		   found=1;
		   object_set[i]--;
		   if (object_set[i]) {
		       printf("\n Object never set - %s",  token);
		       return(0);
		   }
		   switch (i) {
		       case 0:
		            break;
		   }
		}
		i++;
	     }
	     if (!found) {
		printf("\n END_OBJECT token not in Object list - %s", token);
		return(0);
	     }
	  }
	  else {
	     /* find a match for Object token in token list */
	     i=0;
	     while (strcmp(token_list[i].token, "NULL")) {
		 if ( !(strcmp(token_list[i].token, token)) ) {
		     switch (token_list[i].format_type) {
			 case 1:
			    next_token( token );
			    /* remove quote */
			    len = strlen(token);
			    if (len >2) {
			       strncpy(token, &token[1],  len-2);
			       token[len-2]='\0';
			    }
			    else
			       strcpy(token, "");
			    break;
			 case 2:
			    get_int(&la_int);
			    break;
			 case 3:
			    get_float(&la_float);
			    break;
			 case 4:
			    get_double(&la_double);
			    break;
			 case 5:
			 default: /* arrays handled on an individual basis */
			    break;
		     }
		     switch ( i+1 ) {
			 case 1:
			    e->seq_num = la_int;
			    break;
			 case 2:
			    strncpy(e->spare_cdr_1, token, 4);
			    break;
			 case 3:
			    strncpy(e->spare_cdr_2, token, 255 );
			    break;
			 default :
			    break;
		     }
		     break;
		 }
		 i++;
	     }
	  }
    }

    /* Fill in the record type info */
    d->rec_seq  = Record_count++;
    d->rec_sub1 = LB1SUB;
    d->rec_type = LBTYPE;
#ifdef PRE_RADARSAT
    d->rec_sub2 = COM2SUB;
#else
    d->rec_sub2 = LB2SUB;
#endif
    d->rec_sub3 = LB3SUB;
    d->length = t->l_cal;
    return(1);
}


int Decode_ODL_FRR(SARL_ptr *tree)
{
    Fac_Related*r = tree->facility;
    Sarl_Desc_Rec *t = &(tree->descript);
    desc_rec *d;
    char token[MAX_LINE];
    char Object_tokens[] = {"Fac_Related"};
    int n_data = tree->descript.n_fac_data;
    int i=1;
    int la_int;
    float la_float;
    double la_double;
    int len;
    int multi=1;
    int ret;

    static User_token_def token_list[] = {
    	{ "SEQ_NUM",          2 },    { "DATATAKE_ID",          1 }, 
	{ "IMAGE_ID",         1 },    { "CORR_YEAR",            1 }, 
	{ "CORR_GMT",         1 },    { "SITE_NAME",            1 }, 
	{ "DATA_YEAR",        1 },    { "CENTER_GMT",           1 }, 
	{ "CENTER_LAT",       3 },    { "CENTER_LON",           3 }, 
	{ "NEAR_START_LAT",   3 },    { "NEAR_START_LON",       3 }, 
	{ "NEAR_END_LAT",     3 },    { "NEAR_END_LON",         3 }, 
	{ "FAR_START_LAT",    3 },    { "FAR_START_LON",        3 }, 
	{ "FAR_END_LAT",      3 },    { "FAR_END_LON",          3 }, 
	{ "ACTUAL_AZIMUTH",   3 },    { "ACTUAL_RANGE",         3 },
	{ "ACTUAL_PIXELS",    2 },    { "ACTUAL_LINES",         2 },
	{ "TOTAL_PIXELS",     2 },    { "TOTAL_LINES",          2 },
	{ "MEDIA_ID",         1 },    { "START_ADDRESS",        2 },
	{ "END_ADDRESS",      2 },    { "PLATFORM_NAME",        1 },
	{ "SENSOR_MODE",      1 },    { "PRF",                  3 },
	{ "ANT_LOOK_ANGLE",   3 },    { "DATA_RATE",            3 },
	{ "DATA_WIN_POS",     3 },    { "RANGE_GATE_DEL",       3 },
	{ "TRACK_ANGLE",      3 },    { "ASC_DESC",             1 },
	{ "S_C_ALTITUDE",     3 },    { "X_POSITION",           4 },
	{ "Y_POSITION",       4 },    { "Z_POSITION",           4 },
	{ "X_VELOCITY",       4 },    { "Y_VELOCITY",           4 },
	{ "Z_VELOCITY",       4 },    { "ROLL",                 3 },
	{ "YAW",              3 },    { "PITCH",                3 },
	{ "ROLL_FLAG",        2 },    { "YAW_FLAG",             2 },
	{ "PITCH_FLAG",       2 },    { "ROLL_RATE",            3 },
	{ "YAW_RATE",         3 },    { "PITCH_RATE",           3 },
	{ "ROLL_RATE_FLAG",   2 },    { "YAW_RATE_FLAG",        2 },
	{ "PITCH_RATE_FLAG",  2 },    { "NADIR_RADIUS",         3 },
	{ "IMAGE_RADIUS",     3 },    { "INCIDENCE_ANGLE",      3 },
	{ "PROC_VERSION",     1 },    { "PROC_TYPE",            1 },
	{ "TYPE_EPHEMERIS",   1 },    { "LOOKS_AZIMUTH",        3 },
	{ "LOOKS_RANGE",      3 },    { "AZI_WEIGHT_FAC",       3 },
	{ "RANGE_WEIGHT_FAC", 3 },    { "LOOK_ENERGY_EQ",       1 },
	{ "INDUCED_AZIMUTH",  3 },    { "INDUCED_RANGE",        3 },
	{ "GAIN",             3 },    { "SWATH_VELOCITY",       3 },
	{ "SQUINT_ANGLE",     3 },    { "AVG_TERRAIN_HT",       3 },
	{ "PROCESSOR_GAIN",   2 },    { "DESKEW",               1 },
	{ "GND_SLANT_FLAG",   1 },    { "SL_RNG_1ST_PIX",       3 },
	{ "SL_RNG_LAST_PIX",  3 },    { "START_SAMPLE",         2 },
	{ "CLUTTERLOCK_FLG",  1 },    { "DOP_FRQ_CONST",        3 },
	{ "DOP_FRQ_SLOPE",    3 },    { "DOP_FRQ_QUAD",         3 },
	{ "AUTOFOCUS_FLAG",   1 },    { "DOP_FRQ_R_CNST",       3 },
	{ "DOP_FRQ_R_SLOPE",  3 },    { "DOP_FRQ_R_QUAD",       3 },
	{ "AZI_RES",          3 },    { "RNG_RES",              3 },
	{ "AZIMUTH_PIXEL",    3 },    { "RANGE_PIXEL",          3 },
	{ "OBRC_FLAG",        1 },    { "BITS_SAMPLE",          2 },
	{ "CALIB_EST",        3 },    { "BIT_ERR_RATE",         3 },
	{ "SNR",              3 },    { "EST_NOISE_FLR",        3 },
	{ "RADIO_M_RESOL",    3 },    { "SATUR_POINTS",         2 },
	{ "SPEC_FLAG",        1 },  
#ifndef PRE_RADARSAT  
	{ "REPL_AGC",         3 },
	{ "TEMP_RX_LNA",      3 },    { "TEMP_RX_SUB",          3 },
	{ "TEMP_RX_PROT",     3 },    { "TEMP_CAL_SYS",         3 },
	{ "RX_AGC",           3 },    { "PRE_CAL1_POW",         3 },
	{ "PRE_CAL2_POW",     3 },    { "POST_CAL1_POW",        3 },
	{ "POST_CAL2_POW",    3 },    { "REPL_POW",             3 },
	{ "SSAR_ROLL_ANG",    3 },    { "COMMENT",              1 },
#endif
        { "NULL",          NULL }
    };
    
    if (r == NULL) {
          printf("\nError: File Descriptor Record did not have a Facility Related record listed\n");
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (r->seq_num !=0) {
           if (i<=n_data) {
              r=r->next;
           }
           else
              return(DECODE_ERROR);
           i++;
       }
    }
    printf("\n Decode Facility Related record \n");
        
    /* process the record */
    d = &(r->desc);

    while (multi) {
          if ( !(ret=next_token( token )) ) {
	     printf("\n End of File");
	     return(0);
	  }
	  
	  if ( !(strcmp( token, "END_OBJECT")) ) {
	     if ( !(ret=next_token( token )) ) {
	        printf("\n End of File");
	        return(0);
	     }
	     /* compare END_OBJECT to OBJECT tokens */
	     if (!(strcmp( token, Object_tokens )) ) {
	        multi--;
	     }
	  }
	  else {
	     /* find a match for Object token in token list */
	     i=0;
	     while (strcmp(token_list[i].token, "NULL")) {
		 if ( !(strcmp(token_list[i].token, token)) ) {
		     switch (token_list[i].format_type) {
			 case 1:
			    next_token( token );
			    /* remove quote */
			    len = strlen(token);
			    if (len >2) {
			       strncpy(token, &token[1],  len-2);
			       token[len-2]='\0';
			    }
			    else
			       strcpy(token, "");
			    break;
			 case 2:
			    get_int(&la_int);
			    break;
			 case 3:
			    get_float(&la_float);
			    break;
			 case 4:
			    get_double(&la_double);
			    break;
			 case 5:
			 default:
			    break;
			  }
		     switch ( i+1) {
			 case 1:
			    r->seq_num = la_int;
			    break;
			 case 2:
			    strncpy(r->datatake_ID,  token, 14 );
			    break;
			 case 3:
			    strncpy(r->image_ID,  token, 11 );
			    break;
			 case 4:
			    strncpy(r->corr_year, token, 5 );
			    break;
			 case 5:
			    strncpy(r->corr_GMT, token, 17 );
			    break;
			 case 6:
			    strncpy(r->site_name, token, 33 );
			    break;
			 case 7:
			    strncpy(r->data_year, token, 5 );
			    break;
			 case 8:
			    strncpy(r->center_GMT, token, 17 );
			    break;
			 case 9:
			    r->center_LAT = la_float;
			    break;
			 case 10:
			    r->center_LON = la_float;
			    break;
			 case 11:
			    r->near_start_LAT = la_float;
			    break;
			 case 12:
			    r->near_start_LON = la_float;
			    break;
			 case 13:
			    r->near_end_LAT = la_float;
			    break;
			 case 14:
			    r->near_end_LON = la_float;
			    break;
			 case 15:
			    r->far_start_LAT = la_float;
			    break;
			 case 16:
			    r->far_start_LON = la_float;
			    break;
			 case 17:
			    r->far_end_LAT = la_float;
			    break;
			 case 18:
			    r->far_end_LON = la_float;
			    break;
			 case 19:
			    r->actual_azimuth = la_float;
			    break;
			 case 20:
			    r->actual_range = la_float;
			    break;
			 case 21:
			    r->actual_pixels = la_int;
			    break;
			 case 22:
			    r->actual_lines = la_int;
			    break;
			 case 23:
			    r->total_pixels = la_int;
			    break;
			 case 24:
			    r->total_lines = la_int;
			    break;
			 case 25:
			    strncpy(r->media_id, token, 7 );
			    break;
			 case 26:
			    r->start_address = la_int;
			    break;
			 case 27:
			    r->end_address = la_int;
			    break;
			 case 28:
			    strncpy(r->platform_name, token, 17 );
			    break;
			 case 29:
			    strncpy(r->sensor_mode, token, 33);
			    break;
			 case 30:
			    r->PRF = la_float;
			    break;
			 case 31:
			    r->ant_look_angle = la_float;
			    break;
			 case 32:
			    r->data_rate = la_float;
			    break;
			 case 33:
			    r->data_win_pos = la_float;
			    break;
			 case 34:
			    r->range_gate_del = la_float;
			    break;
			 case 35:
			    r->track_angle = la_float;
			    break;
			 case 36:
			    strncpy(r->ASC_DESC, token, 2);
			    break;
			 case 37:
			    r->S_C_altitude = la_float;
			    break;
			 case 38:
			    r->X_position = la_double;
			    break;
			 case 39:
			    r->Y_position = la_double;
			    break;
			 case 40:
			    r->Z_position = la_double;
			    break;
			 case 41:
			    r->X_velocity = la_double;
			    break;
			 case 42:
			    r->Y_velocity = la_double;
			    break;
			 case 43:
			    r->Z_velocity = la_double;
			    break;
			 case 44:
			    r->roll = la_float;
			    break;
			 case 45:
			    r->yaw = la_float;
			    break;
			 case 46:
			    r->pitch = la_float;
			    break;
			 case 47:
			    r->roll_flag = la_int;
			    break;
			 case 48:
			    r->yaw_flag = la_int;
			    break;
			 case 49:
			    r->pitch_flag = la_int;
			    break;
			 case 50:
			    r->roll_rate = la_float;
			    break;
			 case 51:
			    r->yaw_rate = la_float;
			    break;
			 case 52:
			    r->pitch_rate = la_float;
			    break;
			 case 53:
			    r->roll_rate_flag = la_int;
			    break;
			 case 54:
			    r->yaw_rate_flag = la_int;
			    break;
			 case 55:
			    r->pitch_rate_flag = la_int;
			    break;
			 case 56:
			    r->nadir_radius = la_float;
			    break;
			 case 57:
			    r->image_radius = la_float;
			    break;
			 case 58:
			    r->incidence_angle = la_float;
			    break;
			 case 59:
			    strncpy(r->proc_version, token, 8);
			    break;
			 case 60:
			    strncpy(r->proc_type, token, 3);
			    break;
			 case 61:
			    strncpy(r->type_ephemeris, token, 2);
			    break;
			 case 62:
			    r->looks_azimuth = la_float;
			    break;
			 case 63:
			    r->looks_range = la_float;
			    break;
			 case 64:
			    r->azi_weight_fac = la_float;
			    break;
			 case 65:
			    r->range_weight_fac = la_float;
			    break;
			 case 66:
			    strncpy(r->look_energy_eq, token, 4);
			    break;
			 case 67:
			    r->induced_azimuth = la_float;
			    break;
			 case 68:
			    r->induced_range = la_float;
			    break;
			 case 69:
			    r->gain = la_float;
			    break;
			 case 70:
			    r->swath_velocity = la_float;
			    break;
			 case 71:
			    r->squint_angle = la_float;
			    break;
			 case 72:
			    r->avg_terrain_ht = la_float;
			    break;
			 case 73:
			    r->processor_gain = la_int;
			    break;
			 case 74:
			    strncpy(r->deskew, token, 4);
			    break;
			 case 75:
			    strncpy(r->gnd_slant_flag, token, 7);
			    break;
			 case 76:
			    r->sl_rng_1st_pix = la_float;
			    break;
			 case 77:
			    r->sl_rng_last_pix = la_float;
			    break;
			 case 78:
			    r->start_sample = la_int;
			    break;
			 case 79:
			    strncpy(r->clutterlock_flg, token, 4);
			    break;
			 case 80:
			    r->dop_frq_const = la_float;
			    break;
			 case 81:
			    r->dop_frq_slope = la_float;
			    break;
			 case 82:
			    r->dop_frq_quad = la_float;
			    break;
			 case 83:
			    strncpy(r->autofocus_flag, token, 4);
			    break;
			 case 84:
			    r->dop_frq_r_cnst = la_float;
			    break;
			 case 85:
			    r->dop_frq_r_slope = la_float;
			    break;
			 case 86:
			    r->dop_frq_r_quad = la_float;
			    break;
			 case 87:
			    r->azi_res = la_float;
			    break;
			 case 88:
			    r->rng_res = la_float;
			    break;
			 case 89:
			    r->azimuth_pixel = la_float;
			    break;
			 case 90:
			    r->range_pixel = la_float;
			    break;
			 case 91:
			    strncpy(r->OBRC_flag, token, 4);
			    break;
			 case 92:
			    r->bits_sample = la_int;
			    break;
			 case 93:
			    r->calib_est = la_float;
			    break;
			 case 94:
			    r->bit_err_rate = la_float;
			    break;
			 case 95:
			    r->SNR = la_float;
			    break;
			 case 96:
			    r->est_noise_flr = la_float;
			    break;
			 case 97:
			    r->radio_m_resol = la_float;
			    break;
			 case 98:
			    r->satur_points = la_int;
			    break;
			 case 99:
			    strncpy(r->spec_flag, token, 4);
			    break;
#ifndef PRE_RADARSAT 
			 case 100:
			    r->repl_agc = la_float;
			    break;
			 case 101:
			    r->temp_rx_lna = la_float;
			    break;
			 case 102:
			    r->temp_rx_sub = la_float;
			    break;
			 case 103:
			    r->temp_rx_prot = la_float;
			    break;
			 case 104:
			    r->temp_cal_sys = la_float;
			    break;
			 case 105:
			    r->rx_agc = la_float;
			    break;
			 case 106:
			    r->pre_cal1_pow = la_float;
			    break;
			 case 107:
			    r->pre_cal2_pow = la_float;
			    break;
			 case 108:
			    r->post_cal1_pow = la_float;
			    break;
			 case 109:
			    r->post_cal2_pow = la_float;
			    break;
			 case 110:
			    r->repl_pow = la_float;
			    break;
			 case 111:
			    r->ssar_roll_ang = la_float;
			    break;
			 case 112:
			    strncpy(r->comment, token, 100);
			    break;
#endif
                         default:
                            break;
		     }
		     break;
		 }
		 i++;
	     }
	  }
    }

    /* Fill in the record type info */
    d->rec_seq  = Record_count++;
    d->rec_sub1 = LF1SUB;
    d->rec_type = LFTYPE;
#ifdef PRE_RADARSAT
    d->rec_sub2 = COM2SUB;
#else
    d->rec_sub2 = LF2SUB;
#endif
    d->rec_sub3 = LF3SUB;
    d->length = t->l_fac_data;
    return(1);
}



