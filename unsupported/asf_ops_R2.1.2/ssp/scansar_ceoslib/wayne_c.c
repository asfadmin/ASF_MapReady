/* SccsId = @(#)wayne_c.c	2.41 3/24/98 */
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
#include "fortran.h"
#include "error.h"

/* CEOS reader version 1.5 */

struct ceos_struct CEOS;

SARL_ptr *Global_tree;

int Debug;
unsigned char* work;

int *projection;                 /* JMS 2-25-96 */
int *ter_correct;                /* JMS 2-25-96 */

void copy_data(SARL_ptr* tree);
void f2c(char str[], int fsize, int csize);
void f2c2(int len, char *instr, char *out_str);
int printclog(int level, char *errstr);
void CEOS_to_LDR_JEFF(int *istatus);

int write_ceos_ldr(char fname_template[60], char fname[60],
  int *terr_corr_flag, int *proj, int *istatus) {

   FILE *in;
   SARL_ptr* tree;
   int ret,str_len;
   char dummy1_fname[64];
   char dummy2_fname[64];
   char errstr[256];
   int i;

  for (i = 0; i < 64; i++)
    {
    dummy1_fname[i] = '\0';
    dummy2_fname[i] = '\0';
    }

    projection = proj;                     /* JMS 2-25-96 */
    ter_correct = terr_corr_flag;          /* JMS 2-25-96 */

    str_len = 60;
    f2c2(str_len,fname_template,dummy1_fname);
    strcpy(CEOS.infile,dummy1_fname);
    str_len = 60;
    f2c2(str_len,fname,dummy2_fname);
    strcpy(CEOS.outfile,dummy2_fname);
    Debug = 1;
    *istatus = iok;

   
   /* open the leader file */
   if ( ( in = Open_LDR(CEOS.infile, READ_ONLY) ) == NULL) {
      sprintf(errstr,"\n Could not open the leader file for reading --- %s",CEOS.infile);
      *istatus = ierr_2;
      printclog(3,errstr);
      return(0);
   }
 
   /* allocate memory for a SARL structure */

   if ( ( tree = Allocate_SARL() ) == NULL) {
      sprintf(errstr,"\n Failed to Allocate_SARL");
      *istatus = ierr_5;
      printclog(3,errstr);
      return(0);
   }

   SetCurrentSARL( tree );

   /* read the File Descriptor Record, transfer to "descript" structure */

   if ( !Read_FDR_SARL( in, tree ) ) {
      sprintf(errstr,"\n Error in Read_FDR_SARL\n");
      *istatus = ierr_4;
      printclog(3,errstr);
      return(0);
   }

   /* based on the contents of the leader file FDR, allocate space for 
      each of record */

   if ( Allocate_SARL_ODR(tree) ) {
      ret=Read_ALL_SARL( in, tree ); 
      switch (ret) {
           case (END_OF_FILE) :
              copy_data(tree);
              CEOS_to_LDR_JEFF(istatus);
              Free_SARL(GetCurrentSARL());
              break;
           default :
              sprintf(errstr,"\n Aborting the read of the input file\n");
              printclog(3,errstr);
              *istatus = ierr_17;
              Free_SARL(GetCurrentSARL());
              break;
      }
   }
   
   fclose(in);

   return(1);
}

void CEOS_to_LDR_JEFF(int *istatus) {
int status;
    status = Write_CEOS_SARL_JEFF( CEOS.outfile,  GetCurrentSARL());
    if (status == FALSE)
      *istatus = ierr_17;
    else
      *istatus = iok;
}

int Write_CEOS_SARL_JEFF( char name[], SARL_ptr* tree)
{
   FILE* out;
   int ret, len;
   char errstr[256];

   /* open the output CEOS file */
   len=strlen(name);
   if (len) {
      if (( out = Open_LDR(name, WRITE_ONLY) ) == NULL) {
         sprintf(errstr,"\n Could not open the leader file for writing --- %s",name);
         printclog(3,errstr);
         return(FALSE);
      }
   }
   else {
      out = (FILE *) NULL;
   }
   
   if (tree == NULL) return(FALSE);
   
   if (out || Debug) {

      SetCurrentSARL( tree );
      tree->write_count=0;
      ret=FDR_to_LDR( Get_L_FDR(tree), out );
      ret=DSS_to_LDR( Get_L_DSS(tree), out, 0 ); 
/*
      if (*projection != 3) {                              
        ret=MP_to_LDR( Get_L_MP(tree), out, 0 );
      }
*/
      ret=MP_to_LDR( Get_L_MP(tree), out, 0 );
      ret=PP_to_LDR( Get_L_PP(tree), out, 0 );
      ret=AT_to_LDR( Get_L_AT(tree), out, 0 );
      ret=RD_to_LDR( Get_L_RD(tree), out, 0 );
      ret=RC_to_LDR( Get_L_RC(tree), out, 0 );
      ret=DQS_to_LDR( Get_L_DQS(tree), out, 0 );
      ret=DH_to_LDR( Get_L_DH(tree), out, 0 );
      ret=RS_to_LDR( Get_L_RS(tree), out, 0 );
      if (*ter_correct) {                              /* JMS 2-25-96  */
        ret=DE_to_LDR( Get_L_DE(tree), out, 0 );
      }
      ret=DP_to_LDR( Get_L_DP(tree), out, 0 );
      ret=FR_to_LDR( Get_L_FR(tree), out, 0 );
      if (out != (FILE *) NULL ) fclose( out );
   }
   return(TRUE);
}




/*  Function:   copy_data
    
    Purpose:    Copy non-template values from common blocks to SAR
                leader file tree structure
    Arguments:  Pointer to leader file tree structure 
    Returns:    Nothing (void)
    Processing:  Initialize pointers to fortran common blocks for
                 each data record, then call a function to copy
                 common block values into SAR leader tree structure
                 values (one function per record)
*/

#include <stdio.h>
#include <strings.h>
#include <stdlib.h>
#include <ctype.h>

#define PATH_SIZE 60

void f2c(char str[], int fsize, int csize);

void copy_data(SARL_ptr* tree)
{
int stat;

/* Declarations required to link up to fortran common blocks
   which contain leader file record information */
   Inp_Sarl_Desc_Rec* inp_fdr_ptr;   
   extern Inp_Sarl_Desc_Rec  inp_fdr;

   Inp_Dataset_Sum* inp_dss_ptr;       
   extern Inp_Dataset_Sum  inp_dss;

   Inp_Qual_Sum* inp_dqs_ptr;
   extern Inp_Qual_Sum inp_dqs;

   Inp_Pos_Data* inp_pos_ptr;
   extern Inp_Pos_Data inp_pos;

   Inp_Att_Data* inp_att_ptr;
   extern Inp_Att_Data inp_att;

   Inp_Radi_Data* inp_rad_ptr;
   extern Inp_Radi_Data inp_rad;

   Inp_Map_Proj* inp_map_ptr;
   extern Inp_Map_Proj inp_map;

   Inp_Rng_Spec* inp_rsp_ptr;
   extern Inp_Rng_Spec inp_rsp;

   Inp_Raw_Data_Hist* inp_raw_ptr;
   extern Inp_Raw_Data_Hist inp_raw;

   Inp_Proc_Data_Hist* inp_proc_ptr;
   extern Inp_Proc_Data_Hist inp_proc;

   Inp_Digital_Elev* inp_dem_ptr;
   extern Inp_Digital_Elev inp_dem;

   Inp_Fac_Related* inp_fac_ptr;
   extern Inp_Fac_Related inp_fac;

/* Initialize pointers to common blocks */
   inp_fdr_ptr = &inp_fdr;         
   inp_dss_ptr = &inp_dss;        
   inp_dqs_ptr = &inp_dqs;       
   inp_pos_ptr = &inp_pos;      
   inp_att_ptr = &inp_att;     
   inp_rad_ptr = &inp_rad;    
   inp_map_ptr = &inp_map;   
   inp_rsp_ptr = &inp_rsp;  
   inp_raw_ptr = &inp_raw; 
   inp_proc_ptr = &inp_proc;         
   inp_dem_ptr = &inp_dem;         
   inp_fac_ptr = &inp_fac;        


/* Fill in leader file tree structures with data from common blocks 
   One function call per record */
   stat = fill_fdr(tree, inp_fdr_ptr);
   if (stat == FALSE)
     printf(" Template problem for file descriptor record\n");
   stat = fill_dss(tree, inp_dss_ptr);
   if (stat == FALSE)
     printf(" Template problem for data set summary record\n");
   stat = fill_dqs(tree, inp_dqs_ptr);
   if (stat == FALSE)
     printf(" Template problem for data quality summary record\n");
   stat = fill_pos(tree, inp_pos_ptr);
   if (stat == FALSE)
     printf(" Template problem for platform position data record\n");
   stat = fill_att(tree, inp_att_ptr);
   if (stat == FALSE)
     printf(" Template problem for attitude data record\n");
   stat = fill_rad(tree, inp_rad_ptr);
   if (stat == FALSE)
     printf(" Template problem for radiometric data record\n");
   stat = fill_map(tree, inp_map_ptr);    
   if (stat == FALSE)
     printf(" Template problem for map projection record\n");
   stat = fill_rsp(tree, inp_rsp_ptr);
   if (stat == FALSE)
     printf(" Template problem for range spectra record\n");
   stat = fill_raw(tree, inp_raw_ptr);
   if (stat == FALSE)
     printf(" Template problem for signal data histogram record\n");
   stat = fill_proc(tree, inp_proc_ptr);
   if (stat == FALSE)
     printf(" Template problem for processed data histogram record\n");
   stat = fill_dem(tree, inp_dem_ptr);
   if (stat == FALSE)
     printf(" Template problem for digital elevation model record\n");
   stat = fill_fac(tree, inp_fac_ptr);
   if (stat == FALSE)
     printf(" Template problem for facility data record\n");  

}



/*  Function:   fill_fdr
    
    Purpose:    Copy non-template values from common block to file descriptor
                record tree structure
    Arguments:  Pointers to leader file tree structure and common block
    Returns:    TRUE if record tree structure was found, FALSE otherwise
*/

int fill_fdr(SARL_ptr* t, Inp_Sarl_Desc_Rec* in)
{
Sarl_Desc_Rec* csizes;

    t->descript.desc.rec_seq = in->rec_seq;         /* record sequence number */
    f2c(in->file_name,sizeof(in->file_name),sizeof(csizes->product_id));
    strcpy(t->descript.product_id,in->file_name);         /* SAR leader file name  */
    t->descript.n_map_proj = in->n_map_proj;                        /* JMS 2-25-96 */
    t->descript.n_dem_desc = in->n_dem_desc;                        /* JMS 2-25-96 */

    return(TRUE);
}



/*  Function:   fill_dss
    
    Purpose:    Copy non-template values from common block to data set
                summary record tree structure
    Arguments:  Pointers to leader file tree structure and common block
    Returns:    TRUE if record tree structure was found, FALSE otherwise
*/

int fill_dss(SARL_ptr* t, Inp_Dataset_Sum* in)
{
Dataset_Sum* csizes;

    if (t->data_sum == (Dataset_Sum*) NULL)
      return(FALSE);
    t->data_sum->desc.rec_seq = in->rec_seq;         /* Data Set Summary: record sequence number */
    f2c(in->scene_id,sizeof(in->scene_id),sizeof(csizes->product_id));
    strcpy(t->data_sum->product_id,in->scene_id);         /* site ID (3-char ID) */
    f2c(in->inp_sctim,sizeof(in->inp_sctim),sizeof(csizes->inp_sctim));
    strcpy(t->data_sum->inp_sctim,in->inp_sctim);       /* image center GMT: YYYYMMDDhhmmssttt */
    f2c(in->asc_des,sizeof(in->asc_des),sizeof(csizes->asc_des));
    strcpy(t->data_sum->asc_des,in->asc_des);          /* Ascending/descending */
    t->data_sum->pro_lat = in->pro_lat;              /* latitude at scene center */
    t->data_sum->pro_long = in->pro_long;              /* latitude at scene center */
    t->data_sum->pro_head = in->pro_head;              /* latitude at scene center */
    t->data_sum->terrain_h = in->terrain_h;            /* average terrain height */
    t->data_sum->sc_lin = in->sc_lin;               /* image center line number (azimuth) */
    t->data_sum->sc_pix = in->sc_pix;               /* image center line number (azimuth) */
    t->data_sum->scene_len = in->scene_len;            /* image length in km */
    t->data_sum->scene_wid = in->scene_wid;            /* image length in km */
    f2c(in->orbit_num,sizeof(in->orbit_num),sizeof(csizes->revolution));
    strcpy(t->data_sum->revolution,in->orbit_num);         /* orbit number */
    t->data_sum->plat_lat = in->plat_lat;             /* spacecraft latitude at nadir */
    t->data_sum->plat_long = in->plat_long;             /* spacecraft latitude at nadir */
    t->data_sum->plat_head_scene = in->plat_head;             /* spacecraft latitude at nadir */
    t->data_sum->incident_ang = in->incident_ang;         /* incidence angle at image center */
    t->data_sum->clock_ang = in->clock_ang;            /* clock angle (deg) */
    t->data_sum->frequency = in->frequency;            /* radar frequency (GHz) */
    t->data_sum->wave_length = in->wave_length;          /* radar wavelength (m) */
    t->data_sum->mech_sight = in->mech_sight;          /* radar wavelength (m) */
/*    f2c(in->sensor_id,sizeof(in->sensor_id),sizeof(csizes->sensor_id));
    strcpy(t->data_sum->sensor_id,in->sensor_id);         product type */
    t->data_sum->ampl_coef[0] = in->ampl_coef[0];         /* range chirp coefficients */
    t->data_sum->ampl_coef[1] = in->ampl_coef[1];         /* range chirp coefficients */
    t->data_sum->ampl_coef[2] = in->ampl_coef[2];         /* range chirp coefficients */
    t->data_sum->ampl_coef[3] = in->ampl_coef[3];         /* range chirp coefficients */
    t->data_sum->ampl_coef[4] = in->ampl_coef[4];         /* range chirp coefficients */
    t->data_sum->rng_samp_rate = in->fr;                   /* range complex sampling rate */
    t->data_sum->rng_gate = in->rng_gate;             /* range gate at early edge */
    t->data_sum->rng_length = in->rng_length;           /* range pulse length */
    t->data_sum->i_bias = in->i_bias;               /* I channel DC bias */
    t->data_sum->q_bias = in->q_bias;               /* I channel DC bias */
    t->data_sum->iq_ratio = in->iq_ratio;             /* I/Q channel ratio */
    t->data_sum->ele_sight = in->ele_sight;            /* electronic boresight */
/*    f2c(in->prod_type,sizeof(in->prod_type),sizeof(csizes->prod_type));
    strcpy(t->data_sum->prod_type,in->prod_type);         product type */
    t->data_sum->n_azilok = in->n_azilok;             /* number of looks in azimuth */
    t->data_sum->n_rnglok = in->n_rnglok;             /* number of looks in azimuth */
    t->data_sum->bnd_azilok = in->bnd_azilok;           /* bandwidth per look in azimuth */
    t->data_sum->bnd_rnglok = in->bnd_rnglok;           /* bandwidth per look in azimuth */
    t->data_sum->bnd_azi = in->bnd_azi;              /* processor bandwidth (azimuth) */
    t->data_sum->bnd_rng = in->bnd_rng;              /* processor bandwidth (azimuth) */
    t->data_sum->rng_res = in->rng_res;              /* nominal resolution (range) */
    t->data_sum->azi_res = in->azi_res;              /* nominal resolution (range) */
    t->data_sum->alt_dopcen[0] = in->alt_dopcen[0];        /* along track Doppler freq terms */
    t->data_sum->alt_dopcen[1] = in->alt_dopcen[1];        /* along track Doppler freq terms */
    t->data_sum->alt_dopcen[2] = in->alt_dopcen[2];        /* along track Doppler freq terms */
    t->data_sum->crt_dopcen[0] = in->crt_dopcen[0];        /* cross track Doppler freq terms */
    t->data_sum->crt_dopcen[1] = in->crt_dopcen[1];        /* cross track Doppler freq terms */
    t->data_sum->crt_dopcen[2] = in->crt_dopcen[2];        /* cross track Doppler freq terms */
    t->data_sum->alt_rate[0] = in->alt_rate[0];          /* Along track Doppler rate terms */
    t->data_sum->alt_rate[1] = in->alt_rate[1];          /* Along track Doppler rate terms */
    t->data_sum->alt_rate[2] = in->alt_rate[2];          /* Along track Doppler rate terms */
    t->data_sum->crt_rate[0] = in->crt_rate[0];          /* Cross track Doppler rate terms */
    t->data_sum->crt_rate[1] = in->crt_rate[1];          /* Cross track Doppler rate terms */
    t->data_sum->crt_rate[2] = in->crt_rate[2];          /* Cross track Doppler rate terms */
    f2c(in->clutter_lock,sizeof(in->clutter_lock),sizeof(csizes->clutterlock_flg));
    strcpy(t->data_sum->clutterlock_flg,in->clutter_lock);      /* clutter lock flag */
    f2c(in->auto_focus,sizeof(in->auto_focus),sizeof(csizes->auto_focus));
    strcpy(t->data_sum->auto_focus,in->auto_focus);        /* autofocussing flag */
    t->data_sum->line_spacing = in->line_spacing;         /* line spacing (m) */
    t->data_sum->pixel_spacing = in->pix_spacing;         /* line spacing (m) */
    t->data_sum->no_beams=in->no_beams;
    t->data_sum->prf1=in->prf1;  
    t->data_sum->prf2=in->prf2;
    t->data_sum->prf3=in->prf3;
    t->data_sum->prf4=in->prf4;
    t->data_sum->rng_gate1=in->rng_gate1;
    t->data_sum->rng_gate2=in->rng_gate2;
    t->data_sum->rng_gate3=in->rng_gate3;
    t->data_sum->rng_gate4=in->rng_gate4;
    t->data_sum->tot_pls_burst=in->tot_pls_burst; 
    t->data_sum->val_pls_burst=in->val_pls_burst;
    t->data_sum->az_ovlp_nxt_img = in->az_ovlp_nxt_img;    
    t->data_sum->rg_off_nxt_img = in->rg_off_nxt_img;
/*  JMS added 4-3-96 for beam id's  */
    f2c(in->beam1,sizeof(in->beam1),sizeof(csizes->beam1));
    strcpy(t->data_sum->beam1,in->beam1);        
    f2c(in->beam2,sizeof(in->beam2),sizeof(csizes->beam2));
    strcpy(t->data_sum->beam2,in->beam2);        
    f2c(in->beam3,sizeof(in->beam3),sizeof(csizes->beam3));
    strcpy(t->data_sum->beam3,in->beam3);        
    f2c(in->beam4,sizeof(in->beam4),sizeof(csizes->beam4));
    strcpy(t->data_sum->beam4,in->beam4);        

/* JMS added 5-10-96 for data input source */
    f2c(in->data_inpsrc,sizeof(in->data_inpsrc),sizeof(csizes->data_inpsrc));
    strcpy(t->data_sum->data_inpsrc,in->data_inpsrc);    /* data input source */

/* JMS added 8-12-96 for processor version*/
    f2c(in->ver_id,sizeof(in->ver_id),sizeof(csizes->ver_id));
    strcpy(t->data_sum->ver_id,in->ver_id);    /* processor version */

/*  JMS added 9-10-96 new items  */
    f2c(in->scanner_version,sizeof(in->scanner_version),sizeof(csizes->scanner_version));
    strcpy(t->data_sum->scanner_version,in->scanner_version);    /* processor version */
    f2c(in->decode_version,sizeof(in->decode_version),sizeof(csizes->decode_version));
    strcpy(t->data_sum->decode_version,in->decode_version);    /* processor version */
    f2c(in->cal_params_file,sizeof(in->cal_params_file),sizeof(csizes->cal_params_file));
    strcpy(t->data_sum->cal_params_file,in->cal_params_file);    /* calib file name*/
    f2c(in->scan_results_file,sizeof(in->scan_results_file),sizeof(csizes->scan_results_file));
    strcpy(t->data_sum->scan_results_file,in->scan_results_file);    /* scan results file name*/

/*  JMS added 9-13-96 for processor id */
    f2c(in->sys_id,sizeof(in->sys_id),sizeof(csizes->sys_id));
    strcpy(t->data_sum->sys_id,in->sys_id);    /* SSP id */

    return(TRUE);
}



/*  Function:   fill_dqs
    
    Purpose:    Copy non-template values from common block to data 
                quality summary record tree structure
    Arguments:  Pointers to leader file tree structure and common block
    Returns:    TRUE if record tree structure was found, FALSE otherwise
*/

int fill_dqs(SARL_ptr* t, Inp_Qual_Sum* in)
{
Qual_Sum* csizes;

    if (t->data_qual == (Qual_Sum*) NULL)
      return (FALSE);
    t->data_qual->desc.rec_seq = in->rec_seq;      /* Data Qual Summary: record sequence number */
    t->data_qual->snr = in->snr;              /* snr estimate */
    t->data_qual->ber = in->ber;              /* bit error rate */
    t->data_qual->azi_ambig = in->azi_ambig;
    t->data_qual->rng_ambig = in->rng_ambig;
    t->data_qual->dyn_rng = in->dyn_rng;
    t->data_qual->azi_res = in->azi_res;
    t->data_qual->rng_res = in->rng_res;
    t->data_qual->alt_locerr = in->alt_locerr;
    t->data_qual->crt_locerr = in->crt_locerr;
    t->data_qual->alt_scale = in->alt_scale;
    t->data_qual->crt_scale = in->crt_scale;
    t->data_qual->dis_skew = in->dis_skew;
    t->data_qual->ori_err = in->ori_err;
    t->data_qual->islr = in->islr;
    t->data_qual->pslr = in->pslr;
    t->data_qual->nesz = in->nesz;
/* JMS 5-9-96 */
    t->data_qual->rel_rad_unc[0][0] = in->db[0];
    t->data_qual->enl=in->enl;
/* JMS added 10-03-96 for calibration status */
    f2c(in->cal_status,sizeof(in->cal_status),sizeof(csizes->cal_status));
    strcpy(t->data_qual->cal_status,in->cal_status);    /* processor version */
    f2c(in->cal_comment,sizeof(in->cal_comment),sizeof(csizes->cal_comment));
    strcpy(t->data_qual->cal_comment,in->cal_comment);    /* processor version */
    printf("In fill_dqs, cal_status = %s\n",in->cal_status);
    printf("In fill_dqs, cal_status = %s\n",t->data_qual->cal_status);

    return(TRUE);
}



/*  Function:   fill_pos
    
    Purpose:    Copy non-template values from common block to platform
                position data record tree structure
    Arguments:  Pointers to leader file tree structure and common block
    Returns:    TRUE if record tree structure was found, FALSE otherwise
*/

int fill_pos(SARL_ptr* t, Inp_Pos_Data* in)
{
int i;

    if (t->platform == (Pos_Data*) NULL)
      return(FALSE);
    t->platform->desc.rec_seq = in->rec_seq;          
    t->platform->year = in->year;          
    t->platform->month = in->month;          
    t->platform->day = in->day;          
    t->platform->gmt_day = in->gmt_day;          
    t->platform->gmt_sec = in->gmt_sec;          
    t->platform->data_int = in->data_int;          
    t->platform->hr_angle = in->hr_angle;          
    for (i = 0; i < 6; i++)
      t->platform->orbit_ele[i] = in->orbit_ele[i];
    t->platform->pos_vect[0].pos[0] = in->x_pos[0];          
    t->platform->pos_vect[0].pos[1] = in->y_pos[0];          
    t->platform->pos_vect[0].pos[2] = in->z_pos[0];          
    t->platform->pos_vect[1].pos[0] = in->x_pos[1];          
    t->platform->pos_vect[1].pos[1] = in->y_pos[1];          
    t->platform->pos_vect[1].pos[2] = in->z_pos[1];          
    t->platform->pos_vect[2].pos[0] = in->x_pos[2];          
    t->platform->pos_vect[2].pos[1] = in->y_pos[2];          
    t->platform->pos_vect[2].pos[2] = in->z_pos[2];          
    t->platform->pos_vect[0].vel[0] = in->x_vel[0];          
    t->platform->pos_vect[0].vel[1] = in->y_vel[0];          
    t->platform->pos_vect[0].vel[2] = in->z_vel[0];          
    t->platform->pos_vect[1].vel[0] = in->x_vel[1];          
    t->platform->pos_vect[1].vel[1] = in->y_vel[1];          
    t->platform->pos_vect[1].vel[2] = in->z_vel[1];          
    t->platform->pos_vect[2].vel[0] = in->x_vel[2];          
    t->platform->pos_vect[2].vel[1] = in->y_vel[2];          
    t->platform->pos_vect[2].vel[2] = in->z_vel[2];          
    
    return(TRUE);
}



/*  Function:   fill_att
    
    Purpose:    Copy non-template values from common block to 
                attitude data record tree structure
    Arguments:  Pointers to leader file tree structure and common block
    Returns:    TRUE if record tree structure was found, FALSE otherwise
*/

int fill_att(SARL_ptr* t, Inp_Att_Data* in)
{
    Att_Vect_Rec* av;
    int i, status;

    if (t->attitude == (Att_Data*) NULL)
      return (FALSE);
    t->attitude->desc.rec_seq = in->rec_seq;
    if (t->attitude->npoint < ATT_NPOINTS) 
      status = FALSE;
    else
      status = TRUE;
    av = t->attitude->att_vect;
    for (i = 0; i < t->attitude->npoint; i++)
      {
      av->gmt_day = in->gmt_day[i];             /*  day in the year (GMT) */
      av->gmt_msec = in->gmt_sec[i];             /*  sec in the year (GMT) */
      av->pitch = in->pitch[i];               /*  pitch (degrees) */
      av->roll = in->roll[i];                 /* roll (degrees) */
      av->yaw = in->yaw[i];                  /* yaw (degrees) */
      av->pitch_rate = in->pitch_rate[i];        /* pitch rate (degrees/sec) */
      av->roll_rate = in->roll_rate[i];          /* roll rate (degrees/sec) */
      av->yaw_rate = in->yaw_rate[i];             /* yaw_rate (degrees/sec) */
/* JMS added 5-10-96  */
      av->pitch_flag=in->pitch_flag[i];
      av->roll_flag=in->roll_flag[i];
      av->yaw_flag=in->yaw_flag[i];
      av->pitch_rate_flag=in->pitch_rate_flag[i];
      av->roll_rate_flag=in->roll_rate_flag[i];
      av->yaw_rate_flag=in->yaw_rate_flag[i];
      av = av->next;
      }
    return(status);
}



/*  Function:   fill_rad
    
    Purpose:    Copy non-template values from common block to 
                radiometric data record tree structure
    Arguments:  Pointers to leader file tree structure and common block
    Returns:    TRUE if record tree structure was found, FALSE otherwise
*/

int fill_rad(SARL_ptr* t, Inp_Radi_Data* in)
{
  int i;

    if (t->radio_data == (Radi_Data*) NULL)
      return(FALSE);
    t->radio_data->desc.rec_seq = in->rec_seq;              /* radiometric data rec seq number */
    t->radio_data->n_samp = in->n_samp;               /* no. of samples in look up table */
    t->radio_data->noise_fact = in->noise_fact;           /* noise scale factor (a1) */
    t->radio_data->linear_conv_fact = in->linear_conv_fact;     /* linear conversion factor (a2) */
    t->radio_data->offset_conv_fact = in->offset_conv_fact;     /* offset conversion factor (a3) */

    strncpy(t->radio_data->table_desig,"NOISE VS LOOK ANGLE     ",24);

    for (i = 0; i < t->radio_data->n_samp; i++)
      t->radio_data->lookup_tab[i] = in->lookup_tab[i];
    return(TRUE);
}



/*  Function:   fill_map
    
    Purpose:    Copy non-template values from common block to map
                projection record tree structure
    Arguments:  Pointers to leader file tree structure and common block
    Returns:    TRUE if record tree structure was found, FALSE otherwise
*/

int fill_map(SARL_ptr* t, Inp_Map_Proj* in)
{
Map_Proj* csizes;

    if (t->map_proj == (Map_Proj*) NULL)
      return(FALSE);
    t->map_proj->desc.rec_seq = in->rec_seq;          /* Map Projection: record sequence number */
    f2c(in->map_desc,sizeof(in->map_desc),sizeof(csizes->map_desc));
    strcpy(t->map_proj->map_desc,in->map_desc);         /* map projection descriptor */
    printf("n_pixel %d n_line %d pix_spacing %d\n", in->n_pixel, in->n_line, in->pix_spacing);
    t->map_proj->n_pixel = in->n_pixel;              /* number of pixels per line */
    t->map_proj->n_line = in->n_line;              /* number of pixels per line */
    t->map_proj->pixel_spacing = in->pix_spacing;          /* pixel spacing (m) */
    t->map_proj->line_spacing = in->line_spacing;          /* pixel spacing (m) */
    t->map_proj->osc_orient = in->osc_orient;           /* Output Scne cntr orientation */
    t->map_proj->orb_incl = in->orb_incl;             /* Orbital Inclination */
    t->map_proj->asc_node = in->asc_node;             /* Ascending Node Longitude */
    t->map_proj->isc_dist = in->isc_dist;             /* platform distance */
    t->map_proj->geo_alt = in->geo_alt;              /* geodetic altitude */
    t->map_proj->isc_vel = in->isc_vel;              /* ground speed at nadir */
    t->map_proj->plat_head = in->plat_head;            /* platform heading */
    t->map_proj->datum_shift[0] = in->datum_shift[0];       /* Datum shift parameters */
    t->map_proj->datum_shift[1] = in->datum_shift[1];       /* Datum shift parameters */
    t->map_proj->datum_shift[2] = in->datum_shift[2];       /* Datum shift parameters */
    f2c(in->proj_desc,sizeof(in->proj_desc),sizeof(csizes->projection));
    strcpy(t->map_proj->projection,in->proj_desc);        /* Map projection descriptor */
    printf("map_proj_projection %s\n", in->proj_desc);
    f2c(in->utm_zone_sig,sizeof(in->utm_zone_sig),sizeof(csizes->utm_zone_sig));
    strcpy(t->map_proj->utm_zone_sig,in->utm_zone_sig);      /* UTM zone signature */
    t->map_proj->utm_north_orig = in->utm_north_orig;       /* Map origin, false northing */
    t->map_proj->utm_stand_par[0] = in->utm_stand_par[0];       /* Standard parallel */
    t->map_proj->utm_stand_par[1] = in->utm_stand_par[1];       /* Standard parallel */
    t->map_proj->utm_scale = in->utm_scale;       /* Scale factor */
    t->map_proj->ups_cent_lat = in->ups_cent_lat;       /* Center latitude*/
    t->map_proj->ups_cent_long = in->ups_cent_long;       /* Center longitude*/
    strcpy(t->map_proj->ups_desc,in->proj_desc);        /* UPS descriptor */
    t->map_proj->ups_scale = in->ups_scale;       /* ups scale factor*/
    f2c(in->nsp_desc,sizeof(in->nsp_desc),sizeof(csizes->nsp_desc));
    strcpy(t->map_proj->nsp_desc,in->nsp_desc);         /* NSP descriptor */
    t->map_proj->nsp_east_orig = in->nsp_east_orig;        /* Map origin, false easting */
    t->map_proj->nsp_north_orig = in->nsp_north_orig;        /* Map origin, false easting */
    t->map_proj->nsp_cent_long = in->nsp_cent_long;        /* Projection center longitude */
    t->map_proj->nsp_cent_lat = in->nsp_cent_lat;        /* Projection center longitude */
    t->map_proj->nsp_stand_par[0] = in->nsp_stand_par[0];     /* Standard parllels */
    t->map_proj->nsp_stand_par[1] = in->nsp_stand_par[1];     /* Standard parllels */
    t->map_proj->nsp_stand_par[2] = in->nsp_stand_par[2];     /* Standard parllels */
    t->map_proj->nsp_stand_par[3] = in->nsp_stand_par[3];     /* Standard parllels */
    t->map_proj->nsp_stand_mer[0] = in->nsp_stand_mer[0];     /* Standard meridians */
    t->map_proj->nsp_stand_mer[1] = in->nsp_stand_mer[1];     /* Standard meridians */
    t->map_proj->nsp_stand_mer[2] = in->nsp_stand_mer[2];     /* Standard meridians */
    t->map_proj->corner_ne[0] = in->corner_ne[0];         /* corner northing/easting */
    t->map_proj->corner_ne[1] = in->corner_ne[1];         /* corner northing/easting */
    t->map_proj->corner_ne[2] = in->corner_ne[2];         /* corner northing/easting */
    t->map_proj->corner_ne[3] = in->corner_ne[3];         /* corner northing/easting */
    t->map_proj->corner_ne[4] = in->corner_ne[4];         /* corner northing/easting */
    t->map_proj->corner_ne[5] = in->corner_ne[5];         /* corner northing/easting */
    t->map_proj->corner_ne[6] = in->corner_ne[6];         /* corner northing/easting */
    t->map_proj->corner_ne[7] = in->corner_ne[7];         /* corner northing/easting */
    t->map_proj->corner_ll[0] = in->corner_ll[0];         /* corner latitude/longitude */
    t->map_proj->corner_ll[1] = in->corner_ll[1];         /* corner latitude/longitude */
    t->map_proj->corner_ll[2] = in->corner_ll[2];         /* corner latitude/longitude */
    t->map_proj->corner_ll[3] = in->corner_ll[3];         /* corner latitude/longitude */
    t->map_proj->corner_ll[4] = in->corner_ll[4];         /* corner latitude/longitude */
    t->map_proj->corner_ll[5] = in->corner_ll[5];         /* corner latitude/longitude */
    t->map_proj->corner_ll[6] = in->corner_ll[6];         /* corner latitude/longitude */
    t->map_proj->corner_ll[7] = in->corner_ll[7];         /* corner latitude/longitude */
    t->map_proj->terr_height[0] = in->terr_height[0];       /* corner terrain height */
    t->map_proj->terr_height[1] = in->terr_height[1];       /* corner terrain height */
    t->map_proj->terr_height[2] = in->terr_height[2];       /* corner terrain height */
    t->map_proj->terr_height[3] = in->terr_height[3];       /* corner terrain height */
    return(TRUE);
}



/*  Function:   fill_rsp
    
    Purpose:    Copy non-template values from common block to range
                spectra record tree structure
    Arguments:  Pointers to leader file tree structure and common block
    Returns:    TRUE if record tree structure was found, FALSE otherwise
*/

int fill_rsp(SARL_ptr* t, Inp_Rng_Spec* in)
{
int i;

    if (t->spectra == (Rng_Spec*) NULL)
      return(FALSE);
    t->spectra->desc.rec_seq = in->rec_seq;              /* range spectra sequence no. */
    t->spectra->n_pixels = in->n_pixels;             /* number of pixels in line */
    t->spectra->pixel_offset = in->pixel_offset;         /* offset from first pixel */
    t->spectra->n_lines = in->n_lines;              /* number of lines integrated for spectra */
    t->spectra->first_freq = in->first_freq;           /* center freq of first spectra bin */
    t->spectra->last_freq = in->last_freq;            /* center freq of last spectra bin */
    t->spectra->min_power = in->min_power;            /* minimum spectral power */
    t->spectra->max_power = in->max_power;            /* minimum spectral power */
    for (i=0; i < t->spectra->n_bins; i++)
      t->spectra->data_values_spec[i] = in->data_values[i];

    return(TRUE);
}



/*  Function:   fill_raw
    
    Purpose:    Copy non-template values from common block to signal
                data histogram record tree structure
    Arguments:  Pointers to leader file tree structure and common block
    Returns:    TRUE if record tree structure was found, FALSE otherwise
*/

int fill_raw(SARL_ptr* t, Inp_Raw_Data_Hist* in)
{
Hist_Data_Set* av;
int i;

    if (t->histogram == (Data_Hist*) NULL)
      return(FALSE);
    t->histogram->desc.rec_seq = in->rec_seq;               /* data histogram record seq no. */
    av = t->histogram->data_set;
    av->nbin = in->i_nbin;                /* table size, bytes */
    av->ns_lin = in->i_ns_lin;              /* number of pixels in line */
    av->ns_pix = in->i_ns_pix;              /* number of lines in image */
    av->ngrp_lin = in->i_ngrp_lin;            /* pixels/group, cross track */
    av->ngrp_pix = in->i_ngrp_pix;            /* pixels/group, along track */
    av->nsamp_lin = in->i_nsamp_lin;           /* number of groups, cross track */
    av->nsamp_pix = in->i_nsamp_pix;           /* number of groups, along track */
    av->min_smp = in->i_min_smp;             /* minimum pixel value */
    av->max_smp = in->i_max_smp;             /* maximum pixel value */
    av->mean_smp = in->i_mean_smp;            /* mean sample value */
    av->std_smp = in->i_std_smp;             /* std deviation of sample value */
    av->min_hist = in->i_min_hist;            /* minimum table value */
    av->max_hist = in->i_max_hist;            /* maximum table value */
    av->mean_hist = in->i_mean_hist;           /* histogram mean value */
    av->std_hist = in->i_std_hist;            /* histogram standard deviation */
    for (i=0; i < av->nhist; i++)
      av->data_values_hist[i] = in->i_data_values[i];
    av = av->next;
    av->nbin = in->q_nbin;                /* table size, bytes */
    av->ns_lin = in->q_ns_lin;              /* number of pixels in line */
    av->ns_pix = in->q_ns_pix;              /* number of lines in image */
    av->ngrp_lin = in->q_ngrp_lin;            /* pixels/group, cross track */
    av->ngrp_pix = in->q_ngrp_pix;            /* pixels/group, along track */
    av->nsamp_lin = in->q_nsamp_lin;           /* number of groups, cross track */
    av->nsamp_pix = in->q_nsamp_pix;           /* number of groups, along track */
    av->min_smp = in->q_min_smp;             /* minimum pixel value */
    av->max_smp = in->q_max_smp;             /* maximum pixel value */
    av->mean_smp = in->q_mean_smp;            /* mean sample value */
    av->std_smp = in->q_std_smp;             /* std deviation of sample value */
    av->min_hist = in->q_min_hist;            /* minimum table value */
    av->max_hist = in->q_max_hist;            /* maximum table value */
    av->mean_hist = in->q_mean_hist;           /* histogram mean value */
    av->std_hist = in->q_std_hist;            /* histogram standard deviation */
    for (i=0; i < av->nhist; i++)
      av->data_values_hist[i] = in->q_data_values[i];

    return(TRUE);
}



/*  Function:   fill_proc
    
    Purpose:    Copy non-template values from common block to processed
                data histogram record tree structure
    Arguments:  Pointers to leader file tree structure and common block
    Returns:    TRUE if record tree structure was found, FALSE otherwise
*/

int fill_proc(SARL_ptr* t, Inp_Proc_Data_Hist* in)
{
Hist_Data_Set* av;
Data_Hist* ad;
int i;

    if (t->histogram == (Data_Hist*) NULL)
      return (FALSE);
    ad = t->histogram;
    ad = ad->next;   
    if (ad == (Data_Hist*) NULL)
      return (FALSE);
    ad->desc.rec_seq = in->rec_seq;               /* data histogram record seq no. */
    av = ad->data_set;
    av->ns_lin = in->ns_lin;              /* number of pixels in line */
    av->ns_pix = in->ns_pix;              /* number of lines in image */
    av->ngrp_lin = in->ngrp_lin;            /* pixels/group, cross track */
    av->ngrp_pix = in->ngrp_pix;            /* pixels/group, along track */
    av->nsamp_lin = in->nsamp_lin;           /* number of groups, cross track */
    av->nsamp_pix = in->nsamp_pix;           /* number of groups, along track */
    av->mean_smp = in->mean_smp;            /* mean sample value */
    av->std_smp = in->std_smp;             /* std deviation of sample value */
    av->min_hist = in->min_hist;            /* minimum table value */
    av->max_hist = in->max_hist;            /* maximum table value */
    av->mean_hist = in->mean_hist;           /* histogram mean value */
    av->std_hist = in->std_hist;            /* histogram standard deviation */
    for (i=0; i < av->nhist; i++)
      av->data_values_hist[i] = in->data_values[i];

    return(TRUE);
}



/*  Function:   fill_dem
    
    Purpose:    Copy non-template values from common block to digital
                elevation model record tree structure
    Arguments:  Pointers to leader file tree structure and common block
    Returns:    TRUE if record tree structure was found, FALSE otherwise
*/

int fill_dem(SARL_ptr* t, Inp_Digital_Elev* in)
{
int  s1, s2;
Digital_Elev* csizes;
Corner_Pts* av;

    if (t->elevation == (Digital_Elev*) NULL)
      return(FALSE);
    if (t->elevation->set->num_crnr_pts < DEM_CRNR_PTS)
      return(FALSE);
    t->elevation->desc.rec_seq = in->rec_seq;          
    s1 = sizeof(in->raster_unit);
    s2 = sizeof(csizes->raster_unit);
    f2c(in->raster_unit,s1,s2);
    strcpy(t->elevation->raster_unit,in->raster_unit); 
    s1 = sizeof(in->presentation_proj);
    s2 = sizeof(csizes->presentation_proj);
    f2c(in->presentation_proj,s1,s2);
    strcpy(t->elevation->presentation_proj,in->presentation_proj); 
    t->elevation->NS_raster = in->NS_raster;          
    t->elevation->EW_raster = in->EW_raster;          
    s1 = sizeof(in->resample);
    s2 = sizeof(csizes->resample);
    f2c(in->resample,s1,s2);
    strcpy(t->elevation->resample,in->resample); 
    t->elevation->height_err = in->height_err;          
    t->elevation->NS_loc_err = in->NS_loc_err;          
    t->elevation->EW_loc_err = in->EW_loc_err;          
    t->elevation->max_height = in->max_height;          
    t->elevation->min_height = in->min_height;          
    t->elevation->MEAN_height = in->MEAN_height;          
    t->elevation->STD_height = in->STD_height;          
    av = t->elevation->set->pts;
    av->cp_lat_1 = in->latitude[0];
    av->cp_lon_1 = in->longitude[0];
    av = av->next;
    av->cp_lat_1 = in->latitude[1];
    av->cp_lon_1 = in->longitude[1];
    av = av->next;
    av->cp_lat_1 = in->latitude[2];
    av->cp_lon_1 = in->longitude[2];
    av = av->next;
    av->cp_lat_1 = in->latitude[3];
    av->cp_lon_1 = in->longitude[3];
    
    return(TRUE);
}



/*  Function:   fill_fac
    
    Purpose:    Copy non-template values from common block to 
                facility data record tree structure
    Arguments:  Pointers to leader file tree structure and common block
    Returns:    TRUE if record tree structure was found, FALSE otherwise
*/

int fill_fac(SARL_ptr* t, Inp_Fac_Related* in)
{
Fac_Related* csizes;

    if (t->facility == (Fac_Related*) NULL)
      return(FALSE);
    t->facility->desc.rec_seq = in->rec_seq;         /*  record sequence number */
    f2c(in->datatake_ID,sizeof(in->datatake_ID),sizeof(csizes->datatake_ID));
    strcpy(t->facility->datatake_ID,in->datatake_ID);      
    f2c(in->image_ID,sizeof(in->image_ID),sizeof(csizes->image_ID));
    strcpy(t->facility->image_ID,in->image_ID);        
    f2c(in->corr_year,sizeof(in->corr_year),sizeof(csizes->corr_year));
    strcpy(t->facility->corr_year,in->corr_year);       
    f2c(in->corr_GMT,sizeof(in->corr_GMT),sizeof(csizes->corr_GMT));
    strcpy(t->facility->corr_GMT,in->corr_GMT);      
    f2c(in->site_name,sizeof(in->site_name),sizeof(csizes->site_name));
    strcpy(t->facility->site_name,in->site_name);    
    f2c(in->data_year,sizeof(in->data_year),sizeof(csizes->data_year));
    strcpy(t->facility->data_year,in->data_year);    
    f2c(in->center_GMT,sizeof(in->center_GMT),sizeof(csizes->center_GMT));
    strcpy(t->facility->center_GMT,in->center_GMT); 
    t->facility->center_LAT = in->center_LAT;
    t->facility->center_LON = in->center_LON;   
    t->facility->near_start_LAT = in->near_sta_LAT;
    t->facility->near_start_LON = in->near_sta_LON;         
    t->facility->near_end_LAT = in->near_end_LAT;
    t->facility->near_end_LON = in->near_end_LON;
    t->facility->far_start_LAT = in->far_sta_LAT;
    t->facility->far_start_LON = in->far_sta_LON;
    t->facility->far_end_LAT = in->far_end_LAT;
    t->facility->far_end_LON = in->far_end_LON;
    t->facility->actual_azimuth = in->actual_azimuth;
    t->facility->actual_range = in->actual_range;
    t->facility->actual_pixels = in->actual_pixels;
    t->facility->actual_lines = in->actual_lines;
    t->facility->total_pixels = in->total_pixels;
    t->facility->total_lines = in->total_lines;
    t->facility->PRF = in->PRF;                
    t->facility->ant_look_angle = in->ant_look_angle;    
    t->facility->data_rate = in->data_rate;
    t->facility->data_win_pos = in->data_win_pos;
    t->facility->range_gate_del = in->range_gate_del; 
    t->facility->track_angle = in->track_angle;   
    f2c(in->ASC_DESC,sizeof(in->ASC_DESC),sizeof(csizes->ASC_DESC));
    strcpy(t->facility->ASC_DESC,in->ASC_DESC);  
    t->facility->S_C_altitude = in->S_C_altitude;
    t->facility->X_position = in->X_pos;
    t->facility->Y_position = in->Y_pos;
    t->facility->Z_position = in->Z_pos;
    t->facility->X_velocity = in->X_vel;
    t->facility->Y_velocity = in->Y_vel;
    t->facility->Z_velocity = in->Z_vel;
    t->facility->roll = in->roll; 
    t->facility->yaw = in->yaw; 
    t->facility->pitch = in->pitch;                
    t->facility->roll_rate = in->roll_rate;       
    t->facility->yaw_rate = in->yaw_rate;
    t->facility->pitch_rate = in->pitch_rate;
    t->facility->nadir_radius = in->nadir_radius;
    t->facility->image_radius = in->image_radius;
    t->facility->incidence_angle = in->incidence_angle;
    f2c(in->proc_version,sizeof(in->proc_version),sizeof(csizes->proc_version));
    strcpy(t->facility->proc_version,in->proc_version);  
    f2c(in->type_ephemeris,sizeof(in->type_ephemeris),sizeof(csizes->type_ephemeris));
    strcpy(t->facility->type_ephemeris,in->type_ephemeris);
    t->facility->looks_azimuth = in->looks_azimuth;
    t->facility->looks_range = in->looks_range;
    t->facility->induced_azimuth = in->induced_azimuth;
    t->facility->induced_range = in->induced_range;
    t->facility->gain = in->gain;
    t->facility->swath_velocity = in->swath_velocity;
    t->facility->squint_angle = in->squint_angle;
    t->facility->avg_terrain_ht = in->avg_ter_height;
    t->facility->processor_gain = in->proc_gain;
    t->facility->sl_rng_1st_pix = in->sl_rng_1st_pix;      
    t->facility->sl_rng_last_pix = in->sl_rng_last_pix;
    t->facility->start_sample = in->start_sample;
    f2c(in->clutterlock_flg,sizeof(in->clutterlock_flg),sizeof(csizes->clutterlock_flg));
    printf("ABCDEFG in fill_fac clutterlock flag is %s\n",in->clutterlock_flg);
    strcpy(t->facility->clutterlock_flg,in->clutterlock_flg);
    t->facility->dop_frq_const = in->dop_frq_const;
    t->facility->dop_frq_slope = in->dop_frq_slope;
    t->facility->dop_frq_quad = in->dop_frq_quad;
    f2c(in->autofocus_flag,sizeof(in->autofocus_flag),sizeof(csizes->autofocus_flag));
    printf("ABCDEFG in fill_fac autofocus flag is %s\n",in->autofocus_flag);
    strcpy(t->facility->autofocus_flag,in->autofocus_flag);    
    t->facility->dop_frq_r_cnst = in->dop_frq_r_cnst;
    t->facility->dop_frq_r_slope = in->dop_frq_r_slope;
    t->facility->dop_frq_r_quad = in->dop_frq_r_quad;
    t->facility->azi_res = in->resol_azimuth;
    t->facility->rng_res = in->resol_range;
    t->facility->azimuth_pixel = in->azimuth_pixel;
    t->facility->range_pixel = in->range_pixel;
    t->facility->bit_err_rate = in->bit_err_rate;
    t->facility->SNR = in->SNR;
    t->facility->est_noise_flr = in->est_noise_flr;        
    t->facility->satur_points = in->satur_points;
    t->facility->repl_agc = in->repl_agc;
    t->facility->temp_rx_lna = in->temp_rx_lna;
    t->facility->temp_rx_sub = in->temp_rx_sub;
    t->facility->temp_rx_prot = in->temp_rx_prot;
    t->facility->temp_cal_sys = in->temp_cal_sys;
    t->facility->rx_agc = in->rx_agc;
    t->facility->pre_cal1_pow = in->pre_cal1_pow;
    t->facility->pre_cal2_pow = in->pre_cal2_pow;
    t->facility->post_cal1_pow = in->post_cal1_pow;
    t->facility->post_cal2_pow = in->post_cal2_pow;
    t->facility->repl_pow = in->repl_pow;
    t->facility->ssar_roll_ang = in->ssar_roll_ang;

/*  JMS 5-13-96 */
    t->facility->roll_flag = in->roll_flag; 
    t->facility->yaw_flag = in->yaw_flag; 
    t->facility->pitch_flag = in->pitch_flag;                
    t->facility->roll_rate_flag = in->roll_rate_flag;       
    t->facility->yaw_rate_flag = in->yaw_rate_flag;
    t->facility->pitch_rate_flag = in->pitch_rate_flag;
    return(TRUE);
}



/*  Function:   f2c
    
    Purpose:    Convert character strings from fortran format to C format

    Arguments:  str[] -- character string to convert
                fsize -- number of bytes in string in common block.  This
                         is set to the next higher multiple of four bytes
                         to avoid alignment problems
                csize -- number of bytes of string in CEOS tree structure.
                         This is the actual number of bytes for the item
                         in the product specification, plus one byte for
                         the terminating null character

    Returns:    Nothing (void)

    Processing:  Beginning with the "last" byte according to the string size
                 defined in the CEOS tree structure, and ending with the 
                 last byte of the string in the common block, fill in the 
                 bytes with null characters
*/


void f2c(char str[], int fsize, int csize)
{
  int i;

    for (i = (csize - 1); i < fsize; i++)
      str[i] = '\0';
}



/*  Function:   f2c2
    
    Purpose:    Convert character strings from fortran format to C format

    Arguments:  str[61] -- character string to convert

    Returns:    Nothing (void)

    Processing:  Beginning with the last byte, replace blanks with nulls.

*/


void f2c2(int len, char *in_str, char *out_str)
{
int i;

  for (i = 0; i < len; i++)
    {
    out_str[i] = in_str[i];
    if (in_str[i] == ' ')
      out_str[i] = '\0';
    }
}

#include <stdio.h>
#include <strings.h>
#include <stdlib.h>
#include <ctype.h>

static int calc_length;
static int record_length;

FILE* Open_LDR( char* name, int mode) {
   FILE *fp;

   if  ( mode == READ_ONLY )
      fp = fopen(name, "r");
   else if ( mode == WRITE_ONLY )
      fp = fopen(name, "w");
   else
      fp = ( FILE *) NULL;  
   return(fp);
}
void recalc_length(void)
{
   calc_length=TRUE;
}

void reset_rec_length(void)
{
   record_length=0;
   calc_length=FALSE;
}

int get_rec_length(void)
{
   return(record_length);
}

int get_recalc_status(void)
{
   return(calc_length);
}


int Check_Rec_Seq( int* val, unsigned char* tmp, SARL_ptr* tree, int mode) {
   int i=0;

   if (mode==1) {
      i = cvt2int( tmp );
      tree->read_count++;
      if (tree->read_count != i) i=tree->read_count;
   }
   else if (!mode) {
      if (!calc_length) {
         tree->write_count++;
         if (tree->write_count != *val) {
            cvt2char(&(tree->write_count), tmp);
            i=tree->write_count;
         }
         else {
            cvt2char( val, tmp );
            i=*val;
         }
      }
   }
   else {
      printf("Error: Check_Rec_Seq  -  Neither Read or Write mode\n");
   }

   return (i);
}



/************************************************************************/
/* read nitems of record into "buf" */
int read_record( FILE* fp, unsigned char* buf, size_t size, size_t nitems)
{
   int j, nbyte;
   unsigned char *tptr, rval;

   nbyte = fread(buf,sizeof(unsigned char),nitems,fp);

   if (feof(fp)) {
      printf("\n End of volume file reached\n");
      fflush(stdout);
      return(END_OF_FILE);
   }

   if (nbyte!=nitems) {
      printf("\n Error: Read requested %d items ______ returning %d",nitems,nbyte);
      return(READ_ERROR);
   } 
#if 0
#ifdef SWAP_READ
   for (j=0,tptr= (unsigned char *) buf;j<nitems;j+=4,tptr+=4) {
       rval = *tptr;
       *tptr = *(tptr+3);
       *(tptr+3) = rval;
       rval = *(tptr+1);
       *(tptr+1) = *(tptr+2);
       *(tptr+2) = rval;
   } 
#endif
#endif

   return(nbyte);
}

/************************************************************************/
/* write nitems of buf  to file */
int write_record( FILE* fp, unsigned char * buf, size_t size, size_t nitems)
{
   int j, nbyte;
   unsigned char *tptr, rval;
   
   if (calc_length) return(nitems);
#if 0
#ifdef SWAP_WRITE
   for (j=0,tptr = buf;j<nitems;j+=4,tptr+=4) {
       rval = *tptr;
       *tptr = *(tptr+3);
       *(tptr+3) = rval;
       rval = *(tptr+1);
       *(tptr+1)=*(tptr+2);
       *(tptr+2)=rval;
   } 
#endif
#endif
   nbyte = fwrite(buf,sizeof(unsigned char),nitems,fp);


   if (nbyte!=nitems) {
      printf("\n Error: writing requested %d items ______ returning %d",nitems,nbyte);
      return(READ_ERROR);
   } 
   return(nbyte);
}



/************************************************************************/
/* convert 4 bytes to an integer */
int cvt2int(unsigned char *cint)
{
   unsigned char *cnum;
   int num;
   int i;

   cnum = (unsigned char *) &num;
   for (i=0;i<4;i++) {
#if DEC
      cnum[3-i] = cint[i];
   }
#else
      cnum[i] = cint[i];
   }
#endif

   return(num);
}

/************************************************************************/
/* get integer out of ASCII field X chars wide */
int get_I4(unsigned char *cbuf, int field)
{
   char tmp[32];
   int x=0;

   strncpy(tmp, (char*) cbuf, field);
   tmp[field]='\0';
   sscanf(tmp,"%d",&x);
   return(x);
}

/************************************************************************/
/* get float out of ASCII field X chars wide */
float get_F4(unsigned char *cbuf, int field)
{
   char tmp[32];
   float x=0.0;

   strncpy(tmp, (char*) cbuf,field);
   tmp[field]='\0';
   sscanf(tmp,"%f",&x);
   return(x);
}

/************************************************************************/
/* convert 4 bytes to an integer */
void cvt2char(int *val, unsigned char* cint)
{
   unsigned char *cnum;
   int i;

   if (calc_length) {
      record_length+=4;
   }
   else {
      cnum = (unsigned char *) val;
      for (i=0;i<4;i++) {
#if DEC
         cint[3-i] = cnum[i];
#else
         cint[i] = cnum[i];
#endif
      }
   }
}

/************************************************************************/
/* get unsigned char string  X chars wide from an integer */
void put_I4(int n, char* field, int ifield, unsigned char* buf)
{
   char tmp[32];
   if (calc_length) 
      record_length+=ifield;
   else 
      sprintf( (char*) buf, field, n);

}

/************************************************************************/

void put_F4(float val, char * field, int ifield, unsigned char* buf)
{
   char tmp[32];
   if (calc_length) 
      record_length+=ifield;
   else 
      sprintf((char*) buf, field, val);
}

/************************************************************************/

void put_byte(unsigned char* out, unsigned char in)
{
   if (calc_length) 
      record_length++;
   else 
      *out = in;
}

/************************************************************************/

void put_chars( char* s1, char* s2, int ifield)
{
   int len;

   if (calc_length) {
      len = strlen(s2);
      if (len!=ifield) {
         printf("\n Warning: Trying to calculate record length\n\t input string length %d, stated length %d --- put_chars", len, ifield);
         record_length +=len;
      }
      else
         record_length+=ifield;
   }
   else 
      strncpy(s1,s2,ifield);
}

/************************************************************************/

void put_blanks( char* s1, int ifield) 
{
   int i;
   if (calc_length) 
      record_length+=ifield;
   else 
      for(i=0;i<ifield;i++) *(s1+i) = ' ';
}

/************************************************************************/

void zero_set( unsigned char* buf, int nitems)
{
    if (!calc_length) memset((char *)buf,0,nitems);
}

/************************************************************************/

/* get file name out of non-null-terminated char field 
int get_file_name(unsigned char *cbuf,int clen,char *fname)
{
   char tmpbuf[100];
   int i;

   for (i = 0; i < clen; i ++)
      tmpbuf[i] = (char) cbuf[i];
   tmpbuf[clen] = (char) 0;
   sscanf(tmpbuf,"%s",fname);
}
*/
#if 0
/************************************************************************/
/* get (return) file type (IMOP, SARL, or SART) */
int get_file_type(unsigned char *cbuf,int clen)
{
   char tmpbuf[100];
   int i; 

   for (i = 0; i < clen; i ++) 
      tmpbuf[i] = (char) cbuf[i]; 
   tmpbuf[clen] = (char) 0;

   for (i = 0; i < KNOWN_FILES; i ++)
      if (strcmp(cftype[i],tmpbuf) == 0)
         return(i);

   printf("unknown file type\n");
   exit(0);
}
#endif


/*  This is the C code to write the PMF file that the IMS needs
    to catalog the image products.  The module consists of two functions:
    pmf and ftoc.  The argument list to the function pmf includes only 
    inputs;  the only output of the module is the PMF file itself.  The
    function ftoc converts fortran character strings to C format.

    The first input is the name of the template file that contains the ODL
    structure.  The second input is the file name of the PMF output.  All
    other arguments are data values to be written to the PMF file.  These
    items are described in Section 3.3.2.3 of the CP-IMS interface specification,
    Version 1.0.  The items in the argument list are in the same order as
    in the document, with two exceptions:  the common header values are after
    the catalog metadata values, and the detailed metadata object has been left
    out.  Most of the data values are available in the processing request
    block or in the scan results file.
*/
     
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "odl.h"
FILE *fp;
FILE *fp2;


#define  CATALOG_METADATA           "SAR_FRAME_METADATA.CATALOG_METADATA"
#define  JOB_ID                     "JOB_ID"
#define  PLATFORM                   "PLATFORM"
#define  SENSOR                     "SENSOR"
#define  REVOLUTION                 "REVOLUTION"
#define  SEQUENCE                   "SEQUENCE"
#define  ACTIVITY_ID                "ACTIVITY_ID"
#define  MEDIA_ID                   "MEDIA_ID"
#define  MEDIA_TYPE                 "MEDIA_TYPE"
#define  MEDIA_LOCATION             "MEDIA_LOCATION"
#define  RECORDER_ID                "RECORDER_ID"
#define  STATION_ID                 "STATION_ID"
#define  FRAME_MODE                 "FRAME_MODE"
#define  SITE_NAME                  "SITE_NAME"
#define  MODE                       "MODE"
#define  SEGMENT_ID                 "SEGMENT_ID"
#define  DATASET                    "DATASET"
#define  FILE_VERSION               "FILE_VERSION"
#define  LOC_ARCH_DIR               "LOCAL_ARCHIVE_PATH"
#define  PRODUCT_ID                 "PRODUCT_ID"
#define  IMAGE_FILE                 "IMAGE_FILE"
#define  LDR_FILE                   "CEOS_LEADER_FILE"
#define  FRAME_ID                   "FRAME_ID"
#define  SUBFRAME_ID                "SUBFRAME_ID"
#define  START_ADDRESS              "START_ADDRESS"
#define  END_ADDRESS                "END_ADDRESS"
#define  IMG_REC_CNT                "IMAGE_RECORD_COUNT"
#define  IMG_1ST_REC_LEN            "IMAGE_FIRST_RECORD_LENGTH"
#define  IMG_MAX_REC_LEN            "IMAGE_MAX_RECORD_LENGTH"
#define  LDR_MAX_REC_LEN            "LEADER_MAX_RECORD_LENGTH"
#define  LDR_REC_CNT                "LEADER_RECORD_COUNT"
#define  PROD_CREAT                 "PRODUCT_CREATOR"
#define  CEOS_TYPE                  "CEOS_PRODUCT_TYPE"
#define  START_TIME                 "START_TIME"
#define  END_TIME                   "END_TIME"
#define  CENTER_LAT                 "CENTER_LAT"
#define  CENTER_LON                 "CENTER_LON"
#define  CENTER_TIME                "CENTER_TIME"
#define  NS_LAT                     "NEAR_START_LAT"
#define  NS_LON                     "NEAR_START_LON"
#define  NE_LAT                     "NEAR_END_LAT"
#define  NE_LON                     "NEAR_END_LON"
#define  FS_LAT                     "FAR_START_LAT"
#define  FS_LON                     "FAR_START_LON"
#define  FE_LAT                     "FAR_END_LAT"
#define  FE_LON                     "FAR_END_LON"
#define  ASC_DESC                   "ASC_DESC"
#define  PROC_VER                   "PROC_VERSION"
#define  STATE_VECTOR_PRECISION     "STATE_VECTOR_PRECISION"
#define  SNR                        "SIGNAL_TO_NOISE_RATIO"
#define  RADIO_ACC                  "RADIOMETRIC_ACCURACY"
#define  PROD_TIME                  "PRODUCT_CREATION_TIME"
#define  HEADER                     "SAR_FRAME_METADATA.COMMON_HEADER"
#define  TIME                       "TIME"
#define  MSG_TYPE                   "MSG_TYPE"
#define  DEST                       "DESTINATION"
#define  SOURCE                     "SOURCE"
#define  NUM_REC                    "NUMBER_OF_RECORDS"

void ftoc(int len, char *str);

void pmf(char *in_fname, char *out_fname, int *job_id, char *platform,
  char *sensor, int *rev, int *sequence, char *act_id, char *media_id, 
  char *media_type, char *media_loc, char *recorder_id, char *station_id, 
  char *frame_mode, char *site_name, char *mode, int *segment_id, char *dataset, 
  char *loc_arch_dir, char *product_id, char *image_file, char *image_path, 
  char *ldr_file, int *frame_id,
  int *subframe_id, int *start_address, int *end_address, int *img_rec_cnt,
  int *img_1st_rec_len, int *img_max_rec_len, int *ldr_max_rec_len,
  int *ldr_rec_cnt, char *prod_creat, char *prod_type, char *start_time,
  char *end_time, double *center_lat, double *center_lon, char *center_time,
  double *ns_lat, double *ns_lon, double *ne_lat, double *ne_lon,
  double *fs_lat, double *fs_lon, double *fe_lat, double *fe_lon,
  char *asc_desc, char *proc_ver, char *ephem_type, float *snr, float *radio_acc,
  char *prod_time, char *time, char *msg_type, char *dest, char *source,
  int *num_rec, int *istatus)
{
    ODL odl;
    char *sss;
    char errstr[100];
    int bufsiz;
    int i;
    ODL *hdrODL;
    ODL *ctlgODL;
    char jobstr[24];
    char revstr[24];
    char seqstr[24];
    char segstr[24];
    char frmstr[24];
    char subfrmstr[24];
    char staddrstr[24];
    char endaddrstr[24];
    char  ircstr[24];
    char  ifrlstr[24];
    char  imrlstr[24];
    char  lmrlstr[24];
    char  lrcstr[24];
    char  clatstr[24];
    char  clonstr[24];
    char  nslatstr[24];
    char  nslonstr[24];
    char  nelatstr[24];
    char  nelonstr[24];
    char  fslatstr[24];
    char  fslonstr[24];
    char  felatstr[24];
    char  felonstr[24];
    char  snrstr[24];
    char  radstr[24];
    char  recstr[24];
    char  file_ver[64];
    char  ceos_type[64];

    printf(" In pmf routine 1\n");
    ftoc(60,in_fname);
    printf(" In pmf routine 2\n");
    ftoc(60,out_fname);
    printf(" In pmf routine 3\n");

    *istatus = iok;

    bufsiz = 10000;
    fp2=fopen(in_fname,"r");
    if (fp2 == NULL)
      {
      *istatus = ierr_2;
      printclog(3,"CANT OPEN PMF TEMPLATE FILE");
      printf("CANT OPEN PMF TEMPLATE FILE\n");
      return;
      }

    fp=fopen(out_fname,"w");
    if (fp == NULL)
      {
      *istatus = ierr_2;
      printclog(3,"CANT OPEN PMF OUTPUT FILE");
      printf("CANT OPEN PMF OUTPUT FILE\n");
      return;
      }

    printf(" In pmf routine 4\n");

    if ((odl = ODLparse(in_fname,0,errstr)) == NULL) {
      printclog(3,"IN PMF, CANT PARSE FILE");
      printf("IN PMF, CANT PARSE FILE\n");
      *istatus = ierr_11;
      return;
      }


    if ((hdrODL = (ODL*) Val(Lookup(odl, HEADER))) == NULL) {
      printclog(3,"IN PMF,  CANT FIND COMMON_HEADER");
      printf("IN PMF,  CANT FIND COMMON_HEADER\n");
      ODLFree(odl);
      *istatus = ierr_12;
      return;
      }

    for (i = 0; hdrODL[i] != NULL; i++) {
      if (strcasecmp(Name(hdrODL[i]), TIME) == 0) 
        break;
      }
      if (hdrODL[i] == NULL) {
        printclog(3," CANT FIND TIME");
        printf(" CANT FIND TIME\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(64,time);
      SetVal(hdrODL[i],time);    

    for (i = 0; hdrODL[i] != NULL; i++) {
      if (strcasecmp(Name(hdrODL[i]), MSG_TYPE) == 0) 
        break;
      }
      if (hdrODL[i] == NULL) {
        printclog(3," CANT FIND MSG_TYPE");
        printf(" CANT FIND MSG_TYPE\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(64,msg_type);
      SetVal(hdrODL[i],msg_type);    

    for (i = 0; hdrODL[i] != NULL; i++) {
      if (strcasecmp(Name(hdrODL[i]), DEST) == 0) 
        break;
      }
      if (hdrODL[i] == NULL) {
        printclog(3," CANT FIND DESTINATION");
        printf(" CANT FIND DESTINATION\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(64,dest);
      SetVal(hdrODL[i],dest);    

    for (i = 0; hdrODL[i] != NULL; i++) {
      if (strcasecmp(Name(hdrODL[i]), SOURCE) == 0) 
        break;
      }
      if (hdrODL[i] == NULL) {
        printclog(3," CANT FIND SOURCE");
        printf(" CANT FIND SOURCE\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(64,source);
      SetVal(hdrODL[i],source);    

    for (i = 0; hdrODL[i] != NULL; i++) {
      if (strcasecmp(Name(hdrODL[i]), NUM_REC) == 0) 
        break;
      }
      if (hdrODL[i] == NULL) {
        printclog(3," CANT FIND NUMBER_OF_RECORDS");
        printf(" CANT FIND NUMBER_OF_RECORDS\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(recstr,"%d",*num_rec);
      SetVal(hdrODL[i],recstr);    

    printf ("In pmf, done with common header");

    if ((ctlgODL = (ODL*) Val(Lookup(odl, CATALOG_METADATA))) == NULL) {
      printclog(3,"  CANT FIND METADATA");
      printf("  CANT FIND METADATA\n");
      ODLFree(odl);
      *istatus = ierr_12;
      return;
      }
    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), JOB_ID) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND JOB ID");
        printf(" CANT FIND JOB ID\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      sprintf(jobstr,"%d",*job_id);
      SetVal(ctlgODL[i],jobstr);    

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), PLATFORM) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND PLATFORM");
        printf(" CANT FIND PLATFORM\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      ftoc(60,platform);
      SetVal(ctlgODL[i],platform);    

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), SENSOR) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND SENSOR");
        printf(" CANT FIND SENSOR\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      ftoc(60,sensor);
      SetVal(ctlgODL[i],sensor);    

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), REVOLUTION) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND REV");
        printf(" CANT FIND REV\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      sprintf(revstr,"%d",*rev);
      SetVal(ctlgODL[i],revstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), SEQUENCE) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND SEQUENCE");
        printf(" CANT FIND SEQUENCE\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      sprintf(seqstr,"%d",*sequence);
      SetVal(ctlgODL[i],seqstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), ACTIVITY_ID) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND ACTIVITY_ID");
        printf(" CANT FIND ACTIVITY_ID\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      ftoc(60,act_id);
      SetVal(ctlgODL[i],act_id);  


    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), MEDIA_ID) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND MEDIA_ID");
        printf(" CANT FIND MEDIA_ID\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      ftoc(60,media_id);
      SetVal(ctlgODL[i],media_id);  


    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), MEDIA_TYPE) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND MEDIA_TYPE");
        printf(" CANT FIND MEDIA_TYPE\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      ftoc(60,media_type);
      SetVal(ctlgODL[i],media_type);  


    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), MEDIA_LOCATION) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND MEDIA_LOCATION");
        printf(" CANT FIND MEDIA_LOCATION\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(60,media_loc);
      SetVal(ctlgODL[i],media_loc);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), RECORDER_ID) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND RECORDER_ID");
        printf(" CANT FIND RECORDER_ID\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(60,recorder_id);
      SetVal(ctlgODL[i],recorder_id);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), STATION_ID) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND STATION_ID");
        printf(" CANT FIND STATION_ID\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(60,station_id);
      SetVal(ctlgODL[i],station_id);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), FRAME_MODE) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND FRAME_MODE");
        printf(" CANT FIND FRAME_MODE\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(60,frame_mode);
      SetVal(ctlgODL[i],frame_mode);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), SITE_NAME) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND SITE_NAME");
        printf(" CANT FIND SITE_NAME\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(60,site_name);
      SetVal(ctlgODL[i],site_name);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), MODE) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND MODE");
        printf(" CANT FIND MODE\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      ftoc(64,mode);
      SetVal(ctlgODL[i],mode);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), SEGMENT_ID) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND SEGMENT_ID");
        printf(" CANT FIND SEGMENT_ID\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(segstr,"%d",*segment_id);
      SetVal(ctlgODL[i],segstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), DATASET) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND DATASET");
        printf(" CANT FIND DATASET\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(64,dataset);
      SetVal(ctlgODL[i],dataset);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), LOC_ARCH_DIR) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND LOCAL_ARCHIVE_DIRECTORY");
        printf(" CANT FIND LOCAL_ARCHIVE_DIRECTORY\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(100,loc_arch_dir);
      SetVal(ctlgODL[i],loc_arch_dir);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), PRODUCT_ID) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND PRODUCT_ID");
        printf("IN PMF, CANT FIND PRODUCT_ID\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(60,product_id);
      SetVal(ctlgODL[i],product_id);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), IMAGE_FILE) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND IMAGE_FILE");
        printf("IN PMF, CANT FIND IMAGE_FILE\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(100,image_path);
      SetVal(ctlgODL[i],image_path);  

/*    ceos_type[0] = image_file[12];
    ceos_type[1] = '\0';                commented out by JMS 2-25-96  */

    memcpy(file_ver,&image_file[13],3);
    file_ver[3] = '\0';

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), FILE_VERSION) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND FILE_VERSION");
        printf(" CANT FIND FILE_VERSION\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      SetVal(ctlgODL[i],file_ver);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), CEOS_TYPE) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND CEOS_PRODUCT_TYPE");
        printf(" CANT FIND CEOS_PRODUCT_TYPE\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(64,prod_type);
      SetVal(ctlgODL[i],prod_type);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), LDR_FILE) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND LDR_FILE");
        printf("IN PMF, CANT FIND LDR_FILE\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(100,ldr_file);
      SetVal(ctlgODL[i],ldr_file);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), FRAME_ID) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND FRAME_ID");
        printf("IN PMF, CANT FIND FRAME_ID\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(frmstr,"%d",*frame_id);
      SetVal(ctlgODL[i],frmstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), SUBFRAME_ID) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND SUBFRAME_ID");
        printf("IN PMF, CANT FIND SUBFRAME_ID\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(subfrmstr,"%d",*subframe_id);
      SetVal(ctlgODL[i],subfrmstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), START_ADDRESS) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND START_ADDRESS");
        printf("IN PMF, CANT FIND START_ADDRESS\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(staddrstr,"%d",*start_address);
      SetVal(ctlgODL[i],staddrstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), END_ADDRESS) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND END_ADDRESS");
        printf("IN PMF, CANT FIND END_ADDRESS\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      sprintf(endaddrstr,"%d",*end_address);
      SetVal(ctlgODL[i],endaddrstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), IMG_REC_CNT) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND IMAGE_RECORD_COUNT");
        printf(" CANT FIND IMAGE_RECORD_COUNT\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(ircstr,"%d",*img_rec_cnt);
      SetVal(ctlgODL[i],ircstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), IMG_1ST_REC_LEN) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND IMAGE_FIRST_RECORD_LENGTH");
        printf(" CANT FIND IMAGE_FIRST_RECORD_LENGTH\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(ifrlstr,"%d",*img_1st_rec_len);
      SetVal(ctlgODL[i],ifrlstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), IMG_MAX_REC_LEN) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND IMAGE_MAX_RECORD_LENGTH");
        printf(" CANT FIND IMAGE_MAX_RECORD_LENGTH\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      sprintf(imrlstr,"%d",*img_max_rec_len);
      SetVal(ctlgODL[i],imrlstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), LDR_MAX_REC_LEN) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND LEADER_MAX_RECORD_LENGTH");
        printf(" CANT FIND LEADER_MAX_RECORD_LENGTH\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      sprintf(lmrlstr,"%d",*ldr_max_rec_len);
      SetVal(ctlgODL[i],lmrlstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), LDR_REC_CNT) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND LEADER_RECORD_COUNT");
        printf(" CANT FIND LEADER_RECORD_COUNT\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      sprintf(lrcstr,"%d",*ldr_rec_cnt);
      SetVal(ctlgODL[i],lrcstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), PROD_CREAT) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND PRODUCT_CREATOR");
        printf(" CANT FIND PRODUCT_CREATOR\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      ftoc(64,prod_creat);
      SetVal(ctlgODL[i],prod_creat);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), START_TIME) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND START_TIME");
        printf("IN PMF, CANT FIND START_TIME\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      ftoc(64,start_time);
      SetVal(ctlgODL[i],start_time);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), END_TIME) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND END_TIME");
        printf("IN PMF, CANT FIND END_TIME\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(64,end_time);
      SetVal(ctlgODL[i],end_time);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), CENTER_LAT) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND CENTER_LAT");
        printf("IN PMF, CANT FIND CENTER_LAT\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(clatstr,"%f",*center_lat);
      SetVal(ctlgODL[i],clatstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), CENTER_LON) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND CENTER_LON");
        printf("IN PMF, CANT FIND CENTER_LON\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(clonstr,"%f",*center_lon);
      SetVal(ctlgODL[i],clonstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), CENTER_TIME) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND CENTER_TIME");
        printf("IN PMF, CANT FIND CENTER_TIME\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(64,center_time);
      SetVal(ctlgODL[i],center_time);   

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), NS_LAT) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND NEAR_START_LAT");
        printf("IN PMF, CANT FIND NEAR_START_LAT\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      sprintf(nslatstr,"%f",*ns_lat);
      SetVal(ctlgODL[i],nslatstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), NS_LON) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND NEAR_START_LON");
        printf("IN PMF, CANT FIND NEAR_START_LON\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(nslonstr,"%f",*ns_lon);
      SetVal(ctlgODL[i],nslonstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), NE_LAT) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF CANT FIND NEAR_END_LAT");
        printf("IN PMF CANT FIND NEAR_END_LAT\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(nelatstr,"%f",*ne_lat);
      SetVal(ctlgODL[i],nelatstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), NE_LON) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND NEAR_END_LON");
        printf("IN PMF, CANT FIND NEAR_END_LON\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(nelonstr,"%f",*ne_lon);
      SetVal(ctlgODL[i],nelonstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), FS_LAT) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND FAR_START_LAT");
        printf("IN PMF, CANT FIND FAR_START_LAT\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      sprintf(fslatstr,"%f",*fs_lat);
      SetVal(ctlgODL[i],fslatstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), FS_LON) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND FAR_START_LON");
        printf("IN PMF, CANT FIND FAR_START_LON\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(fslonstr,"%f",*fs_lon);
      SetVal(ctlgODL[i],fslonstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), FE_LAT) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND FAR_END_LAT");
        printf("IN PMF, CANT FIND FAR_END_LAT\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(felatstr,"%f",*fe_lat);
      SetVal(ctlgODL[i],felatstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), FE_LON) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND FAR_END_LON");
        printf("IN PMF, CANT FIND FAR_END_LON\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(felonstr,"%f",*fe_lon);
      SetVal(ctlgODL[i],felonstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), ASC_DESC) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3,"IN PMF, CANT FIND ASC_DESC");
        printf("IN PMF, CANT FIND ASC_DESC\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(2,asc_desc);
      SetVal(ctlgODL[i],asc_desc);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), PROC_VER) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND PROCESSOR_VERSION");
        printf(" CANT FIND PROCESSOR_VERSION\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(60,proc_ver);
      SetVal(ctlgODL[i],proc_ver);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), STATE_VECTOR_PRECISION) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND STATE_VECTOR_PRECISION");
        printf(" CANT FIND STATE_VECTOR_PRECISION\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(60,ephem_type);
      SetVal(ctlgODL[i],ephem_type);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), SNR) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND SIGNAL_TO_NOISE_RATIO");
        printf(" CANT FIND SIGNAL_TO_NOISE_RATIO\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      sprintf(snrstr,"%f",*snr);
      SetVal(ctlgODL[i],snrstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), RADIO_ACC) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND RADIOMETRIC_ACCURACY");
        printf(" CANT FIND RADIOMETRIC_ACCURACY\n");
        ODLFree(odl);
      *istatus = ierr_12;
        return;
        }
      sprintf(radstr,"%f",*radio_acc);
      SetVal(ctlgODL[i],radstr);  

    for (i = 0; ctlgODL[i] != NULL; i++) {
      if (strcasecmp(Name(ctlgODL[i]), PROD_TIME) == 0) 
        break;
      }
      if (ctlgODL[i] == NULL) {
        printclog(3," CANT FIND PRODUCT_CREATION_TIME");
        printf(" CANT FIND PRODUCT_CREATION_TIME\n");
      *istatus = ierr_12;
        ODLFree(odl);
        return;
        }
      ftoc(64,prod_time);
      SetVal(ctlgODL[i],prod_time);  

    printf ("In pmf, done with catalog metadata\n");

  sss=  ODLToStr(odl,NULL);

  fprintf(fp,"%s\n",sss);  
  fclose(fp); 
}


void ftoc(int len, char *str)
{
int i, j;

  for (i = 0; i < (len - 1); i++)
    {
    j = (len - 2) - i;
    if (str[j] == ' ')
      str[j] = '\0';
    else
      break;
    }
  str[(len - 1)] = '\0';
}


#include "Array.h"
#include "Int.h"
#include "Double.h"
#include "Seq2.h"
#include "Seq.h"
 #include <stdarg.h> 
 #include <stdio.h> 
 #include "odl.h"  




#define CAL_PARAM            "CAL_PARAM"
#define CONV_FACTORS         "CONV_FACTORS"
#define NOISE_EST            "NOISE_EST"
#define IM_QUALITY           "IM_QUALITY"
#define GEO_ANAL             "GEO_ANALYSIS"
#define RAD_ACC              "RADIO_ACCURACY"
#define RAD_VECTOR           "RAD_VECTOR"
#define NPIXELS              "NPIXELS"
#define TRAD_VALUE           "RAD_VALUE"
#define RAD_VALUE            "RAD_VALUES"
#define GAIN_TEMP            "GAIN_TEMP"
#define NVALUES              "NVALUES"
#define TGAIN_VALUE          "GAIN_VALUE"
#define GAIN_VALUE           "GAIN_VALUES"
#define NOISE_FACT           "NOISE_FACT"
#define LIN_CONV_FACT        "LINEAR_CONV_FACT"
#define OFFSET_CONV_FACT     "OFFSET_CONV_FACT"
#define ABS_COEFF            "ABS_COEFF"
#define NOISE_PRO            "NOISE_PRO"
#define NOISE_PVS            "NOISE_PVS"
#define NOISE_DEL            "NOISE_DELTA"
#define RNG_RES              "RANGE_RES"
#define AZ_RES               "AZIMUTH_RES"
#define RNG_PSLR             "RANGE_PSLR"
#define AZ_PSLR              "AZIMUTH_PSLR"
#define RNG_ISLR             "RANGE_ISLR"
#define AZ_ISLR              "AZIMUTH_ISLR"
#define RNG_AMB              "RNG_AMBIG"
#define AZI_AMB              "AZI_AMBIG"
#define ORI_ERR              "ORI_ERR"
#define SKEW_ERR             "SKEW_ERR"
#define CS_ERR               "CROSS_S_ERR"
#define AS_ERR               "ALONG_S_ERR"
#define CL_ERR               "CROSS_L_ERR"
#define AL_ERR               "ALONG_L_ERR"
#define ISO_R                "ISO_RATIO"
#define REL_RAD_ACC          "REL_RADIO_ACC"
#define DYN_RNG              "DYN_MG"
#define CAL_SNR              "SNR"

#define NVALS_RAD            2048
#define NVALS_GAIN           100

void c2f(char *str, int len);
void str_init(char *str, int len);

void getcaldata(char *fname, double *noisf, double *lincf,
  double *offcf, double *abscf, double *noispro, double *noispvs,
  double *noisdel, double *rngres, double *azres, double *rngpslr,
  double *azpslr, double *rngislr, double *azislr, double *rngamb,
  double *azamb, double *orierr, double *skerr, double *cserr, double *aserr,
  double *clerr, double *alerr, double *isor, double *relra, double *dynrng, 
  double *snr,int *npix, double *elang,double *incang,double *slrng,
  double *radgain, int *nvals, double *temp, double *tempgain,
  int *status, int *rad_v_stat, int *gain_stat, int *cnv_fac_stat,
  int *noise_stat, int *im_qual_stat, int *geo_anal_stat, int *rad_acc_stat,
  int *istatus)
{

   int      fd, i, j, k, l, m, *frame_id, err;
   int      segment_id = 0;
   int      done, found, frameN, segmentN;
   ODL      odl, *bodyODL, *tableODL, *dataODL, *dummyODL;
   ODL      *rad_value_vectors, rad_value_odl;
   ODL      *gain_value_vectors, gain_value_odl;
   ODL      *vector_elems;

   char     errstr[256]; 
   double   *noise_fact;
   double   *lin_conv_fact;
   double   *offset_conv_fact;
   double   *abs_coeff;
   double   *noise_pro;
   double   *noise_pvs;
   double   *noise_del;
   double   *range_res;
   double   *azimuth_res;
   double   *range_pslr;
   double   *azimuth_pslr;
   double   *range_islr;
   double   *azimuth_islr;
   double   *rng_ambig;
   double   *azi_ambig;
   double   *ori_err;
   double   *skew_err;
   double   *cross_s_err;
   double   *along_s_err;
   double   *cross_l_err;
   double   *along_l_err;
   double   *iso_ratio;
   double   *rel_radio_acc;
   double   *dyn_rng;
   double   *sig_n_r;
   int      *npixels;
   int      *nvalues;

   FILE *fp;

   printf("HELLO 1 in getcaldata\n");
   *istatus = 0;

   done = 0;
   for (i=0; i < 60 && !done; i++)
     {
     j = 59 - i;
     if (fname[j] == ' ')
       fname[j] = '\0';
     else
       done = 1;
     }
   printf("HELLO 2 in getcaldata\n");

   fp = fopen(fname, "r");
   if (fp == NULL) 
    {
    *istatus = ierr_2;
     printclog(3," CANT_OPEN_CAL_FILE");
     printf(" CANT_OPEN_CAL_FILE\n");
     return;
    }


   if ((odl = ODLparse(fname, 0, errstr)) == NULL) {
      printclog(3," CANT_PRASE_CAL_FILE");
      printf(" CANT_PARSE_CAL_FILE\n");
      *istatus = ierr_11;
      return;
   }
   /* add up all the frame number in this frame file */
   if ((bodyODL = (ODL*) Val(Lookup(odl, CAL_PARAM))) == NULL) {
      printclog(3," CANT_FIND_CAL_PARAM");
      printf(" CANT_FIND_CAL_PARAM\n");
      ODLFree(odl);
      *istatus = ierr_12;
      return;
   }

   found = 0;

   for (i=0; bodyODL[i]!=NULL && !found; ++i) {
      /* find CONV_FACTORS */
      if (strcasecmp(Name(bodyODL[i]), CONV_FACTORS) ||
          (dataODL = (ODL*) Val(bodyODL[i])) == NULL) 
         continue;
      else
        found = 1;
      }

         if (!found) {
            printclog(3," CANT_FIND_CONV_FACTORS");
            printf(" CANT_FIND_CONV_FACTORS\n");
            *cnv_fac_stat = -1;
         }


    if (found)
      {
        found = 0;

         /* find NOISE_FACT */
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), NOISE_FACT) ||
              (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
         else
          found = 1;
         }

         if (!found) {
            printclog(3," CANT_FIND_NOISE_FACT");
            printf(" CANT_FIND_NOISE_FACT\n");
            *noisf = -99.0; 
         }
         else  {
           k--;
           noise_fact = (double*) Val(dataODL[k]);
           *noisf = *noise_fact;  
         }

         /* find LINEAR_CONV_FACT */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), LIN_CONV_FACT) ||
                (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
         else
           found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_LIN_CONV_FACT");
            printf(" CANT_FIND_LIN_CONV_FACT\n");
            *lincf = -99.0;
         }
         else  {
            k--;
           lin_conv_fact = (double*) Val(dataODL[k]);
           *lincf = *lin_conv_fact;
         }

         /* find OFFSET_CONV_FACT */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), OFFSET_CONV_FACT) ||
              (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
         else
           found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_OFFSET_CONV_FACT");
            *offcf = -99.0;
         }
         else  {
           k--;
           offset_conv_fact = (double*) Val(dataODL[k]);
           *offcf = *offset_conv_fact;
         }

         /* find ABS_COEFF */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), ABS_COEFF) ||
              (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
         else
           found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_ABS_COEFF");
            *abscf = -99.0;
         }
         else  {
           k--;
           abs_coeff = (double*) Val(dataODL[k]);
           *abscf = *abs_coeff;
         }
    }


   found = 0;
   for (i=0; bodyODL[i]!=NULL && !found; ++i) {
      /* find NOISE_EST */
      if (strcasecmp(Name(bodyODL[i]), NOISE_EST) ||
          (dataODL = (ODL*) Val(bodyODL[i])) == NULL) 
         continue; 
      else
        found = 1;
      }

         if (!found) {
            printclog(3," CANT_FIND_NOISE_EST");
            printf(" CANT_FIND_NOISE_EST\n");
            *noise_stat = -1;
         }


   if (found)
    {
         /* find NOISE_PRO */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), NOISE_PRO) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
         else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_NOISE_PRO");
            *noispro = -99.0;
         }
         else  {
           k--;
           noise_pro = (double*) Val(dataODL[k]);
           *noispro = *noise_pro;
         }


         /* find NOISE_PVS */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), NOISE_PVS) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
         else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_NOISE_PVS");
            *noispvs = -99.0;
         }
         else  {
           k--;
           noise_pvs = (double*) Val(dataODL[k]);
           *noispvs = *noise_pvs;
         }


         /* find NOISE_DELTA */
         found = 0;
         for (k=0; dataODL[k]!=NULL; ++k) {
            if (strcasecmp(Name(dataODL[k]), NOISE_DEL) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
         else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_NOISE_DELTA");
            *noisdel = -99.0;
         }
         else  {
           k--;
           noise_del = (double*) Val(dataODL[k]);
           *noisdel = *noise_del;
         }
    }


   found = 0;
   for (i=0; bodyODL[i]!=NULL && !found; ++i) {
      /* find IM_QUALITY */
      if (strcasecmp(Name(bodyODL[i]), IM_QUALITY) ||
          (dataODL = (ODL*) Val(bodyODL[i])) == NULL) 
         continue;
      else
        found = 1;
      }

         if (!found) {
            printclog(3," CANT_FIND_IM_QUALITY");
            printf(" CANT_FIND_IM_QUALITY\n");
            *im_qual_stat = -1;
         }


   if (found)
    {
         /* find RANGE_RES */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), RNG_RES) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
           else
             found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_RANGE_RES");
            *rngres = -99.0;
         }
         else  {
           k--;
           range_res = (double*) Val(dataODL[k]);
           *rngres  = *range_res;
         }

         /* find AZIMUTH_RES */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), AZ_RES) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
             else
                found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_AZIMUTH_RES");
            *azres = -99.0;
         }
         else  {
           k--;
           azimuth_res = (double*) Val(dataODL[k]);
           *azres = *azimuth_res;
         }

         /* find RANGE PSLR */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), RNG_PSLR) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
          else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_RANGE PSLR");
            *rngpslr = -99.0;
         }
         else  {
           k--;
           range_pslr = (double*) Val(dataODL[k]);
           *rngpslr = *range_pslr;
         }

         /* find AZIMUTH PSLR */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), AZ_PSLR) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
          else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_AZIMUTH PSLR");
            *azpslr = -99.0;
         }
         else  {
           k--;
           azimuth_pslr = (double*) Val(dataODL[k]);
           *azpslr = *azimuth_pslr;
         }

         /* find RANGE ISLR */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), RNG_ISLR) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
          else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_RANGE ISLR");
            *rngislr = -99.0;
         }
         else  {
           k--;
           range_islr = (double*) Val(dataODL[k]);
           *rngislr = *range_islr;
         }

         /* find AZIMUTH ISLR */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), AZ_ISLR) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
          else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_AZIMUTH ISLR");
            *azislr = -99.0;
         }
         else  {
           k--;
           azimuth_islr = (double*) Val(dataODL[k]);
           *azislr = *azimuth_islr;
         }

         /* find RANGE AMBIG */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), RNG_AMB) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
          else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_RANGE AMBIG");
            *rngamb = -99.0;
         }
         else  {
           k--;
           rng_ambig = (double*) Val(dataODL[k]);
           *rngamb = *rng_ambig;
         }

         /* find AZIMUTH AMBIG */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), AZI_AMB) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
          else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_AZIMUTH AMBIG");
            *azamb = -99.0;
         }
         else  {
           k--;
           azi_ambig = (double*) Val(dataODL[k]);
           *azamb = *azi_ambig;
         }
    }


   found = 0;
   for (i=0; bodyODL[i]!=NULL && !found; ++i) {
      /* find GEO_ANALYSIS */
      if (strcasecmp(Name(bodyODL[i]), GEO_ANAL) ||
          (dataODL = (ODL*) Val(bodyODL[i])) == NULL) 
         continue;
      else
        found = 1;
      }

         if (!found) {
            printclog(3," CANT_FIND_GEO_ANALYSIS");
            printf(" CANT_FIND_GEO_ANALYSIS\n");
            *geo_anal_stat = -1;
         }

   if (found)
    {
         /* find ORI_ERR */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), ORI_ERR) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
          else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_ORI_ERR");
            *orierr = -99.0;
         }
         else  {
           k--;
           ori_err = (double*) Val(dataODL[k]);
           *orierr  = *ori_err;
         }

         /* find SKEW_ERR */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), SKEW_ERR) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
          else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_SKEW_ERR");
            *skerr = -99.0;
         }
         else  {
           k--;
           skew_err = (double*) Val(dataODL[k]);
           *skerr = *skew_err;
         }

         /* find CROSS_S_ERR */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), CS_ERR) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
          else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_CROSS_S_ERR");
            *cserr = -99.0;
         }
         else  {
           k--;
           cross_s_err = (double*) Val(dataODL[k]);
           *cserr = *cross_s_err;
         }

         /* find ALONG_S_ERR */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), AS_ERR) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
          else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_ALONG_S_ERR");
            printf(" CANT_FIND_ALONG_S_ERR");
            *aserr = -99.0;
         }
         else  {
           k--;
           along_s_err = (double*) Val(dataODL[k]);
           *aserr = *along_s_err;
         }

         /* find CROSS_L_ERR */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), CL_ERR) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
          else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_CROSS_L_ERR");
            printf(" CANT_FIND_CROSS_L_ERR");
            *clerr = -99.0;
         }
         else  {
           k--;
           cross_l_err = (double*) Val(dataODL[k]);
           *clerr = *cross_l_err;
         }

         /* find ALONG_L_ERR */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), AL_ERR) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
          else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_ALONG_L_ERR");
            printf(" CANT_FIND_ALONG_L_ERR");
            *alerr = -99.0;
         }
         else  {
           k--;
           along_l_err = (double*) Val(dataODL[k]);
           *alerr = *along_l_err;
         }

         /* find ISO_RATIO */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), ISO_R) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
          else
            found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_ISO_RATIO");
            printf(" CANT_FIND_ISO_RATIO");
            *isor = -99.0;
         }
         else  {
           k--;
           iso_ratio = (double*) Val(dataODL[k]);
           *isor = *iso_ratio;
         }
    }


   found = 0;
   for (i=0; bodyODL[i]!=NULL && !found; ++i) {
      /* find RADIO_ACCURACY */
      if (strcasecmp(Name(bodyODL[i]), RAD_ACC) ||
          (dataODL = (ODL*) Val(bodyODL[i])) == NULL) 
         continue;
      else
        found = 1;
      }

/*         if (bodyODL[i] == NULL) {  */
         if (!found) {
            printclog(3," CANT_FIND_RADIO_ACCURACY");
            printf(" CANT_FIND_RADIO_ACCURACY\n");
            *rad_acc_stat = -1;
         }

   if (found)
    {
         /* find REL_RADIO_ACC */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), REL_RAD_ACC) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
            else
               found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_REL_RADIO_ACC");
            printf(" CANT_FIND_REL_RADIO_ACC");
            *relra = -99.0;
         }
         else {
           k--;
           rel_radio_acc = (double*) Val(dataODL[k]);
           *relra  = *rel_radio_acc;
         }

         /* find DYNAMIC RANGE*/
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), DYN_RNG) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
            else
               found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_DYNAMIC RANGE");
            printf(" CANT_FIND_DYNAMIC RANGE");
            *dynrng = -99.0;
         }
         else {
           k--;
           dyn_rng = (double*) Val(dataODL[k]);
           *dynrng = *dyn_rng;
         }

         /* find SNR */
         found = 0;
         for (k=0; dataODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(dataODL[k]), CAL_SNR) ||
             (dummyODL = (ODL*) Val(dataODL[k])) == NULL)
               continue;
            else
               found = 1;
         }
         if (!found) {
            printclog(3," CANT_FIND_SNR");
            printf(" CANT_FIND_SNR");
            *snr = -99.0;
         }
         else {
           k--;
           sig_n_r = (double*) Val(dataODL[k]);
           *snr = *sig_n_r;
         }
    }


   found = 0;
   for (i=0; bodyODL[i]!=NULL && !found; ++i) {
      /* find RAD_VECTOR */
      if (strcasecmp(Name(bodyODL[i]), RAD_VECTOR) ||
          (tableODL = (ODL*) Val(bodyODL[i])) == NULL) 
         continue;
      else
        found = 1;
      }

         if (!found) {
            printclog(3," CANT_FIND_RAD_VECTOR");
            printf(" CANT_FIND_RAD_VECTOR\n");
            *rad_v_stat = -1;
         }

   if (found)
    {
         /* find NPIXELS */
         for (k=0; tableODL[k]!=NULL; ++k) {
            if (strcasecmp(Name(tableODL[k]), NPIXELS) == 0)
               break;
         }
         if (tableODL[k] == NULL) {
            printclog(3," CANT_FIND_NPIXELS");
            ODLFree(odl);
            *istatus = ierr_12;
            return;
         }
         npixels = (int*) Val(tableODL[k]);
         *npix = *npixels;

   found = 0;
   for (j=0; tableODL[j]!=NULL && !found; ++j) {
      /* find RAD_VECTOR */
      if (strcasecmp(Name(tableODL[j]), TRAD_VALUE) ||
          (dataODL = (ODL*) Val(tableODL[j])) == NULL) 
         continue;
      else
        found = 1;
      }

         /* find RAD_VALUE */
         for (k=0; dataODL[k]!=NULL; ++k) {
            if (strcasecmp(Name(dataODL[k]), RAD_VALUE) == 0)
               break;
         }
         if (dataODL[k] == NULL) {
            printclog(3," CANT_FIND_RAD_VALUE");
            printf(" CANT_FIND_RAD_VALUE\n");
            ODLFree(odl);
            *istatus = ierr_12;
            return;
         }

    rad_value_odl = dataODL[k];

    /*  Make RAD_VALUE is array of vectors (or 2-dimensional array) */

    if (Type(Value(rad_value_odl)) != (Method_t) Seq2) {
	sprintf(errstr,"%s: bad type", RAD_VALUE);
        printclog(3,errstr);
        printf("rad value not 2dim array\n");
	ODLFree(odl);
	*istatus = ierr_12;
        return;
    }
    rad_value_vectors = Val(rad_value_odl);

    /*  Print out each element in each vector */

    for (i = 0; rad_value_vectors[i] != NULL; ++i) {

	/*  Make sure each vector is a sequence (or 1-dimensional array)  */

/*	if (Type(Value(rad_value_vectors[i])) != (Method_t) Seq) {
	    sprintf(errstr,"%s: bad type", RAD_VALUE);
            printclog(3,errstr);
            printf("rad vector not sequence\n");
	    ODLFree(odl);
	    *istatus = ierr_12;
            return;
	}     */
	vector_elems = Val(rad_value_vectors[i]);

	for (k = 0; k < 5; ++k) {
	    /*  Check the type of each vector element */

	    if (Type(vector_elems[k]) == (Method_t) Int)
		;

	    else if (Type(vector_elems[k]) == (Method_t) Double)
		;

	    else {
		sprintf(errstr,"Element [%d, %d] of %s: bad type", i+1, k, RAD_VALUE);
                printclog(3,errstr);
		ODLFree(odl);
		*istatus = ierr_12;
                return;
	    }  
	}
       elang[i] = * (double*) Val(vector_elems[1]);
       incang[i] = * (double*) Val(vector_elems[2]);
       slrng[i] = * (double*) Val(vector_elems[3]);
       radgain[i] = * (double*) Val(vector_elems[4]);
	/*  Make sure each vector has 5 elements */

	if (vector_elems[k] != NULL) {
	    sprintf(errstr,"Vector %d of %s: bad # of elements", i+1, RAD_VALUE);
            printclog(3,errstr);
	    ODLFree(odl);
	    *istatus = ierr_12;
            return;
       }	
     }
    }


   found = 0;
   for (i=0; bodyODL[i]!=NULL && !found; ++i) {
      /* find GAIN_VECTOR */
      if (strcasecmp(Name(bodyODL[i]), GAIN_TEMP) ||
          (tableODL = (ODL*) Val(bodyODL[i])) == NULL) 
         continue;
      else
        found = 1;
      }

         if (!found) {
            printclog(3," CANT_FIND_GAIN_TEMP");
            *gain_stat = -1;
         }

   if (found)
    {
         /* find NVALUES */
         for (k=0; tableODL[k]!=NULL; ++k) {
            if (strcasecmp(Name(tableODL[k]), NVALUES) == 0)
               break;
         }
         if (tableODL[k] == NULL) {
            printclog(3," CANT_FIND_NVALUES");
            ODLFree(odl);
            *istatus = ierr_12;
            return;
         }
         nvalues = (int*) Val(tableODL[k]);
         *nvals = *nvalues;


   found = 0;
   for (j=0; tableODL[j]!=NULL && !found; ++j) {
      /* find GAIN_VECTOR */
      if (strcasecmp(Name(tableODL[j]), TGAIN_VALUE) ||
          (dataODL = (ODL*) Val(tableODL[j])) == NULL) 
         continue;
      else
        found = 1;
      }


         /* find GAIN_VALUE */
         for (k=0; dataODL[k]!=NULL; ++k) {
            if (strcasecmp(Name(dataODL[k]), GAIN_VALUE) == 0)
               break;
         }
         if (dataODL[k] == NULL) {
            printclog(3," CANT_FIND_GAIN_VALUE");
            ODLFree(odl);
            *istatus = ierr_12;
            return;
         }

    gain_value_odl = dataODL[k];

    /*  Make GAIN_VALUE is array of vectors (or 2-dimensional array) */

    if (Type(Value(gain_value_odl)) != (Method_t) Seq2) {
	sprintf(errstr,"%s: bad type", GAIN_VALUE);
        printclog(3,errstr);
	ODLFree(odl);
	*status = -1;
        return;
    }
    gain_value_vectors = Val(gain_value_odl);

    /*  Print out each element in each vector */

    for (i = 0; gain_value_vectors[i] != NULL; ++i) {

	/*  Make sure each vector is a sequence (or 1-dimensional array)  */

/*	if (Type(Value(gain_value_vectors[i])) != (Method_t) Seq) {
	    sprintf(errstr,"%s: bad type", GAIN_VALUE);
            printclog(3,errstr);
	    ODLFree(odl);
	    *istatus = ierr_12;
            return;
	}   */
	vector_elems = Val(gain_value_vectors[i]);

	for (k = 0; k < 3; ++k) {
	    /*  Check the type of each vector element */

	    if (Type(vector_elems[k]) == (Method_t) Int)
		;

	    else if (Type(vector_elems[k]) == (Method_t) Double)
		;

	    else {
		sprintf(errstr,"Element [%d, %d] of %s: bad type", i+1, k, GAIN_VALUE);
                printclog(3,errstr);
		ODLFree(odl);
		*istatus = ierr_12;
                return;
	    }  
	}
       temp[i] = * (double*) Val(vector_elems[1]);
       tempgain[i] = * (double*) Val(vector_elems[2]);
	/*  Make sure each vector has 5 elements */

	if (vector_elems[k] != NULL) {
	    sprintf(errstr,"Vector %d of %s: bad # of elements", i+1, GAIN_VALUE);
            printclog(3,errstr);
	    ODLFree(odl);
	    *istatus = ierr_12;
            return;
       }	
     }
    }


   ODLFree(odl);
   *status = 0;
   *istatus =iok;
   return;

} 



#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "odl.h"
#include "Int.h"
#include "Double.h"
#include <stdarg.h> 

#define  DETAILED_METADATA            "CAL_PARAM.DETAILED_METADATA"


#define PVS_OBJ              "PVS_OBJ"
#define CALIB_FAC            "CALIB_FAC"
#define NOISE_EST            "NOISE_EST"
#define IM_QUALITY           "IM_QUALITY"
#define GEO_ANAL             "GEO_ANALYSIS"
#define REL_RAD_ACC          "REL_RADIO_ACC"
#define NOISE_FACT           "NOISE_FACT"
#define LIN_CONV_FACT        "LINEAR_CONV_FACT"
#define OFFSET_CONV_FACT     "OFFSET_CONV_FACT"
#define IM_ABSCOEF           "IM_ABSCOEF"
#define NT_CEOS_NOISE        "NT_CEOS_NOISE"
#define NT_IMG_NOISE         "NT_IMG_NOISE"
#define NT_DELTA             "NT_DELTA"
#define RANGE_RES            "RNG_RES"
#define AZI_RES              "AZI_RES"
#define CD_PSLR_RNG          "CD_PSLR_RNG"
#define CD_PSLR_AZM          "CD_PSLR_AZM"
#define CD_ISLR_RNG          "CD_ISLR_RNG"
#define CD_ISLR_AZM          "CD_ISLR_AZM"
#define RNG_AMB              "RNG_AMBIG"
#define AZI_AMB              "AZI_AMBIG"
#define ORI_ERR              "ORI_ERR"
#define DIS_SKEW             "DIS_SKEW"
#define CRT_SCALE            "CRT_SCALE"
#define ALT_SCALE            "ALT_SCALE"
#define CRT_LOCERR           "CRT_LOCERR"
#define ALT_LOCERR           "ALT_LOCERR"
#define ISO_R                "ISO_RATIO"
#define CAL_STATUS           "CAL_STATUS"
#define CAL_COMMENT          "CAL_COMMENT"

void c2f(char *str, int len);
void str_init(char *str, int len);

void getcal_new(char *fname, double *noisf, double *lincf,
  double *offcf, double *abscf, double *noispro, double *noispvs,
  double *noisdel, double *rngres, double *azres, double *rngpslr,
  double *azpslr, double *rngislr, double *azislr, double *rngamb,
  double *azamb, double *orierr, double *skerr, double *cserr, double *aserr,
  double *clerr, double *alerr, double *isor, double *relra, int *status,
  int *pvs_obj_stat, int *cal_fac_stat, int *noise_est_stat, 
  int *im_qual_stat, int *geo_anal_stat, char *calstat, 
  char *calcom, int *istatus)

{
  struct stat stbuf;
  char *buf;
  ODL odl,*bodyODL,*objODL,*dummyODL,msg;
  int fd, nb, ierr;
  char filename_c[60];
  int i,j,k;
  double *rel_radio_acc;
  double *noise_fact;
  double *lin_conv_fact;
  double *offset_conv_fact;
  double *abs_coeff;
  double *noise_pro;
  double *noise_pvs;
  double *noise_del;
  double   *range_res;
  double   *azimuth_res;
  double   *range_pslr;
  double   *azimuth_pslr;
  double   *range_islr;
  double   *azimuth_islr;
  double   *rng_ambig;
  double   *azi_ambig;
  double   *ori_err;
  double   *skew_err;
  double   *cross_s_err;
  double   *along_s_err;
  double   *cross_l_err;
  double   *along_l_err;
  double   *iso_ratio;
  char     *cal_stat;          /* 10-7-96 */
  char     *cal_com;           /* 10-7-96 */
  int      str_len;            /* 10-7-96 */
  int      found;              /* 10-16-96 */
/***************************************/


 *istatus = 0;

 F_to_C (filename_c,fname,60);


  if ((fd = open(filename_c, 0)) == -1) {
      printf ("can't open %s\n", filename_c);
      fflush(stdout);
      ierr=2;
      *istatus = ierr;
      return;
  }
  if (fstat(fd, &stbuf) == -1) {
      printf("can't stat %s\n", filename_c);
      fflush(stdout);
      ierr=2;
      *istatus = ierr;
      return;
  }
  if ((buf = (char *) malloc(stbuf.st_size)) == NULL) {
      printf("can't malloc %s\n", filename_c);
      fflush(stdout);
      ierr=5;
      *istatus = ierr;
      return;
  }

  if (read(fd, buf, stbuf.st_size) != stbuf.st_size) {
      printf("can't read %d\n", stbuf.st_size);
      fflush(stdout);
      ierr=4;
      *istatus = ierr;
      return;
  }
  close(fd);

  if ((msg= StrToODL(buf, stbuf.st_size)) == NULL) {
      printf("can't parse string\n");
      fflush(stdout);
      ierr=11;
      *istatus = ierr;
      return;
  }
  free(buf);



    if ((bodyODL = (ODL*) Val(Lookup(msg,DETAILED_METADATA ))) == NULL)
    { 
     ODLFree(odl);
      ierr=12;
      *istatus = ierr;
      return;
     }

    for (j = 0; bodyODL[j] != NULL; j++)  {
      /* find PVS_OBJ */
      if (strcasecmp(Name(bodyODL[j]),PVS_OBJ) ||
          (objODL = (ODL*) Val(bodyODL[j])) == NULL)
        continue;
      }

         /* find REL_RADIO_ACC */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), REL_RAD_ACC) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_REL_RADIO_ACC\n");
            *relra = -99.0;
         }
         else {
           k--;
           rel_radio_acc = (double*) Val(objODL[k]);
           *relra  = *rel_radio_acc;
         }


         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), CAL_STATUS) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_CAL_STATUS\n");
            str_len = 20;
            str_init(calstat,str_len);
            strcpy(calstat,"   ");
            c2f(calstat,str_len);
         }
         else {
           k--;
           cal_stat = (char*) Val(objODL[k]);
           str_len = 20;
           str_init(calstat,str_len);
           strcpy(calstat,cal_stat);
           c2f(calstat,str_len);
         }

         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), CAL_COMMENT) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_CAL_COMMENT\n");
           str_len = 204;
           str_init(calcom,str_len);
           strcpy(calcom,"   ");
           c2f(calcom,str_len);
         }
         else {
           k--;
           cal_com = (char*) Val(objODL[k]);
           str_len = 204;
           str_init(calcom,str_len);
           strcpy(calcom,cal_com);
           c2f(calcom,str_len);
         }

    for (j = 0; bodyODL[j] != NULL; j++)  {
      /* find CALIB_FAC */
      if (strcasecmp(Name(bodyODL[j]),CALIB_FAC) ||
          (objODL = (ODL*) Val(bodyODL[j])) == NULL)
        continue;
      }


         /* find NOISE_FACT */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), NOISE_FACT) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_NOISE_FACT\n");
            *noisf = -99.0;
         }
         else {
           k--;
           noise_fact = (double*) Val(objODL[k]);
           *noisf = *noise_fact;
         }

         /* find LINEAR_CONV_FACT */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), LIN_CONV_FACT) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_LIN_CONV_FACT\n");
            *lincf = -99.0;
         }
         else {
           k--;
           lin_conv_fact = (double*) Val(objODL[k]);
           *lincf = *lin_conv_fact;
         }

         /* find OFFSET_CONV_FACT */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), OFFSET_CONV_FACT) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_OFFSET_CONV_FACT\n");
            *offcf = -99.0;
         }
         else {
           k--;
           offset_conv_fact = (double*) Val(objODL[k]);
           *offcf = *offset_conv_fact;
         }

         /* find ABS_COEFF */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), IM_ABSCOEF) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_ABS_COEFF\n");
            *abscf = -99.0;
         }
         else {
           k--;
           abs_coeff = (double*) Val(objODL[k]);
           *abscf = *abs_coeff;
         }


    for (j = 0; bodyODL[j] != NULL; j++)  {
      /* find NOISE_EST */
      if (strcasecmp(Name(bodyODL[j]),NOISE_EST) ||
          (objODL = (ODL*) Val(bodyODL[j])) == NULL)
        continue;
      }


         /* find NT_CEOS_NOISE */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), NT_CEOS_NOISE) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_NT_CEOS_NOISE\n");
            *noispro = -99.0;
         }
         else {
           k--;
           noise_pro = (double*) Val(objODL[k]);
           *noispro = *noise_pro;
          }


         /* find NT_IMG_NOISE */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), NT_IMG_NOISE) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_NT_IMG_NOISE\n");
            *noispvs = -99.0;
         }
         else {
           k--;
           noise_pvs = (double*) Val(objODL[k]);
           *noispvs = *noise_pvs;
         }


         /* find NT_DELTA */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), NT_DELTA) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_NT_DELTA\n");
            *noisdel = -99.0;
         }
         else {
           k--;
           noise_del = (double*) Val(objODL[k]);
           *noisdel = *noise_del;
         }


    for (j = 0; bodyODL[j] != NULL; j++)  {
      /* find IM_QUALITY */
      if (strcasecmp(Name(bodyODL[j]),IM_QUALITY) ||
          (objODL = (ODL*) Val(bodyODL[j])) == NULL)
        continue;
      }

         /* find RANGE_RES */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), RANGE_RES) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_RANGE_RES\n");
            *rngres = -99.0;
         }
         else {
           k--;
           range_res = (double*) Val(objODL[k]);
           *rngres  = *range_res;
         }

         /* find AZIMUTH_RES */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), AZI_RES) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_AZIMUTH_RES\n");
            *azres = -99.0;
         }
         else {
           k--;
           azimuth_res = (double*) Val(objODL[k]);
          *azres = *azimuth_res;
         }

         /* find RANGE PSLR */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), CD_PSLR_RNG) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_RANGE PSLR\n");
            *rngpslr = -99.0;
         }
         else {
           k--;
           range_pslr = (double*) Val(objODL[k]);
           *rngpslr = *range_pslr;
         }

         /* find AZIMUTH PSLR */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), CD_PSLR_AZM) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_AZIMUTH PSLR\n");
            *azpslr = -99.0;
         }
         else {
           k--;
           azimuth_pslr = (double*) Val(objODL[k]);
           *azpslr = *azimuth_pslr;
         }

         /* find RANGE ISLR */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), CD_ISLR_RNG) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_RANGE ISLR\n");
            *rngislr = -99.0;
         }
         else {
           k--;
           range_islr = (double*) Val(objODL[k]);
           *rngislr = *range_islr;
         }

         /* find AZIMUTH ISLR */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), CD_ISLR_AZM) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_AZIMUTH ISLR\n");
            *azislr = -99.0;
         }
         else {
           k--;
           azimuth_islr = (double*) Val(objODL[k]);
           *azislr = *azimuth_islr;
         }

         /* find RANGE AMBIG */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), RNG_AMB) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_RANGE AMBIG\n");
            *rngamb = -99.0;
         }
         else {
           k--;
           rng_ambig = (double*) Val(objODL[k]);
           *rngamb = *rng_ambig;
         }

         /* find AZIMUTH AMBIG */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), AZI_AMB) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_AZIMUTH AMBIG\n");
            *azamb = -99.0;
         }
         else {
           k--;
           azi_ambig = (double*) Val(objODL[k]);
           *azamb = *azi_ambig;
         }


    for (j = 0; bodyODL[j] != NULL; j++)  {
      /* find GEO_ANALYSIS */
      if (strcasecmp(Name(bodyODL[j]),GEO_ANAL) ||
          (objODL = (ODL*) Val(bodyODL[j])) == NULL)
        continue;
      }

         /* find ORI_ERR */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), ORI_ERR) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found)  {
            printf(" CANT_FIND_ORI_ERR\n");
            *orierr = -99.0;
         }
         else {
           k--;
           ori_err = (double*) Val(objODL[k]);
           *orierr  = *ori_err;
         }

         /* find SKEW_ERR */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), DIS_SKEW) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_SKEW_ERR\n");
            *skerr = -99.0;
         }
         else {
           k--;
           skew_err = (double*) Val(objODL[k]);
           *skerr = *skew_err;
         }

         /* find CROSS_S_ERR */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), CRT_SCALE) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found)  {
            printf(" CANT_FIND_CROSS_S_ERR\n");
            *cserr = -99.0;
         }
         else {
           k--;
           cross_s_err = (double*) Val(objODL[k]);
           *cserr = *cross_s_err;
         }

         /* find ALONG_S_ERR */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), ALT_SCALE) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_ALONG_S_ERR\n");
            *aserr = -99.0;
         }
         else {
           k--;
           along_s_err = (double*) Val(objODL[k]);
           *aserr = *along_s_err;
         }

         /* find CROSS_L_ERR */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), CRT_LOCERR) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_CROSS_L_ERR\n");
            *clerr = -99.0;
         }
         else {
           k--;
           cross_l_err = (double*) Val(objODL[k]);
           *clerr = *cross_l_err;
         }

         /* find ALONG_L_ERR */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), ALT_LOCERR) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_ALONG_L_ERR\n");
            *alerr = -99.0;
         }
         else {
           k--;
           along_l_err = (double*) Val(objODL[k]);
           *alerr = *along_l_err;
         }

         /* find ISO_RATIO */
         found = 0;
         for (k=0; objODL[k]!=NULL && !found; ++k) {
            if (strcasecmp(Name(objODL[k]), ISO_R) ||
              (dummyODL = (ODL*) Val(objODL[k])) == NULL)
               continue;
            else
              found = 1;
         }
         if (!found) {
            printf(" CANT_FIND_ISO_RATIO\n");
            *isor = -99.0;
         }
         else {
           k--;
           iso_ratio = (double*) Val(objODL[k]);
           *isor = *iso_ratio;
         }

   *istatus =iok;

 return;
}

int F_to_C (str1,str2,len)
char *str1;
char *str2;
int len;
{
  int i;
  char *dummy_char;

  dummy_char= (char *)malloc(sizeof(char)*(len+1));

  for (i=0;i<len;i++)
  {
    if (*(str2 + i) == ' ')
    {
     *(dummy_char +i)= '\0';
     break;
    }
  *(dummy_char + i) = *(str2 +i);
  }

   *(dummy_char + i)= '\0';


  strcpy(str1,dummy_char);



 free(dummy_char);


return(1);
}


 #include <stdarg.h> 
 #include <stdio.h> 
 #include "odl.h"  

#define BODY                     "SCAN_RESULTS_FILE.BODY"
#define SCAN_RESULTS_FILE        "SCAN_RESULTS_FILE"
#define SEGMENT                  "SEGMENT"
#define FRAME                    "FRAME"
#define SCAN_FRAME_ID            "FRAME_ID"
#define SCENE_CENTER_LAT         "CENTER_LAT"
#define SCENE_CENTER_LON         "CENTER_LON"
#define SCENE_CENTER_TIME        "CENTER_TIME"
#define START_ADDR               "START_ADDRESS"
#define END_ADDR                 "END_ADDRESS"
#define SCAN_START_TIME          "START_TIME"
#define SCAN_END_TIME            "END_TIME"
#define NEAR_START_LAT           "NEAR_START_LAT"
#define NEAR_START_LON           "NEAR_START_LON"
#define NEAR_END_LAT             "NEAR_END_LAT"
#define NEAR_END_LON             "NEAR_END_LON"
#define FAR_START_LAT            "FAR_START_LAT"
#define FAR_START_LON            "FAR_START_LON"
#define FAR_END_LAT              "FAR_END_LAT"
#define FAR_END_LON              "FAR_END_LON"
#define ASC_DES                  "ASC_DESC"
#define STATE_VECTOR_RECORD      "STATE_VECTOR_RECORD"
#define STATE_VECTOR_METADATA    "STATE_VECTOR_METADATA"
#define STATE_VECTOR_DATA        "STATE_VECTOR_DATA"
#define STATE_VECTOR_PREC        "STATE_VECTOR_PRECISION"
#define X_POSITION               "X_POSITION"
#define Y_POSITION               "Y_POSITION"
#define Z_POSITION               "Z_POSITION"
#define X_VELOCITY               "X_VELOCITY"
#define Y_VELOCITY               "Y_VELOCITY"
#define Z_VELOCITY               "Z_VELOCITY"
#define SV_TIME                  "TIME"
#define SCAN_JOB_ID              "JOB_ID"
#define SCAN_PLATFORM            "PLATFORM"
#define SCAN_SENSOR              "SENSOR"
#define SCAN_REVOLUTION          "REVOLUTION"
#define SCAN_SEQUENCE            "SEQUENCE"
#define ACT_ID                   "ACTIVITY_ID"
#define SCAN_MEDIA_ID            "MEDIA_ID"
#define SCAN_MEDIA_TYPE          "MEDIA_TYPE"
#define SCAN_MEDIA_LOC           "MEDIA_LOCATION"
#define SCAN_RECORDER_ID         "RECORDER_ID"
#define SCAN_STATION_ID          "STATION_ID"
#define SCAN_FRAME_MODE          "FRAME_MODE"
#define SCAN_SITE_NAME           "SITE_NAME"
#define PRE_CAL1_POW             "PRE_CAL1_POW"
#define PRE_CAL2_POW             "PRE_CAL2_POW"
#define POST_CAL1_POW            "POST_CAL1_POW"
#define POST_CAL2_POW            "POST_CAL2_POW"

void c2f(char *str, int len);
void str_init(char *str, int len);

void getscandata(int *inp_frame_id, char *fname, int *status, 
   int *seg_id,
   int *styr, int *stdoy, int *sthr, int *stmin, int *stsec, int *stusec,
   int *edyr, int *eddoy, int *edhr, int *edmin, int *edsec, int *edusec,
   int *start_addr, int *end_addr,
   char *ascdes, double *pre_cal1, double *pre_cal2, double *post_cal1,
   double * post_cal2, char *vers, int *istatus)
   
{

/*  Following removed from arg list 2-28-96 JMS */
   int *job_id;
   char *platform; 
   char *sensor; 
   int *rev;
   int *sequence; 
   char *act_id;
   char *media_id; 
   char *media_type; 
   char *media_loc; 
   char *recorder_id;
   char *station_id; 
   char *frame_mode; 
   char *site_name;
   int *svyr; 
   int *svdoy; 
   int *svhr; 
   int *svmin; 
   int *svsec; 
   int *svusec;
   int *scyr; 
   int *scdoy; 
   int *schr; 
   int *scmin; 
   int *scsec; 
   int *scusec;
   double *xp, *yp, *zp, *xv, *yv, *zv;
   double *nslat, *nslon, *nelat, *nelon, *fslat;
   double *fslon, *felat, *felon, *sclat, *sclon;
   char *sv_precision;
/* End of values removed from arg list 2-28-96  */

   int      fd, i, j, k, l, m, *frame_id, err;
   int      segment_id = 0;
   int      done, found, frameN, segmentN;
   ODL      odl, *bodyODL, *segmtODL, *imageODL, *vectorODL, *vdataODL;
   ODL      *vmdataODL;
   int      str_len;

   char     errstr[256]; 
   double   *scene_cnt_lat;
   double   *scene_cnt_lon;
   GMT_t    *scene_cnt_time;
   GMT_t    *start_time;
   GMT_t    *end_time;
   GMT_t    *sv_time;
   int      *st_addr;
   int      *ed_addr;
   double   *near_start_lat;
   double   *near_start_lon;
   double   *near_end_lat;
   double   *near_end_lon;
   double   *far_start_lat;
   double   *far_start_lon;
   double   *far_end_lat;
   double   *far_end_lon;
   double   *x_pos;
   double   *y_pos;
   double   *z_pos;
   double   *x_vel;
   double   *y_vel;
   double   *z_vel; 
   char     *asc_des;
   int      *jbid;
   char     *pltfrm;
   char     *snsr;
   int      *rv;
   int      *seq;
   char     *actid;
   char     *medid;
   char     *medtyp;
   char     *medloc;
   char     *recid;
   char     *staid;
   char     *fmode;
   char     *sname;
   char     *svprec;
   double   *precal1;
   double   *precal2;
   double   *postcal1;
   double   *postcal2;

   FILE *fp;

/*  Following added JMS 9-10-96 to get scanner version number */
   char buff[300];        
   char str_c[60];
   char *sss;

   str_len = 64;
   *istatus = 0;

   done = 0;
   for (i=0; i < 60 && !done; i++)
     {
     j = 59 - i;
     if (fname[j] == ' ')
       fname[j] = '\0';
     else
       done = 1;
     }

   printf ("SCAN RESULTS %s\n",fname);

   fp = fopen(fname, "r");
   if (fp == NULL) 
    {
    *istatus = ierr_2;
     printclog(3,"CANT OPEN SCAN RESULTS FILE");
     printf("CANT OPEN SCAN RESULTS FILE\n");
     return;
    }

/*  Following added JMS 9-10-96 to get scanner version number */
   fgets(buff,300,fp);
   sss = (char *) strstr(buff,"RDS");
   sscanf(&sss[3],"%s",str_c);
   strcpy(vers,str_c);

   printf (" Starting getscandata\n");

   /*** R11634501SS1ND000.S ***/
   if ((odl = ODLparse(fname, 0, errstr)) == NULL) {
      printclog(3,"IN SCAN RESULTS, CANT OPEN FRAME");
      *istatus = ierr_11;
      return;
   }

   if ((bodyODL = (ODL*) Val(Lookup(odl, BODY))) == NULL) {
     printclog(3," CANT_FIND_FRAME_BODY");
     ODLFree(odl);
     *istatus = ierr_12;
     return;
     }

    printf("Found BODY\n");
         for (i=0; bodyODL[i]!=NULL; ++i) {
         printf (" bodyODL %d %s\n",i,Name(bodyODL[i]));
         }



         /* find PRE_CAL1_POW from BODY */
         for (i=0; bodyODL[i]!=NULL; ++i) {
            if (strcasecmp(Name(bodyODL[i]), PRE_CAL1_POW) == 0)
               break;
         }
         if (bodyODL[i] == NULL) {
            printf(" CANT_FIND_PRE_CAL1_POW\n");
            ODLFree(odl);
            *istatus = ierr_12;
            return;
         }
         precal1 = (double*) Val(bodyODL[i]);
         *pre_cal1 = *precal1;


         /* find PRE_CAL2_POWfrom BODY */
         for (i=0; bodyODL[i]!=NULL; ++i) {
            if (strcasecmp(Name(bodyODL[i]), PRE_CAL2_POW) == 0)
               break;
         }
         if (bodyODL[i] == NULL) {
            printf(" CANT_FIND_PRE_CAL2_POW\n");
            ODLFree(odl);
            *istatus = ierr_12;
            return;
         }
         precal2 = (double*) Val(bodyODL[i]);
         *pre_cal2 = *precal2;


         /* find POST_CAL1_POWfrom BODY */
         for (i=0; bodyODL[i]!=NULL; ++i) {
            if (strcasecmp(Name(bodyODL[i]), POST_CAL1_POW) == 0)
               break;
         }
         if (bodyODL[i] == NULL) {
            printf(" CANT_FIND_POST_CAL1_POW\n");
            ODLFree(odl);
            *istatus = ierr_12;
            return;
         }
         postcal1 = (double*) Val(bodyODL[i]);
         *post_cal1 = *postcal1;


         /* find POST_CAL2_POWfrom BODY */
         for (i=0; bodyODL[i]!=NULL; ++i) {
            if (strcasecmp(Name(bodyODL[i]), POST_CAL2_POW) == 0)
               break;
         }
         if (bodyODL[i] == NULL) {
            printf(" CANT_FIND_POST_CAL2_POW\n");
            ODLFree(odl);
            *istatus = ierr_12;
            return;
         }
         postcal2 = (double*) Val(bodyODL[i]);
         *post_cal2 = *postcal2;




   printf ("Looking for SEGMENT\n");


   found = 0;
   for (i=0; bodyODL[i]!=NULL && !found; ++i) {
      /* find SEGMENT */
      if (strcasecmp(Name(bodyODL[i]), SEGMENT) ||
          (segmtODL = (ODL*) Val(bodyODL[i])) == NULL) {
         continue;
      }
      segment_id++;
      printf ("Found SEGMENT\n");
      printf ("Looking for FRAME\n");
      for (j=0; segmtODL[j]!=NULL && !found; j++) {
         /* find FRAME */
         if (strcasecmp(Name(segmtODL[j]), FRAME) ||
             (imageODL = (ODL*) Val(segmtODL[j])) == NULL)
            continue;
         else
            frameN++;
            printf ("Found FRAME\n");

         /* find FRAME_ID from FRAME */
         for (k=0; imageODL[k]!=NULL; ++k) {
            if (strcasecmp(Name(imageODL[k]), SCAN_FRAME_ID) == 0)
               break;
         }
         if (imageODL[k] == NULL) {
            printclog(3," CANT_FIND_FRAME_ID");
            ODLFree(odl);
            *istatus = ierr_12;
            return;
         }
         printf("Found frame id\n");
         frame_id = (int*) Val(imageODL[k]);
         if (*frame_id  == *inp_frame_id)
           { 
           found = 1;
           *seg_id = segment_id;


         for (k=0; imageODL[k]!=NULL; k++) {
           /* find STATE_VECTOR_RECORD */
           if (strcasecmp(Name(imageODL[k]), STATE_VECTOR_RECORD) ||
               (vectorODL = (ODL*) Val(imageODL[k])) == NULL)
              continue;
           /* find STATE_VECTOR_METADATA from STATE_VECTOR_RECORD */
           for (l=0; vectorODL[l]!=NULL; ++l) {
              if (strcasecmp(Name(vectorODL[l]), STATE_VECTOR_METADATA) ||
                  (vmdataODL = (ODL*) Val(vectorODL[l])) == NULL)
                 continue;
           }


           /* find STATE_VECTOR_DATA from STATE_VECTOR_RECORD */
           for (l=0; vectorODL[l]!=NULL; ++l) {
              if (strcasecmp(Name(vectorODL[l]), STATE_VECTOR_DATA) ||
                  (vdataODL = (ODL*) Val(vectorODL[l])) == NULL)
                 continue;
           }




 
             
           }



 
           /* find ASCENDING_DESCENDING_FLAG from FRAME */
             for (k=0; imageODL[k]!=NULL; ++k) {
               if (strcasecmp(Name(imageODL[k]), ASC_DES) == 0)
                 break;
             }
             if (imageODL[k] == NULL) {
               printclog(3," CANT_FIND_ASCENDING_DESCENDING_FLAG");
               ODLFree(odl);
               *istatus = ierr_12;
               return;
             }
             asc_des = (char*) Val(imageODL[k]);
             printf("In getscandata, ascdes = %s\n",asc_des);
             str_len = 2;
             str_init(ascdes,str_len);
             strcpy(ascdes,asc_des);
             c2f(ascdes,str_len);
 
           /* find START_ADDRESS from FRAME */
             for (k=0; imageODL[k]!=NULL; ++k) {
               if (strcasecmp(Name(imageODL[k]), START_ADDR) == 0)
                  break;
             }
             if (imageODL[k] == NULL) {
               printclog(3," CANT_FIND_START_ADDRESS");
               ODLFree(odl);
               *istatus = ierr_12;
               return;
             }
             st_addr = (int*) Val(imageODL[k]);
             *start_addr = *st_addr;
 
             
           /* find END_ADDRESS from FRAME */
             for (k=0; imageODL[k]!=NULL; ++k) {
               if (strcasecmp(Name(imageODL[k]), END_ADDR) == 0)
                  break;
             }
             if (imageODL[k] == NULL) {
               printclog(3," CANT_FIND_END_ADDRESS");
               ODLFree(odl);
               *istatus = ierr_12;
               return;
             }
             ed_addr = (int*) Val(imageODL[k]);
             *end_addr = *ed_addr;
 
             
           /* find START_TIME from FRAME */
             for (k=0; imageODL[k]!=NULL; ++k) {
               if (strcasecmp(Name(imageODL[k]), SCAN_START_TIME) == 0)
                  break;
             }
             if (imageODL[k] == NULL) {
               printclog(3," CANT_FIND_START_TIME");
               ODLFree(odl);
               *istatus = ierr_12;
               return;
             }
             start_time = (GMT_t*) Val(imageODL[k]);
             *styr = start_time->tm.tm_year;
             *stdoy = start_time->tm.tm_yday;
             *sthr = start_time->tm.tm_hour;
             *stmin = start_time->tm.tm_min;
             *stsec = start_time->tm.tm_sec;
             *stusec = start_time->tv_usec;
 
             
           /* find END_TIME from FRAME */
             for (k=0; imageODL[k]!=NULL; ++k) {
               if (strcasecmp(Name(imageODL[k]), SCAN_END_TIME) == 0)
                  break;
             }
             if (imageODL[k] == NULL) {
               printclog(3," CANT_FIND_END_TIME");
               ODLFree(odl);
               *istatus = ierr_12;
               return;
             }
             end_time = (GMT_t*) Val(imageODL[k]);
             *edyr = end_time->tm.tm_year;
             *eddoy = end_time->tm.tm_yday;
             *edhr = end_time->tm.tm_hour;
             *edmin = end_time->tm.tm_min;
             *edsec = end_time->tm.tm_sec;
             *edusec = end_time->tv_usec;
 
           }
      }            /*  End of for j loop  */
   }               /*  End of for i loop  */

   printf("Leaving getscandata\n");
   ODLFree(odl);
   *status = 0;
   *istatus =iok ;
   return;

} /* end getScanData................*/




void c2f(char *str, int len)
{
int i;
   for (i=0; i < len; i++)
     {
     if (str[i] == '\0')
       str[i] = ' ';
     }
}


void str_init(char *str, int len)
{
int i;
   for (i=0; i < len; i++)
       str[i] = ' ';
}

#include <time.h>
const time_t *Clock;

void ibm_gmtime(int *stime, int tarray[9])
{
struct tm *gtime;

  gtime=gmtime(stime);
  tarray[0] = gtime->tm_sec;
  tarray[1] = gtime->tm_min;
  tarray[2] = gtime->tm_hour;
  tarray[3] = gtime->tm_mday;
  tarray[4] = gtime->tm_mon;
  tarray[5] = gtime->tm_year;
  tarray[6] = gtime->tm_wday;
  tarray[7] = gtime->tm_yday;
  tarray[8] = gtime->tm_isdst;

}


