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
#include <sys/types.h>
#include <netinet/in.h>

static char sccsid_process_ldr_c[] = "@(#)process_ldr.c	1.17 96/10/04 14:42:45";

extern SARL_ptr *Global_tree;

extern int Debug;

extern unsigned char* work;
int Record_length, Current_length, COM_entry;
int FDR_entry, DSS_entry, MP_entry, PP_entry, AT_entry;
int RD_entry, RC_entry, DQS_entry, RS_entry;
int DE_entry, DP_entry, CD_entry, FR_entry;

int process_from_ldr( void ) {

   FILE *in;
   SARL_ptr* tree;
   int ret;
   
   /* open the leader file */
   if ( (in = Open_LDR(CEOS.infile, READ_ONLY) ) == NULL) {
      printf("\n Could not open the leader file for reading --- %s",CEOS.infile);
      return(0);
   }
 
   /* allocate memory for a SARL structure */

   if ( (tree = Allocate_SARL() ) == NULL) {
      printf("\n Failed to Allocate_SARL");
      return(0);
   }

   SetCurrentSARL( tree );

   /* read the File Descriptor Record, transfer to "descript" structure */

   if ( !Read_FDR_SARL( in, tree ) ) {
      printf("\n Error in Read_FDR_SARL\n");
      return(0);
   }

   /* based on the contents of the leader file FDR, allocate space for 
      each of record */

   if ( Allocate_SARL_ODR(tree) ) {
      ret=Read_ALL_SARL( in, tree ); 
      switch (ret) {
         case (END_OF_FILE) :
            break;
         default :
            printf("\n Aborting the read of the input file\n");
            break;
      }
   }
   
   fclose(in);

   return(1);
}

void CEOS_to_LDR(void) {
   Write_CEOS_SARL( CEOS.outfile,  GetCurrentSARL());
}

int Write_CEOS_SARL( char name[], SARL_ptr* tree)
{
   FILE* out;
   int ret, len;

   /* open the output CEOS file */
   len=strlen(name);
   if (len) {
      if (( out = Open_LDR(name, WRITE_ONLY) ) == NULL) {
         printf("\n Could not open the leader file for writing --- %s",name);
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
      ret=MP_to_LDR( Get_L_MP(tree), out, 0 );
      ret=PP_to_LDR( Get_L_PP(tree), out, 0 );
      ret=AT_to_LDR( Get_L_AT(tree), out, 0 );
      ret=RD_to_LDR( Get_L_RD(tree), out, 0 );
      ret=RC_to_LDR( Get_L_RC(tree), out, 0 );
      ret=DQS_to_LDR( Get_L_DQS(tree), out, 0 );
      ret=DH_to_LDR( Get_L_DH(tree), out, 0 );
      ret=RS_to_LDR( Get_L_RS(tree), out, 0 );
      ret=DE_to_LDR( Get_L_DE(tree), out, 0 );
      ret=DP_to_LDR( Get_L_DP(tree), out, 0 );
      ret=CD_to_LDR( Get_L_CD(tree), out, 0 );
      ret=FR_to_LDR( Get_L_FR(tree), out, 0 );
      if (out != (FILE *) NULL ) fclose( out );
   }
   return(TRUE);
}


Sarl_Desc_Rec* Get_L_FDR( SARL_ptr* t )
{
   if (t != NULL ) {
      return( &(t->descript) );
   }
   else 
      return( (Sarl_Desc_Rec*) NULL);
}

int FDR_to_LDR( Sarl_Desc_Rec* t, FILE* out ) 
{
   SARL_ptr *current;
   unsigned char* buf = work;
   unsigned char* tb;
   desc_rec *d = &(t->desc);
   int nitems;
   int i, off;
   int Zer0 = 0;
   unsigned int *lb;

   lb = (unsigned int *) buf;

   /* Write the contents of the FDR structure of an ODL tree to a CEOS leader file */

   if (t == (Sarl_Desc_Rec*) NULL) return(FALSE);

      nitems=t->desc.length;
      if (nitems!=720) {
         nitems=720;
         t->desc.length=720;
      }
      i=d->rec_seq;
      zero_set(buf,nitems);

      if ( (current=GetCurrentSARL()) != NULL )
         Check_Rec_Seq( &i, buf, current, 0);
      else
         *lb = htonl( i );
      put_byte((buf+4), d->rec_sub1);
      put_byte((buf+5), d->rec_type);
      put_byte((buf+6), d->rec_sub2);
      put_byte((buf+7), d->rec_sub3);
      tb=&buf[4];
      if ( !MATCH_RECORD(tb, LD1SUB, LDTYPE, LD2SUB, LD3SUB) ) {
         printf("\n Descriptor record and subtype mismatch - expecting FDR codes");
      }
      lb[2] = htonl( nitems );
      put_chars((char *) (buf+12), t->ascii_flag, 2);
      put_blanks((char *) (buf+14), 2);         /* spare */
      put_chars((char *) (buf+16), t->format_doc, 12);
      put_chars((char *) (buf+28), t->format_rev, 2);
      put_chars((char *) (buf+30), t->design_rev, 2);
      put_chars((char *) (buf+32), t->software_id, 12);
      put_I4(t->file_num, "%4d", 4, (buf+44));
      put_chars((char *) (buf+48), t->product_id, 16);
      put_chars((char *) (buf+64), t->rec_seq_flag, 4);
      put_I4(t->seq_loc, "%8d", 8, (buf+68));
      put_I4(t->seq_len, "%4d", 4, (buf+76));
      put_chars((char *) (buf+80), t->rec_code, 4);
      put_I4(t->code_loc, "%8d", 8, (buf+84));
      put_I4(t->code_len, "%4d", 4, (buf+92));
      put_chars((char *) (buf+96), t->rec_len, 4);
      put_I4(t->rlen_loc, "%8d", 8, (buf+100));
      put_I4(t->rlen_len, "%4d", 4, (buf+108));
      put_blanks( (char *) (buf+112), 68 );       /* spares */
      put_I4(t->n_dataset, "%6d", 6, (buf+180));
      put_I4(t->l_dataset, "%6d", 6, (buf+186));
      put_I4(t->n_map_proj, "%6d", 6, (buf+192));
      put_I4(t->l_map_proj, "%6d", 6, (buf+198));
      put_I4(t->n_plat_pos, "%6d", 6, (buf+204));
      put_I4(t->l_plat_pos, "%6d", 6, (buf+210));
      put_I4(t->n_att_data, "%6d", 6, (buf+216));
      put_I4(t->l_att_data, "%6d", 6, (buf+222));
      put_I4(t->n_radi_data, "%6d", 6, (buf+228));
      put_I4(t->l_radi_data, "%6d", 6, (buf+234));
      put_I4(t->n_radi_comp, "%6d", 6, (buf+240));
      put_I4(t->l_radi_comp, "%6d", 6, (buf+246));
      put_I4(t->n_qual_sum, "%6d", 6, (buf+252));
      put_I4(t->l_qual_sum, "%6d", 6, (buf+258));
      put_I4(t->n_data_hist, "%6d", 6, (buf+264));
      put_I4(t->l_data_hist, "%6d", 6, (buf+270));
      put_I4(t->n_rang_spec, "%6d", 6, (buf+276));
      put_I4(t->l_rang_spec, "%6d", 6, (buf+282));
      put_I4(t->n_dem_desc, "%6d", 6, (buf+288));
      put_I4(t->l_dem_desc, "%6d", 6, (buf+294));
      put_I4(t->n_radar_par, "%6d", 6, (buf+300));
      put_I4(t->l_radar_par, "%6d", 6, (buf+306));
      put_I4(t->n_anno_data, "%6d", 6, (buf+312));
      put_I4(t->l_anno_data, "%6d", 6, (buf+318));
      put_I4(t->n_det_proc, "%6d", 6, (buf+324));
      put_I4(t->l_det_proc, "%6d", 6, (buf+330));
      put_I4(t->n_cal, "%6d", 6, (buf+336));
      put_I4(t->l_cal, "%6d", 6, (buf+342));
      put_I4(t->n_gcp, "%6d", 6, (buf+348));
      put_I4(t->l_gcp, "%6d", 6, (buf+354));
      for (i=0, off=360; i<10;i++, off+=6) put_I4( Zer0, "%6d", 6, (buf+off));
      put_I4(t->n_fac_data, "%6d", 6, (buf+420));
      put_I4(t->l_fac_data, "%6d", 6, (buf+426));
      put_blanks( (char *) (buf+432), 288 );    /* spare */
      if (out != (FILE *) NULL ) 
         write_record( out, buf, sizeof(unsigned char),  nitems);

      if (Debug && !get_recalc_status()) {

         COM_entry = FDR_entry;
         printf("\n**************** begin of FDR record *******************\n");
         printf("\n RECORD SEQUENCE\t%ld",d->rec_seq);
         printf("\n RECORD SUB 1\t\t%d",d->rec_sub1);
         printf("\n RECORD TYPE\t\t%d",d->rec_type);
         printf("\n RECORD SUB 2\t\t%d",d->rec_sub2);
         printf("\n RECORD SUB 3\t\t%d",d->rec_sub3);
         printf("\n RECORD LENGTH\t\t%ld\n",d->length);
         printfs("\n ACSII FLAG\t\t%s",t->ascii_flag); 
         printfs("\n DOCUMENT FORMAT\t%s\t",t->format_doc);
         printfs("\n FORMAT REVISION\t%s\t",t->format_rev);
         printfs("\n RCRD FRMT REV LVL\t%s\t",t->design_rev);
         printfs("\n SOFTWARE ID\t\t%s",t->software_id);
         printfd("\n FILE NUMBER\t\t%d",t->file_num);
         printfs("\n PRODUCT ID\t\t%s",t->product_id);
         printfs("\n RCRD SEQ FLAG\t\t%s",t->rec_seq_flag);
         printfl("\n SEQ NUM LOC\t\t%ld",t->seq_loc); 
         printfd("\n SEQ NUM FLD LEN\t%d\t",t->seq_len);
         printfs("\n RCRD CODE FLAG\t\t%s",t->rec_code);
         printfl("\n RCRD CODE LOC\t\t%ld",t->code_loc);
         printfd("\n RCRD CODE FLD LEN\t%d\t",t->code_len);
         printfs("\n RCRD LEN FLAG\t\t%s",t->rec_len);
         printfl("\n RCRD LENGTH LOC\t%ld\t",t->rlen_loc);
         printfd("\n RCRD LEN FLD LEN\t%d\t",t->rlen_len);
         printfl("\n NUM DATA SUM RCRDS\t%ld\t",t->n_dataset);
         printfl("\n DSS RCRD LEN\t\t%ld",t->l_dataset);
         printfl("\n NUM MAP PROJ RCRDS\t%ld\t",t->n_map_proj);
         printfl("\n M P RCRD LEN\t\t%ld",t->l_map_proj);
         printfl("\n NUM PLAT POS RCRDS\t%ld\t",t->n_plat_pos);
         printfl("\n P P RCRD LEN\t\t%ld",t->l_plat_pos);
         printfl("\n NUM ATT DATA RCRDS\t%ld\t",t->n_att_data);
         printfl("\n A D RCRD LEN\t\t%ld",t->l_att_data);
         printfl("\n NUM RADI DATA RCRDS\t%ld\t",t->n_radi_data);
         printfl("\n R D RCRD LEN\t\t%ld",t->l_radi_data);
         printfl("\n NUM RADI COMP RCRDS\t%ld\t",t->n_radi_comp);
         printfl("\n R C RCRD LEN\t\t%ld",t->l_radi_comp);
         printfl("\n NUM QUAL SUM RCRDS\t%ld\t",t->n_qual_sum);
         printfl("\n Q S RCRD LEN\t\t%ld",t->l_qual_sum);
         printfl("\n NUM DATA HIST RCRDS\t%ld\t",t->n_data_hist);
         printfl("\n D H RCRD LEN\t\t%ld",t->l_data_hist);
         printfl("\n NUM RNG SPCTR RCRDS\t%ld\t",t->n_rang_spec);
         printfl("\n R S RCRD LEN\t\t%ld",t->l_rang_spec);
         printfl("\n NUM DGTL ELEV RCRDS\t%ld\t",t->n_dem_desc);
         printfl("\n D E RCRD LEN\t\t%ld",t->l_dem_desc);
         printfl("\n NUM RDR UPDT RCRDS\t%ld\t",t->n_radar_par);
         printfl("\n R P RCRD LEN\t\t%ld",t->l_radar_par);
         printfl("\n NUM ANNT DATA RCRDS\t%ld\t",t->n_anno_data);
         printfl("\n A D RCRD LEN\t\t%ld",t->l_anno_data);
         printfl("\n NUM DTLD PROC RCRDS\t%ld\t",t->n_det_proc);
         printfl("\n D P RCRD LEN\t\t%ld",t->l_det_proc);
         printfl("\n NUM CALB DATA RCRDS\t%ld\t",t->n_cal);
         printfl("\n C B RCRD LEN\t\t%ld",t->l_cal);
         printfl("\n NUM GND CTL RCRDS\t%ld\t",t->n_gcp);
         printfl("\n G C RCRD LEN\t\t%ld",t->l_gcp);
         printfl("\n NUM FACI DATA RCRDS\t%ld",t->n_fac_data);
         printfl("\n F D RCRD LEN\t\t%ld\t\t\n",t->l_fac_data);
         printf("\n**************** end of FDR record *******************\n");
      }
      return(TRUE);
}

Dataset_Sum* Get_L_DSS( SARL_ptr* t )
{
    if (t != NULL ) {
       return( t->data_sum );
    }
    else 
       return( (Dataset_Sum*) NULL);
}

Dataset_Sum* Get_DSS_Num( SARL_ptr* t, int num )
{
    Sarl_Desc_Rec* d;
    Dataset_Sum* ds;
    int i=1;

    if (t != NULL) {
       d = &(t->descript);
       if (d->n_dataset > 0 && num > 0 && num <= d->n_dataset) {
          ds = t->data_sum;
          while (i<num) { ds=ds->next; i++; }
       }
       return( ds );
    }
    else 
       return( (Dataset_Sum*) NULL);
}

int DSS_to_LDR( Dataset_Sum* ds, FILE* out, int mode ) 
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    desc_rec *d = &(ds->desc);
    int nitems;
    int i, off;
    unsigned int *lb;

    lb = (unsigned int *) buf;

    /* Write the contents of a Dataset Summary record stored in an ODL
       tree to a CEOS leader file */

    if (ds == (Dataset_Sum*) NULL) return(FALSE);

       /* allocate a temp buffer */
       nitems=ds->desc.length;

       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          DSS_to_LDR(ds, out, 1);
          ds->desc.length=get_rec_length();
          printf("\n Calculated record length for Dataset Summary %ld\n",ds->desc.length);
          nitems=ds->desc.length;
          reset_rec_length();
       }

       i=d->rec_seq;
       zero_set(buf,nitems);

       if ( (current=GetCurrentSARL()) != NULL ) 
          Check_Rec_Seq( &i, buf, current, 0);
       else
	    *lb = htonl( i );
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
	lb[2] = htonl( nitems );
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LS1SUB, LSTYPE, LS2SUB, LS3SUB) ) {
          printf("\n Descriptor record and subtype mismatch - expecting DSS codes");
       }
       put_I4(ds->seq_num, "%4d", 4, (buf+12));
       put_I4(ds->sar_chan, "%4d", 4, (buf+16));
       put_chars((char*) (buf+20), ds->product_id, 16);
       put_chars((char*) (buf+36), ds->scene_des, 32);
       put_chars((char*) (buf+68), ds->inp_sctim, 32);
       put_chars((char*) (buf+100), ds->asc_des, 16);
       put_F4(ds->pro_lat, "%16.7E", 16, (buf+116));
       put_F4(ds->pro_long, "%16.7E", 16, (buf+132));
       put_F4(ds->pro_head, "%16.7E", 16, (buf+148));
       put_chars((char*) (buf+164), ds->ellip_des, 16);
       put_F4(ds->ellip_maj, "%16.7E", 16, (buf+180));
       put_F4(ds->ellip_min, "%16.7E", 16, (buf+196));
       put_F4(ds->earth_mass, "%16.7E", 16, (buf+212));
       put_F4(ds->grav_const, "%16.7E", 16, (buf+228));
       put_F4(ds->ellip_j[0], "%16.7E", 16, (buf+244));
       put_F4(ds->ellip_j[1], "%16.7E", 16, (buf+260));
       put_F4(ds->ellip_j[2], "%16.7E", 16, (buf+276));
#ifdef PRE_RADARSAT
       put_F4(ds->terrain_h, "%16.7E", 16, (buf+292)); 
       put_I4(ds->sc_lin, "%8d", 8, (buf+308));
       put_I4(ds->sc_pix, "%8d", 8, (buf+324));
       put_F4(ds->scene_len, "%16.7E", 16, (buf+340));
       put_F4(ds->scene_wid, "%16.7E", 16, (buf+356));
       put_blanks( (char *) (buf+372), 8);     
       put_blanks( (char *) (buf+380), 8);      
#else
       put_blanks( (char *) (buf+292), 16);       /* spare */
       put_F4(ds->terrain_h, "%16.7E", 16, (buf+308)); 
       put_I4(ds->sc_lin, "%8d", 8, (buf+324));
       put_I4(ds->sc_pix, "%8d", 8, (buf+332));
       put_F4(ds->scene_len, "%16.7E", 16, (buf+340));
       put_F4(ds->scene_wid, "%16.7E", 16, (buf+356));
       put_blanks( (char *) (buf+372), 16);      /* spare */
#endif
       put_I4(ds->nchn, "%4d", 4, (buf+388));
       put_blanks( (char *) (buf+392), 4);       /* spare */
       put_chars((char*) (buf+396), ds->mission_id, 16);
       put_chars((char*) (buf+412), ds->sensor_id, 32);
       put_chars((char*) (buf+444), ds->revolution, 8);
       put_F4(ds->plat_lat, "%8.3f", 8, (buf+452));
       put_F4(ds->plat_long, "%8.3f", 8, (buf+460));
       put_F4(ds->plat_head_scene, "%8.3f", 8, (buf+468));
       put_F4(ds->clock_ang, "%8.3f", 8, (buf+476));
       put_F4(ds->incident_ang, "%8.3f", 8, (buf+484));
       put_F4(ds->frequency, "%8.3f", 8, (buf+492));
       put_F4(ds->wave_length, "%16.7f", 8, (buf+500));
       put_chars((char*) (buf+516), ds->motion_comp, 2);
       put_chars((char*) (buf+518), ds->pulse_code, 16);
       put_F4(ds->ampl_coef[0], "%16.7E", 16, (buf+534));
       put_F4(ds->ampl_coef[1], "%16.7E", 16, (buf+550));
       put_F4(ds->ampl_coef[2], "%16.7E", 16, (buf+566));
       put_F4(ds->ampl_coef[3], "%16.7E", 16, (buf+582));
       put_F4(ds->ampl_coef[4], "%16.7E", 16, (buf+598));
       put_F4(ds->phas_coef[0], "%16.7E", 16, (buf+614));
       put_F4(ds->phas_coef[1], "%16.7E", 16, (buf+630));
       put_F4(ds->phas_coef[2], "%16.7E", 16, (buf+646));
       put_F4(ds->phas_coef[3], "%16.7E", 16, (buf+662));
       put_F4(ds->phas_coef[4], "%16.7E", 16, (buf+678));
       put_I4(ds->chirp_ext_ind, "%8d", 8, (buf+694));
       put_blanks( (char *) (buf+702), 8);        /* spare */
       put_F4(ds->rng_samp_rate, "%16.7f", 16, (buf+710));
       put_F4(ds->rng_gate, "%16.7f", 16, (buf+726));
       put_F4(ds->rng_length, "%16.7f", 16, (buf+742));
       put_chars((char*) (buf+758), ds->baseband_f, 4);
       put_chars((char*) (buf+762), ds->rngcmp_f, 4);
       put_F4(ds->gn_polar, "%16.7f", 16, (buf+766));
       put_F4(ds->gn_cross, "%16.7f", 16, (buf+782));
       put_I4(ds->chn_bits, "%8d", 8, (buf+798));
       put_chars((char*) (buf+806), ds->quant_desc, 12);
       put_F4(ds->i_bias, "%16.7f", 16, (buf+818));
       put_F4(ds->q_bias, "%16.7f", 16, (buf+834));
       put_F4(ds->iq_ratio, "%16.7f", 16, (buf+850));
       put_F4(ds->spare_dss_7, "%16.7f", 16, (buf+866));
       put_F4(ds->spare_dss_8, "%16.7f", 16, (buf+882));
       put_F4(ds->ele_sight, "%16.7f", 16, (buf+898));
       put_F4(ds->mech_sight, "%16.7f", 16, (buf+914));
       put_chars((char*) (buf+930), ds->echo_track, 4);
       put_F4(ds->prf, "%16.7f", 16, (buf+934));
       put_F4(ds->elev_beam, "%16.7f", 16, (buf+950));
       put_F4(ds->azi_beam, "%16.7f", 16, (buf+966));
       put_chars( (char*) (buf+982), ds->sat_bintim, 16);
       put_chars((char*) (buf+998), ds->sat_clktim, 32);
       put_I4(ds->sat_clkinc, "%8d", 8, (buf+1030));
       put_blanks( (char *) (buf+1038), 8);  /* spare */
       put_chars((char*) (buf+1046), ds->fac_id, 16);
       put_chars((char*) (buf+1062), ds->sys_id, 8);
       put_chars((char*) (buf+1070), ds->ver_id, 8);
       put_chars((char*) (buf+1078), ds->fac_code, 16);
       put_chars((char*) (buf+1094), ds->lev_code, 16);
       put_chars((char*) (buf+1110), ds->product_type, 32);
       put_chars((char*) (buf+1142), ds->algor_id, 32);
       put_F4(ds->n_azilok, "%16.7f", 16, (buf+1174));
       put_F4(ds->n_rnglok, "%16.7f", 16, (buf+1190));
       put_F4(ds->bnd_azilok, "%16.7f", 16, (buf+1206));
       put_F4(ds->bnd_rnglok, "%16.7f", 16, (buf+1222));
       put_F4(ds->bnd_azi, "%16.7f", 16, (buf+1238));
       put_F4(ds->bnd_rng, "%16.7f", 16, (buf+1254));
       put_chars((char*) (buf+1270), ds->azi_weight, 32);
       put_chars((char*) (buf+1302), ds->rng_weight, 32);
       put_chars((char*) (buf+1334), ds->data_inpsrc, 16);
       put_F4(ds->rng_res, "%16.7f", 16, (buf+1350));
       put_F4(ds->azi_res, "%16.7f", 16, (buf+1366));
       put_F4(ds->radi_stretch[0], "%16.7f", 16, (buf+1382));
       put_F4(ds->radi_stretch[1], "%16.7f", 16, (buf+1398));
       put_F4(ds->alt_dopcen[0], "%16.7f", 16, (buf+1414));
       put_F4(ds->alt_dopcen[1], "%16.7f", 16, (buf+1430));
       put_F4(ds->alt_dopcen[2], "%16.7f", 16, (buf+1446));
       put_blanks( (char *) (buf+1462), 16);    /* spare */
       put_F4(ds->crt_dopcen[0], "%16.7f", 16, (buf+1478));
       put_F4(ds->crt_dopcen[1], "%16.7f", 16, (buf+1494));
       put_F4(ds->crt_dopcen[2], "%16.7f", 16, (buf+1510));
       put_chars((char*) (buf+1526), ds->time_dir_pix, 8);
       put_chars((char*) (buf+1534), ds->time_dir_lin, 8);
       put_F4(ds->alt_rate[0], "%16.7f", 16, (buf+1542));
       put_F4(ds->alt_rate[1], "%16.7f", 16, (buf+1558));
       put_F4(ds->alt_rate[2], "%16.7f", 16, (buf+1574));
       put_blanks( (char *) (buf+1590), 16);     /* spare */
       put_F4(ds->crt_rate[0], "%16.7f", 16, (buf+1606));
       put_F4(ds->crt_rate[1], "%16.7f", 16, (buf+1622));
       put_F4(ds->crt_rate[2], "%16.7f", 16, (buf+1638));
       put_blanks( (char *) (buf+1654), 16);     /* spare */
       put_chars((char*) (buf+1670), ds->line_cont, 8);
       put_chars((char*) (buf+1678), ds->clutterlock_flg, 4);
       put_chars((char*) (buf+1682),ds->auto_focus,  4);
       put_F4(ds->line_spacing, "%16.7f", 16, (buf+1686));
       put_F4(ds->pixel_spacing, "%16.7f", 16, (buf+1702));
       put_chars((char*) (buf+1718), ds->rngcmp_desg, 16);
#ifdef PRE_RADARSAT
       put_blanks((char*) (buf+1734),272);   /* spare */
       put_I4(ds->annot_pts, "%8d", 8, (buf+2006));
       if (ds->annot_pts>0) {
          put_blanks( (char*) (buf+2014), 8);
          for (i=0, off=2022; i< ds->annot_pts; i++) {
              put_I4( ds->annot_line[i], "%8d", 8, (buf+off)); off+=8;
              put_I4( ds->annot_pixel[i], "%8d", 8, (buf+off)); off+=8;
              put_chars((char *) (buf+off), &(ds->annot_text[i][0]), 16); off+=16;
          }
       }
#else
       put_blanks((char*) (buf+1734),32);   /* spare */
       put_I4(ds->no_beams, "%2d", 2, (buf+1766));
       put_chars((char*) (buf+1768),ds->beam1,  4);
       put_chars((char*) (buf+1772),ds->beam2,  4);
       put_chars((char*) (buf+1776),ds->beam3,  4);
       put_chars((char*) (buf+1780),ds->beam4,  4);
       put_F4(ds->prf1, "%8.3f", 8, (buf+1784));
       put_F4(ds->prf2, "%8.3f", 8, (buf+1792));
       put_F4(ds->prf3, "%8.3f", 8, (buf+1800));
       put_F4(ds->prf4, "%8.3f", 8, (buf+1808));
       put_F4(ds->rng_gate1, "%8.3f", 8, (buf+1816));
       put_F4(ds->rng_gate2, "%8.3f", 8, (buf+1824));
       put_F4(ds->rng_gate3, "%8.3f", 8, (buf+1832));
       put_F4(ds->rng_gate4, "%8.3f", 8, (buf+1840));
       put_I4(ds->tot_pls_burst, "%4d", 4, (buf+1848));
       put_I4(ds->val_pls_burst, "%4d", 4, (buf+1852));
       put_I4(ds->az_ovlp_nxt_img, "%8d", 8, (buf+1856));
       put_I4(ds->rg_off_nxt_img, "%8d", 8, (buf+1864));
       put_chars((char*) (buf+1872), ds->cal_params_file, 32);
       put_chars((char*) (buf+1904), ds->scan_results_file, 32);
       put_chars((char*) (buf+1936), ds->scanner_version, 16);
       put_chars((char*) (buf+1952), ds->decode_version, 16);

       /* for the spare */
       /* put_blanks ((char*) (buf+1872), 2226); should be 2224 */
       put_blanks ((char*) (buf+1968), 2128);
#endif

    if ( out != (FILE *) NULL) 
                write_record( out, buf, sizeof(unsigned char),  nitems);

    if (Debug && !get_recalc_status()) {

       COM_entry = DSS_entry;
       printf("\n*********** begin of Dataset Summary record *******************\n");
       printf("\n RECORD SEQUENCE\t%ld", d->rec_seq);
       printf("\n RECORD SUB 1\t\t%d",d->rec_sub1);
       printf("\n RECORD TYPE\t\t%d",d->rec_type);
       printf("\n RECORD SUB 2\t\t%d",d->rec_sub2);
       printf("\n RECORD SUB 3\t\t%d",d->rec_sub3);
       printf("\n RECORD LENGTH\t\t%ld\n",d->length);
       printfd("\n DSS SEQ NUM\t\t%d",ds->seq_num);
       printfd("\n SAR CHNL INDTR\t\t%d",ds->sar_chan);
       printfs("\n SCENE INDICATOR\t%s",ds->product_id);
       printfs("\n SCENE DESIGNATOR\t%s",ds->scene_des);
       printfs("\n INPT SCN CTR TRIM\t%s",ds->inp_sctim);
       printfs("\n ASC/DESCENDING\t\t%s",ds->asc_des);
       printff("\n LAT @ SCN CTR\t\t%16.7f",ds->pro_lat);
       printff("\n LONG @ SCN CTR\t\t%16.7f",ds->pro_long);
       printff("\n SCN CTR HEADING\t%16.7f",ds->pro_head);
       printfs("\n ELLIP DESIGNATOR\t%s",ds->ellip_des);
       printff("\n ELLIP SEMIMAJOR\t%16.7f",ds->ellip_maj);
       printff("\n ELLIP SEMIMINOR\t%16.7f",ds->ellip_min);
       printff("\n EARTH MASS\t\t%16.7f",ds->earth_mass);
       printff("\n GRAVITATIONAL CNST\t%16.7f",ds->grav_const);
       printff("\n ELLIP PARM 1\t\t%16.7f",ds->ellip_j[0]);
       printff("\n ELLIP PARM 2\t\t%16.7f",ds->ellip_j[1]);
       printff("\n ELLIP PARM 3\t\t%16.7f",ds->ellip_j[2]);
       printff("\n AVG TERRAIN HT\t\t%16.7f",ds->terrain_h); 
#ifdef PRE_RADARSAT
       printfl("\n IMG CTR LINE NUM\t%ld",ds->sc_lin);
       printfl("\n IMG CTR PIX NUM\t%ld",ds->sc_pix);
#else
       printfl("\n IMG CTR LINE NUM\t%ld",ds->sc_lin);
       printfl("\n IMG CTR PIX NUM\t%ld",ds->sc_pix);
#endif
       printff("\n IMAGE LENGTH\t\t%16.7f",ds->scene_len);
       printff("\n IMAGE WIDTH\t\t%16.7f",ds->scene_wid);
       printfd("\n NUM SAR CHANNELS\t%d",ds->nchn);
       printfs("\n MISSION ID\t\t%s",ds->mission_id);
       printfs("\n SENSOR ID\t\t%s",ds->sensor_id);
       printfs("\n ORBIT NUMBER\t\t%s",ds->revolution);
       printff("\n PLAT LAT @ NADIR\t%8.3f",ds->plat_lat);
       printff("\n PLAT LONG @ NADIR\t%8.3f",ds->plat_long);
       printff("\n PLAT HEADING\t\t%8.3f",ds->plat_head_scene);
       printff("\n SNSR CLK ANGLE\t\t%8.3f",ds->clock_ang);
       printff("\n INCIDENCE ANGLE\t\t%8.3f",ds->incident_ang);
       printff("\n RADAR FREQUENCY\t%8.3f",ds->frequency);
       printff("\n RDR WAVELENGTH\t\t%16.7f",ds->wave_length);
       printfs("\n MOTION COMP IND\t%s",ds->motion_comp);
       printfs("\n RNG PULSE CODE\t\t%s",ds->pulse_code);
       printfe("\n RNG CHIRP 1\t\t%16.7E",ds->ampl_coef[0]);
       printfe("\n RNG CHIRP 2\t\t%16.7E",ds->ampl_coef[1]);
       printfe("\n RNG CHIRP 3\t\t%16.7E",ds->ampl_coef[2]);
       printfe("\n RNG CHIRP 4\t\t%16.7E",ds->ampl_coef[3]);
       printfe("\n RNG CHIRP 5\t\t%16.7E",ds->ampl_coef[4]);
       printfe("\n RNG PHASE 1\t\t%16.7E",ds->phas_coef[0]);
       printfe("\n RNG PHASE 2\t\t%16.7E",ds->phas_coef[1]);
       printfe("\n RNG PHASE 3\t\t%16.7E",ds->phas_coef[2]);
       printfe("\n RNG PHASE 4\t\t%16.7E",ds->phas_coef[3]);
       printfe("\n RNG PHASE 5\t\t%16.7E",ds->phas_coef[4]);
       printfl("\n CHRP EXTRACTION IND\t%ld",ds->chirp_ext_ind);
       printff("\n RNG CMPLX SAMPLE RATE\t%16.7f",ds->rng_samp_rate);
       printff("\n RNG GATE\t\t%16.7f",ds->rng_gate);
       printff("\n RNG PULSE LEN\t\t%16.7f",ds->rng_length);
       printfs("\n BASEBAND FLAG\t\t%s",ds->baseband_f);
       printfs("\n RNG COMPRESS FLAG\t%s",ds->rngcmp_f);
       printff("\n RCVR GAIN POLAR\t%16.7f",ds->gn_polar);
       printff("\n RCVR GAIN CROSS\t%16.7f",ds->gn_cross);
       printfl("\n QUANT BITS/CHNL\t%ld",ds->chn_bits);
       printfs("\n QUANTZR DESCRPT\t%s",ds->quant_desc);
       printff("\n I CHNL DC BIAS\t\t%16.7f",ds->i_bias);
       printff("\n Q CHNL DC BIAS\t\t%16.7f",ds->q_bias);
       printff("\n I/Q CHNL RATIO\t\t%16.7f",ds->iq_ratio);
       printff("\n SPARE_DSS_7\t\t\t%16.7f",ds->spare_dss_7);
       printff("\n SPARE_DSS_8\t\t\t%16.7f",ds->spare_dss_8);
       printff("\n ELCTRNC BORESITE\t%16.7f",ds->ele_sight);
       printff("\n MECHNCL BORESITE\t%16.7f",ds->mech_sight);
       printfs("\n ECHO TRK FLAG\t\t%s",ds->echo_track);
       printff("\n NOMINAL PRF\t\t%16.7f",ds->prf);
       printff("\n ANT ELEV BEAM WD\t%16.7f",ds->elev_beam);
       printff("\n ANT AZI BEAM WD\t%16.7f",ds->azi_beam);
       printfs("\n SATLT BINARY TIME\t%s",ds->sat_bintim);
       printfs("\n SATLT CLOCK TIME\t%s",ds->sat_clktim);
       printfl("\n SATLT CLOCK INC\t%ld",ds->sat_clkinc);
       printfs("\n PROCESSING FACILITY\t%s",ds->fac_id);
       printfs("\n PROCESSING SYSTEM\t%s",ds->sys_id);
       printfs("\n PROCESSING VERSION\t%s",ds->ver_id);
       printfs("\n FAC PROCESS CODE\t%s",ds->fac_code);
       printfs("\n PRODUCT CODE\t\t%s",ds->lev_code);
       printfs("\n PRODUCT TYPE\t\t%s",ds->product_type);
       printfs("\n PROCESSING ALGTHM\t%s",ds->algor_id);
       printff("\n NUM LOOKS IN AZI\t%16.7f",ds->n_azilok);
       printff("\n NUM LOOKS IN RNG\t%16.7f",ds->n_rnglok);
       printff("\n BNDWDTH/LOOK IN AZI\t%16.7f",ds->bnd_azilok);
       printff("\n BNDWDTH/LOOK IN RNG\t%16.7f",ds->bnd_rnglok);
       printff("\n PROC BNDWDTH AZI\t%16.7f",ds->bnd_azi);
       printff("\n PROC BNDWDTH RNG\t%16.7f",ds->bnd_rng);
       printfs("\n AZI WEIGHT FUNC\t%s",ds->azi_weight);
       printfs("\n RNG WEIGHT FUNC\t%s",ds->rng_weight);
       printfs("\n DATA INPUT SRC\t\t%s",ds->data_inpsrc);
       printff("\n NOM RESOLUTION RNG\t%16.7f",ds->rng_res);
       printff("\n NOM RESOLUTION AZI\t%16.7f",ds->azi_res);
       printff("\n RADIO STRETCH BIAS\t%16.7f",ds->radi_stretch[0]);
       printff("\n RADIO STRETCH GAIN\t%16.7f",ds->radi_stretch[1]);
       printff("\n ALT DOPPLER FREQ 1\t%16.7f",ds->alt_dopcen[0]);
       printff("\n ALT DOPPLER FREQ 2\t%16.7f",ds->alt_dopcen[1]);
       printff("\n ALT DOPPLER FREQ 3\t%16.7f",ds->alt_dopcen[2]);
       printff("\n CRT DOPPLER FREQ 1\t%16.7f",ds->crt_dopcen[0]);
       printff("\n CRT DOPPLER FREQ 2\t%16.7f",ds->crt_dopcen[1]);
       printff("\n CRT DOPPLER FREQ 3\t%16.7f",ds->crt_dopcen[2]);
       printfs("\n TIME DIRECT RNG\t%s",ds->time_dir_pix);
       printfs("\n TIME DIRECT AZI\t%s",ds->time_dir_lin);
       printff("\n ALT DOPPLER RATE 1\t%16.7f",ds->alt_rate[0]);
       printff("\n ALT DOPPLER RATE 2\t%16.7f",ds->alt_rate[1]);
       printff("\n ALT DOPPLER RATE 3\t%16.7f",ds->alt_rate[2]);
       printff("\n CRT DOPPLER RATE 1\t%16.7f",ds->crt_rate[0]);
       printff("\n CRT DOPPLER RATE 2\t%16.7f",ds->crt_rate[1]);
       printff("\n CRT DOPPLER RATE 3\t%16.7f",ds->crt_rate[2]);
       printfs("\n LINE CONTENT IND\t%s",ds->line_cont);
       printfs("\n CLUTTER LOCK FLAG\t%s",ds->clutterlock_flg);
       printfs("\n AUTOFOCUS FLAG\t\t%s",ds->auto_focus);
       printff("\n LINE SPACING\t\t%16.7f",ds->line_spacing);
       printff("\n PIXEL SPACING\t\t%16.7f",ds->pixel_spacing);
       printfs("\n RNG COMPRESS DESG\t%s",ds->rngcmp_desg);
#ifdef PRE_RADARSAT
       printfd("\n NUM ANNOTATION PTS\t%d",ds->annot_pts);
       for (i=0; i<ds->annot_pts; i++) {
           printfd("\n LINE NUM ANNOT START\t\t%d",ds->annot_line[i]);
           printfd("\n PIXEL NUM ANNOT START\t\t%d",ds->annot_pixel[i]);
           printfd("\n ANNOTATION TEXT\t\t%d",ds->annot_text[i][0]);
       }
#else
       printfd("\n NUM OF BEAMS\t%d",ds->no_beams);
       printfs("\n BEAM 1 IDENTIFIER\t%s",ds->beam1);
       printfs("\n BEAM 2 IDENTIFIER\t%s",ds->beam2);
       printfs("\n BEAM 3 IDENTIFIER\t%s",ds->beam3);
       printfs("\n BEAM 4 IDENTIFIER\t%s",ds->beam4);
       printff("\n PRF OF BEAM 1 HZ\t%8.3f",ds->prf1);
       printff("\n PRF OF BEAM 2 HZ\t%8.3f",ds->prf2);
       printff("\n PRF OF BEAM 3 HZ\t%8.3f",ds->prf3);
       printff("\n PRF OF BEAM 4 HZ\t%8.3f",ds->prf4);
       printff("\n RANGE GATE OF BEAM 1\t%8.3f",ds->rng_gate1);
       printff("\n RANGE GATE OF BEAM 2\t%8.3f",ds->rng_gate2);
       printff("\n RANGE GATE OF BEAM 3\t%8.3f",ds->rng_gate3);
       printff("\n RANGE GATE OF BEAM 4\t%8.3f",ds->rng_gate4);
       printfd("\n TOTAL PULSES PER BURST\t%d",ds->tot_pls_burst);
       printfd("\n VALID PULSES PER BURST\t%d",ds->val_pls_burst);
       printfd("\n RANGE LNS OVERLAP IN AZI WITH NEXT\t%d",ds->az_ovlp_nxt_img);
       printfd("\n PILEX OFFSET IN RANGE WITH NEXT\t%d",ds->rg_off_nxt_img);
       printfs("\n CALIBRATION PARAMETER FILE\t\t%s",ds->cal_params_file);
       printfs("\n SCAN RESULTS FILE\t\t%s",ds->scan_results_file);
       printfs("\n SCANNER VERSION\t\t%s",ds->scanner_version);
       printfs("\n DECODE VERSION\t\t%s",ds->decode_version);
#endif
       printf("\n*********** end of Dataset Summary  record ********************\n");
    }
    if (ds->next != (Dataset_Sum *) NULL && !mode) DSS_to_LDR( ds->next, out, mode );
    return(TRUE);
}

Map_Proj* Get_L_MP( SARL_ptr* t )
{
    if (t != NULL ) {
       return( t->map_proj );
    }
    else 
       return( (Map_Proj*) NULL);
}

Map_Proj* Get_MP_Num( SARL_ptr* t, int num )
{
    Sarl_Desc_Rec* d;
    Map_Proj* mp;
    int i=1;

    if (t != NULL) {
       d = &(t->descript);
       if (d->n_map_proj > 0 && num > 0 && num <= d->n_map_proj) {
          mp = t->map_proj;
          while (i<num) { mp=mp->next; i++; }
       }
       return( mp );
    }
    else 
       return( (Map_Proj*) NULL);
}

int MP_to_LDR( Map_Proj* mp, FILE* out, int mode ) 
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    desc_rec *d = &(mp->desc);
    int nitems;
    int i, off;
    unsigned int *lb;

    lb = (unsigned int *) buf;


    if (mp== (Map_Proj*) NULL) return(FALSE);

       /* allocate a temp buffer */
       nitems=mp->desc.length;

       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          MP_to_LDR(mp, out, 1);
          mp->desc.length=get_rec_length();
          printf("\n Calculated record length for Map Projection %ld\n",mp->desc.length);
          nitems=mp->desc.length;
          reset_rec_length();
       }

       i=d->rec_seq;
       zero_set(buf,nitems);

       if ( (current=GetCurrentSARL()) != NULL )
          Check_Rec_Seq( &i, buf, current, 0);
       else
	    *lb = htonl( i );
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
	lb[2] = htonl( nitems );
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LM1SUB, LMTYPE, LM2SUB, LM3SUB) ) {
          printf("\n Descriptor record and subtype mismatch - expecting MP codes");
       }
       put_blanks((char*) (buf+12), 16);   /* spare */
       put_chars((char*) (buf+28), mp->map_desc, 32);
       put_I4(mp->n_pixel, "%16d", 16, (buf+60));
       put_I4(mp->n_line, "%16d", 16, (buf+76));
       put_F4(mp->pixel_spacing, "%16.7f", 16, (buf+92));
       put_F4(mp->line_spacing, "%16.7f", 16, (buf+108));
       put_F4(mp->osc_orient, "%16.7f", 16, (buf+124));
       put_F4(mp->orb_incl, "%16.7f", 16, (buf+140));
       put_F4(mp->asc_node, "%16.7f", 16, (buf+156));
       put_F4(mp->isc_dist, "%16.7f", 16, (buf+172));
       put_F4(mp->geo_alt, "%16.7f", 16, (buf+188));
       put_F4(mp->isc_vel, "%16.7f", 16, (buf+204));
       put_F4(mp->plat_head, "%16.7f", 16, (buf+220));
       put_chars((char*) (buf+236), mp->ref_ellip, 32);
       put_F4(mp->semi_major, "%16.7f", 16, (buf+268));
       put_F4(mp->semi_minor, "%16.7f", 16, (buf+284));
       for(i=0,off=300; i<3; i++, off+=16) 
           put_F4(mp->datum_shift[i], "%16.7f", 16, (buf+off));
       for(i=0,off=348; i<3; i++, off+=16) 
           put_F4(mp->aux_datum_shift[i], "%16.7f", 16, (buf+off));
       put_F4(mp->scal_ellip, "%16.7f", 16, (buf+396));
       put_chars((char*) (buf+412), mp->projection, 32);
       put_chars((char*) (buf+444), mp->utm_desc, 32);
       put_chars((char*) (buf+476), mp->utm_zone_sig,  4);
       put_F4(mp->utm_east_orig, "%16.7f", 16, (buf+480));
       put_F4(mp->utm_north_orig, "%16.7f", 16, (buf+496));
       put_F4(mp->utm_cent_long, "%16.7f", 16, (buf+512));
       put_F4(mp->utm_cent_lat, "%16.7f", 16, (buf+528));
       put_F4(mp->utm_stand_par[0], "%16.7f", 16, (buf+544));
       put_F4(mp->utm_stand_par[1], "%16.7f", 16, (buf+560));
       put_F4(mp->utm_scale, "%16.7f", 16, (buf+576));
       put_chars((char*) (buf+592), mp->ups_desc, 32);
       put_F4(mp->ups_cent_long, "%16.7f", 16, (buf+624));
       put_F4(mp->ups_cent_lat, "%16.7f", 16, (buf+640));
       put_F4(mp->ups_scale, "%16.7f", 16, (buf+656));
       put_chars((char*) (buf+672), mp->nsp_desc, 32);
       put_F4(mp->nsp_east_orig, "%16.7f", 16, (buf+704));
       put_F4(mp->nsp_north_orig, "%16.7f", 16, (buf+720));
       put_F4(mp->nsp_cent_long, "%16.7f", 16, (buf+736));
       put_F4(mp->nsp_cent_lat, "%16.7f", 16, (buf+752));
       for(i=0,off=768; i<4; i++, off+=16) 
           put_F4(mp->nsp_stand_par[i], "%16.7f", 16, (buf+off));
       for(i=0,off=832; i<3; i++, off+=16) 
           put_F4(mp->nsp_stand_mer[i], "%16.7f", 16, (buf+off));
       put_blanks( (char *) (buf+880), 64);   /* spare */
       for(i=0,off=944; i<8; i++, off+=16) 
           put_F4(mp->corner_ne[i], "%16.7f", 16, (buf+off));
       for(i=0,off=1072; i<8; i++, off+=16) 
           put_F4(mp->corner_ll[i], "%16.7f", 16, (buf+off));
       for(i=0,off=1200; i<4; i++, off+=16) 
           put_F4(mp->terr_height[i], "%16.7f", 16, (buf+off));
       for(i=0,off=1264; i<8; i++, off+=20) 
           put_F4(mp->lp_conv_coef[i], "%20.10E", 20, (buf+off));
       for(i=0,off=1424; i<8; i++, off+=20) 
           put_F4(mp->mp_conv_coef[i], "%20.10E", 20, (buf+off));
       put_blanks( (char *) (buf+1584), 36);  /* spare */
    if ( out != (FILE *) NULL)
               write_record( out, buf, sizeof(unsigned char),  nitems);

    if (Debug && !get_recalc_status()) {
       /* Write the contents of the Map Projection record  */

        COM_entry = MP_entry;
       printf("\n**************** begin of Map Projection record *******************\n");
       printf("\n RECORD SEQUENCE\t%ld", d->rec_seq);
       printf("\n RECORD SUB 1\t\t%d",d->rec_sub1);
       printf("\n RECORD TYPE\t\t%d",d->rec_type);
       printf("\n RECORD SUB 2\t\t%d",d->rec_sub2);
       printf("\n RECORD SUB 3\t\t%d",d->rec_sub3);
       printf("\n RECORD LENGTH\t\t%ld\n",d->length);
       printfs("\n MAP PROJ DESC\t\t%s",mp->map_desc);
       printfl("\n NUM PIX/LINE IMAGE\t\t%ld",mp->n_pixel);
       printfl("\n NUM LINES\t\t%ld",mp->n_line);
       printff("\n NOM INTER-PIX DIST\t\t%16.7f",mp->pixel_spacing);
       printff("\n NOM INTER-LINE DIST\t\t%16.7f",mp->line_spacing);
       printff("\n ORIENT OUT SCN CTR\t\t%16.7f",mp->osc_orient);
       printff("\n PLAT ORB INCLINATION\t\t%16.7f",mp->orb_incl);
       printff("\n ACTL ASCEND NODE\t\t%16.7f",mp->asc_node);
       printff("\n PLAT DIST @ SCN CTR\t\t%16.7f",mp->isc_dist);
       printff("\n ALT PLAT REL 2 ELLIP\t\t%16.7f",mp->geo_alt);
       printff("\n ACTL GRND SPD @ NADIR\t\t%16.7f",mp->isc_vel);
       printff("\n PLAT HEADING\t\t%16.7f",mp->plat_head);
       printfs("\n REF ELLIPSOID NAME\t\t%s",mp->ref_ellip);
       printff("\n SEMIMAJR AXIS ELLIP\t\t%16.7f",mp->semi_major);
       printff("\n SEMIMINR AXIS ELLIP\t\t%16.7f",mp->semi_minor);
       printff("\n DATUM SHFT PARAM 1\t\t%16.7f",mp->datum_shift[0]);
       printff("\n DATUM SHFT PARAM 2\t\t%16.7f",mp->datum_shift[1]);
       printff("\n DATUM SHFT PARAM 3\t\t%16.7f",mp->datum_shift[2]);
       printff("\n AUX SHFT PARAM 1\t\t%16.7f",mp->aux_datum_shift[0]);
       printff("\n AUX SHFT PARAM 2\t\t%16.7f",mp->aux_datum_shift[1]);
       printff("\n AUX SHFT PARAM 3\t\t%16.7f",mp->aux_datum_shift[2]);
       printff("\n ELLIP SCALE FCTR\t\t%16.7f",mp->scal_ellip);
       printfs("\n ALPHANUMERIC MAP DESC\t\t%s",mp->projection);
       printfs("\n UTM DESC\t\t%s",mp->utm_desc);
       printfs("\n UTM ZONE SIGN\t\t%s",mp->utm_zone_sig);
       printff("\n UTM MAP ORG/ FALSE EAST\t\t%16.7f",mp->utm_east_orig);
       printff("\n UTM MAP ORIG/ FALSE NORTH\t\t%16.7f",mp->utm_north_orig);
       printff("\n UTM PROJ CTR LONG\t\t%16.7f",mp->utm_cent_long);
       printff("\n UTM PROJ CTR LAT\t\t%16.7f",mp->utm_cent_lat);
       printff("\n UTM STD PARALLEL 1\t\t%16.7f",mp->utm_stand_par[0]);
       printff("\n UTM STD PARALLEL 2\t\t%16.7f",mp->utm_stand_par[1]);
       printff("\n UTM SCALE FACTOR\t\t%16.7f",mp->utm_scale);
       printfs("\n UPS DESC\t\t%s",mp->ups_desc);
       printff("\n UPS PROJ CTR LONG\t\t%16.7f",mp->ups_cent_long);
       printff("\n UPS PROJ CTR LAT\t\t%16.7f",mp->ups_cent_lat);
       printff("\n UPS SCALE FACTOR\t\t%16.7f",mp->ups_scale );
       printfs("\n NSP DESC\t\t%s",mp->nsp_desc);
       printff("\n NSP ORG/ FALSE EAST\t\t%16.7f",mp->nsp_east_orig);
       printff("\n NSP ORG/ FALE NORTH\t\t%16.7f",mp->nsp_north_orig);
       printff("\n NSP PROJ CTR LONG\t\t%16.7f",mp->nsp_cent_long);
       printff("\n NSP PROJ CTR LAT\t\t%16.7f",mp->nsp_cent_lat);
       printff("\n NSP STR PARALLEL 1\t\t%16.7f",mp->nsp_stand_par[0]);
       printff("\n NSP STR PARALLEL 2\t\t%16.7f",mp->nsp_stand_par[1]);
       printff("\n NSP STR PARALLEL 3\t\t%16.7f",mp->nsp_stand_par[2]);
       printff("\n NSP STR PARALLEL 4\t\t%16.7f",mp->nsp_stand_par[3]);
       printff("\n NSP STD MERIDIAN 1\t\t%16.7f",mp->nsp_stand_mer[0]);
       printff("\n NSP STD MERIDIAN 2\t\t%16.7f",mp->nsp_stand_mer[1]);
       printff("\n NSP STD MERIDIAN 3\t\t%16.7f",mp->nsp_stand_mer[2]);
       printff("\n TOP LEFT CRNR NORTH\t\t%16.7f",mp->corner_ne[0]);
       printff("\n TOP LEFT CRNR EASR\t\t%16.7f",mp->corner_ne[1]);
       printff("\n TOP RGHT CRNR NORTH\t\t%16.7f",mp->corner_ne[2]);
       printff("\n TOP RGHT CRNR EAST\t\t%16.7f",mp->corner_ne[3]);
       printff("\n BTM RGHT CRNR NORTH\t\t%16.7f",mp->corner_ne[4]);
       printff("\n BTM RGHT CRNR EAST\t\t%16.7f",mp->corner_ne[5]);
       printff("\n BTM LEFT CRNR NORTH\t\t%16.7f",mp->corner_ne[6]);
       printff("\n BTM LEFT CRNR EAST\t\t%16.7f",mp->corner_ne[7]);
       printff("\n TOP LEFT CRNR LAT\t\t%16.7f",mp->corner_ll[0]);
       printff("\n TOP LEFT CRNR LONG\t\t%16.7f",mp->corner_ll[1]);
       printff("\n TOP RGHT CRNR LAT\t\t%16.7f",mp->corner_ll[2]);
       printff("\n TOP RGHT CRNR LONG\t\t%16.7f",mp->corner_ll[3]);
       printff("\n BTM RGHT CRNR LAT\t\t%16.7f",mp->corner_ll[4]);
       printff("\n BTM RGHT CRNR LONG\t\t%16.7f",mp->corner_ll[5]);
       printff("\n BTM LEFT CRNR LAT\t\t%16.7f",mp->corner_ll[6]);
       printff("\n BTL LEFT CRNR LONG\t\t%16.7f",mp->corner_ll[7]);
       printff("\n TOP LEFT CRNR HT\t\t%16.7f",mp->terr_height[0]);
       printff("\n TOP RGHT CRNT HT\t\t%16.7f",mp->terr_height[1]);
       printff("\n BTM RGHT CRNR HT\t\t%16.7f",mp->terr_height[2]);
       printff("\n BTM LEFT CRNR HT\t\t%16.7f",mp->terr_height[3]);
       printfe("\n LINE COEFF 1\t\t%20.10E",mp->lp_conv_coef[0]);
       printfe("\n LINE COEFF 2\t\t%20.10E",mp->lp_conv_coef[1]);
       printfe("\n LINE COEFF 3\t\t%20.10E",mp->lp_conv_coef[2]);
       printfe("\n LINE COEFF 4\t\t%20.10E",mp->lp_conv_coef[3]);
       printfe("\n LINE COEFF 5\t\t%20.10E",mp->lp_conv_coef[4]);
       printfe("\n LINE COEFF 6\t\t%20.10E",mp->lp_conv_coef[5]);
       printfe("\n LINE COEFF 7\t\t%20.10E",mp->lp_conv_coef[6]);
       printfe("\n LINE COEFF 8\t\t%20.10E",mp->lp_conv_coef[7]);
       printfe("\n COEFF MAP 2 LINE POS 1\t\t%20.10E",mp->mp_conv_coef[0]);
       printfe("\n COEFF MAP 2 LINE POS 2\t\t%20.10E",mp->mp_conv_coef[1]);
       printfe("\n COEFF MAP 2 LINE POS 3\t\t%20.10E",mp->mp_conv_coef[2]);
       printfe("\n COEFF MAP 2 LINE POS 4\t\t%20.10E",mp->mp_conv_coef[3]);
       printfe("\n COEFF MAP 2 LINE POS 5\t\t%20.10E",mp->mp_conv_coef[4]);
       printfe("\n COEFF MAP 2 LINE POS 6\t\t%20.10E",mp->mp_conv_coef[5]);
       printfe("\n COEFF MAP 2 LINE POS 7\t\t%20.10E",mp->mp_conv_coef[6]);
       printfe("\n COEFF MAP 2 LINE POS 8\t\t%20.10E",mp->mp_conv_coef[7]); 
       printf("\n**************** end of Map Projection record *******************\n");
    }
    if (mp->next != (Map_Proj *) NULL && !mode) MP_to_LDR( mp->next, out, mode );
    return(TRUE);
}

Pos_Data* Get_L_PP( SARL_ptr* t )
{
    if (t != NULL ) {
       return( t->platform );
    }
    else 
       return( (Pos_Data*) NULL);
}

Pos_Data* Get_PP_Num( SARL_ptr* t, int num )
{
    Sarl_Desc_Rec* d;
    Pos_Data* p;
    int i=1;

    if (t != NULL) {
       d = &(t->descript);
       if (d->n_plat_pos > 0 && num > 0 && num <= d->n_plat_pos) {
          p = t->platform;
          while (i<num) { p=p->next; i++; }
       }
       return( p );
    }
    else 
       return( (Pos_Data*) NULL);
}

int PP_to_LDR( Pos_Data* p, FILE* out, int mode) 
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    Pos_Vect_Rec *pv;
    desc_rec *d = &(p->desc);
    int nitems;
    int i, off, set_counter = 0;
    unsigned int *lb;

    lb = (unsigned int *) buf;

    if (p == (Pos_Data*) NULL) return(FALSE);

       nitems=p->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          PP_to_LDR(p, out, 1);
          p->desc.length=get_rec_length();
             printf("\n Calculated record length for Platform Position Data %ld\n",p->desc.length);
          nitems=p->desc.length;
          reset_rec_length();
       }
          
       i=d->rec_seq;
       zero_set(buf,nitems);

       if ( (current=GetCurrentSARL()) != NULL )
          Check_Rec_Seq( &i, buf, current, 0);
       else
	    *lb = htonl( i );
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
	lb[2] = htonl( nitems );
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LP1SUB, LPTYPE, LP2SUB, LP3SUB) ) {
             printf("\n Descriptor record and subtype mismatch - expecting PP codes");
       }
       put_chars( (char*) (buf+12), p->orbit_ele_desg, 32);
       put_F4( p->orbit_ele[0],"%16.7f", 16, (buf+44));
       put_F4( p->orbit_ele[1],"%16.7f", 16, (buf+60));
       put_F4( p->orbit_ele[2],"%16.7f", 16, (buf+76));
       put_F4( p->orbit_ele[3],"%16.7f", 16, (buf+92));
       put_F4( p->orbit_ele[4],"%16.7f", 16, (buf+108));
       put_F4( p->orbit_ele[5],"%16.7f", 16, (buf+124));
       put_I4( p->ndata,"%4d", 4, (buf+140));
       put_I4( p->year,"%4d", 4, (buf+144));
       put_I4( p->month,"%4d", 4, (buf+148));
       put_I4( p->day,"%4d", 4, (buf+152));
       put_I4( p->gmt_day,"%4d", 4, (buf+156));
       put_F4( p->gmt_sec,"%22.15f", 22, (buf+160));
       put_F4( p->data_int,"%22.15f", 22, (buf+182));
       put_chars((char*) (buf+204), p->ref_coord, 64);
       put_F4( p->hr_angle,"%22.15f", 22, (buf+268));
       put_F4( p->alt_poserr,"%16.7f", 16, (buf+290));
       put_F4( p->crt_poserr,"%16.7f", 16, (buf+306));
       put_F4( p->rad_poserr,"%16.7f", 16, (buf+322));
       put_F4( p->alt_velerr,"%16.7f", 16, (buf+338));
       put_F4( p->crt_velerr,"%16.7f", 16, (buf+354));
       put_F4( p->rad_velerr,"%16.7f", 16, (buf+370));

       off=386;
       pv = p->pos_vect;
        while( pv != NULL) {
           put_F4( pv->pos[0],"%22.15f", 22, (buf+off)); off+=22;
           put_F4( pv->pos[1],"%22.15f", 22, (buf+off)); off+=22;
           put_F4( pv->pos[2],"%22.15f", 22, (buf+off)); off+=22;
           put_F4( pv->vel[0],"%22.15f", 22, (buf+off)); off+=22;
           put_F4( pv->vel[1],"%22.15f", 22, (buf+off)); off+=22;
           put_F4( pv->vel[2],"%22.15f", 22, (buf+off)); off+=22;
           pv = pv->next;
	   set_counter++;
       }

	while (set_counter < 3) {
	  put_blanks ((char *)(buf+off), 22); off+=22;
	  put_blanks ((char *)(buf+off), 22); off+=22;
	  put_blanks ((char *)(buf+off), 22); off+=22;
	  put_blanks ((char *)(buf+off), 22); off+=22;
	  put_blanks ((char *)(buf+off), 22); off+=22;
	  put_blanks ((char *)(buf+off), 22); off+=22;
	  set_counter++;
	}

	/* Fill out the spare */
	put_blanks ((char *)(buf+off), 242); off+=242;

    if (out != (FILE *) NULL )
             write_record( out, buf, sizeof(unsigned char),  nitems);  

    if (Debug && !get_recalc_status()) {

    /* Write the contents of the Platform Postion Data record  */

        COM_entry = PP_entry;
       printf("\n**************** begin of Platform Position record *******************\n");
       printf("\n RECORD SEQUENCE\t%ld", d->rec_seq);
       printf("\n RECORD SUB 1\t\t%d",d->rec_sub1);
       printf("\n RECORD TYPE\t\t%d",d->rec_type);
       printf("\n RECORD SUB 2\t\t%d",d->rec_sub2);
       printf("\n RECORD SUB 3\t\t%d",d->rec_sub3);
       printf("\n RECORD LENGTH\t\t%ld\n",d->length);
       printfs("\n ORBITAL ELMTS DESG\t\t%s",p->orbit_ele_desg);
       printff("\n ORBITAL ELEMENT 1\t\t%16.7f",p->orbit_ele[0]);
       printff("\n ORBITAL ELEMENT 2\t\t%16.7f",p->orbit_ele[1]);
       printff("\n ORBITAL ELEMENT 3\t\t%16.7f",p->orbit_ele[2]);
       printff("\n ORBITAL ELEMENT 4\t\t%16.7f",p->orbit_ele[3]);
       printff("\n ORBITAL ELEMENT 5\t\t%16.7f",p->orbit_ele[4]);
       printff("\n ORBITAL ELEMENT 6\t\t%16.7f",p->orbit_ele[5]);
       printfd("\n NUM DATA PTS\t\t\t%d",p->ndata);
       printfd("\n YR OF DATA PT\t\t\t%d",p->year);
       printfd("\n MON OF DATA PT\t\t\t%d",p->month);
       printfd("\n DAY OF DATA PT\t\t\t%d",p->day);
       printfd("\n DAY IN THE YR (GMT)\t\t%d",p->gmt_day);
       printff("\n SECS OF DAY (GMT) OF DATA\t%22.15f",p->gmt_sec);
       printff("\n INTRVL BTWN DATA PTS\t\t%22.15f",p->data_int);
       printfs("\n REF COORD SYSTEM\t\t%s",p->ref_coord);
       printff("\n GREENWICH MEAN HR ANGLE\t%22.15f",p->hr_angle);
       printff("\n ALONG TRK POS ERROR\t\t%16.7f",p->alt_poserr);
       printff("\n CROSS TRK POS ERROR\t\t%16.7f",p->crt_poserr);
       printff("\n RADIAL POS ERROR\t\t%16.7f",p->rad_poserr);
       printff("\n ALONG TRK VEL ERROR\t\t%16.7f",p->alt_velerr);
       printff("\n CROSS TRK VEL ERROR\t\t%16.7f",p->crt_velerr);
       printff("\n RADIAL VEL ERROR\t\t%16.7f",p->rad_velerr);

      pv = p->pos_vect;
      i=0;
      while(pv != NULL) {
          printfdf("\n DATA PT POS VECT #%d FOR X\t%22.15f",i+1,pv->pos[0]);
          printfdf("\n DATA PT POS VECT #%d FOR Y\t%22.15f",i+1,pv->pos[1]);
          printfdf("\n DATA PT POS VECT #%d FOR Z\t%22.15f",i+1,pv->pos[2]);
          printfdf("\n DATA PT VEL VECT #%d FOR X\t%22.15f",i+1,pv->vel[0]);
          printfdf("\n DATA PT VEL VECT #%d FOR Y\t%22.15f",i+1,pv->vel[1]);
          printfdf("\n DATA PT VEL VECT #%d FOR Z\t%22.15f",i+1,pv->vel[2]);
          pv=pv->next; i++;
       }
       printf("\n**************** end of Platform Position record *******************\n");
    }

    if (p->next != (Pos_Data *) NULL && !mode ) PP_to_LDR( p->next, out, mode );
    return(TRUE);
}

Att_Data* Get_L_AT( SARL_ptr* t )
{
    if (t != NULL ) {
       return( t->attitude );
    }
    else 
       return( (Att_Data*) NULL);
}

Att_Data* Get_AT_Num( SARL_ptr* t, int num )
{
    Sarl_Desc_Rec* d;
    Att_Data* a;
    int i=1;

    if (t != NULL) {
       d = &(t->descript);
       if (d->n_att_data > 0 && num > 0 && num <= d->n_att_data) {
          a = t->attitude;
          while (i<num) { a=a->next; i++; }
       }
       return( a );
    }
    else 
       return( (Att_Data*) NULL);
}


int AT_to_LDR( Att_Data* a, FILE* out, int mode ) 
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    Att_Vect_Rec *av;
    desc_rec *d = &(a->desc);
    int nitems;
    int i, off, set_counter = 0;
    unsigned int *lb;

    lb = (unsigned int *) buf;

    if (a == (Att_Data*) NULL) return(FALSE);

       nitems=a->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          AT_to_LDR(a, out, 1);
          a->desc.length=get_rec_length();
          printf("\n Calculated record length for Attitude Data %ld\n",a->desc.length);
          nitems=a->desc.length;
          reset_rec_length();
       }
          
       i=d->rec_seq;
       zero_set(buf,nitems);

       if ( (current=GetCurrentSARL()) != NULL )
          Check_Rec_Seq( &i, buf, current, 0);
       else
	    *lb = htonl( i );
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
	lb[2] = htonl( nitems );
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LA1SUB, LATYPE, LA2SUB, LA3SUB) ) {
          printf("\n Descriptor record and subtype mismatch - expecting  AT codes");
       }

       put_I4(a->npoint, "%4d", 4, (buf+12));

       av=a->att_vect;
       off=16;
       while (av!=NULL) {
            put_I4(av->gmt_day, "%4d", 4, (buf+off));         off+=4;
            put_I4(av->gmt_msec, "%8d", 8, (buf+off));        off+=8;
            put_I4(av->pitch_flag, "%4d", 4, (buf+off));      off+=4;
            put_I4(av->roll_flag, "%4d", 4, (buf+off));       off+=4;
            put_I4(av->yaw_flag, "%4d", 4, (buf+off));        off+=4;
            put_F4(av->pitch, "%14.6E", 14, (buf+off));       off+=14;
            put_F4(av->roll, "%14.6E", 14, (buf+off));        off+=14;
            put_F4(av->yaw, "%14.6E", 14, (buf+off));         off+=14;
            put_I4(av->pitch_rate_flag, "%4d", 4, (buf+off)); off+=4;
            put_I4(av->roll_rate_flag, "%4d", 4, (buf+off));  off+=4;
            put_I4(av->yaw_rate_flag, "%4d", 4, (buf+off));   off+=4;
            put_F4(av->pitch_rate, "%14.6E", 14, (buf+off));  off+=14;
            put_F4(av->roll_rate, "%14.6E", 14, (buf+off));   off+=14;
            put_F4(av->yaw_rate, "%14.6E", 14, (buf+off));    off+=14;
            av=av->next;
	    set_counter++;
       }

       /* Ensure that we did the loop 3 times. */
       while (set_counter < 3) {
            put_blanks((char *)(buf+off), 4);  off+=4;
            put_blanks((char *)(buf+off), 8);  off+=8;
            put_blanks((char *)(buf+off), 4);  off+=4;
            put_blanks((char *)(buf+off), 4);  off+=4;
            put_blanks((char *)(buf+off), 4);  off+=4;
            put_blanks((char *)(buf+off), 14); off+=14;
            put_blanks((char *)(buf+off), 14); off+=14;
            put_blanks((char *)(buf+off), 14); off+=14;
            put_blanks((char *)(buf+off), 4);  off+=4;
            put_blanks((char *)(buf+off), 4);  off+=4;
            put_blanks((char *)(buf+off), 4);  off+=4;
            put_blanks((char *)(buf+off), 14); off+=14;
            put_blanks((char *)(buf+off), 14); off+=14;
            put_blanks((char *)(buf+off), 14); off+=14;
	    set_counter++;
       }

       /* 648 rather than 640 (bug in ceos specs) */
       put_blanks ((char *)(buf+off), 648); off+= 648;

       if ( out != (FILE *) NULL) write_record( out, buf, sizeof(unsigned char),  nitems);


    if (Debug && !get_recalc_status()) {

    /* Write the contents of the Attitude Data record  */

        COM_entry = AT_entry;
       printf("\n**************** begin of Attitude record *******************\n");
       printf("\n RECORD SEQUENCE\t%ld", d->rec_seq);
       printf("\n RECORD SUB 1\t\t%d",d->rec_sub1);
       printf("\n RECORD TYPE\t\t%d",d->rec_type);
       printf("\n RECORD SUB 2\t\t%d",d->rec_sub2);
       printf("\n RECORD SUB 3\t\t%d",d->rec_sub3);
       printf("\n RECORD LENGTH\t\t%ld\n",d->length);
       printfd("\n NUM OF POINTS\t\t\t%d",a->npoint);
       av = a->att_vect;
       i=1;
       while (av!=NULL) {
          printf("\n POINT NUMBER\t\t\t%d",i);
          printfd("\n DAY IN THE YEAR (GMT)\t\t%d",av->gmt_day);
          printfl("\n MS OF THE DAY (GMT)\t\t%ld",av->gmt_msec);
          printfd("\n PTICH DATA QUAL FLAG\t\t%d",av->pitch_flag);
          printfd("\n ROLL DATA QUAL FLAG\t\t%d",av->roll_flag);
          printfd("\n YAW DATA QUAL FLAG\t\t%d",av->yaw_flag);
          printff("\n PITCH\t\t\t\t%14.6f",av->pitch);
          printff("\n ROLL\t\t\t\t%14.6f",av->roll);
          printff("\n YAW\t\t\t\t%14.6f",av->yaw);
          printfd("\n PITCH RATE QUAL FLAG\t\t%d",av->pitch_rate_flag);
          printfd("\n ROLL RATE QUAL FLAG\t\t%d",av->roll_rate_flag);
          printfd("\n YAW RATE QUAL FLAG\t\t%d",av->yaw_rate_flag);
          printff("\n PITCH RATE\t\t\t%14.6f",av->pitch_rate);
          printff("\n ROLL RATE\t\t\t%14.6f",av->roll_rate);
          printff("\n YAW RATE\t\t\t%14.6f",av->yaw_rate);
          printf("\n END OF POINT NUMBER\t\t%d",i);
          i++;
          av=av->next;
       }
       printf("\n**************** end of Attitude record *******************\n");
    }

    if (a->next != (Att_Data *) NULL && !mode) AT_to_LDR( a->next, out, mode);
    return(TRUE);
}

Radi_Data* Get_L_RD( SARL_ptr* t )
{
    if (t != NULL ) {
       return( t->radio_data );
    }
    else 
       return( (Radi_Data*) NULL);
}

Radi_Data* Get_RD_Num( SARL_ptr* t, int num )
{
    Sarl_Desc_Rec* d;
    Radi_Data* r;
    int i=1;

    if (t != NULL) {
       d = &(t->descript);
       if (d->n_radi_data > 0 && num > 0 && num <= d->n_radi_data) {
          r = t->radio_data;
          while (i<num) { r=r->next; i++; }
       }
       return( r );
    }
    else 
       return( (Radi_Data*) NULL);
}

int RD_to_LDR( Radi_Data* r, FILE* out, int mode ) 
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    desc_rec *d = &(r->desc);
    int nitems;
    int i, off;
    unsigned int *lb;

    lb = (unsigned int *) buf;

    if (r == (Radi_Data*) NULL) return(FALSE);

       nitems=r->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          RD_to_LDR(r, out, 1);
          r->desc.length=get_rec_length();
          printf("\n Calculated record length for Radiometric Data %ld\n",r->desc.length);
          nitems=r->desc.length;
          reset_rec_length();
       }
          
       i=d->rec_seq;
       zero_set(buf,nitems);

       if ( (current=GetCurrentSARL()) != NULL )
          Check_Rec_Seq( &i, buf, current, 0);
       else
	    *lb = htonl( i );
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
	lb[2] = htonl( nitems );
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LR1SUB, LRTYPE, LR2SUB, LR3SUB) ) {
          printf("\n Descriptor record and subtype mismatch - expecting  RD codes");
       }

       put_I4(r->seq_num, "%4d", 4, (buf+12));
       put_I4(r->n_data, "%4d", 4, (buf+16));
       put_I4(r->field_size, "%8d", 8, (buf+20));
       put_chars((char*) (buf+28), r->chan_ind, 4);
       put_blanks((char*) (buf+32), 4);      /* spare */
       put_chars((char*) (buf+36), r->table_desig, 24);
       put_I4(r->n_samp, "%8d", 8, (buf+60));
       put_chars((char*) (buf+68), r->samp_type, 16);
       put_F4(r->noise_fact, "%16.7E", 16, (buf+84));
       put_F4(r->linear_conv_fact, "%16.7E", 16, (buf+100));
       put_F4(r->offset_conv_fact, "%16.7E", 16, (buf+116));
       put_blanks((char*) (buf+132), 4);     /* spare */

       off = 136;
       for (i=0; i<r->n_samp; i++, off+=16) {
           put_F4(r->lookup_tab[i], "%16.7f", 16, (buf+off));
       }

       /* Ensure that this is done at least 256 times. */
       for (; i < 256; i++, off+=16) {
           put_blanks((char *)(buf+off), 16);
       }

       if ( out != (FILE *) NULL) write_record( out, buf, sizeof(unsigned char),  nitems);

    if (Debug && !get_recalc_status()) {

    /* Write the contents of the Radiometric Data record  */

        COM_entry = RD_entry;
       printf("\n**************** begin of Radiometric Data record *******************\n");
       printf("\n RECORD SEQUENCE\t%ld", d->rec_seq);
       printf("\n RECORD SUB 1\t\t%d",d->rec_sub1);
       printf("\n RECORD TYPE\t\t%d",d->rec_type);
       printf("\n RECORD SUB 2\t\t%d",d->rec_sub2);
       printf("\n RECORD SUB 3\t\t%d",d->rec_sub3);
       printf("\n RECORD LENGTH\t\t%ld\n",d->length);
       printfd("\n RADIO DATA SEQ NUM\t\t%d",r->seq_num);
       printfd("\n NUM OF RADIO DATA FLDS\t\t%d",r->n_data);
       printfl("\n RADIO DATA SET SIZE\t\t%ld",r->field_size);
       printfs("\n SAR CHANNEL ID\t\t\t%s",r->chan_ind);
       printfs("\n LUT DESIGNATOR\t\t\t%s",r->table_desig);
       printfl("\n NUM SAMPLES IN LUT\t\t%ld",r->n_samp);
       printfs("\n SAMPLE TYPE ID\t\t\t%s",r->samp_type);
       printff("\n NOISE SCALE FCTR\t\t%16.7f",r->noise_fact);
       printff("\n LINEAR CONV FCTR\t\t%16.7f",r->linear_conv_fact);
       printff("\n OFFSET CONV FCTR\t\t%16.7f",r->offset_conv_fact);

       for (i=0; i<r->n_samp; i++) {
           if (i<99) printfdf("\n LUT SAMPLE #%d\t\t\t= %16.7f",i+1,r->lookup_tab[i]);
           else
              printfdf("\n LUT SAMPLE #%d\t\t= %16.7f",i+1,r->lookup_tab[i]);
       }
       printf("\n**************** end of Radiometric Data record *******************\n");
    }

    if (r->next != (Radi_Data *) NULL && !mode) RD_to_LDR( r->next, out, mode);
    return(TRUE);
}


Radi_Comp* Get_L_RC( SARL_ptr* t )
{
    if (t != NULL ) {
       return( t->radio_comp );
    }
    else 
       return( (Radi_Comp*) NULL);
}

Radi_Comp* Get_RC_Num( SARL_ptr* t, int num )
{
    Sarl_Desc_Rec* d;
    Radi_Comp* r;
    int i=1;

    if (t != NULL) {
       d = &(t->descript);
       if (d->n_radi_comp > 0 && num > 0 && num <= d->n_radi_comp) {
          r = t->radio_comp;
          while (i<num) { r=r->next; i++; }
       }
       return( r );
    }
    else 
       return( (Radi_Comp*) NULL);
}


int RC_to_LDR( Radi_Comp* r, FILE* out, int mode ) 
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    Rad_Comp_Set *rc;
    Radio_Comp_Tbl *rt;
    desc_rec *d = &(r->desc);
    int nitems;
    int i=0, off;
    unsigned int *lb;

    lb = (unsigned int *) buf;

    if (r == (Radi_Comp*) NULL) return(FALSE);

       nitems=r->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          RC_to_LDR(r, out, 1);
          r->desc.length=get_rec_length();
          printf("\n Calculated record length for Radiometric Compensation %ld\n",r->desc.length);
          nitems=r->desc.length;
          reset_rec_length();
       }
          
       i=d->rec_seq;
       zero_set(buf,nitems);

       if ( (current=GetCurrentSARL()) != NULL )
          Check_Rec_Seq( &i, buf, current, 0);
       else
	    *lb = htonl( i );
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
	lb[2] = htonl( nitems );
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LC1SUB, LCTYPE, LC2SUB, LC3SUB) ) {
          printf("\n Descriptor record and subtype mismatch - expecting  RC codes");
       }

       put_I4(r->seq_num, "%4d", 4, (buf+12));
       put_I4(r->sar_chan, "%4d", 4, (buf+16));
       put_I4(r->n_dset, "%8d", 8, (buf+20));
       put_I4(r->dset_size, "%8d", 8, (buf+28));

       rc = r->set;
       off=36;
       while(rc!=NULL) {
           put_chars((char*) (buf+off), rc->comp_data_type, 8); off+=8;
           put_chars((char*) (buf+off), rc->data_descr, 32); off+=32;
           put_I4(rc->req_recs, "%4d", 4, (buf+off)); off+=4;
           put_I4(rc->table_seq_num, "%4d", 4, (buf+off)); off+=4;
           put_I4(rc->num_pairs, "%8d", 8, (buf+off)); off+=8;
           put_I4(rc->first_pixel, "%8d", 8, (buf+off)); off+=8;
           put_I4(rc->last_pixel, "%8d", 8, (buf+off)); off+=8;
           put_I4(rc->pixel_size, "%8d", 8, (buf+off)); off+=8;
           put_F4(rc->min_samp_index, "%16.7f", 16, (buf+off)); off+=16;   
           put_F4(rc->min_comp_value, "%16.7f", 16, (buf+off)); off+=16;
           put_F4(rc->max_samp_index, "%16.7f", 16, (buf+off)); off+=16;
           put_F4(rc->max_comp_value, "%16.7f", 16, (buf+off)); off+=16;
           put_blanks((char*) (buf+off), 16); off+=16;
           put_I4(rc->n_table_entries, "%8d", 8, (buf+off)); off+=8;

           rt = rc->tbl;
           while (rt!=NULL) {
               put_F4(rt->sample_offset, "%16.7f", 16, (buf+off)); off+=16;
               put_F4(rt->sample_gain, "%16.7f", 16, (buf+off)); off+=16;
               rt = rt->next;
           }
           rc = rc->next;
       }
       if ( out != (FILE *) NULL) write_record( out, buf, sizeof(unsigned char),  nitems);


    if (Debug && !get_recalc_status()) {

    /* Write the contents of the Radiometric Compensation record  */

        COM_entry = RC_entry;
       printf("\n**************** begin of Radiometric Compensation record *******************\n");
       printf("\n RECORD SEQUENCE\t%ld", d->rec_seq);
       printf("\n RECORD SUB 1\t\t%d",d->rec_sub1);
       printf("\n RECORD TYPE\t\t%d",d->rec_type);
       printf("\n RECORD SUB 2\t\t%d",d->rec_sub2);
       printf("\n RECORD SUB 3\t\t%d",d->rec_sub3);
       printf("\n RECORD LENGTH\t\t%ld\n",d->length);
       printfd("\n RADIO COMP SEQ NUM\t%d",r->seq_num);
       printfd("\n SAR CHANNEL ID\t\t%d",r->sar_chan);
       printfl("\n NUM RADIO COMP SETS\t%ld",r->n_dset);
       printfl("\n COMP DATA SET SIZE\t%ld",r->dset_size);

       rc = r->set;
       while (rc!=NULL) {
           printfs("\n COMP DATA TYPE DESIG\t%s",rc->comp_data_type);
           printfs("\n COMP DATA DESIG\t\t%s",rc->data_descr);
           printfd("\n NUM COMP RECS FOR COMP TBL\t%d",rc->req_recs);
           printfd("\n SEQ NUM IN FULL COMP TBL\t%d",rc->table_seq_num);
           printfl("\n TTL NUM PAIRS IN FULL COMP\t%ld",rc->num_pairs);
           printfl("\n PXL NUM TO 1ST CORCT VAL\t%ld",rc->first_pixel);
           printfl("\n PXL NUM TO LAST CORCT VAL\t%ld",rc->last_pixel);
           printfl("\n PIXEL GROUP SIZE\t%ld",rc->pixel_size);
           printff("\n MIN TBL OFFSET VAL\t%f",rc->min_samp_index);    
           printff("\n MIN TBL GAIN VAL\t%f",rc->min_comp_value);  
           printff("\n MAX TBL OFFSET VAL\t%f",rc->max_samp_index);  
           printff("\n MAX TBL GAIN VAL\t%f",rc->max_comp_value); 
           printfl("\n NUM COMP TBL ENTRIES\t%ld",rc->n_table_entries);

           rt = rc->tbl;
           while (rt!=NULL) {
               i++;
               printfdf("\n SAMPLE OFFSET #%d\t%16.7f",i,rt->sample_offset);
               printfdf("\n SAMPLE GAIN #%d\t%16.7f",i,rt->sample_gain);
               rt=rt->next;
           }
           rc = rc->next;
       }
       printf("\n**************** end of Radiometric Compensation record *******************\n");
    }

    if (r->next != (Radi_Comp *) NULL && !mode) RC_to_LDR( r->next, out, mode );
    return(TRUE);
}

Qual_Sum* Get_L_DQS( SARL_ptr* t )
{
    if (t != NULL ) {
       return( t->data_qual );
    }
    else 
       return( (Qual_Sum*) NULL);
}

Qual_Sum* Get_DQS_Num( SARL_ptr* t, int num )
{
    Sarl_Desc_Rec* d;
    Qual_Sum* q;
    int i=1;

    if (t != NULL) {
       d = &(t->descript);
       if (d->n_qual_sum > 0 && num > 0 && num <= d->n_qual_sum) {
          q = t->data_qual;
          while (i<num) { q=q->next; i++; }
       }
       return( q );
    }
    else 
       return( (Qual_Sum*) NULL);
}

int DQS_to_LDR( Qual_Sum* q, FILE* out, int mode ) 
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    desc_rec *d = &(q->desc);
    int nitems;
    int i, off;
    unsigned int *lb;

    lb = (unsigned int *) buf;

    if (q == (Qual_Sum*) NULL) return(FALSE);

       nitems=q->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          DQS_to_LDR(q, out, 1);
          q->desc.length=get_rec_length();
          printf("\n Calculated record length for Datas Quality Summary %ld\n",q->desc.length);
          nitems=q->desc.length;
          reset_rec_length();
       }
          
       i=d->rec_seq;
       zero_set(buf,nitems);

       if ( (current=GetCurrentSARL()) != NULL )
          Check_Rec_Seq( &i, buf, current, 0);
       else
	    *lb = htonl( i );
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
	lb[2] = htonl( nitems );
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LQ1SUB, LQTYPE, LQ2SUB, LQ3SUB) ) {
          printf("\n Descriptor record and subtype mismatch - expecting  DQS codes");
       }
       put_I4(q->seq_num, "%4d", 4, (buf+12));
       put_chars((char*) (buf+16), q->chan_ind, 4);
       put_chars((char*) (buf+20), q->cali_date, 6);
       put_I4(q->nchn, "%4d", 4, (buf+26));
       put_F4(q->islr, "%16.7f", 16, (buf+30));
       put_F4(q->pslr, "%16.7f", 16, (buf+46));;
       put_F4(q->azi_ambig, "%16.7f", 16, (buf+62));
       put_F4(q->rng_ambig, "%16.7f", 16, (buf+78));
       put_F4(q->snr, "%16.7f", 16, (buf+94));
       put_F4(q->ber, "%16.7e", 16, (buf+110));
       put_F4(q->rng_res, "%16.7f", 16, (buf+126));
       put_F4(q->azi_res, "%16.7f", 16, (buf+142));
       put_F4(q->rad_res, "%16.7f", 16, (buf+158)); 
       put_F4(q->dyn_rng, "%16.7f", 16, (buf+174)); 
       put_F4(q->abs_rad_unc_db, "%16.7f", 16, (buf+190));
       put_F4(q->abs_rad_unc_deg, "%16.7f", 16, (buf+206));

       off=222;
       for (i=0; (i<16); i++) {
          if (i<q->nchn) {
             put_F4(q->rel_rad_unc[0][i], "%16.7f", 16, (buf+off)); off+=16;
             put_F4(q->rel_rad_unc[1][i], "%16.7f", 16, (buf+off)); off+=16;
          }
          else {
             put_blanks((char*) (buf+off), 32); off+=32;
          }
       }
   
       put_F4(q->alt_locerr, "%16.7f", 16, (buf+734)); 
       put_F4(q->crt_locerr, "%16.7f", 16, (buf+750));
       put_F4(q->alt_scale, "%16.7f", 16, (buf+766)); 
       put_F4(q->crt_scale, "%16.7f", 16, (buf+782));
       put_F4(q->dis_skew, "%16.7f", 16, (buf+798));
       put_F4(q->ori_err, "%16.7f", 16, (buf+814));

       off=830;
       for (i=0; i<16; i++) {
           if (i<q->nchn) {
	     put_F4(q->misreg[0][i], "%16.7f", 16, (buf+off)); off+=16;
	     put_F4(q->misreg[1][i], "%16.7f", 16, (buf+off)); off+=16;
	   }
	   else {
	     put_blanks((char*) (buf+off), 32); off+=32;
          }
	     
       }
#ifndef PRE_RADARSAT
       put_F4(q->nesz, "%16.7f", 16, (buf+off)); off+=16;
       put_F4(q->enl, "%16.7f", 16, (buf+off)); off+=16;
       put_chars((char*) (buf+off), q->tb_update, 8); off+=8;
       put_chars((char*) (buf+off), q->cal_status, 16); off+=16;
       put_blanks((char*) (buf+off), 22); off+=22;
       put_chars((char*) (buf+off), q->cal_comment, 200); off+=200;
#else
       put_blanks((char *)(buf+off), 278); off+= 278;    /* spare */
#endif

    if ( out != (FILE *) NULL) 
       write_record( out, buf, sizeof(unsigned char),  nitems);

    if (Debug && !get_recalc_status()) {

    /* Write the contents of the Data Quality Summary record  */

        COM_entry = DQS_entry;
       printf("\n**************** begin of Data Quality Summary record *******************\n");
       printf("\n RECORD SEQUENCE\t%ld", d->rec_seq);
       printf("\n RECORD SUB 1\t\t%d",d->rec_sub1);
       printf("\n RECORD TYPE\t\t%d",d->rec_type);
       printf("\n RECORD SUB 2\t\t%d",d->rec_sub2);
       printf("\n RECORD SUB 3\t\t%d",d->rec_sub3);
       printf("\n RECORD LENGTH\t\t%ld\n",d->length);
       printfd("\n DATA QUAL SUM SEQ NUM\t\t%d",q->seq_num);
       printfs("\n SARA CHANNEL ID\t\t%s",q->chan_ind);
       printfs("\n DATE OF LAST CALIB UPDATE\t%s",q->cali_date);
       printfd("\n NUM OF CHANNELS\t\t%d",q->nchn);
       printff("\n INTEGRATED SIDE LOB RATIO\t%16.7f",q->islr);
       printff("\n PEAK SIDE LOBE RATIO\t\t%16.7f",q->pslr);
       printff("\n AZI AMBIGUITY\t\t\t%16.7f",q->azi_ambig);
       printff("\n RNG AMBIGUITY\t\t\t%16.7f",q->rng_ambig);
       printff("\n ESTIMATE OF SNR\t\t%16.7f",q->snr);
       printfe("\n ACTUAL BIT RATE ERROR\t\t\t%e",q->ber);
       printff("\n SLANT RNG RESOLUTION\t\t%16.7f",q->rng_res);
       printff("\n AZIMUTH RESOLUTION\t\t%16.7f",q->azi_res);
       printff("\n RADIOMETRIC RESOLUTION\t\t%16.7f",q->rad_res);
       printff("\n INSTAN DYNAMIC RANGE\t\t%16.7f",q->dyn_rng);
       printff("\n NOM RAD UNCERTAIN, DB\t\t%16.7f",q->abs_rad_unc_db);
       printff("\n NOM RAD UNCERTAIN, DEG\t\t%16.7f",q->abs_rad_unc_deg);

       printf("\n\n RELATIVE RADIOMETRIC DATA QUALITY");
       for (i=0; i<q->nchn; i++) {
           printfdf("\n REL RAD UNCERTAIN #%d, DB\t%16.7f",i+1,q->rel_rad_unc[0][i]);
           printfdf("\n REL RAD UNCERTAIN #%d, DEG\t%16.7f",i+1,q->rel_rad_unc[1][i]);
       }

       printf("\n\n ABSOLUTE GEOMETRIC DATA QUALITY");
       printff("\n LOC ERROR ALONG TRK\t\t%16.7f",q->alt_locerr);
       printff("\n LOC ERROR CROSS TRK\t\t%16.7f",q->crt_locerr);
       printff("\n ALONG TRK SCALE ERROR\t\t%16.7f",q->alt_scale);
       printff("\n CROSS TRK SCALE ERROR\t\t%16.7f",q->crt_scale);
       printff("\n DISTORTION SKEW\t\t%16.7f",q->dis_skew);
       printff("\n SCENE ORIENT ERROR\t\t%16.7f",q->ori_err);

       printf("\n\n RELATIVE GEOMETRIC DATA QUALITY");
       for (i=0; i<q->nchn; i++) {
           printfdf("\n ALONG TRK MISREG ERROR #%d\t%16.7f",i+1, q->misreg[0][i]);
           printfdf("\n CROSS TRK MISREG ERROR #%d\t%16.7f",i+1, q->misreg[1][i]);
       }
#ifndef PRE_RADARSAT
       printff("\n SCENE ORIENT ERROR\t\t%16.7f",q->nesz);
       printff("\n SCENE ORIENT ERROR\t\t%16.7f",q->enl);
       printfs("\n TABLE UPDATE DATE \t\t%s",q->tb_update);
       printfs("\n CALIBRATION STATUS \t\t%s",q->cal_status);
       printfs("\n CALIBRATION COMMENT\t\t%s",q->cal_comment);
#endif
       printf("\n**************** end of Data Quality Summary record *******************\n");
    }

    if (q->next != (Qual_Sum *) NULL && !mode) DQS_to_LDR( q->next, out, mode );
    return(TRUE);
}

Data_Hist* Get_L_DH( SARL_ptr* t )
{
    if (t != NULL ) {
       return( t->histogram );
    }
    else 
       return( (Data_Hist*) NULL);
}

Data_Hist* Get_DH_Num( SARL_ptr* t, int num )
{
    Sarl_Desc_Rec* d;
    Data_Hist* h;
    int i=1;

    if (t != NULL) {
       d = &(t->descript);
       if (d->n_data_hist > 0 && num > 0 && num <= d->n_data_hist) {
          h = t->histogram;
          while (i<num) { h=h->next; i++; }
       }
       return( h );
    }
    else 
       return( (Data_Hist*) NULL);
}


int DH_to_LDR( Data_Hist* h, FILE* out, int mode ) 
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    Hist_Data_Set *ht;
    int i, j=1;
    desc_rec *d = &(h->desc);
    int nitems;
    int off;
    unsigned int *lb;

    lb = (unsigned int *) buf;

    if (h == (Data_Hist*) NULL) return(FALSE);

#if 0
       nitems=h->desc.length;
#endif
       /* request DH records be fixed length */
       nitems=h->desc.length=4628;
       
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          DH_to_LDR(h, out, 1);
          h->desc.length=get_rec_length();
          printf("\n Calculated record length for Data Histrograms %ld\n",h->desc.length);
          nitems=h->desc.length;
          reset_rec_length();
       }
          
       i=d->rec_seq;
       zero_set(buf,nitems);

       if ( (current=GetCurrentSARL()) != NULL )
          Check_Rec_Seq( &i, buf, current, 0);
       else
	    *lb = htonl( i );
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
	lb[2] = htonl( nitems );
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LH1SUB, LHTYPE, LH2SUB, LH3SUB) ) {
          printf("\n Descriptor record and subtype mismatch - expecting  DH codes");
       }

       put_I4(h->seq_num, "%4d", 4, (buf+12));
       put_I4(h->sar_chan, "%4d", 4, (buf+16));
       put_I4(h->ntab, "%8d", 8, (buf+20));
       put_I4(h->ltab, "%8d", 8, (buf+28));

       ht = h->data_set;
       off = 36;
       while (ht!=NULL) {
            put_chars((char*) (buf+off), ht->hist_desc, 32); off+=32;
            put_I4(ht->nrec, "%4d", 4, (buf+off)); off+=4;
            put_I4(ht->tab_seq, "%4d", 4, (buf+off)); off+=4;
            put_I4(ht->nbin, "%8d", 8, (buf+off)); off+=8;
            put_I4(ht->ns_lin, "%8d", 8, (buf+off)); off+=8;
            put_I4( ht->ns_pix, "%8d", 8, (buf+off)); off+=8;
	    put_I4(ht->ngrp_lin, "%8d", 8, (buf+off)); off+=8;
            put_I4( ht->ngrp_pix, "%8d", 8, (buf+off)); off+=8;
            put_I4(ht->nsamp_lin, "%8d", 8, (buf+off)); off+=8;
            put_I4(ht->nsamp_pix, "%8d", 8, (buf+off)); off+=8;
            put_F4(ht->min_smp, "%16.7f", 16, (buf+off)); off+=16;
            put_F4(ht->max_smp, "%16.7f", 16, (buf+off)); off+=16;
            put_F4(ht->mean_smp, "%16.7f", 16, (buf+off)); off+=16;
            put_F4(ht->std_smp, "%16.7f", 16, (buf+off)); off+=16;
            put_F4(ht->smp_inc, "%16.7f", 16, (buf+off)); off+=16;
            put_F4(ht->min_hist, "%16.7f", 16, (buf+off)); off+=16;
            put_F4(ht->max_hist, "%16.7f", 16, (buf+off)); off+=16;
            put_F4(ht->mean_hist, "%16.7f", 16, (buf+off)); off+=16;
            put_F4(ht->std_hist, "%16.7f", 16, (buf+off)); off+=16;
            put_I4(ht->nhist, "%8d", 8, (buf+off)); off+=8;

            for (i=0; i<ht->nhist; i++) {
               put_I4(ht->data_values_hist[i], "%8d", 8, (buf+off)); off+=8;
            }
            ht=ht->next;
        }
	/* pad data histogram */
	if (nitems-off>0) {
#ifdef HDEBUG
	    printf("\nPadding DH record %d with %d bytes", h->seq_num,  nitems-off);
#endif
	    put_blanks((char*) (buf+off), nitems-off);

        }

    if ( out != (FILE *) NULL) 
	     write_record( out, buf, sizeof(unsigned char),  nitems);

    if (Debug && !get_recalc_status()) {

    /* Write the contents of the Data Histogram record  */

       COM_entry = h->DH_entry;
       printf("\n**************** begin of Data Histogram record *******************\n");
       printf("\n RECORD SEQUENCE\t%ld", d->rec_seq);
       printf("\n RECORD SUB 1\t\t%d",d->rec_sub1);
       printf("\n RECORD TYPE\t\t%d",d->rec_type);
       printf("\n RECORD SUB 2\t\t%d",d->rec_sub2);
       printf("\n RECORD SUB 3\t\t%d",d->rec_sub3);
       printf("\n RECORD LENGTH\t\t%ld\n",d->length);
       printfd("\n DATA HISTOGRAM SEQ NUM\t\t%d",h->seq_num);
       printfd("\n SAR CHANNEL ID\t\t\t%d",h->sar_chan);
       printfl("\n NUM HISTOGRAM DATA SETS\t%ld",h->ntab);
       printfl("\n TABLE DATA SET SIZE\t\t%ld",h->ltab);
 
       ht=h->data_set;
       while (ht!=NULL) {
           printf("\n\n DATA SET NUM\t\t\t%d",j++);
           printfs("\n HISTOGRAM DESCRIPTOR\t\t%s",ht->hist_desc);
           printfd("\n RECS NEEDED FOR FULL TABLE\t%d",ht->nrec);
           printfd("\n TBL REC SEQ NUM\t\t%d",ht->tab_seq);
           printfl("\n NUM OF TBL BINS\t\t%ld",ht->nbin);
           printfl("\n NUM SAMPLES IN LINE DIR\t%ld",ht->ns_lin);
           printfl("\n NUM SAMPLES ACROSS LINE DIR\t%ld",ht->ns_pix);
	   printfl("\n SAMPLE GRP SIZE IN LINE DIR\t%ld",ht->ngrp_lin);
           printfl("\n SAMPLE GRP SIZE ACROSS LINE DIR\t%ld",ht->ngrp_pix);
           printfl("\n NUM USED PER GRP IN LINE DIR\t\t%ld",ht->nsamp_lin);
           printfl("\n NUM USED PER GRP ACROSS LINE DIR\t%ld",ht->nsamp_pix);
           printff("\n MIN VAL FOR IST TBL BIN\t%16.7f",ht->min_smp);
           printff("\n MAX VAL FOR LAST TBL BIN\t%16.7f",ht->max_smp);
           printff("\n MEAN SAMPLE VALUE\t\t%16.7f",ht->mean_smp);
           printff("\n STD DEV OF SAMPLE VALUE\t%16.7f",ht->std_smp);
           printff("\n SAMPLE VALUE INCREMENT\t\t%16.7f",ht->smp_inc);
           printff("\n MIN TBL VALUE\t\t\t%16.7f",ht->min_hist);
           printff("\n MAX TBL VALUE\t\t\t%16.7f",ht->max_hist);
           printff("\n MEAN TBL VALUE\t\t\t%16.7f",ht->mean_hist);
           printff("\n STD DEV OF HIST TBL\t\t%16.7f",ht->std_hist);

           printfl("\n\n HISTOGRAM TBL SIZE\t%ld",ht->nhist);
           for (i=0; i<ht->nhist; i++) {
               printfdl("\n TABLE VALUE #%d\t\t%ld",i+1,ht->data_values_hist[i]);
           }
           ht=ht->next;
       }
       printf("\n**************** end of Data Histogram record *******************\n");
    }

    if (h->next != (Data_Hist *) NULL && !mode) DH_to_LDR( h->next, out, mode );
    return(TRUE);
}

Rng_Spec* Get_L_RS( SARL_ptr* t )
{
    if (t != NULL ) {
       return( t->spectra );
    }
    else 
       return( (Rng_Spec*) NULL);
}

Rng_Spec* Get_RS_Num( SARL_ptr* t, int num )
{
    Sarl_Desc_Rec* d;
    Rng_Spec* r;
    int i=1;

    if (t != NULL) {
       d = &(t->descript);
       if (d->n_rang_spec > 0 && num > 0 && num <= d->n_rang_spec) {
          r = t->spectra;
          while (i<num) { r=r->next; i++; }
       }
       return( r );
    }
    else 
       return( (Rng_Spec*) NULL);
}

int RS_to_LDR( Rng_Spec* r, FILE* out, int mode ) 
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    desc_rec *d = &(r->desc);
    int nitems;
    int i, off;
    unsigned int *lb;

    lb = (unsigned int *) buf;

    if (r == (Rng_Spec*) NULL) return(FALSE);

       nitems=r->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          RS_to_LDR(r, out, 1);
          r->desc.length=get_rec_length();
          printf("\n Calculated record length for Range Spectra %ld\n",r->desc.length);
          nitems=r->desc.length;
          reset_rec_length();
       }

       i=d->rec_seq;
       zero_set(buf,nitems);

       if ( (current=GetCurrentSARL()) != NULL )
          Check_Rec_Seq( &i, buf, current, 0);
       else
	    *lb = htonl( i );
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
	lb[2] = htonl( nitems );
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LZ1SUB, LZTYPE, LZ2SUB, LZ3SUB) ) {
          printf("\n Descriptor record and subtype mismatch - expecting  RS codes");
       }

       put_I4(r->seq_num, "%4d", 4, (buf+12));
       put_I4(r->sar_chan, "%4d", 4, (buf+16));
       put_I4(r->n_dset, "%8d", 8, (buf+20));
       put_I4(r->dset_size, "%8d", 8, (buf+28));
       put_I4(r->req_recs, "%4d", 4, (buf+36));
       put_I4(r->table_no, "%4d", 4, (buf+40));
       put_I4(r->n_pixels, "%8d", 8, (buf+44));
       put_I4(r->pixel_offset, "%8d", 8, (buf+52));
       put_I4(r->n_lines, "%8d", 8, (buf+60));
       put_F4(r->first_freq, "%16.7f", 16, (buf+68));
       put_F4(r->last_freq, "%16.7f", 16, (buf+84));
       put_F4( r->min_power, "%16.7f", 16, (buf+100));
       put_F4(r->max_power, "%16.7f", 16, (buf+116));
       put_blanks( (char*) (buf+132), 32);      /* spare */
       put_I4(r->n_bins, "%8d", 8, (buf+164));

       off=172;
       for (i=0; i<r->n_bins; i++, off +=16) 
           put_F4(r->data_values_spec[i], "%16.7f", 16, (buf+off));

       /* Ensure that 256 entries were filled */
       for (;i<256; i++, off +=16) 
           put_blanks((char *)(buf+off), 16);

       put_blanks((char *)(buf+off), 1052); off+= 1052;  /* spare */

       if ( out != (FILE *) NULL) 
	      write_record( out, buf, sizeof(unsigned char),  nitems);


    if (Debug && !get_recalc_status()) {

    /* Write the contents of the Range Spectra record  */

        COM_entry = RS_entry;
       printf("\n**************** begin of Range Spectra record *******************\n");
       printf("\n RECORD SEQUENCE\t%ld", d->rec_seq);
       printf("\n RECORD SUB 1\t\t%d",d->rec_sub1);
       printf("\n RECORD TYPE\t\t%d",d->rec_type);
       printf("\n RECORD SUB 2\t\t%d",d->rec_sub2);
       printf("\n RECORD SUB 3\t\t%d",d->rec_sub3);
       printf("\n RECORD LENGTH\t\t%ld\n",d->length);
       printfd("\n RANGE SPECTRA SEQ NUM\t\t%d",r->seq_num);
       printfd("\n SAR CHANNEL ID\t\t\t%d",r->sar_chan);
       printfl("\n TBL DATA SET NUM IN RECORD\t%ld",r->n_dset);
       printfl("\n TBL DATA SET SIZE\t\t%ld",r->dset_size);
       printfd("\n DATA SETS REQ FOR FULL TABLE\t%d",r->req_recs);
       printfd("\n FULL TBL SEQ NUM OF TBL IN REC\t%d",r->table_no);
       printfl("\n TTL SAMP NUM IN RNG DIR\t%ld",r->n_pixels);
       printfl("\n NUM SAMP OFFSET FROM 1ST SAMP RNG LIN\t%ld",r->pixel_offset);
       printfl("\n NUM RNG LINES INTEGRATED\t%ld",r->n_lines);
       printff("\n CTR FREQ OF FIRST SPEC BIN\t%16.7f",r->first_freq);
       printff("\n CTR FREQ OF LAST SPEC BIN\t%16.7f",r->last_freq);
       printff("\n MIN SPECTRAL PWR\t\t%16.7f",r->min_power);
       printff("\n MAX SPECTRAL PWR\t\t%16.7f",r->max_power);
       printfl("\n NUM OF FREQ BINS IN TABLE\t%ld",r->n_bins);

       for (i=0; i<r->n_bins; i++) 
           printfdf("\n DATA VALUES #%d\t\t%16.7f",i+1,r->data_values_spec[i]);
       printf("\n**************** end of Range Spectra record *******************\n");
    }

    if (r->next != (Rng_Spec *) NULL && !mode) RS_to_LDR( r->next, out, mode );
    return(TRUE);

}

Digital_Elev* Get_L_DE( SARL_ptr* t )
{
    if (t != NULL ) {
       return( t->elevation );
    }
    else 
       return( (Digital_Elev*) NULL);
}

Digital_Elev* Get_DE_Num( SARL_ptr* t, int num )
{
    Sarl_Desc_Rec* d;
    Digital_Elev* r;
    int i=1;

    if (t != NULL) {
       d = &(t->descript);
       if (d->n_dem_desc > 0 && num > 0 && num <= d->n_dem_desc) {
          r = t->elevation;
          while (i<num) { r=r->next; i++; }
       }
       return( r );
    }
    else 
       return( (Digital_Elev*) NULL);
}

int DE_to_LDR( Digital_Elev* e, FILE* out, int mode ) 
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    desc_rec *d = &(e->desc);
    Dem_Desc *set;
    Corner_Pts *pts;
    int nitems;
    int i, off;
    unsigned int *lb;

    lb = (unsigned int *) buf;

    if (e == (Digital_Elev*) NULL) return(FALSE);

       nitems=e->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          DE_to_LDR(e, out, 1);
          e->desc.length=get_rec_length();
          printf("\n Calculated record length for Digital Elevation %ld\n",e->desc.length);
          nitems=e->desc.length;
          reset_rec_length();
       }
          
       i=d->rec_seq;
       zero_set(buf,nitems);

       if ( (current=GetCurrentSARL()) != NULL )
          Check_Rec_Seq( &i, buf, current, 0);
       else
	    *lb = htonl( i );
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
	lb[2] = htonl( nitems );
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LE1SUB, LETYPE, LE2SUB, LE3SUB) ) {
          printf("\n Descriptor record and subtype mismatch - expecting  DE codes");
       }

       put_I4(e->seq_num, "%4d", 4, (buf+12));
       put_I4(e->ttl_num_sets, "%8d", 8, (buf+16));
       put_I4(e->DEM_seq_num, "%4d", 4, (buf+24));
       put_chars((char *) (buf+28), e->source_DEM, 32);
       put_chars((char *) (buf+60), e->HT_ref_name, 32);
       put_chars((char *) (buf+92), e->gen_method, 32);
       put_chars((char *) (buf+124), e->raster_unit, 12);
       put_chars((char *) (buf+136), e->presentation_proj, 32);
       put_F4(e->NS_raster, "%16.7f", 16, (buf+168));
       put_F4(e->EW_raster, "%16.7f", 16, (buf+184));
       put_chars((char *) (buf+200), e->resample, 32);
       put_F4(e->height_err, "%16.7f", 16, (buf+232));
       put_F4(e->NS_loc_err, "%16.7f", 16, (buf+248));
       put_F4(e->EW_loc_err, "%16.7f", 16, (buf+264));
       put_F4(e->max_height, "%16.7f", 16, (buf+280));
       put_F4(e->min_height, "%16.7f", 16, (buf+296));
       put_F4(e->MEAN_height, "%16.7f", 16, (buf+312));
       put_F4(e->STD_height, "%16.7f", 16, (buf+328));
       put_I4(e->num_polys, "%4d", 4, (buf+344));    

       off = 348;
       set= e->set;
       while (set!=NULL) {
           put_I4(set->poly_seq_num, "%4d", 4, (buf+off)); off+=4;
           put_I4(set->num_crnr_pts, "%4d", 4, (buf+off)); off+=4;
           put_blanks((char *) (buf+off), 8); off+=8;

           pts = set->pts;
           while (pts!=NULL) {
               put_F4(pts->cp_lat_1, "%16.7f", 16, (buf+off)); off+=16;
               put_F4(pts->cp_lon_1,"%16.7f", 16, (buf+off)); off+=16;
               pts=pts->next;
           }
           set=set->next;
       }
       
       put_blanks ((char *) (buf+492), 532);     /* spare */

       if ( out != (FILE *) NULL) 
            write_record( out, buf, sizeof(unsigned char),  nitems);

    if (Debug && !get_recalc_status()) {

       /* Write the contents of the Digital Elevation record  */

        COM_entry = DE_entry;
       printf("\n**************** begin of Digital Elevation record *******************\n");
       printf("\n RECORD SEQUENCE\t%ld", d->rec_seq);
       printf("\n RECORD SUB 1\t\t%d",d->rec_sub1);
       printf("\n RECORD TYPE\t\t%d",d->rec_type);
       printf("\n RECORD SUB 2\t\t%d",d->rec_sub2);
       printf("\n RECORD SUB 3\t\t%d",d->rec_sub3);
       printf("\n RECORD LENGTH\t\t%ld\n",d->length);
       printfd("\n DIGITAL ELEVATION SEQ NUM\t%d",e->seq_num);
       printfl("\n TTL NUM OF DEM DESC. DATASETS%ld",e->ttl_num_sets);
       printfd("\n SEQ NUM THIS DEM\t%d",e->DEM_seq_num);
       printfs("\n SOURCE OF DEM\t\t%s",e->source_DEM);
       printfs("\n HT DATUM REF NAME\t%s",e->HT_ref_name);
       printfs("\n GENERATION METHOD\t%s",e->gen_method);
       printfs("\n RASTER SPACING UNIT\t%s",e->raster_unit);
       printfs("\n PRESENTATION PROJECTION\t%s",e->presentation_proj);
       printff("\n RASTER SPACING N/S\t\t%16.7f",e->NS_raster);
       printff("\n RASTER SPACING E/W\t\t%16.7f",e->EW_raster);
       printfs("\n REAMPLING METHOD\t%s",e->resample);
       printff("\n HEIGHT ERROR\t\t%16.7f",e->height_err);
       printff("\n LOCATION ERROR N/S\t%16.7f",e->NS_loc_err);
       printff("\n LOCATION ERROR E/W\t%16.7f",e->EW_loc_err);
       printff("\n MAX. HEIGHT\t\t%16.7f",e->max_height);
       printff("\n MIN. HEIGHT\t\t%16.7f",e->min_height);
       printff("\n MEAN HEIGHT\t\t%16.7f",e->MEAN_height);
       printff("\n STD HEIGHT\t\t%16.7f",e->STD_height);
       printfd("\n NUM OF POLYGONS\t\t%d",e->num_polys);    

       set= e->set;
       while (set!=NULL) {
           printfd("\n POLYGON SEQ NUM\t%d",set->poly_seq_num);
           printfd("\n NUM OF CORNER PTS\t%d",set->num_crnr_pts);

           pts = set->pts ;
           i=1;
           while (pts!=NULL) {
               printfdf("\n CP_LAT_1[%d]\t\t%16.7f",i,pts->cp_lat_1);
               printfdf("\n CP_LON_1[%d]\t\t%16.7f",i,pts->cp_lon_1);
               pts=pts->next;
               i++;
           }
           set=set->next;
       }
       printf("\n**************** end of  Digital Elevation record *******************\n");
    }

    if (e->next != (Digital_Elev *) NULL && !mode) DE_to_LDR( e->next, out, mode );
    return(TRUE);
}

Proc_Parm* Get_L_DP( SARL_ptr* t )
{
    if (t != NULL ) {
       return( t->detail );
    }
    else 
       return( (Proc_Parm*) NULL);
}

Proc_Parm* Get_DP_Num( SARL_ptr* t, int num )
{
    Sarl_Desc_Rec* d;
    Proc_Parm* r;
    int i=1;

    if (t != NULL) {
       d = &(t->descript);
       if (d->n_det_proc > 0 && num > 0 && num <= d->n_det_proc) {
          r = t->detail;
          while (i<num) { r=r->next; i++; }
       }
       return( r );
    }
    else 
       return( (Proc_Parm*) NULL);
}


int DP_to_LDR( Proc_Parm* e, FILE* out, int mode ) 
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    Beam_Info    *bi;
    Pix_Count    *pc;
    Temp_Rec     *tr;
    Dopcen_Est   *de;
    SRGR_Coefset *sc;
    desc_rec *d = &(e->desc);
    int nitems;
    int off;
    int i=0;
    unsigned int *lb;

    lb = (unsigned int *) buf;
    
    if (e == (Proc_Parm*) NULL) return(FALSE);

    nitems=e->desc.length;
    /* user requests calculation of record length */
    if (nitems==0 && !get_recalc_status()) {
       recalc_length();
       DP_to_LDR(e, out, 1);
       e->desc.length=get_rec_length();
       printf("\n Calculated record length for Detail Processing %ld\n",e->desc.length);
       nitems=e->desc.length;
       reset_rec_length();
    }
          
    i=d->rec_seq;
    zero_set(buf,nitems);

    if ( (current=GetCurrentSARL()) != NULL )
       Check_Rec_Seq( &i, buf, current, 0);
    else
	    *lb = htonl( i );
    put_byte((buf+4), d->rec_sub1);
    put_byte((buf+5), d->rec_type);
    put_byte((buf+6), d->rec_sub2);
    put_byte((buf+7), d->rec_sub3);
	lb[2] = htonl( nitems );
    tb=&buf[4];
    if ( !MATCH_RECORD(tb, LY1SUB, LYTYPE, LY2SUB, LY3SUB) ) {
       printf("\n Descriptor record and subtype mismatch - expecting DP codes");
    }
       off = 12;
       put_I4(e->seq_num, "%4d", 4, (buf+off)); off+=4;
       put_chars((char *) (buf+off), e->spare_dpp_1, 4);  off+=4;
       put_chars((char *) (buf+off),  e->inp_media, 3); off+=3;
       put_I4(e->n_tape_id, "%4d", 4, (buf+off)); off+=4;
       for (i=0; i<10; i++, off+=8) {
	   put_chars((char *) (buf+off), e->tape_id[i], 8);
       }
       put_chars((char *) (buf+off), e->exp_ing_start, 21); off+=21;
       put_chars((char *) (buf+off), e->exp_ing_stop, 21); off+=21;
       put_chars((char *) (buf+off), e->act_ing_start, 21); off+=21;
       put_chars((char *) (buf+off), e->act_ing_stop, 21); off+=21;
       put_chars((char *) (buf+off), e->proc_start, 21); off+=21;
       put_chars((char *) (buf+off), e->proc_stop, 21); off+=21;
       for (i=0;i<10; i++, off+=16) {
	   put_F4(e->mn_sig_lev[i], "%16.7f", 16, (buf+off) );
       }
       put_I4( e->src_data_ind, "%4d", 4, (buf+off)); off+=4;
       put_I4(e->miss_ln, "%8d", 8, (buf+off)); off+=8;
       put_I4(e->rej_ln, "%8d", 8, (buf+off)); off+=8;
       put_I4(e->large_gap, "%8d", 8, (buf+off)); off+=8;
       put_F4(e->bit_error_rate, "%16.7f", 16, (buf+off)); off+=16;
       put_F4(e->fm_crc_err, "%16.7f", 16, (buf+off)); off+=16;
       put_I4(e->date_incons, "%8d", 8, (buf+off)); off+=8;
       put_I4(e->prf_changes, "%8d", 8, (buf+off)); off+=8;
       put_I4(e->delay_changes, "%8d", 8, (buf+off)); off+=8;
       put_I4(e->skipd_frams, "%8d", 8, (buf+off)); off+=8;
       put_I4(e->rej_bf_start, "%8d", 8, (buf+off)); off+=8;
       put_I4(e->rej_few_fram, "%8d", 8, (buf+off)); off+=8;
       put_I4(e->rej_many_fram, "%8d", 8, (buf+off)); off+=8;
       put_I4(e->rej_mchn_err, "%8d", 8, (buf+off)); off+=8;
       put_I4(e->rej_vchn_err, "%8d", 8, (buf+off)); off+=8;
       put_I4(e->rej_rec_type, "%8d", 8, (buf+off)); off+=8;
       put_I4(e->prd_qual_ind, "%8d", 8, (buf+off)); off+=4;
       put_chars((char *) (buf+off), e->qc_rating, 6); off+=6;
       put_chars((char *) (buf+off), e->qc_comment, 80); off+=80;
       put_chars((char *) (buf+off), e->sens_config, 10); off+=10;
       put_chars((char *) (buf+off), e->sens_orient, 9); off+=9;
       put_chars((char *) (buf+off), e->sych_marker, 8); off+=8;
       put_chars((char *) (buf+off), e->rng_ref_src, 12); off+=12;
       for (i=0;i<4; i++, off+=16) {
	   put_F4(e->rng_amp_coef[i], "%16.7f", 16, (buf+off));
       }
       for (i=0;i<4; i++, off+=16) {
	   put_F4(e->rng_phas_coef[i], "%16.7f", 16, (buf+off));
       }
       for (i=0;i<4; i++, off+=16) {
	   put_F4(e->err_amp_coef[i], "%16.7f", 16, (buf+off));
       }
       for (i=0;i<4; i++, off+=16) {
	   put_F4(e->err_phas_coef[i], "%16.7f", 16, (buf+off));
       }
       put_I4(e->pulse_bandw, "%4d", 4, (buf+off) ); off+=4;
       put_chars((char *) (buf+off), e->adc_samp_rate, 5); off+=5;
       put_F4(e->rep_agc_attn, "%16.7f", 16, (buf+off)); off+=16;
       put_F4(e->gn_corctn_fctr, "%16.7f", 16, (buf+off)); off+=16;
       put_F4(e->rep_energy_gn, "%16.7f", 16, (buf+off)); off+=16;
       put_chars((char *) (buf+off), e->orb_data_src, 11); off+=11;
       put_I4(e->pulse_cnt_1, "%4d", 4, (buf+off) ); off+=4;
       put_I4(e->pulse_cnt_2, "%4d", 4, (buf+off) ); off+=4;
       put_chars((char *) (buf+off), e->beam_edge_rqd, 3); off+=3;
       put_F4(e->beam_edge_conf, "%16.7f", 16, (buf+off)); off+=16;
       put_I4(e->pix_overlap, "%4d", 4, (buf+off) ); off+=4;
       put_I4(e->n_beams, "%4d", 4, (buf+off) ); off+=4;
    
       bi = e->beam_info;
       while ( bi != NULL) {
	   put_chars((char *) (buf+off), bi->beam_type, 3); off+=3;
	   put_chars((char *) (buf+off), bi->beam_look_src, 9); off+=9;
           put_F4(bi->beam_look_ang, "%16.7f", 16, (buf+off)); off+=16;
           put_F4(bi->prf, "%16.7f", 16, (buf+off)); off+=16;
	   bi =  bi->next;
       }
    
       put_I4(e->n_pix_updates, "%4d", 4, (buf+off) ); off+=4;
       pc = e->pix_count;
       while ( pc != NULL) {
	   put_chars((char *) (buf+off), pc->pix_update, 21); off+=21;
	   for (i=0;i<4;i++,off+=8) 
               put_I4(pc->n_pix[i], "%8d", 8, (buf+off)); 
	   pc = pc->next;
       }
       put_F4(e->pwin_start, "%16.7f", 16, (buf+off)); off+=16;
       put_F4(e->pwin_end, "%16.7f", 16, (buf+off)); off+=16;
       put_chars((char *) (buf+off), e->recd_type, 9); off+=9;
       put_F4(e->temp_set_inc, "%16.7f", 16, (buf+off)); off+=16;
    
       put_I4(e->n_temp_set, "%4d", 4, (buf+off) ); off+=4;
       tr = e->temp;
       while ( tr != NULL) {
          for (i=0;i<4;i++,off+=4) 
              put_I4(tr->temp_set[i], "%4d", 4, (buf+off));
	  tr= tr->next;
       }
    
       put_I4(e->n_image_pix, "%8d", 8, (buf+off)); off+=8;
       put_F4(e->prc_zero_pix, "%16.7f", 16, (buf+off)); off+=16;
       put_F4(e->prc_satur_pix, "%16.7f", 16, (buf+off)); off+=16;
       put_F4(e->img_hist_mean, "%16.7f", 16, (buf+off)); off+=16;
       put_F4(e->img_cumu_dist[0], "%16.7f", 16,(buf+off)); off+=16;
       put_F4(e->img_cumu_dist[1], "%16.7f", 16, (buf+off)); off+=16;
       put_F4(e->img_cumu_dist[2], "%16.7f", 16, (buf+off)); off+=16;
       put_F4(e->pre_img_gn, "%16.7f", 16, (buf+off)); off+=16;
       put_F4(e->post_img_gn, "%16.7f", 16, (buf+off)); off+=16;
       put_F4(e->dopcen_inc, "%16.7f", 16, (buf+off)); off+=16;
    
       put_I4(e->n_dopcen, "%4d", 4, (buf+off) ); off+=4;
       de = e->dopcen_est;
       while ( de != NULL) {
           put_F4(de->dopcen_conf, "%16.7f", 16, (buf+off)); off+=16;
           put_F4(de->dopcen_ref_tim, "%16.7f", 16, (buf+off)); off+=16;
           for (i=0;i<4;i++,off+=16) 
               put_F4(de->dopcen_coef[i], "%16.7f", 16, (buf+off)); off+=16;
	   de = de->next;
       }
       put_I4(e->dopamb_err, "%4d", 4, (buf+off)); off+=4;
       put_F4(e->dopamb_conf, "%16.7f", 16, (buf+off)); off+=16;
       for (i=0;i<7;i++,off+=16) 
            put_F4(e->eph_orb_data[i], "%16.7f", 16, (buf+off));
       put_chars( (char *) (buf+off), e->appl_type, 12); off+=12;  
       put_F4(e->first_lntim, "%22.15f", 22, (buf+off)); off+=22;
       put_F4(e->lntim_inc, "%22.15f", 22, (buf+off)); off+=22;
    
       put_I4(e->n_srgr, "%4d", 4, (buf+off)); off+=4;
       sc = e->srgr_coefset;
       while( sc != NULL) {
           put_chars( (char *) (buf+off), sc->srgr_update, 21); off+=21;
           for (i=0;i<6;i++,off+=16) 
	       put_F4(sc->srgr_coef[i], "%16.7f", 16, (buf+off));
	sc = sc->next;
       }
    
       put_F4(e->pixel_spacing, "%16.7f", 16, (buf+off)); off+=16;
       put_chars( (char *) (buf+off), e->pics_reqd, 3); off+=3; 
       put_chars( (char *) (buf+off), e->wo_number, 8); off+=8; 
       put_chars( (char *) (buf+off), e->wo_date, 20); off+=20; 
       put_chars( (char *) (buf+off), e->satellite_id, 10); off+=10; 
       put_chars( (char *) (buf+off), e->user_id, 20); off+=20; 
       put_chars( (char *) (buf+off), e->complete_msg, 3); off+=3; 
       put_chars( (char *) (buf+off), e->scene_id, 15); off+=15; 
       put_chars( (char *) (buf+off), e->density_in, 4); off+=4; 
       put_chars( (char *) (buf+off), e->media_id, 8); off+=8;
       put_F4(e->angle_first, "%16.7f", 16, (buf+off)); off+=16;
       put_F4(e->angle_last, "%16.7f", 16, (buf+off)); off+=16;
       put_chars( (char *) (buf+off), e->prod_type, 3); off+=3; 
       put_chars( (char *) (buf+off), e->map_system,   16); off+=16; 
       put_F4(e->centre_lat, "%22.15f", 22, (buf+off)); off+=22;  
       put_F4(e->centre_long, "%22.15f", 22, (buf+off)); off+=22;  
       put_F4(e->span_x,  "%22.15f", 22, (buf+off)); off+=22;  
       put_F4(e->span_y, "%22.15f", 22, (buf+off)); off+=22; 
       put_chars( (char *) (buf+off), e->apply_dtm, 3); off+=3; 
       put_chars( (char *) (buf+off), e->density_out, 4); off+=4; 
       put_chars( (char *) (buf+off), e->spare_dpp_2, 247); off+=247;
      
    if ( out != (FILE *) NULL) 
       write_record( out, buf, sizeof(unsigned char),  nitems);

    if (Debug && !get_recalc_status()) {

       /* Write the contents of the Detail Processing record  */

        COM_entry = DP_entry;
       printf("\n**************** begin of Detail Processing record *******************\n");
       printf("\n RECORD SEQUENCE\t%ld", d->rec_seq);
       printf("\n RECORD SUB 1\t\t%d",d->rec_sub1);
       printf("\n RECORD TYPE\t\t%d",d->rec_type);
       printf("\n RECORD SUB 2\t\t%d",d->rec_sub2);
       printf("\n RECORD SUB 3\t\t%d",d->rec_sub3);
       printf("\n RECORD LENGTH\t\t%ld\n",d->length);
       printfd("\n DETAIL PROCESSING SEQ NUM\t%d",e->seq_num);
       
       printfd("\n SEQ NUM\t\t%d", e->seq_num);
       printfs("\n SPARE_DPP_1\t\t%s", e->spare_dpp_1);
       printfs("\n INP_MEDIA\t\t%s", e->inp_media);
       printfd("\n N_TAPE_ID\t\t%d", e->n_tape_id);
       for (i=0; i<10; i++) {
	   printfs("\n TAPE_ID\t\t%s", e->tape_id[i]);
       }
       printfs("\n EXP_ING_START\t%s", e->exp_ing_start);
       printfs("\n EXP_ING_STOP\t%s", e->exp_ing_stop);
       printfs("\n ACT_ING_START\t%s", e->act_ing_start);
       printfs("\n ACT_ING_STOP\t%s", e->act_ing_stop);
       printfs("\n PROC_START\t%s", e->proc_start); 
       printfs("\n PROC_STOP\t\t%s", e->proc_stop);
       for (i=0;i<10; i++) {
	   printff("\n MN_SIG_LEV\t%16.7f", e->mn_sig_lev[i]);
       }
       printfd("\n SRC_DATA_IND\t%d", e->src_data_ind);
       printfl("\n MISS_LN\t\t%ld", e->miss_ln);
       printfl("\n REJ_LN\t\t%ld", e->rej_ln); 
       printfl("\n LARGE_GAP\t\t%ld", e->large_gap); 
       printff("\n BIT_ERROR_RATE\t%16.7f", e->bit_error_rate);
       printff("\n FM_CRC_ERR\t%16.7f", e->fm_crc_err);
       printfl("\n DATE_INCONS\t%ld", e->date_incons);
       printfl("\n PRF_CHANGES\t%ld", e->prf_changes);
       printfl("\n DELAY_CHANGES\t%ld", e->delay_changes);
       printfl("\n SKIPD_FRAMS\t%ld", e->skipd_frams); 
       printfl("\n REJ_BF_START\t%ld", e->rej_bf_start); 
       printfl("\n REJ_FEW_FRAM\t%ld", e->rej_few_fram); 
       printfl("\n REJ_MANY_FRAM\t%ld", e->rej_many_fram);
       printfl("\n REJ_MCHN_ERR\t%ld", e->rej_mchn_err); 
       printfl("\n REJ_VCHN_ERR\t%ld", e->rej_vchn_err);
       printfl("\n REJ_REC_TYPE\t%ld", e->rej_rec_type); 
       printfl("\n PRD_QUAL_IND\t%ld", e->prd_qual_ind);
       printfs("\n QC_RATING\t\t%s", e->qc_rating); 
       printfs("\n QC_COMMENT\t\t%s", e->qc_comment);
       printfs("\n SENS_CONFIG\t%s", e->sens_config); 
       printfs("\n SENS_ORIENT\t%s", e->sens_orient);
       printfs("\n SYCH_MARKER\t%s", e->sych_marker);
       printfs("\n RNG_REF_SRC\t%s", e->rng_ref_src);
       for (i=0;i<4; i++) {
	   printff("\n RNG_AMP_COEF\t%16.7f", e->rng_amp_coef[i]);
       }
       for (i=0;i<4; i++) {
	   printff("\n RNG_PHAS_COEF\t%16.7f", e->rng_phas_coef[i]);
       }
       for (i=0;i<4; i++) {
	   printff("\n ERR_AMP_COEF\t%16.7f", e->err_amp_coef[i]);
       }
       for (i=0;i<4; i++) {
	   printff("\n ERR_PHAS_COEF\t%16.7f", e->err_phas_coef[i]);
       }
       printfl("\n PULSE_BANDW\t%ld", e->pulse_bandw); 
       printfs("\n ADC_SAMP_RATE\t%s", e->adc_samp_rate);
       printff("\n REP_AGC_ATTN\t%16.7f", e->rep_agc_attn);
       printff("\n GN_CORCTN_FCTR\t%16.7f", e->gn_corctn_fctr);
       printff("\n REP_ENERGY_GN\t%16.7f", e->rep_energy_gn);
       printfs("\n ORB_DATA_SRC\t%s", e->orb_data_src);
       printfl("\n PULSE_CNT_1\t%ld", e->pulse_cnt_1);
       printfl("\n PULSE_CNT_2\t%ld", e->pulse_cnt_2);
       printfs("\n BEAM_EDGE_RQD\t%s", e->beam_edge_rqd);
       printff("\n BEAM_EDGE_CONF\t%16.7f", e->beam_edge_conf);
       printfl("\n PIX_OVERLAP\t%ld", e->pix_overlap);
       printfl("\n N_BEAMS\t\t%ld", e->n_beams);
    
       bi = e->beam_info;
       while ( bi != NULL) {
	   printfs("\n BEAM_TYPE\t\t%s", bi->beam_type);
	   printfs("\n BEAM_LOOK_SRC\t%s", bi->beam_look_src);
           printff("\n BEAM_LOOK_ANG\t%16.7f", bi->beam_look_ang);
           printff("\n PRF\t\t%16.7f", bi->prf);
	   bi =  bi->next;
       }
    
       printfl("\n N_PIX_UPDATES\t%ld", e->n_pix_updates);
       pc = e->pix_count;
       while ( pc != NULL) {
	   printfs("\n PIX_UPDATE\t%s", pc->pix_update);
	   for (i=0;i<4;i++) 
               printfl("\n N_PIX\t\t%ld", pc->n_pix[i]); 
	   pc = pc->next;
       }
       printff("\n PWIN_START\t%16.7f", e->pwin_start);
       printff("\n PWIN_END\t\t%16.7f", e->pwin_end);
       printfs("\n RECD_TYPE\t\t%s", e->recd_type);
       printff("\n TEMP_SET_INC\t%16.7f", e->temp_set_inc);
    
       printfl("\n N_TEMP_SET\t%ld", e->n_temp_set);
       tr = e->temp;
       while ( tr != NULL) {
          for (i=0;i<4;i++) 
              printfl("\n TEMP_SET\t\t%ld", tr->temp_set[i]);
	  tr= tr->next;
       }
    
       printfl("\n N_IMAGE_PIX\t%ld", e->n_image_pix); 
       printff("\n PRC_ZERO_pix\t%16.7f", e->prc_zero_pix); 
       printff("\n PRC_SATUR_PIX\t%16.7f", e->prc_satur_pix); 
       printff("\n IMG_HIST_MEAN\t%16.7f", e->img_hist_mean); 
       printff("\n IMG_CUMU_DIST\t%16.7f", e->img_cumu_dist[0]);
       printff("\n IMG_CUMU_DIST\t%16.7f", e->img_cumu_dist[1]);
       printff("\n IMG_CUMU_DIST\t%16.7f", e->img_cumu_dist[2]); 
       printff("\n PRE_IMG_GN\t%16.7f", e->pre_img_gn); 
       printff("\n POST_IMG_GN\t%16.7f", e->post_img_gn); 
       printff("\n DOPCEN_INC\t%16.7f", e->dopcen_inc); 
    
       printfl("\n N_DOPCEN\t%ld", e->n_dopcen);
       de = e->dopcen_est;
       while ( de != NULL) {
           printff("\n DOPCEN_CONF\t%16.7f", de->dopcen_conf);
           printff("\n DOPCEN_REF_TIM\t%16.7f", de->dopcen_ref_tim);
           for (i=0;i<4;i++) 
               printff("\n DOPCEN_COEF\t%16.7f", de->dopcen_coef[i]);
	   de = de->next;
       }
       printfl("\n DOPAMB_ERR\t%ld", e->dopamb_err);
       printff("\n DOPAMB_CONF\t%16.7f", e->dopamb_conf); 
       for (i=0;i<7;i++) 
            printff("\n EPH_ORB_DATA\t%16.7f", e->eph_orb_data[i]);
       printfs("\n APPL_TYPE\t%s", e->appl_type); 
       printff("\n FIRST_LNTIM\t%22.15f", e->first_lntim);
       printff("\n LNTIM_INC\t\t%22.15f", e->lntim_inc);
    
       printfl("\n N_SRGR\t\t%ld", e->n_srgr);
       sc = e->srgr_coefset;
       while( sc != NULL) {
           printfs("\n SRGR_UPDATE\t%s", sc->srgr_update);
           for (i=0;i<6;i++) 
	       printff("\n SRGR_COEF\t\t%16.7f", sc->srgr_coef[i]);
	sc = sc->next;
       }
    
       printff("\n PIXEL_SPACING\t%16.7f", e->pixel_spacing);
       printfs("\n PICS_REQD\t%s", e->pics_reqd);
       printfs("\n WO_NUMBER\t%s", e->wo_number); 
       printfs("\n WO_DATE\t\t%s", e->wo_date);
       printfs("\n SATELLITE_ID\t%s", e->satellite_id); 
       printfs("\n USER_ID\t\t%s", e->user_id);
       printfs("\n COMPLETE_MSG\t%s", e->complete_msg);
       printfs("\n SCENE_ID\t\t%s", e->scene_id); 
       printfs("\n DENSITY_IN\t%s", e->density_in); 
       printfs("\n MEDIA_ID\t%s", e->media_id);
       printff("\n ANGLE_FIRST\t%16.7f", e->angle_first);
       printff("\n ANGLE_LAST\t%16.7f", e->angle_last);
       printfs("\n PROD_TYPE\t%s", e->prod_type); 
       printfs("\n MAP_SYSTEM\t%s", e->map_system); 
       printff("\n CENTRE_LAT\t%22.15f", e->centre_lat); 
       printff("\n CENTRE_LONG\t%22.15f", e->centre_long);  
       printff("\n SPAN_X\t\t%22.15f", e->span_x);  
       printff("\n SPAN_Y\t\t%22.15f", e->span_y); 
       printfs("\n APPLY_DTM\t\t%s", e->apply_dtm);
       printfs("\n DENSITY_OUT\t%s", e->density_out); 
       printfs("\n SPARE_DPP_2\t\t%s", e->spare_dpp_2);

       printf("\n**************** end of  Detail Processing record *******************\n");
    }

    if (e->next != (Proc_Parm *) NULL && !mode) DP_to_LDR( e->next, out, mode );
    return(TRUE);
}

/* support display od calibration data record in trailer file */
Calib_Data* Get_L_CD( SARL_ptr* t )
{
    if (t != NULL ) {
       return( t->calibration );
    }
    else 
       return( (Calib_Data*) NULL);
}

Calib_Data* Get_CD_Num( SARL_ptr* t, int num )
{
    Sarl_Desc_Rec* d;
    Calib_Data* r;
    int i=1;

    if (t != NULL) {
       d = &(t->descript);
       if (d->n_cal > 0 && num > 0 && num <= d->n_cal) {
          r = t->calibration;
          while (i<num) { r=r->next; i++; }
       }
       return( r );
    }
    else 
       return( (Calib_Data*) NULL);
}


int CD_to_LDR( Calib_Data* e, FILE* out, int mode ) 
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    Beam_Info    *bi;
    Pix_Count    *pc;
    Temp_Rec     *tr;
    Dopcen_Est   *de;
    SRGR_Coefset *sc;
    desc_rec *d = &(e->desc);
    int nitems;
    int off;
    int i=0;
    unsigned int *lb;

    lb = (unsigned int *) buf;
    
    if (e == (Calib_Data*) NULL) return(FALSE);

    nitems=e->desc.length;
    /* user requests calculation of record length */
    if (nitems==0 && !get_recalc_status()) {
       recalc_length();
       CD_to_LDR(e, out, 1);
       e->desc.length=get_rec_length();
       printf("\n Calculated record length for Calibration Data %ld\n",e->desc.length);
       nitems=e->desc.length;
       reset_rec_length();
    }
          
    i=d->rec_seq;
    zero_set(buf,nitems);

    if ( (current=GetCurrentSARL()) != NULL )
       Check_Rec_Seq( &i, buf, current, 0);
    else
       *lb = htonl( i );
    put_byte((buf+4), d->rec_sub1);
    put_byte((buf+5), d->rec_type);
    put_byte((buf+6), d->rec_sub2);
    put_byte((buf+7), d->rec_sub3);
        lb[2] = htonl( nitems );
    tb=&buf[4];
    if ( !MATCH_RECORD(tb, LB1SUB, LBTYPE, LB2SUB, LB3SUB) ) {
       printf("\n Descriptor record and subtype mismatch - expecting CD codes");
    }
       off = 12;
       put_I4(e->seq_num, "%4d", 4, (buf+off)); off+=4;
       put_chars((char *) (buf+off), e->spare_cdr_1, 4);  off+=4;
       put_chars( (char *) (buf+off), e->spare_cdr_2, 255); off+=255;
      
    if ( out != (FILE *) NULL) 
       write_record( out, buf, sizeof(unsigned char),  nitems);

    if (Debug && !get_recalc_status()) {

       /* Write the contents of the Calibration Data record  */

        COM_entry = CD_entry;
       printf("\n**************** begin of Calibration Data record *******************\n");
       printf("\n RECORD SEQUENCE\t%ld", d->rec_seq);
       printf("\n RECORD SUB 1\t\t%d",d->rec_sub1);
       printf("\n RECORD TYPE\t\t%d",d->rec_type);
       printf("\n RECORD SUB 2\t\t%d",d->rec_sub2);
       printf("\n RECORD SUB 3\t\t%d",d->rec_sub3);
       printf("\n RECORD LENGTH\t\t%ld\n",d->length);
       printfd("\n DETAIL PROCESSING SEQ NUM\t%d",e->seq_num);
       
       printfd("\n SEQ NUM\t\t%d", e->seq_num);
       printfs("\n SPARE_DPP_1\t\t%s", e->spare_cdr_1);
       printfs("\n SPARE_DPP_2\t\t%s", e->spare_cdr_2);

       printf("\n**************** end of Calibration Data record *******************\n");
    }

    if (e->next != (Calib_Data *) NULL && !mode) CD_to_LDR( e->next, out, mode );
    return(TRUE);
}
/* end of adding display of calibration data record */

Fac_Related* Get_L_FR( SARL_ptr* t )
{
    if (t != NULL ) {
       return( t->facility );
    }
    else 
       return( (Fac_Related*) NULL);
}

Fac_Related* Get_FR_Num( SARL_ptr* t, int num )
{
    Sarl_Desc_Rec* d;
    Fac_Related* r;
    int i=1;

    if (t != NULL) {
       d = &(t->descript);
       if (d->n_fac_data > 0 && num > 0 && num <= d->n_fac_data) {
          r = t->facility;
          while (i<num) { r=r->next; i++; }
       }
       return( r );
    }
    else 
       return( (Fac_Related*) NULL);
}

int FR_to_LDR( Fac_Related* r, FILE* out, int mode ) 
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    desc_rec *d = &(r->desc);
    int nitems;
    int i;
    unsigned int *lb;

    lb = (unsigned int *) buf;

    if (r == (Fac_Related*) NULL) return(FALSE);

       nitems=r->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          FR_to_LDR(r, out, 1);
          r->desc.length=get_rec_length();
          printf("\n Calculated record length for Facility Related %ld\n",r->desc.length);
          nitems=r->desc.length;
          reset_rec_length();
       }
          
       i=d->rec_seq;
       zero_set(buf,nitems);

       if ( (current=GetCurrentSARL()) != NULL )
          Check_Rec_Seq( &i, buf, current, 0);
       else
	    *lb = htonl( i );
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
	lb[2] = htonl( nitems );
#ifndef PRE_RADARSAT
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LF1SUB, LFTYPE, LF2SUB, LF3SUB) ) {
             printf("\n Descriptor record and subtype mismatch - expecting  FR codes");
       }
#else
       if ( d->rec_type != LFTYPE ) {
          put_chars( (char *) (buf+12), r->bogus, nitems-12);
       }
#endif
       else {
          put_I4(r->seq_num, "%4d", 4, (buf+12));
          put_blanks((char *) (buf+16), 4);    /* spare */
          put_chars((char*) (buf+20), r->datatake_ID, 14);
          put_chars((char*) (buf+34), r->image_ID, 11);
          put_chars((char*) (buf+45), r->corr_year, 4);
          put_blanks((char *) (buf+49), 1);
          put_chars((char*) (buf+50), r->corr_GMT, 17);
          put_chars((char*) (buf+67), r->site_name, 33);
          put_chars((char*) (buf+100), r->data_year, 4);
          put_blanks((char *) (buf+104), 1);
          put_chars((char*) (buf+105), r->center_GMT, 17);
          put_F4(r->center_LAT, "%17.7f", 17, (buf+122));
          put_F4(r->center_LON, "%17.7f", 17, (buf+139));
          put_F4(r->near_start_LAT, "%17.7f", 17, (buf+156));
          put_F4(r->near_start_LON, "%17.7f", 17, (buf+173));
          put_F4(r->near_end_LAT, "%17.7f", 17, (buf+190));
          put_F4(r->near_end_LON, "%17.7f", 17, (buf+207));
          put_F4(r->far_start_LAT, "%17.7f", 17, (buf+224));
          put_F4(r->far_start_LON, "%17.7f", 17, (buf+241));
          put_F4(r->far_end_LAT, "%17.7f", 17, (buf+258));
          put_F4(r->far_end_LON, "%17.7f", 17, (buf+275));
          put_F4(r->actual_azimuth, "%17.7f", 17, (buf+292));
          put_F4(r->actual_range, "%17.7f", 17, (buf+309));
          put_I4(r->actual_pixels, "%9d", 9, (buf+326));
          put_I4(r->actual_lines, "%9d", 9, (buf+335));
          put_I4(r->total_pixels, "%9d", 9, (buf+344));
          put_I4(r->total_lines, "%9d", 9, (buf+353));
          put_chars((char*) (buf+362), r->media_id, 7);
          put_I4(r->start_address, "%17d", 17, (buf+369));
          put_I4(r->end_address, "%17d", 17, (buf+386));
          put_chars((char*) (buf+403), r->platform_name, 17);
          put_chars((char*) (buf+420), r->sensor_mode, 33);
          put_F4(r->PRF, "%17.7f", 17, (buf+453));
          put_F4(r->ant_look_angle, "%17.7f", 17, (buf+470));
          put_F4(r->data_rate, "%17.7f", 17, (buf+487));
          put_F4(r->data_win_pos, "%17.7f", 17, (buf+504));
          put_F4(r->range_gate_del, "%17.7f", 17, (buf+521));
          put_F4(r->track_angle, "%17.7f", 17, (buf+538));
          put_chars((char*) (buf+555), r->ASC_DESC, 2);
	  /* Add 1 from here on. */
          put_F4(r->S_C_altitude, "%17.7f", 17, (buf+557));
          put_F4(r->X_position, "%23.10E", 23, (buf+574));
          put_F4(r->Y_position, "%23.10E", 23, (buf+597));
          put_F4(r->Z_position, "%23.10E", 23, (buf+620));
          put_F4(r->X_velocity, "%23.10E", 23, (buf+643));
          put_F4(r->Y_velocity, "%23.10E", 23, (buf+666));
          put_F4(r->Z_velocity, "%23.10E", 23, (buf+689));
          put_F4(r->roll, "%15.7f", 15, (buf+712));
          put_F4(r->yaw, "%15.7f", 15, (buf+727));
          put_F4(r->pitch, "%15.7f", 15, (buf+742));
          put_I4(r->roll_flag, "%5d", 5, (buf+757));
          put_I4(r->yaw_flag, "%5d", 5, (buf+762));
          put_I4(r->pitch_flag, "%5d", 5, (buf+767));
          put_F4(r->roll_rate, "%15.7f", 15, (buf+772));
          put_F4(r->yaw_rate, "%15.7f", 15, (buf+787));
          put_F4(r->pitch_rate, "%15.7f", 15, (buf+802));
          put_I4(r->roll_rate_flag, "%5d", 5, (buf+817));
          put_I4(r->yaw_rate_flag, "%5d", 5, (buf+822));
          put_I4(r->pitch_rate_flag, "%5d", 5, (buf+827));
          put_F4(r->nadir_radius, "%17.7f", 17, (buf+832));
          put_F4(r->image_radius, "%17.7f", 17, (buf+849));
          put_F4(r->incidence_angle, "%17.7f", 17, (buf+866));       
          put_chars((char*) (buf+883), r->proc_version, 8);
          put_chars((char*) (buf+891), r->proc_type, 3);
          put_chars((char*) (buf+894), r->type_ephemeris, 2);
          put_F4(r->looks_azimuth, "%17.7f", 17, (buf+896));
          put_F4(r->looks_range, "%17.7f", 17, (buf+913));
          put_F4(r->azi_weight_fac, "%17.7f", 17, (buf+930));
          put_F4(r->range_weight_fac, "%17.7f", 17, (buf+947));
          put_chars((char*) (buf+964), r->look_energy_eq, 4);
          put_F4(r->induced_azimuth, "%17.7f", 17, (buf+968));
          put_F4(r->induced_range, "%17.7f", 17, (buf+985));
          put_F4(r->gain, "%17.7f", 17, (buf+1002));
          put_F4(r->swath_velocity, "%17.7f", 17, (buf+1019));
          put_F4(r->squint_angle, "%17.7f", 17, (buf+1036));
          put_F4(r->avg_terrain_ht, "%17.7f", 17, (buf+1053));
          put_I4(r->processor_gain, "%4d", 4, (buf+1070));
          put_chars((char*) (buf+1074), r->deskew, 4);
          put_chars((char*) (buf+1078), r->gnd_slant_flag, 7);
          put_F4(r->sl_rng_1st_pix, "%17.7f", 17, (buf+1085));
          put_F4(r->sl_rng_last_pix, "%17.7f", 17, (buf+1102));
          put_I4(r->start_sample, "%9d", 9, (buf+1119));
          put_chars((char*) (buf+1128), r->clutterlock_flg, 4);
          put_F4(r->dop_frq_const, "%17.7f", 17, (buf+1132));
          put_F4(r->dop_frq_slope, "%17.7f", 17, (buf+1149));
          put_F4(r->dop_frq_quad, "%17.7f", 17, (buf+1166));
          put_chars((char*) (buf+1183), r->autofocus_flag, 4);
          put_F4(r->dop_frq_r_cnst, "%17.7f", 17, (buf+1187));
          put_F4(r->dop_frq_r_slope, "%17.7f", 17, (buf+1204));
          put_F4(r->dop_frq_r_quad, "%17.7f", 17, (buf+1221));
          put_F4(r->azi_res, "%17.7f", 17, (buf+1238));
          put_F4(r->rng_res, "%17.7f", 17, (buf+1255));
          put_F4(r->azimuth_pixel, "%17.7f", 17, (buf+1272));
          put_F4(r->range_pixel, "%17.7f", 17, (buf+1289));
          put_chars((char*) (buf+1306), r->OBRC_flag, 4);
          put_I4(r->bits_sample, "%5d", 5, (buf+1310));
          put_F4(r->calib_est, "%17.7f", 17, (buf+1315));
          put_F4(r->bit_err_rate, "%17.7f", 17, (buf+1332));
          put_F4(r->SNR, "%17.7f", 17, (buf+1349));
          put_F4(r->est_noise_flr, "%17.7f", 17, (buf+1366));
          put_F4(r->radio_m_resol, "%17.7f", 17, (buf+1383));
          put_I4(r->satur_points, "%9d", 9, (buf+1400));
          put_chars((char*) (buf+1409), r->spec_flag, 4);
#ifndef PRE_RADARSAT
          put_F4(r->repl_agc, "%17.7f", 17, (buf+1413));
          put_F4(r->temp_rx_lna, "%17.7f", 17, (buf+1430));
          put_F4(r->temp_rx_sub, "%17.7f", 17, (buf+1447));
          put_F4(r->temp_rx_prot, "%17.7f", 17, (buf+1464));
          put_F4(r->temp_cal_sys, "%17.7f", 17, (buf+1481));
          put_F4(r->rx_agc, "%17.7f", 17, (buf+1498));
          put_F4(r->pre_cal1_pow, "%17.7f", 17, (buf+1515));
          put_F4(r->pre_cal2_pow, "%17.7f", 17, (buf+1532));
          put_F4(r->post_cal1_pow, "%17.7f", 17, (buf+1549));
          put_F4(r->post_cal2_pow, "%17.7f", 17, (buf+1566));
          put_F4(r->repl_pow, "%17.7f", 17, (buf+1583));
          put_F4(r->ssar_roll_ang, "%17.7f", 17, (buf+1600));
          put_chars((char*) (buf+1617), r->comment, 100);
#endif	  
       }
       if ( out != (FILE *) NULL)
             write_record( out, buf, sizeof(unsigned char),  nitems);

    if (Debug && !get_recalc_status()) {

       /* Write the contents of the Facility Related record  */
        COM_entry = FR_entry;
       printf("\n**************** begin of Facility Related record *******************\n");
       printf("\n RECORD SEQUENCE\t%ld", d->rec_seq);
       printf("\n RECORD SUB 1\t\t%d",d->rec_sub1);
       printf("\n RECORD TYPE\t\t%d",d->rec_type);
       printf("\n RECORD SUB 2\t\t%d",d->rec_sub2);
       printf("\n RECORD SUB 3\t\t%d",d->rec_sub3);
       printf("\n RECORD LENGTH\t\t%ld\n",d->length);

       if ( d->rec_type != LFTYPE ) {
             printf("\n%s",r->bogus);
       }
       else {
          printfd("\n FACILITY RELATED REC NUM\t%d",r->seq_num);
          printfs("\n DATA TAKE ID\t\t%s",r->datatake_ID);
          printfs("\n IMAGE ID\t\t%s",r->image_ID);
          printfs("\n CORRELATION YEAR\t%s",r->corr_year);
          printfs("\n CORRELATION TIME\t%s",r->corr_GMT);
          printfs("\n REF IMG OR SITE NAME\t%s",r->site_name);
          printfs("\n DATA YEAR\t\t%s",r->data_year);
          printfs("\n CENTER GMT\t\t%s",r->center_GMT);
          printff("\n CENTER LATITUDE\t%17.7f",r->center_LAT);
          printff("\n CENTER LONGITUDE\t%17.7f",r->center_LON);
          printff("\n NEAR START LATITUDE\t%17.7f",r->near_start_LAT);
          printff("\n NEAR START LONGITUDE\t%17.7f",r->near_start_LON);
          printff("\n NEAR END LATITUDE\t%17.7f",r->near_end_LAT);
          printff("\n NEAR END LONGITUDE\t%17.7f",r->near_end_LON);
          printff("\n FAR START LATITUDE\t%17.7f",r->far_start_LAT);
          printff("\n FAR START LONGITUDE\t%17.7f",r->far_start_LON);
          printff("\n FAR END LATITUDE\t%17.7f",r->far_end_LAT);
          printff("\n FAR END LONGITUDE\t%17.7f",r->far_end_LON);
          printff("\n ACTUAL AZIMUTH\t\t%17.7f",r->actual_azimuth);
          printff("\n ACTUAL RANGE\t\t%17.7f",r->actual_range);
          printfl("\n ACTUAL PIXELS\t\t%ld",r->actual_pixels);
          printfl("\n ACTUAL LINES\t\t%ld",r->actual_lines);
          printfl("\n TOTAL PIXELS\t\t%ld",r->total_pixels);
          printfl("\n TOTAL LINES\t\t%ld",r->total_lines);
          printfs("\n MEDIA ID\t\t%s",r->media_id);
          printfl("\n START ADDRESS\t\t%ld",r->start_address);
          printfl("\n END ADDRESS\t\t%ld",r->end_address);
          printfs("\n PLATFORM NAME\t\t%s",r->platform_name);
          printfs("\n SENSOR AND MODE\t%s",r->sensor_mode);
          printff("\n PRF\t\t\t%17.7f",r->PRF);
          printff("\n ANTENNA LOOK ANGLE\t%17.7f",r->ant_look_angle);
          printff("\n DATA RATE\t\t%17.7f",r->data_rate);
          printff("\n DATA WINDOW POSITION\t%17.7f",r->data_win_pos);
          printff("\n RANGE GATE DELAY\t%17.7f",r->range_gate_del);
          printff("\n TRACK ANGLE\t\t%17.7f",r->track_angle);
          printfs("\n ASC/DESCENDING FLAG\t%s",r->ASC_DESC);
          printff("\n SPACECRAFT ALTITUDE\t%17.7f",r->S_C_altitude);
          printfe("\n X POSITION\t\t%23.10E",r->X_position);
          printfe("\n Y POSITION\t\t%23.10E",r->Y_position);
          printfe("\n Z POSITION\t\t%23.10E",r->Z_position);
          printfe("\n X VELOCITY\t\t%23.10E",r->X_velocity);
          printfe("\n Y VELOCITY\t\t%23.10E",r->Y_velocity);
          printfe("\n Z VELOCITY\t\t%23.10E",r->Z_velocity);
          printff("\n ROLL\t\t\t%15.7f",r->roll);
          printff("\n YAW\t\t\t%15.7f",r->yaw);
          printff("\n PITCH\t\t\t%15.7f",r->pitch);
          printfd("\n ROLL FLAG\t\t%d",r->roll_flag);
          printfd("\n YAW FLAG\t\t%d",r->yaw_flag);
          printfd("\n PITCH FLAG\t\t%d",r->pitch_flag);
          printff("\n ROLL RATE\t\t%15.7f",r->roll_rate);
          printff("\n YAW RATE\t\t%15.7f",r->yaw_rate);
          printff("\n PITCH RATE\t\t%15.7f",r->pitch_rate);
          printfd("\n ROLL RATE FLAG\t\t%d",r->roll_rate_flag);
          printfd("\n YAW RATE FLAG\t\t%d",r->yaw_rate_flag);
          printfd("\n PITCH RATE FLAG\t%d",r->pitch_rate_flag);
          printff("\n NADIR RADIUS\t\t%17.7f",r->nadir_radius);
          printff("\n IMAGE RADIUS\t\t%17.7f",r->image_radius);
          printff("\n INCIDENCE ANGLE\t%17.7f",r->incidence_angle);
          printfs("\n PROCESSOR VERSION\t%s",r->proc_version);
          printfs("\n PROCESSOR TYPE\t\t%s",r->proc_type);
          printfs("\n TYPE OF EPHEMERIS\t%s",r->type_ephemeris);
          printff("\n EFF LOOKS AZIMUTH\t%17.7f",r->looks_azimuth);
          printff("\n EFF LOOKS RANGE\t%17.7f",r->looks_range);
          printff("\n WEIGH PEDESTAL HT AZI\t%17.7f",r->azi_weight_fac);
          printff("\n WEIGH PEDESTAL HT RNG\t%17.7f",r->range_weight_fac);
          printfs("\n LOOK ENERGY NORM FLAG\t%s",r->look_energy_eq);
          printff("\n IND DISTORTION IN AZI\t%17.7f",r->induced_azimuth);
          printff("\n IND DISTORTION IN RNG\t%17.7f",r->induced_range);
          printff("\n RECEIVER GAIN\t\t%17.7f",r->gain);
          printff("\n SWATH VELOCITY\t\t%17.7f",r->swath_velocity);
          printff("\n SQUINT ANGLE\t\t%17.7f",r->squint_angle);
          printff("\n AVERAGE TERRAIN HEIGHT\t%17.7f",r->avg_terrain_ht);
          printfd("\n PROCESSOR GAIN\t\t%d",r->processor_gain);
          printfs("\n DESKEW APPLIED FLAG\t%s",r->deskew);
          printfs("\n SLANT RANGE FLAG\t%s",r->gnd_slant_flag);
          printff("\n SLANT RNG TO 1ST PIX\t%17.7f",r->sl_rng_1st_pix);
          printff("\n SLANT RNG TO LAST PIX\t%17.7f",r->sl_rng_last_pix);
          printfl("\n START SAMPLE PROCESSED\t%ld",r->start_sample);
          printfs("\n CLUTTERLOCK FLAG\t%s",r->clutterlock_flg);
          printff("\n DPLR FREQ CONST\t%17.7f",r->dop_frq_const);
          printff("\n DPLR FREQ SLOPE\t%17.7f",r->dop_frq_slope);
          printff("\n DPLR FREQ QUAD\t\t%17.7f",r->dop_frq_quad);
          printfs("\n AUTOFOCUS FLAG\t%s",r->autofocus_flag);
          printff("\n DPLR FREQ RATE CONST\t%17.7f",r->dop_frq_r_cnst);
          printff("\n DPLR FREQ RATE SLOPE\t%17.7f",r->dop_frq_r_slope);
          printff("\n DPLR FREQ RATE QUAD\t%17.7f",r->dop_frq_r_quad);
          printff("\n RESOLUTION AZIMUTH\t%17.7f",r->azi_res);
          printff("\n RESOLUTION RANGE\t%17.7f",r->rng_res);
          printff("\n AZIMUTH PIXEL\t\t%17.7f",r->azimuth_pixel);
          printff("\n RANGE PIXEL\t\t%17.7f",r->range_pixel);
          printfs("\n OBRC FLAG\t\t%s",r->OBRC_flag);
          printfd("\n BITS SAMPLE\t\t%d",r->bits_sample);
          printff("\n CALIBRATION ESTIMATE\t%17.7f",r->calib_est);
          printff("\n BIT ERROR RATE\t\t%17.7f",r->bit_err_rate);
          printff("\n SIGNAL TO NOISE RATIO\t%17.7f",r->SNR);
          printff("\n ESTIMATED NOISE FLOOR\t%17.7f",r->est_noise_flr);
          printff("\n RADIOMETRIC RESOLUTION\t%17.7f",r->radio_m_resol);
          printfd("\n SATURATED DATA POINTS\t%ld",r->satur_points);
          printfs("\n WITHIN SPEC FLAG\t%s",r->spec_flag);
#ifndef PRE_RADARSAT
          printff("\n CHIRP REPLICA AGC VALUE\t%17.7f",r->repl_agc);
          printff("\n TEMP OF RCVR LNA\t\t%17.7f",r->temp_rx_lna);
          printff("\n TEMP OF RCVR SUBSYSTEM\t\t%17.7f",r->temp_rx_sub);
          printff("\n TEMP OF RCVR PROTECTOR\t\t%17.7f",r->temp_rx_prot);
          printff("\n TEMP OF CALIB SYSTEM\t\t%17.7f",r->temp_cal_sys);
          printff("\n RCVR AGC VALUE\t\t\t%17.7f",r->rx_agc);
          printff("\n PRE CAL1 AVG POWER\t\t%17.7f",r->pre_cal1_pow);
          printff("\n PRE CAL2 AVG POWER\t\t%17.7f",r->pre_cal2_pow);
          printff("\n POST CAL1 AVG POWER\t\t%17.7f",r->post_cal1_pow);
          printff("\n POST CAL2 AVG POWER\t\t%17.7f",r->post_cal2_pow);
          printff("\n REPLICA AVG POWER\t\t%17.7f",r->repl_pow);
          printff("\n EST SCANSAR ROLL ANGLE\t\t%17.7f",r->ssar_roll_ang);
          printfs("\n COMMENT\t\t%s",r->comment);
#endif
       }
       printf("\n**************** end of Facility Related record *******************\n");
    }

    if (r->next != (Fac_Related *) NULL && !mode) FR_to_LDR( r->next, out, mode );
    return(TRUE);
}



SARL_ptr* Allocate_SARL( void ) {
   SARL_ptr *ptr;
    /* allocate a temp buffer */

   if (work == (unsigned char*) NULL) 
     work = (unsigned char*) calloc(1,BUF_BIG_SIZE*sizeof(unsigned char));

   return ( (ptr = (SARL_ptr *) calloc (1,sizeof (SARL_ptr))) );
}


int Allocate_SARL_ODR( SARL_ptr* tree)
{
   if ( tree != NULL ) {
      /* Allocate space for records based on the number stated in the FDR */
      tree->read_count=1;
      tree->write_count=0;
      Allocate_Data_Sum( tree );
      Allocate_Map_Proj( tree );
      Allocate_Plat_Pos( tree );
      Allocate_Att_Data( tree );
      Allocate_Radi_Data( tree );
      Allocate_Radi_Comp( tree );
      Allocate_Qual_Sum( tree );
      Allocate_Data_Hist( tree );
      Allocate_Rng_Spec( tree );
      Allocate_Digital_Elev( tree );
      Allocate_Proc_Parm( tree );
      Allocate_Calib_Data( tree );
      Allocate_Fac_Related( tree );

      return(TRUE);
   }
   return(FALSE);
}


Dataset_Sum* Allocate_Data_Sum( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Dataset_Sum *ds, *ds1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_dataset) return(NULL);
    if ( (ds1 = (Dataset_Sum *) calloc (d->n_dataset,sizeof (Dataset_Sum)) ) == NULL ) {
       printf("\n Fail allocating  Dataset Sum ");
       return(NULL);
    }

    tree->data_sum = &ds1[0];
    for (i=1, ds=tree->data_sum; i< d->n_dataset; ds->next=&ds1[i], ds=ds->next, i++);
    return( tree->data_sum );
}

Map_Proj* Allocate_Map_Proj( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Map_Proj *mp, *mp1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_map_proj) return(NULL);
    if ( (mp1 = (Map_Proj *) calloc (d->n_dataset,sizeof (Map_Proj)) ) == NULL ) {
       printf("\n Fail allocating  Map Projection ");
       return(NULL);
    }

    tree->map_proj = &mp1[0];
    for (i=1, mp=tree->map_proj; i< d->n_map_proj; mp->next=&mp1[i], mp=mp->next, i++);
    return( tree->map_proj );
}

Pos_Data* Allocate_Plat_Pos( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Pos_Data *p, *p1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_plat_pos) return(NULL);
    if ( (p1 = (Pos_Data *) calloc (d->n_plat_pos,sizeof (Pos_Data)) ) == NULL ) {
       printf("\n Fail allocating  Platform Position ");
       return(NULL);
    }

    tree->platform = &p1[0];
    for (i=1, p=tree->platform; i< d->n_plat_pos; p->next=&p1[i], p=p->next, i++);
    return( tree->platform );
}


Att_Data* Allocate_Att_Data( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Att_Data *a, *a1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_att_data) return(NULL);
    if ( (a1 = (Att_Data *) calloc (d->n_att_data,sizeof (Att_Data)) ) == NULL ) {
       printf("\n Fail allocating  Attitude Data ");
       return(NULL);
    }

    tree->attitude = &a1[0];
    for (i=1, a=tree->attitude; i< d->n_att_data; a->next=&a1[i], a=a->next, i++);
    return( tree->attitude );
}

Radi_Data* Allocate_Radi_Data( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Radi_Data *r, *r1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_radi_data) return(NULL);
    if ( (r1 = (Radi_Data *) calloc (d->n_radi_data,sizeof (Radi_Data)) ) == NULL ) {
       printf("\n Fail allocating  Radio Data ");
       return(NULL);
    }

    tree->radio_data = &r1[0];
    for (i=1, r=tree->radio_data; i< d->n_radi_data; r->next=&r1[i], r=r->next, i++);
    return( tree->radio_data );
}

Radi_Comp* Allocate_Radi_Comp( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Radi_Comp *r, *r1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_radi_comp) return(NULL);
    if ( (r1 = (Radi_Comp *) calloc (d->n_radi_comp,sizeof (Radi_Comp)) ) == NULL ) {
       printf("\n Fail allocating  Radio Compensation ");
       return(NULL);
    }

    tree->radio_comp = &r1[0];
    for (i=1, r=tree->radio_comp; i< d->n_radi_comp; r->next=&r1[i], r=r->next, i++);
    return( tree->radio_comp );
}


Qual_Sum* Allocate_Qual_Sum( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Qual_Sum *q, *q1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_qual_sum) return(NULL);
    if ( (q1 = (Qual_Sum *) calloc (d->n_qual_sum,sizeof (Qual_Sum)) ) == NULL ) {
       printf("\n Fail allocating  Quality Sum ");
       return(NULL);
    }

    tree->data_qual = &q1[0];
    for (i=1, q=tree->data_qual; i< d->n_qual_sum; q->next=&q1[i], q=q->next, i++);
    return( tree->data_qual );
}

Data_Hist* Allocate_Data_Hist( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Data_Hist *h, *h1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_data_hist) return(NULL);
    if ( (h1 = (Data_Hist *) calloc (d->n_data_hist,sizeof (Data_Hist)) ) == NULL ) {
       printf("\n Fail allocating  Data Histogram ");
       return(NULL);
    }

    tree->histogram = &h1[0];
    for (i=1, h=tree->histogram; i< d->n_data_hist; h->next=&h1[i], h=h->next, i++);
    return( tree->histogram );
}

Rng_Spec* Allocate_Rng_Spec( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Rng_Spec *r, *r1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_rang_spec) return(NULL);
    if ( (r1 = (Rng_Spec *) calloc (d->n_rang_spec,sizeof (Rng_Spec)) ) == NULL ) {
       printf("\n Fail allocating  Range Spectra ");
       return(NULL);
    }

    tree->spectra = &r1[0];
    for (i=1, r=tree->spectra; i< d->n_rang_spec; r->next=&r1[i], r=r->next, i++);
    return( tree->spectra );
}

Digital_Elev* Allocate_Digital_Elev( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Digital_Elev *r, *r1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_dem_desc) return(NULL);
    if ( (r1 = (Digital_Elev *) calloc (d->n_dem_desc,sizeof (Digital_Elev)) ) == NULL ) {
       printf("\n Fail allocating  Digital Elevation");
       return(NULL);
    }

    tree->elevation = &r1[0];
    for (i=1, r=tree->elevation; i< d->n_dem_desc; r->next=&r1[i], r=r->next, i++);
    return( tree->elevation );
}

Proc_Parm* Allocate_Proc_Parm( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Proc_Parm *r, *r1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_det_proc) return(NULL);
    if ( (r1 = (Proc_Parm *) calloc (d->n_det_proc,sizeof (Proc_Parm)) ) == NULL ) {
       printf("\n Fail allocating Detail Processing");
       return(NULL);
    }

    tree->detail = &r1[0];
    for (i=1, r=tree->detail; i< d->n_det_proc; r->next=&r1[i], r=r->next, i++);
    return( tree->detail );
}

Calib_Data* Allocate_Calib_Data( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Calib_Data *r, *r1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_cal) return(NULL);
    if ( (r1 = (Calib_Data *) calloc (d->n_cal,sizeof (Calib_Data)) ) == NULL ) {
       printf("\n Fail allocating Calibration Data");
       return(NULL);
    }

    tree->calibration = &r1[0];
    for (i=1, r=tree->calibration; i< d->n_cal; r->next=&r1[i], r=r->next, i++);
    return( tree->calibration );
}

Fac_Related* Allocate_Fac_Related( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Fac_Related *r, *r1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_fac_data) return(NULL);
    if ( (r1 = (Fac_Related *) calloc (d->n_fac_data,sizeof (Fac_Related)) ) == NULL ) {
       printf("\n Fail allocating Facility Related ");
       return(NULL);
    }

    tree->facility = &r1[0];
    for (i=1, r=tree->facility; i< d->n_fac_data; r->next=&r1[i], r=r->next, i++);
    return( tree->facility );
}



void Free_SARL( SARL_ptr* tree) {

   Dataset_Sum  *ds, *ds_next;
   Map_Proj  *mp, *mp_next;
   Pos_Data  *pd, *pd_next;
   Att_Data  *ad, *ad_next;
   Radi_Data *rd, *rd_next;
   Radi_Comp *rc, *rc_next;
   Qual_Sum  *qs, *qs_next;
   Data_Hist *dh, *dh_next;
   Rng_Spec  *rs, *rs_next;
   Digital_Elev *de, *de_next;
   Proc_Parm *dp, *dp_next;
   Calib_Data *cd, *cd_next;
   Fac_Related *fr, *fr_next;

   /* Dan F -- for memory leak patches */
   Data_Hist     *dh_first;
   Hist_Data_Set *data_set_next, *data_set_tmp;

   if (work != (unsigned char*) NULL) {
      free ((void*) work);
      work = NULL;
   }


   if (tree != NULL ) {

      ds = tree->data_sum;
      while (ds != NULL) {
            ds_next = ds->next;
            free( ds );
            ds = ds_next;
      }

      mp = tree->map_proj;
      while (mp != NULL) {
            mp_next = mp->next;
            free( mp );
            mp = mp_next;
      }

      /* Changed by Dan F: patched memory leak */
      pd = tree->platform;
      while (pd != NULL) {
            pd_next = pd->next;
	    free( pd->pos_vect ) ;
            free( pd );
            pd = pd_next;
      }

      /* Augmented by Dan F -- patch a leak */
      ad = tree->attitude;
      while (ad != NULL) {
            ad_next = ad->next;
	    free( ad->att_vect ) ;
            free( ad );
            ad = ad_next;
      }

      rd = tree->radio_data;
      while (rd != NULL) {
            rd_next = rd->next;
            free( rd );
            rd = rd_next;
      }

      rc = tree->radio_comp;
      while (rc != NULL) {
            rc_next = rc->next;
            free( rc );
            rc = rc_next;
      }

      qs = tree->data_qual;
      while (qs != NULL) {
            qs_next = qs->next;
            free( qs );
            qs = qs_next;
      }

      /* Changed by Dan F -- patched memory bugs */

      dh = dh_first = tree->histogram;
      while (dh != NULL) {
            dh_next = dh->next;

	    /* data_values_hist was malloc'd, so a loop is needed. */
	    data_set_tmp = dh->data_set ;
	    while (data_set_tmp != NULL) {
	      data_set_next = data_set_tmp->next ;
	      free( data_set_tmp->data_values_hist ) ;
	      data_set_tmp = data_set_next ;
	    }

/*	    free( dh->data_set->data_values_hist ); */
	    free( dh->data_set );
            dh = dh_next;
      }
      free( dh_first ) ;   /* this was calloced, so only one free is needed. */

      rs = tree->spectra;
      while (rs != NULL) {
            rs_next = rs->next;
            free( rs );
            rs = rs_next;
      }

      /* Augmented by Dan F. */
      de = tree->elevation;
      while (de != NULL) {
            de_next = de->next;
	    free( de->set->pts );
	    free( de->set );
            free( de );
            de = de_next;
      }
      
      dp = tree->detail;
      while (dp != NULL) {
            dp_next = dp->next;
            free( dp );
            dp = dp_next;
      }

      cd = tree->calibration;
      while (cd != NULL) {
            cd_next = cd->next;
            free( cd );
            cd = cd_next;
      }

      fr = tree->facility;
      while (fr != NULL) {
            fr_next = fr->next;
            free( fr );
            fr = fr_next;
      }
      free( tree );
   }
}

int Read_FDR_SARL( FILE* fp, SARL_ptr* ldr ) 
{
    int nbytes;
    int rsize;
    int ret;
    unsigned char* tb;
    unsigned char* buf = work;
    unsigned int *lb;

    lb = (unsigned int *) buf;

    /* read first 12 bytes of the record */
    buf = work;
    nbytes = read_record( fp, buf, sizeof(unsigned char), 12 );

    switch ( nbytes ) {

        case (END_OF_FILE):

        case (READ_ERROR):
             break;

        default:
             if (nbytes == 12 ) {
                tb=&buf[4];
                if ( MATCH_RECORD(tb, LD1SUB, LDTYPE, LD2SUB, LD3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the File Desciptor Record structure */
                         Decode_SARL_FDR( buf, &(ldr->descript) );
                         ldr->descript.desc.length=rsize+12;
                         ret=MATCH_FOUND;
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
                else {
                     printf("\n Not a FDR record %d %d %d %d",buf[4], buf[5], buf[6], buf[7]);
                     ret=NO_MATCH;
                }
             }
    }
    return(ret);
}

void Decode_SARL_FDR( unsigned char* buf, Sarl_Desc_Rec* t ) 
{
    desc_rec *d = &(t->desc);
    unsigned int i, *lb;

    lb = (unsigned int *) buf;
     
     d->rec_seq  = ntohl( lb[0] );
     d->rec_sub1 = *(buf+4);
     d->rec_type = *(buf+5);
     d->rec_sub2 = *(buf+6);
     d->rec_sub3 = *(buf+7);

    Record_length = 0;
    for (i=8; i<12; i++) {
       Record_length = Record_length + *(buf+i);
       Record_length *= 256;
    }
    Record_length /= 256;
    Current_length = 12;
    FDR_entry = COM_entry = 0;
     strNcpy(t->ascii_flag, (char*) (buf+12), 2); 
     strNcpy(t->format_doc, (char*) (buf+16), 12);
     strNcpy(t->format_rev, (char*) (buf+28), 2);
     strNcpy(t->design_rev, (char*) (buf+30), 2);
     strNcpy(t->software_id, (char*) (buf+32), 12);
     t->file_num = (short) Get_I4(&buf[44], 4);
     strNcpy(t->product_id, (char*) (buf+48), 16);
     strNcpy(t->rec_seq_flag, (char*) (buf+64), 4);
     t->seq_loc = Get_I4(&buf[68], 8);
     t->seq_len = (short) Get_I4(&buf[76], 4);
     strNcpy(t->rec_code, (char*) (buf+80), 4);
     t->code_loc = Get_I4(&buf[84], 8);
     t->code_len = (short) Get_I4(&buf[92], 4);
     strNcpy(t->rec_len, (char*) (buf+96), 4);
     t->rlen_loc = Get_I4(&buf[100], 8);
     t->rlen_len = (short) Get_I4(&buf[108], 4);
     t->n_dataset = Get_I4(&buf[180], 6);
     t->l_dataset = Get_I4(&buf[186], 6);
     t->n_map_proj = Get_I4(&buf[192], 6);
     t->l_map_proj = Get_I4(&buf[198], 6);
     t->n_plat_pos = Get_I4(&buf[204], 6);
     t->l_plat_pos = Get_I4(&buf[210], 6);
     t->n_att_data = Get_I4(&buf[216], 6);
     t->l_att_data = Get_I4(&buf[222], 6);
     t->n_radi_data = Get_I4(&buf[228], 6);
     t->l_radi_data = Get_I4(&buf[234], 6);
     t->n_radi_comp = Get_I4(&buf[240], 6);
     t->l_radi_comp = Get_I4(&buf[246], 6);
     t->n_qual_sum = Get_I4(&buf[252], 6);
     t->l_qual_sum = Get_I4(&buf[258], 6);
     t->n_data_hist = Get_I4(&buf[264], 6);
     t->l_data_hist = Get_I4(&buf[270], 6);
     /* request DH record be fixed length */
     if (t->l_data_hist) t->l_data_hist=4628;
     t->n_rang_spec = Get_I4(&buf[276], 6);
     t->l_rang_spec = Get_I4(&buf[282], 6);
     t->n_dem_desc = Get_I4(&buf[288], 6);
     t->l_dem_desc = Get_I4(&buf[294], 6);
     t->n_radar_par = Get_I4(&buf[300], 6);
     t->l_radar_par = Get_I4(&buf[306], 6);
     t->n_anno_data = Get_I4(&buf[312], 6);
     t->l_anno_data = Get_I4(&buf[318], 6);
     t->n_det_proc = Get_I4(&buf[324], 6);
     t->l_det_proc = Get_I4(&buf[330], 6);
     t->n_cal = Get_I4(&buf[336], 6);
     t->l_cal = Get_I4(&buf[342], 6);
     t->n_gcp = Get_I4(&buf[348], 6);
     t->l_gcp = Get_I4(&buf[354], 6);
     t->n_fac_data = Get_I4(&buf[420], 6);
     t->l_fac_data = Get_I4(&buf[426], 6);
     FDR_entry = COM_entry;
}

int Read_ALL_SARL( FILE* fp, SARL_ptr* ldr )
{
    int nbytes;
    int rsize;
    int ret=MATCH_FOUND;
    unsigned char* tb;
    unsigned char* buf = work;
    unsigned int *lb;

    lb = (unsigned int *) buf;

    while (ret==MATCH_FOUND) {
        /* read first 12 bytes of the record */

        nbytes = read_record( fp, buf, sizeof(unsigned char), 12 );

        switch ( nbytes ) {

            case (END_OF_FILE):
                 return(END_OF_FILE);
            case (READ_ERROR):
                 return(READ_ERROR);

            default:
             if (nbytes == 12 ) {
                tb=&buf[4];
                if ( MATCH_RECORD(tb, LS1SUB, LSTYPE, LS2SUB, LS3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Dataset Summary structure */
                         if ( (ret=Decode_SARL_DSR( buf, ldr) ) != DECODE_ERROR) {
                            ldr->data_sum->desc.length=rsize+12;
                         }
                         else {
                            ret=READ_ERROR;
                         }
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
                else if ( MATCH_RECORD(tb, LM1SUB, LMTYPE, LM2SUB, LM3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Map Projection structure */
                         if ( (ret=Decode_SARL_MPR( buf, ldr) ) != DECODE_ERROR) {
                            ldr->map_proj->desc.length=rsize+12;
                         }
                         else {
                            ret=READ_ERROR;
                         }
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
                else if ( MATCH_RECORD(tb, LP1SUB, LPTYPE, LP2SUB, LP3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Platform Position structure */
                         if ( (ret=Decode_SARL_PPR( buf, ldr) ) != DECODE_ERROR) {
                            ldr->platform->desc.length=rsize+12;
                         }
                         else {
                            ret=READ_ERROR;
                         }
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
                else if ( MATCH_RECORD(tb, LA1SUB, LATYPE, LA2SUB, LA3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Attitude structure */
                         if ( (ret=Decode_SARL_ATR( buf, ldr) ) != DECODE_ERROR) {
                            ldr->attitude->desc.length=rsize+12;
                         }
                         else {
                            ret=READ_ERROR;
                         }
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
                else if ( MATCH_RECORD(tb, LR1SUB, LRTYPE, LR2SUB, LR3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Radiometric Data structure */
                         if ( (ret=Decode_SARL_RDR( buf, ldr) ) != DECODE_ERROR) {
                            ldr->radio_data->desc.length=rsize+12;
                         }
                         else {
                            ret=READ_ERROR;
                         }
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
                else if ( MATCH_RECORD(tb, LC1SUB, LCTYPE, LC2SUB, LC3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Radiometric Compensation structure */
                         if ( (ret=Decode_SARL_RCR( buf, ldr) ) != DECODE_ERROR) {
                            ldr->radio_comp->desc.length=rsize+12;
                         }
                         else {
                            ret=READ_ERROR;
                         }
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
                else if ( MATCH_RECORD(tb, LQ1SUB, LQTYPE, LQ2SUB, LQ3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Data Quality Summary structure */
                         if ( (ret=Decode_SARL_DQSR( buf, ldr) ) != DECODE_ERROR) {
                            ldr->data_qual->desc.length=rsize+12;
                         }
                         else {
                            ret=READ_ERROR;
                         }
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
                else if ( MATCH_RECORD(tb, LH1SUB, LHTYPE, LH2SUB, LH3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Data Histograms structure */
                         if ( (ret=Decode_SARL_DHR( buf, ldr) ) != DECODE_ERROR) {
#if 0
                            ldr->histogram->desc.length=rsize+12;
#endif
			    /* requested that DH records be fixed length */
                            ldr->histogram->desc.length=4628;
                         }
                         else {
                            ret=READ_ERROR;
                         }
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
                else if ( MATCH_RECORD(tb, LZ1SUB, LZTYPE, LZ2SUB, LZ3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Range Spectra structure */
                         if ( (ret=Decode_SARL_RSR( buf, ldr) ) != DECODE_ERROR) {
                            ldr->spectra->desc.length=rsize+12;
                         }
                         else {
                            ret=READ_ERROR;
                         }
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
                else if ( MATCH_RECORD(tb, LE1SUB, LETYPE, LE2SUB, LE3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Digital Elevation structure */
                         if ( (ret=Decode_SARL_DEM( buf, ldr) ) != DECODE_ERROR) {
                            ldr->elevation->desc.length=rsize+12;
                         }
                         else {
                            ret=READ_ERROR;
                         }
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
	        else if ( MATCH_RECORD(tb, LY1SUB, LYTYPE, LY2SUB, LY3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Detail Processing structure */
                         if ( (ret=Decode_SARL_DPR( buf, ldr) ) != DECODE_ERROR) {
                            ldr->detail->desc.length=rsize+12;
                         }
                         else {
                            ret=READ_ERROR;
                         }
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
	        else if ( MATCH_RECORD(tb, LB1SUB, LBTYPE, LB2SUB, LB3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Calibration Data structure */
                         if ( (ret=Decode_SARL_CDR( buf, ldr) ) != DECODE_ERROR) {
                            ldr->calibration->desc.length=rsize+12;
                         }
                         else {
                            ret=READ_ERROR;
                         }
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
#ifdef PRE_RADARSAT
                else if ( MATCH_RECORD(tb, LF1SUB, LFTYPE, LF2SUB, LF3SUB) ||
                          MATCH_RECORD(tb, LF1ASUB, LIMTYPE, LF2SUB, LF3SUB) ||
                          MATCH_RECORD(tb, LF1ASUB, LICTYPE, LF2SUB, LF3SUB) ||
                          MATCH_RECORD(tb, LF1ASUB, LWMTYPE, LF2SUB, LF3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Facility Related structure */
                         if ( (ret=Decode_SARL_FRR( buf, ldr) ) != DECODE_ERROR) {
                            ldr->facility->desc.length=rsize+12;
                         }
                         else {
                            ret=READ_ERROR;
                         }
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
#else
                else if ( MATCH_RECORD(tb, LF1SUB, LFTYPE, LF2SUB, LF3SUB) ) {
                   rsize =  ntohl( lb[2]  ) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Facility Related structure */
                         if ( (ret=Decode_SARL_FRR( buf, ldr) ) != DECODE_ERROR) {
                            ldr->facility->desc.length=rsize+12;
                         }
                         else {
                            ret=READ_ERROR;
                         }
                      }
                      else {
                         ret=READ_ERROR;
                      }
                   }
                }
#endif
                else {
                   rsize =  ntohl( lb[2]  ) - 12;
		   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12), sizeof(unsigned char), rsize );
                      if ( nbytes != rsize) {
                         ret=READ_ERROR;
                      }
                   }
                }
             }
        }
    }
    return(ret);
}


int  Decode_SARL_DSR( unsigned char* buf, SARL_ptr* t ) 
{
    Dataset_Sum* ds = t->data_sum;
    int i=1, off;
    int n_data = t->descript.n_dataset;
    desc_rec* d;

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

    printf("\n Decode Data Set Summary record\n");
      
    /* process the record */
    d = &(ds->desc);
    d->rec_seq  = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    Record_length = 0;
    for (i=8; i<12; i++) {
       Record_length = Record_length + *(buf+i);
       Record_length *= 256;
    }
    Record_length /= 256;
    Current_length = 12;
    DSS_entry = COM_entry = 0;
    ds->seq_num = (short) Get_I4( &buf[12], 4);
    ds->sar_chan = (short) Get_I4( &buf[16], 4);
    strNcpy(ds->product_id, (char*) (buf+20), 16);
    strNcpy(ds->scene_des, (char*) (buf+36), 32);
    strNcpy(ds->inp_sctim, (char*) (buf+68), 32);
    strNcpy(ds->asc_des, (char*) (buf+100), 16);
    ds->pro_lat = Get_F4( &buf[116], 16);
    ds->pro_long = Get_F4( &buf[132], 16);
    ds->pro_head = Get_F4( &buf[148], 16);
    strNcpy(ds->ellip_des, (char*) (buf+164), 16);
    ds->ellip_maj = Get_F4( &buf[180], 16);
    ds->ellip_min = Get_F4( &buf[196], 16);
    ds->earth_mass = Get_F4( &buf[212], 16);
    ds->grav_const = Get_F4( &buf[228], 16);
    ds->ellip_j[0] = Get_F4( &buf[244], 16);
    ds->ellip_j[1] = Get_F4( &buf[260], 16);
    ds->ellip_j[2] = Get_F4( &buf[276], 16);
#ifdef PRE_RADARSAT
    ds->terrain_h = Get_F4( &buf[292], 16); 
    ds->sc_lin = Get_F4( &buf[308], 16);
    ds->sc_pix = Get_F4( &buf[324], 16);
    ds->scene_len = Get_F4( &buf[340], 16);
    ds->scene_wid = Get_F4( &buf[356], 16);
#else
    ds->terrain_h = Get_F4( &buf[308], 16); 
    ds->sc_lin = Get_I4( &buf[324], 8);
    ds->sc_pix = Get_I4( &buf[332], 8);
    ds->scene_len = Get_F4( &buf[340], 16);
    ds->scene_wid = Get_F4( &buf[356], 16);
#endif
    ds->nchn = (short) Get_I4( &buf[388], 4);
    strNcpy(ds->mission_id, (char*) (buf+396), 16);
    strNcpy(ds->sensor_id, (char*) (buf+412), 32);
    strNcpy(ds->revolution, (char*) (buf+444), 8);
    ds->plat_lat = Get_F4( &buf[452], 8);
    ds->plat_long = Get_F4( &buf[460], 8);
    ds->plat_head_scene = Get_F4( &buf[468], 8);
    ds->clock_ang = Get_F4( &buf[476], 8);
    ds->incident_ang = Get_F4( &buf[484], 8);
    ds->frequency = Get_F4( &buf[492], 8);
    ds->wave_length = Get_F4( &buf[500], 16);
    strNcpy(ds->motion_comp, (char*) (buf+516), 2);
    strNcpy(ds->pulse_code, (char*) (buf+518), 16);
    ds->ampl_coef[0] = Get_F4( &buf[534], 16);
    ds->ampl_coef[1] = Get_F4( &buf[550], 16);
    ds->ampl_coef[2] = Get_F4( &buf[566], 16);
    ds->ampl_coef[3] = Get_F4( &buf[582], 16);
    ds->ampl_coef[4] = Get_F4( &buf[598], 16);
    ds->phas_coef[0]= Get_F4( &buf[614], 16);
    ds->phas_coef[1]= Get_F4( &buf[630], 16);
    ds->phas_coef[2]= Get_F4( &buf[646], 16);
    ds->phas_coef[3]= Get_F4( &buf[662], 16);
    ds->phas_coef[4]= Get_F4( &buf[678], 16);
    ds->chirp_ext_ind = Get_I4( &buf[694], 8);
    ds->rng_samp_rate = Get_F4( &buf[710], 16);
    ds->rng_gate = Get_F4( &buf[726], 16);
    ds->rng_length = Get_F4( &buf[742], 16);
    strNcpy(ds->baseband_f, (char*) (buf+758), 4);
    strNcpy(ds->rngcmp_f, (char*) (buf+762), 4);
    ds->gn_polar = Get_F4( &buf[766], 16);
    ds->gn_cross = Get_F4( &buf[782], 16);
    ds->chn_bits = Get_I4( &buf[798], 8);
    strNcpy(ds->quant_desc, (char*) (buf+806), 12);
    ds->i_bias = Get_F4( &buf[818], 16);
    ds->q_bias = Get_F4( &buf[834], 16);
    ds->iq_ratio = Get_F4( &buf[850], 16);
    ds->spare_dss_7 = Get_F4( &buf[866], 16);
    ds->spare_dss_8 = Get_F4( &buf[882], 16);
    ds->ele_sight = Get_F4( &buf[898], 16);
    ds->mech_sight = Get_F4( &buf[914], 16);
    strNcpy(ds->echo_track, (char*) (buf+930), 4);
    ds->prf = Get_F4( &buf[934], 16);
    ds->elev_beam = Get_F4( &buf[950], 16);
    ds->azi_beam = Get_F4( &buf[966], 16);
    strNcpy(ds->sat_bintim, (char *) (buf+982), 16);
    strNcpy(ds->sat_clktim, (char*) (buf+998), 32);
    ds->sat_clkinc = Get_I4( &buf[1030], 8);
    strNcpy(ds->fac_id, (char*) (buf+1046), 16);
    strNcpy(ds->sys_id, (char*) (buf+1062), 8);
    strNcpy(ds->ver_id, (char*) (buf+1070), 8);
    strNcpy(ds->fac_code, (char*) (buf+1078), 16);
    strNcpy(ds->lev_code, (char*) (buf+1094), 16);
    strNcpy(ds->product_type, (char*) (buf+1110), 32);
    strNcpy(ds->algor_id, (char*) (buf+1142), 32);
    ds->n_azilok = Get_F4( &buf[1174], 16);
    ds->n_rnglok = Get_F4( &buf[1190], 16);
    ds->bnd_azilok = Get_F4( &buf[1206], 16);
    ds->bnd_rnglok = Get_F4( &buf[1222], 16);
    ds->bnd_azi = Get_F4( &buf[1238], 16);
    ds->bnd_rng = Get_F4( &buf[1254], 16);
    strNcpy(ds->azi_weight, (char*) (buf+1270), 32);
    strNcpy(ds->rng_weight, (char*) (buf+1302), 32);
    strNcpy(ds->data_inpsrc, (char*) (buf+1334), 16);
    ds->rng_res = Get_F4( &buf[1350], 16);
    ds->azi_res = Get_F4( &buf[1366], 16);
    ds->radi_stretch[0] = Get_F4( &buf[1382], 16);
    ds->radi_stretch[1] = Get_F4( &buf[1398], 16);
    ds->alt_dopcen[0] = Get_F4( &buf[1414], 16);
    ds->alt_dopcen[1] = Get_F4( &buf[1430], 16);
    ds->alt_dopcen[2] = Get_F4( &buf[1446], 16);
    ds->crt_dopcen[0] = Get_F4( &buf[1478], 16);
    ds->crt_dopcen[1] = Get_F4( &buf[1494], 16);
    ds->crt_dopcen[2] = Get_F4( &buf[1510], 16);
    strNcpy(ds->time_dir_pix, (char*) (buf+1526), 8);
    strNcpy(ds->time_dir_lin, (char*) (buf+1534), 8);
    ds->alt_rate[0] = Get_F4( &buf[1542], 16);
    ds->alt_rate[1] = Get_F4( &buf[1558], 16);
    ds->alt_rate[2] = Get_F4( &buf[1574], 16);
    ds->crt_rate[0] = Get_F4( &buf[1606], 16);
    ds->crt_rate[1] = Get_F4( &buf[1622], 16);
    ds->crt_rate[2] = Get_F4( &buf[1638], 16);
    strNcpy(ds->line_cont, (char*) (buf+1670), 8);
    strNcpy(ds->clutterlock_flg, (char*) (buf+1678), 4);
    strNcpy(ds->auto_focus, (char*) (buf+1682), 4);
    ds->line_spacing = Get_F4( &buf[1686], 16);
    ds->pixel_spacing = Get_F4( &buf[1702], 16);
    strNcpy(ds->rngcmp_desg, (char*) (buf+1718), 16);
#ifdef PRE_RADARSAT
    ds->annot_pts=Get_I4( &buf[2006], 8);
    for (i=0, off=2022; i< ds->annot_pts; i++) {
        ds->annot_line[i] = Get_I4( &buf[off], 8); off+=8;
        ds->annot_pixel[i] = Get_I4( &buf[off], 8); off+=8;
        strNcpy(&(ds->annot_text[i][0]), (char *) (buf+off), 16); off+=16; 
    }
#else
    ds->no_beams = Get_I4( &buf[1766], 2);
    strNcpy(ds->beam1, (char*) (buf+1768), 4);
    strNcpy(ds->beam2, (char*) (buf+1772), 4);
    strNcpy(ds->beam3, (char*) (buf+1776), 4);
    strNcpy(ds->beam4, (char*) (buf+1780), 4);
    ds->prf1 = Get_F4( &buf[1784], 8);
    ds->prf2 = Get_F4( &buf[1792], 8);
    ds->prf3 = Get_F4( &buf[1800], 8);
    ds->prf4 = Get_F4( &buf[1808], 8);
    ds->rng_gate1 = Get_F4( &buf[1816], 8);
    ds->rng_gate2 = Get_F4( &buf[1824], 8);
    ds->rng_gate3 = Get_F4( &buf[1832], 8);
    ds->rng_gate4 = Get_F4( &buf[1840], 8);
    ds->tot_pls_burst = Get_I4( &buf[1848], 4);
    ds->val_pls_burst = Get_I4( &buf[1852], 4);
    ds->az_ovlp_nxt_img = Get_I4( &buf[1856], 8);
    ds->rg_off_nxt_img = Get_I4( &buf[1864], 8);
    strNcpy(ds->cal_params_file, (char*) (buf+1872), 32);
    strNcpy(ds->scan_results_file, (char*) (buf+1904), 32);
    strNcpy(ds->scanner_version, (char*) (buf+1936), 16);
    strNcpy(ds->decode_version, (char*) (buf+1952), 16);
#endif

    DSS_entry = COM_entry;
    return(DECODE_OK);
}

int  Decode_SARL_MPR( unsigned char* buf, SARL_ptr* t ) 
{
    Map_Proj* mp = t->map_proj;
    int i=1;
    int n_data = t->descript.n_map_proj;
    desc_rec* d;

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
    printf("\n Decode Map Projection Data record\n");
 
    /* process the record */
    d = &(mp->desc);
    d->rec_seq  = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    Record_length = 0;
    for (i=8; i<12; i++) {
       Record_length = Record_length + *(buf+i);
       Record_length *= 256;
    }
    Record_length /= 256;
    Current_length = 12;
    MP_entry = COM_entry = 0;
    mp->seq_num = (short) Get_I4( &buf[12], 4);
    strNcpy(mp->map_desc, (char *) (buf+28), 32);
    mp->n_pixel = Get_I4( &buf[60], 16);
    mp->n_line =  Get_I4( &buf[76], 16);
    mp->pixel_spacing = Get_F4( &buf[92], 16);
    mp->line_spacing = Get_F4( &buf[108], 16);
    mp->osc_orient = Get_F4( &buf[124], 16);
    mp->orb_incl = Get_F4( &buf[140], 16);
    mp->asc_node = Get_F4( &buf[156], 16);
    mp->isc_dist = Get_F4( &buf[172], 16);
    mp->geo_alt = Get_F4( &buf[188], 16);
    mp->isc_vel = Get_F4( &buf[204], 16);
    mp->plat_head = Get_F4( &buf[220], 16);
    strNcpy(mp->ref_ellip, (char*) (buf+236), 32);
    mp->semi_major = Get_F4( &buf[268], 16);
    mp->semi_minor = Get_F4( &buf[284], 16);
    mp->datum_shift[0] = Get_F4( &buf[300], 16);
    mp->datum_shift[1] = Get_F4( &buf[316], 16);
    mp->datum_shift[2] = Get_F4( &buf[332], 16);
    mp->aux_datum_shift[0] = Get_F4( &buf[348], 16);
    mp->aux_datum_shift[1] = Get_F4( &buf[364], 16);
    mp->aux_datum_shift[2] = Get_F4( &buf[380], 16);
    mp->scal_ellip = Get_F4( &buf[396], 16);
    strNcpy(mp->projection, (char*) (buf+412), 32);
    strNcpy(mp->utm_desc, (char*) (buf+444), 32);
    strNcpy(mp->utm_zone_sig, (char*) (buf+476), 4);
    mp->utm_east_orig = Get_F4( &buf[480], 16);
    mp->utm_north_orig = Get_F4( &buf[496], 16);
    mp->utm_cent_long = Get_F4( &buf[512], 16);
    mp->utm_cent_lat = Get_F4( &buf[528], 16);
    mp->utm_stand_par[0] = Get_F4( &buf[544], 16);
    mp->utm_stand_par[1] = Get_F4( &buf[560], 16);
    mp->utm_scale = Get_F4( &buf[576], 16);
    strNcpy(mp->ups_desc, (char*) (buf+592), 32);
    mp->ups_cent_long = Get_F4( &buf[624], 16);
    mp->ups_cent_lat = Get_F4( &buf[640], 16);
    mp->ups_scale = Get_F4( &buf[656], 16);
    strNcpy(mp->nsp_desc, (char*) (buf+672), 32);
    mp->nsp_east_orig = Get_F4( &buf[704], 16);
    mp->nsp_north_orig = Get_F4( &buf[720], 16);
    mp->nsp_cent_long = Get_F4( &buf[736], 16);
    mp->nsp_cent_lat = Get_F4( &buf[752], 16);
    mp->nsp_stand_par[0] = Get_F4( &buf[768], 16);
    mp->nsp_stand_par[1] = Get_F4( &buf[784], 16);
    mp->nsp_stand_par[2] = Get_F4( &buf[800], 16);
    mp->nsp_stand_par[3] = Get_F4( &buf[816], 16);
    mp->nsp_stand_mer[0] = Get_F4( &buf[832], 16);
    mp->nsp_stand_mer[1] = Get_F4( &buf[848], 16);
    mp->nsp_stand_mer[2] = Get_F4( &buf[864], 16);
    mp->corner_ne[0] = Get_F4( &buf[944], 16);
    mp->corner_ne[1] = Get_F4( &buf[960], 16);
    mp->corner_ne[2] = Get_F4( &buf[976], 16);
    mp->corner_ne[3] = Get_F4( &buf[992], 16);
    mp->corner_ne[4] = Get_F4( &buf[1008], 16);
    mp->corner_ne[5] = Get_F4( &buf[1024], 16);
    mp->corner_ne[6] = Get_F4( &buf[1040], 16);
    mp->corner_ne[7] = Get_F4( &buf[1056], 16);
    mp->corner_ll[0] = Get_F4( &buf[1072], 16);
    mp->corner_ll[1] = Get_F4( &buf[1088], 16);
    mp->corner_ll[2] = Get_F4( &buf[1104], 16);
    mp->corner_ll[3] = Get_F4( &buf[1120], 16);
    mp->corner_ll[4] = Get_F4( &buf[1136], 16);
    mp->corner_ll[5] = Get_F4( &buf[1152], 16);
    mp->corner_ll[6] = Get_F4( &buf[1168], 16);
    mp->corner_ll[7] = Get_F4( &buf[1184], 16);
    mp->terr_height[0] = Get_F4( &buf[1200], 16);
    mp->terr_height[1] = Get_F4( &buf[1216], 16);
    mp->terr_height[2] = Get_F4( &buf[1232], 16);
    mp->terr_height[3] = Get_F4( &buf[1248], 16);
    mp->lp_conv_coef[0] = (double) Get_F4( &buf[1264], 20);
    mp->lp_conv_coef[1] = (double) Get_F4( &buf[1284], 20);
    mp->lp_conv_coef[2] = (double) Get_F4( &buf[1304], 20);
    mp->lp_conv_coef[3] = (double) Get_F4( &buf[1324], 20);
    mp->lp_conv_coef[4] = (double) Get_F4( &buf[1344], 20);
    mp->lp_conv_coef[5] = (double) Get_F4( &buf[1364], 20);
    mp->lp_conv_coef[6] = (double) Get_F4( &buf[1384], 20);
    mp->lp_conv_coef[7] = (double) Get_F4( &buf[1404], 20);
    mp->mp_conv_coef[0] = (double) Get_F4( &buf[1424], 20);
    mp->mp_conv_coef[1] = (double) Get_F4( &buf[1444], 20);
    mp->mp_conv_coef[2] = (double) Get_F4( &buf[1464], 20);
    mp->mp_conv_coef[3] = (double) Get_F4( &buf[1484], 20);
    mp->mp_conv_coef[4] = (double) Get_F4( &buf[1504], 20);
    mp->mp_conv_coef[5] = (double) Get_F4( &buf[1524], 20);
    mp->mp_conv_coef[6] = (double) Get_F4( &buf[1544], 20);
    mp->mp_conv_coef[7] = (double) Get_F4( &buf[1564], 20); 
    MP_entry = COM_entry;
    return(DECODE_OK);
}

int  Decode_SARL_PPR( unsigned char* buf, SARL_ptr* t ) 
{
    Pos_Data* p = t->platform;
    Pos_Vect_Rec *pv;
    int i=1;
    int off;
    int n_data = t->descript.n_plat_pos;
    desc_rec* d;

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
    printf("\n Decode Platform Position Data record\n");
 
    /* process the record */
    d = &(p->desc);
    d->rec_seq  = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    Record_length = 0;
    for (i=8; i<12; i++) {
       Record_length = Record_length + *(buf+i);
       Record_length *= 256;
    }
    Record_length /= 256;
    Current_length = 12;
    PP_entry = COM_entry = 0;
    strNcpy(p->orbit_ele_desg, (char*) (buf+12), 32);
    p->orbit_ele[0] = Get_F4( &buf[44], 16);
    p->orbit_ele[1] = Get_F4( &buf[60], 16);
    p->orbit_ele[2] = Get_F4( &buf[76], 16);
    p->orbit_ele[3] = Get_F4( &buf[92], 16);
    p->orbit_ele[4] = Get_F4( &buf[108], 16);
    p->orbit_ele[5] = Get_F4( &buf[124], 16);
    p->ndata = (short) Get_I4( &buf[140], 4);
    p->year = (short) Get_I4( &buf[144], 4);
    p->month = (short) Get_I4( &buf[148], 4);
    p->day = (short) Get_I4( &buf[152], 4);
    p->gmt_day = (short) Get_I4( &buf[156], 4);
    p->gmt_sec = (double) Get_F4( &buf[160], 22);
    p->data_int = (double) Get_F4( &buf[182], 22);
    strNcpy(p->ref_coord, (char*) (buf+204), 64);
    p->hr_angle = (double) Get_F4( &buf[268], 22);
    p->alt_poserr = Get_F4( &buf[290], 16);
    p->crt_poserr = Get_F4( &buf[306], 16);
    p->rad_poserr = Get_F4( &buf[322], 16);
    p->alt_velerr = Get_F4( &buf[338], 16);
    p->crt_velerr = Get_F4( &buf[354], 16);
    p->rad_velerr = Get_F4( &buf[370], 16);

    pv = Allocate_Position_Velocity_Sets( p );
    off=386;
    while( pv != NULL) {
       pv->pos[0] = (double) Get_F4( &buf[off], 22); off+=22;
       pv->pos[1] = (double) Get_F4( &buf[off], 22); off+=22;
       pv->pos[2] = (double) Get_F4( &buf[off], 22); off+=22;
       pv->vel[0] = (double) Get_F4( &buf[off], 22); off+=22;
       pv->vel[1] = (double) Get_F4( &buf[off], 22); off+=22;
       pv->vel[2] = (double) Get_F4( &buf[off], 22); off+=22;
       pv=pv->next;
    }

    /* If stuff needs to be added after these sets, be sure to set
       buffer offset properly! */

    PP_entry = COM_entry;
    return(DECODE_OK);
}


Pos_Vect_Rec* Allocate_Position_Velocity_Sets( Pos_Data* p)
{
    Pos_Vect_Rec *pv, *pv1;
    int i;

    if (p==NULL || p->ndata<1) return(NULL);
    if ( (pv1 = (Pos_Vect_Rec *) calloc (p->ndata,sizeof (Pos_Vect_Rec)) ) == NULL ) {
       printf("\n Fail allocating Pos Vect Rec");
       return(NULL);
    }
    p->pos_vect = &pv1[0];
    for (i=1, pv=p->pos_vect; i< p->ndata; pv->next=&pv1[i], pv=pv->next, i++);
    return( p->pos_vect );
}

int  Decode_SARL_ATR( unsigned char* buf, SARL_ptr* t ) 
{
    Att_Data* a = t->attitude;
    int n_data = t->descript.n_att_data;
    desc_rec* d;
    int i=1;
    Att_Vect_Rec *av;
    int off;

    if (a == NULL) {
       printf("\nError: File Descriptor Record did not have a Attitude Data record listed\n");
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
    printf("\n Decode Attitude Data record\n");
 
    /* process the record */
    d = &(a->desc);
    d->rec_seq  = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);
    
    Record_length = 0;
    for (i=8; i<12; i++) {
       Record_length = Record_length + *(buf+i);
       Record_length *= 256;
    }
    Record_length /= 256;
    Current_length = 12;
    AT_entry = COM_entry = 0;
    a->npoint = (short) Get_I4( &buf[12], 4);

    /* Allocate memory for the specified number of compensation sets */

    av = Allocate_Attitude_Sets( a );
    off=16;
    while(av!=NULL) {
       av->gmt_day = (short) Get_I4( &buf[off], 4); off+=4;
       av->gmt_msec = Get_I4( &buf[off], 8); off+=8;
       av->pitch_flag = (short) Get_I4( &buf[off], 4); off+=4;
       av->roll_flag = (short) Get_I4( &buf[off], 4); off+=4;
       av->yaw_flag = (short) Get_I4( &buf[off], 4); off+=4;
       av->pitch = Get_F4( &buf[off], 14); off+=14;
       av->roll = Get_F4( &buf[off], 14); off+=14;
       av->yaw = Get_F4( &buf[off], 14); off+=14;
       av->pitch_rate_flag = (short) Get_I4( &buf[off], 4); off+=4;
       av->roll_rate_flag = (short) Get_I4( &buf[off], 4); off+=4;
       av->yaw_rate_flag = (short) Get_I4( &buf[off], 4); off+=4;
       av->pitch_rate = Get_F4( &buf[off], 14); off+=14;
       av->roll_rate = Get_F4( &buf[off], 14); off+=14;
       av->yaw_rate = Get_F4( &buf[off], 14); off+=14;
       av=av->next;
    }

    /* If stuff needs to be added after these sets, be sure to set
       buffer offset properly! */

    AT_entry = COM_entry;
    return(DECODE_OK);
}

Att_Vect_Rec* Allocate_Attitude_Sets( Att_Data* a)
{
    Att_Vect_Rec *av, *av1;
    int i;

    if (a==NULL || a->npoint<1) return(NULL);
    if ( (av1 = (Att_Vect_Rec *) calloc (a->npoint,sizeof (Att_Vect_Rec)) ) == NULL ) {
       printf("\n Fail allocating Att Vect Rec");
       return(NULL);
    }
    a->att_vect = &av1[0];
    for (i=1, av=a->att_vect; i< a->npoint; av->next=&av1[i], av=av->next, i++);
    return( a->att_vect );
}


int  Decode_SARL_RDR( unsigned char* buf, SARL_ptr* t ) 
{
    Radi_Data* r = t->radio_data;
    int n_data = t->descript.n_radi_data;
    desc_rec* d;
    int i=1;
    int off;

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
    printf("\n Decode Radiometric Data record\n");
 
    /* process the record */
    d = &(r->desc);
    d->rec_seq  = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    Record_length = 0;
    for (i=8; i<12; i++) {
       Record_length = Record_length + *(buf+i);
       Record_length *= 256;
    }
    Record_length /= 256;
    Current_length = 12;
    RD_entry = COM_entry = 0;
    r->seq_num = (short) Get_I4( &buf[12], 4);
    r->n_data = (short) Get_I4( &buf[16], 4);
    r->field_size = Get_I4( &buf[20], 8);
    strNcpy(r->chan_ind, (char*) (buf+28), 4);
    strNcpy(r->table_desig, (char*) (buf+36), 24);
    r->n_samp = Get_I4( &buf[60], 8);
    strNcpy(r->samp_type, (char*) (buf+68), 16);
    r->noise_fact = Get_F4( &buf[84], 16);
    r->linear_conv_fact = Get_F4( &buf[100], 16);
    r->offset_conv_fact = Get_F4( &buf[116], 16);

    off = 136;
    for (i=0; i<r->n_samp; i++, off+=16) {
        r->lookup_tab[i] = Get_F4( &buf[off], 16);
    }
    RD_entry = COM_entry;
    return(DECODE_OK);
}


int  Decode_SARL_RCR( unsigned char* buf, SARL_ptr* t ) 
{
    Radi_Comp* r = t->radio_comp;
    int n_data = t->descript.n_radi_comp;
    desc_rec* d;
    int i=1, off;
    Rad_Comp_Set *rc;
    Radio_Comp_Tbl *rt;

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
    printf("\n Decode Radiometric Compensation record\n");
 
    /* process the record */
    d = &(r->desc);
    d->rec_seq  = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    Record_length = 0;
    for (i=8; i<12; i++) {
       Record_length = Record_length + *(buf+i);
       Record_length *= 256;
    }
    Record_length /= 256;
    Current_length = 12;
    RC_entry = COM_entry = 0;

    r->seq_num = (short) Get_I4( &buf[12], 4);
    r->sar_chan = (short) Get_I4( &buf[16], 4);
    r->n_dset = Get_I4( &buf[20], 8);
    r->dset_size = Get_I4( &buf[28], 8);

    /* Allocate memory for the specified number of compensation sets */

    rc = Allocate_Comp_Sets( r );
    off=36;
    while(rc!=NULL) {
        strNcpy(rc->comp_data_type, (char*) (buf+off), 8); off+=8;
        strNcpy(rc->data_descr, (char*) (buf+off), 32); off+=32;
        rc->req_recs = (short) Get_I4( &buf[off], 4); off+=4;
        rc->table_seq_num = (short) Get_I4( &buf[off], 4); off+=4;
        rc->num_pairs = Get_I4( &buf[off], 8); off+=8;
        rc->first_pixel = Get_I4( &buf[off], 8); off+=8;
        rc->last_pixel = Get_I4( &buf[off], 8); off+=8;
        rc->pixel_size = Get_I4( &buf[off], 8); off+=8;
        rc->min_samp_index = Get_F4( &buf[off], 16); off+=16;   
        rc->min_comp_value = Get_F4( &buf[off], 16); off+=16;
        rc->max_samp_index = Get_F4( &buf[off], 16); off+=16;
        rc->max_comp_value = Get_F4( &buf[off], 16); off+=16; off+=16;
        rc->n_table_entries = Get_I4( &buf[off], 8); off+=8;

        /* Allocate memory for the specified number of compensation tables */

        rt = Allocate_Comp_Tbl( rc );
        while (rt!=NULL) {
            rt->sample_offset = Get_F4( &buf[off], 16); off+=16;
            rt->sample_gain = Get_F4( &buf[off], 16); off+=16; 
            rt = rt->next;
        }
        rc = rc->next;
    }
    RC_entry = COM_entry;
    return(DECODE_OK);
}


Rad_Comp_Set* Allocate_Comp_Sets( Radi_Comp* r )
{
    Rad_Comp_Set *rc, *rc1;
    int i;

    if (r==NULL || r->n_dset<1) return(NULL);
    if ( (rc1 = (Rad_Comp_Set *) calloc (r->n_dset,sizeof (Rad_Comp_Set)) ) == NULL ) {
       printf("\n Fail allocating  Rad Comp Set");
       return(NULL);
    }
    r->set = &rc1[0];
    for (i=1, rc=r->set; i< r->n_dset; rc->next=&rc1[i], rc=rc->next, i++);
    return( r->set );
}


Radio_Comp_Tbl* Allocate_Comp_Tbl( Rad_Comp_Set* s )
{
    Radio_Comp_Tbl *rt, *rt1;
    int i;

    if (s==NULL || s->n_table_entries<1) return(NULL);
    if ( (rt1 = (Radio_Comp_Tbl *) calloc (s->n_table_entries,sizeof (Radio_Comp_Tbl)) ) == NULL ) {
       printf("\n Fail allocating  Radio Comp Tbl");
       return(NULL);
    }
    s->tbl = &rt1[0];
    for (i=1, rt=s->tbl; i< s->n_table_entries; rt->next=&rt1[i], rt=rt->next, i++);
    return( s->tbl );
}


int Decode_SARL_DQSR( unsigned char* buf, SARL_ptr* t ) 
{
    Qual_Sum* q = t->data_qual;
    int n_data = t->descript.n_qual_sum;
    desc_rec* d;
    int i=1,off;

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
    printf("\n Decode Data Quality Summary record\n");
 
    /* process the record */
    d = &(q->desc);
    d->rec_seq  = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    Record_length = 0;
    for (i=8; i<12; i++) {
       Record_length = Record_length + *(buf+i);
       Record_length *= 256;
    }
    Record_length /= 256;
    Current_length = 12;
    DQS_entry = COM_entry = 0;
    q->seq_num = (short) Get_I4( &buf[12], 4);
    strNcpy(q->chan_ind, (char*) (buf+16), 4);
    strNcpy(q->cali_date, (char*) (buf+20), 6);
    q->nchn = (short) Get_I4( &buf[26], 4);
    q->islr = Get_F4( &buf[30], 16);
    q->pslr = Get_F4( &buf[46], 16);
    q->azi_ambig = Get_F4( &buf[62], 16);
    q->rng_ambig = Get_F4( &buf[78], 16);
    q->snr = Get_F4( &buf[94], 16);
    q->ber = Get_F4( &buf[110], 16);
    q->rng_res = Get_F4( &buf[126], 16);
    q->azi_res = Get_F4( &buf[142], 16);
    q->rad_res = Get_F4( &buf[158], 16);
    q->dyn_rng = Get_F4( &buf[174], 16);
    q->abs_rad_unc_db = Get_F4( &buf[190], 16);
    q->abs_rad_unc_deg = Get_F4( &buf[206], 16);

    off=222;
    for (i=0; (i<q->nchn && i<16); i++) {
        q->rel_rad_unc[0][i] = Get_F4( &buf[off], 16); off+=16;
        q->rel_rad_unc[1][i] = Get_F4( &buf[off], 16); off+=16;
    }
    Current_length = 734;
    q->alt_locerr = Get_F4( &buf[734], 16);
    q->crt_locerr = Get_F4( &buf[750], 16);
    q->alt_scale = Get_F4( &buf[766], 16);
    q->crt_scale = Get_F4( &buf[782], 16);
    q->dis_skew = Get_F4( &buf[798], 16);
    q->ori_err = Get_F4( &buf[814], 16);

    off=830;
    for (i=0; i<16; i++) {
        q->misreg[0][i] = Get_F4( &buf[off], 16); off+=16;
        q->misreg[1][i] = Get_F4( &buf[off], 16); off+=16;
    }
#ifndef PRE_RADARSAT
    q->nesz = Get_F4( &buf[off], 16); off+=16;
    q->enl = Get_F4( &buf[off], 16); off+=16;
    strNcpy(q->tb_update, (char*) (buf+off), 8); off+=8;
    strNcpy(q->cal_status, (char*) (buf+off), 16); off+=16; 
    strNcpy(q->spare_dqs_2, (char*) (buf+off), 22); off+=22;
    strNcpy(q->cal_comment, (char*) (buf+off), 200); 
#endif
    DQS_entry = COM_entry;
    return(DECODE_OK);
}

int  Decode_SARL_DHR( unsigned char* buf, SARL_ptr* t ) 
{
    Data_Hist* h = t->histogram;
    int n_data = t->descript.n_data_hist;
    desc_rec* d;
    int i=1, off;
    Hist_Data_Set *ht;
    long *dv;

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
    printf("\n Decode Data Histogram record\n");
 
    /* process the record */
    d = &(h->desc);
    d->rec_seq  = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    Record_length = 0;
    for (i=8; i<12; i++) {
       Record_length = Record_length + *(buf+i);
       Record_length *= 256;
    }
    Record_length /= 256;
    Current_length = 12;
    COM_entry = 0;
    h->seq_num = (short) Get_I4( &buf[12], 4);
    h->sar_chan = (short) Get_I4( &buf[16], 4);
    h->ntab = Get_I4( &buf[20], 8);
    h->ltab = Get_I4( &buf[28], 8);


    /* Allocate memory for the stated number of histogram table sets */ 
    ht = Allocate_Hist_Data_Set( h );
    off = 36;
    while (ht!=NULL) {
        strNcpy(ht->hist_desc, (char*) (buf+off), 32); off+=32;
        ht->nrec = (short) Get_I4( &buf[off], 4); off+=4;
        ht->tab_seq = (short) Get_I4( &buf[off], 4); off+=4;
        ht->nbin = Get_I4( &buf[off], 8); off+=8;
        ht->ns_lin = Get_I4( &buf[off], 8); off+=8;
        ht->ns_pix = Get_I4( &buf[off], 8); off+=8;
	ht->ngrp_lin = Get_I4( &buf[off], 8); off+=8;
        ht->ngrp_pix = Get_I4( &buf[off], 8); off+=8;
        ht->nsamp_lin = Get_I4( &buf[off], 8); off+=8;
        ht->nsamp_pix = Get_I4( &buf[off], 8); off+=8;
        ht->min_smp = Get_F4( &buf[off], 16); off+=16;
        ht->max_smp = Get_F4( &buf[off], 16); off+=16;
        ht->mean_smp = Get_F4( &buf[off], 16); off+=16;
        ht->std_smp = Get_F4( &buf[off], 16); off+=16;
        ht->smp_inc = Get_F4( &buf[off], 16); off+=16;
        ht->min_hist = Get_F4( &buf[off], 16); off+=16;
        ht->max_hist = Get_F4( &buf[off], 16); off+=16;
        ht->mean_hist = Get_F4( &buf[off], 16); off+=16;
        ht->std_hist = Get_F4( &buf[off], 16); off+=16;
        ht->nhist = Get_I4( &buf[off], 8); off+=8;

        dv = Allocate_DH_table( ht );
        for (i=0; i<ht->nhist; i++) {
            *(dv+i) = Get_I4( &buf[off], 8); off+=8;
        }
        ht=ht->next;
    }
    h->DH_entry = COM_entry;
    return(DECODE_OK);
}

long* Allocate_DH_table( Hist_Data_Set* ht) {
    if (ht==NULL || ht->nhist < 1) return(NULL);
    if ( (ht->data_values_hist = (long *) malloc (ht->nhist*sizeof (long)) ) == NULL ) {
       printf("\n Fail allocating  Histogram Table");
       return(NULL);
    }
    memset( (void *) ht->data_values_hist,0,ht->nhist*sizeof (long));
    return (ht->data_values_hist);
}

Hist_Data_Set* Allocate_Hist_Data_Set( Data_Hist* h ) 
{
    Hist_Data_Set *ht,*ht1;
    int i;
    
    if (h==NULL || h->ntab < 1) return(NULL);
    if ( (ht1 = (Hist_Data_Set *) calloc (h->ntab,sizeof (Hist_Data_Set)) ) == NULL ) {
       printf("\n Fail allocating  Hist Data Set");
       return(NULL);
    }
    h->data_set = &ht1[0];
    for (i=1, ht=h->data_set; i< h->ntab; ht->next=&ht1[i], ht=ht->next, i++);
    return(h->data_set);
}

int  Decode_SARL_RSR( unsigned char* buf, SARL_ptr* t ) 
{
    Rng_Spec* r = t->spectra;
    int n_data = t->descript.n_rang_spec;
    desc_rec* d;
    int i=1, off;

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
    printf("\n Decode Range Spectra record\n");
 
    /* process the record */
    d = &(r->desc);
    d->rec_seq  = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    Record_length = 0;
    for (i=8; i<12; i++) {
       Record_length = Record_length + *(buf+i);
       Record_length *= 256;
    }
    Record_length /= 256;
    Current_length = 12;
    RS_entry = COM_entry = 0;
    r->seq_num = (short) Get_I4( &buf[12],  4);
    r->sar_chan = (short) Get_I4( &buf[16], 4);
    r->n_dset = Get_I4( &buf[20], 8);
    r->dset_size = Get_I4( &buf[28], 8);
    r->req_recs = (short) Get_I4( &buf[36], 4);
    r->table_no = (short) Get_I4( &buf[40], 4);
    r->n_pixels = Get_I4( &buf[44], 8);
    r->pixel_offset = Get_I4( &buf[52], 8);
    r->n_lines = Get_I4( &buf[60], 8);
    r->first_freq = Get_F4( &buf[68], 16);
    r->last_freq = Get_F4( &buf[84], 16);
    r->min_power = Get_F4( &buf[100], 16);
    r->max_power = Get_F4( &buf[116], 16);
    r->n_bins = Get_I4( &buf[164], 8);

    off=172;
    for (i=0; i < 256; i++, off +=16) 
        r->data_values_spec[i] = Get_F4( &buf[off], 16);

    /* If stuff needs to be added after these sets, be sure to set
       buffer offset properly! */

    RS_entry = COM_entry;
    return(DECODE_OK);
}


Dem_Desc*  Allocate_DEM_sets( Digital_Elev* e )
{
    Dem_Desc *s,*s1;
    int i;

    if (e==NULL || e->num_polys < 1) return(NULL);
    if ( (s1 = (Dem_Desc *) calloc (e->num_polys,sizeof (Dem_Desc)) ) == NULL ) {
       printf("\n Fail allocating  Dem Desc");
       return(NULL);
    }
    e->set = &s1[0];
    for (i=1, s=e->set; i< e->num_polys; s->next=&s1[i], s=s->next, i++);
    return(e->set);
}

Corner_Pts* Allocate_DEM_pts( Dem_Desc* set )
{
    Corner_Pts *p,*p1;
    int i;

    if (set==NULL || set->num_crnr_pts<1) return(NULL);
    if ( (p1 = (Corner_Pts *) calloc (set->num_crnr_pts,sizeof (Corner_Pts)) ) == NULL ) {
       printf("\n Fail allocating  Corner Pts");
       return(NULL);
    }
    set->pts = &p1[0];
    for (i=1, p=set->pts; i< set->num_crnr_pts; p->next=&p1[i], p=p->next, i++);
    return(set->pts);
}

int  Decode_SARL_DEM( unsigned char* buf, SARL_ptr* t ) 
{
    Digital_Elev* e = t->elevation;
    int n_data = t->descript.n_dem_desc;
    desc_rec* d;
    Dem_Desc *set;
    Corner_Pts *pts;
    int i=1, off;

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
    printf("\n Decode Digital Elevation record\n");
 
    /* process the record */
    d = &(e->desc);
    d->rec_seq  = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    Record_length = 0;
    for (i=8; i<12; i++) {
       Record_length = Record_length + *(buf+i);
       Record_length *= 256;
    }
    Record_length /= 256;
    Current_length = 12;
    DE_entry = COM_entry = 0;
    e->seq_num = (short) Get_I4( &buf[12],  4);
    e->ttl_num_sets = Get_I4( &buf[16], 8);
    e->DEM_seq_num = (short) Get_I4( &buf[24], 4);
    strNcpy(e->source_DEM, (char *) (buf+28), 32);
    strNcpy(e->HT_ref_name, (char *) (buf+60), 32);
    strNcpy(e->gen_method, (char *) (buf+92), 32);
    strNcpy(e->raster_unit, (char *) (buf+124), 12);
    strNcpy(e->presentation_proj, (char *) (buf+136), 32);
    e->NS_raster = Get_F4( &buf[168], 16);
    e->EW_raster = Get_F4( &buf[184], 16);
    strNcpy(e->resample, (char *) (buf+200), 32);
    e->height_err = Get_F4( &buf[232], 16);
    e->NS_loc_err = Get_F4( &buf[248], 16);
    e->EW_loc_err = Get_F4( &buf[264], 16);
    e->max_height = Get_F4( &buf[280], 16);
    e->min_height = Get_F4( &buf[296], 16);
    e->MEAN_height = Get_F4( &buf[312], 16);
    e->STD_height = Get_F4( &buf[328], 16);
    e->num_polys = (short) Get_I4( &buf[344], 4);    

    off = 348;
    set= Allocate_DEM_sets( e );
    while (set!=NULL) {
        set->poly_seq_num = (short) Get_I4( &buf[off], 4 ); off+=4;
        set->num_crnr_pts = (short) Get_I4( &buf[off], 4 ); off+=4;
        strNcpy(set->spare_dem_1, "        ",  8); off+=8;

        pts = Allocate_DEM_pts( set );
        while (pts!=NULL) {
              pts->cp_lat_1 = Get_F4( &buf[off], 16 ); off+=16;
              pts->cp_lon_1 = Get_F4( &buf[off], 16 ); off+=16;
              pts=pts->next;
        }
        set=set->next;
    }
    DE_entry = COM_entry;
    return(DECODE_OK);
}

Beam_Info*  Allocate_Beam_Info( Proc_Parm* e )
{
    Beam_Info *s,*s1;
    int i;

    if (e==NULL || e->n_beams < 1) return(NULL);
    if ( (s1 = (Beam_Info *) calloc (e->n_beams,sizeof (Beam_Info)) ) == NULL ) {
       printf("\n Fail allocating  Beam Info");
       return(NULL);
    }
    e->beam_info = &s1[0];
    for (i=1, s=e->beam_info; i< e->n_beams; s->next=&s1[i], s=s->next, i++);
    return(e->beam_info);
}

Pix_Count*  Allocate_Pix_Count( Proc_Parm* e )
{
    Pix_Count *s,*s1;
    int i;

    if (e==NULL || e->n_pix_updates < 1) return(NULL);
    if ( (s1 = (Pix_Count *) calloc (e->n_pix_updates,sizeof (Pix_Count)) ) == NULL ) {
       printf("\n Fail allocating  Pix Count");
       return(NULL);
    }
    e->pix_count = &s1[0];
    for (i=1, s=e->pix_count; i< e->n_pix_updates; s->next=&s1[i], s=s->next, i++);
    return(e->pix_count);
}


Temp_Rec*  Allocate_Temp_Rec( Proc_Parm* e )
{
    Temp_Rec *s,*s1;
    int i;

    if (e==NULL || e->n_temp_set < 1) return(NULL);
    if ( (s1 = (Temp_Rec *) calloc (e->n_temp_set,sizeof (Temp_Rec)) ) == NULL ) {
       printf("\n Fail allocating  Temp Rec");
       return(NULL);
    }
    e->temp = &s1[0];
    for (i=1, s=e->temp; i< e->n_temp_set; s->next=&s1[i], s=s->next, i++);
    return(e->temp);
}


Dopcen_Est*  Allocate_Dopcen_Est( Proc_Parm* e )
{
    Dopcen_Est *s,*s1;
    int i;

    if (e==NULL || e->n_dopcen < 1) return(NULL);
    if ( (s1 = (Dopcen_Est *) calloc (e->n_dopcen,sizeof (Dopcen_Est)) ) == NULL ) {
       printf("\n Fail allocating  Dopcen Est");
       return(NULL);
    }
    e->dopcen_est = &s1[0];
    for (i=1, s=e->dopcen_est; i< e->n_dopcen; s->next=&s1[i], s=s->next, i++);
    return(e->dopcen_est);
}


SRGR_Coefset*  Allocate_SRGR_Coefset( Proc_Parm* e )
{
    SRGR_Coefset *s,*s1;
    int i;

    if (e==NULL || e->n_srgr < 1) return(NULL);
    if ( (s1 = (SRGR_Coefset *) calloc (e->n_srgr,sizeof (SRGR_Coefset)) ) == NULL ) {
       printf("\n Fail allocating SRGR Coefset");
       return(NULL);
    }
    e->srgr_coefset = &s1[0];
    for (i=1, s=e->srgr_coefset; i< e->n_srgr; s->next=&s1[i], s=s->next, i++);
    return(e->srgr_coefset);
}



int  Decode_SARL_DPR( unsigned char* buf, SARL_ptr* t ) 
{
    Proc_Parm* e = t->detail;
    int n_data = t->descript.n_det_proc;
    desc_rec* d;
    Beam_Info    *bi;
    Pix_Count    *pc;
    Temp_Rec     *tr;
    Dopcen_Est   *de;
    SRGR_Coefset *sc;
    int i=1, off;

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
    printf("\n Decode Detail Processing record\n");
 
    /* process the record */
    d = &(e->desc);
    d->rec_seq  = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    Record_length = 0;
    for (i=8; i<12; i++) {
       Record_length = Record_length + *(buf+i);
       Record_length *= 256;
    }
    Record_length /= 256;
    Current_length = 12;
    DP_entry = COM_entry = 0;
    e->seq_num = (short) Get_I4( &buf[12],  4);
    strNcpy(e->spare_dpp_1, (char *) (buf+16), 4);
    strNcpy(e->inp_media, (char *) (buf+20), 3);
    e->n_tape_id = (short) Get_I4( &buf[23],  4);
    for (i=0, off=27; i<10; i++, off+=8) {
	strNcpy(e->tape_id[i], (char *) (buf+off),  8);
    }
    strNcpy(e->exp_ing_start, (char *) (buf+off),  21); off+=21;
    strNcpy(e->exp_ing_stop, (char *) (buf+off),  21); off+=21;
    strNcpy(e->act_ing_start, (char *) (buf+off),  21); off+=21;
    strNcpy(e->act_ing_stop, (char *) (buf+off),  21); off+=21;
    strNcpy(e->proc_start, (char *) (buf+off),  21); off+=21;
    strNcpy(e->proc_stop, (char *) (buf+off),  21); off+=21;
    for (i=0;i<10; i++, off+=16) {
	e->mn_sig_lev[i] = Get_F4( &buf[off],  16);
    }
    e->src_data_ind = (short) Get_I4( &buf[off],  4); off+=4;
    e->miss_ln = Get_I4( &buf[off],  8); off+=8;
    e->rej_ln = Get_I4( &buf[off],  8); off+=8;
    e->large_gap = Get_I4( &buf[off],  8); off+=8;
    e->bit_error_rate = Get_F4( &buf[off],  16); off+=16;
    e->fm_crc_err = Get_F4( &buf[off],  16); off+=16;
    e->date_incons = Get_I4( &buf[off],  8); off+=8;
    e->prf_changes = Get_I4( &buf[off],  8); off+=8;
    e->delay_changes = Get_I4( &buf[off],  8); off+=8;
    e->skipd_frams = Get_I4( &buf[off],  8); off+=8;
    e->rej_bf_start = Get_I4( &buf[off],  8); off+=8;
    e->rej_few_fram = Get_I4( &buf[off],  8); off+=8;
    e->rej_many_fram = Get_I4( &buf[off],  8); off+=8;
    e->rej_mchn_err = Get_I4( &buf[off],  8); off+=8;
    e->rej_vchn_err = Get_I4( &buf[off],  8); off+=8;
    e->rej_rec_type = Get_I4( &buf[off],  8); off+=8;
    e->prd_qual_ind = Get_I4( &buf[off],  4); off+=4;
    strNcpy(e->qc_rating, (char *) (buf+off),  6); off+=6;
    strNcpy(e->qc_comment, (char *) (buf+off),  80); off+=80;
    strNcpy(e->sens_config, (char *) (buf+off),  10); off+=10;
    strNcpy(e->sens_orient, (char *) (buf+off),  9); off+=9;
    strNcpy(e->sych_marker, (char *) (buf+off),  8); off+=8;
    strNcpy(e->rng_ref_src, (char *) (buf+off),  12); off+=12;
    for (i=0;i<4; i++, off+=16) {
	e->rng_amp_coef[i] = Get_F4( &buf[off],  16);
    }
    for (i=0;i<4; i++, off+=16) {
	e->rng_phas_coef[i] = Get_F4( &buf[off],  16);
    }
    for (i=0;i<4; i++, off+=16) {
	e->err_amp_coef[i] = Get_F4( &buf[off],  16);
    }
    for (i=0;i<4; i++, off+=16) {
	e->err_phas_coef[i] = Get_F4( &buf[off],  16);
    }
    e->pulse_bandw = Get_I4( &buf[off],  4); off+=4;
    strNcpy(e->adc_samp_rate, (char *) (buf+off),  5); off+=5;
    e->rep_agc_attn = Get_F4( &buf[off],  16); off+=16;
    e->gn_corctn_fctr = Get_F4( &buf[off],  16); off+=16;
    e->rep_energy_gn = Get_F4( &buf[off],  16); off+=16;
    strNcpy(e->orb_data_src, (char *) (buf+off),  11); off+=11;
    e->pulse_cnt_1 = Get_I4( &buf[off],  4); off+=4;
    e->pulse_cnt_2 = Get_I4( &buf[off],  4); off+=4;
    strNcpy(e->beam_edge_rqd, (char *) (buf+off),  3); off+=3;
    e->beam_edge_conf = Get_F4( &buf[off],  16); off+=16;
    e->pix_overlap = Get_I4( &buf[off],  4); off+=4;
    e->n_beams = Get_I4( &buf[off],  4); off+=4;
    
    bi = Allocate_Beam_Info( e );
    while( bi != NULL) {
	strNcpy(bi->beam_type, (char *) (buf+off), 3); off+=3;
	strNcpy(bi->beam_look_src, (char *) (buf+off), 9); off+=9;
        bi->beam_look_ang = Get_F4( &buf[off],  16); off+=16;
        bi->prf = Get_F4( &buf[off],  16); off+=16;
	bi = bi->next;
    }
    
    e->n_pix_updates = Get_I4( &buf[off],  4); off+=4;
    pc = Allocate_Pix_Count( e);
    while( pc != NULL) {
	strNcpy(pc->pix_update, (char *) (buf+off), 21); off+=21;
	for (i=0;i<4;i++,off+=4) 
            pc->n_pix[i] = Get_I4( &buf[off],  4); 
	pc = pc->next;
    }
    e->pwin_start = Get_F4( &buf[off],  16); off+=16;
    e->pwin_end = Get_F4( &buf[off],  16); off+=16;
    strNcpy(e->recd_type, (char *) (buf+off),  9); off+=9;
    e->temp_set_inc = Get_F4( &buf[off],  16); off+=16;
    
    e->n_temp_set = Get_I4( &buf[off],  4); off+=4;
    tr = Allocate_Temp_Rec( e );
    while( tr != NULL) {
        for (i=0;i<4;i++,off+=4) 
            tr->temp_set[i] = Get_I4( &buf[off], 4);
	tr = tr->next;
    }
    
    e->n_image_pix = Get_I4( &buf[off],  8); off+=8;
    e->prc_zero_pix = Get_F4( &buf[off],  16); off+=16;
    e->prc_satur_pix = Get_F4( &buf[off],  16); off+=16;
    e->img_hist_mean = Get_F4( &buf[off],  16); off+=16;
    e->img_cumu_dist[0] = Get_F4( &buf[off],  16); off+=16;
    e->img_cumu_dist[1] = Get_F4( &buf[off],  16); off+=16;
    e->img_cumu_dist[2] = Get_F4( &buf[off],  16); off+=16;
    e->pre_img_gn = Get_F4( &buf[off],  16); off+=16;
    e->post_img_gn = Get_F4( &buf[off],  16); off+=16;
    e->dopcen_inc = Get_F4( &buf[off],  16); off+=16;
    
    e->n_dopcen = Get_I4( &buf[off],  4); off+=4;
    de = Allocate_Dopcen_Est( e );
    while( de != NULL) {
        de->dopcen_conf = Get_F4( &buf[off],  16); off+=16;
        de->dopcen_ref_tim = Get_F4( &buf[off],  16); off+=16;
        for (i=0;i<4;i++,off+=16) 
            de->dopcen_coef[i] = Get_F4( &buf[off],  16); off+=16;
	de = de->next;
    }
    e->dopamb_err = Get_I4( &buf[off],  4); off+=4;
    e->dopamb_conf = Get_F4( &buf[off],  16); off+=16;
    for (i=0; i<7; i++, off+=16) 
         e->eph_orb_data[i] = Get_F4( &buf[off],  16);
    strNcpy( e->appl_type,  (char *) (buf+off),  12); off+=12;  
    e->first_lntim = Get_F4( &buf[off],  16); off+=22;
    e->lntim_inc = Get_F4( &buf[off],  16); off+=22;
    
    e->n_srgr = Get_I4( &buf[off],  4); off+=4;
    sc = Allocate_SRGR_Coefset( e );
    while( sc != NULL) {
        strNcpy(sc->srgr_update, (char *) (buf+off),  21); off+=21;  
        for (i=0;i<6;i++,off+=16) 
	    sc->srgr_coef[i] = Get_F4( &buf[off],  16);
	sc = sc->next;
    }
    
    e->pixel_spacing = Get_F4( &buf[off],  16); off+=16;
    strNcpy( e->pics_reqd,  (char *) (buf+off),  3); off+=3; 
    strNcpy( e->wo_number,  (char *) (buf+off),  8); off+=8; 
    strNcpy( e->wo_date,  (char *) (buf+off),  20); off+=20; 
    strNcpy( e->satellite_id,  (char *) (buf+off),  10); off+=10; 
    strNcpy( e->user_id,  (char *) (buf+off),  20); off+=20; 
    strNcpy( e->complete_msg,  (char *) (buf+off),  3); off+=3; 
    strNcpy( e->scene_id,  (char *) (buf+off),  15); off+=15; 
    strNcpy( e->density_in,  (char *) (buf+off),  4); off+=4; 
    strNcpy( e->media_id,  (char *) (buf+off),  8); off+=8;
    e->angle_first = Get_F4( &buf[off],  16); off+=16;
    e->angle_last = Get_F4( &buf[off],  16); off+=16;
    strNcpy( e->prod_type,  (char *) (buf+off), 3); off+=3; 
    strNcpy( e->map_system,  (char *) (buf+off),  16); off+=16; 
    e->centre_lat = Get_F4( &buf[off],  22); off+=22;  
    e->centre_long = Get_F4( &buf[off],  22); off+=22;  
    e->span_x = Get_F4( &buf[off], 22); off+=22;  
    e->span_y = Get_F4( &buf[off],  22); off+=22; 
    strNcpy( e->apply_dtm,  (char *) (buf+off),  3); off+=3; 
    strNcpy( e->density_out,  (char *) (buf+off),  4); off+=4; 
    strNcpy( e->spare_dpp_2,  (char *) (buf+off),  247); off+=247;
    
    DP_entry = COM_entry;
    return(DECODE_OK);
}



int Decode_SARL_CDR( unsigned char* buf, SARL_ptr* t ) 
{
    Calib_Data* e = t->calibration;
    int n_data = t->descript.n_cal;
    desc_rec* d;
    Beam_Info    *bi;
    Pix_Count    *pc;
    Temp_Rec     *tr;
    Dopcen_Est   *de;
    SRGR_Coefset *sc;
    int i=1, off;

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
    printf("\n Decode Calibration Data record\n");
 
    /* process the record */
    d = &(e->desc);
    d->rec_seq  = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    Record_length = 0;
    for (i=8; i<12; i++) {
       Record_length = Record_length + *(buf+i);
       Record_length *= 256;
    }
    Record_length /= 256;
    Current_length = 12;
    CD_entry = COM_entry = 0;
    e->seq_num = (short) Get_I4( &buf[12],  4);
    strNcpy(e->spare_cdr_1, (char *) (buf+16), 4); off=20;
    strNcpy(e->spare_cdr_2, (char *) (buf+off), 255); off+=255;
    
    CD_entry = COM_entry;
    return(DECODE_OK);
}




int  Decode_SARL_FRR( unsigned char* buf, SARL_ptr* t ) 
{
    Fac_Related* r = t->facility;
    int n_data = t->descript.n_fac_data;
    desc_rec* d;
    int i=1;
    unsigned int *lb;

    lb = (unsigned int *) buf;

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
    printf("\n Decode Facility Related record\n");
 
    /* process the record */
    d = &(r->desc);
    d->rec_seq  = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    if (d->rec_type != LFTYPE) {
       /* for now take the ice motion , ice class, wave spectra and stuff it into a buffer */
       i =  ntohl( lb[2]  ) -12;
       r->bogus = (char *) malloc( i*sizeof(char) );
       strNcpy(r->bogus, (char *) (buf+12), i);
    }
    else {
    Record_length = 0;
    for (i=8; i<12; i++) {
       Record_length = Record_length + *(buf+i);
       Record_length *= 256;
    }
    Record_length /= 256;
    Current_length = 12;
    FR_entry = COM_entry = 0;
       r->seq_num = (short) Get_I4( &buf[12],  4);
       strNcpy(r->datatake_ID, (char*) (buf+20), 14);
       strNcpy(r->image_ID, (char*) (buf+34), 11);
       strNcpy(r->corr_year, (char*) (buf+45), 4);
       strNcpy(r->corr_GMT, (char*) (buf+50), 17);
       strNcpy(r->site_name, (char*) (buf+67), 33);
       strNcpy(r->data_year, (char*) (buf+100), 4);
       strNcpy(r->center_GMT, (char*) (buf+105), 17);
       r->center_LAT = Get_F4( &buf[122], 17);
       r->center_LON = Get_F4( &buf[139], 17);
       r->near_start_LAT = Get_F4( &buf[156], 17);
       r->near_start_LON = Get_F4( &buf[173], 17);
       r->near_end_LAT = Get_F4( &buf[190], 17);
       r->near_end_LON = Get_F4( &buf[207], 17);
       r->far_start_LAT = Get_F4( &buf[224], 17);
       r->far_start_LON = Get_F4( &buf[241], 17);
       r->far_end_LAT = Get_F4( &buf[258], 17);
       r->far_end_LON = Get_F4( &buf[275], 17);
       r->actual_azimuth = Get_F4( &buf[292], 17);
       r->actual_range = Get_F4( &buf[309], 17);
       r->actual_pixels = Get_I4( &buf[326], 9);
       r->actual_lines = Get_I4( &buf[335], 9);
       r->total_pixels = Get_I4( &buf[344], 9);
       r->total_lines = Get_I4( &buf[353], 9);
       strNcpy(r->media_id, (char*) (buf+362), 7);
       r->start_address = Get_I4( &buf[369], 17);
       r->end_address = Get_I4( &buf[386], 17);
       strNcpy(r->platform_name, (char*) (buf+403), 17);
       strNcpy(r->sensor_mode, (char*) (buf+420), 33);
       r->PRF = Get_F4( &buf[453], 17);
       r->ant_look_angle = Get_F4( &buf[470], 17);
       r->data_rate = Get_F4( &buf[487], 17);
       r->data_win_pos = Get_F4( &buf[504], 17);
       r->range_gate_del = Get_F4( &buf[521], 17);
       r->track_angle = Get_F4( &buf[538], 17);
       strNcpy(r->ASC_DESC, (char*) (buf+555), 2);
       /* Add one to offsets from here on. */
       r->S_C_altitude = Get_F4( &buf[557], 17);
       r->X_position = (double) Get_F4( &buf[574], 23);
       r->Y_position = (double) Get_F4( &buf[597], 23);
       r->Z_position = (double) Get_F4( &buf[620], 23);
       r->X_velocity = (double) Get_F4( &buf[643], 23);
       r->Y_velocity = (double) Get_F4( &buf[666], 23);
       r->Z_velocity = (double) Get_F4( &buf[689], 23);
       r->roll = Get_F4( &buf[712], 15);
       r->yaw = Get_F4( &buf[727], 15);
       r->pitch = Get_F4( &buf[742], 15);
       r->roll_flag = Get_I4( &buf[757], 5);
       r->yaw_flag = Get_I4( &buf[762], 5);
       r->pitch_flag = Get_I4( &buf[767], 5);
       r->roll_rate = Get_F4( &buf[772], 15);
       r->yaw_rate = Get_F4( &buf[787], 15);
       r->pitch_rate = Get_F4( &buf[802], 15);
       r->roll_rate_flag = Get_I4( &buf[817], 5);
       r->yaw_rate_flag = Get_I4( &buf[822], 5);
       r->pitch_rate_flag = Get_I4( &buf[827], 5);
       r->nadir_radius = Get_F4( &buf[832], 17);
       r->image_radius = Get_F4( &buf[849], 17);
       r->incidence_angle = Get_F4( &buf[866], 17);
       strNcpy(r->proc_version, (char*) (buf+883), 8);
       strNcpy(r->proc_type, (char*) (buf+891), 3);
       strNcpy(r->type_ephemeris, (char *) (buf+894), 2);
       r->looks_azimuth = Get_F4( &buf[896], 17);
       r->looks_range = Get_F4( &buf[913], 17);
       r->azi_weight_fac = Get_F4( &buf[930], 17);
       r->range_weight_fac = Get_F4( &buf[947], 17);
       strNcpy(r->look_energy_eq, (char*) (buf+964), 4);
       r->induced_azimuth = Get_F4( &buf[968], 17);
       r->induced_range = Get_F4( &buf[985], 17);
       r->gain = Get_F4( &buf[1002], 17);
       r->swath_velocity = Get_F4( &buf[1019], 17);
       r->squint_angle = Get_F4( &buf[1036], 17);
       r->avg_terrain_ht = Get_F4( &buf[1053], 17);
       r->processor_gain = Get_I4( &buf[1070], 4);
       strNcpy(r->deskew, (char*) (buf+1074), 4);
       strNcpy(r->gnd_slant_flag, (char*) (buf+1078), 7);
       r->sl_rng_1st_pix = Get_F4( &buf[1085], 17);
       r->sl_rng_last_pix = Get_F4( &buf[1102], 17);
       r->start_sample = Get_I4( &buf[1119], 9);
       strNcpy(r->clutterlock_flg, (char*) (buf+1128), 4);
       r->dop_frq_const = Get_F4( &buf[1132], 17);
       r->dop_frq_slope = Get_F4( &buf[1149], 17);
       r->dop_frq_quad = Get_F4( &buf[1166], 17);
       strNcpy(r->autofocus_flag, (char*) (buf+1183), 4);
       r->dop_frq_r_cnst = Get_F4( &buf[1187], 17);
       r->dop_frq_r_slope = Get_F4( &buf[1204], 17);
       r->dop_frq_r_quad = Get_F4( &buf[1221], 17);
       r->azi_res = Get_F4( &buf[1238], 17);
       r->rng_res = Get_F4( &buf[1255], 17);
       r->azimuth_pixel = Get_F4( &buf[1272], 17);
       r->range_pixel = Get_F4( &buf[1289], 17);
       strNcpy(r->OBRC_flag, (char*) (buf+1306), 4);
       r->bits_sample = Get_I4( &buf[1310], 5);
       r->calib_est = Get_F4( &buf[1315], 17);
       r->bit_err_rate = Get_F4( &buf[1332], 17);
       r->SNR = Get_F4( &buf[1349], 17);
       r->est_noise_flr = Get_F4( &buf[1366], 17);
       r->radio_m_resol = Get_F4( &buf[1383], 17);
       r->satur_points = Get_I4( &buf[1400], 9);
       strNcpy(r->spec_flag, (char*) (buf+1409), 4);
#ifndef PRE_RADARSAT
       r->repl_agc = Get_F4( &buf[1413], 17);
       r->temp_rx_lna = Get_F4( &buf[1430], 17);
       r->temp_rx_sub = Get_F4( &buf[1447], 17);
       r->temp_rx_prot = Get_F4( &buf[1464], 17);
       r->temp_cal_sys = Get_F4( &buf[1481], 17);
       r->rx_agc = Get_F4( &buf[1498], 17);
       r->pre_cal1_pow = Get_F4( &buf[1515], 17);
       r->pre_cal2_pow = Get_F4( &buf[1532], 17);
       r->post_cal1_pow = Get_F4( &buf[1549], 17);
       r->post_cal2_pow = Get_F4( &buf[1566], 17);
       r->repl_pow = Get_F4( &buf[1583], 17);
       r->ssar_roll_ang = Get_F4( &buf[1600], 17);
       strNcpy(r->comment, (char*) (buf+1617), 100);
#endif
    }
    FR_entry = COM_entry;
    return(DECODE_OK);
}



void SetCurrentSARL( SARL_ptr* tree )
{
   Global_tree = tree;
}

SARL_ptr* GetCurrentSARL(void) 
{
   return( Global_tree );
}

















