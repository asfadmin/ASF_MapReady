static char *sccs = "@(#)ims_processLdr.c	5.1  03/17/96";
#include    <stdio.h>
#include    <string.h>
#include    <stdlib.h>
#include    <ctype.h>
#include    <sys/types.h>
#include    <unistd.h>
#include    <sys/utsname.h>

#include    "../lincl/defs.h"
#include    "../lincl/extern.h"
#include    <ims_dbms.h>
#include    <ims_const.h>
#include    <ims_msg.h>
#include    <ims_qi.h>

SARL_ptr *Global_tree;

int Debug;
int Dump;
static  IMS_MSG_STRUCT * msgDesc_save;
static  int  status_save;

#define LFTYPE2  210   /*  the new CEOS format has facility records
    with this as its value, so it is added here.  */

unsigned char* work;

void SetCurrentSARL( SARL_ptr* tree );

SARL_ptr* GetCurrentSARL(void);

int process_ldr( char * name_ldr, int desc, short  debug,
    short dump, IMS_MSG_STRUCT * msgDesc )
{
    FILE *in;
    SARL_ptr* tree;
    int ret;


    int  Read_FDR_SARL( FILE *, SARL_ptr *, int );


    work = NULL;
    Debug = debug;
    Dump = dump;
    msgDesc_save = msgDesc;
    status_save = IMS_OK;
    /* open the leader file */
    in = Open_LDR( name_ldr, READ_ONLY);
    if(  in  ==  NULL ) {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "ProcessLdr: Could not open the leader file for reading:  %s",
            name_ldr);
        return( IMS_ERROR );
    }

    /* allocate memory for a SARL structure */
    if ( ( tree = Allocate_SARL() ) == NULL) {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "ProcessLdr:  Failed to Allocate_SARL structure.");
        status_save = IMS_ERROR;
        return( IMS_ERROR );
    }

    SetCurrentSARL( tree );

    /* read the File Descriptor Record, transfer to "descript" structure */

    if ( !Read_FDR_SARL( in, tree, desc ) ) {
        (void) ims_msg( msgDesc, IMS_ERROR,
            "ProcessLdr:  Error in Read_FDR_SARL.");
        return( IMS_ERROR );
    }

    /* based the contents of the leader file FDR, allocate space for
        each of record */

    if ( Allocate_SARL_ODR(tree) ) {
        ret=Read_ALL_SARL( in, tree, desc );
        switch (ret) {
            case (END_OF_FILE) :
                (void) Write_ODL_SARL( tree);
                break;
            default :
                (void) ims_msg( msgDesc, IMS_ERROR,
            "ProcessLdr:  Aborting the read of the ldr or tlr file.");
                Free_SARL( tree );
                return( IMS_ERROR );
        }
    }

    /* Free allocated SARL structure */
    Free_SARL( tree );
    (void) fclose(in);

    return( IMS_OK);
}


int Write_ODL_SARL( SARL_ptr* tree)
{
    /* ****** in reader, this would write the file.
        this taken out here.  */
   int  out;
   int ret;

   out = -1;

   SetCurrentSARL( tree );
   tree->write_count=0;
   ret=Write_L_FDR_ODL( Get_L_FDR(tree), out );
   ret=Write_L_DSS_ODL( Get_L_DSS(tree), out, 0 );
   ret=Write_L_MP_ODL( Get_L_MP(tree), out, 0 );
   ret=Write_L_PP_ODL( Get_L_PP(tree), out, 0 );
   ret=Write_L_AT_ODL( Get_L_AT(tree), out, 0 );
   ret=Write_L_RD_ODL( Get_L_RD(tree), out, 0 );
   ret=Write_L_RC_ODL( Get_L_RC(tree), out, 0 );
   ret=Write_L_DQS_ODL( Get_L_DQS(tree), out, 0 );
   ret=Write_L_DH_ODL( Get_L_DH(tree), out, 0 );
   ret=Write_L_RS_ODL( Get_L_RS(tree), out, 0 );
   ret=Write_L_DE_ODL( Get_L_DE(tree), out, 0 );
   ret=Write_L_FR_ODL( Get_L_FR(tree), out, 0 );
   if( out != -1 ) (void) close( out );
   ret++;
   return( IMS_OK );
}


Sarl_Desc_Rec* Get_L_FDR( SARL_ptr* t )
{
    if (t != NULL ) {
       return( &(t->descript) );
    }
    else
       return( (Sarl_Desc_Rec*) NULL);
}

int Write_L_FDR_ODL( Sarl_Desc_Rec* t, int out )
{
      SARL_ptr *current;
     unsigned char* buf = work;
     unsigned char* tb;
     desc_rec *d = &(t->desc);
     int nitems;
     int i, off;
     int Zer0 = 0;

     /* Write the contents of the FDR structure of an ODL tree to a
        CEOS leader file */
     if (t == (Sarl_Desc_Rec*) NULL) return( IMS_ERROR );

     if (out != -1 ) {
        nitems=t->desc.length;
        if (nitems!=720) {
           nitems=720;
           t->desc.length=720;
        }
        i=d->rec_seq;
        zero_set(buf,nitems);
        if(  Debug )   (void)  printf(
            "\n Write the FDR struct to the CEOS file\n");

        if ( (current=GetCurrentSARL()) != NULL )
           (void) Check_Rec_Seq( &i, buf, current, 0);
        else
          cvt2char(&i, buf);
        put_byte((buf+4), d->rec_sub1);
        put_byte((buf+5), d->rec_type);
        put_byte((buf+6), d->rec_sub2);
        put_byte((buf+7), d->rec_sub3);
        tb=&buf[4];
        if ( !MATCH_RECORD(tb, LD1SUB, LDTYPE, LD2SUB, LD3SUB) ) {
           if(  Debug )  (void) printf(
            "\n Descriptor record and subtype mismatch - expecting FDR codes");
        }
        cvt2char(&nitems, (buf+8));
        put_chars((char *) (buf+12), t->ascii_flag, 2);
        put_blanks( (char *) (buf+14), 2);
        put_chars((char *) (buf+16), t->format_doc, 12);
        put_chars((char *) (buf+28), t->format_rev, 2);
        put_chars((char *) (buf+30), t->design_rev, 2);
        put_chars((char *) (buf+32), t->software_id, 12);
        put_I4(t->file_num, "%4d", 4, (buf+44));
        put_chars((char *) (buf+48), t->file_name, 16);
        put_chars((char *) (buf+64), t->rec_seq, 4);
        put_I4(t->seq_loc, "%8d", 8, (buf+68));
        put_I4(t->seq_len, "%4d", 4, (buf+76));
        put_chars((char *) (buf+80), t->rec_code, 4);
        put_I4(t->code_loc, "%8d", 8, (buf+84));
        put_I4(t->code_len, "%4d", 4, (buf+92));
        put_chars((char *) (buf+96), t->rec_len, 4);
        put_I4(t->rlen_loc, "%8d", 8, (buf+100));
        put_I4(t->rlen_len, "%4d", 4, (buf+108));
        put_blanks( (char *) (buf+112), 68 );
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
        for (i=0, off=360; i<10;i++, off+=6)
            put_I4( Zer0, "%6d", 6, (buf+off));
        put_I4(t->n_faci_data, "%6d", 6, (buf+420));
        put_I4(t->l_faci_data, "%6d", 6, (buf+426));
        put_blanks( (char *) (buf+432), 288 );
        i = write( out, buf, nitems);
     }

     if (Dump ) {

        (void) printf(
    "\n**************** Begin of FDR record *******************\n");
        (void) printf("\n RECORD SEQUENCE        %ld",d->rec_seq);
        (void) printf("\n RECORD SUB 1           %d",d->rec_sub1);
        (void) printf("\n RECORD TYPE            %d",d->rec_type);
        (void) printf("\n RECORD SUB 2           %d",d->rec_sub2);
        (void) printf("\n RECORD SUB 3           %d",d->rec_sub3);
        (void) printf("\n RECORD LENGTH          %ld\n",d->length);
        (void) printf("\n ACSII FLAG             %s",t->ascii_flag);
        (void) printf("\n DOCUMENT FORMAT        %s",t->format_doc);
        (void) printf("\n FORMAT REVISION        %s",t->format_rev);
        (void) printf("\n RCRD FRMT REV LVL      %s",t->design_rev);
        (void) printf("\n SOFTWARE ID            %s",t->software_id);
        (void) printf("\n FILE NUMBER            %d",t->file_num);
        (void) printf("\n FILE NAME              %s",t->file_name);
        (void) printf("\n RCRD SEQ FLAG          %s",t->rec_seq);
        (void) printf("\n SEQ NUM LOC            %ld",t->seq_loc);
        (void) printf("\n SEQ NUM FLD LEN        %d",t->seq_len);
        (void) printf("\n RCRD CODE FLAG         %s",t->rec_code);
        (void) printf("\n RCRD CODE LOC          %ld",t->code_loc);
        (void) printf("\n RCRD CODE FLD LEN      %d",t->code_len);
        (void) printf("\n RCRD LEN FLAG          %s",t->rec_len);
        (void) printf("\n RCRD LENGTH LOC        %ld",t->rlen_loc);
        (void) printf("\n RCRD LEN FLD LEN       %d ",t->rlen_len);
        (void) printf("\n NUM DATA SUM RCRDS     %ld",t->n_dataset);
        (void) printf("\n DSS RCRD LEN           %ld",t->l_dataset);
        (void) printf("\n NUM MAP PROJ RCRDS     %ld",t->n_map_proj);
        (void) printf("\n M P RCRD LEN           %ld",t->l_map_proj);
        (void) printf("\n NUM PLAT POS RCRDS     %ld",t->n_plat_pos);
        (void) printf("\n P P RCRD LEN           %ld",t->l_plat_pos);
        (void) printf("\n NUM ATT DATA RCRDS     %ld",t->n_att_data);
        (void) printf("\n A D RCRD LEN           %ld",t->l_att_data);
        (void) printf("\n NUM RADI DATA RCRDS    %ld",t->n_radi_data);
        (void) printf("\n R D RCRD LEN           %ld",t->l_radi_data);
        (void) printf("\n NUM RADI COMP RCRDS    %ld",t->n_radi_comp);
        (void) printf("\n R C RCRD LEN           %ld",t->l_radi_comp);
        (void) printf("\n NUM QUAL SUM RCRDS     %ld",t->n_qual_sum);
        (void) printf("\n Q S RCRD LEN           %ld",t->l_qual_sum);
        (void) printf("\n NUM DATA HIST RCRDS    %ld",t->n_data_hist);
        (void) printf("\n D H RCRD LEN           %ld",t->l_data_hist);
        (void) printf("\n NUM RNG SPCTR RCRDS    %ld",t->n_rang_spec);
        (void) printf("\n R S RCRD LEN           %ld",t->l_rang_spec);
        (void) printf("\n NUM DGTL ELEV RCRDS    %ld",t->n_dem_desc);
        (void) printf("\n D E RCRD LEN           %ld",t->l_dem_desc);
        (void) printf("\n NUM RDR UPDT RCRDS     %ld",t->n_radar_par);
        (void) printf("\n R P RCRD LEN           %ld",t->l_radar_par);
        (void) printf("\n NUM ANNT DATA RCRDS    %ld",t->n_anno_data);
        (void) printf("\n A D RCRD LEN           %ld",t->l_anno_data);
        (void) printf("\n NUM DTLD PROC RCRDS    %ld",t->n_det_proc);
        (void) printf("\n D P RCRD LEN           %ld",t->l_det_proc);
        (void) printf("\n NUM CALB DATA RCRDS    %ld",t->n_cal);
        (void) printf("\n C B RCRD LEN           %ld",t->l_cal);
        (void) printf("\n NUM GND CTL RCRDS      %ld",t->n_gcp);
        (void) printf("\n G C RCRD LEN           %ld",t->l_gcp);
        (void) printf("\n NUM FACI DATA RCRDS    %ld",t->n_faci_data);
        (void) printf("\n F D RCRD LEN           %ld\n",t->l_faci_data);
   (void) printf("\n**************** end of FDR record *******************\n");
     }
     return( IMS_OK );
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

int Write_L_DSS_ODL( Dataset_Sum* ds, int out, int mode )
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    desc_rec *d = &(ds->desc);
    int nitems;
    int i, off;

    /* Write the contents of a Dataset Summary record stored in an ODL
      tree to a CEOS leader file */

    if (ds == (Dataset_Sum*) NULL) return( IMS_ERROR );

    if ( out != -1 ) {
       /* allocate a temp buffer */
       nitems=ds->desc.length;

       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          (void) Write_L_DSS_ODL(ds, out, 1);
          ds->desc.length=get_rec_length();
          if(  Debug )  (void) printf(
            "\n Calculated record length for Dataset Summary %ld\n",
            ds->desc.length);
          nitems=ds->desc.length;
          reset_rec_length();
       }

       i=d->rec_seq;
       zero_set(buf,nitems);
       if(  Debug )  (void) printf(
            "\n Write the DSS struct to the CEOS file\n");

       if ( (current=GetCurrentSARL()) != NULL )
          (void) Check_Rec_Seq( &i, buf, current, 0);
       else
          cvt2char(&i, buf);
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
       cvt2char(&nitems, (buf+8));
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LS1SUB, LSTYPE, LS2SUB, LS3SUB) ) {
          (void) ims_msg( msgDesc_save, IMS_INFO,
             "ProcessLdr: Header mismatch - expecting DSS codes.");
       }
       put_I4(ds->seq_num, "%4d", 4, (buf+12));
       put_I4(ds->sar_chn, "%4d", 4, (buf+16));
       put_chars((char*) (buf+20), ds->scene_id, 16);
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
       put_blanks( (char *) (buf+292), 16);
       put_F4(ds->terrain_h, "%16.7E", 16, (buf+308));
       put_I4(ds->sc_lin, "%8d", 8, (buf+324));
       put_I4(ds->sc_pix, "%8d", 8, (buf+332));
       put_F4(ds->scene_len, "%16.7E", 16, (buf+340));
       put_F4(ds->scene_wid, "%16.7E", 16, (buf+356));
       put_blanks( (char *) (buf+372), 16);
       put_I4(ds->nchn, "%4d", 4, (buf+388));
       put_blanks( (char *) (buf+392), 4);
       put_chars((char*) (buf+396), ds->mission_id, 16);
       put_chars((char*) (buf+412), ds->sensor_id, 32);
       put_chars((char*) (buf+444), ds->orbit_num, 8);
       put_F4(ds->plat_lat, "%8.3f", 8, (buf+452));
       put_F4(ds->plat_long, "%8.3f", 8, (buf+460));
       put_F4(ds->plat_head, "%8.3f", 8, (buf+468));
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
       put_blanks( (char *) (buf+702), 8);
       put_F4(ds->fr, "%16.7f", 16, (buf+710));
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
       put_F4(ds->ele_sight, "%16.7f", 16, (buf+898));
       put_F4(ds->mech_sight, "%16.7f", 16, (buf+914));
       put_chars((char*) (buf+930), ds->echo_track, 4);
       put_F4(ds->fa, "%16.7f", 16, (buf+934));
       put_F4(ds->elev_beam, "%16.7f", 16, (buf+950));
       put_F4(ds->azim_beam, "%16.7f", 16, (buf+966));
       put_chars( (char*) (buf+982), ds->sat_bintim, 16);
       put_chars((char*) (buf+998), ds->sat_clktim, 32);
       put_I4(ds->sat_clkinc, "%8d", 8, (buf+1030));
       put_blanks( (char *) (buf+1038), 8);
       put_chars((char*) (buf+1046), ds->fac_id, 16);
       put_chars((char*) (buf+1062), ds->sys_id, 8);
       put_chars((char*) (buf+1070), ds->ver_id, 8);
       put_chars((char*) (buf+1078), ds->fac_code, 16);
       put_chars((char*) (buf+1094), ds->lev_code, 16);
       put_chars((char*) (buf+1110), ds->prod_type, 32);
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
       put_blanks( (char *) (buf+1462), 16);
       put_F4(ds->crt_dopcen[0], "%16.7f", 16, (buf+1478));
       put_F4(ds->crt_dopcen[1], "%16.7f", 16, (buf+1494));
       put_F4(ds->crt_dopcen[2], "%16.7f", 16, (buf+1510));
       put_chars((char*) (buf+1526), ds->time_dir_pix, 8);
       put_chars((char*) (buf+1534), ds->time_dir_lin, 8);
       put_F4(ds->alt_rate[0], "%16.7f", 16, (buf+1542));
       put_F4(ds->alt_rate[1], "%16.7f", 16, (buf+1558));
       put_F4(ds->alt_rate[2], "%16.7f", 16, (buf+1574));
       put_blanks( (char *) (buf+1590), 16);
       put_F4(ds->crt_rate[0], "%16.7f", 16, (buf+1606));
       put_F4(ds->crt_rate[1], "%16.7f", 16, (buf+1622));
       put_F4(ds->crt_rate[2], "%16.7f", 16, (buf+1638));
       put_blanks( (char *) (buf+1654), 16);
       put_chars((char*) (buf+1670), ds->line_cont, 8);
       put_chars((char*) (buf+1678), ds->clutter_lock, 4);
       put_chars((char*) (buf+1682),ds->auto_focus,  4);
       put_F4(ds->line_spacing, "%16.7f", 16, (buf+1686));
       put_F4(ds->pix_spacing, "%16.7f", 16, (buf+1702));
       put_chars((char*) (buf+1718), ds->rngcmp_desg, 16);
        /* ******
       put_blanks((char*) (buf+1734),272);
       put_I4(ds->annot_pts, "%8d", 8, (buf+2006));
       if (ds->annot_pts>0) {
          put_blanks( (char*) (buf+2014), 8);
          for (i=0, off=2022; i< ds->annot_pts; i++) {
              put_I4( ds->annot_line[i], "%8d", 8, (buf+off)); off+=8;
              put_I4( ds->annot_pixel[i], "%8d", 8, (buf+off)); off+=8;
              put_chars((char *) (buf+off), &(ds->annot_text[i][0]), 16);
              off+=16;
          }
       }
        ****** */
       i = write( out, buf, nitems);
    }

    if (Dump ) {

       (void) printf("\n*********** begin of Dataset Summary record " );
       (void) printf("*******************\n");
       (void) printf("\n RECORD SEQUENCE        %ld", d->rec_seq);
       (void) printf("\n RECORD SUB 1           %d",d->rec_sub1);
       (void) printf("\n RECORD TYPE            %d",d->rec_type);
       (void) printf("\n RECORD SUB 2           %d",d->rec_sub2);
       (void) printf("\n RECORD SUB 3           %d",d->rec_sub3);
       (void) printf("\n RECORD LENGTH          %ld\n",d->length);
       (void) printf("\n DSS SEQ NUM            %d",ds->seq_num);
       (void) printf("\n SAR CHNL INDTR         %d",ds->sar_chn);
       (void) printf("\n SCENE INDICATOR        %s",ds->scene_id);
       (void) printf("\n SCENE DESIGNATOR       %s",ds->scene_des);
       (void) printf("\n INPT SCN CTR TRIM      %s",ds->inp_sctim);
       (void) printf("\n ASC/DESCENDING         %s",ds->asc_des);
       (void) printf("\n LAT @ SCN CTR          %16.7f",ds->pro_lat);
       (void) printf("\n LONG @ SCN CTR         %16.7f",ds->pro_long);
       (void) printf("\n SCN CTR HEADING        %16.7f",ds->pro_head);
       (void) printf("\n ELLIP DESIGNATOR       %s",ds->ellip_des);
       (void) printf("\n ELLIP SEMIMAJOR        %16.7f",ds->ellip_maj);
       (void) printf("\n ELLIP SEMIMINOR        %16.7f",ds->ellip_min);
       (void) printf("\n EARTH MASS             %16.7f",ds->earth_mass);
       (void) printf("\n GRAVITATIONAL CNST     %16.7f",ds->grav_const);
       (void) printf("\n ELLIP PARM 1           %16.7f",ds->ellip_j[0]);
       (void) printf("\n ELLIP PARM 2           %16.7f",ds->ellip_j[1]);
       (void) printf("\n ELLIP PARM 3           %16.7f",ds->ellip_j[2]);
       (void) printf("\n AVG TERRAIN HT         %16.7f",ds->terrain_h);
       (void) printf("\n IMG CTR LINE NUM       %ld",ds->sc_lin);
       (void) printf("\n IMG CTR PIX NUM        %ld",ds->sc_pix);
       (void) printf("\n IMAGE LENGTH           %16.7f",ds->scene_len);
       (void) printf("\n IMAGE WIDTH            %16.7f",ds->scene_wid);
       (void) printf("\n NUM SAR CHANNELS       %d",ds->nchn);
       (void) printf("\n MISSION ID             %s",ds->mission_id);
       (void) printf("\n SENSOR ID              %s",ds->sensor_id);
       (void) printf("\n ORBIT NUMBER           %s",ds->orbit_num);
       (void) printf("\n PLAT LAT @ NADIR       %8.3f",ds->plat_lat);
       (void) printf("\n PLAT LONG @ NADIR      %8.3f",ds->plat_long);
       (void) printf("\n PLAT HEADING           %8.3f",ds->plat_head);
       (void) printf("\n SNSR CLK ANGLE         %8.3f",ds->clock_ang);
       (void) printf("\n INCIDENCE ANGLE        %8.3f",ds->incident_ang);
       (void) printf("\n RADAR FREQUENCY        %8.3f",ds->frequency);
       (void) printf("\n RDR WAVELENGTH         %16.7f",ds->wave_length);
       (void) printf("\n MOTION COMP IND        %s",ds->motion_comp);
       (void) printf("\n RNG PULSE CODE         %s",ds->pulse_code);
       (void) printf("\n RNG CHIRP 1            %16.7E",ds->ampl_coef[0]);
       (void) printf("\n RNG CHIRP 2            %16.7E",ds->ampl_coef[1]);
       (void) printf("\n RNG CHIRP 3            %16.7E",ds->ampl_coef[2]);
       (void) printf("\n RNG CHIRP 4            %16.7E",ds->ampl_coef[3]);
       (void) printf("\n RNG CHIRP 5            %16.7E",ds->ampl_coef[4]);
       (void) printf("\n RNG PHASE 1            %16.7E",ds->phas_coef[0]);
       (void) printf("\n RNG PHASE 2            %16.7E",ds->phas_coef[1]);
       (void) printf("\n RNG PHASE 3            %16.7E",ds->phas_coef[2]);
       (void) printf("\n RNG PHASE 4            %16.7E",ds->phas_coef[3]);
       (void) printf("\n RNG PHASE 5            %16.7E",ds->phas_coef[4]);
       (void) printf("\n CHRP EXTRACTION IND    %ld",ds->chirp_ext_ind);
       (void) printf("\n RNG CMPLX SAMPLE RATE  %16.7f",ds->fr);
       (void) printf("\n RNG GATE               %16.7f",ds->rng_gate);
       (void) printf("\n RNG PULSE LEN          %16.7f",ds->rng_length);
       (void) printf("\n BASEBAND FLAG          %s",ds->baseband_f);
       (void) printf("\n RNG COMPRESS FLAG      %s",ds->rngcmp_f);
       (void) printf("\n RCVR GAIN POLAR        %16.7f",ds->gn_polar);
       (void) printf("\n RCVR GAIN CROSS        %16.7f",ds->gn_cross);
       (void) printf("\n QUANT BITS/CHNL        %ld",ds->chn_bits);
       (void) printf("\n QUANTZR DESCRPT        %s",ds->quant_desc);
       (void) printf("\n I CHNL DC BIAS         %16.7f",ds->i_bias);
       (void) printf("\n Q CHNL DC BIAS         %16.7f",ds->q_bias);
       (void) printf("\n I/Q CHNL RATIO         %16.7f",ds->iq_ratio);
       (void) printf("\n ELCTRNC BORESITE       %16.7f",ds->ele_sight);
       (void) printf("\n MECHNCL BORESITE       %16.7f",ds->mech_sight);
       (void) printf("\n ECHO TRK FLAG          %s",ds->echo_track);
       (void) printf("\n NOMINAL PRF            %16.7f",ds->fa);
       (void) printf("\n ANT ELEV BEAM WD       %16.7f",ds->elev_beam);
       (void) printf("\n ANT AZI BEAM WD        %16.7f",ds->azim_beam);
       (void) printf("\n SATLT BINARY TIME      %s",ds->sat_bintim);
       (void) printf("\n SATLT CLOCK TIME       %s",ds->sat_clktim);
       (void) printf("\n SATLT CLOCK INC        %ld",ds->sat_clkinc);
       (void) printf("\n PROCESSING FACILITY    %s",ds->fac_id);
       (void) printf("\n PROCESSING SYSTEM      %s",ds->sys_id);
       (void) printf("\n PROCESSING VERSION     %s",ds->ver_id);
       (void) printf("\n FAC PROCESS CODE       %s",ds->fac_code);
       (void) printf("\n PRODUCT CODE           %s",ds->lev_code);
       (void) printf("\n PRODUCT TYPE           %s",ds->prod_type);
       (void) printf("\n PROCESSING ALGTHM      %s",ds->algor_id);
       (void) printf("\n NUM LOOKS IN AZI       %16.7f",ds->n_azilok);
       (void) printf("\n NUM LOOKS IN RNG       %16.7f",ds->n_rnglok);
       (void) printf("\n BNDWDTH/LOOK IN AZI    %16.7f",ds->bnd_azilok);
       (void) printf("\n BNDWDTH/LOOK IN RNG    %16.7f",ds->bnd_rnglok);
       (void) printf("\n PROC BNDWDTH AZI       %16.7f",ds->bnd_azi);
       (void) printf("\n PROC BNDWDTH RNG       %16.7f",ds->bnd_rng);
       (void) printf("\n AZI WEIGHT FUNC        %s",ds->azi_weight);
       (void) printf("\n RNG WEIGHT FUNC        %s",ds->rng_weight);
       (void) printf("\n DATA INPUT SRC         %s",ds->data_inpsrc);
       (void) printf("\n NOM RESOLUTION RNG     %16.7f",ds->rng_res);
       (void) printf("\n NOM RESOLUTION AZI     %16.7f",ds->azi_res);
       (void) printf("\n RADIO STRETCH BIAS     %16.7f",ds->radi_stretch[0]);
       (void) printf("\n RADIO STRETCH GAIN     %16.7f",ds->radi_stretch[1]);
       (void) printf("\n ALT DOPPLER FREQ 1     %16.7f",ds->alt_dopcen[0]);
       (void) printf("\n ALT DOPPLER FREQ 2     %16.7f",ds->alt_dopcen[1]);
       (void) printf("\n ALT DOPPLER FREQ 3     %16.7f",ds->alt_dopcen[2]);
       (void) printf("\n CRT DOPPLER FREQ 1     %16.7f",ds->crt_dopcen[0]);
       (void) printf("\n CRT DOPPLER FREQ 2     %16.7f",ds->crt_dopcen[1]);
       (void) printf("\n CRT DOPPLER FREQ 3     %16.7f",ds->crt_dopcen[2]);
       (void) printf("\n TIME DIRECT RNG        %s",ds->time_dir_pix);
       (void) printf("\n TIME DIRECT AZI        %s",ds->time_dir_lin);
       (void) printf("\n ALT DOPPLER RATE 1     %16.7f",ds->alt_rate[0]);
       (void) printf("\n ALT DOPPLER RATE 2     %16.7f",ds->alt_rate[1]);
       (void) printf("\n ALT DOPPLER RATE 3     %16.7f",ds->alt_rate[2]);
       (void) printf("\n CRT DOPPLER RATE 1     %16.7f",ds->crt_rate[0]);
       (void) printf("\n CRT DOPPLER RATE 2     %16.7f",ds->crt_rate[1]);
       (void) printf("\n CRT DOPPLER RATE 3     %16.7f",ds->crt_rate[2]);
       (void) printf("\n LINE CONTENT IND       %s",ds->line_cont);
       (void) printf("\n CLUTTER LOCK FLAG      %s",ds->clutter_lock);
       (void) printf("\n AUTOFOCUS FLAG         %s",ds->auto_focus);
       (void) printf("\n LINE SPACING           %16.7f",ds->line_spacing);
       (void) printf("\n PIXEL SPACING          %16.7f",ds->pix_spacing);
       (void) printf("\n RNG COMPRESS DESG      %s",ds->rngcmp_desg);
       /* ********
       (void) printf("\n NUM ANNOTATION PTS     %d",ds->annot_pts);
       for (i=0; i<ds->annot_pts; i++) {
           (void) printf("\n LINE NUM ANNOT START   %d",ds->annot_line[i]);
           (void) printf("\n PIXEL NUM ANNOT START  %d",ds->annot_pixel[i]);
           (void) printf("\n ANNOTATION TEXT        %d",ds->annot_text[i][0]);
       }
       ******* */
       (void) printf("\n*********** end of Dataset Summary  record " );
       (void) printf("********************\n");
    }
    if (ds->next != (Dataset_Sum *) NULL && !mode)
        (void) Write_L_DSS_ODL( ds->next, out, mode );
    return( IMS_OK );
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

int Write_L_MP_ODL( Map_Proj* mp, int out, int mode )
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    desc_rec *d = &(mp->desc);
    int nitems;
    int i, off;


    if (mp== (Map_Proj*) NULL) return( IMS_ERROR );

    if (out != -1  ) {
       /* allocate a temp buffer */
       nitems=mp->desc.length;

       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          (void) Write_L_MP_ODL(mp, out, 1);
          mp->desc.length=get_rec_length();
          if(  Debug )  (void) printf(
            "\n Calculated record length for Map Projection %ld\n",
            mp->desc.length);
          nitems=mp->desc.length;
          reset_rec_length();
       }

       i=d->rec_seq;
       zero_set(buf,nitems);
       if( Debug )  (void) printf(
            "\n Write the Map Proj struct to the CEOS file\n");

       if ( (current=GetCurrentSARL()) != NULL )
          (void) Check_Rec_Seq( &i, buf, current, 0);
       else
          cvt2char(&i, buf);
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
       cvt2char(&nitems, (buf+8));
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LM1SUB, LMTYPE, LM2SUB, LM3SUB) ) {
          (void) ims_msg( msgDesc_save, IMS_INFO,
                "ProcessLdr: Header mismatch - expecting MP codes.");
       }
       put_chars((char*) (buf+28), mp->map_desc, 32);
       put_I4(mp->n_pixel, "%16d", 16, (buf+60));
       put_I4(mp->n_line, "%16d", 16, (buf+76));
       put_F4(mp->pix_spacing, "%16.7f", 16, (buf+92));
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
       for(i=0,off=300; i<3; i++, off=+16)
           put_F4(mp->datum_shift[i], "%16.7f", 16, (buf+off));
       for(i=0,off=348; i<3; i++, off=+16)
           put_F4(mp->aux_datum_shift[i], "%16.7f", 16, (buf+off));
       put_F4(mp->scal_ellip, "%16.7f", 16, (buf+396));
       put_chars((char*) (buf+412), mp->proj_desc, 32);
       put_chars((char*) (buf+444), mp->utm_desc, 32);
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
       for(i=0,off=768; i<4; i++, off=+16)
           put_F4(mp->nsp_stand_par[i], "%16.7f", 16, (buf+off));
       for(i=0,off=832; i<3; i++, off=+16)
           put_F4(mp->nsp_stand_mer[i], "%16.7f", 16, (buf+off));
       put_blanks( (char *) (buf+880), 64);
       for(i=0,off=944; i<8; i++, off=+16)
           put_F4(mp->corner_ne[i], "%16.7f", 16, (buf+off));
       for(i=0,off=1072; i<8; i++, off=+16)
           put_F4(mp->corner_ll[i], "%16.7f", 16, (buf+off));
       for(i=0,off=1200; i<4; i++, off=+16)
           put_F4(mp->terr_height[i], "%16.7f", 16, (buf+off));
       for(i=0,off=1264; i<8; i++, off=+20)
           put_F4(mp->lp_conv_coef[i], "%20.10E", 20, (buf+off));
       for(i=0,off=1424; i<8; i++, off=+20)
           put_F4(mp->mp_conv_coef[i], "%20.10E", 20, (buf+off));
       put_blanks( (char *) (buf+1584), 36);
       i = write( out, buf, nitems);
    }

    if (Dump ) {
       /* Write the contents of the Map Projection record into an
            ODL tree */
    (void) printf("\n**************** begin of Map Projection record " );
    (void) printf("*******************\n");
    (void) printf("\n RECORD SEQUENCE           %ld", d->rec_seq);
    (void) printf("\n RECORD SUB 1              %d",d->rec_sub1);
    (void) printf("\n RECORD TYPE               %d",d->rec_type);
    (void) printf("\n RECORD SUB 2              %d",d->rec_sub2);
    (void) printf("\n RECORD SUB 3              %d",d->rec_sub3);
    (void) printf("\n RECORD LENGTH             %ld\n",d->length);
    (void) printf("\n MAP PROJ DESC             %s",mp->map_desc);
    (void) printf("\n NUM PIX/LINE IMAGE        %ld",mp->n_pixel);
    (void) printf("\n NUM LINES                 %ld",mp->n_line);
    (void) printf("\n NOM INTER-PIX DIST        %16.7f",mp->pix_spacing);
    (void) printf("\n NOM INTER-LINE DIST       %16.7f",mp->line_spacing);
    (void) printf("\n ORIENT OUT SCN CTR        %16.7f",mp->osc_orient);
    (void) printf("\n PLAT ORB INCLINATION      %16.7f",mp->orb_incl);
    (void) printf("\n ACTL ASCEND NODE          %16.7f",mp->asc_node);
    (void) printf("\n PLAT DIST @ SCN CTR       %16.7f",mp->isc_dist);
    (void) printf("\n ALT PLAT REL 2 ELLIP      %16.7f",mp->geo_alt);
    (void) printf("\n ACTL GRND SPD @ NADIR     %16.7f",mp->isc_vel);
    (void) printf("\n PLAT HEADING              %16.7f",mp->plat_head);
    (void) printf("\n REF ELLIPSOID NAME        %s",mp->ref_ellip);
    (void) printf("\n SEMIMAJR AXIS ELLIP       %16.7f",mp->semi_major);
    (void) printf("\n SEMIMINR AXIS ELLIP       %16.7f",mp->semi_minor);
    (void) printf("\n DATUM SHFT PARAM 1        %16.7f",mp->datum_shift[0]);
    (void) printf("\n DATUM SHFT PARAM 2        %16.7f",mp->datum_shift[1]);
    (void) printf("\n DATUM SHFT PARAM 3        %16.7f",mp->datum_shift[2]);
    (void) printf("\n AUX SHFT PARAM 1          %16.7f",mp->aux_datum_shift[0]);
    (void) printf("\n AUX SHFT PARAM 2          %16.7f",mp->aux_datum_shift[1]);
    (void) printf("\n AUX SHFT PARAM 3          %16.7f",mp->aux_datum_shift[2]);
    (void) printf("\n ELLIP SCALE FCTR          %16.7f",mp->scal_ellip);
    (void) printf("\n ALPHANUMERIC MAP DESC     %s",mp->proj_desc);
    (void) printf("\n UTM DESC                  %s",mp->utm_desc);
    (void) printf("\n UTM ZONE SIGN             %s",mp->utm_zone_sig);
    (void) printf("\n UTM MAP ORG/ FALSE EAST   %16.7f",mp->utm_east_orig);
    (void) printf("\n UTM MAP ORIG/ FALSE NORTH %16.7f",mp->utm_north_orig);
    (void) printf("\n UTM PROJ CTR LONG         %16.7f",mp->utm_cent_long);
    (void) printf("\n UTM PROJ CTR LAT          %16.7f",mp->utm_cent_lat);
    (void) printf("\n UTM STD PARALLEL 1        %16.7f",mp->utm_stand_par[0]);
    (void) printf("\n UTM STD PARALLEL 2        %16.7f",mp->utm_stand_par[1]);
    (void) printf("\n UTM SCALE FACTOR          %16.7f",mp->utm_scale);
    (void) printf("\n UPS DESC                  %s",mp->ups_desc);
    (void) printf("\n UPS PROJ CTR LONG         %16.7f",mp->ups_cent_long);
    (void) printf("\n UPS PROJ CTR LAT          %16.7f",mp->ups_cent_lat);
    (void) printf("\n UPS SCALE FACTOR          %16.7f",mp->ups_scale );
    (void) printf("\n NSP DESC                  %s",mp->nsp_desc);
    (void) printf("\n NSP ORG/ FALSE EAST       %16.7f",mp->nsp_east_orig);
    (void) printf("\n NSP ORG/ FALE NORTH       %16.7f",mp->nsp_north_orig);
    (void) printf("\n NSP PROJ CTR LONG         %16.7f",mp->nsp_cent_long);
    (void) printf("\n NSP PROJ CTR LAT          %16.7f",mp->nsp_cent_lat);
    (void) printf("\n NSP STR PARALLEL 1        %16.7f",mp->nsp_stand_par[0]);
    (void) printf("\n NSP STR PARALLEL 2        %16.7f",mp->nsp_stand_par[1]);
    (void) printf("\n NSP STR PARALLEL 3        %16.7f",mp->nsp_stand_par[2]);
    (void) printf("\n NSP STR PARALLEL 4        %16.7f",mp->nsp_stand_par[3]);
    (void) printf("\n NSP STD MERIDIAN 1        %16.7f",mp->nsp_stand_mer[0]);
    (void) printf("\n NSP STD MERIDIAN 2        %16.7f",mp->nsp_stand_mer[1]);
    (void) printf("\n NSP STD MERIDIAN 3        %16.7f",mp->nsp_stand_mer[2]);
    (void) printf("\n TOP LEFT CRNR NORTH       %16.7f",mp->corner_ne[0]);
    (void) printf("\n TOP LEFT CRNR EASR        %16.7f",mp->corner_ne[1]);
    (void) printf("\n TOP RGHT CRNR NORTH       %16.7f",mp->corner_ne[2]);
    (void) printf("\n TOP RGHT CRNR EAST        %16.7f",mp->corner_ne[3]);
    (void) printf("\n BTM RGHT CRNR NORTH       %16.7f",mp->corner_ne[4]);
    (void) printf("\n BTM RGHT CRNR EAST        %16.7f",mp->corner_ne[5]);
    (void) printf("\n BTM LEFT CRNR NORTH       %16.7f",mp->corner_ne[6]);
    (void) printf("\n BTM LEFT CRNR EAST        %16.7f",mp->corner_ne[7]);
    (void) printf("\n TOP LEFT CRNR LAT         %16.7f",mp->corner_ll[0]);
    (void) printf("\n TOP LEFT CRNR LONG        %16.7f",mp->corner_ll[1]);
    (void) printf("\n TOP RGHT CRNR LAT         %16.7f",mp->corner_ll[2]);
    (void) printf("\n TOP RGHT CRNR LONG        %16.7f",mp->corner_ll[3]);
    (void) printf("\n BTM RGHT CRNR LAT         %16.7f",mp->corner_ll[4]);
    (void) printf("\n BTM RGHT CRNR LONG        %16.7f",mp->corner_ll[5]);
    (void) printf("\n BTM LEFT CRNR LAT         %16.7f",mp->corner_ll[6]);
    (void) printf("\n BTL LEFT CRNR LONG        %16.7f",mp->corner_ll[7]);
    (void) printf("\n TOP LEFT CRNR HT          %16.7f",mp->terr_height[0]);
    (void) printf("\n TOP RGHT CRNT HT          %16.7f",mp->terr_height[1]);
    (void) printf("\n BTM RGHT CRNR HT          %16.7f",mp->terr_height[2]);
    (void) printf("\n BTM LEFT CRNR HT          %16.7f",mp->terr_height[3]);
    (void) printf("\n LINE COEFF 1              %20.10E",mp->lp_conv_coef[0]);
    (void) printf("\n LINE COEFF 2              %20.10E",mp->lp_conv_coef[1]);
    (void) printf("\n LINE COEFF 3              %20.10E",mp->lp_conv_coef[2]);
    (void) printf("\n LINE COEFF 4              %20.10E",mp->lp_conv_coef[3]);
    (void) printf("\n LINE COEFF 5              %20.10E",mp->lp_conv_coef[4]);
    (void) printf("\n LINE COEFF 6              %20.10E",mp->lp_conv_coef[5]);
    (void) printf("\n LINE COEFF 7              %20.10E",mp->lp_conv_coef[6]);
    (void) printf("\n LINE COEFF 8              %20.10E",mp->lp_conv_coef[7]);
    (void) printf("\n COEFF MAP 2 LINE POS 1    %20.10E",mp->mp_conv_coef[0]);
    (void) printf("\n COEFF MAP 2 LINE POS 2    %20.10E",mp->mp_conv_coef[1]);
    (void) printf("\n COEFF MAP 2 LINE POS 3    %20.10E",mp->mp_conv_coef[2]);
    (void) printf("\n COEFF MAP 2 LINE POS 4    %20.10E",mp->mp_conv_coef[3]);
    (void) printf("\n COEFF MAP 2 LINE POS 5    %20.10E",mp->mp_conv_coef[4]);
    (void) printf("\n COEFF MAP 2 LINE POS 6    %20.10E",mp->mp_conv_coef[5]);
    (void) printf("\n COEFF MAP 2 LINE POS 7    %20.10E",mp->mp_conv_coef[6]);
    (void) printf("\n COEFF MAP 2 LINE POS 8    %20.10E",mp->mp_conv_coef[7]);
    (void) printf("\n**************** end of Map Projection record " );
    (void) printf("*******************\n");
    }
    if (mp->next != (Map_Proj *) NULL && !mode)
        (void) Write_L_MP_ODL( mp->next, out, mode );
    return( IMS_OK);
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

int Write_L_PP_ODL( Pos_Data* p, int out, int mode)
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    Pos_Vect_Rec *pv;
    desc_rec *d = &(p->desc);
    int nitems;
    int i, off;

    if (p == (Pos_Data*) NULL) return( IMS_ERROR );

    if (out != -1 ) {
       nitems=p->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          (void) Write_L_PP_ODL(p, out, 1);
          p->desc.length=get_rec_length();
             if(  Debug )  (void) printf(
        "\n Calculated record length for Platform Position Data %ld\n",
                p->desc.length);
          nitems=p->desc.length;
          reset_rec_length();
       }

       i=d->rec_seq;
       zero_set(buf,nitems);
       if(  Debug )  (void) printf(
            "\n Write the Platform Position struct to the CEOS file\n");

       if ( (current=GetCurrentSARL()) != NULL )
          (void) Check_Rec_Seq( &i, buf, current, 0);
       else
          cvt2char(&i, buf);
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
       cvt2char(&nitems, (buf+8));
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LP1SUB, LPTYPE, LP2SUB, LP3SUB) ) {
            (void) ims_msg( msgDesc_save, IMS_INFO,
            "ProcessLdr: Header mismatch - expecting PP codes.");
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
       pv = &(p->pos_vect[0]);
       for ( i=0;i<p->ndata;i++) {
           put_F4( pv[i].pos[0],"%22.15f", 22, (buf+off)); off+=22;
           put_F4( pv[i].pos[1],"%22.15f", 22, (buf+off)); off+=22;
           put_F4( pv[i].pos[2],"%22.15f", 22, (buf+off)); off+=22;
           put_F4( pv[i].vel[0],"%22.15f", 22, (buf+off)); off+=22;
           put_F4( pv[i].vel[1],"%22.15f", 22, (buf+off)); off+=22;
           put_F4( pv[i].vel[2],"%22.15f", 22, (buf+off)); off+=22;
       }
       i = write( out, buf, nitems);
    }

    if (Dump ) {

    /* Write the contents of the Platform Postion Data record into
            an ODL tree */

       (void) printf("\n**************** begin of Platform Position " );
       (void) printf("record *******************\n");
       (void) printf("\n RECORD SEQUENCE           %ld", d->rec_seq);
       (void) printf("\n RECORD SUB 1              %d",d->rec_sub1);
       (void) printf("\n RECORD TYPE               %d",d->rec_type);
       (void) printf("\n RECORD SUB 2              %d",d->rec_sub2);
       (void) printf("\n RECORD SUB 3              %d",d->rec_sub3);
       (void) printf("\n RECORD LENGTH             %ld\n",d->length);
       (void) printf("\n ORBITAL ELMTS DESG        %s",p->orbit_ele_desg);
       (void) printf("\n ORBITAL ELEMENT 1         %16.7f",p->orbit_ele[0]);
       (void) printf("\n ORBITAL ELEMENT 2         %16.7f",p->orbit_ele[1]);
       (void) printf("\n ORBITAL ELEMENT 3         %16.7f",p->orbit_ele[2]);
       (void) printf("\n ORBITAL ELEMENT 4         %16.7f",p->orbit_ele[3]);
       (void) printf("\n ORBITAL ELEMENT 5         %16.7f",p->orbit_ele[4]);
       (void) printf("\n ORBITAL ELEMENT 6         %16.7f",p->orbit_ele[5]);
       (void) printf("\n NUM DATA PTS              %d",p->ndata);
       (void) printf("\n YR OF DATA PT             %d",p->year);
       (void) printf("\n MON OF DATA PT            %d",p->month);
       (void) printf("\n DAY OF DATA PT            %d",p->day);
       (void) printf("\n DAY IN THE YR (GMT)       %d",p->gmt_day);
       (void) printf("\n SECS OF DAY (GMT) OF DATA %22.15f",p->gmt_sec);
       (void) printf("\n INTRVL BTWN DATA PTS      %22.15f",p->data_int);
       (void) printf("\n REF COORD SYSTEM          %s",p->ref_coord);
       (void) printf("\n GREENWICH MEAN HR ANGLE   %22.15f",p->hr_angle);
       (void) printf("\n ALONG TRK POS ERROR       %16.7f",p->alt_poserr);
       (void) printf("\n CROSS TRK POS ERROR       %16.7f",p->crt_poserr);
       (void) printf("\n RADIAL POS ERROR          %16.7f",p->rad_poserr);
       (void) printf("\n ALONG TRK VEL ERROR       %16.7f",p->alt_velerr);
       (void) printf("\n CROSS TRK VEL ERROR       %16.7f",p->crt_velerr);
       (void) printf("\n RADIAL VEL ERROR          %16.7f",p->rad_velerr);

       pv = &(p->pos_vect[0]);
       for( i=0;i<p->ndata;i++) {
    (void) printf("\n DATA PT POS VECT #%d FOR X    %22.15f",i+1,pv[i].pos[0]);
    (void) printf("\n DATA PT POS VECT #%d FOR Y    %22.15f",i+1,pv[i].pos[1]);
    (void) printf("\n DATA PT POS VECT #%d FOR Z    %22.15f",i+1,pv[i].pos[2]);
    (void) printf("\n DATA PT VEL VECT #%d FOR X    %22.15f",i+1,pv[i].vel[0]);
    (void) printf("\n DATA PT VEL VECT #%d FOR Y    %22.15f",i+1,pv[i].vel[1]);
    (void) printf("\n DATA PT VEL VECT #%d FOR Z    %22.15f",i+1,pv[i].vel[2]);
       }
       (void) printf("\n************** end of Platform Position record " );
       (void) printf("*****************\n");
    }

    if (p->next != (Pos_Data *) NULL && !mode )
        (void) Write_L_PP_ODL( p->next, out, mode );
    return( IMS_OK );
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


int Write_L_AT_ODL( Att_Data* a, int out, int mode )
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    Att_Vect_Rec *av;
    desc_rec *d = &(a->desc);
    int nitems;
    int i, off;

    if (a == (Att_Data*) NULL) return( IMS_ERROR );

    if ( out != -1  ) {
       nitems=a->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          (void) Write_L_AT_ODL(a, out, 1);
          a->desc.length=get_rec_length();
          if( Debug )  (void) printf(
            "\n Calculated record length for Attitude Data %ld\n",
            a->desc.length);
          nitems=a->desc.length;
          reset_rec_length();
       }

       i=d->rec_seq;
       zero_set(buf,nitems);
       if(  Debug )  (void) printf(
            "\n Write the Attitude Data struct to the CEOS file\n");

       if ( (current=GetCurrentSARL()) != NULL )
          (void) Check_Rec_Seq( &i, buf, current, 0);
       else
          cvt2char(&i, buf);
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
       cvt2char(&nitems, (buf+8));
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LA1SUB, LATYPE, LA2SUB, LA3SUB) ) {
          (void) ims_msg( msgDesc_save, IMS_INFO,
            "ProcessLdr: Header mismatch - expecting  AT codes.");
       }

       put_I4(a->npoint, "%4d", 4, (buf+12));

       av=a->att_vect;
       off=16;
       while (av!=NULL) {
            put_I4(av->gmt_day, "%4d", 4, (buf+off)); off+=4;
            put_I4(av->gmt_sec, "%8d", 8, (buf+off)); off+=8;
            put_I4(av->pitch_flag, "%4d", 4, (buf+off)); off+=4;
            put_I4(av->roll_flag, "%4d", 4, (buf+off)); off+=4;
            put_I4(av->yaw_flag, "%4d", 4, (buf+off)); off+=4;
            put_F4(av->pitch, "%14.6E", 14, (buf+off)); off+=14;
            put_F4(av->roll, "%14.6E", 14, (buf+off)); off+=14;
            put_F4(av->yaw, "%14.6E", 14, (buf+off)); off+=14;
            put_I4(av->pitch_rate_flag, "%4d", 4, (buf+off)); off+=4;
            put_I4(av->roll_rate_flag, "%4d", 4, (buf+off)); off+=4;
            put_I4(av->yaw_rate_flag, "%4d", 4, (buf+off)); off+=4;
            put_F4(av->pitch_rate, "%14.6E", 14, (buf+off)); off+=14;
            put_F4(av->roll_rate, "%14.6E", 14, (buf+off)); off+=14;
            put_F4(av->yaw_rate, "%14.6E", 14, (buf+off)); off+=14;
            av=av->next;
       }
       i = write( out, buf, nitems);
    }

    if (Dump ) {

    /* Write the contents of the Attitude Data record into an ODL tree */

       (void) printf("\n******************* begin of Attitude record " );
       (void) printf("**********************\n");
       (void) printf("\n RECORD SEQUENCE           %ld", d->rec_seq);
       (void) printf("\n RECORD SUB 1              %d",d->rec_sub1);
       (void) printf("\n RECORD TYPE               %d",d->rec_type);
       (void) printf("\n RECORD SUB 2              %d",d->rec_sub2);
       (void) printf("\n RECORD SUB 3              %d",d->rec_sub3);
       (void) printf("\n RECORD LENGTH             %ld\n",d->length);
       (void) printf("\n NUM OF POINTS             %d",a->npoint);
       av = a->att_vect;
       while (av!=NULL) {
          (void) printf("\n POINT NUMBER           %d",i+1);
          (void) printf("\n DAY IN THE YEAR (GMT)  %d",av->gmt_day);
          (void) printf("\n MS OF THE DAY (GMT)    %ld",av->gmt_sec);
          (void) printf("\n PTICH DATA QUAL FLAG   %d",av->pitch_flag);
          (void) printf("\n ROLL DATA QUAL FLAG    %d",av->roll_flag);
          (void) printf("\n YAW DATA QUAL FLAG     %d",av->yaw_flag);
          (void) printf("\n PITCH                  %14.6f",av->pitch);
          (void) printf("\n ROLL                   %14.6f",av->roll);
          (void) printf("\n YAW                    %14.6f",av->yaw);
          (void) printf("\n PITCH RATE QUAL FLAG   %d",av->pitch_rate_flag);
          (void) printf("\n ROLL RATE QUAL FLAG    %d",av->roll_rate_flag);
          (void) printf("\n YAW RATE QUAL FLAG     %d",av->yaw_rate_flag);
          (void) printf("\n PITCH RATE             %14.6f",av->pitch_rate);
          (void) printf("\n ROLL RATE              %14.6f",av->roll_rate);
          (void) printf("\n YAW RATE               %14.6f",av->yaw_rate);
          (void) printf("\n END OF POINT NUMBER    %d",i+1);
          i++;
          av=av->next;
       }
       (void) printf("\n******************** end of Attitude record " );
       (void) printf("*******************\n");
    }

    if (a->next != (Att_Data *) NULL && !mode)
        (void) Write_L_AT_ODL( a->next, out, mode);
    return( IMS_OK );
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

int Write_L_RD_ODL( Radi_Data* r, int out, int mode )
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    desc_rec *d = &(r->desc);
    int nitems;
    int i, off;

    if (r == (Radi_Data*) NULL) return( IMS_ERROR);

    if ( out != -1 ) {
       nitems=r->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          (void) Write_L_RD_ODL(r, out, 1);
          r->desc.length=get_rec_length();
          if(  Debug )  (void) printf(
            "\n Calculated record length for Radiometric Data %ld\n",
            r->desc.length);
          nitems=r->desc.length;
          reset_rec_length();
       }

       i=d->rec_seq;
       zero_set(buf,nitems);
       if(  Debug )  (void) printf(
            "\n Write the Radiometric Data struct to the CEOS file\n");

       if ( (current=GetCurrentSARL()) != NULL )
          (void) Check_Rec_Seq( &i, buf, current, 0);
       else
          cvt2char(&i, buf);
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
       cvt2char(&nitems, (buf+8));
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LR1SUB, LRTYPE, LR2SUB, LR3SUB) ) {
          (void) ims_msg( msgDesc_save, IMS_INFO,
            "ProcessLdr:  Header mismatch - expecting  RD codes.");
       }

       put_I4(r->seq_num, "%4d", 4, (buf+12));
       put_I4(r->n_data, "%4d", 4, (buf+16));
       put_I4(r->field_size, "%8d", 8, (buf+20));
       put_chars((char*) (buf+28), r->chan_ind, 4);
       put_blanks((char*) (buf+32), 4);
       put_chars((char*) (buf+36), r->table_desig, 24);
       put_I4(r->n_samp, "%8d", 8, (buf+60));
       put_chars((char*) (buf+68), r->samp_type, 16);
       put_F4(r->noise_fact, "%16.7E", 16, (buf+84));
       put_F4(r->linear_conv_fact, "%16.7E", 16, (buf+100));
       put_F4(r->offset_conv_fact, "%16.7E", 16, (buf+116));
       put_blanks((char*) (buf+132), 4);

       off = 136;
       for (i=0; i<r->n_samp; i++, off+=16) {
           put_F4(r->lookup_tab[i], "%16.7f", 16, (buf+off));
       }
       i = write( out, buf, nitems);
    }

    if (Dump ) {

    /* Write the contents of the Radiometric Data record into an ODL tree */

       (void) printf("\n************* begin of Radiometric Data record " );
       (void) printf("****************\n");
       (void) printf("\n RECORD SEQUENCE           %ld", d->rec_seq);
       (void) printf("\n RECORD SUB 1              %d",d->rec_sub1);
       (void) printf("\n RECORD TYPE               %d",d->rec_type);
       (void) printf("\n RECORD SUB 2              %d",d->rec_sub2);
       (void) printf("\n RECORD SUB 3              %d",d->rec_sub3);
       (void) printf("\n RECORD LENGTH             %ld\n",d->length);
       (void) printf("\n RADIO DATA SEQ NUM        %d",r->seq_num);
       (void) printf("\n NUM OF RADIO DATA FLDS    %d",r->n_data);
       (void) printf("\n RADIO DATA SET SIZE       %ld",r->field_size);
       (void) printf("\n SAR CHANNEL ID            %s",r->chan_ind);
       (void) printf("\n LUT DESIGNATOR            %s",r->table_desig);
       (void) printf("\n NUM SAMPLES IN LUT        %ld",r->n_samp);
       (void) printf("\n SAMPLE TYPE ID            %s",r->samp_type);
       (void) printf("\n NOISE SCALE FCTR          %16.7f",r->noise_fact);
       (void) printf("\n LINEAR CONV FCTR          %16.7f",r->linear_conv_fact);
       (void) printf("\n OFFSET CONV FCTR          %16.7f",r->offset_conv_fact);

       for (i=0; i<r->n_samp; i++) {
           if (i<99) (void) printf("\n LUT SAMPLE #%d      = %16.7f",i+1,
                r->lookup_tab[i]);
           else
              (void) printf("\n LUT SAMPLE #%d        = %16.7f",i+1,
                r->lookup_tab[i]);
       }
       (void) printf("\n***************** end of Radiometric Data record " );
       (void) printf("********************\n");
    }

    if (r->next != (Radi_Data *) NULL && !mode)
        (void) Write_L_RD_ODL( r->next, out, mode);
    return( IMS_OK );
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


int Write_L_RC_ODL( Radi_Comp* r, int out, int mode )
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    Rad_Comp_Set *rc;
    Radio_Comp_Tbl *rt;
    desc_rec *d = &(r->desc);
    int nitems;
    int i=0, off;

    if (r == (Radi_Comp*) NULL) return(FALSE);

    if ( out != -1 ) {
       nitems=r->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          (void) Write_L_RC_ODL(r, out, 1);
          r->desc.length=get_rec_length();
          if( Debug ) (void) printf(
            "\n Calculated record length for Radiometric Compensation %ld\n",
            r->desc.length);
          nitems=r->desc.length;
          reset_rec_length();
       }

       i=d->rec_seq;
       zero_set(buf,nitems);
       if( Debug ) (void) printf(
        "\n Write the Radiometric Compensation struct to the CEOS file\n");

       if ( (current=GetCurrentSARL()) != NULL )
          (void) Check_Rec_Seq( &i, buf, current, 0);
       else
          cvt2char(&i, buf);
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
       cvt2char(&nitems, (buf+8));
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LC1SUB, LCTYPE, LC2SUB, LC3SUB) ) {
          (void) ims_msg( msgDesc_save, IMS_INFO,
            "ProcessLdr:  Header mismatch - expecting  RC codes.");
       }

       put_I4(r->seq_num, "%4d", 4, (buf+12));
       put_I4(r->chan_ind, "%4d", 4, (buf+16));
       put_I4(r->n_dset, "%8d", 8, (buf+20));
       put_I4(r->dset_size, "%8d", 8, (buf+28));

       rc = r->set;
       off=36;
       while(rc!=NULL) {
           put_chars((char*) (buf+off), rc->data_type, 8); off+=8;
           put_chars((char*) (buf+off), rc->data_descr, 32); off+=32;
           put_I4(rc->req_records, "%4d", 4, (buf+off)); off+=4;
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
       i = write( out, buf, nitems);
    }

    if (Dump ) {

    /* Write the contents of the Radiometric Compensation record
            into an ODL tree */
       (void) printf("\n************* begin of Radiometric Compensation " );
       (void) printf("record ****************\n");
       (void) printf("\n RECORD SEQUENCE               %ld", d->rec_seq);
       (void) printf("\n RECORD SUB 1                  %d",d->rec_sub1);
       (void) printf("\n RECORD TYPE                   %d",d->rec_type);
       (void) printf("\n RECORD SUB 2                  %d",d->rec_sub2);
       (void) printf("\n RECORD SUB 3                  %d",d->rec_sub3);
       (void) printf("\n RECORD LENGTH                 %ld\n",d->length);
       (void) printf("\n RADIO COMP SEQ NUM            %d",r->seq_num);
       (void) printf("\n SAR CHANNEL ID                %d",r->chan_ind);
       (void) printf("\n NUM RADIO COMP SETS           %ld",r->n_dset);
       (void) printf("\n COMP DATA SET SIZE            %ld",r->dset_size);

       rc = r->set;
       while (rc!=NULL) {
      (void) printf("\n COMP DATA TYPE DESIG          %s",rc->data_type);
      (void) printf("\n COMP DATA DESIG               %s",rc->data_descr);
      (void) printf("\n NUM COMP RECS FOR COMP TBL    %d",rc->req_records);
      (void) printf("\n SEQ NUM IN FULL COMP TBL      %d",rc->table_seq_num);
      (void) printf("\n TTL NUM PAIRS IN FULL COMP    %ld",rc->num_pairs);
      (void) printf("\n PXL NUM TO 1ST CORCT VAL      %ld",rc->first_pixel);
      (void) printf("\n PXL NUM TO LAST CORCT VAL     %ld",rc->last_pixel);
      (void) printf("\n PIXEL GROUP SIZE              %ld",rc->pixel_size);
      (void) printf("\n MIN TBL OFFSET VAL            %f",rc->min_samp_index);
      (void) printf("\n MIN TBL GAIN VAL              %f",rc->min_comp_value);
      (void) printf("\n MAX TBL OFFSET VAL            %f",rc->max_samp_index);
      (void) printf("\n MAX TBL GAIN VAL              %f",rc->max_comp_value);
      (void) printf("\n NUM COMP TBL ENTRIES          %ld",rc->n_table_entries);

           rt = rc->tbl;
           while (rt!=NULL) {
               i++;
            (void) printf("\n SAMPLE OFFSET #%d    %16.7f",i,rt->sample_offset);
            (void) printf("\n SAMPLE GAIN   #%d    %16.7f",i,rt->sample_gain);
               rt=rt->next;
           }
           rc = rc->next;
       }
       (void) printf("\n*********** end of Radiometric Compensation record " );
       (void) printf("*************\n");
    }

    if (r->next != (Radi_Comp *) NULL && !mode)
        (void) Write_L_RC_ODL( r->next, out, mode );
    return( IMS_OK );
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

int Write_L_DQS_ODL( Qual_Sum* q, int out, int mode )
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    desc_rec *d = &(q->desc);
    int nitems;
    int i, off;

    if (q == (Qual_Sum*) NULL) return( IMS_ERROR );

    if ( out != -1 ) {
       nitems=q->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          (void) Write_L_DQS_ODL(q, out, 1);
          q->desc.length=get_rec_length();
          if( Debug )  (void) printf(
            "\n Calculated record length for Datas Quality Summary %ld\n",
            q->desc.length);
          nitems=q->desc.length;
          reset_rec_length();
       }

       i=d->rec_seq;
       zero_set(buf,nitems);
       if( Debug )  (void) printf(
        "\n Write the Data Quality Summary struct to the CEOS file\n");

       if ( (current=GetCurrentSARL()) != NULL )
          (void) Check_Rec_Seq( &i, buf, current, 0);
       else
          cvt2char(&i, buf);
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
       cvt2char(&nitems, (buf+8));
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LQ1SUB, LQTYPE, LQ2SUB, LQ3SUB) ) {
          (void) ims_msg( msgDesc_save, IMS_INFO,
            "ProcessLdr:  Header mismatch - expecting  DQS codes.");
       }
       put_I4(q->seq_num, "%4d", 4, (buf+12));
       put_chars((char*) (buf+16), q->sar_chn, 4);
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
       put_F4(q->rad_unc_db, "%16.7f", 16, (buf+190));
       put_F4(q->rad_unc_deg, "%16.7f", 16, (buf+206));

       off=222;
       for (i=0; (i<16); i++) {
           if (i<q->nchn) {
              put_F4(q->rad_unc[i].db, "%16.7f", 16, (buf+off)); off+=16;
              put_F4(q->rad_unc[i].deg, "%16.7f", 16, (buf+off)); off+=16;
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
       for (i=0; (i<q->nchn && i<16); i++) {
           put_F4(q->misreg[i].alt_m, "%16.7f", 16, (buf+off)); off+=16;
           put_F4(q->misreg[i].crt_m, "%16.7f", 16, (buf+off)); off+=16;
       }
       i = write( out, buf, nitems);
    }

    if (Dump ) {

    /* Write the contents of the Data Quality Summary record into an
        ODL tree */

       (void) printf("\n************ begin of Data Quality Summary record " );
       (void) printf("***************\n");
       (void) printf("\n RECORD SEQUENCE           %ld", d->rec_seq);
       (void) printf("\n RECORD SUB 1              %d",d->rec_sub1);
       (void) printf("\n RECORD TYPE               %d",d->rec_type);
       (void) printf("\n RECORD SUB 2              %d",d->rec_sub2);
       (void) printf("\n RECORD SUB 3              %d",d->rec_sub3);
       (void) printf("\n RECORD LENGTH             %ld\n",d->length);
       (void) printf("\n DATA QUAL SUM SEQ NUM     %d",q->seq_num);
       (void) printf("\n SARA CHANNEL ID           %s",q->sar_chn);
       (void) printf("\n DATE OF LAST CALIB UPDATE %s",q->cali_date);
       (void) printf("\n NUM OF CHANNELS           %d",q->nchn);
       (void) printf("\n INTEGRATED SIDE LOB RATIO %16.7f",q->islr);
       (void) printf("\n PEAK SIDE LOBE RATIO      %16.7f",q->pslr);
       (void) printf("\n AZI AMBIGUITY             %16.7f",q->azi_ambig);
       (void) printf("\n RNG AMBIGUITY             %16.7f",q->rng_ambig);
       (void) printf("\n ESTIMATE OF SNR           %16.7f",q->snr);
       (void) printf("\n ACTUAL BIT RATE ERROR     %e",q->ber);
       (void) printf("\n SLANT RNG RESOLUTION      %16.7f",q->rng_res);
       (void) printf("\n AZIMUTH RESOLUTION        %16.7f",q->azi_res);
       (void) printf("\n RADIOMETRIC RESOLUTION    %16.7f",q->rad_res);
       (void) printf("\n INSTAN DYNAMIC RANGE      %16.7f",q->dyn_rng);
       (void) printf("\n NOM RAD UNCERTAIN, DB     %16.7f",q->rad_unc_db);
       (void) printf("\n NOM RAD UNCERTAIN, DEG    %16.7f",q->rad_unc_deg);

       (void) printf("\n\n RELATIVE RADIOMETRIC DATA QUALITY");
       for (i=0; i<q->nchn; i++) {
           (void) printf("\n REL RAD UNCERTAIN #%d, DB     %16.7f",i+1,
            q->rad_unc[i].db);
           (void) printf("\n REL RAD UNCERTAIN #%d, DEG    %16.7f",i+1,
            q->rad_unc[i].deg);
       }

       (void) printf("\n\n ABSOLUTE GEOMETRIC DATA QUALITY");
       (void) printf("\n LOC ERROR ALONG TRK       %16.7f",q->alt_locerr);
       (void) printf("\n LOC ERROR CROSS TRK       %16.7f",q->crt_locerr);
       (void) printf("\n ALONG TRK SCALE ERROR     %16.7f",q->alt_scale);
       (void) printf("\n CROSS TRK SCALE ERROR     %16.7f",q->crt_scale);
       (void) printf("\n DISTORTION SKEW           %16.7f",q->dis_skew);
       (void) printf("\n SCENE ORIENT ERROR        %16.7f",q->ori_err);

       (void) printf("\n\n RELATIVE GEOMETRIC DATA QUALITY");
       for (i=0; i<q->nchn; i++) {
(void) printf("\n ALONG TRK MISREG ERROR #%d    %16.7f",i,q->misreg[i].alt_m);
(void) printf("\n CROSS TRK MISREG ERROR #%d    %16.7f",i,q->misreg[i].crt_m);
       }
       (void) printf(
        "\n************ end of Data Quality Summary record ***************\n");
    }

    if (q->next != (Qual_Sum *) NULL && !mode)
        (void) Write_L_DQS_ODL( q->next, out, mode );
    return( IMS_OK );
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


int Write_L_DH_ODL( Data_Hist* h, int out, int mode )
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    Hist_Data_Set *ht;
    int i, j=1;
    desc_rec *d = &(h->desc);
    int nitems;
    int off;

    if (h == (Data_Hist*) NULL) return( IMS_ERROR );

    if ( out != -1 ) {
       nitems=h->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          (void) Write_L_DH_ODL(h, out, 1);
          h->desc.length=get_rec_length();
          if(  Debug )  (void) printf(
            "\n Calculated record length for Data Histrograms %ld\n",
            h->desc.length);
          nitems=h->desc.length;
          reset_rec_length();
       }

       i=d->rec_seq;
       zero_set(buf,nitems);
       if(  Debug )  (void) printf(
            "\n Write the Data Histogram struct to the CEOS file\n");

       if ( (current=GetCurrentSARL()) != NULL )
          (void) Check_Rec_Seq( &i, buf, current, 0);
       else
          cvt2char(&i, buf);
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
       cvt2char(&nitems, (buf+8));
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LH1SUB, LHTYPE, LH2SUB, LH3SUB) ) {
          (void) ims_msg( msgDesc_save, IMS_INFO,
            "ProcessLdr:  Header mismatch - expecting  DH codes.");
       }

       put_I4(h->seq_num, "%4d", 4, (buf+12));
       put_I4(h->sar_chn, "%4d", 4, (buf+16));
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
               put_I4(ht->data_values[i], "%8d", 8, (buf+off)); off+=8;
            }
            ht=ht->next;
        }
        i = write( out, buf, nitems);
    }

    if (Dump ) {

    /* Write the contents of the Data Histogram record into an ODL tree */

       (void) printf(
        "\n************* begin of Data Histogram record ****************\n");
       (void) printf("\n RECORD SEQUENCE           %ld", d->rec_seq);
       (void) printf("\n RECORD SUB 1              %d",d->rec_sub1);
       (void) printf("\n RECORD TYPE               %d",d->rec_type);
       (void) printf("\n RECORD SUB 2              %d",d->rec_sub2);
       (void) printf("\n RECORD SUB 3              %d",d->rec_sub3);
       (void) printf("\n RECORD LENGTH             %ld\n",d->length);
       (void) printf("\n DATA HISTOGRAM SEQ NUM    %d",h->seq_num);
       (void) printf("\n SAR CHANNEL ID            %d",h->sar_chn);
       (void) printf("\n NUM HISTOGRAM DATA SETS   %ld",h->ntab);
       (void) printf("\n TABLE DATA SET SIZE       %ld",h->ltab);

       ht=h->data_set;
       while (ht!=NULL) {
    (void) printf("\n\n DATA SET NUM                    %d",j++);
    (void) printf("\n HISTOGRAM DESCRIPTOR              %s",ht->hist_desc);
    (void) printf("\n RECS NEEDED FOR FULL TABLE        %d",ht->nrec);
    (void) printf("\n TBL REC SEQ NUM                   %d",ht->tab_seq);
    (void) printf("\n NUM OF TBL BINS                   %ld",ht->nbin);
    (void) printf("\n NUM SAMPLES IN LINE DIR           %ld",ht->ns_lin);
    (void) printf("\n NUM SAMPLES ACROSS LINE DIR       %ld",ht->ns_pix);
    (void) printf("\n SAMPLE GRP SIZE IN LINE DIR       %ld",ht->ngrp_lin);
    (void) printf("\n SAMPLE GRP SIZE ACROSS LINE DIR   %ld",ht->ngrp_pix);
    (void) printf("\n NUM USED PER GRP IN LINE DIR      %ld",ht->nsamp_lin);
    (void) printf("\n NUM USED PER GRP ACROSS LINE DIR  %ld",ht->nsamp_pix);
    (void) printf("\n MIN VAL FOR IST TBL BIN           %16.7f",ht->min_smp);
    (void) printf("\n MAX VAL FOR LAST TBL BIN          %16.7f",ht->max_smp);
    (void) printf("\n MEAN SAMPLE VALUE                 %16.7f",ht->mean_smp);
    (void) printf("\n STD DEV OF SAMPLE VALUE           %16.7f",ht->std_smp);
    (void) printf("\n SAMPLE VALUE INCREMENT            %16.7f",ht->smp_inc);
    (void) printf("\n MIN TBL VALUE                     %16.7f",ht->min_hist);
    (void) printf("\n MAX TBL VALUE                     %16.7f",ht->max_hist);
    (void) printf("\n MEAN TBL VALUE                    %16.7f",ht->mean_hist);
    (void) printf("\n STD DEV OF HIST TBL               %16.7f",ht->std_hist);

           (void) printf("\n\n HISTOGRAM TBL SIZE              %ld",ht->nhist);
           for (i=0; i<ht->nhist; i++) {
          (void) printf("\n TABLE VALUE #%d        %ld",i+1,ht->data_values[i]);
           }
           ht=ht->next;
       }
       (void) printf(
            "\n***************** end of Data Histogram record " );
       (void) printf(
            "********************\n");
    }

    if (h->next != (Data_Hist *) NULL && !mode)
        (void) Write_L_DH_ODL( h->next, out, mode );
    return( IMS_OK );
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

int Write_L_RS_ODL( Rng_Spec* r, int out, int mode )
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    desc_rec *d = &(r->desc);
    int nitems;
    int i, off;

    if (r == (Rng_Spec*) NULL) return( IMS_ERROR );

    if ( out != -1 ) {
       nitems=r->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          (void) Write_L_RS_ODL(r, out, 1);
          r->desc.length=get_rec_length();
          if( Debug )  (void) printf(
            "\n Calculated record length for Range Spectra %ld\n",
            r->desc.length);
          nitems=r->desc.length;
          reset_rec_length();
       }

       i=d->rec_seq;
       zero_set(buf,nitems);
       if(  Debug )  (void) printf(
            "\n Write the Range Spectra struct to the CEOS file\n");

       if ( (current=GetCurrentSARL()) != NULL )
          (void) Check_Rec_Seq( &i, buf, current, 0);
       else
          cvt2char(&i, buf);
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
       cvt2char(&nitems, (buf+8));
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LZ1SUB, LZTYPE, LZ2SUB, LZ3SUB) ) {
          (void) ims_msg( msgDesc_save, IMS_INFO,
            "ProcessLdr:  Header mismatch - expecting  RS codes.");
       }

       put_I4(r->seq_num, "%4d", 4, (buf+12));
       put_I4(r->sar_chn, "%4d", 4, (buf+16));
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
       put_blanks( (char*) (buf+132), 32);
       put_I4(r->n_bins, "%8d", 8, (buf+164));

       off=172;
       for (i=0; i<r->n_bins; i++, off +=16)
           put_F4(r->data_values[i], "%16.7f", 16, (buf+off));
       i = write( out, buf, nitems);
    }


    if (Dump ) {

    /* Write the contents of the Range Spectra record into an ODL tree */

(void) printf(
 "\n************* begin of Range Spectra record ***************\n");
(void) printf("\n RECORD SEQUENCE                       %ld", d->rec_seq);
(void) printf("\n RECORD SUB 1                          %d",d->rec_sub1);
(void) printf("\n RECORD TYPE                           %d",d->rec_type);
(void) printf("\n RECORD SUB 2                          %d",d->rec_sub2);
(void) printf("\n RECORD SUB 3                          %d",d->rec_sub3);
(void) printf("\n RECORD LENGTH                         %ld\n",d->length);
(void) printf("\n RANGE SPECTRA SEQ NUM                 %d",r->seq_num);
(void) printf("\n SAR CHANNEL ID                        %d",r->sar_chn);
(void) printf("\n TBL DATA SET NUM IN RECORD            %ld",r->n_dset);
(void) printf("\n TBL DATA SET SIZE                     %ld",r->dset_size);
(void) printf("\n DATA SETS REQ FOR FULL TABLE          %d",r->req_recs);
(void) printf("\n FULL TBL SEQ NUM OF TBL IN REC        %d",r->table_no);
(void) printf("\n TTL SAMP NUM IN RNG DIR               %ld",r->n_pixels);
(void) printf("\n NUM SAMP OFFSET FROM 1ST SAMP RNG LIN %ld",r->pixel_offset);
(void) printf("\n NUM RNG LINES INTEGRATED              %ld",r->n_lines);
(void) printf("\n CTR FREQ OF FIRST SPEC BIN            %16.7f",r->first_freq);
(void) printf("\n CTR FREQ OF LAST SPEC BIN             %16.7f",r->last_freq);
(void) printf("\n MIN SPECTRAL PWR                      %16.7f",r->min_power);
(void) printf("\n MAX SPECTRAL PWR                      %16.7f",r->max_power);
(void) printf("\n NUM OF FREQ BINS IN TABLE             %ld",r->n_bins);

       for (i=0; i<r->n_bins; i++)
     (void) printf("\n DATA VALUES #%d        %16.7f",i+1,r->data_values[i]);
(void) printf("\n************* end of Range Spectra record ***************\n");
    }

    if (r->next != (Rng_Spec *) NULL && !mode)
        (void) Write_L_RS_ODL( r->next, out, mode );
    return( IMS_OK );

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

int Write_L_DE_ODL( Digital_Elev* e, int out, int mode )
{
    SARL_ptr *current;
    unsigned char* buf = work;
    unsigned char* tb;
    desc_rec *d = &(e->desc);
    Dem_Desc *set;
    Corner_Pts *pts;
    int nitems;
    int i, off;

    if (e == (Digital_Elev*) NULL) return( IMS_ERROR );

    if ( out != -1 ) {
       nitems=e->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          (void) Write_L_DE_ODL(e, out, 1);
          e->desc.length=get_rec_length();
         if(  Debug )  (void) printf(
            "\n Calculated record length for Digital Elevation %ld\n",
            e->desc.length);
          nitems=e->desc.length;
          reset_rec_length();
       }

       i=d->rec_seq;
       zero_set(buf,nitems);
       if(  Debug )  (void) printf(
            "\n Write the Digital Elevation struct to the CEOS file\n");

       if ( (current=GetCurrentSARL()) != NULL )
          (void) Check_Rec_Seq( &i, buf, current, 0);
       else
          cvt2char(&i, buf);
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
       cvt2char(&nitems, (buf+8));
       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LE1SUB, LETYPE, LE2SUB, LE3SUB) ) {
          (void) ims_msg( msgDesc_save, IMS_INFO,
            "ProcessLdr:  Header mismatch - expecting  DE codes.");
       }
       put_I4(e->seq_num, "%4d", 4, (buf+12));
       put_I4(e->ttl_num_sets, "%8d", 8, (buf+16));
       put_I4(e->DEM_seq_num, "%4d", 4, (buf+24));
       put_chars((char *) (buf+28), e->source, 32);
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
           put_blanks( (char *) (buf+off), 8);  off+=8;

           pts = set->pts;
           while (pts!=NULL) {
               put_F4(pts->latitude, "%16.7f", 16, (buf+off)); off+=16;
               put_F4(pts->longitude,"%16.7f", 16, (buf+off)); off+=16;
               pts=pts->next;
           }
           set=set->next;
       }
       i = write( out, buf, nitems);
    }

    if (Dump ) {

       /* Write the contents of the Digital Elevation record into an
            ODL tree */

       (void) printf("\n**************** begin of Digital Elevation record " );
       (void) printf("***************\n");
       (void) printf("\n RECORD SEQUENCE               %ld", d->rec_seq);
       (void) printf("\n RECORD SUB 1                  %c",d->rec_sub1);
       (void) printf("\n RECORD TYPE                   %c",d->rec_type);
       (void) printf("\n RECORD SUB 2                  %c",d->rec_sub2);
       (void) printf("\n RECORD SUB 3                  %c",d->rec_sub3);
       (void) printf("\n RECORD LENGTH                 %ld\n",d->length);
       (void) printf("\n DIGITAL ELEVATION SEQ NUM     %d",e->seq_num);
       (void) printf("\n TTL NUM OF DEM DESC. DATASETS %ld",e->ttl_num_sets);
       (void) printf("\n SEQ NUM THIS DEM              %d",e->DEM_seq_num);
       (void) printf("\n SOURCE OF DEM                 %s",e->source);
       (void) printf("\n HT DATUM REF NAME             %s",e->HT_ref_name);
       (void) printf("\n GENERATION METHOD             %s",e->gen_method);
       (void) printf("\n RASTER SPACING UNIT           %s",e->raster_unit);
     (void) printf("\n PRESENTATION PROJECTION       %s",e->presentation_proj);
       (void) printf("\n RASTER SPACING N/S            %16.7f",e->NS_raster);
       (void) printf("\n RASTER SPACING E/W            %16.7f",e->EW_raster);
       (void) printf("\n REAMPLING METHOD              %s",e->resample);
       (void) printf("\n HEIGHT ERROR                  %16.7f",e->height_err);
       (void) printf("\n LOCATION ERROR N/S            %16.7f",e->NS_loc_err);
       (void) printf("\n LOCATION ERROR E/W            %16.7f",e->EW_loc_err);
       (void) printf("\n MAX. HEIGHT                   %16.7f",e->max_height);
       (void) printf("\n MIN. HEIGHT                   %16.7f",e->min_height);
       (void) printf("\n MEAN HEIGHT                   %16.7f",e->MEAN_height);
       (void) printf("\n STD HEIGHT                    %16.7f",e->STD_height);
       (void) printf("\n NUM OF POLYGONS               %d",e->num_polys);

       set= e->set;
       while (set!=NULL) {
           (void) printf("\n POLYGON SEQ NUM      %d",set->poly_seq_num);
           (void) printf("\n NUM OF CORNER PTS    %d",set->num_crnr_pts);

           pts = set->pts ;
           i=1;
           while (pts!=NULL) {
               (void) printf("\n LATITUDE[%d]        %16.7f",i,pts->latitude);
               (void) printf("\n LONGITUDE[%d]       %16.7f",i,pts->longitude);
               pts=pts->next;
               i++;
           }
           set=set->next;
       }
       (void) printf(
        "\n***************** end of  Digital Elevation record " );
       (void) printf(
        "****************\n");
    }

    if (e->next != (Digital_Elev *) NULL && !mode)
        (void) Write_L_DE_ODL( e->next, out, mode );
    return( IMS_OK );
}

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
       if (d->n_faci_data > 0 && num > 0 && num <= d->n_faci_data) {
          r = t->facility;
          while (i<num) { r=r->next; i++; }
       }
       return( r );
    }
    else
       return( (Fac_Related*) NULL);
}

int Write_L_FR_ODL( Fac_Related* r, int out, int mode )
{
    SARL_ptr *current;
    unsigned char* buf = work;
    desc_rec *d = &(r->desc);
    int nitems;
    int i;

    void  print_nice();   /*  print long char array in a
        nice mannor  */

    if (r == (Fac_Related*) NULL) return( IMS_ERROR );

    if ( out != -1 ) {
       nitems=r->desc.length;
       /* user requests calculation of record length */
       if (nitems==0 && !get_recalc_status()) {
          recalc_length();
          (void) Write_L_FR_ODL(r, out, 1);
          r->desc.length=get_rec_length();
          if(  Debug  )  (void) printf(
            "\n Calculated record length for Facility Related %ld\n",
            r->desc.length);
          nitems=r->desc.length;
          reset_rec_length();
       }

       i=d->rec_seq;
       zero_set(buf,nitems);
       if( Debug )  (void) printf(
            "\n Write the Facility Related struct to the CEOS file\n");

       if ( (current=GetCurrentSARL()) != NULL )
          (void) Check_Rec_Seq( &i, buf, current, 0);
       else
          cvt2char(&i, buf);
       put_byte((buf+4), d->rec_sub1);
       put_byte((buf+5), d->rec_type);
       put_byte((buf+6), d->rec_sub2);
       put_byte((buf+7), d->rec_sub3);
       cvt2char(&nitems, (buf+8));
/*       tb=&buf[4];
       if ( !MATCH_RECORD(tb, LF1SUB, LFTYPE, LF2SUB, LF3SUB) ) {
             (void) ims_msg( msgDesc_save, IMS_INFO,
         "ProcessLdr:  Header mismatch - expecting  FR codes.");
       }
*/
       if ( d->rec_type != LFTYPE  &&
            d->rec_type != LFTYPE2 ) {
          put_chars( (char *) (buf+12), r->bogus, nitems-12);
       }
       else {
          put_I4(r->seq_num, "%4d", 4, (buf+12));
          put_blanks((char *) (buf+16), 4);
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
          put_F4(r->near_sta_LAT, "%17.7f", 17, (buf+156));
          put_F4(r->near_sta_LON, "%17.7f", 17, (buf+173));
          put_F4(r->near_end_LAT, "%17.7f", 17, (buf+190));
          put_F4(r->near_end_LON, "%17.7f", 17, (buf+207));
          put_F4(r->far_sta_LAT, "%17.7f", 17, (buf+224));
          put_F4(r->far_sta_LON, "%17.7f", 17, (buf+241));
          put_F4(r->far_end_LAT, "%17.7f", 17, (buf+258));
          put_F4(r->far_end_LON, "%17.7f", 17, (buf+275));
          put_F4(r->actual_azimuth, "%17.7f", 17, (buf+292));
          put_F4(r->actual_range, "%17.7f", 17, (buf+309));
          put_I4(r->actual_pixels, "%9d", 9, (buf+326));
          put_I4(r->actual_lines, "%9d", 9, (buf+335));
          put_I4(r->total_pixels, "%9d", 9, (buf+344));
          put_I4(r->total_lines, "%9d", 9, (buf+353));
          put_chars((char*) (buf+362), r->media_ID, 7);
          put_I4(r->start_block, "%17d", 17, (buf+369));
          put_I4(r->end_block, "%17d", 17, (buf+386));
          put_chars((char*) (buf+403), r->platform_name, 17);
          put_chars((char*) (buf+420), r->sensor_mode, 33);
          put_F4(r->PRF, "%17.7f", 17, (buf+453));
          put_F4(r->ant_look_angle, "%17.7f", 17, (buf+470));
          put_F4(r->data_rate, "%17.7f", 17, (buf+487));
          put_F4(r->data_win_pos, "%17.7f", 17, (buf+504));
          put_F4(r->range_gate_del, "%17.7f", 17, (buf+521));
          put_F4(r->track_angle, "%17.7f", 17, (buf+538));
          put_chars((char*) (buf+555), r->ASC_DESC, 1);
          put_F4(r->S_C_altitude, "%17.7f", 17, (buf+556));
          put_F4(r->X_pos, "%23.10E", 23, (buf+573));
          put_F4(r->Y_pos, "%23.10E", 23, (buf+596));
          put_F4(r->Z_pos, "%23.10E", 23, (buf+619));
          put_F4(r->X_vel, "%23.10E", 23, (buf+642));
          put_F4(r->Y_vel, "%23.10E", 23, (buf+665));
          put_F4(r->Z_vel, "%23.10E", 23, (buf+688));
          put_F4(r->roll, "%15.7f", 15, (buf+711));
          put_F4(r->yaw, "%15.7f", 25, (buf+726));
          put_F4(r->pitch, "%15.7f", 15, (buf+741));
          put_I4(r->roll_flag, "%5d", 5, (buf+756));
          put_I4(r->yaw_flag, "%5d", 5, (buf+761));
          put_I4(r->pitch_flag, "%5d", 5, (buf+766));
          put_F4(r->roll_rate, "%15.7f", 15, (buf+771));
          put_F4(r->yaw_rate, "%15.7f", 15, (buf+786));
          put_F4(r->pitch_rate, "%15.7f", 15, (buf+801));
          put_I4(r->roll_rate_flag, "%5d", 5, (buf+816));
          put_I4(r->yaw_rate_flag, "%5d", 5, (buf+821));
          put_I4(r->pitch_rate_flag, "%5d", 5, (buf+826));
          put_F4(r->nadir_radius, "%17.7f", 17, (buf+831));
          put_F4(r->image_radius, "%17.7f", 17, (buf+848));
          put_F4(r->incidence_angle, "%17.7f", 17, (buf+865));
          put_chars((char*) (buf+882), r->proc_version, 8);
          put_chars((char*) (buf+890), r->proc_type, 3);
          put_I4(r->type_ephemeris, "%4d", 2, (buf+893));
          put_F4(r->looks_azimuth, "%17.7f", 17, (buf+895));
          put_F4(r->looks_range, "%17.7f", 17, (buf+912));
          put_F4(r->WPH_azimuth, "%17.7f", 17, (buf+929));
          put_F4(r->WPH_range, "%17.7f", 17, (buf+946));
          put_chars((char*) (buf+963), r->look_energy_eq, 4);
          put_F4(r->induced_azimuth, "%17.7f", 17, (buf+967));
          put_F4(r->induced_range, "%17.7f", 17, (buf+984));
          put_F4(r->gain, "%17.7f", 17, (buf+1001));
          put_F4(r->swath_velocity, "%17.7f", 17, (buf+1018));
          put_F4(r->squint_angle, "%17.7f", 17, (buf+1035));
          put_F4(r->avg_ter_height, "%17.7f", 17, (buf+1052));
          put_I4(r->proc_gain, "%4d", 4, (buf+1069));
          put_chars((char*) (buf+1073), r->deskew_flag, 4);
          put_chars((char*) (buf+1077), r->gnd_slant_flag, 7);
          put_F4(r->sl_rng_1st_pix, "%17.7f", 17, (buf+1084));
          put_F4(r->sl_rng_last_pix, "%17.7f", 17, (buf+1101));
          put_I4(r->start_sample, "%9d", 9, (buf+1118));
          put_chars((char*) (buf+1127), r->clutterlock_flg, 4);
          put_F4(r->dop_frq_const, "%17.7f", 17, (buf+1131));
          put_F4(r->dop_frq_slope, "%17.7f", 17, (buf+1148));
          put_F4(r->dop_frq_quad, "%17.7f", 17, (buf+1165));
          put_chars((char*) (buf+1182), r->autofocus_flag, 4);
          put_F4(r->dop_frq_r_cnst, "%17.7f", 17, (buf+1186));
          put_F4(r->dop_frq_r_slope, "%17.7f", 17, (buf+1203));
          put_F4(r->dop_frq_r_quad, "%17.7f", 17, (buf+1220));
          put_F4(r->resol_azimuth, "%17.7f", 17, (buf+1237));
          put_F4(r->resol_range, "%17.7f", 17, (buf+1254));
          put_F4(r->azimuth_pixel, "%17.7f", 17, (buf+1271));
          put_F4(r->range_pixel, "%17.7f", 17, (buf+1288));
          put_chars((char*) (buf+1305), r->OBRC_flag, 4);
          put_I4(r->bits_sample, "%5d", 5, (buf+1309));
          put_F4(r->calib_est, "%17.7f", 17, (buf+1314));
          put_F4(r->bit_err_rate, "%17.7f", 17, (buf+1331));
          put_F4(r->SNR, "%17.7f", 17, (buf+1348));
          put_F4(r->est_noise_flr, "%17.7f", 17, (buf+1365));
          put_F4(r->radio_m_resol, "%17.7f", 17, (buf+1382));
          put_I4(r->satur_points, "%9d", 9, (buf+1399));
          put_chars((char*) (buf+1408), r->spec_flag, 4);
       }
       i = write( out, buf, nitems);
    }

    if (Dump ) {

       /* Write the contents of the Facility Related record into an ODL tree */
       (void) printf(
        "\n************ begin of Facility Related record ***************\n");
       (void) printf("\n RECORD SEQUENCE       %ld", d->rec_seq);
       (void) printf("\n RECORD SUB 1          %d",d->rec_sub1);
       (void) printf("\n RECORD TYPE           %d",d->rec_type);
       (void) printf("\n RECORD SUB 2          %d",d->rec_sub2);
       (void) printf("\n RECORD SUB 3          %d",d->rec_sub3);
       (void) printf("\n RECORD LENGTH         %ld\n",d->length);

       if ( d->rec_type != LFTYPE ) {
            i = strlen( r->bogus );
            if(  i  >  80  ){
                print_nice( r->bogus );
            }
             else  (void) printf("\n%s",r->bogus);
       }
       else {
    (void) printf("\n FACILITY RELATED REC NUM   %d",r->seq_num);
    (void) printf("\n DATA TAKE ID               %s",r->datatake_ID);
    (void) printf("\n IMAGE ID                   %s",r->image_ID);
    (void) printf("\n CORRELATION YEAR           %s",r->corr_year);
    (void) printf("\n CORRELATION TIME           %s",r->corr_GMT);
    (void) printf("\n REF IMG OR SITE NAME       %s",r->site_name);
    (void) printf("\n DATA YEAR                  %s",r->data_year);
    (void) printf("\n CENTER GMT                 %s",r->center_GMT);
    (void) printf("\n CENTER LATITUDE            %17.7f",r->center_LAT);
    (void) printf("\n CENTER LONGITUDE           %17.7f",r->center_LON);
    (void) printf("\n NEAR START LATITUDE        %17.7f",r->near_sta_LAT);
    (void) printf("\n NEAR START LONGITUDE       %17.7f",r->near_sta_LON);
    (void) printf("\n NEAR END LATITUDE          %17.7f",r->near_end_LAT);
    (void) printf("\n NEAR END LONGITUDE         %17.7f",r->near_end_LON);
    (void) printf("\n FAR START LATITUDE         %17.7f",r->far_sta_LAT);
    (void) printf("\n FAR START LONGITUDE        %17.7f",r->far_sta_LON);
    (void) printf("\n FAR END LATITUDE           %17.7f",r->far_end_LAT);
    (void) printf("\n FAR END LONGITUDE          %17.7f",r->far_end_LON);
    (void) printf("\n ACTUAL AZIMUTH             %17.7f",r->actual_azimuth);
    (void) printf("\n ACTUAL RANGE               %17.7f",r->actual_range);
    (void) printf("\n ACTUAL PIXELS              %ld",r->actual_pixels);
    (void) printf("\n ACTUAL LINES               %ld",r->actual_lines);
    (void) printf("\n TOTAL PIXELS               %ld",r->total_pixels);
    (void) printf("\n TOTAL LINES                %ld",r->total_lines);
    (void) printf("\n MEDIA ID                   %s",r->media_ID);
    (void) printf("\n START BLOCK                %ld",r->start_block);
    (void) printf("\n END BLOCK                  %ld",r->end_block);
    (void) printf("\n PLATFORM NAME              %s",r->platform_name);
    (void) printf("\n SENSOR AND MODE            %s",r->sensor_mode);
    (void) printf("\n PRF                        %17.7f",r->PRF);
    (void) printf("\n ANTENNA LOOK ANGLE         %17.7f",r->ant_look_angle);
    (void) printf("\n DATA RATE                  %17.7f",r->data_rate);
    (void) printf("\n DATA WINDOW POSITION       %17.7f",r->data_win_pos);
    (void) printf("\n RANGE GATE DELAY           %17.7f",r->range_gate_del);
    (void) printf("\n TRACK ANGLE                %17.7f",r->track_angle);
    (void) printf("\n ASC/DESCENDING FLAG        %s",r->ASC_DESC);
    (void) printf("\n SPACECRAFT ALTITUDE        %17.7f",r->S_C_altitude);
    (void) printf("\n X POSITION                 %23.10E",r->X_pos);
    (void) printf("\n Y POSITION                 %23.10E",r->Y_pos);
    (void) printf("\n Z POSITION                 %23.10E",r->Z_pos);
    (void) printf("\n X VELOCITY                 %23.10E",r->X_vel);
    (void) printf("\n Y VELOCITY                 %23.10E",r->Y_vel);
    (void) printf("\n Z VELOCITY                 %23.10E",r->Z_vel);
    (void) printf("\n ROLL                       %15.7f",r->roll);
    (void) printf("\n YAW                        %15.7f",r->yaw);
    (void) printf("\n PITCH                      %15.7f",r->pitch);
    (void) printf("\n ROLL FLAG                  %d",r->roll_flag);
    (void) printf("\n YAW FLAG                   %d",r->yaw_flag);
    (void) printf("\n PITCH FLAG                 %d",r->pitch_flag);
    (void) printf("\n ROLL RATE                  %15.7f",r->roll_rate);
    (void) printf("\n YAW RATE                   %15.7f",r->yaw_rate);
    (void) printf("\n PITCH RATE                 %15.7f",r->pitch_rate);
    (void) printf("\n ROLL RATE FLAG             %d",r->roll_rate_flag);
    (void) printf("\n YAW RATE FLAG              %d",r->yaw_rate_flag);
    (void) printf("\n PITCH RATE FLAG            %d",r->pitch_rate_flag);
    (void) printf("\n NADIR RADIUS               %17.7f",r->nadir_radius);
    (void) printf("\n IMAGE RADIUS               %17.7f",r->image_radius);
    (void) printf("\n INCIDENCE ANGLE            %17.7f",r->incidence_angle);
    (void) printf("\n PROCESSOR VERSION          %s",r->proc_version);
    (void) printf("\n PROCESSOR TYPE             %s",r->proc_type);
    (void) printf("\n TYPE OF EPHEMERIS          %d",r->type_ephemeris);
    (void) printf("\n EFF LOOKS AZIMUTH          %17.7f",r->looks_azimuth);
    (void) printf("\n EFF LOOKS RANGE            %17.7f",r->looks_range);
    (void) printf("\n WEIGH PEDESTAL HT AZI      %17.7f",r->WPH_azimuth);
    (void) printf("\n WEIGH PEDESTAL HT RNG      %17.7f",r->WPH_range);
    (void) printf("\n LOOK ENERGY NORM FLAG      %s",r->look_energy_eq);
    (void) printf("\n IND DISTORTION IN AZI      %17.7f",r->induced_azimuth);
    (void) printf("\n IND DISTORTION IN RNG      %17.7f",r->induced_range);
    (void) printf("\n RECEIVER GAIN              %17.7f",r->gain);
    (void) printf("\n SWATH VELOCITY             %17.7f",r->swath_velocity);
    (void) printf("\n SQUINT ANGLE               %17.7f",r->squint_angle);
    (void) printf("\n AVERAGE TERRAIN HEIGHT     %17.7f",r->avg_ter_height);
    (void) printf("\n PROCESSOR GAIN             %d",r->proc_gain);
    (void) printf("\n DESKEW APPLIED FLAG        %s",r->deskew_flag);
    (void) printf("\n SLANT RANGE FLAG           %s",r->gnd_slant_flag);
    (void) printf("\n SLANT RNG TO 1ST PIX       %17.7f",r->sl_rng_1st_pix);
    (void) printf("\n SLANT RNG TO LAST PIX      %17.7f",r->sl_rng_last_pix);
    (void) printf("\n START SAMPLE PROCESSED     %ld",r->start_sample);
    (void) printf("\n CLUTTERLOCK FLAG           %s",r->clutterlock_flg);
    (void) printf("\n DPLR FREQ CONST            %17.7f",r->dop_frq_const);
    (void) printf("\n DPLR FREQ SLOPE            %17.7f",r->dop_frq_slope);
    (void) printf("\n DPLR FREQ QUAD             %17.7f",r->dop_frq_quad);
    (void) printf("\n AUTOFOCUS FLAG             %s",r->autofocus_flag);
    (void) printf("\n DPLR FREQ RATE CONST       %17.7f",r->dop_frq_r_cnst);
    (void) printf("\n DPLR FREQ RATE SLOPE       %17.7f",r->dop_frq_r_slope);
    (void) printf("\n DPLR FREQ RATE QUAD        %17.7f",r->dop_frq_r_quad);
    (void) printf("\n RESOLUTION AZIMUTH         %17.7f",r->resol_azimuth);
    (void) printf("\n RESOLUTION RANGE           %17.7f",r->resol_range);
    (void) printf("\n AZIMUTH PIXEL              %17.7f",r->azimuth_pixel);
    (void) printf("\n RANGE PIXEL                %17.7f",r->range_pixel);
    (void) printf("\n OBRC FLAG                  %s",r->OBRC_flag);
    (void) printf("\n BITS SAMPLE                %d",r->bits_sample);
    (void) printf("\n CALIBRATION ESTIMATE       %17.7f",r->calib_est);
    (void) printf("\n BIT ERROR RATE             %17.7f",r->bit_err_rate);
    (void) printf("\n SIGNAL TO NOISE RATIO      %17.7f",r->SNR);
    (void) printf("\n ESTIMATED NOISE FLOOR      %17.7f",r->est_noise_flr);
    (void) printf("\n RADIOMETRIC RESOLUTION     %17.7f",r->radio_m_resol);
    (void) printf("\n SATURATED DATA POINTS      %ld",r->satur_points);
    (void) printf("\n WITHIN SPEC FLAG           %s",r->spec_flag);
       }
       (void) printf("\n**************** end of Facility Related record " );
       (void) printf("*******************\n");
    }

    if (r->next != (Fac_Related *) NULL && !mode)
        (void) Write_L_FR_ODL( r->next, out, mode );
    return( IMS_ERROR);
}



SARL_ptr* Allocate_SARL( void ) {
    /* allocate a temp buffer */

   if (work == (unsigned char*) NULL)
       work = (unsigned char*) calloc(1,BUF_BIG_SIZE*sizeof(unsigned char));

   return ( ( (SARL_ptr *) calloc (1,sizeof (SARL_ptr))) );
}


int Allocate_SARL_ODR( SARL_ptr* tree)
{
/*    Sarl_Desc_Rec* s= &(tree->descript);  */
   if ( tree != NULL ) {
      /* Allocate space for records based on the number stated in the FDR */
      tree->read_count=1;
      tree->write_count=0;
      (void) Allocate_Data_Sum( tree );
      (void) Allocate_Map_Proj( tree );
      (void) Allocate_Plat_Pos( tree );
      (void) Allocate_Att_Data( tree );
      (void) Allocate_Radi_Data( tree );
      (void) Allocate_Radi_Comp( tree );
      (void) Allocate_Qual_Sum( tree );
      (void) Allocate_Data_Hist( tree );
      (void) Allocate_Rng_Spec( tree );
      (void) Allocate_Digital_Elev( tree );
      (void) Allocate_Fac_Related( tree );

     if(  status_save  <  IMS_OK )  return( status_save );
      return( IMS_OK );
   }
   return( IMS_ERROR );
}


Dataset_Sum* Allocate_Data_Sum( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Dataset_Sum *ds, *ds1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_dataset) return(NULL);
    ds = tree->data_sum = NULL;
    for (i=0 ; i  <  d->n_dataset ; i++ ){
        if ( (ds1 = (Dataset_Sum *) malloc (sizeof (Dataset_Sum)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Dataset Sum.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(ds1, 0, sizeof( Dataset_Sum));
        if(  i  ==  0  )   ds = tree->data_sum = &ds1[0];
        else{
            ds->next = &ds1[0];
            ds = ds->next;
        }
        ds->next = NULL;
    }
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
    mp = tree->map_proj = NULL;
    for (i=0 ; i  <  d->n_map_proj ; i++ ){
        if ( (mp1 = (Map_Proj *) malloc (sizeof (Map_Proj)) )
            == NULL ) {
            (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Map Projection.");
            status_save = IMS_ERROR;
            return(NULL);
        }
        (void) memset(mp1, 0, sizeof( Map_Proj));
        if(  i  ==  0  )   mp = tree->map_proj = &mp1[0];
        else{
            mp->next = &mp1[0];
            mp = mp->next;
        }
        mp->next = NULL;
    }
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
    p = tree->platform = NULL;
    for (i=0 ; i  <  d->n_plat_pos ; i++ ){
        if ( (p1 = (Pos_Data *) malloc (sizeof (Pos_Data)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Platform Position.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(p1, 0, sizeof( Pos_Data));
        if(  i  ==  0  )   p = tree->platform = &p1[0];
        else{
            p->next = &p1[0];
            p = p->next;
        }
        p->next = NULL;
    }
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
    a = tree->attitude = NULL;
    for (i=0 ; i  <  d->n_att_data ; i++ ){
        if ( (a1 = (Att_Data *) malloc (sizeof (Att_Data)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Attitude Data.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(a1, 0, sizeof( Att_Data));
        if(  i  ==  0  )   a = tree->attitude = &a1[0];
        else{
            a->next = &a1[0];
            a = a->next;
        }
        a->next = NULL;
    }
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
    r = tree->radio_data = NULL;
    for (i=0 ; i  <  d->n_radi_data ; i++ ){
        if ( (r1 = (Radi_Data *) malloc (sizeof (Radi_Data)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Radio Data.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(r1, 0, sizeof( Radi_Data));
        if(  i  ==  0  )   r = tree->radio_data = &r1[0];
        else{
            r->next = &r1[0];
            r = r->next;
        }
        r->next = NULL;
    }
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
    r = tree->radio_comp = NULL;
    for (i=0 ; i  <  d->n_radi_comp ; i++ ){
        if ( (r1 = (Radi_Comp *) malloc (sizeof (Radi_Comp)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Radio Compensation.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(r1, 0, sizeof( Radi_Comp));
        if(  i  ==  0  )   r = tree->radio_comp = &r1[0];
        else{
            r->next = &r1[0];
            r = r->next;
        }
        r->next = NULL;
    }
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
    q = tree->data_qual = NULL;
    for (i=0 ; i  <  d->n_qual_sum ; i++ ){
        if ( (q1 = (Qual_Sum *) malloc (sizeof (Qual_Sum)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Quality Sum.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(q1, 0, sizeof( Qual_Sum));
        if(  i  ==  0  )   q = tree->data_qual = &q1[0];
        else{
            q->next = &q1[0];
            q = q->next;
        }
        q->next = NULL;
    }
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
    h = tree->histogram = NULL;
    for (i=0 ; i  <  d->n_data_hist ; i++ ){
        if ( (h1 = (Data_Hist *) malloc (sizeof (Data_Hist)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Data Histogram.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(h1, 0, sizeof( Data_Hist));
        if(  i  ==  0  )   h = tree->histogram = &h1[0];
        else{
            h->next = &h1[0];
            h = h->next;
        }
        h->next = NULL;
    }
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
    r = tree->spectra = NULL;
    for (i=0 ; i  <  d->n_rang_spec ; i++ ){
        if ( (r1 = (Rng_Spec *) malloc (sizeof (Rng_Spec)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Range Spectra.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(r1, 0, sizeof( Rng_Spec));
        if(  i  ==  0  )   r = tree->spectra = &r1[0];
        else{
            r->next = &r1[0];
            r = r->next;
        }
        r->next = NULL;
    }
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
    r = tree->elevation = NULL;
    for (i=0 ; i  <  d->n_dem_desc ; i++ ){
        if ( (r1 = (Digital_Elev *) malloc (sizeof (Digital_Elev)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Digital Elevation.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(r1, 0, sizeof( Digital_Elev));
        if(  i  ==  0  )   r = tree->elevation = &r1[0];
        else{
            r->next = &r1[0];
            r = r->next;
        }
        r->next = NULL;
    }
    return( tree->elevation );
}

Fac_Related* Allocate_Fac_Related( SARL_ptr* tree )
{
    Sarl_Desc_Rec* d;
    Fac_Related *r, *r1;
    int i;

    if (tree==NULL) return(NULL);
    d = &(tree->descript);
    if (!d->n_faci_data) return(NULL);
    r = tree->facility = NULL;
    for (i=0 ; i  <  d->n_faci_data ; i++ ){
        if ( (r1 = (Fac_Related *) malloc (sizeof (Fac_Related)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Facility Related.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(r1, 0, sizeof( Fac_Related));
        if(  i  ==  0  )   r = tree->facility = &r1[0];
        else{
            r->next = &r1[0];
            r = r->next;
        }
        r->next = NULL;
    }
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
   Fac_Related *fr, *fr_next;

   if (work != (unsigned char*) NULL)
      free ((void*) work);

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

      pd = tree->platform;
      while (pd != NULL) {
            pd_next = pd->next;
            free( pd );
            pd = pd_next;
      }

      ad = tree->attitude;
      while (ad != NULL) {
            ad_next = ad->next;
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

      dh = tree->histogram;
      while (dh != NULL) {
            dh_next = dh->next;
            free( dh );
            dh = dh_next;
      }

      rs = tree->spectra;
      while (rs != NULL) {
            rs_next = rs->next;
            free( rs );
            rs = rs_next;
      }

      de = tree->elevation;
      while (de != NULL) {
            de_next = de->next;
            free( de );
            de = de_next;
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

int Read_FDR_SARL( FILE* fp, SARL_ptr* ldr, int desc )
{
    int nbytes;
    int rsize;
    int ret;
    unsigned char* tb;
    unsigned char* buf = work;
    short  i,j,k,l;
    union  char_is{
        short  i_c;
        unsigned  char   ary[2];
    };
    union  char_is  char_i;


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
                if ( MATCH_RECORD(tb, LD1SUB, LDTYPE, LD2SUB, LD3SUB) ){
                   rsize = cvt2int(&buf[8]) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the File Desciptor
                            Record structure */
                         i = write( desc, buf, rsize+12 );
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
                   char_i.i_c = 0;
                   char_i.ary[1] = buf[4];
                   i = char_i.i_c;
                   char_i.ary[1] = buf[5];
                   j = char_i.i_c;
                   char_i.ary[1] = buf[6];
                   k = char_i.i_c;
                   char_i.ary[1] = buf[7];
                   l = char_i.i_c;
                   (void) ims_msg( msgDesc_save, IMS_ERROR,
                        "ProcessLdr:  Header not recognized: %d %d %d %d",
                        i,j,k,l );
                   status_save = IMS_ERROR;
                   ret=NO_MATCH;
                }
             }
    }
    return(ret);
}

void Decode_SARL_FDR( unsigned char* buf, Sarl_Desc_Rec* t )
{
     desc_rec *d = &(t->desc);

     d->rec_seq = cvt2int(buf);
     d->rec_sub1 = *(buf+4);
     d->rec_type = *(buf+5);
     d->rec_sub2 = *(buf+6);
     d->rec_sub3 = *(buf+7);

     (void) strncpy(t->ascii_flag, (char*) (buf+12), 2);
     (void) strncpy(t->format_doc, (char*) (buf+16), 12);
     (void) strncpy(t->format_rev, (char*) (buf+28), 2);
     (void) strncpy(t->design_rev, (char*) (buf+30), 2);
     (void) strncpy(t->software_id, (char*) (buf+32), 12);
     t->file_num = (short) get_I4(&buf[44], 4);
     (void) strncpy(t->file_name, (char*) (buf+48), 16);
     (void) strncpy(t->rec_seq, (char*) (buf+64), 4);
     t->seq_loc = get_I4(&buf[68], 8);
     t->seq_len = (short) get_I4(&buf[76], 4);
     (void) strncpy(t->rec_code, (char*) (buf+80), 4);
     t->code_loc = get_I4(&buf[84], 8);
     t->code_len = (short) get_I4(&buf[92], 4);
     (void) strncpy(t->rec_len, (char*) (buf+96), 4);
     t->rlen_loc = get_I4(&buf[100], 8);
     t->rlen_len = (short) get_I4(&buf[108], 4);
     t->n_dataset = get_I4(&buf[180], 6);
     t->l_dataset = get_I4(&buf[186], 6);
     t->n_map_proj = get_I4(&buf[192], 6);
     t->l_map_proj = get_I4(&buf[198], 6);
     t->n_plat_pos = get_I4(&buf[204], 6);
     t->l_plat_pos = get_I4(&buf[210], 6);
     t->n_att_data = get_I4(&buf[216], 6);
     t->l_att_data = get_I4(&buf[222], 6);
     t->n_radi_data = get_I4(&buf[228], 6);
     t->l_radi_data = get_I4(&buf[234], 6);
     t->n_radi_comp = get_I4(&buf[240], 6);
     t->l_radi_comp = get_I4(&buf[246], 6);
     t->n_qual_sum = get_I4(&buf[252], 6);
     t->l_qual_sum = get_I4(&buf[258], 6);
     t->n_data_hist = get_I4(&buf[264], 6);
     t->l_data_hist = get_I4(&buf[270], 6);
     t->n_rang_spec = get_I4(&buf[276], 6);
     t->l_rang_spec = get_I4(&buf[282], 6);
     t->n_dem_desc = get_I4(&buf[288], 6);
     t->l_dem_desc = get_I4(&buf[294], 6);
     t->n_radar_par = get_I4(&buf[300], 6);
     t->l_radar_par = get_I4(&buf[306], 6);
     t->n_anno_data = get_I4(&buf[312], 6);
     t->l_anno_data = get_I4(&buf[318], 6);
     t->n_det_proc = get_I4(&buf[324], 6);
     t->l_det_proc = get_I4(&buf[330], 6);
     t->n_cal = get_I4(&buf[336], 6);
     t->l_cal = get_I4(&buf[342], 6);
     t->n_gcp = get_I4(&buf[348], 6);
     t->l_gcp = get_I4(&buf[354], 6);
     t->n_faci_data = get_I4(&buf[420], 6);
     t->l_faci_data = get_I4(&buf[426], 6);
}

int Read_ALL_SARL( FILE* fp, SARL_ptr* ldr, int desc )
{
    int nbytes;
    int rsize;
    int ret=MATCH_FOUND;
    unsigned char* tb;
    unsigned char* buf = work;
    short  i,j,k,l;
    union  char_is{
        short  i_c;
        unsigned  char   ary[2];
    };
    union  char_is  char_i;


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
                   rsize = cvt2int(&buf[8]) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Dataset Summary
                            structure */
                         i = write( desc, buf, rsize+12 );
                         if ( (ret=Decode_SARL_DSR( buf, ldr) ) !=
                            DECODE_ERROR) {
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
                else if ( MATCH_RECORD(tb, LM1SUB, LMTYPE, LM2SUB,
                   LM3SUB) ){
                   rsize = cvt2int(&buf[8]) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Map Projection
                            structure */
                         i = write( desc, buf, rsize+12 );
                         if ( (ret=Decode_SARL_MPR( buf, ldr) ) !=
                            DECODE_ERROR) {
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
                else if ( MATCH_RECORD(tb, LP1SUB, LPTYPE, LP2SUB,
                   LP3SUB) ) {
                   rsize = cvt2int(&buf[8]) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Platform Position
                            structure */
                         i = write( desc, buf, rsize+12 );
                         if ( (ret=Decode_SARL_PPR( buf, ldr) ) !=
                            DECODE_ERROR) {
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
                else if ( MATCH_RECORD(tb, LA1SUB, LATYPE, LA2SUB,
                   LA3SUB) ) {
                   rsize = cvt2int(&buf[8]) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Attitude
                            structure */
                         i = write( desc, buf, rsize+12 );
                         if ( (ret=Decode_SARL_ATR( buf, ldr) ) !=
                            DECODE_ERROR) {
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
                else if ( MATCH_RECORD(tb, LR1SUB, LRTYPE, LR2SUB,
                   LR3SUB) ) {
                   rsize = cvt2int(&buf[8]) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Radiometric Data
                            structure */
                         i = write( desc, buf, rsize+12 );
                         if ( (ret=Decode_SARL_RDR( buf, ldr) ) !=
                            DECODE_ERROR) {
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
                else if ( MATCH_RECORD(tb, LC1SUB, LCTYPE, LC2SUB,
                   LC3SUB) ) {
                   rsize = cvt2int(&buf[8]) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Radiometric
                            Compensation structure */
                         i = write( desc, buf, rsize+12 );
                         if ( (ret=Decode_SARL_RCR( buf, ldr) ) !=
                            DECODE_ERROR) {
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
                else if ( MATCH_RECORD(tb, LQ1SUB, LQTYPE, LQ2SUB,
                   LQ3SUB) ) {
                   rsize = cvt2int(&buf[8]) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Data Quality Summary
                            structure */
                         i = write( desc, buf, rsize+12 );
                         if ( (ret=Decode_SARL_DQSR( buf, ldr) ) !=
                            DECODE_ERROR) {
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
                   rsize = cvt2int(&buf[8]) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Data Histograms
                            structure */
                         i = write( desc, buf, rsize+12 );
                         if ( (ret=Decode_SARL_DHR( buf, ldr) ) !=
                            DECODE_ERROR) {
                            ldr->histogram->desc.length=rsize+12;
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
                else if ( MATCH_RECORD(tb, LZ1SUB, LZTYPE, LZ2SUB,
                   LZ3SUB) ) {
                   rsize = cvt2int(&buf[8]) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Range Spectra
                            structure */
                         i = write( desc, buf, rsize+12 );
                         if ( (ret=Decode_SARL_RSR( buf, ldr) ) !=
                            DECODE_ERROR) {
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
                else if ( MATCH_RECORD(tb, LE1SUB, LETYPE, LE2SUB,
                   LE3SUB) ) {
                   rsize = cvt2int(&buf[8]) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Digital Elevation
                            structure */
                         i = write( desc, buf, rsize+12 );
                         if ( (ret=Decode_SARL_DEM( buf, ldr) ) !=
                            DECODE_ERROR) {
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
                else if ( MATCH_RECORD(tb, LF1SUB, LFTYPE, LF2SUB, LF3SUB) ||
                          MATCH_RECORD(tb, LF1ASUB, LIMTYPE, LF2SUB, LF3SUB) ||
                          MATCH_RECORD(tb, LF1ASUB, LICTYPE, LF2SUB, LF3SUB) ||
                          MATCH_RECORD(tb, LF1ASUB, LWMTYPE, LF2SUB, LF3SUB) ||
                          MATCH_RECORD(tb, LF1SUB, LFTYPE2, LF2SUB, LF3SUB) ){
                   rsize = cvt2int(&buf[8]) - 12;
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char), rsize );
                      if ( nbytes == rsize) {
                         /* decode the record into the Facility Related
                            structure */
                         i = write( desc, buf, rsize+12 );
                         if ( (ret=Decode_SARL_FRR( buf, ldr) ) !=
                            DECODE_ERROR) {
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
                else {
                   rsize = cvt2int(&buf[8]) - 12;
                   char_i.i_c = 0;
                   char_i.ary[1] = buf[4];
                   i = char_i.i_c;
                   char_i.ary[1] = buf[5];
                   j = char_i.i_c;
                   char_i.ary[1] = buf[6];
                   k = char_i.i_c;
                   char_i.ary[1] = buf[7];
                   l = char_i.i_c;
                   (void) ims_msg( msgDesc_save, IMS_INFO,
                        "ProcessLdr:  Header not recognized:  %d %d %d %d",
                    i,j,k,l );
                   if ( rsize>0 ) {
                      nbytes = read_record( fp, (buf+12),
                        sizeof(unsigned char),rsize );
                      i = write( desc, buf, rsize+12 );
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
    int i=1;
    int n_data = t->descript.n_dataset;
    desc_rec* d;
    int off;

    if (ds == NULL) {
       (void) ims_msg( msgDesc_save, IMS_ERROR,
"ProcessLdr:  FDR did not have a Dataset Summary record."
        );
        status_save = IMS_ERROR;
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (ds->seq_num !=0) {
           if (i<=n_data) {
              ds=ds->next;
           }
           else{
              (void) ims_msg( msgDesc_save, IMS_ERROR,
"ProcessLdr:  Dataset Summary record has a processing error."
                 );
                status_save = IMS_ERROR;
              return(DECODE_ERROR);
           }
           i++;
       }
    }

    if(  Debug )  (void) printf("\n Decode Data Set Summary record\n");

    /* process the record */
    d = &(ds->desc);
    d->rec_seq = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    ds->seq_num = (short) get_I4( &buf[12], 4);
    ds->sar_chn = (short) get_I4( &buf[16], 4);
    (void) strncpy(ds->scene_id, (char*) (buf+20), 16);
    (void) strncpy(ds->scene_des, (char*) (buf+36), 32);
    (void) strncpy(ds->inp_sctim, (char*) (buf+68), 32);
    (void) strncpy(ds->asc_des, (char*) (buf+100), 16);
    ds->pro_lat = get_F4( &buf[116], 16);
    ds->pro_long = get_F4( &buf[132], 16);
    ds->pro_head = get_F4( &buf[148], 16);
    (void) strncpy(ds->ellip_des, (char*) (buf+164), 16);
    ds->ellip_maj = get_F4( &buf[180], 16);
    ds->ellip_min = get_F4( &buf[196], 16);
    ds->earth_mass = get_F4( &buf[212], 16);
    ds->grav_const = get_F4( &buf[228], 16);
    ds->ellip_j[0] = get_F4( &buf[244], 16);
    ds->ellip_j[1] = get_F4( &buf[260], 16);
    ds->ellip_j[2] = get_F4( &buf[276], 16);
    ds->terrain_h = get_F4( &buf[308], 16);
    ds->sc_lin = get_I4( &buf[324], 16);
    ds->sc_pix = get_I4( &buf[332], 16);
    ds->scene_len = get_F4( &buf[340], 16);
    ds->scene_wid = get_F4( &buf[356], 16);
    ds->nchn = (short) get_I4( &buf[388], 4);
    (void) strncpy(ds->mission_id, (char*) (buf+396), 16);
    (void) strncpy(ds->sensor_id, (char*) (buf+412), 32);
    (void) strncpy(ds->orbit_num, (char*) (buf+444), 8);
    ds->plat_lat = get_F4( &buf[452], 8);
    ds->plat_long = get_F4( &buf[460], 8);
    ds->plat_head = get_F4( &buf[468], 8);
    ds->clock_ang = get_F4( &buf[476], 8);
    ds->incident_ang = get_F4( &buf[484], 8);
    ds->frequency = get_F4( &buf[492], 8);
    ds->wave_length = get_F4( &buf[500], 16);
    (void) strncpy(ds->motion_comp, (char*) (buf+516), 2);
    (void) strncpy(ds->pulse_code, (char*) (buf+518), 16);
    ds->ampl_coef[0] = get_F4( &buf[534], 16);
    ds->ampl_coef[1] = get_F4( &buf[550], 16);
    ds->ampl_coef[2] = get_F4( &buf[566], 16);
    ds->ampl_coef[3] = get_F4( &buf[582], 16);
    ds->ampl_coef[4] = get_F4( &buf[598], 16);
    ds->phas_coef[0]= get_F4( &buf[614], 16);
    ds->phas_coef[1]= get_F4( &buf[630], 16);
    ds->phas_coef[2]= get_F4( &buf[646], 16);
    ds->phas_coef[3]= get_F4( &buf[662], 16);
    ds->phas_coef[4]= get_F4( &buf[678], 16);
    ds->chirp_ext_ind = get_I4( &buf[694], 8);
    ds->fr = get_F4( &buf[710], 16);
    ds->rng_gate = get_F4( &buf[726], 16);
    ds->rng_length = get_F4( &buf[742], 16);
    (void) strncpy(ds->baseband_f, (char*) (buf+758), 4);
    (void) strncpy(ds->rngcmp_f, (char*) (buf+762), 4);
    ds->gn_polar = get_F4( &buf[766], 16);
    ds->gn_cross = get_F4( &buf[782], 16);
    ds->chn_bits = get_I4( &buf[798], 8);
    (void) strncpy(ds->quant_desc, (char*) (buf+806), 12);
    ds->i_bias = get_F4( &buf[818], 16);
    ds->q_bias = get_F4( &buf[834], 16);
    ds->iq_ratio = get_F4( &buf[850], 16);
    ds->ele_sight = get_F4( &buf[898], 16);
    ds->mech_sight = get_F4( &buf[914], 16);
    (void) strncpy(ds->echo_track, (char*) (buf+930), 4);
    ds->fa = get_F4( &buf[934], 16);
    ds->elev_beam = get_F4( &buf[950], 16);
    ds->azim_beam = get_F4( &buf[966], 16);
    (void) strncpy(ds->sat_bintim, (char *) (buf+982), 16);
    (void) strncpy(ds->sat_clktim, (char*) (buf+998), 32);
    ds->sat_clkinc = get_I4( &buf[1030], 8);
    (void) strncpy(ds->fac_id, (char*) (buf+1046), 16);
    (void) strncpy(ds->sys_id, (char*) (buf+1062), 8);
    (void) strncpy(ds->ver_id, (char*) (buf+1070), 8);
    (void) strncpy(ds->fac_code, (char*) (buf+1078), 16);
    (void) strncpy(ds->lev_code, (char*) (buf+1094), 16);
    (void) strncpy(ds->prod_type, (char*) (buf+1110), 32);
    (void) strncpy(ds->algor_id, (char*) (buf+1142), 32);
    ds->n_azilok = get_F4( &buf[1174], 16);
    ds->n_rnglok = get_F4( &buf[1190], 16);
    ds->bnd_azilok = get_F4( &buf[1206], 16);
    ds->bnd_rnglok = get_F4( &buf[1222], 16);
    ds->bnd_azi = get_F4( &buf[1238], 16);
    ds->bnd_rng = get_F4( &buf[1254], 16);
    (void) strncpy(ds->azi_weight, (char*) (buf+1270), 32);
    (void) strncpy(ds->rng_weight, (char*) (buf+1302), 32);
    (void) strncpy(ds->data_inpsrc, (char*) (buf+1334), 16);
    ds->rng_res = get_F4( &buf[1350], 16);
    ds->azi_res = get_F4( &buf[1366], 16);
    ds->radi_stretch[0] = get_F4( &buf[1382], 16);
    ds->radi_stretch[1] = get_F4( &buf[1398], 16);
    ds->alt_dopcen[0] = get_F4( &buf[1414], 16);
    ds->alt_dopcen[1] = get_F4( &buf[1430], 16);
    ds->alt_dopcen[2] = get_F4( &buf[1446], 16);
    ds->crt_dopcen[0] = get_F4( &buf[1478], 16);
    ds->crt_dopcen[1] = get_F4( &buf[1494], 16);
    ds->crt_dopcen[2] = get_F4( &buf[1510], 16);
    (void) strncpy(ds->time_dir_pix, (char*) (buf+1526), 8);
    (void) strncpy(ds->time_dir_lin, (char*) (buf+1534), 8);
    ds->alt_rate[0] = get_F4( &buf[1542], 16);
    ds->alt_rate[1] = get_F4( &buf[1558], 16);
    ds->alt_rate[2] = get_F4( &buf[1574], 16);
    ds->crt_rate[0] = get_F4( &buf[1606], 16);
    ds->crt_rate[1] = get_F4( &buf[1622], 16);
    ds->crt_rate[2] = get_F4( &buf[1638], 16);
    (void) strncpy(ds->line_cont, (char*) (buf+1670), 8);
    (void) strncpy(ds->clutter_lock, (char*) (buf+1678), 4);
    (void) strncpy(ds->auto_focus, (char*) (buf+1682), 4);
    ds->line_spacing = get_F4( &buf[1686], 16);
    ds->pix_spacing = get_F4( &buf[1702], 16);
    (void) strncpy(ds->rngcmp_desg, (char*) (buf+1718), 16);
    /* *********
    ds->annot_pts=get_I4( &buf[2006], 8);
    for (i=0, off=2022; i< ds->annot_pts; i++) {
        ds->annot_line[i] = get_I4( &buf[off], 8); off+=8;
        ds->annot_pixel[i] = get_I4( &buf[off], 8); off+=8;
      (void) strncpy(&(ds->annot_text[i][0]), (char *) (buf+off), 16); off+=16;
    }
    ******** */
    return(DECODE_OK);
}

int  Decode_SARL_MPR( unsigned char* buf, SARL_ptr* t )
{
    Map_Proj* mp = t->map_proj;
    int i=1;
    int n_data = t->descript.n_map_proj;
    desc_rec* d;

    if (mp == NULL) {
       (void) ims_msg( msgDesc_save, IMS_ERROR,
        "ProcessLdr:  FDR does not have a Map Projection Data rec."
        );
       status_save = IMS_ERROR;
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (mp->seq_num !=0) {
           if (i<=n_data) {
              mp=mp->next;
           }
           else{
                (void) ims_msg( msgDesc_save, IMS_ERROR,
            "ProcessLdr:  Map Projection record has processing error."
                    );
                status_save = IMS_ERROR;
                return(DECODE_ERROR);
           }
           i++;
       }
    }
    if(  Debug )  (void) printf(
        "\n Decode Map Projection Data record\n");

    /* process the record */
    d = &(mp->desc);
    d->rec_seq = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    mp->seq_num =  d->rec_seq;

    (void) strncpy(mp->map_desc, (char *) (buf+28), 32);
    mp->n_pixel = get_I4( &buf[60], 16);
    mp->n_line =  get_I4( &buf[76], 16);
    mp->pix_spacing = get_F4( &buf[92], 16);
    mp->line_spacing = get_F4( &buf[108], 16);
    mp->osc_orient = get_F4( &buf[124], 16);
    mp->orb_incl = get_F4( &buf[140], 16);
    mp->asc_node = get_F4( &buf[156], 16);
    mp->isc_dist = get_F4( &buf[172], 16);
    mp->geo_alt = get_F4( &buf[188], 16);
    mp->isc_vel = get_F4( &buf[204], 16);
    mp->plat_head = get_F4( &buf[220], 16);
    (void) strncpy(mp->ref_ellip, (char*) (buf+236), 32);
    mp->semi_major = get_F4( &buf[268], 16);
    mp->semi_minor = get_F4( &buf[284], 16);
    mp->datum_shift[0] = get_F4( &buf[300], 16);
    mp->datum_shift[1] = get_F4( &buf[316], 16);
    mp->datum_shift[2] = get_F4( &buf[332], 16);
    mp->aux_datum_shift[0] = get_F4( &buf[348], 16);
    mp->aux_datum_shift[1] = get_F4( &buf[364], 16);
    mp->aux_datum_shift[2] = get_F4( &buf[380], 16);
    mp->scal_ellip = get_F4( &buf[396], 16);
    (void) strncpy(mp->proj_desc, (char*) (buf+412), 32);
    (void) strncpy(mp->utm_desc, (char*) (buf+444), 32);
    (void) strncpy(mp->utm_zone_sig, (char*) (buf+476), 4);
    mp->utm_east_orig = get_F4( &buf[480], 16);
    mp->utm_north_orig = get_F4( &buf[496], 16);
    mp->utm_cent_long = get_F4( &buf[512], 16);
    mp->utm_cent_lat = get_F4( &buf[528], 16);
    mp->utm_stand_par[0] = get_F4( &buf[544], 16);
    mp->utm_stand_par[1] = get_F4( &buf[560], 16);
    mp->utm_scale = get_F4( &buf[576], 16);
    (void) strncpy(mp->ups_desc, (char*) (buf+592), 32);
    mp->ups_cent_long = get_F4( &buf[624], 16);
    mp->ups_cent_lat = get_F4( &buf[640], 16);
    mp->ups_scale = get_F4( &buf[656], 16);
    (void) strncpy(mp->nsp_desc, (char*) (buf+672), 32);
    mp->nsp_east_orig = get_F4( &buf[704], 16);
    mp->nsp_north_orig = get_F4( &buf[720], 16);
    mp->nsp_cent_long = get_F4( &buf[736], 16);
    mp->nsp_cent_lat = get_F4( &buf[752], 16);
    mp->nsp_stand_par[0] = get_F4( &buf[768], 16);
    mp->nsp_stand_par[1] = get_F4( &buf[784], 16);
    mp->nsp_stand_par[2] = get_F4( &buf[800], 16);
    mp->nsp_stand_par[3] = get_F4( &buf[816], 16);
    mp->nsp_stand_mer[0] = get_F4( &buf[832], 16);
    mp->nsp_stand_mer[1] = get_F4( &buf[848], 16);
    mp->nsp_stand_mer[2] = get_F4( &buf[864], 16);
    mp->corner_ne[0] = get_F4( &buf[944], 16);
    mp->corner_ne[1] = get_F4( &buf[960], 16);
    mp->corner_ne[2] = get_F4( &buf[976], 16);
    mp->corner_ne[3] = get_F4( &buf[992], 16);
    mp->corner_ne[4] = get_F4( &buf[1008], 16);
    mp->corner_ne[5] = get_F4( &buf[1024], 16);
    mp->corner_ne[6] = get_F4( &buf[1040], 16);
    mp->corner_ne[7] = get_F4( &buf[1056], 16);
    mp->corner_ll[0] = get_F4( &buf[1072], 16);
    mp->corner_ll[1] = get_F4( &buf[1088], 16);
    mp->corner_ll[2] = get_F4( &buf[1104], 16);
    mp->corner_ll[3] = get_F4( &buf[1120], 16);
    mp->corner_ll[4] = get_F4( &buf[1136], 16);
    mp->corner_ll[5] = get_F4( &buf[1152], 16);
    mp->corner_ll[6] = get_F4( &buf[1168], 16);
    mp->corner_ll[7] = get_F4( &buf[1184], 16);
    mp->terr_height[0] = get_F4( &buf[1200], 16);
    mp->terr_height[1] = get_F4( &buf[1216], 16);
    mp->terr_height[2] = get_F4( &buf[1232], 16);
    mp->terr_height[3] = get_F4( &buf[1248], 16);
    mp->lp_conv_coef[0] = (double) get_F4( &buf[1264], 20);
    mp->lp_conv_coef[1] = (double) get_F4( &buf[1284], 20);
    mp->lp_conv_coef[2] = (double) get_F4( &buf[1304], 20);
    mp->lp_conv_coef[3] = (double) get_F4( &buf[1324], 20);
    mp->lp_conv_coef[4] = (double) get_F4( &buf[1344], 20);
    mp->lp_conv_coef[5] = (double) get_F4( &buf[1364], 20);
    mp->lp_conv_coef[6] = (double) get_F4( &buf[1384], 20);
    mp->lp_conv_coef[7] = (double) get_F4( &buf[1404], 20);
    mp->mp_conv_coef[0] = (double) get_F4( &buf[1424], 20);
    mp->mp_conv_coef[1] = (double) get_F4( &buf[1444], 20);
    mp->mp_conv_coef[2] = (double) get_F4( &buf[1464], 20);
    mp->mp_conv_coef[3] = (double) get_F4( &buf[1484], 20);
    mp->mp_conv_coef[4] = (double) get_F4( &buf[1504], 20);
    mp->mp_conv_coef[5] = (double) get_F4( &buf[1524], 20);
    mp->mp_conv_coef[6] = (double) get_F4( &buf[1544], 20);
    mp->mp_conv_coef[7] = (double) get_F4( &buf[1564], 20);
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
       (void) ims_msg( msgDesc_save, IMS_ERROR,
            "ProcessLdr:  FDR did not have a Platform Position Data rec." );
        status_save = IMS_ERROR;
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
    if(  Debug  )  (void) printf(
            "\n Decode Platform Position Data record\n");

    /* process the record */
    d = &(p->desc);
    d->rec_seq = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    p->seq_num =  d->rec_seq;

    (void) strncpy(p->orbit_ele_desg, (char*) (buf+12), 32);
    p->orbit_ele[0] = get_F4( &buf[44], 16);
    p->orbit_ele[1] = get_F4( &buf[60], 16);
    p->orbit_ele[2] = get_F4( &buf[76], 16);
    p->orbit_ele[3] = get_F4( &buf[92], 16);
    p->orbit_ele[4] = get_F4( &buf[108], 16);
    p->orbit_ele[5] = get_F4( &buf[124], 16);
    p->ndata = (short) get_I4( &buf[140], 4);
    p->year = (short) get_I4( &buf[144], 4);
    p->month = (short) get_I4( &buf[148], 4);
    p->day = (short) get_I4( &buf[152], 4);
    p->gmt_day = (short) get_I4( &buf[156], 4);
    p->gmt_sec = (double) get_F4( &buf[160], 22);
    p->data_int = (double) get_F4( &buf[182], 22);
    (void) strncpy(p->ref_coord, (char*) (buf+204), 64);
    p->hr_angle = (double) get_F4( &buf[268], 22);
    p->alt_poserr = get_F4( &buf[290], 16);
    p->crt_poserr = get_F4( &buf[306], 16);
    p->rad_poserr = get_F4( &buf[322], 16);
    p->alt_velerr = get_F4( &buf[338], 16);
    p->crt_velerr = get_F4( &buf[354], 16);
    p->rad_velerr = get_F4( &buf[370], 16);

    off=386;
    pv = &(p->pos_vect[0]);
    for( i=0;i<p->ndata;i++) {
       pv[i].pos[0] = (double) get_F4( &buf[off], 22); off+=22;
       pv[i].pos[1] = (double) get_F4( &buf[off], 22); off+=22;
       pv[i].pos[2] = (double) get_F4( &buf[off], 22); off+=22;
       pv[i].vel[0] = (double) get_F4( &buf[off], 22); off+=22;
       pv[i].vel[1] = (double) get_F4( &buf[off], 22); off+=22;
       pv[i].vel[2] = (double) get_F4( &buf[off], 22); off+=22;
    }
    return(DECODE_OK);
}


int  Decode_SARL_ATR( unsigned char* buf, SARL_ptr* t )
{
    Att_Data* a = t->attitude;
    int n_data = t->descript.n_att_data;
    desc_rec* d;
    int i;
    Att_Vect_Rec *av;
    int off;


    i = 0;
    if (a == NULL) {
       (void) ims_msg( msgDesc_save, IMS_ERROR,
        "ProcessLdr:  FDR did not have a Attitude Data record." );
       status_save = IMS_ERROR;
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (a->seq_num !=0) {
           if (i<=n_data) {
              a=a->next;
           }
           else{
                (void) ims_msg( msgDesc_save, IMS_ERROR,
                    "ProcessLdr:  Attitude Data processing error." );
                status_save = IMS_ERROR;
                return(DECODE_ERROR);
           }
           i++;
       }
    }
    if(  Debug )  (void) printf("\n Decode Attitude Data record\n");

    /* process the record */
    d = &(a->desc);
    d->rec_seq = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    a->seq_num =  d->rec_seq;

    a->npoint = (short) get_I4( &buf[12], 4);

    /* Allocate memory for the specified number of compensation sets */

    av = Allocate_Attitude_Sets( a );
    off=16;
    while(av!=NULL) {
       av->gmt_day = (short) get_I4( &buf[off], 4); off+=4;
       av->gmt_sec = get_I4( &buf[off], 8); off+=8;
       av->pitch_flag = (short) get_I4( &buf[off], 4); off+=4;
       av->roll_flag = (short) get_I4( &buf[off], 4); off+=4;
       av->yaw_flag = (short) get_I4( &buf[off], 4); off+=4;
       av->pitch = get_F4( &buf[off], 14); off+=14;
       av->roll = get_F4( &buf[off], 14); off+=14;
       av->yaw = get_F4( &buf[off], 14); off+=14;
       av->pitch_rate_flag = (short) get_I4( &buf[off], 4); off+=4;
       av->roll_rate_flag = (short) get_I4( &buf[off], 4); off+=4;
       av->yaw_rate_flag = (short) get_I4( &buf[off], 4); off+=4;
       av->pitch_rate = get_F4( &buf[off], 14); off+=14;
       av->roll_rate = get_F4( &buf[off], 14); off+=14;
       av->yaw_rate = get_F4( &buf[off], 14); off+=14;
       av=av->next;
    }

    return(DECODE_OK);
}

Att_Vect_Rec* Allocate_Attitude_Sets( Att_Data* a)
{
    Att_Vect_Rec *av, *av1;
    int i;

    if (a==NULL || a->npoint<1) return(NULL);
    av = a->att_vect = NULL;
    for (i=0 ; i  <  a->npoint ; i++ ){
        if ( (av1 = (Att_Vect_Rec *) malloc (sizeof (Att_Vect_Rec)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Att Vect Rec.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(av1, 0, sizeof( Att_Vect_Rec));
        if(  i  ==  0  )   av = a->att_vect = &av1[0];
        else{
            av->next = &av1[0];
            av = av->next;
        }
        av->next = NULL;
    }
    return( a->att_vect );
}


int  Decode_SARL_RDR( unsigned char* buf, SARL_ptr* t )
{
    Radi_Data* r = t->radio_data;
    int n_data = t->descript.n_radi_data;
    desc_rec* d;
    int i;
    int off;

    i = 0;
    if (r == NULL) {
       (void) ims_msg( msgDesc_save, IMS_ERROR,
         "ProcessLdr:  FDR did not have a Radiometric Data record." );
       status_save = IMS_ERROR;
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (r->seq_num !=0) {
           if (i<=n_data) {
              r=r->next;
           }
           else{
                (void) ims_msg( msgDesc_save, IMS_ERROR,
    "ProcessLdr:  Radiometric Data record has processing error." );
                 status_save = IMS_ERROR;
                 return(DECODE_ERROR);
           }
           i++;
       }
    }
    if(  Debug )  (void) printf("\n Decode Radiometric Data record\n");

    /* process the record */
    d = &(r->desc);
    d->rec_seq = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    r->seq_num = (short) get_I4( &buf[12], 4);
    r->n_data = (short) get_I4( &buf[16], 4);
    r->field_size = get_I4( &buf[20], 8);
    (void) strncpy(r->chan_ind, (char*) (buf+28), 4);
    (void) strncpy(r->table_desig, (char*) (buf+36), 24);
    r->n_samp = get_I4( &buf[60], 8);
    (void) strncpy(r->samp_type, (char*) (buf+68), 16);
    r->noise_fact = get_F4( &buf[84], 16);
    r->linear_conv_fact = get_F4( &buf[100], 16);
    r->offset_conv_fact = get_F4( &buf[116], 16);

    off = 136;
    for (i=0; i<r->n_samp; i++, off+=16) {
        r->lookup_tab[i] = get_F4( &buf[off], 16);
    }
    return(DECODE_OK);
}


int  Decode_SARL_RCR( unsigned char* buf, SARL_ptr* t )
{
    Radi_Comp* r = t->radio_comp;
    int n_data = t->descript.n_radi_comp;
    desc_rec* d;
    int i, off;
    Rad_Comp_Set *rc;
    Radio_Comp_Tbl *rt;

    i = 0;
    if (r == NULL) {
       (void) ims_msg( msgDesc_save, IMS_ERROR,
        "ProcessLdr:  FDR did not have a Radiometric Compensation rec." );
       status_save = IMS_ERROR;
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (r->seq_num !=0) {
           if (i<=n_data) {
              r=r->next;
           }
           else{
                (void) ims_msg( msgDesc_save, IMS_ERROR,
     "ProcessLdr:  Radiometric Compensation rec has processing error." );
                status_save = IMS_ERROR;
              return(DECODE_ERROR);
           }
           i++;
       }
    }
    if(  Debug  )  (void) printf(
        "\n Decode Radiometric Compensation record\n");

    /* process the record */
    d = &(r->desc);
    d->rec_seq = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    r->seq_num =  (short) get_I4( &buf[12], 4);
    r->chan_ind = (short) get_I4( &buf[16], 4);
    r->n_dset = get_I4( &buf[20], 8);
    r->dset_size = get_I4( &buf[28], 8);

    /* Allocate memory for the specified number of compensation sets */

    rc = Allocate_Comp_Sets( r );
    off=36;
    while(rc!=NULL) {
        (void) strncpy(rc->data_type, (char*) (buf+off), 8); off+=8;
        (void) strncpy(rc->data_descr, (char*) (buf+off), 32); off+=32;
        rc->req_records = (short) get_I4( &buf[off], 4); off+=4;
        rc->table_seq_num = (short) get_I4( &buf[off], 4); off+=4;
        rc->num_pairs = get_I4( &buf[off], 8); off+=8;
        rc->first_pixel = get_I4( &buf[off], 8); off+=8;
        rc->last_pixel = get_I4( &buf[off], 8); off+=8;
        rc->pixel_size = get_I4( &buf[off], 8); off+=8;
        rc->min_samp_index = get_F4( &buf[off], 16); off+=16;
        rc->min_comp_value = get_F4( &buf[off], 16); off+=16;
        rc->max_samp_index = get_F4( &buf[off], 16); off+=16;
        rc->max_comp_value = get_F4( &buf[off], 16); off+=16; off+=16;
        rc->n_table_entries = get_I4( &buf[off], 8); off+=8;

        /* Allocate memory for the specified number of compensation tables */

        rt = Allocate_Comp_Tbl( rc );
        while (rt!=NULL) {
            rt->sample_offset = get_F4( &buf[off], 16); off+=16;
            rt->sample_gain = get_F4( &buf[off], 16); off+=16;
            rt = rt->next;
        }
        rc = rc->next;
    }
    return(DECODE_OK);
}


Rad_Comp_Set* Allocate_Comp_Sets( Radi_Comp* r )
{
    Rad_Comp_Set *rc, *rc1;
    int i;

    if (r==NULL || r->n_dset<1) return(NULL);
    rc = r->set = NULL;
    for (i=0 ; i  <  r->n_dset ; i++ ){
        if ( (rc1 = (Rad_Comp_Set *) malloc (sizeof (Rad_Comp_Set)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Rad Comp Set.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(rc1, 0, sizeof( Rad_Comp_Set));
        if(  i  ==  0  )   rc = r->set = &rc1[0];
        else{
            rc->next = &rc1[0];
            rc = rc->next;
        }
        rc->next = NULL;
    }
    return( r->set );
}


Radio_Comp_Tbl* Allocate_Comp_Tbl( Rad_Comp_Set* s )
{
    Radio_Comp_Tbl *rt, *rt1;
    int i;

    if (s==NULL || s->n_table_entries<1) return(NULL);
    rt = s->tbl = NULL;
    for (i=0 ; i  <  s->n_table_entries ; i++ ){
        if ( (rt1 = (Radio_Comp_Tbl *) malloc (sizeof (Radio_Comp_Tbl)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Radio Comp Tbl.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(rt1, 0, sizeof( Radio_Comp_Tbl));
        if(  i  ==  0  )   rt = s->tbl = &rt1[0];
        else{
            rt->next = &rt1[0];
            rt = rt->next;
        }
        rt->next = NULL;
    }
    return( s->tbl );
}


int  Decode_SARL_DQSR( unsigned char* buf, SARL_ptr* t )
{
    Qual_Sum* q = t->data_qual;
    int n_data = t->descript.n_qual_sum;
    desc_rec* d;
    int i,off;

    i = 0;
    if (q == NULL) {
       (void) ims_msg( msgDesc_save, IMS_ERROR,
        "ProcessLdr:  FDR did not have a Data Quality Summary rec." );
       status_save = IMS_ERROR;
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (q->seq_num !=0) {
           if (i<=n_data) {
              q=q->next;
           }
           else{
                (void) ims_msg( msgDesc_save, IMS_ERROR,
            "ProcessLdr:  Data Quality Summary rec has processing error." );
                status_save = IMS_ERROR;
              return(DECODE_ERROR);
           }
           i++;
       }
    }
    if(  Debug )  (void) printf("\n Decode Data Quality Summary record\n");

    /* process the record */
    d = &(q->desc);
    d->rec_seq = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    q->seq_num = (short) get_I4( &buf[12], 4);
    (void) strncpy(q->sar_chn, (char*) (buf+16), 4);
    (void) strncpy(q->cali_date, (char*) (buf+20), 6);
    q->nchn = (short) get_I4( &buf[26], 4);
    q->islr = get_F4( &buf[30], 16);
    q->pslr = get_F4( &buf[46], 16);
    q->azi_ambig = get_F4( &buf[62], 16);
    q->rng_ambig = get_F4( &buf[78], 16);
    q->snr = get_F4( &buf[94], 16);
    q->ber = get_F4( &buf[110], 16);
    q->rng_res = get_F4( &buf[126], 16);
    q->azi_res = get_F4( &buf[142], 16);
    q->rad_res = get_F4( &buf[158], 16);
    q->dyn_rng = get_F4( &buf[174], 16);
    q->rad_unc_db = get_F4( &buf[190], 16);
    q->rad_unc_deg = get_F4( &buf[206], 16);

    off=222;
    for (i=0; (i<q->nchn && i<16); i++) {
        q->rad_unc[i].db = get_F4( &buf[off], 16); off+=16;
        q->rad_unc[i].deg = get_F4( &buf[off], 16); off+=16;
    }

    q->alt_locerr = get_F4( &buf[734], 16);
    q->crt_locerr = get_F4( &buf[750], 16);
    q->alt_scale = get_F4( &buf[766], 16);
    q->crt_scale = get_F4( &buf[782], 16);
    q->dis_skew = get_F4( &buf[798], 16);
    q->ori_err = get_F4( &buf[814], 16);

    off=830;
    for (i=0; (i<q->nchn && i<16); i++) {
        q->misreg[i].alt_m = get_F4( &buf[off], 16); off+=16;
        q->misreg[i].crt_m = get_F4( &buf[off], 16); off+=16;
    }

    return(DECODE_OK);
}

int  Decode_SARL_DHR( unsigned char* buf, SARL_ptr* t )
{
    Data_Hist* h = t->histogram;
    int n_data = t->descript.n_data_hist;
    desc_rec* d;
    int i, off;
    Hist_Data_Set *ht;
    long *dv;

    i = 0;
    if (h == NULL) {
       (void) ims_msg( msgDesc_save, IMS_ERROR,
        "ProcessLdr:  FDR did not have a Data Histogram rec.");
       status_save = IMS_ERROR;
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (h->seq_num !=0) {
           if (i<=n_data) {
              h=h->next;
           }
           else{
                (void) ims_msg( msgDesc_save, IMS_ERROR,
        "ProcessLdr:  Data Histogram rec has processing error.");
                status_save = IMS_ERROR;
                return(DECODE_ERROR);
           }
           i++;
       }
    }
    if(  Debug ) (void) printf("\n Decode Data Histogram record\n");

    /* process the record */
    d = &(h->desc);
    d->rec_seq = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    h->seq_num =  (short) get_I4( &buf[12], 4);
    h->sar_chn = (short) get_I4( &buf[16], 4);
    h->ntab = get_I4( &buf[20], 8);
    h->ltab = get_I4( &buf[28], 8);


    /* Allocate memory for the stated number of histogram table sets */
    ht = Allocate_Hist_Data_Set( h );
    off = 36;
    while (ht!=NULL) {
        (void) strncpy(ht->hist_desc, (char*) (buf+off), 32); off+=32;
        ht->nrec = (short) get_I4( &buf[off], 4); off+=4;
        ht->tab_seq = (short) get_I4( &buf[off], 4); off+=4;
        ht->nbin = get_I4( &buf[off], 8); off+=8;
        ht->ns_lin = get_I4( &buf[off], 8); off+=8;
        ht->ns_pix = get_I4( &buf[off], 8); off+=8;
        ht->ngrp_lin = get_I4( &buf[off], 8); off+=8;
        ht->ngrp_pix = get_I4( &buf[off], 8); off+=8;
        ht->nsamp_lin = get_I4( &buf[off], 8); off+=8;
        ht->nsamp_pix = get_I4( &buf[off], 8); off+=8;
        ht->min_smp = get_F4( &buf[off], 16); off+=16;
        ht->max_smp = get_F4( &buf[off], 16); off+=16;
        ht->mean_smp = get_F4( &buf[off], 16); off+=16;
        ht->std_smp = get_F4( &buf[off], 16); off+=16;
        ht->smp_inc = get_F4( &buf[off], 16); off+=16;
        ht->min_hist = get_F4( &buf[off], 16); off+=16;
        ht->max_hist = get_F4( &buf[off], 16); off+=16;
        ht->mean_hist = get_F4( &buf[off], 16); off+=16;
        ht->std_hist = get_F4( &buf[off], 16); off+=16;
        ht->nhist = get_I4( &buf[off], 8); off+=8;

        dv = Allocate_DH_table( ht );
        for (i=0; i<ht->nhist; i++) {
            *(dv+i) = get_I4( &buf[off], 8); off+=8;
        }
        ht=ht->next;
    }
    return(DECODE_OK);
}

long* Allocate_DH_table( Hist_Data_Set* ht) {
    int  val;
    if (ht==NULL || ht->nhist < 1) return(NULL);
    val = ht->nhist*sizeof (long);
    if ( (ht->data_values = (long *) malloc (val) )
       == NULL ) {
       (void) ims_msg( msgDesc_save, IMS_ERROR,
            "ProcessLdr:  Fail allocating  Histogram Table.");
        status_save = IMS_ERROR;
       return(NULL);
    }
    (void) memset(ht->data_values,0,ht->nhist*sizeof (long));
    return (ht->data_values);
}

Hist_Data_Set* Allocate_Hist_Data_Set( Data_Hist* h )
{
    Hist_Data_Set *ht,*ht1;
    int i;

    if (h==NULL || h->ntab < 1) return(NULL);
    ht = h->data_set = NULL;
    for (i=0 ; i  <  h->ntab ; i++ ){
        if ( (ht1 = (Hist_Data_Set *) malloc (sizeof (Hist_Data_Set)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Hist Data Set.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(ht1, 0, sizeof( Hist_Data_Set));
        if(  i  ==  0  )   ht = h->data_set = &ht1[0];
        else{
            ht->next = &ht1[0];
            ht = ht->next;
        }
        ht->next = NULL;
    }
    return(h->data_set);
}

int  Decode_SARL_RSR( unsigned char* buf, SARL_ptr* t )
{
    Rng_Spec* r = t->spectra;
    int n_data = t->descript.n_rang_spec;
    desc_rec* d;
    int i, off;

    i = 0;
    if (r == NULL) {
       (void) ims_msg( msgDesc_save, IMS_ERROR,
            "ProcessLdr:  FDR did not have a Range Spectra rec.");
       status_save = IMS_ERROR;
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (r->seq_num !=0) {
           if (i<=n_data) {
              r=r->next;
           }
           else{
                (void) ims_msg( msgDesc_save, IMS_ERROR,
            "ProcessLdr:  Range Spectra rec has processing error.");
                status_save = IMS_ERROR;
                return(DECODE_ERROR);
           }
           i++;
       }
    }
    if(  Debug )  (void) printf("\n Decode Range Spectra record\n");

    /* process the record */
    d = &(r->desc);
    d->rec_seq = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    r->seq_num = (short) get_I4( &buf[12], 4);
    r->sar_chn = (short) get_I4( &buf[16], 4);
    r->n_dset = get_I4( &buf[20], 8);
    r->dset_size = get_I4( &buf[28], 8);
    r->req_recs = (short) get_I4( &buf[36], 4);
    r->table_no = (short) get_I4( &buf[40], 4);
    r->n_pixels = get_I4( &buf[44], 8);
    r->pixel_offset = get_I4( &buf[52], 8);
    r->n_lines = get_I4( &buf[60], 8);
    r->first_freq = get_F4( &buf[68], 16);
    r->last_freq = get_F4( &buf[84], 16);
    r->min_power = get_F4( &buf[100], 16);
    r->max_power = get_F4( &buf[116], 16);
    r->n_bins = get_I4( &buf[164], 8);

    off=172;
    for (i=0; i<r->n_bins; i++, off +=16)
        r->data_values[i] = get_F4( &buf[off], 16);

    return(DECODE_OK);
}

int  Decode_SARL_DEM( unsigned char* buf, SARL_ptr* t )
{
    Digital_Elev* e = t->elevation;
    int n_data = t->descript.n_dem_desc;
    desc_rec* d;
    Dem_Desc *set;
    Corner_Pts *pts;
    int i, off;

    i = 0;
    if (e == NULL) {
       (void) ims_msg( msgDesc_save, IMS_ERROR,
            "ProcessLdr:  FDR did not have a Digital Elevation rec.");
       status_save = IMS_ERROR;
       return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (e->seq_num !=0) {
           if (i<=n_data) {
              e=e->next;
           }
           else{
                (void) ims_msg( msgDesc_save, IMS_ERROR,
            "ProcessLdr:  Digital Elevation rec has processing error.");
                status_save = IMS_ERROR;
               return(DECODE_ERROR);
           }
           i++;
       }
    }
    if(  Debug )  (void) printf("\n Decode Digital Elevation record\n");

    /* process the record */
    d = &(e->desc);
    d->rec_seq = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    e->seq_num = (short) get_I4( &buf[12], 4);
    e->ttl_num_sets = get_I4( &buf[16], 8);
    e->DEM_seq_num = (short) get_I4( &buf[24], 4);
    (void) strncpy(e->source, (char *) (buf+28), 32);
    (void) strncpy(e->HT_ref_name, (char *) (buf+60), 32);
    (void) strncpy(e->gen_method, (char *) (buf+92), 32);
    (void) strncpy(e->raster_unit, (char *) (buf+124), 12);
    (void) strncpy(e->presentation_proj, (char *) (buf+136), 32);
    e->NS_raster = get_F4( &buf[168], 16);
    e->EW_raster = get_F4( &buf[184], 16);
    (void) strncpy(e->resample, (char *) (buf+200), 32);
    e->height_err = get_F4( &buf[232], 16);
    e->NS_loc_err = get_F4( &buf[248], 16);
    e->EW_loc_err = get_F4( &buf[264], 16);
    e->max_height = get_F4( &buf[280], 16);
    e->min_height = get_F4( &buf[296], 16);
    e->MEAN_height = get_F4( &buf[312], 16);
    e->STD_height = get_F4( &buf[328], 16);
    e->num_polys = (short) get_I4( &buf[344], 4);

    off = 348;
    set= Allocate_DEM_sets( e );
    while (set!=NULL) {
        set->poly_seq_num = (short) get_I4( &buf[off], 4 ); off+=4;
        set->num_crnr_pts = (short) get_I4( &buf[off], 4 ); off+=4;
        (void) strcpy(set->spare1, "        ");  off+=8;

        pts = Allocate_DEM_pts( set );
        while (pts!=NULL) {
              pts->latitude = get_F4( &buf[off], 16 ); off+=16;
              pts->longitude = get_F4( &buf[off], 16 ); off+=16;
              pts=pts->next;
        }
        set=set->next;
    }
    return(DECODE_OK);
}

Dem_Desc*  Allocate_DEM_sets( Digital_Elev* e )
{
    Dem_Desc *s,*s1;
    int i;

    if (e==NULL || e->num_polys < 1) return(NULL);
    s = e->set = NULL;
    for (i=0 ; i  <  e->num_polys ; i++ ){
        if ( (s1 = (Dem_Desc *) malloc (sizeof (Dem_Desc)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Dem Desc.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(s1, 0, sizeof( Dem_Desc));
        if(  i  ==  0  )   s = e->set = &s1[0];
        else{
            s->next = &s1[0];
            s = s->next;
        }
        s->next = NULL;
    }
    return(e->set);
}

Corner_Pts* Allocate_DEM_pts( Dem_Desc* set )
{
    Corner_Pts *p,*p1;
    int i;

    if (set==NULL || set->num_crnr_pts<1) return(NULL);
    p = set->pts = NULL;
    for (i=0 ; i  <  set->num_crnr_pts ; i++ ){
        if ( (p1 = (Corner_Pts *) malloc (sizeof (Corner_Pts)) )
           == NULL ) {
           (void) ims_msg( msgDesc_save, IMS_ERROR,
                "ProcessLdr:  Fail allocating  Corner Pts.");
            status_save = IMS_ERROR;
           return(NULL);
        }
        (void) memset(p1, 0, sizeof( Corner_Pts));
        if(  i  ==  0  )   p = set->pts = &p1[0];
        else{

            p->next = &p1[0];
            p = p->next;
        }
        p->next = NULL;
    }
    return(set->pts);
}

int  Decode_SARL_FRR( unsigned char* buf, SARL_ptr* t )
{
    Fac_Related* r = t->facility;
    int n_data = t->descript.n_faci_data;
    desc_rec* d;
    int i;

    i = 0;
    if (r == NULL) {
        (void) ims_msg( msgDesc_save, IMS_ERROR,
            "ProcessLdr:  FDR did not have a Facility Related rec.");
        status_save = IMS_ERROR;
        return(DECODE_ERROR);
    }
    if (n_data > 1) {
       /* determine the last record filled */
       while (r->seq_num !=0) {
           if (i<=n_data) {
              r=r->next;
           }
           else{
                (void) ims_msg( msgDesc_save, IMS_ERROR,
            "ProcessLdr:  Facility Related rec has processing error.");
                status_save = IMS_ERROR;
              return(DECODE_ERROR);
            }
           i++;
       }
    }
    if(  Debug )  (void) printf("\n Decode Facility Related record\n");

    /* process the record */
    d = &(r->desc);
    d->rec_seq = Check_Rec_Seq(&i, buf, t, 1);
    d->rec_sub1 = *(buf+4);
    d->rec_type = *(buf+5);
    d->rec_sub2 = *(buf+6);
    d->rec_sub3 = *(buf+7);

    if (d->rec_type != LFTYPE) {
       /* for now take the ice motion , ice class, wave spectra and stuff
        it into a buffer */
       i = cvt2int(&buf[8]) -12;
       r->bogus = (char *) malloc( i*sizeof(char) );
       (void) strncpy(r->bogus, (char *) (buf+12), i);
    }
    else {
       r->seq_num = (short) get_I4( &buf[12], 4);
       (void) strncpy(r->datatake_ID, (char*) (buf+20), 14);
       (void) strncpy(r->image_ID, (char*) (buf+34), 11);
       (void) strncpy(r->corr_year, (char*) (buf+45), 4);
       (void) strncpy(r->corr_GMT, (char*) (buf+50), 17);
       (void) strncpy(r->site_name, (char*) (buf+67), 33);
       (void) strncpy(r->data_year, (char*) (buf+100), 4);
       (void) strncpy(r->center_GMT, (char*) (buf+105), 17);
       r->center_LAT = get_F4( &buf[122], 17);
       r->center_LON = get_F4( &buf[139], 17);
       r->near_sta_LAT = get_F4( &buf[156], 17);
       r->near_sta_LON = get_F4( &buf[173], 17);
       r->near_end_LAT = get_F4( &buf[190], 17);
       r->near_end_LON = get_F4( &buf[207], 17);
       r->far_sta_LAT = get_F4( &buf[224], 17);
       r->far_sta_LON = get_F4( &buf[241], 17);
       r->far_end_LAT = get_F4( &buf[258], 17);
       r->far_end_LON = get_F4( &buf[275], 17);
       r->actual_azimuth = get_F4( &buf[292], 17);
       r->actual_range = get_F4( &buf[309], 17);
       r->actual_pixels = get_I4( &buf[326], 9);
       r->actual_lines = get_I4( &buf[335], 9);
       r->total_pixels = get_I4( &buf[344], 9);
       r->total_lines = get_I4( &buf[353], 9);
       (void) strncpy(r->media_ID, (char*) (buf+362), 7);
       r->start_block = get_I4( &buf[369], 17);
       r->end_block = get_I4( &buf[386], 17);
       (void) strncpy(r->platform_name, (char*) (buf+403), 17);
       (void) strncpy(r->sensor_mode, (char*) (buf+420), 33);
       r->PRF = get_F4( &buf[453], 17);
       r->ant_look_angle = get_F4( &buf[470], 17);
       r->data_rate = get_F4( &buf[487], 17);
       r->data_win_pos = get_F4( &buf[504], 17);
       r->range_gate_del = get_F4( &buf[521], 17);
       r->track_angle = get_F4( &buf[538], 17);
       (void) strncpy(r->ASC_DESC, (char*) (buf+555), 1);
       r->S_C_altitude = get_F4( &buf[556], 17);
       r->X_pos = (double) get_F4( &buf[573], 23);
       r->Y_pos = (double) get_F4( &buf[596], 23);
       r->Z_pos = (double) get_F4( &buf[619], 23);
       r->X_vel = (double) get_F4( &buf[642], 23);
       r->Y_vel = (double) get_F4( &buf[665], 23);
       r->Z_vel = (double) get_F4( &buf[688], 23);
       r->roll = get_F4( &buf[711], 15);
       r->yaw = get_F4( &buf[726], 15);
       r->pitch = get_F4( &buf[741], 15);
       r->roll_flag = get_I4( &buf[756], 5);
       r->yaw_flag = get_I4( &buf[761], 5);
       r->pitch_flag = get_I4( &buf[766], 5);
       r->roll_rate = get_F4( &buf[771], 15);
       r->yaw_rate = get_F4( &buf[786], 15);
       r->pitch_rate = get_F4( &buf[801], 15);
       r->roll_rate_flag = get_I4( &buf[816], 5);
       r->yaw_rate_flag = get_I4( &buf[821], 5);
       r->pitch_rate_flag = get_I4( &buf[826], 5);
       r->nadir_radius = get_F4( &buf[831], 17);
       r->image_radius = get_F4( &buf[848], 17);
       r->incidence_angle = get_F4( &buf[865], 17);
       (void) strncpy(r->proc_version, (char*) (buf+882), 8);
       (void) strncpy(r->proc_type, (char*) (buf+890), 3);
       r->type_ephemeris = get_I4( &buf[893], 2);
       r->looks_azimuth = get_F4( &buf[895], 17);
       r->looks_range = get_F4( &buf[912], 17);
       r->WPH_azimuth = get_F4( &buf[929], 17);
       r->WPH_range = get_F4( &buf[946], 17);
       (void) strncpy(r->look_energy_eq, (char*) (buf+963), 4);
       r->induced_azimuth = get_F4( &buf[967], 17);
       r->induced_range = get_F4( &buf[984], 17);
       r->gain = get_F4( &buf[1001], 17);
       r->swath_velocity = get_F4( &buf[1018], 17);
       r->squint_angle = get_F4( &buf[1035], 17);
       r->avg_ter_height = get_F4( &buf[1052], 17);
       r->proc_gain = get_I4( &buf[1069], 4);
       (void) strncpy(r->deskew_flag, (char*) (buf+1073), 4);
       (void) strncpy(r->gnd_slant_flag, (char*) (buf+1077), 7);
       r->sl_rng_1st_pix = get_F4( &buf[1084], 17);
       r->sl_rng_last_pix = get_F4( &buf[1101], 17);
       r->start_sample = get_I4( &buf[1118], 9);
       (void) strncpy(r->clutterlock_flg, (char*) (buf+1127), 4);
       r->dop_frq_const = get_F4( &buf[1131], 17);
       r->dop_frq_slope = get_F4( &buf[1148], 17);
       r->dop_frq_quad = get_F4( &buf[1165], 17);
       (void) strncpy(r->autofocus_flag, (char*) (buf+1182), 4);
       r->dop_frq_r_cnst = get_F4( &buf[1186], 17);
       r->dop_frq_r_slope = get_F4( &buf[1203], 17);
       r->dop_frq_r_quad = get_F4( &buf[1220], 17);
       r->resol_azimuth = get_F4( &buf[1237], 17);
       r->resol_range = get_F4( &buf[1254], 17);
       r->azimuth_pixel = get_F4( &buf[1271], 17);
       r->range_pixel = get_F4( &buf[1288], 17);
       (void) strncpy(r->OBRC_flag, (char*) (buf+1305), 4);
       r->bits_sample = get_I4( &buf[1309], 5);
       r->calib_est = get_F4( &buf[1314], 17);
       r->bit_err_rate = get_F4( &buf[1331], 17);
       r->SNR = get_F4( &buf[1348], 17);
       r->est_noise_flr = get_F4( &buf[1365], 17);
       r->radio_m_resol = get_F4( &buf[1382], 17);
       r->satur_points = get_I4( &buf[1399], 9);
       (void) strncpy(r->spec_flag, (char*) (buf+1408), 4);
    }
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


void  print_nice( array )
char *  array; /* large array - > 80 chars  */
/* *******************************************
    subr print_nice prints a large array in a nice mannor: splitting
        it into lines with the no. of the first char at the start of
        each line   */
{
char  str[100];
long  i,j,k,l;
unsigned char flag,flag2;


i = strlen( array );
(void) printf( "\n\n  *****  large array:  length = %ld  (# is last char)\n\n",
    i );
l = -1;
flag = 0x11;
while( flag ){
    /*  start at 70 chars from start - find nearest blank  */
    flag2 = 0x11;
    k = l+70+1;
    if(  k  >=  i  ){ /* go to end of string */
        k = i;
        j = i;
        flag2 = 0x00;
    }
    for( j=k ; j  >  l+50 &&  flag2 ; j-- )
        if(  array[j]  ==  ' ' )  flag2 = 0x00;

    (void) strncpy( str, array+l+1, 70 );
    str[j-l] = '\0';
    (void) strcat( str, "#" );
    if(  j  >=  i  ){ /* last string - mark end  */
        flag = 0x00;
    }
    (void) printf( " at %3ld: %s\n", l+2, str );
    l = j;
}

return;
}       /*  print_nice  */
