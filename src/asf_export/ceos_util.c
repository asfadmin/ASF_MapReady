#include "asf.h"
#include "ceos.h"
#include "asf_endian.h"
#include "asf_reporting.h"

/* Copied this function unchecked from writeAsfCeosData.c as part of 'sarout' */
void fill_fdr_common(struct FDR *v)
{
  strcpy(v->ascii_flag,"A "); strcpy(v->spare1,"  ");
  strcpy(v->format_doc,"CEOS-SAR-CCT");
  strcpy(v->format_rev," B"); strcpy(v->design_rev," B");
  strcpy(v->software_id,"ASF_EXPORT  ");
  strcpy(v->rec_seq_flag,"FSEQ"); v->seq_loc = 1;  v->seq_len = 4;
  strcpy(v->rec_code,"FTYP");     v->code_loc = 5; v->code_len = 4;
  strcpy(v->rec_len,"FLGT");      v->rlen_loc = 9; v->rlen_len = 4;
  strcpy(v->spare2,"    ");
  strcpy(v->spare3,
         "                                                                ");
}

/* This function comes straight out of writeAsfCeosData.c as part of 'sarout'.
   It looks quite dusty when it comes to the data types to serve as general
   export CEOS tool. At least the nominclature needs to be looked at */
void fill_iof_vfdr(int mode, struct IOF_VFDR *v, int nl, int ns, int nbytes)
{
  fill_fdr_common((struct FDR *)v);

  v->file_num = 2;
  strcpy(v->product_id, "ICE LUT test");
  v->reclen      = nbytes;
  strcpy(v->spare4,"                        ");
  v->justific[0] = '\0';
  v->sarchan     = 1;
  v->lbrdrpxl = v->rbrdrpxl = v->topbrdr = v->botbrdr = 0;
  strcpy(v->interlv,"BSQ");
  v->recline     = 1;
  v->mrecline    = 1;
  v->predata     = H_SZ + R_SZ;
  v->sardata     = nbytes - (H_SZ+R_SZ);
  v->sufdata     = 0;
  strcpy(v->repflag,"    ");
  strcpy(v->lin_loc,  "  1354PB");
  strcpy(v->chn_loc,  "  4952PB");
  strcpy(v->time_loc, "  4554PB");
  strcpy(v->left_loc, "  21 4PB");
  strcpy(v->right_loc,"  29 4PB");
  strcpy(v->pad_ind,"     ");
  strcpy(v->spare6,"                            ");
  strcpy(v->qual_loc,"        ");
  strcpy(v->cali_loc,"        ");
  strcpy(v->gain_loc,"        ");
  strcpy(v->bais_loc,"        ");
  v->leftfill = v->rigtfill = 0;

  switch(mode) {
    case CEOS_CCSD:
      v->numofrec     = nl;
      v->sampdata     = 2;
      v->bytgroup     = 2;
      v->bitssamp     = 8;
      v->linedata     = nl;
      v->datgroup     = ns;
      v->maxidata     = 255;
      strcpy(v->formatid,"COMPLEX INTEGER*2");
      strcpy(v->formcode,"CI*2");
      break;
    case CEOS_SLC:
      v->numofrec     = nl;
      v->sampdata     = 2;
      v->bytgroup     = 4;
      v->bitssamp     = 8 * sizeof(short int);
      v->linedata     = nl;
      v->datgroup     = ns;
      v->maxidata     = SLC_AVG - 1;
      strcpy(v->formatid,"COMPLEX INTEGER*4");
      strcpy(v->formcode,"CI*4");
      break;
    case CEOS_LOW:
      v->numofrec     = nl;
      v->bitssamp     = 8;
      v->sampdata     = 1;
      v->bytgroup     = 1;
      v->linedata     = nl;
      v->datgroup     = ns;
      v->maxidata     = 255;
      strcpy(v->formatid,"UNSIGNED INTERGER1");
      strcpy(v->formcode,"IU1");
      break;
    default: asfPrintError("Unknown image type %i\n",mode);
  }
}

int init_hdr(int type, int size, struct HEADER *h)
{
  static int nxt_num = 1;
  int actual_size;

  actual_size = size;
  h->rectyp[0] = '\12';
  h->rectyp[2] = '\22';
  h->rectyp[3] = '\24';

  if (type != CEOS_CCSD && type != CEOS_SLC &&
      type != CEOS_FUL  && type != CEOS_LOW && type != IOFDR) nxt_num++;

  bigInt32_out(nxt_num,&(h->recnum[0]));

  switch (type) {
   case CEOS_CCSD:
   case CEOS_SLC:
   case CEOS_FUL:
   case CEOS_LOW:
                bigInt32_out(2,&(h->recnum[0]));
                h->rectyp[0]= '2'; h->rectyp[1]='\13'; break;
   case IOFDR:  h->rectyp[0]='?'; h->rectyp[1]='\300';h->rectyp[3]='\22';break;
   case DSSR:   h->rectyp[1]='\12'; actual_size=4096; break;
   case MPDR:   h->rectyp[1]='\24'; actual_size=1620; break;
   case PPDR:   h->rectyp[1]='\36'; actual_size=1024; break;
   case ATDR:   h->rectyp[1]='\50'; actual_size=1024; break;
   case RADR:   h->rectyp[1]='\62'; actual_size=4232; break;
   case DQSR:   h->rectyp[1]='\74'; actual_size=1620; break;
   case SDHR:   h->rectyp[1]='\106';actual_size=4628; break;
   case PDHR:   h->rectyp[1]='\106';actual_size=4628; break;
   case RNSR:   h->rectyp[1]='\120';actual_size=5120; break;
   case DEMR:   h->rectyp[1]='\132';actual_size=1024; break;
   case FACDR:  h->rectyp[0]='Z';h->rectyp[1]='\322';
                h->rectyp[3]='\75';actual_size=1717; break;
   default:     asfPrintError("Cannot find type %i.\n",type);
  }
  bigInt32_out(actual_size,&(h->recsiz[0]));
  return(0);
}

int inc_hdr(struct HEADER *bf)
{
  int tmp;
  tmp = bigInt32(bf->recnum);
  tmp++;
  bigInt32_out(tmp,&(bf->recnum[0]));
  return(0);
}

/* Adjusted the original function to get it to basically work. We want to revisit
   these functions when we go over the entire CEOS related stuff */
int init_rhdr(int ns, meta_parameters *meta, struct RHEADER *r)
{
  r->line_num = 1;
  r->rec_num = 1;
  r->n_left_pixel = 0;
  r->n_data_pixel = ns;
  r->n_right_pixel = 0;
  r->sensor_updf = 1;
  r->acq_year = 2000;
  r->acq_day = 1;
  r->acq_msec = 1;
  r->sar_cib = 1;
  r->sar_chan_code = 2;
  r->tran_polar = 0;
  r->recv_polar = 0;
  r->prf_bin = 0;
  r->spare_1 = 0;
  r->sr_first = 0;
  r->sr_last = 0;
  r->sr_mid = 0;
  r->fdc_first = 0.0;
  r->fdc_mid = 0.0;
  r->fdc_last = 0.0;
  r->ka_first = 0.0;
  r->ka_mid = 0.0;
  r->ka_last = 0.0;
  r->nadir_ang = 0.0;
  r->squint_ang = 0.0;
  r->null_f = 0;
  r->spare_2_1 = 0;
  r->spare_2_2 = 0;
  r->spare_2_3 = 0;
  r->spare_2_4 = 0;
  r->geo_updf = 1;
  r->lat_first = 0;
  r->lat_mid = 0;
  r->lat_last = 0;
  r->long_first = 0;
  r->long_mid = 0;
  r->long_last = 0;
  r->north_first = 0;
  r->spare_3 = 0;
  r->north_last = 0;
  r->east_first = 0;
  r->spare_4 = 0;
  r->east_last = 0;
  r->heading = 0;

 /* The following line causes a bus error on IRIX systems????*/

  r->spare_5  = 0;

  return(0);
}

int inc_rhdr(meta_parameters *meta, int ns, struct RHEADER *bf)
{
  /*
  double lat,lon;

  meta_get_latLon(meta, bf->line_num, 0.0, 0.0, &lat, &lon);
  bf->lat_first = (int) (lat*1000000.0);
  bf->long_first = (int) (lon*1000000.0);

  meta_get_latLon(meta, bf->line_num,ns/2.0, 0.0, &lat, &lon);
  bf->lat_mid = (int) (lat*1000000.0);
  bf->long_mid = (int) (lon*1000000.0);

  meta_get_latLon(meta, bf->line_num, (double)ns, 0.0, &lat, &lon);
  bf->lat_last = (int) (lat*1000000.0);
  bf->long_last = (int) (lon*1000000.0);
  */

  bf->line_num++;
  return(0);
}
