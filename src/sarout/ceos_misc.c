#include "asf.h"
#include "math.h"
#include "sarout.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "ifm.h"

double getSRfromGR(stateVector,double,double,double);
int init_hdr(int type, int size, struct HEADER *h);
int inc_hdr(struct HEADER *bf);
int init_rhdr(int type,ceosLeader *leader,int ns,meta_parameters *meta,struct RHEADER *r);
int inc_rhdr(int type, meta_parameters *meta, int ns, struct RHEADER *bf);
void scan_line(unsigned char *buf,int ns, int *lc, int *dc, int *rc);
void change_rhdr(struct RHEADER *r,int lc,int dc,int rc);
void fill_fdr_common(struct FDR *v);
void fill_iof_vfdr(int mode,struct IOF_VFDR *v,int nl,int ns,int nbytes,char *filename);
int get_hist_stats(struct hist_dset *d, int nl, int ns);

/*---------------------------------------------------------------
        ROUTINES TO PROCESS STANDARD 12 BYTE LEADER
 ---------------------------------------------------------------*/
/* Pack 12 byte header based on record type */
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
   default:     printf("Can't find type %i\n",type); exit(1);
  }
  bigInt32_out(actual_size,&(h->recsiz[0]));
  return(0);
}

/* Increment header record counter */ 
int inc_hdr(struct HEADER *bf)
{
   int tmp;
   tmp = bigInt32(bf->recnum);
   tmp++;
   bigInt32_out(tmp,&(bf->recnum[0]));
   return(0);
}

/*---------------------------------------------------------------
   ROUTINES TO PROCESS STANDARD 180 BYTE PROCESSED DATA LEADER
 ---------------------------------------------------------------*/
int init_rhdr(int type, ceosLeader *leader, int ns, 
	  meta_parameters *meta, struct RHEADER *r)
{
  double lat,lon;

  r->line_num      = 1;
  r->rec_num       = 1;
  r->n_left_pixel  = 0;
  r->n_data_pixel  = ns;
  r->n_right_pixel = 0;
  r->sensor_updf   = 0;
  r->acq_year      = leader->ppdr.year;
  r->acq_day       = leader->ppdr.gmt_day;
  r->acq_msec      = (int) (leader->ppdr.gmt_sec*1000.0);
  r->sar_cib       = 1;
  r->sar_chan_code = 2;
  r->tran_polar    = 0;
  r->recv_polar    = 0;
  r->prf_bin       = leader->facdr.prfreq;
  r->spare_1       = 0;
  r->sr_first      = leader->facdr.sltrngfp;
  r->sr_last       = leader->facdr.sltrnglp;

  if (type == CEOS_SLC || type == CEOS_CCSD)
   {
     r->sr_mid = (r->sr_first+r->sr_last)/2.0; 
   }
  else
   {
     stateVector tVec;

     tVec.pos.x = leader->facdr.scxpos;
     tVec.pos.y = leader->facdr.scypos;
     tVec.pos.z = leader->facdr.sczpos;
     tVec.vel.x = leader->facdr.scxvel;
     tVec.vel.y = leader->facdr.scyvel;
     tVec.vel.z = leader->facdr.sczvel;

     r->sr_mid = getSRfromGR(tVec,leader->facdr.sltrngfp,
		 leader->facdr.eradcntr,leader->facdr.swrange/2.0);
   }

  r->fdc_first = 0.0; r->fdc_mid = 0.0; r->fdc_last = 0.0;
  r->ka_first  = 0.0; r->ka_mid  = 0.0; r->ka_last  = 0.0;
  r->nadir_ang = 0.0; r->squint_ang = 0.0;
  r->null_f      = 0;
  r->spare_2_1   = 0; r->spare_2_2 = 0;
  r->spare_2_3   = 0; r->spare_2_4 = 0;
  r->geo_updf    = 1;

  if (type != CEOS_CCSD)
   { 
    meta_get_latLon(meta, 0.0, 0.0, 0.0, &lat, &lon);
    r->lat_first = (int) (lat*1000000.0);
    r->long_first = (int) (lon*1000000.0);

    meta_get_latLon(meta,0.0,ns/2.0,0,&lat,&lon);
    r->lat_mid = (int) (lat*1000000.0);
    r->long_mid = (int) (lon*1000000.0);

    meta_get_latLon(meta,0.0,ns,0.0,&lat,&lon);
    r->lat_last = (int) (lat*1000000.0);
    r->long_last = (int) (lon*1000000.0);
   }
  else
   {
    r->lat_first = 0;
    r->lat_mid   = 0;
    r->lat_last  = 0;
    r->long_first = 0;
    r->long_mid   = 0;
    r->long_last  = 0;
   }

  r->north_first = 0;
  r->spare_3  = 0; 
  r->north_last = 0;
  r->east_first  = 0;
  r->spare_4  = 0;
  r->east_last  = 0;
  r->heading     = 0;

/* The following line causes a bus error on IRIX systems????

  r->spare_5  = 0.0;

*/
  return(0);
}

int inc_rhdr(int type, meta_parameters *meta, int ns, struct RHEADER *bf)
{
  double lat,lon;

  if (type != CEOS_CCSD)
   {
    meta_get_latLon(meta,bf->line_num,0.0,0.0,&lat,&lon);
    bf->lat_first = (int) (lat*1000000.0);
    bf->long_first = (int) (lon*1000000.0);

    meta_get_latLon(meta,bf->line_num,ns/2.0,0.0,&lat,&lon);
    bf->lat_mid = (int) (lat*1000000.0);
    bf->long_mid = (int) (lon*1000000.0);

    meta_get_latLon(meta,bf->line_num,(double)ns,0.0,&lat,&lon);
    bf->lat_last = (int) (lat*1000000.0);
    bf->long_last = (int) (lon*1000000.0);
   }

  bf->line_num++;
  return(0);
}

/* Scans a line of data, counting the left fill, data, and right fill pixels
 --------------------------------------------------------------------------*/
void scan_line(unsigned char *buf,int ns, int *lc, int *dc, int *rc)
{
    int i=0;
    int left=1,data=0,right=0;

    *lc = ns;
    *rc = 0;
    *dc = 0;

    for (i=0; i<ns; i++)
     {
        if (left && buf[i]!=0) { *lc = i; left = 0; data = 1; }
        else if (data && buf[i]==0) { *dc = i - *lc; data = 0; right = 1; }
        else if (right && buf[i]!=0) { right = 0; data = 1; }
     }
    *rc = ns - (*lc + *dc);
}

void change_rhdr(struct RHEADER *r,int lc,int dc,int rc)
{
  r->n_left_pixel = lc;
  r->n_data_pixel = dc;
  r->n_right_pixel = rc;
}

/*********************************************************************/
void fill_fdr_common(struct FDR *v)
{
 strcpy(v->ascii_flag,"A "); strcpy(v->spare1,"  ");
 strcpy(v->format_doc,"CEOS-SAR-CCT");
 strcpy(v->format_rev," B"); strcpy(v->design_rev," B");
 strcpy(v->software_id,"SAROUT v1.0   ");
 strcpy(v->rec_seq_flag,"FSEQ"); v->seq_loc = 1;  v->seq_len = 4;
 strcpy(v->rec_code,"FTYP");     v->code_loc = 5; v->code_len = 4;
 strcpy(v->rec_len,"FLGT");      v->rlen_loc = 9; v->rlen_len = 4;
 strcpy(v->spare2,"    ");
 strcpy(v->spare3,
    "                                                                ");
}

void fill_iof_vfdr(int mode,struct IOF_VFDR *v,int nl,int ns,int nbytes, char *filename)
{
 fill_fdr_common((struct FDR *)v);

 v->file_num = 2;
 strcpy(v->product_id,filename);
 v->reclen      = nbytes;
 v->justific[0] = '\0';
 v->sarchan     = 1;
 v->lbrdrpxl = v->rbrdrpxl = v->topbrdr = v->botbrdr = 0;
 strcpy(v->interlv,"BSQ");
 v->recline     = 1;
 v->mrecline    = 1;
 v->predata     = H_SZ + R_SZ;
 v->sardata     = nbytes - (H_SZ+R_SZ);
 v->sufdata     = 0;
 v->repflag[0]  = '\0';
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
        strcpy(v->formatid,"UNSIGNED INTERGER*1");
        strcpy(v->formcode,"UI1");
        break;
    default: fprintf(stderr,"Error: Unknown image type %i\n",mode); exit(1);
 }
}

int get_hist_stats(struct hist_dset *d, int nl, int ns)
{
  double tot = 0.0;
  double sumsq = 0.0;
  long sum_hist = 0;
  long max_hist = 0, min_hist = GILLION;
  long samp, val;
  double dsum, dtot;
  double vari;

  for (samp = 0; samp < d->nbin; samp++)
   {
     val = d->data_values_hist[samp];
     tot += (double)(val*samp*(d->smp_inc));
     max_hist = DMAX(val,max_hist);
     min_hist = DMIN(val,min_hist);
     sum_hist += val;
     sumsq += (double)(SQR(val));
   }

  dsum = (double) tot;
  dtot = (double) (ns*nl);
  d->mean_smp = dsum / dtot;

  d->min_hist = (double) min_hist;
  d->max_hist = (double) max_hist;
  dsum = (double) sum_hist;
  dtot = (double) d->nbin;
  d->mean_hist = dsum / dtot;
  vari = (sumsq-(dsum*dsum)/dtot)/(dtot);
  d->std_hist = sqrt(vari);
  return(0);
}
