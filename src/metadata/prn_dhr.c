/********************************************************************
NAME:     prn_dhr.c --  print data histograms record values

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0            4/96   T. Logan (ASF)   Borrowed from JPL ceos reader
  1.1		 9/96   T. Logan (ASF)   Modified to write multiple tables

*********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

char *sprn_dhr(struct data_hist_rec* h)
{
 int i,j=1;
 struct hist_dset *d;

 char *ret = MALLOC(sizeof(char)*1);
 strcpy(ret, "");

 add(&ret, "\n************** begin of Data Histogram record ****************\n");
 add(&ret, "\n DATA HISTOGRAM SEQ NUM\t\t%d",h->seq_num);
 add(&ret, "\n SAR CHANNEL ID\t\t\t%d",h->sar_chan);
 add(&ret, "\n NUM HISTOGRAM DATA SETS\t%d",h->ntab);
 add(&ret, "\n TABLE DATA SET SIZE\t\t%d",h->ltab);
 d = h->data; 

 while (d != NULL)
 {
  add(&ret, "\n\n DATA SET NUM\t\t\t%d",j++);
  add(&ret, "\n HISTOGRAM DESCRIPTOR\t\t%s",d->hist_desc);
  add(&ret, "\n RECS NEEDED FOR FULL TABLE\t%d",d->nrec);
  add(&ret, "\n TBL REC SEQ NUM\t\t%d",d->tab_seq);
  add(&ret, "\n NUM OF TBL BINS\t\t%d",d->nbin);
  add(&ret, "\n NUM SAMPLES IN LINE DIR\t%d",d->ns_lin);
  add(&ret, "\n NUM SAMPLES ACROSS LINE DIR\t%d",d->ns_pix);
  add(&ret, "\n SAMPLE GRP SIZE IN LINE DIR\t%d",d->ngrp_lin);
  add(&ret, "\n SAMPLE GRP SIZE ACROSS LINE DIR\t%d",d->ngrp_pix);
  add(&ret, "\n NUM USED PER GRP IN LINE DIR\t\t%d",d->nsamp_lin);
  add(&ret, "\n NUM USED PER GRP ACROSS LINE DIR\t%d",d->nsamp_pix);
  add(&ret, "\n MIN VAL FOR IST TBL BIN\t%16.7f",d->min_smp);
  add(&ret, "\n MAX VAL FOR LAST TBL BIN\t%16.7f",d->max_smp);
  add(&ret, "\n MEAN SAMPLE VALUE\t\t%16.7f",d->mean_smp);
  add(&ret, "\n STD DEV OF SAMPLE VALUE\t%16.7f",d->std_smp);
  add(&ret, "\n SAMPLE VALUE INCREMENT\t\t%16.7f",d->smp_inc);
  add(&ret, "\n MIN TBL VALUE\t\t\t%16.7f",d->min_hist);
  add(&ret, "\n MAX TBL VALUE\t\t\t%16.7f",d->max_hist);
  add(&ret, "\n MEAN TBL VALUE\t\t\t%16.7f",d->mean_hist);
  add(&ret, "\n STD DEV OF HIST TBL\t\t%16.7f",d->std_hist);
  add(&ret, "\n\n HISTOGRAM TBL SIZE\t%d",d->nhist);
  for (i=0; i<d->nhist; i+=8)
   {
     add(&ret, "\n #%3d - %3d :",i+1,i+8);
     add(&ret, "%8d",d->data_values_hist[i]);
     add(&ret, "%8d",d->data_values_hist[i+1]);
     add(&ret, "%8d",d->data_values_hist[i+2]);
     add(&ret, "%8d",d->data_values_hist[i+3]);
     add(&ret, "%8d",d->data_values_hist[i+4]);
     add(&ret, "%8d",d->data_values_hist[i+5]);
     add(&ret, "%8d",d->data_values_hist[i+6]);
     add(&ret, "%8d",d->data_values_hist[i+7]);
   }
  d = d->next;
 }
 add(&ret, "\n************** end of Data Histogram record ******************\n");
 return ret;
}

void prn_dhr(FILE *fp, struct data_hist_rec* h)
{
    char *rec = sprn_dhr(h);
    fprintf(fp, "%s", rec);
    FREE(rec);
}

