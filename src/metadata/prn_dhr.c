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

void prn_dhr(struct data_hist_rec* h)
{
 int i,j=1;
 struct hist_dset *d;

 printf("\n**************** begin of Data Histogram record ****************\n");
 printf("\n DATA HISTOGRAM SEQ NUM\t\t%d",h->seq_num);
 printf("\n SAR CHANNEL ID\t\t\t%d",h->sar_chan);
 printf("\n NUM HISTOGRAM DATA SETS\t%d",h->ntab);
 printf("\n TABLE DATA SET SIZE\t\t%d",h->ltab);
 d = h->data; 

 while (d != NULL)
 {
  printf("\n\n DATA SET NUM\t\t\t%d",j++);
  printf("\n HISTOGRAM DESCRIPTOR\t\t%s",d->hist_desc);
  printf("\n RECS NEEDED FOR FULL TABLE\t%d",d->nrec);
  printf("\n TBL REC SEQ NUM\t\t%d",d->tab_seq);
  printf("\n NUM OF TBL BINS\t\t%d",d->nbin);
  printf("\n NUM SAMPLES IN LINE DIR\t%d",d->ns_lin);
  printf("\n NUM SAMPLES ACROSS LINE DIR\t%d",d->ns_pix);
  printf("\n SAMPLE GRP SIZE IN LINE DIR\t%d",d->ngrp_lin);
  printf("\n SAMPLE GRP SIZE ACROSS LINE DIR\t%d",d->ngrp_pix);
  printf("\n NUM USED PER GRP IN LINE DIR\t\t%d",d->nsamp_lin);
  printf("\n NUM USED PER GRP ACROSS LINE DIR\t%d",d->nsamp_pix);
  printf("\n MIN VAL FOR IST TBL BIN\t%16.7f",d->min_smp);
  printf("\n MAX VAL FOR LAST TBL BIN\t%16.7f",d->max_smp);
  printf("\n MEAN SAMPLE VALUE\t\t%16.7f",d->mean_smp);
  printf("\n STD DEV OF SAMPLE VALUE\t%16.7f",d->std_smp);
  printf("\n SAMPLE VALUE INCREMENT\t\t%16.7f",d->smp_inc);
  printf("\n MIN TBL VALUE\t\t\t%16.7f",d->min_hist);
  printf("\n MAX TBL VALUE\t\t\t%16.7f",d->max_hist);
  printf("\n MEAN TBL VALUE\t\t\t%16.7f",d->mean_hist);
  printf("\n STD DEV OF HIST TBL\t\t%16.7f",d->std_hist);
  printf("\n\n HISTOGRAM TBL SIZE\t%d",d->nhist);
  for (i=0; i<d->nhist; i+=8)
   {
     printf("\n #%3d - %3d :",i+1,i+8);
     printf("%8d",d->data_values_hist[i]);
     printf("%8d",d->data_values_hist[i+1]);
     printf("%8d",d->data_values_hist[i+2]);
     printf("%8d",d->data_values_hist[i+3]);
     printf("%8d",d->data_values_hist[i+4]);
     printf("%8d",d->data_values_hist[i+5]);
     printf("%8d",d->data_values_hist[i+6]);
     printf("%8d",d->data_values_hist[i+7]);
   }
  d = d->next;
 }
 printf("\n**************** end of Data Histogram record ******************\n");
}

