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

void prn_dhr(FILE *fp, struct data_hist_rec* h)
{
 int i,j=1;
 struct hist_dset *d;

 fprintf(fp, "\n************** begin of Data Histogram record ****************\n");
 fprintf(fp, "\n DATA HISTOGRAM SEQ NUM\t\t%d",h->seq_num);
 fprintf(fp, "\n SAR CHANNEL ID\t\t\t%d",h->sar_chan);
 fprintf(fp, "\n NUM HISTOGRAM DATA SETS\t%d",h->ntab);
 fprintf(fp, "\n TABLE DATA SET SIZE\t\t%d",h->ltab);
 d = h->data; 

 while (d != NULL)
 {
  fprintf(fp, "\n\n DATA SET NUM\t\t\t%d",j++);
  fprintf(fp, "\n HISTOGRAM DESCRIPTOR\t\t%s",d->hist_desc);
  fprintf(fp, "\n RECS NEEDED FOR FULL TABLE\t%d",d->nrec);
  fprintf(fp, "\n TBL REC SEQ NUM\t\t%d",d->tab_seq);
  fprintf(fp, "\n NUM OF TBL BINS\t\t%d",d->nbin);
  fprintf(fp, "\n NUM SAMPLES IN LINE DIR\t%d",d->ns_lin);
  fprintf(fp, "\n NUM SAMPLES ACROSS LINE DIR\t%d",d->ns_pix);
  fprintf(fp, "\n SAMPLE GRP SIZE IN LINE DIR\t%d",d->ngrp_lin);
  fprintf(fp, "\n SAMPLE GRP SIZE ACROSS LINE DIR\t%d",d->ngrp_pix);
  fprintf(fp, "\n NUM USED PER GRP IN LINE DIR\t\t%d",d->nsamp_lin);
  fprintf(fp, "\n NUM USED PER GRP ACROSS LINE DIR\t%d",d->nsamp_pix);
  fprintf(fp, "\n MIN VAL FOR IST TBL BIN\t%16.7f",d->min_smp);
  fprintf(fp, "\n MAX VAL FOR LAST TBL BIN\t%16.7f",d->max_smp);
  fprintf(fp, "\n MEAN SAMPLE VALUE\t\t%16.7f",d->mean_smp);
  fprintf(fp, "\n STD DEV OF SAMPLE VALUE\t%16.7f",d->std_smp);
  fprintf(fp, "\n SAMPLE VALUE INCREMENT\t\t%16.7f",d->smp_inc);
  fprintf(fp, "\n MIN TBL VALUE\t\t\t%16.7f",d->min_hist);
  fprintf(fp, "\n MAX TBL VALUE\t\t\t%16.7f",d->max_hist);
  fprintf(fp, "\n MEAN TBL VALUE\t\t\t%16.7f",d->mean_hist);
  fprintf(fp, "\n STD DEV OF HIST TBL\t\t%16.7f",d->std_hist);
  fprintf(fp, "\n\n HISTOGRAM TBL SIZE\t%d",d->nhist);
  for (i=0; i<d->nhist; i+=8)
   {
     fprintf(fp, "\n #%3d - %3d :",i+1,i+8);
     fprintf(fp, "%8d",d->data_values_hist[i]);
     fprintf(fp, "%8d",d->data_values_hist[i+1]);
     fprintf(fp, "%8d",d->data_values_hist[i+2]);
     fprintf(fp, "%8d",d->data_values_hist[i+3]);
     fprintf(fp, "%8d",d->data_values_hist[i+4]);
     fprintf(fp, "%8d",d->data_values_hist[i+5]);
     fprintf(fp, "%8d",d->data_values_hist[i+6]);
     fprintf(fp, "%8d",d->data_values_hist[i+7]);
   }
  d = d->next;
 }
 fprintf(fp, "\n************** end of Data Histogram record ******************\n");
}

