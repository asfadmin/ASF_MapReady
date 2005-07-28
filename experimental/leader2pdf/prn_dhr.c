#include "asf.h"
#include "ceos.h"

void prn_dhr(char *file, struct data_hist_rec* h)
{
  FILE *fp;
  int i,j=1;
  struct hist_dset *d;
  
  fp = FOPEN(file, "w");
  fprintf(fp, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n");
  fprintf(fp, "\"http://www.w3.org/TR/html4/loose.dtd\">\n<html>\n<head>\n");
  fprintf(fp, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\n");
  fprintf(fp, "<title>Data Histogram record</title>\n</head>\n<body>\n");
  fprintf(fp, "<h2>Data Histogram record</h2>\n");
  fprintf(fp, "<strong>DATA HISTOGRAM SEQ NUM: </strong>%d<br>\n",h->seq_num);
  fprintf(fp, "<strong>SAR CHANNEL ID: </strong>%d<br>\n",h->sar_chan);
  fprintf(fp, "<strong>NUM HISTOGRAM DATA SETS: </strong>%d<br>\n",h->ntab);
  fprintf(fp, "<strong>TABLE DATA SET SIZE: </strong>%d<br>\n",h->ltab);
  d = h->data; 
  
  while (d != NULL)
    {
      fprintf(fp, "<br><strong>DATA SET NUM: </strong>%d<br>\n",j++);
      fprintf(fp, "<strong>HISTOGRAM DESCRIPTOR: </strong>%s<br>\n",d->hist_desc);
      fprintf(fp, "<strong>RECS NEEDED FOR FULL TABLE: </strong>%d<br>\n",d->nrec);
      fprintf(fp, "<strong>TBL REC SEQ NUM: </strong>%d<br>\n",d->tab_seq);
      fprintf(fp, "<strong>NUM OF TBL BINS: </strong>%d<br>\n",d->nbin);
      fprintf(fp, "<strong>NUM SAMPLES IN LINE DIR: </strong>%d<br>\n",d->ns_lin);
      fprintf(fp, "<strong>NUM SAMPLES ACROSS LINE DIR: </strong>%d<br>\n",d->ns_pix);
      fprintf(fp, "<strong>SAMPLE GRP SIZE IN LINE DIR: </strong>%d<br>\n",d->ngrp_lin);
      fprintf(fp, "<strong>SAMPLE GRP SIZE ACROSS LINE DIR: </strong>%d<br>\n",d->ngrp_pix);
      fprintf(fp, "<strong>NUM USED PER GRP IN LINE DIR: </strong>%d<br>\n",d->nsamp_lin);
      fprintf(fp, "<strong>NUM USED PER GRP ACROSS LINE DIR: </strong>%d<br>\n",d->nsamp_pix);
      fprintf(fp, "<strong>MIN VAL FOR IST TBL BIN: </strong>%16.7f<br>\n",d->min_smp);
      fprintf(fp, "<strong>MAX VAL FOR LAST TBL BIN: </strong>%16.7f<br>\n",d->max_smp);
      fprintf(fp, "<strong>MEAN SAMPLE VALUE: </strong>%16.7f<br>\n",d->mean_smp);
      fprintf(fp, "<strong>STD DEV OF SAMPLE VALUE: </strong>%16.7f<br>\n",d->std_smp);
      fprintf(fp, "<strong>SAMPLE VALUE INCREMENT: </strong>%16.7f<br>\n",d->smp_inc);
      fprintf(fp, "<strong>MIN TBL VALUE: </strong>%16.7f<br>\n",d->min_hist);
      fprintf(fp, "<strong>MAX TBL VALUE: </strong>%16.7f<br>\n",d->max_hist);
      fprintf(fp, "<strong>MEAN TBL VALUE: </strong>%16.7f<br>\n",d->mean_hist);
      fprintf(fp, "<strong>STD DEV OF HIST TBL: </strong>%16.7f<br>\n",d->std_hist);
      fprintf(fp, "<br><strong>HISTOGRAM TBL SIZE: </strong>%d<br>\n",d->nhist);
      for (i=0; i<d->nhist; i+=8)
	{
	  fprintf(fp, "<br> #%3d - %3d :",i+1,i+8);
	  fprintf(fp, "%8d",d->data_values_hist[i]);
	  fprintf(fp, "%8d",d->data_values_hist[i+1]);
	  fprintf(fp, "%8d",d->data_values_hist[i+2]);
	  fprintf(fp, "%8d",d->data_values_hist[i+3]);
	  fprintf(fp, "%8d",d->data_values_hist[i+4]);
	  fprintf(fp, "%8d",d->data_values_hist[i+5]);
	  fprintf(fp, "%8d",d->data_values_hist[i+6]);
	  fprintf(fp, "%8d",d->data_values_hist[i+7]);
	}
      fprintf(fp, "<br>\n");
      d = d->next;
    }
  fprintf(fp, "</body>\n</html>\n");
  FCLOSE(fp);
}

