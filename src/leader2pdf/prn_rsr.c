#include "asf.h"
#include "ceos.h"

void prn_rsr(char *file, struct rng_spec_rec *r)
{
  FILE *fp;
  int i;
  
  fp = FOPEN(file, "w");
  fprintf(fp, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n");
  fprintf(fp, "\"http://www.w3.org/TR/html4/loose.dtd\">\n<html>\n<head>\n");
  fprintf(fp, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\n");
  fprintf(fp, "<title>Range Spectra record</title>\n</head>\n<body>\n");
  fprintf(fp, "<h2>Range Spectra record</h2>\n");
  fprintf(fp, "<strong>RANGE SPECTRA SEQ NUM: </strong>%d<br>\n", r->seq_num);
  fprintf(fp, "<strong>SAR CHANNEL ID: </strong>%d<br>\n", r->sar_chan);
  fprintf(fp, "<strong>TBL DATA SET NUM IN RECORD: </strong>%d<br>\n", r->n_dset);
  fprintf(fp, "<strong>TBL DATA SET SIZE: </strong>%d<br>\n", r->dset_size);
  fprintf(fp, "<strong>DATA SETS REQ FOR FULL TABLE: </strong>%d<br>\n", r->req_recs);
  fprintf(fp, "<strong>FULL TBL SEQ NUM OF TBL IN REC: </strong>%d<br>\n", r->table_no);
  fprintf(fp, "<strong>TTL SAMP NUM IN RNG DIR: </strong>%d<br>\n", r->n_pixels);
  fprintf(fp, "<strong>NUM SAMP OFFSET FROM 1ST SAMP RNG LIN: </strong>%d<br>\n", r->pixel_offset);
  fprintf(fp, "<strong>NUM RNG LINES INTEGRATED: </strong>%d<br>\n", r->n_lines);
  fprintf(fp, "<strong>CTR FREQ OF FIRST SPEC BIN: </strong>%16.7f<br>\n", r->first_freq);
  fprintf(fp, "<strong>CTR FREQ OF LAST SPEC BIN: </strong>%16.7f<br>\n", r->last_freq);
  fprintf(fp, "<strong>MIN SPECTRAL PWR: </strong>%16.7f<br>\n", r->min_power);
  fprintf(fp, "<strong>MAX SPECTRAL PWR: </strong>%16.7f<br>\n", r->max_power);
  fprintf(fp, "<strong>NUM OF FREQ BINS IN TABLE: </strong>%d<br>\n", r->n_bins);
  for (i=0; i<r->n_bins; i+=4)
    {
      fprintf(fp, "<strong>DATA VALUES #%3d - %3d :</strong>",i+1,i+4);
      fprintf(fp, "%12.7f",r->data_values_spec[i]);
      fprintf(fp, "%12.7f",r->data_values_spec[i+1]);
      fprintf(fp, "%12.7f",r->data_values_spec[i+2]);
      fprintf(fp, "%12.7f<br>\n",r->data_values_spec[i+3]);
    }
  fprintf(fp, "</body>\n</html>\n");
  FCLOSE(fp);

  return;
}

