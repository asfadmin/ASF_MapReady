/********************************************************************
NAME:     prn_rsr.c --  print attitude data record values

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0            4/96   T. Logan (ASF)   Borrowed from JPL ceos reader
*********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

void prn_rsr(FILE *fp, struct rng_spec_rec *r)
{
 int i;

 fprintf(fp, "\n**************** begin of Range Spectra record ****************\n");
 fprintf(fp, "\n RANGE SPECTRA SEQ NUM\t\t%d",r->seq_num);
 fprintf(fp, "\n SAR CHANNEL ID\t\t\t%d",r->sar_chan);
 fprintf(fp, "\n TBL DATA SET NUM IN RECORD\t%d",r->n_dset);
 fprintf(fp, "\n TBL DATA SET SIZE\t\t%d",r->dset_size);
 fprintf(fp, "\n DATA SETS REQ FOR FULL TABLE\t%d",r->req_recs);
 fprintf(fp, "\n FULL TBL SEQ NUM OF TBL IN REC\t%d",r->table_no);
 fprintf(fp, "\n TTL SAMP NUM IN RNG DIR\t%d",r->n_pixels);
 fprintf(fp, "\n NUM SAMP OFFSET FROM 1ST SAMP RNG LIN\t%d",r->pixel_offset);
 fprintf(fp, "\n NUM RNG LINES INTEGRATED\t%d",r->n_lines);
 fprintf(fp, "\n CTR FREQ OF FIRST SPEC BIN\t%16.7f",r->first_freq);
 fprintf(fp, "\n CTR FREQ OF LAST SPEC BIN\t%16.7f",r->last_freq);
 fprintf(fp, "\n MIN SPECTRAL PWR\t\t%16.7f",r->min_power);
 fprintf(fp, "\n MAX SPECTRAL PWR\t\t%16.7f",r->max_power);
 fprintf(fp, "\n NUM OF FREQ BINS IN TABLE\t%d",r->n_bins);
 for (i=0; i<r->n_bins; i+=4)
  {
     fprintf(fp, "\n DATA VALUES #%3d - %3d :",i+1,i+4);
     fprintf(fp, "%12.7f",r->data_values_spec[i]);
     fprintf(fp, "%12.7f",r->data_values_spec[i+1]);
     fprintf(fp, "%12.7f",r->data_values_spec[i+2]);
     fprintf(fp, "%12.7f",r->data_values_spec[i+3]);
  }
 fprintf(fp, "\n**************** end of Range Spectra record ******************\n");
 return;
}

