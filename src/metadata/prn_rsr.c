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

void prn_rsr(struct rng_spec_rec *r)
{
 int i;

 printf("\n**************** begin of Range Spectra record ****************\n");
 printf("\n RANGE SPECTRA SEQ NUM\t\t%d",r->seq_num);
 printf("\n SAR CHANNEL ID\t\t\t%d",r->sar_chan);
 printf("\n TBL DATA SET NUM IN RECORD\t%d",r->n_dset);
 printf("\n TBL DATA SET SIZE\t\t%d",r->dset_size);
 printf("\n DATA SETS REQ FOR FULL TABLE\t%d",r->req_recs);
 printf("\n FULL TBL SEQ NUM OF TBL IN REC\t%d",r->table_no);
 printf("\n TTL SAMP NUM IN RNG DIR\t%d",r->n_pixels);
 printf("\n NUM SAMP OFFSET FROM 1ST SAMP RNG LIN\t%d",r->pixel_offset);
 printf("\n NUM RNG LINES INTEGRATED\t%d",r->n_lines);
 printf("\n CTR FREQ OF FIRST SPEC BIN\t%16.7f",r->first_freq);
 printf("\n CTR FREQ OF LAST SPEC BIN\t%16.7f",r->last_freq);
 printf("\n MIN SPECTRAL PWR\t\t%16.7f",r->min_power);
 printf("\n MAX SPECTRAL PWR\t\t%16.7f",r->max_power);
 printf("\n NUM OF FREQ BINS IN TABLE\t%d",r->n_bins);
 for (i=0; i<r->n_bins; i+=4)
  {
     printf("\n DATA VALUES #%3d - %3d :",i+1,i+4);
     printf("%12.7f",r->data_values_spec[i]);
     printf("%12.7f",r->data_values_spec[i+1]);
     printf("%12.7f",r->data_values_spec[i+2]);
     printf("%12.7f",r->data_values_spec[i+3]);
  }
 printf("\n**************** end of Range Spectra record ******************\n");
 return;
}

