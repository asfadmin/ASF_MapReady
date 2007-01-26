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

char *sprn_rsr(struct rng_spec_rec *r)
{
 int i;

 char *ret = MALLOC(sizeof(char)*1);
 strcpy(ret, "");

 add(&ret, "\n**************** begin of Range Spectra record ****************\n");
 add(&ret, "\n RANGE SPECTRA SEQ NUM\t\t%d",r->seq_num);
 add(&ret, "\n SAR CHANNEL ID\t\t\t%d",r->sar_chan);
 add(&ret, "\n TBL DATA SET NUM IN RECORD\t%d",r->n_dset);
 add(&ret, "\n TBL DATA SET SIZE\t\t%d",r->dset_size);
 add(&ret, "\n DATA SETS REQ FOR FULL TABLE\t%d",r->req_recs);
 add(&ret, "\n FULL TBL SEQ NUM OF TBL IN REC\t%d",r->table_no);
 add(&ret, "\n TTL SAMP NUM IN RNG DIR\t%d",r->n_pixels);
 add(&ret, "\n NUM SAMP OFFSET FROM 1ST SAMP RNG LIN\t%d",r->pixel_offset);
 add(&ret, "\n NUM RNG LINES INTEGRATED\t%d",r->n_lines);
 add(&ret, "\n CTR FREQ OF FIRST SPEC BIN\t%16.7f",r->first_freq);
 add(&ret, "\n CTR FREQ OF LAST SPEC BIN\t%16.7f",r->last_freq);
 add(&ret, "\n MIN SPECTRAL PWR\t\t%16.7f",r->min_power);
 add(&ret, "\n MAX SPECTRAL PWR\t\t%16.7f",r->max_power);
 add(&ret, "\n NUM OF FREQ BINS IN TABLE\t%d",r->n_bins);
 for (i=0; i<r->n_bins; i+=4)
  {
     add(&ret, "\n DATA VALUES #%3d - %3d :",i+1,i+4);
     add(&ret, "%12.7f",r->data_values_spec[i]);
     add(&ret, "%12.7f",r->data_values_spec[i+1]);
     add(&ret, "%12.7f",r->data_values_spec[i+2]);
     add(&ret, "%12.7f",r->data_values_spec[i+3]);
  }
 add(&ret, "\n**************** end of Range Spectra record ******************\n");
 return ret;
}

void prn_rsr(FILE *fp, struct rng_spec_rec *r)
{
    char *rec = sprn_rsr(r);
    fprintf(fp, "%s", rec);
    FREE(rec);
}

