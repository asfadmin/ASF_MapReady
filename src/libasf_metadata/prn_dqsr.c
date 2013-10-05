/********************************************************************
NAME:     prn_dqsr.c -- print data quality summary record values

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0            4/96   T. Logan (ASF)   Borrowed from JPL ceos reader
*********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

char *sprn_dqsr(struct qual_sum_rec* q, int era)
{
 char *ret = MALLOC(sizeof(char)*1);
 strcpy(ret, "");

 int i;

 add(&ret, "\n************ begin of Data Quality Summary record ************\n");
 add(&ret, "\n DATA QUAL SUM SEQ NUM\t\t%d",q->seq_num);
 add(&ret, "\n SARA CHANNEL ID\t\t%s",q->chan_ind);
 add(&ret, "\n DATE OF LAST CALIB UPDATE\t%s",q->cali_date);
 add(&ret, "\n NUM OF CHANNELS\t\t%d",q->nchn);
 add(&ret, "\n INTEGRATED SIDE LOB RATIO\t%16.7f",q->islr);
 add(&ret, "\n PEAK SIDE LOBE RATIO\t\t%16.7f",q->pslr);
 add(&ret, "\n AZI AMBIGUITY\t\t\t%16.7f",q->azi_ambig);
 add(&ret, "\n RNG AMBIGUITY\t\t\t%16.7f",q->rng_ambig);
 add(&ret, "\n ESTIMATE OF SNR\t\t%16.7f",q->snr);
 add(&ret, "\n ACTUAL BIT RATE ERROR\t\t\t%e",q->ber);
 add(&ret, "\n SLANT RNG RESOLUTION\t\t%16.7f",q->rng_res);
 add(&ret, "\n AZIMUTH RESOLUTION\t\t%16.7f",q->azi_res);
 add(&ret, "\n RADIOMETRIC RESOLUTION\t\t%16.7f",q->rad_res);
 add(&ret, "\n INSTAN DYNAMIC RANGE\t\t%16.7f",q->dyn_rng);
 add(&ret, "\n NOM RAD UNCERTAIN, DB\t\t%16.7f",q->abs_rad_unc_db);
 add(&ret, "\n NOM RAD UNCERTAIN, DEG\t\t%16.7f",q->abs_rad_unc_deg);
 add(&ret, "\n\n RELATIVE RADIOMETRIC DATA QUALITY");
  for (i=0; i<q->nchn; i++) {
    add(&ret, "\n REL RAD UNCERTAIN #%d, DB\t%16.7f",i+1,q->rel_rad_unc[0][i]);
    add(&ret, "\n REL RAD UNCERTAIN #%d, DEG\t%16.7f",i+1,q->rel_rad_unc[1][i]);
  }
 add(&ret, "\n\n ABSOLUTE GEOMETRIC DATA QUALITY");
 add(&ret, "\n LOC ERROR ALONG TRK\t\t%16.7f",q->alt_locerr);
 add(&ret, "\n LOC ERROR CROSS TRK\t\t%16.7f",q->crt_locerr);
 add(&ret, "\n ALONG TRK SCALE ERROR\t\t%16.7f",q->alt_scale);
 add(&ret, "\n CROSS TRK SCALE ERROR\t\t%16.7f",q->crt_scale);
 add(&ret, "\n DISTORTION SKEW\t\t%16.7f",q->dis_skew);
 add(&ret, "\n SCENE ORIENT ERROR\t\t%16.7f",q->ori_err);

 if (era)
  {
   add(&ret, "\n\n RELATIVE GEOMETRIC DATA QUALITY");
   for (i=0; i<q->nchn; i++) {
     add(&ret, "\n ALONG TRK MISREG ERROR #%d\t%16.7f",i+1, q->misreg[0][i]);
     add(&ret, "\n CROSS TRK MISREG ERROR #%d\t%16.7f",i+1, q->misreg[1][i]);
   }
   add(&ret, "\n SCENE ORIENT ERROR\t\t%16.7f",q->nesz);
   add(&ret, "\n SCENE ORIENT ERROR\t\t%16.7f",q->enl);
   add(&ret, "\n TABLE UPDATE DATE \t\t%s",q->tb_update);
   add(&ret, "\n CALIBRATION FLAG  \t\t%s",q->cal_status);
   add(&ret, "\n CALIBRATION COMMENTS\t\t%s",q->cal_comment);
  }

 add(&ret, "\n************** end of Data Quality Summary record ************\n");

 return ret;
}

void prn_dqsr(FILE *fp, struct qual_sum_rec* q, int era)
{
    char *rec = sprn_dqsr(q, era);
    fprintf(fp, "%s", rec);
    FREE(rec);
}


