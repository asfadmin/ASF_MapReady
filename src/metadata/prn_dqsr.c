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

void prn_dqsr(FILE *fp, struct qual_sum_rec* q, int era)
{
 int i;

 fprintf(fp, "\n************ begin of Data Quality Summary record ************\n");
 fprintf(fp, "\n DATA QUAL SUM SEQ NUM\t\t%d",q->seq_num);
 fprintf(fp, "\n SARA CHANNEL ID\t\t%s",q->chan_ind);
 fprintf(fp, "\n DATE OF LAST CALIB UPDATE\t%s",q->cali_date);
 fprintf(fp, "\n NUM OF CHANNELS\t\t%d",q->nchn);
 fprintf(fp, "\n INTEGRATED SIDE LOB RATIO\t%16.7f",q->islr);
 fprintf(fp, "\n PEAK SIDE LOBE RATIO\t\t%16.7f",q->pslr);
 fprintf(fp, "\n AZI AMBIGUITY\t\t\t%16.7f",q->azi_ambig);
 fprintf(fp, "\n RNG AMBIGUITY\t\t\t%16.7f",q->rng_ambig);
 fprintf(fp, "\n ESTIMATE OF SNR\t\t%16.7f",q->snr);
 fprintf(fp, "\n ACTUAL BIT RATE ERROR\t\t\t%e",q->ber);
 fprintf(fp, "\n SLANT RNG RESOLUTION\t\t%16.7f",q->rng_res);
 fprintf(fp, "\n AZIMUTH RESOLUTION\t\t%16.7f",q->azi_res);
 fprintf(fp, "\n RADIOMETRIC RESOLUTION\t\t%16.7f",q->rad_res);
 fprintf(fp, "\n INSTAN DYNAMIC RANGE\t\t%16.7f",q->dyn_rng);
 fprintf(fp, "\n NOM RAD UNCERTAIN, DB\t\t%16.7f",q->abs_rad_unc_db);
 fprintf(fp, "\n NOM RAD UNCERTAIN, DEG\t\t%16.7f",q->abs_rad_unc_deg);
 fprintf(fp, "\n\n RELATIVE RADIOMETRIC DATA QUALITY");
  for (i=0; i<q->nchn; i++) {
    fprintf(fp, "\n REL RAD UNCERTAIN #%d, DB\t%16.7f",i+1,q->rel_rad_unc[0][i]);
    fprintf(fp, "\n REL RAD UNCERTAIN #%d, DEG\t%16.7f",i+1,q->rel_rad_unc[1][i]);
  }
 fprintf(fp, "\n\n ABSOLUTE GEOMETRIC DATA QUALITY");
 fprintf(fp, "\n LOC ERROR ALONG TRK\t\t%16.7f",q->alt_locerr);
 fprintf(fp, "\n LOC ERROR CROSS TRK\t\t%16.7f",q->crt_locerr);
 fprintf(fp, "\n ALONG TRK SCALE ERROR\t\t%16.7f",q->alt_scale);
 fprintf(fp, "\n CROSS TRK SCALE ERROR\t\t%16.7f",q->crt_scale);
 fprintf(fp, "\n DISTORTION SKEW\t\t%16.7f",q->dis_skew);
 fprintf(fp, "\n SCENE ORIENT ERROR\t\t%16.7f",q->ori_err);

 if (era)
  {
   fprintf(fp, "\n\n RELATIVE GEOMETRIC DATA QUALITY");
   for (i=0; i<q->nchn; i++) {
     fprintf(fp, "\n ALONG TRK MISREG ERROR #%d\t%16.7f",i+1, q->misreg[0][i]);
     fprintf(fp, "\n CROSS TRK MISREG ERROR #%d\t%16.7f",i+1, q->misreg[1][i]);
   }
   fprintf(fp, "\n SCENE ORIENT ERROR\t\t%16.7f",q->nesz);
   fprintf(fp, "\n SCENE ORIENT ERROR\t\t%16.7f",q->enl);
   fprintf(fp, "\n TABLE UPDATE DATE \t\t%s",q->tb_update);
   fprintf(fp, "\n CALIBRATION FLAG  \t\t%s",q->cal_status);
   fprintf(fp, "\n CALIBRATION COMMENTS\t\t%s",q->cal_comment);
  }

 fprintf(fp, "\n************** end of Data Quality Summary record ************\n");

 return;
}

