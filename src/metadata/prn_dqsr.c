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

void prn_dqsr(struct qual_sum_rec* q, int era)
{
 int i;

 printf("\n************ begin of Data Quality Summary record **************\n");
 printf("\n DATA QUAL SUM SEQ NUM\t\t%d",q->seq_num);
 printf("\n SARA CHANNEL ID\t\t%s",q->chan_ind);
 printf("\n DATE OF LAST CALIB UPDATE\t%s",q->cali_date);
 printf("\n NUM OF CHANNELS\t\t%d",q->nchn);
 printf("\n INTEGRATED SIDE LOB RATIO\t%16.7f",q->islr);
 printf("\n PEAK SIDE LOBE RATIO\t\t%16.7f",q->pslr);
 printf("\n AZI AMBIGUITY\t\t\t%16.7f",q->azi_ambig);
 printf("\n RNG AMBIGUITY\t\t\t%16.7f",q->rng_ambig);
 printf("\n ESTIMATE OF SNR\t\t%16.7f",q->snr);
 printf("\n ACTUAL BIT RATE ERROR\t\t\t%e",q->ber);
 printf("\n SLANT RNG RESOLUTION\t\t%16.7f",q->rng_res);
 printf("\n AZIMUTH RESOLUTION\t\t%16.7f",q->azi_res);
 printf("\n RADIOMETRIC RESOLUTION\t\t%16.7f",q->rad_res);
 printf("\n INSTAN DYNAMIC RANGE\t\t%16.7f",q->dyn_rng);
 printf("\n NOM RAD UNCERTAIN, DB\t\t%16.7f",q->abs_rad_unc_db);
 printf("\n NOM RAD UNCERTAIN, DEG\t\t%16.7f",q->abs_rad_unc_deg);
 printf("\n\n RELATIVE RADIOMETRIC DATA QUALITY");
  for (i=0; i<q->nchn; i++) {
    printf("\n REL RAD UNCERTAIN #%d, DB\t%16.7f",i+1,q->rel_rad_unc[0][i]);
    printf("\n REL RAD UNCERTAIN #%d, DEG\t%16.7f",i+1,q->rel_rad_unc[1][i]);
  }
 printf("\n\n ABSOLUTE GEOMETRIC DATA QUALITY");
 printf("\n LOC ERROR ALONG TRK\t\t%16.7f",q->alt_locerr);
 printf("\n LOC ERROR CROSS TRK\t\t%16.7f",q->crt_locerr);
 printf("\n ALONG TRK SCALE ERROR\t\t%16.7f",q->alt_scale);
 printf("\n CROSS TRK SCALE ERROR\t\t%16.7f",q->crt_scale);
 printf("\n DISTORTION SKEW\t\t%16.7f",q->dis_skew);
 printf("\n SCENE ORIENT ERROR\t\t%16.7f",q->ori_err);

 if (era)
  {
   printf("\n\n RELATIVE GEOMETRIC DATA QUALITY");
   for (i=0; i<q->nchn; i++) {
     printf("\n ALONG TRK MISREG ERROR #%d\t%16.7f",i+1, q->misreg[0][i]);
     printf("\n CROSS TRK MISREG ERROR #%d\t%16.7f",i+1, q->misreg[1][i]);
   }
   printf("\n SCENE ORIENT ERROR\t\t%16.7f",q->nesz);
   printf("\n SCENE ORIENT ERROR\t\t%16.7f",q->enl);
   printf("\n TABLE UPDATE DATE \t\t%s",q->tb_update);
   printf("\n CALIBRATION FLAG  \t\t%s",q->cal_status);
   printf("\n CALIBRATION COMMENTS\t\t%s",q->cal_comment);
  }

 printf("\n************** end of Data Quality Summary record **************\n");

 return;
}

