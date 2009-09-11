#include "asf.h"
#include "ceos.h"

void prn_dqsr(char *file, struct qual_sum_rec* q, int era)
{
  FILE *fp;
  int i;
  
  fp = FOPEN(file, "w");
  fprintf(fp, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n");
  fprintf(fp, "\"http://www.w3.org/TR/html4/loose.dtd\">\n<html>\n<head>\n");
  fprintf(fp, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\n");
  fprintf(fp, "<title>Data Quality Summary record</title>\n</head>\n<body>\n");
  fprintf(fp, "<h2>Data Quality Summary record</h2>\n");
  fprintf(fp, "<strong>DATA QUAL SUM SEQ NUM: </strong>%d<br>\n",q->seq_num);
  fprintf(fp, "<strong>SARA CHANNEL ID: </strong>%s<br>\n",q->chan_ind);
  fprintf(fp, "<strong>DATE OF LAST CALIB UPDATE: </strong>%s<br>\n",q->cali_date);
  fprintf(fp, "<strong>NUM OF CHANNELS: </strong>%d<br>\n",q->nchn);
  fprintf(fp, "<strong>INTEGRATED SIDE LOB RATIO: </strong>%16.7f<br>\n",q->islr);
  fprintf(fp, "<strong>PEAK SIDE LOBE RATIO: </strong>%16.7f<br>\n",q->pslr);
  fprintf(fp, "<strong>AZI AMBIGUITY: </strong>%16.7f<br>\n",q->azi_ambig);
  fprintf(fp, "<strong>RNG AMBIGUITY: </strong>%16.7f<br>\n",q->rng_ambig);
  fprintf(fp, "<strong>ESTIMATE OF SNR: </strong>%16.7f<br>\n",q->snr);
  fprintf(fp, "<strong>ACTUAL BIT RATE ERROR: </strong>%e<br>\n",q->ber);
  fprintf(fp, "<strong>SLANT RNG RESOLUTION: </strong>%16.7f<br>\n",q->rng_res);
  fprintf(fp, "<strong>AZIMUTH RESOLUTION: </strong>%16.7f<br>\n",q->azi_res);
  fprintf(fp, "<strong>RADIOMETRIC RESOLUTION: </strong>%16.7f<br>\n",q->rad_res);
  fprintf(fp, "<strong>INSTAN DYNAMIC RANGE: </strong>%16.7f<br>\n",q->dyn_rng);
  fprintf(fp, "<strong>NOM RAD UNCERTAIN, DB: </strong>%16.7f<br>\n",q->abs_rad_unc_db);
  fprintf(fp, "<strong>NOM RAD UNCERTAIN, DEG: </strong>%16.7f<br>\n",q->abs_rad_unc_deg);
  fprintf(fp, "<br><strong>RELATIVE RADIOMETRIC DATA QUALITY</strong><br>\n");
  for (i=0; i<q->nchn; i++) {
    fprintf(fp, "<strong>REL RAD UNCERTAIN #%d, DB: </strong>%16.7f<br>\n",i+1,q->rel_rad_unc[0][i]);
    fprintf(fp, "<strong>REL RAD UNCERTAIN #%d, DEG: </strong>%16.7f<br>\n",i+1,q->rel_rad_unc[1][i]);
  }
  fprintf(fp, "<br><strong>ABSOLUTE GEOMETRIC DATA QUALITY</strong><br>\n");
  fprintf(fp, "<strong>LOC ERROR ALONG TRK: </strong>%16.7f<br>\n",q->alt_locerr);
  fprintf(fp, "<strong>LOC ERROR CROSS TRK: </strong>%16.7f<br>\n",q->crt_locerr);
  fprintf(fp, "<strong>ALONG TRK SCALE ERROR: </strong>%16.7f<br>\n",q->alt_scale);
  fprintf(fp, "<strong>CROSS TRK SCALE ERROR: </strong>%16.7f<br>\n",q->crt_scale);
  fprintf(fp, "<strong>DISTORTION SKEW: </strong>%16.7f<br>\n",q->dis_skew);
  fprintf(fp, "<strong>SCENE ORIENT ERROR: </strong>%16.7f<br>\n",q->ori_err);
  
  if (era)
    {
      fprintf(fp, "<br><strong>RELATIVE GEOMETRIC DATA QUALITY</strong><br>\n");
      for (i=0; i<q->nchn; i++) {
	fprintf(fp, "<strong>ALONG TRK MISREG ERROR #%d: </strong>%16.7f<br>\n",i+1, q->misreg[0][i]);
	fprintf(fp, "<strong>CROSS TRK MISREG ERROR #%d: </strong>%16.7f<br>\n",i+1, q->misreg[1][i]);
      }
      fprintf(fp, "<strong>SCENE ORIENT ERROR: </strong>%16.7f<br>\n",q->nesz);
      fprintf(fp, "<strong>SCENE ORIENT ERROR: </strong>%16.7f<br>\n",q->enl);
      fprintf(fp, "<strong>TABLE UPDATE DATE : </strong>%s<br>\n",q->tb_update);
      fprintf(fp, "<strong>CALIBRATION FLAG: </strong>%s<br>\n",q->cal_status);
      fprintf(fp, "<strong>CALIBRATION COMMENTS: </strong>%s<br>\n",q->cal_comment);
    }
  
  fprintf(fp, "</body>\n</html>\n");
  FCLOSE(fp);
  return;
}

