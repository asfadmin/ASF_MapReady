#include "asf.h"
#include "ceos.h"

void prn_atdr(char *file, struct att_data_rec* a)
{
  FILE *fp;
  struct att_vect_rec *d;
  int j = 1;
  
  fp = FOPEN(file, "w");
  fprintf(fp, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n");
  fprintf(fp, "\"http://www.w3.org/TR/html4/loose.dtd\">\n<html>\n<head>\n");
  fprintf(fp, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\n");
  fprintf(fp, "<title>Attitude Data record</title>\n</head>\n<body>\n");
  fprintf(fp, "<h2>Attitude Data record</h2>\n");
  fprintf(fp, "<strong>NUM OF POINTS: </strong>%d<br>\n",a->npoint);
  d = a->data;
  
  for (j = 1; j<=(a->npoint); j++)
    {
      fprintf(fp, "<strong>POINT NUMBER: </strong>%i<br>\n",j);
      fprintf(fp, "<strong>DAY IN THE YEAR (GMT): </strong>%d<br>\n",d->gmt_day);
      fprintf(fp, "<strong>MS OF THE DAY (GMT): </strong>%d<br>\n",d->gmt_msec);
      fprintf(fp, "<strong>PITCH DATA QUAL FLAG: </strong>%d<br>\n",d->pitch_flag);
      fprintf(fp, "<strong>ROLL DATA QUAL FLAG: </strong>%d<br>\n",d->roll_flag);
      fprintf(fp, "<strong>YAW DATA QUAL FLAG: </strong>%d<br>\n",d->yaw_flag);
      fprintf(fp, "<strong>PITCH: </strong>%14.6f<br>\n",d->pitch);
      fprintf(fp, "<strong>ROLL: </strong>%14.6f<br>\n",d->roll);
      fprintf(fp, "<strong>YAW: </strong>%14.6f<br>\n",d->yaw);
      fprintf(fp, "<strong>PITCH RATE QUAL FLAG: </strong>%d<br>\n",d->pitch_rate_flag);
      fprintf(fp, "<strong>ROLL RATE QUAL FLAG: </strong>%d<br>\n",d->roll_rate_flag);
      fprintf(fp, "<strong>YAW RATE QUAL FLAG: </strong>%d<br>\n",d->yaw_rate_flag);
      fprintf(fp, "<strong>PITCH RATE: </strong>%14.6f<br>\n",d->pitch_rate);
      fprintf(fp, "<strong>ROLL RATE: </strong>%14.6f<br>\n",d->roll_rate);
      fprintf(fp, "<strong>YAW RATE: </strong>%14.6f<br>\n",d->yaw_rate);
      fprintf(fp, "<strong>END OF POINT: </strong>%i<br>\n",j);
      d = d->next;
    }
  fprintf(fp, "</body>\n</html>\n");
  FCLOSE(fp);
  return;
}

