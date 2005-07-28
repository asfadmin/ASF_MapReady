#include "asf.h"
#include "ceos.h"

void prn_ppdr(char *file, struct pos_data_rec *p)
{
  FILE *fp;
  int i;
  
  fp = FOPEN(file, "w");
  fprintf(fp, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n");
  fprintf(fp, "\"http://www.w3.org/TR/html4/loose.dtd\">\n<html>\n<head>\n");
  fprintf(fp, "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-1\">\n");
  fprintf(fp, "<title>Platform Position record</title>\n</head>\n<body>\n");
  fprintf(fp, "<h2>Platform Position record</h2>\n");
  fprintf(fp, "<strong>ORBITAL ELMTS DESG: </strong>%s<br>\n",p->orbit_ele_desg);
  fprintf(fp, "<strong>ORBITAL ELEMENT 1: </strong>%16.7f<br>\n",p->orbit_ele[0]);
  fprintf(fp, "<strong>ORBITAL ELEMENT 2: </strong>%16.7f<br>\n",p->orbit_ele[1]);
  fprintf(fp, "<strong>ORBITAL ELEMENT 3: </strong>%16.7f<br>\n",p->orbit_ele[2]);
  fprintf(fp, "<strong>ORBITAL ELEMENT 4: </strong>%16.7f<br>\n",p->orbit_ele[3]);
  fprintf(fp, "<strong>ORBITAL ELEMENT 5: </strong>%16.7f<br>\n",p->orbit_ele[4]);
  fprintf(fp, "<strong>ORBITAL ELEMENT 6: </strong>%16.7f<br>\n",p->orbit_ele[5]);
  fprintf(fp, "<strong>NUM DATA PTS: </strong>%d<br>\n",p->ndata);
  fprintf(fp, "<strong>YR OF DATA PT: </strong>%d<br>\n",p->year);
  fprintf(fp, "<strong>MON OF DATA PT: </strong>%d<br>\n",p->month);
  fprintf(fp, "<strong>DAY OF DATA PT: </strong>%d<br>\n",p->day);
  fprintf(fp, "<strong>DAY IN THE YR (GMT): </strong>%d<br>\n",p->gmt_day);
  fprintf(fp, "<strong>SECS OF DAY (GMT) OF DATA: </strong>%22.15f<br>\n",p->gmt_sec);
  fprintf(fp, "<strong>INTRVL BTWN DATA PTS: </strong>%22.15f<br>\n",p->data_int);
  fprintf(fp, "<strong>REF COORD SYSTEM: </strong>%s<br>\n",p->ref_coord);
  fprintf(fp, "<strong>GREENWICH MEAN HR ANGLE: </strong>%22.15f<br>\n",p->hr_angle);
  fprintf(fp, "<strong>ALONG TRK POS ERROR: </strong>%16.7f<br>\n",p->alt_poserr);
  fprintf(fp, "<strong>CROSS TRK POS ERROR: </strong>%16.7f<br>\n",p->crt_poserr);
  fprintf(fp, "<strong>RADIAL POS ERROR: </strong>%16.7f<br>\n",p->rad_poserr);
  fprintf(fp, "<strong>ALONG TRK VEL ERROR: </strong>%16.7f<br>\n",p->alt_velerr);
  fprintf(fp, "<strong>CROSS TRK VEL ERROR: </strong>%16.7f<br>\n",p->crt_velerr);
  fprintf(fp, "<strong>RADIAL VEL ERROR: </strong>%16.7f<br>\n",p->rad_velerr);
  
  for (i=0;i<p->ndata; i++) {
    fprintf(fp, "<strong>DATA PT POS VECT #%d FOR X: </strong>%22.15f<br>\n",i+1,p->pos_vec[i][0]);
    fprintf(fp, "<strong>DATA PT POS VECT #%d FOR Y: </strong>%22.15f<br>\n",i+1,p->pos_vec[i][1]);
    fprintf(fp, "<strong>DATA PT POS VECT #%d FOR Z: </strong>%22.15f<br>\n",i+1,p->pos_vec[i][2]);
    fprintf(fp, "<strong>DATA PT VEL VECT #%d FOR X: </strong>%22.15f<br>\n",i+1,p->pos_vec[i][3]);
    fprintf(fp, "<strong>DATA PT VEL VECT #%d FOR Y: </strong>%22.15f<br>\n",i+1,p->pos_vec[i][4]);
    fprintf(fp, "<strong>DATA PT VEL VECT #%d FOR Z: </strong>%22.15f<br>\n",i+1,p->pos_vec[i][5]);
  }
  fprintf(fp, "</body>\n</html>\n");
  FCLOSE(fp);
  
  return;
}



