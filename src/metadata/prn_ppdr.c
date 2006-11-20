/********************************************************************
NAME:     prn_ppdr.c --  print platform position data record values

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0            3/96   T. Logan (ASF)   Borrowed from JPL ceos reader
*********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

void prn_ppdr(FILE *fp, struct pos_data_rec *p)
{
  int i;
  
  /* Write the contents of the Platform Postion Data record  */
  
  fprintf(fp, "\n************** begin of Platform Position record *************\n");
  fprintf(fp, "\n ORBITAL ELMTS DESG\t\t%s",p->orbit_ele_desg);
  fprintf(fp, "\n ORBITAL ELEMENT 1\t\t%16.7f",p->orbit_ele[0]);
  fprintf(fp, "\n ORBITAL ELEMENT 2\t\t%16.7f",p->orbit_ele[1]);
  fprintf(fp, "\n ORBITAL ELEMENT 3\t\t%16.7f",p->orbit_ele[2]);
  fprintf(fp, "\n ORBITAL ELEMENT 4\t\t%16.7f",p->orbit_ele[3]);
  fprintf(fp, "\n ORBITAL ELEMENT 5\t\t%16.7f",p->orbit_ele[4]);
  fprintf(fp, "\n ORBITAL ELEMENT 6\t\t%16.7f",p->orbit_ele[5]);
  fprintf(fp, "\n NUM DATA PTS\t\t\t%d",p->ndata);
  fprintf(fp, "\n YR OF DATA PT\t\t\t%d",p->year);
  fprintf(fp, "\n MON OF DATA PT\t\t\t%d",p->month);
  fprintf(fp, "\n DAY OF DATA PT\t\t\t%d",p->day);
  fprintf(fp, "\n DAY IN THE YR (GMT)\t\t%d",p->gmt_day);
  fprintf(fp, "\n SECS OF DAY (GMT) OF DATA\t%22.15f",p->gmt_sec);
  fprintf(fp, "\n INTRVL BTWN DATA PTS\t\t%22.15f",p->data_int);
  fprintf(fp, "\n REF COORD SYSTEM\t\t%s",p->ref_coord);
  fprintf(fp, "\n GREENWICH MEAN HR ANGLE\t%22.15f",p->hr_angle);
  fprintf(fp, "\n ALONG TRK POS ERROR\t\t%16.7f",p->alt_poserr);
  fprintf(fp, "\n CROSS TRK POS ERROR\t\t%16.7f",p->crt_poserr);
  fprintf(fp, "\n RADIAL POS ERROR\t\t%16.7f",p->rad_poserr);
  fprintf(fp, "\n ALONG TRK VEL ERROR\t\t%16.7f",p->alt_velerr);
  fprintf(fp, "\n CROSS TRK VEL ERROR\t\t%16.7f",p->crt_velerr);
  fprintf(fp, "\n RADIAL VEL ERROR\t\t%16.7f",p->rad_velerr);
  
  for (i=0;i<p->ndata; i++) {
    fprintf(fp, "\n DATA PT POS VECT #%d FOR X\t%22.15f",i+1,p->pos_vec[i][0]);
    fprintf(fp, "\n DATA PT POS VECT #%d FOR Y\t%22.15f",i+1,p->pos_vec[i][1]);
    fprintf(fp, "\n DATA PT POS VECT #%d FOR Z\t%22.15f",i+1,p->pos_vec[i][2]);
    fprintf(fp, "\n DATA PT VEL VECT #%d FOR X\t%22.15f",i+1,p->pos_vec[i][3]);
    fprintf(fp, "\n DATA PT VEL VECT #%d FOR Y\t%22.15f",i+1,p->pos_vec[i][4]);
    fprintf(fp, "\n DATA PT VEL VECT #%d FOR Z\t%22.15f",i+1,p->pos_vec[i][5]);
  }
  fprintf(fp, "\n************** end of Platform Position record ***************\n");
  
  return;
}
