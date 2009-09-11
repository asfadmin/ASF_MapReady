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

char *sprn_ppdr(struct pos_data_rec *p)
{
  int i;

  char *ret = MALLOC(sizeof(char)*1);
  strcpy(ret, "");
  
  /* Write the contents of the Platform Postion Data record  */
  
  add(&ret, "\n************** begin of Platform Position record *************\n");
  add(&ret, "\n ORBITAL ELMTS DESG\t\t%s",p->orbit_ele_desg);
  add(&ret, "\n ORBITAL ELEMENT 1\t\t%16.7f",p->orbit_ele[0]);
  add(&ret, "\n ORBITAL ELEMENT 2\t\t%16.7f",p->orbit_ele[1]);
  add(&ret, "\n ORBITAL ELEMENT 3\t\t%16.7f",p->orbit_ele[2]);
  add(&ret, "\n ORBITAL ELEMENT 4\t\t%16.7f",p->orbit_ele[3]);
  add(&ret, "\n ORBITAL ELEMENT 5\t\t%16.7f",p->orbit_ele[4]);
  add(&ret, "\n ORBITAL ELEMENT 6\t\t%16.7f",p->orbit_ele[5]);
  add(&ret, "\n NUM DATA PTS\t\t\t%d",p->ndata);
  add(&ret, "\n YR OF DATA PT\t\t\t%d",p->year);
  add(&ret, "\n MON OF DATA PT\t\t\t%d",p->month);
  add(&ret, "\n DAY OF DATA PT\t\t\t%d",p->day);
  add(&ret, "\n DAY IN THE YR (GMT)\t\t%d",p->gmt_day);
  add(&ret, "\n SECS OF DAY (GMT) OF DATA\t%22.15f",p->gmt_sec);
  add(&ret, "\n INTRVL BTWN DATA PTS\t\t%22.15f",p->data_int);
  add(&ret, "\n REF COORD SYSTEM\t\t%s",p->ref_coord);
  add(&ret, "\n GREENWICH MEAN HR ANGLE\t%22.15f",p->hr_angle);
  add(&ret, "\n ALONG TRK POS ERROR\t\t%16.7f",p->alt_poserr);
  add(&ret, "\n CROSS TRK POS ERROR\t\t%16.7f",p->crt_poserr);
  add(&ret, "\n RADIAL POS ERROR\t\t%16.7f",p->rad_poserr);
  add(&ret, "\n ALONG TRK VEL ERROR\t\t%16.7f",p->alt_velerr);
  add(&ret, "\n CROSS TRK VEL ERROR\t\t%16.7f",p->crt_velerr);
  add(&ret, "\n RADIAL VEL ERROR\t\t%16.7f",p->rad_velerr);
  
  for (i=0;i<p->ndata; i++) {
    add(&ret, "\n DATA PT POS VECT #%d FOR X\t%22.15f",i+1,p->pos_vec[i][0]);
    add(&ret, "\n DATA PT POS VECT #%d FOR Y\t%22.15f",i+1,p->pos_vec[i][1]);
    add(&ret, "\n DATA PT POS VECT #%d FOR Z\t%22.15f",i+1,p->pos_vec[i][2]);
    add(&ret, "\n DATA PT VEL VECT #%d FOR X\t%22.15f",i+1,p->pos_vec[i][3]);
    add(&ret, "\n DATA PT VEL VECT #%d FOR Y\t%22.15f",i+1,p->pos_vec[i][4]);
    add(&ret, "\n DATA PT VEL VECT #%d FOR Z\t%22.15f",i+1,p->pos_vec[i][5]);
  }
  add(&ret, "\n************** end of Platform Position record ***************\n");
  
  return ret;
}

void prn_ppdr(FILE *fp, struct pos_data_rec *p)
{
    char *rec = sprn_ppdr(p);
    fprintf(fp, "%s", rec);
    FREE(rec);
}
