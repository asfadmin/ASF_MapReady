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

void prn_ppdr(struct pos_data_rec *p)
{
     int i;

/* Write the contents of the Platform Postion Data record  */

printf("\n************** begin of Platform Position record ****************\n");
printf("\n ORBITAL ELMTS DESG\t\t%s",p->orbit_ele_desg);
printf("\n ORBITAL ELEMENT 1\t\t%16.7f",p->orbit_ele[0]);
printf("\n ORBITAL ELEMENT 2\t\t%16.7f",p->orbit_ele[1]);
printf("\n ORBITAL ELEMENT 3\t\t%16.7f",p->orbit_ele[2]);
printf("\n ORBITAL ELEMENT 4\t\t%16.7f",p->orbit_ele[3]);
printf("\n ORBITAL ELEMENT 5\t\t%16.7f",p->orbit_ele[4]);
printf("\n ORBITAL ELEMENT 6\t\t%16.7f",p->orbit_ele[5]);
printf("\n NUM DATA PTS\t\t\t%d",p->ndata);
printf("\n YR OF DATA PT\t\t\t%d",p->year);
printf("\n MON OF DATA PT\t\t\t%d",p->month);
printf("\n DAY OF DATA PT\t\t\t%d",p->day);
printf("\n DAY IN THE YR (GMT)\t\t%d",p->gmt_day);
printf("\n SECS OF DAY (GMT) OF DATA\t%22.15f",p->gmt_sec);
printf("\n INTRVL BTWN DATA PTS\t\t%22.15f",p->data_int);
printf("\n REF COORD SYSTEM\t\t%s",p->ref_coord);
printf("\n GREENWICH MEAN HR ANGLE\t%22.15f",p->hr_angle);
printf("\n ALONG TRK POS ERROR\t\t%16.7f",p->alt_poserr);
printf("\n CROSS TRK POS ERROR\t\t%16.7f",p->crt_poserr);
printf("\n RADIAL POS ERROR\t\t%16.7f",p->rad_poserr);
printf("\n ALONG TRK VEL ERROR\t\t%16.7f",p->alt_velerr);
printf("\n CROSS TRK VEL ERROR\t\t%16.7f",p->crt_velerr);
printf("\n RADIAL VEL ERROR\t\t%16.7f",p->rad_velerr);

for (i=0;i<p->ndata; i++) {
  printf("\n DATA PT POS VECT #%d FOR X\t%22.15f",i+1,p->pos_vec[i][0]);
  printf("\n DATA PT POS VECT #%d FOR Y\t%22.15f",i+1,p->pos_vec[i][1]);
  printf("\n DATA PT POS VECT #%d FOR Z\t%22.15f",i+1,p->pos_vec[i][2]);
  printf("\n DATA PT VEL VECT #%d FOR X\t%22.15f",i+1,p->pos_vec[i][3]);
  printf("\n DATA PT VEL VECT #%d FOR Y\t%22.15f",i+1,p->pos_vec[i][4]);
  printf("\n DATA PT VEL VECT #%d FOR Z\t%22.15f",i+1,p->pos_vec[i][5]);
}
printf("\n************** end of Platform Position record *****************\n");

return;
}



