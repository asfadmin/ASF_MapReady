/********************************************************************
NAME:     prn_atdr.c --  print attitude data record values

PROGRAM HISTORY:
VERSION         DATE   AUTHOR
-------         ----   ------
  1.0            4/96   T. Logan (ASF)   Borrowed from JPL ceos reader
  1.1		 9/96   T. Logan (ASF)   Modified to write multiple points

*********************************************************************/
#include "asf.h"
#include "ceos.h"
#include "metadisplay.h"

void prn_atdr(struct att_data_rec* a)
{
 struct att_vect_rec *d;
 int j = 1;

 /* Write the contents of the Attitude Data record  */
 printf("\n*************** begin of Attitude Data record ******************\n");
 printf("\n NUM OF POINTS\t\t\t%d",a->npoint);
 d = a->data;

 for (j = 1; j<=(a->npoint); j++)
  {
   printf("\n POINT NUMBER\t\t\t%i",j);
   printf("\n DAY IN THE YEAR (GMT)\t\t%d",d->gmt_day);
   printf("\n MS OF THE DAY (GMT)\t\t%d",d->gmt_msec);
   printf("\n PITCH DATA QUAL FLAG\t\t%d",d->pitch_flag);
   printf("\n ROLL DATA QUAL FLAG\t\t%d",d->roll_flag);
   printf("\n YAW DATA QUAL FLAG\t\t%d",d->yaw_flag);
   printf("\n PITCH\t\t\t\t%14.6f",d->pitch);
   printf("\n ROLL\t\t\t\t%14.6f",d->roll);
   printf("\n YAW\t\t\t\t%14.6f",d->yaw);
   printf("\n PITCH RATE QUAL FLAG\t\t%d",d->pitch_rate_flag);
   printf("\n ROLL RATE QUAL FLAG\t\t%d",d->roll_rate_flag);
   printf("\n YAW RATE QUAL FLAG\t\t%d",d->yaw_rate_flag);
   printf("\n PITCH RATE\t\t\t%14.6f",d->pitch_rate);
   printf("\n ROLL RATE\t\t\t%14.6f",d->roll_rate);
   printf("\n YAW RATE\t\t\t%14.6f",d->yaw_rate);
   printf("\n END OF POINT\t\t\t%i\n",j);
   d = d->next;
  }
 printf("\n**************** end of Attitude Data record *******************\n");
 return;
}

