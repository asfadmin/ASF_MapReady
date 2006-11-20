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

void prn_atdr(FILE *fp, struct att_data_rec* a)
{
 struct att_vect_rec *d;
 int j = 1;

 /* Write the contents of the Attitude Data record  */
 fprintf(fp, "\n*************** begin of Attitude Data record ****************\n");
 fprintf(fp, "\n NUM OF POINTS\t\t\t%d",a->npoint);
 d = a->data;

 for (j = 1; j<=(a->npoint); j++)
  {
   fprintf(fp, "\n POINT NUMBER\t\t\t%i",j);
   fprintf(fp, "\n DAY IN THE YEAR (GMT)\t\t%d",d->gmt_day);
   fprintf(fp, "\n MS OF THE DAY (GMT)\t\t%d",d->gmt_msec);
   fprintf(fp, "\n PITCH DATA QUAL FLAG\t\t%d",d->pitch_flag);
   fprintf(fp, "\n ROLL DATA QUAL FLAG\t\t%d",d->roll_flag);
   fprintf(fp, "\n YAW DATA QUAL FLAG\t\t%d",d->yaw_flag);
   fprintf(fp, "\n PITCH\t\t\t\t%14.6f",d->pitch);
   fprintf(fp, "\n ROLL\t\t\t\t%14.6f",d->roll);
   fprintf(fp, "\n YAW\t\t\t\t%14.6f",d->yaw);
   fprintf(fp, "\n PITCH RATE QUAL FLAG\t\t%d",d->pitch_rate_flag);
   fprintf(fp, "\n ROLL RATE QUAL FLAG\t\t%d",d->roll_rate_flag);
   fprintf(fp, "\n YAW RATE QUAL FLAG\t\t%d",d->yaw_rate_flag);
   fprintf(fp, "\n PITCH RATE\t\t\t%14.6f",d->pitch_rate);
   fprintf(fp, "\n ROLL RATE\t\t\t%14.6f",d->roll_rate);
   fprintf(fp, "\n YAW RATE\t\t\t%14.6f",d->yaw_rate);
   fprintf(fp, "\n END OF POINT\t\t\t%i\n",j);
   d = d->next;
  }
 fprintf(fp, "\n**************** end of Attitude Data record *****************\n");
 return;
}

