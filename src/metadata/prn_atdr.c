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

char *sprn_atdr(struct att_data_rec* a)
{
 char *ret = MALLOC(sizeof(char)*1);
 strcpy(ret, "");

 struct att_vect_rec *d;
 int j = 1;

 /* Write the contents of the Attitude Data record  */
 add(&ret, "\n*************** begin of Attitude Data record ****************\n");
 add(&ret, "\n NUM OF POINTS\t\t\t%d",a->npoint);
 d = a->data;

 for (j = 1; j<=(a->npoint); j++)
  {
   add(&ret, "\n POINT NUMBER\t\t\t%i",j);
   add(&ret, "\n DAY IN THE YEAR (GMT)\t\t%d",d->gmt_day);
   add(&ret, "\n MS OF THE DAY (GMT)\t\t%d",d->gmt_msec);
   add(&ret, "\n PITCH DATA QUAL FLAG\t\t%d",d->pitch_flag);
   add(&ret, "\n ROLL DATA QUAL FLAG\t\t%d",d->roll_flag);
   add(&ret, "\n YAW DATA QUAL FLAG\t\t%d",d->yaw_flag);
   add(&ret, "\n PITCH\t\t\t\t%14.6f",d->pitch);
   add(&ret, "\n ROLL\t\t\t\t%14.6f",d->roll);
   add(&ret, "\n YAW\t\t\t\t%14.6f",d->yaw);
   add(&ret, "\n PITCH RATE QUAL FLAG\t\t%d",d->pitch_rate_flag);
   add(&ret, "\n ROLL RATE QUAL FLAG\t\t%d",d->roll_rate_flag);
   add(&ret, "\n YAW RATE QUAL FLAG\t\t%d",d->yaw_rate_flag);
   add(&ret, "\n PITCH RATE\t\t\t%14.6f",d->pitch_rate);
   add(&ret, "\n ROLL RATE\t\t\t%14.6f",d->roll_rate);
   add(&ret, "\n YAW RATE\t\t\t%14.6f",d->yaw_rate);
   add(&ret, "\n END OF POINT\t\t\t%i\n",j);
   d = d->next;
  }
 add(&ret, "\n**************** end of Attitude Data record *****************\n");
 return ret;
}

void prn_atdr(FILE *fp, struct att_data_rec* a)
{
    char *rec = sprn_atdr(a);
    fprintf(fp, "%s", rec);
    FREE(rec);
}
