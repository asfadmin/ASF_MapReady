/******************************************************************************
**
** File:      ims_passlog.h
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
******************************************************************************/

#ifndef PASSLOG_H
#define PASSLOG_H

static char *sccsPassLog = "@(#)ims_passlog.h	1.4  04/25/97";


#include <time.h>

#define SECONDS_PER_DAY  86400
#define DAYSTO1JAN1970   719528.0
#define DAYSTOZEROAD     1721060.0
#define EPOCH_1JAN1970   (((double)DAYSTOZEROAD + \
                           (double)DAYSTO1JAN1970)*(double)SECONDS_PER_DAY)

#define SA924_1 5
#define LOCKED 0
#define UNLOCKED 1

#define IMS_DLCOMP 0
#define IMS_DLPART 1
#define IMS_DLFAIL -1

#define odl2tm(a,b) strptime(a,"%Y-%jT%T",b);
#define tm2odl(a,b) strftime(b, sizeof(b) ,"%Y-%jT%T.000",a);

struct log_entry {
   double time;
   int  vals[8];
};

int odl2sa ( char odl[], double *satm);
int sa2odl ( double satm, char *odl);

int ims11m_counts(
   char *fname,
   int chan,
   char start_tm[],
   char end_tm[],
   int *locked,
   int *unlocked,
   int *dlstat
   );

int ims11m_times(
   char *fname,
   int chan,
   char start_tm[],
   char end_tm[],
   char *frst_lck_str,
   char *last_lck_str
   );

void prtentry(struct log_entry entry);



#endif
