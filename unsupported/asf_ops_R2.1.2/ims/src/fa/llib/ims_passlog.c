static char *sccs = "@(#)ims_passlog.c	1.3  04/21/97";
/* 
**   Contains the following routines
**   sa2odl - converts SA time (double) into an ODL Date/Time String 
**   odl2sa - converts an ODL Date/Time string into a SA time (double) 
**   ims11m_counts - counts # insync & out of sync envents 
**   ims11m_times - time of 1st & last "in-sync" reports 
**   prtentry - for debugging - prints entry from 924_1
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
*/


#include <stdio.h>
#include <limits.h>
#include <math.h>
#include <time.h>

#include <ims_passlog.h>

int sa2odl ( 
   double satm,
   char *odl
   )
{
char tmpbuf[21];
int msec;
double fmsec, fsec;
time_t unixtm;
struct tm *tmptr;

   tzset();
   fmsec = modf(satm, &fsec);
   msec = (int)(fmsec * 1000.0);
   unixtm = (time_t)(satm - EPOCH_1JAN1970);
   tmptr = gmtime(&unixtm);
   tm2odl(tmptr, tmpbuf);
   tmpbuf[18] = '\0';
   sprintf (odl, "%s%03d",tmpbuf,msec);
   return(0);
}

int odl2sa ( char odl[], double *satim)
{
struct tm unxtm;
int msec;
time_t utim;

   tzset();
   odl2tm(odl,&unxtm);
   msec = atoi(&odl[18]);
   utim = mktime(&unxtm);
   utim -= timezone;
   *satim = utim + EPOCH_1JAN1970 + ((double)msec/1000);
   return (0);
}

int ims11m_counts(
   char *fname,
   int chan,
   char start_tm_str[],
   char end_tm_str[],
   int *locked,
   int *unlocked,
   int *dl_stat    /* IMS_DLCOMP | IMS_DLPART | IMS_DLFAIL */
   )
{
FILE *fp;
double start_tm, end_tm;
struct log_entry entry;
int ul_count, last_lock;

   /* default - use entire file */
   start_tm = 0.0;
   end_tm = DBL_MAX;
   *locked = 0;
   *unlocked = 0;
   ul_count = 0;
   *dl_stat = IMS_DLCOMP;

   if (NULL == (fp = fopen(fname, "r"))) return(-1);

   if ((start_tm_str != NULL) && (strlen(start_tm_str) == 21))
     odl2sa(start_tm_str, &start_tm);

   if ((end_tm_str != NULL) && (strlen(end_tm_str) == 21))
     odl2sa(end_tm_str, &end_tm);

   while ((int) fread(&entry,sizeof(entry),1,fp) > 0) 
   {
      if ((entry.vals[0] == SA924_1) &&
          ((chan < 0) || (entry.vals[1] == chan))
         )
      {
         if (entry.time <= start_tm) continue;
         if (entry.time >= end_tm) break;
         if (entry.vals[3] == LOCKED) 
         {
            *locked += 1;
            ul_count = 0;
         } else {
             ul_count++;
             if (ul_count > 9) *dl_stat = IMS_DLPART;
         }
#ifdef DBG
         prtentry(entry);
#endif
      }
   }
   fclose(fp);
   if ((*locked == 0)  && (*unlocked > 0)) *dl_stat = IMS_DLFAIL;
}



int ims11m_times(
   char *fname,
   int chan,
   char start_tm_str[],
   char end_tm_str[],
   char *lock_strt_str,
   char *lock_end_str
   )
{
FILE *fp;
double start_tm, end_tm, frst_lck, last_lck;
struct log_entry entry;

   /* default - scan entire file */
   start_tm = 0.0;
   end_tm = DBL_MAX;
   frst_lck = last_lck = 0.0;

   strcpy (lock_strt_str, "0000-000T00:00:00.000");
   strcpy (lock_end_str, "0000-000T00:00:00.000");

   if (NULL == (fp = fopen(fname, "r"))) return(-1);

   if ((start_tm_str != NULL) && (strlen(start_tm_str) == 21))
     odl2sa(start_tm_str, &start_tm);

   if ((end_tm_str != NULL) && (strlen(end_tm_str) == 21))
     odl2sa(end_tm_str, &end_tm);

   while ((int) fread(&entry,sizeof(entry),1,fp) > 0) 
   {
      if ((entry.vals[0] == SA924_1) &&
          ((chan < 0) || (entry.vals[1] == chan)))
      {
         if (entry.time <= start_tm) continue;  /* early read next */
         if (entry.time >= end_tm) break;       /* past end - finished */

         if (entry.vals[3] == LOCKED )                /* bit sync locked */
         {
            last_lck = entry.time;
            if (frst_lck < EPOCH_1JAN1970) 
               frst_lck = entry.time;
         }
#ifdef DBG
         prtentry(entry);
#endif
      }
   }
   fclose(fp);

   if (last_lck >= EPOCH_1JAN1970)              /* got lock times */
   {
      sa2odl(frst_lck, lock_strt_str);
      sa2odl(last_lck, lock_end_str);
   }
}

void prtentry(struct log_entry entry)
{
char outstr[100];
         sa2odl(entry.time, outstr);
         switch (entry.vals[1])
         { 
         case 0:
            strcat (outstr, " X1 ");
            break;
         case 1:
            strcat (outstr, " X2 ");
            break;
         case 2:
            strcat (outstr, " X3 ");
            break;
         case 3:
            strcat (outstr, " X4 ");
            break;
         default:
            strcat (outstr, " ?? ");
            break;
         }
         strcat (outstr, "Demod ");
         strcat (outstr, entry.vals[2] == 1 ? "L ":"U ");
         strcat (outstr, "BitSync ");
         strcat (outstr, entry.vals[3] == 1 ? "L":"U");
         printf ("%s\n", outstr);
}
