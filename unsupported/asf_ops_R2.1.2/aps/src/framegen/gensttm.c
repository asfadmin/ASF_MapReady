#include <stdio.h>
#include "framegen.h"

int dbg_lvl;
main(int argc, char **argv)
{
int cyccnt,
   orbcnt,
   cycday,
   cyctim;
double   temp_tm, orbtim;
int orbdy,orbhr, orbmin,iotim;
double orbsec;

int dbg_lvl;
int cyc_num, rel_rev;
int rev_cyc, days_cyc, start_rev, num_revs, nd_rev;
double time_rev, tot_tim;
unsigned long tot_sec, d_day, d_hr, d_min, rem_sec;
float d_sec;
double tm_in, tm_out;
char str_tm_in[24], str_tm_out[24], sat[3], mode[24], arctic[11];
char str1[24], str2[24], str3[24], str4[24];
double t1, t2, t3, t4;


   if (argc < 5)
   {
      printf ("Usage: %s <time> <days_cyc> <rev_cyc> <start_rev>\n",argv[0]);
      exit(0);
   }
   
   strcpy(mode,"STD");
   strcpy(arctic,"ARCTIC");

   if (argc == 5)
   {
      strcpy(sat,"??");
      strcpy(str_tm_in,argv[1]);
      days_cyc = atoi(argv[2]);
      rev_cyc = atoi(argv[3]);
      start_rev = atoi(argv[4]);
      /* start_rev = 0; */
      nd_rev = 0;
   }

   if (argc == 9)
   {
      strcpy(sat,argv[1]);
      strcpy(str_tm_in,argv[3]);
      nd_rev = atoi(argv[5]);
      days_cyc = atoi(argv[6]);
      rev_cyc = atoi(argv[7]);
      start_rev = atoi(argv[8]);
   }

   if (argc == 10)
   {
      strcpy(sat,argv[1]);
      strcpy(str_tm_in,argv[3]);
      nd_rev = atoi(argv[8]);
      days_cyc = atoi(argv[5]);
      rev_cyc = atoi(argv[6]);
      start_rev = atoi(argv[7]);
      if (argv[9][0] == 'Y') strcpy(arctic,"ANTARCTIC");
   }
   if ((days_cyc == 0) || (rev_cyc == 0))
   {
      fprintf (stderr,"Can't have 0 for days_cyc or rev_cyc\n");
      exit(0);
   }

   if (str_tm_in[4] == ':') str_tm_in[4] = '-';
   if (str_tm_in[8] == ':') str_tm_in[8] = 'T';

   odl2fg(str_tm_in, &tm_in);      /* string to time - float seconds */

   time_rev = (double) days_cyc / (double)rev_cyc;
   time_rev *= (double)SEC_DAY;

   fprintf (stderr, "Alternate Calculation\n");

   cyccnt = start_rev/rev_cyc;      /* full cycles */
   orbcnt = start_rev%rev_cyc;      /* remaining orbits */
   cycday = cyccnt * days_cyc;      /* days from cycle count */
   cyctim = cycday*SEC_DAY;         /* seconds from cycles */
   orbtim = orbcnt * time_rev;      /* seconds per orbit (float) */
   temp_tm = tm_in - cyctim;        /* time after subtracting full cycles */
   fg2odl(temp_tm, str_tm_out);
   
fprintf (stderr,"Rev/Cyc:%d Day/Cyc:%d Sec/Rev %f\n",
         rev_cyc, days_cyc, time_rev);

   fprintf (stderr, "Orbit %4d  <-> Time %s\n",start_rev,str_tm_in);
   fprintf (stderr, "Subtract %d Cycles (%d Days) & %d orbits\n",
      cyccnt, cycday, orbcnt);
   temp_tm = tm_in - cyctim;
   fg2odl(temp_tm, str_tm_out);
   fprintf (stderr, "After subtracting %d cycles ...\n",cyccnt);
   fprintf (stderr, "Orbit %4d  <-> Time %s\n",orbcnt,str_tm_out);

   iotim = (int)orbtim;

   orbdy = iotim/SEC_DAY;
   iotim %= SEC_DAY;

   orbhr = iotim/3600;
   iotim %= 3600;

   orbmin = iotim/60;
   iotim %= 60;

   iotim += ((orbdy*SEC_DAY) + (orbhr*3600) + (orbmin*60));
   orbsec = orbtim - (float)iotim;

   temp_tm -= orbtim;
   fg2odl(temp_tm, str_tm_out);
   fprintf (stderr, "After subtracting %d orbits ... (%d %d %d %f)\n",
      orbcnt, orbdy,orbhr, orbmin, orbsec);
   fprintf (stderr, "Orbit %4d  <-> Time %s\n",0,str_tm_out);

   tot_tim = time_rev * start_rev;
   tot_sec = (int)tot_tim;

   tm_in -= tot_tim;
   fg2odl(tm_in, str_tm_out);

   fprintf (stderr, "Old Calculation generates\n");
   fprintf (stderr, "Orbit %4d  <-> Time %s\n\n",0,str_tm_out);

   printf ("%s %s %d %d %d %d %s %s\n",
      sat, mode, days_cyc, rev_cyc, start_rev, nd_rev, str_tm_out,arctic);

   odl2fg(str_tm_in,&t1);
   fg2odl(t1, str1);

   odl2fg(str_tm_out, &t2);
   fg2odl(t2, str2);

}
