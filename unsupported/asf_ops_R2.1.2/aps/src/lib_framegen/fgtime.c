#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		fgtime.c

Description:	

External Functions Defined:
   fg2odl - converts FG time (double) into an ODL Date/Time String 
   odl2fg - converts an ODL Date/Time string into a FG time (double) 
   odl2tm - converts an ODL Date/Time string into a unix tm struct
   frm_tm - returns time (start, mid or end) of a specified frame 
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)fgtime.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_framegen/SCCS/s.fgtime.c"

#include <stdio.h>
#include <limits.h>
#include <math.h>
#include <time.h>

#include "framegen.h"


/*==============================================================================
Function:       odl2tm

Description:    

Parameters:     

Returns:        

Creator:        Vance A. Heron

Creation Date:  Tue Jul 23 14:36:49 PDT 1996

Notes:		
==============================================================================*/
void odl2tm(
   char *odl,
   struct tm *unxtm
)
{
int msec;
int dy_in_m[12] = {31,28,31,30,31,30,31,31,30,31,30,31};
int cur_day, cur_mon;

   sscanf(odl,"%d-%dT%d:%d:%d.%d",
      &unxtm->tm_year, &unxtm->tm_yday, 
      &unxtm->tm_hour, &unxtm->tm_min, &unxtm->tm_sec, &msec);

   if ((unxtm->tm_year % 4) == 0 && (unxtm->tm_year % 100)) 
      dy_in_m[1] = 29;

   unxtm->tm_year -= 1900;

   cur_day = unxtm->tm_yday;
   cur_mon = 0;
   while (cur_day > dy_in_m[cur_mon])
   {
      cur_day -= dy_in_m[cur_mon];
      cur_mon++;
   }
   unxtm->tm_mday = cur_day;
   unxtm->tm_mon = cur_mon;
}


/*==============================================================================
Function:      fg2odl 

Description:    

Parameters:     

Returns:        

Creator:        Vance A. Heron

Creation Date:  Tue Jul 23 14:37:07 PDT 1996

Notes:		
==============================================================================*/
int fg2odl ( 
   double fgtm,
   char *odl
   )
{
char tmpbuf[21];
int msec;
double fmsec, fsec;
time_t unixtm;
struct tm *tmptr;

   fmsec = modf(fgtm, &fsec);
   msec = (int)(fmsec * 1000.0);
   unixtm = (time_t)(fgtm);
   tmptr = gmtime(&unixtm);
   tm2odl(tmptr, tmpbuf);
   tmpbuf[18] = '\0';
   sprintf (odl, "%s%03d",tmpbuf,msec);

   return(0);
}


/*==============================================================================
Function:      odl2fg 

Description:    

Parameters:     

Returns:        

Creator:        Vance A. Heron

Creation Date:  Tue Jul 23 14:37:16 PDT 1996

Notes:		
==============================================================================*/
int odl2fg ( char odl[], double *fgtim)
{
struct tm unxtm;
int msec;
time_t utim;

   if (strlen(&odl[18]) > 3) odl[21] = '\0';
   msec = atoi(&odl[18]);
   odl2tm(odl,&unxtm);
   unxtm.tm_isdst = 0;
   utim = mktime(&unxtm);
   utim -= timezone;
   *fgtim = utim + ((double)msec/1000);
   return (0);
}


/*==============================================================================
Function:      frm_tm 

Description:    

Parameters:     

Returns:        

Creator:        Vance A. Heron

Creation Date:  Tue Jul 23 14:37:34 PDT 1996

Notes:		
==============================================================================*/
double frm_tm(
   struct orbit0_struct *orbit0, 
   int frnum,   /* Absolute frame */
   int which_tm /* START_TM, STOP_TM, or CTR_TM */
)
{
double cycle_start, rev_start, frame_time;
int rel_frm, rel_rev, rel_cycle, i;
double sec_cycle, sec_rev;
char str_time[22];

   sec_cycle = (orbit0->days_cycle * (double)SEC_DAY);
   sec_rev = sec_cycle/(double)orbit0->rev_cycle;

   i = frnum;
   rel_frm = i % FRAME_REV;
   i /= FRAME_REV;		            /* abs # revs */
   rel_rev = i % orbit0->rev_cycle;
   rel_cycle = i/orbit0->rev_cycle;
   cycle_start = orbit0->start_time + (sec_cycle * (double)rel_cycle);
   rev_start = cycle_start + (sec_rev * (float)rel_rev);
   frame_time = rev_start + orbit0->frame[rel_frm].time[which_tm];
   return(frame_time);
}
