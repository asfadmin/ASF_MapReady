#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		genpmf.c

Description:	

External Functions Defined:
	gen_pmf
	write_pmf
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)genpmf.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_framegen/SCCS/s.genpmf.c"

#include <stdio.h>
#include <stdlib.h>
#include <time.h> 
#include <math.h>

#include "framegen.h"


/*==============================================================================
Function:      gen_pmf 

Description:    

Parameters:     

Returns:        

Creator:        Vance A. Heron

Creation Date:  Tue Jul 23 15:04:21 PDT 1996

Notes:		
==============================================================================*/
int gen_pmf(
   char *pmffn, 
   FRMGEN *fg_in, 
   struct orbit0_struct *orbit0, 
   int cur_frame, 
   char *frame_status
)
{
int i, rel_frame, rel_rev;
int frame_cycle, fg_cycle;
double time_cycle, time_rev, cycle_start_time;
double delta_lon, rev_start;
struct frame_param frm;

   rel_rev = cur_frame/FRAME_REV;		/* abs rev */
   rel_rev -= orbit0->start_rev;

   time_cycle = (double)orbit0->days_cycle * (double)SEC_DAY;
   time_rev = time_cycle/(double)orbit0->rev_cycle;
   frame_cycle = orbit0->rev_cycle * FRAME_REV;
   fg_cycle = cur_frame /frame_cycle;

   cycle_start_time = orbit0->start_time + ((double) fg_cycle * time_cycle);

   rev_start = cycle_start_time + ((double)rel_rev * time_rev);
   delta_lon = orbit0->days_cycle * (-360.0);/* deg per cycle */
   delta_lon /= orbit0->rev_cycle;           /* deg per rev */
   delta_lon *= rel_rev;                     /* deg for this rev */
   delta_lon = fmod(delta_lon,360.0);
   if (dbg_lvl > 2)
      fprintf (stderr, "%s(%d):  rev = %d delta_lon = %6.2f\n",
               __FILE__, __LINE__, rel_rev, delta_lon);

   rel_frame = cur_frame % FRAME_REV;

   frm.frame_id=cur_frame;
   frm.ascdec = orbit0->frame[rel_frame].ascdec;
   for (i=0; i<3 ; i++)
      frm.time[i] = frm_tm(orbit0,cur_frame,i);

   for (i=0; i<5; i++) /* nr/far, start/end + ctr  lat & lon */
   {
      frm.lat[i] = orbit0->frame[rel_frame].lat[i];
      frm.lon[i] = orbit0->frame[rel_frame].lon[i] + delta_lon;
      while (frm.lon[i] > 180.0)  frm.lon[i] -= 360.0;
      while (frm.lon[i] < -180.0) frm.lon[i] += 360.0;
   }
   i = write_pmf(pmffn, fg_in, &frm, orbit0->frame_mode, 
                 frame_status);
   return(i);
}


/*==============================================================================
Function:      write_pmf 

Description:    

Parameters:     

Returns:        

Creator:        Vance A. Heron

Creation Date:  Tue Jul 23 15:04:39 PDT 1996

Notes:		
==============================================================================*/
int write_pmf(
   char *pmffn, 
   FRMGEN *fg_in, 
   struct frame_param *frm,
   char *frame_mode,
   char *frame_status
)
{
char time_str[3][22], today_str[22];
int i, cur_frame;
FILE *fp;
time_t now;

   if ((fp = fopen(pmffn,"w")) == NULL)
   {
      return(-1);
   }

   for (i=0; i<3 ; i++)
      fg2odl(frm->time[i], time_str[i]);
   cur_frame = frm->frame_id;

   fputs("OBJECT = FRAME_METADATA\n", fp);
   /* common header */
   fputs("OBJECT = COMMON_HEADER\n", fp);


   now = time(NULL);
   tm2odl(gmtime(&now), today_str);
   fprintf(fp, "TIME = %s\n", today_str);

   fputs("MSG_TYPE = \"FRAME_METADATA\"\n", fp);
   fputs("NUMBER_OF_RECORDS = 1\n", fp);
   fputs("SOURCE = \"FG\"\n", fp);
   fputs("DESTINATION = \"IMS\"\n", fp);
   fputs("END_OBJECT = COMMON_HEADER\n", fp);

   /* catalog metadata */
   fputs("OBJECT = CATALOG_METADATA\n", fp);
   fprintf(fp,"PLATFORM = \"%s\"\n", fg_in->platform);
   fprintf(fp,"REVOLUTION = %d\n", cur_frame/FRAME_REV);
   fprintf(fp,"FRAME_ID = %d\n",(cur_frame % FRAME_REV)+1); /* 1-900 v 0-899 */
   fprintf(fp,"SEQUENCE = %d\n", fg_in->sequence);
   fprintf(fp,"SENSOR = \"%c\"\n", fg_in->sensor);
   fprintf(fp,"MODE = \"%.3s\"\n", fg_in->mode);
   fprintf(fp,"ACTIVITY_ID = \"%.3s\"\n", fg_in->activity_id);

   fprintf(fp,"ASC_DESC = \"%c\"\n", frm->ascdec);

   fprintf(fp,"START_TIME = %s\n", time_str[START_TM]);
   fprintf(fp,"END_TIME = %s\n", time_str[END_TM]);
   fprintf(fp,"CENTER_TIME = %s\n", time_str[CTR_TM]);
   fprintf(fp,"CENTER_LAT = %-16.7f\n", frm->lat[CENTER]);
   fprintf(fp,"NEAR_START_LAT = %-16.7f\n", frm->lat[NEAR_START]);
   fprintf(fp,"NEAR_END_LAT = %-16.7f\n", frm->lat[NEAR_END]);
   fprintf(fp,"FAR_START_LAT = %-16.7f\n",frm->lat[FAR_START]);
   fprintf(fp,"FAR_END_LAT = %-16.7f\n", frm->lat[FAR_END]);

   fprintf(fp,"CENTER_LON = %-16.7f\n", frm->lon[CENTER]);
   fprintf(fp,"NEAR_START_LON = %-16.7f\n", frm->lon[NEAR_START]);
   fprintf(fp,"NEAR_END_LON = %-16.7f\n", frm->lon[NEAR_END]);
   fprintf(fp,"FAR_START_LON = %-16.7f\n",frm->lon[FAR_START]);
   fprintf(fp,"FAR_END_LON = %-16.7f\n", frm->lon[FAR_END]);

   if (frame_mode != NULL)
      fprintf(fp,"FRAME_MODE = \"%s\"\n", frame_mode);
   fprintf(fp,"STATION_ID = \"%s\"\n", fg_in->station_id);
   fprintf(fp,"FRAME_STATUS = \"%s\"\n", frame_status);

   if (fg_in->media_id !=  NULL) 
      fprintf(fp, "MEDIA_ID = \"%s\"\n", fg_in->media_id);

   if (fg_in->scan_results_file != NULL) 
      fprintf(fp, "SCAN_RESULTS_FILE = \"%s\"\n", fg_in->scan_results_file);

   fputs("END_OBJECT = CATALOG_METADATA\n", fp);
   fputs("END_OBJECT = FRAME_METADATA\n", fp);
   fputs("END\n\n", fp);

   fclose(fp);
   return 0;
}
