#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       framegen.c

Description:    

External Functions Defined:
    frame_generator
    calc_frame_stat
    fill_ims_query 
    fill_ims_evt 
    read_init_file 
    read_scan_res 
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          

==============================================================================*/
#pragma ident   "@(#)framegen.c	5.3 98/03/03 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/lib_framegen/SCCS/s.framegen.c"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ims_query.h>
#include <ims_archive.h>
#include <ims_cmnQuery.h>
#include "framegen.h"

#define MAXSCANFRM 300
#define MAXLIN 100


int dbg_lvl;

static int read_scan_res(FRMGEN *fg_in, struct frame_param *scan_frm);

static void fill_ims_evt(
   IMS_CLNT_EVENT       *ims_event,
   FRMGEN               *fg_in,
   char                 *ims_fn,
   struct orbit0_struct *orbit0
);

static void fill_ims_query(
   IMS_CMN_QUERY  *query,
   IMS_CLNT_EVENT *event
);

static int read_init_file( 
   IMS_CLNT_EVENT *ims_event,
   char *orbit0fn,
   float *fg_pad,
   int *planj
);

static int calc_frame_stat(
   int frame_no,
   struct orbit0_struct *orbit0,
   int scan_cnt,
   struct frame_param *scn_frm,
   int tpc,
   char *dtk_stat,
   double dt_start,
   double dt_end,
   double *tp_start,
   double *tp_end,
   char *out_stat,
   float pad
);

static char *extensions[] = {"M"};


/*==============================================================================
Function:      frame_generator 

Description:    

Parameters:     

Returns:        

Creator:        Vance A. Heron

Creation Date:  Tue Jul 23 14:58:26 PDT 1996

Notes:      
==============================================================================*/
int frame_generator(FRMGEN *fg_in)
{
double dt_start, dt_end,        /* input start & end times fg fmt */
       fg_start, fg_end,        /* overall start & end times fg fmt */
       tp_start[MAX_TIMEPAIR],
       tp_end[MAX_TIMEPAIR],
       cycle_start_time;        /* start time of "cycle" */
double delta_time, time_cycle;
double time_rev, time_frame;
double frame_start, frame_end;
float fg_pad;
int start_frame, end_frame, cur_frame;
int rel_frame, rel_rev, fg_cycle;
int i, stat, skip;

int planj;      /* TRUE/FALSE insert "planned" frames for J */
int scan_cnt;
struct frame_param scn_frm[MAXSCANFRM];


struct orbit0_struct orbit0;
IMS_CLNT_EVENT ims_event;
IMS_CMN_QUERY  ims_query;

char new_frm_stat[10];
char pmffn[121], pmfbase[100];
char orbit0fn[100];
char string1[22], string2[22];
char *dar_string;

   if ((strcmp(fg_in->platform,"J1") == 0) &&
       (strncmp(fg_in->datatake_status,"SCHEDULED",9) == 0)) 
      return (FG_OK);

   if (read_init_file(&ims_event, orbit0fn, &fg_pad, &planj) != 0)
      return (FG_FATAL);

   if (ims_event.sourceDir == NULL) 
   {
      if (dbg_lvl > 0) fprintf (stderr, "Missing FG_PATH\n");
      return (FG_FATAL);
   }

   if (fg_in->sensor != 'S')
   {
      if (dbg_lvl > 0)
         fprintf (stderr,"Sensor value %c  - FG requires 'S'\n",
            fg_in->sensor);
      return(FG_FATAL);
   }

   if (dbg_lvl < 2)
      ims_msgStderrFlag(fg_in->msgDesc, IMS_OFF);


   /* calc overall begin & end times */
   /* start & end of input datatake */
   odl2fg(fg_in->start_time, &dt_start);
   odl2fg(fg_in->end_time, &dt_end);

   fg_start = dt_start;
   fg_end = dt_end;

   /* check timepairs - if they exist */
   for (i=0; i<fg_in->time_pairs_count; i++)
   {
      odl2fg(fg_in->time_pairs[i].start_time, &tp_start[i]);
      odl2fg(fg_in->time_pairs[i].end_time, &tp_end[i]);
      if (tp_start[i] >= tp_end[i])
      {
         if (dbg_lvl > 0)
         {
            fprintf (stderr, "time pair %d start > end\n",i+1);
            fprintf (stderr, "  strt %s  end %s\n",
               fg_in->time_pairs[i].start_time,
               fg_in->time_pairs[i].end_time
            );
         }
         return(FG_FATAL);
      }

      if (fg_start > tp_start[i]) fg_start = tp_start[i];
      if (fg_end < tp_end[i]) fg_end = tp_end[i];
   }


   /* get orbit 0 frames */

   strcat (orbit0fn,"/");
   strcat (orbit0fn,fg_in->platform);
   strcat (orbit0fn,fg_in->mode);
   strcat (orbit0fn,"FRM.TXT");

   stat = get_orbit0(orbit0fn, fg_in->platform, fg_in->mode,  
                     fg_in->revolution, &orbit0);
   if (stat == -1)
   {
      if (dbg_lvl > 0)
         fprintf (stderr, "%s(%d):  Error getting Orbit0 params\n", 
             __FILE__, __LINE__ );
      return(FG_FATAL);
   }

   /* calc absolute start & end frames */

   time_cycle = (double)orbit0.days_cycle * (double)SEC_DAY;
   time_rev = (double)orbit0.days_cycle/(double)orbit0.rev_cycle;
   time_rev *= (double)SEC_DAY;
   time_frame = time_rev/(double)FRAME_REV; /* approximate */

   delta_time = fg_start - orbit0.start_time;   
   fg_cycle = (int) (delta_time/time_cycle);

   cycle_start_time = orbit0.start_time + ((double) fg_cycle * time_cycle);

   delta_time -= (double)fg_cycle * time_cycle;
   rel_rev = (int) (delta_time/time_rev);

   delta_time -= (double)rel_rev * time_rev;

   start_frame = (int) (delta_time/time_frame);
   start_frame += rel_rev * FRAME_REV;
   start_frame += fg_cycle * (FRAME_REV * orbit0.rev_cycle);

   delta_time = fg_end - fg_start;
   end_frame = start_frame + (int) (delta_time/time_frame);

   while(frm_tm(&orbit0, start_frame, START_TM) > fg_start) start_frame--;
   while(frm_tm(&orbit0, start_frame, START_TM) < fg_start+fg_pad) 
      start_frame++;
   while (frm_tm(&orbit0, end_frame, END_TM) > fg_end-fg_pad) end_frame--;


   scan_cnt = 0;
   if (strncmp(fg_in->datatake_status,"SCANNED",7) == 0)
   {
      scan_cnt = read_scan_res(fg_in, scn_frm);

      if (scan_cnt < 0) return(scan_cnt);

      if (scan_cnt > 0)
      {
         if ((scn_frm[0].frame_id < start_frame) || 
            (strcmp(fg_in->platform,"J1") == 0))
         start_frame = scn_frm[0].frame_id;

         if ((scn_frm[scan_cnt-1].frame_id > end_frame) || 
            (strcmp(fg_in->platform,"J1") == 0))
         end_frame = scn_frm[scan_cnt-1].frame_id;
      }
   }

   if (start_frame > end_frame)
      return(FG_INPUT_ERROR);


   /* for each frame */
   for (cur_frame = start_frame; cur_frame <= end_frame;  cur_frame++)
   {
      if (strcmp(fg_in->platform,"J1") == 0)
      {
         /* if "sched" - break out of loop - should not happen */
         if (strncmp(fg_in->datatake_status,"SCHEDULED",9) == 0) break;

         /* if "planned" & !planj - break out of loop */
         if ((strncmp(fg_in->datatake_status,"PLANNED",7) == 0) &&
             (planj == FALSE)) break;
      }

      /*  gen pmf file name */
      sprintf (pmfbase,"%s_%05d_%s_%03d",
         fg_in->platform,  /* platform */
         cur_frame/900,    /* rev */
         fg_in->mode,      /* mode */
         (cur_frame % FRAME_REV)+1       /* frame id */
      );
      strcpy(pmffn,ims_event.sourceDir);
      strcat (pmffn,"/");
      strcat (pmffn,pmfbase);
      strcat (pmffn,".M");
       
      /* scansar mode - do 1st, last, and every Nth (3 or 5) */

      skip = ((strncmp(fg_in->mode,"SW",2) == 0) && 
              (strcmp(fg_in->platform,"R1") == 0) &&
              (cur_frame % 5) != 0);

      skip |= ((strncmp(fg_in->mode,"SN",2) == 0) && 
              (strcmp(fg_in->platform,"R1") == 0) &&
              (cur_frame % 3) != 0);

      /* 
	  -- what if the scanned frame count == 0 
	  -- i.e., if the status is PLANNED?
	  -- there is then an uninitialized memory 
	  -- read on the test of 
	  -- scn_frm[].frame_id
	  */


      skip &= !(cur_frame == start_frame ||
                cur_frame == end_frame   ||
                cur_frame == scn_frm[0].frame_id ||
                cur_frame == scn_frm[scan_cnt-1].frame_id) ;


      if (skip) continue;

      /* calc "status" */

      calc_frame_stat(cur_frame, &orbit0, scan_cnt, scn_frm, 
         fg_in->time_pairs_count, fg_in->datatake_status, 
         dt_start, dt_end, tp_start, tp_end, new_frm_stat,fg_pad);
 
      if ((strcmp(fg_in->platform,"J1") == 0) &&
          (strcmp(fg_in->datatake_status,"MISSED") == 0)) continue;

      if (strcmp(new_frm_stat,"NONE") == 0) continue;

      if (strcmp(new_frm_stat,"SCANNED") == 0) 
      {
         for (i=0; i<scan_cnt; i++) 
            if (scn_frm[i].frame_id == cur_frame) break;
         scn_frm[i].ascdec = orbit0.frame[(cur_frame%FRAME_REV)].ascdec;
         stat = write_pmf(pmffn, fg_in, &scn_frm[i],
            orbit0.frame_mode, new_frm_stat);
      } else {
         stat = gen_pmf(pmffn, fg_in, &orbit0, cur_frame, new_frm_stat);
      }
             
      if (stat < 0)
      {
         if (dbg_lvl > 0) fprintf (stderr, "%s(%d):  Error opening pmf file %s\n",
            __FILE__, __LINE__, pmffn);
         return(FG_FATAL);
      }

      /* update ims status */ 
      fill_ims_evt(&ims_event, fg_in, pmfbase, &orbit0);
      if (dbg_lvl > 2)
         fprintf (stderr, "%s(%d):  calling ims_archive - frm %d %s\n", 
            __FILE__, __LINE__, cur_frame, new_frm_stat);

      stat = ims_archive(&ims_event);

      if (dbg_lvl < 2)
         remove(pmffn);

      if (stat != IMS_OK) 
         return(stat);
   }

   stat = FG_OK;    /* initialize - if last status was "NONE" */

#ifndef  SGI
   /* 220 characters for each frame entry, see darframe.c */
   dar_string = malloc( (end_frame-start_frame+1) * 220 ) ;

   /* if first entry - map frames to dar */
   if ((fg_in->time_pairs_count == 0) && 
       (strcmp(fg_in->datatake_status,"PLANNED")== 0))
   {
        /* 
        --  do not call if the darid = 0; this 
        --  causes an error return 
        */
        if (fg_in->dar_id != 0 )
        {
            if (!planj) 
                fill_ims_evt(&ims_event, fg_in, "dummy", &orbit0 );
            darframe(fg_in, start_frame, end_frame, dar_string);
            fill_ims_query(&ims_query, &ims_event);
            ims_openQueryConnection(&ims_query);
            stat = ims_darFrame(&ims_query, dar_string );
            ims_closeQueryConnection(&ims_query);
        }
   }
   free (dar_string);
#endif
   return (stat);
}


/*==============================================================================
Function:      calc_frame_stat 

Description:    

Parameters:     

Returns:        

Creator:        Vance A. Heron

Creation Date:  Tue Jul 23 14:59:08 PDT 1996

Notes:      
==============================================================================*/
static int calc_frame_stat(
   int frame_no,
   struct orbit0_struct *orbit0,
   int scan_cnt,
   struct frame_param *scn_frm,
   int tpc,     /* frm fg_in */
   char *dtk_stat,  /* frm fg_in */
   double dt_start, /* old time */
   double dt_end,   
   double *tp_start, 
   double *tp_end,
   char *out_stat,
   float fg_pad
)
{
int i, in_dt, in_tp;
double frame_start, frame_end;


char str_time[22];

   frame_start = frm_tm(orbit0, frame_no, START_TM);
   frame_end = frm_tm(orbit0, frame_no, END_TM);

   /* find if frame is "in" datatake time and in a time pair */

   in_dt = (((frame_start + (double)fg_pad) >= dt_start) &&
            ((frame_end - (double)fg_pad) <= dt_end));

  
   in_tp = FALSE;

   if (strcmp(dtk_stat,"SCANNED") == 0) /* in_tp means in scan res file */
   {
      tpc=scan_cnt;
      for (i=0; i<tpc && !in_tp; i++)
      {
         in_tp |= (scn_frm[i].frame_id == frame_no);
      }
   } else {
      for (i=0; i<tpc && !in_tp; i++)
      {
         in_tp |= (((frame_start + (double)fg_pad) >= tp_start[i])  &&
                ((frame_end - (double)fg_pad) <= tp_end[i]));
      }
   }
   strcpy(out_stat,"NONE");    /* for no update needed */

   if ((tpc == 0) && (in_dt))  /* legal values of stat PLN,SCH,REJ,SCN */
   {
      if ((strcmp(dtk_stat,"PLANNED") == 0) ||
          (strcmp(dtk_stat,"SCHEDULED") == 0) ||
          (strcmp(dtk_stat,"REJECTED") == 0))
      strcpy(out_stat, dtk_stat);

      if (strcmp(dtk_stat,"SCANNED") == 0)
         strcpy(out_stat, "MISSED");
   }


   if (tpc > 0)   /* legal values SCN, SCH, PLN, (ACQ) */
   {
      if ((strcmp(dtk_stat,"SCANNED") == 0) ||
          (strcmp(dtk_stat,"ACQUIRED") == 0))
      {
         if (in_tp) strcpy(out_stat, "SCANNED");
         else if (in_dt) strcpy(out_stat, "MISSED");  /* & not in tp */
      }

      if ((strcmp(dtk_stat,"SCHEDULED") == 0) ||
          (strcmp(dtk_stat,"PLANNED") == 0))
      {
         if (in_dt) 
         { 
            strcpy(out_stat, dtk_stat);
         } else if (in_tp) {              /* implied !in_dt */
            strcpy(out_stat, "REJECTED");  
         }
      }
   }
   return(0);
}


/*==============================================================================
Function:      fill_ims_query 

Description:    

Parameters:     

Returns:        

Creator:        Vance A. Heron

Creation Date:  Tue Jul 23 14:59:56 PDT 1996

Notes:      
==============================================================================*/
static void fill_ims_query(
IMS_CMN_QUERY  *query,
IMS_CLNT_EVENT *event
)
{
   query->qDesc = NULL;
   if (event->username != NULL)
      strcpy(query->username, event->username);
   if (event->password != NULL)
      strcpy(query->password, event->password);
   if (event->catSrvName != NULL)
      strcpy(query->server, event->catSrvName);
   if (event->catDbName != NULL)
      strcpy(query->database, event->catDbName);
   query->retStatus = IMS_OK;
   if (event->programName != NULL)
      strcpy(query->program, event->programName);
   query->msgDesc = event->msgDesc;
   query->retPtr = (char *) NULL;
}


/*==============================================================================
Function:      fill_ims_evt 

Description:    

Parameters:     

Returns:        

Creator:        Vance A. Heron

Creation Date:  Tue Jul 23 15:00:23 PDT 1996

Notes:      
==============================================================================*/
static void fill_ims_evt(
   IMS_CLNT_EVENT       *ims_event,
   FRMGEN               *fg_in,
   char                 *ims_fn,
   struct orbit0_struct *orbit0
)
{
    static char ims_platform[20], ims_sensor[20];
    char buf[128] ;
    int  frame_offset ;

    /* args  */
    ims_event->name = ims_fn;

    /* from fg struct */
    if (strcmp(fg_in->platform,"E1") == 0)
    {
        strcpy(ims_platform,"ERS-1");
        ims_event->dataset = "ERS-1 SAR FRAMES";
    }

    if (strcmp(fg_in->platform,"E2") == 0)
    {
        strcpy(ims_platform,"ERS-2");
        ims_event->dataset = "ERS-2 SAR FRAMES";
    }

    if (strcmp(fg_in->platform,"J1") == 0)
    {
        strcpy(ims_platform,"JERS-1");
        ims_event->dataset = "JERS-1 SAR FRAMES";
    }

    if (strcmp(fg_in->platform,"R1") == 0)
    {
        strcpy(ims_platform,"RADARSAT-1");
        if (strncmp(fg_in->mode,"ST",2) == 0)
            ims_event->dataset = "RADARSAT-1 STANDARD BEAM FRAMES";
        if (strncmp(fg_in->mode,"WD",2) == 0)
            ims_event->dataset = "RADARSAT-1 WIDE BEAM FRAMES";
        if (strncmp(fg_in->mode,"FN",2) == 0)
            ims_event->dataset = "RADARSAT-1 FINE RESOLUTION FRAMES";
        if (strncmp(fg_in->mode,"EH",2) == 0)
            ims_event->dataset = "RADARSAT-1 HIGH INCIDENCE FRAMES";
        if (strcmp(fg_in->mode,"EL1") == 0)
            ims_event->dataset = "RADARSAT-1 LOW INCIDENCE FRAMES";
        if ((strncmp(fg_in->mode,"SW",2) == 0) 
        ||  (strncmp(fg_in->mode,"SN",2) == 0)   )
            ims_event->dataset = "RADARSAT-1 SCANSAR FRAMES";
    }

    if( strcmp(orbit0->frame_mode, "ANTARCTIC") == 0 )
    {
        /* strfind() is in libgen   */
        if ( -1 == strfind(ims_event->dataset,"LEFT LOOKING")  )
        if ( -1 != (frame_offset = strfind(ims_event->dataset,"FRAMES"))   )
        {
            /* 
            -- In the first if() statement, we established that 
            -- "LEFT LOOKING" was NOT in the string 
            -- ims_event->dataset.  (There had been a problem 
            -- with this being done more than once.)
            --
            -- In the second if() statement, we established that 
            -- frame_offset points to "FRAMES", which is within 
            -- ims_event->dataset.  
            -- 
            -- Now insert "LEFT LOOKING " just before "FRAMES"
            -- into the dataset name because this is antarctic mode.  
            -- Before:  "RADARSAT-1 FINE RESOLUTION FRAMES"
            -- After:   "RADARSAT-1 FINE RESOLUTION LEFT LOOKING FRAMES"
            */
            /* 
            -- antarctic mode.   
            -- copy the first part of dataset into a buffer, 
            -- then "LEFT LOOKING ", then the rest 
            -- of that dataset.  then copy buf back to 
            -- ims_event->dataset[]
            */
            buf[0] = '\0' ;
            strncat(buf, ims_event->dataset, frame_offset ) ;
            strcat( buf, "LEFT LOOKING " ) ;
            strcat( buf, (ims_event->dataset)+frame_offset ) ;
            strcpy( ims_event->dataset, buf ) ;
        }
    }

    strcpy(ims_sensor,"SAR");   /* input checked earlier */

    ims_event->platform = ims_platform;
    ims_event->msgDesc = fg_in->msgDesc;
    ims_event->sensor = ims_sensor;

    /* fixed values */
    ims_event->extensions = extensions;
    ims_event->requestType = IMS_REPLACE;
    ims_event->format = "PMF";
    ims_event->version = -1;
    ims_event->fileCount = 1;
    ims_event->localArchiveFlag = 'N';
    ims_event->programName = "FG";

/* this value is now seen in the config file FG_CONFIG ;
   it probably would now be bad to null it out.  
   The value is already set when reading the config file.
    ims_event->ftsSrvName = NULL;
*/

}


/*==============================================================================
Function:      read_init_file 

Description:    

Parameters:     

Returns:        

Creator:        Vance A. Heron

Creation Date:  Tue Jul 23 15:00:54 PDT 1996

Notes:      
==============================================================================*/
static int read_init_file(
   IMS_CLNT_EVENT *ims_event,
   char *orbit0fn,
   float *fgpad,
   int *planj
)
{
static char ims_user[100], ims_pwd[100], ims_acnt[100], 
   fts_server[100], ims_server[100], ims_db[100], ims_dir[100];
char configfn[100], *tmpptr;
FILE *fp;
char linein[80], key[80], val[80];

   tmpptr = getenv("FG_CONFIG");

   if (tmpptr == NULL)
      return(-1);
   
   strcpy(configfn,tmpptr);

   if ((fp = fopen(configfn, "r")) == NULL)
   {
      sprintf (linein, "Error opening config file %s",configfn);
      perror (linein);
      return(-1);
   }

   ims_event->username = NULL;
   ims_event->password = NULL;
   ims_event->accountId = NULL;
   ims_event->catDbName = NULL;
   ims_event->catSrvName = NULL;
   ims_event->sourceDir = NULL;;
   dbg_lvl = 0;
   *planj = FALSE;

   while (fgets(linein,80,fp) !=NULL)
   {
      tmpptr = strchr(linein,'#');         /* remove comment */
      if (tmpptr != NULL) *tmpptr = '\0';

      tmpptr = strchr(linein,'\n');        /* remove trailing newline */
      if (tmpptr != NULL) *tmpptr = '\0';

      tmpptr = strchr(linein,'=');         /* need <key>=<value> */
      if (tmpptr == NULL) continue;
      *tmpptr = (char)' ';

      sscanf(linein,"%s %s",key,val);

      if (strcmp(key,"FG_DBG") == 0)
      {
         dbg_lvl=atoi(val);
      }

      if (strcmp(key,"FG_PLANJ") == 0)
      {
         *planj = (strcmp(val,"YES") == 0);
      }

      if (strcmp(key,"FG_PAD") == 0)
         *fgpad = atof(val);

      if (strcmp(key,"FG_FRMPATH") == 0)
         strcpy(orbit0fn,val);

      if (strcmp(key,"FG_USER") == 0)
      {
         strcpy(ims_user,val);
         ims_event->username = ims_user;
      }

      if (strcmp(key,"FG_PASSWORD") == 0)
      {
         strcpy(ims_pwd,val);
         ims_event->password = ims_pwd;
      }

      if (strcmp(key,"FG_ACCOUNT") == 0)
      {
         strcpy(ims_acnt,val);
         ims_event->accountId = ims_acnt;
      }

      if (strcmp(key,"FG_FTS") == 0)
      {
         strcpy(fts_server,val);
         ims_event->ftsSrvName = fts_server;
      }
      if (strcmp(key,"FG_SERVER") == 0)
      {
         strcpy(ims_server,val);
         ims_event->catSrvName = ims_server;
      }
      if (strcmp(key,"FG_DB") == 0)
      {
         strcpy(ims_db,val);
         ims_event->catDbName = ims_db;
      }
      if (strcmp(key,"FG_PATH") == 0)
      {
         strcpy(ims_dir,val);
         ims_event->sourceDir = ims_dir;
      }
   }

   fclose (fp) ;
   return(0);
}


/*==============================================================================
Function:      read_scan_res 

Description:    

Parameters:     

Returns:        

Creator:        Vance A. Heron

Creation Date:  Tue Jul 23 15:01:18 PDT 1996

Notes:      
==============================================================================*/
static int read_scan_res(FRMGEN *fg_in, struct frame_param *scan_frm)
{
FILE *srfp;
char srfn[100], linein[MAXLIN+1], *cp, key[30], val[200];
int rev, frm, i, in_frm;

   cp = strchr(fg_in->scan_results_file,':');

   if (cp != NULL) 
   {
      strcpy(srfn,&cp[1]); 
   } else {
      strcpy(srfn,fg_in->scan_results_file); 
   }
   
   if ((srfp = fopen(srfn,"r")) == NULL)
   {
      return(FG_INPUT_ERROR);
   }

   i = 0; in_frm = FALSE;
   for (;;)
   {
      fgets (linein,MAXLIN,srfp);
      if (feof(srfp)) break;

      cp = strchr(linein,'=');
      if (cp == NULL) continue;
      *cp = (char)' '; 
      sscanf(linein,"%s %s",key, val);

      if (!in_frm)
         in_frm = ((strcmp(key,"OBJECT") == 0) &&
                   (strcmp(val,"FRAME") == 0));

      if (!in_frm) continue;        /* else equiv */
                   
      if (strcmp(key,"FRAME_ID") == 0)
         frm = atoi(val);

      else if (strcmp(key,"REVOLUTION") == 0)
         rev = atoi(val);

      else if (strcmp(key,"START_TIME") == 0)
         odl2fg (val, &scan_frm[i].time[START_TM]);
      else if (strcmp(key,"END_TIME") == 0)
         odl2fg (val, &scan_frm[i].time[END_TM]);
      else if (strcmp(key,"CENTER_TIME") == 0)
         odl2fg (val, &scan_frm[i].time[CTR_TM]);

      else if (strcmp(key,"NEAR_START_LAT") == 0)
         scan_frm[i].lat[NEAR_START] = atof(val);
      else if (strcmp(key,"FAR_START_LAT") == 0)
         scan_frm[i].lat[FAR_START] = atof(val);
      else if (strcmp(key,"NEAR_END_LAT") == 0)
         scan_frm[i].lat[NEAR_END] = atof(val);
      else if (strcmp(key,"FAR_END_LAT") == 0)
         scan_frm[i].lat[FAR_END] = atof(val);
      else if (strcmp(key,"CENTER_LAT") == 0)
         scan_frm[i].lat[CENTER] = atof(val);

      else if (strcmp(key,"NEAR_START_LON") == 0)
         scan_frm[i].lon[NEAR_START] = atof(val);
      else if (strcmp(key,"FAR_START_LON") == 0)
         scan_frm[i].lon[FAR_START] = atof(val);
      else if (strcmp(key,"NEAR_END_LON") == 0)
         scan_frm[i].lon[NEAR_END] = atof(val);
      else if (strcmp(key,"FAR_END_LON") == 0)
         scan_frm[i].lon[FAR_END] = atof(val);
      else if (strcmp(key,"CENTER_LON") == 0)
         scan_frm[i].lon[CENTER] = atof(val);

      else if ((strcmp(key,"END_OBJECT") == 0) &&
          (strcmp(val,"FRAME") == 0))
      {
         scan_frm[i].frame_id = frm + (rev * FRAME_REV) - 1 ;
         i++;
         in_frm = FALSE;
      }

      if (i == MAXSCANFRM) break;
   }
   fclose(srfp);
   return(i);
}
