#ifndef _FRAMEGEN_H
#define _FRAMEGEN_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:	framegen.h	
Description:	
Creator:	Vance A. Heron

Notes:		
==============================================================================*/
#pragma ident	"@(#)framegen.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/include/global/SCCS/s.framegen.h"


#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void odl2tm(
   char *odl,
   struct tm *unxtm
);

#define tm2odl(a,b) strftime(b, sizeof(b) ,"%Y-%jT%T.000",a);

int odl2fg ( char odl[], double *fgtm);
int fg2odl ( double fgtm, char *odl);


#define FG_OK	         1 /* IMS_OK */
#define FG_INPUT_ERROR	-2 /* IMS_ERROR */
#define FG_FATAL        -3 /* IMS_FATAL */

#define ATIMELEN 22

#define FRAME_REV 900

#define NEAR_START  0
#define NEAR_END    1
#define FAR_START   2
#define FAR_END     3
#define CENTER      4

#define START_TM    0
#define END_TM      1
#define CTR_TM      2

#define MAX_TIMEPAIR 150
#define SEC_DAY (60*60*24)
#define FRAME_REV 900
#define FUDGE 1.5       /* seconds from edges */

/*
//	define structure for time pairs called TimePair
*/
typedef struct tagTimePair {
	char	start_time[ATIMELEN];	/* formatted date/time string */
	char	end_time[ATIMELEN];		/* formatted date/time string */
} TimePair;

/*
//	define structure to be used to send data to frame_generator
*/
typedef struct tagFrmGen {
	void	*msgDesc;      /* IMS_MSG_STRUCT ptr */
	char	platform[3];   /* char(2) */
	long	revolution;    /* domain 1:99999 */
	short	sequence;      /* domain 1:99 */
	char	sensor;        /* char */
	char	mode[4];       /* char(3) */
	char	activity_id[4];/* char(3) */
	long	dar_id;
	char	start_time[ATIMELEN];   /* formatted date/time string */
	char	end_time[ATIMELEN];     /* formatted date/time string */
	char *	frame_mode;             /* domain: "ARCTIC", "ANTARCTIC" */
	char	station_id[3];          /* char(2), domain: "FA" */
	char *	site_name;              
	char *	datatake_status;        /* domain: PLN, SCHED, ACQ, REJ */
	char *	media_id;               /* optional */
	char *	scan_results_file;	/* <hostname>:</filespec>, optional: set to NULL if not used */
	short	time_pairs_count;	/* number of TIME PAIRS */
	TimePair * time_pairs;		/* array of TimePair structures */
} FRMGEN;

struct frame_param {
   int frame_id;
   double time[3];
   float lat[5];
   float lon[5];
   int    ascdec;
};

struct orbit0_struct {
   char platform[3];
   char mode[4];
   char frame_mode[10];  /* ARCTIC | ANTARCTIC */
   int days_cycle;
   int rev_cycle;
   int start_rev;
   int end_rev;
   double start_time;
   struct frame_param frame[FRAME_REV];
};

/* declare function prototype */
int frame_generator( FRMGEN * frame_gen );
int  get_orbit0(char *map_table, char *platform, char *mode,  long revolution,
                     struct orbit0_struct *orbit0);

int gen_pmf(
   char *pmffn, 
   FRMGEN *fg_in, 
   struct orbit0_struct *orbit0, 
   int cur_frame, 
   char *frame_status
);

int darframe(
   FRMGEN *frmgen, 
   int start_frame,
   int end_frame,
   char *dar_string
);

double frm_tm(
   struct orbit0_struct *orbit0, 
   int frnum,
   int which_tm
);

int write_pmf(
   char *pmffn, 
   FRMGEN *fg_in, 
   struct frame_param *frm,
   char *frame_mode,
   char *frame_status
);

extern int dbg_lvl;
#endif	/* !_FRAMEGEN_H */
