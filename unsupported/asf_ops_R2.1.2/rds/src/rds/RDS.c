/*=============================================================================
 |  @(#)RDS.c	1.160 98/04/06 10:12:30
 |
 |  Multithreaded Raw Data Scanner (RDS).
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "asf_syslog.h"
#include "asf.h"
#include "Object.h"
#include "Symbol.h"
#include "Ident.h"
#include "Time.h"
#include "Int.h"
#include "RDS.h"

static const char sccsid_RDS_c[] =
        "@(#)RDS.c	1.160 98/04/06 10:12:30";

/*-----------------*
 |  Timing Module
 *-----------------*/
#include "Timer.h"

struct timeval
    sca_timer, sca_time,
    ppr_timer, ppr_time,
    sum_timer, sum_time,
    wrt_timer, wrt_time;

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *============================================================================*/
static
int Tape_LastBuf(TapeBuf_t* p)
{
    return (p->job->end.blk == p->blk_start + p->blk_count - 1);
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *============================================================================*/
static
U8* Tape_Buffer(TapeBuf_t* *p, int blk_start, int bit_start)
{
    const Job_t* job = (*p)->job;
    const int n32 = RoundUp(((Satellite_t*)job->sat)->frame_size_max, 4);
    const int len = (n32+job->blk_size-1)/job->blk_size;
    const int read_num = (*p)->blk_start;
    register int n;

    if (blk_start == 0 && bit_start == 0)
	return (*p)->buf;

    blk_start += bit_start / (job->blk_size*8);
    bit_start %= job->blk_size*8;

    while (*p && (blk_start >= (n = (*p)->blk_start + (*p)->blk_count) ||
		 (blk_start >= n-len && bit_start > (job->blk_size*len-n32)*8)))
	(*p) = (*p)->next;

    if ((*p) == NULL) {
	(*job->Log)(job->err, "TapeBuf #%d: can't find %d/%.5d\n",
		    read_num, blk_start, bit_start);
	return NULL;
    }
    return ((*p)->buf + (blk_start - (*p)->blk_start) * job->blk_size +
			(bit_start >> 3));
}

/*============================================================================*
 |  NAME
 |      get_scan_params - load the RDS scan parameters
 |
 |  SYPNOSIS
 |      int get_scan_params(Job_t* job)
 |
 |          job - ptr to job parameter structure
 |
 |          returns - 0 for no error,
 |                    else error
 |
 |  DESCRIPTION
 |      This routine loads the RDS processing input SCAN parameters.
 |
 *============================================================================*/
static
int get_scan_params(Job_t* job)
{
    GMT end_time = job->end_time;
    register char *s;
    static char START_TIME[]	= "START_TIME";
    static char END_TIME[]	= "END_TIME";
    static char SV_TIME[]	= "STATE_VECTOR_DATA.TIME";
     
    /*------------------------------------------------------------------------*
     |  Needed for PPR corner calculation but not specified in proc. request
     *------------------------------------------------------------------------*/
    gmt_add(&end_time, job->gmt_diff_max);

    if ((s = START_TIME,
	 gmt_diff(&job->start_time, &job->stv.t0) < -STV_SPACING*1.5) ||
	(s = END_TIME,
	 gmt_diff(&end_time, &job->stv.t0) > STV_MAX*STV_SPACING))

	return (*job->Log)(job->err,"%s is too far from %s\n", s, SV_TIME);

    if (! stv_init(&job->stv, &job->stv.t0, job->stv.sv[0]))
	return (*job->Log)(job->err,
	    "Can't initialize statevector propagation\n");

    return 0;
}

/*============================================================================*
 |  NAME
 |      get_scan_results
 |
 |  SYPNOSIS
 |      int get_scan_results(Job_t* job)
 |
 |          job - ptr to job parameter structure
 |
 |          returns - 0 for no error,
 |                    else error
 |
 |  DESCRIPTION
 |      This routine extracts START_BLOCK and END_BLOCK of specified frame
 |      in given scan results file.
 |
 *============================================================================*/
static
int get_scan_results(Job_t* job)
{
    char errstr[256];
    GMT_t *timep;
    ODL scan_result_file, odl, segmt, frame, *body_contents, *segmt_contents;
    int err, i, k;
    char *p;

    p = (p = strchr(job->sca_file, ':')) ? p+1 : job->sca_file;

    if (! (scan_result_file = ODLparse(p, 0, errstr)))
	return (*job->Log)(job->err, "%s: %s\n", p, errstr);

    if (Type(odl = Lookup(scan_result_file, "SCAN_RESULTS_FILE.BODY"))
	!= (Method_t) Object) {
	ODLFree(scan_result_file);
	return (*job->Log)(job->err, "%s: bad format\n", job->sca_file);
    }
    body_contents = Val(odl = Value(odl));

    if (Type(odl = Value(Lookup(odl, "START_ADDRESS"))) != (Method_t) Int) {
	ODLFree(scan_result_file);
	return (*job->Log)(job->err, "%s: can't find START_ADDRESS\n",
			   job->sca_file);
    }
    job->dsk_start = *(int*) Val(odl);

    for (i = 0; body_contents[i] != NULL; ++i) {

	if (strcasecmp(Name(segmt = body_contents[i]), "SEGMENT") ||
	    Type(segmt) != (Method_t) Object)
	    continue;

	segmt_contents = Val(segmt);

	for (k = 0; segmt_contents[k] != NULL; ++k) {
	    if (strcasecmp(Name(frame = segmt_contents[k]), "FRAME") ||
		Type(frame) != (Method_t) Object)
		continue;
	    
	    if (job->frame_id != ODLGetInt(frame, "FRAME_ID", &err) ||
		err == -1)
		continue;

	    if (job->start.blk = ODLGetInt(frame, "START_ADDRESS", &err),
		err == -1 || job->start.blk < 0) {

		ODLFree(scan_result_file);
		return (*job->Log)(job->err,
		    "Frame %d: Invalid or missing START_ADDRESS\n",
		    job->frame_id);
	    }
	    if (job->start.bit = ODLGetInt(frame, "START_BIT_OFFSET", &err),
		err == -1 || job->start.bit < 0) {

		ODLFree(scan_result_file);
		return (*job->Log)(job->err,
		    "Frame %d: Invalid or missing START_BIT_OFFSET\n",
		    job->frame_id);
	    }
	    if (job->end.blk = ODLGetInt(frame, "END_ADDRESS", &err),
		err == -1 || job->end.blk <= job->start.blk) {

		ODLFree(scan_result_file);
		return (*job->Log)(job->err,
		    "Frame %d: Invalid or missing END_ADDRESS\n",
		    job->frame_id);
	    }
	    if (job->end.bit = ODLGetInt(frame, "END_BIT_OFFSET", &err),
		err == -1) {
		job->end.bit = job->blk_size*8-1;
	    }
	    /*--------------------*
	     |  Check START_TIME
	     *--------------------*/
	    odl = Value(Lookup(frame, "START_TIME"));
	    if (Type(odl) != (Method_t) Time || !(timep = (GMT_t*) Val(odl)))
		return (*job->Log)(job->err,
		"Frame %d: Invalid or missing START_TIME\n", job->frame_id);
#ifdef	DEBUG
	    job->t0 = *timep;
#endif
	    job->start_time.yr  = timep->tm.tm_year+1900;
	    job->start_time.day = timep->tm.tm_yday+1;
	    job->start_time.hr  = timep->tm.tm_hour;
	    job->start_time.min = timep->tm.tm_min;
	    job->start_time.second = timep->tm.tm_sec + timep->tv_usec/1.0E6;

	    /*------------------*
	     |  Check END_TIME 
	     *------------------*/
	    odl = Value(Lookup(frame, "END_TIME"));
	    if (Type(odl) != (Method_t) Time || !(timep = (GMT_t*) Val(odl)))
		return (*job->Log)(job->err,
		"Frame %d: Invalid or missing END_TIME\n", job->frame_id);

	    job->end_time.yr  = timep->tm.tm_year+1900;
	    job->end_time.day = timep->tm.tm_yday+1;
	    job->end_time.hr  = timep->tm.tm_hour;
	    job->end_time.min = timep->tm.tm_min;
	    job->end_time.second = timep->tm.tm_sec + timep->tv_usec/1.0E6;

	    if (job->end_time.day*24*3600 + job->end_time.hr*3600 +
		job->end_time.min*60 + (double) job->end_time.second <=
		job->start_time.day*24*3600 + job->start_time.hr*3600 +
		job->start_time.min*60 + (double) job->start_time.second)
		return (*job->Log)(job->err,
		"Frame %d: START_TIME must be < END_TIME\n", job->frame_id);
	    return 0;

	} /* Next ODL in segment */
    } /* Next ODL in BODY */

    return (*job->Log)(job->err, "can't find frame %d in %s\n",
			job->frame_id, job->sca_file);
}

/*============================================================================*
 |  NAME
 |      get_decode_params - load DECODE parameters
 |
 |  SYPNOSIS
 |      int get_decode_params(Job_t* job)
 |
 |          job - ptr to job parameter structure
 |
 |          returns - 0 for no error,
 |                    else error
 |
 |  DESCRIPTION
 |      This routine loads the RDS processing input DECODE parameters.
 |
 *============================================================================*/
static
int get_decode_params(Job_t* job)
{
    static char OUTPUT_FORMAT[]	= "BODY.OUTPUT_FORMAT";
    static char FRAME_ID[]	= "BODY.FRAME_ID";
    static char ECO_FILE[]	= "BODY.ECHO_DATA_FILE";
    static char AUX_FILE[]	= "BODY.AUXILIARY_DATA_FILE";
    static char REP_FILE[]	= "BODY.REPLICA_DATA_FILE";
    static char EPH_FILE[]	= "BODY.EPHEMERIS_DATA_FILE";
    static char BOF_FILE[]	= "BODY.BURST_OFFSET_DATA_FILE";
    int i, err;
    char *s;
    ODL msg = (ODL) job->msg;

    /*------------------*
     |  Check FRAME_ID
     *------------------*/
    job->frame_id = ODLGetInt(msg, FRAME_ID, &err);
    if (err == -1 || job->frame_id < 0)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(FRAME_ID, '.')+1);

    /*------------------------*
     |  Check OUTPUT_FORMAT
     *------------------------*/
    s = ODLGetStr(msg, OUTPUT_FORMAT);
    if (s == NULL || strcmpv(s, "ODL", "CEOS", "HDF", 0) < 0)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(OUTPUT_FORMAT, '.')+1);
    strcpy(job->output_fmt, s);

    /*------------------------*
     |  Check ECHO DATA FILE
     *------------------------*/
    s = ODLGetStr(msg, ECO_FILE);
    if (s == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(ECO_FILE, '.')+1);
    if (i = strlen(s), i >= sizeof(job->eco_file))
	return (*job->Log)(job->err, "%s filename too long, max %d\n",
		strchr(ECO_FILE, '.')+1, sizeof(job->eco_file)-1);
    memcpy(job->eco_file, s, i+1);

    /*-----------------------------*
     |  Check AUXILIARY DATA FILE
     *-----------------------------*/
    s = ODLGetStr(msg, AUX_FILE);
    if (s == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(AUX_FILE, '.')+1);
    if (i = strlen(s), i >= sizeof(job->aux_file))
	return (*job->Log)(job->err, "%s filename too long, max %d\n",
		strchr(AUX_FILE, '.')+1, sizeof(job->aux_file)-1);
    memcpy(job->aux_file, s, i+1);

    /*--------------------------------*
     |  Check BURST OFFSET DATA FILE
     *--------------------------------*/
    s = ODLGetStr(msg, BOF_FILE);
    if (s == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(BOF_FILE, '.')+1);
    if (i = strlen(s), i >= sizeof(job->bof_file))
	return (*job->Log)(job->err, "%s filename too long, max %d\n",
		strchr(BOF_FILE, '.')+1, sizeof(job->bof_file)-1);
    memcpy(job->bof_file, s, i+1);

    /*---------------------------*
     |  Check REPLICA DATA FILE
     *---------------------------*/
    s = ODLGetStr(msg, REP_FILE);
    if (s == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(REP_FILE, '.')+1);
    if (i = strlen(s), i >= sizeof(job->rep_file))
	return (*job->Log)(job->err, "%s filename too long, max %d\n",
		strchr(REP_FILE, '.')+1, sizeof(job->rep_file)-1);
    memcpy(job->rep_file, s, i+1);

    /*-----------------------------*
     |  Check EPHEMERIS DATA FILE
     *-----------------------------*/
    s = ODLGetStr(msg, EPH_FILE);
    if (s == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(EPH_FILE, '.')+1);
    if (i = strlen(s), i >= sizeof(job->eph_file))
	return (*job->Log)(job->err, "%s filename too long, max %d\n",
		strchr(EPH_FILE, '.')+1, sizeof(job->eph_file)-1);
    memcpy(job->eph_file, s, i+1);

    return get_scan_results(job);
}

/*============================================================================*
 |  NAME
 |      RDS_GetJob -
 |
 |  SYPNOSIS
 |      int RDS_GetJob (Job_t* job, ODL msg)
 |
 |          job - ptr to job parameter structure
 |          msg - message from cp
 |
 |          returns - 0 for no error,
 |                    else error
 |
 |  DESCRIPTION
 |      This routine loads the RDS processing input parameters sent from CP.
 |
 *============================================================================*/
static
int RDS_GetJob (Job_t* job, ODL msg)
{
    static char MSG_TYPE[]		= "COMMON_HEADER.MSG_TYPE";
    static char SOURCE[]		= "COMMON_HEADER.SOURCE";
    static char DESTIN[]		= "COMMON_HEADER.DESTINATION";
    static char TIME[]			= "COMMON_HEADER.TIME";
    static char NUMBER_OF_RECORDS[]	= "COMMON_HEADER.NUMBER_OF_RECORDS";

    static char JOB_ID[]		= "BODY.JOB_ID";
    static char SCAN_RESULTS_FILE[]	= "BODY.SCAN_RESULTS_FILE";
    static char PLATFORM[]		= "BODY.PLATFORM";
    static char SENSOR[]		= "BODY.SENSOR";
    static char REV[]			= "BODY.REVOLUTION";
    static char SEQ[]			= "BODY.SEQUENCE";
    static char ACTIVITY[]		= "BODY.ACTIVITY_ID";
    static char MEDIA_ID[]		= "BODY.MEDIA_ID";
    static char MEDIA_TYPE[]		= "BODY.MEDIA_TYPE";
    static char MEDIA_LOCATION[]	= "BODY.MEDIA_LOCATION";
    static char CHECK_MEDIA_ID[]	= "BODY.CHECK_MEDIA_ID";
    static char DATA_DIRECTION[]	= "BODY.DATA_DIRECTION";

    static char START_BLOCK[]		= "BODY.START_ADDRESS";
    static char END_BLOCK[]		= "BODY.END_ADDRESS";
    static char START_TIME[]		= "BODY.START_TIME";
    static char END_TIME[]		= "BODY.END_TIME";
    static char RECORDER_ID[]		= "BODY.RECORDER_ID";
    static char STATION_ID[]		= "BODY.STATION_ID";
    static char MODE[]			= "BODY.MODE";
    static char FRAME_MODE[]		= "BODY.FRAME_MODE";
    static char SITE_NAME[]		= "BODY.SITE_NAME";

    static char GHA_TIME[]		= "BODY.GHA_CORRECTION.TIME";
    static char GHA_ANGLE[]		= "BODY.GHA_CORRECTION.ANGLE";
    static char TC_REV[]		= "BODY.TIME_CORRELATION.REVOLUTION";
    static char TC_TIME[]		= "BODY.TIME_CORRELATION.TIME";
    static char TC_SAT_TIME[]		= "BODY.TIME_CORRELATION.PLATFORM_TIME";
    static char TC_CLK_CYCLE[]		= "BODY.TIME_CORRELATION.CLOCK_CYCLE";

    static char SV_SAT[]		= 
	  "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.PLATFORM";
    static char SV_TYPE[]		= 
	"BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.STATE_VECTOR_PRECISION";
    static char SV_COORD_SYS[]		= 
	  "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.COORDINATE_SYSTEM";
    static char SV_REV[]			= 
	  "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.REVOLUTION";
    static char SV_TIME[]		= 
	  "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.TIME";
    static char SV_POS_X[]		= 
	  "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_POSITION";
    static char SV_POS_Y[]		= 
	  "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_POSITION";
    static char SV_POS_Z[]		= 
	  "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_POSITION";
    static char SV_VEL_X[]		= 
	  "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.X_VELOCITY";
    static char SV_VEL_Y[]		= 
	  "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Y_VELOCITY";
    static char SV_VEL_Z[]		= 
	  "BODY.STATE_VECTOR_RECORD.STATE_VECTOR_DATA.Z_VELOCITY";

    char *s;
    int err, i;
    GMT_t *timep;
    ODL odl;
    job->msg = (void*) msg;	/* Save the processing request for later use */

    /*  Need to clear this otherwise cleanup may botch up previous job */
    job->sca_file[0] = job->aux_file[0] = job->eco_file[0] = 
    job->rep_file[0] = job->eph_file[0] = job->bof_file[0] = 0;

    /*----------------*
     |  Check JOB_ID
     *----------------*/
    job->job_id = ODLGetInt(msg, JOB_ID, &err);
    if (err == -1)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(JOB_ID, '.')+1);

    /*------------------*
     |  Check MSG_TYPE
     *------------------*/
    s = ODLGetStr(msg, MSG_TYPE);
    if (s == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(MSG_TYPE, '.')+1);

    if (! strcmp(s, "SPS_SCAN_REQUEST"))
	job->scan_job = 1;
    else if (! strcmp(s, "SPS_DECODE_REQUEST"))
	job->scan_job = 0;
    else
	return (*job->Log)(job->err, "invalid %s\n",
		strchr(MSG_TYPE, '.')+1);

    /*------------------*
     |  Check PLATFORM
     *------------------*/
    s = ODLGetStr(msg, PLATFORM);
    if (s == NULL || strlen(s) != 2 ||
    	strcmpv(s, "R1", "E1", "E2", "J1", 0) < 0) {
	strcpy(job->platform, "XX");
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(PLATFORM, '.')+1);
    }	
    memcpy(job->platform, s, 3);
    job->sat = (void*)
       (s[0]=='R' ? (job->sync_diff = job->r_sync_diff, &R1) :
	s[0]=='J' ? (job->sync_diff = job->j_sync_diff, &J1) :
		    (job->sync_diff = job->e_sync_diff, s[1]=='1' ? &E1 : &E2));
    /*-------------*
     |  Check REV
     *-------------*/
    job->rev = ODLGetInt(msg, REV, &err);
    if (err == -1 || job->rev < 0)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(REV, '.')+1);

    if (job->scan_job) {
	for (i = 0; i < LOOKTABLE_MAX; ++i)
	    if (looktable[i].rmin <= job->rev && job->rev <= looktable[i].rmax)
		break;
	if (i == LOOKTABLE_MAX)
	    return (*job->Log)(job->err,
		"Nominal look angles for revolution %d missing\n", job->rev);
    }
    /*--------------*
     |  Check MODE 
     *--------------*/
    s = ODLGetStr(msg, MODE);
    if (s == NULL || strlen(s) != 3 ||
	(i = strcmpv(s, "ST1","ST2","ST3","ST4","ST5","ST6","ST7",
			"SWA","SWB","SNA","SNB",
			"WD1","WD2","WD3",
			"FN1","FN2","FN3","FN4","FN5",
			"EL1","EH1","EH2","EH3","EH4","EH5","EH6",
			"STD", 0)) < 0) {
	job->mode = MODE_INVALID;
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(MODE, '.')+1);
    }
    job->mode = (i == ES1 ? (job->platform[0] == 'J' ? JS1 :
			    (job->platform[1] == '1' ? ES1 : ES2))
			  : (ST1 + i));
    printfLLog(LOG_ERR, "Starting %s job %d: %s/%d/%s",
	    job->scan_job ? "SCAN" : "DECODE",
            job->job_id, job->platform, job->rev, ModeImage(job->mode));

    /*--------------------*
     |  Check FRAME_MODE
     *--------------------*/
    s = ODLGetStr(msg, FRAME_MODE);
    if (s == NULL || strcmpv(s, "ARCTIC", "ANTARCTIC", 0) < 0)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(FRAME_MODE, '.')+1);

    strcpy(job->frame_mode, s);

    if (! GetFrame(job->mode, job))
	return (*job->Log)(job->err, "%s/%d/%s frame table missing\n",
		ModeImage(job->mode), job->rev, job->frame_mode);

    /*--------------------*
     |  Check ORIGINATOR
     *--------------------*/
    s = ODLGetStr(msg, SOURCE);
    if (s == NULL || (i = strlen(s)) < 2 || strcmp(s, "CP"))
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(SOURCE, '.')+1);

    if (i >= sizeof(job->src))
	return (*job->Log)(job->err,"%s too long, max %d\n",
		strchr(SOURCE, '.')+1, sizeof(job->src)-1);
    memcpy(job->src, s, i+1);
	
    /*---------------------*
     |  Check DESTINATION
     *---------------------*/
    s = ODLGetStr(msg, DESTIN);
    if (s == NULL /* || (i = strlen(s)) < 3 || strncmp(s, "RDS", 3) */)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(DESTIN, '.')+1);

    if (i >= sizeof(job->dst))
	return (*job->Log)(job->err,"%s too long, max %d\n",
		strchr(DESTIN, '.')+1, sizeof(job->dst)-1);
    memcpy(job->dst, s, i+1);
    
    /*----------------*
     |  Check SENSOR
     *----------------*/
    s = ODLGetStr(msg, SENSOR);
    if (s == NULL || strlen(s) != 1 || strcmpv(s, "S", "D", "O", "V", 0) < 0)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(SENSOR, '.')+1);
    job->sensor = *s;

    /*-------------*
     |  Check SEQ
     *-------------*/
    job->seq = ODLGetInt(msg, SEQ, &err);
    if (err == -1 || job->seq < 0)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(SEQ, '.')+1);

    /*------------------*
     |  Check ACTIVITY
     *------------------*/
    s = ODLGetStr(msg, ACTIVITY);
    if (s == NULL || strlen(s) != 3 ||
    	strcmpv(s, "RLT", "REC", "RTS", "RES", "DMP", 0) < 0)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(ACTIVITY, '.')+1);
    memcpy(job->activity, s, 4);

    /*--------------------*
     |  Check MEDIA_TYPE
     *--------------------*/
    s = ODLGetStr(msg, MEDIA_TYPE);
    if (s == NULL || strcmpv(s, "DCRSI", "ID-1", "DISK", 0) < 0)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(MEDIA_TYPE, '.')+1);
    strcpy(job->media_type, s);
    job->blk_size = (s[1] == 'D' ? SONY_BLOCK_SIZE : DCRSI_BLOCK_SIZE);

    /*-------------------------*
     |  Check MEDIA_LOCATION
     *-------------------------*/
    s = ODLGetStr(msg, MEDIA_LOCATION);
    if (s == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(MEDIA_LOCATION, '.')+1); 
    if ((i = strlen(s)) >= sizeof(job->media_loc))
    	return (*job->Log)(job->err, "%s too long, max %d\n",
		strchr(MEDIA_LOCATION, '.')+1, sizeof(job->media_loc)-1);
    memcpy(job->media_loc, s, i+1);

    /*-------------------*
     |  Check MEDIA_ID 
     *-------------------*/
    s = ODLGetStr(msg, MEDIA_ID);
    if (s == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(MEDIA_ID, '.')+1); 
    if ((i = strlen(s)) > 12)
    	return (*job->Log)(job->err, "%s too long, max %d\n",
		strchr(MEDIA_ID, '.')+1, 12);
    memcpy(job->media_id, s, i+1);

    s = ODLGetStr(msg, CHECK_MEDIA_ID);
    if (s == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(CHECK_MEDIA_ID, '.')+1); 
    job->media_check = !strcasecmp(s, "YES");

    /*------------------------*
     |  Check DATA_DIRECTION
     *------------------------*/
    s = ODLGetStr(msg, DATA_DIRECTION);
    if (s != NULL && (i = strcmpv(s, "REVERSE", "FORWARD", "UNKNOWN", 0)) < 0)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(DATA_DIRECTION, '.')+1);
    job->reverse = s ? (i-1) : 0;
    if (job->reverse && strcmp(job->media_type, "ID-1"))
	return (*job->Log)(job->err,
	"This version of RDS does not support reverse %s off %s\n",
	job->scan_job ? "scanning" : "decoding", job->media_type);

    /*--------------------*
     |  Check START_TIME
     *--------------------*/
    if (Type(odl = Value(Lookup(msg, START_TIME))) != (Method_t) Time ||
	(timep = (GMT_t*) Val(odl)) == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(START_TIME, '.')+1);

    job->start_time.yr  = timep->tm.tm_year+1900;
    job->start_time.day = timep->tm.tm_yday+1;
    job->start_time.hr  = timep->tm.tm_hour;
    job->start_time.min = timep->tm.tm_min;
    job->start_time.second = timep->tm.tm_sec + timep->tv_usec/1.0E6;

    /*------------------*
     |  Check END_TIME 
     *------------------*/
    if (Type(odl = Value(Lookup(msg, END_TIME))) != (Method_t) Time ||
	(timep = (GMT_t*) Val(odl)) == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(END_TIME, '.')+1);

    job->end_time.yr  = timep->tm.tm_year+1900;
    job->end_time.day = timep->tm.tm_yday+1;
    job->end_time.hr  = timep->tm.tm_hour;
    job->end_time.min = timep->tm.tm_min;
    job->end_time.second = timep->tm.tm_sec + timep->tv_usec/1.0E6;

    if (job->end_time.day*24*3600 + job->end_time.hr*3600 +
	job->end_time.min*60 + (double) job->end_time.second <
	job->start_time.day*24*3600 + job->start_time.hr*3600 +
	job->start_time.min*60 + (double) job->start_time.second)
	return (*job->Log)(job->err, "%s must be < %s\n",
		strchr(START_TIME, '.')+1, strchr(END_TIME, '.')+1);

    /*---------------------*
     |  Check RECORDER_ID
     *---------------------*/
    s = ODLGetStr(msg, RECORDER_ID);
    if (s == NULL || strlen(s) > 16)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(RECORDER_ID, '.')+1);
    strcpy(job->recorder, s);

    /*--------------------*
     |  Check STATION_ID
     *--------------------*/
    s = ODLGetStr(msg, STATION_ID);
    if (s == NULL /* || strcmpv(s, "FA", "MC", "OTHER", 0) < 0 */)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(STATION_ID, '.')+1);
    strcpy(job->station, s);

    /*-------------------*
     |  Check SITE_NAME
     *-------------------*/
    s = ODLGetStr(msg, SITE_NAME);
    if (s == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(SITE_NAME, '.')+1);
    if ((i = strlen(s)) >= sizeof(job->site_name))
	return (*job->Log)(job->err, "%s too long, max %d\n",
		strchr(SITE_NAME, '.')+1, sizeof(job->site_name)-1);
    memcpy(job->site_name, s, i+1);

   /*------------------*
     |  Check GHA_TIME
     *------------------*/
    if (Type(odl = Value(Lookup(msg, GHA_TIME))) != (Method_t) Time ||
	(timep = (GMT_t*) Val(odl)) == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(GHA_TIME, '.')+1);

    job->gha.time.yr  = timep->tm.tm_year+1900;
    job->gha.time.day = timep->tm.tm_yday+1;
    job->gha.time.hr  = timep->tm.tm_hour;
    job->gha.time.min = timep->tm.tm_min;
    job->gha.time.second = timep->tm.tm_sec + timep->tv_usec/1.0E6;
    gha.time = job->gha.time;	/* Needed by get_ref_gha_ for utc2gha.f */

    /*-------------------*
     |  Check GHA_ANGLE
     *-------------------*/
    job->gha.angle = ODLGetDouble(msg, GHA_ANGLE, &err);
    if (err == -1)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(GHA_ANGLE, '.')+1);
    gha.angle = job->gha.angle;	/* Needed by get_ref_gha_ for utc2gha.f */

    /*----------------------*
     |  Check SV_SAT 
     *----------------------*/
    s = ODLGetStr(msg, SV_SAT);
    if (s == NULL || strcmpv(s, "R1", "E1", "E2", "J1", 0) < 0)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(SV_SAT, '.')+1);
    if (strcmp(s, job->platform))
    	return (*job->Log)(job->err, "%s and %s mismatched\n",
		strchr(SV_SAT, '.')+1, strchr(PLATFORM, '.')+1);

    /*-----------------*
     |  Check SV_TYPE
     *-----------------*/
    s = ODLGetStr(msg, SV_TYPE);
    if (s == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(SV_TYPE, '.')+1);
    if (! strcmp(s, "RESTITUTED"))
	job->stv.precision = RESTITUTED;
    else if (! strcmp(s, "PREDICTED"))
	job->stv.precision = PREDICTED;
    else if (! strcmp(s, "PRELIMINARY"))
	job->stv.precision = PRELIMINARY;
    else if (! strcmp(s, "PRECISION"))
	job->stv.precision = PRECISION;
    else
	return (*job->Log)(job->err, "invalid %s\n",
		strchr(SV_TYPE, '.')+1);

    /*----------------------*
     |  Check SV_COORD_SYS
     *----------------------*/
    s = ODLGetStr(msg, SV_COORD_SYS);
    if (s == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(SV_COORD_SYS, '.')+1);
    if (!strcmp(s, "TRUE_EQUATORIAL"))
	job->stv.coord_sys = TRUE_EQUATORIAL;
    else if (! strcmp(s, "MEAN_OF_DATE"))
	job->stv.coord_sys = MEAN_OF_DATE;
    else
	return (*job->Log)(job->err, "invalid %s\n",
		strchr(SV_COORD_SYS, '.')+1);

    /*----------------*
     |  Check SV_REV
     *----------------*/
    job->stv.rev = ODLGetInt(msg, SV_REV, &err);
    if (err == -1 || job->stv.rev < 0)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(SV_REV, '.')+1);

    /*-----------------*
     |  Check SV_TIME
     *-----------------*/
    if (Type(odl = Value(Lookup(msg, SV_TIME))) != (Method_t) Time)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(SV_TIME, '.')+1);
    timep = (GMT_t*) Val(odl);
    job->stv.t0.yr  = timep->tm.tm_year+1900;
    job->stv.t0.day = timep->tm.tm_yday+1;
    job->stv.t0.hr  = timep->tm.tm_hour;
    job->stv.t0.min = timep->tm.tm_min;
    job->stv.t0.second = timep->tm.tm_sec + timep->tv_usec/1.0E6;

    /*---------------------------------------*
     |  Check SV_POS_X, SV_POS_Y, SV_POS_Z
     *---------------------------------------*/
    if (job->stv.sv[0][0] = ODLGetDouble(msg, SV_POS_X, &err)*1000, err == -1)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(SV_POS_X, '.')+1);

    if (job->stv.sv[0][1] = ODLGetDouble(msg, SV_POS_Y, &err)*1000, err == -1)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(SV_POS_Y, '.')+1);

    if (job->stv.sv[0][2] = ODLGetDouble(msg, SV_POS_Z, &err)*1000, err == -1)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(SV_POS_Z, '.')+1);

    /*---------------------------------------*
     |  Check SV_VEL_X, SV_VEL_Y, SV_VEL_Z
     *---------------------------------------*/
    if (job->stv.sv[0][3] = ODLGetDouble(msg, SV_VEL_X, &err), err == -1)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(SV_VEL_X, '.')+1);

    if (job->stv.sv[0][4] = ODLGetDouble(msg, SV_VEL_Y, &err), err == -1)
	return (*job->Log)(job->err, "Invalid or missing %s\n", 
		strchr(SV_VEL_Y, '.')+1);

    if (job->stv.sv[0][5] = ODLGetDouble(msg, SV_VEL_Z, &err), err == -1)
	return (*job->Log)(job->err, "Invalid or missing %s\n", 
		strchr(SV_VEL_Z, '.')+1);

    /*------------------*
     |  Check TC_REV
     *------------------*/
    job->tcf.rev = ODLGetInt(msg, TC_REV, &err);
    if (err == -1 || job->tcf.rev < 0) 
	return (*job->Log)(job->err, "Invalid or missing %s\n", 
		strchr(TC_REV, '.')+1);

    /*-------------------*
     |  Check TC_TIME
     *-------------------*/
    if (Type(odl = Value(Lookup(msg, TC_TIME))) != (Method_t) Time ||
	(timep = (GMT_t*) Val(odl)) == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n", 
		strchr(TC_TIME, '.')+1);

    job->tcf.gmt.yr  = timep->tm.tm_year+1900;
    job->tcf.gmt.day = timep->tm.tm_yday+1;
    job->tcf.gmt.hr  = timep->tm.tm_hour;
    job->tcf.gmt.min = timep->tm.tm_min;
    job->tcf.gmt.second = timep->tm.tm_sec + timep->tv_usec/1.0E6;

    /*----------------------*
     |  Check TC_SAT_TIME
     *----------------------*/
    job->tcf.bt = (U32) ODLGetInt(msg, TC_SAT_TIME, &err);
    if (err == -1)
	return (*job->Log)(job->err, "Invalid or missing %s\n", 
		strchr(TC_SAT_TIME, '.')+1);

    /*--------------------------*
     |  Check TC_CLK_CYCLE
     *--------------------------*/
    job->tcf.delta = ODLGetInt(msg, TC_CLK_CYCLE, &err);
    if (err == -1)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(TC_CLK_CYCLE, '.')+1);

    /*-------------------------------------------------*
     |  Make sure we have some tape blocks to process
     *-------------------------------------------------*/
    job->start.bit = 0;
    job->start.blk = job->dsk_start = ODLGetInt(msg, START_BLOCK, &err);
    if (err == -1 || job->start.blk < 0)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(START_BLOCK, '.')+1);

    if (job->start.blk < 6000 && job->media_type[1] == 'D') {
	job->start.blk = 6000;
	printfLLog(LOG_INFO, "Will start at block #6000 instead");
    }
    if (job->start.blk == 0 && job->media_type[1] == 'C') {
	job->start.blk = 1;
	printfLLog(LOG_INFO, "Will start at block #1 instead");
    }
    job->end.bit = job->blk_size*8-1;
    job->end.blk = ODLGetInt(msg, END_BLOCK, &err);
    if (err == -1)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
		strchr(END_BLOCK, '.')+1);

    if (job->end.blk < job->start.blk) {
	int  blk = job->end.blk;
	job->end.blk = job->start.blk;
	job->start.blk = blk;
	printfLLog(LOG_INFO, "%s should be <= %s -- Will reverse them",
		   START_BLOCK, END_BLOCK);
    }
    /*---------------------------*
     |  Check SCAN_RESULTS_FILE
     *---------------------------*/
    s = ODLGetStr(msg, SCAN_RESULTS_FILE);
    if (s == NULL)
	return (*job->Log)(job->err, "Invalid or missing %s\n",
			   SCAN_RESULTS_FILE);
    if (i = strlen(s), i >= sizeof(job->sca_file))
	return (*job->Log)(job->err, "%s filename too long, max %d\n",
			   SCAN_RESULTS_FILE, sizeof(job->sca_file)-1);
    memcpy(job->sca_file, s, i+1);

    return (job->scan_job ? get_scan_params(job) : get_decode_params(job));
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_Q_Shutdown - shutdown all RDS queues
 |
 |  SYPNOSIS
 |      void RDS_Q_Shutdown(RDS_t* rds)
 |
 |          rds - RDS specific parameter structure
 |
 |  DESCRIPTION
 |      This routine will shutdown the merge, scan and tape queues.
 |
 *----------------------------------------------------------------------------*/
static
void RDS_Q_Shutdown(RDS_t* rds)
{
    Q_Shutdown(&rds->merge_Q);
    Q_Shutdown(&rds->scan_Q);
    Q_Shutdown(&rds->tapebuf_Q);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_Q_Destroy - remove rds queues
 |
 |  SYPNOSIS
 |      void RDS_Q_Destroy(RDS_t* rds)
 |
 |          rds - RDS specific parameter structure
 |
 |  DESCRIPTION
 |      This routine will remove/destroy the various RDS processing queues
 |      and free allocated memory.
 |
 *----------------------------------------------------------------------------*/
static
void RDS_Q_Destroy(RDS_t* rds)
{
    Q_Destroy(&rds->merge_Q);
    Q_Destroy(&rds->scan_Q);
    Q_Destroy(&rds->tapebuf_Q);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_read -
 |
 |  SYPNOSIS
 |      void* RDS_read(Q_t* tapebuf_Q, void* tapebuf, RDS_t* rds)
 |
 |          tapebuf_Q - tape queue object
 |          tapebuf   -
 |          rds       - RDS specific parameter structure
 |
 |          returns   - pointer to valid segment merge descriptor
 |			or NULL for error
 |  DESCRIPTION
 |
 *----------------------------------------------------------------------------*/
static
void* RDS_read(Q_t* tapebuf_Q, TapeBuf_t* tapebuf, RDS_t* rds)
{
#ifdef	DEBUG
    struct timeval tape_timer, tape_time;
#endif
    Merge_t* mp;		/* segment merge descriptor */
    register Segmt_t* sp;	/* segment descriptor */
    const Job_t* job = &rds->job;
    const Satellite_t* sat = (Satellite_t*) job->sat;
    const int nb = (DataSource(&rds->tape) == SONY_TAPE
		 ? SONY_READ_SIZE : DCRSI_READ_SIZE);
    Q_Unlock(tapebuf_Q);

    tapebuf->job  = job;
    tapebuf->next = NULL;
    tapebuf->prev = rds->lastbuf;
    tapebuf->blk_start = rds->tapepos;
    tapebuf->blk_count = (job->end.blk - rds->tapepos + 1  < nb/job->blk_size)
		       ? (job->end.blk - rds->tapepos + 1) : nb/job->blk_size;
    if (tapebuf->blk_count == 0)
	return NULL;

#ifndef DEBUG
printf("#%d%c", 
job->reverse ? (job->end.blk-rds->tapepos+job->start.blk) : rds->tapepos,
rds->tapepos + tapebuf->blk_count > job->end.blk ? '\n' :
(((rds->tapepos - job->start.blk) / tapebuf->blk_count)%8 == 7
	? '\n' : ' '));
#endif

    if (DataSource(&rds->tape) == SONY_TAPE) {

        if (rds->sony_256) {
	    tapebuf->buf = tapebuf->data+256;
	    memcpy(tapebuf->buf, rds->sony_buf, 256);
#ifdef	DEBUG
tape_time.tv_sec = tape_time.tv_usec = 0;
start_timer(&tape_timer, &tape_time);
#endif
            if (rds->tapeerr)
		memset(tapebuf->buf+256, 0, nb-256);

	    else if (Tape_Read(&rds->tape, tapebuf->buf+256, nb-256,
			job->scan_job &&
			rds->tapepos > job->end.blk-(job->tape_errlim/512)*16)
		!= nb-256) {

		if (! job->scan_job ||
		    rds->tapepos <= job->end.blk-(job->tape_errlim/512)*16)
		    return NULL;

		rds->tapeerr = 1;
		memset(tapebuf->buf+256, 0, nb-256);
	    }
#ifdef	DEBUG
stop_timer(&tape_timer, &tape_time);
printf(" Read %dB: %d.%ds: %e B/s\n",
	(nb-256), tape_time.tv_sec, tape_time.tv_usec,
	(nb-256)/(tape_time.tv_sec + tape_time.tv_usec/1.0E6));
#endif
	    rds->sony_256 = NULL;
	}
	else {
	    tapebuf->buf = tapebuf->data;
#ifdef	DEBUG
tape_time.tv_sec = tape_time.tv_usec = 0;
start_timer(&tape_timer, &tape_time);
#endif
            if (rds->tapeerr)
		memset(tapebuf->buf, 0, nb+256);

	    else if (Tape_Read(&rds->tape, tapebuf->buf, nb+256,
			job->scan_job &&
			rds->tapepos > job->end.blk-(job->tape_errlim/512)*16)
		!= nb+256) {

		if (! job->scan_job ||
		    rds->tapepos <= job->end.blk-(job->tape_errlim/512)*16)
		    return NULL;

		rds->tapeerr = 1;
		memset(tapebuf->buf, 0, nb+256);
	    }
#ifdef	DEBUG
stop_timer(&tape_timer, &tape_time);
printf(" Read %dB: %d.%ds: %e B/s\n",
	(nb+256), tape_time.tv_sec, tape_time.tv_usec,
	(nb+256)/(tape_time.tv_sec + tape_time.tv_usec/1.0E6));
#endif
	    memcpy(rds->sony_buf, tapebuf->buf+nb, 256);
	    rds->sony_256 = rds->sony_buf;
	}
    }
    else {
	tapebuf->buf = tapebuf->data;
	if (rds->tapeerr)
	    memset(tapebuf->buf, 0, nb);

	else if (Tape_Read(&rds->tape, tapebuf->buf, nb, job->scan_job &&
		 rds->tapepos > job->end.blk-(job->tape_errlim/512)*16) != nb) {

	    if (! job->scan_job ||
		rds->tapepos <= job->end.blk-(job->tape_errlim/512)*16)
		return NULL;

	    rds->tapeerr = 1;
	    memset(tapebuf->buf, 0, nb);
	}
    }
    rds->tapepos += nb/job->blk_size;
    if (rds->tapepos > job->end.blk)
	rds->tapepos = job->end.blk+1;

    if (rds->lastbuf)
	rds->lastbuf->next = tapebuf;
    rds->lastbuf = tapebuf;

    if (rds->merge_cnt >= rds->merge_max) {
	(*job->Log)(job->err, "Too many merges\n");
	return NULL;
    }
    mp = &rds->merge[rds->merge_cnt++];
    mp->tapebuf = tapebuf;
    mp->blk_cnt = tapebuf->blk_count;
    mp->blk_start = tapebuf->blk_start;
    mp->merge = rds->segmt;
    mp->merge_cnt = &rds->segmt_cnt;
    mp->merge_max = rds->segmt_max;
    mp->segmt_cnt = 0;
    mp->pulse_exist = 0;

    sp = &mp->segmt_free;

    sp->pulse = &rds->pulse[rds->pulse_cnt];
    sp->pulse_cnt = PULSE_PER_READ_MAX;
    if ((rds->pulse_cnt += sp->pulse_cnt) >= rds->pulse_max) {
	(*job->Log)(job->err, "Too many pulses\n");
	return NULL;
    }
    sp->burst = &rds->burst[rds->burst_cnt];
    sp->burst_cnt = BURST_PER_READ_MAX;
    if ((rds->burst_cnt += sp->burst_cnt) >= rds->burst_max) {
	(*job->Log)(job->err, "Too many bursts\n");
	return NULL;
    }
    sp->dwp = &rds->dwp[rds->dwp_cnt];
    sp->dwp_cnt = sp->burst_cnt * rds->dwp_per_burst_max;
    if ((rds->dwp_cnt += sp->dwp_cnt) >= rds->dwp_max) {
	(*job->Log)(job->err, "Too many DWPs\n");
	return NULL;
    }
    sp->agc = &rds->agc[rds->agc_cnt];
    sp->agc_cnt = sp->burst_cnt * rds->agc_per_burst_max;
    if ((rds->agc_cnt += sp->agc_cnt) >= rds->agc_max) {
	(*job->Log)(job->err, "Too many AGCs\n");
	return NULL;
    }
    /*  Save read data to disk only if input not from disk and dump enabled */

    if (rds->tapedump[0] == 0 || rds->tapedump_err ||
	job->media_type[1] == 'I' || strcasecmp(job->media_loc, "SHELF"))
	return Q_Insert(&rds->scan_Q, &mp);
	
    if (rds->tapedump_fd == -1) {
	char name[256];
	sprintf(name, "%s.%d", rds->tapedump, rds->tapedump_nf);
	rds->tapedump_fd = open(name, O_CREAT|O_TRUNC|O_WRONLY, 
			    	      S_IRUSR|S_IWUSR|S_IRGRP);
	if (rds->tapedump_fd == -1) {
	    printfLLog(LOG_ERR, "%s: create failed, %s",
		       name, strerror(errno));
	    rds->tapedump_err = 1;
	}
    }
    if (rds->tapedump_fd != -1) {
	if (write(rds->tapedump_fd, tapebuf->buf, nb) != nb) {
	    printfLLog(LOG_ERR, "%s.%d: write failed, %s", 
		       rds->tapedump, rds->tapedump_nf, strerror(errno));
	    close(rds->tapedump_fd);
	    rds->tapedump_err = 1;
	    rds->tapedump_fd = -1;
	    rds->tapedump_nf = 1;
	    rds->tapedump_nb = 0;
	}
	if ((rds->tapedump_nb += nb) >= 2142625792 
	||  tapebuf->blk_start + tapebuf->blk_count == job->end.blk + 1) {
	    close(rds->tapedump_fd);
	    rds->tapedump_fd = -1;
	    rds->tapedump_nb = 0;
	    rds->tapedump_nf++;
	}
    }
    return Q_Insert(&rds->scan_Q, &mp);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_scan -
 |
 |  SYPNOSIS
 |      void* RDS_scan(Q_t* scan_Q, Merge_t* mp, RDS_t* rds)
 |
 |          scan_Q -
 |          mp     - segment merge descriptor
 |          rds    - RDS specific parameter structure
 |
 |          returns - pointer to valid segment merge descriptor
 |                    or NULL for error
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
void* RDS_scan(Q_t* scan_Q, Merge_t* mp, RDS_t* rds)
{
    const Job_t* job = &rds->job;
    const Satellite_t* sat = (Satellite_t*) job->sat;
    const int n32 = RoundUp(sat->frame_size_max, 4);
    const int nb = (DataSource(&rds->tape) == SONY_TAPE
		    ? SONY_READ_SIZE : DCRSI_READ_SIZE);
    const int len = (nb+n32)/4;
    register TapeBuf_t* tapebuf = (TapeBuf_t*) mp->tapebuf;
    register U32* w = (U32*) (tapebuf->buf - n32);

    memcpy(w, rds->overlap, n32);
    memcpy(rds->overlap, w+(nb/4), n32);

    Q_Unlock(scan_Q);

    /*  The inversion logic applies only to JERS if ACTIVITY_ID is 'DMP' */
    if (job->platform[0] == 'J') {
	if (job->activity[0] == 'D') {
	    register int i;
 	    for (i = 0; i < len; ++i) w[i] = ~w[i];
        }
    }
    else if (tapebuf->blk_start == job->start.blk) {

        const int n = (job->reverse
                ? (job->blk_size*8 - job->end.bit) : job->start.bit)/8;
        if (n) memset(tapebuf->buf, 0, n);
    }
    else if (tapebuf->blk_start+tapebuf->blk_count-1 >= job->end.blk) {

        const int n = ((job->reverse
                ? job->start.bit : (job->blk_size*8 - job->end.bit))/8)
                + nb - tapebuf->blk_count*job->blk_size;
        if (n) memset(tapebuf->buf+nb-n, 0, n);
    }
    if (! Scan(mp, job)) {
	Q_Shutdown(&rds->tapebuf_Q);
	Q_Shutdown(&rds->scan_Q);
	return NULL;
    }
    return (job->scan_job
	? Q_Insert(&rds->tapebuf_Q, &mp->tapebuf)
	: Q_Insert(&rds->merge_Q, &mp));
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_decode -
 |
 |  SYPNOSIS
 |      void* RDS_decode(Q_t* merge_Q, Merge_t* mp, RDS_t* rds)
 |
 |          merge_Q -
 |          mp      - segment merge descriptor
 |          rds     - RDS specific parameter structure
 |
 |          returns - pointer to updated segment merge descriptor
 |                    or NULL for error
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
void* RDS_decode(Q_t* merge_Q, Merge_t* mp, RDS_t* rds)
{
    register const Mode_t mode = rds->job.mode;
    register int i, first_segmt, final_segmt, first_burst;

    Q_Unlock(merge_Q);

    first_segmt = (*mp->merge_cnt ? *mp->merge_cnt-1 : 0);
    first_burst = (isScanSAR(mode) ? mp->merge[first_segmt].burst_cnt
				   : mp->merge[first_segmt].pulse_cnt)-1;
    /* Merge the segments */
    if (! Merge(mp, &rds->job)) {

	/* Decode failed */
	RDS_Q_Shutdown(rds);
        return NULL;
    }
    final_segmt = *mp->merge_cnt-1;

    if (! mp->pulse_exist)
	return (Q_Insert(&rds->tapebuf_Q, &mp->tapebuf));

    if (Tape_LastBuf(mp->tapebuf))
	while (first_segmt < final_segmt && mp->merge[final_segmt].type != -1)
	    --final_segmt;

    start_timer(&wrt_timer, &wrt_time);

    /* Decode this frame */
    for (i = first_segmt; i <= final_segmt; ++i) {
	register int k, n, f;
	register Segmt_t* sp = &mp->merge[i];

	if (sp->type != -1)
	    continue;

	f = (i != final_segmt ? 1 : Tape_LastBuf(mp->tapebuf));
	k = (i != first_segmt ? 0 : first_burst);
	n = (isScanSAR(mode) ? sp->burst_cnt : sp->pulse_cnt) - (f ? 1 : 2);

	if (! (*((Satellite_t*)rds->job.sat)->Decode)(sp, k,n,f, &rds->job)) {

	    /* Decode failed */
	    stop_timer(&wrt_timer, &wrt_time);
	    RDS_Q_Shutdown(rds);
	    return NULL;
	}
    }
    stop_timer(&wrt_timer, &wrt_time);

    if (rds->tapebuf_prev && rds->tapebuf_prev != mp->tapebuf
    &&  !Q_Insert(&rds->tapebuf_Q, &rds->tapebuf_prev)) {

	/* Decode failed */
	RDS_Q_Shutdown(rds);
	return NULL;
    }
    rds->tapebuf_prev = mp->tapebuf;
    return (void*) mp;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
int RDS_merge_check (Q_t* merge_Q, Merge_t* mp, RDS_t* rds)
{
    return Merge_cmp(&mp->segmt[0], &mp->merge[*mp->merge_cnt-1]);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_Q_Init - initialize RDS queues
 |
 |  SYPNOSIS
 |      RDS_t* RDS_Q_Init(RDS_t* rds)
 |
 |          rds - RDS specific parameter structure
 |
 |          returns - pointer to initialized RDS specific parameter structure
 |
 |  DESCRIPTION
 |      This routine will initialize the RDS tape, scan and merge queues.
 |
 *----------------------------------------------------------------------------*/
static
RDS_t* RDS_Q_Init(RDS_t* rds)
{
    const Job_t* job = &rds->job;
    const int n32 = RoundUp(((Satellite_t*)job->sat)->frame_size_max, 4);
    register int i = (n32+job->blk_size-1)/job->blk_size;
    register Segmt_t* sp;

    rds->tapebuf_prev = NULL;
    rds->pulse_cnt = rds->burst_cnt = rds->dwp_cnt = rds->agc_cnt =
    rds->merge_cnt = rds->ppr_cnt = 0;
    rds->segmt_cnt = 1;

    sp = &rds->segmt[0];
    sp->type = 0;
    sp->mode = MODE_INVALID;
    sp->reverse = job->reverse;
    sp->pulse_pcm = -1;
    sp->pulse_cnt = sp->pulse_0 = sp->burst_0 = sp->burst_len = sp->pulse_err =
    sp->frame_cnt = sp->dwp_cnt = sp->agc_cnt = sp->ppr_cnt = sp->bit_error =
    sp->burst_cnt = 0;
    sp->cal = sp->prf = sp->lookangle = sp->hidelta = 0;
    sp->start.blk = sp->end.blk =  job->start.blk - i;
    sp->start.bit = sp->end.bit = (job->blk_size*i - n32)*8 + 9;

    for (i = 0; i < rds->pulse_max; ++i)
	rds->pulse[i] = (Pulse_t*) &rds->pulsebuf[i*PULSE_SIZE_MAX];

    Q_Reset(&rds->tapebuf_Q);
    Q_Reset(&rds->scan_Q);
    Q_Reset(&rds->merge_Q);

    return rds;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_scanner - process scan job
 |
 |  SYPNOSIS
 |      void RDS_scanner(RDS_t* rds)
 |
 |          rds - RDS specific parameter structure
 |
 |  DESCRIPTION
 |      This routine controls the scan job.
 |
 *----------------------------------------------------------------------------*/
static
void RDS_scanner(RDS_t* rds)
{
    void* tapebuf;
#ifdef __sgi
    /*  Make sure if our parent died, we children should too! */
    if (prctl(PR_TERMCHILD) == -1)
        printfLLog(LOG_ERR,
	"Scanner: %s - response to RESET/SHUTDOWN will fail", strerror(errno));
#endif
    printfLLog(LOG_INFO, "Scanner started job %d", rds->job.job_id);
    while (Q_Remove(&rds->scan_Q, &tapebuf));
    printfLLog(LOG_INFO, "Scanner completed job %d", rds->job.job_id);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_merger -
 |
 |  SYPNOSIS
 |      void RDS_merger(RDS_t* rds)
 |
 |          rds - RDS specific parameter structure
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
void RDS_merger(RDS_t* rds)
{
    void* mp;
#ifdef __sgi
    /*  Make sure if our parent died, we children should too! */
    if (prctl(PR_TERMCHILD) == -1)
        printfLLog(LOG_ERR,
	"Writer: %s - response to RESET/SHUTDOWN will fail", strerror(errno));
#endif
    printfLLog(LOG_INFO, "Writer started job %d", rds->job.job_id);
    while (Q_Remove(&rds->merge_Q, &mp));
    printfLLog(LOG_INFO, "Writer completed job %d", rds->job.job_id);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_Threads_Init - create RDS threads
 |
 |  SYPNOSIS
 |      RDS_t* RDS_Threads_Init (RDS_t* rds)
 |
 |          rds - RDS specific parameter structure
 |
 |  DESCRIPTION
 |      This routine will start one writer thread and a thread for each
 |      scan thread as specified in the configuration file.
 |
 *----------------------------------------------------------------------------*/
static
RDS_t* RDS_Threads_Init (RDS_t* rds)
{
    const Job_t* job = &rds->job;
    register int i, j;

    /* Start the writer thread if this is a decode job. */
    if (! job->scan_job &&
	pthread_create (&rds->merger, pthread_attr_default,
		(pthread_startroutine_t) RDS_merger, (pthread_addr_t) rds)
	== -1) {
	char err[256];
	(*job->Log)(job->err, "can't start writer thread, %s",
		strcpy(err, strerror(errno)));
	return NULL;
    }

    /* For each scan thread specified. */
    for (i = 0; i < rds->scanner_max; ++i) {
	char err[256];
	if (pthread_create (&rds->scanner[i], pthread_attr_default,
		(pthread_startroutine_t) RDS_scanner, (pthread_addr_t) rds)
	    != -1)
	    continue;

	(*job->Log)(job->err, "can't start Scanner #%d thread, %s", i+1,
		strcpy(err, strerror(errno)));
	for (j = 0; j < i; ++j)
	    pthread_cancel(rds->scanner[j]);

	if (! job->scan_job)
	    pthread_cancel(rds->merger);
	return NULL;
    }
    return rds;
}

/*----------------------------------------------------------------------------*
 |  NOTE: THIS ROUTINE NOT CURRENTLY USED!
 |
 |  NAME
 |      RDS_Threads_Destroy - remove/destroy RDS threads
 |
 |  SYPNOSIS
 |      void RDS_Threads_Destroy(RDS_t* rds)
 |
 |          rds - RDS specific parameter structure
 |
 |  DESCRIPTION
 |      This routine will remove/destroy all RDS threads.
 |
 *----------------------------------------------------------------------------*/
static
void RDS_Threads_Destroy(RDS_t* rds)
{
    int i;
    for (i = 0; i < rds->scanner_max; ++i)
	pthread_cancel(rds->scanner[i]);

    if (! rds->job.scan_job)
	pthread_cancel(rds->merger);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_Init - initialize RDS
 |
 |  SYPNOSIS
 |      RDS_t* RDS_Init(RDS_t* rds, ODL msg)
 |
 |      rds - RDS specific parameter structure
 |      msg - RDS Start Message
 |
 |      returns - updated RDS specific parameter structure
 |                or NULL for error
 |
 |  DESCRIPTION
 |      This routine will initialize the rds process, threads and tape
 |      parameters.
 |
 *----------------------------------------------------------------------------*/
RDS_t* RDS_Init(RDS_t* rds, ODL msg)
{
    register Job_t* job = &rds->job;
    register Satellite_t* sat;
    register int dataOnDisk,i;

    /*  Initialize timers */
    sca_time.tv_sec = sca_time.tv_usec = 0;
    ppr_time.tv_sec = ppr_time.tv_usec = 0;
    sum_time.tv_sec = sum_time.tv_usec = 0;
    wrt_time.tv_sec = wrt_time.tv_usec = 0;

    /*  Initialize tape dump debugging */
    rds->tapedump_err = 0;
    rds->tapedump_fd = -1;
    rds->tapedump_nf = 1;
    rds->tapedump_nb = 0;

    if (rds->sca_cache) {
	free(rds->sca_cache);
	rds->sca_cache = NULL;
    }
    Status_Reset(&rds->status);

    if (RDS_GetJob(job, msg) == -1)
	return NULL;

    if (job->end.blk - job->start.blk >= rds->tapeblk_max) {
        (*job->Log)(job->err,"Too many tape blocks, max %d\n",rds->tapeblk_max);
	return NULL;
    }
    rds->tapepos = job->start.blk;
    rds->lastbuf = NULL;
    rds->job.Buf = (U8* (*)(void**, int, int)) Tape_Buffer;
    rds->pulse_to_check = rds->tapeerr = 0;

    sat = (Satellite_t*) job->sat;
    if (!(*sat->Init)(job))
	return NULL;

    memset(rds->overlap, 0, RoundUp(sat->frame_size_max,4));
    rds->sony_256 = NULL;

    if (job->scan_job &&
	! (rds->sca_cache = (char*) memalign(512, rds->ppr_max * 102400))) {
	(*job->Log)(job->err, "No memory for scan results cache\n");
	(*sat->Destroy)(job);
	return NULL;
    }
    if (! RDS_Q_Init(rds)) {
	free(rds->sca_cache);
	rds->sca_cache = NULL;
	(*sat->Destroy)(job);
	return NULL;
    }
    rds->reader = pthread_self();
    dataOnDisk = job->media_type[1]=='I' || strcasecmp(job->media_loc,"SHELF");

    if (! Tape_Init(&rds->tape,
	job->start.blk, job->end.blk, job->reverse,
	job->Log, job->err,
	job->media_type[1] == 'D' ? SONY_TAPE : DCRSI_TAPE,
	dataOnDisk,
	dataOnDisk ? job->media_loc :
	    (job->media_check && job->tape_check ? job->media_id : NULL),
	dataOnDisk ? job->dsk_start : job->dcrsi_csize,
	job->seek_timeout, job->seek_retry)) {

	RDS_Q_Shutdown(rds);
	free(rds->sca_cache);
	rds->sca_cache = NULL;
	(*sat->Destroy)(job);
	return NULL;
    }
    if (! RDS_Threads_Init(rds)) {
	Tape_Destroy(&rds->tape);
	RDS_Q_Shutdown(rds);
	free(rds->sca_cache);
	rds->sca_cache = NULL;
	(*sat->Destroy)(job);
	return NULL;
    }
    return rds;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_Destroy -
 |
 |  SYPNOSIS
 |      void RDS_Destroy(RDS_t* rds)
 |
 |          rds - RDS specific parameter structure
 |
 |  DESCRIPTION
 |
 *----------------------------------------------------------------------------*/
void RDS_Destroy(RDS_t* rds)
{
    /* Shutdown all RDS queues. */
    RDS_Q_Shutdown(rds);
    Tape_Destroy(&rds->tape);

    if (rds->sca_cache) {
	free(rds->sca_cache);
	rds->sca_cache = NULL;
    }
    (*((Satellite_t*) rds->job.sat)->Destroy)(&rds->job);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_Reset - reset RDS
 |
 |  SYPNOSIS
 |      void RDS_Reset(RDS_t* rds)
 |
 |          rds - RDS specific parameter structure
 |
 |  DESCRIPTION
 |      This routine will terminate any RDS threads and delete any
 |      result files that have been created.
 |
 *----------------------------------------------------------------------------*/
void RDS_Reset(RDS_t* rds)
{
    register char* s;
    register Job_t* job = &rds->job;
    pthread_addr_t status;

   /* Is this the reader thread? */
    if (pthread_equal(rds->reader, pthread_self()))
	return;

    /* Shutdown all RDS threads */
    rds->tape.quit = 1;
    RDS_Q_Shutdown(rds);

    /* Wait for termination of reader thread */
    pthread_join(rds->reader, &status);

    /* Remove result files */
    if (job->scan_job)
	unlink((s = strchr(job->sca_file, ':')) ? s+1 : job->sca_file);
    else {
	unlink((s = strchr(job->eco_file, ':')) ? s+1 : job->eco_file);
	unlink((s = strchr(job->rep_file, ':')) ? s+1 : job->rep_file);
	unlink((s = strchr(job->aux_file, ':')) ? s+1 : job->aux_file);
	unlink((s = strchr(job->eph_file, ':')) ? s+1 : job->eph_file);
	unlink((s = strchr(job->bof_file, ':')) ? s+1 : job->bof_file);
    }
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      mvfile - moves a file to a new location
 |
 |  SYPNOSIS
 |      int mvfile(char* filename, char* dirname, char* errorbuf)
 |
 |          filename - name of file to move
 |          dirname  - directory to move to
 |          errorbuf - error message if error return
 |
 |          returns -  0 if file move successful
 |                    -1 if error
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
#define	DIO_ALIGN	512		/* Data buffer memory alignment */
#define	DIO_UNIT	512		/* Min xfer size for direct I/O */
#define DIO_BUFLEN	(4096*DIO_UNIT)	/* Max xfer size for direct I/O */

static
int mvfile(const char* filename, const char* dirname, char* errorbuf)
{
    struct stat fs, ds;
    char *s, *buf, fn[256];
    register int n, i, src, dst;

    if (dirname == NULL || *dirname == 0) {
	unlink(filename);
	return 0;
    }
    /* Retrieve file and directory information. */

    if (stat(s = (char*)filename, &fs) == -1 ||
	stat(s = (char*)dirname, &ds) == -1) {
	sprintf(errorbuf, "can't stat %s: %s", s, strerror(errno));
	return -1;
    }
    n = strlen(strcpy(fn, dirname));
    sprintf((n>0 && fn[n-1]=='/') ? fn+n-1 : fn+n, "/%s", basename(filename));

    /* Rename file if it is on same device as directory. */

    if (fs.st_dev == ds.st_dev) {
	if (rename(filename, fn) == -1) {
	    sprintf(errorbuf, "can't rename %s: %s", filename, strerror(errno));
	    return -1;
	}
	return 0;
    }
    if ((buf = memalign(DIO_ALIGN, DIO_BUFLEN)) == NULL) {
	sprintf(errorbuf, "No memory for I/O buffer", DIO_BUFLEN);
	return -1;
    }
    if ((src = open(filename, O_RDONLY|O_DIRECT)) == -1) {
	sprintf(errorbuf, "can't open %s: %s", filename, strerror(errno));
	free(buf);
	return -1;
    }
    if ((dst = open(fn, O_WRONLY|O_CREAT|O_TRUNC|O_DIRECT, 
		    S_IRUSR|S_IWUSR|S_IRGRP)) == -1) {
	sprintf(errorbuf, "can't create %s: %s", fn, strerror(errno));
	close(src);
	free(buf);
	return -1;
    }
    n = fs.st_size / DIO_BUFLEN;
    for (i = 0; i < n; ++i)
	if ((s = "Read", read(src, buf, DIO_BUFLEN) != DIO_BUFLEN) ||
	    (s = "Write", write(dst, buf, DIO_BUFLEN) != DIO_BUFLEN)) {
	    sprintf(errorbuf, "%s error: %s", s, strerror(errno));
	    close(src);
	    close(dst);
	    free(buf);
	    return -1;
	}
    n = fs.st_size % DIO_UNIT;
    if (n) {
	close(src);
	src = open(filename, O_RDONLY);
	if (lseek(src, (fs.st_size/DIO_BUFLEN)*DIO_BUFLEN, SEEK_SET) == -1) {
	    sprintf(errorbuf, "can't re-seek: %s", strerror(errno));
	    close(src);
	    close(dst);
	    free(buf);
	    return -1;
	}
	close(dst);
	dst = open(fn, O_WRONLY, S_IRUSR|S_IWUSR|S_IRGRP);
	if (lseek(dst, 0, SEEK_END) == -1) {
	    sprintf(errorbuf, "can't goto EOF: %s", strerror(errno));
	    close(src);
	    close(dst);
	    free(buf);
	    return -1;
	}
    }
    n = fs.st_size % DIO_BUFLEN;
    if (n && (read(src, buf, n) != n || write(dst, buf, n) != n)) {
	sprintf(errorbuf, "last I/O failed: %s", strerror(errno));
	close(src);
	close(dst);
	free(buf);
	return -1;
    }
    free(buf);
    src = close(src);
    dst = close(dst);
    if (src == -1 || dst == -1) {
	sprintf(errorbuf, "file close error: %s", strerror(errno));
	return -1;
    }
    if (unlink(filename) == -1) {
	sprintf(errorbuf, "can't delete %s: %s", filename, strerror(errno));
	return -1;
    }
    return 0;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_Cleanup - move the result files to final destination
 |
 |  SYPNOSIS
 |      int RDS_Cleanup(RDS_t* rds, char* dir, char* err)
 |
 |          rds - RDS specific parameter structure
 |          dir -
 |          err -
 |
 |          returns - 0 no error
 |                    -1 error
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
int RDS_Cleanup(const RDS_t* rds, const char* dir, char* err)
{
    const Job_t* job = &rds->job;
    const char* dp = (dir && (dp = strchr(dir, ':')) ? dp+1 : dir);
    register int st_aux, st_bof, st_eco, st_rep, st_eph;
    register char *fp;
    struct stat fs;

    *err = 0;
    if (job->scan_job) {
	fp = ((fp = strchr(job->sca_file, ':')) ? fp+1 : job->sca_file);
	return (stat(fp, &fs)== -1 && errno== ENOENT) ? 0 : mvfile(fp, dp, err);
    }
    fp = ((fp = strchr(job->eco_file, ':')) ? fp+1 : job->eco_file);
    if (stat(fp, &fs) == -1 && errno == ENOENT) return 0;
#ifdef	SAVE_ALL
    st_eco = mvfile(fp, dp, err);

    fp = ((fp = strchr(job->rep_file, ':')) ? fp+1 : job->rep_file);
    st_rep = mvfile(fp, dp, err);

    fp = ((fp = strchr(job->bof_file, ':')) ? fp+1 : job->bof_file);
    st_bof = mvfile(fp, dp, err);

    fp = ((fp = strchr(job->eph_file, ':')) ? fp+1 : job->eph_file);
    st_eph = mvfile(fp, dp, err);
#else
    st_eco = st_rep = st_bof = st_eph = 0;
#endif
    fp = ((fp = strchr(job->aux_file, ':')) ? fp+1 : job->aux_file);
    st_aux = mvfile(fp, dp, err);

    return (st_aux == -1 || st_bof == -1 || st_eco == -1 ||
	    st_rep == -1 || st_eph == -1) ? -1 : 0;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_Position - returns current tape block position
 |
 |  SYPNOSIS
 |      int RDS_Position(RDS_t* rds)
 |
 |          rds - RDS specific parameter structure
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
int RDS_Position(const RDS_t* rds)
{
    return rds->tapepos;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
const char* RDS_Error(const RDS_t* rds)
{
    return Status_String(&rds->status);
}
/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
int RDS_Errno(const RDS_t* rds)
{
    return *Status_Errno(&rds->status);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |	Binary-search the first pulse with GMT >= job start time 
 |	and the last pulse with GMT <= job end time.
 |	The function returns the number of pulses found.
 |
 *----------------------------------------------------------------------------*/
static
int RDS_find_good_pulses(RDS_t* rds, Segmt_t* sp, int* k0, int* kN)
{
    register int k, lo = 0, hi = sp->pulse_cnt;
    register const Job_t* job = &rds->job;
    register const Satellite_t* sat = (Satellite_t*) job->sat;
    register double td;
    GMT t0, t1;

#ifdef	DEBUG
    static char diskname[256];
    static int fd = -1, fn = 1, fb = 0;

    if (fd == -1) {
        sprintf(diskname, "/d/04/rds/tnt/pulsedump.%d", fn);
        unlink(diskname);
        fd = open(diskname, O_CREAT|O_APPEND|O_WRONLY, 
			    S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP);
        if (fd == -1) {
            printf("%s create fails\n", diskname);
            fflush(stdout);
        }
    }
    if (fd != -1) {
	char buf[256];
	register int k, n;

	n = sprintf(buf, "SP %d/%d\n", 
		BLKNUM(sp->start.blk,sp,job),
		BITNUM(sp->start.bit,sp,job));
	if (write(fd, buf, n) != n) {
            printf("%s write failed\n", diskname);
            fflush(stdout);
	}
        for (k = 0; k < sp->pulse_cnt; ++k) {

	    n = sprintf(buf, "#%d %d/%d: %x %x\n", k, 
		BLKNUM(sp->pulse[k]->start.blk,sp,job),
		BLKNUM(sp->pulse[k]->start.bit,sp,job),
		((J1_Pulse_t*)sp->pulse[k])->pcm_clk,
		((J1_Pulse_t*)sp->pulse[k])->pcm_bit);

            if (write(fd, buf, n) != n) {
                printf("%s write failed\n", diskname);
                fflush(stdout);
            }
            if ((fb += n) >= 2142625792) {
                close(fd);
                fd = -1;
                ++fn;
                fb = 0;

		sprintf(diskname, "/d/04/rds/tnt/pulsedump.%d", fn);
		unlink(diskname);
		fd = open(diskname, O_CREAT|O_APPEND|O_WRONLY, 
				    S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP);
		if (fd == -1) {
		    printf("%s create fails\n", diskname);
		    fflush(stdout);
		    break;
		}

	    }
	}
    }
#endif
    rds->pulse_to_check = 1;

    /*  Binary-search for the first pulse with GMT >= job start time */
    while (lo < hi) {
	k = (lo + hi)/2;
	if (rds->tape.quit)
	    return (*k0 = *kN = 0);

	(*sat->GMT)(&t0, (PulseList_t)&sp->pulse[k], sp, job);
#ifdef	DEBUG
printf("Checking pulse %d: %.4d-%.3dT%.2d:%.2d:%09.6f, hi %d, lo %d -- FIRST\n",
	k, t0.yr, t0.day, t0.hr, t0.min, t0.second, hi, lo);
#endif
	if (gmt_diff(&t0, &job->start_time) >= -job->gmt_diff_max)
	    hi = k;
	else {
	    lo = k+1;
	}
    }
    for (k = hi; k < sp->pulse_cnt; ++k) {
	if (rds->tape.quit)
	    return (*k0 = *kN = 0);

	(*sat->GMT)(&t0, (PulseList_t)&sp->pulse[k], sp, job);
#ifdef	DEBUG
printf("Checking pulse %d: %.4d-%.3dT%.2d:%.2d:%09.6f, hi %d, lo %d\n",
	k, t0.yr, t0.day, t0.hr, t0.min, t0.second, hi, lo);
#endif
	if (gmt_diff(&t0, &job->start_time) >= -job->gmt_diff_max &&
	   (sp->mode < ST1 || sp->mode > ST7 ||
	    ReplicaSamples((const R1_Pulse_t**)&sp->pulse[k], sp)))
	    break;
    }
#ifdef	DEBUG
    if (k == sp->pulse_cnt)
	printf("First good %d: ---- NONE -----\n", sp->pulse_cnt);
    else {	
	GMT t0;
    (*sat->GMT)(&t0, (PulseList_t)&sp->pulse[k], sp, job);
printf("First good %d:  %.4d-%.3dT%.2d:%.2d:%09.6f -------- total %d\n", k,
	t0.yr, t0.day, t0.hr, t0.min, t0.second, sp->pulse_cnt);
    }
#endif
    if ((*k0 = k) == sp->pulse_cnt)
	return (*k0 = *kN = 0);

    /*  Binary-search for the last pulse with GMT <= job end time */

    hi = sp->pulse_cnt;
    lo = *k0+1;
    while (lo < hi) {
	k = (lo + hi)/2;

	if (rds->tape.quit)
	    return (*k0 = *kN = 0);

	(*sat->GMT)(&t0, (PulseList_t)&sp->pulse[k], sp, job);
#ifdef	DEBUG
printf("Checking pulse %d: %.4d-%.3dT%.2d:%.2d:%09.6f, hi %d, lo %d -- LAST\n",
	k, t0.yr, t0.day, t0.hr, t0.min, t0.second, hi, lo);
#endif
	if (gmt_diff(&t0, &job->end_time) <= job->gmt_diff_max &&
	    gmt_diff(&t0, &job->start_time) >= -job->gmt_diff_max)
	    lo = k+1;
	else {
	    hi = k;
	}
    }
    for (k = lo-1; k < sp->pulse_cnt; ++k) {
	if (rds->tape.quit)
	    return (*k0 = *kN = 0);

	(*sat->GMT)(&t0, (PulseList_t)&sp->pulse[k], sp, job);
#ifdef	DEBUG
printf("Checking pulse %d: %.4d-%.3dT%.2d:%.2d:%09.6f\n", k,
	t0.yr, t0.day, t0.hr, t0.min, t0.second);
#endif
	if (gmt_diff(&t0, &job->end_time) <= job->gmt_diff_max)
	    break;
    }
#ifdef	DEBUG
    if (k == sp->pulse_cnt)
	printf("Last good %d: ---- NONE ----\n", sp->pulse_cnt);
    else {
	GMT t0;
	(*sat->GMT)(&t0, (PulseList_t)&sp->pulse[k], sp, job);
printf("Last good %d:  %.4d-%.3dT%.2d:%.2d:%09.6f -------- total %d\n", k,
	t0.yr, t0.day, t0.hr, t0.min, t0.second, sp->pulse_cnt);
    }
#endif
    rds->pulse_to_check = 0;
    return (k == sp->pulse_cnt ? (*k0 = *kN = 0) : ((*kN = k+1) - *k0));
}

/*----------------------------------------------------------------------------*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 *----------------------------------------------------------------------------*/
static
int RDS_SaveScanResult (RDS_t* rds)
{
    register int i, segmt_cnt = 0;
    register Job_t* job = &rds->job;
    register const Satellite_t* sat = (Satellite_t*) job->sat;
    register const frame_size = sat->frame_size*8;

    /*--------------------------------------* 
     |  Merge results from various threads
     *--------------------------------------*/
    for (i = 0; i < rds->merge_cnt; ++i)
	if (! Merge(&rds->merge[i], job))
	    return -1;

#ifdef DEBUG
    i = -1;
    while (++i < rds->segmt_cnt) {
        register Segmt_t* sp = &rds->segmt[i];
        register int k;

        if ((sp->end.bit - sp->start.bit +
            (sp->end.blk - sp->start.blk)*job->blk_size*8) % frame_size == 0)
            continue;

        for (k = 1; k < rds->segmt_cnt-i && sp[k].pulse_cnt == 0; ++k);
        if (--k > 0) {
            register int n;
            sp->end = sp[k].end;
            rds->segmt_cnt -= k;
            n = rds->segmt_cnt-i;
            if (n > 0) memmove(sp+1, sp+1+k, n*sizeof(Segmt_t));
        }
    }
#endif
    /*------------------------------------------------* 
     |  For each segment, generate applicable frames
     *------------------------------------------------*/
    i = rds->segmt[0].start.blk == rds->segmt[0].end.blk &&
	rds->segmt[0].start.bit == rds->segmt[0].end.bit ? 0 : -1;

    while (++i < rds->segmt_cnt) {
	register int k;
	register PPR_t* pp;
	register Segmt_t* sp = &rds->segmt[i];
	int kN;

	if (sp->pulse_cnt == 0) {
	    printfLLog(LOG_INFO, "%s %d/%.5d..%d/%.5d: %d syncs%s",
		sp->frame_cnt ? "SYN" : "GAP",
		BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
		BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job),
		sp->frame_cnt,
		sp->frame_cnt==0 && sat->frame_size_min==sat->frame_size_max &&
		(sp->end.bit - sp->start.bit +
		(sp->end.blk - sp->start.blk)*job->blk_size*8) % frame_size
		    ? ", odd length" : "");
	    continue;
	}
	if (sp->type > 0) {
	    printfLLog(LOG_INFO, sp->burst_cnt > 1
		? "CAL %d/%.5d..%d/%.5d: %d CAL-%d pulses (%d bursts)"
		: "CAL %d/%.5d..%d/%.5d: %d CAL-%d pulses",
		BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
		BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job),
		sp->pulse_cnt, sp->type, sp->burst_cnt);
	    continue;
	}
	if (sp->type == -2) {
	    printfLLog(LOG_INFO, sp->burst_cnt > 1
		? "SWT %d/%.5d..%d/%.5d: beam switch, %d pulses (%d bursts)"
		: "SWT %d/%.5d..%d/%.5d: beam switch, %d pulses",
		BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
		BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job),
		sp->pulse_cnt, sp->burst_cnt);
	    continue;
	}
	sp->burst[sp->burst_cnt] = sp->pulse_cnt;
	sp->mode = (*sat->Mode)(sp, job);

        printfLLog(
	    (job->mode_check && sp->mode != job->mode) ? LOG_ERR : LOG_INFO,
	    sp->burst_cnt > 1
		? "%s %d/%.5d..%d/%.5d: %d pulses%s (%d bursts)"
		: "%s %d/%.5d..%d/%.5d: %d pulses%s",
	    sp->mode == MODE_INVALID ? "SAR" :
	    (sp->mode==ES1 || sp->mode==ES2) ? "ERS" : ModeImage(sp->mode),
	    BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
	    BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job),
	    sp->pulse_cnt,
	    sp->pulse_cnt < PULSE_PER_SEGMT_MIN ? ", too few" : "",
	    sp->burst_cnt);

	/*  Want only IMAGING segments beyond this point */
	if (sp->mode == MODE_INVALID || sp->pulse_cnt < PULSE_PER_SEGMT_MIN)
	    continue;

	if (RDS_find_good_pulses(rds, sp, &sp->pulse_0, &kN) == 0) {
	    GMT *t0 = &sp->gmt_start, *tN = &sp->gmt_end;

	    (*sat->GMT)(t0,(PulseList_t)&sp->pulse[0], sp, job);
	    (*sat->GMT)(tN,(PulseList_t)&sp->pulse[sp->pulse_cnt-1],sp,job);

	    printfLLog(LOG_INFO,
		(sp->gmt_start.yr == 1970
		 ? "%s %d/%.5d..%d/%.5d: can't determine time"
		 : "%s %d/%.5d..%d/%.5d: time found "
		   "%.4d-%.3dT%.2d:%.2d:%09.6f..%.4d-%.3dT%.2d:%.2d:%09.6f"),
                sp->type > 0 ? "CAL" : (sp->type == -2 ? "SWT" : (
		sp->mode == MODE_INVALID ? "SAR" :
		(sp->mode==ES1 || sp->mode==ES2) ? "ERS": ModeImage(sp->mode))),
		BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
		BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job),
		t0->yr, t0->day, t0->hr, t0->min, t0->second,
		tN->yr, tN->day, tN->hr, tN->min, tN->second);
	    continue;
	}
	if (sp->burst_len <= 1 && (sp->pulse_0 > 0 || kN < sp->pulse_cnt))
	    printfLLog(LOG_INFO,
		(sp->pulse_0 > 0 && kN < sp->pulse_cnt
		  ? "%s %d/%.5d..%d/%.5d: discarded first %d & last %d pulses":
		(sp->pulse_0 > 0
		  ? "%s %d/%.5d..%d/%.5d: discarded first %d pulses"
		  : "%s %d/%.5d..%d/%.5d: discarded last %d pulses")),
                sp->type > 0 ? "CAL" : (sp->type == -2 ? "SWT" : (
		sp->mode == MODE_INVALID ? "SAR" :
		(sp->mode==ES1 || sp->mode==ES2) ? "ERS": ModeImage(sp->mode))),
		BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
		BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job),
		(sp->pulse_0 == 0 ? (sp->pulse_cnt-kN) : sp->pulse_0),
		sp->pulse_cnt-kN);
 	sp->pulse_cnt = kN - sp->pulse_0;

#ifdef	DEBUG
printf("Segment: %.4d-%.3dT%.2d:%.2d:%09.6f .. %.4d-%.3dT%.2d:%.2d:%09.6f\n",
	sp->gmt_start.yr, sp->gmt_start.day, sp->gmt_start.hr,
	sp->gmt_start.min, sp->gmt_start.second,
	sp->gmt_end.yr, sp->gmt_end.day, sp->gmt_end.hr,
	sp->gmt_end.min, sp->gmt_end.second);
#endif
	if (sp->burst_len > 1) {
	    register int beam;	

	     /*  First burst must start a cycle and have good 'pulse_cnt1' */
	    for (k = 0; k < sp->burst_cnt; ++k)
		if (sp->burst_len == sp->burst[k+1] - sp->burst[k] &&
		    sp->burst[k]  >= sp->pulse_0 &&
		    (beam = (*sat->Beam)(sp->pulse[sp->burst[k]]),
		     (sp->mode != SNB && beam == 1) ||
		     (sp->mode == SNB && beam == 2)))
		    break;
	    sp->burst_0 = k;
	    sp->burst_cnt -= k;

	    /*  Make sure the last burst in segment has good pulse_cnt1 */
	    k = sp->burst_0+sp->burst_cnt;
	    while (--k >= sp->burst_0)
		if (sp->burst_len == sp->burst[k+1] - sp->burst[k] &&
		    sp->pulse_cnt >= sp->burst[k+1] - sp->pulse_0)
		    break;

	    if (k < sp->burst_0) {
                printfLLog(LOG_INFO,
	"%s %d/%.5d..%d/%.5d: discarded, bad pulse count in all bursts",
		 sp->mode==MODE_INVALID ? "SAR" :
		(sp->mode==ES1 || sp->mode==ES2) ? "ERS": ModeImage(sp->mode),
		    BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
		    BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job));
                continue;
	    }
	    ++k;
	    if ( sp->burst_0 > 0 || k < sp->burst_0+sp->burst_cnt) {
		printfLLog(LOG_INFO,
		(sp->burst_0 > 0 && k < sp->burst_0+sp->burst_cnt
		  ? "%s %d/%.5d..%d/%.5d: discarded first %d & last %d bursts":
		(sp->burst_0 > 0
		  ? "%s %d/%.5d..%d/%.5d: discarded first %d bursts"
		  : "%s %d/%.5d..%d/%.5d: discarded last %d bursts")),
		 sp->mode == MODE_INVALID ? "SAR" :
		(sp->mode==ES1 || sp->mode==ES2) ? "ERS" : ModeImage(sp->mode),
		    BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
		    BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job),
		    (sp->burst_0 == 0 ? (sp->burst_0+sp->burst_cnt-k)
				      : sp->burst_0),
		    sp->burst_0+sp->burst_cnt-k);
	    }
	    sp->burst_cnt = k - sp->burst_0;
	    sp->pulse_cnt = sp->burst[sp->burst_0+sp->burst_cnt] - 
			    sp->burst[sp->burst_0];
	    sp->pulse_0 = sp->burst[sp->burst_0];

            for (k = 0; k < sp->burst_cnt; ++k) {
		const int *bp = &sp->burst[k+sp->burst_0];

		if (sp->burst_len == bp[1]-bp[0])
		    continue;

		printfLLog(LOG_DEBUG,
		    "Burst #%d, %d/%.5d: %d pulses, want %d, Beam %d/%d", k+1,
		    BLKNUM(sp->pulse[bp[0]]->start.blk,sp,job),
		    BITNUM(sp->pulse[bp[0]]->start.bit,sp,job),
		    bp[1]-bp[0], sp->burst_len,
		    (*sat->Beam)(sp->pulse[bp[0]]), bp[0]);
#ifdef DEBUG
{		register int i;
		for (i = 0; i < bp[1]-bp[0]; ++i) {
		    const Pulse_t* pp = sp->pulse[i+bp[0]];
		    if (pp->frame_cnt < sat->frame_per_pulse_min ||
			pp->frame_cnt > sat->frame_per_pulse_max)
			printfLLog(LOG_DEBUG,
			    "Burst #%d: Pulse %d:: %d/%d, %dF\n", k+1, i,
			    pp->start.blk, pp->start.bit, pp->frame_cnt);
		}
}
#endif
            }
        }
	(*sat->GMT)(&sp->gmt_start,
	    (PulseList_t)&sp->pulse[sp->pulse_0], sp, job);

	sp->prf = (*sat->PRF)((PulseList_t)&sp->pulse[sp->pulse_0], sp, job);

	sp->lookangle = (*sat->LookAngle)(
	    (PulseList_t)&sp->pulse[sp->pulse_0], sp, job);

	(*sat->GMT)(&sp->gmt_end,
	    (PulseList_t)&sp->pulse[sp->pulse_0+sp->pulse_cnt-1], sp, job);

	if (sp->pulse_cnt < (int)(15*sp->prf)) {
	    printfLLog(LOG_INFO,
		sp->burst_cnt > 1
		    ? "%s %d/%.5d..%d/%.5d: %d pulses (%d bursts), too few"
		    : "%s %d/%.5d..%d/%.5d: %d pulses, too few",
		 sp->mode==MODE_INVALID ? "SAR" :
		(sp->mode==ES1 || sp->mode==ES2) ? "ERS": ModeImage(sp->mode),
		BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
		BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job),
		sp->pulse_cnt, sp->burst_cnt);
	    continue;
	}
	if (rds->tape.quit)
            return -1;

	/*-------------------------------*
	 |  Calculate frame information
	 *-------------------------------*/

	start_timer(&ppr_timer, &ppr_time);
	sp->ppr = NULL;
	sp->ppr_cnt = 0;

	while (rds->ppr_cnt < rds->ppr_max) {
	    pp = sp->ppr ? PPR_next (&rds->ppr[rds->ppr_cnt],pp,job)
			 : PPR_first(sp->ppr = &rds->ppr[rds->ppr_cnt],sp,job);
	    if (! pp) break;
	    sp->ppr_cnt++;
	    rds->ppr_cnt++;
	}
	if (pp) printfLLog(LOG_ERR,
	"%s %d/%.5d..%d/%.5d: PPR_MAX exceeded, only %d frames generated",
	     sp->mode == MODE_INVALID ? "SAR" :
	    (sp->mode==ES1 || sp->mode==ES2) ? "ERS": ModeImage(sp->mode),
	    BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
	    BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job),
	    sp->ppr_cnt);

	if (sp->ppr_cnt > 0 && (!job->mode_check || sp->mode == job->mode))
	    ++segmt_cnt;

	stop_timer(&ppr_timer, &ppr_time);
    }
    Segmt_Save(rds->segmt, rds->segmt_cnt, rds->sca_cache, job);

/*  return (segmt_cnt > 0 ? 1 : (*job->Log)(job->err,
 |	   "can't find a good %s segment\n", ModeImage(job->mode)));
 */
    return 1;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_Scan - RDS reader thread
 |
 |  SYPNOSIS
 |      RDS_t* RDS_Scan(RDS_t* rds)
 |
 |          rds - RDS specific parameter structure
 |
 |          returns - pointer to updated RDS specific parameter structure
 |                    or NULL for error
 |
 |  DESCRIPTION
 |      This routine will first read and store raw data from the tape. It then
 |      controls the flow of data to the scanner thread(s), merging of
 |      segments and processing of image data in the case of a decode job.
 |
 *----------------------------------------------------------------------------*/
RDS_t* RDS_Scan(RDS_t* rds)
{
    register int i;
    register Job_t* job = &rds->job;
    TapeBuf_t* tapebuf;
    pthread_addr_t status;

    start_timer(&sum_timer, &sum_time);

    /* Read raw data into tape queue. */
    for (i = 0; i < rds->tapebuf_max; ++i) {
	if (!Q_Insert(&rds->tapebuf_Q, (tapebuf = &rds->tapebuf[i],&tapebuf))) {
	    stop_timer(&sum_timer, &sum_time);
	    return NULL;
	}
    }
    /* Move data to reader queue to begin scanning. */
    printfLLog(LOG_INFO, "Reader started job %d", job->job_id);
    while (Q_Remove(&rds->tapebuf_Q, &tapebuf));
    printfLLog(LOG_INFO, "Reader completed job %d", job->job_id);

    Q_Shutdown(&rds->scan_Q);
    for (i = 0; i < rds->scanner_max; ++i)
	pthread_join(rds->scanner[i], &status);

    /* Are we producing scan results only? */
    if (job->scan_job) {
	if (! RDS_Errno(rds)) {
	    start_timer(&sca_timer, &sca_time);

            /* Merge segments. */
	    RDS_SaveScanResult(rds);
	    stop_timer(&sca_timer, &sca_time)
	}
	stop_timer(&sum_timer, &sum_time)
    }
    else { /* This is a decode job. */

	Q_Shutdown(&rds->merge_Q);
	pthread_join(rds->merger, &status);
	stop_timer(&sum_timer, &sum_time)

	if (! RDS_Errno(rds)) {
	    register int i, n = 0;

	    /*  Make sure that one and only one good segment was found */
	    for (i = 0; i < rds->segmt_cnt; ++i) {
		register Segmt_t* sp = &rds->segmt[i];

		if (sp->pulse_cnt == 0) {
		    printfLLog(LOG_INFO,
			"%s %d/%.5d..%d/%.5d: %d frame syncs",
			sp->frame_cnt ? "SYN" : "GAP",
			BLKNUM(sp->start.blk,sp,job),
			BITNUM(sp->start.bit,sp,job),
			BLKNUM(sp->end.blk,sp,job),
			BITNUM(sp->end.bit,sp,job), sp->frame_cnt);
		    continue;
		}
		if (sp->type > 0) {
		    printfLLog(LOG_INFO, sp->burst_cnt > 1
? "CAL %d/%.5d..%d/%.5d: %d CAL-%d pulses (%d bursts)"
: "CAL %d/%.5d..%d/%.5d: %d CAL-%d pulses",
			BLKNUM(sp->start.blk,sp,job), 
			BITNUM(sp->start.bit,sp,job),
			BLKNUM(sp->end.blk,sp,job), 
			BITNUM(sp->end.bit,sp,job),
			sp->pulse_cnt, sp->type, sp->burst_cnt);
		    continue;
		}
		if (sp->type == -2) {
		    printfLLog(LOG_INFO, sp->burst_cnt > 1
? "SWT %d/%.5d..%d/%.5d: beam switch, %d pulses (%d bursts)"
: "SWT %d/%.5d..%d/%.5d: beam switch, %d pulses",
			BLKNUM(sp->start.blk,sp,job),
			BITNUM(sp->start.bit,sp,job),
			BLKNUM(sp->end.blk,sp,job),
			BITNUM(sp->end.bit,sp,job),
			sp->pulse_cnt, sp->burst_cnt);
		    continue;
		}
		/*  Only IMAGING segments from here on */

		if (sp->pulse_cnt < PULSE_PER_READ_MIN) {
		    printfLLog(LOG_INFO,
			"SAR %d/%.5d..%d/%.5d: %d pulses, too few",
			BLKNUM(sp->start.blk,sp,job), 
			BITNUM(sp->start.bit,sp,job),
			BLKNUM(sp->end.blk,sp,job), 
			BITNUM(sp->end.bit,sp,job), sp->pulse_cnt);
		    continue;
		}
		sp->mode = (*((Satellite_t*)job->sat)->Mode)(sp, job);

		printfLLog(
		(job->mode_check && sp->mode != job->mode) ? LOG_ERR :LOG_INFO,
		    sp->burst_cnt > 1
? "%s %d/%.5d..%d/%.5d: %d pulses (%d bursts)"
: "%s %d/%.5d..%d/%.5d: %d pulses",
		    sp->mode == MODE_INVALID ? "SAR" : 
		   (sp->mode==ES1 || sp->mode==ES2) ?"ERS":ModeImage(sp->mode),
		    BLKNUM(sp->start.blk,sp,job), BITNUM(sp->start.bit,sp,job),
		    BLKNUM(sp->end.blk,sp,job), BITNUM(sp->end.bit,sp,job),
		    sp->pulse_cnt, sp->burst_cnt);

		if (sp->mode != job->mode)
		    continue;
		++n;
	    }
	    if (n != 1) (*job->Log)(job->err,
		"Decode results are inconsistent with scan results");
	}
    }
    if (! job->scan_job)
	printfLLog(LOG_INFO,"Decode Files Write: %d.%d s.",
	    wrt_time.tv_sec, wrt_time.tv_usec);
    else {
	printfLLog(LOG_INFO,"Corner Calculation: %d.%d s.",
	    ppr_time.tv_sec, ppr_time.tv_usec);
	printfLLog(LOG_INFO,"Scan Results Write: %d.%d s.",
	    sca_time.tv_sec, sca_time.tv_usec);
    }
    printfLLog(LOG_INFO,"Total Elapsed Time: %d.%d s.",
	sum_time.tv_sec, sum_time.tv_usec);

    printfLLog(LOG_INFO,"   Throughput Rate: %e B/s. for job %d",
	(sum_time.tv_sec == 0 && sum_time.tv_usec == 0) ? 0.0 :
	((job->end.blk - job->start.blk + 1)/
	(sum_time.tv_sec + sum_time.tv_usec/1.0E6))*job->blk_size, job->job_id);

    return (RDS_Errno(rds) == 0 ? rds : NULL);
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_malloc - allocate and initialize RDS specific parameter structure
 |
 |  SYPNOSIS
 |      RDS_t* RDS_malloc (char* configfile, char* err)
 |
 |          configfile - configuration file name
 |          err        - points to error message if error return
 |
 |          returns    - pointer to allocated RDS specific parameter structure
 |                       else NULL if error
 |
 |  DESCRIPTION
 |      This routine will read in parameters from the configuration file
 |      and use them to allocate and initialize the RDS specific parameter
 |      structure.
 |
 *----------------------------------------------------------------------------*/
RDS_t* RDS_malloc (const char* configfile, char* err)
{
    register char *s, *t;
    register RDS_t* rds;
    register Job_t* job;
    register int i, k, m, dcrsi_csize, seek_timeout, seek_retry;
    register int tape_check = 'Y', mode_check = 'Y';
    int read_max, merge_max, segmt_max, pulse_max, burst_max, pcm_max;
    int dwp_max, agc_max, ppr_max, tapebuf_max, tapeblk_max, scanner_max;
    int dwp_per_burst_max, agc_per_burst_max, aux_recover_max = 0; 
    int frm_sync_diff_max, aux_sync_diff_max, pcm_sync_diff_max, gmt_diff_max;
    int burst_per_image_max, min_swa, min_swb, min_sna, min_snb, eco_pad;
    int tapeerr_max, cal_n_lines, n1, n2, n3, ncc;
    int e_sync_diff, j_sync_diff, r_sync_diff, tail_sync_diff;
    double cal_dc_mask, cal_ratio, aux_tdelta, lat_error;
    double e_window, j_window, e_dwp_a, e_dwp_b, j_dwp_a, j_dwp_b;
    ODL config_odl, odl, *ffc, *lat;
    int e;
    int num_exp_beam = 0;
    char **exp_beam, tape_dump[256];

    if ((config_odl = ODLparse((char*)configfile, 0, err)) == NULL) {
	char buf[512];
	sprintf(buf, "%s: %s", configfile, err);
	strncpy(err, buf, 255);
	err[255] = 0;
	return NULL;
    }
    odl = Value(config_odl);

    /* Retrieve parameters from RDS configuration file. */
    if (
	(scanner_max = ODLGetInt(odl, s = "THREAD_MAX", &e),
	 scanner_max = (e != -1 ? scanner_max : 2),
	 scanner_max < 1) ||
	(tapebuf_max = ODLGetInt(odl, s = "TAPEBUF_MAX", &e),
	 tapebuf_max = (e != -1 ? tapebuf_max : 5),
	 tapebuf_max < scanner_max+3) ||
	(tapeblk_max = ODLGetInt(odl, s = "TAPEBLK_MAX", &e),
	 tapeblk_max = (e != -1 ? tapeblk_max : 2600000),
	 tapeblk_max < 1) ||
	(tapeerr_max = ODLGetInt(odl, s ="TAPEERR_MAX", &e),
	 tapeerr_max = (e != -1 ? tapeerr_max : 65536),
	 tapeerr_max < 0) ||
	(burst_per_image_max = ODLGetInt(odl, s = "BURST_PER_IMAGE_MAX", &e),
	 burst_per_image_max = (e != -1 ? burst_per_image_max : 1600),
	 burst_per_image_max < 512) ||
	(dwp_per_burst_max = ODLGetInt(odl, s = "DWP_PER_BURST_MAX", &e),
	 dwp_per_burst_max = (e != -1 ? dwp_per_burst_max : 8),
	 dwp_per_burst_max < 1) ||
	(agc_per_burst_max = ODLGetInt(odl, "AGC_PER_BURST_MAX", &e),
	 agc_per_burst_max = (e != -1 ? agc_per_burst_max : 8),
	 agc_per_burst_max < 1) ||
	(ppr_max = ODLGetInt(odl, s = "PPR_MAX", &e),
	 ppr_max = (e != -1 ?  ppr_max : 100),
	 ppr_max < 1) ||
	(frm_sync_diff_max = ODLGetInt(odl, s = "FRM_SYNC_MISS_MAX", &e),
	 frm_sync_diff_max = (e != -1 ? frm_sync_diff_max : 3),
	 frm_sync_diff_max < 0) ||
	(aux_sync_diff_max = ODLGetInt(odl, s = "AUX_SYNC_MISS_MAX", &e),
	 aux_sync_diff_max = (e != -1 ? aux_sync_diff_max : 2),
	 aux_sync_diff_max < 0) ||
	(pcm_sync_diff_max = ODLGetInt(odl, s = "PCM_SYNC_MISS_MAX", &e),
	 pcm_sync_diff_max = (e != -1 ? pcm_sync_diff_max : 0),
	 pcm_sync_diff_max < 0) ||
	(e_sync_diff = ODLGetInt(odl, s = "E1_SYNC_MISS_MAX", &e),
	 e_sync_diff = (e != -1 ? e_sync_diff : 2),
	 e_sync_diff < 0) ||
	(r_sync_diff = ODLGetInt(odl, s = "R1_SYNC_MISS_MAX", &e),
	 r_sync_diff = (e != -1 ? r_sync_diff : frm_sync_diff_max),
	 r_sync_diff < 0) ||
	(j_sync_diff = ODLGetInt(odl, s = "J1_SYNC_MISS_MAX", &e),
	 j_sync_diff = (e != -1 ? j_sync_diff : 2),
	 j_sync_diff < 0) ||
	(tail_sync_diff = ODLGetInt(odl, s = "TAIL_SYNC_MISS_MAX", &e),
	 tail_sync_diff = (e != -1 ? tail_sync_diff : 1),
	 tail_sync_diff < 0) ||
	(aux_recover_max = ODLGetInt(odl, s = "AUX_RECOVER_MAX", &e),
	 aux_recover_max = (e != -1 ? aux_recover_max : 5),
	 aux_recover_max < 1) ||
	(seek_timeout = ODLGetInt(odl, s = "SEEK_TIMER", &e),
	 seek_timeout = (e != -1 ? seek_timeout: 100),
	 seek_timeout < 0) ||
	(seek_retry = ODLGetInt(odl, s = "SEEK_RETRY", &e),
	 seek_retry = (e != -1 ? seek_retry : 5),
	 seek_retry < 1) ||
	(dcrsi_csize = ODLGetInt(odl, s = "DCRSI_CMD_CSIZE", &e),
	 dcrsi_csize = (e != -1 ? dcrsi_csize : 8),
	 dcrsi_csize < 7 || dcrsi_csize > 8) ||
	(aux_tdelta = ODLGetDouble(odl, s = "SC_TIME_DELTA", &e),
	 aux_tdelta = (e != -1 ? aux_tdelta : 3.8e-04),
	 aux_tdelta < 1.0e-04) ||
	(cal_n_lines = ODLGetInt(odl, s = "CAL_N_LINES", &e),
	 cal_n_lines = (e != -1 ? cal_n_lines : 12),
	 cal_n_lines < 1) ||
	(cal_dc_mask = ODLGetDouble(odl, s = "CAL_DC_MASK", &e),
	 cal_dc_mask = (e != -1 ? cal_dc_mask : 0.1),
	 cal_dc_mask < 0.01) ||
	(cal_ratio = ODLGetDouble(odl, s ="CAL_THRESHOLD_RATIO", &e),
	 cal_ratio = (e != -1 ? cal_ratio : 2.0),
	 cal_ratio < 1.0) ||
	(lat_error = ODLGetDouble(odl, s ="LAT_ERROR", &e),
	 lat_error = (e != -1 ? lat_error : 0.75),
	 lat_error < 0.1) ||
	(window_duration_error = ODLGetDouble(odl, s ="WIN_ERROR", &e),
	 window_duration_error = (e != -1 ? window_duration_error : 44.0e-6),
	 window_duration_error <= 0) ||
	(eco_pad = ODLGetInt(odl, s ="ECHO_PADDING", &e),
	 eco_pad = (e != -1 ? eco_pad : 30), 
	 eco_pad < 0) ||
	(min_swa = ODLGetInt(odl, s = "SWA_MIN_BURST", &e),
	 min_swa = (e != -1 ? min_swa : 1448), 
	 min_swa < 144) ||
	(min_swb = ODLGetInt(odl, s = "SWB_MIN_BURST", &e),
	 min_swb = (e != -1 ? min_swb : 1448), 
	 min_swb < 144) ||
	(min_sna = ODLGetInt(odl, s = "SNA_MIN_BURST", &e),
	 min_sna = (e != -1 ? min_sna : 524), 
	 min_sna < 52) ||
	(min_snb = ODLGetInt(odl, s = "SNB_MIN_BURST", &e),
	 min_snb = (e != -1 ? min_snb : 666), 
	 min_snb < 66) ||
	(gmt_diff_max = ODLGetInt(odl, s = "GMT_DIFF_MAX",  &e),
	 gmt_diff_max = (e != -1 ? gmt_diff_max : (15*60)),
	 gmt_diff_max < 0) ||
	(e_window = ODLGetInt(odl, s = "FRAME_PARAM.E1.WINDOW_DURATION", &e),
	 e_window = (e != -1 ? e_window : ((5616-702)/18.96E6)),
	 e_window <= 0) ||
	(j_window = ODLGetInt(odl, s = "FRAME_PARAM.J1.WINDOW_DURATION", &e),
	 j_window = (e != -1 ? j_window : ((5616-702)/18.96E6)),
	 j_window <= 0) ||
	(e_dwp_a = ODLGetInt(odl, s = "FRAME_PARAM.E1.DWP_A", &e),
	 e_dwp_a = (e != -1 ? e_dwp_a : 6.6E-6), 0) ||
	(e_dwp_b = ODLGetInt(odl, s = "FRAME_PARAM.E1.DWP_B", &e),
	 e_dwp_b = (e != -1 ? e_dwp_b : 210.94E-9), e_dwp_b < 0) ||
	(j_dwp_a = ODLGetInt(odl, s = "FRAME_PARAM.J1.DWP_A", &e),
	 j_dwp_a = (e != -1 ? j_dwp_a : 6.6E-6), 0) ||
	(j_dwp_b = ODLGetInt(odl, s = "FRAME_PARAM.J1.DWP_B", &e),
	 j_dwp_b = (e != -1 ? j_dwp_b : 210.94E-9), j_dwp_b < 0) ||
	(lat = Val(Lookup(odl, s = "NOMINAL_LOOK_ANGLES"))) == NULL ||
	(ffc = Val(Lookup(odl, s = "FIXED_FRAME_CENTERS"))) == NULL) {

	sprintf(err, "can't retrieve %s from %s", s, configfile);
	ODLFree(config_odl);
 	return NULL;
    }
    tape_check = (t = ODLGetStr(odl,"TAPE_CHECK")) && strcasecmp(t,"YES") == 0
		 ? 'Y' : 'N';
    mode_check = (t = ODLGetStr(odl,"MODE_CHECK")) && strcasecmp(t,"YES") == 0
		 ? 'Y' : 'N';
    strcpy(tape_dump, (t = ODLGetStr(odl,"TAPE_DUMP")) ? t : "");

    /* Check if the experimental beam mapping table is defined. */
    num_exp_beam = ODLGetStringArray(odl, s = "EXPERIMENTAL_BEAM_MAPPING",
        &exp_beam);

    /*--------------------------------*
     |  Get nominal look angle table 
     *--------------------------------*/
    for (i = 0; i < BEAM_MAX; ++i)
	nominal_swath[i] = 0;

    if ((nominal_swath[BEAM_S1] = ODLGetDouble(odl,
	s = "NOMINAL_SWATH_WIDTH.BEAM_S1", &e), e == -1) ||
	(nominal_swath[BEAM_S2] = ODLGetDouble(odl,
	s = "NOMINAL_SWATH_WIDTH.BEAM_S2", &e), e == -1) ||
	(nominal_swath[BEAM_S3] = ODLGetDouble(odl,
	s = "NOMINAL_SWATH_WIDTH.BEAM_S3", &e), e == -1) ||
	(nominal_swath[BEAM_S4] = ODLGetDouble(odl,
	s = "NOMINAL_SWATH_WIDTH.BEAM_S4", &e), e == -1) ||
	(nominal_swath[BEAM_S5] = ODLGetDouble(odl,
	s = "NOMINAL_SWATH_WIDTH.BEAM_S5", &e), e == -1) ||
	(nominal_swath[BEAM_S6] = ODLGetDouble(odl,
	s = "NOMINAL_SWATH_WIDTH.BEAM_S6", &e), e == -1) ||
	(nominal_swath[BEAM_S7] = ODLGetDouble(odl,
	s = "NOMINAL_SWATH_WIDTH.BEAM_S7", &e), e == -1)) {

	sprintf(err, "can't retrieve %s from %s", s, configfile);
	ODLFree(config_odl);
	return NULL;
    }
    /*--------------------------------*
     |  Get nominal look angle table 
     *--------------------------------*/

    for (i = 0; i < LOOKTABLE_MAX; ++i)
	looktable[i].rmin = looktable[i].rmax = -1;
    k = 0;

    for (i = 0; lat[i] != NULL; ++i) {
	register LookTable_t* tp = &looktable[k];
	odl = lat[i];

	if (strcasecmp(Name(odl), "LOOK_ANGLE_TABLE"))
	    continue;

	if (k == LOOKTABLE_MAX) {
	    sprintf(err, "Too many nominal look angle tables (%d max)\n",
		    LOOKTABLE_MAX);
	    ODLFree(config_odl);
	    return NULL;
	}
        /* Fill in the look angle table. */
	if ((tp->rmin = ODLGetInt(odl, s ="MIN_REVOLUTION", &e), e == -1) ||
	    (tp->rmax = ODLGetInt(odl, s ="MAX_REVOLUTION", &e), e == -1) ||
	    (tp->beam[BEAM_S1] = ODLGetDouble(odl, s="BEAM_S1", &e), e == -1) ||
	    (tp->beam[BEAM_S2] = ODLGetDouble(odl, s="BEAM_S2", &e), e == -1) ||
	    (tp->beam[BEAM_S3] = ODLGetDouble(odl, s="BEAM_S3", &e), e == -1) ||
	    (tp->beam[BEAM_S4] = ODLGetDouble(odl, s="BEAM_S4", &e), e == -1) ||
	    (tp->beam[BEAM_S5] = ODLGetDouble(odl, s="BEAM_S5", &e), e == -1) ||
	    (tp->beam[BEAM_S6] = ODLGetDouble(odl, s="BEAM_S6", &e), e == -1) ||
	    (tp->beam[BEAM_S7] = ODLGetDouble(odl, s="BEAM_S7", &e), e == -1) ||
	    (tp->beam[BEAM_W1] = ODLGetDouble(odl, s="BEAM_W1", &e), e == -1) ||
	    (tp->beam[BEAM_W2] = ODLGetDouble(odl, s="BEAM_W2", &e), e == -1) ||
	    (tp->beam[BEAM_W3] = ODLGetDouble(odl, s="BEAM_W3", &e), e == -1) ||
	    (tp->beam[BEAM_W2R]= ODLGetDouble(odl, s="BEAM_W2R",&e), e == -1) ||
	    (tp->beam[BEAM_EL1]= ODLGetDouble(odl, s="BEAM_EL1",&e), e == -1) ||
	    (tp->beam[BEAM_EH2]= ODLGetDouble(odl, s="BEAM_EH2",&e), e == -1) ||
	    (tp->beam[BEAM_EH4]= ODLGetDouble(odl, s="BEAM_EH4",&e), e == -1) ||
	    (tp->beam[BEAM_EH6]= ODLGetDouble(odl, s="BEAM_EH6",&e), e == -1) ||
	    (tp->beam[BEAM_F1] = ODLGetDouble(odl, s="BEAM_F1", &e), e == -1) ||
	    (tp->beam[BEAM_F2] = ODLGetDouble(odl, s="BEAM_F2", &e), e == -1) ||
	    (tp->beam[BEAM_F3] = ODLGetDouble(odl, s="BEAM_F3", &e), e == -1) ||
	    (tp->beam[BEAM_F4] = ODLGetDouble(odl, s="BEAM_F4", &e), e == -1) ||
	    (tp->beam[BEAM_F5] = ODLGetDouble(odl, s="BEAM_F5", &e), e == -1) ||
	    (tp->beam[BEAM_E1] = ODLGetDouble(odl, s="BEAM_E1", &e), e == -1) ||
	    (tp->beam[BEAM_E2] = ODLGetDouble(odl, s="BEAM_E2", &e), e == -1) ||
	    (tp->beam[BEAM_J1] = ODLGetDouble(odl, s="BEAM_J1", &e), e == -1)) {

	    sprintf(err, "can't retrieve %s from %s", s, configfile);
	    ODLFree(config_odl);
	    return NULL;
	}
	++k;
    }
    if (k == 0) {
	strcpy(err, "Look angle table(s) invalid or missing\n");
	ODLFree(config_odl);
	return NULL;
    }
    /*---------------------------*
     |  Get fixed framed tables
     *---------------------------*/

    for (i = 0; i < FFC_MAX; ++i)
	fix_ctr[i].mode = MODE_INVALID;
    k = 0;

    for (i = 0; ffc[i] != NULL; ++i) {
	register char *t;
	register FFC_t* fp = &fix_ctr[k];

	if (strcasecmp(Name(ffc[i]), "FRAME_TABLE"))
	    continue;

	if (k == FFC_MAX) {
	    sprintf(err, "Too many fixed frame tables (%d max)\n", FFC_MAX);
	    ODLFree(config_odl);
	    return NULL;
	}	
	if ((s = ODLGetStr(ffc[i], "FRAME_MODE")) == NULL) {
	    strcpy(err, "FRAME_MODE missing from frame table\n");
	    ODLFree(config_odl);
	    return NULL;
	}	
	if (! strcmp(s, "ARCTIC"))
	    fp->left = 0;
	else if (! strcmp(s, "ANTARCTIC"))
	    fp->left = 1;
	else {
	    sprintf(err, "Invalid FRAME_MODE (%s) in frame table\n", s);
	    ODLFree(config_odl);
	    return NULL;
	}	
	if ((t = ODLGetStr(ffc[i], "PLATFORM")) == NULL) {
	    strcpy(err, "PLATFORM missing from frame table\n");
	    ODLFree(config_odl);
	    return NULL;
	}	
	if (strcmpv(t, "R1", "E1", "E2", "J1", 0) < 0) {
	    sprintf(err, "Invalid PLATFORM (%s) in frame table\n", s);
	    ODLFree(config_odl);
	    return NULL;
	}	
	if ((s = ODLGetStr(ffc[i], "MODE")) == NULL) {
	    strcpy(err, "MODE missing from frame table\n");
	    ODLFree(config_odl);
	    return NULL;
	}	
	for (m = ST1; m < MODE_MAX; ++m)
	    if (! strcmp(s, ModeImage(m))) {
		if (strcasecmp(t, "R1"))
		    m = (!strcasecmp(t, "E1") ? ES1 :
			(!strcasecmp(t, "E2") ? ES2 : JS1));
		break;
	    }
	if (m == MODE_MAX) {
	    sprintf(err, "Invalid MODE (%s) in %s frame table\n", s, t);
	    ODLFree(config_odl);
	    return NULL;
	}	
	fp->mode = (Mode_t) m;

	if ((fp->rmin = ODLGetInt(ffc[i], "MIN_REVOLUTION", &e), e == -1) ||
	    fp->rmin < 0) {
	    sprintf(err,
	    "MIN_REVOLUTION invalid or missing from %s/%s frame table\n",t,s);
	    ODLFree(config_odl);
	    return NULL;
	}	
	if ((fp->rmax = ODLGetInt(ffc[i], "MAX_REVOLUTION", &e), e == -1) ||
	    fp->rmax < 0 || fp->rmax < fp->rmin) {
	    sprintf(err,
	    "MAX_REVOLUTION invalid or missing from %s/%s frame table\n",t,s);
	    ODLFree(config_odl);
	    return NULL;
	}
	if ((fp->azim = ODLGetDouble(ffc[i],"AZIMUTH_SWATH_WIDTH",&e),e == -1)||
	    fp->azim <= 0.0) {
	    sprintf(err,
	"AZIMUTH_SWATH_WIDTH invalid or missing from %s/%s frame table\n",t,s);
	    ODLFree(config_odl);
	    return NULL;
	}	
	if (!ODLGetArrayDouble(ffc[i], "CENTER_LATITUDES", fp->lat,
		(e = 1, &e), (fp->cnt = LAT_MAX, &fp->cnt))) {
	    sprintf(err,
	"CENTER_LATITUDES invalid or missing from %s/%s frame table\n",t,s);
	    ODLFree(config_odl);
	    return NULL;
	}
	++k;
    }
    if (k == 0) {
	strcpy(err, "Frames table(s) invalid or missing\n");
	ODLFree(config_odl);
	return NULL;
    }
    ODLFree(config_odl);

    /*-----------------------------*
     |  Allocate required memory 
     *-----------------------------*/
    read_max  = (tapeblk_max + DCRSI_READ_SIZE/DCRSI_BLOCK_SIZE-1)/
		(DCRSI_READ_SIZE/DCRSI_BLOCK_SIZE);
    merge_max = read_max;
    segmt_max = read_max * SEGMT_PER_READ_MAX;
    pulse_max = read_max * PULSE_PER_READ_MAX;
    burst_max = read_max * BURST_PER_READ_MAX;
    pcm_max   = read_max * ((PCM_BIT_PER_READ_MAX+31)/8);
    dwp_max  = burst_max * dwp_per_burst_max;
    agc_max  = burst_max * agc_per_burst_max;
	
    n1 = RoundUp(sizeof(RDS_t) + 
	 (tapebuf_max+1) * sizeof(TapeBuf_t*) * 2 +
	 (merge_max+1) * sizeof(Merge_t*) +
	 pulse_max * sizeof(Pulse_t*) +
	 burst_max * sizeof(U32) +
	 dwp_max * sizeof(U32) +
	 agc_max * sizeof(U32) +
	 pcm_max,
	 16);
    n2 = RoundUp(n1 +
	 RoundUp(scanner_max * sizeof(pthread_t), 16) +
	 RoundUp(merge_max * sizeof(Merge_t), 16) +
	 RoundUp(segmt_max * sizeof(Segmt_t), 16) +
	 RoundUp(pulse_max * PULSE_SIZE_MAX, 16) +
	 RoundUp(ppr_max * sizeof(PPR_t), 16),
	 512);
    n3 = n2 + RoundUp(tapebuf_max * sizeof(TapeBuf_t), 512);

#ifdef	DEBUG
    printf("tapeblk_max %d\n", tapeblk_max);
    printf("tapebuf_max %d\n", tapebuf_max);
    printf("scanner_max %d\n", scanner_max);
    printf("merge_max %d\n", merge_max);
    printf("segmt_max %d\n", segmt_max);
    printf("pulse_max %d\n", pulse_max);
    printf("burst_max %d\n", burst_max);
    printf("dwp_max %d\n", dwp_max);
    printf("agc_max %d\n", agc_max);
    printf("pcm_max %d\n", pcm_max);
    printf("ppr_max %d\n", ppr_max);
    printf("frm_sync_diff_max %d\n", frm_sync_diff_max);
    printf("aux_sync_diff_max %d\n", aux_sync_diff_max);
    printf("pcm_sync_diff_max %d\n", pcm_sync_diff_max);
    printf("dwp_per_burst_max %d\n", dwp_per_burst_max);
    printf("agc_per_burst_max %d\n", agc_per_burst_max);
    printf("cal_n_lines %d\n", cal_n_lines);
    printf("cal_dc_mask %f\n", cal_dc_mask);
    printf("cal_ratio %f\n", cal_ratio);
    printf("burst_per_image_max %d\n", burst_per_image_max);
    printf("n1 %d\n", n1);
    printf("n2 %d\n", n2);
    printf("Total %d\n", n3);
    fflush(stdout);
#endif
    if (! (rds = (RDS_t*) memalign(512, n3))) {
	strcpy(err, "No memory for RDS_t object");
	return NULL;
    }
    memset(rds, 0, sizeof(RDS_t));

    rds->scanner_max = scanner_max;
    rds->tapebuf_max = tapebuf_max;
    rds->tapeblk_max = tapeblk_max;
    rds->merge_max = merge_max;
    rds->segmt_max = segmt_max;
    rds->pulse_max = pulse_max;
    rds->burst_max = burst_max;
    rds->dwp_max = dwp_max;
    rds->agc_max = agc_max;
    rds->pcm_max = pcm_max;
    rds->ppr_max = ppr_max;
    rds->agc_per_burst_max = agc_per_burst_max;
    rds->dwp_per_burst_max = dwp_per_burst_max;
    rds->burst_per_image_max = burst_per_image_max;

    job = &rds->job;
    job->e_win_duration = e_window;
    job->e_dwp_a = e_dwp_a;
    job->e_dwp_b = e_dwp_b;
    job->j_win_duration = j_window;
    job->j_dwp_a = j_dwp_a;
    job->j_dwp_b = j_dwp_b;
    job->sat = NULL;
    job->sat_data = NULL;
    job->Log = (int (*)()) Status_Log;
    job->err = (void*) &rds->status;
    job->min_burst_swa = min_swa;
    job->min_burst_swb = min_swb;
    job->min_burst_sna = min_sna;
    job->min_burst_snb = min_snb;
    job->min_pulse_per_read = PULSE_PER_READ_MIN;
    job->max_burst_per_image = burst_per_image_max;
    job->sync_diff = frm_sync_diff_max;
    job->aux_diff = aux_sync_diff_max;
    job->pcm_diff = pcm_sync_diff_max;
    job->e_sync_diff = e_sync_diff;
    job->j_sync_diff = j_sync_diff;
    job->r_sync_diff = r_sync_diff;
    job->tail_diff = tail_sync_diff;
    job->aux_recover = aux_recover_max ? aux_recover_max : 5;
    job->gmt_diff_max = gmt_diff_max;
    job->dcrsi_csize = dcrsi_csize;
    job->seek_timeout = seek_timeout;
    job->seek_retry = seek_retry;
    job->aux_tdelta = aux_tdelta;
    job->lat_error = lat_error;
    job->eco_pad = eco_pad;
    job->tape_errlim = ((tapeerr_max+511)/512)*512;
    job->tape_check = (tape_check == 'Y' || tape_check == 'y');
    job->mode_check = (mode_check == 'Y' || mode_check == 'y');
    rds->tapebuf_Q_ = (TapeBuf_t**) (rds + 1);
    rds->scan_Q_ = (Merge_t**) (rds->tapebuf_Q_ + tapebuf_max + 1);
    rds->merge_Q_ = (rds->scan_Q_ + tapebuf_max + 1);
    rds->pulse = (Pulse_t**) (rds->merge_Q_ + merge_max + 1);
    rds->burst = (int*) (rds->pulse + pulse_max);
    rds->dwp = rds->burst + burst_max;
    rds->agc = rds->dwp + dwp_max;
    rds->pcm = job->pcm = (U32*)(rds->agc + agc_max);

    rds->scanner = (pthread_t*) ((char*)rds + n1);
    rds->merge = (Merge_t*)
	((char*) rds->scanner + RoundUp(scanner_max * sizeof(pthread_t), 16));
    rds->segmt = (Segmt_t*)
	((char*) rds->merge + RoundUp(merge_max * sizeof(Merge_t), 16));
    rds->pulsebuf = 
	((char*) rds->segmt + RoundUp(segmt_max * sizeof(Segmt_t), 16));
    rds->ppr = (PPR_t*)
	((char*) rds->pulsebuf + RoundUp(pulse_max * PULSE_SIZE_MAX, 16));
    rds->tapebuf = (TapeBuf_t*) ((char*)rds + n2);
    rds->sca_cache = NULL;
    rds->pulse_to_check = rds->tapeerr = 0;

    rds->tapedump_err = 0;
    rds->tapedump_fd = -1;
    rds->tapedump_nf = 1;
    rds->tapedump_nb = 0;
    strcpy(rds->tapedump, tape_dump);

    /*----------------------------------------*
     |  Check experimental beam mapping.
     *----------------------------------------*/
    if (num_exp_beam == 4) {

        /* For each experimental beam slot. */
        for (i = 0; i < 4; i++) {
            num_exp_beam = 0;  /* set to invalid */

            /* Check against each valid mode. */
            for (k = 0; k < 7; k++) {
                if (! strcmp(exp_beam[i], ModeImage(EL1+k))) {
                    /* Store this mapping. */
                    job->exp_beam[i] = EL1+k;
                    num_exp_beam = 4; /* valid */
                    break;
                }
            }
            /*-----------------------------------------------------*
             | As soon as we find an invalid beam break out and
             | use the defaults.
             *-----------------------------------------------------*/
            if (num_exp_beam != 4)
                break;
        }
    }
    if (num_exp_beam != 4) {
        syslog(LOG_INFO, "Experimental Beam Mapping Table format error. "
                         "Using default settings.\n");
        /* Use default mapping. */
        job->exp_beam[0] = EH1;
        job->exp_beam[1] = EH3;
        job->exp_beam[2] = EH4;
        job->exp_beam[3] = EH6;
    }
    else {
        /* Check for duplicate entries. */
        if (job->exp_beam[0] == job->exp_beam[1] ||
            job->exp_beam[0] == job->exp_beam[2] ||
            job->exp_beam[0] == job->exp_beam[3] ||
            job->exp_beam[1] == job->exp_beam[2] ||
            job->exp_beam[1] == job->exp_beam[3] ||
            job->exp_beam[2] == job->exp_beam[3])
            syslog(LOG_INFO, "Experimental Beam Mapping Table duplicate"
                             " entry.\n");
    }
    if (num_exp_beam != -1)
        free(exp_beam);

    if (! Status_Init(&rds->status)) {
	free(rds);
	rds = NULL;
    }
    if (! Q_Init(&rds->tapebuf_Q, rds->tapebuf_Q_, rds->tapebuf_max+1, NULL,
		 NULL, NULL, RDS_read, rds, NULL, "tapebuf_Q")) {
	strcpy(err, "Can't create Tape Buffer Queue\n");
	Status_Destroy(&rds->status);
	free(rds);
	return NULL;
    }
    if (! Q_Init(&rds->scan_Q, rds->scan_Q_, rds->tapebuf_max+1, NULL,
		 NULL, NULL, RDS_scan, rds, NULL, "scan_Q")) {
	strcpy(err, "Can't create Scanner Queue\n");
	Status_Destroy(&rds->status);
	Q_Destroy(&rds->tapebuf_Q);
	free(rds);
	return NULL;
    }
    if (! Q_Init(&rds->merge_Q, rds->merge_Q_, rds->merge_max+1, Segmt_cmp,
		 NULL, NULL, RDS_decode, rds, RDS_merge_check, "merge_Q")) {
	strcpy(err, "Can't create Writer Queue\n");
	Status_Destroy(&rds->status);
	Q_Destroy(&rds->scan_Q);
	Q_Destroy(&rds->tapebuf_Q);
	free(rds);
	return NULL;
    }
    return rds;
}

/*----------------------------------------------------------------------------*
 |  NAME
 |      RDS_free - free RDS allocated memory
 |
 |  SYPNOSIS
 |      void RDS_free(RDS_t* rds)
 |
 |          rds - RDS specific parameter structure
 |
 |  DESCRIPTION
 |      This routine will free the memory of the RDS specific parameter
 |      structure.
 |
 *----------------------------------------------------------------------------*/
void RDS_free(RDS_t* rds)
{
    free(rds);
}
