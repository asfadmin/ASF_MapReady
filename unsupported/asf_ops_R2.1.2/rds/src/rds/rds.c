/*=============================================================================
 |  @(#)rds.c	2.51 98/03/15 12:04:47
 |
 |  Multithreaded Raw Data Scanner (RDS) Main Program.
 |  Copyright (C) Jet Propulsion Laboratory.
 |  Alaska SAR Facility (ASF) Project.
 |
 *============================================================================*/
#include <sys/types.h>
#include <syslog.h>
#include <time.h>
#include "RDS.h"
#include "logUtils.h"
#include "asf_syslog.h"
#include "asf.h"
#include "version.h"

static const char sccsid_rds_c[] =
        "@(#)rds.c	2.51 98/03/15 12:04:47";

static const char copyright[] = "\
Copyright (C) 1996, California Institute of Technology.  \
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.";

static int processing = 0;
static int resetting = 0;
static int off_line = 0;

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
static
int SendAckToServer (ODL msg, char* ack_type, char* status, char* text)
{
    char s[80];
    ODL ack;
    register int n, c = 0;
    if (off_line) return 0;

    if (! (ack = GetAckMsg(msg, ack_type))) {
        printfLLog(LOG_ERR, "SendAckToServer() can't retrieve %s", ack_type);
        return -1;
    }
    n = strlen(text)-1;
    if (text[n] == '\n') {
	c = text[n]; text[n] = 0;
    }
    ODLSetVal(ack, (sprintf(s,"%s.BODY.STATUS", ack_type),s), status);
    ODLSetVal(ack, (sprintf(s,"%s.BODY.COMMENT",ack_type),s), text);

    if (c) text[n] = c;

    if (!strcmp(ack_type, "SUBSYSTEM_STATUS"))
        ODLSetVal(ack, (sprintf(s,"%s.BODY.SUB_TYPE",ack_type),s), "HEARTBEAT");

    if (WriteMsgToServer(ack) == -1) {
	printfLLog(LOG_ERR, "Unable to send %s to CP", ack_type);
	ODLFree(ack);
	return -1;
    }
    printfLLog(LOG_DEBUG, "%s sent to CP", ack_type);
    ODLFree(ack);
    return 0;
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
static
void heartbeat_handler(ODL msg, RDS_t* rds)
{
    int  blk;
    char buf[80];
    
    if (resetting) 
        sprintf(buf, "Resetting job %d...", rds->job.job_id);

    else if (! processing ||
       (blk = RDS_Position(rds) - rds->job.start.blk,
	blk >= rds->job.end.blk - rds->job.start.blk))

	sprintf(buf, rds->pulse_to_check
	    ? "Looking for PCM SYNC to determine segment time..."
	    : "Waiting for a processing request...", rds->pulse_to_check);
    else if (blk == 0)

        sprintf (buf, "Seeking block #%d", rds->job.start.blk);
    else
        sprintf(buf, "%d%% completed - %s %d..%d", (int)
                ((blk * 100.0) / (rds->job.end.blk - rds->job.start.blk)),
		rds->job.scan_job ? "Scanning" : "Decoding",
                BLKNUM(rds->job.start.blk+blk, &rds->job, &rds->job),
		BLKNUM(rds->job.end.blk, &rds->job, &rds->job));
    SendAckToServer(msg, "SUBSYSTEM_STATUS", "COMPLETED", buf);
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
static
void job_handler(ODL msg, RDS_t* rds)
{
    const Job_t* job = &rds->job;

    SendAckToServer(msg, "SUBSYSTEM_ACK", "COMPLETED", "Greetings from RDS!");
    processing = 1;

    if (RDS_Init(rds, Value(msg))) {
	RDS_Scan(rds);
	RDS_Destroy(rds);
    }
    processing = 0;

    if (! resetting) {
	char  err[256];
        const char* cp = RDS_Error(rds);
	const char* np = strchr(cp, '\n');
	const int n = (!np || !(np = strchr(np+1,'\n'))) ? strlen(cp) : (np-cp);

	memcpy(err, cp, n>sizeof(err)-1 ? sizeof(err)-1 : n);
	err[n] = 0;
	
	if (err[0])
	    printfLLog(LOG_ERR, "Job %d done - %s", rds->job.job_id, err);
	else
	    printfLLog(LOG_ERR, "Job %d done - OK", rds->job.job_id);

	SendAckToServer(msg, "SUBSYSTEM_COMPLETED", 
	    (strstr(cp, "tape label") ? "WRONG_TAPE" :
	    (RDS_Errno(rds) == -1 ? "CANCEL/FAIL" : "COMPLETED")), err);
    }
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
static
void cleanup_handler(ODL msg, RDS_t* rds)
{
    char s[64], err[256], *p, *ack_type = "SUBSYSTEM_STATUS";
    register ODL ack = Value(msg);
    register char* status =
	RDS_Cleanup( rds,
	    ((p = ODLGetStr(ack, "BODY.SAVE_FLAG")) && !strcasecmp(p, "YES") ?
	    ODLGetStr(ack, "BODY.SAVE_DIR") : NULL), err)
	== -1 ? "CANCEL/FAIL" : "COMPLETED";

    if (! (ack = GetAckMsg(msg, ack_type))) {
        printfLLog(LOG_ERR, "cleanup_handler: can't retrieve %s", ack_type);
        return;
    }
    ODLSetVal(ack, (sprintf(s,"%s.BODY.STATUS",ack_type), s), status);
    ODLSetVal(ack, (sprintf(s,"%s.BODY.COMMENT",ack_type), s), err);
    ODLSetVal(ack, (sprintf(s,"%s.BODY.SUB_TYPE",ack_type),s), "CLEANUP");

    WriteMsgToServer(ack);
    ODLFree(ack);
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
static
void reset_handler(ODL msg, RDS_t* rds)
{
    resetting = 1;
    if (processing) {
	printfLLog(LOG_ERR, "Cancelling job %d... ", rds->job.job_id);
	RDS_Reset(rds);
	printfLLog(LOG_ERR, "Job %d cancelled", rds->job.job_id);
    }
    resetting = 0;
}

/*============================================================================*
 |  NAME
 |
 |  SYPNOSIS
 |
 |  DESCRIPTION
 |
 *============================================================================*/
int main(int argc, char *argv[])
{
    RDS_t* rds;
    time_t clock;
    char err[256], ts[28];
    register int i = 0;

    ctime_r((time(&clock), &clock), ts, sizeof(ts));
    ts[strlen(ts)-1] = 0;
    openlog(argv[0], LOG_PID|LOG_CONS|LOG_NDELAY, LOG_SPS);

    while (++i < argc && strcmp(argv[i], "-configfile"));
    if (++i >= argc) {
	syslog(LOG_ERR, "%s: configfile parameter missing", version_id);
	printf("%s %s: configfile parameter missing\n", ts, version_id);
	exit(-1);
    }
    if (! (rds = RDS_malloc(argv[i], err))) {
        syslog(LOG_ERR, "%s: %s",  version_id, err);
        printf("%s %s: %s\n",  ts, version_id, err);
        exit(-1);
    }
    for (i = 1; i < argc && strcmp(argv[i], "-jobfile"); ++i);
    if (i == argc) {
	/*  We are running with CP */

	AsfApp app;
	if (! (app = AsfAppInitialize("RDS", &argc, argv))) {
	    syslog(LOG_ERR, "%s: AsfAppInitialize() failed.",  version_id);
	    printf("%s %s: AsfAppInitialize() failed.\n",  ts, version_id);
	    exit (-1);
	}
	printfLLog(LOG_INFO, "%s - Started %s...", version_id, ts);
	printfLLog(LOG_INFO, "%s", copyright);

	AsfAddCallback(app, "SPS_SCAN_REQUEST", job_handler, rds,
	    "SPS_DECODE_REQUEST", job_handler, rds,
	    "SUBSYSTEM_HEARTBEAT", heartbeat_handler, rds,
            "SUBSYSTEM_HALT", reset_handler, rds,
            "SUBSYSTEM_STOP", reset_handler, rds,
            "CLEANUP_REQUEST", cleanup_handler, rds,
	    NULL);
	AsfAppMainLoop(app);
    }
    else if (++i >= argc) {
	syslog(LOG_ERR, "%s: jobfile parameter missing", version_id);
	printf("%s %s: jobfile parameter missing\n", ts, version_id);
	exit(-1);
    }
    else { /*  We are running without CP */

	register int k;
	ODL odl;

	if (initASFlog(argc, argv, "RDS") == -1) {
	    syslog(LOG_ERR, "%s: initASFlog() failed.",  version_id);
	    printf("%s %s: AsfAppInitialize() failed.\n",  ts, version_id);
	    exit (-1);
	}
	for (k = 1; k < argc && strcmp(argv[k], "-config"); ++k);
	if (k == argc) {
	    printfLLog(LOG_ERR, "%s: -config parameter missing", version_id);
	    exit(-1);
	}
	if (! InitODLtemplate(argv[++k])) {
	    printfLLog(LOG_ERR, "%s: InitODLtemplate failed.", version_id);
	    exit (-1);
	}
	printfLLog(LOG_INFO, "%s - Started %s...", version_id, ts);
	printfLLog(LOG_INFO, "%s", copyright);
	off_line = 1;

	while (i < argc && *argv[i] != '-') {
	    if (! (odl = ODLparse(argv[i], 0, err))) {
		printfLLog(LOG_ERR, "%s: %s, %s", version_id, argv[i], err);
		exit (-1);
	    }
	    job_handler(odl, rds);
	    ODLFree(odl);
	    i++;
	}
    }
    RDS_free(rds);
}
