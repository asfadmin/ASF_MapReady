/*=============================================================================
**  @(#)ims.c	1.23 96/07/20 21:37:55
**
**  CP-IMS Interface Module.
**  Copyright (C) Jet Propulsion Laboratory
**  Alaska SAR Facility (ASF) Project.
**
**============================================================================*/
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <libgen.h>
#include "framegen.h"
#include "asf_syslog.h"
#include "logUtils.h"
#include "Time.h"
#include "ims.h"
#include "ims_archive.h"
#include "ims_dbms.h"
#include "ims_const.h"
#include "ims_keyword.h"
#include "ims_msg.h"
#include "ims_ask.h"

static char sccsid_ims_c[] = "@(#)ims.c	1.36 97/08/02 08:53:39";

#define	PRODUCT_BASENAME_LEN	20	/* Max length as limited by IMS */
#define	SCAN_FILENAME_LEN	17
#define	IMAGE_FILENAME_LEN	16
#define	CALPR_FILENAME_LEN	18

static char
    SOURCE[]			= "CATALOG_REQUEST.COMMON_HEADER.SOURCE",
    DESTINATION[]		= "CATALOG_REQUEST.COMMON_HEADER.DESTINATION",
    MSG_TYPE[]			= "CATALOG_REQUEST.COMMON_HEADER.MSG_TYPE",
    SCAN_RESULTS_FILE[]		= "CATALOG_REQUEST.BODY.SCAN_RESULTS_FILE",
    CALPARMS_FILE_PRIMARY[]	= "CATALOG_REQUEST.BODY.CALPARMS_FILE_PRIMARY",
    CALPARMS_FILE_SECONDARY[]	= "CATALOG_REQUEST.BODY.CALPARMS_FILE_SECONDARY",
    PMF_FILE[]			= "CATALOG_REQUEST.BODY.PMF_FILE",
    IMAGE_FILE[]		= "CATALOG_REQUEST.BODY.IMAGE_FILE",
    COMPENSATION_FLAG[]		= "CATALOG_REQUEST.BODY.COMPENSATION_FLAG",
    FRAME_MODE_P[]		= "CATALOG_REQUEST.BODY.FRAME_MODE",
    CEOS_LEADER_FILE[]		= "CATALOG_REQUEST.BODY.CEOS_LEADER_FILE",
    PRODUCT_ID[]		= "CATALOG_REQUEST.BODY.PRODUCT_ID",
    PRODUCT_TYPE[]		= "CATALOG_REQUEST.BODY.PRODUCT_TYPE",
    PRODUCT_BASE_NAME[]		= "CATALOG_REQUEST.BODY.PRODUCT_BASE_NAME",
    QC_STATUS[]			= "CATALOG_REQUEST.BODY.QC_STATUS",

    SCAN_RESULTS_BODY[]		= "SCAN_RESULTS_FILE.BODY",
    MODE[]			= "SEGMENT.MODE",
    JOB_ID[]			= "JOB_ID",
    REVOLUTION[]		= "REVOLUTION",
    SEQUENCE[]			= "SEQUENCE",
    PLATFORM[]			= "PLATFORM",
    SENSOR[]			= "SENSOR",
    SITE_NAME[]			= "SITE_NAME",
    ACTIVITY_ID[]		= "ACTIVITY_ID",
    STATION_ID[]		= "STATION_ID",
    MEDIA_ID[]			= "MEDIA_ID",
    FRAME_MODE[]		= "FRAME_MODE",
    START_TIME[]		= "START_TIME",
    END_TIME[]			= "END_TIME",

    STORE_SCAN_RESULTS[]	= "STORE_SCAN_RESULTS",
    STORE_SCAN_METADATA[]	= "STORE_SCAN_METADATA",
    STORE_SAR_PRODUCTS[]	= "STORE_SAR_PRODUCTS",
    RETRIEVE_FILE_VERSION[]	= "RETRIEVE_FILE_VERSION",
    RETRIEVE_SCAN_RESULTS[]	= "RETRIEVE_SCAN_RESULTS",
    RETRIEVE_CAL_PARAMS[]	= "RETRIEVE_CAL_PARAMS";
    
/*--------------------------------------------------------------------------
**  NAME:
**	ValidMode
**
**  DESCRIPTION:
**	Returns TRUE if given mode is valid.
**
**-------------------------------------------------------------------------*/
static
int ValidMode(char* mode)
{
    static char* modeImage[] = {
    "ST1","ST2","ST3","ST4","ST5","ST6","ST7",
    "SWA","SWB","SNA","SNB",
/*
    "WD1","WD2","WD3",
    "FN1","FN2","FN3","FN4","FN5","EL1",
*/
    "EH1","EH2","EH3","EH4","EH5","EH6",
    "STD"
    };
    register int i;
    for (i = 0; i < sizeof(modeImage)/sizeof(modeImage[0]); ++i)
	if (!strcmp(mode, modeImage[i]))
	    return 1;
    return 0;
}

/*--------------------------------------------------------------------------
**  NAME:
**	log_framegen
**
**  DESCRIPTION:
**
**-------------------------------------------------------------------------*/
static
void log_framegen(FRMGEN* fp)
{
    register int i;
    printfLLog(LOG_DEBUG, "  mode = \"%s\"\n", fp->mode);
    printfLLog(LOG_DEBUG, "  platform = \"%s\"\n", fp->platform);
    printfLLog(LOG_DEBUG, "  frame_mode = \"%s\"\n", fp->frame_mode);
    printfLLog(LOG_DEBUG, "  revolution = %d\n", fp->revolution);
    printfLLog(LOG_DEBUG, "  sequence = %d\n", fp->sequence);
    printfLLog(LOG_DEBUG, "  sensor = '%c'\n", fp->sensor);
    printfLLog(LOG_DEBUG, "  dar_id = %d\n", fp->dar_id);
    printfLLog(LOG_DEBUG, "  media_id = \"%s\"\n", fp->media_id);
    printfLLog(LOG_DEBUG, "  station_id = \"%s\"\n", fp->station_id);
    printfLLog(LOG_DEBUG, "  activity_id = \"%s\"\n", fp->activity_id);
    printfLLog(LOG_DEBUG, "  site_name = \"%s\"\n", fp->site_name);
    printfLLog(LOG_DEBUG, "  datatake_status = \"%s\"\n", fp->datatake_status);
    printfLLog(LOG_DEBUG, "  scan_results_file = \"%s\"\n",
	fp->scan_results_file);
    printfLLog(LOG_DEBUG, "  start_time = %s\n", fp->start_time);
    printfLLog(LOG_DEBUG, "  end_time = %s\n", fp->end_time);
    printfLLog(LOG_DEBUG, "  time_pairs = %d\n", fp->time_pairs_count);

    for (i = 0; i < fp->time_pairs_count; ++i)
	printfLLog(LOG_DEBUG, "  [%d] Start %s End %s\n", i,
	fp->time_pairs[i].start_time, fp->time_pairs[i].end_time);
}

/*--------------------------------------------------------------------------
**  NAME:
**	framegen
**
**  DESCRIPTION:
**
**-------------------------------------------------------------------------*/
static
int framegen(char* scan_results_file, 
    char* qc_status, IMS_MSG_STRUCT* msgDesc, char *error_buf)
{

    register int i,n, status, job_id, revolution, sequence;
    register char *s, *platform, *sensor, *activity_id, *media_id;
    register char *frame_mode, *site_name, *station_id;
    register GMT_t *t0, *tN;
    int ierr;
    ODL odl, scan_results_odl, *myODL;
    FRMGEN frameGen;
    char parse_error[256], mode[4];
    TimePair tm[MAX_TIMEPAIR];

    s = (s = strchr(scan_results_file,':')) ? s+1 : scan_results_file;
    if (! (scan_results_odl = ODLparse(s, 0, parse_error))) {
        sprintf(error_buf, "can't parse %s: %s", s, parse_error);
        return -1;
    }
    memcpy(mode, basename(s)+9, 3);
    mode[3] = 0;
    if (! ValidMode(mode)) {
	sprintf(error_buf, "invalid mode in %s", scan_results_file);
	return -1;
    }
    if (! (odl = Value(Lookup(scan_results_odl, s = SCAN_RESULTS_BODY))) ||
        ! (platform = ODLGetStr(odl, s = PLATFORM)) ||
        ! (sensor = ODLGetStr(odl, s = SENSOR)) ||
        ! (activity_id = ODLGetStr(odl, s = ACTIVITY_ID)) ||
        ! (media_id = ODLGetStr(odl, s = MEDIA_ID)) ||
        ! (site_name = ODLGetStr(odl, s = SITE_NAME)) ||
        ! (station_id = ODLGetStr(odl, s = STATION_ID)) ||
        ! (frame_mode = ODLGetStr(odl, s = FRAME_MODE)) ||
	! (t0 = Val(Lookup(odl, s = START_TIME))) ||
	! (tN = Val(Lookup(odl, s = END_TIME))) ||
        (job_id = ODLGetInt(odl, s = JOB_ID, &ierr), ierr == -1) ||
        (revolution = ODLGetInt(odl, s = REVOLUTION, &ierr), ierr == -1) ||
        (sequence = ODLGetInt(odl, s = SEQUENCE, &ierr), ierr == -1)) {

        sprintf(error_buf, "can't get %s from %s", s, scan_results_file);
        return -1;
    }
    sprintf(frameGen.start_time, "%.4d-%.3dT%.2d:%.2d:%.2d.%.3d",
	    t0->tm.tm_year+1900, t0->tm.tm_yday+1, t0->tm.tm_hour,
	    t0->tm.tm_min, t0->tm.tm_sec, t0->tv_usec/1000);
    sprintf(frameGen.end_time, "%.4d-%.3dT%.2d:%.2d:%.2d.%.3d",
	    tN->tm.tm_year+1900, tN->tm.tm_yday+1, tN->tm.tm_hour,
	    tN->tm.tm_min, tN->tm.tm_sec, tN->tv_usec/1000);
    n = 0;
    myODL = Val(odl);
    for (i = 0; myODL[i] != NULL; ++i) {
	if (strcasecmp(Name(myODL[i]), "SEGMENT") ||
	    ! (t0 = (GMT_t*) Val(Lookup(myODL[i], START_TIME))) ||
	    ! (tN = (GMT_t*) Val(Lookup(myODL[i], END_TIME))) )
	    continue;

	if (n == MAX_TIMEPAIR) {
	    printfLLog(LOG_ERR, "Too many segments - processed first %d", n);
	    break;
	}
	sprintf(tm[n].start_time, "%.4d-%.3dT%.2d:%.2d:%.2d.%.3d",
	    t0->tm.tm_year+1900, t0->tm.tm_yday+1, t0->tm.tm_hour,
	    t0->tm.tm_min, t0->tm.tm_sec, t0->tv_usec/1000);

	sprintf(tm[n].end_time, "%.4d-%.3dT%.2d:%.2d:%.2d.%.3d",
	    tN->tm.tm_year+1900, tN->tm.tm_yday+1, tN->tm.tm_hour,
	    tN->tm.tm_min, tN->tm.tm_sec, tN->tv_usec/1000);
	n++;
    }
    strcpy(frameGen.platform, platform);
    strcpy(frameGen.mode, mode);
    strcpy(frameGen.activity_id, activity_id);
    strcpy(frameGen.station_id, station_id);

    frameGen.msgDesc = msgDesc;
    frameGen.revolution = revolution;
    frameGen.sequence = sequence;
    frameGen.sensor = *sensor;
    frameGen.dar_id = 1;
    frameGen.frame_mode = frame_mode;
    frameGen.site_name = site_name;
    frameGen.datatake_status = "SCANNED";
    frameGen.media_id = media_id;
    frameGen.scan_results_file = scan_results_file;
    frameGen.time_pairs = tm;
    frameGen.time_pairs_count = strncasecmp(qc_status, "REJECT", 6) ? n : 0;

    printfLLog(LOG_DEBUG, "BEFORE frame_generator: job %d", job_id);
    log_framegen(&frameGen);
    status = frame_generator(&frameGen);
    printfLLog(LOG_DEBUG, " AFTER frame_generator: job %d, status %s", job_id,
	status == FG_OK ? "OK" : "ERROR");

    ODLFree(scan_results_odl);
    if (status != FG_OK) {
	sprintf(error_buf, "Frame Generator failed - check syslog for details");
	return -1;
    }
    return 0;
}

/*--------------------------------------------------------------------------
**  NAME:
**	build_metadata
**
**  DESCRIPTION:
**
**-------------------------------------------------------------------------*/
static
int build_metadata(char* scan_results_file, char *error_buf)
{
    static char
    SOURCE[]		= "SCAN_RESULTS_FILE.COMMON_HEADER.SOURCE",
    MEDIA_TYPE[]	= "MEDIA_TYPE",
    MEDIA_LOC[]		= "MEDIA_LOCATION",
    RECORDER_ID[]	= "RECORDER_ID",
    CREATOR[]		= "PRODUCT_CREATOR",
    SV_PRECISION[]	=
	"STATE_VECTOR_RECORD.STATE_VECTOR_METADATA.STATE_VECTOR_PRECISION";

    static char metadata_format[] = 
"\
OBJECT = SCAN_RESULTS_FILE_METADATA\n\
  OBJECT = COMMON_HEADER\n\
    TIME = %.4d-%.3dT%.2d:%.2d:%.2d\n\
    MSG_TYPE = \"SCAN_RESULTS_FILE_METADATA\"\n\
    DESTINATION = \"IMS\"\n\
    SOURCE = \"CP\"\n\
    NUMBER_OF_RECORDS = 1\n\
  END_OBJECT = COMMON_HEADER\n\
  OBJECT = CATALOG_METADATA\n\
    JOB_ID = %d\n\
    PLATFORM = \"%s\"\n\
    SENSOR = \"%s\"\n\
    REVOLUTION = %d\n\
    SEQUENCE = %d\n\
    ACTIVITY_ID = \"%s\"\n\
    MEDIA_ID = \"%s\"\n\
    MEDIA_TYPE = \"%s\"\n\
    MEDIA_LOCATION = \"%s\"\n\
    RECORDER_ID = \"%s\"\n\
    STATION_ID = \"%s\"\n\
    FRAME_MODE = \"%s\"\n\
    SITE_NAME = \"%s\"\n\
    MODE = \"%s\"\n\
    STATE_VECTOR_PRECISION = \"%s\"\n\
    PRODUCT_CREATOR = \"%s\"\n\
    SCAN_RESULTS_FILE = \"%s\"\n\
  END_OBJECT = CATALOG_METADATA\n\
END_OBJECT = SCAN_RESULTS_FILE_METADATA\n\
END\n\
";
    int ierr;
    time_t t;
    struct tm tm;
    ODL odl, scan_results_odl;
    char metafile[256], metadata[1536], parse_error[256], mode[4];
    register int n, fd, job_id, revolution, sequence;
    register char *s, *platform, *sensor, *activity_id, *media_id, *media_type,
	*media_loc, *recorder_id, *station_id, *frame_mode, *site_name,
	*sv_precision, *creator;

    s = (s = strchr(scan_results_file, ':')) ? s+1 : scan_results_file;
    if (! (scan_results_odl = ODLparse(s, 0, parse_error)) ) {
        sprintf(error_buf,
	    "can't parse scan results file %s: %s", s, parse_error);
        return -1;
    }
    memcpy(mode, basename(s)+9, 3);
    mode[3] = 0;
    if (! ValidMode(mode)) {
	sprintf(error_buf, "invalid mode in %s", scan_results_file);
	return -1;
    }
    if (! (creator = ODLGetStr(scan_results_odl, s = SOURCE)) ||
	! (odl = Value(Lookup(scan_results_odl, s = SCAN_RESULTS_BODY))) ||
	! (platform = ODLGetStr(odl, s = PLATFORM)) ||
	! (sensor = ODLGetStr(odl, s = SENSOR)) ||
	! (activity_id = ODLGetStr(odl, s = ACTIVITY_ID)) ||
	! (media_id = ODLGetStr(odl, s = MEDIA_ID)) ||
	! (media_type = ODLGetStr(odl, s = MEDIA_TYPE)) ||
	! (media_loc = ODLGetStr(odl, s = MEDIA_LOC)) ||
	! (recorder_id = ODLGetStr(odl, s = RECORDER_ID)) ||
	! (station_id = ODLGetStr(odl, s = STATION_ID)) ||
	! (frame_mode = ODLGetStr(odl, s = FRAME_MODE)) ||
	! (site_name = ODLGetStr(odl, s = SITE_NAME)) ||
	! (sv_precision = ODLGetStr(odl, s = SV_PRECISION)) ||
	(job_id = ODLGetInt(odl, s = JOB_ID, &ierr), ierr == -1) ||
	(revolution = ODLGetInt(odl, s = REVOLUTION, &ierr), ierr == -1) ||
	(sequence = ODLGetInt(odl, s = SEQUENCE, &ierr), ierr == -1) ) {

	sprintf(error_buf, "can't extract %s from %s", s, scan_results_file);
	return -1;
    }
    localtime_r((time(&t), &t), &tm);
    n = sprintf(metadata, metadata_format,
	tm.tm_year+1900, tm.tm_yday+1, tm.tm_hour, tm.tm_min, tm.tm_sec,
	job_id,
	platform,
	sensor,
	revolution,	
	sequence,
	activity_id,
	media_id,
	media_type,
	media_loc,
	recorder_id,
	station_id,
	frame_mode,
	site_name,
	mode,
	sv_precision,
	creator,
	scan_results_file);
    ODLFree(scan_results_odl);

    s = (s = strchr(scan_results_file, ':')) ? s+1 : scan_results_file;
    memcpy(metafile, s, fd = strlen(s));
    if (metafile[fd-2] == '.')
	metafile[fd-1] = 'M';
    else {
	metafile[fd++] = '.';
	metafile[fd++] = 'M';
    }
    metafile[fd] = 0;

    if ((fd = open(metafile, O_WRONLY|O_CREAT|O_TRUNC, 0644)) == -1) {
	sprintf(error_buf, "can't create file %s, %s", s, strerror(errno));
	return -1;
    }
    if (write(fd, metadata, n) != n) {
	sprintf(error_buf, "can't write metadata to %s, %s",
	    metafile, strerror(errno));
	close(fd);
	return -1;
    }
    close(fd);
    return 0;
}

/*--------------------------------------------------------------------------
**  NAME:
**	get_platform
**
**  DESCRIPTION:
**	get platform from the 1st 2 characters of filename.
**
**-------------------------------------------------------------------------*/
static
const char* get_platform(char* filename)
{
    static const char
	R1[] = "RADARSAT-1",
    	J1[] = "JERS-1",
    	E1[] = "ERS-1",
    	E2[] = "ERS-2",
	*UNKNOWN = NULL;
    register char* s;

    s = basename((s = strchr(filename,':')) ? s+1 : filename);
    return (strncmp(s, "R1", 2) == 0 ? R1 :
	   (strncmp(s, "E1", 2) == 0 ? E1 :
	   (strncmp(s, "E2", 2) == 0 ? E2 :
	   (strncmp(s, "J1", 2) == 0 ? J1 : UNKNOWN))));
}


/*--------------------------------------------------------------------------
**  NAME:
**	create_msgQueue
**
**  DESCRIPTION:
**
**-------------------------------------------------------------------------*/
static
int create_msgQueue(IMS_CMN_QUERY* query, IMS_MSG_STRUCT* *msgDesc)
{
    /* Inititialize the message facility options. */
    if ((*msgDesc = ims_msgStructAlloc()) == NULL)
	return -1;

    (void) ims_msgSubSystem(*msgDesc, query->program);
    (void) ims_msgBanner(*msgDesc, query->program, IMS_MSG_ALLBANNER);
    (void) ims_msgStderrFlag(*msgDesc, IMS_ON);
    (void) ims_msgQueueFlag(*msgDesc, IMS_ON);
    (void) ims_msgSybErrHndlFlag(*msgDesc, IMS_ON);
    (void) ims_msgSybMsgHndlFlag(*msgDesc, IMS_ON);
    return 0;
}

/*--------------------------------------------------------------------------
**  NAME:
**	flush_msgQueue
**
**  DESCRIPTION:
**	Dump the messages from the IMS message queue into
**	the appropriate syslog files.
**
**-------------------------------------------------------------------------*/
static
void flush_msgQueue(IMS_MSG_STRUCT *msgDesc)
{
    IMS_MSG_QUEUE *msgQueue;

    while ((msgQueue = ims_msgQueueExtract(msgDesc)) != NULL) {
	register char* format;
	register int level;
	/*
	 *  Select the syslog level based upon the IMS severity levels.
	 *  Note that IMS_OK and IMS_INFO are written at level DEBUG, not
	 *  INFO; normally, we're not interested in seeing these messages.
	 */
	switch (msgQueue->severity) {
	case IMS_OK:
	    level = LOG_DEBUG;
	    format = "%s";
	    break;
	case IMS_INFO:
	    level = LOG_DEBUG;
	    format = "%s";
	    break;
	case IMS_WARNING:
	    level = LOG_WARNING;
	    format = "WARNING:  %s";
	    break;
	case IMS_ERROR:
	    level = LOG_ERR;
	    format = "ERROR:  %s";
	    break;
	case IMS_FATAL:
	    level = LOG_CRIT;
	    format = "CRITICAL:  %s";
	    break;
	default:
	    /* This should never be reached */
	    level = LOG_ERR;
	    format = "ERROR:  %s";
	}
	/*
	 *  Use the raw message only; the newlines in the "msg" field are
	 *  not handled well by printfLLog().
	 *
	 *  Check that the message length is less than the maximum allowed
	 *  by printfLLog().  If the message is longer, only write the first
	 *  MAX_MESSAGE_LEN-1 characters to the log file.
	 */
	if (msgQueue->rawMsg != NULL) {
	    char msgBuf[MAX_MESSAGE_LEN];

	    if (strlen(msgQueue->rawMsg) < MAX_MESSAGE_LEN)
		(void) printfLLog(level, format, msgQueue->rawMsg);
	    else {
		(void) strncpy(msgBuf, msgQueue->rawMsg, MAX_MESSAGE_LEN-1);
		msgBuf[MAX_MESSAGE_LEN-1] = '\0';
		(void) printfLLog(level, format, msgBuf);
	    }
	}
	(void) ims_msgQueueFree(msgQueue);
    }
}

/*--------------------------------------------------------------------------
**  NAME:
**	destroy_msgQueue
**
**  DESCRIPTION:
**
**-------------------------------------------------------------------------*/
static
void destroy_msgQueue(IMS_MSG_STRUCT* *msgDesc)
{
    flush_msgQueue(*msgDesc);
    (void) ims_msgStructFree(*msgDesc);
    *msgDesc = NULL;
}

/*--------------------------------------------------------------------------
**  NAME:
**	log_ims_request
**
**  DESCRIPTION:
**
**-------------------------------------------------------------------------*/
static
void log_ims_request(IMS_CLNT_EVENT* request)
{
    printfLLog(LOG_DEBUG, "  reqType = %d\n", request->requestType);
    printfLLog(LOG_DEBUG, "  username = \"%s\"\n", request->username);
    printfLLog(LOG_DEBUG, "  password = \"%s\"\n", request->password);
    printfLLog(LOG_DEBUG, "  accountId = \"%s\"\n", request->accountId);
    printfLLog(LOG_DEBUG, "  programName = \"%s\"\n", request->programName);
    printfLLog(LOG_DEBUG, "  server = \"%s\"\n", 
	       request->catSrvName ? request->catSrvName : "(null)");
    printfLLog(LOG_DEBUG, "  database = \"%s\"\n", 
	       request->catDbName ? request->catDbName : "(null)");
    printfLLog(LOG_DEBUG, "  ftsSrvName = \"%s\"\n",
	       request->ftsSrvName ? request->ftsSrvName : "(null)");
    printfLLog(LOG_DEBUG, "  platform = \"%s\"\n", request->platform);
    printfLLog(LOG_DEBUG, "  sensor = \"%s\"\n", request->sensor);
    printfLLog(LOG_DEBUG, "  format = \"%s\"\n", 
	       request->format ? request->format : "(null)");
    printfLLog(LOG_DEBUG, "  dataset = \"%s\"\n", request->dataset);
    printfLLog(LOG_DEBUG, "  name = \"%s\"\n", request->name);
    printfLLog(LOG_DEBUG, "  sourceDir = \"%s\"\n", request->sourceDir);
    printfLLog(LOG_DEBUG, "  version = %d\n", request->version);
    printfLLog(LOG_DEBUG, "  fileCount = %d\n", request->fileCount);
    printfLLog(LOG_DEBUG, "  localArchive= '%c'\n", request->localArchiveFlag);
}

/*----------------------------------------------------------------------------
**  NAME:
**	archive
**
**  DESCRIPTION:
**
**---------------------------------------------------------------------------*/
int ims_process(ODL msg, IMS_CMN_QUERY* query, char *reason)
{
    static char JOB_ID[]	= "CATALOG_REQUEST.BODY.JOB_ID";
    int ierr;
    register int status, job_id;
    register char *s, *msg_type;
    char dir[256], name[SCAN_FILENAME_LEN+2], dataset[256];
    IMS_CLNT_EVENT  request;

    /*-----------------------*
     *  Check input request
     *-----------------------*/
    if (!(msg_type = ODLGetStr(msg, s = MSG_TYPE)) ||
	 (job_id = ODLGetInt(msg, s = JOB_ID, &ierr), ierr == -1)) {

        sprintf(reason, "Can't extract %s from input message", s);
        return -1;
    }
    printfLLog(LOG_DEBUG, "Received %s from CP", msg_type);

    if (create_msgQueue(query, &query->msgDesc) == -1) {
        strcpy(reason, "can't create msgQueue");
	return -1;
    }
    if (strcasecmp(msg_type, RETRIEVE_FILE_VERSION) == 0) {
	register int version;
	register char *product_basename;
	IMS_VERSION_STRUCT retBuf;

        if (! (product_basename = ODLGetStr(msg, s = PRODUCT_BASE_NAME))) {
            sprintf(reason, "Can't extract %s from input message", s);
	    destroy_msgQueue(&query->msgDesc);
            return -1;
        }
	query->qDesc = NULL;
        query->retPtr = (char*) &retBuf;

        printfLLog(LOG_DEBUG, "BEFORE openQueryConnection: job %d\n", job_id);
        status = ims_openQueryConnection(query);
        printfLLog(LOG_DEBUG, " AFTER openQueryConnection:\n");
        flush_msgQueue(query->msgDesc);
        if (status != IMS_OK) {
	    sprintf(reason, "Can't open connection to IMS server");
	    destroy_msgQueue(&query->msgDesc);
            return (query->retStatus = -1);
        }
        printfLLog(LOG_DEBUG, "BEFORE ims_incrVersion:\n");
        status = ims_incrVersion(query, product_basename);
        printfLLog(LOG_DEBUG, " AFTER ims_incrVersion:\n");
	version = retBuf.version;
        flush_msgQueue(query->msgDesc);

        printfLLog(LOG_DEBUG, "BEFORE closeQueryConnection:\n");
        ims_closeQueryConnection(query);
        printfLLog(LOG_DEBUG, " AFTER closeQueryConnection: job %d\n", job_id);
	destroy_msgQueue(&query->msgDesc);

	query->retStatus = version;
	if (status != IMS_OK) {
	    sprintf(reason, "Can't get version because ims_incrVersion failed");
	    return -1;
	}
        return 0;
    }
    request.username = query->username;
    request.password = query->password;
    request.accountId = query->retPtr;
    request.programName = query->program;
    request.catSrvName = NULL;	/* query->server */
    request.catDbName = NULL; 	/* query->database */
    request.ftsSrvName = NULL;	/* query->server */
    request.msgDesc = query->msgDesc;
    request.dataset = dataset;
    request.sensor = "";
    request.version = -1;
    request.localArchiveFlag = 'N';

    if (strcasecmp(msg_type, RETRIEVE_SCAN_RESULTS) == 0) {
	register char* scan_file;

        if (! (scan_file = ODLGetStr(msg, s = SCAN_RESULTS_FILE)) ||
            ! (request.name = ODLGetStr(msg, s = PRODUCT_ID)) ||
	    ! (request.platform = (s = PLATFORM, (char*)
		get_platform(scan_file)))) {

            sprintf(reason, "Can't extract %s from input message", s);
	    destroy_msgQueue(&query->msgDesc);
            return -1;
        }
	request.requestType = IMS_GET;
	request.extensions = NULL;
	request.format = "GENERIC";
	request.fileCount = 1;
	sprintf(request.dataset, "%s SCAN RESULTS FILE", request.platform);
        s = strchr(scan_file, ':');
	request.sourceDir = dirname(strcpy(dir, s ? (s+1) : scan_file));
    }
    else if (strcasecmp(msg_type, RETRIEVE_CAL_PARAMS) == 0) {

	register char *cal_file, *product_id;

	if (! (cal_file = ODLGetStr(msg, s = CALPARMS_FILE_PRIMARY))) {
            sprintf(reason, "Can't extract %s from input message", s);
            destroy_msgQueue(&query->msgDesc);
            return -1;
        }
 
	if (! (product_id = ODLGetStr(msg, s = PRODUCT_ID)) ||
	    ! (request.platform = (s = PLATFORM, (char*)
		get_platform(product_id)))) {

            sprintf(reason, "Can't extract %s from input message", s);
	    destroy_msgQueue(&query->msgDesc);
            return -1;
        }
	request.requestType = IMS_GET;
	request.extensions = NULL;	
	request.format = "PMF";
	request.fileCount = 1;

	s = strchr(cal_file, ':');
	request.name = strncpy(name, basename(s ? (s+1) : cal_file),
	     CALPR_FILENAME_LEN);
        name[CALPR_FILENAME_LEN] = 0;
        sprintf(request.dataset, "%s CALIBRATION PARAMETER FILE",
             request.platform);
        request.sourceDir = dirname(strcpy(dir, s ? (s+1) : cal_file));

    }
    else if (strcasecmp(msg_type, STORE_SCAN_RESULTS) == 0) {

	register char *scan_file;
	static char *extensions[]	= {"M", "D"};

        if (! (scan_file = ODLGetStr(msg, s = SCAN_RESULTS_FILE)) ||
	    ! (request.platform = (s = PLATFORM, (char*)
		get_platform(scan_file)))) {

            sprintf(reason, "Can't extract %s from input message", s);
	    destroy_msgQueue(&query->msgDesc);
            return -1;
        }
	if (build_metadata(scan_file, reason) == -1) {
	    destroy_msgQueue(&query->msgDesc);
	    return -1;
	}
	request.requestType = IMS_REPLACE;
	request.extensions = extensions;
	request.format = "GENERIC";
	request.fileCount = 2;
	s = strchr(scan_file, ':');
	request.name = strncpy(name, basename(s ? (s+1) : scan_file),
	     SCAN_FILENAME_LEN);
	name[SCAN_FILENAME_LEN] = 0;
	sprintf(request.dataset, "%s SCAN RESULTS FILE", request.platform);
	request.sourceDir = dirname(strcpy(dir, s ? (s+1) : scan_file));
    }
    else if (strcasecmp(msg_type, STORE_SCAN_METADATA) == 0) {

	register char *qc_status, *scan_file;

        if (! (qc_status = ODLGetStr(msg, s = QC_STATUS)) ||
	    ! (scan_file = ODLGetStr(msg, s = SCAN_RESULTS_FILE)) ||
	    ! (request.platform = (s = PLATFORM, (char*)
		get_platform(scan_file)))) {

            sprintf(reason, "Can't extract %s from input message", s);
	    destroy_msgQueue(&query->msgDesc);
            return -1;
        }
/*	request.requestType = IMS_ADD;
 |	request.extensions = NULL;
 |	request.format = "CEOS";
 |	request.fileCount = 3;
 |	s = strchr(scan_file, ':');
 |	request.name = strncpy(name, basename(s ? (s+1) : scan_file),
 |     	     SCAN_FILENAME_LEN);
 |	name[SCAN_FILENAME_LEN] = 0;
 |	sprintf(request.dataset, "%s CALIBRATION PARAMETER FILE",
 |     		request.platform);
 |	request.sourceDir = dirname(strcpy(dir, s ? (s+1) : scan_file));
*/
        status = framegen(scan_file, qc_status, query->msgDesc, reason);
	destroy_msgQueue(&query->msgDesc);

	return (status);
    }
    else if (strcasecmp(msg_type, STORE_SAR_PRODUCTS) == 0) {

        register char *product_type, *image_file;
        register char *mode;
        register char *compensated;
	register char *product_id;
        register char *frame_mode; 
	static char *extensions[]	= {"M", "D", "L"};
	static char MODE[] 		= "CATALOG_REQUEST.BODY.MODE";

        if (! (product_id = ODLGetStr(msg, s = PRODUCT_ID)) ||
	    ! (product_type = ODLGetStr(msg, s = PRODUCT_TYPE)) ||
	    ! (image_file = ODLGetStr(msg, s = IMAGE_FILE)) ||
	    ! (mode = ODLGetStr(msg, s = MODE)) ||
	    ! (compensated = ODLGetStr(msg, s = COMPENSATION_FLAG)) ||
            ! (frame_mode = ODLGetStr(msg, s = FRAME_MODE_P)) || 
	    ! (request.platform = (s = PLATFORM, (char*)
		get_platform(image_file))) ||
	    ! (request.dataset = (s = "DATASET", (char*)
		get_dataset(product_id, product_type, mode, 
                            frame_mode, "FRAME_REQUEST", compensated)))) {
            sprintf(reason, "Can't extract %s from input message", s);
	    destroy_msgQueue(&query->msgDesc);
            return -1;
        }
	request.requestType = IMS_REPLACE;
	request.extensions = extensions;
	request.format = "CEOS"; 
	request.sensor = "SAR";
	request.version = -1;
	request.fileCount = 3;
	request.localArchiveFlag = 'N';
	s = strchr(image_file, ':');
	request.name = strncpy(name, basename(s ? (s+1) : image_file),
	     IMAGE_FILENAME_LEN);
	name[IMAGE_FILENAME_LEN] = 0;
	request.sourceDir = dirname( strcpy(dir, s ? (s+1) : image_file));
    }
    else {
	sprintf(reason, "Invalid request type");
	destroy_msgQueue(&query->msgDesc);
	return -1;
    }
    printfLLog(LOG_DEBUG, "BEFORE ims_archive: job %d", job_id);
    log_ims_request(&request);
    status = ims_archive(&request);
    printfLLog(LOG_DEBUG, " AFTER ims_archive: job %d", job_id);

    destroy_msgQueue(&query->msgDesc);
    if (status != IMS_OK) {
	sprintf(reason, "IMS_ARCHIVE failed - check syslog for details");
	return -1;
    }
    if (strcasecmp(msg_type, RETRIEVE_CAL_PARAMS) == 0) {
	char old_name[256], new_name[256];

        sprintf(old_name, "%s/%s.M", request.sourceDir, request.name);
	sprintf(new_name, "%s/%s", request.sourceDir, request.name);

	if (rename(old_name, new_name) == -1) {
	    sprintf(reason, "Can't rename %s, %s", old_name, strerror(errno));
	    return -1;
	}
    }
    if (strcasecmp(msg_type, RETRIEVE_SCAN_RESULTS) == 0) {
	char removeFile[256];

        sprintf(removeFile, "/bin/rm -f %s/%s.M", request.sourceDir, request.name);
        system(removeFile);
    }
    return 0;
}
