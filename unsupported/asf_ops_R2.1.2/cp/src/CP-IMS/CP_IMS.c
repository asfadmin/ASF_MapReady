/*----------------------------------------------------------------------------
 | File:        ims.c
 |
 | Function:    CP-IMS interface program for the FTS client event interface.
 |
 | Author:      J. Ho      09/29/95	Original
 |		T. Truong  05/07/96	Rewrote thread interaction
 | Notes:
 |              This program is the main part of interface routine. 
 |              It sets up the communication with CP first. Then wait for
 |              the catalog request from CP to IMS/DADS.
 |
 | 12/03/96 JH  change STATUS from 0/-1 to COMPLETED/CANCEL
 | 1/16/97  JH  add and verify the libcp.a for get_dataset()
 | 1/19/97  JH  change WAIT_CP_ACK timer to 30 seconds, timestamp to all msgs
 | 2/18/97  JH  remove .M after retrieve scan results file
 | 2/27/97  JH  wait 2 secs before WriteMsgToSrv, change Makefile for new libs
 | 
 ----------------------------------------------------------------------------*/
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <syslog.h>
#include <sys/stat.h>
#include <sys/timers.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include "pthread_wrapper.h"
#include "logUtils.h"
#include "asf_syslog.h"
#include "asf.h"
#include "ims.h"

static char sccsid_CP_IMS_c[] = "@(#)CP_IMS.c	1.29 97/08/02 08:54:54";

#define CP_IMS_ERROR_LEN	511
#define CP_ACK_TIMER_LIMIT      5  /*** 5 ***/
typedef struct Job_t {
    IMS_CMN_QUERY	query;
    char		accountId[IMS_NAME_LEN+1];
    char		reason[CP_IMS_ERROR_LEN+1];
    ODL			msg;
    pthread_t		msg_thread;
    pthread_t		ims_thread;
    pthread_mutex_t	mutex;
    pthread_cond_t	cond;
    int			quit;	/* Asynchronous exit notification */
    int			job_id;
    char		status[15];
    int			ack_ed;	/* True if an ACK was received from CP */
    int			retry;	/* > 0 => No. of times SUBSYSTEM_STATUS sent */
				/*   0 => IMS processing just completed */
    int			CP_ACK_RETRY;
    int			CP_ACK_TIMER;
    int			DEFAULT_IMS_TIMER;
    int			STORE_METADATA_TIMER;
    int			STORE_PRODUCTS_RATE;
} Job_t;
    
static Job_t* current_job = NULL;
static pid_t  pid;

/*---------------------------------------------------------------------------*
 | NAME:
 |  ims_heartbeat
 |
 | DESCRIPTION:
 |
 *---------------------------------------------------------------------------*/
#ifdef	IMS_HEARTBEAT
static
void ims_heartbeat(ODL msg, Job_t* job)
{
    register int retry_cnt = job->retry_cnt;
    char resp[CP_IMS_ERROR_LEN+1];
    ODL ack;
#ifdef __sgi
    prctl(PR_TERMCHILD);
#endif
    if (current_job == NULL)
	strcpy(resp, "Waiting for a processing request");

    else if (retry_cnt < 0)
	sprintf(resp, "Waiting for IMS to complete job %d", job->job_id);

    else if (retry_cnt == 0)
	sprintf(resp, "IMS completed job %d - notifying CP", job->job_id);
    else
	sprintf(resp, "Completion sent - awaiting acknowledgement (#%d)",
	    retry_cnt);

    if (! (ack = GetAckMsg(msg, "SUBSYSTEM_STATUS"))) {
        printfLLog(LOG_ERR,
	"can't respond to HEARTBEAT because can't retrieve SUBSYSTEM_STATUS");
        return;
    }
    strcpy(job->status, "COMPLETED");
    ODLSetVal(ack, "SUBSYSTEM_STATUS.BODY.STATUS", job->status);
    ODLSetVal(ack, "SUBSYSTEM_STATUS.BODY.COMMENT", resp);
    ODLSetVal(ack, "SUBSYSTEM_STATUS.BODY.SUB_TYPE", "HEARTBEAT");

    WriteMsgToServer(ack);
    ODLFree(ack);
}
#endif /*! IMS_HEARTBEAT */

/*---------------------------------------------------------------------------*
 | NAME:
 |  ims_ack
 |
 | DESCRIPTION:
 |
 *---------------------------------------------------------------------------*/
static
void ims_ack(ODL msg, Job_t* job)
{
#ifdef __sgi
    prctl(PR_TERMCHILD);
#endif
    if (current_job == NULL) {
        printfLLog(LOG_DEBUG, "got a spurious SUBSYSTEM_ACK - ignored!");
	return;
    }
    job->ack_ed = 1;
    pthread_cond_signal(&job->cond);
}

/*---------------------------------------------------------------------------*
 | NAME:
 |	set_timeout
 |
 | DESCRIPTION:
 |
 *---------------------------------------------------------------------------*/
static
struct timespec* set_timeout(struct timespec* timeout, int duration)
{
    gettimeofday((struct timeval*) timeout, 0);
    timeout->tv_sec += duration;
    timeout->tv_nsec *= 1000;
    return timeout;
}

/*---------------------------------------------------------------------------*
 | NAME:
 |
 | DESCRIPTION:
 |  call ims_req_archive to interface with IMS/DADS only.
 |  Also, set up control flags for main thread to go on.
 |
 *---------------------------------------------------------------------------*/
static
int SendStatusToServer (char* ack_type, Job_t* job)
{
    char s[80];
    ODL  ack;
    time_t imsTime;

    if (! (ack = GetAckMsg(job->msg, ack_type))) {
        printfLLog(LOG_ERR, "can't retrieve %s to send back to CP for job %d",
	    ack_type, job->job_id);
        return -1;
    }
    ODLSetVal(ack, (sprintf(s,"%s.BODY.STATUS", ack_type), s), job->status);
    ODLSetVal(ack, (sprintf(s,"%s.BODY.COMMENT",ack_type), s), job->reason);

    if (strcmp(ack_type, "SUBSYSTEM_ACK") == 0) {
	WriteMsgToServer(ack);
	ODLFree(ack);
	return 0;
    }
    ODLSetVal(ack, (sprintf(s, "%s.BODY.SUB_TYPE", ack_type), s), "CATALOG");
    ODLSetVal(ack, (sprintf(s, "%s.BODY.FILE_VERSION", ack_type), s),
	job->query.retStatus);

    for (job->retry = 1; job->retry <= job->CP_ACK_RETRY; ++job->retry) {
	register int status;
	struct timespec timeout;

        pthread_mutex_lock(&job->mutex);
        if (!job->quit && !job->ack_ed)
            if ((status = pthread_cond_timedwait( &job->cond, &job->mutex,
                        set_timeout(&timeout, 2))) == -1)
               status = errno;
        if (job->quit || job->ack_ed) {
            pthread_mutex_unlock(&job->mutex);
            ODLFree(ack);
            return 0;
        }
        pthread_mutex_unlock(&job->mutex);

        time(&imsTime);
        ODLSetVal(ack, (sprintf(s, "%s.COMMON_HEADER.TIME", ack_type), s), 
                  &imsTime); 

	WriteMsgToServer(ack);

        pthread_mutex_lock(&job->mutex);
        if (!job->quit && !job->ack_ed)
            if ((status = pthread_cond_timedwait( &job->cond, &job->mutex,
                        set_timeout(&timeout, job->CP_ACK_TIMER-2))) == -1)
               status = errno;
        if (job->quit || job->ack_ed) {
            pthread_mutex_unlock(&job->mutex);
            ODLFree(ack);
            return 0;
        }
        pthread_mutex_unlock(&job->mutex);
        if (status != EAGAIN)
            printfLLog(LOG_ERR, "timedwait failed on retry #%d for job %d, %s",
                job->retry, job->job_id, strerror(status));
    }
    ODLFree(ack);
    return -1;
}

/*----------------------------------------------------------------------------
**  NAME:
**      ims_timeout
**
**  DESCRIPTION:
**
**---------------------------------------------------------------------------*/
static
int ims_timeout(ODL msg, register Job_t* job)
{
    static char
	MSG_TYPE[]		= "CATALOG_REQUEST.COMMON_HEADER.MSG_TYPE",
	IMAGE_FILE[]		= "CATALOG_REQUEST.BODY.IMAGE_FILE",
	STORE_SCAN_METADATA[]	= "STORE_SCAN_METADATA",
	STORE_SAR_PRODUCTS[]	= "STORE_SAR_PRODUCTS";

    register char *msg_type, *image_file;
    struct stat fs;
    return
	(!(msg_type = ODLGetStr(msg, MSG_TYPE)) ? job->DEFAULT_IMS_TIMER :
	(!strcasecmp(msg_type, STORE_SCAN_METADATA) ? job->STORE_METADATA_TIMER:
	(!strcasecmp(msg_type, STORE_SAR_PRODUCTS) &&
	 (image_file = ODLGetStr(msg, IMAGE_FILE)) && !stat(image_file, &fs) ?
		((fs.st_size / job->STORE_PRODUCTS_RATE) + 1) :
	job->DEFAULT_IMS_TIMER)));
}

/*---------------------------------------------------------------------------*
 | NAME:
 |  ims_main
 |
 | DESCRIPTION:
 |  call ims_req_archive to interface with IMS/DADS only.
 |  Also, set up control flags for main thread to go on.
 |
 *---------------------------------------------------------------------------*/
static
void ims_main(Job_t* job)
{
    register int status;
#ifdef __sgi
    prctl(PR_TERMCHILD);
#endif
    pid = getpid();
    status = ims_process(job->msg, &job->query, job->reason);

    pthread_mutex_lock(&job->mutex);
    strcpy(job->status, (status ? "CANCEL/FAIL" : "COMPLETED"));
    job->retry = 0;
    pthread_cond_signal(&job->cond);
    pthread_mutex_unlock(&job->mutex);
}

/*---------------------------------------------------------------------------*
 | NAME:
 |  asf_ims
 |
 | DESCRIPTION:
 |  The msg ODL is pointing to the CP-IMS CATALOG_REQUEST.
 |  This a callback routine which executes once a CATALOG_REQUEST received.
 |
 *---------------------------------------------------------------------------*/
static
void ims_request(ODL msg, register Job_t* job)
{
    static char JOB_ID[] = "CATALOG_REQUEST.BODY.JOB_ID";
    register char *s;
    int ierr;
    char killCmd[20];

#ifdef __sgi
    prctl(PR_TERMCHILD);
#endif
    if (current_job != NULL) {
	/*  Disallow multiple CATALOG_REQUEST */
	printfLLog(LOG_ERR, "Job %d still in progress - request ignored.",
	           job->job_id);
	return;
    }
    job->msg_thread = pthread_self();
    job->msg = msg;
    job->quit = 0;
    job->ack_ed = 0;
    job->reason[0] = 0;
    job->query.retPtr = job->accountId;
    job->query.retStatus = -1;
    job->retry = -1;

    if (job->job_id = ODLGetInt(msg, JOB_ID, &ierr), ierr == -1) {
	printfLLog(LOG_ERR, "can't retrieve JOB_ID - request ignored.");
        return;
    }	
    current_job = job;

    printfLLog(LOG_INFO, "Starting job %d...", job->job_id);

    strcpy(job->status, "COMPLETED");
    SendStatusToServer("SUBSYSTEM_ACK", job);
    strcpy(job->status, "CANCEL/FAIL"); 

    pthread_mutex_lock(&job->mutex);

    if (pthread_create(&job->ims_thread, pthread_attr_default,
           (pthread_startroutine_t) ims_main,
	   (pthread_addr_t) job) == -1)

	sprintf(job->reason, "can't start IMS thread for job %d: %s",
	        job->job_id, strerror(errno));
    else {
	struct timespec timeout;
	pthread_addr_t status;
	int duration;

printfLLog(LOG_INFO, "BEFORE pthread_cond_timedwait job %d...", job->job_id);
	if (pthread_cond_timedwait( &job->cond, &job->mutex,
	    set_timeout(&timeout, duration = ims_timeout(msg, job))) == -1) {
printfLLog(LOG_INFO, "AFTER(error) pthread_cond_timedwait job %d", job->job_id);

	    if (errno == EAGAIN)
		sprintf(job->reason, "IMS failed to complete job %d in %d s.",
		        job->job_id, duration);
	    else
		sprintf(job->reason, "can't start IMS timer - abort job %d: %s",
			job->job_id, strerror(errno));
            strcpy(job->status, "CANCEL/FAIL");
	}
        else {
printfLLog(LOG_INFO, "AFTER(no err) pthread_cond_timedwait job %d", job->job_id);
        }
/***
	pthread_cancel(job->ims_thread);
	pthread_join(job->ims_thread, &status);
        pthread_kill(job->ims_thread);
***/
        /* kill ims_thread here */
printfLLog(LOG_INFO, "BEFORE pthread_kill job %d...", job->job_id);
        pthread_kill(job->ims_thread);
        if (getpgid(pid) != -1) { /* is ims_thread gone ? */
            sprintf(killCmd, "kill -9 %d", (int) pid);
            system(killCmd);
            printfLLog(LOG_INFO, killCmd);
        }
        else {
            printfLLog(LOG_INFO, "pthread_kill succeeded");
        }
printfLLog(LOG_INFO, "AFTER pthread_kill job %d...", job->job_id);
    }
    pthread_mutex_unlock(&job->mutex);

    printfLLog(LOG_INFO, "Job %d done.", job->job_id);
printfLLog(LOG_INFO, "BEFORE SendStatusToServer job %d...", job->job_id);
    SendStatusToServer("SUBSYSTEM_STATUS", job);
printfLLog(LOG_INFO, "AFTER SendStatusToServer job %d...", job->job_id);

    current_job = NULL;
}

/*----------------------------------------------------------------------------*
 | NAME:
 |  get_config_data
 |
 | DESCRIPTION:
 |  This routine gets the content of an image request in ODL format from CP.
 |
 *----------------------------------------------------------------------------*/
static
int get_config_data(char* programName, char* configfile, register Job_t *job)
{
    static char
        STORE_METADATA_TIMER[]	= "IMS_CONFIG.BODY.STORE_METADATA_TIMER",
        STORE_PRODUCTS_RATE[]	= "IMS_CONFIG.BODY.STORE_PRODUCTS_RATE",
        DEFAULT_IMS_TIMER[]	= "IMS_CONFIG.BODY.DEFAULT_IMS_TIMER",
        CP_ACK_TIMER[]		= "IMS_CONFIG.BODY.CP_ACK_TIMER",
        CP_ACK_RETRY[]		= "IMS_CONFIG.BODY.CP_ACK_RETRY",
        USERNAME[]		= "IMS_CONFIG.BODY.USERNAME",
        PASSWORD[]		= "IMS_CONFIG.BODY.PASSWORD",
        ACCOUNTID[]		= "IMS_CONFIG.BODY.ACCOUNTID",
        PROGRAM[]		= "CP-IMS",
        SERVER[]		= "",
        DATABASE[]		= "";
    register char *s, *username, *password, *accountId;
    int err;
    ODL odl;
    if (! (odl = ODLparse(configfile, 0, job->reason)))
        return -1;

    if (! (username = ODLGetStr(odl, s = USERNAME)) ||
	! (password = ODLGetStr(odl, s = PASSWORD)) ||
	! (accountId = ODLGetStr(odl, s = ACCOUNTID)) ||
 	(job->CP_ACK_RETRY = ODLGetInt(odl, s = CP_ACK_RETRY,
	   &err), err == -1 || job->CP_ACK_RETRY < 1) ||
 	(job->CP_ACK_TIMER = ODLGetInt(odl, s = CP_ACK_TIMER,
	   &err), err == -1 || job->CP_ACK_TIMER < 1) ||
 	(job->DEFAULT_IMS_TIMER = ODLGetInt(odl, s = DEFAULT_IMS_TIMER,
	   &err), err == -1 || job->DEFAULT_IMS_TIMER < 1) ||
 	(job->STORE_METADATA_TIMER = ODLGetInt(odl, s = STORE_METADATA_TIMER,
	   &err), err == -1 || job->STORE_METADATA_TIMER < 1) ||
 	(job->STORE_PRODUCTS_RATE = ODLGetInt(odl, s = STORE_PRODUCTS_RATE,
	   &err), err == -1 || job->STORE_PRODUCTS_RATE < 1)) {
	sprintf(job->reason, "can't extract %s from configuration file %s",
	    s, configfile);
        ODLFree(odl);
        return -1;
    }
    if (job->CP_ACK_TIMER < CP_ACK_TIMER_LIMIT) {
        sprintf(job->reason, 
            "CP_ACK_TIMER no less than 5 seconds in configuration file %s",
            configfile);
        ODLFree(odl);
        return -1;
    }
    if (strlen((s = USERNAME, username)) > IMS_NAME_LEN ||
	strlen((s = PASSWORD, password)) > IMS_NAME_LEN ||
	strlen((s = ACCOUNTID,accountId))> IMS_NAME_LEN) {
	sprintf(job->reason, "name %s too long (max %d characters)",
	    s, IMS_NAME_LEN);
        ODLFree(odl);
        return -1;
    }
    strcpy(job->query.username, username);
    strcpy(job->query.password, password);
    strcpy(job->query.program, PROGRAM);
    strcpy(job->query.server, SERVER);
    strcpy(job->query.database, DATABASE);
    strcpy(job->accountId, accountId);

    ODLFree(odl);
    return 0;
}

/*---------------------------------------------------------------------------*
 | NAME:
 |  ims_reset
 |
 | DESCRIPTION:
 |
 *---------------------------------------------------------------------------*/
static
void ims_reset(ODL msg, Job_t* job)
{
#ifdef __sgi
    prctl(PR_TERMCHILD);
#endif
    if (current_job != NULL) {
	pthread_addr_t status;

	printfLLog(LOG_INFO, "Cancelling job %d... ", job->job_id);
	job->quit = 1;
	pthread_cond_broadcast(&job->cond);
	pthread_join(job->msg_thread, &status);
	printfLLog(LOG_INFO, "Job %d cancelled.", job->job_id);
    }
}
/*--------------------------------------------------------------------------*
 | NAME:
 |  main
 |
 | DESCRIPTION:
 |  This program is the main part of CP - IMS interface routine.
 |  It sets up the communication with CP first, and establish
 |  the callback events to handle CATALOG REQUEST from CP to IMS.
 |
 *-------------------------------------------------------------------------*/

int main(int argc, char *argv[])
{
#include "version.h"

    register char *configFile = NULL, *programName = NULL;
    register int i;
    char timestamp[28];
    time_t clock;
    Job_t  job;
    AsfApp app;

    if (! (app = AsfAppInitialize(programName, &argc, argv))) {
	register char* id = strrchr(argv[0], '/');

	openlog(id ? (id+1) : argv[0], LOG_PID|LOG_CONS|LOG_NDELAY|LOG_PERROR,
		LOG_SPS);
        syslog( LOG_ERR, "can't initialize application.\n");
	closelog();
        exit(-1);
    }
    /*
     | Parse the command line to extract two application-dependent parameters:
     |     -asfn	Name by which subsystem is known to CP.
     |     -configfile	Configuration file contains info for this application.
     */
    for (i = 1; argv[i] != NULL; i++) {
        if (strcmp(argv[i], "-asfn") == 0) {
            if (argv[++i] == NULL || argv[i][0] == '-') {
                printfLLog(LOG_ERR, "Missing argument on -asfn flag\n");
	        exit(-1);
            }
            programName = argv[i];
        } 
        else if (strcmp(argv[i], "-configfile") == 0) {
            if (argv[++i] == NULL || argv[i][0] == '-') {
                printfLLog(LOG_ERR, "Missing argument on -configfile flag\n");
                exit(-1);
            }
            configFile = argv[i];
        }
    }
    if (configFile == NULL) {
	printfLLog(LOG_ERR, "-configfile argument required\n");
	exit(-1);
    }
    if (programName == NULL) {
	printfLLog(LOG_ERR, "-asfn argument required\n");
	exit(-1);
    }
#ifdef __sgi
    if (usconfig(CONF_INITUSERS, 32) == -1)
	printfLLog(LOG_ERR, "CONF_INITUSERS(32) failed, %s\n", strerror(errno));
#endif
    if (pthread_cond_init(&job.cond, pthread_condattr_default) == -1) {
	printfLLog(LOG_ERR, "can't initialize mutex CV, %s\n", strerror(errno));
	exit(-1);
    }
    if (pthread_mutex_init(&job.mutex, pthread_mutexattr_default) == -1) {
	printfLLog(LOG_ERR, "can't initialize mutex, %s\n", strerror(errno));
	exit(-1);
    }
    if (get_config_data(programName, configFile, &job) == -1) {
	printfLLog(LOG_ERR, "%s", job.reason);
	exit(-1);
    }
    /* ctime_r((time(&clock), &clock), timestamp, sizeof(timestamp)); */
                      clock = time ((time_t *) 0);
                        (void) ctime_r (&clock, timestamp );


    timestamp[strlen(timestamp)-1] = 0;

    printfLLog(LOG_INFO, "%s - Started %s...\n", version, timestamp);

    AsfAddCallback(app,
	"SUBSYSTEM_HALT", ims_reset, &job,
	"SUBSYSTEM_ACK", ims_ack, &job,
    	"CATALOG_REQUEST", ims_request, &job,
#ifdef	IMS_HEARTBEAT
    	"SUBSYSTEM_HEARTBEAT", ims_heartbeat, &job,
#endif
	NULL);
    AsfAppMainLoop(app);
}
