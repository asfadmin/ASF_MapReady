/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <syslog.h>

#include "PPSdefs.h"
#include "PPSerr.h"
#include "defs.h"
#include "pps_db.h"
#include "send_status.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"

#define		PPS_CONFIG_FILE_SUBPATH	"pps/config/pps.config"

static char SccsFileId[] = "@(#)pps_report_to_ims.c	1.4  02/19/97";

/* global program name for reporting to syslog */
char ProgName[MAX_FILENAME_LEN];

/* database connections */
CS_CONNECTION	*queryConnection=0;
CS_CONNECTION	*updateConnection=0;
IMS_CMN_QUERY	*imsQuery=0;

/* structure for sybase communication */
struct pps_db_exec_dcl	ppsQuery;
struct pps_db_exec_dcl	ppsUpdate;


PPSJobStatus		jobStatus;

void
deleteJob(void)
{
	char 	sqlBuf[MAXLINE];
	char 	logmsg[MAX_SYSLOG_MSGLEN];

	(void)sprintf(logmsg, "Found %s job[%d] state[%s]",
				jobStatus.jobType, jobStatus.jobId, jobStatus.status);
	pps_logMsg(ProgName, PPS_INFO, logmsg);

	/*-------------------------------------------------------*/
	/* if failed to report the order status to IMS,          */
	/* don't continue, just return. job remains in db.       */
	/*-------------------------------------------------------*/
	if (sendFinalOrderStatusToIMS(updateConnection, imsQuery,
		jobStatus.jobId, jobStatus.jobType, jobStatus.status) != ER_NO_ERROR)
	{
		return;
	}

	if (strcmp(jobStatus.jobType, SCAN_KEYWD) == 0)
	{
		(void)sprintf(sqlBuf, "sp_remove_scan %d", jobStatus.jobId);
		if (db_exec(&updateConnection, sqlBuf, &ppsUpdate) == ER_NO_ERROR)
		{
			(void)sprintf(logmsg, "SCAN job[%d] state[%s] removed",
				jobStatus.jobId, jobStatus.status);
			pps_logMsg(ProgName, PPS_INFO, logmsg);
		}
		else
		{
			(void)sprintf(logmsg, "Failed to remove SCAN job[%d] state[%s]",
				jobStatus.jobId, jobStatus.status);
			pps_logMsg(ProgName, PPS_ERROR, logmsg);
		}
	}
	else /* L1 */
	{
		(void)sprintf(sqlBuf, "sp_remove_l1_complete %d", jobStatus.jobId);
		if (db_exec(&updateConnection, sqlBuf, &ppsUpdate) == ER_NO_ERROR)
		{
			(void)sprintf(logmsg, "L1 job[%d] state[%s] removed\n",
				jobStatus.jobId, jobStatus.status);
			pps_logMsg(ProgName, PPS_INFO, logmsg);
		}
		else
		{
			(void)sprintf(logmsg, "Failed to remove L1 job[%d] state[%s]",
				jobStatus.jobId, jobStatus.status);
			pps_logMsg(ProgName, PPS_ERROR, logmsg);
		}
	}

} /* deleteJob */


main(
int		argc,
char 	*argv[])
{
	const char*		optionList="c:";
	char 			logmsg[MAX_SYSLOG_MSGLEN];
	char 			ppsConfigFilePath[MAXLINE];
	char 			sqlBuf[MAXLINE];
	char			*varLocal;
	int				option;
	char			*tempPtr=0;
	int				i=0;

	ppsConfigFilePath[0] = '\0';
	while ((option = getopt(argc, argv, optionList)) != EOF)
	{
		switch(option)
		{
			case 'c':
				(void) strncpy(ppsConfigFilePath, optarg, MAXLINE - 1);
				ppsConfigFilePath[MAXLINE - 1] = '\0';
				break;
			case '?':
			default:
				(void) fprintf(stderr, "Usage: %s [-c config_file]\n",
											argv[0]);
				exit(1);
				break;
		} /* switch */
	} /* while */

	(void)strcpy(ProgName, argv[0]);

	/* Open syslog */
	openlog("PPS:", LOG_PID | LOG_CONS | LOG_NDELAY, SYSLOG_PPS) ;
	pps_logMsg(ProgName, PPS_INFO, "Started");

	for (i = strlen(argv[0]) - 1, tempPtr = argv[0] + strlen(argv[0]);
			i >= 0; i--, tempPtr--)
	{
		if (*tempPtr == '/')
		{
			(void)strcpy(ProgName, tempPtr+1);
			break;
		}
	}

	/*---------------------------------------------------------*/
	/* if the configuration file is not specified in option,   */
	/* get the ENV VAR "LOCAL" and construct the path          */
	/* of the configuration file.                              */
	/*---------------------------------------------------------*/
	if (ppsConfigFilePath[0] == '\0')
	{
		if ((varLocal = (char *)getenv(PPS_ENV_LOCAL)) == NULL)
		{
			(void)sprintf(logmsg,
					"Environment variable %s not set, exiting...\n",
					PPS_ENV_LOCAL);
			pps_logMsg(ProgName, PPS_ERROR, logmsg);
			fprintf(stderr, "%s: %s\n", argv[0], logmsg);
			exit(2);
		}
		(void)sprintf(ppsConfigFilePath, "%s/%s", varLocal,
									PPS_CONFIG_FILE_SUBPATH);
	}

	/*---------------------------------------------------------*/
	/* Read configuration file (PRIOR to db connect)           */
	/* because we need DB info is in the config file           */
	/*---------------------------------------------------------*/
	if (read_config_file(ppsConfigFilePath) == ER_NO_ERROR)
	{
		(void)sprintf(logmsg, "Using configuration file %s", ppsConfigFilePath);
		pps_logMsg(ProgName, PPS_INFO, logmsg);
		fprintf(stdout, "%s\n", logmsg);
	}
	else
	{
		(void)sprintf(logmsg, "Error reading configuration file %s",
									ppsConfigFilePath);
		pps_logMsg(ProgName, PPS_ERROR, logmsg);
		fprintf(stderr, "%s: %s\n", argv[0], logmsg);
		exit(3);
	}

	/*---------------------------------------------------------*/
	/* make a connection to Sybase for Query to PPS database   */
	/*---------------------------------------------------------*/
	if ((db_connect(&queryConnection)) != CS_SUCCEED)
	{
		(void)sprintf(logmsg, "Unable to make Query Connection to Sybase");
		pps_logMsg(ProgName, PPS_ERROR, logmsg);
		fprintf(stderr, "%s: %s\n", argv[0], logmsg);
		exit(4);
	}

	/*---------------------------------------------------------*/
	/* make a connection to Sybase for Update to PPS database  */
	/*---------------------------------------------------------*/
	if ((db_connect(&updateConnection)) != CS_SUCCEED)
	{
		(void)sprintf(logmsg, "Unable to make Update Connection to Sybase");
		pps_logMsg(ProgName, PPS_ERROR, logmsg);
		fprintf(stderr, "%s: %s\n", argv[0], logmsg);
		exit(5);
	}

	/*---------------------------------------------------------*/
	/* make a connection to IMS                                */
	/*---------------------------------------------------------*/
	if ((ims_db_connect(&imsQuery)) != ER_NO_ERROR)
	{
		(void)sprintf(logmsg, "Unable to connect to IMS");
		pps_logMsg(ProgName, PPS_ERROR, logmsg);
		fprintf(stderr, "%s: %s\n", argv[0], logmsg);
		exit(6);
	}

	/*---------------------------------------------------------*/
	/* initialize the params for update db connection          */
	/*---------------------------------------------------------*/
	ppsUpdate.num_items = 0;
	ppsUpdate.callback = 0;

	/*---------------------------------------------------------*/
	/* get all completed and cancelled jobs                    */
	/* and let callback handle the updates.                    */
	/* bind job id, job status and job type.                   */
	/*---------------------------------------------------------*/
	(void)sprintf(sqlBuf,
					"select job_id, job_state, job_type from jobs "
					"where job_state='COMPLETED' or job_state='CANCEL/FAIL'");
	ppsQuery.num_items = 0;
	ppsQuery.callback = deleteJob;
	pps_db_bind_int(&ppsQuery, &jobStatus.jobId);
	pps_db_bind_char(&ppsQuery, jobStatus.status, MAXLINE);
	pps_db_bind_char(&ppsQuery, jobStatus.jobType, MAXLINE);

	(void) db_exec(&queryConnection, sqlBuf, &ppsQuery);

	(void)sprintf(logmsg, "Done.  Exiting...");
	pps_logMsg(ProgName, PPS_INFO, logmsg);
	fprintf(stdout, "%s: %s\n", argv[0], logmsg);

	return(0);

} /* main */
