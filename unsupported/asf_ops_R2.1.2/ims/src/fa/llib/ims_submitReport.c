static char *sccs = "@(#)ims_submitReport.c	5.2  04/16/97";
/******************************************************************************
**
** File:	ims_submitReport.c
**
** Function: Perform processing of FA Reports for the various required 
**           flight agency reports.
**
** Author: Dan Crichton	
**
** Date:	6/14/95
**
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_query.h> 
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <signal.h>
#include <syslog.h>

#include <ims_shm.h>
#include <ims_job_control.h>
#include <ims_fa_reports.h>

/*
** Local Functions
*/

int ims_end_job(int i_sig, int i_code, char *data);

/*
** Global Variables
*/



/******************************************************************************
**
** ims_submitReport 
**
******************************************************************************/
int ims_submitReport(
	IMS_MSG_STRUCT *msgDesc, 
	IMS_JOB_USER_SPEC *userSpec,
	int report_id,
	int timeout,
	void (*child_callback) (),
	int shmid,
	char *data)
	

{
	int validReport;      
	int job_id;
	char report_str[10];
	char job_str[10];
	char *argv[10];
	int i, pid;
	char *execDir;
	char fullpathName[IMS_PATH_LEN+1];
	char msg[255]; /* Added R2.1 */


	/*
	** Validate report id
	*/

	if ((report_id > 0) && (report_id <= IMS_MAX_REPORTS))
		validReport = 1;
	else
	{
			ims_msg(msgDesc, IMS_ERROR, "Report Id %d is not valid.", report_id);
			return(IMS_ERROR);
	}


	/*
	** Add the job to the report queue.
	*/

	if (userSpec->username == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "User name must be present");
		return(IMS_ERROR);
	}
	if (userSpec->password == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Password must be present");
		return(IMS_ERROR);
	}

	if (ims_addJob (msgDesc, userSpec, report_id, -1, &job_id, 
				shmid, timeout,  child_callback, data) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not add report to job queue.");
		return(IMS_ERROR);
	}

	/*
	** Get the executible path and kick of the job
	*/

	execDir = getenv("IMS_EXEC_PATH");
	if (execDir == NULL)
	{
		execDir = ".";
	}

	ims_concatFilePath(fullpathName, execDir, "ims_FA_task");


	if (ims_startJob(msgDesc, job_id, report_id, 
					shmid, fullpathName) < IMS_OK)
		return(-1);
	else
	{
		sprintf(msg, "ims_FA_task Job %d starting."); /* Added R2.1 */
		ims_msg(msgDesc, IMS_INFO, msg); /* Added R2.1 */
		return(job_id);
  }
}


