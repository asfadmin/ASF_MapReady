static char *sccs = "@(#)ims_jobControl.c	5.2  03/13/97";
/******************************************************************************
**
** File:	ims_jobControl.c
**
** Function:  Contains functions for controling and statusing multiple tasks
**            for the IMS/DADS backend processing functions.
**
** Author: Dan Crichton	
**
** Date:	6/16/95
**
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

/*
** These are needed by ipc.h
*/
 
typedef unsigned long   ulong;
typedef unsigned short   ushort;

#include <sys/utsname.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>


#include <ims_dbms.h>
#include <ims_query.h> 
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <signal.h>
#include <syslog.h>

#include <ims_shm.h>
#include <ims_job_control.h>

/*
** Local Functions
*/

void ims_end_job(int i_sig, int i_code, char *data);
static int execCmd (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static void perform_callback(void (*) (), int, int, int, int, char *); 

/*
** Global Variables
*/

/******************************************************************************
**
** ims_getJobNumber 
**
******************************************************************************/
int ims_getJobNumber (
	IMS_MSG_STRUCT *msgDesc, 
	IMS_JOB_USER_SPEC *userSpec)
{
	IMS_QI_DESC_OBJ *qDesc;
	int job_id;
	int status;

				 
	/*
	**  Get row from database.
	*/
	
    /*
	** Allocate a query descriptor
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		sigrelse(SIGCHLD);
		(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate a query descriptor.");
		return(IMS_FATAL);
	}
	
    qDesc->cmd = (char *) malloc (IMS_COL512_LEN);
	if ((char *) qDesc->cmd == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate the command area for the query descriptor.");

		free(qDesc->cmd);

		(void) ims_qiFreeDesc(qDesc);
		sigrelse(SIGCHLD);
		return(IMS_FATAL);
	}

	/*
	** Setup login parameters.
	*/

	IMS_SETUSER (qDesc, userSpec->username);
	IMS_SETPSWD (qDesc, userSpec->password);

	if ((userSpec->server != NULL) &&
		(userSpec->server[0] != '\0'))
	{
		IMS_SETSERVER(qDesc, userSpec->server);
	}

	if ((userSpec->database != NULL) &&
		(userSpec->database[0] != '\0'))
	{
		IMS_SETDBNAME(qDesc, userSpec->database);
	}


	status = ims_qiLogin(qDesc);

	if (status < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not login to database server.");
		free(qDesc->cmd);
		(void) ims_qiFreeDesc(qDesc);
		sigrelse(SIGCHLD);
		return(status);
	}

	IMS_SET_USERDATA(qDesc);

	sprintf(qDesc->cmd, "set rowCount 1 \n select job_id from job_control\
			order by job_id desc");

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{

		if (status < IMS_OK)
		{
			free(qDesc->cmd);
			ims_qiFreeDesc (qDesc);
			sigrelse(SIGCHLD);
			return(ims_msgGetSeverity(msgDesc));
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		(void) memcpy((char *) &job_id, qDesc->valAddr[0], 
				qDesc->valLength[0]);
		job_id ++; /* Increment to next job */

	}
	
	/* 
	** Verify that there are jobs in the table.  If not, then 
	** set this job to an initial value to be the first job in 
	** the table.
	*/

	if (IMS_AFFECTED(qDesc) < 1)
	{
		job_id = 101;
	}
	free(qDesc->cmd);
	(void) ims_qiFreeDesc(qDesc);
	
	return(job_id);
}


/******************************************************************************
**
** ims_addJob 
**
******************************************************************************/
int ims_addJob (
	IMS_MSG_STRUCT *msgDesc,
	IMS_JOB_USER_SPEC *userSpec,
	int report_id, 
	int media_id,
	int *job_id, 
	int shmid, /* Passed structure */
	int timeout,
	void (*callback) (),
	char *data)
{
	IMS_JOB_QUEUE_HDR *q_hdr;
	int i;
	int status;
	IMS_QI_DESC_OBJ *qDesc;
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	int start;
	time_t clock;

	/*
	** Get node information
	*/

	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';

	/*
	** Get start time... 
	*/

	start = time(&clock);



	/*
	** Lock report queue table.
	*/

	q_hdr = (IMS_JOB_QUEUE_HDR *) ims_shm_lock(IMS_JOB_SHMID);

	if (q_hdr == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not attach to Job Queue.\n");	
		return(IMS_ERROR);
	}

	/*
	** Hold signals for critical database section
	*/
	sighold(SIGCHLD);

	/*
	**  Add row to database.
	*/

    /*
	** Allocate a query descriptor
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate a query descriptor.");
		(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
		return(IMS_FATAL);
	}
	
    qDesc->cmd = (char *) malloc (IMS_COL512_LEN);
	if ((char *) qDesc->cmd == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate the command area for the query descriptor.");
		free(qDesc->cmd);
		(void) ims_qiFreeDesc(qDesc);
		(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
		sigrelse(SIGCHLD);
		return(IMS_FATAL);
	}
	
	IMS_SETUSER (qDesc, userSpec->username);
	IMS_SETPSWD (qDesc, userSpec->password);

	if ((userSpec->server != NULL) &&
		(userSpec->server[0] != '\0'))
	{
		IMS_SETSERVER(qDesc, userSpec->server);
	}

	if ((userSpec->database != NULL) &&
		(userSpec->database[0] != '\0'))
	{
		IMS_SETDBNAME(qDesc, userSpec->database);
	}


	status = ims_qiLogin(qDesc);

	if (status < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not login to database server.");
		free(qDesc->cmd);
		(void) ims_qiFreeDesc(qDesc);
		(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
		sigrelse(SIGCHLD);
		return(status);
	}

	IMS_SET_USERDATA(qDesc);

	/*
	** Get a unique job identifier.
	*/

	if ((*job_id = ims_getJobNumber(msgDesc, userSpec)) < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not assign job id.");
		free(qDesc->cmd);
		(void) ims_qiFreeDesc(qDesc);
		(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
		sigrelse(SIGCHLD);
		return(IMS_ERROR);
	}

	/*
	** Update the table
	*/

	sprintf(qDesc->cmd, "insert into job_control (\
		node, job_id, report_id, media_id, timeout, start,\
		pid, status) values ('%s', %d, %d, %d, %d, \
		%d, %d, %d)",  hostName, *job_id, report_id, media_id,
		timeout, start, -1, IMS_JOB_STARTING);

	if (execCmd(msgDesc, qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not add job to database.");
		free(qDesc->cmd);
		(void) ims_qiFreeDesc(qDesc);
		(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
		sigrelse(SIGCHLD);
		return(IMS_ERROR);
		
	}

	/*
	** Logoff database.
	*/
	free(qDesc->cmd);
	(void) ims_qiFreeDesc(qDesc);


	/*
	** Find a slot for the job in the shared memory segment.
	*/

	for (i = 0; i < IMS_JOB_QUEUE_MAX; i++)
	{

		if (!q_hdr[i].active)
		{
			/*
			** Active queue slot.
			*/
			q_hdr[i].active = TRUE;
			q_hdr[i].job_id = *job_id;
			q_hdr[i].callback = (void *) callback;
			q_hdr[i].pid = -1; /* Job needs to report it */
			q_hdr[i].data = data;
			q_hdr[i].status = IMS_JOB_STARTING;
			q_hdr[i].msgDesc = msgDesc;
			q_hdr[i].parent_pid = getpid();
			q_hdr[i].shmid = shmid; 
			q_hdr[i].report_id = report_id;
			q_hdr[i].perform_cb = IMS_FALSE;
			memcpy(&(q_hdr[i].userSpec), userSpec, sizeof(IMS_JOB_USER_SPEC));
			break;
		}
	}

	/*
	** If we could not insert the job, then error and exit.
	*/

	if (i == IMS_JOB_QUEUE_MAX)
	{
		(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
		ims_msg(msgDesc, IMS_ERROR, "Could not update Job Queue.");	
		sigrelse(SIGCHLD);
		return(IMS_ERROR);
	}
	(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
	sigrelse(SIGCHLD);
	return(IMS_OK);
}

/******************************************************************************
**
** ims_updateJobStatus 
**
** Tasks should report in once spawned.
** 
******************************************************************************/
int ims_updateJobStatus (
	IMS_MSG_STRUCT *msgDesc, 
	int job_id,
	int job_status,
	int pid) 
{
	IMS_JOB_QUEUE_HDR *q_hdr;
	int i;
	int status;
	IMS_QI_DESC_OBJ *qDesc;


	/*
	** Block signals for critical section.
	*/

	sighold(SIGCHLD);

	/*
	** Lock onto job queue.
	*/

	q_hdr = (IMS_JOB_QUEUE_HDR *) ims_shm_lock(IMS_JOB_SHMID);

	if (q_hdr == NULL)
	{
	   ims_msg(msgDesc, IMS_ERROR, "Could not attach to Job Queue.\n");	
	   sigrelse(SIGCHLD);
	   return(IMS_ERROR);
	}

	/*
	**  Update report queue table.
	*/

	for (i = 0; i < IMS_JOB_QUEUE_MAX; i++)
	{

		if (q_hdr[i].job_id == job_id)
		{
			/*
			** Active queue slot.
			*/

			q_hdr[i].pid = pid;
			q_hdr[i].status = job_status;
			break;
		}
	}


	if (i == IMS_JOB_QUEUE_MAX)
	{
		(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
		ims_msg(msgDesc, IMS_ERROR, "Job not found in internal job queue.");
	    sigrelse(SIGCHLD);
		return(IMS_ERROR);
	}


	/*
	**  Update row in database.
	*/
	
    /*
	** Allocate a query descriptor
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
		(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate a query descriptor.");
		(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
	    sigrelse(SIGCHLD);
		return(IMS_FATAL);
	}
	
  	qDesc->cmd = (char *) malloc (IMS_COL512_LEN);
	if ((char *) qDesc->cmd == NULL)
	{
		(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate the command area for the query descriptor.");

		(void) ims_qiFreeDesc(qDesc);
		(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
	    sigrelse(SIGCHLD);
		return(IMS_FATAL);
	}

	IMS_SETUSER (qDesc, q_hdr[i].userSpec.username);
	IMS_SETPSWD (qDesc, q_hdr[i].userSpec.password);

	if ((q_hdr[i].userSpec.server != NULL) &&
		(q_hdr[i].userSpec.server[0] != '\0'))
	{
		IMS_SETSERVER(qDesc, q_hdr[i].userSpec.server);
	}

	if ((q_hdr[i].userSpec.database != NULL) &&
		(q_hdr[i].userSpec.database[0] != '\0'))
	{
		IMS_SETDBNAME(qDesc, q_hdr[i].userSpec.database);
	}


	(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
	/*
	** Logon to database and initiate update.
	*/

	status = ims_qiLogin(qDesc);

	if (status < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not login to database server.");
		free(qDesc->cmd);
		(void) ims_qiFreeDesc(qDesc);
		(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
	    sigrelse(SIGCHLD);
		return(status);
	}

	IMS_SET_USERDATA(qDesc);

	sprintf(qDesc->cmd, "update job_control set \
		pid = %d, status = %d where job_id = %d",
		pid, job_status, job_id);


	if ((status = execCmd(msgDesc, qDesc)) < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, 
					"Could not update job status in database for job_id %d.",
					job_id);
		free(qDesc->cmd);
		(void) ims_qiFreeDesc(qDesc);
		(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
	    sigrelse(SIGCHLD);
		return(IMS_ERROR);
		
	}
	free(qDesc->cmd);
	(void) ims_qiFreeDesc(qDesc);

	sigrelse(SIGCHLD);
	return(IMS_OK);
}

/******************************************************************************
**
** ims_init_job_queue 
**
******************************************************************************/
int ims_init_job_queue(IMS_MSG_STRUCT *msgDesc)
{
	int key;
	char *ptr;

    key = ims_shm_create(IMS_JOB_SHMID, 
				IMS_JOB_QUEUE_MAX * sizeof(IMS_JOB_QUEUE_HDR));

	if (key < 0)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not setup Job Queue.");
		return(IMS_ERROR);
	}

    if ((ptr = (char *) ims_shm_lock(IMS_JOB_SHMID)) == (void *) -1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not lock Job Queue.");
		return(IMS_ERROR);
	}
								 
	memset(ptr, 0, sizeof(IMS_JOB_QUEUE_HDR) * IMS_JOB_QUEUE_MAX);
									  
	(void) ims_shm_unlock(IMS_JOB_SHMID, ptr);

	return(IMS_OK);

}


/******************************************************************************
**
** ims_remove_job 
**
******************************************************************************/

int ims_remove_job(int job_id)
{
	IMS_JOB_QUEUE_HDR *q_hdr;
	int i;

	/*
	** Attach and update queue to process end of job.
	*/ 

	q_hdr = (IMS_JOB_QUEUE_HDR *) ims_shm_lock(IMS_JOB_SHMID);

	if (q_hdr == NULL)
	{
   		return(IMS_ERROR);
	}
	
	/*
	** Find job, and update its status
	*/

	for (i = 0; i < IMS_JOB_QUEUE_MAX; i++)
	{
		if (q_hdr[i].job_id == job_id)
		{
			/*
			** Found job, now update status and exit.
			*/
			memset(&q_hdr[i], 0, sizeof(IMS_JOB_QUEUE_HDR));
			
		}
	}
	(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
	
}

/******************************************************************************
**
** ims_sendJobMsgs 
**
** Send the messages to the parent.
** 
******************************************************************************/
int ims_sendJobMsgs (
	IMS_MSG_STRUCT *msgDesc, 
	int job_id)
{
	int status;
	int msgid;
	IMS_SHM_QUEUE msgq;
	IMS_MSG_QUEUE *msgQueue;
	int dumpmsg = 0;

	/*
	** Put this in Job Complete....
	*/
		 
	msgid = msgget(job_id, 0777);
			  
	if (msgid > -1)
	{
		while ((msgQueue = ims_msgQueueExtract(msgDesc)) != NULL)
		{
			msgq.mtype = 1;
			msgq.severity = msgQueue->severity;

			strncpy(msgq.mtext, msgQueue->rawMsg, sizeof(msgq.mtext) - 1);

			msgsnd(msgid, &msgq, sizeof(msgq) - sizeof(long), IPC_NOWAIT);
			ims_msgQueueFree(msgQueue);
		}
	}
	else
		dumpmsg  = 1;

	if (dumpmsg)
	{
		while ((msgQueue = ims_msgQueueExtract(msgDesc)) != NULL)
		{
			fprintf(stdout, msgQueue->msg); 
			ims_msgQueueFree(msgQueue);
		}

	}


	return(IMS_OK);
}

/******************************************************************************
**
** ims_jobComplete 
**
** Job tasks should call this before exiting.
** 
******************************************************************************/
int ims_jobComplete (
	IMS_MSG_STRUCT *msgDesc, 
	int job_id)
{
	IMS_JOB_QUEUE_HDR *q_hdr;
	int i, pid, parent_pid;
	int status;
	int msgid;
	IMS_SHM_QUEUE msgq;
	IMS_MSG_QUEUE *msgQueue;
	int dumpmsg = 0;
			 


	q_hdr = (IMS_JOB_QUEUE_HDR *) ims_shm_lock(IMS_JOB_SHMID);
	if (q_hdr == NULL)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not lock memory");
   		return(IMS_ERROR);
	}


	for (i = 0; i < IMS_JOB_QUEUE_MAX; i++)
	{
		if (q_hdr[i].job_id == job_id)
		{
			/*
			** Found job, now get the pid and parent_pid
			*/

			parent_pid = q_hdr[i].parent_pid;
			pid = q_hdr[i].pid;
			q_hdr[i].perform_cb = IMS_TRUE;
			break;

		}
	}

	(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);


	ims_updateJobStatus(msgDesc, job_id, IMS_JOB_COMPLETE, pid);

	/*
	** If parent no longer exists, then remove the job since 
	** no callback is necessary.
	*/


	if (getpgid(parent_pid) < 1)
	{
		/*
		** Dump messages here 
		*/

		ims_remove_job(job_id);
		dumpmsg  = 1;
	}
	else
	{
		/*
		** Send messages to parent.
		*/
		(void) ims_sendJobMsgs(msgDesc, job_id);

	}
	if (dumpmsg)
	{
		while ((msgQueue = ims_msgQueueExtract(msgDesc)) != NULL)
		{
			fprintf(stdout, msgQueue->msg); 
			ims_msgQueueFree(msgQueue);
		}

	}

	return(IMS_OK);	

}



/*****************************************************************************
** perform_callback
**
******************************************************************************/
static void perform_callback(func, arg1, arg2, arg3, arg4, data)
void (*func) ();
int arg1;
int arg2;
int arg3;
int arg4;
char *data;
{

	(*func) (arg1, arg2, arg3, arg4, data);
}


/******************************************************************************
**
** ims_end_job 
**
** Essentially the signal handler which will status the process and perform
** the callback.
******************************************************************************/

void ims_end_job(
	int i_sig, 
	int i_code, 
	char *data)
{
	int pid, temp_pid;
	int exit_status;
	int i;
	IMS_JOB_QUEUE_HDR *q_hdr;
	IMS_MSG_STRUCT *msgDesc;
	int job_id;
	int job_status;
	int shmid;
	void *callback;
	char *userdata;
	int report_id;
	int msgid;
	IMS_SHM_QUEUE msgq;


	job_id = -1;
	if (i_sig != SIGCHLD)
	{
		return;
	}



	if ((pid = wait(&exit_status)) > 0)
	{

		/*
		** Block other signals while executing.
		*/
		sighold(SIGCHLD);

		/*
		** Process all pending jobs, and perform callbacks 
		*/


		/*
		** Attach and update queue to process end of job.
		*/ 

		q_hdr = (IMS_JOB_QUEUE_HDR *) ims_shm_lock(IMS_JOB_SHMID);

		if (q_hdr == NULL)
		{
			sigrelse(SIGCHLD);
	   		return;
		}

		/*
		** Locate job using pid.
		*/
		for (i = 0; i < IMS_JOB_QUEUE_MAX; i++)
		{
			/*
			** If job waiting for callback, then perform.
			*/

			if ((q_hdr[i].perform_cb == IMS_TRUE) || (pid == q_hdr[i].pid))
			{
				job_id = q_hdr[i].job_id;

				/* 
				** Update status...
				*/

				if (q_hdr[i].status < IMS_JOB_COMPLETE)
				{
					/*
					** Process terminated without cleaning up.
					** This is regarded as an aborted state.
					*/
					q_hdr[i].status = IMS_JOB_ABORTED;
				}
				
				/*
				** Extract all the messages from the 
				** message queue and place in the IMS Message
				** message queue.
				*/

				msgDesc = q_hdr[i].msgDesc;
				msgid = msgget(q_hdr[i].job_id, 0777);

				if (msgid > -1)
				{
					while (msgrcv(msgid, &msgq, 
							sizeof(IMS_SHM_QUEUE) - sizeof(long), -1, 
							IPC_NOWAIT | MSG_NOERROR) > -1)
					{
						(void) ims_msg(msgDesc, msgq.severity, msgq.mtext);	
					}
				}

				/*
				** Dump the message queue.
				*/

				if (msgid > -1)
				{
					msgctl(msgid, IPC_RMID, 0);
				}


				/*
				** Now execute callback
				*/

				shmid = q_hdr[i].shmid;
				callback = q_hdr[i].callback; 
				job_status = q_hdr[i].status;
				userdata = q_hdr[i].data;
				report_id = q_hdr[i].report_id;
				q_hdr[i].perform_cb = IMS_FALSE;
				temp_pid = q_hdr[i].pid;


				(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);

				perform_callback ((void (*) ()) callback, 
								job_id, report_id,
								job_status, 
								shmid,
								(void *) userdata);


				/*
				** Force updates to all status tables ....
				*/


				(void) ims_updateJobStatus (msgDesc, job_id, 
							job_status, temp_pid);

				ims_remove_job(job_id);

				/*
				** Re-lock job table
				*/
				q_hdr = (IMS_JOB_QUEUE_HDR *) ims_shm_lock(IMS_JOB_SHMID);

				if (q_hdr == NULL)
				{
					sigrelse(SIGCHLD);
	   				return;
				}
				memset(&q_hdr[i], 0, sizeof(q_hdr[i]));
			}
		}
		sigrelse(SIGCHLD);
		(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) q_hdr);
		return;
	}

	/*
	** Could not get exit status or pid for signal... 
	** We should not even be here if that is the case, but this
	** will guarantee that the error is propagated back in any event.
	*/ 

	return;
	
}



/******************************************************************************
**
** ims_startJob
**
******************************************************************************/
int ims_startJob(
	IMS_MSG_STRUCT *msgDesc, 
	int job_id,
	int report_id,
	int shmid, /* Passed structure */
	char *taskName)
	

{
	char report_str[10];
	char job_str[10];
	char shm_str[10];
	char *argv[10];
	int i, pid;

	/*
	** Setup signal handler for death of a child...
	*/


	if ((void (*) ()) sigset(SIGCHLD, (void (*) ()) ims_end_job) ==  SIG_ERR)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not setup signal handler.");
		return(IMS_ERROR);
	}



	/*
	** Fork and execute the report processor.
	*/

	for (i =0 ; i < 10; i++) 
	{
		argv[i] = 0;
	}
	(void) msgget(job_id, 0777 | IPC_CREAT);

	
	argv[0] = (char *) taskName;
	sprintf(report_str, "%d", report_id);
	argv[1]  = (char *) report_str;
	sprintf(job_str, "%d", job_id);
	argv[2] = (char *) job_str;
	sprintf(shm_str, "%d", shmid);
	argv[3] = (char *) shm_str;


	pid = fork();

	if (pid < 0)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not fork report process.\n");
		perror("Fork error");
		return(IMS_ERROR);	
	}
	else if (pid == 0)
	{
		/*
		** Child process
		*/
		execv(taskName, argv);
		fprintf(stderr, "Could not execute process %s\n", taskName);
		exit(1);
	}

	/*
	** Parent process, pass back job id. 
	*/

	return(IMS_OK);
}



/******************************************************************************
**
** execCmd ()
**
******************************************************************************/


static int execCmd (
IMS_MSG_STRUCT *msgDesc, 
IMS_QI_DESC_OBJ *qDesc)
{
	int status;
	int severity;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
		if (status < IMS_OK)
		{
			return (ims_msgGetSeverity (msgDesc));
		}
	}

	return(IMS_OK);

#if 0  /* Currently, this seems to present a problem. */
	/*
	** Check the stored procedure status returned value.
	*/
	if ((severity = ims_msgGetSeverity (msgDesc)) < IMS_OK)
	{
		return (severity);
	}
	 
	if (qDesc->msgNo != 0)
 	{
		return (ims_msgGetSeverity (msgDesc));
 	}
		  
	return (IMS_OK);
#endif
}
							  

