static char *sccs = "@(#)ims_jobMaster.c	5.2  07/24/96";
/******************************************************************************
**
** File:	ims_jobMaster.c
**
** Function: Perform master job control functions.  
**          
**
** Author: Dan Crichton	
**
** Date:	6/20/95
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
#include <ims_cmd.h>
#include <signal.h>
#include <syslog.h>

#include <ims_shm.h>
#include <ims_job_control.h>


static IMS_JOB_USER_SPEC userSpec;
int cmdDone = 0;
 
/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/

static struct commands
{
	char *username;
	char *password;
	char *server;
	char *database;
	char *help;
} commands;

/*
** "Itemlist" for getting the above commands from the commandline.
*/

static IMS_CMD_CLASS cmdLineElm [] =
{
	{"-U",           &commands.username},
	{"+username",    &commands.username},
	{"-P",           &commands.password},
	{"+password",    &commands.password},
	{"-X",           &commands.server},
	{"+server",      &commands.server},
	{"-Y",           &commands.database},
	{"+database",    &commands.database},
	{"-h",           &commands.help},
	{"+help",        &commands.help},
};

static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

/*
** "Itemlist" for getting the above commands from a command file.
*/
static IMS_CMD_CLASS cmdFileElm [] =
{
	{"username",    &commands.username},
	{"password",    &commands.password},
	{"server",      &commands.server},
	{"database",    &commands.database},
};

static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

static IMS_MSG_STRUCT *msgDesc;

/*
** Local Functions
*/

void monitorJobs(IMS_MSG_STRUCT *, char *);
static int getArgInput (IMS_MSG_STRUCT *);
int shutdown(int i_sig, int i_code, char *data);
static void usage();

static char *glb_programName;

void main(int argc, char *argv[])
{
	struct utsname uname_info;    /* Structure for uname() */
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	int status;

	/*
	** Setup message facility.
	*/

	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';  /* Null-terminate just in case */
	
    /*
	** Initialize the message processor.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr,
				"Memory allocation for IMS_MSG_STRUCT structure failed.");
		exit (IMS_FATAL);
	}

    glb_programName = ims_extractFileName (argv[0]);
		 
	  
	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgProgramName (msgDesc, glb_programName);
	(void) sprintf (banner, "%s::%s", hostName, glb_programName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

	/*
	** Print out copyright info.
	*/

	fprintf(stderr, "\nIMS/DADS Job Control Monitor\n");
	(void) ims_printVersion(stderr);

	/*
	** Get the command line arguments. The variable status will actually
	** contain the number of command line arguments processed upon
	** successful completion.
	*/
	if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
				cmdLineElmCount, msgDesc)) < IMS_OK)
	{
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

 
	 /*
	 ** Check to see if we got everything off of the command line.
	 */
	 if (status < argc)
	 {
		 (void) ims_msg (msgDesc, IMS_WARNING,
				 "Only %d out of the %d command line arguments were processed.",
				 status, argc);
	 }
														  

    /*
	** If help has been specified, print usage and exit.
	*/
	if (commands.help != (char *) NULL)
	{
		usage ();
		(void) ims_msgStructFree (msgDesc);
		exit (0);
	}

	/*
	** Process the information from the command-line and/or command-file.
	*/
	if ((status = getArgInput (msgDesc)) < IMS_OK)
	{
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}
											 

	/*
	** Setup signal handler to shutdown for ctrl-c...
	*/
	if (sigset(SIGINT, shutdown) == -1)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not setup signal handler.");
		exit(1);
	}
								 

	/*
	** Setup shared memory table...
	*/


	if (ims_init_job_queue(msgDesc) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not initialize job queue.\n");
		(void) ims_msgStructFree (msgDesc);
		exit(1);

	}

	/*
	** Status and monitor jobs ...
	*/

#ifdef DEBUG
	fprintf(stderr, "Job Queue Installed.\n\n");
#endif

	for (; ;)
	{
		sleep(5);
		monitorJobs(msgDesc, hostName);
	}


}

/******************************************************************************
**
** monitorJobs
**
******************************************************************************/


void monitorJobs(
	IMS_MSG_STRUCT *msgDesc,
	char *node)
{
	IMS_QI_DESC_OBJ *qDesc;
	int pid, start, timeout, job_id;
	int parent_pid;
	int i;
	time_t clock;
	int status;
	IMS_JOB_QUEUE_HDR *qptr;


    /*
	** Allocate a query descriptor
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate a query descriptor.");
		return;
	}
	
    qDesc->cmd = (char *) malloc (IMS_COL255_LEN);
	if ((char *) qDesc->cmd == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate the command area for the query descriptor.");

		free(qDesc->cmd);
		(void) ims_qiFreeDesc(qDesc);
		return;
	}

	IMS_SETUSER (qDesc, userSpec.username);
	IMS_SETPSWD (qDesc, userSpec.password);

	if (userSpec.program != NULL)
		IMS_SETPROG(qDesc, userSpec.program); 

	if (userSpec.server != NULL)
		IMS_SETSERVER(qDesc, userSpec.server); 

	if (userSpec.database != NULL)
		IMS_SETDBNAME(qDesc, userSpec.database); 

	/*
	** Logon to database and initiate update.
	*/

	status = ims_qiLogin(qDesc);

	if (status < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not login to database server.");
		free(qDesc->cmd);
		(void) ims_qiFreeDesc(qDesc);
		return;
	}

	IMS_SET_USERDATA(qDesc);

	sprintf(qDesc->cmd, "select pid, start, timeout, job_id from job_control\
			where status < %d and node = '%s'", IMS_JOB_COMPLETE, node);


	/*
	** Scan through response list...
	*/

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{

		if (status < IMS_OK)
		{
			free(qDesc->cmd);
			ims_qiFreeDesc (qDesc);
			return;
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** Get pid
		*/

		(void) memcpy((char *) &pid, qDesc->valAddr[0], 
				qDesc->valLength[0]);
		/*
		** Get start
		*/

		(void) memcpy((char *) &start, qDesc->valAddr[1], 
				qDesc->valLength[1]);

		/*
		** Get timeout
		*/

		(void) memcpy((char *) &timeout, qDesc->valAddr[2], 
				qDesc->valLength[2]);
		/*
		** Get job_id
		*/

		(void) memcpy((char *) &job_id, qDesc->valAddr[3], 
				qDesc->valLength[3]);

		/*
		** Job can't be cancelled.
		*/

		if (timeout == -1)
			continue;
		
		
		/*
		** Has this process timed out?
		*/

		if (start + timeout < time(&clock))
		{
			(void) ims_msg(msgDesc, IMS_INFO, "Timeout of job id %d", job_id);

			if (ims_updateJobStatus(msgDesc, job_id, IMS_JOB_TIMEOUT, 
					pid) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not update job %d status to timeout", job_id);
			}

			/*
			** Send a terminate to the process...
			*/

			if ((pid > 0) && (getpgid(pid) > 0))
			{

				if (kill(pid, 9))
				{
					(void) ims_msg(msgDesc, IMS_ERROR, 
						"Could not timeout job_id %d", job_id);
				}
			}

			/*
			** Does parent of pid exist???
			** If not, then remove entry from shared memory since no
			** callback will occur.
			*/

			qptr = (IMS_JOB_QUEUE_HDR *) ims_shm_lock(IMS_JOB_SHMID);

			parent_pid = -1;

			for (i = 0; i < IMS_JOB_QUEUE_MAX;  i++)
			{
				if (qptr[i].pid == pid)
				{
					parent_pid = qptr[i].parent_pid;
					break;
				}
			}

			(void) ims_shm_unlock(IMS_JOB_SHMID, (void *) qptr);

			/*
			** If parent does not exist, then remove job...
			*/

			if (parent_pid > 0)
			{
				if (getpgid(parent_pid) < 1)
					(void) ims_remove_job(job_id);

			}

		}

	}
	
	free(qDesc->cmd);
	ims_qiFreeDesc (qDesc);

}

int shutdown(int i_sig, int i_code, char *data)
{
	fprintf(stderr, "\nShutting down ims_jobMaster monitoring program.\n");
	ims_shm_remove(IMS_JOB_SHMID);
	(void) ims_msgStructFree (msgDesc);
	exit(1);
}




/******************************************************************************
**
** getArgInput ()
**
** Prompt for needed information not provided in the command-line
** and command-file.
**
******************************************************************************/

static int getArgInput (
	IMS_MSG_STRUCT *msgDesc)
{
	char inputBuffer[IMS_INPUT_BUF_LEN+1];
	char prompt[20];

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/

	memset((char *) &userSpec, 0, sizeof(userSpec));

	/* username */
	if (commands.username != (char *) NULL)
	{
		strcpy(userSpec.username, commands.username);
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Username: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		(void) strcpy (userSpec.username, inputBuffer);
	}

	/* password */
	if (commands.password != (char *) NULL)
	{
		strcpy(userSpec.password, commands.password);
	}
	else
	{
		if (ims_getPassword (inputBuffer) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		(void) strcpy (userSpec.password, inputBuffer);
	}

	/* server */
	if (commands.server != (char *) NULL)
	{
		strcpy(userSpec.server, commands.server);
	}


	/* database */
	if (commands.database != (char *) NULL)
	{
		strcpy(userSpec.database, commands.database);
	}

	return (IMS_OK);
}



/*******************************************************************
**
** usage
**
*******************************************************************/
static void usage()
{
	printf("IMS/DADS Job Master\n");
	printf("ims_jobMaster [-U <user>] [-P <pswd>] [-X <dbase>] [-Y <srvr>]");
	printf("\n");
}
