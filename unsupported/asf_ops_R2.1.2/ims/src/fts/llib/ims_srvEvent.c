static char *sccs = "@(#)ims_srvEvent.c	5.9 03/13/97";
/******************************************************************************
**
** File:        ims_srvEvent.c
**
** Function:    File Transfer System event functions for the server side.
**
** Author:      Hoshyar Sayah, Jeff Jacobson, K. Pattaphongse,
**
** Date:        10/90
**
** Modified:    1/5/94 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Added the include files stdlib.h, string.h, errno.h
**              and sys/unistd.h. Removed the include files malloc.h,
**              ctype.h, sys/types.h, strings.h and sys/stat.h.
**              Replaced calls to index() with strchr(). Replaced the use of
**              sys_errlist[] with strerror().
**
**              8/17/94 - H. Sayah, S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              12/12/94 - S. Hardman - R1B
**              Ported to Sybase System 10. See the Open Server
**              Server-Library/C Reference Manual for more information.
**
**              03/01/95 - D. Crichton - R1B
**              Modify calls to access the capabilities for the adgr events.
**           
**              03/02/95 - D. Crichton - R1B
**              Add access to new structure FTS_DATASET_PATH_POLICY.
**
**              04/06/95 - D. Crichton - R1B
**              Add new function to return file types to the client.
**				
**              05/04/95 - D. Crichton - R1B
**              Enhance aux processing to send platform, sensor, dataset
**              to aux process if requested.
**
**              05/15/95 - D. Crichton - R1B
**              Added updateFileSizes() function to update the file sizes
**              of the incoming files during the ingestion process.
**
**				11/10/95 - D. Crichton - R1B'
**				Modify to open a new syslog for each thread when validating
**				the metadata.  Extract the ODL errors and send back to 
**				the client.
**
**				11/15/95 - D. Crichton - R1B'
**				Modify auxillary processing to handle new messages.  Also,
**				modify the invokeAux() function to determine the path from
**				the IMS_EXEC_PATH environment variables.
**
**				12/8/95 - D. Crichton - R1B'
**				Added the name_stamp field which is used for replace. Removed
**				Modified replace event to use the same name in granuleSpec
**				and then just add the name_stamp to the end to distinguish
**				between the old granule item, and then new one replacing it.
**
** Note:        This file includes the routines called by the ims_fts server
**              program to process file operations requested by the client. 
**
******************************************************************************/

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>

/*
** Define timeout structure.
*/

struct timeval {
	long    tv_sec;     /* seconds */
	long    tv_usec;    /* and microseconds */
};


#include <sys/select.h>


#include <ims_dbms.h>
#include <ospublic.h>
#include <oserror.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_hash.h>
#include <ims_archive.h>
#include <ims_ftsSrvCat.h>
#include <ims_ftsSrv.h>
#include <ims_util.h>
#include <ims_keyword.h>
#include <ims_childHandler.h>
#include <odldef.h>
/* #include <ims_krb.h> */

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_ftsSrv.h.
** They are listed here for reference.
**
**	int ims_addEventBegin (SRV_PROC *);
**	int ims_addEventEnd (SRV_PROC *);
**	int ims_deleteEvent (SRV_PROC *);
**	int ims_getEventBegin (SRV_PROC *);
**	int ims_getEventEnd (SRV_PROC *);
**	int ims_replaceEventBegin (SRV_PROC *);
**	int ims_replaceEventEnd (SRV_PROC *);
**	int ims_validateClientLogin (SRV_PROC *);
**	int ims_validateMetadata (SRV_PROC *);
*/

/*
** Local Functions
*/
int invokeAux (FTS_DATASET_POLICY *, int *, IMS_MSG_STRUCT *);
static void decodeAuxProcMessage (char *, FTS_BUFFERSPEC_TYPE *,
	FTS_PROC_DESC *);
static int parseODL (char *, FTS_KEYWORD_LIST **, IMS_MSG_STRUCT *);
static int validateKeywordList (FTS_PROC_DESC *);
static FTS_KEYWORD_LIST  *sortKeywordList (FTS_KEYWORD_LIST *);
static int updateFileSizes (SRV_PROC *, FTS_PROC_DESC *); 

/*
** Error messages
*/
static char *EFTS_CLOSE_CONNECTION =
	"Closing database connection failed. Contact the DBA.";
static char *EFTS_OPEN_CONNECTION =
	"Openning database connection failed. Contact the DBA.";

/******************************************************************************
**
** ims_addEventBegin ()
**
** Begin processing granule transfer request into storage.
**
** The processing status, whether successful or fail, must be sent back 
** to the client.
**
******************************************************************************/

int ims_addEventBegin (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_CAT_USERSPEC *userSpec;
	FTS_CAT_STRUCT *catReq;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_SENSOR_POLICY *sensorPolicy;
	FTS_CLIENT_REQUEST *clientReq;
	FTS_PATH_POLICY *pathPolicy;
	IMS_MSG_STRUCT *msgStruct;
	FTS_CAT_USERCAP *userCap;
	CS_SERVERMSG srvMsg;
	char msg[CS_MAX_MSG];
	int status;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_addEventBegin.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_addEventBegin.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** We have access to the policy information through the
	** procDesc->mission and procDesc->policy pointers.
	*/
	catReq = &(procDesc->catReq);
	userSpec = &(catReq->userSpec);
	datasetPolicy = procDesc->datasetPolicy;
	sensorPolicy = procDesc->sensorPolicy;
	clientReq = &(procDesc->clientReq);
	msgStruct = procDesc->msgStruct;
	userCap = &(catReq->userCap);

	/*
	** If versions are supported we should set the request version
	** number to 0.  This tells our granule search to find the granule
	** with the latest version.
	*/
	if (datasetPolicy->version_p == 'Y')
	{
		clientReq->version = 0;
	}

	/*
	** Set user specification to open a database connection.
	*/
	userSpec->dbUserName = procDesc->surrogate.login;
	userSpec->dbPassword = procDesc->surrogate.password;
	userSpec->program = procDesc->ftsProgramName;
	userSpec->server = procDesc->catSrvName;
	userSpec->database = procDesc->catDbName;

	/*
	** Open database connection. This connection is to remain open
	** for the entire granule event.
	*/
	if (ims_srvCat (srvproc, FTS_OPEN_CONNECTION) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL, EFTS_OPEN_CONNECTION);
		return (IMS_FATAL);
	}

	/*
	** Get user capability to add a granule.
	*/
	userCap->userName = procDesc->userName;
	userCap->accountId = clientReq->accountId;
	userCap->capMask = 8;

	if ((status = ims_srvCat (srvproc, FTS_GET_USER_CAP)) < IMS_OK)
	{
		return (status);
	}


	/*
	** Lock the mutex.
	*/
	if (ims_lockMutexWait (procDesc->threadMutex) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_ERROR,
			"Could not lock thread mutex; Server busy. Please try again.");
		return (IMS_ERROR);
	}

	/*
	** Raise priority.
	*/
	if (ims_changePriority (srvproc, FTS_PRIORITY_DELTA) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Could not raise client thread priority. Contact the DBA.");

		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		return (IMS_FATAL);
	}

	/*
	** Check for existence of a granule.
	*/
	if ((status = ims_srvCat (srvproc, FTS_SEARCH_GRANULE)) < IMS_OK)
	{
		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}

	/* Assign the granuleSpec pointer. */
	granuleSpec = catReq->granuleSpec;

	/*
	** When versions are not suported we should not get a record
	** returned from the searchGranule function.
	*/
	if (datasetPolicy->version_p != 'Y')
	{
		/* This structure is allocated when a record is returned. */
		if ((granuleSpec != (FTS_CAT_GRANULESPEC *) NULL) &&
			(granuleSpec->granule_idx > 0))
		{
			if (granuleSpec->status < FTS_STATUS_COUNT)
				(void) ims_msg (msgStruct, IMS_ERROR,
					"Granule '%s' already exists for '%s, %s, %s'; %s", 
					clientReq->name, sensorPolicy->platform, 
					sensorPolicy->sensor, datasetPolicy->dataset,
					procDesc->statusTable->description[granuleSpec->status]);
			else
				(void) ims_msg (msgStruct, IMS_ERROR,
			"Granule '%s' already exists for '%s, %s, %s'; with status (%d).", 
					clientReq->name, sensorPolicy->platform, 
					sensorPolicy->sensor, datasetPolicy->dataset,
					granuleSpec->status);

	
			/* Unlock mutex and lower priority. */
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);
	
			return (IMS_ERROR);
		}
	}

	if ((status = ims_srvCat (srvproc, FTS_BEGIN_TRANSACTION)) < IMS_OK)
	{
		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}

	/*
	** Increment the granule_idx value.
	*/
	if ((status = ims_srvCat (srvproc, FTS_INCR_GRANULEIDX)) < IMS_OK)
	{
		(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}

	/* Initialize granuleSpec structure members. */
	granuleSpec = catReq->granuleSpec;
	granuleSpec->status = IMS_ADD_IN_PROGRESS;
	if (datasetPolicy->version_p == 'Y')
	{
		if (granuleSpec->version <= 0)
		{
			granuleSpec->version = 1;
		}
		else
		{
			granuleSpec->version += 1;
		}
	}

	/*
	** Insert a record into the appropriate granules_x table.
	*/
	if ((status = ims_srvCat (srvproc, FTS_INSERT_GRANULE)) < IMS_OK)
	{
		(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}

	/*
	** Commit the transaction.
	*/
	if ((status = ims_srvCat (srvproc, FTS_COMMIT_TRANSACTION)) < IMS_OK)
	{
		(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}

	/* Unlock mutex and lower priority. */
	if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Could not unlock thread mutex. Contact the DBA.");
	}
	(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

	/*
	** Determine the path policy based on granule index.
	*/
	pathPolicy = (FTS_PATH_POLICY *) datasetPolicy->pathPolicy; 

	while (pathPolicy != (FTS_PATH_POLICY *) NULL)
	{
		if ((pathPolicy->start_granule == -1) &&
			(pathPolicy->end_granule == -1))
		{
			break;
		}	

		if ((pathPolicy->start_granule <= granuleSpec->granule_idx) &&
			(pathPolicy->end_granule == -1))
		{
			break;
		}	

		if ((pathPolicy->start_granule <= granuleSpec->granule_idx) &&
			(granuleSpec->granule_idx <= pathPolicy->end_granule))
		{
			break;  /* Found a matching policy path for the granule */
		}
		pathPolicy = pathPolicy->next;
	} 

	/* No match means big policy problems. */
	if (pathPolicy == (FTS_PATH_POLICY *) NULL)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"No path was found for the given granule index %d. Possible installation failure.  Contact the DBA.",
		granuleSpec->granule_idx);
		return (IMS_FATAL);
    }

	/* Set pointer in procDesc to path found. */
	procDesc->pathPolicy = pathPolicy;  

	/*
	** A new record was inserted into the granules_x table to record the
	** addition of the requested granule.  Set rollbackOnExit flag to "ON",
	** so that the server would cleanup the database tables when possible.
	*/
	procDesc->rollbackOnExit = 1;

	return (IMS_OK);
}

/******************************************************************************
**
** ims_addEventEnd ()
**
******************************************************************************/

int ims_addEventEnd (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	FTS_GRANULE_DESC *granuleDesc;
	FTS_FILE_LIST *fileList;
	FTS_CAT_STRUCT *catReq;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_CAT_STATESPEC *stateSpec;
	FTS_BUFFERSPEC_TYPE bufferSpec;
	FTS_SENSOR_POLICY *sensorPolicy;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_CLIENT_REQUEST *clientReq;
	IMS_MSG_STRUCT *msgStruct;
	CS_SERVERMSG srvMsg;
	char auxBuffer[IMS_COL255_LEN+1];
	int pid;
	int lastMsg; 		/* last message flag from the auxProcess. */
	int i;
	int status;
	int savefd0;		/* Storage for saving file descriptors.*/
	int savefd1;
	IMS_HASH_ENTRY item;
	IMS_HASH_ENTRY *hashEntry;
	int count;
	char msg[CS_MAX_MSG];
	fd_set fds;
	int max_descriptors;
	struct timeval timeout;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_addEventEnd.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_addEventEnd.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}
	
	granuleDesc = &(procDesc->granuleDesc);
	fileList = granuleDesc->fileList;
	catReq = &(procDesc->catReq);
	granuleSpec = catReq->granuleSpec;
	stateSpec = &(catReq->stateSpec);
	sensorPolicy = procDesc->sensorPolicy;
	datasetPolicy = procDesc->datasetPolicy;
	clientReq = &(procDesc->clientReq);
	msgStruct = procDesc->msgStruct;

	/*
	** Change the status to indicate that we've got the granule.
	*/
	stateSpec->status = IMS_ADD_TRANSFER_COMPLETE;

	/*
	** Start critical section to avoid deadlock.
	*/
	(void) ims_lockMutexWait (procDesc->threadMutex);
	(void) ims_changePriority (srvproc, FTS_PRIORITY_DELTA);

	if ((status = ims_srvCat (srvproc, FTS_CHANGE_GRANULE_STATE)) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Changing granule record status failed. Contact the DBA.");

		/* Unlock thread mutex and lower priority. */
		(void) ims_unlockMutex (procDesc->threadMutex);
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (IMS_FATAL);
	}

	/*
	**  Update file sizes transfered in the granules table.
	*/

	if (updateFileSizes(srvproc, procDesc) < IMS_OK)
	{

		(void) ims_msg (msgStruct, IMS_FATAL,
			"Could not update file sizes in granules table.");
		return(IMS_FATAL);		
	}


	/* Unlock thread mutex and lower priority. */
	(void) ims_unlockMutex (procDesc->threadMutex);
	(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

	/*
	** Reset status value.
	*/
	status = IMS_OK;

	/*
	** Granule transfer into storage is complete at this point.
	** Start any auxiliary process if required as the last step.
	*/
	if (procDesc->datasetPolicy->load_program[0] != '\0')
	{
		/*
		** Yield for other client threads.
		*/
		(CS_VOID) srv_yield ();

		/*
		** Lock the mutex.
		*/
		if (ims_lockMutexWait (procDesc->auxMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not lock mutex for auxProcess. Please try again.");
			return (IMS_FATAL);
		}

		/*
		** All the lines below must be collected and placed into a 
		** function for clarity and reduction of errors.
		*/

		/*
		** We must get a duplicate copy of the fileDescriptors 0 and 1
		** before we continue with the auxiliary process execution and
		** communication.  invokeAux redirects filedescriptors 0 and 1
		** for communication between the server and auxiliary process.
		** The saved fileDescriptors will be used to reinstate the 
		** fileDescriptors 0 and 1 to their default stdout and
		** stdin streams.
		*/
		if ((savefd0 = dup (0)) == -1)
		{
			if (ims_unlockMutex (procDesc->auxMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock mutex for auxProcess. Contact the DBA.");
			}
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Dup system call failed for file descriptor '0'. Contact the DBA.");
		    procDesc->rollbackOnExit = 1;
			return (IMS_FATAL);
		}

		if ((savefd1 = dup (1)) == -1)
		{
			(void) close (savefd0);
			if (ims_unlockMutex (procDesc->auxMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock mutex for auxProcess. Contact the DBA.");
			}
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Dup system call failed for file descriptor '1'. Contact the DBA.");
		    procDesc->rollbackOnExit = 1;
			return (IMS_FATAL);
		}

		if (invokeAux (procDesc->datasetPolicy, &pid, msgStruct) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not invoke process '%s'.  Possible installation failure.  Contact the DBA.",
				procDesc->datasetPolicy->load_program);

			/*
			** Reset the file descriptors before returning..
			*/
			(void) close (0);
			(void) close (1);

			if ((dup (savefd0) == -1) || (dup (savefd1) == -1))
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Dup system call failed for saved file descriptors.");
			}
			(void) close (savefd0);
			(void) close (savefd1);

			if (ims_unlockMutex (procDesc->auxMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock mutex for auxProcess. Contact the DBA.");
			}
		    procDesc->rollbackOnExit = 1;
			return (IMS_FATAL);
		}

		/*
		** Turn off rollback flag.
		*/
		procDesc->rollbackOnExit = 0;

		/*
		** Start communication with the auxiliary process
		*/
		lastMsg = 0;
		i=0;
		max_descriptors = 1;

		while (! lastMsg)
		{
			i++;

			/*
			** Wait for activity on the stdin pipe.
			*/

			timeout.tv_sec = 0;
			timeout.tv_usec = 50000;
			FD_SET(0, &fds);


			while ((!FD_ISSET(0, &fds)) && (getpgid(pid) > 0))
			{
				srv_yield();
				srv_yield();
				srv_yield();

				select(max_descriptors + 1, &fds, (fd_set *) 0, (fd_set *) 0,
					&timeout);
			}



			if (read (0, auxBuffer, IMS_COL255_LEN+1) == 0)
			{
				/*
				** Premature EOF detected, AUXP is terminated
				*/

				/*
				** Reset the file descriptors before returning..
				*/
				(void) close (0);
				(void) close (1);

				if ((dup (savefd0) == -1) ||
					(dup (savefd1) == -1))
				{
					(void) ims_msg (msgStruct, IMS_FATAL,
						"Dup system call failed for saved file descriptors.");
				}
				(void) close (savefd0);
				(void) close (savefd1);

				if (i > 1)
				{
					(void) ims_msg (msgStruct, IMS_FATAL,
						"Premature process termination:\
%s.",
						procDesc->datasetPolicy->load_program);
				}
				else
				{
					(void) ims_msg (msgStruct, IMS_FATAL,
						"Could not run auxilliary process '%s'.\nInstallation suspect. Contact the DBA.",
						procDesc->datasetPolicy->load_program);
				}

				if (ims_unlockMutex (procDesc->auxMutex)
					< IMS_OK)
				{
					(void) ims_msg (msgStruct, IMS_FATAL,
						"Could not unlock mutex for auxProcess. Contact the DBA.");
				}
		        procDesc->rollbackOnExit = 1;
				return (IMS_FATAL);
			}

			/*
			** Decode message from aux process and draft replies if
			** necessery, or process messages.
			*/

			decodeAuxProcMessage (auxBuffer, &bufferSpec, procDesc);

			/*
			** Three buffer types are possible: CMD(1), MSG(2),
			** OTHER
			*/

			switch (bufferSpec.kind)
			{
			case 1: /* send reply */
				write (1, bufferSpec.reply, 
					 strlen (bufferSpec.reply) + 1);

				if (bufferSpec.LCF) 
				{
					/*
					** Heuristic wait until the auxProcess
					** is complete.
					*/
					count = (granuleDesc->dataBytes / 1000) + 1;
					for (i = 0; i < count; i++)
					{
						(CS_VOID) srv_yield (); 
						(CS_VOID) srv_yield ();
						(CS_VOID) srv_yield ();
					}
				}
				break;

			case 2: /* put message in the msgQueue */
				if (bufferSpec.LMF)
				{
					lastMsg = 1; /*last message*/
				}
				else if ((status = bufferSpec.severity)
					< IMS_OK)
				{
					/*
					** Here we must not include the label.
					*/
					(void) ims_msg (msgStruct,
						bufferSpec.severity,
						bufferSpec.reply);
				}
				break;

			default:  /* Just ignore it */
				break;
			}
		}

		/*
		** Reset the fileDescriptors before returning..
		*/
		(void) close (0);
		(void) close (1);

		if ((dup (savefd0) == -1) || (dup (savefd1) == -1))
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Dup system call failed for saved file descriptors.");
		}
		(void) close (savefd0);
		(void) close (savefd1);
	
		(CS_VOID) srv_yield ();


		/*
		** Now wait for the child to terminate
		*/

#if WAIT_FOR_CHILD
		/*
		** Issue: Why wait for child to terminate.  Maybe the child wants
		** to do some independent processing.  Why block FTS?????
		** Once the pipe closed, they should be independent....
		*/

		if (ims_waitForChild (msgStruct, pid) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Auxiliary Process, '%s', did not terminate normally.",
				datasetPolicy->load_program);

			if (ims_unlockMutex (procDesc->auxMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock mutex for auxProcess. Contact the DBA.");
			}
		    procDesc->rollbackOnExit = 1;
			return (IMS_FATAL);
		}
#endif

		if (ims_unlockMutex (procDesc->auxMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock mutex for auxProcess. Contact the DBA.");
			return(IMS_FATAL);		
		}

	}

	/*
	** Modified so server will accept warnings from aux processes.
	*/

	if (status < IMS_WARNING) 
	{
		/*
		** auxProcess failed
		*/
		procDesc->rollbackOnExit = 1;
		return (status);
	}


	/*
	** Turn off rollback on exit.
	*/
	procDesc->rollbackOnExit = 0;

	/*
	** Change the status of the new record to available.
	*/
	stateSpec->status = IMS_AVAILABLE;

	/*
	** Start critical section to avoid deadlock.
	*/
	(void) ims_lockMutexWait (procDesc->threadMutex);
	(void) ims_changePriority (srvproc, FTS_PRIORITY_DELTA);

	if ((status = ims_srvCat (srvproc, FTS_CHANGE_GRANULE_STATE)) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Changing granule record status failed. Contact the the DBA.");

		/* Unlock thread mutex and lower priority. */
		(void) ims_unlockMutex (procDesc->threadMutex);
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (IMS_FATAL);
	}

	/* Unlock thread mutex and lower priority. */
	(void) ims_unlockMutex (procDesc->threadMutex);
	(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

	/*
	** All processing steps are OK.  Send an OK status to client
	*/
	return (IMS_OK);
}

/******************************************************************************
**
** ims_deleteEvent ()
**
** Delete a granule from storage.
**
******************************************************************************/

int ims_deleteEvent (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	FTS_CLIENT_REQUEST *clientReq;
	FTS_CAT_STRUCT *catReq;
	FTS_CAT_USERSPEC *userSpec;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_CAT_GRANULESPEC *granulePtr;
	FTS_CAT_STATESPEC *stateSpec;
	FTS_SENSOR_POLICY *sensorPolicy;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_FILE_POLICY *filePolicy;
	FTS_FORMAT_POLICY *formatPolicy;
	FTS_PATH_POLICY *pathPolicy;
	IMS_MSG_STRUCT *msgStruct;
	IMS_HASH_ENTRY item;
	IMS_HASH_ENTRY *hashEntry;
	CS_SERVERMSG srvMsg;
	FTS_CAT_USERCAP *userCap;
	char fullPathName[IMS_PATH_LEN+IMS_COL60_LEN+1];
	char fileName[IMS_COL60_LEN+1];
	int status;
	int errFlag;
	char msg[CS_MAX_MSG];

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_deleteEvent.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_deleteEvent.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}
	
	clientReq = &(procDesc->clientReq);
	catReq = &(procDesc->catReq);
	userSpec = &(catReq->userSpec);
	stateSpec = &(catReq->stateSpec);
	sensorPolicy = procDesc->sensorPolicy;
	datasetPolicy = procDesc->datasetPolicy;
	filePolicy = procDesc->filePolicy;
	msgStruct = procDesc->msgStruct;
	userCap = &(catReq->userCap);

	/*
	** Check versions policy.
	*/
	if ((datasetPolicy->version_p == 'N') && (clientReq->version >= 0))
	{
		(void) ims_msg (msgStruct, IMS_ERROR,
			"Policy for '%s, %s, %s' does not support versions. Client delete request must not include a version number.",
			sensorPolicy->platform, sensorPolicy->sensor,
			datasetPolicy->dataset);
		return (IMS_ERROR);
	}

	if ((datasetPolicy->version_p == 'Y') && (clientReq->version < 0))
	{
		(void) ims_msg (msgStruct, IMS_ERROR,
			"Policy for '%s, %s, %s' supports versions. Client delete request must include a version number.",
			sensorPolicy->platform, sensorPolicy->sensor,
			datasetPolicy->dataset);
		return (IMS_ERROR);
	}

	userSpec->dbUserName = procDesc->surrogate.login;
	userSpec->dbPassword = procDesc->surrogate.password;
	userSpec->program = procDesc->ftsProgramName;
	userSpec->server = procDesc->catSrvName;
	userSpec->database = procDesc->catDbName;

	/*
	** Open a database connection.
	*/
	if ((status = ims_srvCat (srvproc, FTS_OPEN_CONNECTION)) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL, EFTS_OPEN_CONNECTION);
		return (IMS_FATAL);
	}

	/*
	** Get user capability to delete a file.
	*/
	userCap->userName = procDesc->userName;
	userCap->accountId = clientReq->accountId;
	userCap->capMask = 2;

	if ((status = ims_srvCat (srvproc, FTS_GET_USER_CAP)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Lock the mutex.
	*/
	if (ims_lockMutexWait (procDesc->threadMutex) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_ERROR,
			"Could not lock thread mutex; Server busy. Please try again.");
		return (IMS_ERROR);
	}

	/*
	** Raise priority.
	*/
	if (ims_changePriority (srvproc, FTS_PRIORITY_DELTA) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Could not raise client thread priority. Contact the DBA.");
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		return (IMS_FATAL);
	}

	/*
	** Check for existence of the granule or granules.
	*/
	if ((status = ims_srvCat (srvproc, FTS_SEARCH_GRANULES)) < IMS_OK)
	{
		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}



	/* Assign the granuleSpec pointers. */
	granuleSpec = catReq->granuleSpec;
	granulePtr = granuleSpec;
	errFlag = 0;

	/*
	** Start a transaction
	*/
	if ((status = ims_srvCat (srvproc, FTS_BEGIN_TRANSACTION)) < IMS_OK)
	{
		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}

	/*
	** Change the status of the granule records.
	*/
	stateSpec->status = IMS_DELETE_IN_PROGRESS;

	if ((status = ims_srvCat (srvproc, FTS_CHANGE_GRANULE_STATE)) < IMS_OK)
	{
		(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

		/* Unlock mutex and lower priority. */
		(void) ims_unlockMutex (procDesc->threadMutex);
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}

	/*
	** Check user capability to delete the granule.
	** If user is the Data Administrator, then access is granted.
	*/
	granulePtr = granuleSpec;
	while (granulePtr != (FTS_CAT_GRANULESPEC *) NULL)
	{
		/*
		** Check user capability to delete file.
		** If user is owner of the file, then access is granted.
		*/
		if (strcmp(userCap->userName, granulePtr->contributor))
		{
			/* User is not owner, check delete capability */
			if ((granulePtr->o_gdr & 2) != 2) 
			{
				errFlag = 1;
				if (granulePtr->version > 0)
				{
					(void) ims_msg (msgStruct, IMS_ERROR,
						"User '%s' not authorized to delete granule '%s', version '%d' for '%s, %s, %s'.",
						userCap->userName, granulePtr->name,
	 					granulePtr->version,
						sensorPolicy->platform,
						sensorPolicy->sensor,
						datasetPolicy->dataset);
				}
				else
				{
					(void) ims_msg (msgStruct, IMS_ERROR,
						"User '%s' not authorized to delete granule '%s' for '%s, %s, %s'.",
						userCap->userName, granulePtr->name,
						sensorPolicy->platform,
						sensorPolicy->sensor,
						datasetPolicy->dataset);
				}	
			}
		}
		granulePtr = granulePtr->next;
	}

	/*
	** Check the read count for the granule.  If the read count
	** is greater than zero, then this granule can not be deleted.
	*/

	if ((status = ims_srvCat (srvproc, FTS_GET_READ_COUNT)) < IMS_OK)
	{
		(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);
		return (status);
	}

	/*
	** Check all read count values for granule list now that we
	** have obtained the list.
	*/

	granulePtr = granuleSpec;
	while ((granulePtr != (FTS_CAT_GRANULESPEC *) NULL) && (errFlag == 0))
	{

		if (granulePtr->readCount > 0)
		{
				errFlag = 1;

				(void) ims_msg (msgStruct, IMS_ERROR,
					"Granule '%s' is currently being read by another user.",
					granulePtr->name);

		}
		granulePtr = granulePtr->next;
	}


	/*
	** Rollback the transaction if the user did not have privileges.
	*/
	if (errFlag == 1)
	{
		(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (IMS_ERROR);
	}

	/*
	** Check whether any granule is being read by another user.
	*/
	errFlag = 0;
	granulePtr = granuleSpec;
	item.data = (void *) NULL;

	while (granulePtr != (FTS_CAT_GRANULESPEC *) NULL)
	{
		item.key = (void *) &(granulePtr->granule_idx);

		if (((hashEntry = ims_hashSearch (procDesc->readHashPtr, &item,
			IMS_FIND, msgStruct)) != (IMS_HASH_ENTRY *) NULL))
		{
			errFlag = 1;
			if (granulePtr->version > 0)
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
					"Granule '%s', version '%d' for '%s, %s, %s' is not available; Granule is being read by another user.",
					granulePtr->name, granulePtr->version,
					sensorPolicy->platform,
					sensorPolicy->sensor,
					datasetPolicy->dataset);
			}
			else
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
					"Granule '%s' for '%s, %s, %s' is not available; Granule is being read by another user.",
					granulePtr->name, 
					sensorPolicy->platform,
					sensorPolicy->sensor,
					datasetPolicy->dataset);
			}
		}

		if (granulePtr->status != IMS_AVAILABLE)
		{
			errFlag = 1;
			if (granulePtr->version > 0)
			{
				if (granulePtr->status < FTS_STATUS_COUNT)
				{
					(void) ims_msg (msgStruct, IMS_ERROR,
			"Granule '%s', version '%d' for '%s, %s, %s' is not available; %s",
						granulePtr->name, granulePtr->version,
						sensorPolicy->platform, sensorPolicy->sensor,
						datasetPolicy->dataset,
						procDesc->statusTable->description[granulePtr->status]);
				}
				else
				{
					(void) ims_msg (msgStruct, IMS_ERROR,
	"Granule '%s', version '%d' for '%s, %s, %s' is not available; status (%d)",
						granulePtr->name, granulePtr->version,
						sensorPolicy->platform, sensorPolicy->sensor,
						datasetPolicy->dataset,
						granulePtr->status);
				}
			}
			else
			{
				if (granulePtr->status < FTS_STATUS_COUNT)
				{
					(void) ims_msg (msgStruct, IMS_ERROR,
						"Granule '%s' for '%s, %s, %s' is not available; %s",
						granulePtr->name, 
						sensorPolicy->platform, sensorPolicy->sensor,
						datasetPolicy->dataset,
						procDesc->statusTable->description[granulePtr->status]);
				}
				else
				{
					(void) ims_msg (msgStruct, IMS_ERROR,
			"Granule '%s' for '%s, %s, %s' is not available; status (%d)",
						granulePtr->name, 
						sensorPolicy->platform, sensorPolicy->sensor,
						datasetPolicy->dataset,
						granulePtr->status);
				}
			}
		}

		granulePtr = granulePtr->next;
	}

	/*
	** Rollback the transaction if the granule was not available.
	*/
	if (errFlag == 1)
	{
		(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (IMS_ERROR);
	}

	/*
	** Commit transaction
	*/
	if ((status = ims_srvCat (srvproc, FTS_COMMIT_TRANSACTION)) < IMS_OK)
	{
		(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}

	/*
	** Unlock mutex and lower priority.
	*/
	if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Could not unlock thread mutex. Contact the DBA.");
	}
	(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

	/*
	** When policy is marked for delete, then change the status of
	** the granule record in the granules_x table to IMS_MARK_FOR_DELETE.
	** The granule and granule record are kept for later processing by the
	** data administrator.
	*/

	if (datasetPolicy->mfd_p == 'Y') 
	{
		/*
		** Change the status of the new record.
		*/
		stateSpec->status = IMS_MARK_FOR_DELETE;

		/*
		** Start critical section to avoid deadlock.
		*/
		(void) ims_lockMutexWait (procDesc->threadMutex);
		(void) ims_changePriority (srvproc, FTS_PRIORITY_DELTA);

		if ((status = ims_srvCat (srvproc, FTS_CHANGE_GRANULE_STATE)) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Changing file record status failed. Contact the DBA.");

			/* Unlock mutex and lower priority. */
			(void) ims_unlockMutex (procDesc->threadMutex);
			(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

			return (IMS_FATAL);
		}

		/*
		** Unlock mutex and lower priority.
		*/
		(void) ims_unlockMutex (procDesc->threadMutex);
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);
	}
	else
	{
		/*
		** Delete the files for each granule from the repository directory.
		*/
		while (granuleSpec != (FTS_CAT_GRANULESPEC *) NULL)
		{

			/*
			** Determine the path policy based on granule index.
			*/
			pathPolicy = (FTS_PATH_POLICY *) datasetPolicy->pathPolicy;
			while (pathPolicy != (FTS_PATH_POLICY *) NULL)
			{
				if ((pathPolicy->start_granule == -1) &&
					(pathPolicy->end_granule == -1))
				{
					break;
				}	

				if ((pathPolicy->start_granule <= granuleSpec->granule_idx) &&
					(pathPolicy->end_granule == -1))
				{
					break;
				}	

		        if ((pathPolicy->start_granule <= granuleSpec->granule_idx) &&
			        (granuleSpec->granule_idx <= pathPolicy->end_granule))
				{
					break;
				}
				pathPolicy = pathPolicy->next;
			}

			/* No match means big policy problems. */
			if (pathPolicy == (FTS_PATH_POLICY *) NULL)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"No path was found for the given granule index %d. Possible installation failure.  Contact the DBA.",
					granuleSpec->granule_idx);
				return (IMS_FATAL);
			}


			/*
			** Determine the file policy based on format.
			*/
			filePolicy = datasetPolicy->filePolicy;
			while (filePolicy != (FTS_FILE_POLICY *) NULL)
			{
				if (strcmp (filePolicy->format,
					granuleSpec->format) == 0)
				{
					break;
				}
				filePolicy = filePolicy->next;
			}

			/* No match means big policy problems. */
			if (filePolicy == (FTS_FILE_POLICY *) NULL)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"The format returned by the catalog, %s, does not match a format in the server's cache policy. Possible installation failure.  Contact the DBA.",
					granuleSpec->format);
				return (IMS_FATAL);
			}

			formatPolicy = filePolicy->formatPolicy;
			while (formatPolicy != (FTS_FORMAT_POLICY *) NULL)
			{
				/*
				** Construct the file name as it appears on disk.
				*/
				if (granuleSpec->version > 0)
				{
					(void) sprintf (fileName, "%s.%s.%d",
						 granuleSpec->name, formatPolicy->extension,
						 granuleSpec->version);

					ims_concatFilePath (fullPathName,
						pathPolicy->path, fileName);
				}
				else
				{
					(void) sprintf (fileName, "%s.%s",
						granuleSpec->name, formatPolicy->extension);

					ims_concatFilePath (fullPathName,
						pathPolicy->path, fileName);
				}

				/*
				** Remove the file from the repository.
				*/
				if (unlink (fullPathName) == -1)
				{
					(void) ims_msg (msgStruct, IMS_FATAL,
						"Could not delete file '%s' for '%s, %s, %s' from storage. %s.  Possible policy conflict or installation failure. Contact the DBA.",
						ims_extractFileName (fullPathName),
						sensorPolicy->platform, sensorPolicy->sensor,
						datasetPolicy->dataset, strerror (errno));
					return (IMS_FATAL);
				}
				formatPolicy = formatPolicy->next;
			}
			granuleSpec = granuleSpec->next;
		}

		/*
		** Lock the mutex.
		*/
		if (ims_lockMutexWait (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_ERROR,
				"Could not lock thread mutex; Server busy. Please try again.");
			return (IMS_ERROR);
		}

		/*
		** Raise priority.
		*/
		if (ims_changePriority (srvproc, FTS_PRIORITY_DELTA) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not raise client thread priority. Contact the DBA.");
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			return (IMS_FATAL);
		}

		/*
		** Drop the granules_x record from the table, now that the files have
		** been unlinked.
		*/
		if (ims_srvCat (srvproc, FTS_DELETE_GRANULE_RECORD) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not delete granule record from the granules_x table. Contact the DBA.");

			/* Unlock mutex and lower priority. */
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

			return (IMS_FATAL);
		}

		/*
		** Unlock mutex and lower priority.
		*/
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);
	}

	/*
	** All processing steps are OK.  Send an OK status to client
	*/
	return (IMS_OK);
}

/******************************************************************************
**
** ims_getEventBegin () 
**
** Process file transfer request from CDB
**
** This routine is called by ims_fts server program to process the file
** transfer request out of CDB storage.
**
******************************************************************************/

int ims_getEventBegin (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;  /* FT process descriptor. */
	FTS_CAT_STRUCT *catReq;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_GRANULE_DESC *granuleDesc;
	FTS_FILE_LIST *currFile;
	FTS_CAT_USERSPEC *userSpec;
	FTS_CAT_USERCAP *userCap;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_PATH_POLICY *pathPolicy;
	FTS_FILE_POLICY *filePolicy;
	FTS_SENSOR_POLICY *sensorPolicy;
	FTS_CLIENT_REQUEST *clientReq;
	IMS_MSG_STRUCT *msgStruct;
	IMS_CLNT_REQUEST_TYPE requestType;
	FTS_FORMAT_POLICY *formatPolicy;
	CS_SERVERMSG srvMsg;
	char fileName[IMS_COL60_LEN+1];
	int fileCount;
	int status;
	char msg[CS_MAX_MSG];

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_getEventBegin.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_getEventBegin.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}
	
	catReq = &(procDesc->catReq);
	datasetPolicy = procDesc->datasetPolicy;
	sensorPolicy = procDesc->sensorPolicy;
	userCap = &(catReq->userCap);
	userSpec = &(catReq->userSpec);
	clientReq = &(procDesc->clientReq);
	granuleSpec = catReq->granuleSpec;
	granuleDesc = &(procDesc->granuleDesc);
	currFile = granuleDesc->currFile;
	requestType = clientReq->requestType;
	msgStruct = procDesc->msgStruct;
	userCap = &(catReq->userCap);

	/*
	** Check versions policy when the client request is not latest file.
	** Version number is ignored for latest file operation.
	*/
	if (clientReq->requestType != IMS_GET_LATEST)
	{
		if ((datasetPolicy->version_p == 'N') && (clientReq->version >= 0))
		{
			(void) ims_msg (msgStruct, IMS_ERROR,
				"Policy for '%s, %s, %s' does not support versions. Client request must not include a version number.",
				sensorPolicy->platform, sensorPolicy->sensor,
				datasetPolicy->dataset);
			return (IMS_ERROR);
		}

		if ((datasetPolicy->version_p == 'Y') && (clientReq->version < 0))
		{
			(void) ims_msg (msgStruct, IMS_ERROR,
				"Policy for '%s, %s, %s' supports versions. Client request must include a version number.",
				sensorPolicy->platform, sensorPolicy->sensor,
				datasetPolicy->dataset);
			return (IMS_ERROR);
		}
	}

	/*
	** Make a connection to the database using the surrogate username,
	** password and server names specified in catReq->userSpec.
	*/
	userSpec->dbUserName = procDesc->surrogate.login;
	userSpec->dbPassword = procDesc->surrogate.password;
	userSpec->program = procDesc->ftsProgramName;
	userSpec->server = procDesc->catSrvName;
	userSpec->database = procDesc->catDbName;

	/*
	** Open database connection.
	*/
	if ((status = ims_srvCat (srvproc, FTS_OPEN_CONNECTION)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Get user capability to get a file.
	*/
	userCap->userName = procDesc->userName;
	userCap->accountId = clientReq->accountId;
	userCap->capMask = 4;

	if ((status = ims_srvCat (srvproc, FTS_GET_USER_CAP)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Lock the mutex.
	*/
	if (ims_lockMutexWait (procDesc->threadMutex) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_ERROR,
			"Could not lock thread mutex; Server busy. Please try again.");
		return (IMS_ERROR);
	}

	/*
	** Raise priority.
	*/
	if (ims_changePriority (srvproc, FTS_PRIORITY_DELTA) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Could not raise client thread priority. Contact the DBA.");

		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		return (IMS_FATAL);
	}

	/*
	** Get the file specification from the database based on the
	** request type.
	*/
	switch (requestType)
	{
	case IMS_GET:
		if ((status = ims_srvCat (srvproc, FTS_SEARCH_GRANULE)) < IMS_OK)
		{
			/* Unlock mutex and lower priority. */
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

			return (status); 
		}
		break;

	case IMS_GET_MAX:
#ifdef MAX
		if ((status = ims_srvCat (srvproc, FTS_GET_MAX_GRANULE)) < IMS_OK)
		{
			/* Unlock mutex and lower priority. */
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

			return (status);
		}
#endif	/* MAX */
		break;

	case IMS_GET_LATEST:
		if ((status = ims_srvCat (srvproc, FTS_GET_LATEST_GRANULE)) < IMS_OK)
		{
			/* Unlock mutex and lower priority. */
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

			return (status);
		}
		break;

	default:
		(void) ims_msg (msgStruct, IMS_FATAL, 
			"Invalid get request type.");

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (IMS_FATAL);
	}

	/*
	** Check to see if we found a record in the granules_x table.
	*/
	granuleSpec = catReq->granuleSpec;
	if (granuleSpec == (FTS_CAT_GRANULESPEC *) NULL)
	{
		if (clientReq->version > 0)
		{
			(void) ims_msg (msgStruct, IMS_ERROR,
				"Granule %s, version %d for '%s, %s, %s' not found.",
				clientReq->name, clientReq->version,
				sensorPolicy->platform, sensorPolicy->sensor,
				datasetPolicy->dataset);
		}
		else
		{
			(void) ims_msg (msgStruct, IMS_ERROR,
				"Granule %s for '%s, %s, %s' not found.",
				clientReq->name, sensorPolicy->platform,
				sensorPolicy->sensor, datasetPolicy->dataset);
		}
		return (IMS_ERROR);
	}

	/*
	** Check if the format returned matches the format specified
	** by the client, if they specified one.
	*/
	if ((int) strlen (clientReq->format) > 0)
	{
		if (strcmp (granuleSpec->format, clientReq->format) != 0)
		{
			(void) ims_msg (msgStruct, IMS_ERROR,
				"The format for granule %s, %s, does not match the format requested by the client, %s.",
				granuleSpec->name, granuleSpec->format, clientReq->format);
			return (IMS_ERROR);
		}
	}

	/*
	** Check the user capability to get this file.
	** If user is the Data Administrator, then access is granted.
	** If user is the owner of the file, then access is granted.
	*/
	if (strcmp(userCap->userName, granuleSpec->contributor))
	{

		if ((granuleSpec->o_gdr & 4) != 4) 
		{
			if (granuleSpec->version > 0)
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
					"User '%s' not authorized to get granule '%s', version '%d' for '%s, %s, %s'.",
					userCap->userName, granuleSpec->name,
					granuleSpec->version, sensorPolicy->platform,
					sensorPolicy->sensor, datasetPolicy->dataset);
			}
			else
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
					"User '%s' is not authorized to get granule '%s' for '%s, %s, %s'.",
					userCap->userName, granuleSpec->name, 
					sensorPolicy->platform,
					sensorPolicy->sensor, datasetPolicy->dataset);
			}

			/* Unlock mutex and lower priority. */
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

			return (IMS_FATAL);
		}
	}

	/*
	** Check the status of the file returned.  We can only access files
	** with status value of IMS_AVAILABLE.
	*/
	if (granuleSpec->status != IMS_AVAILABLE) 
	{
		if (granuleSpec->version > 0)
		{
			if (granuleSpec->status < FTS_STATUS_COUNT)
			{

				(void) ims_msg (msgStruct, IMS_ERROR,
					"Granule '%s', version '%d' for '%s, %s, %s' is not available; %s",
					granuleSpec->name, granuleSpec->version, 
					sensorPolicy->platform,
					sensorPolicy->sensor, datasetPolicy->dataset,
					procDesc->statusTable->description[granuleSpec->status]);
			}			
			else
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
			"Granule '%s', version '%d' for '%s, %s, %s' is not available; status (%d)",
					granuleSpec->name, granuleSpec->version, 
					sensorPolicy->platform,
					sensorPolicy->sensor, datasetPolicy->dataset,
					granuleSpec->status);

			}
		}
		else
		{
			if (granuleSpec->status < FTS_STATUS_COUNT)
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
					"Granule '%s' for '%s, %s, %s' is not available; %s",
					granuleSpec->name, 
					sensorPolicy->platform,
					sensorPolicy->sensor, datasetPolicy->dataset,
					procDesc->statusTable->description[granuleSpec->status]);
			}
			else			
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
					"Granule '%s' for '%s, %s, %s' is not available; status (%d)",
					granuleSpec->name, 
					sensorPolicy->platform,
					sensorPolicy->sensor, datasetPolicy->dataset,
					granuleSpec->status);
			}
		}

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (IMS_ERROR);
	}

	/*
	** Now increment the granule read counter.
	*/
	if ((procDesc->readGranule = ims_incrReadGranule
		(procDesc->readHashPtr, granuleSpec->granule_idx, msgStruct)) ==
		(FTS_READ_GRANULE *) NULL)
	{
		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (IMS_FATAL);
	}

	/* Unlock mutex and lower priority. */
	if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Could not unlock thread mutex. Contact the DBA.");
	}
	(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

	/*
	** Turn on rollbackOnExit flag.
	*/
	procDesc->rollbackOnExit = 1;


	/*
	** Determine the path policy based on granule index.
	*/
	pathPolicy = (FTS_PATH_POLICY *) datasetPolicy->pathPolicy;

	while (pathPolicy != (FTS_PATH_POLICY *) NULL)
	{
		if ((pathPolicy->start_granule == -1) &&
			(pathPolicy->end_granule == -1))
		{
			break;
		}	

		if ((pathPolicy->start_granule <= granuleSpec->granule_idx) &&
			(pathPolicy->end_granule == -1))
		{
			break;
		}	

		if ((pathPolicy->start_granule <= granuleSpec->granule_idx) &&
			(granuleSpec->granule_idx <= pathPolicy->end_granule))
		{
			break;  /* Found a matching policy path for the granule */
		}
		pathPolicy = pathPolicy->next;
	} 

	/* No match means big policy problems. */
	if (pathPolicy == (FTS_PATH_POLICY *) NULL)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"No path was found for the given granule index %d. Possible installation failure.  Contact the DBA.",
		granuleSpec->granule_idx);
		return (IMS_FATAL);

	}

	/*
	** Determine the file policy based on format.
	*/
	filePolicy = datasetPolicy->filePolicy;
	while (filePolicy != (FTS_FILE_POLICY *) NULL)
	{
		if (strcmp (filePolicy->format,
			granuleSpec->format) == 0)
		{
			break;
		}
		filePolicy = filePolicy->next;
	}

	/* No match means big policy problems. */
	if (filePolicy == (FTS_FILE_POLICY *) NULL)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"The format returned by the catalog, %s, does not match a format in the server's cache policy. Possible installation failure.  Contact the DBA.",
			granuleSpec->format);
		return (IMS_FATAL);
	}

	/*
	** Populate the file list.
	*/
	formatPolicy = filePolicy->formatPolicy;
	fileCount = 0;
	currFile = (FTS_FILE_LIST *) NULL;

	while (formatPolicy != (FTS_FORMAT_POLICY *) NULL)
	{
		/*
		** If the dataset has files archive locally, then only send
		** the MTA file back to the client.
		*/ 

		if ((datasetPolicy->local_archive_p == 'Y') &&
			(strncmp(formatPolicy->extension, "MTA", 3)) &&
			(strcmp(formatPolicy->extension, "M")))
		{
			formatPolicy = formatPolicy->next;
			continue;
		}

		/*
		** Allocate space for the FTS_FILE_LIST structure.
		**
		** lint: pointer cast may result in improper alignment
		** ???? Review this.
		*/
		if ((currFile = (FTS_FILE_LIST *) malloc (sizeof (FTS_FILE_LIST)))
				== (FTS_FILE_LIST *) NULL)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Memory allocation for FTS_FILE_LIST failed.");
			return (IMS_FATAL);
		}

		/* Initialize structure members. */
		(void) strcpy (currFile->extension,
			ims_truncStr (formatPolicy->extension));
		currFile->fileIndex = 0;
		currFile->fd = -1;
		currFile->isFileOpen = IMS_FALSE;
		currFile->hasFileBeenOpen = IMS_FALSE;
		currFile->fileToWrite[0] = '\0';
		currFile->next = (FTS_FILE_LIST *)NULL;

		if (granuleSpec->version > 0)
		{
			(void) sprintf (fileName, "%s.%s.%d",
				 granuleSpec->name, currFile->extension, granuleSpec->version);
	
			ims_concatFilePath (currFile->fullPathName,
				pathPolicy->path, fileName);
		}
		else
		{
			(void) sprintf (fileName, "%s.%s",
				granuleSpec->name, currFile->extension);
	
			ims_concatFilePath (currFile->fullPathName,
				pathPolicy->path, fileName);
		}

		fileCount += 1;

		/* Place currFile in the list of files. */
		if (fileCount == 1)
		{
			granuleDesc->fileList = currFile;
			granuleDesc->currFile = currFile;
		}
		else
		{
			granuleDesc->currFile->next = currFile;
			granuleDesc->currFile = currFile;
		}

		formatPolicy = formatPolicy->next;
	}

	granuleDesc->currFile = granuleDesc->fileList;
	granuleDesc->fileCount = fileCount;


	/*
	** Send ack and the file Count to the client process. 
	*/
	return (IMS_OK);
}

/******************************************************************************
**
** ims_getEventEnd ()
**
******************************************************************************/

int ims_getEventEnd (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	CS_SERVERMSG srvMsg;
	char msg[CS_MAX_MSG];

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_getEventEnd.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_getEventEnd.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	/*
	** Turn off the rollback flag.
	*/
	procDesc->rollbackOnExit = 0;

	/*
	** Decrement the readCount for this file.
	*/
	if (ims_decrReadGranule (procDesc->readHashPtr, procDesc->readGranule,
		procDesc->msgStruct) < IMS_OK)
	{
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_replaceEventBegin ()
**
** Begin processing the granule replace event.
**
** The processing status, whether successful or fail, must be sent back 
** to the client.
**
******************************************************************************/

int ims_replaceEventBegin (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	FTS_CAT_GRANULESPEC *granuleSpec, *granulePtr;
	FTS_CAT_USERSPEC *userSpec;
	FTS_CAT_STATESPEC *stateSpec;
	FTS_CAT_STRUCT *catReq;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_PATH_POLICY *pathPolicy;
	FTS_SENSOR_POLICY *sensorPolicy;
	FTS_CLIENT_REQUEST *clientReq;
	IMS_MSG_STRUCT *msgStruct;
	IMS_HASH_ENTRY item;
	IMS_HASH_ENTRY *hashEntry;
	CS_SERVERMSG srvMsg;
	FTS_CAT_USERCAP *userCap;
	DBINT tempGranuleIdx;
	int status;
	char msg[CS_MAX_MSG];

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_replaceEventBegin.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_replaceEventBegin.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	catReq = &(procDesc->catReq);
	userSpec = &(catReq->userSpec);
	stateSpec = &(catReq->stateSpec);
	datasetPolicy = procDesc->datasetPolicy;
	sensorPolicy = procDesc->sensorPolicy;
	clientReq = &(procDesc->clientReq);
	msgStruct = procDesc->msgStruct;
	userCap = &(catReq->userCap);

	/*
	** Check versions policy.
	*/
	if ((datasetPolicy->version_p == 'N') && (clientReq->version >= 0))
	{
		(void) ims_msg (msgStruct, IMS_ERROR,
			"Policy for '%s, %s, %s' does not support versions. Client request must not include a version number.",
			sensorPolicy->platform, sensorPolicy->sensor,
			datasetPolicy->dataset);
		return (IMS_ERROR);
	}

	if ((datasetPolicy->version_p == 'Y') && (clientReq->version < 0))
	{
		(void) ims_msg (msgStruct, IMS_ERROR,
			"Policy for '%s, %s, %s' supports versions. Client replace request must include a version number.",
			sensorPolicy->platform, sensorPolicy->sensor,
			datasetPolicy->dataset);
		return (IMS_ERROR);
	}

	/*
	** Set user specification to open a database connection.
	*/
	userSpec->dbUserName = procDesc->surrogate.login;
	userSpec->dbPassword = procDesc->surrogate.password;
	userSpec->program = procDesc->ftsProgramName;
	userSpec->server = procDesc->catSrvName;
	userSpec->database = procDesc->catDbName;

	/*
	** Open database connection. This connection is to remain open
	** for the entire granule event.
	*/
	if (ims_srvCat (srvproc, FTS_OPEN_CONNECTION) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL, EFTS_OPEN_CONNECTION);
		return (IMS_FATAL);
	}

	/*
	** Get user capability to replace a file.
	*/
	userCap->userName = procDesc->userName;
	userCap->accountId = clientReq->accountId;
	userCap->capMask = 1;

	if ((status = ims_srvCat (srvproc, FTS_GET_USER_CAP)) < IMS_OK)
	{
		return (status);
	}
		
	/*
	** Lock the mutex.
	*/
	if (ims_lockMutexWait (procDesc->threadMutex) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_ERROR,
			"Could not lock thread mutex; Server busy. Please try again.");
		return (IMS_ERROR);
	}

	/*
	** Raise priority.
	*/
	if (ims_changePriority (srvproc, FTS_PRIORITY_DELTA) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Could not raise client thread priority. Contact the DBA.");

		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		return (IMS_FATAL);
	}

	/*
	** Check for existence of a granule.
	*/
	if ((status = ims_srvCat (srvproc, FTS_SEARCH_GRANULE)) < IMS_OK)
	{
		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}

	/* Initialize granuleSpec structure members. */
	granuleSpec = catReq->granuleSpec;

	/*
	** Check the read count for the granule.  If the read count
	** is greater than zero, then this granule can not be deleted.
	*/

	if ((status = ims_srvCat (srvproc, FTS_GET_READ_COUNT)) < IMS_OK)
	{
		(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);
		return (status);
	}

	/*
	** Check all read count values for granule list now that we
	** have obtained the list.
	*/
	

	granulePtr = granuleSpec;
	while (granulePtr != (FTS_CAT_GRANULESPEC *) NULL) 
	{

		if (granulePtr->readCount > 0)
		{

				(void) ims_msg (msgStruct, IMS_ERROR,
					"Granule '%s' is currently being read by another user.",
					granulePtr->name);
				return(IMS_ERROR);

		}
		granulePtr = granulePtr->next;
	}


	/*
	** Start a transaction.
	*/
	if ((status = ims_srvCat (srvproc, FTS_BEGIN_TRANSACTION)) < IMS_OK)
	{
		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}


	/*
	** Increment the granule_idx value for our new record.
	*/
	if ((status = ims_srvCat (srvproc, FTS_INCR_GRANULEIDX)) < IMS_OK)
	{
		(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}

	/* Initialize granuleSpec structure members. */
	granuleSpec = catReq->granuleSpec;


	/*
	** Check user capability.
	** If user is Data Administrator, then access is granted.
	** If user is owner of the file, then access is granted.
	*/
	if 	(strcmp(userCap->userName, granuleSpec->contributor))
	{
		if ((granuleSpec->o_gdr & 1) != 1)
		{
			if (granuleSpec->version > 0)
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
					"User '%s' not authorized to replace granule '%s', version '%d' for '%s, %s, %s'.",
					userCap->userName, granuleSpec->name,
					granuleSpec->version, sensorPolicy->platform,
					sensorPolicy->sensor, datasetPolicy->dataset);
			}
			else
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
					"User '%s' not authorized to replace granule '%s' for '%s, %s, %s'.",
					userCap->userName, granuleSpec->name,
					sensorPolicy->platform, sensorPolicy->sensor,
					datasetPolicy->dataset);
			}

			(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

			/* Unlock mutex and lower priority. */
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

			return (IMS_ERROR);
		}
	}

	/*
	** Check the status of the granule to be replaced.
	*/
	if (granuleSpec->oldGranule_idx > 0)
	{
		item.key = (void *) &(granuleSpec->oldGranule_idx);

		/*
		** See if the granule is being read by another user.
		*/
		if (((hashEntry = ims_hashSearch (procDesc->readHashPtr, &item,
			IMS_FIND, msgStruct)) != (IMS_HASH_ENTRY *) NULL))
		{
			if (granuleSpec->version > 0)
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
					"Granule '%s', version '%d' for '%s, %s, %s' is not ready for access; Granule being read by another user.",
					granuleSpec->name, granuleSpec->version,
					sensorPolicy->platform, sensorPolicy->sensor,
					datasetPolicy->dataset);
			}
			else
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
					"Granule '%s' for '%s, %s, %s' is not ready for access; Granule being read by another user.",
					granuleSpec->name, sensorPolicy->platform,
					sensorPolicy->sensor, datasetPolicy->dataset);
			}

			(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

			/* Unlock mutex and lower priority. */
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

			return (IMS_ERROR);
		}

		/*
		** See if the status is set to available.
		*/
		if (granuleSpec->status != IMS_AVAILABLE)
		{
			if (granuleSpec->version > 0)
			{
				if (granuleSpec->status < FTS_STATUS_COUNT)
				{
					(void) ims_msg (msgStruct, IMS_ERROR,
						"Granule '%s', version '%d' for '%s, %s, %s' is not available; %s",
						granuleSpec->name, granuleSpec->version,
						sensorPolicy->platform,
						sensorPolicy->sensor, datasetPolicy->dataset,
						procDesc->statusTable->description[granuleSpec->status]);
				}
				else
				{
					(void) ims_msg (msgStruct, IMS_ERROR,
						"Granule '%s', version '%d' for '%s, %s, %s' is not available; status (%d)",
						granuleSpec->name, granuleSpec->version,
						sensorPolicy->platform,
						sensorPolicy->sensor, datasetPolicy->dataset,
						granuleSpec->status);
				}
			}
			else
			{
				if (granuleSpec->status < FTS_STATUS_COUNT)
				{
					(void) ims_msg (msgStruct, IMS_ERROR,
						"Granule '%s' for '%s, %s, %s' is not available; %s",
						granuleSpec->name, sensorPolicy->platform,
						sensorPolicy->sensor, datasetPolicy->dataset,
						procDesc->statusTable->description[granuleSpec->status]);
				}
				else
				{
					(void) ims_msg (msgStruct, IMS_ERROR,
						"Granule '%s' for '%s, %s, %s' is not available; status (%d)",
						granuleSpec->name, sensorPolicy->platform,
						sensorPolicy->sensor, datasetPolicy->dataset,
						granuleSpec->status);
				}
			}

			(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

			/* Unlock mutex and lower priority. */
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

			return (IMS_ERROR);
		}

		/*
		** Change the status of the granule to be replaced.
		*/
		stateSpec->status = IMS_REPLACE_IN_PROGRESS;
		tempGranuleIdx = granuleSpec->granule_idx;
		granuleSpec->granule_idx = granuleSpec->oldGranule_idx;

		if ((status = ims_srvCat (srvproc, FTS_CHANGE_GRANULE_STATE)) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Changing granule record status failed. Contact the DBA.");

			/* Unlock thread mutex and lower priority. */
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

			return (IMS_FATAL);
		}

		/* Set the granule_idx back to the current granule. */
		granuleSpec->granule_idx = tempGranuleIdx;

		/*
		** Keep the name stamp (time stamp).
		*/
		strcpy(granuleSpec->name_stamp, ims_timeStamp());
	}
	else /* The granule doesn't exist so we add a new one. */
	{
		(void) ims_msg (msgStruct, IMS_INFO,
			"Granule does not exist for replace. Granule will be added to storage.");
	}

	granuleSpec->status = IMS_REPLACE_IN_PROGRESS;

	/*
	** Insert a record into the appropriate granules_x table.
	*/
	if ((status = ims_srvCat (srvproc, FTS_INSERT_GRANULE)) < IMS_OK)
	{
		(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}

	/*
	** Commit the transaction.
	*/
	if ((status = ims_srvCat (srvproc, FTS_COMMIT_TRANSACTION)) < IMS_OK)
	{
		(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}

	/* Unlock mutex and lower priority. */
	if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Could not unlock thread mutex. Contact the DBA.");
	}
	(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

	/*
	** Determine the path policy based on granule index.
	*/
	pathPolicy = (FTS_PATH_POLICY *) datasetPolicy->pathPolicy; 

	while (pathPolicy != (FTS_PATH_POLICY *) NULL)
	{
		if ((pathPolicy->start_granule == -1) &&
			(pathPolicy->end_granule == -1))
		{
			break;
		}	

		if ((pathPolicy->start_granule <= granuleSpec->granule_idx) &&
			(pathPolicy->end_granule == -1))
		{
			break;
		}	

		if ((pathPolicy->start_granule <= granuleSpec->granule_idx) &&
			(granuleSpec->granule_idx <= pathPolicy->end_granule))
		{
			break;  /* Found a matching policy path for the granule */
		}
		pathPolicy = pathPolicy->next;
	} 

	/* No match means big policy problems. */
	if (pathPolicy == (FTS_PATH_POLICY *) NULL)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"No path was found for the given granule index %d. Possible installation failure.  Contact the DBA.",
		granuleSpec->granule_idx);
		return (IMS_FATAL);
    }

	/* Set pointer in procDesc to path found. */
	procDesc->pathPolicy = pathPolicy;  

	/*
	** Set rollbackOnExit flag.
	*/
	procDesc->rollbackOnExit = 1;

	/*
	** We are now ready to accept the granule.
	*/
	return (IMS_OK);
}

/******************************************************************************
**
** ims_replaceEventEnd ()
**
******************************************************************************/

int ims_replaceEventEnd (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	FTS_GRANULE_DESC *granuleDesc;
	FTS_CAT_STRUCT *catReq;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_CAT_STATESPEC *stateSpec;
	FTS_BUFFERSPEC_TYPE bufferSpec;
	FTS_SENSOR_POLICY *sensorPolicy;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_PATH_POLICY *pathPolicy;
	FTS_FILE_POLICY *filePolicy;
	FTS_FORMAT_POLICY *formatPolicy;
	FTS_CLIENT_REQUEST *clientReq;
	FTS_FILE_LIST *fileList;
	IMS_MSG_STRUCT *msgStruct;
	CS_SERVERMSG srvMsg;
	char auxBuffer[IMS_COL255_LEN+1];
	int pid;     		/* process id number. */
	int lastMsg; 		/* last message flag from the auxProcess. */
	int i;
	int status;
	int savefd0;		/* Storage for saving file descriptors.*/
	int savefd1;
	IMS_HASH_ENTRY item;	/* ims_hash items. */
	IMS_HASH_ENTRY *hashEntry;
	char fullPathName[IMS_PATH_LEN+IMS_COL60_LEN+1];
	char fileName[IMS_COL60_LEN+1];
	int count; 
	char msg[CS_MAX_MSG];
	fd_set fds;
	int max_descriptors;
	struct timeval timeout;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_replaceEventEnd.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_replaceEventEnd.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}
	
	granuleDesc = &(procDesc->granuleDesc);
	catReq = &(procDesc->catReq);
	granuleSpec = catReq->granuleSpec;
	stateSpec = &(catReq->stateSpec);
	sensorPolicy = procDesc->sensorPolicy;
	datasetPolicy = procDesc->datasetPolicy;
	clientReq = &(procDesc->clientReq);
	msgStruct = procDesc->msgStruct;

	/*
	** Change the status to indicate that the new granule has been transfered.
	*/
	stateSpec->status = IMS_REPLACE_TRANSFER_COMPLETE;

	/*
	** Start critical section to avoid deadlock.
	*/
	(void) ims_lockMutexWait (procDesc->threadMutex);
	(void) ims_changePriority (srvproc, FTS_PRIORITY_DELTA);

	if ((status = ims_srvCat (srvproc, FTS_CHANGE_GRANULE_STATE)) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Changing granule record status failed. Contact the DBA.");

		/* Unlock thread mutex and lower priority. */
		(void) ims_unlockMutex (procDesc->threadMutex);
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (IMS_FATAL);
	}

	/*
	**  Update file sizes transfered in the granules table.
	*/

	if (updateFileSizes(srvproc, procDesc) < IMS_OK)
	{

		(void) ims_msg (msgStruct, IMS_FATAL,
			"Could not update file sizes in granules table.");
		return(IMS_FATAL);		
	}


	/* Unlock thread mutex and lower priority. */
	(void) ims_unlockMutex (procDesc->threadMutex);
	(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

	/*
	** Reset status value.
	*/
	status = IMS_OK;

	/*
	** File transfer into CDB storage is complete at this point.
	** Start any auxiliary process if required as the last step.
	*/
	if (procDesc->datasetPolicy->load_program[0] != '\0')
	{
		/*
		** Yield for other client threads.
		*/
		(CS_VOID) srv_yield ();

		/*
		** Lock the mutex.
		*/
		if (ims_lockMutexWait (procDesc->auxMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_ERROR,
				"Could not lock mutex for auxProcess. Please try again.");
			return (IMS_ERROR);
		}

		/*
		** All the lines below must be collected and placed into a 
		** function for clarity and reduction of errors.
		*/

		/*
		** We must get a duplicate copy of the fileDescriptors 0 and 1
		** before we continue with the auxiliary process execution and
		** communication.  invokeAux redirects filedescriptors 0 and 1
		** for communication between the server and auxiliary process.
		** The saved fileDescriptors will be used to reinstate the 
		** fileDescriptors 0 and 1 to their default stdout and
		** stdin streams.
		*/
		if ((savefd0 = dup (0)) == -1)
		{
			if (ims_unlockMutex (procDesc->auxMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock mutex for auxProcess. Contact the DBA.");
			}
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Dup system call failed for file descriptor '0'. Contact the DBA.");
		    procDesc->rollbackOnExit = 1;
			return (IMS_FATAL);
		}

		if ((savefd1 = dup (1)) == -1)
		{
			(void) close (savefd0);

			if (ims_unlockMutex (procDesc->auxMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock mutex for auxProcess. Contact the DBA.");
			}
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Dup system call failed for file descriptor '1'. Contact the DBA.");
		    procDesc->rollbackOnExit = 1;
			return (IMS_FATAL);
		}

		if (invokeAux (procDesc->datasetPolicy, &pid, msgStruct) < IMS_OK)
		{
			(void) sprintf (msg,
				"Could not invoke process '%s'.  Possible installation failure.  Contact the DBA.", 
				procDesc->datasetPolicy->load_program);
			(void) ims_msg (msgStruct, IMS_FATAL, msg);

			/*
			** Reset the fileDescriptors befor returning..
			*/
			(void) close (0);
			(void) close (1);

			if ((dup (savefd0) == -1) || (dup (savefd1) == -1))
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Dup system call failed for saved file descriptors.");
			}
			(void) close (savefd0);
			(void) close (savefd1);

			if (ims_unlockMutex (procDesc->auxMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock mutex for auxProcess. Contact the DBA.");
			}
		    procDesc->rollbackOnExit = 1;
			return (IMS_FATAL);
		}

		/*
		** Turn off rollbackOnExit flag
		*/
		procDesc->rollbackOnExit = 0;

		/*
		** Start communication with the auxiliary process
		*/
		lastMsg = 0;
		i=0;
		max_descriptors = 1;
		while (! lastMsg)
		{
			i++;

			/*
			** Wait for activity on the stdin pipe.
			*/

			timeout.tv_sec = 0;
			timeout.tv_usec = 50000;
			FD_SET(0, &fds);


			while ((!FD_ISSET(0, &fds)) && (getpgid(pid) > 0))
			{
				srv_yield();
				srv_yield();
				srv_yield();

				select(max_descriptors + 1, &fds, (fd_set *) 0, (fd_set *) 0,
					&timeout);
			}


			if (read (0, auxBuffer, IMS_COL255_LEN+1) == 0)
			{
				/*
				** Premature EOF detected, AUXP is terminated
				*/

				/*
				** Reset the fileDescriptors before returning..
				*/
				(void) close (0);
				(void) close (1);
				if ((dup (savefd0) == -1) ||
					(dup (savefd1) == -1))
				{
					(void) ims_msg (msgStruct, IMS_FATAL,
						"Dup system call failed for saved file descriptors.");
				}
				(void) close (savefd0);
				(void) close (savefd1);

				if (i > 1)
				{
					(void) sprintf (msg, 
						"Premature process termination: %s.", 
						procDesc->datasetPolicy->load_program);
					(void) ims_msg (msgStruct, IMS_FATAL,
						msg);
				}
				else
				{
					(void) sprintf (msg,
				  		"Could not run auxilliary process '%s'.\nInstallation suspect. Contact the DBA.",
				  		procDesc->datasetPolicy->load_program);
					(void) ims_msg (msgStruct, IMS_FATAL,
						msg);
				}

				if (ims_unlockMutex (procDesc->auxMutex) <
					IMS_OK)
				{
					(void) ims_msg (msgStruct, IMS_FATAL,
						"Could not unlock mutex for auxProcess. Contact the DBA.");
				}
		        procDesc->rollbackOnExit = 1;
				return (IMS_FATAL);
			}

			/*
			** Decode message from aux process and draft replies if
			** necessery, or process messages.
			*/
			decodeAuxProcMessage (auxBuffer, &bufferSpec, procDesc);

			/*
			** Three buffer types are possible: CMD(1), MSG(2),
			** OTHER.
			*/

			switch (bufferSpec.kind)
			{
			case 1: /* send reply */
				write (1, bufferSpec.reply, 
					 strlen (bufferSpec.reply) + 1);

				if (bufferSpec.LCF)
				{
					/*
					** Heuristic wait until the auxProcess
					** is complete.
					*/
					count = (granuleDesc->dataBytes /
						1000) + 1;
					for (i = 0; i < count; i++)
					{
						(CS_VOID) srv_yield (); 
						(CS_VOID) srv_yield ();
						(CS_VOID) srv_yield ();
					}
				}
				break;

			case 2: /* put message in the msgQueue */
				if (bufferSpec.LMF)
				{
					lastMsg = 1; /*last message*/
				}
				else if ((status = bufferSpec.severity) < 
					IMS_OK)
				{

					(void) ims_msg (msgStruct,
						bufferSpec.severity,
						 bufferSpec.reply);
				}
				break;

			default:  /* Just ignore it */
				break;
			}
		}

		/*
		** Reset the fileDescriptors before returning..
		*/
		(void) close (0);
		(void) close (1);
		if ((dup (savefd0) == -1) || (dup (savefd1) == -1))
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Dup system call failed for saved file descriptors.");
		}
		(void) close (savefd0);
		(void) close (savefd1);
	
		/*
		** Now wait for the child to terminate.
		*/
#if WAIT_FOR_CHILD
		/*
		** Issue: Why wait for child to terminate.  Maybe the child wants
		** to do some independent processing.  Why block FTS?????
		** Once the pipe closed, they should be independent....
		*/
		if (ims_waitForChild (msgStruct, pid) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_ERROR,
				"Auxiliary Process, '%s', did not terminate normally.",
				datasetPolicy->load_program);

			if (ims_unlockMutex (procDesc->auxMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock mutex for auxProcess. Contact the DBA.");
			}
		    procDesc->rollbackOnExit = 1;
			return (IMS_FATAL);
		}
#endif

		if (ims_unlockMutex (procDesc->auxMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock mutex for auxProcess. Contact the DBA.");
			return (IMS_FATAL);
		}
	}

	/*
	** Allow WARNINGS to continue.
	*/

	if (status < IMS_WARNING) 
	{
		/*
		** Set rollbackOnExit flag.
		** auxProcess failed
		*/
		procDesc->rollbackOnExit = 1;
		return (status);
	}

	/*
	** Turn off rollback on exit.
	*/
	procDesc->rollbackOnExit = 0;

	/*
	** Delete the old granule and its associated record in the catalog.
	** oldFileIdx is 0 when file does not exist in storage.
	*/
	if (granuleSpec->oldGranule_idx > 0)
	{
		/*
		** Delete the files associated with the granule being replaced.
		*/
		while (granuleSpec != (FTS_CAT_GRANULESPEC *) NULL)
		{
			/*
			** Determine the path policy based on granule index.
			*/
			pathPolicy = (FTS_PATH_POLICY *) datasetPolicy->pathPolicy; 

			while (pathPolicy != (FTS_PATH_POLICY *) NULL)
			{
				if ((pathPolicy->start_granule == -1) &&
					(pathPolicy->end_granule == -1))
				{
					break;
				}	

				if ((pathPolicy->start_granule <= granuleSpec->granule_idx) &&
					(pathPolicy->end_granule == -1))
				{
					break;
				}	

				if ((pathPolicy->start_granule <= granuleSpec->oldGranule_idx) &&
					(granuleSpec->oldGranule_idx <= pathPolicy->end_granule))
				{
					break;  /* Found a matching policy path for the granule */
				}
				pathPolicy = pathPolicy->next;
			} 

			/* No match means big policy problems. */
			if (pathPolicy == (FTS_PATH_POLICY *) NULL)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"No path was found for the given granule index %d. Possible installation failure.  Contact the DBA.",
				granuleSpec->oldGranule_idx);
				return (IMS_FATAL);
    		}


			/*
			** Determine the file policy based on format.
			*/
			filePolicy = datasetPolicy->filePolicy;
			while (filePolicy != (FTS_FILE_POLICY *) NULL)
			{
				if (strcmp (filePolicy->format,
					granuleSpec->format) == 0)
				{
					break;
				}
				filePolicy = filePolicy->next;
			}

			/* No match means big policy problems. */
			if (filePolicy == (FTS_FILE_POLICY *) NULL)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"The format returned by the catalog, %s, does not match a format in the server's cache policy. Possible installation failure.  Contact the DBA.",
					granuleSpec->format);
				return (IMS_FATAL);
			}

			formatPolicy = filePolicy->formatPolicy;
			while (formatPolicy != (FTS_FORMAT_POLICY *) NULL)
			{
				/*
				** Construct the file name as it appears on disk.
				** This is to remove the old file.
				*/
				if (granuleSpec->version > 0)
				{
					(void) sprintf (fileName, "%s.%s.%d",
						 granuleSpec->name, formatPolicy->extension,
						 granuleSpec->version);

					ims_concatFilePath (fullPathName,
						pathPolicy->path, fileName);
				}
				else
				{
					(void) sprintf (fileName, "%s.%s",
						granuleSpec->name, formatPolicy->extension);

					ims_concatFilePath (fullPathName,
						pathPolicy->path, fileName);
				}

				/*
				** Remove the old file from the repository.
				*/
				if (unlink (fullPathName) == -1)
				{
					(void) ims_msg (msgStruct, IMS_FATAL,
						"Could not delete file '%s' for '%s, %s, %s' from storage. %s.  Possible policy conflict or installation failure. Contact the DBA.",
						ims_extractFileName (fullPathName),
						sensorPolicy->platform, sensorPolicy->sensor,
						datasetPolicy->dataset, strerror (errno));
					return (IMS_FATAL);
				}
				formatPolicy = formatPolicy->next;
			}
			granuleSpec = granuleSpec->next;
		}

		/*
		** Lock the mutex.
		*/
		if (ims_lockMutexWait (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_ERROR,
				"Could not lock thread mutex; Server busy. Please try again.");
			return (IMS_ERROR);
		}

		/*
		** Raise priority.
		*/
		if (ims_changePriority (srvproc, FTS_PRIORITY_DELTA) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not raise client thread priority. Contact the DBA.");
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			return (IMS_FATAL);
		}

		/*
		** Change the granule_idx to the old granule idx
		*/

		if ((status = ims_srvCat (srvproc, FTS_UPDATE_GRANULE_INDEX)) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not update name in granules_x table. Contact the DBA.");

			/* Unlock thread mutex and lower priority. */
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

			return (status);
		}


		/*
		** Delete the old granule record in the granules_x table.
		*/
		if ((status = ims_srvCat (srvproc, FTS_DELETE_OLD_GRANULE_RECORD))
			< IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL, 
				"Could not delete granule record from the granules_x table. Contact the DBA.");

			/* Unlock thread mutex and lower priority. */
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

			return (status);
		}

		/*
		** clear the name stamp from the database
		*/

		if ((status = ims_srvCat (srvproc, FTS_CLEAR_NAME_STAMP)) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not update name in granules_x table. Contact the DBA.");

			/* Unlock thread mutex and lower priority. */
			if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not unlock thread mutex. Contact the DBA.");
			}
			(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

			return (status);
		}


		/*
		** Unlock mutex and lower priority.
		*/
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		/*
		** Rename the new files from their temporary names
		** to their original names.
		*/
		granuleSpec = catReq->granuleSpec;
		while (granuleSpec != (FTS_CAT_GRANULESPEC *) NULL)
		{
			/*
			** Determine the file policy based on format.
			*/
			filePolicy = datasetPolicy->filePolicy;
			while (filePolicy != (FTS_FILE_POLICY *) NULL)
			{
				/*
				** We compare with the client provided format because
				** it may be different than the granule we are replacing.
				*/
				if (strcmp (filePolicy->format,
					clientReq->format) == 0)
				{
					break;
				}
				filePolicy = filePolicy->next;
			}

			/* No match means big policy problems. */
			if (filePolicy == (FTS_FILE_POLICY *) NULL)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"The format requested by the client, %s, does not match a format in the server's cache policy. Possible installation failure.  Contact the DBA.",
					clientReq->format);
				return (IMS_FATAL);
			}

			formatPolicy = filePolicy->formatPolicy;
			while (formatPolicy != (FTS_FORMAT_POLICY *) NULL)
			{
				/*
				** Construct the original file name.
				*/
				if (granuleSpec->version > 0)
				{
					(void) sprintf (fileName, "%s.%s.%d",
						 granuleSpec->name, formatPolicy->extension,
						 granuleSpec->version);

					ims_concatFilePath (fullPathName,
						procDesc->pathPolicy->path, fileName);
				}
				else
				{
					(void) sprintf (fileName, "%s.%s",
						granuleSpec->name, formatPolicy->extension);

					ims_concatFilePath (fullPathName,
						procDesc->pathPolicy->path, fileName);
				}

				/*
				** Make sure we are renaming the correct file.
				*/
				fileList = granuleDesc->fileList;
				while (fileList != (FTS_FILE_LIST *) NULL)
				{
					if (strcmp (formatPolicy->extension,
						fileList->extension) == 0)
					{
						break;
					}
					fileList = fileList->next;
				}

				/* If our extensions did not match, we have big problems. */
				if (fileList == (FTS_FILE_LIST *) NULL)
				{
					(void) ims_msg (msgStruct, IMS_FATAL,
						"A mismatch occured when comparing extensions from the policy with extensions from the file list prior to renaming the files.");
					return (IMS_FATAL);
				}

				/*
				** Check to see if we are going to overwrite an existing
				** file. This should not be the case because we deleted
				** the files associated with the granule being replaced.
				*/
				if (access (fullPathName, F_OK) == 0)
				{
					(void) ims_msg (msgStruct, IMS_FATAL,
						"File '%s' exists in the repository and should have already been deleted in the replace process. Contact the DBA.",
						fullPathName);
					return (IMS_FATAL);
				}

				/*
				** Rename the file in the repository.
				*/
				if (rename (fileList->fileToWrite, fullPathName) == -1)
				{
					(void) ims_msg (msgStruct, IMS_FATAL,
						"Could not rename file '%s' to '%s'. %s. Contact the DBA.",
						fileList->fileToWrite, fullPathName,
						strerror (errno));
					return (IMS_FATAL);
				}
				formatPolicy = formatPolicy->next;
			}
			granuleSpec = granuleSpec->next;
		}
	}

	/*
	** Change the status of the new record to available.
	*/
	stateSpec->status = IMS_AVAILABLE;

	/* Start critical section to avoid deadlock. */
	(void) ims_lockMutexWait (procDesc->threadMutex);
	(void) ims_changePriority (srvproc, FTS_PRIORITY_DELTA);

	if ((status = ims_srvCat (srvproc, FTS_CHANGE_GRANULE_STATE)) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Changing granule record status failed. Contact the the DBA.");

		/* Unlock thread mutex and lower priority. */
		(void) ims_unlockMutex (procDesc->threadMutex);
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (IMS_FATAL);
	}

	/* Unlock thread mutex and lower priority. */
	(void) ims_unlockMutex (procDesc->threadMutex);
	(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

	return (IMS_OK);
}

/******************************************************************************
**
** ims_validateClientLogin ()
**
******************************************************************************/

int ims_validateClientLogin (SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	FTS_CAT_USERSPEC *userSpec;
	FTS_CAT_USERCAP *userCap;
	CS_SERVERMSG srvMsg;
	char msg[CS_MAX_MSG];

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_validateClientLogin.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_validateClientLogin.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	userSpec = &(procDesc->catReq.userSpec);
	userCap = &(procDesc->catReq.userCap);

	/*
	** Set specification to open a database connection for the server.  Check
	** against username, password and account ID.   
	*/
	userSpec->dbUserName = procDesc->surrogate.login;
	userSpec->dbPassword = procDesc->surrogate.password;
	userSpec->program = procDesc->ftsProgramName;
	userSpec->server = procDesc->catSrvName;
	userSpec->database = procDesc->catDbName;
	userCap->accountId = procDesc->clientReq.accountId;


	/*
	** Open database connection. 
	*/
	if (ims_srvCat (srvproc, FTS_OPEN_CONNECTION) < IMS_OK)
	{
		return (IMS_ERROR);
	}


	/*
	** Validate that user is found in user catalog database.
	*/
	if (ims_srvCat (srvproc, FTS_CHECK_LOGIN) < IMS_OK)
	{
	    (void) ims_srvCat (srvproc, FTS_CLOSE_CONNECTION); 
		return (IMS_ERROR);
	}


	if (ims_srvCat (srvproc, FTS_CLOSE_CONNECTION) < IMS_OK)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL, 
			EFTS_CLOSE_CONNECTION);
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** ims_validateMetadata ()
**
******************************************************************************/

int ims_validateMetadata (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	FTS_GRANULE_DESC *granuleDesc;
	FTS_FILE_LIST *currFile;
	IMS_MSG_STRUCT *msgStruct;
	CS_SERVERMSG srvMsg;
	int status;
	char msg[CS_MAX_MSG];
	char syslogFileFullPath[IMS_COL255_LEN+IMS_COL60_LEN+1];
	int threadId;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_validateMetadata.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_validateMetadata.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	granuleDesc = &(procDesc->granuleDesc);
	currFile = granuleDesc->currFile;
	msgStruct = procDesc->msgStruct;

	/*
	** Get the thread id 
	*/

    (CS_VOID) srv_thread_props (srvproc, CS_GET, SRV_T_SPID,
			(CS_VOID *) &threadId, (CS_INT) sizeof (threadId),
			(CS_INT *) NULL);

	/*
	** Lock out other threads so that this thread may use the
	** syslog file since syslog can not handle multiple 
	** syslogs for a single process.
	*/

	/*
	** Lock the mutex.
	*/
	if (ims_lockMutexWait (procDesc->threadMutex) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_ERROR,
			"Could not lock thread mutex; Server busy. Please try again.");
		return (IMS_ERROR);
	}

	/*
	** Raise priority.
	*/
	if (ims_changePriority (srvproc, FTS_PRIORITY_DELTA) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Could not raise client thread priority. Contact the DBA.");

		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		return (IMS_FATAL);
	}


	/* 
	** Open the syslog file to write the ODL errors to
	*/

	(void) sprintf (syslogFileFullPath, "%s/syslog_ftsSrv.%d",
			procDesc->logFileDir, threadId);

	if (IK_NameSyslog (syslogFileFullPath) < 0)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
				"Opening IK syslog '%s' failed.", syslogFileFullPath);

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);
		return(IMS_FATAL);
	}


	/*
	** Parse the metadata file using ODL library function calls.
	*/
	if ((status = parseODL (currFile->fullPathName, &procDesc->keywordList,
		msgStruct)) < IMS_OK)
	{
		IK_CloseSyslog();

		(void) ims_msgIKSyslog(msgStruct,  syslogFileFullPath);
		unlink(syslogFileFullPath);

		/* Unlock mutex and lower priority. */
		if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not unlock thread mutex. Contact the DBA.");
		}
		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (status);
	}

	/*
	** Close the syslog since we only need if for ODL.
	*/

	IK_CloseSyslog();

	/*
	** Remove the syslog file since we extracted the data.
	*/

	unlink(syslogFileFullPath);


	/*
	** Compare keywords extracted from the metadata file with the
	** keyword policy for the current dataset.
	*/
	if ((status = validateKeywordList (procDesc)) < IMS_OK)
	{
		(void) ims_unlockMutex (procDesc->threadMutex);

		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);
		return (status);
	}

	/*
	** Update keyword-value pairs, extracted from the Metadata file, in the 
	** granules_x table.
	*/
	if (ims_srvCat (srvproc, FTS_BEGIN_TRANSACTION) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Begin transaction for addGranuleMetadata failed. Contact the DBA.");
		(void) ims_unlockMutex (procDesc->threadMutex);

		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (IMS_FATAL);
	}

	if ((status = ims_srvCat (srvproc, FTS_ADD_GRANULE_METADATA)) < IMS_OK)
	{
		(void) ims_msg (msgStruct, status,
			"Could not set keyword values. See previous messages for additional information.");
		(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

		(void) ims_unlockMutex (procDesc->threadMutex);

		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);
		return (status);
	}

	if (ims_srvCat (srvproc, FTS_COMMIT_TRANSACTION) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Commit transaction for addGranuleKeywords failed. Contact the DBA.");
		(void) ims_srvCat (srvproc, FTS_ROLLBACK_TRANSACTION);

		(void) ims_unlockMutex (procDesc->threadMutex);

		(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);

		return (IMS_FATAL);
	}

	/*
	** End of critical section.  Unblock the other threads.
	*/

	if (ims_unlockMutex (procDesc->threadMutex) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Could not unlock thread mutex. Contact the DBA.");
	}

	(void) ims_changePriority (srvproc, -1 * FTS_PRIORITY_DELTA);


	return (IMS_OK);
}

/******************************************************************************
**
** invokeAux ()
**
** Invoke the auxiliary process.
**
******************************************************************************/

int invokeAux (
	FTS_DATASET_POLICY *datasetPolicy,
	int *pid,
	IMS_MSG_STRUCT *msgStruct)
{
	char fullPath[256];
	char srcPath[256];

	if (getenv("IMS_EXEC_PATH") != NULL)
	{
		strcpy(srcPath, getenv("IMS_EXEC_PATH"));
	}
	else
	{
		strcpy(srcPath, ".");
	}

	sprintf(fullPath, "%s/%s", srcPath, datasetPolicy->load_program);
	

	if ((*pid = ims_startChild2 (msgStruct, fullPath, fullPath,
		"DUPLEX_PIPE", (char *) NULL)) < 0)
	{
		return (IMS_FATAL);
	}

#ifdef DEBUG
	fprintf(stderr, "Starting Aux Process %s pid %d\n", 
			datasetPolicy->load_program, *pid);
#endif

	return (IMS_OK);
}

/******************************************************************************
**
** decodeAuxProcMessage ()
**
** Routine for recognizing and parsing the communication messages between ftr
** and auxProcess over a duplex pipe.
**
******************************************************************************/

static void decodeAuxProcMessage (
	char *buffer,
	FTS_BUFFERSPEC_TYPE *bufferSpec,
	FTS_PROC_DESC *procDesc)
{
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_CAT_STRUCT *catReq;
	FTS_CAT_USERSPEC *userSpec;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_SENSOR_POLICY *sensorPolicy;
	FTS_CLIENT_REQUEST *clientReq;

	/*
	** Local variables are defined here
	*/
	char *temp;
	char *t,*t1,*t2,*t3,*t4;
	int length;
	char fileName[IMS_COL80_LEN+1];
	char *fileNamePtr;



    catReq = &(procDesc->catReq);
	userSpec = &(procDesc->catReq.userSpec);
	datasetPolicy   = procDesc->datasetPolicy;
	sensorPolicy  = procDesc->sensorPolicy;
	granuleSpec = catReq->granuleSpec;
	clientReq = &(procDesc->clientReq);


	/*
	** Concatenate the complete file name.
	*/

	/*
	** Probably should not hard-code D here for R2.
	*/

	if (granuleSpec->version > 0)
	{
		(void) sprintf (fileName, "%s.D.%d",
			granuleSpec->name,  granuleSpec->version);
	}
	else
	{
		(void) sprintf (fileName, "%s.D",
			granuleSpec->name);
	}

	/*
	** Append name stamp if there is one.
	*/

	if (granuleSpec->name_stamp[0] !='\0')
	{
		strcat(fileName, granuleSpec->name_stamp);
	}

	fileNamePtr = fileName;


	length = strlen (buffer);
	temp = buffer;
	if((strncmp (buffer, "CMD:", 4) == 0) && (length > 5) &&
		((t1 = strchr (buffer, ':')) != (char *) NULL) &&
		((t2 = strchr (++t1, ':')) != (char *) NULL))
	{
		/*
		** Looks like a CMD message
		*/
		t2++;
		bufferSpec->kind = 1;
		t = t1;
		t--;
		t[0] = '\0';
		t = t2;
		t--;
		t[0] = '\0';

		if (t1[0] == '\0') 
		{
			bufferSpec->LCF = 0;
		}
		else 
		{
			(void) sscanf (t1, "%d", &(bufferSpec->LCF));
		}

		temp = t2;

		if (strncmp (temp, "dbUserName", 10)  == 0)
		{
			(void) sprintf (bufferSpec->reply, "%s\n",
				userSpec->dbUserName);
		}
		else if (strncmp (temp, "dbPassword", 10) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%s\n",
				userSpec->dbPassword);
		}
		else if (strncmp (temp, "server", 6) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%s\n",
				userSpec->server);
		}
		else if (strncmp (temp, "dbName", 6) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%s\n",
				userSpec->database);
		}
		else if (strncmp (temp, "fileName", 8) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%s\n", fileNamePtr);
		}
		else if (strncmp (temp, "repositoryDir", 13) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%s\n",
				procDesc->pathPolicy);
		}
		else if (strncmp (temp, "granuleName", 11) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%s\n", 
				datasetPolicy->granules_table);
		}
		else if (strncmp (temp, "platform", 8) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%s\n", 
				clientReq->platform);
		}
		else if (strncmp (temp, "sensor", 6) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%s\n", 
				clientReq->sensor);
		}
		else if (strncmp (temp, "datasetIdx", 10) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%d\n", 
				datasetPolicy->dataset_idx);
		}
		else if (strncmp (temp, "dataset", 7) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%s\n", 
				clientReq->dataset);
		}
		else if (strncmp (temp, "version", 7) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%d\n", 
				granuleSpec->version);
		}
		else if (strncmp(temp, "granuleIdx", 10) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%d\n", 
				granuleSpec->granule_idx);

		}
		else if (strncmp(temp, "accountId", 9) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%s\n", 
				clientReq->accountId);

		}
		else if (strncmp(temp, "clientUser", 10) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%s\n", 
				procDesc->client.login);

		}
		else if (strncmp(temp, "clientPassword", 14) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%s\n", 
				procDesc->client.password);

		}
		else 
		{
			(void) sprintf (bufferSpec->reply, "\n");
		}
	}
	else
	{
		if ((strncmp (buffer, "MSG:", 4) == 0) && (length > 7) &&
			 (buffer[length-1] == '\n') && 
			 ((t1 = strchr (buffer, ':')) != (char *) NULL) &&
			 ((t2 = strchr (++t1, ':')) != (char *) NULL) &&
			 ((t3 = strchr (++t2, ':')) != (char *) NULL) &&
			 ((t4 = strchr (++t3, ':')) != (char *) NULL) )
		{
			/*
			** Looks like a MSG
			*/
			++t4;
			bufferSpec->kind = 2;

			/*
			** Replace ':' with '\0' in the buffer string.
			*/
			t = t1;
			t--;
			t[0] = '\0';
			t = t2;
			t--;
			t[0] = '\0';
			t = t3;
			t--;
			t[0] = '\0';
			t = t4;
			t--;
			t[0] = '\0';
			buffer[length-1] = '\0';
			length--;
	
			if (t1[0] == '\0')
			{
				bufferSpec->severity = IMS_OK;
			}
			else
			{
				(void) sscanf (t1, "%d",
					&(bufferSpec->severity));
			}

			if (t2[0] == '\0')
			{
				bufferSpec->errorno = 0;
			}
			else
			{
				(void) sscanf (t2, "%d",
					&(bufferSpec->errorno));
			}


			if (t3[0] == '\0')
			{
				bufferSpec->LMF = 0;
			}
			else
			{
				(void) sscanf (t3, "%d", &(bufferSpec->LMF));
			}
			(void) sprintf (bufferSpec->reply, "%s", t4);
		}
		else
		{
			bufferSpec->kind = 0;

			if (buffer[length-1] != '\n')
			{
				buffer[length-1] = '\n';
			}
			(void) strcpy (bufferSpec->reply, buffer);
		}
	}
}

/******************************************************************************
**
** parseODL ()
**
******************************************************************************/

static int parseODL (
	char *fullPathName,
	FTS_KEYWORD_LIST **keywordList,
	IMS_MSG_STRUCT *msgStruct)
{
	FILE *metadataFile;
	FTS_KEYWORD_LIST *currList;
	FTS_KEYWORD_LIST *prevList;
	AGGREGATE rootAggregate;
	AGGREGATE baseAggregate;
	AGGREGATE currAggregate;
	PARAMETER currParameter;
	VALUE currValue;
	int keywordCount;
	unsigned int milliseconds;

	/*
	** Open the Metadata file for read.
	*/
	if ((metadataFile = fopen (fullPathName, "r")) == (FILE *) NULL)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Unable to open Metadata file '%s'; %s",
			fullPathName, strerror (errno));
		return (IMS_FATAL);
	}

	/*
	** Allocate the structure for the root node of the ODL tree.
	*/
	if ((rootAggregate = NewAggregate (NULL, KA_GROUP, "root", NULL)) ==
		(AGGREGATE) NULL)
	{
		(void) fclose (metadataFile);
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Unable to allocate the structure for the root node of the ODL tree.");
		return (IMS_FATAL);
	}

	/*
	** Call the ODL function to read and parse the Metadata file
	** into the ODL tree.
	*/
	if ((ReadLabel (metadataFile, rootAggregate)) == 0)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"An error occured while parsing the Metadata file %s. ",
			ims_extractFileName (fullPathName));
		(void) RemoveAggregate (rootAggregate);
		(void) fclose (metadataFile);
		return (IMS_FATAL);
	}

	/*
	** Process the target aggregate and any sub-aggregates along with the
	** associated parameters and values.
	*/
	if ((baseAggregate = FindAggregate (rootAggregate, "CATALOG_METADATA")) ==
		(AGGREGATE) NULL)
	{
		(void) ims_msg (msgStruct, IMS_ERROR,
			"The '%s' aggregate was not found in the Metadata file %s.",
			"CATALOG_METADATA", ims_extractFileName (fullPathName));
		(void) RemoveAggregate (rootAggregate);
		(void) fclose (metadataFile);
		return (IMS_ERROR);
	}

	keywordCount = 0;
	currAggregate = baseAggregate;
	while (currAggregate != (AGGREGATE) NULL)
	{
		/* Process the parameters for the current aggregate. */
		currParameter = FirstParameter (currAggregate);
		while (currParameter != (PARAMETER) NULL)
		{
			/* We only support one value per parameter. */
			if (currParameter->value_count > 1)
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
					"More than one value found for keyword %s. Multiple values are not allowed for keywords in the Metadata file.",
					currParameter->name);
				(void) RemoveAggregate (rootAggregate);
				(void) fclose (metadataFile);
				return (IMS_ERROR);
			}

			keywordCount++;

			/*
			** Allocate space for the FTS_KEYWORD_LIST structure.
			**
			** lint: pointer cast may result in improper alignment
			** ???? Review this.
			*/
			if ((currList = (FTS_KEYWORD_LIST *) malloc
				((unsigned) sizeof (FTS_KEYWORD_LIST))) ==
				(FTS_KEYWORD_LIST *) NULL)
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Could not allocate memory for FTS_KEYWORD_LIST structure.");
				(void) RemoveAggregate (rootAggregate);
				(void) fclose (metadataFile);
				return (IMS_FATAL);
			}

			/*
			** lint: warning: variable may be used before set: prevList
			** We don't use this variable until the second time through, and
			** by that time it has been set.
			*/

			if (keywordCount == 1)
			{
				*keywordList = currList;
			}
			else
			{
				prevList->next = currList;
			}

			/* Initialize structure members. */
			(void) strcpy (currList->keyword, currParameter->name);
			currList->next = (FTS_KEYWORD_LIST *) NULL;

			/* Process the value for the current parameter. */
			currValue = FirstValue (currParameter);
			while (currValue != (VALUE) NULL)
			{
				/* See if value exceeds maximum length. */
				if (currValue->item.length > IMS_COL255_LEN)
				{
					(void) ims_msg (msgStruct, IMS_ERROR,
						"Value for keyword '%s' exceeded maximum length of %d.",
						currParameter->name, IMS_COL255_LEN);
					(void) RemoveAggregate (rootAggregate);
					(void) fclose (metadataFile);
					return (IMS_ERROR);
				}

				/* Copy the value to the list based on data type. */
				switch (currValue->item.type)
				{
				case TV_INTEGER:
					currList->value_integer =
						currValue->item.value.integer.number;
					currList->data_type = IMS_INT4_TYPE;
					break;

				case TV_REAL:
					currList->value_real =
						currValue->item.value.real.number;
					currList->data_type = IMS_FLOAT8_TYPE;
					break;

				case TV_SYMBOL:
					(void) strcpy (currList->value_string,
						ims_truncStr(currValue->item.value.string));
					currList->data_type = IMS_SYMBOL_TYPE;
					break;

				case TV_STRING:
					(void) strcpy (currList->value_string,
						ims_truncStr(currValue->item.value.string));
					currList->data_type = IMS_STRING_TYPE;
					break;

				case TV_DATE:
					(void) ims_msg (msgStruct, IMS_ERROR,
						"The value for keyword %s, has an unsupported value type, 'DATE'. Please use the 'DATETIME' value format.\n",
						currParameter->name);
					currList->data_type = IMS_NO_DATA_TYPE;
					break;

				case TV_TIME:
					(void) ims_msg (msgStruct, IMS_ERROR,
						"The value for keyword %s, has an unsupported value type, 'TIME'. Please use the 'DATETIME' value format.\n",
						currParameter->name);
					currList->data_type = IMS_NO_DATA_TYPE;
					break;

				case TV_DATE_TIME:

					milliseconds = (unsigned int)
					ims_nano2msecs(currValue->item.value.date_time.nanoseconds);

					(void) sprintf (currList->value_string,
						"%04u-%03uT%02u:%02u:%02u.%03u",
						currValue->item.value.date_time.year,
						currValue->item.value.date_time.doy,
						currValue->item.value.date_time.hours,
						currValue->item.value.date_time.minutes,
						currValue->item.value.date_time.seconds,
						milliseconds);

					currList->data_type = IMS_DATETIME_TYPE;

					/*
					** Validate that the milliseconds are correct range.
					*/

					if ((milliseconds < 0) ||
						(milliseconds > 999))
					{
						(void) ims_msg (msgStruct, IMS_FATAL,
							"The value for keyword %s, has an invalid millisecond value %d.",
							currParameter->name, 
							milliseconds);

						(void) RemoveAggregate (rootAggregate);
						(void) fclose (metadataFile);
						return (IMS_FATAL);
					}
					break;

				default:
					(void) ims_msg (msgStruct, IMS_FATAL,
						"The value for keyword %s, has an invalid value type.",
						currParameter->name);
					(void) RemoveAggregate (rootAggregate);
					(void) fclose (metadataFile);
					return (IMS_FATAL);
				}

				/* We don't support multiple values at this time. */
				/* currValue = NextValue (currValue); */
				currValue = (VALUE) NULL;
			}

			prevList = currList;
			currParameter = NextParameter (currParameter);
		}

		currAggregate = NextSubAggregate (baseAggregate, currAggregate);
	}

	/*
	** Free the entire ODL tree.
	*/
	if ((RemoveAggregate (rootAggregate)) != (AGGREGATE) NULL)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Unable to free the ODL tree structure.");
		return (IMS_FATAL);
	}

	(void) fclose (metadataFile);

	return (IMS_OK);
}

/******************************************************************************
**
** validateKeywordList ()
**
** Keyword validation routine.  This routine validates keyword-value
** pairs extracted from the k-Header portion of the sfdu file.
** Validation policy is set by the keyword policy scheme loaded
** into cache memory.  
**
** Note:  The extracted keyword-value pairs are sorted in an ascending 
**        order based on keyword names before matching against keyword 
**        policies is started.
**
******************************************************************************/

static int validateKeywordList (
	FTS_PROC_DESC *procDesc)
{
	FTS_KEYWORD_POLICY *currValid;
	FTS_KEYWORD_LIST *keywordList;
	FTS_KEYWORD_LIST *currKey;
	FTS_KEYWORD_LIST *prevKey;
	IMS_MSG_STRUCT *msgStruct;
	int status;
	int errFlag1;
	int errFlag2;
	int matchFlag;

	currValid = procDesc->datasetPolicy->keywordPolicy;
	msgStruct = procDesc->msgStruct;

	/*
	** currValid is already sorted acording to keyword name.
	** We sort the keyword list to simplify keyword matching 
	** during vaidation search.
	*/
	keywordList = sortKeywordList (procDesc->keywordList);
	procDesc->keywordList = currKey = keywordList;
	prevKey = (FTS_KEYWORD_LIST *) NULL;

	/*
	** Validate the list of keywords extracted from the k-Header portion
	** of the SFDU file against the keyword policy scheme.
	*/

	/*
	** Initialize flags
	*/
	errFlag1 = 0;
	matchFlag = 0; 

	while ((currKey != (FTS_KEYWORD_LIST *) NULL) &&
		(currValid != (FTS_KEYWORD_POLICY *) NULL))
	{
		/* 
		** Match currKey against currValid.
		*/
		if ((status = strcmp (currKey->keyword, currValid->keyword))
			== 0)
		{
			/* We have a match. */
			matchFlag = 1;

			/*
			** Compare the data type of the value with the
			** data type expected.
			*/
			if (currKey->data_type != currValid->data_type)
			{
				/* Similar datatypes for integer and float are allowed. */
				switch (currKey->data_type)
				{
					case IMS_INT1_TYPE:
					case IMS_INT2_TYPE:
					case IMS_INT4_TYPE:
						if ((currValid->data_type != IMS_INT4_TYPE) &&
							(currValid->data_type != IMS_INT2_TYPE) &&
							(currValid->data_type != IMS_INT1_TYPE))
						{
							errFlag1 = 1;
							(void) ims_msg (msgStruct, IMS_ERROR,
								"Integer data type for keyword '%s' does not match the keyword policy.",
								currKey->keyword);
						}
						break;

					case IMS_FLOAT4_TYPE:
					case IMS_FLOAT8_TYPE:
						if ((currValid->data_type != IMS_FLOAT8_TYPE) &&
							(currValid->data_type != IMS_FLOAT4_TYPE))
						{
							errFlag1 = 1;
							(void) ims_msg (msgStruct, IMS_ERROR,
								"Float data type for keyword '%s' does not match the keyword policy.",
								currKey->keyword);
						}
						break;

					case IMS_CHAR_TYPE:
					case IMS_SYMBOL_TYPE:
					case IMS_STRING_TYPE:
					case IMS_DOYTIME_TYPE:
					case IMS_DATETIME_TYPE:
						if ((currValid->data_type != IMS_CHAR_TYPE) &&
							(currValid->data_type != IMS_SYMBOL_TYPE) &&
							(currValid->data_type != IMS_STRING_TYPE) &&
							(currValid->data_type != IMS_DATETIME_TYPE) &&
							(currValid->data_type != IMS_DOYTIME_TYPE))
						{
							errFlag1 = 1;
							(void) ims_msg (msgStruct, IMS_ERROR,
								"Character data type for keyword '%s' does not match the keyword policy.",
								currKey->keyword);
						}
						break;

					default:
						errFlag1 = 1;
						(void) ims_msg (msgStruct, IMS_ERROR,
							"The data type for keyword '%s' does not match the keyword policy.",
							currKey->keyword);
				}
			}

			/* Copy the policy values into our current list. */
			currKey->keyword_idx = currValid->keyword_idx;
			currKey->data_type = currValid->data_type;
			currKey->max_len = currValid->max_len;
			currKey->min_val = currValid->min_val;
			currKey->max_val = currValid->max_val;
			currKey->significance = currValid->significance;
			currKey->query_type = currValid->query_type;
			currKey->keywordValue = currValid->keywordValue;

			/*
			** Detect duplicate keywords.
			*/
			if (prevKey != (FTS_KEYWORD_LIST *) NULL)
			{
				if (prevKey->keyword_idx == currKey->keyword_idx)
				{
					(void) ims_msg (msgStruct, IMS_INFO,
						"Keyword '%s' is duplicated; duplicate value will be ignored.",
						currKey->keyword);

					/* Duplicate keyword values are ignored. */
					currKey->significance = IMS_OPTIONAL_NOT_INDEXED;
				}
			}
			prevKey = currKey;
			currKey = currKey->next;
		}
		else if (status > 0)
		{
			if ( !matchFlag &&
				((currValid->significance == IMS_MANDATORY_INDEXED) ||
				(currValid->significance == IMS_MANDATORY_NOT_INDEXED)))
			{
				errFlag1 = 1;
				(void) ims_msg (msgStruct, IMS_ERROR,
					"Keyword '%s' is mandatory but not present.",
					currValid->keyword);
			}
			matchFlag = 0; /* reset matchFlag */
			currValid = currValid->next;
		}
		else
		{
			matchFlag = 0; /* reset matchFlag */
			errFlag1 = 1;
			(void) ims_msg (msgStruct, IMS_ERROR,
				"Keyword '%s' is present but not supported.",
				currKey->keyword);
			currKey = currKey->next;
		}
	}

	/*
	** Check the boundry conditions.
	*/
	errFlag2 = 0;
	if ((currKey == (FTS_KEYWORD_LIST *) NULL) &&
		(currValid != (FTS_KEYWORD_POLICY *) NULL))
	{
		/*
		** If a match is already made for currValid. Go to the
		** next currValid node.
		*/
		if (matchFlag == 1)
		{
			currValid = currValid->next;
		}

		/*
		** Check the remaining keywords defined in the policy for 
		** significance.
		*/
		while (currValid != (FTS_KEYWORD_POLICY *) NULL)
		{
			if ((currValid->significance == IMS_MANDATORY_INDEXED) ||
				(currValid->significance == IMS_MANDATORY_NOT_INDEXED))
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
					"Keyword '%s' is mandatory but not present.",
					currValid->keyword);
				errFlag2 = 1;
			}
			currValid = currValid->next;
		}

		if (errFlag1 || errFlag2)
		{
			return (IMS_ERROR);
		}

		return (IMS_OK);
	}

	if ((currKey != (FTS_KEYWORD_LIST *) NULL) &&
		(currValid == (FTS_KEYWORD_POLICY *) NULL))
	{
		while (currKey != (FTS_KEYWORD_LIST *) NULL)
		{
			(void) ims_msg (msgStruct, IMS_ERROR,
				"Keyword '%s' is present but not supported.",
				currKey->keyword);
			currKey = currKey->next;
		}
		return (IMS_ERROR);
	}
	
	if (errFlag1)
	{
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** sortKeywordList ()
**
** Sort the list in an ascending order.  Bubble sort algorithm is used.
**
******************************************************************************/

static FTS_KEYWORD_LIST *sortKeywordList (
	FTS_KEYWORD_LIST *keywordList)
{
	FTS_KEYWORD_LIST *currPtr;
	FTS_KEYWORD_LIST *prevPtr;
	FTS_KEYWORD_LIST *nextPtr;
	FTS_KEYWORD_LIST *topPtr;
	int flipped;          /* Flag indicating whether a flip has occured. */

	/*
	** Check whether the list provided is a null list. 
	*/
	if (keywordList == (FTS_KEYWORD_LIST *) NULL)
	{
		return (keywordList);
	}

	topPtr = keywordList;
	flipped = IMS_TRUE;
	while (flipped == IMS_TRUE)
	{
		flipped = IMS_FALSE;

		/* Initialize temporary pointers. */
		prevPtr = (FTS_KEYWORD_LIST *) NULL;
		currPtr = topPtr;
		nextPtr = topPtr->next;

		while ((nextPtr != (FTS_KEYWORD_LIST *) NULL) &&
			(currPtr != (FTS_KEYWORD_LIST *) NULL))
		{
			/* Match currPtr with nextPtr. */
			if (strcmp (currPtr->keyword, nextPtr->keyword) > 0)
			{
				flipped = IMS_TRUE;

				/* Flip the positions of currPtr and nextPtr. */
				if (prevPtr != (FTS_KEYWORD_LIST *) NULL) 
				{
					prevPtr->next = nextPtr;
				}
				else
				{
					/*
					** We must update the topPtr to point
					** to the first element of the list.
					*/
					topPtr = nextPtr;
				}
				currPtr->next = nextPtr->next;
				nextPtr->next = currPtr;
				
				prevPtr = nextPtr;
				nextPtr = currPtr->next;
			}
			else
			{
				prevPtr = currPtr;
				currPtr = nextPtr;
				nextPtr = nextPtr->next;
			}
		}
	}
	return (topPtr);
}


/******************************************************************************
**
** ims_checkServerFileTypes ()
**
**
******************************************************************************/

IMS_FILE_TYPES *ims_checkServerFileTypes (
	SRV_PROC *srvproc)
{
	FTS_PROC_DESC *procDesc;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_CAT_USERSPEC *userSpec;
	FTS_CAT_STRUCT *catReq;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_SENSOR_POLICY *sensorPolicy;
	FTS_CLIENT_REQUEST *clientReq;
	FTS_PATH_POLICY *pathPolicy;
	IMS_MSG_STRUCT *msgStruct;
	FTS_CAT_USERCAP *userCap;
	CS_SERVERMSG srvMsg;
	char msg[CS_MAX_MSG];
	int status;

	/*
	** Initialize the CS_SERVERMSG structure.
	*/
	srv_bzero ((CS_VOID *) &srvMsg, (CS_INT) sizeof (srvMsg));
	srvMsg.severity = SRV_FATAL_PROCESS;
	srvMsg.status = CS_FIRST_CHUNK | CS_LAST_CHUNK;

	/*
	** Get the process descriptor for this thread.
	*/
	if (srv_thread_props (srvproc, CS_GET, SRV_T_USERDATA,
		(CS_VOID *) &procDesc, (CS_INT) sizeof (procDesc),
		(CS_INT *) NULL) == CS_FAIL)
	{
		(void) sprintf (msg,
			"Could not get SRV_T_USERDATA in ims_addEventBegin.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (NULL);
	}

	if (procDesc == (FTS_PROC_DESC *) NULL)
	{
		(void) sprintf (msg,
			"Process descriptor is NULL in ims_addEventBegin.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (NULL);
	}

	/*
	** Access policy information.
	*/
	catReq = &(procDesc->catReq);
	userSpec = &(catReq->userSpec);
	datasetPolicy = procDesc->datasetPolicy;
	sensorPolicy = procDesc->sensorPolicy;
	clientReq = &(procDesc->clientReq);
	msgStruct = procDesc->msgStruct;
	userCap = &(catReq->userCap);


	/*
	** Set user specification to open a database connection.
	*/
	userSpec->dbUserName = procDesc->surrogate.login;
	userSpec->dbPassword = procDesc->surrogate.password;
	userSpec->program = procDesc->ftsProgramName;
	userSpec->server = procDesc->catSrvName;
	userSpec->database = procDesc->catDbName;

	/*
	** Open database connection. This connection is to remain open
	** for the entire granule event.
	*/
	if (ims_srvCat (srvproc, FTS_OPEN_CONNECTION) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL, EFTS_OPEN_CONNECTION);
		return (NULL);
	}


	if ((status = ims_srvCat (srvproc, FTS_CHECK_FILE_TYPES)) < IMS_OK)
	{
		return (NULL);
	}

	/*
	** Close FTS catalog connection. 
	*/

	if (ims_srvCat (srvproc, FTS_CLOSE_CONNECTION) < IMS_OK)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL, 
			EFTS_CLOSE_CONNECTION);
		return (NULL);
	}
	return(procDesc->fileTypes);

}

/******************************************************************************
**
** updateFileSizes ()
**
******************************************************************************/

int updateFileSizes (
	SRV_PROC *srvproc,
	FTS_PROC_DESC *procDesc)
{
	FTS_GRANULE_DESC *granuleDesc;
	FTS_FILE_LIST *fileList;
	FTS_CAT_STRUCT *catReq;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_CAT_GRANULESPEC *granuleSpec;
	IMS_MSG_STRUCT *msgStruct;
	FTS_FILE_POLICY *filePolicy;
	FTS_FORMAT_POLICY *formatPolicy;
	CS_SERVERMSG srvMsg;
	struct stat statBuf;
	char fileName[IMS_COL60_LEN+1];
	char fullPathName[IMS_PATH_LEN+IMS_COL60_LEN+1];
	unsigned long dataBytes = 0;
	unsigned long metaBytes = 0;

	/*
	** We have access to the policy information through the
	** procDesc->mission and procDesc->policy pointers.
	*/

	granuleDesc = &(procDesc->granuleDesc);
	fileList = granuleDesc->fileList;
	msgStruct = procDesc->msgStruct;
	catReq = &(procDesc->catReq);
	filePolicy = procDesc->filePolicy;
	granuleSpec = catReq->granuleSpec;
	datasetPolicy = procDesc->datasetPolicy;

	/*
	** Determine the file policy based on format.
	*/

	filePolicy = datasetPolicy->filePolicy;

	while (filePolicy != (FTS_FILE_POLICY *) NULL)
	{
		if (strcmp (filePolicy->format,
			granuleSpec->format) == 0)
		{
			break;
		}
		filePolicy = filePolicy->next;
	}


	/*
	** Sum file sizes based on type.
	** Metadata files are summed seperately so we need to extract
	** the type of the file.
	*/


	formatPolicy = filePolicy->formatPolicy;
	while (formatPolicy != NULL)
	{
		/*
		** If the archive files are local, then only sum
		** the MTA at this point.  
		*/

		if ((datasetPolicy->local_archive_p == 'Y') &&
			strcmp(formatPolicy->extension, "MTA"))	
		{
			formatPolicy = formatPolicy->next;
			continue;
		}

		if ((datasetPolicy->local_archive_p == 'Y') &&
			strcmp(formatPolicy->extension, "M"))	
		{
			formatPolicy = formatPolicy->next;
			continue;
		}

		while (fileList != (FTS_FILE_LIST *) NULL)
		{
			/*
			** If we found the extension, then process it...
			*/

			if (!strcmp (formatPolicy->extension, fileList->extension))
			{
				break;
			}
			fileList = fileList->next;
		}	


		/*
		** If we have the format of the file, then sum the correct
		** fields, otherwise we have problems...
		*/

		if (fileList != NULL)
		{
			/*
			** Get file size
			*/


			if (stat(fileList->fullPathName, &statBuf))
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
					"Could not stat file %s.", 
					fileList->fullPathName); 

				return(IMS_ERROR);
			}	

			/*
			** If this is the Metadata file, then sum it
			*/
			if (formatPolicy->type == IMS_META_DATA)
				metaBytes += statBuf.st_size;
			else
				dataBytes += statBuf.st_size;
		}
		else
		{
			/*
			** Big problems, the file type is unknown...
			*/

			(void) ims_msg (msgStruct, IMS_ERROR,
				"The extension of %s is not found in the fileList",
					formatPolicy->extension);
					
			return(IMS_ERROR);
		}

		formatPolicy = formatPolicy->next;
	}

	/*
	** If the local archive flag is set, then sum all files
	** but the MTA file since the local files were skipped in the
	** above control loop.
	*/

	formatPolicy = filePolicy->formatPolicy;

	while ((formatPolicy != (FTS_FORMAT_POLICY *) NULL) &&
			(datasetPolicy->local_archive_p == 'Y'))	
	{
		/*
		** Verify that the local archive path is set.
		*/
		
		if (datasetPolicy->localArchivePath == NULL)
		{
			(void) ims_msg(msgStruct, IMS_ERROR,
				"No local archive path provided for the dataset");
			return(IMS_ERROR);
		}

		/*
		** Check if extension is MTA.  Metadata files are ingested
		** into the archive and can not be local.
		*/

		if (!strncmp(formatPolicy->extension, "MTA", 3))
		{
			formatPolicy = formatPolicy->next;	
			continue;	
		}

		if (!strcmp(formatPolicy->extension, "M"))
		{
			formatPolicy = formatPolicy->next;	
			continue;	
		}

		/*
		**  Build a filename for the local file
		*/

		if (granuleSpec->version > 0)
		{
			(void) sprintf (fileName, "%s.%s.%d",
				granuleSpec->name, formatPolicy->extension, 
				granuleSpec->version);

			ims_concatFilePath (fullPathName,
				datasetPolicy->localArchivePath, fileName);
		}
		else
		{
			(void) sprintf (fileName, "%s.%s",
				granuleSpec->name, formatPolicy->extension);

			ims_concatFilePath (fullPathName,
				datasetPolicy->localArchivePath, fileName);
		}
		/*
		** Check permission to read file and then stat the file
		** to get the size of it.
		*/

		if (access(fullPathName, R_OK) < 0)
		{
			(void) ims_msg(msgStruct, IMS_ERROR,
				"Server does not have access or correct permission to file %s",
				fullPathName);
			return(IMS_ERROR);
		}

		/*
		** Check file size 
		*/

		if (stat(fullPathName, &statBuf))
		{
			(void) ims_msg (msgStruct, IMS_ERROR,
				"Could not stat file %s.", 
				fullPathName); 

			return(IMS_ERROR);
		}	

		/*
		** Increment data bytes which does not include the 
		** metadata file.  MTA files are excluded from being
		** local and therefore no check is required.
		*/

		dataBytes += statBuf.st_size;

		formatPolicy = formatPolicy->next;

	}

	/*
	** Note that the database connection must currently be open by
	** the caller.
	*/


	/*
	** Update the values specific values.
	*/

	granuleDesc->dataBytes = dataBytes;
	granuleDesc->metaBytes = metaBytes;
	
	if (ims_srvCat (srvproc, FTS_UPDATE_GRANULE_SIZES) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL, 
			"Could not update granules table file sizes");
		return (IMS_FATAL);
	}


	return(IMS_OK);
}



