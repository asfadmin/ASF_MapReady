static char *sccs = "@(#)ims_mediaJob.c	5.3  07/19/96";
/******************************************************************************
**
** File:	ims_mediaJob.c
**
** Function: Provide interface between ims_op and ims_mediaDist to 
** perform job control processing and convert key parameters passed to
** the job by the ims_op parent.
**
** Author: Dan Crichton
**
** Date:    9/6/95
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
#include <syslog.h>
#include <signal.h>
#include <sys/wait.h>

#include <ims_job_control.h>
#include <ims_media.h>
#include <ims_mediaJob.h>

/*
** Local Types
*/

typedef struct CAT_USERSPEC
{
	char *dbUserName;
} CAT_USERSPEC;

/*
** Local Functions
*/

static int perform_media_dist(IMS_MSG_STRUCT *, int);

/*
** Global variables
*/

static char *glb_programName;


/****************************************************************************
**
** main
**
****************************************************************************/

void main(int argc, char *argv[])
{
	int key;
	char *ptr;
	struct utsname uname_info;    /* Structure for uname() */
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	IMS_MSG_STRUCT *msgDesc;
	int job_id;

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
	(void) ims_msgQueueFlag(msgDesc, IMS_ON);

	/*
	** Ignore death of a parent signal event
	*/

	signal(SIGINT, SIG_IGN);
								 

	/*
	** Do some processing....
	*/

	if (argc < 2)
	{
		(void) ims_msgStructFree (msgDesc);
		exit(1);
	}


	if (ims_updateJobStatus(msgDesc, atoi(argv[2]), IMS_JOB_PENDING, 
				getpid()) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Could not set job status.\n");
		(void) ims_sendJobMsgs(msgDesc, atoi(argv[2]));
		(void) ims_msgStructFree (msgDesc);
		exit(1);
	}

	/*
	** Parse user parameters in shared memory id named in argv[3]. 
	*/


	if (perform_media_dist(msgDesc, atoi(argv[3])) < IMS_OK)
	{
		ims_msg(msgDesc, IMS_ERROR, "Media job %d distribution failed.",
			atoi(argv[2]));
		(void) ims_sendJobMsgs(msgDesc, atoi(argv[2]));
		(void) ims_msgStructFree(msgDesc);
		exit(1);
	}

	(void) ims_jobComplete(msgDesc, atoi(argv[2]));

	(void) ims_msgStructFree (msgDesc);
	exit(0);
}

/****************************************************************************
**
** perform_media_dist
**
** Parses shared memory block passed from parent and calls media distribution
** function.
****************************************************************************/

static int perform_media_dist(
	IMS_MSG_STRUCT *msgDesc,
	int shmid)
{
	IMS_MSG_STRUCT *parentDesc;
	char *mediaItemList;
	char qcSpec[IMS_PATH_LEN + 1];
	char stagePath[IMS_PATH_LEN + 1];
	DEVICE_INFO deviceInfo;
	MEDIA_USER_SPEC mediaUserSpec;
	MEDIA_ITEM_LIST *element, *itemList;
	IMS_MEDIA_PARAMS *media_params;
	DBSMALLINT mediaType, media_fmt_type;
	pnt_vdf_cat_request_t pnt_vdfReq;
	char mediaId[IMS_COL15_LEN+1];

	DBINT order_id;
	IMS_JOB_USER_SPEC userSpec; 
	int mediaItemCount;
	int i;
	int status;
	int ceosFlag = IMS_TRUE;
	
	/*
	** Lock down shared memory...
	*/

	media_params = (void *) ims_shm_lock(shmid);

	if (media_params == NULL)
	{
		/*
		** Could not lock memory.
		*/

		(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not get media distribution parameters");

		return(IMS_ERROR);

	}


	/*
	** Split apart the objects sent to this child process
	*/

	parentDesc = msgDesc;
	mediaType = media_params->media_type;
	media_fmt_type = media_params->media_fmt_type;
	order_id = media_params->order_id;

	(void) strcpy(userSpec.username, media_params->catSpec.username);
	(void) strcpy(userSpec.password, media_params->catSpec.password);
	(void) strcpy(userSpec.database, media_params->catSpec.database);
	(void) strcpy(userSpec.server, media_params->catSpec.server);
	(void) strcpy(userSpec.program,  media_params->catSpec.program);



	mediaUserSpec.username = (char *) userSpec.username;
	mediaUserSpec.password = (char *) userSpec.password;
	mediaUserSpec.program = (char *) userSpec.program;

	if (userSpec.server[0] != '\0')
		mediaUserSpec.server = (char *) userSpec.server;
	else
		mediaUserSpec.server = NULL;


	if (userSpec.database[0] != '\0')
		mediaUserSpec.database = (char *) userSpec.database;
	else
		mediaUserSpec.database = NULL;



	memcpy((void *) &deviceInfo, (void *) &(media_params->deviceInfo), 
			sizeof(DEVICE_INFO));
	

	/*
	** Build Linked List
	*/

	itemList = NULL;

	mediaItemCount = media_params->mediaItemCount;

	for (i = 0 ; i < media_params->mediaItemCount; i++)
	{
		/*
		** Build each link 
		*/


		if (itemList == NULL)
		{
			element = malloc(sizeof(MEDIA_ITEM_LIST));
			itemList = element;
		}
		else
		{
			element->next = malloc(sizeof(MEDIA_ITEM_LIST));
			element = element->next;
		}
		
		
		if (element == NULL)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not allocate memory for media dist. parameters");
			ims_shm_unlock(shmid, (void *) media_params);
			return(IMS_ERROR);
		}

		element->item_id = media_params->mediaItemArray[i].item_id;
		element->status = media_params->mediaItemArray[i].status;
		element->next = NULL;
	}

	/*
	** Unlock shared memory 
	*/


	ims_shm_unlock(shmid, (void *) media_params);


    /*
	** Allocate space for the vdf_cat_request_t structure.
	*/
	if ((pnt_vdfReq = (pnt_vdf_cat_request_t) malloc
		((size_t) sizeof (vdf_cat_request_t))) ==
			(pnt_vdf_cat_request_t) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for vdf_cat_request_t structure.");
		status = IMS_FATAL;
		return(IMS_ERROR);
	}

    (void) memset (pnt_vdfReq, 0, (size_t) sizeof (vdf_cat_request_t));
	 
	 /*
	 ** Assign request structure values.
	 */
	 pnt_vdfReq->msgDesc = msgDesc;
	(void) strcpy (pnt_vdfReq->username, mediaUserSpec.username);
	(void) strcpy (pnt_vdfReq->password, mediaUserSpec.password);
	(void) strcpy (pnt_vdfReq->programName, mediaUserSpec.program);

	if (mediaUserSpec.server != NULL)
		(void) strcpy (pnt_vdfReq->catSrvName, mediaUserSpec.server);
	else
		pnt_vdfReq->catSrvName[0] = '\0';

	if (mediaUserSpec.database != NULL)
		(void) strcpy (pnt_vdfReq->catDbName, mediaUserSpec.database);
	else
		pnt_vdfReq->catDbName[0] = '\0';

	
	/*
	** Open a connection to the database server.
	*/
	if ((status = ims_vdfCat (pnt_vdfReq, VDF_OPEN_CONNECTION)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
				"Could not open a database server connection.");
		return(IMS_ERROR);
	}

	/*
	** Get the next media identifier if applicable.
	*/

	if ((status = ims_deviceMediaId (msgDesc, (char *) &mediaUserSpec,
			mediaType, media_fmt_type, mediaId, IMS_FALSE)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not obtain the media identifier for the target media.");
		goto ERROR;

	}

#if 0
	/*
	** Record the beginning of the media job.
	** Set-up the necessary variables first.
	*/
	if (mediaType == IMS_DISK)
	{
		/*
		** Populate the deviceInfo stucture with
		** the FTP device information.
		*/
		deviceInfo.device_id = (DBSMALLINT) 0;
		(void) strcpy (deviceInfo.name, "FTP Device 0");
	}
#endif


	pnt_vdfReq->device_id = deviceInfo.device_id;
    pnt_vdfReq->order_id = order_id;
	pnt_vdfReq->media_type = mediaType;
	(void) strcpy (pnt_vdfReq->media_id, mediaId);
		 
	if ((status = ims_vdfCat (pnt_vdfReq, VDF_START_JOB)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			 "Could not record the beginning of the media job in the catalog.");
		goto ERROR;
	}



	/*
	** Perform Distribution
	*/

	status = IMS_OK;

	if (ims_mediaDist(parentDesc, pnt_vdfReq,  
		mediaType,
		media_fmt_type, order_id, itemList, &deviceInfo,
		qcSpec, BRIEF_REPORT, &ceosFlag) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Media Distribution Failed");
		status = IMS_ERROR;
		pnt_vdfReq->status = MEDIA_JOB_FAILED;
		(void) ims_vdfCat (pnt_vdfReq, VDF_END_JOB);
		goto ERROR;
	}

	pnt_vdfReq->status = MEDIA_JOB_COMPLETE;

	/*
	** Perform Quality Check if applicable.
	*/
	if (ceosFlag == IMS_FALSE)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"The quality check report cannot be performed on non-CEOS "
			"products. This distribution contains non-CEOS products.");
	}
	else /* All products are in CEOS format. */
	{
		/*
		** If we generated a CEOS tape then the quality check
		** should be performed on the target tape.
		*/
		if ((media_fmt_type == IMS_CEOS) && (mediaType != IMS_DISK)) 
		{
			(void) strcpy (qcSpec, deviceInfo.path);
		}

		if (status == IMS_OK) 
		{
			status = ims_qc(msgDesc, (void *) &mediaUserSpec, BRIEF_REPORT,
							qcSpec, mediaId, IMS_FALSE);
			
			if (status < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR, "Quality Check Failed");
				status = IMS_ERROR;
				pnt_vdfReq->status = MEDIA_JOB_FAILED;

				/*
				** Fall through and end media job.
				*/
			}

			/*
			** Remove temporary un-TAR area.
			** Go ahead and remove even if an error above occurred. 
			*/

			if ((media_fmt_type == IMS_TAR) && (mediaType != IMS_DISK))
			{
				(void) ims_extractPath (qcSpec, stagePath);
												 
				if (ims_removeStageDir (msgDesc,
										 stagePath) < IMS_OK)
				{
					(void) ims_msg (msgDesc, IMS_WARNING,
						"Could not remove the stage path '%s'",
						stagePath);

					/*
					** Fall through and end media job.
					*/
				}
			}
		}
	}

	/*
	** Record the ending of the media job.
	*/
	if (ims_vdfCat (pnt_vdfReq, VDF_END_JOB) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"Could not record the ending of media job '%d' in the catalog.",
		pnt_vdfReq->job_id);
	}

	
    /*
	** Close the database server connection, free any allocated tape device,
	** free the deviceInfo structure and shutdown the message facility.
	*/
ERROR:
	(void) ims_vdfCat (pnt_vdfReq, VDF_CLOSE_CONNECTION);

	
	
	/*
	** Free up memory
	*/

	free(pnt_vdfReq);

	while (itemList != NULL)
	{
		element = itemList;
		itemList = itemList -> next;
		free(element);
	}

	if (status < IMS_OK)
		return(status);

	/*
	** Lock down shared memory...
	** Send back the media Id to caller.
	*/

	media_params = (void *) ims_shm_lock(shmid);

	if (media_params == NULL)
	{
		/*
		** Could not lock memory.
		*/

		(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not get media distribution parameters");

		return(IMS_ERROR);

	}

	strcpy(media_params->mediaId, mediaId);

	ims_shm_unlock(shmid);	

	return(status);

}



