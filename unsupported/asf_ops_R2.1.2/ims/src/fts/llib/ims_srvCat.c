static char *sccs = "@(#)ims_srvCat.c	5.7 03/13/97";
/******************************************************************************
**
** File:        ims_srvCat.c
**
** Function:    This file provides catalog access for the FTS server.
**
** Date:        5/7/90
**
** Modified:    10/90 - Hoshyar Sayah
**              Modified to allow multi-tasked catalog accesses by the
**              FTS software.
**
**              1/5/94 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Added the include files stdlib.h and string.h. Removed the
**              include files malloc.h and strings.h. Replaced calls to bcopy()
**              with (void) memcpy().
**
**              8/17/94 - H. Sayah, S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              12/9/94 - S. Hardman - R1B
**              Ported to Sybase System 10. See the Open Server
**              Server-Library/C Reference Manual for more information.
**
**              2/27/95 - D. Crichton - R1B
**              Added Account ID processing to the getUserCapability()
**              module.
**
**              3/15/95 - D. Crichton - R1B
**              Add character validation to metadata to addGranuleMetadata(). 
**
**              4/5/95 - D. Crichton - R1B
**              Add new function to getFileTypes for the client.
**
**              5/12/95 - D. Crichton - R1B
**              Add new function updateGranuleSizes to update the 
**              size of the product files in the granules table.
**
**				11/10/95 - D. Crichton - R1B'
**				Add a new function updateGranuleIndex.
**
** Notes:
**
** All catalog accesses made by the FTS server are handled by the function 
** ims_srvCat().  The synopsis is listed below under Local Functions.
**
** The function determines the catalog access requested by the event
** argument.  The catReq structure contains all necessary information
** for accessing the catalog as specified by the event.  procDesc->catReq
** and procDesc->qDesc are the two structures used by the routines in 
** this file.  Allocation and initialization of these structures must
** be performed by the calling routine.
** 
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include <ims_dbms.h>
#include <ospublic.h>
#include <oserror.h>

#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_hash.h>
#include <ims_util.h>
#include <ims_archive.h>
#include <ims_ftsSrvCat.h>
#include <ims_ftsSrv.h>
#include <ims_timeConv.h>
#include <ims_keyword.h>

#include <odldef.h>
#include <ims_v0.h>

/* #include <ims_krb.h> */

/*
** Local Functions (Entry points into this module)
**
** This function prototype can be found in ims_ftsSrvCat.h.
** It is listed here for reference.
**
**	int ims_srvCat (SRV_PROC *, FTS_CAT_EVENT);
*/


/*
** Macro to always round up as necessary.
*/

#define INT_CEILING(x, y)\
		((int) (x/y) < ((float) x/y) ? (int) (x/y + 1) : (int) (x/y))   

/*
** Local Functions
*/
static int searchGranule (FTS_PROC_DESC *);
static int searchGranules (FTS_PROC_DESC *);
static int incrGranuleIdx (FTS_PROC_DESC *);
static int insertGranule (FTS_PROC_DESC *);
static int getLatestGranule (FTS_PROC_DESC *);
static int getMaxGranule (FTS_PROC_DESC *);
static int changeGranuleState (FTS_PROC_DESC *);
static int deleteGranuleRecord (FTS_PROC_DESC *);
static int deleteOldGranuleRecord (FTS_PROC_DESC *);
static int addSpatialData(FTS_PROC_DESC *, FTS_SPATIAL_DATA *, char **);
static int addGranuleMetadata (FTS_PROC_DESC *);
static int clearGranuleNameStamp (FTS_PROC_DESC *);
static int getUserCapability (FTS_PROC_DESC *);
static int addSigEvent (FTS_PROC_DESC *);
static int beginTransaction (FTS_PROC_DESC *);
static int commitTransaction (FTS_PROC_DESC *);
static int rollbackTransaction (FTS_PROC_DESC *);
static int execCmd (FTS_PROC_DESC *);
static int processRetStatus (FTS_PROC_DESC *);
static int checkUserLogin (FTS_PROC_DESC *);
static int getFileTypes (FTS_PROC_DESC *procDesc);
static int updateGranuleSizes (FTS_PROC_DESC *procDesc);
static int updateGranuleIndex (FTS_PROC_DESC *procDesc);
static getReadCount (FTS_PROC_DESC *procDesc);

/******************************************************************************
**
** ims_srvCat ()
**
** Main function handling fts catalog queries.
**
******************************************************************************/

int ims_srvCat (
	SRV_PROC *srvproc,
	FTS_CAT_EVENT event)
{
	FTS_PROC_DESC *procDesc;
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CAT_STRUCT *catReq;
	IMS_MSG_STRUCT *msgStruct;
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
			"Could not get SRV_T_USERDATA in ims_srvCat.\n");
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
			"Process descriptor is NULL in ims_srvCat.\n");
		(CS_VOID) srv_log ((SRV_SERVER *) NULL, CS_TRUE, msg, CS_NULLTERM);

		/* Populate the CS_SERVERMSG structure. */
		srvMsg.msgnumber = FTS_THREAD_ERROR;
		(void) strcpy (srvMsg.text, msg);
		srvMsg.textlen = (CS_INT) strlen (msg);

		(CS_VOID) srv_sendinfo (srvproc, &srvMsg, CS_TRAN_UNDEFINED);
		return (IMS_FATAL);
	}

	msgStruct = procDesc->msgStruct;

	/*
	** We must first make sure that we have a descriptor if the event is
	** anything but FTS_OPEN_CONNECTION.
	*/
	if (procDesc->qDesc == (IMS_QI_DESC_OBJ *) NULL) 
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Query descriptor is not initialized.");
		return (IMS_FATAL);
	}

	/*
	** Assign qDesc and catReq pointers to the current process.
	*/
	qDesc = procDesc->qDesc;
	catReq = &(procDesc->catReq);

	/*
	** Now, let's do our 'catalog' business according to the type of event
	** passed into the function.  
	*/
	switch (event)
	{

	case FTS_CHECK_LOGIN:

		status = checkUserLogin (procDesc);
		break;

	case FTS_CHECK_FILE_TYPES:
		status = getFileTypes (procDesc);
		break;

	case FTS_OPEN_CONNECTION:
		/*
		** We also want to stay logged into the catalog until the 
		** FTS_CLOSE_CONNECTION event is called by the exiting process.
		*/

		/*
		** Setup the descriptor with necessary information.
		*/ 
		IMS_SETUSER (qDesc, catReq->userSpec.dbUserName);

		IMS_SETPSWD (qDesc, catReq->userSpec.dbPassword);

		IMS_SETPROG (qDesc, catReq->userSpec.program);

		IMS_SET_VERBOSE (qDesc, 10);

		if ((int) strlen (catReq->userSpec.server) > 0)
		{
			IMS_SETSERVER (qDesc, catReq->userSpec.server);
		}

		if ((int) strlen (catReq->userSpec.database) > 0)
		{
			IMS_SETDBNAME (qDesc, catReq->userSpec.database);
		}

		/*
		** Do a login to the database.
		*/
		if ((status = ims_qiLogin (qDesc)) < IMS_OK)
		{
			return (status);
		}

		/*
		** Assign srvproc to be the user defined data portion of the
		** dbproc structure.  This is needed by the message and
		** error handlers for SQLServer connections of the ims_fts
		** Open Server.
		*/
		(void) dbsetuserdata (qDesc->dbproc, (BYTE *) srvproc);

		return (IMS_OK);

	case FTS_UPDATE_GRANULE_SIZES:
		status = updateGranuleSizes (procDesc);
		break;

	case FTS_SEARCH_GRANULE:
		status = searchGranule (procDesc);
		break;

	case FTS_SEARCH_GRANULES:
		status = searchGranules (procDesc);
		break;

	case FTS_INCR_GRANULEIDX:
		status = incrGranuleIdx (procDesc);
		break;

	case FTS_INSERT_GRANULE:
		status = insertGranule (procDesc);
		break;

	case FTS_GET_READ_COUNT:
		status = getReadCount (procDesc);
		break;

	case FTS_GET_LATEST_GRANULE:
		status = getLatestGranule (procDesc);
		break;

	case FTS_GET_MAX_GRANULE:
		status = getMaxGranule (procDesc);
		break;

	case FTS_CHANGE_GRANULE_STATE:
		status = changeGranuleState (procDesc);
		break;

	case FTS_DELETE_GRANULE_RECORD:
		status = deleteGranuleRecord (procDesc);
		break;

	case FTS_DELETE_OLD_GRANULE_RECORD:
		status = deleteOldGranuleRecord (procDesc);
		break;

	case FTS_ADD_GRANULE_METADATA:
		status = addGranuleMetadata (procDesc);
		break;

	case FTS_CLEAR_NAME_STAMP:
		status = clearGranuleNameStamp (procDesc);
		break;

	case FTS_UPDATE_GRANULE_INDEX:
		status = updateGranuleIndex (procDesc);
		break;

	case FTS_GET_USER_CAP:
		status = getUserCapability (procDesc);
		break;

	case FTS_ADD_SIG_EVENT:
		status = addSigEvent (procDesc);
		break;

	case FTS_BEGIN_TRANSACTION:
		status = beginTransaction (procDesc);
		break;

	case FTS_ROLLBACK_TRANSACTION:
		status = rollbackTransaction (procDesc);
		break;

	case FTS_COMMIT_TRANSACTION:
		status = commitTransaction (procDesc);
		break;

	case FTS_CLOSE_CONNECTION:
		/* Close the catalog the connection. */
		return (ims_qiLogoff (qDesc)); 

	default:
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Invalid catalog event passed to ims_srvCat().");
		status = IMS_FATAL;
		break;
	}

	/*
	** Release all query-allocated space and re-initialize qDesc for the
	** next time in, leaving open the connection to the catalog.
	*/
	if (qDesc->dbproc != (DBPROCESS *) NULL)
	{
		/*
		** Re-initialize query descriptor for next command, but do
		** not cancel previous command
		*/
		if (ims_qiResetDesc (qDesc) < IMS_OK)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Could not reinitialize the query descriptor.");
			return (IMS_FATAL);
		}
	}

	/*
	** Return with the appropriate status
	*/
	if (status < IMS_OK)
	{
		return (status);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** checkUserLogin ()
**
** Validate that the client username, password, etc are in the catalog tables.
******************************************************************************/

static int checkUserLogin (FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CAT_USERCAP *userCap;
	int status;

	qDesc = procDesc->qDesc;
	userCap = &(procDesc->catReq.userCap);

	
	status = ims_validUser(qDesc, qDesc->msgDesc, procDesc->client.login,
			procDesc->client.password, 0, userCap->accountId);

	return (status);
}



/******************************************************************************
**
** getFileTypes ()
**
******************************************************************************/

static int getFileTypes (FTS_PROC_DESC *procDesc)
{
	IMS_MSG_STRUCT *msgDesc;
	IMS_FILE_TYPES *currPtr;
	IMS_FILE_TYPES *prevPtr;
	char tempExtension[IMS_COL10_LEN+1];
	int status;
	int rowCount;
	int severity;
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CLIENT_REQUEST *clientReq;


	qDesc = procDesc->qDesc;
	msgDesc = qDesc->msgDesc;
	clientReq = &(procDesc->clientReq);

	(void) sprintf (qDesc->cmd, "fts_get_file_types '%s', '%s', '%s', '%s'",
		clientReq->platform, clientReq->sensor, clientReq->dataset,
		clientReq->format);

	procDesc->fileTypes = currPtr = prevPtr = 
		(IMS_FILE_TYPES *) NULL;

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (ims_msgGetSeverity (msgDesc));
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** A row has been returned.
		*/
		rowCount++;

		/*
		** Allocate space for the IMS_FILE_TYPES structure.
		**
		** lint: pointer cast may result in improper alignment
		** No problem, malloc() aligns on worst case boundary.
		*/
		if ((currPtr = (IMS_FILE_TYPES *) malloc 
			((unsigned) sizeof (IMS_FILE_TYPES))) ==
			(IMS_FILE_TYPES *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for IMS_FILE_TYPES structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			return (IMS_FATAL);
		}

		/*
		** fileList points to the first element of the list.
		*/
		if (rowCount == 1)
		{
			procDesc->fileTypes = currPtr;
		}
		else
		{
			prevPtr->next = currPtr;
		}

		currPtr->next = (IMS_FILE_TYPES *) NULL;

		/*
		** Copy the returned data into the structure.
		*/
		(void) memcpy ((char *) &(currPtr->type),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((char *) &(currPtr->position),
			qDesc->valAddr[1], qDesc->valLength[1]);

		/* Copy to a temporary area so we can truncate spaces. */
		(void) memcpy (tempExtension, qDesc->valAddr[2],
			qDesc->valLength[2]);
		tempExtension[qDesc->valLength[2]] = '\0';
		(void) strcpy (currPtr->extension, ims_truncStr (tempExtension));


		/*
		** If the local archive flag is set for the dataset, then look
		** at the extension.  If the extension is not MTA, then 
		** the file will remain local at the client.
		*/


		(void) memcpy ((char *) &(currPtr->localArchiveFlag),
			qDesc->valAddr[3], qDesc->valLength[3]);

		if (currPtr->localArchiveFlag == 'Y')
		{
			if (strncmp(currPtr->extension, "MTA", 3))
			{
				/*
				** If this is not the metadata file...
				*/
				currPtr->localArchiveFlag = 'Y';
			}
			else if (strcmp(currPtr->extension, "M"))
			{
				/*
				** If this is not the metadata file...
				*/
				currPtr->localArchiveFlag = 'Y';
			}
			else
				/*
				** Metadata is not archived at the client.
				*/

				currPtr->localArchiveFlag = 'N';

		}
		else
			currPtr->localArchiveFlag = 'N';

		prevPtr = currPtr;
	}

	/*
	** See if our stored procedure returned anything.
	*/
	if (rowCount <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"There were no file types returned for format '%s', for the '%s', '%s', '%s' policy.",
			clientReq->format, clientReq->platform,
			clientReq->sensor, clientReq->dataset);
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** searchGranule ()
**
******************************************************************************/

static int searchGranule (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CLIENT_REQUEST *clientReq;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_CAT_GRANULESPEC *granuleSpec;
	IMS_MSG_STRUCT *msgStruct;
	char tempFormat[IMS_COL10_LEN+1];
	char temp_stamp[IMS_COL15_LEN+1];
	char temp_contributor[IMS_COL30_LEN+1];
	int status;

	qDesc = procDesc->qDesc;
	datasetPolicy = procDesc->datasetPolicy;
	clientReq = &(procDesc->clientReq);
	msgStruct = procDesc->msgStruct;

	if (clientReq->version < 0)
	{
		/* Versions are not supported. */
		(void) sprintf (qDesc->cmd, 
			"select granule_idx, status, version, format, o_gdr, name_stamp, \
			contributor\
			from %s where name = '%s'",
			datasetPolicy->granules_table, clientReq->name);
	}
	else if (clientReq->version == 0)
	{
		/* Versions are supported, get the latest. */
		(void) sprintf (qDesc->cmd, 
			"select granule_idx, status, version, format, o_gdr, name_stamp, \
			contributor from %s where (name = '%s' and \
			version = (select max(version) from %s where name = '%s'))", 
			datasetPolicy->granules_table, clientReq->name,
			datasetPolicy->granules_table, clientReq->name);
	}
	else /* Version is greater than 0. */
	{
		/* Versions are supported, get the specific version. */
		(void) sprintf (qDesc->cmd, 
			"select granule_idx, status, version, format, o_gdr, name_stamp, \
			contributor	from %s where (name = '%s' and version = %d)", 
			datasetPolicy->granules_table, clientReq->name,
			clientReq->version);
	}

	if ((status = execCmd (procDesc)) < IMS_OK)
	{
		return (status);
	}

	if (IMS_AFFECTED (qDesc) < 1)
	{
		return (IMS_OK);
	}
	else if (IMS_AFFECTED (qDesc) > 1)
	{
		(void) ims_msg (msgStruct, IMS_FATAL, 
			"Multiple records found for granule '%s'.  Contact DBA.", 
			clientReq->name);
		return (IMS_FATAL);
	}

	/*
	** Allocate space for the FTS_CAT_GRANULESPEC structure.
	**
	** lint: pointer cast may result in improper alignment
	** ???? Review this.
	*/
	if ((granuleSpec = (FTS_CAT_GRANULESPEC *) malloc 
		(sizeof (FTS_CAT_GRANULESPEC))) == (FTS_CAT_GRANULESPEC *) NULL)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Memory allocation for FTS_CAT_GRANULESPEC structure failed.");
		return (IMS_FATAL);
	}

	/* Initialize structure members. */
	granuleSpec->oldGranule_idx = -1;
	procDesc->catReq.granuleSpec = granuleSpec;
	granuleSpec->next = (FTS_CAT_GRANULESPEC *) NULL;
	(void) strcpy (granuleSpec->name, clientReq->name);

	/*
	** Copy in the returned data
	*/
	(void) memcpy ((char *) &(granuleSpec->granule_idx), qDesc->valAddr[0], 
		qDesc->valLength[0]);

	(void) memcpy ((char *) &(granuleSpec->status), qDesc->valAddr[1], 
		qDesc->valLength[1]);

	if ((qDesc->valLength[2] == 0) || (qDesc->valAddr[2] == (char *)NULL))
	{
		/* Version is null. */
		granuleSpec->version = -1;
	}
	else
	{
		(void) memcpy ((char *) &(granuleSpec->version), qDesc->valAddr[2],
			qDesc->valLength[2]);
	}

	(void) memcpy ((char *) tempFormat, qDesc->valAddr[3], 
		qDesc->valLength[3]);
	tempFormat[qDesc->valLength[3]] = '\0';
	(void) strcpy (granuleSpec->format, ims_truncStr (tempFormat));

	(void) memcpy ((char *) &(granuleSpec->o_gdr), qDesc->valAddr[4], 
		qDesc->valLength[4]);

	(void) memcpy ((char *) temp_stamp, qDesc->valAddr[5], 
		qDesc->valLength[5]);
	temp_stamp[qDesc->valLength[5]] = '\0';
	(void) strcpy (granuleSpec->name_stamp, ims_truncStr (temp_stamp));

	(void) memcpy ((char *) temp_contributor, qDesc->valAddr[6], 
		qDesc->valLength[6]);
	temp_stamp[qDesc->valLength[6]] = '\0';
	(void) strcpy (granuleSpec->contributor, ims_truncStr (temp_contributor));

	return (IMS_OK);
}

/******************************************************************************
**
** searchGranules ()
**
******************************************************************************/

static int searchGranules (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CLIENT_REQUEST *clientReq;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_SENSOR_POLICY *sensorPolicy;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_CAT_GRANULESPEC *currPtr;
	FTS_CAT_GRANULESPEC *prevPtr;
	char tempFormat[IMS_COL10_LEN+1];
	char tempContributor[IMS_COL30_LEN+1];
	char temp_stamp[IMS_COL15_LEN+1];
	int status;
	int rowCount;
	int severity;

	qDesc = procDesc->qDesc;
	sensorPolicy = procDesc->sensorPolicy;
	datasetPolicy = procDesc->datasetPolicy;
	clientReq = &(procDesc->clientReq);

	granuleSpec = currPtr = prevPtr = (FTS_CAT_GRANULESPEC *) NULL;

	/*
	** Get the list of files for deletion.
	*/
	if (clientReq->version <= 0)
	{
		(void) sprintf (qDesc->cmd,
			"select granule_idx, status, version, format, o_gdr, contributor,\
			name_stamp\
			from %s where name = '%s'",
			datasetPolicy->granules_table, clientReq->name);
	}
	else
	{
		/* Versions are supported, get the specific version. */
		(void) sprintf (qDesc->cmd,
			"select granule_idx, status, version, format, o_gdr, contributor,\
			name_stamp\
			from %s where (name = '%s' and version = %d)",
			datasetPolicy->granules_table, clientReq->name,
			clientReq->version);
	}

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}
		

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** A row has been returned.
		** Allocate memory space for the FTS_CAT_GRANULESPEC structure to
		** contain the return results.
		**
		** lint: pointer cast may result in improper alignment
		** ???? Review this.
		*/
		rowCount++;
		if ((currPtr = (FTS_CAT_GRANULESPEC *) 
			malloc (sizeof (FTS_CAT_GRANULESPEC))) ==
			(FTS_CAT_GRANULESPEC *) NULL)
		{
			(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
				"Memory allocation for FTS_CAT_GRANULESPEC structure failed.");
			return (IMS_FATAL);
		}

		if (rowCount == 1)
		{
			granuleSpec = currPtr;
			procDesc->catReq.granuleSpec = granuleSpec;
		}
		else
		{
			prevPtr->next = currPtr;
		}

		/* Initialize structure members. */
		currPtr->oldGranule_idx = -1;
		currPtr->next = (FTS_CAT_GRANULESPEC *) NULL;
		(void) strcpy (currPtr->name, clientReq->name);
		prevPtr = currPtr;

		/*
		** Copy in the returned data.
		*/
		(void) memcpy ((char *) &(currPtr->granule_idx), qDesc->valAddr[0], 
			qDesc->valLength[0]);

		(void) memcpy ((char *) &(currPtr->status), qDesc->valAddr[1], 
			qDesc->valLength[1]);

		if ((qDesc->valLength[2] == 0) ||
			(qDesc->valAddr[2] == (char *) NULL))
		{
			/* Version is null. */
			currPtr->version = -1;
		}
		else
		{
			(void) memcpy ((char *) &(currPtr->version),
				qDesc->valAddr[2], qDesc->valLength[2]);
		}

		(void) memcpy ((char *) tempFormat, qDesc->valAddr[3],
			qDesc->valLength[3]);
		tempFormat[qDesc->valLength[3]] = '\0';
		(void) strcpy ((char *) currPtr->format, ims_truncStr (tempFormat));

		(void) memcpy ((char *) &(currPtr->o_gdr), qDesc->valAddr[4], 
			qDesc->valLength[4]);

        /* Copy and format contributor field */
		(void) memcpy ((char *) tempContributor, qDesc->valAddr[5],
			qDesc->valLength[5]);
		tempContributor[qDesc->valLength[5]] = '\0';
		(void) strcpy ((char *) currPtr->contributor, 
					   ims_truncStr (tempContributor));
      
        /* Copy and name stamp field */
		(void) memcpy ((char *) temp_stamp, qDesc->valAddr[6],
			qDesc->valLength[6]);
		temp_stamp[qDesc->valLength[6]] = '\0';
		(void) strcpy ((char *) currPtr->name_stamp,
					   ims_truncStr (temp_stamp));
      
	}

	/*
	** Check the returned status value.
	*/
	if ((severity = processRetStatus (procDesc)) < IMS_OK)
	{
		return (severity);
	}

	/*
	** Check if a list of files exists for delete.
	*/
	if (IMS_AFFECTED (qDesc) < 1)
	{
		if (clientReq->version > 0)
		{
			(void) ims_msg (procDesc->msgStruct, IMS_ERROR,
				"Granule '%s', version '%d' for '%s, %s, %s' not found.",
				clientReq->name, clientReq->version, 
				sensorPolicy->platform, sensorPolicy->sensor,
				datasetPolicy->dataset);
		}
		else
		{
			(void) ims_msg (procDesc->msgStruct, IMS_ERROR,
				"Granule '%s' for '%s, %s, %s' not found.",
				clientReq->name, 
				sensorPolicy->platform, sensorPolicy->sensor,
				datasetPolicy->dataset);
		}
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** incrGranuleIdx ()
**
** Increment the granule_idx field in the last_granule_idx table and use
** this new value for our current granule.
**
******************************************************************************/

static int incrGranuleIdx (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CLIENT_REQUEST *clientReq;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_CAT_STRUCT *catReq;
	IMS_MSG_STRUCT *msgStruct;
	int status;

	qDesc = procDesc->qDesc;
	catReq = &(procDesc->catReq);
	datasetPolicy = procDesc->datasetPolicy;
	clientReq = &(procDesc->clientReq);
	msgStruct = procDesc->msgStruct;

	(void) sprintf (qDesc->cmd, "fts_incr_granule_idx %d",
		datasetPolicy->dataset_idx);

	if ((status = execCmd (procDesc)) < IMS_OK)
	{
		return (status);
	}

	if (IMS_AFFECTED (qDesc) < 1)
	{
		(void) ims_msg (msgStruct, IMS_FATAL, 
			"Incrementing granule_idx failed for granule '%s'.  Contact DBA.", 
			clientReq->name);
		return (IMS_FATAL);
	}

	/*
	** Allocate space for the FTS_CAT_GRANULESPEC structure if it has not
	** already been done.
	*/
	if (catReq->granuleSpec == (FTS_CAT_GRANULESPEC *)NULL)
	{
		/*
		** lint: pointer cast may result in improper alignment
		** ???? Review this.
		*/
		if ((granuleSpec = (FTS_CAT_GRANULESPEC *) malloc 
		(sizeof (FTS_CAT_GRANULESPEC))) == (FTS_CAT_GRANULESPEC *) NULL)
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Memory allocation for FTS_CAT_GRANULESPEC structure failed.");
			return (IMS_FATAL);
		}

		/* Initialize structure members. */
		granuleSpec->oldGranule_idx = -1;
		granuleSpec->version = clientReq->version;
		granuleSpec->o_gdr = (datasetPolicy->oagdr & (BYTE) 23);
		(void) strcpy (granuleSpec->name, clientReq->name);
		(void) strcpy (granuleSpec->format, clientReq->format);
		granuleSpec->next = (FTS_CAT_GRANULESPEC *)NULL;
		procDesc->catReq.granuleSpec = granuleSpec;
		memset(granuleSpec->name_stamp, 0, sizeof(granuleSpec->name_stamp));

	}
	else
	{
		/* Set oldGranule_idx for replace operations. */
		granuleSpec = catReq->granuleSpec;
		granuleSpec->oldGranule_idx = granuleSpec->granule_idx;
	}

	/*
	** Copy in the returned data
	*/
	(void) memcpy ((char *) &(granuleSpec->granule_idx), qDesc->valAddr[0], 
		qDesc->valLength[0]);

	return (IMS_OK);
}

/******************************************************************************
**
** insertGranule ()
**
** Insert a record into the pertinent granules_x table.
**
******************************************************************************/

static int insertGranule (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CLIENT_REQUEST *clientReq;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_CAT_STRUCT *catReq;
	int status;

	qDesc = procDesc->qDesc;
	clientReq = &(procDesc->clientReq);
	catReq = &(procDesc->catReq);
	datasetPolicy = procDesc->datasetPolicy;
	granuleSpec = catReq->granuleSpec;

	if (granuleSpec->version <= 0)
	{
		(void) sprintf (qDesc->cmd, 
			"insert into %s (granule_idx, dataset_idx, status, o_gdr,\
			contributor, received_time, format, name, version, \
			data_kbytes, metadata_kbytes, name_stamp) values \
			(%d, %d, %d, %d, '%s', getdate(), '%s', '%s', NULL, %d, %d, '%s')",
			datasetPolicy->granules_table,
			granuleSpec->granule_idx, datasetPolicy->dataset_idx, 
			granuleSpec->status, granuleSpec->o_gdr, procDesc->userName, 
			clientReq->format, granuleSpec->name, 0, 0, 
			granuleSpec->name_stamp);
	}
	else
	{
		(void) sprintf (qDesc->cmd, 
			"insert into %s (granule_idx, dataset_idx, status, o_gdr,\
			contributor, received_time, format, name, version, \
			data_kbytes, metadata_kbytes, name_stamp) values \
			(%d, %d, %d, %d, '%s', getdate(), '%s', '%s', %d, %d, %d, '%s')", 
			datasetPolicy->granules_table,
			granuleSpec->granule_idx, datasetPolicy->dataset_idx, 
			granuleSpec->status, granuleSpec->o_gdr, procDesc->userName, 
			clientReq->format, granuleSpec->name, granuleSpec->version, 0, 0,
			granuleSpec->name_stamp);
	}

	if ((status = execCmd (procDesc)) < IMS_OK)
	{
		return (status);
	}

	return (IMS_OK);
}
/******************************************************************************
**
** getReadCount ()
**
** Get the granule's read count.
**
******************************************************************************/

static getReadCount (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CLIENT_REQUEST *clientReq;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_CAT_GRANULESPEC *granuleSpec, *ptr;
	IMS_MSG_STRUCT *msgStruct;
	int status;
	char tempFormat[IMS_COL10_LEN+1];

	qDesc = procDesc->qDesc;
	datasetPolicy = procDesc->datasetPolicy;
	clientReq = &(procDesc->clientReq);
	msgStruct = procDesc->msgStruct;
	granuleSpec = procDesc->catReq.granuleSpec;


	/*
	** Initialize read count values.
	*/

	if (granuleSpec != NULL)
	{
	   granuleSpec->readCount = 0;
	}
	else
	{
		/*
		** No granules exist to check the read count against.
		*/
		
		return(IMS_OK);
	}


	(void) sprintf (qDesc->cmd, "get_read_count %d, %d",
			datasetPolicy->dataset_idx,
			granuleSpec->granule_idx);
	
	if ((status = execCmd (procDesc)) < IMS_OK)
	{
		return (status);
	}

	if (IMS_AFFECTED (qDesc) < 1)
	{
		return (IMS_OK);
	}
	else if (IMS_AFFECTED (qDesc) > 1)
	{
		(void) ims_msg (msgStruct, IMS_FATAL, 
			"Multiple records found for granule '%s'.  Contact DBA.", 
			clientReq->name);
		return (IMS_FATAL);
	}


	/*
	** Verify that the granuleSpec points to a granules table.
	*/

	if (granuleSpec == NULL)
	{
		(void) ims_msg (msgStruct, IMS_FATAL, 
			"granuleSpec not initialzed.  Can't determine readCount.");

		return (IMS_FATAL);
	}


	/*
	** Copy in the returned data into the readCount area.
	*/

	for (ptr = granuleSpec; ptr != NULL; ptr = ptr->next)
	{

	    (void) memcpy ((char *) &(ptr->readCount), qDesc->valAddr[0], 
	    		qDesc->valLength[0]);
	}

	return(IMS_OK);
}


/******************************************************************************
**
** getLatestGranule ()
**
******************************************************************************/

static getLatestGranule (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CLIENT_REQUEST *clientReq;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_CAT_GRANULESPEC *granuleSpec;
	IMS_MSG_STRUCT *msgStruct;
	int status;
	char tempFormat[IMS_COL10_LEN+1];
	char temp_stamp[IMS_COL15_LEN+1];

	qDesc = procDesc->qDesc;
	datasetPolicy = procDesc->datasetPolicy;
	clientReq = &(procDesc->clientReq);
	msgStruct = procDesc->msgStruct;

	(void) sprintf (qDesc->cmd, 
		"select granule_idx, status, name, version, format, o_gdr,\
			name_stamp\
			from %s where (status = %d and received_time = \
			(select max(received_time) from %s where status = %d))", 
		datasetPolicy->granules_table, IMS_AVAILABLE,
		datasetPolicy->granules_table, IMS_AVAILABLE);

	if ((status = execCmd (procDesc)) < IMS_OK)
	{
		return (status);
	}

	if (IMS_AFFECTED (qDesc) < 1)
	{
		return (IMS_OK);
	}
	else if (IMS_AFFECTED (qDesc) > 1)
	{
		(void) ims_msg (msgStruct, IMS_FATAL, 
			"Multiple records found for granule '%s'.  Contact DBA.", 
			clientReq->name);
		return (IMS_FATAL);
	}

	/*
	** Allocate space for the FTS_CAT_GRANULESPEC structure.
	**
	** lint: pointer cast may result in improper alignment
	** ???? Review this.
	*/
	if ((granuleSpec = (FTS_CAT_GRANULESPEC *) malloc 
		(sizeof (FTS_CAT_GRANULESPEC))) == (FTS_CAT_GRANULESPEC *) NULL)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Memory allocation for FTS_CAT_GRANULESPEC structure failed.");
		return (IMS_FATAL);
	}

	/* Initialize structure members. */
	granuleSpec->oldGranule_idx = -1;
	procDesc->catReq.granuleSpec = granuleSpec;
	granuleSpec->next = (FTS_CAT_GRANULESPEC *) NULL;
	(void) strcpy (granuleSpec->name, clientReq->name);

	/*
	** Copy in the returned data
	*/
	(void) memcpy ((char *) &(granuleSpec->granule_idx), qDesc->valAddr[0], 
		qDesc->valLength[0]);

	(void) memcpy ((char *) &(granuleSpec->status), qDesc->valAddr[1], 
		qDesc->valLength[1]);

	(void) memcpy ((char *) granuleSpec->name, qDesc->valAddr[2], 
		qDesc->valLength[2]);
	granuleSpec->name[qDesc->valLength[2]] = '\0';

	if ((qDesc->valLength[3] == 0) || (qDesc->valAddr[3] == (char *)NULL))
	{
		/* Version is null. */
		granuleSpec->version = -1;
	}
	else
	{
		(void) memcpy ((char *) &(granuleSpec->version), qDesc->valAddr[3],
			qDesc->valLength[3]);
	}

	(void) memcpy ((char *) tempFormat, qDesc->valAddr[4], 
		qDesc->valLength[4]);
	tempFormat[qDesc->valLength[4]] = '\0';
	(void) strcpy (granuleSpec->format, ims_truncStr (tempFormat));

	(void) memcpy ((char *) &(granuleSpec->o_gdr), qDesc->valAddr[5], 
		qDesc->valLength[5]);

	(void) memcpy ((char *) temp_stamp, qDesc->valAddr[5], 
		qDesc->valLength[5]);
	temp_stamp[qDesc->valLength[5]] = '\0';
	(void) strcpy (granuleSpec->name_stamp, ims_truncStr (temp_stamp));

	return (IMS_OK);
}

/******************************************************************************
**
** getMaxGranule ()
**
******************************************************************************/

static getMaxGranule (
	FTS_PROC_DESC *procDesc)
{
	return (IMS_OK);
}

/******************************************************************************
**
** changeGranuleState ()
**
** Change the status in the appropriate granules_x table.
**
******************************************************************************/

static int changeGranuleState (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CAT_STATESPEC *stateSpec;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_DATASET_POLICY *datasetPolicy;
	int rowCount;
	int status;

	qDesc = procDesc->qDesc;
	stateSpec = &(procDesc->catReq.stateSpec);
	granuleSpec = procDesc->catReq.granuleSpec;
	datasetPolicy = procDesc->datasetPolicy;

	rowCount = 0;
	while (granuleSpec != (FTS_CAT_GRANULESPEC *) NULL)
	{
		rowCount++;

		(void) sprintf (qDesc->cmd,
			"update %s set status = %d where granule_idx = %d",
			datasetPolicy->granules_table, stateSpec->status,
			granuleSpec->granule_idx);

		if ((status = execCmd (procDesc)) < IMS_OK)
		{
			return (status);
		}

		if (ims_qiResetDesc (qDesc) < IMS_OK)
		{
			(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
				"Could not reinitialize the query descriptor.");
			return (IMS_FATAL);
		}

		granuleSpec = granuleSpec->next;
	}

	if (rowCount <= 0)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"The granuleSpec structure was empty. Unable to change the granule status.");
		return (IMS_FATAL);
	}
	return (IMS_OK);
}

/******************************************************************************
**
** deleteGranuleRecord ()
**
******************************************************************************/

static int deleteGranuleRecord (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_DATASET_POLICY *datasetPolicy;
	int status;
	int rowCount;

	qDesc = procDesc->qDesc;
	granuleSpec = procDesc->catReq.granuleSpec;
	datasetPolicy = procDesc->datasetPolicy;

	rowCount = 0;
	while (granuleSpec != (FTS_CAT_GRANULESPEC *) NULL)
	{
		rowCount++;

		(void) sprintf (qDesc->cmd,
			"delete from %s where granule_idx = %d",
			datasetPolicy->granules_table,
			granuleSpec->granule_idx);

		if ((status = execCmd (procDesc)) < IMS_OK)
		{
			return (status);
		}

		if (ims_qiResetDesc (qDesc) < IMS_OK)
		{
			(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
				"Could not reinitialize the query descriptor.");
			return (IMS_FATAL);
		}

		/*
		** Reset old record status for replace operation.
		*/
		if (granuleSpec->oldGranule_idx > 0)
		{
			if (ims_qiResetDesc (qDesc) < IMS_OK)
			{
				(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
					"Could not reinitialize the query descriptor.");
				return (IMS_FATAL);
			}

			(void) sprintf (qDesc->cmd,
				"update %s set status = %d where granule_idx = %d",
				datasetPolicy->granules_table, granuleSpec->status,
				granuleSpec->oldGranule_idx);

			if ((status = execCmd (procDesc)) < IMS_OK)
			{
				return (status);
			}
		}
		granuleSpec = granuleSpec->next;
	}

	if (rowCount <= 0)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"The granuleSpec structure was empty. Unable to delete the granule record.");
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** deleteOldGranuleRecord ()
**
******************************************************************************/

static int deleteOldGranuleRecord (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_DATASET_POLICY *datasetPolicy;
	int status;
	int rowCount;

	qDesc = procDesc->qDesc;
	granuleSpec = procDesc->catReq.granuleSpec;
	datasetPolicy = procDesc->datasetPolicy;

	rowCount = 0;
	while (granuleSpec != (FTS_CAT_GRANULESPEC *) NULL)
	{
		rowCount++;

		(void) sprintf (qDesc->cmd,
			"delete from %s where granule_idx = %d",
			datasetPolicy->granules_table,
			granuleSpec->oldGranule_idx);

		if ((status = execCmd (procDesc)) < IMS_OK)
		{
			return (status);
		}

		if (ims_qiResetDesc (qDesc) < IMS_OK)
		{
			(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
				"Could not reinitialize the query descriptor.");
			return (IMS_FATAL);
		}
		granuleSpec = granuleSpec->next;
	}

	if (rowCount <= 0)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"The granuleSpec structure was empty. Unable to delete the old granule record.");
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** addSpatialData ()
**
** Adds the spatial search results to the granule update string.
******************************************************************************/

int addSpatialData(FTS_PROC_DESC *procDesc,
					FTS_SPATIAL_DATA *spData, char **cmdLine)
{
	float lat_array[4];
	float lon_array[4];
	float minLat, maxLat;
	float minLon, maxLon;
	char pole_included;
	int status;
	char *cPtr = *cmdLine;


	if (spData->asc_data == 'A')
	{
		lat_array[0] = spData->near_e_lat;
		lat_array[1] = spData->far_e_lat;
		lat_array[2] = spData->far_s_lat;
		lat_array[3] = spData->near_s_lat;

		lon_array[0] = spData->near_e_lon;
		lon_array[1] = spData->far_e_lon;
		lon_array[2] = spData->far_s_lon;
		lon_array[3] = spData->near_s_lon;
	}
	else if (spData->asc_data == 'D')
	{
		lat_array[0] = spData->far_s_lat;
		lat_array[1] = spData->near_s_lat;
		lat_array[2] = spData->near_e_lat;
		lat_array[3] = spData->far_e_lat;

		lon_array[0] = spData->far_s_lon;
		lon_array[1] = spData->near_s_lon;
		lon_array[2] = spData->near_e_lon;
		lon_array[3] = spData->far_e_lon;
	}
	else
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
				"Unknown ascending/descending descriptor '%c'.", spData->asc_data);
		return(IMS_FATAL);
	}


	/*
	** Perform spatial search using exported library routine.
	*/

	status = v0_spatial__v0_calcb(&lat_array[0], &lon_array[0],
			&minLat, &maxLat, &minLon, &maxLon, &pole_included);

	
	if (status < IMS_OK)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
				"Spatial search calculation failed.");
		return(IMS_FATAL);

	}

	/*
	** Add the keywords into the update statement
	**/


	sprintf(cPtr, "%s = %f, ", "north_lat", maxLat);
	cPtr = cPtr + strlen(cPtr);
	sprintf(cPtr, "%s = %f, ", "south_lat", minLat);
	cPtr = cPtr + strlen(cPtr);
	sprintf(cPtr, "%s = %f, ", "west_lon", minLon);
	cPtr = cPtr + strlen(cPtr);
	sprintf(cPtr, "%s = %f, ", "east_lon", maxLon);
	cPtr = cPtr + strlen(cPtr);
	sprintf(cPtr, "%s = '%c', ", "pole_included", pole_included);
	cPtr = cPtr + strlen(cPtr);

	*cmdLine = cPtr;

	return (IMS_OK);
}

/******************************************************************************
**
** addGranuleMetadata ()
**
** We treat the insertion of all keyword-value pairs as a single
** transaction.  Either all keyword-value pairs are successfully entered
** or none at all.
**
******************************************************************************/

static int addGranuleMetadata (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_KEYWORD_LIST *keywordList;
	FTS_KEYWORD_LIST *currKey;
	FTS_KEYWORD_VALUE *keywordValue;
	IMS_MSG_STRUCT *msgStruct;
	FTS_SPATIAL_DATA spData;
	IMS_NUMERIC_DATE dateTbl;
	double north_lat, south_lat, east_lon, west_lon;
	char *cPtr;
	int maxLength;
	int keyFound;
	int status;
	int ims_visible = 0;
	char dbTimeBuf[IMS_DATETIME_LEN+1];

	qDesc = procDesc->qDesc;
	granuleSpec = procDesc->catReq.granuleSpec;
	datasetPolicy = procDesc->datasetPolicy;
	keywordList = procDesc->keywordList;
	msgStruct = procDesc->msgStruct;

	/*
	** Allow null keywordList since the mandatory fields were already
	** checked.
	*/

	if (keywordList == NULL)
	{
		return(IMS_OK);
	}

	
	/*
	** Pointer to the last character in the cmd.
	*/
	cPtr = qDesc->cmd;


	/*
	** Free the current local archive since the metadata 
	** should provide a new local archive path if the dataset policy 
	** is set for local archiving.
	*/

	if (datasetPolicy->localArchivePath != NULL)
	{
		free(datasetPolicy->localArchivePath);
		datasetPolicy->localArchivePath = NULL;
	}

	/*
	** Build a dynamic SQL statement to update keyword values
	** in the appropriate granules_x table.
	*/
	(void) sprintf (cPtr, "update %s set ",
		datasetPolicy->granules_table);
	cPtr = cPtr + strlen (cPtr);

	/*
	** Place the keyword names and their values in the update statement.
	*/
	for (currKey = keywordList; currKey != (FTS_KEYWORD_LIST *) NULL; 
		currKey = currKey->next)
	{
		if ((currKey->significance == IMS_MANDATORY_INDEXED) || 
			(currKey->significance == IMS_OPTIONAL_INDEXED) ||
			(currKey->significance == IMS_MANDATORY_NOT_INDEXED) ||
			(currKey->significance == IMS_OPTIONAL_NOT_INDEXED))
		{
			(void) sprintf (cPtr, "%s = ", currKey->keyword);
			cPtr = cPtr + strlen (cPtr);

			switch (currKey->data_type)
			{
			case IMS_INT1_TYPE:
			case IMS_INT2_TYPE:
			case IMS_INT4_TYPE:
				/* Check for a range specification. */
				if ((currKey->min_val != 0) || (currKey->max_val != 0))
				{
					/* See if the value falls in the range. */
					if (((double) currKey->value_integer < currKey->min_val) ||
						((double) currKey->value_integer > currKey->max_val))
					{
						(void) ims_msg (msgStruct, IMS_ERROR,
							"Value for keyword '%s', '%d', is not in the specified range of '%f' to '%f'.",
							currKey->keyword, currKey->value_integer,
							currKey->min_val, currKey->max_val);
						return (IMS_ERROR);
					}
				}

				(void) sprintf (cPtr, "%d, ", currKey->value_integer);
				cPtr = cPtr + strlen (cPtr);
				break;

			case IMS_FLOAT4_TYPE:
			case IMS_FLOAT8_TYPE:
				/* Check for a range specification. */
				if ((currKey->min_val != 0) || (currKey->max_val != 0))
				{
					/* See if the value falls in the range. */
					if ((currKey->value_real < currKey->min_val) ||
						(currKey->value_real > currKey->max_val))
					{
						(void) ims_msg (msgStruct, IMS_ERROR,
							"Value for keyword '%s', '%f', is not in the specified range of '%f' to '%f'.",
							currKey->keyword, currKey->value_real,
							currKey->min_val, currKey->max_val);
						return (IMS_ERROR);
					}
				}

				(void) sprintf (cPtr, "%f, ", currKey->value_real);
				cPtr = cPtr + strlen (cPtr);

				/*
				** If dataset is a point, then get the 
				** center lat/lon coordinates for updating...  
				*/

				if (datasetPolicy->spatial_type == 1)
				{

					switch(currKey->query_type) 
					{
						case 4:
							/* Center Lat */
							north_lat = south_lat = currKey->value_real;
							break;

						case 5:
							/* Center Lon */
							east_lon = west_lon = currKey->value_real;
							break;

					}

				}


				/*
				** If dataset is rectangular, then get the 
				** four point coordinates for updating...  
				*/

				if (datasetPolicy->spatial_type == 2)
				{

					switch(currKey->query_type) 
					{
						case 14:
							/* NORTH_LAT */
							north_lat = currKey->value_real;
							break;

						case 15:
							/* SOUTH_LAT */
							south_lat = currKey->value_real;
							break;

						case 16:
							/* WEST_LON */
							west_lon = currKey->value_real;
							break;
							
						case 17:
							/* EAST_LON */
							east_lon = currKey->value_real;
							break;
					}

				}



				
				/*
				**	If dataset is a polygon, then check if this 
				**  is keyword spatial data.
				*/

				if (datasetPolicy->spatial_type == 4)
				{
					switch (currKey->query_type)
					{

						case 6:
							/*
							** Near Start Latitude.
							*/

							spData.near_s_lat = currKey->value_real;
							break;

						case 7:
							/*
							** Near Start Longitude.
							*/

							spData.near_s_lon = currKey->value_real;
							break;

						case 8:
							/*
							** Near End Latitude.
							*/

							spData.near_e_lat = currKey->value_real;
							break;

						case 9:
							/*
							** Near End Longitude.
							*/

							spData.near_e_lon = currKey->value_real;
							break;


						case 10:
							/*
							** Far Start Latitude.
							*/

							spData.far_s_lat = currKey->value_real;
							break;

						case 11:
							/*
							** Far Start Longitude.
							*/

							spData.far_s_lon = currKey->value_real;
							break;

						case 12:
							/*
							** Far End Latitude.
							*/

							spData.far_e_lat = currKey->value_real;
							break;

						case 13:
							/*
							** Far End Longitude.
							*/

							spData.far_e_lon = currKey->value_real;
							break;

						default:
							/*  
							** Not a critical spatial data keyword.
							*/
							break;
					}	
				}

				break;

			case IMS_CHAR_TYPE:
			case IMS_SYMBOL_TYPE:
			case IMS_STRING_TYPE:
			case IMS_DATETIME_TYPE:
			case IMS_DOYTIME_TYPE: 

				if (currKey->max_len == 0 || currKey->max_len > IMS_COL255_LEN) 
				{
					maxLength = IMS_COL255_LEN;
				}
				else
				{
					maxLength = currKey->max_len;
				}

				if ((int) strlen (currKey->value_string) > maxLength)
				{
					(void) ims_msg (msgStruct, IMS_ERROR,
						"Value for keyword '%s' exceeded the maximum length of %d.",
						currKey->keyword, maxLength);
					return (IMS_ERROR);
				}

				/*
				** Convert to DBMS from DOY time format.
				*/

				if (currKey->data_type == IMS_DATETIME_TYPE)
				{
					/* 
					** Get a structure for the date and then format it.
					*/
					if (ims_timeToNumericDate (msgStruct, currKey->value_string,
						(void *) &dateTbl) < IMS_OK)
					{
						(void) ims_msg (msgStruct, IMS_ERROR,
							"The keyword '%s' has an invalid date/time value of '%s'.",
							currKey->keyword, currKey->value_string);
						return (IMS_ERROR);
					}

					ims_numericDateToDBMSA((void *) &dateTbl, 
							currKey->value_string);
				}
				
				/*
				** Validate keyword values 
				*/

				keywordValue = currKey->keywordValue;

				/*
				** Scan through returned list of keyword values 
				*/

				keyFound = 0;

				/*
				** If the there are no matching values, then accept it
				*/

				if (keywordValue == NULL)
					keyFound = 1;


				while ((keywordValue != NULL) && (!keyFound))
				{
					status = strcmp(keywordValue->value, 
									currKey->value_string);

					if (status == 0)
						keyFound = 1; 

					/*
					** If already past the string in sorted list then not found
					*/

					if (status > 0)
					{
						keywordValue = NULL;
						continue;
					}

					keywordValue = keywordValue->next;
				}

				if (!keyFound)
				{
					(void) ims_msg (msgStruct, IMS_ERROR,
						"Value for keyword '%s' is not found in the keyword value table.",
						currKey->keyword);
					return (IMS_ERROR);


				}

				(void) sprintf (cPtr, "'%s', ", currKey->value_string);
				cPtr = cPtr + strlen (cPtr);

				/*
				** If this is the ascent description keyword for the 
				** spatial data, then save it.
				*/
				if ((datasetPolicy->spatial_type == 4) &&
					(currKey->query_type == 18))
				{
					spData.asc_data = currKey->value_string[0];
				}

				/*
				** If this is the keyword for the archive path, then
				** save it.
				*/
				if (currKey->query_type == 20)
				{


					datasetPolicy->localArchivePath = 
							malloc(strlen(currKey->value_string) + 1);
					if (datasetPolicy->localArchivePath == NULL)
					{
						(void) ims_msg(msgStruct, IMS_ERROR,
							"Could not store local archive path");
						return(IMS_ERROR);
					}
					strcpy(datasetPolicy->localArchivePath, 
								currKey->value_string);
				}

				/*
				** If this is the keyword for the frame status, then
				** check the status and update the ims_visible_p flag.
				*/
				if (currKey->query_type == 19)
				{
					if ((strcmp(currKey->value_string, "PLANNED") == 0) ||
						(strcmp(currKey->value_string, "SCHEDULED") == 0) ||
						(strcmp(currKey->value_string, "SCANNED") == 0) ||
						(strcmp(currKey->value_string, "ACQUIRED") == 0))

						ims_visible = 1;

					else

						ims_visible = 0;

					(void) sprintf (cPtr, "ims_visible_p = '%c', ", 
						ims_visible ? 'Y' : 'N');
					cPtr = cPtr + strlen (cPtr);


				}

				/*
				** If dataset is to record center or start/stop times
				** then check the query types of the keywords.
				*/

				if (datasetPolicy->temporal_type == 1)
				{
					if (currKey->query_type == 3)
					{
						/* 
						** Get a structure for the date and then format it.
						*/
						if (ims_timeToNumericDate (msgStruct, 
							currKey->value_string,
							(void *) &dateTbl) < IMS_OK)
						{
							(void) ims_msg (msgStruct, IMS_ERROR,
							 "keyword '%s' has an invalid date/time value of '%s'.",
								currKey->keyword, currKey->value_string);
							return (IMS_ERROR);
						}

						ims_numericDateToDBMSA((void *) &dateTbl, 
								dbTimeBuf);
						/*
						** Enter Center Time Information
						*/
							(void) sprintf (cPtr, "start_time = '%s', end_time = '%s', ", 
								dbTimeBuf, dbTimeBuf);
							cPtr = cPtr + strlen (cPtr);
					}
				}
				else if ((datasetPolicy->temporal_type == 2) &&
					((currKey->query_type == 1) ||
					 (currKey->query_type == 2)))
				{
					/* 
					** Get a structure for the date and then format it.
					*/
					if (ims_timeToNumericDate (msgStruct, 
						currKey->value_string,
						(void *) &dateTbl) < IMS_OK)
					{
						(void) ims_msg (msgStruct, IMS_ERROR,
						 "keyword '%s' has an invalid date/time value of '%s'.",
							currKey->keyword, currKey->value_string);
						return (IMS_ERROR);
					}

					ims_numericDateToDBMSA((void *) &dateTbl, 
							dbTimeBuf);
					/*
					** Enter Start Time Information
					*/

					if (currKey->query_type == 1)
					{

						(void) sprintf (cPtr, "start_time = '%s', ", 
								dbTimeBuf);
						cPtr = cPtr + strlen (cPtr);
					}

					else if (currKey->query_type == 2)
					{

							(void) sprintf (cPtr, "end_time = '%s', ", 
								dbTimeBuf);
							cPtr = cPtr + strlen (cPtr);
					}


				}
				break;

			default:
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Keyword '%s' has an invalid data type.",
					currKey->keyword);
				return (IMS_FATAL);
			}
		}
	}

	/*
	** If the spatial data flag indicates a rectange, then
	** add the four coordinates.
	*/

	if ((datasetPolicy->spatial_type == 1) ||
		(datasetPolicy->spatial_type == 2))
 	{	
		/*
		** Add the keywords into the update statement
		**/

		sprintf(cPtr, "%s = %f, ", "north_lat", north_lat);
		cPtr = cPtr + strlen(cPtr);
		sprintf(cPtr, "%s = %f, ", "south_lat", south_lat);
		cPtr = cPtr + strlen(cPtr);
		sprintf(cPtr, "%s = %f, ", "west_lon", west_lon);
		cPtr = cPtr + strlen(cPtr);
		sprintf(cPtr, "%s = %f, ", "east_lon", east_lon);
		cPtr = cPtr + strlen(cPtr);
	}



	/* 
	** If the spatial data flag indicates a polygon, then
	** perform the spatial search and add the result.
	*/

	if (datasetPolicy->spatial_type == 4)
 	{	
		if (addSpatialData(procDesc, &spData, &cPtr) < IMS_OK)
		{
		   return(IMS_FATAL);
		}
	}


	*(cPtr-2) = ' ';


	/*
	** Put the where clause into the update statement.
	*/
	(void) sprintf (cPtr, "where granule_idx = %d",
		granuleSpec->granule_idx);

	if (execCmd (procDesc) < IMS_OK)
	{
		return (IMS_FATAL);
	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Could not reinitialize the query descriptor.");
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** updateGranuleIndex ()
**
** This function will copy the granule information from the granuleIdx table
** into the oldGranuleIdx table.  The granuleIdx index then becomes the old
** granuleIdx.
******************************************************************************/

static int updateGranuleIndex (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_DATASET_POLICY *datasetPolicy;
	char buf[IMS_COL512_LEN+1];
	int status;
	int rowCount;
	int attrCount;
	int i;
	int temp_idx;
	int queryCount;
	int maxCount;

	qDesc = procDesc->qDesc;
	granuleSpec = procDesc->catReq.granuleSpec;
	datasetPolicy = procDesc->datasetPolicy;

	/*
	** Build the SQL statement as:
	** update granules_xx set keyword1 = (select keyword1 from granules_xx 
	**										where granule_idx = new_granule_idx),
	**                                   (select keyword2 from granules_xx
	**										where granule_idx = new_granule_idx)
	**					where granule_idx = old_granule_idx	
	*/

	maxCount = 0;

	/*
	** Process the table updates in multiples of 15.
	*/



	i = 0;

	
	do 
	{
		(void) sprintf (qDesc->cmd,
			"select * from %s where granule_idx = %d",
			datasetPolicy->granules_table, 
			granuleSpec->granule_idx);

		if ((status = ims_qiTblDesc(qDesc)) < IMS_OK)
		{
			return(status);
		}
		attrCount = IMS_ATTRCOUNT(qDesc);

		sprintf (qDesc->cmd, "update %s set ", datasetPolicy->granules_table);

		queryCount = 0;

		if (maxCount == attrCount)
		{
			i = attrCount;
			continue;
		}

		for (i = maxCount; i < attrCount; i++)
		{
			/*
			** Do not include granule_idx in statement
			*/


			if (strcmp(IMS_ATTRNAME(qDesc, i), "granule_idx"))
			{
				sprintf(buf, "%s = (select %s from %s where granule_idx = %d) ",
					IMS_ATTRNAME(qDesc, i), IMS_ATTRNAME(qDesc, i), 
					datasetPolicy->granules_table, granuleSpec->granule_idx);

				strcat(qDesc->cmd, buf);
				strcat(qDesc->cmd, ",");

			}

			maxCount++;

			queryCount ++;

			if (queryCount == 7)
				break;
		}


	
		qDesc->cmd[strlen(qDesc->cmd) - 1] = '\0'; /* Null Terminate */

		sprintf(&qDesc->cmd[strlen(qDesc->cmd) - 1], 
					" where granule_idx = %d", granuleSpec->oldGranule_idx);

		/*
		** Now perform the operation.
		*/

		if (ims_qiResetDesc (qDesc) < IMS_OK)
		{
			(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
				"Could not reinitialize the query descriptor.");
			return (IMS_FATAL);
		}

		if ((status = execCmd (procDesc)) < IMS_OK)
		{
			return (status);
		}

		if (ims_qiResetDesc (qDesc) < IMS_OK)
		{
			(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
				"Could not reinitialize the query descriptor.");
			return (IMS_FATAL);
		}
	} while (i < attrCount);

	/*
	** Now swap granule_idx and old granule idx
	*/
	temp_idx = granuleSpec->oldGranule_idx;
	granuleSpec->oldGranule_idx = granuleSpec->granule_idx;
	granuleSpec->granule_idx = temp_idx;


	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Could not reinitialize the query descriptor.");
		return (IMS_FATAL);
	}



	return (IMS_OK);
}


/******************************************************************************
**
** clearGranuleNameStamp ()
**
******************************************************************************/

static int clearGranuleNameStamp(
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CAT_GRANULESPEC *granuleSpec;
	FTS_DATASET_POLICY *datasetPolicy;
	int status;
	int rowCount;

	qDesc = procDesc->qDesc;
	granuleSpec = procDesc->catReq.granuleSpec;
	datasetPolicy = procDesc->datasetPolicy;

	rowCount = 0;
	while (granuleSpec != (FTS_CAT_GRANULESPEC *) NULL)
	{
		memset(granuleSpec->name_stamp, 0, sizeof(granuleSpec->name_stamp));
		rowCount++;
		(void) sprintf (qDesc->cmd,
			"update %s set name_stamp = '%s' where granule_idx = %d",
			datasetPolicy->granules_table, granuleSpec->name_stamp,
			granuleSpec->granule_idx);

		if ((status = execCmd (procDesc)) < IMS_OK)
		{
			return (status);
		}

		if (ims_qiResetDesc (qDesc) < IMS_OK)
		{
			(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
				"Could not reinitialize the query descriptor.");
			return (IMS_FATAL);
		}
		granuleSpec = granuleSpec->next;
	}

	if (rowCount <= 0)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"The granuleSpec structure was empty. Unable to update granule name.");
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getUserCapability ()
**
******************************************************************************/

static int getUserCapability (FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_CAT_USERCAP *userCap;
	int status;
	char *mask_data[]={"replace", "delete", "get", "add"};
	int maskVal;
	int dataset_list[1];


	qDesc = procDesc->qDesc;
	datasetPolicy = procDesc->datasetPolicy;
	userCap = &(procDesc->catReq.userCap);


	dataset_list[0] = datasetPolicy->dataset_idx;

	status = ims_validAcct(qDesc, procDesc->msgStruct, userCap->accountId,
		(void *) &dataset_list[0], 1, userCap->capMask);
					
    if (status  < IMS_OK)
	{
	    /* Format event message based on capability mask. */
		switch (userCap->capMask)
		{
           case 1:
			 maskVal = 0;
			 break;

           case 2:
			 maskVal = 1;
			 break;

           case 4:
			 maskVal = 2;
			 break;

           case 8:
			 maskVal = 3;
			 break;
		}

		(void) ims_msg (procDesc->msgStruct, status,
	    		"User %s, Account %s,  capabilities to %s failed.",
		        procDesc->userName,	 
			    userCap->accountId,
		        mask_data[maskVal]);
	}

	return (status);
}

/******************************************************************************
**
** addSigEvent ()
**
******************************************************************************/

static int addSigEvent (
	FTS_PROC_DESC *procDesc)
{
#ifdef SIGEVENT
	IMS_QI_DESC_OBJ *qDesc;
	FTS_CAT_STATESPEC *stateSpec;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_SENSOR_POLICY *sensorPolicy;
	int status;

	qDesc = procDesc->qDesc;
	stateSpec = &(procDesc->catReq.stateSpec);
	sensorPolicy= procDesc->sensorPolicy;
	
	(void) sprintf (qDesc->cmd,
		"fts_add_sig_event '%s', '%s', '%s'",
		procDesc->ftsSrvName, stateSpec->eventType, stateSpec->msg);

	if ((status = execCmd (procDesc)) < IMS_OK)
	{
		return (status);
	}

#endif	/* SIGEVENT */
	return (IMS_OK);
}

/******************************************************************************
**
** updateGranuleSizes ()
**
** Update the granule sizes in the granules table.
**
******************************************************************************/

static int updateGranuleSizes (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_GRANULE_DESC *granuleDesc;
	FTS_CAT_GRANULESPEC *granuleSpec;
	IMS_MSG_STRUCT *msgStruct;
	FTS_CAT_STRUCT *catReq;
	int status;

	qDesc = procDesc->qDesc;
	msgStruct = procDesc->msgStruct;
	granuleDesc = &(procDesc->granuleDesc);
	datasetPolicy = procDesc->datasetPolicy;
	granuleSpec = procDesc->catReq.granuleSpec;

	(void) sprintf (qDesc->cmd,
			"update %s set data_kbytes = %d, metadata_kbytes = %d\
			where granule_idx = %d",
			datasetPolicy->granules_table,
			INT_CEILING(granuleDesc->dataBytes, 1000),
			INT_CEILING(granuleDesc->metaBytes, 1000),
			granuleSpec->granule_idx);

	if (execCmd (procDesc) < IMS_OK)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Could not update granule table sizes\n");
		return (IMS_FATAL);
	}
	return (IMS_OK);
}


/******************************************************************************
**
** beginTransaction ()
**
** Open a transaction.
**
******************************************************************************/

static int beginTransaction (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	qDesc = procDesc->qDesc;

	(void) sprintf (qDesc->cmd, "begin transaction");

	if (execCmd (procDesc) < IMS_OK)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Could not begin transaction.");
		return (IMS_FATAL);
	}
	return (IMS_OK);
}

/******************************************************************************
**
** commitTransaction ()
**
** Commit the transaction opened by beginTransaction.
**
******************************************************************************/

static int commitTransaction (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	qDesc = procDesc->qDesc;

	(void) sprintf (qDesc->cmd, "commit transaction");

	if (execCmd (procDesc) < IMS_OK)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Could not commit transaction.");
		return (IMS_FATAL);
	}
	return (IMS_OK);
}

/******************************************************************************
**
** rollbackTransaction ()
**
** Rollback the transaction opened by beginTransaction.
**
******************************************************************************/

static int rollbackTransaction (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	qDesc = procDesc->qDesc;

	/*
	** Rollback this transaction.
	*/
	(void) sprintf (qDesc->cmd, "rollback transaction");

	if (execCmd (procDesc) < IMS_OK)
	{
		(void) ims_msg (procDesc->msgStruct, IMS_FATAL,
			"Could not rollback transaction.");
		return (IMS_FATAL);
	}
	return (IMS_OK);
}

/******************************************************************************
**
** execCmd ()
**
** Execute an SQL procedure that writes data into the catalog database.
** We don't pass a parameter, but assume that when this function is called,
** the declared static buffer 'qDesc->cmd' has been properly filled in with
** the SQL statement to be executed.
**
** THIS ROUTINE IS ONLY USED FOR EXEUTING SQL STATEMENTS THAT WILL NOT RETURN
** ROWS FROM THE DATABASE.
**
** If a deadlock occurs, reexecute the operation from the restart point.
**
******************************************************************************/

static int execCmd (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	int status;
	int severity;

	qDesc = procDesc->qDesc;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		/* Check the returned status. */
		if (status < IMS_OK)
		{
			return (status);
		}
	}

	/*
	** Check the stored procedure status returend value.
	*/
	if ((severity = processRetStatus (procDesc)) < IMS_OK)
	{
		return (severity);
	}

	if (qDesc->msgNo != 0)
	{
		return (ims_msgGetSeverity (procDesc->msgStruct));
	}

	return (IMS_OK);
}

/******************************************************************************
**
** processRetStatus ()
**
** Check the procedure returned status value.
** When status returned is not an error, then return IMS_OK.
**
******************************************************************************/

static int processRetStatus (
	FTS_PROC_DESC *procDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_SENSOR_POLICY *sensorPolicy;
	IMS_MSG_STRUCT *msgStruct;
	FTS_CLIENT_REQUEST *clientReq;
	int procReturn;
	int severity;

	qDesc = procDesc->qDesc;
	datasetPolicy = procDesc->datasetPolicy;
	sensorPolicy = procDesc->sensorPolicy;
	msgStruct = procDesc->msgStruct;
	clientReq = &(procDesc->clientReq);

	/*
	** Check to see if the Sybase procedure returned a status.  If it did
	** and it is not 0 (the OK value for a return), deal with the error.
	** Return status of less than -100 correspond to message facility
	** severity levels modulo 100.
	** Return status of less than -200 correspond to stored procedure
	** error messages.
	*/
	if (IMS_HASRETSTAT (qDesc) == IMS_TRUE)
	{
		if ((procReturn = IMS_PROCRETURN (qDesc)) != 0)
		{
			switch (procReturn)
			{
				case -101:
					severity = IMS_WARNING;
					break;

				case -102:
					severity = IMS_ERROR;
					break;

				case -103:
					severity = IMS_FATAL;
					break;

				default:
					(void) ims_msg (msgStruct, severity,
						"Procedure '%s' returned an unrecognized status of '%d'.",
						qDesc->cmd, procReturn);
					severity = IMS_ERROR;
					break;
			}
			return (severity);
		}
	}
	return (IMS_OK);
}
