static char *sccs = "@(#)ims_auxCat.c	5.1  03/18/96";
/******************************************************************************
**
** File:        ims_auxCat.c
**
** Function:    Executes the auxiliary Sybase stored procedures.
**
** Author:      Hoshyar Sayah
**
** Date:        12/7/90
**
** Modified:    12/28/93 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Added the include files string.h stdlib.h. Removed the include
**              files malloc.h and strings.h. Replaced calls to bcopy() with
**              (void) memcpy().
**
**              8/17/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              12/9/94 - S. Hardman - R1B
**              Ported to Sybase System 10. See the Open Server
**              Server-Library/C Reference Manual for more information.
**
**              03/01/95 - D. Crichton - R1B
**              Add contributor field to search granules function. 
** 
**              03/02/95 - D. Crichton - R1B
**              Updated fields for stored procedure calls. Added 
**              a function to access the dataset path policy table.
**
**              03/15/95 - D. Crichton - R1B
**              Modified getFileTypes to call new stored procedure 
**              get_file_types.  Also added new function and stored
**              procedure getKeywordValue.
**
**              03/23/95 - D. Crichton - R1B
**              Removed getServerName and getFileTypes in a effort to issolate
**              auxCat from the client library.  
**
**              8/11/95 - S. Hardman - R1B
**              Modified getSensorPolicy() to place a blank in the sensor
**              field if the query returned null.
**
** Notes:
**
** 1. FTS catalog-access description
**
** All catalog accesses made by the auxiliary programs are handled 
** by the function ims_auxCat(). The synopsis is listed below under
** Local Functions.
**
** The function determines the catalog access requested by the event
** argument.  The auxReq structure contains all necessary information
** for accessing the catalog as specified by the event.
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
#include <ims_archive.h>
#include <ims_ftsSrvCat.h> 
#include <ims_ftsSrv.h>
#include <ims_auxCat.h>
#include <ims_util.h>
/* #include <ims_krb.h> */
 
/*
** Local Functions (Entry points into this module)
**
** This function prototype can be found in ims_auxCat.h.
** It is listed here for reference.
**
**	int ims_auxCat (AUX_CAT_REQUEST *, AUX_CAT_EVENT);
*/

/*
** Local Functions
*/
static int openConnection (AUX_INFO_SPEC *);
static int getServerHost (AUX_INFO_SPEC *);
static int getStatusTable (AUX_INFO_SPEC *, FTS_STATUS_TABLE **);
static int getSensorPolicy (AUX_INFO_SPEC *, FTS_SENSOR_POLICY **);
static int getDatasetPolicy (AUX_INFO_SPEC *, FTS_DATASET_POLICY **, int *);
static int getPathPolicy (AUX_INFO_SPEC *, FTS_PATH_POLICY **, int *);
static int getFilePolicy (AUX_INFO_SPEC *, FTS_FILE_POLICY **);
static int getFormatPolicy (AUX_INFO_SPEC *, FTS_FORMAT_POLICY **, int *);
static int getKeywordPolicy (AUX_INFO_SPEC *, FTS_KEYWORD_POLICY **, int *);
static int addProcessInfo (char *, int *, char *, IMS_MSG_STRUCT *);
static int addProcessEndTime (char *, int *, IMS_MSG_STRUCT *);
static int execCmd (IMS_MSG_STRUCT *);
static int checkRetStatus (IMS_MSG_STRUCT *);
static int getKeywordValue (AUX_INFO_SPEC *, FTS_KEYWORD_VALUE **);

/*
** Definition of local constants
*/
#define BUF_SIZE 512

/*
** The command buffer. Used by all routines in this file when a
** SQL command string must be built.
*/
static char cmdBuf[BUF_SIZE];

/*
** Pointer to the catalog query descriptor used by all routines in this file 
** when a Catalog Query Interface function is called.  This descriptor is
** allocated and this pointer initialized the first time ims_ftsCat is
** called.  The pointer is initialized to (IMS_QI_DESC_OBJ *) to inidicate
** that the process has not yet logged into the catalog database.  The
** catalog connection is not closed until the CLOSE_CONNECTION event is called
** by the FTR process as it exits.
*/
static IMS_QI_DESC_OBJ *qDesc = (IMS_QI_DESC_OBJ *) NULL;

/******************************************************************************
**
** ims_auxCat ()
**
** Main function handling AUX catalog queries.
**
******************************************************************************/

int ims_auxCat (
	AUX_CAT_REQUEST *auxReq,
	AUX_CAT_EVENT event)
{
	IMS_MSG_STRUCT *msgDesc;
	int status;

	msgDesc = auxReq->auxSpec->msgDesc;

	/*
	** We must first make sure that we have a descriptor if the event is
	** anything but AUX_OPEN_CONNECTION.
	*/
	if ((qDesc == (IMS_QI_DESC_OBJ *) NULL) &&
		(event != AUX_OPEN_CONNECTION))
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"AUX_OPEN_CONNECTION must be the first auxCat event called.");
		return (IMS_FATAL);
	}

	/*
	** Now, let's do our 'catalog' business according to the type of event
	** passed into the function.  
	*/
	switch (event)
	{
	case AUX_OPEN_CONNECTION:
		status = openConnection (auxReq->auxSpec);

		/*
		** Return from here rather than break because there is no need
		** to call ims_qiCancel() before leaving as there is for all
		** other events, except AUX_CLOSE_CONNECTION
		*/
		return (status);

	case AUX_GET_SERVER_HOST:
		status = getServerHost (auxReq->auxSpec);
		break;

	case AUX_GET_STATUS_TABLE: 
		status = getStatusTable (auxReq->auxSpec,
			(FTS_STATUS_TABLE **) &(auxReq->itemList1));
		break;
			
	case AUX_GET_SENSOR_POLICY:
		status = getSensorPolicy (auxReq->auxSpec,
			(FTS_SENSOR_POLICY **) &(auxReq->itemList1));
		break;

	case AUX_GET_DATASET_POLICY:
		status = getDatasetPolicy (auxReq->auxSpec,
			(FTS_DATASET_POLICY **) &(auxReq->itemList1),
			(int *) auxReq->itemList2);
		break;

	case AUX_GET_PATH_POLICY:
		status = getPathPolicy (auxReq->auxSpec,
			(FTS_PATH_POLICY **) &(auxReq->itemList1),
			(int *) auxReq->itemList2);
		break;

	case AUX_GET_FILE_POLICY:
		status = getFilePolicy (auxReq->auxSpec,
			(FTS_FILE_POLICY **) &(auxReq->itemList1));
		break;

	case AUX_GET_FORMAT_POLICY:
		status = getFormatPolicy (auxReq->auxSpec,
			(FTS_FORMAT_POLICY **) &(auxReq->itemList1),
			(int *) auxReq->itemList2);
		break;

	case AUX_GET_KEYWORD_POLICY:
		status = getKeywordPolicy (auxReq->auxSpec,
			(FTS_KEYWORD_POLICY **) &(auxReq->itemList1),
			(int *) auxReq->itemList2);
		break;

	case AUX_GET_KEYWORD_VALUE:
		status = getKeywordValue (auxReq->auxSpec,
			(FTS_KEYWORD_VALUE **) &(auxReq->itemList1));
		break;

	case AUX_ADD_PROCESS_INFO:
		status = addProcessInfo ((char *) auxReq->itemList1, 
			(int *) auxReq->itemList2, (char *) auxReq->itemList3,
			msgDesc);
		break;

	case AUX_ADD_PROCESS_ENDTIME:
		status = addProcessEndTime ((char *) auxReq->itemList1, 
			(int *) auxReq->itemList2, msgDesc);
		break;

	case AUX_CLOSE_CONNECTION:
		/* Close the catalog connection. */ 
		ims_qiExit ();
		return (IMS_OK);

	default:
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"Invalid catalog event passed to ims_auxCat.");
		status = IMS_FATAL;
		break;
	}

	/*
	** Release all query-allocated space and re-initialize qDesc for the
	** next time in, leaving open the connection to the catalog.
	*/
	if (qDesc->dbproc != (DBPROCESS *) NULL)
	{
		(void) ims_qiCancel (qDesc);
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
** openConnection ()
**
******************************************************************************/

static int openConnection (
	AUX_INFO_SPEC *auxSpec)
{
	IMS_MSG_STRUCT *msgDesc;
	int status;

	msgDesc = auxSpec->msgDesc;

	/*
	** We only want to generate the descriptor once, the first
	** time in.  We also want to stay logged into the catalog
	** until the AUX_CLOSE_CONNECTION event is called by the
	** AUX process.
	*/

	/* 
	** Since this is the first time to access the catalog, we
	** need a query descriptor allocated.  If we can't get a
	** descriptor, return with a bad status ... we can't go on.
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		return (IMS_FATAL);
	}

	/*
	** Setup the descriptor with necessary information about this 
	** process.
	*/
	if (auxSpec->username == (char *) NULL) 
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Cannot setup catalog query descriptor.");
		return (IMS_FATAL);
	}

	IMS_SETUSER (qDesc, auxSpec->username);

	IMS_SETPSWD (qDesc, auxSpec->password);

	IMS_SETPROG (qDesc, auxSpec->programName);

	if ((int) strlen (auxSpec->catSrvName) > 0)
	{
		IMS_SETSERVER (qDesc, auxSpec->catSrvName);
	}

	if ((int) strlen (auxSpec->catDbName) > 0)
	{
		IMS_SETDBNAME (qDesc, auxSpec->catDbName);
	}

	IMS_SET_VERBOSE (qDesc, 10);

	IMS_SETCMD (qDesc, cmdBuf);

	/*
	** Login to the catalog database.
	*/
	if ((status = ims_qiLogin (qDesc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Associate the message descriptor with the dbproc so
	** the Sybase error and message handling can be performed.
	*/
	dbsetuserdata (qDesc->dbproc, (BYTE *) msgDesc);

	return (IMS_OK);
}


/******************************************************************************
**
** getServerHost ()
**
******************************************************************************/

static int getServerHost (
	AUX_INFO_SPEC *auxSpec)
{
	IMS_MSG_STRUCT *msgDesc;

	msgDesc = auxSpec->msgDesc;

	/*
	** Set up the command buffer with the stored procedure call.
	*/
	(void) sprintf (cmdBuf, "fts_get_server_host '%s'", auxSpec->ftsSrvName);

	if (execCmd (msgDesc) < IMS_OK)
	{
		return (IMS_FATAL);
	}

	/*
	** There should be exactly one host for a given FTS server.
	*/
	if (qDesc->count == 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"No entry found in the catalog database for FTS server '%s'.",
			auxSpec->ftsSrvName);
		return (IMS_ERROR);
	}
	else if (qDesc->count > 1)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Multiple host entries found in the catalog database for FTS server '%s'.", 
			auxSpec->ftsSrvName);
		return (IMS_FATAL);
	}

	/*
	** Copy in the returned data.
	*/

	(void) memcpy ((char *) auxSpec->ftsHostName, qDesc->valAddr[0], 
		qDesc->valLength[0]);
	auxSpec->ftsHostName[qDesc->valLength[0]] = '\0';

	return (IMS_OK);
}

/******************************************************************************
**
** getStatusTable ()
**
******************************************************************************/

static int getStatusTable (
	AUX_INFO_SPEC *auxSpec,
	FTS_STATUS_TABLE **statusTable)
{
	IMS_MSG_STRUCT *msgDesc;
	FTS_STATUS_TABLE *currPtr;
	DBSMALLINT index;
	int status;
	int rowCount;
	int severity;

	msgDesc = auxSpec->msgDesc;

	(void) sprintf (cmdBuf, "fts_get_status_table");

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
		** Allocate space for the FTS_STATUS_TABLE structure.
		*/
		if (rowCount == 1) 
		{
			if ((currPtr = (FTS_STATUS_TABLE *) malloc 
				((unsigned)sizeof (FTS_STATUS_TABLE))) == 
				(FTS_STATUS_TABLE *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Could not allocate memory for FTS_STATUS_TABLE structure.");
				while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
				return (IMS_FATAL);
			}
			*statusTable = currPtr;
		}

		/*
		** Copy the returned data into the structure.
		*/
		(void) memcpy ((char *) &index,
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((char *) currPtr->description[index],
			qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->description[index][qDesc->valLength[1]] = '\0';
	}

	/*
	** Check the returned status value.
	*/
	if ((severity = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return (severity);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getSensorPolicy ()
**
******************************************************************************/

static int getSensorPolicy (
	AUX_INFO_SPEC *auxSpec,
	FTS_SENSOR_POLICY **sensorPolicy)
{
	IMS_MSG_STRUCT *msgDesc;
	FTS_SENSOR_POLICY *currPtr;
	FTS_SENSOR_POLICY *prevPtr;
	char tempPlatform[IMS_COL30_LEN+1];
	char tempSensor[IMS_COL30_LEN+1];
	int status;
	int rowCount;
	int severity;

	msgDesc = auxSpec->msgDesc;

	(void) sprintf (cmdBuf, "fts_get_sensor_policy '%s'", auxSpec->ftsSrvName);

	*sensorPolicy = currPtr = prevPtr = 
		(FTS_SENSOR_POLICY *) NULL; /* Initialize to NULL */

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
		** Allocate space for the FTS_SENSOR_POLICY structure.
		**
		** lint: pointer cast may result in improper alignment
		** No problem, malloc() aligns on worst case boundary.
		*/
		if ((currPtr = (FTS_SENSOR_POLICY *) malloc 
			((unsigned)sizeof (FTS_SENSOR_POLICY))) == 
			(FTS_SENSOR_POLICY *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for FTS_SENSOR_POLICY structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			return (IMS_FATAL);
		}

		/*
		** sensorPolicy points to the first element of the list.
		*/
		if (rowCount == 1) 
		{
			*sensorPolicy = currPtr;
		}
		else
		{
			prevPtr->next = currPtr;
		}

		currPtr->next = (FTS_SENSOR_POLICY *) NULL;
		currPtr->hashPtr = (IMS_HASH_STRUCT *) NULL;

		/*
		** Copy the returned data into the structure.
		*/
		/* Copy to a temporary area so we can truncate spaces. */
		(void) memcpy (tempPlatform, qDesc->valAddr[0], 
			qDesc->valLength[0]);
		tempPlatform[qDesc->valLength[0]] = '\0';
		(void) strcpy (currPtr->platform, ims_truncStr (tempPlatform));

		if ((qDesc->valLength[1] == 0) || (qDesc->valAddr[1] == (char *) NULL))
		{
			/* Insert a blank for comparing with client request. */
			(void) strcpy (currPtr->sensor, " ");
		}
		else
		{
			/* Copy to a temporary area so we can truncate spaces. */
			(void) memcpy (tempSensor, qDesc->valAddr[1], 
				qDesc->valLength[1]);
			tempSensor[qDesc->valLength[1]] = '\0';
			(void) strcpy (currPtr->sensor, ims_truncStr (tempSensor));
		}

		prevPtr = currPtr;
	}

	/*
	** Check the returned status value.
	*/
	if ((severity = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return (severity);
	}
	return (IMS_OK);
}

/******************************************************************************
**
** getDatasetPolicy ()
**
******************************************************************************/

static int getDatasetPolicy (
	AUX_INFO_SPEC *auxSpec,
	FTS_DATASET_POLICY **policyList,
	int *listCounter)
{
	IMS_MSG_STRUCT *msgDesc;
	FTS_DATASET_POLICY *currPtr;
	FTS_DATASET_POLICY *prevPtr;
	char tempDataset[IMS_COL80_LEN+1];
	int status;
	int rowCount;
	int severity;

	msgDesc = auxSpec->msgDesc;

	(void) sprintf (cmdBuf, "fts_get_dataset_policy '%s', '%s', '%s'", 
		auxSpec->platform, auxSpec->sensor, auxSpec->ftsSrvName);

	*policyList = currPtr = prevPtr = 
		(FTS_DATASET_POLICY *) NULL; /* Initialize to NULL */

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
		** Allocate space for the FTS_DATASET_POLICY structure.
		**
		** lint: pointer cast may result in improper alignment
		** No problem, malloc() aligns on worst case boundary.
		*/
		if ((currPtr = (FTS_DATASET_POLICY *) malloc 
			((unsigned)sizeof (FTS_DATASET_POLICY))) == 
			(FTS_DATASET_POLICY *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for FTS_DATASET_POLICY structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			return (IMS_FATAL);
		}

		/*
		** policyList points to the first element of the list.
		*/
		if (rowCount == 1) 
		{
			*policyList = currPtr;
		}
		else
		{
			prevPtr->next = currPtr;
		}

		/* Initialize structure members. */
		currPtr->next = (FTS_DATASET_POLICY *) NULL;
		currPtr->filePolicy = (FTS_FILE_POLICY *) NULL;
		currPtr->keywordPolicy = (FTS_KEYWORD_POLICY *) NULL;
		currPtr->keywordPolicyLen = 0;

		/*
		** Copy the returned data into the structure.
		*/
		/* Copy to a temporary area so we can truncate spaces. */
		(void) memcpy (tempDataset, qDesc->valAddr[0], 
			qDesc->valLength[0]);
		tempDataset[qDesc->valLength[0]] = '\0';
		(void) strcpy (currPtr->dataset, ims_truncStr (tempDataset));

		(void) memcpy ((char *) &(currPtr->dataset_idx),
			qDesc->valAddr[1], qDesc->valLength[1]);

		(void) memcpy ((char *) &(currPtr->oagdr),
			qDesc->valAddr[2], qDesc->valLength[2]);

		(void) memcpy ((char *) &(currPtr->mfd_p),
			qDesc->valAddr[3], qDesc->valLength[3]);

		(void) memcpy ((char *) &(currPtr->version_p),
			qDesc->valAddr[4], qDesc->valLength[4]);

		(void) memcpy (currPtr->granules_table,
			qDesc->valAddr[5], qDesc->valLength[5]);
		currPtr->granules_table[qDesc->valLength[5]] = '\0';

		(void) memcpy (currPtr->load_program, qDesc->valAddr[6], 
			qDesc->valLength[6]);
		currPtr->load_program[qDesc->valLength[6]] = '\0';

		(void) memcpy (currPtr->dist_program, qDesc->valAddr[7], 
			qDesc->valLength[7]);
		currPtr->dist_program[qDesc->valLength[7]] = '\0';
		
		(void) memcpy ((char *) &(currPtr->local_archive_p),
			qDesc->valAddr[8], qDesc->valLength[8]);

		(void) memcpy ((char *) &(currPtr->spatial_type),
			qDesc->valAddr[9], qDesc->valLength[9]);

		(void) memcpy ((char *) &(currPtr->temporal_type),
			qDesc->valAddr[10], qDesc->valLength[9]);


		prevPtr = currPtr;
	}

	*listCounter = rowCount;

	/*
	** Check the returned status value
	*/
	if ((severity = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return (severity);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getPathPolicy ()
**
******************************************************************************/

static int getPathPolicy (
	AUX_INFO_SPEC *auxSpec,
	FTS_PATH_POLICY **pathList,
	int *pathCount)
{
	IMS_MSG_STRUCT *msgDesc;
	FTS_PATH_POLICY *currPtr;
	FTS_PATH_POLICY *prevPtr;
	char tempPath[IMS_COL255_LEN+1];
	int status;
	int rowCount;
	int severity;


	msgDesc = auxSpec->msgDesc;

	(void) sprintf (cmdBuf, "fts_get_path_policy %d",
					auxSpec->dataset_idx);

	*pathList = currPtr = prevPtr = 
		(FTS_PATH_POLICY *) NULL; /* Initialize to NULL */

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
		** Allocate space for the FTS_PATH_POLICY structure.
		**
		** lint: pointer cast may result in improper alignment
		** No problem, malloc() aligns on worst case boundary.
		*/
		if ((currPtr = (FTS_PATH_POLICY *) malloc 
			((unsigned) sizeof (FTS_PATH_POLICY))) ==
			(FTS_PATH_POLICY *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for FTS_PATH_POLICY structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			return (IMS_FATAL);
		}

		/*
		** pathList points to the first element of the list.
		*/
		if (rowCount == 1)
		{
			*pathList = currPtr;		/* Head of linked list */
		}
		else
		{
			prevPtr->next = currPtr;	/* Tail of linked list */
		}

		currPtr->next = (FTS_PATH_POLICY *) NULL;

		/*
		** Copy the returned data into the structure.
		*/
		/* Copy to a temporary area so we can truncate spaces. */
		(void) memcpy (tempPath, qDesc->valAddr[0],
			qDesc->valLength[0]);
		tempPath[qDesc->valLength[0]] = '\0';
		(void) strcpy (currPtr->path, ims_truncStr (tempPath));

		if ((qDesc->valLength[1] == 0) || (qDesc->valAddr[1] == (char *) NULL))
		{	
				currPtr->start_granule = -1;
		}
		else
		{	
			(void) memcpy ((int *) &(currPtr->start_granule),
				qDesc->valAddr[1], qDesc->valLength[1]);
		}

		if ((qDesc->valLength[2] == 0) || (qDesc->valAddr[2] == (char *) NULL))
		{	
				currPtr->end_granule = -1;
		}
		else
		{	
			(void) memcpy ((int *) &(currPtr->end_granule),
				qDesc->valAddr[2], qDesc->valLength[2]);
		}	

		prevPtr = currPtr;
	}

	/* Set the value of the file counter. */
	*pathCount = rowCount;

	/*
	** Check the returned status value.
	*/
	if ((severity = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return (severity);
	}

	return (IMS_OK);

}


/******************************************************************************
**
** getFilePolicy ()
**
******************************************************************************/

static int getFilePolicy (
	AUX_INFO_SPEC *auxSpec,
	FTS_FILE_POLICY **fileList)
{
	IMS_MSG_STRUCT *msgDesc;
	FTS_FILE_POLICY *currPtr;
	FTS_FILE_POLICY *prevPtr;
	char tempFormat[IMS_COL10_LEN+1];
	int status;
	int rowCount;
	int severity;

	msgDesc = auxSpec->msgDesc;

	(void) sprintf (cmdBuf, "fts_get_file_policy %d", auxSpec->dataset_idx);

	*fileList = currPtr = prevPtr = 
		(FTS_FILE_POLICY *) NULL; /* Initialize to NULL */

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
		** Allocate space for the FTS_FILE_POLICY structure.
		**
		** lint: pointer cast may result in improper alignment
		** No problem, malloc() aligns on worst case boundary.
		*/
		if ((currPtr = (FTS_FILE_POLICY *) malloc
			((unsigned) sizeof (FTS_FILE_POLICY))) ==
			(FTS_FILE_POLICY *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for FTS_FILE_POLICY structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			return (IMS_FATAL);
		}
		
		/*
		** fileList points to the first element of the list.
		*/
		if (rowCount == 1)
		{
			*fileList = currPtr;
		}
		else
		{
			prevPtr->next = currPtr;
		}

		currPtr->next = (FTS_FILE_POLICY *) NULL;
		currPtr->formatPolicy = (void *) NULL;
		currPtr->fileCount = 0;

		/*
		** Copy the returned data into the structure.
		*/
		/* Copy to a temporary area so we can truncate spaces. */
		(void) memcpy (tempFormat, qDesc->valAddr[0], 
			qDesc->valLength[0]);
		tempFormat[qDesc->valLength[0]] = '\0';
		(void) strcpy (currPtr->format, ims_truncStr (tempFormat));

		prevPtr = currPtr;
	}

	/*
	** Check the returned status value.
	*/
	if ((severity = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return (severity);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getFormatPolicy ()
**
******************************************************************************/

static int getFormatPolicy (
	AUX_INFO_SPEC *auxSpec,
	FTS_FORMAT_POLICY **formatList,
	int *fileCount)
{
	IMS_MSG_STRUCT *msgDesc;
	FTS_FORMAT_POLICY *currPtr;
	FTS_FORMAT_POLICY *prevPtr;
	char tempExtension[IMS_COL10_LEN+1];
	int status;
	int rowCount;
	int severity;

	msgDesc = auxSpec->msgDesc;

	(void) sprintf (cmdBuf, "fts_get_format_policy %d, '%s'",
		auxSpec->dataset_idx, auxSpec->format);

	*formatList = currPtr = prevPtr = 
		(FTS_FORMAT_POLICY *) NULL; /* Initialize to NULL */

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
		** Allocate space for the FTS_FORMAT_POLICY structure.
		**
		** lint: pointer cast may result in improper alignment
		** No problem, malloc() aligns on worst case boundary.
		*/
		if ((currPtr = (FTS_FORMAT_POLICY *) malloc 
			((unsigned) sizeof (FTS_FORMAT_POLICY))) ==
			(FTS_FORMAT_POLICY *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for FTS_FORMAT_POLICY structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			return (IMS_FATAL);
		}

		/*
		** formatList points to the first element of the list.
		*/
		if (rowCount == 1)
		{
			*formatList = currPtr;
		}
		else
		{
			prevPtr->next = currPtr;
		}

		currPtr->next = (FTS_FORMAT_POLICY *) NULL;

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


		prevPtr = currPtr;
	}

	/* Set the value of the file counter. */
	*fileCount = rowCount;

	/*
	** Check the returned status value.
	*/
	if ((severity = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return (severity);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getKeywordPolicy ()
**
******************************************************************************/

static int getKeywordPolicy (
	AUX_INFO_SPEC *auxSpec,
	FTS_KEYWORD_POLICY **keywordList,
	int *listCounter)
{
	IMS_MSG_STRUCT *msgDesc;
	FTS_KEYWORD_POLICY *currPtr;
	FTS_KEYWORD_POLICY *prevPtr;
	char tempKeyword[IMS_COL30_LEN+1];
	int status;
	int rowCount;
	int keywordCount;
	int severity;

	msgDesc = auxSpec->msgDesc;

	(void) sprintf (cmdBuf, "fts_get_keyword_policy %d", auxSpec->dataset_idx);

	*keywordList = currPtr = prevPtr = 
		(FTS_KEYWORD_POLICY *) NULL; /* Initialize to NULL */

	keywordCount = 0;
	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (ims_msgGetSeverity (msgDesc));
		}

		/* If ENDOFQUERY, we want to finish out command and return. */
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/* A row has been returned. */
		rowCount++;

		if (rowCount == 1)
		{
			/*
			** The first data returned is the number of keyword
			** policy rows.
			*/
			(void) memcpy ((char *) &keywordCount,
				qDesc->valAddr[0], qDesc->valLength[0]);

			if (keywordCount <= 0) 
			{
				/* Keyword policy list is empty. */
				while (ims_qiNextRow (qDesc) !=
					IMS_ENDOFTRANSACTION) {}
				*listCounter = 0;
				return (IMS_OK);
			}

			/*
			** Allocate space for the FTS_KEYWORD_POLICY structure.
			**
			** lint: pointer cast may result in improper alignment
			** No problem, malloc() aligns on worst case boundary.
			*/
			if ((currPtr = (FTS_KEYWORD_POLICY *) calloc 
				((unsigned)keywordCount,
				(unsigned)sizeof (FTS_KEYWORD_POLICY))) 
				== (FTS_KEYWORD_POLICY *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Could not allocate memory for FTS_KEYWORD_POLICY structure.");
				while (ims_qiNextRow (qDesc) !=
					IMS_ENDOFTRANSACTION) {}
				return (IMS_FATAL);
			}

			/* keywordList points to the first element of the list. */
			*keywordList = currPtr;

			continue;
		}

		/*
		** Copy the returned data into the structure.
		*/
		/* Copy to a temporary area so we can truncate spaces. */
		(void) memcpy (tempKeyword, qDesc->valAddr[0], 
			qDesc->valLength[0]);
		tempKeyword[qDesc->valLength[0]] = '\0';
		(void) strcpy (currPtr->keyword, ims_truncStr (tempKeyword));

		(void) memcpy ((char *) &(currPtr->keyword_idx),
			qDesc->valAddr[1], qDesc->valLength[1]);

		(void) memcpy ((char *) &(currPtr->data_type),
			qDesc->valAddr[2], qDesc->valLength[2]);

		if ((qDesc->valLength[3] == 0) || (qDesc->valAddr[3] == (char *) NULL))
		{
			currPtr->max_len = 0;
		}
		else
		{
			(void) memcpy ((char *) &(currPtr->max_len),
				qDesc->valAddr[3], qDesc->valLength[3]);
		}

		if ((qDesc->valLength[4] == 0) || (qDesc->valAddr[4] == (char *) NULL))
		{
			currPtr->min_val = 0;
		}
		else
		{
			(void) memcpy ((char *) &(currPtr->min_val),
				qDesc->valAddr[4], qDesc->valLength[4]);
		}

		if ((qDesc->valLength[5] == 0) || (qDesc->valAddr[5] == (char *) NULL))
		{
			currPtr->max_val = 0;
		}
		else
		{
			(void) memcpy ((char *) &(currPtr->max_val),
				qDesc->valAddr[5], qDesc->valLength[5]);
		}

		(void) memcpy ((char *) &(currPtr->significance),
			qDesc->valAddr[6], qDesc->valLength[6]);

		(void) memcpy ((char *) &(currPtr->position),
			qDesc->valAddr[7], qDesc->valLength[7]);

		(void) memcpy ((char *) &(currPtr->query_type),
			qDesc->valAddr[8], qDesc->valLength[8]);

		/* Increment currPtr. */
		prevPtr = currPtr;
		currPtr++;
		prevPtr->next = currPtr;
	}
	prevPtr->next = (FTS_KEYWORD_POLICY *) NULL;

	if ((*listCounter = rowCount-1) != keywordCount)
	{
		(void) free ((FTS_KEYWORD_POLICY *)(*keywordList));
		*keywordList = (FTS_KEYWORD_POLICY *) NULL;
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"Number of keyword policies returned does not match number of rows.");
		return (IMS_FATAL);
	}

	/*
	** Check the returned status value.
	*/
	if ((severity = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return (severity);
	}

	return (IMS_OK);
}


/******************************************************************************
**
** getKeywordValue ()
**
******************************************************************************/

static int getKeywordValue (
	AUX_INFO_SPEC *auxSpec,
	FTS_KEYWORD_VALUE **keywordValue)
{
	IMS_MSG_STRUCT *msgDesc;
	FTS_KEYWORD_VALUE *currPtr;
	FTS_KEYWORD_VALUE *prevPtr;
	char tempValue[IMS_COL255_LEN+1];
	int status;
	int rowCount;
	int severity;

	msgDesc = auxSpec->msgDesc;

	(void) sprintf (cmdBuf, "fts_get_keyword_value %d", auxSpec->keyword_idx);

	*keywordValue = currPtr = prevPtr = 
		(FTS_KEYWORD_VALUE *) NULL; /* Initialize to NULL */

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
		** Allocate space for the FTS_KEYWORD_VALUE structure.
		**
		** lint: pointer cast may result in improper alignment
		** No problem, malloc() aligns on worst case boundary.
		*/
		if ((currPtr = (FTS_KEYWORD_VALUE *) malloc 
			((unsigned)sizeof (FTS_KEYWORD_VALUE))) == 
			(FTS_KEYWORD_VALUE *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for FTS_KEYWORD_VALUE structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			return (IMS_FATAL);
		}

		/*
		** policyList points to the first element of the list.
		*/
		if (rowCount == 1) 
		{
			*keywordValue = currPtr;
		}
		else
		{
			prevPtr->next = currPtr;
		}

		/* Initialize structure members. */
		currPtr->next = (FTS_KEYWORD_VALUE *) NULL;
		currPtr->keyword_idx = auxSpec->keyword_idx;

		/*
		** Copy the returned data into the structure.
		*/

		/* Copy to a temporary area so we can truncate spaces. */

		(void) memcpy (tempValue, qDesc->valAddr[0], 
			qDesc->valLength[0]);
		tempValue[qDesc->valLength[0]] = '\0';
		(void) strcpy (currPtr->value, ims_truncStr (tempValue));

		prevPtr = currPtr;
	}

	/*
	** Check the returned status value
	*/
	if ((severity = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return (severity);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** addProcessInfo () 
**
******************************************************************************/

static int addProcessInfo (
	char *host,
	int *pid,
	char *name,
	IMS_MSG_STRUCT *msgDesc)
{
	unsigned char missionIndIdx;

	/*
	** First we need to get the missionIdx for the mission independent 
	** data types. 
	** MissionAcr 'MID' with scId '1' is reserved for mission independent 
	** data types.
	*/
	(void) sprintf (cmdBuf, "fts_getMissionIdx 'MID', 1");

	if (execCmd (msgDesc) < IMS_OK)
	{
		return (IMS_FATAL);
	}

	/*
	** There should be exactly one missionIdx for mission and scId.
	*/
	if (qDesc->count == 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"MissionIdx not found for mission 'MID' and scId '1'.");
		return (IMS_FATAL);
	}
	else if (qDesc->count > 1)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Multiple MissionIdx found for mission 'MID' and scId '1'.");
		return (IMS_FATAL);
	}

	/*
	** Copy in the returned data
	*/
	(void) memcpy ((char *) &(missionIndIdx), qDesc->valAddr[0], 
		qDesc->valLength[0]);

	(void) sprintf (cmdBuf, "fts_addProcessInfo '%s', %d, %d, '%s'", 
		host, *pid, missionIndIdx, name);

	if (execCmd (msgDesc) < IMS_OK)
	{
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** addProcessEndTime ()
**
******************************************************************************/

static int addProcessEndTime (
	char *host,
	int *pid,
	IMS_MSG_STRUCT *msgDesc)
{
	(void) sprintf (cmdBuf, "fts_addProcessEndTime '%s', %d", 
		host, *pid);

	if (execCmd (msgDesc) < IMS_OK)
	{
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
** the declared static buffer 'cmdBuf' has been properly filled in with
** the SQL statement to be executed.
**
** THIS ROUTINE IS ONLY USED FOR EXEUTING SQL STATEMENTS THAT WILL NOT RETURN
** ROWS FROM THE DATABASE.
**
** If a deadlock occurs, reexecute the operation from the restart point.
**
******************************************************************************/

static int execCmd (
	IMS_MSG_STRUCT *msgDesc)
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

	/*
	** Check the stored procedure status returned value.
	*/
	if ((severity = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return (severity);
	}

	if (qDesc->msgNo != 0)
	{
		return (ims_msgGetSeverity (msgDesc));
	}

	return (IMS_OK);
}

/******************************************************************************
**
** checkRetStatus ()
**
** Check the procedure returned status value.
** When status returned is not an error, then return IMS_OK.
**
******************************************************************************/

static int checkRetStatus (
	IMS_MSG_STRUCT *msgDesc)
{
	int procReturn;
	int severity;

	/*
	** Check to see if the Sybase procedure returned a status.  If it did
	** and it is not 0 (the OK value for a return), deal with the error.
	** Return status of less than -100 correspond to message facility
	** severity levels modulo 100.
	*/
	if (IMS_HASRETSTAT (qDesc) == IMS_TRUE)
	{
		if ((procReturn = IMS_PROCRETURN (qDesc)) < 0)
		{
			if (procReturn == -103)
			{
				severity = IMS_FATAL;
			}
			else if (procReturn == -102)
			{
				severity = IMS_ERROR;
			}
			else if (procReturn == -101)
			{
				severity = IMS_WARNING;
			}
			else
			{
				severity = IMS_ERROR;
			}
			(void) ims_msg (msgDesc, severity, 
				"Sybase procedure '%s' returned a status of %ld",
				qDesc->cmd, procReturn);
			return (severity);
		}
	}

	return (IMS_OK);
}
