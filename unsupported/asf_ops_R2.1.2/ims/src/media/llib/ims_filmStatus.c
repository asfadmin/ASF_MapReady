static char *sccs = "@(#)ims_filmStatus.c	5.2  08/28/96";
/******************************************************************************
**
** File:        ims_filmStatus.c
**
** Function:    A routine that updates the film generation job status
**              and cleans up any associated staged files.
**
** Author:      S. Hardman and D. Pass
**
** Date:        9/26/95
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_util.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_media.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_media.h.
** They are listed here for reference.
**
**  int ims_filmStatus (IMS_MSG_STRUCT *, char *, DBINT, DBSMALLINT,
**		DBSMALLINT, DBCHAR *);
*/

/*
** Local Functions.
*/
static int openConnection (IMS_MSG_STRUCT *, MEDIA_USER_SPEC *);
static FILM_QUEUE_LIST *getQueueList (IMS_MSG_STRUCT *);
static int updateStatus(IMS_MSG_STRUCT *, FILM_QUEUE_LIST *,
	FILM_QUEUE_LIST *, DBSMALLINT, DBCHAR *);
static FILM_FILE_LIST *getFileList (IMS_MSG_STRUCT *, FILM_QUEUE_LIST *,
	DBCHAR *);
static int getGranuleInfo (IMS_MSG_STRUCT *, FILM_QUEUE_LIST *, char *);
static int getStageArea (IMS_MSG_STRUCT *, char *, char *);
static void deleteFiles (IMS_MSG_STRUCT *, FILM_QUEUE_LIST *,
	FILM_FILE_LIST *, char *, int *);
static int execCmd (IMS_MSG_STRUCT *);
static int checkRetStatus (IMS_MSG_STRUCT *);
static void freeQueueList (FILM_QUEUE_LIST *);
static void freeFileList (FILM_FILE_LIST *);

/*
** Global Variables.
*/
static IMS_QI_DESC_OBJ *qDesc = (IMS_QI_DESC_OBJ *) NULL;
static char cmdBuf[1024];

/* *************************************************************
**
** subr ims_filmStatus ()
**
** This function is called by ims_filmStatusRet (a stand-alone
** program) or a program written by the operator after the TTDL
** (Things-To-Do-List) has been processed.  The operator inputs the
** new status, which is put into the database, and the aprropriate
** files are deleted from the stage area.
**
**************************************************************** */

int ims_filmStatus (
	IMS_MSG_STRUCT *msgDesc,
	char *mediaUserSpec,
	DBINT orderId,
	DBSMALLINT itemId,
	DBSMALLINT inStatus,
	DBCHAR *opComment)
{
	MEDIA_USER_SPEC *userSpec;
	FILM_FILE_LIST *fileList;
	FILM_QUEUE_LIST *queueList;
	FILM_QUEUE_LIST *currList;
	FILM_QUEUE_LIST *targetItem;
	char stageHost[IMS_HOST_LEN+1];
	char stagePath[IMS_PATH_LEN+1];
	char format[IMS_COL10_LEN+1];
	int status;
	int deleteFilesFlag;
	int filesDeleted;   /* Number of files deleted from stage area. */

	/*
	** lint: pointer cast may result in improper alignment
	** The structure definitions are identical.
	*/
	userSpec = (MEDIA_USER_SPEC *) mediaUserSpec;

	queueList = (FILM_QUEUE_LIST *) NULL;
	currList = (FILM_QUEUE_LIST *) NULL;
	targetItem = (FILM_QUEUE_LIST *) NULL;
	fileList = (FILM_FILE_LIST *) NULL;

	/*
	** Open the database server connection.
	*/
	if ((status = openConnection (msgDesc, userSpec)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Get the film queue list.
	*/
	if ((queueList = getQueueList (msgDesc)) == (FILM_QUEUE_LIST *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain the film queue list.");
		status = IMS_ERROR;
		goto ERROR;
	}

	/*
	** Find the current order item in the film queue list.
	*/
	currList = queueList;
	while (currList != (FILM_QUEUE_LIST *) NULL)
	{
		if ((orderId == currList->order_id) &&
			(itemId == currList->item_id))
		{
			targetItem = currList;
			break;
		}

		currList = currList->next;
	}

	/*
	** See if we got a match.
	*/
	if (currList == (FILM_QUEUE_LIST *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not find an entry in the film queues matching "
			"item '%d' of order '%d' with a status of IN-LASER OR IN-FIRE.",
			itemId, orderId);
		status = IMS_ERROR;
		goto ERROR;
	}

	/*
	** Update the item's status.
	*/
	if ((status = updateStatus (msgDesc, targetItem, queueList,
		inStatus, opComment)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not update the film status for item '%d' of order '%d'.",
			targetItem->item_id, targetItem->order_id);
		goto ERROR;
	}

	/*
	** Get the path for the staging area.
	*/
	if ((status = getStageArea (msgDesc, stageHost, stagePath)) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** Get the specific granule information.
	*/
	if ((status = getGranuleInfo (msgDesc, targetItem, format)) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** Get the list of files that make up this product.
	** Since only the CEOS format is supported we are looking
	** for the leader and data files.
	*/
	if ((fileList = getFileList (msgDesc, targetItem,
		format)) == (FILM_FILE_LIST *) NULL)
	{
		status = IMS_ERROR;
		goto ERROR;
	}

	/*
	** Determine if we should delete the staged files.
	** If an item in our list is using the same granule,
	** and this item is not the current item than do not
	** delete the staged files.
	*/
	deleteFilesFlag = IMS_TRUE;
	if (strcmp (targetItem->queue_type, "LASER") == 0)
	{
		currList = queueList;
		while (currList != (FILM_QUEUE_LIST *) NULL)
		{
			/* Match the granule identifiers.*/
			if ((targetItem->p_dataset_idx == currList->p_dataset_idx) &&
				(targetItem->p_granule_idx == currList->p_granule_idx))
			{
				/* See if it is not the same order item. */
				if ((targetItem->order_id != currList->order_id) ||
					(targetItem->item_id != currList->item_id))
				{
					deleteFilesFlag = IMS_FALSE;
					break;
				}
			}

			currList = currList->next;
		}
	}
	/*
	** The fire queue item is handled the same way as the laser
	** queue item above until the corresponding film list
	** generation function collapses multiple requests for the
	** same granule into one request.  In this case all matching
	** items had their statuses updated so delete the files.
	*/
	else if (strcmp (targetItem->queue_type, "FIRE") == 0)
	{
		currList = queueList;
		while (currList != (FILM_QUEUE_LIST *) NULL)
		{
			/* Match the granule identifiers.*/
			if ((targetItem->p_dataset_idx == currList->p_dataset_idx) &&
				(targetItem->p_granule_idx == currList->p_granule_idx))
			{
				/* See if it is not the same order item. */
				if ((targetItem->order_id != currList->order_id) ||
					(targetItem->item_id != currList->item_id))
				{
#ifdef COLLAPSED
					/*
					** If the matching item is in the laser queue
					** then we do not want to delete the files.
					*/
					if (strcmp (currList->queue_type, "LASER") == 0)
					{
#endif /* COLLAPSED */
						deleteFilesFlag = IMS_FALSE;
						break;
#ifdef COLLAPSED
					}
#endif /* COLLAPSED */
				}
			}

			currList = currList->next;
		}
	}

	/*
	** Delete the files in the staging area.
	*/
	if (deleteFilesFlag == IMS_TRUE)
	{
		deleteFiles (msgDesc, targetItem, fileList,
			stagePath, &filesDeleted);

		if (filesDeleted > 0)
		{
			(void) ims_msg (msgDesc, IMS_INFO,
				"Staged files for granule '%s' were "
				"successfully removed.",
				targetItem->p_granule_name);
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_WARNING,
				"Staged files for granule '%s' were "
				"not available for removal.",
				targetItem->p_granule_name);
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"Staged files for granule '%s' were "
			"not eligible for deletion.",
			targetItem->p_granule_name);
	}

	(void) ims_qiFreeDesc (qDesc);
	freeQueueList (queueList);
	freeFileList (fileList);
	return (IMS_OK);

ERROR:
	(void) ims_qiFreeDesc (qDesc);
	freeQueueList (queueList);
	freeFileList (fileList);

	return (status);
}   /*  ims_filmStatus   */

/* *************************************************************
**
** subr openConnection ()
**
** Open database server connection.
**
**************************************************************** */

static int openConnection (
	IMS_MSG_STRUCT *msgDesc,
	MEDIA_USER_SPEC *userSpec)
{
	int status;

	/*
	** Allocate a query descriptor.
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		return (IMS_FATAL);
	}

	/*
	** Setup the descriptor with necessary information about this
	** process.
	*/
	IMS_SETUSER (qDesc, userSpec->username);

	IMS_SETPSWD (qDesc, userSpec->password);

	IMS_SETPROG (qDesc, userSpec->program);

	if (userSpec->server != (char *) NULL)
	{
		IMS_SETSERVER (qDesc, userSpec->server);
	}

	if (userSpec->database != (char *) NULL)
	{
		IMS_SETDBNAME (qDesc, userSpec->database);
	}

	IMS_SET_VERBOSE (qDesc, 10);

	IMS_SETCMD (qDesc, cmdBuf);

	/*
	** Login to the catalog database.
	*/
	if ((status = ims_qiLogin (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not login to the database server.");
		(void) ims_qiFreeDesc (qDesc);
		return (status);
	}

	/*
	** Associate the message descriptor with the dbproc so
	** the Sybase error and message handling can be performed.
	*/
	IMS_SET_USERDATA (qDesc);

	return (IMS_OK);
}

/* *************************************************************
**
** subr getQueueList ()
**
** Query for all film items from the fire_queue and laser_queue
** tables that have a status of IN-FIRE or IN-LASER respectively.
**
**************************************************************** */

static FILM_QUEUE_LIST *getQueueList (
	IMS_MSG_STRUCT *msgDesc)
{
	FILM_QUEUE_LIST *queueList;
	FILM_QUEUE_LIST *currPtr;
	FILM_QUEUE_LIST *prevPtr;
	int status;
	int rowCount;

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not reset the query descriptor.");
		return ((FILM_QUEUE_LIST *) NULL);
	}

	/*
	** Set up the command buffer with SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"select queue_type = 'LASER', l.order_id, l.item_id, "
		"o.p_dataset_idx, o.p_granule_idx, o.p_granule_name, "
		"d.granules_table "
		"from laser_queue l, order_item o, dataset_policy d "
		"where l.status = 2 "
		"and l.order_id = o.order_id "
		"and l.item_id = o.item_id "
		"and o.p_dataset_idx = d.dataset_idx "
		"\n"
		"select queue_type = 'FIRE', f.order_id, f.item_id, "
		"o.p_dataset_idx, o.p_granule_idx, o.p_granule_name, "
		"d.granules_table "
		"from fire_queue f, order_item o, dataset_policy d "
		"where f.status = 2 "
		"and f.order_id = o.order_id "
		"and f.item_id = o.item_id "
		"and o.p_dataset_idx = d.dataset_idx");

	/*
	** Process the result rows for this query.
	*/
	prevPtr = NULL;
	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return ((FILM_QUEUE_LIST *) NULL);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** Allocate space for the FILM_QUEUE_LIST structure.
		*/
		if ((currPtr = (FILM_QUEUE_LIST *) malloc
			((size_t) sizeof (FILM_QUEUE_LIST))) ==
			(FILM_QUEUE_LIST *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for FILM_QUEUE_LIST structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			return ((FILM_QUEUE_LIST *) NULL);
		}

		/*
		** queueList points to the first element of the list.
		*/
		if (++rowCount == 1)
		{
			queueList = currPtr;
		}
		else
		{
			prevPtr->next = currPtr;
		}

		currPtr->next = (FILM_QUEUE_LIST *) NULL;

		/*
		** Copy in the returned data.
		*/

		/* queue_type */
		(void) memcpy (currPtr->queue_type,
			IMS_VALUE (qDesc, 0), IMS_VALUELENGTH (qDesc, 0));
		currPtr->queue_type[IMS_VALUELENGTH (qDesc, 0)] = '\0';

		/* order_id */
		(void) memcpy (&(currPtr->order_id),
			IMS_VALUE (qDesc, 1), IMS_VALUELENGTH (qDesc, 1));

		/* item_id */
		(void) memcpy (&(currPtr->item_id),
			IMS_VALUE (qDesc, 2), IMS_VALUELENGTH (qDesc, 2));

		/* p_dataset_idx */
		(void) memcpy (&(currPtr->p_dataset_idx),
			IMS_VALUE (qDesc, 3), IMS_VALUELENGTH (qDesc, 3));

		/* p_granule_idx */
		(void) memcpy (&(currPtr->p_granule_idx),
			IMS_VALUE (qDesc, 4), IMS_VALUELENGTH (qDesc, 4));

		/* p_granule_name */
		(void) memcpy (currPtr->p_granule_name,
			IMS_VALUE (qDesc, 5), IMS_VALUELENGTH (qDesc, 5));
		currPtr->p_granule_name[IMS_VALUELENGTH (qDesc, 5)] = '\0';

		/* granules_table */
		(void) memcpy (currPtr->granules_table,
			IMS_VALUE (qDesc, 6), IMS_VALUELENGTH (qDesc, 6));
		currPtr->granules_table[IMS_VALUELENGTH (qDesc, 6)] = '\0';

		prevPtr = currPtr;
	}

	/*
	** Check to see if any rows were returned.
	*/
	if (rowCount <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The film queues do not contain any items with a "
			"status of IN-LASER OR IN-FIRE.");
		return ((FILM_QUEUE_LIST *) NULL);
	}

	return (queueList);
} /* getQueueList */

/* *************************************************************
**
** subr updateStatus ()
**
** Update the status value for the current film job.
**
*************************************************************** */

static int updateStatus (
	IMS_MSG_STRUCT *msgDesc,
	FILM_QUEUE_LIST *targetItem,
	FILM_QUEUE_LIST *queueList,
	DBSMALLINT inStatus,
	DBCHAR *opComment)
{
	FILM_QUEUE_LIST *currList;
	int status;

	currList = queueList;

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not reset the query descriptor.");
		return (status);
	}

	/*
	** Update the status of the laser_queue item.
	*/
	if (strcmp (targetItem->queue_type, "LASER") == 0)
	{
		/*
		** Set up the command buffer with the stored procedure call.
		** Only send the comment if it is not NULL.
		*/
		if ((opComment == (DBCHAR *) NULL) || (strlen (opComment) == 0))
		{
			(void) sprintf (cmdBuf,
				"med_update_film_status '%s', %ld, %d, %d",
				targetItem->queue_type, targetItem->order_id,
				targetItem->item_id, inStatus);
		}
		else
		{
			(void) sprintf (cmdBuf,
				"med_update_film_status '%s', %ld, %d, %d, '%s'",
				targetItem->queue_type, targetItem->order_id,
				targetItem->item_id, inStatus, opComment);
		}

		/*
		** Execute the SQL statement.
		*/
		if ((status = execCmd (msgDesc)) < IMS_OK)
		{
			return (status);
		}

		(void) ims_msg (msgDesc, IMS_INFO,
			"Updated the laser_queue status to '%s' for "
			"item '%d' of order '%d'.",
			ims_filmStatusDesc (inStatus),
			targetItem->item_id, targetItem->order_id);
	}
	/*
	** Update the status of the fire_queue item.
	*/
	else if (strcmp (targetItem->queue_type, "FIRE") == 0)
	{
#ifdef COLLAPSED
		/*
		** Update the status for all items that required a film
		** for the target granule. This only pertains when the
		** corresponding film list generation function collapses
		** multiple requests for the same granule into one request.
		*/
		while (currList != (FILM_QUEUE_LIST *) NULL)
		{
			if ((targetItem->p_dataset_idx == currList->p_dataset_idx) &&
				(targetItem->p_granule_idx == currList->p_granule_idx))
			{
#endif /* COLLAPSED */

				/*
				** Set up the command buffer with the stored procedure call.
				** Only send the comment if it is not NULL.
				*/
				if ((opComment == (DBCHAR *) NULL) ||
					(strlen (opComment) == 0))
				{
					(void) sprintf (cmdBuf,
						"med_update_film_status '%s', %ld, %d, %d",
						targetItem->queue_type, targetItem->order_id,
						targetItem->item_id, inStatus);
				}
				else
				{
					(void) sprintf (cmdBuf,
						"med_update_film_status '%s', %ld, %d, %d, '%s'",
						targetItem->queue_type, targetItem->order_id,
						targetItem->item_id, inStatus, opComment);
				}

				/*
				** Execute the SQL statement.
				*/
				if ((status = execCmd (msgDesc)) < IMS_OK)
				{
					return (status);
				}

				(void) ims_msg (msgDesc, IMS_INFO,
					"Updated the fire_queue status to '%s' for "
					"item '%d' of order '%d'.",
					ims_filmStatusDesc (inStatus),
					targetItem->item_id, targetItem->order_id);
#ifdef COLLAPSED
			}

			currList = currList->next;
		}
#endif /* COLLAPSED */
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"An invalid queue type of '%s' was encountered.",
			targetItem->queue_type);
		return (IMS_ERROR);
	}

	return (IMS_OK);
} /* updateStatus */

/* **********************************************************
**
** getGranuleInfo ()
**
** Get the necessary information from the granules_x table.
**
************************************************************ */

static int getGranuleInfo (
	IMS_MSG_STRUCT *msgDesc,
	FILM_QUEUE_LIST *queueList,
	char *format)
{
	int status;

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not reset the query descriptor.");
		return (status);
	}

	/*
	** Set up the command buffer with the select statement.
	*/
	(void) sprintf (cmdBuf,
		"select format "
		"from %s "
		"where granule_idx = %ld",
		queueList->granules_table, queueList->p_granule_idx);

	/*
	** Process the result row for this query.
	*/
	if ((status = execCmd (msgDesc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Check to see if only one row was returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"No rows of granule information returned for item '%d' of order '%d'.",
			queueList->item_id, queueList->order_id);
		return (IMS_ERROR);
	}
	else if (IMS_AFFECTED (qDesc) > 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"More than one row of granule information returned for item '%d' of order '%d'.",
			queueList->item_id, queueList->order_id);
		return (IMS_ERROR);
	}

	/*
	** Copy the returned data into the structure.
	*/

	/* format  */
	(void) memcpy (format,
		IMS_VALUE (qDesc, 0), IMS_VALUELENGTH (qDesc, 0));
	format[IMS_VALUELENGTH (qDesc, 0)] = '\0';
	(void) ims_truncStr (format);

	return (IMS_OK);
} /* getGranuleInfo */

/* *************************************************************
**
** subr getStageArea ()
**
** Get the staging area for the TTDL list.
**
**************************************************************** */

static int getStageArea (
	IMS_MSG_STRUCT *msgDesc,
	char *stageHost,
	char *stagePath)
{
	int status;

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not reset the query descriptor.");
		return (status);
	}

	/*
	** Set up the command buffer with the stored procedure call.
	*/
	(void) sprintf (cmdBuf, "med_get_stage_area %d", FPS_TTDL);

	/*
	** Process the result row for this query.
	*/
	if ((status = execCmd (msgDesc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Check to see if a path was returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain the staging area path.");
		return (IMS_ERROR);
	}
	else if (IMS_AFFECTED (qDesc) > 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"More than one row of stage path information returned." );
		return (IMS_ERROR);
	}

	/*
	** Copy in the returned data.
	*/

	/* host */
	(void) memcpy (stageHost, IMS_VALUE (qDesc, 0),
		IMS_VALUELENGTH (qDesc, 0));
	stageHost[IMS_VALUELENGTH (qDesc, 0)] = '\0';

	/* path */
	(void) memcpy (stagePath, IMS_VALUE (qDesc, 1),
		IMS_VALUELENGTH (qDesc, 1));
	stagePath[IMS_VALUELENGTH (qDesc, 1)] = '\0';

	return (IMS_OK);
}

/* *************************************************************
**
** subr getFileList ()
**
** Get a linked list of the file extensions for this granule.
**
**************************************************************** */

static FILM_FILE_LIST *getFileList (
	IMS_MSG_STRUCT *msgDesc,
	FILM_QUEUE_LIST *targetItem,
	DBCHAR *format)
{
	FILM_FILE_LIST *fileList;
	FILM_FILE_LIST *currPtr;
	FILM_FILE_LIST *prevPtr;
	int status;
	int rowCount;

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not reset the query descriptor.");
		return ((FILM_FILE_LIST *) NULL);
	}

	/*
	** Set up the command buffer with the stored procedure call.
	*/
	(void) sprintf (cmdBuf, "med_get_file_list %d, %ld, '%s'",
		targetItem->p_dataset_idx, targetItem->p_granule_idx, format);

	/*
	** Process the result rows for this query.
	*/
	prevPtr = NULL;
	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return ((FILM_FILE_LIST *) NULL);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** Allocate space for the FILM_FILE_LIST structure.
		**
		** lint: pointer cast may result in improper alignment
		** No problem, malloc() aligns on worst case boundary.
		*/
		if ((currPtr = (FILM_FILE_LIST *) malloc
			((size_t) sizeof (FILM_FILE_LIST))) ==
			(FILM_FILE_LIST *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for FILM_FILE_LIST structure.");
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			return ((FILM_FILE_LIST *) NULL);
		}

		/*
		** fileList points to the first element of the list.
		*/
		if (++rowCount == 1)
		{
			fileList = currPtr;
		}
		else
		{
			prevPtr->next = currPtr;
		}

		currPtr->next = (FILM_FILE_LIST *) NULL;

		/*
		** Copy in the returned data.
		*/

		/* type */
		(void) memcpy (&(currPtr->type),
			IMS_VALUE (qDesc, 0), IMS_VALUELENGTH (qDesc, 0));

		/* extension */
		(void) memcpy (currPtr->extension,
			IMS_VALUE (qDesc, 1), IMS_VALUELENGTH (qDesc, 1));
		currPtr->extension[IMS_VALUELENGTH (qDesc, 1)] = '\0';
		(void) ims_truncStr (currPtr->extension);

		/* path */
		(void) memcpy (currPtr->path,
			IMS_VALUE (qDesc, 2), IMS_VALUELENGTH (qDesc, 2));
		currPtr->path[IMS_VALUELENGTH (qDesc, 2)] = '\0';

		prevPtr = currPtr;
	}

	/*
	** Check the stored procedure status return value.
	*/
	if ((status = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return ((FILM_FILE_LIST *) NULL);
	}

	/*
	** Check to see if any rows were returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain the path and extensions for the current granule.");
		return ((FILM_FILE_LIST *) NULL);
	}

	return (fileList);
}

/* *************************************************************
**
** subr deleteFiles ()
**
** Delete files from the staging area.
**
**************************************************************** */

static void deleteFiles (
	IMS_MSG_STRUCT *msgDesc,
	FILM_QUEUE_LIST *targetItem,
	FILM_FILE_LIST *fileList,
	char *stagePath,
	int *filesDeleted)
{
	FILM_FILE_LIST *filePtr;
	char targetName[IMS_NAME_LEN+IMS_COL10_LEN+1];
	char targetSpec[IMS_PATH_LEN+IMS_NAME_LEN+IMS_COL10_LEN+1];
	struct stat statBuf;

	*filesDeleted = 0;
	filePtr = fileList;
	while (filePtr != (FILM_FILE_LIST *) NULL)
	{
		/*
		** Concatenate the destination file name.
		*/
		(void) sprintf (targetName, "%s.%s",
			targetItem->p_granule_name, filePtr->extension);

		/*
		** Concatenate the target and destination specifications.
		*/
		ims_concatFilePath (targetSpec, stagePath, targetName);

		/*
		** Make sure the file was copied to the staging area.
		** If it exists, remove it.
		*/
		if (stat (targetSpec, &statBuf) == 0)
		{
			if (remove (targetSpec) == -1)
			{
				(void) ims_msg (msgDesc, IMS_WARNING,
					"Could not remove staged file '%s'. %s",
					targetSpec, strerror (errno));
			}
			else /* File was deleted. */
			{
				(*filesDeleted)++;
			}
		}

		filePtr = filePtr->next;
	}

	return;
}   /*  deleteFiles  */

/* *************************************************************
**
** subr execCmd ()
**
** Execute an SQL procedure that writes data into the catalog database.
** We don't pass a parameter, but assume that when this function is
** called, the declared static buffer 'cmdBuf' has been properly filled
** in with the SQL statement to be executed.
**
** THIS ROUTINE IS ONLY USED FOR EXEUTING SQL STATEMENTS THAT WILL ONLY
** RETURN ONE ROW OR LESS FROM THE DATABASE.
**
**************************************************************** */

static int execCmd (
	IMS_MSG_STRUCT *msgDesc)
{
	int status;

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
	if ((status = checkRetStatus (msgDesc)) < IMS_OK)
	{
		return (status);
	}
	return (IMS_OK);
}

/* *************************************************************
**
**  subr checkRetStatus ()
**
** Check the procedure returned status value.
** When status returned is not an error, then return IMS_OK.
**
**************************************************************** */

static int checkRetStatus (
	IMS_MSG_STRUCT *msgDesc)
{
	int procReturn;
	int severity;

	/*
	** Check to see if the Sybase procedure returned a status. If it did
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
}   /*  checkRetStatus   */

/* *************************************************************
**
** subr freeQueueList ()
**
** Free the FILM_QUEUE_LIST structure.
**
**************************************************************** */

static void freeQueueList (
	FILM_QUEUE_LIST *currPtr)
{
	FILM_QUEUE_LIST *nextPtr;

	while (currPtr != (FILM_QUEUE_LIST *) NULL)
	{
		nextPtr = currPtr->next;
		free (currPtr);
		currPtr = nextPtr;
	}
	return;
}   /*  freeQueueList  */

/* *************************************************************
**
** subr freeFileList ()
**
** Free the FILM_FILE_LIST structure.
**
**************************************************************** */

static void freeFileList (
	FILM_FILE_LIST *currPtr)
{
	FILM_FILE_LIST *nextPtr;

	while (currPtr != (FILM_FILE_LIST *) NULL)
	{
		nextPtr = currPtr->next;
		free (currPtr);
		currPtr = nextPtr;
	}
	return;
}   /*  freeFileList  */
