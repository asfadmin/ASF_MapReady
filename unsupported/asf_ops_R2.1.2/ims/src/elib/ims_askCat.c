static char *sccs = "@(#)ims_askCat.c	5.1  16 Mar 1996";
/******************************************************************************
**
** File:        ims_askCat.c
**
** Function:    Catalog access functions.
**
** Author:      Hoshyar Sayah
**
** Date:        March 1995
**
** Notes:
**
** 1. catalog-access description
**
** The function determines the catalog access requested by the event
** argument.  The catReq structure contains all necessary information
** for accessing the catalog as specified by the event.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_keyword.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_ask.h>
#include <ims_askCat.h>
#include <ims_util.h>
 
/*
** Local Functions (Entry points into this module)
**
** This function prototype can be found in ims_auxCat.h.
** It is listed here for reference.
**
**	int ims_askCat (ASK_CAT_REQUEST *, ASK_CAT_EVENT);
*/

/*
** Local Functions
*/
static int openConnection (ASK_CAT_REQUEST *);
static int validate_PSD (ASK_CAT_REQUEST *);
static int getGranulesTable (ASK_CAT_REQUEST *);
static int getKeywordList (ASK_CAT_REQUEST *);
static int getResultList (ASK_CAT_REQUEST *);
static int execCmd (ASK_CAT_REQUEST *);
static int checkRetStatus (ASK_CAT_REQUEST *);
static void freeResultList (IMS_ASK_RLIST *);
static void freeKeywordList (ASK_KEYWORD_LIST *);

/* */
#define bcopy(x, y, z) memcpy (y, x, z)

/*
** Definition of local constants
*/
#define BUF_SIZE 512

/*
** The command buffer. Used by all routines in this file when a
** SQL command string must be built.
*/
static char cmdBuf[BUF_SIZE];

/****************************************************************************
**
** ims_askCat ()
**
** Main function handling ASK catalog queries.
**
****************************************************************************/

int ims_askCat (
	ASK_CAT_REQUEST *catReq,
	ASK_CAT_EVENT event)
{
	IMS_MSG_STRUCT *msgDesc;
	int status;

	msgDesc = catReq->msgDesc;

	/*
	** We must first make sure that we have a descriptor if the event is
	** anything but ASK_OPEN_CONNECTION.
	*/
	if ((catReq->qDesc == (IMS_QI_DESC_OBJ *) NULL) &&
		(event != ASK_OPEN_CONNECTION))
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"ASK_OPEN_CONNECTION must be the first askCat event called.");
		return (IMS_FATAL);
	}

	/*
	** Now, let's do our 'catalog' business according to the type of event
	** passed into the function.  
	*/
	switch (event)
	{
	case ASK_OPEN_CONNECTION:
		status = openConnection (catReq);

		/*
		** Return from here rather than break because there is no need
		** to call ims_qiCancel() before leaving as there is for all
		** other events, except ASK_CLOSE_CONNECTION
		*/
		return (status);

	case ASK_VALIDATE_PSD:
		status = validate_PSD (catReq);
		break;

	case ASK_GET_GRANULES_TABLE:
		status = getGranulesTable (catReq);
		break;

	case ASK_GET_KEYWORD_LIST:
		status = getKeywordList (catReq);
		break;

	case ASK_GET_RESULT_LIST:
		status = getResultList (catReq);
		break;

	case ASK_CLOSE_CONNECTION:
		/* Close the catalog connection. */ 
		return (ims_qiLogoff (catReq->qDesc));

	default:
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"Invalid catalog event passed to ims_askCat().");
		status = IMS_FATAL;
		break;
	}

	/* Re-assign query command buffer */
	IMS_SETCMD (catReq->qDesc, cmdBuf);

	/*
	** Release all query-allocated space and re-initialize qDesc for the
	** next time in, leaving open the connection to the catalog.
	*/
	if (catReq->qDesc->dbproc != (DBPROCESS *) NULL)
	{
		/*
		** Re-initialize query descriptor. But don't cancel prev command.
		*/
		if (ims_qiResetDesc (catReq->qDesc) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
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
** openConnection ()
**
******************************************************************************/

static int openConnection (
	ASK_CAT_REQUEST *catReq)
{
	IMS_MSG_STRUCT *msgDesc;
	ASK_USER_SPEC *userSpec;
	ASK_INFO_SPEC *infoSpec;
	IMS_QI_DESC_OBJ *qDesc;
	int status;

	msgDesc = catReq->msgDesc;
	userSpec = &(catReq->userSpec);
	infoSpec = &(catReq->infoSpec);

	/*
	** We only want to generate the descriptor once, the first
	** time in.  We also want to stay logged into the catalog
	** until the ASK_CLOSE_CONNECTION event is called. 
	*/

	/* 
	** Since this is the first time to access the catalog, we
	** need a query descriptor allocated.  If we can't get a
	** descriptor, return with a bad status ... we can't go on.
	*/
	if ((catReq->qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		return (IMS_FATAL);
	}
	qDesc = catReq->qDesc;
	qDesc->msgDesc = msgDesc;

	/*
	** Setup the descriptor with necessary information about this 
	** process.
	*/
	if (userSpec->username == (char *) NULL) 
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Cannot setup catalog query descriptor.");
		return (IMS_FATAL);
	}

	IMS_SETUSER (qDesc, userSpec->username);

	IMS_SETPSWD (qDesc, userSpec->password);

	IMS_SETPROG (qDesc, userSpec->program);

	if ((int) strlen (userSpec->catSrvName) > 0)
	{
		IMS_SETSERVER (qDesc, userSpec->catSrvName);
	}

	if ((int) strlen (userSpec->catDbName) > 0)
	{
		IMS_SETDBNAME (qDesc, userSpec->catDbName);
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
	IMS_SET_USERDATA (qDesc);

	return (IMS_OK);
}

/******************************************************************************
**
** validate_PSD ()
**
******************************************************************************/

static int validate_PSD (
	ASK_CAT_REQUEST *catReq)
{
	IMS_MSG_STRUCT *msgDesc;
	ASK_INFO_SPEC *infoSpec;
	IMS_QI_DESC_OBJ *qDesc;

	msgDesc = catReq->msgDesc;
	infoSpec = &(catReq->infoSpec);
	qDesc = catReq->qDesc;

	/*
	** Set up the command buffer with the stored procedure call.
	*/
	if ((int) strlen(infoSpec->sensor) > (int) 0)
	{
		(void) sprintf (cmdBuf, "validate_PSD '%s', '%s', '%s'", 
			infoSpec->platform, infoSpec->sensor, infoSpec->dataset);
	}
	else
	{
		(void) sprintf (cmdBuf, "validate_PSD '%s', null, '%s'", 
			infoSpec->platform, infoSpec->dataset);
	}

	if (execCmd (catReq) < IMS_OK)
	{
		return (IMS_FATAL);
	}

	/* */
	if (qDesc->count == 0)
	{
		return (IMS_ERROR);
	}
	else if (qDesc->count > 1)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Multiple/Duplicate policy entries; Contact DBA.");
		return (IMS_FATAL);
	}

	/*
	** Copy in the returned data.
	*/

	(void) memcpy ((char *) &(infoSpec->dataset_idx), qDesc->valAddr[0], 
		qDesc->valLength[0]);

	return (IMS_OK);
}

/******************************************************************************
**
** getGranulesTable ()
**
******************************************************************************/

static int getGranulesTable (
	ASK_CAT_REQUEST *catReq)
{
	IMS_MSG_STRUCT *msgDesc;
	ASK_INFO_SPEC *infoSpec;
	IMS_QI_DESC_OBJ *qDesc;

	msgDesc = catReq->msgDesc;
	infoSpec = &(catReq->infoSpec);
	qDesc = catReq->qDesc;

	/*
	** Set up the command buffer with the stored procedure call.
	*/
	(void) sprintf (cmdBuf, "ask_granules_table %d", 
		infoSpec->dataset_idx);

	if (execCmd (catReq) < IMS_OK)
	{
		return (IMS_FATAL);
	}

	/* */
	if (qDesc->count == 0)
	{
		return (IMS_ERROR);
	}
	else if (qDesc->count > 1)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Multiple/Duplicate policy entries; Contact DBA.");
		return (IMS_FATAL);
	}

	/*
	** Copy in the returned data.
	*/

	(void) memcpy ((char *) infoSpec->granules_table, qDesc->valAddr[0], 
		qDesc->valLength[0]);
	infoSpec->granules_table[qDesc->valLength[0]] = '\0';

	return (IMS_OK);
}

/******************************************************************************
**
** getKeywordList ()
**
******************************************************************************/

static int getKeywordList (
	ASK_CAT_REQUEST *catReq)
{
	IMS_MSG_STRUCT *msgDesc;
	ASK_INFO_SPEC *infoSpec;
	ASK_KEYWORD_LIST *keywordList;
	ASK_KEYWORD_LIST *currPtr;
	ASK_KEYWORD_LIST *prevPtr;
	IMS_QI_DESC_OBJ *qDesc;
	int status;
	int rowCount;
	int keywordCount;
	int severity;

	msgDesc = catReq->msgDesc;
	infoSpec = &(catReq->infoSpec);

	/* initialize */
	keywordList = (ASK_KEYWORD_LIST *)NULL;
	currPtr = prevPtr = (ASK_KEYWORD_LIST *) NULL;
	*(int *)catReq->item[0] = (int) 0;
	catReq->item[1] = (void *)NULL;

	msgDesc = catReq->msgDesc;
	qDesc = catReq->qDesc;

	(void) sprintf (cmdBuf, "ask_keyword_list %d", infoSpec->dataset_idx);


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
				/* Keyword list is empty. */
				while (ims_qiNextRow (qDesc) !=
					IMS_ENDOFTRANSACTION) {}
				return (IMS_OK);
			}

			/*
			** Allocate space for the ASK_KEYWORD_LIST structure.
			**
			** lint: pointer cast may result in improper alignment
			** No problem, malloc() aligns on worst case boundary.
			*/
			if ((currPtr = (ASK_KEYWORD_LIST *) calloc 
				((unsigned)keywordCount,
				(unsigned)sizeof (ASK_KEYWORD_LIST))) 
				== (ASK_KEYWORD_LIST *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Could not allocate memory for ASK_KEYWORD_LIST structure.");
				while (ims_qiNextRow (qDesc) !=
					IMS_ENDOFTRANSACTION) {}
				return (IMS_FATAL);
			}

			/* keywordList points to the first element of the list. */
			keywordList = currPtr;

			continue;
		}

		/*
		** Copy the returned data into the structure.
		*/
		/* Copy to a temporary area so we can truncate spaces. */
		(void) memcpy (currPtr->keyword, qDesc->valAddr[0], 
			qDesc->valLength[0]);
		currPtr->keyword[qDesc->valLength[0]] = '\0';
		ims_truncStr (currPtr->keyword);

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
	prevPtr->next = (ASK_KEYWORD_LIST *) NULL;

	if (keywordCount != rowCount-1)
	{
		(void) freeKeywordList (keywordList);
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"Number of keywords does not match number of rows.");
		return (IMS_FATAL);
	}

	/*
	** Check the returned status value.
	*/
	if ((severity = checkRetStatus (catReq)) < IMS_OK)
	{
		(void) freeKeywordList (keywordList);
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"Number of keywords does not match number of rows.");
		return (severity);
	}

	*(int *)catReq->item[0] = keywordCount;
	catReq->item[1] = (void *)keywordList;
	return (IMS_OK);
}

/***************************************************************************
**
** getResultList - 
**
****************************************************************************/
static int getResultList (
	ASK_CAT_REQUEST *catReq)
{
	IMS_ASK_RLIST *resultList, *currPtr, *prevPtr;
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	ASK_INFO_SPEC *infoSpec;
	char *sqlBuffer;
	int rowCount;
	int status;

	msgDesc = catReq->msgDesc;
	qDesc = catReq->qDesc;
	infoSpec = &(catReq->infoSpec);

	/* initialize */
	resultList = currPtr = prevPtr = (IMS_ASK_RLIST *)NULL;
	sqlBuffer = catReq->item[0];
	*(int *)catReq->item[1] = (int) 0;  /* returned row counter */
	catReq->item[2] = (void *)NULL;     /* returned results */

	IMS_SETCMD (qDesc, sqlBuffer);

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			(void) freeResultList (resultList);
			return (status);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY) continue;

		/*
		** A row has been returned.
		*/
		rowCount++; /* Increment rowCount */

		/*
		** Allocate space for the row returned.
		*/
		if ((currPtr = (IMS_ASK_RLIST *) malloc 
				((unsigned) sizeof (IMS_ASK_RLIST))) == (IMS_ASK_RLIST *)NULL)
		{
			ims_msg (msgDesc, IMS_FATAL,
				"Memory allocation for IMS_ASK_RLIST structure failed.");
			(void) freeResultList (resultList);
			return (IMS_FATAL);
		}
		currPtr->next = (IMS_ASK_RLIST *)NULL;
		if (rowCount == 1)
		{
			resultList = prevPtr = currPtr;
		}
		else
		{
			prevPtr->next = currPtr;
			prevPtr = currPtr;
		}

		/*
		** Copy the returned data into the structure.
		*/
		(void) bcopy (qDesc->valAddr[0], &(currPtr->dataset_idx),
			qDesc->valLength[0]);

		(void) bcopy (qDesc->valAddr[1], &(currPtr->granule_idx),
			qDesc->valLength[1]);

		(void) bcopy (qDesc->valAddr[2], (char *) currPtr->name,
			qDesc->valLength[2]);
		currPtr->name[qDesc->valLength[2]] = '\0';

		if ((qDesc->valLength[3] == 0) || (qDesc->valAddr[3] == (char *)NULL))
		{
			currPtr->version = -1;
		}
		else
		{   
			(void) bcopy (qDesc->valAddr[3], (char *) &(currPtr->version),
				qDesc->valLength[3]);
		}

		(void) bcopy (qDesc->valAddr[4], (char *) &(currPtr->data_kbytes),
			qDesc->valLength[4]);

		(void) bcopy (qDesc->valAddr[5], (char *) &(currPtr->metadata_kbytes),
			qDesc->valLength[5]);
	}

	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *)resultList;
	return (IMS_OK);
}

/****************************************************************************
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
****************************************************************************/

static int execCmd (
	ASK_CAT_REQUEST *catReq)
{
	IMS_MSG_STRUCT *msgDesc;
	IMS_QI_DESC_OBJ *qDesc;
	int status;
	int severity;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

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
	if ((severity = checkRetStatus (catReq)) < IMS_OK)
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
	ASK_CAT_REQUEST *catReq)
{
	IMS_MSG_STRUCT *msgDesc;
	IMS_QI_DESC_OBJ *qDesc;
	int procReturn;
	int severity;

	msgDesc = catReq->msgDesc;
	qDesc = catReq->qDesc;

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

/****************************************************************************
**
** freeResultList -  free the resultList allocated memory.
**
*****************************************************************************/
static void freeResultList (
	IMS_ASK_RLIST *resultList)
{
	IMS_ASK_RLIST *currPtr;
	currPtr = resultList;
	while (resultList != (IMS_ASK_RLIST *)NULL)
	{
		currPtr = resultList->next;
		free (resultList);
		resultList = currPtr;
	}
	resultList = (IMS_ASK_RLIST *)NULL;
}

/****************************************************************************
**
** freeKeywordList -  free the keywordList allocated memory.
**
*****************************************************************************/
static void freeKeywordList (
	ASK_KEYWORD_LIST *keywordList)
{
	(void) free (keywordList);
	keywordList = (ASK_KEYWORD_LIST *)NULL;
}


