static char *sccs = "@(#)ims_dispCat.c	5.4  25 Jul 1996";
/******************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** File:	   ims_dispCat.c
**
** Function: Catalog database access facility for ims_disp.
**
** Note:     It is assumed that all callers to functions to this facility
**           have already successfully opened a connection with the IMS
**           catalog.
**
** Creator:  Julie Wang
**
** Date:     June 29, 1995
**
** History:  
**
**           07/24/96   jwang   Get order_lock for step processing
**
**           02/27/96   jwang   R1Bprime preliminary.
**
*****************************************************************************/

/*
** Undefine Posix Source flag because of imcompatibility
** with IK include files.
*/
#undef _POSIX_SOURCE

#include <odldef.h>
#include <odlinter.h>
#include <ims_query.h>
#include <ims_v0.h>
#include <ims_disp.h>

/*
** Definition of local constants
*/
#define BUF_SIZE 1024   /* Maximum size of the cmd buffer */
 
/*
** Global variables
*/
char cmdBuf[BUF_SIZE];


/*
** Local Functions
*/
static int getItemList (V0_CAT_STRUCT *);
static int getNewSV (V0_CAT_STRUCT *);
static int getOrderLock (V0_CAT_STRUCT *);
static int recheckStatus (V0_CAT_STRUCT *);
static int execCmd (IMS_QI_DESC_OBJ *);
static int processRetStatus (IMS_QI_DESC_OBJ *);

/******************************************************************************
**
** disp_cat ()
**
** Main function handling catalog queries.
**
******************************************************************************/

int disp_cat (V0_CAT_STRUCT *catReq, DISP_CAT_EVENT event)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	char     msgbuf[IMS_COL512_LEN+1];
	int      status;

	msgDesc = catReq->msgDesc;
	cmdBuf[0] = '\0';

	/*
	** Make sure the caller has a database connection opened, which is required
	** for this routine.
	*/
	if ( catReq->qDesc == (IMS_QI_DESC_OBJ *)NULL )
	{
		msgbuf[0] = '\0';
		(void)sprintf (msgbuf, "disp_cat: Database connection has not been opened.");
		(void)ims_msg (msgDesc, IMS_FATAL, msgbuf);
		return (IMS_FATAL);
		
	}

	qDesc = catReq->qDesc;
	qDesc->cmd = cmdBuf;

	/*
	** Now, let's do our 'catalog' business according to the type of event
	** passed into the function.  
	*/
	switch (event)
	{
		case DISP_GETITEMLIST: 
			status = getItemList (catReq);
		break;

		case DISP_GETNEWSV: 
			status = getNewSV (catReq);
		break;

		case DISP_GETORDERLOCK: 
			status = getOrderLock (catReq);
		break;

		case DISP_RECHECKSTATUS: 
			status = recheckStatus (catReq);
		break;

		default:
			msgbuf[0] = '\0';
			(void)sprintf (msgbuf, "disp_cat: Invalid catalog event passed to disp_cat.");
			(void) ims_msg (msgDesc, IMS_FATAL, msgbuf);
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
		if (ims_qiResetDesc(qDesc) < IMS_OK)
		{
			msgbuf[0] = '\0';
			(void)sprintf (msgbuf, "disp_cat: Could not reinitialize query descriptor.");
			(void) ims_msg (msgDesc, IMS_FATAL, msgbuf);
			return(IMS_FATAL);
		}
	}

	/*
	** Return with the appropriate status
	*/
	if (status < IMS_OK) return (status);


	return (IMS_OK);

} /* end of disp_cat */

/***********************************************************************
** 
** getItemList -  
**
***********************************************************************/
static int getItemList (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	char     msgbuf[IMS_COL512_LEN+1];
	DISP_ORDER_LIST  *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (DISP_ORDER_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			msgbuf[0] = '\0';
			(void) sprintf (msgbuf, "disp_cat__getItemList: failed to get the result row.");
			(void) ims_msg (msgDesc, IMS_FATAL, msgbuf);
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (DISP_ORDER_LIST *)
			malloc (sizeof (DISP_ORDER_LIST))) == (DISP_ORDER_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"disp_cat__getItemList: Memory allocation for DISP_ORDER_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->order_id = 0;
		currPtr->item_id  = 0;
		currPtr->initial_status = 0;
		currPtr->next_p = (DISP_ORDER_LIST *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
    (void) memcpy ((DBINT *)&(currPtr->order_id),
		 qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->item_id),
		 qDesc->valAddr[1], qDesc->valLength[1]);

    (void) memcpy ((DBSMALLINT *)&(currPtr->initial_status),
		 qDesc->valAddr[2], qDesc->valLength[2]);

		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}

		else

		{
			lastPtr->next_p = currPtr;
			lastPtr = currPtr;
		}

	}
	qDesc->cmd = cmdBuf;

	/*
	** Re-initialize query descriptor for next command, but do not 
	** cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"disp_cat__getItemList: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (DISP_ORDER_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}


	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getItemList */

/***********************************************************************
** 
** getNewSV -  
**
***********************************************************************/
static int getNewSV (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	char     msgbuf[IMS_COL512_LEN+1];
	DISP_SV_LIST  *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (DISP_SV_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			msgbuf[0] = '\0';
			(void) sprintf (msgbuf, "disp_cat__getNewSV: failed to get the result row.");
			(void) ims_msg (msgDesc, IMS_FATAL, msgbuf);
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (DISP_SV_LIST *)
			malloc (sizeof (DISP_SV_LIST))) == (DISP_SV_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"disp_cat__getNewSV: Memory allocation for DISP_SV_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->dataset_idx = 0;
		currPtr->granule_idx  = 0;
		currPtr->platform[0] = '\0';
		currPtr->start_rev  = 0;
		currPtr->end_rev  = 0;
		currPtr->start_time[0] = '\0';
		currPtr->end_time[0] = '\0';
		currPtr->sv_precision[0] = '\0';
		currPtr->next_p = (DISP_SV_LIST *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBSMALLINT *)&(currPtr->dataset_idx),
		 qDesc->valAddr[0], qDesc->valLength[0]);

    (void) memcpy ((DBINT *)&(currPtr->granule_idx),
		 qDesc->valAddr[1], qDesc->valLength[1]);

		(void) memcpy ((DBCHAR *)currPtr->platform,
		   qDesc->valAddr[2], qDesc->valLength[2]);
		currPtr->platform[qDesc->valLength[2]] = '\0';
		ims_truncStr (currPtr->platform);

    (void) memcpy ((DBINT *)&(currPtr->start_rev),
		 qDesc->valAddr[3], qDesc->valLength[3]);

    (void) memcpy ((DBINT *)&(currPtr->end_rev),
		 qDesc->valAddr[4], qDesc->valLength[4]);

		(void) memcpy ((DBCHAR *)currPtr->start_time,
		   qDesc->valAddr[5], qDesc->valLength[5]);
		currPtr->start_time[qDesc->valLength[5]] = '\0';
		ims_truncStr (currPtr->start_time);

		(void) memcpy ((DBCHAR *)currPtr->end_time,
		   qDesc->valAddr[6], qDesc->valLength[6]);
		currPtr->end_time[qDesc->valLength[6]] = '\0';
		ims_truncStr (currPtr->end_time);

		(void) memcpy ((DBCHAR *)currPtr->sv_precision,
		   qDesc->valAddr[7], qDesc->valLength[7]);
		currPtr->sv_precision[qDesc->valLength[7]] = '\0';
		ims_truncStr (currPtr->sv_precision);

		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}

		else

		{
			lastPtr->next_p = currPtr;
			lastPtr = currPtr;
		}

	}
	qDesc->cmd = cmdBuf;

	/*
	** Re-initialize query descriptor for next command, but do not 
	** cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"disp_cat__getNewSV: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (DISP_SV_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}


	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getNewSV */

/****************************************************************************
**
** getOrderLock ()
**
** get a lock for order_item table.
**
****************************************************************************/
static int getOrderLock (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT  *msgDesc;

	qDesc  =  catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/*
	** execute stored procedure get_order_lock
	*/
	(void) sprintf (qDesc->cmd, "exec get_order_lock");

	if (execCmd (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, "disp_cat__getOrderLock: could not get order_lock.");
		return (IMS_FATAL);		
	}

	return (IMS_OK);

} /* end of getOrderLock */

/***********************************************************************
** 
** recheckStatus -  
**
***********************************************************************/
static int recheckStatus (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	if (execCmd (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, "disp_cat__recheckStatus: failed to recheck order status.");

		return (IMS_FATAL);
	}

	return (IMS_OK); 

} /* end of recheckStatus */

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

static int execCmd (IMS_QI_DESC_OBJ *qDesc)
{
	IMS_MSG_STRUCT *msgDesc;
	RETCODE status;
	int severity;

	msgDesc = qDesc->msgDesc;
	qDesc->cmd[strlen(qDesc->cmd)] = '\0';

	while ( (status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		/*
		** Check the returned status
		*/
		if (status < IMS_OK)
		{
			/*
			** ims_qiNextRow returns a bad status value when no
			** row results returned from the stored procedure.
			** Therefore, the following is added to correct the
			** situation.  This situation must be resolved before
			** delivering the code.
			** 
			** Check the stored procedure status returned value.
			**
			** if ((severity = processRetStatus (qDesc)) < IMS_OK)
			** {
			** 	return (severity);
			** }
			*/

			return (status);
		}
	}

	/*
	** Check the stored procedure status returend value.
	*/
	if ((severity = processRetStatus (qDesc)) < IMS_OK)
	{
		return (severity);
	}

	if (qDesc->msgNo != 0)
	{
		return(ims_msgGetSeverity(msgDesc));
	}
	return (IMS_OK);
} /* end of execCmd */

/****************************************************************************
**
** processRetStatus ()
**
** Check the procedure returned status value.
** When status returned is not an error, then return IMS_OK.
**
****************************************************************************/

static int processRetStatus (IMS_QI_DESC_OBJ *qDesc)
{
	IMS_MSG_STRUCT *msgDesc;
	int procReturn, severity;

	msgDesc = qDesc->msgDesc;

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
					severity = IMS_ERROR;
					(void) ims_msg (msgDesc, severity,
					"Procedure '%s' returned an unrecognized status of '%d'.",
						qDesc->cmd, procReturn);
					break;
			}
			return (severity);
		}
	}
	return (IMS_OK);
}
