static char *sccs = "@(#)ims_cmnODLQuery.c	5.5  03/27/97";
/******************************************************************************
**
** File:        ims_cmnODLQuery.c
**
** Function:    This file contains functions which rely on the ODL library.
**              These functions were moved over from ims_cmnQuery.c.
**
** Author:      Sean Hardman, David Pass and Alin Tilden
**
** Date:        2/9/96
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <syslog.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_cmnQuery.h>
#include <ims_util.h>
#include <ims_timeConv.h>
#include <ims_keyword.h>

#include <odldef.h>
#include <ims_odl.h>

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in ims_cmnQuery.h.
**
**   int ims_orderStatus( IMS_CMN_QUERY *, char * );
**   int ims_darFrame (IMS_CMN_QUERY *, char *);
*/

/*
** Local function prototypes
*/
static int getItemsValue (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, char * ,
	char *, int *, int *);
static int getOrderItemType (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, int,
	short, int *, int *);
static int setTransState (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, char *);
static int getProcessStatus (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
	int , short, int *, int *);
static int updateProcessStatus (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, int,
	int, int, char *, int, int, char *, int, int, int, int *);
static int updateItemStatus (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, int,
	int, char *, int *);
static int getGranuleTable (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
	char *, char *, int *);
static int getGranuleInfo (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
	char *, char *, int *, int *, int *, int *, int *);
static int setDarFrameData (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
	int, short, char *, int, int, int *);
static int validateOrderItem (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
	int, short, int *);
static int checkConnection (IMS_CMN_QUERY *);
static int execCmd (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static int checkRetStatus (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static int getOrderLock (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);

/******************************************************************************
**
** ims_orderStatus ()
**
** This function will update an order item's processing status as well
** as its own status.  The status information resides in an ODL buffer.
** If the STATUS keyword is "COMPLETED", then the p_dataset_idx and
** p_granule_idx columns have to also be updated.
**
******************************************************************************/

int ims_orderStatus (
	IMS_CMN_QUERY *query,
	char *odlBuffer)
{
	IMS_MSG_STRUCT *msgDesc;
	IMS_QI_DESC_OBJ *qDesc;
	IMS_KEYWORD_LIST *keywordList;
	IMS_KEYWORD_LIST *listPtr;
	static char cmdBuf[IMS_COL512_LEN];
	int order_id;
	int item_id;
	char status_type[33];
	char status_id[33];
	char comment[IMS_COL255_LEN+1];
	char dataset[IMS_COL80_LEN+1];
	char product_id[IMS_COL30_LEN+1];
	char granules_table[IMS_COL30_LEN+1];
	int granule_idx;
	int dataset_idx;
	int process_status;
	int current_process_status;
	int status_completed;
	int status;
	int finalFlag;
	int completedFlag;
	int order_item_type;
	int item_type_tsr;
	int data_kbytes;
	int metadata_kbytes;


	/*
	** Initialize variables.
	*/
	msgDesc = query->msgDesc;
	query->retStatus = IMS_OK;
	completedFlag = IMS_FALSE;

	/*
	** Check for an active database server connection.
	*/
	if ((status = checkConnection (query)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"The database server connection was not valid.");
		query->retStatus = IMS_NOCONNECT;
		return (status);
	}

	/*
	** Assign the command buffer to the query descriptor.
	*/
	qDesc = query->qDesc;
	IMS_SETCMD (qDesc, cmdBuf);

	/*
	** Parse the ODL buffer into a linked list.
	** Only parse the 'BODY' object.
	*/
	if ((status = ims_parseODLBuffer (msgDesc, odlBuffer,
		"BODY", &keywordList)) < IMS_OK)
	{
		(void) ims_qiResetDesc (qDesc);
		return (status);
	}

	/*
	** Find the ORDER_ID keyword.
	*/
	if ((status = ims_findListItem (msgDesc, keywordList, "ORDER_ID",
		IMS_KEYWORD, IMS_TRUE, &listPtr)) < IMS_OK)
	{
		goto LEAVE;
	}
	else /* Get the associated value. */
	{
		if (listPtr->data_type == IMS_INT4_TYPE)
		{
			order_id = listPtr->value_integer;
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Keyword '%s' has an invalid data type.",
				listPtr->keyword);
			status = IMS_ERROR;
			goto LEAVE;
		}
	}

	/*
	** Find the ITEM_ID keyword.
	*/
	if ((status = ims_findListItem (msgDesc, keywordList, "ITEM_ID",
		IMS_KEYWORD, IMS_TRUE, &listPtr)) < IMS_OK)
	{
		goto LEAVE;
	}
	else /* Get the associated value. */
	{
		if (listPtr->data_type == IMS_INT4_TYPE)
		{
			if (listPtr->value_integer > IMS_MAX_SINT)
			{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Keyword 'ITEM_ID' has an invalid value: %d.",
				listPtr->value_integer);
			status = IMS_ERROR;
			goto LEAVE;
			}
			else
			{
			item_id = listPtr->value_integer;
			}
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Keyword '%s' has an invalid data type.",
				listPtr->keyword);
			status = IMS_ERROR;
			goto LEAVE;
		}
	}

	/*
	** Retrieve the order_item_type for the given order_id and
	** item_id values from the order_item table.
	*/
		if ((status = getOrderItemType (msgDesc, qDesc,
		  order_id, item_id, &order_item_type, &(query->retStatus)))
		< IMS_OK)
	{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not find order_item entry with order "
				"id: %d, item_id: %d.",
				order_id, item_id);
			goto LEAVE;
	}

	/*
	** Get instance of Tape Scan Request (TSR) order_item_type
	*/
	if ((status = getItemsValue (msgDesc, qDesc, "item_type",
		"TSR", &item_type_tsr, &(query->retStatus))) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not translate item_type TSR.");
		goto LEAVE;
	}

	/*
	** Find the STATUS_TYPE keyword.
	*/
	if ((status = ims_findListItem (msgDesc, keywordList, "STATUS_TYPE",
		IMS_KEYWORD, IMS_TRUE, &listPtr)) < IMS_OK)
	{
		goto LEAVE;
	}
	else /* Get the associated value. */
	{
		if ((listPtr->data_type == IMS_SYMBOL_TYPE) ||
			(listPtr->data_type == IMS_STRING_TYPE))
		{
			(void) strcpy (status_type, listPtr->value_string);
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Keyword '%s' has an invalid data type.",
				listPtr->keyword);
			status = IMS_ERROR;
			goto LEAVE;
		}
	}

	/*
	** Check the STATUS_TYPE value.
	*/
	if (strcmp (status_type, "FINAL") == 0)
	{
		finalFlag = IMS_TRUE;
	}
	else if (strcmp (status_type, "INTERMEDIATE") == 0)
	{
		finalFlag = IMS_FALSE;
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Keyword 'STATUS_TYPE' has an invalid value '%s'.",
			status_type);
		status = IMS_ERROR;
		goto LEAVE;
	}

	/*
	** Find the STATUS keyword.
	*/
	if ((status = ims_findListItem (msgDesc, keywordList, "STATUS",
		IMS_KEYWORD, IMS_TRUE, &listPtr)) < IMS_OK)
	{
		goto LEAVE;
	}
	else /* Get the associated value. */
	{
		if ((listPtr->data_type == IMS_SYMBOL_TYPE) ||
			(listPtr->data_type == IMS_STRING_TYPE))
		{
			(void) strcpy (status_id, listPtr->value_string);
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Keyword '%s' has an invalid data type.",
				listPtr->keyword);
			status = IMS_ERROR;
			goto LEAVE;
		}
	}

	/*
	** Check the STATUS value.
	*/
	if ((status = getItemsValue (msgDesc, qDesc, "process_status",
		status_id, &process_status, &(query->retStatus))) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Keyword 'STATUS' has an invalid value '%s'.",
			status_id);
		goto LEAVE;
	}

	/*
	** Check to see if the STATUS value corresponds with the
	** STATUS_TYPE value.
	*/
	if (finalFlag == IMS_TRUE)
	{
		if ((strcmp (status_id, "COMPLETED") != 0) &&
			(strcmp (status_id, "FAILED") != 0) &&
			(strcmp (status_id, "CANCEL") != 0))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"An invalid status of '%s' was encountered for "
				"status type '%s'.",
				status_id, status_type);
			status = IMS_ERROR;
			goto LEAVE;
		}
	}
	else /* STATUS_TYPE is IMTERMEDIATE */
	{
		if ((strcmp (status_id, "COMPLETED") == 0) ||
			(strcmp (status_id, "FAILED") == 0) ||
			(strcmp (status_id, "CANCEL") == 0))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"An invalid status of '%s' was encountered "
				"for status type '%s'.",
				status_id, status_type);
			status = IMS_ERROR;
			goto LEAVE;
		}
	}

	/*
	** Find the COMMENT keyword.
	*/
	if ((status = ims_findListItem (msgDesc, keywordList, "COMMENT",
		IMS_KEYWORD, IMS_FALSE, &listPtr)) < IMS_OK)
	{
		goto LEAVE;
	}
	else /* Get the associated value. */
	{
		if (listPtr != (IMS_KEYWORD_LIST *) NULL)
		{
			if ((listPtr->data_type == IMS_SYMBOL_TYPE) ||
				(listPtr->data_type == IMS_STRING_TYPE))
			{
				(void) strcpy (comment, listPtr->value_string);
			}
			else
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"Keyword '%s' has an invalid data type.",
					listPtr->keyword);
				status = IMS_ERROR;
				goto LEAVE;
			}
		}
		else
		{
			comment[0] = '\0';
		}
	}

	/*
	** For all order_item_types except TSR, the COMPLETED state
	** (and potentially the CANCEL state) requires additional processing.
	*/
	if (order_item_type != item_type_tsr)
	{
		/*
		** If the STATUS keyword value is 'COMPLETED' then
		** we need to get some more keyword values.
		*/
		if (strcmp (status_id, "COMPLETED") == 0)
		{
			completedFlag = IMS_TRUE;

			/*
			** At this time we ignore the PLATFORM and SENSOR keywords
			** because we do not need them to uniquely identify the
			** granules table.
			*/

			/*
			** Find the DATASET keyword.
			*/
			if ((status = ims_findListItem (msgDesc, keywordList, "DATASET",
					 IMS_KEYWORD, IMS_TRUE, &listPtr)) < IMS_OK)
			{
					goto LEAVE;
			}
			else /* Get the associated value. */
			{
					if ((listPtr->data_type == IMS_SYMBOL_TYPE)
					||
					(listPtr->data_type == IMS_STRING_TYPE))
				{
					(void) strcpy (dataset, listPtr->value_string);
				}
				else
				{
					(void) ims_msg (msgDesc, IMS_ERROR,
					"Keyword '%s' has an invalid data type.",
					listPtr->keyword);
					status = IMS_ERROR;
					goto LEAVE;
				}
			}

			/*
			** Find the PRODUCT_ID keyword.
			*/
			if ((status = ims_findListItem (msgDesc, keywordList, "PRODUCT_ID",
					 IMS_KEYWORD, IMS_TRUE, &listPtr)) < IMS_OK)
			{
				goto LEAVE;
			}
			else /* Get the associated value. */
			{
				if ((listPtr->data_type == IMS_SYMBOL_TYPE)
				||
				(listPtr->data_type == IMS_STRING_TYPE))
				{
					(void) strcpy (product_id, listPtr->value_string);
				}
				else
				{
					(void) ims_msg (msgDesc, IMS_ERROR,
					"Keyword '%s' has an invalid data type.",
					listPtr->keyword);
					status = IMS_ERROR;
					goto LEAVE;
				}
			}
		}
#ifdef CANCEL_WITHOUT_PRODUCT
		else if (strcmp (status_id, "CANCEL") == 0)
		{
			/*
			** PR 892 - Added this line so that we don't try to update the
			** product information for the order item.  This is a temporary
			** fix. The long-term fix would involve CP, PPS and IMS reviewing
			** their interfaces to solve the problem of determining whether
			** the product information for a given cancelled order item.
			**
			** 3/97: for r2.1, CP will continue to use "CANCEL/FAIL",
			** PPS will xlate this to "FAILED", and IMS will not update the
			** product information for "FAILED" or "CANCEL", ie,
			**			completedFlag = IMS_FALSE;
			** which it already is, so nothing needs to be done.
			*/
			completedFlag = IMS_FALSE;

#ifdef CANCEL_WITH_PRODUCT
			/*
			** If the PLATFORM, SENSOR, DATASET, PRODUCT_ID keywords
			** exist than the product has already been generated,
			** process these keywords in order to cancel the product.
			** Note: All or none of the required keywords must be present.
			*/
			/*
			** At this time we ignore the PLATFORM and SENSOR keywords
			** because we do not need them to uniquely identify the
			** granules table.
			*/

			/*
			** Find the DATASET keyword.
			*/
			if ((status = ims_findListItem (msgDesc, keywordList, "DATASET",
				 IMS_KEYWORD, IMS_FALSE, &listPtr)) < IMS_OK)
			{
				goto LEAVE;
			}
			else
			{
				if (listPtr == (IMS_KEYWORD_LIST *) NULL)
				{
					completedFlag = IMS_FALSE;
				}
				else  /* Get the associated value. */
				{
					completedFlag = IMS_TRUE;

					if ((listPtr->data_type == IMS_SYMBOL_TYPE)
					||
					(listPtr->data_type == IMS_STRING_TYPE))
					{
						(void) strcpy (dataset, listPtr->value_string);
					}
					else
					{
						(void) ims_msg (msgDesc, IMS_ERROR,
						 "Keyword '%s' has an invalid data type.",
						 listPtr->keyword);
						status = IMS_ERROR;
						goto LEAVE;
					}
				}
			}

			/*
			** Find the PRODUCT_ID keyword.
			*/
			if ((status = ims_findListItem (msgDesc, keywordList, "PRODUCT_ID",
					 IMS_KEYWORD, IMS_FALSE, &listPtr)) < IMS_OK)
			{
					goto LEAVE;
			}
			else
			{
				if (listPtr == (IMS_KEYWORD_LIST *) NULL)
				{
					if (completedFlag == IMS_TRUE)
					{
						(void) ims_msg (msgDesc, IMS_ERROR,
							"Keyword PRODUCT_ID is mandatory and not found.");
						status = IMS_ERROR;
						goto LEAVE;
					}
				}
				else /* Get the associated value. */
				{
					if (completedFlag == IMS_FALSE)
					{
						(void) ims_msg (msgDesc, IMS_ERROR,
							"Keyword DATASET is mandatory and not found.");
						status = IMS_ERROR;
						goto LEAVE;
					}

					if ((listPtr->data_type == IMS_SYMBOL_TYPE)
						||
						(listPtr->data_type == IMS_STRING_TYPE))
					{
						(void) strcpy (product_id, listPtr->value_string);
					}
					else
					{
						(void) ims_msg (msgDesc, IMS_ERROR,
						"Keyword '%s' has an invalid data type.",
						listPtr->keyword);
						status = IMS_ERROR;
						goto LEAVE;
					}
				}
			}
#endif /* CANCEL_WITH_PRODUCT */
		}
#endif /* CANCEL_WITHOUT_PRODUCT */

		/*
		** If the keywords for the completed state have been extracted,
		** retrieve the granule information.
		*/
		if (completedFlag == IMS_TRUE)
		{
			/*
			** Get the granule table name.
			*/
				if ((status = getGranuleTable (msgDesc, qDesc, dataset,
					 granules_table, &(query->retStatus))) < IMS_OK)
			{
					(void) ims_msg (msgDesc, IMS_ERROR,
						"Could not obtain granule table name for dataset '%s'.",
						dataset);
				goto LEAVE;
			}

			/*
			** Get the granule specific indexes.
			*/
			if ((status = getGranuleInfo (msgDesc, qDesc, product_id,
					 granules_table, &granule_idx, &dataset_idx,
					 &data_kbytes, &metadata_kbytes,
					 &(query->retStatus))) < IMS_OK)
			{
					(void) ims_msg (msgDesc, IMS_ERROR,
						"Could not obtain granule information for product '%s' from dataset '%s'.",
						product_id, dataset);
					goto LEAVE;
			}
		}
	} /* end of if (!TSR) */

	/*
	** Begin the update transaction.
	*/
	if ((status = setTransState (msgDesc, qDesc, "begin")) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not begin the update transaction.");
		query->retStatus = IMS_QUERYFAIL;
		goto LEAVE;
	}

	/*
	** Lock the table until update is complete.
	*/
	if ((status = getOrderLock (msgDesc, qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status, "Could not get order_lock.");
		(void) setTransState (msgDesc, qDesc, "rollback");
		goto LEAVE;
	}

	/*
	** Get the current status of this order_item entry.
	*/
	if ((status = getProcessStatus (msgDesc, qDesc, order_id, item_id,
		&current_process_status, &(query->retStatus))) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not get status from order_item table for "
			"order_id:%d, item_id:%d.",
			order_id, item_id);
		(void) setTransState (msgDesc, qDesc, "rollback");
		goto LEAVE;
	}

	/*
	** Get the correct instance for process status value COMPLETED.
	*/
	if ((status = getItemsValue (msgDesc, qDesc, "process_status",
		"COMPLETED", &status_completed, &(query->retStatus))) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not translate STATUS value COMPLETED.");
		(void) setTransState (msgDesc, qDesc, "rollback");
		goto LEAVE;
	}

	/*
	** Make sure the current process status is not COMPLETED.
	** Dar entries with IMS status value COMPLETED can not be updated.
	*/
	if (current_process_status == status_completed)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Process status is already COMPLETED.");
		(void) setTransState (msgDesc, qDesc, "rollback");
		status = IMS_ERROR;
		goto LEAVE;
	}

	/*
	** Update the order item's process status based on given input.
	*/
	if ((status = updateProcessStatus (msgDesc, qDesc, order_id,
		item_id, process_status, comment, dataset_idx, granule_idx,
		product_id, completedFlag, data_kbytes, metadata_kbytes,
		&(query->retStatus))) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not update the process status.");
		(void) setTransState (msgDesc, qDesc, "rollback");
		goto LEAVE;
	}

	/*
	** Update the order item's status.
	*/
	if (finalFlag == IMS_TRUE)
	{
		if ((status = updateItemStatus (msgDesc, qDesc, order_id,
			item_id, status_id, &(query->retStatus))) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not update the item status.");
			(void) setTransState (msgDesc, qDesc, "rollback");
			goto LEAVE;
		}
	}

	/*
	** Commit the update transaction.
	*/
	if ((status = setTransState (msgDesc, qDesc, "commit")) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not commit the update transaction.");
		query->retStatus = IMS_QUERYFAIL;
	}

LEAVE:
	ims_freeKeywordList (keywordList);
	(void) ims_qiResetDesc (qDesc);

	if (status < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not update order status for item '%d' of order '%d' "
			"based on process status '%s'.",
			item_id, order_id, status_id);
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"Successfully updated order status for item '%d' of order '%d' "
			"based on process status '%s'.",
			item_id, order_id, status_id);
	}

	return (status);
}

/******************************************************************************
**
** getItemsValue ()
**
** This function queries for the integer representation of the value
** given from the items table.
**
******************************************************************************/

static int getItemsValue (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	char *type,
	char *description,
	int *instance,
	int *retStatus)
{
	static char cmdBuf[IMS_COL512_LEN];
	DBSMALLINT instanceTemp;
	int status;

	/*
	** Populate the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"select instance from items \
		where type = '%s' and description = '%s'",
		type, description);

	/*
	** Assign the command buffer to the query descriptor.
	*/
	IMS_SETCMD (qDesc, cmdBuf);

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (status);
	}

	/*
	** Execute the command.
	*/
	if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
	{
		*retStatus = status;
		return (IMS_ERROR);
	}

	/*
	** See if we got one row returned.
	*/
	if (IMS_AFFECTED (qDesc) < 1)
	{
		return (IMS_ERROR);
	}

	/*
	** Get the instance value.
	*/
	(void) memcpy (&instanceTemp,
		qDesc->valAddr[0], qDesc->valLength[0]);

	*instance = instanceTemp;

	return (IMS_OK);
}

/******************************************************************************
**
** getOrderItemType ()
**
** This function will retrieve the order_item_type value from
** the order_item table for the given order_id, item_id.
**
******************************************************************************/

static int getOrderItemType (
	IMS_MSG_STRUCT  *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	int              order_id,
	short            item_id,
	int             *order_item_type,
	int             *retStatus)
{
	static char cmdBuf[IMS_COL512_LEN];
	DBSMALLINT  itemTypeTemp;
	int         status;

	/*
	** Populate the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"select order_item_type from order_item \
		where order_id = %d and item_id = %d",
		order_id, item_id);

	/*
	** Assign the command buffer to the query descriptor.
	*/
	IMS_SETCMD (qDesc, cmdBuf);

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (status);
	}

	/*
	** Execute the command.
	*/
	if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
	{
		*retStatus = status;
		return (IMS_ERROR);
	}

	/*
	** See if we got one row returned.
	*/
	if (IMS_AFFECTED (qDesc) < 1)
	{
			*retStatus = IMS_NOROWS;
		return (IMS_ERROR);
	}

	/*
	** Get the instance value.
	*/
	(void) memcpy (&itemTypeTemp,
		qDesc->valAddr[0], qDesc->valLength[0]);

	*order_item_type = itemTypeTemp;

	return (IMS_OK);
}

/******************************************************************************
**
** setTransState ()
**
** Set the state of the transaction.
**
******************************************************************************/

static int setTransState (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	char *transType)
{
	static char cmdBuf[IMS_COL512_LEN];
	int status;

	/*
	** Populate the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf, "%s transaction", transType);

	/*
	** Assign the command buffer to the query descriptor.
	*/
	IMS_SETCMD (qDesc, cmdBuf);

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
	** Execute the cammand.
	*/
	if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
	{
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getProcessStatus ()
**
** This function will retrieve the process_status value from
** the order_item table.
**
******************************************************************************/

static int getProcessStatus (
	IMS_MSG_STRUCT  *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	int              order_id,
	short            item_id,
	int             *process_status,
	int             *retStatus)
{
	int            status;
	static char    cmdBuf[IMS_COL512_LEN];
	short          temp_status;

	(void) sprintf (cmdBuf,
		"select process_status \
		from   order_item \
		where  order_id = %d \
		and item_id = %d",
		order_id, item_id);

	/*
	** Assign the command buffer to the query descriptor.
	*/
	IMS_SETCMD (qDesc, cmdBuf);

	/*
	** Reset the query descriptor.
	*/
	(void) ims_qiResetDesc (qDesc);

	/*
	** Execute order_item table query.
	** This query will return a single row.
	*/
	if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Order_item table query failed.");
		*retStatus = status;
		status = IMS_ERROR;
		return (status);
	}

	if (IMS_AFFECTED (qDesc) < 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Order_item entry order_id:%d, item_id:%d not found.",
			order_id, item_id);
		*retStatus = IMS_NOROWS;
		status = IMS_ERROR;
		return (status);
	}

	/*
	** Copy the returned data into the output variable.
	*/
	(void) memcpy ((char *) &temp_status,
		qDesc->valAddr[0], qDesc->valLength[0]);
	*process_status = temp_status;

	return (IMS_OK);

} /* getProcessStatus */

/******************************************************************************
**
** updateProcessStatus ()
**
** Update the process status and associated values.
**
******************************************************************************/

static int updateProcessStatus (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	int order_id,
	int item_id,
	int process_status,
	char *comment,
	int dataset_idx,
	int granule_idx,
	char *product_id,
	int completedFlag,
	int data_kbytes,
	int metadata_kbytes,
	int *retStatus)
{
	static char cmdBuf[IMS_COL512_LEN];
	int status;

	/*
	** Populate the command buffer with the SQL statement.
	*/
	if (completedFlag == IMS_FALSE)
	{
		(void) sprintf (cmdBuf,
			"update order_item \
			set process_status = %d, \
			process_comment = '%s' \
			where order_id = %d \
			and item_id = %d",
			process_status, comment, order_id, item_id);
	}
	else /* Update the granule and dataset indexes too. */
	{
		(void) sprintf (cmdBuf,
			"update order_item \
			set process_status = %d, \
			process_comment = '%s', \
			p_dataset_idx = %d, \
			p_granule_idx = %d, \
			p_granule_name = '%s', \
			p_data_kbytes = %d, \
			p_metadata_kbytes = %d \
			where order_id = %d \
			and item_id = %d",
			process_status, comment, dataset_idx, granule_idx, product_id,
			data_kbytes, metadata_kbytes, order_id, item_id);
	}

	/*
	** Assign the command buffer to the query descriptor.
	*/
	IMS_SETCMD (qDesc, cmdBuf);

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
	** Execute the command.
	*/
	if ((status = execCmd ( msgDesc, qDesc)) < IMS_OK)
	{
		*retStatus = status;
		return (IMS_ERROR);
	}

	/*
	** See if we updated one row.
	*/
	if (IMS_AFFECTED (qDesc) < 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not find a row to update for item '%d' of order '%d'.",
			item_id, order_id);
		*retStatus = IMS_NOROWS;
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** updateItemStatus ()
**
** Update the item status.
**
******************************************************************************/

static int updateItemStatus (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	int order_id,
	int item_id,
	char *status_id,
	int *retStatus)
{
	static char cmdBuf[IMS_COL512_LEN];
	int item_status;
	int status;

	/*
	** Determine the item status from the process status.
	*/
	if (strcmp (status_id, "COMPLETED") == 0)
	{
		/* ON-LINE : changed from GENERATED D. Pass 4/10/96 */
		if (getItemsValue (msgDesc, qDesc, "item_status",
			"ON-LINE", &item_status, retStatus) < IMS_OK)
		{
			/* should never get here; hard coding the value */
			item_status = 4;
		}
	}
	else if (strcmp (status_id, "CANCEL") == 0)
	{
		/* CANCELLED */
		if (getItemsValue (msgDesc, qDesc, "item_status",
			"CANCELLED", &item_status, retStatus) < IMS_OK)
		{
			/* should never get here; hard coding the value */
			item_status = 12;
		}
	}
	else if (strcmp (status_id, "FAILED") == 0)
	{
		/* FAILED */
		if (getItemsValue (msgDesc, qDesc, "item_status",
			"FAILED", &item_status, retStatus) < IMS_OK)
		{
			/* should never get here; hard coding the value */
			item_status = 14;
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"An invalid status of '%s' was encountered for status type 'FINAL'.",
			status_id);
		return (IMS_ERROR);

	}

	/*
	** Populate the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"update order_item \
		set status = %d \
		where order_id = %d \
		and item_id = %d",
		item_status, order_id, item_id);

	/*
	** Assign the command buffer to the query descriptor.
	*/
	IMS_SETCMD (qDesc, cmdBuf);

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
	** Execute the command.
	*/
	if ((status = execCmd ( msgDesc, qDesc)) < IMS_OK)
	{
		*retStatus = status;
		return (IMS_ERROR);
	}

	/*
	** See if we updated one row.
	*/
	if (IMS_AFFECTED (qDesc) < 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not find a row to update for item '%d' of order '%d'.",
			item_id, order_id);
		*retStatus = IMS_NOROWS;
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getGranuleTable ()
**
** Get the granule table name for the given dataset.
**
******************************************************************************/

static int getGranuleTable (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	char *dataset,
	char *granules_table,
	int *retStatus)
{
	static char cmdBuf[IMS_COL512_LEN];
	int status;
	int j;

	/*
	** Populate the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"select granules_table \
		from dataset_policy d, dataset_relation r \
		where r.dataset = '%s' and r.dataset_idx = d.dataset_idx",
		dataset);

	/*
	** Assign the command buffer to the query descriptor.
	*/
	IMS_SETCMD (qDesc, cmdBuf);

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (status);
	}

	/*
	** Execute the command.
	*/
	if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
	{
		*retStatus = status;
		return (IMS_ERROR);
	}

	/*
	** See if we got one row returned.
	*/
	if (IMS_AFFECTED (qDesc) < 1)
	{
		return (IMS_ERROR);
	}

	/*
	** Get the granules_table name.
	*/
	(void) memcpy (granules_table,
		qDesc->valAddr[0], qDesc->valLength[0]);
	granules_table[qDesc->valLength[0]] = '\0';

	return (IMS_OK);
}

/******************************************************************************
**
** getGranuleInfo ()
**
** Get the granule_idx and database_idx values from the granules table.
**
******************************************************************************/

static int getGranuleInfo (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	char *product_id,
	char *granule_table,
	int *granule_idx,
	int *dataset_idx,
	int *data_kbytes,
	int *metadata_kbytes,
	int *retStatus)
{
	static char cmdBuf[IMS_COL512_LEN];
	DBINT granuleTemp;
	DBSMALLINT datasetTemp;
	int status;

	/*
	** Populate the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"select granule_idx, dataset_idx, data_kbytes, "
		" metadata_kbytes from %s where name = '%s'",
		granule_table, product_id);

	/*
	** Assign the command buffer to the query descriptor.
	*/
	IMS_SETCMD (qDesc, cmdBuf);

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (status);
	}

	/*
	** Execute the command.
	*/
	if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
	{
		*retStatus = status;
		return (IMS_ERROR);
	}

	/*
	** Check the number of rows returned.
	*/
	if (IMS_AFFECTED (qDesc) < 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"No rows of granule information returned.");
		*retStatus = IMS_NOROWS;
		return (IMS_ERROR);
	}

	if (IMS_AFFECTED (qDesc) > 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"More than one row of granule information returned.");
		return (IMS_ERROR);
	}

	/*
	** Copy the returned data into the structure.
	*/

	/* granule_idx */
	(void) memcpy (&granuleTemp,
		qDesc->valAddr[0], qDesc->valLength[0]);

	/* dataset_idx */
	(void) memcpy (&datasetTemp,
		qDesc->valAddr[1], qDesc->valLength[1]);

	*granule_idx = granuleTemp;
	*dataset_idx = datasetTemp;

	(void) memcpy (&granuleTemp,
		qDesc->valAddr[2], qDesc->valLength[2]);
	*data_kbytes = granuleTemp;

	(void) memcpy (&granuleTemp,
		qDesc->valAddr[3], qDesc->valLength[3]);
	*metadata_kbytes = granuleTemp;

	return (IMS_OK) ;
}

/********************************************************************
**
** ims_darFrame ()
**
** Reports dar-frame relationships to IMS.
** This function will update the dar_frame table with the data
** reported via the odlBuffer parameter.
**
**********************************************************************/

int ims_darFrame (
		  IMS_CMN_QUERY  *query,
		  char           *odlBuffer)
{
	int               status;
	IMS_MSG_STRUCT   *msgDesc;
	IMS_QI_DESC_OBJ  *qDesc;
	IMS_KEYWORD_LIST *keyListFirst;
	IMS_KEYWORD_LIST *keyListOut;
	int               order_id;
	short             item_id;
	static char       cmdBuf[IMS_COL512_LEN];
	char              targetObject[IMS_NAME_LEN+1];
	char              platform[31];
	int               revolution;
	int               frame_id;


	/*
	** Initialize variables.
	*/
	status = IMS_OK;
	query->retStatus = IMS_OK;
	msgDesc = query->msgDesc;
	qDesc = query->qDesc;

	/*
	** Check for an active database connection.
	*/
	if ((status = checkConnection (query)) < IMS_OK)
	{
	(void) ims_msg (msgDesc, status,
		"ims_darFrame: The database server connection was not valid.");
	query->retStatus = IMS_NOCONNECT;
	return (status);
	}

	/*
	** Assign the command buffer to the query descriptor.
	*/
	IMS_SETCMD (qDesc, cmdBuf);

	/*
	 ** Parse the ODL buffer into a linked list.
	 ** Only parse the 'IMS_DAR_FRAME' group.
	 */
	(void) strcpy (targetObject, "IMS_DAR_FRAME");

   /*
	** Parse the ODL buffer into a linked list.
	*/
	if ((status = ims_parseODLBuffer (msgDesc,
		 odlBuffer, targetObject, &keyListFirst)) < IMS_OK)
	{
	(void) ims_msg (msgDesc, status,
		"ims_darFrame: No values output from ODL buffer.");
	return (status);
	}

	/*
	** Get the order_id value from the odl list.
	*/
	if ((status = ims_findListItem (msgDesc, keyListFirst, "ORDER_ID",
				   IMS_KEYWORD, IMS_TRUE, &keyListOut)) < IMS_OK)
	{
	(void) ims_msg (msgDesc, status,
		   "ims_darFrame: Could not find ORDER_ID value in ODL buffer.");
	(void) ims_freeKeywordList (keyListFirst);
	return (status);
	}
	else /* Get the associated value. */
	{
	if (keyListOut->data_type == IMS_INT4_TYPE)
	{
		order_id = keyListOut->value_integer;
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
				"Keyword '%s', has an invalid data type.",
				keyListOut->keyword);
		(void) ims_freeKeywordList (keyListFirst);
		status = IMS_ERROR;
		return (status);
	}
	}

	/*
	** Get the item_id value from the odl list.
	*/
	if ((status = ims_findListItem (msgDesc, keyListFirst, "ITEM_ID",
				   IMS_KEYWORD, IMS_TRUE, &keyListOut)) < IMS_OK)
	{
	(void) ims_msg (msgDesc, IMS_ERROR,
		   "ims_darFrame: Could not find ITEM_ID value in ODL buffer.");
	(void) ims_freeKeywordList (keyListFirst);
	status = IMS_ERROR;
	return (status);
	}
	else /* Get the associated value. */
	{
	if (keyListOut->data_type == IMS_INT4_TYPE)
	{
		if (keyListOut->value_integer > IMS_MAX_SINT)
		{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Keyword 'ITEM_ID' has an invalid value: %d.",
			keyListOut->value_integer);
		(void) ims_freeKeywordList (keyListFirst);
		status = IMS_ERROR;
		return (status);
		}
		else
		{
		item_id = keyListOut->value_integer;
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
				"Keyword '%s', has an invalid data type.",
				keyListOut->keyword);
		(void) ims_freeKeywordList (keyListFirst);
		status = IMS_ERROR;
		return (status);
	}
	}

	/*
	** Verify that an order_item table entry exists for the
	** given order_id and item_id values.
	*/
	if ((status = validateOrderItem (msgDesc, qDesc,
		  order_id, item_id, &(query->retStatus))) < IMS_OK)
	{
	(void) ims_msg (msgDesc, status,
	"ims_darFrame: Could not find order_item entry with order "
		"id: %d, item_id: %d.",
		order_id, item_id);
	(void) ims_freeKeywordList (keyListFirst);
	(void) ims_qiResetDesc (qDesc);
	return (status);
	}

	/*
	** Get the first IMS_FRAME group from the odl list.
	*/
	if ((status = ims_findListItem (msgDesc, keyListFirst, "IMS_FRAME",
					   IMS_OBJECT, IMS_TRUE, &keyListOut)) < IMS_OK)
	{
	(void) ims_msg (msgDesc, IMS_ERROR,
		"ims_darFrame: Could not find IMS_FRAME group in ODL buffer.");
	(void) ims_freeKeywordList (keyListFirst);
	(void) ims_qiResetDesc (qDesc);
	return (status);
	}

	/*
	** Begin the update transaction.
	*/
	if ((status = setTransState (msgDesc, qDesc, "begin")) < IMS_OK)
	{
	(void) ims_msg (msgDesc, status,
		"Could not begin the update transaction.");
	(void) ims_freeKeywordList (keyListFirst);
	(void) ims_qiResetDesc (qDesc);
	return (status);
	}

	/*
	** Process each IMS_FRAME group in the odl list.
	*/
	while (keyListOut != (IMS_KEYWORD_LIST *) NULL)
	{
	/*
	** Process next IMS_FRAME object.
	*/
	if ((keyListOut->item_type != IMS_OBJECT) ||
		(strcmp (keyListOut->keyword, "IMS_FRAME") != 0))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		"ims_darFrame: Could not find IMS_FRAME group in ODL buffer.");
		(void) setTransState (msgDesc, qDesc, "rollback");
		(void) ims_freeKeywordList (keyListFirst);
		(void) ims_qiResetDesc (qDesc);
		status = IMS_ERROR;
		return (status);
	}

	/*
		** Get the platform value from the odl list.
		*/
	keyListOut = keyListOut->next;
	if (strcmp (keyListOut->keyword, "PLATFORM") != 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		"ims_darFrame: Could not find PLATFORM value in ODL buffer.");
		(void) setTransState (msgDesc, qDesc, "rollback");
		(void) ims_freeKeywordList (keyListFirst);
		(void) ims_qiResetDesc (qDesc);
		status = IMS_ERROR;
		return (status);
	}
	else /* Get the associated value. */
	{
		if ((keyListOut->data_type == IMS_SYMBOL_TYPE) ||
			(keyListOut->data_type == IMS_STRING_TYPE))
		{
		(void) strcpy (platform, keyListOut->value_string);
		}
		else
		{
		(void) ims_msg (msgDesc, IMS_ERROR,
				"Keyword '%s' has an invalid data type.",
				keyListOut->keyword);
		(void) setTransState (msgDesc, qDesc, "rollback");
		(void) ims_freeKeywordList (keyListFirst);
		(void) ims_qiResetDesc (qDesc);
		status = IMS_ERROR;
		return (status);
		}
	}

	/*
		** Get the revolution value from the odl list.
		*/
	keyListOut = keyListOut->next;
	if (strcmp (keyListOut->keyword, "REVOLUTION") != 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		"ims_darFrame: Could not find REVOLUTION value in ODL buffer.");
		(void) setTransState (msgDesc, qDesc, "rollback");
		(void) ims_freeKeywordList (keyListFirst);
		(void) ims_qiResetDesc (qDesc);
		status = IMS_ERROR;
		return (status);
	}
	else /* Get the associated value. */
	{
		if (keyListOut->data_type == IMS_INT4_TYPE)
		{
		revolution = keyListOut->value_integer;
		}
		else
		{
		(void) ims_msg (msgDesc, IMS_ERROR,
				"Keyword '%s' has an invalid data type.",
				keyListOut->keyword);
		(void) setTransState (msgDesc, qDesc, "rollback");
		(void) ims_freeKeywordList (keyListFirst);
		(void) ims_qiResetDesc (qDesc);
		status = IMS_ERROR;
		return (status);
		}
	}

	/*
		** Get the frame_id value from the odl list.
		*/
	keyListOut = keyListOut->next;
	if (strcmp (keyListOut->keyword, "FRAME_ID") != 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		"ims_darFrame: Could not find FRAME_ID value in ODL buffer.");
		(void) setTransState (msgDesc, qDesc, "rollback");
		(void) ims_freeKeywordList (keyListFirst);
		(void) ims_qiResetDesc (qDesc);
		status = IMS_ERROR;
		return (status);
	}
	else /* Get the associated value. */
	{
		if (keyListOut->data_type == IMS_INT4_TYPE)
		{
		frame_id = keyListOut->value_integer;
		}
		else
		{
		(void) ims_msg (msgDesc, IMS_ERROR,
				"Keyword '%s' has an invalid data type.",
				keyListOut->keyword);
		(void) setTransState (msgDesc, qDesc, "rollback");
		(void) ims_freeKeywordList (keyListFirst);
		(void) ims_qiResetDesc (qDesc);
		status = IMS_ERROR;
		return (status);
		}
	}

	if ((status = setDarFrameData (msgDesc, qDesc,
					  order_id, item_id, platform, revolution, frame_id,
					  &(query->retStatus))) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
				"ims_darFrame: Error in updating dar frame table for"
				" order_id: %d, item_id: %d.",
					order_id, item_id);
		(void) setTransState (msgDesc, qDesc, "rollback");
		(void) ims_freeKeywordList (keyListFirst);
		(void) ims_qiResetDesc (qDesc);
		return (status);
	}

		keyListOut = keyListOut->next;
	}

	/*
	** Commit the update transaction.
	*/
	if ((status = setTransState (msgDesc, qDesc, "commit")) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not commit the update transaction.");
		(void) ims_freeKeywordList (keyListFirst);
		(void) ims_qiResetDesc (qDesc);
		return (status);
	}

	(void) ims_freeKeywordList (keyListFirst);
	(void) ims_qiResetDesc(qDesc);

	(void) ims_msg (msgDesc, IMS_INFO,
		"Successfully inserted the dar-frame relationships for "
		"item '%d' of order '%d'.",
		item_id, order_id);

	return (status);

}   /* ims_darFrame */


/********************************************************************
**
** setDarFrameData ()
**
** This function will update the dar frame table with the reported
** data for the given order_id, and item_id.
**
**********************************************************************/
static int setDarFrameData (
				IMS_MSG_STRUCT  *msgDesc,
				IMS_QI_DESC_OBJ *qDesc,
				int              order_id,
				short            item_id,
				char            *platform,
				int              revolution,
				int              frame_id,
				int             *retStatus)
{
	int               status;
	static char       cmdBuf[IMS_COL512_LEN+1];


	status = IMS_OK;

	(void) sprintf (cmdBuf,
	"insert dar_frame (order_id, item_id, PLATFORM, REVOLUTION, FRAME_ID)\n"
	"values (%d, %d, '%s', %d, %d)",
	order_id, item_id, platform, revolution, frame_id);

	/*
	 ** Assign the command buffer to the query descriptor.
	 */
	IMS_SETCMD (qDesc, cmdBuf);

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc(qDesc)) < IMS_OK)
	{
	(void) ims_msg (msgDesc, status,
		"Could not reset the query descriptor.");
	return (status);
	}

	/*
	 ** Process the result row for this query.
	 */
	if ((status = execCmd (msgDesc, qDesc)) < IMS_OK)
	{
	(void) ims_msg (msgDesc, IMS_ERROR,
		"setDarFrameData: Dar Frame table update failed.");
	*retStatus = status;
	status = IMS_ERROR;
	return (status);
	}

	return (IMS_OK);

} /* setDarFrameData */

/********************************************************************
**
** validateOrderItem ()
**
** This function will validate order_id and item_id values by
** verifying the existence of an order_id table entry with
** the given order_id and item_id values.
**
**********************************************************************/
static int validateOrderItem (
				  IMS_MSG_STRUCT      *msgDesc,
				  IMS_QI_DESC_OBJ     *qDesc,
				  int                  order_id,
				  short                item_id,
				  int                 *retStatus)
{
	int               status;
	static char       cmdBuf[IMS_COL512_LEN+1];


	/*
	** Initialize local variables.
	*/
	status = IMS_OK;
	*retStatus = IMS_OK;

	/*
	** validate_order will verify the existence of an order_item entry.
	*/
	(void) sprintf (cmdBuf, "validate_order %d, %d", order_id, item_id);

	/*
	** Assign the command buffer to the query descriptor.
	*/
	IMS_SETCMD (qDesc, cmdBuf);

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
	(void) ims_msg (msgDesc, IMS_ERROR,
		"Could not reset the query descriptor.");
	return (IMS_ERROR);
	}

	/*
	 ** Execute the stored procedure.
	 */
	if ((status = execCmd ( msgDesc, qDesc)) < IMS_OK)
	{
	*retStatus = status;
	return (IMS_ERROR);
	}

	return (IMS_OK);

}  /*  validateOrderItem  */

/******************************************************************************
**
** checkConnection ()
**
** Check the qDesc pointer for a valid database server connection.
**
******************************************************************************/

static int checkConnection (
	IMS_CMN_QUERY *query)
{
	int status;

	/*
	** If this in NULL we can't possibly have a connection.
	*/
	if (query->qDesc  == (IMS_QI_DESC_OBJ *) NULL)
	{
		return (IMS_ERROR);
	}
	else
	{
		/*
		** Resetting the query descriptor will validate it.
		*/
		if ((status = ims_qiResetDesc (query->qDesc)) < IMS_OK)
		{
			return (status);
		}
		else
		{
			/*
			** See if the DBPROCESS has been marked dead.
			*/
			if (DBDEAD (query->qDesc->dbproc) == TRUE)
			{
				return (IMS_ERROR);
			}
		}
	}
	return (IMS_OK);
}  /* checkConnection */

/******************************************************************************
**
** execCmd ()
**
** Execute a query. This function should only be used for commands
** that return one or no rows.
**
******************************************************************************/

static int execCmd (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc)
{
	int status;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			if (qDesc->msgNo == IMS_SYB_DEADLOCK)
			{
				status = IMS_DEADLOCK;
			}
			else
			{
				status = IMS_QUERYFAIL;
			}
			return (status);
		}
	}

	/*
	** Check the stored procedure status returned value.
	*/
	if (checkRetStatus (msgDesc, qDesc) < IMS_OK)
	{
		return (IMS_QUERYFAIL);
	}

	return (IMS_OK);
}  /* execCmd */

/******************************************************************************
**
** checkRetStatus ()
**
** Check the procedure returned status value.
** When status returned is not an error, then return IMS_OK.
**
******************************************************************************/

static int checkRetStatus (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc)
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
				"Sybase procedure '%s' returned a status of %d",
				qDesc->cmd, procReturn);
			return (severity);
		}
	}

	return (IMS_OK);
}  /* checkRetStatus */

/******************************************************************************
**
** getOrderLock ()
**
** Execute stored procedure get_order_lock
**
******************************************************************************/

static int getOrderLock (
	IMS_MSG_STRUCT      *msgDesc,
	IMS_QI_DESC_OBJ     *qDesc)
{
	/*
	** Execute stored procedure get_order_lock
	*/
	(void) sprintf (qDesc->cmd, "get_order_lock");

	if (execCmd (msgDesc, qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"execution of stored procedure get_order_lock failed.");
		return (IMS_FATAL);
	}

	return (IMS_OK);
}
