static char *sccs = "@(#)ims_dispHandler.c	5.6  06/09/97";

/**************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
**   ims_dispHandler - Submits orders to appropriate subsystems 
**
**   Creator    : Julie Wang 
** 
**   Date       : June 13 1995 
**   
**   History:  
**
**           07/24/96   jwang   Stop sending PPS bound ODLs if DCE error 
**                              occurred.
**
**           02/27/96   jwang   R1Bprime preliminary.
**
***************************************************************************/

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

/* TxTree for PPS bound message is global */
AGGREGATE  TxTree;

#define BUF_SIZE 1024

/*
** static routines
*/
static int collect_order_list (IMS_MSG_STRUCT *, DISP_DESC_STRUCT *);
static int dispDesc_init(IMS_MSG_STRUCT *, char *, char *,DISP_DESC_STRUCT *);
static int dispDesc_cleanup (DISP_DESC_STRUCT *);
static int transmit_order_item (IMS_MSG_STRUCT *, V0_CAT_STRUCT *, 
					DISP_FULL_LIST *);
static int init_disp_full_list (DISP_FULL_LIST *); 

/***********************************************************************
** 
** ims_dispHandler - 
**
** return IMS_OK    if successful
**        IMS_FATAL if system error occurs
************************************************************************/
int ims_dispHandler (IMS_MSG_STRUCT *msgDesc, 
                     char *glbLogin, 
                     char *glbPassword)
{
	DISP_DESC_STRUCT dispDesc;
	V0_CAT_STRUCT    catReq;
	DISP_FULL_LIST  *o_list;
	int              status;

	if ( (status = dispDesc_init (msgDesc, glbLogin , glbPassword, &dispDesc))
	               < IMS_OK)
	{
		(void) ims_msg (msgDesc, status, 
		               "ims_dispHandler: failed to initialize	disp descriptor.");
		return (status);
	}

	/*
	** open db connection
	*/

	if ( (status = v0_cat (&dispDesc.catReq, V0_OPENCONNECTION)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"ims_dispHandler: Database login failed."); 
		(void) dispDesc_cleanup (&dispDesc);
		if (dispDesc.catReq.qDesc != (IMS_QI_DESC_OBJ *) NULL)
		{
			(void) ims_qiFreeDesc(dispDesc.catReq.qDesc);
		}
		return (status);
	}

	catReq = dispDesc.catReq;

	/*
	** we are not going to continue either when error occurs during
	** order list collection (IMS_ERROR or IMS_FATAL) or 
	** if nothing found in the order_item table (IMS_OK and the list is
	** null) that needs to be taken care of
	*/
	if ( ((status = collect_order_list (msgDesc, &dispDesc)) < IMS_OK) ||
			 (dispDesc.disp_list == (DISP_FULL_LIST *)NULL) )
	{
		(void) dispDesc_cleanup (&dispDesc);

		if ( (v0_cat (&catReq, V0_CLOSECONNECTION)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"ims_dispHandler: Database close failed."); 
			return (IMS_FATAL);
		}

		return (status);
	} 

	o_list = dispDesc.disp_list;

	/*
	** process order items one-by-one
	*/
	while (o_list != (DISP_FULL_LIST *)NULL)
	{ 

		TxTree = (AGGREGATE)NULL;

		/*
		** void the return value.  All error conditions have been handled 
		** before returning.
		*/
		(void) transmit_order_item (msgDesc, &catReq, o_list) ; 

		if (TxTree != (AGGREGATE)NULL )
		{
			(void) v0_msgTree__destroy(TxTree);
		}

		o_list = o_list->next_p;

	} /* while not end of order list */

	/*
	** clean up allocated space
	*/
	(void) dispDesc_cleanup (&dispDesc);

	/*
	** close db connection
	*/
  if ( v0_cat (&catReq, V0_CLOSECONNECTION) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"ims_dispHandler: Database close failed."); 
		return (IMS_FATAL);
	}

	return (IMS_OK);

} /* end of ims_dispHandler */

/***********************************************************************
** 
** collect_order_list - collects a list of order_id and item_id which
**        have 'validated' status
**
** return IMS_OK    if successful
**        IMS_FATAL if system error occurs
************************************************************************/
static int collect_order_list (IMS_MSG_STRUCT *msgDesc, 
                               DISP_DESC_STRUCT *dispDesc)
{
	V0_CAT_STRUCT    catReq;
	char             sql[IMS_COL1024_LEN];
	int              rowCount;
	char             msgbuf[IMS_COL255_LEN+1];
	DISP_ORDER_LIST  *ho;
	DISP_SV_LIST     *hs;
	DISP_FULL_LIST   *temp;
	int              first_order_found;

	ims_msg (msgDesc, IMS_INFO, "Collecting items for Order Dispatcher!");

	first_order_found = 0;

	catReq = dispDesc->catReq;

	sql[0] = '\0';

	(void)sprintf (sql, "select order_id, item_id, status from order_item where status in (%d, %d, %d, %d)", IMS_VALIDATED_ITEM, IMS_ON_LINE_ITEM, IMS_ON_FILM_ITEM, IMS_CANCEL_ITEM); 
	sql[strlen(sql)] = '\0';

	catReq.item[0] = (void *)sql;		
	rowCount = 0;
	catReq.item[1] = (int *)&rowCount;

	msgbuf[0] = '\0';

#	ifdef QDEBUG
	(void)sprintf (msgbuf, "sql ==> %s", sql);
	(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#	endif

	if ( disp_cat (&catReq, DISP_GETITEMLIST) < IMS_OK )
	{
		return (IMS_FATAL);
	}

	/*
	** it's okay if the list is empty, just return IMS_OK
	*/
	if (rowCount != 0)
	{
		ho = (DISP_ORDER_LIST *)catReq.item[2]; 
		dispDesc->o_list = ho;

		while (ho !=  (DISP_ORDER_LIST *)NULL)
		{
			if (first_order_found == 0)
			{
				if ((temp = (DISP_FULL_LIST *) malloc (sizeof (DISP_FULL_LIST))) == NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL, 
			  		"ims_dispHandler__collect_order_list: Memory allocation failed for DISP_FULL_LIST.");
					return (IMS_FATAL);
				}

				(void)init_disp_full_list (temp);

				temp->order_id = ho->order_id;
				temp->item_id  = ho->item_id;
				temp->initial_status = ho->initial_status;
				temp->first_item_flag = 1;

				dispDesc->disp_list = temp;
				first_order_found = 1;
			}
			else
			{
				if ((temp->next_p=(DISP_FULL_LIST*)malloc(sizeof(DISP_FULL_LIST)))==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL, 
			  		"ims_dispHandler__collect_order_list: Memory allocation failed for DISP_FULL_LIST.");
					return (IMS_FATAL);
				}

				temp = temp->next_p;

				(void)init_disp_full_list (temp);

				temp->order_id = ho->order_id;
				temp->item_id  = ho->item_id;
				temp->initial_status = ho->initial_status;

			}

			ho = ho->next_p;

		}/*while*/
	}

	sql[0] = '\0';

	(void)sprintf (sql, 
	   "select dataset_idx, granule_idx, PLATFORM, START_REV, \
			 END_REV, START_TIME, END_TIME, SV_PRECISION from sv_available \
			 where status = %d",  IMS_SV_NEW);
	sql[strlen(sql)] = '\0';

	catReq.item[0] = (void *)sql;		
	rowCount = 0;
	catReq.item[1] = (int *)&rowCount;

	msgbuf[0] = '\0';

#	ifdef QDEBUG
	(void)sprintf (msgbuf, "sql ==> %s", sql);
	(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#	endif

	if ( disp_cat (&catReq, DISP_GETNEWSV) < IMS_OK )
	{
		return (IMS_FATAL);
	}

	/*
	** it's okay if the list is empty, just return IMS_OK
	*/
	if (rowCount != 0)
	{
		hs = (DISP_SV_LIST *)catReq.item[2]; 
		dispDesc->sv_list = hs;

		while (hs !=  (DISP_SV_LIST *)NULL)
		{
			if (first_order_found == 0)
			{
				if ((temp = (DISP_FULL_LIST *) malloc (sizeof (DISP_FULL_LIST))) == NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL, 
			  		"ims_dispHandler__collect_order_list: Memory allocation failed for DISP_FULL_LIST.");
					return (IMS_FATAL);
				}

				(void)init_disp_full_list (temp);

				temp->dataset_idx = hs->dataset_idx;
				temp->granule_idx = hs->granule_idx;
				strcpy (temp->platform, hs->platform);
				temp->start_rev = hs->start_rev;
				temp->end_rev = hs->end_rev;
				strcpy (temp->start_time, hs->start_time);
				strcpy (temp->end_time, hs->end_time);
				strcpy (temp->sv_precision, hs->sv_precision);

				/*
				** assign order_id as -1, which is an indication of STATE VECTOR
				** availability
				*/
				temp->order_id = -1;
				temp->first_item_flag = 1;

				dispDesc->disp_list = temp;
				first_order_found = 1;
			}
			else
			{
				if ((temp->next_p=(DISP_FULL_LIST*)malloc(sizeof(DISP_FULL_LIST)))==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL, 
			  		"ims_dispHandler__collect_order_list: Memory allocation failed for DISP_FULL_LIST.");
					return (IMS_FATAL);
				}

				temp = temp->next_p;

				(void)init_disp_full_list (temp);

				temp->dataset_idx = hs->dataset_idx;
				temp->granule_idx = hs->granule_idx;
				strcpy (temp->platform, hs->platform);
				temp->start_rev = hs->start_rev;
				temp->end_rev = hs->end_rev;
				strcpy (temp->start_time, hs->start_time);
				strcpy (temp->end_time, hs->end_time);
				strcpy (temp->sv_precision, hs->sv_precision);

				/*
				** assign order_id as -1, which is an indication of STATE VECTOR
				** availability
				*/
				temp->order_id = -1;
			}

			hs = hs->next_p;
		}/*while*/
	}
	return (IMS_OK);

} /* end of collect_order_list */

/**********************************************************************
**
** transmit_order_item - 
**        send an order item to step handling routine to move the order
**        to the next processing phase
**
** return IMS_OK    if successful
**        IMS_FATAL if system error occurs
**
**********************************************************************/
static int transmit_order_item (IMS_MSG_STRUCT *msgDesc, 
                                V0_CAT_STRUCT *catReq, 
                                DISP_FULL_LIST *disp_list)
{
		
	int  status, rollback_status;
	char sql[IMS_COL1024_LEN];
	char msgbuf[IMS_COL1024_LEN];

	/* 
	** begin transaction
	*/
	if ( v0_cat (catReq, V0_BEGINTRANSACTION) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"ims_dispHandler__transmit_order_item: begin transaction failed."); 

		return (IMS_FATAL);
	}


	/*
	** get an order lock
	*/
	/* this may not be needed for Order Dispatcher -jlw
	if ( disp_cat (catReq, DISP_GETORDERLOCK) < IMS_OK)
	{
		if ( v0_cat (catReq, V0_ROLLBACKTRANSACTION) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
						 "ims_dispHandler__transmit_order_item: transaction rollback failed.");
		}

		return (IMS_FATAL);
	}
	*/


	/*
	** if this is not a STATE VECTOR available condition, then double check 
	** if the status is still VALIDATED, ON_LINE, ON_FILM, or CANCEL
	*/
	if (disp_list->order_id != -1)
	{
		sql[0] = '\0';
		(void)sprintf (sql, "exec disp_recheck_status %d, %d, %d", 
			disp_list->order_id, disp_list->item_id, disp_list->initial_status); 
		sql[strlen(sql)] = '\0';

		catReq->item[0] = (void *)sql;		

		msgbuf[0] = '\0';

#	ifdef QDEBUG
		(void)sprintf (msgbuf,"sql ==> %s", sql);
		(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#	endif

		/*
		** if status on order_item table is not the same as what we get before
		** the transaction began, send a syslog message and rollback so it can 
		** be retried
		*/
		if ( ( status = disp_cat (catReq, DISP_RECHECKSTATUS)) < IMS_OK)
		{
				(void) ims_msg (msgDesc, IMS_WARNING,
					 "ims_dispHandler__transmit_order_item: status of this order item has been changed unexexpectly. order_id %d, item_id %d, original status was %d", 
		            disp_list->order_id,
								disp_list->item_id,
								disp_list->initial_status);

				if (v0_cat(catReq, V0_ROLLBACKTRANSACTION) < IMS_OK)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
					 		"ims_dispHandler__transmit_order_item: transaction rollback failed.");
					return (IMS_FATAL);
				}

				return (IMS_WARNING);
		}
	}


	/* 
	** if statuses are consistant, advance to next step
	**
	**    if successful, commit transaction
	**    else, rollback
	*/
				
	if ( (status=ims_do_next_step
				(msgDesc, catReq->qDesc, disp_list) ) < IMS_OK)
	{

		/*
		** when error occured, rollback the transaction so this entry can be 
		** resubmitted at a later time
		*/
		if (v0_cat(catReq, V0_ROLLBACKTRANSACTION) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
						 "ims_dispHandler__transmit_order_item: transaction rollback failed.");
			return (IMS_FATAL);
		}

	} 
		
	else 
	{
		if ( v0_cat (catReq, V0_COMMITTRANSACTION) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
						 "ims_dispHandler__transmit_order_item: transaction rollback failed.");

			if ((rollback_status=v0_cat(catReq,V0_ROLLBACKTRANSACTION)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
							 	"ims_dispHandler__transmit_order_item: transaction rollback failed.");
			}

			return (IMS_FATAL);
		}
	}

	return (IMS_OK);

}/* end of transmit_order_item */

/**********************************************************************
**
** dispDesc_init - 
**
**********************************************************************/
static int dispDesc_init (IMS_MSG_STRUCT *msgDesc, 
                          char *glbLogin, 
													char *glbPassword, 
													DISP_DESC_STRUCT *dispDesc)
{

	dispDesc->msgDesc = msgDesc;

	/*
	** initialize catReq
	*/
	dispDesc->catReq.qDesc = (IMS_QI_DESC_OBJ *)NULL;
	dispDesc->catReq.msgDesc = msgDesc;
	dispDesc->catReq.userSpec.dbUserName = glbLogin;
	dispDesc->catReq.userSpec.dbPassword = glbPassword;
	dispDesc->catReq.userSpec.server     = DISP_SERVER;
	dispDesc->catReq.userSpec.dbName     = DISP_DBNAME;
	dispDesc->catReq.userSpec.program    = DISP_PROGRAM;

	dispDesc->o_list     = (DISP_ORDER_LIST *)NULL;
	dispDesc->sv_list    = (DISP_SV_LIST *)NULL;
	dispDesc->disp_list  = (DISP_FULL_LIST *)NULL;

	return (IMS_OK);

} /* end of dispDesc_init */

/**********************************************************************
**
** init_disp_full_list - 
**
**********************************************************************/
static int init_disp_full_list (DISP_FULL_LIST *list) 
{
	list->order_id = 0;
	list->item_id  = 0;
	list->initial_status = 0;
	list->dataset_idx = 0;
	list->granule_idx = 0;
	list->platform[0] = '\0';
	list->start_rev   = 0;
	list->end_rev     = 0;
	list->start_time[0] = '\0';
	list->end_time[0]   = '\0';
	list->sv_precision[0]  = '\0';
	list->first_item_flag  = 0;
	list->next_p  = (DISP_FULL_LIST *)NULL;

	return (IMS_OK);

} /* end of init_disp_full_list */

/**********************************************************************
**
** dispDesc_cleanup - 
**
**********************************************************************/
static int dispDesc_cleanup (DISP_DESC_STRUCT *dispDesc)
{
	DISP_FULL_LIST *hf_ptr, *tf_ptr;
	DISP_ORDER_LIST *ho_ptr, *to_ptr;
	DISP_SV_LIST *hs_ptr, *ts_ptr;

	if (dispDesc->disp_list != (DISP_FULL_LIST *)NULL)
	{
		hf_ptr = dispDesc->disp_list;

		while ( hf_ptr != (DISP_FULL_LIST *)NULL)
		{
			tf_ptr = hf_ptr->next_p;
			free(hf_ptr);
			hf_ptr = tf_ptr;
		}
	}

	if (dispDesc->o_list != (DISP_ORDER_LIST *)NULL)
	{
		ho_ptr = dispDesc->o_list;

		while ( ho_ptr != (DISP_ORDER_LIST *)NULL)
		{
			to_ptr = ho_ptr->next_p;
			free(ho_ptr);
			ho_ptr = to_ptr;
		}
	}

	if (dispDesc->sv_list != (DISP_SV_LIST *)NULL)
	{
		hs_ptr = dispDesc->sv_list;

		while ( hs_ptr != (DISP_SV_LIST *)NULL)
		{
			ts_ptr = hs_ptr->next_p;
			free(hs_ptr);
			hs_ptr = ts_ptr;
		}
	}

	return (IMS_OK);
} /* dispDesc_cleanup */



