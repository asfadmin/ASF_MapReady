static char *sccs = "@(#)ims_step.c	5.12  06/27/97";

/**************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
**   ims_step - functions to update IMS step information
**
**   Creator    : Julie Wang 
** 
**   Date       : Aug 2 1995 
**
**   History:  
**
**            1/09/97   jwang   Retrieve RECORDER_TYPE as MEDIA_TYPE for
**                              Scan requests.
**
**           12/12/96   jwang   Added DATA_DIRECTION for L1 and Scan requests
**                              and COMPENSATION_FLAG for L1 request
**
**           07/24/96   jwang   Hold PPS bound ODLs if DCE error 
**                              occurred. 
**
**           07/24/96   jwang   Consolidated SV avail info into main worklist
**                              structure
**
**           07/24/96   jwang   Error msg enhancement.
**
**           06/06/96   jwang   Modified some error messages
**
**           02/27/96   jwang   R1Bprime preliminary.
**
***************************************************************************/

/*
** Undefine Posix Source flag because of imcompatibility
** with IK include files.
*/
#undef _POSIX_SOURCE

#include <stdio.h>
#include <string.h>

#ifdef DCEON
#include <PPSerr.h>        
#include <ims_ppsDefs.h>
#include <messages.h>        
#include <check_status.h>
#endif

#include <odldef.h>
#include <odlinter.h>
#include <ims_query.h>
#include <ims_v0.h>
#include <ims_disp.h>
#include <ims_timeConv.h>
#include <ims_acct.h>

extern char *glbDirectory;
extern AGGREGATE TxTree;

/* global variables and definitions */

#define BUF_SIZE 5000

static int no_dce;            /* set to 0 when ready to process first item
																 set to 1 when dce error occurred */

/* 
** local function declaration  
*/
static int get_item_info(IMS_MSG_STRUCT *,V0_CAT_STRUCT *,STEP_INFO *);
static int get_next_step(IMS_MSG_STRUCT *,V0_CAT_STRUCT *,STEP_INFO *);
static int ims_stepPPS  (IMS_MSG_STRUCT *,V0_CAT_STRUCT *,STEP_INFO *);
static int ims_stepFR   (IMS_MSG_STRUCT *,V0_CAT_STRUCT *,STEP_INFO *);
static int ims_stepLT   (IMS_MSG_STRUCT *,V0_CAT_STRUCT *,STEP_INFO *);
static int ims_stepPL   (IMS_MSG_STRUCT *,V0_CAT_STRUCT *,STEP_INFO *); 
static int ims_stepStatusChg (IMS_MSG_STRUCT *,V0_CAT_STRUCT *,STEP_INFO *); 
static int ims_cancel_order (IMS_MSG_STRUCT *, V0_CAT_STRUCT *, STEP_INFO *); 
static int ims_sv_avail (IMS_MSG_STRUCT *, V0_CAT_STRUCT *, STEP_INFO *); 
static int collectRFCVal(IMS_MSG_STRUCT *,V0_CAT_STRUCT *,STEP_INFO *);
static int collectTSRVal(IMS_MSG_STRUCT *,V0_CAT_STRUCT *,STEP_INFO *);
static int transmitPPSReq (IMS_MSG_STRUCT *, STEP_INFO *, int *);
static int update_before_submit (IMS_MSG_STRUCT *,V0_CAT_STRUCT *,STEP_INFO *); 
static int update_after_submit (IMS_MSG_STRUCT *,V0_CAT_STRUCT *,STEP_INFO *); 
static int record_error_status (IMS_MSG_STRUCT *,V0_CAT_STRUCT *,STEP_INFO *); 
static int check_pps_result ( IMS_MSG_STRUCT *,int,STEP_INFO *);
static int init_pps_req (STEP_INFO *);


/***********************************************************************
** 
** ims_do_next_step - entry point to IMS/DADS step handler 
**
** return:   IMS_OK    when succeeded or non-retriable error occurred. 
**                     Upon receiving this status, the caller does a COMMIT.
**           IMS_ERROR when retriable error occurrd.  
**                     Upon receiving this status, the callerr, does a ROLLBACK.
**
** ---------------------------------------------------------------------
** 
** Error handling policy:
**
** ims_do_next_step expects one of the two types of error returned from 
** routines it calls:
**
**   Type 1: Non retriable errors - IMS_ERROR 
**
**      Errors require changes in database contents (data/policy),  
**      or errors require interface modification between IMS and 
**      other subsystems, e.g. IMS/PPS.  
**
**      After received IMS_ERROR, ims_do_next_step does:
**          - update order_item table with ERROR status and op_comment,
**            if failed, treat it as type 2 error.
**          - return IMS_OK to the caller of ims_do_next_step so that 
**            a COMMIT will be performed
**
**   Type 2: Retriable errors - IMS_WARNING
**
**      Memory allocation errors, e.g. memory allocation errors, 
**      database access error e.g. unable to do table insert/update/select,
**      DCE communication error, or 
**      any error that does not require software/database modification. 
**      They can usually be resolved by a system or database server restart.
** 
**      After received IMS_WARNING, ims_do_next_step does:
**          - return IMS_ERROR to the the caller of ims_do_next_step so that 
**            a ROLLBACK will be performed
**
**
************************************************************************/
int ims_do_next_step (IMS_MSG_STRUCT  *msgDesc, 
                       IMS_QI_DESC_OBJ *qDesc, 
											DISP_FULL_LIST *list_item)
{

	V0_CAT_STRUCT catReq;
	int           status;
	char          msgbuf[IMS_COL1024_LEN];
	STEP_INFO     step;

	/*
	** initialize step strcture
	*/
	step.order_id            = list_item->order_id;
	step.item_id             = list_item->item_id;
	step.order_item_type     = 0;
	step.order_type[0]       = '\0';
	step.media_class         = 0;
	step.media_type          = 0;
	step.process_type        = 0;
	step.v0_process_type[0]  = '\0';
	step.step_name[0]        = '\0';
	step.step_sequence       = 0;
	step.status              = 0;
	step.step_started_p[0]   = '\0';
	step.start_status        = 0;
	step.end_status          = 0;
	step.quantity            = 0;
	step.priority            = 0;
	step.quicklook_p[0]      = '\0';
	step.curr_time[0]        = '\0';
	step.cost                = 0.0;
	step.account_id[0]       = '\0';
	step.process_status      = 0;
	step.op_comment[0]       = '\0';
	step.dce_err_flag        = 0;
	step.dataset_idx         = list_item->dataset_idx;
	step.granule_idx         = list_item->granule_idx;
	strcpy (step.platform, list_item->platform);
	step.start_rev           = list_item->start_rev;
	step.end_rev             = list_item->end_rev;
	strcpy (step.start_time, list_item->start_time);
	strcpy (step.end_time, list_item->end_time);
	strcpy (step.sv_precision, list_item->sv_precision);

	/*
	** first, check if the qDesc has been initialized. If not, initialize one.
	*/
	if (qDesc == (IMS_QI_DESC_OBJ *)NULL)
	{
		if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			      "ims_do_next_step: Fail to initialize query descriptor.");
	
			return (IMS_ERROR);
		}

	}

	/* 
	** reset static value no_dce if this is the first item  
	*/
	if (list_item->first_item_flag == 1)
	{
		no_dce = 0;
	}

	/*
	** initialize a catReq structure for local usage
	*/
	catReq.msgDesc  = msgDesc;
	catReq.qDesc    = qDesc;

	/*
	** order_id -1 is a new STATE VECTOR Avail request
	*/
	if (step.order_id == -1)
	{

		/*
		** if DCE error exists, skip this item and visit it again the next time
		*/
		if (no_dce)
		{
			return (IMS_ERROR);
		}

		if ( (status =ims_sv_avail (msgDesc, &catReq, &step) ) <IMS_OK)
		{

			/*
			** if DCE error occurred, we set the no_dce flag on
			*/
			if (step.dce_err_flag == 1)
			{
				no_dce = 1;
			}
				
			return (IMS_ERROR);
		}

		return (IMS_OK);
	}


	/*
	** collect step handling related information for the current order item
	*/
	if ( (status = get_item_info (msgDesc, &catReq, &step)) < IMS_OK)
	{
		if (status == IMS_ERROR)
		{
			if ( (record_error_status (msgDesc, &catReq, &step)) < IMS_OK)
			{
				return (IMS_ERROR);
			}
			else
			{
				return (IMS_OK);
			}
		} 
		else
		{
			return (IMS_ERROR);
		}
	}

	/*
	** only if current status is VALIDATED, ON_LINE, ON_FILM, and CANCEL requires 	** step handling 
	**
	** if the current status is other than the above, the software has to be
	** examined.
	*/
	if ( (step.status != IMS_VALIDATED_ITEM) &&
			 (step.status != IMS_ON_LINE_ITEM) &&
			 (step.status != IMS_ON_FILM_ITEM) &&
	     (step.status != IMS_CANCEL_ITEM))
	{
		msgbuf[0] ='\0';
		step.op_comment[0] = '\0'; 

		(void) sprintf (step.op_comment, 
		  "ims_do_next_step: step handler received unexpected current "
			"status %d, order_id  %d, item_id  %d. Software modification "
			"may be needed.", 
         step.status,
		     step.order_id, 
         step.item_id); 

		(void) sprintf (msgbuf, "%s", step.op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

		if ( (record_error_status (msgDesc, &catReq, &step)) < IMS_OK)
		{
			return (IMS_ERROR);
		}
		else
		{
			return (IMS_OK);
		}

	}	

	if (step.status == IMS_CANCEL_ITEM)
	{
		if ( (status =ims_cancel_order (msgDesc, &catReq, &step) ) <IMS_OK)
		{
			/*
			** if DCE error occurred, we set the no_dce flag on
			*/
			if (step.dce_err_flag == 1)
			{
				no_dce = 1;
			}
				
			/*
			** if non-retryable error occurred, update the status column in 
			** order_item to ERROR 
			*/
			if (status == IMS_ERROR)
			{
				if ( (record_error_status (msgDesc, &catReq, &step)) < IMS_OK)
				{
					return (IMS_ERROR);
				}
				/*
				** successfully recorded the error to order_item table, return an
				** IMS_OK so the db changes will be committed
				*/
				else
				{
					return (IMS_OK);
				}
			} 
			/*
			** retryable error occurred, return IMS_ERROR to rollback db changes
			*/
			else
			{
				return (IMS_ERROR);
			}
		}

		/* cancellation has been completed, return from here */
		return (IMS_OK);
	}

	/*
	** retrieve information for the next step
	*/
	if ( (status = get_next_step (msgDesc, &catReq, &step)) < IMS_OK)
	{
		if (status == IMS_ERROR)
		{
			if ( (record_error_status (msgDesc, &catReq, &step)) < IMS_OK)
			{
				return (IMS_ERROR);
			}
			else
			{
				return (IMS_OK);
			}
		} 
		else
		{
			return (IMS_ERROR);
		}
	}

	/*
	** start_status is supposed to match with the current status
	*/
	if (step.status != step.start_status)
	{
		msgbuf[0] = '\0';
		step.op_comment[0] = '\0'; 

		(void) sprintf (step.op_comment, 
		    "ims_do_next_step: inconsistant step info.  "
				"Current status %d does not match with step start_status %d, "
				"order_id %d item_id %d",
					 step.status,
					 step.start_status,
 		       step.order_id, 
		       step.item_id); 

		(void) sprintf (msgbuf, "%s", step.op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

		if ( (record_error_status (msgDesc, &catReq, &step)) < IMS_OK)
		{
			return (IMS_ERROR);
		}
		else
		{
			return (IMS_OK);
		}
	}
	
	/*
	** check step name to decide what action to take next
	*/

	if (strcmp (step.step_name, "PPF PROCESS") == 0)
	{
		/*
		** if DCE error exists, skip this item and visit it again the next time
		*/
		if (no_dce)
		{
			return (IMS_ERROR);
		}

		if ( (status = ims_stepPPS (msgDesc, &catReq, &step)) < IMS_OK )
		{
			/*
			** if DCE error occurred, we set the no_dce flag on
			*/
			if (step.dce_err_flag == 1)
			{
				no_dce = 1;
			}
				
			/*
			** if non-retryable error occurred, update the status column in 
			** order_item to ERROR 
			*/
			if (status == IMS_ERROR)
			{
				if ( (record_error_status (msgDesc, &catReq, &step)) < IMS_OK)
				{
					return (IMS_ERROR);
				}
				/*
				** successfully recorded the error to order_item table, return an
				** IMS_OK so the db changes will be committed
				*/
				else
				{
					return (IMS_OK);
				}
			} 
			/*
			** retryable error occurred, return IMS_ERROR to rollback db changes
			*/
			else
			{
				return (IMS_ERROR);
			}
		}
	}

	else if (strcmp (step.step_name, "FIRE RECORDER") == 0)
	{
		if ( (status = ims_stepFR (msgDesc, &catReq, &step)) < IMS_OK )
		{
		
			/*
			** if non-retryable error occurred, update the status column in 
			** order_item to ERROR 
			*/
			if (status == IMS_ERROR)
			{
				if ( (record_error_status (msgDesc, &catReq, &step)) < IMS_OK)
				{
					return (IMS_ERROR);
				}
				/*
				** successfully recorded the error to order_item table, return an
				** IMS_OK so the db changes will be committed
				*/
				else
				{
					return (IMS_OK);
				}
			} 
			/*
			** retryable error occurred, return IMS_ERROR to rollback db changes
			*/
			else
			{
				return (IMS_ERROR);
			}
		}
		
#ifdef DEBUG
		(void)ims_msg (msgDesc, IMS_INFO,
		  "ims_do_next_step: successfully inserted  "
			"order_id %d item_id %d to fire queue.",
		     step.order_id,
		     step.item_id);
#endif 	   

	}

	else if (strcmp (step.step_name, "LASER TECH") == 0)
	{
		if ( (status = ims_stepLT (msgDesc, &catReq, &step)) < IMS_OK )
		{
			/*
			** if non-retryable error occurred, update the status column in 
			** order_item to ERROR 
			*/
			if (status == IMS_ERROR)
			{
				if ( (record_error_status (msgDesc, &catReq, &step)) < IMS_OK)
				{
					return (IMS_ERROR);
				}
				/*
				** successfully recorded the error to order_item table, return an
				** IMS_OK so the db changes will be committed
				*/
				else
				{
					return (IMS_OK);
				}
			} 
			/*
			** retryable error occurred, return IMS_ERROR to rollback db changes
			*/
			else
			{
				return (IMS_ERROR);
			}
		}
		
#ifdef DEBUG
		(void)ims_msg (msgDesc, IMS_INFO,
		      "ims_do_next_step: successfully inserted "
					"order_id %d item_id %d to laser queue.",
		        step.order_id,
		        step.item_id);
#endif 

	}

	else if (strcmp (step.step_name, "PHOTO LAB") == 0)
	{
		if ( (status = ims_stepPL (msgDesc, &catReq, &step)) < IMS_OK )
		{
			/*
			** if non-retryable error occurred, update the status column in 
			** order_item to ERROR 
			*/
			if (status == IMS_ERROR)
			{
				if ( (record_error_status (msgDesc, &catReq, &step)) < IMS_OK)
				{
					return (IMS_ERROR);
				}
				/*
				** successfully recorded the error to order_item table, return an
				** IMS_OK so the db changes will be committed
				*/
				else
				{
					return (IMS_OK);
				}
			} 
			/*
			** retryable error occurred, return IMS_ERROR to rollback db changes
			*/
			else
			{
				return (IMS_ERROR);
			}
		}

#ifdef DEBUG
		(void)ims_msg (msgDesc, IMS_INFO,
		      "ims_do_next_step: successfully inserted "
					"order_id %d item_id %d to photo queue.",
		        step.order_id,
		        step.item_id);
#endif

	}

	else if (strcmp (step.step_name, "COPY TO MEDIA") == 0)
	{
		/* 
		** ims_stepStatusChg handles table update for both Copy to Media and No Op 
		** steps 
		*/
		if ( (status = ims_stepStatusChg (msgDesc, &catReq, &step)) < IMS_OK )
		{
			/*
			** if non-retryable error occurred, update the status column in 
			** order_item to ERROR 
			*/
			if (status == IMS_ERROR)
			{
				if ( (record_error_status (msgDesc, &catReq, &step)) < IMS_OK)
				{
					return (IMS_ERROR);
				}
				/*
				** successfully recorded the error to order_item table, return an
				** IMS_OK so the db changes will be committed
				*/
				else
				{
					return (IMS_OK);
				}
			} 
			/*
			** retryable error occurred, return IMS_ERROR to rollback db changes
			*/
			else
			{
				return (IMS_ERROR);
			}
		}

#ifdef DEBUG
		(void)ims_msg (msgDesc, IMS_INFO,
		      "ims_do_next_step: successfully changed "
					"order_id %d item_id %d to IN-MEDIA-Q status.",
		        step.order_id,
		        step.item_id);
#endif

	}

	else if (strcmp (step.step_name, "STATUS CHANGE") == 0)
	{
		/* 
		** ims_stepStatusChg handles table update for both Copy to Media and No Op 
		** steps 
		*/
		if ( (status = ims_stepStatusChg (msgDesc, &catReq, &step)) < IMS_OK )
		{
			/*
			** if non-retryable error occurred, update the status column in 
			** order_item to ERROR 
			*/
			if (status == IMS_ERROR)
			{
				if ( (record_error_status (msgDesc, &catReq, &step)) < IMS_OK)
				{
					return (IMS_ERROR);
				}
				/*
				** successfully recorded the error to order_item table, return an
				** IMS_OK so the db changes will be committed
				*/
				else
				{
					return (IMS_OK);
				}
			} 
			/*
			** retryable error occurred, return IMS_ERROR to rollback db changes
			*/
			else
			{
				return (IMS_ERROR);
			}
		}

#ifdef DEBUG
		(void)ims_msg (msgDesc, IMS_INFO,
		      "ims_do_next_step: successfully changed "
					"order_id %d item_id %d to status %d .",
		        step.order_id,
		        step.item_id,
						step.end_status);
#endif

		if (step.order_item_type == DAR_TYPE)
		{
			ims_msg (msgDesc, IMS_INFO, 
				"DAR order %d is ready for the planner.", step.order_id);
		}
	}

	else
	{
		msgbuf[0] = '\0';
		step.op_comment[0] = '\0'; 

		(void) sprintf (step.op_comment, 
		    "ims_do_next_step: unexpected step_name %s. order_id %d "
			  "item_id %d. Software modification may be needed.",
					step.step_name,
		      step.order_id, 
		      step.item_id);

		(void) sprintf (msgbuf, "%s", step.op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

		if ( (record_error_status (msgDesc, &catReq, &step)) < IMS_OK)
		{
			return (IMS_ERROR);
		}
		else
		{
			return (IMS_OK);
		}

	}
	
	return (IMS_OK);

} /* end of ims_do_next_step */

/**********************************************************************
**
** ims_cancel_order -  cancel an existing order
**
** return IMS_OK       if successful
**        IMS_ERROR    if type 1 error occurred 
**        IMS_WARNING   if type 2 error occurred
** 
**********************************************************************/
static int ims_cancel_order (IMS_MSG_STRUCT *msgDesc,
                             V0_CAT_STRUCT *catReq,
                             STEP_INFO *step) 

{
	int            status, tstatus;
	char           msgbuf[IMS_COL1024_LEN];
	char           sql[IMS_COL1024_LEN];
	int             pps_ret_code;
	IMS_NUMERIC_DATE temp_date;

	/* initialization */
	pps_ret_code = 0;

	/*
	** a CANCEL REQUEST will be sent to PPS only if 
	** - the order item is in PPF PROCESS (i.e. step_name is PPF PROCESS and
	**   step_started_p is 'Y' 
	** -  process_status is neither marked as COMPLETED or CANCEL 
	**   (a PPS initiated cancellation causes both the process_status and 
	**    status be changed to CANCEL. process_status is changed to 
	**    COMPLETED when IMS is notified 
	*/

	if ( ( (strcmp (step->step_name, "PPF PROCESS") == 0) &&
			   (strcmp (step->step_started_p, "Y") == 0)) 
			 &&
			 ( (step->process_status == IMS_P_COMPLETED) || 
		     (step->process_status == IMS_P_CANCEL) ) )
	{
		/* this is not considered as an error, only log a message for record */
		ims_msg (msgDesc, IMS_WARNING, 
						"ims_step__ims_cancl_order: cannot cancel order id %d item %d"
						" at this time. It has been submitted to PPS and holds a"
						" processing status of %d",
						step->order_id, step->item_id, step->process_status);
		return (IMS_WARNING);

	}

	else if ( (strcmp (step->step_name, "PPF PROCESS") == 0) &&
			   (strcmp (step->step_started_p, "Y") == 0)) 
	{			
		/*
		** if DCE error exists, return IMS_WARING to retry this item the next time
		*/
		if (no_dce)
		{
			return (IMS_WARNING);
		}
		
		/*
		** get current time 
		*/
		if ( (status = ims_getCurrentDate(msgDesc, &temp_date)) < IMS_OK)
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
            "ims_step__ims_cancel_order: failed to convert current "
						"time  to IMS_NUMERIC_DATE for order_id %d item_id %d. "
						"Software modification may be needed.",
		           	step->order_id,
							 	step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

			return (IMS_ERROR);
		}
		(void) ims_numericDateToIMSA (&temp_date, step->curr_time);

		if ( (TxTree = NewAggregate(NULL, KA_OBJECT, "root", NULL)) == 
	                       (AGGREGATE)NULL )
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
			  "ims_step__ims_step_PPS: Failed to create ODL tree for orders to PPS, "
			  "order_id %d item_id %d",
							step->order_id,
							step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
	
			return (IMS_ERROR);
		}

		if ( (status = ims_step_buildCancelTree (msgDesc, step, TxTree)) < IMS_OK)
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
			  "ims_step__ims_cancel_order: Failed to build a Cancel Order ODL tree, "
			  "order_id %d item_id %d",
			      step->order_id,
			      step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);
			(void) ims_msg (msgDesc, status, msgbuf);

			return (status);
		}

		/* send request to PPS */
#ifdef DCEON
	pps_ret_code = ER_NO_ERROR;
#else
	pps_ret_code = 0;
#endif

		if ((tstatus=transmitPPSReq (msgDesc, step, &pps_ret_code)) < IMS_OK)
		{
			/* check pps_ret_code */
			if (tstatus == IMS_FATAL)
			{
  			if ( (status = check_pps_result (msgDesc, pps_ret_code, step)) < IMS_OK)
				{
					return (status);
				}
			}

			return (tstatus);
		}
		else 
		{
			(void) ims_msg (msgDesc, IMS_INFO, 
				 "Order cancellation for order_id %d item_id %d "
				 "submitted to PPS successfully",
				 step->order_id, step->item_id);
		}

	}

	/* 
	** Do the followings only when a CANCEL ORDER request has been sucessfully
	** sent to PPS, or if the situatioin does not require a CANCEL ORDER request
	** to be sent, e.g. cancellation of APRs, or DARs
	*/

	/*
	** Rollback the amount from account table if it's a PRODUCT REQUEST.
	** The debit action is skipped for SCAN and DAR for now
	*/
	if ( (step->order_item_type != TSR_TYPE) && 
			 (step->order_item_type != DAR_TYPE))
	{
		if ( (status = ims_acctTran (catReq->qDesc, msgDesc, step->account_id, 
 	             step->order_id, step->cost, DEBIT_ROLLBACK)) <IMS_OK)
		{
			msgbuf[0] = '\0';
			(void) sprintf (msgbuf, 
	        	"ims_step__ims_cancel_order: failed to rollback the balance "
						"for order_id %d item_id %d account_id %s",
	        	step->order_id,
	        	step->item_id,
						step->account_id);
			(void) ims_msg (msgDesc, status, msgbuf);

			/* change status to IMS_WARNING so it can be retried*/
			return (IMS_WARNING);
		}
	}

	/*
	** change status to CANCELLED on order_item table  
	*/
	sql[0] = '\0';
	(void) sprintf (sql,
	 	"update order_item set status = %d where order_id = %d and item_id = %d",
	 	IMS_CANCELLED_ITEM,
	 	step->order_id,
	 	step->item_id);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
	
	catReq->item[0] = (void *)sql;
	
	if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		msgbuf[0] = '\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (step->op_comment, 
	        	"ims_step__ims_cancel_order: failed to update order_item "
						"for order_id %d item_id %d.",
	        	step->order_id,
	        	step->item_id);
		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

		return (IMS_ERROR);
	}
	else
	{
		(void)ims_msg (msgDesc, IMS_INFO,
		    "ims_step__ims_cancel_order: Successfully cancelled order_id %d "
				" item_id %d",
				step->order_id, step->item_id);
	}

	return (IMS_OK);

} /* end of ims_cancel_order */

/**********************************************************************
**
** ims_sv_avail - notify STATE VECTOR availability 
**
** return IMS_OK       if successful
**        IMS_ERROR    if error occurred 
** 
**********************************************************************/
static int ims_sv_avail (IMS_MSG_STRUCT *msgDesc,
                             V0_CAT_STRUCT *catReq,
                             STEP_INFO *step) 

{
	int            status, tstatus;
	char           msgbuf[IMS_COL1024_LEN];
	char           sql[IMS_COL1024_LEN];
	int             pps_ret_code;
	IMS_NUMERIC_DATE temp_date;

	/* initialization */
	pps_ret_code = 0;

	/*
	** get current time 
	*/
	if ( (status = ims_getCurrentDate(msgDesc, &temp_date)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR, 
		  "ims_step__ims_sv_avail: failed to convert current time  to IMS_NUMERIC_DATE. Software modification may be needed.");
		return (IMS_ERROR);
	}
	(void) ims_numericDateToIMSA (&temp_date, step->curr_time);

	if ( (TxTree = NewAggregate(NULL, KA_OBJECT, "root", NULL)) == 
             (AGGREGATE)NULL )
	{
		msgbuf[0] = '\0';
		(void) sprintf (msgbuf, 
		 "Failed to create a tree for STATE VECTOR available message, "
		 "dataset_idx %d granule_idx %d",
		 step->dataset_idx, step->granule_idx);
		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf); 
		return (IMS_ERROR);
	}

	/* build tree */
	if ( (status = ims_step_buildSVTree (msgDesc, step, TxTree)) < IMS_OK)
	{
		msgbuf[0] = '\0';
		(void) sprintf (msgbuf, 
		 "Failed to build a STATE VECTOR available tree, dataset_idx %d "
		 "granule_idx %d",
		 step->dataset_idx, step->granule_idx);
		(void) ims_msg (msgDesc, status, msgbuf); 

		return (status);
	}

	/* send request to PPS */
#ifdef DCEON
	pps_ret_code = ER_NO_ERROR;
#else
	pps_ret_code = 0;
#endif

	if ((tstatus=transmitPPSReq (msgDesc, step, &pps_ret_code)) < IMS_OK)
	{
		if (tstatus == IMS_FATAL)
		{ 
			/* check pps_ret_code */
  		if ( (status = check_pps_result (msgDesc, pps_ret_code, step)) < IMS_OK)
			{
				return (status);
			}
		}

		return (tstatus);
	}
	else 
	{
		(void) ims_msg (msgDesc, IMS_INFO, 
				 "SVAvail order submitted to PPS successfully. "
				 "dataset_idx %d granule_idx %d",
 	     step->dataset_idx,
 	     step->granule_idx);
	}


	/* update table with completed status */
	sql[0] = '\0';
	(void) sprintf (sql,
	 	"update sv_available set status = %d where dataset_idx = %d "
		"and granule_idx = %d",
	 	IMS_SV_COMPLETE,
	 	step->dataset_idx,
	 	step->granule_idx);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
	
	catReq->item[0] = (void *)sql;
	
	if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		msgbuf[0] = '\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (step->op_comment, 
	        	"ims_step__ims_sv_avail: failed to update sv_available for "
						"dataset_idx %d granule_idx %d.",
	        	step->dataset_idx,
	        	step->granule_idx);
		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

		return (IMS_ERROR);
	}


	return (IMS_OK);
} /* end of ims_sv_avail */

/**********************************************************************
**
** get_item_info -  get step related information for the current line item
**
** return IMS_OK       if successful
**        IMS_ERROR    if type 1 error occurred 
**        IMS_WARNING   if type 2 error occurred 
**********************************************************************/
static int get_item_info (IMS_MSG_STRUCT  *msgDesc, 
                          V0_CAT_STRUCT *catReq, 
                          STEP_INFO *step)
{

	int            status;
	char           msgbuf[IMS_COL1024_LEN];
	char           sql[IMS_COL1024_LEN];
	ITEM_INFO_LIST *th, *tt;
	int            rowCount;

	sql[0] = '\0';
	(void) sprintf (sql, "exec disp_get_item_info %d, %d", 
              step->order_id, step->item_id);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if ((status = step_cat (catReq, STEP_GETITEMINFO)) < IMS_OK)
	{
		if ((rowCount == 0) || (rowCount >= 2))
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
		    "ims_step__get_item_info: found none or multiple item info for "
				"order_id %d item_id %d. ",
	          step->order_id,
	          step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR);
		}

		/* change status for other type of errors to IMS_WARNING */
		return (IMS_WARNING);
	}

	th = (ITEM_INFO_LIST *)catReq->item[2];	

	step->order_item_type     = th->order_item_type;
	step->media_class         = th->media_class;
	step->media_type          = th->media_type;
	step->process_type        = th->process_type;
	strcpy (step->step_name, th->step_name);
	step->step_sequence       = th->step_sequence;
	step->status              = th->status;
	strcpy (step->step_started_p, th->step_started_p);
	step->quantity            = th->quantity;
	step->priority            = th->priority;
	strcpy (step->quicklook_p, th->quicklook_p);
	step->cost                = th->cost;
	strcpy (step->account_id, th->account_id);
	step->process_status      = th->process_status;
	step->media_fmt_type      = th->media_fmt_type;
	strcpy (step->v0_process_type, th->v0_process_type);

	while (th != (ITEM_INFO_LIST *)NULL)
	{
		tt = th->next_p;
		free (th);
		th = tt;
	}

	return (IMS_OK);

} /* end of get_item_info */

/**********************************************************************
**
** get_next_step - get the information about what to do next
**
** return IMS_OK       if successful
**        IMS_ERROR    if type 1 error occurred 
**        IMS_WARNING   if type 2 error occurred
**********************************************************************/
static int get_next_step (IMS_MSG_STRUCT  *msgDesc, 
                          V0_CAT_STRUCT *catReq, 
                          STEP_INFO *step)
{
	int            status;
	char           msgbuf[IMS_COL1024_LEN];
	char           sql[IMS_COL1024_LEN];
	NEXT_STEP_INFO *dh, *dt;
	STEP_VALUE_LIST *vh, *vt;
	int            rowCount;

	sql[0] = '\0';
	(void)sprintf (sql, "exec get_next_step %d, %d, %d, %d", 
	                      step->order_item_type,
                        step->media_class, 
                        step->process_type, 
                        step->step_sequence);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if ((status = step_cat (catReq, STEP_GETNEXTSTEP)) < IMS_OK)
	{
		/* change to IMS_WARNING so it can be retried */
		return (IMS_WARNING);
	}

	dh = (NEXT_STEP_INFO *)catReq->item[2];	

	if ( (rowCount == 0)  || (rowCount >= 2) )
	{
		while ( dh != (NEXT_STEP_INFO *)NULL )
		{
			dt = dh->next_p;
			free (dh);
			dh = dt;
		}

		msgbuf[0] ='\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (step->op_comment, 
   	   "ims_step__get_next_step: found none or multiple next step "
			 "info for order_id %d, item_id %d. ",
 		        	step->order_id,
		        	step->item_id);

		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
		return (IMS_ERROR);
	}
			
	step->step_sequence       = dh->step_sequence;
	strcpy (step->step_name, dh->step_name);
	step->start_status        = dh->start_status;
	step->end_status          = dh->end_status;

	while ( dh != (NEXT_STEP_INFO *)NULL )
	{
		dt = dh->next_p;
		free (dh);
		dh = dt;
	}

	/* get order_type string */

	sql[0] = '\0';
	(void)sprintf (sql, 
      "select description from items where type = 'item_type' and "
			"instance = %d", step->order_item_type);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if ( (status = step_cat (catReq, STEP_GETSTR)) < IMS_OK )
	{

		if ( (rowCount == 0)  || (rowCount >= 2) )
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
        "ims_step__get_next_step: found none or multiple matching "
				"description for order_item_type %d from items table. "
				" order_id %d item_id %d",
          step->order_item_type, step->order_id, step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR); 
		}

		/* change status for other type of errors to IMS_WARNING */
		return (IMS_WARNING);
	}
	
	vh = (STEP_VALUE_LIST *)catReq->item[2];	

	strcpy (step->order_type, vh->char_value1);

	while ( vh != (STEP_VALUE_LIST *)NULL )
	{
		vt = vh->next_p;
		free (vh);
		vh = vt;
	}

	return (IMS_OK);

} /* end of get_next_step */	

/**********************************************************************
**
** ims_stepPPS - handles PPS step processing 
**
** return IMS_OK       if successful
**        IMS_ERROR    if type 1 error occurred 
**        IMS_WARNING   if type 2 error occurred
**********************************************************************/
static int ims_stepPPS       (IMS_MSG_STRUCT *msgDesc, 
                              V0_CAT_STRUCT  *catReq, 
                              STEP_INFO      *step)

{
	char            msgbuf[IMS_COL1024_LEN+1];
	char            sql[IMS_COL1024_LEN+1];
	int             status, tstatus;
	PPS_REQ_STRUCT  *pps_req;
	PPS_BASIC_INFO  *dh, *dt;
	STEP_VALUE_LIST *vh, *vt;
	int             pps_ret_code;
	int             temp_format; 
	int             rowCount;

	/*
	** initialization 
	*/
	pps_ret_code = 0;
	temp_format  = 0;

	/*
	** a quick varification. 
	*/
	if ( (step->order_item_type != RPR_TYPE) &&
			 (step->order_item_type != FPR_TYPE) &&
			 (step->order_item_type != COR_TYPE) &&
			 (step->order_item_type != TDR_TYPE) &&
			 (step->order_item_type != TSR_TYPE)) 
	{
		msgbuf[0] = '\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (step->op_comment, 
		      "ims_step__ims_stepPPS: unexpected order_item_type %d "
					"for order_id %d item_id %d",
		          step->order_item_type,
							step->order_id,
							step->item_id);

		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
		return (IMS_ERROR);
	}

	/*
	** initialization
	**
	** pps_req is a generic structure for all type of orders that will
	** be sent to PPS. Not all fields are required for just any type of order. 
	** Conditional check will be performed. 
	*/

	pps_req       = &(step->pps_req);

	(void) init_pps_req (step);

	/*
	** collect basic information for submitting ruquests to PPS 
	*/

	sql[0] = '\0';
	(void)sprintf (sql, "exec disp_get_pps_basic_info %d, %d", 
                        step->order_id, 
                        step->item_id);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if ( (status = step_cat (catReq, STEP_GETPPSBASIC)) < IMS_OK )
	{
		if ( (rowCount == 0)  || (rowCount >= 2) )
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
		    "ims_step__ims_step_PPS: found none or multiple basic "
				"PPS info for order_id %d, item_id %d. ",
 		      step->order_id,
		      step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR);
		}

		/* change status for other type of errors to IMS_WARNING */
		return (IMS_WARNING);
	}
	
	dh = (PPS_BASIC_INFO *)catReq->item[2];	

	pps_req->dataset_idx   =  dh->dataset_idx;
	pps_req->granule_idx   =  dh->granule_idx;
	temp_format  = dh->output_format;


	while ( dh != (PPS_BASIC_INFO *)NULL )
	{
		dt = dh->next_p;
		free (dh);
		dh = dt;
	}

	sql[0] = '\0';
	(void)sprintf (sql, 
	       "select granules_table, temporal_type from dataset_policy "
				 "where dataset_idx = %d", 
          pps_req->dataset_idx);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if ( (status = step_cat (catReq, STEP_GETGNULTBL)) < IMS_OK )
	{

		if ( (rowCount == 0)  || (rowCount >= 2) )
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
		      "ims_step__ims_step_PPS: found none or multiple granule "
					"table name for dataset_idx %d.  order_id %d item_id %d",
		         pps_req->dataset_idx, step->order_id, step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR); 
		}

		/* change status for other type of errors to IMS_WARNING */
		return (IMS_WARNING);
	}
	
	vh = (STEP_VALUE_LIST *)catReq->item[2];	

	strcpy (pps_req->granules_table, vh->char_value1);
	pps_req->temporal_type = vh->smallint_value;

	while ( vh != (STEP_VALUE_LIST *)NULL )
	{
		vt = vh->next_p;
		free (vh);
		vh = vt;
	}


	/* assign a matching ascii value to represent the priority */

	sql[0] = '\0';
	(void)sprintf (sql, 
      "select description from items where type = 'priority' and "
			"instance = %d", pps_req->priority);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if ( (status = step_cat (catReq, STEP_GETSTR)) < IMS_OK )
	{

		if ( (rowCount == 0)  || (rowCount >= 2) )
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
        "ims_step__ims_step_PPS: found none or multiple matching "
				"description for priority %d from items table. "
				" order_id %d item_id %d",
          pps_req->priority, step->order_id, step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR); 
		}

		/* change status for other type of errors to IMS_WARNING */
		return (IMS_WARNING);
	}
	
	vh = (STEP_VALUE_LIST *)catReq->item[2];	

	strcpy (pps_req->char_priority, vh->char_value1);

	while ( vh != (STEP_VALUE_LIST *)NULL )
	{
		vt = vh->next_p;
		free (vh);
		vh = vt;
	}

	/*
	** initialize a root ODL tree node
	*/

	if ( (TxTree = NewAggregate(NULL, KA_OBJECT, "root", NULL)) == 
	                       (AGGREGATE)NULL )
	{
		msgbuf[0] ='\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (step->op_comment, 
		  "ims_step__ims_step_PPS: Failed to create ODL tree for orders to PPS, "
		  "order_id %d item_id %d",
						step->order_id,
						step->item_id);

		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

		return (IMS_ERROR);
	}

	/*
	** 
	*/
	switch (step->order_item_type)
	{
		case (RPR_TYPE):
		case (FPR_TYPE):
		case (COR_TYPE):

			/* 
			** collect values for RPR/FPR/COR request specific attributes
			*/
			if ( (status = collectRFCVal (msgDesc, catReq, step)) < IMS_OK)
			{
				return (status);
			}

			/* assign a matching ascii value to represent the ourput format */

			sql[0] = '\0';
			(void)sprintf (sql, 
	       "select description from items where type = 'media_fmt_type' and "
				"instance = %d", temp_format);
			sql[strlen(sql)] = '\0';

			msgbuf[0] = '\0';

#ifdef QDEBUG
			(void) sprintf (msgbuf, "sql ==> %s", sql);
			(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

			catReq->item[0] = (void *)sql;

			rowCount = 0;
			catReq->item[1] = (int *)&rowCount;

			if ( (status = step_cat (catReq, STEP_GETSTR)) < IMS_OK )
			{

				if ( (rowCount == 0)  || (rowCount >= 2) )
				{
					msgbuf[0] ='\0';
					step->op_comment[0] = '\0'; 

					(void) sprintf (step->op_comment, 
		        "ims_step__ims_step_PPS: found none or multiple matching "
						"description for media_fmt_type %d in items table. "
						" order_id %d item_id %d",
		         temp_format, step->order_id, step->item_id);

					(void) sprintf (msgbuf, "%s", step->op_comment);

					(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
					return (IMS_ERROR); 
				}

				/* change status for other type of errors to IMS_WARNING */
				return (IMS_WARNING);
			}
	
			vh = (STEP_VALUE_LIST *)catReq->item[2];	

			strcpy (pps_req->output_format, vh->char_value1);

			while ( vh != (STEP_VALUE_LIST *)NULL )
			{
				vt = vh->next_p;
				free (vh);
				vh = vt;
			}

			/*
			** some values has to be expanded to fit into the PPS protocol
			*/
			if ( strcmp(pps_req->quicklook_p, "N") == 0  )
			{
				strcpy (pps_req->quicklook_p, "NO");
			}
			else if (strcmp(pps_req->quicklook_p, "Y") == 0)
			{
				strcpy (pps_req->quicklook_p, "YES");
			}

			if (strcmp(pps_req->deskew_p, "N") == 0)
			{
				strcpy (pps_req->deskew_p, "NO");
			}
			else if (strcmp(pps_req->deskew_p, "Y") == 0)
			{
				strcpy (pps_req->deskew_p, "YES");
			}

			if (strcmp(pps_req->terrain_correct_p, "N") == 0)
			{
				strcpy (pps_req->terrain_correct_p, "NO");
			}
			else if (strcmp(pps_req->terrain_correct_p, "Y") == 0)
			{
				strcpy (pps_req->terrain_correct_p, "YES");
			}

			if (strcmp(pps_req->compensation_p, "N") == 0)
			{
				strcpy (pps_req->compensation_p, "NO");
			}
			else if (strcmp(pps_req->compensation_p, "Y") == 0)
			{
				strcpy (pps_req->compensation_p, "YES");
			}

			/*
			** build the ODL tree for RPR/FPR/COR type request
			*/

			if ( (status = ims_step_buildRFCTree (msgDesc, step, TxTree)) < IMS_OK)
			{
				msgbuf[0] ='\0';
				step->op_comment[0] = '\0'; 

				(void) sprintf (step->op_comment, 
				  "ims_step__ims_step_PPS: Failed to build an L1 order ODL tree, "
				  "order_id %d item_id %d",
				      step->order_id,
				      step->item_id);

				(void) sprintf (msgbuf, "%s", step->op_comment);

				(void) ims_msg (msgDesc, status, msgbuf);

				return (status);
			}

			break;

		case (TDR_TYPE):

			break;

		case (TSR_TYPE):
			/* 
			** collect values for TSR specific attributes
			*/
			if ( (status = collectTSRVal (msgDesc, catReq, step)) < IMS_OK)
			{
				return (status);
			}

			if (strcmp(pps_req->quicklook_p, "N") == 0)
			{
				strcpy (pps_req->quicklook_p, "NO");
			}
			else if (strcmp(pps_req->quicklook_p, "Y") == 0)
			{
				strcpy (pps_req->quicklook_p, "YES");
			}
 
			if (strcmp(pps_req->compensation_p, "N") == 0)
			{
				strcpy (pps_req->compensation_p, "NO");
			}
			else if (strcmp(pps_req->compensation_p, "Y") == 0)
			{
				strcpy (pps_req->compensation_p, "YES");
			}
 
			/*
			** build the ODL tree for TSR type request
			*/
			if ( (status = ims_step_buildTSRTree (msgDesc, step, TxTree)) < IMS_OK)
			{
				msgbuf[0] ='\0';
				step->op_comment[0] = '\0'; 

				(void) sprintf (step->op_comment, 
				  "ims_step__ims_step_PPS: Failed to build an SCAN_ORDER ODL tree, "
					"order_id %d item_id %d",
				      step->order_id,
				      step->item_id);

				(void) sprintf (msgbuf, "%s", step->op_comment);

				(void) ims_msg (msgDesc, status, msgbuf);

				return (status);
			}

			break;


		case (DAR_TYPE):
			
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
			  "ims_step__ims_step_PPS: Unexpected DAR order entry %d item_id %d.",
			       step->order_id,
			       step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

			return (IMS_ERROR);
			
			break;

		case (APR_TYPE):
			
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
				"ims_step__ims_step_PPS: Unexpected APR order entry %d item_id %d.",
			       step->order_id,
			       step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

			return (IMS_ERROR);
			
			break;

		default:

			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
				"ims_step__ims_stepPPS: unrecognizable order type for "
				"order_id %d item_id %d. Software modification may be needed.",
			       step->order_id, 
						 step->item_id); 

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

			return (IMS_ERROR);

			break;

	}

	/*
	** update info on order_item before proceeding 
	*/
	/* commented out for R1Bprime to resolve the ims_disp/PPS RPC server
		 deadlock condition  - jlw
	if ( (status = update_before_submit (msgDesc, catReq, step)) < IMS_OK)
	{
		return (status);
	}
	*/


	/*
	** prepare the label and invoke PPS API and submit the request
	** if error occurs, return with IMS_ERROR
	*/
#ifdef DCEON
	pps_ret_code = ER_NO_ERROR;
#else
	pps_ret_code = 0;
#endif

	if ((tstatus=transmitPPSReq (msgDesc, step, &pps_ret_code)) < IMS_OK)
	{
		if (tstatus == IMS_FATAL)
		{
  		if ( (status = check_pps_result (msgDesc, pps_ret_code, step)) < IMS_OK)
			{
				return (status);
			}
		}

		return (tstatus);
	}
	else 
	{
		(void) ims_msg (msgDesc, IMS_INFO, 
			 "orderi_id %d item_id %d submitted to PPS successfully",
			 step->order_id, step->item_id);
	}

	/*
	** if submitted request to PPS sucessfully, update order_item with the new 
	** status
	*/
	/* commented out for R1Bprime to resolve the ims_disp/PPS RPC server
		 deadlock condition  -jlw
	if ( (status = update_after_submit (msgDesc, catReq, step)) < IMS_OK)
	{
		return (status);
	}
	*/
	/* use the following for R1Bprime to update order_item info after calling
		 PPS RPC server  - jlw*/
	if ( (status = ims_stepStatusChg (msgDesc, catReq, step)) < IMS_OK )
	{
		if (status == IMS_ERROR)
		{
			if ( (record_error_status (msgDesc, catReq, step)) < IMS_OK)
			{
				return (IMS_ERROR);
			}
			else
			{
				return (IMS_OK);
			}
		} 
		else
		{
			return (IMS_ERROR);
		}
	}

	return (IMS_OK);

} /* end of ims_stepPPS */

/**********************************************************************
**
** ims_stepFR - handles Fire Recorder step processing
**
** return IMS_OK       if successful
**        IMS_ERROR    if type 1 error occurred 
**        IMS_WARNING   if type 2 error occurred
**********************************************************************/
static int ims_stepFR        (IMS_MSG_STRUCT *msgDesc, 
                              V0_CAT_STRUCT  *catReq, 
                              STEP_INFO      *step)

{
	char msgbuf[IMS_COL1024_LEN+1];
	char sql[IMS_COL1024_LEN+1];
	int  status;
	
	/*
	** update info on order_item before proceeding 
	*/
	if ( (status = update_before_submit (msgDesc, catReq, step)) < IMS_OK)
	{
		return (status);
	}

	/*
	** insert a new entry into fire_queue table, the status is 1, i.e. NEW
	*/
	sql[0] = '\0';
	(void) sprintf (sql,
	        "insert fire_queue (order_id, item_id, status) values (%d, %d, 1)",
	          step->order_id,
	          step->item_id);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		step->op_comment[0] = '\0'; 
		msgbuf[0] = '\0';

		(void) sprintf (step->op_comment, 
		  "ims_step__ims_stepPR: failed to insert "
			"order_id %d item_id %d to fire queue.",
		     step->order_id,
		     step->item_id);

		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

		return (IMS_ERROR);
	}

	/*
	** update status info on order_item if succesfully inserted the entry to 
	** fire_queue
	*/
	if ( (status = update_after_submit (msgDesc, catReq, step)) < IMS_OK)
	{
		return (status);
	}

	return (IMS_OK);

} /* end of ims_stepFR */

/**********************************************************************
**
** ims_stepLT - handles Laser Tech step processing
**
** return IMS_OK       if successful
**        IMS_ERROR    if type 1 error occurred 
**        IMS_WARNING   if type 2 error occurred
**********************************************************************/
static int ims_stepLT        (IMS_MSG_STRUCT *msgDesc, 
                              V0_CAT_STRUCT  *catReq, 
                              STEP_INFO      *step)

{
	char msgbuf[IMS_COL1024_LEN+1];
	char sql[IMS_COL1024_LEN+1];
	int  status;

	/*
	** update info on order_item before proceeding 
	*/
	if ( (status = update_before_submit (msgDesc, catReq, step)) < IMS_OK)
	{
		return (status);
	}

	/*
	** insert a new entry into laser_queue table, the status should be NEW
	*/
	sql[0] = '\0';
	(void) sprintf (sql,
	        "insert laser_queue (order_id, item_id, status) values (%d, %d, 1)",
	          step->order_id,
	          step->item_id);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		step->op_comment[0] = '\0'; 
		msgbuf[0] = '\0';

		(void) sprintf (step->op_comment, 
		      "ims_step__ims_stepLT: failed to insert "
					"order_id %d item_id %d to laser queue.",
		        step->order_id,
		        step->item_id);
		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

		return (IMS_ERROR);
	}

	/*
	** update status info on order_item if succesfully inserted the entry to 
	** laser_queue
	*/
	if ( (status = update_after_submit (msgDesc, catReq, step)) < IMS_OK)
	{
		return (status);
	}

	return (IMS_OK);

} /* end of ims_stepLT */

/**********************************************************************
**
** ims_stepPL - handles Photo Lab step processing
**
** return IMS_OK       if successful
**        IMS_ERROR    if type 1 error occurred 
**        IMS_WARNING   if type 2 error occurred
**********************************************************************/
static int ims_stepPL        (IMS_MSG_STRUCT *msgDesc, 
                              V0_CAT_STRUCT  *catReq, 
                              STEP_INFO      *step)

{
	int  status;
	char msgbuf[IMS_COL1024_LEN+1];
	char sql[IMS_COL1024_LEN+1];
	int  photo_type;
	STEP_VALUE_LIST *vh, *vt;
	int  rowCount;


	/*
	** update info on order_item before proceeding 
	*/
	if ( (status = update_before_submit (msgDesc, catReq, step)) < IMS_OK)
	{
		return (status);
	}

	/*
	** find the matching photo_type
	*/
	sql[0] = '\0';
	(void) sprintf (sql,
	       "select photo_type from photo_type_map where "
				 "media_type = %d and media_fmt_type = %d",
	          step->media_type, step->media_fmt_type);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if ( (status = step_cat (catReq, STEP_GETPHOTOTYPE)) < IMS_OK)
	{

		if ((rowCount == 0) || (rowCount >= 2))
		{
			msgbuf[0] = '\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
		        "ims_step__ims_stepPL: found none or multiple "
						"photo_type info for media_type %d media_fmt_type %d "
						"from photo_type_map table. order_id %d item_id %d",
							step->media_type,
							step->media_fmt_type,
		        	step->order_id,
		        	step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

			return (IMS_ERROR);
		}

		/* change other errors to IMS_WARNING */
		return (IMS_WARNING);
	}

	vh = (STEP_VALUE_LIST *)catReq->item[2];
	photo_type = vh->smallint_value;

	while (vh != (STEP_VALUE_LIST *)NULL)
	{
		vt = vh->next_p;
		free (vh);
		vh = vt;
	}


	/*
	** insert a new entry into photo_queue table, the status is 1, i.e. NEW
	*/
	sql[0] = '\0';
	(void) sprintf (sql,
	       "insert photo_queue (order_id, item_id, photojob_id, "
				 "photo_type, quantity, status, user_comment, op_comment) "
			   "values (%d, %d, null, %d, %d, 1, null, null)",
	          step->order_id,
	          step->item_id,
	          photo_type,
	          step->quantity);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		msgbuf[0] = '\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (step->op_comment, 
		      "ims_step__ims_stepPL: failed to insert "
					"order_id %d item_id %d to photo queue.",
		        step->order_id,
		        step->item_id);
		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

		(void) sprintf (msgbuf, "%s", step->op_comment);

		return (IMS_ERROR);
	}

	/*
	** update status info on order_item if succesfully inserted the entry to 
	** photo_queue
	*/
	if ( (status = update_after_submit (msgDesc, catReq, step)) < IMS_OK)
	{
		return (status);
	}

	return (IMS_OK);

} /* end of ims_stepPL */


/**********************************************************************
**
** ims_stepStatusChg - changes status and records the step information 
**        on order_item table
**
** return IMS_OK       if successful
**        IMS_ERROR    if type 1 error occurred 
**        IMS_WARNING   if type 2 error occurred
**********************************************************************/
static int ims_stepStatusChg      (IMS_MSG_STRUCT *msgDesc, 
                              V0_CAT_STRUCT  *catReq, 
                              STEP_INFO      *step)

{
	int  status;
	char msgbuf[IMS_COL1024_LEN+1];
	char sql[IMS_COL1024_LEN+1];

	sql[0] = '\0';
	(void) sprintf (sql, 
	  "update order_item set step_name='%s', step_sequence=%d, "
		"step_started_p='Y', status = %d where order_id=%d and item_id=%d",
			 step->step_name,
			 step->step_sequence,
			 step->end_status,
			 step->order_id,
			 step->item_id);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		msgbuf[0] = '\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (step->op_comment, 
		      "ims_step__ims_stepStatusChg: failed to update order_item "
					"for order_id %d item_id %d.",
		        step->order_id,
            step->item_id);
		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

		return (IMS_ERROR);
	}


	return (IMS_OK);

} /* end of ims_stepStatusChg */

/**********************************************************************
**
** collectRFCVal - collect attribute values for RPR/FPR/COR orders
**
** return IMS_OK       if successful
**        IMS_ERROR    if type 1 error occurred 
**        IMS_WARNING   if type 2 error occurred
**********************************************************************/
static int collectRFCVal   (IMS_MSG_STRUCT *msgDesc, 
                         V0_CAT_STRUCT *catReq, 
                         STEP_INFO *step)
{
	
	int  status;
	char msgbuf[IMS_COL1024_LEN+1];
	char sql[IMS_COL1024_LEN+1];
	PPS_REQ_STRUCT   *pps_req;
	RFC_PROCESS_LIST *rh, *rt;
	RFC_GNUL_LIST    *gh, *gt;
	STEP_VALUE_LIST *vh, *vt;
	IMS_NUMERIC_DATE temp_date;
	int rowCount;
	char platform[IMS_COL30_LEN+1];
	char sensor[IMS_COL30_LEN+1];
	char dataset[IMS_COL80_LEN+1];
	char scan_results_file[IMS_COL128_LEN+1];
	char scan_results_file_table[IMS_COL30_LEN+1];
	char raw_signal_table[IMS_COL30_LEN+1];
	
	pps_req = &(step->pps_req);

	platform[0] = '\0';
	sensor[0] = '\0';
	dataset[0] = '\0';
	scan_results_file[0] = '\0';
	scan_results_file_table[0] = '\0';
	raw_signal_table[0] = '\0';

	sql[0] = '\0';
	(void) sprintf (sql, 
	   "select platform, sensor, dataset from order_item "
		 "where order_id = %d and item_id = %d", 
			 step->order_id,
			 step->item_id);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if ( (status = step_cat (catReq, STEP_GETSTR3)) < IMS_OK )
	{
		
		if ( (rowCount == 0)  || (rowCount >= 2) )
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (msgbuf, 
           "ims_step__collectRFCVal: found none or multiple platform, "
					 "sensor, and dataset combination from order_item table "
					 "for order_id %d item_id %d",
		           	step->order_id,
								step->item_id);

			(void) sprintf (step->op_comment, "%s", msgbuf);
              	
			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR);
		}

		/* change status for other type of errors to IMS_WARNING */
		return (IMS_WARNING); 
	} 
	
	vh = (STEP_VALUE_LIST *)catReq->item[2];	
	strcpy (platform, vh->char_value1);
	strcpy (sensor, vh->char_value2);
	strcpy (dataset, vh->char_value3);

	while (vh != (STEP_VALUE_LIST *)NULL)
	{
		vt = vh->next_p;
		free (vh);
		vh = vt;
	}

	/*
	** collect information which can be found on process_type_map table
	** based on process_type
	*/

	sql[0] = '\0';
	if (sensor[0] == '\0')
	{
		(void) sprintf (sql, 
						"exec disp_get_RFC_process_info '%s', NULL, '%s', '%s', %d", 
	            platform,
							dataset,
							pps_req->v0_process_type,
              pps_req->process_type);
	}
	else
	{
		(void) sprintf (sql, 
						"exec disp_get_RFC_process_info '%s', '%s', '%s', '%s', %d", 
	            platform,
							sensor,
							dataset,
							pps_req->v0_process_type,
              pps_req->process_type);
	}
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if ( (status = step_cat (catReq, STEP_GETRFCPROC)) < IMS_OK )
	{
		
		if ( (rowCount == 0)  || (rowCount >= 2) )
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
             	"ims_step__collectRFCVal: found none or multiple "
							"RPR/FPR/COR processing info from process_type_map table "
							"for order_id %d item_id %d",
								step->order_id, step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR);
		}

		/* change status for other type of errors to IMS_WARNING */
		return (IMS_WARNING); 
	} 
	
	rh = (RFC_PROCESS_LIST *)catReq->item[2];	
	
	strcpy (pps_req->product_type, rh->product_type);
	pps_req->pixel_spacing       =   rh->pixel_spacing;
	strcpy (pps_req->projection, rh->projection);
	pps_req->processing_gain     =   rh->processing_gain;
	pps_req->avg_terrain_ht      =   rh->avg_terrain_ht;
	strcpy (pps_req->deskew_p, rh->deskew_p);
	pps_req->ps_reference_lat    =   rh->ps_reference_lat;
	pps_req->ps_reference_lon    =   rh->ps_reference_lon;
	pps_req->utm_zone            =   rh->utm_zone;
	strcpy (pps_req->terrain_correct_p, rh->terrain_correct_p);
	pps_req->lambert_lat_n       =   rh->lambert_lat_n;
	pps_req->lambert_lat_s       =   rh->lambert_lat_s;
	pps_req->subframe_id         =   rh->subframe_id;
	strcpy (pps_req->compensation_p, rh->compensation_p);

	while (rh != (RFC_PROCESS_LIST *)NULL)
	{
		rt = rh->next_p;
		free (rh);
		rh = rt;
	}

	/*
	** collect information from frame table 
	*/

	sql[0] = '\0';

	(void) sprintf (sql, 
		"select PLATFORM, SENSOR, REVOLUTION, MODE, SEQUENCE, "
		"FRAME_ID, FRAME_MODE, MEDIA_ID, STATION_ID, "
		"START_TIME, END_TIME, CENTER_TIME, ACTIVITY_ID, "
		"SCAN_RESULTS_FILE from %s where  granule_idx = %d",
	         pps_req->granules_table,
					 pps_req->granule_idx);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if ( (status = step_cat (catReq, STEP_GETRFCGNUL)) < IMS_OK )
	{
		
		if ( (rowCount == 0)  || (rowCount >= 2) )
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
             "ims_step__collectRFCVal: found none or multiple "
						 "RPR/FPR/COR granule info for granule_idx %d platform %s from "
						 "granules_table %s,  order_id %d item_id %d ",
		           	pps_req->granule_idx,
								platform,
	              pps_req->granules_table,
								step->order_id,
								step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR);
		}

		/* change status for other type of errors to IMS_WARNING */
		return (IMS_WARNING);
	}

	gh = (RFC_GNUL_LIST *)catReq->item[2];	

	strcpy (pps_req->platform, gh->platform);
	strcpy (pps_req->sensor, gh->sensor);
	pps_req->rev        =  gh->rev;
	strcpy (pps_req->mode, gh->mode);
	pps_req->sequence   =  gh->sequence;
	pps_req->frame_id   =  gh->frame_id;
	strcpy (pps_req->frame_mode, gh->frame_mode);
	strcpy (pps_req->media_id, gh->media_id);
	strcpy (pps_req->station_id, gh->station_id);
	strcpy (pps_req->start_time, gh->start_time);
	strcpy (pps_req->end_time, gh->end_time);
	strcpy (pps_req->center_time, gh->center_time);
	strcpy (pps_req->activity_id, gh->activity_id);
	strcpy (scan_results_file, gh->scan_results_file);

	while (gh != (RFC_GNUL_LIST *)NULL)
	{
		gt = gh->next_p;
		free (gh);
		gh = gt;
	}

	/*
	** media_id and scan_results_file have to be either both exist or both
	** not exist.  Otherwise, the database has an incosistancy problem
	*/
	if (((pps_req->media_id[0]=='\0') && (scan_results_file[0]!='\0'))||
	    ((pps_req->media_id[0]!='\0') && (scan_results_file[0]=='\0')))
	{
		msgbuf[0] ='\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (step->op_comment, 
          "ims_step__collectRFCVal: missing one value of either media_id  "
		      "or scan_results_file for granule_idx %d in "
		      "granules_table %s,  order_id %d item_id %d ",
		         	pps_req->granule_idx,
	            pps_req->granules_table,
							step->order_id, 
							step->item_id);

		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
		return (IMS_ERROR);
	}

	/*
	** collect information to put in SOURCE_MEDIA 
	*/
	if ( (pps_req->media_id[0]!='\0') && (scan_results_file[0]!='\0'))
	{
		/*
		** convert start_time, end_time from SYBASE datetime to IMS_NUMERIC_DATE
		** format. 
		*/
		if ( (status = ims_timeToNumericDate 
							 	(msgDesc, pps_req->start_time, &temp_date)) < IMS_OK)
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 
	
			(void) sprintf (step->op_comment, 
      	"ims_step__collectRFCVal: failed to convert start time " 
				"to IMS_NUMERIC_DATE for order_id %d item_id %d. ",
		           	step->order_id,
							 	step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);
	
			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR);
		}
		(void) ims_numericDateToIMSA (&temp_date, pps_req->start_time);
	
		if ( (status = ims_timeToNumericDate 
							 	(msgDesc, pps_req->end_time, &temp_date)) < IMS_OK)
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 
	
			(void) sprintf (step->op_comment, 
      	"ims_step__collectRFCVal: failed to convert end_time time " 
				"to IMS_NUMERIC_DATE for order_id %d item_id %d. ",
		           	step->order_id,
							 	step->item_id);
	
			(void) sprintf (msgbuf, "%s", step->op_comment);
	
			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR);
		}
		(void) ims_numericDateToIMSA (&temp_date, pps_req->end_time);
	
		if ( (status = ims_timeToNumericDate 
							 	(msgDesc, pps_req->center_time, &temp_date)) < IMS_OK)
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 
	
			(void) sprintf (step->op_comment, 
      	"ims_step__collectRFCVal: failed to convert center time " 
				"to IMS_NUMERIC_DATE for order_id %d item_id %d. ",
		           	step->order_id,
							 	step->item_id);
	
			(void) sprintf (msgbuf, "%s", step->op_comment);
	
			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR);
		}
		(void) ims_numericDateToIMSA (&temp_date, pps_req->center_time);


		/*
		** find name of the scan results file table based on the platform if 
		** both media_id and scan_results_file fields are non-null from the 
		** previous query
		*/
		sql[0] = '\0';
			(void)sprintf (sql, 
      	"select dp.granules_table from dataset_policy dp, dataset_relation dr "
				"where dr.dataset = '%s SCAN RESULTS FILE' and "
				"dp.dataset_idx = dr.dataset_idx", platform);
		sql[strlen(sql)] = '\0';
	
		msgbuf[0] = '\0';

#ifdef QDEBUG
		(void) sprintf (msgbuf, "sql ==> %s", sql);
		(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

		catReq->item[0] = (void *)sql;

		rowCount = 0;
		catReq->item[1] = (int *)&rowCount;

		if ( (status = step_cat (catReq, STEP_GETSTR)) < IMS_OK )
		{

			if ( (rowCount == 0)  || (rowCount >= 2) )
			{
				msgbuf[0] ='\0';
				step->op_comment[0] = '\0'; 

				(void) sprintf (step->op_comment, 
        	"ims_step__collectRFCVal: found none or multiple matching "
					"scan results file table name for %s from "
					"dataset_relation and dataset policy table. "
					" order_id %d item_id %d", 
					platform, step->order_id, step->item_id);

				(void) sprintf (msgbuf, "%s", step->op_comment);

				(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
				return (IMS_ERROR); 
			}

			/* change status for other type of errors to IMS_WARNING */
			return (IMS_WARNING);
		}
	
		vh = (STEP_VALUE_LIST *)catReq->item[2];	

		strcpy (scan_results_file_table, vh->char_value1);

		while ( vh != (STEP_VALUE_LIST *)NULL )
		{
			vt = vh->next_p;
			free (vh);
			vh = vt;
		}

		/*
		** retrieve the rest of the RFC information from the granule table
		** with scan results 
		*/

		sql[0] = '\0';

		(void) sprintf (sql, 
			"select MEDIA_TYPE, RECORDER_ID, SITE_NAME,"
			" MEDIA_LOCATION"
			" from %s where MEDIA_ID = '%s' and REVOLUTION = %d and"
			" SEQUENCE = %d and PLATFORM = '%s' and SCAN_RESULTS_FILE = '%s'", 
					 scan_results_file_table,
					 pps_req->media_id, 
					 pps_req->rev,
					 pps_req->sequence,
					 pps_req->platform,
					 scan_results_file);
		sql[strlen(sql)] = '\0';

		msgbuf[0] = '\0';

#ifdef QDEBUG
		(void) sprintf (msgbuf, "sql ==> %s", sql);
		(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

		catReq->item[0] = (void *)sql;

		rowCount = 0;
		catReq->item[1] = (int *)&rowCount;

		if ( (status = step_cat (catReq, STEP_GETSTR4)) < IMS_OK )
		{
		
			if ( (rowCount == 0)  || (rowCount >= 2) )
			{
				msgbuf[0] ='\0';
				step->op_comment[0] = '\0'; 

				(void) sprintf (step->op_comment, 
             "ims_step__collectRFCVal: found none or multiple "
						 "entries to match MEDIA_ID %s, "
						 "REVOLUTION %d, SEQUENCE %d, and PLATFORM %s  from "
						 "scan results table %s"
						 " order_id %d item_id %d",
								pps_req->media_id,
								pps_req->rev,
								pps_req->sequence,
								pps_req->platform,
				        scan_results_file_table,
								step->order_id,
								step->item_id);

				(void) sprintf (msgbuf, "%s", step->op_comment);

				(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
				return (IMS_ERROR);
			}

			/* change status for other type of errors to IMS_WARNING */
			return (IMS_WARNING);
		}

		vh = (STEP_VALUE_LIST *)catReq->item[2];	

		strcpy (pps_req->media_type, vh->char_value1);
		strcpy (pps_req->recorder_id, vh->char_value2);
		strcpy (pps_req->site_name, vh->char_value3);
		strcpy (pps_req->media_location, vh->char_value4);

		while ( vh != (STEP_VALUE_LIST *)NULL )
		{
			vt = vh->next_p;
			free (vh);
			vh = vt;
		}


		/*
		** find actual table name of the <platform> RAW SIGNAL SEGMENT table
		*/
		sprintf (raw_signal_table, "%s RAW SIGNAL SEGMENT", platform);
		sql[0] = '\0';

		(void) sprintf (sql, 
			"select dp.granules_table from dataset_policy dp,"
			" dataset_relation dr where dr.dataset = '%s' and"
			" dr.dataset_idx = dp.dataset_idx",
					 raw_signal_table);
		sql[strlen(sql)] = '\0';
		msgbuf[0] = '\0';

#ifdef QDEBUG
		(void) sprintf (msgbuf, "sql ==> %s", sql);
		(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

		catReq->item[0] = (void *)sql;

		rowCount = 0;
		catReq->item[1] = (int *)&rowCount;

		if ( (status = step_cat (catReq, STEP_GETSTR)) < IMS_OK )
		{
		
			if ( (rowCount == 0)  || (rowCount >= 2) )
			{
				msgbuf[0] ='\0';
				step->op_comment[0] = '\0'; 

				(void) sprintf (step->op_comment, 
             "ims_step__collectRFCVal: found none or multiple "
						 "entries in dataset_relation table with dataset name %s "
						 "order_id %d item_id %d",
				        raw_signal_table,
								step->order_id,
								step->item_id);

				(void) sprintf (msgbuf, "%s", step->op_comment);

				(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
				return (IMS_ERROR);
			}

			/* change status for other type of errors to IMS_WARNING */
			return (IMS_WARNING);
		}

		vh = (STEP_VALUE_LIST *)catReq->item[2];	

		raw_signal_table[0] = '\0';
		strcpy (raw_signal_table, vh->char_value1);

		while ( vh != (STEP_VALUE_LIST *)NULL )
		{
			vt = vh->next_p;
			free (vh);
			vh = vt;
		}

		/*
		** find DATA DIRECTION value
		*/
		sql[0] = '\0';

		(void) sprintf (sql, 
			"select g.DATA_DIRECTION from %s g, datatake_entry m "
			"where MEDIA_ID = '%s' and "
			"((g.PLATFORM = m.PLATFORM and g.REVOLUTION = m.REVOLUTION and"
			"  g.SEQUENCE = m.SEQUENCE) or"
			" (g.PLATFORM = m.DT_PLATFORM and g.REVOLUTION = m.DT_REVOLUTION and" 
			"  g.SEQUENCE = m.DT_SEQUENCE)) and "
			"((m.PLATFORM = '%s' and m.REVOLUTION = %d and m.SEQUENCE = %d) or"
			" (m.DT_PLATFORM = '%s' and m.DT_REVOLUTION = %d and "
			"  m.DT_SEQUENCE = %d))",
			raw_signal_table,
			pps_req->media_id, 
			pps_req->platform, pps_req->rev, pps_req->sequence,
			pps_req->platform, pps_req->rev, pps_req->sequence);

		sql[strlen(sql)] = '\0';
		msgbuf[0] = '\0';

#ifdef QDEBUG
		(void) sprintf (msgbuf, "sql ==> %s", sql);
		(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

		catReq->item[0] = (void *)sql;

		rowCount = 0;
		catReq->item[1] = (int *)&rowCount;

		if ( (status = step_cat (catReq, STEP_GETSTR)) < IMS_OK )
		{
		
			if ( (rowCount == 0)  || (rowCount >= 2) )
			{
				msgbuf[0] ='\0';
				step->op_comment[0] = '\0'; 

				(void) sprintf (step->op_comment, 
             "ims_step__collectRFCVal: found none or multiple "
						 "DATA_DIRECTION value match with MEDIA_ID %s, "
						 "REVOLUTION %d, SEQUENCE %d, and PLATFORM %s  from "
						 "RAW SIGNAL SEGMENT table %s order_id %d item_id %d",
								pps_req->media_id,
								pps_req->rev,
								pps_req->sequence,
								pps_req->platform,
				        raw_signal_table,
								step->order_id,
								step->item_id);

				(void) sprintf (msgbuf, "%s", step->op_comment);

				(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
				return (IMS_ERROR);
			}

			/* change status for other type of errors to IMS_WARNING */
			return (IMS_WARNING);
		}

		vh = (STEP_VALUE_LIST *)catReq->item[2];	

		strcpy (pps_req->data_direction, vh->char_value1);

		while ( vh != (STEP_VALUE_LIST *)NULL )
		{
			vt = vh->next_p;
			free (vh);
			vh = vt;
		}

	} /* SOURCE_MEDIA information collection when both media_id and 
			 scan_results_file are not null */

	/*
	** get current time from the system and store it in IMS_NUMERIC_DATE fmt
	** may need to examine software if failed
	*/
	if ( (status = ims_getCurrentDate(msgDesc, &temp_date)) < IMS_OK)
	{
		msgbuf[0] ='\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (step->op_comment, 
      "ims_step__collectRFCVal: failed to convert current time " 
			"to IMS_NUMERIC_DATE for order_id %d item_id %d. ",
		           step->order_id,
							 step->item_id);

		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
		return (IMS_ERROR);
	}
	(void) ims_numericDateToIMSA (&temp_date, step->curr_time);

	return (IMS_OK);

} /* end of collectRFCVal */

/**********************************************************************
**
** collectTSRVal - collect attribute values for TSR orders
**
** return IMS_OK       if successful
**        IMS_ERROR    if type 1 error occurred 
**        IMS_WARNING   if type 2 error occurred
**********************************************************************/
static int collectTSRVal   (IMS_MSG_STRUCT *msgDesc, 
                         V0_CAT_STRUCT *catReq, 
                         STEP_INFO *step)
{
	
	int  status;
	char msgbuf[IMS_COL1024_LEN+1];
	char sql[IMS_COL1024_LEN+1];
	PPS_REQ_STRUCT   *pps_req;
	TSR_INFO_LIST    *gh, *gt;
	IMS_NUMERIC_DATE temp_date;
	int  rowCount;
	
	pps_req = &(step->pps_req); 
	
	sql[0] = '\0';
	msgbuf[0] = '\0';

        /*
        ** collect information from granule tables named
        **    "<platform> RAW SIGNAL SEGMENT", "scan" table and
        **    and "downlink_entry" table
        */
 
	(void) sprintf (sql, 
		"select s.DT_PLATFORM, s.DT_SENSOR, s.DT_REVOLUTION,"
		"s.MODE, s.DT_SEQUENCE, s.ACTIVITY_ID, s.FRAME_MODE,"
		"g.RECORDER_TYPE, g.MEDIA_ID, g.START_ADDRESS, g.STOP_ADDRESS, "
		"g.RECORDER_ID, s.STATION_ID, s.SITE_NAME, s.TIME_ON,"
		"s.TIME_OFF, 'SHELF', g.DATA_DIRECTION "
		"from scan s, %s g "
		"where g.granule_idx = %d and s.order_id = %d and s.item_id = %d",
	        pps_req->granules_table,
		pps_req->granule_idx, step->order_id, step->item_id);

	sql[strlen(sql)] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	if ( (status = step_cat (catReq, STEP_GETTSRGNUL)) < IMS_OK )
	{
		
		if ( (rowCount == 0)  || (rowCount >= 2) )
		{
			msgbuf[0] ='\0';
			step->op_comment[0] = '\0'; 

			(void) sprintf (step->op_comment, 
		             "ims_step__collectTSRVal: found none or multiple "
				"granule info for granule_idx %d from "
				"granules_table %s for SCAN_ORDER order_id %d item_id %d",
		           	pps_req->granule_idx,
				pps_req->granules_table,
				step->order_id, step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR);
		}

		/* change status for other type of errors to IMS_WARNING */
		return (IMS_WARNING);
	}

	gh = (TSR_INFO_LIST *)catReq->item[2];	

	strcpy (pps_req->platform, gh->platform);
	strcpy (pps_req->sensor, gh->sensor);
	pps_req->rev        =  gh->rev;
	strcpy (pps_req->mode, gh->mode);
	pps_req->sequence   =  gh->sequence;
	strcpy (pps_req->activity_id, gh->activity_id);

#ifdef I_DONT_THINK_WE_NEED_THIS_ANYMORE /* Thuy */
	/*
	** Kludge to recognize AMM test data.
	** S. Hardman - 3/10/97
	*/
	if ((gh->sequence >= 50) && (gh->sequence <= 60))
	{
		strcpy (pps_req->frame_mode, "ANTARCTIC");
	}
	else
	{
		strcpy (pps_req->frame_mode, gh->frame_mode);
	}
#endif
	strcpy (pps_req->frame_mode, gh->frame_mode);
	strcpy (pps_req->media_type, gh->media_type);
	strcpy (pps_req->media_id, gh->media_id);
	pps_req->start_address =  gh->start_address;
	pps_req->end_address   =  gh->end_address;
	strcpy (pps_req->recorder_id, gh->recorder_id);
	strcpy (pps_req->station_id, gh->station_id);
	strcpy (pps_req->site_name, gh->site_name);
	strcpy (pps_req->start_time, gh->start_time);
	strcpy (pps_req->end_time, gh->end_time);
	strcpy (pps_req->media_location, gh->media_location);
	strcpy (pps_req->data_direction, gh->data_direction);

	while (gh != (TSR_INFO_LIST *)NULL)
	{
		gt = gh->next_p;
		free (gh);
		gh = gt;
	}

	/*
	** convert start_time, end_time from SYBASE datetime to IMS_NUMERIC_DATE
	** format
	*/
	if ( (status = ims_timeToNumericDate 
			(msgDesc, pps_req->start_time, &temp_date)) < IMS_OK)
	{
		msgbuf[0] ='\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (step->op_comment, 
      "ims_step__collectTSRVal: failed to convert start time " 
			"to IMS_NUMERIC_DATE for order_id %d item_id %d. ",
		           step->order_id, step->item_id);

		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
		return (IMS_ERROR);
	}
	(void) ims_numericDateToIMSA (&temp_date, pps_req->start_time);

	if ( (status = ims_timeToNumericDate 
		(msgDesc, pps_req->end_time, &temp_date)) < IMS_OK)
	{
		msgbuf[0] ='\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (step->op_comment, 
      "ims_step__collectTSRVal: failed to convert end time " 
			"to IMS_NUMERIC_DATE for order_id %d item_id %d. ",
		           step->order_id, step->item_id);

		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
		return (IMS_ERROR);
	}
	(void) ims_numericDateToIMSA (&temp_date, pps_req->end_time);

	/*
	** get current time from the system and store it in IMS_NUMERIC_DATE fmt
	*/
	if ( (status = ims_getCurrentDate(msgDesc, &temp_date)) < IMS_OK)
	{
		msgbuf[0] ='\0';
		(void) sprintf (msgbuf, 
            "ims_step__collectTSRVal: failed to convert current time " 
		"to IMS_NUMERIC_DATE for order_id %d item_id %d",
		step->order_id, step->item_id);
		(void) ims_msg (msgDesc, status, msgbuf);

		return (status);
	}
	(void) ims_numericDateToIMSA (&temp_date, step->curr_time);


	return (IMS_OK);

} /* end of collectTSRVal */

/**********************************************************************
**
** transmitPPSReq - prepare PPS bound requests.  Write ODL to a file, and
**    call PPS API.
**
** return IMS_OK       if successful
**        IMS_ERROR    if type 1 error occurred 
**        IMS_WARNING   if type 2 error occurred
**********************************************************************/
static int transmitPPSReq (IMS_MSG_STRUCT *msgDesc, 
													 STEP_INFO      *step, 
													 int            *pps_ret_code)
{
	FILE *fp, *tp;
	char filename[IMS_COL255_LEN+1];
	char msgbuf[IMS_COL255_LEN+1]; 
	long  nread = 0;
	unsigned char   buffer[BUF_SIZE];
	char *fn;
	int  status;
	int  dce_err;
	int  pps_err;

#ifdef DCEON
	unsigned char *dce_msgbuf; 
	error_status_t  dce_status;       
#endif

	dce_err = 0;
	pps_err = 0;

	/*
	** get a name for our temporary file.  The file will be created in the
	** same directory where the log files reside
	*/

	(void) ims_concatFilePath (filename, glbDirectory, "temp.lbl");

	if ( ( fp = fopen(filename, "w")) == NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR, 
		      "ims_step__transmitPPSReq: Failed to open temporary file to write.");
	
		/* change status to IMS_WARNING so it can be retried */ 
		return (IMS_ERROR);
	}

	(void) WriteLabel (fp,TxTree);
	(void) fflush (fp);

#ifdef ODEBUG
			(void) PrintLabel (TxTree);
#endif
	
#ifdef DCEON
	fn = (char *)NULL;
	fn = filename;
	msgbuf[0] = '\0';

	if (step->status == IMS_CANCEL_ITEM)
	{
		status = client_send (fn, IMS_CANCEL , msgbuf, pps_ret_code, 0);

		/* IMS_ERROR is an indication of DCE error */
		if (status == IMS_ERROR)
		{
			(void)ims_msg (msgDesc, IMS_ERROR, "transmitPPSReq: DCE error => %s",
			     msgbuf);
			dce_err = 1;
		}
		else if (status == IMS_FATAL)
		{
			pps_err = 1;	
		}
	}

	else if (step->order_id == -1)
	{
		status = client_send (fn, IMS_SV_AVAIL , msgbuf, pps_ret_code, 0);

		/* IMS_ERROR is an indication of DCE error */
		if (status == IMS_ERROR)
		{
			(void)ims_msg (msgDesc, IMS_ERROR, "transmitPPSReq: DCE error => %s",
			     msgbuf);
			dce_err = 1;
		}
		else if (status == IMS_FATAL)
		{
			pps_err = 1;	
		}
	}

	else
	{
		switch (step->order_item_type)
		{
			case RPR_TYPE:
			case FPR_TYPE:
	 		case COR_TYPE:
				status = client_send (fn, IMS_L1PR , msgbuf, pps_ret_code, 0);

				/* IMS_ERROR is an indication of DCE error */
				if (status == IMS_ERROR)
				{
					(void)ims_msg (msgDesc, IMS_ERROR, "transmitPPSReq: DCE error => %s",
			       msgbuf);
					dce_err = 1;
				}
				else if (status == IMS_FATAL)
				{
					pps_err = 1;	
				}

				break;
	
			/* tape dub is not handled for now */	
			case TDR_TYPE:

				/* IMS_ERROR is an indication of DCE error */
				if (status == IMS_ERROR)
				{
					(void)ims_msg (msgDesc, IMS_ERROR, "transmitPPSReq: DCE error => %s",
			       msgbuf);
					dce_err = 1;
				}
				else if (status == IMS_FATAL)
				{
					pps_err = 1;	
				}

				break;
	
			case TSR_TYPE: 
				status = client_send (fn, IMS_SCAN , msgbuf, pps_ret_code, 0);

				/* IMS_ERROR is an indication of DCE error */
				if (status == IMS_ERROR)
				{
					(void)ims_msg (msgDesc, IMS_ERROR, "transmitPPSReq: DCE error => %s",
			       msgbuf);
					dce_err = 1;
				}
				else if (status == IMS_FATAL)
				{
					pps_err = 1;	
				}

				break;

			default:
				msgbuf[0] = '\0';
				step->op_comment[0] = '\0'; 

				(void)sprintf (step->op_comment, 
			   "ims_step__transmitPPSReq: unexpected order type, " 
				 "order_id %d item id %d. Software modification may be needed." ,
			    	step->order_id,
						step->item_id);

				(void) sprintf (msgbuf, "%s", step->op_comment);

				(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
	
				return (IMS_ERROR);
				break;
		}
	}

#endif

	fclose (fp);
	unlink (filename);

	/* DCE error is considered retriable */
	if (dce_err)
	{
		step->dce_err_flag = 1;
		return (IMS_WARNING);
	}

	if (pps_err)
		return (IMS_FATAL);

	return (IMS_OK);

}/* transmitPPSReq */

/**********************************************************************
**
** update_before_submit - update order_item table with correct step info
**                        before an order item proceeds to a new step
**
** return IMS_OK       if successful
**        IMS_ERROR    if type 1 error occurred 
**        IMS_WARNING   if type 2 error occurred
**********************************************************************/
static int update_before_submit (IMS_MSG_STRUCT *msgDesc,
                                 V0_CAT_STRUCT *catReq,
                                 STEP_INFO *step) 

{

	char msgbuf[IMS_COL1024_LEN+2];
	char sql[IMS_COL1024_LEN+2];
	int  status;

	/*
	** update order_item with new values of step_name, step_sequence and
	**      step_started_p
	*/
	sql[0] = '\0';
	(void) sprintf (sql, 
	  "update order_item set step_name='%s', step_sequence=%d, "
		"step_started_p='N' where order_id=%d and item_id=%d",
			 step->step_name,
			 step->step_sequence,
			 step->order_id,
			 step->item_id);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	if ( ( status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		msgbuf[0] = '\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (step->op_comment, 
		    "ims_step__update_before_submit: failed to update "
				"order_item for order_id %d item_id %d.",
		        step->order_id,
            step->item_id);
		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

		return (IMS_ERROR);
	}

	return (IMS_OK);

} /* end of update_before_submit */



/**********************************************************************
**
** update_after_submit - update the order_item table with new status info
**                     after an order item has been submitted to a new
**                     processing step
**
** return IMS_OK       if successful
**        IMS_ERROR    if type 1 error occurred 
**        IMS_WARNING   if type 2 error occurred
**********************************************************************/
static int update_after_submit (IMS_MSG_STRUCT *msgDesc,
                              V0_CAT_STRUCT *catReq,
                              STEP_INFO *step) 
{
	char msgbuf[IMS_COL1024_LEN+2];
	char sql[IMS_COL1024_LEN+2];
	int  status;

	/*
	** if no error occurred, update order_item again with end_status and 
	** step_started
	*/
	sql[0] = '\0';
	(void) sprintf (sql,
	  "update order_item set step_started_p = 'Y', status = %d "
		"where order_id = %d and item_id = %d",
		 step->end_status,
		 step->order_id,
		 step->item_id);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		msgbuf[0] = '\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (step->op_comment, 
		   "ims_step__update_after_submit: failed to update "
			 "order_item for order_id %d item_id %d.",
		        step->order_id,
		        step->item_id);
		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

		return (IMS_ERROR);
	}

	return (IMS_OK);

} /* end of update_after_submit */


/**********************************************************************
**
** record_error_status - update order_item table with ERROR status
**
** return IMS_OK       if successful
**        IMS_ERROR    if type 1 error occurred 
**        IMS_WARNING   if type 2 error occurred
**********************************************************************/
static int record_error_status (IMS_MSG_STRUCT *msgDesc,
                         V0_CAT_STRUCT *catReq,
                         STEP_INFO *step) 
{	
	char msgbuf[IMS_COL1024_LEN+2];
	char sql[IMS_COL1024_LEN+2];
	int  status;

	sql[0] = '\0';
	(void) sprintf (sql,
	  "update order_item set status = %d, op_comment = \"%s\" "
		"where order_id = %d and item_id = %d",
	     IMS_ERROR_ITEM,
		   step->op_comment,
		   step->order_id,
		   step->item_id);
	sql[strlen(sql)] = '\0';

	msgbuf[0] = '\0';

#ifdef QDEBUG
	(void) sprintf (msgbuf, "sql ==> %s", sql);
	(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	catReq->item[0] = (void *)sql;

	if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		msgbuf[0] = '\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (step->op_comment, 
	     "ims_step__record_error_status: failed to update "
			 "order_item for order_id %d item_id %d.",
		        step->order_id,
		        step->item_id);
		(void) sprintf (msgbuf, "%s", step->op_comment);

		(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);

		return (IMS_ERROR);
	}

	return (IMS_OK);

} /* end of record_error_status */

/**********************************************************************
**
** check_pps_result  - checks return code from PPS for IMS reqeust submission 
**
** return IMS_OK       if successful
**        IMS_ERROR    if type error occurred 
**********************************************************************/
static int check_pps_result ( IMS_MSG_STRUCT *msgDesc,
                              int             pps_ret_code,
                              STEP_INFO      *step)
{
#ifdef DCEON
	char msgbuf[IMS_COL255_LEN+1];
	char buf[IMS_COL255_LEN+1];
	extern char  *pps_err_msgs[];

	if (pps_ret_code != ER_NO_ERROR)
	{
		msgbuf[0] ='\0';
		buf[0] ='\0';
		step->op_comment[0] = '\0'; 

		(void) sprintf (buf, "ims_step__check_pps_result: Error from PPS: %s ",
												 pps_err_msgs[pps_ret_code]);
		free (pps_err_msgs);

		if (step->order_id == -1)
		{
			(void) sprintf (msgbuf, 
				 "%s. dataset_idx = %d granule_idx = %d",
				 buf,
			   step->dataset_idx,
			   step->granule_idx);
			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR);
		}
		else
		{

			(void) sprintf (step->op_comment, 
					"%s. order_id %d item_id %d.",
						buf,
	           step->order_id,
	           step->item_id);

			(void) sprintf (msgbuf, "%s", step->op_comment);

			(void) ims_msg (msgDesc, IMS_ERROR, msgbuf);
			return (IMS_ERROR);
		}
	}

#endif 
	return (IMS_OK);

} /* end of check_pps_result */

/**********************************************************************
** 
** init_pps_req -
**
**********************************************************************/
static int init_pps_req (STEP_INFO *step)
{
	PPS_REQ_STRUCT  *pps_req;

	pps_req       = &(step->pps_req);

 	pps_req->dataset_idx         = 0;
	pps_req->granules_table[0]   = '\0';
 	pps_req->temporal_type       = 0;
 	pps_req->granule_idx         = 0;
 	pps_req->priority            = step->priority; 
	pps_req->char_priority[0]    = '\0';
	strcpy (pps_req->quicklook_p, step->quicklook_p);
	pps_req->output_format[0]    = '\0';              /* i.e. media_fmt_type */
 	pps_req->process_type        = step->process_type;
	strcpy (pps_req->v0_process_type, step->v0_process_type);
	pps_req->platform[0]         = '\0';
	pps_req->sensor[0]           = '\0';
	pps_req->rev                 = 0;
	pps_req->mode[0]             = '\0';
	pps_req->sequence            = 0;
	pps_req->activity_id[0]      = '\0';
	pps_req->frame_id            = 0;
	pps_req->subframe_id         = 0;
	pps_req->compensation_p[0]       = '\0';
	pps_req->frame_mode[0]       = '\0';
	pps_req->media_type[0]       = '\0';
	pps_req->media_id_type[0]    = '\0';
	pps_req->media_id[0]         = '\0';
	pps_req->media_location[0]   = '\0';
	pps_req->data_direction[0]   = '\0';
	pps_req->start_address       = 0;
	pps_req->end_address         = 0;
	pps_req->start_time[0]       = '\0';
	pps_req->end_time[0]         = '\0';
	pps_req->center_time[0]      = '\0';
	pps_req->recorder_id[0]      = '\0';
	pps_req->station_id[0]       = '\0';
	pps_req->site_name[0]        = '\0';
	pps_req->product_type[0]     = '\0';
	pps_req->pixel_spacing       = 0.0;
	pps_req->projection[0]       = '\0';
	pps_req->processing_gain     = 0;
	pps_req->avg_terrain_ht      = 0.0;
	pps_req->deskew_p[0]         = '\0';
	pps_req->ps_reference_lat    = 0.0;
	pps_req->ps_reference_lon    = 0.0;
	pps_req->utm_zone            = 0;
	pps_req->terrain_correct_p[0]= '\0';
	pps_req->lambert_lat_n       = 0.0;
	pps_req->lambert_lat_s       = 0.0;

	return (IMS_OK);
}/* end of init_pps_req */
