static char *sccs = "@(#)ims_v0Order.c	5.14  09/05/97";

/************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** ims_v0Order.c
**
** Purpose
**		Order handling routines.
**
**	Creator   :   Julie Wang
**
**	Date      :   April 20, 1995
**
** Modifications:
**
**   10/20/96    jwang   Acct Mgr related changes.
**
**   07/24/96    jwang   User profile handling change.
**
**   07/24/96    jwang   Removed affliation info checking.   (PR977)
**   
**   06/06/96    jwang   PR907
**
**   06/06/96    jwang   Reset order_id to 0 if error occurred during insert
**
**   06/06/96    jwang   Allow quicklook in SCAN ITEM
**
**   05/24/96    jwang   PR 771, 849, and START_FIELD/END_FIELD time conv. 
**
**   03/26/96    jwang   Replaced calls to verify_media_option to 
**                       verify_process_option
**
**   03/26/96    jwang   Code modification for new pricing policy.
**                       Call v0_get_item_cost to get cost for an order item.
**
**   03/05/96    jwang   Add media_option verification for APRs.
**
**   02/20/96    jwang   R1Bprime (preliminary).
**
**   01/22/96    jwang   Changes added to handle DARs and SCAN requests
**
**   12/14/95    jwang   added error detection on environment veriables
**                       SYBASE, IMS_SERVER, IMS_DB, IMS_INT
**
**   12/12/95    jwang   added SCAN REQUEST handling
**
**   10/04/95    jwang   added call to IK_MakeAuthenticator to encrypt
**                       subsystem order authenticator so the user verification 
**                       would check the crypt_key for both v0 and subsystem
**                       orders.
**
**   09/14/95    jwang   added condition check between process_type = 0
**                       and process_type >= 1.  Code modification to 
**                       utilize common accounting routines.
**
**   09/05/95    jwang   modified error logging sheme
**
**   07/28/95    jwang   V3.10 schema changes
**
**   06/05/95    jwang   Changed all reference of cdb_ or CDB_ to 
**                       ims_ or IMS_
**
**   06/05/95    jwang   IMS front-end order processing
**
************************************************************************/

/*
** Undefine Posix Source flag because of imcompatibility
** with IK include files.
*/
#undef _POSIX_SOURCE

#include <stdio.h>
#include <string.h>
#include <IK_Network.h>
#include <odlinter.h>
#include <ims_query.h>
#include <ims_archive.h>
#include <ims_v0.h>
#include <ims_acct.h>
#include <ims_disp.h>
#include <ims_timeConv.h>

/*
** constants to be used by subsystem order requests
*/
#define  glbLogin            "ims_srv"
#define  glbPassword         "ims/srv"

#define  orderMask           16

static int order_verification (IMS_MSG_STRUCT *, V0_DESC_STRUCT *, int);
static int prepare_user_acct_result (IMS_MSG_STRUCT *, V0_DESC_STRUCT *);
static int insert_order (IMS_MSG_STRUCT *, V0_DESC_STRUCT *, int);
static int insert_profile (IMS_MSG_STRUCT *, V0_DESC_STRUCT *, int);
static int insert_order_item (IMS_MSG_STRUCT *, V0_DESC_STRUCT *, int);
static int insert_order_queue (IMS_MSG_STRUCT *, V0_DESC_STRUCT *, int);
static int generate_scan_item (IMS_MSG_STRUCT *, V0_DESC_STRUCT *);
static int insert_dar (IMS_MSG_STRUCT *, V0_DESC_STRUCT *, int);
static int insert_scan (IMS_MSG_STRUCT *, V0_DESC_STRUCT *);
static int log_order_error (IMS_MSG_STRUCT *, V0_DESC_STRUCT *, char *, int);
static int retrieve_daac_contact_info (IMS_MSG_STRUCT *, V0_DESC_STRUCT *);
static int check_existance_req (IMS_MSG_STRUCT *, V0_DESC_STRUCT *, int);
static int check_user_req (IMS_MSG_STRUCT *, V0_DESC_STRUCT *, int);
static int check_line_keyword(IMS_MSG_STRUCT *, V0_DESC_STRUCT *, int);

/*************************************************************************
**
** ims_order -
**
** purpose: interface for subsystems product requests and scan requests
**
** return :  IMS_OK    if successful
**           IMS_FATAL if system error occurs
**           IMS_ERROR if error occurrs
**************************************************************************/

int ims_order(IMS_MSG_STRUCT *msgDesc, AGGREGATE RxTree, int *order_id)
{

	V0_DESC_STRUCT  v0Desc;
	int  status;
	AGGREGATE  input_msg;

	input_msg = (AGGREGATE)NULL;

	/*
	** check if all required environment variables, i.e. IMS_SERVER, IMS_DB
	** SYBASE have been defined
	*/
	if (getenv("IMS_SERVER") == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
		     "ims_order: environment variable IMS_SERVER has not been defined.");
		(void) v0_handler__v0Desc_cleanup (&v0Desc);
		return (IMS_FATAL);		
	}

	if (getenv("IMS_DB") == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
		     "ims_order: environment variable IMS_DB has not been defined.");
		(void) v0_handler__v0Desc_cleanup (&v0Desc);
		return (IMS_FATAL);		
	}

	if (getenv("SYBASE") == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
		     "ims_order: environment variable SYBASE has not been defined.");
		(void) v0_handler__v0Desc_cleanup (&v0Desc);
		return (IMS_FATAL);		
	}

	/* 
	** initialize a v0Desc structure based on the type of the message
	*/

	if ((input_msg = FindAggregate (RxTree, "PRODUCT_REQUEST")) != NULL)
	{
		(void) v0_handler__v0Desc_init(msgDesc, RxTree, V0_PRODUCT_REQUEST, 0, 
	                &v0Desc, glbLogin, glbPassword);
	}
	else if ((input_msg = FindAggregate (RxTree, "SCAN_REQUEST")) != NULL)
	{
		(void) v0_handler__v0Desc_init(msgDesc, RxTree, V0_SCAN_REQUEST, 0, 
	                &v0Desc, glbLogin, glbPassword);
	}
	else if ((input_msg = FindAggregate (RxTree, "DAR_REQUEST")) != NULL)
	{
		(void) v0_handler__v0Desc_init(msgDesc, RxTree, V0_DAR_REQUEST, 0, 
	                &v0Desc, glbLogin, glbPassword);
	}

	/* assign -1 as callerFlag */
	v0Desc.request.callerFlag = -1;

	if ( (status = v0_cat (&v0Desc.catReq, V0_OPENCONNECTION)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
		               "ims_order: Database login failed.");
		(void) v0_handler__v0Desc_cleanup (&v0Desc);
		return (IMS_FATAL);		
	}
	else
	{

#ifdef ODEBUG
	(void)PrintLabel(RxTree);
#endif 

		status = v0_order__place_order(msgDesc, &v0Desc, v0Desc.request.callerFlag); 
		if (v0_cat (&v0Desc.catReq, V0_CLOSECONNECTION) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_WARNING, 
				"ims_order: Database close failed.");
			(void) v0_handler__v0Desc_cleanup (&v0Desc);
			return (IMS_FATAL);		
		}

	}


	*order_id = v0Desc.request.order_id;

	(void) v0_handler__v0Desc_cleanup (&v0Desc);

	return (status);

}/* end of ims_order */

/*************************************************************************
**
** v0_order__place_order -
**
** purpose: handles V0 or subsystems orders 
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/

int v0_order__place_order(IMS_MSG_STRUCT *msgDesc,
                          V0_DESC_STRUCT *v0Desc,
			  int callerFlag)
{
	int   status, dstatus;
	char 	msgBuf[IMS_COL80_LEN+1];
	char 	*auth_ptr = NULL;
	int   order_p = 0;
	int   scan_p = 0;
	int   dar_acct_p = 0;
	int   dar_req_p = 0;

	order_p      = 0;
	scan_p       = 0;
	dar_acct_p   = 0;
	dar_req_p    = 0;

	/*
	** callerFlag = 1 for V0 orders and callerFlag = -1 for subsystem orders
	** there may be a list of error status get passed back to the caller 
	**
	** When callerFlag = 1 and an error occurs, we will create an associating 
	** entry in the v0_order_error table as a workaround before V0 client
	** establishes a way for DAACs to return order related error status.
	** 
	*/

	/*
	** retrieve user service information, which will be utilized as
	** external Product request and Dar request result, and for sending 
	** email message to external users. 
	**
	** In addition, retrieve user's email address if it's a dar request.
	** Because the information does not come with the request.  
	*/
	if ( (callerFlag == 1) &&
			 ((v0Desc->RxType == V0_PRODUCT_REQUEST) ||
				(v0Desc->RxType == V0_DAR_REQUEST)))
	{
		if ( (dstatus = retrieve_daac_contact_info (msgDesc, v0Desc)) < IMS_OK)
		{
			/* set order_id to be 0 as indication of order related error */
			v0Desc->request.order_id = 0;
			v0Desc->result.order_id = 0;

			return (dstatus);
		}

	}

	/* 
	** parse the request 
	*/
	if ((status = v0_process__parse_RxTree (v0Desc)) < IMS_OK)
	{
		/*
		** return DAAC CONTACT INFO for IMS_ERROR condition.
		*/
		return (status);

	}

	if ( v0Desc->RxType == V0_PRODUCT_REQUEST )
	{
		order_p = 1;
	}
	else if ( v0Desc->RxType == V0_SCAN_REQUEST )
	{
		scan_p = 1;
	}
	else if ( v0Desc->RxType == V0_DAR_ACCT_SEARCH )
	{
		dar_acct_p = 1;
	}
	else if ( v0Desc->RxType == V0_DAR_REQUEST )
	{
		dar_req_p = 1;
	}

	/*
	** the authenticator sbmitted by subsystem is in fact only the password
	** we'll encrypt it to the real authenticator before verification 
	*/
	if (callerFlag == -1)
	{
		if ((scan_p ) || (dar_req_p))
		{
			if ( (v0Desc->request.user_info.first_name[0] == '\0') ||
			     (v0Desc->request.user_info.last_name[0]  == '\0') ||
			     (v0Desc->request.authenticator[0]        == '\0'))
			{
				msgBuf[0] = '\0';
				(void) sprintf (msgBuf,"Missing first_name, last_name, "
				       "or authenticator.");
				(void) ims_msg(msgDesc, IMS_ERROR, msgBuf);
				return (IMS_ERROR);

			}

			if ((auth_ptr = (char *)IK_MakeAuthenticator 
		         	(v0Desc->request.user_info.first_name,
								v0Desc->request.user_info.last_name,
								v0Desc->request.authenticator)) == NULL)
			{
				msgBuf[0] = '\0';
				(void) sprintf (msgBuf,"Missing authenticator.");
				(void) ims_msg(msgDesc, IMS_ERROR, msgBuf);
				return (IMS_ERROR);
			}
		}
		else if (order_p)
		{
			if ( (v0Desc->request.contact_profile.first_name[0] == '\0') ||
			     (v0Desc->request.contact_profile.last_name[0]  == '\0') ||
			     (v0Desc->request.authenticator[0]        == '\0'))
			{
				msgBuf[0] = '\0';
				(void) sprintf (msgBuf,"Missing first_name, last_name, "
				       "or authenticator.");
				(void) ims_msg(msgDesc, IMS_ERROR, msgBuf);
				return (IMS_ERROR);

			}

			if ((auth_ptr = (char *)IK_MakeAuthenticator 
		         	(v0Desc->request.contact_profile.first_name,
								v0Desc->request.contact_profile.last_name,
								v0Desc->request.authenticator)) == NULL)
			{
				msgBuf[0] = '\0';
				(void) sprintf (msgBuf,"Missing authenticator for request %s.",
			                    	v0Desc->request.order_request_id);
				(void) ims_msg(msgDesc, IMS_ERROR, msgBuf);
				return (IMS_ERROR);
			}
		}

		v0Desc->request.authenticator[0] = '\0';
		(void) sprintf (v0Desc->request.authenticator, "%s", auth_ptr);
		free (auth_ptr);

	}

	/* 
	** check if any required field is missing, also calculate the cost of each
	** line item.
	*/

	if ( (status = order_verification (msgDesc, v0Desc, callerFlag)) < IMS_OK)
	{
			return (status);
	}

	/*
	** For scan request, we need to generate one order_item entry
	** per datatake entry corresponding to the downlink associated
	** with the request.  
	*/
	if (scan_p)
	{
		if ((status = generate_scan_item(msgDesc, v0Desc)) <IMS_OK)
		{
			return (status);
		}
	}

	/*
	** prepare results for USER_ACCT_SEARCH if the request was verified,
	** and return to the caller from here since no more processing is
	** needed after this point
	*/
	if (dar_acct_p)
	{
		if ( (status = prepare_user_acct_result (msgDesc, v0Desc)) <IMS_OK)
		{
			return (status);
		}

		return (IMS_OK);
	}

#ifdef DEBUG
	else
	{
		msgBuf[0] = '\0';
		(void) sprintf (msgBuf, "the request has been verified.");
		(void) ims_msg(msgDesc, IMS_INFO, msgBuf);
	}
#endif 

	/*
	** Done with all the preparation for ordering. Insert info into catalog 
	*/ 
	if ( (status = insert_order (msgDesc, v0Desc, callerFlag)) < IMS_OK)
	{
		return (status);
	}

#ifdef MDEBUG
	else
	{
		msgBuf[0] = '\0';
		(void) sprintf (msgBuf, "Order_id %d has been inserted into catalog.", 
		                v0Desc->request.order_id);
		(void) ims_msg(msgDesc, IMS_INFO, msgBuf);
	}
#endif 


 	return (IMS_OK);

}/* end of place_order */

/*************************************************************************
**
** order_verification -
**
** purpose: verifies incoming product requests, scan requests, or dar 
**          related requests, estimates cost if needed, and collect 
**          information which will be stored into the order tables. 
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int order_verification (IMS_MSG_STRUCT *msgDesc, 
                               V0_DESC_STRUCT *v0Desc, 
                               int callerFlag)
{
	int    status;

	if ( (status = check_existance_req (msgDesc, v0Desc, callerFlag)) <IMS_OK) 
	{
		return (status);
	}

	if ( (status = check_user_req (msgDesc, v0Desc, callerFlag)) <IMS_OK) 
	{
		return (status);
	}

	if ( v0Desc->RxType == V0_DAR_ACCT_SEARCH )
	{
		return (IMS_OK);
	}

	if ( (status = check_line_keyword (msgDesc, v0Desc, callerFlag)) <IMS_OK) 
	{
		return (status);
	}

	return (IMS_OK);

}/* end of order_verification */

/*************************************************************************
**
** check_existance_req -
**
** purpose: check existance of required keyword on the top level of an order 
**          and perform range checking if needed
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int check_existance_req (IMS_MSG_STRUCT *msgDesc, 
                               V0_DESC_STRUCT *v0Desc, 
                               int callerFlag)
{
	V0_REQUEST_STRUCT *request;
	V0_CAT_STRUCT     *catReq;
	int    status;
	char   errbuf[IMS_COL255_LEN+1];      
	char   v0_errbuf[IMS_COL255_LEN+1];  
	char   msgbuf[IMS_COL1024_LEN+1];   
	int    rowCount, count;
	char   temp_account[IMS_COL15_LEN+1];
	int    order_p = 0;       /* set to 1 if it's a PRODUCT REQUEST */
	int    scan_p = 0;        /* set to 1 if it's a SCAN REQUEST */
	int    dar_acct_p = 0;    /* set to 1 if it's a USER ACCT SEARCH */
	int    dar_req_p = 0;     /* set to 1 if it's a DAR REQUEST */
	V0_LINE_ITEM_LIST *line_item;
	IMS_NUMERIC_DATE temp_start, temp_end;
	V0_PROFILE_INFO  *hp, *tp;
	char   temp_activity_start[IMS_COL30_LEN], temp_activity_end[IMS_COL30_LEN];
	int    error;

	request  =  &v0Desc->request;
	catReq   =  &v0Desc->catReq;

	if ( v0Desc->RxType == V0_PRODUCT_REQUEST )
	{
		order_p = 1;
	}
	else if ( v0Desc->RxType == V0_SCAN_REQUEST )
	{
		scan_p = 1;
	}
	else if ( v0Desc->RxType == V0_DAR_ACCT_SEARCH )
	{
		dar_acct_p = 1;
	}
	else if ( v0Desc->RxType == V0_DAR_REQUEST )
	{
		dar_req_p = 1;
	}

	/* -------------------------------------
	** request_id check for PRODUCT REQUEST
	** -------------------------------------
	**
	*/
	if ((request->order_request_id[0] == '\0') && (order_p))
	{
		errbuf[0] = '\0';
		v0_errbuf[0] = '\0';

		(void) sprintf (v0_errbuf, "Invalid PRODUCT REQUEST. Missing REQUEST_ID.");
		(void) sprintf (errbuf, "check_existance_req: %s", v0_errbuf);

		if ( (status = v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
					< IMS_OK)
		{
			return (status);
		}
		else 
		{
			return (IMS_ERROR);
		}

	}

	/* ----------------------------------------------------------------
	** name, authenticator, account_id, and/or user_id  existance check 
	** ----------------------------------------------------------------
	*/

	/* 
	** USER_ACCT_REQUEST and subsystem DAR_REQUEST require firstname,
	** lastname and authenticator.  
	** 
	** Account id is also required for subsystem DAR_REQUESTs 
	*/
	if ( (dar_acct_p) || 
	     ((dar_req_p) && (callerFlag == -1)))
	{
		if ( (request->user_info.first_name[0] == '\0') ||
		     (request->user_info.last_name[0] == '\0')  ||
		     (request->authenticator[0] == '\0') ||
				 (dar_req_p && (request->account_id[0] == '\0')) )
		{
			(void) strcpy (v0_errbuf, "Missing name(s), password, or account_id.");
			(void) sprintf (errbuf, "check_existance_req: %s", v0_errbuf);
			(void) ims_msg (msgDesc, IMS_ERROR, errbuf);
		
			/* construct status_code_comment */
					
			if (dar_acct_p)
			{
				if ( (v0Desc->result.odl_status_code_comment =
		           (V0_ERR_LIST *)malloc(sizeof(V0_ERR_LIST))) == NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL, 
			    	"check_existance_req: Memory allocation failed for "
						"odl_status_code_comment.");
	
					/* 03 is system error status for DARs */
					(void) strcpy (v0Desc->odl_status, "03");
					return(IMS_FATAL);
				}
		
				/* 02 is user error status for DARs*/
				(void) strcpy (v0Desc->odl_status, "02");
				v0Desc->result.odl_status_code_comment->err_buf[0] = '\0';
				(void)strcpy(v0Desc->result.odl_status_code_comment->err_buf,v0_errbuf);
				v0Desc->result.odl_status_code_comment->next_p = (V0_ERR_LIST *)NULL;
				return (IMS_ERROR);
			}

			return (IMS_ERROR);
		}
	}

	/* 
	** V0 DAR_REQUEST requires firstname, lastname, account_id and user_id 
	*/
	else if ( (dar_req_p) && (callerFlag == 1))
	{
		if ( (request->user_info.first_name[0] == '\0') ||
		     (request->user_info.last_name[0] == '\0')  ||
				 (request->account_id[0] == '\0') || 
				 (request->user_id[0] == '\0') ) 
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
				"check_existance_req: Missing  name, account_id, "
				"or user_id in DAR REQUEST.");

			(void) strcpy (v0Desc->odl_status, "03");
			return (IMS_ERROR);
		}
	}

	if (dar_req_p)
	{

		/*---------------------------------------------------------
		** check existance of required fields
		**
		** missing required fields is a system error for V0 interface
		** since these fields should have been checked before the 
		** ODL is submitted 
		**---------------------------------------------------------
		*/ 

		/* quicklook, direction */
		if ( (request->quicklook_p[0] == '\0') ||
				 (request->asc_desc[0] == '\0') ||
				 (request->active_p[0] == '\0'))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"check_existance_req: Missing QUICK_LOOK, DIRECTION, "
				"PRIORITY, or ACTIVITY specification for DAR REQUEST.");
			(void) strcpy (v0Desc->odl_status, "03");
			return (IMS_ERROR);
		}
	
		else if ( ( (strcmp (request->quicklook_p, "Y") != 0) &&
							  (strcmp (request->quicklook_p, "N") != 0)) ||
							( (strcmp (request->asc_desc, "A") != 0) &&
								(strcmp (request->asc_desc, "D") != 0) &&
								(strcmp (request->asc_desc, "B") != 0))    ||
							( request->dar_priority == 0)                ||
							( (strcmp (request->active_p, "Y") != 0) &&
								(strcmp (request->active_p, "N") != 0) ))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"check_existance_req: Undefined value specified either in  "
				"QUICK_LOOK, DIRECTION, PRIORITY, or ACTIVITY specification "
				"for DAR REQUEST.");
			(void) strcpy (v0Desc->odl_status, "03");
			return (IMS_ERROR);
		}

		/* sensor existance check */
		if (request->sensor == (V0_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
				"check_existance_req: Missing SENSOR_NAME specification for "
				"DAR REQUEST.");
			(void) strcpy (v0Desc->odl_status, "03");
			return (IMS_ERROR);
		}

		/* verify sensor */
		else
		{
			v0Desc->query.sql[0] = '\0';

			(void)sprintf (v0Desc->query.sql, 
				"select sensor from sensors where sensor = '%s'", 
		        request->sensor->char_value1);

			/* item[0] points to the sql statement */
			catReq->item[0] = (void *)v0Desc->query.sql;
	
			rowCount = 0;
			catReq->item[1] = (int *)&rowCount;
			
#ifdef QDEBUG
			sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql); 
			(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

			if ( (status = v0_cat (catReq, V0_VERIFYENTRYCHAR) ) < IMS_OK)
			{
				return (status);
			}

			if ( (rowCount == 0) || (rowCount >= 2))
			{
				(void)ims_msg(msgDesc, IMS_ERROR,
					"check_existance_req: Undefined SENSOR_NAME %s for DAR REQUEST.", 
				       request->sensor->char_value1);
				(void) strcpy (v0Desc->odl_status, "03");
				return (IMS_ERROR);
			}
		}

		/* platform existance check */
		if (request->platform == (V0_VALUE_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
				"check_existance_req: Missing SOURCE_NAME (i.e. platform) "
				"specification for DAR REQUEST.");
			(void) strcpy (v0Desc->odl_status, "03");
			return (IMS_ERROR);
		}
		/* verify platform */
		else
		{
			v0Desc->query.sql[0] = '\0';

			(void)sprintf (v0Desc->query.sql, 
				"select platform from platforms where platform = '%s'", 
		        request->platform->char_value1);

			/* item[0] points to the sql statement */
			catReq->item[0] = (void *)v0Desc->query.sql;
	
			rowCount = 0;
			catReq->item[1] = (int *)&rowCount;
			
#ifdef QDEBUG
			sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql); 
			(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

			if ( (status = v0_cat (catReq, V0_VERIFYENTRYCHAR) ) < IMS_OK)
			{
				return (status);
			}

			if ( (rowCount == 0) || (rowCount >= 2))
			{
				(void)ims_msg(msgDesc, IMS_ERROR,
						"check_existance_req: Undefined SOURCE_NAME %s for DAR REQUEST.", 
				       request->platform->char_value1);
				(void) strcpy (v0Desc->odl_status, "03");
				return (IMS_ERROR);
			}
		}

		/* 
		** check mode existance if it's RADARSAT-1 
		*/
		if ( strcmp (request->platform->char_value1, "RADARSAT-1") == 0)
		{
			if (request->mode[0] == '\0')
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
					"check_existance_req: Missing MODE specification "
					"for RADARSAT-1 related DAR REQUEST.");
				(void) strcpy (v0Desc->odl_status, "03");
				return (IMS_ERROR);
			}

			/* mode range checking */
			else
			{
				/* range checking for mode values */
				if ( (strcmp (request->mode, "ST1") != 0) &&
						 (strcmp (request->mode, "ST2") != 0) &&
						 (strcmp (request->mode, "ST3") != 0) &&
						 (strcmp (request->mode, "ST4") != 0) &&
						 (strcmp (request->mode, "ST5") != 0) &&
						 (strcmp (request->mode, "ST6") != 0) &&
						 (strcmp (request->mode, "ST7") != 0) &&
						 (strcmp (request->mode, "SNA") != 0) &&
						 (strcmp (request->mode, "SNB") != 0) &&
						 (strcmp (request->mode, "SWA") != 0) &&
						 (strcmp (request->mode, "SWB") != 0) &&
						 (strcmp (request->mode, "WD1") != 0) &&
						 (strcmp (request->mode, "WD2") != 0) &&
						 (strcmp (request->mode, "WD3") != 0) &&
						 (strcmp (request->mode, "FN1") != 0) &&
						 (strcmp (request->mode, "FN2") != 0) &&
						 (strcmp (request->mode, "FN3") != 0) &&
						 (strcmp (request->mode, "FN4") != 0) &&
						 (strcmp (request->mode, "FN5") != 0) &&
						 (strcmp (request->mode, "EL1") != 0) &&
						 (strcmp (request->mode, "EH1") != 0) &&
						 (strcmp (request->mode, "EH2") != 0) &&
						 (strcmp (request->mode, "EH3") != 0) &&
						 (strcmp (request->mode, "EH4") != 0) &&
						 (strcmp (request->mode, "EH5") != 0) &&
						 (strcmp (request->mode, "EH6") != 0) &&
						 (strcmp (request->mode, "SAR") != 0)) 
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
						"check_existance_req: Undefined MODE specification %s "
						"for RADARSAT-1 in DAR REQUEST.", request->mode);
					(void) strcpy (v0Desc->odl_status, "03");
					return (IMS_ERROR);
				}
			}
		}

		/* start_time, end_time, observation_number, observation_freq existance
			 check */
		if ( (request->start_time[0] == '\0') ||
				 (request->end_time[0] == '\0')) 
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
				"check_existance_req: Missing START_DATE or STOP_DATE "
				"specification for DAR REQUEST.");
			(void) strcpy (v0Desc->odl_status, "03");
			return (IMS_ERROR);
		}
		else
		{

			/*
			** start_time should be earlier than stop_time
			*/
			error = v0_time__compareIMSA(request->start_time, request->end_time);

			if ( error )
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
					"check_existance_req: START_DATE exceeds STOP_DATE in DAR REQUEST.");
				/* V0 u/i should have compared the values before sending the DAR
				 	so it's a system error */
				(void) strcpy (v0Desc->odl_status, "03");
				return(IMS_ERROR);
			}
		}

		/* activity_start_date, activity_end_date are required if active_p is Y */
		if ( strcmp (request->active_p, "Y") == 0)
		{
			if ( (request->activity_start_date[0] == '\0') ||
				 	(request->activity_end_date[0] == '\0')) 
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
					"check_existance_req: Missing START_FIELD or "
					"END_FIELD specification for DAR REQUEST.");
				(void) strcpy (v0Desc->odl_status, "03");
				return (IMS_ERROR);
			}

			/* 
			** convert user provided activity_start_date and activity_end_date
			** into IMS time format
			*/

			if ((status = ims_timeToNumericDate 
				(msgDesc, request->activity_start_date, &temp_start)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"check_existance_req: error format for START_FIELD. "
					"Please specify exactly in the form of yyyy-mm-dd, e.g. 1995-03-20 ");
				(void) strcpy (v0Desc->odl_status, "03");
				return (IMS_ERROR);
			}

			if ((status = ims_timeToNumericDate 
				(msgDesc, request->activity_end_date, &temp_end)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"check_existance_req: error format for END_FIELD. "
					"Please specify exactly in the form of yyyy-mm-dd , e.g. 1995-03-20");
				(void) strcpy (v0Desc->odl_status, "03");
				return (IMS_ERROR);
			}

			temp_activity_start[0] = temp_activity_end[0] = '\0';
			(void) ims_numericDateToIMSA (&temp_start, temp_activity_start);
			(void) ims_numericDateToIMSA (&temp_end, temp_activity_end);

			error = v0_time__compareIMSA(temp_activity_start, temp_activity_end);

			if ( error )
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
					"check_existance_req: START_FIELD exceeds END_FIELD in DAR REQUEST.");
				(void) strcpy (v0Desc->odl_status, "03");
				return(IMS_ERROR);
			}
			else
			{
				strcpy (request->activity_start_date, temp_activity_start);
				strcpy (request->activity_end_date, temp_activity_end);
			}
		}

		if (((request->observation_num == -1) && 
		     (request->observation_freq[0] !='\0')) ||
		    ((request->observation_num != -1) &&
		     (request->observation_freq[0] == '\0')))
		/*
		if ( (request->observation_num == -1) || 
				 (request->observation_freq[0] == '\0'))
		*/
		{
			
			(void) ims_msg (msgDesc, IMS_ERROR, 
				"check_existance_req: Missing OB_NUMBER or OB_FREQUENCY "
				"specification for DAR REQUEST.");
			(void) strcpy (v0Desc->odl_status, "03");
			return (IMS_ERROR);
		}

		/* site_shape existance check */
		if ( request->spatial_type == 0)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
				"check_existance_req: Missing or undefined SITE_SHAPE "
				"specification for DAR REQUEST.");
			(void) strcpy (v0Desc->odl_status, "03");
			return (IMS_ERROR);
		}

		/* check range of geographical specification, the existance checking
			 has been done during parsing time */

		if ( ( (request->spatial_type == 3) &&
					 ( (request->center_lat > 90.0) ||
				     (request->center_lat < -90.0))) ||
				 ( (request->spatial_type == 4) &&
					 ( (request->nw_lat > 90.0) ||
						 (request->nw_lat < -90.0) ||
						 (request->ne_lat > 90.0) ||
						 (request->ne_lat < -90.0) ||
						 (request->se_lat > 90.0) ||
						 (request->se_lat < -90.0) ||
						 (request->sw_lat > 90.0) ||
						 (request->sw_lat < -90.0))))
		{
				(void) ims_msg (msgDesc, IMS_ERROR, 
					"check_existance_req: Latitude specification out of range. "
					"Please specify latitudes between -90.0 and 90.0");
				(void) strcpy (v0Desc->odl_status, "03");
				return (IMS_ERROR);
		}

		if ( ( (request->spatial_type == 3) &&
					 ( (request->center_lon > 180.0) ||
				     (request->center_lon < -180.0))) ||
				 ( (request->spatial_type == 4) &&
					 ( (request->nw_lon > 180.0) ||
						 (request->nw_lon < -180.0) ||
						 (request->ne_lon > 180.0) ||
						 (request->ne_lon < -180.0) ||
						 (request->se_lon > 180.0) ||
						 (request->se_lon < -180.0) ||
						 (request->sw_lon > 180.0) ||
						 (request->sw_lon < -180.0))))
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
				"check_existance_req: Latitude specification out of range. "
				"Please specify longitudes between -180.0 and 180.0");
			(void) strcpy (v0Desc->odl_status, "03");
			return (IMS_ERROR);
		}

	}

	/* 
	** SCAN_REQUEST requires firstname, lastname, account_id and authenticator
	*/
	else if (scan_p)
	{
		if ((request->account_id[0] == '\0') || 
				(request->user_info.first_name[0] == '\0') ||
				(request->user_info.last_name[0]  == '\0') ||
				(request->authenticator[0] == '\0'))
		{

			errbuf[0] = '\0';
		
			(void) sprintf (errbuf, 
				"check_existance_req: Invalid SCAN REQUEST. "
				"Missing either FIRST_NAME, LAST_NAME, AUTHENTICATOR, or BILLING_ID.");
			(void) ims_msg (msgDesc, IMS_ERROR, errbuf);	

			return (IMS_ERROR);
		}
	
	}

	/* 
	** PRODUCT_REQUEST reqires authenticator, and names in the contact address.
	** Names are also required if billing or shipping address are provided 
	*/
	else if (order_p)
	{
		if (request->authenticator[0] == '\0')
		{
			errbuf[0] = '\0';
			v0_errbuf[0] = '\0';
	
			(void) sprintf (v0_errbuf, 
				"Invalid PRODUCT REQUEST, request = %s. "
				"Missing RESTRICTED DATA ACCESS KEY "
				"(i.e. AUTHENTICATOR, Required for ASF orders).", 
			       request->order_request_id);

			(void) sprintf (errbuf, "check_existance_req: %s", v0_errbuf); 
	
			if ( (status = v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
						< IMS_OK)
			{
				return (status);
			}
			else 
			{
				return (IMS_ERROR);
			}

		}

		/* 
		** contact profile is required for PRODUCT REQUEST 
		*/
		if (request->contact_p == 0)
		{
			errbuf[0] = '\0';
			v0_errbuf[0] = '\0';
		
			(void) sprintf (v0_errbuf, 
					"Invalid PRODUCT REQUEST, request_id = %s. Missing CONTACT_ADDRESS"
					" (i.e. User Address)", 
	         request->order_request_id);
			(void) sprintf (errbuf, "check_existance_req: %s", v0_errbuf);
		
			if ((status =v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
					< IMS_OK)
			{
				return (status);
			}
			else 
			{
				return (IMS_ERROR);
			}
		
		}
		
		/*
		** check keyword existance in contact profile.
		**
		** first_name, last_name are required
		** address, city, state, country, zipcode, phone, email, fax, and 
		** organization are 'combined optional', which means they can be 
		** either all null or all filled.  
		**
		** When nothing provided, the information will be retrieved from user 
		** profile.
		**
		** Partially filled information is considered an error
		*/
		if ((request->contact_profile.first_name[0] == '\0') || 
				(request->contact_profile.last_name[0] == '\0')) 
		{
			errbuf[0] = '\0';
			v0_errbuf[0] = '\0';
		
			(void) sprintf (v0_errbuf, 
					"Invalid PRODUCT REQUEST, request_id = %s. "
					"Missing first_name or last_name in CONTACT ADDRESS."
					" (i.e. User Address)", 
		 	         request->order_request_id);
			(void) sprintf (errbuf, "check_existance_req: %s", v0_errbuf); 
		
			if ((status =v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
						< IMS_OK)
			{
				return (status);
			}
			else 
			{
				return (IMS_ERROR);
			}
		
		}

		if ((request->contact_profile.organization[0] == '\0') &&
				(request->contact_profile.street[0] == '\0') &&
				(request->contact_profile.city[0] == '\0') &&
				(request->contact_profile.state[0] == '\0') &&
				(request->contact_profile.zipcode[0] == '\0') &&
				(request->contact_profile.country[0] == '\0') &&
				(request->contact_profile.phone[0] == '\0') &&
				(request->contact_profile.email[0] == '\0') &&
				(request->contact_profile.fax[0] == '\0'))
		{
			/*
			** retrieve required values from the user profile
			*/
			v0Desc->query.sql[0] = '\0';

			(void)sprintf (v0Desc->query.sql, 
				"select organization, street, city, state, zipcode, country, phone, "
				"email, fax from user_profile where upper(first_name) = '%s' and"
				" upper(last_name) = '%s' and crypt_key = '%s'", 
		    	request->contact_profile.first_name,
					request->contact_profile.last_name,
					request->authenticator);

			/* item[0] points to the sql statement */
			catReq->item[0] = (void *)v0Desc->query.sql;
	
			rowCount = 0;
			catReq->item[1] = (int *)&rowCount;
			
#ifdef QDEBUG
			sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql); 
			(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

			if ( (status = v0_cat (catReq, V0_GETPROFILEINFO) ) < IMS_OK)
			{
				return (status);
			}

			hp = (V0_PROFILE_INFO *)catReq->item[2];

			/* 
			** if no matching information or multiple entries found, 
			** reject the order 
			*/
			if ( (rowCount == 0) || (rowCount >= 2))
			{
				while (hp != (V0_PROFILE_INFO *)NULL)
				{
					tp = hp->next_p;
					free (hp);
					hp = tp;
				}

				errbuf[0] = '\0';
				v0_errbuf[0] = '\0';
		
				(void) sprintf (v0_errbuf, 
					"Invalid PRODUCT REQUEST, request_id = %s. "
					"Either user name in CONTACT ADDRESS (i.e. User Address) %s %s "
					"is not regiestered with ASF or the password was incorrect.",
		 	         request->order_request_id,
							 request->contact_profile.first_name,
							 request->contact_profile.last_name);
				(void) sprintf (errbuf, "check_existance_req: %s", v0_errbuf); 
		
				if ((status =v0_order__report_order_error 
										(msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
							< IMS_OK)
				{
					return (status);
				}
				else 
				{
					return (IMS_ERROR);
				}
		
			}

			strcpy (request->contact_profile.organization, hp->organization);
			strcpy (request->contact_profile.street, hp->street); 
			strcpy (request->contact_profile.city, hp->city);
			strcpy (request->contact_profile.state, hp->state);
			strcpy (request->contact_profile.zipcode, hp->zipcode);
			strcpy (request->contact_profile.country, hp->country);
			strcpy (request->contact_profile.phone, hp->phone);
			strcpy (request->contact_profile.email, hp->email);
			strcpy (request->contact_profile.fax, hp->fax);

			while (hp != (V0_PROFILE_INFO *)NULL)
			{
				tp = hp->next_p;
				free (hp);
				hp = tp;
			}

		}
		else if ((request->contact_profile.organization[0] != '\0') &&
				     (request->contact_profile.street[0] != '\0') &&
				     (request->contact_profile.city[0] != '\0') &&
				     (request->contact_profile.state[0] != '\0') &&
				     (request->contact_profile.zipcode[0] != '\0') &&
				     (request->contact_profile.country[0] != '\0') &&
				     (request->contact_profile.phone[0] != '\0') &&
				     (request->contact_profile.email[0] != '\0') &&
				     (request->contact_profile.fax[0] != '\0'))
		{
				/* NO OP */
		}

		/*
		** the other cases, i.e. partially filled contact profile, is considered
		** user error.
		*/
		else
		{
			errbuf[0] = '\0';
			v0_errbuf[0] = '\0';
		
			(void) sprintf (v0_errbuf, 
					"Invalid PRODUCT REQUEST, request_id = %s. "
					"Missing organization, street address, city, state, zipcode, "
					"country, phone, email, or fax number in CONTACT ADDRESS"
					" (i.e. User Address).", 
		 	         request->order_request_id);
			(void) sprintf (errbuf, "check_existance_req: %s", v0_errbuf); 
		
			if ((status =v0_order__report_order_error 
									(msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
						< IMS_OK)
			{
				return (status);
			}
			else 
			{
				return (IMS_ERROR);
			}
		
		}

		/*
		** SHIPPING ADDRESS is optional. Keyword existance has to be check
		** if shipping profile is specified.
		*/
		if (request->shipping_p == 1)
		{
		
			if ((request->shipping_profile.first_name[0] == '\0') || 
					(request->shipping_profile.last_name[0] == '\0')) 
			{
				errbuf[0] = '\0';
				v0_errbuf[0] = '\0';
		
				(void) sprintf (v0_errbuf, 
					"Invalid PRODUCT REQUEST, request_id = %s. "
					"Missing first_name or last_name in SHIPPING ADDRESS.", 
		            request->order_request_id);
			
				(void) sprintf (errbuf, "check_existance_req: %s", v0_errbuf); 
			
				if ( (status = v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
								< IMS_OK)
				{
					return (status);
				}
				else 
				{
					return (IMS_ERROR);
				}

			}

			if ((request->shipping_profile.organization[0] == '\0') &&
				  (request->shipping_profile.street[0] == '\0') &&
				  (request->shipping_profile.city[0] == '\0') &&
				  (request->shipping_profile.state[0] == '\0') &&
				  (request->shipping_profile.zipcode[0] == '\0') &&
				  (request->shipping_profile.country[0] == '\0') &&
				  (request->shipping_profile.phone[0] == '\0') &&
				  (request->shipping_profile.email[0] == '\0') &&
				  (request->shipping_profile.fax[0] == '\0'))
			{
				/*
				** copy information over from contact profile
				*/
			 	strcpy (request->shipping_profile.organization,
			 	        request->contact_profile.organization);
				strcpy (request->shipping_profile.street, 
				        request->contact_profile.street); 
				strcpy (request->shipping_profile.city,
				        request->contact_profile.city);
				strcpy (request->shipping_profile.state,
				        request->contact_profile.state);
				strcpy (request->shipping_profile.zipcode,
				        request->contact_profile.zipcode);
				strcpy (request->shipping_profile.country,
				        request->contact_profile.country);
				strcpy (request->shipping_profile.phone,
				        request->contact_profile.phone);
				strcpy (request->shipping_profile.email,
				        request->contact_profile.email);
				strcpy (request->shipping_profile.fax,
				        request->contact_profile.fax);
			}
			else if ((request->shipping_profile.organization[0] != '\0') &&
				       (request->shipping_profile.street[0] != '\0') &&
				       (request->shipping_profile.city[0] != '\0') &&
				       (request->shipping_profile.state[0] != '\0') &&
				       (request->shipping_profile.zipcode[0] != '\0') &&
				       (request->shipping_profile.country[0] != '\0') &&
				       (request->shipping_profile.phone[0] != '\0') &&
				       (request->shipping_profile.email[0] != '\0') &&
				       (request->shipping_profile.fax[0] != '\0'))
			{
				/* NO OP */
			}

			/*
			** the other cases, i.e. partially filled shipping profile, is considered
			** user error.
			*/
			else
			{
				errbuf[0] = '\0';
				v0_errbuf[0] = '\0';
		
				(void) sprintf (v0_errbuf, 
					"Invalid PRODUCT REQUEST, request_id = %s. "
					"Missing organization, street address, city, state, zipcode, "
					"country, phone, email, or fax number in SHIPPING ADDRESS.",
		 	         request->order_request_id);
				(void) sprintf (errbuf, "check_existance_req: %s", v0_errbuf); 
		
				if ((status =v0_order__report_order_error 
										(msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
						< IMS_OK)
				{
					return (status);
				}
				else 
				{
					return (IMS_ERROR);
				}
		
			}
		}
		
		/*
		** BILLING ADDRESS is optional. Keyword existance has to be check
		** if billing profile is specified.
		*/
		if (request->billing_p == 1)
		{
			if ((request->billing_profile.first_name[0] == '\0') || 
					(request->billing_profile.last_name[0] == '\0')) 
			{
				errbuf[0] = '\0';
				v0_errbuf[0] = '\0';
		
				(void) sprintf (v0_errbuf, 
					"Invalid PRODUCT REQUEST, request_id = %s. "
					"Missing first name or last_name in BILLING ADDRESS.", 
			            		request->order_request_id);
				(void) sprintf (errbuf, "check_existance_req: %s", v0_errbuf); 
			
				if ( (status = v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
								< IMS_OK)
				{
					return (status);
				}
				else 
				{
					return (IMS_ERROR);
				}
		
			}

			if ((request->billing_profile.organization[0] == '\0') &&
				  (request->billing_profile.street[0] == '\0') &&
				  (request->billing_profile.city[0] == '\0') &&
				  (request->billing_profile.state[0] == '\0') &&
				  (request->billing_profile.zipcode[0] == '\0') &&
				  (request->billing_profile.country[0] == '\0') &&
				  (request->billing_profile.phone[0] == '\0') &&
				  (request->billing_profile.email[0] == '\0') &&
				  (request->billing_profile.fax[0] == '\0'))
			{
				/*
				** copy information over from contact profile
				*/
			 	strcpy (request->billing_profile.organization,
			 	        request->contact_profile.organization);
				strcpy (request->billing_profile.street, 
				        request->contact_profile.street); 
				strcpy (request->billing_profile.city,
				        request->contact_profile.city);
				strcpy (request->billing_profile.state,
				        request->contact_profile.state);
				strcpy (request->billing_profile.zipcode,
				        request->contact_profile.zipcode);
				strcpy (request->billing_profile.country,
				        request->contact_profile.country);
				strcpy (request->billing_profile.phone,
				        request->contact_profile.phone);
				strcpy (request->billing_profile.email,
				        request->contact_profile.email);
				strcpy (request->billing_profile.fax,
				        request->contact_profile.fax);
			}
			else if ((request->billing_profile.organization[0] != '\0') &&
				       (request->billing_profile.street[0] != '\0') &&
				       (request->billing_profile.city[0] != '\0') &&
				       (request->billing_profile.state[0] != '\0') &&
				       (request->billing_profile.zipcode[0] != '\0') &&
				       (request->billing_profile.country[0] != '\0') &&
				       (request->billing_profile.phone[0] != '\0') &&
				       (request->billing_profile.email[0] != '\0') &&
				       (request->billing_profile.fax[0] != '\0'))
			{
				/* NO OP */
			}

			/*
			** the other cases, i.e. partially filled billing profile, is considered
			** user error.
			*/
			else
			{
				errbuf[0] = '\0';
				v0_errbuf[0] = '\0';
		
				(void) sprintf (v0_errbuf, 
					"Invalid PRODUCT REQUEST, request_id = %s. "
					"Missing organization, street address, city, state, zipcode, "
					"country, phone, email, or fax number in BILLING ADDRESS.",
		 	         request->order_request_id);
				(void) sprintf (errbuf, "check_existance_req: %s", v0_errbuf); 
		
				if ((status =v0_order__report_order_error 
										(msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
						< IMS_OK)
				{
					return (status);
				}
				else 
				{
					return (IMS_ERROR);
				}
		
			}
		}

		/*
		** verify if any account_id was specified.
		** Only one single account_id is expected for each PRODUCT REQUEST.
		*/
		count = 0;
		temp_account[0] = '\0';
		
		line_item  = request->line_item_list;
		
		while (line_item != (V0_LINE_ITEM_LIST *)NULL)
		{
			/* 
			** collect account_id
			*/
			if ( (strcmp(line_item->account_id, temp_account) != 0) && 
			 		(line_item->account_id[0] != '\0'))
			{
				strcpy (temp_account, line_item->account_id);
				count++;	
			}
		
			line_item = line_item->next_p;
		}
	
		if ( (count >= 2) || (count == 0))
		{
			errbuf[0] = '\0';
			v0_errbuf[0] = '\0';
	
			(void) sprintf (v0_errbuf, 
				"Invalid PRODUCT REQUEST, request_id = %s. "
				"Missing account_id or multiple account ids received. "
				"Only one account_id is expected per order.", 
	           		request->order_request_id);
			(void) sprintf (errbuf, "check_existance_req: %s", v0_errbuf);
		
			if ((status=v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
						< IMS_OK)
			{
				return (status);
			}
			else 
			{
				return (IMS_ERROR);
			}
		
		}

		(void) strcpy (request->account_id, temp_account);

	} 

	return (IMS_OK);

}/* end of check_existance_req */


/*************************************************************************
**
** check_user_req -
**
** purpose: user eligibility check.  Make sure the user and the account info 
**          provided are matching
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int check_user_req (IMS_MSG_STRUCT *msgDesc, 
                               V0_DESC_STRUCT *v0Desc, 
                               int callerFlag)
{
	V0_REQUEST_STRUCT *request;
	V0_CAT_STRUCT     *catReq;
	int    status;
	char   errbuf[IMS_COL255_LEN+1];      
	char   v0_errbuf[IMS_COL255_LEN+1];  
	char   msgbuf[IMS_COL1024_LEN+1];   
	int    rowCount;
	int    crypt_flag;
	int    order_p = 0;       /* set to 1 if it's a PRODUCT REQUEST */
	int    scan_p = 0;        /* set to 1 if it's a SCAN REQUEST */
	int    dar_acct_p = 0;    /* set to 1 if it's a USER ACCT SEARCH */
	int    dar_req_p = 0;     /* set to 1 if it's a DAR REQUEST */
	V0_VALUE_LIST     *hv, *tv;
	IMS_QI_DESC_OBJ *qDesc;
	char   temp1[IMS_COL20_LEN+1], temp2[IMS_COL20_LEN+1];
	int    error, count;

	request  =  &v0Desc->request;
	catReq   =  &v0Desc->catReq;
	qDesc    =  catReq->qDesc;

	if ( v0Desc->RxType == V0_PRODUCT_REQUEST )
	{
		order_p = 1;
	}
	else if ( v0Desc->RxType == V0_SCAN_REQUEST )
	{
		scan_p = 1;
	}
	else if ( v0Desc->RxType == V0_DAR_ACCT_SEARCH )
	{
		dar_acct_p = 1;
	}
	else if ( v0Desc->RxType == V0_DAR_REQUEST )
	{
		dar_req_p = 1;
	}

	/* 
	** firstname, lastname, and authenticator are required for 
	** PRODUCT REQUESTs, SCAN REQUESTs, USER ACCT REQUESTs and 
	** subsystem DAR REQUESTs 
	*/
	if ( (order_p) || 
			 (scan_p) || 
			 (dar_acct_p) ||
			 ((dar_req_p ==1) && (callerFlag == -1)))
	{

		v0Desc->query.sql[0] = '\0';

		if ( (scan_p) || 
				 (dar_acct_p) || 
				 (dar_req_p))
		{
			(void)sprintf (v0Desc->query.sql, 
				"exec v0_verify_user_key '%s', '%s', '%s'", 
		          	request->user_info.first_name,
								request->user_info.last_name,
							request->authenticator);
		}
		else if (order_p)
		{
			(void)sprintf (v0Desc->query.sql, 
				"exec v0_verify_user_key '%s', '%s', '%s'", 
		          	request->contact_profile.first_name,
								request->contact_profile.last_name,
								request->authenticator);
		}
	
		/* item[0] points to the sql statement */
		catReq->item[0] = (void *)v0Desc->query.sql;

		rowCount = 0;
		catReq->item[1] = (int *)&rowCount;
			
#ifdef QDEBUG
		sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
		(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

		if ( (status = v0_cat (catReq, V0_VERIFYUSER) ) < IMS_OK)
		{
			return (status);
		}

		hv = (V0_VALUE_LIST *)catReq->item[2];

		if ((rowCount == 0) || (rowCount >= 2))
		{
			while (hv != (V0_VALUE_LIST *)NULL)
			{
				tv = hv->next_p;
				free (hv);
				hv = tv;
			}

			errbuf[0] = '\0';
			v0_errbuf[0] = '\0';
	
			if (scan_p)
			{
				{
					(void) sprintf (errbuf, 
						"check_user_req: Invalid SCAN REQUEST. "
						"User %s %s is either not registered or registered "
						"more than once with ASF.", 
             	request->user_info.first_name,
             	request->user_info.last_name);
					(void) ims_msg (msgDesc, IMS_ERROR, errbuf);
					return (IMS_ERROR);
				}
			}
	
			else if ((dar_acct_p) || (dar_req_p))
			{
				(void) sprintf (v0_errbuf, 
						"Redundently or never registered. Please contact ASF.");

				(void) sprintf (errbuf, 
						"check_user_req: found none or more then one "
						"user named %s %s with same Access Key.", 
		        		request->user_info.first_name, 
								request->user_info.last_name);
				(void) ims_msg (msgDesc, IMS_ERROR, errbuf);
			
				/* 
				** need status_code_comment for USER_ACCT_SEARCH because this 
				** is a user error 
				*/
				if (callerFlag == 1)
				{
					/* construct status_code_comment */
						
					if ( (v0Desc->result.odl_status_code_comment =
		 			(V0_ERR_LIST *)malloc(sizeof(V0_ERR_LIST))) == NULL)
					{
						(void) ims_msg (msgDesc, IMS_FATAL, 
		    			"check_user_req: Memory allocation failed for odl_status_code_comment.");
						(void) strcpy (v0Desc->odl_status, "03");
						return(IMS_FATAL);
					}
			
					(void) strcpy (v0Desc->odl_status, "02");
					v0Desc->result.odl_status_code_comment->err_buf[0] = '\0';
					(void) strcpy (v0Desc->result.odl_status_code_comment->err_buf,v0_errbuf);
					v0Desc->result.odl_status_code_comment->next_p = (V0_ERR_LIST *)NULL;
					return (IMS_ERROR);
				}

				return (IMS_ERROR);
			}

			else if (order_p)
			{
	
				if (callerFlag == 1)
				{
				(void) sprintf (v0_errbuf, 
						"Invalid PRODUCT REQUEST, request_id = %s. "
						"The combination of name %s %s %s and restricted access "
						"key was invalid.  Please correct these values "
						"at the User Profile, and resubmit the order.",
             	request->order_request_id,
             	request->contact_profile.first_name,
							request->contact_profile.initial_name,
             	request->contact_profile.last_name);
				}
				else if (callerFlag == -1)
				{
				(void) sprintf (errbuf, 
						"check_user_req: Invalid PRODUCT REQUEST, request_id = %s. "
						"User name %s %s %s duplicate or not found, or invalid authenticator",
             	request->order_request_id,
             	request->contact_profile.first_name,
							request->contact_profile.initial_name,
             	request->contact_profile.last_name);
				}

				if ( (status = v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
					< IMS_OK)
				{
					return (status);
				}
				else 
				{
					return (IMS_ERROR);
				}
			}
		} /* error handling for user name and authenticator matching */
	
		sprintf (request->user_id, "%s",hv->char_value1);
		request->priority  = hv->smallint_value;
	
		while (hv != (V0_VALUE_LIST *)NULL)
		{
			tv = hv->next_p;
			free (hv);
			hv = tv;
		}

	}  
	
	/* 
	** fistname, lastname and user_id are required for 
	** V0 DAR REQUESTs 
	*/
	else if ( (dar_req_p) && (callerFlag == 1))
	{
		v0Desc->query.sql[0] = '\0';

		(void)sprintf (v0Desc->query.sql, 
			"select user_id from user_profile where "
			"upper(first_name) = '%s' and "
			"upper(last_name) = '%s' and user_id = '%s'", 
		        request->user_info.first_name,
		        request->user_info.last_name,
		        request->user_id);

		/* item[0] points to the sql statement */
		catReq->item[0] = (void *)v0Desc->query.sql;

		rowCount = 0;
		catReq->item[1] = (int *)&rowCount;
			
#ifdef QDEBUG
		sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql); 
		(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

		if ( (status = v0_cat (catReq, V0_GETSTR) ) < IMS_OK)
		{
			return (status);
		}

		hv = (V0_VALUE_LIST *)catReq->item[2];

		while (hv != (V0_VALUE_LIST *)NULL)
		{
			tv = hv->next_p;
			free (hv);
			hv = tv;
		}

		if ( (rowCount == 0) || (rowCount >= 2) )
		{
			/*
			** this is consider a system error (inconsistancy). These are information
			** that's been returned by USER_ACCT_RESULT previously.
			*/
			(void) ims_msg (msgDesc, IMS_FATAL, 
			   "check_user_req: None or multiple matching user_id found "
				 "for current DAR request.");
			(void) strcpy (v0Desc->odl_status, "03");
			return (IMS_FATAL);
		}
	} 

	/*
	** Finally, we'll check if the user is authorized to use the account_id
	** for PRODUCT_REQUEST, and SCAN_REQUEST
	*/
	
	if ( (order_p) || (scan_p))
	{
		/* set crypt_flag to indicate the authenticator is encripted */
		crypt_flag = 1;
		
		if ( (status = ims_validUser (qDesc, msgDesc, request->user_id, 
                         	request->authenticator, crypt_flag,
													request->account_id)) < IMS_OK)
		{
			errbuf[0] = '\0';
			v0_errbuf[0] = '\0';
	
			if (scan_p)  
			{
				(void) sprintf (errbuf, 
				   "check_user_req: Invalid SCAN REQUEST. "
				   "Account %s was not assigned to user %s %s or may be expired.", 
							request->account_id,
             	request->user_info.first_name,
             	request->user_info.last_name);
			}

			else
			{
				(void) sprintf (v0_errbuf, 
				    "Invalid PRODUCT REQUEST, request_id = %s. "
				    "Account %s was not assigned to user %s %s %s or may be expired. "
				    "Please contact ASF user service.", 
             	request->order_request_id,
							request->account_id,
             	request->contact_profile.first_name,
							request->contact_profile.initial_name,
             	request->contact_profile.last_name);
	
				(void) sprintf (errbuf, "check_user_req: %s", v0_errbuf);
			}
	
			if ( (status = v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
						< IMS_OK)
			{
				return (status);
			}
			else 
			{
				return (IMS_ERROR);
			}
		}
	}

	return (IMS_OK);

}/* end of check_user_req */

/*************************************************************************
**
** check_line_keyword -
**
** purpose: loop thru line items and check if keywords are valid, and if 
**          the combination of certain keyword is allowed in the system.
**          Also collect any other required information for the order line
**          from the catalog.
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int check_line_keyword (IMS_MSG_STRUCT *msgDesc, 
                               V0_DESC_STRUCT *v0Desc, 
                               int callerFlag)
{
	V0_REQUEST_STRUCT *request;
	V0_CAT_STRUCT     *catReq;
	int    status;
	char   errbuf[IMS_COL255_LEN+1];      
	char   v0_errbuf[IMS_COL255_LEN+1];  
	char   temp_errbuf[IMS_COL255_LEN+1];  
	char   msgbuf[IMS_COL1024_LEN+1];   
	char   media_id_type_name[IMS_COL40_LEN+1];
	char   granules_table[IMS_COL30_LEN+1];
	int    dataset_list[1];
	int    oagdr; 
	int    rowCount, count;
	float  total_cost;
	char   *query;
	int    order_p = 0;       /* set to 1 if it's a PRODUCT REQUEST */
	int    scan_p = 0;        /* set to 1 if it's a SCAN REQUEST */
	int    dar_req_p = 0;     /* set to 1 if it's a DAR REQUEST */
	IMS_QI_DESC_OBJ *qDesc;
	V0_LINE_ITEM_LIST *line_item;
	V0_VALUE_LIST            *hv, *tv;
	V0_ACCOUNT_INFO_LIST     *ha, *ta;
	V0_DS_INFO_LIST          *hd, *td;
	V0_PROC_MEDIA_INFO_LIST  *hp, *tp;
	V0_GRANULE_INFO          *hg, *tg;

	request  =  &v0Desc->request;
	catReq   =  &v0Desc->catReq;
	qDesc    =  catReq->qDesc;

	if ( v0Desc->RxType == V0_PRODUCT_REQUEST )
	{
		order_p = 1;
	}
	else if ( v0Desc->RxType == V0_SCAN_REQUEST )
	{
		scan_p = 1;
	}
	else if ( v0Desc->RxType == V0_DAR_REQUEST )
	{
		dar_req_p = 1;
	}

	total_cost = 0.0;
	line_item = request->line_item_list;

	/*
	** make sure the user did supply line/scan items. Skip DAR REQUEST here
	** its line item existance has been checked in range checking
	*/
	if (line_item == (V0_LINE_ITEM_LIST *)NULL)
	{
		if (order_p) 
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			    "check_line_keyword: Missing LINE_ITEM");
			return (IMS_ERROR);
		}
		else if (scan_p)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			    "check_line_keyword: Missing SCAN_ITEM");
			return (IMS_ERROR);
		}
	}

	while (line_item != (V0_LINE_ITEM_LIST *)NULL)
	{ 

		request->curr_line_item = line_item;
		
		if (dar_req_p)
		{
			line_item->order_item_type = DAR_TYPE;
			strcpy (line_item->quicklook_p, request->quicklook_p);

			if (request->sensor != (V0_VALUE_LIST *)NULL)
			{
				strcpy (line_item->sensor_name, request->sensor->char_value1);
			}

			if (request->platform != (V0_VALUE_LIST *)NULL)
			{
				strcpy (line_item->platform, request->platform->char_value1);
			}

			/* make sure the user is entitled to use the account and the account is
			 	good for order at least one dataset in the catalog */
			v0Desc->query.sql[0] = '\0';

			(void)sprintf (v0Desc->query.sql, 
			    "exec v0_verify_user_account '%s', '%s'", 
							request->account_id,
							request->user_id);

			/* item[0] points to the sql statement */
			catReq->item[0] = (void *)v0Desc->query.sql;

			rowCount = 0;
			catReq->item[1] = (int *)&rowCount;
		
#ifdef QDEBUG
			sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
			(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

			if ( (status = v0_cat (catReq, V0_VERIFYVAL) ) < IMS_OK)
			{
				errbuf[0]= '\0';
				(void) sprintf (errbuf, 
					"check_line_keyword: user %s %s was not assigned with order "
					"capability with account_id %s.", 
					 request->user_info.first_name,
					 request->user_info.last_name,
			     request->account_id);
				(void)ims_msg (msgDesc, status, errbuf);
				return (status);
			}

			/*
			** retrieve op_validate_p flag
			*/
			v0Desc->query.sql[0] = '\0';

			(void)sprintf (v0Desc->query.sql, 
		      "select op_validate_p from account where account_id = '%s'", 
							request->account_id);

			/* item[0] points to the sql statement */
			catReq->item[0] = (void *)v0Desc->query.sql;

			rowCount = 0;
			catReq->item[1] = (int *)&rowCount;
		
#ifdef QDEBUG
			sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
			(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

			if ( (status = v0_cat (catReq, V0_GETSTR) ) < IMS_OK)
			{
				return (status);
			}

			hv = (V0_VALUE_LIST *)catReq->item[2];
	
			if ( (rowCount == 0) || (rowCount >= 2))
			{
				while (hv != (V0_VALUE_LIST *)NULL)
				{
					tv = hv->next_p;
					free (hv);
					hv = tv;
				}

				(void)ims_msg (msgDesc, IMS_FATAL, 
				     "check_line_keyword: found none or more then one "
				     "op_validate_p for %s", request->account_id);

				(void) strcpy (v0Desc->odl_status, "03");
				return (IMS_FATAL);
			}

			strcpy(request->op_validate_p, hv->char_value1);

			while (hv != (V0_VALUE_LIST *)NULL)
			{
				tv = hv->next_p;
				free (hv);
				hv = tv;
			}

			/* 
			** retrieve pi information
			*/

			v0Desc->query.sql[0] = '\0';

			(void)sprintf (v0Desc->query.sql, 
			    "select u.first_name, u.last_name, u.organization from"
					" user_profile u, account a where a.account_id = '%s'"
					" and u.user_id = a.mgr_user_id", 
							request->account_id);

			/* item[0] points to the sql statement */
			catReq->item[0] = (void *)v0Desc->query.sql;

			rowCount = 0;
			catReq->item[1] = (int *)&rowCount;

#ifdef QDEBUG
			sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
			(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

			if ( (status = v0_cat (catReq, V0_GETSTR3) ) < IMS_OK)
			{
				return (status);
			}

			hv = (V0_VALUE_LIST *)catReq->item[2];

			/* it's allowed to have no pi mapping for DARs */
			if ( rowCount >= 2)
			{
				while (hv != (V0_VALUE_LIST *)NULL)
				{
					tv = hv->next_p;
					free (hv);
					hv = tv;
				}

				(void)ims_msg (msgDesc, IMS_FATAL, 
				     "check_line_keyword: found multiple matching acct "
				     "manager info for account %s in account table.", 
				 	        request->account_id);
				(void) strcpy (v0Desc->odl_status, "03");
				return (IMS_FATAL);
			}

			else if (rowCount == 1)
			{
				(void) sprintf (line_item->pi_name, "%s %s", 
		                	hv->char_value1,
											hv->char_value2);
				(void) strcpy (line_item->pi_discipline, hv->char_value3);
	
	
				while (hv != (V0_VALUE_LIST *)NULL)
				{
					tv = hv->next_p;
					free (hv);
					hv = tv;
				}
			}
		
		} /* end collect misc line item info for DAR_REQUEST */

		else if ( (order_p) || (scan_p))
		{

			/*
			** check if any other required field is missing
			*/
			if ( ((scan_p) &&
			  		((line_item->dataset_id[0] == '\0') ||
	       		(line_item->granule_id[0] == '\0') ||
						(line_item->platform[0] == '0')) )
			 		||
	     		((order_p) && 
	      		((line_item->dataset_id[0] == '\0') ||
		     		(line_item->granule_id[0] == '\0') ||
		     		(line_item->v0_process_type[0] =='\0') ||
		     		(line_item->v0_media_type[0] == '\0') ||
		     		(line_item->v0_media_fmt_type[0] == '\0'))))
			{
				errbuf[0] = '\0';
				v0_errbuf[0] = '\0';
	
				if (scan_p)
				{
					(void) sprintf (errbuf, 
					    "check_line_keyword: Invalid SCAN REQUEST. "
					    "Missing SCAN ITEM information.");
				}
				else
				{
					(void) sprintf (v0_errbuf, 
				     	"Invalid PRODUCT REQUEST, request_id = %s. "
					    "Missing LINE ITEM information.", 
            			request->order_request_id);
					(void) sprintf (errbuf, "check_line_keyword: %s", v0_errbuf);
				}
	
				if ( (status = v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
							< IMS_OK)
				{
					return (status);
				}
				else 
				{
					return (IMS_ERROR);
				}
	
			}

			/*
			** collect dataset related info for the current line_item 
			*/
			if (order_p)
			{
				v0Desc->query.sql[0] = '\0';
	
				(void)sprintf (v0Desc->query.sql, 
				"exec v0_get_ds_info '%s'", line_item->dataset_id);
	
	
				/* item[0] points to the sql statement */
				catReq->item[0] = (void *)v0Desc->query.sql;
				rowCount = 0;
				catReq->item[1] = (int *)&rowCount;
	
#ifdef QDEBUG
				sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
				(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
		
				if ( (status = v0_cat (catReq, V0_GETDSINFO) ) < IMS_OK)
				{
					return(status);
				}

				hd = (V0_DS_INFO_LIST *)catReq->item[2];
	
				if ((rowCount == 0) || (rowCount >= 2))
				{
					while (hd != (V0_DS_INFO_LIST *)NULL)
					{
						td = hd->next_p;
						free (hd);
						hd = td;
					}
	
					errbuf[0] = '\0';
					v0_errbuf[0] = '\0';
	
					(void) sprintf (v0_errbuf, 
					"Invalid PRODUCT REQUEST, request_id = %s. "
					"Found none or multiple matching dataset info for "
					"dataset %s in dataset_relation or dataset_policy table. "
					"Please contact ASF user service.", 
		       		request->order_request_id,
		       		line_item->dataset_id);

					(void) sprintf (errbuf, "check_line_keyword: %s", v0_errbuf);
	
					if ( (status = v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
						< IMS_OK)
					{
						return (status);
					}
					else 
					{
						return (IMS_ERROR);
					}
				}	

				line_item->order_item_type = hd->order_item_type;
				line_item->dataset_idx = hd->ds_idx;
				granules_table[0] = '\0';
				strcpy (granules_table, hd->granules_table);
	
				while (hd != (V0_DS_INFO_LIST *)NULL)
				{
					td = hd->next_p;
					free (hd);
					hd = td;
				}
			}
		
			else if (scan_p)
			{
				line_item->order_item_type = TSR_TYPE;

				v0Desc->query.sql[0] = '\0';
	
				(void)sprintf (v0Desc->query.sql, 
				    "select dr.dataset_idx, dp.granules_table from  "
				    "dataset_policy dp, dataset_relation dr where " 
				    "dr.dataset = '%s' and dr.platform = '%s' and "
				    "dp.dataset_idx = dr.dataset_idx ", 
				          line_item->dataset_id,
				          line_item->platform);


				/* item[0] points to the sql statement */
				catReq->item[0] = (void *)v0Desc->query.sql;
				rowCount = 0;
				catReq->item[1] = (int *)&rowCount;

#ifdef QDEBUG
				sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
				(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

				if ( (status = v0_cat (catReq, V0_GETPROCINFO) ) < IMS_OK)
				{
					return(status);	
				}
	
				hv = (V0_VALUE_LIST *)catReq->item[2];
	
				if ( (rowCount == 0) || (rowCount >= 2))
				{
					while (hv != (V0_VALUE_LIST *)NULL)
					{
						tv = hv->next_p;
						free (hv);
						hv = tv;
					}
	
					(void) ims_msg (msgDesc, IMS_ERROR, 
					     "check_line_keyword: Invalid SCAN REQUEST. "
					     "Found none or multiple matching dataset info "
					     "for dataset %s, platform %s in dataset_relation or "
				             "dataset_policy. Please contact ASF user service.", 
		       		        line_item->dataset_id, line_item->platform);
	
					return (IMS_ERROR);
				}

				line_item->dataset_idx = hv->smallint_value;
				granules_table[0] = '\0';
				strcpy (granules_table, hv->char_value1);
	
				while (hv != (V0_VALUE_LIST *)NULL)
				{
					tv = hv->next_p;
					free (hv);
					hv = tv;
				}
		
			}
			
			/*
			** verify if the user is authorize to order from the dataset for
			** both scan_p and order_p
			*/
			dataset_list[0] = line_item->dataset_idx;

			if ( (status = ims_validAcct (qDesc, msgDesc, request->account_id,
	                     	(void *)&dataset_list[0], 1, 16)) <IMS_OK)
			{
				errbuf[0] = '\0';
				v0_errbuf[0] = '\0';
	
				if (scan_p)
				{
					(void) sprintf (errbuf, 
					    "Invalid SCAN REQUEST. Invalid account id %s, "
					    "or dataset %s is not orderable by %s.", 
				            request->account_id,
		             	            line_item->dataset_id, request->account_id);
				}
				else if (order_p)
				{
					(void) sprintf (v0_errbuf, 
				      "Invalid PRODUCT REQUEST, request_id = %s. "
					    "Invalid account id %s, or dataset %s is not orderable "
					    "by %s. Please contact ASF user service.", 
		             	request->order_request_id, request->account_id,
		             	line_item->dataset_id, request->account_id);

					(void) sprintf (errbuf, "check_line_keyword: %s", v0_errbuf);
				}
	
				if ( (status = v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
						< IMS_OK)
				{
					return (status);
				}
				else 
				{
					return (IMS_ERROR);
				}
			}

			/*
			** user profile extension - check if the account_id provided
			** in this order is eligible for ordering the correct combination
			** of PLATFORM, SENSOR, DATASET, MODE
			**
			** needed only for PRODUCT REQUEST
			*/
			if (order_p)	
			{
				line_item->platform_alias[0]  = '\0';
				line_item->sensor_alias[0]    = '\0';
				line_item->mode[0]            = '\0';
				v0Desc->query.sql[0]          = '\0';
		
				/* some granule tables don't have MODE  
                                ** so we need to construct the "select"
				** statement accordingly */

				/*
				** First, check the keyword_policy to see if
				** if MODE is a keyword for this dataset
				*/ 
				sprintf(v0Desc->query.sql,
					"select p.keyword "
					"from keyword_set s, keyword_policy p "
					"where s.dataset_idx = %d "
					"and s.keyword_idx = p.keyword_idx "
					"and p.keyword = 'MODE'", line_item->dataset_idx);	

				/* item[0] points to the sql statement */
				catReq->item[0] = (void *)v0Desc->query.sql;
				rowCount = 0;
				catReq->item[1] = (int *)&rowCount;
#ifdef QDEBUG
				sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
				(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
                        	if ((status = v0_cat (catReq, V0_VERIFYENTRYCHAR)) < IMS_OK)
                        	{
					(void) ims_msg (msgDesc, status,
						"Failed to check if MODE is a keyword "
						"for dataset id %d", line_item->dataset_idx);
                               		 return (status);
                        	}
 
                        	if (rowCount > 0)  /* MODE is a valid keyword */
                        	{

					sprintf(v0Desc->query.sql, 
						"select distinct p.acronym, s.acronym, g.MODE "
						"from platforms p, sensors s, %s g,"
						"dataset_relation d "
						"where d.platform = p.platform and "
						"d.sensor = s.sensor and "
						"d.dataset_idx = %d and "
						"g.name = '%s'",
						granules_table, line_item->dataset_idx,
						line_item->granule_id);
				}
				else
				{
					sprintf(v0Desc->query.sql,
                                                "select distinct p.acronym, s.acronym, '' "
                                                "from platforms p, sensors s, "
                                                "dataset_relation d "
                                                "where d.platform = p.platform and "
                                                "d.sensor = s.sensor and "
                                                "d.dataset_idx = %d ",
                                                line_item->dataset_idx);
				}
				
				/* item[0] points to the sql statement */
				catReq->item[0] = (void *)v0Desc->query.sql;
				rowCount = 0;
				catReq->item[1] = (int *)&rowCount;
	
#ifdef QDEBUG
				sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
				(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
		
				if ( (status = v0_cat (catReq, V0_GETSTR3) ) < IMS_OK)
				{
					return(status);
				}

				hv = (V0_VALUE_LIST *)catReq->item[2];
	
				if ( (rowCount == 0) || (rowCount >= 2))
				{
					while (hv != (V0_VALUE_LIST *)NULL)
					{
						tv = hv->next_p;
						free (hv);
						hv = tv;
					}
                                        (void) sprintf(v0_errbuf,"Invalid PRODUCT REQUEST. "
                                                "Found none or multiple matching PLATFORM,"
                                                "SENSOR,MODE values for granule id %s in"
                                                " table %s. Please contact ASF user service.",
                                                line_item->granule_id, granules_table);
                                        (void) sprintf (errbuf,
                                           "check_line_keyword: %s", v0_errbuf);
 
                                        if ((status =v0_order__report_order_error (msgDesc,
                                                v0Desc, callerFlag, errbuf, v0_errbuf))
                                                                < IMS_OK)
                                        {
                                                return (status);
                                        }
                                        else
                                        {
                                                return (IMS_ERROR);
                                        }
	
				}

				strcpy (line_item->platform_alias, hv->char_value1);
				strcpy (line_item->sensor_alias, hv->char_value2);
				strcpy (line_item->mode, hv->char_value3);

				while (hv != (V0_VALUE_LIST *)NULL)
				{
					tv = hv->next_p;
					free (hv);
					hv = tv;
				}

				v0Desc->query.sql[0] = '\0';

				(void)sprintf (v0Desc->query.sql, 
			    "exec v0_validate_account_ext '%s', '%s', '%s', '%s', '%s'", 
				request->account_id, 
				line_item->platform_alias, 
				line_item->sensor_alias,
				line_item->dataset_id, 
				line_item->mode);

				catReq->item[0] = (void *)v0Desc->query.sql;

				rowCount = 0;
				catReq->item[1] = (int *)&rowCount;
		
#ifdef QDEBUG
				sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
				(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

				if ( (status = v0_cat (catReq, V0_VERIFYVAL) ) < IMS_OK)
				{
					errbuf[0] = '\0';
					v0_errbuf[0] = '\0';

					(void) sprintf (v0_errbuf, 
						"account %s is not eligible to order dataset %s in %s MODE",
	          		request->account_id, line_item->dataset_id, 
				line_item->mode );

					(void) sprintf (errbuf, 
					   "check_line_keyword: %s", v0_errbuf);

					if ((status =v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
								< IMS_OK)
					{
						return (status);
					}
					else 
					{
						return (IMS_ERROR);
					}
				}

			} /* user profile extension */

			/*
			** collect internal values for process_type, media_type, media_class
			** media_fmt_type specified from user, this is needed for PRODUCT REQUEST
			** only
			**
			** also, check if this dataset_idx is available for ordering 
			*/
		
			if (order_p)
			{
			
				/* check oagdr */
				v0Desc->query.sql[0] = '\0';

				(void)sprintf (v0Desc->query.sql, 
				    "select oagdr from dataset_policy where dataset_idx = %d", 
						 		line_item->dataset_idx);
	
				/* item[0] points to the sql statement */
				catReq->item[0] = (void *)v0Desc->query.sql;
				rowCount = 0;
				catReq->item[1] = (int *)&rowCount;
	
#ifdef QDEBUG
				sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
				(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
		
				if ( (status = v0_cat (catReq, V0_GETTINYINT) ) < IMS_OK)
				{
					return (status);
				}

				hv = (V0_VALUE_LIST *)catReq->item[2];

				if ( (rowCount == 0) || (rowCount >= 2) )
				{
					while (hv != (V0_VALUE_LIST *)NULL)
					{
						tv = hv->next_p;
						free (hv);
						hv = tv;
					}

					errbuf[0] = '\0';
					v0_errbuf[0] = '\0';
					temp_errbuf[0] = '\0';
		
					(void) sprintf (temp_errbuf, 
					    "Database internal error, found none or "
					    "multiple matching oagdr for dataset_idx = %d "
					    "in dataset_policy table.", 
						        line_item->dataset_idx);

				 	(void) sprintf (v0_errbuf, 
						  "%s Please contact ASF user service.", 
							      temp_errbuf);

					(void) sprintf (errbuf, 
							"%s Please contact IMS staff.", 
							temp_errbuf);
		
					if ( (status = v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
							< IMS_OK)
					{
						return (status);
					}
					else 
					{
						return (IMS_ERROR);
					}
				}

				oagdr = 0;
				oagdr = hv->tinyint_value;
		
				while (hv != (V0_VALUE_LIST *)NULL)
				{
					tv = hv->next_p;
					free (hv);
					hv = tv;
				}

				if ( (oagdr & orderMask) != orderMask )
				{
					errbuf[0] = '\0';
					v0_errbuf[0] = '\0';
	
					(void) sprintf(v0_errbuf, 
						 "Dataset %s is not available for ordering.",
				            line_item->dataset_id);
					(void) sprintf (errbuf, "check_line_keyword: %s", v0_errbuf);
		
					if ( (status = v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
							< IMS_OK)
					{
						return (status);
					}
					else 
					{
						return (IMS_ERROR);
					}
				
				}

				/* find process_type, media_type, media_fmt_type */
				v0Desc->query.sql[0] = '\0';

				(void)sprintf (v0Desc->query.sql, 
				   "exec v0_get_proc_media_info %d, '%s','%s', '%s', '%s'", 
			          line_item->dataset_idx,
								line_item->dataset_id,
						 		line_item->v0_process_type,
						 		line_item->v0_media_type,
						 		line_item->v0_media_fmt_type);
	
				/* item[0] points to the sql statement */
				catReq->item[0] = (void *)v0Desc->query.sql;
				rowCount = 0;
				catReq->item[1] = (int *)&rowCount;
			
#ifdef QDEBUG
				sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
				(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
			
				if ( (status = v0_cat (catReq, V0_GETPROCMEDIAINFO) ) < IMS_OK)
				{
					return (status);
				}
	
				hp = (V0_PROC_MEDIA_INFO_LIST *)catReq->item[2];
	
				if ( (rowCount == 0) || (rowCount >= 2))
				{
					while (hp != (V0_PROC_MEDIA_INFO_LIST *)NULL)
					{
						tp = hp->next_p;
						free (hp);
						hp = tp;
					}
	
					errbuf[0] = '\0';
					v0_errbuf[0] = '\0';
	
					(void) sprintf (v0_errbuf, 
					   "Invalid PRODUCT REQUEST, request_id = %s. "
					   "Failed to locate internal values for process_type %s, "
					   "media_type, or media_format %s. "
						 "Please contact ASF user service.", 
		         		request->order_request_id,
		         		line_item->v0_process_type,
						 		line_item->v0_media_type,
						 		line_item->v0_media_fmt_type);
					(void) sprintf (errbuf, "check_line_keyword: %s", v0_errbuf);
		
					if ( (status = v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
							< IMS_OK)
					{
						return (status);
					}
					else 
					{
						return (IMS_ERROR);
					}
				}

				line_item->process_type = hp->process_type;
				line_item->media_type = hp->media_type;
				line_item->media_class = hp->media_class;
				line_item->media_fmt_type = hp->media_fmt_type;
				strcpy (line_item->quicklook_p, hp->quicklook_p);

				while (hp != (V0_PROC_MEDIA_INFO_LIST *)NULL)
				{
					tp = hp->next_p;
					free (hp);
					hp = tp;
				}
	
			} /* if order_p */

			/*
			** collect account related information for both scan_p and order_p
			*/
	
			v0Desc->query.sql[0] = '\0';
	
			(void)sprintf (v0Desc->query.sql, 
			      "exec v0_get_account_info '%s'", request->account_id);

			/* item[0] points to the sql statement */
			catReq->item[0] = (void *)v0Desc->query.sql;
			rowCount = 0;
			catReq->item[1] = (int *)&rowCount;
	
#ifdef QDEBUG
			sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
			(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
		
			if ( (status = v0_cat (catReq, V0_GETACCTINFO) ) < IMS_OK)
			{
				return (status);
			}

			ha = (V0_ACCOUNT_INFO_LIST *)catReq->item[2];

			if ( (rowCount == 0) || (rowCount >= 2))
			{
				while (ha != (V0_ACCOUNT_INFO_LIST *)NULL)
				{
					ta = ha->next_p;
					free (ha);
					ha = ta;
				}
	
				errbuf[0] = '\0';
				v0_errbuf[0] = '\0';
	
				if (scan_p)
				{
					(void) sprintf (v0_errbuf, 
					    "Invalid SCAN REQUEST. Failed to locate accounting "
					    "related info for account_id %s.", 
						 			request->account_id);
				}
				else
				{
					(void) sprintf (v0_errbuf, 
					    "Invalid PRODUCT REQUEST, request_id = %s. "
					    "Failed to locate accounting related info for "
					    "account_id %s. Please contact ASF user service.", 
		         		request->order_request_id,
						 		request->account_id);
					(void) sprintf (errbuf, "check_line_keyword: %s", v0_errbuf);
				}
	
				if ((status = v0_order__report_order_error(msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
						< IMS_OK)
				{
				return (status);
				}
				else 
				{
					return (IMS_ERROR);
				}
			}	
			
			request->resource_type = ha->resource_type;
			request->rate_multiplier = ha->rate_multiplier;
			strcpy (request->op_validate_p, ha->op_validate_p);

			while (ha != (V0_ACCOUNT_INFO_LIST *)NULL)
			{
				ta = ha->next_p;
				free (ha);
				ha = ta;
			}

			/* 
			** get platform, sensor_name for both scan_p and order_p
			*/
	
			v0Desc->query.sql[0] = '\0';
			query = v0Desc->query.sql;

			sprintf (query, 
		      "select platform, sensor from dataset_relation "
		      "where dataset_idx = %d and dataset = '%s'", 
	       	    line_item->dataset_idx, 
					    line_item->dataset_id);

			query = query + strlen(query);
	
		
			catReq->item[0] = (void *)v0Desc->query.sql;
	
			rowCount = 0;
			catReq->item[1] = (int *)&rowCount;
	
#ifdef QDEBUG
			sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
			(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
	
			if ( (status = v0_cat (catReq, V0_GETSTR2) ) < IMS_OK)
			{
				return (status);
			}

			hv = (V0_VALUE_LIST *)catReq->item[2];

			if ( (rowCount == 0) || (rowCount >= 2))
			{

				while (hv != (V0_VALUE_LIST *)NULL)
				{
					tv = hv->next_p;
					free (hv);
					hv = tv;
				}
	
				errbuf[0] = '\0';
				v0_errbuf[0] = '\0';
				temp_errbuf[0] = '\0';
		
				if (scan_p)
				{
					(void) sprintf (errbuf, 
					   "check_line_keyword: Database internal error, "
					   "found none or multiple matching platform, sensor "
					   "from dataset_relation table for dataset_idx %d dataset_id %s.", 
							line_item->dataset_idx,
							line_item->dataset_id);
				}
				else if (order_p)
				{
					(void) sprintf (temp_errbuf, 
					   "Database internal error, found none or multiple matching "
					   "platform, sensor from dataset_relation table for "
					   "dataset_idx %d dataset_id %s.", 
						     line_item->dataset_idx,
						     line_item->dataset_id);

					(void) sprintf (v0_errbuf, 
					   "%s Please contact ASF user service.", 
						     temp_errbuf);

				 	(void) sprintf (errbuf, 
					    "%s Please contact IMS staff.", 
						     temp_errbuf);
			 	}
		
				if ((status =v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
							< IMS_OK)
				{
					return (status);
				}
				else 
				{
					return (IMS_ERROR);
				}
			}	

			strcpy (line_item->platform, hv->char_value1);
			strcpy (line_item->sensor_name, hv->char_value2);

			while (hv != (V0_VALUE_LIST *)NULL)
			{
				tv = hv->next_p;
				free (hv);
				hv = tv;
			}

			/* 
			** get granule_idx for both scan_p and order_p, and get
			** data_kbytes, metadata_kbytes for APR orders
			*/
	
			v0Desc->query.sql[0] = '\0';
			query = v0Desc->query.sql;
	
			sprintf (query, "select granule_idx, data_kbytes,"
				" metadata_kbytes from %s where name = '%s' ", 
				granules_table, line_item->granule_id);

			query = query + strlen(query);
	
	
			catReq->item[0] = (void *)v0Desc->query.sql;
	
			rowCount = 0;
			catReq->item[1] = (int *)&rowCount;
		
#ifdef QDEBUG
			sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
			(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

			if ( (status = v0_cat (catReq, V0_GETGNULINFO) ) < IMS_OK)
			{
				return (status);
			}

			hg = (V0_GRANULE_INFO *)catReq->item[2];

			if ( (rowCount == 0) || (rowCount >= 2))
			{

				while (hg != (V0_GRANULE_INFO *)NULL)
				{
					tg = hg->next_p;
					free (hg);
					hg = tg;
				}

				errbuf[0] = '\0';
				v0_errbuf[0] = '\0';
				temp_errbuf[0] = '\0';

				if (scan_p)
				{
					(void) sprintf (errbuf, 
					"check_line_keyword: Database internal error, "
					"found none or multiple matching granule_idx for "
					"granule_id %s from granule table  %s.", 
					line_item->granule_id, granules_table);
				}
				else if (order_p)
				{
					(void) sprintf (temp_errbuf, 
					"Database internal error, found none or multiple matching "
					"granule_idx for granule_id %s from granule table %s.", 
					line_item->granule_id, granules_table);

					(void) sprintf (v0_errbuf, 
					   "%s Please contact ASF user service.", 
							   temp_errbuf);

					(void) sprintf (errbuf, 
				     "%s Please contact IMS staff.", 
							   temp_errbuf);
				}

				if ((status =v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
							< IMS_OK)
				{
					return (status);
				}
				else 
				{
					return (IMS_ERROR);
				}

			}

			line_item->granule_idx  = hg->granule_idx;
			line_item->data_kbytes  = hg->data_kbytes;
			line_item->metadata_kbytes  = hg->metadata_kbytes;

			while (hg != (V0_GRANULE_INFO *)NULL)
			{
				tg = hg->next_p;
				free (hg);
				hg = tg;
			}
	
			/*
			** verify whether this granule is available from IMS and it's orderable
			** Needed for PRODUCT REQUEST only
			*/
			if (order_p)
			{
				v0Desc->query.sql[0] = '\0';

				query = v0Desc->query.sql;
				sprintf (query, 
				"select count(*) from %s where granule_idx = %d and "
				"(o_gdr & %d) = %d and status = %d", 
				granules_table,
				line_item->granule_idx,
				orderMask, orderMask, IMS_AVAILABLE);

				query = query + strlen(query);
		
				catReq->item[0] = (void *)v0Desc->query.sql;
			
				rowCount = 0;
				catReq->item[1] = (int *)&rowCount;
			
#ifdef QDEBUG
				sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
				(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
			

				if ( (status = v0_cat (catReq, V0_GETINT) ) < IMS_OK)
				{
					return (status);
				}

				hv = (V0_VALUE_LIST *)catReq->item[2];

				if (rowCount == 1)
				{
					count = hv->int_value;
				}

				while (hv != (V0_VALUE_LIST *)NULL)
				{
					tv = hv->next_p;
					free (hv);
					hv = tv;
				}
	
				if (count == 0)
				{
					errbuf[0] = '\0';
					v0_errbuf[0] = '\0';
					temp_errbuf[0] = '\0';

					(void) sprintf (v0_errbuf, 
					"granule_id %s of granule table %s is either not orderable or "
					"not available at the current time.", 
					line_item->granule_id, granules_table);

					(void) sprintf (errbuf, "%s" , v0_errbuf);

					if ((status =v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
								< IMS_OK)
					{
						return (status);
					}
					else 
					{
						return (IMS_ERROR);
					}
				}
			}

			/*
			** verify media info.  Only needed for PRODUCT REQUEST 
			** required processing.  
			*/
			if (order_p) 
			{
				v0Desc->query.sql[0] = '\0';
				query = v0Desc->query.sql;

				sprintf (query, "exec v0_verify_process_media %d", 
					line_item->dataset_idx);
				query = query + strlen(query);

				if (line_item->order_item_type == APR_TYPE )
				{
					sprintf (query, ",null");
					query = query + strlen(query);
				}
				else
				{
					sprintf (query, ",%d", line_item->process_type);
					query = query + strlen(query);
				}

				if (line_item->media_class == 0)
				{
					sprintf (query, ",null");
					query = query + strlen(query);
				}
				else
				{
					sprintf (query, ",%d", line_item->media_class);
					query = query + strlen(query);
				}

				catReq->item[0] = (void *)v0Desc->query.sql;
			
				rowCount = 0;
				catReq->item[1] = (int *)&rowCount;
			
#ifdef QDEBUG
				sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
				(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
			
				if ( (status = v0_cat (catReq, V0_VERIFYVAL) ) < IMS_OK)
				{
					errbuf[0] = '\0';
					v0_errbuf[0] = '\0';
					temp_errbuf[0] = '\0';

					(void) sprintf (temp_errbuf, 
					"Database internal error, found none or multiple matching "
					"entries for dataset_idx %d media_class %d process_type %d "
					"from dataset_process_media table.", 
	          		line_item->dataset_idx,
								line_item->media_class,
								line_item->process_type);

					(void) sprintf (v0_errbuf, 
					   "%s Please contact ASF user service.", 
							   temp_errbuf);

					(void) sprintf (errbuf, 
				     "%s Please contact IMS staff.", 
							   temp_errbuf);

					if ((status =v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
								< IMS_OK)
					{
						return (status);
					}
					else 
					{
						return (IMS_ERROR);
					}
				}

			} 

			/* 
			** find out the current line_item cost 
			** only PRODUCT REQUEST get charged for now 
			*/
	
			if (order_p)
			{
				v0Desc->query.sql[0] = '\0';
				query = v0Desc->query.sql;

				sprintf (query, "exec v0_get_item_cost '%s'",
					line_item->platform);
				query = query + strlen(query);

				if (line_item->sensor_name[0] == '\0')
				{
					sprintf (query, ",null, '%s'", line_item->dataset_id);
					query = query + strlen(query);
				}
				else
				{
					sprintf (query, ",'%s', '%s'", 
						line_item->sensor_name, 
						line_item->dataset_id);
					query = query + strlen(query);
				}

				if (line_item->v0_process_type[0] == '\0')
				{
					sprintf (query, ",null, %d, %d, %d", 
						line_item->media_type,
						line_item->media_fmt_type,
						request->resource_type);
					query = query + strlen(query);
				}
				else
				{
					sprintf (query, ",'%s', %d, %d, %d", 
						line_item->v0_process_type,
						line_item->media_type,
						line_item->media_fmt_type,
						request->resource_type);
					query = query + strlen(query);
				}

				catReq->item[0] = (void *)v0Desc->query.sql;
				rowCount = 0;
				catReq->item[1] = (int *)&rowCount;
		
#ifdef QDEBUG
				sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
				(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
	
				if ( (status = v0_cat (catReq, V0_GETLINEITEMCOST) ) < IMS_OK)
				{
					return (status);
				}
	
				hv = (V0_VALUE_LIST *)catReq->item[2];
	
				if ( rowCount != 1)
				{
					while (hv != (V0_VALUE_LIST *)NULL)
					{
						tv = hv->next_p;
						free (hv);
						hv = tv;
					}
	
					errbuf[0] = '\0';
					v0_errbuf[0] = '\0';
					temp_errbuf[0] = '\0';
		
					(void) sprintf (temp_errbuf, 
					     "Database internal error, found none or multiple "
					     "matching cost info for dataset_idx %d process_type %d "
					     "resource_type %d. ", 
						line_item->dataset_idx,
						line_item->process_type,
						request->resource_type);

					(void) sprintf (v0_errbuf, 
					     "%s Please contact ASF user service.", 
						temp_errbuf);

					(void) sprintf (errbuf, 
					     "%s Please contact IMS staff.",
						temp_errbuf);
	
					if ((status =v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
							< IMS_OK)
					{
						return (status);
					}
					else 
					{
						return (IMS_ERROR);
					}
				}

				line_item->line_item_cost = hv->real_value;

				while (hv != (V0_VALUE_LIST *)NULL)
				{
					tv = hv->next_p;
					free (hv);
					hv = tv;
				}
			}
			/* free of charge for DAR and SCAN */
			else
			{
				line_item->line_item_cost = 0.0;
			}

			line_item->line_item_cost = 
				(line_item->line_item_cost) * (request->rate_multiplier);


			/* 
			** add to the total_cost 
			*/
		
			total_cost = total_cost + line_item->line_item_cost;

                        /*
                        ** For SCAN request only,
                        ** get PLATFORM, SENSOR, REVOLUTION, SEQUENCE and
                        ** MEDIA_ID_TYPE_NAME from granules_table.
                        ** The first 4 fields will be used to find the corresponding
                        ** record for this scan item in downlink_entry table.
                        ** MEDIA_ID_TYPE_NAME will be used to set the quicklook_p
                        ** for the scan item
                        */
                        if (scan_p)
                        {
                                v0Desc->query.sql[0] = '\0';
 
                                (void)sprintf (v0Desc->query.sql,
                                        "select PLATFORM, SENSOR, REVOLUTION,"
                                        "SEQUENCE, MEDIA_ID_TYPE_NAME from %s"
                                        " where name = '%s'",
                                        granules_table, line_item->granule_id);
 
                                /* item[0] points to the sql statement */
                                catReq->item[0] = (void *)v0Desc->query.sql;
                                rowCount = 0;
                                catReq->item[1] = (int *)&rowCount;
 
#ifdef QDEBUG
                                sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
                                (void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
 
                                if ( (status = v0_cat (catReq, V0_GETDOWNLINK) ) < IMS_OK)
                                {
                                        return(status);
                                }
 
                                hv = (V0_VALUE_LIST *)catReq->item[2];
 
                                if ( (rowCount == 0) || (rowCount >= 2))
                                {
                                        while (hv != (V0_VALUE_LIST *)NULL)
                                        {
                                                tv = hv->next_p;
                                                free (hv);
                                                hv = tv;
                                        }
 
                                        (void) ims_msg (msgDesc, IMS_ERROR,
                                             "check_line_keyword: Database internal error."
                                             "Found none or multiple matching PLATFORM, SENSOR, "
                                             "REVOLUTION, SEQUENCE, MEDIA_ID_TYPE_NAME "
                                             "for granule id %s in table %s. "
                                             "Please contact ASF user service.",
                                                  line_item->granule_id, granules_table);
 
                                        return (IMS_ERROR);
                                }
 
                                strcpy (line_item->platform_alias, hv->char_value1);
                                strcpy (line_item->sensor_alias, hv->char_value2);
                                strcpy (media_id_type_name, hv->char_value3);
                                line_item->revolution = hv->int_value;
                                line_item->sequence = hv->smallint_value;
 
                                while (hv != (V0_VALUE_LIST *)NULL)
                                {
                                        tv = hv->next_p;
                                        free (hv);
                                        hv = tv;
                                }
                                /* set the quicklook_p flag based on the media_id_type_name
                                ** retrieved from the granules table */
                                if (strcmp(media_id_type_name,"QUICKLOOK") == 0)
                                {
                                        strcpy (line_item->quicklook_p, "Y");
                                }
                                else
                                {
                                        strcpy (line_item->quicklook_p, "N");
                                }
                        } /* if scan_p */
 
                } /* end of collect line item info for PRODUCT_REQUEST and SCAN_REQUEST */
 
                line_item = line_item->next_p;
 
        }
 
        request->total_cost = total_cost;
 
 
        return (IMS_OK);
 
}/* end of check_line_keyword */

/*************************************************************************
**
** prepare_acct_user_result - collects information needed to create 
**       result for a USER_ACCT_SEARCH request.
** **************************************************************************/
static int prepare_user_acct_result (IMS_MSG_STRUCT *msgDesc, 
                                     V0_DESC_STRUCT *v0Desc)
{
	V0_CAT_STRUCT     *catReq;
	V0_VALUE_LIST     *hv, *tv;
	char   errbuf[IMS_COL255_LEN+1];      
	char   v0_errbuf[IMS_COL255_LEN+1];  
	char   msgbuf[IMS_COL1024_LEN+1];   
	V0_REQUEST_STRUCT *request;
	int    rowCount;
	int    status;

	request  =  &v0Desc->request;
	catReq   =  &v0Desc->catReq;

	v0Desc->query.sql[0] = '\0';

	(void)sprintf (v0Desc->query.sql, "exec v0_get_acct_platform_list '%s'", 
						request->user_id);

	/* item[0] points to the sql statement */
	catReq->item[0] = (void *)v0Desc->query.sql;

	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;
		
#ifdef QDEBUG
	sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
	(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	if ( (status = v0_cat (catReq, V0_GETSTR2) ) < IMS_OK)
	{
		return (status);
	}

	if (rowCount >= 1)
	{
		v0Desc->result.user_list = (V0_VALUE_LIST *)catReq->item[2];
	}
	else if (rowCount == 0)
	{
		(void) strcpy (v0_errbuf, "No ASF account assigned to the current user.");
		(void) sprintf (errbuf, "prepare_user_acct_result: %s. user_id = %s", 
			               v0_errbuf,
										 request->user_id);
		(void) ims_msg (msgDesc, IMS_ERROR, errbuf);

		if ( (v0Desc->result.odl_status_code_comment =
	  	(V0_ERR_LIST *)malloc(sizeof(V0_ERR_LIST))) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
		   		"prepare_user_acct_result: Memory allocation failed for odl_status_code_comment.");

			/* 03 is system error status for DARs */
			(void) strcpy (v0Desc->odl_status, "03");
			return(IMS_FATAL);
		}

		(void) strcpy (v0Desc->odl_status, "02");
		v0Desc->result.odl_status_code_comment->err_buf[0] = '\0';
		(void)strcpy(v0Desc->result.odl_status_code_comment->err_buf,v0_errbuf);
		v0Desc->result.odl_status_code_comment->next_p = (V0_ERR_LIST *)NULL;
		return (IMS_ERROR);
	}

	/* copy the user id to the result structure */
	strcpy (v0Desc->result.user_id , request->user_id);

	/* find an associating directory */
	/*
	** use user_id as directory name for now
	*/
	/*
	v0Desc->query.sql[0] = '\0';

	(void)sprintf (v0Desc->query.sql, 
		"select local_dir from user_profile where user_id = '%s'", 
						request->user_id);

	catReq->item[0] = (void *)v0Desc->query.sql;
	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;
		
#ifdef QDEBUG
	sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
	(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	if ( (status = v0_cat (catReq, V0_GETSTR) ) < IMS_OK)
	{
		return (status);
	}

	hv = (V0_VALUE_LIST *)catReq->item[2];

  if ( rowCount >= 2)
	{

		while (hv != (V0_VALUE_LIST *)NULL)
		{
			tv = hv->next_p;
			free (hv);
			hv = tv;
		}
	
		ims_msg( msgDesc, IMS_FATAL, 
			"prepare_user_acct_result: found multiple matching directory "
			"found for user id %s ", request->user_id);
		strcpy (v0Desc->odl_status, "03");
		return (IMS_FATAL);	
	}

	strcpy (v0Desc->result.directory,  hv->char_value1);

	while (hv != (V0_VALUE_LIST *)NULL)
	{
		tv = hv->next_p;
		free (hv);
		hv = tv;
	}
	*/
	
	strcpy (v0Desc->result.directory,  v0Desc->result.user_id);

	return (IMS_OK);

} /* end of prepare_user_acct_result */

/*************************************************************************
**
** insert_order - inserts order related information to the catalog.
**
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**
** Note:
**     This process has not been optimized.  Currently, the entire order 
**     insertion is processed in one single critical section.  Which means,
**     all other processes which require account_lock or v0_order_lock have
**     to wait for this critical section be completed in order to get the lock.
**     Although this is the safest and most straightforward method to handle
**     table updates, i.e. let the database do the rollover when anything goes
**     wrong during the table insertion, this may slow down the overall 
**     performance when demand for the locks gets high in the future.  
**     When that happen, we'll have to switch to a multiple critical section
**     approach such as:
** 
**     get v0_order_lock, enter critical section
**        get a new order_id 
**     exit critical section
**
**     insert info into profile tebles
**     
**     if error occur, delete rows from profile tables 
**
**     get account_lock, enter critical section
**        update account table
**        if error occur, delete rows from profile tables and rollback
**     exit critical section
**
**************************************************************************/
static int insert_order (IMS_MSG_STRUCT *msgDesc,
                         V0_DESC_STRUCT *v0Desc, 
                         int callerFlag)
{
	V0_REQUEST_STRUCT *request;
	V0_CAT_STRUCT     *catReq;
	int   status, rollback_status;
	V0_VALUE_LIST    *hv, *tv;
	char   errbuf[IMS_COL255_LEN+1];     
	char   v0_errbuf[IMS_COL255_LEN+1]; 
	char   msgbuf[IMS_COL1024_LEN+1];   
	IMS_QI_DESC_OBJ   *qDesc;
	int    order_p = 0;
	int    dar_req_p = 0;
	int    scan_p = 0;
	int    rowCount  = 0;

	request = &v0Desc->request;
	catReq  = &v0Desc->catReq;
	qDesc   =  catReq->qDesc;

	if (v0Desc->RxType == V0_PRODUCT_REQUEST)
	{
		order_p = 1;
	}
	else if (v0Desc->RxType == V0_DAR_REQUEST)
	{
		dar_req_p = 1;
	}
	else if (v0Desc->RxType == V0_SCAN_REQUEST)
	{
		scan_p = 1;
	}

	/* begin transaction */
	if ( (status = v0_cat (catReq, V0_BEGINTRANSACTION)) < IMS_OK)
	{
		return (status);
	}

#ifdef QDEBUG
	(void) ims_msg (msgDesc, IMS_INFO, "*** Began Transaction.");
#endif

	/* 
	** get v0_order_lock
	*/
	if ((status = v0_cat (catReq, V0_GETV0ORDERLOCK)) < IMS_OK)
	{
		if ( (rollback_status=v0_cat(catReq, V0_ROLLBACKTRANSACTION)) < IMS_OK)
		{
			return (rollback_status);
		}
#ifdef QDEBUG
	(void) ims_msg (msgDesc, IMS_INFO, "*** Rollbacked Transaction.");
#endif

		return (status);
	}

#ifdef QDEBUG
	(void) ims_msg (msgDesc, IMS_INFO, "*** Got v0 order lock.");
#endif

	/*
	** get a new order_id  
	*/

	v0Desc->query.sql[0] = '\0';
	(void)sprintf(v0Desc->query.sql,"exec incr_order_id"); 
	catReq->item[0] = (void *)v0Desc->query.sql;
	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;


#ifdef QDEBUG
	sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
	(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	if ( (status = v0_cat (catReq, V0_GETNEWID) ) < IMS_OK)
	{
		if ( (rollback_status=v0_cat(catReq, V0_ROLLBACKTRANSACTION)) < IMS_OK)
		{
			return (rollback_status);
		}
#ifdef QDEBUG
	(void) ims_msg (msgDesc, IMS_INFO, "*** Rollbacked Transaction.");
#endif

		return (status);
	}

	hv = (V0_VALUE_LIST *)catReq->item[2];

	if ( (rowCount == 0) || (rowCount >=2))
	{
		while (hv != (V0_VALUE_LIST *)NULL)
		{
			tv = hv->next_p;
			free (hv);
			hv = tv;
		}

		(void) ims_msg (msgDesc, IMS_FATAL, 
			 "ims_v0Order__insert_order: Failed to get a new order id. ");

		if ( (rollback_status=v0_cat(catReq, V0_ROLLBACKTRANSACTION)) < IMS_OK)
		{
			return (rollback_status);
		}
#ifdef QDEBUG
	(void) ims_msg (msgDesc, IMS_INFO, "*** Rollbacked Transaction.");
#endif

		return (IMS_FATAL);
	}

	request->order_id = hv->int_value;

	/* duplicate this value to result structre if this is a DAR 
		 or a V0 order */

	if ( (dar_req_p) || 
			 ((order_p) && (callerFlag == 1)) )
	{
		v0Desc->result.order_id = hv->int_value;
	}

	while (hv != (V0_VALUE_LIST *)NULL)
	{
		tv = hv->next_p;
		free (hv);
		hv = tv;
	}

	/* 
	** only PRODUCT REQUEST triggers accounting transactions and 
	** entry insertion into profile tables (i.e. contact_profile, billing_profile,
	** shipping_profile)
	*/
	if (order_p)
	{
		if (request->total_cost > 0.0)
		{
			if ( (status = ims_acctTran (qDesc, msgDesc, request->account_id, 
 	             request->order_id, request->total_cost, DEBIT_BEGIN)) <IMS_OK)
			{

				/* set order_id to be 0 as indication of order error */
				request->order_id = 0;
				v0Desc->result.order_id = 0;

				if ( (rollback_status=v0_cat(catReq, V0_ROLLBACKTRANSACTION)) < IMS_OK)
				{
					return (rollback_status);
				}
#ifdef QDEBUG
				(void) ims_msg (msgDesc, IMS_INFO, "*** Rollbacked Transaction.");
#endif

				errbuf[0] = '\0';
				v0_errbuf[0] = '\0';
	
				(void) sprintf (v0_errbuf, 
					"Failed to perform accounting transaction for "
					"your order. request id %s", 
	              	request->order_request_id);
	
				(void) sprintf (errbuf, "insert_order: %s", v0_errbuf);
	
				if ( (status = v0_order__report_order_error (msgDesc, v0Desc, callerFlag, errbuf, v0_errbuf))
							< IMS_OK)
				{
					return (status);
				}
				else 
				{
					return (IMS_ERROR);
				}
			} 
		}

		/*
		** insert user information into contact_profile, billing_profile, and
		** shipping_profile tables for PRODUCT REQUEST.
		*/

		if ( (status = insert_profile (msgDesc, v0Desc, callerFlag)) < IMS_OK)
		{
			/* set order_id to be 0 as indication of order error */
			request->order_id = 0;
			v0Desc->result.order_id = 0;

			if ( (rollback_status=v0_cat(catReq, V0_ROLLBACKTRANSACTION)) < IMS_OK)
			{
				return (rollback_status);
			}
#ifdef QDEBUG
		(void) ims_msg (msgDesc, IMS_INFO, "*** Rollbacked Transaction.");
#endif
	
			return (status);

		} /* insert profile */

	} /* PRODUCT REQUEST only */


	/*
	** insert line_items into order_item table one-by-one
	*/
	if ( (status = insert_order_item (msgDesc, v0Desc, callerFlag)) < IMS_OK)
	{
		/* set order_id to be 0 as indication of order error */
		request->order_id = 0;	
		v0Desc->result.order_id = 0;

		if ( (rollback_status=v0_cat(catReq, V0_ROLLBACKTRANSACTION)) < IMS_OK)
		{
			return (rollback_status);
		}
#ifdef QDEBUG
	(void) ims_msg (msgDesc, IMS_INFO, "*** Rollbacked Transaction.");
#endif

		return (status);
	}


	/*
	** insert into order_queue
	*/
	if ( (status = insert_order_queue (msgDesc, v0Desc, callerFlag)) < IMS_OK)
	{
		/* set order_id to be 0 as indication of order error */
		request->order_id = 0;
		v0Desc->result.order_id = 0;

		if ( (rollback_status=v0_cat(catReq, V0_ROLLBACKTRANSACTION)) < IMS_OK)
		{
			return (rollback_status);
		}
#ifdef QDEBUG
	(void) ims_msg (msgDesc, IMS_INFO, "*** Rollbacked Transaction.");
#endif

		return (status);
	}

	/*
	** insert values to dar table if it's a DAR REQUEST
	*/
	if (dar_req_p)
	{
		if ( (status = insert_dar (msgDesc, v0Desc, callerFlag)) < IMS_OK)
		{

			/* set order_id to be 0 as indication of order error */
			request->order_id = 0;
			v0Desc->result.order_id = 0;

			if ((rollback_status=v0_cat(catReq, V0_ROLLBACKTRANSACTION)) < IMS_OK)
			{
				return (rollback_status);
			}
#ifdef QDEBUG
		(void) ims_msg (msgDesc, IMS_INFO, "*** Rollbacked Transaction.");
#endif

			return (status);
		}
	}

	/*
	** insert data into the scan table for SCAN REQUEST
	*/
	if (scan_p)
	{
                if ( (status = insert_scan (msgDesc, v0Desc)) < IMS_OK)
                {
 
                        /* set order_id to be 0 as indication of order error */
                        request->order_id = 0;
                        v0Desc->result.order_id = 0;
 
                        if ((rollback_status=v0_cat(catReq, V0_ROLLBACKTRANSACTION)) < IMS_OK)
                        {
                                return (rollback_status);
                        }
#ifdef QDEBUG
                	(void) ims_msg (msgDesc, IMS_INFO, "*** Rollbacked Transaction.");
#endif
 
                        return (status);
                }
        }

	/* commit transaction */
	if ( (status = v0_cat (catReq, V0_COMMITTRANSACTION)) < IMS_OK)
	{
		/* set order_id to be 0 as indication of order error */
		request->order_id = 0;
		v0Desc->result.order_id = 0;

		if ( (rollback_status=v0_cat(catReq, V0_ROLLBACKTRANSACTION)) < IMS_OK)
		{
			return (rollback_status);
		}
#ifdef QDEBUG
		(void) ims_msg (msgDesc, IMS_INFO, "*** Rollbacked Transaction.");
#endif

		return (status);
	}

#ifdef QDEBUG
		(void) ims_msg (msgDesc, IMS_INFO, "*** Committed Transaction.");
#endif

	(void) ims_msg (msgDesc, IMS_INFO, 
		"order %d submitted to IMS catalog successfully.",
		request->order_id);

	return (IMS_OK);

} /* end of insert_order */


/*************************************************************************
**
** insert_profile - build sql statment to insert profile information to
**                  the catalog
**
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int insert_profile (IMS_MSG_STRUCT *msgDesc,
                           V0_DESC_STRUCT *v0Desc, 
                           int callerFlag)
{
	V0_REQUEST_STRUCT *request;
	V0_CAT_STRUCT     *catReq;
	char   insert[IMS_COL255_LEN+1];
	char   contact_value[IMS_COL1024_LEN+1];
	char   value[IMS_COL1024_LEN+1];
	char   msgbuf[IMS_COL1024_LEN+1];   
	char   *i, *v, *cv;
	int    status;

	request = &v0Desc->request;
	catReq  = &v0Desc->catReq;

	insert[0] = '\0';
	contact_value[0] = '\0';
	value[0] = '\0';

	i = insert;
	cv = contact_value;

	/*
	** contact_profile
	*/
	(void) strcpy (i, " order_id, first_name, initial_name, last_name, title, organization, street, city, state, zipcode, country, phone, fax, email");
	i = i + strlen(i);

	(void) sprintf (cv, 
			"%d, '%s', '%s', '%s', '%s','%s', '%s', '%s', '%s', '%s', '%s', "
			"'%s', '%s', '%s'",
	                   request->order_id,
							       request->contact_profile.first_name,
			       			   request->contact_profile.initial_name,
						       	 request->contact_profile.last_name,
						       	 request->contact_profile.title,
										 request->contact_profile.organization,
										 request->contact_profile.street,
										 request->contact_profile.city,
										 request->contact_profile.state,
										 request->contact_profile.zipcode,
										 request->contact_profile.country,
										 request->contact_profile.phone,
										 request->contact_profile.fax,
										 request->contact_profile.email);
	cv = cv + strlen(cv);

	v0Desc->query.sql[0] = '\0';
	(void)sprintf (v0Desc->query.sql, 
		"insert contact_profile ( %s ) values ( %s )", insert, contact_value);
	catReq->item[0] = (void *)v0Desc->query.sql;

#ifdef QDEBUG
	sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
	(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "insert_profile: failed to insert to contact_profile");

		return (status);
	}

	/*
	**  both billing_profile and shipping_profile have an extra field -
	**  profile_id
	*/
	strcpy (i, ", profile_id");
	i = i + strlen(i);


	/*
	** billing_profile
	*/
	value[0] = '\0';
	v = value;

	/*
	** if different billing info, insert the given info. Otherwise, use the
	** contact info.
	*/
	if (request->billing_p == 1)
	{
		(void) sprintf (v, 
			"%d, '%s', '%s', '%s', '%s','%s', '%s', '%s', '%s', '%s', '%s', "
			"'%s', '%s', '%s', 1",
	                   request->order_id,
							       request->billing_profile.first_name,
			       			   request->billing_profile.initial_name,
						       	 request->billing_profile.last_name,
						       	 request->billing_profile.title,
										 request->billing_profile.organization,
										 request->billing_profile.street,
										 request->billing_profile.city,
										 request->billing_profile.state,
										 request->billing_profile.zipcode,
										 request->billing_profile.country,
										 request->billing_profile.phone,
										 request->billing_profile.fax,
										 request->billing_profile.email);
		v = v + strlen(v);

	}
	else
	{
		(void) sprintf (v, "%s, 1", contact_value);
		v = v + strlen(v);
	}

	v0Desc->query.sql[0] = '\0';
	(void)sprintf (v0Desc->query.sql, 
		"insert billing_profile ( %s ) values ( %s )", insert, value);
	catReq->item[0] = (void *)v0Desc->query.sql;

#ifdef QDEBUG
	sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
	(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "insert_profile: failed to insert to billing_profile");

		return (status);
	}

	/*
	** shipping_profile
	*/
	value[0] = '\0';
	v = value;

	/*
	** if different shipping info, insert the given info. Otherwise, use the
	** contact info.
	*/
	if (request->shipping_p == 1)
	{
		(void) sprintf (v, 
			"%d, '%s', '%s', '%s', '%s','%s', '%s', '%s', '%s', '%s', '%s', "
			"'%s', '%s', '%s', 1",
	                   request->order_id,
							       request->shipping_profile.first_name,
			       			   request->shipping_profile.initial_name,
						       	 request->shipping_profile.last_name,
						       	 request->shipping_profile.title,
										 request->shipping_profile.organization,
										 request->shipping_profile.street,
										 request->shipping_profile.city,
										 request->shipping_profile.state,
										 request->shipping_profile.zipcode,
										 request->shipping_profile.country,
										 request->shipping_profile.phone,
										 request->shipping_profile.fax,
										 request->shipping_profile.email);
		v = v + strlen(v);

	}
	else
	{
		(void) sprintf (v, "%s, 1", contact_value);
		v = v + strlen(v);
	}


	v0Desc->query.sql[0] = '\0';
	(void)sprintf (v0Desc->query.sql, 
		"insert shipping_profile ( %s ) values ( %s )", insert, value);
	catReq->item[0] = (void *)v0Desc->query.sql;

#ifdef QDEBUG
	sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
	(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "insert_profile: failed to insert to shipping_profile");

		return (status);
	}
	

	return (IMS_OK);
}/* end of insert_profile */

/*************************************************************************
**
** insert_order_item - build sql statment to insert line_item information to
**                  the order_item table
**
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int insert_order_item (IMS_MSG_STRUCT *msgDesc,
                              V0_DESC_STRUCT *v0Desc, 
                              int callerFlag)
{
	V0_REQUEST_STRUCT *request;
	V0_CAT_STRUCT     *catReq;
	V0_LINE_ITEM_LIST *line_item;
	char   insert[IMS_COL1024_LEN+1];
	char   value[IMS_COL1024_LEN*2];
	char   msgbuf[IMS_COL1024_LEN+1];   
	char   *i, *v;
	int    status;
	int    item_id;
	int    order_p, scan_p, dar_req_p;

	request = &v0Desc->request;
	catReq  = &v0Desc->catReq;

	order_p  = 0;
	scan_p   = 0;
	dar_req_p  = 0;
	item_id = 0;
	line_item = request->line_item_list;

	if ( v0Desc->RxType == V0_PRODUCT_REQUEST )
	{
		order_p = 1;
	}
	else if ( v0Desc->RxType == V0_SCAN_REQUEST )
	{
		scan_p = 1;
	}
	else if ( v0Desc->RxType == V0_DAR_REQUEST )
	{
		dar_req_p = 1;
	}

	/*
	** assign default value to op_validate_p 
	*/
	if (request->op_validate_p[0] == '\0')
	{
		strcpy (request->op_validate_p, "N");	
	}

	while (line_item != (V0_LINE_ITEM_LIST *)NULL)
	{
		if (line_item->quicklook_p[0] == '\0')
		{
			strcpy (line_item->quicklook_p, "N");	
		}

		item_id++;

		insert[0] = '\0';
		value[0] = '\0';

		i = insert;
		v = value;

		/*
		** order_id, item_id, order_item_type, quantity, cost, shipping_id, 
		** billing_id
		*/
		(void)strcpy (i, " order_id, item_id, order_item_type, quantity, cost, shipping_id, billing_id");
		i = i + strlen(i);

		(void)sprintf (v, " %d, %d, %d, 1, %8.2f, null, null",
                       request->order_id,
                       item_id,
		       line_item->order_item_type,
                       line_item->line_item_cost);
		v = v + strlen(v);

		/*
		** priority, dataset_idx, granule_idx, granule_name
		*/
		(void)strcpy (i, ", priority, dataset_idx, granule_idx, granule_name");
		i = i + strlen(i);

		if (line_item->order_item_type == DAR_TYPE)
		{
			(void)sprintf (v, ", %d, %d, %d, '%s'",
		                   request->dar_priority,
		                   line_item->dataset_idx,
		                   line_item->granule_idx,
		                   line_item->granule_id);
		}
		else
		{
			(void)sprintf (v, ", %d, %d, %d, '%s'",
		                   request->priority,
		                   line_item->dataset_idx,
		                   line_item->granule_idx,
				   line_item->granule_id);
		}
		v = v + strlen(v);

		/*
		** media_id, media_type, media_class, media_fmt_type, validated_p, 
		** cost_debited_p, status
		*/
		(void)strcpy (i, ", media_id, media_type, media_class, media_fmt_type, validated_p, cost_debited_p, status");
		i = i + strlen(i);

		if ( request->op_validate_p[0] ==  'Y')
		{
			(void)sprintf (v, ",null , %d, %d, %d, 'N', 'N', %d",
		                   	line_item->media_type,
		                   	line_item->media_class,
		                   	line_item->media_fmt_type,
												IMS_NEW_ITEM);
			v = v + strlen(v);
		}
		else if ( request->op_validate_p[0] == 'N')
		{
			(void)sprintf (v, ",null , %d, %d, %d, 'Y', 'N', %d",
		                   	line_item->media_type,
		                   	line_item->media_class,
		                   	line_item->media_fmt_type,
												IMS_VALIDATED_ITEM);
			v = v + strlen(v);
		}

		/*
		** shipped_p, billed_p, process_type, v0_process_type, 
		** process_status
		*/
		(void)strcpy (i, ", shipped_p, billed_p, process_type, v0_process_type, process_status, quicklook_p");
		i = i + strlen(i);

		(void)sprintf (v, ", 'N', 'N', %d, '%s', 0, '%s'",
				line_item->process_type,
			        line_item->v0_process_type,
			        line_item->quicklook_p);
		v = v + strlen(v);

		/*
		** platform, sensor, dataset
		*/
		(void)strcpy (i, ", platform, sensor, dataset");
		i = i + strlen(i);

		(void)sprintf (v, ", '%s', '%s', '%s'",
		               line_item->platform,
			       line_item->sensor_name,
			       line_item->dataset_id);
		v = v + strlen(v);

		/*
		** step_name, step_sequence, step_started_p, process_comment, 
		** op_comment
		*/
		(void)strcpy (i, ", step_name, step_sequence, step_started_p, process_comment, op_comment");
		i = i + strlen(i);

		(void)strcpy (v, ", null, 0, 'N', null, null");
		v = v + strlen(v);

		v0Desc->query.sql[0] = '\0';
		(void)sprintf (v0Desc->query.sql, 
		"insert order_item ( %s ) values ( %s )", insert, value);

		catReq->item[0] = (void *)v0Desc->query.sql;

#ifdef QDEBUG
		sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
		(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

		if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
		{
			(void) ims_msg(msgDesc, status, "insert_order_item: failed to insert to order_item");

			return (status);
		}

		/* 
		** update order_item with p_dataset_idx, p_granule_idx, p_granule_name,
		** p_data_kbytes, p_metadata_kbytes 
		*/
		if (line_item->order_item_type == APR_TYPE)
		{

			v0Desc->query.sql[0] = '\0';
			(void)sprintf (v0Desc->query.sql, 
			"update order_item set p_dataset_idx = %d, p_granule_idx = %d,"
			" p_granule_name = '%s', p_data_kbytes = %d, p_metadata_kbytes = %d"
			" where order_id = %d and item_id = %d", 
			    line_item->dataset_idx,
			    line_item->granule_idx,
			    line_item->granule_id,
                            line_item->data_kbytes,
			    line_item->metadata_kbytes,
                            request->order_id, item_id);

			catReq->item[0] = (void *)v0Desc->query.sql;

#ifdef QDEBUG
			sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
			(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

			if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
			{
				(void) ims_msg(msgDesc, status, "insert_order_item: failed to update order_item with p_dataset_idx, etc.");

				return (status);
			}

		}/* special handling for APR */

		line_item = line_item->next_p;
	}

	request->item_count = item_id;

	return(IMS_OK);

} /* end of insert_order_item */

/*************************************************************************
**
** insert_order_queue - build sql statment to insert order information to
**                  the order_queue table
**
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int insert_order_queue (IMS_MSG_STRUCT *msgDesc,
                               V0_DESC_STRUCT *v0Desc, 
                               int callerFlag)
{
	V0_REQUEST_STRUCT *request;
	V0_CAT_STRUCT     *catReq;
	char   insert[IMS_COL255_LEN+1];
	char   value[IMS_COL1024_LEN+1];
	char   msgbuf[IMS_COL1024_LEN+1];   
	char   *i, *v;
	int    status;
	int    dar_req_p;

	request = &v0Desc->request;
	catReq  = &v0Desc->catReq;

	insert[0] = '\0';
	value[0] = '\0';
	dar_req_p = 0;

	if (v0Desc->RxType == V0_DAR_REQUEST)
	{
		dar_req_p = 1;
	}

	i = insert;
	v = value;

	(void)strcpy (i, " order_id, user_id, account_id, received_time, completed_time, priority, item_count, status, op_comment");
	i = i + strlen(i);

	/*
	** order_id, user_id, account_id, received_time, completed_time, priority, 
	** item_count, status, op_comment
	*/
	if (dar_req_p)
	{
		if ( request->op_validate_p[0] ==  'Y')
		{
			(void)sprintf (v, " %d, '%s', '%s', getdate(), null, %d, %d, "
			"%d, 'received a DAR request,  order_id %d'",
	                request->order_id,
			request->user_id,
			request->account_id,
			request->dar_priority,
			request->item_count,
			OQ_NEW,
			request->order_id);
			v = v + strlen(v);
		}
		else if ( request->op_validate_p[0] ==  'N')
		{
			(void)sprintf (v, " %d, '%s', '%s', getdate(), null, %d, %d, "
				"%d, 'received a DAR request,  order_id %d'",
	                   request->order_id,
			   request->user_id,
			   request->account_id,
			   request->dar_priority,
			   request->item_count,
 			   OQ_PENDING,
			   request->order_id);
			v = v + strlen(v);
		}
	}
	else
	{
		if ( request->op_validate_p[0] ==  'Y')
		{
			(void)sprintf (v, " %d, '%s', '%s', getdate(), null, %d, %d, "
											"%d, 'started order processing for order_id %d'",
	                   request->order_id,
										 request->user_id,
										 request->account_id,
										 request->priority,
										 request->item_count,
										 OQ_NEW,
										 request->order_id);
			v = v + strlen(v);
		}
		else if ( request->op_validate_p[0] ==  'N')
		{
			(void)sprintf (v, " %d, '%s', '%s', getdate(), null, %d, %d, "
											"%d, 'started order processing for order_id %d'",
	                   request->order_id,
										 request->user_id,
										 request->account_id,
										 request->priority,
										 request->item_count,
										 OQ_PENDING,
										 request->order_id);
			v = v + strlen(v);
		}
	}

	v0Desc->query.sql[0] = '\0';
	(void)sprintf (v0Desc->query.sql, 
		"insert order_queue ( %s ) values ( %s )", insert, value);

	catReq->item[0] = (void *)v0Desc->query.sql;

#ifdef QDEBUG
	sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
	(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "insert_order_queue: failed to insert to order_queue");

		return (status);
	}

	return(IMS_OK);

} /* end of insert_order_queue */


/********************************************************************************
**
** generate_scan_item - 
**	Expand the line_item list for scan request.  Each scan_item
**      in the original line_item list generates up to the number of
**      entries of datatake_entry corresponding to the downlink
**      associated with this scan_item. 
**
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**
**************************************************************************/
static int generate_scan_item (IMS_MSG_STRUCT *msgDesc,
                               V0_DESC_STRUCT *v0Desc) 
{
	V0_REQUEST_STRUCT *request;
	V0_CAT_STRUCT     *catReq;
	V0_LINE_ITEM_LIST *line_item;  /* original request line item list */
	V0_LINE_ITEM_LIST *scan_item;  /* expanded scan item list */
	V0_LINE_ITEM_LIST *lastPtr;
	int               status;
	int		  lineItemIndex = 0;
	int		  rowCount;
	char		  msgbuf[IMS_COL1024_LEN+1];

	request = &v0Desc->request;
	catReq  = &v0Desc->catReq;

	line_item = request->line_item_list;
	scan_item = lastPtr = (V0_LINE_ITEM_LIST *) NULL;

	while (line_item != (V0_LINE_ITEM_LIST *)NULL)
	{

		/* 
		** get item info for this line item from
		** from the datatake_entry table
		*/
		v0Desc->query.sql[0] = '\0';
		(void) sprintf(v0Desc->query.sql,
			"exec v0_get_datatake '%s','%s', %d, %d",
			line_item->platform_alias,
			line_item->sensor_alias,
			line_item->revolution,
			line_item->sequence); 
#ifdef QDEBUG
                sprintf(msgbuf,"sql => %s", v0Desc->query.sql);
                (void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

		/*
		** set up inputs to v0_cat__V0_GETDATATAKE
		** item[0] points to the sql statement to be executed
		** item[3] points to the original scan item request
		*/
		catReq->item[0] = (void *)v0Desc->query.sql;
		catReq->item[3] = (void *)line_item;

		/*
		** number of datatake records found will be
		** returned in item[1]
		*/ 
		rowCount = 0;
		catReq->item[1] = (int *)&rowCount;

		if ((status = v0_cat(catReq,V0_GETDATATAKE)) < IMS_OK)
		{
			return(status);
		}

		/*
		** It is an error condition if no records found in the
		** datatake_entry for a line_item
		*/
		if (rowCount <= 0)
		{
			(void)ims_msg(msgDesc, IMS_ERROR, "v0_Order__generate_scan_item:"
				" Found no datatake records for granule '%s' [idx = %d]"
				" dataset '%s'",
				line_item->granule_id, line_item->granule_idx,
				line_item->dataset_id);
			/* free the scan_item list */
			while (scan_item != (V0_LINE_ITEM_LIST *)NULL)
                	{
				lastPtr = scan_item->next_p;
				free (scan_item);
				scan_item = lastPtr;
			}
			return (IMS_ERROR);
		}	

		/*
		** v0_cat__V0_GETDATATAKE returns through item[2]
		** the scan_item list containing one item per record found
		** in the datatake_entry table.  It also returns the
		** pointer to the last item in the list through item[4].
		** We want to attach this returned list to the scan_item list.
		*/
		lineItemIndex++;

		if (lineItemIndex == 1)
		{
			scan_item = (V0_LINE_ITEM_LIST *)catReq->item[2];
			lastPtr = (V0_LINE_ITEM_LIST *)catReq->item[4];
		}
		else
		{
			lastPtr->next_p = (V0_LINE_ITEM_LIST *)catReq->item[2];
			lastPtr = (V0_LINE_ITEM_LIST *)catReq->item[4];
		}

		/* process the next line_item */
		line_item = line_item->next_p;

	}
	/*
	** Replace the line_item list with the expanded scan_item list
	** Free the original line_item list
	*/ 
	if (lineItemIndex > 0)
	{
		line_item = request->line_item_list;
                while (line_item != (V0_LINE_ITEM_LIST *)NULL)
                {
                        lastPtr = line_item->next_p;
                        free (line_item);
                        line_item = lastPtr;
                }
		request->line_item_list = scan_item;
	}

	return (IMS_OK);

} /* end of generate_scan_item */

/*************************************************************************
**
** insert_dar - build sql statment to insert information to the dar table
**
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int insert_dar (IMS_MSG_STRUCT *msgDesc,
                       V0_DESC_STRUCT *v0Desc,
			int callerFlag )
{
	V0_REQUEST_STRUCT *request;
	V0_CAT_STRUCT     *catReq;
	V0_LINE_ITEM_LIST *line_item;
	char   msgbuf[IMS_COL1024_LEN+1];   
	char   insert[IMS_COL255_LEN+1];
	char   value[IMS_COL1024_LEN+1];
	char   *i, *v;
	int    item_id;
	int    status;

	request = &v0Desc->request;
	catReq  = &v0Desc->catReq;
	line_item = request->line_item_list;
	item_id = 0;

	while (line_item != (V0_LINE_ITEM_LIST *)NULL)
	{
		insert[0] = '\0';
		value[0]  = '\0';
		i = insert;
		v = value;
		item_id++;
	
		(void) strcpy (i, "order_id, item_id, platform, sensor, mode");
		i = i + strlen(i);
	
		if (strcmp (line_item->platform, "RADARSAT-1") == 0)
		{
			(void) sprintf (v, "%d, %d, '%s', '%s', '%s'",
	                  request->order_id,
									 	item_id,
										line_item->platform,
										line_item->sensor_name,
										request->mode);
		}
		else
		{
			(void) sprintf (v, "%d, %d, '%s', '%s', null",
	                  request->order_id,
									 	item_id,
										line_item->platform,
										line_item->sensor_name);
		}
		v = v+ strlen(v);
	
		(void) strcpy (i, 
				 ", asc_desc, start_date, end_date, site_name, spatial_type");
		i = i + strlen(i);

		(void) sprintf (v, ", '%s', '%s', '%s', '%s', %d",
		                request->asc_desc,
										request->start_time,
										request->end_time,
                    request->site_name,
										request->spatial_type);
		v = v + strlen(v);

		switch (request->spatial_type)
		{
			case 3:

				(void) strcpy (i, ", radius, center_lat, center_lon");
				i = i + strlen(i);

				(void) sprintf (v, ", %3.4f, %3.4f, %3.4f",
				            request->radius,
										request->center_lat,
										request->center_lon);
				v = v + strlen(v);

				break;

			case 4:

				(void) strcpy (i, ", north_west_lat, north_west_lon, north_east_lat, north_east_lon, south_east_lat, south_east_lon, south_west_lat, south_west_lon");
				i = i + strlen(i);

				(void) sprintf 
					(v, ", %3.4f, %3.4f, %3.4f, %3.4f, %3.4f, %3.4f, %3.4f, %3.4f",
				            request->nw_lat,  
				            request->nw_lon,  
				            request->ne_lat,  
				            request->ne_lon,  
				            request->se_lat,  
				            request->se_lon,  
				            request->sw_lat,  
				            request->sw_lon);  
				v = v + strlen(v);

				break;
		}

		(void) strcpy 
			(i, ", observation_freq, observation_num, pi_name, pi_discipline");
		i = i + strlen(i);

		if ((request->observation_num == -1) && 
				(request->observation_freq[0] =='\0'))
		{
			(void) sprintf (v, ", null, null, '%s', '%s'",
										line_item->pi_name,
										line_item->pi_discipline);
		}
		else
		{
			(void) sprintf (v, ", '%s', %d, '%s', '%s'",
		                request->observation_freq,
										request->observation_num,
										line_item->pi_name,
										line_item->pi_discipline);
		}
		v = v + strlen(v);

		(void) strcpy (i, ", active_p, activity_start_date, activity_end_date");
		i = i + strlen(i);

		if ( strcmp (request->active_p, "Y") == 0)
		{
			(void) sprintf (v, ", '%s', '%s', '%s'",
		                request->active_p,
										request->activity_start_date,
										request->activity_end_date);
		}
		else if ( strcmp (request->active_p, "N") == 0)
		{
			(void) sprintf (v, ", '%s', null, null",
		                request->active_p);
		}
		v = v + strlen(v);

		(void) strcpy (i, ", status, user_comment, planner_comment, op_comment");
		i = i + strlen(i);

		if ( request->op_validate_p[0] ==  'Y')
		{
			(void)sprintf (v, ", %d, '%s', null, null", 
					DAR_NEW,
					request->dar_comment);
			v = v + strlen(v);
		}
		else if ( request->op_validate_p[0] == 'N')
		{
			(void)sprintf (v, ", %d, '%s', null, null", 
					DAR_VALIDATED,
					request->dar_comment);
			v = v + strlen(v);
		}

		v0Desc->query.sql[0] = '\0';
		(void)sprintf (v0Desc->query.sql, 
			"insert dar ( %s ) values ( %s )", insert, value);
	
		catReq->item[0] = (void *)v0Desc->query.sql;
	
#ifdef QDEBUG
		sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
		(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
	
		if ( (status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
		{
			(void) ims_msg(msgDesc, status, 
						"insert_dar: failed to insert to dar table");
			(void) strcpy (v0Desc->odl_status, "03");
	
			return (status);
		}

		line_item = line_item->next_p;
	}

	return (IMS_OK);
}/* end of insert_dar */

/*************************************************************************
**
** insert_scan - build sql statment to insert information to the scan table
**
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int insert_scan (IMS_MSG_STRUCT *msgDesc,
                        V0_DESC_STRUCT *v0Desc)
{
        V0_REQUEST_STRUCT *request;
        V0_CAT_STRUCT     *catReq;
        V0_LINE_ITEM_LIST *line_item;
        char   msgbuf[IMS_COL1024_LEN+1];
        char   insert[IMS_COL255_LEN+1];
        char   value[IMS_COL1024_LEN+1];
        int    item_id;
        int    status;
 
        request = &v0Desc->request;
        catReq  = &v0Desc->catReq;
        line_item = request->line_item_list;
        item_id = 0;
	insert[0] = '\0';
	(void) strcpy (insert, "order_id, item_id, DT_PLATFORM, DT_SENSOR," 
                                "DT_REVOLUTION, DT_SEQUENCE, MODE, QUICKLOOK_FLAG,"
                                "FRAME_MODE, TIME_ON, TIME_OFF, SITE_NAME,"
                                "ACTIVITY_ID, STATION_ID"); 
 
        while (line_item != (V0_LINE_ITEM_LIST *)NULL)
        {
                value[0]  = '\0';
                item_id++;
 
		(void) sprintf (value, "%d, %d, '%s', '%s',"
				"%d, %d, '%s', '%s',"
				"'%s', '%s', '%s', '%s',"
				"'%s', '%s'",
				request->order_id, item_id, 
				line_item->platform_alias, line_item->sensor_alias,
				line_item->revolution, line_item->sequence,
				line_item->mode, line_item->quicklook_p,
				line_item->frame_mode, line_item->time_on,
				line_item->time_off, line_item->site_name,
				line_item->activity_id, line_item->station_id);

		v0Desc->query.sql[0] = '\0';
		(void) sprintf(v0Desc->query.sql,
			"insert scan (%s) values (%s)", insert, value);

		catReq->item[0] = (void *)v0Desc->query.sql;

#ifdef QDEBUG
		sprintf (msgbuf,"sql ==> %s",v0Desc->query.sql);
		(void) ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif
		if ((status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
		{
			(void) ims_msg(msgDesc, status,
				"v0Order__insert_scan: failed to insert"
				" into scan table");
			return (status);
		}
				
		line_item = line_item->next_p;
	}

	return (IMS_OK);
}

/*************************************************************************
**
** log_order_error -
**
** purpose: record errors encounterred during user/account verification 
**
**   Note:  This is a workaround, because V0 currently do not provide
**          return status code for PRODUCT REQUESTs.  All the error messages
**          will be sent back to V0 client once the error reporting capability
**          is available.  -jlw
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int log_order_error (IMS_MSG_STRUCT *msgDesc,
                            V0_DESC_STRUCT *v0Desc, 
                            char *msg, int callerFlag)
{

	V0_REQUEST_STRUCT *request;
	V0_CAT_STRUCT     *catReq;
	int status, rollback_status;
	char   msgbuf[IMS_COL1024_LEN+1];   

	request = &v0Desc->request;
	catReq  = &v0Desc->catReq;

	if ((status = v0_cat (catReq, V0_BEGINTRANSACTION)) < IMS_OK)
	{
		return (status);
	}
#ifdef QDEBUG
	(void) ims_msg (msgDesc, IMS_INFO, "*** Began Transaction.");
#endif

	v0Desc->query.sql[0] = '\0';
	if (v0Desc->RxType == V0_PRODUCT_REQUEST)
	{
		(void)sprintf (v0Desc->query.sql, 
			"insert into v0_order_error (request_time, request_id, message_name,"
			"first_name, initial_name, last_name, phone, fax, email, description) "
			"values (getdate(),'%s','%s','%s','%s','%s','%s','%s','%s','%s')",
			request->order_request_id, v0Desc->message_name,      
			request->contact_profile.first_name, 
			request->contact_profile.initial_name,
			request->contact_profile.last_name,  
			request->contact_profile.phone,      
			request->contact_profile.fax,
			request->contact_profile.email,      
			msg);
	}
	else if (v0Desc->RxType == V0_DAR_ACCT_SEARCH )
	{
		(void)sprintf (v0Desc->query.sql, 
			"insert into v0_order_error (request_time, request_id, message_name,"
			"first_name, initial_name, last_name, phone, fax, email, description) "
			"values (getdate(),'DAR Login','%s','%s',null,'%s',null,null,null,'%s')",
			v0Desc->message_name,
			request->user_info.first_name, 
			request->user_info.last_name,  
			msg);
	}
	else if ( v0Desc->RxType == V0_DAR_REQUEST )
	{
		(void)sprintf (v0Desc->query.sql, 
			"insert into v0_order_error (request_time, request_id, message_name,"
			"first_name, initial_name, last_name, phone, fax, email, description) "
			"values (getdate(),'DAR Request','%s','%s',null,'%s',null,null,null,'%s')",
			v0Desc->message_name,
			request->user_info.first_name, 
			request->user_info.last_name,  
			msg);
	}
	catReq->item[0] = (void *)v0Desc->query.sql;

#ifdef QDEBUG
		sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
		(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	if ((status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status, 
			"log_order_error: failed to log order error in database");

		if ( (rollback_status=v0_cat(catReq, V0_ROLLBACKTRANSACTION)) < IMS_OK)
		{
			return (rollback_status);
		}
#ifdef QDEBUG
		(void) ims_msg (msgDesc, IMS_INFO, "*** Rollbacked Transaction.");
#endif

		return (status);
	}

	if ( (status = v0_cat (catReq, V0_COMMITTRANSACTION)) < IMS_OK)
	{
		if ( (rollback_status=v0_cat(catReq, V0_ROLLBACKTRANSACTION)) < IMS_OK)
		{
			return (rollback_status);
		}
#ifdef QDEBUG
	(void) ims_msg (msgDesc, IMS_INFO, "*** Rollbacked Transaction.");
#endif

		return (status);
	}

#ifdef QDEBUG
	(void) ims_msg (msgDesc, IMS_INFO, "*** Committed Transaction.");
#endif

	return (IMS_OK);

} /* end of log_order_error */

/*************************************************************************
**
** v0_order__report_order_error - 
**		error handling for order process based on callerFlag
**
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
int v0_order__report_order_error (IMS_MSG_STRUCT *msgDesc, 
                               V0_DESC_STRUCT *v0Desc, 
                               int callerFlag, 
                               char *errbuf,     
                               char *v0_errbuf)
{
	int    status;
	char   err_msg[IMS_COL1024_LEN*2];
	char   subject[IMS_COL255_LEN+1];
	char   *em;
		
	if (callerFlag == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR, errbuf);
	}

	else if (callerFlag == 1)
	{
		
		(void) ims_msg (msgDesc, IMS_ERROR, v0_errbuf);

		/*
		** insert entry into v0_order_error
		*/
		if ((status=log_order_error(msgDesc,v0Desc,v0_errbuf,callerFlag))<IMS_OK)
		{
			return (status);
		}
                /*
                ** Copy the v0_errbuf to status_code_comment to return
                ** to the client
                */
                (void) v0_util__set_status_code_comment
                        (v0Desc, v0_errbuf, V0_ORDER_ERROR);
 
#ifdef NOTYET

		/*
		** if we've already collected user's email address at this point
		** an electronic mail will be sent to both the user and ASF User
		** Service, otherwise, only ASF User Service will get notified 
		**
		** ASF User Service would not be notified about order error if it's
		** address is not available.
		**
		** send email notification only for user errors in PRODUCT REQUEST
		** and DAR REQUEST
		*/

		/*
		** get user's email address from other possible fields
		*/
		
		/* commented out before R2.1
		if (v0Desc->request.contact_profile.email[0] == '\0')
		{
			strcpy (v0Desc->request.contact_profile.email,
							v0Desc->request.user_email);
		}

		if ( (v0Desc->RxType == V0_PRODUCT_REQUEST) ||
				 (v0Desc->RxType == V0_DAR_REQUEST))
		{
			if ((v0Desc->request.contact_profile.email[0] != '\0') &&
					(strcmp(v0Desc->result.contact_info->email, "N/A") != 0))
			{
				err_msg[0] = '\0';
				em = err_msg;
	
				if (v0Desc->RxType == V0_PRODUCT_REQUEST)
				{ 
					subject[0] = '\0';
					sprintf (subject, "V0 Order Error Notification, Message id %s",
				         	v0Desc->request.order_request_id);

					strcpy (em, "********  Product Request Error Notification  ********\n");
					em = em + strlen(em);
				}
				else if (v0Desc->RxType == V0_DAR_REQUEST)
				{
					subject[0] = '\0';
					sprintf (subject, "DAR Request Error Notification. Username %s %s",
				         	v0Desc->request.user_info.first_name, 
								 	v0Desc->request.user_info.last_name);

					strcpy (em, "********  DAR Request Error Notification  ********\n");
					em = em + strlen(em);
				}
	
				strcpy (em, "Please modify your request and resubmit it.\n" );
				em = em + strlen(em);
	
				sprintf (em, "Or contact ASF User Service at %s \n", 
			      		v0Desc->result.contact_info->email);
				em = em + strlen(em);
	
				sprintf (em, "or %s for details.\n\n", 
							v0Desc->result.contact_info->phone);
				em = em + strlen(em);
	
				sprintf (em, "Description: %s\n\n", v0_errbuf);
				em = em + strlen(em);

				if ( (status = v0_util__send_email 
					(msgDesc, subject, err_msg, 
				 	v0Desc->request.contact_profile.email, 
				 	v0Desc->result.contact_info->email)) < IMS_OK)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
							"v0_order__report_order_error: Failed to create/send an email");
					return (status);
				}
			}
	
			else if (strcmp(v0Desc->result.contact_info->email, "N/A") != 0)
			{
				err_msg[0] = '\0';
				em = err_msg;
	
				if (v0Desc->RxType == V0_PRODUCT_REQUEST)
				{ 
					subject[0] = '\0';
					sprintf (subject, "V0 Order Error Notification, Message id %s",
				         	v0Desc->request.order_request_id);

					strcpy (em, "********  Product Request Error Notification  ********\n");
					em = em + strlen(em);

					if ( (v0Desc->request.contact_profile.first_name[0] != '\0') &&
							 (v0Desc->request.contact_profile.last_name[0] != '\0'))
					{
						sprintf (em, 
							"To ASF User Service: Please notify user %s %s "
							"on the following error.\n\n",
							v0Desc->request.contact_profile.first_name, 
							v0Desc->request.contact_profile.last_name);
						em = em + strlen(em);
					}
					else
					{
						strcpy (em, 
							"To ASF User Service: Please notify user "
							"on the following error.\n\n" );
						em = em + strlen(em);
					}
				}
				else if (v0Desc->RxType == V0_DAR_REQUEST)
				{
					subject[0] = '\0';
					sprintf (subject, 
									 "DAR Request Error Notification. Username %s %s",
				         	v0Desc->request.user_info.first_name, 
								 	v0Desc->request.user_info.last_name);

					strcpy (em, "********  DAR Request Error Notification  ********\n");
					em = em + strlen(em);

					sprintf (em, 
						"To ASF User Service: Please notify user"
						" %s %s on the following error.\n\n", 
						v0Desc->request.user_info.first_name, 
						v0Desc->request.user_info.last_name);
					em = em + strlen(em);
				}

				sprintf (em, "Description: %s", v0_errbuf);
				em = em + strlen(em);

				if ( (status = v0_util__send_email 
					(msgDesc, subject, err_msg, 
				 	v0Desc->result.contact_info->email, NULL)) < IMS_OK)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
							"v0_order__report_order_error: Failed to create/send an email");
					return (status);
				}
		
			}
		}   commented out before R2.1*/ 
#endif /* NOTYET */
		
	} /* if callerFlag == 1 */


	return (IMS_OK);
}/* end of v0_order__report_order_error */

/***********************************************************************
**      
** retrive_daac_contact_info -  query the catalog for daac contact info
**         as return value for product requests.
**
** called by: v0_process
**
************************************************************************/           
static int retrieve_daac_contact_info (IMS_MSG_STRUCT *msgDesc,
                                       V0_DESC_STRUCT *v0Desc) 
{
	V0_CAT_STRUCT    *catReq;
	V0_RESULT_STRUCT *result;
	int status;
	char   msgbuf[IMS_COL1024_LEN+1];   

	catReq =  &v0Desc->catReq;
	result =  &v0Desc->result;

	v0Desc->query.sql[0] = '\0';
	(void)strcpy (v0Desc->query.sql, "exec v0_get_contact_info");

	catReq->item[0] = (void *)v0Desc->query.sql;
		
	
#ifdef QDEBUG
	sprintf (msgbuf, "sql ==> %s", v0Desc->query.sql);
	(void)ims_msg (msgDesc, IMS_INFO, msgbuf);
#endif

	if ( (status = v0_cat (catReq, V0_GETCONTACTINFO) ) < IMS_OK)
	{
		(void)ims_msg(msgDesc,status,"Failed to retrieve ASF User Service contact information.");
		return (status);
	}

	result->contact_info = (V0_CONTACT_INFO *)catReq->item[1];

	/*
	** fill values into the blanks
	*/
	if (strlen(result->contact_info->organization) == 0) 
	{
		strcpy (result->contact_info->organization, "N/A");
	}
	else if (strlen(result->contact_info->street) == 0)
	{
		strcpy (result->contact_info->street, "N/A");
	}
	else if (strlen(result->contact_info->city) == 0)
	{
		strcpy (result->contact_info->city, "N/A");
	}
	else if (strlen(result->contact_info->state) == 0)
	{
		strcpy (result->contact_info->state, "N/A");
	}
	else if (strlen(result->contact_info->zipcode) == 0)
	{
		strcpy (result->contact_info->zipcode, "N/A");
	}
	else if (strlen(result->contact_info->country) == 0)
	{
		strcpy (result->contact_info->country, "N/A");
	}
	else if (strlen(result->contact_info->phone) == 0)
	{
		strcpy (result->contact_info->phone, "N/A");
	}
 	else if (strlen(result->contact_info->fax) == 0) 
	{
		strcpy (result->contact_info->fax, "N/A");
	}
	else if (strlen(result->contact_info->email) == 0) 
	{
		strcpy (result->contact_info->email, "N/A");
	}

	return (IMS_OK);
} /* end of retrieve_daac_contact_info */
