static char *sccs = "@(#)ims_v0Handler.c	5.12  09/05/97";
/************************************************************************
** 
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
**   ims_v0Handler - Client connection handler.
**
**   Purpose
**    	The ims_v0Handler is responsible for interpreting and processing
** 	    messages received by the ims_v0Srv. 
**
**   Creator   :  Julie Wang, Hoshyar Sayah
**
**   Date      :  April 28 1994
**
** Modifications:
**
**   04/18/97    jwang   Granule id search. 
**   
**   07/24/96    jwang   User abort handling, for Inventory Srch only.
**   
**   06/06/96    jwang   added comment for timeout count change (for PR822)
**
**   05/24/96    jwang   PR 771 
**
**   04/25/96    jwang   Changed statusCode type from char * to char string 
**
**   04/03/96    jwang   Added code to handle detail metadata collection
**
**   02/20/96    jwang   R1Bprime (preliminary).
**
**   02/12/96    jwang   Took out all possible syslog calls in the case of
**                       receiving QUIT as the first message to avoid
**                       side-effect from V0 ponging function.
**
**   01/22/96    jwang   Changes added to handle DARs and SCAN requests
**
**   07/28/95    jwang   V3.10 schema changes
**
**   06/05/95    jwang   Changed all reference of cdb_ or CDB_ to 
**                       ims_ or IMS_
**
**   06/05/95    jwang   IMS front-end order processing
**
**   03/31/95    jwang   Spatial search pass 2 
**
**   02/01/95    jwang   Spatial search pass 1
**
**   11/18/94    jwang   Added seasonal search capability.
**
**   10/01/94    jwang   IMS/DADS Release 0.
**
**
*************************************************************************/

/*
** Undefine Posix Source flag because of imcompatibility
** with IK include files.
*/
#undef _POSIX_SOURCE

#include <stdio.h>
#include <IK_Network.h>
#include <odlinter.h>
#include <ims_query.h>
#include <ims_v0.h>

/* External Functions */
extern int RxODL ();
extern int TxODL ();

/* External defination of system and IK error variables */
#ifndef NDEBUG
extern int DebugLevel;
#endif
extern int errno;
extern IK_Errno IK_ImsErrno;

/*
** Local function declarations
*/
static int v0_getArchiveFileName (V0_DESC_STRUCT *);
static int v0Message_Archive(V0_DESC_STRUCT *, char *);
static int v0_insertMessageArchive (V0_DESC_STRUCT *, char, char *);
static int v0_updateMessageArchive (V0_DESC_STRUCT *, int); 

/************************************************************************
**		
**		v0_handler
**
** Purpose
**		Reads an odl tree from the network and identifies the type of
**		message and passes the message to the appropriate routines.
**
*************************************************************************/
int v0_handler (IMS_MSG_STRUCT *msgDesc, int msgId, char *glbLogin,
	char *glbPassword, char *glbArchiveDir)
{
	AGGREGATE	RxTree;      /* received message tree */
	AGGREGATE TxTree;      /* response message tree */
	AGGREGATE result_msg;  /* part of TxTree containing the msg */
	V0_DESC_STRUCT v0Desc; /* structure for search specs and results */
	V0_MSG_TYPE msgType;   /* message id type */
	char errbuf[IK_MAXBUFLEN];       /* IK message buffer */
	char *source = "CH__INIT";
	int count;              /* count for time out in IK_RxODL loop */
	int processFlag;        /* process flag indicator */
	int quitFlag;           /* flag to send quit msg */
	int errorFlag;          /* error flag indicator */
	int status;             /* returned status */
	int processStatus;	/* v0_process status */
	int archiveStatus;      /* status returned from v0Message_Archive */
	char statusCode[IMS_COL10_LEN+1];
	char msgTypeName[IMS_COL20_LEN+1];
	int pongFlag;           /* set to 1 if a V0_QUIT is received as the first
														 message, which is an indication of the periodically
														 V0 statistic ponging. Nothing should be recorded 
														 to the syslog in this case. Otherwise the syslog 
														 would be filled with debug messages and may cause 
														 disk space problem */


	/* initailize */
	errno = 0;
	statusCode[0]='\0';
	strcpy (statusCode,"00"); /* status not set */
	processFlag = 0;
	RxTree = (AGGREGATE) NULL;
	pongFlag = 0;

	/* 
	** wait for a message, 
	** the socket is non-blocking so we have to 
	** loop in order to wait 
	**/

	if ((status = v0_handler__get (msgDesc, &RxTree, msgId))
			< IMS_OK)
	{
		/* initialize */
		errno = 0;

		/* error detected while receiving msg */

		/* initialize transmit msg structure */
		TxTree = (AGGREGATE) NULL;

		/* 
		** Create abort message. 
		*/
		strcpy (statusCode, "01");

		if (v0_msgTree__create (msgDesc, (AGGREGATE)NULL, 
			&TxTree, V0_ABORT, statusCode, NULL, 0) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_handler: Failed to create abort msg.");
		}
		else
		{
			/*
			** Transmit abort msg 
			*/
			if ( (status = v0_handler__send (msgDesc, TxTree, msgId)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_handler: Failed to transmit abort msg.");
			}

		}

#		ifdef ODEBUG

			if (status == IMS_OK)
			{
				printf("\n********** ODL returned to V0 client ************\n");
				PrintLabel (TxTree);
			}		
#		endif

		(void) v0_msgTree__destroy (TxTree);
		(void) v0_msgTree__destroy (RxTree);

		return (status);

	} /* if failed to get ODL tree */

	else
	{
		/* a message was received. Identify the message type */
		msgType = (V0_MSG_TYPE) v0_msgTree__identify (msgDesc, RxTree);

		processFlag = 0;
		quitFlag = 0;
		switch (msgType)
		{
			case V0_INVENTORY_SEARCH:
			case V0_DIRECTORY_SEARCH:
			case V0_PRODUCT_REQUEST:
			case V0_DAR_ACCT_SEARCH:
			case V0_DAR_REQUEST:
			case V0_DAR_LIST_QUERY:
			case V0_DAR_STATUS_QUERY:
			case V0_DAR_CONTENT_QUERY:
			case V0_DAR_GRANULE_QUERY:
	
#			ifdef MDEBUG
				(void) v0_msgTree__get_message_type (msgDesc, 
					RxTree, msgTypeName);
				(void) ims_msg (msgDesc, IMS_INFO, 
					"*** message id %d is a %s", msgId, msgTypeName);
#			endif

			processFlag = 1; /* flag to process the msg */

			break;

		case V0_BROWSE_REQUEST:
			strcpy (statusCode, "10"); /* unsupported function */
			quitFlag = 1;
			break;

		case V0_ACKNOWLEDGE:
			strcpy (statusCode, "17"); 
			/* bad message, protocal error. 
			** Not suppose to receive an ACK here*/
			quitFlag = 1;
			break;

		/*
		case V0_ABORT: 
			strcpy (statusCode, "01"); 
			quitFlag = 1;
			break;
			*/

		case V0_QUIT:
			/* 
			** received a QUIT as the first message.  This is caused by the
			** IK ponging, v0 server does not need to take any action.
			*/
			pongFlag = 1;
			break;

		default:
			strcpy (statusCode, "17"); /* bad message */
			quitFlag = 1;
			break;
		}

		/* process the message */
		if (processFlag)
		{
#			ifdef DEBUG
				(void) ims_msg (msgDesc, IMS_INFO, "Processing message.");
#			endif

			/* initialize v0Desc */
			(void) v0_handler__v0Desc_init
			 (msgDesc, RxTree, msgType, msgId, &v0Desc, glbLogin, glbPassword);

			/* callerFlag should be set to one since request come in should all
				 be from V0 */
			v0Desc.request.callerFlag = 1;

			/*
			** Open a connection to the database 
			*/
			if (v0_cat (&v0Desc.catReq, V0_OPENCONNECTION) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
					"v0_handler: Database login failed.");
				strcpy (statusCode, "08");
				quitFlag = 1;
			}
			else
			{

				/* 
				** Archive the message and make initial
				** record in v0_message_archive table. 
				*/
				(void) v0_getArchiveFileName(&v0Desc);
				archiveStatus = v0Message_Archive 
						(&v0Desc, glbArchiveDir);
			
				/*
				** Process the message
				*/	
				if (((processStatus = v0_process 
					(msgDesc, &v0Desc)) < IMS_OK) ||
				    ((msgType == V0_INVENTORY_SEARCH) && 
					(!v0Desc.request.user_aborted_p))     || 
				    ((msgType == V0_DIRECTORY_SEARCH) && 
					(v0Desc.result.no_match_found))  )
				{
					strcpy (statusCode, v0Desc.odl_status);
					quitFlag = 1;
				}

				/*
				** Update the status field for the message
				** in v0_message_archive table (only if the initial record
				** was successfully inserted).
				*/
				if (archiveStatus == IMS_OK)
				{	
					(void)v0_updateMessageArchive
						(&v0Desc, processStatus);
				}
				
				/*
				** close connection to the database 
				*/
				if (v0_cat (&v0Desc.catReq, V0_CLOSECONNECTION) < IMS_OK)
				{
					(void) ims_msg (msgDesc, IMS_WARNING, 
						"v0_handler: Database close failed.");
				}
			}


		} /* processFlag handling */

		if (quitFlag)
		{
			/* initialize transmit msg structure */
			TxTree = (AGGREGATE) NULL;

			/* 
			** Create quit message. 
			*/
			if ( (msgType == V0_DAR_ACCT_SEARCH) ||
		        	(msgType == V0_DAR_REQUEST)  ||
				(msgType == V0_DAR_LIST_QUERY) ||
				(msgType == V0_DAR_STATUS_QUERY) ||
				(msgType == V0_DAR_CONTENT_QUERY) ||
				(msgType == V0_DAR_GRANULE_QUERY) ||
				(msgType == V0_PRODUCT_REQUEST))
			{
				status = v0_msgTree__create
					(msgDesc, RxTree, &TxTree, V0_QUIT, 
					 statusCode,&v0Desc.result,0);
			}
			else
			{
				status = v0_msgTree__create
					(msgDesc, RxTree, &TxTree, V0_QUIT, 
					 statusCode, NULL, 0);
			}

			if (status < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_handler: Failed to create quit msg.");
			}
			else
			{
				/*
				** Transmit quit msg 
				*/
				if ( (status= v0_handler__send (msgDesc, TxTree, msgId)) < IMS_OK)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_handle: Failed to transmit quit msg.");
				}


			} 

#			ifdef ODEBUG
				if ((status == IMS_OK) && (!pongFlag))
				{ 
					printf("\n********** ODL returned to V0 client ************\n");
					PrintLabel (TxTree);
				}
#			endif


			(void) v0_msgTree__destroy (TxTree);

		}

	} /* if received a message successfully */ 

	if (processFlag) 
	{
		/* Cleanup v0Desc */
		(void) v0_handler__v0Desc_cleanup (&v0Desc);
	}

	/* release msg tree structures */
	(void) v0_msgTree__destroy (RxTree);


#	ifdef DEBUG
	if (!pongFlag)
	{
		errbuf[0] = '\0';
		(void) sprintf (errbuf, "Completed message processing. msgId = %d", msgId);
		(void) ims_msg (msgDesc, IMS_INFO, errbuf);
	}
#	endif

	return (ims_msgGetSeverity(msgDesc));
}  /* end of v0_handler */       

/***********************************************************************
**
** v0_handler__send - transmit msg structure on messag_id socket.
**                    when acknowledge flag is set, get the acknowledge
**                    msg before transmitting.
**
***********************************************************************/
int v0_handler__send (IMS_MSG_STRUCT *msgDesc,
	AGGREGATE msgTree, int msgId)
{
	int status;
	V0_MSG_TYPE msgType;

	/* initialize */
	errno = 0;

	/* transmit msg */
	if (IK_TxODL (msgId, &msgTree) < 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_handler__send: IK_TxODL transmit failed, %s - errno=%d",
			strerror (errno), errno);
		return (IMS_FATAL);
	}

	/* initialize */
	errno = 0;

	return (IMS_OK);
} /* end of v0_handler__send */

/***********************************************************************
**
** v0_handler__get - receive msg structure on messag_id socket.
**
***********************************************************************/
int v0_handler__get (IMS_MSG_STRUCT *msgDesc,
	AGGREGATE *msgTree, int msgId)
{
	int count;
	int status;

	/* initialize */
	count = 0;

	/*
	** initialize root structures for receive msg.
	*/

	*msgTree = (AGGREGATE) NULL;

	if ((*msgTree = NewAggregate(NULL, KA_GROUP, "root", NULL))
			== (AGGREGATE)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_handler__get: creating root aggregate failed.");
		return (IMS_FATAL);
	}

	/* initialize */
	errno = 0;

	while ((status = IK_RxODL (msgId, msgTree)) < 0) 
	{
		if (errno == EWOULDBLOCK) 
		{
			/* set timeout to 10 minutes, i.e 2 sec * 300 = 600 sec */
			if (++count > 300)
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
					"v0_handler__get: Timeout waiting for message.");
				return (IMS_FATAL);
			}
		  (void) sleep (2);
		  errno = 0;
		  continue;
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
		  	"v0_handler__get: IK_RxODL failed with errno = %d - %s",
			  errno, strerror (errno));
			return (IMS_FATAL);
		}
	}

#ifdef ODEBUG
	printf ("\n****** ODL received from V0 client  *********\n");
	PrintLabel (*msgTree);
#endif

	/* initialize */
	errno = 0;

	return (IMS_OK);
} /* end of v0_handler__get */

/************************************************************************
**
** __v0Desc_init - Initialize fields in v0Desc 
**
************************************************************************/
void v0_handler__v0Desc_init    (IMS_MSG_STRUCT *msgDesc, AGGREGATE RxTree,
	V0_MSG_TYPE msgType, int msgId, V0_DESC_STRUCT *v0Desc,
	char *glbLogin, char *glbPassword)
{

	v0Desc->msgId = msgId;
	v0Desc->msgDesc = msgDesc;

	v0Desc->catReq.qDesc = (IMS_QI_DESC_OBJ *)NULL; 

	v0Desc->RxType = msgType;
	v0Desc->RxTree = RxTree;

	v0Desc->TxType = V0_UNIDENTIFIED;
	v0Desc->TxTree = (AGGREGATE)NULL;

	v0Desc->odl_status[0] = '\0';
	strcpy(v0Desc->odl_status, "00");

	v0Desc->message_id[0] = '\0';
	v0Desc->message_name[0] = '\0';

	/*
	** initialize search related fields.
	*/
	v0Desc->request.callerFlag   = 0;
	v0Desc->request.dataset_id   = (V0_VALUE_LIST *)NULL; 
	v0Desc->request.sensor       = (V0_VALUE_LIST *)NULL;
	v0Desc->request.platform     = (V0_VALUE_LIST *)NULL;
	v0Desc->request.parameter    = (V0_VALUE_LIST *)NULL;
	v0Desc->request.campaign     = (V0_VALUE_LIST *)NULL;
	v0Desc->request.process_level= (V0_VALUE_LIST *)NULL;
	v0Desc->request.granule_id= (V0_VALUE_LIST *)NULL;

	v0Desc->request.browse_p[0]        = '\0';
	v0Desc->request.start_time[0]      = '\0';
	v0Desc->request.start_time_mmdd[0] = '\0';
	v0Desc->request.start_time_year[0] = '\0';
	v0Desc->request.end_time[0]        = '\0';
	v0Desc->request.end_time_mmdd[0]   = '\0';
	v0Desc->request.end_time_year[0]   = '\0';

	v0Desc->request.start_day_of_year= 0; 
	v0Desc->request.stop_day_of_year = 0;

	v0Desc->request.day_night[0]     = '\0';
	v0Desc->request.granule_limit    = 0;
	v0Desc->request.select_all = 1;   /* select all is initially set to true 
                                        the value may be changed during parsing*/
	v0Desc->request.global_granules_p = 'N';
	v0Desc->request.global_search     = 'N';
	v0Desc->request.region_type       = V0_UNIDENTIFIED_REGION;
	v0Desc->request.pole_included     = ' '; /* has to be initiated to ' ' */
	v0Desc->request.mode[0]           = '\0';
	v0Desc->request.site_name[0]      = '\0';
	v0Desc->request.map_projection_type = NO_PROJECTION;

	v0Desc->request.tangent_latitude = 0.0;
	v0Desc->request.tangent_longitude= 0.0;

	/*
	** Point or Polygon user input coordinates storage array. 
	** Only the firstelement is initialized
	*/
	v0Desc->request.u_lat[0]         = INFINITY;  
	v0Desc->request.u_lon[0]         = INFINITY;

	/*
	** Range_Loc (i.e. Rectangular search on map or form, or 
	** Point & Range search on form, user input coordinates storage variables.
	*/
	v0Desc->request.u_min_lat        = INFINITY;
	v0Desc->request.u_max_lat        = INFINITY;
	v0Desc->request.u_min_lon        = INFINITY;
	v0Desc->request.u_max_lon        = INFINITY;

	/*
	** search area boundaries are for internal use
	*/
	v0Desc->request.minimum_latitude = INFINITY;
	v0Desc->request.maximum_latitude = INFINITY;
	v0Desc->request.minimum_longitude= INFINITY;
	v0Desc->request.maximum_longitude= INFINITY;
	v0Desc->request.cross_180 = 0;
	v0Desc->request.north_pole_only = 0;
	v0Desc->request.south_pole_only = 0;


	/*
	** initialize order related fields
	*/

	v0Desc->request.order_request_id[0] = '\0';
	v0Desc->request.user_email[0]       = '\0';
	v0Desc->request.data_center_id      = (V0_VALUE_LIST *)NULL;
	v0Desc->request.order_id            = 0;
	v0Desc->request.item_id		    = 0;
	v0Desc->request.total_cost          = 0.0;
	v0Desc->request.item_count          = 0;
	v0Desc->request.authenticator[0]    = '\0';
	v0Desc->request.user_id[0]          = '\0';
	v0Desc->request.priority            = 0;
	v0Desc->request.dar_priority        = 0;
	v0Desc->request.dar_status	    = 0;
	v0Desc->request.rate_multiplier     = 0.0;
	v0Desc->request.resource_type       = 0;
	v0Desc->request.account_id[0]       = '\0';
	v0Desc->request.op_validate_p[0]    = 'N';
	v0Desc->request.quicklook_p[0]      = 'N';
	v0Desc->request.observation_num     = 0;
	v0Desc->request.observation_freq[0] = '\0';
	v0Desc->request.active_p[0]         = '\0';
	v0Desc->request.activity_start_date[0]= '\0';
	v0Desc->request.activity_end_date[0]  = '\0';
	v0Desc->request.dar_comment[0]      = '\0';

	v0Desc->request.user_affiliation.category[0] = '\0';
	v0Desc->request.user_affiliation.type[0]     = '\0';

	v0Desc->request.contact_p  = 0;
	v0Desc->request.shipping_p = 0;
	v0Desc->request.billing_p  = 0;
	v0Desc->request.user_aborted_p  = 0;

	v0Desc->request.contact_profile.title[0]        = '\0';
	v0Desc->request.contact_profile.last_name[0]    = '\0';
	v0Desc->request.contact_profile.first_name[0]   = '\0';
	v0Desc->request.contact_profile.initial_name[0] = '\0';
	v0Desc->request.contact_profile.organization[0] = '\0';
	v0Desc->request.contact_profile.street[0]       = '\0';
	v0Desc->request.contact_profile.city[0]         = '\0';
	v0Desc->request.contact_profile.state[0]        = '\0';
	v0Desc->request.contact_profile.zipcode[0]      = '\0';
	v0Desc->request.contact_profile.country[0]      = '\0';
	v0Desc->request.contact_profile.phone[0]        = '\0';
	v0Desc->request.contact_profile.fax[0]          = '\0';
	v0Desc->request.contact_profile.email[0]        = '\0';

	v0Desc->request.shipping_profile.title[0]       = '\0';
	v0Desc->request.shipping_profile.last_name[0]   = '\0';
	v0Desc->request.shipping_profile.first_name[0]  = '\0';
	v0Desc->request.shipping_profile.initial_name[0]= '\0';
	v0Desc->request.shipping_profile.organization[0]= '\0';
	v0Desc->request.shipping_profile.street[0]      = '\0';
	v0Desc->request.shipping_profile.city[0]        = '\0';
	v0Desc->request.shipping_profile.state[0]       = '\0';
	v0Desc->request.shipping_profile.zipcode[0]     = '\0';
	v0Desc->request.shipping_profile.country[0]     = '\0';
	v0Desc->request.shipping_profile.phone[0]       = '\0';
	v0Desc->request.shipping_profile.fax[0]         = '\0';
	v0Desc->request.shipping_profile.email[0]       = '\0';

	v0Desc->request.billing_profile.title[0]        = '\0';
	v0Desc->request.billing_profile.last_name[0]    = '\0';
	v0Desc->request.billing_profile.first_name[0]   = '\0';
	v0Desc->request.billing_profile.initial_name[0] = '\0';
	v0Desc->request.billing_profile.organization[0] = '\0';
	v0Desc->request.billing_profile.street[0]       = '\0';
	v0Desc->request.billing_profile.city[0]         = '\0';
	v0Desc->request.billing_profile.state[0]        = '\0';
	v0Desc->request.billing_profile.zipcode[0]      = '\0';
	v0Desc->request.billing_profile.country[0]      = '\0';
	v0Desc->request.billing_profile.phone[0]        = '\0';
	v0Desc->request.billing_profile.fax[0]          = '\0';
	v0Desc->request.billing_profile.email[0]        = '\0';


	v0Desc->request.user_info.first_name[0] = '\0';
	v0Desc->request.user_info.last_name[0]  = '\0';

	v0Desc->request.asc_desc[0]   = '\0';
	v0Desc->request.spatial_type  = 0;
	v0Desc->request.center_lat    = INFINITY;
	v0Desc->request.center_lon    = INFINITY;
	v0Desc->request.radius        = INFINITY;
	v0Desc->request.nw_lat        = INFINITY;
	v0Desc->request.nw_lon        = INFINITY;
	v0Desc->request.ne_lat        = INFINITY;
	v0Desc->request.ne_lon        = INFINITY;
	v0Desc->request.se_lat        = INFINITY;
	v0Desc->request.se_lon        = INFINITY;
	v0Desc->request.sw_lat        = INFINITY;
	v0Desc->request.sw_lon        = INFINITY;

	v0Desc->request.line_item_list  = (V0_LINE_ITEM_LIST *)NULL;
	v0Desc->request.curr_line_item  = (V0_LINE_ITEM_LIST *)NULL;

	/*
	** initialize v0Desc->result fields.
	*/
	v0Desc->result.dataset_list    = (V0_DATASET_LIST *)NULL;
	v0Desc->result.dataset_count   = 0;
	v0Desc->result.curr_dataset    = (V0_DATASET_LIST *)NULL;
	v0Desc->result.first_chunk_sent   = 0;
	v0Desc->result.curr_dataset_count = 0;
	v0Desc->result.curr_granule       = (V0_GRANULE_LIST *)NULL;
	v0Desc->result.curr_granule_count = 0;
	v0Desc->result.last_dataset_flag  = 0;
	v0Desc->result.last_msg_flag      = 0; /* this flag will be set either
															when both last_dataset_flag
															is true and curr_granule is
															null, or end of processing 
															all datasets */
	v0Desc->result.previous_ds_idx  = 0;
	v0Desc->result.contact_info     = (V0_CONTACT_INFO *)NULL;
	v0Desc->result.user_id[0]       = '\0';
	v0Desc->result.order_id         = 0;
	v0Desc->result.directory[0]     = '\0';
	v0Desc->result.dataset_comment[0]     = '\0';
	v0Desc->result.dataset_restriction[0]     = '\0';
	v0Desc->result.dataset_info_list1      = (V0_VALUE_LIST *)NULL;
	v0Desc->result.dataset_info_list2      = (V0_VALUE_LIST *)NULL;
	v0Desc->result.user_list               = (V0_VALUE_LIST *)NULL;
	v0Desc->result.odl_status_code_comment = (V0_ERR_LIST *)NULL;
	v0Desc->result.no_match_found   = 0;
	v0Desc->result.dar_query_result.number_of_items = 0;
	v0Desc->result.dar_query_result.curr_dar_item =
			(V0_DAR_RESULT_LIST *)NULL;
	v0Desc->result.dar_query_result.dar_result_list =
			(V0_DAR_RESULT_LIST *)NULL;


	/*
	** setup database login, password, database, and server names
	*/
	v0Desc->catReq.msgDesc = msgDesc; 
	v0Desc->catReq.userSpec.dbUserName = glbLogin;
	v0Desc->catReq.userSpec.dbPassword = glbPassword;
	v0Desc->catReq.userSpec.server  = V0_SERVER;
	v0Desc->catReq.userSpec.dbName  = V0_DBNAME;
	v0Desc->catReq.userSpec.program = V0_PROGRAM;

	return;
} /* end of v0_handler__v0Desc_init */

/************************************************************************
**
** __v0Desc_cleanup - Cleanup all allocated spaces for v0Desc structure.
**
************************************************************************/
void v0_handler__v0Desc_cleanup (V0_DESC_STRUCT *v0Desc)
{
	V0_VALUE_LIST 		*h_ptr, *t_ptr;
	V0_GRANULE_LIST 	*hg_ptr, *tg_ptr;
	V0_KEYWORD_LIST 	*hk_ptr, *tk_ptr;
	V0_DATASET_LIST 	*hd_ptr, *td_ptr;
	V0_USER_ACCT_LIST 	*hu_ptr, *tu_ptr;
	V0_LINE_ITEM_LIST 	*hl_ptr, *tl_ptr;
	V0_ERR_LIST       	*he_ptr, *te_ptr;
	V0_CONTACT_INFO 	*hc_ptr, *tc_ptr;
	V0_DAR_RESULT_LIST	*hr_ptr, *tr_ptr;
	V0_DAR_FRAME_LIST	*hf_ptr, *tf_ptr;

	/*
	** cleanup v0Desc->request 
	*/


	if (v0Desc->result.user_list != (V0_VALUE_LIST *)NULL)
	{
		h_ptr=v0Desc->result.user_list;

		while (h_ptr!=(V0_VALUE_LIST *)NULL)
		{
			t_ptr = h_ptr->next_p;
			free (h_ptr);
			h_ptr=t_ptr;
		}
	}
	v0Desc->result.user_list = (V0_VALUE_LIST *)NULL;
	
	/* dataset_id */
	if (v0Desc->request.dataset_id != (V0_VALUE_LIST *)NULL )
	{
		h_ptr=v0Desc->request.dataset_id;
		while (h_ptr!=(V0_VALUE_LIST *)NULL)
		{
			t_ptr = h_ptr->next_p;
			free (h_ptr);
			h_ptr=t_ptr;
		}
	}
	v0Desc->request.dataset_id = (V0_VALUE_LIST *)NULL;

	/* sensor */
	if (v0Desc->request.sensor != (V0_VALUE_LIST *)NULL )
	{
		h_ptr=v0Desc->request.sensor;
		while (h_ptr!=(V0_VALUE_LIST *)NULL)
		{
			t_ptr = h_ptr->next_p;
			free (h_ptr);
			h_ptr=t_ptr;
		}
	}
	v0Desc->request.sensor = (V0_VALUE_LIST *)NULL;

	/* platform */
	if (v0Desc->request.platform != (V0_VALUE_LIST *)NULL )
	{
		h_ptr=v0Desc->request.platform;
	 	while (h_ptr!=(V0_VALUE_LIST *)NULL)
		{
			t_ptr = h_ptr->next_p;
			free (h_ptr);
			h_ptr=t_ptr;
		}
	}
	v0Desc->request.platform = (V0_VALUE_LIST *)NULL;

	/* parameter */
	if (v0Desc->request.parameter != (V0_VALUE_LIST *)NULL )
	{
		h_ptr=v0Desc->request.parameter;
		while (h_ptr!=(V0_VALUE_LIST *)NULL)
		{
			t_ptr = h_ptr->next_p;
			free (h_ptr);
			h_ptr=t_ptr;
		}
	}
	v0Desc->request.parameter = (V0_VALUE_LIST *)NULL;

	/* campaign */
	if (v0Desc->request.campaign != (V0_VALUE_LIST *)NULL )
	{
		h_ptr=v0Desc->request.campaign;
		while (h_ptr!=(V0_VALUE_LIST *)NULL)
		{
			t_ptr = h_ptr->next_p;
			free (h_ptr);
			h_ptr=t_ptr;
		}
	}
	v0Desc->request.campaign = (V0_VALUE_LIST *)NULL;

	/* process_level */
	if (v0Desc->request.process_level != (V0_VALUE_LIST *)NULL )
	{
		h_ptr=v0Desc->request.process_level;
		while (h_ptr!=(V0_VALUE_LIST *)NULL)
		{
			t_ptr = h_ptr->next_p;
			free (h_ptr);
			h_ptr=t_ptr;
		}
	}
	v0Desc->request.process_level = (V0_VALUE_LIST *)NULL;

	/* granule_id */
	if (v0Desc->request.granule_id != (V0_VALUE_LIST *)NULL )
	{
		h_ptr=v0Desc->request.granule_id;
		while (h_ptr!=(V0_VALUE_LIST *)NULL)
		{
			t_ptr = h_ptr->next_p;
			free (h_ptr);
			h_ptr=t_ptr;
		}
	}
	v0Desc->request.granule_id = (V0_VALUE_LIST *)NULL;

	/* data_center_id */
	if (v0Desc->request.data_center_id != (V0_VALUE_LIST *)NULL ) 
	{ 
		h_ptr = v0Desc->request.data_center_id;
		while (h_ptr != (V0_VALUE_LIST *)NULL)
		{
			t_ptr = h_ptr->next_p;
			free (h_ptr);
			h_ptr = t_ptr;
		}
	}
	v0Desc->request.data_center_id = (V0_VALUE_LIST *)NULL;


	/* line_item_list */
	if (v0Desc->request.line_item_list != (V0_LINE_ITEM_LIST *)NULL ) 
	{ 
		hl_ptr = v0Desc->request.line_item_list;
		while (hl_ptr != (V0_LINE_ITEM_LIST *)NULL)
		{
			tl_ptr = hl_ptr->next_p;
			free (hl_ptr);
			hl_ptr = tl_ptr;
		}
	}
	v0Desc->request.line_item_list = (V0_LINE_ITEM_LIST *)NULL;

	/*
	** cleanup v0Desc->result
	*/


	if (v0Desc->result.dataset_list != (V0_DATASET_LIST *)NULL )	
	{
		hd_ptr = v0Desc->result.dataset_list;

		while (hd_ptr != (V0_DATASET_LIST *) NULL)
		{

			td_ptr = hd_ptr->next_p;

			if (hd_ptr->parameter != (V0_VALUE_LIST *)NULL )
			{
				h_ptr=hd_ptr->parameter;
				while (h_ptr!=(V0_VALUE_LIST *)NULL)
				{
					t_ptr = h_ptr->next_p;
					free (h_ptr);
					h_ptr=t_ptr;
				}
			}
			hd_ptr->parameter = (V0_VALUE_LIST *)NULL;
	
			if (hd_ptr->temporal_key_list != (V0_KEYWORD_LIST *)NULL)
			{
				hk_ptr=hd_ptr->temporal_key_list;
				while (hk_ptr!=(V0_KEYWORD_LIST *)NULL)
				{
					tk_ptr = hk_ptr->next_p;
					free (hk_ptr);
					hk_ptr=tk_ptr;
				}
			}
			hd_ptr->temporal_key_list = (V0_KEYWORD_LIST *)NULL;
	
			if (hd_ptr->spatial_key_list != (V0_KEYWORD_LIST *)NULL)
			{
				hk_ptr=hd_ptr->spatial_key_list;
				while (hk_ptr!=(V0_KEYWORD_LIST *)NULL)
				{
					tk_ptr = hk_ptr->next_p;
					free (hk_ptr);
					hk_ptr=tk_ptr;
				}
			}
			hd_ptr->spatial_key_list = (V0_KEYWORD_LIST *)NULL;
	
			if (hd_ptr->detail_key_list != (V0_KEYWORD_LIST *)NULL)
			{
				hk_ptr=hd_ptr->detail_key_list;
				while (hk_ptr!=(V0_KEYWORD_LIST *)NULL)
				{
					tk_ptr = hk_ptr->next_p;
					free (hk_ptr);
					hk_ptr=tk_ptr;
				}
			}
			hd_ptr->detail_key_list = (V0_KEYWORD_LIST *)NULL;

			if (hd_ptr->account_list != (V0_USER_ACCT_LIST *)NULL)
			{
				hu_ptr=hd_ptr->account_list;
				while (hu_ptr!=(V0_USER_ACCT_LIST *)NULL)
				{


					he_ptr = hu_ptr->err_msg;
					while (he_ptr!=(V0_ERR_LIST *)NULL)
					{

						te_ptr = he_ptr->next_p;
						free (he_ptr);
						he_ptr=te_ptr;
					}

					tu_ptr = hu_ptr->next_p;
					free (hu_ptr);
					hu_ptr=tu_ptr;
				}
			}
			hd_ptr->account_list = (V0_USER_ACCT_LIST *)NULL;
	
			if (hd_ptr->granule_list != (V0_GRANULE_LIST *)NULL)
			{
				hg_ptr=hd_ptr->granule_list;

				while (hg_ptr!=(V0_GRANULE_LIST *)NULL)
				{
					tg_ptr = hg_ptr->next_p;

					h_ptr = hg_ptr->detailed_keyword;

					while (h_ptr != (V0_VALUE_LIST *)NULL)
					{
						t_ptr = h_ptr->next_p;
						free(h_ptr);
						h_ptr = t_ptr;
					}
					hg_ptr->detailed_keyword = (V0_VALUE_LIST *)NULL;

					h_ptr = hg_ptr->formatted_list;

					while (h_ptr != (V0_VALUE_LIST *)NULL)
					{
						t_ptr = h_ptr->next_p;
						free(h_ptr);
						h_ptr = t_ptr;
					}
					hg_ptr->formatted_list = (V0_VALUE_LIST *)NULL;

					free (hg_ptr);
					hg_ptr=tg_ptr;
				}
			}
			hd_ptr->granule_list = (V0_GRANULE_LIST *)NULL;
	
			if (hd_ptr->old_granule_list != (V0_GRANULE_LIST *)NULL)
			{
				hg_ptr=hd_ptr->old_granule_list;

				while (hg_ptr!=(V0_GRANULE_LIST *)NULL)
				{
					tg_ptr = hg_ptr->next_p;

					h_ptr = hg_ptr->detailed_keyword;

					while (h_ptr != (V0_VALUE_LIST *)NULL)
					{
						t_ptr = h_ptr->next_p;
						free(h_ptr);
						h_ptr = t_ptr;
					}
					hg_ptr->detailed_keyword = (V0_VALUE_LIST *)NULL;

					h_ptr = hg_ptr->formatted_list;

					while (h_ptr != (V0_VALUE_LIST *)NULL)
					{
						t_ptr = h_ptr->next_p;
						free(h_ptr);
						h_ptr = t_ptr;
					}
					hg_ptr->formatted_list = (V0_VALUE_LIST *)NULL;

					free (hg_ptr);
					hg_ptr=tg_ptr;
				}
			}
			hd_ptr->old_granule_list = (V0_GRANULE_LIST *)NULL;
	
			if (hd_ptr->unordered_granule_list != (V0_GRANULE_LIST *)NULL)
			{
				hg_ptr=hd_ptr->unordered_granule_list;

				while (hg_ptr!=(V0_GRANULE_LIST *)NULL)
				{
					tg_ptr = hg_ptr->next_p;

					h_ptr = hg_ptr->detailed_keyword;

					while (h_ptr != (V0_VALUE_LIST *)NULL)
					{
						t_ptr = h_ptr->next_p;
						free(h_ptr);
						h_ptr = t_ptr;
					}
					hg_ptr->detailed_keyword = (V0_VALUE_LIST *)NULL;

					h_ptr = hg_ptr->formatted_list;

					while (h_ptr != (V0_VALUE_LIST *)NULL)
					{
						t_ptr = h_ptr->next_p;
						free(h_ptr);
						h_ptr = t_ptr;
					}
					hg_ptr->formatted_list = (V0_VALUE_LIST *)NULL;

					free (hg_ptr);
					hg_ptr=tg_ptr;
				}
			}
			hd_ptr->unordered_granule_list = (V0_GRANULE_LIST *)NULL;
			free (hd_ptr);

			hd_ptr = td_ptr;
		} /* while hd_ptr is not null */

	} /* while v0Desc->result.dataset_list is not null */
		
	v0Desc->result.dataset_list = (V0_DATASET_LIST *)NULL;

	/* contact_info */
	if (v0Desc->result.contact_info != (V0_CONTACT_INFO *)NULL ) 
	{ 
		hc_ptr = v0Desc->result.contact_info;
		while (hc_ptr != (V0_CONTACT_INFO *)NULL)
		{
			tc_ptr = hc_ptr->next_p;
			free (hc_ptr);
			hc_ptr = tc_ptr;
		}
	}
	v0Desc->result.contact_info = (V0_CONTACT_INFO *)NULL;

	/* odl_status_code_comment */
	if (v0Desc->result.odl_status_code_comment != (V0_ERR_LIST *)NULL )
	{
		he_ptr=v0Desc->result.odl_status_code_comment;
		while (he_ptr!=(V0_ERR_LIST *)NULL)
		{
			te_ptr = he_ptr->next_p;
			free (he_ptr);
			he_ptr=te_ptr;
		}
	}
	v0Desc->result.odl_status_code_comment = (V0_ERR_LIST *)NULL;

	/* dar_query_result */
	if (v0Desc->result.dar_query_result.dar_result_list !=
		(V0_DAR_RESULT_LIST *) NULL)
	{
		hr_ptr = v0Desc->result.dar_query_result.dar_result_list;
		while (hr_ptr != (V0_DAR_RESULT_LIST *) NULL)
		{
			hf_ptr = hr_ptr->dar_frames.frame_list;
			while (hf_ptr != (V0_DAR_FRAME_LIST *) NULL)
			{
				tf_ptr = hf_ptr->next_p;
				free (hf_ptr);
				hf_ptr = tf_ptr;
			}
			tr_ptr = hr_ptr->next_p;
			free (hr_ptr);
			hr_ptr = tr_ptr;
		}
	}
	v0Desc->result.dar_query_result.dar_result_list = (V0_DAR_RESULT_LIST *) NULL;

	/*
	** remove tree structure 
	*/
	(void) v0_msgTree__destroy (v0Desc->RxTree);
	v0Desc->RxTree = (AGGREGATE)NULL; 
	(void) v0_msgTree__destroy (v0Desc->TxTree);
	v0Desc->TxTree = (AGGREGATE)NULL; 


	return;
} /* v0Desc_cleanup */

/***************************************************************************
**
** v0_updateMessageArchive
**
***************************************************************************/
static int
v0_updateMessageArchive (V0_DESC_STRUCT *v0Desc,
                         int		procStatus)
{
	V0_CAT_STRUCT   	*catReq = &(v0Desc->catReq);
	IMS_V0_MSG_STATUS	msgStatus;
	int			status, rollback_status;

	if (procStatus < IMS_OK)
	{
		msgStatus = V0_FAILED;	
	}
	else
	{
		msgStatus = V0_COMPLETED;
	}
        v0Desc->query.sql[0] = '\0';
        (void) sprintf(v0Desc->query.sql,
                "update v0_message_archive set status = %d"
		" where message_name = '%s'", msgStatus, v0Desc->message_name);
 
        catReq->item[0] = (void *) v0Desc->query.sql;
 
        if ((status = v0_cat (catReq, V0_BEGINTRANSACTION)) < IMS_OK)
        {
                (void) ims_msg (v0Desc->msgDesc, status,
                        "v0_updateMessageArchive: "
                        "failed to begin transaction.");
                return (status);
        }
 
        if ((status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
        {
                (void) ims_msg (v0Desc->msgDesc, status,
                        "v0_updateMessageArchive: "
                        "failed to update record in the database.");
                if ((rollback_status = v0_cat(catReq,V0_ROLLBACKTRANSACTION))
                        < IMS_OK)
                {
                        return (rollback_status);
                }
                return (status);
        }
 
        if ((status = v0_cat (catReq, V0_COMMITTRANSACTION)) < IMS_OK)
        {
                (void) ims_msg (v0Desc->msgDesc, status,
                        "v0_updateMessageArchive: "
                        "failed to commit transaction.");
                if ((rollback_status = v0_cat(catReq,V0_ROLLBACKTRANSACTION))
                        < IMS_OK)
                {
                        return (rollback_status);
                }
                return (status);
        }
 
        return (IMS_OK);
 
} /* v0_updateMessageArchive */


/***************************************************************************
**
** v0_insertMessageArchive
**
***************************************************************************/
static int
v0_insertMessageArchive (V0_DESC_STRUCT *v0Desc, 
			 char 		archiveFlag,
			 char		*glbArchiveDir)
{
	V0_CAT_STRUCT	*catReq = &(v0Desc->catReq);
	int		status, rollback_status;
	char		msgType[IMS_COL20_LEN+1];
	char		message_id[IMS_COL30_LEN+1];
	IMS_V0_MSG_STATUS msgStatus;

	/* get the message_id */
	(void) v0_msgTree__get_message_id (v0Desc->msgDesc, v0Desc->RxTree,
					message_id);

	/* get the message_type */
	(void) v0_msgTree__get_message_type (v0Desc->msgDesc, v0Desc->RxTree,
					   msgType);

	msgStatus = V0_RECEIVED;

        v0Desc->query.sql[0] = '\0';
	if (glbArchiveDir != (char *) NULL)
	{
	        (void) sprintf(v0Desc->query.sql,
                "insert v0_message_archive (message_name, message_id, "
		"message_time," "message_type, path, archived_p, status) "
                "values ('%s', '%s', getdate(), '%s', '%s', '%c', %d)",
                v0Desc->message_name, message_id, msgType, glbArchiveDir,
		archiveFlag, msgStatus);
	}
	/* if message archiving is not enabled, then insert 'NULL' in path */ 
	else
	{
                (void) sprintf(v0Desc->query.sql,
                "insert v0_message_archive (message_name, message_id, "
                "message_time," "message_type, path, archived_p, status) "
                "values ('%s', '%s', getdate(), '%s', NULL, '%c', %d)",
                v0Desc->message_name, message_id, msgType, 
                archiveFlag, msgStatus);
	}

	catReq->item[0] = (void *) v0Desc->query.sql;

	if ((status = v0_cat (catReq, V0_BEGINTRANSACTION)) < IMS_OK)
	{
		(void) ims_msg (v0Desc->msgDesc, status,
			"v0_insertMessageArchive: "
			"failed to begin transaction.");
		return (status);
	}

	if ((status = v0_cat (catReq, V0_INSERTROW)) < IMS_OK)
	{
		(void) ims_msg (v0Desc->msgDesc, status, 
			"v0_insertMessageArchive: "
			"failed to insert into the database.");
		if ((rollback_status = v0_cat(catReq,V0_ROLLBACKTRANSACTION))
			< IMS_OK)
		{
			return (rollback_status);
		}
		return (status); 
	}

	if ((status = v0_cat (catReq, V0_COMMITTRANSACTION)) < IMS_OK)
	{
                (void) ims_msg (v0Desc->msgDesc, status,
                        "v0_insertMessageArchive: "
                        "failed to commit transaction.");

                if ((rollback_status = v0_cat(catReq,V0_ROLLBACKTRANSACTION))
                        < IMS_OK)
                {
                        return (rollback_status);
                }
		return (status);
	}

	return (IMS_OK);

} /* v0_insertMessageArchive */

/***************************************************************************
**
** v0_getArchiveFileName -
**
***************************************************************************/
static int 
v0_getArchiveFileName (V0_DESC_STRUCT *v0Desc)
{
        char 	*timeStamp;
        char 	msgType[IMS_COL10_LEN];
	time_t 	now ;
	struct tm *local_tm;
	int	year;
 

	timeStamp = (char *)ims_timeStamp();

	/* get current year since ims_timeStamp does not return year */
        now = time ((time_t *) 0);
        local_tm = localtime(&now);
	year = 1900 + local_tm->tm_year;
	free(local_tm);

	switch (v0Desc->RxType)
	{
		case V0_INVENTORY_SEARCH:
			strcpy(msgType, "INV");
			break;
		case V0_DIRECTORY_SEARCH:
			strcpy(msgType, "DIR");
			break;
		case V0_PRODUCT_REQUEST:
			strcpy(msgType, "PRQ");
			break;
		case V0_DAR_ACCT_SEARCH:
			strcpy(msgType, "DAS");
			break;
		case V0_DAR_REQUEST:
			strcpy(msgType, "DAR");
			break;
		case V0_DAR_LIST_QUERY:
			strcpy(msgType, "DLQ");
			break;
		case V0_DAR_CONTENT_QUERY:
			strcpy(msgType, "DCQ");
			break;
		case V0_DAR_STATUS_QUERY:
			strcpy(msgType, "DSQ");
			break;
		case V0_DAR_GRANULE_QUERY:
			strcpy(msgType, "DGQ");
			break;
		default:
			/* unexpected message type */
			free (timeStamp);
			return (IMS_ERROR);			
	}
	
	(void) sprintf (v0Desc->message_name, "%s_%d_%d%s", msgType, getpid(), 
			year, timeStamp);
	free (timeStamp);
	return (IMS_OK);

} /* v0_getArchiveFileName */

/***************************************************************************
**
** v0Message_Archive -  
**
***************************************************************************/
static int 
v0Message_Archive (V0_DESC_STRUCT *v0Desc, char *glbArchiveDir)
{
	char 	ArchiveFileName[IMS_PATH_LEN];
	FILE	*odlFile;
	char	archiveFlag = 'N';
	int	status;

	/* 
	** If message archiving is enabled then save the message in the
	** specified archive directory.  
	*/
	if (glbArchiveDir != (char *) NULL)
	{
		/* open the file to write the ODL message received 
		   from V0 client */
		(void) ims_concatFilePath (ArchiveFileName, glbArchiveDir, 
					   v0Desc->message_name);

		if ((odlFile = fopen (ArchiveFileName, "w")) == NULL)
		{
			(void)ims_msg(v0Desc->msgDesc, IMS_FATAL,
		          "v0Message_Archive: Unable to open file for writing "
				"ODL message");
		}
		else
		{
			/* save the message */
			(void) WriteLabel (odlFile , v0Desc->RxTree);
			(void) fclose (odlFile);
                        if ((chmod (ArchiveFileName, 0644)) == -1)
                        {
                                (void) ims_msg (v0Desc->msgDesc, IMS_ERROR,
                                "Could not change permission for ODL file %s",
                                ArchiveFileName);
                        }
			archiveFlag = 'Y';
		}
	}

	/*
	** Enter a record in the database for this message
	** (even if archiving is not enabled)
	*/
	status = v0_insertMessageArchive (v0Desc, archiveFlag, glbArchiveDir);

	/* return the status received from v0_insertMessageArchive to the caller */
	return (status);

} /* v0Message_Archive */
