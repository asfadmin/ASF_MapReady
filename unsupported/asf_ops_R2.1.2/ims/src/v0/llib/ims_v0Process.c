static char *sccs = "@(#)ims_v0Process.c	5.13  09/05/97";

/************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** ims_v0Process.c
**
** Purpose
**		Processes requests, and transmits results.
**
**	Creator   :   Julie Wang
**
**	Date      :   June 15, 1994
**
** Modifications:
**
**   04/22/97    jwang   Added special wildcard handling for granule id srch.
**
**   04/18/97    jwang   Granule id search. 
**   
**   07/24/96    jwang   Removed logging of 'no match found' for Dir/Inv srch.
**   
**   07/24/96    jwang   Removed affliation information collection (PR 977)
**   
**   07/24/96    jwang   Changed error message for non-registered users.
**   
**   07/24/96    jwang   Process_level datatype change.
**   
**   07/24/96    jwang   User abort handling, for Inventory Srch only.
**   
**   06/06/96    jwang   PR771 fix address length checking
**
**   06/06/96    jwang   PR907 quicklook fix
**
**   06/06/96    jwang   Allow quicklook in SCAN ITEM
**
**   05/24/96    jwang   PR 771, 849 
**
**   04/03/96    jwang   Added code to handle detail metadata collection
**
**   02/20/96    jwang   R1Bprime (preliminary).
**
**   01/22/96    jwang   Changes added to handle DARs and SCAN requests
**
**   10/31/95    jwang   Added billing enhancement feature
**
**   09/25/95    jwang   convert string inputs into uppercase, which
**                       is how IMS catalog store string data.  Exceptions
**                       are package_id, title, address, and email.
**
**   09/05/95    jwang   IMS_NUMERIC_DATE  conversion
**
**   07/28/95    jwang   V3.10 schema changes
**
**   06/05/95    jwang   IMS front-end order processing
**
**   06/05/95    jwang   Changed all reference of cdb_ or CDB_ to 
**                       ims_ or IMS_
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
#include <ims_v0.h>
#include <ims_timeConv.h>

/* static routines */
static int retrieve_ds_list (V0_DESC_STRUCT *);
static int retrieve_granules_list (V0_DESC_STRUCT *);
static int retrieve_parameter_list (V0_DESC_STRUCT *);
static int retrieve_account_list (V0_DESC_STRUCT *);
static int retrieve_dataset_info (V0_DESC_STRUCT *);
static int remove_newlines (char *);
static int build_dlink_list (V0_DESC_STRUCT *);
static int init_line_item_list (V0_LINE_ITEM_LIST *); 
static int format_detail_info(IMS_MSG_STRUCT *, V0_GRANULE_LIST *);
static int prepare_keyword_string (V0_VALUE_LIST *);
static int prepare_wildcard_string (V0_VALUE_LIST *);

/*************************************************************************
** 
** v0_process -
**
** purpose: Process requests submitted by V0 end users 
**
** called from: v0_handler
**
** return  IMS_OK     successful
**         IMS_ERROR  input or user error
**         IMS_FATAL  system error
**************************************************************************/

int v0_process (IMS_MSG_STRUCT *msgDesc, V0_DESC_STRUCT *v0Desc)
{
	int status;                   
	AGGREGATE newMsg;             /* any new message comes in after receiving
                                    the original request, i.e v0Desc->RxTree */
	V0_MSG_TYPE newMsgType;       /* type of the new msg */
	V0_RESULT_STRUCT *result;
	V0_DATASET_LIST *curr_dataset;
	char msgbuf[IMS_COL30_LEN+1]; 
	int begin_count;

	/*
	** Initialize local variables
	*/
	result = &(v0Desc->result);
	newMsg = (AGGREGATE)NULL;
	newMsgType = V0_UNIDENTIFIED;

	/* all DAR queries are processed in ims_v0Dar */
	if ( (v0Desc->RxType == V0_DAR_LIST_QUERY) ||
	     (v0Desc->RxType == V0_DAR_STATUS_QUERY) ||
 	     (v0Desc->RxType == V0_DAR_CONTENT_QUERY) ||
	     (v0Desc->RxType == V0_DAR_GRANULE_QUERY) )
	{
		status = v0_dar__process_query(msgDesc, v0Desc);
		return (status);
	}

	if ( (v0Desc->RxType == V0_PRODUCT_REQUEST) ||
			 (v0Desc->RxType == V0_DAR_ACCT_SEARCH) ||
			 (v0Desc->RxType == V0_DAR_REQUEST) )
	{
		
		if ( (status = v0_order__place_order (msgDesc, v0Desc, 1)) < IMS_OK) 
		{
			if ( v0Desc->RxType == V0_PRODUCT_REQUEST )
			{
                                /*
                                ** Only return to the caller if FATAL error
                                ** occur - a QUIT message will be returned to
                                ** the client by the caller.
                                */
				if (status == IMS_FATAL)
				{
					strcpy (v0Desc->odl_status, "19");
					return (status);
				}
                                /*
                                ** For other kind of errors, v0_order__place_order
                                ** should have filled in the status_code
                                ** and the status_code_comment appropriately.
                                ** Go on and create the PRODUCT_RESULT message
                                ** to return to the client.
                                */
			}

			else if ( (v0Desc->RxType == V0_DAR_ACCT_SEARCH) ||
				(v0Desc->RxType == V0_DAR_REQUEST))
			{
				/* 
				** if error occurred and odl_status has not been assigned, most likely
				** it a fatal error, such as memory allocation error when interface
				** with qi library.  Therefore, 03 - system error for DARs - is given
				*/
				if ( strcmp (v0Desc->odl_status, "00") == 0)
				{
					strcpy (v0Desc->odl_status, "03");
				}

				return (status);
			}
	
		} 
                if (status == IMS_OK)
                {
                        strcpy (v0Desc->odl_status, "01");
                }

		if (v0Desc->RxType == V0_PRODUCT_REQUEST)
		{
			if (status=(v0_msgTree__create (msgDesc, v0Desc->RxTree, 
				&(v0Desc->TxTree), V0_PRODUCT_RESULT, v0Desc->odl_status,
				(V0_RESULT_STRUCT *)&v0Desc->result, 0)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_process: Failed to create product_result msg for transmit.");
	
				(void) v0_msgTree__destroy (v0Desc->TxTree);
				v0Desc->TxTree = (AGGREGATE)NULL;
	
				/* Set status field to indicate system error */
				strcpy (v0Desc->odl_status, "19"); 
	
				return (status);
			}

		}

		else if (v0Desc->RxType == V0_DAR_ACCT_SEARCH)
		{
			if (status=(v0_msgTree__create (msgDesc, v0Desc->RxTree, 
				&(v0Desc->TxTree), V0_DAR_ACCT_RESULT, v0Desc->odl_status,
				(V0_RESULT_STRUCT *)&v0Desc->result, 0)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_process: Failed to create dar_acct_result msg for transmit.");
	
				(void) v0_msgTree__destroy (v0Desc->TxTree);
				v0Desc->TxTree = (AGGREGATE)NULL;
	
				/* Set status field to indicate system error */
				strcpy (v0Desc->odl_status, "03"); 
	
				return (status);
			}

		}

		else if (v0Desc->RxType == V0_DAR_REQUEST)
		{
			if (status=(v0_msgTree__create (msgDesc, v0Desc->RxTree, 
				&(v0Desc->TxTree), V0_DAR_RESULT, v0Desc->odl_status,
				(V0_RESULT_STRUCT *)&v0Desc->result, 0)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_process: Failed to create dar_acct_result msg for transmit.");
	
				(void) v0_msgTree__destroy (v0Desc->TxTree);
				v0Desc->TxTree = (AGGREGATE)NULL;
	
				/* Set status field to indicate system error */
				strcpy (v0Desc->odl_status, "03"); 
	
				return (status);
			}

		}

		if (status = (v0_handler__send (msgDesc, v0Desc->TxTree, 
			v0Desc->msgId)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_process: failed to send Product Request or DAR Request result.");
	
			(void) v0_msgTree__destroy (v0Desc->TxTree);
			v0Desc->TxTree = (AGGREGATE)NULL;
	
			if (v0Desc->RxType == V0_PRODUCT_REQUEST)
			{
				strcpy (v0Desc->odl_status, "19");
			}

			else if ( (v0Desc->RxType == V0_DAR_ACCT_SEARCH) ||
								(v0Desc->RxType == V0_DAR_REQUEST))
			{
				strcpy (v0Desc->odl_status, "03");
			}
	
			return (status);
		}
	

#		ifdef ODEBUG
			printf("\n********** ODL returned to V0 client ************\n");
			PrintLabel (v0Desc->TxTree);
#		endif

		(void) v0_msgTree__destroy (v0Desc->TxTree);
		v0Desc->TxTree = (AGGREGATE)NULL;

		return (IMS_OK);
	}


	/*
	** call v0_process__parse_RxTree to fill the v0Desc with search specs
	** received from the odl message 
	**
	** possible returned odl_status are:
	**
	**    03    system error (for DAR only)
	**    09    bad message, syntax error occurred
	**    10    non-supported feature
	**    12    search too wide (inventory search only)
	**    17    invalid message, protocol error 
	**    19    memory allocation error
	**
	** a QUIT will be returned.  Their associating odl status has been 
	** assigned in the parsing function
	*/
	if ((status = v0_process__parse_RxTree (v0Desc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** calculate the min/max lat/lon and other spatial search factors
	*/
	if ((v0Desc->request.global_search != 'Y') &&
		 (v0Desc->request.global_granules_p != 'Y') )
	{
		if ( (status = v0_spatial__v0_boundaries (&v0Desc->request)) < IMS_OK)
		{
			if (status == -1)
			{

				(void) ims_msg (v0Desc->msgDesc, IMS_FATAL,
				"v0_process: unrecognizable region type.");
	
				strcpy (v0Desc->odl_status, "17");
				return (IMS_FATAL);
			}
			
			else if (status == -2)
			{

				(void) ims_msg (v0Desc->msgDesc, IMS_FATAL,
				"v0_process: invalid coordinate value.");
	
				strcpy (v0Desc->odl_status, "09");
				return (IMS_FATAL);
			}
			
		}
	}

	else 
	{
		v0Desc->request.maximum_latitude = 90.0;
		v0Desc->request.minimum_latitude = -90.0;
		v0Desc->request.maximum_longitude = 180.0;
		v0Desc->request.minimum_longitude = -180.0;
	}

	switch (v0Desc->RxType)
	{
		case V0_INVENTORY_SEARCH:
			/*
			** retrieve a list of dataset idx that match the user spec.
			** if errors occurs during dataset list retrieval, or
			** nothing was found from the query execution, return a QUIT
			** with proper odl_status
			*/
			if ((status = retrieve_ds_list (v0Desc)) < IMS_OK)
			{
				/* odl_status has been assigned in retrieve_ds_list */
				return (status);
			}

			/*
			** no dataset found is OK
			*/
			if (v0Desc->result.dataset_count == 0)
			{
				if (v0Desc->request.global_granules_p == 'Y')
				{
					strcpy (v0Desc->odl_status, "15");

#ifdef MDEBUG
					(void) ims_msg (msgDesc, IMS_INFO,
						"v0_process: no matching granules for global_granules_only request.");
#endif
				}
				else
				{
					strcpy (v0Desc->odl_status, "02"); 
#ifdef MDEBUG
					(void) ims_msg (msgDesc, IMS_INFO,
						"v0_process: no matching dataset found in inventory search.");
#endif
				}

				v0Desc->result.no_match_found = 1;
				return (IMS_OK);
			}

			/*
			** build the doubly link list, prev_p is needed for INVENTORY_SEARCH 
			** only
			*/
			(void) build_dlink_list (v0Desc);

			/*
			** begin to process the  the rest of the inventory search against 
			** granule tables. 
			**
			** The processing rules are:
			**
			**  1.If number of granules found is greater than the granule_
			**    limit specified by the user or the default, the returned  
			**    NUMBER_OF_GRANULES_HIT should be incremented by 1 so to
			**    notify the client on such condition.  See NSIDC v0 server 
			**    source code, function add_granule of irm.c
			**
			**  2. only the last chunk of result for each dataset contains 
			**     NUMBER_OF_GRANULES_HIT, the others will not contain this field.
			**
			**  3. only granules associating with the same dataset are put into
			**     the same result chunk
			** 
			**  4. NUMBER_OF_DATASETS is sent with the last chunk of granule
			**     result if the granules are associating with the last dataset,
			**     or sent separately if trailing dataset(s) dose not result with
			**     any granules.
			*/


			/* 
			** curr_dataset points to the dataset_list member
			** currently been processed 
			*/
			result->curr_dataset = result->dataset_list;
			result->previous_ds_idx = 0;
			result->last_msg_flag = 0;


			while ((result->curr_dataset != (V0_DATASET_LIST *)NULL ) &&
				(!result->last_msg_flag))
			{

				curr_dataset = result->curr_dataset;
				result->curr_granule = (V0_GRANULE_LIST *)NULL;

				if (curr_dataset->next_p == (V0_DATASET_LIST *)NULL)
				{
					/* last_dataset_flag is set to 1 when the last member 
						of dataset list is encountered */
					result->last_dataset_flag = 1;
				}

				curr_dataset->granule_count = 0;

				if (((status = retrieve_granules_list (v0Desc)) < IMS_OK) )
				{
					/* odl_status has been assigned in retrieve_granules_list */
					return (status);
				} 

				/* some granules were found */
				if ((curr_dataset->granule_count > 0) &&
					(curr_dataset->granule_list != (V0_GRANULE_LIST *)NULL))
				{


					/* 
					** prepare to return result for a new dataset 
					*/
					result->curr_granule = curr_dataset->granule_list;
					result->first_chunk_sent = 0;
					result->curr_granule_count = 0;
					curr_dataset->parameter = (V0_VALUE_LIST *)NULL;
					curr_dataset->account_list = (V0_USER_ACCT_LIST *)NULL;

					/*
					** retrieve associating parameter values of the dataset
					*/
					if ((status = retrieve_parameter_list (v0Desc)) < IMS_OK) 

					{
						/* odl_status has been assigned in the called functions */
						return (status);
					} 
	
					/*
					** retrieve a list of account id associating with the current user 
					*/
					if ((status = retrieve_account_list (v0Desc)) < IMS_OK)
					{
						/* odl_status has been assigned in the called functions */
						return (status);
					}

					/*
					** retrieve dataset comment and dataset restriction
					*/
					if ((status = retrieve_dataset_info (v0Desc)) <IMS_OK)
					{
						/* odl_status has been assigned in the called functions */
						return (status);
					}

					/* curr_dataset_count tracks the actual number of datasets
						containing granules */
					result->curr_dataset_count += 1;
				}
				else
				{
					if (result->last_dataset_flag)
					{
						result->last_msg_flag = 1;
					}
				}

				/* update previous_ds_idx with the latest dataset_idx */
				result->previous_ds_idx = curr_dataset->dataset_idx;

				/*
				** return serarch result and send back to the client  
				*/
				if ( (curr_dataset->granule_count > 0) || (result->last_msg_flag == 1))
				{
					do
					{
						/* for performance measuring use only - jlw
						begin_count = result->curr_granule_count;
						*/

						/*
						** build the result msg until reaches the maximum granules
						** for a chunk, or granule limit or end of granule list 
						*/
						strcpy (v0Desc->odl_status, "01");
	
						if (status=(v0_msgTree__create (msgDesc, v0Desc->RxTree, 
							&(v0Desc->TxTree), V0_INVENTORY_RESULT, v0Desc->odl_status,
							(V0_RESULT_STRUCT *)result, 
							v0Desc->request.granule_limit)) < IMS_OK)
						{
							(void) ims_msg (msgDesc, IMS_FATAL,
								"v0_process: Failed to create inventory_result msg.");
	
							(void) v0_msgTree__destroy (v0Desc->TxTree);
							v0Desc->TxTree = (AGGREGATE)NULL;

							strcpy (v0Desc->odl_status, "19");
							return (status);
						} 
	
						/*
						** Send an inventory search result chunk 
						*/
						if (status = (v0_handler__send (msgDesc, v0Desc->TxTree, 
							v0Desc->msgId)) < IMS_OK)
						{
							(void) ims_msg (msgDesc, IMS_FATAL,
								"v0_process: failed to send Inventory Search result.");
	
							(void) v0_msgTree__destroy (v0Desc->TxTree);
							v0Desc->TxTree = (AGGREGATE)NULL;

							strcpy (v0Desc->odl_status, "19");
							return (status);
						}

						/* for performance measuring use only - jlw 
						ims_msg (msgDesc, IMS_INFO, 
								"Returned a chunk of Inventory result for "
								"dataset %s, number of granules %d",
						curr_dataset->dataset_id, 
						(result->curr_granule_count - begin_count));
						*/

#						ifdef ODEBUG
							printf("\n********** ODL returned to V0 client ************\n");
							PrintLabel (v0Desc->TxTree);
#						endif

	
						(void) v0_msgTree__destroy (v0Desc->TxTree);
						v0Desc->TxTree = (AGGREGATE)NULL;

						/* 
						** wait for the ACKNOWLEDGE to come in before 
						** constructing the next result message. Stop the process 
						** if any interrupts or errors occurs.
						** 
						** use a temporary tree - newMsg - to receive the ACK, so
						** we do not lose the origianl RxTree
						** 
						** newMsg is sharing the same msgDesc with v0Desc->RxTree 
						*/
						if ((status = v0_handler__get (msgDesc, &newMsg, 
							v0Desc->msgId)) < IMS_OK)
						{
		
							errno = 0;
							strcpy (v0Desc->odl_status, "19");
							ims_msg (msgDesc, status, 
								"Failed to receive ACKNOWLEGE from V0.");
							(void) v0_msgTree__destroy (newMsg);
		
							return (status);
		
						} /* error(s) occurred while waiting for a new msg */
		
						else
						{
							/* a message was received. Identify the message type */
							newMsgType = v0_msgTree__identify(msgDesc, newMsg);
		
							/*
							** remove the new message, because all we are interested is
							** to find out the msg type 
							*/	
							(void) v0_msgTree__destroy (newMsg);
							msgbuf[0] = '\0';
			
							switch (newMsgType)
							{
								case V0_ACKNOWLEDGE:

									/* for performance measuring only - jlw
									ims_msg (msgDesc, IMS_INFO, "*** received an ACKNOWLEDGE");
									*/

#									ifdef DEBUG
										(void) ims_msg (msgDesc, IMS_INFO, 
										   "*** received an ACKNOWLEDGE");
#									endif

				 					break;
	
								case V0_ABORT:
								case V0_QUIT:

#ifdef MDEBUG
									(void) ims_msg (msgDesc, IMS_INFO, 
										    "*** received an ABORT or QUIT");
#endif
									strcpy (v0Desc->odl_status, "01");
									v0Desc->request.user_aborted_p = 1;
									return (IMS_OK);
									break;
	
								default:
									(void) ims_msg (msgDesc, IMS_FATAL,
										"v0_process: unknown message type received.");
									strcpy (v0Desc->odl_status, "17"); 
									return (IMS_FATAL);
									break;
							}
						}
					} while ( (!result->last_msg_flag) && 
					          (result->curr_granule != (V0_GRANULE_LIST *)NULL));
				}

				/* advance to the next dataset in the list */
				result->curr_dataset = result->curr_dataset->next_p;

			} /* while more in dataset list */

			/*
			** check if no granules found at all for any dataset  
			*/

			if ((result->curr_dataset_count <= 0) ||
				(result->dataset_list == (V0_DATASET_LIST *)NULL))
			{
				if (v0Desc->request.global_granules_p == 'Y')
				{
					strcpy (v0Desc->odl_status, "15");

#ifdef MDEBUG
					(void) ims_msg (msgDesc, IMS_INFO,
						"v0_process: no matching granules for global_granules_only request.");
#endif 
				}
				else
				{
					strcpy (v0Desc->odl_status, "02");

#ifdef MDEBUG
					(void) ims_msg (msgDesc, IMS_INFO,
						"v0_process: no matching granules found for any dataset");
#endif 
				}

				v0Desc->result.no_match_found = 1;
				return (IMS_OK);
				
			} 

			break;

		case V0_DIRECTORY_SEARCH:

			if ((status = retrieve_ds_list (v0Desc)) < IMS_OK)
			{
				/* odl_status has been assigned in retrieve_ds_list */
				return (status);
			}

			/*
			** no dataset found is OK
			*/
			if (v0Desc->result.dataset_count == 0)
			{
				if (v0Desc->request.global_granules_p == 'Y')
				{
					strcpy (v0Desc->odl_status, "15");

#ifdef MDEBUG
					(void) ims_msg (msgDesc, IMS_INFO,
						"v0_process: no matching granules for global_granules_only request.");
#endif
				}
				else
				{
					strcpy (v0Desc->odl_status, "02");
#ifdef MDEBUG
					(void) ims_msg (msgDesc, IMS_INFO,
						"v0_process: no matching dataset found.");
#endif
				}

				v0Desc->result.no_match_found = 1;
				return (IMS_OK);
			}


			/*
			** Build and send the result transmit tree
			*/
			strcpy (v0Desc->odl_status, "01");

			/*
			** create the directory search result tree
			*/
			if (status=(v0_msgTree__create (msgDesc, v0Desc->RxTree, 
				&(v0Desc->TxTree), V0_DIRECTORY_RESULT, v0Desc->odl_status,
				(V0_RESULT_STRUCT *)&v0Desc->result, 0)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_process: Failed to create directory_result msg for transmit.");

				(void) v0_msgTree__destroy (v0Desc->TxTree);
				v0Desc->TxTree = (AGGREGATE)NULL;

				/* Set status field to indicate system error */
				strcpy (v0Desc->odl_status, "19"); 

				return (status);
			}

			/*
			** Send the directory search result tree
			*/
			if (status = (v0_handler__send (msgDesc, v0Desc->TxTree, 
				v0Desc->msgId)) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_process: fail to send Directory Search result tree.");

				(void) v0_msgTree__destroy (v0Desc->TxTree);
				v0Desc->TxTree = (AGGREGATE)NULL;

				/* Set status field to indicate system error */
				strcpy (v0Desc->odl_status, "19"); 

				return (status);
			}

#			ifdef ODEBUG
				printf("\n********** ODL returned to V0 client ************\n");
				PrintLabel (v0Desc->TxTree);
#			endif

			(void) v0_msgTree__destroy (v0Desc->TxTree);
			v0Desc->TxTree = (AGGREGATE)NULL;


			break;

		default:
			(void) ims_msg (msgDesc, IMS_FATAL, "v0_process: Invalid msg type.");

			/* Set status field to indicate system error, we should not 
				reach this point since msgType has been filtered before
				this function was called */

			strcpy (v0Desc->odl_status, "17"); 

			return (IMS_FATAL);
	}   /* switch */

	return (IMS_OK);

} /* end of v0_process */


/*************************************************************************
**
** v0_process__parse_RxTree -
** 
** purpose: parse RxTree and fill search specifications into the
**          data structure V0_DESC_STRUCT, which will be utilized in  
**          creation of Inventory Search or Directory Search queries.
**          This routine also does neccessary conversion to reformat
**          search values to be internally recognizable. 
**
** called by: v0_process
**
** return  IMS_OK     successful
**         IMS_ERROR  input or user error
**         IMS_FATAL  system error
**
** possible return values of v0Desc.odl_status  
**          00  if successful 
**          09  if syntax error occurs
**          10  if the RxTree contains non-supported features
**          12  if the search is too broad (inventory search only)
**          17  if message invalid or protocol error
**          19  if memory allocation errors occurs
**
** Note: data type check applied to subsystem orders only, therefore, it's
**       not necessary to to create status code comment for errors
**
**************************************************************************/
int v0_process__parse_RxTree (V0_DESC_STRUCT *v0Desc)
{
	AGGREGATE input_msg;            /* temp buf of msg strings */
	AGGREGATE group;                /* temp buf of aggregate on odl*/
	ATTRIBUTE attribute;            /* temp buf of parameter on odl*/
	VALUE value_ptr;                /* temp buf of parameter value on odl*/ 
	IMS_MSG_STRUCT *msgDesc;        /* msg descriptor */
	V0_VALUE_LIST *track_ptr, *hv, *tv; 
	V0_REQUEST_STRUCT *request;
	V0_LINE_ITEM_LIST *line_item_ptr; /*ptr to track the line items */
	V0_CAT_STRUCT *catReq; 
	char msgbuf[IMS_COL1024_LEN+1]; /* temp text string */
	int  global_lat, global_lon;    /*factors to decide global_search flag*/
	int  count;
	char *s_ptr;
	char errbuf[IMS_COL255_LEN+1], v0_errbuf[IMS_COL255_LEN+1];
	int  status;
	char temp[IMS_COL1024_LEN+1];
	int  rowCount;
																	 

	/* 
	** Initialize local variables 
	*/
	request    = &v0Desc->request;
	input_msg  = (AGGREGATE)NULL;
	group      = (AGGREGATE)NULL;
	attribute = (ATTRIBUTE)NULL;
	value_ptr = (VALUE)NULL;
	track_ptr = (V0_VALUE_LIST *)NULL;
	line_item_ptr = (V0_LINE_ITEM_LIST *)NULL;

	msgDesc   = v0Desc->msgDesc;
	catReq = &v0Desc->catReq;

	global_lat = 1;
	global_lon = 1;

 	if ( ((input_msg=FindAggregate(v0Desc->RxTree,"INVENTORY_SEARCH")) == NULL) &&
		   ((input_msg=FindAggregate(v0Desc->RxTree,"DIRECTORY_SEARCH")) == NULL) &&
		   ((input_msg=FindAggregate(v0Desc->RxTree,"PRODUCT_REQUEST")) == NULL) &&
		   ((input_msg=FindAggregate(v0Desc->RxTree,"USER_ACCT_SEARCH")) == NULL) &&
		   ((input_msg=FindAggregate(v0Desc->RxTree,"DAR_REQUEST")) == NULL) &&
		   ((input_msg=FindAggregate(v0Desc->RxTree,"DAR_LIST_QUERY")) == NULL) &&
		   ((input_msg=FindAggregate(v0Desc->RxTree,"DAR_STATUS_QUERY")) == NULL) &&
		   ((input_msg=FindAggregate(v0Desc->RxTree,"DAR_CONTENT_QUERY")) == NULL) &&
		   ((input_msg=FindAggregate(v0Desc->RxTree,"DAR_GRANULE_QUERY")) == NULL) &&
	           ((input_msg=FindAggregate(v0Desc->RxTree,"SCAN_REQUEST")) == NULL) )
	{
		/* invalid message type, return quit with status of 17
		*/
		strcpy(v0Desc->odl_status, "17");

		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_process__parse_RxTree:Bad message, return a QUIT with status 17.");

		return (IMS_FATAL);

	}

	/*
	** start parsing the RxTree and filling values into v0Desc structure 
	*/

        /* message_id */
        attribute=(ATTRIBUTE)NULL;
        value_ptr=(VALUE)NULL;
 
        if ( ((attribute=FindParameter(input_msg, "MESSAGE_ID"))!=NULL) &&
               ( ((value_ptr=FirstValue(attribute))!=NULL) &&
               (value_ptr->item.length>0) ) )
        {
 
                if (value_ptr->item.type != TV_STRING)
                {
                        (void) ims_msg (msgDesc, IMS_ERROR,
                           "v0_process__parse_RxTree: Invalid format for MESSAGE_ID.");
                        return (IMS_ERROR);
                }
 
                if (value_ptr->item.length > MAX_MSG_ID)
                {
                        errbuf[0]    = '\0';
                        v0_errbuf[0] = '\0';
                        (void) sprintf (v0_errbuf, 
				"MESSAGE_ID value exceeded maximum "
                                "length of %d.", MAX_MSG_ID);
                        (void) sprintf (errbuf, "v0_process__parse_RxTree: %s",
					 v0_errbuf);
                        if (status = v0_order__report_order_error
                                (msgDesc, v0Desc, v0Desc->request.callerFlag, 
					errbuf, v0_errbuf) < IMS_OK)
                        {
                                return (status);
                        }
                        else
                        {
                                return (IMS_ERROR);
                        }
                }
 
                strcpy (v0Desc->message_id,
                        value_ptr->item.value.string);
                (void)remove_newlines (v0Desc->message_id);
                ims_truncStr (v0Desc->message_id);
        }

	/* authenticator */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "AUTHENTICATOR"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for AUTHENTICATOR.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_AUTH)
		{
			errbuf[0]    = '\0';
			v0_errbuf[0] = '\0';
			(void) sprintf (v0_errbuf, "AUTHENTICATOR value exceeded maximum "
				"length of %d.", MAX_AUTH);
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				return (status);
			}
			else
			{
				return (IMS_ERROR);
			}
		}

		strcpy (request->authenticator, 
		        value_ptr->item.value.string);
		(void)remove_newlines (request->authenticator);
		ims_truncStr (request->authenticator);
	}

	/* user_id */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "USER_ID"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for USER_ID.");
			return (IMS_ERROR);
		}

		strcpy (request->user_id, value_ptr->item.value.string);
		(void)remove_newlines (request->user_id);
		ims_truncStr (request->user_id);

		/*
		** collect email information if this is a dar request.  The information
		** will be used for sending error notifications.
		*/
		if (v0Desc->RxType == V0_DAR_REQUEST)
		{
			v0Desc->query.sql[0] = '\0';

			(void)sprintf (v0Desc->query.sql, 
				"select email from user_profile where "
				"user_id = '%s'", v0Desc->request.user_id);

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

			/*
			** collect the email infomation if found. Ignore the condition of
			** not found or multiple returned entries, which will be checked
			** later in the varification process.
			*/
			if (rowCount == 1)
			{
				strcpy (v0Desc->request.user_email,  hv->char_value1);		
			}

			while (hv != (V0_VALUE_LIST *)NULL)
			{
				tv = hv->next_p;
				free (hv);
				hv = tv;
			}
		}
	}

	/* 
	** get contact early, if available, because it will be needed for sending 
	** email notification in case of a few types of errors
	*/

	/* 
	** V0 user errors in CONATACT_ADDRESS, SHIPPING_ADDRESS, and 
	** BILLING_ADDRESS, i.e. string exceed valid length when user filling 
	** out user profile, should be recorded into v0_order_error table 
	*/

	group = (AGGREGATE)NULL;

	if ((group=FindAggregate(input_msg, "CONTACT_ADDRESS"))!=NULL) 
	{ 
		request->contact_p = 1;

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "EMAIL"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for EMAIL.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_EMAIL)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "EMAIL value in User Profile exceeded "
					"maximum length of %d.", MAX_EMAIL);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->contact_profile.email, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->contact_profile.email);
			ims_truncStr (request->contact_profile.email);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "LAST_NAME"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for LAST_NAME.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_NAME)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "LAST_NAME value in User Profile exceeded "
					"maximum length of %d.", MAX_NAME);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->contact_profile.last_name, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->contact_profile.last_name);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "FIRST_NAME"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for FIRST_NAME.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_NAME)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "FIRST_NAME value in User Profile exceeded "
					"maximum length of %d.", MAX_NAME);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}


			strcpy (request->contact_profile.first_name, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->contact_profile.first_name);
		}

		/*
		** get the user's email address if possible, so error message could
		** be sent to the user directly thru email
		*/

		if ( (request->contact_profile.first_name[0] != '\0') &&
				 (request->contact_profile.last_name[0] != '\0') &&
				 (request->authenticator[0] != '\0') &&
				 (request->contact_profile.email[0] == '\0') &&
				 (request->callerFlag == 1) &&
				 (v0Desc->RxType == V0_PRODUCT_REQUEST))
		{
			v0Desc->query.sql[0] = '\0';

			(void)sprintf (v0Desc->query.sql, 
				"select email from user_profile where "
				"upper(first_name) = '%s' and upper(last_name) = '%s' "
				"and crypt_key = '%s'", 
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

			if ( (status = v0_cat (catReq, V0_GETSTR) ) < IMS_OK)
			{
				return (status);
			}
	
			hv = (V0_VALUE_LIST *)catReq->item[2];

			/*
			** collect the email infomation if found. Ignore the condition of
			** not found or multiple returned entries, which will be checked
			** later in the varification process.
			**
			** store the information in request.user_email instead of 
			** contact_profile, so the ordering routine would not get confused
			** during verification time. The email address given in contact profile,
			** if provided, precedes this one.
			*/
			if (rowCount == 1)
			{
				strcpy (v0Desc->request.user_email,  hv->char_value1);		
			}

			while (hv != (V0_VALUE_LIST *)NULL)
			{
				tv = hv->next_p;
				free (hv);
				hv = tv;
			}
		}


		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "MIDDLE_INITIAL"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for MIDDLE_INITIAL.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_MID_NAME)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "MIDDLE_INITIAL value in User Profile exceeded "
					"maximum length of %d.", MAX_MID_NAME);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			request->contact_profile.initial_name[0] = 
				value_ptr->item.value.string[0];
			request->contact_profile.initial_name[1] = '\0'; 
			ims_toUpper (request->contact_profile.initial_name);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "TITLE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for TITLE.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_TITLE)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "TITLE value in User Profile exceeded "
					"maximum length of %d.", MAX_TITLE);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->contact_profile.title, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->contact_profile.title);
			ims_truncStr (request->contact_profile.title);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "ORGANIZATION"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for ORGANIZATION.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_ORG)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "ORGANIZATION value in User Profile exceeded "
					"maximum length of %d.", MAX_ORG);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->contact_profile.organization, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->contact_profile.organization);
		}


		/* street */

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		temp[0] = '\0';
		s_ptr   = temp;
		count = 0;

		if ( ((attribute=FindParameter(group, "ADDRESS"))!=NULL) &&
	     ( ((value_ptr=FirstValue(attribute))!=NULL) && 
	       (value_ptr->item.length>0) ) ) 
		{


			/*
			** get the first address line  
			*/

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for ADDRESS.");
				return (IMS_ERROR);
			}

			(void) remove_newlines (value_ptr->item.value.string);
			ims_truncStr (value_ptr->item.value.string);
			strcpy (s_ptr, value_ptr->item.value.string);
			count = count + strlen(s_ptr);
			s_ptr = s_ptr + strlen(s_ptr);

			/*
			**	traverse the tree to extract the rest of the address values 
			*/
			while ((value_ptr=NextValue(value_ptr))!=NULL)
			{

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			 	  	"v0_process__parse_RxTree: Invalid format for ADDRESS.");
					return (IMS_ERROR);
				}

				(void)remove_newlines (value_ptr->item.value.string);
				ims_truncStr (value_ptr->item.value.string);

				if (value_ptr->item.value.string[0] != '\0')
				{
					strcpy (s_ptr, ", ");
					s_ptr = s_ptr + strlen(s_ptr);

					strcpy (s_ptr, value_ptr->item.value.string);
					count = count + strlen(s_ptr);
					s_ptr = s_ptr + strlen(s_ptr);
				}

			}  /* while more NextValue */

			if (count > (MAX_ADDR*3) )
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "ADDRESS value in User Profile exceeded "
					"maximum total length of %d.", (MAX_ADDR*3));
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}
			else
			{
				(void) strcpy (request->contact_profile.street, temp);
			}
		} /* street */



		/* city */

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "CITY"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for CITY.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_CITY)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "CITY value in User Profile exceeded "
					"maximum length of %d.", MAX_CITY);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->contact_profile.city, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->contact_profile.city);
			ims_truncStr (request->contact_profile.city);
		}


		/* state */

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "STATE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for STATE.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_STATE)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "STATE value in User Profile exceeded "
					"maximum length of %d.", MAX_STATE);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->contact_profile.state, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->contact_profile.state);
		}

		/* zip */

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "ZIP"))!=NULL) && 
				 ( ((value_ptr=FirstValue(attribute))!=NULL) && 
					 (value_ptr->item.length>0) ) ) 
		{ 

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for ZIP.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_ZIP)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "ZIP value in User Profile exceeded "
					"maximum length of %d.", MAX_ZIP);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->contact_profile.zipcode, value_ptr->item.value.string); 
			(void)remove_newlines (request->contact_profile.zipcode); 
			ims_truncStr (request->contact_profile.zipcode); 
		} 
		
		/* country */ 
		attribute=(ATTRIBUTE)NULL; 
		value_ptr=(VALUE)NULL;
		if ( ((attribute=FindParameter(group, "COUNTRY"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for COUNTRY.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_COUNTRY)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "COUNTRY value in User Profile exceeded "
					"maximum length of %d.", MAX_COUNTRY);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->contact_profile.country, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->contact_profile.country);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "PHONE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for PHONE.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_PHONE)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "PHONE value in User Profile exceeded "
					"maximum length of %d.", MAX_PHONE);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->contact_profile.phone, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->contact_profile.phone);
			ims_truncStr (request->contact_profile.phone);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "FAX"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for FAX.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_PHONE)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "FAX value in User Profile exceeded "
					"maximum length of %d.", MAX_PHONE);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->contact_profile.fax, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->contact_profile.fax);
			ims_truncStr (request->contact_profile.fax);
		}

	} /* contact_profile */

	/* shipping_profile */

	group = (AGGREGATE)NULL;

	if ((group=FindAggregate(input_msg, "SHIPPING_ADDRESS"))!=NULL) 
	{ 
		request->shipping_p = 1;

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "EMAIL"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for EMAIL.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_EMAIL)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "EMAIL value in User Profile exceeded "
					"maximum length of %d.", MAX_EMAIL);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->shipping_profile.email, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->shipping_profile.email);
			ims_truncStr (request->shipping_profile.email);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "LAST_NAME"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for LAST_NAME.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_NAME)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "LAST_NAME value in User Profile exceeded "
					"maximum length of %d.", MAX_NAME);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}
	
			strcpy (request->shipping_profile.last_name, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->shipping_profile.last_name);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "FIRST_NAME"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			 		  "v0_process__parse_RxTree: Invalid format for FIRST_NAME.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_NAME)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "FIRST_NAME value in User Profile exceeded "
					"maximum length of %d.", MAX_NAME);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->shipping_profile.first_name, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->shipping_profile.first_name);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "MIDDLE_INITIAL"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for MIDDLE_INITIAL.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_MID_NAME)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "MIDDLE_INITIAL value in User Profile exceeded "
					"maximum length of %d.", MAX_MID_NAME);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->shipping_profile.initial_name, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->shipping_profile.initial_name);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "TITLE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for TITLE.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_TITLE)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "TITLE value in User Profile exceeded "
					"maximum length of %d.", MAX_TITLE);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->shipping_profile.title, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->shipping_profile.title);
			ims_truncStr (request->shipping_profile.title);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "ORGANIZATION"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for ORGANIZATION.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_ORG)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "ORGANIZATION value in User Profile exceeded "
					"maximum length of %d.", MAX_ORG);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->shipping_profile.organization, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->shipping_profile.organization);
		}


		/* street */

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		temp[0] = '\0';
		s_ptr   = temp;
		count = 0;

		if ( ((attribute=FindParameter(group, "ADDRESS"))!=NULL) &&
	     ( ((value_ptr=FirstValue(attribute))!=NULL) && 
	       (value_ptr->item.length>0) ) ) 
		{

			/*
			** get the first address line  
			*/

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for ADDRESS.");
				return (IMS_ERROR);
			}

			(void) remove_newlines(value_ptr->item.value.string);
			ims_truncStr (value_ptr->item.value.string);
			strcpy (s_ptr, value_ptr->item.value.string);
			count = count + strlen(s_ptr);
			s_ptr = s_ptr + strlen(s_ptr);

			/*
			**	traverse the tree to extract the rest of the address values 
			*/
			while ((value_ptr=NextValue(value_ptr))!=NULL)
			{

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	   "v0_process__parse_RxTree: Invalid format for ADDRESS.");
					return (IMS_ERROR);
				}

				ims_truncStr (value_ptr->item.value.string);
				(void)remove_newlines (value_ptr->item.value.string);

				if (value_ptr->item.value.string[0] != '\0')
				{
					strcpy (s_ptr, ", ");
					s_ptr = s_ptr + strlen(s_ptr);

					strcpy (s_ptr, value_ptr->item.value.string);
					count = count + strlen(s_ptr);
					s_ptr = s_ptr + strlen(s_ptr);
				}

			}  /* while more NextValue */

			if (count > (MAX_ADDR*3))
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "ADDRESS value in Shipping Profile exceeded "
					"maximum total length of %d.", (MAX_ADDR*3));
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}
			else
			{
				strcpy (request->shipping_profile.street, temp);
			}
		} /* street */



		/* city */

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "CITY"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for CITY.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_CITY)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "CITY value in User Profile exceeded "
					"maximum length of %d.", MAX_CITY);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->shipping_profile.city, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->shipping_profile.city);
			ims_truncStr (request->shipping_profile.city);
		}


		/* state */

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "STATE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for STATE.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_STATE)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "STATE value in User Profile exceeded "
					"maximum length of %d.", MAX_STATE);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->shipping_profile.state, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->shipping_profile.state);
		}

		/* zip */

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "ZIP"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for ZIP.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_ZIP)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "ZIP value in User Profile exceeded "
					"maximum length of %d.", MAX_ZIP);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->shipping_profile.zipcode, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->shipping_profile.zipcode);
			ims_truncStr (request->shipping_profile.zipcode);
		}

		/* country */

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "COUNTRY"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for COUNTRY.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_COUNTRY)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "COUNTRY value in User Profile exceeded "
					"maximum length of %d.", MAX_COUNTRY);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->shipping_profile.country, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->shipping_profile.country);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "PHONE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for PHONE.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_PHONE)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "PHONE value in User Profile exceeded "
					"maximum length of %d.", MAX_PHONE);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->shipping_profile.phone, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->shipping_profile.phone);
			ims_truncStr (request->shipping_profile.phone);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "FAX"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for FAX.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_PHONE)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "FAX value in User Profile exceeded "
					"maximum length of %d.", MAX_PHONE);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->shipping_profile.fax, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->shipping_profile.fax);
			ims_truncStr (request->shipping_profile.fax);
		}

	} /* shipping_profile */

	/* billing_profile */

	group = (AGGREGATE)NULL;

	if ((group=FindAggregate(input_msg, "BILLING_ADDRESS"))!=NULL) 
	{ 
		request->billing_p = 1;

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "EMAIL"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for EMAIL.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_EMAIL)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "EMAIL value in User Profile exceeded "
					"maximum length of %d.", MAX_EMAIL);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->billing_profile.email, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->billing_profile.email);
			ims_truncStr (request->billing_profile.email);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "LAST_NAME"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for LAST_NAME.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_NAME)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "LAST_NAME value in User Profile exceeded "
					"maximum length of %d.", MAX_NAME);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->billing_profile.last_name, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->billing_profile.last_name);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "FIRST_NAME"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for FIRST_NAME.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_NAME)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "FIRST_NAME value in User Profile exceeded "
					"maximum length of %d.", MAX_NAME);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->billing_profile.first_name, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->billing_profile.first_name);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "MIDDLE_INITIAL"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for MIDDLE_INITIAL.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_MID_NAME)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "MIDDLE_INITIAL value in User Profile exceeded "
					"maximum length of %d.", MAX_MID_NAME);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->billing_profile.initial_name, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->billing_profile.initial_name);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "TITLE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for TITLE.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_TITLE)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "TITLE value in User Profile exceeded "
					"maximum length of %d.", MAX_TITLE);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->billing_profile.title, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->billing_profile.title);
			ims_truncStr (request->billing_profile.title);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "ORGANIZATION"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for ORGANIZATION.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_ORG)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "ORGANIZATION value in User Profile exceeded "
					"maximum length of %d.", MAX_ORG);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->billing_profile.organization, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->billing_profile.organization);
		}


		/* street */

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		temp[0] = '\0';
		s_ptr   = temp;
		count = 0;

		if ( ((attribute=FindParameter(group, "ADDRESS"))!=NULL) &&
	     ( ((value_ptr=FirstValue(attribute))!=NULL) && 
	       (value_ptr->item.length>0) ) ) 
		{
			/*
			** get the first address line  
			*/
			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for ADDRESS.");
				return (IMS_ERROR);
			}

			(void) remove_newlines (value_ptr->item.value.string);
			ims_truncStr (value_ptr->item.value.string);
			strcpy (s_ptr, value_ptr->item.value.string);
			count = count + strlen(s_ptr);
			s_ptr = s_ptr + strlen(s_ptr);

			/*
			**	traverse the tree to extract the rest of the address values 
			*/
			while ((value_ptr=NextValue(value_ptr))!=NULL)
			{

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	    "v0_process__parse_RxTree: Invalid format for ADDRESS.");
					return (IMS_ERROR);
				}

				ims_truncStr (value_ptr->item.value.string);
				(void)remove_newlines (value_ptr->item.value.string);

				if (value_ptr->item.value.string[0] != '\0')
				{
					strcpy (s_ptr, ", ");
					s_ptr = s_ptr + strlen(s_ptr);

					strcpy (s_ptr, value_ptr->item.value.string);
					count = count + strlen(s_ptr);
					s_ptr = s_ptr + strlen(s_ptr);
				}

			}  /* while more NextValue */

			if (count > (MAX_ADDR*3))
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "ADDRESS value in Billing Profile exceeded "
					"maximum total length of %d.", (MAX_ADDR*3));
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}
			else
			{
				strcpy (request->billing_profile.street, temp);
			}
		} /* street */



		/* city */

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "CITY"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for CITY.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_CITY)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "CITY value in User Profile exceeded "
					"maximum length of %d.", MAX_CITY);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->billing_profile.city, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->billing_profile.city);
			ims_truncStr (request->billing_profile.city);
		}


		/* state */

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "STATE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for STATE.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_STATE)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "STATE value in User Profile exceeded "
					"maximum length of %d.", MAX_STATE);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->billing_profile.state, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->billing_profile.state);
		}

		/* zip */

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "ZIP"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for ZIP.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_ZIP)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "ZIP value in User Profile exceeded "
					"maximum length of %d.", MAX_ZIP);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->billing_profile.zipcode, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->billing_profile.zipcode);
			ims_truncStr (request->billing_profile.zipcode);
		}

		/* country */

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "COUNTRY"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for COUNTRY.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_COUNTRY)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "COUNTRY value in User Profile exceeded "
					"maximum length of %d.", MAX_COUNTRY);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->billing_profile.country, 
							value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->billing_profile.country);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "PHONE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for PHONE.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_PHONE)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "PHONE value in User Profile exceeded "
					"maximum length of %d.", MAX_PHONE);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->billing_profile.phone, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->billing_profile.phone);
			ims_truncStr (request->billing_profile.phone);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "FAX"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for FAX.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_PHONE)
			{
				errbuf[0]    = '\0';
				v0_errbuf[0] = '\0';
				(void) sprintf (v0_errbuf, "FAX value in User Profile exceeded "
					"maximum length of %d.", MAX_PHONE);
				(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
				if (status = v0_order__report_order_error
					(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
						< IMS_OK)
				{
					return (status);
				}
				else
				{
					return (IMS_ERROR);
				}
			}

			strcpy (request->billing_profile.fax, 
							value_ptr->item.value.string);
			(void)remove_newlines (request->billing_profile.fax);
			ims_truncStr (request->billing_profile.fax);
		}

	} /* billing_profile */ 

	/* first name */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "FIRST_NAME"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{
		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for FIRST_NAME.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_NAME)
		{
			errbuf[0]    = '\0';
			v0_errbuf[0] = '\0';
			(void) sprintf (v0_errbuf, "FIRST_NAME value exceeded maximum "
				"length of %d.", MAX_NAME);
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				return (status);
			}
			else
			{
				strcpy(v0Desc->odl_status, "03");
				return (IMS_ERROR);
			}
		}

		strcpy (request->user_info.first_name, 
		        value_ptr->item.value.string);
		(void) v0_process__prepare_string (request->user_info.first_name);
	}

	/* last name */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "LAST_NAME"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for LAST_NAME.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_NAME) 
		{ 
			errbuf[0]    = '\0'; 
			v0_errbuf[0] = '\0'; 
			(void) sprintf (v0_errbuf, "LAST_NAME value exceeded maximum " 
					"length of %d.", MAX_NAME); 
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf); 
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				return (status);
			}
			else
			{
				return (IMS_ERROR);
			}
		}

		strcpy (request->user_info.last_name, 
		        value_ptr->item.value.string);
		(void) v0_process__prepare_string (request->user_info.last_name);
	}

	/*
	** browse_only
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "BROWSE_ONLY"))!=NULL) &&
  	     ( ((value_ptr=FirstValue(attribute))!=NULL) && 
	       (value_ptr->item.length>0) ) ) 
	{
		strcpy(request->browse_p, value_ptr->item.value.string);

		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}
	}

	/*
	** global_granules_only
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if (((attribute=FindParameter(input_msg,"GLOBAL_GRANULES_ONLY"))!=NULL)&&
  	     ( ((value_ptr=FirstValue(attribute))!=NULL) && 
	       (value_ptr->item.length>0) ) ) 
	{
		/*
		** 'Y' is the only valid value for this field
		*/
		if (strcmp (value_ptr->item.value.string, "Y") == 0)
		{
			request->global_granules_p = 'Y'; 
			
			if (request->select_all == 1)  
			{
				request->select_all = 0;
			}
		}

		else
		{
			strcpy(v0Desc->odl_status, "08");

			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_process__parse_RxTree:Bad value for global_granules_only, return a QUIT with status 17.");

			return (IMS_FATAL);
		}
	}

	/*
	** start_time
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "START_DATE"))!=NULL) &&
	     ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_DATE_TIME)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for START_DATE.");
			return (IMS_ERROR);
		}

		if ((v0Desc->RxType == V0_DAR_REQUEST) ||
		    (v0Desc->RxType == V0_DAR_LIST_QUERY))
		{
			(void)v0_time__OdlToIMSTime (value_ptr, request->start_time);
		}
		else
		{
			(void) v0_time__OdlToDatetime(value_ptr, request->start_time,
				request->start_time_mmdd, 
				request->start_time_year);
			request->start_time[strlen(request->start_time)]='\0';
			request->start_time_mmdd[strlen(request->start_time_mmdd)]='\0';
			request->start_time_year[strlen(request->start_time_year)]='\0';

		}

		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}
	}

 
	/*
	** end_time
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "STOP_DATE"))!=NULL) &&
	     ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_DATE_TIME)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for STOP_DATE.");
			return (IMS_ERROR);
		}

		if ((v0Desc->RxType == V0_DAR_REQUEST) ||
		    (v0Desc->RxType == V0_DAR_LIST_QUERY))
		{
			(void)v0_time__OdlToIMSTime (value_ptr, request->end_time);
		}
		else
		{
			(void) v0_time__OdlToDatetime(value_ptr, request->end_time,
				request->end_time_mmdd, 
				request->end_time_year);
			request->end_time[strlen(request->end_time)]='\0';
			request->end_time_mmdd[strlen(request->end_time_mmdd)]='\0';
			request->end_time_year[strlen(request->end_time_year)]='\0';
		}

		if (request->select_all == 1 )  
		{
			request->select_all = 0;
		}
	}

	/*
	** start_day_of_year
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "START_DAY_OF_YEAR"))!=NULL) &&
	     ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{
		request->start_day_of_year = value_ptr->item.value.integer.number;

		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}
	}

	/*
	** stop_day_of_year 
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "STOP_DAY_OF_YEAR"))!=NULL) &&
	     ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{
		request->stop_day_of_year = value_ptr->item.value.integer.number;

		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}
	}

	/*
	** day_night
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "DAY_NIGHT"))!=NULL) &&
	     ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{
		strcpy(request->day_night, value_ptr->item.value.string);

		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}
	}

	/*
	** granule_limit
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "GRANULE_LIMIT"))!=NULL) &&
	     ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{
		request->granule_limit = value_ptr->item.value.integer.number;
	}

	/* account_id */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "BILLING_ID"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for BILLING_ID.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_BILL_ID)
		{
			errbuf[0]    = '\0';
			v0_errbuf[0] = '\0';
			(void) sprintf (v0_errbuf, "BILLING_ID value exceeded maximum "
				"length of %d.", MAX_BILL_ID);
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				return (status);
			}
			else
			{
				return (IMS_ERROR);
			}
		}

		strcpy (request->account_id, value_ptr->item.value.string);
		(void)remove_newlines (request->account_id);
		ims_truncStr (request->account_id);
	}


	/* mode */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "MODE"))!=NULL) && 
			 ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for MODE.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_MODE)
		{
			errbuf[0]    = '\0';
			v0_errbuf[0] = '\0';
			(void) sprintf (v0_errbuf, "MODE value exceeded maximum "
				"length of %d.", MAX_MODE);
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				return (status);
			}
			else
			{
				return (IMS_ERROR);
			}
		}

		strcpy (request->mode, value_ptr->item.value.string);
		(void)remove_newlines (request->mode);
		ims_truncStr (request->mode);
	}

	/* dar_status */
        attribute=(ATTRIBUTE)NULL;
        value_ptr=(VALUE)NULL;
 
        if ( ((attribute=FindParameter(input_msg, "DAR_STATUS"))!=NULL) &&
             ( ((value_ptr=FirstValue(attribute))!=NULL) &&
               (value_ptr->item.length>0) ) )
        {
 
                if (value_ptr->item.type != TV_INTEGER)
                {
                        (void) ims_msg (msgDesc, IMS_ERROR,
                           "v0_process__parse_RxTree: Invalid format for DAR_STATUS.");
                        return (IMS_ERROR);
                }
 
                request->dar_status = value_ptr->item.value.integer.number;
        }
        else
        {
                request->dar_status = 0;
        }

        /* order_id */
        attribute=(ATTRIBUTE)NULL;
        value_ptr=(VALUE)NULL;
 
        if ( ((attribute=FindParameter(input_msg, "ORDER_ID"))!=NULL) &&
             ( ((value_ptr=FirstValue(attribute))!=NULL) &&
               (value_ptr->item.length>0) ) )
        {
 
                if (value_ptr->item.type != TV_INTEGER)
                {
                        (void) ims_msg (msgDesc, IMS_ERROR,
                           "v0_process__parse_RxTree: Invalid format for ORDER_ID.");
                        return (IMS_ERROR);
                }
 
                request->order_id = value_ptr->item.value.integer.number;
        }
        else
        {
                request->order_id = 0;
        }

        /* item_id */
        attribute=(ATTRIBUTE)NULL;
        value_ptr=(VALUE)NULL;
 
        if ( ((attribute=FindParameter(input_msg, "ITEM_ID"))!=NULL) &&
             ( ((value_ptr=FirstValue(attribute))!=NULL) &&
               (value_ptr->item.length>0) ) )
        {
 
                if (value_ptr->item.type != TV_INTEGER)
                {
                        (void) ims_msg (msgDesc, IMS_ERROR,
                           "v0_process__parse_RxTree: Invalid format for ITEM_ID.");
                        return (IMS_ERROR);
                }
 
                request->item_id = value_ptr->item.value.integer.number;
        }
        else
        {
                request->item_id = 0;
        }

	/* quicklook_p */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "QUICK_LOOK"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for QUICK_LOOK.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_QL)
		{
			errbuf[0]    = '\0';
			v0_errbuf[0] = '\0';
			(void) sprintf (v0_errbuf, "QUICK_LOOK value exceeded maximum "
				"length of %d.", MAX_QL);
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				return (status);
			}
			else
			{
				return (IMS_ERROR);
			}
		}

		strcpy (request->quicklook_p, value_ptr->item.value.string);
		(void) v0_process__prepare_string (request->quicklook_p);
	}

	/*
	** observation_num
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "OB_NUMBER"))!=NULL) &&
	     ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_INTEGER)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for OB_NUMBER.");
			return (IMS_ERROR);
		}

		request->observation_num = value_ptr->item.value.integer.number;
	}
	else
	{
		request->observation_num = -1;
	}


	/* observation_freq */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "OB_FREQUENCY"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for OB_FREQUENCY.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_FREQ)
		{
			errbuf[0]    = '\0';
			v0_errbuf[0] = '\0';
			(void) sprintf (v0_errbuf, "OB_FREQUENCY value exceeded maximum "
				"length of %d.", MAX_FREQ);
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				strcpy(v0Desc->odl_status, "03");
				return (status);
			}
			else
			{
				strcpy(v0Desc->odl_status, "03");
				return (IMS_ERROR);
			}
		}

		strcpy (request->observation_freq, value_ptr->item.value.string);
		(void)remove_newlines (request->observation_freq);
		ims_truncStr (request->observation_freq);
	}

	/* 
	** V0 DAR passes observation_number 0 and observation_feq "" if 
	** the user did not specify any value for both fields.  Change the
	** observation_number to -1 in this case.
	*/
	if ((v0Desc->RxType == V0_DAR_REQUEST) &&
			(v0Desc->request.callerFlag == 1) &&
	    ((request->observation_num == 0) && 
	     (request->observation_freq[0] == '\0')))
	{
		request->observation_num = -1;			
	}

	/* active_p */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "ACTIVITY"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for ACTIVITY.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_ACT_P)
		{
			errbuf[0]    = '\0';
			v0_errbuf[0] = '\0';
			(void) sprintf (v0_errbuf, "ACTIVITY value exceeded maximum "
				"length of %d.", MAX_ACT_P);
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				return (status);
			}
			else
			{
				return (IMS_ERROR);
			}
		}

		strcpy (request->active_p, value_ptr->item.value.string);
		(void) v0_process__prepare_string (request->active_p);
	}

	/* activity_start_date */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "START_FIELD"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for START_FIELD.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_S_FIELD)
		{
			errbuf[0]    = '\0';
			v0_errbuf[0] = '\0';
			(void) sprintf (v0_errbuf, "START_FIELD value exceeded maximum "
				"length of %d.", MAX_S_FIELD);
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				return (status);
			}
			else
			{
				return (IMS_ERROR);
			}
		}

		strcpy (request->activity_start_date, value_ptr->item.value.string);
		(void)remove_newlines (request->activity_start_date);
		ims_truncStr (request->activity_start_date);
	}

	/* activity_end_date */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "END_FIELD"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for END_FIELD.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_E_FIELD)
		{
			errbuf[0]    = '\0';
			v0_errbuf[0] = '\0';
			(void) sprintf (v0_errbuf, "END_FIELD value exceeded maximum "
				"length of %d.", MAX_E_FIELD);
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				return (status);
			}
			else
			{
				return (IMS_ERROR);
			}
		}

		strcpy (request->activity_end_date, value_ptr->item.value.string);
		(void)remove_newlines (request->activity_end_date);
		ims_truncStr (request->activity_end_date);
	}

	/* priority */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "PRIORITY"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for PRIORITY.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_PRIORITY)
		{
			errbuf[0]    = '\0';
			v0_errbuf[0] = '\0';
			(void) sprintf (v0_errbuf, "PRIORITY value exceeded maximum "
				"length of %d.", MAX_PRIORITY);
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				return (status);
			}
			else
			{
				return (IMS_ERROR);
			}
		}

		msgbuf[0] = '\0';
		strcpy (msgbuf, value_ptr->item.value.string);
		(void) v0_process__prepare_string (msgbuf);

		if (strcmp (msgbuf, "LOW") == 0)
		{
			request->dar_priority = 1;
		}
		else if (strcmp (msgbuf, "ROUTINE") == 0)
		{
			request->dar_priority = 2;
		}
		else if (strcmp (msgbuf, "HIGH") == 0)
		{
			request->dar_priority = 3;
		}
		else if (strcmp (msgbuf, "URGENT") == 0)
		{
			request->dar_priority = 4;
		}
	} 

	/* dar_comment */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	temp[0] = '\0';
	s_ptr = temp;
	count = 0;

	if ( ((attribute=FindParameter(input_msg, "DAR_COMMENT"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for DAR_COMMENT.");
			return (IMS_ERROR);
		}

		(void)remove_newlines (value_ptr->item.value.string);
		ims_truncStr (value_ptr->item.value.string);
		strcpy (s_ptr, value_ptr->item.value.string);
		count = count + strlen(s_ptr);
		s_ptr = s_ptr + strlen(s_ptr);

		while ( ((value_ptr=NextValue(value_ptr))!=NULL) && (count<MAX_DAR_COMT))
		{
			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
				 "v0_process__parse_RxTree: Invalid format for DAR_COMMENT.");
				return (IMS_ERROR);
			}

			(void)remove_newlines (value_ptr->item.value.string);
			ims_truncStr (value_ptr->item.value.string);

			if (value_ptr->item.value.string[0] != '\0')
			{
				sprintf (s_ptr, " %s", value_ptr->item.value.string);
				count = count + strlen(s_ptr);
				s_ptr = s_ptr + strlen(s_ptr);
			}
		}

		if (count > MAX_DAR_COMT)
		{
			errbuf[0]    = '\0';
			v0_errbuf[0] = '\0';
			(void) sprintf (v0_errbuf, "DAR_COMMENT exceeded "
				"maximum length of %d.", MAX_DAR_COMT);
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				strcpy(v0Desc->odl_status, "03");
				return (status);
			}
			else
			{
				strcpy(v0Desc->odl_status, "03");
				return (IMS_ERROR);
			}
		}
		else
		{
			strcpy (request->dar_comment, temp);
		}
		
	}

	/* asc_desc */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "DIRECTION"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for DIRECTION.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_DIRECTION)
		{
			errbuf[0]    = '\0';
			v0_errbuf[0] = '\0';
			(void) sprintf (v0_errbuf, "DIRECTION value exceeded maximum "
				"length of %d.", MAX_DIRECTION);
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				return (status);
			}
			else
			{
				return (IMS_ERROR);
			}
		}

		strcpy (request->asc_desc, value_ptr->item.value.string);
		(void) v0_process__prepare_string (request->asc_desc);
	}

	/* site_name */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "SITE_NAME"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for SITE_NAME.");
			return (IMS_ERROR);
		}

		if ( value_ptr->item.length> MAX_S_NAME)
		{
			errbuf[0]    = '\0';
			v0_errbuf[0] = '\0';
			(void) sprintf (v0_errbuf, "SITE_NAME exceeded "
				"maximum length of %d.", MAX_S_NAME);
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				strcpy(v0Desc->odl_status, "03");
				return (status);
			}
			else
			{
				strcpy(v0Desc->odl_status, "03");
				return (IMS_ERROR);
			}
		}

		strcpy (request->site_name, value_ptr->item.value.string);
		(void) v0_process__prepare_string (request->site_name);
	}

	/* site_shape */
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "SITE_SHAPE"))!=NULL) && 
	       ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for SITE_SHAPE.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_S_SHAPE)
		{
			errbuf[0]    = '\0';
			v0_errbuf[0] = '\0';
			(void) sprintf (v0_errbuf, "SITE_SHAPE value exceeded maximum "
				"length of %d.", MAX_S_SHAPE);
			(void) sprintf (errbuf, "v0_process__parse_RxTree: %s", v0_errbuf);
			if (status = v0_order__report_order_error
				(msgDesc, v0Desc, v0Desc->request.callerFlag, errbuf, v0_errbuf)
					< IMS_OK)
			{
				return (status);
			}
			else
			{
				return (IMS_ERROR);
			}
		}

		msgbuf[0] = '\0';
		strcpy (msgbuf, value_ptr->item.value.string);
		(void) v0_process__prepare_string (msgbuf);

		if (strcmp (msgbuf, "P")== 0)
		{
			request->spatial_type = 3;
		}
		else if ((strcmp (msgbuf, "R") == 0) || (strcmp (msgbuf, "Q") == 0))
		{
			request->spatial_type = 4;
		}
	}

	/*
	** dataset_id
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "DATASET_ID"))!=NULL) &&
	     ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{

		if ( ( track_ptr = (V0_VALUE_LIST *) 
				malloc (sizeof(V0_VALUE_LIST))) ==NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_process: Memory allocation failed for track_ptr");
			strcpy (v0Desc->odl_status, "19");  
			return(IMS_FATAL);
		}							

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for DATASET_ID.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_DS_ID)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: DATASET_ID value exceeded maximum "
				 "length of %d", MAX_DS_ID);
			return (IMS_ERROR);
		}

		/*
		** get the first search spec for dataset name 
		*/
		strcpy (track_ptr->char_value1, value_ptr->item.value.string);
		(void) v0_process__prepare_string (track_ptr->char_value1);
		track_ptr->next_p = (V0_VALUE_LIST *)NULL;

		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}			

		request->dataset_id = track_ptr;

		/*
		**	traverse the tree to extract the rest of the search values for
		**	dataset names 
		*/
		while ((value_ptr=NextValue(value_ptr))!=NULL)
		{

				if (( track_ptr->next_p= 
					(V0_VALUE_LIST *) malloc (sizeof(V0_VALUE_LIST))) ==NULL)
				{     
					(void) ims_msg (msgDesc, IMS_FATAL, 
					"v0_process: Memory allocation failed for track_ptr");
					strcpy (v0Desc->odl_status, "19");  
					return(IMS_FATAL);
			
				}

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for DATASET_ID.");
					return (IMS_ERROR);
				}

				if (value_ptr->item.length > MAX_DS_ID)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: DATASET_ID value exceeded maximum "
				 "length of %d", MAX_DS_ID);
					return (IMS_ERROR);
				}

				track_ptr = track_ptr->next_p;
				strcpy (track_ptr->char_value1, value_ptr->item.value.string);
				(void) v0_process__prepare_string (track_ptr->char_value1);
				track_ptr->next_p=(V0_VALUE_LIST *)NULL;
		}  /* while more NextValue */
      
	} /* dataset name */


	/*
	** sensor
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "SENSOR_NAME"))!=NULL) &&
	     ( ((value_ptr=FirstValue(attribute))!=NULL) && 
	       (value_ptr->item.length>0) ) ) 
	{

		if ( ( track_ptr = (V0_VALUE_LIST *) 
				malloc (sizeof(V0_VALUE_LIST))) ==NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_process: Memory allocation failed for track_ptr");
			strcpy (v0Desc->odl_status, "19");  
			return(IMS_FATAL);
		}							

		/*
		** get the first search spec for sensor  
		*/

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for SENSOR_NAME.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_SENSOR)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: SENSOR_NAME value exceeded maximum "
				 "length of %d", MAX_SENSOR);
			return (IMS_ERROR);
		}

		strcpy (track_ptr->char_value1, value_ptr->item.value.string);
		(void) v0_process__prepare_string (track_ptr->char_value1);
		track_ptr->next_p=(V0_VALUE_LIST *)NULL;

		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}

		request->sensor = track_ptr;

		/*
		**	traverse the tree to extract the rest of the search values for
		**	sensor 
		*/
		while ((value_ptr=NextValue(value_ptr))!=NULL)
		{

				if ( ( track_ptr->next_p= 
					(V0_VALUE_LIST *) malloc (sizeof(V0_VALUE_LIST))) ==NULL)
				{     
					(void) ims_msg (msgDesc, IMS_FATAL, 
					"v0_process: Memory allocation failed for track_ptr");
					strcpy (v0Desc->odl_status, "19");  
					return(IMS_FATAL);
				}


			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for SENSOR_NAME.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_SENSOR)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: SENSOR_NAME value exceeded maximum "
				 "length of %d", MAX_SENSOR);
				return (IMS_ERROR);
			}

				track_ptr = track_ptr->next_p;
				strcpy (track_ptr->char_value1, value_ptr->item.value.string);
				(void) v0_process__prepare_string (track_ptr->char_value1);
				track_ptr->next_p=(V0_VALUE_LIST *)NULL;

		}  /* while more NextValue */
	} /* sensor */

	/*
	**  platform
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "SOURCE_NAME"))!=NULL) && 
	     ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{


		if ( ( track_ptr = (V0_VALUE_LIST *) 
				malloc (sizeof(V0_VALUE_LIST))) ==NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
			"v0_process: Memory allocation failed for track_ptr");
			strcpy (v0Desc->odl_status, "19");  
			return(IMS_FATAL);
		}							

		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for SOURCE_NAME.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_PLAT)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: SOURCE_NAME value exceeded maximum "
				 "length of %d", MAX_PLAT);
			return (IMS_ERROR);
		}

		/* get the first search spec for platform  */
		strcpy (track_ptr->char_value1, value_ptr->item.value.string);
		(void) v0_process__prepare_string (track_ptr->char_value1);
		track_ptr->next_p = (V0_VALUE_LIST *)NULL;

		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}

		request->platform = track_ptr;

		/*	traverse the tree to extract the rest of the search values for
			platform  */
		while ((value_ptr=NextValue(value_ptr))!=NULL)
		{


				if ( ( track_ptr->next_p= 
					(V0_VALUE_LIST *) malloc (sizeof(V0_VALUE_LIST))) ==NULL)
				{     
					(void) ims_msg (msgDesc, IMS_FATAL, 
					"v0_process: Memory allocation failed for track_ptr");
					strcpy (v0Desc->odl_status, "19");  
					return(IMS_FATAL);
				}

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for SOURCE_NAME.");
					return (IMS_ERROR);
				}

				if (value_ptr->item.length > MAX_PLAT)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: SOURCE_NAME value exceeded maximum "
				 	"length of %d", MAX_PLAT);
					return (IMS_ERROR);
				}

				track_ptr = track_ptr->next_p;
				strcpy (track_ptr->char_value1, value_ptr->item.value.string);
				(void) v0_process__prepare_string (track_ptr->char_value1);
				track_ptr->next_p=(V0_VALUE_LIST *)NULL;
		}  /* while more NextValue */
	} /* platform */


	/*
	** parameter
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "PARAMETER"))!=NULL) && 
	     ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{


		if ( ( track_ptr = (V0_VALUE_LIST *) 
				malloc (sizeof(V0_VALUE_LIST))) ==NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_process: Memory allocation failed for track_ptr");
			strcpy (v0Desc->odl_status, "19");  
			return(IMS_FATAL);
		}							


		/* get the first search spec for parameter name */
		strcpy (track_ptr->char_value1, value_ptr->item.value.string);
		(void) v0_process__prepare_string (track_ptr->char_value1);
		track_ptr->next_p = (V0_VALUE_LIST *)NULL;

		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}

		request->parameter = track_ptr;

		/*	traverse the tree to extract the rest of the search values for
			parameter names */
		while ((value_ptr=NextValue(value_ptr))!=NULL)
		{


				if ( ( track_ptr->next_p= 
					(V0_VALUE_LIST *) malloc (sizeof(V0_VALUE_LIST))) ==NULL)
				{     
					(void) ims_msg (msgDesc, IMS_FATAL, 
						"v0_process: Memory allocation failed for track_ptr");
					strcpy (v0Desc->odl_status, "19");  
					return(IMS_FATAL);
				}

				track_ptr = track_ptr->next_p;
				strcpy (track_ptr->char_value1, value_ptr->item.value.string);
				(void) v0_process__prepare_string (track_ptr->char_value1);
				track_ptr->next_p=(V0_VALUE_LIST *)NULL;
		}  /* while more NextValue */
	} /* parameter */

	/*
	** campaign
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "CAMPAIGN"))!=NULL) && 
	     ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{


		if ( ( track_ptr = (V0_VALUE_LIST *) 
				malloc (sizeof(V0_VALUE_LIST))) ==NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_process: Memory allocation failed for track_ptr");
			strcpy (v0Desc->odl_status, "19");  
			return(IMS_FATAL);
		}							


		/* get the first search spec for campaign name */
		strcpy (track_ptr->char_value1, value_ptr->item.value.string);
		(void) v0_process__prepare_string (track_ptr->char_value1);
		track_ptr->next_p = (V0_VALUE_LIST *)NULL;

		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}

		request->campaign = track_ptr;

		/*	traverse the tree to extract the rest of the search values for
			campaign names */
		while ((value_ptr=NextValue(value_ptr))!=NULL)
		{


				if ( ( track_ptr->next_p= 
					(V0_VALUE_LIST *) malloc (sizeof(V0_VALUE_LIST))) ==NULL)
				{     
					(void) ims_msg (msgDesc, IMS_FATAL, 
						"v0_process: Memory allocation failed for track_ptr");
					strcpy (v0Desc->odl_status, "19");  
					return(IMS_FATAL);
				}

				track_ptr = track_ptr->next_p;
				strcpy (track_ptr->char_value1, value_ptr->item.value.string);
				(void) v0_process__prepare_string (track_ptr->char_value1);
				track_ptr->next_p=(V0_VALUE_LIST *)NULL;
		}  /* while more NextValue */
	} /* campaign */

	/*
	** process_level
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "PROCESSING_LEVEL"))!=NULL) && 
	     ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{


		if ( ( track_ptr = (V0_VALUE_LIST *) 
				malloc (sizeof(V0_VALUE_LIST))) ==NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_process: Memory allocation failed for track_ptr");
			strcpy (v0Desc->odl_status, "19");  
			return(IMS_FATAL);
		}							


		/* get the first search spec for process_level name */
		strcpy (track_ptr->char_value1, value_ptr->item.value.string);
		(void) v0_process__prepare_string (track_ptr->char_value1);
		track_ptr->next_p = (V0_VALUE_LIST *)NULL;

		/* 
		** convert process level from string to integer, the level is expected
		** to be only one digit, and letter part of the string is ignored, 
		** e.g. "1a" -> 1 
		*/	
		for (count=0; count <= (int)strlen(track_ptr->char_value1); count++)
		{
			if (isdigit(track_ptr->char_value1[count]))
			{
				track_ptr->smallint_value = atoi(track_ptr->char_value1);
				break;
			}
		}

		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}

		request->process_level = track_ptr;

		/*	traverse the tree to extract the rest of the search values for
			process_level names */
		while ((value_ptr=NextValue(value_ptr))!=NULL)
		{


				if ( ( track_ptr->next_p= 
					(V0_VALUE_LIST *) malloc (sizeof(V0_VALUE_LIST))) ==NULL)
				{     
					(void) ims_msg (msgDesc, IMS_FATAL, 
						"v0_process: Memory allocation failed for track_ptr");
					strcpy (v0Desc->odl_status, "19");  
					return(IMS_FATAL);
				}

				track_ptr = track_ptr->next_p;
				strcpy (track_ptr->char_value1, value_ptr->item.value.string);
				(void) v0_process__prepare_string (track_ptr->char_value1);
				track_ptr->next_p=(V0_VALUE_LIST *)NULL;

				for (count=0; count <= (int)strlen(track_ptr->char_value1); count++)
				{
					if (isdigit(track_ptr->char_value1[count]))
					{
						track_ptr->smallint_value = atoi(track_ptr->char_value1);
						break;
					}
				}
		}  /* while more NextValue */
	} /* process_level */

	/*
	** granule_id
	*/
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "GRANULE_ID_REQ"))!=NULL) && 
	     ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       (value_ptr->item.length>0) ) ) 
	{


		if ( ( track_ptr = (V0_VALUE_LIST *) 
				malloc (sizeof(V0_VALUE_LIST))) ==NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_process: Memory allocation failed for track_ptr");
			strcpy (v0Desc->odl_status, "19");  
			return(IMS_FATAL);
		}							

		if ((request->granule_limit == 0) || (request->dataset_id == NULL))
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_process: Either granule limit or dataset id is missing "
				"for granule id search.");
			strcpy (v0Desc->odl_status, "17");  
			return(IMS_FATAL);
		}

		/* get the first search spec for granule ids */
		strcpy (track_ptr->char_value1, value_ptr->item.value.string);
		(void) v0_process__prepare_string (track_ptr->char_value1);
		(void) prepare_wildcard_string (track_ptr);
		track_ptr->next_p = (V0_VALUE_LIST *)NULL;

		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}

		request->granule_id = track_ptr;

		/*	traverse the tree to extract the rest of the search values for
			granule ids */
		while ((value_ptr=NextValue(value_ptr))!=NULL)
		{


				if ( ( track_ptr->next_p= 
					(V0_VALUE_LIST *) malloc (sizeof(V0_VALUE_LIST))) ==NULL)
				{     
					(void) ims_msg (msgDesc, IMS_FATAL, 
						"v0_process: Memory allocation failed for track_ptr");
					strcpy (v0Desc->odl_status, "19");  
					return(IMS_FATAL);
				}

				track_ptr = track_ptr->next_p;
				strcpy (track_ptr->char_value1, value_ptr->item.value.string);
				(void) v0_process__prepare_string (track_ptr->char_value1);
				(void) prepare_wildcard_string (track_ptr);
				track_ptr->next_p=(V0_VALUE_LIST *)NULL;
		}  /* while more NextValue */
	} /* granule_id */

	/*
	** point_loc, i.e. u_lat[0] and u_lon[0]
	*/
	group = (AGGREGATE)NULL;
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ((group=FindAggregate(input_msg, "POINT_LOC"))!=NULL) 
	{ 
		if ( ((attribute=FindParameter(group, "LATITUDE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{
			request->u_lat[0] = value_ptr->item.value.real.number;

		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "LONGITUDE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{
		 	request->u_lon[0] = value_ptr->item.value.real.number;

		}

		if  ( (request->u_lat[0] == INFINITY)  ||
				(request->u_lon[0] == INFINITY)  )
		{
			strcpy(v0Desc->odl_status, "09");

			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_process__parse_RxTree:missing point_loc coordinate, return a QUIT with status 17.");

			return (IMS_FATAL);
			
		}

		request->region_type = POINT_LOC; 

		global_lat = 0;
		global_lon = 0;

		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}
	}  /* end of point_loc */
	
	/*
	** point_radius_loc, i.e. center_lat, center_lon, radius 
	*/
	group = (AGGREGATE)NULL;
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ((group=FindAggregate(input_msg, "POINT_RADIUS_LOC"))!=NULL) 
	{ 
		if ( ((attribute=FindParameter(group, "CENTROID_LAT"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for CENTROID_LAT.");
				return (IMS_ERROR);
			}

			request->center_lat = value_ptr->item.value.real.number;
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "CENTROID_LON"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for CENTROID_LON.");
				return (IMS_ERROR);
			}

		 	request->center_lon = value_ptr->item.value.real.number;
		}

		if ( ((attribute=FindParameter(group, "RADIUS"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for RADIUS.");
				return (IMS_ERROR);
			}

		 	request->radius = value_ptr->item.value.real.number;
		}

		if  ( (request->center_lat == INFINITY)  ||
				(request->center_lon == INFINITY)  ||
				(request->radius  == INFINITY) )
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"v0_process__parse_RxTree:missing point_radius coordinate for DAR REQUEST.");
			return (IMS_ERROR);
		}

	}  /* end of point_radius_loc */
	
	/*
	** quadr_loc, i.e. nw_lat, nw_lon, ne_lat, ne_lon, se_lat, se_lon, sw_lat,
	** sw_lon
	*/
	group = (AGGREGATE)NULL;
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ((group=FindAggregate(input_msg, "QUADR_LOC"))!=NULL) 
	{ 
		if ( ((attribute=FindParameter(group, "NORTHWEST_LAT"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for NORTHWEST_LAT.");
				return (IMS_ERROR);
			}

			request->nw_lat = value_ptr->item.value.real.number;
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "NORTHWEST_LON"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for NORTHWEST_LON.");
				return (IMS_ERROR);
			}

		 	request->nw_lon = value_ptr->item.value.real.number;
		}

		if ( ((attribute=FindParameter(group, "NORTHEAST_LAT"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for NORTHEAST_LAT.");
				return (IMS_ERROR);
			}

			request->ne_lat = value_ptr->item.value.real.number;
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "NORTHEAST_LON"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for NORTHEAST_LON.");
				return (IMS_ERROR);
			}

		 	request->ne_lon = value_ptr->item.value.real.number;
		}

		if ( ((attribute=FindParameter(group, "SOUTHEAST_LAT"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for SOUTHEAST_LAT.");
				return (IMS_ERROR);
			}

			request->se_lat = value_ptr->item.value.real.number;
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "SOUTHEAST_LON"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for SOUTHEAST_LON.");
				return (IMS_ERROR);
			}

		 	request->se_lon = value_ptr->item.value.real.number;
		}

		if ( ((attribute=FindParameter(group, "SOUTHWEST_LAT"))!=NULL) && 
				 ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for SOUTHWEST_LAT.");
				return (IMS_ERROR);
			}

			request->sw_lat = value_ptr->item.value.real.number;
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "SOUTHWEST_LON"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for SOUTHWEST_LON.");
				return (IMS_ERROR);
			}

			request->sw_lon = value_ptr->item.value.real.number;
		}

		if  ( (request->nw_lat == INFINITY)  ||
				(request->nw_lon == INFINITY)  ||
		    (request->ne_lat == INFINITY)  ||
				(request->ne_lon == INFINITY)  ||
		    (request->se_lat == INFINITY)  ||
				(request->se_lon == INFINITY)  ||
		    (request->sw_lat == INFINITY)  ||
				(request->sw_lon == INFINITY) ) 
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"v0_process__parse_RxTree:missing quadrilateral coordinate for DAR REQUEST.");
			return (IMS_ERROR);
			
		}

	}  /* end of quadr_loc */

	/*  
	** range_loc, i.e. u_min_lat, u_max_lat, u_min_lon, and u_max_lon 
	**
	**
	** RANGE_LOC is received when user select RECTANGULAR (on map or form)
	** or POINT & RANGE (on form) on v0 client for INVENTORY SEARCH.
	**
	** RANGE_LOC is also utilized in DAR REQUEST. The four boundaries are 
	** converted into four points in this case.
	**
	*/ 

	group = (AGGREGATE)NULL;
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;
	
	if ((group=FindAggregate(input_msg, "RANGE_LOC"))!=NULL)
	{
		if ( ((attribute=FindParameter(group, "NORTH_LATITUDE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for NORTH_LATITUDE.");
				return (IMS_ERROR);
			}

			request->u_max_lat = value_ptr->item.value.real.number;

		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "SOUTH_LATITUDE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for SOUTH_LATITUDE.");
				return (IMS_ERROR);
			}

			request->u_min_lat = value_ptr->item.value.real.number;

		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "EAST_LONGITUDE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	         (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for EAST_LONGITUDE.");
				return (IMS_ERROR);
			}

			request->u_max_lon =  value_ptr->item.value.real.number;

		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "WEST_LONGITUDE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_REAL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for WEST_LONGITUDE.");
				return (IMS_ERROR);
			}

			request->u_min_lon = value_ptr->item.value.real.number;

			if (request->u_min_lon <= -180.0 )
			{
				request->u_min_lon = -180.0;
			}

		}

		if ( (request->u_min_lat == INFINITY) ||
			  (request->u_max_lat == INFINITY) ||
			  (request->u_min_lon == INFINITY) ||
			  (request->u_max_lon == INFINITY)  )
		{
			if (v0Desc->RxType == V0_INVENTORY_SEARCH)
			{
				strcpy(v0Desc->odl_status, "09");

				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_process__parse_RxTree:missing range_loc coordinate, "
					"return a QUIT with status 09 for INVENTORY SEARCH.");

				return (IMS_FATAL);
			}
			else if (v0Desc->RxType == V0_DAR_REQUEST)
			{

				(void) ims_msg (msgDesc, IMS_ERROR, "v0_process__parse_RxTree: missing range_loc coordinate for DAR REQUEST");

				return (IMS_ERROR);
			}
		}

		if (v0Desc->RxType == V0_INVENTORY_SEARCH )
		{ 
			if (request->u_max_lat >= 90.0)
			{
				request->u_max_lat = 90.0;
			}

			if (request->u_min_lat <= -90.0)
			{
				request->u_min_lat = -90.0;
			}

			if (request->u_max_lon >= 180.0 )
			{
				request->u_max_lon = 180.0;
			}

			if ( (request->u_max_lat != 90.0) || (request->u_min_lat != -90.0) ||
					 (request->u_max_lon != 180.0)|| (request->u_min_lon != -180.0))
			{
				global_lon = 0;
			}

			request->region_type = RANGE_LOC; 

		}
		else if (v0Desc->RxType == V0_DAR_REQUEST )
		{
			if  (request->u_max_lat < request->u_min_lat) 
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
				  "v0_process__parse_RxTree: South latitude exceeded North latitude.");
				return (IMS_ERROR);
			}
			else if (request->u_max_lon < request->u_min_lon)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
				  "v0_process__parse_RxTree: West longitude exceeded East longitude.");
				return (IMS_ERROR);
			}


			(void) v0_spatial__rectTo4pt (&request->u_max_lat, &request->u_min_lat,
			                              &request->u_max_lon, &request->u_min_lon,
					                          &request->nw_lat,    &request->nw_lon,
																		&request->ne_lat,    &request->ne_lon,
																		&request->se_lat,    &request->se_lon,
																		&request->sw_lat,    &request->sw_lon);
		}
	
		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}

	}  /* end of range_loc */


	/*
	** polygon_loc, i.e. u_lat[0] to u_lat [PT_MAX-1] and u_lon[0] to 
	** u_lon[PT_MAX-1]
	**
	** The min/max here is based on the order received from the ODL
	** message.  The actual minimum/maximum boundaries will be calculated
	** later.
	*/
	group = (AGGREGATE)NULL;
	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;


	if ((group=FindAggregate(input_msg, "POLYGON_LOC"))!=NULL) 
	{ 
		if ( ((attribute=FindParameter(group, "LATITUDE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{
			count = 0;

			while ( value_ptr!=NULL)
			{
				request->u_lat[count] = value_ptr->item.value.real.number;
				count++;

				value_ptr = NextValue (value_ptr);
			}
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "LONGITUDE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			count = 0;

			while ( value_ptr!=NULL)
			{
				request->u_lon[count] = value_ptr->item.value.real.number;

				count++;
				value_ptr = NextValue (value_ptr);
			}
		}

		for (count=0; count<PT_MAX; count++)
		{
			if ( (request->u_lat[count] == INFINITY) ||
				  (request->u_lon[count] == INFINITY))
			{
				strcpy(v0Desc->odl_status, "09");

				(void) ims_msg (msgDesc, IMS_FATAL, 
					"v0_process__parse_RxTree:missing polygon_loc coordinate, return a QUIT with status 17.");

				return (IMS_FATAL);
			}
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "MAP_PROJECTION_TYPE"))!=NULL) &&
  	  	   ( ((value_ptr=FirstValue(attribute))!=NULL) && 
	  	     (value_ptr->item.length>0) ) ) 
		{

			if (strcmp ( value_ptr->item.value.string, 
				   "NORTH_POLAR_STEREOGRAPHIC") == 0)  
			{
				request->map_projection_type = POLAR_STEREOGRAPHIC;
			}

			else if (strcmp ( value_ptr->item.value.string,
					"SOUTH_POLAR_STEREOGRAPHIC") == 0) 
			{

				request->map_projection_type = POLAR_STEREOGRAPHIC;
			}

			else if (strcmp ( value_ptr->item.value.string, 
				"PLATE_CARREE") == 0)
			{
				request->map_projection_type = PLATE_CARREE;
			}
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "TANGENT_LATITUDE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{
			request->tangent_latitude = value_ptr->item.value.integer.number;
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "TANGENT_LONGITUDE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{
			request->tangent_longitude = value_ptr->item.value.integer.number; 
		}

		request->region_type = POLYGON_LOC; 

		global_lat = 0;
		global_lon = 0;

		if (request->select_all == 1)  
		{
			request->select_all = 0;
		}

	}  
	/* end of polygon_loc */
	
	/*
	** We treat a granule id search without spatial specification as
	** a global rectangular search
	*/
	if ( (request->granule_id != (V0_VALUE_LIST *)NULL) &&
			 (request->region_type == V0_UNIDENTIFIED_REGION))
	{
		request->region_type = RANGE_LOC; 
		request->u_max_lat = 90.0;
		request->u_min_lat = -90.0;
		request->u_max_lon = 180.0;
		request->u_min_lon = -180.0;
		global_lat = 1;
		global_lon = 1;
	}

	if ( (global_lat == 1) && ( global_lon == 1) )
	{
			request->global_search = 'Y';	
	}


	/* inventory search does not accept select all */
	if ( (v0Desc->RxType == V0_INVENTORY_SEARCH) && 
		  (request->select_all == 1))   
	{
		strcpy (v0Desc->odl_status , "12");     /* search too wide */

#ifdef DEBUG
		(void) ims_msg (msgDesc, IMS_ERROR,
		"v0_process__parse_RxTree: Search too broad, return a QUIT with status 12.");
#endif
		return (IMS_ERROR);
	}


	/*
	** PRODUCT_REQUEST values 
	*/ 

	/* request_id */

	attribute=(ATTRIBUTE)NULL; 
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "REQUEST_ID"))!=NULL) &&
  	     ( ((value_ptr=FirstValue(attribute))!=NULL) && 
	       (value_ptr->item.length>0) ) ) 
	{
		if (value_ptr->item.type != TV_STRING)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for REQUEST_ID.");
			return (IMS_ERROR);
		}

		if (value_ptr->item.length > MAX_REQ_ID)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: REQUEST_ID value exceeded maximum "
				 "length of %d", MAX_REQ_ID);
			return (IMS_ERROR);
		}

		strcpy(request->order_request_id, value_ptr->item.value.string);
		(void) v0_process__prepare_string (request->order_request_id);
	}

	/* data_center_id */

	attribute=(ATTRIBUTE)NULL;
	value_ptr=(VALUE)NULL;

	if ( ((attribute=FindParameter(input_msg, "DATA_CENTER_ID"))!=NULL) &&
	     ( ((value_ptr=FirstValue(attribute))!=NULL) && 
	       (value_ptr->item.length>0) ) ) 
	{


		if ( ( track_ptr = (V0_VALUE_LIST *) 
				malloc (sizeof(V0_VALUE_LIST))) ==NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_process: Memory allocation failed for track_ptr");
			strcpy (v0Desc->odl_status, "19");  
			return(IMS_FATAL);
		}							

		/*
		** get the first data_center_id value  
		*/

		strcpy (track_ptr->char_value1, value_ptr->item.value.string);
		(void) v0_process__prepare_string (track_ptr->char_value1);
		track_ptr->next_p=(V0_VALUE_LIST *)NULL;

		request->data_center_id = track_ptr;

		/*
		**	traverse the tree to extract the rest of the data_center_id values 
		*/
		while ((value_ptr=NextValue(value_ptr))!=NULL)
		{

				if ( ( track_ptr->next_p= 
					(V0_VALUE_LIST *) malloc (sizeof(V0_VALUE_LIST))) ==NULL)
				{     
					(void) ims_msg (msgDesc, IMS_FATAL, 
					"v0_process: Memory allocation failed for track_ptr");
					strcpy (v0Desc->odl_status, "19");  
					return(IMS_FATAL);
				}

				track_ptr = track_ptr->next_p;
				strcpy (track_ptr->char_value1, value_ptr->item.value.string);
				(void) v0_process__prepare_string (track_ptr->char_value1);
				track_ptr->next_p=(V0_VALUE_LIST *)NULL;

		}  /* while more NextValue */
	} /* data_center_id */

	/* user_info  or user_acct_info */
	group = (AGGREGATE)NULL;

	if (((group=FindAggregate(input_msg, "USER_INFO"))!=NULL)        ||
	    ((group=FindAggregate(input_msg, "USER_ACCT_INFO"))!=NULL))
	{ 
		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "FIRST_NAME"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for FIRST_NAME.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_NAME)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: FIRST_NAME value exceeded maximum "
				 "length of %d", MAX_NAME);
				return (IMS_ERROR);
			}

			strcpy (request->user_info.first_name, 
			        value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->user_info.first_name);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "LAST_NAME"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for LAST_NAME.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_NAME)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: LAST_NAME value exceeded maximum "
				 "length of %d", MAX_NAME);
				return (IMS_ERROR);
			}

			strcpy (request->user_info.last_name, 
			        value_ptr->item.value.string);
			(void) v0_process__prepare_string (request->user_info.last_name);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "AUTHENTICATOR"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for AUTHENTICATOR.");
				return (IMS_ERROR);
			}

			strcpy (request->authenticator, 
			        value_ptr->item.value.string);
			(void)remove_newlines (request->authenticator);
			ims_truncStr (request->authenticator);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "BILLING_ID"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for BILLING_ID.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_BILL_ID)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: BILLING_ID value exceeded maximum "
				 "length of %d", MAX_BILL_ID);
				return (IMS_ERROR);
			}

			strcpy (request->account_id, 
			        value_ptr->item.value.string);
			(void)remove_newlines (request->account_id);
			ims_truncStr (request->account_id);
		}
	} /* user_info */

	/* line_item. LINE_ITEM and SCAN_ITEM attributes are collected separately */

	group = (AGGREGATE)NULL;
	if ( (group=FindAggregate(input_msg, "LINE_ITEM"))!=NULL) 
	{
		if ( (line_item_ptr = (V0_LINE_ITEM_LIST *) 
		     malloc (sizeof(V0_LINE_ITEM_LIST))) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_process: Memory allocation failed for line_item_ptr");
			strcpy (v0Desc->odl_status, "19");
			return(IMS_FATAL);
		}

		(void) init_line_item_list (line_item_ptr);

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "DATASET_ID"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for DATASET_ID.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_DS_ID)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: DATASET_ID value exceeded maximum "
				 "length of %d", MAX_DS_ID);
				return (IMS_ERROR);
			}

			strcpy (line_item_ptr->dataset_id, value_ptr->item.value.string);
			(void) v0_process__prepare_string (line_item_ptr->dataset_id);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "PROCESSING_OPTIONS"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
		   	"v0_process__parse_RxTree: Invalid format for PROCESSING_OPTIONS.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_PROC_OPT)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: PROCESSING_OPTIONS value exceeded maximum "
				 "length of %d", MAX_PROC_OPT);
				return (IMS_ERROR);
			}

			strcpy (line_item_ptr->v0_process_type, value_ptr->item.value.string);
			(void) v0_process__prepare_string (line_item_ptr->v0_process_type);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "PACKAGE_ID"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for PACKAGE_ID.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_GNUL_ID)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: PACKAGE_ID value exceeded maximum "
				 "length of %d", MAX_GNUL_ID);
				return (IMS_ERROR);
			}

			strcpy (line_item_ptr->granule_id, value_ptr->item.value.string);
			(void)remove_newlines (line_item_ptr->granule_id);
			ims_truncStr (line_item_ptr->granule_id);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "MEDIA_TYPE"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			  	"v0_process__parse_RxTree: Invalid format for MEDIA_TYPE.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_MEDIA_TYPE)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: MEDIA_TYPE value exceeded maximum "
				 "length of %d", MAX_MEDIA_TYPE);
				return (IMS_ERROR);
			}

			strcpy (line_item_ptr->v0_media_type, value_ptr->item.value.string);
			(void) v0_process__prepare_string (line_item_ptr->v0_media_type);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "MEDIA_FORMAT"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			  	"v0_process__parse_RxTree: Invalid format for MEDIA_FORMAT.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_MEDIA_FMT)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: MEDIA_FORMAT value exceeded maximum "
				 "length of %d", MAX_MEDIA_FMT);
				return (IMS_ERROR);
			}

			strcpy (line_item_ptr->v0_media_fmt_type, value_ptr->item.value.string);
			(void) v0_process__prepare_string (line_item_ptr->v0_media_fmt_type);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		/*
		** v0 client version 5.0 and up holds account_id in BILLING_ID
		** prior to 5.0, account_id was held in ADDITIONAL_INFO 
		** We accept request from users using either keywords. 
		*/
		if ( ((attribute=FindParameter(group, "BILLING_ID"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{


			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
		       "v0_process__parse_RxTree: Invalid format for BILLING_ID.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_BILL_ID)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: BILLING_ID value exceeded maximum "
				 "length of %d", MAX_BILL_ID);
				return (IMS_ERROR);
			}

			strcpy (line_item_ptr->account_id, value_ptr->item.value.string);
			(void)remove_newlines (line_item_ptr->account_id);
			ims_truncStr (line_item_ptr->account_id);
		}

		else if ( ((attribute=FindParameter(group, "ADDITIONAL_INFO"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			  	"v0_process__parse_RxTree: Invalid format for ADDITIONAL_INFO.");
				return (IMS_ERROR);
			}

			strcpy (line_item_ptr->account_id, value_ptr->item.value.string);
			(void)remove_newlines (line_item_ptr->account_id);
			ims_truncStr (line_item_ptr->account_id);
		}

		request->line_item_list = line_item_ptr;

		while ((group=FindNextAggregate(input_msg, group, "LINE_ITEM"))!=NULL)
		{
			if ( (line_item_ptr->next_p = (V0_LINE_ITEM_LIST *) 
			     malloc (sizeof(V0_LINE_ITEM_LIST))) == NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
					"v0_process: Memory allocation failed for line_item_ptr");
				strcpy (v0Desc->odl_status, "19");
				return(IMS_FATAL);
			}
	
			line_item_ptr = line_item_ptr->next_p;
	
			(void) init_line_item_list (line_item_ptr);

			attribute=(ATTRIBUTE)NULL;
			value_ptr=(VALUE)NULL;
	
			if ( ((attribute=FindParameter(group, "DATASET_ID"))!=NULL) && 
		        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
		        (value_ptr->item.length>0) ) ) 
			{

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	   "v0_process__parse_RxTree: Invalid format for DATASET_ID.");
					return (IMS_ERROR);
				}

				if (value_ptr->item.length > MAX_DS_ID)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: DATASET_ID value exceeded maximum "
				 	"length of %d", MAX_DS_ID);
					return (IMS_ERROR);
				}

				strcpy (line_item_ptr->dataset_id, value_ptr->item.value.string);
				(void) v0_process__prepare_string (line_item_ptr->dataset_id);
			}
	
			attribute=(ATTRIBUTE)NULL;
			value_ptr=(VALUE)NULL;
	
			if ( ((attribute=FindParameter(group, "PROCESSING_OPTIONS"))!=NULL) && 
		        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
		        (value_ptr->item.length>0) ) ) 
			{

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for PROCESSING_OPTIONS.");
					return (IMS_ERROR);
				}

				if (value_ptr->item.length > MAX_PROC_OPT)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: PROCESSING_OPTIONS value exceeded maximum "
				 	"length of %d", MAX_PROC_OPT);
					return (IMS_ERROR);
				}

				strcpy (line_item_ptr->v0_process_type, value_ptr->item.value.string);
				(void) v0_process__prepare_string (line_item_ptr->v0_process_type);
			}
	
			attribute=(ATTRIBUTE)NULL;
			value_ptr=(VALUE)NULL;
	
			if ( ((attribute=FindParameter(group, "PACKAGE_ID"))!=NULL) && 
		        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
		        (value_ptr->item.length>0) ) ) 
			{

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	  "v0_process__parse_RxTree: Invalid format for PACKAGE_ID.");
					return (IMS_ERROR);
				}

				if (value_ptr->item.length > MAX_GNUL_ID)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: PACKAGE_ID value exceeded maximum "
				 	"length of %d", MAX_GNUL_ID);
					return (IMS_ERROR);
				}

				strcpy (line_item_ptr->granule_id, value_ptr->item.value.string);
				(void)remove_newlines (line_item_ptr->granule_id);
				ims_truncStr (line_item_ptr->granule_id);
			}
	
			attribute=(ATTRIBUTE)NULL;
			value_ptr=(VALUE)NULL;
	
			if ( ((attribute=FindParameter(group, "MEDIA_TYPE"))!=NULL) && 
		        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
		        (value_ptr->item.length>0) ) ) 
			{

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			  	    "v0_process__parse_RxTree: Invalid format for MEDIA_TYPE.");
					return (IMS_ERROR);
				}

				if (value_ptr->item.length > MAX_MEDIA_TYPE)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: MEDIA_TYPE value exceeded maximum "
				 	"length of %d", MAX_MEDIA_TYPE);
					return (IMS_ERROR);
				}

				strcpy (line_item_ptr->v0_media_type, value_ptr->item.value.string);
				(void) v0_process__prepare_string (line_item_ptr->v0_media_type);
			}
	
			attribute=(ATTRIBUTE)NULL;
			value_ptr=(VALUE)NULL;
	
			if ( ((attribute=FindParameter(group, "MEDIA_FORMAT"))!=NULL) && 
		        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
		        (value_ptr->item.length>0) ) ) 
			{

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			  	"v0_process__parse_RxTree: Invalid format for MEDIA_FORMAT.");
					return (IMS_ERROR);
				}

				if (value_ptr->item.length > MAX_MEDIA_FMT)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: MEDIA_FORMAT value exceeded maximum "
				 	"length of %d", MAX_MEDIA_FMT);
					return (IMS_ERROR);
				}

				strcpy (line_item_ptr->v0_media_fmt_type, value_ptr->item.value.string);
				(void) v0_process__prepare_string (line_item_ptr->v0_media_fmt_type);
			}
	
			attribute=(ATTRIBUTE)NULL;
			value_ptr=(VALUE)NULL;

			if ( ((attribute=FindParameter(group, "BILLING_ID"))!=NULL) && 
		        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
		        (value_ptr->item.length>0) ) ) 
			{

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: Invalid format for BILLING_ID.");
					return (IMS_ERROR);
				}

				if (value_ptr->item.length > MAX_BILL_ID)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: BILLING_ID value exceeded maximum "
				 	"length of %d", MAX_BILL_ID);
					return (IMS_ERROR);
				}

				strcpy (line_item_ptr->account_id, value_ptr->item.value.string);
				(void)remove_newlines (line_item_ptr->account_id);
				ims_truncStr (line_item_ptr->account_id);
			}
	
			else if ( ((attribute=FindParameter(group, "ADDITIONAL_INFO"))!=NULL) && 
		        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
		        (value_ptr->item.length>0) ) ) 
			{

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			  	"v0_process__parse_RxTree: Invalid format for ADDITIONAL_INFO.");
					return (IMS_ERROR);
				}

				strcpy (line_item_ptr->account_id, value_ptr->item.value.string);
				(void)remove_newlines (line_item_ptr->account_id);
				ims_truncStr (line_item_ptr->account_id);
			}

		}

	} /* LINE_ITEM groups */

	if ( (group=FindAggregate(input_msg, "SCAN_ITEM"))!=NULL) 
	{ 

		if ( (line_item_ptr = (V0_LINE_ITEM_LIST *) 
		     malloc (sizeof(V0_LINE_ITEM_LIST))) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_process: Memory allocation failed for line_item_ptr");
			strcpy (v0Desc->odl_status, "19");
			return(IMS_FATAL);
		}

		(void) init_line_item_list (line_item_ptr);

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "PLATFORM"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for PLATFORM.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_PLAT)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: PLATFORM value exceeded maximum "
				 "length of %d", MAX_PLAT);
				return (IMS_ERROR);
			}

			strcpy (line_item_ptr->platform, value_ptr->item.value.string);
			(void) v0_process__prepare_string (line_item_ptr->platform);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "SENSOR_NAME"))!=NULL) && 
	        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        (value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			      "v0_process__parse_RxTree: Invalid format for SENSOR_NAME.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_SENSOR)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: SENSOR_NAME value exceeded maximum "
				 "length of %d", MAX_SENSOR);
				return (IMS_ERROR);
			}

			strcpy (line_item_ptr->sensor_name, value_ptr->item.value.string);
			(void) v0_process__prepare_string (line_item_ptr->sensor_name);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "DATASET"))!=NULL) && 
        	( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        	(value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for DATASET.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_DS_ID)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: DATASET value exceeded maximum "
				 "length of %d", MAX_DS_ID);
				return (IMS_ERROR);
			}

			strcpy (line_item_ptr->dataset_id, value_ptr->item.value.string);
			(void) v0_process__prepare_string (line_item_ptr->dataset_id);
		}

		/* quicklook_p */
		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;
	
		if ( ((attribute=FindParameter(group, "QUICK_LOOK"))!=NULL) && 
	       	( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       	(value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for QUICK_LOOK.");
				return (IMS_ERROR);
			}
	
			if (value_ptr->item.length > MAX_QL)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: QUICK_LOOK value exceeded maximum "
				 	"length of %d", MAX_QL);
				return (IMS_ERROR);
			}
	
			strcpy (line_item_ptr->quicklook_p, value_ptr->item.value.string);
			(void) v0_process__prepare_string (line_item_ptr->quicklook_p);
		}

		attribute=(ATTRIBUTE)NULL;
		value_ptr=(VALUE)NULL;

		if ( ((attribute=FindParameter(group, "FILENAME"))!=NULL) && 
        	( ((value_ptr=FirstValue(attribute))!=NULL) &&
        	(value_ptr->item.length>0) ) ) 
		{

			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: Invalid format for FILENAME.");
				return (IMS_ERROR);
			}

			if (value_ptr->item.length > MAX_GNUL_ID)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
			   "v0_process__parse_RxTree: FILENAME value exceeded maximum "
				 "length of %d", MAX_GNUL_ID);
				return (IMS_ERROR);
			}

			strcpy (line_item_ptr->granule_id, value_ptr->item.value.string);
			(void)remove_newlines (line_item_ptr->granule_id);
			ims_truncStr (line_item_ptr->granule_id);
		}
	
		request->line_item_list = line_item_ptr;

		while ((group=FindNextAggregate(input_msg, group, "SCAN_ITEM"))!=NULL)
		{
		
			if ( (line_item_ptr->next_p = (V0_LINE_ITEM_LIST *) 
			     malloc (sizeof(V0_LINE_ITEM_LIST))) == NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
					"v0_process: Memory allocation failed for line_item_ptr");
				strcpy (v0Desc->odl_status, "19");
				return(IMS_FATAL);
			}
	
			line_item_ptr = line_item_ptr->next_p;
	
			(void) init_line_item_list (line_item_ptr);

			attribute=(ATTRIBUTE)NULL;
			value_ptr=(VALUE)NULL;
	
			if ( ((attribute=FindParameter(group, "PLATFORM"))!=NULL) && 
	        	( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        	(value_ptr->item.length>0) ) ) 
			{

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			         "v0_process__parse_RxTree: Invalid format for PLATFORM.");
					return (IMS_ERROR);
				}

				if (value_ptr->item.length > MAX_PLAT)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: PLATFORM value exceeded maximum "
				 	"length of %d", MAX_PLAT);
					return (IMS_ERROR);
				}

				strcpy (line_item_ptr->platform, value_ptr->item.value.string);
				(void) v0_process__prepare_string (line_item_ptr->platform);
			}
	
			attribute=(ATTRIBUTE)NULL;
			value_ptr=(VALUE)NULL;
	
			if ( ((attribute=FindParameter(group, "SENSOR_NAME"))!=NULL) && 
	        	( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        	(value_ptr->item.length>0) ) ) 
			{

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			        "v0_process__parse_RxTree: Invalid format for SENSOR_NAME.");
					return (IMS_ERROR);
				}

				if (value_ptr->item.length > MAX_SENSOR)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: SENSOR_NAME value exceeded maximum "
				 	"length of %d", MAX_SENSOR);
					return (IMS_ERROR);
				}

				strcpy (line_item_ptr->sensor_name, value_ptr->item.value.string);
				(void) v0_process__prepare_string (line_item_ptr->sensor_name);
			}
	
			attribute=(ATTRIBUTE)NULL;
			value_ptr=(VALUE)NULL;
	
			if ( ((attribute=FindParameter(group, "DATASET"))!=NULL) && 
	        	( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        	(value_ptr->item.length>0) ) ) 
			{

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			      "v0_process__parse_RxTree: Invalid format for DATASET.");
					return (IMS_ERROR);
				}

				strcpy (line_item_ptr->dataset_id, value_ptr->item.value.string);
				(void) v0_process__prepare_string (line_item_ptr->dataset_id);
			}
	
			/* quicklook_p */
			attribute=(ATTRIBUTE)NULL;
			value_ptr=(VALUE)NULL;

			if ( ((attribute=FindParameter(group, "QUICK_LOOK"))!=NULL) && 
	       		( ((value_ptr=FirstValue(attribute))!=NULL) &&
	       		(value_ptr->item.length>0) ) ) 
			{
		
				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   		"v0_process__parse_RxTree: Invalid format for QUICK_LOOK.");
					return (IMS_ERROR);
				}
		
				if (value_ptr->item.length > MAX_QL)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   		"v0_process__parse_RxTree: QUICK_LOOK value exceeded maximum "
				 		"length of %d", MAX_QL);
					return (IMS_ERROR);
				}
		
				strcpy (line_item_ptr->quicklook_p, value_ptr->item.value.string);
				(void) v0_process__prepare_string (line_item_ptr->quicklook_p);
			}

			attribute=(ATTRIBUTE)NULL;
			value_ptr=(VALUE)NULL;
	
			if ( ((attribute=FindParameter(group, "FILENAME"))!=NULL) && 
	        	( ((value_ptr=FirstValue(attribute))!=NULL) &&
	        	(value_ptr->item.length>0) ) ) 
			{

				if (value_ptr->item.type != TV_STRING)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			      "v0_process__parse_RxTree: Invalid format for FILENAME.");
					return (IMS_ERROR);
				}

				if (value_ptr->item.length > MAX_GNUL_ID)
				{
					(void) ims_msg (msgDesc, IMS_ERROR, 
			   	"v0_process__parse_RxTree: FILENAME value exceeded maximum "
				 	"length of %d", MAX_GNUL_ID);
					return (IMS_ERROR);
				}

				strcpy (line_item_ptr->granule_id, value_ptr->item.value.string);
				(void)remove_newlines (line_item_ptr->granule_id);
				ims_truncStr (line_item_ptr->granule_id);
			}
	
		}

	}/* SCAN_ITEM groups */

	/* create a one entry line item for DAR REQUEST */
	if (v0Desc->RxType == V0_DAR_REQUEST )
	{
		if ( (line_item_ptr = (V0_LINE_ITEM_LIST *) 
		     malloc (sizeof(V0_LINE_ITEM_LIST))) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_process: Memory allocation failed for line_item_ptr");
			strcpy (v0Desc->odl_status, "19");
			return(IMS_FATAL);
		}

		(void) init_line_item_list (line_item_ptr);
		request->line_item_list = line_item_ptr;
	}

	return (IMS_OK);


} /* end of v0_process__parse_RxTree */

/***********************************************************************
**      
** retrive_ds_list - Build a query based on contents in v0Desc, and 
**                   execute it to retrieve a dataset list from database
**
** called by: v0_process
**
************************************************************************/           
static int retrieve_ds_list (V0_DESC_STRUCT *v0Desc)
{
	int status;
	IMS_MSG_STRUCT *msgDesc;
	V0_CAT_STRUCT *catReq;        /* catalog request structure */
	char msg[IMS_COL1024_LEN*11];

	/*
	** initialization
	*/
	msgDesc = v0Desc->msgDesc;

	/*
	** initialize the query structure 
	*/
	strcpy (v0Desc->query.select, "\0");
	strcpy (v0Desc->query.from, "\0");
	strcpy (v0Desc->query.where, "\0");
	strcpy (v0Desc->query.sql, "\0");

	/*
	** construct a query to retrieve a dataset list 
	*/
	if ( (status = v0_query__build_ds (v0Desc)) < IMS_OK )
	{
		return (status);
	}

#	ifdef QDEBUG
		sprintf (msg, "\n\nsql ==> %s", v0Desc->query.sql);
		(void) ims_msg(msgDesc, IMS_INFO, msg);
#	endif

	/* 
	** Initiallize catalog request structure 
	*/
	catReq = &v0Desc->catReq;
	catReq->msgDesc = msgDesc;

	/* item[0] points to the sql statement */
	catReq->item[0] = (void *)v0Desc->query.sql;

	/*
	** item[1] will receive the number of datasets
	*/
	catReq->item[1] = (int *)&v0Desc->result.dataset_count;

	/*
	** execute the query for dataset list
	*/
	if ( (status = v0_cat (catReq, V0_EXECDATASETSEARCH)) < IMS_OK)
	{
		strcpy (v0Desc->odl_status, "19");
		return (status);
	}

	/*
	** item[2] points to the beginning of the dataset list
	*/
 	v0Desc->result.dataset_list = (V0_DATASET_LIST *)catReq->item[2];

	return (IMS_OK);

} /* end of retrieve_ds_list */

/***********************************************************************
**      
** retrive_granules_list - Build a query based on contents in v0Desc, and 
**                   execute it to retrieve a list of granule information
**	                  associated with the current dataset from database
**
** called by: v0_process
**
************************************************************************/           
static int retrieve_granules_list (V0_DESC_STRUCT *v0Desc)
{
	int status;
	IMS_MSG_STRUCT *msgDesc;
	V0_CAT_STRUCT *catReq;        /* catalog request structure */
	V0_DATASET_LIST *curr_dataset;
	V0_GRANULE_LIST *g_ptr;
	V0_KEYWORD_LIST *first_ptr, *last_ptr, *new_ptr; 
	DBSMALLINT query_type;
	V0_GRANULE_LIST *temp;
	int max_granule;
	char msg[IMS_COL1024_LEN*11];
	IMS_NUMERIC_DATE temp_date;

	msgDesc = v0Desc->msgDesc;
	curr_dataset = v0Desc->result.curr_dataset;
	catReq = &v0Desc->catReq;

	max_granule = 0;


	/* return if curr_dataset is null */
	if (curr_dataset == (V0_DATASET_LIST *)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
			"retrieve_granule_list: Unexpected NULL pointer for curr_dataset");
		strcpy (v0Desc->odl_status, "19");
		return (IMS_FATAL);
	}

	/*
	** initialization
	*/
	curr_dataset->temporal_key_list = (V0_KEYWORD_LIST *)NULL;
	curr_dataset->temporal_key_count = 0;
	curr_dataset->spatial_key_list = (V0_KEYWORD_LIST *)NULL;
	curr_dataset->spatial_key_count = 0;
	curr_dataset->detail_key_list = (V0_KEYWORD_LIST *)NULL;
	curr_dataset->detail_key_count = 0;

	/* 
	** Get temporal keywords required for our search
	*/
	switch (curr_dataset->temporal_type)
	{
		case 1:

			/*
			** single point,  i.e. center_time
			*/
			query_type = CENTER_TIME_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			curr_dataset->temporal_key_list = 
				(V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->temporal_key_count++;

			break;

		case 2:

			/*
			** continuous range, i.e. start time, stop_time
			*/
			query_type = START_TIME_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			curr_dataset->temporal_key_list = 
				(V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->temporal_key_count++;

			query_type = END_TIME_TYPE;
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			curr_dataset->temporal_key_list->next_p = 
				(V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->temporal_key_count++;

			break;


		case 0:
			/*
			** This is a valid temporal type, but we ignore it at this point 
			*/
			break;

		default:
			msg[0] = '\0';
			(void) sprintf(msg, "retrieve_granules_list: invalid temporal_type %d",
			               curr_dataset->temporal_type);			
			(void) ims_msg (msgDesc, IMS_FATAL, msg);
			strcpy (v0Desc->odl_status, "19");
			return (IMS_FATAL);

			break;

	}

	/*
	** get keywords for each spatial_type, i.e. 1=point, 2= rectangular, 
	** 3= point&range (not used), 4=four corner
	*/

	switch (curr_dataset->spatial_type)
	{
		case 1:

			/*
			** point
			*/

			/*
			** CENTER_LAT or alike
			*/
			query_type = CENTER_LAT_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			curr_dataset->spatial_key_list 
				= (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;

			/*
			** CENTER_LON or alike
			*/
			query_type = CENTER_LON_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			curr_dataset->spatial_key_list->next_p
				= (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;
			
			break;

		case 2:

			/*
			**  rectangle
			*/

			/*
			** NORTH_LAT or alike
			*/	
			query_type = NORTH_LAT_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr = (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;

			last_ptr = first_ptr = new_ptr;
			curr_dataset->spatial_key_list = first_ptr;

			/*
			** SOUTH_LAT or alike
			*/	
			query_type = SOUTH_LAT_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr = (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;

			last_ptr->next_p = new_ptr;
			last_ptr = last_ptr->next_p;

			/*
			** WEST_LON or alike
			*/
			query_type = WEST_LON_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr = (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;
			
			last_ptr->next_p = new_ptr;
			last_ptr = last_ptr->next_p;

			/*
			** EAST_LON or alike
			*/
			query_type = EAST_LON_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr = (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;

			last_ptr->next_p = new_ptr;
			last_ptr = last_ptr->next_p;

			break;

		case 0:
		case 3:
			/*
			** These are valid spatial_type, but we ignore it for now because V0
			** does not support it.  -jlw
			*/
			break;

		case 4:

			/*
			** four corner
			*/

			/*
			** NEAR_START_LAT or alike
			*/	
			query_type = NEAR_START_LAT_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr = (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;

			last_ptr = first_ptr = new_ptr; 
			curr_dataset->spatial_key_list = first_ptr; 

			/*
			** NEAR_START_LON or alike
			*/	
			query_type = NEAR_START_LON_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr = (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;

			last_ptr->next_p = new_ptr;
			last_ptr = last_ptr->next_p;
			

			/*
			** NEAR_END_LAT or alike
			*/
			query_type = NEAR_END_LAT_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr = (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;
			
			last_ptr->next_p = new_ptr;
			last_ptr = last_ptr->next_p;
			
			/*
			** NEAR_END_LON or alike
			*/
			query_type = NEAR_END_LON_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr = (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;

			last_ptr->next_p = new_ptr;
			last_ptr = last_ptr->next_p;
			
			/*
			** FAR_START_LAT or alike
			*/
			query_type = FAR_START_LAT_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr = (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;
			
			last_ptr->next_p = new_ptr;
			last_ptr = last_ptr->next_p;
			
			/*
			** FAR_START_LON or alike
			*/
			query_type = FAR_START_LON_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr = (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;
			
			last_ptr->next_p = new_ptr;
			last_ptr = last_ptr->next_p;
			
			/*
			** FAR_END_LAT or alike
			*/
			query_type = FAR_END_LAT_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr = (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;
			
			last_ptr->next_p = new_ptr;
			last_ptr = last_ptr->next_p;
			
			/*
			** FAR_END_LON or alike
			*/
			query_type = FAR_END_LON_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr	= (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;
			
			last_ptr->next_p = new_ptr;
			last_ptr = last_ptr->next_p;
			
			/*
			** CENTER_LAT or alike
			*/
			query_type = CENTER_LAT_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr	= (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;

			last_ptr->next_p = new_ptr;
			last_ptr = last_ptr->next_p;
			
			/*
			** CENTER_LON or alike
			*/
			query_type = CENTER_LON_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr	= (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;
			
			last_ptr->next_p = new_ptr;
			last_ptr = last_ptr->next_p;
			
			/*
			** ASC_DESC or alike
			*/
			query_type = ASC_DESC_TYPE;
			catReq->item[0] = (void *) &(curr_dataset->dataset_idx);
			catReq->item[1] = (void *) &query_type;
			if ( (status = v0_cat (catReq, V0_GETKEYWORDTYPE)) < IMS_OK)
			{
				strcpy (v0Desc->odl_status, "19");
				return (status);
			}
			new_ptr	= (V0_KEYWORD_LIST *)catReq->item[2];
			curr_dataset->spatial_key_count++;
			
			last_ptr->next_p = new_ptr;
			last_ptr = last_ptr->next_p;
			
			break;

		default:
	
			msg[0] = '\0';
			(void) sprintf(msg, "retrieve_granules_list: invalid spatial_type %d",
			               curr_dataset->spatial_type);			
			(void) ims_msg (msgDesc, IMS_FATAL, msg);
			strcpy (v0Desc->odl_status, "19");
			return (IMS_FATAL);
				
			break;
	}

	/*
	** get a list of keywords that will be sent to V0 as detailed metadata
	** infomation, to be displayed in the detail information screen
	** 
	** these keywords have one of the following query types 
	** 3(CENTER_TIME), 4(CENTER_LAT), 5(CENTER_LON), 18(ASC_DESC), 
	** 19(FRAME_STATUS), 21(DETAILED_DATA)
	*/
	v0Desc->query.sql[0]    =  '\0';
	(void) sprintf (v0Desc->query.sql,
		"select kp.keyword, kp.data_type from keyword_policy kp, keyword_set ks"
		" where ks.dataset_idx = %d and ks.query_type in (%d, %d, %d, %d, %d, %d)"
		" and kp.keyword_idx = ks.keyword_idx order by ks.position",
		curr_dataset->dataset_idx,
			CENTER_TIME_TYPE,
			CENTER_LAT_TYPE,
			CENTER_LON_TYPE,
			ASC_DESC_TYPE,
			FRAME_STATUS_TYPE,
			DETAILED_DATA_TYPE);

	catReq->item[0] = (void *)v0Desc->query.sql;
	catReq->item[1] = (int *)&(curr_dataset->detail_key_count);
	
#	ifdef QDEBUG
		sprintf (msg,"\n\nsql ==>%s", v0Desc->query.sql);
		(void) ims_msg(msgDesc, IMS_INFO, msg);
#	endif

	if ( (status = v0_cat (catReq, V0_GETDETAILKEYWORD)) < IMS_OK)
	{
		msg[0] = '\0';
		(void) ims_msg(msgDesc, status, 
					 "retrieve_granules_list: failed to retrieve detail keywords.");
		strcpy (v0Desc->odl_status, "19");
		return (status);
	}

	/*
	** item[2] points to the beginning of the detail keyword list
	*/
 	curr_dataset->detail_key_list = 
		(V0_KEYWORD_LIST *)catReq->item[2];

	/*
	** reinitialize the query structure 
	*/
	v0Desc->query.select[0] =  '\0';
	v0Desc->query.from[0]   =  '\0';
	v0Desc->query.where[0]  =  '\0';
	v0Desc->query.sql[0]    =  '\0';

	/*
	** build a query to extract granules from the associating 
	** granule table for this dataset 
	**
	** IMS_FATAL is returned if any system error occurs
	** IMS_ERROR is returned if user's seasonal and temporal search time
	**     ranges do not intersect.  
	**     	
	*/
	if ( (status = v0_query__build_granule (v0Desc)) < IMS_OK )
	{
		if (status == IMS_ERROR)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"retrieve_granules_list: repeating seasonal interval does not intersect with request start/stop time.");
			strcpy (v0Desc->odl_status, "02");
		}
		else if (status == IMS_FATAL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"retrieve_granules_list: invalid temporal type.");
			strcpy (v0Desc->odl_status, "19");
		} 
		return (status);
	}

#	ifdef QDEBUG
		sprintf (msg,"\n\nsql ==>%s", v0Desc->query.sql);
		(void) ims_msg(msgDesc, IMS_INFO, msg);
#	endif

	/* 
	** Initiallize catalog request structure 
	*/
	catReq = &v0Desc->catReq;
	catReq->msgDesc = msgDesc;

	/* 
	** set a maximum limit of granules retrieved from the catalog  
	** For performance, we only retrieve matching granules to 
	** the granule_limit set by the user.  Just in case any granules
	** got disqualified in the refined spatial search, the maximum
	** granule count is set to be twice the number of user defined
	** granule_limit
	*/
	max_granule = v0Desc->request.granule_limit*2;

	/* item[0] points to the sql statement */
	catReq->item[0] = (void *)v0Desc->query.sql;

	/*
	** item[1] will receive the number of granules 
	*/
	catReq->item[1] = (void *)&(curr_dataset->granule_count);
	catReq->item[3] = (void *)max_granule;
	catReq->item[4] = (void *)curr_dataset->spatial_type;
	catReq->item[5] = (void *)curr_dataset->detail_key_count;
	catReq->item[6] = (void *)curr_dataset->detail_key_list;

	/*
	** execute the query to get the granule list
	*/
	if ( (status = v0_cat (catReq, V0_EXECGRANULESEARCH)) < IMS_OK)
	{
		strcpy (v0Desc->odl_status, "19");
		return (status);
	}

	/*
	** item[2] points to the beginning of the granule list
	*/
 	curr_dataset->granule_list = 
		(V0_GRANULE_LIST *)catReq->item[2];

	/*
	** convert time format to a time string recognizable by ODL
	*/
	g_ptr = curr_dataset->granule_list;
	while (g_ptr != (V0_GRANULE_LIST *)NULL)
	{
		(void) ims_timeToNumericDate (msgDesc, g_ptr->start_time, &temp_date);
		(void) ims_numericDateToV0A  (&temp_date, g_ptr->start_time);
		(void) ims_timeToNumericDate (msgDesc, g_ptr->stop_time, &temp_date);
		(void) ims_numericDateToV0A  (&temp_date, g_ptr->stop_time);

		/*
		** The keywords and values are concatenated and formatted into max 
		** 60 characters long strings. 
		*/
		if ( (format_detail_info (msgDesc, g_ptr)) < IMS_OK)
		{
			msg[0] = '\0';
			(void) ims_msg(msgDesc, status, 
					 "retrieve_granules_list: failed to format detail keywords.");
			strcpy (v0Desc->odl_status, "19");
			return (IMS_FATAL);
		}

		g_ptr = g_ptr->next_p;
	}

	/*
	** If spatial search was activated, we have to process the preliminary
	** granule list furthur more (second pass of spatial search) to do a 
	** one to one comparison of the user region and granule region.
	**
	** The first pass of search takes the min/max lat/lon of the user region
	** and retrieve granules which overlap with the min/max 'box'.  The result
	** may hit granules that do not overlay with the original user region.
	** The second pass of spatial search does a detail one-to-one checking on
	** the granules resulted from the first pass to ensure lower error rate
	**
	**
	** IMPORTANT:  we have problem in doing a clean spatial search of POINT
	**             granules in pass 1.  Our algorithm picks up all point 
	**             granules within the range of min/max latitudes around the
	**             world. Therefore the call for refine search is a MUST for 
	**             point granules, i.e. spatial type = 1, even if v0 users be 
	**             given a choice to disable the pass 2 spatial search 
	**             in the future.  
	*/
	if ( (curr_dataset->granule_list != (V0_GRANULE_LIST *)NULL) &&
		   (v0Desc->request.global_granules_p != 'Y') &&
			 ( ((v0Desc->request.region_type == POINT_LOC) && 
				  (v0Desc->result.curr_dataset->spatial_type == 1)) ||
			   ((v0Desc->request.region_type == POINT_LOC) && 
				  (v0Desc->result.curr_dataset->spatial_type == 4)) ||
			   ((v0Desc->request.region_type == RANGE_LOC) && 
				  (v0Desc->result.curr_dataset->spatial_type == 1)) ||
			   (v0Desc->request.region_type == POLYGON_LOC)   ) )
	{
		if ( (status = v0_spatial__refine_srch (v0Desc)) != IMS_OK)
		{
			/* 
			** message has been logged in ims_v0Spatial        
			*/
			strcpy (v0Desc->odl_status, "19");
			return (IMS_FATAL);
		}
	}

#ifdef SDEBUG
	else if ( (curr_dataset->granule_list != (V0_GRANULE_LIST *)NULL) &&
				 (v0Desc->request.global_granules_p != 'Y') )
 	{
		printf ("\n==>skip pass 2 ... the result granule list is:");

		g_ptr = curr_dataset->granule_list;
		while (g_ptr != (V0_GRANULE_LIST *)NULL)
		{
			printf ("\n    %s", g_ptr->granule_id);
			g_ptr = g_ptr->next_p;
		}
	}
#endif

	/*
	** sort the result by start_time 
	*/
	if ( curr_dataset->granule_list != (V0_GRANULE_LIST *)NULL)
	{
		status = ims_v0SortList (msgDesc, curr_dataset->granule_list, 
						&temp);
		
		if ( (temp == (V0_GRANULE_LIST *)NULL) || 
				 (status < IMS_OK))
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"retrieve_granules_list: granule result list sorting failed."); 
			strcpy (v0Desc->odl_status, "19");
			return (IMS_FATAL);
		}

		curr_dataset->unordered_granule_list = curr_dataset->granule_list;
		curr_dataset->granule_list = temp;
	}

	return (IMS_OK);

} /* end of retrieve_granules_list */

/***********************************************************************
**      
** retrive_parameter_list - Build a query based on contents in v0Desc, and 
**                   execute it to retrieve a list of parameter information
**	                  for the current dataset from database
**
** called by: v0_process
**
************************************************************************/           
static int retrieve_parameter_list (V0_DESC_STRUCT *v0Desc)
{
	int status;
	IMS_MSG_STRUCT *msgDesc;
	V0_CAT_STRUCT *catReq;        /* catalog request structure */
	char msg[IMS_COL1024_LEN*11];
	int  rowCount;

	/*
	** initialization
	*/
	msgDesc = v0Desc->msgDesc;

	/*
	** cleanup the query structure 
	*/
	strcpy (v0Desc->query.select, "\0");
	strcpy (v0Desc->query.from, "\0");
	strcpy (v0Desc->query.where, "\0");
	strcpy (v0Desc->query.sql, "\0");

	/*
	** build a query to extract parameter values from the dataset_parameter 
	** table for this dataset 
	*/

	(void) sprintf (v0Desc->query.sql, 
		        "select parameter from dataset_parameter where dataset_idx = %d", 
		        v0Desc->result.curr_dataset->dataset_idx);

#	ifdef QDEBUG
		sprintf (msg,"\n\nsql ==> %s", v0Desc->query.sql);
		(void) ims_msg(msgDesc, IMS_INFO, msg);
#	endif
				

	/* 
	** Initiallize catalog request structure 
	*/
	catReq = &v0Desc->catReq;
	catReq->msgDesc = msgDesc;

	/* item[0] points to the sql statement */
	catReq->item[0] = (void *)v0Desc->query.sql;
	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	/*
	** execute the query to get the parameter list
	*/
	if ( (status = v0_cat (catReq, V0_GETSTR)) < IMS_OK)
	{
		(void)ims_msg (msgDesc, status, "retrieve_parameter_list: failed to get parameter list");
		strcpy (v0Desc->odl_status, "19");
		return (status);
	}

	/*
	** item[2] points to the beginning of the parameter list
	** it's okay if nothing found
	*/
 	v0Desc->result.curr_dataset->parameter = 
		(V0_VALUE_LIST *)catReq->item[2];

	if (rowCount == 0)
	{
		v0Desc->result.curr_dataset->parameter = (V0_VALUE_LIST *)NULL;
	}

	return (IMS_OK);

} /* end of retrieve_parameter_list */


/***********************************************************************
**      
** retrive_account_list - Build a query and execute it to retrieve 
**                        account ids associating with the user who
**                        holds the authenticator provided from the 
**                        INVENTORY_SEARCH.  These account info will be
**                        returned to the client in VALID_ACCOUNTS group.
**
** Note: if no authenticator found for the user or no matching account id
**       found, at least an error message will be sent in VALID_ACCONTS 
**       group, so some information will be given to the user.
** 
**       Individual error message should be given to any account that has 
**       negative balance.
**
**
** called by: v0_process
**
************************************************************************/           
static int retrieve_account_list (V0_DESC_STRUCT *v0Desc)
{
	int status;
	IMS_MSG_STRUCT *msgDesc;
	V0_CAT_STRUCT *catReq;        /* catalog request structure */
	char msg[IMS_COL1024_LEN*11];
	V0_USER_ACCT_LIST *temp_ptr;
	V0_VALUE_LIST     *uh, *ut;
	char uso_phone[IMS_COL30_LEN+1], uso_email[IMS_COL128_LEN+1];
	int rowCount;

	/*
	** initialization
	*/
	msgDesc = v0Desc->msgDesc;

	/*
	** execute query to get the account list
	*/
	if (v0Desc->request.authenticator[0] != '\0')
	{
		/*
		** cleanup the query structure 
		*/
		strcpy (v0Desc->query.select, "\0");
		strcpy (v0Desc->query.from, "\0");
		strcpy (v0Desc->query.where, "\0");
		strcpy (v0Desc->query.sql, "\0");


		/* 
		** Initiallize catalog request structure 
		*/
		catReq = &v0Desc->catReq;
		catReq->msgDesc = msgDesc;

		/*
		** extract account ids and their current balance based on the authenticator
		** sent together with the INVENTORY SEARCH
		*/
		(void) sprintf (v0Desc->query.sql,
			"exec v0_get_account_list '%s', %d", 
			v0Desc->request.authenticator,
			v0Desc->result.curr_dataset->dataset_idx);

		/* item[0] points to the sql statement */
		catReq->item[0] = (void *)v0Desc->query.sql;

		rowCount = 0;
		catReq->item[1] = (int *)&rowCount;

#	ifdef QDEBUG
		sprintf (msg,"\n\nsql ==> %s", v0Desc->query.sql);
		(void) ims_msg(msgDesc, IMS_INFO, msg);
#	endif
				

		if ( (status = v0_cat (catReq, V0_GETACCTLIST)) < IMS_OK)
		{
			(void)ims_msg (msgDesc, status, "retrieve_account_list: failed to get account list");
			strcpy (v0Desc->odl_status, "19");
			return (status);
		}

		/*
		** item[2] points to the beginning of the account list
		*/
 		v0Desc->result.curr_dataset->account_list = 
			(V0_USER_ACCT_LIST *)catReq->item[2];
	}

	/*
	** if no authenticator provided or no account matched with the authenticator
	** we'll create an error message and return IMS_OK
	*/
	if ( (v0Desc->request.authenticator[0] == '\0') || (rowCount == 0))
	{
		/* query the ASF user service contacting information to be put in the
			 error message */
		strcpy (v0Desc->query.select, "\0");

		(void) strcpy (v0Desc->query.sql,
			"select phone, email from user_profile where user_id = 'ASF_REP'"); 

		/* item[0] points to the sql statement */
		catReq->item[0] = (void *)v0Desc->query.sql;

#	ifdef QDEBUG
		sprintf (msg,"\n\nsql ==> %s", v0Desc->query.sql);
		(void) ims_msg(msgDesc, IMS_INFO, msg);
#	endif

		if ( (status = v0_cat (catReq, V0_GETSTR2)) < IMS_OK)
		{
			(void)ims_msg (msgDesc, status, "retrieve_account_list: failed to get user service info");
			strcpy (v0Desc->odl_status, "19");
			return (status);
		}

 		uh = (V0_VALUE_LIST *)catReq->item[2];

		(void) strcpy (uso_phone, uh->char_value1);
		(void) strcpy (uso_email, uh->char_value2);

		while ( uh != (V0_VALUE_LIST *)NULL)
		{
			ut = uh->next_p;
			free (uh);
			uh = ut;
		}

		/*
		** create an error message, the maximum length for a message line is 80
		** based on v0 Message Data Dictionary and the message can be multiple lines
		*/
		if ( ( temp_ptr = (V0_USER_ACCT_LIST *)
      	malloc (sizeof(V0_USER_ACCT_LIST))) ==NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
		  	"v0_process__retrieve_account_list: Memory allocation failed for temp_ptr");
			strcpy (v0Desc->odl_status, "19");
			return(IMS_FATAL);
		}
	
		if ( ( temp_ptr->err_msg = (V0_ERR_LIST *)
      	malloc (sizeof(V0_ERR_LIST))) ==NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
		  	"v0_process__retrieve_account_list: Memory allocation failed for err_msg");
			strcpy (v0Desc->odl_status, "19");
			return(IMS_FATAL);
		}
	
		temp_ptr->account_id[0]= '\0';
		temp_ptr->balance      = 0.0;
		temp_ptr->next_p = (V0_USER_ACCT_LIST *)NULL;

		strcpy (temp_ptr->err_msg->err_buf, "*** ASF restricted data, need account to request product.");
		temp_ptr->err_msg->next_p = (V0_ERR_LIST *)NULL;
			
		if ( ( temp_ptr->err_msg->next_p = (V0_ERR_LIST *)
      	malloc (sizeof(V0_ERR_LIST))) ==NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
		  	"v0_process__retrieve_account_list: Memory allocation failed for err_msg->next_p");
			strcpy (v0Desc->odl_status, "19");
			return(IMS_FATAL);
		}
	
		(void)sprintf (temp_ptr->err_msg->next_p->err_buf, 
				"*** Contact %s or %s", uso_phone, uso_email);
		temp_ptr->err_msg->next_p->next_p = (V0_ERR_LIST *)NULL;

		v0Desc->result.curr_dataset->account_list = temp_ptr;

	}

	/*
	** even if something found, we'll still check if balance is below zero 
	*/
	else
	{
		temp_ptr = v0Desc->result.curr_dataset->account_list;

		while (temp_ptr != (V0_USER_ACCT_LIST *)NULL)
		{
			if (temp_ptr->balance <= 0.0)
			{
				if ( ( temp_ptr->err_msg = (V0_ERR_LIST *)
      			malloc (sizeof(V0_ERR_LIST))) ==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
		  			"v0_process__retrieve_account_list: Memory allocation failed for err_msg");
					strcpy (v0Desc->odl_status, "19");
					return(IMS_FATAL);
				}
	
				strcpy (temp_ptr->err_msg->err_buf, "Warning: insufficient account balance.");
				temp_ptr->err_msg->next_p  = (V0_ERR_LIST *)NULL;
			}
			else
			{
				temp_ptr->err_msg = (V0_ERR_LIST *)NULL;
			}

			temp_ptr = temp_ptr->next_p;
		}
		
	}

	return (IMS_OK);
	
} /* end of retrieve_account_list */

/***********************************************************************
**      
** retrive_dataset_info - Build a query based on contents in v0Desc, and 
**                   execute it to retrieve dataset comment and restriction 
**	                 information for the current dataset
**
** called by: v0_process
**
************************************************************************/           
static int retrieve_dataset_info (V0_DESC_STRUCT *v0Desc)
{
	int status;
	IMS_MSG_STRUCT *msgDesc;
	V0_CAT_STRUCT *catReq;        /* catalog request structure */
	V0_COMMENT_LIST *hc, *tc;
	char *q;
	char msg[IMS_COL1024_LEN];
	char base_query[IMS_COL1024_LEN];
	int  rowCount;

	/*
	** initialization
	*/
	msgDesc = v0Desc->msgDesc;
	v0Desc->result.dataset_comment[0] = '\0';
	v0Desc->result.dataset_restriction[0] = '\0';

	/*
	** cleanup the query structure 
	*/
	strcpy (v0Desc->query.select, "\0");
	strcpy (v0Desc->query.from, "\0");
	strcpy (v0Desc->query.where, "\0");
	strcpy (v0Desc->query.sql, "\0");

	/*
	** build a query to retrieve dataset comment and restriction 
	** information from the dataset_info table for this dataset 
	*/
	q = v0Desc->query.sql;

	(void)sprintf (q, "exec v0_get_dataset_info '%s'", 
	                  v0Desc->result.curr_dataset->platform );
	q = q + strlen (q);

	if (v0Desc->result.curr_dataset->sensor[0] == '\0')
	{
		(void)strcpy (q, ", null");
	}
	else
	{
		(void)sprintf (q, ", '%s'", v0Desc->result.curr_dataset->sensor);
	}
	q = q + strlen (q);
	
	if (v0Desc->result.curr_dataset->dataset_id[0] == '\0')
	{
		(void)strcpy (q, ", null");
	}
	else
	{
		(void)sprintf (q, ", '%s'", v0Desc->result.curr_dataset->dataset_id);
	}
	q = q + strlen (q);
	
	/*
	** base query is reusable 
	*/
	strcpy (base_query, v0Desc->query.sql);

	(void) sprintf (q, ", %d", DS_INFO_COMMENT);
	q = q + strlen(q);

#	ifdef QDEBUG
		sprintf (msg,"\n\nsql ==> %s", v0Desc->query.sql);
		(void) ims_msg(msgDesc, IMS_INFO, msg);
#	endif

	/* 
	** Initiallize catalog request structure 
	*/
	catReq = &v0Desc->catReq;
	catReq->msgDesc = msgDesc;

	/* item[0] points to the sql statement */
	catReq->item[0] = (void *)v0Desc->query.sql;
	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	/*
	** execute the query to get the dataset comment 
	*/
	if ( (status = v0_cat (catReq, V0_GETDATASETINFO)) < IMS_OK)
	{
		(void)ims_msg (msgDesc, status, 
								"retrieve_dataset_info: failed to get dataset comment");
		strcpy (v0Desc->odl_status, "19");
		return (status);
	}

	/*
	** item[2] points to the beginning of the dataset comment, it's okay if 
	** no result is returned.
	*/
	if (rowCount > 0)
	{
		hc = (V0_COMMENT_LIST *)catReq->item[2];

		strcpy (v0Desc->result.dataset_comment, hc->comment_info);

		while (hc != (V0_COMMENT_LIST *)NULL)
		{
			tc = hc->next_p;
			free(hc);
			hc = tc;
		}
	}

	/*
	** construct the query for dataset restriction using base query
	*/
	(void) strcpy (v0Desc->query.sql, "\0");
	(void) sprintf (v0Desc->query.sql, "%s, %d", base_query, DS_INFO_RESTRICTION);

#	ifdef QDEBUG
		sprintf (msg,"\n\nsql ==> %s", v0Desc->query.sql);
		(void) ims_msg(msgDesc, IMS_INFO, msg);
#	endif

	catReq = &v0Desc->catReq;
	catReq->msgDesc = msgDesc;

	/* item[0] points to the sql statement */
	catReq->item[0] = (void *)v0Desc->query.sql;
	rowCount = 0;
	catReq->item[1] = (int *)&rowCount;

	/*
	** execute the query to get the dataset restriction 
	*/
	if ( (status = v0_cat (catReq, V0_GETDATASETINFO)) < IMS_OK)
	{
		(void)ims_msg (msgDesc, status, "retrieve_dataset_info: failed to get dataset restriction.");
		strcpy (v0Desc->odl_status, "19");
		return (status);
	}

	/*
	** item[2] points to the beginning of the dataset restriction, it's okay if 
	** no result is returned.
	*/
	if (rowCount > 0)
	{

		hc = (V0_COMMENT_LIST *)catReq->item[2];

		strcpy (v0Desc->result.dataset_restriction, hc->comment_info);

		while (hc != (V0_COMMENT_LIST *)NULL)
		{
			tc = hc->next_p;
			free(hc);
			hc = tc;
		}
	}

	return (IMS_OK);

} /* end of retrieve_dataset_info */

/***********************************************************************
**
** remove_newlines - remove newline character and repace \" with '
**
************************************************************************/           
static int remove_newlines (char *str)
{
	int ii, jj;
	char out_str[IMS_COL512_LEN+1];
	char new_str[IMS_COL512_LEN+1];

	out_str[0] = '\0';
	new_str[0] = '\0';

	ii = 0;    /* counter pointing to original string */
	jj = 0;    /* counter pointing to the new string */

	while (str[ii] != '\0')
	{
		if ( (str[ii] == '\\') && (str[ii+1] == 'n'))
		{
			ii = ii + 2;	
		}
		else if ( (str[ii] == '\\') && (str[ii+1] == '"') )
		{
			out_str[jj] = '\'';
			ii = ii+2;
			jj++;
		}
		else
		{
			out_str[jj] = str[ii];
			ii++;
			jj++;
		}
	}

	out_str[jj] = '\0';

	(void) ims_formatQuotedString (out_str, new_str);

	strcpy (str, new_str);

	return (IMS_OK);
} /* end of remove_newlines */

/***********************************************************************
**
** build_dlink_list - make the dataset list into a doubly link list 
**
************************************************************************/           
static int build_dlink_list (V0_DESC_STRUCT *v0Desc)
{
	V0_DATASET_LIST *list1, *list2; /* list1 points to the previous list
																		 list2 points to the current list */


	if ((v0Desc->result.dataset_list == (V0_DATASET_LIST *)NULL) ||
			(v0Desc->result.dataset_list->next_p == (V0_DATASET_LIST *)NULL))
	{
		return(IMS_OK);
	}

	list1 = v0Desc->result.dataset_list;
	list2 = v0Desc->result.dataset_list->next_p;

	while (list2 !=(V0_DATASET_LIST *)NULL)
	{
		list2->prev_p = list1;
		list2 = list2->next_p;
		list1 = list1->next_p;

	}

	return(IMS_OK);
} /* end of build_dlink_list */

/***********************************************************************
**
** v0_process__prepare_string -   
**
************************************************************************/
int v0_process__prepare_string (char *str)
{
	(void)remove_newlines(str);
	(void)ims_truncStr(str);
	(void)ims_toUpper(str);

	return (IMS_OK);
} /* end of v0_process__prepare_string */

/***********************************************************************
**
** init_line_item_list -   
**
************************************************************************/
static int init_line_item_list (V0_LINE_ITEM_LIST *line_item_ptr) 
{
	/* initialize line_item_list fields */
	line_item_ptr->order_item_type      = 0;
	line_item_ptr->dataset_id[0]        = '\0';
	line_item_ptr->dataset_idx          = 0;
	line_item_ptr->granule_id[0]        = '\0';
	line_item_ptr->granule_idx          = 0;
	line_item_ptr->data_kbytes          = 0;
	line_item_ptr->metadata_kbytes      = 0;
	line_item_ptr->v0_process_type[0]   = '\0';
	line_item_ptr->process_type         = 0;
	line_item_ptr->v0_media_type[0]     = '\0';
	line_item_ptr->media_type           = 0;
	line_item_ptr->media_class          = 0;
	line_item_ptr->v0_media_fmt_type[0] = '\0';
	line_item_ptr->media_fmt_type       = 0;
	line_item_ptr->media_label[0]       = '\0';
	line_item_ptr->line_item_cost       = 0.0;
	line_item_ptr->account_id[0]        = '\0';
	line_item_ptr->platform[0]          = '\0';
	line_item_ptr->sensor_name[0]       = '\0';
	line_item_ptr->pi_name[0]           = '\0';
	line_item_ptr->pi_discipline[0]     = '\0';
	line_item_ptr->quicklook_p[0]       = '\0';
	line_item_ptr->revolution           = 0;
	line_item_ptr->sequence             = 0;
	line_item_ptr->time_on[0]           = '\0';
	line_item_ptr->time_off[0]          = '\0';
	line_item_ptr->mode[0]              = '\0';
	line_item_ptr->frame_mode[0]        = '\0';
	line_item_ptr->activity_id[0]       = '\0';
	line_item_ptr->station_id[0]        = '\0';
	line_item_ptr->site_name[0]         = '\0';
	line_item_ptr->next_p               = (V0_LINE_ITEM_LIST *)NULL;

	return (IMS_OK);

} /* end of init_line_item_list */

/*****************************************************************************
**
** format_detail_info -
**        formats detail metadata information to be return to V0
**
** Formatting rule:
** 
** - Keyword name and its matching value should be kept within the same line.
** - Quotes will be added around character type values. 
** - In the case that one keyword/value combination is larger than maximum
**   ODL string limit, a new line is initiated to store the string. 
**   Value exceeded the maximum string limit will be truncated and '...' will
**   be added as indication of incompleteness.  
**
******************************************************************************/

static int format_detail_info (IMS_MSG_STRUCT *msgDesc, 
                               V0_GRANULE_LIST *g_ptr)
{
	V0_VALUE_LIST *temp_ptr;      /* points to the original string */
	V0_VALUE_LIST *curr_ptr;      /* points to the formated string */
	int  curr_length;             /* number of characters has been inserted to
																	 the current line */
	int  format_started;          /* indicates the first line of formatted text*/
	char *t;                      /* points to the next insert point */	

	(void) prepare_keyword_string (g_ptr->detailed_keyword);

	curr_length = 0;
	format_started = 0;
	temp_ptr = g_ptr->detailed_keyword;

	while (temp_ptr != (V0_VALUE_LIST *)NULL)
	{
		/*
		** a new structure has to be allocated in one of the following conditions:
		** - begining of formatting
		** - the current keyword/value combination require a whole line 
		** - the line may exceed maximum string length after adding the current
		**    keyword/value.
		*/
		if ( (!format_started) ||
				 ( (temp_ptr->int_value==V0_MAX_COMMENT_LENGTH) && (format_started)) ||
				 ( (curr_length + temp_ptr->int_value) > V0_MAX_COMMENT_LENGTH ))
		{
			/*
			** The very first value entry
			*/
			if (!format_started)
			{
				if ( (curr_ptr = (V0_VALUE_LIST *)malloc(sizeof (V0_VALUE_LIST)))
						 == NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL, 
					    "v0_process__format_detail_info: failed to allocate"
							" memory space." );
					return (IMS_FATAL);
				}

				curr_ptr->next_p = (V0_VALUE_LIST *)NULL;
				g_ptr->formatted_list = curr_ptr;
				
				format_started = 1;
			}
			else
			{
				curr_ptr->char_value1[curr_length] = '\0';

				if ( (curr_ptr->next_p = (V0_VALUE_LIST *)malloc(sizeof(V0_VALUE_LIST)))
						 == NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL, 
					    "v0_process__format_detail_info: failed to allocate"
							" memory space." );
					return (IMS_FATAL);
				}

				curr_ptr = curr_ptr->next_p;
				curr_ptr->next_p = (V0_VALUE_LIST *)NULL;

			}

			/*
			** initialize the text string, text pointer, and lenghth 
			*/
			curr_ptr->char_value1[0] = '\0';
			t = curr_ptr->char_value1;
			curr_length = 0;
		}

		sprintf (t, "%s", temp_ptr->char_value3);
		curr_length = curr_length + temp_ptr->int_value;
		t = t + strlen(t);

		if (curr_length < V0_MAX_COMMENT_LENGTH)
		{
			strcpy (t, " ");
			t = t + strlen(t);
			curr_length++;
		}

		temp_ptr = temp_ptr->next_p;
	}

	curr_ptr->char_value1[curr_length] = '\0';

	return (IMS_OK);

} /* end of format_detail_info */

/*****************************************************************************
**
** prepare_keyword_string -- prepackages detail metadata information to be:
**
**                <keyword>=<value>;           
**                or
**                <keyword>=<value>...;       <-- value exceed max length 
**
**           and calculates total length of the information including 
**           punctuations.
**
** input:  a linked list in V0_VALUE_LIST type containing the following info:
**
**                char_value1     <-- name of the keyword
**                char_value2     <-- value of keyword (in Ascii)
**                
** ouptput: tha same linked list with the following fields filled
**
**                char_value3     <-- packaged keyword/value set
**                int_value       <-- total length of the keyword/value set
**
******************************************************************************/
static int prepare_keyword_string (V0_VALUE_LIST *keyword_list)
{

	char *k;
	V0_VALUE_LIST *t_ptr;

	t_ptr = keyword_list;

	while (t_ptr != (V0_VALUE_LIST *)NULL)
	{
		t_ptr->char_value3[0] = '\0';
		k = t_ptr->char_value3;

		sprintf (k, "%s=%s;", t_ptr->char_value1, t_ptr->char_value2 );
		k = k + strlen (k);

		/*
		** truncate a long character value if needed
		*/
		if ( (int)strlen(t_ptr->char_value3) > V0_MAX_COMMENT_LENGTH)
		{
			t_ptr->char_value3[V0_MAX_COMMENT_LENGTH-2] =
			t_ptr->char_value3[V0_MAX_COMMENT_LENGTH-3] =
			t_ptr->char_value3[V0_MAX_COMMENT_LENGTH-4] = '.';
			t_ptr->char_value3[V0_MAX_COMMENT_LENGTH-1] = ';';
			t_ptr->char_value3[V0_MAX_COMMENT_LENGTH]   = '\0';
		}

		t_ptr->int_value = strlen(t_ptr->char_value3);

		/*
		** look ahead to remove the ending ';' in the last value
		*/
		if (t_ptr->next_p == (V0_VALUE_LIST *)NULL)
		{
			t_ptr->char_value3[strlen(t_ptr->char_value3)-1] = '\0';
		}

		t_ptr = t_ptr->next_p;	
	}

	return (IMS_OK);
	
} /* end of prepare_keyword_string */

/*****************************************************************************
**
** prepare_wildcard_string -- 
**
******************************************************************************/
static int prepare_wildcard_string (V0_VALUE_LIST *granule_id)
{
	int  len, i;
	int  j;
	char temp_string[IMS_COL255_LEN+1];

	len = (int) strlen(granule_id->char_value1);
	temp_string[0] = '\0';
	j = 0;

	for (i=0; i < len; i++) 
	{
		if (granule_id->char_value1[i] == '*')
		{
			temp_string[j++] = '%';
		}
		else if (granule_id->char_value1[i] == '%')
		{
			temp_string[j++] = '[';
			temp_string[j++] = '%';
			temp_string[j++] = ']';
		}
		else if (granule_id->char_value1[i] == '_')
		{
			temp_string[j++] = '[';
			temp_string[j++] = '_';
			temp_string[j++] = ']';
		}
		else if (granule_id->char_value1[i] == '[')
		{
			temp_string[j++] = '[';
			temp_string[j++] = '[';
			temp_string[j++] = ']';
		}
		else if (granule_id->char_value1[i] == ']')
		{
			temp_string[j++] = '[';
			temp_string[j++] = ']';
			temp_string[j++] = ']';
		}
		else	
		{
			temp_string[j++] = granule_id->char_value1[i];
		}
	}

	if ((len > 0) && (temp_string[j-1] == ','))
	{
		temp_string[j-1] = '\0';
	}
	else
	{
		temp_string[j] = '\0';
	}
	strcpy (granule_id->char_value1, temp_string);

	return (IMS_OK);
	
} /* end of prepare_wildcard_string */
