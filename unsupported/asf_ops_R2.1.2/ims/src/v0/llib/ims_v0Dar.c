static char *sccs = "@(#)ims_v0Dar.c	1.2  11/14/97";

/************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** ims_v0Dar.c    : Process DAR Queries from DARnet.
**
**	Creator   :   Thuy Tran 
**
**	Date      :   April 18, 1997
**
** Modifications:
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
#include <ims_cmnQuery.h>
#include <ims_v0.h>
#include <ims_acct.h>

#define	 MAX_DAR_INFO_ITEMS	30
#define  MAX_DAR_CONTENT_ITEMS	15
#define  MAX_DAR_GRANULE_ITEMS 	15
#define	 DAR_OK_STATUS_CODE	"01"
#define  DAR_NO_MATCH_FOUND     "02"
#define	 DAR_ODL_ERROR_CODE	"09"
#define  DAR_SYSTEM_ERROR_CODE	"19"
#define	 DAR_USER_ERROR_CODE	"99"

static int construct_list_query(IMS_MSG_STRUCT *, V0_DESC_STRUCT *);
static int construct_status_query(IMS_MSG_STRUCT *, V0_DESC_STRUCT *);
static int construct_content_query(IMS_MSG_STRUCT *, V0_DESC_STRUCT *);
static int construct_granule_query(IMS_MSG_STRUCT *, V0_DESC_STRUCT *);
static int construct_list_result(V0_DESC_STRUCT *);
static int construct_status_result(V0_DESC_STRUCT *);
static int construct_content_result(V0_DESC_STRUCT *);
static int construct_granule_result(V0_DESC_STRUCT *);
static int process_query_results(V0_DESC_STRUCT *);
static int process_granule_query(V0_DESC_STRUCT *);
static int add_to_search_list (V0_VALUE_LIST *, char *);
static int check_queries(IMS_MSG_STRUCT *, V0_DESC_STRUCT *);
static int get_list_result (IMS_QI_DESC_OBJ *, V0_DAR_RESULT_LIST *);
static int get_content_result (IMS_QI_DESC_OBJ *, V0_DAR_RESULT_LIST *);
static int get_granule_result (IMS_QI_DESC_OBJ *, V0_DAR_RESULT_LIST *);
static int send_result (V0_DESC_STRUCT *);
static int create_monitor_version_groups 
	   (AGGREGATE, AGGREGATE *, IMS_MSG_STRUCT *);
static int create_dar_info_groups 
	   (AGGREGATE *, DAR_QUERY_RESULT *, IMS_MSG_STRUCT *);
static int create_dar_content_groups
           (AGGREGATE *, DAR_QUERY_RESULT *, IMS_MSG_STRUCT *);
static int create_dar_granule_groups 
 	   (AGGREGATE *, DAR_QUERY_RESULT *, IMS_MSG_STRUCT *);
static int create_status_code_comment 
	   (AGGREGATE *, V0_ERR_LIST *, IMS_MSG_STRUCT *);
static int  verify_user_key (IMS_MSG_STRUCT *, V0_DESC_STRUCT *);
static int  verify_user_order_access (IMS_MSG_STRUCT *, V0_DESC_STRUCT *);
static int  convert_date_time (char *, char *);
static int  get_client_ACK (V0_DESC_STRUCT *);
static int  getFrameTable (V0_DESC_STRUCT *, char *, char *, char *, char *);
static int  get_dar_frames (V0_DESC_STRUCT *, V0_DAR_RESULT_LIST *, char *);


/*************************************************************************
**
** v0_dar__process_query -
**
** Purpose: handles DARnet queries 
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/

int v0_dar__process_query(IMS_MSG_STRUCT *msgDesc,
                          V0_DESC_STRUCT *v0Desc)
{
	char 	msgBuf[IMS_COL80_LEN+1];
	int	status;

	/* 
	** Parse the client query.  If the parsing routine returns an error
	** status code,  return to the caller who will send a QUIT message 
	** to the client. 
	*/ 
	if ((status = v0_process__parse_RxTree (v0Desc)) < IMS_OK)
	{
		(void)strcpy(v0Desc->odl_status, DAR_ODL_ERROR_CODE);
		return (status);
	}

	/* 
	** Invoke the "check_queries" function to check that all required 
	** fields are provided and to perform any necessary validation 
	** (eg. user authenticator).
	** If the "check_queries" function returns an error status code,
	** return to the caller who will send a QUIT message to the client.
	** The appropriate odl_status code and comment, set by the
	** the "check_queries" function, will be sent to the client
	** in the QUIT message. 
	*/

	if ( (status = check_queries (msgDesc, v0Desc)) < IMS_OK)
	{
		return (status);
	}

	/*
	** Invoke the appropriate routine to construct the SQL query
	** for the message being processed.
	*/
	switch ( v0Desc->RxType)
	{
		case V0_DAR_LIST_QUERY:
			status = construct_list_query(msgDesc, v0Desc);
			break;
		case V0_DAR_STATUS_QUERY:
			status = construct_status_query(msgDesc, v0Desc);
			break;
		case V0_DAR_CONTENT_QUERY:
                        status = construct_content_query(msgDesc, v0Desc);
                        break;
                case V0_DAR_GRANULE_QUERY:
                        status = construct_granule_query(msgDesc, v0Desc);
                        break;
		default:
			(void) ims_msg(msgDesc, IMS_FATAL,
				"Unrecognized message type - "
				"v0Dar__process_query: does not know how to process.");
			status = IMS_FATAL;
			break;
	}
	if (status < IMS_OK)
	{
		(void) v0_util__set_status_code_comment (v0Desc,
			"Internal Server error encountered while "
			"constructing SQL query", DAR_SYSTEM_ERROR_CODE);
		return (status);
	}

#ifdef QDEBUG
	(void)ims_msg(msgDesc, IMS_INFO,"sql ==> %s", v0Desc->query.sql);
#endif

	/*
	** Execute the SQL query statement, store the results in a list,
	** construct the result message and send the result to DARnet. 
 	*/	
	status = process_query_results(v0Desc);

	/*
	** For DAR_GRANULE_QUERY only - if the initial query which determines
	** whether frame info exists for the requested DARs returns
	** data, then we'll proceed further to retrieve the frame info 
	** and return to DARnet. 
	*/
	if ((v0Desc->RxType == V0_DAR_GRANULE_QUERY) &&
	    (status == IMS_OK))
	{
		status = process_granule_query(v0Desc);
	}
	return (status);

}/* end of process_query */

/*************************************************************************
**
** check_queries -
**
** purpose: verifies that all required keywords exist, and validates
**          user identifications. 
**
** return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int check_queries (IMS_MSG_STRUCT *msgDesc, 
               	          V0_DESC_STRUCT *v0Desc )
{
	int    			status;
	V0_REQUEST_STRUCT	*request;
	char   			errbuf[IMS_COL255_LEN+1];	

	request = &v0Desc->request;

	/*
	** FIRST_NAME, LAST_NAME and AUTHENTICATOR are
	** required for all DAR queries 
	*/
	if ( (request->user_info.first_name[0] == '\0') ||
             (request->user_info.last_name[0] == '\0')  ||
	     (request->authenticator[0] == '\0'))
	{
		strcpy(errbuf, "Missing data for USER_ACCT_INFO group.");
		(void) ims_msg(msgDesc, IMS_ERROR, errbuf);
		(void) v0_util__set_status_code_comment (v0Desc, errbuf, DAR_ODL_ERROR_CODE);
		return (IMS_ERROR);
	}

	/*
	** Verify the user authenticator
	*/
	if ((status = verify_user_key (msgDesc, v0Desc)) < IMS_OK)
	{
		return (status);
	}

	/* 
	** DAR_LIST_QUERY validation includes:
	** 1. Check if the required keyword BILLING_ID (account_id) is provided. 
	** 2. Verify that the specified user is authorized to use this
	**    account_id.
	*/ 
	if ( v0Desc->RxType == V0_DAR_LIST_QUERY )
	{
		if (request->account_id[0] == '\0')
		{
			strcpy(errbuf, "Missing BILLING_ID keyword value.");
			(void) ims_msg(msgDesc, IMS_ERROR, errbuf);
			(void) v0_util__set_status_code_comment (v0Desc, errbuf, 
				DAR_ODL_ERROR_CODE);
	                return (IMS_ERROR);
		}
		status = ims_validUser (v0Desc->catReq.qDesc, msgDesc,
				v0Desc->result.user_id, 
				v0Desc->request.authenticator, 1,
				v0Desc->request.account_id);
		if (status == IMS_ERROR)
		{
			(void) v0_util__set_status_code_comment (v0Desc,
			"Invalid BILLING_ID specified for user.",
			DAR_USER_ERROR_CODE);
		}	
		else if (status == IMS_FATAL)
		{
			(void) v0_util__set_status_code_comment (v0Desc,
			"Internal Server error encountered when attempt "
			"to verify user account id.", DAR_SYSTEM_ERROR_CODE);		
		}
		return (status);
	}

	/*
	** Validation for all other DAR queries includes:
	** 1. Check if the required keyword ORDER_ID is provided.
	** 2. Verify that the specified user is authorized to have
	** access to the given order.
	*/
	if (v0Desc->RxType == V0_DAR_STATUS_QUERY ||
	    v0Desc->RxType == V0_DAR_CONTENT_QUERY ||
	    v0Desc->RxType == V0_DAR_GRANULE_QUERY)
	{
		if (request->order_id == 0)
		{
			strcpy(errbuf, "Missing ORDER_ID keyword value.");
	                (void) ims_msg(msgDesc, IMS_ERROR, errbuf);
	                (void) v0_util__set_status_code_comment (v0Desc, errbuf, 
				DAR_ODL_ERROR_CODE);
			return (IMS_ERROR);
		}
		status = verify_user_order_access (msgDesc, v0Desc);
		return (status);
        }

	return (IMS_OK);

}/* end of check_queries */

/*************************************************************************
**
** construct_list_query -
**
** Purpose: Construct the SQL query to retrieve data for the DAR_LIST_QUERY
** 	    message. 
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int
construct_list_query (IMS_MSG_STRUCT *msgDesc,
                      V0_DESC_STRUCT *v0Desc)
{
	char			*select;
	char			*from;
	char			*where;
	V0_REQUEST_STRUCT	request;
	int			status;

	/* 
	** Initialize local variables
	*/
	select = v0Desc->query.select;
	from = v0Desc->query.from;
	where = v0Desc->query.where;
	request = v0Desc->request;

	/*
	** Construct the "select" clause
	*/
	strcpy(select, "d.order_id,d.item_id,d.start_date,d.end_date,"
		       "d.platform,d.sensor,d.mode,d.site_name,d.status,"
		       "o.user_id,o.account_id ");
	/*
	** Construct the "from" clause to retrieve data from "dar" and 
	** "order_queue" tables
	*/
	strcpy(from, "dar d, order_queue o ");
	
	/*
	** Construct the "where" clause
	** account_id is mandatory keyword - 
	** dar and order_queue are joined via order_id
	*/
	sprintf(where, "o.account_id = '%s' and d.order_id = o.order_id ", 
		request.account_id);
	where = where + strlen(where);

	/*
	** Add user_id to search condition if specified
	*/
	if (request.user_id[0] != '\0')
	{
		sprintf(where, "and o.user_id = '%s' ", request.user_id);
		where = where + strlen(where);
	} 
	
	/*
	** Add end_date to search condition if start_time is specified
	*/
	if (request.start_time[0] != '\0')
	{
		sprintf(where, "and d.end_date >= '%s' ",request.start_time);
		where = where + strlen(where);
	}

        /*
        ** Add start_date to search condition if end_time is specified
        */
        if (request.end_time[0] != '\0')
        {
                sprintf(where, "and d.start_date <= '%s' ",request.end_time);
                where = where + strlen(where);
        }

	/*
	** Add platform to search condition if source_name is specified
	*/
	if (request.platform != (V0_VALUE_LIST *)NULL)
	{
		strcpy(where,"and d.platform in (");
		where = where + strlen(where);

		if ( (status = add_to_search_list
				(request.platform, where)) < IMS_OK)
		{
			(void)ims_msg(msgDesc, status,
			"Error encountered while constructing platform list"
			" for DAR_LIST_QUERY 'where' clause");
			return (status);
		}
		where = where + strlen(where);
		strcpy(where, ") ");
		where = where + strlen(where);
	} /* platform */

        /*
        ** Add sensor to search condition if sensor_name is specified
        */
        if (request.sensor != (V0_VALUE_LIST *)NULL)
        {
                strcpy(where,"and d.sensor in (");
                where = where + strlen(where);
 
                if ( (status = add_to_search_list
                                (request.sensor, where)) < IMS_OK)
                {
                        (void)ims_msg(msgDesc, status,
                        	"Error encountered while constructing sensor list"
	                        " for DAR_LIST_QUERY 'where' clause");
                        return (status);
                }
		where = where + strlen(where);
		strcpy(where, ") ");
		where = where + strlen(where);
        } /* sensor */

	/*
	** Add mode to search condition if mode is specified
	*/
        if (request.mode[0] != '\0')
        {
                sprintf(where, "and d.mode = '%s' ",request.mode);
                where = where + strlen(where);
        }

        /*
        ** Add site_name to search condition if site_name is specified
        */
        if (request.site_name[0] != '\0')
        {
                sprintf(where, "and d.site_name = '%s' ",request.site_name);
                where = where + strlen(where);
        }

	/*
	** Add status to search condition if dar_status is specified
	*/
	if (request.dar_status != 0)
	{
		sprintf(where, "and d.status = %d ", request.dar_status);
		where = where + strlen(where);
	}

	(void)sprintf(v0Desc->query.sql,"select %s from %s where %s "
		"order by d.order_id", v0Desc->query.select, 
		v0Desc->query.from, v0Desc->query.where);

	return (IMS_OK);

} /* end of construct_list_query */


/*************************************************************************
**
** construct_status_query -
**
** Purpose: Construct the SQL query to retrieve data for the 
**	    DAR_STATUS_QUERY message.
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int
construct_status_query (IMS_MSG_STRUCT *msgDesc,
                        V0_DESC_STRUCT *v0Desc)
{
	char			*select;
	char			*from;
	char			*where;
	V0_REQUEST_STRUCT	request;

	/* 
	** Initialize local variables
	*/
	select = v0Desc->query.select;
	from = v0Desc->query.from;
	where = v0Desc->query.where;
	request = v0Desc->request;

	/*
	** Construct the "select" clause
	*/
	strcpy(select, "d.order_id,d.item_id,d.start_date,d.end_date,"
		       "d.platform,d.sensor,d.mode,d.site_name,d.status,"
		       "o.user_id,o.account_id ");
	/*
	** Construct the "from" clause to retrieve data from "dar" and 
	** "order_queue" tables
	*/
	strcpy(from, "dar d, order_queue o ");
	
	/*
	** Construct the "where" clause
	** order_id is mandatory keyword - 
	** dar and order_queue are joined via order_id
	*/
	sprintf(where, "o.order_id = %d and d.order_id = o.order_id ", 
		request.order_id);
	where = where + strlen(where);

	/*
	** Add item_id to search condition if specified
	*/
	if (request.item_id > 0)
	{
		sprintf(where, "and d.item_id = %d ",request.item_id);
		where = where + strlen(where);
	} 
	
	(void)sprintf(v0Desc->query.sql,"select %s from %s where %s "
		"order by d.item_id", v0Desc->query.select, 
		v0Desc->query.from, v0Desc->query.where);

        return (IMS_OK);

} /* end of construct_status_query */

/*************************************************************************
**
** construct_content_query -
**
** Purpose: Construct the SQL query to retrieve data for the
**	    DAR_CONTENT_QUERY message.
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int
construct_content_query (IMS_MSG_STRUCT *msgDesc,
                         V0_DESC_STRUCT *v0Desc)
{
	char			*from;
	char			*where;
	V0_REQUEST_STRUCT	request;

	/* 
	** Initialize local variables
	*/
	from = v0Desc->query.from;
	where = v0Desc->query.where;
	request = v0Desc->request;

	/*
	** Construct the "select" clause
	*/
	strcpy(v0Desc->query.sql, "select "
		       "d.order_id,d.item_id,d.pi_name,d.pi_discipline,"
		       "d.platform,d.sensor,d.mode,d.start_date,d.end_date,"
		       "d.observation_num,d.observation_freq,d.asc_desc,"
		       "d.site_name,d.spatial_type,d.radius,d.center_lat,"
		       "d.center_lon,d.north_west_lat,d.north_west_lon,"
		       "d.north_east_lat,d.north_east_lon,d.south_west_lat,"
		       "d.south_west_lon,d.south_east_lat,d.south_east_lon,"
		       "d.active_p,d.activity_start_date,d.activity_end_date,"
		       "d.status,d.user_comment,d.planner_comment,d.op_comment,"
		       "t.description,o.user_id,o.account_id,i.quicklook_p");
	/*
	** Construct the "from" clause to retrieve data from "dar", "order_queue"
	** "order_item" and "items" tables
	*/
	strcpy(from, "dar d, order_queue o, order_item i, items t ");
	
	/*
	** Construct the "where" clause
	** order_id is mandatory keyword - 
	** dar and order_queue are joined via order_id
	** dar and order_item are joined via order_id/item_id
	** description of priority code is retrieved from items table
	*/
	sprintf(where, "o.order_id = %d and d.order_id = o.order_id " 
		       "and d.order_id = i.order_id and d.item_id = i.item_id "
		       "and o.priority = t.instance and t.type = 'priority'",
           		request.order_id);
	where = where + strlen(where);

	/*
	** Add item_id to search condition if specified
	*/
	if (request.item_id > 0)
	{
		sprintf(where, "and d.item_id = %d ",request.item_id);
		where = where + strlen(where);
	} 
	
	(void)sprintf(v0Desc->query.sql,"%s from %s where %s "
		"order by d.item_id", v0Desc->query.sql, 
		v0Desc->query.from, v0Desc->query.where);

        return (IMS_OK);

} /* end of construct_content_query */

/*************************************************************************
**
** construct_granule_query -
**
** Purpose: Construct the SQL query to retrieve data for the
**	    DAR_GRANULE_QUERY message.
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int
construct_granule_query (IMS_MSG_STRUCT *msgDesc,
			 V0_DESC_STRUCT *v0Desc)
{
	char			*where;
	V0_REQUEST_STRUCT	request;

	/*
	** This query joins the dar and dar_frame tables to determine
	** if any dar_frames records exist for the specified order and
	** also to retrieve the platform and mode in order to determine
	** the appropriate granules table.  A good portion of the data 
	** required for granule result message will be retrieved from 
	** the frame granules table
	*/
	where = v0Desc->query.where;
	request = v0Desc->request;

	strcpy(v0Desc->query.sql, 
		"select distinct d.order_id,d.item_id,d.platform,d.mode "
		"from dar d, dar_frame f");

	sprintf(where, "d.order_id = %d and f.order_id = d.order_id " 
		       "and d.item_id = f.item_id ",
           		request.order_id);
	where = where + strlen(where);

	/*
	** Add item_id to search condition if specified
	*/
	if (request.item_id > 0)
	{
		sprintf(where, "and d.item_id = %d ",request.item_id);
		where = where + strlen(where);
	} 
	
	(void)sprintf(v0Desc->query.sql,"%s where %s "
		"order by d.item_id", v0Desc->query.sql, 
		v0Desc->query.where);

        return (IMS_OK);

} /* end of construct_granule_query */

/*************************************************************************
**
** process_query_results -
**
** Purpose: Execute the SQL query which has been constructed and saved in
**   	    v0Desc; store the returned results in the dar_result list;
**          construct the result message and send to DARnet.
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int
process_query_results (V0_DESC_STRUCT *v0Desc)
{
	V0_CAT_STRUCT		*catReq;
	IMS_QI_DESC_OBJ    	*qDesc;
	IMS_MSG_STRUCT		*msgDesc;
	DAR_QUERY_RESULT	*dar_result;
	V0_DAR_RESULT_LIST	*firstPtr, *currPtr, *lastPtr;	
	int			status, rowCount;

        catReq = &v0Desc->catReq;
	msgDesc = catReq->msgDesc;
	qDesc = catReq->qDesc;
        dar_result = &(v0Desc->result.dar_query_result);

        firstPtr = lastPtr = currPtr = (V0_DAR_RESULT_LIST *)NULL;
        rowCount = 0;

	/*
	** Send the query to SQL server and store the results in a list
	*/
        qDesc->cmd = v0Desc->query.sql;
 
        while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
        {
                if (status < IMS_OK)
                {
                        qDesc->cmd[0] = '\0';
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar__process_query_results: failed to get the result row.");
			(void) v0_util__set_status_code_comment (v0Desc,
				"Internal Server error occured while "
				"retrieving data from the catalog.",
				DAR_SYSTEM_ERROR_CODE);
			return (status);
                }
 
                if (status == IMS_ENDOFQUERY) continue;
 
                if ( (currPtr = (V0_DAR_RESULT_LIST *)
                        malloc (sizeof (V0_DAR_RESULT_LIST))) == (V0_DAR_RESULT_LIST *)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar__process_query_results: Memory allocation "
                                "for V0_DAR_RESULT_LIST failed.");
			(void) v0_util__set_status_code_comment (v0Desc,
				"Internal Server error - Memory allocation failed",
				DAR_SYSTEM_ERROR_CODE);
                        return (IMS_FATAL);
                }
 
                currPtr->next_p = (V0_DAR_RESULT_LIST *)NULL;
		currPtr->dar_frames.number_of_frames = 0;
		currPtr->dar_frames.frame_list = (V0_DAR_FRAME_LIST *)NULL;
		currPtr->dar_frames.curr_frame = (V0_DAR_FRAME_LIST *)NULL;
 
                /* a row is returned */
                rowCount ++;
 
                /* copy in  the returned data */
  
		switch (v0Desc->RxType)
		{
	                case V0_DAR_LIST_QUERY:
		 	case V0_DAR_STATUS_QUERY:
	                        (void) get_list_result(qDesc, currPtr);
	                        break;
	                case V0_DAR_CONTENT_QUERY:
	                        (void) get_content_result(qDesc, currPtr);
	                        break;
	                case V0_DAR_GRANULE_QUERY:
	                        (void) get_granule_result(qDesc, currPtr);
	                        break;
	                default:
	                        (void) ims_msg(msgDesc, IMS_FATAL,
	                                "Unrecognized message type - "
	                                "v0Dar__process_query_results: does not know how to"
	                                " process.");
				return (IMS_FATAL);
	                        break;
	        }
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
        qDesc->cmd[0] = '\0';
 
        /*
        ** Re-initialize query descriptor for next command, but do not
        ** cancel previous command
        */
        if (ims_qiResetDesc(qDesc) < IMS_OK)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__process_query_results: "
			"Could not reinitialize query descriptor.");
		(void) v0_util__set_status_code_comment (v0Desc, 
			"Internal Server error - "
			"Could not reinitialize query descriptor.",
			DAR_SYSTEM_ERROR_CODE); 
		/* free the list */
                currPtr = firstPtr;
                while (currPtr != (V0_DAR_RESULT_LIST *)NULL)
                {
                        lastPtr = currPtr->next_p;
                        free (currPtr);
                        currPtr = lastPtr;
                }
                return(IMS_FATAL);
        }
 
	dar_result->dar_result_list = firstPtr; 
	dar_result->number_of_items = rowCount;
	dar_result->curr_dar_item = firstPtr;

	/*
	** If "no match found", then set the odl_status_code accordingly,
	** and return IMS_ERROR to the caller to trigger a QUIT message to
	** be sent to DARnet.  
	*/
	if (rowCount == 0)
	{
		(void) strcpy (v0Desc->odl_status, DAR_NO_MATCH_FOUND);
		return (IMS_ERROR);
	}

	/*
	** Construct the result message and send to DARnet.
	** For DAR_GRANULE_QUERY, we'll need to do some more queries before
	** we can send out the results.
	*/
	if (v0Desc->RxType == V0_DAR_GRANULE_QUERY)
	{
		return (IMS_OK);
	}

	if ((status = send_result(v0Desc)) < IMS_OK)
	{
                (void)ims_msg(msgDesc, status, "Failed to return "
                                "DAR_LIST_RESULT");
		(void)v0_util__set_status_code_comment (v0Desc,
			"Internal Server error encountered while attempting to "
			"return results to DARnet.", DAR_SYSTEM_ERROR_CODE);
		return (status);
        }

	return (IMS_OK);

} /* end of process_query_results */

/*************************************************************************
**
** process_granule_query
**
** Purpose: Execute the SQL query which has been constructed and saved in
**   	    v0Desc; store the returned results in the dar_result list;
**          construct the result message and send to DARnet.
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int
process_granule_query (V0_DESC_STRUCT *v0Desc)
{
	V0_CAT_STRUCT		*catReq;
	IMS_QI_DESC_OBJ    	*qDesc;
	IMS_MSG_STRUCT		*msgDesc;
	DAR_QUERY_RESULT	*dar_result;
	V0_DAR_RESULT_LIST	*darPtr;	
	int			status;

        catReq = &v0Desc->catReq;
	msgDesc = catReq->msgDesc;
	qDesc = catReq->qDesc;
        dar_result = &(v0Desc->result.dar_query_result);
	darPtr = dar_result->dar_result_list;
	
	/*
	** Determine the granule table containing frame info for each
	** of the DARs in the dar_result list.
	** For RADARSAT, if the frame info does not exist in right-looking frames
	** table, we will look for them in the left-looking frames.  Therefore,
	** we have to determine both dataset names now. 
	*/
	while (darPtr != (V0_DAR_RESULT_LIST *) NULL)
	{
		darPtr->rl_frame_table_name[0] = '\0';
		darPtr->ll_frame_table_name[0] = '\0';

		if ((status = getFrameTable (v0Desc, darPtr->platform,
			darPtr->mode, darPtr->rl_frame_table_name, "")) < IMS_OK)
    		{
		        (void) ims_msg (msgDesc, IMS_ERROR,
		            "Could not get right-looking frame granule table name for " 
			    "platform '%s' mode '%s'", darPtr->platform, darPtr->mode);
			(void) v0_util__set_status_code_comment (v0Desc,
			    "Internal Server error - Failed to get "
			    "frame granule table name", DAR_SYSTEM_ERROR_CODE);
        		return (status);
    		}

		if (strcmp(darPtr->platform, "RADARSAT-1") == 0)
		{
			if ((status = getFrameTable (v0Desc, darPtr->platform,
				darPtr->mode, darPtr->ll_frame_table_name, 
				" LEFT LOOKING")) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"Could not get left-looking frame granule table "
					"name for platform '%s' mode '%s'", 
					darPtr->platform, darPtr->mode);
				(void) v0_util__set_status_code_comment (v0Desc,
					"Internal Server error - Failed to get "
					"frame granule table name", DAR_SYSTEM_ERROR_CODE);
				return (status);
			}
		}

		darPtr = darPtr->next_p;
	}
	/*
	** Retrieve the list of frames and frame info for each of
	** the DARs in the dar_result list.
	*/
	dar_result->number_of_items = 0;
	darPtr = dar_result->dar_result_list;
	while (darPtr != (V0_DAR_RESULT_LIST *) NULL)
	{
		if ((status = get_dar_frames(v0Desc, darPtr, 
				darPtr->rl_frame_table_name)) < IMS_OK)
		{
			/*
			** The presence of a left-looking dataset name means
			** that the frames may exist in that dataset.  If no records
			** found in the right-looking dataset, go on and search
			** the left-looking dataset.
			**
			** It is a database inconsistency error if no records found
			** in the right-looking dataset and a left-looking dataset
			** does not exist.
			*/ 
			if (darPtr->ll_frame_table_name[0] == '\0')
			{
				(void) ims_msg (msgDesc,status,
					"Failed to get frame data for order %d "
					"item %d", darPtr->order_id, darPtr->item_id);
				(void) v0_util__set_status_code_comment (v0Desc,
					"Internal Server error - Failed to get "
					"frame data", DAR_SYSTEM_ERROR_CODE);
				return (status);
			}
		}
		/*
		** For RADARSAT-1 only, if no frame records were found in the right-
		** looking dataset, search the left-looking dataset (if such a dataset
		** exists).
		*/
		if ((darPtr->dar_frames.number_of_frames <= 0) &&
		    (strcmp(darPtr->platform, "RADARSAT-1") == 0) &&
		    (darPtr->ll_frame_table_name[0] != '\0'))
		{
			if ((status = get_dar_frames(v0Desc, darPtr,
				darPtr->ll_frame_table_name)) < IMS_OK)
			{
				(void) ims_msg (msgDesc,status,
					"Failed to get frame data for order %d "
					"item %d", darPtr->order_id, darPtr->item_id);
				(void) v0_util__set_status_code_comment (v0Desc,
					"Internal Server error - Failed to get "
					"frame data", DAR_SYSTEM_ERROR_CODE);
				return (status);
			}
		}
		dar_result->number_of_items += darPtr->dar_frames.number_of_frames;
printf("items=%d, frames=%d\n\n\n", dar_result->number_of_items, darPtr->dar_frames.number_of_frames);
		darPtr = darPtr->next_p;
	}  

	/*
	** Construct the result message and send to DARnet.
	*/
	if ((status = send_result(v0Desc)) < IMS_OK)
	{
                (void)ims_msg(msgDesc, status, "Failed to return "
                                "DAR_LIST_RESULT");
		(void)v0_util__set_status_code_comment (v0Desc,
			"Internal Server error encountered while attempting to "
			"return results to DARnet.", DAR_SYSTEM_ERROR_CODE);
		return (status);
        }
	return (IMS_OK);

} /* end of process_granule_query */

/*************************************************************************
**
** get_dar_frames
**
** Purpose: Retrieve the list of frames and frame info for the given DAR
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int
get_dar_frames (V0_DESC_STRUCT *v0Desc, V0_DAR_RESULT_LIST *darPtr,
		char *frame_table_name)
{
        V0_CAT_STRUCT           *catReq;
        IMS_QI_DESC_OBJ         *qDesc;
        IMS_MSG_STRUCT          *msgDesc;
	V0_DAR_FRAME_LIST	*firstPtr, *currPtr, *lastPtr;
	int			status;

	catReq = &v0Desc->catReq;
        qDesc = catReq->qDesc;
	msgDesc = v0Desc->msgDesc;

	darPtr->dar_frames.number_of_frames = 0;

	/*
	** Construct the SQL query to retrieve frame data
	*/
	(void) sprintf (v0Desc->query.sql,
			"select o.account_id, p.platform, "
			"s.sensor, f.REVOLUTION, f.FRAME_ID, g.FRAME_MODE, "
			"g.ASC_DESC, g.name, g.north_lat, g.south_lat, "
			"g.west_lon, g.east_lon, g.pole_included, g.CENTER_TIME, "
			"g.CENTER_LAT, g.CENTER_LON, g.FRAME_STATUS "
			"from order_queue o, dar_frame f, %s g, "
			"platforms p, sensors s "
			"where o.order_id = %d and f.order_id = %d and f.item_id = %d "
			"and p.acronym = f.PLATFORM and s.acronym = g.SENSOR "
			"and g.PLATFORM = f.PLATFORM and g.REVOLUTION = f.REVOLUTION "
			"and g.FRAME_ID = f.FRAME_ID order by g.FRAME_ID",
			frame_table_name, darPtr->order_id, 
			darPtr->order_id, darPtr->item_id);
#ifdef QDEBUG
        (void)ims_msg (msgDesc, IMS_INFO, "sql ==> %s", v0Desc->query.sql);
#endif
	/*
	** Execute the query and store the result in the list
	*/
	IMS_SETCMD (qDesc, v0Desc->query.sql);

        while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
        {
                if (status < IMS_OK)
                {
                        qDesc->cmd[0] = '\0';
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar__get_dar_frames: failed to get the result row.");
			(void) v0_util__set_status_code_comment (v0Desc,
				"Internal Server error - Failed to "
				"retrieve frame info from the catalog.",
				DAR_SYSTEM_ERROR_CODE);
			return (status);
                }
 
                if (status == IMS_ENDOFQUERY) continue;
 
                if ( (currPtr = (V0_DAR_FRAME_LIST *)
                        malloc (sizeof (V0_DAR_FRAME_LIST))) == (V0_DAR_FRAME_LIST *)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar__get_dar_frames: Memory allocation "
                                "for V0_DAR_FRAME_LIST failed.");
			(void) v0_util__set_status_code_comment (v0Desc,
				"Internal Server error - Memory allocation failed",
				DAR_SYSTEM_ERROR_CODE);
                        return (IMS_FATAL);
                }
 
                currPtr->next_p = (V0_DAR_FRAME_LIST *)NULL;
 
                /* a row is returned */
                (darPtr->dar_frames.number_of_frames)++;
 
                /*
		** copy in  the returned data 
		*/
                (void) memcpy ((DBCHAR *)darPtr->account_id,
                        qDesc->valAddr[0], qDesc->valLength[0]);
                darPtr->account_id[qDesc->valLength[0]] = '\0';
                (void) ims_truncStr (darPtr->account_id);

                (void) memcpy ((DBCHAR *)currPtr->platform,
                        qDesc->valAddr[1], qDesc->valLength[1]);
                currPtr->platform[qDesc->valLength[1]] = '\0';
                (void) ims_truncStr (currPtr->platform);

                (void) memcpy ((DBCHAR *)currPtr->sensor,
                        qDesc->valAddr[2], qDesc->valLength[2]);
                currPtr->sensor[qDesc->valLength[2]] = '\0';
                (void) ims_truncStr (currPtr->sensor);

                (void) memcpy ((DBINT *)&(currPtr->revolution),
                        qDesc->valAddr[3], qDesc->valLength[3]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->frame_id),
                        qDesc->valAddr[4], qDesc->valLength[4]);
 
		(void) memcpy ((DBCHAR *)currPtr->frame_mode,
                        qDesc->valAddr[5], qDesc->valLength[5]);
		currPtr->frame_mode[qDesc->valLength[5]] = '\0';
		(void) ims_truncStr (currPtr->frame_mode);

                (void) memcpy ((DBCHAR *)currPtr->asc_desc,
                        qDesc->valAddr[6], qDesc->valLength[6]);
                currPtr->asc_desc[qDesc->valLength[6]] = '\0';
                (void) ims_truncStr (currPtr->asc_desc);

                (void) memcpy ((DBCHAR *)currPtr->granule_id,
                        qDesc->valAddr[7], qDesc->valLength[7]);
                currPtr->granule_id[qDesc->valLength[7]] = '\0';
                (void) ims_truncStr (currPtr->granule_id);

		(void) memcpy ((DBREAL *)&(currPtr->north_lat),
		qDesc->valAddr[8], qDesc->valLength[8]);
	
                (void) memcpy ((DBREAL *)&(currPtr->south_lat),
                qDesc->valAddr[9], qDesc->valLength[9]);

                (void) memcpy ((DBREAL *)&(currPtr->west_lon),
                qDesc->valAddr[10], qDesc->valLength[10]);

                (void) memcpy ((DBREAL *)&(currPtr->east_lon),
                qDesc->valAddr[11], qDesc->valLength[11]);

                (void) memcpy ((DBCHAR *)currPtr->pole_included,
                        qDesc->valAddr[12], qDesc->valLength[12]);
                currPtr->pole_included[qDesc->valLength[12]] = '\0';
                (void) ims_truncStr (currPtr->pole_included);
 
                (void) memcpy ((DBCHAR *)currPtr->center_time,
                        qDesc->valAddr[13], qDesc->valLength[13]);
                currPtr->center_time[qDesc->valLength[13]] = '\0';
                (void) ims_truncStr (currPtr->center_time);

                (void) memcpy ((DBREAL *)&(currPtr->center_lat),
                qDesc->valAddr[14], qDesc->valLength[14]);

                (void) memcpy ((DBREAL *)&(currPtr->center_lon),
                qDesc->valAddr[15], qDesc->valLength[15]);
 
                (void) memcpy ((DBCHAR *)currPtr->frame_status,
                        qDesc->valAddr[16], qDesc->valLength[16]);
                currPtr->frame_status[qDesc->valLength[16]] = '\0';
                (void) ims_truncStr (currPtr->frame_status);
 
                if (darPtr->dar_frames.number_of_frames == 1)
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
        qDesc->cmd[0] = '\0';
 
        /*
        ** Re-initialize query descriptor for next command, but do not
        ** cancel previous command
        */
        if (ims_qiResetDesc(qDesc) < IMS_OK)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__get_dar_frames: "
			"Could not reinitialize query descriptor.");
		(void) v0_util__set_status_code_comment (v0Desc, 
			"Internal Server error - "
			"Could not reinitialize query descriptor.",
			DAR_SYSTEM_ERROR_CODE); 
		/* free the list */
                currPtr = firstPtr;
                while (currPtr != (V0_DAR_FRAME_LIST *)NULL)
                {
                        lastPtr = currPtr->next_p;
                        free (currPtr);
                        currPtr = lastPtr;
                }
                return(IMS_FATAL);
        }

	/*
	** If we did not get any rows returned, it is a serious database
	** inconsistency.  Return error status to the caller.
	*/
	if (darPtr->dar_frames.number_of_frames == 0)
	{
		return (IMS_ERROR);
	}
	darPtr->dar_frames.frame_list = firstPtr; 
	darPtr->dar_frames.curr_frame = firstPtr;
	return (IMS_OK);

} /* end of get_dar_frames */

/*************************************************************************
**
** getFrameTable -
**
** Purpose: Return the frame granule table name for the dataset of the given
**	    platform and mode. 
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
 
static int
getFrameTable (V0_DESC_STRUCT *v0Desc, char *platform, char *mode,
			char *granules_table_name, char *frame_type)
{
	IMS_QI_DESC_OBJ		*qDesc = v0Desc->catReq.qDesc;
	char 			dataset[IMS_COL80_LEN+1];
	int			i, status;

	/*
	** Compose dataset name.
	*/
	dataset[0] = '\0';
	for (i = 0; i < IMS_MAXPLATFORMMODES; i++)
	{
	if (strcmp (mode, IMS_PLATFORM_MODES[i].mode) == 0)
		{
			(void) strcpy (dataset, platform);
			(void) strcat (dataset, IMS_PLATFORM_MODES[i].dataset);
			(void) strcat (dataset, frame_type);
			(void) strcat (dataset, " FRAMES");
			break;
		}
        }
	/*
	** Make sure we got a dataset name
	*/
	if ((int) strlen (dataset) == 0)
	{
                (void) ims_msg (v0Desc->msgDesc, IMS_ERROR,
                        "Could not get frame granule table name for "
			"platform '%s' and mode '%s'.",
                        platform, mode);
                return (IMS_ERROR);
	}
	/*
	** Populate the command buffer with the SQL statement to retrieve
	** the granules table name for this dataset.
	*/
	(void) sprintf (v0Desc->query.sql,"select granules_table "
			"from dataset_policy d, dataset_relation r "
			"where r.dataset = '%s' and r.dataset_idx = d.dataset_idx",
			dataset);
	IMS_SETCMD (qDesc, v0Desc->query.sql);
	/*
	** Execute the query 
	*/
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd[0] = '\0';
			(void) ims_msg (v0Desc->msgDesc, status,
				"v0Dar__getFrameTable: failed to get result row");
			return (status);
		}
	}
	/*
	** If we did not get exactly 1 row returned, something may be wrong.
	** In the case of Radarsat-1, if we're unable to retrieve the table name
	** for left-looking frames, it's very possible that such a dataset does not
	** exist, in which case, IMS_OK is returned to the caller.  The caller will
	** not perform a search on the left-looking dataset.
	*/
	if (IMS_AFFECTED (qDesc) != 1) 
	{
		qDesc->cmd[0] = '\0';
		if ((strcmp(frame_type," LEFT LOOKING") == 0) &&
		    (IMS_AFFECTED (qDesc) == 0))
		{
			(void) ims_msg (v0Desc->msgDesc, IMS_INFO,
				"v0Dar__getFrameTable: No frame table name"
				" returned for dataset '%s'", dataset);
			return (IMS_OK); 
		}
		else
		{
			(void) ims_msg (v0Desc->msgDesc, IMS_ERROR,
				"v0Dar__getFrameTable: None or multiple frame granule "
				"table name returned for dataset '%s'", dataset);
			return (IMS_ERROR);
		}
	}
	/*
	** Get the granules table name
	*/
	(void) memcpy (granules_table_name,
			qDesc->valAddr[0], qDesc->valLength[0]);
	granules_table_name[qDesc->valLength[0]] = '\0';
	(void) ims_truncStr(granules_table_name);

	/*
	** Re-initialize the query descriptor for next command.
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (v0Desc->msgDesc, IMS_FATAL,
			"v0Dar__getFrameTable: Could not reinitialize "
			"query descriptor.");
		return (IMS_FATAL);
	}	
	return (IMS_OK);		
	
} /* end of getFrameTable */


/*************************************************************************
**
** add_to_search_list -
**
** Purpose: Add to the search_clause all values from list.
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/

static int
add_to_search_list (V0_VALUE_LIST *list,
		    char	  *search_clause)
{
	V0_VALUE_LIST	*list_ptr = list;
	int		first_item = 1;

	while (list_ptr != (V0_VALUE_LIST *)NULL)
	{
		if (first_item)
		{
			sprintf(search_clause, "'%s'",list_ptr->char_value1);
			first_item = 0;
		}
		else
		{
			sprintf(search_clause, ",'%s'",list_ptr->char_value1);
		}
		search_clause = search_clause + strlen(search_clause);
		list_ptr = list_ptr->next_p;
	}

	return (IMS_OK);
}

/*************************************************************************
**
** get_list_result -
**
** Purpose: Store the result returned from SQL Server for the
**          DAR_LIST_QUERY or DAR_STATUS_QUERY message 
**	    in the V0_DAR_RESULT_LIST. 
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
 
static int
get_list_result (IMS_QI_DESC_OBJ    *qDesc, 
		 V0_DAR_RESULT_LIST *currPtr)
{

	(void) memcpy ((DBINT *)&(currPtr->order_id),
			qDesc->valAddr[0], qDesc->valLength[0]);
 
	(void) memcpy ((DBSMALLINT *)&(currPtr->item_id),
                        qDesc->valAddr[1], qDesc->valLength[1]);

	(void) memcpy ((DBCHAR *)currPtr->start_date,
                        qDesc->valAddr[2], qDesc->valLength[2]);
	currPtr->start_date[qDesc->valLength[2]] = '\0';
	(void) ims_truncStr (currPtr->start_date);

	(void) memcpy ((DBCHAR *)currPtr->end_date,
			qDesc->valAddr[3], qDesc->valLength[3]);
	currPtr->end_date[qDesc->valLength[3]] = '\0';
	(void) ims_truncStr (currPtr->end_date);
 
	(void) memcpy ((DBCHAR *)currPtr->platform,
                        qDesc->valAddr[4], qDesc->valLength[4]);
	currPtr->platform[qDesc->valLength[4]] = '\0';
	ims_truncStr (currPtr->platform);
 
	(void) memcpy ((DBCHAR *)currPtr->sensor,
                        qDesc->valAddr[5], qDesc->valLength[5]);
	currPtr->sensor[qDesc->valLength[5]] = '\0';
	ims_truncStr (currPtr->sensor);

	(void) memcpy ((DBCHAR *)currPtr->mode,
                        qDesc->valAddr[6], qDesc->valLength[6]);
	currPtr->mode[qDesc->valLength[6]] = '\0';
	ims_truncStr (currPtr->mode);

	(void) memcpy ((DBCHAR *)currPtr->site_name,
                        qDesc->valAddr[7], qDesc->valLength[7]);
	currPtr->site_name[qDesc->valLength[7]] = '\0';
	ims_truncStr (currPtr->site_name);

	(void) memcpy ((DBSMALLINT *)&(currPtr->dar_status),
                        qDesc->valAddr[8], qDesc->valLength[8]);

	(void) memcpy ((DBCHAR *)currPtr->user_id,
                        qDesc->valAddr[9], qDesc->valLength[9]);
	currPtr->user_id[qDesc->valLength[9]] = '\0';
	(void) ims_truncStr (currPtr->user_id);

	(void) memcpy ((DBCHAR *)currPtr->account_id,
                        qDesc->valAddr[10], qDesc->valLength[10]);
	currPtr->account_id[qDesc->valLength[10]] = '\0';
	(void) ims_truncStr (currPtr->account_id);
 
        return (IMS_OK);
 
}/* end of get_list_result */


/*************************************************************************
**
** get_content_result -
**
** Purpose: Store the result returned for DAR_CONTENT_QUERY message 
**          in the V0_DAR_RESULT_LIST 
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
 
static int
get_content_result (IMS_QI_DESC_OBJ    *qDesc,
                    V0_DAR_RESULT_LIST *currPtr)
{
 
	(void) memcpy ((DBINT *)&(currPtr->order_id),
			qDesc->valAddr[0], qDesc->valLength[0]);
 
	(void) memcpy ((DBSMALLINT *)&(currPtr->item_id),
                        qDesc->valAddr[1], qDesc->valLength[1]);

        (void) memcpy ((DBCHAR *)currPtr->pi_name,
                        qDesc->valAddr[2], qDesc->valLength[2]);
        currPtr->pi_name[qDesc->valLength[2]] = '\0';
        (void) ims_truncStr (currPtr->pi_name);

        (void) memcpy ((DBCHAR *)currPtr->pi_discipline,
                        qDesc->valAddr[3], qDesc->valLength[3]);
        currPtr->pi_discipline[qDesc->valLength[3]] = '\0';
        (void) ims_truncStr (currPtr->pi_discipline);

	(void) memcpy ((DBCHAR *)currPtr->platform,
                        qDesc->valAddr[4], qDesc->valLength[4]);
	currPtr->platform[qDesc->valLength[4]] = '\0';
	ims_truncStr (currPtr->platform);
 
	(void) memcpy ((DBCHAR *)currPtr->sensor,
                        qDesc->valAddr[5], qDesc->valLength[5]);
	currPtr->sensor[qDesc->valLength[5]] = '\0';
	ims_truncStr (currPtr->sensor);

	(void) memcpy ((DBCHAR *)currPtr->mode,
                        qDesc->valAddr[6], qDesc->valLength[6]);
	currPtr->mode[qDesc->valLength[6]] = '\0';
	ims_truncStr (currPtr->mode);

	(void) memcpy ((DBCHAR *)currPtr->start_date,
                        qDesc->valAddr[7], qDesc->valLength[7]);
	currPtr->start_date[qDesc->valLength[7]] = '\0';
	(void) ims_truncStr (currPtr->start_date);

	(void) memcpy ((DBCHAR *)currPtr->end_date,
			qDesc->valAddr[8], qDesc->valLength[8]);
	currPtr->end_date[qDesc->valLength[8]] = '\0';
	(void) ims_truncStr (currPtr->end_date);

        (void) memcpy ((DBINT *)&(currPtr->ob_number),
                        qDesc->valAddr[9], qDesc->valLength[9]);
 
        (void) memcpy ((DBCHAR *)currPtr->ob_frequency,
                        qDesc->valAddr[10], qDesc->valLength[10]);
        currPtr->ob_frequency[qDesc->valLength[10]] = '\0';
        (void) ims_truncStr (currPtr->ob_frequency);

        (void) memcpy ((DBCHAR *)currPtr->asc_desc,
                        qDesc->valAddr[11], qDesc->valLength[11]);
        currPtr->asc_desc[qDesc->valLength[11]] = '\0';
        (void) ims_truncStr (currPtr->asc_desc);

        (void) memcpy ((DBCHAR *)currPtr->site_name,
                        qDesc->valAddr[12], qDesc->valLength[12]);
        currPtr->site_name[qDesc->valLength[12]] = '\0';
        (void) ims_truncStr (currPtr->site_name);

        (void) memcpy ((DBSMALLINT *)&(currPtr->spatial_type),
                        qDesc->valAddr[13], qDesc->valLength[13]);

        (void) memcpy ((DBREAL *)&(currPtr->radius),
                        qDesc->valAddr[14], qDesc->valLength[14]);

        (void) memcpy ((DBREAL *)&(currPtr->center_lat),
                        qDesc->valAddr[15], qDesc->valLength[15]);

        (void) memcpy ((DBREAL *)&(currPtr->center_lon),
                        qDesc->valAddr[16], qDesc->valLength[16]);

        (void) memcpy ((DBREAL *)&(currPtr->nw_lat),
                        qDesc->valAddr[17], qDesc->valLength[17]);

        (void) memcpy ((DBREAL *)&(currPtr->nw_lon),
                        qDesc->valAddr[18], qDesc->valLength[18]);

        (void) memcpy ((DBREAL *)&(currPtr->ne_lat),
                        qDesc->valAddr[19], qDesc->valLength[19]);

        (void) memcpy ((DBREAL *)&(currPtr->ne_lon),
                        qDesc->valAddr[20], qDesc->valLength[20]);

        (void) memcpy ((DBREAL *)&(currPtr->sw_lat),
                        qDesc->valAddr[21], qDesc->valLength[21]);

        (void) memcpy ((DBREAL *)&(currPtr->sw_lon),
                        qDesc->valAddr[22], qDesc->valLength[22]);

        (void) memcpy ((DBREAL *)&(currPtr->se_lat),
                        qDesc->valAddr[23], qDesc->valLength[23]);

        (void) memcpy ((DBREAL *)&(currPtr->se_lon),
                        qDesc->valAddr[24], qDesc->valLength[24]);

        (void) memcpy ((DBCHAR *)currPtr->active_p,
                        qDesc->valAddr[25], qDesc->valLength[25]);
        currPtr->active_p[qDesc->valLength[25]] = '\0';
        (void) ims_truncStr (currPtr->active_p);

        (void) memcpy ((DBCHAR *)currPtr->activity_start_date,
                        qDesc->valAddr[26], qDesc->valLength[26]);
        currPtr->activity_start_date[qDesc->valLength[26]] = '\0';
        (void) ims_truncStr (currPtr->activity_start_date);

        (void) memcpy ((DBCHAR *)currPtr->activity_end_date,
                        qDesc->valAddr[27], qDesc->valLength[27]);
        currPtr->activity_end_date[qDesc->valLength[27]] = '\0';
        (void) ims_truncStr (currPtr->activity_end_date);

        (void) memcpy ((DBSMALLINT *)&(currPtr->dar_status),
                        qDesc->valAddr[28], qDesc->valLength[28]);

        (void) memcpy ((DBCHAR *)currPtr->user_comment,
                        qDesc->valAddr[29], qDesc->valLength[29]);
        currPtr->user_comment[qDesc->valLength[29]] = '\0';
        (void) ims_truncStr (currPtr->user_comment);

        (void) memcpy ((DBCHAR *)currPtr->planner_comment,
                        qDesc->valAddr[30], qDesc->valLength[30]);
        currPtr->planner_comment[qDesc->valLength[30]] = '\0';
        (void) ims_truncStr (currPtr->planner_comment);

        (void) memcpy ((DBCHAR *)currPtr->op_comment,
                        qDesc->valAddr[31], qDesc->valLength[31]);
        currPtr->op_comment[qDesc->valLength[31]] = '\0';
        (void) ims_truncStr (currPtr->op_comment);

        (void) memcpy ((DBCHAR *)currPtr->priority,
                        qDesc->valAddr[32], qDesc->valLength[32]);
        currPtr->priority[qDesc->valLength[32]] = '\0';
        (void) ims_truncStr (currPtr->priority);

        (void) memcpy ((DBCHAR *)currPtr->user_id,
                        qDesc->valAddr[33], qDesc->valLength[33]);
        currPtr->user_id[qDesc->valLength[33]] = '\0';
        (void) ims_truncStr (currPtr->user_id);

        (void) memcpy ((DBCHAR *)currPtr->account_id,
                        qDesc->valAddr[34], qDesc->valLength[34]);
        currPtr->account_id[qDesc->valLength[34]] = '\0';
        (void) ims_truncStr (currPtr->account_id);

        (void) memcpy ((DBCHAR *)currPtr->quicklook_p,
                        qDesc->valAddr[35], qDesc->valLength[35]);
        currPtr->quicklook_p[qDesc->valLength[35]] = '\0';
        (void) ims_truncStr (currPtr->quicklook_p);

        return (IMS_OK);
 
}/* end of get_content_result */

/*************************************************************************
**
** get_granule_result -
**
** Purpose: Send the query for DAR_GRANULE_QUERY message 
**          to SQL Server and store the returned data into a list. 
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
 
static int
get_granule_result (IMS_QI_DESC_OBJ    *qDesc,
                    V0_DAR_RESULT_LIST *currPtr)
{
 
	(void) memcpy ((DBINT *)&(currPtr->order_id),
			qDesc->valAddr[0], qDesc->valLength[0]);
 
	(void) memcpy ((DBSMALLINT *)&(currPtr->item_id),
                        qDesc->valAddr[1], qDesc->valLength[1]);

	(void) memcpy ((DBCHAR *)currPtr->platform,
                        qDesc->valAddr[2], qDesc->valLength[2]);
	currPtr->platform[qDesc->valLength[2]] = '\0';
	ims_truncStr (currPtr->platform);
 
	(void) memcpy ((DBCHAR *)currPtr->mode,
                        qDesc->valAddr[3], qDesc->valLength[3]);
	currPtr->mode[qDesc->valLength[3]] = '\0';
	ims_truncStr (currPtr->mode);

        return (IMS_OK);
 
}/* end of get_granule_result */

/*************************************************************************
**
** send_result -
**
** Purpose: Send a RESULT message to DARnet in chunks.
**	    Wait for an ACK from the client after sending a chunk before
**	    sending the next chunk.  Send a QUIT message to the client
**	    when all chunks have been sent. 
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
 
static int
send_result (V0_DESC_STRUCT *v0Desc)
{
	int	status;

	(void) strcpy(v0Desc->odl_status, DAR_OK_STATUS_CODE);
	while (!v0Desc->result.last_msg_flag)
	{
		switch (v0Desc->RxType)
		{
			case V0_DAR_LIST_QUERY:
				status = construct_list_result(v0Desc);
				break;
			case V0_DAR_STATUS_QUERY:
				status = construct_status_result(v0Desc);
				break;
			case V0_DAR_CONTENT_QUERY:
				status = construct_content_result(v0Desc);
				break;
			case V0_DAR_GRANULE_QUERY:
				status = construct_granule_result(v0Desc);
				break;
			default:
				(void) ims_msg(v0Desc->msgDesc, IMS_FATAL,
				 	"Unrecognized result message passed "
					"to v0Dar__send_result");
				return (IMS_FATAL);
		}

		if (status < IMS_OK)
		{
			(void) v0_msgTree__destroy (v0Desc->TxTree);
			v0Desc->TxTree = (AGGREGATE)NULL;
			return (status);
		}
#ifdef MDEBUG
		PrintLabel (v0Desc->TxTree);
#endif
		/* send a chunk */
		status = v0_handler__send
			(v0Desc->msgDesc, v0Desc->TxTree, v0Desc->msgId);
		(void) v0_msgTree__destroy (v0Desc->TxTree);
               	v0Desc->TxTree = (AGGREGATE)NULL;

		if (status < IMS_OK)
		{
			(void) ims_msg (v0Desc->msgDesc, IMS_FATAL,
				 "v0Dar: failed to send a result chunk");
                        return (status);
		}

		/*
		** Wait for an ACK message before constructing
		** and sending the next chunk.
		*/
		if ((status = get_client_ACK (v0Desc)) < IMS_OK)
		{
			(void) ims_msg (v0Desc->msgDesc, status,
				"v0Dar: failed to get ack from client to "
				"send next result chunk.");	
                        return (status);
		}
		/*
		** If the client sent an ABORT or QUIT message,
		** we do not need to send the rest of the result,
		** just return to the caller
		*/
		if (v0Desc->request.user_aborted_p == 1)
		{
			return (IMS_OK);
		}
	}

	/* 
	** All chunks have been sent, send QUIT message.  
	*/
	(void) v0_util__set_status_code_comment(v0Desc,"All results have been sent",
		DAR_OK_STATUS_CODE);
	(void) v0_msgTree__create(v0Desc->msgDesc, v0Desc->RxTree,
		&(v0Desc->TxTree), V0_QUIT, DAR_OK_STATUS_CODE, 
		&(v0Desc->result), 0);
#ifdef MDEBUG
        PrintLabel (v0Desc->TxTree);
#endif
        status = v0_handler__send
                        (v0Desc->msgDesc, v0Desc->TxTree, v0Desc->msgId);
        (void) v0_msgTree__destroy (v0Desc->TxTree);

	return (status);

} /* end of send_result */


/*************************************************************************
**
** create_result_msg_header -
**
** Purpose: Construct a result message header which includes 
**	    MESSAGE_ID, STATUS_CODE, STATUS_CODE_COMMENT, NUMBER_OF_ITEMS. 
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
 
static int
create_result_msg_header (V0_DESC_STRUCT *v0Desc, 	
			  AGGREGATE *rTree, AGGREGATE *dTree,
			  char *query_msg_name, char *result_msg_name)
{
        AGGREGATE       group;    /* ptr to MONITOR node */
        PARAMETER       parameter;/* temp ptr to parameters */
        VALUE_DATA      value;    /* point to values */
        char            buf[IMS_COL80_LEN];     /* temp buffer for strings */
	V0_REQUEST_STRUCT *request = &(v0Desc->request);
	DAR_QUERY_RESULT *dar_result = &(v0Desc->result.dar_query_result);
	AGGREGATE	RxTree = v0Desc->RxTree; 
	IMS_MSG_STRUCT	*msgDesc = v0Desc->msgDesc;
	int		status;

	/* rTree points to the top node of the QUERY message tree */
	/* dTree points to the top node of the RESULT message tree */

        v0Desc->TxTree = (AGGREGATE) NULL;
        if ((v0Desc->TxTree = NewAggregate(NULL, KA_GROUP, "root", NULL)) ==
                        (AGGREGATE)NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__create_result_msg_header: creating root aggregate failed.");
 
                return (IMS_FATAL);
        }

        /*
        ** create the "result_msg_name" node under TxTree
        */
        if ((*dTree = NewAggregate (v0Desc->TxTree, KA_GROUP, result_msg_name , NULL ))
                        == (AGGREGATE)NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                  "v0Dar__create_result_msg_header: failed to create "
                        "%s aggregate.", result_msg_name);
                return (IMS_FATAL);
        }
 
        /*
        ** locate the top level aggregate in RxTree, i.e. "query_msg_name" node
        */
        if ( (*rTree = (AGGREGATE) FindAggregate (RxTree, query_msg_name) ) ==
                (AGGREGATE) NULL )
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__create_result_msg_header: failed to locate top level of RxTree.");
                return (IMS_FATAL);
        }

        /*
        ** get value of msgId  and attach it to the output dTree
        */
        if ((parameter = FindParameter (*rTree, "MESSAGE_ID")) ==
                        (PARAMETER) NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__create_result_msg_header: missing parameter MESSAGE_ID");
                return (IMS_FATAL);
        }
 
        if ((parameter = CopyParameter (parameter)) == (PARAMETER)NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__create_result_msg_header: failed to copy MESSAGE_ID.");
                return (IMS_FATAL);
        }
        if ((parameter = PasteParameter (*dTree, parameter)) ==
                        (PARAMETER)NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__create_result_msg_header: PasteParameter failed.");
                return (IMS_FATAL);
        }
        /*
        ** get the value of odl_status from v0Desc and attach it to dTree as
        ** status code
        */
        parameter = NewParameter (*dTree, KP_ATTRIBUTE, "STATUS_CODE");
        if (parameter == (PARAMETER)NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__create_result_msg_header: failed to create STATUS_CODE.");
                return (IMS_FATAL);
 
        }
        parameter->value_kind = KV_SCALAR;
        value = ODLConvertInteger (v0Desc->odl_status, 
				   strlen(v0Desc->odl_status));
        NewValue (parameter, &value);

	/*
	** status code comments (if any)
	*/
	if ((status = create_status_code_comment 
			(dTree, v0Desc->result.odl_status_code_comment, 
			 msgDesc)) < IMS_OK) 
	{
		(void) ims_msg (msgDesc, status,
			"v0Dar__create_result_msg_header: failed to create "
			"STATUS_CODE_COMMENT.");
		return (status);
	}

        /*
        ** get the number of items and attach it to NUMBER_OF_ITEMS
        */
        parameter = NewParameter (*dTree, KP_ATTRIBUTE, "NUMBER_OF_ITEMS");
        if (parameter == (PARAMETER)NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__create_result_msg_header: failed to create NUMBER_OF_ITEMS.");
                return (IMS_FATAL);
        }
        parameter->value_kind = KV_SCALAR;
        buf[0] = '\0';
        (void) sprintf (buf,"%d", dar_result->number_of_items);
        value = ODLConvertInteger (buf, strlen(buf));
        NewValue (parameter, &value);

	return (IMS_OK);

} /* end of create_result_msg_header */

/*************************************************************************
**
** construct_list_result -
**
** Purpose: Construct the DAR_LIST_RESULT message.
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
 
static int
construct_list_result (V0_DESC_STRUCT *v0Desc)
{
        AGGREGATE       group;    /* ptr to MONITOR node */
        AGGREGATE       rTree;    /* point to the input tree */
        AGGREGATE       dTree;    /* DAR_LIST_RESULT node */
        PARAMETER       parameter;/* temp ptr to parameters */
        VALUE_DATA      value;    /* point to values */
        char            buf[IMS_COL80_LEN];     /* temp buffer for strings */
	DAR_QUERY_RESULT *dar_result = &(v0Desc->result.dar_query_result);
	V0_REQUEST_STRUCT *request = &(v0Desc->request);
	IMS_MSG_STRUCT	*msgDesc = v0Desc->msgDesc;
	int		status;


	if ((status = create_result_msg_header (v0Desc, &rTree, &dTree, 
			"DAR_LIST_QUERY", "DAR_LIST_RESULT")) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0Dar__construct_list_result: Failed to create "
			"message header");
		return (IMS_FATAL);
	}
	
        /* billing_id */
        parameter = NewParameter (dTree, KP_ATTRIBUTE, "BILLING_ID");
        if (parameter == (PARAMETER)NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__construct_list_result: failed to create BILLING_ID.");
                return (IMS_FATAL);
        }
        parameter->value_kind = KV_SCALAR;
        value = ODLConvertString (request->account_id, strlen(request->account_id));
        NewValue (parameter, &value);

	/*
	** Build the DAR_INFO groups
	*/
	if ((status = create_dar_info_groups
			(&dTree, dar_result, msgDesc)) < IMS_OK)
	{
                (void) ims_msg (msgDesc, status,
                        "v0Dar__construct_list_result: Failed to create "
                        "DAR_INFO group.");
                return (status);
        }
	if (dar_result->curr_dar_item == (V0_DAR_RESULT_LIST *) NULL)
	{
		v0Desc->result.last_msg_flag = 1;	
	}

	/*
	** create the MONITOR and VERSION groups 
	*/
	if ((status = create_monitor_version_groups 
		(rTree, &dTree, msgDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"v0Dar__construct_list_result: Failed to create "
			"MONITOR/VERSION group.");
		return (status);
	}

	return (IMS_OK);
 
} /* end of construct_list_result */

/*************************************************************************
**
** construct_status_result -
**
** Purpose: Construct the DAR_STATUS_RESULT message.
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
 
static int
construct_status_result (V0_DESC_STRUCT *v0Desc)
{
        AGGREGATE       group;    /* ptr to MONITOR node */
        AGGREGATE       rTree;    /* point to the input tree */
        AGGREGATE       dTree;    /* DAR_STATUS_RESULT node */
        PARAMETER       parameter;/* temp ptr to parameters */
        VALUE_DATA      value;    /* point to values */
        char            buf[IMS_COL80_LEN];     /* temp buffer for strings */
	DAR_QUERY_RESULT *dar_result = &(v0Desc->result.dar_query_result);
	V0_REQUEST_STRUCT *request = &(v0Desc->request);
	IMS_MSG_STRUCT	*msgDesc = v0Desc->msgDesc;
	int		status;


        if ((status = create_result_msg_header (v0Desc, &rTree, &dTree,
                        "DAR_STATUS_QUERY", "DAR_STATUS_RESULT")) < IMS_OK)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__construct_status_result: Failed to create "
                        "message header");
                return (IMS_FATAL);
        }

        /* billing_id */
        parameter = NewParameter (dTree, KP_ATTRIBUTE, "BILLING_ID");
        if (parameter == (PARAMETER)NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__construct_list_result: failed to create BILLING_ID.");
                return (IMS_FATAL);
        }
        parameter->value_kind = KV_SCALAR;
	if (dar_result->curr_dar_item != (V0_DAR_RESULT_LIST *) NULL)
	{
		strcpy (buf, dar_result->curr_dar_item->account_id);
	}
	else
	{
		buf[0] = '\0';
	}
        value = ODLConvertString (buf, strlen(buf));
        NewValue (parameter, &value);

	/*
	** Build the DAR_INFO groups
	*/
	if ((status = create_dar_info_groups
			(&dTree, dar_result, msgDesc)) < IMS_OK)
	{
                (void) ims_msg (msgDesc, status,
                        "v0Dar__construct_status_result: Failed to create "
                        "DAR_INFO group.");
                return (status);
        }
	if (dar_result->curr_dar_item == (V0_DAR_RESULT_LIST *) NULL)
	{
		v0Desc->result.last_msg_flag = 1;	
	}

	/*
	** create the MONITOR and VERSION groups 
	*/
	if ((status = create_monitor_version_groups 
		(rTree, &dTree, msgDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"v0Dar__construct_list_result: Failed to create "
			"MONITOR/VERSION group.");
		return (status);
	}

	return (IMS_OK);
 
} /* end of construct_status_result */

/*************************************************************************
**
** construct_content_result -
**
** Purpose: Construct the DAR_CONTENT_RESULT message.
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
 
static int
construct_content_result (V0_DESC_STRUCT *v0Desc)
{
        AGGREGATE       group;    /* ptr to MONITOR node */
        AGGREGATE       rTree;    /* point to the input tree */
        AGGREGATE       dTree;    /* DAR_STATUS_RESULT node */
        PARAMETER       parameter;/* temp ptr to parameters */
        VALUE_DATA      value;    /* point to values */
        char            buf[IMS_COL80_LEN];     /* temp buffer for strings */
	DAR_QUERY_RESULT *dar_result = &(v0Desc->result.dar_query_result);
	V0_REQUEST_STRUCT *request = &(v0Desc->request);
	IMS_MSG_STRUCT	*msgDesc = v0Desc->msgDesc;
	int		status;


        if ((status = create_result_msg_header (v0Desc, &rTree, &dTree,
                        "DAR_CONTENT_QUERY", "DAR_CONTENT_RESULT")) < IMS_OK)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__construct_content_result: Failed to create "
                        "message header");
                return (IMS_FATAL);
        }

        /* order_id */
        parameter = NewParameter (dTree, KP_ATTRIBUTE, "ORDER_ID");
        if (parameter == (PARAMETER)NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__construct_status_result: failed to create ORDER_ID.");
                return (IMS_FATAL);
        }
        parameter->value_kind = KV_SCALAR;
        buf[0] = '\0';
        (void) sprintf (buf,"%d", request->order_id);
        value = ODLConvertInteger (buf, strlen(buf));
        NewValue (parameter, &value);

	/*
	** Build the DAR_CONTENT groups
	*/
	if ((status = create_dar_content_groups
			(&dTree, dar_result, msgDesc)) < IMS_OK)
	{
                (void) ims_msg (msgDesc, status,
                        "v0Dar__construct_content_result: Failed to create "
                        "DAR_CONTENT group.");
                return (status);
        }
	if (dar_result->curr_dar_item == (V0_DAR_RESULT_LIST *) NULL)
	{
		v0Desc->result.last_msg_flag = 1;	
	}

        /*
        ** create the MONITOR and VERSION groups
        */
        if ((status = create_monitor_version_groups
                (rTree, &dTree, msgDesc)) < IMS_OK)
        {
                (void) ims_msg (msgDesc, status,
                        "v0Dar__construct_list_result: Failed to create "
                        "MONITOR/VERSION group.");
                return (status);
        }

	return (IMS_OK);
 
} /* end of construct_content_result */

/*************************************************************************
**
** construct_granule_result -
**
** Purpose: Construct the DAR_GRANULE_RESULT message.
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
 
static int
construct_granule_result (V0_DESC_STRUCT *v0Desc)
{
        AGGREGATE       group;    /* ptr to MONITOR node */
        AGGREGATE       rTree;    /* point to the input tree */
        AGGREGATE       dTree;    /* DAR_GRANULE_RESULT node */
        PARAMETER       parameter;/* temp ptr to parameters */
        VALUE_DATA      value;    /* point to values */
        char            buf[IMS_COL80_LEN];     /* temp buffer for strings */
	DAR_QUERY_RESULT *dar_result = &(v0Desc->result.dar_query_result);
	V0_REQUEST_STRUCT *request = &(v0Desc->request);
	IMS_MSG_STRUCT	*msgDesc = v0Desc->msgDesc;
	int		status;


        if ((status = create_result_msg_header (v0Desc, &rTree, &dTree,
                        "DAR_GRANULE_QUERY", "DAR_GRANULE_RESULT")) < IMS_OK)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__construct_granule_result: Failed to create "
                        "message header");
                return (IMS_FATAL);
        }

        /* order_id */
        parameter = NewParameter (dTree, KP_ATTRIBUTE, "ORDER_ID");
        if (parameter == (PARAMETER)NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar__construct_status_result: failed to create ORDER_ID.");
                return (IMS_FATAL);
        }
        parameter->value_kind = KV_SCALAR;
        buf[0] = '\0';
        (void) sprintf (buf,"%d", request->order_id);
        value = ODLConvertInteger (buf, strlen(buf));
        NewValue (parameter, &value);

	/*
	** Build the DAR_GRANULE groups
	*/
	if ((status = create_dar_granule_groups
			(&dTree, dar_result, msgDesc)) < IMS_OK)
	{
                (void) ims_msg (msgDesc, status,
                        "v0Dar__construct_content_result: Failed to create "
                        "DAR_CONTENT group.");
                return (status);
        }
	if (dar_result->curr_dar_item == (V0_DAR_RESULT_LIST *) NULL)
	{
		v0Desc->result.last_msg_flag = 1;	
	}

        /*
        ** create the MONITOR and VERSION groups
        */
        if ((status = create_monitor_version_groups
                (rTree, &dTree, msgDesc)) < IMS_OK)
        {
                (void) ims_msg (msgDesc, status,
                        "v0Dar__construct_granule_result: Failed to create "
                        "MONITOR/VERSION group.");
                return (status);
        }

	return (IMS_OK);
 
} /* end of construct_granule_result */

/*************************************************************************
**
** create_monitor_version_groups -
**
** Purpose: Locate the MONITOR and VERSION groups from rTree and paste
**	    to xTree.
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int
create_monitor_version_groups (AGGREGATE rTree, AGGREGATE *xTree,
			       IMS_MSG_STRUCT *msgDesc)
{
	AGGREGATE	group;

	/* locate and paste MONITOR group */
        if ((group = FindAggregate (rTree, "MONITOR")) ==
                (AGGREGATE)NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: Failed to locate MONITOR.");
                return (IMS_FATAL);
        }
        if ((group = CopyAggregate (group)) == (AGGREGATE)NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: CopyAggregate MONITOR failed.");
                return (IMS_FATAL);
        }
        PasteAggregate (*xTree, group);

        /* locate and paste VERSION group */
        if ((group = FindAggregate (rTree, "VERSION")) ==
                (AGGREGATE)NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: Failed to locate VERSION.");
                return (IMS_FATAL);
        }
        if ((group = CopyAggregate (group)) == (AGGREGATE)NULL)
        {
                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: CopyAggregate VERSION failed.");
                return (IMS_FATAL);
        }
        PasteAggregate (*xTree, group);

	return (IMS_OK);

} /* end of create_monitor_version_groups */


/*************************************************************************
**
** create_dar_info_groups -
**
** Purpose: Loop through the dar_result list and build a DAR_INFO node for 
**	    each item of the list under the xTree. 
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int
create_dar_info_groups (AGGREGATE *xTree, DAR_QUERY_RESULT *dar_result,
			IMS_MSG_STRUCT *msgDesc)

{
	AGGREGATE		new_item;
	V0_DAR_RESULT_LIST 	*dar_item;
        PARAMETER       	parameter;
        VALUE_DATA      	value;    
	char			buf[IMS_COL80_LEN];
	int			count = 1;

	dar_item = dar_result->curr_dar_item;

	/*
	** Loop through the dar_result list and build a node
	** for each item in the list
	*/
	while ((dar_item != (V0_DAR_RESULT_LIST *) NULL) &&
		(count <= MAX_DAR_INFO_ITEMS))
	{
                if ( (new_item = NewAggregate (*xTree, KA_GROUP, "DAR_INFO", NULL) ) ==
                        (AGGREGATE)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "Failed to create new aggregate for DAR_INFO.");
                        return (IMS_FATAL);
 
                }

		/* order_id */
        	parameter = NewParameter (new_item, KP_ATTRIBUTE, "ORDER_ID");
	        if (parameter == (PARAMETER)NULL)
	        {
	                (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create ORDER_ID for DAR_INFO group.");
	                return (IMS_FATAL);
	        }
	        parameter->value_kind = KV_SCALAR;
        	buf[0] = '\0';
	        (void) sprintf (buf,"%d", dar_item->order_id);
	        value = ODLConvertInteger (buf, strlen(buf));
	        NewValue (parameter, &value);

                /* item_id */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "ITEM_ID");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create ITEM_ID for DAR_INFO group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                buf[0] = '\0';
                (void) sprintf (buf,"%d", (int)dar_item->item_id);
                value = ODLConvertInteger (buf, strlen(buf));
                NewValue (parameter, &value);

		/* user_id */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"USER_ID"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create USER_ID for DAR_INFO group.");
                        return (IMS_FATAL);
                }
 
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->user_id,
                                strlen (dar_item->user_id) );
                NewValue (parameter, &value);

		/* start_date */
                /*
                ** use a temp string for conversion so we could keep the original value.
                ** ODL somehow changes the string to another daytime format after the
                ** conversion.
                */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"START_DATE"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create START_DATE for DAR_INFO group.");
                        return (IMS_FATAL);
                }
 
                parameter->value_kind = KV_SCALAR;
                buf[0] ='\0';
		convert_date_time(dar_item->start_date, buf);
                value = ODLConvertDateTime (buf, strlen (buf) );
                NewValue (parameter, &value);

		/* stop_date */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"STOP_DATE"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create STOP_DATE for DAR_INFO group.");
                        return (IMS_FATAL);
                }
 
                parameter->value_kind = KV_SCALAR;
                buf[0] ='\0';
                convert_date_time(dar_item->end_date, buf);
                value = ODLConvertDateTime (buf, strlen (buf) ); 
                NewValue (parameter, &value);

		/* source_name (platform) */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"SOURCE_NAME"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create SOURCE_NAME for DAR_INFO group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->platform,
                                strlen (dar_item->platform) );
                NewValue (parameter, &value);

		/* sensor */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"SENSOR_NAME"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create SENSOR_NAME for DAR_INFO group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->sensor,
                                strlen (dar_item->sensor) );
                NewValue (parameter, &value);
 
		/* mode -- skip it if null */
		if (strlen (dar_item->mode) > 0)
		{
	                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"MODE"))
                                == (PARAMETER)NULL )
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create MODE for DAR_INFO group.");
				return (IMS_FATAL);
			}
			parameter->value_kind = KV_SCALAR;
			value = ODLConvertString (dar_item->mode,
                                strlen (dar_item->mode) );
			NewValue (parameter, &value);
		}

		/* site_name */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"SITE_NAME"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create SITE_NAME for DAR_INFO group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->site_name,
                                strlen (dar_item->site_name) );
                NewValue (parameter, &value);
 
                /* dar_status */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "DAR_STATUS");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create DAR_STATUS for DAR_INFO group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                buf[0] = '\0';
                (void) sprintf (buf,"%d", (int)dar_item->dar_status);
                value = ODLConvertInteger (buf, strlen(buf));
                NewValue (parameter, &value);

		count++;
		dar_item = dar_item->next_p;

	} /* end while */

	dar_result->curr_dar_item = dar_item;
	return (IMS_OK);

} /* end of create_dar_info_groups */


/*************************************************************************
**
** create_dar_content_groups -
**
** Purpose: Loop through the dar_result list and build a DAR_CONTENT
**	    node for each item of the list under the xTree. 
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int
create_dar_content_groups (AGGREGATE *xTree, DAR_QUERY_RESULT *dar_result,
		           IMS_MSG_STRUCT *msgDesc)

{
	AGGREGATE		new_item;
	AGGREGATE		loc_group;
	V0_DAR_RESULT_LIST 	*dar_item;
        PARAMETER       	parameter;
        VALUE_DATA      	value;    
	char			buf[IMS_COL80_LEN];
	int			count = 1;

	dar_item = dar_result->curr_dar_item;

	/*
	** Loop through the dar_result list and build a node
	** for each item in the list
	*/
	while ((dar_item != (V0_DAR_RESULT_LIST *) NULL) &&
		(count <= MAX_DAR_CONTENT_ITEMS))
	{
                if ( (new_item = NewAggregate (*xTree, KA_GROUP, "DAR_CONTENT", NULL) ) ==
                        (AGGREGATE)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "Failed to create new aggregate for DAR_CONTENT.");
                        return (IMS_FATAL);
 
                }

                /* pi_name */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"PI_NAME"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create PI_NAME for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
 
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->pi_name,
                                strlen (dar_item->pi_name) );
                NewValue (parameter, &value);
 
                /* pi_discipline */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"PI_DISCIPLINE"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create PI_DISCIPLINE for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
 
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->pi_discipline,
                                strlen (dar_item->pi_discipline) );
                NewValue (parameter, &value);
 

		/* user_id */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"USER_ID"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create USER_ID for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
 
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->user_id,
                                strlen (dar_item->user_id) );
                NewValue (parameter, &value);

                /* item_id */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "ITEM_ID");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create ITEM_ID for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                buf[0] = '\0';
                (void) sprintf (buf,"%d", (int)dar_item->item_id);
                value = ODLConvertInteger (buf, strlen(buf));
                NewValue (parameter, &value);

		/* billing_id */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"BILLING_ID"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create BILLING_ID for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
 
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->account_id,
                                strlen (dar_item->account_id) );
                NewValue (parameter, &value);

		/* source_name (platform) */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"SOURCE_NAME"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create SOURCE_NAME for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->platform,
                                strlen (dar_item->platform) );
                NewValue (parameter, &value);

		/* sensor */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"SENSOR_NAME"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create SENSOR_NAME for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->sensor,
                                strlen (dar_item->sensor) );
                NewValue (parameter, &value);
 
		/* mode */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"MODE"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create MODE for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->mode,
                                strlen (dar_item->mode) );
                NewValue (parameter, &value);

                /* quick_look */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"QUICK_LOOK"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create QUICK_LOOK for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->quicklook_p,
                                strlen (dar_item->quicklook_p) );
                NewValue (parameter, &value);
 

		/* start_date */
                /*
                ** use a temp string for conversion so we could keep the original value.
                ** ODL somehow changes the string to another daytime format after the
                ** conversion.
                */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"START_DATE"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create START_DATE for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
 
                parameter->value_kind = KV_SCALAR;
                buf[0] ='\0';
		convert_date_time(dar_item->start_date, buf);
                value = ODLConvertDateTime (buf, strlen (buf) );
                NewValue (parameter, &value);

		/* stop_date */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"STOP_DATE"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create STOP_DATE for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
 
                parameter->value_kind = KV_SCALAR;
                buf[0] ='\0';
		convert_date_time(dar_item->end_date, buf);
                value = ODLConvertDateTime (buf, strlen (buf) ); 
                NewValue (parameter, &value);

                /* ob_number */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "OB_NUMBER");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create OB_NUMBER for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                buf[0] = '\0';
                (void) sprintf (buf,"%d", (int)dar_item->ob_number);
                value = ODLConvertInteger (buf, strlen(buf));
                NewValue (parameter, &value);
 
                /* ob_frequency */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"OB_FREQUENCY"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create OB_FREQUENCY for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->ob_frequency,
                                strlen (dar_item->ob_frequency) );
                NewValue (parameter, &value);

                /* direction (asc_desc) */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"DIRECTION"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create DIRECTION for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->asc_desc,
                                strlen (dar_item->asc_desc) );
                NewValue (parameter, &value);

		/* site_name */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"SITE_NAME"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create SITE_NAME for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->site_name,
                                strlen (dar_item->site_name) );
                NewValue (parameter, &value);

		/* site_shape and location */	
		if ((dar_item->spatial_type == 3) ||
		    (dar_item->spatial_type == 4))
		{	
			/*
			** Currently, we only have 3 and 4 for DARs.
			** When a DAR is received, its spatial_type is converted to "4"
			** if range_loc is specified.
			*/

                	if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"SITE_SHAPE"))
                                == (PARAMETER)NULL )
	                {
	                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create SITE_SHAPE for DAR_CONTENT group.");
	                        return (IMS_FATAL);
	                }
	                parameter->value_kind = KV_SCALAR;
			if (dar_item->spatial_type == 3)
			{
	                	value = ODLConvertString ("P",1);
			}
			else if (dar_item->spatial_type == 4)
			{
				value = ODLConvertString ("Q",1);
			}
			NewValue (parameter, &value);

			/* point_radius_loc */
			if (dar_item->spatial_type == 3)
			{
		                if ((loc_group = NewAggregate (new_item, KA_GROUP, 
					"POINT_RADIUS_LOC", NULL) ) == (AGGREGATE)NULL)
		                {
		                        (void) ims_msg (msgDesc, IMS_FATAL,
	                                "v0Dar: Failed to create new aggregate for POINT_RADIUS_LOC.");
		                        return (IMS_FATAL);
                		}
				/* radius */
                        	if ( (parameter = NewParameter (loc_group, KP_ATTRIBUTE,
	                                "RADIUS")) == (PARAMETER)NULL )
	                        {
	                                (void) ims_msg (msgDesc, IMS_FATAL,
                                        "v0Dar: failed to create RADIUS for DAR_CONTENT group.");
                                	return (IMS_FATAL);
				}
                        	parameter->value_kind = KV_SCALAR;
	                        buf[0] = '\0';
	                        (void) sprintf(buf, "%-3.4f", dar_item->radius);
                        	value = ODLConvertReal (buf, strlen(buf) );
	                        NewValue (parameter, &value);

				/* centroid_lat */
                                if ( (parameter = NewParameter (loc_group, KP_ATTRIBUTE,
                                        "CENTROID_LAT")) == (PARAMETER)NULL )
                                {
                                        (void) ims_msg (msgDesc, IMS_FATAL,
                                        "v0Dar: failed to create CENTROID_LAT for DAR_CONTENT group.");
                                        return (IMS_FATAL);
                                }
                                parameter->value_kind = KV_SCALAR;
                                buf[0] = '\0';
                                (void) sprintf(buf, "%-3.4f", dar_item->center_lat);
                                value = ODLConvertReal (buf, strlen(buf) );
                                NewValue (parameter, &value);

				/* centroid_lon */ 
                                if ( (parameter = NewParameter (loc_group, KP_ATTRIBUTE,
                                        "CENTROID_LON")) == (PARAMETER)NULL )
                                {
                                        (void) ims_msg (msgDesc, IMS_FATAL,
                                        "v0Dar: failed to create CENTROID_LON for DAR_CONTENT group.");
                                        return (IMS_FATAL);
                                }
                                parameter->value_kind = KV_SCALAR;
                                buf[0] = '\0';
                                (void) sprintf(buf, "%-3.4f", dar_item->center_lon);
                                value = ODLConvertReal (buf, strlen(buf) );
                                NewValue (parameter, &value);
			}

			/* quadr_loc */	
			else if (dar_item->spatial_type == 4)
			{
                                if ((loc_group = NewAggregate (new_item, KA_GROUP, 
                                        "QUADR_LOC", NULL) ) == (AGGREGATE)NULL)
                                {
                                        (void) ims_msg (msgDesc, IMS_FATAL,
                                        "Failed to create new aggregate for QUADR_LOC.");
                                        return (IMS_FATAL);
                                }

                                /* northwest_lat */
                                if ( (parameter = NewParameter (loc_group, KP_ATTRIBUTE,
                                        "NORTHWEST_LAT")) == (PARAMETER)NULL )
                                {
                                        (void) ims_msg (msgDesc, IMS_FATAL,
                                        "v0Dar: failed to create NORTHWEST_LAT for DAR_CONTENT group.");
                                        return (IMS_FATAL);
                                }
                                parameter->value_kind = KV_SCALAR;
                                buf[0] = '\0';
                                (void) sprintf(buf, "%-3.4f", dar_item->nw_lat);
                                value = ODLConvertReal (buf, strlen(buf) );
                                NewValue (parameter, &value);

                                /* northwest_lon */
                                if ( (parameter = NewParameter (loc_group, KP_ATTRIBUTE,
                                        "NORTHWEST_LON")) == (PARAMETER)NULL )
                                {
                                        (void) ims_msg (msgDesc, IMS_FATAL,
                                        "v0Dar: failed to create NORTHWEST_LON for DAR_CONTENT group.");
                                        return (IMS_FATAL);
                                }
                                parameter->value_kind = KV_SCALAR;
                                buf[0] = '\0';
                                (void) sprintf(buf, "%-3.4f", dar_item->nw_lon);
                                value = ODLConvertReal (buf, strlen(buf) );
                                NewValue (parameter, &value);

                                /* northeast_lat */
                                if ( (parameter = NewParameter (loc_group, KP_ATTRIBUTE,
                                        "NORTHEAST_LAT")) == (PARAMETER)NULL )
                                {
                                        (void) ims_msg (msgDesc, IMS_FATAL,
                                        "v0Dar: failed to create NORTHEAST_LAT for DAR_CONTENT group.");
                                        return (IMS_FATAL);
                                }
                                parameter->value_kind = KV_SCALAR;
                                buf[0] = '\0';
                                (void) sprintf(buf, "%-3.4f", dar_item->ne_lat);
                                value = ODLConvertReal (buf, strlen(buf) );
                                NewValue (parameter, &value);

                                /* northeast_lon */
                                if ( (parameter = NewParameter (loc_group, KP_ATTRIBUTE,
                                        "NORTHEAST_LON")) == (PARAMETER)NULL )
                                {
                                        (void) ims_msg (msgDesc, IMS_FATAL,
                                        "v0Dar: failed to create NORTHEAST_LON for DAR_CONTENT group.");
                                        return (IMS_FATAL);
                                }
                                parameter->value_kind = KV_SCALAR;
                                buf[0] = '\0';
                                (void) sprintf(buf, "%-3.4f", dar_item->ne_lon);
                                value = ODLConvertReal (buf, strlen(buf) );
                                NewValue (parameter, &value);

                                /* southeast_lat */
                                if ( (parameter = NewParameter (loc_group, KP_ATTRIBUTE,
                                        "SOUTHEAST_LAT")) == (PARAMETER)NULL )
                                {
                                        (void) ims_msg (msgDesc, IMS_FATAL,
                                        "v0Dar: failed to create SOUTHEAST_LAT for DAR_CONTENT group.");
                                        return (IMS_FATAL);
                                }
                                parameter->value_kind = KV_SCALAR;
                                buf[0] = '\0';
                                (void) sprintf(buf, "%-3.4f", dar_item->se_lat);
                                value = ODLConvertReal (buf, strlen(buf) );
                                NewValue (parameter, &value);

                                /* southeast_lon */
                                if ( (parameter = NewParameter (loc_group, KP_ATTRIBUTE,
                                        "SOUTHEAST_LON")) == (PARAMETER)NULL )
                                {
                                        (void) ims_msg (msgDesc, IMS_FATAL,
                                        "v0Dar: failed to create SOUTHEAST_LON for DAR_CONTENT group.");
                                        return (IMS_FATAL);
                                }
                                parameter->value_kind = KV_SCALAR;
                                buf[0] = '\0';
                                (void) sprintf(buf, "%-3.4f", dar_item->se_lon);
                                value = ODLConvertReal (buf, strlen(buf) );
                                NewValue (parameter, &value);

                                /* southwest_lat */
                                if ( (parameter = NewParameter (loc_group, KP_ATTRIBUTE,
                                        "SOUTHWEST_LAT")) == (PARAMETER)NULL )
                                {
                                        (void) ims_msg (msgDesc, IMS_FATAL,
                                        "v0Dar: failed to create SOUTHWEST_LAT for DAR_CONTENT group.");
                                        return (IMS_FATAL);
                                }
                                parameter->value_kind = KV_SCALAR;
                                buf[0] = '\0';
                                (void) sprintf(buf, "%-3.4f", dar_item->sw_lat);
                                value = ODLConvertReal (buf, strlen(buf) );
                                NewValue (parameter, &value);

                                /* southwest_lon */
                                if ( (parameter = NewParameter (loc_group, KP_ATTRIBUTE,
                                        "SOUTHWEST_LON")) == (PARAMETER)NULL )
                                {
                                        (void) ims_msg (msgDesc, IMS_FATAL,
                                        "v0Dar: failed to create SOUTHWEST_LON for DAR_CONTENT group.");
                                        return (IMS_FATAL);
                                }
                                parameter->value_kind = KV_SCALAR;
                                buf[0] = '\0';
                                (void) sprintf(buf, "%-3.4f", dar_item->sw_lon);
                                value = ODLConvertReal (buf, strlen(buf) );
                                NewValue (parameter, &value);
			}
		}

                /* priority */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "PRIORITY");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create PRIORITY for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->priority,
                                strlen (dar_item->priority) );
                NewValue (parameter, &value);

                /* activity flag */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "ACTIVITY");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create ACTIVITY for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->active_p,
                                strlen (dar_item->active_p) );
                NewValue (parameter, &value);

		/* start_field, and end_field (if activity = "Y") */
		if (strcmp(dar_item->active_p, "Y") == 0)
		{
	                parameter = NewParameter (new_item, KP_ATTRIBUTE, "START_FIELD");
	                if (parameter == (PARAMETER)NULL)
	                {
	                        (void) ims_msg (msgDesc, IMS_FATAL,
	                        "v0Dar: failed to create START_FIELD for DAR_CONTENT group.");
	                        return (IMS_FATAL);
	                }
	                parameter->value_kind = KV_SCALAR;
	                buf[0] ='\0';
	                convert_date_time(dar_item->activity_start_date, buf);
	                value = ODLConvertDate(buf, strlen (buf) );
	                NewValue (parameter, &value);

			parameter = NewParameter (new_item, KP_ATTRIBUTE, "END_FIELD");
	                if (parameter == (PARAMETER)NULL)
	                {
	                        (void) ims_msg (msgDesc, IMS_FATAL,
	                        "v0Dar: failed to create END_FIELD for DAR_CONTENT group.");
	                        return (IMS_FATAL);
	                }
                        parameter->value_kind = KV_SCALAR;
                        buf[0] ='\0';
                        convert_date_time(dar_item->activity_end_date, buf);
                        value = ODLConvertDate(buf, strlen (buf) );
                        NewValue (parameter, &value);
		}

                /* dar_status */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "DAR_STATUS");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create DAR_STATUS for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                buf[0] = '\0';
                (void) sprintf (buf,"%d", (int)dar_item->dar_status);
                value = ODLConvertInteger (buf, strlen(buf));
                NewValue (parameter, &value);

		/* dar_comment */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "DAR_COMMENT");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create DAR_COMMENT for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->user_comment,
                                strlen (dar_item->user_comment) );
                NewValue (parameter, &value);

                /* planner_comment */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "PLANNER_COMMENT");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create PLANNER_COMMENT for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->planner_comment,
                                strlen (dar_item->planner_comment) );
                NewValue (parameter, &value);

                /* op_comment */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "OP_COMMENT");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create OP_COMMENT for DAR_CONTENT group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->op_comment,
                                strlen (dar_item->op_comment) );
                NewValue (parameter, &value);

		count++;
		dar_item = dar_item->next_p;

	} /* end while */

	dar_result->curr_dar_item = dar_item;
	return (IMS_OK);

} /* end of create_dar_content_groups */

/*************************************************************************
**
** create_dar_granule_groups -
**
** Purpose: Loop through the dar_result list and build a DAR_GRANULE
**	    node for each item of the list under the xTree. 
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int
create_dar_granule_groups (AGGREGATE *xTree, DAR_QUERY_RESULT *dar_result,
		           IMS_MSG_STRUCT *msgDesc)

{
	AGGREGATE		new_item;
	V0_DAR_RESULT_LIST 	*dar_item;
	V0_DAR_FRAME_LIST	*frame_item;
        PARAMETER       	parameter;
        VALUE_DATA      	value;    
	char			buf[IMS_COL80_LEN];
	int			count = 0;

	dar_item = dar_result->curr_dar_item;
	frame_item = dar_item->dar_frames.curr_frame;

	/*
	** Loop through the dar_result list and build a node
	** for each frame_item in the dar_item list.  
	** (A dar_result list contains multiple dar_item.
	**  Each dar_item can contain multiple frame_item).
	*/
	while (1)
	{
                if ( (new_item = NewAggregate (*xTree, KA_GROUP, "DAR_GRANULE", NULL) ) ==
                        (AGGREGATE)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "Failed to create new aggregate for DAR_GRANULE.");
                        return (IMS_FATAL);
 
                }

		/* billing_id */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"BILLING_ID"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create BILLING_ID for DAR_GRANUL group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (dar_item->account_id,
                                strlen (dar_item->account_id) );
                NewValue (parameter, &value);

                /* item_id */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "ITEM_ID");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create ITEM_ID for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                buf[0] = '\0';
                (void) sprintf (buf,"%d", (int)dar_item->item_id);
                value = ODLConvertInteger (buf, strlen(buf));
                NewValue (parameter, &value);
 
		/* source_name (platform) */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"SOURCE_NAME"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create SOURCE_NAME for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (frame_item->platform,
                                strlen (frame_item->platform) );
                NewValue (parameter, &value);

		/* sensor */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"SENSOR_NAME"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create SENSOR_NAME for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (frame_item->sensor,
                                strlen (frame_item->sensor) );
                NewValue (parameter, &value);

                /* revolution */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "REVOLUTION");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create REVOLUTION for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                buf[0] = '\0';
                (void) sprintf (buf,"%d", (int)frame_item->revolution);
                value = ODLConvertInteger (buf, strlen(buf));
                NewValue (parameter, &value);

                /* frame_id */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "FRAME_ID");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create FRAME_ID for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                buf[0] = '\0';
                (void) sprintf (buf,"%d", (int)frame_item->frame_id);
                value = ODLConvertInteger (buf, strlen(buf));
                NewValue (parameter, &value);

                /* frame_mode */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"FRAME_MODE"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create FRAME_MODE for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (frame_item->frame_mode,
                                strlen (frame_item->frame_mode) );
                NewValue (parameter, &value);

                /* direction (asc_desc) */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"DIRECTION"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create DIRECTION for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (frame_item->asc_desc,
                                strlen (frame_item->asc_desc) );
                NewValue (parameter, &value);

                /* granule_id */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"GRANULE_ID"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create GRANULE_ID for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (frame_item->granule_id,
                                strlen (frame_item->granule_id) );
                NewValue (parameter, &value);

                /* north_latitude */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "NORTH_LATITUDE");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create NORTH_LATITUDE for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                buf[0] = '\0';
		(void) sprintf(buf, "%-3.4f", frame_item->north_lat);
		value = ODLConvertReal (buf, strlen(buf) );
		NewValue (parameter, &value);

                /* south_latitude */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "SOUTH_LATITUDE");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create SOUTH_LATITUDE for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                buf[0] = '\0';
                (void) sprintf(buf, "%-3.4f", frame_item->south_lat);
                value = ODLConvertReal (buf, strlen(buf) );
                NewValue (parameter, &value);

                /* west_longitude */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "WEST_LONGITUDE");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create WEST_LONGITUDE for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                buf[0] = '\0';
                (void) sprintf(buf, "%-3.4f", frame_item->west_lon);
                value = ODLConvertReal (buf, strlen(buf) );
                NewValue (parameter, &value);

		/* east_longitude */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "EAST_LONGITUDE");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create EAST_LONGITUDE for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                buf[0] = '\0';
                (void) sprintf(buf, "%-3.4f", frame_item->east_lon);
                value = ODLConvertReal (buf, strlen(buf) );
                NewValue (parameter, &value);

		/* pole_included -- skip it if there is no value */ 
		if (strlen(frame_item->pole_included) > 0)
		{ 
                	if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,
				"POLE_INCLUDED")) == (PARAMETER)NULL )
			{
                        	(void) ims_msg (msgDesc, IMS_FATAL,
				  "v0Dar: failed to create POLE_INCLUDED for "
				  "DAR_GRANULE group.");
	                        return (IMS_FATAL);
			}
	                parameter->value_kind = KV_SCALAR;
			value = ODLConvertString (frame_item->pole_included,
                                strlen (frame_item->pole_included) );
	                NewValue (parameter, &value);
		}

		/* center_time */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"CENTER_TIME"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                                "v0Dar: failed to create CENTER_TIME for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
 
                parameter->value_kind = KV_SCALAR;
                buf[0] ='\0';
		convert_date_time(frame_item->center_time, buf);
                value = ODLConvertDateTime (buf, strlen (buf) );
                NewValue (parameter, &value);

                /* center_lat */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "CENTER_LAT");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create CENTER_LAT for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                buf[0] = '\0';
                (void) sprintf(buf, "%-3.4f", frame_item->center_lat);
                value = ODLConvertReal (buf, strlen(buf) );
                NewValue (parameter, &value);

                /* center_lon */
                parameter = NewParameter (new_item, KP_ATTRIBUTE, "CENTER_LON");
                if (parameter == (PARAMETER)NULL)
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
                        "v0Dar: failed to create CENTER_LON for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                buf[0] = '\0';
                (void) sprintf(buf, "%-3.4f", frame_item->center_lon);
                value = ODLConvertReal (buf, strlen(buf) );
                NewValue (parameter, &value);

                /* frame_status */
                if ( (parameter = NewParameter (new_item, KP_ATTRIBUTE,"FRAME_STATUS"))
                                == (PARAMETER)NULL )
                {
                        (void) ims_msg (msgDesc, IMS_FATAL,
			  "v0Dar: failed to create FRAME_STATUS for DAR_GRANULE group.");
                        return (IMS_FATAL);
                }
                parameter->value_kind = KV_SCALAR;
                value = ODLConvertString (frame_item->frame_status,
                                strlen (frame_item->frame_status) );
                NewValue (parameter, &value);

		count++;

		frame_item = frame_item->next_p;
		/*
		** If we've gone through all frame_item for this dar_item,
		** go to the next dar_item.
		*/
		if (frame_item == (V0_DAR_FRAME_LIST *) NULL)
		{
			dar_item = dar_item->next_p;
			if (dar_item == (V0_DAR_RESULT_LIST *) NULL)
			{
				/* all done */
				dar_result->curr_dar_item = dar_item;
				return (IMS_OK);			
			}
			frame_item = dar_item->dar_frames.frame_list;
		}
		/*
		** If we have enough for this chunk, break out of the loop
		** to return to the caller.
		*/
		if (count == MAX_DAR_GRANULE_ITEMS)
		{
			break;
		}	
	} /* end while */

	/*
	** Remember where we left off - this is where we will start 
	** for the next chunk
	*/
	dar_item->dar_frames.curr_frame = frame_item;
	dar_result->curr_dar_item = dar_item;
	return (IMS_OK);

} /* end of create_dar_granule_groups */

/*************************************************************************
**
** create_status_code_comment
**
** Purpose: Loop through the comment list and attach each item of the list 
**	    to the xTree. 
**
** Return : IMS_OK     if successful
**          IMS_ERROR  if error occuur
**          IMS_FATAL  if system error occur
**************************************************************************/
static int
create_status_code_comment (AGGREGATE *xTree, V0_ERR_LIST *comment_list,
			    IMS_MSG_STRUCT *msgDesc)
{
	PARAMETER 	parameter;
	VALUE_DATA	value;
	V0_ERR_LIST	*he_ptr;

	if (comment_list == (V0_ERR_LIST *)NULL)
		return (IMS_OK);

	if ((parameter=NewParameter(*xTree, KP_ATTRIBUTE, "STATUS_CODE_COMMENT"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Failed to create parameter for STATUS_CODE_COMMENT");
		return (IMS_FATAL);
	}
 
	parameter->value_kind = KV_SEQUENCE;
	he_ptr = comment_list;
 
	while (he_ptr != (V0_ERR_LIST *)NULL)
	{
		value =ODLConvertString(he_ptr->err_buf, strlen(he_ptr->err_buf));
		if (NewSqValue(parameter, &value)==NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Failed to create sequence value for "
				"STATUS_CODE_COMMENT");
			return (IMS_FATAL);
		}
 
		he_ptr = he_ptr->next_p;
	}
	return (IMS_OK);

} /* end of create_status_code_comment */


/*************************************************************************
**
** get_client_ACK -
**
** Purpose: Wait to receive an ACK/QUIT/ABORT message from the client. 
**	    Set the user_aborted_p flag appropriately to tell the caller
**	    the type of message received from client.
**
** Return : IMS_OK     if an ACK/QUIT/ABORT message received from client 
**          IMS_FATAL  if error ocurrs.
** 
**************************************************************************/
static int
get_client_ACK (V0_DESC_STRUCT *v0Desc)
{
	AGGREGATE 	clientMsg;
	V0_MSG_TYPE 	msgType;
	int		status;

	if ((status = v0_handler__get (v0Desc->msgDesc, &clientMsg,
			v0Desc->msgId)) < IMS_OK)
	{
		strcpy (v0Desc->odl_status, DAR_SYSTEM_ERROR_CODE);
		(void) ims_msg (v0Desc->msgDesc, status,
			"Failed to receive acknowlegement from client.");
		(void) v0_msgTree__destroy (clientMsg);
		return (IMS_FATAL);
	} /* error(s) occurred while waiting for an ACK msg */
 
	/* a message was received. Identify the message type */
	msgType = v0_msgTree__identify(v0Desc->msgDesc, clientMsg);
 
	/* discard the message, because all we are interested is
	** to find out the msg type */
	(void) v0_msgTree__destroy (clientMsg);

	switch (msgType)
	{
		case V0_ACKNOWLEDGE:
#ifdef MDEBUG
			(void) ims_msg (v0Desc->msgDesc, IMS_INFO,
				"An ACK was received from client.");
#endif
			v0Desc->request.user_aborted_p = 0;
			break;
		case V0_ABORT:
		case V0_QUIT:
#ifdef MDEBUG
                        (void) ims_msg (v0Desc->msgDesc, IMS_INFO,
                                "An ABORT/QUIT was received from client.");
#endif
			v0Desc->request.user_aborted_p = 1;
			break;
		default:
                        (void) ims_msg (v0Desc->msgDesc, IMS_FATAL,
				"Unknown message type received -" 
				" ACK/ABORT/QUIT expected.");
			strcpy (v0Desc->odl_status, DAR_SYSTEM_ERROR_CODE);	
			return (IMS_FATAL); 
			break;

	}
	return (IMS_OK);

} /* end of get_client_ACK */

/*************************************************************************
**
** verify_user_key -
**
** Purpose:  Verify the information provided in USER_ACCT_INFO group.
**	    
**
** Return : IMS_OK     if user info is correct. 
**          IMS_ERROR  if user info is not correct. 
**          IMS_FATAL  if internal server error ocurrs. 
**
**************************************************************************/
static int
verify_user_key (IMS_MSG_STRUCT *msgDesc, V0_DESC_STRUCT *v0Desc)
{
	V0_CAT_STRUCT	*catReq = &v0Desc->catReq;
	int		rowCount = 0;
	int		status;
	V0_VALUE_LIST	*hv, *tv;

	/*
	** Execute the v0_verify_user_key stored procedure
	** which retrieves, from the user_profile table,
	** the user_id with the matching first_name, last_name, authenticator. 
	*/ 
	(void)sprintf (v0Desc->query.sql, 
		"exec v0_verify_user_key '%s', '%s', '%s'", 
		v0Desc->request.user_info.first_name,
		v0Desc->request.user_info.last_name,
		v0Desc->request.authenticator);

	catReq->item[0] = (void *)v0Desc->query.sql;
	catReq->item[1] = (int *)&rowCount;
			
#ifdef QDEBUG
	(void)ims_msg (msgDesc, IMS_INFO, "sql ==> %s", v0Desc->query.sql);
#endif

	if ( (status = v0_cat (catReq, V0_VERIFYUSER) ) < IMS_OK)
	{
		(void) v0_util__set_status_code_comment (v0Desc,
                        "Internal Server error encountered while attempting to "
			"verify user authenticator. ", DAR_SYSTEM_ERROR_CODE);
		return (IMS_FATAL);
	}

	hv = (V0_VALUE_LIST *)catReq->item[2];
	while (hv != (V0_VALUE_LIST *)NULL)
	{
		strcpy(v0Desc->result.user_id,hv->char_value1);
		tv = hv->next_p;
		free (hv);
		hv = tv;
	}

	/*
	** If we did not find 1 exact match, set user error status code
	** and comment to return to the client
	*/
	if (rowCount != 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"v0Dar__verify_user_key: Failed to retrieve "
			"one exact match for user %s %s.",
			v0Desc->request.user_info.first_name,
			v0Desc->request.user_info.last_name);

		(void) v0_util__set_status_code_comment (v0Desc, 
			"User authenticator validation failed. ",
			DAR_USER_ERROR_CODE); 
		return (IMS_ERROR);	
	}
	return (IMS_OK);

} /* end of verify_user_key */

/*************************************************************************
**
** verify_user_order_access -
**
** Purpose:  Verify that the specified user is authorized to have access
**           to the given order. 
**	    
** Return : IMS_OK     if user is authorized. 
**          IMS_ERROR  if user is not authorized. 
**          IMS_FATAL  if internal server error ocurrs. 
**
**************************************************************************/
static int
verify_user_order_access (IMS_MSG_STRUCT *msgDesc, V0_DESC_STRUCT *v0Desc)
{
	V0_CAT_STRUCT	*catReq = &v0Desc->catReq;
	int		rowCount = 0;
	int		status;

	(void)sprintf (v0Desc->query.sql, 
		"select o.account_id from account_user a,"
		"order_queue o where a.user_id = '%s' and "
		"o.account_id = a.account_id and o.order_id = %d",
		v0Desc->result.user_id,
		v0Desc->request.order_id);

	catReq->item[0] = (void *)v0Desc->query.sql;
	catReq->item[1] = (int *)&rowCount;
			
#ifdef QDEBUG
	(void)ims_msg (msgDesc, IMS_INFO, "sql ==> %s", v0Desc->query.sql);
#endif

	if ( (status = v0_cat (catReq, V0_VERIFYENTRYCHAR) ) < IMS_OK)
	{
		(void) v0_util__set_status_code_comment (v0Desc,
                        "Internal Server error encountered while attempting to "
			"verify user order access. ", DAR_SYSTEM_ERROR_CODE);
		return (IMS_FATAL);
	}

	/*
	** If we did not find 1 exact match, set user error status code
	** and comment to return to the client
	*/
	if (rowCount != 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"v0Dar__verify_user_order_access: Failed to retrieve "
			"one exact match for user_id %s order_id %d.",
			v0Desc->result.user_id,
			v0Desc->request.order_id);

		(void) v0_util__set_status_code_comment (v0Desc, 
			"The specified user does not have access to the "
			"given order.", DAR_USER_ERROR_CODE); 
		return (IMS_ERROR);	
	}
	return (IMS_OK);

} /* end of verify_user_key */

/*************************************************************************
**
** convert_date_time -
**
** Purpose:  Convert datetime string in "yyyy-doyThh:mm:ss" format
**	     to "yyyy-mm-ddThh:mm:ss"
**	    
**************************************************************************/
static int
convert_date_time (char *input_time, char *output_time)
{
	int    day;              /* Day of month */
	int    doy;              /* Day of year */
	int    month;            /* Month number */
	int    year;             /* Year number */
	char   md[10];
	char   temp[10];
	char   time_str[15];

	(void)sscanf (input_time, "%d-%dT%s", &year, &doy, time_str);
	(void) v0_time__DoyToDate(doy, year, md, temp);
	(void)sprintf(output_time,"%d-%sT%s", year, md, time_str);	
	return (IMS_OK);
}
