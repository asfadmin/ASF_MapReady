static char *sccs = "@(#)ims_v0MsgTree.c	5.10  09/05/97";
/************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** ims_v0MsgTree.c
**
** Purpose   :   Message tree manipulation.
**
**	Creator   :   Hoshyar Sayah, Julie Wang
**
**	Date      :   April 28, 1994
**
** Modifications:
**
**   07/24/96    jwang   Process_level datatype change.
**  
**   07/24/96    jwang   Order error message change.
**   
**   06/06/96    jwang   Reworded the DAAC_ORDER_ID content
**
**   05/24/96    jwang   PR 776, 844, 858 
**
**   04/25/96    jwang   Added handling of dataset comment and restriction. 
**
**   02/20/96    jwang   R1Bprime (preliminary).
**
**   02/12/96    jwang   Added DAAC_ORDER_ID as part of PRODUCT RESULT.
**
**   01/22/96    jwang   Changes added to handle DARs
**
**   10/31/95    jwang   Added billing enhancement feature
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
#include <ims_util.h>
#include <ims_hash.h>
#include <ims_v0.h>


/* external variables */
extern IMS_HASH_STRUCT *v0_package_HashPtr;

static int v0_msgTree__create_abort (IMS_MSG_STRUCT *, AGGREGATE);
static int v0_msgTree__create_quit (IMS_MSG_STRUCT *, AGGREGATE, 
  AGGREGATE, char *, V0_RESULT_STRUCT *);
static int v0_msgTree__create_dirResult (IMS_MSG_STRUCT *, AGGREGATE, 
	AGGREGATE, char *, V0_RESULT_STRUCT *);
static int v0_msgTree__create_invResult (IMS_MSG_STRUCT *, AGGREGATE, 
  AGGREGATE, char *, V0_RESULT_STRUCT *, int);
static int v0_msgTree__create_prodResult (IMS_MSG_STRUCT *, AGGREGATE, 
  AGGREGATE, char *, V0_RESULT_STRUCT *);
static int v0_msgTree__create_darAcctResult (IMS_MSG_STRUCT *, AGGREGATE, 
  AGGREGATE , char *, V0_RESULT_STRUCT *);
static int v0_msgTree__create_darReqResult (IMS_MSG_STRUCT *, AGGREGATE, 
  AGGREGATE , char *, V0_RESULT_STRUCT *);
static int tokenize_dataset_info (IMS_MSG_STRUCT *, V0_RESULT_STRUCT *, 
	IMS_DS_INFO_TYPE);
static int format_dataset_info (IMS_MSG_STRUCT *, V0_RESULT_STRUCT *);
static int create_empty_package (IMS_MSG_STRUCT *, AGGREGATE *, char *);


VALUE NewSqValue ( PARAMETER, VALUE_DATA *);

/**************************************************************************
**
** v0_msgTree__identify - identify the type of msg. 
**                        return value is enumerated V0_MSG_TYPE.
**
*************************************************************************/
V0_MSG_TYPE v0_msgTree__identify (IMS_MSG_STRUCT *msgDesc, 
	AGGREGATE msgTree)
{
	AGGREGATE msg_node; /* holds the message after extraction from odl tree */
	V0_MSG_TYPE msg_type;
	char msg_name[IMS_COL255_LEN+1];

	msg_node = (AGGREGATE) NULL;
	msg_type = V0_UNIDENTIFIED;

	if ((msg_node = NextAggregate(msgTree)) != NULL)
	{
		strcpy(msg_name, msg_node->name);
		(void) ims_toUpper(msg_name);

		if (strcmp(msg_name, "INVENTORY_SEARCH") == 0)
		{
			msg_type = V0_INVENTORY_SEARCH;
		}

		else if (strcmp(msg_name, "DIRECTORY_SEARCH") == 0)
		{
			msg_type = V0_DIRECTORY_SEARCH;
		}

		else if (strcmp(msg_name, "PRODUCT_REQUEST") == 0)
		{
			msg_type = V0_PRODUCT_REQUEST;
		}

		else if (strcmp(msg_name, "USER_ACCT_SEARCH") == 0)
		{
			msg_type = V0_DAR_ACCT_SEARCH;
		}

		else if (strcmp(msg_name, "DAR_REQUEST") == 0)
		{
			msg_type = V0_DAR_REQUEST;
		}

                else if (strcmp(msg_name, "DAR_LIST_QUERY") == 0)
                {
                        msg_type = V0_DAR_LIST_QUERY;
                }

                else if (strcmp(msg_name, "DAR_STATUS_QUERY") == 0)
                {
                        msg_type = V0_DAR_STATUS_QUERY;
                }

                else if (strcmp(msg_name, "DAR_CONTENT_QUERY") == 0)
                {
                        msg_type = V0_DAR_CONTENT_QUERY;
                }

                else if (strcmp(msg_name, "DAR_GRANULE_QUERY") == 0)
                {
                        msg_type = V0_DAR_GRANULE_QUERY;
                }

		else if (strcmp(msg_name, "BROWSE_REQUEST")  == 0)
		{
			msg_type = V0_BROWSE_REQUEST;
		}

		else if (strcmp(msg_name, "ACKNOWLEDGE") == 0)
		{
			msg_type = V0_ACKNOWLEDGE;
		}

		else if (strcmp(msg_name, "ABORT") == 0)
		{
			msg_type = V0_ABORT;
		}

		else if (strcmp(msg_name, "QUIT") == 0)
		{
			msg_type = V0_QUIT;
		}
	}

	return (msg_type);

}/* end of v0_msgTree__identify */

/**************************************************************************
**
** v0_msgTree__get_message_type 
**    return the message type string.
**
*************************************************************************/
void v0_msgTree__get_message_type (IMS_MSG_STRUCT *msgDesc,
        AGGREGATE msgTree, char *message_type)
{
        AGGREGATE msg_node; /* holds the message after extraction from odl tree */
 
        msg_node = (AGGREGATE) NULL;
       	message_type[0] = '\0'; 
 
        if ((msg_node = NextAggregate(msgTree)) != NULL)
        {
                strcpy(message_type, msg_node->name);
                (void) ims_toUpper(message_type);
	}
} /* v0_msgTree__get_message_type */

/**************************************************************************
**
** v0_msgTree__get_message_id 
**    parse the message to get the message_id
**
*************************************************************************/
int v0_msgTree__get_message_id (IMS_MSG_STRUCT *msgDesc,
        AGGREGATE msgTree, char *message_id)
{
	AGGREGATE msg_node;
        ATTRIBUTE attribute;
        VALUE 	  value_ptr;   

        attribute=(ATTRIBUTE)NULL;
        value_ptr=(VALUE)NULL;
	message_id[0] = '\0';

	if ((msg_node = NextAggregate(msgTree)) != NULL)
	{ 
	        if ( ((attribute=FindParameter(msg_node, "MESSAGE_ID"))!=NULL) &&
       		        ( ((value_ptr=FirstValue(attribute))!=NULL) &&
               		(value_ptr->item.length>0) ) )
        	{
 
			if (value_ptr->item.type != TV_STRING)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
                           		"v0_msgTree__get_message_id: "
			   		"Invalid format for MESSAGE_ID.");
	                        return (IMS_ERROR);
			}
 
			if (value_ptr->item.length > MAX_MSG_ID)
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"v0_msgTree__get_message_id: "
					"Invalid length for MESSAGE_ID.");
				return (IMS_ERROR);
			}
			strcpy (message_id, value_ptr->item.value.string);
			/* (void)remove_newlines (message_id); */
			ims_truncStr (message_id);
			return (IMS_OK);
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
			"v0_msgTree__get_message_id: "
			"failed to extract MESSAGE_ID");
			return (IMS_ERROR);
		}
	}
	else
	{
                        (void) ims_msg (msgDesc, IMS_ERROR,
                        "v0_msgTree__get_message_id: "
                        "failed to extract MESSAGE_ID");
                        return (IMS_ERROR);
	}

} /* v0_msgTree__get_message_id */

/***********************************************************************
**
** v0_msgTree__destroy - destroy the msg structure and all
**                       allocated spaces.
**
***********************************************************************/
void v0_msgTree__destroy (AGGREGATE msgTree)
{
	/*
	if (FindAggregate (msgTree, "root") != NULL)
	{
	*/
		(void) RemoveAggregate(msgTree);
	/*
	}
	*/
	msgTree = (AGGREGATE) NULL;
	return;
} /* end of v0_msgTree__destroy */

/***********************************************************************
**
** v0_msgTree__create - create the msg structure as specified by
**                      msg_type.
**
***********************************************************************/
int v0_msgTree__create (IMS_MSG_STRUCT *msgDesc,
	AGGREGATE RxTree, AGGREGATE *TxTree, V0_MSG_TYPE msgType, 
	char *statusCode, V0_RESULT_STRUCT *result, int granule_limit)
{
	/*
	** initialize root structures for transmit msg.
	*/

	*TxTree = (AGGREGATE) NULL;

	if ((*TxTree = NewAggregate(NULL, KA_GROUP, "root", NULL)) == 
			(AGGREGATE)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create: creating root aggregate failed.");

		return (IMS_FATAL);
	}

	switch (msgType)
	{
	case V0_ABORT:
		if (v0_msgTree__create_abort (msgDesc, *TxTree) < IMS_OK)
		{
			return (IMS_FATAL);
		}
		break;

	case V0_QUIT:
		if (v0_msgTree__create_quit 
			(msgDesc, RxTree, *TxTree, statusCode, result) < IMS_OK)
		{
			return (IMS_FATAL);
		}
		break;

	case V0_DIRECTORY_RESULT:
		if (v0_msgTree__create_dirResult 
			(msgDesc, RxTree, *TxTree, statusCode, 
			(V0_RESULT_STRUCT *)result) < IMS_OK)
		{
			return (IMS_FATAL);
		}
		break;

	case V0_INVENTORY_RESULT:
		if (v0_msgTree__create_invResult 
			(msgDesc, RxTree, *TxTree, statusCode, 
			(V0_RESULT_STRUCT *)result, granule_limit) < IMS_OK)
		{
			return (IMS_FATAL);
		}
		break;

	case V0_PRODUCT_RESULT:
		if (v0_msgTree__create_prodResult 
			(msgDesc, RxTree, *TxTree, statusCode, 
			(V0_RESULT_STRUCT *)result) < IMS_OK)
		{
			return (IMS_FATAL);
		}
		break;

	case V0_DAR_ACCT_RESULT:
		if (v0_msgTree__create_darAcctResult 
			(msgDesc, RxTree, *TxTree, statusCode, 
			(V0_RESULT_STRUCT *)result) < IMS_OK)
		{
			return (IMS_FATAL);
		}
		break;
	
	case V0_DAR_RESULT:
		if (v0_msgTree__create_darReqResult 
			(msgDesc, RxTree, *TxTree, statusCode, 
			(V0_RESULT_STRUCT *)result) < IMS_OK)
		{
			return (IMS_FATAL);
		}
		break;
	
	default:
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create: creating msg type '%d' not supported.",
			msgType);
		return (IMS_FATAL);
	}

	return (IMS_OK);
} /* end of v0_msgTree__create */

/*************************************************************************
**
** v0_msgTree__create_abort - 
**
*************************************************************************/
static int v0_msgTree__create_abort (IMS_MSG_STRUCT *msgDesc,
	AGGREGATE TxTree)
{
	if (NewAggregate (TxTree, KA_GROUP, "ABORT", NULL) 
		== (AGGREGATE)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_abort: failed to create ABORT aggregate.");
		return (IMS_FATAL);
	}
	return (IMS_OK);
}/* end of v0_msgTree_create_abort */

/*************************************************************************
**
** v0_msgTree__create_quit - 
**
*************************************************************************/
static int v0_msgTree__create_quit (IMS_MSG_STRUCT *msgDesc,
	AGGREGATE RxTree, AGGREGATE TxTree, char *statusCode, 
	V0_RESULT_STRUCT *result) 
{
	PARAMETER parameter;
	VALUE_DATA value;
	AGGREGATE group;
	AGGREGATE qTree;
	AGGREGATE tTree;
	V0_ERR_LIST *he_ptr;

	/* Initialize */
	parameter = (PARAMETER) NULL;
	group = (AGGREGATE) NULL;

	if ((qTree = (AGGREGATE) NewAggregate (TxTree, KA_GROUP, "QUIT", NULL)) 
		== (AGGREGATE)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_quit: failed to create QUIT aggregate.");
		return (IMS_FATAL);
	}

	/* point to the top level aggregate */
	if ((tTree = (AGGREGATE) NextAggregate (RxTree)) == (AGGREGATE) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_quit: failed to locate top level aggregate.");
		return (IMS_FATAL);
	}

	/* locate and paste MESSAGE_ID */
	if ((parameter = FindParameter (tTree, "MESSAGE_ID")) ==
			(PARAMETER) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_quit: missing parameter MESSAGE_ID");
		return (IMS_FATAL);
	}
	if ((parameter = CopyParameter (parameter)) == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_quit: CopyParameter failed.");
		return (IMS_FATAL);
	}
	if ((parameter = PasteParameter (qTree, parameter)) ==
			(PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_quit: PasteParameter failed.");
		return (IMS_FATAL);
	}

	/* create new parameter DATA_CENTER_ID */
	parameter = NewParameter (qTree, KP_ATTRIBUTE, "DATA_CENTER_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_quit: NewParameter failed.");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("ASF", 3);
	NewValue (parameter, &value);

	/* create new parameter STATUS_CODE */
	parameter = NewParameter (qTree, KP_ATTRIBUTE, "STATUS_CODE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_quit: NewParameter failed.");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertInteger (statusCode, strlen(statusCode));
	NewValue (parameter, &value);

	/* if STATUS_CODE_COMMENT available */
	if (result != (V0_RESULT_STRUCT *)NULL)
	{
		if (result->odl_status_code_comment != (V0_ERR_LIST *)NULL)
		{
			if ((parameter=NewParameter(qTree, KP_ATTRIBUTE, "STATUS_CODE_COMMENT")) 
			       		== (PARAMETER)NULL )
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_quit: failed to create new "
					"parameter for STATUS_CODE_COMMENT");
				return (IMS_FATAL);
			}
	
			parameter->value_kind = KV_SEQUENCE;
			he_ptr = result->odl_status_code_comment;
	
			while (he_ptr != (V0_ERR_LIST *)NULL)
			{
				value =ODLConvertString(he_ptr->err_buf, strlen(he_ptr->err_buf));
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_quit: failed to create new "
						"sequence value for STATUS_CODE_COMMENT");
					return (IMS_FATAL);
				}
	
				he_ptr = he_ptr->next_p;
			}
	
		}
	}

	/* locate and paste monitor group */
	if ((group = FindAggregate (tTree, "MONITOR")) == 
		(AGGREGATE)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_quit: Missing group MONITOR.");
		return (IMS_FATAL);
	}
	if ((group = CopyAggregate (group)) == (AGGREGATE)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_quit: CopyAggregate failed.");
		return (IMS_FATAL);
	}
	PasteAggregate (qTree, group);

	return (IMS_OK);
} /* end of v0_msgTree__create_quit */

/***********************************************************************
**
** v0_msgTree__create_dirResult -
**
** purpose: formats and returns result messages for Directory Search  
**
** called by: v0_process
**
** return    IMS_OK  successful
**           IMS_FATAL system error
**
************************************************************************/
static int v0_msgTree__create_dirResult (IMS_MSG_STRUCT *msgDesc,
	AGGREGATE RxTree, AGGREGATE TxTree, char *statusCode, 
	V0_RESULT_STRUCT *result)
{
	V0_DATASET_LIST *dir_result;
	AGGREGATE       new_ds;   /* ptr to a dataset node */
	AGGREGATE       group;    /* ptr to MONITOR node */
	AGGREGATE       rTree;    /* point to the input tree */
	AGGREGATE       dTree;    /* DIRECTORY_RESULT node */
	PARAMETER       parameter;/* temp ptr to parameters */
	VALUE_DATA      value;    /* point to values */
	char            buf[IMS_COL80_LEN];     /* temp buffer for strings */

	/* 
	** Initialization
	*/
	dir_result = result->dataset_list;
	parameter  = (PARAMETER)NULL;
	group      = (AGGREGATE)NULL;
	new_ds     = (AGGREGATE)NULL;

	/*
	** create the DIRECTORY_RESULT node under TxTree 
	*/
	if ((dTree = NewAggregate (TxTree, KA_GROUP, "DIRECTORY_RESULT", NULL )) 
			== (AGGREGATE)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		  "v0_msgTree__create_dirResult: failed to create "
			"DIRECTORY_RESULT aggregate.");
		return (IMS_FATAL);
	}

	/*
	** locate the top level aggregate in RxTree, i.e. DIRECTORY_SEARCH node
	*/
	if ( (rTree = (AGGREGATE) FindAggregate (RxTree, "DIRECTORY_SEARCH") ) == 
		(AGGREGATE) NULL )
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"v0_msgTree__create_dirResult: failed to locate top level of RxTree.");
		return (IMS_FATAL);
	}

	/* 
	** get value of msgId  and attach it to the output dTree 
	*/
	if ((parameter = FindParameter (rTree, "MESSAGE_ID")) ==
			(PARAMETER) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_dirResult: missing parameter MESSAGE_ID");
		return (IMS_FATAL);
	}

	if ((parameter = CopyParameter (parameter)) == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_dirResult: failed to copy MESSAGE_ID.");
		return (IMS_FATAL);
	}
	if ((parameter = PasteParameter (dTree, parameter)) ==
			(PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_dirResult: PasteParameter failed.");
		return (IMS_FATAL);
	}

	/* 
	** insert to dTree the ASF data_center id 
	*/
 	parameter = NewParameter (dTree, KP_ATTRIBUTE, "DATA_CENTER_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			 "v0_msgTree__create_dirResult: failed to create DATA_CENTER_ID.");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("ASF", 3);
	NewValue (parameter, &value);


	/* 
	** get the value of odl_status from v0Desc and attach it to dTree as
	** status code
	*/
	parameter = NewParameter (dTree, KP_ATTRIBUTE, "STATUS_CODE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_dirResult: failed to create STATUS_CODE.");
		return (IMS_FATAL);

	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertInteger (statusCode, strlen(statusCode));
	NewValue (parameter, &value);

	
	/* 
	** get the number of dataset and attach it to NUMBER_OF_DATASETS 
	*/
	parameter = NewParameter (dTree, KP_ATTRIBUTE, "NUMBER_OF_DATASETS");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_dirResult: failed to create NUMBER_OF_DATASETS.");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	buf[0] = '\0';
	(void) sprintf (buf,"%d", result->dataset_count );
	value = ODLConvertInteger (buf, strlen(buf));
	NewValue (parameter, &value);


	/*
	** loop through the dataset_list and build a node for each dataset
	*/
	while (dir_result != (V0_DATASET_LIST *)NULL )
	{
		if ( (new_ds = NewAggregate (dTree, KA_GROUP, "DATASET", NULL) ) ==
			(AGGREGATE)NULL) 
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"failed to create new aggregate for DATASET.");
			return (IMS_FATAL);

		}

		/*
		** insert DATASET_ID
		*/
		if ( (parameter = NewParameter (new_ds, KP_ATTRIBUTE,"DATASET_ID"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"failed to create new parameter for DATASET_ID");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString (dir_result->dataset_id, 
				strlen (dir_result->dataset_id) );
		value.type = TV_SYMBOL;
		NewValue (parameter, &value);


		/*
		** insert MD_ENTRY_ID 
		*/
		if ( (parameter = NewParameter (new_ds, KP_ATTRIBUTE,"MD_ENTRY_ID"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"failed to create new parameter for MD_ENTRY_ID");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString (dir_result->md_id, 
				strlen (dir_result->md_id) );
		NewValue (parameter, &value);


		dir_result = dir_result->next_p;
	}


	/* get MONITOR from RxTree, i.e. rTree, and insert it to dTree.
	** The IK routines will update it with new timestamps
	*/
	if ( (group = FindAggregate (rTree, "MONITOR")) == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_dirResult: failed to locate MONITOR.");
		return (IMS_FATAL);
	}
	if ( (group = CopyAggregate (group)) == (AGGREGATE)NULL  )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_dirResult: failed to copy MONITOR");
		return (IMS_FATAL);
	}
	PasteAggregate (dTree, group);


	return (IMS_OK);	
} /* end of v0_msgTree__create_dirResult */

/***********************************************************************
**
** v0_msgTree__create_invResult -
**
** purpose: creates and formats result messages for inventory Search  
**
** called by: v0_process
**
** return    IMS_OK  successful
**           IMS_FATAL system error
**
************************************************************************/
static int v0_msgTree__create_invResult (IMS_MSG_STRUCT *msgDesc,
	AGGREGATE RxTree, AGGREGATE TxTree, char *statusCode, 
	V0_RESULT_STRUCT *result, int granule_limit)
{

	V0_DATASET_LIST *ds_result;
	AGGREGATE       new_ds;   /* temp ptr to a dataset node */
	AGGREGATE       valid_acct;/* temp ptr to VALID_ACCOUNT node */
	AGGREGATE       new_gnul;   /* temp ptr to a granule node */
	AGGREGATE       group;      /* tmp ptr to an aggregate */
	AGGREGATE       rTree;      /* point to the input tree */
	AGGREGATE       iTree;      /* INVENTORY_RESULT node */
	AGGREGATE       pTree;      /* temporary package tree */
	AGGREGATE       tmpTree;    /* temporary package tree */
	PARAMETER       parameter;  /* temp ptr to parameters */
	VALUE_DATA      value;      /* point to values */
	char            buf[IMS_COL30_LEN];     /* temp buffer for strings */
	int             chunk_granule_count; /* number of granules inserted */
	V0_VALUE_LIST   *hv, *tv;
	char            data[IMS_COL30_LEN];
	V0_USER_ACCT_LIST *ha_ptr;
	V0_ERR_LIST       *he_ptr;
	int             status;
	IMS_HASH_ENTRY item;
	IMS_HASH_ENTRY *hashPtr;
	int            empty_package;

	/* 
	** Initialization
	*/
	ds_result  = result->curr_dataset;
	parameter  = (PARAMETER)NULL;
	group      = (AGGREGATE)NULL;
	new_ds     = (AGGREGATE)NULL;
	new_gnul   = (AGGREGATE)NULL;
	chunk_granule_count = 0;
	empty_package =0;

	/*
	** create the INVENTORY_RESULT node under TxTree 
	*/
	if ((iTree = NewAggregate (TxTree, KA_GROUP, "INVENTORY_RESULT", NULL )) 
			== (AGGREGATE)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		  "v0_msgTree__create_invResult: failed to create INVENTORY_RESULT aggregate.");
		return (IMS_FATAL);
	}

	/*
	** locate the top level aggregate in RxTree, i.e. INVENTORY_SEARCH node
	*/
	if ( (rTree = (AGGREGATE) FindAggregate (RxTree, "INVENTORY_SEARCH") ) == 
		(AGGREGATE) NULL )
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"v0_msgTree__create_invResult: failed to locate top level of RxTree.");
		return (IMS_FATAL);
	}

	/* 
	** get value of msgId  and attach it to the output iTree 
	*/
	if ((parameter = FindParameter (rTree, "MESSAGE_ID")) ==
			(PARAMETER) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_invResult: missing parameter MESSAGE_ID");
		return (IMS_FATAL);
	}

	if ((parameter = CopyParameter (parameter)) == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_invResult: failed to copy MESSAGE_ID.");
		return (IMS_FATAL);
	}
	if ((parameter = PasteParameter (iTree, parameter)) ==
			(PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_invResult: PasteParameter failed.");
		return (IMS_FATAL);
	}

	/* 
	** insert to iTree the ASF data_center id 
	*/
 	parameter = NewParameter (iTree, KP_ATTRIBUTE, "DATA_CENTER_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			 "v0_msgTree__create_invResult: failed to create DATA_CENTER_ID.");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("ASF", 3);
	NewValue (parameter, &value);


	/* 
	** get the value of odl_status from v0Desc and attach it to iTree as
	** status code
	*/
	parameter = NewParameter (iTree, KP_ATTRIBUTE, "STATUS_CODE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_invResult: failed to create STATUS_CODE.");
		return (IMS_FATAL);

	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertInteger (statusCode, strlen(statusCode));
	NewValue (parameter, &value);

	/* 
	** include dataset information information if there are granules be sent out 
	** from this message 
	*/
	if ( !result->last_msg_flag && 
		result->curr_granule != (V0_GRANULE_LIST *)NULL )
	{
		/*
		** new DATASET node
		*/
		if ( (new_ds = NewAggregate (iTree, KA_GROUP, "DATASET", NULL) ) ==
			(AGGREGATE)NULL) 
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new aggregate "
				"for DATASET.");
			return (IMS_FATAL);

		}

		/*
		** insert DATASET_ID
		*/
		if ( (parameter = NewParameter (new_ds, KP_ATTRIBUTE,"DATASET_ID"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new parameter "
				"for DATASET_ID");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString (ds_result->dataset_id, 
				strlen (ds_result->dataset_id) );
		value.type = TV_SYMBOL;
		NewValue (parameter, &value);
	

		/*
		** insert MD_ENTRY_ID 
		*/
		if ( (parameter = NewParameter (new_ds, KP_ATTRIBUTE,"MD_ENTRY_ID"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new parameter "
				"for MD_ENTRY_ID");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString (ds_result->md_id, strlen(ds_result->md_id) );
		NewValue (parameter, &value);


		/*
		** insert SENSOR_NAME
		*/
		if ( (parameter = NewParameter (new_ds, KP_ATTRIBUTE,"SENSOR_NAME"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new parameter "
				"for SENSOR_NAME");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString(ds_result->sensor,strlen(ds_result->sensor) );
		NewValue (parameter, &value);


		/*
		** insert SOURCE_NAME
		*/
		if ( (parameter = NewParameter (new_ds, KP_ATTRIBUTE,"SOURCE_NAME"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new parameter "
				"for SOURCE_NAME");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString (ds_result->platform, 
			strlen(ds_result->platform) );
		NewValue (parameter, &value);


		/*
		** insert PARAMETER
		*/
		if (ds_result->parameter != (V0_VALUE_LIST *)NULL)
		{
			if ( (parameter = NewParameter (new_ds, KP_ATTRIBUTE,"PARAMETER"))
					== (PARAMETER)NULL )
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new parameter "
					"for PARAMETER");
				return (IMS_FATAL);
			}
	
			parameter->value_kind = KV_SEQUENCE; 
			hv = ds_result->parameter;
	
			while (hv != (V0_VALUE_LIST *)NULL )
			{
				value = ODLConvertString (hv->char_value1,strlen(hv->char_value1));
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for PARAMETER");
					return (IMS_FATAL);
				}
			
				hv = hv->next_p;
			}
		}
	
		/*
		** insert VALID_ACCOUNT group 
		*/
		ha_ptr = ds_result->account_list;

		while (ha_ptr != (V0_USER_ACCT_LIST *)NULL)
		{
			if ((valid_acct = NewAggregate(new_ds, KA_GROUP, "VALID_ACCOUNT", NULL))
		                        	== (AGGREGATE)NULL) 
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new "
					"aggregate for VALID_ACCOUNT.");
				return (IMS_FATAL);
			}

			if ( ha_ptr->account_id[0] != '\0')
			{
				/* 
				** insert ACCOUNT_NUMBER  
				*/	
				if ( (parameter = 
			            	NewParameter (valid_acct, KP_ATTRIBUTE, "ACCOUNT_NUMBER")) 
			            	== (PARAMETER)NULL )
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new "
						"parameter for ACCOUNT_NUMBER");
					return (IMS_FATAL);
				}
	
				parameter->value_kind = KV_SCALAR;
				value=ODLConvertString(ha_ptr->account_id,strlen(ha_ptr->account_id));
				NewValue (parameter, &value);
	

				/*
				** insert BALANCE
				*/
				if ( (parameter = 
			            	NewParameter (valid_acct, KP_ATTRIBUTE, "BALANCE")) 
			            	== (PARAMETER)NULL )
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new "
						"parameter for BALANCE");
					return (IMS_FATAL);
				}
	
				parameter->value_kind = KV_SCALAR;
				buf[0] = '\0';
				(void) sprintf(buf, "%-3.4f", ha_ptr->balance);
				value = ODLConvertReal (buf, strlen(buf) );
				NewValue (parameter, &value);
			}

			/* 
			** insert ERROR if needed 
			*/	
			if (ha_ptr->err_msg != (V0_ERR_LIST *)NULL)
			{
				if ( (parameter = 
			           	NewParameter (valid_acct, KP_ATTRIBUTE, "ERROR")) 
					       	== (PARAMETER)NULL )
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new "
						"parameter for ERROR");
					return (IMS_FATAL);
				}

				parameter->value_kind = KV_SEQUENCE;
				he_ptr = ha_ptr->err_msg;

				while (he_ptr != (V0_ERR_LIST *)NULL)
				{
					value =ODLConvertString(he_ptr->err_buf, strlen(he_ptr->err_buf));
					if (NewSqValue(parameter, &value)==NULL)
					{
						(void) ims_msg (msgDesc, IMS_FATAL,
							"v0_msgTree__create_invResult: failed to create "
							"new sequence value for ERROR");
						return (IMS_FATAL);
					}
	
					he_ptr = he_ptr->next_p;
				}
			}

			ha_ptr = ha_ptr->next_p;

		} /* built a list of valid accounts */

		/*
		** send the following info if this is the first chunk to be return for
		** a dataset.  We can find out if this is the first chunk by checking
		** the curr_granule_count
		*/
		if (!result->first_chunk_sent)
		{
		
			/*
			** insert Dataset comment, if available
			*/

			if (result->dataset_comment[0] != '\0')
			{
				 result->dataset_info_list1 = (V0_VALUE_LIST *)NULL;
				 result->dataset_info_list2 = (V0_VALUE_LIST *)NULL;

				/* 
				** tokenize the long comment text by words and store in 
				** dataset_info_list1 
				*/
				if ( (status = tokenize_dataset_info 
										 (msgDesc, result, DS_INFO_COMMENT)) < IMS_OK)
				{
					hv = result->dataset_info_list1;
					while (hv != (V0_VALUE_LIST *)NULL)
					{
						tv = hv->next_p;
						free(hv);
						hv = tv;
					}
					result->dataset_info_list1 = (V0_VALUE_LIST *)NULL;
					return (status);
				}
	
				/*
				** format the dataset comment information
				*/
				if ( (status = format_dataset_info (msgDesc, result)) < IMS_OK)
				{
					hv = result->dataset_info_list1;
					while (hv != (V0_VALUE_LIST *)NULL)
					{
						tv = hv->next_p;
						free(hv);
						hv = tv;
					}
					result->dataset_info_list1 = (V0_VALUE_LIST *)NULL;
					
					hv = result->dataset_info_list2;
					while (hv != (V0_VALUE_LIST *)NULL)
					{
						tv = hv->next_p;
						free(hv);
						hv = tv;
					}
					result->dataset_info_list2 = (V0_VALUE_LIST *)NULL;
					return (status);
				}
	
				if ( (parameter = NewParameter (new_ds, KP_ATTRIBUTE,"COMMENT"))
					== (PARAMETER)NULL )
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new parameter "
						"for Dataset COMMENT");

						hv = result->dataset_info_list1;
						while (hv != (V0_VALUE_LIST *)NULL)
						{
							tv = hv->next_p;
							free(hv);
							hv = tv;
						}
						result->dataset_info_list1 = (V0_VALUE_LIST *)NULL;

						hv = result->dataset_info_list2;
						while (hv != (V0_VALUE_LIST *)NULL)
						{
							tv = hv->next_p;
							free(hv);
							hv = tv;
						}
						result->dataset_info_list2 = (V0_VALUE_LIST *)NULL;

					return (IMS_FATAL);
				}
	
				parameter->value_kind = KV_SEQUENCE; 
				hv = result->dataset_info_list2;
	
				while (hv != (V0_VALUE_LIST *)NULL )
				{
					value = ODLConvertString (hv->char_value1,strlen(hv->char_value1));
					if (NewSqValue(parameter, &value)==NULL)
					{
						(void) ims_msg (msgDesc, IMS_FATAL,
							"v0_msgTree__create_invResult: failed to create new sequence "
							"value for Dataset COMMENT");

						hv = result->dataset_info_list1;
						while (hv != (V0_VALUE_LIST *)NULL)
						{
							tv = hv->next_p;
							free(hv);
							hv = tv;
						}
						result->dataset_info_list1 = (V0_VALUE_LIST *)NULL;

						hv = result->dataset_info_list2;
						while (hv != (V0_VALUE_LIST *)NULL)
						{
							tv = hv->next_p;
							free(hv);
							hv = tv;
						}
						result->dataset_info_list2 = (V0_VALUE_LIST *)NULL;

						return (IMS_FATAL);
					}
				
					hv = hv->next_p;
				}

				hv = result->dataset_info_list1;
				while (hv != (V0_VALUE_LIST *)NULL)
				{
					tv = hv->next_p;
					free(hv);
					hv = tv;
				}
				result->dataset_info_list1 = (V0_VALUE_LIST *)NULL;

				hv = result->dataset_info_list2;
				while (hv != (V0_VALUE_LIST *)NULL)
				{
					tv = hv->next_p;
					free(hv);
					hv = tv;
				}
				result->dataset_info_list2 = (V0_VALUE_LIST *)NULL;

			} /* insert dataset comment */
			
			/*
			** insert dataset restriction, if available
			*/
			if (result->dataset_restriction[0] != '\0')
			{
				 result->dataset_info_list1 = (V0_VALUE_LIST *)NULL;
				 result->dataset_info_list2 = (V0_VALUE_LIST *)NULL;

				/* 
				** tokenize the long comment text by words and store in 
				** dataset_info_list1 
				*/
				if ( (status = tokenize_dataset_info 
										 (msgDesc, result, DS_INFO_RESTRICTION)) < IMS_OK)
				{
					hv = result->dataset_info_list1;
					while (hv != (V0_VALUE_LIST *)NULL)
					{
						tv = hv->next_p;
						free(hv);
						hv = tv;
					}
					result->dataset_info_list1 = (V0_VALUE_LIST *)NULL;
					return (status);
				}
					
				/*
				** format the dataset restriction
				*/
				if ( (status = format_dataset_info (msgDesc, result)) < IMS_OK)
				{
					hv = result->dataset_info_list1;
					while (hv != (V0_VALUE_LIST *)NULL)
					{
						tv = hv->next_p;
						free(hv);
						hv = tv;
					}
					result->dataset_info_list1 = (V0_VALUE_LIST *)NULL;
					
					hv = result->dataset_info_list2;
					while (hv != (V0_VALUE_LIST *)NULL)
					{
						tv = hv->next_p;
						free(hv);
						hv = tv;
					}
					result->dataset_info_list2 = (V0_VALUE_LIST *)NULL;
					return (status);
				}
	
				if ( (parameter = NewParameter (new_ds, KP_ATTRIBUTE,"RESTRICTION"))
					== (PARAMETER)NULL )
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new parameter "
						"for Dataset RESTRICTION");

						hv = result->dataset_info_list1;
						while (hv != (V0_VALUE_LIST *)NULL)
						{
							tv = hv->next_p;
							free(hv);
							hv = tv;
						}
						result->dataset_info_list1 = (V0_VALUE_LIST *)NULL;

						hv = result->dataset_info_list2;
						while (hv != (V0_VALUE_LIST *)NULL)
						{
							tv = hv->next_p;
							free(hv);
							hv = tv;
						}
						result->dataset_info_list2 = (V0_VALUE_LIST *)NULL;

					return (IMS_FATAL);
				}
	
				parameter->value_kind = KV_SEQUENCE; 
				hv = result->dataset_info_list2;
	
				while (hv != (V0_VALUE_LIST *)NULL )
				{
					value = ODLConvertString (hv->char_value1,strlen(hv->char_value1));
					if (NewSqValue(parameter, &value)==NULL)
					{
						(void) ims_msg (msgDesc, IMS_FATAL,
							"v0_msgTree__create_invResult: failed to create new sequence "
							"value for Dataset RESTRICTION");

						hv = result->dataset_info_list1;
						while (hv != (V0_VALUE_LIST *)NULL)
						{
							tv = hv->next_p;
							free(hv);
							hv = tv;
						}
						result->dataset_info_list1 = (V0_VALUE_LIST *)NULL;

						hv = result->dataset_info_list2;
						while (hv != (V0_VALUE_LIST *)NULL)
						{
							tv = hv->next_p;
							free(hv);
							hv = tv;
						}
						result->dataset_info_list2 = (V0_VALUE_LIST *)NULL;

						return (IMS_FATAL);
					}
				
					hv = hv->next_p;
				}

				hv = result->dataset_info_list1;
				while (hv != (V0_VALUE_LIST *)NULL)
				{
					tv = hv->next_p;
					free(hv);
					hv = tv;
				}
				result->dataset_info_list1 = (V0_VALUE_LIST *)NULL;

				hv = result->dataset_info_list2;
				while (hv != (V0_VALUE_LIST *)NULL)
				{
					tv = hv->next_p;
					free(hv);
					hv = tv;
				}
				result->dataset_info_list2 = (V0_VALUE_LIST *)NULL;

			} /* insert dataset restriction */
			
			/*
			** insert Dynamic Packaging information
			*/
			
			if (v0_package_HashPtr != (IMS_HASH_STRUCT *)NULL)
			{
				item.key = (char *)ds_result->dataset_id;
				item.data = (void *)NULL;

				if ((hashPtr = ims_hashSearch
					(v0_package_HashPtr, &item, IMS_FIND, msgDesc)) 
						== (IMS_HASH_ENTRY *) NULL)
				{
					(void) ims_msg (msgDesc, IMS_WARNING,
						">>> Failed to find dynamic packaging information for %s\n"
						">>> Package file modification needed.", 
						ds_result->dataset_id);

  				pTree = (AGGREGATE) NULL;
	 
					if ( (status = create_empty_package 
											 (msgDesc, &pTree, ds_result->dataset_id)) < IMS_OK)
					{
						(void) ims_msg (msgDesc, status, 
						 "v0_msgTree__create_invResult: failed to create empty package"
						 " group.");
						(void) v0_msgTree__destroy (pTree);
						return (status);
					}

					if ((tmpTree = FindAggregate (pTree, "PACKAGE")) == 
						(AGGREGATE)NULL)
					{
						(void) ims_msg (msgDesc, IMS_FATAL,
							"v0_msgTree__create_invResult: can not locate"
							" empty PACKAGE group.");
						(void) v0_msgTree__destroy (pTree);
						return (IMS_FATAL);
					}
	
					empty_package = 1;

				}
				

#ifdef ODEBUG
				printf("\n========= Package tree  ========\n");
				if (!empty_package)
				{
					PrintLabel ( (AGGREGATE)hashPtr->data);
				}
				else
				{
					PrintLabel ( tmpTree );
				}

#endif 

				if (!empty_package)
				{
					group = CopyAggregate ((AGGREGATE)hashPtr->data);
				}
				else
				{
					group = CopyAggregate ((AGGREGATE)tmpTree);
				}

				if ( group == (AGGREGATE)NULL )
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to copy AGGREGATE PACKAGE.");
					return (IMS_FATAL);
				}
				else
				{
					PasteAggregate (new_ds, group);

					if (empty_package)
					{
						(void) v0_msgTree__destroy (pTree);
					}

					/*
					** To avoid bumping to the 64K chunk limit, no granules will be 
					** returned in a result message if package information is included 
					*/
					chunk_granule_count = V0_MAX_CHUNK_GRANULES;
				}
			} /* if hash ptr is not null */
			
			result->first_chunk_sent = 1;

		} /* if this is the first chunk for the dataset by checking current 
			 granule count */
		
	} /* if curr_granule is not NULL */

	while ( (!result->last_msg_flag) && 
		(result->curr_granule != (V0_GRANULE_LIST *)NULL) &&
		(chunk_granule_count < V0_MAX_CHUNK_GRANULES) &&
		(result->curr_granule_count < granule_limit))
	{
		/* 
		** insert GRANULE node   
		*/	
		if ( (new_gnul = NewAggregate (new_ds, KA_GROUP, "GRANULE", NULL) ) ==
			(AGGREGATE)NULL) 
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new aggregate "
				"for GRANULE.");
			return (IMS_FATAL);
		}

		/*
		** insert GRANULE_ID
		*/
		if ( (parameter = NewParameter (new_gnul, KP_ATTRIBUTE,"GRANULE_ID"))
			== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new parameter "
				"for GRANULE_ID");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString (result->curr_granule->granule_id, 
				strlen (result->curr_granule->granule_id) );
		NewValue (parameter, &value);
	

		/*
		** insert START_DATE
		*/
		if ((parameter = NewParameter (new_gnul, KP_ATTRIBUTE,"START_DATE"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new parameter "
				"for START_DATE");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		buf[0] ='\0';
		/* 
		** use a temp string for conversion so we could keep the original value. 
		** ODL somehow changes the string to another daytime format after the 
		** conversion. 
		*/
		strcpy (buf, result->curr_granule->start_time);
		value = ODLConvertDateTime (buf, strlen (buf) );
		NewValue (parameter, &value);

		/*
		** insert STOP_DATE
		*/
		if ( (parameter = NewParameter (new_gnul, KP_ATTRIBUTE,"STOP_DATE"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new parameter "
				"for STOP_DATE");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		buf[0] ='\0';
		strcpy (buf, result->curr_granule->stop_time);
		value = ODLConvertDateTime (buf, strlen (buf) );
		NewValue (parameter, &value);
		
		/* 
		** insert CAMPAIGN
		*/
		if ( (parameter = NewParameter (new_gnul, KP_ATTRIBUTE,"CAMPAIGN"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new parameter "
				"for CAMPAIGN");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString (ds_result->campaign, 
			strlen(ds_result->campaign) );
		NewValue (parameter, &value);

		/* 
		** insert DAY_NIGHT  
		*/	
		if ( (parameter = NewParameter (new_gnul, KP_ATTRIBUTE,"DAY_NIGHT"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new parameter "
				"for DAY_NIGHT");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString (ds_result->day_night, 
			strlen(ds_result->day_night) );
		NewValue (parameter, &value);


		/* 
		** insert PROCESSING_LEVEL  
		*/	
		if ( (parameter = NewParameter (new_gnul, KP_ATTRIBUTE,"PROCESSING_LEVEL"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new parameter "
				"for PROCESS_LEVEL");
			return (IMS_FATAL);
		} 
		parameter->value_kind = KV_SCALAR;
		buf[0] ='\0';
		sprintf (buf, "%d", ds_result->process_level);
		value = ODLConvertString (buf, strlen(buf) );
		NewValue (parameter, &value);

		/* 
		** insert PACKAGE_ID
		*/
		if ( (parameter = NewParameter (new_gnul, KP_ATTRIBUTE,"PACKAGE_ID"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new parameter "
				"for PACKAGE_ID");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString ("*", 1 );
		NewValue (parameter, &value);

		/* 
		** insert COMMENT
		*/
		if ( (parameter = NewParameter (new_gnul, KP_ATTRIBUTE,"COMMENT"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new parameter "
				"for COMMENT");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SEQUENCE; 
		hv = result->curr_granule->formatted_list;

		while (hv != (V0_VALUE_LIST *)NULL )
		{
			value = ODLConvertString (hv->char_value1,strlen(hv->char_value1));
			if (NewSqValue(parameter, &value)==NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new sequence "
					"value for COMMENT");
				return (IMS_FATAL);
			}
		
			hv = hv->next_p;
		}

		/* 
		** insert POINT_LOC, RANGE_LOC, POLYGON_LOC accordingly  
		*/	
		if (result->curr_dataset->spatial_type == 1) 
		{ 
			if ( (group = NewAggregate (new_gnul, KA_GROUP, "POINT_LOC", NULL) ) 
				== (AGGREGATE)NULL) 
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new aggregate "
					"for POINT_LOC.");
				return (IMS_FATAL);
			}

			/* 
			** insert LATITUDE  
			*/	
			if ( (parameter = NewParameter (group, KP_ATTRIBUTE,
				"LATITUDE")) == (PARAMETER)NULL )
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new parameter "
					"for LATITUDE");
				return (IMS_FATAL);
			}

			parameter->value_kind = KV_SEQUENCE;
			buf[0] = '\0';
			(void) sprintf(buf, "%-3.4f", result->curr_granule->center_lat);
			value = ODLConvertReal (buf, strlen(buf) );
			NewValue (parameter, &value);


			/* 
			** insert LONGITUDE  
			*/	
			if ( (parameter = NewParameter (group, KP_ATTRIBUTE,
				"LONGITUDE")) == (PARAMETER)NULL ) 
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new parameter "
					"for LONGITUDE");
				return (IMS_FATAL);
			}

			parameter->value_kind = KV_SEQUENCE;
			buf[0] = '\0';
			(void) sprintf(buf, "%-3.4f", result->curr_granule->center_lon);
			value = ODLConvertReal (buf, strlen(buf) );
			NewValue (parameter, &value);


		}

		else if (result->curr_dataset->spatial_type == 2)
		{

			if ( (group = NewAggregate (new_gnul, KA_GROUP, "RANGE_LOC", NULL) ) 
				== (AGGREGATE)NULL) 
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new aggregate "
					"for RANGE_LOC.");
				return (IMS_FATAL);
			}

			/* 
			** insert NORTH_LATITUDE  
			*/	
			if ( (parameter = NewParameter (group, KP_ATTRIBUTE,
				"NORTH_LATITUDE")) == (PARAMETER)NULL )
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new parameter "
					"for NORTH_LATITUDE");
				return (IMS_FATAL);
			}

			parameter->value_kind = KV_SCALAR;
			buf[0] = '\0';
			(void) sprintf(buf, "%-3.4f", result->curr_granule->north_lat);
			value = ODLConvertReal (buf, strlen(buf) );
			NewValue (parameter, &value);


			/* 
			** insert SOUTH_LATITUDE  
			*/	
			if ( (parameter = NewParameter (group, KP_ATTRIBUTE,
				"SOUTH_LATITUDE")) == (PARAMETER)NULL )
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new parameter "
					"for SOUTH_LATITUDE");
				return (IMS_FATAL);
			}

			parameter->value_kind = KV_SCALAR;
			buf[0] = '\0';
			(void) sprintf(buf, "%-3.4f", result->curr_granule->south_lat);
			value = ODLConvertReal (buf, strlen(buf) );
			NewValue (parameter, &value);


			/* 
			** insert WEST_LONGITUDE  
			*/	
			if ( (parameter = NewParameter (group, KP_ATTRIBUTE,
				"WEST_LONGITUDE")) == (PARAMETER)NULL ) 
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new parameter "
					"for WEST_LONGITUDE");
				return (IMS_FATAL);
			}

			parameter->value_kind = KV_SCALAR;
			buf[0] = '\0';
			(void) sprintf(buf, "%-3.4f", result->curr_granule->west_lon);
			value = ODLConvertReal (buf, strlen(buf) );
			NewValue (parameter, &value);


			/* 
			** insert EAST_LONGITUDE  
			*/	
			if ( (parameter = NewParameter (group, KP_ATTRIBUTE,
				"EAST_LONGITUDE")) == (PARAMETER)NULL )
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new parameter "
					"for EAST_LONGITUDE");
				return (IMS_FATAL);
			}

			parameter->value_kind = KV_SCALAR;
			buf[0] = '\0';
			(void) sprintf(buf, "%-3.4f", result->curr_granule->east_lon);
			value = ODLConvertReal (buf, strlen(buf) );
			NewValue (parameter, &value);


		}

		else if (result->curr_dataset->spatial_type == 4)
		{

			if ( (group = NewAggregate (new_gnul, KA_GROUP, "POLYGON_LOC", NULL) ) 
				== (AGGREGATE)NULL) 
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new aggregate "
					"for POLYGON_LOC."); 
					return (IMS_FATAL); 
			} 

			/* 
			** insert LATITUDE  
			*/	
			if ( (parameter = NewParameter (group, KP_ATTRIBUTE,
				"LATITUDE")) == (PARAMETER)NULL )
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new parameter "
					"for LATITUDE");
				return (IMS_FATAL);
			}

			parameter->value_kind = KV_SEQUENCE;

			if (strcmp (result->curr_granule->asc_desc,"A") == 0)
			{
				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->ne_lat);
				value = ODLConvertReal (data, strlen(data) );

				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LATITUDE");
					return (IMS_FATAL);
				}

				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->fe_lat);
				value = ODLConvertReal (data, strlen(data) );
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LATITUDE");
					return (IMS_FATAL);
				}

				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->fs_lat);
				value = ODLConvertReal (data, strlen(data) );
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LATITUDE");
					return (IMS_FATAL);
				}
	
				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->ns_lat);
				value = ODLConvertReal (data, strlen(data) );
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LATITUDE");
					return (IMS_FATAL);
				}
			}

			else if ( strcmp (result->curr_granule->asc_desc,"D") == 0)
			{
				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->fs_lat);
				value = ODLConvertReal (data, strlen(data) );
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LATITUDE");
					return (IMS_FATAL);
				}

				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->ns_lat);
				value = ODLConvertReal (data, strlen(data) );
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LATITUDE");
					return (IMS_FATAL);
				}

				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->ne_lat);
				value = ODLConvertReal (data, strlen(data) );
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LATITUDE");
					return (IMS_FATAL);
				}
	
				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->fe_lat);
				value = ODLConvertReal (data, strlen(data) );
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LATITUDE");
					return (IMS_FATAL);
				}
			}


			/* 
			** insert LONGITUDE  
			*/	
			if ( (parameter = NewParameter (group, KP_ATTRIBUTE,
				"LONGITUDE")) == (PARAMETER)NULL ) 
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new parameter "
					"for LONGITUDE");
				return (IMS_FATAL);
			}

			parameter->value_kind = KV_SEQUENCE;

			if (strcmp(result->curr_granule->asc_desc,"A") == 0)
			{
				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->ne_lon);
				value = ODLConvertReal (data, strlen(data) );

				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LONGITUDE");
					return (IMS_FATAL);
				}

				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->fe_lon);
				value = ODLConvertReal (data, strlen(data) );
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LONGITUDE");
					return (IMS_FATAL);
				}
	
				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->fs_lon);
				value = ODLConvertReal (data, strlen(data) );
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LONGITUDE");
					return (IMS_FATAL);
				}

				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->ns_lon);
				value = ODLConvertReal (data, strlen(data) );
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LONGITUDE");
					return (IMS_FATAL);
				}
			}

			else if ( strcmp (result->curr_granule->asc_desc,"D") == 0)
			{
				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->fs_lon);
				value = ODLConvertReal (data, strlen(data) );
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LONGITUDE");
					return (IMS_FATAL);
				}

				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->ns_lon);
				value = ODLConvertReal (data, strlen(data) );
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LONGITUDE");
					return (IMS_FATAL);
				}
	
				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->ne_lon);
				value = ODLConvertReal (data, strlen(data) );
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LONGITUDE");
					return (IMS_FATAL);
				}

				data[0] = '\0';
				(void) sprintf(data, "%-3.4f", result->curr_granule->fe_lon);
				value = ODLConvertReal (data, strlen(data) );
				if (NewSqValue(parameter, &value)==NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new sequence "
						"value for POLYGON_LOC LONGITUDE");
					return (IMS_FATAL);
				}
			}

			/* 
			** insert CENTROID_LAT  
			*/	
			if ( (parameter = NewParameter (group, KP_ATTRIBUTE,
				"CENTROID_LAT")) == (PARAMETER)NULL )
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new parameter "
					"for CENTROID_LAT");
				return (IMS_FATAL);
			}

			parameter->value_kind = KV_SCALAR;

			strcpy (data,"\0");
			(void) sprintf(data, "%-3.4f", result->curr_granule->center_lat);
			value = ODLConvertReal (data, strlen(data) );
			NewValue (parameter, &value);

			/* 
			** insert CENTROID_LON  
			*/	
			if ( (parameter = NewParameter (group, KP_ATTRIBUTE,
				"CENTROID_LON")) == (PARAMETER)NULL )
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_invResult: failed to create new parameter "
					"for CENTROID_LON");
				return (IMS_FATAL);
			}

			parameter->value_kind = KV_SCALAR;

			strcpy (data,"\0");
			(void) sprintf(data, "%-3.4f", result->curr_granule->center_lon);
			value = ODLConvertReal (data, strlen(data) );
			NewValue (parameter, &value);

			/* 
			** insert POLE_INCLUDED  
			*/	
			if ( (strcmp(result->curr_granule->pole_included,"N") == 0) ||
				  (strcmp(result->curr_granule->pole_included,"S") == 0) )
			{
				if ( (parameter = NewParameter (group, KP_ATTRIBUTE,"POLE_INCLUDED"))
						== (PARAMETER)NULL )
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"v0_msgTree__create_invResult: failed to create new parameter "
						"for POLE_INCLUDED");
					return (IMS_FATAL);
				}

				parameter->value_kind = KV_SCALAR;
				value = ODLConvertString (result->curr_granule->pole_included, 
				strlen(result->curr_granule->pole_included) );
				NewValue (parameter, &value);
			}


		}

		chunk_granule_count += 1;
		result->curr_granule_count += 1;

		result->curr_granule = result->curr_granule->next_p;

		/*
		** Check to set the last_msg_flag
		*/
		if ((result->last_dataset_flag) &&
			((result->curr_granule == (V0_GRANULE_LIST *)NULL) ||
			 (result->curr_granule_count >= granule_limit)))
		{
			result->last_msg_flag = 1;
		}


	} /* end while */

	/*
	** The last result chunk for a dataset requires curr_granule_count
	** placed in the odl tree. 
	*/
	if ( ( (result->curr_granule == (V0_GRANULE_LIST *)NULL) &&
	       (ds_result->granule_count >0))   ||
	     ( (result->curr_granule != (V0_GRANULE_LIST *)NULL) &&
	       (result->curr_granule_count >= granule_limit)))
	{
		if ( (parameter = NewParameter (new_ds, KP_ATTRIBUTE,
			"NUMBER_OF_GRANULE_HITS")) == (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new parameter "
				"for NUMBER_OF_GRANULE_HITS");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		buf[0] = '\0';

		if (result->curr_granule == (V0_GRANULE_LIST *)NULL)
		{
			(void) sprintf(buf, "%d", ds_result->granule_count);
		}
		else
		{
			/*
			** insert (curr_granule_count + 1) as NUMBER_OF_GRANULE_HITS
			** if there are more granule results than what's allowed
			** to return
			*/
			(void) sprintf(buf, "%d", (result->curr_granule_count + 1));
			result->curr_granule = (V0_GRANULE_LIST *)NULL;
		}

		value = ODLConvertInteger (buf, strlen(buf) );
		NewValue (parameter, &value);
	}
		
	/*
	** This is done only for the last message.
	*/
	if (result->last_msg_flag) 
	{
		/*
		** insert NUMBER_OF_DATASETS
		*/	
		if ( (parameter = NewParameter (iTree, KP_ATTRIBUTE,
			"NUMBER_OF_DATASETS")) == (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_invResult: failed to create new parameter for DATASETS");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		buf[0] = '\0';
		(void) sprintf(buf, "%d", result->curr_dataset_count);
		value = ODLConvertInteger (buf, strlen(buf) );
		NewValue (parameter, &value);
	}


	/*
	** insert MONITOR
	*/	
	if ( (group = FindAggregate (rTree, "MONITOR")) == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_invResult: failed to locate MONITOR.");
		return (IMS_FATAL);
	}
	if ( (group = CopyAggregate (group)) == (AGGREGATE)NULL  )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_invResult: failed to copy MONITOR");
		return (IMS_FATAL);
	}
	PasteAggregate (iTree, group);

	return (IMS_OK);
} /* end of v0_msgTree__create_invResult */


/***********************************************************************
**
** v0_msgTree__create_prodResult -
**
** purpose: formats and returns result messages for Product Request  
**
** called by: v0_process
**
** return    IMS_OK  successful
**           IMS_FATAL system error
**
************************************************************************/
static int v0_msgTree__create_prodResult (IMS_MSG_STRUCT *msgDesc,
	AGGREGATE RxTree, AGGREGATE TxTree, char *statusCode, 
	V0_RESULT_STRUCT *result)
{
	AGGREGATE       group;   	/* ptr to a group */ 
	AGGREGATE       contact;  /* ptr to DAAC_CONTACT_ADDRESS */
	AGGREGATE       rTree;    /* point to the input tree */
	AGGREGATE       pTree;    /* PRODUCT_RESULT node */
	PARAMETER       parameter;/* temp ptr to parameters */
	VALUE_DATA      value;    /* point to values */
	V0_CONTACT_INFO *contact_info;
	V0_ERR_LIST     *he_ptr;
	char            buf[IMS_COL60_LEN];     /* temp buffer for strings */
	char            *p;

	/* 
	** Initialization
	*/
	parameter  = (PARAMETER)NULL;
	group      = (AGGREGATE)NULL;
	contact_info = result->contact_info;

	/*
	** create the PRODUCT_RESULT node under TxTree 
	*/
	if ((pTree = NewAggregate (TxTree, KA_GROUP, "PRODUCT_RESULT", NULL )) 
			== (AGGREGATE)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		  "v0_msgTree__create_prodResult: failed to create DIRECTORY_RESULT aggregate.");
		return (IMS_FATAL);
	}

	/*
	** locate the top level aggregate in RxTree, i.e. PRODUCT_REQUEST node
	*/
	if ( (rTree = (AGGREGATE) FindAggregate (RxTree, "PRODUCT_REQUEST") ) == 
		(AGGREGATE) NULL )
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"v0_msgTree__create_prodResult: failed to locate top level of RxTree.");
		return (IMS_FATAL);
	}

	/* 
	** get value of msgId  and attach it to the output pTree 
	*/
	if ((parameter = FindParameter (rTree, "MESSAGE_ID")) ==
			(PARAMETER) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: missing parameter MESSAGE_ID");
		return (IMS_FATAL);
	}

	if ((parameter = CopyParameter (parameter)) == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to copy MESSAGE_ID.");
		return (IMS_FATAL);
	}
	if ((parameter = PasteParameter (pTree, parameter)) ==
			(PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: PasteParameter failed.");
		return (IMS_FATAL);
	}

	/* 
	** insert to pTree the ASF data_center id 
	*/
 	parameter = NewParameter (pTree, KP_ATTRIBUTE, "DATA_CENTER_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			 "v0_msgTree__create_prodResult: failed to create DATA_CENTER_ID.");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("ASF", 3);
	NewValue (parameter, &value);


	/* 
	** get the value of odl_status from v0Desc and attach it to pTree as
	** status code
	*/
	parameter = NewParameter (pTree, KP_ATTRIBUTE, "STATUS_CODE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to create STATUS_CODE.");
		return (IMS_FATAL);

	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertInteger (statusCode, strlen(statusCode));
	NewValue (parameter, &value);

        /* if STATUS_CODE_COMMENT available */
        if (result != (V0_RESULT_STRUCT *)NULL)
        {
                if (result->odl_status_code_comment != (V0_ERR_LIST *)NULL)
                {
                        if ((parameter=NewParameter(pTree, KP_ATTRIBUTE, 
				"STATUS_CODE_COMMENT")) == (PARAMETER)NULL )
                        {
                                (void) ims_msg (msgDesc, IMS_FATAL,
                                        "v0_msgTree__create_prodResult: failed to create new "
                                        "parameter for STATUS_CODE_COMMENT");
                                return (IMS_FATAL);
                        }
 
                        parameter->value_kind = KV_SEQUENCE;
                        he_ptr = result->odl_status_code_comment;
 
                        while (he_ptr != (V0_ERR_LIST *)NULL)
                        {
                                value =ODLConvertString(he_ptr->err_buf, strlen(he_ptr->err_buf));
                                if (NewSqValue(parameter, &value)==NULL)
                                {
                                        (void) ims_msg (msgDesc, IMS_FATAL,
                                                "v0_msgTree__create_prodResult: failed to create new "
                                                "sequence value for STATUS_CODE_COMMENT");
                                        return (IMS_FATAL);
                                }
 
                                he_ptr = he_ptr->next_p;
                        }
 
                }
        }
 
	
	if ((contact = NewAggregate(pTree, KA_GROUP,"DAAC_CONTACT_ADDRESS", NULL) ) ==
		(AGGREGATE)NULL) 
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"failed to create new aggregate for DAAC_CONTACT_ADDRESS.");
		return (IMS_FATAL);

	}


	/* 
	** insert DAAC_ORDER_ID 
	*/
	parameter = NewParameter (contact, KP_ATTRIBUTE, "DAAC_ORDER_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to create DAAC_ORDER_ID.");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;

	buf[0] = '\0';

	if (result->order_id == 0)
	{
		strcpy (buf, "*** Order error. Contact ASF. ***");
	}
	else
	{
		sprintf (buf, "OrderID=%d", result->order_id);
	}
	value = ODLConvertString (buf, strlen(buf));
	NewValue (parameter, &value);
	
	/*
	** insert CONTACT_NAME
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"CONTACT_NAME"))
				== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to create new parameter for CONTACT_NAME");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->contact_name, 
	                          strlen (contact_info->contact_name) );
	NewValue (parameter, &value);


	/*
	** insert ORGANIZATION 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"ORGANIZATION"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to create new parameter for ORGANIZATION");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->organization, 
	                          strlen (contact_info->organization) );
	NewValue (parameter, &value);

	/*
	** insert ADDRESS
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"ADDRESS"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to create new parameter for ADDRESS");
		return (IMS_FATAL);
	}

	p = strtok(contact_info->street, ",");

	if (p)
	{
		parameter->value_kind = KV_SEQUENCE;

		buf[0]= '\0';
		(void)sprintf (buf, "%s", p);
		value = ODLConvertString (buf, strlen(buf)); 

		if (NewSqValue(parameter, &value)==NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_prodResult: failed to create new sequence value for ADDRESS");
			return (IMS_FATAL);
	
		}

		while ( p = strtok('\0', ",") )
		{
			buf[0]= '\0';
			(void)sprintf (buf, "%s", p);
			value = ODLConvertString (buf, strlen(buf)); 

			if (NewSqValue(parameter, &value)==NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_prodResult: failed to create new sequence value for ADDRESS");
				return (IMS_FATAL);
	
			}
		} 
	}

	/*
	** insert CITY 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"CITY"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to create new parameter for CITY");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->city, 
	                          strlen (contact_info->city) );
	NewValue (parameter, &value);

	/*
	** insert STATE 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"STATE"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to create new parameter for STATE");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->state, 
	                          strlen (contact_info->state) );
	NewValue (parameter, &value);

	/*
	** insert ZIP 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"ZIP"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to create new parameter for ZIP");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->zipcode, 
	                          strlen (contact_info->zipcode) );
	NewValue (parameter, &value);

	/*
	** insert COUNTRY 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"COUNTRY"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to create new parameter for COUNTRY");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->country, 
	                          strlen (contact_info->country) );
	NewValue (parameter, &value);

	/*
	** insert PHONE 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"PHONE"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to create new parameter for PHONE");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->phone, 
	                          strlen (contact_info->phone) );
	NewValue (parameter, &value);

	/*
	** insert FAX 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"FAX"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to create new parameter for FAX");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->fax, 
	                          strlen (contact_info->fax) );
	NewValue (parameter, &value);

	/*
	** insert EMAIL 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"EMAIL"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to create new parameter for EMAIL");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->email, 
	                          strlen (contact_info->email) );
	NewValue (parameter, &value);


	/* get MONITOR from RxTree, i.e. rTree, and insert it to pTree.
	** The IK routines will update it with new timestamps
	*/
	if ( (group = FindAggregate (rTree, "MONITOR")) == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to locate MONITOR.");
		return (IMS_FATAL);
	}
	if ( (group = CopyAggregate (group)) == (AGGREGATE)NULL  )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_prodResult: failed to copy MONITOR");
		return (IMS_FATAL);
	}
	PasteAggregate (pTree, group);


	return (IMS_OK);	
} /* end of v0_msgTree__create_prodResult */

/***********************************************************************
**
** v0_msgTree__create_darAcctResult -
**
** purpose: formats and returns result messages for DAR User Acct request 
**
** called by: v0_process
**
** return    IMS_OK  successful
**           IMS_FATAL system error
**
************************************************************************/
static int v0_msgTree__create_darAcctResult (IMS_MSG_STRUCT *msgDesc,
	AGGREGATE RxTree, AGGREGATE TxTree, char *statusCode, 
	V0_RESULT_STRUCT *result)
{
	V0_VALUE_LIST *darAcctList;
	AGGREGATE       group;    /* temp ptr to an aggregate */
	PARAMETER       parameter;/* temp ptr to parameters */
	AGGREGATE       rTree;    /* point to the input tree */
	AGGREGATE       dTree;    /* point USER_ACCT_RESULT node */
	AGGREGATE       aTree;    /* point to the ACCT node */
	VALUE_DATA      value;    /* point to values */
	V0_ERR_LIST     *he_ptr, *te_ptr;

	/* 
	** Initialization
	*/
	parameter  = (PARAMETER)NULL;
	group      = (AGGREGATE)NULL;
	dTree      = (AGGREGATE)NULL;
	darAcctList = result->user_list;

	/*
	** create the USER_ACCT_RESULT node under TxTree 
	*/
	if ((dTree = NewAggregate (TxTree, KA_GROUP, "USER_ACCT_RESULT", NULL )) 
			== (AGGREGATE)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		  "v0_msgTree__create_darAcctResult: failed to create USER_ACCT_RESULT aggregate.");
		return (IMS_FATAL);
	}

	/*
	** locate the top level aggregate in RxTree, i.e. USER_ACCT_SEARCH node
	*/
	if ( (rTree = (AGGREGATE) FindAggregate (RxTree, "USER_ACCT_SEARCH") ) == 
		(AGGREGATE) NULL )
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"v0_msgTree__create_darAcctResult: failed to locate top level of RxTree.");
		return (IMS_FATAL);
	}

	/* 
	** get value of msgId  and attach it to the output dTree 
	*/
	if ((parameter = FindParameter (rTree, "MESSAGE_ID")) ==
			(PARAMETER) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darAcctResult: missing parameter MESSAGE_ID");
		return (IMS_FATAL);
	}

	if ((parameter = CopyParameter (parameter)) == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darAcctResult: failed to copy MESSAGE_ID.");
		return (IMS_FATAL);
	}
	if ((parameter = PasteParameter (dTree, parameter)) ==
			(PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darAcctResult: PasteParameter failed.");
		return (IMS_FATAL);
	}

	/* 
	** insert to dTree the ASF data_center id 
	*/
 	parameter = NewParameter (dTree, KP_ATTRIBUTE, "DATA_CENTER_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			 "v0_msgTree__create_darAcctResult: failed to create DATA_CENTER_ID.");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("ASF", 3);
	NewValue (parameter, &value);


	/* 
	** get the value of odl_status from v0Desc and attach it to dTree as
	** status code
	*/
	parameter = NewParameter (dTree, KP_ATTRIBUTE, "STATUS_CODE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darAcctResult: failed to create STATUS_CODE.");
		return (IMS_FATAL);

	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertInteger (statusCode, strlen(statusCode));
	NewValue (parameter, &value);

	/* 
	** insert status_code_comment if available 
	*/
	if (result->odl_status_code_comment != (V0_ERR_LIST *)NULL)
	{
		if ((parameter=NewParameter(dTree, KP_ATTRIBUTE, "STATUS_CODE_COMMENT")) 
			       	== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_darAcctResult: failed to create new parameter for STATUS_CODE_COMMENT");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SEQUENCE;
		he_ptr = result->odl_status_code_comment;

		while (he_ptr != (V0_ERR_LIST *)NULL)
		{
			value =ODLConvertString(he_ptr->err_buf, strlen(he_ptr->err_buf));
			if (NewSqValue(parameter, &value)==NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_quit: failed to create new sequence value for STATUS_CODE_COMMENT");
				return (IMS_FATAL);
			}

			he_ptr = he_ptr->next_p;
		}

	}

	/* user_id */
	parameter = NewParameter (dTree, KP_ATTRIBUTE, "USER_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darAcctResult: failed to create USER_ID.");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (result->user_id, strlen(result->user_id));
	NewValue (parameter, &value);

	parameter = NewParameter (dTree, KP_ATTRIBUTE, "DIRECTORY");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darAcctResult: failed to create DIRECTORY.");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (result->directory, strlen(result->directory));
	NewValue (parameter, &value);

	/*
	** loop through the dataset_list and build a node for each dataset
	*/
	while (darAcctList != (V0_VALUE_LIST *)NULL )
	{
		if ( (aTree = NewAggregate (dTree, KA_GROUP, "ACCT", NULL) ) ==
			(AGGREGATE)NULL) 
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_darAcctResult: failed to create new aggregate for ACCT.");
			return (IMS_FATAL);

		}

		/*
		** insert BILLING_ID 
		*/
		if ( (parameter = NewParameter (aTree, KP_ATTRIBUTE,"BILLING_ID"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_darAcctResult: failed to create new parameter for BILLING_ID");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString (darAcctList->char_value1, 
				strlen (darAcctList->char_value1) );
		NewValue (parameter, &value);

		/*
		** insert SOURCE_NAME
		*/
		if ( (parameter = NewParameter (aTree, KP_ATTRIBUTE,"SOURCE_NAME"))
				== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_darAcctResult: failed to create new parameter for SOURCE_NAME");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SCALAR;
		value = ODLConvertString (darAcctList->char_value2, 
				strlen (darAcctList->char_value2) );
		NewValue (parameter, &value);

		darAcctList = darAcctList->next_p;
	}


	/* get MONITOR from RxTree, i.e. rTree, and insert it to dTree.
	** The IK routines will update it with new timestamps
	*/
	if ( (group = FindAggregate (rTree, "MONITOR")) == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darAcctResult: failed to locate MONITOR.");
		return (IMS_FATAL);
	}
	if ( (group = CopyAggregate (group)) == (AGGREGATE)NULL  )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darAcctResult: failed to copy MONITOR");
		return (IMS_FATAL);
	}
	PasteAggregate (dTree, group);

	return (IMS_OK);	

} /* end of v0_msgTree__create_darAcctResult */

/***********************************************************************
**
** v0_msgTree__create_darReqResult -
**
** purpose: formats and returns result messages for DAR request 
**
** called by: v0_process
**
** return    IMS_OK  successful
**           IMS_FATAL system error
**
************************************************************************/
static int v0_msgTree__create_darReqResult (IMS_MSG_STRUCT *msgDesc,
	AGGREGATE RxTree, AGGREGATE TxTree, char *statusCode, 
	V0_RESULT_STRUCT *result)
{
	AGGREGATE       group;    /* temp ptr to an aggregate */
	AGGREGATE       contact;  /* ptr to DAAC_CONTACT_ADDRESS */
	PARAMETER       parameter;/* temp ptr to parameters */
	AGGREGATE       rTree;    /* point to the input tree */
	AGGREGATE       dTree;    /* point DAR_RESULT node */
	VALUE_DATA      value;    /* point to values */
	V0_ERR_LIST     *he_ptr, *te_ptr;
	V0_CONTACT_INFO *contact_info;
	char            buf[IMS_COL30_LEN];     /* temp buffer for strings */
	char            *p;

	/* 
	** Initialization
	*/
	parameter  = (PARAMETER)NULL;
	group      = (AGGREGATE)NULL;
	dTree      = (AGGREGATE)NULL;
	contact_info = result->contact_info;

	/*
	** create the DAR_REQUEST_RESULT node under TxTree 
	*/
	if ((dTree = NewAggregate (TxTree, KA_GROUP, "DAR_REQUEST_RESULT", NULL )) 
			== (AGGREGATE)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		  "v0_msgTree__create_darReqResult: failed to create DAR_REQUEST_RESULT aggregate.");
		return (IMS_FATAL);
	}

	/*
	** locate the top level aggregate in RxTree, i.e. DAR_REQUEST node
	*/
	if ( (rTree = (AGGREGATE) FindAggregate (RxTree, "DAR_REQUEST") ) == 
		(AGGREGATE) NULL )
	{
		(void) ims_msg (msgDesc, IMS_INFO,
			"v0_msgTree__create_darReqResult: failed to locate top level of RxTree.");
		return (IMS_FATAL);
	}

	/* 
	** get value of msgId  and attach it to the output dTree 
	*/
	if ((parameter = FindParameter (rTree, "MESSAGE_ID")) ==
			(PARAMETER) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: missing parameter MESSAGE_ID");
		return (IMS_FATAL);
	}

	if ((parameter = CopyParameter (parameter)) == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to copy MESSAGE_ID.");
		return (IMS_FATAL);
	}
	if ((parameter = PasteParameter (dTree, parameter)) ==
			(PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: PasteParameter failed.");
		return (IMS_FATAL);
	}

	/* 
	** insert to dTree the ASF data_center id 
	*/
 	parameter = NewParameter (dTree, KP_ATTRIBUTE, "DATA_CENTER_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			 "v0_msgTree__create_darReqResult: failed to create DATA_CENTER_ID.");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("ASF", 3);
	NewValue (parameter, &value);


	/* 
	** get the value of odl_status from v0Desc and attach it to dTree as
	** status code
	*/
	parameter = NewParameter (dTree, KP_ATTRIBUTE, "STATUS_CODE");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to create STATUS_CODE.");
		return (IMS_FATAL);

	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertInteger (statusCode, strlen(statusCode));
	NewValue (parameter, &value);

	/* 
	** insert status_code_comment if available 
	*/
	if (result->odl_status_code_comment != (V0_ERR_LIST *)NULL)
	{
		if ((parameter=NewParameter(dTree, KP_ATTRIBUTE, "STATUS_CODE_COMMENT")) 
			       	== (PARAMETER)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_darReqResult: failed to create new parameter for STATUS_CODE_COMMENT");
			return (IMS_FATAL);
		}

		parameter->value_kind = KV_SEQUENCE;
		he_ptr = result->odl_status_code_comment;

		while (he_ptr != (V0_ERR_LIST *)NULL)
		{
			value =ODLConvertString(he_ptr->err_buf, strlen(he_ptr->err_buf));
			if (NewSqValue(parameter, &value)==NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_quit: failed to create new sequence value for STATUS_CODE_COMMENT");
				return (IMS_FATAL);
			}

			he_ptr = he_ptr->next_p;
		}

	}

	/* order_id */
	parameter = NewParameter (dTree, KP_ATTRIBUTE, "ORDER_ID");
	if (parameter == (PARAMETER)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to create ORDER_ID.");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	buf[0] = '\0';
	sprintf (buf, "%d", result->order_id);
	value = ODLConvertInteger (buf, strlen(buf));
	NewValue (parameter, &value);
	
	if ((contact = NewAggregate(dTree, KA_GROUP,"DAAC_CONTACT_ADDRESS", NULL) ) ==
		(AGGREGATE)NULL) 
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to create new aggregate for DAAC_CONTACT_ADDRESS.");
		return (IMS_FATAL);

	}

	/*
	** insert CONTACT_NAME
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"CONTACT_NAME"))
				== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to create new parameter for CONTACT_NAME");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->contact_name, 
	                          strlen (contact_info->contact_name) );
	NewValue (parameter, &value);


	/*
	** insert ORGANIZATION 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"ORGANIZATION"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to create new parameter for ORGANIZATION");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->organization, 
	                          strlen (contact_info->organization) );
	NewValue (parameter, &value);

	/*
	** insert ADDRESS
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"ADDRESS"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to create new parameter for ADDRESS");
		return (IMS_FATAL);
	}

	p = strtok(contact_info->street, ",");

	if (p)
	{
		parameter->value_kind = KV_SEQUENCE;

		buf[0]= '\0';
		(void)sprintf (buf, "%s", p);
		value = ODLConvertString (buf, strlen(buf)); 

		if (NewSqValue(parameter, &value)==NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_msgTree__create_prodResult: failed to create new sequence value for ADDRESS");
			return (IMS_FATAL);
	
		}

		while ( p = strtok('\0', ",") )
		{
			buf[0]= '\0';
			(void)sprintf (buf, "%s", p);
			value = ODLConvertString (buf, strlen(buf)); 

			if (NewSqValue(parameter, &value)==NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"v0_msgTree__create_prodResult: failed to create new sequence value for ADDRESS");
				return (IMS_FATAL);
	
			}
		} 
	}

	/*
	** insert CITY 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"CITY"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to create new parameter for CITY");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->city, 
	                          strlen (contact_info->city) );
	NewValue (parameter, &value);

	/*
	** insert STATE 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"STATE"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to create new parameter for STATE");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->state, 
	                          strlen (contact_info->state) );
	NewValue (parameter, &value);

	/*
	** insert ZIP 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"ZIP"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to create new parameter for ZIP");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->zipcode, 
	                          strlen (contact_info->zipcode) );
	NewValue (parameter, &value);

	/*
	** insert COUNTRY 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"COUNTRY"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to create new parameter for COUNTRY");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->country, 
	                          strlen (contact_info->country) );
	NewValue (parameter, &value);

	/*
	** insert PHONE 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"PHONE"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to create new parameter for PHONE");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->phone, 
	                          strlen (contact_info->phone) );
	NewValue (parameter, &value);

	/*
	** insert FAX 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"FAX"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to create new parameter for FAX");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->fax, 
	                          strlen (contact_info->fax) );
	NewValue (parameter, &value);

	/*
	** insert EMAIL 
	*/
	if ( (parameter = NewParameter (contact, KP_ATTRIBUTE,"EMAIL"))
			== (PARAMETER)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to create new parameter for EMAIL");
		return (IMS_FATAL);
	}

	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (contact_info->email, 
	                          strlen (contact_info->email) );
	NewValue (parameter, &value);


	/* get MONITOR from RxTree, i.e. rTree, and insert it to dTree.
	** The IK routines will update it with new timestamps
	*/
	if ( (group = FindAggregate (rTree, "MONITOR")) == (AGGREGATE)NULL )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to locate MONITOR.");
		return (IMS_FATAL);
	}
	if ( (group = CopyAggregate (group)) == (AGGREGATE)NULL  )
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_darReqResult: failed to copy MONITOR");
		return (IMS_FATAL);
	}
	PasteAggregate (dTree, group);

	return (IMS_OK);	
} /* end of v0_msgTree__cretate_darReqResult */

/*****************************************************************************
**
** tokenize_dataset_info - 
**
******************************************************************************/
static int tokenize_dataset_info (IMS_MSG_STRUCT *msgDesc,
																	V0_RESULT_STRUCT *result, 
																	IMS_DS_INFO_TYPE info_type)
{
	char *t;												/* points to the string been tokenized */
	int  init_flag;
	V0_VALUE_LIST *temp_ptr;
	char temp_word[IMS_COL255_LEN+1];
	int  temp_len;
	int  word_split;
	V0_VALUE_LIST *s_ptr, *s_list;
	char          s_word[IMS_COL255_LEN+1];
	int  count, t_count;
	int  s_init_flag;
	V0_VALUE_LIST *hv, *tv;

	init_flag = 0;

	switch (info_type)
	{
		case DS_INFO_COMMENT:
			t = strtok (result->dataset_comment, " ");
			break;
		case DS_INFO_RESTRICTION:
			t = strtok (result->dataset_restriction, " ");
			break;
	}

	while (t)
	{

		temp_word[0] = '\0';
		strcpy (temp_word, t);
		temp_len = 0;
		temp_len = strlen(temp_word);
		word_split = 0;
		s_list = (V0_VALUE_LIST *)NULL;

		/*
		** if a word is longer than the maximum length of a line, it is 
		** split until the remainder is shorter than a line
		*/
		if (temp_len > V0_MAX_COMMENT_LENGTH)
		{
			word_split = 1;
			t_count = 0;
			s_init_flag = 0;

			while (t_count < temp_len)
			{
				s_word[0] = '\0';

				for (count=0; 
						 (count < V0_MAX_COMMENT_LENGTH) && (t_count < temp_len); )
				{
					s_word[count++] = temp_word[t_count++];
				}
				s_word[count] = '\0';

				if (!s_init_flag)
				{
					if ((s_ptr = 
								(V0_VALUE_LIST *)malloc(sizeof(V0_VALUE_LIST))) == NULL)
					{
						(void) ims_msg (msgDesc, IMS_FATAL, 
				   		"tokenize_dataset_info: Memory allocation failed for s_ptr");
						return(IMS_FATAL);
					}

					s_init_flag = 1;
					strcpy (s_ptr->char_value1, s_word);
					s_ptr->char_value1[count] = '\0';
					s_ptr->int_value = count;
					s_ptr->next_p = (V0_VALUE_LIST *)NULL;
					s_list = s_ptr;
				}
				else
				{
					if ((s_ptr->next_p = 
						(V0_VALUE_LIST *)malloc(sizeof(V0_VALUE_LIST))) == NULL)
					{
						(void) ims_msg (msgDesc, IMS_FATAL, 
				   		"tokenize_dataset_info: Memory allocation failed for s_ptr->next_p");
						hv = s_list;
						while (hv != (V0_VALUE_LIST*)NULL)
						{
							tv = hv->next_p;
							free(hv);
							hv = tv;
						}

						return(IMS_FATAL);
					}
					s_ptr = s_ptr->next_p;

					strcpy (s_ptr->char_value1, s_word);
					s_ptr->char_value1[count] = '\0';
					s_ptr->int_value = count;
					s_ptr->next_p = (V0_VALUE_LIST*)NULL;
				}
			}
			
		}

		if (!init_flag)
		{
			if ((temp_ptr = (V0_VALUE_LIST *)malloc(sizeof(V0_VALUE_LIST))) == NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
				   "tokenize_dataset_info: Memory allocation failed for temp_ptr");

				hv = s_list;
				while (hv != (V0_VALUE_LIST*)NULL)
				{
					tv = hv->next_p;
					free(hv);
					hv = tv;
				}

				return(IMS_FATAL);
			}

			init_flag = 1;

			if (word_split)
			{
				hv = s_list;

				strcpy (temp_ptr->char_value3, hv->char_value1);
				temp_ptr->int_value = hv->int_value;
				temp_ptr->next_p = (V0_VALUE_LIST *)NULL;
				result->dataset_info_list1 = temp_ptr;
				hv = hv->next_p;

				while (hv != (V0_VALUE_LIST *)NULL)
				{
					if ((temp_ptr->next_p = 
								(V0_VALUE_LIST *)malloc(sizeof(V0_VALUE_LIST))) == NULL)
					{
						(void) ims_msg (msgDesc, IMS_FATAL, 
			  		"tokenize_dataset_info: Memory allocation failed for temp_ptr->next_p");
						hv = s_list;
						while (hv != (V0_VALUE_LIST*)NULL)
						{
							tv = hv->next_p;
							free(hv);
							hv = tv;
						}

						return(IMS_FATAL);
					}
			
					temp_ptr = temp_ptr->next_p;
					strcpy (temp_ptr->char_value3, hv->char_value1);
					temp_ptr->int_value = hv->int_value;
					temp_ptr->next_p = (V0_VALUE_LIST *)NULL;

					hv = hv->next_p;
				}

			}
			else
			{
				strcpy (temp_ptr->char_value3, temp_word);
				temp_ptr->char_value3[strlen(t)] = '\0';
				temp_ptr->int_value = temp_len;
				temp_ptr->next_p = (V0_VALUE_LIST *)NULL;
				result->dataset_info_list1 = temp_ptr;
			}
		}
		else
		{
			if ((temp_ptr->next_p = 
								(V0_VALUE_LIST *)malloc(sizeof(V0_VALUE_LIST))) == NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
			  "tokenize_dataset_info: Memory allocation failed for temp_ptr->next_p");

				hv = s_list;
				while (hv != (V0_VALUE_LIST*)NULL)
				{
					tv = hv->next_p;
					free(hv);
					hv = tv;
				}

				return(IMS_FATAL);
			}
			temp_ptr = temp_ptr->next_p;

			if (word_split)
			{
				hv = s_list;

				strcpy (temp_ptr->char_value3, hv->char_value1);
				temp_ptr->int_value = hv->int_value;
				temp_ptr->next_p = (V0_VALUE_LIST *)NULL;

				hv = hv->next_p;

				while (hv != (V0_VALUE_LIST *)NULL)
				{
					if ((temp_ptr->next_p = 
								(V0_VALUE_LIST *)malloc(sizeof(V0_VALUE_LIST))) == NULL)
					{
						(void) ims_msg (msgDesc, IMS_FATAL, 
			  		"tokenize_dataset_info: Memory allocation failed for temp_ptr->next_p");
						hv = s_list;
						while (hv != (V0_VALUE_LIST*)NULL)
						{
							tv = hv->next_p;
							free(hv);
							hv = tv;
						}

						return(IMS_FATAL);
					}
			
					temp_ptr = temp_ptr->next_p;
					strcpy (temp_ptr->char_value3, hv->char_value1);
					temp_ptr->int_value = hv->int_value;
					temp_ptr->next_p = (V0_VALUE_LIST *)NULL;

					hv = hv->next_p;
				}
			}
			else
			{
				strcpy (temp_ptr->char_value3, temp_word);
				temp_ptr->char_value3[strlen(t)] = '\0';
				temp_ptr->int_value = temp_len;
				temp_ptr->next_p = (V0_VALUE_LIST *)NULL;
			}
		}

		hv = s_list;
		while (hv != (V0_VALUE_LIST*)NULL)
		{
			tv = hv->next_p;
			free(hv);
			hv = tv;
		}

		t = strtok ('\0', " ");
	}


	return (IMS_OK);

} /* end of tokenize_dataset_info */

/*****************************************************************************
**
** format_dataset_info - 
**
******************************************************************************/

static int format_dataset_info (IMS_MSG_STRUCT *msgDesc, 
                                V0_RESULT_STRUCT *result)
{
	V0_VALUE_LIST *temp_ptr;      /* points to the original struct */
	V0_VALUE_LIST *curr_ptr;      /* points to the formated struct */
	int  curr_length;             /* number of characters has been inserted to
																	 the current line */
	int  format_started;          /* indicates the first line of formatted text*/
	char *t;                      /* points to the next insert point */	

	curr_length = 0;
	format_started = 0;
	temp_ptr = result->dataset_info_list1;

	while (temp_ptr != (V0_VALUE_LIST *)NULL)
	{
		/*
		** a new structure has to be allocated in one of the following conditions:
		** - begining of formatting
		** - the current word is the same length of the max line length 
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
				result->dataset_info_list2 = curr_ptr;
				
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

} /* end of format_dataset_info */

/*****************************************************************************
**
** create_empty_package -  
**
******************************************************************************/
static int create_empty_package (IMS_MSG_STRUCT *msgDesc, 
																 AGGREGATE *pTree,
																 char *dataset_id)
{
	AGGREGATE group;
	ATTRIBUTE parameter;
	VALUE_DATA value;
	char tmpStr[IMS_COL20_LEN+1];

	if ((*pTree = NewAggregate(NULL, KA_GROUP, "root", NULL)) == 
			(AGGREGATE)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_msgTree__create_empty_package:"
			" creating root aggregate failed.");
							
		return (IMS_FATAL);
	}

	if ((group=NewAggregate(*pTree, KA_GROUP, "PACKAGE", NULL))==(AGGREGATE)NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		  "v0_msgTree__create_empty_package: failed to create PACKAGE aggregate.");
		return (IMS_FATAL);
	}

	if ( (parameter = NewParameter 
		(group, KP_ATTRIBUTE, "DATA_CENTER_ID")) == (PARAMETER)NULL)
	{
		(void)ims_msg (msgDesc, IMS_FATAL, 
			"create_empty_package: Failed to initilize DATA_CENTER_ID parameter. ");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("ASF", 3);
	NewValue (parameter, &value);
		            
	if ( (parameter = NewParameter 
		(group, KP_ATTRIBUTE, "DATASET_ID")) == (PARAMETER)NULL)
	{
		(void)ims_msg (msgDesc, IMS_FATAL, 
			"create_empty_package: Failed to initilize DATASET_ID parameter. ");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString (dataset_id, strlen(dataset_id));
	NewValue (parameter, &value);
		            
	if ( (parameter = NewParameter 
		(group, KP_ATTRIBUTE, "PACKAGE_ID")) == (PARAMETER)NULL)
	{
		(void)ims_msg (msgDesc, IMS_FATAL, 
			"create_empty_package: Failed to initilize PACKAGE_ID parameter. ");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	value = ODLConvertString ("*", 1);
	NewValue (parameter, &value);
		            
	if ( (parameter = NewParameter 
		(group, KP_ATTRIBUTE, "COMMENT")) == (PARAMETER)NULL)
	{
		(void)ims_msg (msgDesc, IMS_FATAL, 
				"create_empty_package: Failed to initilize COMMENT parameter. ");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	tmpStr[0] = '\0';
	strcpy (tmpStr, "");
	value = ODLConvertString (tmpStr, strlen(tmpStr));
	NewValue (parameter, &value);
		            
	if ( (parameter = NewParameter 
		(group, KP_ATTRIBUTE, "INFO_PROMPT")) == (PARAMETER)NULL)
	{
		(void)ims_msg (msgDesc, IMS_FATAL, 
			"create_empty_package: Failed to initilize INFO_PROMPT parameter. ");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	tmpStr[0] = '\0';
	strcpy (tmpStr, "N/A");
	value = ODLConvertString (tmpStr, strlen(tmpStr));
	NewValue (parameter, &value);
		            
	if ((parameter=NewParameter
		(group, KP_ATTRIBUTE, "NUMBER_OF_GRANULES")) == (PARAMETER)NULL)
	{
		(void)ims_msg (msgDesc, IMS_FATAL, 
				"create_empty_package: Failed to initilize"
				" NUMBER_OF_GRANULES parameter. ");
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	tmpStr[0] = '\0';
	strcpy (tmpStr, "1");
	value = ODLConvertInteger (tmpStr, strlen(tmpStr));
	NewValue (parameter, &value);

	if ((parameter=NewParameter
			 (group, KP_ATTRIBUTE, "NUMBER_OF_OPTIONS"))==(PARAMETER)NULL)
	{
		(void)ims_msg (msgDesc, IMS_FATAL, 
			"create_empty_package: Failed to initilize"
			" NUMBER_OF_OPTIONS parameter. "); 
		return (IMS_FATAL);
	}
	parameter->value_kind = KV_SCALAR;
	tmpStr[0] = '\0';
	strcpy (tmpStr, "0");
	value = ODLConvertInteger (tmpStr, strlen(tmpStr));
	NewValue (parameter, &value);

	return (IMS_OK);
} /* end of create_empty_package */

/*****************************************************************************
**
** NewSqValue - Assign value and update counters in a SEQUENCE node.
**
******************************************************************************/

VALUE NewSqValue ( PARAMETER parameter, VALUE_DATA *value_data)
{
	VALUE val;       /* return value from NewValue */
					  
	val = NewValue(parameter, value_data);

	if (val != NULL && parameter->value_kind == KV_SEQUENCE)
	{
		parameter->rows = 1;
		parameter->columns = parameter->value_count;
	}

	return val;
}  /* end of NewSqValue */
