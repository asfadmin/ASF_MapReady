static char *sccs = "@(#)ims_op_accCat.c	5.6  09/15/97";
/*******************************************************************************
**
** File:	ims_op_accCat.c
**
** Function: 	Catalog database access facility for the ims_op_acc.
**
** Author:	Jennifer Ting, Armando Cardona
**
** Date: 	Feb 1995
**
** Modified:
**
**    11-10-95  A. Tilden  Modified get_account_list function to execute two
**                         separate queries for account and account_mgr 
**                         tables.
**                         Modified insert statements for account_user, and
**                         account_dataset tables to specify attributes.
**    11-24-95  A. Tilden  Modified functions get_assign_datasets,
**                         add_assign_datasets, delete_assign_datasets, and
**                         update_assign_datasets to maintain the
**                         op_account_dataset list with a unique dataset_idx, 
**                         in addition to the op_assigned_datasets list where
**                         the dataset_idx field is not unique.
**    12-07-95  A. Tilden  Modified functions get_assign_users,
**                         add_assign_users, delete_assign_users, and added
**                         get_all_users in order to maintain the
**                         op_account_user list.
**
**    04-22-95  J. Ting    Added IMS_SET_VERBOSE to echo out the complete
**											   sql statement in case of error.
**    10-07-96  J. Ting    R2.1 - Modified save_account_data function to 
**                         take out account manager query part.
**    10-14-96  J. Ting    R2.1 - Modified get_account_list function 
**                                for schema 3.5
**    10-14-96  J. Ting    R2.1 - Modified get_account_list function, 
**                         remove field quicklook_p.
**    11-05-96  J. Ting    R2.1 - change local_dir to ftp_dir.
**    11-11-96  J. Ting    R2.1 - added access_dir.
**    06-12-97  D. Ting    Added delete_account_user() & delete_account_dataset() 
**    07-23-97  D. Ting    Added delete_account_extension()
**	  07-30-97  D. Ting	   Added getManager() PR2441
** Notes:
**
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <Xm/Xm.h>
/* 
** #include <IK_Network.h>
** #include <IK_Syslog.h> 
** #include <odlinter.h>
*/
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_dbms.h>
#include <ims_qi.h>
#include <ims_cmnQuery.h>
#include <ims_util.h>
#include <ims_media.h>
#include <ims_opCat.h>
#include <ims_op.h>
/*
** Definition of local constants
*/
#define BUF_SIZE 1024   /* Maximum size of the cmd buffer */
#define EDEADLOCK 1205	/* Sybase DeadLock error number */

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in the modules where they
** are called. They are listed here for reference.
**
** int op_cat (OP_CAT_STRUCT *, OP_CAT_EVENT, char *);
*/

/*
** Local Functions
*/
static int useDatabase (OP_CAT_STRUCT *);
static int get_user_list (OP_CAT_STRUCT *);
static int get_account_list (OP_CAT_STRUCT *);
static int save_user_data (OP_CAT_STRUCT *);
static int save_account_data (OP_CAT_STRUCT *);
static int delete_user (OP_CAT_STRUCT *);
static int getManager (OP_CAT_STRUCT *);
static int delete_account (OP_CAT_STRUCT *);
static int delete_account_mgr (OP_CAT_STRUCT *);
static int getCatalogItems (OP_CAT_STRUCT *);/* To be done by Jenniffer's code*/
static int get_account_lock (OP_CAT_STRUCT *);
/*static int update_begin_current_balance (OP_CAT_STRUCT *);*/
static int get_all_users (OP_CAT_STRUCT *);
static int get_assign_users (OP_CAT_STRUCT *);
static int add_assign_users (OP_CAT_STRUCT *);
static int delete_assign_users (OP_CAT_STRUCT *);
static int get_all_datasets (OP_CAT_STRUCT *);
static int get_assign_datasets (OP_CAT_STRUCT *);
static int add_assign_datasets (OP_CAT_STRUCT *);
static int delete_assign_datasets (OP_CAT_STRUCT *);
static int update_assign_datasets (OP_CAT_STRUCT *);
static int beginTransaction (OP_CAT_STRUCT *);
static int commitTransaction (OP_CAT_STRUCT *);
static int rollbackTransaction (OP_CAT_STRUCT *);
static int execCmd (IMS_QI_DESC_OBJ *);
static int processRetStatus (IMS_QI_DESC_OBJ *);
static int delete_account_user (OP_CAT_STRUCT *);
static int delete_account_dataset (OP_CAT_STRUCT *);
static int delete_account_extension (OP_CAT_STRUCT *);

/*
** External Functions
*/

/*
** Global variables
*/
char cmdBuf[BUF_SIZE];
extern OP_GLOBAL_DATA glbData;

/******************************************************************************
**
** 
** Main function handling catalog queries.
**
******************************************************************************/

int ims_op_accCat (OP_CAT_STRUCT *catReq, OP_ACC_CAT_EVENT event)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;

	int status;

	msgDesc = catReq->msgDesc;

	/*
	** We must first make sure that we have a descriptor if the event is
	** anything BUT OPENCONNECTION.
	*/

	if (catReq->qDesc == (IMS_QI_DESC_OBJ *)NULL) 
	{
		if (event == OP_ACC_OPENCONNECTION)
		{
			if ((catReq->qDesc = ims_qiDescAlloc (msgDesc)) 
				== (IMS_QI_DESC_OBJ *)NULL)
			{
				ims_msg (msgDesc, IMS_FATAL,
					"Could not initialize query descriptor.");
				return (IMS_FATAL);
			}
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
			"First cat event must be OP_ACC_OPENCONNECTION.");
			return (IMS_FATAL);
		}
	}
	qDesc = catReq->qDesc;
	qDesc->cmd = cmdBuf;

	/*
	** Now, let's do our 'catalog' business according to the type of event
	** passed into the function.  
	*/
	switch (event)
	{
	case OP_ACC_OPENCONNECTION:
		/*
		** We also want to stay logged into the catalog until the 
		** OP_CLOSECONNECTION event is called.
		*/

		/*
		** Set the process' catalog login name
		*/
		IMS_SETUSER (qDesc, catReq->userSpec.dbUserName);

		/*
		** Set the process' catalog password
		*/
		IMS_SETPSWD (qDesc, catReq->userSpec.dbPassword);

		/*
		** Set the database name.
		*/
		if ((catReq->userSpec.dbName != (char *)NULL) &&
				(strlen (catReq->userSpec.dbName) != 0))
		{
			IMS_SETDBNAME (qDesc, catReq->userSpec.dbName);
		}

		/*
		** Set the program name 
		*/
		if ((catReq->userSpec.program != (char *)NULL) &&
				(strlen (catReq->userSpec.program) != 0))
		{
			IMS_SETPROG (qDesc, catReq->userSpec.program);
		}

		/*
		** Set the server name.
		*/
		if ((catReq->userSpec.server != (char *)NULL) &&
				(strlen (catReq->userSpec.server) != 0))
		{
			IMS_SETSERVER (qDesc, catReq->userSpec.server);
		}

		/*
		** 4/22/96 set VERBOSE so we can get the sql stmt in case of an error.
		*/
		IMS_SET_VERBOSE (qDesc, 10);

		/*
		** Do a login to the database.
		*/
		if ((status = ims_qiLogin (qDesc)) < IMS_OK)
		{
			return (status);
		}

		/*
		** Assign msgDesc to be the user defined data portion of the
		** dbproc structure.  This is needed by the message and
		** error handlers for SQLServer connections of the ims_op.
		*/
		(void) dbsetuserdata (qDesc->dbproc, (BYTE *)msgDesc);
		return (IMS_OK);

	case OP_ACC_USEDATABASE:
		status = useDatabase (catReq);
		break;

	case OP_ACC_GETUSERLIST:
		status = get_user_list (catReq);
		break;

	case OP_ACC_GETACCOUNTLIST:
		status = get_account_list (catReq);
		break;

	case OP_ACC_SAVEUSERDATA:
		status = save_user_data (catReq);
		break;

	case OP_ACC_SAVEACCDATA:
		status = save_account_data (catReq);
		break;

	case OP_ACC_DELETEUSER:
		status = delete_user (catReq);
		break;

  case OP_ACC_GETMANAGER:
		status = getManager (catReq);
		break;

	case OP_ACC_DELETEACCOUNT:
		status = delete_account (catReq);
		break;

        case OP_ACC_DELETEACCOUNTMGR:
		status = delete_account_mgr (catReq);
		break;

        case OP_ACC_DELETEACCOUNTUSER:
		status = delete_account_user (catReq);
		break;

        case OP_ACC_DELETEACCOUNTDATASET:
		status = delete_account_dataset (catReq);
		break;

        case OP_ACC_DELETEACCOUNTEXTENSION:
		status = delete_account_extension (catReq);
		break;

	case OP_ACC_GETACCOUNTLOCK:
		status = get_account_lock (catReq);
		break;

	/*case OP_UPDATE_BEGIN_CURRENT_BALANCE:
		status = update_begin_current_balance (catReq);
		break;*/

	case OP_ACC_GET_ALL_USERS:
		status = get_all_users (catReq);
		break;

	case OP_ACC_GET_ASSIGN_USERS:
		status = get_assign_users (catReq);
		break;

	case OP_ACC_ADD_ASSIGN_USERS:
		status = add_assign_users (catReq);
		break;

	case OP_ACC_DELETE_ASSIGN_USERS:
		status = delete_assign_users (catReq);
		break;

	case OP_ACC_GET_ALL_DATASETS:
		status = get_all_datasets (catReq);
		break;

	case OP_ACC_GET_ASSIGN_DATASETS:
		status = get_assign_datasets (catReq);
		break;

	case OP_ACC_ADD_ASSIGN_DATASETS:
		status = add_assign_datasets (catReq);
		break;

	case OP_ACC_DELETE_ASSIGN_DATASETS:
		status = delete_assign_datasets (catReq);
		break;

	case OP_ACC_UPDATE_ASSIGN_DATASETS:
		status = update_assign_datasets (catReq);
		break;

	case OP_ACC_GETCATALOGITEMS: /* done by Jeniffer's code */
		status = getCatalogItems (catReq);
		break;	
	
	case OP_ACC_BEGINTRANSACTION:
		status = beginTransaction (catReq);
		break;

	case OP_ACC_ROLLBACKTRANSACTION:
		status = rollbackTransaction (catReq);
		break;

	case OP_ACC_COMMITTRANSACTION:
		status = commitTransaction (catReq);
		break;

	case OP_ACC_CLOSECONNECTION:
		/*
		** Close the catalog connection.  
		*/
		status = ims_qiLogoff (qDesc);
		/* Free qDesc and associated allocated memory */
		ims_qiFreeDesc (qDesc);
		catReq->qDesc = (IMS_QI_DESC_OBJ *)NULL;
		return (status); 

	default:
		(void) ims_msg (msgDesc, IMS_FATAL,
		"op_acccat: Invalid catalog event passed to op_acccat.");
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
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"op_cat: Could not reinitialize query descriptor.");
			return(IMS_FATAL);
		}
	}

	/*
	** Return with the appropriate status
	*/
	if (status < IMS_OK) return (status);


	return (IMS_OK);
} /* op_cat */

/***********************************************************************
**
** useDatabase -  
**
***********************************************************************/
static int useDatabase (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_CAT_USERSPEC *userSpec;
	int status;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
	userSpec = &(catReq->userSpec);

	(void) sprintf (qDesc->cmd, "use %s", userSpec->dbName);

	if ((status = execCmd (qDesc)) < IMS_OK)
	{
		return (status);
	}
	return (IMS_OK);
}

/***********************************************************************
**
** get_user_list 
**
** Revisions: 11/11/96 - R2.1 - modified local_dir to ftp_dir
**                       R2.1 - added access_dir
**
***********************************************************************/
static int get_user_list (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_USER_PROFILE *currPtr, *prevPtr;
	OP_USER_PROFILE *firstPtr, *lastPtr;
	int status; 
	int rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd = (char *)catReq->item[1];

	prevPtr = currPtr = (OP_USER_PROFILE *)NULL;
	firstPtr = lastPtr = (OP_USER_PROFILE *)NULL;
	*(int *)catReq->item[0] = (int)0;
	catReq->item[2] = (void *)NULL;


	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
		    currPtr = firstPtr;
		    while (currPtr != (OP_USER_PROFILE *)NULL)
		    {
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		    }
		    return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_USER_PROFILE *) 
                 malloc (sizeof (OP_USER_PROFILE))) == (OP_USER_PROFILE *)NULL)
		{
		   ims_msg (msgDesc, IMS_FATAL, 
   "op_cat__get_user_list: Memory allocation for OP_USER_PROFILE failed.");
		   currPtr = firstPtr;
		   while (currPtr != (OP_USER_PROFILE *)NULL)
		   {
		       lastPtr = currPtr->next;
		       free (currPtr);
		       currPtr = lastPtr;
		   }
		   return (IMS_FATAL);
		}

		currPtr->next = (OP_USER_PROFILE *)NULL;
		currPtr->prev = (OP_USER_PROFILE *)NULL;
		currPtr->position = rowCount;
                currPtr->selected = 0 ;
		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)currPtr->user_id,
			qDesc->valAddr[0], qDesc->valLength[0]);
		currPtr->user_id[qDesc->valLength[0]] = '\0';
		ims_truncStr (currPtr->user_id);

		(void) memcpy ((char *)&(currPtr->user_type),
			qDesc->valAddr[1], qDesc->valLength[1]);

		(void) memcpy ((char *)currPtr->auth_key,
			qDesc->valAddr[2], qDesc->valLength[2]);
		currPtr->auth_key[qDesc->valLength[2]] = '\0';
		ims_truncStr (currPtr->auth_key);

		(void) memcpy ((char *)&(currPtr->priority),
			qDesc->valAddr[3], qDesc->valLength[3]);

		(void) memcpy ((char *)currPtr->first_name,
			qDesc->valAddr[4], qDesc->valLength[4]);
		currPtr->first_name[qDesc->valLength[4]] = '\0';
		ims_truncStr (currPtr->first_name);

		(void) memcpy ((char *)currPtr->initial_name,
			qDesc->valAddr[5], qDesc->valLength[5]);
		currPtr->initial_name[qDesc->valLength[5]] = '\0';
		ims_truncStr (currPtr->initial_name);

		(void) memcpy ((char *)currPtr->last_name,
			qDesc->valAddr[6], qDesc->valLength[6]);
		currPtr->last_name[qDesc->valLength[6]] = '\0';
		ims_truncStr (currPtr->last_name);

		(void) memcpy ((char *)currPtr->title,
			qDesc->valAddr[7], qDesc->valLength[7]);
		currPtr->title[qDesc->valLength[7]] = '\0';
		ims_truncStr (currPtr->title);

		(void) memcpy ((char *)currPtr->zipcode,
			qDesc->valAddr[8], qDesc->valLength[8]);
		currPtr->zipcode[qDesc->valLength[8]] = '\0';
		ims_truncStr (currPtr->zipcode);

		(void) memcpy ((char *)currPtr->phone,
			qDesc->valAddr[9], qDesc->valLength[9]);
		currPtr->phone[qDesc->valLength[9]] = '\0';
		ims_truncStr (currPtr->phone);

		(void) memcpy ((char *)currPtr->fax,
			qDesc->valAddr[10], qDesc->valLength[10]);
		currPtr->fax[qDesc->valLength[10]] = '\0';
		ims_truncStr (currPtr->fax);

		(void) memcpy ((char *)currPtr->organization,
			qDesc->valAddr[11], qDesc->valLength[11]);
		currPtr->organization[qDesc->valLength[11]] = '\0';
		ims_truncStr (currPtr->organization);

		(void) memcpy ((char *)currPtr->street,
			qDesc->valAddr[12], qDesc->valLength[12]);
		currPtr->street[qDesc->valLength[12]] = '\0';
		ims_truncStr (currPtr->street);

		(void) memcpy ((char *)currPtr->city,
			qDesc->valAddr[13], qDesc->valLength[13]);
		currPtr->city[qDesc->valLength[13]] = '\0';
		ims_truncStr (currPtr->city);

		(void) memcpy ((char *)currPtr->state,
			qDesc->valAddr[14], qDesc->valLength[14]);
		currPtr->state[qDesc->valLength[14]] = '\0';
		ims_truncStr (currPtr->state);

		(void) memcpy ((char *)currPtr->country,
			qDesc->valAddr[15], qDesc->valLength[15]);
		currPtr->country[qDesc->valLength[15]] = '\0';
		ims_truncStr (currPtr->country);

		(void) memcpy ((char *)currPtr->email,
			qDesc->valAddr[16], qDesc->valLength[16]);
		currPtr->email[qDesc->valLength[16]] = '\0';
		ims_truncStr (currPtr->email);

		(void) memcpy ((char *)currPtr->ftp_dir,
			qDesc->valAddr[17], qDesc->valLength[17]);
		currPtr->ftp_dir[qDesc->valLength[17]] = '\0';
		ims_truncStr (currPtr->ftp_dir);

		(void) memcpy ((char *)currPtr->access_dir,
			qDesc->valAddr[18], qDesc->valLength[18]);
		currPtr->access_dir[qDesc->valLength[18]] = '\0';
		ims_truncStr (currPtr->access_dir);

 
		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			currPtr->prev = lastPtr;
			lastPtr = currPtr;
		}

	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		ims_msg (msgDesc, IMS_FATAL,
	"op_cat_get_user_list: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (OP_USER_PROFILE *)NULL)
		{
			lastPtr = currPtr->next;
			free (currPtr);
			currPtr = lastPtr;
		}
		return (IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[0] = (int)rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* end of get_user_list */

/***********************************************************************
**
** get_account_list 
**
** Revision: 10/07/96 Took out account manager information.
**           10/14/96 Added mgr_user_id field.
**
***********************************************************************/
static int get_account_list (OP_CAT_STRUCT *catReq)
{
    IMS_QI_DESC_OBJ *qDesc;
    IMS_MSG_STRUCT *msgDesc;
    OP_ACCOUNT *currPtr, *prevPtr;
    OP_ACCOUNT *firstPtr, *lastPtr;
    int status; 
    int rowCount, i ;
    char create1[15], create2[15] ;
    char expire1[15], expire2[15] ;
    char  sqlBuf[IMS_COL1024_LEN*8+1];
    char *sqlPtr = (char *)NULL;
        
    qDesc = catReq->qDesc;
    msgDesc = catReq->msgDesc;

    /* account table query to be executed is input through catReq->item[1] */
    qDesc->cmd = (char *)catReq->item[1];

    prevPtr = currPtr = (OP_ACCOUNT *)NULL;
    firstPtr = lastPtr = (OP_ACCOUNT *)NULL;
    *(int *)catReq->item[0] = (int)0;
    catReq->item[3] = (void *)NULL;


    rowCount = 0;
    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
	if (status < IMS_OK) 
	{
	    printf ("account query failed at row: %d\n", rowCount);
	    currPtr = firstPtr;
	    while (currPtr != (OP_ACCOUNT *)NULL)
	    {
		lastPtr = currPtr->next;
		free (currPtr);
		currPtr = lastPtr;
	    }
	    return (status);
	}
	if (status == IMS_ENDOFQUERY) continue;

	if ((currPtr = (OP_ACCOUNT *) 
	     malloc (sizeof (OP_ACCOUNT))) == (OP_ACCOUNT *)NULL)
	{
	    ims_msg (msgDesc, IMS_FATAL, 
   "op_cat_get_account_list: Memory allocation for OP_ACCOUNT failed.");
	    currPtr = firstPtr;
	    while (currPtr != (OP_ACCOUNT *)NULL)
	    {
		lastPtr = currPtr->next;
		free (currPtr);
		currPtr = lastPtr;
	    }
	    return (IMS_FATAL);
	}

	currPtr->next = (OP_ACCOUNT *)NULL;
	currPtr->prev = (OP_ACCOUNT *)NULL;
	currPtr->position = rowCount;
	currPtr->selected = 0 ;

	/* a row is returned */
	rowCount += 1;

	/* copy in the returned data */
	(void) memcpy ((char *)currPtr->account_id,
		       qDesc->valAddr[0], qDesc->valLength[0]);
	currPtr->account_id[qDesc->valLength[0]] = '\0';
	ims_truncStr (currPtr->account_id);

	(void) memcpy ((char *)&(currPtr->account_type),
		       qDesc->valAddr[1], qDesc->valLength[1]);
	
	(void) memcpy ((char *)&(currPtr->resource_type),
		       qDesc->valAddr[2], qDesc->valLength[2]);

	(void) memcpy ((char *)&(currPtr->begin_balance),
		       qDesc->valAddr[3], qDesc->valLength[3]);
		
	(void) memcpy ((char *)&(currPtr->curr_balance),
		       qDesc->valAddr[4], qDesc->valLength[4]);
		
	(void) memcpy ((char *)&(currPtr->hold_balance),
		       qDesc->valAddr[5], qDesc->valLength[5]);

	(void) memcpy ((char *)create1,
		       qDesc->valAddr[6], qDesc->valLength[6]);
	create1[qDesc->valLength[6]] = '\0';
	ims_truncStr (create1);
	i = 0 ;
	while ( create1[i] )
	{
	    if ( create1[i++] == '/' )
		create1[i-1] = '-' ;
	}
	create1[i] = ' ' ; create1[i+1] = '\0' ;
	(void) memcpy ((char *)create2,
		       qDesc->valAddr[7], qDesc->valLength[7]);
	create2[qDesc->valLength[7]] = '\0';
	ims_truncStr (create2);
	currPtr->create_time[0] = '\0' ;
	strcat ( currPtr->create_time, create1);
	strcat ( currPtr->create_time, create2);

	(void) memcpy ((char *)expire1,
		       qDesc->valAddr[8], qDesc->valLength[8]);
	expire1[qDesc->valLength[8]] = '\0';
	ims_truncStr (expire1);
	i = 0 ;
	while ( expire1[i] )
	{
	    if ( expire1[i++] == '/' )
		expire1[i-1] = '-' ;
	}
	expire1[i] = ' ' ; expire1[i+1] = '\0' ;
	(void) memcpy ((char *)expire2,
		       qDesc->valAddr[9], qDesc->valLength[9]);
	expire2[qDesc->valLength[9]] = '\0';
	ims_truncStr (expire2);
	currPtr->expire_time[0] = '\0' ;
	strcat ( currPtr->expire_time, expire1);
	strcat ( currPtr->expire_time, expire2);

	(void) memcpy ((char *)(&currPtr->rate_multiplier),
		       qDesc->valAddr[10], qDesc->valLength[10]);

	(void) memcpy ((char *)currPtr->op_validate_p,
		       qDesc->valAddr[11], qDesc->valLength[11]);
	currPtr->op_validate_p[qDesc->valLength[11]] = '\0';
	ims_truncStr (currPtr->op_validate_p);

	/**************************************************
	(void) memcpy ((char *)currPtr->quicklook_p,
		       qDesc->valAddr[12], qDesc->valLength[12]);
	currPtr->quicklook_p[qDesc->valLength[12]] = '\0';
	ims_truncStr (currPtr->quicklook_p);
	**************************************************/

	(void) memcpy ((char *)currPtr->account_proc,
		       qDesc->valAddr[12], qDesc->valLength[12]);
	currPtr->account_proc[qDesc->valLength[12]] = '\0';
	ims_truncStr (currPtr->account_proc);

	(void) memcpy ((char *)currPtr->op_comment,
		       qDesc->valAddr[13], qDesc->valLength[13]);
	currPtr->op_comment[qDesc->valLength[13]] = '\0';
	ims_truncStr (currPtr->op_comment);

	(void) memcpy ((char *)currPtr->mgr_user_id,
		       qDesc->valAddr[14], qDesc->valLength[14]);
	currPtr->mgr_user_id[qDesc->valLength[14]] = '\0';
	ims_truncStr (currPtr->mgr_user_id);


	/* initialize account_mgr fields */
	/********************************************************
	(void) memcpy ((char *)currPtr->first_name,
		       "\0",
		       IMS_COL30_LEN+1);
	(void) memcpy ((char *)currPtr->initial_name,
		       "\0",
		       IMS_COL10_LEN+1);
	(void) memcpy ((char *)currPtr->last_name,
		       "\0",
		       IMS_COL30_LEN+1);
	(void) memcpy ((char *)currPtr->title,
		       "\0",
		       IMS_COL10_LEN+1);
	(void) memcpy ((char *)currPtr->organization,
		       "\0",
		       IMS_COL40_LEN+1);
	(void) memcpy ((char *)currPtr->street,
		       "\0",
		       IMS_COL128_LEN+1);
	(void) memcpy ((char *)currPtr->city,
		       "\0",
		       IMS_COL40_LEN+1);
	(void) memcpy ((char *)currPtr->state,
		       "\0",
		       IMS_COL30_LEN+1);
	(void) memcpy ((char *)currPtr->country,
		       "\0",
		       IMS_COL30_LEN+1);
	(void) memcpy ((char *)currPtr->zipcode,
		       "\0",
		       IMS_COL10_LEN+1);
	(void) memcpy ((char *)currPtr->phone,
		       "\0",
		       IMS_COL30_LEN+1);
	(void) memcpy ((char *)currPtr->fax,
		       "\0",
		       IMS_COL30_LEN+1);
	(void) memcpy ((char *)currPtr->email,
		       "\0",
		       IMS_COL128_LEN+1);
  *******************************************************/

	if (rowCount == 1)
	{
	    firstPtr = currPtr;
	    lastPtr = currPtr;
	}
	else
	{
	    lastPtr->next = currPtr;
	    currPtr->prev = lastPtr;
	    lastPtr = currPtr;
	}
    }

    /*
     ** Re-initialize query descriptor for next command, but do
     ** not cancel previous command
     */
    if (ims_qiResetDesc(qDesc) < IMS_OK)
    {
	ims_msg (msgDesc, IMS_FATAL,
	"op_cat_get_account_list: Could not reinitialize query descriptor.");
	
	currPtr = firstPtr;
	while (currPtr != (OP_ACCOUNT *)NULL)
	{
	    lastPtr = currPtr->next;
	    free (currPtr);
	    currPtr = lastPtr;
	}
	return(IMS_FATAL);
    }

 /************************************************************************
 ** retrieve account_mgr table data
 ** account_mgr table query to be executed is input through catReq->item[2]

    sqlPtr = (char *)catReq->item[2];
    strcpy (sqlBuf, sqlPtr);
    if ((sqlPtr = strchr (sqlBuf, '\0')) == (char *)NULL)
    {
	ims_msg (msgDesc, IMS_FATAL, 
		 "op_cat_get_account_list: can not execute account_mgr query.");

	currPtr = firstPtr;
	while (currPtr != (OP_ACCOUNT *)NULL)
	{
	    lastPtr = currPtr->next;
	    free (currPtr);
	    currPtr = lastPtr;
	}
	return (IMS_FATAL);
    }
    qDesc->cmd = (char *)sqlBuf;

    for (currPtr = firstPtr; 
	 (currPtr != (OP_ACCOUNT *)NULL); 
	 currPtr = currPtr->next)
    {
	sprintf ( sqlPtr, " '%s'", currPtr->account_id ) ;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
	    if (status < IMS_OK) 
	    {
		printf ("account_mgr query failed for account_id: %s\n", currPtr->account_id);

		currPtr = firstPtr;
		while (currPtr != (OP_ACCOUNT *)NULL)
		{
		    lastPtr = currPtr->next;
		    free (currPtr);
		    currPtr = lastPtr;
		}
		return (status);
	    }
	    if (status == IMS_ENDOFQUERY) continue;

	    (void) memcpy ((char *)currPtr->first_name,
			   qDesc->valAddr[0], qDesc->valLength[0]);
	    currPtr->first_name[qDesc->valLength[0]] = '\0';
	    ims_truncStr (currPtr->first_name);

	    (void) memcpy ((char *)currPtr->initial_name,
			   qDesc->valAddr[1], qDesc->valLength[1]);
	    currPtr->initial_name[qDesc->valLength[1]] = '\0';
	    ims_truncStr (currPtr->initial_name);

	    (void) memcpy ((char *)currPtr->last_name,
			   qDesc->valAddr[2], qDesc->valLength[2]);
	    currPtr->last_name[qDesc->valLength[2]] = '\0';
	    ims_truncStr (currPtr->last_name);

	    (void) memcpy ((char *)currPtr->title,
			   qDesc->valAddr[3], qDesc->valLength[3]);
	    currPtr->title[qDesc->valLength[3]] = '\0';
	    ims_truncStr (currPtr->title);
	  
	    (void) memcpy ((char *)currPtr->organization,
			   qDesc->valAddr[4], qDesc->valLength[4]);
	    currPtr->organization[qDesc->valLength[4]] = '\0';
	    ims_truncStr (currPtr->organization);

	    (void) memcpy ((char *)currPtr->street,
			   qDesc->valAddr[5], qDesc->valLength[5]);
	    currPtr->street[qDesc->valLength[5]] = '\0';
	    ims_truncStr (currPtr->street);

	    (void) memcpy ((char *)currPtr->city,
			   qDesc->valAddr[6], qDesc->valLength[6]);
	    currPtr->city[qDesc->valLength[6]] = '\0';
	    ims_truncStr (currPtr->city);
	    
	    (void) memcpy ((char *)currPtr->state,
			   qDesc->valAddr[7], qDesc->valLength[7]);
	    currPtr->state[qDesc->valLength[7]] = '\0';
	    ims_truncStr (currPtr->state);
	    
	    (void) memcpy ((char *)currPtr->country,
			   qDesc->valAddr[8], qDesc->valLength[8]);
	    currPtr->country[qDesc->valLength[8]] = '\0';
	    ims_truncStr (currPtr->country);
	    
	    (void) memcpy ((char *)currPtr->zipcode,
			   qDesc->valAddr[9], qDesc->valLength[9]);
	    currPtr->zipcode[qDesc->valLength[9]] = '\0';
	    ims_truncStr (currPtr->zipcode);
	    
	    (void) memcpy ((char *)currPtr->phone,
			   qDesc->valAddr[10], qDesc->valLength[10]);
	    currPtr->phone[qDesc->valLength[10]] = '\0';
	    ims_truncStr (currPtr->phone);
	    
	    (void) memcpy ((char *)currPtr->fax,
			   qDesc->valAddr[11], qDesc->valLength[11]);
	    currPtr->fax[qDesc->valLength[11]] = '\0';
	    ims_truncStr (currPtr->fax);
	    
	    (void) memcpy ((char *)currPtr->email,
			   qDesc->valAddr[12], qDesc->valLength[12]);
	    currPtr->email[qDesc->valLength[12]] = '\0';
	    ims_truncStr (currPtr->email);
	}  ** end of while (status) **
    }  ** end of for (currPtr) **
*******************************************************************/

  /*
  ** Re-initialize query descriptor for next command, but do
  ** not cancel previous command
  */
  if (ims_qiResetDesc(qDesc) < IMS_OK)
  {
		ims_msg (msgDesc, IMS_FATAL,
		"op_cat_get_account_list: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (OP_ACCOUNT *)NULL)
		{
	    lastPtr = currPtr->next;
	    free (currPtr);
	    currPtr = lastPtr;
		}
		return(IMS_FATAL);
  }

  /* Return result counter and list to the calling routine */
  *(int *)catReq->item[0] = (int)rowCount;
  catReq->item[3] = (void *)firstPtr;
    
  return(IMS_OK);

} /* end of get_account_list */


static int save_user_data (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd = (char *)catReq->item[1];

	catReq->item[2] = (void *)NULL;

        /* execute query */
        if ( ( status = execCmd (qDesc) ) < IMS_OK ) 
	{
	    return ( status );
	}

	if (IMS_AFFECTED (qDesc) < 1)
	{
	    return(IMS_FATAL);
	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		ims_msg (msgDesc, IMS_FATAL,
	"op_cat_save_user_data: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	return(IMS_OK);
}/* end of save_user_data */


/***************************************************************************
**
** Function Name :	save_account_data
**
** Revision: 10/07/96 - Modified to take out account manager query Struct
**
****************************************************************************/
static int save_account_data (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

/* query to be executed is input through catReq->item[1] and catReq->item[2] */
	qDesc->cmd = (char *)catReq->item[1];

        /* execute query for the account table */
        if ( ( status = execCmd (qDesc) ) < IMS_OK ) 
	{
	    return ( status );
	}

	if (IMS_AFFECTED (qDesc) < 1)
	{
	    return(IMS_FATAL);
	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		ims_msg (msgDesc, IMS_FATAL,
	"op_cat_save_account_data: Could not reinitialize query descriptor.");

	
		return(IMS_FATAL);
	}

	/*****
	qDesc->cmd = (char *)catReq->item[2];

        ** execute query for the account_mgr table **
        if ( ( status = execCmd (qDesc) ) < IMS_OK ) 
          return ( status );

	if (IMS_AFFECTED (qDesc) < 1)
	{
	    return(IMS_FATAL);
	}
	*****/

	return(IMS_OK);
}/* end of save_account_data */

static int delete_user (OP_CAT_STRUCT *catReq)
{
  IMS_QI_DESC_OBJ *qDesc;
  IMS_MSG_STRUCT *msgDesc;
  int status; 
  
  status = IMS_OK;
  qDesc = catReq->qDesc;
  msgDesc = catReq->msgDesc;
  
  /* query to be executed is input through catReq->item[1] */
  qDesc->cmd = (char *)catReq->item[1];
  catReq->item[2] = (void *)NULL;

  /* execute query */
  status = execCmd (qDesc);

  if (status >= IMS_OK)
  {
      if (IMS_AFFECTED (qDesc) < 1)
	  status = IMS_NOROWS;
  }

  /*
   ** Re-initialize query descriptor for next command, but do
   ** not cancel previous command
   */
  if (ims_qiResetDesc(qDesc) < IMS_OK)
  {
    ims_msg (msgDesc, IMS_FATAL,
	"op_cat_delete_user: Could not reinitialize query descriptor.");
	
    status = IMS_FATAL;
  }

  return (status);

}/* end of delete_user */

/***********************************************************************
**
** getManager - 	
**
***********************************************************************/
static int getManager (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	int rowCount;


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd = (char *)catReq->item[1];


	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* a row is returend */
		rowCount += 1;
	}

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
	}

  *(int *)catReq->item[0] = (int)rowCount;
	return(IMS_OK);

} /* getManager */

static int delete_account (OP_CAT_STRUCT *catReq)
{
  IMS_QI_DESC_OBJ *qDesc;
  IMS_MSG_STRUCT *msgDesc;
  int status; 

  status = IMS_OK;
  qDesc = catReq->qDesc;
  msgDesc = catReq->msgDesc;

  /* query to be executed is input through catReq->item[1] */
  qDesc->cmd = (char *)catReq->item[1];
  catReq->item[2] = (void *)NULL;

  /* execute query */
  status = execCmd (qDesc);

  if (status >= IMS_OK)
  {
      if (IMS_AFFECTED (qDesc) < 1)
	  status = IMS_NOROWS;
  }

  /*
   ** Re-initialize query descriptor for next command, but do
   ** not cancel previous command
   */
  if (ims_qiResetDesc(qDesc) < IMS_OK)
  {
    ims_msg (msgDesc, IMS_FATAL,
	"op_cat_delete_account: Could not reinitialize query descriptor.");
	
    status = IMS_FATAL;
  }

  return (status);

}/* end of delete_account */

static int delete_account_mgr (OP_CAT_STRUCT *catReq)
{
  IMS_QI_DESC_OBJ *qDesc;
  IMS_MSG_STRUCT *msgDesc;
  int status; 

  qDesc = catReq->qDesc;
  msgDesc = catReq->msgDesc;

  /* query to be executed is input through catReq->item[1] */
  qDesc->cmd = (char *)catReq->item[1];

  catReq->item[2] = (void *)NULL;

  /* execute query */
  if ( ( status = execCmd (qDesc) ) < IMS_OK ) 
    return ( status );

  /*
   ** Re-initialize query descriptor for next command, but do
   ** not cancel previous command
   */
  if (ims_qiResetDesc(qDesc) < IMS_OK)
  {
    ims_msg (msgDesc, IMS_FATAL,
	"op_cat_delete_account_mgr: Could not reinitialize query descriptor.");
	
    return(IMS_FATAL);
  }

  return(IMS_OK);

}/* end of delete_account_mgr */

static int delete_account_user (OP_CAT_STRUCT *catReq)
{
  IMS_QI_DESC_OBJ *qDesc;
  IMS_MSG_STRUCT *msgDesc;
  int status; 

  qDesc = catReq->qDesc;
  msgDesc = catReq->msgDesc;

  /* query to be executed is input through catReq->item[1] */
  qDesc->cmd = (char *)catReq->item[1];

  catReq->item[2] = (void *)NULL;

  /* execute query */
  if ( ( status = execCmd (qDesc) ) < IMS_OK ) 
    return ( status );

  /*
   ** Re-initialize query descriptor for next command, but do
   ** not cancel previous command
   */
  if (ims_qiResetDesc(qDesc) < IMS_OK)
  {
    ims_msg (msgDesc, IMS_FATAL,
	"op_cat_delete_account_user: Could not reinitialize query descriptor.");
	
    return(IMS_FATAL);
  }

  return(IMS_OK);

}/* end of delete_account_user */

static int delete_account_dataset (OP_CAT_STRUCT *catReq)
{
  IMS_QI_DESC_OBJ *qDesc;
  IMS_MSG_STRUCT *msgDesc;
  int status; 

  qDesc = catReq->qDesc;
  msgDesc = catReq->msgDesc;

  /* query to be executed is input through catReq->item[1] */
  qDesc->cmd = (char *)catReq->item[1];

  catReq->item[2] = (void *)NULL;

  /* execute query */
  if ( ( status = execCmd (qDesc) ) < IMS_OK ) 
    return ( status );

  /*
   ** Re-initialize query descriptor for next command, but do
   ** not cancel previous command
   */
  if (ims_qiResetDesc(qDesc) < IMS_OK)
  {
    ims_msg (msgDesc, IMS_FATAL,
	"op_cat_delete_account_dataset: Could not reinitialize query descriptor.");
	
    return(IMS_FATAL);
  }

  return(IMS_OK);

}/* end of delete_account_dataset */

static int delete_account_extension (OP_CAT_STRUCT *catReq)
{
  IMS_QI_DESC_OBJ *qDesc;
  IMS_MSG_STRUCT *msgDesc;
  int status; 

  qDesc = catReq->qDesc;
  msgDesc = catReq->msgDesc;

  /* query to be executed is input through catReq->item[1] */
  qDesc->cmd = (char *)catReq->item[1];

  catReq->item[2] = (void *)NULL;

  /* execute query */
  if ( ( status = execCmd (qDesc) ) < IMS_OK ) 
    return ( status );

  /*
   ** Re-initialize query descriptor for next command, but do
   ** not cancel previous command
   */
  if (ims_qiResetDesc(qDesc) < IMS_OK)
  {
    ims_msg (msgDesc, IMS_FATAL,
	"op_cat_delete_account_extension: Could not reinitialize query descriptor.");
	
    return(IMS_FATAL);
  }

  return(IMS_OK);

}/* end of delete_account_extension */




/***********************************************************************
**
** getCatalogItems - 	
**
***********************************************************************/
static int getCatalogItems (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int status; 
	int rowCount;


	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/*
	** Get user_type valids
	*/
	strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'user_type' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.user_type[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.user_type[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
	        glbData.user_type[rowCount].item_name[qDesc->valLength[1]]
                                                                     = '\0';
		ims_truncStr (glbData.user_type[rowCount].item_name);
		ims_toUpper (glbData.user_type[rowCount].item_name);

		/* a row is returend */
		rowCount += 1;
	}
	glbData.user_type_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
	}


	/*
	** Get priority valids
	*/
	strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'priority' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.priority[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.priority[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.priority[rowCount].item_name[qDesc->valLength[1]]
                                                                    = '\0';
		ims_truncStr (glbData.priority[rowCount].item_name);
		ims_toUpper (glbData.priority[rowCount].item_name);

		/* a row is returend */

		rowCount += 1;
	}
	glbData.priority_count = rowCount;

	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
	}


	/*
	** Get account_type valids
	*/
	strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'account_type' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.account_type[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.account_type[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.account_type[rowCount].item_name[qDesc->valLength[1]]
                                                    = '\0';
		ims_truncStr (glbData.account_type[rowCount].item_name);
		ims_toUpper (glbData.account_type[rowCount].item_name);

		/* a row is returend */
		rowCount += 1;
	}
	glbData.account_type_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
	}
	/*
	** Get resource_type valids
	*/
	strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'resource_type' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(glbData.resource_type[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((glbData.resource_type[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		glbData.resource_type[rowCount].item_name[qDesc->valLength[1]]
                                                    = '\0';
		ims_truncStr (glbData.resource_type[rowCount].item_name);
		ims_toUpper (glbData.resource_type[rowCount].item_name);

		/* a row is returend */
		rowCount += 1;
	}
	glbData.resource_type_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		ims_msg (msgDesc, IMS_FATAL,
			"op_cat_getCatalogItems: Could not reinitialize query descriptor.");
	}


	return(IMS_OK);

} /* getCatalogItems */




/****************************************************************************
**
** beginTransaction ()
**
** Open a transaction.
**
****************************************************************************/
static int beginTransaction (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	(void) sprintf (qDesc->cmd, "begin transaction");

	if (execCmd (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"op_cat__beginTransaction:"
                       " Could not begin transaction.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}





/******************************************************************************
**
** update_begin_current_balance ()
**
** Execute stored procedure update_begin_current_balance to update table account
**
******************************************************************************/

/***static int update_begin_current_balance (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
        float number ;
        char * acc_id ;
        IMS_ACCT_TRANS_TYPE type ;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;


	acc_id = (char *)catReq->item[1];
        number = *((float *)catReq->item[2]);
        if ( number < 0 ){
          number *= -1.0 ; 
          type = DEBIT ;
        }
        else
          type = CREDIT ;
        
        return ( ims_acctTran ( qDesc, msgDesc, acc_id, 
                                                  -1, number, type ) );
}
***/
/******************************************************************************
**
** get_account_lock ()
**
** Execute stored procedure get_account_lock to update table account
**
******************************************************************************/

static int get_account_lock (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/*
	** Execute stored procedure get_account_lock
	*/
	(void) sprintf (qDesc->cmd, "get_account_lock");

	if (execCmd(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
		"execution of stored procedure get_account_lock failed.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}


/******************************************************************************
**
** commitTransaction ()
**
** Commit the transaction opened by openTransaction.
**
******************************************************************************/

static int commitTransaction (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	(void) sprintf (qDesc->cmd, "commit transaction");

	if (execCmd(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not commit transaction.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}

/******************************************************************************
**
** rollbackTransaction ()
**
** Rollback the transaction opened by openTransaction.
**
******************************************************************************/

static int rollbackTransaction (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/*
	** Rollback this transaction.
	*/
	(void) sprintf (qDesc->cmd, "rollback transaction");

	if (execCmd(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not rollback transaction.");
		return(IMS_FATAL);
	}
	return(IMS_OK);
}

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
** ROWS FROM THE DATABASE OR ONLY ONE ROW IS EXPECTED.
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
}

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


/***********************************************************************
**
** get_all_users 
**
***********************************************************************/
static int get_all_users (OP_CAT_STRUCT *catReq)
{
    IMS_QI_DESC_OBJ  *qDesc;
    IMS_MSG_STRUCT   *msgDesc;
    OP_ASSIGN_USERS  *currPtr, *prevPtr;
    OP_ASSIGN_USERS  *firstPtr, *lastPtr;
    int               status; 
    int               rowCount;

    qDesc = catReq->qDesc;
    msgDesc = catReq->msgDesc;

    /* query to be executed is input through catReq->item[1] */
    qDesc->cmd = (char *)catReq->item[1];

    prevPtr = currPtr = (OP_ASSIGN_USERS *)NULL;
    firstPtr = lastPtr = (OP_ASSIGN_USERS *)NULL;
    *(int *)catReq->item[0] = (int)0;
    catReq->item[2] = (void *)NULL;

    rowCount = 0;
    while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
	if (status < IMS_OK) 
	{
	    return (status);
	}
	if (status == IMS_ENDOFQUERY) continue;

	if ((currPtr = (OP_ASSIGN_USERS *) 
	     malloc (sizeof (OP_ASSIGN_USERS))) == (OP_ASSIGN_USERS *)NULL)
	{
	    ims_msg (msgDesc, IMS_FATAL, 
   "op_cat_get_all_users: Memory allocation for OP_ASSIGN_USERS failed.");
	    return (IMS_FATAL);
	}

	currPtr->next = (OP_ASSIGN_USERS *)NULL;
	currPtr->prev = (OP_ASSIGN_USERS  *)NULL;
	currPtr->account_user_ptr = (OP_ACCOUNT_USER *)NULL;
	currPtr->position = rowCount;
	currPtr->selected = 0;
	
	/* a row is returned */
	rowCount += 1;

	/* copy in the returned data */
	(void) memcpy ((char *)currPtr->user_id,
		       qDesc->valAddr[0], qDesc->valLength[0]);
	currPtr->user_id[qDesc->valLength[0]] = '\0';
	ims_truncStr (currPtr->user_id);

	(void) memcpy ((char *)currPtr->first_name,
		       qDesc->valAddr[1], qDesc->valLength[1]);
	currPtr->first_name[qDesc->valLength[1]] = '\0';
	ims_truncStr (currPtr->first_name);
    
	(void) memcpy ((char *)currPtr->initial_name,
		       qDesc->valAddr[2], qDesc->valLength[2]);
	currPtr->initial_name[qDesc->valLength[2]] = '\0';
	ims_truncStr (currPtr->initial_name);
    
	(void) memcpy ((char *)currPtr->last_name,
		       qDesc->valAddr[3], qDesc->valLength[3]);
	currPtr->last_name[qDesc->valLength[3]] = '\0';
	ims_truncStr (currPtr->last_name);
 
	if (rowCount == 1)
	{
	    firstPtr = currPtr;
	    lastPtr = currPtr;
	}
	else
	{
	    lastPtr->next = currPtr;
	    currPtr->prev = lastPtr;
	    lastPtr = currPtr;
	}
    
    } /* end of while (ims_qiNextRow) */

    /* Return result counter and list to the calling routine */
    *(int *)catReq->item[0] = (int)rowCount;
    catReq->item[2] = (void *)firstPtr;

    return(IMS_OK);
    
} /* end of get_all_users */


/***********************************************************************
**
** get_assign_users 
**
***********************************************************************/
static int get_assign_users (OP_CAT_STRUCT *catReq)
{
  IMS_QI_DESC_OBJ *qDesc;
  IMS_MSG_STRUCT *msgDesc;
  OP_ASSIGN_USERS *currPtr, *prevPtr;
  OP_ASSIGN_USERS *firstPtr, *lastPtr;
  OP_ACCOUNT_USER *first_accusrPtr, *curr_accusrPtr, *new_accusrPtr;
  int status; 
  int rowCount;

  qDesc = catReq->qDesc;
  msgDesc = catReq->msgDesc;

  /* query to be executed is input through catReq->item[1] */
  qDesc->cmd = (char *)catReq->item[1];

  prevPtr = currPtr = (OP_ASSIGN_USERS *)NULL;
  firstPtr = lastPtr = (OP_ASSIGN_USERS *)NULL;
  first_accusrPtr = (OP_ACCOUNT_USER *)NULL;
  curr_accusrPtr = (OP_ACCOUNT_USER *)NULL;
  new_accusrPtr = (OP_ACCOUNT_USER *)NULL;
  *(int *)catReq->item[0] = (int)0;
  catReq->item[2] = (void *)NULL;
  catReq->item[3] = (void *)NULL;

  rowCount = 0;
  while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
  {
    if (status < IMS_OK) 
    {
	currPtr = firstPtr;
	while (currPtr != (OP_ASSIGN_USERS *)NULL)
	{
	    lastPtr = currPtr->next;
	    free (currPtr);
	    currPtr = lastPtr;
	}
	curr_accusrPtr = first_accusrPtr;
	while (curr_accusrPtr != (OP_ACCOUNT_USER *)NULL)
	{
	    new_accusrPtr = curr_accusrPtr->next;
	    free (curr_accusrPtr);
	    curr_accusrPtr = new_accusrPtr;
	}
	return (status);
    }
    if (status == IMS_ENDOFQUERY) continue;

    if ((currPtr = (OP_ASSIGN_USERS *) 
	 malloc (sizeof (OP_ASSIGN_USERS))) == (OP_ASSIGN_USERS *)NULL)
    {
	ims_msg (msgDesc, IMS_FATAL, 
   "op_cat_get_assign_users: Memory allocation for OP_ASSIGN_USERS failed.");
	currPtr = firstPtr;
	while (currPtr != (OP_ASSIGN_USERS *)NULL)
	{
	    lastPtr = currPtr->next;
	    free (currPtr);
	    currPtr = lastPtr;
	}
	curr_accusrPtr = first_accusrPtr;
	while (curr_accusrPtr != (OP_ACCOUNT_USER *)NULL)
	{
	    new_accusrPtr = curr_accusrPtr->next;
	    free (curr_accusrPtr);
	    curr_accusrPtr = new_accusrPtr;
	}
	return (IMS_FATAL);
    }

    currPtr->next = (OP_ASSIGN_USERS *)NULL;
    currPtr->prev = (OP_ASSIGN_USERS  *)NULL;
    currPtr->account_user_ptr = (OP_ACCOUNT_USER *)NULL;
    currPtr->position = rowCount;
    currPtr->selected = 0;
	
    /* a row is returned */
    rowCount += 1;

    /* copy in the returned data */
    (void) memcpy ((char *)currPtr->user_id,
		   qDesc->valAddr[0], qDesc->valLength[0]);
    currPtr->user_id[qDesc->valLength[0]] = '\0';
    ims_truncStr (currPtr->user_id);

    (void) memcpy ((char *)currPtr->first_name,
		   qDesc->valAddr[1], qDesc->valLength[1]);
    currPtr->first_name[qDesc->valLength[1]] = '\0';
    ims_truncStr (currPtr->first_name);
    
    (void) memcpy ((char *)currPtr->initial_name,
		   qDesc->valAddr[2], qDesc->valLength[2]);
    currPtr->initial_name[qDesc->valLength[2]] = '\0';
    ims_truncStr (currPtr->initial_name);
    
    (void) memcpy ((char *)currPtr->last_name,
		   qDesc->valAddr[3], qDesc->valLength[3]);
    currPtr->last_name[qDesc->valLength[3]] = '\0';
    ims_truncStr (currPtr->last_name);
 
    if (rowCount == 1)
    {
      firstPtr = currPtr;
      lastPtr = currPtr;
    }
    else
    {
      lastPtr->next = currPtr;
      currPtr->prev = lastPtr;
      lastPtr = currPtr;
    }

    curr_accusrPtr = first_accusrPtr;
    if (curr_accusrPtr != (OP_ACCOUNT_USER *)NULL)
    {
      /* find the right location in the account_user list */
      while ((curr_accusrPtr->next != (OP_ACCOUNT_USER *)NULL)
	     && (strcmp (currPtr->user_id, curr_accusrPtr->user_id) > 0))
      {
	curr_accusrPtr = curr_accusrPtr->next;
      }

      if (strcmp (curr_accusrPtr->user_id, currPtr->user_id) == 0)
      {
	/* user info already exists in the list, just initialize pointer */
	currPtr->account_user_ptr = curr_accusrPtr;
	continue;
      }
    }

    /* add a new element to the account_user list */
    if ((new_accusrPtr =
	 (OP_ACCOUNT_USER *)malloc (sizeof(OP_ACCOUNT_USER)))
	== (OP_ACCOUNT_USER *)NULL)
    {
	(void) ims_msg (msgDesc, IMS_FATAL,
		      "Memory allocation failed.");
	currPtr = firstPtr;
	while (currPtr != (OP_ASSIGN_USERS *)NULL)
	{
	    lastPtr = currPtr->next;
	    free (currPtr);
	    currPtr = lastPtr;
	}
	curr_accusrPtr = first_accusrPtr;
	while (curr_accusrPtr != (OP_ACCOUNT_USER *)NULL)
	{
	    new_accusrPtr = curr_accusrPtr->next;
	    free (curr_accusrPtr);
	    curr_accusrPtr = new_accusrPtr;
	}
	return (IMS_FATAL);
    }
    else
    {
      new_accusrPtr->prev = (OP_ACCOUNT_USER *)NULL;
      new_accusrPtr->next = (OP_ACCOUNT_USER *)NULL;
      strcpy (new_accusrPtr->user_id, currPtr->user_id);
      new_accusrPtr->account_user_status = UNCHANGED;
      currPtr->account_user_ptr = new_accusrPtr;
    }

    if (curr_accusrPtr == (OP_ACCOUNT_USER *)NULL)
    {
      first_accusrPtr = new_accusrPtr;
    }
    else
    {
      /* 
       * The users are retrieved from the catalog ordered by 
       * user_id, append new element at the end of the list.
       */
      curr_accusrPtr->next = new_accusrPtr;
      new_accusrPtr->prev = curr_accusrPtr;
    }
    
  } /* end of while (ims_qiNextRow) */

  /* Return result counter and list to the calling routine */
  *(int *)catReq->item[0] = (int)rowCount;
  catReq->item[2] = (void *)firstPtr;
  catReq->item[3] = (void *)first_accusrPtr;

  return(IMS_OK);

} /* end of get_assign_users */

/******************************************************************************
**
** add_assign_users ()
**
** Adds the specified users to the specified account
**
******************************************************************************/

static int add_assign_users (OP_CAT_STRUCT *catReq)
{
        IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ACCOUNT_USER *account_user_ptr ;
	char *acc_id ;
	char  attributes[IMS_COL255_LEN+1] ;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
        account_user_ptr = (OP_ACCOUNT_USER *) catReq->item[1] ;
	acc_id = (char *)catReq->item[2] ;
	strcpy ( attributes, "(account_id, user_id)" ) ;

        while (account_user_ptr != (OP_ACCOUNT_USER *) NULL) 
	{
	    sprintf (qDesc->cmd, 
                     "insert into account_user %s values ('%s','%s')",
                     attributes, 
		     acc_id, 
		     account_user_ptr->user_id);

	    if (execCmd(qDesc) < IMS_OK) 
	    {
		(void) ims_msg (msgDesc, IMS_FATAL,
				"execution of add_assign_users failed.");
		return(IMS_FATAL);
	    }

	    /* 
	     * Re-initialize query descriptor for next command, 
	     * but do not cancel previous command 
	     */
	    if (ims_qiResetDesc(qDesc) < IMS_OK)
	    {
		(void) ims_msg (msgDesc, IMS_FATAL,
		"add_assign_users: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	    }
	
	  account_user_ptr = account_user_ptr->next ;

        } /* end of while */

	return(IMS_OK);
}

/******************************************************************************
**
** delete_assign_users ()
**
** Deletes the specified users to the specified account
**
******************************************************************************/

static int delete_assign_users (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT  *msgDesc;
	OP_ACCOUNT_USER *account_user_ptr;
	char            *acc_id;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
        account_user_ptr = (OP_ACCOUNT_USER *) catReq->item[1] ;
	acc_id = (char *)  catReq->item[2] ;

        while (account_user_ptr != (OP_ACCOUNT_USER *) NULL ) 
	{
	    sprintf (qDesc->cmd, 
		     "delete account_user where account_id='%s' and user_id='%s'",
		     acc_id, account_user_ptr->user_id );

	    if (execCmd(qDesc) < IMS_OK) 
	    {
		(void) ims_msg (msgDesc, IMS_FATAL,
		"execution of delete_assign_users failed.");
		return(IMS_FATAL);
	    }

	    /* 
	     * Re-initialize query descriptor for next command, 
	     * but do not cancel previous command
	     */
	    if (ims_qiResetDesc(qDesc) < IMS_OK)
	    {
		(void ) ims_msg (msgDesc, IMS_FATAL,
		"add_assign_users: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	    }

	  account_user_ptr = account_user_ptr->next ;

        } /* end of while */

	return(IMS_OK);
}

/***********************************************************************
**
** get_all_datasets 
**
***********************************************************************/
static int get_all_datasets (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ASSIGN_DATASETS *currPtr, *prevPtr;
	OP_ASSIGN_DATASETS *firstPtr, *lastPtr;
	int status; 
	int rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* query to be executed is input through catReq->item[1] */
	qDesc->cmd = (char *)catReq->item[1];

	prevPtr = currPtr = (OP_ASSIGN_DATASETS *)NULL;
	firstPtr = lastPtr = (OP_ASSIGN_DATASETS *)NULL;
	*(int *)catReq->item[0] = (int)0;
	catReq->item[2] = (void *)NULL;


	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (OP_ASSIGN_DATASETS *) 
                 malloc (sizeof (OP_ASSIGN_DATASETS))) == 
                                          (OP_ASSIGN_DATASETS *)NULL)
		{
		   ims_msg (msgDesc, IMS_FATAL, 
   "op_cat__get_all_datasets: Memory allocation for OP_ASSIGN_DATASETS failed.");
			return (IMS_FATAL);
		}


		currPtr->next = (OP_ASSIGN_DATASETS *)NULL;
		currPtr->position = rowCount;
                currPtr->selected = 0 ;
		currPtr->account_dataset_ptr = (OP_ACCOUNT_DATASET *)NULL;
	
		/* a row is returned */
		rowCount += 1;

		/* copy in the returned data */
		(void) memcpy ((char *)&currPtr->dataset_idx,
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((char *)currPtr->dataset,
			qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->dataset[qDesc->valLength[1]] = '\0';
		ims_truncStr (currPtr->dataset);

		(void) memcpy ((char *)currPtr->sensor,
			qDesc->valAddr[2], qDesc->valLength[2]);
		currPtr->sensor[qDesc->valLength[2]] = '\0';
		ims_truncStr (currPtr->sensor);

		(void) memcpy ((char *)currPtr->platform,
			qDesc->valAddr[3], qDesc->valLength[3]);
		currPtr->platform[qDesc->valLength[3]] = '\0';
		ims_truncStr (currPtr->platform);

		if (rowCount == 1)
		{
			firstPtr = currPtr;
			lastPtr = currPtr;
		}
		else
		{
			lastPtr->next = currPtr;
			currPtr->prev = lastPtr;
			lastPtr = currPtr;
		}

	}
	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[0] = (int)rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} 

/***********************************************************************
**
** get_assign_datasets 
**
***********************************************************************/
static int get_assign_datasets (OP_CAT_STRUCT *catReq)
{
  IMS_QI_DESC_OBJ *qDesc;
  IMS_MSG_STRUCT *msgDesc;
  OP_ASSIGN_DATASETS *currPtr, *prevPtr;
  OP_ASSIGN_DATASETS *firstPtr, *lastPtr;
  OP_ACCOUNT_DATASET *first_accdsetPtr, *curr_accdsetPtr, *new_accdsetPtr;
  int status; 
  int rowCount;

  qDesc = catReq->qDesc;
  msgDesc = catReq->msgDesc;

  /* query to be executed is input through catReq->item[1] */
  qDesc->cmd = (char *)catReq->item[1];

  prevPtr = currPtr = (OP_ASSIGN_DATASETS *)NULL;
  firstPtr = lastPtr = (OP_ASSIGN_DATASETS *)NULL;
  first_accdsetPtr = (OP_ACCOUNT_DATASET *)NULL;
  curr_accdsetPtr  = (OP_ACCOUNT_DATASET *)NULL;
  new_accdsetPtr   = (OP_ACCOUNT_DATASET *)NULL;
  *(int *)catReq->item[0] = (int)0;
  catReq->item[2] = (void *)NULL;
  catReq->item[3] = (void *)NULL;
  
  rowCount = 0;
  while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
  {
    if (status < IMS_OK) 
    {
	currPtr = firstPtr;
	while (currPtr != (OP_ASSIGN_DATASETS *)NULL)
	{
	    lastPtr = currPtr->next;
	    free (currPtr);
	    currPtr = lastPtr;
	}
	curr_accdsetPtr = first_accdsetPtr;
	while (curr_accdsetPtr != (OP_ACCOUNT_DATASET *)NULL)
	{
	    new_accdsetPtr = curr_accdsetPtr->next;
	    free (curr_accdsetPtr);
	    curr_accdsetPtr = new_accdsetPtr;
	}
	return (status);
    }
    if (status == IMS_ENDOFQUERY) continue;
    
    if ((currPtr = (OP_ASSIGN_DATASETS *) 
	 malloc (sizeof (OP_ASSIGN_DATASETS))) == 
	(OP_ASSIGN_DATASETS *)NULL)
    {
      ims_msg (msgDesc, IMS_FATAL, 
	       "op_cat_get_assign_datasets: Memory allocation for OP_ASSIGN_DATASETS failed.");
      currPtr = firstPtr;
      while (currPtr != (OP_ASSIGN_DATASETS *)NULL)
      {
	  lastPtr = currPtr->next;
	  free (currPtr);
	  currPtr = lastPtr;
      }
      curr_accdsetPtr = first_accdsetPtr;
      while (curr_accdsetPtr != (OP_ACCOUNT_DATASET *)NULL)
      {
	  new_accdsetPtr = curr_accdsetPtr->next;
	  free (curr_accdsetPtr);
	  curr_accdsetPtr = new_accdsetPtr;
      }
      return (IMS_FATAL);
    }

    currPtr->next = (OP_ASSIGN_DATASETS *)NULL;
    currPtr->prev = (OP_ASSIGN_DATASETS *)NULL;
    currPtr->account_dataset_ptr = (OP_ACCOUNT_DATASET *)NULL;
    currPtr->position = rowCount;
    currPtr->selected = 0 ;
	
    /* a row is returned */
    rowCount += 1;

    /* copy in the returned data */
    (void) memcpy ((char *)&currPtr->dataset_idx,
		   qDesc->valAddr[0], qDesc->valLength[0]);

    (void) memcpy ((char *)currPtr->dataset,
		   qDesc->valAddr[1], qDesc->valLength[1]);
    currPtr->dataset[qDesc->valLength[1]] = '\0';
    ims_truncStr (currPtr->dataset);

    (void) memcpy ((char *)currPtr->sensor,
		   qDesc->valAddr[2], qDesc->valLength[2]);
    currPtr->sensor[qDesc->valLength[2]] = '\0';
    ims_truncStr (currPtr->sensor);

    (void) memcpy ((char *)currPtr->platform,
		   qDesc->valAddr[3], qDesc->valLength[3]);
    currPtr->platform[qDesc->valLength[3]] = '\0';
    ims_truncStr (currPtr->platform);
    
    if (rowCount == 1)
    {
      firstPtr = currPtr;
      lastPtr = currPtr;
    }
    else
    {
      lastPtr->next = currPtr;
      currPtr->prev = lastPtr;
      lastPtr = currPtr;
    }

    curr_accdsetPtr = first_accdsetPtr;
    if (curr_accdsetPtr != (OP_ACCOUNT_DATASET *)NULL)
    {
      /* find the right location in the account_dataset list */
      while ((curr_accdsetPtr->next != (OP_ACCOUNT_DATASET *)NULL)
	     && (currPtr->dataset_idx > curr_accdsetPtr->dataset_idx))
      {
	curr_accdsetPtr = curr_accdsetPtr->next;
      }

      if (curr_accdsetPtr->dataset_idx == currPtr->dataset_idx)
      {
	/* dataset info already exists in the list, just update count */
	curr_accdsetPtr->dataset_count++;
	currPtr->account_dataset_ptr = curr_accdsetPtr;
	continue;
      }
    }

    /* add a new element to the account_dataset list */
    if ((new_accdsetPtr =
	 (OP_ACCOUNT_DATASET *)malloc (sizeof(OP_ACCOUNT_DATASET)))
	== (OP_ACCOUNT_DATASET *)NULL)
    {
        (void) ims_msg (msgDesc, IMS_FATAL,
		      "Memory allocation failed.");
        currPtr = firstPtr;
        while (currPtr != (OP_ASSIGN_DATASETS *)NULL)
        {
	    lastPtr = currPtr->next;
	    free (currPtr);
	    currPtr = lastPtr;
        }
	curr_accdsetPtr = first_accdsetPtr;
	while (curr_accdsetPtr != (OP_ACCOUNT_DATASET *)NULL)
	{
	    new_accdsetPtr = curr_accdsetPtr->next;
	    free (curr_accdsetPtr);
	    curr_accdsetPtr = new_accdsetPtr;
	}
	return (IMS_FATAL);
    }
    else
    {
      new_accdsetPtr->prev = (OP_ACCOUNT_DATASET *)NULL;
      new_accdsetPtr->next = (OP_ACCOUNT_DATASET *)NULL;
      new_accdsetPtr->dataset_idx = currPtr->dataset_idx;
      (void) memcpy ((char *)&new_accdsetPtr->permissions,
		     qDesc->valAddr[4], qDesc->valLength[4]);
      new_accdsetPtr->account_dataset_status = UNCHANGED;
      new_accdsetPtr->dataset_count = 1;
      currPtr->account_dataset_ptr = new_accdsetPtr;
    }

    if (curr_accdsetPtr == (OP_ACCOUNT_DATASET *)NULL)
    {
      first_accdsetPtr = new_accdsetPtr;
    }
    else
    {
      /* 
       * The datasets are retrieved from the catalog ordered by 
       * dataset_idx, append new element at the end of the list.
       */
      curr_accdsetPtr->next = new_accdsetPtr;
      new_accdsetPtr->prev = curr_accdsetPtr;
    }
  } /* end of while (ims_qiNextRow) */

  /* Return result counter and list to the calling routine */
  *(int *)catReq->item[0] = (int)rowCount;
  catReq->item[2] = (void *)firstPtr;
  catReq->item[3] = (void *)first_accdsetPtr;

  return(IMS_OK);

} /* end of get_assign_datasets */

/******************************************************************************
**
** add_assign_datasets ()
**
** Adds the specified datasets to the specified account
**
******************************************************************************/

static int add_assign_datasets (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ACCOUNT_DATASET *accdset_ptr ;
	char *acc_id ;
	char  attributes[IMS_COL255_LEN+1] ;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
        accdset_ptr = (OP_ACCOUNT_DATASET *) catReq->item[1] ;
	acc_id = (char *)  catReq->item[2] ;
	strcpy ( attributes, "(account_id, dataset_idx, oagdr)" ) ;

        while (accdset_ptr != (OP_ACCOUNT_DATASET *) NULL ) 
	{
	  sprintf (qDesc->cmd, 
		   "insert into account_dataset %s values ('%s', %d, %d)",
		   attributes, 
		   acc_id, 
		   accdset_ptr->dataset_idx, 
		   accdset_ptr->permissions );

	  if (execCmd(qDesc) < IMS_OK) 
	  {
	    (void) ims_msg (msgDesc, IMS_FATAL,
			    "execution of add_assign_datasets failed.");
	    return(IMS_FATAL);
	  }

	  /* 
	   * Re-initialize query descriptor for next command, 
	   * but do not cancel previous command.
	   */
	  if (ims_qiResetDesc(qDesc) < IMS_OK)
	  {
	    (void) ims_msg (msgDesc, IMS_FATAL,
            "add_assign_datasets: Could not reinitialize query descriptor.");
	    return(IMS_FATAL);
	  }

	  accdset_ptr = accdset_ptr->next ;
	  
	} /* end of while */

	return(IMS_OK);
}


/******************************************************************************
**
** delete_assign_datasets ()
**
** Deletes the specified datasets to the specified account
**
******************************************************************************/

static int delete_assign_datasets (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ    *qDesc;
	IMS_MSG_STRUCT     *msgDesc;
	OP_ACCOUNT_DATASET *accdset_ptr;
	char               *acc_id;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
        accdset_ptr = (OP_ACCOUNT_DATASET *) catReq->item[1];
	acc_id = (char *) catReq->item[2];

        while (accdset_ptr != (OP_ACCOUNT_DATASET *) NULL ) 
	{
	  sprintf (qDesc->cmd, 
	      "delete account_dataset where account_id='%s' and dataset_idx=%d",
	      acc_id, accdset_ptr->dataset_idx );

	  if (execCmd(qDesc) < IMS_OK) 
	  {
	    (void) ims_msg (msgDesc, IMS_FATAL,
			    "execution of delete_assign_datasets failed.");
	    return(IMS_FATAL);
	  }

	  /* 
	   * Re-initialize query descriptor for next command, 
	   * but do not cancel previous command.
	   */
	  if (ims_qiResetDesc(qDesc) < IMS_OK)
	  {
	    (void) ims_msg (msgDesc, IMS_FATAL,
	    "delete_assign_datasets: Could not reinitialize query descriptor.");
	    return(IMS_FATAL);
	  }

	  accdset_ptr = accdset_ptr->next ;

        } /* end of while */
	
	return(IMS_OK);
}

/******************************************************************************
**
** update_assign_datasets ()
**
** Updates all the datasets for the specified account
**
******************************************************************************/

static int update_assign_datasets (OP_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	OP_ACCOUNT_DATASET *accdset_ptr ;
	char * acc_id ;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;
        accdset_ptr = (OP_ACCOUNT_DATASET *) catReq->item[1] ;
	acc_id = (char *)  catReq->item[2];

        while (accdset_ptr != (OP_ACCOUNT_DATASET *) NULL ) 
	{
	  sprintf (qDesc->cmd, 
 "update account_dataset set oagdr=%d where account_id='%s'and dataset_idx=%d",
		   accdset_ptr->permissions,
		   acc_id, 
		   accdset_ptr->dataset_idx );

	  if (execCmd(qDesc) < IMS_OK) 
	  {
	    (void) ims_msg (msgDesc, IMS_FATAL,
			    "execution of update_assign_datasets failed.");
	    return(IMS_FATAL);
	  }

	  if (IMS_AFFECTED (qDesc) < 1)
	  {
	    (void) ims_msg (msgDesc, IMS_FATAL,
		"update of account dataset failed due to no rows.");
	    return(IMS_FATAL);
	  }

	  /* 
	   * Re-initialize query descriptor for next command, 
	   * but do not cancel previous command.
	   */
	  if (ims_qiResetDesc(qDesc) < IMS_OK)
	  {
	    (void) ims_msg (msgDesc, IMS_FATAL,
	"update_assign_datasets: Could not reinitialize query descriptor.");
	    return(IMS_FATAL);
	  }
	  
	  accdset_ptr = accdset_ptr->next ;
	  
        } /* end of while */

	return(IMS_OK);
}
