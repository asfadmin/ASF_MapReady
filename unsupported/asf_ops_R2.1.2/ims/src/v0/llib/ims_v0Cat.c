static char *sccs = "@(#)ims_v0Cat.c	5.12  08/20/97";
/******************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** File:	ims_v0Cat.c
**
** Function: Catalog database access facility for the v0 server.
**
** Creator   :  Julie Wang, Hoshyar Sayah
**
** Date: July 12, 1994
**
** Modifications:
**
**   07/24/96    jwang   Process_level datatype change.
**
**   07/24/96    jwang   User profile handling change.
**   
**   05/24/96    jwang   PR 849 
**
**   04/25/96    jwang   Added getDatasetInfo to collect dataset comment
**                       and restriction
**
**   04/03/96    jwang   Added code to handle detail metadata collection
**
**   02/20/96    jwang   R1Bprime (preliminary).
**
**   01/22/96    jwang   Changes added to handle DARs and SCAN requests
**
**   10/31/95    jwang   Added getAcctList to handle billing enhancement.  
**
**   09/14/95    jwang   Added order verification related functions 
**
**   09/05/95    jwang   discarded getErrLogLock.  Locking is not needed
**                       for inserting to v0_order_error table 
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
**   01/23/95    jwang   Replaced calls to cdb_qiDescInit() to
**                       cdb_qiDescAlloc()
**
**   11/18/94    jwang   Added seasonal search capability.
**
**   10/01/94    jwang   IMS/DADS Release 0.
**
*****************************************************************************/

/*
** Undefine Posix Source flag because of imcompatibility
** with IK include files.
*/
#undef _POSIX_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <IK_Network.h>
#include <odlinter.h>
#include <ims_query.h>
#include <ims_keyword.h>
#include <ims_v0.h>

/*
** Definition of local constants
*/
#define BUF_SIZE 1024   /* Maximum size of the cmd buffer */

/*
** Local Functions (Entry points into this module)
**
** These function prototypes can be found in the modules where they
** are called. They are listed here for reference.
**
** int v0_cat (V0_CAT_STRUCT *, V0_CAT_EVENT, char *);
*/

/*
** Local Functions
*/
static int useDatabase (V0_CAT_STRUCT *);
static int execDatasetSearch (V0_CAT_STRUCT *);
static int execGranuleSearch (V0_CAT_STRUCT *);
static int getKeywordType (V0_CAT_STRUCT *);
static int getDetailKeyword (V0_CAT_STRUCT *);
static int getInt (V0_CAT_STRUCT *);
static int getTinyInt (V0_CAT_STRUCT *);
static int getSmallInt (V0_CAT_STRUCT *);
static int getDsInfo (V0_CAT_STRUCT *);
static int getProcInfo (V0_CAT_STRUCT *);
static int getProcMediaInfo (V0_CAT_STRUCT *);
static int getAcctInfo (V0_CAT_STRUCT *);
static int getLineItemCost (V0_CAT_STRUCT *);
static int getGnulInfo (V0_CAT_STRUCT *);
static int getNewId (V0_CAT_STRUCT *);
static int getContactInfo (V0_CAT_STRUCT *);
static int getStr (V0_CAT_STRUCT *);
static int getStr2 (V0_CAT_STRUCT *);
static int getStr3 (V0_CAT_STRUCT *);
static int getAcctList (V0_CAT_STRUCT *);
static int getDatasetInfo (V0_CAT_STRUCT *);
static int getProfileInfo (V0_CAT_STRUCT *);
static int getDownlinkKey (V0_CAT_STRUCT *);
static int getDatatakeEntry (V0_CAT_STRUCT *);
static int verifyUser (V0_CAT_STRUCT *);
static int verifyVal (V0_CAT_STRUCT *);
static int verifyEntryChar (V0_CAT_STRUCT *);
static int getV0OrderLock (V0_CAT_STRUCT *);
static int getAccountLock (V0_CAT_STRUCT *);
static int insertRow (V0_CAT_STRUCT *);
static int beginTransaction (V0_CAT_STRUCT *);
static int commitTransaction (V0_CAT_STRUCT *);
static int rollbackTransaction (V0_CAT_STRUCT *);
static int execCmd (IMS_QI_DESC_OBJ *);
static int processRetStatus (IMS_QI_DESC_OBJ *);
static int init_v0_value_list (V0_VALUE_LIST *);
/*
** External Functions
*/

/*
** Global variables
*/
char cmdBuf[BUF_SIZE];

/******************************************************************************
**
** v0_cat ()
**
** Main function handling catalog queries.
**
******************************************************************************/

int v0_cat (V0_CAT_STRUCT *catReq, V0_CAT_EVENT event)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;

	int status;

	msgDesc = catReq->msgDesc;
	cmdBuf[0] = '\0';

	/*
	** We must first make sure that we have a descriptor if the event is
	** anything BUT OPENCONNECTION.
	*/

	if (catReq->qDesc == (IMS_QI_DESC_OBJ *)NULL) 
	{
		if (event == V0_OPENCONNECTION)
		{
			/*
			** allocate and initialize a query descriptor, including geting
			** values from environment variables IMS_DB, IMS_SERVER, IMS_INT
			*/
			if ((catReq->qDesc = ims_qiDescAlloc (msgDesc)) 
				== (IMS_QI_DESC_OBJ *)NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Could not initialize query descriptor.");
				return (IMS_FATAL);
			}
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"First cat event must be V0_OPENCONNECTION.");
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
	case V0_OPENCONNECTION:
		/*
		** We also want to stay logged into the catalog until the 
		** V0_CLOSECONNECTION event is called.
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
				( (int)strlen (catReq->userSpec.dbName) > 0))
		{
			IMS_SETDBNAME (qDesc, catReq->userSpec.dbName);
		}

		/*
		** Set the program name 
		*/
		if ((catReq->userSpec.program != (char *)NULL) &&
				( (int)strlen (catReq->userSpec.program) > 0))
		{
			IMS_SETPROG (qDesc, catReq->userSpec.program);
		}

		/*
		** Set the server name.
		*/
		if ((catReq->userSpec.server != (char *)NULL) &&
				( (int)strlen (catReq->userSpec.server) > 0))
		{
			IMS_SETSERVER (qDesc, catReq->userSpec.server);
		}

		/*
		** Do a login to the database.
		*/
		if ((status = ims_qiLogin (qDesc)) < IMS_OK)
		{
			return (status);
		}

		/*
		** Assign srvproc to be the user defined data portion of the
		** dbproc structure.  This is needed by the message and
		** error handlers for SQLServer connections of the ims_v0Srv.
		*/
		(void) dbsetuserdata (qDesc->dbproc, (BYTE *)msgDesc);
		return (IMS_OK);

	case V0_USEDATABASE:
		status = useDatabase (catReq);
		break;

	case V0_EXECDATASETSEARCH:
		status = execDatasetSearch (catReq);
		break;

	case V0_EXECGRANULESEARCH:
		status = execGranuleSearch (catReq);
		break;

	case V0_GETKEYWORDTYPE:
		status = getKeywordType (catReq);
		break;

	case V0_GETDETAILKEYWORD:
		status = getDetailKeyword (catReq);
		break;

	case V0_GETACCTLIST:
		status = getAcctList (catReq);
		break;

	case V0_GETGNULINFO:
		status = getGnulInfo (catReq);
		break;

	case V0_GETSTR:
		status = getStr (catReq);
		break;

	case V0_GETSTR2:
		status = getStr2 (catReq);
		break;

	case V0_GETSTR3: 
		status = getStr3 (catReq);
		break;

	case V0_GETINT:
		status = getInt (catReq);
		break;

	case V0_GETTINYINT:
		status = getTinyInt (catReq);
		break;

	case V0_GETSMALLINT:
		status = getSmallInt (catReq);
		break;

	case V0_GETDSINFO:
		status = getDsInfo (catReq);
		break;

	case V0_GETPROCINFO:
		status = getProcInfo (catReq);
		break;

	case V0_GETPROCMEDIAINFO:
		status = getProcMediaInfo (catReq);
		break;

	case V0_GETACCTINFO:
		status = getAcctInfo (catReq);
		break;

	case V0_GETLINEITEMCOST:
		status = getLineItemCost (catReq);
		break;

	case V0_GETNEWID:
		status = getNewId (catReq);
		break;

	case V0_GETCONTACTINFO:
		status = getContactInfo (catReq);
		break;

	case V0_GETDATASETINFO:
		status = getDatasetInfo (catReq);
		break;

	case V0_GETPROFILEINFO:
		status = getProfileInfo (catReq);
		break;

	case V0_GETV0ORDERLOCK:
		status = getV0OrderLock (catReq);
		break;

	case V0_GETACCOUNTLOCK:
		status = getAccountLock (catReq);
		break;

	case V0_GETDATATAKE:
		status = getDatatakeEntry (catReq);
		break;

	case V0_GETDOWNLINK:
		status = getDownlinkKey (catReq);
		break;

	case V0_VERIFYUSER:
		status = verifyUser (catReq);
		break;

	case V0_VERIFYVAL: 
		status = verifyVal (catReq);
		break;

	case V0_VERIFYENTRYCHAR: 
		status = verifyEntryChar (catReq);
		break;

	case V0_INSERTROW:
		status = insertRow (catReq);
		break;

	case V0_BEGINTRANSACTION:
		status = beginTransaction (catReq);
		break;

	case V0_ROLLBACKTRANSACTION:
		status = rollbackTransaction (catReq);
		break;

	case V0_COMMITTRANSACTION:
		status = commitTransaction (catReq);
		break;

	case V0_CLOSECONNECTION:
		/*
		** Close the catalog the connection.  
		*/
		status = ims_qiLogoff (catReq->qDesc);
		/* Free qDesc and associated allocated memory */
		ims_qiFreeDesc (catReq->qDesc);
		catReq->qDesc = (IMS_QI_DESC_OBJ *)NULL;
		return (status); 

	default:
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_cat: Invalid catalog event passed to v0_cat.");
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
				"v0_cat: Could not reinitialize query descriptor.");
			return(IMS_FATAL);
		}
	}

	/*
	** Return with the appropriate status
	*/
	if (status < IMS_OK) return (status);


	return (IMS_OK);
} /* v0_cat */

/***********************************************************************
**
** useDatabase -  
**
***********************************************************************/
static int useDatabase (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	V0_CAT_USERSPEC *userSpec;
	int status;

	qDesc = catReq->qDesc;
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
** execDatasetSearch -  
**
***********************************************************************/
static int execDatasetSearch (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_DATASET_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_DATASET_LIST *)NULL;

	/* SQL statement to be executed is input through item[0] */
	qDesc->cmd =  (char *)catReq->item[0];

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__execDatasetSearch: failed to get result row.");
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (V0_DATASET_LIST *)
			malloc (sizeof (V0_DATASET_LIST))) == (V0_DATASET_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__execDatasetSearch: Memory allocation for "
				"V0_DATASET_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->parameter = (V0_VALUE_LIST *)NULL;
		currPtr->account_list = (V0_USER_ACCT_LIST *)NULL;
		currPtr->temporal_key_list = (V0_KEYWORD_LIST *)NULL;
		currPtr->temporal_key_count = 0;
		currPtr->spatial_key_list = (V0_KEYWORD_LIST *)NULL;
		currPtr->spatial_key_count = 0;
		currPtr->detail_key_list = (V0_KEYWORD_LIST *)NULL;
		currPtr->detail_key_count = 0;
		currPtr->granule_list = (V0_GRANULE_LIST *)NULL;
		currPtr->old_granule_list = (V0_GRANULE_LIST *)NULL;
		currPtr->unordered_granule_list = (V0_GRANULE_LIST *)NULL;
		currPtr->granule_count = 0;
		currPtr->next_p = (V0_DATASET_LIST *)NULL;
		currPtr->prev_p = (V0_DATASET_LIST *)NULL;

		/* a row is returend */
		rowCount += 1;

		/* copy in  the returned data */
		(void) memcpy ((DBSMALLINT *)&(currPtr->dataset_idx),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((DBCHAR *)currPtr->dataset_id,
			qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->dataset_id[qDesc->valLength[1]] = '\0';
		ims_truncStr (currPtr->dataset_id);

		(void) memcpy ((DBCHAR *)currPtr->md_id,
			qDesc->valAddr[2], qDesc->valLength[2]);
		currPtr->md_id[qDesc->valLength[2]] = '\0';
		ims_truncStr (currPtr->md_id);

		(void) memcpy ((DBCHAR *)currPtr->sensor,
			qDesc->valAddr[3], qDesc->valLength[3]);
		currPtr->sensor[qDesc->valLength[3]] = '\0';
		ims_truncStr (currPtr->sensor);

		(void) memcpy ((DBCHAR *)currPtr->platform,
			qDesc->valAddr[4], qDesc->valLength[4]);
		currPtr->platform[qDesc->valLength[4]] = '\0';
		ims_truncStr (currPtr->platform);

		(void) memcpy ((DBCHAR *)currPtr->day_night,
			qDesc->valAddr[5], qDesc->valLength[5]);
		currPtr->day_night[qDesc->valLength[5]] = '\0';
		ims_truncStr (currPtr->day_night);

		(void) memcpy ((DBSMALLINT *)&(currPtr->process_level),
			qDesc->valAddr[6], qDesc->valLength[6]);

		(void) memcpy ((DBCHAR *)currPtr->granules_table,
			qDesc->valAddr[7], qDesc->valLength[7]);
		currPtr->granules_table[qDesc->valLength[7]] = '\0';
		ims_truncStr (currPtr->granules_table);

		(void) memcpy ((DBSMALLINT *)&(currPtr->temporal_type),
			qDesc->valAddr[8], qDesc->valLength[8]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->spatial_type),
			qDesc->valAddr[9], qDesc->valLength[9]);

		(void) memcpy ((DBCHAR *)currPtr->campaign,
			qDesc->valAddr[10], qDesc->valLength[10]);
		currPtr->campaign[qDesc->valLength[10]] = '\0';
		ims_truncStr (currPtr->campaign);


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
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_cat__execDatasetSearch: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_DATASET_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* execDatasetSearch */

/***********************************************************************
**
** execGranuleSearch -  
**
***********************************************************************/
static int execGranuleSearch (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_GRANULE_LIST *firstPtr, *lastPtr, *currPtr;
	V0_VALUE_LIST   *temp_ptr;
	V0_KEYWORD_LIST *k_ptr;
	int status, rowCount;
	char row_limit[IMS_COL10_LEN+1];
	int  spatial_type;
	int  detail_key_count;
	int  count, i;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_GRANULE_LIST *)NULL;

	/* SQL statement to be executed is input through item[0] */
	qDesc->cmd =  (char *)catReq->item[0];

	rowCount = 0;

	/* 
	** set the option to return maximum rows for a granule search based
	** on users granule_limit specification.  
	*/
	row_limit[0] = '\0';
	(void) sprintf (row_limit, "%d", (int *)catReq->item[3]); 

	IMS_SETROWCOUNT (qDesc, row_limit);

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			qDesc->cmd = cmdBuf;

			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__execGranuleSearch: failed to get result row.");
			/*
			** set the DBROWCOUNT to default.  
			*/
			IMS_SETROWCOUNT (qDesc, "0");

			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (V0_GRANULE_LIST *)
			malloc (sizeof (V0_GRANULE_LIST))) == (V0_GRANULE_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__execGranuleSearch: Memory allocation for "
				"V0_GRANULE_LIST failed.");
			IMS_SETROWCOUNT (qDesc, "0");
			return (IMS_FATAL);
		}

		currPtr->detailed_keyword = (V0_VALUE_LIST *)NULL;
		currPtr->formatted_list = (V0_VALUE_LIST *)NULL;
		currPtr->next_p = (V0_GRANULE_LIST *)NULL;

		/* a row is returend */
		rowCount += 1;
		count = 0;

		/* copy in  the returned data */
		(void) memcpy ((DBINT *)&(currPtr->granule_idx),
			qDesc->valAddr[0], qDesc->valLength[0]);
		count++;

		(void) memcpy ((DBCHAR *)currPtr->granule_id,
			qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->granule_id[qDesc->valLength[1]] = '\0';
		(void) ims_truncStr (currPtr->granule_id);
		count++;

		(void) memcpy ((DBCHAR *)currPtr->start_time,
			qDesc->valAddr[2], qDesc->valLength[2]);
		currPtr->start_time[qDesc->valLength[2]] = '\0';
		(void) ims_truncStr (currPtr->start_time);
		count++;

		(void) memcpy ((DBCHAR *)currPtr->stop_time,
			qDesc->valAddr[3], qDesc->valLength[3]);
		currPtr->stop_time[qDesc->valLength[3]] = '\0';
		(void) ims_truncStr (currPtr->stop_time);
		count++;

		/*
		** spatial_type: 1=point, 2=rectangular, 3=point&range (not used),
		**               4= four corner
		*/
		spatial_type = (int)catReq->item[4];

		switch  (spatial_type)
		{
			case 0:

				/*
				** nothing is returned for 0 spatial_type for now.
				*/
				
				break;

			case 1:
				(void) memcpy ((DBREAL *)&currPtr->center_lat,
						qDesc->valAddr[4], qDesc->valLength[4]);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->center_lon,
					qDesc->valAddr[5], qDesc->valLength[5]);
				count++;

				break;

			case 2:
				(void) memcpy ((DBREAL *)&currPtr->north_lat,
						qDesc->valAddr[4], qDesc->valLength[4]);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->south_lat,
					qDesc->valAddr[5], qDesc->valLength[5]);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->west_lon,
					qDesc->valAddr[6], qDesc->valLength[6]);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->east_lon,
					qDesc->valAddr[7], qDesc->valLength[7]);
				count++;

				break;

			case 3:
				break;

			case 4:
				(void) memcpy ((DBREAL *)&currPtr->ns_lat,
						qDesc->valAddr[4], qDesc->valLength[4]);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->ns_lon,
					qDesc->valAddr[5], qDesc->valLength[5]);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->ne_lat,
					qDesc->valAddr[6], qDesc->valLength[6]);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->ne_lon,
					qDesc->valAddr[7], qDesc->valLength[7]);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->fs_lat,
						qDesc->valAddr[8], qDesc->valLength[8]);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->fs_lon,
					qDesc->valAddr[9], qDesc->valLength[9]);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->fe_lat,
					qDesc->valAddr[10], qDesc->valLength[10]);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->fe_lon,
					qDesc->valAddr[11], qDesc->valLength[11]);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->center_lat,
						qDesc->valAddr[12], qDesc->valLength[12]);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->center_lon,
					qDesc->valAddr[13], qDesc->valLength[13]);
				count++;

				(void) memcpy ((DBCHAR *)currPtr->asc_desc,
					qDesc->valAddr[14], qDesc->valLength[14]);
				currPtr->asc_desc[qDesc->valLength[14]] = '\0';
				ims_truncStr (currPtr->asc_desc);
				count++;

				(void) memcpy ((DBCHAR *)currPtr->pole_included,
					qDesc->valAddr[15], qDesc->valLength[15]);
				currPtr->pole_included[qDesc->valLength[15]] = '\0';
				(void) ims_truncStr (currPtr->pole_included);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->max_lat,
						qDesc->valAddr[16], qDesc->valLength[16]);
				count++;

				(void) memcpy ((DBREAL *)&currPtr->min_lat,
					qDesc->valAddr[17], qDesc->valLength[17]);
				count++;

				break;

		} /* spatial_type switch */

		/* start retrieving detail keywords */

		detail_key_count = (int)catReq->item[5];
		k_ptr = (V0_KEYWORD_LIST *)catReq->item[6];

		for (i=1; 
				 (i <= detail_key_count) && (k_ptr != (V0_KEYWORD_LIST *)NULL); 
				 i++, count++, k_ptr = k_ptr->next_p)
		{
			if ( i == 1)
			{
				if ((temp_ptr = (V0_VALUE_LIST *)
					malloc (sizeof (V0_VALUE_LIST))) == (V0_VALUE_LIST *)NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL, 
						"v0_cat__execGranuleSearch: Memory allocation for "
						"V0_VALUE_LIST failed.");
						IMS_SETROWCOUNT (qDesc, "0");
						return (IMS_FATAL);
				}
			}
			else
			{
				if ((temp_ptr->next_p = (V0_VALUE_LIST *)
					malloc (sizeof (V0_VALUE_LIST))) == (V0_VALUE_LIST *)NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL, 
						"v0_cat__execGranuleSearch: Memory allocation for "
						"V0_VALUE_LIST failed.");
						IMS_SETROWCOUNT (qDesc, "0");
						return (IMS_FATAL);
				}
				temp_ptr = temp_ptr->next_p;

			}

			temp_ptr->next_p = (V0_VALUE_LIST *)NULL; 

			/*
			** temp_ptr points to a structure of V0_VALUE_LIST type. 
			** Results are stored in the following fields:
			**
			**  char_value1       <-- keyword
			**  char_value2       <-- keyword value (in ascii) 
			*/
			strcpy (temp_ptr->char_value1, k_ptr->keyword);

			switch (k_ptr->data_type)
			{
				case (IMS_INT1_TYPE):
					(void) memcpy ((DBTINYINT *)&(temp_ptr->tinyint_value),
					qDesc->valAddr[count], qDesc->valLength[count]);

					temp_ptr->char_value2[0] = '\0';
					sprintf (temp_ptr->char_value2, "%d", temp_ptr->tinyint_value); 
					ims_truncStr (temp_ptr->char_value2);
					break;

				case (IMS_INT2_TYPE):
					(void) memcpy ((DBSMALLINT *)&(temp_ptr->smallint_value),
					qDesc->valAddr[count], qDesc->valLength[count]);

					temp_ptr->char_value2[0] = '\0';
					sprintf (temp_ptr->char_value2, "%d", temp_ptr->smallint_value); 
					ims_truncStr (temp_ptr->char_value2);
					break;

				case (IMS_INT4_TYPE):
					(void) memcpy ((DBINT *)&(temp_ptr->int_value),
					qDesc->valAddr[count], qDesc->valLength[count]);

					temp_ptr->char_value2[0] = '\0';
					sprintf (temp_ptr->char_value2, "%d", temp_ptr->int_value); 
					ims_truncStr (temp_ptr->char_value2);
					break;

				case (IMS_FLOAT4_TYPE):
					(void) memcpy ((DBREAL *)&(temp_ptr->real_value),
					qDesc->valAddr[count], qDesc->valLength[count]);

					temp_ptr->char_value2[0] = '\0';
					sprintf (temp_ptr->char_value2, "%-30.2f", temp_ptr->real_value); 
					ims_truncStr (temp_ptr->char_value2);
					break;

				case (IMS_FLOAT8_TYPE):
					(void) memcpy ((DBFLT8 *)&(temp_ptr->flt8_value),
					qDesc->valAddr[count], qDesc->valLength[count]);

					temp_ptr->char_value2[0] = '\0';
					sprintf (temp_ptr->char_value2, "%-30.2f", temp_ptr->flt8_value); 
					ims_truncStr (temp_ptr->char_value2);
					break;

				case (IMS_CHAR_TYPE):
				case (IMS_SYMBOL_TYPE):
				case (IMS_STRING_TYPE):
				case (IMS_DATETIME_TYPE):
				case (IMS_DOYTIME_TYPE):
					(void) memcpy ((DBCHAR *)temp_ptr->char_value2,
						qDesc->valAddr[count], qDesc->valLength[count]);
					temp_ptr->char_value2[qDesc->valLength[count]] = '\0';
					ims_truncStr (temp_ptr->char_value2);
					break;

				default:
					(void) ims_msg (msgDesc, IMS_FATAL, 
						"v0_cat__execGranuleSearch: Data type of"
						" keyword %s is either missing or invalid.", 
						k_ptr->keyword);
					IMS_SETROWCOUNT (qDesc, "0");
					return (IMS_FATAL);
			} /* data_type switch */

			if (i == 1)
			{
				currPtr->detailed_keyword = temp_ptr;
			}
		} /* for */

		if ((i <= detail_key_count) || (k_ptr != (V0_KEYWORD_LIST *)NULL)) 
		{
					(void) ims_msg (msgDesc, IMS_FATAL, 
						"v0_cat__execGranuleSearch: Number of detail keyword is "
						" does not match number of returned values.");
					IMS_SETROWCOUNT (qDesc, "0");
					return (IMS_FATAL);
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
	qDesc->cmd = cmdBuf;
	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_cat__execGranuleSearch: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_GRANULE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		IMS_SETROWCOUNT (qDesc, "0");
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	IMS_SETROWCOUNT (qDesc, "0");
	return(IMS_OK);

} /* end of execGranuleSearch  */

/***********************************************************************
**
** getDatatakeEntry -  
**
***********************************************************************/
static int getDatatakeEntry (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_LINE_ITEM_LIST *firstPtr, *lastPtr, *currPtr;
	V0_LINE_ITEM_LIST *line_item;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_LINE_ITEM_LIST *)NULL;

	/* SQL statement to be executed is input through item[0] */
	qDesc->cmd =  (char *)catReq->item[0];

	/* original line item info is input through item[3] */
	line_item = (V0_LINE_ITEM_LIST *)catReq->item[3];

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__getDatatakeEntry: failed to get result row.");
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (V0_LINE_ITEM_LIST *)
			malloc (sizeof (V0_LINE_ITEM_LIST))) == (V0_LINE_ITEM_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__getDatatakeEntry: Memory allocation for "
				"V0_LINE_ITEM_LIST failed.");
			return (IMS_FATAL);
		}

		/* 
		** initialize this scan item with info received from the 
		** original line_item
		*/
		*currPtr = *line_item;
 
		/* a row is returend */
		rowCount += 1;

		/* copy in  the returned data */
                (void) memcpy ((DBCHAR *)currPtr->platform_alias,
                        qDesc->valAddr[0], qDesc->valLength[0]);
                currPtr->platform_alias[qDesc->valLength[0]] = '\0';
                ims_truncStr (currPtr->platform_alias);
	
                (void) memcpy ((DBCHAR *)currPtr->sensor_alias,
                        qDesc->valAddr[1], qDesc->valLength[1]);       
                currPtr->sensor_alias[qDesc->valLength[1]] = '\0';       
                ims_truncStr (currPtr->sensor_alias);
	
		(void) memcpy ((DBINT *)&(currPtr->revolution),
			qDesc->valAddr[2], qDesc->valLength[2]);

                (void) memcpy ((DBSMALLINT *)&(currPtr->sequence),
                        qDesc->valAddr[3], qDesc->valLength[3]);

                (void) memcpy ((DBCHAR *)currPtr->quicklook_p,      
                        qDesc->valAddr[4], qDesc->valLength[4]);       
                currPtr->quicklook_p[qDesc->valLength[4]] = '\0';           
                ims_truncStr (currPtr->quicklook_p);

		if (currPtr->quicklook_p[0] == 'Y' &&
		    line_item->quicklook_p[0] == 'Y')
		{
			strcpy(currPtr->quicklook_p,"Y");
		}
		else
		{
			strcpy(currPtr->quicklook_p,"N");
		}
			
                (void) memcpy ((DBCHAR *)currPtr->mode,      
                        qDesc->valAddr[5], qDesc->valLength[5]);       
                currPtr->mode[qDesc->valLength[5]] = '\0';           
		ims_truncStr (currPtr->mode);

                (void) memcpy ((DBCHAR *)currPtr->frame_mode,      
                        qDesc->valAddr[6], qDesc->valLength[6]);       
                currPtr->frame_mode[qDesc->valLength[6]] = '\0';           
		ims_truncStr (currPtr->frame_mode);

                (void) memcpy ((DBCHAR *)currPtr->site_name,      
                        qDesc->valAddr[7], qDesc->valLength[7]);       
                currPtr->site_name[qDesc->valLength[7]] = '\0';           
                ims_truncStr (currPtr->site_name);

                (void) memcpy ((DBCHAR *)currPtr->time_on,      
                        qDesc->valAddr[8], qDesc->valLength[8]);       
                currPtr->time_on[qDesc->valLength[8]] = '\0';           
                ims_truncStr (currPtr->time_on);

                (void) memcpy ((DBCHAR *)currPtr->time_off,      
                        qDesc->valAddr[9], qDesc->valLength[9]);       
                currPtr->time_off[qDesc->valLength[9]] = '\0';           
                ims_truncStr (currPtr->time_off);

                (void) memcpy ((DBCHAR *)currPtr->activity_id,      
                        qDesc->valAddr[10], qDesc->valLength[10]);       
                currPtr->activity_id[qDesc->valLength[10]] = '\0';           
                ims_truncStr (currPtr->activity_id);

                (void) memcpy ((DBCHAR *)currPtr->station_id,      
                        qDesc->valAddr[11], qDesc->valLength[11]);       
                currPtr->station_id[qDesc->valLength[11]] = '\0';           
                ims_truncStr (currPtr->station_id);

                (void) memcpy ((DBCHAR *)currPtr->sensor_name,
                        qDesc->valAddr[12], qDesc->valLength[12]);
                currPtr->sensor_name[qDesc->valLength[12]] = '\0';
                ims_truncStr (currPtr->sensor_name);

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
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_cat__getDatatakeEntry: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_LINE_ITEM_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}
	/* return number of records found in the datatake_entry table */
	*(int *)catReq->item[1] = (int) rowCount;

	/* Return list to the calling routine */
	catReq->item[2] = (void *)firstPtr;

	/* return pointer to the last item in the list to the caller */
	catReq->item[4] = (void *) lastPtr;

	return(IMS_OK);

} /* end of getDatatakeEntry */

/***********************************************************************
**
** getDownlinkKey -  
**
***********************************************************************/

static int getDownlinkKey (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_VALUE_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_VALUE_LIST *)NULL;

	/* 
	** item[0] points to the sql statement 
	** which looks like "select PLATFORM, SENSOR, REVOLUTION,SEQUENCE,
	** MEDIA_ID_TYPE_NAME from granules_table..."
	**/
	qDesc->cmd =  (char *)catReq->item[0];

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__getDownlinkKey: failed to get result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (V0_VALUE_LIST *)
			malloc (sizeof (V0_VALUE_LIST))) == (V0_VALUE_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__getStr: Memory allocation for V0_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_v0_value_list (currPtr);

		/* a row is returend */
		rowCount += 1;

		/* copy in  the returned data */

		/* PLATFORM */
		(void) memcpy ((DBCHAR *)currPtr->char_value1,
			qDesc->valAddr[0], qDesc->valLength[0]);
		currPtr->char_value1[qDesc->valLength[0]] = '\0';
		ims_truncStr (currPtr->char_value1);

		/* SENSOR */
		(void) memcpy ((DBCHAR *)currPtr->char_value2,
                        qDesc->valAddr[1], qDesc->valLength[1]);
                currPtr->char_value2[qDesc->valLength[0]] = '\0';
                ims_truncStr (currPtr->char_value2);
 
		/* REVOLUTION */
                (void) memcpy ((DBINT *)&(currPtr->int_value),
                        qDesc->valAddr[2], qDesc->valLength[2]);

		/* SEQUENCE */
                (void) memcpy ((DBSMALLINT *)&(currPtr->smallint_value),
                        qDesc->valAddr[3], qDesc->valLength[3]);

		/* MEDIA_ID_TYPE_NAME */
                (void) memcpy ((DBCHAR *)currPtr->char_value3,
                        qDesc->valAddr[4], qDesc->valLength[4]);
                currPtr->char_value3[qDesc->valLength[4]] = '\0';
                ims_truncStr (currPtr->char_value3);

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
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_cat__getDownlinkKey: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* item[2] points to the result rows to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return(IMS_OK);

} /* end of getDownlinkKey */

/***********************************************************************
**
** getStr -  general to get one, or a list of, character strings
**
***********************************************************************/
static int getStr (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_VALUE_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_VALUE_LIST *)NULL;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__getStr: failed to get result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (V0_VALUE_LIST *)
			malloc (sizeof (V0_VALUE_LIST))) == (V0_VALUE_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__getStr: Memory allocation for V0_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_v0_value_list (currPtr);

		/* a row is returend */
		rowCount += 1;

		/* copy in  the returned data */

		(void) memcpy ((DBCHAR *)currPtr->char_value1,
			qDesc->valAddr[0], qDesc->valLength[0]);
		currPtr->char_value1[qDesc->valLength[0]] = '\0';
		ims_truncStr (currPtr->char_value1);

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
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_cat__getStr: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* item[2] points to the result rows to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return(IMS_OK);

} /* end of getStr */

/***********************************************************************
**
** getStr2 -  get one or a list of result rows with two strings each
**
***********************************************************************/
static int getStr2 (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_VALUE_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_VALUE_LIST *)NULL;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__getStr2: failed to get result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (V0_VALUE_LIST *)
			malloc (sizeof (V0_VALUE_LIST))) == (V0_VALUE_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__getStr2: Memory allocation for V0_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_v0_value_list (currPtr);

		/* a row is returend */
		rowCount += 1;

		/* copy in  the returned data */

		(void) memcpy ((DBCHAR *)currPtr->char_value1,
			qDesc->valAddr[0], qDesc->valLength[0]);
		currPtr->char_value1[qDesc->valLength[0]] = '\0';
		ims_truncStr (currPtr->char_value1);

		(void) memcpy ((DBCHAR *)currPtr->char_value2,
			qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->char_value2[qDesc->valLength[1]] = '\0';
		ims_truncStr (currPtr->char_value2);

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
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_cat__getStr2: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* item[2] points to the result rows to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* end of getStr2 */

/***********************************************************************
**
** getStr3 -  get one or a list of result rows with three strings each
**
***********************************************************************/
static int getStr3 (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_VALUE_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_VALUE_LIST *)NULL;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__getStr3: failed to get result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (V0_VALUE_LIST *)
			malloc (sizeof (V0_VALUE_LIST))) == (V0_VALUE_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__getStr3: Memory allocation for V0_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_v0_value_list (currPtr);

		/* a row is returend */
		rowCount += 1;

		/* copy in  the returned data */

		(void) memcpy ((DBCHAR *)currPtr->char_value1,
			qDesc->valAddr[0], qDesc->valLength[0]);
		currPtr->char_value1[qDesc->valLength[0]] = '\0';
		ims_truncStr (currPtr->char_value1);

		(void) memcpy ((DBCHAR *)currPtr->char_value2,
			qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->char_value2[qDesc->valLength[1]] = '\0';
		ims_truncStr (currPtr->char_value2);

		(void) memcpy ((DBCHAR *)currPtr->char_value3,
			qDesc->valAddr[2], qDesc->valLength[2]);
		currPtr->char_value3[qDesc->valLength[2]] = '\0';
		ims_truncStr (currPtr->char_value3);

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
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_cat__getStr3: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* item[2] points to the result rows to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *)firstPtr;

	return(IMS_OK);

} /* end of getStr3 */

/***********************************************************************
** 
** getInt -  general routine to get one or a list of integers
**
***********************************************************************/
static int getInt (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_VALUE_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_VALUE_LIST *)NULL;

	rowCount = 0;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getInt: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_VALUE_LIST *)
			malloc (sizeof (V0_VALUE_LIST))) == (V0_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getInt: Memory allocation for V0_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_v0_value_list (currPtr);

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBINT *)&(currPtr->int_value),
			qDesc->valAddr[0], qDesc->valLength[0]);

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
			"v0_cat__getInt: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

}/* end of getInt */

/***********************************************************************
** 
** getTinyInt -  general routine to get one or a list of tiny integers
**
***********************************************************************/
static int getTinyInt (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_VALUE_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_VALUE_LIST *)NULL;

	rowCount = 0;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getTinyInt: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_VALUE_LIST *)
			malloc (sizeof (V0_VALUE_LIST))) == (V0_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getTinyInt: Memory allocation for V0_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_v0_value_list (currPtr);

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBTINYINT *)&(currPtr->tinyint_value),
			qDesc->valAddr[0], qDesc->valLength[0]);

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
			"v0_cat__getTinyInt: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

}/*  end of getTinyInt */

/***********************************************************************
** 
** getSmallInt -  general routine to get one or a list of small integers
**
***********************************************************************/
static int getSmallInt (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_VALUE_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_VALUE_LIST *)NULL;

	rowCount = 0;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getSmallInt: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_VALUE_LIST *)
			malloc (sizeof (V0_VALUE_LIST))) == (V0_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getSmallInt: Memory allocation for V0_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_v0_value_list (currPtr);

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBSMALLINT *)&(currPtr->smallint_value),
			qDesc->valAddr[0], qDesc->valLength[0]);

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
			"v0_cat__getSmallInt: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

}/*  end of getSmallInt */

/***********************************************************************
** 
** getKeywordType -  
**
***********************************************************************/
static int getKeywordType (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_KEYWORD_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_KEYWORD_LIST *)NULL;

	(void) sprintf (qDesc->cmd, "exec v0_get_keyword_type %d, %d",
		*(DBSMALLINT *)catReq->item[0], *(DBSMALLINT *)catReq->item[1]);

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getKeywordType: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_KEYWORD_LIST *)
			malloc (sizeof (V0_KEYWORD_LIST))) == (V0_KEYWORD_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getKeywordType: Memory allocation for V0_KEYWORD_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next_p = (V0_KEYWORD_LIST *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
	 	(void) memcpy ((DBCHAR *)currPtr->keyword,
			qDesc->valAddr[0], qDesc->valLength[0]);
	 	currPtr->keyword[qDesc->valLength[0]] = '\0';
	 	ims_truncStr (currPtr->keyword);
												  
		(void) memcpy ((DBSMALLINT *)&(currPtr->data_type),
			qDesc->valAddr[1], qDesc->valLength[1]);

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
			"v0_cat__getKeywordType: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_KEYWORD_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	if (rowCount == 0)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_cat__getKeywordType: Could not find matching keyword "
			"for dataset_idx %d  query_type %d",
		*(DBSMALLINT *)catReq->item[0], *(DBSMALLINT *)catReq->item[1]);
		return (IMS_FATAL);
	}


	/* item[2] points to the result rows to the calling routine */
	catReq->item[2] = (void *) firstPtr;

	return(IMS_OK);
} /* end of getKeywordType */

/***********************************************************************
**
** getDetailKeyword -  
**
***********************************************************************/
static int getDetailKeyword (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_KEYWORD_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_KEYWORD_LIST *)NULL;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__getDetailKeyword: failed to get result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (V0_KEYWORD_LIST *)
			malloc (sizeof (V0_KEYWORD_LIST))) == (V0_KEYWORD_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__getDetailKeyword: Memory allocation for V0_KEYWORD_LIST failed.");
			return (IMS_FATAL);
		}
		currPtr->next_p = (V0_KEYWORD_LIST *)NULL;

		/* a row is returend */
		rowCount += 1;

		/* copy in  the returned data */
			 
	 	(void) memcpy ((DBCHAR *)currPtr->keyword,
			qDesc->valAddr[0], qDesc->valLength[0]);
	 	currPtr->keyword[qDesc->valLength[0]] = '\0';
	 	ims_truncStr (currPtr->keyword);
												  
		(void) memcpy ((DBSMALLINT *)&(currPtr->data_type),
			qDesc->valAddr[1], qDesc->valLength[1]);

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
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_cat__getDetailKeyword: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_KEYWORD_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* item[2] points to the result rows to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return(IMS_OK);

} /* getDetailKeyword */

/***********************************************************************
** 
** getAcctList -  
**
***********************************************************************/
static int getAcctList (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_USER_ACCT_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_USER_ACCT_LIST *)NULL;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getAcctList: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_USER_ACCT_LIST *)
			malloc (sizeof (V0_USER_ACCT_LIST))) == (V0_USER_ACCT_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getAcctList: Memory allocation for V0_USER_ACCT_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->err_msg = (V0_ERR_LIST *)NULL;
		currPtr->next_p = (V0_USER_ACCT_LIST *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
	 	(void) memcpy ((DBCHAR *)currPtr->account_id,
			qDesc->valAddr[0], qDesc->valLength[0]);
	 	currPtr->account_id[qDesc->valLength[0]] = '\0';
	 	ims_truncStr (currPtr->account_id);
												  
		(void) memcpy ((DBREAL *)&(currPtr->balance),
			qDesc->valAddr[1], qDesc->valLength[1]);

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
			"v0_cat__getAcctList: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_USER_ACCT_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}


	/* item[2] points to the result rows to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return(IMS_OK);
} /* end of getAcctList */

/***********************************************************************
** 
** getGnulInfo -  
**
***********************************************************************/
static int getGnulInfo (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_GRANULE_INFO *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_GRANULE_INFO *)NULL;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getGnulInfo: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_GRANULE_INFO *)
			malloc (sizeof (V0_GRANULE_INFO))) == (V0_GRANULE_INFO *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getGnulInfo: Memory allocation for V0_GRANULE_INFO failed.");
			return (IMS_FATAL);
		}

		currPtr->next_p = (V0_GRANULE_INFO *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBINT *)&(currPtr->granule_idx),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((DBINT *)&(currPtr->data_kbytes),
			qDesc->valAddr[1], qDesc->valLength[1]);

		(void) memcpy ((DBINT *)&(currPtr->metadata_kbytes),
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
			"v0_cat__getGnulInfo: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_GRANULE_INFO *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}


	/* item[2] points to the result rows to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return(IMS_OK);
} /* end of getGnulInfo */ 

/***********************************************************************
** 
** getDsInfo -  
**
***********************************************************************/
static int getDsInfo (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_DS_INFO_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_DS_INFO_LIST *)NULL;

	rowCount = 0;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getDsInfo: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_DS_INFO_LIST *)
			malloc (sizeof (V0_DS_INFO_LIST))) == (V0_DS_INFO_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getDsInfo: Memory allocation for V0_DS_INFO_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next_p = (V0_DS_INFO_LIST *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBSMALLINT *)&(currPtr->ds_idx),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->order_item_type),
			qDesc->valAddr[1], qDesc->valLength[1]);

	 	(void) memcpy ((DBCHAR *)currPtr->granules_table,
			qDesc->valAddr[2], qDesc->valLength[2]);
	 	currPtr->granules_table[qDesc->valLength[2]] = '\0';
	 	ims_truncStr (currPtr->granules_table);
												  
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
			"v0_cat__getDsInfo: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_DS_INFO_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

}/* end of getDsInfo */

/***********************************************************************
** 
** getProcMediaInfo -  
**
***********************************************************************/
static int getProcMediaInfo (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_PROC_MEDIA_INFO_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_PROC_MEDIA_INFO_LIST *)NULL;

	rowCount = 0;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getProcMediaInfo: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_PROC_MEDIA_INFO_LIST *)
			malloc (sizeof (V0_PROC_MEDIA_INFO_LIST))) == (V0_PROC_MEDIA_INFO_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getProcMediaInfo: Memory allocation for "
				"V0_PROC_MEDIA_INFO_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next_p = (V0_PROC_MEDIA_INFO_LIST *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBSMALLINT *)&(currPtr->process_type),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->media_type),
			qDesc->valAddr[1], qDesc->valLength[1]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->media_class),
			qDesc->valAddr[2], qDesc->valLength[2]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->media_fmt_type),
			qDesc->valAddr[3], qDesc->valLength[3]);

	 	(void) memcpy ((DBCHAR *)currPtr->quicklook_p,
			qDesc->valAddr[4], qDesc->valLength[4]);
	 	currPtr->quicklook_p[qDesc->valLength[4]] = '\0';
	 	ims_truncStr (currPtr->quicklook_p);
												  
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
			"v0_cat__getProcMediaInfo: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_PROC_MEDIA_INFO_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

}/* end of getProcMediaInfo */


/***********************************************************************
** 
** getAcctInfo -  
**
***********************************************************************/
static int getAcctInfo (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_ACCOUNT_INFO_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_ACCOUNT_INFO_LIST *)NULL;

	rowCount = 0;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getAcctInfo: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_ACCOUNT_INFO_LIST *)
			malloc (sizeof (V0_ACCOUNT_INFO_LIST))) == (V0_ACCOUNT_INFO_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getAcctInfo: Memory allocation for "
				"V0_ACCOUNT_INFO_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next_p = (V0_ACCOUNT_INFO_LIST *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBSMALLINT *)&(currPtr->resource_type),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((DBINT *)&(currPtr->rate_multiplier),
			qDesc->valAddr[1], qDesc->valLength[1]);

	 	(void) memcpy ((DBCHAR *)currPtr->op_validate_p,
			qDesc->valAddr[2], qDesc->valLength[2]);
	 	currPtr->op_validate_p[qDesc->valLength[2]] = '\0';
	 	ims_truncStr (currPtr->op_validate_p);
												  
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
			"v0_cat__getAcctInfo: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_ACCOUNT_INFO_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

}/* end of getAcctInfo */

/***********************************************************************
** 
** getLineItemCost -  
**
***********************************************************************/
static int getLineItemCost (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_VALUE_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_VALUE_LIST *)NULL;

	rowCount = 0;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getLineItemCost: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_VALUE_LIST *)
			malloc (sizeof (V0_VALUE_LIST))) == (V0_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getLineItemCost: Memory allocation for "
				"V0_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_v0_value_list (currPtr);

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */

		(void) memcpy ((DBREAL *)&(currPtr->real_value),
			qDesc->valAddr[0], qDesc->valLength[0]);

			 
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
			"v0_cat__getLineItemCost: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return(IMS_OK);

} /* end of getLineItemCost */

/***********************************************************************
** 
** getProcInfo -  
**
***********************************************************************/
static int getProcInfo (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_VALUE_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_VALUE_LIST *)NULL;

	rowCount = 0;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getProcInfo: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_VALUE_LIST *)
			malloc (sizeof (V0_VALUE_LIST))) == (V0_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getProcInfo: Memory allocation for V0_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_v0_value_list (currPtr);

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */

		(void) memcpy ((DBSMALLINT *)&(currPtr->smallint_value),
			qDesc->valAddr[0], qDesc->valLength[0]);

	 	(void) memcpy ((DBCHAR *)currPtr->char_value1,
			qDesc->valAddr[1], qDesc->valLength[1]);
	 	currPtr->char_value1[qDesc->valLength[1]] = '\0';
	 	ims_truncStr (currPtr->char_value1);
			 
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
			"v0_cat__getProcInfo: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return(IMS_OK);

} /* end of getProcInfo */

/***********************************************************************
** 
** getNewId -  
**
***********************************************************************/
static int getNewId (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_VALUE_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (V0_VALUE_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getNewId: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_VALUE_LIST *)
			malloc (sizeof (V0_VALUE_LIST))) == (V0_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getNewId: Memory allocation for V0_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_v0_value_list (currPtr);

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBINT *)&(currPtr->int_value),
			qDesc->valAddr[0], qDesc->valLength[0]);

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
			"v0_cat__getNewId: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_VALUE_LIST *)NULL)
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

}/* end of getNewId */

/***********************************************************************
** 
** getContactInfo -  
**
***********************************************************************/
static int getContactInfo (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_CONTACT_INFO *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (V0_CONTACT_INFO *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getContactInfo: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_CONTACT_INFO *)
			malloc (sizeof (V0_CONTACT_INFO))) == (V0_CONTACT_INFO *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__getContactInfo: Memory allocation for V0_CONTACT_INFO failed.");
			return (IMS_FATAL);
		}

		currPtr->next_p = (V0_CONTACT_INFO *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
	 	(void) memcpy ((DBCHAR *)currPtr->contact_name,
			qDesc->valAddr[0], qDesc->valLength[0]);
	 	currPtr->contact_name[qDesc->valLength[0]] = '\0';
	 	ims_truncStr (currPtr->contact_name);
												  
	 	(void) memcpy ((DBCHAR *)currPtr->organization,
			qDesc->valAddr[1], qDesc->valLength[1]);
	 	currPtr->organization[qDesc->valLength[1]] = '\0';
	 	ims_truncStr (currPtr->organization);
												  
	 	(void) memcpy ((DBCHAR *)currPtr->street,
			qDesc->valAddr[2], qDesc->valLength[2]);
	 	currPtr->street[qDesc->valLength[2]] = '\0';
	 	ims_truncStr (currPtr->street);
												  
	 	(void) memcpy ((DBCHAR *)currPtr->city,
			qDesc->valAddr[3], qDesc->valLength[3]);
	 	currPtr->city[qDesc->valLength[3]] = '\0';
	 	ims_truncStr (currPtr->city);
												  
	 	(void) memcpy ((DBCHAR *)currPtr->state,
			qDesc->valAddr[4], qDesc->valLength[4]);
	 	currPtr->state[qDesc->valLength[4]] = '\0';
	 	ims_truncStr (currPtr->state);
												  
	 	(void) memcpy ((DBCHAR *)currPtr->zipcode,
			qDesc->valAddr[5], qDesc->valLength[5]);
	 	currPtr->zipcode[qDesc->valLength[5]] = '\0';
	 	ims_truncStr (currPtr->zipcode);
												  
	 	(void) memcpy ((DBCHAR *)currPtr->country,
			qDesc->valAddr[6], qDesc->valLength[6]);
	 	currPtr->country[qDesc->valLength[6]] = '\0';
	 	ims_truncStr (currPtr->country);
												  
	 	(void) memcpy ((DBCHAR *)currPtr->phone,
			qDesc->valAddr[7], qDesc->valLength[7]);
	 	currPtr->phone[qDesc->valLength[7]] = '\0';
	 	ims_truncStr (currPtr->phone);
												  
	 	(void) memcpy ((DBCHAR *)currPtr->fax,
			qDesc->valAddr[8], qDesc->valLength[8]);
	 	currPtr->fax[qDesc->valLength[8]] = '\0';
	 	ims_truncStr (currPtr->fax);
												  
	 	(void) memcpy ((DBCHAR *)currPtr->email,
			qDesc->valAddr[9], qDesc->valLength[9]);
	 	currPtr->email[qDesc->valLength[9]] = '\0';
	 	ims_truncStr (currPtr->email);
												  
												  

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
			"v0_cat__getContactInfo: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_CONTACT_INFO *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	catReq->item[1] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getContactInfo */

/***********************************************************************
**
** getDatasetInfo -  
**
***********************************************************************/
static int getDatasetInfo (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_COMMENT_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_COMMENT_LIST *)NULL;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__getDatasetInfo: failed to get result row.");

			currPtr = firstPtr;
			while (currPtr != (V0_COMMENT_LIST *)NULL)
			{
				lastPtr = currPtr->next_p;
				free (currPtr);
				currPtr = lastPtr;
			}
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (V0_COMMENT_LIST *)
			malloc (sizeof (V0_COMMENT_LIST))) == (V0_COMMENT_LIST *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
			"v0_cat__getDatasetInfo: Memory allocation for V0_COMMENT_LIST failed.");

			currPtr = firstPtr;
			while (currPtr != (V0_COMMENT_LIST *)NULL)
			{
				lastPtr = currPtr->next_p;
				free (currPtr);
				currPtr = lastPtr;
			}
			return (IMS_FATAL);
		}

		currPtr->next_p = (V0_COMMENT_LIST *)NULL;

		/* a row is returend */
		rowCount += 1;

		/* copy in  the returned data */

		(void) memcpy ((DBCHAR *)currPtr->comment_info,
			qDesc->valAddr[0], qDesc->valLength[0]);
		currPtr->comment_info[qDesc->valLength[0]] = '\0';
		ims_truncStr (currPtr->comment_info);

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
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_cat__getDatasetInfo: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_COMMENT_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* item[2] points to the result rows to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return(IMS_OK);

} /*  end of getDatasetInfo */

/***********************************************************************
**
** getProfileInfo -  
**
***********************************************************************/
static int getProfileInfo (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_PROFILE_INFO *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_PROFILE_INFO *)NULL;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"v0_cat__getProfileInfo: failed to get result row.");

			currPtr = firstPtr;
			while (currPtr != (V0_PROFILE_INFO *)NULL)
			{
				lastPtr = currPtr->next_p;
				free (currPtr);
				currPtr = lastPtr;
			}
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ((currPtr = (V0_PROFILE_INFO *)
			malloc (sizeof (V0_PROFILE_INFO))) == (V0_PROFILE_INFO *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
			"v0_cat__getProfileInfo: Memory allocation for V0_PROFILE_INFO failed.");

			currPtr = firstPtr;
			while (currPtr != (V0_PROFILE_INFO *)NULL)
			{
				lastPtr = currPtr->next_p;
				free (currPtr);
				currPtr = lastPtr;
			}
			return (IMS_FATAL);
		}

		currPtr->organization[0] = '\0';
		currPtr->street[0] = '\0';
		currPtr->city[0] = '\0';
		currPtr->state[0] = '\0';
		currPtr->zipcode[0] = '\0';
		currPtr->country[0] = '\0';
		currPtr->phone[0] = '\0';
		currPtr->email[0] = '\0';
		currPtr->fax[0] = '\0';
		currPtr->next_p = (V0_PROFILE_INFO *)NULL;

		/* a row is returend */
		rowCount += 1;

		/* copy in  the returned data */

		(void) memcpy ((DBCHAR *)currPtr->organization,
			qDesc->valAddr[0], qDesc->valLength[0]);
		currPtr->organization[qDesc->valLength[0]] = '\0';
		ims_truncStr (currPtr->organization);

		(void) memcpy ((DBCHAR *)currPtr->street,
			qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->street[qDesc->valLength[1]] = '\0';
		ims_truncStr (currPtr->street);

		(void) memcpy ((DBCHAR *)currPtr->city,
			qDesc->valAddr[2], qDesc->valLength[2]);
		currPtr->city[qDesc->valLength[2]] = '\0';
		ims_truncStr (currPtr->city);

		(void) memcpy ((DBCHAR *)currPtr->state,
			qDesc->valAddr[3], qDesc->valLength[3]);
		currPtr->state[qDesc->valLength[3]] = '\0';
		ims_truncStr (currPtr->state);

		(void) memcpy ((DBCHAR *)currPtr->zipcode,
			qDesc->valAddr[4], qDesc->valLength[4]);
		currPtr->zipcode[qDesc->valLength[4]] = '\0';
		ims_truncStr (currPtr->zipcode);

		(void) memcpy ((DBCHAR *)currPtr->country,
			qDesc->valAddr[5], qDesc->valLength[5]);
		currPtr->country[qDesc->valLength[5]] = '\0';
		ims_truncStr (currPtr->country);

		(void) memcpy ((DBCHAR *)currPtr->phone,
			qDesc->valAddr[6], qDesc->valLength[6]);
		currPtr->phone[qDesc->valLength[6]] = '\0';
		ims_truncStr (currPtr->phone);

		(void) memcpy ((DBCHAR *)currPtr->email,
			qDesc->valAddr[7], qDesc->valLength[7]);
		currPtr->email[qDesc->valLength[7]] = '\0';
		ims_truncStr (currPtr->email);

		(void) memcpy ((DBCHAR *)currPtr->fax,
			qDesc->valAddr[8], qDesc->valLength[8]);
		currPtr->fax[qDesc->valLength[8]] = '\0';
		ims_truncStr (currPtr->fax);

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
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"v0_cat__getProfileInfo: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_PROFILE_INFO *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* item[2] points to the result rows to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return(IMS_OK);

} /*  end of getProfileInfo */

/***********************************************************************
** 
** verifyUser -  
**
***********************************************************************/
static int verifyUser (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_VALUE_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_VALUE_LIST *)NULL;

	rowCount = 0;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__verifyUser: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_VALUE_LIST *)
			malloc (sizeof (V0_VALUE_LIST))) == (V0_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__verifyUser: Memory allocation for V0_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_v0_value_list (currPtr);

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
	 	(void) memcpy ((DBCHAR *)currPtr->char_value1,
			qDesc->valAddr[0], qDesc->valLength[0]);
	 	currPtr->char_value1[qDesc->valLength[0]] = '\0';
	 	ims_truncStr (currPtr->char_value1);
												  
		(void) memcpy ((DBSMALLINT *)&(currPtr->smallint_value),
			qDesc->valAddr[1], qDesc->valLength[1]);

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
			"v0_cat__verifyUser: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return result counter and list to the calling routine */
	*(int *)catReq->item[1] = (int) rowCount;
	catReq->item[2] = (void *) firstPtr;

	return(IMS_OK);

} /* end of verifyUser */

/****************************************************************************
**
** verifyEntryChar - a general purpose function to verify existance of a 
**             condition by selecting against a character field against
**             the associating table.  Only rowCount is returned as the
**             result value.
**
****************************************************************************/
static int verifyEntryChar (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	V0_VALUE_LIST *firstPtr, *lastPtr, *currPtr;
	int status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	firstPtr = lastPtr = currPtr = (V0_VALUE_LIST *)NULL;

	rowCount = 0;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__verifyEntryChar: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (V0_VALUE_LIST *)
			malloc (sizeof (V0_VALUE_LIST))) == (V0_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"v0_cat__verifyEntryChar: Memory allocation for V0_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_v0_value_list (currPtr);

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
	 	(void) memcpy ((DBCHAR *)currPtr->char_value1,
			qDesc->valAddr[0], qDesc->valLength[0]);
	 	currPtr->char_value1[qDesc->valLength[0]] = '\0';
	 	ims_truncStr (currPtr->char_value1);
												  
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
			"v0_cat__verifyEntryChar: Could not reinitialize query descriptor.");

		currPtr = firstPtr;
		while (currPtr != (V0_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	/* Return counter to the calling routine and free the result rows 
		 since they are no longer needed*/
	*(int *)catReq->item[1] = (int) rowCount;

	currPtr = firstPtr;

	while (currPtr != (V0_VALUE_LIST *)NULL)
	{
		lastPtr = currPtr->next_p;
		free (currPtr);
		currPtr = lastPtr;
	}

	return(IMS_OK);

} /* end of verifyEntryChar */

/****************************************************************************
**
** verifyVal - receives a verification query, and returns an error status
**             if the verification failed
**
****************************************************************************/
static int verifyVal (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	int             status;

	qDesc = catReq->qDesc;

	qDesc->cmd =  (char *)catReq->item[0];

	if ( (status = execCmd (qDesc)) < IMS_OK)
	{
		return(status);
	}

	return (IMS_OK);

} /* end of verifyVal */

/******************************************************************************
**
** insertRow -  a generic routine to insert one row to the database.  Only a
**              status is returned from this routine.
**
******************************************************************************/

static int insertRow (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int            status;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	if ( (status = execCmd(qDesc)) < IMS_OK)
	{
		return(status);
	}

	if (IMS_AFFECTED (catReq->qDesc) < 1)
	{
		(void) ims_msg (msgDesc, IMS_FATAL, 
				 "Failed to insert/update a row. The query was ==> %s", qDesc->cmd);
		return (IMS_FATAL);
	}

	return(IMS_OK);
} /* end of insertRow */



/****************************************************************************
**
** getV0OrderLock ()
**
** get a lock for order_item table.
**
****************************************************************************/
static int getV0OrderLock (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT  *msgDesc;
	int             status;

	qDesc  =  catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/*
	** execute stored procedure get_v0_order_lock
	*/
	(void) sprintf (qDesc->cmd, "exec get_v0_order_lock");

	if ( (status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status, 
						 "v0_cat__getV0OrderLock: could not get v0_order_lock.");
		return (status);		
	}

	return (IMS_OK);

} /* end of getOrderLock */

/****************************************************************************
**
** getAccountLock ()
**
** get a lock for account table.
**
****************************************************************************/
static int getAccountLock (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT  *msgDesc;
	int             status;

	qDesc  =  catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/*
	** execute stored procedure get_account_lock
	*/
	(void) sprintf (qDesc->cmd, "exec get_account_lock");

	if ( (status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status, 
					 "v0_cat__getAccountLock: could not get v0_account_lock.");
		return (status);		
	}

	return (IMS_OK);

} /* end of getAccountLock */

/****************************************************************************
**
** beginTransaction ()
**
** Open a transaction.
**
****************************************************************************/
static int beginTransaction (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int            status;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	(void) sprintf (qDesc->cmd, "begin transaction");

	if ( (status = execCmd (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"v0_cat__beginTransaction: Could not begin transaction.");
		return(status);
	}

	return(IMS_OK);
}/* end of bebigTransaction */

/******************************************************************************
**
** commitTransaction ()
**
** Commit the transaction opened by openTransaction.
**
******************************************************************************/

static int commitTransaction (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int            status;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	(void) sprintf (qDesc->cmd, "commit transaction");

	if ( (status = execCmd(qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"v0_cat__commitTransaction: Could not commit transaction.");
		return(status);
	}
	return(IMS_OK);
} /* end of commitTransaction */

/******************************************************************************
**
** rollbackTransaction ()
**
** Rollback the transaction opened by openTransaction.
**
******************************************************************************/

static int rollbackTransaction (V0_CAT_STRUCT *catReq)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	int            status;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/*
	** Rollback this transaction.
	*/
	(void) sprintf (qDesc->cmd, "rollback transaction");

	if ( (status = execCmd(qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"v0_cat__rollbackTransaction: Could not rollback transaction.");
		return(status);
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
/***************************************************************************
**
** init_v0_value_list ()
**
****************************************************************************/

static int init_v0_value_list (V0_VALUE_LIST *list)
{
	list->char_value1[0] = '\0';
	list->char_value2[0] = '\0';
	list->char_value3[0] = '\0';
	list->flt8_value     = 0.0;
	list->real_value     = 0.0;
	list->int_value      = 0;
	list->smallint_value = 0;
	list->tinyint_value  = 0;
	list->next_p = (V0_VALUE_LIST *)NULL;

	return (IMS_OK);
}
