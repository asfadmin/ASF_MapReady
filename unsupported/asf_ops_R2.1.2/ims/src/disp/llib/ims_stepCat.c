static char *sccs = "@(#)ims_stepCat.c	5.5  01/14/97";
/******************************************************************************
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
** File:	   ims_stepCat.c
**
** Function: Catalog database access facility for ims_step.
**
** Creator:  Julie Wang
**
** Date:     Aug 3, 1995
**
** History:  
**            1/14/97   jwnag   changed GETSTR5 back to GETSTR4
**
**           12/12/96   jwang   changed GETSTR4 to GETSTR5
**
**           02/27/96   jwang   R1Bprime preliminary.
**
*****************************************************************************/

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


/*
** Definition of local constants
*/
#define BUF_SIZE 1024   /* Maximum size of the cmd buffer */
 
/*
** Global variables
*/
char cmdBuf[BUF_SIZE];


/*
** Local Functions
*/
static int getItemInfo  (V0_CAT_STRUCT *);
static int getNextStep  (V0_CAT_STRUCT *);
static int getPhotoType (V0_CAT_STRUCT *);
static int getPPSBasic  (V0_CAT_STRUCT *);
static int getGnulTbl   (V0_CAT_STRUCT *);
static int getRFCProc   (V0_CAT_STRUCT *);
static int getRFCGnul   (V0_CAT_STRUCT *);
static int getStr3      (V0_CAT_STRUCT *);
static int getTSRGnul   (V0_CAT_STRUCT *);
static int getCurrItemInfo(V0_CAT_STRUCT *);
static int getItemCost(V0_CAT_STRUCT *);
static int getStr     (V0_CAT_STRUCT *);
static int getStr4    (V0_CAT_STRUCT *);
static int getSmallInt (V0_CAT_STRUCT *);
static int execCmd (IMS_QI_DESC_OBJ *);
static int processRetStatus (IMS_QI_DESC_OBJ *);
static int init_step_value_list (STEP_VALUE_LIST *);

/******************************************************************************
**
** step_cat ()
**
** Main function handling catalog queries.
**
******************************************************************************/

int step_cat (V0_CAT_STRUCT *catReq, STEP_CAT_EVENT event)
{
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc;
	char     msgbuf[IMS_COL512_LEN+1];
	int      status;

	msgDesc = catReq->msgDesc;
	cmdBuf[0] = '\0';

	/*
	** Make sure the caller has a database connection opened, which is required
	** for this routine.
	*/
	if ( catReq->qDesc == (IMS_QI_DESC_OBJ *)NULL )
	{
		msgbuf[0] = '\0';
		(void)sprintf (msgbuf, "step_cat: Database connection has not been opened.");
		(void)ims_msg (msgDesc, IMS_FATAL, msgbuf);
		return (IMS_FATAL);
		
	}

	qDesc = catReq->qDesc;
	qDesc->cmd = cmdBuf;

	/*
	** Now, let's do our 'catalog' business according to the type of event
	** passed into the function.  
	*/
	switch (event)
	{

		case STEP_GETITEMINFO: 
			status = getItemInfo (catReq);
		break;

		case STEP_GETNEXTSTEP: 
			status = getNextStep (catReq);
		break;

		case STEP_GETPHOTOTYPE: 
			status = getPhotoType (catReq);
		break;

		case STEP_GETPPSBASIC: 
			status = getPPSBasic (catReq);
		break;

		case STEP_GETGNULTBL: 
			status = getGnulTbl (catReq);
		break;

		case STEP_GETRFCPROC: 
			status = getRFCProc (catReq);
		break;

		case STEP_GETRFCGNUL: 
			status = getRFCGnul (catReq);
		break;

		case STEP_GETSTR3: 
			status = getStr3 (catReq);
		break;

		case STEP_GETTSRGNUL: 
			status = getTSRGnul (catReq);
		break;

		case STEP_GETCURRITEMINFO: 
			status = getCurrItemInfo (catReq);
		break;

		case STEP_GETITEMCOST: 
			status = getItemCost (catReq);
		break;

		case STEP_GETSTR: 
			status = getStr (catReq);
		break;

		case STEP_GETSTR4: 
			status = getStr4 (catReq);
		break;

		case STEP_GETSMALLINT: 
			status = getSmallInt (catReq);
		break;

		default:
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat: Invalid catalog event passed to step_cat.");
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
				"step_cat: Could not reinitialize query descriptor.");
			return(IMS_FATAL);
		}
	}

	/*
	** Return with the appropriate status
	*/
	if (status < IMS_OK) return (status);


	return (IMS_OK);

} /* end of step_cat */

/***********************************************************************
** 
** getItemInfo -  
**
***********************************************************************/
static int getItemInfo (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	ITEM_INFO_LIST   *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (ITEM_INFO_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, status,
				"step_cat__getItemInfo: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (ITEM_INFO_LIST *)
			malloc (sizeof (ITEM_INFO_LIST))) == (ITEM_INFO_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getItemInfo: Memory allocation for ITEM_INFO_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next_p = (ITEM_INFO_LIST *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBSMALLINT *)&(currPtr->order_item_type),
		 qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->media_class),
		 qDesc->valAddr[1], qDesc->valLength[1]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->media_type),
		 qDesc->valAddr[2], qDesc->valLength[2]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->process_type),
		 qDesc->valAddr[3], qDesc->valLength[3]);

		(void) memcpy ((DBCHAR *)currPtr->step_name,
		   qDesc->valAddr[4], qDesc->valLength[4]);
		currPtr->step_name[qDesc->valLength[4]] = '\0';
		ims_truncStr (currPtr->step_name);

		(void) memcpy ((DBSMALLINT *)&(currPtr->step_sequence),
		 qDesc->valAddr[5], qDesc->valLength[5]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->status),
		 qDesc->valAddr[6], qDesc->valLength[6]);

		(void) memcpy ((DBCHAR *)currPtr->step_started_p,
		   qDesc->valAddr[7], qDesc->valLength[7]);
		currPtr->step_started_p[qDesc->valLength[7]] = '\0';
		ims_truncStr (currPtr->step_started_p);

		(void) memcpy ((DBSMALLINT *)&(currPtr->quantity),
		 qDesc->valAddr[8], qDesc->valLength[8]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->priority),
		 qDesc->valAddr[9], qDesc->valLength[9]);

		(void) memcpy ((DBCHAR *)currPtr->quicklook_p,
		   qDesc->valAddr[10], qDesc->valLength[10]);
		currPtr->quicklook_p[qDesc->valLength[10]] = '\0';
		ims_truncStr (currPtr->quicklook_p);

		(void) memcpy ((DBREAL *)&(currPtr->cost),
		 qDesc->valAddr[11], qDesc->valLength[11]);

		(void) memcpy ((DBCHAR *)currPtr->account_id,
		   qDesc->valAddr[12], qDesc->valLength[12]);
		currPtr->account_id[qDesc->valLength[12]] = '\0';
		ims_truncStr (currPtr->account_id);

		(void) memcpy ((DBSMALLINT *)&(currPtr->process_status),
		 qDesc->valAddr[13], qDesc->valLength[13]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->media_fmt_type),
		 qDesc->valAddr[14], qDesc->valLength[14]);

		(void) memcpy ((DBCHAR *)currPtr->v0_process_type,
		   qDesc->valAddr[15], qDesc->valLength[15]);
		currPtr->v0_process_type[qDesc->valLength[15]] = '\0';
		ims_truncStr (currPtr->v0_process_type);


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
			"step_cat__getItemInfo: Could not reinitialize query descriptor.");

		currPtr = firstPtr;

		while (currPtr != (ITEM_INFO_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	if (rowCount == 0)
	{
		return (IMS_ERROR);
	}
	else if (rowCount >= 2)
	{
		currPtr = firstPtr;

		while (currPtr != (ITEM_INFO_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return (IMS_ERROR);
	}

	*(int *)catReq->item[1] = (int)rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getItemInfo */

/***********************************************************************
** 
** getNextStep -  
**
***********************************************************************/
static int getNextStep (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	NEXT_STEP_INFO   *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (NEXT_STEP_INFO *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getNextStep: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (NEXT_STEP_INFO *)
			malloc (sizeof (NEXT_STEP_INFO))) == (NEXT_STEP_INFO *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getNextStep: Memory allocation for NEXT_STEP_INFO failed.");
			return (IMS_FATAL);
		}

		currPtr->next_p = (NEXT_STEP_INFO *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBSMALLINT *)&(currPtr->step_sequence),
		 qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((DBCHAR *)currPtr->step_name,
		   qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->step_name[qDesc->valLength[1]] = '\0'; 
		ims_truncStr (currPtr->step_name);

		(void) memcpy ((DBSMALLINT *)&(currPtr->start_status),
		 qDesc->valAddr[2], qDesc->valLength[2]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->end_status),
		 qDesc->valAddr[3], qDesc->valLength[3]);


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
			"step_cat__getNextStep: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (NEXT_STEP_INFO *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	*(int *)catReq->item[1] = (int)rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getNextStep */
	
/***********************************************************************
** 
** getPhotoType -  
**
***********************************************************************/
static int getPhotoType (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	STEP_VALUE_LIST  *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (STEP_VALUE_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getPhotoType: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (STEP_VALUE_LIST *)
			malloc (sizeof (STEP_VALUE_LIST))) == (STEP_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getPhotoType: Memory allocation for STEP_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_step_value_list (currPtr);

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
			"step_cat__getPhotoType: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	if (rowCount == 0)
	{
		return (IMS_ERROR);
	}
	else if (rowCount >= 2)
	{
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return (IMS_ERROR);
	}

	*(int *)catReq->item[1] = (int)rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getPhotoType */

/***********************************************************************
** 
** getPPSBasic -  
**
***********************************************************************/
static int getPPSBasic (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	PPS_BASIC_INFO   *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (PPS_BASIC_INFO *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getPPSBasic: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (PPS_BASIC_INFO *)
			malloc (sizeof (PPS_BASIC_INFO))) == (PPS_BASIC_INFO *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getPPSBasic: Memory allocation for PPS_BASIC_INFO failed.");
			return (IMS_FATAL);
		}

		currPtr->next_p = (PPS_BASIC_INFO *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBSMALLINT *)&(currPtr->dataset_idx),
		 qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((DBINT *)&(currPtr->granule_idx),
		 qDesc->valAddr[1], qDesc->valLength[1]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->output_format),
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
			"step_cat__getPPSBasic: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (PPS_BASIC_INFO *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	if (rowCount == 0)
	{
		return (IMS_ERROR);
	}
	else if (rowCount >= 2)
	{
		currPtr = firstPtr;

		while (currPtr != (PPS_BASIC_INFO *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return (IMS_ERROR);
	}

	*(int *)catReq->item[1] = (int)rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getPPSBasic */
	
/***********************************************************************
** 
** getGnulTbl -  
**
***********************************************************************/
static int getGnulTbl (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	STEP_VALUE_LIST   *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (STEP_VALUE_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getGnulTbl: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (STEP_VALUE_LIST *)
			malloc (sizeof (STEP_VALUE_LIST))) == (STEP_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getGnulTbl: Memory allocation for STEP_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_step_value_list (currPtr);

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
			"step_cat__getGnulTbl: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	if (rowCount == 0)
	{
		return (IMS_ERROR);
	}
	else if (rowCount >= 2)
	{
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return (IMS_ERROR);
	}

	*(int *)catReq->item[1] = (int)rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getGnulTbl */

/***********************************************************************
** 
** getRFCProc -  
**
***********************************************************************/
static int getRFCProc (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	RFC_PROCESS_LIST   *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (RFC_PROCESS_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getRFCProc: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (RFC_PROCESS_LIST *)
			malloc (sizeof (RFC_PROCESS_LIST))) == (RFC_PROCESS_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getRFCProc: Memory allocation for RFC_PROCESS_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next_p = (RFC_PROCESS_LIST *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBCHAR *)currPtr->product_type,
		   qDesc->valAddr[0], qDesc->valLength[0]);
		currPtr->product_type[qDesc->valLength[0]] = '\0'; 
		ims_truncStr (currPtr->product_type);

		(void) memcpy ((DBFLT8 *)&(currPtr->pixel_spacing),
			qDesc->valAddr[1], qDesc->valLength[1]);

		(void) memcpy ((DBCHAR *)currPtr->projection,
		   qDesc->valAddr[2], qDesc->valLength[2]);
		currPtr->projection[qDesc->valLength[2]] = '\0'; 
		ims_truncStr (currPtr->projection);

		(void) memcpy ((DBINT *)&(currPtr->processing_gain),
			qDesc->valAddr[3], qDesc->valLength[3]);

		(void) memcpy ((DBFLT8 *)&(currPtr->avg_terrain_ht),
			qDesc->valAddr[4], qDesc->valLength[4]);

		(void) memcpy ((DBCHAR *)currPtr->deskew_p,
		   qDesc->valAddr[5], qDesc->valLength[5]);
		currPtr->deskew_p[qDesc->valLength[5]] = '\0'; 
		ims_truncStr (currPtr->deskew_p);

		(void) memcpy ((DBREAL *)&(currPtr->ps_reference_lat),
			qDesc->valAddr[6], qDesc->valLength[6]);

		(void) memcpy ((DBREAL *)&(currPtr->ps_reference_lon),
			qDesc->valAddr[7], qDesc->valLength[7]);

		(void) memcpy ((DBINT *)&(currPtr->utm_zone),
			qDesc->valAddr[8], qDesc->valLength[8]);

		(void) memcpy ((DBCHAR *)currPtr->terrain_correct_p,
		   qDesc->valAddr[9], qDesc->valLength[9]);
		currPtr->terrain_correct_p[qDesc->valLength[9]] = '\0'; 
		ims_truncStr (currPtr->terrain_correct_p);

		(void) memcpy ((DBREAL *)&(currPtr->lambert_lat_n),
		 qDesc->valAddr[10], qDesc->valLength[10]);

		(void) memcpy ((DBREAL *)&(currPtr->lambert_lat_s),
		 qDesc->valAddr[11], qDesc->valLength[11]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->subframe_id),
		 qDesc->valAddr[12], qDesc->valLength[12]);

		(void) memcpy ((DBCHAR *)currPtr->compensation_p,
		   qDesc->valAddr[13], qDesc->valLength[13]);
		currPtr->compensation_p[qDesc->valLength[13]] = '\0'; 
		ims_truncStr (currPtr->compensation_p);

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
			"step_cat__getRFCProc: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (RFC_PROCESS_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	if (rowCount == 0)
	{
		return (IMS_ERROR);
	}
	else if (rowCount >= 2)
	{

		currPtr = firstPtr;

		while (currPtr != (RFC_PROCESS_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return (IMS_ERROR);
	}

	*(int *)catReq->item[1] = (int)rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getRFCProc */

/***********************************************************************
** 
** getRFCGnul -  
**
***********************************************************************/
static int getRFCGnul (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	RFC_GNUL_LIST   *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (RFC_GNUL_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getRFCGnul: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (RFC_GNUL_LIST *)
			malloc (sizeof (RFC_GNUL_LIST))) == (RFC_GNUL_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getRFCGnul: Memory allocation for RFC_GNUL_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next_p = (RFC_GNUL_LIST *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBCHAR *)currPtr->platform,
		   qDesc->valAddr[0], qDesc->valLength[0]);
		currPtr->platform[qDesc->valLength[0]] = '\0'; 
		ims_truncStr (currPtr->platform);

		(void) memcpy ((DBCHAR *)currPtr->sensor,
		   qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->sensor[qDesc->valLength[1]] = '\0'; 
		ims_truncStr (currPtr->sensor);

		(void) memcpy ((DBINT *)&(currPtr->rev),
			qDesc->valAddr[2], qDesc->valLength[2]);

		(void) memcpy ((DBCHAR *)currPtr->mode,
		   qDesc->valAddr[3], qDesc->valLength[3]);
		currPtr->mode[qDesc->valLength[3]] = '\0'; 
		ims_truncStr (currPtr->mode);

		(void) memcpy ((DBSMALLINT *)&(currPtr->sequence),
			qDesc->valAddr[4], qDesc->valLength[4]);

		(void) memcpy ((DBSMALLINT *)&(currPtr->frame_id),
			qDesc->valAddr[5], qDesc->valLength[5]);

		(void) memcpy ((DBCHAR *)currPtr->frame_mode,
		   qDesc->valAddr[6], qDesc->valLength[6]);
		currPtr->frame_mode[qDesc->valLength[6]] = '\0'; 
		ims_truncStr (currPtr->frame_mode);

		(void) memcpy ((DBCHAR *)currPtr->media_id,
		   qDesc->valAddr[7], qDesc->valLength[7]);
		currPtr->media_id[qDesc->valLength[7]] = '\0'; 
		ims_truncStr (currPtr->media_id);

		(void) memcpy ((DBCHAR *)currPtr->station_id,
		   qDesc->valAddr[8], qDesc->valLength[8]);
		currPtr->station_id[qDesc->valLength[8]] = '\0'; 
		ims_truncStr (currPtr->station_id);

		(void) memcpy ((DBCHAR *)currPtr->start_time,
		   qDesc->valAddr[9], qDesc->valLength[9]);
		currPtr->start_time[qDesc->valLength[9]] = '\0'; 
		ims_truncStr (currPtr->start_time);

		(void) memcpy ((DBCHAR *)currPtr->end_time,
		   qDesc->valAddr[10], qDesc->valLength[10]);
		currPtr->end_time[qDesc->valLength[10]] = '\0'; 
		ims_truncStr (currPtr->end_time);

		(void) memcpy ((DBCHAR *)currPtr->center_time,
		   qDesc->valAddr[11], qDesc->valLength[11]);
		currPtr->center_time[qDesc->valLength[11]] = '\0'; 
		ims_truncStr (currPtr->center_time);

		(void) memcpy ((DBCHAR *)currPtr->activity_id,
		   qDesc->valAddr[12], qDesc->valLength[12]);
		currPtr->activity_id[qDesc->valLength[12]] = '\0'; 
		ims_truncStr (currPtr->activity_id);

		(void) memcpy ((DBCHAR *)currPtr->scan_results_file,
		   qDesc->valAddr[13], qDesc->valLength[13]);
		currPtr->scan_results_file[qDesc->valLength[13]] = '\0'; 
		ims_truncStr (currPtr->scan_results_file);

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
			"step_cat__getRFCGnul: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (RFC_GNUL_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	if (rowCount == 0)
	{
		return (IMS_ERROR);
	}
	else if (rowCount >= 2)
	{
		currPtr = firstPtr;

		while (currPtr != (RFC_GNUL_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return (IMS_ERROR);
	}

	*(int *)catReq->item[1] = (int)rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getRFCGnul */

/***********************************************************************
** 
** getTSRGnul -  
**
***********************************************************************/
static int getTSRGnul (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	TSR_INFO_LIST    *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];

	firstPtr = lastPtr = currPtr = (TSR_INFO_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getTSRGnul: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (TSR_INFO_LIST *)
			malloc (sizeof (TSR_INFO_LIST))) == (TSR_INFO_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getTSRGnul: Memory allocation for TSR_INFO_LIST failed.");
			return (IMS_FATAL);
		}

		currPtr->next_p = (TSR_INFO_LIST *)NULL;

		/* a row is returned */
		rowCount ++;

		/* copy in  the returned data */
			 
		(void) memcpy ((DBCHAR *)currPtr->platform,
		   qDesc->valAddr[0], qDesc->valLength[0]);
		currPtr->platform[qDesc->valLength[0]] = '\0'; 
		ims_truncStr (currPtr->platform);

		(void) memcpy ((DBCHAR *)currPtr->sensor,
		   qDesc->valAddr[1], qDesc->valLength[1]);
		currPtr->sensor[qDesc->valLength[1]] = '\0'; 
		ims_truncStr (currPtr->sensor);

		(void) memcpy ((DBINT *)&(currPtr->rev),
			qDesc->valAddr[2], qDesc->valLength[2]);

		(void) memcpy ((DBCHAR *)currPtr->mode,
		   qDesc->valAddr[3], qDesc->valLength[3]);
		currPtr->mode[qDesc->valLength[3]] = '\0'; 
		ims_truncStr (currPtr->mode);

		(void) memcpy ((DBSMALLINT *)&(currPtr->sequence),
			qDesc->valAddr[4], qDesc->valLength[4]);

		(void) memcpy ((DBCHAR *)currPtr->activity_id,
		   qDesc->valAddr[5], qDesc->valLength[5]);
		currPtr->activity_id[qDesc->valLength[5]] = '\0'; 
		ims_truncStr (currPtr->activity_id);

		(void) memcpy ((DBCHAR *)currPtr->frame_mode,
		   qDesc->valAddr[6], qDesc->valLength[6]);
		currPtr->frame_mode[qDesc->valLength[6]] = '\0'; 
		ims_truncStr (currPtr->frame_mode);

		(void) memcpy ((DBCHAR *)currPtr->media_type,
		   qDesc->valAddr[7], qDesc->valLength[7]);
		currPtr->media_type[qDesc->valLength[7]] = '\0'; 
		ims_truncStr (currPtr->media_type);

		(void) memcpy ((DBCHAR *)currPtr->media_id,
		   qDesc->valAddr[8], qDesc->valLength[8]);
		currPtr->media_id[qDesc->valLength[8]] = '\0'; 
		ims_truncStr (currPtr->media_id);

		(void) memcpy ((DBINT *)&(currPtr->start_address),
			qDesc->valAddr[9], qDesc->valLength[9]);

		(void) memcpy ((DBINT *)&(currPtr->end_address),
			qDesc->valAddr[10], qDesc->valLength[10]);

		(void) memcpy ((DBCHAR *)currPtr->recorder_id,
		   qDesc->valAddr[11], qDesc->valLength[11]);
		currPtr->recorder_id[qDesc->valLength[11]] = '\0'; 
		ims_truncStr (currPtr->recorder_id);

		(void) memcpy ((DBCHAR *)currPtr->station_id,
		   qDesc->valAddr[12], qDesc->valLength[12]);
		currPtr->station_id[qDesc->valLength[12]] = '\0'; 
		ims_truncStr (currPtr->station_id);

		(void) memcpy ((DBCHAR *)currPtr->site_name,
		   qDesc->valAddr[13], qDesc->valLength[13]);
		currPtr->site_name[qDesc->valLength[13]] = '\0'; 
		ims_truncStr (currPtr->site_name);

		(void) memcpy ((DBCHAR *)currPtr->start_time,
		   qDesc->valAddr[14], qDesc->valLength[14]);
		currPtr->start_time[qDesc->valLength[14]] = '\0'; 
		ims_truncStr (currPtr->start_time);

		(void) memcpy ((DBCHAR *)currPtr->end_time,
		   qDesc->valAddr[15], qDesc->valLength[15]);
		currPtr->end_time[qDesc->valLength[15]] = '\0'; 
		ims_truncStr (currPtr->end_time);

		(void) memcpy ((DBCHAR *)currPtr->media_location,
		   qDesc->valAddr[16], qDesc->valLength[16]);
		currPtr->media_location[qDesc->valLength[16]] = '\0'; 
		ims_truncStr (currPtr->media_location);

		(void) memcpy ((DBCHAR *)currPtr->data_direction,
		   qDesc->valAddr[17], qDesc->valLength[17]);
		currPtr->data_direction[qDesc->valLength[17]] = '\0'; 
		ims_truncStr (currPtr->data_direction);

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
			"step_cat__getTSRGnul: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (TSR_INFO_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	if (rowCount == 0)
	{
		return (IMS_ERROR);
	}
	else if (rowCount >= 2)
	{
		currPtr = firstPtr;

		while (currPtr != (TSR_INFO_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return (IMS_ERROR);
	}

	*(int *)catReq->item[1] = (int)rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getTSRGnul */
	
/***********************************************************************
** 
** getStr -  
**
***********************************************************************/
static int getStr (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	STEP_VALUE_LIST   *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (STEP_VALUE_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getStr: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (STEP_VALUE_LIST *)
			malloc (sizeof (STEP_VALUE_LIST))) == (STEP_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getStr: Memory allocation for STEP_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_step_value_list (currPtr);

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
			"step_cat__getStr: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	if (rowCount == 0)
	{
		return (IMS_ERROR);
	}
	else if (rowCount >= 2)
	{
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return (IMS_ERROR);
	}

	*(int *)catReq->item[1] = (int)rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getStr */
	
/***********************************************************************
** 
** getStr3 -  
**
***********************************************************************/
static int getStr3 (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	STEP_VALUE_LIST   *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (STEP_VALUE_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getStr3: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (STEP_VALUE_LIST *)
			malloc (sizeof (STEP_VALUE_LIST))) == (STEP_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getStr3: Memory allocation for STEP_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_step_value_list (currPtr);

		/* a row is returned */
		rowCount ++;

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
	** Re-initialize query descriptor for next command, but do not 
	** cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"step_cat__getStr3: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	if (rowCount == 0)
	{
		return (IMS_ERROR);
	}
	else if (rowCount >= 2)
	{
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return (IMS_ERROR);
	}

	*(int *)catReq->item[1] = (int)rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getStr3 */
	
/***********************************************************************
** 
** getStr4 -  
**
***********************************************************************/
static int getStr4 (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	STEP_VALUE_LIST   *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (STEP_VALUE_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getStr4: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (STEP_VALUE_LIST *)
			malloc (sizeof (STEP_VALUE_LIST))) == (STEP_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getStr4: Memory allocation for STEP_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_step_value_list (currPtr);

		/* a row is returned */
		rowCount ++;

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

		(void) memcpy ((DBCHAR *)currPtr->char_value4,
		   qDesc->valAddr[3], qDesc->valLength[3]);
		currPtr->char_value4[qDesc->valLength[3]] = '\0'; 
		ims_truncStr (currPtr->char_value4);

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
			"step_cat__getStr4: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	if (rowCount == 0)
	{
		return (IMS_ERROR);
	}
	else if (rowCount >= 2)
	{
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return (IMS_ERROR);
	}

	*(int *)catReq->item[1] = (int)rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getStr4 */

/***********************************************************************
** 
** getSmallInt -  
**
***********************************************************************/
static int getSmallInt (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	STEP_VALUE_LIST   *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (STEP_VALUE_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getSmallInt: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (STEP_VALUE_LIST *)
			malloc (sizeof (STEP_VALUE_LIST))) == (STEP_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getSmallInt: Memory allocation for STEP_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_step_value_list (currPtr);

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
			"step_cat__getSmallInt: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	if (rowCount == 0)
	{
		return (IMS_ERROR);
	}
	else if (rowCount >= 2)
	{
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return (IMS_ERROR);
	}

	*(int *)catReq->item[1] = (int)rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getSmallInt */

/***********************************************************************
** 
** getCurrItemInfo -  
**
***********************************************************************/
static int getCurrItemInfo (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	STEP_VALUE_LIST   *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (STEP_VALUE_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getCurrItemInfo: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (STEP_VALUE_LIST *)
			malloc (sizeof (STEP_VALUE_LIST))) == (STEP_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getCurrItemInfo: Memory allocation for STEP_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_step_value_list (currPtr);

		/* a row is returned */
		rowCount ++;

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
	** Re-initialize query descriptor for next command, but do not 
	** cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"step_cat__getCurrItemInfo: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	if (rowCount == 0)
	{
		return (IMS_ERROR);
	}
	else if (rowCount >= 2)
	{
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return (IMS_ERROR);
	}

	*(int *)catReq->item[1] = (int)rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /*  end of getCurrItemInfo */
	
/***********************************************************************
** 
** getItemCost -  
**
***********************************************************************/
static int getItemCost (V0_CAT_STRUCT *catReq)
{

	IMS_QI_DESC_OBJ  *qDesc;
	IMS_MSG_STRUCT   *msgDesc;
	STEP_VALUE_LIST   *firstPtr, *lastPtr, *currPtr;
	int              status, rowCount;

	qDesc = catReq->qDesc;
	msgDesc = catReq->msgDesc;

	/* item[0] points to the sql statement */
	qDesc->cmd =  (char *)catReq->item[0];


	firstPtr = lastPtr = currPtr = (STEP_VALUE_LIST *)NULL;

	rowCount = 0;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = cmdBuf;
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getItemCost: failed to get the result row.");
			return (status);
		}

		if (status == IMS_ENDOFQUERY) continue;

		if ( (currPtr = (STEP_VALUE_LIST *)
			malloc (sizeof (STEP_VALUE_LIST))) == (STEP_VALUE_LIST *)NULL )
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"step_cat__getItemCost: Memory allocation for STEP_VALUE_LIST failed.");
			return (IMS_FATAL);
		}

		(void) init_step_value_list (currPtr);

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
			"step_cat__getItemCost: Could not reinitialize query descriptor.");
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return(IMS_FATAL);
	}

	if (rowCount == 0)
	{
		return (IMS_ERROR);
	}
	else if (rowCount >= 2)
	{
		currPtr = firstPtr;

		while (currPtr != (STEP_VALUE_LIST *)NULL)
		{
			lastPtr = currPtr->next_p;
			free (currPtr);
			currPtr = lastPtr;
		}
		return (IMS_ERROR);
	}

	*(int *)catReq->item[1] = (int)rowCount;
	catReq->item[2] = (void *) firstPtr;

	return (IMS_OK); 

} /* end of getItemCost */

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

} /* end of execCmd */

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

} /* end of processRetStatus */

static int init_step_value_list (STEP_VALUE_LIST *list)
{
	
	list->smallint_value = 0;
	list->real_value = 0.0;
	list->char_value1[0] = '\0';
	list->char_value2[0] = '\0';
	list->char_value3[0] = '\0';
	list->char_value4[0] = '\0';
	list->char_value5[0] = '\0';
	list->next_p = (STEP_VALUE_LIST *)NULL;

	return (IMS_OK);

}/* end of init_step_value_list */
