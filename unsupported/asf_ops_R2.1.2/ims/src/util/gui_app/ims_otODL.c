static char *sccs = "@(#)ims_otODL.c	1.9 11/24/97";
/*****************************************************************************
**
** File:    ims_otODL.c
**
** Function: 
**
** Author: Dan Crichton
**
** Date:    3/13/96
**
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <sys/signal.h>

#include <X11/Intrinsic.h>
#include <Xm/MainW.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/RowColumn.h>


#include <ims_dbms.h>
#include <ims_query.h> 
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <ims_cmd.h>
#include <ims_archive.h>
#include <syslog.h>
#include <odldef.h>
#include <ims_odl.h>

#include <ims_pmfTool.h>
#include <ims_keyword.h>

int openConnection (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ **);
int closeConnection (IMS_QI_DESC_OBJ *);


/*******************************************************************
** 
** ims_otBldCmnHdr
**
*******************************************************************/
int ims_otBldCmnHdr(
	IMS_MSG_STRUCT *msgDesc,
	Widget work_w,
	IMS_ODL_TREE **cmn_hdr)
{
	IMS_NUMERIC_DATE dateStruct;
	char num_recs_str[15];
	char time[IMS_DATETIME_LEN+1];
	IMS_OT_KEYWORD *data;


	if (ims_getCurrentDate(msgDesc, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not get current date");
		return(IMS_ERROR);
	}

	ims_numericDateToIMSA(&dateStruct, time);

	(void) ims_addODLObject(msgDesc, NULL, cmn_hdr, 
				"COMMON_HEADER", FALSE, IMS_OBJECT); 

				
	(void) ims_addODLKeyword(msgDesc, *cmn_hdr, "TIME", TV_DATE_TIME,
		time);

	data = (IMS_OT_KEYWORD *) malloc(sizeof(IMS_OT_KEYWORD));
	data->keyword = (char *) malloc(5);
	strcpy(data->keyword, "TIME");
	data->keyword_idx = -1;
	data->max_len = 50;
	(void) ims_attachODLData(*cmn_hdr, (char *) data);

	(void) ims_addODLKeyword(msgDesc, *cmn_hdr, "MSG_TYPE", TV_STRING,
		"");
	data = (IMS_OT_KEYWORD *) malloc(sizeof(IMS_OT_KEYWORD));
	data->keyword = (char *) malloc(9);
	strcpy(data->keyword, "MSG_TYPE");
	data->keyword_idx = -1;
	data->max_len = 50;
	(void) ims_attachODLData(*cmn_hdr, (char *) data);

	(void) ims_addODLKeyword(msgDesc, *cmn_hdr, "DESTINATION", TV_STRING,
		"IMS");
	data = (IMS_OT_KEYWORD *) malloc(sizeof(IMS_OT_KEYWORD));
	data->keyword = (char *) malloc(12);
	strcpy(data->keyword, "DESTINATION");
	data->keyword_idx = -1;
	data->max_len = 50;
	(void) ims_attachODLData(*cmn_hdr, (char *) data);

	(void) ims_addODLKeyword(msgDesc, *cmn_hdr, "SOURCE", TV_STRING,
		"IMS");
	data = (IMS_OT_KEYWORD *) malloc(sizeof(IMS_OT_KEYWORD));
	data->keyword = (char *) malloc(7);
	data->keyword_idx = -1;
	strcpy(data->keyword, "SOURCE");
	data->max_len = 50;
	(void) ims_attachODLData(*cmn_hdr, (char *) data);

	sprintf(num_recs_str, "%d", 1);

	(void) ims_addODLKeyword(msgDesc, *cmn_hdr, "NUMBER_OF_RECORDS", TV_INTEGER,
		num_recs_str);
	data = (IMS_OT_KEYWORD *) malloc(sizeof(IMS_OT_KEYWORD));
	data->keyword = (char *) malloc(18);
	data->keyword_idx = -1;
	strcpy(data->keyword, "NUMBER_OF_RECORDS");
	data->max_len = 50;
	(void) ims_attachODLData(*cmn_hdr, (char *) data);


#if 0
	(void) ims_otBldKwList(msgDesc, *cmn_hdr, "JERS-1", "SAR",
		"JERS-1 SAR FRAMES");

	(void) ims_otShowObject(msgDesc, work_w, *cmn_hdr, 0);
#endif

	ims_ot_tree = *cmn_hdr;

	return(IMS_OK);
}

/*******************************************************************
** 
** ims_otBldKwList
**
** Builds catalog metadata from database.
**
*******************************************************************/
int ims_otBldKwList(
	IMS_MSG_STRUCT *msgDesc,
	IMS_ODL_TREE *parent,
	char *platform,
	char *sensor,
	char *dataset)

{
	int status, rowCount;
	IMS_ODL_TREE *tree, *ptr;
	IMS_OT_KEYWORD *data;
	IMS_QI_DESC_OBJ *qDesc;
	int type;

	/*
	** Need to get all the keywords, their value and significance.
	*/



	/*
	** Create metadata tree object for CATALOG_METADATA.
	*/

	if (ims_addODLObject(msgDesc, parent, &tree, 
				"CATALOG_METADATA", 0, IMS_OBJECT) < IMS_OK)
	{

		/*
		** Could not create object.
		*/

			(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not create metadata object.");
			(void) ims_otMsg(msgDesc);
			return(IMS_ERROR);
	}


	if (openConnection(msgDesc, (IMS_QI_DESC_OBJ **) &qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not open connection to database server");
		(void) ims_otMsg(msgDesc);
		return(IMS_ERROR);
	}

	rowCount = 0;

	if (platform && sensor && dataset)
	{
		sprintf(qDesc->cmd, 
			"select p.keyword, p.data_type, p.max_len, s.significance,  \
			p.description, p.keyword_idx \
			from dataset_relation r, keyword_policy p, keyword_set s where \
			r.platform = '%s' and r.dataset =  '%s' and r.sensor = '%s' and \
			r.dataset_idx = s.dataset_idx and s.keyword_idx = p.keyword_idx \
			order by s.position",
			platform, dataset, sensor);
	}
	else if (platform && dataset)
	{
		sprintf(qDesc->cmd, 
			"select p.keyword, p.data_type, p.max_len, s.significance,  \
			p.description, p.keyword_idx \
			from dataset_relation r, keyword_policy p, keyword_set s where \
			r.platform = '%s' and r.dataset =  '%s' and \
			r.dataset_idx = s.dataset_idx and s.keyword_idx = p.keyword_idx \
			order by s.position",
			platform, dataset);
	}
	else
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"The platform and dataset must be specified.");
		return(IMS_ERROR);
	}

	/*
	printf("%s\n", qDesc->cmd);
	*/


	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		
		if (status < IMS_OK)
		{
			closeConnection(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not load keyword information.");
			(void) ims_otMsg(msgDesc);
			return(IMS_ERROR);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** Process returned rows
		*/
		rowCount++;


		/*
		** Get keyword
		*/

		data = (IMS_OT_KEYWORD *) malloc(sizeof(IMS_OT_KEYWORD));

		data->keyword = (char *) malloc(qDesc->valLength[0] + 1);
		(void) memcpy((char *) data->keyword, qDesc->valAddr[0],
			qDesc->valLength[0]);

		data->keyword[qDesc->valLength[0]] = '\0';

		/*
		** Get datatype 
		*/

		(void) memcpy((char *) (&data->data_type), qDesc->valAddr[1],
			qDesc->valLength[1]);

		/*
		** Get max_len
		*/

		(void) memcpy((char *) (&data->max_len), qDesc->valAddr[2],
			qDesc->valLength[2]);

		/*
		** Get significance
		*/

		(void) memcpy((char *) (&data->significance), qDesc->valAddr[3],
			qDesc->valLength[3]);

		/*
		** Get description
		*/


		data->description = (char *) malloc(qDesc->valLength[4] + 1);
		(void) memcpy((char *) data->description, qDesc->valAddr[4],
			qDesc->valLength[4]);

		data->description[qDesc->valLength[4]] = '\0';

		/*
		** Add the keyword into the tree structure and 
		** then attach the data pointer. 
		*/

		switch (data->data_type)
		{
			case IMS_INT1_TYPE:
				type = TV_INTEGER;
				break;

			case IMS_INT2_TYPE:
				type = TV_INTEGER;
				break;

			case IMS_INT4_TYPE:
				type = TV_INTEGER;
				break;

			case IMS_FLOAT4_TYPE:
				type = TV_REAL;
				break;

			case IMS_FLOAT8_TYPE:
				type = TV_REAL;
				break;

			case IMS_CHAR_TYPE:
				type = TV_STRING;
				break;

			case IMS_SYMBOL_TYPE:
				type = TV_SYMBOL;
				break;

			case IMS_STRING_TYPE:
				type = TV_STRING;
				break;

			case IMS_DATETIME_TYPE:
				type = TV_DATE_TIME;
				break;

			case IMS_DOYTIME_TYPE:
				type = TV_DATE_TIME;
				break;

		}

		if (ims_addODLKeyword(msgDesc, tree, data->keyword,
			type, NULL) < IMS_OK)
		{
			closeConnection(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not load keyword information for keyword = %s.", 
					data->keyword);
			(void) ims_otMsg(msgDesc);
			continue;
		}

		/*
		** Get keyword_idx
		*/


		(void) memcpy((char *) &(data->keyword_idx), qDesc->valAddr[5],
			qDesc->valLength[5]);

		/*
		** Attach data information to keyword which is last item
		** attach to the object.
		*/

		ptr = tree->children;

		while ((ptr != NULL) && (ptr->next != NULL))
		{
			ptr = ptr->next;
		}

		if (ptr != NULL)
		{
			ptr->data = (char *) data;
		}


	}
	closeConnection(qDesc);

}

/******************************************************************************
**
** openConnection ()
**
** This function will open a connection to the SQL server.
** 
**
******************************************************************************/

int openConnection (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ **qDescPass)
{
	IMS_QI_DESC_OBJ *qDesc;   
	int status;


	/*
	** Allocate a query descriptor
	*/

	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
					"Could not allocate a query descriptor.");
		return(IMS_ERROR);
	}
	
	qDesc->cmd = (char *) malloc(IMS_COL512_LEN);

    if ((char *) qDesc->cmd == NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate the command area for the query descriptor.");
				 
		 free(qDesc->cmd);
		 (void) ims_qiFreeDesc(qDesc);
		 return(IMS_ERROR);
	}
														  
	IMS_SETUSER (qDesc, ims_ot_userSpec.username);
	IMS_SETPSWD (qDesc, ims_ot_userSpec.password);

	if (ims_ot_userSpec.program != NULL)
		IMS_SETPROG(qDesc, ims_ot_userSpec.program);

	if (ims_ot_userSpec.server != NULL)
		IMS_SETSERVER(qDesc, ims_ot_userSpec.server);

	if (ims_ot_userSpec.database != NULL)
		IMS_SETDBNAME(qDesc, ims_ot_userSpec.database);

	/*
	** Attempt to logon to database
	*/

	status = ims_qiLogin(qDesc);

	if (status < IMS_OK)
	{
		(void) ims_msg(msgDesc, status, "Could not login to database server.");
		free(qDesc->cmd);
		(void) ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	IMS_SET_USERDATA(qDesc);

	*qDescPass = qDesc;  /* Set return query descriptor */
	return(IMS_OK);
}

/******************************************************************************
**
** closeConnection ()
**
** This function will close a connection to the SQL server.
** 
**
******************************************************************************/

int closeConnection (
	IMS_QI_DESC_OBJ *qDesc)
{

	free(qDesc->cmd);
	(void) ims_qiFreeDesc(qDesc);
	return(IMS_OK);
}


/******************************************************************************
**
** ims_otAddODLData()
**
******************************************************************************/

void ims_otAddODLData(
	IMS_ODL_TREE *tree)
{
	IMS_OT_KEYWORD *data;

	
	if (tree == NULL)
		return;


	/*
	** Get keyword
	*/

	tree->data = data = (IMS_OT_KEYWORD *) malloc(sizeof(IMS_OT_KEYWORD));



	data->keyword = (char *) malloc(strlen(tree->node_name) + 1);
	strcpy(data->keyword, tree->node_name);

	/*
	** Get datatype 
	*/

	switch (tree->value.type)
	{
		case TV_INTEGER:
			data->data_type = IMS_INT4_TYPE;
			break;

		case TV_REAL:
			data->data_type = IMS_FLOAT4_TYPE;
			break;

		case TV_STRING: 
			data->data_type = IMS_STRING_TYPE;
			break;

		case TV_SYMBOL: 
			data->data_type = IMS_SYMBOL_TYPE;
			break;

		case TV_DATE_TIME:  
			data->data_type = IMS_DATETIME_TYPE;
			break;

	}


	/*
	** Get max_len
	*/

	data->max_len = 100;  /* For now ... */


	/*
	** Get significance
	*/

	data->significance = 1;

	/*
	** Get description
	*/

	data->description = NULL;

	data->keyword_idx = -1;


	ims_otAddODLData(tree->children);
	ims_otAddODLData(tree->next);

}

/******************************************************************************
**
** ims_otGetKeywordValues
**
******************************************************************************/

int ims_otGetKeywordValues(
	IMS_MSG_STRUCT *msgDesc, 
	int keyword_idx,
	char *optionList[100],
	int *count)
{
	IMS_QI_DESC_OBJ *qDesc;   
	int status;
	int rowCount;
	

	if (openConnection(msgDesc, (IMS_QI_DESC_OBJ **) &qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not open connection to database server");
		(void) ims_otMsg(msgDesc);
		return(IMS_ERROR);
	}

	sprintf(qDesc->cmd, 
		"select value from keyword_value where keyword_idx = %d",
		keyword_idx);
		
	rowCount = 0;


	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		
		if (status < IMS_OK)
		{
			closeConnection(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not load keyword information.");
			(void) ims_otMsg(msgDesc);
			return(IMS_ERROR);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** Process returned rows
		*/
		rowCount++;

#if 0

		if (rowCount == 1)
		{
			/*
			** array of pointers.
			*/
						
			optionList = malloc(sizeof(char *) * IMS_AFFECTED(qDesc));
		}
#endif


		/*
		** Get value
		*/

		optionList[rowCount - 1] = malloc(qDesc->valLength[0] + 1);
		memset(optionList[rowCount-1], 0, qDesc->valLength[0] + 1);

		(void) memcpy((char *) optionList[rowCount-1], 
			qDesc->valAddr[0],
			qDesc->valLength[0]);

	}

	*count = rowCount;

	closeConnection(qDesc);
	
	return(IMS_OK);

}
