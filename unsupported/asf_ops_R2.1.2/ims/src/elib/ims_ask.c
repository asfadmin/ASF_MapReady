static char *sccs = "@(#)ims_ask.c	5.1  16 Mar 1996";
/***************************************************************************
**
** File: ims_ask.c
**
** Function: Query the catalog database for granules matching the 
**           specified keyword criteria.  
**
** Creator:  Hoshyar Sayah
**
** Date:     August 7, 1991
**
** Modified: March 1995, H. Sayah
**           ASF adoption of AMMOS-CDB procedures.
**
***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_keyword.h>
#include <ims_msg.h>
#include <ims_qi.h>
#include <ims_ask.h>
#include <ims_askCat.h>
#include <ims_util.h>

/* For debug only */
/* #define IMS_DEBUG */

/*
** external functions
*/
extern int ims_parseKwBuffer ();

/*
** Error messages 
*/
static char	*EINCOMPLETE_PARAM = 
"Incomplete parameter for query file request.  Missing: %s.";
static char *ESIZEEXCEEDED = 
"Parameter length for '%s' exceeded the maximum length of '%d'.";
/*
** database user name and password.
** Must change to anonymous.
*/
static char *IMS_CLIENT_LOGIN = "IMS_READER";
static char *IMS_CLIENT_PASSWORD = "IMS_READER";
static char *EMPTY_STRING = "";

/*
** Global buffer for dynamic sql generation
*/
static char sqlBuffer[10240+1];

/*
** static functions.
*/
static int checkRequiredParam (IMS_ASK_REQUEST *);
static void freeKeywordPolicy (ASK_KEYWORD_POLICY *);
static void freeResultList (IMS_ASK_RLIST *);
static void freeResult (IMS_ASK_RESULT *);


/***********************************************************************
**
** ims_ask - Select granule names from specific granules table 
**           based on the search criteria in <keyword=value> 
**           format.
**
***********************************************************************/
IMS_ASK_RESULT	*ims_ask (
	IMS_ASK_REQUEST *request)
{
	ASK_CAT_REQUEST catReq;
	ASK_USER_SPEC *userSpec;
	ASK_INFO_SPEC *infoSpec;
	IMS_ASK_RESULT *result;
	ASK_KEYWORD_POLICY keywordPolicy;
	ASK_KEYWORD_LIST *keywordList;
	IMS_MSG_STRUCT *msgDesc;
	char server[IMS_COL30_LEN+1];
	int keywordCount;
	int status;

	/* Initialize variables */
	catReq.msgDesc = request->msgDesc;
	catReq.qDesc = (IMS_QI_DESC_OBJ *)NULL;
	result = (IMS_ASK_RESULT *)NULL;
	keywordList = (ASK_KEYWORD_LIST *)NULL;
	sqlBuffer[0] = '\0';
	keywordCount = 0;

	userSpec = &catReq.userSpec;
	infoSpec = &catReq.infoSpec;
	msgDesc = catReq.msgDesc;

	userSpec->username = (char *)NULL;
	userSpec->password = (char *)NULL;
	userSpec->program = (char *)NULL;
	userSpec->catDbName = (char *)NULL;
	userSpec->catSrvName = (char *)NULL;

	/*
	** Check required parameters in request structure.
	*/
	if (checkRequiredParam (request) < IMS_OK)
	{
		return (result);
	}

	/*
	** Prepare catReq structure for database connection.
	*/
	userSpec->username = IMS_CLIENT_LOGIN;
	userSpec->password = IMS_CLIENT_PASSWORD;
	if (request->program != (char *)NULL)
		userSpec->program = ims_truncStr(request->program);
	if (request->catDbName != (char *)NULL)
		userSpec->catDbName = ims_truncStr (request->catDbName);
	if (request->catSrvName != (char *)NULL)
		userSpec->catSrvName = ims_truncStr (request->catSrvName);

	if (request->sensor == (char *)NULL)
	{
		infoSpec->sensor[0] = '\0';
	}
	else
	{
		strcpy (infoSpec->sensor, request->sensor);
	}
	strcpy (infoSpec->dataset, request->dataset);
	strcpy (infoSpec->platform, request->platform);

	keywordPolicy.platform = infoSpec->platform;
	keywordPolicy.sensor = infoSpec->sensor;
	keywordPolicy.dataset = infoSpec->dataset;

	/*
	** Allocate space for the IMS_ASK_RESULT structure.
	*/
	if ((result = (IMS_ASK_RESULT *) malloc 
			((unsigned)sizeof (IMS_ASK_RESULT))) == (IMS_ASK_RESULT *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate memory for IMS_ASK_RESULT structure.");
		return (result);
	}

	/* Initialize result */
	result->resultCount = 0;
	result->resultList = (IMS_ASK_RLIST *)NULL;


	/*
	** open database connection 
	*/
	if ((status = ims_askCat (&catReq, ASK_OPEN_CONNECTION)) < IMS_OK)
	{
		ims_msg (msgDesc, status, "Opening database connection failed.");
		(void) freeResult (result);
		result = (IMS_ASK_RESULT *)NULL;
		return (result);
	}

	/*
	** validate platform, sensor, dataset entries
	*/
	if ((status = ims_askCat (&catReq, ASK_VALIDATE_PSD)) < IMS_OK)
	{
		ims_msg (msgDesc, status, 
			"Policy validation failed for '%s, %s, %s'.",
			keywordPolicy.platform, keywordPolicy.sensor, 
			keywordPolicy.dataset);
		(void) ims_askCat (&catReq, ASK_CLOSE_CONNECTION);
		(void) freeResult (result);
		result = (IMS_ASK_RESULT *)NULL;
		return (result);
	}
	keywordPolicy.dataset_idx = infoSpec->dataset_idx;

	if ((status = ims_askCat (&catReq, ASK_GET_GRANULES_TABLE)) < IMS_OK)
	{
		ims_msg (msgDesc, status, 
			"Getting granules table name failed for '%s, %s, %s'.",
			keywordPolicy.platform, keywordPolicy.sensor, 
			keywordPolicy.dataset);
		(void) ims_askCat (&catReq, ASK_CLOSE_CONNECTION);
		(void) freeResult (result);
		result = (IMS_ASK_RESULT *)NULL;
		return (result);
	}
	keywordPolicy.granules_table = infoSpec->granules_table;
		
	/*
	** get keyword policy information.  This information is required 
	** for the parser program to create a valid SQL statement.
	*/
	catReq.item[0] = (void *) &keywordCount;
	if ((status = ims_askCat (&catReq, ASK_GET_KEYWORD_LIST)) < IMS_OK)
	{
		ims_msg (msgDesc, status, 
			"Failed to get keyword policy for '%s, %s, %s'.",
			infoSpec->platform, infoSpec->sensor, infoSpec->dataset);
		(void) ims_askCat (&catReq, ASK_CLOSE_CONNECTION);
		(void) freeResult (result);
		result = (IMS_ASK_RESULT *)NULL;
		return (result);
	}
	keywordPolicy.keywordList = catReq.item[1];

	/*
	** Parse the keyword buffer command provided from the user.
	*/
	if ((status = ims_parseKwBuffer (msgDesc, &keywordPolicy, 
			request->expression, sqlBuffer)) < IMS_OK)
	{
		(void) ims_askCat (&catReq, ASK_CLOSE_CONNECTION);
		(void) freeKeywordPolicy (&keywordPolicy);
		(void) freeResult (result);
		result = (IMS_ASK_RESULT *)NULL;
		return (result);
	}

#ifdef IMS_DEBUG
	printf ("SQL BUFFER:\n%s\n", sqlBuffer);
#endif

	catReq.item[0] = sqlBuffer;
	catReq.item[1] = (void *)&result->resultCount;
	/*
	** Execute the dynamic SQL statement created by the parser.
	*/
	if ((status = ims_askCat (&catReq, ASK_GET_RESULT_LIST)) < IMS_OK)
	{
		ims_msg (msgDesc, status, 
			"Failed to get RLIST for '%s, %s, %s'.",
			infoSpec->platform, infoSpec->sensor, infoSpec->dataset);
		(void) ims_askCat (&catReq, ASK_CLOSE_CONNECTION);
		(void) freeResult (result);
		result = (IMS_ASK_RESULT *)NULL;
		return (result);
	}
	result->resultList = catReq.item[2];

	/*
	** close database connection
	*/
	if ((status = ims_askCat (&catReq, ASK_CLOSE_CONNECTION)) < IMS_OK)
	{
		/* continue the operations; not a fatal error */
		ims_msg (msgDesc, IMS_WARNING, "Closing database connection failed");
	}

	(void) freeKeywordPolicy (&keywordPolicy);

	return (result);
}

/************************************************************************
**
** checkRequiredParam() - check that all required query files criteria
**                        parameters.
**
**************************************************************************/
static int checkRequiredParam (
	IMS_ASK_REQUEST	*request)
{
	IMS_MSG_STRUCT *msgDesc;
	int len;

	msgDesc = request->msgDesc;

	/*
	** Check that all required parameters are given in criteria structure
	*/
	if ((len = strlen(ims_truncStr(request->platform))) == 0)
	{
		ims_msg (msgDesc, IMS_ERROR, EINCOMPLETE_PARAM, "platform");
		return (IMS_ERROR);
	}
	if (len > IMS_COL30_LEN)
	{
		ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED, "platform", IMS_COL30_LEN);
		return (IMS_ERROR);
	}

	len = strlen(ims_truncStr(request->sensor));
	if (len > IMS_COL30_LEN)
	{
		ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED, "sensor", IMS_COL30_LEN);
		return (IMS_ERROR);
	}

	if ((len = strlen(ims_truncStr(request->dataset))) == 0)
	{
		ims_msg (msgDesc, IMS_ERROR, EINCOMPLETE_PARAM, "dataset");
		return (IMS_ERROR);
	}
	if (len > IMS_COL80_LEN)
	{
		ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED, "dataset", IMS_COL80_LEN);
		return (IMS_ERROR);
	}

	if (request->expression != (char *)NULL)
	{
		if ((len = strlen (request->expression)) > 5120)
		{
			ims_msg (msgDesc, IMS_ERROR, ESIZEEXCEEDED, "expression", 5120);
			return (IMS_ERROR);
		}
	}
	return(IMS_OK);
}

/****************************************************************************
**
** freeKeywordPolicy - free the keywordPolicy allocated memory.
**
****************************************************************************/
static void freeKeywordPolicy (
	ASK_KEYWORD_POLICY *keywordPolicy)
{
	if (keywordPolicy != (ASK_KEYWORD_POLICY *)NULL)
	{
		if (keywordPolicy->keywordList != (ASK_KEYWORD_LIST *)NULL)
		{
			(void) cfree (keywordPolicy->keywordList);
		}
		keywordPolicy->keywordList = (ASK_KEYWORD_LIST *)NULL;
	}
}

/****************************************************************************
**
** freeResultList -  free the fileList allocated memory.
**
*****************************************************************************/
static void freeResultList (
	IMS_ASK_RLIST *resultList)
{
	IMS_ASK_RLIST *currPtr;
	currPtr = resultList;
	while (resultList != (IMS_ASK_RLIST *)NULL)
	{
		currPtr = resultList->next;
		free (resultList);
		resultList = currPtr;
	}
	resultList = (IMS_ASK_RLIST *)NULL;
}

/****************************************************************************
**
** freeResult - free the result allocated memory.
**
****************************************************************************/
static void freeResult (
	IMS_ASK_RESULT *result)
{
	if (result != (IMS_ASK_RESULT *)NULL)
	{
		freeResultList (result->resultList);
	}
	free (result);
	result = (IMS_ASK_RESULT *)NULL;
}


