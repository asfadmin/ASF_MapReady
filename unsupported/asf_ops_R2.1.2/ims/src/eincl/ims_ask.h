/******************************************************************************
**
** File:        ims_ask.h
**
** Function:    This header file defines the structures required for 
**              ims_ask.c function and program.
**
** Author:      H. Sayah
**
** Date:        March 1995
**
******************************************************************************/

#ifndef _IMS_ASK_H
#define _IMS_ASK_H

static char *sccsAsk = "@(#)ims_ask.h	5.1  16 Mar 1996";

/*
** Global structure for keyword search routine.
*/
typedef struct imsAskRequest {
	char *platform;            /* platform */
	char *sensor;              /* sensor */
	char *dataset;             /* dataset */
	char *expression;		       /* keyword search buffer */
	char *program;             /* program name */
	char *catSrvName;          /* optional SQL-Server name */
	char *catDbName;           /* optional database name */
	IMS_MSG_STRUCT *msgDesc;   /* msg structure pointer */
} IMS_ASK_REQUEST;

/*
** Keyword information extarcted from catalog policy tables are
** stored in the following data structure for ims_ask() routine.
*/
typedef struct askKeywordList {
	CS_CHAR keyword[IMS_COL30_LEN+1];
	CS_SMALLINT keyword_idx; 
	CS_SMALLINT data_type;
	CS_SMALLINT max_len;
	CS_FLOAT min_val;
	CS_FLOAT max_val;
	CS_SMALLINT significance; 
	CS_SMALLINT position;
	CS_SMALLINT query_type;
	struct askKeywordList *next;
} ASK_KEYWORD_LIST;

/*
** Keyword policy data structure for ims_ask() routine.
*/
typedef struct askKeywordPolicy {
	CS_SMALLINT dataset_idx;
	CS_CHAR *platform;
	CS_CHAR *sensor;
	CS_CHAR *dataset;
	CS_CHAR *granules_table;
	CS_INT keywordCount;
	ASK_KEYWORD_LIST *keywordList;
} ASK_KEYWORD_POLICY;

/*
** List of files returned from the ims_ask() routine.
*/
typedef struct imsAskRlist {
	CS_SMALLINT dataset_idx;
	CS_INT granule_idx;
	CS_CHAR name[IMS_COL30_LEN+1];
	CS_SMALLINT version;
	CS_INT data_kbytes;
	CS_INT metadata_kbytes;
	struct imsAskRlist *next;
} IMS_ASK_RLIST;


/*
** result structure 
*/
typedef struct imsAskResult {
	int resultCount;
	IMS_ASK_RLIST *resultList;
} IMS_ASK_RESULT;

/*
** Function Prototypes for the ims_ask.c
*/
extern IMS_ASK_RESULT *ims_ask (IMS_ASK_REQUEST *);

#endif	/* !_IMS_ASK_H */
