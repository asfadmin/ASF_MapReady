/******************************************************************************
**
** File:        ims_askCat.h
**
** Function:    This is the header file for the ims_askCat.c functions which
**              perform catalog queries and updates for the ASK process.
**
** Author:      Hoshyar Sayah
**
** Date:        03/10/95
**
******************************************************************************/

#ifndef _IMS_ASKCAT_H
#define _IMS_ASKCAT_H

static char *sccsAskCat = "@(#)ims_askCat.h	5.1  16 Mar 1996";

typedef struct ask_user_spec
{
	char *username;
	char *password;
	char *program;
	char *catDbName;
	char *catSrvName;
} ASK_USER_SPEC;
/*
*/
typedef struct ask_info_spec
{
	char platform[IMS_COL30_LEN+1];
	char sensor[IMS_COL30_LEN+1];
	char dataset[IMS_COL80_LEN+1];
	char granules_table[IMS_COL30_LEN+1];
	CS_SMALLINT dataset_idx;
	char name[IMS_COL30_LEN+1];
} ASK_INFO_SPEC;

/*
** Following is the request structure passed to the ASK
** catalog function, ims_askCat.  
*/
typedef struct askCatRequest
{
	ASK_USER_SPEC userSpec;
  ASK_INFO_SPEC infoSpec;       /* data specification structure */
	IMS_MSG_STRUCT *msgDesc;     /* msg descriptor pointer */
	IMS_QI_DESC_OBJ *qDesc;      /* qi descriptor pointer */
  void *item[IMS_COL10_LEN];   /* items to insert and/or extract */
} ASK_CAT_REQUEST;

/*
** Auxiliary Catalog Events
*/
typedef enum ask_cat_event
{
	ASK_OPEN_CONNECTION,
	ASK_VALIDATE_PSD,
	ASK_GET_GRANULES_TABLE,
	ASK_GET_KEYWORD_LIST,
	ASK_GET_RESULT_LIST,
	ASK_CLOSE_CONNECTION
} ASK_CAT_EVENT; 

/*
** Function Prototype for the ims_askCat.c module.
*/
extern int ims_askCat (ASK_CAT_REQUEST *, ASK_CAT_EVENT);

#endif	/* !_IMS_ASKCAT_H */
