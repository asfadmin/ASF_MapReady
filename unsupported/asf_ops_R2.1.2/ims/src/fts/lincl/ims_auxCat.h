/******************************************************************************
**
** File:        ims_auxCat.h
**
** Function:    This is the header file for the ims_auxCat.c functions which
**              perform catalog queries and updates for the AUX process.
**
** Author:      Hoshyar Sayah
**
** Date:        12/10/90
**
** Modified:    8/17/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              12/20/94 - S. Hardman - R1B
**              Ported to Sybase System 10. See the Open Server
**              Server-Library/C Reference Manual for more information.
**
**              03/02/95 - D. Crichton - R1B
**              Added a new function getPathPolicy().
**
******************************************************************************/

#ifndef IMS_AUXCAT_H
#define IMS_AUXCAT_H

static char *sccsAuxCat = "@(#)ims_auxCat.h	5.1  18 Mar 1996";


/*
** Internal CDB file descriptor.  Contains information on files, most
** of which comes from the catalog.
*/
typedef struct aux_info_spec
{
	char platform[IMS_COL30_LEN+1];
	char sensor[IMS_COL30_LEN+1];
	char dataset[IMS_COL80_LEN+1];
	CS_SMALLINT dataset_idx;
	CS_SMALLINT keyword_idx;
	char name[IMS_COL30_LEN+1];
	char format[IMS_COL10_LEN+1];
  	char username[IMS_COL15_LEN+1];
  	char password[IMS_COL15_LEN+1];
  	char programName[IMS_COL30_LEN+1];
	char catSrvName[IMS_COL30_LEN+1];
	char catDbName[IMS_COL30_LEN+1];
	char ftsSrvName[IMS_COL30_LEN+1];
	char ftsHostName[IMS_COL30_LEN+1];
	IMS_MSG_STRUCT *msgDesc;
} AUX_INFO_SPEC;

/*
** Following is the request structure passed to the AUX
** catalog function, ims_auxCat.  
*/
typedef struct auxCatReqObj
{
  AUX_INFO_SPEC *auxSpec;       /* Auxiliary data specification. */
  void *itemList1;             	/* List of items to insert or extract. */
  void *itemList2;             	/* List of items to insert or extract. */
  void *itemList3;             	/* List of items to insert or extract. */
} AUX_CAT_REQUEST;

/*
** Auxiliary Catalog Events
*/
typedef enum aux_cat_event
{
	AUX_OPEN_CONNECTION,
	AUX_GET_SERVER_HOST,
	AUX_GET_STATUS_TABLE, 
	AUX_GET_SENSOR_POLICY,
	AUX_GET_DATASET_POLICY, 
	AUX_GET_PATH_POLICY,
	AUX_GET_FILE_POLICY,
	AUX_GET_FORMAT_POLICY,
	AUX_GET_KEYWORD_POLICY,
	AUX_GET_KEYWORD_VALUE,
	AUX_ADD_PROCESS_INFO,
	AUX_ADD_PROCESS_ENDTIME,
	AUX_CLOSE_CONNECTION
} AUX_CAT_EVENT; 

/*
** Function Prototype for the ims_auxCat.c module.
*/
extern int ims_auxCat (AUX_CAT_REQUEST *, AUX_CAT_EVENT);

#endif	/* !IMS_AUXCAT_H */
