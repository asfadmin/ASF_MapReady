/******************************************************************************
**
** File:        ims_archive.h
**
** Function:    This header file defines the structures and enumerated types
**              required for the IMS File Transfer System (FTS) interface.
**
** Author:      S. Hardman
**
** Date:        8/3/94
**
** Modified:    8/17/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              2/20/95 - D. Crichton
**              Added Account ID to the client IMS_CLNT_EVENT structure.
**
**              5/17/95 - D. Crichton
**              Added a new datatype DOY_TIME.
** 
**				10/10/96 - D. Crichton - R2.1
**				Port to Sybase Open Client Client Library.
**
******************************************************************************/

#ifndef _IMS_ARCHIVE_H
#define _IMS_ARCHIVE_H

static char *sccsArchive = "@(#)ims_archive.h	5.2 03/13/97";

/*
** Enumerated types for statuses in the granules_x tables.
*/
enum IMS_GRANULE_STATUS
{
	IMS_NO_STATUS,
	IMS_AVAILABLE,
	IMS_ADD_IN_PROGRESS,
	IMS_ADD_TRANSFER_COMPLETE,
	IMS_REPLACE_IN_PROGRESS,
	IMS_REPLACE_TRANSFER_COMPLETE,
	IMS_DELETE_IN_PROGRESS,
	IMS_MARK_FOR_DELETE,
	IMS_RESERVED_1,
	IMS_RESERVED_2,
	IMS_RESERVED_3
};

/*
** Enumerated types for file types.
*/
enum IMS_FILE_TYPE
{
	IMS_NO_FILE_TYPE,
	IMS_META_DATA,
	IMS_LEADER,
	IMS_DATA,
	IMS_TRAILER
};

/*
** Enumerated types for file transfer events.
*/
typedef enum {
	IMS_NO_REQUEST,
	IMS_ADD,      
	IMS_GET,
	IMS_GET_MAX,
	IMS_GET_LATEST,
	IMS_DELETE,
	IMS_REPLACE,
	IMS_WHO
} IMS_CLNT_REQUEST_TYPE;

/*
** Client event structure.
*/
typedef struct {
	IMS_CLNT_REQUEST_TYPE requestType;  
	char *username;			/* Client's catalog username. */
	char *password;			/* Client's catalog password. */
	char *accountId;		/* Client's Account ID. */
	char *platform;
	char *sensor;
	char *dataset;
	char *name;
	char *format;
	short version;
	short fileCount;
	char **extensions;
	char *sourceDir;
	char localArchiveFlag;
	char *programName;		/* Client's executable name. */
	char *catSrvName;		/* Catalog server name. */
	char *catDbName;		/* Catalog database name. */
	char *ftsSrvName;		/* Server name provided for testing. */
	IMS_MSG_STRUCT *msgDesc;
} IMS_CLNT_EVENT;

/*
** CDB file types structure.
*/
typedef struct ims_file_types
{
	CS_SMALLINT type;
	CS_SMALLINT position;
	char extension[IMS_COL10_LEN+1];
	char archive_path[IMS_COL255_LEN+1];
	char localArchiveFlag;
	struct ims_file_types *next;
} IMS_FILE_TYPES;

/*
** CDB get file list structure.
*/
typedef struct ims_get_list
{
	int fileExist;
	char fileToWrite[IMS_PATH_LEN+IMS_COL60_LEN+1];
	char fullPathName[IMS_PATH_LEN+IMS_COL60_LEN+1];
	struct ims_get_list *next;
} IMS_GET_LIST;

/*
** Function Prototypes for the ims_archive.c module.
*/
int ims_archive (IMS_CLNT_EVENT *);

/*
** Function Prototypes for the ims_archiveUtil.c module.
*/
#ifdef KRB
int ims_clntSendKrbTicket (DBPROCESS *, IMS_CLIENT_KRB *, IMS_MSG_STRUCT *);
#endif	/* KRB */
int ims_clntValidateLogin (IMS_MSG_STRUCT *, CS_COMMAND *, IMS_CLNT_EVENT *);
int ims_clntAcceptFileName (IMS_MSG_STRUCT *, CS_COMMAND *,  char *, char *,
CS_INT *);
int ims_clntAcceptFile (IMS_MSG_STRUCT *, CS_COMMAND *, char *);
int ims_clntOpenFile (IMS_MSG_STRUCT *, CS_COMMAND *, char *, char *);
int ims_clntSendFile (IMS_MSG_STRUCT *, CS_COMMAND *, char *);
int ims_clntCloseFile (IMS_MSG_STRUCT *, CS_COMMAND *, char *, char *);
int ims_clntValidateMetadata (IMS_MSG_STRUCT *, CS_COMMAND *, char *);
int ims_getVersionNumber (IMS_MSG_STRUCT *, CS_COMMAND *, int *);
IMS_FILE_TYPES *ims_checkFileTypes(IMS_MSG_STRUCT *, CS_COMMAND *, IMS_CLNT_EVENT *);
int ims_getServerName(IMS_MSG_STRUCT *, IMS_CLNT_EVENT *, CS_COMMAND *);

#endif	/* !_IMS_ARCHIVE_H */
