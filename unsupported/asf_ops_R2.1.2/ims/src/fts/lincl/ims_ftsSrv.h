
/******************************************************************************
**
** File:        ims_ftsSrv.h
**
** Function:    Header file for the File Transfer Server.
**
** Date:        9/25/90
**
** Modified:    2/17/94 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Changed the datatype of ftProcessDesc.pid from int to pid_t.
**
**              8/17/94 - S. Hardman - R0
**              ASF adaptation of code inherited from AMMOS-CDB.
**
**              12/20/94 - S. Hardman - R1B
**              Ported to Sybase System 10. See the Open Server
**              Server-Library/C Reference Manual for more information.
**              Modified the user defined message codes.
**
**              02/22/95 - D. Crichton - R1B
**              Added account id to the client request structure.
**
**              03/01/95 - D. Crichton - R1B
**              Modify DataSetPolicy structure field names.
**
**              03/02/95 - D. Crichton - R1B
**              Add dataset policy path structure.
**
**				03/15/95 - D. Crichton - R1B
**				Added keyword value structure for validating character type
**				metadata.
**
**				05/12/95 - D. Crichton - R1B
**				Added fields to granule desc., and file list structures
**				to support file sizes.
**
******************************************************************************/

#ifndef _IMS_FTSSRV_H
#define _IMS_FTSSRV_H

static char *sccsFtsSrv = "@(#)ims_ftsSrv.h	5.3  03/13/97";

#define FTS_PRIORITY_DELTA (CS_INT)2
#define FTS_AUX_MUTEX "FTS_AUX_MUTEX"
#define FTS_THREAD_MUTEX "FTS_THREAD_MUTEX"
#define FTS_STATUS_COUNT 12


/*
** FTS status description table structure.
*/
typedef struct fts_status_table
{
	CS_CHAR description[FTS_STATUS_COUNT][IMS_COL255_LEN+1];
} FTS_STATUS_TABLE;

/*
** FTS read file structure.
*/
typedef struct ftsReadGranule
{
	CS_INT granule_idx;    /* Internal database index number. */
	int readCounter;       /* Read access counter. */
} FTS_READ_GRANULE;

/*
** FTS platform/sensor policy structure.
*/
typedef struct fts_sensor_policy
{
	CS_CHAR platform[IMS_COL30_LEN+1];
	CS_CHAR sensor[IMS_COL30_LEN+1];
	IMS_HASH_STRUCT *hashPtr;
	struct fts_sensor_policy *next;
} FTS_SENSOR_POLICY;

/*
** FTS file format policy structure.
*/
typedef struct fts_format_policy
{
	CS_SMALLINT type;
	CS_SMALLINT position;
	CS_CHAR extension[IMS_COL10_LEN+1];
	struct fts_format_policy *next;
} FTS_FORMAT_POLICY;

/*
** FTS file policy structure.
*/
typedef struct fts_file_policy
{
	CS_CHAR format[IMS_COL10_LEN+1];
	int fileCount;
	FTS_FORMAT_POLICY *formatPolicy;
	struct fts_file_policy *next;
} FTS_FILE_POLICY;

/*
** Extracted keyword value list. 
*/
typedef struct ftsKeywordValue
{
	CS_SMALLINT keyword_idx;
	CS_CHAR value[IMS_COL255_LEN+1];
	struct ftsKeywordValue *next;
} FTS_KEYWORD_VALUE;


/*
** FTS keyword policy structure.
*/
typedef struct fts_keyword_policy
{
	CS_CHAR keyword[IMS_COL30_LEN+1];
	CS_SMALLINT keyword_idx;
	CS_SMALLINT data_type;
	CS_SMALLINT max_len;
	CS_FLOAT min_val;
	CS_FLOAT max_val;
	CS_SMALLINT significance;
	CS_SMALLINT position;
	CS_SMALLINT query_type;
	FTS_KEYWORD_VALUE *keywordValue;
	struct fts_keyword_policy *next;
} FTS_KEYWORD_POLICY;

/*
** FTS dataset path policy structure.
*/
typedef struct fts_path_policy
{
	CS_CHAR path[IMS_COL255_LEN+1];
	CS_INT start_granule;
	CS_INT end_granule;
	struct fts_path_policy *next;
} FTS_PATH_POLICY;

/*
** FTS dataset policy structure.
*/
typedef struct fts_dataset_policy
{
	CS_CHAR dataset[IMS_COL80_LEN+1];
	CS_SMALLINT dataset_idx;
	CS_TINYINT oagdr;
	CS_CHAR mfd_p;
	CS_CHAR version_p;
	CS_CHAR granules_table[IMS_COL30_LEN+1];
	CS_CHAR load_program[IMS_COL30_LEN+1];
	CS_CHAR dist_program[IMS_COL30_LEN+1];
	CS_CHAR local_archive_p;
	char *localArchivePath;
	CS_SMALLINT spatial_type;
	CS_SMALLINT temporal_type;
	FTS_FILE_POLICY *filePolicy;
	FTS_PATH_POLICY *pathPolicy;
	FTS_KEYWORD_POLICY *keywordPolicy;
	int keywordPolicyLen;
	struct fts_dataset_policy *next;
} FTS_DATASET_POLICY;


/*
** FTS user specification structure.
*/
typedef struct ftsUserSpec
{
	char login[IMS_COL15_LEN+1];
	char password[IMS_COL15_LEN+1];
} FTS_USERSPEC;

/*
** FTS file list structure.
*/
typedef struct fts_file_list
{
	char extension[IMS_COL10_LEN+1];
	int fileIndex;                  /* File fragment counter. */
	int fd;                         /* File descriptor.	*/
	int isFileOpen;
	int hasFileBeenOpen;
	char fullPathName[IMS_PATH_LEN+IMS_COL60_LEN+1];
	char fileToWrite[IMS_PATH_LEN+IMS_COL60_LEN+1];
	struct fts_file_list *next;
} FTS_FILE_LIST;

/*
** FTS granule descriptor structure.
*/
typedef struct fts_granule_desc
{
	CS_INT granule_idx;
	int fileCount;          	/* total file counter */
	int metaBytes; 			  	/* size in bytes of metadata file */
	int dataBytes; 				/* size in bytes of data file(s) */	
	FTS_FILE_LIST *fileList;   	/* pointer to list of files */ 
	FTS_FILE_LIST *currFile;   	/* pointer to working file */
} FTS_GRANULE_DESC;

typedef struct
{
	int kIndex;
	char *kBuffer;
	int kLength;
	char kLanguage;
} FTS_KHEADER_DESC;

/*
** FTS client request structure which mirrors the IMS_CLNT_EVENT structure.
*/
typedef struct fts_client_request
{
    char accountId[IMS_COL15_LEN+1];       /* Account Id */
	IMS_CLNT_REQUEST_TYPE requestType;
	char platform[IMS_COL30_LEN+1];
	char sensor[IMS_COL30_LEN+1];
	char dataset[IMS_COL80_LEN+1];
	char name[IMS_COL30_LEN+1];
	char format[IMS_COL10_LEN+1];
	DBSMALLINT version;
} FTS_CLIENT_REQUEST;

typedef struct ftsNameList
{
	char name[IMS_COL30_LEN+1];
	struct ftsNameList *next;
} FTS_NAME_LIST;


/*
** Extracted keyword list from the ODL Metadata file.
*/
typedef struct ftsKeywordList
{
	CS_CHAR keyword[IMS_COL30_LEN+1];
	CS_INT value_integer;
	CS_FLOAT value_real;
	CS_CHAR value_string[IMS_COL255_LEN+1];
	CS_SMALLINT keyword_idx;
	CS_SMALLINT data_type;
	CS_SMALLINT max_len;
	CS_FLOAT min_val;
	CS_FLOAT max_val;
	CS_SMALLINT significance;
	CS_SMALLINT query_type;
	FTS_KEYWORD_VALUE *keywordValue;
	struct ftsKeywordList *next;
} FTS_KEYWORD_LIST; 



/*
** FT SERVER Process (thread) descriptor.  All functions within
** an open server must be re-entrant.  This means that there can
** be no storage of a function's private data in any global or
** static space.  The open server provides a special user pointer
** in its srv_proc structure which is setup by the function call
** srv_thread_props().  All process-specific data must be referenced
** from this structure.
*/
typedef struct fts_proc_desc
{
	FTS_USERSPEC client;
	FTS_USERSPEC surrogate;
	FTS_CAT_STRUCT catReq;
	FTS_KEYWORD_LIST *keywordList;
	FTS_CLIENT_REQUEST clientReq;
	FTS_GRANULE_DESC granuleDesc;
	FTS_KHEADER_DESC kHeaderDesc;
	FTS_SENSOR_POLICY *sensorPolicy;
	FTS_DATASET_POLICY *datasetPolicy;
	FTS_FILE_POLICY *filePolicy;
	FTS_PATH_POLICY *pathPolicy;
	IMS_QI_DESC_OBJ *qDesc;
	IMS_HASH_STRUCT *readHashPtr;
	FTS_READ_GRANULE *readGranule;
/*	IMS_SERVER_KRB sKrb; */
	IMS_MSG_STRUCT *msgStruct;
	SRV_OBJID *auxMutex;
	SRV_OBJID *threadMutex;
	FTS_STATUS_TABLE *statusTable;
	IMS_FILE_TYPES *fileTypes;
	unsigned char rollbackOnExit;
	unsigned char cancelFlag;
	unsigned char validUser;
	unsigned char recordMsgs;
	char *userName;
	char *ftsSrvName;
	char *ftsProgramName;
	char *ftsHostName;
	pid_t ftsPid;
	char *catSrvName;
	char *catDbName;
	char *logFileDir;
	char clientUser[IMS_COL30_LEN+1];		/* Foreign Client Username */ 
	char clientMachine[IMS_COL30_LEN+1];	/* Foreign Client Machine Name */
	int clientPid;							/* Foreign Client Process Id */
} FTS_PROC_DESC;

/*
** This structure keeps track of all client srvproc connections.
*/
typedef struct ftsSrvProcList
{
	SRV_PROC *srvproc;
	struct ftsSrvProcList *next;
} FTS_SRVPROC_LIST;
 
/*
** This structure is used for FTS and AUXP communication.
*/
typedef struct ftsBufferSpecType 
{
	char reply[IMS_COL255_LEN + 1];
	int kind;		/* type of buffer, CMD(1), MSG(2), other */
	int severity;
	int errorno; 
	int LCF;			/* last command flag		*/
	int LMF;			/* last message flag		*/
} FTS_BUFFERSPEC_TYPE; 

				
/*
** Define open server user defined message codes.
*/
/* SRV_MAXERROR is defined in oserror.h to be (CS_INT) 20000 */
#define FTS_INIT_ERROR   SRV_MAXERROR + 1
#define FTS_POLICY_ERROR SRV_MAXERROR + 2
#define FTS_THREAD_ERROR SRV_MAXERROR + 3
#define FTS_RPC_ERROR    SRV_MAXERROR + 4
#define FTS_QUEUED_MSG   SRV_MAXERROR + 5
#define FTS_INFO         SRV_MAXERROR + 6

/*
** Function Prototypes for the ims_srvMutex.c module.
*/
extern int ims_changePriority (SRV_PROC *, int);
extern int ims_lockMutexWait (SRV_OBJID *);
extern int ims_lockMutexNoWait (SRV_OBJID *);
extern int ims_unlockMutex (SRV_OBJID *);

/*
** Function Prototypes for the ims_srvReadGranule.c module.
*/
extern FTS_READ_GRANULE *ims_incrReadGranule (IMS_HASH_STRUCT *, DBINT,
	IMS_MSG_STRUCT *);
extern int ims_decrReadGranule (IMS_HASH_STRUCT *, FTS_READ_GRANULE *,
	IMS_MSG_STRUCT *);

/*
** Function Prototypes for the ims_srvEvent.c module.
*/
extern int ims_validateClientLogin (SRV_PROC *);
extern int ims_validateMetadata (SRV_PROC *);
extern int ims_addEventBegin (SRV_PROC *);
extern int ims_addEventEnd (SRV_PROC *);
extern int ims_getEventBegin (SRV_PROC *);
extern int ims_getEventEnd (SRV_PROC *);
extern int ims_replaceEventBegin (SRV_PROC *);
extern int ims_replaceEventEnd (SRV_PROC *);
extern int ims_deleteEvent (SRV_PROC *);
extern IMS_FILE_TYPES *ims_checkServerFileTypes(SRV_PROC *);

/*
** Function Prototypes for the ims_srvShipMsgs.c module.
*/
extern int ims_srvShipMsgs (register SRV_PROC *);
extern int ims_srvLogMsgs (register SRV_PROC *);

/*
** Function Prototypes for the ims_srvUtil.c module.
*/
extern int ims_srvAcceptKrbTicket (SRV_PROC *);
extern int ims_srvSendFileName (SRV_PROC *);
extern int ims_srvSendFile (SRV_PROC *);
extern int ims_srvOpenFile (SRV_PROC *);
extern int ims_srvAcceptFile (SRV_PROC *);
extern int ims_srvCloseFile (SRV_PROC *);
extern int ims_sendFileTypes (SRV_PROC *, IMS_FILE_TYPES *);
extern int ims_sendVersionNumber(SRV_PROC *);

#endif	/* !_IMS_FTSSRV_H */
