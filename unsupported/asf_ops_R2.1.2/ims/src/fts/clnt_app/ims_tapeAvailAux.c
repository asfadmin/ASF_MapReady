char *sccs = "@(#)ims_tapeAvailAux.c	5.8 05/15/97";
/******************************************************************************
**
** File:	ims_tapeAvailAux.c
**
** Function:	Perform all auxillary processing using a pipe with the 
**				FTS network server for tape available messages.
**
** Author: Dan Crichton	
**
** Date:	12/5/95
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>

#include <ims_dbms.h>
#include <ims_query.h>
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <syslog.h>
#include <odldef.h>
#include <odlinter.h>
#include <ims_odl.h>

#include <ims_job_control.h>

typedef struct USERSPEC
{
	char *username;
	char *password;
	char *server;
	char *program;
	char *database;
	char *accountId;
	char first_name[IMS_COL20_LEN+1];
	char last_name[IMS_COL20_LEN+1];
	char *clientUser;
	char *clientPassword;
} USERSPEC;

typedef struct tableNames
{
	char granuleName[IMS_COL30_LEN+1];
	char platform[IMS_COL30_LEN+1];
	char dataset[IMS_COL80_LEN+1];
	char quick_look;
	struct tableNames *next;
	struct tableNames *prev;
} TABLE_NAMES;


/*
** External Functions
*/
extern char *ims_extractFileName (char *);
extern int ims_forwardMsgQueue (IMS_MSG_STRUCT *);

/*
** Local Functions
*/
static int execCmd (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static int getGranuleTableName(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, 
							USERSPEC *, char *, char *, char *);
static int buildAggregate(IMS_MSG_STRUCT *, TABLE_NAMES *, AGGREGATE *, 
					USERSPEC *);
static int processTapeAvail(IMS_MSG_STRUCT *, USERSPEC *, int, char *);
static int locateTape(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, USERSPEC *, char *, 
					int, TABLE_NAMES **);
static int getUserInfo(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, char *, char *, 
					char *);

static int checkDatatake(IMS_MSG_STRUCT *, USERSPEC *, char *, char *, int, int, int *, int *, int);

/*
** Global Variables
*/

static char *glb_programName;
IMS_MSG_STRUCT *msgDesc;


/******************************************************************************
**
** main ()
**
******************************************************************************/

main (int argc, char *argv[])
{
	char *cmds[20], *replies[20];
	int cmdLen, i;
	char fullPathName[IMS_COL255_LEN+1]; /* KEY_TIMES filename to process */
	int status, id;
	IMS_MSG_STRUCT *msgDesc;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	USERSPEC userSpec;


	/*
	** Initialize the message processor.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr,
			"Memory allocation for IMS_MSG_STRUCT structure failed.");
			exit (IMS_FATAL);
	}

	glb_programName = ims_extractFileName (argv[0]);
	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';

	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgProgramName (msgDesc, glb_programName);
	(void) sprintf (banner, "%s::%s", hostName, glb_programName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgQueueFlag(msgDesc, IMS_ON);
	(void) ims_msgStderrFlag(msgDesc, IMS_OFF);
	(void) ims_msgSybMsgHndlFlag(msgDesc, IMS_ON);


	/*
	** Initialize commands and replies to FTS Server
	*/

	for (i = 0; i < 20; i++)
	{
		cmds[i] = malloc(IMS_COL255_LEN);
		replies[i] = malloc(IMS_COL255_LEN);
	}



	/*
	** Initialize commands
	*/
	(void) strcpy (cmds[0], "CMD:0:dbUserName: ");
	(void) strcpy (cmds[1], "CMD:0:dbPassword: ");
	(void) strcpy (cmds[2], "CMD:0:server: ");
	(void) strcpy (cmds[3], "CMD:0:dbName: ");
	(void) strcpy (cmds[4], "CMD:0:granuleName: ");
	(void) strcpy (cmds[5], "CMD:0:granuleIdx: ");
	(void) strcpy (cmds[6], "CMD:0:accountId: ");
	(void) strcpy (cmds[7], "CMD:0:clientUser: ");
	(void) strcpy (cmds[8], "CMD:0:clientPassword: ");


#ifdef AUX_DEBUG
	fprintf(stderr, "Auxilary Process %s preparing requests to FTS Server\n",
			argv[0]);
#endif


	/*
	** request for required information
	*/
	cmdLen = 9;
	for ( i=0; i < cmdLen; i++ )
	{
#ifdef AUX_DEBUG
		fprintf(stderr, "Sending CMD[%d] to FTS SERVER:%s\n",i, cmds[i]);
#endif
		(void) write (1, cmds[i], strlen (cmds[i]) + 1);
		if (read( 0, replies[i], IMS_COL255_LEN+1) == 0)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Premature ftr termination, abort auxProcess");
			exit (0);
		}
#ifdef AUX_DEBUG
		fprintf(stderr, "Receiving reply from FTS SERVER:%s\n", replies[i]);
#endif		

		/*
		** Get rid of new line characters
		*/
		replies[i][strlen(replies[i])-1] = '\0';
	}


	userSpec.username = ims_trim(replies[0]);
	userSpec.password = ims_trim(replies[1]);
	userSpec.server   = ims_trim(replies[2]);
	userSpec.database = ims_trim(replies[3]);
	userSpec.program = ims_extractFileName(argv[0]);
	userSpec.accountId = ims_trim(replies[6]);
	userSpec.clientUser = ims_trim(replies[7]);
	userSpec.clientPassword = ims_trim(replies[8]);


	if ((status = processTapeAvail(msgDesc, &userSpec,
			atoi(replies[5]), replies[4])) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not process the tape available for granule_idx %d", 
			atoi(replies[5]));
	}


	/*
	** Close connection with server
	*/

	(void) ims_forwardMsgQueue (msgDesc); 


	/* free */

	ims_msgStructFree(msgDesc);


	exit(0);
}

/******************************************************************************
**
** processTapeAvail ()
**
******************************************************************************/

static int processTapeAvail(
	IMS_MSG_STRUCT *msgDesc,
	USERSPEC *userSpec,
	int granuleIdx,
	char *granuleName)
{
	int status;
	int order_id;
	IMS_QI_DESC_OBJ *qDesc;
	static char qbuf[IMS_COL512_LEN+1];
	TABLE_NAMES *ptr, *table_names = NULL;
	AGGREGATE root;


	/*         
	** Allocate a query descriptor
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		return(IMS_ERROR);
	}	
	

	IMS_SETUSER (qDesc, userSpec->username);
	IMS_SETPSWD (qDesc, userSpec->password);
	IMS_SETPROG (qDesc, userSpec->program);
	IMS_SETSERVER (qDesc, userSpec->server);
	IMS_SETDBNAME (qDesc, userSpec->database);
	IMS_SET_VERBOSE (qDesc, 10);

	status = ims_qiLogin(qDesc);

	if (status < IMS_OK)
	{

		(void) ims_msg(msgDesc, status, "Could not login to database.");
		ims_qiFreeDesc(qDesc);
		exit(1);
	}

	IMS_SET_USERDATA(qDesc);
	qDesc->cmd = qbuf;

	

	/*
	** Locate the datasets and granule table names which have 
	** acquisitions on the available media.
	*/

	if (locateTape(msgDesc, qDesc, userSpec, granuleName, granuleIdx, 
			&table_names) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Locating downlink failed for the available media");
		return(IMS_ERROR);
	}

	if (table_names == NULL)
	{
		ims_qiFreeDesc (qDesc);
		while (table_names != NULL)
		{
			ptr = table_names;
			table_names = table_names->next;
			free(ptr);
		}
		return(IMS_OK);
	}

	/*
	** Determine the user information for the scan job
	*/

	if (getUserInfo(msgDesc, qDesc, userSpec->clientUser,
		userSpec->first_name, userSpec->last_name) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not determine user information");
		return(IMS_ERROR);
	}

	ims_qiFreeDesc (qDesc);

	/*
	** Build the aggregate which contains the user name, and 
	** scan items for each dataset which has an acquisition on the
	** available tape.
	*/

	if (buildAggregate(msgDesc, table_names,  &root, userSpec) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not build the aggregate for ordering scan jobs");
		return(IMS_ERROR);
	}

	while (table_names != NULL)
	{
		ptr = table_names;
		table_names = table_names->next;
		free(ptr);
	}

	/*
	** Submit the scan job to ims_order processing.
	*/

#ifndef NO_JULIE

	if (ims_order(msgDesc, root, &order_id) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not order scan job");
		return(IMS_ERROR);
	}
#endif

	return(IMS_OK);
}




/******************************************************************************
**
** execCmd ()
**
******************************************************************************/


static int execCmd (
IMS_MSG_STRUCT *msgDesc, 
IMS_QI_DESC_OBJ *qDesc)
{
	int status;
	int severity;

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
    {
		if (status < IMS_OK)
		{
			return (ims_msgGetSeverity (msgDesc));
		}
	}

	/*
	** Check the stored procedure status returned value.
	*/
	if ((severity = ims_msgGetSeverity (msgDesc)) < IMS_OK)
	{
		return (severity);
	}
	 
	if (qDesc->msgNo != 0)
 	{
		return (ims_msgGetSeverity (msgDesc));
 	}
		  
	return (IMS_OK);
}
							  


/******************************************************************************
**
** locateTape ()
**
******************************************************************************/

static int locateTape(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	USERSPEC *userSpec,
	char *granuleName,
	int granuleIdx,
	TABLE_NAMES **table_names)
{

	TABLE_NAMES *ptr;
	char *downlink_tables[] =
		{{"RADARSAT-1 RAW SIGNAL SEGMENT"},
		 {"ERS-1 RAW SIGNAL SEGMENT"},
		 {"ERS-2 RAW SIGNAL SEGMENT"},
		 {"JERS-1 RAW SIGNAL SEGMENT"},
		 {"ADEOS-1 RAW SIGNAL SEGMENT"}};
	
	char *platforms[] =
		{{"RADARSAT-1"}, {"ERS-1"}, {"ERS-2"}, {"JERS-1"}, {"ADEOS-1"}};

	char *splatforms[] =
		{{"R1"}, {"E1"}, {"E2"}, {"J1"}, {"A1"}};

	int datasetCount;
	char tableName[IMS_COL30_LEN+1];
	int status;
	char qbuf[IMS_COL512_LEN+1];
	char fileName[IMS_COL30_LEN+1];
	char sensor[IMS_COL15_LEN+1];
	int rev;
	short int seq;
	int startScan, quickFlag;
	
	/*
	** Scan through the granules tables for the different platforms.
	*/

	*table_names = NULL;
	ptr = NULL;
	datasetCount = 5;

	while (datasetCount > 0)
	{
		/*
		** Get the name of the granules table.
		*/

		if (getGranuleTableName(msgDesc, qDesc, userSpec, tableName, 
			downlink_tables[datasetCount-1], platforms[datasetCount-1]) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get granule table name for platform %s", 
				platforms[datasetCount-1]);
			return(IMS_ERROR);
		}

		/*
		** Determine whether the downlink data for this dataset has
		** any media that matches the media id for the tape availabile 
		** message.
		*/

		(void) sprintf(qDesc->cmd,
			"select d.name, SENSOR, REVOLUTION, SEQUENCE "
			"from %s g, %s d "
			"where g.granule_idx = %d "
			"and d.status = 1 "
			"and (g.MEDIA_ID_TYPE_NAME = 'WORKING_SIGNAL' "
			"or g.MEDIA_ID_TYPE_NAME = 'QUICKLOOK' "
			"or g.MEDIA_ID_TYPE_NAME = 'TEST')"
			"and (d.RECORDER_TYPE = 'ID-1' or "
			"  (d.RECORDER_TYPE = 'DCRSI' and d.DATA_DIRECTION = 'FORWARD')) "
			"and g.MEDIA_ID = d.MEDIA_ID "
			"and g.STATUS = 'HST_S_AVAILABLE' "
			"and d.STATUS = 'HST_S_OK'",
			granuleName, tableName, granuleIdx);

		while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
		{
			if (status < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not perform query of %s.", granuleName);
				(void) ims_qiFreeDesc (qDesc);
				return (IMS_ERROR);
			}

			if (status == IMS_ENDOFQUERY)
			{
				continue;
			}

			/*
			** Get sensor
			*/

			(void) memset(sensor, 0, sizeof(sensor));
			(void) memcpy((char *) sensor,
				qDesc->valAddr[1], qDesc->valLength[1]);
			(void) ims_trim(sensor);

			/*
			** Get revolution
			*/

			(void) memcpy((char *) &rev,
				qDesc->valAddr[2], qDesc->valLength[2]);

			/*
			** Get sequence
			*/

			(void) memcpy((char *) &seq,
				qDesc->valAddr[3], qDesc->valLength[3]);

			if (checkDatatake(msgDesc, userSpec, splatforms[datasetCount-1],
				sensor, rev, seq, &startScan, &quickFlag, IMS_TRUE) < IMS_OK)
			{
				if (checkDatatake(msgDesc, userSpec, splatforms[datasetCount-1],
					sensor, rev, seq, &startScan, &quickFlag, IMS_FALSE) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not perform query of datake info for scan check. Platform = %s Sensor = %s Rev = %d Seq = %d.", splatforms[datasetCount - 1], sensor,
						rev, seq);
					(void) ims_qiFreeDesc (qDesc);
					return (IMS_ERROR);
				}

			}

			/*
			** Continue sunce we don't scan tapes unless there is a datake
			** found with PROCESS_AUTH_FLAG = true.
			*/


			if (startScan == IMS_FALSE)
			{
				continue;

			}

			/*
			** This is a granule table we need. Add the name.
			*/
			if (ptr == NULL)
			{
				ptr = malloc(sizeof(TABLE_NAMES));
				*table_names = ptr;
				ptr->prev = NULL;
			}
			else
			{
				ptr->next = malloc(sizeof(TABLE_NAMES));
				ptr->next->prev = ptr;
				ptr = ptr->next;
			}

			if (ptr == NULL)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not allocate space for granule table list");
				return(IMS_ERROR);
			}

			ptr->next = NULL;

			(void) strcpy(ptr->dataset, downlink_tables[datasetCount-1]);

			(void) strcpy(ptr->platform, platforms[datasetCount-1]);
			(void) ims_trim(ptr->platform);	

			(void) memset(ptr->granuleName, 0, sizeof(ptr->granuleName));
			(void) memcpy((char *) ptr->granuleName,
				qDesc->valAddr[0], qDesc->valLength[0]);
			(void) ims_trim(ptr->granuleName);

			/*
			** Check Quicklook Flag.
			*/
			if (quickFlag == IMS_TRUE)
			{
				ptr->quick_look = 'Y';
			}
			else
			{
				ptr->quick_look = 'N';
			}
		}


		if (ims_qiResetDesc (qDesc) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not reinitialize the query descriptor.");
			(void) ims_qiFreeDesc (qDesc);
			return (IMS_FATAL);
		}

		datasetCount --;
	}

	return(IMS_OK);
}

/******************************************************************************
**
** getGranuleTableName ()
**
******************************************************************************/

static int getGranuleTableName(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	USERSPEC *userSpec,
	char *tableName,
	char *dataset_name,
	char *platform)
{
	int status;
	char qbuf[IMS_COL512_LEN];

	sprintf(qDesc->cmd,
		"select p.granules_table from dataset_relation r, \
			dataset_policy p where r.dataset = '%s' and \
			p.dataset_idx = r.dataset_idx and \
			r.platform = '%s'",
			dataset_name, platform);
		
	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
					   "Could not perform query of dataset relation.");
			ims_qiFreeDesc (qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		(void) memcpy((char *) tableName,
					qDesc->valAddr[0], qDesc->valLength[0]);
		tableName[qDesc->valLength[0]] = '\0';
		ims_trim(tableName);

	}

	if (IMS_AFFECTED(qDesc) != 1) 
	{
		/* 
		** This is a problem ...
		*/

		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not locate granule name for dataset %s", dataset_name);

		if (ims_qiResetDesc (qDesc) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not reinitialize the query descriptor.");
			ims_qiFreeDesc (qDesc);
			return (IMS_FATAL);
		}

		return(IMS_ERROR);

	}

	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not reinitialize the query descriptor.");
		ims_qiFreeDesc (qDesc);
		return (IMS_FATAL);
	}

	return(IMS_OK);
}

/******************************************************************************
**
** buildAggregate ()
**
** Build the ODL Aggregate for the scan message.
**
******************************************************************************/
static int buildAggregate(
	IMS_MSG_STRUCT *msgDesc,
	TABLE_NAMES *table_names,
	AGGREGATE *root,
	USERSPEC *userSpec)
{
	IMS_ODL_TREE *hdr, *user, *scan;



	(void) ims_addODLObject(msgDesc, NULL, &hdr, 
		"SCAN_REQUEST", FALSE, IMS_OBJECT);

	(void) ims_addODLObject(msgDesc, hdr,  &user,
		"USER_INFO", TRUE, IMS_OBJECT);

	(void) ims_addODLKeyword(msgDesc, user, "FIRST_NAME",
		TV_STRING, userSpec->first_name);

	(void) ims_addODLKeyword(msgDesc, user, "LAST_NAME",
		TV_STRING, userSpec->last_name);

	(void) ims_addODLKeyword(msgDesc, user, "AUTHENTICATOR",
		TV_STRING, userSpec->clientPassword);

	(void) ims_addODLKeyword(msgDesc, user, "BILLING_ID",
		TV_STRING, userSpec->accountId);

	/*
	** Build an object for each scan item
	*/


	while (table_names != NULL)
	{
		(void) ims_addODLObject(msgDesc, hdr,  &scan, 
			"SCAN_ITEM", TRUE, IMS_OBJECT);

		(void) ims_addODLKeyword(msgDesc, scan, "PLATFORM",
			TV_STRING, table_names->platform);

		(void) ims_addODLKeyword(msgDesc, scan, "DATASET",
			TV_STRING, table_names->dataset);
		
		(void) ims_addODLKeyword(msgDesc, scan, "FILENAME",
			TV_STRING, table_names->granuleName);

		if (table_names->quick_look == 'Y')
		{
			(void) ims_addODLKeyword(msgDesc, scan, "QUICK_LOOK",
				TV_STRING, "Y");
		}				
		else
		{
			(void) ims_addODLKeyword(msgDesc, scan, "QUICK_LOOK",
				TV_STRING, "N");
		}

		table_names = table_names->next;
	}

#ifndef NO_JULIE
	if (ims_buildAggregate(msgDesc, hdr, root) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not build the aggregate");
		return(IMS_ERROR);
	}
#else
	if (ims_buildPMF(msgDesc, hdr, "DANTEST.MTA", NULL) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not build the aggregate");
		return(IMS_ERROR);
	}
#endif

	return(IMS_OK);
}

/******************************************************************************
**
** getUserInfo
**
******************************************************************************/

static int getUserInfo(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	char *userName,
	char *first_name,
	char *last_name)

{
	int status;

	sprintf(qDesc->cmd,
		"select first_name, last_name from user_profile where \
		user_id = '%s'", userName);
		
	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
					   "Could not perform query of user information.");
			ims_qiFreeDesc (qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** First Name
		*/

		(void) memcpy((char *) first_name,
					qDesc->valAddr[0], qDesc->valLength[0]);
		first_name[qDesc->valLength[0]] = '\0';
		ims_trim(first_name);

		/*
		** Last Name
		*/

		(void) memcpy((char *) last_name,
					qDesc->valAddr[1], qDesc->valLength[1]);
		last_name[qDesc->valLength[1]] = '\0';
		ims_trim(last_name);

	}
	if (IMS_AFFECTED(qDesc) != 1)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not determine user first/last name");
		return(IMS_ERROR);

	}


	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not reinitialize the query descriptor.");
		ims_qiFreeDesc (qDesc);
		return (IMS_FATAL);
	}


	return(IMS_OK);

}

/******************************************************************************
**
** checkDatatake
**
** Check the datatakes for the downlink and see if the PROCESS_AUTH_FLAG
** and/or the QUICKLOOK_FLAG is set in the datatake.
**
** datatake_flag indicates that the datatake should search for corresponding
** datatakes to a downlink, otherwise it searches for a specific datatake.
******************************************************************************/

static int checkDatatake(
	IMS_MSG_STRUCT *msgDesc, 
	USERSPEC *userSpec, 
	char *platform, 
	char *sensor, 
	int rev, 
	int seq,
	int *start_scan, 
	int *quick_look_set,
	int datatake_flag) 
{
	IMS_QI_DESC_OBJ *qDesc;
	int status;
	char qbuf[IMS_COL512_LEN+1];
	int rowCount;
	char quick_flag[4];
	char auth_flag[4];


	*start_scan = IMS_FALSE;
	*quick_look_set = IMS_FALSE;


	/*         
	** Allocate a query descriptor
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		return(IMS_ERROR);
	}	
	

	IMS_SETUSER (qDesc, userSpec->username);
	IMS_SETPSWD (qDesc, userSpec->password);
	IMS_SETPROG (qDesc, userSpec->program);
	IMS_SETSERVER (qDesc, userSpec->server);
	IMS_SETDBNAME (qDesc, userSpec->database);
	IMS_SET_VERBOSE (qDesc, 10);

	status = ims_qiLogin(qDesc);

	if (status < IMS_OK)
	{

		(void) ims_msg(msgDesc, status, "Could not login to database.");

		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}


	IMS_SET_USERDATA(qDesc);
	qDesc->cmd = qbuf;


	if (datatake_flag == IMS_TRUE)
	{
		(void) sprintf (qbuf,
 	   "select PROCESS_AUTH_FLAG, QUICKLOOK_FLAG from datatake_entry where "
			"DT_PLATFORM = '%s' and DT_SENSOR = '%s' and DT_SEQUENCE = %d and "
			"DT_REVOLUTION = %d", platform, sensor, seq, rev);
	}
	else
	{
		(void) sprintf (qbuf,
   	 "select PROCESS_AUTH_FLAG, QUICKLOOK_FLAG from datatake_entry where "
			"PLATFORM = '%s' and SENSOR = '%s' and SEQUENCE = %d and "
			"REVOLUTION = %d", platform, sensor, seq, rev);
	}


	rowCount = 0;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				   "Could not perform query of datatake_entry table.");
			(void) ims_qiFreeDesc (qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		rowCount ++;

		/*
		** Get the results if interested.  Maybe they will be of use?
		*/

		(void) memcpy((char *) auth_flag, qDesc->valAddr[0],
				qDesc->valLength[0]);


		(void) memcpy((char *) quick_flag, qDesc->valAddr[1],
				qDesc->valLength[1]);


		if (auth_flag[0] == 'Y')
			*start_scan = IMS_TRUE;

		if (quick_flag[0] == 'Y')
			*quick_look_set = IMS_TRUE;


	}

	/*
	** If the row already is there, then don't add.
	*/


 	(void) ims_qiFreeDesc (qDesc);

	if (rowCount == 0)
		return(IMS_ERROR);

	return(IMS_OK);


}
