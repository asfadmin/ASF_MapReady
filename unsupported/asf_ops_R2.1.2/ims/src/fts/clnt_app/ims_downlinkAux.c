char *sccs = "@(#)ims_downlinkAux.c	5.11  07/17/97";
/******************************************************************************
**
** File:        ims_downlinkAux.c
**
** Function:    Perform all auxillary processing using a pipe with the 
**              FTS network server for processing downlink files .
**							This aux process performs the following functions:
**								1) Check the datatake->downlink entry map
**								2) Add new acquisitions to the reporting tables 
**								3) Initiate scan jobs 
**
** Author:      Dan Crichton	
**
** Date:        11/15/95
** 
** Modified:	Dan Crichton	R2.1
**						4/9/97 - Modify to support R2.1 requirements.
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
#include <ims_fa_track.h>


#define IMS_FA_DOWNLINK 	1  /* Look at this !!! */


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


/*
** External Functions
*/
extern char *ims_extractFileName (char *);
extern int ims_forwardMsgQueue (IMS_MSG_STRUCT *);

/*
** Local Functions
*/
static int execCmd (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static int processDownlink(IMS_MSG_STRUCT *, USERSPEC *, int, char *, char *, char *);
static int checkScan(IMS_MSG_STRUCT *, USERSPEC *, int, char *, char *, char *,
	char *, char *, char *, int, int);
static int getGranuleTableName(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
	char *, char *, char *);
static int buildAggregate(IMS_MSG_STRUCT *, char *, char *, char *, char *,
	char *, AGGREGATE *, USERSPEC *);

static int getUserInfo(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *,
	char *, char *, char *);

static int findFATracking(IMS_MSG_STRUCT *, USERSPEC *, char *,
	int, int, char *, int *);

static int findDatatakeInfo(IMS_MSG_STRUCT *msgDesc,
	USERSPEC *, char *, char *, int *, int *, char *,
	char *, int *, int *, int *);

static int findDownlinkInfo(IMS_MSG_STRUCT *, USERSPEC *,
	char *, int, char *, char *, int *, int *);

static int checkDownlink(IMS_MSG_STRUCT *, USERSPEC *, 
	char *, char *, int, int, char *, char *);

static int checkDatatake(IMS_MSG_STRUCT *, USERSPEC *, char *, char *,
	int, int, int *, int *, int);

/*
** Global Variables
*/

static char *glb_programName;
IMS_MSG_STRUCT *msgDesc;
static int glbl_update_off;


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
	char platform[IMS_COL15_LEN+1];
	char sensor[IMS_COL10_LEN+1];
	int rev, seq;
	char dt_platform[IMS_COL15_LEN+1];
	char dt_sensor[IMS_COL10_LEN+1];
	int dt_rev, dt_seq;
	char fa_schedule_link[IMS_COL15_LEN+1];
	int found;
	char station_id[IMS_COL15_LEN+1];
	int dtFlag;


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

	glbl_update_off = IMS_FALSE;


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
	**
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
	(void) strcpy (cmds[2], "CMD:0:fileName: ");
	(void) strcpy (cmds[3], "CMD:0:repositoryDir: ");
	(void) strcpy (cmds[4], "CMD:0:server: ");
	(void) strcpy (cmds[5], "CMD:0:dbName: ");
	(void) strcpy (cmds[6], "CMD:0:granuleName: ");
	(void) strcpy (cmds[7], "CMD:0:granuleIdx: ");
	(void) strcpy (cmds[8], "CMD:0:accountId: ");
	(void) strcpy (cmds[9], "CMD:0:clientUser: ");
	(void) strcpy (cmds[10], "CMD:0:clientPassword: ");
	(void) strcpy (cmds[11], "CMD:0:platform: ");
	(void) strcpy (cmds[12], "CMD:0:dataset: ");


#ifdef AUX_DEBUG
	fprintf(stderr, "Auxilary Process %s preparing requests to FTS Server\n",
			argv[0]);
#endif


	/*
	** request for required information
	*/
	cmdLen = 13;
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

	userSpec.username = replies[0];
	userSpec.password = replies[1];
	userSpec.server   = replies[4];
	userSpec.database = replies[5];
	userSpec.program = "ims_downlinkAux";
	userSpec.accountId = ims_trim(replies[8]);
	userSpec.clientUser = ims_trim(replies[9]);
	userSpec.clientPassword = ims_trim(replies[10]);

    /*
		** Don't update status to ACQUIRED if not running
		** through FTS
		*/
		if (strcmp (replies[3], ".") == 0)
		{
			glbl_update_off = IMS_TRUE;
		}

		/*
		** Get the downlink information from the granule...
		*/

		if (findDownlinkInfo(msgDesc, &userSpec, replies[6],
			atoi(replies[7]), platform, sensor, &rev, &seq) < IMS_OK)
		{
			 /*
			 ** Major error. We didn't find the granule???
			 */

			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get the downlink info for granule_idx %d in table %s", 
				atoi(replies[7]), replies[6]);

				goto exit_aux_proc;

		}

		/*
		** Set the datatake flag to off.
		*/
		dtFlag = IMS_FALSE;

		/*
		** Check if the downlink info from the granule is found
		** in the downlink_entry table.
		*/

		if (checkDownlink(msgDesc, &userSpec, platform,
			sensor, rev, seq, fa_schedule_link, station_id) < IMS_OK)
		{
			/*
			** Not found.  Try checking the datatake entries...
			*/

			if (findDatatakeInfo(msgDesc, &userSpec, platform,
				sensor, &rev, &seq, dt_platform, dt_sensor,
				&dt_rev, &dt_seq, &dtFlag) < IMS_OK)
			{

				(void) ims_msg(msgDesc, IMS_ERROR,
			"No downlink to datatake map entry found for platform '%s', sensor '%s',\
					revolution '%d', sequence '%d'.",platform, sensor, rev, seq);

				goto exit_aux_proc;
			}

			/* Check again and see if we can map the datatake to an entry
			** in the downlink_entry table.
			*/

			if (checkDownlink(msgDesc, &userSpec, platform,
				sensor, rev, seq, fa_schedule_link, station_id) < IMS_OK)
			{
				 /*
				 ** Major error. No parent for datatake.
				 */

				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not get the datatake->downlink map entry for \
					platform '%s', sensor '%s', revolution '%d', sequence '%d'",
					platform, sensor, rev, seq);

				goto exit_aux_proc;
			}
		}

		/*
		** Determine whether we should generate a report for this
		** data sequence.  If it was a datatake, skip it completely.
		*/
		if (dtFlag == IMS_FALSE)
		{
			found = IMS_FALSE;

			/*
			** For now, don't add to tracking table if repositoryDir is
			** '.'.  This means that the aux program is being run from
			** ims_sequenceScan.  There should be another way to do this
			** in R2.1. - S. Hardman 3/3/97
			*/
			if (strcmp (replies[3], ".") == 0)
			{
				found = IMS_TRUE;
			}

			/*
			** Check the fa_tracking table for this entry.
			*/
			if (found == IMS_FALSE)
			{
				if (findFATracking(msgDesc, &userSpec, platform, rev, seq,
					"fa_tracking", &found) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not query fa_tracking table"); 
					goto exit_aux_proc;
				}
			}

			/*
			** Check the fa_tracking_history table for this entry.
			**
			if (found == IMS_FALSE)
			{
				if (findFATracking(msgDesc, &userSpec, platform, rev, seq,
					"fa_tracking_history", &found) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not query fa_tracking table"); 
					goto exit_aux_proc;
				}

			}

			/*
			** If the station identifier is not for FA or MC, then just act like
			** we found it in the fa_tracking tables and don't add it to be
			** reported on.
			*/
			if ((strncmp(station_id, "MC",2) != 0) &&
					(strncmp(station_id, "FA",2) != 0))
			{
				found = IMS_TRUE;
			}

			/*
			** If we have not reported on the downlink yet, then add
			** it to the fa_tracking table.
			*/
			if (found == IMS_FALSE)
			{
				if ((status = processDownlink(msgDesc, &userSpec,
					atoi(replies[7]), replies[6], fa_schedule_link,
					station_id)) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not process the downlink for granule_idx %d", 
						atoi(replies[7]));
				}
			}
		}

	/*
	** Check if a scan job should be submitted.
	*/
	if (dtFlag == IMS_FALSE)
	{
		if (checkScan(msgDesc, &userSpec, atoi(replies[7]), 
			replies[6], replies[11], replies[12], replies[2],
			platform, sensor, rev, seq) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Aux process failed checking the scan job information.");
		}
	}
	else /* The message was a datatake. */
	{
		if (checkScan(msgDesc, &userSpec, atoi(replies[7]), 
			replies[6], replies[11], replies[12], replies[2],
			dt_platform, dt_sensor, dt_rev, dt_seq) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Aux process failed checking the scan job information.");
		}
	}

exit_aux_proc:


	/*
	** Close connection with server
	*/

	(void) ims_forwardMsgQueue (msgDesc); 


	/* free */

	ims_msgStructFree(msgDesc);


	for (i = 0; i < 20; i++)
	{ 
		free(cmds[i]);
		free(replies[i]);
	}

	exit(0);
}

/******************************************************************************
**
** processDownlink ()
**
** Add the downlink to the fa_tracking table so that FA report system
** can schedule the activity to be reported on.
**
******************************************************************************/

static int processDownlink(
	IMS_MSG_STRUCT *msgDesc,
	USERSPEC *userSpec,
	int granule_idx,
	char *granule_name,
	char *fa_schedule_link,
	char *station_id)
{
	struct 
	{
		int orbit;
		int message_id;
		char platform[IMS_COL15_LEN+1];
		char start_time[IMS_DATETIME_LEN+1];
		char end_time[IMS_DATETIME_LEN+1];
		short int segment_id;
		int report_id;
		char name[IMS_COL80_LEN+1];
	} downlink;
	IMS_QI_DESC_OBJ *qDesc;
	int status;
	char qbuf[IMS_COL512_LEN+1];
	IMS_FA_TRACKING track;
	IMS_NUMERIC_DATE dateStruct;
	int short_seq;


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

	/*
	** Get granule downlink information
	*/
	
	(void) sprintf (qbuf,
		"select REVOLUTION, PLATFORM, START_TIME, \
		END_TIME, SEQUENCE, name \
		from %s where \
		((MEDIA_ID_TYPE_NAME = 'FLIGHT_AGENCY' \
		and (PLATFORM = 'A1' or PLATFORM = 'J1' ))\
		or ('A1' <> PLATFORM and 'J1' <> PLATFORM and\
		MEDIA_ID_TYPE_NAME = 'ARCHIVE_SIGNAL')) and \
		granule_idx = %d", granule_name, granule_idx);


	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				   "Could not perform query of %s.", granule_name);
			(void) ims_qiFreeDesc (qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** Get orbit
		*/


		(void) memcpy((char *) &(downlink.orbit), qDesc->valAddr[0],
					 qDesc->valLength[0]);

		/*
		** Get platform
		*/

		(void) memset(downlink.platform, 0, sizeof(downlink.platform));

		(void) memcpy((char *) downlink.platform, qDesc->valAddr[1],
					 qDesc->valLength[1]);

		(void) ims_trim(downlink.platform);

		/*
		** Get start time
		*/

		(void) memset(downlink.start_time, 0, sizeof(downlink.start_time));

		(void) memcpy((char *) downlink.start_time, qDesc->valAddr[2],
					 qDesc->valLength[2]);

		(void) ims_trim(downlink.start_time);

		/* 
		** Get end time
		*/

		(void) memset(downlink.end_time, 0, sizeof(downlink.end_time));

		(void) memcpy((char *) downlink.end_time, qDesc->valAddr[3],
					 qDesc->valLength[3]);

		(void) ims_trim(downlink.end_time);


		/*
		** Get segment id
		*/

		(void) memcpy((char *) &(downlink.segment_id), qDesc->valAddr[4],
					 qDesc->valLength[4]);


		/*
		** Get file name
		*/

		(void) memset(downlink.name, 0, sizeof(downlink.name));

		(void) memcpy((char *) downlink.name, qDesc->valAddr[5],
					 qDesc->valLength[5]);

		ims_trim(downlink.name);
		 

	}

	/*
	** If we didn't get a row back let's get out of here.
	*/
	if (IMS_AFFECTED(qDesc) < 1)
	{
		(void) ims_qiFreeDesc (qDesc);
		return(IMS_OK);
	}

	/*
	** Update tracking table
	*/

	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not reinitialize the query descriptor.");
		(void) ims_qiFreeDesc (qDesc);
		return (IMS_FATAL);
	}

	track.orbit_id = downlink.orbit;
	track.message_id = IMS_FA_DOWNLINK;
	(void) strcpy(track.platform,  downlink.platform);
	(void) strcpy(track.start_time, downlink.start_time);
	(void) strcpy(track.end_time, downlink.end_time);
	track.segment_id = downlink.segment_id;
	(void) strcpy(track.plan_num, fa_schedule_link);
	track.report_id = -1;
	(void) strcpy(track.station_id, station_id);
	(void) strcpy(track.name, downlink.name);

    if (ims_getCurrentDate(msgDesc, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get current date time");
		return(IMS_ERROR);
	}

	ims_numericDateToIMSA(&dateStruct, track.received_date);


	track.next = NULL;


	if (ims_addToTrackingTable(msgDesc, qDesc, &track) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
			"Could not add downlink message into FA Tracking Table.");
		(void) ims_qiFreeDesc (qDesc);
		return (IMS_ERROR);
	}

	(void) ims_qiFreeDesc (qDesc);

	return (IMS_OK);

}

/******************************************************************************
**
** checkScan ()
**
** Check if a scan job is needed for this downlink.  This would be
** that the media_id_type_name is WORKING_SIGNAL or QUICKLOOK and a tape
** available message exists.
**
******************************************************************************/

static int checkScan(
	IMS_MSG_STRUCT *msgDesc,
	USERSPEC *userSpec,
	int granule_idx,
	char *granule_name,
	char *platform,
	char *dataset,
	char *filename,
	char *dt_platform,
	char *dt_sensor,
	int rev,
	int seq)
{
	IMS_QI_DESC_OBJ *qDesc;
	int status;
	char qbuf[IMS_COL512_LEN+1];
	char quick_look[IMS_COL10_LEN+1];
	char table[IMS_COL30_LEN+1];
	AGGREGATE root;
	int order_id;
	int rowCount;
	int start_scan, quick_look_set;

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
		(void) ims_qiFreeDesc(qDesc);
		return (IMS_ERROR);
	}

	IMS_SET_USERDATA(qDesc);
	qDesc->cmd = qbuf;

	/*
	** Get the table name for the Tape Available Messages.
	*/
	if (getGranuleTableName(msgDesc, qDesc, table,
		"HC TAPE AVAILABILITY", "HC") < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get tape available dataset.");
		return(IMS_ERROR);
	}

	/*
	** Get granule downlink information
	*/
	qDesc->cmd = qbuf;
	(void) sprintf (qbuf,
		"select d.name "
		"from %s d, %s t "
		"where d.MEDIA_ID = t.MEDIA_ID "
		"and d.granule_idx = %d "
		"and (t.MEDIA_ID_TYPE_NAME = 'WORKING_SIGNAL' or "
		"t.MEDIA_ID_TYPE_NAME = 'QUICKLOOK' or t.MEDIA_ID_TYPE_NAME = 'TEST') "
		"and (d.RECORDER_TYPE='ID-1' or (d.RECORDER_TYPE='DCRSI' and "
			"d.DATA_DIRECTION='FORWARD')) "
		"and t.status = 1 "
		"and t.STATUS = 'HST_S_AVAILABLE' "
		"and d.STATUS = 'HST_S_OK'",
		granule_name, table, granule_idx);

	rowCount = 0;


	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				   "Could not perform query of %s.", granule_name);
			(void) ims_qiFreeDesc (qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		/*
		** Get granule name.
		*/
		(void) memcpy (filename, qDesc->valAddr[0], qDesc->valLength[0]);
		filename[qDesc->valLength[0]] = '\0';
		(void) ims_trim (filename);


		/*
		** Get datatake quicklook and PROCESS_AUTH_FLAG info
		** for the downlink.
		*/


		if (checkDatatake(msgDesc, userSpec, dt_platform, dt_sensor, rev, seq,
			&start_scan, &quick_look_set, IMS_TRUE) < IMS_OK)
		{
			if (checkDatatake(msgDesc, userSpec, dt_platform, dt_sensor, rev, seq,
				&start_scan, &quick_look_set, IMS_FALSE) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
				   "Could not perform query of datatake info for scan check.");
				(void) ims_qiFreeDesc (qDesc);
				return(IMS_ERROR);
			}
		}

		if (quick_look_set == IMS_TRUE)
			strcpy(quick_look, "Y");
		else
			strcpy(quick_look, "N");


		/*
		** Continue since we don't want to scan this tape...
		*/

		if (start_scan == IMS_FALSE)
		{
				continue;
		}


		rowCount++;

	}

	if (rowCount < 1)
	{
		(void) ims_qiFreeDesc (qDesc);
		return(IMS_OK);

	}

	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not reinitialize the query descriptor.");
		(void) ims_qiFreeDesc (qDesc);
		return (IMS_FATAL);
	}


	if (getUserInfo(msgDesc, qDesc, userSpec->clientUser,
		userSpec->first_name, userSpec->last_name) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not determine user information.");
		(void) ims_qiFreeDesc (qDesc);
		return(IMS_ERROR);
	}


	(void) ims_qiFreeDesc (qDesc);


	if (buildAggregate(msgDesc, table, platform, dataset,
		filename, quick_look, &root, userSpec) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not build the aggregate record.");

		return(IMS_ERROR);
	}


#ifndef NO_JULIE
	if (ims_order(msgDesc, root, &order_id) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not submit scan job order.");
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
** getGranuleTableName ()
**
******************************************************************************/

static int getGranuleTableName(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
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
	char *table_name,
	char *platform,
	char *dataset,
	char *filename,
	char *quick_look,
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
	** Build an object for the downlink message.
	*/


	(void) ims_addODLObject(msgDesc, hdr,  &scan, 
		"SCAN_ITEM", TRUE, IMS_OBJECT);

	(void) ims_addODLKeyword(msgDesc, scan, "PLATFORM",
		TV_STRING, platform);

	(void) ims_addODLKeyword(msgDesc, scan, "DATASET",
		TV_STRING, dataset);
		
	(void) ims_addODLKeyword(msgDesc, scan, "FILENAME",
		TV_STRING, filename);

	(void) ims_addODLKeyword(msgDesc, scan, "QUICK_LOOK",
		TV_STRING, quick_look);


/*
** This is to test without submitting scan jobs....
** The output goes to a file in /tmp/DANTEST.MTA and shows
** the ODL buffer that is normally passed to ims_order.
*/


#ifndef NO_JULIE
	if (ims_buildAggregate(msgDesc, hdr, root) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not build the aggregate");
		return(IMS_ERROR);
	}
#else
    if (ims_buildPMF(msgDesc, hdr, "/tmp/DANTEST.MTA", NULL) < IMS_OK)
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
			"Could not determine user first/last name: %s",
			qDesc->cmd);
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
** checkDownlink
**
** Find the downlink in the downlink_entry table.  If it exists, then
** update the status of the downlink to ACQUIRED.  If it does not exist,
** then return and error.
**
******************************************************************************/

static int checkDownlink(
	IMS_MSG_STRUCT *msgDesc,
	USERSPEC *userSpec,
	char *platform,
	char *sensor,
	int rev,
	int seq,
	char *fa_schedule_link,
	char *station_id)
{
	IMS_QI_DESC_OBJ *qDesc;
	int status;
	char qbuf[IMS_COL512_LEN+1];
	int rowCount;


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

	/*
	** Query the downlink_entry table and look for the record.
	*/
	
	(void) sprintf (qbuf,
		"select STATION_ID, FA_SCHEDULE_LINK \
		from downlink_entry where \
		PLATFORM = '%s' and \
		SENSOR = '%s' and \
		SEQUENCE = %d and \
		REVOLUTION = %d", platform, sensor, seq, rev);

	rowCount = 0;

	/*
	** Process the rows...
	*/


	
	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				   "Could not perform query of downlink_entry table.");
			(void) ims_qiFreeDesc (qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		rowCount ++;

		/*
		** Get station identifier
		*/

		(void) memcpy((char *) station_id, qDesc->valAddr[0],
				qDesc->valLength[0]);

		/*
		** Get FA_SCHEDULE_LINK
		*/

		(void) memcpy((char *) fa_schedule_link, qDesc->valAddr[1],
				qDesc->valLength[1]);

	}

	if (rowCount == 0)
	{
		/*
		** Return an error since it was not found.
		*/

		(void) ims_qiFreeDesc (qDesc);
		return(IMS_ERROR);
	}

	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not reinitialize the query descriptor.");
		ims_qiFreeDesc (qDesc);
		return (IMS_FATAL);
	}

	/*
	** Don't update if glbl_update is off ...
	*/

	if (glbl_update_off == IMS_TRUE)
	{
		ims_qiFreeDesc(qDesc);
		return(IMS_OK);
	}

	/*
	** Update status of downlink to acquired.
	*/

	sprintf(qbuf,
		"update downlink_entry set DOWNLINK_STATUS = 'ACQUIRED' where \
		PLATFORM = '%s' and SENSOR = '%s' and REVOLUTION = %d and \
		SEQUENCE = %d", platform, sensor, rev, seq);

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				   "Could not update downlink_entry table.");
			(void) ims_qiFreeDesc (qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

	}

	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not reinitialize the query descriptor.");
		ims_qiFreeDesc (qDesc);
		return (IMS_FATAL);
	}


	(void) ims_qiFreeDesc (qDesc);

	return(IMS_OK);

}

/******************************************************************************
**
** findDownlinkInfo
**
** Find the downlink_entry information for the given downlink message.
**
******************************************************************************/

static int findDownlinkInfo(
  IMS_MSG_STRUCT *msgDesc,
	USERSPEC *userSpec,
	char *granule_name,
	int granule_idx,
	char *platform,
	char *sensor,
	int *rev,
	int *seq)
{
	IMS_QI_DESC_OBJ *qDesc;
	int status;
	char qbuf[IMS_COL512_LEN+1];
	int rowCount;
	short int short_seq;


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

	(void) sprintf (qbuf,
    "select PLATFORM, SENSOR, REVOLUTION, SEQUENCE 	\
		from %s where granule_idx = %d",
		granule_name, granule_idx);

	rowCount = 0;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				   "Could not perform query of %s table.", granule_name);
			(void) ims_qiFreeDesc (qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		rowCount ++;

		/*
		** Get platform, sensor, rev, seq...
		*/

		(void) memcpy((char *) platform, qDesc->valAddr[0],
				qDesc->valLength[0]);
		(void) memcpy((char *) sensor, qDesc->valAddr[1],
				qDesc->valLength[1]);
		(void) memcpy((char *) rev, qDesc->valAddr[2],
				qDesc->valLength[2]);
		(void) memcpy((char *) &short_seq, qDesc->valAddr[3],
				qDesc->valLength[3]);

	}

	*seq = short_seq;

 (void) ims_qiFreeDesc (qDesc);

	if (rowCount == 0)
	{
		/*
		** Return an error since it was not found.
		*/

		return(IMS_ERROR);
	}

 return(IMS_OK);

}

/******************************************************************************
**
** findDatatakeInfo
**
** Find the datatake_entry information for the given downlink message.
**
******************************************************************************/

static int findDatatakeInfo(
  IMS_MSG_STRUCT *msgDesc,
	USERSPEC *userSpec,
	char *platform,
	char *sensor,
	int *rev,
	int *seq,
	char *dt_platform,
	char *dt_sensor,
	int *dt_rev,
	int *dt_seq,
	int *dtFlag)
{
	IMS_QI_DESC_OBJ *qDesc;
	int status;
	char qbuf[IMS_COL512_LEN+1];
	int rowCount;
	char temp_platform[IMS_COL15_LEN+1];
	char temp_sensor[IMS_COL10_LEN+1];
	int temp_rev;
	short temp_seq;


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

	(void) sprintf (qbuf,
		"select PLATFORM, SENSOR, REVOLUTION, SEQUENCE 	\
		from datatake_entry where DT_PLATFORM = '%s' and DT_SENSOR = '%s' and \
		DT_SEQUENCE = %d and DT_REVOLUTION = %d",
		platform, sensor, *seq, *rev);

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
		** Get platform, sensor, rev, seq...
		*/

		(void) memcpy((char *) temp_platform, qDesc->valAddr[0],
				qDesc->valLength[0]);
		temp_platform[qDesc->valLength[0]] = '\0';
		(void) ims_truncStr (temp_platform);
		(void) memcpy((char *) temp_sensor, qDesc->valAddr[1],
				qDesc->valLength[1]);
		temp_sensor[qDesc->valLength[1]] = '\0';
		(void) ims_truncStr (temp_sensor);
		(void) memcpy((char *) &temp_rev, qDesc->valAddr[2],
				qDesc->valLength[2]);
		(void) memcpy((char *) &temp_seq, qDesc->valAddr[3],
				qDesc->valLength[3]);
	}


 (void) ims_qiFreeDesc (qDesc);
	if (rowCount <= 0)
	{
		/*
		** Return an error since it was not found.
		*/

		return(IMS_ERROR);
	}
	else /* We got a row so swap the values. */
	{
		/*
		** Move the original values to the datatake values.
		*/
		(void) strcpy (dt_platform, platform);
		(void) strcpy (dt_sensor, sensor);
		*dt_rev = *rev;
		*dt_seq = *seq;

		/*
		** Move the retrieved values to the downlink values.
		*/
		(void) strcpy (platform, temp_platform);
		(void) strcpy (sensor, temp_sensor);
		*rev = temp_rev;
		*seq = (int) temp_seq;

		/*
		**Set the datatake flag meaning the values provided matched
		** a datatake.
		*/
		*dtFlag = IMS_TRUE;
	}

 return(IMS_OK);

}

/******************************************************************************
**
** findFATracking
**
** Find the entry in the FA tracking table or the fa_tracking_history.
** These tables are for acquisition ready to be reported on, or acquisitions
** which have already been reported on respectively.
******************************************************************************/

static int findFATracking(
  IMS_MSG_STRUCT *msgDesc,
	USERSPEC *userSpec,
	char *platform,
	int rev,
	int seq,
	char *table,
	int *found)
{
	IMS_QI_DESC_OBJ *qDesc;
	int status;
	char qbuf[IMS_COL512_LEN+1];
	int rowCount;


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

	/*
	** Search for an existing rev, platform, seq in the 
	** fa_tracking table.
	*/

	(void) sprintf (qbuf,
   	 "select name from %s where \
			orbit = %d and platform = '%s' and sequence = %d", table,
			rev, platform, seq);

	rowCount = 0;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				   "Could not perform query of %s table.", table);
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
	}

	/*
	** If the row already is there, then don't add.
	*/

 	(void) ims_qiFreeDesc (qDesc);

	if (rowCount > 0)
	{
		*found = IMS_TRUE;
		return(IMS_OK);

	}

	*found = IMS_FALSE;

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
	{
		return(IMS_ERROR);
	}

	return(IMS_OK);


}
