static char *sccs = "@(#)ims_cmnAux.c	5.7  06/09/97";
/******************************************************************************
**
** File:	ims_cmnAux.c
**
** Function: Provides common routines for FTS/FA Reports backend processing.
**
** Author: Dan Crichton	
**
** Date:	11/15/95
**
** Modification: 5/6/97 D. Ting
**							 R2.1 Query based on station_id for fa_tracking.
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

#include <ims_job_control.h>
#include <ims_fa_track.h>

static int addToHistory(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, IMS_FA_TRACKING *);
static int moveAcqToHistory(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, IMS_FA_TRACKING *);


/******************************************************************************
**
** ims_addToTrackingTable ()
**
** Add a list of tracking messages to the fa_tracking table.  This table
** is used by the FA Scheduler to track fa reports and start jobs.
******************************************************************************/

int ims_addToTrackingTable(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	IMS_FA_TRACKING *track)
{
	char qbuf[IMS_COL1024_LEN+1];
	char *savebuf;
	int status;



	/*
	** Insert into tracking table 
	*/

	savebuf = qDesc->cmd;
	qDesc->cmd = qbuf;



	while (track != NULL)
	{

		sprintf(qbuf,
			"insert into fa_tracking (orbit, message_id, platform, \
			start_time, end_time, sequence, report_id, name, plan_num, station_id, \
			received_date) \
			values (%d, %d, '%s', '%s', '%s', %d, %d,'%s', '%s', '%s', '%s')",
			track->orbit_id, track->message_id, track->platform, 
			track->start_time, track->end_time, track->segment_id,
			track->report_id, track->name, track->plan_num, track->station_id, 
			track->received_date); /* R2.1 */
		track = track->next;

		
		while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
		{
			if (status < IMS_OK)
			{
				qDesc->cmd = savebuf;
				return (ims_msgGetSeverity (msgDesc));
			}
		}


		if (ims_qiResetDesc (qDesc) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not reinitialize the query descriptor.");
			qDesc->cmd = savebuf;
			return (IMS_FATAL);
		}

	}


	qDesc->cmd = savebuf;
	return(IMS_OK);
}


/******************************************************************************
**
** ims_deleteFromTrackingTable ()
**
** This will delete a list of tracking entries from the tracking table.
** If values are left NULL or zero, then those values will not be used
** as constraints to the SQL statement.
******************************************************************************/

int ims_deleteFromTrackingTable(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	IMS_FA_TRACKING *track)
{
	char qbuf[IMS_COL1024_LEN+1];
	char *savebuf;
	char workbuf[128];
	int status;
	char *platform;



	/*
	** Delete from tracking table 
	*/

	savebuf = qDesc->cmd;
	qDesc->cmd = qbuf;



	while (track != NULL)
	{
		/*
		** move the acquisition to the history table.
		*/

		if (moveAcqToHistory(msgDesc, qDesc, track) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not move the acquisition to the history table.");
		}

		sprintf(qbuf,
			"delete from fa_tracking where ");
		/*
		** Add parameters 
		*/

		if (track->orbit_id > 0)
		{
			sprintf(workbuf, "orbit = %d and ", track->orbit_id);
			strcat(qbuf, workbuf);
		}

		if (track->message_id > 0)
		{
			sprintf(workbuf, "message_id = %d and ", track->message_id);
			strcat(qbuf, workbuf);
		}
		
		if (track->platform[0] != '\0')
		{
			/*
			** Map platform to acronym for consistency.
			*/

			platform = track->platform;

			if (strcmp(platform, "ERS-1") == 0)
				platform = "E1";
			if (strcmp(platform, "ERS-2") == 0)
				platform = "E2";
			if (strcmp(platform, "JERS-1") == 0)
				platform = "J1";
			if (strcmp(platform, "ADEOS-1") == 0)
				platform = "A1";
			if (strcmp(platform, "RADARSAT-1") == 0)
				platform = "R1";

			sprintf(workbuf, "platform = '%s' and ", platform);
			strcat(qbuf, workbuf);
		}

		if (track->start_time[0] != '\0')
		{
			sprintf(workbuf, "start_time >= '%s' and ", track->start_time);
			strcat(qbuf, workbuf);
		}

		if (track->end_time[0] != '\0')
		{
			sprintf(workbuf, "end_time <= '%s' and ", track->end_time);
			strcat(qbuf, workbuf);
		}

		if (track->station_id[0] != '\0') /* R2.1 */
		{
			sprintf(workbuf, "station_id = '%s' and ", track->station_id);
			strcat(qbuf, workbuf);
		}

		if (track->plan_num[0] != '\0')
		{
			sprintf(workbuf, "plan_num = '%s' and ", track->plan_num);
			strcat(qbuf, workbuf);
		}

		if (track->report_id > 0)
		{
			sprintf(workbuf, "report_id = %d and ", track->report_id);
			strcat(qbuf, workbuf);
		}

		if (track->name[0] != '\0')
		{
			sprintf(workbuf, "name = '%s' and ", track->name);
			strcat(qbuf, workbuf);
		}


		if ((int) strlen(qbuf) <= (int) 32)
		{
			track = track->next;
			continue;
		}

		qbuf[strlen(qbuf) - 4] = '\0'; 
		(void) ims_msg(msgDesc, IMS_INFO,  qbuf);

		track = track->next;
		
		while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
		{
			if (status < IMS_OK)
			{
				qDesc->cmd = savebuf;
				return (ims_msgGetSeverity (msgDesc));
			}
		}

		if (ims_qiResetDesc (qDesc) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not reinitialize the query descriptor.");
			return (IMS_FATAL);
		}
	}


	qDesc->cmd = savebuf;
	return(IMS_OK);
}


/******************************************************************************
**
** moveAcqToHistory
**
** This function will move the acquisition to the history table named
** fa_tracking_history.
** 
******************************************************************************/

static int moveAcqToHistory(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	IMS_FA_TRACKING *track)
{
	char qbuf[IMS_COL512_LEN];
	char *savebuf;
	char *platform;
	char workbuf[128];
	int status;
	IMS_FA_TRACKING *track_new, *ptr;
	short int segment_id_temp;/*R2.1*/


	savebuf = qDesc->cmd;
	qDesc->cmd = qbuf;
	
	sprintf(qbuf,
			"select orbit, sequence, platform, start_time, name \
			from fa_tracking where ");
	/*
	** Add parameters 
	*/

	if (track->orbit_id > 0)
	{
		sprintf(workbuf, "orbit = %d and ", track->orbit_id);
		strcat(qbuf, workbuf);
	}

	if (track->message_id > 0)
	{
		sprintf(workbuf, "message_id = %d and ", track->message_id);
		strcat(qbuf, workbuf);
	}
	
	if (track->platform[0] != '\0')
	{
		/*
		** Map platform to acronym for consistency.
		*/

		platform = track->platform;

		if (strcmp(platform, "ERS-1") == 0)
			platform = "E1";
		if (strcmp(platform, "ERS-2") == 0)
			platform = "E2";
		if (strcmp(platform, "JERS-1") == 0)
			platform = "J1";
		if (strcmp(platform, "ADEOS-1") == 0)
			platform = "A1";
		if (strcmp(platform, "RADARSAT-1") == 0)
			platform = "R1";

		sprintf(workbuf, "platform = '%s' and ", platform);
			strcat(qbuf, workbuf);
	}

	if (track->start_time[0] != '\0')
	{
		sprintf(workbuf, "start_time >= '%s' and ", track->start_time);
		strcat(qbuf, workbuf);
	}

	if (track->end_time[0] != '\0')
	{
		sprintf(workbuf, "end_time <= '%s' and ", track->end_time);
		strcat(qbuf, workbuf);
	}

	if (track->station_id != '\0')
	{
		sprintf(workbuf, "station_id = '%s' and ", track->station_id);
		strcat(qbuf, workbuf);
	}

	if (track->plan_num[0] != '\0')
	{
		sprintf(workbuf, "plan_num = '%s' and ", track->plan_num);
		strcat(qbuf, workbuf);
	}

	if (track->report_id > 0)
	{
		sprintf(workbuf, "report_id = %d and ", track->report_id);
		strcat(qbuf, workbuf);
	}

	if (track->name[0] != '\0')
	{
		sprintf(workbuf, "name = '%s' and ", track->name);
		strcat(qbuf, workbuf);
	}


	if ((int) strlen(qbuf) <= (int) 32)
	{
		track = track->next;
	}

	qbuf[strlen(qbuf) - 4] = '\0'; 

	(void) ims_msg(msgDesc, IMS_INFO,  qbuf);

	track_new = NULL;

	/*printf("%s\n", qDesc->cmd);*/
	
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			qDesc->cmd = savebuf;
			return (ims_msgGetSeverity (msgDesc));
		}

		if (status == IMS_ENDOFQUERY)
			continue;

		/*
		** Move the acqusitions over 
		*/

		if (track_new == NULL)
		{
			track_new = malloc(sizeof(IMS_FA_TRACKING));
			ptr = track_new;
			
		}
		else
		{
			ptr->next = malloc(sizeof(IMS_FA_TRACKING));
			ptr = ptr->next;
		}

		if (ptr == NULL)
		{
			/*
			** Error out...we couldn't get memory...
			*/
			(void) ims_msg(msgDesc, IMS_FATAL,
				"COuldn't get any memory to move tracking info.");

			return(IMS_FATAL);
		}

		/*
		** Okay.  setup the new ptr.
		*/
		
		ptr->next = NULL;
		memcpy(&(ptr->orbit_id), qDesc->valAddr[0], 4);
		memcpy(&(segment_id_temp), qDesc->valAddr[1], 2); /* R2.1*/
		ptr->segment_id = (int) segment_id_temp; /*R2.1*/
		memcpy(ptr->platform, qDesc->valAddr[2], qDesc->valLength[2]);
		memcpy(ptr->start_time, qDesc->valAddr[3], qDesc->valLength[3]);
		memcpy(ptr->name, qDesc->valAddr[4], qDesc->valLength[4]);

		ptr->platform[qDesc->valLength[2]] = '\0';
		ptr->start_time[qDesc->valLength[3]] = '\0';
		ptr->name[qDesc->valLength[4]] = '\0';

	}

	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not reinitialize the query descriptor in moveAcqToHistory.");
		return (IMS_FATAL);
	}

	if (addToHistory(msgDesc, qDesc, track_new) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not add tracking information to fa_tracking_history.");
		return(IMS_ERROR);
	}

	qDesc->cmd = savebuf;
	return(IMS_OK);

}

/******************************************************************************
**
** addToHistory
**
** This function will add to the fa_tracking_history table.
** 
******************************************************************************/

static int addToHistory(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	IMS_FA_TRACKING *track)

{
	int status;
	char qbuf[IMS_COL512_LEN];
	IMS_FA_TRACKING *ptr;

	qDesc->cmd = qbuf;

	while (track != NULL)
	{
		sprintf(qbuf, 
			"insert into fa_tracking_history \
			(orbit, platform, start_time, sequence, name,\
			granule_idx, dataset_idx) values \
			(%d, '%s', '%s', %d, '%s', %d, %d)", track->orbit_id, 
			track->platform,
			track->start_time, track->segment_id, track->name, -1, -1);

		while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
		{
			if (status < IMS_OK)
			{
				return (ims_msgGetSeverity (msgDesc));
			}
		}

		if (ims_qiResetDesc (qDesc) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not reinitialize the query descriptor in moveAcqToHistory.");
			return (IMS_FATAL);
		}

		ptr = track; 
		track = track->next;
		free(ptr);
	}
	return(IMS_OK);
}
