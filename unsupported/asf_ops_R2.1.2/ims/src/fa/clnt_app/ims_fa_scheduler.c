static char *sccs = "@(#)ims_fa_scheduler.c	5.16 02/19/98";
/*****************************************************************************
*
**
** File:    ims_fa_scheduler.c
**
** Function:  This program is a scheduler which will submit FA report jobs
**            based on a set of information in the fa_scheduler table.  
**            Reports will be submitted using the IMS Job Control based
**            on time or based on the messages received and processed by
**            IMS.
**
** Author: Dan Crichton
**
** Date:    11/2/95
**
**
** Modification: D. Ting R2.1 Added message when complete report
**               				 4/17/97 Changed segemnt_id to sequence
**											 R2.1 Changed sequence and segment_id to station_id
**							 D. Ting Coy station_id at right place
**
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <sys/signal.h>
#include <sys/types.h>
#include <time.h>


#include <ims_dbms.h>
#include <ims_query.h> 
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <ims_cmd.h>
#include <syslog.h>

#include <ims_job_control.h>
#include <ims_fa_reports.h>
#include <ims_fa_track.h>

#define IMS_WAKE_UP		20		/* Every 20 seconds */
#define IMS_FA_TIMEOUT 	1800 	/* Allow 30 minutes transfer time */
#define IMS_RESET_TIME	"00:15:01" /* */

/*
** Global variables 
*/

static IMS_JOB_USER_SPEC userSpec;
static char *glb_programName;
static IMS_MSG_STRUCT *glbl_msgDesc;
int cmdDone = 0;

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *username;
	char *password;
	char *commandFile;
	char *report;
	char *server;
	char *database;
	char *help;
	char *release;
} commands;

/*
** "Itemlist" for getting the above commands from the commandline.
*/
static IMS_CMD_CLASS cmdLineElm [] =
{
	{"-U",           &commands.username},
	{"+username",    &commands.username},
	{"-P",           &commands.password},
	{"+password",    &commands.password},
	{"-R",			 &commands.report},
	{"+report", 	 &commands.report},
	{"-C",           &commands.commandFile},
	{"+commandFile", &commands.commandFile},
	{"-X",           &commands.server},
	{"+server",      &commands.server},
	{"-Y",           &commands.database},
	{"+database",    &commands.database},
	{"-h",           &commands.help},
	{"+help",        &commands.help},
	{"-r",           &commands.release},
	{"+release",     &commands.release}
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);


/*
** "Itemlist" for getting the above commands from a command file.
*/
static IMS_CMD_CLASS cmdFileElm [] =
{
	{"username",    &commands.username},
	{"password",    &commands.password},
	{"server",      &commands.server},
	{"database",    &commands.database},
	{"report",      &commands.report},
};



/* 
** Create structure to hold database cache of scheduler  
** information.
*/

typedef struct IMS_SCHEDULE_CACHE
{
	int report_id;
	char time[9]; 
	int month_day;
	int week_day;
	int period;
	int event_mask;
	int message_mask; 
	int count;
	int day_count;
	char platform[IMS_COL15_LEN+1];
	int message_count;
	int pass_id;  /* Specifies current pass for report generation */
	int seq_num;
	char station_id[IMS_COL15_LEN]; /* R2.1 */
	char plan_num[IMS_COL15_LEN+1]; /* plan for report generation */
	int time_displacement; /* Specifies how far to back up the end of a window. */
	int timeout; /* Specifies length of time to wait to generate report. */
	struct IMS_SCHEDULE_CACHE *next;
} IMS_SCHEDULE_CACHE;

/*
** Structure to cache the track outstanding flight 
** agency jobs started by the scheduler.
*/

typedef struct IMS_SCHEDULER_JOBS
{
	int report_id;
	int job_id;
	int free;
	struct IMS_SCHEDULER_JOBS *next;
} IMS_SCHEDULER_JOBS;

int glbl_shutdown = 0;
	

static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);
static IMS_SCHEDULER_JOBS *glbl_jobs;


static int getArgInput (IMS_MSG_STRUCT *);
static void usage();
static void end_report_cb(int, int, int, int, char *);
static int processElement(IMS_MSG_STRUCT *, int, IMS_SCHEDULE_CACHE *, IMS_NUMERIC_DATE *);
static int processSchedule (IMS_MSG_STRUCT *, IMS_SCHEDULE_CACHE **);
static int ims_getWeekDay (IMS_MSG_STRUCT *, IMS_NUMERIC_DATE *, int *);
static int ims_microSleep(IMS_MSG_STRUCT *, int, long);
static int loadCacheData (IMS_MSG_STRUCT *, IMS_SCHEDULE_CACHE **);
static int closeConnection (IMS_QI_DESC_OBJ *);
static int openConnection (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ **);
static int processEventElement (IMS_MSG_STRUCT *, IMS_SCHEDULE_CACHE *, 
								IMS_SCHEDULE_CACHE *, int, int);
static int checkMessages (IMS_MSG_STRUCT *, IMS_SCHEDULE_CACHE *, 
						IMS_FA_TRACKING *, int *, int *, char *);
static int cacheTracking(IMS_MSG_STRUCT *, IMS_FA_TRACKING **);
static int dumpTrackingCache(IMS_MSG_STRUCT *, IMS_FA_TRACKING *);
static int dumpStatistics(IMS_MSG_STRUCT *, IMS_SCHEDULE_CACHE *);
static int addToJobList(IMS_MSG_STRUCT *, int, int);
static int findReportJob(IMS_MSG_STRUCT *, int, int *);
static int RemoveFromJobList(IMS_MSG_STRUCT *, int);
static int performResetProcessing(IMS_MSG_STRUCT *, IMS_SCHEDULE_CACHE **);
static int dumpScheduleCache (IMS_MSG_STRUCT *, IMS_SCHEDULE_CACHE *);
static int resetCounter(IMS_MSG_STRUCT *, IMS_SCHEDULE_CACHE *);


/*******************************************************************
** 
** main
**
*******************************************************************/
void main(int argc, char *argv[])
{
	int status;
	char *ptr;
	struct utsname uname_info;    /* Structure for uname() */
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	IMS_MSG_STRUCT *msgDesc;
	IMS_SCHEDULE_CACHE *cacheData;
	
	glbl_jobs = NULL;

	/*
	** Setup message facility.
	*/

	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';  /* Null-terminate just in case */
	
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
		 
	  
	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgProgramName (msgDesc, glb_programName);
	(void) sprintf (banner, "%s::%s", hostName, glb_programName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgStderrFlag (msgDesc, IMS_OFF);

	glbl_msgDesc = msgDesc;


	/*
	** Get the command line arguments. The variable status will actually
	** contain the number of command line arguments processed upon
	** successful completion.
	*/
	if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
		cmdLineElmCount, msgDesc)) < IMS_OK)
	{
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** Check to see if we got everything off of the command line.
	*/
	if (status < argc)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"Only %d out of the %d command line arguments were processed.",
			status, argc);
	}

	/*
	** If release was specified, print it out.
	*/
	if (commands.release != (char *) NULL)
	{
		(void) ims_printVersion (stderr);
	}

	/*
	** If help has been specified, print usage and exit.
	*/
	if (commands.help != (char *) NULL)
	{
		usage ();
		(void) ims_msgStructFree (msgDesc);
		exit (0);
	}

	/*
	** If there is a command file present, then get any commands from
	** this file, then overlay all commands from the command-line, except
	** password, which will be gone by this point.
	*/
	if (commands.commandFile != (char *) NULL)
	{
		if ((status = ims_getFileParms (commands.commandFile,
			cmdFileElm, cmdFileElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msgStructFree (msgDesc);
			exit (1);
		}

		/*
		** Now, get command line arguments again to overlay file arguments.
		*/
		if ((status = ims_getCmdLine (argc, argv,
			cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msgStructFree (msgDesc);
			exit (1);
		}
	}

	/*
	** Process the information from the command-line and/or command-file.
	*/
	if ((status = getArgInput (msgDesc)) < IMS_OK)
	{
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** Cache Database 
	*/

	if (loadCacheData(msgDesc, &cacheData) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not cache database to load schedule information.");

		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	(void) dumpStatistics(msgDesc, cacheData);

	/*
	** processSchedule
	*/


	while ((processSchedule(msgDesc, &cacheData) == IMS_OK) && 
			(!glbl_shutdown));


	ims_msgStructFree(msgDesc);
	exit(0);

}

/*******************************************************************
** 
** dumpStatistics
**
** Dump the scheduling information.
**
*******************************************************************/
static int dumpStatistics(
	IMS_MSG_STRUCT *msgDesc,
	IMS_SCHEDULE_CACHE *cacheData)
{
#ifdef DEBUG
	printf("IMS/DADS Flight Agency Report Scheduler\n");
	printf("ASF -  R1B'\n\n");
	printf("End of Day Processing Occurs at %s\n", IMS_RESET_TIME);
	while (cacheData != NULL)
	{
		printf("REPORT %d:\n", cacheData->report_id);

		if ((cacheData->period & IMS_FA_EVERYDAY) == IMS_FA_EVERYDAY)
		{
			printf("\tGenerated Daily At %s\n", cacheData->time);
		}

		if (cacheData->period & IMS_FA_EVERYWEEK)
		{
			printf("\tGenerated Weekly on Week Day %d at %s\n",
					cacheData->week_day, cacheData->time);
		}

		if (cacheData->period & IMS_FA_EVERYMONTH)
		{
			printf("\tGenerated Monthly on Day %d at %s\n",
					cacheData->month_day, cacheData->time);
		}

		if (cacheData->event_mask & IMS_FA_MESSAGE)
		{
			printf("\tDriven by Event Messages: \n");

			if (cacheData->message_mask & IMS_FA_DOWNLINK)
			{
				printf("\t\tDOWNLINK MESSAGE\n");
			}

			if (cacheData->message_mask & IMS_FA_SCANNED_RESULTS)
			{
				printf("\t\tSCANNED RESULTS\n");
			}

			if (cacheData->message_mask & IMS_FA_PCD)
			{
				printf("\t\tPCD\n");
			}
		}

		if (cacheData->event_mask & IMS_FA_PASS_CHANGE)
		{
			printf("\tGenerated Upon Pass Changes \n");
		}

		if (cacheData->event_mask & IMS_FA_END_OF_DAY)
		{
			printf("\tEnd of Day Processing: \n");

			if (cacheData->event_mask & IMS_RESET_COUNTER)
				printf("\t\tReset Cyclic Counter\n");

		}


		cacheData = cacheData->next;
	}

#endif
	return(IMS_OK);
}


/*******************************************************************
** 
** usage
**
*******************************************************************/
static void usage()
{
	printf("IMS/DADS FA Scheduler\n");
}


/******************************************************************************
**
** getArgInput ()
**
** Prompt for needed information not provided in the command-line
** and command-file.
**
******************************************************************************/

static int getArgInput (
	IMS_MSG_STRUCT *msgDesc)
{
	char inputBuffer[IMS_INPUT_BUF_LEN+1];
	char prompt[20];

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/

	memset((char *) &userSpec, 0, sizeof(userSpec));

	/* username */
	if (commands.username != (char *) NULL)
	{
		strcpy(userSpec.username, commands.username);
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Username: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		(void) strcpy (userSpec.username, inputBuffer);
	}

	/* password */
	if (commands.password != (char *) NULL)
	{
		strcpy(userSpec.password, commands.password);
	}
	else
	{
		if (ims_getPassword (inputBuffer) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		(void) strcpy (userSpec.password, inputBuffer);
	}

	/* server */
	if (commands.server != (char *) NULL)
	{
		strcpy(userSpec.server, commands.server);
	}


	/* database */
	if (commands.database != (char *) NULL)
	{
		strcpy(userSpec.database, commands.database);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** openConnection ()
**
** This function will open a connection to the SQL server.
** 
**
******************************************************************************/

static int openConnection (
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
														  
	IMS_SETUSER (qDesc, userSpec.username);
	IMS_SETPSWD (qDesc, userSpec.password);

	if (userSpec.program != NULL)
		IMS_SETPROG(qDesc, userSpec.program);

	if (userSpec.server != NULL)
		IMS_SETSERVER(qDesc, userSpec.server);

	if (userSpec.database != NULL)
		IMS_SETDBNAME(qDesc, userSpec.database);

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

static int closeConnection (
	IMS_QI_DESC_OBJ *qDesc)
{

	free(qDesc->cmd);
	(void) ims_qiFreeDesc(qDesc);
	return(IMS_OK);
}


/******************************************************************************
**
** loadCacheData ()
**
** This function will cache information from the fa_scheduler database.
** 
**
******************************************************************************/

static int loadCacheData (
	IMS_MSG_STRUCT *msgDesc,
	IMS_SCHEDULE_CACHE **cacheData)
{
	IMS_QI_DESC_OBJ *qDesc;
	int status;
	int rowCount = 0;
	IMS_SCHEDULE_CACHE *ptr;
	short int report_id_temp; /* R2.1 */

	/*
	** Connect to the database.
	*/

	if (openConnection(msgDesc, &qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not logon to database to cache schedule information.");
		return(IMS_ERROR);
	}

	sprintf(qDesc->cmd, "select report_id, time, month_day, week_day, \
		period_id, event_mask, message_mask, platform, message_count, \
		reports_per_day, time_displacement, timeout from fa_scheduler \
		order by report_id");

	*cacheData = NULL;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		
		if (status < IMS_OK)
		{
			closeConnection(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR, 
					"Can't cache data. Query failed.");
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


		if (rowCount == 1)
		{
			ptr = (void *) malloc(sizeof(IMS_SCHEDULE_CACHE));
			if (ptr == NULL)
			{
				(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not allocate memory to cache data");
				closeConnection(qDesc);
				return(IMS_FATAL);
			}
			*cacheData  = ptr;
		}
		else
		{
			ptr->next = (void *) malloc(sizeof(IMS_SCHEDULE_CACHE));

			if (ptr->next == NULL)
			{
				(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not allocate memory to cache data");
				closeConnection(qDesc);
				return(IMS_FATAL);
			}
			ptr = ptr->next;
		}

		ptr->next = NULL;

		/*
		** Get report_id
		*/

		(void) memcpy((char *) &report_id_temp, qDesc->valAddr[0],
			qDesc->valLength[0]);

    ptr->report_id = (int) report_id_temp;

		/*
		** Get time
		*/

		memset(ptr->time, 0, sizeof(ptr->time));

		(void) memcpy((char *) ptr->time, qDesc->valAddr[1],
			qDesc->valLength[1]);

		ims_trim(ptr->time);
		

		/*
		** Get month day
		*/

		(void) memcpy((char *) &(ptr->month_day), qDesc->valAddr[2],
			qDesc->valLength[2]);

		/*
		** Get week day
		*/

		(void) memcpy((char *) &(ptr->week_day), qDesc->valAddr[3],
			qDesc->valLength[3]);


		/*
		** Get period 
		*/

		(void) memcpy((char *) &(ptr->period), qDesc->valAddr[4],
			qDesc->valLength[4]);


		/*
		** Get event_mask
		*/

		(void) memcpy((char *) &(ptr->event_mask), qDesc->valAddr[5],
			qDesc->valLength[5]);


		/*
		** Get message_mask
		*/

		(void) memcpy((char *) &(ptr->message_mask), qDesc->valAddr[6],
			qDesc->valLength[6]);

		/*
		** Get platform
		*/

		memset(ptr->platform, 0, sizeof(ptr->platform));
		(void) memcpy((char *) ptr->platform, qDesc->valAddr[7],
			qDesc->valLength[7]);
		ims_trim(ptr->platform);

		/*
		** Get message_count
		*/

		(void) memcpy((char *) &(ptr->message_count), qDesc->valAddr[8],
			qDesc->valLength[8]);

		/*
		** Get messages_per_day
		*/

		(void) memcpy((char *) &(ptr->day_count), qDesc->valAddr[9],
			qDesc->valLength[9]);


		/*
		** Time Displacement.
		*/

		(void) memcpy((char *) &(ptr->time_displacement), qDesc->valAddr[10],
			qDesc->valLength[10]);

		/*
		** Timeout
		*/

		(void) memcpy((char *) &(ptr->timeout), qDesc->valAddr[11],
			qDesc->valLength[11]);



		memset(ptr->plan_num, 0, sizeof(ptr->plan_num));
		ptr->count = 0;
		ptr->pass_id = -1;


	}
	
	(void) closeConnection(qDesc);
	return(IMS_OK);

}

/******************************************************************************
**
** dumpScheduleCache ()
**
** This function will dump the Schedule database cache information.
** 
**
******************************************************************************/

static int dumpScheduleCache (
	IMS_MSG_STRUCT *msgDesc,
	IMS_SCHEDULE_CACHE *cacheData)
{
	IMS_SCHEDULE_CACHE *ptr;

	while (cacheData != NULL)
	{
		ptr = cacheData;
		cacheData = cacheData->next;
		free(ptr);
	}
	return (IMS_ERROR);

}



/******************************************************************************
**
** ims_microSleep ()
**
** Put process to sleep.  Provides resolution in seconds and microseconds.
** 
**
******************************************************************************/
static int ims_microSleep(
	IMS_MSG_STRUCT *msgDesc,
	int sec,
	long msec)
{
	struct timeval 
	{
		long tv_sec;
		long tv_usec;
	} timeout;


	if ((sec < 0) || (msec < 0))
		return(IMS_OK);

	timeout.tv_sec = sec;
	timeout.tv_usec = msec;

	(void) select(0,  0,  0,  0, &timeout);

	return(IMS_OK);
}

/******************************************************************************
**
** ims_getWeekDay ()
**
** Returns a weekday 1 - 7 (sun - sat)
**
******************************************************************************/

static int ims_getWeekDay (
	IMS_MSG_STRUCT *msgDesc,
	IMS_NUMERIC_DATE *dateStruct,
	int *weekday)
{
	int days, msecs;
	
	/*
	** First convert to days 
	*/

	ims_numericDateToESAI(dateStruct, &days, &msecs);

	/*
	** Now, the number of days converted is from JAN 1, 1950 (a sunday).
	** We assume that no days of the week have ever been skipped :-) so 
	** we just take the number of days mod 7 and add that to sunday.
	*/

	*weekday = ((days - 1) % 7) + 1;

	return(IMS_OK);
}


/******************************************************************************
**
** processSchedule ()
**
** Process through list of schedule items and perform schedule processing
** 
******************************************************************************/

static int processSchedule (
	IMS_MSG_STRUCT *msgDesc,
	IMS_SCHEDULE_CACHE **masterCacheData)
{
	
	IMS_SCHEDULE_CACHE *ptr, *cacheData;
	char currentDate[IMS_DATETIME_LEN+1];
	char refDate[IMS_DATETIME_LEN+1];
	int days, msecs, secs;
	long next_wake_up = (IMS_WAKE_UP + 1) * 1000;
	int next_element_id = 0;
	int element_id = 0;
	IMS_NUMERIC_DATE dateStruct, dateStructTemp;
	char tempDate[IMS_DATETIME_LEN+1];
	int weekday;
	int flag;

	cacheData = *masterCacheData;

	ptr = cacheData;

	/*
	** Get current time.
	*/

	if (ims_getCurrentDate(msgDesc, &dateStruct) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get current date time");
		return(IMS_ERROR);
	}

	ims_numericDateToIMSA(&dateStruct, currentDate);

	/*
	** Scan through list of cached schedule.
	*/

	while (ptr != NULL)
	{

		element_id ++;

		/*
		** If the day count has been set for this report, then
		** just skip and continue.  This is used for time-based reports
		** only.
		*/

		if ((ptr->count > ptr->day_count) &&
			((ptr->event_mask  & IMS_FA_TIMEBASED) == IMS_FA_TIMEBASED))
		{
			ptr = ptr->next;
			continue;
		}

		/*
		** If a job is currently outstanding for the report, then
		** skip.  We don't want to try and have the scheduler kick off
		** reports that are already being generated.
		*/

		if (findReportJob(msgDesc, ptr->report_id, &flag) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not look through schedule jobs");
			return(IMS_ERROR);
		}

		if (flag == IMS_TRUE)
		{
			ptr = ptr->next;
			continue;
		}

		/*
		** Look at period information to perform correct calculation 
		** until wakeup time.
		*/


		if ((ptr->event_mask  & IMS_FA_TIMEBASED)== IMS_FA_TIMEBASED)
		{
			
			/*
			** Check everyday schedule
			*/
			
			if (ptr->period == IMS_FA_EVERYDAY) 
			{
				/*
				** We just need to compare times.
				*/

				memcpy((char *) &dateStructTemp, (char *) &dateStruct, sizeof(dateStruct));

				sscanf(ptr->time, 
					"%02d:%02d:%02d", &(dateStructTemp.hours), 
					&(dateStructTemp.minutes), &(dateStructTemp.seconds));
					
				dateStructTemp.msecs = 0;


				ims_numericDateToIMSA(&dateStructTemp, refDate);


				if (ims_numericDateDiff(msgDesc, currentDate, refDate, &days, 
								&msecs) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not determine date difference");
					return(IMS_ERROR);
				}

				if (msecs < next_wake_up)
				{
					next_wake_up = msecs;
					next_element_id = element_id;
				}

				/*
				** If the time has past, test the window and setup.
				*/

				if (strcmp(currentDate, refDate) > 0)
				{
					if (msecs > IMS_WAKE_UP * 10000)
					{
						(void) ims_msg(msgDesc, IMS_INFO,
						"Window missed to send report %d in scheduler at %s.",
							ptr->report_id, refDate);

						ptr->count ++;
					}
					else
					{
						next_wake_up *= -1;
						next_element_id = element_id;
					}
				}
				

			}

			/*
			** Check Every Week
			*/
	
			if (ims_getWeekDay (msgDesc, &dateStruct, &weekday) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not determine day of the week");
				return(IMS_ERROR);
			}


			if ((weekday == ptr->week_day) && (ptr->period == IMS_FA_EVERYWEEK))
			{
				/*
				** Our day of the week matches...
				*/

				memcpy((char *) &dateStructTemp, (char *) &dateStruct, sizeof(dateStruct));

				sscanf(ptr->time, 
					"%02d:%02d:%02d", &(dateStructTemp.hours), 
					&(dateStructTemp.minutes), &(dateStructTemp.seconds));
					
				dateStructTemp.msecs = 0;

				ims_numericDateToIMSA(&dateStructTemp, refDate);

				if (ims_numericDateDiff(msgDesc, currentDate, refDate, 
							&days, &msecs) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not determine date difference");
					return(IMS_ERROR);
				}


				if (msecs < next_wake_up)
				{
					next_wake_up = msecs;
					next_element_id = element_id;
				}

				/*
				** If the time has past, test the window and setup.
				*/

				if (strcmp(currentDate, refDate) > 0)
				{
					if (msecs > IMS_WAKE_UP * 10000)
					{

						(void) ims_msg(msgDesc, IMS_INFO,
							"Window missed to send weekly report %d in scheduler at %s. \n", 
							ptr->report_id, refDate);
						ptr->count ++;
					}
					else
					{
						next_wake_up *= -1;
						next_element_id = element_id;
					}
				}
				

			}

			/*
			** Every Month
			*/

			if ((ptr->month_day == dateStruct.day) && (ptr->period == IMS_FA_EVERYMONTH))
			{
				/*
				** Our day of the month matches...
				*/

				memcpy((void *) &dateStructTemp, (void *) &dateStruct, 
						sizeof(dateStruct));

				sscanf(ptr->time, 
					"%02d:%02d:%02d", &(dateStructTemp.hours), 
					&(dateStructTemp.minutes), &(dateStructTemp.seconds));
					
				ims_numericDateToIMSA(&dateStructTemp, refDate);

				dateStructTemp.msecs = 0;

				/*
				** Calculate the difference 
				*/

				if (ims_numericDateDiff(msgDesc, currentDate, refDate, &days, &msecs) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not determine date difference");
					return(IMS_ERROR);
				}

				if ((msecs < next_wake_up) && (days == 0))
				{
					next_wake_up = msecs;
					next_element_id = element_id;
				}

				/*
				** If the time has past, test the window and setup.
				*/

				if (strcmp(currentDate, refDate) > 0)
				{
					if (msecs > IMS_WAKE_UP * 10000)
					{

						/*
						** Time has past too far 
						*/

						ptr->count ++;
					}
					else
					{
						next_wake_up *= -1;
						next_element_id = element_id;
					}
				}
				

			} /* if PERIODIC */


		}

		/*
		** Process the event based reports
		*/

		if (ptr->event_mask & IMS_FA_MESSAGE)
		{
			/*
			** If this report is event driven, then see if there are 
			** messages available to kick off the report.
			*/

			if (processEventElement (msgDesc, ptr, cacheData,
						element_id, IMS_FA_MESSAGE) < IMS_OK)
			{
				return(IMS_ERROR);
			}
			
		}

		ptr = ptr->next;

	}

	/*
	** Check distance to reset time...
	*/

	memcpy((void *) &dateStructTemp,  (void *) &dateStruct,
		sizeof(dateStruct));

	sscanf(IMS_RESET_TIME, 
		"%02d:%02d:%02d", &(dateStructTemp.hours), 
		&(dateStructTemp.minutes), &(dateStructTemp.seconds));

	ims_numericDateToIMSA(&dateStructTemp, refDate);

	if (ims_numericDateDiff(msgDesc, currentDate, refDate, &days, 
				&msecs) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
		"Could not determine date difference");
		return(IMS_ERROR);
	}

	/*
	** Perform end of day processing...
	*/

	if  (abs(msecs) < IMS_WAKE_UP * 10000) 
	{
		if (performResetProcessing(msgDesc, masterCacheData) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"End of day processing could not be performed");
		}
		next_element_id = 0;
	}
	else if (msecs < next_wake_up)
	{
		/*
		** Reduce scheduler wake up time, so that it wakes up
		** at time IMS_RESET_TIME.
		*/

		next_wake_up = msecs;
		next_element_id = 0;
	}



	/*
	** Process any time based reports, and resechedule
	** Anything within the 4-minute window.
	*/

	if ((next_wake_up < 240) && (next_element_id > 0))
	{
		/*
		** Process the element
		*/

		if (processElement(msgDesc, next_element_id, cacheData, &dateStruct) < IMS_OK)
			return(IMS_ERROR);

		/*
		** Resubmit immediately to see if other pending tasks.
		*/

		/*

			(void) ims_microSleep(msgDesc, 0, 1);
		*/
	}
	else
	{


		/*
		** Go to sleep until next wake up time or IMS_WAKE_UP 
		** (which ever is sooner)
		*/

		if (next_wake_up / 1000 > IMS_WAKE_UP)
		{
			(void) ims_microSleep(msgDesc, IMS_WAKE_UP, 0); 
		}
		else
		{
			secs = next_wake_up / 1000;
			msecs = next_wake_up % 1000;
			(void) ims_microSleep(msgDesc, secs, msecs);
		}

	}

	return(IMS_OK);

}

/******************************************************************************
**
** processElement ()
**
** Found a element that is ready to be scheduled.
** 
******************************************************************************/
static int processElement(
	IMS_MSG_STRUCT *msgDesc,
	int elementId,
	IMS_SCHEDULE_CACHE *cacheData,
	IMS_NUMERIC_DATE *currentDate)
{
	int i;
	IMS_FA_INTERFACE *fa_data;
	char workbuf[64];
	int shmid;
	int job_id;
	int days;
	int msecs;


	for (i = 0; i < elementId - 1; i++)
		cacheData = cacheData->next;

	/*
	** Okay, now we have the element to process.
	*/


	shmid = ims_shm_getid();

	if (ims_shm_create(shmid, sizeof(IMS_FA_INTERFACE)) < 0)
	{
		/*
		** Can't allocate shared memory
		*/

		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not allocate memory to send report %d.", cacheData->report_id);

		return(IMS_ERROR);
	}

	fa_data = (IMS_FA_INTERFACE *) ims_shm_lock(shmid);

	if (fa_data == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not lock shared memory address to send FA Report %d", 
			cacheData->report_id);
		return(IMS_ERROR);
	}

	memset(fa_data, 0, sizeof(IMS_FA_INTERFACE));

	/*
	** Fill in period information: start and stop time into shared memory
	** structure so the FA Job can access it.
	*/


	if ((cacheData->event_mask & IMS_FA_TIMEBASED) && 
		(currentDate != NULL))
	{
		switch(cacheData->period)
		{
			case IMS_FA_EVERYDAY:
				/*
				** Convert to end time
				*/

				sscanf(cacheData->time, 
					"%02d:%02d:%02d", &(currentDate->hours), 
					&(currentDate->minutes), &(currentDate->seconds));

				/*
				** Need to back up in time here...
				*/


				(void) ims_numericDateToESAI(currentDate, &days, &msecs);
				days++;  /* Days does not include current day */

				msecs -= (cacheData->time_displacement * 1000); 

				if (msecs < 0)
				{
					days --;
					msecs = 3600 * 1000 + msecs;
				}

				ims_dateItoDef(days, msecs, fa_data->date_end);

				/*
				** Calculate start time
				*/

				days --;
				ims_dateItoDef(days, msecs, fa_data->date_start);

				break;
			
				/*
				** Convert to end time
				*/

			case IMS_FA_EVERYWEEK:
				sscanf(cacheData->time, 
					"%02d:%02d:%02d", &(currentDate->hours), 
					&(currentDate->minutes), &(currentDate->seconds));

				/*
				** Need to back up in time here...
				*/

				ims_numericDateToESAI(currentDate, &days, &msecs);

				days++;  /* Days does not include current day */


				msecs -= (cacheData->time_displacement * 1000); 

				if (msecs < 0)
				{
					days --;
					msecs = 3600 * 1000 + msecs;
				}

				ims_dateItoDef(days, msecs, fa_data->date_end);

				/*
				** Calculate start time which is one week prior.
				*/

				days -= 7;

				ims_dateItoDef(days, msecs, fa_data->date_start);


				break;

			case IMS_FA_EVERYMONTH:

				sscanf(cacheData->time, 
					"%02d:%02d:%02d", &(currentDate->hours), 
					&(currentDate->minutes), &(currentDate->seconds));

				/*
				** Need to back up in time here...
				*/
				ims_numericDateToESAI(currentDate, &days, &msecs);
				days++;  /* Days does not include current day */


				msecs -= (cacheData->time_displacement * 1000); 

				if (msecs < 0)
				{
					days --;
					msecs = 3600 * 1000 + msecs;
				}

				ims_dateItoDef(days, msecs, fa_data->date_end);


				/*
				** Calculate start time which is one month prior.
				** Algorithm is to modify the current date so that 
				** it reflects one month prior, build a string, and
				** have the date library re-parse it to fill in the
				** appropriate values.
				*/

				currentDate->month --;

				if (currentDate->month < 1)
				{
					currentDate->month = 12;
					currentDate->year --;
				}

				sprintf(workbuf,
					"%04d-%02d-%02dT%02d:%02d:%02d.%03d",
					currentDate->year, currentDate->month,
					currentDate->day, currentDate->hours,
					currentDate->minutes, currentDate->seconds,
					currentDate->msecs);
			
				if (ims_timeToNumericDate(msgDesc, workbuf, 
						currentDate) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
						"Could not get start date for report %d, date %s",
						cacheData->report_id, workbuf);

					(void) ims_shm_unlock(shmid, (void *) fa_data);

					(void) ims_shm_remove(shmid);
					return(IMS_ERROR);
				}

				ims_numericDateToIMSA(currentDate, fa_data->date_start);


				break;
				
		}
	}

	fa_data->pass = cacheData->pass_id; 
	strcpy(fa_data->plan_num, cacheData->plan_num);
	fa_data->tape_count = 0;
	strcpy(fa_data->platform, cacheData->platform);
	fa_data->seq_num = cacheData->seq_num; 
	strcpy(fa_data->station_id, cacheData->station_id); /* R2.1 */

	memcpy((char *) &(fa_data->jobSpec), (char *) &userSpec, 
		sizeof(IMS_JOB_USER_SPEC));

	(void) ims_shm_unlock(shmid, (void *) fa_data);

	job_id = ims_submitReport(msgDesc, &userSpec,
		cacheData->report_id, IMS_FA_TIMEOUT,  end_report_cb, shmid, NULL);

	if (job_id < 0)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"ims_fa_scheduler could not start FA Report %d", 
			cacheData->report_id);
		(void) ims_shm_remove(shmid);
		return(IMS_ERROR);
	}

	/*
	** Indicate that the report has been submitted
	*/

	if (cacheData->day_count > 0)
		cacheData->day_count --;

	(void)  addToJobList(msgDesc, cacheData->report_id, job_id);

	cacheData->count ++ ;
	return(IMS_OK);

}


/*******************************************************************
**
** end_report_cb
**
*******************************************************************/
static void end_report_cb(
	int job_id,
	int report,
	int status,
	int shmid,
	char *data)
{
	IMS_SCHEDULER_JOBS *ptr;
	char msg[255]; /* R2.1 */

	ptr = glbl_jobs;
	
	(void) ims_shm_remove(shmid);

  /* The following added for R2.1 */

	if (status == IMS_JOB_COMPLETE)
	{
			sprintf(msg, "Job %d successfully completed. ",
			job_id);
	}
	else if (status == IMS_JOB_ABORTED)
	{
			sprintf(msg, "Job %d Aborted.",
			job_id);

	}
	else if (status == IMS_JOB_TIMEOUT)
	{
			sprintf(msg, "Job %d for Report %d Timed Out.",
							job_id, report);
	}
	else
	{
			sprintf(msg, "Job %d for Report %d Terminated With Unknown Status.",
							job_id, report);
	}
	if (status != IMS_JOB_COMPLETE)
	{
		(void) ims_msg(glbl_msgDesc, IMS_ERROR, msg);
	}
	else
	{
		(void) ims_msg(glbl_msgDesc, IMS_INFO, msg);
	}

	/* end of added R2.1 */

	/*
	** Free the job.
	*/

	while (ptr != NULL) 
	{
		if (ptr->job_id == job_id)
		{
			ptr->free = 1;
		}
		ptr = ptr->next;
	}


	/*
	** Probably want to do some sort of processing of the status
	*/
}


/*******************************************************************
**
** processEventElement
**
** This will attempt to process an element in the schedule cache
** which has events associated with it.  An element refers to 
** a report scenario.
**
*******************************************************************/
static int processEventElement (
	IMS_MSG_STRUCT *msgDesc,
	IMS_SCHEDULE_CACHE *cacheData,
	IMS_SCHEDULE_CACHE *masterCacheData,
	int elementId,
	int eventCode)
{
	IMS_FA_TRACKING *trackData, *ptr;
	int ready_flag;
	int orbit_id = -1;
	int found;	
	int msecs, down_msecs;
	int days, down_days;
	IMS_NUMERIC_DATE dateStruct;
	char station_id[IMS_COL15_LEN+1];
	
	strcpy(station_id,"NO");
	if (cacheTracking(msgDesc, &trackData) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not cache tracking data to process element events.");
		return(IMS_ERROR);
	}
	
	switch ( eventCode )
	{
		case IMS_FA_END_OF_DAY:
			/*
			** End of day processing is performed for
			** those elments which require there cyclic counters
			** to be reset.
			*/


			if (cacheData->event_mask & IMS_RESET_COUNTER)
			{
				if (resetCounter(msgDesc, cacheData) < IMS_OK)
				{
					(void) dumpTrackingCache(msgDesc, trackData);
					return(IMS_ERROR);
				}
			}
			return(IMS_OK);

			break;

		case IMS_FA_MESSAGE:
			/*
			** An FA Message may have constraints:
			**   1) Reports may have more than one message required
			**	    to generate the report.
			**   2) Reports may require a pass change.
			*/

			/*
			** Check pass change 
			*/

			if (cacheData->event_mask & IMS_FA_PASS_CHANGE)
			{
				/*
				** Need to see if there are multiple passes in table
				** for a downlink message.
				*/ 

				ptr = trackData;
				found = IMS_FALSE;
				while (ptr != NULL) 
				{
					if ((strcmp(ptr->platform, cacheData->platform) == 0) &&
						(ptr->message_id == IMS_FA_DOWNLINK))
					{
						if ((orbit_id != ptr->orbit_id) && (orbit_id != -1))
						{
							/*
							** There are multiple orbits, so 
							** generate a report.
							*/
							found = IMS_TRUE;	
								
							/*
							** Grab the smallest.
							*/

							if (orbit_id > ptr->orbit_id)
								orbit_id = ptr->orbit_id;
						
						}
						else if (orbit_id == -1)
						{
							orbit_id = ptr->orbit_id;
						}

					}
					ptr = ptr->next;
				}

				/*
				** If no new acquisitions are found, then don't send it.
				*/

				if (found == IMS_FALSE)
				{
					break;

				}

			}

			/*
			** Check timeout
			*/
			if (cacheData->event_mask & IMS_FA_TIMEOUT_DOWNLINK)
			{

				/*
				** Get current time.
				*/

				if (ims_getCurrentDate(msgDesc, &dateStruct) < IMS_OK)
				{
					(void) ims_msg(msgDesc, IMS_ERROR,
							"Could not get current date time");
					return(IMS_ERROR);
				}

				(void) ims_numericDateToESAI(&dateStruct, &days, &msecs);

				msecs = msecs - cacheData->timeout * 1000;

				if (msecs < 0)
				{
					days --;
					msecs = 3600 * 1000 + msecs;
				}



				ptr = trackData;
				found = IMS_FALSE;
				while (ptr != NULL) 
				{
					if ((strcmp(ptr->platform, cacheData->platform) == 0) &&
						(ptr->message_id == IMS_FA_DOWNLINK))
					{

						if (ims_timeToNumericDate(msgDesc, 
								ptr->received_date, &dateStruct) < IMS_OK)
						{
							(void) ims_msg(msgDesc, IMS_ERROR,
									"Could not get current date time");
							return(IMS_ERROR);
						}

						(void) ims_numericDateToESAI(&dateStruct, 
								&down_days, &down_msecs);
						

						/*
						** Check if acquisitions timed out.
						*/

						if ((down_days < days) ||
							((down_days == days) &&
							 (down_msecs < msecs)))
						{ 
							found = IMS_TRUE;					
							orbit_id = ptr->orbit_id;
							strcpy(station_id, ptr->station_id);
						}



					}
					ptr = ptr->next;
				}

				/*
				** If no new acquisitions are found, so don't send it.
				*/

				if (found == IMS_FALSE)
				{
					break;
				}

			}



			/*
			** Check if this report has all messages registered
			** necessary for generation.
			*/

			if (checkMessages(msgDesc, cacheData, trackData, &orbit_id,
					&ready_flag, station_id) < IMS_OK)
			{
				(void) ims_msg (msgDesc, IMS_ERROR, 
					"FA Scheduler failed when checking messages for tracking");
				(void) dumpTrackingCache(msgDesc, trackData);
				return(IMS_ERROR);
			}

			if (ready_flag == IMS_TRUE)
			{
				/*
				** Send the report, we are ready.
				*/

				cacheData->pass_id = orbit_id;
				strcpy(cacheData->station_id, station_id);

				fprintf(stderr, "FA Report %d ready.\n", cacheData->report_id);

				if (processElement(msgDesc, elementId, 
							masterCacheData, NULL) < IMS_OK)
				{
					(void) dumpTrackingCache(msgDesc, trackData);
					return(IMS_ERROR);
				}

			}
			break;
	}

	(void) dumpTrackingCache(msgDesc, trackData);

	return(IMS_OK);
}

/*******************************************************************
**
** checkMessages
**
** This routine will check to see if all messages have come in for
** a given report.  It returns true or false depending on the case.
**
*******************************************************************/
static int checkMessages (
	IMS_MSG_STRUCT *msgDesc,
	IMS_SCHEDULE_CACHE *cacheData,
	IMS_FA_TRACKING *trackData,
	int *orbit_id,
	int *flag,
	char *station_id)
{
	IMS_FA_TRACKING *ptr, *ptr2;
	int msg_mask = cacheData->message_mask;
	int msg_id, msg_id_tmp;
	int count = 0;

	
	ptr = trackData;
	*flag = IMS_FALSE;


	/*
	** Scan through list of tracked messages.  
	*/

	while (ptr != NULL)
	{
		if (((ptr->message_id & msg_mask) > 0) &&
			 (strcmp(ptr->platform, cacheData->platform) == 0)
			 )
		{
			/*
			** Found a message, now scan for comparible messages.
			*/

			strcpy(cacheData->plan_num, ptr->plan_num);
			cacheData->seq_num = ptr->segment_id;
			strcpy(cacheData->station_id, ptr->station_id); /* R2.1 */
			ptr2 = ptr->next;
			msg_id = (ptr->message_id & msg_mask);
			count = 1;

			while (ptr2 != NULL)
			{
				msg_id_tmp = (ptr2->message_id & msg_mask);

				if (msg_id_tmp != msg_id)
				{
					/*
					** Check if these messages are equivelent.
					** Check if comparable messages are in the tracking table. Current systme
					** only have message cnt=1, downlink message, this code may be extra.
					*/

					if ((ptr->orbit_id == ptr2->orbit_id) && 
						(strcmp(ptr->station_id, ptr2->station_id) == 0) && /* R2.1 */
						(strcmp(ptr->platform, ptr2->platform) == 0) &&
						((*orbit_id == -1) || (*orbit_id == ptr2->orbit_id)) &&
						((strcmp(station_id, "NO") == 0) || (strcmp(station_id, ptr2->station_id) == 0)))
					{
						count++;
					}

				}
				ptr2 = ptr2->next;
			}

			/*
			** If we have all the required messages available, then
			** we found a report that can be processed.
			*/

			if (count >= cacheData->message_count)
			{
				*flag = IMS_TRUE;

				/*
				** This is for RADARSAT-1
				*/

				if (*orbit_id == -1)
				{
					*orbit_id = ptr->orbit_id;
				}
				if (strcmp(station_id,"NO") == 0)
					strcpy(station_id, ptr->station_id);

				return(IMS_OK);
			}

		}

		ptr = ptr->next;
	}

	return(IMS_OK);


}

/*******************************************************************
**
** cacheTracking
**
** Cache the tracking table to perform event processing...
**
*******************************************************************/
static int cacheTracking(
	IMS_MSG_STRUCT *msgDesc,
	IMS_FA_TRACKING **cacheData
	)
{
	IMS_QI_DESC_OBJ *qDesc;
	int status;
	int rowCount = 0;
	IMS_FA_TRACKING *ptr;
	short int segment_id_temp; /*R2.1*/

	/*
	** Connect to the database.
	*/

	if (openConnection(msgDesc, &qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not logon to database to cache tracking information.");
		return(IMS_ERROR);
	}
 /* R2.1 Change */
	sprintf(qDesc->cmd, "select orbit, message_id, platform, \
		start_time, end_time, sequence, report_id, name, plan_num, station_id, \
		received_date from fa_tracking");

	*cacheData = NULL;

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		
		if (status < IMS_OK)
		{
			closeConnection(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR, 
					"Can't cache data. Query failed.");
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


		if (rowCount == 1)
		{
			ptr = (void *) malloc(sizeof(IMS_FA_TRACKING));
			if (ptr == NULL)
			{
				(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not allocate memory to cache data");
				closeConnection(qDesc);
				return(IMS_FATAL);
			}
			*cacheData  = ptr;
		}
		else
		{
			ptr->next = (void *) malloc(sizeof(IMS_FA_TRACKING));

			if (ptr->next == NULL)
			{
				(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not allocate memory to cache data");
				closeConnection(qDesc);
				return(IMS_FATAL);
			}
			ptr = ptr->next;
		}

		ptr->next = NULL;
		
		/*
		** Get orbit_id
		*/

		(void) memcpy((char *) &(ptr->orbit_id), qDesc->valAddr[0],
			qDesc->valLength[0]);

		/*
		** Get message_id
		*/

		(void) memcpy((char *) &(ptr->message_id), qDesc->valAddr[1],
			qDesc->valLength[1]);

		
		/*
		** Get platform
		*/

		memset(ptr->platform, 0, sizeof(ptr->platform));

		(void) memcpy((char *) ptr->platform, qDesc->valAddr[2],
			qDesc->valLength[2]);
		ims_trim(ptr->platform);

		/*
		** Get start_time
		*/

		memset(ptr->start_time, 0, sizeof(ptr->start_time));

		(void) memcpy((char *) ptr->start_time, qDesc->valAddr[3],
			qDesc->valLength[3]);
		ims_trim(ptr->start_time);
		

		/*
		** Get end_time
		*/
		
		memset(ptr->end_time, 0, sizeof(ptr->end_time));

		(void) memcpy((char *) ptr->end_time, qDesc->valAddr[4],
			qDesc->valLength[4]);
		ims_trim(ptr->end_time);

    /*
		** Get segment_id
	  */

		(void) memcpy((char *) &(segment_id_temp), qDesc->valAddr[5],
			 qDesc->valLength[5]);
    ptr->segment_id = (int) segment_id_temp;

		/*
		** Get report_id
		*/
		
		(void) memcpy((char *) &(ptr->report_id), qDesc->valAddr[6],
			qDesc->valLength[6]);

		/*
		** Get name
		*/

		memset(ptr->name, 0, sizeof(ptr->name));

		(void) memcpy((char *) ptr->name, qDesc->valAddr[7],
			qDesc->valLength[7]);
		ims_trim(ptr->name);

		/*
		** Get plan_num
		*/

		memset(ptr->plan_num,  0, sizeof(ptr->plan_num));

		(void) memcpy((char *) ptr->plan_num, qDesc->valAddr[8],
			qDesc->valLength[8]);
		ims_trim(ptr->plan_num);

		/*
		** Get station_id
		*/
		
		memset(ptr->station_id,  0, sizeof(ptr->station_id));
		(void) memcpy((char *) &(ptr->station_id), qDesc->valAddr[9],
			qDesc->valLength[9]); /* R2.1 */
		ims_trim(ptr->station_id);

		/*
		** Received date
		*/

		memset(ptr->received_date, 0, sizeof(ptr->received_date));

		(void) memcpy((char *) ptr->received_date, qDesc->valAddr[10],
			qDesc->valLength[10]);
		ims_trim(ptr->received_date);


	}

	(void) closeConnection(qDesc);
	return(IMS_OK);
}



/*******************************************************************
**
** dumpTrackingCache
**
** Cache the tracking table to perform event processing...
**
*******************************************************************/
static int dumpTrackingCache(
	IMS_MSG_STRUCT *msgDesc,
	IMS_FA_TRACKING *cacheData
	)
{
	IMS_FA_TRACKING *ptr;

	while (cacheData != NULL)
	{
		ptr = cacheData;
		cacheData = cacheData->next;
		free(ptr);
	}

	return(IMS_OK);

}

/*******************************************************************
**
** addToJobList
**
**
*******************************************************************/
static int addToJobList(
	IMS_MSG_STRUCT *msgDesc,
	int report_id,
	int job_id)
{
	IMS_SCHEDULER_JOBS *ptr;


	ptr = glbl_jobs;

	glbl_jobs = malloc(sizeof(IMS_SCHEDULER_JOBS));

	if (glbl_jobs == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not add job to internal scheduler list");
		return(IMS_ERROR);
	}

	glbl_jobs->next = ptr;
	glbl_jobs->report_id = report_id;
	glbl_jobs->job_id = job_id;
	glbl_jobs->free = 0;

	return(IMS_OK);
}

/*******************************************************************
**
** removeFromJobList
**
**
*******************************************************************/
static int RemoveFromJobList(
	IMS_MSG_STRUCT *msgDesc,
	int job_id)
{
	IMS_SCHEDULER_JOBS *ptr, *ptr2;



	ptr = glbl_jobs;

	while (ptr != NULL)
	{
		
		if ((ptr->next != NULL) &&
			(ptr->next->job_id == job_id)) 
		{
			ptr2 = ptr;
			ptr = ptr->next->next;		
			ptr2->next = NULL;
			free(ptr);
			goto exit;
		}
		else if ((ptr->job_id == job_id) && (ptr == glbl_jobs))
		{
			glbl_jobs = ptr->next;
			free(ptr);
			goto exit;
		}

		ptr = ptr->next;
	}

	(void) ims_msg(msgDesc, IMS_ERROR, 
		"Could not find job in the scheduler job list for report");

	return(IMS_ERROR);


exit:


	return(IMS_OK);
}

/*******************************************************************
**
** findReportJob
**
** Look to see if a report is currently being generated.  We want
** to only generate one type of report at a time for the scheduler.
*******************************************************************/
static int findReportJob(
	IMS_MSG_STRUCT *msgDesc,
	int report_id,
	int *flag)
{

	IMS_SCHEDULER_JOBS *ptr;
	int stale;
	int job_id;

	*flag = IMS_FALSE;
	stale = IMS_FALSE;

	ptr = glbl_jobs;

	while (ptr != NULL)
	{
		if (ptr->report_id == report_id) 
		{
			if (ptr->free == IMS_FALSE)
				*flag = IMS_TRUE;
			else 
			{
				stale = IMS_TRUE;
				job_id = ptr->job_id;
			}
		}
		
		ptr = ptr->next;
	}

	if (stale == IMS_TRUE)
	{
		(void) RemoveFromJobList(msgDesc, job_id);
	}

	  
	return(IMS_OK);
}

/*******************************************************************
**
** performResetProcessing
**
** This function will do all required processing for the end of a 
** day (probably specified around midnight).  Processing for each
** element flagged for IMS_END_OF_DAY will be performed,  as well
** as processing for the scheduler in general such as dumping
** and reloading the schedule cache information.
*******************************************************************/
static int performResetProcessing(
	IMS_MSG_STRUCT *msgDesc,
	IMS_SCHEDULE_CACHE **MasterScheduleCache)
{

	IMS_SCHEDULE_CACHE *cacheData;
	IMS_FA_TRACKING track;
	IMS_QI_DESC_OBJ *qDesc;
	int element;

	cacheData = *MasterScheduleCache;
	glbl_shutdown = TRUE;

	while (cacheData != NULL)
	{
		element ++;
		if (cacheData->event_mask  & IMS_FA_END_OF_DAY)
		{
			if (processEventElement(msgDesc, cacheData, 
						*MasterScheduleCache, element,
						IMS_FA_END_OF_DAY) < IMS_OK)
			{
				(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not perform end of day reset for report %d",
					cacheData->report_id);
			}
		}
		
		cacheData = cacheData->next;
	}

	/*
	** Remove all FA Reports sent for the day from the tracking table
	** 
	** We currently do not put the FA reports into the tracking table.
	** This might want to be revisited later for more automated processing
	** of FA Reports...
	*/

#if 0

	if (openConnection (msgDesc, &qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not open connection to SQL server for end of day.");
		return(IMS_ERROR);
	}

	if (ims_deleteFromTrackingTable(msgDesc, qDesc, &track) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not delete FA Report data from tracking table.");
		(void) closeConnection (qDesc);
		return(IMS_ERROR);
	}
	
	(void) closeConnection (qDesc);

	/*
	** Dump Cache
	*/

	(void) dumpScheduleCache(msgDesc, *MasterScheduleCache);

	if (loadCacheData(msgDesc, MasterScheduleCache) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not re-cache database to load schedule information.");

		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}
#endif

	(void) ims_msg(msgDesc, IMS_INFO, 
		"End of Day Processing Complete for FA Scheduler.");

	return(IMS_OK);

}


/*******************************************************************
**
** resetCounter
**
** This function will reset the counter for a specific report.
**
*******************************************************************/
static int resetCounter(
	IMS_MSG_STRUCT *msgDesc, 
	IMS_SCHEDULE_CACHE *cacheData)
{
	IMS_QI_DESC_OBJ *qDesc;
	int status;
	int rowCount = 0;
	IMS_SCHEDULE_CACHE *ptr;

	/*
	** Connect to the database.
	*/

	if (openConnection(msgDesc, &qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not logon to database to cache schedule information.");
		return(IMS_ERROR);
	}

	sprintf(qDesc->cmd, "update fa_history set last_report_counter = 0 \
		where report_id = %d", cacheData->report_id);

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		
		if (status < IMS_OK)
		{
			closeConnection(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR, 
					"Failed to update counter table for report %d",
					cacheData->report_id);
			return(IMS_ERROR);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}
	}

	(void) closeConnection(qDesc);
	return(IMS_OK);
}

