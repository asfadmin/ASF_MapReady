static char *sccs = "@(#)ims_svAux.c	5.7  05/09/97";

/******************************************************************************
**
** File:    ims_svAux.c
**
** Function:    Perform all auxillary processing using a pipe with the
**              FTS network server for processing state-vector files .
**              ****  modified to use bcp by David Pass  7/15/96
**                  changed input to accept command line input if args
**                  Also changed to bcp, bulk copy program, from Sybase
**                  for speed.
**
** Author: Dan Crichton
**
** Date:    11/15/95
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
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_query.h>
#include <ims_cmd.h>
#include <ims_util.h>
#include <ims_getInput.h>
#include <ims_version.h>
#include <ims_signal.h>
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <syslog.h>

#include <ims_job_control.h>



typedef struct USERSPEC
{
	char *username;
	char *password;
	char *server;
	char *program;
	char *database;
} USERSPEC;


/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *username;
	char *password;
	char *fileName;
	char *repositoryDir;
	char *granuleName;
	char *granuleIdx;
	char *platform;
	char *datasetIdx;
	char *commandFile;
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
	{"-U",              &commands.username},
	{"+username",       &commands.username},
	{"-P",              &commands.password},
	{"+password",       &commands.password},
	{"-F",              &commands.fileName},
	{"+fileName",       &commands.fileName},
	{"-R",              &commands.repositoryDir},
	{"+repositoryDir",  &commands.repositoryDir},
	{"-G",              &commands.granuleName},
	{"+granuleName",    &commands.granuleName},
	{"-L",              &commands.platform},
	{"+platform",       &commands.platform},
	{"-D",              &commands.datasetIdx},
	{"+datasetIdx",     &commands.datasetIdx},
	{"-C",              &commands.commandFile},
	{"+commandFile",    &commands.commandFile},
	{"-X",              &commands.server},
	{"+server",         &commands.server},
	{"-Y",              &commands.database},
	{"+database",       &commands.database},
	{"-h",              &commands.help},
	{"+help",           &commands.help},
	{"-r",              &commands.release},
	{"+release",        &commands.release}
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

static IMS_CMD_CLASS cmdFileElm [] =
{
	{"username",        &commands.username},
	{"password",        &commands.password},
	{"platform",        &commands.platform},
	{"fileName",        &commands.fileName},
	{"repositoryDir",   &commands.repositoryDir},
	{"granuleName",     &commands.granuleName},
	{"granuleIdx",      &commands.granuleIdx},
	{"datasetIdx",      &commands.datasetIdx},
	{"server",          &commands.server},
	{"database",        &commands.database}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);


/*
** Local Functions
*/
static int execCmd (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
static int getGranuleInfo(IMS_MSG_STRUCT *, USERSPEC *, char *, int, int, char);
static int parseSVFile (IMS_MSG_STRUCT *, USERSPEC *, char *, char *, char *, int, char *);
static int getArgInput (  IMS_MSG_STRUCT * );
static void usage (void);

/*
** External Functions
*/
extern int ims_forwardMsgQueue (IMS_MSG_STRUCT *);

/*
** Global Variables
*/
static char *glb_programName;


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
	char sv_flag;


	commands.username = NULL;
	commands.password = NULL;
	commands.platform = NULL;
	commands.fileName = NULL;
	commands.repositoryDir = NULL;
	commands.granuleName = NULL;
	commands.granuleIdx = NULL;
	commands.datasetIdx = NULL;
	commands.commandFile = NULL;
	commands.server = NULL;
	commands.database = NULL;
	commands.help = NULL;
	commands.release = NULL;

	/*
	** Initialize the message processor.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr,
			"Memory allocation for IMS_MSG_STRUCT structure failed.");
			exit (0);
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
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgQueueFlag(msgDesc, IMS_ON);
	(void) ims_msgStderrFlag(msgDesc, IMS_OFF);


	/*
	** Initialize command and reply structures
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
	(void) strcpy (cmds[8], "CMD:0:platform: ");
	(void) strcpy (cmds[9], "CMD:1:datasetIdx: ");

#ifdef DEBUG
	fprintf(stderr, "Auxilary Process %s preparing requests to FTS Server\n",
			argv[0]);
#endif

	/* *****************************
	******  D. Pass: added another method of getting the data into the
	**      program.  if the args are > 1, then the command line method is
	**      to be used.  otherwise, the data is obtained from the called
	**      procedure.
	*/
	if(  argc  >  1  ){
		/*
		** Get the command line arguments. The variable status will actually
		** contain the number of command line arguments processed upon
		** successful completion.
		*/
		if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
			cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			goto ERROR;
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
			exit( 0 );
		}

		/*
		** If there is a command file present, then get any commands from
		** this file, then overlay all commands from the commandline, except
		** password, which will be gone by this point.
		*/
		if (commands.commandFile != (char *) NULL){
			if ((status = ims_getFileParms (commands.commandFile,
				cmdFileElm, cmdFileElmCount, msgDesc)) < IMS_OK){
				goto ERROR;
			}

			/*
			** Now, get command line arguments again to overlay file args.
			*/
			if ((status = ims_getCmdLine (argc, argv,
				cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
					{
				goto ERROR;
			}
		}

		/*
		** Process the information from command-line and/or command-file.
		*/
		if( (status = getArgInput (msgDesc ) )  < IMS_OK)
		{
			goto ERROR;
		}

		/*
		**  put the results into the replies area
		*/
		strcpy( replies[0], commands.username  );
		strcpy( replies[1], commands.password  );
		strcpy( replies[2], commands.fileName  );
		strcpy( replies[3], commands.repositoryDir  );
		strcpy( replies[4], commands.server  );
		strcpy( replies[5], commands.database  );
		strcpy( replies[6], commands.granuleName  );
		strcpy( replies[7], commands.granuleIdx  );
		strcpy( replies[8], commands.platform  );
		strcpy( replies[9], commands.datasetIdx  );
	}
	else{
		/*
		**  **** obtain through read,write (from called program).
		**
		** request for required information
		*/
		cmdLen = 10;
		for ( i=0; i < cmdLen; i++ )
		{
#ifdef DEBUG
			fprintf(stderr, "Sending CMD[%d] to FTS SERVER:%s\n",i, cmds[i]);
#endif
			(void) write (1, cmds[i], strlen (cmds[i]) + 1);
			if (read( 0, replies[i], IMS_COL255_LEN+1) == 0)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Premature ftr termination, abort auxProcess");
				exit (0);
			}
#ifdef DEBUG
			fprintf(stderr, "Receiving reply from FTS SERVER:%s\n", replies[i]);
#endif

			/*
			** Get rid of new line characters
			*/
			replies[i][strlen(replies[i])-1] = '\0';
		}
	}

	(void) sprintf(fullPathName, "%s/%s", replies[3], replies[2]);


	userSpec.username = replies[0];
	userSpec.password = replies[1];
	userSpec.server   = replies[4];
	userSpec.database = replies[5];
	userSpec.program = glb_programName;

	status = IMS_OK;


	if (parseSVFile (msgDesc, &userSpec, fullPathName, replies[8], replies[6],
		atoi(replies[7]), &sv_flag) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Parsing State Vector file failed.");
		status = IMS_ERROR;
	}
	else
	{
		if (getGranuleInfo(msgDesc, &userSpec, replies[6], atoi(replies[7]),
			atoi(replies[9]), sv_flag) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Inserting into sv_available table failed.");
			status = IMS_ERROR;
		}
	}


	(void) ims_forwardMsgQueue (msgDesc);


	/* free */

	(void) ims_msgStructFree(msgDesc);


	for (i = 0; i < 20; i++)
	{
		free(cmds[i]);
		free(replies[i]);
	}

	exit(0);
ERROR:
	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_ERROR,
		"The ims_svAux program failed.");
	(void) ims_forwardMsgQueue (msgDesc);
	(void) ims_msgStructFree (msgDesc);
	for (i = 0; i < 20; i++)
	{
		free(cmds[i]);
		free(replies[i]);
	}

	exit (1);
}


/* *************************************************************
**
**  subr getArgInput () - process command-line and command-file
**      arguments.
**
**************************************************************** */

static int getArgInput (
	IMS_MSG_STRUCT *msgDesc )
{
	char inputBuffer[IMS_INPUT_BUF_LEN+1];
	long  i,j;


	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/

	/* username */
	if (commands.username == (char *) NULL)
		{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Username: ") == (char *) NULL)
				{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		i = strlen( inputBuffer )+1;
		commands.username = (char *) malloc( i * sizeof( char ));
		(void) strcpy ( commands.username, inputBuffer);
	}

	/* password */
	if (commands.password == (char *) NULL)
		{
		if (ims_getPassword (inputBuffer) == NULL)
				{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		i = strlen( inputBuffer )+1;
		commands.password = (char *) malloc( i * sizeof( char ));
		(void) strcpy (commands.password, inputBuffer);
	}

	/* fileName  */
	if (commands.fileName  ==  (char *) NULL)
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"fileName: ") == (char *) NULL)
				{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}
		i = strlen( inputBuffer )+1;
		commands.fileName = (char *) malloc( i * sizeof( char ));
		(void) strcpy (commands.fileName, inputBuffer);
	}


	/* repositoryDir */
	if (commands.repositoryDir  ==  (char *) NULL)
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"repositoryDir: ") == (char *) NULL)
				{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}
		i = strlen( inputBuffer )+1;
		commands.repositoryDir = (char *) malloc( i * sizeof( char ));
		(void) strcpy (commands.repositoryDir, inputBuffer);
	}


	/* granuleName */
	if (commands.granuleName  ==  (char *) NULL)
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"granuleName: ") == (char *) NULL)
				{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}
		i = strlen( inputBuffer )+1;
		commands.granuleName = (char *) malloc( i * sizeof( char ));
		(void) strcpy (commands.granuleName, inputBuffer);
	}

	try_again:;
	/* granuleIdx */
	if (commands.granuleIdx  ==  (char *) NULL)
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"granuleIdx: ") == (char *) NULL)
				{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}
		i = strlen( inputBuffer )+1;
		for( j=0; j  <  i ; j++ ){
			if(  !isdigit( inputBuffer[j] )  ){
				(void) ims_msg (msgDesc, IMS_INFO,
					"granuleIdx must be a number: try again." );
				goto try_again;
			}
		}
		commands.granuleIdx = (char *) malloc( i * sizeof( char ));
		(void) strcpy (commands.granuleIdx, inputBuffer);
	}


	/* platform */
	if (commands.platform  ==  (char *) NULL)
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Platform: ") == (char *) NULL)
				{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}
		i = strlen( inputBuffer )+1;
		commands.platform = (char *) malloc( i * sizeof( char ));
		(void) strcpy (commands.platform, inputBuffer);
	}

	try_again2:;
	/* datasetIdx */
	if (commands.datasetIdx  ==  (char *) NULL)
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"datasetIdx: ") == (char *) NULL)
				{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}
		i = strlen( inputBuffer )+1;
		for( j=0; j  <  i ; j++ ){
			if(  !isdigit( inputBuffer[j] )  ){
				(void) ims_msg (msgDesc, IMS_INFO,
					"datasetIdx must be a number: try again." );
				goto try_again2;
			}
		}
		commands.datasetIdx = (char *) malloc( i * sizeof( char ));
		(void) strcpy (commands.datasetIdx, inputBuffer);
	}


	/* server */
	if (commands.server  ==  (char *) NULL)
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Server: ") == (char *) NULL)
				{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}
		i = strlen( inputBuffer )+1;
		commands.server = (char *) malloc( i * sizeof( char ));
		(void) strcpy (commands.server, inputBuffer);
	}

	/* database */
	if (commands.database  ==  (char *) NULL)
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"database: ") == (char *) NULL)
				{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}
		i = strlen( inputBuffer )+1;
		commands.database = (char *) malloc( i * sizeof( char ));
		(void) strcpy (commands.database, inputBuffer);
	}

	return (IMS_OK);
}   /*  getArgInput */


/* *************************************************************
**
**  subr usage () - Print command line argument switches. called
**      for -help option.
**
**************************************************************** */

static void usage (void)
{
	int i;

	(void) fprintf (stderr,
		"\n%s command-line arguments:\n\n", glb_programName);

	for (i = 0; i < cmdLineElmCount; i++)
	{
		(void) fprintf (stderr, "%s\n", cmdLineElm[i].paramKeyword);
	}

	(void) fprintf (stderr, "\n\n");
}   /*  usage  */


/******************************************************************************
**
** parseSVFile
**
******************************************************************************/

static int parseSVFile (
	IMS_MSG_STRUCT *msgDesc,
	USERSPEC *userSpec,
	char *filename,
	char *platform,
	char *granuleName,
	int granuleIdx,
	char *sv_type)
{
	FILE *filePtr;
	char revolution[6];  /* Spacecraft state vector info */
	char year[5];
	char time[17];
	char x_pos[12];
	char y_pos[12];
	char z_pos[12];
	char x_vel[12];
	char y_vel[12];
	char z_vel[12];
	char dateStr[IMS_DATETIME_LEN+1];
	char start_time[IMS_DATETIME_LEN+1];
	char end_time[IMS_DATETIME_LEN+1];
	IMS_NUMERIC_DATE dateStruct;
	char header[80];  /* SV data file header information */
	char tablename[32];
	int status;
	char sv_flag; /* Restituted or Predicted */
	IMS_QI_DESC_OBJ *qDesc;
	char qbuf[IMS_COL512_LEN+1];
	char * nulls;
	long cnt,cnt2;
	char  prec[2];


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
	IMS_SETBCP (qDesc, IMS_TRUE);
	IMS_SET_VERBOSE (qDesc, 10);

	status = ims_qiLogin(qDesc);

	if (status < IMS_OK)
	{

		(void) ims_msg(msgDesc, status, "Could not login to database.");
		(void) ims_qiFreeDesc(qDesc);
		return(status);
	}

	IMS_SET_USERDATA(qDesc);
	qDesc->cmd = qbuf;

	/*
	** Determine table name based on platform type.
	*/
	(void) sprintf(qDesc->cmd, "select sv_table_name from platform_tables\
				where platform ='%s'", platform);

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{

		/*
		** We must be able to get the platform name.
		*/

		if (status < IMS_OK)
		{
			(void) ims_qiFreeDesc(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get state vector name for platform %s.",
				platform);
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
		** Get the table name from the query descriptor.
		*/

		(void) memcpy(tablename, qDesc->valAddr[0], qDesc->valLength[0]);
		tablename[qDesc->valLength[0]] = '\0';
		(void) strcpy(tablename, ims_truncStr(tablename));
	}

	/*
	** Make sure the tablename was retrieved.
	*/

	if (IMS_AFFECTED(qDesc) < 1)
	{
		(void) ims_qiFreeDesc(qDesc);
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get state vector table name for platform %s", platform);
		return(IMS_ERROR);
	}

	/*
	** Reset Query Descriptor
	*/

	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_qiFreeDesc(qDesc);
		(void) ims_msg(msgDesc, IMS_FATAL, "Could not reset query descriptor.");
		return(IMS_FATAL);
	}

	/*
	** Determine Start and End Times
	*/

	(void) sprintf(qDesc->cmd, "select VALID_START_TIME, VALID_END_TIME, \
		GEN_FILE_TYPE from %s where granule_idx = %d", granuleName,
		granuleIdx);

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{

		/*
		** We must be able to get the platform name.
		*/

		if (status < IMS_OK)
		{
			(void) ims_qiFreeDesc(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get state vector start/end time for granule %d.",
				granuleIdx);
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
		** Get the start time from the query descriptor.
		*/

		(void) memcpy(start_time, qDesc->valAddr[0], qDesc->valLength[0]);
		start_time[qDesc->valLength[0]] = '\0';
		(void) strcpy(start_time, ims_truncStr(start_time));

		/*
		** Get the end time from the query descriptor.
		*/

		(void) memcpy(end_time, qDesc->valAddr[1], qDesc->valLength[1]);
		end_time[qDesc->valLength[1]] = '\0';
		(void) strcpy(end_time, ims_truncStr(end_time));

		status = ims_timeToNumericDate(msgDesc, end_time, &dateStruct);

		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not convert end date ");
			return(status);
		}

		/*
		** Extend out the end time for the delete to make sure we
		** don't have any round-off errors.
		*/

		if (dateStruct.msecs = 999)
		{
			dateStruct.seconds ++;
			dateStruct.msecs = 0;
		}
		else
			dateStruct.msecs ++;

		ims_numericDateToIMSA(&dateStruct, end_time);


		/*
		** Get the general file type from the query descriptor.
		*/

		if ((qDesc->valLength[2] == strlen("PREDICTED_STATE_VECTORS")) &&
			(memcmp(qDesc->valAddr[2], "PREDICTED_STATE_VECTORS", 23) == 0))
		{
			sv_flag = 'P';
		}
		else if ((qDesc->valLength[2] == strlen("RESTITUTED_STATE_VECTORS"))
		   && (memcmp(qDesc->valAddr[2], "RESTITUTED_STATE_VECTORS", 24)== 0))
		{
			sv_flag = 'R';
		}
		else
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"state vector data does not have PREDICTED or RESTITUTED.");

			fclose(filePtr);
			return(IMS_ERROR);
		}

		*sv_type = sv_flag;
	}

	if (IMS_AFFECTED(qDesc) < 1)
	{
		(void) ims_qiFreeDesc(qDesc);
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get start/end time");
		return(IMS_ERROR);
	}

	/*
	** Reset Query Descriptor
	*/

	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_qiFreeDesc(qDesc);
		(void) ims_msg(msgDesc, IMS_FATAL, "Could not reset query descriptor.");
		return(IMS_FATAL);
	}

	/*
	** Delete all entries in the state vector file between the
	** valid start and valid end dates.
	*/

	(void) sprintf(qDesc->cmd, "delete from %s where date >= '%s' and \
			date <= '%s' and prec = '%c'", tablename, start_time, end_time,
			sv_flag);

	if (execCmd (msgDesc, qDesc) < IMS_OK)
	{
		(void) ims_qiFreeDesc(qDesc);
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Error deleting obsolete state vector data");
		return (IMS_ERROR);
	}

	/*
	** Open file
	*/
	if ((filePtr = fopen(filename, "r")) == NULL)
	{

		(void) ims_qiFreeDesc(qDesc);
		(void) ims_msg (msgDesc, IMS_FATAL,
			"state vector data file %s not found.", filename);
		return(IMS_FATAL);
	}

	/*
	** Scan through data file and insert.
	*/

	/*
	** Get initial state vector header info. Ignore, in material to us.
	*/

	(void) fgets(header, 80, filePtr);

	if (feof(filePtr))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,

			"state vector data file %s has invalid format.", filename);

		fclose(filePtr);

		return(IMS_ERROR);
	}

	/*
	** 2nd header line
	*/

	(void) fgets(header, 80, filePtr);

	if (feof(filePtr))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,

			"state vector data file %s has invalid format.", filename);

		fclose(filePtr);
		(void) ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	/* ****************************
	**  bcp changes: initialize, set up variables
	*/
	prec[0] = sv_flag;
	prec[1] = '\0';
	nulls = NULL;
	status = bcp_init( qDesc->dbproc, tablename, nulls, nulls, DB_IN );

	/*
	**  now need each column defined.
	*/
	if(  bcp_bind( qDesc->dbproc, (BYTE *) dateStr, 0, -1,
		(BYTE *) "", 1, 0, 1 )  ==  FAIL  ){
		(void) ims_msg (msgDesc, IMS_ERROR,
			"failed to bcp bind col 1" );
		fclose(filePtr);
		return(IMS_ERROR);
	}
	if(  bcp_bind( qDesc->dbproc, (BYTE *) prec, 0, -1,
		(BYTE *) "", 1, 0, 2 )  ==  FAIL  ){
		(void) ims_msg (msgDesc, IMS_ERROR,
			"failed to bcp bind col 2" );
		fclose(filePtr);
		return(IMS_ERROR);
	}
	if(  bcp_bind( qDesc->dbproc, (BYTE *) revolution, 0, -1,
		(BYTE *) "", 1, 0, 3 )  ==  FAIL  ){
		(void) ims_msg (msgDesc, IMS_ERROR,
			"failed to bcp bind col 3" );
		fclose(filePtr);
		return(IMS_ERROR);
	}
	if(  bcp_bind( qDesc->dbproc, (BYTE *) x_pos, 0, -1,
		(BYTE *) "", 1, 0, 4 )  ==  FAIL  ){
		(void) ims_msg (msgDesc, IMS_ERROR,
			"failed to bcp bind col 4" );
		fclose(filePtr);
		return(IMS_ERROR);
	}
	if(  bcp_bind( qDesc->dbproc, (BYTE *) y_pos, 0, -1,
		(BYTE *) "", 1, 0, 5 )  ==  FAIL  ){
		(void) ims_msg (msgDesc, IMS_ERROR,
			"failed to bcp bind col 5" );
		fclose(filePtr);
		return(IMS_ERROR);
	}
	if(  bcp_bind( qDesc->dbproc, (BYTE *) z_pos, 0, -1,
		(BYTE *) "", 1, 0, 6 )  ==  FAIL  ){
		(void) ims_msg (msgDesc, IMS_ERROR,
			"failed to bcp bind col 6" );
		fclose(filePtr);
		return(IMS_ERROR);
	}
	if(  bcp_bind( qDesc->dbproc, (BYTE *) x_vel, 0, -1,
		(BYTE *) "", 1, 0, 7 )  ==  FAIL  ){
		(void) ims_msg (msgDesc, IMS_ERROR,
			"failed to bcp bind col 7" );
		fclose(filePtr);
		return(IMS_ERROR);
	}
	if(  bcp_bind( qDesc->dbproc, (BYTE *) y_vel, 0, -1,
		(BYTE *) "", 1, 0, 8 )  ==  FAIL  ){
		(void) ims_msg (msgDesc, IMS_ERROR,
			"failed to bcp bind col 8" );
		fclose(filePtr);
		return(IMS_ERROR);
	}
	if(  bcp_bind( qDesc->dbproc, (BYTE *) z_vel, 0, -1,
		(BYTE *) "", 1, 0, 9 )  ==  FAIL  ){
		(void) ims_msg (msgDesc, IMS_ERROR,
			"failed to bcp bind col 9" );
		fclose(filePtr);
		return(IMS_ERROR);
	}

	/*
	** Parse row 1 data information.
	*/

	(void) fscanf(filePtr, "%s %s %s %s %s %s %s %s %s",  revolution,
		year, time, x_pos, y_pos, z_pos, x_vel, y_vel, z_vel);
	cnt = 0;

	while (!feof(filePtr))
	{
		cnt++;
		/*
		** Format date in IMS format.
		*/

		(void) sprintf(dateStr, "%s %s",year, time);

		status = ims_timeToNumericDate(msgDesc, dateStr, &dateStruct);

		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
					"Could not parse date from state vector file.");
			return(status);
		}

		ims_numericDateToIMSA(&dateStruct, dateStr);

		/*
		** Add row data
		*/
		if(  bcp_sendrow( qDesc->dbproc )  ==  FAIL ){
			(void) ims_msg(msgDesc, IMS_ERROR,
					"Error sending sv row %d.", cnt );
			return( IMS_ERROR );
		}

		(void) fscanf(filePtr, "%s %s %s %s %s %s %s %s %s",  revolution,
			year, time, x_pos, y_pos, z_pos, x_vel, y_vel, z_vel);
	}
	cnt2 = bcp_done( qDesc->dbproc );
	if(  cnt2  !=  cnt  ){
		(void) ims_msg( msgDesc, IMS_ERROR,
			"Sent %d rows, but only %d was inserted (bcp_done).",
			cnt, cnt2 );
		return( IMS_ERROR );
	}
	(void) ims_msg(msgDesc, IMS_INFO,
		"Sent %d rows of sv data.", cnt );
	(void) ims_qiFreeDesc(qDesc);
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
** getGranuleInfo
**
** This function will get the granule information for the state vector
** file which came in and update the sv_avail table.
******************************************************************************/

static int getGranuleInfo(
	IMS_MSG_STRUCT *msgDesc,
	USERSPEC *userSpec,
	char *granuleName,
	int granuleIdx,
	int datasetIdx,
	char sv_flag)
{
	IMS_QI_DESC_OBJ *qDesc;
	char qbuf[IMS_COL1024_LEN+1];
	int status;
	char start_time[IMS_DATETIME_LEN+1];
	char end_time[IMS_DATETIME_LEN+1];
	int start_rev;
	int end_rev;
	char platform[IMS_COL30_LEN+1];


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


	status = ims_qiLogin(qDesc);

	if (status < IMS_OK)
	{

		(void) ims_msg(msgDesc, status, "Could not login to database.");
		ims_qiFreeDesc(qDesc);
		return(status);
	}

	IMS_SET_USERDATA(qDesc);
	qDesc->cmd = qbuf;

	/*
	** Get granule information
	*/

	sprintf(qDesc->cmd, "select VALID_START_TIME, \
		VALID_END_TIME, START_REV, END_REV, PLATFORM \
		from %s where granule_idx = %d",
		granuleName, granuleIdx);

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		/*
		** We must be able to get the platform name.
		*/

		if (status < IMS_OK)
		{
			ims_qiFreeDesc(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get state vector information for sv_available table.");
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
		** Get the start time from the query descriptor.
		*/

		(void) memcpy(start_time, qDesc->valAddr[0], qDesc->valLength[0]);
		start_time[qDesc->valLength[0]] = '\0';
		(void) strcpy(start_time, ims_truncStr(start_time));

		/*
		** Get the end time from the query descriptor.
		*/

		(void) memcpy(end_time, qDesc->valAddr[1], qDesc->valLength[1]);
		end_time[qDesc->valLength[1]] = '\0';
		(void) strcpy(end_time, ims_truncStr(end_time));

		/*
		** Get the start revolution
		*/

		(void) memcpy(&start_rev, qDesc->valAddr[2], qDesc->valLength[2]);

		/*
		** Get the end revolution
		*/

		(void) memcpy(&end_rev, qDesc->valAddr[3], qDesc->valLength[3]);


		/*
		** Get the platform
		*/
		(void) memcpy(platform, qDesc->valAddr[4], qDesc->valLength[4]);
		platform[qDesc->valLength[4]] = '\0';
		(void) strcpy(platform, ims_truncStr(platform));
	}

	/*
	** Reset Query Descriptor
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		ims_qiFreeDesc(qDesc);
		ims_msg(msgDesc, IMS_FATAL, "Could not reset query descriptor.");
		return(IMS_FATAL);
	}

	/*
	** Update sv_available table.
	*/
	if (sv_flag == 'R')
	{
		sprintf(qbuf, "insert into sv_available (dataset_idx, granule_idx, \
			status, PLATFORM, START_REV, END_REV, START_TIME, END_TIME, \
			SV_PRECISION) values (%d, %d, %d, '%s', %d , %d, '%s', '%s', \
			'RESTITUTED')", datasetIdx, granuleIdx, 1, platform, start_rev,
			end_rev, start_time, end_time);
	}
	else
	{
		sprintf(qbuf, "insert into sv_available (dataset_idx, granule_idx, \
			status, PLATFORM, START_REV, END_REV, START_TIME, END_TIME, \
			SV_PRECISION) values (%d, %d, %d, '%s', %d, %d, '%s', '%s', \
			'PREDICTED')", datasetIdx, granuleIdx, 1, platform, start_rev,
			end_rev, start_time, end_time);
	}

	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		/*
		** We must be able to get the platform name.
		*/

		if (status < IMS_OK)
		{
			ims_qiFreeDesc(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not insert into sv_avail table");
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

	ims_qiFreeDesc(qDesc);
	return(IMS_OK);
}
