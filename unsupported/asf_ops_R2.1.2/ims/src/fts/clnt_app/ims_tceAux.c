static char *sccs = "@(#)ims_tceAux.c	5.4 24 Jul 1996";
/******************************************************************************
**
** File:	ims_tceAux.c
**
** Function:	Perform all auxillary processing using a pipe with the 
**				FTS network server for processing Time Correlation files .
**
** Author: Dan Crichton	
**
** Date:	5/23/95
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
#include <ims_cmd.h>
#include <ims_version.h>
#include <ims_util.h>
#include <ims_getInput.h>
#include <ims_signal.h>
#include <ims_query.h> 
#include <ims_timeConv.h>

#include <syslog.h>


/*
** External Functions
*/
extern int ims_forwardMsgQueue (IMS_MSG_STRUCT *);

/*
** Local Functions
*/
static int execCmd (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
int parseTCEfile (IMS_MSG_STRUCT *,  char *, char *, IMS_QI_DESC_OBJ *);


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
	char hostName[65];
	IMS_QI_DESC_OBJ *qDesc;
	char extractFile [255];
	struct utsname uname_info;    /* Structure for uname() */


	/*
	** Initialize the message processor.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr,
			"Memory allocation for IMS_MSG_STRUCT structure failed.");
			exit (1);
	}

	glb_programName = ims_extractFileName (argv[0]);
	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, sizeof (hostName));


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
	** Allocate a query descriptor
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not allocate a query descriptor.");
		exit(1);
	}	

	qDesc->cmd = (char *) malloc (IMS_COL1024_LEN);
	if ((char *) qDesc == NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not allocate a the command area for the query descriptor.");
		exit(1);

	}

	/*
	** Allocate memory for commands and replies.
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
	(void) strcpy (cmds[6], "CMD:0:platform: ");
	(void) strcpy (cmds[7], "CMD:1:version: "); 

	/*
	** request for required information
	*/
	cmdLen = 7;
	for ( i=0; i < cmdLen; i++ )
	{
#ifdef DEBUG
		(void) fprintf(stderr, "Sending CMD[%d] to FTS SERVER:%s\n",i, cmds[i]);
#endif
		(void) write (1, cmds[i], strlen (cmds[i]) + 1);
		if (read( 0, replies[i], IMS_COL255_LEN+1) == 0)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Premature ftr termination, abort auxProcess");
			exit (0);
		}
#ifdef DEBUG
		(void) fprintf(stderr, "Receiving reply from FTS SERVER:%s\n", replies[i]);
#endif		

		/*
		** Get read of new line characters
		*/
		replies[i][strlen(replies[i])-1] = '\0';
	}

	/*
	** Close connection with server
	*/

	(void) strcpy (fullPathName, replies[3]);
	(void) strcat (fullPathName, "/");
	(void) strcat (fullPathName, replies[2]);


	/*
	** If versions are supported, then add the version number
	** to the end of the filename.
	*/
	
	if (atoi(replies[7]) > 0)
	{
		(void) strcat (fullPathName, replies[7]);    
	}
						 
#ifdef DEBUG
	fprintf(stderr, "Parsing Aux File %s\n", fullPathName);
#endif


	IMS_SETUSER (qDesc, replies[0]);
	IMS_SETPSWD (qDesc, replies[1]);
	IMS_SETPROG (qDesc, glb_programName);
	IMS_SETSERVER (qDesc, replies[4]);
	IMS_SETDBNAME (qDesc, replies[5]);
	IMS_SET_VERBOSE (qDesc, 10);

	status = ims_qiLogin(qDesc);
	IMS_SET_USERDATA(qDesc);


	if (parseTCEfile (msgDesc, fullPathName, 
								replies[6], qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not parse TCE file %s",
				fullPathName);
		status = IMS_ERROR;
	}
	else
	{
		status = IMS_OK;
	}

	(void) ims_forwardMsgQueue (msgDesc); 

	/* free */

	(void) ims_qiFreeDesc (qDesc);
	(void) ims_msgStructFree(msgDesc);

	for (i = 0; i < 20; i++)
	{ 
		free(cmds[i]);
		free(replies[i]);
	}

	exit(0);
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
** parseTCEfile
**
******************************************************************************/
 
int parseTCEfile (
	IMS_MSG_STRUCT *msgDesc, 
	char *filename,
	char *platform,
	IMS_QI_DESC_OBJ *qDesc)
{
	FILE *filePtr;
	char rev[6];  /* Sizes from JPL D-5267 */
	char year[5];
	char time[17];
	char sat_time[12];
	char clk_cycle[12];
	char dateStr[100];
	IMS_NUMERIC_DATE dateStruct;
	char header[80];  /* TCE data file header information */
	char tablename[32];
	int status;


	/*
	** Determine table name based on platform type.
	*/ 
	(void) sprintf(qDesc->cmd, "select tce_table_name from platform_tables\
		where platform ='%s'", platform);

	status = ims_qiNextRow (qDesc);

	/*
	** We must be able to get the platform name. 
	*/

	if (status < IMS_OK)
	{
		return(IMS_ERROR);
	}

	/* 
	** Get the table name from the query descriptor.
	*/

	(void) memcpy(tablename, qDesc->valAddr[0], qDesc->valLength[0]);
	tablename[qDesc->valLength[0]] = '\0';
	(void) strcpy(tablename, ims_truncStr(tablename));

	/*
	** Reset Query Descriptor
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not reset query descriptor.");
		return(IMS_ERROR);
	}


	/*
	** Open the file.
	*/
	if ((filePtr = fopen(filename, "r")) == NULL) 
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Time correlation data file '%s' not found.", filename);
		return(IMS_ERROR);
	}

#ifdef DEBUG
		(void) fprintf(stderr, "Processing time correlation element file information...\n");
#endif

	/*
	** Parse header 1 data information.
	*/
	(void) fgets(header, sizeof(header) -1, filePtr);

	/*
	** Read the first data line.
	*/
	(void) fscanf(filePtr, "%s %s %s %s %s",  rev, year, time, sat_time,
		clk_cycle);
	
	/*
	** Begin the transaction.
	*/
	(void) sprintf (qDesc->cmd, "begin transaction");

	if (execCmd (msgDesc, qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not begin the transaction.");
		return (IMS_ERROR);
	}

	/*
	** Now process the first line and remaining data lines and
	** insert them into the database.
	*/
	while (!feof(filePtr))
	{
		/*
		** Format date.
		*/
		(void) sprintf(dateStr, "%s %s", year, time);
		if (ims_timeToNumericDate(msgDesc, dateStr, &dateStruct) < IMS_OK)
		{
			(void) sprintf (qDesc->cmd, "rollback transaction");
			(void) execCmd (msgDesc, qDesc);
			return (IMS_ERROR);
		}
		ims_numericDateToIMSA(&dateStruct, dateStr);

		/*
		** Add row data
		*/

		(void) sprintf(qDesc->cmd,
		"if exists (select date from %s where date = '%s') "
		"begin "
			"update %s set rev = '%s', sat_time = '%s', clk_cycle = '%s' "
			"where date = '%s'"
		"end "
		"else "
		"begin " 
			"insert into %s (rev, date, sat_time, clk_cycle) values " 
			"('%s', '%s', '%s', '%s') "
		"end ",
			 tablename, dateStr,
			tablename, rev, sat_time, clk_cycle, dateStr,
			tablename, rev, dateStr, sat_time, clk_cycle);


#ifdef DEBUG
		fprintf(stderr, "%s\n", qDesc->cmd);	
#endif

		if (execCmd (msgDesc, qDesc) < IMS_OK)
		{
			(void) sprintf (qDesc->cmd, "rollback transaction");
			(void) execCmd (msgDesc, qDesc);
			return (IMS_ERROR);
		}

		(void) fscanf(filePtr, "%s %s %s %s %s",  rev, year, time, sat_time,
			clk_cycle);
	}

	/*
	** Commit the transaction.
	*/
	(void) sprintf (qDesc->cmd, "commit transaction");

	if (execCmd (msgDesc, qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not commit the transaction.");
		return (IMS_ERROR);
	}

	return(IMS_OK);
}


