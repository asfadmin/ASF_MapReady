static char *sccs = "@(#)ims_tapeScan.c	5.2 03/07/97";
/******************************************************************************
**
** File:        ims_tapeScan.c
**
** Function:    Issue a scan job for the given tape
**              by executing the ims_tapeAvailAux application.
**
** Author:      Dan Crichton
**
** Date:        12/11/95
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
#include <ims_cmd.h>
#include <signal.h>
#include <syslog.h>
  
/*
** Message structure that is received from the auxillary process.
*/
typedef struct 
{
	char reply[IMS_COL255_LEN + 1];
	int kind;       /* type of buffer, CMD(1), MSG(2), other */
	int severity;
	int errorno;
	int LCF;            /* last command flag        */
	int LMF;            /* last message flag        */
					
} BUFFERSPEC_TYPE;

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *username;
	char *password;
	char *server;
	char *database;
	char *accountId;
	char *tapeId;
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
	{"-X",           &commands.server},
	{"+server",      &commands.server},
	{"-Y",           &commands.database},
	{"+database",    &commands.database},
	{"-A",           &commands.accountId},
	{"+accountId",	 &commands.accountId},
	{"-M",           &commands.tapeId},
	{"+mediaId",     &commands.tapeId}
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
	{"accountId",   &commands.accountId},
	{"mediaId",     &commands.tapeId}
};

static struct 
{
	char username[IMS_COL15_LEN+1];
	char password[IMS_COL15_LEN+1];
	char program[IMS_COL30_LEN+1];
	char server[IMS_COL30_LEN+1];
	char database[IMS_COL30_LEN+1];
	char accountId[IMS_COL30_LEN+1];
} userSpec;

/*
** Local function prototypes.
*/
static int openConnection (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ **);
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);
static int getArgInput (IMS_MSG_STRUCT *);
int shutdown(int, int, char *);
static void decodeAuxProcMessage (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, char *, 
	BUFFERSPEC_TYPE *);
static int getGranuleTableName(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, char *, 
	char *);
static int closeConnection (IMS_QI_DESC_OBJ *);
static int getGranuleIndex (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, int *, char *, char *);

/*
** Global Variables.
*/
char *glbl_dataset = "HC TAPE AVAILABILITY";
char glbl_tapeId[IMS_COL30_LEN+1];
static char *glb_programName;
static IMS_MSG_STRUCT *glbl_msgDesc;

/**************************************************************************
**   
** main
**
**************************************************************************/

void main(int argc, char *argv[])
{
	struct utsname uname_info;    /* Structure for uname() */
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	int status;
	IMS_MSG_STRUCT *msgDesc;

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
		exit (1);
	}

	glbl_msgDesc = msgDesc;
	glb_programName = ims_extractFileName (argv[0]);
		 
	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgProgramName (msgDesc, glb_programName);
	(void) sprintf (banner, "%s::%s", hostName, glb_programName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

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
	** Process the information from the command-line and/or command-file.
	*/
	if ((status = getArgInput (msgDesc)) < IMS_OK)
	{
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}
											 

	/*
	** Setup signal handler to shutdown for ctrl-c...
	*/
	if (sigset(SIGINT, shutdown) == -1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR, "Could not setup signal handler.");
		(void) ims_msgStructFree (msgDesc);
		exit(1);
	}

	(void) ims_msg (msgDesc, IMS_INFO,
		"Starting scan job submission for tape '%s'.", glbl_tapeId);

	/*
	** Run the auxiliary program to submit the scan job(s).
	*/
	if ((status = runAuxProgram (msgDesc, "ims_tapeAvailAux")) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Tape scan job submission failed.");
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	(void) ims_msg (msgDesc, IMS_INFO,
		"If an order identifier was displayed in the previous messages, "
		"then the scan job submission was successful, otherwise the "
		"given tape or its contents did not meet the criteria for "
		"issuing a scan job.");

	(void) ims_msgStructFree (msgDesc);
	exit (0);
}

/******************************************************************************
**
** runAuxProgram ()
**
** Setup and invoke the auxillary program.
**
******************************************************************************/


int runAuxProgram(   
	IMS_MSG_STRUCT *msgStruct,
	char *auxName)
{
	BUFFERSPEC_TYPE bufferSpec;
	char auxBuffer[IMS_COL255_LEN+1];
	int savefd0, savefd1;
	int pid;
	int i;
	int lastMsg;
	int status;
	IMS_QI_DESC_OBJ *qDesc;   
	int severity = IMS_OK;
	
	/*
	** Open a connection to the SQL Server
	*/

	if (openConnection (msgStruct, &qDesc) < IMS_OK)
	{
		(void) ims_msg(msgStruct, IMS_ERROR,
			"Could not open a connection to the SQL Server");
		return(IMS_ERROR);
	}
	
	/*
	** Get a copy of file descriptors 0 and 1 (stdin, stdout).
	** Kick off the auxillary process and then communicate.
	*/


	if ((savefd0 = dup (0)) == -1)
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Dup system call failed for file descriptor '0'. Contact the DBA.");
		(void) closeConnection (qDesc);
		return (IMS_FATAL);
	}

	if ((savefd1 = dup (1)) == -1)
	{
		(void) close (savefd0);
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Dup system call failed for file descriptor '1'. Contact the DBA.");
		(void) closeConnection (qDesc);
		return (IMS_FATAL);
	}

	/*
	** Start the auxillary process.
	*/


	if (invokeAux (auxName, &pid, msgStruct) < IMS_OK)
	{
		(void)ims_msg (msgStruct, IMS_ERROR,
			"Could not invoke process %s", auxName);

		/*
		** Reset the fileDescriptors befor returning..
		*/
		(void) close (0);
		(void) close (1);

		if ((dup (savefd0) == -1) || (dup (savefd1) == -1))
		{
			(void) ims_msg (msgStruct, IMS_FATAL,
				"Dup system call failed for saved file descriptors.");
		}
		(void) close (savefd0);
		(void) close (savefd1);
		(void) closeConnection (qDesc);

	}

	/*
	** Start communication with the auxiliary process
	*/

	lastMsg = 0;
	i=0;
	while (! lastMsg)
	{
		i++;
		if (read (0, auxBuffer, IMS_COL255_LEN+1) == 0)
		{
			/*
			** Premature EOF detected, AUXP is terminated
			*/

			/*
			** Reset the fileDescriptors before returning..
			*/
			(void) close (0);
			(void) close (1);
			if ((dup (savefd0) == -1) ||
				(dup (savefd1) == -1))
			{
				(void) ims_msg (msgStruct, IMS_FATAL,
					"Dup system call failed for saved file descriptors.");
			}
			(void) close (savefd0);
			(void) close (savefd1);
			(void) closeConnection (qDesc);

			if (i > 1)
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
					"Premature process termination: %s.", 
					auxName);
			}
			else
			{
				(void) ims_msg (msgStruct, IMS_ERROR,
			  		"Could not run auxilliary process '%s'",
					auxName);
			}

			return (IMS_FATAL);
		}

		/*
		** Decode message from aux process and draft replies if
		** necessery, or process messages.
		*/
		decodeAuxProcMessage (msgStruct, qDesc, auxBuffer, &bufferSpec);

		/*
		** Three buffer types are possible: CMD(1), MSG(2),
		** OTHER.
		*/

		switch (bufferSpec.kind)
		{
		case 1: /* send reply */
			write (1, bufferSpec.reply, 
				 strlen (bufferSpec.reply) + 1);

			break;

		case 2: /* put message in the msgQueue */
			if (bufferSpec.LMF)
			{
				lastMsg = 1; /*last message*/
			}
			else
			{
				severity = bufferSpec.severity;
				(void) ims_msg (msgStruct,
					bufferSpec.severity,
					bufferSpec.reply);
			}
			break;

		default:  /* Just ignore it */
				break;
		}
	}

	/*
	** Reset the fileDescriptors before returning..
	*/
	(void) close (0);
	(void) close (1);

	if ((dup (savefd0) == -1) || (dup (savefd1) == -1))
	{
		(void) ims_msg (msgStruct, IMS_FATAL,
			"Dup system call failed for saved file descriptors.");
	}
	(void) close (savefd0);
	(void) close (savefd1);
	
	/*
	** Now wait for the child to terminate.
	*/

	if (ims_waitForChild (msgStruct, pid) < IMS_OK)
	{
		(void) ims_msg (msgStruct, IMS_ERROR,
				"Auxiliary Process, '%s', did not terminate normally.",
		auxName);
		(void) closeConnection (qDesc);

		return (IMS_FATAL);
	}

	(void) closeConnection (qDesc);

	if (severity < IMS_OK)
	{
		return (severity);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** invokeAux ()
**
** Invoke the auxiliary process.
**
******************************************************************************/

int invokeAux (
	char *auxName,
	int *pid,
	IMS_MSG_STRUCT *msgStruct)
{
	char fullPath[256];
	char srcPath[256];

	if (getenv("IMS_EXEC_PATH") != NULL)
	{
		strcpy(srcPath, getenv("IMS_EXEC_PATH"));
	}
	else
	{
		strcpy(srcPath, ".");
	}

	sprintf(fullPath, "%s/%s", srcPath,  auxName);
	

	if ((*pid = ims_startChild2 (msgStruct, fullPath, fullPath,
		"DUPLEX_PIPE", (char *) NULL)) < 0)
	{
		return (IMS_FATAL);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** decodeAuxProcMessage ()
**
** Routine for recognizing and parsing the communication messages between ftr
** and auxProcess over a duplex pipe.
**
******************************************************************************/

static void decodeAuxProcMessage (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	char *buffer,
	BUFFERSPEC_TYPE *bufferSpec)
{
	/*
	** Local variables are defined here
	*/
	char *temp;
	char *t,*t1,*t2,*t3,*t4;
	int length;
	char fileName[IMS_COL30_LEN+1];
	char *fileNamePtr;
	char workbuf[IMS_COL255_LEN+1];
	int granuleIndex;


	length = strlen (buffer);
	temp = buffer;

	if((strncmp (buffer, "CMD:", 4) == 0) && (length > 5) &&
		((t1 = strchr (buffer, ':')) != (char *) NULL) &&
		((t2 = strchr (++t1, ':')) != (char *) NULL))
	{
		/*
		** Looks like a CMD message
		*/
		t2++;
		bufferSpec->kind = 1;
		t = t1;
		t--;
		t[0] = '\0';
		t = t2;
		t--;
		t[0] = '\0';

		if (t1[0] == '\0') 
		{
			bufferSpec->LCF = 0;
		}
		else 
		{
			(void) sscanf (t1, "%d", &(bufferSpec->LCF));
		}

		temp = t2;

		if (strncmp (temp, "dbUserName", 10)  == 0)
		{
			sprintf(bufferSpec->reply, 
				"%s\n", userSpec.username);
		}
		else if (strncmp (temp, "dbPassword", 10) == 0)
		{
			sprintf(bufferSpec->reply, 
				"%s\n", userSpec.password);
		}
		else if (strncmp (temp, "server", 6) == 0)
		{
			sprintf(bufferSpec->reply, 
				"%s\n", userSpec.server);
		}
		else if (strncmp (temp, "dbName", 6) == 0)
		{
			sprintf(bufferSpec->reply, 
				"%s\n", userSpec.database);
		}
		else if (strncmp (temp, "fileName", 8) == 0)
		{
		}
		else if (strncmp (temp, "repositoryDir", 13) == 0)
		{
		}
		else if (strncmp (temp, "granuleName", 11) == 0)
		{
			if (getGranuleTableName(msgDesc, qDesc, workbuf, 
							glbl_dataset) < IMS_OK)
			{
				return;
			}
			sprintf(bufferSpec->reply, 
				"%s\n", workbuf);
		}
		else if (strncmp (temp, "platform", 8) == 0)
		{
		}
		else if (strncmp (temp, "sensor", 6) == 0)
		{
		}
		else if (strncmp (temp, "dataset", 7) == 0)
		{
		}
		else if (strncmp (temp, "version", 7) == 0)
		{
		}
		else if (strncmp(temp, "granuleIdx", 10) == 0)
		{
			if (getGranuleTableName(msgDesc, qDesc, workbuf, 
							glbl_dataset) < IMS_OK)
			{
				return;
			}
			if (getGranuleIndex(msgDesc, qDesc, &granuleIndex,
					glbl_tapeId, workbuf) < IMS_OK)
			{
				return;
			}
			sprintf(bufferSpec->reply, 
					"%d\n", granuleIndex);
		}
		else if (strncmp(temp, "accountId", 9) == 0)
		{
			(void) sprintf (bufferSpec->reply, "%s\n",
						userSpec.accountId);
		}
        else if (strncmp(temp, "clientUser", 10) == 0)
		{
				(void) sprintf (bufferSpec->reply, "%s\n",
					userSpec.username);
		}
		else if (strncmp(temp, "clientPassword", 14) == 0)
		{
			 (void) sprintf (bufferSpec->reply, "%s\n",
					userSpec.password);
		}
		else 
		{
			(void) sprintf (bufferSpec->reply, "\n");
		}
	}
	else
	{
		if ((strncmp (buffer, "MSG:", 4) == 0) && (length > 7) &&
			 (buffer[length-1] == '\n') && 
			 ((t1 = strchr (buffer, ':')) != (char *) NULL) &&
			 ((t2 = strchr (++t1, ':')) != (char *) NULL) &&
			 ((t3 = strchr (++t2, ':')) != (char *) NULL) &&
			 ((t4 = strchr (++t3, ':')) != (char *) NULL) )
		{
			/*
			** Looks like a MSG
			*/
			++t4;
			bufferSpec->kind = 2;

			/*
			** Replace ':' with '\0' in the buffer string.
			*/
			t = t1;
			t--;
			t[0] = '\0';
			t = t2;
			t--;
			t[0] = '\0';
			t = t3;
			t--;
			t[0] = '\0';
			t = t4;
			t--;
			t[0] = '\0';
			buffer[length-1] = '\0';
			length--;
	
			if (t1[0] == '\0')
			{
				bufferSpec->severity = IMS_OK;
			}
			else
			{
				(void) sscanf (t1, "%d",
					&(bufferSpec->severity));
			}

			if (t2[0] == '\0')
			{
				bufferSpec->errorno = 0;
			}
			else
			{
				(void) sscanf (t2, "%d",
					&(bufferSpec->errorno));
			}

			if (t3[0] == '\0')
			{
				bufferSpec->LMF = 0;
			}
			else
			{
				(void) sscanf (t3, "%d", &(bufferSpec->LMF));
			}
			(void) sprintf (bufferSpec->reply, "%s", t4);
		}
		else
		{
			bufferSpec->kind = 0;

			if (buffer[length-1] != '\n')
			{
				buffer[length-1] = '\n';
			}
			(void) strcpy (bufferSpec->reply, buffer);
		}
	}
}

/******************************************************************************
**
** shutdown ()
**
/******************************************************************************

int shutdown(int i_sig, int i_code, char *data)
{
	fprintf(stderr, "\nClosing connection to auxillary process.\n");
	(void) ims_msgStructFree (glbl_msgDesc);
	exit(1);
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

	(void) memset((char *) &userSpec, 0, sizeof(userSpec));

	/* username */
	if (commands.username != (char *) NULL)
	{
		(void) strcpy(userSpec.username, commands.username);
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
		(void) strcpy(userSpec.password, commands.password);
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
		(void) strcpy(userSpec.server, commands.server);
	}
	else
	{
		if (getenv("IMS_SERVER") != NULL)
			(void) strcpy(userSpec.server, getenv("IMS_SERVER"));
		else
		{			
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"IMS_SERVER environment variable not set");
			return(IMS_ERROR);
		}
	}


	/* database */
	if (commands.database != (char *) NULL)
	{
		(void) strcpy(userSpec.database, commands.database);
	}
	else
	{
		
		if (getenv("IMS_DB") != NULL)
			(void) strcpy(userSpec.database, getenv("IMS_DB"));
		else
		{			
			(void) ims_msg(msgDesc, IMS_ERROR, 
				"IMS_DB environment variable not set");
			return(IMS_ERROR);
		}
	}


	/* accountId */
	if (commands.accountId != (char *) NULL)
	{
		(void) strcpy(userSpec.accountId, commands.accountId);
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Account ID: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		(void) strcpy (userSpec.accountId, inputBuffer);
	}

	/* tapeId */
	if (commands.tapeId != (char *) NULL)
	{
		(void) strcpy(glbl_tapeId, commands.tapeId);
	}
	else
	{
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Media ID: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		(void) strcpy (glbl_tapeId, inputBuffer);
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
	char *dataset_name)
{
	int status;
	char qbuf[IMS_COL512_LEN];

	sprintf(qDesc->cmd,
		"select p.granules_table from dataset_relation r, \
			dataset_policy p where r.dataset = '%s' and \
			p.dataset_idx = r.dataset_idx",
			dataset_name);
		
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
** getGranuleIndex ()
**
******************************************************************************/

static int getGranuleIndex (
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	int *granuleIndex,
	char *tape_id,
	char *tableName)
{
	int status;
	char qbuf[IMS_COL512_LEN];

	(void) sprintf(qDesc->cmd,
		"select granule_idx "
		"from %s "
		"where MEDIA_ID = '%s' "
		"and (MEDIA_ID_TYPE_NAME = 'WORKING_SIGNAL' or "
		"MEDIA_ID_TYPE_NAME = 'QUICKLOOK') "
		"and STATUS = 'HST_S_AVAILABLE'",
		tableName, tape_id);
		
	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
					   "Could not perform to determine dataset information");
			(void) ims_qiFreeDesc (qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		(void) memcpy(granuleIndex, qDesc->valAddr[0], qDesc->valLength[0]); 

	}

	if (IMS_AFFECTED(qDesc) < 1) 
	{
		/* 
		** This is a problem ...
		*/

		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not find tape %s to scan.", tape_id);

		return(IMS_ERROR);

	}

	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not reinitialize the query descriptor.");
		(void) ims_qiFreeDesc (qDesc);
		return (IMS_FATAL);
	}

	return(IMS_OK);
}

