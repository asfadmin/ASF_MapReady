static char *sccs = "@(#)ims_askClnt.c	5.1  17 Mar 1996";
/*****************************************************************************
**
** File:  ims_askClnt.c
**
** Function:  Command line program to query files based on a specified
**            keyword search criteria.  The search category is limited 
**            to a specified platform, sensor, dataset.
**
** Date:  12 August, 1991
**
** Creator:  H. Sayah
**
** Updated:  May 15, 1995   
**           ASF adoption of AMMOS-CDB procedures. 
**
****************************************************************************/

#include <stdio.h>
#include <stdlib.h> 
#include <string.h>
#include <sys/utsname.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <syslog.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_keyword.h>
#include <ims_msg.h>
#include <ims_hash.h>
#include <ims_cmd.h>
#include <ims_ask.h>
#include <ims_util.h>
#include <ims_getInput.h>

/*
** Declaration of internal static function calls.  Note: Functions appear in 
** this file in the order they are listed below.
*/
static void runDown();
static void usage();
static int readFromFile (); 
static int updateRequest ();


/* global msg pointer */
static IMS_MSG_STRUCT *glbMsgDesc;

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command line.
*/
static struct commands
{
	char *userName;
	char *password;
	char *platform;
	char *sensor;
	char *dataset;
	char *expression;
	char *fileName;
	char *commandFile;
	char *help;
	char *releaseNo;
} commands;
/*
** "Itemlist" for getting the above commands from the commandline.
*/
static IMS_CMD_CLASS cmdLineElm [] =
{
	{"-P",          &commands.platform},
	{"+platform",   &commands.platform},
	{"-S",          &commands.sensor},
	{"+sensor",     &commands.sensor},
	{"-D",          &commands.dataset},
	{"+dataset",    &commands.dataset},
	{"-E",          &commands.expression},
	{"+expression", &commands.expression},
	{"-C",          &commands.commandFile},
	{"+commandFile",&commands.commandFile},
	{"-F",          &commands.fileName},
	{"+fileName",   &commands.fileName},
	{"-h",          &commands.help},
	{"+help",       &commands.help},
	{"-r",          &commands.releaseNo},
	{"+release",    &commands.releaseNo},
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);
/*
** "Itemlist" for getting the above commands from a command file.
*/
static IMS_CMD_CLASS cmdFileElm [] =
{
	{"platform",   &commands.platform},
	{"sensor",     &commands.sensor},
	{"dataset",    &commands.dataset},
	{"fileName",   &commands.fileName},
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);
/*
** Our request structure for file transfer stuff.
*/

main(argc, argv)
int	argc;
char	*argv[];
{
	char *arg;
	IMS_ASK_REQUEST request;
	IMS_ASK_RESULT *result; /* list of files returned. */
	IMS_ASK_RLIST  *resultList;
	IMS_MSG_STRUCT *msgDesc;
	struct utsname uname_info;
	char banner[IMS_HOST_LEN + IMS_PROGRAM_LEN + 3];
	char hostname [IMS_HOST_LEN + 1];
	char *program;
	int status, i;


	/*
	** extractFileName ALWAYS returns a pointer to a string.
	*/
	program = ims_extractFileName (argv[0]);

	/* assign host name */
	(void) uname (&uname_info);
	(void) strncpy (hostname, uname_info.nodename, IMS_HOST_LEN);
	hostname[IMS_HOST_LEN] = '\0';

	/*
	** Initialize message facility.
	*/
	/* Allocate msg structure */
	if ((msgDesc = (IMS_MSG_STRUCT *)ims_msgStructAlloc ())
		== (IMS_MSG_STRUCT *)NULL)
	{
		fprintf (stderr, "IMS_MSG_STRUCT allocation error.");
		exit (-1);
	}
					 
	ims_msgSubSystem (msgDesc, "IMS");
	ims_msgProgramName (msgDesc, program);
	sprintf (banner, "%s::%s", hostname, program);
	ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	ims_msgSeverityFlag (msgDesc, IMS_ON);
	ims_msgStderrFlag (msgDesc, IMS_ON);
	ims_msgQueueFlag (msgDesc, IMS_OFF);
	ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
	ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

	glbMsgDesc = msgDesc;
												 
	/* Initialize request structure */
	request.platform = (char *)NULL;
	request.sensor = (char *)NULL;
	request.dataset = (char *)NULL;
	request.expression = (char *)NULL;
	request.program = program;
	request.catSrvName = "";
	request.catDbName = "";
	request.msgDesc = msgDesc;


	/*
	** Initialize the signal handler.
	*/
	if (ims_setWrapup (runDown) < IMS_OK)
		exit(-1);

	/*
	** Now, get command line args.
	*/
	if ((status = ims_getCmdLine 
		(argc, argv, cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
	{
		runDown (status);
	}

	/*
	** If help has been specified, print usage and exit.
	*/
	if (commands.help != (char *) NULL)
	{
		(void) usage ();
		runDown (-1);
	}
	/*
	** If there is a command file present, then get any commands from
	** this file, then overlay all commands from the commandline, except
	** password, which will be gone by this point.
	*/
	if (commands.commandFile != (char *) NULL)
	{
		if (ims_getFileParms (commands.commandFile, cmdFileElm, 
			cmdFileElmCount, msgDesc) < IMS_OK)
		{
			runDown (ims_msgGetSeverity (msgDesc));
		}
		/*
		** Now, get command line args again to overlay file args.
		*/
		if ((status = ims_getCmdLine (argc, argv, cmdLineElm, 
			cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			runDown (ims_msgGetSeverity (msgDesc));
		}
	}

	/*
	** Update the request structure with information from the command
	** line or from a command file.
	*/
	if (updateRequest (&request) < IMS_OK)
	{
		runDown (-1);
	}

	/*
	** call library function to handle the request.
	*/
	if ((result = ims_ask (&request)) == (IMS_ASK_RESULT *)NULL)
	{
		ims_msg (msgDesc, status,
			"Query from IMS catalog failed.");
		runDown (status);
	}

	resultList = result->resultList;

	printf ("\n Total of '%d' granule(s) matched the criteria:\n",
		result->resultCount);

	printf   ("----------------------------------------------------\n");

	i=0;
	while (resultList != (IMS_ASK_RLIST *) NULL)
	{
		i++;
		if (resultList->version > 0)
		{
			printf(" %3d:  %s, Version %d\n", i, 
				resultList->name, resultList->version);
		}
		else
		{
			printf(" %3d:  %s\n", i, resultList->name);
		}
		resultList = resultList->next;
	}
	printf("\n");
	(void) fflush(stdout);

	runDown (0);

}  /* end of main */


/***************************************************************************
**
**  runDown - Cleanup and exit from program.
**
***************************************************************************/
static void runDown(stat)
int	stat;
{

	/* free msgDesc structure */
	(void) ims_msgStructFree (glbMsgDesc);

	exit(stat);
}

/***************************************************************************
**
** usage() - print command line argument switches
**
***************************************************************************/
static void usage()
{
	int i;

	fprintf(stderr, "\nims_ask usage:\n\n");
	for (i = 0; i < cmdLineElmCount; i++)
	{
		fprintf (stderr, "%s\n", cmdLineElm[i].paramKeyword);
	}
	fprintf(stderr,"\n\n");
}

/***************************************************************************
**
** updateRequest - Update the request structure and prompt for needed 
**                 information not provided in the commandLine and
**                 commandFile.
**
***************************************************************************/

static int updateRequest (
	IMS_ASK_REQUEST *request)
{
	IMS_MSG_STRUCT *msgDesc;
	char buff[IMS_COL1024_LEN + 1];
	int invalid, i, number;
	int status;

	msgDesc = request->msgDesc;

	/*
	** Prompt user for any information NOT provided in the command structure.
	*/
	if (commands.platform != (char *) NULL)
	{
		request->platform = commands.platform;
	}
	else
	{
		if (ims_getString (IMS_TRUE, buff, sizeof (buff),
			"Input platform:  ") == (char *)NULL)
		{
			ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}
		request->platform = (char *)strdup (buff);
	}

	if (commands.sensor != (char *) NULL)
	{
		request->sensor = commands.sensor;
	}
	else
	{
		if (ims_getString (IMS_FALSE, buff, sizeof (buff),
			"Input sensor:  ") == (char *)NULL)
		{
			ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}
		request->sensor = (char *)strdup (buff);
	}


	if (commands.dataset != (char *) NULL)
	{
		request->dataset = commands.dataset;
	}
	else
	{
		if (ims_getString (IMS_TRUE, buff, sizeof (buff),
			"Input dataset:  ") == (char *)NULL)
		{
			ims_msg (msgDesc, IMS_FATAL, 
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}
		request->dataset = (char *)strdup (buff);
	}

	if (commands.expression != (char *)NULL)
	{
		request->expression = commands.expression;
	}
	else
	{
		if (commands.fileName != (char *)NULL)
		{
			if ((status = readFromFile (request, commands.fileName)) < IMS_OK)
			{
				return (status);
			}
		}
		else
		{
			if (ims_getString (IMS_FALSE, buff, sizeof (buff),
				"Input expression:  ") == (char *)NULL)
			{
				ims_msg (msgDesc, IMS_FATAL, 
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}
			request->expression = (char *)strdup (buff);
		}
	}

	return (IMS_OK);
}

/***************************************************************************
**
** readFromFile - Read criteria file keyword search statements into 
**                    criteria.expression.
**
****************************************************************************/
static int readFromFile (
	IMS_ASK_REQUEST *request,
	char *fileName)
{
	IMS_MSG_STRUCT *msgDesc;
	int byteCount, fd;
	char buffer[IMS_COL1024_LEN + 1];
	char *cPtr;

	/* Initialize criteria.keBuffer pointer. */
	msgDesc = request->msgDesc;
	request->expression = (char *)strdup ("");

	if (strlen(ims_truncStr (fileName)) == 0)
	{
		return (IMS_OK);
	}
	else
	{
		if ((fd = open (fileName, O_RDONLY, 00400)) == -1)
		{
			ims_msg (msgDesc, IMS_ERROR,
				"Could not open to read file '%s'.", fileName);
			return (IMS_ERROR);
		}

		byteCount = 1;
		while (byteCount > 0)
		{
			if ((byteCount = read (fd, buffer, IMS_COL1024_LEN)) < 0)
			{
				/* read error */
				ims_msg (msgDesc, IMS_FATAL,
					"Error reading file '%s'", fileName);
				(void) close (fd);
				return (IMS_FATAL);
			}
			buffer[byteCount] = '\0';
			if ( byteCount > 0)
			{
				cPtr = (char *) realloc (request->expression, 
					strlen(request->expression) + byteCount + 1);
				if (cPtr == (char *)NULL)
				{
					ims_msg (msgDesc, IMS_FATAL,
						"Memory allocation for criteria expression failed.");
					return (IMS_FATAL);		
				}
				request->expression = cPtr;
				(void) strcat (request->expression, buffer);
			}
		}
		(void) close (fd);
		return (IMS_OK);
	}
}

