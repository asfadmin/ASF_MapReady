static char *sccs = "@(#)ims_mrd.c	5.2  04/15/96";
/******************************************************************************
**
** File:        ims_mrd.c
**
** Function:    Create the repository directories on the current node 
**              based on policy information in the catalog.
**
** Author:      K.Tembekjian
**
** Date:        5/29/90
**
** Modified:    11/8/93 - S. Hardman - V20.0
**              Porting activities for the HP.
**              Commented out a call in function pathXpand().
**              Added the include files sys/unistd.h, errno.h and stdlib.h.
**              Replaced calls to getwd() with getcwd(). Replaced a call
**              to cdb_ctgDescInit() with cdb_qiDescInit(). Replaced a call
**              to cdb_ctgNextRow() with cdb_qiNextRow(). Replaced the
**              include files sybfront.h and sybdb.h with ims_dbms.h.
**              Replaced the use of sys_errlist[] with strerror().
**              Replaced a call to gethostname() with uname().
**
**              5/3/95 - D. Crichton - R1B
**              ASF adaptation of code inherited from AMMOS-CDB.
**              Modified to support ASF.  Generate list of repository dirs
**              based on catalog policy tables.  Updated message handling to
**              ASF standards.
**
**              8/11/95 - S. Hardman - R1B
**              Revised argument processing.
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/unistd.h>
#include <sys/wait.h>
#include <signal.h>
#include <syslog.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_cmd.h>
#include <ims_qi.h>
#include <ims_util.h>
#include <ims_getInput.h>
#include <ims_signal.h>
#include <ims_version.h>

/*
** Define structure to hold repository list.
*/
typedef struct repList  
{
	char path[IMS_PATH_LEN+1];
	struct repList *next;
} REP_LIST;

/*
** Local Functions
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *);
REP_LIST *getRepDirs (IMS_MSG_STRUCT *);
int mkRepDirs (IMS_MSG_STRUCT *, REP_LIST *, int);
char *getnode (char [], int *);
int pathXpand (char *, char *);
int popen_rw (char *, FILE *[]);
int pclose_rw (FILE *[]);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *username;
	char *password;
	char *platform;
	char *sensor;
	char *dataset;
	char *mask;
	char *repCheck;
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
	{"-U",           &commands.username},
	{"+username",    &commands.username},
	{"-P",           &commands.password},
	{"+password",    &commands.password},
	{"-L",           &commands.platform},
	{"+platform",    &commands.platform},
	{"-S",           &commands.sensor},
	{"+sensor",      &commands.sensor},
	{"-D",           &commands.dataset},
	{"+dataset",     &commands.dataset},
	{"-M",           &commands.mask},
	{"+mask",        &commands.mask},
	{"-R",           &commands.repCheck},
	{"+repCheck",    &commands.repCheck},
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
	{"platform",    &commands.platform},
	{"sensor",      &commands.sensor},
	{"dataset",     &commands.dataset},
	{"mask",        &commands.mask},
	{"server",      &commands.server},
	{"database",    &commands.database}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
char xpath[MAXPATHLEN];	/* the expanded path array */
static char *programName;
static char hostName[IMS_HOST_LEN+1];
static char *username;
static char *password;
static int repCheck;
static char *platform = (char *) NULL;
static char *sensor = (char *) NULL;
static char *dataset = (char *) NULL;
static char *server = (char *) NULL;
static char *database = (char *) NULL;

/******************************************************************************
**
** main ()
**
** The main module of the ims_mkRepDirs. It accepts user parameters, turns on
** the error reporting routines, retrieves the repository directory list and
** calls the routine that makes them all.
**
******************************************************************************/

void main (
	int argc,
	char *argv[])
{
	IMS_MSG_STRUCT *msgDesc;
	REP_LIST *dp;
	struct utsname uname_info;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	int status;

	/*
	** Get the program name and host name.
	*/
	programName = ims_extractFileName (argv[0]);
	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';  /* Just in case. */

	/*
	** Allocate the Message Facility structure.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr,
			"Memory allocation for IMS_MSG_STRUCT structure failed.");
		exit (1);
	}
														 
	/*
	** Initialize the Message Facility options.
	*/
	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgProgramName (msgDesc, programName);
	(void) sprintf (banner, "%s::%s", hostName, programName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);
	
	/*
	** Initialize the signal handler.
	*/
	if (ims_setWrapup (runDown) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Initialization of the signal handler failed. %s.",
			strerror (errno));
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

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
	** If there is a command-file present, then get any commands from
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
	** See if we are only supposed to check.
	*/
	if (commands.repCheck != (char *) NULL)
	{
		repCheck = IMS_TRUE;
	}
	else
	{
		repCheck = IMS_FALSE;
	}
	
	/*
	** See if we are supposed to use the users mask.
	*/
	if (commands.mask == (char *) NULL)
	{
		/*
		** Set the umask to create dirs with user write and group
		** read permissions only.
		*/
		(void) umask (027);
	}				  
	
	/*
	** Get the repository directory paths.
	*/
	if ((dp = getRepDirs (msgDesc)) == (REP_LIST *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain repository directory paths.");
		goto ERROR;
	}
	
	/*
	** Create the repository directories.
	*/
	if ((status = mkRepDirs (msgDesc, dp, repCheck)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not create the repository directories.");
		goto ERROR;
	}

	(void) ims_msg (msgDesc, IMS_INFO,
		"Repository directory creation completed successfully.");

	/*
	** Shutdown the message facility.
	*/
	(void) ims_msgStructFree (msgDesc);
	exit (0);

ERROR:
	(void) ims_msg (msgDesc, IMS_ERROR,
		"An error occurred during creation of the repository directories.");

	/*
	** Shutdown the message facility.
	*/
	(void) ims_msgStructFree (msgDesc);
	exit (1);
}

/******************************************************************************
**
** runDown ()
**
** Cleanup and exit from program.
**
******************************************************************************/

static int runDown (
	int sig)
{
	/* Print out the signal caught. */
	(void) fprintf (stderr, "\n\nTermination of %s due to signal: %s (%d)\n\n",
		programName, ims_sigMsg (sig), sig);

	return (sig);
}

/******************************************************************************
**
** usage ()
**
** Print command line argument switches.
**
******************************************************************************/

static void usage (void)
{
	int i;

	(void) fprintf (stderr,
		"\n%s command-line arguments:\n\n", programName);

	for (i = 0; i < cmdLineElmCount; i++)
	{
		(void) fprintf (stderr, "%s\n", cmdLineElm[i].paramKeyword);
	}

	(void) fprintf (stderr, "\n\n");

	return;
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
	int status;
	size_t argLength;

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/

	/* username */
	if (commands.username != (char *) NULL)
	{
		username = commands.username;
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

		username = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (username, inputBuffer);
	}

	/* password */
	if (commands.password != (char *) NULL)
	{
		password = commands.password;
	}
	else
	{
		if (ims_getPassword (inputBuffer) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		password = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (password, inputBuffer);
	}

	/* platform */
	if (commands.platform != (char *) NULL)
	{
		/* Allow for a null entry. */
		if ((argLength = strlen (commands.platform)) != 0)
		{
			platform = commands.platform;
		}
	}
	else
	{
		if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Platform: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		/* Allow for a null entry. */
		if ((argLength = strlen (inputBuffer)) != 0)
		{
			platform = malloc (argLength + 1);
			(void) strcpy (platform, inputBuffer);
		}
	}

	/* sensor */
	if (commands.sensor != (char *) NULL)
	{
		/* Allow for a null entry. */
		if ((argLength = strlen (commands.sensor)) != 0)
		{
			sensor = commands.sensor;
		}
	}
	else
	{
		if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Sensor: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		/* Allow for a null entry. */
		if ((argLength = strlen (inputBuffer)) != 0)
		{
			sensor = malloc (argLength + 1);
			(void) strcpy (sensor, inputBuffer);
		}
	}

	/* dataset */
	if (commands.dataset != (char *) NULL)
	{
		/* Allow for a null entry. */
		if ((argLength = strlen (commands.dataset)) != 0)
		{
			dataset = commands.dataset;
		}
	}
	else
	{
		if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Dataset: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		/* Allow for a null entry. */
		if ((argLength = strlen (inputBuffer)) != 0)
		{
			dataset = malloc (argLength + 1);
			(void) strcpy (dataset, inputBuffer);
		}
	}

	/* server */
	if (commands.server != (char *) NULL)
	{
		server = commands.server;
	}

	/* database */
	if (commands.database != (char *) NULL)
	{
		database = commands.database;
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getRepDirs ()
**
** Login to catalog database and retrieve into a linked list the repository
** directories for the current node where we are running from now.
**
** Supply user, password character arrays.
** Return pointer to the head of the linked list, NULL if no dirs were found.
**
******************************************************************************/

REP_LIST *getRepDirs (
	IMS_MSG_STRUCT *msgDesc)
{
	IMS_QI_DESC_OBJ *qDesc;
	REP_LIST *head;
	REP_LIST *curr;
	REP_LIST *elem;
	REP_LIST *last;
	char cmdBuf[1024];
	int	count;
	int status;
	int retcode;
	
	/*
	** Initialize the list pointers.
	*/
	head = curr = elem = last = (REP_LIST *) NULL;

	/*
	** Allocate a query descriptor.
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		return ((REP_LIST *) NULL);
	}
	
	/*
	** Setup the descriptor with necessary information about this
	** process.
	*/
	IMS_SETUSER (qDesc, username);
	
	IMS_SETPSWD (qDesc, password);
	
	IMS_SETPROG (qDesc, programName);

	if (server != (char *) NULL)
	{
		IMS_SETSERVER (qDesc, server);
	}

	if (database != (char *) NULL)
	{
		IMS_SETDBNAME (qDesc, database);
	}

	IMS_SET_VERBOSE (qDesc, 10);

	IMS_SETCMD (qDesc, cmdBuf);

	/*
	** Login to the catalog database.
	*/
	if ((status = ims_qiLogin (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"Could not login to the database server.");
		(void) ims_qiFreeDesc (qDesc);
		return ((REP_LIST *) NULL);
	}

	/*
	** Associate the message descriptor with the dbproc so
	** the Sybase error and message handling can be performed.
	*/
	IMS_SET_USERDATA(qDesc);

	/* 
	** Build the query in the command buffer.
	** We need two separate queries because the sensor value in
	** the catalog can be NULL.  
	*/
	if (sensor != (char *) NULL)
	{
		(void) sprintf (cmdBuf, 
			"select distinct path \
			from dataset_path_policy p, dataset_relation r, dataset_policy d \
			where p.dataset_idx = r.dataset_idx \
			and platform like '%s' \
			and sensor like '%s' \
			and dataset like '%s' \
			and host = '%s' \
			order by path asc", 
			platform ? platform : "%", 
			sensor, 
			dataset ? dataset : "%",
			hostName);
	}
	else
	{
		(void) sprintf (cmdBuf, 
			"select distinct path \
			from dataset_path_policy p, dataset_relation r, dataset_policy d \
			where p.dataset_idx = r.dataset_idx \
			and platform like '%s' \
			and dataset like '%s' \
			and host = '%s' \
			order by path asc", 
			platform ? platform : "%", 
			dataset ? dataset : "%",
			hostName);
	}
	
	/*
	** Process the result rows for this query.
	*/
	for (count = 0; (retcode = ims_qiNextRow (qDesc)) == IMS_ROWRETURNED; count++)
	{
		/*
		** Allocate space for the REP_LIST structure.
		*/
		if ((elem = (REP_LIST *) malloc ((size_t) sizeof (REP_LIST))) ==
			(REP_LIST *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not allocate memory for REP_LIST structure.");
			head = (REP_LIST *) NULL;
			while (ims_qiNextRow (qDesc) != IMS_ENDOFTRANSACTION) {}
			goto ERROR; 
		}
		
		/*
		** head points to the first element of the list.
		*/
		if (head == (REP_LIST *) NULL)
		{
			head = elem;
		}
		else
		{
			curr->next = elem;
		}
		
		elem->next = (REP_LIST *) NULL;
		
		/*
		** copy the repository dir name 
		*/
		(void) memcpy (elem->path, IMS_VALUE (qDesc,0),
			IMS_VALUELENGTH(qDesc,0));
		elem->path[IMS_VALUELENGTH(qDesc,0)] = '\0';	

		/*
		** If the last element is the same, then don't add in the
		** new element.
		*/

		if (last)
		{
			if (strcmp(last->path, elem->path) == 0)
			{
				/* Elements are identical, so remove new node. */
				free (elem);
				last->next = (REP_LIST *) NULL;
				elem = last;
			}
		}

		/*
		** make this the current element 
		*/
		curr = elem;
		last = elem;
	}
	
	/*
	** If the row supply stopped because of some kind of error. 
	*/
	if (retcode < IMS_OK)
	{
		head = (REP_LIST *) NULL;
	}
	else if (count == 0) /* else if row count is zero. */
	{
		(void) ims_msg (msgDesc, retcode,
			"No repository directories found for '%s, %s, %s'.",
			platform?platform:"<ALL>",
			sensor?sensor:"<ALL>",
			dataset?dataset:"<ALL>");
		head = (REP_LIST *) NULL;
	}

	(void) ims_qiFreeDesc(qDesc);
	return (head);

ERROR:
	(void) ims_qiFreeDesc(qDesc);
	return (head);
}

/******************************************************************************
**
** mkRepDirs ()
**
** Given a list of pathnames, check and create each one of the nodes.
** if report flag is set, then only report about existing or missing
** directories.
**
** Return IMS_OK if all directory creations went ok else return the
** error / warning (IMS_WARNING / IMS_ERROR) that occured.
**
******************************************************************************/

int mkRepDirs (
	IMS_MSG_STRUCT *msgDesc,
	REP_LIST *head,
	int repCheck)
{
	REP_LIST	*elem;	
	struct stat ds;	
	char *sp;                /* pointer to '/' in a node */
	char *node;              /* hold a node of the current path */
	char orgwd[MAXPATHLEN];  /* the original working dir */
	char wd[MAXPATHLEN];     /* current working directory */
	int cnt;                 /* node count */
	int pos;                 /* current pathname pos to get a node from */
	int status = IMS_OK;
	
	/*
	** Get the current working directory where we will return.
	*/
	if (! getcwd (orgwd, MAXPATHLEN-1))
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain the current working directory. %s",
			strerror (errno));
		return (IMS_ERROR);
	}
	
	/*
	** for each of the repository directories in the list 
	*/
	for (elem = head; elem; elem = elem->next)
	{
		/*
		** first, expand the pathname and if it returns error 
		*/
		if (pathXpand (elem->path, xpath))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not expand the directory '%s' (%s)",
				elem->path, xpath);
			
			/* Skip to next repdir. */ 
			continue;
		}
		
		/*
		** if we can't change to original working directory 
		*/
		if (chdir (orgwd))
		{
			/*
			** report the error, then return 
			*/
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Could not change directory on node '%s' in working directory '%s' for directory '%s' (%s). %s",
				node, getcwd(wd, MAXPATHLEN-1),
				elem->path, xpath, strerror (errno));
			return (IMS_ERROR);
		}
		
		/*
		** if this repository directory pathname exists 
		*/
		if (! stat (xpath, &ds))
		{
			/*
			** if it is of directory type 
			*/
			if (S_ISDIR(ds.st_mode)) /* POSIX uses S_ISDIR */
			{
				/*
				** report its existence 
				*/
				(void) ims_msg (msgDesc, IMS_INFO,
					"Directory already exists '%s' (%s)",
					elem->path, xpath);
			}
			else /* It's other than a directory type. */
			{
				/*
				** report the conflict 
				*/
				(void) ims_msg (msgDesc, IMS_INFO,
					"A non-directory file already exists '%s' (%s)",
					elem->path, xpath);
			}
		}
		/*
		** else: if the stat error on this repdir path is any other 
		** than 'non-existent' or 'non-dir component' 
		*/
		else if (errno != ENOENT && errno != ENOTDIR)
		{
			/*
			** report the possible system error 
			*/
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Error occurred accessing directory %s (%s). %s",
				elem->path, xpath, strerror (errno));
			status = IMS_ERROR;
		}
		/*
		** else: the error from stat was one of 'non-existent' 
		** or 'non-directory node' 
		*/
		else
		{
			/*
			** for all nodes in this path, search the culprit 
			*/
			for (cnt=0, pos=0; (node = getnode (xpath, &pos)); cnt++)
			{
				/*
				** if the node has a slash and 
				** it isn't the root node 
				*/
				if ((sp = strchr (node, '/')) && !(*node == '/' && cnt == 0))
				{
					/*
					** remove it (to accomodate mkdir(2) 
					*/
					*sp = '\0';
				}
				
				/*
				** if the stat on the node does not succeed 
				*/
				if (stat (node, &ds))
				{
					/*
					** if error is other 
					** than 'non-existent node' 
					*/
					if (errno != ENOENT)
					{
						(void) ims_msg (msgDesc, IMS_ERROR,
							"Could not stat directory on node '%s' in working directory '%s' for directory '%s' (%s). %s",
								node, getcwd (wd, MAXPATHLEN-1),
								elem->path, xpath, strerror (errno));
						status=IMS_ERROR;
						
						/* Skip to next rep dir. */
						break;
					}
					else  /* Error was 'non-existant node. */
					{
						/*
						** If report only is requested.
						*/
						if (repCheck == IMS_TRUE)
						{
							(void) ims_msg (msgDesc, IMS_INFO,
								"Directory does not exist '%s' (%s)",
								elem->path, xpath);
							
							/* Skip to next repdir. */ 
							break;
						}
						else  /* Creation is requested. */
						{
							/*
							** Create the node.
							*/
							if (mkdir (node, 0777))
							{
								(void) ims_msg (msgDesc, IMS_ERROR,
									"Could not make directory on node '%s' in working directory '%s' for directory '%s' (%s). %s",
									node, getcwd (wd, MAXPATHLEN-1),
									elem->path, xpath, strerror (errno));
								status=IMS_ERROR;
								
								/* Skip to next rep dir. */
								break;
							}
						}
					}
				}
				
				/*
				** node existed. if change of working directory
				** to the new one fails 
				*/
				if (chdir (node))
				{
					(void) ims_msg (msgDesc, IMS_ERROR,
						"Could not change directory on node '%s' in working directory '%s' for directory '%s' (%s). %s",
						node, getcwd(wd, MAXPATHLEN-1),
						elem->path, xpath, strerror (errno));
					status = IMS_ERROR;
					
					/* Skip to next directory. */
					break;
				}
			}
			
			/*
			** If we did traverse all of this path.
			*/
			if (node == NULL)
			{
				(void) ims_msg (msgDesc, IMS_INFO,
					"Created repository directory '%s' (%s)",
					elem->path, xpath);
			}
		}
	}
	
	return (status);
}

/******************************************************************************
**
** getnode()
** 
** This function returns a node from the current position in given pathname.
**
** Arguments:	path - null terminated string containing pathname of the form:
**			[/][node-1[...[/node-n]]][/]
**		pos - position to get nodes from.
**
** Returns:	NULL - if no more nodes to get.
**		Character pointer to the node.
**
******************************************************************************/

char *getnode (char path[], int *pos)
{
	static char
		node[MAXPATHLEN];
	int
		len,
		cnt,
		old;
	
	/*
	** from each given starting position in pathname 
	*/
	for (old = *pos, len = strlen (path); *pos <= len; (*pos)++)
	{
		/*
		** if character at current position is '/' or NULL 
		*/
		if (path[*pos] == '/' || path[*pos] == '\0')
		{
			/*
			** if we traversed enough to return a node 
			*/
			if ((cnt = ++(*pos) - old))
			{
				/*
				** copy it from old pathname position 
				*/
				(void) strncpy (node, &path[old], cnt);
				
				/*
				** null terminate it 
				*/
				node[cnt] = '\0';
				
				/*
				** return address of the node 
				*/
				return (node);
			}
		}
	}
	
	/*
	** we have exhausted all the nodes. return NULL 
	*/
	return ((char *) NULL);
}

/******************************************************************************
**
** pathXpand ()
**
** Take pathname consisting of nodes and environmental variables and replace
** environmental variables with their values returning a new, expanded path
** by calling csh(1) via popen_rw()
**
** Arguments:	path - character array pathname which is to be expanded
**		xpath - character array pathname to contain the expanded
**			pathname
**
** Returns:	0 successful, xpath contains the expanded pathname
** 		!0 failure, xpath contains an error message
**
** 11/8/93 - Commented out a call to strcpy() that uses a void expression
**	as an argument.
**
******************************************************************************/

#define POPEN_CMD "echo -n %s"

static char cmd[MAXPATHLEN + sizeof(POPEN_CMD) ]; /* cmd buffer */

int pathXpand (char *path, char *xpath)
{
	int sts;
	FILE *fp[2];
	
	/*
	** prepare the csh(1) command to expand the pathname 
	*/
	(void) sprintf (cmd, POPEN_CMD, path);

	/*
	** if popen_rw to execute the command via csh is unsuccessful
	*/
	if (popen_rw (cmd, fp) < 0 )
	{
		/*
		** set the error message 
		**
		** The following line of code causes an error in acc as it
		** rightly should.  We are using a void expression as an
		** argument (perror(0)). perror()'s purpose is to print
		** additional information to stderr and not to return a
		** pointer to a string.
		*/
		/* (void) strcpy (xpath, perror(0)); */

		/*
		** return an error indication 
		*/
		return (-1);
	}
	
	/*
	** get the expanded pathname or csh's output 
	*/
	(void) fgets (xpath, MAXPATHLEN, fp[0]);
	
	/*
	** return the ending status 
	*/
	return (pclose_rw (fp));
}

/******************************************************************************
**
** popen_rw ()
**
** This function serves the same purpose as popen(2). The difference is that
** it opens both read and write file pointers to communicate with the invoked
** command and it passes the command to csh(1) instead of sh(1).
**
******************************************************************************/

#define COMMANDELEM 3	/* Element in csh command arguments where the user
			** command will be placed. */
#define MAXPOPENRW 20	/* Maximum number of popens which can be issued. */

/*
** Declare the csh command argument list
*/
static char *av[] = {
	"csh", "-f", "-c", "the command goes here", (char *)0
};

/*
** Declare a place to hold pids of up to 20 csh children
*/
static int pidstatus[ MAXPOPENRW ]; 
static int pidlist[ MAXPOPENRW ];

int popen_rw (char *cmd, FILE *fp[])
{
	int     
		p1[2], 
		p2[2],
		mywrite, 
		hisread, 
		hiswrite, 
		myread;

	pid_t	pid;
	
	/*
	** place the command in argument list to csh 
	*/
	av[COMMANDELEM] = cmd;
	
	/*
	** if the creation of the two pipes (one for each direction) fails 
	*/
	if (pipe (p1) < 0 || pipe (p2) < 0)
	{
		/*
		** return an error 
		*/
		return (-1);
	}

	/*
	** give the pipes better names. (my) is in the context of the parent
	** program 
	*/
	mywrite = p1[1];
	hisread = p1[0];
	
	hiswrite = p2[1];
	myread = p2[0];
	
	/*
	** fork off the child 
	*/
	if ((pid = fork()) == 0) 
	{
		/*
		** close the parent's descriptors 
		*/
		(void) close (mywrite);
		(void) close (myread);
		
		/*
		** duplicate child's descriptor as 0,1,2 
		*/
		(void) dup2 (hisread, 0);
		(void) dup2 (hiswrite, 1);
		(void) dup2 (hiswrite, 2);
		
		/*
		** close the child's descriptors now that we have 0,1,2
		** redefined 
		*/
		(void) close (hiswrite);
		(void) close (hisread);
		
		/*
		** exec the csh(1) 
		*/
		(void) execvp (*av, av);
		
		/*
		** if we get here something verry wrong went on 
		*/
		(void) fprintf (stderr, "execvp error for %s\n", *av);
		_exit(1);
	}

	/*
	** if fork() didn't succeed 
	*/
	if (pid == -1) 
	{
		/*
		** close all the pipes 
		*/
		(void) close (mywrite);
		(void) close (hisread);
		(void) close (hiswrite);
		(void) close (myread);
		
		/*
		** return error 
		*/
		return (-1);
	}
	
	/*
	** we are the parent and forked off a child successfully. now save
	** its pid 
	*/
	pidlist[myread] = pid;
	pidstatus[myread] = 0;
	
	/*
	** close the child's descriptors. now they are of no use to us. 
	*/
	(void) close (hiswrite);
	(void) close (hisread);
	
	/*
	** get the stream equivalents to our descriptors 
	*/
	fp[0] = (FILE *) fdopen (myread, "r");
	fp[1] = (FILE *) fdopen (mywrite, "w");
	
	/*
	** return successfully 
	*/
	return (0);
}

/******************************************************************************
**
** pclose_rw ()
**
** This function closes the file pointers opened by popen_rw() and returns 
** the status of the executed command.
**
** IMPORTANT NOTE: pclose_rw was originally written to accomodate path
** expantion via csh(1). The way it waits for completion of the child 
** process could slow down any application who has more than one popen_rw()
** executed and wishes to pclose_rw() one set of file pointers. As pclose_rw()
** executes a blocking wait, it may wait on a child which is taking very long
** time to execute, when the status for the current call is ready.
**
** To avoid this kind of situation modify the code to use wait3.
**
******************************************************************************/

int pclose_rw (FILE *ptr[])
{
    	int	f,
	    	r, 
	    	status;

	void	(*hstat)(),
                (*istat)(),
                (*qstat)();


    	/*
    	** get the descriptor for the file pointer 
    	*/
    	f = fileno (ptr[0]);

    	/*
    	** close the file pointers 
    	*/
    	fclose (ptr[0]);
    	fclose (ptr[1]);

    	/*
    	** ignore interrupt quit and hangup signals while we are waitin for our
    	** child 
    	*/
    	istat = signal (SIGINT, SIG_IGN);
    	qstat = signal (SIGQUIT, SIG_IGN);
    	hstat = signal (SIGHUP, SIG_IGN);
  
    	/*
    	** wait for the termination of child with pid  
    	*/
    	while ((r = wait (&status)) != -1)
    	{
	    	int fc;
	    	for (fc = 0; fc < MAXPOPENRW; fc++)
	    	{
		    	if (pidlist[fc] == r)
		    	{
			   	pidstatus[fc] = status;
			    	break;
		    	}
	    	}
    	}
    
    	/*
    	** reinstate the previous signal handlers 
    	*/
    	signal (SIGINT, istat);
    	signal (SIGQUIT, qstat);
    	signal (SIGHUP, hstat);
    
    	/*
    	** return the child's status 
    	*/
    	return (pidstatus[f]);
}
