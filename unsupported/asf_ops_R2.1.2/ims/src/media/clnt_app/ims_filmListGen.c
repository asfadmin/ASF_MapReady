static char *sccs = "@(#)ims_filmListGen.c	5.1  03/17/96";
/******************************************************************************
**
** File:        ims_filmListGen.c
**
** Function:    An client program that uses the FPS TTDL
**              generation routine.
**
** Author:      Sean Hardman and David Pass
**
** Date:        7/11/95
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>
#include <sys/utsname.h>
#include <syslog.h>

#include <ims_dbms.h>
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_cmd.h>
#include <ims_media.h>
#include <ims_util.h>
#include <ims_getInput.h>
#include <ims_version.h>
#include <ims_signal.h>

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *);
static void freeItemList (FILM_REQUEST_LIST *);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *username;
	char *password;
	char *itemCount;
	char *itemList;
	char *commandFile;
	char *server;
	char *database;
	char *help;
	char *release;
	char *ord_items;
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
	{"-I",           &commands.itemCount},
	{"+itemCount",   &commands.itemCount},
	{"-L",           &commands.itemList},
	{"+itemList",    &commands.itemList},
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

static IMS_CMD_CLASS cmdFileElm [] =
{
	{"username",    &commands.username},
	{"password",    &commands.password},
	{"itemCount",   &commands.itemCount},
	{"itemList",    &commands.itemList},
	{"server",      &commands.server},
	{"database",    &commands.database}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static char *glb_programName;
static int itemCount;
static FILM_REQUEST_LIST *itemList = (FILM_REQUEST_LIST *) NULL;
static MEDIA_USER_SPEC userSpec;

/******************************************************************************
**
** main ()
**
** This is the driver for the Things-to-do-list (TTDL)
** for FPS.  It calls ims_filmList() to process the input.
**
******************************************************************************/

void main (
	int argc,
	char *argv[])
{
	IMS_MSG_STRUCT *msgDesc;
	int status;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */

	/*
	** Get the program name and the node name.
	*/
	glb_programName = ims_extractFileName (argv[0]);
	userSpec.program = glb_programName;
	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';  /* Just in case. */

	/*
	** Allocate message facility structure.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr,
			"Memory allocation for IMS_MSG_STRUCT structure failed.");
		exit (1);
	}

	/*
	** Initialize the message facility options.
	*/
	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgProgramName (msgDesc, glb_programName);
	(void) sprintf (banner, "%s::%s", hostName, glb_programName);
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
		goto ERROR;
	}

	/*
	** Get the command line arguments. The variable status will actually
	** contain the number of command line arguments processed upon
	** successful completion.
	*/
	if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
		cmdLineElmCount, msgDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
		  "An error occurred parsing the command-line.");
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
		(void) ims_msgStructFree (msgDesc);
		exit (0);
	}

	/*
	** If there is a command file present, then get any commands from
	** this file, then overlay all commands from the commandline, except
	** password, which will be gone by this point.
	*/
	if (commands.commandFile != (char *) NULL)
	{
		if ((status = ims_getFileParms (commands.commandFile,
			cmdFileElm, cmdFileElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"An error occurred parsing the command-file.");
			goto ERROR;
		}

		/*
		** Now, get command line arguments again to overlay file args.
		*/
		if ((status = ims_getCmdLine (argc, argv,
			cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"An error occurred parsing the command-line.");
			goto ERROR;
		}
	}

	/*
	** Process the information from command-line and/or command-file.
	*/
	if ((status = getArgInput (msgDesc)) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** Generate FPS TTDL file.
	*/
	if ((status = ims_filmList (msgDesc, (char *) &userSpec,
		itemList )) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** Update the film item status to IN-*.
	*/
	if ((status = ims_filmUpdate (msgDesc, (char *) &userSpec,
		itemList )) < IMS_OK)
	{
		goto ERROR;
	}

	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_INFO,
		"The FPS TTDL file was successfully generated.");
	(void) ims_msgStructFree (msgDesc);
	freeItemList (itemList);

	exit (0);

ERROR:
	/*
	** Shutdown the message facility.
	*/
	(void) ims_msg (msgDesc, IMS_ERROR,
		"The FPS TTDL file generation failed.");
	(void) ims_msgStructFree (msgDesc);
	freeItemList (itemList);

	exit (1);
}   /*  main   */

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
	(void) fprintf (stderr,
		"\n\nTermination of %s due to signal: %s (%d)\n\n",
		glb_programName, ims_sigMsg (sig), sig);

	return (sig);
}   /*  runDown  */


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
		"\n%s command-line arguments:\n\n", glb_programName);

	for (i = 0; i < cmdLineElmCount; i++)
	{
		(void) fprintf (stderr, "%s\n", cmdLineElm[i].paramKeyword);
	}

	(void) fprintf (stderr, "\n\n");
}   /*  usage  */


/******************************************************************************
**
** getArgInput ()
**
** Process command-line and command-file arguments.
**
******************************************************************************/

static int getArgInput (
	IMS_MSG_STRUCT *msgDesc)
{
	FILM_REQUEST_LIST *currPtr;
	FILM_REQUEST_LIST *prevPtr;
	char inputBuffer[IMS_INPUT_BUF_LEN+1];
	char prompt[20];
	int orderId;
	int itemId;
	int filmTarget;
	int number;
	int invalid;
	int index;
	int listFlag;
	char str[256], str2[256];
	int firstItem;
	int secondItem;
	int tempItem;
	int listCount;
	int i,j;

	/*
	** Initialize variables.
	*/
	prevPtr = (FILM_REQUEST_LIST *) NULL;

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/

	/* username */
	if (commands.username != (char *) NULL)
	{
		userSpec.username = commands.username;
	}
	else
	{
		if (ims_getString (IMS_FALSE, inputBuffer, IMS_INPUT_BUF_LEN,
			"Username: ") == (char *) NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		userSpec.username = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (userSpec.username, inputBuffer);
	}

	/* password */
	if (commands.password != (char *) NULL)
	{
		userSpec.password = commands.password;
	}
	else
	{
		if (ims_getPassword (inputBuffer) == NULL)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Error detected while reading input string.");
			return (IMS_FATAL);
		}

		userSpec.password = malloc (strlen (inputBuffer) + 1);
		(void) strcpy (userSpec.password, inputBuffer);
	}

	/* itemCount */
	number = 0;
	if (commands.itemCount == (char *) NULL)
	{
		/* We expect a number. */
		do
		{
			if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
				"Item Count: ") == (char *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}

			invalid = 0;
			for (i=0; inputBuffer[i]; i++)
			{
				if ((i == 0) && ((inputBuffer[i] == '-') ||
					(inputBuffer[i] == '+')))
				{
					continue;
				}
			
				if (! isdigit (inputBuffer[i]))
				{
					(void) printf (
						"Expecting numerical input. Try again.\n");
					invalid = 1;
					break;
				}
			}

			number = (int) atoi (inputBuffer);
			if ((invalid == 0 ) && ((int) strlen (inputBuffer) > 0) &&
				((number < 1) || (number > IMS_MAX_SINT)))
			{
				invalid = 1;
				(void) printf (
					"Numerical input is not in the range of '1' to '%d'. Try again.\n",
					IMS_MAX_SINT);
			}
		}while (invalid);

		itemCount = number;
	}
	else if (ims_isInteger (commands.itemCount) == IMS_TRUE)
	{
		number = (int) atoi (commands.itemCount);
		if ((number < 1) || (number > IMS_MAX_SINT))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The parameter 'itemCount' has a value of '%d', which is not in the range of '1' to '%d'.",
				number, IMS_MAX_INT);
			return (IMS_ERROR);
		}
		else
		{
			itemCount = number;
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The parameter 'itemCount' must contain a valid integer value.");
		return (IMS_ERROR);
	}

	/* itemList */
	if (commands.itemList == (char *) NULL)
	{
		for (j = 0; j < itemCount; j++)
		{
			/* We expect a number for Order Id. */
			do
			{
				(void) sprintf (prompt, "Order Id (%d): ", j+1);
	
				if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
					prompt) == (char *) NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"Error detected while reading input string.");
					return (IMS_FATAL);
				}
	
				invalid = 0;
				for (i=0; inputBuffer[i]; i++)
				{
					if ((i == 0) && ((inputBuffer[i] == '-') ||
						(inputBuffer[i] == '+')))
					{
						continue;
					}
			
					if (! isdigit (inputBuffer[i]))
					{
						(void) printf (
							"Expecting numerical input. Try again.\n");
						invalid = 1;
						break;
					}
				}

				orderId = (int) atoi (inputBuffer);
				if ((invalid == 0 ) && ((int) strlen (inputBuffer) > 0) &&
					((orderId < NULL_ORDER) || (orderId > IMS_MAX_INT)))
				{
					invalid = 1;
					(void) printf (
						"Numerical input is not in the range of '%d' to '%d'. Try again.\n",
						NULL_ORDER, IMS_MAX_INT);
				}
			}while (invalid);
	
			/* We expect a number for itemId. */
			do
			{
				(void) sprintf (prompt, "Item Id (%d): ", j+1);
	
				if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
					prompt) == (char *) NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"Error detected while reading input string.");
					return (IMS_FATAL);
				}
	
				invalid = 0;
				for (i=0; inputBuffer[i]; i++)
				{
					if ((i == 0) && ((inputBuffer[i] == '-') ||
						(inputBuffer[i] == '+')))
					{
						continue;
					}
				
					if (! isdigit (inputBuffer[i]))
					{
						(void) printf (
							"Expecting numerical input. Try again.\n");
						invalid = 1;
						break;
					}
				}
	
				itemId = (int) atoi (inputBuffer);
				if ((invalid == 0 ) && ((int) strlen (inputBuffer) > 0) &&
					((itemId < NULL_ORDER) || (itemId > IMS_MAX_SINT)))
				{
					invalid = 1;
					(void) printf (
						"Numerical input is not in the range of '%d' to '%d'. Try again.\n",
						NULL_ORDER, IMS_MAX_SINT);
				}
			}while (invalid);
	
			/* We expect a number for filmTarget. */
			do
			{
				(void) sprintf (prompt, "Film Target (%d): ", j+1);
	
				if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
					prompt) == (char *) NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"Error detected while reading input string.");
					return (IMS_FATAL);
				}
	
				invalid = 0;
				for (i=0; inputBuffer[i]; i++)
				{
					if ((i == 0) && ((inputBuffer[i] == '-') ||
						(inputBuffer[i] == '+')))
					{
						continue;
					}
				
					if (! isdigit (inputBuffer[i]))
					{
						(void) printf (
							"Expecting numerical input. Try again.\n");
						invalid = 1;
						break;
					}
				}
	
				filmTarget = (int) atoi (inputBuffer);
				if ((invalid == 0 ) && ((int) strlen (inputBuffer) > 0) &&
					((filmTarget < NULL_ORDER) || (filmTarget > IMS_MAX_SINT)))
				{
					invalid = 1;
					(void) printf (
						"Numerical input is not in the range of '%d' to '%d'. Try again.\n",
						NULL_ORDER, IMS_MAX_SINT);
				}
			}while (invalid);
	
			/*
			** Allocate memory space for the FILM_REQUEST_LIST structure.
			*/
			if ((currPtr = (FILM_REQUEST_LIST *) malloc
				((unsigned) sizeof (FILM_REQUEST_LIST))) ==
				(FILM_REQUEST_LIST *) NULL)
			{
				(void) ims_msg (msgDesc, IMS_FATAL,
					"Could not allocate memory for the FILM_REQUEST_LIST structure.");
				return (IMS_FATAL);
			}
	
			/*
			** itemList points to the first element of the list.
			*/
			if (j == 0)
			{
				itemList = currPtr;
			}
			else
			{
				prevPtr->next = currPtr;
			}
	
			currPtr->order_id = (DBINT) orderId;
			currPtr->item_id = (DBSMALLINT) itemId;
			currPtr->film_target = (DBSMALLINT) filmTarget;
			currPtr->status = FILM_LOCKED;
			currPtr->next = (FILM_REQUEST_LIST *) NULL;
	
			prevPtr = currPtr;
		}
	}
	else  /* Parse the provided itemList. */
	{
		(void) strcpy (str, ims_truncStr (ims_removeQuotes (commands.itemList)));
		listCount = 0;
		firstItem = IMS_TRUE;
		secondItem = IMS_TRUE;
		listFlag = IMS_TRUE;
		while (listFlag == IMS_TRUE)
		{
			/*
			** First read order id then item id then film target.
			*/

			/*
			** See if we are at the end of the list.
			*/
			if ((index = ims_strIndex (str, " ")) == -1)
			{
				index = (int) strlen (str);
				listFlag = IMS_FALSE;
			}

			/*
			** Save our current value.
			*/
			(void) strcpy (str2, str);
			str2[index] = '\0';

			/*
			** Position the list at the next value.
			*/
			(void) strcpy (str, ims_truncStr (str + index));

			if (ims_isInteger (str2) == IMS_TRUE)
			{
				tempItem = atoi (str2);
			}
			else
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"The parameter 'itemList' must contain valid integer values.");
				return (IMS_ERROR);
			}

			/*
			** Determine which item we have.
			*/
			if (firstItem == IMS_TRUE)
			{
				/* Save the order id. */
				orderId = tempItem;
				firstItem = IMS_FALSE;
			}
			else if (secondItem == IMS_TRUE)
			{
				/* Save the item id. */
				itemId = tempItem;
				secondItem = IMS_FALSE;
			}
			else /* We have all three items so let's store them. */
			{
				/* Save the film target. */
				filmTarget = tempItem;

				/*
				** Allocate memory space for the FILM_REQUEST_LIST structure.
				*/
				if ((currPtr = (FILM_REQUEST_LIST *) malloc
					((size_t) sizeof (FILM_REQUEST_LIST))) ==
					(FILM_REQUEST_LIST *) NULL)
				{
					(void) ims_msg (msgDesc, IMS_FATAL,
						"Could not allocate memory for the FILM_REQUEST_LIST structure.");
					return (IMS_FATAL);
				}

				/*
				** itemList points to the first element of the list.
				*/
				if (listCount == 0)
				{
					itemList = currPtr;
				}
				else
				{
					prevPtr->next = currPtr;
				}

				listCount++;
				currPtr->order_id = (DBINT) orderId;
				currPtr->item_id = (DBSMALLINT) itemId;
				currPtr->film_target = (DBSMALLINT) filmTarget;
				currPtr->status = FILM_LOCKED;
				currPtr->next = (FILM_REQUEST_LIST *) NULL;

				prevPtr = currPtr;
				firstItem = IMS_TRUE;
				secondItem = IMS_TRUE;
			}
		}

		/*
		** Check for an even number of values.
		*/
		if ((firstItem == IMS_FALSE) || (secondItem == IMS_FALSE))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The parameter 'itemList' does not contain complete sets of values.");
			return (IMS_ERROR);
		}

		/*
		** Compare itemCount with the number of sets parsed.
		*/
		if (itemCount != listCount)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The itemCount '%d' does not match the number of items in itemList '%d'.",
				itemCount, listCount);
			return (IMS_ERROR);
		}
	}

	/* server */
	if (commands.server != (char *) NULL)
	{
		userSpec.server = commands.server;
	}


	/* database */
	if (commands.database != (char *) NULL)
	{
		userSpec.database = commands.database;
	}

	return (IMS_OK);
}   /*  getArgInput */

/******************************************************************************
**
** freeItemList ()
**
** Free the FILM_REQUEST_LIST structure.
**
******************************************************************************/

static void freeItemList (
	FILM_REQUEST_LIST *currPtr)
{
	FILM_REQUEST_LIST *nextPtr;
			
	while (currPtr != (FILM_REQUEST_LIST *) NULL)
	{
		nextPtr = currPtr->next;
		free (currPtr);
		currPtr = nextPtr;
	}
												
	return;
}

