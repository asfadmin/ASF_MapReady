static char *sccs = "@(#)ims_reorder.c	1.1  04/21/97";
/******************************************************************************
**
** File:        ims_reorder.c
**
** Function:    This application will take an existing order item or
**              items in an IMS/DADS catalog database and create a new
**              order.
**
** Author:      Sean Hardman
**
** Date:        4/19/97
**
** Copyright (C) 1996, California Institute of Technology.  U.S. Government
** Sponsorship under NASA Contract NAS7-1260 is acknowledged.
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
#include <ims_qi.h>
#include <ims_cmd.h>
#include <ims_util.h>
#include <ims_getInput.h>
#include <ims_version.h>
#include <ims_signal.h>

#include <odldef.h>
#include <odlinter.h>
#include <ims_odl.h>
#include <IK_Syslog.h>
#include <ims_order.h>

/*
** Definitions.
*/
#define IMS_MAX_ITEMS   100
#define IMS_ZERO    0
#define CONTACT     1
#define SHIPPING    2
#define BILLING     3

/*
** User specification structure definition.
*/
typedef struct userSpec
{
	char *username;
	char *password;
	char *program;
	char *server;
	char *database;
} USER_SPEC;

/*
** User address structure definition.
*/
typedef struct addressSpec
{
	char first_name[21];
	char initial_name[4];
	char last_name[21];
	char title[11];
	char organization[36];
	char street[129];
	char city[36];
	char state[21];
	char country[21];
	char zipcode[11];
	char phone[26];
	char fax[26];
	char email[129];
} ADDRESS_SPEC;

/*
** Order item structure definition.
*/
typedef struct itemSpec
{
	char dataset_id[81];
	char package_id[31];
	char process_option[31];
	char media_type[31];
	char media_format[31];
} ITEM_SPEC;

/*
** Local Functions.
*/
static int runDown (int);
static void usage (void);
static int getArgInput (IMS_MSG_STRUCT *);
static int openConnection (IMS_MSG_STRUCT *);
static int getAuthInfo (IMS_MSG_STRUCT *, char *, char *);
static int getAddressInfo (IMS_MSG_STRUCT *, ADDRESS_SPEC *, int);
static int getItemInfo (IMS_MSG_STRUCT *, ITEM_SPEC *, int);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *username;
	char *password;
	char *orderId;
	char *itemCount;
	char *itemList;
	char *odlFileFlag;
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
	{"-O",           &commands.orderId},
	{"+orderId",     &commands.orderId},
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
	{"-f",           &commands.odlFileFlag},
	{"+odlFileFlag", &commands.odlFileFlag},
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
	{"orderId",     &commands.orderId},
	{"itemCount",   &commands.itemCount},
	{"itemList",    &commands.itemList},
	{"server",      &commands.server},
	{"database",    &commands.database},
	{"odlFileFlag", &commands.odlFileFlag}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Global Variables
*/
static USER_SPEC userSpec;
static char *programName;
static int orderId;
static int itemCount = 0;
static int itemList[IMS_MAX_ITEMS];
static int odlFileFlag;
static IMS_QI_DESC_OBJ *qDesc = (IMS_QI_DESC_OBJ *) NULL;
static char cmdBuf[1024];

/******************************************************************************
**
** main ()
**
******************************************************************************/

void main (
	int argc,
	char *argv[])
{
	IMS_MSG_STRUCT *msgDesc;
	AGGREGATE root;
	IMS_ODL_TREE *product_request;
	IMS_ODL_TREE *contact_address;
	IMS_ODL_TREE *shipping_address;
	IMS_ODL_TREE *billing_address;
	IMS_ODL_TREE *user_affiliation;
	IMS_ODL_TREE *line_item;
	ADDRESS_SPEC addressSpec;
	ITEM_SPEC itemSpec;
	int status;
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	struct utsname uname_info;    /* Structure for uname() */
	char authenticator[21];
	char request_id[IMS_PROGRAM_LEN+15];
	char billing_id[16];
	char fileName[IMS_PATH_LEN+1];
	int addressType;
	int newOrderId;
	int i;

	/*
	** Get the program name and the node name.
	*/
	programName = ims_extractFileName (argv[0]);
		userSpec.program = programName;
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
		(void) ims_msg (msgDesc, IMS_ERROR,
			"An error occurred parsing the command-line.");
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** Check to see if we got everything off of the command line.
	*/
	if (status < argc)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"Only %d out of the %d command line arguments were "
			"processed.", status, argc);
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
	** this file, then overlay all commands from the command-line,
	** except password, which will be gone by this point.
	*/
	if (commands.commandFile != (char *) NULL)
	{
		if ((status = ims_getFileParms (commands.commandFile,
			cmdFileElm, cmdFileElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"An error occurred parsing the command-file.");
			(void) ims_msgStructFree (msgDesc);
			exit (1);
		}

		/*
		** Now, get command line args again to overlay file arguments.
		*/
		if ((status = ims_getCmdLine (argc, argv,
			cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"An error occurred parsing the command-line.");
			(void) ims_msgStructFree (msgDesc);
			exit (1);
		}
	}

	/*
	** Process the information from the command-line and/or file.
	*/
	if ((status = getArgInput (msgDesc)) < IMS_OK)
	{
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** Open a database server connection.
	*/
	if ((status = openConnection (msgDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"An error occurred opening a database server connection.");
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** Query for authorization information.
	*/
	if ((status = getAuthInfo (msgDesc, authenticator, billing_id))
		< IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"An error occurred obtaining the authorization information.");
		goto ERROR;
	}

	/*
	** Build the PRODUCT_REQUEST group.
	*/
	(void) ims_addODLObject (msgDesc, NULL, &product_request,
		"PRODUCT_REQUEST", IMS_FALSE, IMS_ODL_GROUP);

	(void) ims_addODLKeyword (msgDesc, product_request,
		"AUTHENTICATOR", TV_STRING, authenticator);

	(void) ims_addODLKeyword (msgDesc, product_request,
		"DATA_CENTER_ID", TV_STRING, "ASF");

	(void) sprintf (request_id, "%s_%d", programName, orderId);

	(void) ims_addODLKeyword (msgDesc, product_request,
		"REQUEST_ID", TV_STRING, request_id);

	/*
	** Query for contact address information.
	*/
	(void) memset ((ADDRESS_SPEC *) &addressSpec, 0, sizeof(ADDRESS_SPEC));
	addressType = CONTACT;

	if ((status = getAddressInfo (msgDesc, &addressSpec, addressType))
		< IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"An error occurred obtaining the contact address information.");
		goto ERROR;
	}

	/*
	** Build the CONTACT_ADDRESS group.
	*/
	(void) ims_addODLObject (msgDesc, product_request, &contact_address,
		"CONTACT_ADDRESS", IMS_TRUE, IMS_ODL_GROUP);

	(void) ims_addODLKeyword (msgDesc, contact_address,
		"FIRST_NAME", TV_STRING, addressSpec.first_name);

	(void) ims_addODLKeyword (msgDesc, contact_address,
		"MIDDLE_INITIAL", TV_STRING, addressSpec.initial_name);

	(void) ims_addODLKeyword (msgDesc, contact_address,
		"LAST_NAME", TV_STRING, addressSpec.last_name);

	(void) ims_addODLKeyword (msgDesc, contact_address,
		"TITLE", TV_STRING, addressSpec.title);

	(void) ims_addODLKeyword (msgDesc, contact_address,
		"ORGANIZATION", TV_STRING, addressSpec.organization);

	(void) ims_addODLKeyword (msgDesc, contact_address,
		"ADDRESS", TV_STRING, addressSpec.street);

	(void) ims_addODLKeyword (msgDesc, contact_address,
		"CITY", TV_STRING, addressSpec.city);

	(void) ims_addODLKeyword (msgDesc, contact_address,
		"STATE", TV_STRING, addressSpec.state);

	(void) ims_addODLKeyword (msgDesc, contact_address,
		"COUNTRY", TV_STRING, addressSpec.country);

	(void) ims_addODLKeyword (msgDesc, contact_address,
		"ZIP", TV_STRING, addressSpec.zipcode);

	(void) ims_addODLKeyword (msgDesc, contact_address,
		"PHONE", TV_STRING, addressSpec.phone);

	(void) ims_addODLKeyword (msgDesc, contact_address,
		"FAX", TV_STRING, addressSpec.fax);

	(void) ims_addODLKeyword (msgDesc, contact_address,
		"EMAIL", TV_STRING, addressSpec.email);

	/*
	** Query for shipping address information.
	*/
	(void) memset ((ADDRESS_SPEC *) &addressSpec, 0, sizeof(ADDRESS_SPEC));
	addressType = SHIPPING;

	if ((status = getAddressInfo (msgDesc, &addressSpec, addressType))
		< IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"An error occurred obtaining the shipping address information.");
		goto ERROR;
	}

	/*
	** Build the SHIPPING_ADDRESS group.
	*/
	(void) ims_addODLObject (msgDesc, product_request, &shipping_address,
		"SHIPPING_ADDRESS", IMS_TRUE, IMS_ODL_GROUP);

	(void) ims_addODLKeyword (msgDesc, shipping_address,
		"FIRST_NAME", TV_STRING, addressSpec.first_name);

	(void) ims_addODLKeyword (msgDesc, shipping_address,
		"MIDDLE_INITIAL", TV_STRING, addressSpec.initial_name);

	(void) ims_addODLKeyword (msgDesc, shipping_address,
		"LAST_NAME", TV_STRING, addressSpec.last_name);

	(void) ims_addODLKeyword (msgDesc, shipping_address,
		"TITLE", TV_STRING, addressSpec.title);

	(void) ims_addODLKeyword (msgDesc, shipping_address,
		"ORGANIZATION", TV_STRING, addressSpec.organization);

	(void) ims_addODLKeyword (msgDesc, shipping_address,
		"ADDRESS", TV_STRING, addressSpec.street);

	(void) ims_addODLKeyword (msgDesc, shipping_address,
		"CITY", TV_STRING, addressSpec.city);

	(void) ims_addODLKeyword (msgDesc, shipping_address,
		"STATE", TV_STRING, addressSpec.state);

	(void) ims_addODLKeyword (msgDesc, shipping_address,
		"COUNTRY", TV_STRING, addressSpec.country);

	(void) ims_addODLKeyword (msgDesc, shipping_address,
		"ZIP", TV_STRING, addressSpec.zipcode);

	(void) ims_addODLKeyword (msgDesc, shipping_address,
		"PHONE", TV_STRING, addressSpec.phone);

	(void) ims_addODLKeyword (msgDesc, shipping_address,
		"FAX", TV_STRING, addressSpec.fax);

	(void) ims_addODLKeyword (msgDesc, shipping_address,
		"EMAIL", TV_STRING, addressSpec.email);

	/*
	** Query for billing address information.
	*/
	(void) memset ((ADDRESS_SPEC *) &addressSpec, 0, sizeof(ADDRESS_SPEC));
	addressType = BILLING;

	if ((status = getAddressInfo (msgDesc, &addressSpec, addressType))
		< IMS_OK)
	{
		(void) ims_msg (msgDesc, status,
			"An error occurred obtaining the billing address information.");
		goto ERROR;
	}

	/*
	** Build the BILLING_ADDRESS group.
	*/
	(void) ims_addODLObject (msgDesc, product_request, &billing_address,
		"BILLING_ADDRESS", IMS_TRUE, IMS_ODL_GROUP);

	(void) ims_addODLKeyword (msgDesc, billing_address,
		"FIRST_NAME", TV_STRING, addressSpec.first_name);

	(void) ims_addODLKeyword (msgDesc, billing_address,
		"MIDDLE_INITIAL", TV_STRING, addressSpec.initial_name);

	(void) ims_addODLKeyword (msgDesc, billing_address,
		"LAST_NAME", TV_STRING, addressSpec.last_name);

	(void) ims_addODLKeyword (msgDesc, billing_address,
		"TITLE", TV_STRING, addressSpec.title);

	(void) ims_addODLKeyword (msgDesc, billing_address,
		"ORGANIZATION", TV_STRING, addressSpec.organization);

	(void) ims_addODLKeyword (msgDesc, billing_address,
		"ADDRESS", TV_STRING, addressSpec.street);

	(void) ims_addODLKeyword (msgDesc, billing_address,
		"CITY", TV_STRING, addressSpec.city);

	(void) ims_addODLKeyword (msgDesc, billing_address,
		"STATE", TV_STRING, addressSpec.state);

	(void) ims_addODLKeyword (msgDesc, billing_address,
		"COUNTRY", TV_STRING, addressSpec.country);

	(void) ims_addODLKeyword (msgDesc, billing_address,
		"ZIP", TV_STRING, addressSpec.zipcode);

	(void) ims_addODLKeyword (msgDesc, billing_address,
		"PHONE", TV_STRING, addressSpec.phone);

	(void) ims_addODLKeyword (msgDesc, billing_address,
		"FAX", TV_STRING, addressSpec.fax);

	(void) ims_addODLKeyword (msgDesc, billing_address,
		"EMAIL", TV_STRING, addressSpec.email);

	/*
	** Build the USER_AFFILIATION group.
	*/
	(void) ims_addODLObject (msgDesc, product_request, &user_affiliation,
		"USER_AFFILIATION", IMS_TRUE, IMS_ODL_GROUP);

	(void) ims_addODLKeyword (msgDesc, user_affiliation,
		"CATEGORY", TV_STRING, "USA");

	(void) ims_addODLKeyword (msgDesc, user_affiliation,
		"TYPE", TV_STRING, "GOVERNMENT");

	/*
	** Process all input order items.
	*/
	for (i = 0; i < itemCount; i++)
	{
	 	/*
		** Query for order item information.
		*/
		(void) memset ((ITEM_SPEC *) &itemSpec, 0, sizeof(ITEM_SPEC));

		if ((status = getItemInfo (msgDesc, &itemSpec, i)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"An error occurred obtaining the order item information.");
			goto ERROR;
		}

		/*
		** Build the LINE_ITEM group.
		*/
		(void) ims_addODLObject (msgDesc, product_request, &line_item,
			"LINE_ITEM", IMS_TRUE, IMS_ODL_GROUP);

		(void) ims_addODLKeyword (msgDesc, line_item,
			"DATASET_ID", TV_STRING, itemSpec.dataset_id);

		(void) ims_addODLKeyword (msgDesc, line_item,
			"PACKAGE_ID", TV_STRING, itemSpec.package_id);

		(void) ims_addODLKeyword (msgDesc, line_item,
			"PROCESSING_OPTIONS", TV_STRING, itemSpec.process_option);

		(void) ims_addODLKeyword (msgDesc, line_item,
			"MEDIA_TYPE", TV_STRING, itemSpec.media_type);

		(void) ims_addODLKeyword (msgDesc, line_item,
			"MEDIA_FORMAT", TV_STRING, itemSpec.media_format);

		(void) ims_addODLKeyword (msgDesc, line_item,
			"BILLING_ID", TV_STRING, billing_id);
	}

	/*
	** If the flag is set, generate an ODL file, otherwise
	** place an order.
	*/
	if (odlFileFlag == IMS_TRUE)
	{
		(void) sprintf (fileName, "%s_%d.odl", programName, orderId);

		if ((status = ims_buildPMF (msgDesc, product_request,
			fileName, NULL)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not write out the ODL file 'ODL.out'.");
			goto ERROR;
		}

		(void) ims_msg (msgDesc, IMS_INFO,
			"Successfully generated ODL file '%s'.", fileName);
	}
	else
	{
		if ((status = ims_buildAggregate (msgDesc, product_request,
			&root)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not build the aggregate structure.");
			goto ERROR;
		}

		if ((status = ims_order (msgDesc, root, &newOrderId)) < IMS_OK)
		{
			(void) ims_msg (msgDesc, status,
				"Could not submit the order.");
			goto ERROR;
		}

		(void) ims_msg (msgDesc, IMS_INFO,
			"Successfully submitted order '%d'.", newOrderId);
	}

	(void) ims_qiFreeDesc (qDesc);
	(void) ims_msgStructFree (msgDesc);

	exit (0);

ERROR:
	(void) ims_qiFreeDesc (qDesc);
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
	(void) fprintf (stderr, "\n\nTermination of %s due to signal: "
		"%s (%d)\n\n", programName, ims_sigMsg (sig), sig);

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
	char prompt[20];
	char str[1024];
	char str2[1024];
	int invalid;
	int i;
	int j;
	short itemFlag;
	int number;
	int num_items;
	int status;

	status = IMS_OK;

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
		if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
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

	/* orderId */
	number = 0;
	if (commands.orderId == (char *) NULL)
	{
		/* We expect a number. */
		do
		{
			if (ims_getString (IMS_TRUE, inputBuffer, IMS_INPUT_BUF_LEN,
				"Order Id: ") == (char *) NULL)
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
				((number < IMS_ZERO) || (number > IMS_MAX_INT)))
			{
				invalid = 1;
				(void) printf (
					"Numerical input is not in the range of '%d' "
					"to '%d'. Try again.\n", IMS_ZERO, IMS_MAX_INT);
			}
		}while (invalid);

		orderId = (int) number;
	}
	else if (ims_isInteger (commands.orderId) == IMS_TRUE)
	{
		number = (int) atoi (commands.orderId);
		if ((number < IMS_ZERO ) || (number > IMS_MAX_INT))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The parameter 'orderId' has a value of '%d', which "
				"is not in the range of '%d' to '%d'.",
				number, IMS_ZERO, IMS_MAX_INT);
			status = IMS_ERROR;
		}
		else
		{
			orderId = (int) number;
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The parameter 'orderId' must contain a valid integer "
			"value.");
		status = IMS_ERROR;
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
				((number < 1) || (number > IMS_MAX_ITEMS)))
			{
				invalid = 1;
				(void) printf (
					"Numerical input is not in the range of '1' "
					"to '%d'. Try again.\n", IMS_MAX_ITEMS);
			}
		}while (invalid);

		itemCount = (int) number;
	}
	else if (ims_isInteger (commands.itemCount) == IMS_TRUE)
	{
		number = (int) atoi (commands.itemCount);
		if ((number < 1) || (number > IMS_MAX_ITEMS))
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The parameter 'itemCount' has a value of '%d', which "
				"is not in the range of '1' to '%d'.",
				number, IMS_MAX_ITEMS);
			return (IMS_ERROR);
		}
		else
		{
			itemCount = (int) number;
		}
	}
	else
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"The parameter 'itemCount' must contain a valid "
			"integer value.");
		return (IMS_ERROR);
	}

	/* itemList */
	if (commands.itemList == (char *) NULL)
	{
		for (j = 0; j < itemCount; j++)
		{
			/* We expect a number. */
			do
			{
				(void) sprintf (prompt, "Item %d: ", j+1);

				if (ims_getString (IMS_TRUE, inputBuffer,
					IMS_INPUT_BUF_LEN, prompt) == (char *) NULL)
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
				if((invalid == 0 ) && ((int) strlen(inputBuffer) > 0) &&
					((number < IMS_ZERO) || (number > IMS_MAX_SINT)))
				{
					invalid = 1;
					(void) printf (
						"Numerical input is not in the range of '%d' "
						"to '%d'. Try again.\n", IMS_ZERO,
						IMS_MAX_SINT);
				}
			}while (invalid);

			itemList[j] = (int) number;
		}
	}
	else  /* Parse the provided itemList. */
	{
		(void) strcpy (str, ims_truncStr (ims_removeQuotes (
			commands.itemList)));
		j = num_items = 0;
		itemFlag = IMS_TRUE;
		while (itemFlag == IMS_TRUE)
		{
			/*
			** See if we are at the end of the list.
			*/
			if ((i = ims_strIndex (str, " ")) ==  -1)
			{
				i = strlen (str);
				itemFlag = IMS_FALSE;
			}

			/*
			** Save our current item.
			*/
			(void) strcpy (str2, str);
			str2[i] = '\0';

			/*
			** Position the list at the next item.
			*/
			(void) strcpy (str, ims_truncStr (str+i));

			if (ims_isInteger (str2) == IMS_TRUE)
			{
				number = (int) atoi (str2);
			}
			else
			{
				(void) ims_msg (msgDesc, IMS_ERROR,
					"The parameter 'itemList' must contain valid "
					"integer values separated by blanks.");
				return (IMS_ERROR);
			}

			num_items++;
			itemList[j] = (int) number;
			j++;
		}

		/*
		** Compare itemCount with the number of sets parsed.
		*/
		if (itemCount != num_items)
		{
			(void) ims_msg (msgDesc, IMS_ERROR,
				"The itemCount '%d' does not match the number of "
				"items in itemList '%d'.", itemCount, num_items);
			status = IMS_ERROR;
		}
	}

	/* odlFileFlag */
	if (commands.odlFileFlag != (char *) NULL)
	{
		odlFileFlag = IMS_TRUE;
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

	if (status < IMS_OK)
	{
		return (status);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** openConnection ()
**
******************************************************************************/

static int openConnection (
	IMS_MSG_STRUCT *msgDesc)
{
	int status;

	/*
	** Allocate a query descriptor.
	*/
	if ((qDesc = ims_qiDescAlloc (msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not allocate a query descriptor.");
		return (IMS_FATAL);
	}

	/*
	** Setup the descriptor with necessary information about this
	** process.
	*/
	IMS_SETUSER (qDesc, userSpec.username);

	IMS_SETPSWD (qDesc, userSpec.password);

	IMS_SETPROG (qDesc, userSpec.program);

	if (userSpec.server != (char *) NULL)
	{
		IMS_SETSERVER (qDesc, userSpec.server);
	}

	if (userSpec.database != (char *) NULL)
	{
		IMS_SETDBNAME (qDesc, userSpec.database);
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
		return (status);
	}

	/*
	** Associate the message descriptor with the dbproc so
	** the Sybase error and message handling can be performed.
	*/
	IMS_SET_USERDATA (qDesc);

	return (IMS_OK);
}

/******************************************************************************
**
** getAuthInfo ()
**
******************************************************************************/

static int getAuthInfo (
	IMS_MSG_STRUCT *msgDesc,
	char *authenticator,
	char *billing_id)
{
	short orderItemType;
	int status;

	/*
	** Set the number of rows to be returned.
	*/
	IMS_SETROWCOUNT (qDesc, "1");

	/*
	** Set up the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"select auth_key, account_id, order_item_type "
		"from order_queue o, user_profile u, order_item i "
		"where o.order_id = %d "
		"and o.order_id = i.order_id "
		"and o.user_id = u.user_id",
		orderId);

	/*
	** Process the result rows for this query.
	*/
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}
	}

	/*
	** Check to see if a row was returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain information for order '%d'.",
			orderId);
		return (IMS_ERROR);
	}

	/*
	** Copy in the returned data.
	*/

	/* auth_key */
	(void) memcpy (authenticator, IMS_VALUE (qDesc, 0),
		IMS_VALUELENGTH (qDesc, 0));
	authenticator[IMS_VALUELENGTH (qDesc, 0)] = '\0';
	(void) ims_truncStr (authenticator);

	/* account_id */
	(void) memcpy (billing_id, IMS_VALUE (qDesc, 1),
		IMS_VALUELENGTH (qDesc, 1));
	billing_id[IMS_VALUELENGTH (qDesc, 1)] = '\0';
	(void) ims_truncStr (billing_id);

	/* order_item_type */
	(void) memcpy (&orderItemType, IMS_VALUE (qDesc, 2),
		IMS_VALUELENGTH (qDesc, 2));

	/*
	** This software only supports certain product requests.
	*/
	switch (orderItemType)
	{
		case 1: /* APR */
		case 2: /* RPR */
		case 3: /* FPR */
		case 4: /* COR */
			break;

		case 5: /* TDR */
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Order resubmission is not supported for Tape Dub Request orders.");
			return (IMS_ERROR);

		case 6: /* TSR */
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Order resubmission is not supported for Tape Scan Request orders.");
			return (IMS_ERROR);

		case 7: /* DAR */
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Order resubmission is not supported for Data Acquisition "
				"Request orders.");
			return (IMS_ERROR);

		default:
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Invalid order item type encountered.");
			return (IMS_ERROR);
	}

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (IMS_ERROR);
	}
	IMS_SETROWCOUNT (qDesc, "0");

	return (IMS_OK);
}

/******************************************************************************
**
** getAddressInfo ()
**
******************************************************************************/

static int getAddressInfo (
	IMS_MSG_STRUCT *msgDesc,
	ADDRESS_SPEC *addressSpec,
	int addressType)
{
	char addressTable[IMS_COL30_LEN+1];
	int status;

	/*
	** Determine the table to query based on the address type.
	*/
	switch (addressType)
	{
		case CONTACT:
			(void) sprintf (addressTable, "contact_profile");
			break;

		case SHIPPING:
			(void) sprintf (addressTable, "shipping_profile");
			break;

		case BILLING:
			(void) sprintf (addressTable, "billing_profile");
			break;

		default:
			(void) ims_msg (msgDesc, IMS_ERROR,
				"Invalid address type specified.");
			return (IMS_ERROR);
	}

	/*
	** Set up the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"select first_name, initial_name, last_name, title, organization, "
		"street, city, state, country, zipcode, phone, fax, email "
		"from %s "
		"where order_id = %d",
		addressTable, orderId);

	/*
	** Process the result rows for this query.
	*/
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}
	}

	/*
	** Check to see if a row was returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"No rows returned.");
		return (IMS_ERROR);
	}

	/*
	** Check to see if more than one row was returned.
	*/
	if (IMS_AFFECTED (qDesc) > 1)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"More than one row returned.");
		return (IMS_ERROR);
	}

	/*
	** Copy in the returned data.
	*/

	/* first_name */
	(void) memcpy (addressSpec->first_name, IMS_VALUE (qDesc, 0),
		IMS_VALUELENGTH (qDesc, 0));
	addressSpec->first_name[IMS_VALUELENGTH (qDesc, 0)] = '\0';
	(void) ims_truncStr (addressSpec->first_name);

	/* initial_name */
	if ((IMS_VALUELENGTH (qDesc, 1) == 0) ||
		(IMS_VALUE (qDesc, 1) == (char *) NULL))
	{
		/* initial_name is NULL */
		addressSpec->initial_name[0] = '\0';
	}
	else
	{
		(void) memcpy (addressSpec->initial_name, IMS_VALUE (qDesc, 1),
			IMS_VALUELENGTH (qDesc, 1));
		addressSpec->initial_name[IMS_VALUELENGTH (qDesc, 1)] = '\0';
		(void) ims_truncStr (addressSpec->initial_name);
	}

	/* last_name */
	(void) memcpy (addressSpec->last_name, IMS_VALUE (qDesc, 2),
		IMS_VALUELENGTH (qDesc, 2));
	addressSpec->last_name[IMS_VALUELENGTH (qDesc, 2)] = '\0';
	(void) ims_truncStr (addressSpec->last_name);

	/* title */
	if ((IMS_VALUELENGTH (qDesc, 3) == 0) ||
		(IMS_VALUE (qDesc, 3) == (char *) NULL))
	{
		/* title is NULL */
		addressSpec->title[0] = '\0';
	}
	else
	{
		(void) memcpy (addressSpec->title, IMS_VALUE (qDesc, 3),
			IMS_VALUELENGTH (qDesc, 3));
		addressSpec->title[IMS_VALUELENGTH (qDesc, 3)] = '\0';
		(void) ims_truncStr (addressSpec->title);
	}

	/* organization */
	if ((IMS_VALUELENGTH (qDesc, 4) == 0) ||
		(IMS_VALUE (qDesc, 4) == (char *) NULL))
	{
		/* organization is NULL */
		addressSpec->organization[0] = '\0';
	}
	else
	{
		(void) memcpy (addressSpec->organization, IMS_VALUE (qDesc, 4),
			IMS_VALUELENGTH (qDesc, 4));
		addressSpec->organization[IMS_VALUELENGTH (qDesc, 4)] = '\0';
		(void) ims_truncStr (addressSpec->organization);
	}

	/* street */
	if ((IMS_VALUELENGTH (qDesc, 5) == 0) ||
		(IMS_VALUE (qDesc, 5) == (char *) NULL))
	{
		/* street is NULL */
		addressSpec->street[0] = '\0';
	}
	else
	{
		(void) memcpy (addressSpec->street, IMS_VALUE (qDesc, 5),
			IMS_VALUELENGTH (qDesc, 5));
		addressSpec->street[IMS_VALUELENGTH (qDesc, 5)] = '\0';
		(void) ims_truncStr (addressSpec->street);
	}

	/* city */
	if ((IMS_VALUELENGTH (qDesc, 6) == 0) ||
		(IMS_VALUE (qDesc, 6) == (char *) NULL))
	{
		/* city is NULL */
		addressSpec->city[0] = '\0';
	}
	else
	{
		(void) memcpy (addressSpec->city, IMS_VALUE (qDesc, 6),
			IMS_VALUELENGTH (qDesc, 6));
		addressSpec->city[IMS_VALUELENGTH (qDesc, 6)] = '\0';
		(void) ims_truncStr (addressSpec->city);
	}

	/* state */
	if ((IMS_VALUELENGTH (qDesc, 7) == 0) ||
		(IMS_VALUE (qDesc, 7) == (char *) NULL))
	{
		/* state is NULL */
		addressSpec->state[0] = '\0';
	}
	else
	{
		(void) memcpy (addressSpec->state, IMS_VALUE (qDesc, 7),
			IMS_VALUELENGTH (qDesc, 7));
		addressSpec->state[IMS_VALUELENGTH (qDesc, 7)] = '\0';
		(void) ims_truncStr (addressSpec->state);
	}

	/* country */
	if ((IMS_VALUELENGTH (qDesc, 8) == 0) ||
		(IMS_VALUE (qDesc, 8) == (char *) NULL))
	{
		/* country is NULL */
		addressSpec->country[0] = '\0';
	}
	else
	{
		(void) memcpy (addressSpec->country, IMS_VALUE (qDesc, 8),
			IMS_VALUELENGTH (qDesc, 8));
		addressSpec->country[IMS_VALUELENGTH (qDesc, 8)] = '\0';
		(void) ims_truncStr (addressSpec->country);
	}

	/* zipcode */
	if ((IMS_VALUELENGTH (qDesc, 9) == 0) ||
		(IMS_VALUE (qDesc, 9) == (char *) NULL))
	{
		/* zipcode is NULL */
		addressSpec->zipcode[0] = '\0';
	}
	else
	{
		(void) memcpy (addressSpec->zipcode, IMS_VALUE (qDesc, 9),
			IMS_VALUELENGTH (qDesc, 9));
		addressSpec->zipcode[IMS_VALUELENGTH (qDesc, 9)] = '\0';
		(void) ims_truncStr (addressSpec->zipcode);
	}

	/* phone */
	if ((IMS_VALUELENGTH (qDesc, 10) == 0) ||
		(IMS_VALUE (qDesc, 10) == (char *) NULL))
	{
		/* phone is NULL */
		addressSpec->phone[0] = '\0';
	}
	else
	{
		(void) memcpy (addressSpec->phone, IMS_VALUE (qDesc, 10),
			IMS_VALUELENGTH (qDesc, 10));
		addressSpec->phone[IMS_VALUELENGTH (qDesc, 10)] = '\0';
		(void) ims_truncStr (addressSpec->phone);
	}

	/* fax */
	if ((IMS_VALUELENGTH (qDesc, 11) == 0) ||
		(IMS_VALUE (qDesc, 11) == (char *) NULL))
	{
		/* fax is NULL */
		addressSpec->fax[0] = '\0';
	}
	else
	{
		(void) memcpy (addressSpec->fax, IMS_VALUE (qDesc, 11),
			IMS_VALUELENGTH (qDesc, 11));
		addressSpec->fax[IMS_VALUELENGTH (qDesc, 11)] = '\0';
		(void) ims_truncStr (addressSpec->fax);
	}

	/* email */
	if ((IMS_VALUELENGTH (qDesc, 12) == 0) ||
		(IMS_VALUE (qDesc, 12) == (char *) NULL))
	{
		/* email is NULL */
		addressSpec->email[0] = '\0';
	}
	else
	{
		(void) memcpy (addressSpec->email, IMS_VALUE (qDesc, 12),
			IMS_VALUELENGTH (qDesc, 12));
		addressSpec->email[IMS_VALUELENGTH (qDesc, 12)] = '\0';
		(void) ims_truncStr (addressSpec->email);
	}

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (IMS_ERROR);
	}

	return (IMS_OK);
}

/******************************************************************************
**
** getItemInfo ()
**
** This function restricts the rows returned to one row because the
** media_type and media_fmt_type values from the order_item table may map
** to more than one v0_*_type value in their respective map tables.
** This doesn't cause a problem because the end result is the same.
**
******************************************************************************/

static int getItemInfo (
	IMS_MSG_STRUCT *msgDesc,
	ITEM_SPEC *itemSpec,
	int counter)
{
	int status;

	/*
	** Set the number of rows to be returned.
	*/
	IMS_SETROWCOUNT (qDesc, "1");

	/*
	** Set up the command buffer with the SQL statement.
	*/
	(void) sprintf (cmdBuf,
		"select dataset, granule_name, v0_process_type, "
		"v0_media_type, v0_media_fmt_type "
		"from order_item i, media_type_map t, media_fmt_map f "
		"where order_id = %d "
		"and item_id = %d "
		"and i.media_type = t.media_type "
		"and i.media_fmt_type = f.media_fmt_type",
		orderId, itemList[counter]);

	/*
	** Process the result rows for this query.
	*/
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			return (status);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}
	}

	/*
	** Check to see if a row was returned.
	*/
	if (IMS_AFFECTED (qDesc) <= 0)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not obtain information for item '%d' of order '%d'.",
			itemList[counter], orderId);
		return (IMS_ERROR);
	}

	/*
	** Copy in the returned data.
	*/

	/* dataset */
	(void) memcpy (itemSpec->dataset_id, IMS_VALUE (qDesc, 0),
		IMS_VALUELENGTH (qDesc, 0));
	itemSpec->dataset_id[IMS_VALUELENGTH (qDesc, 0)] = '\0';
	(void) ims_truncStr (itemSpec->dataset_id);

	/* granule_name */
	(void) memcpy (itemSpec->package_id, IMS_VALUE (qDesc, 1),
		IMS_VALUELENGTH (qDesc, 1));
	itemSpec->package_id[IMS_VALUELENGTH (qDesc, 1)] = '\0';
	(void) ims_truncStr (itemSpec->package_id);

	/* v0_process_type */
	(void) memcpy (itemSpec->process_option, IMS_VALUE (qDesc, 2),
		IMS_VALUELENGTH (qDesc, 2));
	itemSpec->process_option[IMS_VALUELENGTH (qDesc, 2)] = '\0';
	(void) ims_truncStr (itemSpec->process_option);

	/* v0_media_type */
	(void) memcpy (itemSpec->media_type, IMS_VALUE (qDesc, 3),
		IMS_VALUELENGTH (qDesc, 3));
	itemSpec->media_type[IMS_VALUELENGTH (qDesc, 3)] = '\0';
	(void) ims_truncStr (itemSpec->media_type);

	/* v0_media_fmt_type */
	(void) memcpy (itemSpec->media_format, IMS_VALUE (qDesc, 4),
		IMS_VALUELENGTH (qDesc, 4));
	itemSpec->media_format[IMS_VALUELENGTH (qDesc, 4)] = '\0';
	(void) ims_truncStr (itemSpec->media_format);

	/*
	** Reset the query descriptor.
	*/
	if ((status = ims_qiResetDesc (qDesc)) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_ERROR,
			"Could not reset the query descriptor.");
		return (IMS_ERROR);
	}
	IMS_SETROWCOUNT (qDesc, "0");

	return (IMS_OK);
}

