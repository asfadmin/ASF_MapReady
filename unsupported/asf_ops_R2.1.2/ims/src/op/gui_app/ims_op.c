static char *sccs = "@(#)ims_op.c	5.5  05/12/97";
/*-----------------------------------------------------------
 * This is the project main program file for Xt generated 
 * code. You may add application dependent source code 
 * at the appropriate places. 
 * 			     
 * Do not modify the statements preceded by the dollar
 * sign ($), these statements will be replaced with
 * the appropriate source code when the main program is  
 * generated.  
 *
 * $Date$  		$Revision$ 
 *-----------------------------------------------------------*/ 

#include <stdio.h>
#include <syslog.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xlib.h>
#include <Xm/Xm.h>
#include <Xm/MessageB.h>

/*
** Include applications specific header files 
*/
#include <ims_const.h>
#include <ims_msg.h>
#include <ims_cmd.h>
#include <ims_dbms.h>
#include <ims_qi.h>
#include <ims_media.h>
#include <ims_opCat.h>
#include <ims_op.h>

/*---------------------------------------------------- 
 * UxXt.h needs to be included only when compiling a 
 * stand-alone application. 
 *---------------------------------------------------*/
#ifndef DESIGN_TIME
#include "UxXt.h"
#endif /* DESIGN_TIME */

XtAppContext	UxAppContext;
Widget		UxTopLevel;
Display		*UxDisplay;
int		UxScreen;

/*----------------------------------------------
 * Insert application global declarations here
 *---------------------------------------------*/
#define DEF_LOG_DIRECTORY  "./"
#define DEF_WORK_DIRECTORY "./"


/* global data definition for the main program. */
extern Widget		dl2dtkPB;
OP_GLOBAL_DATA	glbData;     /* global data; common to all interfaces */
char						*glbLogin;             
char						*glbPassword;
char						*glbLogFileDir;
char						*glbWorkDir;
char						*glbLogFile;
int							glbDefaultSearchFlag = 0; /* default search query flag */
int							glbWelcomeOrderFlag = 0;  /* welcome_order flag */


/*---------------------------------------------- 
 * External function declarations 
 *---------------------------------------------*/

extern char *ims_extractFileName ();
extern int ims_getCmdLine ();
extern int ims_getFileParms ();
extern char *ims_getString ();
extern char *ims_getPassword ();
extern char *ims_timeStamp ();
extern int  ims_msgStructFree ();    
extern Widget create_workingDlg();
extern void forceUpdate();

/*---------------------------------------------- 
 * static function declarations 
 *---------------------------------------------*/

static int processCmdLine (IMS_MSG_STRUCT *);
static void usage (void);
static int validateUser (OP_GLOBAL_DATA *);

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
**
** 09/09/96 - Added defaultSearch command.
*/
static struct commands
{
	char *login;
	char *password;
	char *logFile;
	char *logFileDir;
	char *workDir;
	char *help;
	char *releaseNo;
	char *commandFile;
	char *defaultSearch;
} commands;

/*
** "Itemlist" for getting the above commands from the commandline.
**
** Added defaultSearch command.
*/
static IMS_CMD_CLASS cmdLineElm[] =
{
	{"-U",          &commands.login},
	{"+login",      &commands.login},
	{"-P",          &commands.password},
	{"+password",   &commands.password},
	{"-L",          &commands.logFile},
	{"+logFile",    &commands.logFile},
	{"-D",          &commands.logFileDir},
	{"+logFileDir", &commands.logFileDir},
	{"-W",          &commands.workDir},
	{"+workDir",    &commands.workDir},
	{"-C",          &commands.commandFile},
	{"+commandFile",&commands.commandFile},
	{"-h",          &commands.help},
	{"+help",       &commands.help},
	{"-r",          &commands.releaseNo},
	{"+release",    &commands.releaseNo},
	{"-s",          &commands.defaultSearch},
	{"+search",     &commands.defaultSearch},
};

static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);

/*
** "Itemlist" for getting the above commands from a command file.
*/
static IMS_CMD_CLASS cmdFileElm[] =
{
	{"login",      &commands.login},
	{"password",   &commands.password},
	{"logFile",    &commands.logFile},
	{"logFileDir", &commands.logFileDir},
	{"workDir",    &commands.workDir},
	{"release",    &commands.releaseNo},
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

/*
** Working Dialog box 
*/
static Widget workingDlg;

#ifdef _NO_PROTO
main(argc,argv)
        int     argc;
        char    *argv[];
#else
main( int argc, char *argv[])
#endif /* _NO_PROTO */
{

	/*-----------------------------------------------------------
	 * Declarations.
   * The default identifier - mainIface will only be declared 
	 * if the interface function is global and of type swidget.
	 * To change the identifier to a different name, modify the
	 * string mainIface in the file "xtmain.dat". If "mainIface"
	 * is declared, it will be used below where the return value
   * of  PJ_INTERFACE_FUNCTION_CALL will be assigned to it.
   *----------------------------------------------------------*/ 

  Widget mainIface;
	IMS_MSG_STRUCT *msgDesc;
	OP_USER_SPEC *userSpec;
	OP_CAT_STRUCT *catReq;
	char logName[IMS_COL60_LEN+1];
	char logFilePath[IMS_COL255_LEN+1];
	int status;

	/*---------------------------------
   * Interface function declaration
	 *--------------------------------*/	

 	Widget  create_welcome(swidget);

	swidget UxParent = NULL;

	/*---------------------
	 * Initialize program
	 *--------------------*/

#ifdef XOPEN_CATALOG
	if (XSupportsLocale()) {
		XtSetLanguageProc(NULL,(XtLanguageProc)NULL,NULL);
	}
#endif

  	UxTopLevel = XtAppInitialize(&UxAppContext, "ims_op",
				     NULL, 0, &argc, argv, NULL, NULL, 0);

	UxDisplay = XtDisplay(UxTopLevel);
	UxScreen = XDefaultScreen(UxDisplay);

	/* We set the geometry of UxTopLevel so that dialogShells
	   that are parented on it will get centered on the screen
	   (if defaultPosition is true). */

	XtVaSetValues(UxTopLevel,
			XtNx, 0,
			XtNy, 0,
			XtNwidth, DisplayWidth(UxDisplay, UxScreen),
			XtNheight, DisplayHeight(UxDisplay, UxScreen),
			NULL);

	/*-------------------------------------------------------
	 * Insert initialization code for your application here 
	 *------------------------------------------------------*/

	/*
	** Initialize message facility.
	*/
  /* Allocate msg structure */
	if ((msgDesc = (IMS_MSG_STRUCT *)ims_msgStructAlloc ())
					== (IMS_MSG_STRUCT *)NULL)
	{
		fprintf (stderr, "IMS_MSG_STRUCT allocation error.");
		exit (IMS_FATAL);
	}

	(void) ims_msgBanner (msgDesc, "ims_op", IMS_MSG_ALLBANNER);
	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgProgramName (msgDesc, "IMS_OP");
	(void) ims_msgSeverityFlag (msgDesc, IMS_ON);
	(void) ims_msgStderrFlag (msgDesc, IMS_ON);
	(void) ims_msgQueueFlag (msgDesc, IMS_OFF);
	(void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);


	/*
	** Now, get command line args.
	*/
	if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
		(int) cmdLineElmCount, msgDesc)) < IMS_OK)
	{
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** If help has been specified, print usage and exit.
	*/
	if (commands.help != (char *) NULL)
	{
		usage ();
		(void) ims_msgStructFree (msgDesc);
		exit (IMS_INFO);
	}

	/*
	** 09/09/96 - PR 85
	** If default search been specified, execution of default order
	** search is desired, absence of the default search option 
	** indicates that default search is not executed and blank
	** order screen is displayed.
	*/
	if (commands.defaultSearch != (char *) NULL)
		glbDefaultSearchFlag = 1;
	else
		glbDefaultSearchFlag = 0;


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
			(void) ims_msgStructFree (msgDesc);
			exit (1);
		}

		/*
		** Now, get command line args again to overlay file args.
		*/
		if ((status = ims_getCmdLine (argc, argv, cmdLineElm, 
			(int) cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msgStructFree (msgDesc);
			exit (1);
		}
	}

	/*
	** Update the request structure with information from the command
	** line or from a command file.
	*/
	if ((status = processCmdLine (msgDesc)) < IMS_OK)
	{
		(void) ims_msgStructFree (msgDesc);
		exit (status);
	}

	/* Initialize log file */
	if (glbLogFileDir != (char *)NULL)
	{
		if (glbLogFile != (char *)NULL)
		{
			(void) ims_concatFilePath (logFilePath, glbLogFileDir, glbLogFile);
		}
		else
		{
			(void) sprintf (logName, "ims_op_%s.log", 
			(char *)ims_timeStamp ());
			(void) ims_concatFilePath (logFilePath, glbLogFileDir, logName);
		}

		/* Turn-on logfile option in ims_msg */
		if (ims_msgLogFileName (msgDesc, logFilePath) < IMS_OK)
		{
			ims_msg (msgDesc, IMS_FATAL, 
				"Log file creation failed, '%s'.", logFilePath);

			(void) ims_msgStructFree (msgDesc);
			exit (-1);
		}
	}


	/*
	** 1/4/96 - If user did not specify Login or Password, popup 
	** the login dialog box, otherwise just use the Login and Password
	** from the command line parameters.
	*/
	if ((glbLogin == (char *)NULL) || (glbPassword == (char *)NULL))
	{
		if ((status = getUserLogin(UxParent)) < IMS_OK)
		{
			/* user decided not to start ims_op */
			(void) ims_msgStructFree (msgDesc);
			exit (0);
		}
	}


	/*
	** Initialize global_data parameters.
	*/
	glbData.program = ims_extractFileName (argv[0]);
	glbData.msgDesc = msgDesc;
	glbData.welcomeFlag = 0;
	glbData.orderFlag = 0;
	glbData.searchFlag = 0;

	userSpec = &(glbData.userSpec);
	glbData.workDirectory = glbWorkDir;
	userSpec->userName = glbLogin;
	userSpec->password = glbPassword;
	userSpec->server = NULL;
	userSpec->dbName = NULL;
	userSpec->userType = OP_NOTYPE;

	/*
	** validate user login and verify capabilities
	*/
	if (!validateUser (&glbData))
	{
		ims_msg (msgDesc, IMS_ERROR, 
			"Invalid user '%s'; Contact DBA.", userSpec->userName);
		exit (IMS_FATAL);
	}


	/*
	** turn-off stderr flag
	*/
	ims_msgStderrFlag (msgDesc, IMS_OFF);


	/*
	** Pop up working dialog while creating all ims_op screens.
	** to inform user that ims_op is in the process of initializing.
	*/
	workingDlg = create_workingDlg(UxTopLevel);
	XtUnmanageChild (XmMessageBoxGetChild (workingDlg, XmDIALOG_SEPARATOR));
	XtUnmanageChild (XmMessageBoxGetChild (workingDlg, XmDIALOG_CANCEL_BUTTON));
	XtUnmanageChild (XmMessageBoxGetChild (workingDlg, XmDIALOG_OK_BUTTON));
	XtUnmanageChild (XmMessageBoxGetChild (workingDlg, XmDIALOG_HELP_BUTTON));

	/* 
	** 06/13/96 - this is done to prevent program core.
  ** Tell the Window Mgr. that you are going to handle 
  ** the close action yourself by setting XmNdeleteResponse
  ** to XmDO_NOTHING
  */
  XtVaSetValues(XtParent(workingDlg), 
      XmNdeleteResponse, XmDO_NOTHING,
      NULL );

	XtManageChild (workingDlg);
	XtPopup (XtParent(workingDlg), XtGrabNone);
	forceUpdate (workingDlg);

	/* 3/25/96 - not needed anymore.
  while (XtAppPending(XtWidgetToApplicationContext(workingDlg)))
	{
    XtAppProcessEvent(XtWidgetToApplicationContext(workingDlg), XtIMAll);
	}
	*/

	/*----------------------------------------------------------------
	 * Create and popup the first window of the interface.  The 	 
	 * return value can be used in the popdown or destroy functions.
   * The Widget return value of  PJ_INTERFACE_FUNCTION_CALL will 
   * be assigned to "mainIface" from  PJ_INTERFACE_RETVAL_TYPE. 
	 *---------------------------------------------------------------*/

	mainIface = create_welcome(UxParent);
	glbData.welcomeW = mainIface;

	/*
	** Let's destroy the working dialog before we pop up main interface.
	*/
	if (workingDlg)
	{
		XtPopdown (XtParent(workingDlg));
		XtDestroyWidget(workingDlg);
		workingDlg = NULL;
	}

	UxPopupInterface(mainIface, no_grab);
	glbData.welcomeFlag = 1;

	/*-----------------------
	 * Enter the event loop 
	 *----------------------*/

  	XtAppMainLoop (UxAppContext);

}

/**************************************************************************
**
** processCmdLine - 
**
** Update the request structure and prompt for needed 
** information not provided either in the commandLine or
** commandFile.
**
***************************************************************************/

static int processCmdLine (msgDesc)
IMS_MSG_STRUCT *msgDesc;
{
	char buff[IMS_COL255_LEN+1];
	int invalid, i, number;
	char *Msg;
	/* Initialize */
	glbLogFileDir = (char *) NULL;
	glbLogin = (char *) NULL;
	glbPassword = (char *) NULL;
	invalid = 1;

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/
	
	/* define the work directory path */
	if ( (commands.workDir != (char *) NULL) &&
		  (strlen (commands.workDir) != 0 ))
	{
		glbWorkDir = commands.workDir;
	}
	else
	{
		glbWorkDir = DEF_WORK_DIRECTORY;
	}

	/* define the logfile name */
	if ((commands.logFile != (char *)NULL) && 
			(strlen (commands.logFile) != 0))
	{
		glbLogFile = commands.logFile;
	}
	else
	{
		glbLogFile = (char *)NULL;
	}

	/* define the log directory path */
	if ( (commands.logFileDir != (char *) NULL) &&
		  (strlen (commands.logFileDir) != 0 ))
	{
		glbLogFileDir = commands.logFileDir;
	}
	else
	{
		if (glbLogFile != (char *)NULL)
		{
			glbLogFileDir = DEF_LOG_DIRECTORY;
		}
		else
		{
			glbLogFileDir = (char *)NULL;
		}
	}

	invalid = 1;

	/* Check login information */
	/*
	** Modify later to request for a password when a login is specified 
	** in the command line.  All other login verifications must be supported 
	** through a special login popup-dialog window. 
	*/
	if (commands.login != (char *) NULL)
	{
		glbLogin = commands.login;
	}
/****
	else
	{
		do
		{
			if  (ims_getString(IMS_TRUE, buff, sizeof(buff),
				"login : ") == (char *)NULL) 
			{
				(void) ims_msg (msgDesc, IMS_FATAL, 
					"Error detected while reading input string.");
				return (IMS_FATAL);
			}
      
			ims_truncStr (buff);

			if (strlen(buff) != 0)
			{
				invalid = 0;
			}

		} while (invalid);

		glbLogin = malloc (strlen (buff) + 1);
		(void) strcpy (glbLogin, buff);
	}
*****/
	invalid = 1;

	if (commands.password != (char *) NULL)
	{
		glbPassword = commands.password;

	}
/****
	else
	{
		if ((glbLogin != (char *)NULL) && (strlen(glbLogin) != 0))
		{
			do
			{
				if (ims_getPassword (buff) == NULL)
				{
					return (IMS_FATAL);
				}

				ims_truncStr (buff);

				if (strlen (buff) != 0)
				{
					invalid = 0;
				}

			} while (invalid);

			glbPassword = malloc (strlen (buff) + 1);
			(void) strcpy (glbPassword, buff);
		}
	}

*****/

	return (IMS_OK);
}

/***************************************************************************
**
** usage ()
**
** Print command line argument switches.
**
***************************************************************************/

static void usage (void)
{
	int i;

	(void) fprintf (stderr, "\nims_op usage:\n\n");
	for (i = 0; i < cmdLineElmCount; i++)
	{
		(void) fprintf (stderr, "%s\n", cmdLineElm[i].paramKeyword);
	}
	(void) fprintf (stderr,"\n\n");
}

/***************************************************************************
**
** validateUser ()
**
***************************************************************************/

static int validateUser (OP_GLOBAL_DATA *glbData)
{
	OP_CAT_STRUCT catReq;
	OP_CAT_USERSPEC *userSpec;
	OP_USER_SPEC *glbUser;
	char query[IMS_COL255_LEN+1];
	DBSMALLINT user_type;
	int tmpint ;

	glbUser = &(glbData->userSpec);
	userSpec = &catReq.userSpec;

	userSpec->dbUserName = glbUser->userName;
	userSpec->dbPassword = glbUser->password;
	userSpec->server = glbUser->server;
	userSpec->dbName = glbUser->dbName;
	userSpec->program = glbData->program;
	catReq.msgDesc = glbData->msgDesc;
	catReq.qDesc = (IMS_QI_DESC_OBJ *) NULL;

	/*
	** Open database connection
	*/
	if (ims_opCat (&catReq, OP_OPENCONNECTION) < IMS_OK)
	{
		return (0);
	}
	else 
	{
		/*
		** Assign default operator type, for now.
		** Later, look up an access control list stored in the database
		** for operator user type and capabilities.
		*/

		/*
		** 1/5/96 - get user type from user_profile table.
		*/

		sprintf (query, "select user_type from user_profile "
										"where user_id = '%s'", glbUser->userName);

		catReq.item[0] = (char *)&query;
		catReq.item[1] = (DBSMALLINT *)&user_type;
		if (ims_opCat (&catReq, OP_GETUSERTYPE) < IMS_OK)
		{
			return (0);
		}

		glbUser->userType = user_type;
	
		/*
		** if user has no type defined in the user_profile table,
		** assign user_type GENERAL .
		*/
		if (glbUser->userType == OP_NOTYPE)
		{
			glbUser->userType = OP_GENERAL;
		}

		if (ims_opCat (&catReq, OP_GETCATALOGITEMS) < IMS_OK)
		{
			return (0);
		}

		if (ims_opCat (&catReq, OP_GETKEYWORDVALUES) < IMS_OK)
		{
			/* stderr msg & exit */
			tmpint = catReq.msgDesc->stderrFlag ;
			catReq.msgDesc->stderrFlag = 1 ;
			ims_msg( catReq.msgDesc, IMS_ERROR, 
				"Out of Memory: could not create Downlink Manager" ) ;
			catReq.msgDesc->stderrFlag = tmpint ;

			/* not an invalid user, so don't return an "invalid user" err, but EXIT */
			exit (IMS_FATAL);
		}

		(void) ims_opCat (&catReq, OP_CLOSECONNECTION);
		return (1);
	}
}
