static char *sccs = "@(#)ims_pmfTool.c	1.10 11/24/97";

/*****************************************************************************
**
**
** File:    ims_pmfTool.c
**
** Function: 
**			
**
** Author: Dan Crichton
**
** Date:    3/13/96
**
**
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <sys/signal.h>

#include <X11/Intrinsic.h>
#include <Xm/MainW.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <Xm/MenuShell.h>
#include <Xm/DialogS.h>
#include <Xm/BulletinB.h>
#include <Xm/MessageB.h>


#include <ims_dbms.h>
#include <ims_query.h> 
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <ims_cmd.h>
#include <syslog.h>
#include <odldef.h>
#include <ims_odl.h>
#include <ims_archive.h>

#include <ims_pmfTool.h>

/*
** Define some global widget ids
*/

XtAppContext app;
Widget app_shell;
XColor color;
Colormap cmap; 

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
	char *platform;
	char *sensor;
	char *dataset;
	char *filename;
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
	{"-h",           &commands.help},
	{"+help",        &commands.help},
	{"-r",           &commands.release},
	{"+release",     &commands.release},
	{"-L",           &commands.platform},
	{"+platform",    &commands.platform},
	{"-D",           &commands.dataset},
	{"+dataset",     &commands.dataset},
	{"-S",           &commands.sensor},
	{"+sensor",      &commands.sensor},
	{"-F",           &commands.filename},
	{"+filename",    &commands.filename},
	{"-C",			 &commands.commandFile},
	{"+commandFile", &commands.commandFile}

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
	{"dataset", 	&commands.dataset},
	{"platform",    &commands.platform},
	{"sensor",   	&commands.sensor},
	{"filename",    &commands.filename}
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);

IMS_OT_WIDGETS ims_otWidgets;
IMS_MSG_STRUCT *ims_ot_glbl_msg;
IMS_OT_USERSPEC ims_ot_userSpec;
IMS_ODL_TREE *ims_ot_tree;
IMS_ODL_TREE *ims_otFocusTree;
IMS_CLNT_EVENT ims_ot_request;
int data_type_count;
IMS_OT_CATALOG_ITEM data_type[IMS_COL10_LEN+1];
int significance_count;
IMS_OT_CATALOG_ITEM significance[IMS_COL10_LEN+1];

static int getArgInput (IMS_MSG_STRUCT *);
static void BuildLogon(Widget);
static void logon_query_cb(Widget, XtPointer, XtPointer);
static void quit_cb(Widget, XtPointer, XtPointer);
static int getCatalogItems (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *);
void ims_otMsg_box(Widget, char *);
static void valid_user_cb(Widget, XtPointer, XtPointer);
void check_passwd(Widget, XtPointer, XtPointer);

/*******************************************************************
** 
** main
**
*******************************************************************/
void main(int argc, char *argv[])
{
	char *programName;
	IMS_MSG_STRUCT *msgDesc;
	char hostName[IMS_COL30_LEN+1];
	char banner[IMS_COL80_LEN+1];
	struct utsname uname_info;
	int status;
	IMS_ODL_TREE *odl_tree = NULL;
	XColor exact;
	IMS_QI_DESC_OBJ *qDesc;

	Widget w_main_win, widget, scrollWin;
	
	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';  /* Null-terminate just in case */

	memset((char *) &ims_ot_userSpec, 0, sizeof(IMS_OT_USERSPEC));
	memset((char *) &ims_ot_request, 0, sizeof(IMS_CLNT_EVENT));
	
    /*
	** Initialize the message processor.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr,
				"Memory allocation for IMS_MSG_STRUCT structure failed.");
		exit (IMS_FATAL);
	}

    programName = ims_extractFileName (argv[0]);
	ims_ot_userSpec.program = programName;
		 
	  
	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgProgramName (msgDesc, programName);
	(void) sprintf (banner, "%s::%s", hostName, programName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

	ims_ot_glbl_msg = msgDesc;


	ims_otWidgets.headList = ims_otWidgets.tailList = NULL;

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

#if 0
	/*
	** If help has been specified, print usage and exit.
	*/
	if (commands.help != (char *) NULL)
	{
		usage ();
		(void) ims_msgStructFree (msgDesc);
		exit (0);
	}
#endif

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
	** Setup X stuff...
	*/

	memset((char *) &ims_otWidgets, 0, sizeof(ims_otWidgets));

	/*
	app_shell = XtVaAppInitialize(&app, programName, NULL,
		0, &argc, argv, NULL, NULL);
	*/

	app_shell = XtVaAppInitialize(&app, programName, NULL,
		0, &argc, argv, NULL, XmNwidth, 700,
		XmNheight, 800, XtNmappedWhenManaged, FALSE, NULL);
	

	if (app_shell == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not setup main window");
		exit(1);
	}

	/*
	** Setup some pretty colors...
	*/

	XtVaGetValues(app_shell, XmNcolormap, &cmap, NULL);

	XAllocNamedColor(XtDisplay(app_shell), cmap, "green",
		&(ims_otWidgets.bgPixel), &exact);

	XAllocNamedColor(XtDisplay(app_shell), cmap, "red",
		&(ims_otWidgets.mandPixel), &exact);


	/*
	** Allocate the main window widget.
	*/
	
	w_main_win = 
		XtVaCreateManagedWidget("mainw", xmMainWindowWidgetClass, 
		app_shell, XmNwidth, IMS_OT_MW_WID,
		XmNheight, IMS_OT_MW_HT,  XmNscrollingPolicy, XmAUTOMATIC,
		XmNscrollBarDisplayPolicy, XmCONSTANT,
		XmNallowShellResize, TRUE,
		NULL);

	ims_otWidgets.main_w = w_main_win;

	/*
	** Attach Menu Bar 
	*/

	if (ims_otBldMenuBar(msgDesc, w_main_win) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not setup main window");
		exit(1);
	}


	if (ims_otBldMsgWindow(msgDesc, w_main_win) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not setup main window");
		exit(1);
	}

	if (ims_otBldMainWin(msgDesc, w_main_win) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not setup main window");
		exit(1);
	}


	ims_msgQueueFlag(msgDesc, IMS_ON);


	/*
	** pop up the logon box if we are missing login information
	*/
	if ((ims_ot_userSpec.username == NULL) || 
			(ims_ot_userSpec.password == NULL) ||
			(ims_ot_userSpec.database == NULL) ||
			(ims_ot_userSpec.server == NULL))
	{
		BuildLogon(app_shell);
	}
	else
	{
		/* verify login */
    if ((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
		{
			(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not allocate a query descriptor.");
			ims_msgStructFree(msgDesc);
			exit(1);
		}
										 
		qDesc->cmd  = (char *) malloc(IMS_COL1024_LEN);

		IMS_SETUSER (qDesc, ims_ot_userSpec.username);
		IMS_SETPSWD (qDesc, ims_ot_userSpec.password);

		if (ims_ot_userSpec.server != NULL)
			IMS_SETSERVER(qDesc, ims_ot_userSpec.server);

		if (ims_ot_userSpec.database != NULL)
			IMS_SETDBNAME(qDesc, ims_ot_userSpec.database);
	
		if (ims_qiLogin(qDesc) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_FATAL,
					"Invalid login.");
			(void) ims_qiFreeDesc(qDesc);

			exit(1);
		}

		if (getCatalogItems(msgDesc, qDesc) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not cash data_type and significance values.");

			(void) ims_qiFreeDesc(qDesc);
			exit (1);
		}

		(void) ims_qiFreeDesc(qDesc);

		XtRealizeWidget(app_shell);
		XtMapWidget (app_shell);
	}

	/*
	** Let's cash data_type and significance values
	*/
/*
	if (openConnection(msgDesc, (IMS_QI_DESC_OBJ **) &qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not open connection to database server");

		ims_msgStructFree(msgDesc);

		exit (1);
	}
	
	if (getCatalogItems(msgDesc, qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not cash data_type and significance values.");

		(void) ims_qiFreeDesc(qDesc);
		exit (1);
	}
*/

	/*
	** Load an existing or new ODL file.
	*/

	odl_tree = NULL;
	ims_ot_request.platform = commands.platform;
	ims_ot_request.sensor = commands.sensor;
	ims_ot_request.dataset = commands.dataset;

	if (commands.filename != (char *) NULL)
	{
		odl_tree = NULL;
		printf("Loading ODL File....%s\n", commands.filename);
    	if (ims_ODLFileToTree (msgDesc, commands.filename, &odl_tree) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not load ODL file.");
		}
		else
		{
			ims_otAddODLData(odl_tree);
			(void) ims_otShowObject(msgDesc, ims_otWidgets.work_w, odl_tree, 0);
			ims_ot_userSpec.saveFile = commands.filename;
		}
	}

	else if (commands.dataset != (char *) NULL)
	{
		(void) ims_otBldCmnHdr(msgDesc, ims_otWidgets.work_w, 
			&odl_tree);
		(void) ims_otBldKwList(msgDesc, odl_tree, 
			commands.platform, commands.sensor, commands.dataset);
		
		(void) ims_otShowObject(msgDesc, ims_otWidgets.work_w, odl_tree, 0);


	}
	ims_ot_tree = odl_tree;
	ims_otFocusTree = NULL;

	XtAppMainLoop(app);

	exit(0);

}

/*******************************************************************
** 
** ims_otMsg
**
*******************************************************************/

int ims_otMsg(
	IMS_MSG_STRUCT *msgDesc)
{
	
	IMS_MSG_QUEUE *msgQueue;
	XmString xm_str;
	char *ptr, *ptr2, *tempStr;
	int ipos = 0;

	
	/*
	** Extract to screen.
	*/

	while ((msgQueue = ims_msgQueueExtract(msgDesc)) != 
		(IMS_MSG_QUEUE *) NULL)
	{

		ptr2 = ptr = msgQueue->msg;
		ipos = 0;

		while (*ptr2 != '\0')
		{
			ptr2 = ptr;
			while ((*ptr2 != '\n') && (*ptr2 != '\0'))
				ptr2 ++;

			tempStr = (char *) malloc(ptr2 - ptr + 1);
			strncpy(tempStr, ptr, ptr2 - ptr);
			tempStr[ptr2-ptr] = '\0';
			
			
			xm_str =  XmStringCreateLocalized(tempStr);

			ipos ++; /* Position in list.  Relative to top of list . */

			XmListAddItemUnselected (ims_otWidgets.msg_w, xm_str, ipos);
			XmStringFree(xm_str);
			free(tempStr);
			ptr = ptr2 + 1;


		}
		ims_msgQueueFree(msgQueue);
	}

	return(IMS_OK);

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

	/* username */
	if (commands.username != (char *) NULL)
	{
		ims_ot_userSpec.username = commands.username;
	}

	/* password */
	if (commands.password != (char *) NULL)
	{
		ims_ot_userSpec.password = commands.password;
	}

	/* server */
	if (commands.server != (char *) NULL)
	{
		ims_ot_userSpec.server = commands.server;
	}
	else 
		if (getenv("IMS_SERVER") != NULL)
		{
			ims_ot_userSpec.server = getenv("IMS_SERVER");
		}

	/* database */
	if (commands.database != (char *) NULL)
	{
		ims_ot_userSpec.database = commands.database;
	}
	else
		if (getenv("IMS_DB") != NULL)
		{
			ims_ot_userSpec.database = getenv("IMS_DB");
		}

	return (IMS_OK);
}


/*******************************************************************
** 
** BuildLogon
**
*******************************************************************/

static void BuildLogon(
		Widget wshell)
{
	Widget bb, pshell, submit_button, quit_button, Msgtextlabel;
	XmString t;
	Arg args[4];
	XmString tstr;
	char msg[255];
	XmString title;
	Widget w_username, w_password, w_database, w_server;
	Widget textlabel1, textlabel2, textlabel3;
	int i;
	Display *dpy;
	XFontStruct *font;
	XmFontList fontlist;



	/*
	** Create dialog box tiled just below Upper-Left 
	** of other dialog box.
	*/

	XtSetArg(args[0], XmNx, 312);
	XtSetArg(args[1], XmNy, 284);
	XtSetArg(args[2], XmNwidth, 375);
	XtSetArg(args[3], XmNheight, 250);

	pshell = XtCreatePopupShell("ims_pmfTool",  transientShellWidgetClass,
		wshell, args, 4);

	bb = XtVaCreateManagedWidget("Logon", xmBulletinBoardWidgetClass,
		pshell, args, 4, NULL);



	/*
	** Add USERNAME
	*/


	t = XmStringCreateSimple("Username:");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNlabelString, t,
		XmNx, 40,
		XmNy, 30, NULL);
	XmStringFree(t);


	w_username = XtVaCreateManagedWidget("text", xmTextWidgetClass,
			bb, XmNeditable, TRUE,
			XmNwidth, 150,
			XmNmaxLength, 10, XmNx, 40+ 80,
			XmNy, 30,  NULL);


	XtAddCallback(w_username,  XmNmotionVerifyCallback,
														 valid_user_cb, (XtPointer) 0); 

	XtAddCallback(w_username,  XmNactivateCallback, 
				logon_query_cb, (void *)pshell);

	XmTextSetString(w_username, ims_ot_userSpec.username);

	/*
	** Add PASSWORD
	*/

	t = XmStringCreateSimple("Password:");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNlabelString, t,
		XmNx, 40,
		XmNy, 60, NULL);

	w_password = XtVaCreateManagedWidget("text", xmTextWidgetClass,
			bb, XmNeditable, TRUE,
			XmNwidth, 150,
			XmNmaxLength, 10, XmNx, 40 + 80,
			XmNy, 60,  NULL);

	XmStringFree(t);

	/*
	XtAddCallback(w_password,  XmNmotionVerifyCallback,
														 valid_user_cb, (XtPointer) 1);
	*/
	XtAddCallback(w_password,  XmNmodifyVerifyCallback, 
														 check_passwd, (XtPointer) 0);

	XtAddCallback(w_password,  XmNactivateCallback, 
				logon_query_cb, (void *)pshell);

	XmTextSetString(w_password, ims_ot_userSpec.password);

	/*
	** Add SERVER
	*/

	t = XmStringCreateSimple("Server:");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNlabelString, t,
		XmNx, 40,
		XmNy, 90, NULL);
	XmStringFree(t);

	w_server = XtVaCreateManagedWidget("text", xmTextWidgetClass,
			bb, XmNeditable, TRUE,
			XmNwidth, 150,
			XmNmaxLength, 30, XmNx, 40 + 80,
			XmNy, 90,  NULL);

	XtAddCallback(w_server,  XmNmotionVerifyCallback,
													 valid_user_cb, (XtPointer) 2); 

	XtAddCallback(w_server,  XmNactivateCallback, 
				logon_query_cb, (void *)pshell);

	XmTextSetString(w_server, ims_ot_userSpec.server);

	/*
	** Add Database
	*/

	t = XmStringCreateSimple("Database:");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNlabelString, t,
		XmNx, 40,
		XmNy, 120, NULL);

	w_database = XtVaCreateManagedWidget("text", xmTextWidgetClass,
			bb, XmNeditable, TRUE,
			XmNwidth, 150,
			XmNmaxLength, 30, XmNx, 40 + 80,
			XmNy, 120,  NULL);

	XmStringFree(t);

	XtAddCallback(w_database,  XmNmotionVerifyCallback, 
														 valid_user_cb, (XtPointer) 3);

	XtAddCallback(w_database,  XmNactivateCallback, 
				logon_query_cb, (void *)pshell);

	XmTextSetString(w_database, ims_ot_userSpec.database);

	/*
	** Setup buttons...
	*/

	submit_button = XtVaCreateManagedWidget(" LOGON ", 
			xmPushButtonWidgetClass,
			bb, XmNx, 50, XmNy, 160, 
			NULL);

	quit_button = XtVaCreateManagedWidget(" CANCEL ", 
			xmPushButtonWidgetClass,
			bb, XmNx, 240, XmNy, 160, NULL);

	XtAddCallback(submit_button,  XmNactivateCallback, 
			logon_query_cb, (void *)pshell);

	XtAddCallback(quit_button, XmNactivateCallback, 
			quit_cb, NULL);


    /*
	** Create a special font for the copyright.
	*/
			 
	dpy = XtDisplay(wshell);

	font = XLoadQueryFont(dpy, "-adobe-times-medium-r-normal--11-80-100-100-p-54-iso8859-1");
	fontlist = XmFontListCreate(font, "charset1");

	/*
	** Add copyright notice
	*/
										
	t = XmStringCreate("Copyright (C) 1996, California Institute of Technology.  U.S. Government", "charset1");

	(void) XtVaCreateManagedWidget("label",
		xmLabelWidgetClass, bb,
		XmNlabelString, t,
		XmNfontList, fontlist,
		XmNx, 25,
		XmNy, 200, NULL);
	XmStringFree(t);

	t = XmStringCreate("Sponsorship under NASA Contract NAS7-1260 is acknowledged.", "charset1");

	(void) XtVaCreateManagedWidget("label",
		  xmLabelWidgetClass, bb,
		  XmNlabelString, t,
		  XmNfontList, fontlist,
		  XmNx, 45,
		  XmNy, 215, NULL);

	XmStringFree(t);

	XtManageChild(pshell);
	XtRealizeWidget(wshell);


	XtPopup(pshell, XtGrabExclusive);

	/*
	** Set focus to w_username text widget
	*/

	XmProcessTraversal(w_username, XmTRAVERSE_CURRENT);

}

/*******************************************************************
** 
** valid_user_cb
**
*******************************************************************/

static void valid_user_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	char c;
	int len;
	XmTextVerifyCallbackStruct *cbs;
	int glbl_data;
	char *tmp_data;

	glbl_data = (int) client_d;
	tmp_data = XmTextGetString(w);

	switch (glbl_data)
	{
		case 0:
			ims_ot_userSpec.username = tmp_data;
		break;
		case 1:
			ims_ot_userSpec.password = tmp_data;
		break;
		case 2:	
			ims_ot_userSpec.server = tmp_data;
		break;
		case 3:	
			ims_ot_userSpec.database = tmp_data;
		break;
		default:
		break;
	}

}


/*******************************************************************
** 
** logon_query_cb
**
** Callback for logon
*******************************************************************/

static void logon_query_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	IMS_QI_DESC_OBJ *qDesc;

	/*
	** Attempt to logon
	*/
    if ((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
			(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not allocate a query descriptor.");
			ims_msgStructFree(msgDesc);
			exit(0);
	}
										 
	qDesc->cmd  = (char *) malloc(IMS_COL1024_LEN);

	/*
	** Setup login parameters.
	*/
	if (ims_ot_userSpec.username != NULL)
	{
		IMS_SETUSER (qDesc, ims_ot_userSpec.username);
	}
	if (ims_ot_userSpec.password != NULL)
	{
		IMS_SETPSWD (qDesc, ims_ot_userSpec.password);
	}

	if (ims_ot_userSpec.server != NULL)
		IMS_SETSERVER(qDesc, ims_ot_userSpec.server);

	if (ims_ot_userSpec.database != NULL)
		IMS_SETDBNAME(qDesc, ims_ot_userSpec.database);
	
	if (ims_qiLogin(qDesc) < IMS_OK)
	{
		ims_otMsg_box(w, "Unable to Login.");
		ims_qiFreeDesc(qDesc);
		return;
	}

	if (getCatalogItems(msgDesc, qDesc) < IMS_OK)
	{
		XtPopdown((Widget) client_d);
		XtDestroyWidget ((Widget) client_d);

		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not cash data_type and significance values.");
		(void) ims_qiFreeDesc(qDesc);

		exit (1);
	}

	ims_qiFreeDesc(qDesc);
	
	XtPopdown((Widget) client_d);
	XtDestroyWidget ((Widget) client_d);
	XtMapWidget ((Widget) XtParent(client_d));
	
}

/*******************************************************************
** 
** quit_cb
**
*******************************************************************/
static void quit_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;

	ims_msgStructFree(msgDesc);
	exit(0);
}


/*******************************************************************
** 
** ims_otMsg_box
**
*******************************************************************/
void ims_otMsg_box(
Widget parent,
char *text)
/* generates an error dialog box */
{
  Widget dialog;
  Arg arg[2];
  XmString t;
  Display *dpy;
  
  t=XmStringCreateSimple(text);
  XtSetArg(arg[0],XmNmessageString,t);
  XtSetArg(arg[1],XmNdialogStyle,XmDIALOG_PRIMARY_APPLICATION_MODAL);
  dialog = XmCreateWarningDialog(parent,(void *) "Message",arg, (Cardinal) 2);
  XtUnmanageChild(XmMessageBoxGetChild(dialog,XmDIALOG_HELP_BUTTON));
	/*
  XtSetSensitive(XmMessageBoxGetChild(dialog,XmDIALOG_HELP_BUTTON),False);
	*/
  XtUnmanageChild(XmMessageBoxGetChild(dialog,XmDIALOG_CANCEL_BUTTON));
	/*
  XtSetSensitive(XmMessageBoxGetChild(dialog,XmDIALOG_CANCEL_BUTTON),False);
	*/
  XmStringFree(t);
  XtManageChild(dialog); 

  dpy = XtDisplay(parent);
  XFlush(dpy);

}


/*******************************************************************
** 
** check_passwd
** 
** by Dan Heller
**
*******************************************************************/
void check_passwd(
Widget        text_w,
XtPointer     client_data,
XtPointer     call_data)
{
  char *new, *password;
  int len;
  XmTextVerifyCallbackStruct *cbs = 
     (XmTextVerifyCallbackStruct *) call_data;

	int glbl_data = (int)client_data;

	if (client_data)
		password = ims_ot_request.password;
	else
		password = ims_ot_userSpec.password;

  if (cbs->startPos < cbs->currInsert) 
	{ /* backspace */
    cbs->endPos = strlen (password);
    password[cbs->startPos] = 0;
    return;
  }

  if (cbs->text->length > 1) 
	{
    cbs->doit = False;  /* don't allow "paste" operations */
    return;             /* make the user *type* the password! */
  }

  new = XtMalloc (cbs->endPos + 2); /* new char + NULL terminator */
  if (password) 
	{
    strcpy (new, password);
    XtFree (password);
  }
	else
    new[0] = NULL;

  password = new;
  strncat (password, cbs->text->ptr, cbs->text->length);
  password[cbs->endPos + cbs->text->length] = 0;

  for (len = 0; len < cbs->text->length; len++)
    cbs->text->ptr[len] = '*';

	if (client_data)
		ims_ot_request.password = password;
	else
		ims_ot_userSpec.password = password;

}

/*******************************************************************
** 
** getCatalogItems
**
*******************************************************************/
static int getCatalogItems(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc)
{
	int status; 
	int k, rowCount;

	/*
	** Get data type valids
	*/
	strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'data_type' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(data_type[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((data_type[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		data_type[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (data_type[rowCount].item_name);
		ims_toUpper (data_type[rowCount].item_name);

		/* a row is returend */
		rowCount += 1;

	}
	data_type_count = rowCount;


	/*
	** Re-initialize query descriptor for next command, but do
	** not cancel previous command
	*/
	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"getCatalogItems: Could not reinitialize query descriptor.");
		return(IMS_FATAL);
	}


	/*
	** Get significance valids
	*/
	strcpy (qDesc->cmd,
		"select instance, description from items "
		"where type = 'significance' order by instance");

	rowCount = 0;
	while ((status = ims_qiNextRow (qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK) 
		{
			return (status);
		}
		if (status == IMS_ENDOFQUERY) continue;

		/* copy in the returned data */
		(void) memcpy (&(significance[rowCount].item_id),
			qDesc->valAddr[0], qDesc->valLength[0]);

		(void) memcpy ((significance[rowCount].item_name),
			qDesc->valAddr[1], qDesc->valLength[1]);
		significance[rowCount].item_name[qDesc->valLength[1]] = '\0';
		ims_truncStr (significance[rowCount].item_name);
		ims_toUpper (significance[rowCount].item_name);

		/* a row is returend */
		rowCount += 1;

	}
	significance_count = rowCount;

	return (IMS_OK);
}
