

#ifndef lint
static char sccsid[] = "@(#)log_main.c	1.1  12/19/96";
#endif


#ifdef MODULE_HDR
/* ***************************************************************************

Project:		NOCC-RT
Program Set:	NUI - NOCC User Interface
Assembly:		N/A
Task:			N/A
Module:			log_main.c
Title:			Main Module routine for Ops Log Browser.
------------------------------------------------------------------------------
Modification History:

	Date			By				Description
------------------------------------------------------------------------------
12/14/90		Bill West		Initial Delivery
08/13/93		Mike Tankenson	Added calls to syslog()
------------------------------------------------------------------------------

Module Overview:	Creates and manages the main widgets needed for the 	
					Ops Log Browser.

Procedures/Functions called from other modules:
	void	CreateAlertPopup() ;
	void	CreateBrowseModePopup() ;
	void	SetLogResources() ;
	void	GetCurrentLog() ;
	void	CreateLogPopup() ;
	void	OpenLog() ;
	void	CloseLog() ;
	void	SetFilterResources() ;
	void	CreateFilterPopup() ;
	void	CreateList() ;
	void	FillList() ;
	void	UpdateList() ;
	void	CreateMenuOptions() ;
	void	CreatePrintPopup() ;
	void	CreateSoundPopup() ;
	void	CreateStatusBar() ;


Procedures/Functions called in other modules:
	void	LogErrrorMessage() ;
	void	DisplayErrrorMessage() ;
	void	FinalCleanupCB() ;

*************************************************************************** */
#endif

/*	=========================	INCLUDES	=========================	*/

/* Standard Includes */
#include "log_main.h"

/* X & Motif Includes */
#include <X11/Xlib.h>
#include <Xm/Protocols.h>
#include <Xm/Form.h>
#include <Xm/MainW.h>
#include <Xm/MessageB.h>
#include <Xm/RowColumn.h>

/* Log Browser Includes */
#include "log_resrc.h"
#include "log_icon.h"
#include <syslog.h>

/*  =========================  DEFINES  =========================  */

#define	MAX_ERR_MSG		100
#define	APPL_CLASS_NAME	"Log_browser"

/*  =========================  GLOBAL DATA  =========================  */

		resource_t		Resources ;
		Widget	 		TopLevel = (Widget) NULL ;
static	Widget			messageWidget ;

/*	=========================  Prototypes	=========================  */

static	void	setWMHints() ;
static  Widget  createMainWindow(/* parent */) ;
static  Widget  createMenuBar(/* parent */) ;
static  Widget  createForm(/* parent */) ;
static	void	createMessagePopup(/* parent */) ;

static char Usage[] = {
	"\nUsage: log_browser -log <logFileSpec> [-options]\n\
    [-filter <str>] (Contains 1 or 2 filters separated by '&&' or '||')\n\
    [-warn <str>] (Substring to indicate a warning)\n\
    [-crit <str>] (Substring to indicate a critical entry)\n\
    [-endlog <str>] (Contains 1 or 2 end-of-log markers w/ '&&' or '||')\n\
    [-debug] (debug mode)\n\
    [-delay <milliseconds>] (Refresh time in milliseconds, default=1000)\n\
    [-sound | -silent] (Toggles sound, default=silent)\n\
    [-browse | -update] (Toggles mode, default=update)\n\
    [-first | -last] (Defines 'current' log in ordered list, default=last)\n\
    [Xt toolkit arguments (i.e. -iconic)]\n\
    [-help (print usage message)]\n\
    \nFor details, try 'man log_browser'\n\n"
};


/*	=========================  Functions	=========================  */

#ifdef TASK_HDR
/* ***************************************************************************

Task Description: 	Reads and displays any log files containing text only.

Assumptions and Constraints:	
    If current local log data is to be viewed, the NUI Ops Log
	Handler (nui_log) must be running.

Start-up:	Console manager pulldown menu entry which invokes nui_browser with
			various command line options which specify what logs to read.

Termination:	Window Manager "Close" menu option or Alt-F4.
	Also, a pulldown menu sports a "Quit" button which does the same.

Invocation Arguments:
	-log <str>			Path+Log name for the log files.  Option is required.
	[-filter <str>]		Contains 1 or 2 filters separated by " && " or " || ".
	[-endlog <str>]		Contains 1 or 2 end-of-log markers w/ " && " or " || ".
	[-warn <str>]		Defines what substring must be present to indicate a warning.
	[-crit <str>]		Defines substring for a critical entry.
	[-debug]			If specified, send error messages to stdout.
	[-delay]			Specifies the refresh time in milliseconds.  Default=1000.
	[-sound|silent]		Sets sound initially on|off.  Default is silent.
	[-browse|update]	Sets initial browse mode.  Default is update.
	[-first|last]		Defines which log file in an alphebetically ordered
						list is considered the "current" one.  Default is last.
	[Xt toolkit arguments]		Standard toolkit parameters such as -iconic
	

Task Decompostion:                        log_main.c
                                             |
           o-------o--------o-------o--------o-------o--------o--------o-------o
           |       |        |       |        |       |        |        |       |
        log_alert  |     log_browse |     log_file   |     log_filter  |    log_list 
                log_menu         log_print        log_wound         log_status 

Use of COTS Software:	none.

Use of Disk Files:	

	System Log:	/nocc/nfs/log/<type>_log.ddd.ii	
					where	<type> is either 
							"external" or "nocc",
							ddd is the GMT day of year.
							ii is an index from 00 to 99.

	Local Log:	/nocc/log/NUI_Log.yyddd[.ii] 
					where	yy is the GMT year mod 100.
							ddd is the GMT day of year.
							ii is an optional index for log 
								file addendums.

Libraries Used:	nui2, Xm, Xt, X11

Notes:	none.

*************************************************************************** */
#endif

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:	main(argc, argv)


Description:	Main driver program for the Ops Log Browser.

Arguments:
	int argc -- number of command line arguments.
	char *argv[] -- command line arguments.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:	Send startup entry to Ops Log Handler (log_handler.c).

		Initialize X Windows / Motif.

		Create the following widget tree:

                                 TopLevel
                                    |
                                MainWindow
                    ________________|_____________
                   |                              |
                MenuBar                          Form
     ______________|_________                   __|_____________
    |            |           |                 |                |
PushButton  CascadeMenu  FileSelection     ScrolledList      RowColumn
                /|\          |                                 /|\
       PushButtonGadgets InformationDialog                   Labels


		Get the initial log file
	
		Read the initial log file, appending all entries to 
			Scrolled list.

		Prepare to be notified of new log entries.

		Enter X Windows Main Loop.


*************************************************************************** */
#endif
main(argc, argv)
int		  argc ;
char	**argv ;
{
	Widget	mainWindow, mainForm, menuBar, statusBar ;
	char	msg[200+MAX_FILE_NAME_LEN] ;

	if(argc < 3)
		goto usage;
	if((strcasecmp(argv[1],"-HELP")) == 0)	
		goto usage;

	/* Register via syslog */
	openlog("ASF_SYSTEM",LOG_PID|LOG_NDELAY,LOG_LOCAL0);
	syslog(LOG_ERR,"(ASF Log Browser) up and running.") ;

	/*  Do some X initializing  */
  	TopLevel = XtInitialize(APPL_CLASS_NAME, APPL_CLASS_NAME,
						  	options, XtNumber(options),
						  	&argc, argv);

  	XtGetApplicationResources(TopLevel, &Resources, resources,
						XtNumber(resources), NULL, 0);

  	/* Set the Window Manager Hints and delete window protocols */
  	setWMHints();

	/* =================================================================== */


	/*  Set up the main window, menubar and form work area  */
	mainWindow = createMainWindow(TopLevel) ;
	menuBar = createMenuBar(mainWindow) ;
	mainForm = createForm(mainWindow) ;
	XmMainWindowSetAreas(mainWindow, menuBar, NULL, NULL, NULL, mainForm) ;

	/*  Create the pulldown menus as children of the menu bar  */
	CreateMenuOptions(menuBar) ;

	/*  Set up the scrolled list and status window as children of the form  */
	statusBar = CreateStatusBar(mainForm) ;
	CreateList(mainForm, statusBar) ;

	/*  Create the two Pop Up dialogs that might be needed immediately  */
	createMessagePopup(mainWindow) ;
	CreateAlertPopup(mainWindow) ;

	/*  Display our new creation  */
	XtManageChild(mainWindow) ;
	XtRealizeWidget(TopLevel) ;


	/*  Make sure all gets created and managed before going on.
	 *	This will ensure that any pop-up windows will be a 
	 *	reasonable size, etc.
	 */
	XmUpdateDisplay(TopLevel) ;

	/*  Set up each of the sub modules that need it  */
	SetLogResources() ;
	SetFilterResources() ;

	/*  Create the file selection box and other dialog boxes  */
	CreateBrowseModePopup(mainWindow) ;
	CreateLogPopup(mainWindow) ;
	CreateFilterPopup(mainWindow) ;
	CreatePrintPopup(mainWindow) ;
	CreateSoundPopup(mainWindow) ;

	/*  Get the name of the initial log file  */
	if (GetCurrentLog() == True) 
	{
		/*  Open the log  */
		if (OpenLog() == True)
		{
			/*  Update the display with the new log's data  */
			FillList() ;
			/*  Arrange for the log to update  */
			SetListUpdateTimer() ;
		}
		else
		{
			sprintf(msg, "Unable to open current log file (%s%s).",
					LogPath, LogName) ;
			LOG_ERROR(msg) ;
			PopupMessage(msg) ;
		}
	}
	else
	{
		sprintf(msg, "No log files exist for specified log (%s%s).",
				LogPath, LogSpec) ;
		LOG_ERROR(msg) ;
		PopupMessage(msg) ;
	}
			
	UpdateStatusBar() ;

	/* =================================================================== */

	/*  Enter the Xt main loop to handler user and timeout events  */
	XtMainLoop() ;

	/*	Exit nicely  */
	FINAL_CLEANUP(LOG_EXIT_OK) ;

usage:
	fprintf(stderr, Usage);

	/*	Exit nicely  */
	FINAL_CLEANUP(LOG_EXIT_OK) ;

}  /* main  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:	void 	FinalCleanupCB(w, exitCode, call_data) ;


Description:	Handles shutdown from the following reasons:
			Kill (or similar) signal.
			Exit call.
			MWM Close Window.
			Shutdown client message.

Arguments:
	Widget		w -- unused.
	int			exitCode -- the code to exit the program with.
	XtPointer	call_data -- unused.

Return Value:	none.

Messages/Data Stores:	Global Log file, NUI Shared Memory.

Notes/Constraints:	none.

Pseudo Code:
		Send shutdown log message to Ops Log Handler.
		Close Log.
		Close X Windows display.
		Exit.

*************************************************************************** */
#endif
void 	FinalCleanupCB(w, exitCode, call_data)
Widget		w;
int			exitCode ;
XtPointer	call_data;
{
	/*  Keep any further updates from happening  */
	UnsetListUpdateTimer() ;

	syslog(LOG_ERR,"(ASF Log Browser) shutting down.") ;

	/* Close the syslog log */
	closelog();

	/* Close Log.  */
	if (LOG_SELECTED)
		CloseLog() ;

	/* Close X Windows display.  */
	if (TopLevel)
	  	XCloseDisplay (XtDisplay (TopLevel));

	/* Exit.  */
  	exit (exitCode);

}	/*  FinalCleanupCB  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:	static void	setWMHints () ;

Description:	Sets up MWM Window Manager parameters.

Arguments:	none.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:	Set up MWM Window Manager parameters.
		Prepare to handle the MWM Close Window.

*************************************************************************** */
#endif

static void	setWMHints()
{
	int			 i = 0;
	Display		*dpy;
	Atom		wm_delete_window;
	Arg			myArgs[20];
	Pixmap		icon_pix ;

	dpy = XtDisplay(TopLevel);

	XtSetArg(myArgs[i], XmNwidth, 500); i++;
	XtSetArg(myArgs[i], XmNheight, 300 ); i++; 
	XtSetArg(myArgs[i], XmNmaxHeight, 700); i++;
	XtSetArg(myArgs[i], XmNmaxWidth, 1000); i++;
	XtSetArg(myArgs[i], XmNminHeight, 150); i++;
	XtSetArg(myArgs[i], XmNminWidth, 300); i++;
	XtSetArg(myArgs[i], XmNsaveUnder, True); i++;
	XtSetArg(myArgs[i], XmNshellUnitType, XmPIXELS); i++;
	XtSetArg(myArgs[i], XmNtitle, "ASF Log Browser"); i++;

#ifdef COMMENT
	icon_pix = XCreateBitmapFromData(dpy, dpy->screens[dpy->default_screen].root,
										log_icon_bits,
										log_icon_width, log_icon_height) ;
	XtSetArg(myArgs[i], XmNiconPixmap, icon_pix); i++;
#endif /* COMMENT */

	XtSetValues(TopLevel, myArgs, i);

	wm_delete_window = XmInternAtom(dpy, "WM_DELETE_WINDOW", False);

	/* Add some processing before terminating */
	XmAddWMProtocolCallback(TopLevel, wm_delete_window, 	
							FinalCleanupCB, (XtPointer) LOG_EXIT_OK);

}	/* setWMHints	*/

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:	void LogErrorMessage (msg) ;

Description:	Handles error message reporting.

Arguments:
		msg --		Raw error message.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
		Prepends the program name to the message.
		Sends the error message to the Ops Log Browser.
		If the debug resource is set, 
			send the message to stdout.

*************************************************************************** */
#endif

void	LogErrorMessage(msg)
char		*msg ;
{
	char	message[MAX_ERR_MSG+1] ;
	int		len ;

	strcpy(message,APPL_CLASS_NAME) ;
	strcat(message,":  ") ;
	len = MAX_ERR_MSG - strlen(message) ;
	strncat(message,msg,len) ;
	message[MAX_ERR_MSG] = '\0' ; 			/* force an end-of-string */

	if (DEBUG_MODE)
		puts(message) ;						/*  write to stdout  */

	puts(message) ; /* -- changed by M.T.  10aug93 */

}  /*  LogErrorMessage  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:	static	void createMainWindow () ;

Description:	Creates the main window of the application.

Arguments:	none.

Return Value:	The MainWindow widget.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	Set the resources for and create the MainWindow widget.

*************************************************************************** */
#endif

static  Widget  createMainWindow(parent)
Widget	parent ;
{
	Arg		myArgs[20] ;
	int		i ;
	Widget	mainWindow ;

	/*  create the main window on which everything will reside  */
	i = 0 ;
	XtSetArg(myArgs[i], XmNshowSeparator, True) ; i++ ;
	mainWindow = XmCreateMainWindow(parent, "MainWindow", myArgs, i) ;

	return(mainWindow) ;

}	/*  createMainWindow  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:	static	void createMenuBar () ;

Description:	Creates the menu bar which sits on the main window.

Arguments:	none.

Return Value:	The menu bar widget.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	Set the resources for and create the menu bar widget.

*************************************************************************** */
#endif

static  Widget  createMenuBar(parent)
Widget	parent ;
{
	Arg		myArgs[20] ;
	int		i ;
	Widget	menuBar ;

	/*  create the menu bar  */	
	menuBar = XmCreateMenuBar(parent,"MenuBar", NULL, 0) ;
	XtManageChild(menuBar) ;

	return(menuBar) ;

}	/*  createMenuBar  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:	static	void createForm () ;

Description:	Creates the main form which sits on the main window.

Arguments:	none.

Return Value:	The Form widget.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	Set the resources for and create the Form widget.

*************************************************************************** */
#endif

static  Widget  createForm(parent) 
Widget	parent ;
{
	Arg		myArgs[20] ;
	int		i ;
	Widget	mainForm ;

	/*  create the main window on which everything will reside  */
	i = 0 ;
	XtSetArg(myArgs[i], XmNresizePolicy, XmRESIZE_NONE) ; i++ ;
	XtSetArg(myArgs[i], XmNallowOverlap, True) ; i++ ;
	mainForm = XmCreateForm(parent, "MainForm", myArgs, i) ;
	XtManageChild(mainForm) ;

	return(mainForm) ;

}	/*  createForm  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:	static	void createMessagePopup () ;

Description:	Creates a general purpose dialog box to inform the user
		        of some condition.

Arguments:	none.

Return Value:	none.

Messages/Data Stores:	Initializes the global widget messageWidget.

Notes/Constraints:	none.

Pseudo Code:
	Set the resources for and create the popup dialog.
	(Dialog is auto-unmanaged <popped down>).

*************************************************************************** */
#endif

static	void	createMessagePopup(parent)
Widget	parent ;
{
	Arg			myArgs[20] ;
	int			i = 0 ;
	XmString	buttonLabel, dialogTitle ;
	Widget		helpButton, cancelButton ;

	/*  set the resources for the popup  */
	dialogTitle = XmStringCreateSimple("Log Browser Message") ;
	buttonLabel = XmStringCreateSimple(" acknowledged... ") ;

	XtSetArg(myArgs[i], XmNdialogTitle, dialogTitle) ; i++ ;
	XtSetArg(myArgs[i], XmNokLabelString, buttonLabel) ; i++ ;
	/*  The following is commented out, because sometimes, when
	 *	the server is extra-busy, the pop up comes up too small
	 *	and the user has no way to press the Acknowledge button.
	 *	Why?  Something to do with Motif's slick geometry management
	 *	I guess.
	 */
/*
	XtSetArg(myArgs[i], XmNnoResize, True) ; i++ ;
*/
	
	/*  create the popup  */
	messageWidget = XmCreateWarningDialog(parent, "LogBrowserMessage",
											myArgs, i) ;

	/*  take away unnecessary buttons  */
	helpButton = XmMessageBoxGetChild(messageWidget, XmDIALOG_HELP_BUTTON) ;
	cancelButton = XmMessageBoxGetChild(messageWidget, XmDIALOG_CANCEL_BUTTON) ;
	if (helpButton)
		XtUnmanageChild(helpButton) ;
	if (cancelButton)
		XtUnmanageChild(cancelButton) ;


	/*  free the strings  */
	XmStringFree(dialogTitle) ;
	XmStringFree(buttonLabel) ;

}	/*  createMessagePopup  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:	void PopupMessage (msg) ;

Description:	Pops up a general purpose dialog box to inform the user
		        of some condition.

Arguments:
		msg --		Error message.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	Apply the msg string to the pop up dialog, and manage it.
	(Dialog is auto-unmanaged <popped down>).

*************************************************************************** */
#endif

void	PopupMessage(msg)
char	*msg ;
{
	Arg			myArgs[20] ;
	int			i = 0 ;
	XmString	string ;

	string = XmStringLtoRCreate(msg, XmSTRING_DEFAULT_CHARSET) ;

	XtSetArg(myArgs[i], XmNmessageString, string) ; i++ ;
	XtSetValues(messageWidget, myArgs, i) ;

	XtManageChild(messageWidget) ;

	XmStringFree(string) ;

}  /*  PopupMessage  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
	int		FontHeight(w) 

Description:	Returns the height in pixels of the default font for the 
				widget w.

Arguments:  w -- widget in question.

Return Value:	height of font.

Messages/Data Stores:	none.

Notes/Constraints:  none.

Pseudo Code:	create a single blank string.
				get the default font list
				get and return the font height.

*************************************************************************** */
#endif

int     FontHeight(w)
Widget	w ;
{
	XmString	blank ;
	XmFontList	font ;
	int			height ;

	/*  create a blank string for getting the height of the labels  */
	blank = XmStringCreateSimple(" ") ;

	/*  get the default font list for this widget  */
	XtVaGetValues(w,
				XmNfontList,    &font,
				NULL) ;

	/*  get the font's height  */
	height = (int) XmStringHeight(font, blank) ;

	/*  free up the string  */
	XmStringFree(blank) ;

	/*  return the height  */
	return(height) ;

}	/*  FontHeight  */



#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
	int		FontWidth(w) 

Description:	Returns the Width in pixels of the default font for the 
				widget w.

Arguments:  w -- widget in question.

Return Value:	Width of font.

Messages/Data Stores:	none.

Notes/Constraints:  none.

Pseudo Code:	create a single blank string.
				get the default font list
				get and return the font Width.

*************************************************************************** */
#endif

int     FontWidth(w)
Widget	w ;
{
	XmString	blank ;
	XmFontList	font ;
	int			width ;

	/*  create a blank string for getting the width of the labels  */
	blank = XmStringCreateSimple(" ") ;

	/*  get the default font list for this widget  */
	XtVaGetValues(w,
				XmNfontList,    &font,
				NULL) ;

	/*  get the font's width  */
	width = (int) XmStringWidth(font, blank) ;

	/*  free up the string  */
	XmStringFree(blank) ;

	/*  return the width  */
	return(width) ;

}	/*  FontWidth  */


