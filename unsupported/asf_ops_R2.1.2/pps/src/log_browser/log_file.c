

#ifndef lint
static char sccsid[] = "@(#)log_file.c	1.2  02/16/97";
#endif

#ifdef MODULE_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Task:           N/A
Module:         log_file.c
Title:			Log File functions.
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
10/31/94		Mike Tankenson		Mod to handle OSF header files
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
------------------------------------------------------------------------------

Module Overview:	Provides all functions necessary to open, read and close
					the log files.

Procedures/Functions called from other modules:
	SetLogResources()
	GetCurrentLog()
	CreateLogPopup()
	PopupLog()
	OpenLog()
	ReadLog()
	CloseLog()

Procedures/Functions called in other modules:
	LogErrorMessage()
	PopupMessage()
	UnsetListUpdateTimer()
	SetListUpdateTimer()
	FillList()
	UpdateStatusBar()



*************************************************************************** */
#endif

/*  =========================  Includes  =========================  */

#include "log_main.h"

#include <dirent.h>

#ifdef OSF
#include <sys/ioctl.h>
#else
#include <sys/filio.h>
#endif


#include <Xm/FileSB.h>
#include <Xm/List.h>


/*  =========================  Defines  =========================  */

#define	NEW_LOG_CXL	0
#define	NEW_LOG_OK	1


/*  =========================  Types  =========================  */

/*  =========================  Global Variables  =========================  */

FILE	*Log = (FILE *) NULL ;

char	 LogPath[MAX_PATH_NAME_LEN] ;
char	 LogName[MAX_LOG_NAME_LEN] ;
char	 LogSpec[MAX_LOG_NAME_LEN] ;

Widget	 fileDialog, fileDialogList ;

/*  =========================  Prototypes  =========================  */

extern	char	*re_comp(/* s */) ;
extern	int		 re_exec(/* s */) ;

static	void	newLogCB(/* w, client_data, call_data */) ;
static	void	invalidLog_CB(/* w, client_data, call_data */) ;


/*  =========================  Functions  =========================  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void SetLogResources ()

Description:	Parses logFile resource and sets the global variables
				LogPath, LogName and LogSpec.

Arguments:	none.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	Initialize the Global variables
	Check to make sure the -log option was included
	Parse the command line to retrieve the concatenated Log Path
	Check to be sure LogName is valid

*************************************************************************** */
#endif

void	 SetLogResources() 
{
	char	*lastSlash ;
	int		 pathLen, nameLen ;
	char	 msg[MAX_LOG_NAME_LEN+100] ;
	const char	 *err ;

	/*  Initialize the Global variables  */
	LogPath[0] = LogName[0] = LogSpec[0] = '\0' ;
	
	/*  Check to make sure the -log option was included  */
	if (!Resources.logFile)
	{
		PopupMessage(
"Log specification option \n(-log <path+filespec>) missing.  Exiting...") ;
		LOG_ERROR(
"Log specification option (-log <path+filespec>) missing.  Exiting...") ;
		FINAL_CLEANUP(LOG_NO_LOGSPEC) ;
	}


	/*  The command line contains the concatenated Log Path and
	 * 	Log Name.  Split the two apart and save in global variables.
	 */

	lastSlash = strrchr(Resources.logFile, '/') ;

	if (lastSlash)
	{
		nameLen = MIN(strlen(lastSlash + 1),
						MAX_LOG_NAME_LEN-1)  ;
		pathLen = MIN((strlen(Resources.logFile) - nameLen),
						MAX_PATH_NAME_LEN-1) ;

		strncpy(LogPath, Resources.logFile, pathLen) ;
		LogPath[pathLen] = '\0' ;	/*  end of string */

		strncpy(LogSpec, lastSlash+1, nameLen) ;
		LogSpec[nameLen] = '\0' ;	/*  end of string */
	}
	else
	{
		/*  there's no path, only a file spec  */
		strcpy(LogPath, "./") ;
		strncpy(LogSpec, Resources.logFile, MAX_LOG_NAME_LEN-1) ;
		LogSpec[MAX_LOG_NAME_LEN-1] = '\0' ;
	}
		
	/*  Now, check to be sure LogName is valid  */
	err = re_comp(LogSpec) ;

	if (err)
	{
		strcpy(msg, "Invalid Regular Expression on log file spec (") ;
		strcat(msg, LogSpec) ;
		strcat(msg, ").  Error=") ;
		strcat(msg,err);
		LOG_ERROR(msg) ;
		PopupMessage(msg) ;
		FINAL_CLEANUP(LOG_BAD_LOGSPEC) ;
	}
				

}	/*  SetLogResources  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        Boolean GetCurrentLog ()

Description:	Sets the global varialble LogName to the "current" log.

Arguments:	none.

Return Value:	True if current log was found; False otherwise.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	Open the right directory
	Search through the directory looking for any files which match the Log name spec.
		If a match is found
			If it is alphabetically first (last) and Resources.firstLog is True (False)
				then this is more current.
	Close the directory

*************************************************************************** */
#endif

Boolean	 GetCurrentLog()
{
	DIR				*logdir ;
	struct dirent	*logfil ;
	int				 fileSpecMatch, alphaDiff, firstFile=1 ;
	char			 msg[MAX_PATH_NAME_LEN+100] ;

	/*  first, assume no luck  */
	LogName[0] = '\0' ;

	/*  open the right directory  */
	logdir = opendir(LogPath) ;

	if (!logdir)
	{
		strcpy(msg, "Invalid log directory specified = ") ;
		strcat(msg,	LogPath) ;
		LOG_ERROR(msg) ;
		PopupMessage(msg) ;
		FINAL_CLEANUP(LOG_BAD_LOGDIR) ;
	}

	/*  Search through the directory, 
 	 *	looking for any files which match the 
	 *	Log name spec.
	 */

	while (logfil = readdir(logdir))
	{

#ifdef COMMENT
		(void)printf( "%s\n", logfil->d_name );
#endif /* COMMENT */

		fileSpecMatch = re_exec(logfil->d_name) ;
		
		if (fileSpecMatch<0)
		{
			strcpy(msg, "Invalid Regular Expression on log file spec (") ;
			strcat(msg, LogSpec) ;
			strcat(msg, ").") ;
			LOG_ERROR(msg) ;
			PopupMessage(msg) ;
			FINAL_CLEANUP(LOG_BAD_LOGSPEC) ;
		}
	
		if (fileSpecMatch)
		{
			/*  We have a match!!!  Check to see if it's the
			 *	first/last alphabetically speaking, based on
			 *	the first/last resource set.
			 */
			if (firstFile)
			{
				strcpy(LogName, logfil->d_name) ;
				firstFile = 0 ;
			}
			else
			{
				alphaDiff = strcmp(logfil->d_name, LogName) ;
				if ((alphaDiff<0 && FIRST_IS_CURRENT) ||
					(alphaDiff>0 && LAST_IS_CURRENT))
					strcpy(LogName, logfil->d_name) ;
			}
		}	/*  if  */
	}	/*  while  */

	/*  close the directory  */
	closedir (logdir);

	if (LogName[0])
		return(True) ;
	else
		return(False) ;

}	/*  GetCurrentLog  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void CreateLogPopup (parent)

Description:	Creates the popup dialog which contains a file selection
				box so that the user can select a log.

Arguments:
        parent -- parent widget for popup.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	Set up the whole directory name
	Create the file selection box widget
	Add the callback for the buttons
	Make sure the user sees the latest file

*************************************************************************** */
#endif

void	 CreateLogPopup(parent)
Widget	parent ;
{
	Arg			myArgs[20] ;
	int			i=0 ;
	XmString	dr, lb, sl, tl ;
	char		directoryMask[MAX_FILE_NAME_LEN] ;
	Widget		child ;

	/*--------------------------------------------------------*/
	/*  set up the whole filter name                          */
	/* motif doesn't like wildcard starting with non-wildcard */
	/* character.  So we will fool it with a wildcard first   */
	/*--------------------------------------------------------*/
	sprintf(directoryMask, "%s*%s*",  LogPath, LogSpec);

	dr = XmStringCreateSimple(directoryMask) ;
	lb = XmStringCreateSimple("Logs") ;
	sl = XmStringCreateSimple("Log Selection") ;
	tl = XmStringCreateSimple("Select New Log") ;

	/*  create the file selection box widget  */
	i = 0 ;
	XtSetArg(myArgs[i], XmNdirMask, dr) ; i++ ;
	XtSetArg(myArgs[i], XmNdirSpec, dr) ; i++ ;
	XtSetArg(myArgs[i], XmNfileListLabelString, lb) ; i++ ;
	XtSetArg(myArgs[i], XmNselectionLabelString, sl) ; i++ ;
	XtSetArg(myArgs[i], XmNdialogTitle, tl) ; i++ ;
	XtSetArg(myArgs[i], XmNmustMatch, True) ; i++ ;
	fileDialog = XmCreateFileSelectionDialog(parent,"Select Log",myArgs,i) ;

	/*  get rid of the parts we don't need  */
	child = XmFileSelectionBoxGetChild(fileDialog,XmDIALOG_HELP_BUTTON) ;
	XtUnmanageChild(child) ;
	child = XmFileSelectionBoxGetChild(fileDialog,XmDIALOG_APPLY_BUTTON) ;
	XtUnmanageChild(child) ;
	child = XmFileSelectionBoxGetChild(fileDialog,XmDIALOG_FILTER_LABEL) ;
	XtUnmanageChild(child) ;
	child = XmFileSelectionBoxGetChild(fileDialog,XmDIALOG_FILTER_TEXT) ;
	XtUnmanageChild(child) ;
	child = XmFileSelectionBoxGetChild(fileDialog,XmDIALOG_DIR_LIST) ;
	XtUnmanageChild(child) ;
	child = XmFileSelectionBoxGetChild(fileDialog,XmDIALOG_DIR_LIST_LABEL) ;
	XtUnmanageChild(child) ;

	XtAddCallback(fileDialog,XmNokCallback, newLogCB, (caddr_t)NEW_LOG_OK) ;
	XtAddCallback(fileDialog,XmNcancelCallback, newLogCB, (caddr_t)NEW_LOG_CXL) ;
	XtAddCallback(fileDialog,XmNnoMatchCallback, invalidLog_CB, (caddr_t)1) ;

	/*  pull out the list for later use  */
	fileDialogList = XmFileSelectionBoxGetChild(fileDialog, XmDIALOG_LIST) ;

	if (LAST_IS_CURRENT)
		/*  make sure the user sees the latest file  */
		XmListSetBottomPos(fileDialogList, 0) ;

	/*  clean up  */
	XmStringFree(dr) ;
	XmStringFree(lb) ;
	XmStringFree(sl) ;
	XmStringFree(tl) ;


}	/*  CreateLogPopup  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void PopupLog (w, client, call)

Description:	Pops up the dialog which contains the file selection dialog
				so the user can pick a log.

Arguments:
        w --		unused.
        client --	unused.
        call --		unused.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	Force the fileselection box to reread the disk to check for new files.
	Make sure there isn't anything selected from last time
	Manage the widget to make it appear.

*************************************************************************** */
#endif

void	 PopupLog(w, client, call)
Widget		w ;
XtPointer	client ;
XtPointer	call ;
{
	/*  Force the fileselection box to reread the disk to check for
		new files.  */
	XmFileSelectionDoSearch(fileDialog,NULL) ;

	/*  make sure there isn't anything selected from last time  */
	XmListDeselectAllItems(fileDialogList) ;

	/*  Manage the widget to make it appear.  */
	XtManageChild(fileDialog) ;

}	/*  PopupLog  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        Boolean OpenLog ()

Description:	Opens a log file.

Arguments:	none.

Return Value:	True if successful; False otherwise.

Messages/Data Stores:	Assigns the log's file handle to the global variable
						Log.

Notes/Constraints:	none.

Pseudo Code:
	Build the whole file name
	Open the file

*************************************************************************** */
#endif

Boolean	 OpenLog()
{
	char	wholeName[MAX_FILE_NAME_LEN] ;

	/*  Build the whole file name  */
	strcpy(wholeName, LogPath) ;
	strcat(wholeName, LogName) ;

	/*  Open the file  */
	Log = fopen(wholeName, "r") ;

	if (Log)
		return(True) ;
	else
	{
		LogName[0] = '\0' ;
		return(False) ;
	}

}	/*  OpenLog  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        Boolean LogHasDataPending()

Description:	Determines if there are any new entries in the current log,
				and if there are, clears the previous E-O-F marker, so
				the new data can be read.

Arguments:	none.

Return Value:	True if there was more data; False if end of file.

Messages/Data Stores:	Uses global variable Log.

Notes/Constraints:	none.

Pseudo Code:
	See if there are any more characters pending on the file
	if there are:
		clear the previously encountered E-O-F marker
		return true.
	else
		return false.

*************************************************************************** */
#endif

Boolean  LogHasDataPending() 
{
	int		fileHandle, ok ;
	long	charsPending ;

	/*  make sure there's a log open  */
	if (NO_LOG)
		return(False) ;

	/*  convert the file "stream" to a file "handle" for ioctl to use  */
	fileHandle = fileno(Log) ;
	if (fileHandle<0)
	{
		LOG_ERROR("Unable to convert open file stream to file handle") ;
		FINAL_CLEANUP(LOG_READ_ERROR) ;
	}

	/*  See if there are any more characters pending on the file  */
	ok = ioctl(fileHandle, FIONREAD, &charsPending) ;
	if (ok<0)
	{
		LOG_ERROR("Unable to check characters pending on log file") ;
		perror("ioctl");
		FINAL_CLEANUP(LOG_READ_ERROR) ;
	}

	/* Until I figure out this bug in SOLARIS that does not handle the
		above ioctl(), always return true */

	clearerr(Log) ;
	return(True) ;

#ifdef COMMENT
	if (charsPending>0)
	{
		/*  clear the previously encountered E-O-F marker  */
		clearerr(Log) ;
		return(True) ;
	}
	else
		return(False) ;
#endif /* COMMENT */
	
}	/*  LogHasDataPending  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        Boolean ReadLog (entry)

Description:	Reads a single entry from the current log file.

Arguments:
        entry --	gets the next line from the file.

Return Value:	True if there was more data; False if end of file.

Messages/Data Stores:	Uses global variable Log.

Notes/Constraints:	none.

Pseudo Code:
	Read the file
	Strip out the "\n" that fgets sometimes puts in

*************************************************************************** */
#endif

Boolean	 ReadLog(entry)
char	*entry ;
{
	char	*newline ;

	/*  Read the file  */
	if (fgets(entry, MAX_ENTRY_LEN, Log))
	{
		entry[MAX_ENTRY_LEN-1] = '\0' ;
		/*  Strip out the "\n" that fgets sometimes puts in  */
		newline = strrchr(entry,'\n');
		if (newline!=NULL)
			*newline = '\0'; /* replace with EOLN */
		return(True) ;
	}
	else
		return(False);


}	/*  ReadLog  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void CloseLog ()

Description:	Closes the current log.

Arguments:	none.

Return Value:	none.

Messages/Data Stores:	Uses the global variable Log.

Notes/Constraints:	none.

Pseudo Code:
	Close the log.

*************************************************************************** */
#endif

void	 CloseLog()
{
	/*  Close the log.  */
	LogName[0] = '\0' ;
	(void)	fclose(Log) ;

	Log = (FILE *) NULL ;

}	/*  CloseLog  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static void newLogCB (w, reason, call_data)

Description:	This is the callback function which gets called when the
				user presses a button on the log selection dialog. 

Arguments:
        w --			unused.
        reason --		either the user pressed OK or Cancel.
        call_data --	unused.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	Pop down the log selection dialog
	If the user hit "OK": 
		Get the new file name
		Yank the file name out of the entire path & file name
		Halt the old log from updating
		Update the display with the new log's data
		Arrange for the log to update

*************************************************************************** */
#endif

static	void	newLogCB(w, reason, call_data)
Widget								 w ;
int									 reason ;
XmFileSelectionBoxCallbackStruct	*call_data ;
{
	char	*name, err[200+MAX_FILE_NAME_LEN], *lastSlash ;
	int		 nameLen ;

	/*  Pop down the log selection dialog  */
	XtUnmanageChild(w) ;

	if (reason!=NEW_LOG_OK)
		return ;

	/*  the user must have hit "OK", get the new file name  */

	XmStringGetLtoR(call_data->value, XmSTRING_DEFAULT_CHARSET, &name) ;

	if (LOG_SELECTED)
		CloseLog();		/* close the old one before opening new */

	/*  yank the file name out of the entire path & file name  */
	lastSlash = strrchr(name, '/') ;
	if (lastSlash)
	{
		nameLen = MIN(strlen(lastSlash + 1),
						MAX_LOG_NAME_LEN-1)  ;

		strncpy(LogName, lastSlash+1, nameLen) ;
		LogName[nameLen] = '\0' ;	/*  end of string */
	}
	else
	{
		/*  there's no path, only a file spec  */
		strncpy(LogName, name, MAX_LOG_NAME_LEN-1) ;
		LogName[MAX_LOG_NAME_LEN-1] = '\0' ;
	}

	if (OpenLog()==True)
	{
		/*  Halt the old log from updating  */
		UnsetListUpdateTimer() ;
		/*  Update the display with the new log's data  */
		FillList() ;
		UpdateStatusBar() ;  /*  Display the new log file name.  */
		/*  Arrange for the log to update  */
		SetListUpdateTimer() ;
	}
	else
	{
		sprintf(err, "Unable to open requested log file (%s%s).",
					LogPath, LogName) ;
		LOG_ERROR(err) ;
		PopupMessage(err) ;
		LogName[0] = 0 ;
	}

	XtFree(name) ;

}	/* newLogCB */



#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static void invalidLog_CB (w, client_data, call_data)

Description:	Pop's up a message showing the user that s/he selected an 
				invalid log, or typed in a bad one.

Arguments:
        w --			unused.
        client_data --	unused.
        call_data --	unused.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	Pop up a message showing the user that s/he selected an 
		invalid log, or typed in a bad one.

*************************************************************************** */
#endif

static	void	invalidLog_CB(w, client_data, call_data)
Widget	  w ;
XtPointer	client_data ;
XtPointer	call_data ;
{
	/*
 	 *	Pop up a message showing the user that s/he selected an 
	 *	invalid log, or typed in a bad one.
	 */
	PopupMessage("Select an available log file or select cancel") ;

}	/*  invalidLog_CB  */



