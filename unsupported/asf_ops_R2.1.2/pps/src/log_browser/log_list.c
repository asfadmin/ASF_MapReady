
#ifndef lint
static char sccsid[] = "@(#)log_list.c	1.1  12/19/96";
#endif

#ifdef MODULE_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Task:           N/A
Module:         log_list.c
Title:			Log Browser List-related functions.
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
07/07/93		Bill West		Made critical entries beep twice, but no
								more than 5 times total per second.
07/07/93		Bill West		Added critical log entry to pop up.

------------------------------------------------------------------------------

Module Overview:	This contains the functions to access and respond to 
					changes in the list widget part of the browser.

Procedures/Functions called from other modules:
			FillList()
			EntriesSelected()
			TotalEntries()
			ShowListBottom()
			SetListUpdateTimer()
			UnsetListUpdateTimer()

Procedures/Functions called in other modules:
			ReadLog()
			EntryIsEoLogTag()
			EntryIsWarning()
			EntryIsCritical()
			Beep()
			PopupAlert()
			PopupMessage()
			CloseLog()
			GetCurrentLog()
			OpenLog()
			UpdateStatusBar()
			LogErrorMessage()
			FinalCleanupCB()


*************************************************************************** */
#endif

/*  =========================  Includes  =========================  */

#include "log_main.h"

#include <Xm/List.h>

/*  =========================  Defines  =========================  */

#define	NEW_TABLE_CHUNK		1000
#define	APPEND_TABLE_CHUNK	10

#define	BEEP_MAX			5


/*  =========================  Types  =========================  */

typedef	struct	{
	XmStringTable	entries ;
	int				used ;
	int				allocated ;
}	stringTable_t ;


/*  =========================  Prototypes  =========================  */

static	void	updateList(/* firstX */) ;
static	void	initTable() ;
static	void	string2table(/* newList, entry */) ;
static	void	table2list(/* newList */) ;
static	void	listUpdateCB(/* client_data, id */) ;


/*  =====================  Global Variables  =====================  */

static	Widget			listWidget ;
static	XtIntervalId	timeoutID = (XtIntervalId) NULL ;
static	stringTable_t	stringTable = {NULL, 0, 0} ;


/*  =========================  Functions  =========================  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void CreateList (parent, sibling)

Description:	This creates the scrolled list widget.

Arguments:
        parent --	The form on which this sits.
        sibling --	The status bar over which this sits.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	create the list
	manage the list

*************************************************************************** */
#endif

void	CreateList(parent, sibling)
Widget	parent ;
Widget	sibling ;
{
	Arg	  	myArgs[20] ;
	int	  	i = 0 ;

	/*  create the list  */
	i = 0 ;
	XtSetArg(myArgs[i], XmNselectionPolicy, XmEXTENDED_SELECT) ; i++ ;
	XtSetArg(myArgs[i], XmNlistSizePolicy, XmCONSTANT) ; i++ ;
	XtSetArg(myArgs[i], XmNscrollBarDisplayPolicy, XmSTATIC) ; i++ ;
	XtSetArg(myArgs[i], XmNscrolledWindowMarginHeight, 10) ; i++ ;
	XtSetArg(myArgs[i], XmNscrolledWindowMarginWidth, 10) ; i++ ;
	XtSetArg(myArgs[i], XmNbottomAttachment, XmATTACH_WIDGET) ; i++ ;
	XtSetArg(myArgs[i], XmNbottomWidget, sibling) ; i++ ;
	XtSetArg(myArgs[i], XmNtopAttachment, XmATTACH_FORM) ; i++ ;
	XtSetArg(myArgs[i], XmNleftAttachment, XmATTACH_FORM) ; i++ ;
	XtSetArg(myArgs[i], XmNrightAttachment, XmATTACH_FORM) ; i++ ;
	listWidget =
		XmCreateScrolledList(parent, "MainList", myArgs, i) ;

	XtManageChild(listWidget) ;

}	/*  CreateList  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void FillList ()

Description:	Initially fills the list with the current log's data.

Arguments:	none.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	change cursor to a watch
	unmanage the list so this doesn't take so long
	empty any current entries in the list
	Remanage so as to show new log
	return cursor to normal

*************************************************************************** */
#endif

void	FillList()
{	  
	/*  unmanage the list so this doesn't take so long  */
	XtUnmanageChild(listWidget) ;

	/*  empty any current entries in the list  */
	XmListDeleteAllItems(listWidget) ;
	
	/*	Update list until there are no more entries  */
	updateList(1) ;

	/*  Remanage so as to show new log  */
	XtManageChild(listWidget) ;


}	/*  FillList  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static void updateList (firstX)

Description:	This reads any new data from the log, and updates the list.

Arguments:
        firstX --	1 if called for the first time for this log; 0 otherwise.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	if no log currently, don't do anything
	while there's unread entries in the log
		check to see if this entry is an end of log marker
		check to see if this entry should be included with the current filter
		append the data to the log buffer
		beep the bell if this is a warning or critical entry
		keep a count of any critical entries
	flush the data buffer to the list
	Pop up alert window if any new critical entries were encountered
	force the user to see the latest entries
	if this is the end of log, 
		Halt the old log from updating
		close the old log
		if a more current log is found
			open the new log
			fill up the next list
			Display the new log file name.
			Arrange for the log to update

*************************************************************************** */
#endif

static	void	updateList(firstX)
int	firstX ;
{
	char			logEntry[MAX_ENTRY_LEN] ;
	int				entryCount=0, beepCount=0, alertCount=0, eoLogReached=0 ;

	/*  if no log currently, don't do anything  */
	if (NO_LOG)
		return ;

	/*
	 *  Rewind the file, just in case.  (Actually, one common
	 *	case is that the filter was reset by the user.  Then
	 *	the whole log will be reread in from the beginning.
	 */
	if (firstX)
		rewind(Log) ;

	/*  while there's unread entries in the log  */
	while (ReadLog(logEntry) == True)
	{
		entryCount++ ;

		/*  check to see if this entry is an end of log marker  */
		if (!firstX && EOL_LOG_CHECK)
		{
			if (EntryIsEoLogTag(logEntry)==True)
				eoLogReached = 1 ;
		}

		/*  check to see if this entry should be included with the current filter  */
		if (LOG_FILTERED)
		{
			if (EntryMeetsFilterCriteria(logEntry)==False)
				continue ;	/*  skip this entry  */
		}
		/*  append the data to the log buffer  */
		string2table(firstX, logEntry) ;
		
		/*  beep the bell if this is a warning or critical entry  */
		if (!firstX && SOUND_ON)
		{
			if (beepCount<BEEP_MAX && 
				(EntryIsWarning(logEntry)==True || EntryIsCritical(logEntry)==True))
			{
				Beep(listWidget) ;
				beepCount++ ;

				/*  Now beep a second time if critical  */
				if (EntryIsCritical(logEntry)==True)
				{
					Beep(listWidget) ;
					beepCount++ ;
				}
			}
		}

		/*  keep a count of any critical entries  */
		if (!firstX && EntryIsCritical(logEntry)==True)
		{
			RegisterAlert(logEntry) ;
			alertCount++ ;
		}
		
	}	/*  while  */

	if (!entryCount)
		return ;	/*  no data, get out...  */

	/*  flush the data buffer to the list  */
	table2list(firstX) ;

	/*  Pop up alert window if any new critical entries were encountered  */
	if (alertCount)
		PopupAlert() ;

	if (firstX || UPDATE_MODE)
		/*  force the user to see the latest entries  */
		ShowListBottom() ;

	/*  Now, if this is the end of log, show the next log  */
	if (eoLogReached)
	{
		PopupMessage("Log is full or finished.  Switching to new log...") ;

		/*  Halt the old log from updating  */
		UnsetListUpdateTimer() ;

		/*  close the old log  */
		CloseLog() ;
		
		/*  if a more current log is found  */
		if (GetCurrentLog()==True)
		{
			/*  open the new log  */
			if (OpenLog()==True)
			{
				/*  fill up the next list  */
				FillList() ;  
				/*  Display the new log file name.  */
				UpdateStatusBar() ;
				/*  Arrange for the log to update  */
				SetListUpdateTimer() ;
			}
			else
			{
				PopupMessage("Unable to open new log") ;
				LOG_ERROR("Unable to open new log") ;
			}
		}
		else
		{
			PopupMessage("Unable to find new log") ;
			LOG_ERROR("Unable to find new log") ;
		}
	}	/*  if not initially filling in list, and e-o-log  */
	
}	/*  updateList  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static void initTable ()

Description:	Initialized the log entry buffer.

Arguments:	none.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	(re)initialize the stringTable structure
	if there were old entries previously, 
		free the memory

*************************************************************************** */
#endif

static	void	initTable() 
{
	int	str ;

	/*  (re)initialize the stringTable structure  */

	for (str=0 ; str<stringTable.used ; str++)
		XmStringFree(stringTable.entries[str]) ;

	stringTable.used = stringTable.allocated = 0 ;

	/*  if there were old entries previously, free the memory  */
	if (stringTable.entries)
		XtFree((char *)stringTable.entries) ;

	stringTable.entries = (XmStringTable) NULL ;

}	/*  initTable  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static void string2table (newList, entry)

Description:	Appends an entry to the list buffer.

Arguments:
        newList --	1 if this is a brand new log; 0 for updates to an old one.
        entry --	a log entry.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	bump the count of strings
	see if we need to allocate more to hold the new string
	now, add the string to the table

*************************************************************************** */
#endif

static	void	string2table(newList, entry)
int		 newList ;
char	*entry ;
{
	/*  bump the count of strings  */
	(stringTable.used)++ ;

	/*  see if we need to allocate more to hold the new string  */
	if (stringTable.used > stringTable.allocated)
	{
		if (newList)
			stringTable.allocated += NEW_TABLE_CHUNK ;
		else
			stringTable.allocated += APPEND_TABLE_CHUNK ;
				
		stringTable.entries = (XmStringTable) 
			XtRealloc((char *) stringTable.entries,
				stringTable.allocated * sizeof (XmString)) ;;

		if (!(stringTable.entries))
		{
			LOG_DISASTER("Insufficient memory to append to list.") ;
			FINAL_CLEANUP(LOG_ALLOC_ERROR) ;
		}
	}

	/*  now, add the string to the table  */
	stringTable.entries[stringTable.used-1] = 
		XmStringLtoRCreate(entry, XmSTRING_DEFAULT_CHARSET) ;

}	/*  string2table  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static void table2list (newList)

Description:	Flushes the log buffer to the visible list.

Arguments:
        newLista --	1 if this is a new log; 0 for mere updates.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	append the items to the list
	free up and reinitialize the string table

*************************************************************************** */
#endif

static	void	table2list(newList)
int	newList ;
{

	/*  append the items to the list  */
	if (newList)
		/*  for some reason the following XtVaSetValues takes up
		 *	about half the memory that using XmListAddItems does.
		 */
		XtVaSetValues(listWidget,
						XmNitemCount,	stringTable.used,
						XmNitems,		stringTable.entries,
						NULL) ;
	else
		XmListAddItems(listWidget, stringTable.entries, 
						stringTable.used, 0) ;


	/*  free up and reinitialize the string table  */
	initTable() ;

}	/*  table2list  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        Boolean EntriesSelected (first, last)

Description:	This returns whether or not entries have been selected with
				the mouse.  If so, it returns the first and last selected
				line numbers in the log.

Arguments:
        first --	The first selected entry.
        last --		The last selected entry.

Return Value:	True if any entries are selected; False otherwise.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	check to see if any log is being viewed; if not, 
		return False
	get the current list of selected positions
	if nothing is selected, 
		return False
	make sure our "selected" list includes everything.
	free up the memory that was used.

*************************************************************************** */
#endif

Boolean	EntriesSelected(first, last)
int		*first ;
int		*last ;
{
	int		*selectedArray, numSelected ;
	Boolean	any ;

	/*  check to see if any log is being viewed  */
	if (NO_LOG)
		return(False) ;

	/*  get the current list of selected positions  */
	any = XmListGetSelectedPos(listWidget, &selectedArray, &numSelected) ;

	/*  if nothing is selected, return False  */
	if (!any)
		return(False) ;

	if (!numSelected)
		return(False) ;

	/*  make sure our "selected" list includes everything that's really
	 *	selected, including those that were unselected using shift-mb1.
	 */
	*first = selectedArray[0] ;
	*last = selectedArray[numSelected-1] ;

	/*  free up the memory that XmListGetSelectedPos allocated  */
	XtFree((char *)selectedArray) ;

	return(True) ;

}	/*  EntriesSelected  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        int TotalEntries ()

Description:	Returns the total number of log entries in the current log.

Arguments:	none.

Return Value:	The total number of log entries in the current log.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	If no log is being viewed,
		return 0
	else
		get the number of entries.
		return it.

*************************************************************************** */
#endif

int		TotalEntries()
{
	int	total ;

	/*  Is there a log being viewed?  */
	if (NO_LOG)
		return(0) ;

	/*  How many entries does it have?  */
	XtVaGetValues(listWidget,
					XmNitemCount,	&total,
					NULL) ;

	return(total) ;
}

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void ShowListBottom ()

Description:	Scrolls the list so that the last entry is showing.

Arguments:	none.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:	force the user to see the last entry

*************************************************************************** */
#endif

void	ShowListBottom()
{
	/*  force the user to see the last entry  */
	XmListSetBottomPos(listWidget, 0) ;

}	/*  ShowListBottom  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void SetListUpdateTimer ()

Description:	Starts the timer until the next list update.

Arguments:	none.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	Set up the timeout for the number of seconds defined in the delay resource.

*************************************************************************** */
#endif

void	SetListUpdateTimer()
{
	
	/*  Set up the timeout for the seconds defined in the delay resource.  */
	timeoutID = XtAddTimeOut(Resources.delay, listUpdateCB, NULL) ;

}	/*	SetListUpdateTimer  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void UnsetListUpdateTimer ()

Description:	Turns off any time previous set by SetListUpdateTimer.

Arguments:	none.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	if a timer is in effect
		turn it off.
	mark the timer as off

*************************************************************************** */
#endif

void	UnsetListUpdateTimer()
{
	/*  if a timer is in effect,  turn it off.   */
	if (timeoutID)
		XtRemoveTimeOut(timeoutID) ;

	/*  mark the timer as off  */
	timeoutID = (XtIntervalId) NULL ;

}	/*  UnsetListUpdateTimer  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static void listUpdateCB (client_data, id)

Description:	The function called by a timeout expiring which updates the log.

Arguments:
        client_data --	unused.
        id --			unused.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	If there is a log currently and the window is visible, 
		Update the list
	If there is a log currently, 
		See up the next timeout

*************************************************************************** */
#endif


static	void	listUpdateCB(client_data, id)
XtPointer		 client_data ;
XtIntervalId	*id ;
{
	/*  If window is visible, Update the list  */
	if (LOG_SELECTED)
	{
		if (LogHasDataPending() == True)
			updateList(0) ;
	}

	/*  If there is a log currently, Set up the next timeout  */
	if (LOG_SELECTED)
		SetListUpdateTimer() ;

}	/*  listUpdateCB  */
