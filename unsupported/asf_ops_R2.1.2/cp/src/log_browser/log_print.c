
#ifndef lint
static char sccsid[] = "@(#)log_print.c	1.7 93/05/21 16:29:09 ";
#endif

#ifdef MODULE_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Task:           N/A
Module:         log_print.c
Title:			Print Functionality
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
------------------------------------------------------------------------------

Module Overview:	This provides the printing-related functions.

Procedures/Functions called from other modules:
	CreatePrintPopup()
	PopupPrint()

Procedures/Functions called in other modules:
	EntriesSelected()
	PopupMessage()
	TotalEntries()


*************************************************************************** */
#endif


/*  =========================  Includes  =========================  */

#include "log_main.h"

#include <Xm/MessageB.h>

/*  =========================  Defines  =========================  */

#define	PRINT_COMMAND			"log_lpr.sh "

#define	BIG_PRINTOUT_THRESHOLD	500


/*  =========================  Types  =========================  */


/*  =========================  Prototypes  =========================  */

static	void	printem(/* w, client, call */) ;


/*  =====================  Global Variables  =====================  */

static	Widget	queryPopup ;
static	int		printRequest=PRINT_ALL ;
static	int		partialStart=0, partialCount=0 ;


/*  =========================  Functions  =========================  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void CreatePrintPopup (parent)

Description:	This creates the popup which is used to verify the user's
				intentions regarding printing.

Arguments:
        parent --	Top level (or any other parent-type) widget for 
					parenting the pop up on.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	create the pop up dialog to verify that the user will print
	get rid of the help button
	Set up the callback for the Print (OK) button
	free up the XmStrings

*************************************************************************** */
#endif

void	CreatePrintPopup(parent)
Widget	parent ;
{
	Arg			myArgs[20] ;
	int			i=0 ;
	XmString	printString, title ;
	Widget		child ;

	/*  create the pop up dialog to verify that the user will print  */
	printString = XmStringCreateSimple("Print") ;
	title = XmStringCreateSimple("Confirm Print Request") ;
	XtSetArg(myArgs[i], XmNokLabelString, printString) ; i++ ;
	XtSetArg(myArgs[i], XmNdialogTitle, title) ; i++ ;
	queryPopup = XmCreateQuestionDialog(parent, "LogBrowserMessage",
										myArgs, i) ;

	/*  get rid of the help button  */
	child = XmMessageBoxGetChild(queryPopup, XmDIALOG_HELP_BUTTON) ;
	XtUnmanageChild(child) ;

	/*  Set up the callback for the Print (OK) button  */
	XtAddCallback(queryPopup, XmNokCallback, printem, (caddr_t)NULL) ;

	/*  free up the XmStrings  */
	XmStringFree(printString) ;
	XmStringFree(title) ;

}	/*  CreatePrintPopup  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void PopupPrint (w, allOrPart, call)

Description:	This is the callback which is called if the user hits
				the OK button on the print popup.

Arguments:
        w --			unused.
        allOrPart --	either PRINT_ALL or PRINT_PART for partial print.
        call --			unused.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	If no log being viewed, 
		exit
	determine whether this is a full or partial print
	figure out how big a job this is
		if this is a partial print, 
			if no entries have been selected, 
				Inform the user and exit
			save the start entry number and the total entries for later
		else
		print all
	pop up a dialog to verify that the user really wants to do this

*************************************************************************** */
#endif

void	PopupPrint(w, allOrPart, call)
Widget		w ;
int			allOrPart ;
XtPointer	call ;	
{
	char		msg[200], query[100] ;
	int			first, last, lineCount ;
	Boolean		anySelected ;
	XmString	prompt ;

	/*  If no log being viewed, exit  */
	if (NO_LOG)
	{
		PopupMessage("There is currently no log to print!") ;
		return ;
	}

	/*  determine whether this is a full or partial print  */
	printRequest = allOrPart ;
	
	/*  figure out how big a job this is  */
	if (allOrPart==PRINT_PART)
	{
		/*  if partial print, and no entries have been selected, exit */
		anySelected = EntriesSelected(&first,&last) ;
		if (anySelected)
			lineCount = last - first + 1 ;
		else
			lineCount = 0 ;
		if (!lineCount)
		{
			PopupMessage(
	"No entries have been selected for a partial log print.") ;
			return ;

		}
		else
		{
			/*  save start entry and total entries for later  */
			partialStart = first ;
			partialCount = lineCount ;
		}
	}
	else
	{
		/*  print all  */
		lineCount = TotalEntries() ;
		if (!lineCount)
		{
			PopupMessage(
	"This log is currently empty.  No printout sent.") ;
			return ;
		}
	}

	/*  pop up a dialog to verify that the user really wants to do this  */
	strcpy(query, "Do you really want to print ") ;
	if (allOrPart==PRINT_PART)
		strcat(query, "a partial ") ;
	else
		strcat(query, "the entire ") ;
	strcat(query, "log") ;

	if (lineCount > BIG_PRINTOUT_THRESHOLD)
		sprintf(msg, "\n%s which is %d entries long?", query, lineCount) ;
	else
		sprintf(msg, "%s?", query) ;

	/*  apply the new query to the pop up and pop it up  */
	prompt = XmStringCreateLtoR(msg, XmSTRING_DEFAULT_CHARSET) ;
	XtVaSetValues(queryPopup, 
					XmNmessageString,	prompt,
					NULL) ;
	XtManageChild(queryPopup) ;

	XmStringFree(prompt) ;

}	/*  PopupPrint  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static void printem (w, client, call)

Description:	This builds the script command to print the requested 
				entries, and invokes it.

Arguments:
        w --		unused
        client --	unused
        call --		unused

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	pop down the pop up right away
	start building the print command
	if this is a partial print, 
		add that info to the command
	if this is filtered, 
		add the filter info to the command
	now, lastly, add the log path and name
	envoke the command
	inform the user that the log has been sent

*************************************************************************** */
#endif

static	void	printem(w, client, call)
Widget		w ;
XtPointer	client ;
XtPointer	call ;
{
	char	msg[100] ;
	char	cmd[200] ;
	char	tmp[100] ;

	/*  pop down the pop up right away  */
	XmUpdateDisplay(TopLevel) ;

	/*  start building the print command  */
	strcpy(cmd, PRINT_COMMAND) ;

	/*  if this is a partial print, add that info to the command  */
	if (printRequest==PRINT_ALL)
		msg[0] = '\0' ;
	else
	{
		strcpy(msg, "Partial ") ;
		sprintf(tmp, "-p %d %d ", partialStart, partialCount) ;
		strcat(cmd, tmp) ;
	}

	/*  if this is filtered, add the filter info to the command  */
	if (LOG_FILTERED)
	{
		strcat(msg, "Filtered ") ;
		if (Filter_type == INCLUSION_FILTER)
			strcat(cmd, "-f \"") ;
		else
			strcat(cmd, "-e \"") ;

		if (DOUBLE_FILTERED)
		{
			strcat(cmd, Filter[0]) ;
			strcat(cmd, "\" ") ;
			if (FilterCriteria==or)
				strcat(cmd, "OR") ;
			else
				strcat(cmd, "AND") ;
			strcat(cmd, " \"") ;
			strcat(cmd, Filter[1]) ;
		}	/*  if DOUBLE_FILTERED  */
		else
		{
			if (Filter_on[0] == True)
				strcat(cmd, Filter[0]) ;
			else
				strcat(cmd, Filter[1]) ;
		}	/*  if DOUBLE_FILTERED...else  */
		strcat(cmd, "\" ") ;
	}	/* if LOG_FILTERED  */

	/*  now, lastly, add the log path and name  */
	strcat(cmd, LogPath) ;
	strcat(cmd, LogName) ;

	/*  envoke the command  */
	system(cmd) ;

	/*  inform the user that the log has been sent  */
	strcat(msg, "Log sent to printer...") ;

	PopupMessage(msg) ;

}	/*  printem  */

