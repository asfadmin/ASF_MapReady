

#ifndef lint
static char sccsid[] = "@(#)log_alert.c	1.10 93/07/09 10:32:03 ";
#endif

#ifdef HEADER_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Module:         log_alert.c
Title:          Interface for critical log entry routines.
------------------------------------------------------------------------------
Modification History:

Date		By		Description
------------------------------------------------------------------------------
05/28/91	Bill West	Initial Delivery
07/08/93	Bill West	Added RegisterAlert call.
------------------------------------------------------------------------------

Description:    Interface to constants and routines needed to handle
				critical log entries.
 

Module Structures:      none.

Included Modules:       none.

*************************************************************************** */
#endif

/*  ==========================  Includes  ==========================  */

#include "log_main.h"

#include <Xm/MessageB.h>

/*   ==========================  Global Variables   ==========================  */

#define	ALERT_ENTRY_LEN			60
#define	ELIPSIS					"..."
#define	ALERT_ENTRY_TEXT_LEN	(ALERT_ENTRY_LEN - strlen(ELIPSIS) - 1)

#define	MESSAGE_SINGLE		"Critical entry detected in "
#define	MESSAGE_NUM_LEN		15
#define	MESSAGE_MULTIPLE	" unacknowledged critical entries in "
#define	MESSAGE_MULTIPLE_2	"\nbeginning with the following entry"
#define	MESSAGE_TEXT_LEN	200 
#define	MESSAGE_LEN			(MESSAGE_NUM_LEN+MESSAGE_TEXT_LEN+MAX_LOG_NAME_LEN+5)


/*   ==========================  Global Variables   ==========================  */

static	Widget  alertWidget ;

static	char	alertEntry[ALERT_ENTRY_LEN] ;
static	int		alertCount=0 ;

/*   ==========================  Prototypes   ==========================  */

static	void	acknowledgeCB(/* w, client, call */) ;

/*   ==========================  Functions   ==========================  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:void   CreateAlertPopup(Widget	parent)


Description:   	Creates a pop up warning dialog box to inform the user
		of a critical log entry(s).

Arguments:  parent -- widget parent.

Return Value:   none.

Messages/Data Stores:   none.

Notes/Constraints:  none.

Pseudo Code: 
        Create the dialog. 
*************************************************************************** */
#endif

void    CreateAlertPopup(parent)
Widget	parent  ;
{
	XmString dialogTitle, buttonLabel ;
	Widget	helpButton, cancelButton, message ;
	Arg		myArgs[20] ;
	int		i = 0 ;

	dialogTitle = XmStringLtoRCreate("ASF Log Warning", 
					 XmSTRING_DEFAULT_CHARSET) ;
	buttonLabel = XmStringLtoRCreate("Acknowledged", 
					 XmSTRING_DEFAULT_CHARSET) ;

	i = 0 ;
	XtSetArg(myArgs[i], XmNdialogTitle, dialogTitle) ; i++ ;
	XtSetArg(myArgs[i], XmNokLabelString, buttonLabel) ; i++ ;

	alertWidget = XmCreateWarningDialog(parent, "AlertDialog", 
						myArgs, i) ;

	XtAddCallback(alertWidget, XmNokCallback, acknowledgeCB, (caddr_t)NULL) ;

	helpButton = XmMessageBoxGetChild(alertWidget, XmDIALOG_HELP_BUTTON) ;
	cancelButton = XmMessageBoxGetChild(alertWidget, XmDIALOG_CANCEL_BUTTON) ;
	message = XmMessageBoxGetChild(alertWidget, XmDIALOG_MESSAGE_LABEL) ;

	if (helpButton)
		XtUnmanageChild(helpButton) ;
	if (cancelButton)
		XtUnmanageChild(cancelButton) ;
	if (message)
		XtVaSetValues(message,
						XmNalignment,	XmALIGNMENT_CENTER,
						NULL) ;

	alertEntry[0] = '\0' ;

	XmStringFree(dialogTitle) ;
	XmStringFree(buttonLabel) ;

}	/*  CreateAlertPopup  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:	void   RegisterAlert(char *alert)


Description:    Saves up the count and initial unacknowledged alert entry.

Arguments:  
	alert --	The current alert entry.

Return Value:   none.

Messages/Data Stores:   global variables alertCount and alertEntry.

Notes/Constraints:  none.

Pseudo Code:    
	Increment the alert count.
	if this is the first alert,
		save the alert entry for use by PopupAlert.

*************************************************************************** */
#endif

void	RegisterAlert(alert)
char	*alert ;
{
	/*  Increment the alert count.  */
	alertCount++ ;

	/*  if this is the first alert,  */
	if (alertCount==1)
	{
		/*  save the alert entry for use by PopupAlert.  */
		if (strlen(alert) < ALERT_ENTRY_LEN)
			strcpy(alertEntry, alert) ;
		else
		{
			strncpy(alertEntry, alert, ALERT_ENTRY_TEXT_LEN) ;
			alertEntry[ALERT_ENTRY_TEXT_LEN] = '\0' ;
			strcat(alertEntry, ELIPSIS) ;
		}
	}

}	/*  RegisterAlert  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:	void   PopupAlert()


Description:    Pops up a warning dialog box to inform the user
				of a critical log entry(s).

Arguments:  none.

Return Value:   none.

Messages/Data Stores:   global variables alertCount and alertEntry.

Notes/Constraints:  none.

Pseudo Code:    
		Build the warning message.
		Apply the warning message to the pop up dialog.
        Pop up the dialog.  

*************************************************************************** */
#endif

void    PopupAlert()
{
		Arg			myArgs[20] ;
		XmString	warningLabel ;
		int			i=0, alreadyPoppedUp ;
		char		warning[MESSAGE_LEN], num[MESSAGE_NUM_LEN] ;

	alreadyPoppedUp = (XtIsManaged(alertWidget) == True) ;

	/*  build the warning message  */
	if (alertCount==1)
		strcpy(warning, MESSAGE_SINGLE) ;
	else
	{
		sprintf(num,"%d", alertCount) ;
		strcpy(warning, num) ;
		strcat(warning, MESSAGE_MULTIPLE) ;
	}
	strcat(warning, LogName) ;

	if (alertCount>1)
		strcat(warning, MESSAGE_MULTIPLE_2) ;

	strcat(warning, ":\n\n\"") ;
	strcat(warning, alertEntry) ;
	strcat(warning, "\"") ;

	/*  Due to flukey motif feature, we have to manage this BEFORE 
	 *	applying the label.
	 */
	if (!alreadyPoppedUp)
		XtManageChild(alertWidget) ;

	/*  apply it to the popup dialog  */
	warningLabel = XmStringLtoRCreate(warning, XmSTRING_DEFAULT_CHARSET) ;
	XtSetArg(myArgs[i], XmNmessageString, warningLabel) ; i++ ;
	XtSetValues(alertWidget, myArgs, i) ;

	XmStringFree(warningLabel) ;

}	/*  PopupAlert  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:	void   acknowledgeCB()

Description:    Clears global variables alertCount and alertEntry
				to indicate that the critical entries have been 
				acknowledged.

Arguments:  none.

Return Value:   none.

Messages/Data Stores:   global variables alertCount and alertEntry.

Notes/Constraints:  none.

Pseudo Code:    
		Clear global variables alertCount and alertEntry.
		Pop down the dialog.

*************************************************************************** */
#endif

static	void	acknowledgeCB(w, client, call)
Widget		w ;
XtPointer	client ;
XtPointer	call ;
{
	/*  clear global variables alertCount and alertEntry  */
	alertCount = 0 ;
	alertEntry[0] = '\0' ;

	/*  pop down the dialog  */
	XtUnmanageChild(alertWidget) ;
	XmUpdateDisplay(TopLevel) ;

}	/*  acknowledgeCB  */
