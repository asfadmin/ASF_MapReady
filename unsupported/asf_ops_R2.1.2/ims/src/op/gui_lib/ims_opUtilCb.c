static char *sccs = "@(#)ims_opUtilCb.c	5.6  06/27/97";
/*****************************************************************************

	File:			ims_opUtilCb.c

	Function:	GUI utility functions for Operator Interface

	Author:		Jennifer Ting

	Date:			4/1995

	Revision: 6/10/1996 -  Added function addWinMgrCloseCB to correct PR 942.
						6/11/1996 -  Modified function askUser to correct PR 942.
						7/12/1996 -  Modified function printScreen to use import tool.
						4/02/1997 -  Added function isTimeFieldValid

*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#include <X11/XWDFile.h>
#include <X11/cursorfont.h>
#include <Xm/Screen.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <X11/Shell.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include <Xm/CascadeB.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Protocols.h>
#include <UxXt.h>

#define _IMS_OP_UTILCB_C
#include "ims_opCb.h"
#include <ims_timeConv.h>

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

#include <ims_opAskUserDlg.h>

#define OP_LOW_YEAR 1990
#define OP_HIGH_YEAR 2050

extern OP_GLOBAL_DATA glbData;

/* Function local to this file */
static void changeCursors(Cursor crsr);
static void ynResponse(Widget, XtPointer, XtPointer);


/*===========================================================================*
** 
** Function Name: isDateFieldValid
**
** Description:		Validates any Date field of the format:
**										yyyyXmmXdd, where X can be any separator
**
** Arguments:			1. fieldValue
** 
** Return Value:	IMS_OK or IMS_ERROR
** 
** Revision History:
**
**==========================================================================*/

int isDateFieldValid(
	char  *fieldValue)
{
	IMS_NUMERIC_DATE	timei;
	char							dateStr[BUFSIZ];
	int								status ;

	(void) strcpy (dateStr, fieldValue);
	ims_truncStr (dateStr);

	/* empty string is ok */
	if (*dateStr == '\0')
		return (IMS_OK);

	if (strlen(dateStr) < 10)
		return (IMS_ERROR);

	if ((status = ims_timeToNumericDate (	NULL, (char *) dateStr, &timei ))
			== IMS_OK)
	{
		if (timei.year < OP_LOW_YEAR  || timei.year > OP_HIGH_YEAR)
			status = IMS_ERROR ;
	}

	return (status);

}


/*===========================================================================*
** 
** Function Name: isTimeFieldValid
**
** Description:		Validates time for ASF time format: yyyy-doyThh:mm:ss.fff
**
** Arguments:			1. fieldValue
**								2. timeArray	- array of numeric time components or NULL
** 
** Return Value:	IMS_OK or IMS_ERROR
** 
** Revision History:
**
**==========================================================================*/

int
isTimeFieldValid(
	char  *fieldValue,
	IMS_NUMERIC_DATE *timeArray )
{
	IMS_NUMERIC_DATE	*timei ;
	char							value[BUFSIZ];
	int								len ;
	int								status ;

	(void) strcpy (value, fieldValue);
	ims_truncStr (value);
	len = strlen( value ) ;

	/* empty string is ok */
	if (len == 0)
		return (IMS_OK);

	if (timeArray)
		timei = timeArray ;
	else if ((timei = malloc( sizeof(IMS_NUMERIC_DATE) )) == NULL)
		return IMS_ERROR ;

	status = ims_timeToNumericDate (	NULL, (char *) value, timei ) ;

	if (!timeArray)
		free( timei ) ;

	return (status);
}


/*===========================================================================*
** 
** Function Name: printScreen
**
** Description:		dump an image of the screen and send it to printer
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- not used
** 
** Return Value: 	None
** 
** Revision History: 07/12/96 - use import to print. /usr/local/bin/import
**
**==========================================================================*/
	
void printScreen (Widget wgt)
{
	char cmdbuf[IMS_COL255_LEN+1];
	int mypid;

	mypid = (int) getpid();
	(void) sprintf(cmdbuf, 
					"/usr/local/bin/import -rotate 90 -window 0x%lx /tmp/%d.ps\n",
					XtWindow(wgt), mypid);

	(void) system (cmdbuf);
	(void) sprintf (cmdbuf, "lpr /tmp/%d.ps\n", mypid);
	(void) system (cmdbuf);
	(void) sprintf (cmdbuf, "rm /tmp/%d.ps\n", mypid);
	(void) system (cmdbuf);

	/*
  	Window window;
  	char cmdbuf[IMS_COL255_LEN+1];

  	window = XtWindow (wgt);
  	(void) sprintf (cmdbuf,
										"xwd -id %d | xpr -device ps -scale 3 -landscape | lpr ",
										window);
  	(void) system (cmdbuf);
  */

}


/*===========================================================================*
** 
** Function Name: timeOutCursors
**
** Description: timeOut_cursors() turns on the "watch" cursor over 
**							the application 
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. cd 		- not used
**								3. cbs 		- XmScrollBarCallbackStructure
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/
	
void timeOutCursors(int on)
{
  static int locked;
  static Cursor watchCursor;
  XEvent event;

	/*
 	 * "locked" keeps track if we've already called the function.
	 * This allows recursion and is necessary for most situations.
	 */

  on? locked++ : locked--;
  if(locked > 1 || locked == 1 && on == 0)
    return; /* already locked and we're not unlocking */

  if(!watchCursor) /* make sure the timeOut cursor is initialized */
    watchCursor = XCreateFontCursor(XtDisplay(glbData.welcomeW), XC_watch);

	/*
 	 * if "on" is true, then turn on watch cursor, otherwise, return
	 * the shell's cursor to normal.
 	 */   

  if(on)
    changeCursors(watchCursor);
  else 
	{
    changeCursors(None);
		/* 
 		* get rid of all button and keyboard events that occured
 		* during the time out.  The user shouldn't have done anything
 		* during this time, so flush for button and keypress events.
 		* KeyRelease events are not discarded because accelerators
 		* require the corresponding release event before normal input
 		* can continue.
 		*/

    while(XCheckMaskEvent(UxDisplay,
			  ButtonPressMask | ButtonReleaseMask | ButtonMotionMask
			  | PointerMotionMask | KeyPressMask, &event)) 
		{
      /* do nothing */;
	  }
  }

}



/*===========================================================================*
** 
** Function Name: changeCursors
**
** Description:		turns on the cursor over the application.  It checks 
**								to see as to which screens are up.  For any screen
**								that is up, it changes to desired cursor shape
**
** Arguments: 1. Cursor type -- can be busy cursor, normal arrow cursor
** 
** Return Value: 	None
** 
** Revision History: 6/18/97 Added dlSearch
**
**==========================================================================*/
	
static void changeCursors(Cursor crsr)
{
  XSetWindowAttributes attrs;

  attrs.cursor = crsr;

  if(glbData.welcomeFlag)
    XChangeWindowAttributes(XtDisplay(glbData.welcomeW), 
			    XtWindow(glbData.welcomeW), 
			    CWCursor, &attrs);
 
  if(glbData.orderFlag)
    XChangeWindowAttributes(XtDisplay(glbData.orderW), 
			    XtWindow(glbData.orderW), 
			    CWCursor, &attrs);
  
  if(glbData.searchFlag)
    XChangeWindowAttributes(XtDisplay(glbData.searchW), 
			    XtWindow(glbData.searchW), 
			    CWCursor, &attrs);

  if(glbData.photoOrderFlag)
    XChangeWindowAttributes(XtDisplay(glbData.photoOrderW), 
			    XtWindow(glbData.photoOrderW), 
			    CWCursor, &attrs);

  if(glbData.photoJobFlag)
    XChangeWindowAttributes(XtDisplay(glbData.photoJobW), 
			    XtWindow(glbData.photoJobW), 
			    CWCursor, &attrs);

  if(glbData.filmFlag)
    XChangeWindowAttributes(XtDisplay(glbData.filmW), 
			    XtWindow(glbData.filmW), 
			    CWCursor, &attrs);

  if(glbData.mediaFlag)
    XChangeWindowAttributes(XtDisplay(glbData.mediaW), 
			    XtWindow(glbData.mediaW), 
			    CWCursor, &attrs);

  if(glbData.dlSearchFlag)
    XChangeWindowAttributes(XtDisplay(glbData.dlSearchW), 
			    XtWindow(glbData.dlSearchW), 
			    CWCursor, &attrs);

  XFlush(XtDisplay(glbData.welcomeW));

}


/*===========================================================================*
** 
** Function Name: askUser
**
** Description:		turns on the cursor over the application.  It checks 
**
** Arguments: 1. Cursor type -- can be busy cursor, normal arrow cursor
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/
	
int askUser(Widget parent, char *question)
{
  Widget dialog;
  XmString text;
  int answer = 0;


  dialog = create_askUserDlg(parent);

  XtUnmanageChild(
     XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
  XtAddCallback(dialog, XmNokCallback, ynResponse, &answer);
  XtAddCallback(dialog, XmNcancelCallback, ynResponse, &answer);
  text = XmStringCreateLtoR(question, XmSTRING_DEFAULT_CHARSET);
  XtVaSetValues(dialog,
      XmNmessageString,      text,
      NULL);
  XmStringFree(text);

	/*
	** 06/11/96 - 
	** This is to add the callbacks to the window manager quit
	** button for each screen, this is to correct PR 942
	*/
	addWinMgrCloseCB (dialog, ynResponse, (XtPointer) &answer);

  XtManageChild(dialog);

  /* while the user hasn't provided an answer, simulate XtMainLoop.
   * The answer changes as soon as the user selects one of the
   * buttons and the callback routine changes its value.  Don't
   * break loop until XtPending() also returns False to assure
   * widget destruction.
   */
  while (answer == 0 || XtAppPending(XtWidgetToApplicationContext(parent)))
      XtAppProcessEvent(XtWidgetToApplicationContext(parent), XtIMAll);
  return answer;
}


/*
 * Name: ynResponse
 *
 * Description: The user made some sort of response to the
 *              question posed in AskUser().  Set the answer (client_data)
 *              accordingly and destroy the dialog.
 *              
 * Arguments: 1. Widget that is calling the callback.
 *            2. return the answer as either as IX_YES or IX_NO
 *            3. reason indicating whether it is OK or CANCEL
 *
 * Return Values: None
 *
 * Warnings: None
 *
 * Global Variables Used: None
 *
 * Conditions:
 * 	Precondition 1:
 *		User pressed OK pushButton.
 * 
 *	Postcondition 1: 
 *		Set answer as IX_YES and destroy dialog
 *
 * 	Precondition 2:
 *		User pressed CANCEL pushButton.
 * 
 *	Postcondition 2: 
 *		Set answer as IX_NO and destroy dialog
 *
 * 	Precondition 3:
 *		User pressed Window manager's close pushButton.
 * 
 *	Postcondition 3: 
 *		Set answer as IX_NO and destroy dialog
 *
 * Revision History:
 */
static void
ynResponse(Widget w, XtPointer clientData, XtPointer callData)
{
  int *answer = (int *) clientData;
  XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) callData;

  switch (cbs->reason) {
     case XmCR_OK:
        *answer = IMS_OK;
        break;
     case XmCR_CANCEL:
        *answer = IMS_ERROR;
        break;
  }

/* User pressed window manager close button */
  if(cbs->reason != XmCR_OK && cbs->reason != XmCR_CANCEL)
    *answer = IMS_ERROR;
    
  XtDestroyWidget(w);
}


/*===========================================================================*
** 
** Function Name: concatString
**
** Description:		concatenate existing string with new string.
**
** Return Value: 	IMS_OK or IMS_FATAL
** 
** Revision History:
**
**==========================================================================*/
	
int concatString(
	CONCAT_STR *concatStr,
	char *newStr)
{
	char *t_Msg;
	int cur_posn, sizeReq;
	char errorMsg[IMS_COL255_LEN+1];

	/* the size of the new message string */
	sizeReq = strlen(newStr) + 1;

	if ((concatStr->msgPtr + strlen(newStr) +1) > 
			(concatStr->Msg + concatStr->Msg_size))
	{
		cur_posn = concatStr->msgPtr - concatStr->Msg;

		/*
		** use ceiling function ((x+n-1)/n)*n to calculate
		** the malloc size.
		*/
		concatStr->Msg_size += (((sizeReq + 224)/225)*225);

		if ((t_Msg = realloc(concatStr->Msg, concatStr->Msg_size)))
		{
			concatStr->Msg = t_Msg;
			concatStr->msgPtr = concatStr->Msg + cur_posn;
		}
		else
		{
			(void) strcpy (errorMsg,
					"Memory allocation failed, please exit program.\n");
			msgBoxDlg_popupCb (glbData.welcomeW, IMS_FATAL, errorMsg); 
			return (IMS_FATAL);
		}
	}

	(void) strcpy (concatStr->msgPtr, newStr);
	concatStr->msgPtr += strlen(newStr);

	return(IMS_OK);

}


/*===========================================================================*
** 
** Function Name: forceUpdate
**
** Description:		This function is a superset of XmUpdateDisplay() in that
**								it will ensure that a window's contents are visible 
**								before returning.  This function is intended to be called
**								after XtPopup(), XtManageChild() on a widget.
**
** Return Value: 	None
** 
** Revision History: Copied from V0 IX_forceUpdate function. (IX_common.c)
**
**==========================================================================*/
	
void forceUpdate(Widget wgt)
{
  Widget dialogShell;
  Window dialogWindow;
  XWindowAttributes xWa;
  XEvent event;

  for(dialogShell = wgt; !XtIsShell(dialogShell); dialogShell = 
      XtParent(dialogShell));

	/*
 	** If the dialog shell is not realized, do not bother.
 	*/
  if(XtIsRealized(dialogShell)) 
	{
    dialogWindow   = XtWindow(dialogShell);

		/*
 		** Wait for the dialog to be mapped. It's gauranteed to be mapped.
 		*/
    while(XGetWindowAttributes(UxDisplay, dialogWindow, &xWa) &&
	  xWa.map_state != IsViewable) 
		{

			/*
			** 3/22/96 - so this was the problem!!
			*/
	
			/*
 	     if(xWa.map_state == IsUnmapped)
				break;
			*/

			/*
 			** There will be event of some kind.
 			*/
 	     XtAppNextEvent(UxAppContext, &event);
 	     XtDispatchEvent(&event);
    }
  }

	/*
	** The next XSync() will get an expose event
 	*/

 	 XmUpdateDisplay(UxTopLevel);

}


/*
 * Name: addWinMgrCloseCB
 *
 * Description: addWinMgrCloseCB() first tells the window manager to
 *              do nothing when the user presses window manager's close
 *              button. Then it setups a protocal via which the window
 *              managers redirects the event to application's desired
 *              callback. Usually the callback should be either close
 *              callback or cancel callback.
 *
 * Arguments: 1. Toplevel shell widget if it is a main screen or dialog
 *               widget.
 *
 *            2. Callback that needs to be activated when user clicks the
 *               window manager's close push button.
 *
 *            3. The parameter that you would like to be passed to the
 *               callback when it is activated. Usually it is NULL.
 *
 * Return Values: None
 *
 * Warnings: None
 *
 * Global Variables Used: UxDisplay
 *
 * Conditions:
 *     Precondition 1:
 *              Window Manager's close pushbutton is activated.
 *
 *     Postcondition 1:
 *              Activate corresponding close or cancel callback.
 *
 * Revision History: 
 */
void addWinMgrCloseCB(
			 Widget wgt, 
			 XtCallbackProc closeFunc, 
			 XtPointer clientData      
			 )
{
  Atom WM_DELETE_WINDOW;

/* Tell the Window Mgr. that you are going to handle the close action yourself
   by setting XmNdeleteResponse to XmDO_NOTHING */
  XtVaSetValues(XtParent(wgt), 
		XmNdeleteResponse, XmDO_NOTHING,
		NULL );
/*
  XtVaSetValues(wgt, 
		XmNdeleteResponse, XmDO_NOTHING,
		NULL );
*/

/* Then add a callback for the WM_DELETE_WINDOW protocol */
  WM_DELETE_WINDOW = XmInternAtom(UxDisplay, "WM_DELETE_WINDOW", False);

/* Get either toplevelshell or dialogshell */
  while(wgt && !XtIsShell(wgt))
    wgt = XtParent(wgt);

  XmAddWMProtocolCallback(wgt, WM_DELETE_WINDOW, closeFunc, clientData); 
}


/*===========================================================================*
** 
** Function Name: isApostrophe
**
** Description:		Search through the input string to see if there is 
**                any apostrophe.
**
** Arguments:			1. day
**								2. month
**								3. year
** 
** Return Value:	Returns IMS_TRUE or IMS_FALSE
** 
** Revision History:
**
**==========================================================================*/

int isApostrophe(
	char *str)
{
	char *cPtr;
	int aFlag;

	aFlag = IMS_FALSE;
	cPtr = str;

	while ((*cPtr != '\0') && (!aFlag))
	{
		if (*cPtr == '\'')
			aFlag = IMS_TRUE;

		cPtr++;
	}

	return(aFlag);
}
