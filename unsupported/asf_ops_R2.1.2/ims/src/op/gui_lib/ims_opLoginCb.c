static char *sccs = "@(#)ims_opLoginCb.c	5.3  05/12/97";
/*******************************************************************************

	File:			ims_opLoginCb.c

	Function:	Callback functions for Initial Login box 

	Author:		Jennifer Ting

	Date:			1/1996

	Revision: 6/11/1996 - Modified function getUserLogin() to correct PR 942.
						4/01/1997 - Modified getUserLogin() to use "user", if specified.
						4/07/1997 - Modified login_passwdCb() to handle "select" op.s.

*******************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <Xm/Xm.h>
#include <Xm/MwmUtil.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <X11/Shell.h>
#include <UxXt.h>

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

#define _IMS_OP_LOGINCB_C
#include "ims_opCb.h"

/*******************************************************************************
       Includes, Defines, and Global variables from the Declarations Editor:
*******************************************************************************/

extern char *glbLogin;             
extern char *glbPassword;

/*******************************************************************************
       The following header file defines the context structure.
*******************************************************************************/

#define CONTEXT_MACRO_ACCESS 
#include <ims_opLogin.h>
#undef CONTEXT_MACRO_ACCESS

/*
** Local Declarations
*/
#define OK_PRESSED 1
#define CANCEL_PRESSED 2

static void userResponse(Widget, XtPointer, XtPointer);
char *user_passwd;

/*******************************************************************************
       The following are callback functions.
*******************************************************************************/


/*===========================================================================*
** 
** Function Name: getUserLogin
**
** Description:		Create the User Login Box, 
**
** Arguments: 
** 
** Return Value: 	
** 
** Revision History:
**
**==========================================================================*/
	
int getUserLogin(Widget parent)
{
	_UxClogin               *UxSaveCtx, *UxContext;
  Widget dialog;
  int answer = 0;
	int valid;
	char *buff;

  dialog = create_login(parent);
	glbPassword = (char *)NULL;

	UxSaveCtx = UxLoginContext;
	UxLoginContext = UxContext =
			(_UxClogin *) UxGetContext(dialog);

  XtAddCallback(okPB, XmNactivateCallback, userResponse, &answer);
	XtVaSetValues (okPB, XmNuserData, OK_PRESSED, NULL);

  XtAddCallback(cancelPB, XmNactivateCallback, userResponse, &answer);
	XtVaSetValues (cancelPB, XmNuserData, CANCEL_PRESSED, NULL);

  XtAddCallback(passwdText, XmNactivateCallback, userResponse, &answer);
	XtVaSetValues (passwdText, XmNuserData, OK_PRESSED, NULL);

	/*
	** This is to add the callbacks to the window manager quit
	** button for each screen, this is to correct PR 942
	*/
	addWinMgrCloseCB (dialog, userResponse, (XtPointer) &answer);

	/* if specified, use the given "user" name */
	if (glbLogin != NULL)
	{
			XmTextSetString( userIdText, glbLogin ) ;
			XtVaSetValues( userIdText,
					XmNcursorPosition, (XmTextPosition) strlen( glbLogin ),
					NULL ) ;
	}

	XtPopup(XtParent(dialog), XtGrabNone);

	/* set cursor at userIdText widget */
	XmProcessTraversal (userIdText, XmTRAVERSE_CURRENT);

  /* while the user hasn't provided an answer, simulate XtMainLoop.
   * The answer changes as soon as the user selects one of the
   * buttons and the callback routine changes its value.  Don't
   * break loop until XtPending() also returns False to assure
   * widget destruction.
   */

	valid = IMS_FALSE;
  while (!valid && (answer == 0 || XtAppPending(XtWidgetToApplicationContext(dialog))))
	{
    XtAppProcessEvent(XtWidgetToApplicationContext(dialog), XtIMAll);

		if (answer == IMS_OK)
		{
			/*
			** get User ID and Password.
			*/

			buff = XmTextGetString (userIdText);
			ims_truncStr (buff);

			if (buff && *buff)
			{
				glbLogin = malloc (strlen(buff) + 1);
				(void) strcpy (glbLogin, buff);
			}

			ims_truncStr (user_passwd);

			if (user_passwd && *user_passwd)
			{
				glbPassword = malloc (strlen(user_passwd) + 1);
				(void) strcpy (glbPassword, user_passwd);
			}

			if ((glbLogin && *glbLogin) && (glbPassword && *glbPassword))
			{
				valid = IMS_TRUE;
			}
			else
			{
				answer = 0;
			}
		}
	}

	XtFree (buff);
	XtPopdown (XtParent(dialog));
	XtDestroyWidget(dialog);

	UxLoginContext = UxSaveCtx;

  return answer;

}


/*===========================================================================*
**
** Function Name: userResponse
**
** Description: The user made some sort of response to the
**              question posed in AskUser().  Set the answer (client_data)
**              accordingly and destroy the dialog.
**              
** Arguments: 1. Widget that is calling the callback.
**            2. return the answer as either as IX_YES or IX_NO
**            3. not used
**
** Return Values: None
**
**==========================================================================*/

/* ARGSUSED2 */
static void
userResponse(Widget w, XtPointer clientData, XtPointer callData)
{
  int *answer = (int *) clientData;
	int which;

	XtVaGetValues (w, XmNuserData, &which, NULL);

  switch (which)
	{
     case OK_PRESSED:
       	*answer = IMS_OK;
       	break;

     case CANCEL_PRESSED:
        *answer = IMS_ERROR;
        break;
		
		default:
    		*answer = IMS_ERROR;
        break;
  }
    
}


/*===========================================================================*
** 
** Function Name: login_passwdCb
**
** Description:		Callback function for passwdText widget
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. status - IMS_ status type
**								3. cbs 		- message to display
** 
** Return Value: 	None
** 
** Revision History:
**				4/07/1997 (Teresa) - Modified to handle "select" operations.
**
**==========================================================================*/
	
/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 * Permission to use, copy, and modify this program without
 * restriction is hereby granted, as long as this copyright
 * notice appears in each copy of the program source code.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* password.c -- prompt for a password. All input looks like
 * a series of *'s.  Store the actual data typed by the user in
 * an internal variable.  Don't allow paste operations.  Handle
 * backspacing by deleting all text from insertion point to the
 * end of text.
 */
void login_passwdCb(
			Widget    text_w,
			XtPointer cd, 
			XtPointer cb)
{
	_UxClogin               *UxSaveCtx, *UxContext;
	Widget                  UxWidget = text_w;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;
	int											useEndPos ;

	UxSaveCtx = UxLoginContext;
	UxLoginContext = UxContext =
			(_UxClogin *) UxGetContext( UxWidget );
	{
    char *new;
    int len;
    XmTextVerifyCallbackStruct *cbs = 
        (XmTextVerifyCallbackStruct *) cb;

		useEndPos = cbs->endPos ;

    if (cbs->startPos < cbs->currInsert     /* backspace or some text */
				|| cbs->endPos > cbs->startPos)			/* was selected */
		{
			/* NOTE: if get here, already have part of a passwd: user_passwd != NULL*/
			cbs->endPos = strlen (user_passwd);    /* FORCE delete thru end */
			user_passwd[cbs->startPos] = 0;        /* deletion--terminate */
			useEndPos = strlen( user_passwd ) ;
		}

    if (cbs->text->length > 1) 
		{
      cbs->doit = False;  /* don't allow "paste" operations */
      return;             /* make the user *type* the password! */
    }

    new = XtMalloc (useEndPos + 2); /* new char + NULL terminator */
    if (user_passwd) 
		{
      (void) strcpy (new, user_passwd);
      XtFree (user_passwd);
    }
		else
		{
      new[0] = NULL;
		}

    user_passwd = new;
    (void) strncat (user_passwd, cbs->text->ptr, cbs->text->length);
    user_passwd[cbs->endPos + cbs->text->length] = 0;

    for (len = 0; len < cbs->text->length; len++)
			cbs->text->ptr[len] = '*';

	}
	UxLoginContext = UxSaveCtx;
}


/*===========================================================================*
** 
** Function Name: login_moveFocusCb
**
** Description:		Callback function for userIdText widget
**
** Arguments:			1. widget - Widget that is calling this callback
**								2. status - IMS_ status type
**								3. cbs 		- message to display
** 
** Return Value: 	None
** 
** Revision History:
**
**==========================================================================*/

void login_moveFocusCb(
			Widget    text_w,
			XtPointer cd, 
			XtPointer cb)
{
	_UxClogin               *UxSaveCtx, *UxContext;
	Widget                  UxWidget = text_w;
	XtPointer               UxClientData = cd;
	XtPointer               UxCallbackArg = cb;

	UxSaveCtx = UxLoginContext;
	UxLoginContext = UxContext =
			(_UxClogin *) UxGetContext( UxWidget );
	{

		XmProcessTraversal (text_w, XmTRAVERSE_NEXT_TAB_GROUP);
	}

	UxLoginContext = UxSaveCtx;

}
