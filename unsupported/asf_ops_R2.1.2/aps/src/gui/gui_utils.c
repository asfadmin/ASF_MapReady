#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		gui_utils.c

Description:	

External Functions Defined:
				gui_trimstring
				gui_TF_string
				gui_optionMenu_string
				gui_setEditable
				gui_XmList_selected_item
				gui_filebox_filename
				gui_ask_file_replacement
				AskUser
				gui_aps_internal_error
				gui_display_message_widget
				gui_AddToTopLevelInterfaces
				gui_GetShellWidget
				TimeoutCursors
				DisplayXCursor
				CheckForInterrupt

File Scope Functions:
				response
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)gui_utils.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.gui_utils.c"

#include <stdio.h>
#include <ctype.h>

#include <sys/stat.h>
#include <unistd.h>

#include <Xm/DialogS.h>
#include <Xm/List.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <X11/cursorfont.h>

#include "dapps_defs.h"
#include "gui_utils.h"
#include "gui_defs.h"
#include "cb_datetime.h"

char question[1024] ;
char blank_char = ' ' ;
char blank_str[] = " " ;

extern void popup_message() ;
extern char display_string[] ;

extern XtAppContext UxAppContext ;
extern Widget UxTopLevel ;
extern Display *UxDisplay ;

typedef struct TL_INTERFACES
{
	Widget	window ;
	int		locked ;	/* lock for window-only timeout */
} TL_INTERFACES ;

static TL_INTERFACES interfaces[MAX_TOPLEVEL_WIDGETS] ;




/*==============================================================================
Function:		gui_AddToTopLevelInterfaces

Description:	Add widget to array of TopLevel interfaces.
            	The list is used whenever access to all interfaces are needed

Parameters:		

Returns:     	the number of widgets in the array
				OR -1 if this widget would have exceeded the array bounds

Creator:		Ron Green

Creation Date:	01/10/1995

Notes:		
==============================================================================*/
int
gui_AddToTopLevelInterfaces(Widget widget) 
{
	static count = 0 ;

	if (count == MAX_TOPLEVEL_WIDGETS)
		return(-1) ;

	interfaces[count++].window = widget ;

	return(count) ;
}



/*==============================================================================
Function:		gui_getShellWidget

Description:	Gets the shell widget of the input widget.

Parameters:		Widget	widget - the input widget.

Returns:		The shell widget.

Creator:		Teresa McKillop

Creation Date:	Mon Jul 10 16:18:10 PDT 1995

Notes:			All widgets either are shells or have a parent shell.
==============================================================================*/
Widget
gui_GetShellWidget( Widget widget ) 
{
	Widget	tmpWidget;

	for (tmpWidget = widget; !XtIsShell( tmpWidget ); )
		tmpWidget = XtParent( tmpWidget );

	return (tmpWidget);
}


/*==============================================================================
Function:		gui_trimstring

Description:    removes leading and trailing "space" characters from
				a string.  Also turns embedded strings of "space"
				characters into a single blank char.

Parameters:		char *	- string to be trimmed

Returns:     	char *	- pointer to modified string, which might be
						  an empty string

Creator:		??? 

Creation Date:	11/03/1994

Notes:		
		Originally called  IC_trimString
		Borrowed from code on an IMS/DADs system
==============================================================================*/
char *
gui_trimstring(char *Str)
{ 
   int i = 0, j = 0, Blank = 0;

   while (Str[i] != NULL && isspace(Str[i]))
     i++;

   while (Str[i] != NULL)
    {
     Blank = 0;
     while (Str[i] != NULL && !isspace(Str[i]))
       Str[j++] = Str[i++];
     if (Str[i] != NULL && isspace(Str[i]))
      {
       Str[j++] = ' ';
       i++;
       Blank = 1;
      }
     while (Str[i] != NULL && isspace(Str[i]))
       i++;
    }

   if (Blank)
     Str[j-1] = '\0';
   else
     Str[j] = '\0';
	return(Str) ;
} 


/*==============================================================================
Function:       gui_TF_string

Description:    gets the widget's text string and trims it.

Parameters:     

Returns:        char *	- the trimmed text, which might be an empty string

Creator:        unknown

Creation Date:  unknown

Notes:			The application is responsible for freeing the storage
				associated with the string by calling XtFree.
==============================================================================*/
char *
gui_TF_string(Widget widget) 
{
	char * str ;
	str = XmTextGetString(widget) ;
	(void) gui_trimstring(str) ;
	return(str) ;
}


/*==============================================================================
Function:       gui_optionMenu_string

Description:    Gets the label string from an option menu.

Parameters:     

Returns:        

Creator:        unknown (Tlm added this header)

Creation Date:  before 12/12/96

Notes:			The application is responsible for freeing the storage
				associated with the string by calling XtFree.
==============================================================================*/
char *
gui_optionMenu_string(Widget widget)
{
	Widget button ;
	XmString labelstring ;
	char *label ;

	/* get the currently selected menu option choice */
	XtVaGetValues(widget,
		XmNmenuHistory, &button,
		NULL) ;

	/* the selected menu option's label string contains the status */
	XtVaGetValues(button,
		XmNlabelString, &labelstring,
		NULL) ;

	XmStringGetLtoR(labelstring, XmFONTLIST_DEFAULT_TAG, &label) ;
	(void) gui_trimstring(label) ;

	return(label) ;
}



/*==============================================================================
Function:       gui_setEditable

Description:    Turns on/off the sensitivity of a
				widget.  For text and textfield widgets, turns on/off
				the editability of the widget via the ...SetEditable
				X functions and makes visible/invisible the cursor.

Parameters:     widgetName	- the widget 
				widgetType	- the type of the widget
				editability	- True:  make widget editable
							  False: make widget uneditable
				
Returns:        None

Creator:        Teresa McKillop

Creation Date:  07/17/96

Notes:		
==============================================================================*/
void
gui_setEditable( Widget widgetName, AG_widget_type widgetType,
		Boolean editability )
{
	switch (widgetType)
	{
		case AG_TEXT :
			XmTextSetEditable( widgetName, editability ) ;
			cb_toggle_cursor( widgetName, (XtPointer) editability, NULL ) ;
			break ;
		case AG_TEXTFIELD :
			XmTextFieldSetEditable( widgetName, editability ) ;
			cb_toggle_cursor( widgetName, (XtPointer) editability, NULL ) ;
			break ;
		default :
			break ;
	}

	XtSetSensitive( widgetName, editability ) ;

	return ;
}


/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 * Permission to use, copy, and modify this program without
 * restriction is hereby granted, as long as this copyright
 * notice appears in each copy of the program source code.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/* response() --The user made some sort of response to the
 * question posed in AskUser().  Set the answer (client_data)
 * accordingly.
 */
static void
response(widget, client_data, call_data)
	Widget widget;
	XtPointer client_data;
	XtPointer call_data;
{
    int *answer = (int *) client_data;
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;

    if (cbs->reason == XmCR_OK)
        *answer = YES;
    else if (cbs->reason == XmCR_CANCEL)
        *answer = NO;
}

/*
 * AskUser() -- a generalized routine that asks the user a question
 * and returns a response.  Parameters are: the question, the labels
 * for the "Yes" and "No" buttons, and the default selection to use.
 */
AskUser(parent, qtext, default_ans)
	Widget parent;
	char *qtext ;
	int default_ans;
{
    static Widget dialog; /* static to avoid multiple creation */
    XmString text, ok, yes, no, dialog_title ;
    int answer;


		dialog_title = XmStringCreateLocalized ("Question");
        dialog = XmCreateQuestionDialog (parent, "dialog", NULL, 0);
        XtVaSetValues (dialog,
            XmNdialogStyle,        XmDIALOG_FULL_APPLICATION_MODAL,
			XmNdialogTitle,        dialog_title,     
            NULL);

		XtUnmanageChild(XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON)) ;
        XtAddCallback (dialog, XmNokCallback, response, &answer);
        XtAddCallback (dialog, XmNcancelCallback, response, &answer);
		XmStringFree(dialog_title) ;

    answer = 0;

	/*
	-- make a copy of the question string 
	-- for some reason this routine core dumps if the
	-- original string that was passed is used again
	-- therefore use a local copy
	*/
	(void) strcpy(question, qtext) ;
    text = XmStringCreateLtoR (question, XmFONTLIST_DEFAULT_TAG);

    ok  = XmStringCreateLocalized ("Ok");
    yes = XmStringCreateLocalized ("Yes");
    no  = XmStringCreateLocalized ("No");
    XtVaSetValues (dialog,
        XmNmessageString,      text,
        XmNokLabelString,      default_ans == ASK_OK ? ok : yes,
        XmNcancelLabelString,  no,
        XmNdefaultButtonType,  default_ans == NO ?
            XmDIALOG_CANCEL_BUTTON : XmDIALOG_OK_BUTTON,
        NULL);
	XmStringFree (text);
    XmStringFree (ok);
    XmStringFree (yes);
    XmStringFree (no);
    XtManageChild (dialog);
    XtPopup (XtParent (dialog), XtGrabNone);
	DisplayXCursor(True) ;

    while (answer == 0)
        XtAppProcessEvent (UxAppContext, XtIMAll);
	DisplayXCursor(False) ;

    XtPopdown (XtParent (dialog));
	XtDestroyWidget(dialog) ;
    /* make sure the dialog goes away before returning. Sync with server
     * and update the display.
     */
    XSync (XtDisplay (dialog), 0);
    XmUpdateDisplay (parent);


    return answer;
}

void
gui_aps_internal_error( 
    int		dialog_type,
	char    *source_file,
	int     line_number,
	char    *error_message )
{
	char	internal_err_string[APS_GUI_DISPLAY_STR_SIZE + 2 * FILENAME_MAX] ;

	(void) sprintf( internal_err_string,
		"APS INTERNAL ERROR AT\n SOURCE FILE: %s LINE: %d\n%s",
		source_file, line_number, error_message ) ;
	popup_message( dialog_type, "APS:INTERNAL ERROR",
		internal_err_string, XtGrabNone ) ;

	return ;
}


/*==============================================================================
Function:		gui_XmList_selected_item
Description:	
Parameters:		list (a list widget)
Returns:		postion of the currently selected item
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:          returns 0 if no item is selected
==============================================================================*/
int
gui_XmList_selected_item(list)
	Widget list ;
{
	int *pos_list ;
	int pos_count ;
	int i = 0 ;

	/*
	-- get the selected item in the list 
	-- only one item can be selected so
	-- it's always at pos_list[0]
	*/

	if (XmListGetSelectedPos(list, &pos_list, &pos_count))
	{
		i = pos_list[0] ;
		XtFree( (char *)pos_list) ;  /* free the position indicator */
		return(i) ;
	}
	return(i) ;
}



/*==============================================================================
Function:		gui_display_message_widget
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
void
gui_display_message_widget(scrolledTextWidget , text)
	Widget scrolledTextWidget;
	char *text;
{
	XmTextInsert(scrolledTextWidget, 
		XmTextGetLastPosition(scrolledTextWidget), text);
	XmTextShowPosition(scrolledTextWidget,
		XmTextGetLastPosition(scrolledTextWidget));
}



/*==============================================================================
Function:		gui_filebox_filename

Description:	Get the filename from the filebox
                If the filename is okay then popdown the file box

Parameters:		Standard X Callback Parameters

Returns:     	the filename, else NULL if the filename is not okay

Creator:		Ron Green

Creation Date:	12/05/1994

Notes:			The application is responsible for freeing the storage
				associated with the filename by calling XtFree.
==============================================================================*/
char *
gui_filebox_filename(
	Widget widget,  XmFileSelectionBoxCallbackStruct *cbs)
{

	char *file, *dir, *newfile ;
	struct stat stat_buf ;

	if (!XmStringGetLtoR(cbs->value, XmFONTLIST_DEFAULT_TAG, &file))
		return(NULL) ;

	if (!*file) /* nothing typed */
	{
		XtFree(file) ;

		/* remove the file box */
		XtPopdown(XtParent(widget)) ;

		(void) sprintf(display_string, "%s", "No file selected") ;
		popup_message(XmDIALOG_ERROR, "APS: File Selection",
			display_string, XtGrabNone) ;

		return(NULL) ;	
	}

	if (*file != '/') 
	{
		/* if it's not a directory, determine the full pathname
		 * of the selection by concatenating it to the "dir" part
		*/
		if (XmStringGetLtoR (cbs->dir, XmFONTLIST_DEFAULT_TAG, &dir))
		{
			newfile = XtMalloc (strlen (dir) + 1 + strlen (file) + 1);

			/*
			-- the code in the oreilly book used this statement...
			--
			--    sprintf (newfile, "%s/%s", dir, file) ;
			--
			-- however the dir name returned already has a slash
			-- concatenated at the end, so we don't need explicitly
			-- put the slash here 
			*/
			(void) sprintf (newfile, "%s%s", dir, file) ;
			XtFree(file) ;
			XtFree(dir) ;
			file = newfile;
		}
	}

	/* remove the file box */
	XtPopdown(XtParent(widget)) ;

	/*
	-- if file can't be accessed, 
	-- assume it is to be created and return
	*/	
	if (stat(file, &stat_buf) == -1)
		return(file) ;

	if (!S_ISREG(stat_buf.st_mode))  /* regular file */
	{
		(void) sprintf(display_string, "Invalid File:\n\n%s\n", file) ;
		popup_message(XmDIALOG_ERROR, "APS: File Selection",
			display_string, XtGrabNone) ;

		XtFree(file) ;
		return(NULL) ;
	}

	return(file) ;
}



/*==============================================================================
Function:		gui_ask_file_replacement

Description:	Checks for existence of file, if the file exists a
            	popup question dialog asking for replacement.  If
				Yes, return True otherwise return False

Parameters:		filename

Returns:     	int

Creator:		Ron Green

Creation Date:	12/06/1994

Notes:		
==============================================================================*/
int
gui_ask_file_replacement(file) 
	char *file ;
{
	/* if file already exists, ask if it should be over written */
	if (access(file, F_OK) == 0)
	{
		(void) sprintf(question,
			"File:\n\n%s\n\nalready exists.\nDo you wish to overwrite?\n", file) ;
		if (AskUser(UxTopLevel, question, NO) == NO)
			return (NO) ;
	}
	return(YES) ;
}




/* The interesting part of the program -- extract and use at will */

static Boolean stopped;  /* True when user wants to stop task */
static Widget dialog;    /* WorkingDialog displayed */

/* TimeoutCursors() -- turns on the watch cursor over the application
 * to provide feedback for the user that she's going to be waiting
 * a while before she can interact with the application again.
 *
 * If the window widget argument is not NULL, turns on watch cursor
 * only in that window (but doesn't 'cause the program to "wait"; other
 * work/window events can still be started)
 */
void
TimeoutCursors( Boolean on, Boolean interruptable, Widget window )
{
    static int locked;
    static Cursor cursor;
	extern Widget UxTopLevel ;
    XSetWindowAttributes attrs;
    Display *dpy = XtDisplay (UxTopLevel) ; 
    XEvent event;
	TL_INTERFACES *interfacesPtr;
	int *lockedPtr;
    int n;
#ifdef NO_DIALOG
    extern void stop();
#endif

	if (window)
	{
		for (n = 0 ; interfaces[n].window ; n++)
		{
			if (interfaces[n].window == window)
			{
				interfacesPtr = &interfaces[n] ;
				break ;
			}
		}
	}

    /* "locked" keeps track if we've already called the function.
     * This allows recursion and is necessary for most situations.
     */
	if (window)
		lockedPtr = &(interfacesPtr->locked) ;
	else
		lockedPtr = &locked ;

	if (on) 
		(*lockedPtr)++ ;
	else 
		(*lockedPtr)-- ;
	if (*lockedPtr > 1 || *lockedPtr == 1 && on == 0)
		return; /* already locked and we're not unlocking */

    stopped = False;
    if (!cursor) 
        cursor = XCreateFontCursor (dpy, XC_watch);

    /* if on is true, then turn on watch cursor, otherwise, return
     * the shell's cursor to normal.
     */
    attrs.cursor = on ? cursor : None;

    /* change the main application shell's cursor to be the timeout
     * cursor or to reset it to normal.  If other shells exist in
     * this application, they will have to be listed here in order
     * for them to have timeout cursors too.
     */

	if (window && XtIsRealized( window))
	{
		if (!locked)	/* if application-wide is not "timeout" */
		{
			XChangeWindowAttributes( dpy,
				XtWindow( window ), CWCursor, &attrs ) ;
		}
	}
	else
	{
		for (n = 0 ; interfaces[n].window ; n++)
		{
			if (XtIsRealized(interfaces[n].window) && !interfaces[n].locked)
			{
				XChangeWindowAttributes(
					dpy, XtWindow(interfaces[n].window), CWCursor, &attrs);
			}
		}
	}
	XFlush (dpy);

    if (!on) {
        /* get rid of all button and keyboard events that occurred
         * during the time out.  The user shouldn't have done anything
         * during this time, so flush for button and keypress events.
         * KeyRelease events are not discarded because accelerators
         * require the corresponding release event before normal input
         * can continue.
         */
        while (XCheckMaskEvent (dpy,
                ButtonPressMask | ButtonReleaseMask | ButtonMotionMask
                | PointerMotionMask | KeyPressMask, &event)) {
            /* do nothing */;
        }
#ifdef NO_DIALOG
        XtDestroyWidget (dialog) ;
#endif
    } 
#ifdef NO_DIALOG
    else {
        /* we're timing out, put up a WorkingDialog.  If the process
         * is interruptable, allow a "Stop" button.  Otherwise, remove
         * all actions so the user can't stop the processing.
         */
        n = 0;
        str = XmStringCreateLocalized ("Busy -- Please Wait.");
        XtSetArg (args[n], XmNmessageString, str); n++;
        dialog = XmCreateWorkingDialog (UxTopLevel, "busy", args, n);
        XmStringFree (str);
        XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_OK_BUTTON));
        XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON));
        if (interruptable) {
            str = XmStringCreateLocalized ("Stop");
            XtVaSetValues (dialog, XmNcancelLabelString, str, NULL);
            XmStringFree (str);
            XtAddCallback (dialog, XmNcancelCallback, stop, NULL);
        } 
        else
            XtUnmanageChild (XmMessageBoxGetChild 
                (dialog, XmDIALOG_CANCEL_BUTTON));
        XtManageChild (dialog);
    XSync (XtDisplay (dialog), 0);
    XmUpdateDisplay (dialog);
    }
#endif

    XFlush (dpy);
}

/* stop() -- user pressed the "Stop" button in dialog. */
/* ARGSUSED0 */
void
stop(dialog, client_data, call_data)
	Widget dialog;
	XtPointer client_data;
	XtPointer call_data;
{
    stopped = True;
}

/* CheckForInterrupt() -- check events in event queue and process
 * the interesting ones.
 */
Boolean
CheckForInterrupt()
{
    Display *dpy = XtDisplay (UxTopLevel);
    Window win = XtWindow (dialog);
    XEvent event;

    /* Make sure all our requests get to the server */
    XFlush (dpy);

    /* Let Motif process all pending exposure events for us. */
    XmUpdateDisplay (UxTopLevel);

    /* Check the event loop for events in the dialog ("Stop"?) */
    while (XCheckMaskEvent (dpy,
           ButtonPressMask | ButtonReleaseMask | ButtonMotionMask |
           PointerMotionMask | KeyPressMask, &event)) {
        /* got an "interesting" event. */
        if (event.xany.window == win)
            XtDispatchEvent (&event); /* it's in our dialog.. */
        else /* uninteresting event--throw it away and sound bell */
            XBell (dpy, 50);
    }
    return stopped;
}


void
DisplayXCursor(Boolean on) 
{
    static Cursor cursor;
	static int locked ;
	extern Widget UxTopLevel ;
    XSetWindowAttributes attrs;
    Display *dpy = XtDisplay (UxTopLevel) ; 
	int n ;

    /* "locked" keeps track if we've already called the function.
     * This allows recursion and is necessary for most situations.
     */
    if (on) 
        locked++;
    else 
        locked--;
    if (locked > 1 || locked == 1 && on == 0)
        return; /* already locked and we're not unlocking */

    if (!cursor) 
        cursor = XCreateFontCursor (dpy, XC_X_cursor);

    /* if on is true, then turn on X cursor, otherwise, return
     * the shell's cursor to normal.
     */
    attrs.cursor = on ? cursor : None;

    /* change the main application shell's cursor to be the timeout
     * cursor or to reset it to normal.  If other shells exist in
     * this application, they will have to be listed here in order
     * for them to have timeout cursors too.
     */

	for (n = 0 ; interfaces[n].window ; n++)
	{
		if (XtIsRealized(interfaces[n].window))
		{
    		XChangeWindowAttributes(
				dpy, XtWindow(interfaces[n].window), CWCursor, &attrs);
		}
	}
	XFlush (dpy);
}
