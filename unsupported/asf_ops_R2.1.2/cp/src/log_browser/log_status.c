

#ifndef lint
static char sccsid[] = "@(#)log_status.c	1.8 93/05/06 15:39:03 ";
#endif

#ifdef MODULE_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Task:           N/A
Module:         log_status.c
Title:			Status Bar functions.
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
05/06/93		Bill West		Handles Exclusive Filtering.
------------------------------------------------------------------------------

Module Overview:	This provides the functions for creating and managing
					the status bar which shows the current state of the
					browser.

Procedures/Functions called from other modules:
	CreateStatusBar()
	UpdateStatusBar()

Procedures/Functions called in other modules:
	FontHeight()

*************************************************************************** */
#endif

/*  =========================  Includes  =========================  */

#include "log_main.h"

#include <Xm/RowColumn.h>
#include <Xm/Label.h>

/*  =========================  Defines  =========================  */

#define	LOG	0
#define	SOUND	1
#define	MODE	2
#define	FILTER	3

#define	NUM_LABELS	4


/*  =========================  Types  =========================  */

/*  =========================  Prototypes  =========================  */

/*  =====================  Global Variables  =====================  */

static	Widget	statusLabel[NUM_LABELS] ;


/*  =========================  Functions  =========================  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        Widget CreateStatusBar (parent)

Description:	This creates the widgets needed to display the status
				information.

Arguments:
        parent --	form on which the status bar resides.

Return Value:	The status bar's top level (row column) widget.

Messages/Data Stores:	none.

Notes/Constraints:	The status bar's top level widget is returned because
					it is later used as an attachment to the main list 
					part of the browser.

Pseudo Code:
	create the row column that contains all the labels 
	create a blank string for the initial state of the labels 
	create the labels that sit on the row column 
	get the height of the current font 
	use the height to ensure that the status bar 
		is tall enough for two rows of labels. 
	free up the string and return the row column 

*************************************************************************** */
#endif

Widget	CreateStatusBar(parent)
Widget	parent ;
{
	Widget		rc ;
	int			l, height ;
	XmString	blank ;
	XmFontList	font ;

	/*  create the row column that contains all the labels  */
	rc = XtVaCreateManagedWidget("StatusBar",
			xmRowColumnWidgetClass,	parent,
			XmNentryBorder,			1,
			XmNadjustLast,			False,
			XmNorientation,			XmHORIZONTAL,
			XmNresizeHeight,		False,
			XmNresizeWidth,			False,
			XmNspacing,				10,
			XmNmarginWidth,			10,
			XmNleftAttachment,		XmATTACH_FORM,
			XmNrightAttachment,		XmATTACH_FORM,
			XmNbottomAttachment,	XmATTACH_FORM,
			NULL) ;
		
	/*  create a blank string for the initial state of the labels  */
	blank = XmStringCreateSimple("  ") ;

	/*  create the labels that sit on the row column  */
	for (l=0 ; l<NUM_LABELS ; l++)
		statusLabel[l] = XtVaCreateWidget("StatusLabel",
							xmLabelWidgetClass,	rc,
							XmNlabelString,		blank,
							XmNmarginWidth,		10,
							NULL) ;

	XtManageChildren(statusLabel, NUM_LABELS) ;
			
	/*  get the height of the current font  */
	height = FontHeight(statusLabel[1]) ;

	/*  use the height to ensure that the status bar is 
	 *	tall enough for two rows of labels.  
	 */
	XtVaSetValues(rc,
				XmNheight,	height*2+20,
				NULL) ;

	/*  free up the string and return the row column  */
	XmStringFree(blank) ;
	
	return(rc) ;
	
}	/*  CreateStatusBar  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void UpdateStatusBar ()

Description:	Updates the status bar's labels to evince the current state
				of various aspects of the program's idiom.

Arguments:	none.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	set up the labels to be displayed 
	apply the labels to the label widgets, freeing them as we go 

*************************************************************************** */
#endif

void	UpdateStatusBar()
{
	char		*string[NUM_LABELS] ;
	XmString	 xstring[NUM_LABELS] ;
	int			 l ;

	/*  set up the labels to be displayed  */
	string[LOG] = malloc(MAX_LOG_NAME_LEN+20) ;
	if (string[LOG])
	{
		if (LOG_SELECTED)
			sprintf(string[LOG], "Log = %s%s", LogPath, LogName) ;
		else
			strcpy(string[LOG], "<no log>") ;
	}

	if (SOUND_ON)
		string[SOUND] = (char *)strdup("Sound = ON") ;
	else
		string[SOUND] = (char *)strdup("Sound = off") ;

	if (BROWSE_MODE)
		string[MODE] = (char *)strdup("Mode = browse") ;
	else
		string[MODE] = (char *)strdup("Mode = UPDATE") ;

	string[FILTER] = malloc(COMMAND_LINE_FILTER_LEN+50) ;
	if (string[FILTER])
	{
		strcpy(string[FILTER], "Filter = ") ;
		if (LOG_FILTERED)
		{
			if (Filter_type == EXCLUSION_FILTER)
				strcat(string[FILTER], "NOT: ") ;
			strcat(string[FILTER], "\"") ;
			if (Filter_on[0]==True)
				strcat(string[FILTER], Filter[0]) ;
			if (DOUBLE_FILTERED)
			{
				if (FilterCriteria==or)
					strcat(string[FILTER], "\" -OR- \"") ;
				else
					strcat(string[FILTER], "\" -AND- \"") ;
			}
			if (Filter_on[1]==True)
				strcat(string[FILTER], Filter[1]) ;
			strcat(string[FILTER], "\"") ;
		}
		else
			strcat(string[FILTER], "<none>") ;
	}
	
	/*  apply the labels to the label widgets, freeing them as we go  */
	for (l=0 ; l<NUM_LABELS ; l++)
		if (string[l])
		{
			xstring[l] = XmStringCreateLtoR(string[l], 
							XmSTRING_DEFAULT_CHARSET) ;

			if (xstring[l])
			{
				XtVaSetValues(statusLabel[l],
								XmNlabelString,	xstring[l],
								NULL) ;
				XmStringFree(xstring[l]) ;
			}
		
			(void) free(string[l]) ;
		}
	
}	/*  UpdateStatusBar  */

