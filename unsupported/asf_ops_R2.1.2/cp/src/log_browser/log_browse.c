

#ifndef lint
static char sccsid[] = "@(#)log_browse.c	1.6 93/05/06 15:38:57 ";
#endif

#ifdef MODULE_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Task:           N/A
Module:         log_browse.c
Title:			log browse mode toggle functions

------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
------------------------------------------------------------------------------

Module Overview:	This allows the creation of widgets which in turn allow
					the user to toggle the view mode from "browse" to "update".
					In update mode, if a new record is appended to the list,
					the user is forced to view it by showing the last page
					of entries.  In browse mode, this does not happen.

Procedures/Functions called from other modules:
	CreateBrowseModePopup()
	PopupBrowseMode()

Procedures/Functions called in other modules:
	UpdateStatusBar()



*************************************************************************** */
#endif

/*  =========================  Includes  =========================  */

#include "log_main.h"

#include <Xm/PushBG.h>
#include <Xm/BulletinB.h>
#include <Xm/RowColumn.h>


/*  =========================  Defines  =========================  */

#define	BROWSE_OFF	0
#define	BROWSE_ON	1


/*  =========================  Types  =========================  */


/*  =========================  Global Variables  =========================  */

static	Widget	browseOptionDialog, optionmenu ;

/*  =========================  Functions  =========================  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void CreateBrowseModePopup (parent)

Description:	Creates the toggle button on a popup dialog for toggling
				the view mode.

Arguments:
        parent --	parent widget for the dialog.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	create the dialog that everything else will reside on
	Create the two options for the option menu
	add callbacks for selecting the two option

*************************************************************************** */
#endif

void	CreateBrowseModePopup(parent)
Widget	parent ;
{
	Arg			myArgs[20] ;
	int			i = 0 ;
	XmString	dir, ttl ;
	Widget		opt[2], browseMenu ;

	/*  create the dialog that everything else will reside on  */
	i = 0 ;
	ttl = XmStringCreateSimple("Toggle View Setting") ;
	XtSetArg(myArgs[i], XmNdialogTitle, ttl) ; i++ ;
	browseOptionDialog =
		XmCreateBulletinBoardDialog(parent, "BrowseOptionDialog", myArgs, i) ;

	/*  Create the two options for the option menu  */
	optionmenu = XmCreatePulldownMenu(browseOptionDialog, "optionmenu", 
										NULL, 0) ;
	opt[0] = XmCreatePushButtonGadget(optionmenu, "  View New Entries  ", 
										NULL, 0) ;
	opt[1] = XmCreatePushButtonGadget(optionmenu, " Browse Old Entries ", 
										NULL, 0) ;
	XtManageChildren(opt,2) ;

	/*	create the option menu itself  */
	i = 0 ;
	dir = XmStringCreateSimple(" Select View Mode: ") ;
	XtSetArg(myArgs[i], XmNmnemonic, 'V') ; i++ ;
	XtSetArg(myArgs[i], XmNlabelString, dir) ; i++ ;
	XtSetArg(myArgs[i], XmNsubMenuId, optionmenu) ; i++ ;
	if (BROWSE_MODE)
	{
		XtSetArg(myArgs[i], XmNmenuHistory, opt[1]) ; i++ ;
	}
	else
	{
		XtSetArg(myArgs[i], XmNmenuHistory, opt[0]) ; i++ ;
	}

	browseMenu = XmCreateOptionMenu(browseOptionDialog,"BrowseToggle",myArgs,i) ;
	XtManageChild(browseMenu) ;

	/*  add callbacks for selecting the two options */
	XtAddCallback(opt[0],XmNactivateCallback, PopupBrowseMode, (caddr_t)BROWSE_OFF) ;
	XtAddCallback(opt[1],XmNactivateCallback, PopupBrowseMode, (caddr_t)BROWSE_ON) ;

	/*  clean up my mess  */
	XmStringFree(dir) ;
	XmStringFree(ttl) ;

}	/*  CreateBrowseModePopup  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void PopupBrowseMode (w, reason, call)

Description:	Pops up the view mode dialog to allow the user to change it.

Arguments:
        w --		unused.
        reason --	Pop up the dialog, set view to browse, or set view to update.
        call --		unused.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	based on the "reason" this was called, do that:
		PopUp:  			manage the dialog.
		Browse or Update: 	pop down browse mode box
							update the status bar

*************************************************************************** */
#endif

void	PopupBrowseMode(w, reason, call)
Widget		w ;
int			reason ;
XtPointer	call ;
{
	
	/*  based on the "reason" this was called, do that  */

	switch(reason)	{
		case POP_UP_BROWSE:	XtManageChild(browseOptionDialog)  ;
							break ;
		case BROWSE_OFF:	Resources.browseMode = False ;
							break ;
		case BROWSE_ON:		Resources.browseMode = True ;
							break ;
		default:	LOG_ERROR("Invalid reason passed to PopupBrowseMode") ;
					return ;
	}	/*  end of switch  */

	if (reason!=POP_UP_BROWSE && XtIsManaged(optionmenu)==False)
	{
		/*  pop down browse mode box  */
		XtUnmanageChild(browseOptionDialog) ;

		/*  update the status bar  */
		UpdateStatusBar() ;

	}  /*  if (reason!=POP_UP_BROWSE && XtIsManaged(optionmenu)==False) */

}	/*  PopupBrowseMode  */


