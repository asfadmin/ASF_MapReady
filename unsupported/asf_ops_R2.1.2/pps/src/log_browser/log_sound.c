
#ifndef lint
static char sccsid[] = "@(#)log_sound.c	1.1  12/19/96";
#endif

#ifdef MODULE_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Task:           N/A
Module:         log_sound.c
Title:			Sound module.
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
------------------------------------------------------------------------------

Module Overview:	This provides the sound-related functions.

Procedures/Functions called from other modules:
	CreateSoundPopup() ;
	PopupSound() ;
	Beep() ;

Procedures/Functions called in other modules:
	UpdateStatusBar() ;

*************************************************************************** */
#endif

/*  =========================  Includes  =========================  */

#include "log_main.h"

#include <Xm/PushBG.h>
#include <Xm/BulletinB.h>
#include <Xm/RowColumn.h>


/*  =========================  Defines  =========================  */

#define SOUND_OFF	0
#define SOUND_SET	1

#define	BELL_VOLUME	0

/*  =========================  Types  =========================  */


/*  =========================  Global Variables  =========================  */

static	Widget	soundOptionDialog, optionmenu ;

/*  =========================  Functions  =========================  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void CreateSoundPopup (parent)

Description:	This creates the sound toggle pop up dialog.

Arguments:
        parent --	the parent for the dialog.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	create the dialog that everything else will reside on 
	Create the two options for the option menu 
	create the option menu itself 
	add callbacks for selecting the two options
	clean up the string allocations

*************************************************************************** */
#endif

void	CreateSoundPopup(parent)
Widget	parent ;
{
	Arg			myArgs[20] ;
	int			i = 0 ;
	XmString	dir, ttl ;
	Widget 		opt[2], soundMenu ;

	/*  create the dialog that everything else will reside on  */
	i = 0 ;
	ttl = XmStringCreateSimple("Toggle Sound Setting") ;
	XtSetArg(myArgs[i], XmNdialogTitle, ttl) ; i++ ;
	soundOptionDialog =
		XmCreateBulletinBoardDialog(parent, "SoundOptionDialog", myArgs, i) ;

	/*  Create the two options for the option menu  */
	optionmenu = XmCreatePulldownMenu(soundOptionDialog,"optionmenu",NULL,0) ;
	opt[0] = XmCreatePushButtonGadget(optionmenu," Off  ",NULL,0) ;
	opt[1] = XmCreatePushButtonGadget(optionmenu," On ",NULL,0) ;
	XtManageChildren(opt,2) ;

	/*  create the option menu itself  */
	i = 0 ;
	dir = XmStringCreateSimple(" Choose Sound Mode: ") ;
	XtSetArg(myArgs[i], XmNmnemonic, 'S') ; i++ ;
	XtSetArg(myArgs[i], XmNlabelString, dir) ; i++ ;
	XtSetArg(myArgs[i], XmNsubMenuId, optionmenu) ; i++ ;
	if (SOUND_ON)
	{
		XtSetArg(myArgs[i], XmNmenuHistory, opt[1]) ; i++ ;
	}
	else
	{
		XtSetArg(myArgs[i], XmNmenuHistory, opt[0]) ; i++ ;
	}
	soundMenu = XmCreateOptionMenu(soundOptionDialog,"SoundToggle",myArgs,i) ;
	XtManageChild(soundMenu) ;

	XtAddCallback(opt[0],XmNactivateCallback, PopupSound, (caddr_t)SOUND_OFF) ;
	XtAddCallback(opt[1],XmNactivateCallback, PopupSound, (caddr_t)SOUND_SET) ;

	/*  clean up my mess  */
	XmStringFree(dir) ;
	XmStringFree(ttl) ;

}	/*  CreateSoundPopup  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void PopupSound (w, reason, call)

Description:	Pops up the sound toggle dialog and responds to users
				pressing the on or off switch.

Arguments:
        w --		unused.
		reason --	the action to be taken 
					(either POP_UP_SOUND, SOUND_OFF, or SOUND_SET).
        call --		unused.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	The XtIsManaged check on the option menu before 
					unmanaging the popup is necessary because of a bug
					that OSF won't admit to, where an option menu will
					send a "selection made" callback before a selection
					has been made.

Pseudo Code:
	take action based on the "reason" this was called: 
		case POP_UP_SOUND:	pop up the dialog
		case SOUND_OFF:		turn the global sound variable off.
		case SOUND_SET:		turn the global sound variable on.
	if the reason was anything but POP_UP_SOUND,
		pop down the dialog.

*************************************************************************** */
#endif

void	PopupSound(w, reason, call)
Widget		w ;
int			reason ;
XtPointer	call ;
{
	/*  take action based on the "reason" this was called  */
	switch(reason)  {
		case POP_UP_SOUND:	XtManageChild(soundOptionDialog)  ;
							break ;
		case SOUND_OFF:		Resources.soundOn = False ;
							break ;
		case SOUND_SET:		Resources.soundOn = True ;
							break ;
		default:	LOG_ERROR("Invalid reason passed to PopupSound") ;
					return ;
	}	/*  end of switch  */

	if (reason!=POP_UP_SOUND && XtIsManaged(optionmenu)==False)
	{
		/*  pop down sound mode box  */
		XtUnmanageChild(soundOptionDialog) ;

		/*  update the status bar  */
		UpdateStatusBar() ;

	}  /*  if (reason!=POP_UP_SOUND && XtIsManaged(optionmenu)==False) */

}	/*  PopupSound  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void Beep (w)

Description:	Makes a strange, haunting "beep" sound.

Arguments:
        w --	any widget used in this program.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	On the current hardware, the volume argument has no
					effect.  If in the future it does, this should 
					perhaps be expanded to include volume as a parameter.

Pseudo Code:	Beep the bell.

*************************************************************************** */
#endif

void    Beep(w) 
Widget	w ;
{
	/*  Beep the bell; Note:  bell volume is ignored by current H/W  */	
	XBell(XtDisplay(w), BELL_VOLUME) ;

}	/*  Beep  */
