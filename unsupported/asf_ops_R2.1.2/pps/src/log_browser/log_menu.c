

#ifndef lint
static char sccsid[] = "@(#)log_menu.c	1.1  12/19/96";
#endif

#ifdef MODULE_HDR
/* ***************************************************************************

Project:        NOCC-RT
Program Set:    NUI - NOCC User Interface
Assembly:       N/A
Task:           N/A
Module:         log_menu.c
Title:
------------------------------------------------------------------------------
Modification History:

  Date            By               Description
------------------------------------------------------------------------------
09/17/92		Bill West		Initial Delivery
05/06/93		Bill West		Added Exclusive Filter option.
08/13/93		Mike Tankenson	Added Info about original author(s)
------------------------------------------------------------------------------

Module Overview:	Provides menuBar-related functions.

Procedures/Functions called from other modules:	none.

Procedures/Functions called in other modules:	CreateMenuOptions()

*************************************************************************** */
#endif

/*  =========================  Includes  =========================  */

#include "log_main.h"

#include <Xm/RowColumn.h>
#include <Xm/CascadeB.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>


/*  =========================  Defines  =========================  */

#define	NUM_LOG_OPTIONS		3
#define	NUM_FILTER_OPTIONS	2
#define	NUM_OPTION_OPTIONS	3
#define	NUM_PRINT_OPTIONS	2

#ifdef ADD_INFO
#define	NUM_INFO_OPTIONS	1
#endif /* ADD_INFO */

/*  =========================  Types  =========================  */

typedef struct menuOption {
	char				 *label ;
	WidgetClass 		 *class ;
	char				  mnemonic ;
	void				(*callback)() ;
	int					  client ;
	struct menuOption	 *subitems ;
}	menuOption_t ;


/*  =========================  Prototypes  =========================  */

static  Widget	makeMenu(/* Widget parent, char *title, char *mnemonic,
							menuOption_t *options, int numOptions */) ;


/*  =====================  Global Variables  =====================  */

static  menuOption_t	log_menu_options[NUM_LOG_OPTIONS] = {
{" Open ",	&xmPushButtonGadgetClass, 'O',	PopupLog,		0,			NULL},
{" ---- ",  &xmSeparatorGadgetClass, NULL,	NULL,			NULL,		NULL},
{" Quit ",	&xmPushButtonGadgetClass, 'Q',	FinalCleanupCB, LOG_EXIT_OK,NULL}
} ;

static	menuOption_t	filter_menu_options[NUM_FILTER_OPTIONS] = {
{" Inclusive ", &xmPushButtonGadgetClass, 'I', PopupFilter, INCLUSION_FILTER, NULL},
{" Exclusive ", &xmPushButtonGadgetClass, 'E', PopupFilter, EXCLUSION_FILTER, NULL},
} ;

static  menuOption_t	option_menu_options[NUM_OPTION_OPTIONS] = {
{" Filter ",&xmPushButtonGadgetClass, 'F', NULL, NUM_FILTER_OPTIONS, filter_menu_options},
{" Sound ",	&xmPushButtonGadgetClass, 'S',	PopupSound,		POP_UP_SOUND,	NULL},
{" View ",  &xmPushButtonGadgetClass, 'V',	PopupBrowseMode,POP_UP_BROWSE,	NULL},
} ;

static  menuOption_t	print_menu_options[NUM_PRINT_OPTIONS] = {
{" Entire Log ",		&xmPushButtonGadgetClass, 'L',	PopupPrint, PRINT_ALL,	NULL},
{" Selected Entries ",	&xmPushButtonGadgetClass, 'E',	PopupPrint, PRINT_PART,	NULL}
} ;

#ifdef ADD_INFO
static  menuOption_t	print_info_options[NUM_INFO_OPTIONS] = {
{" Created by JPL Section 363 (B. West) ",  &xmPushButtonGadgetClass, NULL,  NULL,           NULL, NULL},
} ;
#endif /* ADD_INFO */

/*  =========================  Functions  =========================  */

#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        void CreateMenuOptions (parent)

Description:	This creates the pulldown menus and their button gadget
				children which reside on the menu bar.

Arguments:
        parent --	The menubar widget.

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	Make the Log Open + Quit menu
	Make the Options menu
	Make the Print menu

*************************************************************************** */
#endif

void	CreateMenuOptions(parent)
Widget	parent ;
{
	/*  Make the Log Open + Quit menu  */
	(void) makeMenu(parent, "  LOG  ", 'L', 
					log_menu_options, NUM_LOG_OPTIONS) ;

	/*  Make the Options menu  */
	(void) makeMenu(parent, "  OPTIONS  ", 'O', 
					option_menu_options, NUM_OPTION_OPTIONS) ;

	/*  Make the Print menu  */
	(void) makeMenu(parent, "  PRINT  ", 'P', 
					print_menu_options, NUM_PRINT_OPTIONS) ;

#ifdef ADD_INFO
	/*  Make the Info button */
	(void) makeMenu(parent, "  INFO  ", 'I', 
					print_info_options, NUM_INFO_OPTIONS) ;
#endif /* ADD_INFO */

}	/*  CreateMenuOptions  */


#ifdef FUNCT_HDR
/* ***************************************************************************

Synopsis:
        static Widget makeMenu (parent, title, mnemonic, options,
        numOptions)

Description:	This actually creates the pulldown menu widgets and button 
				gadgets.

Arguments:
        parent --		the menu bar widget.
        title --		the title of the pull down mneu.
        mnemonic --		the mneumonic of the title.
        options --		a structure containing details on the button gadgets options.
        numOptions --	how many options are there?

Return Value:	none.

Messages/Data Stores:	none.

Notes/Constraints:	none.

Pseudo Code:
	create the menu button and it's pulldown
	for each of the options
		if this is a pull-right menu
			recursive call to make pull-right menu
		else
			create the option on the pulldown

*************************************************************************** */
#endif

static  Widget	makeMenu(parent, title, mnemonic, options, numOptions)
Widget			parent ;
char			*title ;
char			 mnemonic ;
menuOption_t	*options ;
int				 numOptions  ;
{
	Widget	  pulldown, cascade, label ;
	int		 i, o ;
	XmString	str ;

	/*  create the menu button and it's pulldown  */
	pulldown = XmCreatePulldownMenu(parent, "pulldown", NULL, 0) ;

	str = XmStringCreateSimple(title) ;
	cascade = XtVaCreateManagedWidget(title, xmCascadeButtonWidgetClass,
										parent, XmNsubMenuId, pulldown,
										XmNlabelString, str,
										XmNmnemonic, mnemonic, NULL) ;
	XmStringFree(str) ;

	/*  create the options on the pulldown  */
	for (i=0 ; i<numOptions ; i++)
	{
		/*  see if this is a pull-right menu  */
		if (options[i].subitems)
			/*  recursive call to make pull-right menu  */
			label = makeMenu(pulldown, options[i].label, options[i].mnemonic,
							 options[i].subitems, options[i].client) ;
			/*  Note:  the client data (which is not needed for pull-right
			 *	menus is here used to hold the number of options on the
			 *	sub-menu.
			 */
		else
			/*  make a regular menu option  */
			label = XtVaCreateManagedWidget(options[i].label,
											*options[i].class,
											pulldown, NULL) ;
		if (options[i].mnemonic)
			XtVaSetValues(label, XmNmnemonic, options[i].mnemonic, NULL) ;
		if (options[i].callback)
			XtAddCallback(label, XmNactivateCallback, options[i].callback,
							(XtPointer) options[i].client) ;

	}	/*  for  */

	return(cascade) ;

}	/*  makeMenu  */
