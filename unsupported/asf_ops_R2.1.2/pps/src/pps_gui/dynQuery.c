/*-----------------------------------------*/
/* for vi: set ts=4 sw=4
/*-----------------------------------------*/

#include <stdlib.h>
#include <dirent.h>
#include <limits.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <math.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <Xm/CascadeB.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>

#include "db_util.h"
#include "dynQuery_util.h"
#include "gui_common.h"

static const char rcsid[] = "@(#)dynQuery.c	1.16  03/05/97";

#define		DEFAULT_INT_WIDTH		10
#define		DEFAULT_FLOAT_WIDTH		12
#define		DEFAULT_TEXT_WIDTH		12

#define		INITIAL_NUM_LISTS		5
#define		MAIN_FORM_SPACING		5
#define		QUERY_FORM_SPACING		10
#define		ROWS_PER_FETCH			100

#define		MAX_NUM_MENU_BUTTONS	10

typedef struct globalDataS
{
	String		_rootPath;

} GlobalData, *GlobalDataPtr;

static enum
{
	LOGIN_PB,
	DATABASE_PB,
	QUIT_PB,
	NUM_FILE_PBS
};
const String menuTitleNames[] =
{
	"fileMenu"
};
const String menuTitleStrings[] =
{
	"File"
};

const String menuBtnNames[][MAX_NUM_MENU_BUTTONS] =
{
	{
		"loginPB",
		"databasePB",
		"quitPB",
		0
	}
};

const String menuBtnStrings[][MAX_NUM_MENU_BUTTONS] =
{
	{
		"Login...",
		"Database...",
		"Quit",
		0
	}
};

/*----------------------------------------*/
/* global variables                       */
/*----------------------------------------*/
Widget			mainForm=0;
Widget			menuBar=0;

Widget			queryDBAcctDlg=0;
Widget			changeDatabaseDlg=0;

static Widget	createMenuBar(Widget parent);

static void		loginCB(Widget, XtPointer, XtPointer);
static void		changeDatabaseCB(Widget, XtPointer, XtPointer);
static void		quitCB(Widget, XtPointer, XtPointer);

GlobalData		globalData;		/* global resource data */
XtAppContext	appContext;		/* X application context */
Widget			topLevel=0;		/* top level shell */

CS_CONNECTION	*connection=0;		/* sybase connection */

/*----------------------------------------*/
/* extern variables                       */
/*----------------------------------------*/
extern Widget			lowerForm;		/* main form */

static XtResource resources[] =
{
	{	"rootPath", "RootPath", XtRString, sizeof(String),
		XtOffset(GlobalDataPtr, _rootPath), XtRString, "." }
};

static XrmOptionDescRec options[] =
{
	{ "-root", "*rootPath", XrmoptionSepArg, (XtPointer) 0 }
};


main(
unsigned int	argc,		/* arg count */
char**			argv)		/* arg vector */
{
	PPS_LOGIN_STRUCT	loginStruct;
	PPS_COLUMN_STRUCT	*columnDescList=0;
	int					columnListSize=0;
	int					i=0;
	Widget				upperForm;
	Widget				resultLabel;
	Widget				itemsLabel;
	Widget				queryPB;
	Widget				settingLabel;
	Widget				orderLabel;
	XmString			xmString;
	Dimension			width=0;

	topLevel = XtAppInitialize (&appContext, "DynQuery",
								options, XtNumber(options),
								(int*)&argc, argv,
								(char**)0, (ArgList)0, 0);

#if 0
	/* get the resources from database */
	XtGetApplicationResources (topLevel, (XtPointer)&globalData,
						resources, XtNumber(resources), 0, 0);
#endif

	/*----------------------------------------------*/
	/* create a main form to hold filters and order */
	/*----------------------------------------------*/
	mainForm = XtVaCreateManagedWidget("mainForm",
						xmFormWidgetClass, topLevel,
						XmNhorizontalSpacing, MAIN_FORM_SPACING,
						XmNverticalSpacing, MAIN_FORM_SPACING,
						0);

	/* create a menu bar */
	menuBar = createMenuBar(mainForm);
	XtVaSetValues(menuBar,
						XmNtopAttachment, XmATTACH_FORM,
						XmNleftAttachment, XmATTACH_FORM,
						XmNrightAttachment, XmATTACH_FORM,
						0);

	/* realize the toplevel widget and enter the main loop */
	XtRealizeWidget (topLevel);

	loginCB(mainForm, 0, 0);

	/* enter main loop */
	XtAppMainLoop (appContext);

	return (0);

} /* main */

static Widget
createMenuBar(
Widget		parent)
{
	Widget	menuBar;
	Widget	*cascadeBtns;
	Widget	subMenu;
	Widget	*pushBtns;
	int		i, k;
	int		numCascadeBtns = XtNumber(menuTitleNames);
	XmString	xmString;

	menuBar = XmCreateMenuBar(parent, "menuBar", 0, 0);
	XtManageChild(menuBar);

	cascadeBtns = (Widget*) XtMalloc(numCascadeBtns * sizeof(Widget));

	/* create the cascade buttons */
	for (i=0; i < numCascadeBtns; i++)
	{
		/* create each pulldown menu */
		subMenu = XmCreatePulldownMenu(menuBar, "subMenu", 0, 0);
		pushBtns = (Widget*) XtMalloc(MAX_NUM_MENU_BUTTONS * sizeof(Widget));
		k=0;
		while (menuBtnNames[i][k] != 0)
		{
			xmString = CREATE_XMSTRING(menuBtnStrings[i][k]);
			pushBtns[k] = XtVaCreateManagedWidget(menuBtnNames[i],
							xmPushButtonWidgetClass, subMenu,
							XmNlabelString, xmString,
							0);
			XmStringFree(xmString);
			k++;
		}

		xmString = CREATE_XMSTRING(menuTitleStrings[i]);
		cascadeBtns[i] = XtVaCreateManagedWidget(menuTitleNames[i],
						xmCascadeButtonWidgetClass, menuBar,
						XmNlabelString, xmString,
						XmNsubMenuId, subMenu,
						0);
		XmStringFree(xmString);
	}

	XtAddCallback(pushBtns[LOGIN_PB],
						XmNactivateCallback, loginCB, 0);
	XtAddCallback(pushBtns[DATABASE_PB],
						XmNactivateCallback, changeDatabaseCB, 0);
	XtAddCallback(pushBtns[QUIT_PB],
						XmNactivateCallback, quitCB, 0);

	return(menuBar);

} /* createMenuBar */

static void
quitCB(
Widget		widget,
XtPointer	clientData,
XtPointer	callData)
{
	exit(0);

} /* quitCB */

static void
loginCB(
Widget		widget,
XtPointer	clientData,
XtPointer	callData)
{
	if (queryDBAcctDlg == 0)
		queryDBAcctDlg = createQueryDBAcctDialog(widget);
	else
	{
		XtManageChild(queryDBAcctDlg);
		XMapRaised(XtDisplay(queryDBAcctDlg),
						XtWindow(XtParent(queryDBAcctDlg)));
	}

} /* loginCB */

static void
changeDatabaseCB(
Widget		widget,
XtPointer	clientData,
XtPointer	callData)
{
	if (changeDatabaseDlg == 0)
		createChangeDatabaseDialog(widget);
	else
	{
		XtManageChild(changeDatabaseDlg);
		XMapRaised(XtDisplay(changeDatabaseDlg),
						XtWindow(XtParent(changeDatabaseDlg)));
	}

} /* changeDatabaseCB */
