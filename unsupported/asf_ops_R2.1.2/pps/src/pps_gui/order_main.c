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

#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/Text.h>

#include "order_list.h"
#include "gui_common.h"

static const char rcsid[] = "@(#) $Id$";

typedef struct globalDataS
{
	String		_rootPath;

} GlobalData, *GlobalDataPtr;

static XtResource resources[] =
{
	{	"rootPath", "RootPath", XtRString, sizeof(String),
		XtOffset(GlobalDataPtr, _rootPath), XtRString, "." }
};

static XrmOptionDescRec options[] =
{
	{ "-root", "*rootPath", XrmoptionSepArg, (XtPointer) 0 }
};

/*-----------------------------*/
/* global vars                 */
/*-----------------------------*/
GlobalData		globalData;		/* global resource data */
XtAppContext	appContext;		/* X application context */
Widget			_topLevel=0;		/* top level shell */


main(
unsigned int	argc,		/* arg count */
char**			argv)		/* arg vector */
{
	Widget			mainForm=0;		/* main form */
	Widget			orderList=0;	/* order list */
	int				i;
	PPSOrderClientDataP	orderListClientData;

	_topLevel = XtAppInitialize (&appContext, "Order_main",
								options, XtNumber(options),
								(int*)&argc, argv,
								(char**)0, (ArgList)0, 0);

#if 0
	/* get the resources from database */
	XtGetApplicationResources (_topLevel, (XtPointer)&globalData,
						resources, XtNumber(resources), 0, 0);
#endif

	/* create a main form as the child of top level shell */
	mainForm = XtVaCreateManagedWidget("mainForm",
						xmFormWidgetClass, _topLevel,
						XmNhorizontalSpacing, 10,
						XmNverticalSpacing, 10,
						0);

	{
	String* strings;
	strings = (char**) XtMalloc(10 * sizeof(String));
	for (i=0; i < 10; i++)
	{
		strings[i] = (String) XtMalloc(80 * sizeof(char));
	}
	strcpy(strings[0], "sally 1");
	strcpy(strings[1], "thuy 2");
	strcpy(strings[2], "douglas 3");
	orderList = ppsCreateOrderList(mainForm, "orderList",
						strings, 3, orderListClientData);
	XtVaSetValues(orderList,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		0);
	}

	/* realize the toplevel widget and enter the main loop */
	XtRealizeWidget (_topLevel);
	XtAppMainLoop (appContext);

	return (0);

} /* main */
