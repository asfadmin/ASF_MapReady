/*-----------------------------------------*
/* for vi: set ts=4 sw=4
/*-----------------------------------------*/

#include <string.h>

#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>

#include "gui_common.h"
#include "smart_list.h"

#define PPS_SMART_OPTION_ANY				"*"

static char SCCSFileId[] = "@(#)smart_list.c	1.6 23 Oct 1996";

typedef struct
{
	Widget*				labelWidget;
	Widget*				valueWidget;
	PPSSmartWidgetSpec*	specs;
	int					numSpecs;

} PPSSmartWidgetRec;

typedef struct _PPSSmartClientData
{
	Widget				masterList;
	Widget				attachList;
	PPSSmartWidgetRec	specRec;

} PPSSmartClientData, *PPSSmartClientDataP;

#if 0
/*-------------------------------------------------------*/
/* definition of the param list popup dialog             */
/*-------------------------------------------------------*/
PPSSmartWidgetSpec _ppsSmartWidgets[] =
{
    /*-------------------------------------------------------------------*/
    /* widget name      label           widget_type           attributes */
    /*-------------------------------------------------------------------*/
    { 0, 0, "comments", "Comments",      PPS_SMART_TEXT,        "" },
    { 0, 0, "name",     "Name",          PPS_SMART_TEXT,        "" },
    { 0, 0, "pixel",    "Pixel Spacing", PPS_SMART_FLOAT,       "2.0..5.0" },
    { 0, 0, "rev",      "Rev",           PPS_SMART_INT,         "1..10" },
    { 0, 0, "sen",  	"Sensor",       PPS_SMART_OPTION_MENU, "O,D" },
    { 0, 0, "station",  "Station",       PPS_SMART_OPTION_MENU, "FA,MC" },
    { 0, 0, "state",    "State",         PPS_SMART_OPTION_MENU,
                                     "PENDING,READY,AVAILABLE,SUBMITTED" }
}; 
#endif

/*-----------------------------------------------------------*/
/* ATTENTION: to override the default resources              */
/*            prepand "*smart_list_name" in front of these   */
/*-----------------------------------------------------------*/
static String resourceSpec[] = 
{
	/*----------------------------*/
	/* general                    */
	/*----------------------------*/
	"*paramList.dialogTitle:	Smart Parameters List",
	"*title.labelString:		Filters",
	"*editPB.labelString:		Edit...",
	"*list.visibleItemCount:	15",

	/*----------------------------*/
	/* for all text, int, float   */
	/*----------------------------*/
	"*text.columns:		20",

	0
};

static void 	ppsCreateAttachList(PPSSmartClientDataP	smartClientData);
static void		ppsSetDefaultResources(Widget, char* name);
static void		ppsCreateSmartTextWidget(
									Widget				parent,
									PPSSmartWidgetRec*	smartRec,
									int					index);
static void		ppsCreateSmartOptionWidget(
									Widget				parent,
									PPSSmartWidgetRec*	smartRec,
									int					index);

static Boolean	getSmartText(
									PPSSmartWidgetRec*	smartRec,
									int					i,
									char*				value);
static Boolean	getSmartOption(
									PPSSmartWidgetRec*	smartRec,
									int					i,
									char*				value);

static void addValueCB(Widget, XtPointer, XtPointer);
static void editValueCB(Widget, XtPointer, XtPointer);
static void getFiltersCB(Widget, XtPointer, XtPointer);

Widget
ppsCreateSmartList(
Widget					parent,
String					name,
PPSSmartWidgetSpec*		smartSpecs,
int						numSpecs)
{
	Widget			w;
	Widget			title;
	Widget			list;
	Widget			editPB;
	XmString		xmString;
	int				doubleClickInterval;
	int				maxLabelLen=0;
	int				i=0;
	XmFontList		fontList;
	XmFontContext	fontContext;
	short			charWidth=0;
	short			charHeight=0;
	Dimension		listWidth, listHeight;
	int				visibleItemCount;
	PPSSmartClientDataP smartClientData=0;
	Arg				args[10];
	int				n=0;

	ppsSetDefaultResources(parent, name);

	/*----------------------------------------------------------------*/
	/* create a smart widget client data                              */
	/*----------------------------------------------------------------*/
	smartClientData = (PPSSmartClientDataP)
						XtMalloc(sizeof(PPSSmartClientData));
	smartClientData->specRec.labelWidget = (Widget*)
						XtMalloc(sizeof(Widget) * numSpecs);
	smartClientData->specRec.valueWidget = (Widget*)
						XtMalloc(sizeof(Widget) * numSpecs);

	/*----------------------------------------------------------------*/
	/* create a form widget to hold the edit pb and scrolled list     */
	/*----------------------------------------------------------------*/
	w = XtVaCreateManagedWidget(name, xmFormWidgetClass, parent,
						XmNshadowType, XmSHADOW_ETCHED_IN,
						XmNshadowThickness, 2,
						XmNhorizontalSpacing, 10,
						XmNverticalSpacing, 10,
						0);

	/*----------------------------------------------*/
	/* create the "Edit" push button                */
	/*----------------------------------------------*/
	editPB = XtVaCreateManagedWidget("editPB",
					xmPushButtonWidgetClass, w,
					XmNtopAttachment, XmATTACH_FORM,
					XmNrightAttachment, XmATTACH_FORM,
					0);
	/*----------------------------------------------*/
	/* create a scrolled list                       */
	/*----------------------------------------------*/
	n=0;
	XtSetArg(args[n], XmNlistSizePolicy, XmCONSTANT); n++;
	list = XmCreateScrolledList(w, "list", args, n);
	smartClientData->masterList = list;
	smartClientData->specRec.specs = smartSpecs;
	smartClientData->specRec.numSpecs = numSpecs;

	XtVaGetValues(list,
					XmNdoubleClickInterval, &doubleClickInterval,
					XmNfontList, &fontList,
					XmNvisibleItemCount, &visibleItemCount,
					0);
	doubleClickInterval = (doubleClickInterval > MIN_DOUBLE_CLICK_INTERVAL ?
					doubleClickInterval : MIN_DOUBLE_CLICK_INTERVAL);

	XtVaSetValues(list,
					XmNselectionPolicy, XmSINGLE_SELECT,
					XmNdoubleClickInterval, doubleClickInterval,
					0);
	XtManageChild(list);

#if 0
	XtAddCallback(list, XmNdefaultActionCallback,
					addValueCB, 0);
#endif

	XtVaSetValues(XtParent(list),
					XmNtopAttachment, XmATTACH_WIDGET,
					XmNtopWidget, editPB,
					XmNbottomAttachment, XmATTACH_FORM,
					XmNleftAttachment, XmATTACH_FORM,
					XmNrightAttachment, XmATTACH_FORM,
					0);

	/*----------------------------------------------*/
	/* calculate the width for the list's parent    */
	/*----------------------------------------------*/
	if (XmFontListInitFontContext (&fontContext, fontList))
	{
		XmStringCharSet charset;
		XFontStruct*    font;
		if (XmFontListGetNextFont (fontContext, &charset, &font))
		{
			charWidth = (font->max_bounds.width + font->min_bounds.width)/2;
			charHeight = font->ascent + font->descent;
		}
	}
	for(i=0; i < numSpecs; i++)
	{
		int labelLen = strlen(smartSpecs[i].label);
		if (labelLen > maxLabelLen)
			maxLabelLen = labelLen;
	}

	listWidth = charWidth * maxLabelLen * 2 + 40;
	listHeight = charHeight * visibleItemCount + 40;
	XtVaSetValues(w,
					XmNwidth, listWidth,
#if 0
					XmNheight, listHeight,
#endif
					0);

	XtAddCallback(editPB, XmNactivateCallback, addValueCB, smartClientData);

	XtVaSetValues(w,
						XmNuserData, (XtPointer) smartClientData,
						0);
	return(w);

}/* ppsCreateSmartList */

static void
ppsSetDefaultResources(
Widget 		w,
char*		name)
{
	int				i=0;
	XrmDatabase		rdb = XrmGetStringDatabase("");
	while (resourceSpec[i] != 0)
	{
		char buf[PPS_BIG_SIZE];
		sprintf(buf, "*%s%s", name, resourceSpec[i++]);
		XrmPutLineResource(&rdb, buf);
	}

	/*---------------------------------------------------------*/
	/* merge them into the Xt database, with lowest precedence */
	/* the resources added later have the higher precedence    */
	/*---------------------------------------------------------*/
	if (rdb)
	{
		Display* display = XtDisplay(w);
		/* merge rdb into display's database, no override */
		XrmCombineDatabase (rdb, &(display->db), False);

	}

}/* ppsSetDefaultResources */

static void
addValueCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	Widget attachList = ((PPSSmartClientDataP) clientData)->attachList;

	if (attachList)
	{
		XtManageChild(attachList);
		XMapRaised(XtDisplay(XtParent(attachList)),
							XtWindow(XtParent(attachList)));
	}
	else
	{
		ppsCreateAttachList((PPSSmartClientDataP)clientData);
	}

}/* addValueCB */

static void
ppsCreateAttachList(
PPSSmartClientDataP		smartClientData)
{
	int 		i=0;
	Widget		okPB, separator;
	Widget		scrolledWindow, rc;
	Widget		newWidget=0;
	XmString	xmString;
	Widget		attachList;
	PPSSmartWidgetSpec*		smartSpecs = smartClientData->specRec.specs;
	PPSSmartWidgetRec*		smartRec = &(smartClientData->specRec);
	int			numSpecs = smartClientData->specRec.numSpecs;

	attachList = smartClientData->attachList =
			XmCreateFormDialog(smartClientData->masterList, "paramList", 0, 0);

	xmString = CREATE_XMSTRING("Ok");
	okPB = XtVaCreateManagedWidget("okPB",
				xmPushButtonWidgetClass, attachList,
				XmNlabelString, xmString,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment, XmATTACH_POSITION,
				XmNleftPosition, 45,
				0);
	XmStringFree(xmString);

	XtAddCallback(okPB, XmNactivateCallback, getFiltersCB, smartClientData);

	/* make the OK button the default button */
	XtVaSetValues(attachList,
				XmNhorizontalSpacing, 10,
				XmNverticalSpacing, 10,
				XmNheight, 400,
				XmNwidth, 400,
				XmNdefaultButton, okPB,
				0);

	separator = XtVaCreateManagedWidget("separator",
				xmSeparatorWidgetClass, attachList,
				XmNbottomAttachment, XmATTACH_WIDGET,
				XmNbottomWidget, okPB,
				XmNleftAttachment, XmATTACH_FORM,
				XmNleftOffset, 0,
				XmNrightAttachment, XmATTACH_FORM,
				XmNrightOffset, 0,
				0);

	scrolledWindow = XtVaCreateManagedWidget("scrolledWindow",
				xmScrolledWindowWidgetClass, attachList,
				XmNscrollingPolicy, XmAUTOMATIC,
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_WIDGET,
				XmNbottomWidget, separator,
				XmNleftAttachment, XmATTACH_FORM,
				XmNrightAttachment, XmATTACH_FORM,
				0);
	rc = XtVaCreateManagedWidget("rc",
				xmRowColumnWidgetClass, scrolledWindow,
				XmNentryAlignment, XmALIGNMENT_END,
				XmNorientation, XmHORIZONTAL,
				XmNpacking, XmPACK_COLUMN,
				XmNnumColumns, numSpecs,
				0);

	for (i=0; i < numSpecs; i++)
	{
		switch (smartSpecs[i].widgetType)
		{
			case PPS_SMART_TEXT:
			case PPS_SMART_INT:
			case PPS_SMART_FLOAT:
				ppsCreateSmartTextWidget(rc, smartRec, i);
				break;
			case PPS_SMART_OPTION_MENU:
				ppsCreateSmartOptionWidget(rc, smartRec, i);
				break;
		}
	}

	XtManageChild(attachList);

}/* ppsCreateAttachList */

static void
ppsCreateSmartTextWidget(
Widget				parent,
PPSSmartWidgetRec*	smartRec,
int					index)
{
	XmString xmString;

	if (parent == 0 || smartRec == 0)
	{
		fprintf(stderr, "ppsCreateSmartTextWidget: internal error.\n");
		exit(1);
	}

	xmString = CREATE_XMSTRING(smartRec->specs[index].label);
	smartRec->labelWidget[index] = XtVaCreateManagedWidget("label",
				xmLabelWidgetClass, parent,
				XmNlabelString, xmString,
				0);

	XmStringFree(xmString);
	smartRec->valueWidget[index] = XtVaCreateManagedWidget("text",
				xmTextFieldWidgetClass, parent,
				0);

}/* ppsCreateSmartTextWidget */

static void
ppsCreateSmartOptionWidget(
Widget				parent,
PPSSmartWidgetRec*	smartRec,
int					index)
{
	Widget	pulldown;
	XmString xmString;
	Arg		args[6];
	int		n, i;
	Widget*	pushButtons;
	int		numPushButtons;
	PPSSmartOptionAttr	*attributes = 0;
	char	*optionStr;

	if (parent == 0 || smartRec == 0)
	{
		fprintf(stderr, "ppsCreateSmartTextWidget: internal error.\n");
		exit(1);
	}

	attributes = (PPSSmartOptionAttr*) smartRec->specs[index].attributes;

	/* create the label */
	xmString = CREATE_XMSTRING(smartRec->specs[index].label);
	smartRec->labelWidget[index] = XtVaCreateManagedWidget("label",
				xmLabelWidgetClass, parent,
				XmNlabelString, xmString,
				0);
	XmStringFree(xmString);

	/*-----------------------------------------------------------*/
	/* create a pulldown for submenu then create the option menu */
	/*-----------------------------------------------------------*/
	pulldown = XmCreatePulldownMenu(parent, "pulldown", 0, 0);

	/*-----------------------------------------------------------*/
	/* create the pushbuttons for the pulldown menupane          */
	/*-----------------------------------------------------------*/
	numPushButtons = attributes->numOptions + 1;
	pushButtons = (Widget*)XtMalloc(sizeof(Widget) * numPushButtons);

	/* create the "*" button */
	xmString = CREATE_XMSTRING("*");
	pushButtons[0] = XtVaCreateWidget("any",
				xmPushButtonWidgetClass, pulldown,
				XmNlabelString, xmString,
				0);
	XmStringFree(xmString);

	optionStr = attributes->options;
	for(i=0; i < attributes->numOptions; i++)
	{
		xmString = CREATE_XMSTRING(optionStr);
		pushButtons[i+1] = XtVaCreateWidget(optionStr,
					xmPushButtonWidgetClass, pulldown,
					XmNlabelString, xmString,
					0);
		XmStringFree(xmString);
		optionStr += attributes->unitLen;
	}
	XtManageChildren(pushButtons, numPushButtons);


	n=0;
	XtSetArg(args[n], XmNsubMenuId, pulldown); n++;
	XtSetArg(args[n], XmNmarginHeight, 0); n++;
	XtSetArg(args[n], XmNmarginWidth, 0); n++;
	XtSetArg(args[n], XmNspacing, 0); n++;
	smartRec->valueWidget[index]= XmCreateOptionMenu(parent, "option", args, n);
	XtManageChild(smartRec->valueWidget[index]);

}/* ppsCreateSmartOptionWidget */

static void
getFiltersCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	int			i;
	char		value[PPS_BIG_SIZE];
	Boolean		hasValue;
	XmString	newListString;
	PPSSmartClientDataP		smartClientData = (PPSSmartClientDataP) clientData;
	PPSSmartWidgetRec*		smartRec = &(smartClientData->specRec);

	/*-----------------------------*/
	/* clear the master list first */
	/*-----------------------------*/
	XmListDeleteAllItems(smartClientData->masterList);

	for (i=0; i < smartClientData->specRec.numSpecs; i++)
	{
		hasValue = False;
		switch(smartRec->specs[i].widgetType)
		{
			case PPS_SMART_TEXT:
			case PPS_SMART_INT:
			case PPS_SMART_FLOAT:
				hasValue = getSmartText(&(smartClientData->specRec), i, value);
				break;
			case PPS_SMART_OPTION_MENU:
				hasValue = getSmartOption(&(smartClientData->specRec), i,value);
				break;
		}
		if (hasValue)
		{
			newListString = CREATE_XMSTRING(value);
			/* add the new string to the end of the master list */
			XmListAddItemUnselected(smartClientData->masterList,
									newListString, 0);
			XmStringFree(newListString);
		}
	}

}/* getFiltersCB */

static Boolean
getSmartText(
PPSSmartWidgetRec*	smartRec,
int					i,
char*				value)
{
	XmString	xmString;
	char* 		labelString=0;
	char* 		valueString=0;
	char		*head, *tail;

	valueString = XmTextFieldGetString(smartRec->valueWidget[i]);
	if (valueString == 0)
		return (0);
	else if (valueString[0] == '\0')
	{
		XtFree(valueString);
		return 0;
	}
	else
	{
		/*-----------------------------------------------*/
		/* take out the beginning and trailing blanks    */
		/*-----------------------------------------------*/
		head = valueString;
		while (*head == ' ') head++;
		tail = head + strlen(head);
		while (*tail == ' ') 
		{
			*tail = '\0';
			tail--;
		}
		if (*head == '\0')
		{
			XtFree(valueString);
			return 0;
		}

		XtVaGetValues(smartRec->labelWidget[i],
					XmNlabelString, &xmString,
					0);
		if (XmStringGetLtoR(xmString, XmSTRING_DEFAULT_CHARSET, &labelString))
		{
			(void)sprintf(value, "%s = %s", labelString, head);
			XtFree(labelString);
			XtFree(valueString);
			return(1);
		}
		else
		{
			XtFree(valueString);
			return(0);
		}
	}

}/* getSmartText */

static Boolean
getSmartOption(
PPSSmartWidgetRec*	smartRec,
int					i,
char*				value)
{
	XmString	xmString;
	char* 		labelString=0;
	char* 		optionString=0;

	XtVaGetValues(XmOptionButtonGadget(smartRec->valueWidget[i]),
					XmNlabelString, &xmString,
					0);
	if (XmStringGetLtoR(xmString, XmSTRING_DEFAULT_CHARSET, &optionString))
	{
		if (strcmp(optionString, PPS_SMART_OPTION_ANY) == 0)
		{
			XtFree(optionString);
			return(0);
		}
		XtVaGetValues(smartRec->labelWidget[i],
					XmNlabelString, &xmString,
					0);
		if (XmStringGetLtoR(xmString, XmSTRING_DEFAULT_CHARSET, &labelString))
		{
			(void)sprintf(value, "%s = '%s'", labelString, optionString);
			XtFree(labelString);
			XtFree(optionString);
			return(1);
		}
		else
		{
			XtFree(optionString);
			return(0);
		}
	}
	else
		return(0);

}/* getSmartOption */

Boolean
ppsGetSmartListStrings(
Widget					w,			/* IN */
String**				strings,	/* OUT */
int*					numStrings) /* OUT */
{
	PPSSmartClientDataP	clientData=0;
	XmString*	xmStrings;
	int			itemCount;
	int			i;

	XtVaGetValues(w,
					XmNuserData, &clientData,
					0);
	if (clientData == 0)
		return False;

	XtVaGetValues(clientData->masterList,
				XmNitems, &xmStrings,
				XmNitemCount, &itemCount,
				0);

	if (itemCount > 0)
	{
		*strings = (String*)XtMalloc(itemCount * sizeof(char*));
		for (i=0; i < itemCount; i++)
		{
			(void)XmStringGetLtoR(xmStrings[i], XmSTRING_DEFAULT_CHARSET,
								&((*strings)[i]));
		}
		*numStrings = itemCount;
		return True;
	}
	else
		return False;

} /* ppsGetSmartListStrings */

void
ppsFreeSmartListStrings(
String*				strings,	/* IN */
int					numStrings) /* IN */
{
	int			i;

	for (i=0; i < numStrings; i++)
		XtFree(strings[i]);
	XtFree((char*)strings);

} /* ppsFreeSmartListStrings */

Boolean
ppsSetSmartListStrings(
Widget					w,			/* IN */
String*					strings,	/* IN */
int						numStrings) /* IN */
{
	PPSSmartClientDataP	clientData=0;
	XmString*	xmStrings;
	int			i;

	XtVaGetValues(w,
					XmNuserData, &clientData,
					0);
	if (clientData == 0)
		return False;

	XmListDeleteAllItems(clientData->masterList);

	xmStrings = (XmString*) XtMalloc(numStrings * sizeof(XmString));
	for (i=0; i < numStrings; i++)
	{
		xmStrings[i] = CREATE_XMSTRING(strings[i]);
	}
	XmListAddItemsUnselected(clientData->masterList,
						xmStrings, numStrings, 0);

	for (i=0; i < numStrings; i++)
	{
		XmStringFree(xmStrings[i]);
	}
	XtFree(xmStrings);

} /* ppsSetSmartListStrings */
