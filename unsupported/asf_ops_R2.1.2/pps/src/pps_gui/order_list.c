/*-----------------------------------------*/
/* for vi: set ts=4 sw=4
/*-----------------------------------------*/

#include <string.h>

#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>

#include "gui_common.h"
#include "order_list.h"

static char SCCSFileId[] = "@(#)order_list.c	1.7  01/13/97";

typedef struct _PPSOrderClientData
{
	Widget	masterList;
	Widget	attachDialog;
	Widget	attachList;
	String*	strings;
	int		numStrings;

} PPSOrderClientData, *PPSOrderClientDataP;

/*-----------------------------------------------------------*/
/* ATTENTION: to override the default resources              */
/*            prepand "*order_list_name" in front of these   */
/*-----------------------------------------------------------*/
static String resourceSpec[] = 
{
	/*----------------------------*/
	/* general                    */
	/*----------------------------*/
	"*paramList.dialogTitle:			Order Parameters List",
	"*list.visibleItemCount:			15",

	/*----------------------------*/
	/* for all text, int, float   */
	/*----------------------------*/
	"*text.columns:		20",

	0
};

static void 	ppsCreateAttachList(PPSOrderClientDataP orderClientData);
static void		ppsSetDefaultResources(Widget, char* name);

static void 	editValueCB(Widget, XtPointer, XtPointer);
static void 	deleteValueCB(Widget, XtPointer, XtPointer);
static void 	clearCB(Widget, XtPointer, XtPointer);
static void 	getStringCB(Widget, XtPointer, XtPointer);
static void 	popdownAttachCB(Widget, XtPointer, XtPointer);
static void 	destroyOrderListCB(Widget, XtPointer, XtPointer);

Widget
ppsCreateOrderList(
Widget					parent,				/* IN */
String					name,				/* IN */
String*					strings,			/* IN */
int						numStrings)			/* IN */
{
	Widget			w;
	Widget			list;
	Widget			addPB, deletePB, clearPB;
	XmString		xmString;
	int				doubleClickInterval;
	int				maxLabelLen=0;
	int				i=0;
	XmFontList		fontList;
	XmFontContext	fontContext;
	short			charWidth=0;
	short			charHeight=0;
	short			pbHeight=0;
	Dimension		listWidth, listHeight;
	int				visibleItemCount;
	Arg				args[10];
	int				n=0;
	PPSOrderClientDataP		orderClientData=0;;

	ppsSetDefaultResources(parent, name);

	/*----------------------------------------------------------------*/
	/* create a order widget client data                              */
	/*----------------------------------------------------------------*/
	orderClientData = (PPSOrderClientDataP)
						XtMalloc(sizeof(PPSOrderClientData));

	/*----------------------------------------------------------------*/
	/* create a form widget to hold the buttons and scrolled list     */
	/*----------------------------------------------------------------*/
	w = XtVaCreateManagedWidget(name, xmFormWidgetClass, parent,
						XmNshadowType, XmSHADOW_ETCHED_IN,
						XmNshadowThickness, 2,
						XmNhorizontalSpacing, 10,
						XmNverticalSpacing, 10,
						0);
	XtAddCallback(w, XmNdestroyCallback,
						destroyOrderListCB, orderClientData);

	/*----------------------------------------------*/
	/* create the "Add" push button                */
	/*----------------------------------------------*/
	xmString = CREATE_XMSTRING("Add...");
	addPB = XtVaCreateManagedWidget("addPB",
					xmPushButtonWidgetClass, w,
					XmNlabelString, xmString,
					XmNtopAttachment, XmATTACH_FORM,
					XmNrightAttachment, XmATTACH_FORM,
					0);
	XmStringFree(xmString);
	XtAddCallback(addPB, XmNactivateCallback, editValueCB, orderClientData);

	/*----------------------------------------------*/
	/* create the "Delete" push button              */
	/*----------------------------------------------*/
	xmString = CREATE_XMSTRING("Delete");
	deletePB = XtVaCreateManagedWidget("deletePB",
					xmPushButtonWidgetClass, w,
					XmNlabelString, xmString,
					XmNtopAttachment, XmATTACH_FORM,
					XmNleftAttachment, XmATTACH_FORM,
					0);
	XmStringFree(xmString);
	XtAddCallback(deletePB, XmNactivateCallback, deleteValueCB,
					orderClientData);

	/*----------------------------------------------*/
	/* create the "Clear" push button               */
	/*----------------------------------------------*/
	xmString = CREATE_XMSTRING("Clear");
	clearPB = XtVaCreateManagedWidget("clearPB",
					xmPushButtonWidgetClass, w,
					XmNlabelString, xmString,
					XmNtopAttachment, XmATTACH_FORM,
					XmNleftAttachment, XmATTACH_POSITION,
					XmNleftPosition, 40,
					0);
	XmStringFree(xmString);
	XtAddCallback(clearPB, XmNactivateCallback, clearCB,
					orderClientData);

	/*----------------------------------------------*/
	/* create a scrolled list                       */
	/*----------------------------------------------*/
	n=0;
	XtSetArg(args[n], XmNlistSizePolicy, XmCONSTANT); n++;
	XtSetArg(args[n], XmNscrollBarDisplayPolicy, XmSTATIC); n++;
	list = XmCreateScrolledList(w, "list", args, n);
	XtManageChild(list);

	orderClientData->masterList = list;
	orderClientData->attachDialog = 0;
	orderClientData->attachList = 0;
	orderClientData->strings = strings;
	orderClientData->numStrings = numStrings;
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

#if 0
	XtAddCallback(list, XmNdefaultActionCallback,
					editValueCB, 0);
#endif

	XtVaSetValues(XtParent(list),
					XmNtopAttachment, XmATTACH_WIDGET,
					XmNtopWidget, addPB,
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
	for(i=0; i < numStrings; i++)
	{
		int labelLen = strlen(strings[i]);
		if (labelLen > maxLabelLen)
			maxLabelLen = labelLen;
	}

	XtVaGetValues(deletePB,
					XmNheight, &pbHeight,
					0);
	listWidth = charWidth * maxLabelLen + 60;
	listHeight = charHeight * visibleItemCount + pbHeight + 40;
	XtVaSetValues(w,
					XmNwidth, listWidth,
#if 0
					XmNheight, listHeight,
#endif
					0);

	XtVaSetValues(w,
						XmNuserData, (XtPointer) orderClientData,
						0);
	return(w);

}/* ppsCreateOrderList */

static void
ppsSetDefaultResources(
Widget 		w,			/* IN */
char*		name)		/* IN */
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
editValueCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	PPSOrderClientDataP orderClientData = (PPSOrderClientDataP) clientData;

	/*----------------------------------------------*/
	/* if the param list has been created, manage   */
	/* it and bring to front, else create it.       */
	/*----------------------------------------------*/
	if (orderClientData->attachDialog == 0)
	{
		ppsCreateAttachList(orderClientData);
	}
	else
	{
		XtManageChild(orderClientData->attachDialog);
		XMapRaised(XtDisplay(XtParent(orderClientData->attachDialog)),
			XtWindow(XtParent(orderClientData->attachDialog)));
	}

}/* editValueCB */

static void
deleteValueCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	int*	posList;
	int		posCount;
	PPSOrderClientDataP orderClientData = (PPSOrderClientDataP) clientData;
	Widget masterList = orderClientData->masterList;

	if (XmListGetSelectedPos(masterList, &posList, &posCount))
	{
		XmListDeletePos(masterList, posList[0]);
		XtFree((char*) posList);
	}
	else
	{
		XppsCreateErrorDialog(orderClientData->masterList,
						"Please select an item to delete.",
						True, 0, 0);
	}

} /* deleteValueCB */

static void
clearCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	int*	posList;
	int		posCount;
	PPSOrderClientDataP orderClientData = (PPSOrderClientDataP) clientData;

	XmListDeleteAllItems(orderClientData->masterList);

} /* clearCB */

static void
ppsCreateAttachList(
PPSOrderClientDataP		orderClientData)	/* IN */
{
	int 		i=0;
	Widget		attachDialog;
	Widget		attachList;
	Widget		rc, addPB, cancelPB, separator;
	Widget		list;
	XmString	xmString;
	XmString*	listStrings;

	attachDialog = orderClientData->attachDialog =
			XmCreateFormDialog(orderClientData->masterList, "paramList", 0, 0);

	rc = XtVaCreateManagedWidget("rc",
				xmRowColumnWidgetClass, attachDialog,
				XmNorientation, XmVERTICAL,
				XmNpacking, XmPACK_COLUMN,
				XmNnumColumns, 2,
				XmNentryAlignment, XmALIGNMENT_CENTER,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment, XmATTACH_FORM,
				0);
	xmString = CREATE_XMSTRING("Add");
	addPB = XtVaCreateManagedWidget("addPB",
				xmPushButtonWidgetClass, rc,
				XmNlabelString, xmString,
				0);
	XmStringFree(xmString);
	XtAddCallback(addPB, XmNactivateCallback,
				getStringCB, orderClientData);

	xmString = CREATE_XMSTRING("Done");
	cancelPB = XtVaCreateManagedWidget("cancelPB",
				xmPushButtonWidgetClass, rc,
				XmNlabelString, xmString,
				0);
	XmStringFree(xmString);
	XtAddCallback(cancelPB, XmNactivateCallback,
				popdownAttachCB, orderClientData);

	XtVaSetValues(attachDialog,
				XmNhorizontalSpacing, 10,
				XmNverticalSpacing, 10,
				0);

	separator = XtVaCreateManagedWidget("separator",
				xmSeparatorWidgetClass, attachDialog,
				XmNbottomAttachment, XmATTACH_WIDGET,
				XmNbottomWidget, rc,
				XmNleftAttachment, XmATTACH_FORM,
				XmNleftOffset, 0,
				XmNrightAttachment, XmATTACH_FORM,
				XmNrightOffset, 0,
				0);

	list = orderClientData->attachList =
				XmCreateScrolledList(attachDialog, "list", 0, 0);
	XtVaSetValues(XtParent(list),
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_WIDGET,
				XmNbottomWidget, separator,
				XmNleftAttachment, XmATTACH_FORM,
				0);
	XtAddCallback(list, XmNdefaultActionCallback,
				getStringCB, orderClientData);

	/*--------------------------------------------------*/
	/* put the strings in the list                      */
	/*--------------------------------------------------*/
	if (orderClientData->numStrings > 0)
	{
		listStrings = (XmString*) XtMalloc(orderClientData->numStrings *
							sizeof(XmString));
		for (i=0; i < orderClientData->numStrings; i++)
		{
			listStrings[i] = CREATE_XMSTRING(orderClientData->strings[i]);
		}
		XmListAddItemsUnselected(list, listStrings,
							orderClientData->numStrings, 0);
		for (i=0; i < orderClientData->numStrings; i++)
		{
			XmStringFree(listStrings[i]);
		}
		XtFree((char*)listStrings);
	}
	XtManageChild(list);

	XtManageChild(attachDialog);

}/* ppsCreateAttachList */

/*-------------------------------------------------------*/
/* get the first selected string in the attach list      */
/* and add it to the end of the master list              */
/*-------------------------------------------------------*/
static void
getStringCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	PPSOrderClientDataP		orderClientData = (PPSOrderClientDataP) clientData;
    XmStringTable   xmStrings;
    int				numItems=0;

    XtVaGetValues (orderClientData->attachList,
                    XmNselectedItemCount, &numItems,
                    XmNselectedItems, &xmStrings,
                    0);
    if (numItems > 0)
    {
		char msg[PPS_STRING_LEN];
		char* text;
		if (XmListItemExists(orderClientData->masterList, xmStrings[0]))
		{
			(void)XmStringGetLtoR(xmStrings[0], XmSTRING_DEFAULT_CHARSET,
								&text);
			(void)sprintf(msg, "\"%s\" already exists.", text);
			XppsCreateErrorDialog(orderClientData->attachList,
							msg, True, 0, 0);
			XtFree(text);
		}
		else
		{
			XmListAddItemUnselected(orderClientData->masterList,
									xmStrings[0], 0);
		}
    }

}/* getStringCB */

static void
popdownAttachCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	PPSOrderClientDataP		orderClientData = (PPSOrderClientDataP) clientData;
	XtUnmanageChild(orderClientData->attachDialog);

} /* popdownAttachCB */

static void
destroyOrderListCB(
Widget			w,
XtPointer		clientData,
XtPointer		callData)
{
printf("destroying clientdata\n");
	XtFree((char*) clientData);

} /* destroyOrderListCB */

Boolean
ppsGetOrderListStrings(
Widget					w,			/* IN */
String**				strings,	/* OUT */
int*					numStrings) /* OUT */
{
	PPSOrderClientDataP	clientData=0;
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

} /* ppsGetOrderListStrings */

void
ppsFreeOrderListStrings(
String*				strings,	/* IN */
int					numStrings) /* IN */
{
	int			i;

	for (i=0; i < numStrings; i++)
		XtFree(strings[i]);
	XtFree((char*)strings);

} /* ppsFreeOrderListStrings */

Boolean
ppsSetOrderListStrings(
Widget					w,			/* IN */
String*					strings,	/* IN */
int						numStrings) /* IN */
{
	PPSOrderClientDataP	clientData=0;
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

} /* ppsSetOrderListStrings */
