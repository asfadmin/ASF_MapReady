/*-----------------------------------------*/
/* for vi: set ts=4 sw=4
/*-----------------------------------------*/

#include <string.h>
#include <varargs.h>
#include <stdio.h>

#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>

#include "column_list.h"

static char SCCSFileId[] = "@(#)column_list.c	1.6  01/11/97";

typedef struct _PPSColumnClientData
{
	int						numLists;
	Widget*					forms;
	Widget*					lists;
	Widget					verticalBar;
	PPSColumnListSpec*	    columnListSpecs;
	short					charHeight;

} PPSColumnClientData, *PPSColumnClientDataP;


static void destroyClientDataCB(Widget, XtPointer, XtPointer);
static void incrementCB(Widget, XtPointer, XtPointer);
static void decrementCB(Widget, XtPointer, XtPointer);
static void pageIncrementCB(Widget, XtPointer, XtPointer);
static void pageDecrementCB(Widget, XtPointer, XtPointer);
static void dragCB(Widget, XtPointer, XtPointer);
static void resizeCB(Widget, XtPointer, XtPointer);
static void singleClickCB(Widget, XtPointer, XtPointer);

Widget			drawArea=0;

/*-----------------------------------------------*/
/* constructor: create a column list             */
/*-----------------------------------------------*/
Widget
ppsCreateColumnList(
Widget				parent,
String				name,
PPSColumnListSpec	*columnListSpecs,
int					numColumnLists,
int					visibleItems)
{
	Widget			w;
	Widget			title;
	char			widgetName[PPS_STRING_LEN];
	XmString		xmString;
	int				doubleClickInterval;
	int				maxLabelLen=0;
	int				i=0;
	XmFontList		fontList;
	XmFontContext	fontContext;
	short			charWidth=0;
	short			charHeight=0;
	Dimension		listWidth, listHeight, lastListWidth;
	PPSColumnClientDataP columnClientData;

	/*----------------------------------------------------------------*/
	/* create a form widget to hold the all the titles and lists      */
	/*----------------------------------------------------------------*/
	w = XtVaCreateManagedWidget(name, xmFormWidgetClass, parent,
						XmNhorizontalSpacing, 10,
						XmNverticalSpacing, 10,
						0);


	columnClientData = (PPSColumnClientDataP)
							XtMalloc(sizeof(PPSColumnClientData));
	columnClientData->lists = (Widget*)
							XtMalloc(numColumnLists * sizeof(Widget));
	columnClientData->forms = (Widget*)
							XtMalloc(numColumnLists * sizeof(Widget));

	/*----------------------------------------------------*/
	/* create all column lists according to the table     */
	/*----------------------------------------------------*/
	for (i=0; i < numColumnLists; i++)
	{
		char label[PPS_STRING_LEN];

		/*----------------------------------------------*/
		/* create a form to hold each title and list    */
		/*----------------------------------------------*/
		(void)sprintf(widgetName, "%s%s",
						columnListSpecs[i].label, "Form");
		columnClientData->forms[i] = XtVaCreateWidget(widgetName,
						xmFormWidgetClass, w,
						XmNtopAttachment, XmATTACH_FORM,
						XmNbottomAttachment, XmATTACH_FORM,
						0);

		/*----------------------------------------------*/
		/* create a label widget                        */
		/*----------------------------------------------*/
		(void)strncpy(label, columnListSpecs[i].label,
						columnListSpecs[i].width);
		label[columnListSpecs[i].width] = '\0';
		xmString = CREATE_XMSTRING(label);
		title = XtVaCreateManagedWidget(columnListSpecs[i].label,
					xmLabelWidgetClass, columnClientData->forms[i],
					XmNlabelString, xmString,
					XmNalignment, XmALIGNMENT_CENTER,
					XmNtopAttachment, XmATTACH_FORM,
					XmNleftAttachment, XmATTACH_FORM,
					XmNleftOffset, 0,
					XmNrightAttachment, XmATTACH_FORM,
					XmNrightOffset, 0,
					0);
		XmStringFree(xmString);

		/*----------------------------------------------*/
		/* create a scrolled list                       */
		/*----------------------------------------------*/
		(void)sprintf(widgetName, "%s%s",
						columnListSpecs[i].label, "List");
		columnClientData->lists[i] = XtVaCreateManagedWidget(widgetName,
						xmListWidgetClass, columnClientData->forms[i],
						XmNlistSizePolicy, XmCONSTANT,
						XmNtopAttachment, XmATTACH_WIDGET,
						XmNtopWidget, title,
						XmNbottomAttachment, XmATTACH_FORM,
						XmNleftAttachment, XmATTACH_FORM,
						XmNrightAttachment, XmATTACH_FORM,
						0);
		XtVaGetValues(columnClientData->lists[i],
						XmNfontList, &fontList,
						XmNdoubleClickInterval, &doubleClickInterval,
						0);
		doubleClickInterval = (doubleClickInterval > MIN_DOUBLE_CLICK_INTERVAL ?
						doubleClickInterval : MIN_DOUBLE_CLICK_INTERVAL);

		XtVaSetValues(columnClientData->lists[i],
						XmNvisibleItemCount, visibleItems,
						XmNselectionPolicy, XmSINGLE_SELECT,
						XmNdoubleClickInterval, doubleClickInterval,
						0);

		if (i == 0)
		{
			XtVaSetValues(columnClientData->forms[i],
						XmNleftAttachment, XmATTACH_FORM,
						0);
		}
		else
		{
			XtVaSetValues(columnClientData->forms[i],
						XmNleftAttachment, XmATTACH_WIDGET,
						XmNleftWidget, columnClientData->forms[i-1],
						0);
		}

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

		listWidth = charWidth * columnListSpecs[i].width + 10;
		/* label's height, including margin height */
		listHeight = charHeight + 4;
		/* 3 pixels between each item, plus shadowThickness */
		listHeight += (charHeight + 3) * visibleItems + 3 + 4;
		XtVaSetValues(columnClientData->forms[i],
						XmNwidth, listWidth,
						0);
		XtManageChild(columnClientData->forms[i]);

	} /* for */

	/*---------------------------------------------------*/
	/* for some reason, creation of drawArea affects the */
	/* width of the last list's form's width             */
	/* get the last child's width, and set it later      */
	/*---------------------------------------------------*/
	XtVaGetValues(columnClientData->forms[numColumnLists-1],
					XmNwidth, &lastListWidth,
					0);

	drawArea = XtVaCreateManagedWidget(name, xmDrawingAreaWidgetClass,
					columnClientData->forms[numColumnLists-1],
					XmNmappedWhenManaged, False,
					XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
					XmNtopWidget, columnClientData->lists[0],
					XmNtopOffset, 0,
					XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
					XmNbottomWidget, columnClientData->lists[0],
					XmNbottomOffset, 0,
					XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
					XmNleftWidget, columnClientData->lists[0],
					XmNleftOffset, 0,
					XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
					XmNrightWidget, columnClientData->lists[0],
					XmNrightOffset, 0,
					0);

	XtVaSetValues(columnClientData->forms[numColumnLists-1],
					XmNwidth, lastListWidth,
					0);
	for (i=0; i < numColumnLists; i++)
	{
		XtAddCallback(columnClientData->lists[i], XmNsingleSelectionCallback,
						singleClickCB, (XtPointer) columnClientData);
	}

	columnClientData->charHeight = charHeight;

	columnClientData->numLists = numColumnLists;

	columnClientData->columnListSpecs = columnListSpecs;

	columnClientData->verticalBar = XtVaCreateManagedWidget("vertScrollB",
						xmScrollBarWidgetClass, w,
						XmNwidth, 20,
						XmNincrement, 1,
						XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
						XmNtopWidget, columnClientData->forms[numColumnLists-1],
						XmNtopOffset, charHeight + 4,
						XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
						XmNbottomWidget, columnClientData->forms[numColumnLists-1],
						XmNbottomOffset, 0,
						XmNleftAttachment, XmATTACH_WIDGET,
						XmNleftWidget, columnClientData->forms[numColumnLists-1],
						0);

	XtAddCallback(columnClientData->verticalBar, XmNincrementCallback,
						incrementCB, (XtPointer) columnClientData);
	XtAddCallback(columnClientData->verticalBar, XmNdecrementCallback,
						decrementCB, (XtPointer) columnClientData);
	XtAddCallback(columnClientData->verticalBar, XmNpageIncrementCallback,
						pageIncrementCB, (XtPointer) columnClientData);
	XtAddCallback(columnClientData->verticalBar, XmNpageDecrementCallback,
						pageDecrementCB, (XtPointer) columnClientData);
	XtAddCallback(columnClientData->verticalBar, XmNdragCallback,
						dragCB, (XtPointer) columnClientData);

	XtAddCallback(w, XmNdestroyCallback,
						destroyClientDataCB, (XtPointer) columnClientData);

	XtAddCallback(drawArea, XmNresizeCallback,
						resizeCB, (XtPointer) columnClientData);

	XtVaSetValues(w,
						XmNuserData, (XtPointer) columnClientData,
						0);
	return(w);

}/* ppsCreateColumnList */

/*----------------------------------------------------*/
/* replace all the items in the column list           */
/*                                                    */
/* varargs:                                           */
/*         PPSColumnListSpec *columnListSpecs;        */
/*                    // array of column list structs */
/*         int    rows; // number of rows             */
/*         int    columns; // number of columns       */
/*         void*  array_of_1st_list;                  */
/*         void*  array_of_2nd_list;                  */
/*         ........................                   */
/*         void*  array_of_last_list;                 */
/*----------------------------------------------------*/
void
ppsColumnListReplaceAllItems(va_alist)
va_dcl
{
	va_list		varList;
	Widget		columnListWidget;
	PPSColumnListSpec *columnListSpecs;
	PPSColumnClientDataP columnClientData;
	int			rows;
	int			columns;
	int			i, j;
	XmString*	xmStrings;
	int			visibleItemCount;

	va_start(varList);

	/* get the column list widget */
	columnListWidget = (Widget) va_arg(varList, int);
	XtVaGetValues(columnListWidget,
					XmNuserData, &columnClientData,
					0);
	columnListSpecs = columnClientData->columnListSpecs;

	rows = va_arg(varList, int);
	columns = va_arg(varList, int);

	xmStrings = (XmString*) XtMalloc(columns * sizeof(XmString));

	for (i=0; i < rows; i++)
	{
		/*---------------------------------*/
		/* print the items out as strings, */
		/* and convert them to XmStrings   */
		/*---------------------------------*/
		switch (columnListSpecs[i].dataType)
		{
			case PPS_TEXT_COLUMN_LIST:
			{
				char** array;
				char string[PPS_BIG_SIZE];
				char format[PPS_STRING_LEN];
				array = va_arg(varList, char**);
				sprintf(format, "%%%ds", columnListSpecs[i].width);
				for (j=0; j < columns; j++)
				{
					sprintf(string, format, array[j]);
					xmStrings[j] = CREATE_XMSTRING(string);
				}
				break;
			}
			case PPS_INT_COLUMN_LIST:
			{
				int* array;
				char string[PPS_BIG_SIZE];
				char format[PPS_STRING_LEN];
				sprintf(format, "%%%dd", columnListSpecs[i].width);
				array = va_arg(varList, int*);
				for (j=0; j < columns; j++)
				{
					sprintf(string, format, array[j]);
					xmStrings[j] = CREATE_XMSTRING(string);
				}
				break;
			}
			case PPS_FLOAT_COLUMN_LIST:
			{
				float* array;
				char string[PPS_BIG_SIZE];
				char format[PPS_STRING_LEN];
				sprintf(format, "%%%d.2f", columnListSpecs[i].width);
				array = va_arg(varList, float*);
				for (j=0; j < columns; j++)
				{
					sprintf(string, format, array[j]);
					xmStrings[j] = CREATE_XMSTRING(string);
				}
				break;
			}
		}/*switch*/

		/*----------------------------------------------------*/
		/* clear the list, then add the new items in the list */
		/*----------------------------------------------------*/
		XmListDeleteAllItems(columnClientData->lists[i]);
		XmListAddItemsUnselected(columnClientData->lists[i],
										xmStrings, columns, 0);

		/* free up XmString */
		for (j=0; j < columns; j++)
		{
			XmStringFree(xmStrings[j]);
		}
	}

	va_end(varList);

	XtFree((char*) xmStrings);

	XtVaGetValues(columnClientData->lists[0],
				XmNvisibleItemCount, &visibleItemCount,
				0);

	XtVaSetValues(columnClientData->verticalBar,
				XmNvalue, 1,
				XmNsliderSize, visibleItemCount-1,
				XmNpageIncrement, visibleItemCount-1,
				XmNminimum, 1,
				XmNmaximum, columns,
				0);

	return;

} /* ppsColumnListReplaceAllItems */

/*----------------------------------------------------*/
/* add the items at the end of the column list        */
/*                                                    */
/* varargs:                                           */
/*         PPSColumnListSpec *columnListSpecs;        */
/*                    // array of column list structs */
/*         int    rows; // number of rows             */
/*         int    columns; // number of columns       */
/*         void*  array_of_1st_list;                  */
/*         void*  array_of_2nd_list;                  */
/*         ........................                   */
/*         void*  array_of_last_list;                 */
/*----------------------------------------------------*/
void
ppsColumnListAddItems(
Widget		columnListWidget,
int			rows,
int			columns,
void**		valueArrays,
int*		textSizes)
{
	PPSColumnListSpec *columnListSpecs;
	PPSColumnClientDataP columnClientData;
	int			i, j;
	XmString*	xmStrings;
	int			visibleItemCount;
	int			itemCount=0;

	XtVaGetValues(columnListWidget,
					XmNuserData, &columnClientData,
					0);
	columnListSpecs = columnClientData->columnListSpecs;

	xmStrings = (XmString*) XtMalloc(rows * sizeof(XmString));

	for (i=0; i < columns; i++)
	{
		/*---------------------------------*/
		/* print the items out as strings, */
		/* and convert them to XmStrings   */
		/*---------------------------------*/
		switch (columnListSpecs[i].dataType)
		{
			case PPS_TEXT_COLUMN_LIST:
			{
				char* array;
				char string[PPS_BIG_SIZE];
				char format[PPS_STRING_LEN];
				array = (char*)valueArrays[i];
				sprintf(format, "%%%ds", columnListSpecs[i].width);
				for (j=0; j < rows; j++)
				{
					sprintf(string, format, array);
					xmStrings[j] = CREATE_XMSTRING(string);
					array += textSizes[i];
				}
				break;
			}
			case PPS_INT_COLUMN_LIST:
			{
				int* array;
				char string[PPS_BIG_SIZE];
				char format[PPS_STRING_LEN];
				sprintf(format, "%%%dd", columnListSpecs[i].width);
				array = (int*) valueArrays[i];
				for (j=0; j < rows; j++)
				{
					sprintf(string, format, array[j]);
					xmStrings[j] = CREATE_XMSTRING(string);
				}
				break;
			}
			case PPS_FLOAT_COLUMN_LIST:
			{
				float* array;
				char string[PPS_BIG_SIZE];
				char format[PPS_STRING_LEN];
				sprintf(format, "%%%d.2f", columnListSpecs[i].width);
				array = (float*) valueArrays[i];
				for (j=0; j < rows; j++)
				{
					sprintf(string, format, array[j]);
					xmStrings[j] = CREATE_XMSTRING(string);
				}
				break;
			}
		}/*switch*/

		/*----------------------------------------------------*/
		/* add the new items in the list                      */
		/*----------------------------------------------------*/
		XmListAddItemsUnselected(columnClientData->lists[i],
										xmStrings, rows, 0);

		/* free up XmString */
		for (j=0; j < rows; j++)
		{
			XmStringFree(xmStrings[j]);
		}
	}

	XtFree((char*) xmStrings);

	XtVaGetValues(columnClientData->lists[0],
				XmNvisibleItemCount, &visibleItemCount,
				XmNitemCount, &itemCount,
				0);

	XtVaSetValues(columnClientData->verticalBar,
				XmNminimum, 1,
				XmNmaximum, itemCount,
				XmNvalue, 1,
				XmNsliderSize, visibleItemCount-1,
				XmNpageIncrement, visibleItemCount-1,
				0);

	return;

} /* ppsColumnListAddItems */


/*----------------------------------------------*/
/* free up what column list widget allocates    */
/*----------------------------------------------*/
static void
destroyClientDataCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	PPSColumnClientDataP columnClientData = (PPSColumnClientDataP) clientData;
	XtFree((char*) columnClientData->forms);
	XtFree((char*) columnClientData->lists);
	XtFree((char*) columnClientData);

} /* destroyClientDataCB */

static void
increment(
PPSColumnClientDataP	columnListClientData,
int						numSteps)
{
	int		totalItemCount;
	int		topItemPos;
	int		visibleItemCount;
	int		i;

	XtVaGetValues(columnListClientData->lists[0],
		XmNitemCount, &totalItemCount,
		XmNtopItemPosition, &topItemPos,
		XmNvisibleItemCount, &visibleItemCount,
		0);

	/* if increment is ok (before the bottom), do it */
	if ((topItemPos + numSteps + visibleItemCount - 1) <= totalItemCount)
	{
		for (i=0; i < columnListClientData->numLists; i++)
		{
			XmListSetPos(columnListClientData->lists[i],
								topItemPos + numSteps);
		}
	}
	else
	{
		for (i=0; i < columnListClientData->numLists; i++)
		{
			XmListSetBottomPos(columnListClientData->lists[i],
								totalItemCount);
		}
	}
} /* increment */

static void
incrementCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{

	increment((PPSColumnClientDataP) clientData, 1);

} /* incrementCB */

static void
decrement(
PPSColumnClientDataP	columnListClientData,
int						numSteps)
{
	int		topItemPos;
	int		i;

	XtVaGetValues(columnListClientData->lists[0],
		XmNtopItemPosition, &topItemPos,
		0);

	/* if top is already reached, then do nothing */
	if ((topItemPos - numSteps) >= 1)
	{
		for (i=0; i < columnListClientData->numLists; i++)
		{
			XmListSetPos(columnListClientData->lists[i],
								topItemPos - numSteps);
		}
	}
	else
	{
		for (i=0; i < columnListClientData->numLists; i++)
		{
			XmListSetPos(columnListClientData->lists[i],
								1);
		}
	}
} /* decrement */

static void
decrementCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{

	decrement((PPSColumnClientDataP) clientData, 1);

} /* decrementCB */

static void
pageIncrementCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	int	visibleItemCount;
	
	PPSColumnClientDataP columnListClientData = 
						(PPSColumnClientDataP) clientData;
	XtVaGetValues(columnListClientData->lists[0],
		XmNvisibleItemCount, &visibleItemCount,
		0);

	increment((PPSColumnClientDataP) clientData, visibleItemCount-1);

} /* pageIncrementCB */

static void
pageDecrementCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	int	visibleItemCount;

	PPSColumnClientDataP columnListClientData = 
						(PPSColumnClientDataP) clientData;
	XtVaGetValues(columnListClientData->lists[0],
		XmNvisibleItemCount, &visibleItemCount,
		0);

	decrement((PPSColumnClientDataP) clientData, visibleItemCount-1);

} /* pageDecrementCB */

static void
dragCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	int		i=0;
	int		value=0;

	PPSColumnClientDataP columnListClientData = 
						(PPSColumnClientDataP) clientData;

	XtVaGetValues(columnListClientData->verticalBar,
						XmNvalue, &value,
						0);

	for (i=0; i < columnListClientData->numLists; i++)
	{
		XmListSetPos(columnListClientData->lists[i], value);
	}

} /* dragCB */

static void
resizeCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	Dimension width=0, height=0;
	Dimension highlightThickness=0, shadowThickness=0;
	int visibleItemCount;
	int totalItemCount;
	PPSColumnClientDataP columnListClientData = 
						(PPSColumnClientDataP) clientData;
	int		value=0, max=0;
	int		sliderSize=0;

	/* get the width and height of the drawing area widget */
	XtVaGetValues(w,
					XmNwidth, &width,
					XmNheight, &height,
					0);

	XtVaGetValues(columnListClientData->lists[0],
		XmNhighlightThickness, &highlightThickness,
		XmNshadowThickness, &shadowThickness,
		XmNitemCount, &totalItemCount,
		0);

	if (totalItemCount <= 0)
		return;

	/* subtract list's shadow and one extra spacing */
	height -= 2 * shadowThickness + 3;

	/* each item = char height + spacing (highlightThcikness + 1) */
	visibleItemCount = (float)height /
			((float)columnListClientData->charHeight + 3);

	/*-------------------------------------------------------------*/
	/* adjust the value if the visible item > total item           */
	/* else Xt will complain that                                  */
	/* "The specified scrollbar value is greater than the maximum  */
	/*  scrollbar value minus the scrollbar slider size.           */
	/*-------------------------------------------------------------*/
	XtVaGetValues(columnListClientData->verticalBar,
					XmNvalue, &value,
					XmNmaximum, &max,
					0);

	visibleItemCount = visibleItemCount > totalItemCount ?
					totalItemCount : visibleItemCount;
	sliderSize = visibleItemCount - 1;
	if (sliderSize <= 0)
		sliderSize = 1;
	if (value > max - sliderSize)
	{
		XtVaSetValues(columnListClientData->verticalBar,
						XmNvalue, 1,
						0);
	}

	/* now, set the new slider size and page increment */
	XtVaSetValues(columnListClientData->verticalBar,
					XmNsliderSize, sliderSize,
					XmNpageIncrement, sliderSize,
					0);

} /* resizeCB */

static void
singleClickCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	PPSColumnClientDataP columnListClientData = 
						(PPSColumnClientDataP) clientData;
	int 	i=0, itemPos=0;

	itemPos = ((XmListCallbackStruct*) callData)->item_position;

	for (i=0; i < columnListClientData->numLists; i++)
	{
		XmListSelectPos(columnListClientData->lists[i],
							itemPos, False);
	}

} /* singleClickCB */

short
ppsColumnListRecalcWidth(
Widget		columnList)
{
	PPSColumnClientDataP	columnClientData;
	Dimension				newWidth=0, childWidth=0;
	Dimension				verticalSpacing=0;
	int						i;

	/* get column client data */
	XtVaGetValues(columnList,
						XmNuserData, (XtPointer) &columnClientData,
						XmNverticalSpacing, &verticalSpacing,
						0);

	/* get the total width of all lists */
	for (i=0; i < columnClientData->numLists; i++)
	{
		XtVaGetValues(columnClientData->forms[i],
						XmNwidth, &childWidth,
						0);
		newWidth += childWidth;
	}

	/* add the width of the vertical scrollbar */
	XtVaGetValues(columnClientData->verticalBar,
						XmNwidth, &childWidth,
						0);
	newWidth += childWidth;

	/* add the vertical spacing of all children (all lists */
	/* plus 1 for the scrollbar and 1 for the end          */
	newWidth += verticalSpacing * (columnClientData->numLists + 2);

	return newWidth;

} /* ppsColumnListRecalcWidth */

int
ppsColumnListGetItemCount(
Widget		columnList)
{
	PPSColumnClientDataP	columnClientData;
	int						itemCount;

	/* get column client data */
	XtVaGetValues(columnList,
						XmNuserData, (XtPointer) &columnClientData,
						0);

	XtVaGetValues(columnClientData->lists[0],
						XmNitemCount, &itemCount,
						0);

	return (itemCount);

} /* ppsColumnListGetItemCount */

void
ppsColumnListDeleteAllItems(
Widget		columnList)
{
	PPSColumnClientDataP	columnClientData;
	int						i;

	/* get column client data */
	XtVaGetValues(columnList,
						XmNuserData, (XtPointer) &columnClientData,
						0);

	for (i=0; i < columnClientData->numLists; i++)
		XmListDeleteAllItems(columnClientData->lists[i]);

} /* ppsColumnListDeleteAllItems */
