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

#include "column_list.h"
#include "db_util.h"
#include "dynQuery_util.h"
#include "order_list.h"
#include "smart_list.h"

static const char rcsid[] = "@(#)dynQuery_queryW.c	1.2  03/05/97";

#define		DEFAULT_INT_WIDTH		10
#define		DEFAULT_FLOAT_WIDTH		12
#define		DEFAULT_TEXT_WIDTH		12

#define		INITIAL_NUM_LISTS		5
#define		MAIN_FORM_SPACING		5
#define		QUERY_FORM_SPACING		10
#define		ROWS_PER_FETCH			100

#define		MAX_NUM_MENU_BUTTONS	10

static enum
{
	LOGIN_PB,
	DATABASE_PB,
	QUIT_PB,
	NUM_FILE_PBS
};

Dimension		minWindowWidth=0;

Widget			panedW=0;
Widget			lowerForm=0;		/* main form */
Widget			columnList=0;	/* column list */
Widget			selectColumnDialog=0;
Widget			smartList=0;
Widget			sortList=0;
Widget			configPB=0;
Widget			itemsText=0;

static Widget	createMenuBar(Widget parent);
static void		createSelectColumnDialog(Widget parent);

static void		configColumnListCB(Widget, XtPointer, XtPointer);
static void		getColumnNamesCB(Widget, XtPointer, XtPointer);
static void		queryCB(Widget, XtPointer, XtPointer);
static void		loginCB(Widget, XtPointer, XtPointer);
static void		changeDatabaseCB(Widget, XtPointer, XtPointer);
static void		quitCB(Widget, XtPointer, XtPointer);

static void		createCurrentColumnListStruct(String* strings, int numStrings);
static void		exec_query(char* sqlBuf);
static int		getColumnOptions(
						CS_CONNECTION		**connection,
						char*				tableName,
						char*				columnName,
						PPS_QUERY_RESULT	**columnOptionList, /* OUT */
						int					*numOptions);	    /* OUT */

PPSColumnListSpec *_ppsTotalColumnListWidgets=0;
int					_tableColumnCount=0;

PPSColumnListSpec *_ppsCurrentColumnListWidgets=0;
int					_currentColumnCount=0;

PPSSmartWidgetSpec*	_ppsFilterListSpec=0;

String			*_currentSelectStrings=0;
int				_numCurrentSelectStrings=0;

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

/*---------------------------------------------*/
/* external variables                          */
/*---------------------------------------------*/
extern Widget			topLevel;
extern Widget			menuBar;
extern Widget			changeDatabaseDlg;
extern Widget			queryDBAcctDlg;

extern CS_CONNECTION	*connection;		/* sybase connection */
extern char				userName[];
extern char				passwd[];
extern char				serverName[];
extern char				databaseName[];
extern char				databaseTableName[];


void
createQueryWindow(
Widget		parent)
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
	Dimension			width=0, newWidth=0;

    String* strings;

	/*-------------------------------------------------------*/
	/* get column field names from database                  */
	/*-------------------------------------------------------*/
	(void) strcpy(loginStruct.username, userName);
	(void) strcpy(loginStruct.password, passwd);
	(void) strcpy(loginStruct.servername, serverName);
	if (db_connect (&loginStruct, &connection) == CS_FAIL)
	{
		fprintf(stderr, "createQueryWindow: connect to Sybase failed\n");
		exit(1);
	}

	if (getTableColumnDesc (&connection, databaseTableName,
					&columnDescList, &_tableColumnCount) == CS_FAIL)
	{
		fprintf(stderr, "createQueryWindow: cannot get database Table columns\n");
		exit(2);
	}

	/* allocate space for column list specs */
	_ppsTotalColumnListWidgets = (PPSColumnListSpec*)
					XtMalloc(sizeof(PPSColumnListSpec) * _tableColumnCount);

	/* allocate space for filter list specs */
	_ppsFilterListSpec = (PPSSmartWidgetSpec*)
					XtMalloc(sizeof(PPSSmartWidgetSpec) * _tableColumnCount);

	for (i=0; i < _tableColumnCount; i++)
	{
		PPS_QUERY_RESULT	*columnOptions=0;
		PPSColumnListSpec *nextColumnSpec =
								&(_ppsTotalColumnListWidgets[columnListSize]);
		PPSSmartWidgetSpec *nextFilterSpec =
								&(_ppsFilterListSpec[columnListSize]);

		switch(columnDescList[i].colType)
		{
			case CS_INT_TYPE:
				(void)strcpy(nextColumnSpec->label, columnDescList[i].colName);
				nextColumnSpec->dataType = PPS_INT_COLUMN_LIST;
				nextColumnSpec->width = DEFAULT_INT_WIDTH;

				(void)strcpy(nextFilterSpec->widgetName,
										columnDescList[i].colName);
				(void)strcpy(nextFilterSpec->label,
										columnDescList[i].colName);
				nextFilterSpec->widgetType = PPS_SMART_INT;

				columnListSize++;

				break;

			case CS_FLOAT_TYPE:
			case CS_REAL_TYPE:
				(void)strcpy(nextColumnSpec->label, columnDescList[i].colName);
				nextColumnSpec->dataType = PPS_FLOAT_COLUMN_LIST;
				nextColumnSpec->width = DEFAULT_FLOAT_WIDTH;

				(void)strcpy(nextFilterSpec->widgetName,
										columnDescList[i].colName);
				(void)strcpy(nextFilterSpec->label,
										columnDescList[i].colName);
				nextFilterSpec->widgetType = PPS_SMART_FLOAT;

				columnListSize++;

				break;

			case CS_CHAR_TYPE:
				(void)strcpy(nextColumnSpec->label, columnDescList[i].colName);
				nextColumnSpec->dataType = PPS_TEXT_COLUMN_LIST;
				nextColumnSpec->width = DEFAULT_TEXT_WIDTH;

				(void)strcpy(nextFilterSpec->widgetName,
										columnDescList[i].colName);
				(void)strcpy(nextFilterSpec->label,
										columnDescList[i].colName);

				if (columnDescList[i].colRuleId != 0)
				{
						PPSSmartOptionAttr* attributes;
						attributes = (PPSSmartOptionAttr*)
										XtMalloc(sizeof(PPSSmartOptionAttr));
						nextFilterSpec->widgetType = PPS_SMART_OPTION_MENU;
						getColumnOptions(&connection,
										databaseTableName,
										columnDescList[i].colName,
										&columnOptions,
										&(attributes->numOptions));
						attributes->unitLen =
								columnOptions->columns[0].datafmt.maxlength;
						attributes->options = (char*)
								columnOptions->columns[0].colValue;
						nextFilterSpec->attributes = attributes;
				}
				else
						nextFilterSpec->widgetType = PPS_SMART_TEXT;

				columnListSize++;

				break;
			default:
				fprintf(stderr,
							"createQueryWindow: cannot handle %s, type = %d\n",
							columnDescList[i].colName,
							columnDescList[i].colType);
				break;
		
		} /* switch */
	} /* for */

	_tableColumnCount = columnListSize;

	freeTableColumnDesc(columnDescList);

	if (panedW)
		XtDestroyWidget(panedW);

	panedW = XtVaCreateManagedWidget("panedW",
						xmPanedWindowWidgetClass, parent,
						XmNallowResize, True,
						XmNtopAttachment, XmATTACH_WIDGET,
						XmNtopWidget, menuBar,
						XmNbottomAttachment, XmATTACH_FORM,
						XmNleftAttachment, XmATTACH_FORM,
						XmNrightAttachment, XmATTACH_FORM,
						0);

	/*----------------------------------------------*/
	/* create a upper form to hold filters and order*/
	/*----------------------------------------------*/
	upperForm = XtVaCreateManagedWidget("upperForm",
						xmFormWidgetClass, panedW,
						XmNhorizontalSpacing, QUERY_FORM_SPACING,
						XmNverticalSpacing, QUERY_FORM_SPACING,
						0);

	/*----------------------------------------------*/
	/* create the "Query Settings" label            */
	/*----------------------------------------------*/
	xmString = CREATE_XMSTRING("Query Settings:");
	settingLabel = XtVaCreateManagedWidget("settingLabel",
						xmLabelWidgetClass, upperForm,
						XmNlabelString, xmString,
						XmNtopAttachment, XmATTACH_FORM,
						XmNleftAttachment, XmATTACH_FORM,
						0);
	XmStringFree(xmString);

	/*----------------------------------------------*/
	/* create the filter smart list object          */
	/*----------------------------------------------*/
	smartList = ppsCreateSmartList(upperForm,
						"smartList",
						_ppsFilterListSpec,
						_tableColumnCount);
	XtVaSetValues(smartList,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, settingLabel,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		0);


    strings = (char**) XtMalloc(_tableColumnCount * sizeof(String));
    for (i=0; i < _tableColumnCount; i++)
    {
        strings[i] = (String) XtMalloc(PPS_STRING_LEN * sizeof(char));
		(void) strcpy(strings[i], _ppsTotalColumnListWidgets[i].label);
    }

	/*----------------------------------------------*/
	/* create the "Order By" label                  */
	/*----------------------------------------------*/
	xmString = CREATE_XMSTRING("Order By:");
	orderLabel = XtVaCreateManagedWidget("orderLabel",
						xmLabelWidgetClass, upperForm,
						XmNlabelString, xmString,
						XmNtopAttachment, XmATTACH_FORM,
						XmNleftAttachment, XmATTACH_WIDGET,
						XmNleftWidget, smartList,
						XmNleftOffset, 40,
						0);
	XmStringFree(xmString);

	/*----------------------------------------------*/
	/* create the sorting order list object         */
	/*----------------------------------------------*/
	sortList = ppsCreateOrderList(upperForm,
						"sortList",
						strings, _tableColumnCount);

	XtVaSetValues(sortList,
						XmNtopAttachment, XmATTACH_WIDGET,
						XmNtopWidget, orderLabel,
						XmNbottomAttachment, XmATTACH_FORM,
						XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
						XmNleftWidget, orderLabel,
						XmNleftOffset, 0,
						0);

	XtVaGetValues(smartList,
						XmNwidth, &width,
						0);
	minWindowWidth = width + 40;
	XtVaGetValues(sortList,
						XmNwidth, &width,
						0);
	minWindowWidth += width + 3 * QUERY_FORM_SPACING;

	/*----------------------------------------------*/
	/* create a lower form to hold display list     */
	/*----------------------------------------------*/
	lowerForm = XtVaCreateManagedWidget("lowerForm",
						xmFormWidgetClass, panedW,
						XmNhorizontalSpacing, QUERY_FORM_SPACING,
						XmNverticalSpacing, QUERY_FORM_SPACING,
						0);

	/*----------------------------------------------*/
	/* create the "Configure..." push button, etc...*/
	/*----------------------------------------------*/
	xmString = CREATE_XMSTRING("Results:");
	resultLabel = XtVaCreateManagedWidget("resultLabel",
						xmLabelWidgetClass, lowerForm,
						XmNlabelString, xmString,
						XmNtopAttachment, XmATTACH_FORM,
						XmNleftAttachment, XmATTACH_FORM,
						0);
	XmStringFree(xmString);

	itemsText = XtVaCreateManagedWidget("resultLabel",
						xmTextFieldWidgetClass, lowerForm,
						XmNcolumns, 10,
						XmNtopAttachment, XmATTACH_FORM,
						XmNleftAttachment, XmATTACH_WIDGET,
						XmNleftWidget, resultLabel,
						0);

	xmString = CREATE_XMSTRING("Items");
	itemsLabel = XtVaCreateManagedWidget("itemsLabel",
						xmLabelWidgetClass, lowerForm,
						XmNlabelString, xmString,
						XmNtopAttachment, XmATTACH_FORM,
						XmNleftAttachment, XmATTACH_WIDGET,
						XmNleftWidget, itemsText,
						0);
	XmStringFree(xmString);

	xmString = CREATE_XMSTRING("Configure Columns...");
	configPB = XtVaCreateManagedWidget("configPB",
						xmPushButtonWidgetClass, lowerForm,
						XmNlabelString, xmString,
						XmNtopAttachment, XmATTACH_FORM,
						XmNrightAttachment, XmATTACH_FORM,
						0);
	XmStringFree(xmString);
	XtAddCallback(configPB, XmNactivateCallback,
						configColumnListCB, 0);

	xmString = CREATE_XMSTRING("Query");
	queryPB = XtVaCreateManagedWidget("queryPB",
						xmPushButtonWidgetClass, lowerForm,
						XmNlabelString, xmString,
						XmNtopAttachment, XmATTACH_FORM,
						XmNrightAttachment, XmATTACH_WIDGET,
						XmNrightWidget, configPB,
						XmNrightOffset, 20,
						0);
	XmStringFree(xmString);
	XtAddCallback(queryPB, XmNactivateCallback,
						queryCB, 0);

	_ppsCurrentColumnListWidgets = (PPSColumnListSpec*)
					XtMalloc(sizeof(PPSColumnListSpec) * INITIAL_NUM_LISTS);
	(void)memcpy(_ppsCurrentColumnListWidgets,
						_ppsTotalColumnListWidgets,
						sizeof(PPSColumnListSpec) * INITIAL_NUM_LISTS);
	_currentColumnCount = INITIAL_NUM_LISTS;

	columnList = ppsCreateColumnList(lowerForm, "columnList",
						_ppsCurrentColumnListWidgets,
						_currentColumnCount, 7);
	XtVaSetValues(columnList,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, configPB,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		0);

	_currentSelectStrings = (String*) XtMalloc(INITIAL_NUM_LISTS *
							sizeof(String));
	for (i=0; i < INITIAL_NUM_LISTS; i++)
	{
		_currentSelectStrings[i] = XtMalloc(PPS_STRING_LEN * sizeof(char));
		(void)strcpy(_currentSelectStrings[i], 
							_ppsTotalColumnListWidgets[i].label);
	}
	_numCurrentSelectStrings = INITIAL_NUM_LISTS;

	/*---------------------------------------------*/
	/* get the new width of the column list object */
	/* and change the width of the toplevel shell  */
	/*---------------------------------------------*/
	newWidth = ppsColumnListRecalcWidth(columnList) + 20;
	if (newWidth < minWindowWidth)
		newWidth = minWindowWidth;

	XtVaSetValues(topLevel,
					XmNwidth, newWidth,
					0);
	return;

} /* createQueryWindow */

static void
configColumnListCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	if (selectColumnDialog)
	{
		XtManageChild(selectColumnDialog);
		XMapRaised(XtDisplay(selectColumnDialog),
			XtWindow(XtParent(selectColumnDialog)));
	}
	else
		createSelectColumnDialog(lowerForm);

} /* configColumnListCB */

static void
createSelectColumnDialog(
Widget		parent)
{
	Widget				orderList=0;
	XmString			xmString;
	Widget				okPB;
	char				**initStrings;

    String* strings;
	int i;

    strings = (char**) XtMalloc(_tableColumnCount * sizeof(String));
    for (i=0; i < _tableColumnCount; i++)
    {
        strings[i] = (String) XtMalloc(PPS_STRING_LEN * sizeof(char));
		(void) strcpy(strings[i], _ppsTotalColumnListWidgets[i].label);
    }


	selectColumnDialog = XmCreateFormDialog(parent,
						"selectColumnDialog",
						0, 0);
	XtVaSetValues(selectColumnDialog,
						XmNautoUnmanage, False,
						XmNhorizontalSpacing, QUERY_FORM_SPACING,
						XmNverticalSpacing, QUERY_FORM_SPACING,
						0);

	/*----------------------------------------------*/
	/* create the "OK" push button                  */
	/*----------------------------------------------*/
	xmString = CREATE_XMSTRING("OK");
	okPB = XtVaCreateManagedWidget("okPB",
						xmPushButtonWidgetClass, selectColumnDialog,
						XmNlabelString, xmString,
						XmNbottomAttachment, XmATTACH_FORM,
						XmNleftAttachment, XmATTACH_POSITION,
						XmNleftPosition, 40,
						0);
	XmStringFree(xmString);

	/*----------------------------------------------*/
	/* create the order list object                 */
	/*----------------------------------------------*/
	orderList = ppsCreateOrderList(selectColumnDialog,
						"orderList",
						strings, _tableColumnCount);

	XtVaSetValues(orderList,
						XmNtopAttachment, XmATTACH_FORM,
						XmNbottomAttachment, XmATTACH_WIDGET,
						XmNbottomWidget, okPB,
						XmNleftAttachment, XmATTACH_FORM,
						XmNrightAttachment, XmATTACH_FORM,
						0);

	/* need orderListClientData for callback */
	XtAddCallback(okPB, XmNactivateCallback,
						getColumnNamesCB, (XtPointer)orderList);

	initStrings = (char**) XtMalloc(INITIAL_NUM_LISTS * sizeof(char*));
	for (i=0; i < INITIAL_NUM_LISTS; i++)
	{
		initStrings[i] = XtMalloc(PPS_STRING_LEN * sizeof(char));
		(void)strcpy(initStrings[i], _ppsTotalColumnListWidgets[i].label);
	}
	ppsSetOrderListStrings(orderList,
						initStrings, INITIAL_NUM_LISTS);

	for (i=0; i < INITIAL_NUM_LISTS; i++)
	{
		XtFree(initStrings[i]);
	}
	XtFree((char*) initStrings);

	XtManageChild(selectColumnDialog);

} /* createSelectColumnDialog */

static void
getColumnNamesCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	Widget				orderList = (Widget) clientData;
	Dimension 			newWidth=0;

	if (_currentSelectStrings != 0)
	{
		ppsFreeOrderListStrings( _currentSelectStrings,
							_numCurrentSelectStrings);
	}

	if (ppsGetOrderListStrings(orderList,
							&_currentSelectStrings,
							&_numCurrentSelectStrings))
	{
		createCurrentColumnListStruct(_currentSelectStrings,
							_numCurrentSelectStrings);

	}
	else
	{
		XppsCreateErrorDialog(XtParent(w),
						"You must select at lease one parameter.",
						True, 0, 0);
		return;
	}


	/*----------------------------------------------------*/
	/* destroy the old column list, then create a new one */
	/*----------------------------------------------------*/
	XtDestroyWidget(columnList);
	columnList = ppsCreateColumnList(lowerForm, "columnList",
						_ppsCurrentColumnListWidgets,
						_currentColumnCount, 7);
	XtVaSetValues(columnList,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, configPB,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		0);

	/*---------------------------------------------*/
	/* get the new width of the column list object */
	/* and change the width of the toplevel shell  */
	/*---------------------------------------------*/
	newWidth = ppsColumnListRecalcWidth(columnList) + 20;
	if (newWidth < minWindowWidth)
		newWidth = minWindowWidth;
	XtVaSetValues(topLevel,
					XmNwidth, newWidth,
					0);

	XtUnmanageChild(selectColumnDialog);

} /* getColumnNamesCB */

static void
createCurrentColumnListStruct(
String*				strings,
int					numStrings)
{
	int i, j;

	if (_currentColumnCount != numStrings)
	{
		if (_ppsCurrentColumnListWidgets)
			XtFree((char*) _ppsCurrentColumnListWidgets);

		_ppsCurrentColumnListWidgets = (PPSColumnListSpec*)
						XtMalloc(sizeof(PPSColumnListSpec) * numStrings);
		_currentColumnCount = numStrings;
	}

	for (i=0; i < numStrings; i++)
	{
		for (j=0; j < _tableColumnCount; j++)
		{
			if (strcmp(strings[i], _ppsTotalColumnListWidgets[j].label) == 0)
			{
				(void) memcpy(&(_ppsCurrentColumnListWidgets[i]),
							&(_ppsTotalColumnListWidgets[j]),
							sizeof(PPSColumnListSpec));
				continue;
			}
		}
	}

} /* createCurrentColumnListStruct */

static void
queryCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	char	sqlBuf[4 * PPS_BIG_SIZE];
	int		i;
	String	*strings;
	int		numStrings;

	if (_currentSelectStrings == 0)
		return;
	(void)strcpy(sqlBuf, "select ");
	(void) strcat(sqlBuf, _currentSelectStrings[0]);
	for (i=1; i < _numCurrentSelectStrings; i++)
	{
		(void) strcat(sqlBuf, ",");
		(void) strcat(sqlBuf, _currentSelectStrings[i]);
	}
	(void) strcat(sqlBuf, " from ");
	(void) strcat(sqlBuf, databaseTableName);

	if (ppsGetSmartListStrings(smartList, &strings, &numStrings))
	{
		(void) strcat(sqlBuf, " where ");
		(void) strcat(sqlBuf, strings[0]);
		for (i=1; i < numStrings; i++)
		{
			(void) strcat(sqlBuf, ",");
			(void) strcat(sqlBuf, strings[i]);
		}

		ppsFreeSmartListStrings(strings, numStrings);
	}

	if (ppsGetOrderListStrings(sortList, &strings, &numStrings))
	{
		(void) strcat(sqlBuf, " order by ");
		(void) strcat(sqlBuf, strings[0]);
		for (i=1; i < numStrings; i++)
		{
			(void) strcat(sqlBuf, ",");
			(void) strcat(sqlBuf, strings[i]);
		}

		ppsFreeOrderListStrings(strings, numStrings);
	}

	/*---------------------------------------------------*/
	/* clear the query result list for new data          */
	/*---------------------------------------------------*/
	ppsColumnListDeleteAllItems(columnList);

printf("%s\n", sqlBuf);
	exec_query(sqlBuf);

	return;

} /* queryCB */

static void
exec_query(
char*		sqlBuf)
{
	PPS_QUERY_RESULT	*queryResult=0;
	int					rowsRead=0, i, columnLen=0;
	void**				valueArrays;
	int*				textSizes;
	int					itemCount=0;
	char				itemString[PPS_STRING_LEN];

	/* execute the sql */
	if (db_exec(&connection, sqlBuf, ROWS_PER_FETCH, &queryResult)
					== CS_FAIL)
	{
		fprintf(stderr, "db_exec failed\n");
		return;
	}

	/*-----------------------------------------------------*/
	/* sql executed sucessfully, now fetch all the results */
	/* and update the query results columns                */
	/*-----------------------------------------------------*/
	valueArrays = (void**)XtMalloc(_currentColumnCount * sizeof(void*));
	textSizes = (int*) XtMalloc(_currentColumnCount * sizeof(int));
	while (db_fetch(queryResult, &rowsRead) == CS_SUCCEED)
	{
		/* put the address of the fetched array in a void** */
		for (i=0; i < _currentColumnCount; i++)
		{
			valueArrays[i] = (void*)queryResult->columns[i].colValue;
			textSizes[i] = queryResult->columns[i].datafmt.maxlength;
		}
		ppsColumnListAddItems(columnList,
								rowsRead,
								_currentColumnCount,
								valueArrays,
								textSizes);
	}

	free(queryResult);
	XtFree((char*) valueArrays);
	XtFree((char*) textSizes);

	itemCount = ppsColumnListGetItemCount(columnList);
	(void)sprintf(itemString, "%10d", itemCount);
	XmTextFieldSetString(itemsText, itemString);

	return;

} /* exec_query */

static int
getColumnOptions(
CS_CONNECTION		**connection,
char*				tableName,
char*				columnName,
PPS_QUERY_RESULT	**columnOptionList,
int					*numOptions)
{
	char				sqlBuf[PPS_BIG_SIZE];
	int					rowsRead=0, i, columnLen=0;
	void**				valueArrays;
	int*				textSizes;
	int					itemCount=0;
	char				itemString[PPS_STRING_LEN];

	/*----------------------------------------------------*/
	/* retrieve the option values for the specified       */
	/* column from syscomments system table               */
	/*----------------------------------------------------*/
	(void)sprintf(sqlBuf, "sp_getColumnOptions '%s', '%s'",
						columnName, tableName);
	/* execute the sql */
	if (db_exec(connection, sqlBuf, ROWS_PER_FETCH, columnOptionList)
					== CS_FAIL)
	{
		fprintf(stderr, "db_exec to get column options failed\n");
		*columnOptionList = 0;
		return FALSE;
	}

	/*-----------------------------------------------------*/
	/* sql executed sucessfully, now fetch all the results */
	/*-----------------------------------------------------*/
	while (db_fetch(*columnOptionList, &rowsRead) == CS_SUCCEED)
	{
		char* str = (char*) (*columnOptionList)->columns[0].colValue;
		for (i=0; i < rowsRead; i++)
		{
			str += (*columnOptionList)->columns[0].datafmt.maxlength;
		}
	}

	*numOptions = rowsRead;

	return TRUE;

} /* getColumnOptions */
