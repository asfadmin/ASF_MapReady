/*-----------------------------------------*/
/* for vi: set ts=4 sw=4
/*-----------------------------------------*/

static const char rcsid[] = "@(#)dynQuery_util.c	1.4  03/05/97";

#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/TextF.h>

#include "db_util.h"
#include "gui_common.h"
#include "dynQuery_common.h"
#include "dynQuery_queryW.h"

#define NUM_MAX_OPTIONS		100

#define SERVER_OPTION		"serverOption"
#define USER_TEXT			"userText"
#define PASSWD_TEXT			"passwdText"
#define SUB_PULLDOWN		"subPulldown"

char	userName[PPS_STRING_LEN];
char	passwd[PPS_STRING_LEN];
char	serverName[PPS_STRING_LEN];
char	databaseName[PPS_STRING_LEN];
char	databaseTableName[PPS_STRING_LEN];

Widget	tableNameOption=0;
Widget	tableNamePane=0;

extern CS_CONNECTION	*connection;		/* sybase connection */
extern Widget			changeDatabaseDlg;
extern Widget			mainForm;

static void
userLoginCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	char	name[PPS_STRING_LEN];
	Widget queryDBAcctDlg = (Widget) clientData;
	Widget	serverOption=0, userText=0, passwdText=0;
	Widget	optionItem=0;
	XmString	xmString=0;
	String		textString=0;
	PPS_LOGIN_STRUCT	loginStruct;

	/*-------------------------------------------------------*/
	/* get username, passwd and server name from dialog      */
	/*-------------------------------------------------------*/
	sprintf(name, "*%s", SERVER_OPTION);
	serverOption = XtNameToWidget(queryDBAcctDlg, name);
	sprintf(name, "*%s", USER_TEXT);
	userText = XtNameToWidget(queryDBAcctDlg, name);
	sprintf(name, "*%s", PASSWD_TEXT);
	passwdText = XtNameToWidget(queryDBAcctDlg, name);

	if (serverOption == 0 || userText == 0 || passwdText == 0)
	{
		XppsCreateErrorDialog(queryDBAcctDlg,
						"Internal Error, NULL children in dialog",
						True, 0, 0);
		return;
	}

	XtVaGetValues(serverOption,
				XmNmenuHistory, &optionItem,
				0);
	if (optionItem == 0)
	{
		XppsCreateErrorDialog(queryDBAcctDlg,
						"Internal Error, NULL option item",
						True, 0, 0);
		return;
	}
	XtVaGetValues(optionItem,
				XmNlabelString, &xmString,
				0);

	XmStringGetLtoR(xmString, XmSTRING_DEFAULT_CHARSET, &textString);
	(void)strcpy(serverName, textString);
	XtFree(textString);

	textString = XmTextFieldGetString(userText);
	if (*textString == 0)
	{
		XppsCreateErrorDialog(queryDBAcctDlg, "Empty User Name", True, 0, 0);
		XtFree(textString);
		return;
	}
	(void)strcpy(userName, textString);
	XtFree(textString);

	textString = XmTextFieldGetString(passwdText);
	if (*textString == 0)
	{
		XppsCreateErrorDialog(queryDBAcctDlg, "Empty Password", True, 0, 0);
		XtFree(textString);
		return;
	}
	(void)strcpy(passwd, textString);
	XtFree(textString);

printf("server=%s, user=%s, passwd=%s\n", serverName, userName, passwd);

	/*-------------------------------------------------------*/
	/* disconnect the old connection, if exists              */
	/*-------------------------------------------------------*/
	if (connection != 0)
		(void)db_disconnect(connection);

	/*-------------------------------------------------------*/
	/* establish a new connection using the new info         */
	/*-------------------------------------------------------*/
	(void) strcpy(loginStruct.username, userName);
	(void) strcpy(loginStruct.password, passwd);
	(void) strcpy(loginStruct.servername, serverName);
	if (db_connect(&loginStruct, &connection) == CS_FAIL)
	{
		XppsCreateErrorDialog(queryDBAcctDlg,
						"Connect to Sybase Failed", True, 0, 0);
		return;
	}

#if 0
	createQueryWindow(mainForm);
#endif

} /* userLoginCB */


Widget
createQueryDBAcctDialog(
Widget		parent)
{
	Widget				queryDBAcctDlg;
	DATA_SERVER_LIST	serverListPtr;
	int                 serverCount;
	int					i, n;
	Widget				*pushButtons;
	Widget				subPulldown;
	Widget				serverOption;
	Widget				userLabel, passwdLabel;
	Widget				userText, passwdText;
	Widget				separator, loginPB, cancelPB;
	Arg					args[NUM_MAX_ARGS];
	XmString			xmString;

	/*--------------------------------------------*/
	/* create an option menu for all server names */
	/*--------------------------------------------*/
	if (getServerList(&serverListPtr, &serverCount) != CS_SUCCEED)
	{
		XppsCreateErrorDialog(parent,
						"Cannot get Database Server Name.",
						True, 0, 0);
		return;
	}

	queryDBAcctDlg = XmCreateFormDialog(parent, "queryDBAcctDlg", 0, 0);

	subPulldown = XmCreatePulldownMenu(queryDBAcctDlg, "subPulldown", 0, 0);
	pushButtons = (Widget*)XtMalloc(sizeof(Widget) * serverCount);

	for (i=0; i < serverCount; i++)
	{
		xmString = CREATE_XMSTRING(serverListPtr[i]);
		pushButtons[i] = XtVaCreateWidget(serverListPtr[i],
					xmPushButtonWidgetClass, subPulldown,
					XmNlabelString, xmString,
					0);
		XmStringFree(xmString);
	}
	XtManageChildren(pushButtons, serverCount);

	xmString = CREATE_XMSTRING("Server Name:");
	n=0;
	XtSetArg(args[n], XmNsubMenuId, subPulldown); n++;
	XtSetArg(args[n], XmNlabelString, xmString); n++;
	serverOption = XmCreateOptionMenu(queryDBAcctDlg, "serverOption", args, n);
	XtManageChild(serverOption);
	XmStringFree(xmString);
	XtVaSetValues(serverOption,
					XmNtopAttachment, XmATTACH_FORM,
					XmNleftAttachment, XmATTACH_FORM,
					0);

	free(serverListPtr);

	/*--------------------------------------------*/
	/* create label and text for user name        */
	/*--------------------------------------------*/
	xmString = CREATE_XMSTRING("  User Name:");
	userLabel = XtVaCreateManagedWidget("userLabel",
					xmLabelWidgetClass, queryDBAcctDlg,
					XmNlabelString, xmString,
					XmNtopAttachment, XmATTACH_WIDGET,
					XmNtopWidget, serverOption,
					XmNleftAttachment, XmATTACH_FORM,
					0);
	XmStringFree(xmString);
	userText = XtVaCreateManagedWidget("userText",
					xmTextFieldWidgetClass, queryDBAcctDlg,
					XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
					XmNtopWidget, userLabel,
					XmNtopOffset, -2,
					XmNleftAttachment, XmATTACH_WIDGET,
					XmNleftWidget, userLabel,
					XmNrightAttachment, XmATTACH_FORM,
					0);

	/*--------------------------------------------*/
	/* create label and text for password         */
	/*--------------------------------------------*/
	xmString = CREATE_XMSTRING("   Password:");
	passwdLabel = XtVaCreateManagedWidget("passwdLabel",
					xmLabelWidgetClass, queryDBAcctDlg,
					XmNlabelString, xmString,
					XmNtopAttachment, XmATTACH_WIDGET,
					XmNtopWidget, userLabel,
					XmNrightAttachment, XmATTACH_OPPOSITE_WIDGET,
					XmNrightWidget, userLabel,
					XmNrightOffset, 0,
					0);
	XmStringFree(xmString);
	passwdText = XtVaCreateManagedWidget("passwdText",
					xmTextFieldWidgetClass, queryDBAcctDlg,
					XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
					XmNtopWidget, passwdLabel,
					XmNtopOffset, -2,
					XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
					XmNleftWidget, userText,
					XmNleftOffset, 0,
					XmNrightAttachment, XmATTACH_FORM,
					0);

	separator = XtVaCreateManagedWidget("separator",
					xmSeparatorWidgetClass, queryDBAcctDlg,
					XmNtopAttachment, XmATTACH_WIDGET,
					XmNtopWidget, passwdText,
					XmNleftAttachment, XmATTACH_FORM,
					XmNleftOffset, 0,
					XmNrightAttachment, XmATTACH_FORM,
					XmNrightOffset, 0,
					0);

	xmString = CREATE_XMSTRING("Login");
	loginPB = XtVaCreateManagedWidget("loginPB",
					xmPushButtonWidgetClass, queryDBAcctDlg,
					XmNlabelString, xmString,
					XmNshowAsDefault, True,
					XmNtopAttachment, XmATTACH_WIDGET,
					XmNtopWidget, separator,
					XmNbottomAttachment, XmATTACH_FORM,
					XmNleftAttachment, XmATTACH_POSITION,
					XmNleftPosition, 10,
					0);
	XmStringFree(xmString);
	XtAddCallback(loginPB, XmNactivateCallback,
					userLoginCB, (XtPointer) queryDBAcctDlg);

	xmString = CREATE_XMSTRING("Cancel");
	cancelPB = XtVaCreateManagedWidget("cancelPB",
					xmPushButtonWidgetClass, queryDBAcctDlg,
					XmNlabelString, xmString,
					XmNtopAttachment, XmATTACH_WIDGET,
					XmNtopWidget, separator,
					XmNbottomAttachment, XmATTACH_FORM,
					XmNrightAttachment, XmATTACH_POSITION,
					XmNrightPosition, 90,
					0);
	XmStringFree(xmString);

	XtVaSetValues(queryDBAcctDlg,
						XmNhorizontalSpacing, 20,
						XmNverticalSpacing, 20,
						0);
	XtManageChild(queryDBAcctDlg);

	return(queryDBAcctDlg);

} /* createQueryDBAcctDialog */

static Boolean
sqlGetNameString(
String		sqlBuf,				/* IN */
int			fetchOnly,			/* IN */
String		*nameStrings,		/* OUT, needs free() */
int			*nameSize,			/* OUT */
int			*nameCount)			/* OUT */
{
	static PPS_QUERY_RESULT	*queryResult;
	int						rowsRead=0;

	/* execute the sql */
	if ( ! fetchOnly &&
		db_exec(&connection, sqlBuf, ROWS_PER_FETCH, &queryResult)
					== CS_FAIL)
	{
		fprintf(stderr, "db_exec failed\n");
		return;
	}

	/*-----------------------------------------------------*/
	/* sql executed sucessfully, now fetch all the results */
	/* and return them                                     */
	/*-----------------------------------------------------*/
	if (db_fetch(queryResult, &rowsRead) == CS_SUCCEED)
	{
		*nameStrings = (char*)queryResult->columns[0].colValue;
		*nameSize = queryResult->columns[0].datafmt.maxlength;
		*nameCount = rowsRead;
		return True;
	}
	else
	{
		*nameStrings = 0;
		*nameSize = 0;
		*nameCount = 0;
		free(queryResult);
		return False;
	}
} /* sqlGetNameString */

static void
selectTableCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	Widget		optionItem=0;
	XmString	xmString=0;
	char*		labelText=0;

	XtVaGetValues(tableNameOption,
				XmNmenuHistory, &optionItem,
				0);

	XtVaGetValues(optionItem,
				XmNlabelString, &xmString,
				0);
	if (xmString)
	{
		XmStringGetLtoR(xmString, XmSTRING_DEFAULT_CHARSET, &labelText);
		if (labelText)
		{
			strncpy(databaseTableName, labelText, PPS_STRING_LEN-1);
			databaseTableName[PPS_STRING_LEN-1] = '\0';
			XtFree(labelText);
		}
	}

	createQueryWindow(mainForm);

} /* selectTableCB */

static void
databaseNameChangedCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	Widget		currentDBPB;
	Widget		*tableNamePBs;
	String		widgetName=0;
	String		databaseTableNames;
	String		tempStr=0;
	XmString	xmString;
	int			databaseTableSize;
	int			databaseTableCount;
	int			fetchOnly;
	int			count;
	int			i;
	XmRowColumnCallbackStruct* rcData = (XmRowColumnCallbackStruct*) callData;

	/*---------------------------------------------------------*/
	/* switch the database                                     */
	/*---------------------------------------------------------*/
	if (rcData)
		widgetName = XtName(rcData->widget);
	else
	{
		XtVaGetValues(w,
					XmNmenuHistory, &currentDBPB,
					0);
		widgetName = XtName(currentDBPB);
	}
printf("switching to %s\n", widgetName);
	(void) strcpy(databaseName, widgetName);
	if (db_set_database(&connection, databaseName) != CS_SUCCEED)
	{
		return;
	}

	/*---------------------------------------------------------*/
	/* create the pulldown pane for all the database tables    */
	/* for the newly selected database name                    */
	/*---------------------------------------------------------*/
	if (tableNamePane)
		XtDestroyWidget(tableNamePane);

	databaseTableCount = 0;
	tableNamePane = XmCreatePulldownMenu(changeDatabaseDlg,
					"tableNamePane", 0, 0);
	XtVaSetValues(tableNamePane,
					XmNorientation, XmHORIZONTAL,
					XmNpacking, XmPACK_COLUMN,
					XmNnumColumns, 20,
					0);
	tableNamePBs = (Widget*)XtMalloc(sizeof(Widget) * 100);
	fetchOnly = False;
	while(sqlGetNameString(
		"select name from sysobjects where type = 'U' or type = 'V'",
			fetchOnly, &databaseTableNames, &databaseTableSize, &count))
	{
		tempStr = databaseTableNames;
		for (i=0; i < count; i++)
		{
			xmString = CREATE_XMSTRING(tempStr);
			tableNamePBs[databaseTableCount++] = XtVaCreateWidget(tempStr,
						xmPushButtonWidgetClass, tableNamePane,
						XmNlabelString, xmString,
						0);
			XmStringFree(xmString);
			tempStr += databaseTableSize;
		}
		fetchOnly = True;
	}
	XtManageChildren(tableNamePBs, databaseTableCount);

	XtVaSetValues(	tableNameOption,
					XmNsubMenuId, tableNamePane,
					0);
} /* databaseNameChangedCB */

void
createChangeDatabaseDialog(
Widget		parent)
{
	String				tempStr=0;
	int					width=0;
	String				databaseNames;
	int					databaseNameSize;
	int					databaseCount;
	int					fetchOnly;
	int					count;
	int					i, n;
	Widget				*dbNamePBs;
	Widget				dbNamePane;
	Widget				dbNameOption;
	Widget				separator, selectPB, cancelPB;
	Widget				currentDBPB;
	Arg					args[NUM_MAX_ARGS];
	XmString			xmString;
	static XtCallbackRec entryCallbacks[] =
							{ {databaseNameChangedCB, 0},
							  { 0, 0 } };

	changeDatabaseDlg = XmCreateFormDialog(parent, "changeDatabaseDlg", 0, 0);

	/*----------------------------------------------*/
	/* create an option menu for all database names */
	/*----------------------------------------------*/
	databaseCount = 0;

	n=0;
	XtSetArg(args[n], XmNentryCallback, entryCallbacks); n++;
	dbNamePane = XmCreatePulldownMenu(changeDatabaseDlg, "dbNamePane",
					args, n);
	XtVaSetValues(dbNamePane,
					XmNorientation, XmHORIZONTAL,
					XmNpacking, XmPACK_COLUMN,
					XmNnumColumns, 20,
					0);
	dbNamePBs = (Widget*)XtMalloc(sizeof(Widget) * 100);
	fetchOnly = False;
	while ( sqlGetNameString("select name from master..sysdatabases",
			fetchOnly, &databaseNames, &databaseNameSize, &count))
	{
		tempStr = databaseNames;
		for (i=0; i < count; i++)
		{
			xmString = CREATE_XMSTRING(tempStr);
			dbNamePBs[databaseCount++] = XtVaCreateWidget(tempStr,
						xmPushButtonWidgetClass, dbNamePane,
						XmNlabelString, xmString,
						0);
			XmStringFree(xmString);
			tempStr += databaseNameSize;
		}
		fetchOnly = True;
	}
	XtManageChildren(dbNamePBs, databaseCount);

	xmString = CREATE_XMSTRING("Database Name:");
	n=0;
	XtSetArg(args[n], XmNsubMenuId, dbNamePane); n++;
	XtSetArg(args[n], XmNlabelString, xmString); n++;
	dbNameOption = XmCreateOptionMenu(changeDatabaseDlg,
							"dbNameOption", args, n);
	XtManageChild(dbNameOption);
	XmStringFree(xmString);
	XtVaSetValues(dbNameOption,
					XmNtopAttachment, XmATTACH_FORM,
					XmNleftAttachment, XmATTACH_FORM,
					0);

	/*----------------------------------------------------------------*/
	/* find out the default database name and set the option button   */
	/*----------------------------------------------------------------*/
	fetchOnly = False;
	while(sqlGetNameString( "select db_name()", fetchOnly, &tempStr,
		&width, &count))
	{
		(void)strcpy(databaseName, tempStr);
		currentDBPB = XtNameToWidget(dbNamePane, tempStr);
		XtVaSetValues(dbNameOption,
					XmNmenuHistory, currentDBPB,
					0);
		fetchOnly = True;
	}

	/*-------------------------------------------*/
	/* create an option menu for all table names */
	/*-------------------------------------------*/
	xmString = CREATE_XMSTRING("   Table Name:");
	n=0;
	XtSetArg(args[n], XmNlabelString, xmString); n++;
	tableNameOption = XmCreateOptionMenu(changeDatabaseDlg,
							"tableNameOption", args, n);
	databaseNameChangedCB(dbNameOption, 0, 0);
	XtManageChild(tableNameOption);
	XmStringFree(xmString);

	XtVaSetValues(tableNameOption,
					XmNtopAttachment, XmATTACH_WIDGET,
					XmNtopWidget, dbNameOption,
					XmNleftAttachment, XmATTACH_FORM,
					0);

	separator = XtVaCreateManagedWidget("separator",
					xmSeparatorWidgetClass, changeDatabaseDlg,
					XmNtopAttachment, XmATTACH_WIDGET,
					XmNtopWidget, tableNameOption,
					XmNleftAttachment, XmATTACH_FORM,
					XmNleftOffset, 0,
					XmNrightAttachment, XmATTACH_FORM,
					XmNrightOffset, 0,
					0);

	xmString = CREATE_XMSTRING("Select");
	selectPB = XtVaCreateManagedWidget("selectPB",
					xmPushButtonWidgetClass, changeDatabaseDlg,
					XmNlabelString, xmString,
					XmNshowAsDefault, True,
					XmNtopAttachment, XmATTACH_WIDGET,
					XmNtopWidget, separator,
					XmNbottomAttachment, XmATTACH_FORM,
					XmNleftAttachment, XmATTACH_POSITION,
					XmNleftPosition, 10,
					0);
	XmStringFree(xmString);
	XtAddCallback(selectPB, XmNactivateCallback,
					selectTableCB, (XtPointer) changeDatabaseDlg);

	xmString = CREATE_XMSTRING("Cancel");
	cancelPB = XtVaCreateManagedWidget("cancelPB",
					xmPushButtonWidgetClass, changeDatabaseDlg,
					XmNlabelString, xmString,
					XmNtopAttachment, XmATTACH_WIDGET,
					XmNtopWidget, separator,
					XmNbottomAttachment, XmATTACH_FORM,
					XmNrightAttachment, XmATTACH_POSITION,
					XmNrightPosition, 90,
					0);
	XmStringFree(xmString);

	XtVaSetValues(changeDatabaseDlg,
						XmNhorizontalSpacing, 20,
						XmNverticalSpacing, 20,
						0);
	XtManageChild(changeDatabaseDlg);

	return;

} /* createChangeDatabaseDialog */
