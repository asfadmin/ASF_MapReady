static char *sccs = "@(#)ims_otIngest.c	1.11 11/24/97";
/*****************************************************************************
*
**
** File:    ims_otIngest.c
**
** Function: 
**			
**
** Author: Jennifer Ting
**
** Date:    3/15/96
**
** Revision:  06/19/96  Modified ims_otIngestOkCB.  Remove leading and
**                      trailing blanks of source directory and file name.
**
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <sys/signal.h>

#include <X11/Intrinsic.h>
#include <Xm/MainW.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <Xm/SelectioB.h>
#include <Xm/DialogS.h>
#include <Xm/MenuShell.h>

#include <ims_dbms.h>
#include <ims_query.h> 
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <ims_cmd.h>
#include <syslog.h>

#include <odldef.h>
#include <ims_odl.h>

#include <ims_archive.h>
#include <ims_pmfTool.h>

#define DATASET_DLG 0
#define INGEST_DLG 1

typedef struct 
{
	Widget form_w;
	Widget dataset_w;
	Widget dataset_list_w;
	Widget platform_w;
	Widget sensor_w;
	Widget format_w;
	Widget fileTypes_w;
	Widget noFiles_w;
	Widget name_w;
	Widget fileName_w;
	Widget userName_w;
	Widget password_w;
	Widget accountID_w;
	Widget database_w;
	Widget server_w;
	Widget ingest_w;
	Widget cancel_w;
} IMS_INGEST_WIDGETS;

IMS_INGEST_WIDGETS ims_ingestWidgets;

typedef struct 
{
	Widget form_w;
	Widget dataset_w;
	Widget platform_w;
	Widget sensor_w;
} IMS_DATASET_SELECT_WIDGETS;

IMS_DATASET_SELECT_WIDGETS ims_datasetSelectWidgets;

typedef DBCHAR item[IMS_COL30_LEN+1];

int platform_count;
item platform[10];

int sensor_count;
item sensor[5];

int format_count;
item format[5];

int fileType_count;
item fileType[5];
 
IMS_CLNT_REQUEST_TYPE requestType = IMS_ADD;

static void ims_otIngestCancelCB(Widget, XtPointer, XtPointer);
static void ims_otIngestOkCB(Widget, XtPointer, XtPointer);
static void ims_otRequestTypeCB(Widget, XtPointer, XtPointer);
static Widget ims_otBuildOptionMenu(Widget, Widget, int, item *);
static void ims_otDatasetSelectOkCB(Widget, XtPointer, XtPointer);
static void ims_otDatasetListCB(Widget, XtPointer, XtPointer);
static void ims_otDatasetListOkCB
					(Widget, XtPointer, XmSelectionBoxCallbackStruct *);
static int ims_otGetPolicyInfo(char *);

extern int openConnection (IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ **);
extern int closeConnection (IMS_QI_DESC_OBJ *);
extern int ims_widgetToTree(IMS_MSG_STRUCT *, IMS_ODL_TREE *);
extern void check_passwd (Widget, XtPointer, XtPointer);

/*******************************************************************
** 
** ims_otIngestCB
**
*******************************************************************/
void ims_otIngestCB(
	Widget main_w,
	XtPointer client_data,
	XtPointer call_data)
{
	Widget ingestDlg, title_w, label_w, separator_w;
	Widget platformOM_pane_shell, platformOM_pane;
	Widget sensorOM_pane_shell, sensorOM_pane;
	Widget formatOM_pane_shell, formatOM_pane;
	XmString labelStr, add, replace;
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	char buffer[20];
	char *buffPtr;
	Widget menu, radio_box;
	int i;
	char *fileName, *endPtr, filePath[225];
	char *objectClass = "keyword_mand";

	/* initialization */
	memset((char *) &ims_ingestWidgets, 0, sizeof(ims_otWidgets));
	memset((char *) &filePath, 0, sizeof(filePath));
	fileName = NULL;

	/* initialize requestType each time ingest dialog pops up */
	requestType = IMS_ADD;

	platform_count = sensor_count = format_count = fileType_count = 0;

	/*
	** Based on the dataset passed, select the following information
	** from the catalog: dataset_relation.platform, dataset_relation.sensor,
	** dataset_file_policy.format, dataset_file_policy.type
	*/
	if (ims_ot_request.dataset)
	{
		if (ims_otGetPolicyInfo(ims_ot_request.dataset) < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not get policy information.");
			(void) ims_otMsg(msgDesc);
			return;
		}
	}
	
	ingestDlg = XtVaCreatePopupShell ("ingestDlg",
								xmDialogShellWidgetClass, main_w,
								XmNtitle, "Ingest Dialog",
								XmNdeleteResponse, XmDESTROY,
								XmNwidth, 400,
								XmNheight, 730,
								NULL);

	ims_ingestWidgets.form_w = 
							XtVaCreateWidget ("form", xmFormWidgetClass, ingestDlg,
								XmNwidth, 400,
								XmNheight, 730,
								XmNautoUnmanage, False,
								NULL);

	labelStr = XmStringCreateLocalized ("INGEST");

	/* Creation of label INGEST */
	title_w = XtVaCreateManagedWidget( "ingestLBL",
			xmLabelWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNlabelString, labelStr,
			XmNwidth, 50,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 100,
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 100,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 10,
			NULL );

	XmStringFree(labelStr);

	labelStr = XmStringCreateLocalized ("DATASET:");

	/* Creation of label DATASET */
	label_w = XtVaCreateManagedWidget( "datasetLBL",
			xmLabelWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNlabelString, labelStr,
			XmNwidth, 70,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, title_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);

	/* Creation of text widget DATASET */
	ims_ingestWidgets.dataset_w = XtVaCreateManagedWidget( objectClass,
			xmTextWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 164,
			XmNeditable, FALSE,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, title_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			NULL );

	if (ims_ot_request.dataset)
	{
		XmTextSetString (ims_ingestWidgets.dataset_w, ims_ot_request.dataset);
	}

	
	/* Creation of pushbutton Dataset List... */
	labelStr = XmStringCreateLocalized ("List...");

	ims_ingestWidgets.dataset_list_w = XtVaCreateManagedWidget( "datasetListPB",
			xmPushButtonWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNlabelString, labelStr,
			XmNheight, 30,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, title_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, ims_ingestWidgets.dataset_w,
			XmNleftOffset, 10,
			NULL );

	XtAddCallback(ims_ingestWidgets.dataset_list_w, XmNactivateCallback,
			ims_otDatasetListCB, (XtPointer)INGEST_DLG);

	XmStringFree(labelStr);


	labelStr = XmStringCreateLocalized ("PLATFORM:");

	/* Creation of label PLATFORM */
	label_w = XtVaCreateManagedWidget( "platformLBL",
			xmLabelWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 70,
			XmNlabelString, labelStr,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.dataset_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);


	/* Creation of PLATFORM option menu */
	ims_ingestWidgets.platform_w = XtVaCreateManagedWidget ("platformOM",
			xmRowColumnWidgetClass,
			ims_ingestWidgets.form_w,
			XmNrowColumnType, XmMENU_OPTION,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.dataset_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			NULL);

	/* Build platform option menu items */
	menu = ims_otBuildOptionMenu (ims_ingestWidgets.form_w, 
																ims_ingestWidgets.platform_w,
																platform_count, platform);

	labelStr = XmStringCreateLocalized ("SENSOR:");

	/* Creation of label SENSOR */
	label_w = XtVaCreateManagedWidget( "sensorLBL",
			xmLabelWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 70,
			XmNlabelString, labelStr,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.platform_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);

	/* Creation of SENSOR option menu */
	ims_ingestWidgets.sensor_w = XtVaCreateManagedWidget ("sensorOM",
			xmRowColumnWidgetClass,
			ims_ingestWidgets.form_w,
			XmNrowColumnType, XmMENU_OPTION,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.platform_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			NULL);

	/* Build sensor option menu items */
	menu = ims_otBuildOptionMenu (ims_ingestWidgets.form_w,
																ims_ingestWidgets.sensor_w,
																sensor_count, sensor);


	labelStr = XmStringCreateLocalized ("FORMAT:");

	/* Creation of label FORMAT */
	label_w = XtVaCreateManagedWidget( "formatLBL",
			xmLabelWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 70,
			XmNlabelString, labelStr,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.sensor_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);

	/* Creation of FORMAT option menu */
	ims_ingestWidgets.format_w = XtVaCreateManagedWidget ("formatOM",
			xmRowColumnWidgetClass,
			ims_ingestWidgets.form_w,
			XmNrowColumnType, XmMENU_OPTION,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.sensor_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			NULL);

	/* Build format option menu items */
	menu = ims_otBuildOptionMenu (ims_ingestWidgets.form_w,
																ims_ingestWidgets.format_w,
																format_count, format);

	labelStr = XmStringCreateLocalized ("FILE TYPES:");


	/* Creation of label FILE TYPES */
	label_w = XtVaCreateManagedWidget( "fileTypesLBL",
			xmLabelWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 70,
			XmNlabelString, labelStr,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.format_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);

	buffPtr = buffer; 
	for (i = 0; i < fileType_count; i++)
	{
		sprintf (buffPtr, (i? ",%s" : "%s"), fileType[i]);
		buffPtr = buffPtr + strlen(buffPtr);
	}

	/* Creation of FILE TYPES text widget */
	ims_ingestWidgets.fileTypes_w = XtVaCreateManagedWidget( objectClass,
			xmTextWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 164,
			XmNeditable, FALSE,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.format_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			XmNvalue, buffer,
			NULL );

	labelStr = XmStringCreateLocalized ("NAME:");

	/* Creation of label NAME */
	label_w = XtVaCreateManagedWidget( "nameLBL",
			xmLabelWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 70,
			XmNlabelString, labelStr,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.fileTypes_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);

	if (ims_ot_userSpec.saveFile)
	{
		fileName = ims_extractFileName (ims_ot_userSpec.saveFile);
		if ((endPtr = strrchr (fileName, '.')) != NULL)
		{
			fileName[abs(fileName-endPtr)] = '\0';
		}
	}

	/* Creation of NAME text widget */
	ims_ingestWidgets.name_w = XtVaCreateManagedWidget( objectClass,
			xmTextWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 164,
			XmNeditable, TRUE,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.fileTypes_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			NULL );

	if (fileName)
	{
		XmTextSetString (ims_ingestWidgets.name_w, fileName);
	}

	/* Creation of label Source Dir */
	labelStr = XmStringCreateLocalized ("Source Dir:");

	label_w = XtVaCreateManagedWidget( "sourceDirLBL",
			xmLabelWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 70,
			XmNlabelString, labelStr,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.name_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);

	if (ims_ot_userSpec.saveFile)
	{
		(void) ims_extractPath (ims_ot_userSpec.saveFile, filePath);
	}

	/* Creation of Source Directory text widget */
	ims_ingestWidgets.fileName_w = XtVaCreateManagedWidget( objectClass,
			xmTextWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 164,
			XmNeditable, TRUE,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.name_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			NULL );

	if (filePath)
	{
		XmTextSetString (ims_ingestWidgets.fileName_w, filePath);
	}

	/* Creation of separator widget */
	separator_w = XtVaCreateManagedWidget( "separator1",
			xmSeparatorWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.fileName_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			NULL );

	labelStr = XmStringCreateLocalized ("USER NAME:");

	/* Creation of label USER NAME */
	label_w = XtVaCreateManagedWidget( "userNameLBL",
			xmLabelWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 70,
			XmNlabelString, labelStr,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, separator_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);

	/* Creation of USER NAME text widget */
	ims_ingestWidgets.userName_w = XtVaCreateManagedWidget( objectClass,
			xmTextWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 164,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, separator_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			XmNmaxLength, 15,
			NULL );

	ims_truncStr (ims_ot_request.username);
	if (ims_ot_request.username != NULL)
	{
		XmTextSetString (ims_ingestWidgets.userName_w, ims_ot_request.username);
	}
	else
	{
		XmTextSetString (ims_ingestWidgets.userName_w, ims_ot_userSpec.username);
	}

	labelStr = XmStringCreateLocalized ("PASSWORD:");

	/* Creation of label PASSWORD */
	label_w = XtVaCreateManagedWidget( "passwordLBL",
			xmLabelWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 70,
			XmNlabelString, labelStr,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.userName_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);

	/* Creation of PASSWORD text widget */
	ims_ingestWidgets.password_w = XtVaCreateManagedWidget( objectClass,
			xmTextWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 164,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.userName_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			XmNmaxLength, 15,
			NULL );

	XtAddCallback(ims_ingestWidgets.password_w,  XmNmodifyVerifyCallback, 
														 check_passwd, (XtPointer) 1);

	ims_ot_request.password = NULL;
/*
	if (ims_ot_request.password)
	{
		XmTextSetString (ims_ingestWidgets.password_w, ims_ot_request.password);
	}
	else
	{
		XmTextSetString (ims_ingestWidgets.password_w, ims_ot_userSpec.password);
	}
*/

	labelStr = XmStringCreateLocalized ("ACCOUNT ID:");

	/* Creation of label ACCOUNT ID */
	label_w = XtVaCreateManagedWidget( "accountIDLBL",
			xmLabelWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 70,
			XmNlabelString, labelStr,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.password_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);

	/* Creation of ACCOUNT ID text widget */
	ims_ingestWidgets.accountID_w = XtVaCreateManagedWidget( 
			objectClass,
			xmTextWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 164,
			XmNeditable, TRUE,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.password_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			XmNmaxLength, 15,
			NULL );

	if (ims_ot_request.accountId)
	{
		XmTextSetString (ims_ingestWidgets.accountID_w, ims_ot_request.accountId);
	}

	labelStr = XmStringCreateLocalized ("DATABASE:");

	/* Creation of label DATABASE */
	label_w = XtVaCreateManagedWidget( "databaseLBL",
			xmLabelWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 70,
			XmNlabelString, labelStr,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.accountID_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);

	/* Creation of DATABASE text widget */
	ims_ingestWidgets.database_w = XtVaCreateManagedWidget( objectClass,
			xmTextWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 164,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.accountID_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			XmNmaxLength, 15,
			NULL );

	if (ims_ot_request.catDbName)
	{
		XmTextSetString (ims_ingestWidgets.database_w, ims_ot_request.catDbName);
	}
	else
	{
		XmTextSetString (ims_ingestWidgets.database_w, ims_ot_userSpec.database);
	}

	labelStr = XmStringCreateLocalized ("SERVER:");

	/* Creation of label SERVER */
	label_w = XtVaCreateManagedWidget( "serverLBL",
			xmLabelWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 70,
			XmNlabelString, labelStr,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.database_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);

	/* Creation of SERVER text widget */
	ims_ingestWidgets.server_w = XtVaCreateManagedWidget( objectClass,
			xmTextWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNwidth, 164,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_ingestWidgets.database_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			XmNvalue, ims_ot_userSpec.server,
			XmNmaxLength, 15,
			NULL );

	if (ims_ot_request.catSrvName != NULL)
	{
		XmTextSetString (ims_ingestWidgets.server_w, ims_ot_request.catSrvName);
	}
	else
	{
		XmTextSetString (ims_ingestWidgets.server_w, ims_ot_userSpec.server);
	}

	/* Creation of radio box */
	add = XmStringCreateLocalized ("Add");
	replace = XmStringCreateLocalized ("Replace");

	radio_box = XmVaCreateSimpleRadioBox (ims_ingestWidgets.form_w, 
									"radio_box",
									0,
									ims_otRequestTypeCB,
									XmVaRADIOBUTTON, add, NULL, NULL, NULL,
									XmVaRADIOBUTTON, replace, NULL, NULL, NULL,
									XmNtopAttachment, XmATTACH_WIDGET,
									XmNtopWidget, ims_ingestWidgets.server_w,
									XmNtopOffset, 10,
									XmNleftAttachment, XmATTACH_FORM,
									XmNleftOffset, 40,
									XmNorientation, XmHORIZONTAL,
									NULL);

	XtManageChild (radio_box);

	XmStringFree (add);
	XmStringFree (replace);


	/* Creation of separator widget */
	separator_w = XtVaCreateManagedWidget( "separator2",
			xmSeparatorWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, radio_box,
			XmNtopOffset, 10,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			NULL );

	/* Creation of pushbutton INGEST */
	labelStr = XmStringCreateLocalized ("INGEST");

	ims_ingestWidgets.ingest_w = XtVaCreateManagedWidget( "ingestPB",
			xmPushButtonWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNlabelString, labelStr,
			XmNheight, 30,
			XmNwidth, 100,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, separator_w,
			XmNtopOffset, 20,
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			NULL );

	XtAddCallback (ims_ingestWidgets.ingest_w, XmNactivateCallback, 
																	ims_otIngestOkCB, NULL);

	XmStringFree(labelStr);

	/* Creation of pushbutton CANCEL */
	labelStr = XmStringCreateLocalized ("CANCEL");

	ims_ingestWidgets.cancel_w = XtVaCreateManagedWidget( "cancelPB",
			xmPushButtonWidgetClass,
			ims_ingestWidgets.form_w,  
			XmNlabelString, labelStr,
			XmNheight, 30,
			XmNwidth, 100,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, separator_w,
			XmNtopOffset, 20,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, ims_ingestWidgets.ingest_w,
			XmNleftOffset, 120,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 40,
			NULL );

	XmStringFree(labelStr);

	XtAddCallback (ims_ingestWidgets.cancel_w, XmNactivateCallback, 
																	ims_otIngestCancelCB, NULL);

	XtManageChild(ims_ingestWidgets.form_w);
	XtPopup(ingestDlg, XtGrabNone);

}

/*******************************************************************
** 
** ims_otDatasetListCB
**
*******************************************************************/
static void ims_otDatasetListCB(
	Widget w,
	XtPointer client_data,
	XtPointer call_data)
{
	Widget selectionDlg;
	XmString labelStr;
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	IMS_QI_DESC_OBJ *qDesc;
	int i, memory, status, rowCount;
	XmStringTable datasetStr;
	char buffer[IMS_COL80_LEN+1];
	int which_dlg = (int)client_data;

	typedef struct datasetStruct
	{
		DBCHAR dataset[IMS_COL80_LEN+1];
		struct datasetStruct *next;
	} DATASET_STRUCT;

	DATASET_STRUCT *datasetList, *newPtr, *dPtr;


	if (openConnection(msgDesc, (IMS_QI_DESC_OBJ **) &qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not open connection to database server");
		(void) ims_otMsg(msgDesc);
		return;
	}

	rowCount = 0;

	sprintf(qDesc->cmd, 
		"select distinct dataset from dataset_relation order by dataset");

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			closeConnection(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not load dataset information.");
			(void) ims_otMsg(msgDesc);
			return;
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/

		if (status == IMS_ENDOFQUERY) continue;

		/*
		** Process returned rows
		*/
		rowCount++;

		/*
		** Get datasets
		*/

		if ((newPtr = (DATASET_STRUCT *)
			malloc (sizeof (DATASET_STRUCT))) == (DATASET_STRUCT *)NULL)
		{
			(void) ims_msg (msgDesc, IMS_ERROR, 
				"Memory allocation for datasets failed.");
			(void) ims_otMsg(msgDesc);
			return;
		}

		(void) memcpy((char *)newPtr->dataset, qDesc->valAddr[0],
			qDesc->valLength[0]);
		newPtr->dataset[qDesc->valLength[0]] = '\0';
		ims_truncStr (newPtr->dataset);

		newPtr->next = NULL;

		if (rowCount == 1)
		{
			datasetList = newPtr;
			dPtr = newPtr;
		}
		else
		{
			dPtr->next = newPtr;
			dPtr = dPtr->next;
		}

	}
	closeConnection(qDesc);


	/*
	** Build Selection Dialog
	*/
	if ((rowCount <= 0) || (datasetList == NULL))
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
				"No dataset found in the catalog.");
		(void) ims_otMsg(msgDesc);
		return;
	}
	else
	{
		/* create selection dialog box */
		selectionDlg = (Widget)XmCreateSelectionDialog (w, "selection", NULL, 0);

		XtAddCallback (selectionDlg, XmNokCallback,
									 (XtCallbackProc) ims_otDatasetListOkCB, 
									 (XtPointer)which_dlg);
		
		/* Unmange Help and Apply buttons */
		XtUnmanageChild (
			XmSelectionBoxGetChild (selectionDlg, XmDIALOG_HELP_BUTTON));

		XtUnmanageChild (
			XmSelectionBoxGetChild (selectionDlg, XmDIALOG_APPLY_BUTTON));

		/* create dataset list strings */	
		memory = 1;
		datasetStr = (XmStringTable)XtMalloc(rowCount *sizeof(XmString *));

		memory = memory && datasetStr;
		if (!memory)
		{
			i = rowCount;
			while (--i)
				XmStringFree (datasetStr[i]);
			
			XtFree ((char *)datasetStr);
			(void) ims_msg(msgDesc, IMS_ERROR, 
					"Insufficient memory to create dataset list strings, please exit program.");
			(void) ims_otMsg(msgDesc);
			return;
		}
		
		dPtr = datasetList;
		for (i = 0; i < rowCount && dPtr != NULL; i++)
		{
			sprintf (buffer, "%s", dPtr->dataset);
			datasetStr[i] = XmStringCreateLocalized(buffer);
			dPtr = dPtr->next;
		}

		XtVaSetValues(selectionDlg, 
				XmNlistItems, datasetStr,
				XmNlistItemCount, rowCount,
				NULL);

		/* free memory allocated */
		dPtr = datasetList;
		while (dPtr != NULL)
		{
			datasetList = datasetList->next;
			free (dPtr);
			dPtr = datasetList;
		}

		/* free compound strings */
		while (--i)
			XmStringFree (datasetStr[i]);
	
		XtManageChild (selectionDlg);
	}
}

/*******************************************************************
** 
** ims_otGetPolicyInfo
**
*******************************************************************/
static int ims_otGetPolicyInfo(
	char *dataset)
{
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	IMS_QI_DESC_OBJ *qDesc;
	int status, rowCount;

	/*
	** base on the dataset value, let's get the following: 
	** PLATFORM, SENSOR, FORMAT, FILE TYPES
	*/

	if (openConnection(msgDesc, (IMS_QI_DESC_OBJ **) &qDesc) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not open connection to database server");
		(void) ims_otMsg(msgDesc);
		return (IMS_ERROR);
	}

	/* Let's get platform */
	rowCount = 0;

	sprintf(qDesc->cmd, 
		"select distinct platform from dataset_relation where "
		"dataset = '%s'", dataset);

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			closeConnection(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not get platform information.");
			(void) ims_otMsg(msgDesc);
			return (IMS_ERROR);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY) continue;

		/*
		** Process returned rows
		*/
		(void) memcpy 
			((char *)platform[rowCount], qDesc->valAddr[0], qDesc->valLength[0]);

		platform[rowCount][qDesc->valLength[0]] = '\0';
		ims_truncStr(platform[rowCount]);
		rowCount++;
	}

	platform_count = rowCount;

	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		closeConnection(qDesc);
		(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not get platform information.");
		(void) ims_otMsg(msgDesc);
		return (IMS_ERROR);
	}

	/* Let's get sensor */
	rowCount = 0;

	sprintf(qDesc->cmd, 
		"select distinct sensor from dataset_relation where "
		"dataset = '%s'", dataset);

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			closeConnection(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not get sensor information.");
			(void) ims_otMsg(msgDesc);
			return (IMS_ERROR);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY) continue;

		/*
		** there might not be any sensor associated with the dataset
		*/
		if ((IMS_VALUELENGTH (qDesc, 0) == 0) || 
				(IMS_VALUE (qDesc, 0) == (char *)NULL)) continue;

		/*
		** Process returned rows
		*/
		(void) memcpy 
				((char *)sensor[rowCount], qDesc->valAddr[0], qDesc->valLength[0]);

		sensor[rowCount][qDesc->valLength[0]] = '\0';

		ims_truncStr(sensor[rowCount]);
		rowCount++;
	}

	sensor_count = rowCount;

	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		closeConnection(qDesc);
		(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not get platform information.");
		(void) ims_otMsg(msgDesc);
		return (IMS_ERROR);
	}

	/* Let's get format */
	rowCount = 0;

	sprintf(qDesc->cmd, 
		"select distinct format from "
		"dataset_relation t1, dataset_file_policy t2 where "
		"t1.dataset_idx = t2.dataset_idx and "
		"t1.dataset = '%s'", dataset);

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			closeConnection(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not get format information.");
			(void) ims_otMsg(msgDesc);
			return (IMS_ERROR);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY) continue;

		/*
		** Process returned rows
		*/
		(void) memcpy 
			((char *)format[rowCount], qDesc->valAddr[0], qDesc->valLength[0]);

		format[rowCount][qDesc->valLength[0]] = '\0';

		ims_truncStr(format[rowCount]);
		rowCount++;
	}

	format_count = rowCount;

	if (ims_qiResetDesc(qDesc) < IMS_OK)
	{
		closeConnection(qDesc);
		(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not get platform information.");
		(void) ims_otMsg(msgDesc);
		return (IMS_ERROR);
	}

	/* Let's get file types */
	rowCount = 0;

	sprintf(qDesc->cmd, 
		"select distinct extension from "
		"dataset_relation t1, dataset_file_policy t2 where "
		"t1.dataset_idx = t2.dataset_idx and "
		"dataset = '%s'", dataset);

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			closeConnection(qDesc);
			(void) ims_msg(msgDesc, IMS_ERROR, 
					"Could not get file extension information.");
			(void) ims_otMsg(msgDesc);
			return (IMS_ERROR);
		}

		/*
		** If ENDOFQUERY, we want to finish out command and return.
		*/
		if (status == IMS_ENDOFQUERY) continue;

		/*
		** Process returned rows
		*/
		(void) memcpy 
			((char *)fileType[rowCount], qDesc->valAddr[0], qDesc->valLength[0]);

		fileType[rowCount][qDesc->valLength[0]] = '\0';

		ims_truncStr(fileType[rowCount]);
		rowCount++;
	}

	fileType_count = rowCount;

	closeConnection(qDesc);

	return (IMS_OK);
}


/*******************************************************************
** 
** ims_otBuildOptionMenu
**
*******************************************************************/
static Widget ims_otBuildOptionMenu(
	Widget form_w,
	Widget parent,
	int item_count,
	item *menu_items)
{
	Widget w, optionMenu_pane_shell, optionMenu_pane;
	XmString label;
	int i, count;
	char buffer[255];

	optionMenu_pane_shell = NULL;
	optionMenu_pane = NULL;

	/* Creation of PLATFORM option menu */
	optionMenu_pane_shell = XtVaCreatePopupShell ("optionMenu_shell",
			xmMenuShellWidgetClass, form_w,
			XmNwidth, 1,
			XmNheight, 1,
			XmNallowShellResize, TRUE,
			XmNoverrideRedirect, TRUE,
			NULL);
	
	optionMenu_pane = XtVaCreateWidget ("optionMenu_pane",
			xmRowColumnWidgetClass,
			optionMenu_pane_shell,
			XmNrowColumnType, XmMENU_PULLDOWN,
			XmNwidth, 100,
			NULL);


	/* let's build the option menu */
	if (item_count == 0)
	{
		label = XmStringCreateLocalized ("     NONE     ");
		w = XtVaCreateManagedWidget
				((char *)label, xmPushButtonWidgetClass, optionMenu_pane,
				 XmNlabelString, label,
				 NULL);
		XmStringFree (label);
	}
	else
	{
		for (i = 0; i < item_count; i++)
		{
			sprintf (buffer, "     %s    ", menu_items[i]);      
			label = XmStringCreateLocalized (buffer);
			w = XtVaCreateManagedWidget
					((char *)label, xmPushButtonWidgetClass, optionMenu_pane,
					 XmNlabelString, label,
					 NULL);
			XmStringFree (label);
		}
	}

	XtVaSetValues (parent, XmNsubMenuId, optionMenu_pane, NULL);

}

/*******************************************************************
** 
** ims_otDatasetListOkCB
**
*******************************************************************/
static void ims_otDatasetListOkCB(
	Widget w,
	XtPointer client_data,
	XmSelectionBoxCallbackStruct *call_data)
{
	char *dataset, *buffPtr;
	char buffer[20];
	int i;
	Widget menu;
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	IMS_QI_DESC_OBJ *qDesc;
	int which_dlg = (int)client_data;

	/* initialize */
	memset(&platform, 0, sizeof(platform));
	memset(&sensor, 0, sizeof(sensor));
	memset(&format, 0, sizeof(format));
	memset(&fileType, 0, sizeof(fileType));

	/* grep dataset selected and pass to dataset text widget */
	XmStringGetLtoR (call_data->value, XmFONTLIST_DEFAULT_TAG, &dataset);

	if (which_dlg)
	{
		XmTextSetString (ims_ingestWidgets.dataset_w, dataset);
	}
	else
	{
		XmTextSetString (ims_datasetSelectWidgets.dataset_w, dataset);
	}

	if (ims_otGetPolicyInfo(dataset) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get policy information.");
		(void) ims_otMsg(msgDesc);
		return;
	}
	
	/* Build platform option menu */
	if (which_dlg == INGEST_DLG)
	{
		menu = ims_otBuildOptionMenu (ims_ingestWidgets.form_w,
																	ims_ingestWidgets.platform_w,
																	platform_count, platform);

		menu = ims_otBuildOptionMenu (ims_ingestWidgets.form_w,
																	ims_ingestWidgets.sensor_w,
																	sensor_count, sensor);

		menu = ims_otBuildOptionMenu (ims_ingestWidgets.form_w,
																	ims_ingestWidgets.format_w,
																	format_count, format);
		/* update file type text widget */
		buffPtr = buffer; 
		for (i = 0; i < fileType_count; i++)
		{
			sprintf (buffPtr, (i? ",%s" : "%s"), fileType[i]);
			buffPtr = buffPtr + strlen(buffPtr);
		}

		XmTextSetString (ims_ingestWidgets.fileTypes_w, buffer);

	}
	else
	{
		menu = ims_otBuildOptionMenu (ims_datasetSelectWidgets.form_w,
																	ims_datasetSelectWidgets.platform_w,
																	platform_count, platform);

		menu = ims_otBuildOptionMenu (ims_datasetSelectWidgets.form_w,
																	ims_datasetSelectWidgets.sensor_w,
																	sensor_count, sensor);
	}

}

/*******************************************************************
** 
** ims_otIngestOkCB
**
*******************************************************************/
static void ims_otIngestOkCB(
	Widget w,
	XtPointer client_data,
	XtPointer call_data)
{
	int i, status;
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	XmString str;
	Widget temp_w;
	char *extensions[5];
	char *file;


	memset (&extensions, 0, sizeof(extensions));

	/* set up ims_ot_request structure for file ingestion */
	ims_ot_request.requestType = requestType;
	ims_ot_request.username = XmTextGetString (ims_ingestWidgets.userName_w);
	/*
	ims_ot_request.password = XmTextGetString (ims_ingestWidgets.password_w);
	*/

	ims_ot_request.accountId = XmTextGetString (ims_ingestWidgets.accountID_w);

	if (platform_count == 0)
	{
		ims_ot_request.platform = NULL;
	}
	else
	{
		temp_w = XmOptionButtonGadget (ims_ingestWidgets.platform_w);
		XtVaGetValues (temp_w, XmNlabelString, &str, NULL);
		XmStringGetLtoR (str, XmFONTLIST_DEFAULT_TAG, &ims_ot_request.platform);
		ims_truncStr (ims_ot_request.platform);
	}

	if (sensor_count == 0)
	{
		ims_ot_request.sensor = NULL;
	}
	else
	{
		temp_w = XmOptionButtonGadget (ims_ingestWidgets.sensor_w);
		XtVaGetValues (temp_w, XmNlabelString, &str, NULL);
		XmStringGetLtoR (str, XmFONTLIST_DEFAULT_TAG, &ims_ot_request.sensor);
		ims_truncStr (ims_ot_request.sensor);
	}

	if (format_count == 0)
	{
		ims_ot_request.format = NULL;
	}
	else
	{
		temp_w = XmOptionButtonGadget (ims_ingestWidgets.format_w);
		XtVaGetValues (temp_w, XmNlabelString, &str, NULL);
		XmStringGetLtoR (str, XmFONTLIST_DEFAULT_TAG, &ims_ot_request.format);
		ims_truncStr (ims_ot_request.format);
	}

	XmStringFree (str);

	ims_ot_request.dataset = XmTextGetString (ims_ingestWidgets.dataset_w);
	ims_ot_request.name = XmTextGetString (ims_ingestWidgets.name_w);
	ims_ot_request.version = -1;
	ims_ot_request.fileCount = fileType_count;

	for (i = 0; i < fileType_count; i++)
		extensions[i] = fileType[i];

	ims_ot_request.extensions = extensions;
	ims_ot_request.sourceDir = XmTextGetString (ims_ingestWidgets.fileName_w);
	ims_ot_request.localArchiveFlag = 'N';
	ims_ot_request.programName = ims_ot_userSpec.program;
	ims_ot_request.catSrvName = XmTextGetString (ims_ingestWidgets.server_w);
	ims_ot_request.catDbName = XmTextGetString (ims_ingestWidgets.database_w);
	ims_ot_request.msgDesc = msgDesc;

	/* Unmanage ingest window */
	XtUnmanageChild (XtParent(w));

	/* Build PMF file */
	/*
	** 06/19/96 - let's get rid of leading and trailing blanks
	*/
	ims_truncStr(ims_ot_request.sourceDir);
	ims_truncStr(ims_ot_request.name);

	file = XtMalloc(strlen(ims_ot_request.sourceDir) + 1 +
									strlen(ims_ot_request.name) + 1 + 3);
	sprintf(file, "%s/%s.M", ims_ot_request.sourceDir, ims_ot_request.name);

	if (ims_widgetToTree(msgDesc, ims_ot_tree) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not save ODL file '%s'.", 
				file);
		(void) ims_otMsg(msgDesc);

		XtFree(file);	
		return;
	}

	if (ims_buildPMF (msgDesc, ims_ot_tree, file, NULL) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not save ODL file '%s'.", 
				file);
		(void) ims_otMsg(msgDesc);

		XtFree(file);	
		return;
	}

	if (ims_ot_userSpec.saveFile != NULL)
		free(ims_ot_userSpec.saveFile);

	ims_ot_userSpec.saveFile = file;

	/* call ims_archive to handle the request */
	status = ims_archive (&ims_ot_request);

	/* display messages to the message window in main */
	if (status == IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_OK, "File ingestion completed successfully.\n");
	}
	else
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "File ingestion failed.\n");
	}

	(void) ims_otMsg(msgDesc);

}

/*******************************************************************
** 
** ims_otIngestCancelCB
**
*******************************************************************/
static void ims_otIngestCancelCB(
	Widget w,
	XtPointer client_data,
	XtPointer call_data)
{
	XtUnmanageChild (XtParent(w));
}


/*******************************************************************
** 
** ims_otRequestTypeCB
**
*******************************************************************/
static void ims_otRequestTypeCB(
	Widget w,
	XtPointer client_data,
	XtPointer call_data)
{
	int which = (int)client_data;

	XmToggleButtonCallbackStruct *state =
		(XmToggleButtonCallbackStruct *) call_data;
	
	if ((which) && (state->set))
	{
		requestType = IMS_REPLACE;
	}
	else
	{
		requestType = IMS_ADD;
	}

}

/*******************************************************************
** 
** ims_otDatasetSelect
**
*******************************************************************/
void ims_otDatasetSelect(
	Widget main_w)
{
	Widget datasetDlg, title_w, label_w, separator_w, dataset_list_w; 
	Widget ok_w, cancel_w;
	Widget platformOM_pane_shell, platformOM_pane;
	Widget sensorOM_pane_shell, sensorOM_pane;
	Widget formatOM_pane_shell, formatOM_pane;
	XmString labelStr, add, replace;
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	char buffer[20];
	char *buffPtr;
	Widget menu;
	int i;

	/* initialization */

	platform_count = sensor_count = 0;

	/*
	** Build dataset, platform, sensor selection dialog 
	*/
	datasetDlg = XtVaCreatePopupShell ("datasetDlg",
								xmDialogShellWidgetClass, main_w,
								XmNtitle, "Dataset Selection Dialog",
								XmNdeleteResponse, XmDESTROY,
								XmNwidth, 400,
								XmNheight, 250,
								XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
								NULL);

	ims_datasetSelectWidgets.form_w = XtVaCreateWidget (
							  "form", xmFormWidgetClass, datasetDlg,
								XmNwidth, 400,
								XmNheight, 250,
								XmNautoUnmanage, False,
								NULL);

	labelStr = XmStringCreateLocalized ("DATASET SELECTION:");

	/* Creation of label DATASET SELECTION */
	title_w = XtVaCreateManagedWidget( "titleLBL",
			xmLabelWidgetClass,
			ims_datasetSelectWidgets.form_w,  
			XmNlabelString, labelStr,
			XmNwidth, 50,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 100,
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 100,
			XmNtopAttachment, XmATTACH_FORM,
			XmNtopOffset, 10,
			NULL );

	XmStringFree(labelStr);

	labelStr = XmStringCreateLocalized ("DATASET:");

	/* Creation of label DATASET */
	label_w = XtVaCreateManagedWidget( "datasetLBL",
			xmLabelWidgetClass,
			ims_datasetSelectWidgets.form_w,  
			XmNlabelString, labelStr,
			XmNwidth, 70,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, title_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);

	/* Creation of text widget DATASET */
	ims_datasetSelectWidgets.dataset_w = XtVaCreateManagedWidget( "datasetTXT",
			xmTextWidgetClass,
			ims_datasetSelectWidgets.form_w,  
			XmNwidth, 164,
			XmNeditable, FALSE,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, title_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			NULL );

	
	/* Creation of pushbutton Dataset List... */
	labelStr = XmStringCreateLocalized ("List...");

	dataset_list_w = XtVaCreateManagedWidget( "datasetListPB",
			xmPushButtonWidgetClass,
			ims_datasetSelectWidgets.form_w,  
			XmNlabelString, labelStr,
			XmNheight, 30,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, title_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, ims_datasetSelectWidgets.dataset_w,
			XmNleftOffset, 10,
			NULL );

	XtAddCallback(dataset_list_w, XmNactivateCallback,
								ims_otDatasetListCB, (XtPointer)DATASET_DLG);

	XmStringFree(labelStr);


	labelStr = XmStringCreateLocalized ("PLATFORM:");

	/* Creation of label PLATFORM */
	label_w = XtVaCreateManagedWidget( "platformLBL",
			xmLabelWidgetClass,
			ims_datasetSelectWidgets.form_w,  
			XmNwidth, 70,
			XmNlabelString, labelStr,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_datasetSelectWidgets.dataset_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);


	/* Creation of PLATFORM option menu */
	ims_datasetSelectWidgets.platform_w = XtVaCreateManagedWidget ("platformOM",
			xmRowColumnWidgetClass,
			ims_datasetSelectWidgets.form_w,
			XmNrowColumnType, XmMENU_OPTION,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_datasetSelectWidgets.dataset_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			NULL);

	/* Build platform option menu items */
	menu = ims_otBuildOptionMenu (ims_datasetSelectWidgets.form_w, 
																ims_datasetSelectWidgets.platform_w, 
																platform_count, platform);

	labelStr = XmStringCreateLocalized ("SENSOR:");

	/* Creation of label SENSOR */
	label_w = XtVaCreateManagedWidget( "sensorLBL",
			xmLabelWidgetClass,
			ims_datasetSelectWidgets.form_w,  
			XmNwidth, 70,
			XmNlabelString, labelStr,
			XmNalignment, XmALIGNMENT_BEGINNING, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_datasetSelectWidgets.platform_w,
			XmNtopOffset, 20,
			NULL );

	XmStringFree(labelStr);

	/* Creation of SENSOR option menu */
	ims_datasetSelectWidgets.sensor_w = XtVaCreateManagedWidget ("sensorOM",
			xmRowColumnWidgetClass,
			ims_datasetSelectWidgets.form_w,
			XmNrowColumnType, XmMENU_OPTION,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget,	ims_datasetSelectWidgets.platform_w,
			XmNtopOffset, 15,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, label_w,
			XmNleftOffset, 20,
			NULL);

	/* Build sensor option menu items */
	menu = ims_otBuildOptionMenu (ims_datasetSelectWidgets.form_w, 
																ims_datasetSelectWidgets.sensor_w,
																sensor_count, sensor);


	/* Creation of separator widget */
	separator_w = XtVaCreateManagedWidget( "separator2",
			xmSeparatorWidgetClass,
			ims_datasetSelectWidgets.form_w,  
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, ims_datasetSelectWidgets.sensor_w,
			XmNtopOffset, 30,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			NULL );

	/* Creation of pushbutton INGEST */
	labelStr = XmStringCreateLocalized ("OK");

	ok_w = XtVaCreateManagedWidget( "okPB",
			xmPushButtonWidgetClass,
			ims_datasetSelectWidgets.form_w,  
			XmNlabelString, labelStr,
			XmNheight, 30,
			XmNwidth, 100,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, separator_w,
			XmNtopOffset, 10,
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 40,
			NULL );

	XtAddCallback (ok_w, XmNactivateCallback, ims_otDatasetSelectOkCB, NULL);

	XmStringFree(labelStr);

	/* Creation of pushbutton CANCEL */
	labelStr = XmStringCreateLocalized ("CANCEL");

	cancel_w = XtVaCreateManagedWidget( "cancelPB",
			xmPushButtonWidgetClass,
			ims_datasetSelectWidgets.form_w,  
			XmNlabelString, labelStr,
			XmNheight, 30,
			XmNwidth, 100,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, separator_w,
			XmNtopOffset, 10,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, ok_w,
			XmNleftOffset, 120,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 40,
			NULL );

	XmStringFree(labelStr);

	XtAddCallback (cancel_w, XmNactivateCallback, ims_otIngestCancelCB, NULL);

	XtManageChild(ims_datasetSelectWidgets.form_w);
	XtPopup(datasetDlg, XtGrabNone);

}

/*******************************************************************
** 
** ims_otDatasetSelectOkCB
**
*******************************************************************/
static void ims_otDatasetSelectOkCB(
	Widget w,
	XtPointer client_data,
	XtPointer call_data)
{
	IMS_ODL_TREE *odl_tree;
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	Widget temp_w;
	XmString str;

	odl_tree = NULL;

	ims_ot_userSpec.saveFile = NULL;

	ims_ot_request.dataset = XmTextGetString (ims_datasetSelectWidgets.dataset_w);

	if (platform_count == 0)
	{
		ims_ot_request.platform = NULL;
	}
	else
	{
		temp_w = XmOptionButtonGadget (ims_datasetSelectWidgets.platform_w);
		XtVaGetValues (temp_w, XmNlabelString, &str, NULL);
		XmStringGetLtoR (str, XmFONTLIST_DEFAULT_TAG, &ims_ot_request.platform);
		ims_truncStr (ims_ot_request.platform);
	}

	if (sensor_count == 0)
	{
		ims_ot_request.sensor = NULL;
	}
	else
	{
		temp_w = XmOptionButtonGadget (ims_datasetSelectWidgets.sensor_w);
		XtVaGetValues (temp_w, XmNlabelString, &str, NULL);
		XmStringGetLtoR (str, XmFONTLIST_DEFAULT_TAG, &ims_ot_request.sensor);
		ims_truncStr (ims_ot_request.sensor);
	}

	if (ims_ot_tree != NULL)
	{
		/*
		** If there is an existing window then remove it and free the ODL Tree.
		*/
		XtUnmapWidget(ims_otWidgets.work_w);
		(void) ims_deleteWidgetList(msgDesc);
		/*
		XtUnrealizeWidget(ims_otWidgets.main_w);
		*/
	}

	/*
	** Build keyword list
	*/
	(void) ims_otBldCmnHdr(msgDesc, ims_otWidgets.work_w, &odl_tree);

	(void) ims_otBldKwList(msgDesc, odl_tree, 
		ims_ot_request.platform, ims_ot_request.sensor, ims_ot_request.dataset);

	(void) ims_otShowObject(msgDesc, ims_otWidgets.work_w, odl_tree, 0);
	/*
	XtRealizeWidget(ims_otWidgets.main_w);
	*/
	XtMapWidget(ims_otWidgets.work_w);

	ims_ot_tree = odl_tree;

	XtUnmanageChild (XtParent(w));
}

