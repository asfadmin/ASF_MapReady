static char *sccs = "@(#)ims_otWidgets.c	1.10 11/24/97";

/*****************************************************************************
**
**
** File:    ims_otWidgets.c
**
** Function: 
**			
**
** Author: Dan Crichton, Jennifer Ting
**
** Date:    3/13/96
**
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
#include <Xm/FileSB.h>
#include <Xm/DialogS.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>



#include <ims_dbms.h>
#include <ims_query.h> 
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <ims_cmd.h>
#include <syslog.h>

#include <odldef.h>
#include <odlinter.h>
#include <ims_odl.h>

#include <ims_keyword.h>
#include <ims_archive.h>
#include <ims_pmfTool.h>

static void ims_otOptionCB(Widget, XtPointer, XtPointer);
static void ims_otKeywordCB (Widget, XtPointer, XtPointer);
static void ims_otFileCB(Widget, XtPointer, XtPointer);
static void ims_otOpenCB(Widget, XtPointer, XtPointer);
static void ims_otSaveCB(Widget, XtPointer, XtPointer);
static void ims_otKeywordMenuCB(Widget, XtPointer, XtPointer);
int ims_widgetToTree(IMS_MSG_STRUCT *, IMS_ODL_TREE *);
static void ims_otGainFocusCB(Widget, XtPointer, XtPointer);
void ims_otKeywordDialog(Widget, IMS_OT_KEYWORD *);
static void ims_otKeywordOkCB(Widget, XtPointer, XtPointer);
static void ims_otKeywordSelectCB(Widget, XtPointer, XtPointer);
void ims_otClearKeywordCB (Widget, XtPointer, XtPointer);

/*******************************************************************
** 
** ims_otBldMenuBar
**
*******************************************************************/
int ims_otBldMenuBar(
	IMS_MSG_STRUCT *msgDesc,
	Widget main_w)
{
	Widget menubar_w, file_w, option_w, keyword_w;
	XmString file_s, option_s, keyword_s, help_s;
	XmString new_s, open_s, save_s, save_as_s, print_s, quit_s;
	XmString ingest_s, clear_msg_s;
	XmString clear_s, info_s;
	
	/*
	** First create the menu bar widget.
	*/

	file_s = XmStringCreateSimple("File");
	option_s = XmStringCreateSimple("Option");
	keyword_s = XmStringCreateSimple("Keyword");
	help_s = XmStringCreateSimple("Help");

	menubar_w = XmVaCreateSimpleMenuBar(main_w, "menubar",
		XmVaCASCADEBUTTON, file_s, 'F',
		XmVaCASCADEBUTTON, option_s, 'O',
		XmVaCASCADEBUTTON, keyword_s, 'K',
		XmVaCASCADEBUTTON, help_s, 'H',
		NULL);

	ims_otWidgets.menubar_w = menubar_w; 

	XmStringFree(file_s);
	XmStringFree(option_s);
	XmStringFree(keyword_s);
	XmStringFree(help_s);


	/*
	** Identify Help Button.
	*/

	XtVaSetValues(menubar_w, XmNmenuHelpWidget,
		XtNameToWidget(menubar_w, "button_3"), NULL);

	/*
	** Set up file pulldown.
	*/

	new_s = XmStringCreateSimple("New");
	open_s = XmStringCreateSimple("Open ...");
	save_s = XmStringCreateSimple("Save");
	save_as_s = XmStringCreateSimple("Save As ...");
	print_s = XmStringCreateSimple("Print");
	quit_s = XmStringCreateSimple("Quit");

	file_w = XmVaCreateSimplePulldownMenu(menubar_w,
		"file", 0, ims_otFileCB,
		XmVaPUSHBUTTON, new_s, 'N', NULL, NULL,
		XmVaPUSHBUTTON, open_s, 'O', NULL, NULL,
		XmVaPUSHBUTTON, save_s, 'S', NULL, NULL,
		XmVaPUSHBUTTON, save_as_s, 'A', NULL, NULL,
		XmVaSEPARATOR,
		XmVaPUSHBUTTON, print_s, 'P', NULL, NULL,
		XmVaSEPARATOR,
		XmVaPUSHBUTTON, quit_s, 'Q', NULL, NULL,
		NULL);

	XmStringFree(new_s);
	XmStringFree(open_s);
	XmStringFree(save_s);
	XmStringFree(save_as_s);
	XmStringFree(print_s);
	XmStringFree(quit_s);

	/*
	** Set up options pulldown.
	*/

	ingest_s = XmStringCreateSimple("Ingest");
	clear_msg_s = XmStringCreateSimple("Clear Messages");

	option_w = XmVaCreateSimplePulldownMenu(menubar_w,
		"option", 1, ims_otOptionCB,
		XmVaPUSHBUTTON, ingest_s, 'I', NULL, NULL,
		XmVaPUSHBUTTON, clear_msg_s, 'L', NULL, NULL,
		NULL);

	XmStringFree(ingest_s);
	XmStringFree(clear_msg_s);

	/*
	** Setup keyword menu.
	*/

	clear_s = XmStringCreateSimple("Clear");
	info_s = XmStringCreateSimple("Info");

	keyword_w = XmVaCreateSimplePulldownMenu(menubar_w,
		"keyword", 2, ims_otKeywordMenuCB,
		XmVaPUSHBUTTON, clear_s, 'C', NULL, NULL,
		XmVaPUSHBUTTON, info_s, 'I', NULL, NULL,
		NULL);

	XmStringFree(clear_s);
	XmStringFree(info_s);

	XtManageChild(menubar_w);
	XtVaSetValues(main_w, XmNmenuBar, menubar_w, NULL);
	return(IMS_OK);
}


/*******************************************************************
** 
** ims_otBldMsgWindow
**
*******************************************************************/
int ims_otBldMsgWindow(
	IMS_MSG_STRUCT *msgDesc,
	Widget main_w)
{
	Widget list_w, widget;

	list_w = XmCreateScrolledList(main_w, "msg_list", NULL, 0);

	XtVaSetValues(list_w, XmNscrollBarDisplayPolicy, XmSTATIC,  NULL);

	XtManageChild(list_w);

	XtVaSetValues(main_w, XmNmessageWindow, XtParent(list_w), NULL);
	XtVaSetValues(XmMainWindowSep3(main_w), XmNshowSeparator, True, NULL);



	ims_otWidgets.msg_w = list_w; 

	return(IMS_OK);
}

/*******************************************************************
** 
** ims_otBldMainWin
**
*******************************************************************/
int ims_otBldMainWin(
	IMS_MSG_STRUCT *msgDesc,
	Widget main_w)
{

	Widget row_w;

	XtVaSetValues(XmMainWindowSep1(main_w), XmNshowSeparator, True, NULL);
	XtVaSetValues(main_w, XmNscrollBarDisplayPolicy, XmSTATIC, NULL);

	row_w = XtVaCreateManagedWidget("keywords", xmRowColumnWidgetClass, main_w,
		NULL);

	XtVaSetValues(main_w, XmNworkWindow, row_w,  NULL);

/*
	(void) ims_otBldCmnHdr(msgDesc, row_w);
*/

	ims_otWidgets.work_w = row_w; 
		
	return(IMS_OK);
}

/*******************************************************************
** 
** ims_otShowObject
**
*******************************************************************/
int ims_otShowObject(
	IMS_MSG_STRUCT *msgDesc,
	Widget w,
	IMS_ODL_TREE *tree,
	int indent)
{
	Widget form_w, text_w;
	char object_id[IMS_COL80_LEN+1];
	char stmt[IMS_COL255_LEN+1];
	int column;
	IMS_OT_KEYWORD *data;
	int i, len;
	char *objectClass;

	if (tree == NULL)
	{
		return(IMS_OK);
	}

	data = (IMS_OT_KEYWORD *) tree->data;



	if (tree->type == IMS_OBJECT)
	{
		/*
		** Create object node.
		*/
		
		form_w = XtVaCreateWidget("object_w", xmFormWidgetClass, w, 
			XmNfractionBase, 15  , NULL);


		(void) ims_addDynamicWidget(msgDesc, form_w);
		sprintf(object_id, "OBJECT = %s", tree->node_name);

		(void) ims_addDynamicWidget(msgDesc, 
			XtVaCreateManagedWidget(object_id,
			xmLabelGadgetClass, form_w,
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNtopAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 0 + indent,
			NULL));

		XtManageChild(form_w);

		/*
		** All keywords, and child objects 
		*/
		
		(void) ims_otShowObject(msgDesc, w, tree->children, indent + 1);

		/*
		** End of Object 
		*/

		form_w = XtVaCreateWidget("object_w", xmFormWidgetClass, w, NULL);
		(void) ims_addDynamicWidget(msgDesc, form_w);



		sprintf(object_id, "END OBJECT = %s", tree->node_name);

		(void) ims_addDynamicWidget(msgDesc, 
			XtVaCreateManagedWidget(object_id, xmLabelGadgetClass, form_w,
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNtopAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 0 + indent,
			NULL));

		XtManageChild(form_w);

		/*
		** Next object of same level.
		*/

		(void) ims_otShowObject(msgDesc, w, tree->next, indent);
	}
	else
	{

		form_w = XtVaCreateWidget("keyword_w", xmFormWidgetClass, w, 
			XmNfractionBase, 15, NULL);


		(void) ims_addDynamicWidget(msgDesc, form_w);
		sprintf(object_id, "%s", tree->node_name);


		text_w = XtVaCreateManagedWidget(object_id, xmLabelGadgetClass, form_w,
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNtopAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 1 + indent,
			NULL);
		
		(void) ims_addDynamicWidget(msgDesc, text_w);


		switch (tree->value.type)
		{
			case TV_INTEGER:
				if (tree->value.length <= 0)
					break;

				ODLFormatInteger(stmt, &(tree->value));
				break;

			case TV_REAL:
				if (tree->value.length <= 0)
					break;

				ODLFormatReal(stmt, &(tree->value));
				break;
			
			case TV_DATE_TIME:
				if (tree->value.length <= 0)
					break;

				ODLFormatDateTime(stmt, &(tree->value));
				break;

			case TV_STRING:
				if (tree->value.length <= 0)
					break;

				column = 0;
					ODLFormatString(stmt,  &(tree->value),
					&column, 0, 132, FALSE, TRUE);
 
				/*
				** Get rid of quotes around string.
				*/
									  
				stmt[strlen(stmt) - 1] = '\0';
				len = strlen(stmt)-1;
				for (i = 0; i < len; i++)
				{
					stmt[i] = stmt[i+1];
				}
				stmt[i] = '\0'; 
				break;

			case TV_SYMBOL:
				if (tree->value.length <= 0)
					break;
				ODLFormatSymbol(stmt, &(tree->value));

				/*
				** Get rid of quotes around string.
				*/
									  
				stmt[strlen(stmt) - 1] = '\0';
				len = strlen(stmt)-1;
				for (i = 0; i < len; i++)
				{
					stmt[i] = stmt[i+1];
				}
				stmt[i] = '\0'; 

				break;
		}

		if ((data->significance != IMS_OPTIONAL_INDEXED) &&
			(data->significance != IMS_OPTIONAL_NOT_INDEXED))
			objectClass = "keyword_mand";
		else
			objectClass = "keyword_opt";


		if ((data->max_len > 0) && (data->max_len < 50))
		{

			data->widget = XtVaCreateManagedWidget(objectClass, xmTextWidgetClass, 
				form_w,
				XmNalignment, XmALIGNMENT_BEGINNING,
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment, XmATTACH_POSITION,
				XmNleftPosition, 5 + indent,
				XmNvalue, stmt,
				XmNmaxLength, data->max_len,
				XmNcolumns, data->max_len,
				NULL);
			(void) ims_addDynamicWidget(msgDesc, data->widget);
		}
		else if (data->max_len >= 50)
		{

			data->widget = XtVaCreateManagedWidget(objectClass, xmTextWidgetClass, 
				form_w,
				XmNalignment, XmALIGNMENT_BEGINNING,
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment, XmATTACH_POSITION,
				XmNleftPosition, 5 + indent,
				XmNvalue, stmt,
				XmNmaxLength, data->max_len,
				XmNcolumns, 50,
				NULL);
			(void) ims_addDynamicWidget(msgDesc, data->widget);
		}
		else if (data->data_type == 9) /* date time, length is NULL in DB */
		{

			data->widget = XtVaCreateManagedWidget(objectClass, xmTextWidgetClass, 
				form_w,
				XmNalignment, XmALIGNMENT_BEGINNING,
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment, XmATTACH_POSITION,
				XmNleftPosition, 5 + indent,
				XmNvalue, stmt,
				XmNmaxLength, 21,
				XmNcolumns, 21,
				NULL);
			(void) ims_addDynamicWidget(msgDesc, data->widget);
		}
		else
		{

			data->widget = XtVaCreateManagedWidget(objectClass, xmTextWidgetClass, 
				form_w,
				XmNalignment, XmALIGNMENT_BEGINNING,
				XmNtopAttachment, XmATTACH_FORM,
				XmNbottomAttachment, XmATTACH_FORM,
				XmNleftAttachment, XmATTACH_POSITION,
				XmNleftPosition, 5 + indent,
				XmNvalue, stmt,
				NULL);
			(void) ims_addDynamicWidget(msgDesc, data->widget);
		}


		XtAddCallback(data->widget, XmNactivateCallback,
			ims_otKeywordCB, (void *) tree);

        XtAddCallback(data->widget, XmNfocusCallback, ims_otGainFocusCB, 
				(void *) tree);

		XtManageChild(form_w);


		(void) ims_otShowObject(msgDesc, w, tree->next, indent);
	}
}

/*******************************************************************
** 
** ims_otOptionCB
**
*******************************************************************/
static void ims_otOptionCB(
	Widget w,
	XtPointer client_data,
	XtPointer call_data)
{
	Widget list_w;
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	int item = (int) client_data;


	if (item)
	{
		XmListDeleteAllItems (ims_otWidgets.msg_w);
	}
	else
	{
		(void) ims_otIngestCB (ims_otWidgets.main_w);
	}

}

/*******************************************************************
** 
** ims_otKeywordCB
**
*******************************************************************/
static void ims_otKeywordCB(
	Widget w,
	XtPointer client_data,
	XtPointer call_data)
{
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	IMS_ODL_TREE *tree = (IMS_ODL_TREE *) client_data;

	if (tree->next != NULL)
		XmProcessTraversal(w, XmTRAVERSE_NEXT_TAB_GROUP);
}

/*******************************************************************
** 
** ims_otGainFocusCB
**
*******************************************************************/
static void ims_otGainFocusCB(
	Widget w,
	XtPointer client_data,
	XtPointer call_data)
{
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;

	/*
	** Set global pointer.
	*/

	ims_otFocusTree = (IMS_ODL_TREE *) client_data;
}

/*******************************************************************
** 
** ims_otFileCB
**
*******************************************************************/
static void ims_otFileCB(
	Widget w,
	XtPointer client_data,
	XtPointer call_data)
{
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	int item = (int) client_data;

	/*
	ims_msg(msgDesc, IMS_INFO, "File CB Item = %d", item);
	ims_otMsg(msgDesc);
	*/
	switch(item)
	{
		
		case 0:
			ims_otDatasetSelect (ims_otWidgets.main_w);
			break;
		case 1:			

			ims_otWidgets.open_w = (Widget) XmCreateFileSelectionDialog(
				ims_otWidgets.main_w,
				"File_Selection", NULL, (Cardinal) 0);
		
			XtAddCallback(ims_otWidgets.open_w, XmNokCallback,
				(XtCallbackProc) ims_otOpenCB, NULL);
			XtAddCallback(ims_otWidgets.open_w, XmNcancelCallback,
				(XtCallbackProc) XtUnmanageChild,
				NULL);
			XtManageChild(ims_otWidgets.open_w);
			XtPopup(XtParent(ims_otWidgets.open_w), XtGrabExclusive);
				
			break;
		case 2:
			if (ims_ot_userSpec.saveFile != NULL)
			{
				if (ims_widgetToTree(msgDesc, ims_ot_tree) < IMS_OK)
				{
					ims_msg(msgDesc, IMS_ERROR,
						"Could not save file do to prior errors.");
					break;
				}

				if (ims_buildPMF(msgDesc, ims_ot_tree, 
					ims_ot_userSpec.saveFile, NULL) < IMS_OK)
				{
					ims_msg(msgDesc, IMS_ERROR,
						"Could not save file.");
				}
				break;
			}

			/* Fall through */

		case 3:
			ims_otWidgets.save_w = (Widget) XmCreateFileSelectionDialog(
					ims_otWidgets.main_w,
					"Save As...", NULL, (Cardinal) 0);
			
			XtAddCallback(ims_otWidgets.save_w, XmNokCallback,
					(XtCallbackProc)ims_otSaveCB, NULL);
			XtAddCallback(ims_otWidgets.save_w, XmNcancelCallback,
					(XtCallbackProc)XtUnmanageChild,
					NULL);
			XtManageChild(ims_otWidgets.save_w);
			XtPopup(XtParent(ims_otWidgets.save_w), XtGrabExclusive);

			break;

		
		case 5:
			exit(0);
	
	}
	


}

/*******************************************************************
** 
** ims_otOpenCB
**
*******************************************************************/
static void ims_otOpenCB(
	Widget w,
	XtPointer client_data,
	XtPointer call_data)
{
	XmFileSelectionBoxCallbackStruct *cbs 
		= (XmFileSelectionBoxCallbackStruct *) call_data;
	char *file;
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	IMS_ODL_TREE *tree;
	
		
	if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &file))
		return;

	if (*file != '/')
	{
		char *dir, *newfile;

		if (XmStringGetLtoR(cbs->dir, XmFONTLIST_DEFAULT_TAG, &dir))
		{

			newfile = XtMalloc(strlen(dir) + 1 + strlen(file) + 1);
			sprintf(newfile, "%s/%s", dir, file);
			XtFree(file);
			XtFree(dir);
			file = newfile;
		}
	}


	if (ims_ODLFileToTree (msgDesc, file, &tree) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not load ODL file '%s'.", 
				file);
		(void) ims_otMsg(msgDesc);
		XtFree(file);	
		XtUnmanageChild(w);
		memset(&(ims_otWidgets.open_w), 0, sizeof(Widget));
		return;
	}
	ims_otAddODLData(tree);

	/*
	** If there is an existing window then remove it and free the ODL Tree.
	*/

	if (ims_ot_tree != NULL)
	{
		XtUnmapWidget(ims_otWidgets.work_w);
		(void) ims_deleteWidgetList(msgDesc);
	/*
		XtUnrealizeWidget(ims_otWidgets.main_w);
	*/
		(void) ims_otShowObject(msgDesc, ims_otWidgets.work_w, tree, 0);
	/*
		XtRealizeWidget(ims_otWidgets.main_w);
	*/
		XtMapWidget(ims_otWidgets.work_w);
	}		
	else
	{
		(void) ims_otShowObject(msgDesc, ims_otWidgets.work_w, tree, 0);

	}
	ims_ot_tree = tree;


	XtFree(file);	
	XtUnmanageChild(w);
	memset(&(ims_otWidgets.open_w), 0, sizeof(Widget));
}


/*******************************************************************
** 
** ims_addDynamicWidget
**
*******************************************************************/
int ims_addDynamicWidget(
	IMS_MSG_STRUCT *msgDesc,
	Widget w)
{
	IMS_OT_WIDGET_LIST *ptr;

	ptr = ims_otWidgets.tailList;

	if (ptr != NULL)
	{
		ptr->next = malloc(sizeof(IMS_OT_WIDGET_LIST));
		ptr = ptr->next; 
	}
	else
	{

		ptr = malloc(sizeof(IMS_OT_WIDGET_LIST));
	}

	ptr->widget = w;
	ptr->next = NULL;

	ims_otWidgets.tailList = ptr;
	if (ims_otWidgets.headList == NULL)
		ims_otWidgets.headList = ptr;

	return(IMS_OK);
}

/*******************************************************************
** 
** ims_deleteWidgetList
**
*******************************************************************/
int ims_deleteWidgetList(
	IMS_MSG_STRUCT *msgDesc)
{
	IMS_OT_WIDGET_LIST *ptr;

	ptr = ims_otWidgets.headList;

	while (ptr != NULL)
	{

		XtUnmanageChild(ptr->widget);
		XtUnrealizeWidget(ptr->widget);
		ims_otWidgets.headList = ptr->next;
		free(ptr);
		ptr = ims_otWidgets.headList;
	}

	ims_otWidgets.headList = ims_otWidgets.tailList = NULL;
	return(IMS_OK);
}

/*******************************************************************
** 
** ims_otWidgetToTree
**
** Write changes from the widget text fields to the tree.
**
*******************************************************************/
int ims_widgetToTree(
	IMS_MSG_STRUCT *msgDesc,
	IMS_ODL_TREE *tree)
{
	IMS_OT_KEYWORD *data;
	char *value, *sp;
	IMS_ODL_TREE *ptr;
	IMS_NUMERIC_DATE dateStruct;
	char buffer[24];
	int status;

	if (tree == NULL)
		return(IMS_OK);

	if (tree->type == IMS_OBJECT)
	{
		if (ims_widgetToTree(msgDesc, tree->children) < IMS_OK)
		  	return(IMS_ERROR); 
		if (ims_widgetToTree(msgDesc, tree->next) < IMS_OK)
			return(IMS_ERROR);
	}
	else
	{
		/*
		** Extract keyword value.
		*/

		data =  (IMS_OT_KEYWORD *) tree->data;

		if (value = XmTextGetString(data->widget))
		{
			ptr = tree;
			

			/*
			** Setup value data
			*/

			if ((int)strlen (value) > 0)
			{
				switch(ptr->value.type)
				{
					case TV_INTEGER:
						ptr->value = ODLConvertInteger(value, strlen(value));
						break;

					case TV_REAL:
						ptr->value = ODLConvertReal(value, strlen(value));
						break;
										
					case TV_SYMBOL:
						ptr->value = ODLConvertSymbol(value, strlen(value), 2);
						break;

					case TV_STRING:
						ptr->value = ODLConvertString(value, strlen(value));
						break;

					case TV_DATE:
						ptr->value = ODLConvertDate(value, strlen(value));
						break;

					case TV_TIME:
						ptr->value = ODLConvertTime(value, strlen(value));
						break;

					case TV_DATE_TIME:
						/*
						** Make sure that the value is a valid date.
						*/

						if (ims_timeToNumericDate(msgDesc,
							value, &dateStruct) < IMS_OK)
						{
							(void) ims_msg(msgDesc,
								IMS_ERROR,
								"Date '%s' is not valid.", value);
							(void) ims_otMsg(msgDesc); 
							return(IMS_ERROR);
						}

						ims_numericDateToIMSA(&dateStruct, buffer);


						/*
						** ODL library modifies read-only memory addresses.
						** This can bus error therefore we will setup the
						** value data structure ourselves.
						*/

						ptr->value.length = strlen(buffer);
						ptr->value.valid = 1;
						ptr->value.format = 0;
						ptr->value.precision = 0;
						ODLExtractDate(buffer, &(ptr->value));
						sp = strchr(buffer, 'T') + 1;
						ODLExtractTime(sp, &(ptr->value));
						break;
				}
			}
			else 
			{
				ptr->value.valid = 0;
			}

			XtFree(value);
		}
		if (ims_widgetToTree(msgDesc, tree->children) < IMS_OK)
			return(IMS_ERROR);
		if (ims_widgetToTree(msgDesc, tree->next) < IMS_OK)
			return(IMS_ERROR);
	}

	return(IMS_OK);

}


/*******************************************************************
** 
** ims_otSaveCB
**
*******************************************************************/
static void ims_otSaveCB(
	Widget w,
	XtPointer client_data,
	XtPointer call_data)
{
	XmFileSelectionBoxCallbackStruct *cbs 
		= (XmFileSelectionBoxCallbackStruct *) call_data;
	char *file;
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	
		
	if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &file))
		return;

	if (*file != '/')
	{
		char *dir, *newfile;

		if (XmStringGetLtoR(cbs->dir, XmFONTLIST_DEFAULT_TAG, &dir))
		{

			newfile = XtMalloc(strlen(dir) + 1 + strlen(file) + 1);
			sprintf(newfile, "%s/%s", dir, file);
			XtFree(file);
			XtFree(dir);
			file = newfile;
		}
	}

	if (ims_widgetToTree(msgDesc, ims_ot_tree) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not save ODL file '%s'.", 
				file);
		(void) ims_otMsg(msgDesc);
		XtFree(file);	
		XtUnmanageChild(w);
		memset(&(ims_otWidgets.save_w), 0, sizeof(Widget));
		return;
	}

	if (ims_buildPMF (msgDesc, ims_ot_tree, file, NULL) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, "Could not save ODL file '%s'.", 
				file);
		(void) ims_otMsg(msgDesc);
		XtFree(file);	
		XtUnmanageChild(w);
		memset(&(ims_otWidgets.save_w), 0, sizeof(Widget));
		return;
	}

	if (ims_ot_userSpec.saveFile != NULL)
		free(ims_ot_userSpec.saveFile);

	ims_ot_userSpec.saveFile = file;


	memset(&(ims_otWidgets.open_w), 0, sizeof(Widget));
	(void) ims_msg(msgDesc, IMS_INFO, "File '%s' saved.", 
				file);
	(void) ims_otMsg(msgDesc);
	XtUnmanageChild(w);
}


/*******************************************************************
** 
** ims_otKeywordMenuCB
**
*******************************************************************/
static void ims_otKeywordMenuCB(
	Widget w, 
	XtPointer client_data,
	XtPointer call_data)
{
	IMS_OT_WIDGET_LIST *ptr;
	int item = (int) client_data;
	IMS_OT_KEYWORD *data;
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	Widget dialog;
	XmString text, ok_str, cancel_str;


	/*
	ims_msg(msgDesc, IMS_INFO, "File CB Item = %d", item);
	ims_otMsg(msgDesc);
	*/
	switch(item)
	{
		case 0:

			
			dialog = XmCreateWarningDialog(ims_otWidgets.main_w, 
					"warning", NULL, 0);

			text = XmStringCreateLtoR(
				"Do you really want to clear all fields?", 
				XmFONTLIST_DEFAULT_TAG);

			ok_str = XmStringCreateLocalized("Yes");
			cancel_str = XmStringCreateLocalized("No");
			XtVaSetValues(dialog, XmNmessageString, text,
				XmNokLabelString, ok_str,
				XmNcancelLabelString, cancel_str,
				XmNdefaultButtonType, XmDIALOG_CANCEL_BUTTON,
				NULL);

			XtAddCallback(dialog, XmNokCallback, ims_otClearKeywordCB,
				NULL);

			XtSetSensitive(
				XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON),
				False);
			
			XmStringFree(text);
			XmStringFree(ok_str);
			XmStringFree(cancel_str);

			XtManageChild(dialog);
			XtPopup(XtParent(dialog), XtGrabNone);




			break;

		case 1:

			if (ims_otFocusTree == NULL)
				break;


			data = (IMS_OT_KEYWORD *) ims_otFocusTree->data;

			if (data == NULL)
				break;

			if (data->keyword_idx == -1)
			{
				ims_otMsg_box(ims_otWidgets.main_w, 
					"Keyword Data Unavailable.");
				break;

			}

			ims_otKeywordDialog(ims_otWidgets.main_w, data);


			break;

	}

}

/*******************************************************************
** 
** ims_otClearKeywordCB
**
*******************************************************************/
void ims_otClearKeywordCB (
	Widget w, 
	XtPointer client_data,
	XtPointer call_data)
{
	IMS_OT_WIDGET_LIST *ptr;

	ptr = ims_otWidgets.headList;
	
	while (ptr != NULL)
	{
		if (XtIsSubclass(ptr->widget, xmTextWidgetClass))
		{
			XmTextSetString (ptr->widget, "");
		}
		ptr = ptr->next;
	}

	XtPopdown (XtParent(w));
}


/*******************************************************************
** 
** ims_otKeywordDialog
**
*******************************************************************/
void ims_otKeywordDialog(
	Widget main_w,
	IMS_OT_KEYWORD *data)
{
	Widget keywordDlg, title_w, label_w;
	Widget form_w, list_w;
	Widget ok_form_w, ok_w, separator_w;
	IMS_MSG_STRUCT *msgDesc = ims_ot_glbl_msg;
	char buffer[IMS_COL255_LEN+1];
	char *optionList[100];
	XmString labelStr;
	XmStringTable str_list;
	char *passPtr;
	int i, count;


	keywordDlg = XtVaCreatePopupShell ("keywordDlg",
				xmDialogShellWidgetClass, main_w,
				XmNtitle, "Keyword Information Dialog",
				XmNdeleteResponse, XmDESTROY,
				XmNwidth, 600,
				XmNheight, 300,
				XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
				NULL);

	form_w = XtVaCreateManagedWidget (
			  "keyword_dialog", xmRowColumnWidgetClass, keywordDlg,
				XmNwidth, 600,
				XmNheight, 300,
				NULL);

	sprintf(buffer, "KEYWORD: %s", data->keyword);


	label_w = XtVaCreateManagedWidget( buffer,
			xmLabelGadgetClass,
			form_w,  
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNtopPosition, 20,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 0,
			NULL );

	sprintf(buffer, "KEYWORD INDEX: %d", data->keyword_idx);

	label_w = XtVaCreateManagedWidget( buffer,
			xmLabelGadgetClass,
			form_w,  
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNtopAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 0, NULL);


	sprintf(buffer, "DESCRIPTION: %s", data->description);

	label_w = XtVaCreateManagedWidget( buffer,
			xmLabelGadgetClass,
			form_w,  
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNtopAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 0,
			NULL );


	i = 0;
	while ((i < data_type_count) && (data->data_type != data_type[i].item_id))
		i++;
	
	sprintf(buffer, "DATA TYPE: %s", data_type[i].item_name);

	label_w = XtVaCreateManagedWidget( buffer,
			xmLabelGadgetClass,
			form_w,  
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNtopAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 0, NULL);

	i = 0;
	while ((i < significance_count) &&
				 (data->significance != significance[i].item_id))
		i++;
	
	sprintf(buffer, "SIGNIFICANCE: %s", significance[i].item_name);

	label_w = XtVaCreateManagedWidget( buffer,
			xmLabelGadgetClass,
			form_w,  
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNtopAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 0, NULL);

	sprintf(buffer, "MAX LENGTH: %d", data->max_len);

	label_w = XtVaCreateManagedWidget( buffer,
			xmLabelGadgetClass,
			form_w,  
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNtopAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 0, NULL);


	if (ims_otGetKeywordValues(msgDesc, data->keyword_idx, optionList,
						&count) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not get keyword value info.");
		(void) ims_otMsg(msgDesc);
		return;
	}

	sprintf(buffer, "%d KEYWORD VALUE ITEMS", count);

	label_w = XtVaCreateManagedWidget( buffer,
			xmLabelGadgetClass,
			form_w,  
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNtopAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftOffset, 220,
			XmNrightAttachment, XmATTACH_POSITION,
			XmNrightOffset, 220,
			NULL);

	/*
	** If there are keywords for this item ...
	*/

	if (count > 0)
	{

		str_list = (void *) XtMalloc(sizeof(XmString) * count);

		for (i = 0; i < count; i++)
		{
			str_list[i] = XmStringCreateLocalized(optionList[i]);
		}

		list_w = XmCreateScrolledList(form_w, "KEYWORD_VALUE",
			NULL, 0);


		XtVaSetValues(list_w, 
			XmNvisibleItemCount, 6,
			XmNitemCount, count,
			XmNitems, str_list, NULL);

		XtAddCallback(list_w, XmNbrowseSelectionCallback,
				(XtCallbackProc) ims_otKeywordSelectCB,
				data->widget);
/*
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNtopAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 0, NULL);
*/

		XtManageChild(list_w);

		for (i = 0; i < count; i++)
		{
			XmStringFree(str_list[i]);
			XtFree(optionList[i]);
		}


		XtFree(str_list);
	}

	/*
	** Seperator
	*/

	separator_w  = XtVaCreateManagedWidget( "separator2",
		xmSeparatorWidgetClass,
		form_w,
		XmNalignment, XmALIGNMENT_BEGINNING,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_POSITION,
		XmNleftPosition, 0, NULL);

	ok_form_w = XtVaCreateWidget("ok_w", xmFormWidgetClass, form_w, 
			 XmNtopWidget, separator_w,
			XmNfractionBase, 15  , NULL);

    /* Creation of pushbutton INGEST */
	labelStr = XmStringCreateLocalized ("     OK     ");
		 
	 ok_w = XtVaCreateManagedWidget( "okPB",
			xmPushButtonWidgetClass,
			ok_form_w,
			XmNlabelString, labelStr,
			XmNalignment, XmALIGNMENT_BEGINNING,
			XmNtopAttachment, XmATTACH_FORM,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_POSITION,
			XmNleftPosition, 6,
			NULL );

	XtAddCallback(ok_w, XmNactivateCallback,
		(XtCallbackProc) ims_otKeywordOkCB,
		keywordDlg);

	XtManageChild(ok_form_w);

	XtManageChild(form_w);
	XtPopup(keywordDlg, XtGrabNone);

}

static void ims_otKeywordOkCB(
	Widget w,
	XtPointer client_data,
	XtPointer call_data)
{
	XtPopdown((Widget) client_data);
}

static void ims_otKeywordSelectCB(
Widget w,
XtPointer client_data,
XtPointer call_data)
{
	char *choice;

	XmListCallbackStruct *cbs = (XmListCallbackStruct *)call_data;

	XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &choice);

	XmTextSetString ((Widget)client_data, choice);

}
