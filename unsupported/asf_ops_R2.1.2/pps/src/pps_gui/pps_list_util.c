/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
static char SccsFileId[] = "@(#)pps_list_util.c	1.1    11/21/96";

/* support routines for various callbacks for PPS GUI */

#include <stdio.h>
#include <unistd.h>
#include <errno.h>

#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/List.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Separator.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>

#include "UxLib.h"
#include "pps_util.h"

#define	FILE_RADIO_BUTTON	"fileRadioButton"
#define	PRINTER_RADIO_BUTTON	"printerRadioButton"
#define FILENAME_TEXTF		"filenameTextF"
#define PRINTER_TEXTF		"printerTextF"

typedef struct
{
	Widget	dialogW;
	char	filename[MAXSMALLBUF];

} PrintResultsData, *PrintResultsDataP;

typedef struct
{
	Widget	parent;
	char	oldFilename[MAXSMALLBUF];
	char	newFilename[MAXSMALLBUF];

} FilenamesData, *FilenamesDataP;

typedef struct
{
	Widget	fileSelectDialog;
	Widget	filenameTextF;

} FindData, *FindDataP;

char
list_get_all_items(
Widget		list,
char***		items,
int*		numItems)
{
	XmStringTable	xmStrings;
	int i=0;
	XtVaGetValues(list,
		XmNitemCount, numItems,
		XmNitems, &xmStrings,
		0);
	if (*numItems > 0)
	{
		*items = (char**)XtMalloc(*numItems * sizeof(char*));
		for (i=0; i < *numItems; i++)
		{
			XmStringGetLtoR(xmStrings[i],
				XmSTRING_DEFAULT_CHARSET,
				(char**)&((*items)[i]));
		}
		return(True);
	}
	else
		return(False);
	

}/* list_print_all_items */

static void
overwriteFileCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	char	buf[MAXSMALLBUF];

	FilenamesDataP filenamesData = (FilenamesDataP)clientData;
	if (fopen(filenamesData->newFilename, "w") == NULL)
	{
		(void)sprintf(buf, "Write to %s failed.",
				filenamesData->newFilename);
		XppsCreateErrorDialog(filenamesData->parent, buf, True, 0, 0);
		XtFree(filenamesData);
		unlink(filenamesData->oldFilename);
		return;
	}
	sprintf(buf, "mv %s %s", filenamesData->oldFilename,
				filenamesData->newFilename);
	system(buf);

	XtFree(filenamesData);

}/*overwriteFileCB*/

static void
nooverwriteFileCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	XtFree(clientData);

}/*nooverwriteFileCB*/

static void
save_to_new_file(
Widget	parent,
char*	oldFilename,
char*	newFilename)
{
	FilenamesDataP filenamesData;

	filenamesData = (FilenamesDataP)XtMalloc(sizeof(FilenamesData));
	filenamesData->parent = parent;
	(void)strcpy(filenamesData->oldFilename, oldFilename);
	(void)strcpy(filenamesData->newFilename, newFilename);

	/*------------------------------------------------------*/
	/* if file exists, ask user if he wants to overwrite it */
	/*------------------------------------------------------*/
	if (access(newFilename, F_OK) == 0)
	{
		XppsCreateQuestionDialog(parent,
			"File exists.  Overwrite it?",
			True, overwriteFileCB, (XtPointer)filenamesData,
			nooverwriteFileCB, (XtPointer)filenamesData);
	}
	else
	{
		(*overwriteFileCB)(parent, (XtPointer)filenamesData, 0);
	}

}/*save_to_new_file*/

static void
print_results_printCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	PrintResultsDataP	printResultsDataP;
	char			buf[MAXBIGBUF];
	Widget			fileRadioButton;
	Widget			filenameTextF;
	Widget			printerTextF;
	char*			filename;
	char*			printerString;

	printResultsDataP = (PrintResultsDataP)clientData;
	(void)sprintf(buf, "*%s", FILE_RADIO_BUTTON);
	fileRadioButton = XtNameToWidget(printResultsDataP->dialogW, buf);
	if (XmToggleButtonGetState(fileRadioButton))
	/* to file */
	{
		(void)sprintf(buf, "*%s", FILENAME_TEXTF);
		filenameTextF = XtNameToWidget(printResultsDataP->dialogW, buf);
		filename = XmTextFieldGetString(filenameTextF);
		if (filename && filename[0] != '\0')
		{
			save_to_new_file(
				XtParent(XtParent(printResultsDataP->dialogW)),
				printResultsDataP->filename, filename);
		}
		else
		{
			XppsCreateErrorDialog(
				XtParent(XtParent(printResultsDataP->dialogW)),
				"Filename not entered.", True, 0, 0);
			XtFree(filename);
			return;
		}
		XtFree(filename);
	}
	else
	/* to printer */
	{
		(void)sprintf(buf, "*%s", PRINTER_TEXTF);
		printerTextF = XtNameToWidget(printResultsDataP->dialogW, buf);
		printerString = XmTextFieldGetString(printerTextF);
		
		if (printerString && printerString[0] != '\0')
		{
			/*--------------------------------------*/
			/* print then remove the temp file      */
			/*--------------------------------------*/
			sprintf(buf, "/usr/local/bin/nenscript -Gr -P%s %s",
				printerString, printResultsDataP->filename);
			system(buf);

			sprintf(buf, "rm %s", printResultsDataP->filename);
			system(buf);
		}
		XtFree(printerString);
	}
	XtUnmanageChild(printResultsDataP->dialogW);

}/*print_results_printCB*/

static void
print_results_cancelCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	PrintResultsDataP printResultsDataP = (PrintResultsDataP)clientData;
	unlink(printResultsDataP->filename);
	XtUnmanageChild(printResultsDataP->dialogW);

}/*print_results_cancelCB*/

static void
findOkCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	XmString xmString;
	String	 filename=0;
	FindDataP findDataP = (FindDataP)clientData;
	Widget	dialogW = findDataP->fileSelectDialog;
	Widget	filenameTextF = findDataP->filenameTextF;

	if (dialogW == 0 || filenameTextF == 0)
	{
		fprintf(stderr, 
			"Internal Error: NULL file select or filename Widget\n");
		return;
	}

	XtVaGetValues(dialogW,
			XmNtextString, &xmString,
			0);
	XmStringGetLtoR(xmString, XmSTRING_DEFAULT_CHARSET, &filename);
	XmTextFieldSetString(filenameTextF, filename);
	XmStringFree(filename);

	XtUnmanageChild(dialogW);

}/*findOkCB*/

static void
findCancelCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	FindDataP findDataP = (FindDataP)clientData;
	XtUnmanageChild(findDataP->fileSelectDialog);

}/*findCancelCB*/

static void
print_results_findCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	FindDataP findDataP = (FindDataP)clientData;

	Widget fileSelectDialog = findDataP->fileSelectDialog;
	if (fileSelectDialog == 0)
	{
		/* create a file selection dialog */
		fileSelectDialog = XmCreateFileSelectionDialog(
				w, "File Selection", 0, 0);
		XtVaSetValues(fileSelectDialog,
			XmNtextColumns, 60,
			XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
			0);
		XtUnmanageChild(
			XmFileSelectionBoxGetChild(fileSelectDialog,
			XmDIALOG_HELP_BUTTON));
		XtManageChild(fileSelectDialog);

		/* add ok and cancel callbacks */
		XtAddCallback(fileSelectDialog,
			XmNokCallback, findOkCB, (XtPointer)findDataP);
		XtAddCallback(fileSelectDialog,
			XmNcancelCallback, findCancelCB, (XtPointer)findDataP);

		findDataP->fileSelectDialog = fileSelectDialog;
	}
	else
	{
		XtManageChild(fileSelectDialog);
		XMapRaised(XtDisplay(XtParent(fileSelectDialog)),
				XtWindow(XtParent(fileSelectDialog)));
	}

}/*print_results_findCB*/

Widget
create_print_results_dialog(
Widget		parent,
XmString	dialogTitle,
char*		filename)
{
	Widget	dialogW;
	Widget	radioBox, fileRadioButton, printerRadioButton;
	Widget	filenameTextF, findFilePB;
	Widget	printerTextF;
	Widget	separator;
	Widget	printPB, cancelPB;
	XmString	xmString;
	PrintResultsDataP	printResultsDataP;
	char*	printerString=0;
	Arg	wargs[20];
	int	n;
	FindDataP		findDataP;

	n=0;
	XtSetArg(wargs[n], XmNautoUnmanage, False); n++;
	XtSetArg(wargs[n], XmNdefaultButton, 0); n++;
	XtSetArg(wargs[n], XmNinitialFocus, 0); n++;
	XtSetArg(wargs[n], XmNdialogTitle, dialogTitle); n++;
	XtSetArg(wargs[n], XmNdialogStyle,
				XmDIALOG_PRIMARY_APPLICATION_MODAL); n++;
	XtSetArg(wargs[n], XmNverticalSpacing, 10); n++;
	XtSetArg(wargs[n], XmNhorizontalSpacing, 10); n++;
	dialogW = XmCreateFormDialog(parent, "printResultsDialog", wargs, n);

	/*--------------------------------------------*/
	/* create a radio box for "file" or "printer" */
	/*--------------------------------------------*/
	radioBox = XmCreateRadioBox(dialogW, "radioBox", 0, 0);
	XtVaSetValues(radioBox,
			XmNpacking, XmPACK_COLUMN,
			XmNspacing, 0,
			XmNtopAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			0);

	xmString = XmStringCreateLocalized("to File");
	fileRadioButton = XtVaCreateManagedWidget(FILE_RADIO_BUTTON,
			xmToggleButtonWidgetClass, radioBox,
			XmNspacing, 2,
			XmNlabelString, xmString,
			0);
	XmStringFree(xmString);
	xmString = XmStringCreateLocalized("to Printer");
	XmToggleButtonSetState(fileRadioButton, True, False);

	printerRadioButton = XtVaCreateManagedWidget(PRINTER_RADIO_BUTTON,
			xmToggleButtonWidgetClass, radioBox,
			XmNspacing, 2,
			XmNlabelString, xmString,
			0);
	XmStringFree(xmString);
	XtManageChild(radioBox);
	
	/*--------------------------------------------*/
	/* create a push button to bring up           */
	/* file selection dialog                      */
	/*--------------------------------------------*/
	xmString = XmStringCreateLocalized("Find...");
	findFilePB = XtVaCreateManagedWidget("findFilePB",
			xmPushButtonWidgetClass, dialogW,
			XmNshowAsDefault, 0,
			XmNlabelString, xmString,
			XmNtopAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
                        0);
	XmStringFree(xmString);

	/*--------------------------------------------*/
	/* create text fields for file and printer    */
	/*--------------------------------------------*/
	filenameTextF = XtVaCreateManagedWidget(FILENAME_TEXTF,
			xmTextFieldWidgetClass, dialogW,
			XmNcolumns, 40,
			XmNtopAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, radioBox,
			XmNrightAttachment, XmATTACH_WIDGET,
			XmNrightWidget, findFilePB,
			0);

	findDataP = (FindDataP)XtMalloc(sizeof(FindData));
	findDataP->fileSelectDialog = 0;
	findDataP->filenameTextF = filenameTextF;
	/* add callback for find push button */
	XtAddCallback(findFilePB, XmNactivateCallback,
			print_results_findCB, (XtPointer)findDataP);

	printerTextF = XtVaCreateManagedWidget(PRINTER_TEXTF,
			xmTextFieldWidgetClass, dialogW,
			XmNcolumns, 20,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, filenameTextF,
			XmNtopOffset, 0,
			XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
			XmNleftWidget, filenameTextF,
			XmNleftOffset, 0,
			0);
	if (printerString = (char*)getenv("PRINTER"))
	{
		XmTextFieldSetString(printerTextF, printerString);
	}

	/*--------------------------------------------*/
	/* create a separator                         */
	/*--------------------------------------------*/
	separator = XtVaCreateManagedWidget("separator",
			xmSeparatorWidgetClass, dialogW,
			XmNseparatorType, XmSHADOW_ETCHED_IN,
			XmNheight, 4,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, printerTextF,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			0);

	/*--------------------------------------------*/
	/* create the client data to pass to callback */
	/*--------------------------------------------*/
	printResultsDataP =
			(PrintResultsDataP)XtMalloc(sizeof(PrintResultsData));
	printResultsDataP->dialogW = dialogW;
	(void)strcpy(printResultsDataP->filename, filename);


	/*--------------------------------------------*/
	/* create "print" and "cancel" push buttons   */
	/*--------------------------------------------*/
	xmString = XmStringCreateLocalized("Print");
	printPB = XtVaCreateManagedWidget("printPB",
			xmPushButtonWidgetClass,
			dialogW,
			XmNshowAsDefault, 1,
			XmNalignment, XmALIGNMENT_CENTER,
			XmNlabelString, xmString,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, separator,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNleftAttachment, XmATTACH_FORM,
			XmNleftOffset, 20,
			0);
	XmStringFree(xmString);
	XtAddCallback(printPB, XmNactivateCallback,
			print_results_printCB, printResultsDataP);
	xmString = XmStringCreateLocalized("Cancel");

	cancelPB = XtVaCreateManagedWidget("cancelPB",
			xmPushButtonWidgetClass,
			dialogW,
			XmNalignment, XmALIGNMENT_CENTER,
			XmNlabelString, xmString,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, separator,
			XmNbottomAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNrightOffset, 20,
			0);
	XmStringFree(xmString);
	XtAddCallback(cancelPB, XmNactivateCallback,
			print_results_cancelCB, printResultsDataP);

	XtVaSetValues(dialogW,
			XmNdefaultButton, printPB,
			0);
	XtManageChild(dialogW);

	return(dialogW);

}/* create_print_result_dialog */
