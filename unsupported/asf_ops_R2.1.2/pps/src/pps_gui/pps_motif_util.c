/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
static char SccsFileId[] = "@(#)pps_motif_util.c	1.2    12/16/96";

#include <stdio.h>

#include <Xm/Xm.h>
#include <Xm/MessageB.h>


typedef struct
{
	XtCallbackProc	yesCallback;
	XtPointer	yesClientData;
	XtCallbackProc	noCallback;
	XtPointer	noClientData;

} XppsCallbackData, *XppsCallbackDataP;

static void
XppsQuestionDialogOkCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	XppsCallbackDataP callbackDataP = (XppsCallbackDataP)clientData;
	if (callbackDataP->yesCallback)
		(*(callbackDataP->yesCallback))
			(w, callbackDataP->yesClientData, 0);

	XtDestroyWidget(w);

}/*XppsQuestionDialogOkCB*/

static void
XppsQuestionDialogCancelCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	XppsCallbackDataP callbackDataP = (XppsCallbackDataP)clientData;
	if (callbackDataP->noCallback)
		(*(callbackDataP->noCallback))
			(w, callbackDataP->noClientData, 0);

	XtDestroyWidget(w);

}/*XppsQuestionDialogCancelCB*/

static void
XppsDestroyQuestionDialogCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	/*-------------------------------------------------------------*/
	/* destroy callback is installed on both dialog and shell      */
	/* but ok and cancel callbaks are installed on dialog only     */
	/* so, make sure we remove the callbacks on dialog only        */
	/*-------------------------------------------------------------*/
	if (XtHasCallbacks(w, XmNokCallback) == XtCallbackHasSome)
	{
		XtRemoveCallback(w, XmNokCallback,
			XppsQuestionDialogOkCB, (XtPointer)clientData);
	}
	if (XtHasCallbacks(w, XmNcancelCallback) == XtCallbackHasSome)
	{
		XtRemoveCallback(w, XmNcancelCallback,
			XppsQuestionDialogCancelCB, (XtPointer)clientData);
	}

	if (clientData)
		XtFree((char*)clientData);

}/*XppsDestroyQuestionDialogCB*/

void
XppsCreateQuestionDialog(
Widget		parent,
char*		msgString,
unsigned char	modal,
XtCallbackProc	yesCallback,
XtPointer	yesClientData,
XtCallbackProc	noCallback,
XtPointer	noClientData)
{
	Arg			wargs[20];
	int			n=0;
	Widget 			dialog=0;
	XmString		xmString=0;
	XppsCallbackDataP	clientData;
	XtCallbackRec		destroyCallbacks[2];

	/*--------------------------*/
	/* create the client data   */
	/*--------------------------*/
	clientData = (XppsCallbackDataP)XtMalloc(sizeof(XppsCallbackData));
	clientData->yesCallback = yesCallback;
	clientData->yesClientData = yesClientData;
	clientData->noCallback = noCallback;
	clientData->noClientData = noClientData;

	destroyCallbacks[0].callback = XppsDestroyQuestionDialogCB;
	destroyCallbacks[0].closure = (XtPointer)clientData;
	destroyCallbacks[1].callback = 0;
	destroyCallbacks[1].closure = 0;

	n=0;
	XtSetArg(wargs[n], XmNdestroyCallback, destroyCallbacks); n++;
	xmString = XmStringCreateLtoR("Question",XmFONTLIST_DEFAULT_TAG);
	XtSetArg(wargs[n], XmNdialogTitle, xmString); n++;
	dialog = XmCreateQuestionDialog(parent, "Question", wargs, n);
	XmStringFree(xmString);

	if (modal)
		XtVaSetValues(dialog,
			XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
			0);
	if (msgString)
	{
		xmString = XmStringCreateLtoR(msgString,XmFONTLIST_DEFAULT_TAG);
		XtVaSetValues(dialog,
			XmNmessageString, xmString,
			0);
		XmStringFree(xmString);
	}

	/*--------------------------*/
	/* unmanage the help button */
	/*--------------------------*/
	XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));

	/*----------------------------*/
	/* add ok and cancel callback */
	/*----------------------------*/
	XtAddCallback(dialog, XmNokCallback,
			XppsQuestionDialogOkCB, (XtPointer)clientData);
	XtAddCallback(dialog, XmNcancelCallback,
			XppsQuestionDialogCancelCB, (XtPointer)clientData);
	XtManageChild(dialog);

}/*XppsCreateQuestionDialog*/

static void
XppsErrorDialogOkCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	XppsCallbackDataP callbackDataP = (XppsCallbackDataP)clientData;
	if (callbackDataP->yesCallback)
		(*(callbackDataP->yesCallback))
			(w, callbackDataP->yesClientData, 0);

	XtDestroyWidget(w);

}/*XppsErrorDialogOkCB*/

static void
XppsDestroyErrorDialogCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	/*-------------------------------------------------------------*/
	/* destroy callback is installed on both dialog and shell      */
	/* but ok callbaks are installed on dialog only                */
	/* so, make sure we remove the callback on dialog only         */
	/*-------------------------------------------------------------*/
	if (XtHasCallbacks(w, XmNokCallback) == XtCallbackHasSome)
	{
		XtRemoveCallback(w, XmNokCallback,
			XppsErrorDialogOkCB, (XtPointer)clientData);
	}

	if (clientData)
		XtFree((char*)clientData);

}/*XppsDestroyErrorDialogCB*/

void
XppsCreateErrorDialog(
Widget		parent,
char*		msgString,
unsigned char	modal,
XtCallbackProc	yesCallback,
XtPointer	yesClientData)
{
	Arg			wargs[20];
	int			n=0;
	Widget 			dialog=0;
	XmString		xmString=0;
	XppsCallbackDataP	clientData;
	XtCallbackRec		destroyCallbacks[2];

	/*--------------------------*/
	/* create the client data   */
	/*--------------------------*/
	clientData = (XppsCallbackDataP)XtMalloc(sizeof(XppsCallbackData));
	clientData->yesCallback = yesCallback;
	clientData->yesClientData = yesClientData;
	clientData->noCallback = 0;
	clientData->noClientData = 0;

	destroyCallbacks[0].callback = XppsDestroyErrorDialogCB;
	destroyCallbacks[0].closure = (XtPointer)clientData;
	destroyCallbacks[1].callback = 0;
	destroyCallbacks[1].closure = 0;

	n=0;
	XtSetArg(wargs[n], XmNdestroyCallback, destroyCallbacks); n++;
	xmString = XmStringCreateLtoR("Error",XmFONTLIST_DEFAULT_TAG);
	XtSetArg(wargs[n], XmNdialogTitle, xmString); n++;
	dialog = XmCreateErrorDialog(parent, "Error", wargs, n);
	XmStringFree(xmString);

	if (modal)
		XtVaSetValues(dialog,
			XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
			0);
	if (msgString)
	{
		xmString = XmStringCreateLtoR(msgString,XmFONTLIST_DEFAULT_TAG);
		XtVaSetValues(dialog,
			XmNmessageString, xmString,
			0);
		XmStringFree(xmString);
	}

	/*-------------------------------------*/
	/* unmanage the cancel and help button */
	/*-------------------------------------*/
	XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));

	/*----------------------------*/
	/* add ok and cancel callback */
	/*----------------------------*/
	XtAddCallback(dialog, XmNokCallback,
			XppsErrorDialogOkCB, (XtPointer)clientData);

	XtManageChild(dialog);

}/*XppsCreateErrorDialog*/

static void
XppsWarningDialogOkCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	XppsCallbackDataP callbackDataP = (XppsCallbackDataP)clientData;
	if (callbackDataP->yesCallback)
		(*(callbackDataP->yesCallback))
			(w, callbackDataP->yesClientData, 0);

	XtDestroyWidget(w);

}/*XppsWarningDialogOkCB*/

static void
XppsDestroyWarningDialogCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	/*-------------------------------------------------------------*/
	/* destroy callback is installed on both dialog and shell      */
	/* but ok callbaks are installed on dialog only                */
	/* so, make sure we remove the callback on dialog only         */
	/*-------------------------------------------------------------*/
	if (XtHasCallbacks(w, XmNokCallback) == XtCallbackHasSome)
	{
		XtRemoveCallback(w, XmNokCallback,
			XppsWarningDialogOkCB, (XtPointer)clientData);
	}

	if (clientData)
		XtFree((char*)clientData);

}/*XppsDestroyWarningDialogCB*/

void
XppsCreateWarningDialog(
Widget		parent,
char*		msgString,
unsigned char	modal,
XtCallbackProc	yesCallback,
XtPointer	yesClientData)
{
	Arg			wargs[20];
	int			n=0;
	Widget 			dialog=0;
	XmString		xmString=0;
	XppsCallbackDataP	clientData;
	XtCallbackRec		destroyCallbacks[2];

	/*--------------------------*/
	/* create the client data   */
	/*--------------------------*/
	clientData = (XppsCallbackDataP)XtMalloc(sizeof(XppsCallbackData));
	clientData->yesCallback = yesCallback;
	clientData->yesClientData = yesClientData;
	clientData->noCallback = 0;
	clientData->noClientData = 0;

	destroyCallbacks[0].callback = XppsDestroyWarningDialogCB;
	destroyCallbacks[0].closure = (XtPointer)clientData;
	destroyCallbacks[1].callback = 0;
	destroyCallbacks[1].closure = 0;

	n=0;
	XtSetArg(wargs[n], XmNdestroyCallback, destroyCallbacks); n++;
	xmString = XmStringCreateLtoR("Warning",XmFONTLIST_DEFAULT_TAG);
	XtSetArg(wargs[n], XmNdialogTitle, xmString); n++;
	dialog = XmCreateWarningDialog(parent, "Warning", wargs, n);
	XmStringFree(xmString);

	if (modal)
		XtVaSetValues(dialog,
			XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
			0);
	if (msgString)
	{
		xmString = XmStringCreateLtoR(msgString,XmFONTLIST_DEFAULT_TAG);
		XtVaSetValues(dialog,
			XmNmessageString, xmString,
			0);
		XmStringFree(xmString);
	}

	/*-------------------------------------*/
	/* unmanage the cancel and help button */
	/*-------------------------------------*/
	XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));

	/*----------------------------*/
	/* add ok and cancel callback */
	/*----------------------------*/
	XtAddCallback(dialog, XmNokCallback,
			XppsWarningDialogOkCB, (XtPointer)clientData);

	XtManageChild(dialog);

}/*XppsCreateWarningDialog*/
