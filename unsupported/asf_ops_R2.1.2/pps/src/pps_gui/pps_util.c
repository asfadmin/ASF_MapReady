/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/
static char SccsFileId[] = "@(#)pps_util.c	1.4    05/15/97";

/* support routines for various callbacks for PPS GUI */

#include "UxLib.h"
#include <Xm/Xm.h>
#include <X11/cursorfont.h>
#include <Xm/MessageB.h>
#include <Xm/TextF.h>

#include "pps_file_selector.h"
#include "pps_util.h"
#include "pps_error_dialog.h"
#include "pps_db.h"
#include "resload.h"
#include "PPShdr.h"
#include "PPSerr.h"

extern CS_CONNECTION *query_connection;

/*-------------------------------------*/
/* callback to destroy an error dialog */
/*-------------------------------------*/
static void
cb_error_dialog(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	XtRemoveCallback(w, XmNokCallback, cb_error_dialog, clientData);
	XtDestroyWidget(w);

} /* cb_error_dialog*/

/*-----------------------------------*/
/* routine to create an error dialog */
/*-----------------------------------*/
void
do_error_dialog(
swidget		parent,
char		*buf)
{
	Widget		errorDialog;
	XmString	msgString, titleString;

	if (parent == 0 || buf == 0)
	{
		fprintf(stderr, "do_error_dialog: NULL parent or msg string\n");
		return;
	}

	/* create the dialog */
	errorDialog = XmCreateErrorDialog(UxGetWidget(parent), "pps_error", 0, 0);

	/*----------------------------------------------------------*/
	/* set the title, error message string, and set it to modal */
	/*----------------------------------------------------------*/
	msgString = XmStringCreateLtoR(buf,XmFONTLIST_DEFAULT_TAG);
	titleString = XmStringCreateLtoR("PPS Error Dialog",XmFONTLIST_DEFAULT_TAG);
	XtVaSetValues(errorDialog,
			XmNmessageString, msgString,
			XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
			XmNdialogTitle, titleString,
			0);
	XmStringFree(msgString);
	XmStringFree(titleString);

	/* unmanage unused buttons */
	XtUnmanageChild(XmMessageBoxGetChild(errorDialog, XmDIALOG_HELP_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(errorDialog, XmDIALOG_CANCEL_BUTTON));

	/* add callbacks */
	XtAddCallback(errorDialog, XmNokCallback,
							cb_error_dialog, (XtPointer)errorDialog);

	/* show it */
	XtManageChild(errorDialog);

}/* do_error_dialog*/

/*-------------------------------------------*/
/* callback to destroy an information dialog */
/*-------------------------------------------*/
static void
cb_information_dialog(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	XtRemoveCallback(w, XmNokCallback, cb_information_dialog, clientData);
	XtDestroyWidget(w);

}/* cb_information_dialog*/

/*-----------------------------------------*/
/* routine to create an information dialog */
/*-----------------------------------------*/
void
do_information_dialog(
swidget		parent,
char		*buf)
{
	Widget		infoDialog;
	XmString	msgString, titleString;

	if (parent == 0 || buf == 0)
	{
		fprintf(stderr, "do_information_dialog: NULL parent or msg string\n");
		return;
	}

	/* create the dialog */
	infoDialog = XmCreateInformationDialog(UxGetWidget(parent),
					"pps_error", 0, 0);

	/*----------------------------------------------------------*/
	/* set the title, info message string, and set it to modal  */
	/*----------------------------------------------------------*/
	msgString = XmStringCreateLtoR(buf,XmFONTLIST_DEFAULT_TAG);
	titleString = XmStringCreateLtoR("PPS Information Dialog",XmFONTLIST_DEFAULT_TAG);
	XtVaSetValues(infoDialog,
			XmNmessageString, msgString,
			XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
			XmNdialogTitle, titleString,
			0);
	XmStringFree(msgString);
	XmStringFree(titleString);

	/* unmanage unused buttons */
	XtUnmanageChild(XmMessageBoxGetChild(infoDialog, XmDIALOG_HELP_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(infoDialog, XmDIALOG_CANCEL_BUTTON));

	/* add callbacks */
	XtAddCallback(infoDialog, XmNokCallback,
							cb_information_dialog, (XtPointer)infoDialog);

	/* show it */
	XtManageChild(infoDialog);

}/* do_information_dialog*/

/*---------------------------------------*/
/* callback to destroy an warning dialog */
/*---------------------------------------*/
static void
cb_warning_dialog(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	XtRemoveCallback(w, XmNokCallback, cb_warning_dialog, clientData);
	XtDestroyWidget(w);

}/* cb_warning_dialog*/

/*-------------------------------------*/
/* routine to create an warning dialog */
/*-------------------------------------*/
void
do_warning_dialog(
swidget		parent,
char		*buf)
{
	Widget		warningDialog;
	XmString	msgString, titleString;

	if (parent == 0 || buf == 0)
	{
		fprintf(stderr, "do_warning_dialog: NULL parent or msg string\n");
		return;
	}

	/* create the dialog */
	warningDialog = XmCreateWarningDialog(UxGetWidget(parent),
						"pps_error", 0, 0);

	/*------------------------------------------------------------*/
	/* set the title, warning message string, and set it to modal */
	/*------------------------------------------------------------*/
	msgString = XmStringCreateLtoR(buf,XmFONTLIST_DEFAULT_TAG);
	titleString = XmStringCreateLtoR("PPS Warning Dialog",XmFONTLIST_DEFAULT_TAG);
	XtVaSetValues(warningDialog,
			XmNmessageString, msgString,
			XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
			XmNdialogTitle, titleString,
			0);
	XmStringFree(msgString);
	XmStringFree(titleString);

	/* unmanage unused buttons */
	XtUnmanageChild(XmMessageBoxGetChild(warningDialog,
								XmDIALOG_HELP_BUTTON));
	XtUnmanageChild(XmMessageBoxGetChild(warningDialog,
								XmDIALOG_CANCEL_BUTTON));

	/* add callbacks */
	XtAddCallback(warningDialog, XmNokCallback,
							cb_warning_dialog, (XtPointer)warningDialog);

	/* show it */
	XtManageChild(warningDialog);

}/* do_warning_dialog*/

/* common code for print screen callbacks */
void pps_print_screen(swidget sw)
{
	char buf[MAXBIGBUF];
	int mypid;

	mypid = getpid();
	sprintf(buf,
		"/usr/local/bin/import -rotate 90 -window 0x%x /tmp/%d.ps\n",
		XtWindow(UxGetWidget(sw)), mypid);
	system(buf);
	sprintf(buf, "lpr /tmp/%d.ps\n", mypid);
	system(buf);
	sprintf(buf, "rm /tmp/%d.ps\n", mypid);
	system(buf);
}

/* get the LabelString for the currently selected button in an option menu */
char *get_om_label(swidget sw)
{
	return UxGetLabelString(UxFindSwidget(UxGetMenuHistory(sw)));
}

/* callback for file selector dialog box Cancel button */
void cb_no_file_selected(Widget widget, XtPointer client_data,
	XtPointer call_data)
{
	/* pop down the file selector */
	UxDestroyInterface(pps_file_selector);
}


/*--------------------------------------------------------*/
/* routine to get the current job_state from the database */
/*--------------------------------------------------------*/

int get_job_state (int job_id, char *job_state)
{
	struct 		pps_db_exec_dcl pps_query;
	char		current_job_state[ORDER_STATUS_STRLEN];
	int		retcode;
	char		cmdbuf[MAXSMALLBUF];

        pps_query.num_items = 0;
	pps_query.callback = 0;

	/* set up the binding for retrieving data */
        pps_db_bind_char(&pps_query, current_job_state, ORDER_STATUS_STRLEN);

	/* make the actual sql select statement */
	sprintf(cmdbuf, "select job_state from jobs where job_id = %d",
			job_id);

        /* execute the sql command */
        retcode = db_exec(&query_connection, cmdbuf, &pps_query);
 
        if (retcode == ER_NO_ERROR)
        {
		strcpy(job_state,current_job_state);
		return (ER_NO_ERROR);
	}
	else
		return (ER_DB_ACCESS);
}

/*--------------------------------------------------------*/
/* routine to validate numeric fields                     */ 
/*--------------------------------------------------------*/

int
validate_int_fields(
swidget		mainWindow,
intFields	planIntFields[],
int			numFields)
{
	int	i=0, k=0, dec_pt;

	for (i=0; i < numFields; i++)
	{
		char* string = XmTextFieldGetString(UxGetWidget(planIntFields[i].textField));
		/* if the text field is not empty */
		if (string[0] != '\0')
		{
			dec_pt = 0;
			for (k=0; k < strlen(string); k++)
			{
				/* 
				** if the text field contains more than one decimal
				** point or contains non-digit char, it's not qualified
				** as a numeric field
				*/
				if ((string[k] == '.' &&
				     dec_pt == 1)          ||
				    (string[k] != '.' &&
					 (! isdigit((int)string[k]))))
				{
					XmString xmString;
					char* labelString;
					XtVaGetValues(UxGetWidget(planIntFields[i].label),
						XmNlabelString, &xmString,
						0);
					if(XmStringGetLtoR(xmString,
							XmSTRING_DEFAULT_CHARSET,
							&labelString))
					{
						char	errorString[MAXSMALLBUF];

						(void)sprintf(errorString,
							"\"%s\" is not a valid value for the \"%s\" field",
							string, labelString);
						XtFree(labelString);
						XppsCreateErrorDialog(
							UxGetWidget(mainWindow),
							errorString, True, 0, 0);
						return (False);
					}
					else
						return (False);
				}
				if (string[k] == '.')
				{
					dec_pt++;
				}
			}
		}
		XtFree(string);
	}
	return (True);

} /* validate_int_fields */

