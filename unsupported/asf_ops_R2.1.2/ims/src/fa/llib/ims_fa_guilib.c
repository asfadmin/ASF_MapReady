static char *sccs = "@(#)ims_fa_guilib.c	6.1 03/13/98";
/*****************************************************************************
*
**
** File:    ims_fa_guilib.c
**
** Function: GUI support library for flight agency i/f     
**		
**
** Author: Dan Crichton
**
** Date:    9/11/96
**
** Modification: D. Ting 4/17/97
**							 Changed segment_id to sequence
**               D. Ting 5/28/97
**							 Corrected dataset_idx short interger problem.
**							 Added if granule_idx == -1 then error out for view metadata and report file.
**							 D. Ting 6/3/97
**						   Corrected per Joe requests.
**						   D. Ting 6/17/97
**						   added view CSA reports
**
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <sys/signal.h>

#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <Xm/RowColumn.h>
#include <Xm/BulletinB.h>
#include <Xm/SelectioB.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/MessageB.h>
#include <Xm/ToggleBG.h>


#include <ims_dbms.h>
#include <ims_query.h> 
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <ims_cmd.h>
#include <syslog.h>

#include <ims_job_control.h>
#include <ims_fa_reports.h>
#include <ims_reporter.h>

#define REPORTED_ACQUISITIONS 		1
#define UNREPORTED_ACQUISITIONS		2

static struct
{
	Widget aq_reported,  		/* Acquisition Reported Toggle Button */
		aq_not_reported,	/* Acquisition Not Reported Toggle Button */
		date_all,			/* Date All Toggle Button */
		date_range,			/* Date Range Toggle Button */
		date_to,			/* Date To Text Field */
		date_from,			/* Date From Text Field */
		rev_all,			/* Rev All Toggle Button */
		rev_range,			/* Rev Range Toggle Button */
		rev_to,				/* Rev To Text Field */
		rev_from,			/* Rev From Text Field */
		platforms,			/* Platform list */
		reports, 			/* Reports list */
		name_all,			/* Name All Toggle Button */
		name_range,			/* Name Range Toggle Button */
		name_from,			/* Name From Toggle Button */
		view_list,			/* View List Window for Acquisition and History */
		send_list,			/* View List Window for Resend Capability. */
		dialog;

	int	platform_id;		/* Current Platform Selection */
	int	report_id;			/* Current Report Type Selection */
} local_widgets;

typedef struct datalist
{
	int dataset_idx;
	int granule_idx;
	short int report_id;
	struct datalist *next;
} DATALIST;

extern Widget app_shell;
extern IMS_MSG_STRUCT *glbl_msgDesc;
extern IMS_JOB_USER_SPEC userSpec;

XmFontList loadTitleFont(Widget wshell, char *name);  
XmFontList loadCopyFont(Widget wshell, char *name);  
XmFontList loadReporterFont(Widget wshell, char *name);  
XmFontList loadDialogFont(Widget wshell, char *name);  
XmFontList loadListFont(Widget wshell, char *name);
static void exit_button_cb(Widget, XtPointer, XtPointer);
static void generate_button_cb(Widget, XtPointer, XtPointer);
static void viewa_button_cb(Widget, XtPointer, XtPointer);
static void view_cancel_cb(Widget, XtPointer, XtPointer);
static void view_metadata_cb(Widget, XtPointer, XtPointer);
static void view_report_cb(Widget, XtPointer, XtPointer);
static void viewh_button_cb(Widget, XtPointer, XtPointer);
static void resend_button_cb(Widget, XtPointer, XtPointer);
static void viewa_query_cb(Widget, XtPointer, XtPointer);
static void valid_date_cb(Widget, XtPointer, XtPointer);
static void valid_rev_cb(Widget, XtPointer, XtPointer);
static void valid_name_cb(Widget, XtPointer, XtPointer);
static void platforms_cb(Widget, int, XtPointer);
static void reports_cb(Widget, int, XtPointer);
static void showAcquisitionList(Widget, int, char *, char *, char *, char *, char *);
static void viewh_button_cb(Widget, XtPointer, XtPointer);
void showHistoryScreen(Widget wshell);
static void showHistoryList(Widget, short int, char *, char *, char *, char *); 
static int showMetadata(IMS_MSG_STRUCT *, int, int);
static int showReportDetail(IMS_MSG_STRUCT *, int, int, int);
static int startXClientJob(IMS_MSG_STRUCT *, char *, char *, void (*) (), char *);
static void endViewerCb(int, int, int, int, char *);
static void showResendScreen(Widget);
static void resend_report_cb(Widget, XtPointer, XtPointer);
int startSubmission(char *, char *, char *);


/*******************************************************************
** 
** showIntroScreen
**
*******************************************************************/

void showIntroScreen(
		Widget wshell)
{
	XmFontList fontlist, fontlist2, fontreporter;
	Widget bboard, 
			gen_button, resend_button, viewh_button, viewa_button,
			exit_button;
	Widget welcome;
	Arg args[10];
	XmString t;
	Pixel fg, bg;
	Pixmap pixmap;
	char buffer[IMS_COL255_LEN+1];

	/*
	**
	** Setup the window and initialize it's resources
	**
	*/

	bboard = XtVaCreateManagedWidget("bb_main", xmBulletinBoardWidgetClass,
		wshell, args, 0, NULL);

	/*
	** Create the buttons...
	*/

    fontlist = loadTitleFont(wshell, "charset1");

	gen_button = XtVaCreateManagedWidget(   "   GENERATE REPORT   ",
		xmPushButtonWidgetClass, bboard,
		XmNx, IMS_FA_MW_GEN_X1, XmNy, IMS_FA_MW_GEN_Y1,
		XmNfontList, fontlist,
		NULL);

	resend_button = XtVaCreateManagedWidget("    UNSENT REPORTS   ",
		xmPushButtonWidgetClass, bboard,
		XmNx, IMS_FA_MW_RESEND_X1, XmNy, IMS_FA_MW_RESEND_Y1,
		XmNfontList, fontlist,
		NULL);

	viewh_button = XtVaCreateManagedWidget( "     VIEW HISTORY    ",
		xmPushButtonWidgetClass, bboard,
		XmNx, IMS_FA_MW_VIEWH_X1, XmNy, IMS_FA_MW_VIEWH_Y1,
		XmNfontList, fontlist,
		NULL);

	viewa_button = XtVaCreateManagedWidget( "   VIEW ACQUISITIONS  ",
		xmPushButtonWidgetClass, bboard,
		XmNx, IMS_FA_MW_VIEWA_X1, XmNy, IMS_FA_MW_VIEWA_Y1,
		XmNfontList, fontlist,
		NULL);

	exit_button = XtVaCreateManagedWidget(  "         EXIT        ",
		xmPushButtonWidgetClass, bboard,
		XmNx, IMS_FA_MW_EXIT_X1, XmNy, IMS_FA_MW_EXIT_Y1,
		XmNfontList, fontlist,
		NULL);

	/*
	**
	** Display the title
	**
	*/

	t = XmStringCreate("FLIGHT AGENCY REPORTS - ALASKA SAR FACILITY", "charset1");

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNlabelString, t,
		XmNfontList, fontlist,
		XmNx, IMS_FA_MW_TITLE1_X1,
		XmNy, IMS_FA_MW_TITLE1_Y1, NULL);


	XmStringFree(t);

		

	/*
	**
	** Display the welcome
	**
	*/
    fontreporter = loadReporterFont(wshell, "reporter");

	sprintf(buffer,
		"Welcome %s.  You are currently logged into %s on the %s server.",
		userSpec.username, userSpec.database, userSpec.server);

	t = XmStringCreateSimple(buffer);

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNlabelString, t,
		XmNfontList, fontreporter,
		XmNx, 40,
		XmNy, IMS_FA_MW_TITLE1_Y1 + 30, NULL);


	XmStringFree(t);

		

	/*
	**
	** Display the copyright information.
	**
	*/

    fontlist2 = loadCopyFont(wshell, "charset2");


	t = XmStringCreate("Copyright (C) 1996, California Institute of Technology.  U.S. Government", "charset2");

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNlabelString, t,
		XmNfontList, fontlist2,
		XmNx, IMS_FA_MW_COPY1_X1,
		XmNy, IMS_FA_MW_COPY1_Y1, NULL);


	XmStringFree(t);

	t = XmStringCreate("Sponsorship under NASA Contract NAS7-1260 is acknowledged.",
	"charset2");

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNlabelString, t,
		XmNfontList, fontlist2,
		XmNx, IMS_FA_MW_COPY2_X1,
		XmNy, IMS_FA_MW_COPY2_Y1, NULL);

	XmStringFree(t);

	/*
	**
	** Load the pixmap for this window...
	**
	*/


	XtVaGetValues(bboard, 
		XmNforeground, &fg,
		XmNbackground, &bg,
		NULL);


	pixmap = XmGetPixmap(XtScreen(bboard), "/local/imsdads/app-defaults/pixmaps/COMPBANK.XBM",
		fg, bg);
	
	(void) XtVaCreateManagedWidget("bitmap", xmLabelGadgetClass,
		bboard, 
		XmNlabelType, XmPIXMAP, XmNlabelPixmap, pixmap,
		XmNx, IMS_FA_MW_PIC_X1, XmNy, IMS_FA_MW_PIC_Y1, 
		NULL);


	/*
	**
	** Setup the callbacks for this window
	**
	**/

	XtAddCallback(gen_button, XmNactivateCallback,
		generate_button_cb, (void *) bboard);

	XtAddCallback(viewa_button, XmNactivateCallback,
		viewa_button_cb, (void *) bboard);

	XtAddCallback(viewh_button, XmNactivateCallback,
		viewh_button_cb, (void *) bboard);

	XtAddCallback(resend_button, XmNactivateCallback,
		resend_button_cb, (void *) bboard);

	XtAddCallback(exit_button, XmNactivateCallback,
		exit_button_cb, NULL);

	
	/*
	**
	** Manage the new bulletin board main window.
	** 
	*/ 

	XtManageChild(bboard);
	XtRealizeWidget(app_shell);
}


/*******************************************************************
** 
** loadTitleFont
**
*******************************************************************/

XmFontList loadTitleFont(Widget wshell, char *name)
{
	Display *dpy;
	XFontStruct *font;
	XmFontList fontlist;

	dpy = XtDisplay(wshell);
	font = XLoadQueryFont(dpy, "-adobe-new century schoolbook-bold-r-normal--14-100-100-100-p-87-iso8859-1");
	fontlist = XmFontListCreate(font, name);
	return(fontlist);

}

/*******************************************************************
** 
** loadCopyFont
**
*******************************************************************/

XmFontList loadCopyFont(Widget wshell, char *name)
{
	Display *dpy;
	XFontStruct *font;
	XmFontList fontlist;

	dpy = XtDisplay(wshell);
	font = XLoadQueryFont(dpy, "-adobe-times-medium-r-normal--11-80-100-100-p-54-iso8859-1");
	fontlist = XmFontListCreate(font, name);
	return(fontlist);

}

/*******************************************************************
** 
** loadReporterFont
**
*******************************************************************/

XmFontList loadReporterFont(Widget wshell, char *name)
{
	Display *dpy;
	XFontStruct *font;
	XmFontList fontlist;

	dpy = XtDisplay(wshell);
	font = XLoadQueryFont(dpy, "-adobe-new century schoolbook-medium-r-normal-*-14-20-*-*-*-*-*-*");
	fontlist = XmFontListCreate(font, name);
	return(fontlist);

}

/*******************************************************************
** 
** loadDialogFont
**
*******************************************************************/

XmFontList loadDialogFont(Widget wshell, char *name)
{
	Display *dpy;
	XFontStruct *font;
	XmFontList fontlist;

	dpy = XtDisplay(wshell);
	font = XLoadQueryFont(dpy, "-adobe-new century schoolbook-medium-r-normal--12-*-*-*-*-*-*-*");
	fontlist = XmFontListCreate(font, name);
	return(fontlist);

}

/*******************************************************************
** 
** loadCourierFont
**
*******************************************************************/

XmFontList loadCourierFont(Widget wshell, char *name)
{
	Display *dpy;
	XFontStruct *font;
	XmFontList fontlist;

	dpy = XtDisplay(wshell);
	font = XLoadQueryFont(dpy, "-adobe-courier-medium-r-normal--12-*-*-*-*-*-*-*");
	fontlist = XmFontListCreate(font, name);
	return(fontlist);

}

/*******************************************************************
** 
** loadListFont
**
*******************************************************************/

XmFontList loadListFont(Widget wshell, char *name)
{
	Display *dpy;
	XFontStruct *font;
	XmFontList fontlist;

	dpy = XtDisplay(wshell);
	font = XLoadQueryFont(dpy, "-*-courier-medium-r-normal-*-11-20-*-*-*-*-*-*");
	fontlist = XmFontListCreate(font, name);
	return(fontlist);

}

/*******************************************************************
** 
** exit_button_cb
**
*******************************************************************/

static void exit_button_cb(
	Widget w, 
	XtPointer client_d, 
	XtPointer call_d)
{
	exit(1);
}


/*******************************************************************
** 
** generate_button_cb
**
*******************************************************************/

static void generate_button_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	BuildDialog((Widget) client_d);
}

/*******************************************************************
** 
** showAcquisitionScreen
**
*******************************************************************/

void showAcquisitionScreen(
		Widget wshell)
{
	Widget bboard, option, date_from, date_to, rev_from, rev_to;
	Widget pshell, acq_setting, date_range, rev_range;
	Widget query_button, cancel_button;
	Widget aq_reported, aq_not_reported;
	Arg args[7];
	XmString t;
	XmString platforms[6];
	XmFontList fontlist;

	XtSetArg(args[0], XmNx, 0);
	XtSetArg(args[1], XmNy, 0);
	XtSetArg(args[2], XmNwidth, IMS_FA_VAQ_X2);	
	XtSetArg(args[3], XmNheight, IMS_FA_VAQ_Y2);	

	pshell = XtCreatePopupShell("View Acquisition Information",
		transientShellWidgetClass, wshell, args, 4);

	bboard = XtVaCreateManagedWidget("bboard", xmBulletinBoardWidgetClass,
		pshell, args, 4, NULL);

    fontlist = loadDialogFont(wshell, "dialogFont");
	/*
	** Display window options
	*/

	/*
	** Make acquisition setting radio box and labels.
	*/

	t = XmStringCreateSimple("Acquisition Type:");

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNlabelString, t,
		XmNfontList, fontlist,
		XmNx, IMS_FA_VAQ_ACQLAB_X1,
		XmNy, IMS_FA_VAQ_ACQLAB_Y1, NULL);


	XmStringFree(t);

	acq_setting = 
		XmCreateRadioBox(bboard, "acq_setting", NULL, 0);

	XtVaSetValues(acq_setting, XmNnumColumns, 2, 
		XmNx, IMS_FA_VAQ_ACQRADIO_X1,
		XmNy, IMS_FA_VAQ_ACQRADIO_Y1,
		XmNfontList, fontlist,
		NULL);

	local_widgets.aq_not_reported = XtVaCreateManagedWidget("Not Reported", 
		xmToggleButtonGadgetClass, acq_setting, NULL);

	local_widgets.aq_reported =	XtVaCreateManagedWidget("Reported", 
		xmToggleButtonGadgetClass, acq_setting, NULL);

	XtManageChild(acq_setting);

	XmToggleButtonSetState(local_widgets.aq_not_reported,
		TRUE, FALSE);

	/*
	** Make platform list
	*/

	local_widgets.platform_id = 1;

	t = XmStringCreateSimple("Platform:");
	platforms[0] = XmStringCreateSimple("<ALL>");
	platforms[1] = XmStringCreateSimple("ADEOS-1");
	platforms[2] = XmStringCreateSimple("ERS-1");
	platforms[3] = XmStringCreateSimple("ERS-2");
	platforms[4] = XmStringCreateSimple("JERS-1");
	platforms[5] = XmStringCreateSimple("RADARSAT-1");

	local_widgets.platforms = XmVaCreateSimpleOptionMenu(bboard, "option",
		t, 0, 0, (XtCallbackProc) platforms_cb,
		XmVaPUSHBUTTON, platforms[0], NULL, NULL, NULL,
		XmVaPUSHBUTTON, platforms[1], NULL, NULL, NULL,
		XmVaPUSHBUTTON, platforms[2], NULL, NULL, NULL,
		XmVaPUSHBUTTON, platforms[3], NULL, NULL, NULL,
		XmVaPUSHBUTTON, platforms[4], NULL, NULL, NULL,
		XmVaPUSHBUTTON, platforms[5], NULL, NULL, NULL,
		XmNfontList, fontlist,
		XmNy, IMS_FA_VAQ_PLATFORM_Y1, 0);

	XmStringFree(t);
	XmStringFree(platforms[0]);
	XmStringFree(platforms[1]);
	XmStringFree(platforms[2]);
	XmStringFree(platforms[3]);
	XmStringFree(platforms[4]);
	XmStringFree(platforms[5]);

	XtManageChild(local_widgets.platforms);


	/*
	** Make date range setting radio box and labels.
	*/

	t = XmStringCreateSimple("Date Range:");

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNlabelString, t,
		XmNfontList, fontlist,
		XmNx, IMS_FA_VAQ_DATELAB_X1,
		XmNy, IMS_FA_VAQ_DATELAB_Y1, NULL);


	XmStringFree(t);

	date_range = 
		XmCreateRadioBox(bboard, "date_range", NULL, 0);

	XtVaSetValues(date_range, XmNnumColumns, 2, 
		XmNx, IMS_FA_VAQ_DATERADIO_X1,
		XmNy, IMS_FA_VAQ_DATERADIO_Y1,
		XmNradioAlwaysOne, TRUE,
		XmNfontList, fontlist,
		NULL);

	local_widgets.date_all =  XtVaCreateManagedWidget("All", 
		xmToggleButtonGadgetClass, date_range, NULL);

	local_widgets.date_range = XtVaCreateManagedWidget("From:", 
		xmToggleButtonGadgetClass, date_range, NULL);

	local_widgets.date_from = XtVaCreateManagedWidget("text",
		xmTextWidgetClass, bboard, XmNeditable, TRUE,
		XmNwidth, 140, 
		XmNmaxLength, 21, 
		XmNx, IMS_FA_VAQ_DATEFROM_X1+18,/*r2.1*/
		XmNy, IMS_FA_VAQ_DATEFROM_Y1,
		XmNfontList, fontlist,
		NULL);

	XtAddCallback(local_widgets.date_from,  XmNmodifyVerifyCallback,
		valid_date_cb, NULL);


	t = XmStringCreateSimple("To:");

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNlabelString, t,
		XmNfontList, fontlist,
		XmNx, IMS_FA_VAQ_DATETOLAB_X1+10, /*r2.1*/
		XmNy, IMS_FA_VAQ_DATETOLAB_Y1, NULL);


	XmStringFree(t);

	local_widgets.date_to = XtVaCreateManagedWidget("text",
		xmTextWidgetClass, bboard, XmNeditable, TRUE,
		XmNfontList, fontlist,
		XmNwidth, 140, 
		XmNmaxLength, 21, 
		XmNx, IMS_FA_VAQ_DATETO_X1+12,/*r2.1*/
		XmNy, IMS_FA_VAQ_DATETO_Y1,
		NULL);

	XtAddCallback(local_widgets.date_to,  XmNmodifyVerifyCallback,
		valid_date_cb, NULL);

	XtManageChild(date_range);

	XmToggleButtonSetState(local_widgets.date_all,
		TRUE, FALSE);

	/*
	** Make revolution range setting radio box and labels.
	*/

	t = XmStringCreateSimple("Revolution Range:");

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNfontList, fontlist,
		XmNlabelString, t,
		XmNx, IMS_FA_VAQ_REVLAB_X1,
		XmNy, IMS_FA_VAQ_REVLAB_Y1, NULL);


	XmStringFree(t);

	rev_range = 
		XmCreateRadioBox(bboard, "rev_range", NULL, 0);

	XtVaSetValues(rev_range, XmNnumColumns, 2, 
		XmNfontList, fontlist,
		XmNx, IMS_FA_VAQ_REVRADIO_X1,
		XmNy, IMS_FA_VAQ_REVRADIO_Y1,
		XmNradioAlwaysOne, TRUE,
		NULL);

	local_widgets.rev_all =  XtVaCreateManagedWidget("All", 
		xmToggleButtonGadgetClass, rev_range, NULL);

	local_widgets.rev_range = XtVaCreateManagedWidget("From:", 
		xmToggleButtonGadgetClass, rev_range, NULL);

	local_widgets.rev_from = XtVaCreateManagedWidget("text",
		xmTextWidgetClass, bboard, XmNeditable, TRUE,
		XmNfontList, fontlist,
		XmNwidth, 70, 
		XmNmaxLength, 10, 
		XmNx, IMS_FA_VAQ_REVFROM_X1+18, /*r2.1*/
		XmNy, IMS_FA_VAQ_REVFROM_Y1,
		NULL);

	XtAddCallback(local_widgets.rev_from,  XmNmodifyVerifyCallback,
		valid_rev_cb, NULL);

	t = XmStringCreateSimple("To:");

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNfontList, fontlist,
		XmNlabelString, t,
		XmNx, IMS_FA_VAQ_REVTOLAB_X1+14, /*r2.1*/
		XmNy, IMS_FA_VAQ_REVTOLAB_Y1, NULL);


	XmStringFree(t);

	local_widgets.rev_to = XtVaCreateManagedWidget("text",
		xmTextWidgetClass, bboard, XmNeditable, TRUE,
		XmNfontList, fontlist,
		XmNwidth, 70, 
		XmNmaxLength, 10, 
		XmNx, IMS_FA_VAQ_REVTO_X1+14,/*r2.1*/
		XmNy, IMS_FA_VAQ_REVTO_Y1,
		NULL);

	XtAddCallback(local_widgets.rev_to,  XmNmodifyVerifyCallback,
		valid_rev_cb, NULL);

	XtManageChild(rev_range);

	XmToggleButtonSetState(local_widgets.rev_all,
		TRUE, FALSE);

	/*
	** Add the buttons for QUERY and CANCEL
	*/


	query_button = XtVaCreateManagedWidget( "   QUERY   ",
		xmPushButtonWidgetClass, bboard,
		XmNfontList, fontlist,
		XmNx, IMS_FA_VAQ_QUERY_X1, XmNy, IMS_FA_VAQ_QUERY_Y1,
		NULL);

	cancel_button = XtVaCreateManagedWidget( "   CANCEL   ",
		xmPushButtonWidgetClass, bboard,
		XmNfontList, fontlist,
		XmNx, IMS_FA_VAQ_CANCEL_X1, XmNy, IMS_FA_VAQ_CANCEL_Y1,
		NULL);

	/*
	** Add the callbacks.
	*/

	XtAddCallback(cancel_button, XmNactivateCallback,
		view_cancel_cb, (void *) pshell);

	XtAddCallback(query_button, XmNactivateCallback,
		viewa_query_cb, (void *) pshell);

	/*
	** Manage the popup shell widget.
	*/

	XtManageChild(pshell);
	XtRealizeWidget(app_shell);
	XtPopup(pshell, XtGrabExclusive);

}


/*******************************************************************
** 
** viewa_button_cb
**
*******************************************************************/

static void viewa_button_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{

	showAcquisitionScreen((Widget) client_d);
}

/*******************************************************************
** 
** viewh_button_cb
**
*******************************************************************/

static void viewh_button_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{

	showHistoryScreen((Widget) client_d);
}

/*******************************************************************
** 
** platforms_cb
**
*******************************************************************/

static void platforms_cb(
	Widget w,
	int item_no,
	XtPointer call_d)
{
	local_widgets.platform_id = item_no + 1;
}

/*******************************************************************
** 
** view_cancel_cb
**
*******************************************************************/

static void view_cancel_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	XtPopdown((Widget) client_d);
}

/*******************************************************************
** 
** viewh_query_cb
**
*******************************************************************/

static void viewh_query_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	char *report_name = NULL;
	char *name = NULL;
	char *date_start = NULL;
	char *date_end = NULL;
	IMS_NUMERIC_DATE dateStruct;
	char db_date_start[IMS_COL30_LEN+1];
	char db_date_end[IMS_COL30_LEN+1];

	switch (local_widgets.report_id)
	{
		case 0:
			report_name = "<All Report Types>";
			break;

		case 1:
			report_name = "RADARSAT-1 ASR";
			break;

		case 2:
			report_name = "RADARSAT-1 Reception Report";
			break;

		case 3:
			report_name = "RESERVED";
			break;

		case 4:
			report_name = "ERS-1 RESM";
			break;

		case 5:
			report_name = "ERS-1 REAQ";
			break;

		case 6:
			report_name = "ERS-1 REEX";
			break;

		case 7:
			report_name = "JERS-1 CATA";
			break;

		case 8:
			report_name = "JERS-1 REAC";
			break;

		case 9:
			report_name = "JERS-1 MSGM";
			break;

		case 10:
			report_name = "ADEOS-1 REAC";
			break;

		case 11:
			report_name = "ADEOS-1 SRRD";
			break;

		case 12:
			report_name = "ERS-2 RESM";
			break;

		case 13:
			report_name = "ERS-2 REAQ";
			break;

		case 14:
			report_name = "ERS-2 REEX";
			break;
	}

	/*
	** Check if name provided.
	*/
	if (XmToggleButtonGetState(local_widgets.name_range) == TRUE)
	{
		name = XmTextGetString(local_widgets.name_from);
	}


	/*
	** Check if date range provided.
	*/

	if (XmToggleButtonGetState(local_widgets.date_range) == TRUE)
	{
		date_start = XmTextGetString(local_widgets.date_from);
		date_end = XmTextGetString(local_widgets.date_to);

	    if (ims_timeToNumericDate(glbl_msgDesc,
				date_start, &dateStruct) < IMS_OK)
		{
			msg_box(app_shell, IMS_ERROR, "Error", 
				"'From' Time not in YYYY-DDDTHH:MM:SS.mss format.");
			goto free_and_exit;
		}

		ims_numericDateToDBMSA(&dateStruct, db_date_start);

   		if (ims_timeToNumericDate(glbl_msgDesc,
			date_end, &dateStruct) < IMS_OK)
		{
			msg_box(app_shell,  IMS_ERROR, "Error",
				"'To' Time not in YYYY-DDDTHH:MM:SS.mss format.");
			goto free_and_exit;
		}

		ims_numericDateToDBMSA(&dateStruct, db_date_end);
	} 


	if (date_start == NULL)
	{
		showHistoryList((Widget) client_d, local_widgets.report_id, 
				report_name, NULL, NULL, name);

	}
	else
	{
		showHistoryList((Widget) client_d, local_widgets.report_id, 
				report_name, db_date_start, db_date_end, name);
	}

free_and_exit:

	if (name != NULL)
		XtFree(name);

	if (date_start != NULL)
		XtFree(date_start);
	if (date_end != NULL)
		XtFree(date_end);
}

/*******************************************************************
** 
** viewa_query_cb
**
*******************************************************************/

static void viewa_query_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	char *platform;
	char *date_start = NULL;
	char *date_end = NULL;
	char *rev_start = NULL;
	char *rev_end = NULL;
	int option;
	IMS_NUMERIC_DATE dateStruct;


	/*
	** Get the widget values for the query dialog box.
	*/ 

	if (XmToggleButtonGetState(local_widgets.aq_reported) == TRUE)
	{
		option = REPORTED_ACQUISITIONS;
	}
	else
	{
		option = UNREPORTED_ACQUISITIONS;

	}


	switch (local_widgets.platform_id)
	{
		case 1:
			platform = "<ALL>";
			break;
		case 2:
			platform = "A1";
			break;
		case 3:
			platform = "E1";
			break;
		case 4:
			platform = "E2";
			break;
		case 5:
			platform = "J1";
			break;
		case 6:
			platform = "R1";
			break;
	}

	/*
	** Check if date range provided.
	*/

	if (XmToggleButtonGetState(local_widgets.date_range) == TRUE)
	{
		date_start = XmTextGetString(local_widgets.date_from);
		date_end = XmTextGetString(local_widgets.date_to);
	}

	/*
	** Check if rev range provided.
	*/
	if (XmToggleButtonGetState(local_widgets.rev_range) == TRUE)
	{
		rev_start = XmTextGetString(local_widgets.rev_from);
		rev_end = XmTextGetString(local_widgets.rev_to);
	}

	/*
	** Need to validate the dates here.
	*/

	if (XmToggleButtonGetState(local_widgets.date_range) == TRUE)
	{
	    if (ims_timeToNumericDate(glbl_msgDesc,
				date_start, &dateStruct) < IMS_OK)
		{
			msg_box(app_shell, IMS_ERROR, "Error", 
				"'From' Time not in YYYY-DDDTHH:MM:SS.mss format.");
			goto free_and_exit;
		}

   		if (ims_timeToNumericDate(glbl_msgDesc,
			date_end, &dateStruct) < IMS_OK)
		{
			msg_box(app_shell,  IMS_ERROR, "Error",
				"'To' Time not in YYYY-DDDTHH:MM:SS.mss format.");
			goto free_and_exit;
		}
	} 

	/*
	** Okay, go grab the acquisitions and display the results.
	*/

	showAcquisitionList((Widget) client_d, option,
		platform, date_start, date_end, rev_start, rev_end);

free_and_exit:
	
	if (date_start != NULL)
		XtFree(date_start);
	if (date_end != NULL)
		XtFree(date_end);
	if (rev_start != NULL)
		XtFree(rev_start);
	if (rev_end != NULL)
		XtFree(rev_end);
}


/*******************************************************************
** 
** showAcquisitionList
**
*******************************************************************/

static void showAcquisitionList(
		Widget wshell,
		int option,
		char *platform,
		char *date_start,
		char *date_end,
		char *rev_start,
		char *rev_end)
{
	Widget bboard, list_w;
	Widget pshell;
	Widget close_button;
	Arg args[7];
	XmString t;
	char buffer[IMS_COL255_LEN+1];
	char platbuf[IMS_COL30_LEN+1];
	char namebuf[IMS_COL80_LEN+10];
	int orbit, segment; 
	short int segment_temp;
	char start_time[IMS_COL30_LEN+1];
	int granule_idx;
	char qbuf[IMS_COL512_LEN+1];
	int where_clause = FALSE;
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc = glbl_msgDesc;
	int status;
	XmFontList fontlist;

	XtSetArg(args[0], XmNx, 0);
	XtSetArg(args[1], XmNy, 0);
	XtSetArg(args[2], XmNwidth, IMS_FA_VAL_X2);	
	XtSetArg(args[3], XmNheight, IMS_FA_VAL_Y2);	

	/*
	** Build the title buffer to show query parameters.
	*/ 

	if (option == REPORTED_ACQUISITIONS)
	{
		sprintf(buffer,
			"Reported Acquisitions for platform=%s, ",
			platform); 
		sprintf(qbuf,
			"select platform, name, orbit, sequence, start_time, granule_idx \
			from fa_tracking_history ");

	}			
	else
	{
		sprintf(buffer,
			"Unreported Acquisitions for platform=%s, ",
			platform);  
		/* R2.1 Change */
		sprintf(qbuf,
			"select platform, name, orbit, sequence, start_time, -1 from\
			fa_tracking ");

	}


	if (date_start != NULL)
	{
		strcat(buffer, "From ");
		strcat(buffer, date_start);
		strcat(buffer, " To ");
		strcat(buffer, date_end);
		strcat(buffer, ", ");

		strcat(qbuf, "where start_time >= '");
		strcat(qbuf, date_start);
		strcat(qbuf, "' and start_time <= '");
		strcat(qbuf, date_end);
		strcat(qbuf, "' ");
		where_clause = TRUE;

	}
	else
	{
		strcat(buffer,"All Dates, ");
	}

	if (rev_start != NULL)
	{
		strcat(buffer, "From ");
		strcat(buffer, rev_start);
		strcat(buffer, " To ");
		strcat(buffer, rev_end);

		if (where_clause == FALSE)
			strcat(qbuf, "where orbit >= ");
		else
			strcat(qbuf, "and orbit >= ");

		strcat(qbuf, rev_start);
		strcat(qbuf, " and orbit <= ");
		strcat(qbuf, rev_end);
		strcat(qbuf, " ");
		where_clause = TRUE;
	}
	else
	{
		strcat(buffer,"All Revolutions ");
	}

	if (strcmp(platform, "<ALL>") != 0)
	{
		if (where_clause == FALSE)
		{
			strcat(qbuf, " where platform = '");
		}
		else
		{
			strcat(qbuf, " and platform = '");
		}
		strcat(qbuf, platform);
		strcat(qbuf, "'");

	}
	strcat (qbuf, " order by platform, start_time, orbit");

	/*
	** Login to the DBMS before we create the window.
	** Allocate a query descriptor and login.
	*/
	if ((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
				"Could not allocate a query descriptor.");
		ims_msgStructFree(msgDesc);
		exit(0);
	}

	qDesc->cmd  = &qbuf[0];


	IMS_SETUSER (qDesc, userSpec.username);
	IMS_SETPSWD (qDesc, userSpec.password);
	IMS_SET_VERBOSE (qDesc, 10);
		 
	if (userSpec.server[0] != '\0')
		IMS_SETSERVER(qDesc, userSpec.server);
				  
	if (userSpec.database[0] != '\0')
		IMS_SETDBNAME(qDesc, userSpec.database);
								   
	if (ims_qiLogin(qDesc) < IMS_OK)
	{
		msg_box(wshell,  IMS_ERROR, "Error", "Unable to Access Database");
		ims_qiFreeDesc(qDesc);
		return;
	}

	/*
	** Create a popup shell for the popup window and the bulletin
	** board widget to attach the list window and buttons too.
	*/

    fontlist = loadListFont(wshell, "listFont");

	pshell = XtCreatePopupShell(buffer,
		transientShellWidgetClass, wshell, args, 4);
	bboard = XtVaCreateManagedWidget("bboard", xmBulletinBoardWidgetClass,
		pshell, args, 4, NULL);

	local_widgets.dialog = pshell;

	/*
	** Create the headers for the list
	*/

	sprintf(buffer, "%-10s%-32s%-7s%-8s%-23s%-12s         ",
		"PLATFORM", "NAME", "REV",  "SEQ", "DATE", "GRANULE ID");

	t = XmStringCreateSimple(buffer);

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNlabelString, t,
		XmNfontList, fontlist,
		XmNx, 12,
		XmNy, 0, NULL);

	XmStringFree(t);
	

	/*
	** Create a list window.
	*/

	XtSetArg(args[0], XmNx, 0);
	XtSetArg(args[1], XmNy, 30);
    XtSetArg(args[2], XmNscrollingPolicy, XmAUTOMATIC);
	XtSetArg(args[3], XmNlistSizePolicy, XmCONSTANT);
	XtSetArg(args[4], XmNtraversalOn, False);


	list_w = XmCreateScrolledList(bboard, "Acquisitions", args, 5);

	local_widgets.view_list = list_w;

	XtVaSetValues(list_w, 
		XmNscrollBarDisplayPolicy, XmSTATIC, 
		XmNx, 0,
		XmNfontList, fontlist,
		XmNvisibleItemCount, 20,
		XmNwidth, IMS_FA_VAL_X2 - 40,
		XmNheight,  IMS_FA_VAL_Y2 - 80,
		NULL);

	/*
	** Perform SQL query and get the row information back to 
	** display in the list window.
	*/


	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not perform query of acquisition information");
			msg_box(wshell, IMS_ERROR, "Error",
					"Unable to query acqusitiion information");
			ims_qiFreeDesc(qDesc);
			return;
		}

		if (status == IMS_ENDOFQUERY)
			continue;

		/*
		** Process returned rows...
		*/

		memset(platbuf, 0, sizeof(platbuf));
		memcpy(platbuf, qDesc->valAddr[0], qDesc->valLength[0]);
		ims_trim(platbuf);

		memset(namebuf, 0, sizeof(namebuf));
		memcpy(namebuf, qDesc->valAddr[1], qDesc->valLength[1]);
		ims_trim(namebuf);
		
		memcpy(&orbit, qDesc->valAddr[2], qDesc->valLength[2]);
		memcpy(&segment_temp, qDesc->valAddr[3], qDesc->valLength[3]);
		segment = (int) segment_temp;
		
		memset(start_time, 0, sizeof(start_time));
		memcpy(start_time, qDesc->valAddr[4], qDesc->valLength[4]);
		ims_trim(start_time);

		memcpy(&granule_idx, qDesc->valAddr[5], qDesc->valLength[5]);

		sprintf(buffer, "%-10s%-32s%-7d%-8d%-23s%-12d         ",
			platbuf, namebuf, orbit, segment, start_time, granule_idx);
		
		t = XmStringCreateSimple(buffer);
		XmListAddItem(list_w, t, 0);
		XmStringFree(t);

	}

	ims_qiFreeDesc(qDesc);



	/*
	** Create buttons
	*/
	
	close_button = XtVaCreateManagedWidget( "   CLOSE   ",
		xmPushButtonWidgetClass, bboard,
		XmNfontList, fontlist,
		XmNx, IMS_FA_VAL_X2/2 - 40, XmNy, IMS_FA_VAL_Y2 - 30,
		NULL);

	XtAddCallback(close_button, XmNactivateCallback,
		view_cancel_cb, (void *) pshell);

	/*
	** Manage the popup shell widget.
	*/

	XtManageChild(list_w);
	XtManageChild(pshell);
	XtRealizeWidget(app_shell);
	XtPopup(pshell, XtGrabExclusive);
}


/*******************************************************************
** 
** valid_date_cb
**
** Validate the start date and add the delimiters
** Format: YYYY-DDDTHH:MM:SS.MSS
*******************************************************************/

static void valid_date_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	char c;
	int len;
	XmTextVerifyCallbackStruct *cbs;
	char *glbl_data;

	/*
	** User is typing so toggle button state
	*/

	XmToggleButtonSetState(local_widgets.date_range,
		TRUE, FALSE);

	glbl_data = (char *) client_d;
	len = XmTextGetLastPosition(w);
	cbs =  (XmTextVerifyCallbackStruct *) call_d;

	if (cbs->reason == XmCR_MOVING_INSERT_CURSOR)
	{
		if (cbs->newInsert != len)
			cbs->doit = False;
		return;
	}

	/*
	** Prohibit backspace, typing or stuffing in middle of string.
	*/

	if (cbs->currInsert < len)
	{
		cbs->doit = False;
		return;
	}

	/*
	** When backspacing make sure delimiter is removed as well.
	*/

	if (cbs->text->length == 0)
	{
		if (cbs->startPos == 4 ||
			cbs->startPos == 8 || cbs->startPos ==  11 ||
			cbs->startPos == 14 || cbs->startPos == 17)
		{
			cbs->startPos --;
		}
		return;
	}

	/*
	** Only allow single text entries, no clipboard pastes.
	*/

	if (cbs->text->length > 1)
	{
		cbs->doit = False;
		return;
	}

	/*
	** Don't allow non-digits or let the input exceed maxlength of
	** IMS_DATETIME_LEN
	*/

	if (!isdigit (c = cbs->text->ptr[0]) || len >= IMS_DATETIME_LEN)
	{
		cbs->doit = False;
	}
	else if (cbs->startPos == 3 ||
			cbs->startPos == 7 || cbs->startPos ==  10 ||
			cbs->startPos == 13 || cbs->startPos == 16)
	{
		cbs->text->ptr = XtRealloc(cbs->text->ptr, 2);
		cbs->text->length = 2;
		cbs->text->ptr[0] = c;

		switch (cbs->startPos)
		{
			case 3:
				cbs->text->ptr[1] = '-';
				break;
			case 7:
				cbs->text->ptr[1] = 'T';
				break;
			case 10:
				cbs->text->ptr[1] = ':';
				break;
			case 13:
				cbs->text->ptr[1] = ':';
				break;
			case 16:
				cbs->text->ptr[1] = '.';
				break;
		}
	}

}


/*******************************************************************
** 
** valid_rev_cb
**
*******************************************************************/

static void valid_rev_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	char c;
	int len;
	XmTextVerifyCallbackStruct *cbs;

	/*
	** User is typing so toggle button state
	*/

	XmToggleButtonSetState(local_widgets.rev_range,
		TRUE, FALSE);
}

/*******************************************************************
** 
** valid_name_cb
**
*******************************************************************/

static void valid_name_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	char c;
	int len;
	XmTextVerifyCallbackStruct *cbs;

	/*
	** User is typing so toggle button state
	*/

	XmToggleButtonSetState(local_widgets.name_range,
		TRUE, FALSE);
}


/*******************************************************************
** 
** showHistoryScreen
**
*******************************************************************/

void showHistoryScreen(
		Widget wshell)
{
	Widget bboard, option, date_from, date_to;
	Widget pshell, date_range, report_name;
	Widget query_button, cancel_button;
	Arg args[7];
	XmString t;
	XmString reports[16];
	XmFontList fontlist;
	int i;

	XtSetArg(args[0], XmNx, 0);
	XtSetArg(args[1], XmNy, 0);
	XtSetArg(args[2], XmNwidth, IMS_FA_HIS_X2);	
	XtSetArg(args[3], XmNheight, IMS_FA_HIS_Y2);	

	pshell = XtCreatePopupShell("View History Information",
		transientShellWidgetClass, wshell, args, 4);

	bboard = XtVaCreateManagedWidget("bboard", xmBulletinBoardWidgetClass,
		pshell, args, 4, NULL);

        fontlist = loadDialogFont(wshell, "dialogFont");
        local_widgets.report_id = 0;
	/*
	** Display window options
	*/
	t = XmStringCreateSimple("Report Type:");
	reports[0] = XmStringCreateSimple("<All Report Types>");
	reports[1] = XmStringCreateSimple("RADARSAT-1 Archive Storage Report");
	reports[2] = XmStringCreateSimple("RADARSAT-1 Reception Report");
	reports[3] = XmStringCreateSimple("RESERVED");
	reports[4] = XmStringCreateSimple("ERS-1 Shipment Report (RESM)");
	reports[5] = XmStringCreateSimple("ERS-1 Acquisition Report (REAQ)");
	reports[6] = XmStringCreateSimple("ERS-1 Extracted Data Report (REEX)");
	reports[7] = XmStringCreateSimple("JERS-1 Catalog Report (CATA)");
	reports[8] = XmStringCreateSimple("JERS-1 Acquisition Report (REAC)");
	reports[9] = XmStringCreateSimple("JERS-1 Shipment Report (MSGM)");
	reports[10] = XmStringCreateSimple("ADEOS-1 Acquisition Report (REAC)");
	reports[11] = XmStringCreateSimple("ADEOS-1 Shipment Report (SRRD)");
	reports[12] = XmStringCreateSimple("ERS-2 Shipment Report (RESM)");
	reports[13] = XmStringCreateSimple("ERS-2 Acquisition Report (REAQ)");
	reports[14] = XmStringCreateSimple("ERS-2 Extracted Data Report (REEX)");

	local_widgets.reports = XmVaCreateSimpleOptionMenu(bboard, "option2",
		t, 0, 0, (XtCallbackProc) reports_cb,
		XmVaPUSHBUTTON, reports[0], NULL, NULL, NULL,
		XmVaPUSHBUTTON, reports[1], NULL, NULL, NULL,
		XmVaPUSHBUTTON, reports[2], NULL, NULL, NULL,
		XmVaPUSHBUTTON, reports[3], NULL, NULL, NULL,
		XmVaPUSHBUTTON, reports[4], NULL, NULL, NULL,
		XmVaPUSHBUTTON, reports[5], NULL, NULL, NULL,
		XmVaPUSHBUTTON, reports[6], NULL, NULL, NULL,
		XmVaPUSHBUTTON, reports[7], NULL, NULL, NULL,
		XmVaPUSHBUTTON, reports[8], NULL, NULL, NULL,
		XmVaPUSHBUTTON, reports[9], NULL, NULL, NULL,
		XmVaPUSHBUTTON, reports[10], NULL, NULL, NULL,
		XmVaPUSHBUTTON, reports[11], NULL, NULL, NULL,
		XmVaPUSHBUTTON, reports[12], NULL, NULL, NULL,
		XmVaPUSHBUTTON, reports[13], NULL, NULL, NULL,
		XmVaPUSHBUTTON, reports[14], NULL, NULL, NULL,
		XmNfontList, fontlist,
		XmNy, IMS_FA_HIS_REPORT_Y1, 
		NULL);

	XtManageChild(local_widgets.reports);


	XmStringFree(t);
	for (i = 0; i < 15; i++)
	{
		XmStringFree(reports[i]);
	}

	/*
	** Make date range setting radio box and labels.
	*/

	t = XmStringCreateSimple("Date Range:");

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNlabelString, t,
		XmNfontList, fontlist,
		XmNx, IMS_FA_HIS_DATELAB_X1,
		XmNy, IMS_FA_HIS_DATELAB_Y1, NULL);


	XmStringFree(t);

	date_range = 
		XmCreateRadioBox(bboard, "date_range", NULL, 0);

	XtVaSetValues(date_range, XmNnumColumns, 2, 
		XmNx, IMS_FA_HIS_DATERADIO_X1,
		XmNy, IMS_FA_HIS_DATERADIO_Y1,
		XmNradioAlwaysOne, TRUE,
		XmNfontList, fontlist,
		NULL);

	local_widgets.date_all =  XtVaCreateManagedWidget("All", 
		xmToggleButtonGadgetClass, date_range, NULL);

	local_widgets.date_range = XtVaCreateManagedWidget("From:", 
		xmToggleButtonGadgetClass, date_range, NULL);

	local_widgets.date_from = XtVaCreateManagedWidget("text",
		xmTextWidgetClass, bboard, XmNeditable, TRUE,
		XmNwidth, 140, 
		XmNmaxLength, 21, 
		XmNx, IMS_FA_HIS_DATEFROM_X1+14,/*r2.1*/
		XmNy, IMS_FA_HIS_DATEFROM_Y1,
		XmNfontList, fontlist,
		NULL);

	XtAddCallback(local_widgets.date_from,  XmNmodifyVerifyCallback,
		valid_date_cb, NULL);


	t = XmStringCreateSimple("To:");

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNlabelString, t,
		XmNfontList, fontlist,
		XmNx, IMS_FA_HIS_DATETOLAB_X1+10, /*r2.1*/
		XmNy, IMS_FA_HIS_DATETOLAB_Y1, NULL);


	XmStringFree(t);

	local_widgets.date_to = XtVaCreateManagedWidget("text",
		xmTextWidgetClass, bboard, XmNeditable, TRUE,
		XmNfontList, fontlist,
		XmNwidth, 140, 
		XmNmaxLength, 21, 
		XmNx, IMS_FA_HIS_DATETO_X1+12, /*r2.1*/
		XmNy, IMS_FA_HIS_DATETO_Y1,
		NULL);

	XtAddCallback(local_widgets.date_to,  XmNmodifyVerifyCallback,
		valid_date_cb, NULL);

	XtManageChild(date_range);

	XmToggleButtonSetState(local_widgets.date_all,
		TRUE, FALSE);

	/*
	** Name radio box
	*/

	t = XmStringCreateSimple("Report Name:");

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNlabelString, t,
		XmNfontList, fontlist,
		XmNx, IMS_FA_HIS_NAMELAB_X1,
		XmNy, IMS_FA_HIS_NAMELAB_Y1, NULL);

	XmStringFree(t);

	report_name = 
		XmCreateRadioBox(bboard, "report_name", NULL, 0);

	XtVaSetValues(report_name, XmNnumColumns, 2, 
		XmNfontList, fontlist,
		XmNx, IMS_FA_HIS_NAMERADIO_X1,
		XmNy, IMS_FA_HIS_NAMERADIO_Y1,
		XmNradioAlwaysOne, TRUE,
		NULL);

	local_widgets.name_all =  XtVaCreateManagedWidget("All", 
		xmToggleButtonGadgetClass, report_name, NULL);

	local_widgets.name_range = XtVaCreateManagedWidget("Exact:", 
		xmToggleButtonGadgetClass, report_name, NULL);

	local_widgets.name_from = XtVaCreateManagedWidget("text",
		xmTextWidgetClass, bboard, XmNeditable, TRUE,
		XmNfontList, fontlist,
		XmNwidth, 200, 
		XmNmaxLength, 30, 
		XmNx, IMS_FA_HIS_NAMEFROM_X1+14,/*r2.1*/
		XmNy, IMS_FA_HIS_NAMEFROM_Y1,
		NULL);

	XtAddCallback(local_widgets.name_from,  XmNmodifyVerifyCallback,
		valid_name_cb, NULL);

	XtManageChild(report_name);

	XmToggleButtonSetState(local_widgets.name_all,
		TRUE, FALSE);

	/*
	** Add the buttons for QUERY and CANCEL
	*/


	query_button = XtVaCreateManagedWidget( "   QUERY   ",
		xmPushButtonWidgetClass, bboard,
		XmNfontList, fontlist,
		XmNx, IMS_FA_HIS_QUERY_X1, XmNy, IMS_FA_HIS_QUERY_Y1,
		NULL);

	cancel_button = XtVaCreateManagedWidget( "   CANCEL   ",
		xmPushButtonWidgetClass, bboard,
		XmNfontList, fontlist,
		XmNx, IMS_FA_HIS_CANCEL_X1, XmNy, IMS_FA_HIS_CANCEL_Y1,
		NULL);

	/*
	** Add the callbacks.
	*/

	XtAddCallback(cancel_button, XmNactivateCallback,
		view_cancel_cb, (void *) pshell);

	XtAddCallback(query_button, XmNactivateCallback,
		viewh_query_cb, (void *) pshell);

	/*
	** Manage the popup shell widget.
	*/

	/*
	** Manage the popup shell widget.
	*/

	XtManageChild(pshell);
	XtRealizeWidget(app_shell);
	XtPopup(pshell, XtGrabExclusive);


}

/*******************************************************************
** 
** reports_cb
**
*******************************************************************/

static void reports_cb(
	Widget w,
	int item_no,
	XtPointer call_d)
{
	local_widgets.report_id = item_no;

}

/*******************************************************************
** 
** showHistoryList
**
*******************************************************************/

static void showHistoryList(
		Widget wshell,
		short int report_id,
		char *report_name,
		char *date_start,
		char *date_end,
		char *name)
{
	Widget bboard, list_w;
	Widget pshell;
	Widget close_button, viewm_button, viewr_button;
	Arg args[7];
	XmString t;
	char buffer[IMS_COL255_LEN+1];
	char namebuf[IMS_COL80_LEN+10];
	short int dataset_idx;
	int granule_idx;/*R2.1 fixed */
	char report_archived, report_sent, report_attempt;
	char archive_time[IMS_COL30_LEN+1];
	char qbuf[IMS_COL512_LEN+1];
	int where_clause = FALSE;
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc = glbl_msgDesc;
	int status;
	char tempbuf[32];
	char gentime[IMS_COL30_LEN+1];
	DATALIST *datahdr = NULL, *dataptr = NULL;   
	int rowCount = 0;
	XmFontList fontlist;

	XtSetArg(args[0], XmNx, 0);
	XtSetArg(args[1], XmNy, 0);
	XtSetArg(args[2], XmNwidth, IMS_FA_HISL_X2);	
	XtSetArg(args[3], XmNheight, IMS_FA_HISL_Y2);	

	/*
	** Build the title buffer to show query parameters.
	*/ 
	sprintf(buffer,
		"Report History for Report=%s, ",report_name);

	sprintf(qbuf,
		"select report_id, name, dataset_idx,  report_attempt, \
		report_fasent, report_archived, convert(char(24), gen_time), \
		granule_idx from \
		fa_report_history ");



	if (date_start != NULL)
	{
		strcat(buffer, "From ");
		strcat(buffer, date_start);
		strcat(buffer, " To ");
		strcat(buffer, date_end);
		strcat(buffer, ", ");

		strcat(qbuf, "where gen_time >= '");
		strcat(qbuf, date_start);
		strcat(qbuf, "' and gen_time <= '");
		strcat(qbuf, date_end);
		strcat(qbuf, "' ");
		where_clause = TRUE;

	}
	else
	{
		strcat(buffer,"All Dates, ");
	}

	if (name != NULL)
	{
		if (where_clause == FALSE)
			strcat(qbuf, "where name = '");
		else
			strcat(qbuf, "and name = '");

		strcat(qbuf, name);
		strcat(qbuf, "' ");
		where_clause = TRUE;
	}
	else
	{
		strcat(buffer,"All Names ");
	}

	if (report_id > 0)
	{
		if (where_clause == FALSE)
		{
			strcat(qbuf, " where report_id = ");
		}
		else
		{
			strcat(qbuf, " and report_id = ");
		}
		sprintf(tempbuf, "%d", report_id);
		strcat(qbuf, tempbuf);
		strcat(qbuf, " ");

	}
	strcat (qbuf, " order by report_id, gen_time");


	/*
	** Login to the DBMS before we create the window.
	** Allocate a query descriptor and login.
	*/
	if ((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
				"Could not allocate a query descriptor.");
		ims_msgStructFree(msgDesc);
		exit(0);
	}

	qDesc->cmd  = &qbuf[0];


	IMS_SETUSER (qDesc, userSpec.username);
	IMS_SETPSWD (qDesc, userSpec.password);
	IMS_SET_VERBOSE (qDesc, 10);
		 
	if (userSpec.server[0] != '\0')
		IMS_SETSERVER(qDesc, userSpec.server);
				  
	if (userSpec.database[0] != '\0')
		IMS_SETDBNAME(qDesc, userSpec.database);
								   
	if (ims_qiLogin(qDesc) < IMS_OK)
	{
		msg_box(wshell,  IMS_ERROR, "Error", "Unable to Access Database");
		ims_qiFreeDesc(qDesc);
		return;
	}

	/*
	** Create a popup shell for the popup window and the bulletin
	** board widget to attach the list window and buttons too.
	*/

    fontlist = loadListFont(wshell, "listFont");

	pshell = XtCreatePopupShell(buffer,
		transientShellWidgetClass, wshell, args, 4);
	bboard = XtVaCreateManagedWidget("bboard", xmBulletinBoardWidgetClass,
		pshell, args, 4, NULL);

	local_widgets.dialog = pshell;

	/*
	** Create the headers for the list
	*/

	sprintf(buffer, "%-15s%-32s%-23s%-12s%-12s%-12s",
		"REPORT TYPE", "FILE NAME", "GENERATION TIME",  "ATTEMPTED", "ARCHIVED", "SENT");

	t = XmStringCreateSimple(buffer);

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNlabelString, t,
		XmNfontList, fontlist,
		XmNx, 12,
		XmNy, 0, NULL);

	XmStringFree(t);
	

	/*
	** Create a list window.
	*/

	XtSetArg(args[0], XmNx, 0);
	XtSetArg(args[1], XmNy, 30);
    XtSetArg(args[2], XmNscrollingPolicy, XmAUTOMATIC);
	XtSetArg(args[3], XmNlistSizePolicy, XmCONSTANT);
	XtSetArg(args[4], XmNtraversalOn, False);


	list_w = XmCreateScrolledList(bboard, "Report_List", args, 5);

	local_widgets.view_list = list_w;

	XtVaSetValues(list_w, 
		XmNscrollBarDisplayPolicy, XmSTATIC, 
		XmNfontList, fontlist,
		XmNvisibleItemCount, 20,
		XmNx, 0,
		XmNwidth, IMS_FA_HISL_X2 - 40,
		XmNheight,  IMS_FA_HISL_Y2 - 80,
		NULL);

	/*
	** Perform SQL query and get the row information back to 
	** display in the list window.
	*/

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not perform query of acquisition information");
			msg_box(wshell, IMS_ERROR, "Error",
					"Unable to query acqusitiion information");
			ims_qiFreeDesc(qDesc);
			return;
		}

		if (status == IMS_ENDOFQUERY)
			continue;

		/*
		** Process returned rows...
		*/

		memcpy(&report_id, qDesc->valAddr[0], qDesc->valLength[0]);

		memset(namebuf, 0, sizeof(namebuf));
		memcpy(namebuf, qDesc->valAddr[1], qDesc->valLength[1]);
		ims_trim(namebuf);
		
		memcpy(&dataset_idx, qDesc->valAddr[2], qDesc->valLength[2]);
		memcpy(&report_attempt, qDesc->valAddr[3], qDesc->valLength[3]);
		memcpy(&report_sent, qDesc->valAddr[4], qDesc->valLength[4]);
		memcpy(&report_archived, qDesc->valAddr[5], qDesc->valLength[5]);
		
		memset(gentime, 0, sizeof(gentime));
		memcpy(gentime, qDesc->valAddr[6], qDesc->valLength[6]);
		ims_trim(gentime);

		memcpy(&granule_idx, qDesc->valAddr[7], qDesc->valLength[7]);

		switch (report_id)
		{
			case 1:
				report_name = "R1 ASR";
				break;
			case 2:
				report_name = "R1 Reception";
				break;
			case 3:
				report_name = "RESERVED";
				break;
			case 4:
				report_name = "ERS-1 RESM";
				break;
			case 5:
				report_name = "ERS-1 REAQ";
				break;
			case 6:
				report_name = "ERS-1 REEX";
				break;
			case 7:
				report_name = "JERS-1 CATA";
				break;
			case 8:
				report_name = "JERS-1 REAC";
				break;
			case 9:
				report_name = "JERS-1 MSGM";
				break;
			case 10:
				report_name = "ADEOS-1 REAC";
				break;
			case 11:
				report_name = "ADEOS-1 SRRD";
				break;
			case 12:
				report_name = "ERS-2 RESM";
				break;
			case 13:
				report_name = "ERS-2 REAQ";
				break;
			case 14:
				report_name = "ERS-2 REEX";
				break;
		}

		
		sprintf(buffer, "%-15s%-32s%-23s%-12c%-12c%-12c",
			report_name, namebuf, gentime,  report_attempt, 
			report_archived, report_sent);
		
		t = XmStringCreateSimple(buffer);
		XmListAddItem(list_w, t, 0);
		XmStringFree(t);

		/*
		** Add dataset and granule indexes to list.
		*/
		rowCount ++;

		if (rowCount == 1)
		{
			datahdr = (void *) malloc(sizeof(DATALIST));
			if (datahdr == NULL)
			{
				msg_box(wshell, IMS_ERROR, "Error",
					"Out of Memory.");  
				ims_msg(msgDesc, IMS_ERROR,
					"Could not allocate space for dataset list.");
				return;
			}

			dataptr = datahdr;
		}
		else
		{
			dataptr->next = (void *) malloc(sizeof(DATALIST));
			if (dataptr->next == NULL)
			{
				msg_box(wshell, IMS_ERROR, "Error",
					"Out of Memory.");  
				ims_msg(msgDesc, IMS_ERROR,
					"Could not allocate space for dataset list.");
				return;
			}
			dataptr = dataptr->next;
		}
		dataptr->granule_idx = (int) granule_idx;
		dataptr->dataset_idx = (int) dataset_idx;
		dataptr->report_id = report_id;
		dataptr->next = NULL;
	}

	ims_qiFreeDesc(qDesc);


	/*
	** Create buttons
	*/
	
	viewm_button = XtVaCreateManagedWidget( "VIEW METADATA",
		xmPushButtonWidgetClass, bboard,
		XmNfontList, fontlist,
		XmNx, IMS_FA_VAL_X2/2 - 180, XmNy, IMS_FA_VAL_Y2 - 30,
		NULL);

	viewr_button = XtVaCreateManagedWidget( " VIEW REPORT ",
		xmPushButtonWidgetClass, bboard,
		XmNfontList, fontlist,
		XmNx, IMS_FA_VAL_X2/2 - 50, XmNy, IMS_FA_VAL_Y2 - 30,
		NULL);

	close_button = XtVaCreateManagedWidget( "   CLOSE   ",
		xmPushButtonWidgetClass, bboard,
		XmNfontList, fontlist,
		XmNx, IMS_FA_VAL_X2/2 + 80, XmNy, IMS_FA_VAL_Y2 - 30,
		NULL);

	XtAddCallback(close_button, XmNactivateCallback,
		view_cancel_cb, (void *) pshell);

	XtAddCallback(viewm_button, XmNactivateCallback,
		view_metadata_cb, (void *) datahdr);

	XtAddCallback(viewr_button, XmNactivateCallback,
		view_report_cb, (void *) datahdr);

	/*
	** Manage the popup shell widget.
	*/

	XtManageChild(list_w);

	XtManageChild(pshell);
	XtRealizeWidget(app_shell);
	XtPopup(pshell, XtGrabExclusive);
	
}


/*******************************************************************
** 
** view_metadata_cb
**
*******************************************************************/

static void view_metadata_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	DATALIST *dataptr = (void *) client_d;
	int *pos_list;
	int pos_cnt;
	int curr_pos;

	/*
	** Get positional information from list window.
	*/

	XmListGetSelectedPos(local_widgets.view_list, &pos_list, &pos_cnt);

	if (pos_cnt == 0)
	{
		msg_box(local_widgets.dialog, IMS_WARNING, "Warning", 
				"You must first select an item.");
		return;
	}

	curr_pos = *pos_list - 1;

	while (curr_pos != 0)
	{
		dataptr = dataptr->next;	
		curr_pos --;
	}

	XtFree((void *) pos_list);

	(void) showMetadata(glbl_msgDesc, dataptr->granule_idx, 
		dataptr->dataset_idx);

}

/*******************************************************************
** 
** view_report_cb
**
*******************************************************************/

static void view_report_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	DATALIST *dataptr = (void *) client_d;
	int *pos_list;
	int pos_cnt;
	int curr_pos;

	/*
	** Get positional information from list window.
	*/

	XmListGetSelectedPos(local_widgets.view_list, &pos_list, &pos_cnt);

	if (pos_cnt == 0)
	{
		msg_box(local_widgets.dialog, IMS_WARNING, "Warning", 
				"You must first select an item.");
		return;
	}

	curr_pos = *pos_list - 1;

	while (curr_pos != 0)
	{
		dataptr = dataptr->next;	
		curr_pos --;
	}

	XtFree((void *) pos_list);

	(void) showReportDetail(glbl_msgDesc, dataptr->report_id,
		dataptr->granule_idx, 
		dataptr->dataset_idx);

}

/*******************************************************************
** 
** showMetadata
**
*******************************************************************/

static int showMetadata(
	IMS_MSG_STRUCT *msgDesc, 
	int granule_idx,
	int dataset_idx)
{
	IMS_QI_DESC_OBJ *qDesc;
	char qbuf[IMS_COL512_LEN+1];
	int start_granule_idx, end_granule_idx;
	char dataPath[IMS_PATH_LEN+1], path[IMS_PATH_LEN+1];
	char filename[IMS_COL30_LEN+1];
	int status;
	int pid;
	char *progPath;

	/*
	** First determine where the file is located...
	*/

	/*
	** Login to the DBMS before we create the window.
	** Allocate a query descriptor and login.
	*/
	if ((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
				"Could not allocate a query descriptor.");
		ims_msgStructFree(msgDesc);
		exit(0);
	}

	qDesc->cmd  = &qbuf[0];


	IMS_SETUSER (qDesc, userSpec.username);
	IMS_SETPSWD (qDesc, userSpec.password);
	IMS_SET_VERBOSE (qDesc, 10);
		 
	if (userSpec.server[0] != '\0')
		IMS_SETSERVER(qDesc, userSpec.server);
				  
	if (userSpec.database[0] != '\0')
		IMS_SETDBNAME(qDesc, userSpec.database);
								   
	if (ims_qiLogin(qDesc) < IMS_OK)
	{
		msg_box(app_shell,  IMS_ERROR, "Error", "Unable to Access Database");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	sprintf(qbuf, "select path, start_granule_idx, end_granule_idx \
		from dataset_path_policy where \
		dataset_idx = %d", dataset_idx);

	memset(dataPath, 0, sizeof(dataPath));

	/*
	** Perform SQL query and get the row information back to 
	** determine the correct path to use.
	*/

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not perform query of path policy information");
			msg_box(app_shell, IMS_ERROR, "Error",
					"Unable to query the path policy information");
			ims_qiFreeDesc(qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
			continue;
		
		memset(path, 0, sizeof(path));

		memcpy(path,
				qDesc->valAddr[0], qDesc->valLength[0]);
		ims_trim(path);

		memcpy(&start_granule_idx, 
				qDesc->valAddr[1], qDesc->valLength[1]);

		if (qDesc->valLength[1] == 0)
			start_granule_idx = -1;

		memcpy(&end_granule_idx, 
				qDesc->valAddr[2], qDesc->valLength[2]);

		if (qDesc->valLength[2] == 0)
			end_granule_idx = -1;
		/*
		** Now, check and see if the start/end granule_idx are in the range
		*/

		 
		if ((start_granule_idx  == -1) &&
			(end_granule_idx == -1))
		{
			strcpy(dataPath, path);
			(void) ims_qiCancel(qDesc);
			break;
		}


		if ((start_granule_idx  <= granule_idx ) &&
			(end_granule_idx == -1))
		{
			strcpy(dataPath, path);
			(void) ims_qiCancel(qDesc);
			break;
		}

		if ((start_granule_idx  <= granule_idx ) &&
			(granule_idx <= end_granule_idx))
		{
			strcpy(dataPath, path);
			(void) ims_qiCancel(qDesc);
			break;
		}

	}

	ims_qiResetDesc(qDesc);

	/*
	** Now if we don't have a path, then we got a problem...
	*/

	if (dataPath[0] == '\0')
	{
				
		msg_box(app_shell, IMS_ERROR, "Error",
			"No Path Definition for the Selected Item");
		ims_msg(msgDesc, IMS_ERROR,
			"Could not find path information for dataset %d granule_idx %d",
				dataset_idx, granule_idx);
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

  /* The following added for R2.1 */
  if ( granule_idx == -1 )
	{
		msg_box(app_shell, IMS_WARNING, "Warning",
			"The selected item is not available from the archive.");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}
	/*
	** Now, get the name of the metadata file.
	*/

	sprintf(qbuf, "select name from granules_%d where granule_idx = %d",
			dataset_idx, granule_idx);

	memset(filename, 0, sizeof(filename));

	/*
	** Perform SQL query and get the row information back to 
	** determine the correct path to use.
	*/

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not perform query of granule table information");
			msg_box(app_shell, IMS_ERROR, "Error",
					"Unable to query the granule information");
			ims_qiFreeDesc(qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
			continue;
		
		memset(filename, 0, sizeof(filename));

		memcpy(filename,
				qDesc->valAddr[0], qDesc->valLength[0]);

		ims_trim(filename);

	}
	ims_qiFreeDesc(qDesc);

	sprintf(path, "%s/%s.M", dataPath, filename);

	progPath = getenv("IMS_EXEC_PATH");

	if (progPath == NULL)
		progPath = ".";

	/*
	** Now run the file viewer.
	*/

	sprintf(qbuf, "%s/ims_fv", progPath);

	sprintf(dataPath, "-F%s", path);

	if (ims_startChild(msgDesc, qbuf,
		"ims_fv", dataPath, NULL) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Unable to load the file viewer for the metadata file.");

		msg_box(app_shell, IMS_ERROR, "Error",
			"Unable to load the file viewer for the metadata file.");
		return(IMS_ERROR);
	}

	return(IMS_OK);
}

/*******************************************************************
** 
** showReportDetail
**
*******************************************************************/

static int showReportDetail(
	IMS_MSG_STRUCT *msgDesc, 
	int report_id,
	int granule_idx,
	int dataset_idx)
{
	IMS_QI_DESC_OBJ *qDesc;
	char qbuf[IMS_COL512_LEN+1];
	int start_granule_idx, end_granule_idx;
	char dataPath[IMS_PATH_LEN+1], path[IMS_PATH_LEN+1];
	char filename[IMS_COL30_LEN+1];
	int status;
	int pid;
	char *progPath, *temp_path;
	char *tempFileName;

	/*
	** If the report selected is E1 or E2 REAQ then we cannot display
	** them because they are binary.
	*/
	if ((report_id == IMS_ESA_REAQ) || (report_id == IMS_ESA2_REAQ))
	{
		msg_box(app_shell, IMS_INFO, "Info",
			"Reports in binary format may not be displayed using this function.");
		return (IMS_OK);
	}

	/*
	** First determine where the file is located...
	*/

	/*
	** Login to the DBMS before we create the window.
	** Allocate a query descriptor and login.
	*/
	if ((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
				"Could not allocate a query descriptor.");
		ims_msgStructFree(msgDesc);
		exit(0);
	}

	qDesc->cmd  = &qbuf[0];


	IMS_SETUSER (qDesc, userSpec.username);
	IMS_SETPSWD (qDesc, userSpec.password);
	IMS_SET_VERBOSE (qDesc, 10);
		 
	if (userSpec.server[0] != '\0')
		IMS_SETSERVER(qDesc, userSpec.server);
				  
	if (userSpec.database[0] != '\0')
		IMS_SETDBNAME(qDesc, userSpec.database);
								   
	if (ims_qiLogin(qDesc) < IMS_OK)
	{
		msg_box(app_shell,  IMS_ERROR, "Error", "Unable to Access Database");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	sprintf(qbuf, "select path, start_granule_idx, end_granule_idx \
		from dataset_path_policy where \
		dataset_idx = %d", dataset_idx);

	memset(dataPath, 0, sizeof(dataPath));

	/*
	** Perform SQL query and get the row information back to 
	** determine the correct path to use.
	*/

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not perform query of path policy information");
			msg_box(app_shell, IMS_ERROR, "Error",
					"Unable to query the path policy information");
			ims_qiFreeDesc(qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
			continue;
		
		memset(path, 0, sizeof(path));

		memcpy(path,
				qDesc->valAddr[0], qDesc->valLength[0]);
		ims_trim(path);

		memcpy(&start_granule_idx, 
				qDesc->valAddr[1], qDesc->valLength[1]);

		if (qDesc->valLength[1] == 0)
			start_granule_idx = -1;

		memcpy(&end_granule_idx, 
				qDesc->valAddr[2], qDesc->valLength[2]);

		if (qDesc->valLength[2] == 0)
			end_granule_idx = -1;
		/*
		** Now, check and see if the start/end granule_idx are in the range
		*/

		 
		if ((start_granule_idx  == -1) &&
			(end_granule_idx == -1))
		{
			strcpy(dataPath, path);
			(void) ims_qiCancel(qDesc);
			break;
		}


		if ((start_granule_idx  <= granule_idx ) &&
			(end_granule_idx == -1))
		{
			strcpy(dataPath, path);
			(void) ims_qiCancel(qDesc);
			break;
		}

		if ((start_granule_idx  <= granule_idx ) &&
			(granule_idx <= end_granule_idx))
		{
			strcpy(dataPath, path);
			(void) ims_qiCancel(qDesc);
			break;
		}

	}

	ims_qiResetDesc(qDesc);

	/*
	** Now if we don't have a path, then we got a problem...
	*/

	if (dataPath[0] == '\0')
	{
				
		msg_box(app_shell, IMS_ERROR, "Error",
			"No Path Definition for the Selected Item");
		ims_msg(msgDesc, IMS_ERROR,
			"Could not find path information for dataset %d granule_idx %d",
				dataset_idx, granule_idx);
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

  /* The following added for R2.1 */
  if ( granule_idx == -1 )
	{
		msg_box(app_shell, IMS_WARNING, "Warning",
			"The selected item is not available from the archive.");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}
	/*
	** Now, get the name of the metadata file.
	*/

	sprintf(qbuf, "select name from granules_%d where granule_idx = %d",
			dataset_idx, granule_idx);

	memset(filename, 0, sizeof(filename));

	/*
	** Perform SQL query and get the row information back to 
	** determine the correct path to use.
	*/

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not perform query of granule table information");
			msg_box(app_shell, IMS_ERROR, "Error",
					"Unable to query the granule information");
			ims_qiFreeDesc(qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
			continue;
		
		memset(filename, 0, sizeof(filename));

		memcpy(filename,
				qDesc->valAddr[0], qDesc->valLength[0]);

		ims_trim(filename);

	}
	ims_qiFreeDesc(qDesc);

	sprintf(path, "%s/%s.D", dataPath, filename);


	progPath = getenv("IMS_EXEC_PATH");

	if (progPath == NULL)
		progPath = ".";

	/*
	** Now run the file viewer.
	*/

	sprintf(qbuf, "%s/ims_fv", progPath);

	sprintf(dataPath, "-F%s", path);

	if (ims_startChild(msgDesc, qbuf,
		"ims_fv", dataPath, NULL) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Unable to load the file viewer for the report file.");

		msg_box(app_shell, IMS_ERROR, "Error",
			"Unable to load the file viewer for the report file.");
		return(IMS_ERROR);
	}

#ifdef FA_READER
	/*
	** For now we are skipping the use ims_fa_reader to convert the
	** reports into a keyword equal value format for viewing.
	** This could be implemented as a separate button later on.
	*/

	/*
	** Now, run the ims_fa_reader program and show the results.
	*/

	temp_path =  tmpnam(NULL);

	sprintf(qbuf, "%s/ims_fa_reader %d %s > %s", progPath, 
			report_id, path, temp_path);

	system(qbuf);

	if ( (report_id == IMS_CSA_ARCHSTRGRPT) || (report_id == IMS_CSA_RECRPT) )
	{
		sprintf(qbuf,"cp %s %s", path, temp_path);
		system(qbuf);
	}

	/*
	** Now, run the file viewer 
	*/

	sprintf(qbuf, "%s/ims_fv", progPath);

	sprintf(dataPath, "%s", temp_path);

	tempFileName = malloc(strlen(temp_path)+1);
	strcpy(tempFileName, temp_path);

	if (startXClientJob(msgDesc, qbuf, dataPath, endViewerCb, 
			tempFileName) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not load the fileviewer for the report file.");

		msg_box(app_shell, IMS_ERROR, "Error",
					"Unable to load the file viewer for the report file.");
		return(IMS_ERROR);
	}

#endif /* FA_READER */

	return(IMS_OK);
}



/*******************************************************************
** 
** startXClientJob
**
*******************************************************************/

static int startXClientJob(
	IMS_MSG_STRUCT *msgDesc, 
	char *program,
	char *arguments,
	void (*callback) (),
	char *userData)
{
	int shmid, job_id;
	char *shared_args;
	
	shmid = ims_shm_getid();

	if (ims_shm_create(shmid, strlen(arguments)+1) < 0)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not allocate memory for interface");
		return(IMS_ERROR);
	}

	shared_args = (char *) ims_shm_lock(shmid);

	if (shared_args == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not allocate memory for interface");
		return(IMS_ERROR);
	}

	strcpy(shared_args, arguments);

	(void) ims_shm_unlock(shmid);
	
	if (ims_addJob(msgDesc, &userSpec, -1, -1, &job_id, shmid, -1, 
			callback,  userData) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not add the job");
		return(IMS_ERROR);
	}

	if (ims_startJob(msgDesc, job_id, -1, shmid, program) < IMS_OK)
	{
		return(IMS_ERROR);
	}
	return(IMS_OK);
}

/*******************************************************************
** 
** endViewerCb
**
*******************************************************************/

static void endViewerCb(
	int job_id,
	int report,
	int status,
	int shmid,
	char *data)
{
	/*
	** Zap the temp file used by the fileviewer. 
	*/

	unlink(data);
	free(data);
}

/*******************************************************************
** 
** resend_button_cb
**
*******************************************************************/

static void resend_button_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{

	showResendScreen((Widget) client_d);
}



/*******************************************************************
** 
** resend_button_cb
**
*******************************************************************/

static void showResendScreen(
	Widget wshell)
{
	Widget bboard, list_w;
	Widget pshell;
	Widget close_button, resend_button;
	short int report_id;
	Arg args[7];
	XmString t;
	char buffer[IMS_COL255_LEN+1];
	char namebuf[IMS_COL80_LEN+10];
	char *report_name;
	short int dataset_idx;/*R2.1*/
	int granule_idx;
	char archive_time[IMS_COL30_LEN+1];
	char qbuf[IMS_COL512_LEN+1];
	IMS_QI_DESC_OBJ *qDesc;
	IMS_MSG_STRUCT *msgDesc = glbl_msgDesc;
	DATALIST *datahdr = NULL, *dataptr = NULL;   
	int status;
	char tempbuf[32];
	char gentime[IMS_COL30_LEN+1];
	int rowCount = 0;
	XmFontList fontlist;


	XtSetArg(args[0], XmNx, 0);
	XtSetArg(args[1], XmNy, 0);
	XtSetArg(args[2], XmNwidth, IMS_FA_RESEND_X2);	
	XtSetArg(args[3], XmNheight, IMS_FA_RESEND_Y2);	

	/*
	** Build the title buffer to show query parameters.
	*/ 
	sprintf(buffer,
		"Archived Reports Not Sent to Flight Agency");

	sprintf(qbuf,
		"select report_id, name, dataset_idx, convert(char(24), gen_time), \
		granule_idx from \
		fa_report_history where report_attempt = 'Y' and \
		report_archived = 'Y' and report_fasent = 'N' order by \
		gen_time desc");

	/*
	** Login to the DBMS before we create the window.
	** Allocate a query descriptor and login.
	*/

	if ((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
		(void) ims_msg(msgDesc, IMS_FATAL,
				"Could not allocate a query descriptor.");
		ims_msgStructFree(msgDesc);
		exit(0);
	}

	qDesc->cmd  = &qbuf[0];



	IMS_SETUSER (qDesc, userSpec.username);
	IMS_SETPSWD (qDesc, userSpec.password);
	IMS_SET_VERBOSE (qDesc, 10);
		 
	if (userSpec.server[0] != '\0')
		IMS_SETSERVER(qDesc, userSpec.server);
				  
	if (userSpec.database[0] != '\0')
		IMS_SETDBNAME(qDesc, userSpec.database);
								   
	if (ims_qiLogin(qDesc) < IMS_OK)
	{
		msg_box(wshell,  IMS_ERROR, "Error", "Unable to Access Database");
		ims_qiFreeDesc(qDesc);
		return;
	}

	/*
	** Create a popup shell for the popup window and the bulletin
	** board widget to attach the list window and buttons too.
	*/

    fontlist = loadListFont(wshell, "listFont");

	pshell = XtCreatePopupShell(buffer,
		transientShellWidgetClass, wshell, args, 4);
	bboard = XtVaCreateManagedWidget("bboard", xmBulletinBoardWidgetClass,
		pshell, args, 4, NULL);

	local_widgets.dialog = pshell;

	/*
	** Create the headers for the list
	*/

	sprintf(buffer, "%-15s%-32s%-23s%-14s%",
		"REPORT NAME", "FILE NAME", "GENERATION TIME",  "DATASET IDX");

	t = XmStringCreateSimple(buffer);

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bboard,
		XmNlabelString, t,
		XmNfontList, fontlist,
		XmNx, 12,
		XmNy, 0, NULL);

	XmStringFree(t);
	

	/*
	** Create a list window.
	*/

	XtSetArg(args[0], XmNx, 0);
	XtSetArg(args[1], XmNy, 30);
    XtSetArg(args[2], XmNscrollingPolicy, XmAUTOMATIC);
	XtSetArg(args[3], XmNlistSizePolicy, XmCONSTANT);
	XtSetArg(args[4], XmNtraversalOn, False);


	list_w = XmCreateScrolledList(bboard, "Report_List", args, 5);

	local_widgets.send_list = list_w;

	XtVaSetValues(list_w, 
		XmNscrollBarDisplayPolicy, XmSTATIC, 
		XmNfontList, fontlist,
		XmNvisibleItemCount, 20,
		XmNx, 0,
		XmNwidth, IMS_FA_RESEND_X2 - 40,
		XmNheight,  IMS_FA_RESEND_Y2 - 80,
		NULL);

	/*
	** Perform SQL query and get the row information back to 
	** display in the list window.
	*/

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not perform query of fa_report_history information");
			msg_box(wshell, IMS_ERROR, "Error", "Could not query table.");
		}

		if (status == IMS_ENDOFQUERY)
			continue;

		/*
		** Process returned rows...
		*/

		memcpy(&report_id, qDesc->valAddr[0], qDesc->valLength[0]);

		memset(namebuf, 0, sizeof(namebuf));
		memcpy(namebuf, qDesc->valAddr[1], qDesc->valLength[1]);
		ims_trim(namebuf);
		
		memcpy(&dataset_idx, qDesc->valAddr[2], qDesc->valLength[2]);
		
		memset(gentime, 0, sizeof(gentime));
		memcpy(gentime, qDesc->valAddr[3], qDesc->valLength[3]);
		ims_trim(gentime);

		memcpy(&granule_idx, qDesc->valAddr[4], qDesc->valLength[4]);

		switch (report_id)
		{
			case 1:
				report_name = "R1 ASR";
				break;
			case 2:
				report_name = "R1 Reception";
				break;
			case 3:
				report_name = "RESERVED";
				break;
			case 4:
				report_name = "ERS-1 RESM";
				break;
			case 5:
				report_name = "ERS-1 REAQ";
				break;
			case 6:
				report_name = "ERS-1 REEX";
				break;
			case 7:
				report_name = "JERS-1 CATA";
				break;
			case 8:
				report_name = "JERS-1 REAC";
				break;
			case 9:
				report_name = "JERS-1 MSGM";
				break;
			case 10:
				report_name = "ADEOS-1 REAC";
				break;
			case 11:
				report_name = "ADEOS-1 SRRD";
				break;
			case 12:
				report_name = "ERS-2 RESM";
				break;
			case 13:
				report_name = "ERS-2 REAQ";
				break;
			case 14:
				report_name = "ERS-2 REEX";
				break;
			default:
				(void) ims_msg(msgDesc, IMS_WARNING,
					"Report ID %d unknown in fa_report_history table.",
					report_id);
		}

		
		sprintf(buffer, "%-15s%-32s%-23s%-14d%",
			report_name, namebuf, gentime,  dataset_idx);
		
		t = XmStringCreateSimple(buffer);
		XmListAddItem(list_w, t, 0);
		XmStringFree(t);

		/*
		** Add dataset and granule indexes to list.
		*/
		rowCount ++;

		if (rowCount == 1)
		{
			datahdr = (void *) malloc(sizeof(DATALIST));
			if (datahdr == NULL)
			{
				msg_box(wshell, IMS_ERROR, "Error",
					"Out of Memory.");  
				ims_msg(msgDesc, IMS_ERROR,
					"Could not allocate space for dataset list.");
				return;
			}

			dataptr = datahdr;
		}
		else
		{
			dataptr->next = (void *) malloc(sizeof(DATALIST));
			if (dataptr->next == NULL)
			{
				msg_box(wshell, IMS_ERROR, "Error",
					"Out of Memory.");  
				ims_msg(msgDesc, IMS_ERROR,
					"Could not allocate space for dataset list.");
				return;
			}
			dataptr = dataptr->next;
		}
		dataptr->granule_idx = granule_idx;
		dataptr->dataset_idx = (int) dataset_idx;
		dataptr->report_id = report_id;
		dataptr->next = NULL;
	}

	ims_qiFreeDesc(qDesc);


	/*
	** Create buttons
	*/
	
#if 0
	resend_button = XtVaCreateManagedWidget( "UNSENT REPORTS",
		xmPushButtonWidgetClass, bboard,
		XmNfontList, fontlist,
		XmNx, IMS_FA_RESEND_X2/2 - 140, XmNy, IMS_FA_RESEND_Y2 - 30,
		NULL);
#endif


	close_button = XtVaCreateManagedWidget( "   CLOSE   ",
		xmPushButtonWidgetClass, bboard,
		XmNfontList, fontlist,
		XmNx, IMS_FA_RESEND_X2/2 - 60, XmNy, IMS_FA_RESEND_Y2 - 30,
		NULL);

#if 0
	XtAddCallback(resend_button, XmNactivateCallback,
		resend_report_cb, (void *) datahdr);
#endif

	XtAddCallback(close_button, XmNactivateCallback,
		view_cancel_cb, (void *) pshell);

	/*
	** Manage the popup shell widget.
	*/

	XtManageChild(list_w);

	XtManageChild(pshell);
	XtRealizeWidget(app_shell);
	XtPopup(pshell, XtGrabExclusive);
}

/*******************************************************************
** 
** resend_report_cb
**
*******************************************************************/

static void resend_report_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	Widget parent;
	DATALIST *dataptr = (void *) client_d;
	int *pos_list;
	int pos_cnt;
	int curr_pos;
	Cursor watch;

	
	/*
	** Set cursor to busy.
	*/
	watch = XCreateFontCursor(XtDisplay(local_widgets.dialog), XC_watch);
	XDefineCursor(XtDisplay(local_widgets.dialog), 
			XtWindow(local_widgets.dialog), watch);
	XFreeCursor(XtDisplay(local_widgets.dialog), watch);


	/*
	** Get positional information from list window.
	*/

	XmListGetSelectedPos(local_widgets.send_list, &pos_list, &pos_cnt);

	if (pos_cnt == 0)
	{
		msg_box(local_widgets.dialog, IMS_WARNING, "Warning", 
				"You must first select an item.");
		XUndefineCursor(XtDisplay(local_widgets.dialog), 
				XtWindow(local_widgets.dialog));
		return;
	}

	curr_pos = *pos_list - 1;

	while (curr_pos != 0)
	{
		printf("Looking at report %d for dataset %d granule %d \n", 
			dataptr->report_id,
			dataptr->dataset_idx,
			dataptr->granule_idx);

		dataptr = dataptr->next;	
		curr_pos --;
	}

	XtFree((void *) pos_list);

	printf("Resend report %d for dataset %d granule %d \n", 
		dataptr->report_id,
		dataptr->dataset_idx,
		dataptr->granule_idx);


/*
	(void) showReportDetail(glbl_msgDesc, dataptr->report_id,
		dataptr->granule_idx, 
		dataptr->dataset_idx);
*/

	/*
	** Set cursor to ready.
	*/

	parent = XtParent (local_widgets.dialog);
	XUndefineCursor(XtDisplay(local_widgets.dialog), 
			XtWindow(local_widgets.dialog));

	XtPopdown((Widget) local_widgets.dialog);
	showResendScreen(parent);
}



