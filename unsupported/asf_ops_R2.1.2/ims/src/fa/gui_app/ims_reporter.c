static char *sccs = "@(#)ims_reporter.c	6.1 03/13/98";

/*****************************************************************************
*
**
** File:    ims_reporter.c
**
** Function: Allow user to start FA Reports through a GUI interface.
**		
**
** Author: Dan Crichton
**
** Date:    8/28/95
**
**	3/12/98 J. Ho
**      PR2957 - generate CSA archive storage report only on 
**	         "ARCHIVE_SIGNAL" tapes for "FA" and "MC".
**      PR2878 - perform correct "report-type" in view HISTORY
**
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>
#include <sys/signal.h>

#include <X11/Intrinsic.h>
#include <Xm/RowColumn.h>
#include <Xm/BulletinB.h>
#include <Xm/SelectioB.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/List.h>
#include <Xm/MessageB.h>
#include <Xm/Scale.h>
#include <Xm/DialogS.h>


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

#define PRE_LOGIN	1
#define	LOGIN		2
#define MAIN_WIN	3


/*
** Structure which defines a shipping entry.
*/

typedef struct
{
	char tape_id[IMS_COL15_LEN + 1];
	char ship_date[IMS_DATETIME_LEN + 1];
	Widget list_w;
	Widget parent;
} SHIP_INFO;

/*
** Linked list for tracking outstanding jobs 
*/

typedef struct fa_jobs
{
	int job_id;
	char start_time[25];
	char job_title[IMS_COL30_LEN+1];
	int status;
	struct fa_jobs *next;
} FA_JOBS;

/*
** Structure which defines a linked list of available tapes.
*/

typedef struct avail_tapes
{
	char media_id[IMS_COL15_LEN+1];
	char fa_media_id[IMS_COL15_LEN+1];
	char recorder_id[IMS_COL30_LEN+1];
	struct avail_tapes *next;
} AVAIL_TAPES;

/*
** Define a global structure for holding a report
*/

struct 
{
	IMS_FA_INTERFACE fa_data;
	int report_id;
} report_info;

/*
** Define some global widget ids which are shared with other ims_reporter
** routines.
*/

XtAppContext app;
Widget app_shell;
Widget glbl_listw;  /* Status list window */


static char *glb_programName;
static int glbl_report_id = 1;
static int glbl_scale_value = 1800;
IMS_JOB_USER_SPEC userSpec;
int cmdDone = 0;
static FA_JOBS *glbl_fa_jobs;
static reporter_state = PRE_LOGIN;

IMS_MSG_STRUCT *glbl_msgDesc; /* Shared with other ims_reporter routines */

/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/
static struct commands
{
	char *username;
	char *password;
	char *commandFile;
	char *report;
	char *server;
	char *database;
	char *help;
	char *release;
} commands;

/*
** "Itemlist" for getting the above commands from the commandline.
*/
static IMS_CMD_CLASS cmdLineElm [] =
{
	{"-U",           &commands.username},
	{"+username",    &commands.username},
	{"-P",           &commands.password},
	{"+password",    &commands.password},
	{"-R",			 &commands.report},
	{"+report", 	 &commands.report},
	{"-C",           &commands.commandFile},
	{"+commandFile", &commands.commandFile},
	{"-X",           &commands.server},
	{"+server",      &commands.server},
	{"-Y",           &commands.database},
	{"+database",    &commands.database},
	{"-h",           &commands.help},
	{"+help",        &commands.help},
	{"-r",           &commands.release},
	{"+release",     &commands.release}
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);


/*
** "Itemlist" for getting the above commands from a command file.
*/
static IMS_CMD_CLASS cmdFileElm [] =
{
	{"username",    &commands.username},
	{"password",    &commands.password},
	{"server",      &commands.server},
	{"database",    &commands.database},
	{"report",      &commands.report},
};
static int cmdFileElmCount = IMS_CMD_CLASS_COUNT (cmdFileElm);
static AVAIL_TAPES *glbl_availTapes;


void msg_box(Widget, int, char *, char *);
void BuildDialog(Widget);

static void cmd_cb(int, int, int, int, char *);
static void end_report_cb(int, int, int, int, char *);
static void quit_cb(Widget, XtPointer, XtPointer);
static void logon_quit_cb(Widget, XtPointer, XtPointer);
static void schedule_query_cb(Widget, XtPointer, XtPointer);
static void acq_query_cb(Widget, XtPointer, XtPointer);
static void logon_query_cb(Widget, XtPointer, XtPointer);
static void end_query_cb(Widget, XtPointer, XtPointer);
static void submit_job_cb(Widget, XtPointer, XtPointer);
static void valid_date_cb(Widget, XtPointer, XtPointer);
static void valid_user_cb(Widget, XtPointer, XtPointer);
static void valid_schedule_cb(Widget, XtPointer, XtPointer);
static void menuCallback(Widget, int, XtPointer);
static void scale_cb(Widget, caddr_t, XmScaleCallbackStruct *);
static int getArgInput (IMS_MSG_STRUCT *);
static int startReport();
static int startReport(IMS_FA_INTERFACE *);
static void usage();
static void BuildLogon(Widget);
static void BuildSingleTape(Widget, char *, int, char *);
static void BuildShippingDialog(Widget, char *, char *);
static void BuildRevSeq(Widget, char *, int);
static void BuildAcquisitionDialog(Widget, char *, int);
static void valid_tape_cb(Widget, XtPointer, XtPointer);
static void valid_pass_cb(Widget, XtPointer, XtPointer);
static void ship_add_query_cb(Widget, XtPointer, XtPointer);
static void ship_remove_query_cb(Widget, XtPointer, XtPointer);
static void ship_query_cb(Widget, XtPointer, XtPointer);
static void date_fld_cb(Widget, XtPointer, XtPointer);
static void timeout(Widget, XtPointer, XtPointer);
static void update_job_window(int, char *, int);
static int getTapeAvailInfo (IMS_MSG_STRUCT *, Widget, AVAIL_TAPES **, char *);
static int getTableName(IMS_MSG_STRUCT *, IMS_QI_DESC_OBJ *, char *, char *, char *);
static int dumpTapeAvailInfo (IMS_MSG_STRUCT *, AVAIL_TAPES *);
static void check_passwd(Widget, XtPointer, XtPointer);
static int test_logon();
static void station_id_cb(Widget, int, XtPointer);

/*******************************************************************
** 
** main
**
*******************************************************************/
void main(int argc, char *argv[])
{
	int status;
	char *ptr;
	struct utsname uname_info;    /* Structure for uname() */
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	IMS_MSG_STRUCT *msgDesc;
	int job_id;
	int shmid;
	IMS_FA_INTERFACE *fa_data;
	int logon_flag = FALSE;
	

	/*
	** Setup message facility.
	*/

	glbl_fa_jobs = NULL;

	/*
	** Initialize tape list 
	*/

	glbl_availTapes = NULL;

	(void) uname (&uname_info);
	(void) strncpy (hostName, uname_info.nodename, IMS_HOST_LEN);
	hostName[IMS_HOST_LEN] = '\0';  /* Null-terminate just in case */
	
    /*
	** Initialize the message processor.
	*/
	if ((msgDesc = ims_msgStructAlloc ()) == (IMS_MSG_STRUCT *) NULL)
	{
		(void) fprintf (stderr,
				"Memory allocation for IMS_MSG_STRUCT structure failed.");
		exit (IMS_FATAL);
	}

    glb_programName = ims_extractFileName (argv[0]);
		 
	  
	(void) ims_msgSubSystem (msgDesc, "IMS");
	(void) ims_msgProgramName (msgDesc, glb_programName);
	(void) sprintf (banner, "%s::%s", hostName, glb_programName);
	(void) ims_msgBanner (msgDesc, banner, IMS_MSG_ALLBANNER);
	(void) ims_msgOpenSyslog (msgDesc, "IMS/DADS:", LOG_LOCAL5);
	(void) ims_msgSybErrHndlFlag (msgDesc, IMS_ON);
	(void) ims_msgSybMsgHndlFlag (msgDesc, IMS_ON);

	glbl_msgDesc = msgDesc;


	/*
	** Get the command line arguments. The variable status will actually
	** contain the number of command line arguments processed upon
	** successful completion.
	*/
	if ((status = ims_getCmdLine (argc, argv, cmdLineElm,
		cmdLineElmCount, msgDesc)) < IMS_OK)
	{
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}

	/*
	** Check to see if we got everything off of the command line.
	*/
	if (status < argc)
	{
		(void) ims_msg (msgDesc, IMS_WARNING,
			"Only %d out of the %d command line arguments were processed.",
			status, argc);
	}

	/*
	** If release was specified, print it out.
	*/
	if (commands.release != (char *) NULL)
	{
		(void) ims_printVersion (stderr);
	}

	/*
	** If help has been specified, print usage and exit.
	*/
	if (commands.help != (char *) NULL)
	{
		usage ();
		(void) ims_msgStructFree (msgDesc);
		exit (0);
	}

	/*
	** If there is a command file present, then get any commands from
	** this file, then overlay all commands from the command-line, except
	** password, which will be gone by this point.
	*/
	if (commands.commandFile != (char *) NULL)
	{
		if ((status = ims_getFileParms (commands.commandFile,
			cmdFileElm, cmdFileElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msgStructFree (msgDesc);
			exit (1);
		}

		/*
		** Now, get command line arguments again to overlay file arguments.
		*/
		if ((status = ims_getCmdLine (argc, argv,
			cmdLineElm, cmdLineElmCount, msgDesc)) < IMS_OK)
		{
			(void) ims_msgStructFree (msgDesc);
			exit (1);
		}
	}

	/*
	** Process the information from the command-line and/or command-file.
	*/
	if ((status = getArgInput (msgDesc)) < IMS_OK)
	{
		(void) ims_msgStructFree (msgDesc);
		exit (1);
	}


	/*    
	** Setup X stuff...
	*/

	app_shell = XtVaAppInitialize(&app, "ims_reporter", NULL,
		0, &argc, argv, NULL, XmNwidth, IMS_FA_MW_X2,
		XmNheight, IMS_FA_MW_Y2, XtNmappedWhenManaged, FALSE, NULL);
	

	/*
	** Get Logon
	*/

	if ((userSpec.username[0] != '\0') &&
	    (userSpec.password[0] != '\0') &&
	    (userSpec.server[0] != '\0') &&
	    (userSpec.database[0] != '\0'))
	{
		if (test_logon() < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not login.");
			exit(0);
		}
		logon_flag = TRUE;
	}
	else
	{

		BuildLogon(app_shell);
	}



	/*
	** Add a timeout processing function...
	*/
	(void) XtAppAddTimeOut(app, 40000, (XtTimerCallbackProc) timeout, NULL); 

	XtRealizeWidget(app_shell);

	if (logon_flag)
		XtMapWidget((Widget) app_shell); /* Display FA Window */

	XtAppMainLoop(app);

	exit(0);

}

/*******************************************************************
** 
** cmd_cb
**
** Process a command line callback.
**
*******************************************************************/
static void cmd_cb(
	int job_id, 
	int report, 
	int status,
	int shmid,
	char *data)
{
	IMS_MSG_STRUCT *msgDesc = glbl_msgDesc;
	IMS_MSG_QUEUE *msgQueue;

	fprintf(stderr, "Child has finished. Extracted Messages:\n");
	while ((msgQueue = ims_msgQueueExtract(msgDesc)) != NULL)
	{
		fprintf(stderr, "%s\n", msgQueue->msg);
	}

	cmdDone = 1;

	if (shmid > -1)
		(void) ims_shm_remove(shmid);

}


/*******************************************************************
** 
** end_report_cb
**
*******************************************************************/
static void end_report_cb(
	int job_id, 
	int report, 
	int status,
	int shmid,
	char *data)
{
	IMS_MSG_STRUCT *msgDesc = (void *) data; 
	char msg[255];



	/*
	** Any necessary processing could be done here...
	*/


	if (status == IMS_JOB_COMPLETE)
	{
		sprintf(msg, "Job %d successfully completed. ",
			job_id);
	}
	else if (status == IMS_JOB_ABORTED)
	{
		sprintf(msg, "Job %d Aborted.",
			job_id);

	}
	else if (status == IMS_JOB_TIMEOUT)
	{
		sprintf(msg, "Job %d for Report %d Timed Out.",
			job_id, report); /* R2.1 */
	}
	else
	{
		sprintf(msg, "Job %d for Report %d Terminated With Unknown Status.",
			job_id, report);
	}

	(void) ims_shm_remove(shmid);

	update_job_window(job_id, NULL, status);

	/*
	** Display dialog box only for FA reports that aren't 
	** successfully generated.
	*/

	if (status != IMS_JOB_COMPLETE)
	{
		msg_box(app_shell, IMS_ERROR, "Error", msg);
		(void) ims_msg(glbl_msgDesc, IMS_ERROR, msg); /* Added R2.1 */ 
  }
  else
	{
		msg_box(app_shell, IMS_INFO, "Info", msg); /* Added R2.1 */
		(void) ims_msg(glbl_msgDesc, IMS_INFO, msg); /* Added R2.1 */ 
	}

}


/*******************************************************************
** 
** BuildDialog
**
*******************************************************************/

void BuildDialog(Widget wshell)
{
	Widget bb, submit_button, quit_button, Msgtextlabel, option, scale;
	Widget pshell;
	static Widget list_w;
	Widget wtext1, wtext2, wtext3, wtext4;
	Arg args[8];
	XmString tstr;
	char msg[255];
	XmString report[20], title;
	XmString text1, text2, text3, text4;
	int i;
	XmFontList fontreporter, fontlist, fontlist2, fontlist3;

	fontreporter = (XmFontList) loadReporterFont(wshell, "reporter");
	fontlist = (XmFontList) loadDialogFont(wshell, "dialogFont");
	fontlist2 = (XmFontList) loadCourierFont(wshell, "dialogFont");

	glbl_report_id = 1;

	XtSetArg(args[0], XmNx, IMS_FA_GEN_X1);
	XtSetArg(args[1], XmNy, IMS_FA_GEN_Y1);
	XtSetArg(args[2], XmNwidth, IMS_FA_GEN_X2);
	XtSetArg(args[3], XmNheight, IMS_FA_GEN_Y2);
	XtSetArg(args[4],XmNfontList, fontlist);

	pshell = XtCreatePopupShell("Report Generation", transientShellWidgetClass, 
		wshell, args, 5);

	bb = XtVaCreateManagedWidget("bb", xmBulletinBoardWidgetClass, 
		pshell, args, 5, NULL);

	/*
	** Create job information area.
	*/

	fontlist3 = (XmFontList) loadListFont(wshell, "listFont");

	XtSetArg(args[0], XmNx, IMS_FA_JI_X1);
	XtSetArg(args[1], XmNy, IMS_FA_JI_Y1);
	XtSetArg(args[2], XmNwidth, IMS_FA_JI_X2);
	XtSetArg(args[3], XmNheight, IMS_FA_JI_Y2);
	XtSetArg(args[4], XmNscrollingPolicy, XmAUTOMATIC);
	XtSetArg(args[5], XmNlistSizePolicy, XmCONSTANT);
	XtSetArg(args[6], XmNtraversalOn, False);
	XtSetArg(args[7], XmNfontList, fontlist3);
	
	list_w = XmCreateScrolledList (bb, "Job_Status", args, 8);
	glbl_listw = list_w;
	XtManageChild(list_w);

	text1 = XmStringCreateSimple("JOB ID");
	text2 = XmStringCreateSimple("REPORT TITLE");
	text3 = XmStringCreateSimple("TIME STARTED");
	text4 = XmStringCreateSimple("STATUS");

	wtext1 = XtVaCreateManagedWidget("label",
		xmLabelWidgetClass, bb,
		XmNlabelString, text1, 
		XmNfontList, fontlist3, 
		XmNx, IMS_FA_JS1_X1 ,
		XmNy, IMS_FA_JS_Y1, NULL);

	wtext2 = XtVaCreateManagedWidget("label",
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist3, 
		XmNlabelString, text2, 
		XmNx, IMS_FA_JS2_X1, 
		XmNy, IMS_FA_JS_Y1, NULL);

	wtext3 = XtVaCreateManagedWidget("label",
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist3, 
		XmNlabelString, text3, 
		XmNx, IMS_FA_JS3_X1,
		XmNy, IMS_FA_JS_Y1, NULL);

	wtext4 = XtVaCreateManagedWidget("label",
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist3, 
		XmNlabelString, text4, 
		XmNx, IMS_FA_JS4_X1 ,
		XmNy, IMS_FA_JS_Y1, NULL);

	XmStringFree(text1);
	XmStringFree(text2);
	XmStringFree(text3);
	XmStringFree(text4);



	/*
	** Setup buttons...
	*/

	submit_button = XtVaCreateManagedWidget(" CREATE ", 
			xmPushButtonWidgetClass,
			bb, XmNx, IMS_FA_MWSUBMIT_X1, XmNy, IMS_FA_MWSUBMIT_Y1, 
			XmNfontList, fontlist,
			NULL);

	quit_button = XtVaCreateManagedWidget(" CANCEL ", 
			xmPushButtonWidgetClass,
			bb, XmNx, IMS_FA_MWQUIT_X1, XmNy, IMS_FA_MWQUIT_Y1, 
			XmNfontList, fontlist,
			NULL);

	XtAddCallback(submit_button, XmNactivateCallback, 
			submit_job_cb, NULL);

	XtAddCallback(quit_button, XmNactivateCallback, 
			quit_cb, (void *) pshell);

	/*
	** Setup option menu...
	*/

	title = XmStringCreateSimple("Report Type:");
	report[0] = XmStringCreateSimple(" RADARSAT-1 Archive Storage Report");
	report[1] = XmStringCreateSimple(" RADARSAT-1 Reception Report");
	report[2] = XmStringCreateSimple(" Reserved");
	report[3] = XmStringCreateSimple(" ERS-1 Shipment Report");
	report[4] = XmStringCreateSimple(" ERS-1 Acquisition Report");
	report[5] = XmStringCreateSimple(" ERS-1 Extracted Data Report");
	report[6] = XmStringCreateSimple(" JERS-1 Catalog Report");
	report[7] = XmStringCreateSimple(" JERS-1 Acquisition Report");
	report[8] = XmStringCreateSimple(" JERS-1 Shipment Report");
	report[9] = XmStringCreateSimple(" ADEOS-1 Acquisition Report");
	report[10] = XmStringCreateSimple(" ADEOS-1 Shipment Report");
	report[11] = XmStringCreateSimple(" ERS-2 Shipment Report");
	report[12] = XmStringCreateSimple(" ERS-2 Acquisition Report");
	report[13] = XmStringCreateSimple(" ERS-2 Extracted Data Report");


	option = XmVaCreateSimpleOptionMenu(bb, "option",
		title, 0, 0, (XtCallbackProc) menuCallback,
		XmVaPUSHBUTTON, report[0], NULL, NULL, NULL, 
		XmVaPUSHBUTTON, report[1], NULL, NULL, NULL, 
		XmVaPUSHBUTTON, report[2], NULL, NULL, NULL, 
		XmVaPUSHBUTTON, report[3], NULL, NULL, NULL, 
		XmVaPUSHBUTTON, report[4], NULL, NULL, NULL, 
		XmVaPUSHBUTTON, report[5], NULL, NULL, NULL, 
		XmVaPUSHBUTTON, report[6], NULL, NULL, NULL, 
		XmVaPUSHBUTTON, report[7], NULL, NULL, NULL, 
		XmVaPUSHBUTTON, report[8], NULL, NULL, NULL, 
		XmVaPUSHBUTTON, report[9], NULL, NULL, NULL, 
		XmVaPUSHBUTTON, report[10], NULL, NULL, NULL, 
		XmVaPUSHBUTTON, report[11], NULL, NULL, NULL, 
		XmVaPUSHBUTTON, report[12], NULL, NULL, NULL, 
		XmVaPUSHBUTTON, report[13], NULL, NULL, NULL, 
		XmNy, IMS_FA_MW_Y2/4 - 20, 
		XmNfontList, fontlist,
		0);

	XmStringFree(title);
	for (i = 0; i < 14; i++)
	 	XmStringFree(report[i]);

	XtManageChild(option);

	/*
	** Setup slide scale...
	*/
#if 0

	scale = XtVaCreateManagedWidget("Timeout (Secs)", 
		xmScaleWidgetClass, bb, 
		XtVaTypedArg, XmNtitleString, XmRString, "Timeout", 7,
		XmNmaximum,	3600,
		XmNminimum, 1,
		XmNvalue, glbl_scale_value,
		XmNshowValue, True,
		XmNx, IMS_FA_SCALE_X1, 
		XmNy, IMS_FA_SCALE_Y1,
		XmNorientation, XmHORIZONTAL,
		NULL);

	XtAddCallback(scale, XmNvalueChangedCallback, (XtCallbackProc) scale_cb,
				NULL);
#endif
		

	/*
	** Add message
	*/


	sprintf(msg, "Flight Agency Report Generation");

	tstr = XmStringCreateSimple(msg);

	Msgtextlabel = XtVaCreateManagedWidget("label",
		xmLabelWidgetClass, bb,
		XmNlabelString, tstr, 
		XmNfontList, fontreporter,
		XmNx, IMS_FA_MWTITLE_X1,
		XmNy, IMS_FA_MWTITLE_Y1, NULL);

	XmStringFree(tstr);
	XtManageChild(bb);
		
	XtManageChild(pshell);
	XtRealizeWidget(app_shell);
	XtPopup(pshell, XtGrabExclusive);
}


/*******************************************************************
** 
** BuildAcquisitionDialog
**
*******************************************************************/

static void BuildAcquisitionDialog(
		Widget wshell,
		char *title_msg,
		int report_id)
{
	Widget bb, pshell, submit_button, quit_button, Msgtextlabel;
	XmString t;
	Arg args[4];
	XmString tstr;
	char msg[255];
	XmString title;
	XmString station[2]; /* R2.1 */
	Widget start_date, end_date, pass_num, option; 
	Widget textlabel1, textlabel2, textlabel3;
	int i;
	XmFontList fontlist;

	fontlist = (XmFontList) loadDialogFont(wshell, "dialogFont");


	XtSetArg(args[0], XmNx, IMS_FA_ACQ_X1);
	XtSetArg(args[1], XmNy, IMS_FA_ACQ_Y1);
	XtSetArg(args[2], XmNwidth, IMS_FA_ACQ_X2);
	XtSetArg(args[3], XmNheight, IMS_FA_ACQ_Y2);

	pshell = XtCreatePopupShell("Acquisition Query", transientShellWidgetClass, 
		wshell, args, 4);

	bb = XtVaCreateManagedWidget("bb", xmBulletinBoardWidgetClass, 
		pshell, args, 4, NULL);


	/*
	** Setup buttons...
	*/

	submit_button = XtVaCreateManagedWidget(" CREATE ", 
			xmPushButtonWidgetClass,
			bb, XmNx, IMS_FA_AQSUBMIT_X1, XmNy, IMS_FA_AQSUBMIT_Y1, 
			XmNfontList, fontlist,
			NULL);

	quit_button = XtVaCreateManagedWidget(" CANCEL ", 
			xmPushButtonWidgetClass,
			bb, XmNx, IMS_FA_AQQUIT_X1, XmNy, IMS_FA_AQQUIT_Y1, 
			XmNfontList, fontlist,
			NULL);

	XtAddCallback(submit_button,  XmNactivateCallback, 
		acq_query_cb, (void *) pshell);

	XtAddCallback(quit_button, XmNactivateCallback, 
		end_query_cb, (void *) pshell);

	if ((report_id != IMS_ESA_REAQ) && (report_id != IMS_ESA2_REAQ))
	{


		/*
		** Create two text fields for date range
		*/

		t = XmStringCreateSimple("Start Date:");

		textlabel1 = XtVaCreateManagedWidget("label", 
			xmLabelWidgetClass, bb,
			XmNlabelString, t,
			XmNfontList, fontlist,
			XmNx, IMS_FA_AQSTART_X1,
			XmNy, IMS_FA_AQSTART_Y1, NULL);

		start_date = XtVaCreateManagedWidget("text", xmTextWidgetClass,
			bb, XmNeditable, TRUE,
			XmNfontList, fontlist,
			XmNwidth, 150,
			XmNmaxLength, IMS_DATETIME_LEN + 1, XmNx, IMS_FA_AQSTART_X1 + 70,
			XmNy, IMS_FA_AQSTART_Y1,  NULL);

		XtAddCallback(start_date,  XmNmodifyVerifyCallback, 
			valid_date_cb, (void *) &(report_info.fa_data.date_start[0]));
			
		XtAddCallback(start_date,  XmNmotionVerifyCallback, 
			valid_date_cb, (void *) &(report_info.fa_data.date_start[0]));

		XmStringFree(t);

		t = XmStringCreateSimple("End Date:");
		textlabel2 = XtVaCreateManagedWidget("label", 
			xmLabelWidgetClass, bb,
			XmNfontList, fontlist,
			XmNlabelString, t,
			XmNx, IMS_FA_AQEND_X1,
			XmNy, IMS_FA_AQEND_Y1, NULL);

		end_date = XtVaCreateManagedWidget("text", xmTextWidgetClass,
			bb, XmNeditable, TRUE,
			XmNfontList, fontlist,
			XmNwidth, 150,
			XmNmaxLength, IMS_DATETIME_LEN + 1, XmNx, IMS_FA_AQEND_X1 + 70,
			XmNy, IMS_FA_AQEND_Y1,  NULL);

		XtAddCallback(end_date,  XmNmodifyVerifyCallback, 
				valid_date_cb, &(report_info.fa_data.date_end[0]));

		XtAddCallback(end_date,  XmNmotionVerifyCallback, 
				valid_date_cb, (void *) &(report_info.fa_data.date_start[0]));

		XmStringFree(t);
	}

	/*
	** Add pass number field.
	*/


	if ((report_id == IMS_ESA_REAQ) || (report_id == IMS_ESA2_REAQ))
	{
		t = XmStringCreateSimple("Orbit Id:");

		textlabel3 = XtVaCreateManagedWidget("label", 
			xmLabelWidgetClass, bb,
			XmNfontList, fontlist,
			XmNlabelString, t,
			XmNx, IMS_FA_AQPASS_X1-20,
			XmNy, IMS_FA_AQPASS_Y1,
			NULL);

		pass_num = XtVaCreateManagedWidget("text", xmTextWidgetClass,
				bb, XmNeditable, TRUE,
				XmNfontList, fontlist,
				XmNwidth, 150,
				XmNmaxLength, 10, XmNx, IMS_FA_AQPASS_X1 + 70,
				XmNy, IMS_FA_AQPASS_Y1,  NULL);

    report_info.fa_data.pass = -999;
		XtAddCallback(pass_num,  XmNmotionVerifyCallback, 
				valid_pass_cb, (void *) &(report_info.fa_data.pass));

		XmStringFree(t);

		/* The following added for R2.1 */
	  t = XmStringCreateSimple("Station id:");


		(void) XtVaCreateManagedWidget("label",
	          	 	xmLabelWidgetClass, bb,
		 						XmNfontList, fontlist,
		 						XmNlabelString, t,
							  XmNx, IMS_FA_AQPASS_X1-20,
							  XmNy, IMS_FA_AQPASS_Y1+35, NULL);

		 XmStringFree(t);
		 t = XmStringCreateSimple("");
		 station[0] = XmStringCreateSimple("Fairbanks");
		 station[1] = XmStringCreateSimple("McMurdo");

		 strcpy(report_info.fa_data.station_id, "FA") ;
		 option = XmVaCreateSimpleOptionMenu(bb, "option",
													 t, 0, 0, (XtCallbackProc) station_id_cb,
													 XmVaPUSHBUTTON, station[0], NULL, NULL, NULL,
													 XmVaPUSHBUTTON, station[1], NULL, NULL, NULL,
													 XmNx, IMS_FA_AQPASS_X1+60,
													 XmNy, IMS_FA_AQPASS_Y1+35,
													 XmNfontList, fontlist,
													 0);

	   XmStringFree(t);

	   for (i = 0; i < 2; i++)
			 XmStringFree(station[i]);

	   XtManageChild(option);
		 /* End of added */

	}


	/*
	** Add message
	*/

	tstr = XmStringCreateSimple(title_msg);

	Msgtextlabel = XtVaCreateManagedWidget("label",
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist,
		XmNlabelString, tstr, 
		XmNx, IMS_FA_AQTITLE_X1,
		XmNy, IMS_FA_AQTITLE_Y1, NULL);

	XmStringFree(tstr);


	XtManageChild(pshell);
	XtRealizeWidget(app_shell);
	XtPopup(pshell, XtGrabExclusive);

}

/*******************************************************************
** 
** BuildSingleTape
**
*******************************************************************/

static void BuildSingleTape(
		Widget wshell,
		char *title_msg,
		int report_id,
		char *platform)
{
	Widget bb, pshell, submit_button, quit_button, Msgtextlabel;
	XmString t;
	Arg args[10];
	XmString tstr;
	char msg[255];
	XmString title;
	Widget tape_num;
	Widget textlabel1, textlabel2, textlabel3;
	int i;
	AVAIL_TAPES *availTapes;
	static SHIP_INFO shipinfo;
	XmFontList fontlist, fontlist2;

	fontlist = (XmFontList) loadDialogFont(wshell, "dialogFont");
	fontlist2 = (XmFontList) loadCourierFont(wshell, "courierFont");

	/*
	** Create dialog box tiled just below Upper-Left 
	** of other dialog box.
	*/

	XtSetArg(args[0], XmNx, IMS_FA_STAPE_X1);
	XtSetArg(args[1], XmNy, IMS_FA_STAPE_Y1);
	XtSetArg(args[2], XmNwidth, IMS_FA_STAPE_X2); 
	XtSetArg(args[3], XmNheight, IMS_FA_STAPE_Y2);

	pshell = XtCreatePopupShell("Shipping Query", transientShellWidgetClass, 
		wshell, args, 4);

	bb = XtVaCreateManagedWidget("bb", xmBulletinBoardWidgetClass, 
		pshell, args, 4, NULL);


	/*
	** Setup buttons...
	*/

	submit_button = XtVaCreateManagedWidget(" CREATE ", 
			xmPushButtonWidgetClass,
			bb, XmNx, IMS_FA_ST_SUBMIT_X1, XmNy, IMS_FA_ST_SUBMIT_Y1, 
			XmNfontList, fontlist,
			NULL);

	quit_button = XtVaCreateManagedWidget(" CANCEL ", 
			xmPushButtonWidgetClass,
			bb, XmNx, IMS_FA_ST_QUIT_X1, XmNy, IMS_FA_ST_QUIT_Y1, 
			XmNfontList, fontlist,
			NULL);

	XtAddCallback(submit_button,  XmNactivateCallback, 
			ship_query_cb, (void *) &(shipinfo));

	XtAddCallback(quit_button, XmNactivateCallback, 
			end_query_cb, (void *) pshell);


	/*
	** Build the list of available tapes.
	*/

	XtSetArg(args[0], XmNx, IMS_FA_ST_TA_X1);
	XtSetArg(args[1], XmNy, IMS_FA_ST_TA_Y1);
	XtSetArg(args[2], XmNwidth, IMS_FA_ST_TA_X2);
	XtSetArg(args[3], XmNheight, IMS_FA_ST_TA_Y2);
	XtSetArg(args[4], XmNscrollingPolicy, XmAUTOMATIC);
	XtSetArg(args[5], XmNlistSizePolicy, XmCONSTANT);
	XtSetArg(args[6], XmNtraversalOn, False);
	XtSetArg(args[7], XmNfontList, fontlist2);
	
	tape_num = XmCreateScrolledList (bb, "Avail_Tapes", args, 8);
	XtManageChild(tape_num);

	if (getTapeAvailInfo(glbl_msgDesc, tape_num, &availTapes, platform) == NULL)
	{
		(void) ims_msg(glbl_msgDesc, IMS_ERROR, 
				"Could not get tape available list");
		return;
	}

	if (availTapes == NULL)
	{
		msg_box(wshell, IMS_WARNING, "Warning",
				"No Tapes Available for Shipping");
		return;
	}

	glbl_availTapes = availTapes;

	XtAddCallback(tape_num,  XmNbrowseSelectionCallback,
				valid_tape_cb, report_info.fa_data.tape_num[0]);

	/*
	** Add message
	*/

	tstr = XmStringCreateSimple("SELECT A TAPE TO REPORT TO CSA");

	Msgtextlabel = XtVaCreateManagedWidget("label",
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist,
		XmNlabelString, tstr, 
		XmNx, IMS_FA_STA_TITLE_X1,
		XmNy, IMS_FA_STA_TITLE_Y1, NULL);

	XmStringFree(tstr);

	t = XmStringCreateSimple("TAPE ID");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist2,
		XmNlabelString, t,
		XmNx, IMS_FA_STA_STATUS1_X1,
		XmNy, IMS_FA_STA_STATUS_Y1, NULL);

	XmStringFree(t);

	t = XmStringCreateSimple("FA TAPE ID");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist2,
		XmNlabelString, t,
		XmNx, IMS_FA_STA_STATUS2_X1,
		XmNy, IMS_FA_STA_STATUS_Y1, NULL);

	XmStringFree(t);

	t = XmStringCreateSimple("RECORDING ID");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist2,
		XmNlabelString, t,
		XmNx, IMS_FA_STA_STATUS3_X1,
		XmNy, IMS_FA_STA_STATUS_Y1, NULL);

	XmStringFree(t);

	shipinfo.list_w = NULL;
	shipinfo.parent = pshell;

	/*
	** Add message
	*/

	tstr = XmStringCreateSimple(title_msg);

	Msgtextlabel = XtVaCreateManagedWidget("label",
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist,
		XmNlabelString, tstr, 
		XmNx, IMS_FA_ST_TITLE_X1,
		XmNy, IMS_FA_ST_TITLE_Y1, NULL);

	XmStringFree(tstr);
	XtManageChild(pshell);
	XtRealizeWidget(app_shell);

	/*
	** Select first item in tape_num
	*/

	XmListSelectPos(tape_num, 1, TRUE);
	XtPopup(pshell, XtGrabExclusive);

}
/*******************************************************************
** 
** BuildRevSeq
**
*******************************************************************/

static void BuildRevSeq(
		Widget wshell,
		char *title_msg,
		int report_id)
{
	Widget bb, pshell, submit_button, quit_button, Msgtextlabel;
	XmString t;
	Arg args[4];
	XmString tstr;
	char msg[255];
	XmString title;
	XmString station[2] /* R2.1 */;
	Widget start_date, end_date, rev_num, option;
	Widget textlabel1, textlabel2, textlabel3;
	int i;
	XmFontList fontlist;

	fontlist = (XmFontList) loadDialogFont(wshell, "dialogFont");


	/*
	** Create dialog box tiled just below Upper-Left 
	** of other dialog box.
	*/

	XtSetArg(args[0], XmNx, IMS_FA_BS_X1);
	XtSetArg(args[1], XmNy, IMS_FA_BS_Y1);
	XtSetArg(args[2], XmNwidth, IMS_FA_BS_X2);
	XtSetArg(args[3], XmNheight, IMS_FA_BS_Y2+20);

	pshell = XtCreatePopupShell("Acquisition Query", transientShellWidgetClass, 
		wshell, args, 4);

	bb = XtVaCreateManagedWidget("bb", xmBulletinBoardWidgetClass, 
		pshell, args, 4, NULL);


	/*
	** Setup buttons...
	*/

	submit_button = XtVaCreateManagedWidget(" CREATE ", 
			xmPushButtonWidgetClass,
			bb, XmNx, IMS_FA_BS_SUBMIT_X1, 
			XmNfontList, fontlist,
			XmNy, 
			IMS_FA_BS_SUBMIT_Y1+20, NULL);

	quit_button = XtVaCreateManagedWidget(" CANCEL ", 
			xmPushButtonWidgetClass,
			bb, XmNx, IMS_FA_BS_QUIT_X1, 
			XmNfontList, fontlist,
			XmNy, 
			IMS_FA_BS_QUIT_Y1+20, NULL);

	XtAddCallback(submit_button,  XmNactivateCallback, 
			schedule_query_cb, (void *) pshell);

	XtAddCallback(quit_button, XmNactivateCallback, 
			end_query_cb, (void *) pshell);

	/*
	** Add schedule number field.
	*/


	t = XmStringCreateSimple("Revolution:");

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist,
		XmNlabelString, t,
		XmNx, IMS_FA_BS_TEXT_X1,
		XmNy, IMS_FA_BS_TEXT_Y1, NULL);

	rev_num = XtVaCreateManagedWidget("text", xmTextWidgetClass,
			bb, XmNeditable, TRUE,
			XmNfontList, fontlist,
			XmNwidth, 150,
			XmNmaxLength, 12, XmNx, IMS_FA_BS_TEXT_X1 + 90,
			XmNy, IMS_FA_BS_TEXT_Y1,  NULL);

  report_info.fa_data.pass = -999;
	XtAddCallback(rev_num,  XmNmotionVerifyCallback, 
				valid_pass_cb, &(report_info.fa_data.pass));

	XmStringFree(t);


	t = XmStringCreateSimple("Station id:");


	(void) XtVaCreateManagedWidget("label",
			xmLabelWidgetClass, bb,
			XmNfontList, fontlist,
			XmNlabelString, t,
			XmNx, IMS_FA_BS_TEXT2_X1,
			XmNy, IMS_FA_BS_TEXT2_Y1, NULL);

	XmStringFree(t);
	t = XmStringCreateSimple("");
  station[0] = XmStringCreateSimple("Fairbanks");
	station[1] = XmStringCreateSimple("McMurdo");
 
  strcpy(report_info.fa_data.station_id, "FA") ;
	option = XmVaCreateSimpleOptionMenu(bb, "option",
				t, 0, 0, (XtCallbackProc) station_id_cb,
				XmVaPUSHBUTTON, station[0], NULL, NULL, NULL,
				XmVaPUSHBUTTON, station[1], NULL, NULL, NULL,
				XmNx, IMS_FA_BS_TEXT2_X1+80,
				XmNy, IMS_FA_BS_TEXT2_Y1,
				XmNfontList, fontlist,
				0);

	XmStringFree(t);
	for (i = 0; i < 2; i++)
			XmStringFree(station[i]);

	XtManageChild(option);


	/*
	** Add message
	*/

	tstr = XmStringCreateSimple(title_msg);

	Msgtextlabel = XtVaCreateManagedWidget("label",
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist,
		XmNlabelString, tstr, 
		XmNx, IMS_FA_BS_TITLE_X1,
		XmNy, IMS_FA_BS_TITLE_Y1, NULL);

	XmStringFree(tstr);
	XtManageChild(pshell);
	XtRealizeWidget(app_shell);
	XtPopup(pshell, XtGrabExclusive);

}

/*******************************************************************
** 
** BuildShippingDialog
**
*******************************************************************/

static void BuildShippingDialog(
		Widget wshell,
		char *title_msg,
		char *platform)
{
	Widget pshell, list_w, submit_button, quit_button, add_button;
	Widget tape_date, tape_id, bb, textlabel1, textlabel2;
	Widget textlabel3, textlabel4, Msgtextlabel, remove_button;
	XmString t, tstr;
	Arg args[10];
	XmString title;
	static SHIP_INFO shipinfo;
	IMS_NUMERIC_DATE dateStruct;
	char current_date[IMS_DATETIME_LEN+1];
	AVAIL_TAPES *availTapes;
	XmFontList fontlist, fontlist2;

	fontlist = (XmFontList) loadDialogFont(wshell, "dialogFont");
	fontlist2 = (XmFontList) loadCourierFont(wshell, "courierFont");
	
	
	memset ((char *) &shipinfo, 0, sizeof(SHIP_INFO));		
	
	/*
	** Create row/column widget...
	*/

	XtSetArg(args[0], XmNx, IMS_FA_MS_X1);
	XtSetArg(args[1], XmNy, IMS_FA_MS_Y1);
	XtSetArg(args[2], XmNwidth, IMS_FA_MS_X2);
	XtSetArg(args[3], XmNheight, IMS_FA_MS_Y2);

	pshell = XtCreatePopupShell("Shipping Query", transientShellWidgetClass, 
		wshell, args, 4);

	bb = XtVaCreateManagedWidget("bb", xmBulletinBoardWidgetClass, 
		pshell, args, 4, NULL);

	/*
	** Create scrolled window for tape ids/dates currently being
	** shipped.
	*/

	XtSetArg(args[0], XmNx, IMS_FA_MS_TAPES_X1);
	XtSetArg(args[1], XmNy, IMS_FA_MS_TAPES_Y1);
	XtSetArg(args[2], XmNwidth, IMS_FA_MS_TAPES_X2);
	XtSetArg(args[3], XmNheight, IMS_FA_MS_TAPES_Y2);
	XtSetArg(args[4], XmNscrollingPolicy, XmAUTOMATIC);
	XtSetArg(args[5], XmNlistSizePolicy, XmCONSTANT);
	XtSetArg(args[6], XmNtraversalOn, False);
	XtSetArg(args[7], XmNfontList, fontlist2);
	
	list_w = XmCreateScrolledList (bb, "Ship_Tapes", args, 8);

	shipinfo.list_w = list_w;
	shipinfo.parent = pshell;
	XtManageChild(list_w);


	t = XmStringCreateSimple("SHIPMENT DATE");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist2,
		XmNlabelString, t,
		XmNx, IMS_FA_MS_STATUS1_X1,
		XmNy, IMS_FA_MS_STATUS_Y1, NULL);

	XmStringFree(t);

	t = XmStringCreateSimple("TAPE IDENTIFIER");

	textlabel4 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist2,
		XmNlabelString, t,
		XmNx, IMS_FA_MS_STATUS2_X1,
		XmNy, IMS_FA_MS_STATUS_Y1, NULL);

	XmStringFree(t);

	remove_button = XtVaCreateManagedWidget(" REMOVE ", 
			xmPushButtonWidgetClass,
			bb, XmNx, IMS_FA_MS_REMOVE_X1,
			XmNfontList, fontlist,
			XmNy, IMS_FA_MS_REMOVE_Y1,
			XmNwidth, 80, XmNtraversalOn, False, NULL);

	XtAddCallback(remove_button, XmNactivateCallback, 
			ship_remove_query_cb, (void *) list_w);

	/*
	** Create text entry boxes for Shipping Date and Tape ID.
	*/

	t = XmStringCreateSimple("Ship Date (YYYYMMDD):");

	textlabel1 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist,
		XmNlabelString, t,
		XmNx, IMS_FA_MS_DATE_X1,
		XmNy, IMS_FA_MS_DATE_Y1, NULL);

	tape_date = XtVaCreateManagedWidget("text", xmTextWidgetClass,
			bb, XmNeditable, TRUE,
			XmNfontList, fontlist,
			XmNwidth, 150,
			XmNmaxLength, IMS_DATETIME_LEN + 1, XmNx, IMS_FA_MS_DATE_X1,
			XmNy, IMS_FA_MS_DATE_Y1 + 25,  NULL);

	XmStringFree(t);

	/*
	** Set current date in tape date field.
	*/
	if (ims_getCurrentDate(glbl_msgDesc, &dateStruct) == IMS_OK)
	{
		sprintf(current_date, "%04d%02d%02d",
			dateStruct.year, dateStruct.month,  
			dateStruct.day);

		XmTextSetString(tape_date, current_date);
	}		

	strcpy(shipinfo.ship_date, current_date);

	XtAddCallback(tape_date,  XmNmodifyVerifyCallback, 
			date_fld_cb, (void *) &(shipinfo.ship_date[0]));

	XtAddCallback(tape_date,  XmNmotionVerifyCallback, 
			date_fld_cb, (void *) &(shipinfo.ship_date[0]));


	/*
	** Build the list of available tapes.
	*/

	XtSetArg(args[0], XmNx, IMS_FA_MS_TA_X1);
	XtSetArg(args[1], XmNy, IMS_FA_MS_TA_Y1);
	XtSetArg(args[2], XmNwidth, IMS_FA_MS_TA_X2);
	XtSetArg(args[3], XmNheight, IMS_FA_MS_TA_Y2);
	XtSetArg(args[4], XmNscrollingPolicy, XmAUTOMATIC);
	XtSetArg(args[5], XmNlistSizePolicy, XmCONSTANT);
	XtSetArg(args[6], XmNtraversalOn, False);
	XtSetArg(args[7], XmNfontList, fontlist2);
	
	tape_id = XmCreateScrolledList (bb, "Avail_Tapes", args, 8);
	XtManageChild(tape_id);

	if (getTapeAvailInfo(glbl_msgDesc, tape_id, &availTapes, platform) == NULL)
	{
		(void) ims_msg(glbl_msgDesc, IMS_ERROR,
			"Could not get tapes");
		return;
	}

	if (availTapes == NULL)
	{
		msg_box(wshell, IMS_WARNING, "Warning",
			"No Tapes Available for Shipping");
		return;
	}

	glbl_availTapes = availTapes;

	XtAddCallback(tape_id,  XmNbrowseSelectionCallback,
			valid_tape_cb, (void *) &(shipinfo.tape_id[0]));

	/*
	** Add message
	*/

	tstr = XmStringCreateSimple("CURRENT AVAILABLE TAPES FOR SHIPPING");

	Msgtextlabel = XtVaCreateManagedWidget("label",
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist,
		XmNlabelString, tstr, 
		XmNx, IMS_FA_TA_TITLE_X1,
		XmNy, IMS_FA_TA_TITLE_Y1, NULL);

	XmStringFree(tstr);

	t = XmStringCreateSimple("TAPE ID");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist2,
		XmNlabelString, t,
		XmNx, IMS_FA_TA_STATUS1_X1,
		XmNy, IMS_FA_TA_STATUS_Y1, NULL);

	XmStringFree(t);

	t = XmStringCreateSimple("FA TAPE ID");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist2,
		XmNlabelString, t,
		XmNx, IMS_FA_TA_STATUS2_X1,
		XmNy, IMS_FA_TA_STATUS_Y1, NULL);

	XmStringFree(t);

	t = XmStringCreateSimple("RECORDING ID");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNfontList, fontlist,
		XmNlabelString, t,
		XmNx, IMS_FA_TA_STATUS3_X1,
		XmNy, IMS_FA_TA_STATUS_Y1, NULL);

	XmStringFree(t);


	add_button = XtVaCreateManagedWidget(" ADD ", 
			xmPushButtonWidgetClass,
			bb, XmNx, IMS_FA_MS_ADD_X1, XmNy, IMS_FA_MS_ADD_Y1,
			XmNwidth, 80, 
			XmNfontList, fontlist,
			NULL);

	XtAddCallback(add_button, XmNactivateCallback, 
			ship_add_query_cb, (void *) &shipinfo);


	/* 
	** CREATE and QUIT Buttons
	*/


	submit_button = XtVaCreateManagedWidget(" CREATE ", 
			xmPushButtonWidgetClass,
			bb, XmNx, IMS_FA_MS_SUBMIT_X1, XmNy, IMS_FA_MS_SUBMIT_Y1,
			XmNwidth, 80, 
			XmNfontList, fontlist,
			NULL);

	XtAddCallback(submit_button,  XmNactivateCallback, 
			ship_query_cb, (void *) &shipinfo);

	quit_button = XtVaCreateManagedWidget(" CANCEL ", 
			xmPushButtonWidgetClass,
			bb, XmNx, IMS_FA_MS_QUIT_X1, XmNy, IMS_FA_MS_QUIT_Y1,
			XmNwidth, 80, 
			XmNfontList, fontlist,
			NULL);

	XtAddCallback(quit_button, XmNactivateCallback, 
			end_query_cb, (void *) pshell);

	/*
	** Add message
	*/

	tstr = XmStringCreateSimple(title_msg);

	Msgtextlabel = XtVaCreateManagedWidget("label",
		xmLabelWidgetClass, bb,
		XmNlabelString, tstr, 
		XmNfontList, fontlist,
		XmNx, IMS_FA_MS_TITLE_X1,
		XmNy, IMS_FA_MS_TITLE_Y1, NULL);

	XmStringFree(tstr);

	
	XtManageChild(pshell);
	XtRealizeWidget(app_shell);

	XmListSelectPos(tape_id, 1, TRUE);

	XtPopup(pshell, XtGrabExclusive);
}

/*******************************************************************
** 
** BuildLogon
**
*******************************************************************/

static void BuildLogon(
		Widget wshell)
{
	Widget bb, pshell, submit_button, quit_button, Msgtextlabel;
	XmString t;
	Arg args[4];
	XmString tstr;
	char msg[255];
	XmString title;
	Widget w_username, w_password, w_database, w_server;
	Widget textlabel1, textlabel2, textlabel3;
	Display *dpy;
	XFontStruct *font;
	XmFontList fontlist;
	int i;


	/*
	** Create dialog box tiled just below Upper-Left 
	** of other dialog box.
	*/

	XtSetArg(args[0], XmNx, IMS_FA_LOGON_X1);
	XtSetArg(args[1], XmNy,  IMS_FA_LOGON_Y1);
	XtSetArg(args[2], XmNwidth, IMS_FA_LOGON_X2);
	XtSetArg(args[3], XmNheight, IMS_FA_LOGON_Y2);

	pshell = XtCreatePopupShell("ims_reporter",  transientShellWidgetClass,
		wshell, args, 4);

	bb = XtVaCreateManagedWidget("Logon", xmBulletinBoardWidgetClass,
		pshell, args, 4, NULL);



	/*
	** Add USERNAME
	*/


	t = XmStringCreateSimple("Username:");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNlabelString, t,
		XmNx, IMS_FA_LG_USER_X1,
		XmNy, IMS_FA_LG_USER_Y1, NULL);
	XmStringFree(t);


	w_username = XtVaCreateManagedWidget("text", xmTextWidgetClass,
			bb, XmNeditable, TRUE,
			XmNwidth, 150,
			XmNmaxLength, 15, XmNx, IMS_FA_LG_USER_X1 + 80,
			XmNy, IMS_FA_LG_USER_Y1,  NULL);


	XtAddCallback(w_username,  XmNmotionVerifyCallback, 
				valid_user_cb, &userSpec.username); 

	XmTextSetString(w_username, userSpec.username);

	/*
	** Send RETURN to attempt a logon.
	*/

	XtAddCallback(w_username,  XmNactivateCallback, 
				logon_query_cb, (void *) pshell);


	/*
	** Add PASSWORD
	*/

	t = XmStringCreateSimple("Password:");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNlabelString, t,
		XmNx, IMS_FA_LG_PASS_X1,
		XmNy, IMS_FA_LG_PASS_Y1, NULL);

	w_password = XtVaCreateManagedWidget("text", xmTextWidgetClass,
			bb, XmNeditable, TRUE,
			XmNwidth, 150,
			XmNmaxLength, 30, XmNx, IMS_FA_LG_PASS_X1 + 80,
			XmNy, IMS_FA_LG_PASS_Y1,  NULL);

	XmStringFree(t);


	/*
	** Send RETURN to attempt a logon.
	*/

	XtAddCallback(w_password,  XmNactivateCallback, 
				logon_query_cb, (void *) pshell);

	XtAddCallback(w_password, XmNmodifyVerifyCallback, 
				check_passwd, (XtPointer) 0);

	XmTextSetString(w_password, userSpec.password);

	/*
	** Add SERVER
	*/

	t = XmStringCreateSimple("Server:");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNlabelString, t,
		XmNx, IMS_FA_LG_SERVER_X1,
		XmNy, IMS_FA_LG_SERVER_Y1, NULL);
	XmStringFree(t);

	w_server = XtVaCreateManagedWidget("text", xmTextWidgetClass,
			bb, XmNeditable, TRUE,
			XmNwidth, 150,
			XmNmaxLength, 30, XmNx, IMS_FA_LG_SERVER_X1 + 80,
			XmNy, IMS_FA_LG_SERVER_Y1,  NULL);

	XtAddCallback(w_server,  XmNmotionVerifyCallback, 
				valid_user_cb, &userSpec.server); 

	XmTextSetString(w_server, userSpec.server);

	/*
	** Send RETURN to attempt a logon.
	*/

	XtAddCallback(w_server,  XmNactivateCallback, 
				logon_query_cb, (void *) pshell);


	/*
	** Add Database
	*/

	t = XmStringCreateSimple("Database:");

	textlabel3 = XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNlabelString, t,
		XmNx, IMS_FA_LG_DATABASE_X1,
		XmNy, IMS_FA_LG_DATABASE_Y1, NULL);

	w_database = XtVaCreateManagedWidget("text", xmTextWidgetClass,
			bb, XmNeditable, TRUE,
			XmNwidth, 150,
			XmNmaxLength, 30, XmNx, IMS_FA_LG_DATABASE_X1 + 80,
			XmNy, IMS_FA_LG_DATABASE_Y1,  NULL);

	XmStringFree(t);

	XtAddCallback(w_database,  XmNmotionVerifyCallback, 
				valid_user_cb, &userSpec.database);

	XmTextSetString(w_database, userSpec.database);

	/*
	** Send RETURN to attempt a logon.
	*/

	XtAddCallback(w_database,  XmNactivateCallback, 
				logon_query_cb, (void *) pshell);

	/*
	** Setup buttons...
	*/

	submit_button = XtVaCreateManagedWidget(" LOGON ", 
			xmPushButtonWidgetClass,
			bb, XmNx, IMS_FA_LG_SUBMIT_X1, XmNy, IMS_FA_LG_SUBMIT_Y1, 
			NULL);

	quit_button = XtVaCreateManagedWidget(" CANCEL ", 
			xmPushButtonWidgetClass,
			bb, XmNx, IMS_FA_LG_QUIT_X1, XmNy, IMS_FA_LG_QUIT_Y1, NULL);

	XtAddCallback(submit_button,  XmNactivateCallback, 
			logon_query_cb, (void *) pshell);

	XtAddCallback(quit_button, XmNactivateCallback, 
			logon_quit_cb, NULL);

	/*
	** Create a special font for the copyright.
	*/

	dpy = XtDisplay(wshell);
	
	font = XLoadQueryFont(dpy, "-adobe-times-medium-r-normal--11-80-100-100-p-54-iso8859-1");
	fontlist = XmFontListCreate(font, "charset1");

	/*
	** Add copyright notice
	*/

	t = XmStringCreate("Copyright (C) 1996, California Institute of Technology.  U.S. Government", "charset1");

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNlabelString, t,
		XmNfontList, fontlist,
		XmNx, IMS_FA_LG_COPY1_X1,
		XmNy, IMS_FA_LG_COPY1_Y1, NULL);


	XmStringFree(t);

	t = XmStringCreate("Sponsorship under NASA Contract NAS7-1260 is acknowledged.", "charset1");

	(void) XtVaCreateManagedWidget("label", 
		xmLabelWidgetClass, bb,
		XmNlabelString, t,
		XmNfontList, fontlist,
		XmNx, IMS_FA_LG_COPY2_X1,
		XmNy, IMS_FA_LG_COPY2_Y1, NULL);


	XmStringFree(t);

	XtManageChild(pshell);
	XtRealizeWidget(app_shell);


	XtPopup(pshell, XtGrabExclusive);

	/*
	** Set focus to w_username text widget
	*/

	XmProcessTraversal(w_username, XmTRAVERSE_CURRENT);

	reporter_state = LOGIN;
}

/*******************************************************************
** 
** ship_remove_query_cb
**
*******************************************************************/

static void ship_remove_query_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	int pos;
	int *position_list;
	int position_count;
	Widget list_w;
	int x,i;

	/* Determine selected item */
	list_w = (Widget) client_d;

	if (!XmListGetSelectedPos (list_w, &position_list, &position_count))
	{	
		return;
	}

	for (i = position_count; i > 0; i --)
	{
		/*
		** Remove the entry from the global 
		*/
		pos = position_list[i - 1] - 1;

		/*
		** Scan through list of tapes and remove the entry
		*/

		for (x = IMS_MAX_SHIP_TAPES; x > pos; x--)
		{
			memcpy(report_info.fa_data.tape_num[i-1], 
				report_info.fa_data.tape_num[i],
				sizeof(report_info.fa_data.tape_num[i]));

			memcpy(report_info.fa_data.tape_date[i-1], 
				report_info.fa_data.tape_date[i],
				sizeof(report_info.fa_data.tape_date[i]));
		}
		memset(report_info.fa_data.tape_date[IMS_MAX_SHIP_TAPES-1],
			0, sizeof(report_info.fa_data.tape_date[IMS_MAX_SHIP_TAPES-1]));
			 
			
		report_info.fa_data.tape_count --;
	}

	XmListDeletePositions(list_w, position_list, position_count);

	XtFree((void *) position_list);
	
}

/*******************************************************************
** 
** update_job_window
**
*******************************************************************/

static void update_job_window(
	int job_id,
	char *job_title,
	int status)
{
	char  status_name[IMS_COL30_LEN+1];
	char date[IMS_DATETIME_LEN+10];
	char buffer[128];
	IMS_NUMERIC_DATE dateStruct;
	XmString item;
  	Display *dpy;
	int position_list;
	FA_JOBS *fa_jobs;
	int i = 0;

	fa_jobs = glbl_fa_jobs;

	if (status < IMS_JOB_COMPLETE)
	{
		strcpy(status_name, "IN PROGRESS");
		memset(date,0,sizeof(date));
		if (ims_getCurrentDate(glbl_msgDesc, &dateStruct) == IMS_OK)
		{
			ims_numericDateToDBMSA(&dateStruct, date);
		}
	}
	else if (status == IMS_JOB_ABORTED)
	{
		strcpy(status_name, "ABORTED");
	}
	else if (status == IMS_JOB_COMPLETE)
	{
		strcpy(status_name, "COMPLETE");
	}
	else if (status == IMS_JOB_TIMEOUT)
	{
		strcpy(status_name, "TIMEOUT");
	}

	/*
	** Update status table linked list for existing jobs.
	*/

	if (status >= IMS_JOB_COMPLETE)
	{
		while (fa_jobs != NULL)
		{
			i++;
			if (fa_jobs->job_id == job_id)
			{
				fa_jobs->status = status;
				strcpy(date, fa_jobs->start_time);
				job_title = fa_jobs->job_title;
				break;
			}
			fa_jobs = fa_jobs->next;
		}

		/*
		** If we can't find the job...then get out.
		*/

		if (fa_jobs == NULL)
			return;
	}

	sprintf(buffer, "%-7d%-15s%-25s%-12s",
		job_id, job_title, date, status_name);

	item = XmStringCreateSimple(buffer);
	position_list = i;

	/* 
	** i indicates a replacement or a new entry.
	*/

	if (i == 0)
		XmListAddItem(glbl_listw, item, i);
	else
		XmListReplacePositions(glbl_listw, &position_list, &item, 1);

	dpy = XtDisplay(glbl_listw);
  	XFlush(dpy);
		
	XmStringFree(item);

	/*
	** If changing an existing line then do not 
	** add a new entry to the job status table.
	*/

	if (status >= IMS_JOB_COMPLETE)
	{
		return;
	}

	/*
	** Add link
	*/

	if (fa_jobs == NULL)
	{
		fa_jobs = malloc(sizeof(FA_JOBS));
		if (fa_jobs == NULL)
		{
			(void) ims_msg(glbl_msgDesc, IMS_ERROR,
				"Could not malloc memory for status table.");
				
			return;
		}
		glbl_fa_jobs = fa_jobs;
	}
	else
	{
		while (fa_jobs->next != NULL)
			fa_jobs = fa_jobs->next;

		fa_jobs->next = malloc(sizeof(FA_JOBS));
		if (fa_jobs->next == NULL)
		{
			(void) ims_msg(glbl_msgDesc, IMS_ERROR,
				"Could not malloc memory for status table.");
			return;
		}
		fa_jobs = fa_jobs->next;
	}
	fa_jobs->next = NULL;
	fa_jobs->status = status;
	fa_jobs->job_id = job_id;
	strcpy(fa_jobs->start_time, date);
	strcpy(fa_jobs->job_title, job_title);
}

/*******************************************************************
** 
** timeout
**
** Perform periodic X processing to clean up status window
**
*******************************************************************/
static void timeout(
	Widget w, 
	XtPointer client_d, 
	XtPointer call_d)
{
	FA_JOBS *fa_jobs, *prev, *save_prev;
	int position_list;
	int i;

	/*
	** Scan through the currently running jobs...
	*/

	sighold(SIGCLD); /* Don't allow signal to preempt */

	fa_jobs = glbl_fa_jobs;
	prev = NULL;

	while (fa_jobs != NULL)
	{
		i++;
		if (fa_jobs->status >= IMS_JOB_COMPLETE)
		{
			position_list = i;
			XmListDeletePositions(glbl_listw, &position_list, 1);
			if (prev == NULL)
			{
				glbl_fa_jobs = glbl_fa_jobs->next;
			}
			else
			{
				prev->next = fa_jobs->next;
			}
			save_prev = prev;
			prev = fa_jobs;
			fa_jobs = fa_jobs->next;
			free(prev);
			prev = save_prev;
			i--; /* Since one less item in list now... */
		}
		else
		{
			prev = fa_jobs;
			fa_jobs = fa_jobs->next;
		}
	}
	(void) XtAppAddTimeOut(app, 40000, (XtTimerCallbackProc) timeout, NULL); 
	sigrelse(SIGCLD);
			
}




/*******************************************************************
** 
** ship_add_query_cb
**
*******************************************************************/

static void ship_add_query_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	SHIP_INFO *glbl_data = (SHIP_INFO *) client_d;
	IMS_NUMERIC_DATE dateStruct;
	XmString item;
	char ship_fld[IMS_COL30_LEN+1];
	int i, pos;
	
#if 0
	/*
	** Validate date
	*/

	if (ims_timeToNumericDate(glbl_msgDesc, 
			glbl_data->ship_date, &dateStruct) < IMS_OK)
	{
		msg_box(app_shell,	IMS_ERROR, "Error", 
				"Tape Date not in format YYYYMMDD");
		return;	
	}
#endif



	pos = report_info.fa_data.tape_count;

	for (i = 0; i < pos; i++)
	{
		if (!strcmp(report_info.fa_data.tape_num[i], glbl_data->tape_id) &&
			!strcmp(report_info.fa_data.tape_date[i], glbl_data->ship_date))
		{
			msg_box(app_shell, IMS_ERROR, "Error", 
					"Duplicate Shipment Date and Tape ID");
			return;
		}
	}

	sprintf(ship_fld, "%-28s%s", 
				glbl_data->ship_date, glbl_data->tape_id);

	item = XmStringCreateSimple(ship_fld);
	
	strcpy(report_info.fa_data.tape_num[pos], glbl_data->tape_id);
	strcpy(report_info.fa_data.tape_date[pos], glbl_data->ship_date);
	report_info.fa_data.tape_count ++;
	XmListAddItem(glbl_data->list_w, item, report_info.fa_data.tape_count);
	XmStringFree(item);
	(void) XmProcessTraversal(w, XmTRAVERSE_NEXT_TAB_GROUP);

}

/*******************************************************************
** 
** valid_schedule_cb
**
*******************************************************************/

static void valid_schedule_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	char c;
	int len;
	XmTextVerifyCallbackStruct *cbs;
	char *schedule;
	char *tmp_data;

	schedule = (char *) client_d;
	len = XmTextGetLastPosition(w);
	cbs =  (XmTextVerifyCallbackStruct *) call_d;

	tmp_data = XmTextGetString(w);

	strcpy(schedule, tmp_data);
}

/*******************************************************************
** 
** valid_pass_cb
**
*******************************************************************/

static void valid_pass_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	char c;
	int len;
	XmTextVerifyCallbackStruct *cbs;
	char *tmp_data;
	int *orbit_id;

	orbit_id = (int *) client_d;
	len = XmTextGetLastPosition(w);
	cbs =  (XmTextVerifyCallbackStruct *) call_d;

	tmp_data = XmTextGetString(w);

	/*
	** Should we check for numeric values?
	*/

	if (tmp_data != NULL)
	{
		*orbit_id = atoi(tmp_data);
		free(tmp_data);
	}

}
/*******************************************************************
** 
** station_id_cb
**
*******************************************************************/
static void station_id_cb(
	Widget w,
	int item_no,
	XtPointer call_d)
{
				if (item_no == 0)
				    strcpy(report_info.fa_data.station_id, "FA") ;
				else
				    strcpy(report_info.fa_data.station_id, "MC") ;
}

/*******************************************************************
** 
** valid_user_cb
**
*******************************************************************/

static void valid_user_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	char c;
	int len;
	XmTextVerifyCallbackStruct *cbs;
	char *glbl_data;
	char *tmp_data;


	glbl_data = (char *) client_d;
	tmp_data = XmTextGetString(w);
	strcpy(glbl_data, tmp_data);
}

/*******************************************************************
** 
** valid_tape_cb
**
*******************************************************************/

static void valid_tape_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	char c;
	int len;
	XmTextVerifyCallbackStruct *cbs;
	char *glbl_data;
	char *tmp_data;
	int *position_list;
	int position_count;
	int pos, i;
	AVAIL_TAPES *ptr; 


	glbl_data = (char *) client_d;


	if (!XmListGetSelectedPos (w, &position_list, &position_count))
	{
		return;
	}

	pos = position_list[0];

	ptr = glbl_availTapes;
	for (i = 0; i < pos - 1; i++)
	{
		ptr = ptr->next;
	}

	strcpy(glbl_data, ptr->media_id);



	return;

#if 0
	len = XmTextGetLastPosition(w);
	cbs =  (XmTextVerifyCallbackStruct *) call_d;

	if (cbs->doit)
	{
		memcpy(glbl_data + cbs->startPos, cbs->text->ptr, cbs->text->length);
		glbl_data[IMS_COL15_LEN] = '\0';
	}
#endif
}

/*******************************************************************
** 
** date_fld_cb
**
** For YYYYMMDD fields.
*******************************************************************/

static void date_fld_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	char c;
	int len;
	XmTextVerifyCallbackStruct *cbs;
	char *glbl_data;


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
	** Only allow single text entries, no clipboard pastes.
	*/

	if (cbs->text->length > 1)
	{
		cbs->doit = False;
		return;
	}


	/*
	** When backspacing make sure delimiter is removed as well.
	*/

	if (cbs->doit)
	{
		memcpy(glbl_data + cbs->startPos, cbs->text->ptr, cbs->text->length);
		glbl_data[IMS_DATETIME_LEN] = '\0';
	}
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

	if (cbs->doit)
	{
		memcpy(glbl_data + cbs->startPos, cbs->text->ptr, cbs->text->length);
		glbl_data[IMS_DATETIME_LEN] = '\0';
	}
}



/*******************************************************************
** 
** check_passwd
** 
** by Dan Heller
**
*******************************************************************/
void check_passwd(
Widget        text_w,
XtPointer     client_data,
XtPointer     call_data)
{
  char *new, *password;
  int len;
  XmTextVerifyCallbackStruct *cbs = 
     (XmTextVerifyCallbackStruct *) call_data;

  int glbl_data = (int)client_data;

  password = userSpec.password;

  if (cbs->startPos < cbs->currInsert) 
	{ /* backspace */
    cbs->endPos = strlen (password);
    password[cbs->startPos] = 0;
    return;
  }

  if (cbs->text->length > 1) 
	{
    cbs->doit = False;  /* don't allow "paste" operations */
    return;             /* make the user *type* the password! */
  }

  new = XtMalloc (cbs->endPos + 2); /* new char + NULL terminator */
  if (password) 
  {
    strcpy (new, password);
  }
  else
    new[0] = NULL;

  password = new;
  strncat (password, cbs->text->ptr, cbs->text->length);
  password[cbs->endPos + cbs->text->length] = 0;

  for (len = 0; len < cbs->text->length; len++)
    cbs->text->ptr[len] = '*';

  strcpy(userSpec.password, password);
  XtFree(new);

}

/*******************************************************************
** 
** test_logon
**
** 
*******************************************************************/

static int test_logon()
{
	IMS_MSG_STRUCT *msgDesc = glbl_msgDesc;
	IMS_QI_DESC_OBJ *qDesc;
	static char buffer[1024];

	/*
	** Attempt to logon
	*/
    if ((qDesc = ims_qiDescAlloc(msgDesc)) == (IMS_QI_DESC_OBJ *) NULL)
	{
			(void) ims_msg(msgDesc, IMS_FATAL,
					"Could not allocate a query descriptor.");
			ims_msgStructFree(msgDesc);
			exit(0);
	}
										 
	qDesc->cmd  = (char *) buffer;

	/*
	** Setup login parameters.
	*/

	IMS_SETUSER (qDesc, userSpec.username);
	IMS_SETPSWD (qDesc, userSpec.password);

	if (userSpec.server[0] != '\0')
		IMS_SETSERVER(qDesc, userSpec.server);

	if (userSpec.database[0] != '\0')
		IMS_SETDBNAME(qDesc, userSpec.database);
	
	if (ims_qiLogin(qDesc) < IMS_OK)
	{
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	ims_qiFreeDesc(qDesc);
	return(IMS_OK);

}

/*******************************************************************
** 
** logon_query_cb
**
** Callback for logon
*******************************************************************/

static void logon_query_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	IMS_MSG_STRUCT *msgDesc = glbl_msgDesc;


	if (test_logon() < IMS_OK)
	{
		msg_box(w, IMS_ERROR, "Error", "Unable to Login.");
		return;
	}

	XtPopdown((Widget) client_d);
	XtDestroyWidget((Widget) client_d);
	reporter_state = MAIN_WIN;
	showIntroScreen(app_shell);
	XtMapWidget((Widget) app_shell); /* Display FA Window */
}

/*******************************************************************
** 
** ship_query_cb
**
** Callback for shipment reports
*******************************************************************/

static void ship_query_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	SHIP_INFO *shipinfo = (SHIP_INFO *) client_d;

	XtPopdown((Widget) shipinfo->parent);
	XtDestroyWidget((Widget) shipinfo->parent);
 	(void) dumpTapeAvailInfo (glbl_msgDesc, glbl_availTapes);
	glbl_availTapes = NULL;

	/*
	** Start the report 
	*/

	if (startReport(&report_info.fa_data) < IMS_OK)
	{
		msg_box(app_shell,	IMS_ERROR, 
				"Error", "Report could not be started.  Check log file.");
	}


}

/*******************************************************************
** 
** acq_query_cb
**
** Callback for acquisition reports
*******************************************************************/

static void acq_query_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	IMS_NUMERIC_DATE dateStruct;


  if (report_info.fa_data.pass == -999) 
	{
			msg_box(app_shell, IMS_ERROR, "Error", 
					"No orbit number entered!");
			return;
	}
	/*
	** Do some validation here...
	*/  

	if ((report_info.report_id != IMS_ESA_REAQ) &&
		(report_info.report_id != IMS_ESA2_REAQ)) 
	{

		if (ims_timeToNumericDate(glbl_msgDesc, 
			report_info.fa_data.date_start, &dateStruct) < IMS_OK)
		{
			msg_box(app_shell, IMS_ERROR, "Error",
					"Start date not in format YYYY-DDDTHH:MM:SS.MSS");
			return;	
		}

		if (ims_timeToNumericDate(glbl_msgDesc, 
				report_info.fa_data.date_end, &dateStruct) < IMS_OK)
		{
			msg_box(app_shell,	IMS_ERROR, "Error", 
					"End date not in format YYYY-DDDTHH:MM:SS.MSS");
			return;	
		}

	}

	XtPopdown((Widget) client_d);
	XtDestroyWidget((Widget) client_d);

	/*
	** Start the report 
	*/

	if (startReport(&report_info.fa_data) < IMS_OK)
	{
		msg_box(app_shell,	IMS_ERROR, "Error", 
				"Report could not be started.  Check log file.");
	}
}

/*******************************************************************
** 
** schedule_query_cb
**
** Callback for entering schedule ids
*******************************************************************/

static void schedule_query_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	char *query_data = (char *) client_d;


  if (report_info.fa_data.pass == -999) 
	{
			msg_box(app_shell, IMS_ERROR, "Error", 
					"No orbit number entered!");
			return;
	}

	XtPopdown((Widget) client_d);
	XtDestroyWidget((Widget) client_d);

	/*
	** Start the report 
	*/

	if (startReport(&report_info.fa_data) < IMS_OK)
	{
		msg_box(app_shell,	IMS_ERROR, "Error", 
				"Report could not be started.  Check log file.");
	}
}

/*******************************************************************
** 
** end_query_cb
**
** Cancel query callback for all reports
*******************************************************************/

static void end_query_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	XtPopdown((Widget) client_d);
	XtDestroyWidget((Widget) client_d);
	memset(&report_info, 0, sizeof(report_info));
	report_info.report_id = -1;  /* Flag that no report selected */

	/*
	** We just always dump the tape information since we only
	** have one query dialog box displayed at a time.  
	** It doesn't hurt.
	*/

 	(void) dumpTapeAvailInfo (glbl_msgDesc, glbl_availTapes);
	glbl_availTapes = NULL;
}



/*******************************************************************
** 
** scale_cb
**
** Set the scale value...
*******************************************************************/

static void scale_cb(
	Widget w,
	caddr_t client_data,
	XmScaleCallbackStruct *cbs)
{
	glbl_scale_value = cbs->value;
}
/*******************************************************************
** 
** menuCallback
**
** Get the report id
*******************************************************************/

static void menuCallback(
	Widget w,
	int item_no,
	XtPointer call_d)
{
	glbl_report_id = item_no + 1;
	

}


/*******************************************************************
** 
** submit_job_cb
**
*******************************************************************/
static void submit_job_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{

	/*
	** Grab a shared memory id
	*/
	memset(&report_info, 0, sizeof(report_info));
	report_info.report_id = glbl_report_id;

	switch (report_info.report_id)
	{
    	case IMS_ADEOS_REAC:
			BuildAcquisitionDialog(app_shell, 
					"ADEOS-1 REAC FLIGHT AGENCY QUERY INTERFACE",
					IMS_ADEOS_REAC);
			break;
		case IMS_NASDA_REAC:
			BuildAcquisitionDialog(app_shell, 
					"JERS-1 REAC FLIGHT AGENCY QUERY INTERFACE",
					IMS_NASDA_REAC);

			break;

		case IMS_ESA_REAQ:
			BuildAcquisitionDialog(app_shell, 
					"ERS-1 REAQ FLIGHT AGENCY QUERY INTERFACE",
					IMS_ESA_REAQ);
			break;

		case IMS_ESA2_REAQ:
			BuildAcquisitionDialog(app_shell, 
					"ERS-2 REAQ FLIGHT AGENCY QUERY INTERFACE",
					IMS_ESA2_REAQ);
			break;

		case IMS_CSA_ARCHSTRGRPT:
			BuildSingleTape(app_shell, 
					"CSA ARCHIVE STORAGE REPORT QUERY INTERFACE",
					IMS_CSA_ARCHSTRGRPT, IMS_FA_R1);
	
			break;

    	case IMS_NASDA_CATA:
			BuildAcquisitionDialog(app_shell, 
					"NASDA CATA FLIGHT AGENCY QUERY INTERFACE",
					IMS_NASDA_CATA);
			break;

		case IMS_CSA_RECRPT:
			BuildRevSeq(app_shell, 
					"CSA RECEPTION REPORT FA QUERY INTERFACE",
					IMS_CSA_RECRPT);

			break;

		case IMS_NASDA_MSGM:
			BuildShippingDialog(app_shell, 
					"NASDA MSGM FLIGHT AGENCY QUERY INTERFACE",
					IMS_FA_J1);
			break;

		case IMS_ADEOS_SRRD:
			BuildShippingDialog(app_shell, 
					"ADEOS-1 SRRD FLIGHT AGENCY QUERY INTERFACE",
					IMS_FA_A1);
			break;
		
		case IMS_ESA_REEX:
			BuildAcquisitionDialog(app_shell, 
					"   ERS-1 REEX FLIGHT AGENCY QUERY INTERFACE",
					IMS_ESA_REEX);
			break;

		case IMS_ESA2_REEX:
			BuildAcquisitionDialog(app_shell, 
					"   ERS-2 REEX FLIGHT AGENCY QUERY INTERFACE",
					IMS_ESA2_REEX);
			break;

		case IMS_ESA_RESM:
			BuildShippingDialog(app_shell, 
					"   ERS-1 RESM FLIGHT AGENCY QUERY INTERFACE",
					IMS_FA_E1);
			break;

		case IMS_ESA2_RESM:
			BuildShippingDialog(app_shell, 
					"   ERS-2 RESM FLIGHT AGENCY QUERY INTERFACE",
					IMS_FA_E2);
			break;
	}

}

/*******************************************************************
**
** startReport
**
*******************************************************************/
static int startReport(
	IMS_FA_INTERFACE *fa_data_passed)
{
	int job_id;
	IMS_MSG_STRUCT *msgDesc = glbl_msgDesc;
	int i;
	int shmid;
	IMS_FA_INTERFACE *fa_data;

	/*
	** Get a shared memory id to begin building the interface...
	*/

	if (fa_data_passed->tape_count == 0)
	{
		switch(report_info.report_id)
		{
			case IMS_ESA_RESM:
			case IMS_ESA2_RESM:
			case IMS_ADEOS_SRRD:
			case IMS_NASDA_MSGM:
			{
				msg_box(app_shell, IMS_WARNING, "Warning",
					"No Tapes Selected For Shipping");
				
				return(IMS_OK);
			}
		}
	}


	shmid = ims_shm_getid();

	if (ims_shm_create(shmid, sizeof(IMS_FA_INTERFACE)) < 0)
	{
		(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not allocate memory for interface");
		return(IMS_ERROR);
	}

	fa_data = (IMS_FA_INTERFACE *) ims_shm_lock(shmid);

	if (fa_data == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not lock shared memory address for FA interface");
 		return(IMS_ERROR);
	}


	memcpy(fa_data, fa_data_passed, sizeof(IMS_FA_INTERFACE));
	memcpy((void *) &(fa_data->jobSpec), (void *) &userSpec, 
		sizeof(IMS_JOB_USER_SPEC));

	/*
	** Set the correct platform.
	*/
	
	switch (report_info.report_id)
	{
		case IMS_ESA_REEX:
		case IMS_ESA_RESM:
		case IMS_ESA_REAQ:
			strcpy(fa_data->platform, "ERS-1");
			break;

		case IMS_ESA2_RESM:
		case IMS_ESA2_REEX:
		case IMS_ESA2_REAQ:
			strcpy(fa_data->platform, "ERS-2");
			break;

		case IMS_ADEOS_REAC:
		case IMS_ADEOS_SRRD:
			strcpy(fa_data->platform, "ADEOS-1");
			break;

		case IMS_CSA_RECRPT:
		case IMS_CSA_ARCHSTRGRPT:
			strcpy(fa_data->platform, "RADARSAT-1");

			/*
			** The selection box is single, so set the count 
			** of tapes.  The other shipping dialog boxes handle 
			** this interactively in the add callback.
			*/

			fa_data->tape_count = 1;
			break;

		case IMS_NASDA_CATA:
		case IMS_NASDA_REAC:
		case IMS_NASDA_MSGM:
			strcpy(fa_data->platform, "JERS-1");
			break;

	}

	(void) ims_shm_unlock(shmid, (void *) fa_data);


	/*
	** Submit the new FA Report for processing...
	*/

	job_id = ims_submitReport(msgDesc, &userSpec, 
						report_info.report_id, glbl_scale_value, 
						end_report_cb, shmid, NULL);
	if (job_id < 0)
	{
		(void) ims_shm_remove(shmid);

		(void) ims_msg(msgDesc, IMS_ERROR, 
				"Could not start FA report %d", report_info.report_id);

		msg_box(app_shell, IMS_WARNING, "Warning",
				"Could not start report");
	}

	sighold(SIGCLD); /* Don't allow signal to preempt */
	switch (report_info.report_id)
	{
		case IMS_ESA_REAQ:
			update_job_window(job_id, "ERS-1 REAQ", IMS_JOB_STARTING);
			break;

		case IMS_ESA2_REAQ:
			update_job_window(job_id, "ERS-2 REAQ", IMS_JOB_STARTING);
			break;

		case IMS_ESA_REEX:
			update_job_window(job_id, "ERS-1 REEX", IMS_JOB_STARTING);
			break;

		case IMS_ESA2_REEX:
			update_job_window(job_id, "ERS-2 REEX", IMS_JOB_STARTING);
			break;

		case IMS_ESA_RESM:
			update_job_window(job_id, "ERS-1 RESM", IMS_JOB_STARTING);
			break;

		case IMS_ESA2_RESM:
			update_job_window(job_id, "ERS-2 RESM", IMS_JOB_STARTING);
			break;

		case IMS_ADEOS_REAC:
			update_job_window(job_id, "ADEOS-1 REAC", IMS_JOB_STARTING);
			break;

		case IMS_NASDA_REAC:
			update_job_window(job_id, "NASDA REAC", IMS_JOB_STARTING);
			break;

		case IMS_NASDA_CATA:
			update_job_window(job_id, "NASDA CATA", IMS_JOB_STARTING);
			break;

		case IMS_NASDA_MSGM:
			update_job_window(job_id, "NASDA MSGM ", IMS_JOB_STARTING);
			break;

		case IMS_ADEOS_SRRD:
			update_job_window(job_id, "ADEOS-1 SRRD", IMS_JOB_STARTING);
			break;

		case IMS_CSA_ARCHSTRGRPT:
			update_job_window(job_id, "CSA ARCHSTRGRPT", IMS_JOB_STARTING);
			break;

		case IMS_CSA_RECRPT:
			update_job_window(job_id, "CSA RECRPT", IMS_JOB_STARTING);
			break;
	}
	sigrelse(SIGCLD);
	return(IMS_OK);

}

/*******************************************************************
** 
** logon_quit_cb
**
*******************************************************************/
static void logon_quit_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	exit(0);
}

/*******************************************************************
** 
** quit_cb
**
*******************************************************************/
static void quit_cb(
	Widget w,
	XtPointer client_d,
	XtPointer call_d)
{
	XtPopdown((Widget) client_d);
}



/*******************************************************************
** 
** msg_box
**
*******************************************************************/
void msg_box(
	Widget parent,
	int severity,
	char *title,
	char *text)
/* generates an error dialog box */
{
	Widget dialog, pshell;
	Arg arg[3];
	XmString t;
	Display *dpy;
	XmFontList fontlist;
	Position x, y, wid, ht;

	if (reporter_state == PRE_LOGIN)
		return;
		
	if (reporter_state == LOGIN)
	{
		parent = app_shell;
	}		


	pshell = XtCreatePopupShell(title,
		transientShellWidgetClass, parent, NULL, 0);


				 
	fontlist = (XmFontList) loadReporterFont(parent, "reporter");

	t=XmStringCreateSimple(text);

	XtSetArg(arg[0],XmNmessageString,t);

	switch (severity)
	{
		case IMS_INFO:
    		dialog = XtVaCreateManagedWidget("dialog_m", 
				xmMessageBoxWidgetClass, pshell,
				XmNdialogType,  XmDIALOG_INFORMATION,
				XmNmessageString, t,
				XmNfontList, fontlist,
				XmNlabelFontList, fontlist,
				XmNdefaultPosition, TRUE,
				NULL);

			break;
		case IMS_WARNING:
    		dialog = XtVaCreateManagedWidget("dialog_m", 
				xmMessageBoxWidgetClass, pshell,
				XmNdialogType,  XmDIALOG_INFORMATION,
				XmNmessageString, t,
				XmNfontList, fontlist,
				XmNlabelFontList, fontlist,
				XmNdefaultPosition, TRUE,
				NULL);
			break;
		case IMS_ERROR:
    		dialog = XtVaCreateManagedWidget("dialog_m", 
				xmMessageBoxWidgetClass, pshell,
				XmNdialogType,  XmDIALOG_INFORMATION,
				XmNmessageString, t,
				XmNfontList, fontlist,
				XmNlabelFontList, fontlist,
				XmNdefaultPosition, TRUE,
				NULL);
			break;

	}


	XtUnmanageChild(XmMessageBoxGetChild(dialog,XmDIALOG_HELP_BUTTON));
	XtSetSensitive(XmMessageBoxGetChild(dialog,XmDIALOG_HELP_BUTTON),False);
	XtUnmanageChild(XmMessageBoxGetChild(dialog,XmDIALOG_CANCEL_BUTTON));
	XtSetSensitive(XmMessageBoxGetChild(dialog,XmDIALOG_CANCEL_BUTTON),False);
	XmStringFree(t);

	XtAddCallback(dialog, XmNokCallback,
		quit_cb,
		pshell);


	XtVaGetValues(parent, XmNwidth, &wid, 
			XmNheight, &ht, XmNx, &x, XmNy, &y,
			NULL);


	if (reporter_state == LOGIN)
		XtVaSetValues(pshell, XmNx, 500, 
							XmNy, 320,
							NULL);
	else
		XtVaSetValues(pshell, XmNx, x+wid/2-80, 
							XmNy, y + ht/2 - 60,
							NULL);


	XtManageChild(pshell);
														 
	XtRealizeWidget(app_shell);
	XtPopup(pshell, XtGrabExclusive);


	dpy = XtDisplay(parent);
	XFlush(dpy);

}



/******************************************************************************
**
** getArgInput ()
**
** Prompt for needed information not provided in the command-line
** and command-file.
**
******************************************************************************/

static int getArgInput (
	IMS_MSG_STRUCT *msgDesc)
{
	char inputBuffer[IMS_INPUT_BUF_LEN+1];
	char prompt[20];

	/*
	** Prompt user for any information NOT provided in the command
	** structure.
	*/

	memset((char *) &userSpec, 0, sizeof(userSpec));

	/* username */
	if (commands.username != (char *) NULL)
	{
		strcpy(userSpec.username, commands.username);
	}

#if 0 
	/* password not currently supported. */
	if (commands.password != (char *) NULL)
	{
		strcpy(userSpec.password, commands.password);
	}
#endif

	/* server */
	if (commands.server != (char *) NULL)
	{
		strcpy(userSpec.server, commands.server);
	}
	else
		if (getenv("IMS_SERVER"))
			strcpy(userSpec.server, getenv("IMS_SERVER"));


	/* database */
	if (commands.database != (char *) NULL)
	{
		strcpy(userSpec.database, commands.database);
	}
	else
		if (getenv("IMS_DB"))
			strcpy(userSpec.database, getenv("IMS_DB"));

	return (IMS_OK);
}

/*******************************************************************
** 
** usage
**
*******************************************************************/
static void usage()
{
	printf("IMS/DADS FA Reporter\n");
	printf("ims_reporter [-U <user>] [-P <pswd>] [-X <dbase>] [-Y <srvr>] [-R <report id>]");
	printf("\n");
}


/*******************************************************************
** 
** getTapeAvailInfo
**
** Query the catalog and get a list of available tapes.  Place the 
** tapes into the list window.
**
*******************************************************************/

static int getTapeAvailInfo (
	IMS_MSG_STRUCT *msgDesc,
	Widget w,
	AVAIL_TAPES **availTapes,
	char *platform)
{
	AVAIL_TAPES *ptr;
	char qbuf[IMS_COL512_LEN+1];
	IMS_QI_DESC_OBJ *qDesc;
	int rowCount;
	char buffer[IMS_COL80_LEN+1];
	char tableName[IMS_COL30_LEN+1];
	char downName[IMS_COL30_LEN+1];
	char oldMediaId[IMS_COL30_LEN+1];
	char curMediaId[IMS_COL30_LEN+1];
	char curStatus[IMS_COL15_LEN+1];
  	Display *dpy;
	XmString item;
	int status;
	char *datasetName;
	char *downlink;

	rowCount = 0;

	*availTapes = NULL;

	memset(curStatus, 0, sizeof(curStatus));
	memset(curMediaId, 0, sizeof(curMediaId));
	memset(oldMediaId, 0, sizeof(oldMediaId));


	/*
	** Perform query based on platform type.
	*/

	if (!strcmp(platform, IMS_FA_A1))
	{
		datasetName = IMS_FA_TA_A1;
		downlink = IMS_FA_DL_A1;
	}
	else if (!strcmp(platform, IMS_FA_E1))
	{
		datasetName = IMS_FA_TA_E1;
		downlink = IMS_FA_DL_E1;
	}
	else if (!strcmp(platform, IMS_FA_E2))
	{
		datasetName = IMS_FA_TA_E2;
		downlink = IMS_FA_DL_E2;
	}
	else if (!strcmp(platform, IMS_FA_J1))
	{
		datasetName = IMS_FA_TA_J1;
		downlink = IMS_FA_DL_J1;
	}
	else if (!strcmp(platform, IMS_FA_R1))
	{
		datasetName = IMS_FA_TA_R1;
		downlink = IMS_FA_DL_R1;
	}
	else
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Unknown platform specified for getting available tape information");
		return(IMS_ERROR);
	}

	

	/*
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

	/*
	** Setup login parameters.
	*/

	IMS_SETUSER (qDesc, userSpec.username);
	IMS_SETPSWD (qDesc, userSpec.password);

	if (userSpec.server[0] != '\0')
		IMS_SETSERVER(qDesc, userSpec.server);

	if (userSpec.database[0] != '\0')
		IMS_SETDBNAME(qDesc, userSpec.database);
	
	if (ims_qiLogin(qDesc) < IMS_OK)
	{
		msg_box(w, IMS_ERROR, "Error", "Unable to Access Database");
		ims_qiFreeDesc(qDesc);
		return(IMS_ERROR);
	}

	/*
	** Get the granules table names: Tape Available, Downlink
	*/

	if (getTableName(msgDesc, qDesc, tableName, datasetName, "HC") < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get granule table name for tape available.");
		msg_box(w,  IMS_ERROR, "Error",
				"Unable to Get Available Tape Information");
		return(IMS_ERROR);

	}

	if (getTableName(msgDesc, qDesc, downName, downlink, platform) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not get granule table name for tape available.");
		msg_box(w,  IMS_ERROR, "Error",
				"Unable to Get Available Tape Information");
		return(IMS_ERROR);

	}


        /* R2.1.2 : PR2957 */
        if (!strcmp(platform, IMS_FA_R1)) {
          sprintf(qDesc->cmd,
	    "select distinct t.MEDIA_ID, t.MEDIA_ID_ALIAS, \
	    t.RECORDER_MEDIA_TYPE, t.STATUS from %s t, \
	    %s d, platforms p where  \
	    (t.MEDIA_ID_TYPE_NAME = 'ARCHIVE_SIGNAL' and d.PLATFORM = 'R1') \
	    and p.platform = '%s' and d.PLATFORM = p.acronym \
	    and t.MEDIA_ID_ALIAS = d.MEDIA_ID_ALIAS \
	    order by MEDIA_ID_ALIAS, t.received_time desc",
	    tableName, downName, platform);
        }
        else {
          sprintf(qDesc->cmd,
	    "select distinct t.MEDIA_ID, t.MEDIA_ID_ALIAS, \
	    t.RECORDER_MEDIA_TYPE, t.STATUS from %s t, \
	    %s d, platforms p where  \
	    (t.MEDIA_ID_TYPE_NAME = 'FLIGHT_AGENCY' or \
	    (t.MEDIA_ID_TYPE_NAME = 'ARCHIVE_SIGNAL' and d.PLATFORM = 'R1')) \
	    and p.platform = '%s' and d.PLATFORM = p.acronym \
	    and t.MEDIA_ID_ALIAS = d.MEDIA_ID_ALIAS \
	    order by MEDIA_ID_ALIAS, t.received_time desc",
	    tableName, downName, platform);
        }

	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not perform query of tape available table for platform %s",
			platform);
			ims_qiFreeDesc(qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}


		memset(curMediaId, 0, sizeof(curMediaId)); /* FA Media Id */
		memcpy(curMediaId, qDesc->valAddr[1], qDesc->valLength[1]);
		ims_trim(curMediaId);

		if (!strcmp(oldMediaId, curMediaId))
		{
			/*
			** If we did not find a new media id then continue.  The
			** query returns at most two media ids which are the same.
			** because of the available, unavailable status.
			*/
			continue;
		}

		strcpy(oldMediaId, curMediaId);

		/*
		**  If the tape is unavailable, then don't add it to the 
		** list.
		*/

		memset(curStatus, 0, sizeof(curStatus));
		memcpy(curStatus, qDesc->valAddr[3], qDesc->valLength[3]);
		ims_trim(curStatus);

		if (strcmp(curStatus, "HST_S_AVAILABLE"))
			continue;


		rowCount ++;



		if (rowCount == 1)
		{
			ptr = (void *) malloc(sizeof(AVAIL_TAPES));
			*availTapes = ptr;
		}
		else
		{
			ptr->next = (void *) malloc(sizeof(AVAIL_TAPES));
			ptr = ptr->next;
		}

		if (ptr == NULL)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"Could not allocate memory for tape available list.");
			ims_qiFreeDesc(qDesc);
			msg_box(w, IMS_ERROR, "Error", "Out of memory.");
			return(IMS_ERROR);
		}

		/*
		** Get query results.
		*/

		memset(ptr->media_id, 0, sizeof(ptr->media_id));
		memcpy(ptr->media_id, qDesc->valAddr[0], qDesc->valLength[0]);
		ims_trim(ptr->media_id);

		memset(ptr->fa_media_id, 0, sizeof(ptr->fa_media_id));
		memcpy(ptr->fa_media_id, qDesc->valAddr[1], qDesc->valLength[1]);
		ims_trim(ptr->fa_media_id);

		memset(ptr->recorder_id, 0, sizeof(ptr->recorder_id));
		memcpy(ptr->recorder_id, qDesc->valAddr[2], qDesc->valLength[2]);
		ims_trim(ptr->recorder_id);

		ptr->next = NULL;

	}

	ims_qiFreeDesc(qDesc);


	/*
	** Add the items to the widgets list 
	*/

	ptr = *availTapes;

	while (ptr != NULL)
	{

		sprintf(buffer, "%-15s%-15s%-20s",
			ptr->media_id, ptr->fa_media_id, ptr->recorder_id);

		item = XmStringCreateSimple(buffer);

		XmListAddItem(w, item, 0);

		XmStringFree(item);
		ptr = ptr->next;
	}


	dpy = XtDisplay(w);
  	XFlush(dpy);
		

	return(IMS_OK);

}


/*******************************************************************
** 
** dumpTapeAvailInfo
**
*******************************************************************/

static int dumpTapeAvailInfo (
	IMS_MSG_STRUCT *msgDesc,
	AVAIL_TAPES *availTapes)
{
	AVAIL_TAPES *ptr;

	
	while (availTapes != NULL)
	{
		ptr = availTapes;
		availTapes = availTapes->next;
		free(ptr);
	}
	return(IMS_OK);
}


/******************************************************************************
**
** getTableName ()
**
******************************************************************************/

static int getTableName(
	IMS_MSG_STRUCT *msgDesc,
	IMS_QI_DESC_OBJ *qDesc,
	char *tableName,
	char *dataset_name,
	char *platform)
{
	int status;
	char qbuf[IMS_COL512_LEN];

	sprintf(qDesc->cmd,
		"select p.granules_table from dataset_relation r, \
			dataset_policy p where r.dataset = '%s' and \
			p.dataset_idx = r.dataset_idx and r.platform = '%s'",
			dataset_name, platform);
		
	while ((status = ims_qiNextRow(qDesc)) != IMS_ENDOFTRANSACTION)
	{
		if (status < IMS_OK)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
					   "Could not perform query of dataset relation.");
			ims_qiFreeDesc (qDesc);
			return(IMS_ERROR);
		}

		if (status == IMS_ENDOFQUERY)
		{
			continue;
		}

		(void) memcpy((char *) tableName,
					qDesc->valAddr[0], qDesc->valLength[0]);
		tableName[qDesc->valLength[0]] = '\0';
		ims_trim(tableName);

	}

	if (IMS_AFFECTED(qDesc) != 1) 
	{
		/* 
		** This is a problem ...
		*/

		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not locate granule name for dataset %s", dataset_name);

		if (ims_qiResetDesc (qDesc) < IMS_OK)
		{
			(void) ims_msg (msgDesc, IMS_FATAL,
				"Could not reinitialize the query descriptor.");
			ims_qiFreeDesc (qDesc);
			return (IMS_FATAL);
		}

		return(IMS_ERROR);

	}

	if (ims_qiResetDesc (qDesc) < IMS_OK)
	{
		(void) ims_msg (msgDesc, IMS_FATAL,
			"Could not reinitialize the query descriptor.");
		ims_qiFreeDesc (qDesc);
		return (IMS_FATAL);
	}

	return(IMS_OK);
}

