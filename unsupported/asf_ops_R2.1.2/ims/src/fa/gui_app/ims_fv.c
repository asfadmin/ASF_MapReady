static char *sccs = "@(#)ims_fv.c	1.3 06/27/97";

/*****************************************************************************
*
**
** File:    ims_fv.c
**
** Function: Simple X file viewer utility.
**		
**
** Author: Dan Crichton
**
** Date:    9/23/96
**
**
*****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/utsname.h>


/*
** X/Motif includes...
*/

#include <X11/Intrinsic.h>
#include <Xm/RowColumn.h>
#include <Xm/BulletinB.h>
#include <Xm/Text.h>
#include <Xm/MainW.h>

/*
** IMS Query Interface and Message Logging Stuff...
*/

#include <ims_dbms.h>
#include <ims_query.h> 
#include <ims_getInput.h>
#include <ims_timeConv.h>
#include <ims_util.h>
#include <ims_cmd.h>
#include <syslog.h>
#include <ims_job_control.h>



/*
** Structures for getting arguments from the command line, and also
** for getting them from the command file.
*/

static struct commands
{
	char *filename;
	char *help;
	char *release;
} commands;

/*
** "Itemlist" for getting the above commands from the commandline.
*/
static IMS_CMD_CLASS cmdLineElm [] =
{
	{"-F",           &commands.filename},
	{"+filename",    &commands.filename},
	{"-h",           &commands.help},
	{"+help",        &commands.help},
	{"-r",           &commands.release},
	{"+release",     &commands.release}
};
static int cmdLineElmCount = IMS_CMD_CLASS_COUNT (cmdLineElm);


static char *glb_programName;

static int glbl_jobId; /* Assigned Job ID for the IMS/DADS World */

static Widget app_shell, glbl_text_w;
static XtAppContext app;

static IMS_MSG_STRUCT *glbl_msgDesc;

void file_cb(Widget, XtPointer, XtPointer);
void open_file_cb(Widget, XtPointer, XtPointer);
int loadFile(IMS_MSG_STRUCT *, Widget, char *);
XmFontList setFont(Widget, char *);



/*******************************************************************
** 
** main
**
*******************************************************************/
void main(int argc, char *argv[])
{
	int status;
	struct utsname uname_info;    /* Structure for uname() */
	char banner[IMS_HOST_LEN+IMS_PROGRAM_LEN+3];
	char hostName[IMS_HOST_LEN+1];
	IMS_MSG_STRUCT *msgDesc;
	char buffer[255];
	Widget main_w, text_w, scrolled_w;
	Widget menubar_w;
	XmString file_menu_s, open_s, quit_s;
	XmFontList fontlist;
	int shmid = -1;
	char *arg_ptr;

	glbl_jobId = -1;
	

	/*
	** Setup message facility.
	*/

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

	if (argc < 2)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"No input file specified.");
		(void) ims_msg(msgDesc, IMS_INFO,
			"ims_fv -F <input file>");
		(void) ims_msgStructFree(msgDesc);
		exit(0);
	}


    /*
	** Ignore death of a parent or other interuptions
	*/
		 

	if (strncmp(argv[1],"-F", 2) != 0)
	{
		/*
		** Check to see if this was invoked from a parent that passed,
		** the arguments through shared memory.
		*/

		if (argc < 3)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
				"No input file specified.");
			(void) ims_msgStructFree(msgDesc);
			exit(0);
		}

		glbl_jobId = atoi(argv[2]);
		if (ims_updateJobStatus(msgDesc, glbl_jobId, IMS_JOB_PENDING,
				getpid()) < IMS_OK)
		{
			ims_msg(msgDesc, IMS_ERROR, 
					"Could not set job status. Check startup.\n");
			(void) ims_msgStructFree (msgDesc);
			exit(1);
		}

		shmid = atoi(argv[3]);

		arg_ptr  = (void *) ims_shm_lock(shmid);

		if (arg_ptr == NULL)
		{
			(void) ims_msg(msgDesc, IMS_ERROR,
			"Shared Memory Error. Check startup.  Assumed to be through job control.");
			(void) ims_msgStructFree (msgDesc);
			exit(1);
		}

		commands.filename = malloc(strlen(arg_ptr)+1);
		strcpy(commands.filename, arg_ptr);
		(void) ims_shm_unlock(shmid, (void *) arg_ptr);

		goto skip_cmd_parser;
	}

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
		printf("ims_fv -F <filename>\n");
		(void) ims_msgStructFree (msgDesc);
		exit (0);
	}

	/*
	** Make sure program was invoked with a file to read...
	*/

	if (commands.filename == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"No input file specified.");
		(void) ims_msgStructFree(msgDesc);
		exit(0);
	}




	/*    
	** Setup X stuff...
	*/

skip_cmd_parser:


	sprintf(buffer, "ims_fv - %s", commands.filename);

	app_shell = XtVaAppInitialize(&app, buffer, NULL,
		0, &argc, argv, NULL, XmNwidth, 640,
		XmNheight, 480, 
		XmNtitle, buffer, NULL);

	fontlist = setFont(app_shell, "fv_Font");

	/*
	** Build the main window.
	*/

	main_w = XtVaCreateManagedWidget("fv_main",
		xmMainWindowWidgetClass, app_shell,
		NULL);

	/*
	** Create a scrolled window.
	*/

	scrolled_w = (Widget) XmCreateScrolledWindow(main_w, 
		"Scrolled Window", NULL, 0);

	XtVaSetValues(scrolled_w, XmNfontList, fontlist, NULL);

	XtManageChild(scrolled_w);

	/*
	** Add the scrolled text widget.
	*/ 

	text_w = XmCreateScrolledText(scrolled_w, "Text Window",
		NULL, 0);
	
	glbl_text_w = text_w;

	XtVaSetValues(text_w, 
		XmNeditMode, XmMULTI_LINE_EDIT,
		XmNeditable, FALSE,
		XmNfontList, fontlist, 
		NULL);

	if (loadFile(msgDesc, text_w, commands.filename) < IMS_OK)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not load the file '%s'", commands.filename);
		(void) ims_msgStructFree (msgDesc);
		exit(1);
	}
	
	XtManageChild(text_w);
	XtVaSetValues(main_w, XmNworkWindow, scrolled_w, NULL);

	/*
	** Add the file menu.
	*/

	file_menu_s = XmStringCreate("File", "fv_Font");

	menubar_w = XmVaCreateSimpleMenuBar(main_w, "menubar",
		XmVaCASCADEBUTTON, file_menu_s, 'F',
		XmNfontList, fontlist, 
		NULL);

	XmStringFree(file_menu_s);

	open_s = XmStringCreate("  Open...  ", "fv_Font");
	quit_s = XmStringCreate("  Quit  ", "fv_Font");

	XmVaCreateSimplePulldownMenu(menubar_w, "file_menu", 0, 
		file_cb,
		XmVaPUSHBUTTON, open_s, 'O', NULL, NULL,
		XmVaSEPARATOR,
		XmVaPUSHBUTTON, quit_s, 'Q', NULL, NULL,
		XmNdefaultFontList, fontlist, 
		NULL);

	XmStringFree(open_s);
	XmStringFree(quit_s);

	XtManageChild(menubar_w);
	XtVaSetValues(main_w, XmNmenuBar, menubar_w, NULL);
	XmMainWindowSetAreas(main_w, menubar_w, NULL, NULL, NULL, scrolled_w);

	XtRealizeWidget(app_shell);

	XtAppMainLoop(app);

	exit(0);

}


/*******************************************************************
** 
** file_cb
**
*******************************************************************/

void file_cb (
	Widget w,
	XtPointer client_data, 
	XtPointer call_data) 
{
	int item_no = (int) client_data;
	static Widget dialog = NULL;
	IMS_MSG_STRUCT *msgDesc = glbl_msgDesc;

	/*
	** If open is selected, then ...
	*/

	if (item_no == 0)
	{
		if (!dialog)
		{
			dialog = (Widget) XmCreateFileSelectionDialog(app_shell, 
				"Open", NULL, 0);
			XtAddCallback(dialog, XmNokCallback, open_file_cb, (void *) dialog);
			XtAddCallback(dialog, XmNcancelCallback, (void *) XtUnmanageChild, NULL);
		}
		XtManageChild(dialog);
		XtPopup(XtParent(dialog), XtGrabNone);
	}

	/*
	** if quit is selected, then ... 
	*/
	
	if (item_no == 1)
	{
		if (glbl_jobId > -1)
			(void) ims_jobComplete(msgDesc, glbl_jobId);
		(void) ims_msgStructFree (msgDesc);
		exit(0);
	}
}

/*******************************************************************
** 
** loadFile
**
*******************************************************************/

int loadFile(
	IMS_MSG_STRUCT *msgDesc,
	Widget w,
	char *filename)
{
	
	FILE *fptr;
	static char buffer[16000];
	int bytes_read = 0;
	int total_read = 0;

	fptr = fopen(filename, "r");

	if (fptr == NULL)
	{
		(void) ims_msg(msgDesc, IMS_ERROR,
			"Could not load file '%s'", filename);
		return(IMS_ERROR);
	}

	memset(buffer, 0, sizeof(buffer));
	bytes_read = fread(buffer, 1, sizeof(buffer) - 1, fptr);

	while (!feof(fptr) || (bytes_read > 0))
	{
		XmTextInsert(w, total_read + 1, buffer);
		total_read += bytes_read;
		memset(buffer, 0, sizeof(buffer));
		bytes_read = fread(buffer, 1, sizeof(buffer) - 1, fptr);
	}
	fclose(fptr);
	return(IMS_OK);
}


/*******************************************************************
** 
** open_file_cb
**
*******************************************************************/

void open_file_cb(
	Widget w,
	XtPointer clientd,
	XtPointer calld)
{
	char *file;
	XmFileSelectionBoxCallbackStruct *cbs = 
		(XmFileSelectionBoxCallbackStruct *) calld;

	if (cbs)
	{
		XmTextSetString(glbl_text_w, "");
		if (!XmStringGetLtoR(cbs->value, XmFONTLIST_DEFAULT_TAG, &file))
			return;

		(void) loadFile(glbl_msgDesc, glbl_text_w, file);

		XtFree(file);
		XtUnmanageChild(clientd);
	}

}

/*******************************************************************
** 
** setFont
**
*******************************************************************/

XmFontList setFont(
	Widget wshell, 
	char *name)
{
	Display *dpy;
	XFontStruct *font;
	XmFontList fontlist;
	 
	dpy = XtDisplay(wshell);
	font = XLoadQueryFont(dpy, "-adobe-courier-medium-r-normal--12-*-*-*-*-*-*-*");
	fontlist = XmFontListCreate(font, name);
	return(fontlist);

}

