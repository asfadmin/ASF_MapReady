/*************************************************************************
 * Copyright(c) 1996, California Institute of Technology.                *
 *             ALL RIGHTS RESERVED.                                      *
 * U.S. Government Sponsorship acknowledged.                             *
 *************************************************************************/

/*------------------------------------------------------------------------------
 * $Date$			$Revision$
 *------------------------------------------------------------------------------
 *		Copyright 1991, Visual Edge Software Ltd.
 *
 * ALL  RIGHTS  RESERVED.  Permission  to  use,  copy,  modify,  and
 * distribute  this  software  and its documentation for any purpose
 * and  without  fee  is  hereby  granted,  provided  that the above
 * copyright  notice  appear  in  all  copies  and  that  both  that
 * copyright  notice and this permission notice appear in supporting
 * documentation,  and that  the name of Visual Edge Software not be
 * used  in advertising  or publicity  pertaining to distribution of
 * the software without specific, written prior permission. The year
 * included in the notice is the year of the creation of the work.
 *------------------------------------------------------------------------------
 * This is the project main program file for Ux generated code.
 * You may add application dependent source code at the appropriate places.
 *
 * Do not modify the statements preceded by the dollar sign ($), these
 * statements will be replaced with the appropriate source code when the
 * main program is automatically generated.
 *----------------------------------------------------------------------------*/

static char SccsFileId[] = "@(#)pps.c	1.4    02/18/97";

#ifdef XOPEN_CATALOG
#include <locale.h>
#endif

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include <pwd.h>

#include <UxLib.h>
#include <X11/Xlib.h>
#include <ctpublic.h>

#include "pps.h"
#include "pps_util.h"
#include "PPSdefs.h"
#include "PPSerr.h"
#include "defs.h"
#include "ims_query.h"
#include "ims_cmnQuery.h"

#ifdef UX_CATALOG
nl_catd		UxMsgCatalog;
#endif /* UX_CATALOG */

/*----------------------------------------------
 * Insert application global declarations here
 *---------------------------------------------*/

#define  SYSLOG_PPS     (LOG_LOCAL2)

char rootPath[200];

/* global program name for reporting to syslog */
char ProgName[MAXLINE];

GlobalData globalData;

/* is the user authorized to edit */
char IsAuthorizedUser=False;

/* database connections */
CS_CONNECTION   *query_connection;
CS_CONNECTION   *exec_connection;
IMS_CMN_QUERY   *ims_query;
int		ims_connected;

static XtResource resources[] =
{
    {   "configFile", "ConfigFile", XtRString, sizeof(String),
        XtOffset(GlobalDataPtr, configFile), XtRString, "" },
    {   "showSQL", "ShowSQL", XtRBoolean, sizeof(Boolean),
        XtOffset(GlobalDataPtr, showSQL), XtRString, "False" }
};

static XrmOptionDescRec options[] =
{
    { "-c", "*configFile", XrmoptionSepArg, (XPointer)NULL },
    { "-sql", "*showSQL", XrmoptionNoArg,   "True" }
};

const char* fallbackResources[]=
{
    "Uimx2_9*foreground:                black",
    "Uimx2_9*background:		AntiqueWhite",
    "Uimx2_9*bottomShadowColor:	        black",
    "Uimx2_9*topShadowColor:		white",
    "Uimx2_9*armColor:		        yellow",
    "Uimx2_9*borderColor:		turquoise",
    "Uimx2_9*highlightColor:		violet",
    "Uimx2_9*selectColor:		blue",
    "Uimx2_9*troughColor:		khaki",
    "Uimx2_9*FontList:	-adobe-helvetica-bold-r-normal--12-120-75-75-p-70-iso8859-1",
    "Pps*foreground:		        black",
    "Pps*background:		        AntiqueWhite",
    "Pps*bottomShadowColor:		black",
    "Pps*topShadowColor:		white",
    "Pps*armColor:			yellow",
    "Pps*borderColor:			turquoise",
    "Pps*highlightColor:		violet",
    "Pps*selectColor:			blue",
    "Pps*troughColor:			khaki",
    "Pps*FontList:			-adobe-helvetica-bold-r-normal--12-120-75-75-p-70-iso8859-1",
    "Pps*geometry:			+0+0",
    (char*) NULL
};



/*****************************************************************************/

static void
setAppFallbackResources(
Widget              w,
const String*       resourceSpec)
{
    int i=0;

    /* create an empty resource database */
    XrmDatabase rdb=0;

    rdb = XrmGetStringDatabase ("");
 
    /*------------------------------------------------------------
     * add the component resources, prepending
     * the name of the component 
     *------------------------------------------------------------*/
    for(i=0; resourceSpec[i] != 0; i++)
    {
        XrmPutLineResource (&rdb, resourceSpec[i]);
    } /*end while*/
 
    /*------------------------------------------------------------
     * merge them into the Xt database, with lowest precedence
     * the resources added later have the higher precedence
     *------------------------------------------------------------*/
    if (rdb)
    {
        Display* display = XtDisplay(w);
        /* merge rdb into display's database, no override */
        XrmCombineDatabase (rdb, &(display->db), False);
    }
}/*setAppFallbackResources*/

static void
continueProcessCB(
Widget		w,
XtPointer	clientData,
XtPointer	callData)
{
	/*---------------------------------
	 * Interface function declaration
	 *--------------------------------*/	

	swidget create_pps_main(swidget);

	swidget UxParent = NULL;

	/*-----------------------------------------------------------
	 * Declarations.
	 * The default identifier - mainIface will only be declared
	 * if the interface function is global and of type swidget.
	 * To change the identifier to a different name, modify the
	 * string mainIface in the file "main.dat". If "mainIface"
	 * is declared, it will be used below where the return value
	 * of PJ_INTERFACE_FUNCTION_CALL will be assigned to it.
	 *----------------------------------------------------------*/ 

	swidget mainIface;

	/*----------------------------------------------------------------
	 * Create and popup the first window of the interface. The
	 * return value can be used in the popdown or destroy functions.
	 * The swidget return value of PJ_INTERFACE_FUNCTION_CALL will
	 * be assigned to "mainIface" from PJ_INTERFACE_RETVAL_TYPE.
	 *---------------------------------------------------------------*/

	mainIface = create_pps_main(UxParent);

	Interface_UxManage(mainIface, &UxEnv);

} /* continueProcessCB */

main(
int		argc,
char		*argv[])
{
	char* 	   configFile;
        char*      tempPtr=0;
	char       configFilename[MAXSMALLBUF];
	struct passwd*	userPasswd;
	int		i;


	/*---------------------
	 * Initialize program
	 *--------------------*/

#ifdef XOPEN_CATALOG
	setlocale(LC_ALL, "");
	if (XSupportsLocale())
	{
		XtSetLanguageProc(NULL, (XtLanguageProc)NULL, NULL);
	}
#endif

#ifdef UX_CATALOG

#if defined(SOLARIS)
	/* Ensure NLSPATH has default value if not set. */
	if (!getenv("NLSPATH"))
	{
		putenv("NLSPATH=./%N");
	}
#endif
	UxMsgCatalog = UxCATOPEN(UX_CATALOG_NAME, 0);
#endif

	/*----------------------------------
	 * Initialize the message catalog.
	 *---------------------------------*/
	(void) UxInitCat();

	UxTopLevel = XtAppInitialize(&UxAppContext, "Pps",
				options, XtNumber(options),
				&argc, argv, NULL, NULL, 0);
	UxAppInitialize("Pps", &argc, argv);

	/*-------------------------------------------------------
	 * force the fallback resource to go in                
	 *------------------------------------------------------*/
	setAppFallbackResources(UxTopLevel,
			(const String*)fallbackResources);

	/*-------------------------------------------------------
	 * get the option resources                            
	 *------------------------------------------------------*/
	XtGetApplicationResources(UxTopLevel, (XtPointer)&globalData,
			resources, XtNumber(resources), 0, 0);

	/*-------------------------------------------------------
	 * Insert initialization code for your application here
	 *------------------------------------------------------*/
        tempPtr = (char *)getenv(PPS_ENV_LOCAL);
        if (tempPtr == (char *)NULL)
        {
                fprintf(stderr, "%s unknown.  Exiting.\n", PPS_ENV_LOCAL);
                exit(1);
        }
	else
	{
		(void)sprintf(rootPath, "%s/pps", tempPtr);
	}

	/*--------------------------------------------------------
	 *  open syslog
	 *--------------------------------------------------------*/

	(void)strcpy(ProgName, argv[0]);
        openlog("PPS:", LOG_PID|LOG_CONS|LOG_NDELAY, SYSLOG_PPS) ;
	pps_logMsg(ProgName, PPS_INFO, "Started");

	for (i = strlen(argv[0]) - 1, tempPtr = argv[0] + strlen(argv[0]); 
			i >= 0; i--, tempPtr--)
	{
		if (*tempPtr == '/')
		{
			(void)strcpy(ProgName, tempPtr+1);
			break;
		}
	}

        /*-------------------------------------------------
         * read the keyword-value pairs from config file
         *-------------------------------------------------*/
	if (globalData.configFile[0])
	{
		if ((read_config_file(globalData.configFile)) == ER_CONFIG_FILE)
			exit(1);
	}
	else
	{
		(void)sprintf(configFilename, "%s/config/pps.config",
				rootPath);
		if ((read_config_file(configFilename)) == ER_CONFIG_FILE)
			exit(1);
	}

	/*----------------------------------------------------------------
	 * Make connections with PPS Sybase Server - we need 2 right now
	 *---------------------------------------------------------------*/
	if ((db_connect(&query_connection)) != CS_SUCCEED)
	{
		fprintf(stderr, "Unable to make query_connection with Sybase\n");
		exit(1);
	}
        if ((db_connect(&exec_connection)) != CS_SUCCEED)
        {
                fprintf(stderr, "Unable to make exec_connection with Sybase\n");
                exit(1);
        }

	ims_connected = FALSE;

        /*----------------------------------------
         * Make connection with IMS Sybase Server 
         *---------------------------------------*/
	if ((ims_db_connect(&ims_query)) != ER_NO_ERROR)
	{
		fprintf(stderr,
		" ====> Failed to establish connection with the IMS Server \n");
		fprintf(stderr,
		" ====> Further attempt will be made to connect to IMS Server later \n");

		ims_connected = FALSE;
	}
	else
	{
		ims_connected = TRUE;
	}

	/*------------------------------------------------*/
	/* check whether the user is authorized to edit   */
	/*------------------------------------------------*/
	userPasswd = getpwuid(getuid());
	if (userPasswd != NULL)
	{
		IsAuthorizedUser = isAuthorizedUser(userPasswd->pw_name);
	}

	if (IsAuthorizedUser)
		continueProcessCB(0, 0, 0);
	else
		XppsCreateWarningDialog(UxTopLevel,
					"You are authorized to query only.",
					True, continueProcessCB, 0);

	/*-----------------------
	 * Enter the event loop 
	 *----------------------*/

	UxMainLoop();

	/*----------------------------
	 * Close database connections
	 *---------------------------*/
	(void)db_disconnect(query_connection);
	(void)db_disconnect(exec_connection);
	ims_db_disconnect(ims_query);

}/*main*/
