/*-----------------------------------------------------------
 * This is the project main program file for Xt generated 
 * code. You may add application dependent source code 
 * at the appropriate places. 
 * 			     
 * Do not modify the statements preceded by the dollar
 * sign ($), these statements will be replaced with
 * the appropriate source code when the main program is  
 * generated.  
 *
 * $Date$  		$Revision$ 
 *-----------------------------------------------------------*/ 
/*----------------------------------------------------------
 * SCCS Header
 * File:scan_qc.prj   Rev:4.4.0.0   Date:95/09/25
 *
 *---------------------------------------------------------*/

static char sccsid_scan_qc_prj[] = "@(#)scan_qc.c	1.6 96/07/17 14:45:49";

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xlib.h>

#include <Xm/Xm.h>
#include "inet.h"
#include "asf.h"

/*---------------------------------------------------- 
 * UxXt.h needs to be included only when compiling a 
 * stand-alone application. 
 *---------------------------------------------------*/
#ifndef DESIGN_TIME
#include "UxXt.h"
#endif /* DESIGN_TIME */

XtAppContext	UxAppContext;
Widget		UxTopLevel;
Display		*UxDisplay;
int		UxScreen;

/*----------------------------------------------
 * Insert application global declarations here
 *---------------------------------------------*/


#ifdef _NO_PROTO
main(argc,argv)
        int     argc;
        char    *argv[];
#else
main(int argc, char *argv[])
#endif /* _NO_PROTO */
{
	/*-----------------------------------------------------------
	 * Declarations.
         * The default identifier - mainIface will only be declared 
	 * if the interface function is global and of type swidget.
	 * To change the identifier to a different name, modify the
	 * string mainIface in the file "xtmain.dat". If "mainIface"
         * is declared, it will be used below where the return value
	 * of  PJ_INTERFACE_FUNCTION_CALL will be assigned to it.
         *----------------------------------------------------------*/ 

  	Widget mainIface;
        int    sockfd;
        int    serverPort;

	/*---------------------------------
	 * Interface function declaration
	 *--------------------------------*/	

 	Widget  create_scanQcDialogShell(swidget);

	swidget UxParent = NULL;


	/*---------------------
	 * Initialize program
	 *--------------------*/

#ifdef XOPEN_CATALOG
	if (XSupportsLocale()) {
		XtSetLanguageProc(NULL,(XtLanguageProc)NULL,NULL);
	}
#endif

  	UxTopLevel = XtAppInitialize(&UxAppContext, "CP_Scan_qc",
				     NULL, 0, &argc, argv, NULL, NULL, 0);

	UxDisplay = XtDisplay(UxTopLevel);
	UxScreen = XDefaultScreen(UxDisplay);

	/* We set the geometry of UxTopLevel so that dialogShells
	   that are parented on it will get centered on the screen
	   (if defaultPosition is true). */

	XtVaSetValues(UxTopLevel,
			XtNx, 0,
			XtNy, 0,
			XtNwidth, DisplayWidth(UxDisplay, UxScreen),
			XtNheight, DisplayHeight(UxDisplay, UxScreen),
			NULL);

	/*-------------------------------------------------------
	 * Insert initialization code for your application here 
	 *------------------------------------------------------*/

	/*----------------------------------------------------------------
	 * Create and popup the first window of the interface.  The 	 
	 * return value can be used in the popdown or destroy functions.
         * The Widget return value of  PJ_INTERFACE_FUNCTION_CALL will 
         * be assigned to "mainIface" from  PJ_INTERFACE_RETVAL_TYPE. 
	 *---------------------------------------------------------------*/

	mainIface = create_scanQcDialogShell(UxParent);

/***
	UxPopupInterface(mainIface, no_grab);
***/
 
        scan_qc(mainIface, argc, argv);	

	/*-----------------------
	 * Enter the event loop 
	 *----------------------*/

  	XtAppMainLoop (UxAppContext);

}
