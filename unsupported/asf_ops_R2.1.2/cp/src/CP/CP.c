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
static char sccsid_cp_c[] = "@(#)CP.c	1.8 96/12/04 14:46:54";

#ifdef XOPEN_CATALOG
#include <locale.h>
#endif

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xlib.h>

#include <Xm/Xm.h>
#include <syslog.h>
#include "CPmainQ.h"
#include "cplogs.h"

extern void check_number();
extern int setPPSqueueSize();


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

#if	defined(SOLARIS_FORM_PATCH)
extern	
#if	defined(__cplusplus)
"C"
#endif
void	UxInstallFormPatch();
#endif

/*----------------------------------------------
 * Insert application global declarations here
 *---------------------------------------------*/


extern void popupStartupBox();

#ifdef _NO_PROTO
main(argc,argv)
        int     argc;
        char    *argv[];
#else
main( int argc, char *argv[])
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

	/*---------------------------------
	 * Interface function declaration
	 *--------------------------------*/	

 	Widget  create_CPmainQ(swidget UxParent);
	swidget UxParent = NULL;
        Display *disp;
        char *env;

        /* first initialize syslog so we can print errors before X comes up */
        if (initASFlog(argc, argv, NULL) == -1) {
           printf("Can't open syslog files -- CP exiting\n");
           exit(-1);
        }

                              /* make sure DISPLAY variable is set properly */
        env =  getenv("DISPLAY");
        if (env == NULL) {    /* check if it's set at all */
           printfLLog(LOG_ERR, NO_DISPLAY);
           exit(-1);
        }
        else {                /* DISPLAY is set; check if it's valid */
          if ((disp = XOpenDisplay(env)) == NULL ) {
            printfLLog(LOG_ERR, BAD_DISPLAY, env);
            exit(-1);
          }
          XCloseDisplay(disp);
        }


	/*---------------------
	 * Initialize program
	 *--------------------*/

#ifdef XOPEN_CATALOG
	setlocale(LC_ALL,"");
	if (XSupportsLocale()) {
		XtSetLanguageProc(NULL,(XtLanguageProc)NULL,NULL);
	}
#endif

  	UxTopLevel = XtAppInitialize(&UxAppContext, "CP",
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

#if	defined(SOLARIS_FORM_PATCH)
	UxInstallFormPatch();
#endif
	/*-------------------------------------------------------
	 * Insert initialization code for your application here 
	 *------------------------------------------------------*/
	

	/*----------------------------------------------------------------
	 * Create and popup the first window of the interface.  The 	 
	 * return value can be used in the popdown or destroy functions.
         * The Widget return value of  PJ_INTERFACE_FUNCTION_CALL will 
         * be assigned to "mainIface" from  PJ_INTERFACE_RETVAL_TYPE. 
	 *---------------------------------------------------------------*/



	mainIface = create_CPmainQ(UxParent);
/*        XtVaSetValues(XtParent(mainIface), XmNminWidth, 950, NULL); */

        XtAddCallback( maxQueueSize_tf, XmNactivateCallback,
                (XtCallbackProc) check_number,
                (XtCallbackProc) setPPSqueueSize);


	UxPopupInterface(mainIface, no_grab);

	popupStartupBox();

        cpMainrtn(argc, argv, mainIface);

	/*-----------------------
	 * Enter the event loop 
	 *----------------------*/
        if (purify_is_running())
          purify_all_leaks();

  	XtAppMainLoop (UxAppContext);

}
