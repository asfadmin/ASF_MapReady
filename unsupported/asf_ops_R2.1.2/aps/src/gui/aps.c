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

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:
 
Description:
 
External Functions Defined:
 
File Scope Functions:
 
External Variables Defined:
 
File Scope Variables:
 
Notes:
 
==============================================================================*/
#pragma ident	"@(#)aps.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.aps.c"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xlib.h>
#include <Xm/FileSB.h>

#include "aps.h"
#include "aps_defs.h"
#include "aps_extern.h"
#include "gui_utils.h"

#ifdef QAP
#include <partner.h>
#endif

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

extern void	*UxMethodLookup() ;
extern int	init_vec_lib() ;
extern int	init_vec_lib_exit() ;
 
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

static void usage()
{
	(void) printf( "\n" ) ;
	(void) printf( "Acquisition Planning Subsystem (APS)\n" ) ;
	(void) printf( "Graphical User Interface\n\n" ) ;
		
	(void) printf( "Usage: aps -U <userid>\n") ;
	(void) printf( "       (where <userid> is a Sybase User ID)\n" ) ;
	(void) printf("\n\n") ;
	exit( APS_EXIT_ERROR ) ;
}

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
	/* explicitly declared as a global,above: vctrm complains if move
	-- "$ PJ_INTERFACE_RETVAL_TYPE to the global area;
	-- original statement (with no space after the "$" sign):
	$ PJ_INTERFACE_RETVAL_TYPE 
	*/

	/*---------------------------------
	 * Interface function declaration
	 *--------------------------------*/	

 	Widget  create_APSLoginForm(swidget);

	swidget UxParent = NULL;


	/*---------------------
	 * Initialize program
	 *--------------------*/


	static int rcode ;
	static int  opt ;
	static char *optlist = "U:P:" ;
 
	extern int  optind ;
	extern char *optarg ;

	/* set stdout to unbuffered I/O */
	setbuf( stdout, (char *) NULL ) ;

	APS_progname = strrchr( argv[0], '/' ) ;
	APS_progname = (APS_progname == NULL) ? argv[0] : ++APS_progname ;

	userid = NULL ;
	while((opt = getopt(argc, argv, optlist)) != EOF)
	{
		switch(opt)
        	{
        	case 'U' :
			userid = (char *) malloc( strlen( optarg ) + 1 );
			(void)strcpy( userid, optarg ) ;
			break ;

		default :
			usage() ;
			break ;
		}
	}			
	argv = argv + optind ;
	argc = argc - optind ;

	rcode = init_vec_lib() ;
	if (rcode)
		init_vec_lib_exit(rcode);
 

#ifdef XOPEN_CATALOG
	if (XSupportsLocale()) {
		XtSetLanguageProc(NULL,(XtLanguageProc)NULL,NULL);
	}
#endif

	/*
	-- Uimx generated stmt is:
	--  	UxTopLevel = XtAppInitialize(&UxAppContext, "$ PJ_APP_CLASS_NAME",
	-- have to figure out how to set PJ_APP_CLASS_NAME via Uimx
	*/
  	UxTopLevel = XtAppInitialize(&UxAppContext, "Aps",
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

	mainIface = create_APSLoginForm(UxParent);

	Interface_UxManage(mainIface, &UxEnv);
	if (gui_AddToTopLevelInterfaces(mainIface) <= 0)
	{
		aps_internal_error( stderr, __FILE__, __LINE__,
			"\n*** EXITING ***, can't start the Main Menu (main window)\n\
(array in gui_AddToTopLevelInterfaces() is too small)\n" ) ;
 		exit( APS_EXIT_ERROR ) ;
}

	/*-----------------------
	 * Enter the event loop 
	 *----------------------*/
#ifdef QAP
	QAP_BindApp(UxAppContext) ;
#endif

  	XtAppMainLoop(UxAppContext);

	return (APS_EXIT_OK);
}
