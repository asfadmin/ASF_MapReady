#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.  U.S. Government
Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:       cb_apswoscompare.c
 
Description:    Contains callback functions for the aps_WOS_compare program
                GUI interface
 
External Functions Defined:
        
External Variables Defined:
        
File Scope Variables:
        
Notes:
 
==============================================================================*/
#pragma ident   "@(#)cb_apswoscompare.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_apswoscompare.c"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <string.h>
#include <signal.h>
#include <errno.h>

#include <Xm/Xm.h>
#include <Xm/Text.h>
#include "UxXt.h"
 
#include "dapps_defs.h"
#include "aps_defs.h"
#include "aps_exe_names.h"
#include "aps_extern.h"
#include "apspath.h"
#include "apsfiledef.h"
 
#define CONTEXT_MACRO_ACCESS
#include "vc_apswoscompare.h"
#undef CONTEXT_MACRO_ACCESS
#include "cb_apswoscompare.h"
#include "vc_msgbox.h"
#include "subprocess.h"
#include "gui_utils.h"
 
 
 
/*==============================================================================
        Macro/Constant Definitions
==============================================================================*/
 
#define BEGIN_CONTEXT(widget) \
    _UxCAPSWOSCompare          *UxSaveCtx; \
    UxSaveCtx = UxAPSWOSCompareContext; \
    UxAPSWOSCompareContext = \
            (_UxCAPSWOSCompare *) UxGetContext( widget ); \
    {
 
#define END_CONTEXT \
        } \
        UxAPSWOSCompareContext = UxSaveCtx;


/*==============================================================================
	Function Declarations
==============================================================================*/
extern void		popup_message();


/*==============================================================================
        Global Variable Declarations
==============================================================================*/
extern XtAppContext     UxAppContext ;

 
/*==============================================================================
Function:       cb_mwos_filename
 
Description:    create callback for putting the full pathname of the APS MWOS
		file into the text field
 
Parameters:             
 
Returns:        
 
Creator:        Philip Yurchuk
 
Creation Date:  11/21/96
 
Notes:          
==============================================================================*/
/* ARGSUSED2 */
void
cb_mwos_filename(Widget widget, XtPointer client_data, XtPointer cbs)
{
  	char	*mwosfilename;
	int		i ;

    /*
	** Find the global data structure that contains the default file
	** path and default file name for the McMurdo WOS
	*/
	for (i = 0 ; reports[i].type != NULL ; i++)
	{
		if (strcmp( reports[i].type, MWOS_TYPE) == 0)
			break ;
	}
	mwosfilename = aps_fullpath (reports[i].aps_fullpath_id,
			reports[i].default_file_name);

	XmTextSetString(widget, mwosfilename);

	free(mwosfilename);

}

/*==============================================================================
Function:       wos_compare_done
 
Description:    sets the OK pushbutton sensitive
 
Parameters:             
 
Returns:        
 
Creator:        Philip Yurchuk
 
Creation Date:  12/11/96
 
Notes:          
==============================================================================*/
/* ARGSUSED0 */
static void
wos_compare_done( PROCESS_INFO *process, void *dummyarg )
{

BEGIN_CONTEXT( APSWOSCompare )
 
	XtSetSensitive( pushButton_OK, True ) ;

END_CONTEXT
 
        return;
}


/*==============================================================================
Function:       cb_aps_wos_compare
 
Description:    callback for launching the aps_WOS_compare program
 
Parameters:             
 
Returns:        
 
Creator:        Philip Yurchuk
 
Creation Date:  11/21/96
 
Notes:          
==============================================================================*/
/* ARGSUSED1 */
void
cb_aps_wos_compare( Widget widget, XtPointer client_data, XtPointer cbs )
{
        PROCESS_INFO    *process;
	char 		*filename;
	char		display_string[256];
	char		command[256];
	FILE		*fp;

BEGIN_CONTEXT( widget )
	  
	/* Get the file name to compare */
  
	filename = XmTextGetString(textField1);
	
	/* Pop up an error dialog if the filename entered is NULL */

	if (filename == "")
	{
	  	(void) sprintf(display_string, "Please enter a file name.");
		popup_message(XmDIALOG_ERROR, 
			      "APS:ERROR", 
			      display_string, 
			      XtGrabNone);

		XtFree(filename);
		return;
	}

	/* Pop up an error dialog if the file cannot be read */

	fp = fopen(filename, "r");
	if (fp == NULL)
	{
		(void) sprintf(display_string, 
			       "File cannot be read.  Please enter a valid file name.");
		popup_message(XmDIALOG_ERROR, 
			      "APS:ERROR", 
			      display_string, 
			      XtGrabNone);
		(void) fclose(fp);
		XtFree(filename);
		return;
	}
	(void) fclose(fp);

	/* Launch the aps_WOS_compare tool */

	(void) sprintf(command, "%s -P %s %s", WOS_COMPARE_CMD, password, filename);
	process = create_process(command, NULL, TRUE, NULL,
				 gui_display_message_widget, 
				 scrolledText_woscompare,
				 wos_compare_done, NULL );
	if (process == NULL)
	{
   		(void)sprintf(display_string,
			"can't create the file processor\n Too many processes running?" ) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
		/* 
		-- Reset for next invocation: as if had completed 
		-- the WOS_COMPARE_CMD
		*/
		XtFree(filename);
		return ;
	}

	if (start_process( process ))
	{
		/*
		-- can't start process (can't get pipes), message already popped
		-- up; restore to beginning of this callback; try again later.
		*/
	  	destroy_process( process );
		XtFree(filename);
		return;
	}

	XtSetSensitive( pushButton_OK, False ) ;

END_CONTEXT

	XtFree(filename);
	return;
}
