#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       cb_crtapsfile.c

Description:

External Functions Defined:
                cb_update_capsfile_form
                cb_show_reports
                cb_create_report
                cb_capsfile_select_input
                cb_init_extended_times
                remote_STGS_filename
                remote_REUG_filename
                remote_REQW_filename
                remote_REQQ_filename
                remote_MSGE_filename
                remote_MSGF_filename
                remote_wos_filename
                remote_sv_filename
                remote_WFF_WOS_filename
                remote_WFF_EPH_filename
                remote_WFF_REQ_filename
                CRT_local_filename
                aps_getPMFKeyWordValue
                xlate_wos_to_wff_format
                xlate_sv_to_wff_format
                cb_faif_filexfer
                cb_hc_filexfer

File Scope Functions:
                init_create_xfer
                init_extendTimeArray
                verifyCreate
                set_capsfile_form_sensitivity
                cb_capsfile_save_input_file
                CRT_remote_filename
                CRT_YYYYDDDHHMMSSUUU_remote_filename
                agency_done
                archive_done
                ask_to_send_file
                ask_to_archive_file
                create_ims_filename
                dce_xmit_file

External Variables Defined:

File Scope Variables:

Notes:
Nadia Adhami 11/18/94   cb_create_report():
        display_message_widget, scrolledText_create_report changed to NULL

==============================================================================*/
#pragma ident   "@(#)cb_crtapsfile.c	5.2 98/03/12 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_crtapsfile.c"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <libgen.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/file.h>
#include <string.h>
#include <errno.h>

#include <Xm/FileSB.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/Xm.h>
#include "UxXt.h"

#include "dapps_defs.h"
#include "db_sybint.h"
#include "db_reqq_phase.h"
#include "aps_db_tablenums.h"
#include "aps_db_table.h"
#include "aps_defs.h"
#include "aps_extern.h"
#include "aps_exe_names.h"
#include "apspath.h"
#include "mu_utilities.h"
#include "apsfiledef.h"
#include "timeconv.h"

/* within this file:  */
/* 
-- this is to be able to toggle between the time brackets for 
-- REQQ and non-REQQ files, when the user clicks on different file types.  
*/
static char     init_strttime[ASF_TIME_STR_LENGTH+1] ;
static char     init_stoptime[ASF_TIME_STR_LENGTH+1] ;
static char     reqq_strttime[ASF_TIME_STR_LENGTH+1] ;
static char     reqq_stoptime[ASF_TIME_STR_LENGTH+1] ;

#define CONTEXT_MACRO_ACCESS
#include "vc_crtapsfile.h"
#include "vc_phaseselect.h"
#undef CONTEXT_MACRO_ACCESS
#include "vc_msgbox.h"
#include "cb_crtapsfile.h"
#include "cb_datetime.h"
#include "subprocess.h"
#include "gui_utils.h"
#include "gui_mu_utils.h"

#include "xlate.h"
#include <odldef.h>
#include "faifdefs.h"


/*==============================================================================
    Function Declarations
==============================================================================*/
extern void     popup_message() ;


/*==============================================================================
    Global Variables
==============================================================================*/
extern Widget   UxTopLevel;

extern Widget   apsfilegen_form ;
extern char     display_string[] ;
extern Widget   filebox ;

extern Widget   apsfilegen_form ;
extern Widget   apsphaseselect_form ;

/*
-- NOTE: this could be another entry in the REPORT struct, but
-- "extend time" is non-permanent
*/
static char     *extendTimeReports[] =
                    { ME1E_TYPE, ME2E_TYPE, MR1E_TYPE, NULL } ;
static int      *extendTimeArray=NULL ; /* malloc'ed: one entry per reports[] */

REPORT   *curr_aps_file ;   /* reports info for the current report    */
char *DMAP_filename ;       

static llist        *phases ;

static int      currPermId = 0 ;    /* MU perm ID for the current report, or 0*/
static int      fa_xfer  = 0 ;      /* flag set if doing an FA transfer       */
static int      ims_xfer = 0 ;      /* flag set if doing an IMS transfer      */
static int      ims_xfer2 = 0 ;     /* flag set if doing a 2nd IMS transfer   */
static int      first_time_in_routine = 1 ;
static int      first_time_REQQ       = 1 ;

/*==============================================================================
    Macros/Constants
==============================================================================*/
#define FA_AND_IMS              1
#define IMS_ONLY                2
#define CMD_STRING_LEN          512

#define ASF_HOUR_POS    9   /* position of first hour char in ASF time string */
#define ASF_DAY_START   "00:00:00.000"  /* start of day hh:mm:ss.uuu */
#define CRT_QUIT        YES+NO+100  /* needs to be different than YES or NO */

#define BEGIN_CONTEXT(widget) \
    _UxCAPSFileGeneration       *UxSaveCtx; \
    UxSaveCtx           = UxAPSFileGenerationContext; \
    UxAPSFileGenerationContext = \
            (_UxCAPSFileGeneration *) UxGetContext( widget ) ; \
    {

#define END_CONTEXT \
    } \
    UxAPSFileGenerationContext = UxSaveCtx;



/*==============================================================================
    Create Functions (Transfer Functions follow)
==============================================================================*/

/*==============================================================================
Function:       set_capsfile_form_sensitivity

Description:    Sets the file generation form to the specified
                sensitivity, and if there is a current MU permission,
                releases it.

Parameters:

Returns:

Creator:        Ron Green

Creation Date:  Fri May 26 10:44:20 PDT 1995

Notes:
==============================================================================*/
static void
set_capsfile_form_sensitivity(process, sensitivity)

    PROCESS_INFO *process ;
    Boolean sensitivity ;
{
BEGIN_CONTEXT( apsfilegen_form )

    /* Release permissions if necessary */
    if (currPermId)
    {
        if ( gui_free_permission( currPermId,
                curr_aps_file->mu_activityid,
                MU_SINGLE_ACTIVITY_TYPE ) <= 0 )
        {
            (void) sprintf( display_string,
                "Couldn't release the %s permission: type %s, no. %d\n",
                MU_SINGLE_ACTIVITY_TYPE, curr_aps_file->mu_activityid,
                currPermId ) ;
            popup_message(XmDIALOG_WARNING, "APS:WARNING",
                display_string, XtGrabNone) ;
        }
        currPermId = 0 ;
    }

    if ((sensitivity && !currPermId) || !sensitivity)
    {
        XtSetSensitive(pushButton_CreateReportFile, sensitivity) ;
        XtSetSensitive(pushButton_transfer, sensitivity) ;
        XtSetSensitive(scrolledList_reports, sensitivity) ;
    }

END_CONTEXT
}

/*==============================================================================
Function:       create_report_done

Description:    Called when a subprocess started by cb_create_report has 
                finished.

Parameters:

Returns:

Creator:        Philip Yurchuk

Creation Date:  5/14/97

Notes:
==============================================================================*/
static void
create_report_done(process, aps_file)

    PROCESS_INFO *process ;
    REPORT *aps_file ;
{
BEGIN_CONTEXT( apsfilegen_form )

    switch (process->exit_status)
    {
      case APS_EXIT_OK :
        
        (void) sprintf(display_string,
                       "APS File/Report Generation:\n\nCompleted Successfully!\n\nThe following file was created:\n%s\n", process->original_filename);
        popup_message(XmDIALOG_INFORMATION, "APS:INFORMATION",
                          display_string, XtGrabNone) ;
        break ;
        
      case APSGUI_EXIT_COREDUMP : /* process core dumped */
        
            (void) sprintf( display_string,
                           "%s:\n\nCompleted abnormally\nSignal Caught CORE DUMPED!\n\nThe following file was NOT created:\n%s\n",
                           "APS File/Report Generation", process->original_filename) ;
        popup_message(XmDIALOG_ERROR, "APS:ERROR",
                      display_string, XtGrabNone) ;
        break ;
        
      case APS_EXIT_ERROR :
        
            (void) sprintf(display_string,
                           "%s:\n\nCompleted abnormally\nFile not created!\n\nThe following file was NOT created:\n%s\n",
                           "APS File/Report Generation", process->original_filename);
            popup_message(XmDIALOG_ERROR, "APS:ERROR",
                              display_string, XtGrabNone) ;
            break ;
        
      case APS_REPORT_NONE       :
        
        (void) sprintf(display_string,
                       "%s:\n\nNo data record found\nFile not created!\n\nThe following file was NOT created:\n%s\n",
                       "APS File/Report Generation", process->original_filename);
        popup_message(XmDIALOG_INFORMATION, "APS:INFORMATION",
                      display_string, XtGrabNone) ;
        break ;
        
      case APS_REPORT_ERROR      :

        (void) sprintf(display_string,
                       "%s:\n\nCompleted abnormally\nFile not created!\n\nThe following file was NOT created:\n%s\n",
                       "APS File/Report Generation", process->original_filename);
        popup_message(XmDIALOG_ERROR, "APS:ERROR",
                      display_string, XtGrabNone) ;
        break ;
        
      case APS_REPORT_INCOMPLETE :

        (void) sprintf(display_string,
                       "%s:\n\nOutput File/Report is incomplete!\n\nThe following file was NOT created:\n%s\n",
                       "APS File/Report Generation", process->original_filename);
        popup_message(XmDIALOG_ERROR, "APS:ERROR",
                      display_string, XtGrabNone) ;
        break ;
        
      case APS_REPORT_FILE_ERROR :

        (void) sprintf(display_string,
                       "APS File/Report Generation:\n\nFile I/O Error\n\nThe following file was NOT created:\n%s\n", process->original_filename) ;
        popup_message(XmDIALOG_ERROR, "APS:ERROR",
                      display_string, XtGrabNone) ;
        break ;
        
      case APS_REPORT_DB_ERROR   :

        (void) sprintf(display_string,
                       "APS File/Report Generation:\n\nAPS Database error\n\nThe following file was NOT created:\n%s\n", process->original_filename) ;
        popup_message(XmDIALOG_ERROR, "APS:ERROR",
                      display_string, XtGrabNone) ;
        break ;
        
      case APS_REPORT_NO_METADATA :

        (void) sprintf(display_string,
                       "%s:\n\nUnable to create\nMeta Data for File Transfer\n\nThe following file was NOT created:\n%s\n",
                       "APS File/Report Generation", process->original_filename);
        popup_message(XmDIALOG_ERROR, "APS:ERROR",
                      display_string, XtGrabNone) ;
        break ;
      
      default :

        /* if process caught a signal, but no core dump */
        if (process->exit_status < 0)
          {
            (void) sprintf( display_string,
                           "%s:\n\nCompleted abnormally\n\
SIGNAL Caught (signal = %d)!\n\nThe following file was NOT created:\n%s\n",
                           "APS File/Report Generation",
                           -(process->exit_status), process->original_filename ) ;
            popup_message(XmDIALOG_ERROR, "APS:ERROR",
                          display_string, XtGrabNone) ;
            break ;
          }
        
        (void) sprintf(display_string,
                       "%s:\n\nStatus %d returned by %s\n",
                       "APS File/Report Generation",
                       process->exit_status, process->cmd) ;
        popup_message(XmDIALOG_ERROR, "APS:ERROR",
                      display_string, XtGrabNone) ;
    }

    /* Check to see if we have to create a companion DMAP file */
    if ((strcmp(curr_aps_file->type, AWOS_TYPE) == 0) ||
        (strcmp(curr_aps_file->type, MWOS_TYPE) == 0))
    {
        create_DMAP_file() ;
        return ;
    }

/* Free permissions and set sensitivity */
    else
        set_capsfile_form_sensitivity(process, (void *)TRUE) ;

END_CONTEXT
}


/*==============================================================================
Function:       init_create_xfer

Description:    Does the set up common to the start of an external agency
                or an archive transfer.

Parameters:     None

Returns:        the return value from the call to the permission request
                function:
                    > 0, the permission id
                    = 0, permission not granted; is blocked
                    < 0, an mu library error occurred

Creator:        Teresa McKillop

Creation Date:  01/30/97

Notes:
==============================================================================*/
static int
init_create_xfer()
{
    /*
    -- lock out further create/transfers until this one is done
    */
    set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, FALSE);

    /*
    --  get permission to create/transfer the selected item
    */
    if ( (currPermId = gui_get_single_permission(
            curr_aps_file->mu_activityid )) <= 0)
    {
        /* error popup already done in subroutine */
        /* unlock to allow another create/transfer */
        set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
    }

    return (currPermId) ;
}


/*==============================================================================
Function:       init_extendTimeArray

Description:    initializes the "extend time" array: for each "reports[]"
                the corresponding entry is set if that report is eligible
                to have its start/stop times extended to represent complete
                days.

Parameters:     None

Returns:        the array, else NULL if could not allocate the memory.

Creator:        Teresa McKillop

Creation Date:  02/14/97

Notes:          
==============================================================================*/
static int *
init_extendTimeArray()
{
    char    *strPtr ;
    int     numReports ;
    int     i ;
    int     j ;

    if (extendTimeArray != NULL)
        return (extendTimeArray) ;

    /* get the number of reports */
    for (numReports = 0 ; reports[numReports].type != NULL ; numReports++)
        ;

    /* get the space for the "extend time" array */
    extendTimeArray = calloc( 1, numReports * sizeof( int ) ) ;
    if (extendTimeArray == NULL )
    {
        (void) sprintf( display_string,
                "Can't do file generation: out of memory (malloc errno: %d)",
                errno ) ;
        popup_message( XmDIALOG_ERROR, "APS:FILE ERROR",
                display_string, XtGrabNone );
        return (NULL) ;
    }

    /* initialize the "extend time" array elements */
    for (i = 0 ; i < numReports ; i++)
    {
        strPtr = reports[i].type ;

        for (j = 0 ; extendTimeReports[j] != NULL ; j++)
        {
            if (!strcmp( strPtr, extendTimeReports[j] ))
            {
                extendTimeArray[i] = 1 ;
                break ;
            }
        }
    }

    return (extendTimeArray) ;
}


/*==============================================================================
Function:       verifyCreate

Description:    verifies that user wants to create the current report with
                the given parameters.

Parameters:     widget          - parent widget for the dialogs
                starttime       - beginning time for the data in the report.
                stoptime        - ending time for the data in the report.
                extendedTimes   - flag set if using extended times

Returns:        YES         - create the report with these parameters
                NO          - do NOT create the report with these parameters
                CRT_QUIT    - quit: user chooses not to create any report

Creator:        Teresa McKillop

Creation Date:  02/17/97

Notes:          
==============================================================================*/
static int
verifyCreate( Widget widget, char *starttime, char *stoptime,
        int extendedTimes, char **filename, int reqq_phase_number )
{
    double      et_total_days ;
    char        *crar_pathname ;
    char        msg[BUFSIZ] ;
    char        filetype[20] ;   /*  value will be like:  "REQQ phase 99 "  */
                                 /*  or:                  "REQW"            */

    /* calculate the total days of the report */
    tc_et_ASF_datetime_diff(starttime, stoptime, &et_total_days) ;

    /* if NOT extended times, display the total days */
    if (!extendedTimes)
    {
        (void) sprintf(display_string, "%.2f", et_total_days) ;
        XmTextFieldSetString(TF_report_total_days, display_string) ;
    }

    /* get the filename for output */
    *filename = XmTextFieldGetString(textField_reportname) ;

    if (gui_ask_file_replacement( *filename ) == NO)
        return (CRT_QUIT) ;

    /*
    -- execute the corresponding function
    -- of the selected item in the list
    */

    if( reqq_phase_number <= 0 )
        strcpy( filetype, curr_aps_file->type ) ;
    else
    {
        /* this is REQQ; there is a reqq_phase_number */
        sprintf( filetype, 
            "%s phase %d ", curr_aps_file->type, reqq_phase_number ) ;
    }
    /* form the appropriate message */
    if (extendedTimes)
    {
        /* 
        -- filetype is copied from curr_aps_file->type above, 
        -- possibly modified to include "phase <reqq_phase_number>"
        */
        (void) sprintf( msg,
                "Create file type: %s\n\n***NOTE***: TIMES have been EXPANDED to the day boundaries\n    START: %s\n    STOP:  %s\n    #DAYS: %.2f",
                filetype, starttime, stoptime, et_total_days ) ;
    }
    else
    {
        (void) sprintf( msg,
                "Create file type: %s\n\nSTART: %s\nSTOP:  %s\n#DAYS: %.2f",
                filetype, starttime, stoptime, et_total_days ) ;
    }
        
    /* form the question using the message */
    if (strcmp( curr_aps_file->type, "CRAR" ) == 0)
    {
        crar_pathname = aps_fullpath(APS_RADARSAT_RAR, NULL) ;
        (void) sprintf(question,
            "CRAR file(s) default to directory\n   %s\n\n%s\n",
            crar_pathname, msg ) ;
        free(crar_pathname) ;
    }
    else
    {
        (void) sprintf(question, "%s\n", msg) ;
    }

    /* Ask the user if should do the create */
    return (AskUser(widget, question, YES)) ;
}


/*==============================================================================
Function:       update_reqq_phase_list
Description:    This will fill in the REQQ Phase Selection scrolled list.
Parameters:
Returns:
Creator:        Philip Yurchuk
Creation Date:  8/17/97
Notes:
==============================================================================*/
void
update_reqq_phase_list(Widget widget, XtPointer client_data, XtPointer cbs)
{

    cursor  ptr ;
    DB_RECORD **phase_rec ;
    char    *order_columns ;
    int i ;
    int     num_recs ;
    XmStringTable str_list ;


BEGIN_CONTEXT( widget )

    /* Initialize the llist */

    if (phases)
    {
        DEL_LIST( phases ) ;
        phases = NULL ;
    }

    /* Form the where clause and orderby clause */
    
    strcpy(where_clause, "where due_date >= \"1997:090:00:00:00.000\"") ;
    strcpy(orderby_cols, "reqq_id") ;

    phases = db_get_records( APS_dbproc, APS_TABLE( REQQ_PHASE ), 
                            where_clause, orderby_cols, APS_CDEFS( REQQ_PHASE ), 
                            ALL_COLS ) ;

    if (!phases)
    {
        (void) sprintf( display_string, 
            "Error with the APS DB\nNeed to fix the DB" ) ;
        popup_message( XmDIALOG_ERROR, "APS:DB ERROR" , 
            display_string, XtGrabNone ) ;

        /* since we had a db problem ensure that no items are in the list */
        XtVaSetValues( scrolledList_REQQ, 
            XmNitemCount, 0,
            NULL ) ;

        return ;
    }
    
    /* Allocate memory for the list of phases */

    num_recs = NUMELTS( phases );
    str_list = (XmStringTable) XtMalloc( num_recs * sizeof(XmString) ) ;
 
    /* Go through the records and add each one to the list */

    i = 0 ;
    for (phase_rec = (DB_RECORD **) FIRST( phases, ptr ) ; phase_rec ;
        phase_rec = (DB_RECORD **) NEXT( phases, ptr ))
    {
 
        (void) sprintf( display_string, 
                       "    %d           %17.17s        %17.17s      %17.17s",
                       CAST_REQQ_PHASE_REQQ_ID phase_rec[REQQ_PHASE_REQQ_ID], 
                       CAST_REQQ_PHASE_STRTTIME phase_rec[REQQ_PHASE_STRTTIME],
                       CAST_REQQ_PHASE_STOPTIME phase_rec[REQQ_PHASE_STOPTIME],
                       CAST_REQQ_PHASE_DUE_DATE phase_rec[REQQ_PHASE_DUE_DATE]) ;

        str_list[i++] = XmStringCreateLocalized( display_string ) ;
    }
 
 
    XtVaSetValues( widget,
        XmNitems, str_list,
        XmNitemCount, num_recs,
        NULL ) ;
 
    for (i = 0; i < num_recs ;i++)
        XmStringFree( str_list[i] ) ;
    XtFree( (char *) str_list ) ;


END_CONTEXT
}




/*==============================================================================
Function:       cb_update_capsfile_form
Description:
Parameters:
Returns:
Creator:        Ron Green
Creation Date:  mm/dd/yyyy
Notes:
==============================================================================*/
void
cb_update_capsfile_form(widget, client_data, cbs)
   Widget widget ;
   XtPointer client_data ;
   XmListCallbackStruct *cbs ;
{
BEGIN_CONTEXT( widget )

    char            *aps_fullpath_filename ;
    int             reportIndex ;

    char            *init_strttime_ptr ;
    char            *init_stoptime_ptr ;
    int             reqq_phase_number ;
    char            reqq_due_date[ASF_TIME_STR_LENGTH+1] ;
    int             return_code ;


    /*
    -- the selected item matches the file in the reports
    -- table, the scrolled list was built from the reports
    -- table
    */
    /* get current (selected) aps file info pointer:  */
    reportIndex = cbs->item_position - 1 ;
    curr_aps_file = &reports[reportIndex] ;

    /* done ONCE: initialize the "extend time" for each report */
    if (extendTimeArray == NULL)
    {
        /* 
        -- take advantage of re-initialization  
        */
        first_time_in_routine = 1 ;
        first_time_REQQ       = 1 ;

        if (init_extendTimeArray() == NULL)
            return ;
    }

    /*
    -- use aps_fullpath to prepend the full directory path to the
    -- default filename to be used when creating this file/report
    */
    aps_fullpath_filename = aps_fullpath(
        curr_aps_file->aps_fullpath_id, curr_aps_file->default_file_name) ;

    if( first_time_in_routine )
    {
        first_time_in_routine = 0 ;
        /* 
        -- save the initial time bracket into local storage, for 
        -- setting later, when a new file type is selected.  
        -- NOTE:  I tried it with a static char init_strttime[] 
        --        within the subroutine but that didn't work, so it 
        --        is global variable within this file.
        */
        init_strttime_ptr = XmTextFieldGetString(TF_report_start) ;
        (void) strcpy( init_strttime, init_strttime_ptr ) ;
        XtFree(init_strttime_ptr) ;

        init_stoptime_ptr = XmTextFieldGetString(TF_report_stop) ;
        (void) strcpy( init_stoptime, init_stoptime_ptr ) ;
        XtFree(init_stoptime_ptr) ;

		/* now display the time bracket on the screen */
		XmTextFieldSetString(TF_report_start, init_strttime) ;
        XmTextFieldSetString(TF_report_stop, init_stoptime) ;

    }

    /* now display the time bracket on the screen */
    if ( strcmp( curr_aps_file->type, REQQ_TYPE ) == 0 )
    {
        /* REQQ  */
        if( first_time_REQQ )
        {
            first_time_REQQ = 0 ;   
            /* 
            -- get and save effective start/stop 
            -- time for REQQ phase
            */
            return_code = get_current_reqq_phase( 
                &reqq_phase_number, reqq_due_date, 
                reqq_strttime, reqq_stoptime ) ;
            if( return_code < 0 )
            {
                /* error handling for get_current_reqq_phase():   */
                (void) sprintf(display_string,
                    "Error (%d) getting current REQQ phase information.\n"
                    "Please contact DBA to fill reqq_phase relation.\n"
                    "In the meantime, you will have to type the start\n"
                    "and stop times yourself.  Good luck.\n\n", return_code ) ;

                    popup_message(XmDIALOG_ERROR, "APS:ERROR", 
                        display_string, XtGrabNone);
            }

			/* Display the effective start/stop for the current phase */
			XmTextFieldSetString(TF_report_start, reqq_strttime);
			XmTextFieldSetString(TF_report_stop, reqq_stoptime);

		}
    }

    /* now display the file/report name on the screen */
    XmTextFieldSetString(textField_reportname, aps_fullpath_filename) ;
    free(aps_fullpath_filename) ;

    XtSetSensitive (pushButton_view, TRUE) ;

    /* manage/unmanage "extend time" toggle button */
    if (extendTimeArray[reportIndex] && !XtIsManaged( ExtendTimes_tb ))
        XtManageChild( ExtendTimes_tb ) ;
    if (!extendTimeArray[reportIndex] && XtIsManaged( ExtendTimes_tb ))
        XtUnmanageChild( ExtendTimes_tb ) ;

    /* if it's an REQQ file, pop up the Phase selection dialog */
    if (strcmp(curr_aps_file->type, REQQ_TYPE) == 0)
        XtPopup( XtParent(apsphaseselect_form), XtGrabNone) ;

    /*
    -- Set up the appropiate transfer callback for this function
    */
    XtRemoveAllCallbacks(pushButton_transfer, XmNactivateCallback) ;

    if (curr_aps_file->transfer_func)
    {
        XtAddCallback(pushButton_transfer, XmNactivateCallback,
                      (XtCallbackProc) curr_aps_file->transfer_func,
                      (XtPointer) curr_aps_file) ;
        XtSetSensitive (pushButton_transfer, TRUE) ;
    }
    else
    {
        XtSetSensitive (pushButton_transfer, FALSE) ;
    }

    /*
    -- if the entry for curr_aps_file->func is NULL, it indicates the file
    -- is available through an outside source (transferred in), so
    -- the Create/Import button is made unavailable (ie, insensitive)
    --
    */
    XtRemoveAllCallbacks(pushButton_CreateReportFile, XmNactivateCallback) ;
    if (curr_aps_file->func || curr_aps_file->create_executable)
    {
        XtAddCallback(pushButton_CreateReportFile, XmNactivateCallback,
            (XtCallbackProc) cb_create_report, NULL) ;
        XtSetSensitive (pushButton_CreateReportFile, TRUE) ;
    }
    else
    {
        XtSetSensitive (pushButton_CreateReportFile, FALSE) ;
    }

END_CONTEXT
}



/*==============================================================================
Function:           cb_show_reports
Description:
Parameters:
Returns:
Creator:        Ron Green
Creation Date:  mm/dd/yyyy
Notes:
==============================================================================*/
/* ARGSUSED2 */
void
cb_show_reports(widget, client_data, cbs)
    Widget widget ;
    XtPointer client_data, cbs ;
{
    int i, nreports ;
    char asftime_report_start[22] ;
    char asftime_report_stop[22] ;

    XmStringTable str_list ;

    Widget list_widget = (Widget ) client_data ;

BEGIN_CONTEXT( list_widget )

    for (nreports = 0; reports[nreports].type ; nreports++)
        ;

    str_list = (XmStringTable) XtMalloc(nreports * sizeof(XmString)) ;

    for (i = 0 ; i < nreports ; i++)
    {
        (void) sprintf(display_string, "%-15s%-40s",
            reports[i].type, reports[i].name) ;
        str_list[i] = XmStringCreateLocalized(display_string) ;
    }

    XtVaSetValues((Widget) list_widget,
        XmNitems, str_list,
        XmNitemCount, nreports,
        NULL) ;

    for (i = 0 ; i < nreports ; i++)
        XmStringFree(str_list[i]) ;
    XtFree((char *) str_list) ;

    /* now display the current date as the starting time */
    tc_systime2asf(asftime_report_start) ;
    /* TO DO check return value */

    /*
    -- overwrite the hours, min. sec, etc with zeros
    -- so we always start at the beginning of the day
    -- "YYYY:DD:HH:MM:SS:000"
    */
    (void) sprintf(asftime_report_start+ASF_HOUR_POS, "%s", ASF_DAY_START ) ;
    XmTextFieldSetString(TF_report_start, asftime_report_start) ;
    
    /* convert to ephemeris time and add the # days to the start time */
    tc_asf_add_ndays( asftime_report_start, (double) 7.0, asftime_report_stop) ;

    XmTextFieldSetString(TF_report_stop, asftime_report_stop) ;
    XmTextFieldSetString(TF_report_total_days, "    7.00") ;
    

END_CONTEXT
}


/*==============================================================================
Function:       create_DMAP_file

Description:    Initiate a subprocess to create a DMAP file.

Parameters:     None

Returns:        None

Creator:        Philip Yurchuk

Creation Date:  5/9/97

Notes:
==============================================================================*/
void create_DMAP_file()
{
    PROCESS_INFO    *process ;
    int             reportIndex ;
    char            *starttime ;
    char            *stoptime ;
    char            *aps_fullpath_filename ;
    int             status ;

BEGIN_CONTEXT( apsfilegen_form )

    starttime = XmTextFieldGetString(TF_report_start) ;
    stoptime  = XmTextFieldGetString(TF_report_stop) ;

    /* 
    -- Remember, the ADDM/MDDM files always come directly after the
    -- AWOS/MWOS files.  So we just have to increment reportIndex to
    -- get the right file.
    */
    reportIndex = gui_XmList_selected_item(scrolledList_reports) - 1 ;
    curr_aps_file = &reports[++reportIndex] ;
    
    /*
    -- use aps_fullpath to prepend the full directory path to the
    -- default filename to be used when creating this file/report
    */
    aps_fullpath_filename = aps_fullpath(curr_aps_file->aps_fullpath_id, 
                                         curr_aps_file->default_file_name) ;

    (void) sprintf(display_string,
                   "%s -t %s -b %s -e %s -U %s -P %s -p %d -o %s",
                   curr_aps_file->create_executable, curr_aps_file->type,
                   starttime, stoptime, userid, password, currPermId, 
                   aps_fullpath_filename );

    process = (PROCESS_INFO *) create_process(display_string, &status,
                                              TRUE, NULL, 
                                              gui_display_message_widget, 
                                              scrolledText_create_report,
                                              create_report_done, 
                                              curr_aps_file) ;

    if (process != NULL)
    {
        (void) sprintf(process->original_filename, "%s", aps_fullpath_filename);

        if (start_process(process) > OK)
        {
            set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
            destroy_process(process);
        }
    }

    free(aps_fullpath_filename) ;
    XtFree(starttime) ;
    XtFree(stoptime) ;

END_CONTEXT
}


/*==============================================================================
Function:       cb_create_report

Description:    Initiate a subprocess to create a specified report/file

Parameters:     Standard X Parameters

Returns:        None

Creator:        Ron Green

Creation Date:  XX/XX/XXXX

Notes:
==============================================================================*/
/* ARGSUSED1 */
void
cb_create_report(widget, client_data, cbs)
    Widget widget ; XtPointer client_data, cbs ;
{
BEGIN_CONTEXT( widget )

    PROCESS_INFO    *process ;
    int             reportIndex ;
    char            *filename = NULL ;
    char            *starttime ;
    char            *stoptime ;
    char            extendStartTime[ASF_TIME_STR_LENGTH+1] ;
    char            extendStopTime[ASF_TIME_STR_LENGTH+1];
    int             adjustStartTime = 0;
    int             adjustStopTime  = 0;
    int             answer = NO ;
    int             status ;
    int             reqq_phase_number = 0 ;
    char            filetype[20] ;
    int             return_code ;

    /* done ONCE: initialize the "extend time" for each report */
    if (extendTimeArray == NULL)
    {
        if (init_extendTimeArray() == NULL)
            return ;
    }

    reportIndex = gui_XmList_selected_item(scrolledList_reports) - 1 ;
    curr_aps_file = &reports[reportIndex] ;

    if ( init_create_xfer() <= 0 )
        return ;    /* error popup already done in subroutine */

    /* activate the start/stop time fields for error validation */
    XtCallActionProc(TF_report_start, "activate", NULL, NULL, 0) ;
    if (ASF_datetime_err)
    {
        set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
        return ;
    }

    XtCallActionProc(TF_report_stop, "activate", NULL, NULL, 0) ;
    if (ASF_datetime_err)
    {
        set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
        return ;
    }

    starttime = XmTextFieldGetString(TF_report_start) ;
    stoptime  = XmTextFieldGetString(TF_report_stop) ;

    if (time_range_error(starttime, stoptime))
    {
        set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
        XtFree(starttime) ;
        XtFree(stoptime) ;
        return ;
    }

    /* modification for REQQ */
    strcpy( filetype, curr_aps_file->type ) ;

    if ( strcmp( curr_aps_file->type, REQQ_TYPE ) == 0 )
    {
        reqq_phase_number = get_reqq_phase_no( starttime ) ;
        if( reqq_phase_number <= 0 )
        {
            /* error handling for get_current_reqq_phase():   */
            (void) sprintf(display_string,
                "Error (%d) getting current REQQ phase number for %s.\n"
                "Please contact DBA to fill reqq_phase relation.\n\n",
                return_code, starttime ) ;
                popup_message(XmDIALOG_ERROR, "APS:ERROR", 
                    display_string, XtGrabNone);
        }
        else
        {
            /* 
            -- This is a REQQ creation:  
            -- add "-r n " onto the end of filetype, 
            -- where n = reqq_phase_number
            */
            sprintf( filetype, "%s -r %d ", 
                curr_aps_file->type, reqq_phase_number ) ;
        }
    }

    /* see if should be using extended times */
    if ((extendTimeArray[reportIndex] &&
            XmToggleButtonGetState( ExtendTimes_tb ) == True) &&
        ((adjustStartTime =
            strcmp( starttime+ASF_HOUR_POS, ASF_DAY_START )) != 0 ||
         (adjustStopTime =
            strcmp( stoptime+ASF_HOUR_POS, ASF_DAY_START )) != 0))
    {
        /* copy the gui times */
        (void) strcpy( extendStartTime, starttime ) ;
        (void) strcpy( extendStopTime, stoptime ) ;

        /* at least one of the times needs to be adjusted to the extend time */
        if (adjustStartTime)
        {
            /* start time: truncate to day start */
            (void) strcpy( extendStartTime+ASF_HOUR_POS, ASF_DAY_START ) ;
        }
            
        if (adjustStopTime)
        {
            /* stop time: add 1 day then truncate to day start */
            tc_asf_add_ndays( extendStopTime, (double) 1, extendStopTime) ;
            (void) strcpy( extendStopTime+ASF_HOUR_POS, ASF_DAY_START ) ;
        }

        /* ask if should create using the extended times */
        switch (answer = verifyCreate( widget,
                extendStartTime, extendStopTime, TRUE, &filename, 
                reqq_phase_number ))
        {
            case CRT_QUIT :
                set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
                XtFree(starttime) ;
                XtFree(stoptime) ;
                XtFree(filename) ;
                return ;
            case YES :
                /* copy the verified times */
                (void) strcpy( starttime, extendStartTime ) ;
                (void) strcpy( stoptime, extendStopTime ) ;
                break ;
            default :   /* NO */
                XtFree(filename) ;
                filename = NULL ;
                break ;
        }
    }

    if (answer == NO)
    {
        /* don't create with extended times, see if should use gui times */
        answer = verifyCreate( widget,
                starttime, stoptime, FALSE, &filename, reqq_phase_number ) ;
    }

    if (answer == YES)
    {
        /* 
        -- filetype[] is copied from curr_aps_file->type and 
        -- modified above if the file is an REQQ above.  
        */
        if (curr_aps_file->func != NULL)
        {
            (void) sprintf(display_string,
                "%s -t %s -b %s -e %s -U %s -P %s -p %d -o %s",
                FILE_CREATION_CMD   /* FILE_CREATION_CMD will run "func" */,
                filetype, starttime, stoptime,
                userid, password, currPermId, filename );
        }
        else
        {
            (void) sprintf(display_string,
                "%s -t %s -b %s -e %s -U %s -P %s -p %d -o %s",
                curr_aps_file->create_executable, filetype,
                starttime, stoptime, userid, password, currPermId, filename );
        }

        process = (PROCESS_INFO *) create_process(display_string, &status,
            TRUE, NULL, gui_display_message_widget, scrolledText_create_report,
            create_report_done, curr_aps_file) ;

        if (process != NULL)
        {
            XmTextSetString(scrolledText_create_report, "") ;
            (void) sprintf(process->original_filename, "%s", filename);

            if (start_process(process) > OK)
            {
                set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
                destroy_process(process);
            }
        }
    }
    else
        set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);

    XtFree(starttime) ;
    XtFree(stoptime) ;
    XtFree(filename) ;

END_CONTEXT

    return ;
}


/*==============================================================================
Function:       cb_capsfile_save_input_file

Description:    This function saves the file name selected/specified in
                the file selection box and update the APS File Generation
                window accordingly.

Parameters:

Returns:

Creator:        Ron Green

Creation Date:  Fri May 26 10:43:54 PDT 1995

Notes:
==============================================================================*/
static void
cb_capsfile_save_input_file(widget, client_data, call_data)
   Widget widget ;
   XtPointer client_data ;
   XmListCallbackStruct *call_data ;
{
BEGIN_CONTEXT( apsfilegen_form )

    char *file ;
    char *dir, *newfile;

    XmFileSelectionBoxCallbackStruct *cbs =
                        (XmFileSelectionBoxCallbackStruct *) call_data;
    if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &file))
                    return;
    if (*file != '/')
    {
        /* if it's not a directory, determine the full pathname
        ** of the selection by concatenating it to the "dir" part
        */
        if (XmStringGetLtoR (cbs->dir, XmFONTLIST_DEFAULT_TAG, &dir))
        {
            newfile =XtMalloc(strlen(dir)+1+strlen(file)+1);
            (void) sprintf (newfile, "%s/%s", dir, file);
            XtFree(file) ;
            XtFree(dir) ;
            file = newfile;
        }
    }

    /* remove the file box */
    XtPopdown(XtParent(widget)) ;

    if (file)
        XmTextFieldSetString(textField_reportname, file) ;

END_CONTEXT
}



/*==============================================================================
Function:       cb_capsfile_select_input

Description:    This function brings up a file selection box to allow
                the user to specify the file name to be used for file
                creation or transfer.

Parameters:

Returns:

Creator:        Ron Green

Creation Date:  03/15/1995

Notes:
==============================================================================*/
/* ARGSUSED1 */
void
cb_capsfile_select_input(   Widget widget,
                                XtPointer client_data,
                                XtPointer cbs)
{
BEGIN_CONTEXT( widget )

    XmString    dirmask ;
    int         i;
    int         pos_count = 0;
    int         *pos_list;
    char        *aps_fullpath_filename ;

    /* find the selected items */
    if (!XmListGetSelectedPos(scrolledList_reports, &pos_list,&pos_count))
    {
        popup_message(XmDIALOG_ERROR, "APS:FILE ERROR",
            "Select a file type first.", XtGrabNone);
        return;
    }

    i = pos_list[0] - 1;
    XtFree((char *) pos_list) ;

    /*
    -- use aps_fullpath to prepend the full directory path to the
    -- directory mask to be used when creating the file selection box
    */
    aps_fullpath_filename = aps_fullpath(
        reports[i].aps_fullpath_id, reports[i].dirmask) ;

    dirmask = XmStringCreateLocalized(aps_fullpath_filename) ;
    XmFileSelectionDoSearch(filebox, dirmask) ;

    XmStringFree(dirmask) ;
    free(aps_fullpath_filename) ;

    XtRemoveAllCallbacks(filebox, XmNokCallback) ;
    XtVaSetValues(filebox,
        RES_CONVERT(XmNdialogTitle, reports[i].name) ,
        NULL) ;

    XtAddCallback(filebox, XmNokCallback,
        (XtCallbackProc)cb_capsfile_save_input_file, (XtPointer *)widget) ;

    XtPopup( XtParent( filebox ), XtGrabExclusive ) ;

END_CONTEXT
}


/*==============================================================================
Function:       cb_init_extended_times

Description:    callback to initialize the "extend times" toggle button.

Parameters:     standard X Callback Parameters

Returns:        None

Creator:        Teresa McKillop

Creation Date:  02/13/97

Notes:          
==============================================================================*/
/* ARGSUSED1 */
void
cb_init_extended_times( Widget widget, XtPointer client_data, XtPointer *cbs)
{

BEGIN_CONTEXT( widget )

    typedef struct {
        Boolean extend_times ;
    } EXTEND_TIMES_VARS ;

    static XtResource resources[] = {
            { EXTEND_TIMES_XRES, EXTEND_TIMES_XCLASS,
              XtRBoolean, sizeof( Boolean ),
              XtOffsetOf( EXTEND_TIMES_VARS, extend_times ),
              XtRImmediate, (XtPointer) EXTEND_TIMES_DEF_VALUE
            }
    } ;

    EXTEND_TIMES_VARS   extend_time_vars ;

    /* get the initial value for setting the "extend times" toggle button */
    XtGetApplicationResources( UxTopLevel, &extend_time_vars,
            resources, XtNumber( resources ), NULL, 0 ) ;

    /* set the button */
    XmToggleButtonSetState( widget, extend_time_vars.extend_times, False ) ;

END_CONTEXT

    return ;
}


/*==============================================================================
Function:       CRT_remote_filename

Description:    Creates a remote filename of the format
                XXX_1995-103T23:59:59.999 where XXX is the file prefix
                passed in as a parameter.

Parameters:     char *filename_prefix
                char *pmf_filename

Returns:        char * (resulting filename)
                NULL on failure

Creator:        Q. Sun

Creation Date:  02/03/96

Revision History:

Notes:
==============================================================================*/
static char *
CRT_remote_filename(char* filename_prefix, char* pmf_filename)
{
    char *fname = NULL;
    char *fpathname = NULL;
    char *file_creation_time = NULL;

    if (filename_prefix == NULL)
        return(NULL);

    /*
    ** Extract the file creation time from the pmf file.
    */
    if ((file_creation_time = aps_getPMFKeyWordValue(
                                PMF_CREATION_TIME_KEYWORD, PMF_AGGREGATE_NAME,
                                pmf_filename)) == NULL)
    {
        return(NULL);
    }

    if ((fname = (char *)
            malloc(ASF_TIME_STR_LENGTH + strlen(filename_prefix) +2)) == NULL)
        return(NULL);
    else
    {
        (void) sprintf(fname, "%s_%s", filename_prefix, file_creation_time);
        fpathname = aps_fullpath(APS_TEMP, fname);
    }
    free(fname);
    return( fpathname ) ;
}


/*==============================================================================
Function:       CRT_YYYYDDDHHMMSSUUU_remote_filename

Description:    Creates a remote filename of the format XXX_1996103223344555
                where XXX is the file prefix passed in as a parameter.

Parameters:     char *filename_prefix
                char *pmf_filename

Returns:        char * (resulting filename)
                NULL on failure

Creator:        T. McKillop

Creation Date:  10/01/96

Revision History:

Notes:
==============================================================================*/
#define CRT_TIME_YEAR_LEN   4   /* length of the year substring in create time*/
#define CRT_TIME_DOY_LEN    3   /* length of the doy substring in create time */
#define CRT_TIME_HOUR_LEN   2   /* length of the hour substring in create time*/
#define CRT_TIME_MIN_LEN    2   /* length of the min substring in create time */
#define CRT_TIME_SEC_LEN    2   /* length of the sec substring in create time */
#define CRT_TIME_MSEC_LEN   3   /* length of the msec substring in create time*/

static char *
CRT_YYYYDDDHHMMSSUUU_remote_filename( char* filename_prefix, char* pmf_filename )
{
    char    *fname ;
    char    *fpathname ;
    char    ftime[CRT_TIME_YEAR_LEN +
                CRT_TIME_DOY_LEN +
                CRT_TIME_HOUR_LEN +
                CRT_TIME_MIN_LEN +
                CRT_TIME_SEC_LEN +
                CRT_TIME_MSEC_LEN + 1] ;
    char    *ftimePtr = ftime ; /* points to next available byte in ftime */

    char    *file_creation_time ;
    char    *tmpptr ;

    if (filename_prefix == NULL)
        return( NULL );

    /*
    -- Extract the file creation time from the pmf file
    -- (format: YYYY-DDDTHH:MM:SS.UUUZ)
    ** and format it to YYYYDDDHHMMSSUUU.
    */
    if ((file_creation_time = aps_getPMFKeyWordValue(
            PMF_CREATION_TIME_KEYWORD, PMF_AGGREGATE_NAME, pmf_filename ))
            == NULL)
    {
        return (NULL);
    }
    /* year */
    (void) memcpy( ftimePtr, file_creation_time, CRT_TIME_YEAR_LEN ) ;
    /* doy */
    ftimePtr += CRT_TIME_YEAR_LEN ;
    tmpptr   = file_creation_time + CRT_TIME_YEAR_LEN + 1;  /* skip the hyphen*/
    (void) memcpy( ftimePtr, tmpptr, CRT_TIME_DOY_LEN ) ;
    /* hour */
    ftimePtr += CRT_TIME_DOY_LEN ;
    tmpptr   += CRT_TIME_DOY_LEN + 1;   /* skip the "T" */
    (void) memcpy( ftimePtr, tmpptr, CRT_TIME_HOUR_LEN ) ;
    /* minutes */
    ftimePtr += CRT_TIME_HOUR_LEN ;
    tmpptr   += CRT_TIME_HOUR_LEN + 1;  /* skip the colon */
    (void) memcpy( ftimePtr, tmpptr, CRT_TIME_MIN_LEN ) ;
    /* seconds */
    ftimePtr += CRT_TIME_MIN_LEN ;
    tmpptr   += CRT_TIME_MIN_LEN + 1;   /* skip the "T" */
    (void) memcpy( ftimePtr, tmpptr, CRT_TIME_SEC_LEN ) ;
    /* milliseconds */
    ftimePtr += CRT_TIME_SEC_LEN ;
    tmpptr   += CRT_TIME_SEC_LEN + 1;   /* skip the "T" */
    (void) memcpy( ftimePtr, tmpptr, CRT_TIME_MSEC_LEN ) ;
    *(ftimePtr + CRT_TIME_MSEC_LEN) = STREND ;

    if ((fname = (char *) malloc(
            ASF_TIME_STR_LENGTH + strlen( filename_prefix) +2 )) == NULL )
    {
        return (NULL );
    }
    else
    {
        (void) sprintf( fname, "%s_%s", filename_prefix, ftime );
        fpathname = aps_fullpath( APS_TEMP, fname );
    }
    free( fname );

    return( fpathname ) ;
}


/*==============================================================================
Function:       remote_STGS_filename

Description:    Creates the STGS (Reply File for REQR) filename used by ADEOS

Parameters:     char *pmf_filename

Returns:        char * (resulting filename)

Creator:        Q. Sun

Creation Date:  02/05/96

Revision history:

Notes:
==============================================================================*/
char *
remote_STGS_filename(char *pmf_filename)
{
char *fname = NULL;
char *fullpath_fname = NULL;

    /*
    ** Extract the file creation time from the pmf file.
    */
    if ((fname = aps_getPMFKeyWordValue(
                                PMF_FILENAME_KEYWORD, PMF_AGGREGATE_NAME,
                                pmf_filename)) == NULL)
    {
        return(NULL);
    }

    fullpath_fname = aps_fullpath(APS_TEMP, fname);
    free(fname);
    return(fullpath_fname) ;
}


/*==============================================================================
Function:       remote_REUG_filename

Description:    Creates the REUG (Unavailability Report) filename used by ESA

Returns:        char * (resulting filename)

Creator:        Q. Sun

Creation Date:  02/05/96

Revision history:

Notes:
==============================================================================*/
char *
remote_REUG_filename(char *pmf_filename)
{
char *fname = NULL;
char *fullpath_fname = NULL;

    /*
    ** Extract the file creation time from the pmf file.
    */
    if ((fname = aps_getPMFKeyWordValue(
                                PMF_FILENAME_KEYWORD, PMF_AGGREGATE_NAME,
                                pmf_filename)) == NULL)
    {
        return(NULL);
    }

    fullpath_fname = aps_fullpath(APS_TEMP, fname);
    free(fname);
    return(fullpath_fname) ;
}


/*==============================================================================
Function:       remote_REQW_filename

Description:    Creates the REQW (J-ERS-1 Weekly Request Fil) filename
                used by NASDA

Parameters:     char *pmf_filename

Returns:        char * (resulting filename)

Creator:        Q. Sun

Creation Date:  02/05/96

Revision history:

Notes:
==============================================================================*/
/* ARGSUSED0 */
char *
remote_REQW_filename(char *pmf_filename)
{
char fname[] = "REQW";

    return(aps_fullpath(APS_TEMP, fname));
}


/*==============================================================================
Function:       remote_REQQ_filename

Description:    Creates the REQQ (J-ERS-1 Quarterly Request File) filename
                used by NASDA

Parameters:     char *pmf_filename

Returns:        char * (resulting filename)

Creator:        Q. Sun

Creation Date:  02/05/96

Revision history:

Notes:
==============================================================================*/
/* ARGSUSED0 */
char *
remote_REQQ_filename(char *pmf_filename)
{
char fname[] = "REQQ";

    return(aps_fullpath(APS_TEMP, fname));
}


/*==============================================================================
Function:       remote_MSGE_filename

Description:    Creates an MSGE (J-ERS-1 Reply File for MDR Dumps (REQM))
                filename of the format MSGE_1995-103T23:59:59.999, for NASDA

Parameters:     char *pmf_filename

Returns:        char * (resulting filename)

Creator:        Q. Sun

Creation Date:  02/05/96

Revision history:

Notes:
==============================================================================*/
char *
remote_MSGE_filename(char *pmf_filename)
{
    return(CRT_remote_filename("MSGE", pmf_filename));
}


/*==============================================================================
Function:       remote_MSGF_filename

Description:    Creates an MSGF (ASF Down Times Report) filename of the
                format MSGF_1995-103T23:59:59.999, for NASDA

Parameters:     char *pmf_filename

Returns:        char * (resulting filename)

Creator:        Q. Sun

Creation Date:  02/05/96

Revision history:

Notes:
==============================================================================*/
char *
remote_MSGF_filename(char *pmf_filename)
{
    return(CRT_remote_filename("MSGF", pmf_filename));
}


/*==============================================================================
Function:       remote_wos_filename

Description:    Creates a WOS filename of the format
                WOS_1995-103T23:59:59.999

Parameters:     char* pmf_filename

Returns:        char * (resulting filename)

Creator:        Ron Green

Creation Date:  04/19/1995

Revision history:
                02/03/96 -QS- Modify for R1B prime
Notes:
==============================================================================*/
char *
remote_wos_filename(char *pmf_filename)
{
    return(CRT_remote_filename("WOS", pmf_filename));
}


/*==============================================================================
Function:       remote_ADDM_filename

Description:    Creates an ADDM filename of the format
                ADDM_1995-103T23:59:59.999

Parameters:     char* pmf_filename

Returns:        char * (resulting filename)

Creator:        Philip Yurchuk

Creation Date:  04/25/1997

Revision history:

Notes:
==============================================================================*/
char *
remote_ADDM_filename(char *pmf_filename)
{
    return(CRT_remote_filename("ADDM", pmf_filename));
}


/*==============================================================================
Function:       remote_MDDM_filename

Description:    Creates an MDDM filename of the format
                MDDM_1995-103T23:59:59.999

Parameters:     char* pmf_filename

Returns:        char * (resulting filename)

Creator:        Philip Yurchuk

Creation Date:  04/25/1997

Revision history:

Notes:
==============================================================================*/
char *
remote_MDDM_filename(char *pmf_filename)
{
    return(CRT_remote_filename("MDDM", pmf_filename));
}


/*==============================================================================
Function:               remote_sv_filename

Description:    Creates a Predicted State Vector filename of the format
                SV_1995-103T23:59:59.999

Parameters:     char *pmf_filename

Returns:        char * (resulting filename)
                NULL if fail

Creator:                Nadia Adhami

Creation Date:  04/26/1995

Modification History:
                02/02/96 -QS- Modify for R1B'

Notes:  02/02/96 -QS- Modify
==============================================================================*/
char *
remote_sv_filename(char *pmf_filename)
{
    return(CRT_remote_filename("SV", pmf_filename));
}


/*==============================================================================
Function:       remote_WFF_WOS_filename

Description:    Creates a Weekly Operations Schedule filename for
                McMurdo/Wallops

Parameters:     char *pmf_filename

Returns:        char * (resulting filename)

Creator:        Teresa McKillop

Creation Date:  10/01/96

Revision history:

Notes:
==============================================================================*/
char *
remote_WFF_WOS_filename( char *pmf_filename )
{
    return (CRT_YYYYDDDHHMMSSUUU_remote_filename( "WOS", pmf_filename )) ;
}


/*==============================================================================
Function:       remote_WFF_EPH_filename

Description:    Creates a Weekly Operations Schedule filename for
                McMurdo/Wallops

Parameters:     char *pmf_filename

Returns:        char * (resulting filename)

Creator:        Teresa McKillop

Creation Date:  10/01/96

Revision history:

Notes:
==============================================================================*/
char *
remote_WFF_EPH_filename( char *pmf_filename )
{
    return (CRT_YYYYDDDHHMMSSUUU_remote_filename( "EPH", pmf_filename )) ;
}


/*==============================================================================
Function:       remote_WFF_REQ_filename

Description:    Creates a Request For Availability Report filename
                for McMurdo/Wallops

Parameters:     char *pmf_filename

Returns:        char * (resulting filename)

Creator:        Ron Green

Creation Date:  Thu Jun 22 13:50:45 PDT 1995

Revision history:
                02/03/96 -QS- Modify for R1B prime

Notes:
==============================================================================*/
char *
remote_WFF_REQ_filename(char *pmf_filename)
{
    return (CRT_YYYYDDDHHMMSSUUU_remote_filename( "REQ", pmf_filename )) ;
}


/*==============================================================================
Function:       CRT_local_filename

Description:    Gets the name of the local APS filename.
                This information is gotten from the Create Files GUI
                The default filename may be found in the apsfiledef.c file
                However the user has the ability to change it

Parameters:     None

Returns:        char * (resulting filename)
                OR empty string

Creator:        Ron Green

Creation Date:  04/19/1995

Notes:
==============================================================================*/
char *
CRT_local_filename()
{
    char *tmpFname ;
    char *fname ;

BEGIN_CONTEXT( apsfilegen_form )

    /* read the file name from the gui text field */
    tmpFname    = gui_TF_string(textField_reportname) ;
    /* change it to an alloc'ed string and XtFree() the original */
    fname       = malloc( strlen( tmpFname ) + 1 ) ;
    (void) strcpy( fname, tmpFname ) ;
    XtFree( tmpFname ) ;

END_CONTEXT

    return( fname ) ;
}


/*==============================================================================
    Transfer Functions (Create Functions precede here)
==============================================================================*/

/*==============================================================================
Function:       aps_getPMFKeyWordValue

Description:    This routine extracts and returns the value of a specified
                keyword from the pmf file.

Parameters:     char* keyword
                char* targetAggregate: The name of the object that contains
                                        the target keyword.
                char* pmfFileName: The name of the pmf file name.

Returns:        (char* ) the value of the specified keyword

Creator:        Q. Sun

Creation Date:  02/02/96

Notes:
==============================================================================*/
char *
aps_getPMFKeyWordValue( char* keyword,
                        char* targetAggregate,
                        char* pmfFileName)
{
    char *keyword_value = NULL;
    FILE *fd = NULL;
    AGGREGATE rootAggregate = NULL;
    AGGREGATE currAggregate = NULL;
    PARAMETER currParameter = NULL;
    VALUE currValue = NULL;
    unsigned int milliseconds = 0;

    /*
    ** Open the Metadata file for read.
    */
    if ((fd = fopen (pmfFileName, "r")) == (FILE *) NULL)
    {
        return(NULL);
    }

    /*
    ** Allocate the structure for the root node of the ODL tree.
    */
    if ((rootAggregate = NewAggregate (NULL, KA_GROUP, "root", NULL)) ==
                                                            (AGGREGATE) NULL)
    {
        (void) fclose(fd);
        return(NULL);
    }

    /*
    ** Call the ODL function to read and parse the Metadata file
    ** into the ODL tree.
    */
    if ((ReadLabel (fd, rootAggregate)) == 0)
    {
        RemoveAggregate(rootAggregate);
        (void) fclose(fd);
        return(NULL);
    }
    if ((currAggregate = FindAggregate(rootAggregate, targetAggregate))
                                                        == (AGGREGATE) NULL)
    {
        RemoveAggregate(rootAggregate);
        (void) fclose(fd);
        return(NULL);
    }

    if ((currParameter = FindParameter(currAggregate, keyword)) == NULL)
    {
        RemoveAggregate(currAggregate);
        RemoveAggregate(rootAggregate);
        (void) fclose(fd);
        return(NULL);
    }

    if ((currValue = FirstValue (currParameter)) == NULL)
    {
        RemoveParameter(currParameter);
        RemoveAggregate(currAggregate);
        RemoveAggregate(rootAggregate);
        (void) fclose(fd);
        return(NULL);
    }

    switch (currValue->item.type)
    {
        /*
        case TV_INTEGER:
            (void) printf ("keyword_value = %d\n",
                    currValue->item.value.integer.number);
            break;
        */
        case TV_SYMBOL:
            if ((keyword_value =
                        malloc(strlen(currValue->item.value.string)+1)) == NULL)
                return (NULL);
            else
            {
                (void) sprintf(keyword_value, "%s",
                        currValue->item.value.string);
            }
            break;
        case TV_STRING:
            if ((keyword_value =
                        malloc(strlen(currValue->item.value.string)+1)) == NULL)
                return (NULL);
            else
            {
                (void) sprintf(keyword_value, "%s",
                        currValue->item.value.string);
            }
            break;
        case TV_DATE_TIME:
            milliseconds = (unsigned int)
                    (currValue->item.value.date_time.nanoseconds / 1000000);
            if ((keyword_value = malloc (ASF_TIME_STR_LENGTH+1)) == NULL)
                return (NULL);
            else
            {
                (void) sprintf(keyword_value, "%04u-%03uT%02u:%02u:%02u.%03u",
                    currValue->item.value.date_time.year,
                    currValue->item.value.date_time.doy,
                    currValue->item.value.date_time.hours,
                    currValue->item.value.date_time.minutes,
                    currValue->item.value.date_time.seconds,
                    milliseconds);
            }
            break;
        default:
            RemoveValue(currValue);
            RemoveParameter(currParameter);
            RemoveAggregate(currAggregate);
            RemoveAggregate(rootAggregate);
            (void) fclose(fd);
            return(NULL);
    }
    /*
    ** Free the entire ODL tree and close the file.
    */
    RemoveValue(currValue);
    RemoveParameter(currParameter);
    RemoveAggregate(currAggregate);
    RemoveAggregate(rootAggregate);
    (void) fclose(fd);

    return (keyword_value);
}


/*==============================================================================
Function:       agency_done

Description:    This function is invoked when the file transfer to the
                external agency is completed.

Parameters:

Returns:

Creator:        Norbert Piega

Creation Date:  mm/dd/yyyy

Notes:
==============================================================================*/
/* ARGSUSED1 */
static void
agency_done(PROCESS_INFO *process,  REPORT *aps_file)
{
    switch (process->exit_status)
    {
    case APS_EXIT_OK :
        (void) unlink(process->target_filename);
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "External agency file transfer completed successfully.\n\n"
            "The following file was transferred:\n"
            "%s\n", process->original_filename);
        popup_message(XmDIALOG_INFORMATION, "APS:INFORMATION", display_string,
            XtGrabNone);
        break ;
    case APS_EXIT_ERROR :
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "External agency file transfer failed!\n\n"
            "The following file was NOT transferred:\n"
            "%s\n\n"
            "The following was the intermediate file created:\n"
            "%s\n\n",
            process->original_filename,
            process->target_filename);
        popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
        break ;
    case APSGUI_EXIT_COREDUMP : /* process core dumped */
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "External agency file transfer, Signal Caught CORE DUMPED!\n\n"
            "The following file was NOT transferred:\n"
            "%s\n\n"
            "The following was the intermediate file created:\n"
            "%s\n\n",
            process->original_filename,
            process->target_filename);
        popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
        break ;
    default :   /* process caught a signal, but no core dump */
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "External agency file transfer, caught SIGNAL (signal = %d)!\n\n"
            "The following file was NOT transferred:\n"
            "%s\n\n"
            "The following was the intermediate file created:\n"
            "%s\n\n",
            -(process->exit_status),
            process->original_filename,
            process->target_filename);
        popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
        break ;
    }

    /* restore the file generation sensitivity and free the MU permission */

    /* no active ims xfer (and this fa xfer is done) */
    if ( !ims_xfer && !ims_xfer2 )  
        set_capsfile_form_sensitivity( process, TRUE ) ;

    fa_xfer = 0 ;

    return ;
}


/*==============================================================================
Function:       archive_done

Description:    This function is invoked when the IMS/DADS file archiving is
                completed.

Parameters:

Returns:        none

Creator:        Q. Sun

Creation Date:  02/02/96

Notes:
==============================================================================*/
/* ARGSUSED1 */
static void
archive_done(PROCESS_INFO *process, REPORT *aps_file)
{
    char *fname = NULL;

    switch (process->exit_status)
    {
    case APS_EXIT_OK :
        fname = malloc(strlen(process->target_filename)
                        + strlen(PMF_EXTENSION) + 2);
        if (fname != NULL)
        {
            (void) sprintf(fname, "%s.%s",
                    process->target_filename, PMF_EXTENSION);
            (void) unlink(fname);
            free(fname);
            fname = NULL;
        }

        fname = malloc(strlen(process->target_filename)
                        + strlen(DATAFILE_EXTENSION) + 2);
        if (fname != NULL)
        {
            (void) sprintf(fname, "%s.%s", process->target_filename,
                                    DATAFILE_EXTENSION);
            (void) unlink(fname);
            free(fname);
        }

        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "IMS/DADS file archiving completed successfully.\n\n"
            "The following files were archived:\n"
            "Data File name:      %s\n"
            "Meta Data file name: %s.%s\n",
            process->original_filename,
            process->original_filename, PMF_EXTENSION);
        popup_message(XmDIALOG_INFORMATION, "APS:INFORMATION", display_string,
            XtGrabNone);

        break ;
    case APS_EXIT_ERROR :
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "IMS/DADS file archiving failed!\n\n"
            "The following files were NOT archived:\n"
            "Data File name:      %s\n"
            "Meta Data file name: %s.%s\n\n"
            "The following were the intermediate files created:\n"
            "Data File name:      %s.%s\n"
            "Meta Data file name: %s.%s\n\n",
            process->original_filename,
            process->original_filename, PMF_EXTENSION,
            process->target_filename, DATAFILE_EXTENSION,
            process->target_filename, PMF_EXTENSION);

        popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);

        break ;
    case APSGUI_EXIT_COREDUMP : /* process core dumped */
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "IMS/DADS file archiving Signal Caught CORE DUMPED!\n\n"
            "The following files were NOT archived:\n"
            "Data File name:      %s\n"
            "Meta Data file name: %s.%s\n\n"
            "The following were the intermediate files created:\n"
            "Data File name:      %s.%s\n"
            "Meta Data file name: %s.%s\n\n",
            process->original_filename,
            process->original_filename, PMF_EXTENSION,
            process->target_filename, DATAFILE_EXTENSION,
            process->target_filename, PMF_EXTENSION);

        popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);

        break ;
    default :   /* process caught a signal, but no core dump */
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "IMS/DADS file archiving caught a SIGNAL (signal = %d)!\n\n"
            "The following files were NOT archived:\n"
            "Data File name:      %s\n"
            "Meta Data file name: %s.%s\n\n"
            "The following were the intermediate files created:\n"
            "Data File name:      %s.%s\n"
            "Meta Data file name: %s.%s\n\n",
            -(process->exit_status),
            process->original_filename,
            process->original_filename, PMF_EXTENSION,
            process->target_filename, DATAFILE_EXTENSION,
            process->target_filename, PMF_EXTENSION);

        popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);

        break ;
    }

    /* restore the file generation sensitivity and free the MU permission */
    if ( !fa_xfer && !ims_xfer2 )
        set_capsfile_form_sensitivity( process, TRUE ) ;
    
    ims_xfer = 0 ;

    return ;
}


/*==============================================================================
Function:       ims_xfer_done

Description:    This function is invoked when the IMS/DADS file archiving is
                completed.

Parameters:

Returns:        none

Creator:        Philip Yurchuk (inspired by Q. Sun)

Creation Date:  05/15/97

Notes:
==============================================================================*/
/* ARGSUSED1 */
static void
ims_xfer_done(PROCESS_INFO *process, REPORT *aps_file)
{
    char *fname = NULL;

    switch (process->exit_status)
    {
    case APS_EXIT_OK :
        fname = malloc(strlen(process->target_filename)
                        + strlen(PMF_EXTENSION) + 2);
        if (fname != NULL)
        {
            (void) sprintf(fname, "%s.%s",
                    process->target_filename, PMF_EXTENSION);
            (void) unlink(fname);
            free(fname);
            fname = NULL;
        }

        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "IMS/DADS file archiving completed successfully.\n\n"
            "The following file was archived:\n"
            "Data File name:      %s\n",
             process->original_filename);
        popup_message(XmDIALOG_INFORMATION, "APS:INFORMATION", display_string,
            XtGrabNone);

        break ;

    case APS_EXIT_ERROR :
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "IMS/DADS file archiving failed!\n\n"
            "The following file was NOT archived:\n"
            "Data File name:      %s\n\n"
            "The following was the intermediate file created:\n"
            "Meta Data file name: %s.%s\n\n",
            process->original_filename,
            process->target_filename, PMF_EXTENSION);

        popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);

        break ;
    case APSGUI_EXIT_COREDUMP : /* process core dumped */
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "IMS/DADS file archiving Signal Caught CORE DUMPED!\n\n"
            "The following file was NOT archived:\n"
            "Data File name:      %s\n\n"
            "The following was the intermediate file created:\n"
            "Meta Data file name: %s.%s\n\n",
            process->original_filename,
            process->target_filename, PMF_EXTENSION);

        popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);

        break ;
    default :   /* process caught a signal, but no core dump */
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "IMS/DADS file archiving caught a SIGNAL (signal = %d)!\n\n"
            "The following file was NOT archived:\n"
            "Data File name:      %s\n\n"
            "The following was the intermediate file created:\n"
            "Meta Data file name: %s.%s\n\n",
            -(process->exit_status),
            process->original_filename,
            process->target_filename, PMF_EXTENSION);

        popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);

        break ;
    }

    /* restore the file generation sensitivity and free the MU permission */
    /* no active fa or ims xfers (and this ims xfer is done) */
    if ( !fa_xfer && !ims_xfer )
        set_capsfile_form_sensitivity( process, TRUE ) ;

    ims_xfer2 = 0 ;

    return ;
}


/*==============================================================================
Function:       ask_to_send_file

Description:    Ask to confirm send file to both external agency and IMS/DADS

Parameters:     Widget (widget activating function)
                filename - filename to send
                aps_file - file description

Returns:        YES/NO

Creator:        Ron Green

Creation Date:  04/21/1995

Notes:
==============================================================================*/
static int
ask_to_send_file(widget, filename, aps_file)
    Widget widget ;
    char *filename ;
    REPORT *aps_file ;
{
    (void) sprintf(question,
        "Transfer file to both external agency and IMS/DADS?\n\n"
        "File Type: %s\n"
        "File Name: %s\n",
        aps_file->type, filename) ;

    return (AskUser(XtParent(widget), question, NO)) ;
}


/*
==============================================================================
Function:       ask_to_archive_file

Description:    Ask to confirm send file to IMS/DADS

Parameters:     Widget (widget activating function)
                filename - filename to archive
                aps_file - file description

Returns:        YES/NO

Creator:        Q. Sun

Creation Date:  02/01/1996

Notes:
==============================================================================*/
static int
ask_to_archive_file(Widget widget, char* filename, REPORT* aps_file)
{
    (void) sprintf(question,
        "Transfer file to IMS/DADS only?\n\n"
        "File Type: %s\n"
        "File Name: %s\n",
        aps_file->type, filename) ;

    return (AskUser(XtParent(widget), question, NO)) ;
}


/*==============================================================================
Function:       create_ims_filename()

Description:    Create the temporary file name to be used for IMS archiving.

Parameters:

Returns:

Creator:

Creation Date:  02/02/1996

Notes:
==============================================================================*/
static char *
create_ims_filename(REPORT *aps_file, char *pmf_filename)
{
    char *file_creation_time = NULL;
    char *tmpFileName = NULL;
    char *fullpath_filename = NULL;

    if ((file_creation_time = aps_getPMFKeyWordValue(
                                    PMF_CREATION_TIME_KEYWORD,
                                    PMF_AGGREGATE_NAME,
                                    pmf_filename)) == NULL)
    {
        (void) sprintf(display_string, "APS File Transfer:\n\n"
                "Can't retrieve FILE_CREATION_TIME from the PMF file %s\n",
                pmf_filename) ;
        popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer",
            display_string, XtGrabNone) ;
        return(NULL);
    }

    /*
    ** Create the IMS/DADS archive file name
    ** to be used in calling ims_archive().
    */
    if ((tmpFileName = malloc(  strlen(aps_file->type) +
                                strlen(file_creation_time) + 2)) == NULL)
        return(NULL);
    else
    {
        (void) sprintf(tmpFileName, "%s_%s",
                aps_file->type, file_creation_time);
    }

    fullpath_filename = aps_fullpath(APS_TEMP, tmpFileName);

    free(file_creation_time);
    free(tmpFileName);

    return(fullpath_filename);
}


/*==============================================================================
Function:       xlate_wos_to_wff_format()

Description:    Translate MWOS and AREQ files to the Walops format.

Parameters:

Returns:

Creator:

Creation Date:  02/02/1996

Notes:
==============================================================================*/
int
xlate_wos_to_wff_format(char* infile, char* outfile)
{
    char *xlateTableName = NULL;
    int tmpi ;

    xlateTableName = getenv( APS_WOS_XLATE_TBL_ENVVAR ) ;
    if (xlateTableName)
    {
        (void) gui_trimstring( xlateTableName ) ;
        tmpi = strlen( xlateTableName ) ;
    }

    if (!xlateTableName || tmpi == 0)
    {
        (void) sprintf( display_string,
                        "APS File Translation:\n\n"
                        "Environment Variable '%s' not set!",
                        APS_WOS_XLATE_TBL_ENVVAR ) ;
        popup_message( XmDIALOG_ERROR, "APS:ENVIRONMENT ERROR",
                        display_string, XtGrabNone) ;
    }

    /*
    -- Note: NULL xlateTableName error
    -- is caught in xlate() calling read_table()
    */
    return( xlate(infile, outfile, xlateTableName) );
}


/*==============================================================================
Function:       xlate_sv_to_wff_format()

Description:    Translate predicted state vector file to the Walops format.

Parameters:

Returns:

Creator:

Creation Date:  02/02/1996

Notes:
==============================================================================*/
int
xlate_sv_to_wff_format(char* infile, char* outfile)
{
    char *xlateTableName = NULL;
    int tmpi ;

    xlateTableName = getenv( APS_EPHEM_XLATE_TBL_ENVVAR ) ;
    if (xlateTableName)
    {
        (void) gui_trimstring( xlateTableName ) ;
        tmpi = strlen( xlateTableName ) ;
    }

    if (!xlateTableName || tmpi == 0)
    {
        (void) sprintf( display_string,
                        "APS File Translation:\n\n"
                        "Environment Variable '%s' not set!",
                        APS_EPHEM_XLATE_TBL_ENVVAR ) ;
        popup_message( XmDIALOG_ERROR, "APS:ENVIRONMENT ERROR",
                        display_string, XtGrabNone) ;
    }

    /*
    -- Note: NULL xlateTableName error
    -- is caught in xlate() calling read_table()
    */
    return( xlate(infile, outfile, xlateTableName) );
}


/*==============================================================================
Function:       dce_xmit_file

Description:    Transfer a file to the external agency via FAIF using
                a DCE client program.

Parameters:

Returns:        none

Creator:        Q. Sun

Creation Date:  02/05/1996

Notes:
==============================================================================*/
static void
dce_xmit_file(Widget widget, XtPointer client_data, char* cmdString)
{
    int status ;
    REPORT   *aps_file ;
    PROCESS_INFO *process = NULL;
    int destination;
    char *original_filename = NULL;
    char *pmf_filename = NULL;
    char *destination_filename = NULL;
    char *ims_archive_filename = NULL;
    char *ims_data_filename = NULL;
    char *ims_metadata_filename = NULL;

    /*
    -- get the active file information we are to transmit
    -- it is contained in the client data
    */
    aps_file = (REPORT *) client_data ;

    /*
    -- get/create the local filename for the aps file/report
    -- a function must be specified for creating the local
    -- filename the format of the function is
    --    char *mk_local_filename()
    */
    if (aps_file->mk_local_filename)
        original_filename = (*(aps_file->mk_local_filename))() ;
    else
    {
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "INTERNAL ERROR\nMissing function to get/create local filename") ;
        popup_message(XmDIALOG_ERROR, "APS:ERROR",
            display_string, XtGrabNone) ;
        set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
        return ;
    }

    /*
    -- Note the following check is set for send only (we are making
    -- sure the file exists to send).
    --
    -- if we are ever to receive, make sure we're not overwriting an
    -- existing file
    */

    if (access(original_filename, F_OK) != 0)
    {
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "File Type: %s\nFile Name: %s\n\nDoes not exist!\n",
            aps_file->type, original_filename) ;
        popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer",
            display_string, XtGrabNone) ;
        free(original_filename) ;
        set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
        return ;
    }

    /* Derive the pmf file name from the report file name */
    pmf_filename = (char *) malloc (strlen(original_filename)
                                    + strlen(PMF_EXTENSION) + 2);
    (void) sprintf(pmf_filename, "%s.%s", original_filename, PMF_EXTENSION);
    if (access(pmf_filename, F_OK) != 0)
    {
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "File Type: %s PMF\nFile Name: %s\n\nDoes not exist!\n",
            aps_file->type, pmf_filename) ;
        popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer",
            display_string, XtGrabNone) ;
        free(original_filename) ;
        free(pmf_filename) ;
        set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
        return ;
    }

    /*
    ** Prompt user for destination. If a file is sent to an external
    ** agency, it is also automatically sent to IMS/DADS.
    */
    destination = FA_AND_IMS;
    if (ask_to_send_file(widget, original_filename, aps_file) == NO)
    {
        if (ask_to_archive_file(widget, original_filename, aps_file) == NO)
        {
            free(original_filename) ;
            free(pmf_filename) ;
            set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
            return ;
        }
        else
            destination = IMS_ONLY ;
    }

    /*
    ** Create the IMS/DADS file name used to invoke the ims_archive() call.
    ** If the file name is XXXX, the ims_archive() will look for the
    ** XXXX.D (for actual data) and XXXX.M (for meta data) files.
    */
    if ((ims_archive_filename = create_ims_filename(
                                    aps_file, pmf_filename)) == NULL)
    {
        (void) sprintf(display_string, "APS File Transfer:\n\n"
                "Can't create temporary file names for IMS/DADS archiving\n");
        popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer",
            display_string, XtGrabNone) ;
        free(original_filename) ;
        free(pmf_filename) ;
        set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
        return ;
    }
    ims_metadata_filename = (char *) malloc(strlen(ims_archive_filename)
                                            + strlen(PMF_EXTENSION) + 2);
    ims_data_filename = (char *) malloc(strlen(ims_archive_filename)
                                            + strlen(DATAFILE_EXTENSION) + 2);
    (void) sprintf(ims_data_filename, "%s.%s",
            ims_archive_filename, DATAFILE_EXTENSION);
    (void) sprintf(ims_metadata_filename, "%s.%s",
            ims_archive_filename, PMF_EXTENSION);

#ifdef PRINTDIAG
    (void) printf("ims_archive_filename  = %s\n", ims_archive_filename);
    (void) printf("ims_data_filename     = %s\n", ims_data_filename);
    (void) printf("ims_metadata_filename = %s\n", ims_metadata_filename);
#endif

    /* Create the IMS/DADS destination file */
    (void) unlink(ims_data_filename);
    if (link(original_filename, ims_data_filename) == ERROR)
    {
            (void) sprintf(display_string, "APS File Transfer:\n\n"
                "Can't create a temporary file for IMS/DADS archiving!\n"
                "File name: %s\n",
                ims_data_filename);
            popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer",
                          display_string, XtGrabNone) ;
            free(original_filename) ;
            free(pmf_filename) ;
            free(ims_data_filename) ;
            free(ims_metadata_filename) ;
            free(ims_archive_filename) ;
            set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
            return;
    }

    (void) unlink(ims_metadata_filename);
    if (link(pmf_filename, ims_metadata_filename) == ERROR)
    {
            (void) sprintf(display_string, "APS File Transfer:\n\n"
                "Can't create a temporary pmf file for IMS/DADS archiving!\n"
                "File name: %s\n",
                ims_metadata_filename);
            popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer",
                display_string, XtGrabNone) ;

            (void) unlink(ims_data_filename);

            free(original_filename) ;
            free(pmf_filename) ;
            free(ims_data_filename) ;
            free(ims_metadata_filename) ;
            free(ims_archive_filename) ;
            set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
            return;
    }

    /*
    -- Create the destination filename for the external agency.
    -- Use mk_remote_filename function in the aps_file
    -- definition to get the filename
    -- else if no function exists for creating a
    -- destination filename use the original APS report filename
    --
    */
    if (destination == FA_AND_IMS)
    {
        if (aps_file->mk_remote_filename)
        {
            destination_filename =
                        (*(aps_file->mk_remote_filename))(pmf_filename) ;
        }
        else
            destination_filename = aps_fullpath(APS_TEMP,
                                        (char *)basename(original_filename));

        if (destination_filename == NULL)
        {
            (void) sprintf(display_string, "APS File Transfer:\n\n"
                "Can't create remote file name\n");
            popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer",
                            display_string, XtGrabNone) ;

            (void) unlink(ims_data_filename);
            (void) unlink(ims_metadata_filename);

            free(original_filename) ;
            free(pmf_filename) ;
            free(ims_data_filename) ;
            free(ims_metadata_filename) ;
            free(ims_archive_filename) ;
            set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
            return;
        }

        /* now translate the file if necessary */
        if (aps_file->xlate_func)
        {
            status = (*(aps_file->xlate_func))( original_filename,
                                                destination_filename);

            /*
            ** The translation function returns zero on success.
            */
            if (status != OK)
            {
                (void) sprintf(display_string,
                        "APS File Transfer:\n\n"
                        "Unable to translate file:\n%s\n\n", original_filename);

                switch (status)
                {
                    case E_SAMEFILE:
                        (void) sprintf(display_string+strlen(display_string),
                                "Input and Output files must be different!\n");
                        break;
                    case E_INFILE:
                        (void) sprintf(display_string+strlen(display_string),
                                "Can't open the input file:\n%s\n",
                                original_filename);
                        break;
                    case E_OUTFILE:
                        (void) sprintf(display_string+strlen(display_string),
                                "Can't open the output file:\n%s\n",
                                destination_filename);
                        break;
                    case E_TBLFILE:
                        (void) sprintf(display_string+strlen(display_string),
                                "Can't open the translation table!\n");
                        break;
                    case E_TBLFMT:
                        (void) sprintf(display_string+strlen(display_string),
                                "No entry found in the translation table!\n");
                        break;
                    default:
                        (void) sprintf(display_string+strlen(display_string),
                                "Unknown file translation return code: %d\n",
                                status);
                        break;
                }

                popup_message(XmDIALOG_ERROR, "APS:ERROR",
                                display_string, XtGrabNone) ;

                (void) unlink(ims_data_filename);
                (void) unlink(ims_metadata_filename);
                (void) unlink(destination_filename);

                free(original_filename) ;
                free(pmf_filename) ;
                free(destination_filename) ;
                free(ims_data_filename) ;
                free(ims_metadata_filename) ;
                free(ims_archive_filename) ;

                set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);

                return ;
            }
        }
        else
        {
            /*
            ** Make a copy of the report file to be transfered to the
            ** external agency.
            */
            (void) unlink(destination_filename);
            if (link(original_filename, destination_filename) == ERROR)
            {
                    (void) sprintf(display_string, "APS File Transfer:\n\n"
                        "Can't make a destination file to transfer to external agency\n%s\n",
                        destination_filename);
                    popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer",
                        display_string, XtGrabNone) ;

                    (void) unlink(ims_data_filename);
                    (void) unlink(ims_metadata_filename);

                    free(original_filename) ;
                    free(pmf_filename) ;
                    free(destination_filename) ;
                    free(ims_data_filename) ;
                    free(ims_metadata_filename) ;
                    free(ims_archive_filename) ;

                    set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);

                    return ;
            }
        }

        /* transmit the file to the external agency */
        (void) sprintf(display_string, "%s -f %s",
                cmdString,
                destination_filename);

        process = (PROCESS_INFO *) create_process(display_string, &status, TRUE,
            NULL, gui_display_message_widget, scrolledText_create_report,
            agency_done, aps_file) ;
        if (process != NULL)
        {
            (void) sprintf(process->target_filename, "%s",
                    destination_filename);
            (void) sprintf(process->original_filename, "%s", original_filename);
            if (start_process(process) > OK)
                destroy_process(process);
            else
                fa_xfer = 1 ;
        }
    } /* if (destination == FA_AND_IMS) */

    process = NULL;
    /* transmit the file to the IMS/DADS */
    (void) sprintf(display_string, "%s -f %s -d \"%s\" -U %s -P %s",
                            IMS_ARCHIVE_CMD,
                            ims_archive_filename,
                            aps_file->metacode,
                            userid, password);

    process = (PROCESS_INFO *) create_process(display_string, &status, TRUE,
            NULL, gui_display_message_widget, scrolledText_create_report,
            archive_done, aps_file) ;
    if (process != NULL)
    {
        (void) sprintf(process->target_filename, "%s", ims_archive_filename);
        (void) sprintf(process->original_filename, "%s", original_filename);
        if (start_process(process) > OK)
            destroy_process(process);
        else
            ims_xfer = 1 ;
    }

    /*
    ** free the created filenames
    */
    free(original_filename) ;
    free(pmf_filename) ;
    free(destination_filename) ;
    free(ims_data_filename) ;
    free(ims_metadata_filename) ;
    free(ims_archive_filename) ;

    if (!ims_xfer && !fa_xfer)
        set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);

    return ;
}


/*==============================================================================
Function:       cb_ims_filexfer

Description:    Transfer a file to IMS using a DCE client program.

Parameters:

Returns:        none

Creator:        Philip Yurchuk (inspired by Q. Sun)

Creation Date:  4/11/97

Notes:
==============================================================================*/
void
cb_ims_filexfer(Widget widget, XtPointer client_data, XtPointer *cbs)
{
    int status ;
    REPORT   *aps_file ;
    PROCESS_INFO *process = NULL;
    char *original_filename = NULL;
    char *destination_filename = NULL;
    char *ims_archive_filename = NULL;
    char *ims_data_filename = NULL;
    char *ims_metadata_filename = NULL;
    
    /*
    -- get the active file information we are to transmit
    -- it is contained in the client data
    */
    aps_file = (REPORT *) client_data ;

    /*
    -- get/create the local filename for the aps file/report
    -- a function must be specified for creating the local
    -- filename the format of the the function is
    --    char *mk_local_filename()
    */
    
    /* 
    -- Check to see if the filename was passed in cbs.  This would happen if
    -- this file is being automatically generated as a companion file for 
    -- an AWOS or MWOS file.
    */
    if (DMAP_filename)
    {
        original_filename = (char *) DMAP_filename ;
        DMAP_filename = NULL ;
    }
    else if (aps_file->mk_local_filename)
        original_filename = (*(aps_file->mk_local_filename))() ;
    else
    {
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "INTERNAL ERROR\nMissing function to get/create local filename") ;
        popup_message(XmDIALOG_ERROR, "APS:ERROR",
            display_string, XtGrabNone) ;
        set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
        return ;
    }

    /*
    -- Note the following check is set for send only (we are making
    -- sure the file exists to send).
    --
    -- if we are ever to receive, make sure we're not overwriting an
    -- existing file
    */

    if (access(original_filename, F_OK) != 0)
    {
        (void) sprintf(display_string,
            "APS File Transfer:\n\n"
            "File Type: %s\nFile Name: %s\n\nDoes not exist!\n",
            aps_file->type, original_filename) ;
        popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer",
            display_string, XtGrabNone) ;
        free(original_filename) ;
        set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
        return ;
    }

    /*
    ** Create the IMS/DADS file name .
    */
    if ((ims_archive_filename = create_ims_filename(
                                    aps_file, original_filename)) == NULL)
    {
        (void) sprintf(display_string, "APS File Transfer:\n\n"
                "Can't create temporary file names for IMS/DADS archiving\n");
        popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer",
            display_string, XtGrabNone) ;
        free(original_filename) ;
        set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
        return ;
    }
    ims_metadata_filename = (char *) malloc(strlen(ims_archive_filename)
                                            + strlen(PMF_EXTENSION) + 2);

    (void) sprintf(ims_metadata_filename, "%s.%s",
            ims_archive_filename, PMF_EXTENSION);

#ifdef PRINTDIAG
    (void) printf("ims_archive_filename  = %s\n", ims_archive_filename);
    (void) printf("ims_data_filename     = %s\n", ims_data_filename);
    (void) printf("ims_metadata_filename = %s\n", ims_metadata_filename);
#endif

    /* Create the IMS/DADS destination file */
    (void) unlink(ims_metadata_filename);
    if (link(original_filename, ims_metadata_filename) == ERROR)
    {
            (void) sprintf(display_string, "APS File Transfer:\n\n"
                "Can't create a temporary pmf file for IMS/DADS archiving!\n"
                "File name: %s\n",
                ims_metadata_filename);
            popup_message(XmDIALOG_ERROR, "APS:ERROR File Transfer",
                display_string, XtGrabNone) ;

            (void) unlink(ims_data_filename);

            free(original_filename) ;
            free(ims_data_filename) ;
            free(ims_metadata_filename) ;
            free(ims_archive_filename) ;
            set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
            return;
    }

    process = NULL;

    /* transmit the file to the IMS/DADS */

    /* Let's get permission if we don't have it already */
    if (!currPermId)
        if (init_create_xfer() <= 0) 
        {
            /* if there was an error... */

            free(original_filename) ;
            free(ims_data_filename) ;
            free(ims_metadata_filename) ;
            free(ims_archive_filename) ;
            set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);
            return;
        }

    (void) sprintf(display_string, "%s -f %s -d \"%s\" -U %s -P %s",
                            IMS_ARCHIVE_CMD,
                            ims_archive_filename,
                            aps_file->metacode,
                            userid, password);

    process = (PROCESS_INFO *) create_process(display_string, &status, TRUE,
            NULL, gui_display_message_widget, scrolledText_create_report,
            ims_xfer_done, aps_file) ;
    if (process != NULL)
    {
        (void) sprintf(process->target_filename, "%s", ims_archive_filename);
        (void) sprintf(process->original_filename, "%s", original_filename);
        if (start_process(process) > OK)
            destroy_process(process);
        else
            ims_xfer2 = 1 ;
    }

    /*
    ** free the created filenames
    */
    free(original_filename) ;
    free(destination_filename) ;
    free(ims_data_filename) ;
    free(ims_metadata_filename) ;
    free(ims_archive_filename) ;

    if (!ims_xfer && !fa_xfer && !ims_xfer2)
        set_capsfile_form_sensitivity((PROCESS_INFO *) NULL, TRUE);

    return ;
}


/*==============================================================================
Function:       cb_faif_filexfer

Description:    Transfer a file to the external agency via FAIF using
                a DCE client program.

Parameters:     Standard X Parameters

Returns:        none

Creator:        Q. Sun

Creation Date:  02/05/1996

Notes:
==============================================================================*/
/* ARGSUSED2 */
void
cb_faif_filexfer(   Widget widget,
                        XtPointer client_data,
                        XtPointer call_data)
{
    char cmdString[CMD_STRING_LEN];

    REPORT *aps_file = (REPORT *) client_data;

    int i ;

    /* get the perm id and set the sensitivity on the window */
    if (init_create_xfer() > 0)
    {
        (void) sprintf(cmdString, "%s -d %s -t %s",
                            FAIF_XMITCLIENT_CMD,
                            aps_file->fa_destination,
                            aps_file->fa_filetype) ;
        dce_xmit_file(widget, client_data, cmdString);
    }

    /* Now transfer the companion file if need be */
    if (strcmp(aps_file->type, "AWOS") == 0)
    {
        /* Get the correct report to pass */
        i = 0 ;   
        while (strcmp(reports[i].type, "ADDM") != 0)
            i++ ;

        /* Give it the correct filename so it doesn't look to the GUI for it */
        DMAP_filename = aps_fullpath(reports[i].aps_fullpath_id, 
                                     reports[i].default_file_name) ;

        cb_ims_filexfer(widget, &reports[i], call_data) ;
    }
    else if (strcmp(aps_file->type, "MWOS") == 0)
    {
        /* Get the correct report to pass */
        i = 0 ;   
        while (strcmp(reports[i].type, "MDDM") != 0)
            i++ ;

        /* Give it the correct filename so it doesn't look to the GUI for it */
        DMAP_filename = aps_fullpath(reports[i].aps_fullpath_id, 
                                     reports[i].default_file_name) ;

        cb_ims_filexfer(widget, &reports[i], call_data) ;
    }
	else if ((strcmp(aps_file->type, AE1E_TYPE) == 0) ||
			 (strcmp(aps_file->type, AE2E_TYPE) == 0) ||
			 (strcmp(aps_file->type, AJ1E_TYPE) == 0) ||
			 (strcmp(aps_file->type, AA1E_TYPE) == 0) ||
			 (strcmp(aps_file->type, AR1E_TYPE) == 0))
	  set_capsfile_form_sensitivity(NULL, TRUE) ;


    return ;
}


/*==============================================================================
Function:       cb_hc_filexfer

Description:    Transfer a file to the Host Controller (HC) using
                a DCE client program.

Parameters:     Standard X Parameters

Returns:        none

Creator:        Q. Sun

Creation Date:  02/05/1996

Notes:
==============================================================================*/
/* ARGSUSED2 */
void
cb_hc_filexfer(Widget widget, XtPointer client_data, XtPointer call_data)
{
    char cmdString[CMD_STRING_LEN];

    REPORT *aps_file = (REPORT *) client_data;

    int i ;

    /* get the perm id and set the sensitivity on the window */
    if (init_create_xfer() > 0)
    {
        (void) sprintf(cmdString, "%s -t %s",
                            HC_XMITCLIENT_CMD,
                            aps_file->type) ;
        dce_xmit_file(widget, client_data, cmdString);
    }
    else
        return ;

    /* Now transfer the companion file if need be */
    if (strcmp(aps_file->type, AWOS_TYPE) == 0)
    {
        /* Get the correct report to pass */
        i = 0 ;   
        while (strcmp(reports[i].type, ADDM_TYPE) != 0)
            i++ ;

        /* Give it the correct filename so it doesn't look to the GUI for it */
        DMAP_filename = aps_fullpath(reports[i].aps_fullpath_id, 
                                     reports[i].default_file_name) ;

        cb_ims_filexfer(widget, &reports[i], call_data) ;
    }
    else if (strcmp(aps_file->type, "MWOS") == 0)
    {
        /* Get the correct report to pass */
        i = 0 ;   
        while (strcmp(reports[i].type, "MDDM") != 0)
            i++ ;

        /* Give it the correct filename so it doesn't look to the GUI for it */
        DMAP_filename = aps_fullpath(reports[i].aps_fullpath_id, 
                                     reports[i].default_file_name) ;

        cb_ims_filexfer(widget, &reports[i], call_data) ;
    }
	else if ((strcmp(aps_file->type, AE1E_TYPE) == 0) ||
			 (strcmp(aps_file->type, AE2E_TYPE) == 0) ||
			 (strcmp(aps_file->type, AJ1E_TYPE) == 0) ||
			 (strcmp(aps_file->type, AA1E_TYPE) == 0) ||
			 (strcmp(aps_file->type, AR1E_TYPE) == 0))
	  set_capsfile_form_sensitivity(NULL, TRUE) ;


    return ;
}


/*==============================================================================
Function:       cb_quit_filegen

Description:    Quit the file generation GUI.

Parameters:     Standard X Parameters

Returns:        none

Creator:        P. Yurchuk

Creation Date:  02/17/98

Notes:
==============================================================================*/
/* ARGSUSED2 */
void
cb_quit_filegen(Widget widget, XtPointer client_data, XtPointer call_data)
{

    extern Widget apsfilegen_form ;
 
BEGIN_CONTEXT ( widget )
 
    if (currPermId != 0)
    {
    	/* 
		-- If there are outstanding permissions, query the user to see if he or 
		-- she wishes to continue.
		*/
 
      	(void) sprintf( question, "Pressing the QUIT button merely dismisses \nthe window, and does not release permissions, \nwhich still exist.  \nDo you wish to continue?" ) ;
	
		if (AskUser(widget, question, NO) == YES)
		{
		  	XtPopdown(XtParent(apsfilegen_form)) ;
		}
    }
    else
      	XtPopdown(XtParent(apsfilegen_form)) ;
	
	first_time_in_routine = 1 ;
	first_time_REQQ = 1 ;

END_CONTEXT

}

