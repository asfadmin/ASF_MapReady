#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   cb_darmanager.c

Description:    

External Functions Defined:
            void cb_show_dar_records
            void cb_update_darmanager_form
            void cb_set_darmanager_editability
            void cb_save_dar_changes
            void cb_delete_dar_record
            void cb_load_dars_from_ims
            void cb_print_dars
            void cb_set_print_dars_to_file_cb
    
File Scope Functions:
            int   check_for_selected_dar
            void  display_ims_error
            void  dar_report_done
            int   dar_odl2asf_time
            llist *get_darList_from_ims
    
External Variables Defined:
    
File Scope Variables:
    
Notes:

==============================================================================*/
#pragma ident   "@(#)cb_darmanager.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_darmanager.c"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>
#include <malloc.h>
#include <string.h>
#include <sys/stat.h>

#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/FileSB.h>

#include "dapps_defs.h"

#define CONTEXT_MACRO_ACCESS 1
#include "vc_darmanager.h"
#undef CONTEXT_MACRO_ACCESS
#include "UxXt.h"

#include <ims_query.h>          /* needed for ims_cmnQuery.h */
#include <ims_cmnQuery.h>   /* needed for IMS_DAR_LIST struct */

#include "fa_defs.h"
#include "DARconversions.h"
#include "ODLconversions.h"

#include "nmalloc.h"
#include "db_sybint.h"
#include "db_dar.h"
#include "aps_defs.h"
#include "aps_extern.h"
#include "aps_db_table.h"
#include "aps_imsDB.h"

#include "gui_defs.h"
#include "gui_utils.h"
#include "gui_mu_utils.h"
#include "satmenus.h"
#include "cb_darmanager.h"
#include "cb_datetime.h"
#include "subprocess.h"
#include "apspath.h"
#include "aps_encrypt.h"
#include "apsfiledef.h"
#include "timeconv.h"
#include "mu_utilities.h"

#define BLANK_STR       " "
#define DEF_R1_SENSOR   "SAR"

extern void     popup_message();

extern char     display_string[] ;
extern Widget   UxToplevel ;
extern Widget   DAR_manager ;
extern Widget   filebox ;

static llist    *dars ;
static char     blank[] = " " ;
static int      darmgr_perm_id = 0 ; /* permission id for MU support */


#define BEGIN_CONTEXT(widget) \
    _UxCDARManager          *UxSaveCtx; \
    UxSaveCtx = UxDARManagerContext; \
    UxDARManagerContext = (_UxCDARManager *) UxGetContext( widget ) ; \
    {

#define END_CONTEXT \
    } \
    UxDARManagerContext = UxSaveCtx; 


/*==============================================================================
Function:               cb_quit_darmanager
 
Description:    
        This function pops down the DAR Manager interface.  If permissions have
        not been released, the user is queried as to whether or not he/she
        wants to continue.
 
Parameters:             Standard X Callback parameters
 
Returns:                None
 
Creator:                Philip Yurchuk
 
Creation Date:  3/2/97
 
Notes:          
==============================================================================*/
/* ARGSUSED1 */
void 
cb_quit_darmanager(Widget widget, XtPointer client_data, XtPointer cbs)
{

    int retcode;

BEGIN_CONTEXT ( widget )
 
    if (darmgr_perm_id > 0)
    {
        retcode = mu_permission_validate(darmgr_perm_id,
                                         MU_EDIT_OR_DELETE_DAR,
                                         MU_DAR_ACTIVITY_TYPE);

        /* 
        -- If the permission was valid query the user to see if he or she wishes
        -- to continue.
        */

        if (retcode > 0)
        {
            (void) sprintf( question, "Pressing the QUIT button merely dismisses \nthe window, and does not release permissions, \nwhich still exist.  \nDo you wish to continue?" ) ;

            if (AskUser(widget, question, NO) == YES)
            {
                XtPopdown(XtParent(DAR_manager)) ;
            }
        }
    }
    else
        XtPopdown(XtParent(DAR_manager)) ;

END_CONTEXT
 
}


/*==============================================================================
Function:       check_for_selected_dar

Description:    Sees if a DAR is selected, if not pops up an error
                message.

Parameters:     listWidget  - DAR list widget

Returns:        position of the currently selected item
                0 if no item is selected (or there is no item to select)

Creator:        Teresa McKillop

Creation Date:  02/13/96

Notes:          
==============================================================================*/
static int
check_for_selected_dar( Widget listWidget )
{
    int     index;

    if (!(index = gui_XmList_selected_item( listWidget )))
    {
        (void) sprintf( display_string, "No DAR is Selected" ) ;
        popup_message(XmDIALOG_ERROR, "APS:ERROR", 
            display_string, XtGrabNone) ;
    }

    return (index);
}



/*==============================================================================
Function:       display_ims_error

Description:    Pops up an error message made up of the IMS error
                message portion of the IMS_MSG_QUEUE argument,
                preceded by msgStr, if it is not NULL.

Parameters:     msgStr      - a string to prepend to the msgStruct msg's
                msgStruct   - message structure set during an IMS call

Returns:        N/A

Creator:        Teresa McKillop

Creation Date:  01/30/96

Notes:          
==============================================================================*/
static void
display_ims_error( char *msgStr, IMS_MSG_STRUCT *msgStruct )
{
    IMS_MSG_QUEUE   *msgQueue;
    int             msgLength;
    char            buf[APS_GUI_DISPLAY_STR_SIZE];

    if (msgStruct != NULL)
    {
        /* Extract and display the queued messages. */
        if ((msgLength = (msgStr == NULL) ? 0 : strlen( msgStr )) > 0)
            (void) strcpy( buf, msgStr );
        while ((msgQueue = ims_msgQueueExtract( msgStruct )) != NULL)
        {
            if ((msgLength += (int) strlen( msgQueue->msg ))
                >=  APS_GUI_DISPLAY_STR_SIZE)
            {
                break;
            }
            (void) strcat( buf, msgQueue->msg );
            ims_msgQueueFree (msgQueue);
        }
    }
    popup_message( XmDIALOG_ERROR, "APS:ERROR", buf, XtGrabNone);

    return;
}


/*==============================================================================
Function:       free_imsDarList

Description:    The first item in the retPtr from an IMS API call is
                NOT malloc'ed by them, but if there is more than one
                record returned from the query, IMS malloc's storage
                for these.  This routine frees all the memory that
                IMS malloc'ed and sets the 1st item's "next" ptr to NULL.

Parameters:     

Returns:        

Creator:        Teresa McKillop

Creation Date:  01/30/96

Notes:      
==============================================================================*/
static void
free_imsDarList( IMS_DAR_LIST *imsDarList )
{
    IMS_DAR_LIST    *currImsDar;
    IMS_DAR_LIST    *nextImsDar;

    if (imsDarList == NULL || imsDarList->next == NULL)
        return;

    /* free all elements, except the 1st (it wasn't allocated) */
    for (currImsDar = imsDarList->next ; currImsDar != NULL
        ; currImsDar = nextImsDar)
    {
        nextImsDar = currImsDar->next ;
        free( currImsDar );
    }

    imsDarList->next = NULL;

    return;
}



/*==============================================================================
Function:       dar_odl2asf_time

Description:    converts an odl time to asf time and inserts the asf
                time in a dar record field (ie, memcpy()'s it without
                the string terminator.

Parameters:     odlTime - odl time string, with a string terminator.

                asfTime - equivalent asf time w/out the string terminator

Returns:        TRUE    - if the time was converted without errors
                FALSE   - if input string is not correct size (nothing is done)

Creator:        Teresa McKillop

Creation Date:  01/15/96

Notes:      
==============================================================================*/
static int
dar_odl2asf_time( char *odlTime, char *asfTime )
{
    char    tmptime[ASF_TIME_STR_LENGTH+1] ;
    int     retStatus = TRUE;

    if (strlen( odlTime ) == ASF_TIME_STR_LENGTH)
    {
        (void) tc_odl2asf( odlTime, tmptime );
        (void) memcpy( asfTime, tmptime, strlen( tmptime ) );
    }
    else
        retStatus = FALSE;

    return (retStatus);
}



/*==============================================================================
Function:       get_darList_from_ims

Description:    Queries IMS for any new DARs, makes each of these into
                an APS record in a DAR list.

Parameters:     

Returns:        allocated darlist or NULL if any errors occur

Creator:        Teresa McKillop

Creation Date:  01/17/96

Notes:          if there are no new IMS DARs, darlist will be a list with zero
                elements
==============================================================================*/
static llist *
get_darList_from_ims()
{
    llist           *darlist = NULL;
    DB_RECORD       **dar_rec ;

    IMS_DAR_LIST    imsDarList ;
    IMS_DAR_LIST    *imsDarPtr;

    IMS_MSG_STRUCT  *imsMsgStruct = NULL;   /* struct for IMS error messages */

    char            *tmpSensor = NULL;
    float           tmpNWLat;
    float           tmpNWLon;

    char            *tmpPtr = NULL;
    int             ret_value;
    int             i;

    /*
    -- get the list of new dars from IMS
    */

    if ((imsMsgStruct = alloc_imsMsgStruct()) == NULL)
        return (NULL);  /* message was already popped up */

    if ((ret_value = aps_darQuery( APS_QUERY_STAT_TYPE, NULL, NULL,
        &imsDarList, imsMsgStruct )) < IMS_OK)
    { 
        if (ret_value == IMS_NOROWS)    /* no rows match query; inform user */
        {
            (void) sprintf( display_string,
                "No %s DARs were found in the IMS/DADS DB",
                APS_QUERY_STAT_TYPE );
            popup_message( XmDIALOG_INFORMATION, "APS:INFORMATION",
                display_string, XtGrabNone);
        }
        else
        {
            /* display error */
            (void) sprintf( display_string,
                "ERROR can't get New DARs from IMS/DADS\n" );
            display_ims_error( display_string, imsMsgStruct );
        }

        free_imsMsgStruct( &imsMsgStruct );
        return (NULL);
    }

    free_imsMsgStruct( &imsMsgStruct );

    /*
    -- change each new DAR to an APS db record and make these into a list
    */

    darlist = create_dyn_llist();

    for (imsDarPtr = &imsDarList ; imsDarPtr != NULL ;
        imsDarPtr = imsDarPtr->next)
    {
        /*
        -- create a new aps dar record from the current ims dar
        -- [NOTE that these fields are not set: prvdarid, prvreqsat, rev]
        */

        dar_rec = new_table_record( APS_CDEFS( DAR ) );

        /* dar id */
        CAST_DAR_DARID dar_rec[DAR_DARID] = imsDarPtr->order_id;

        /* user id */
        (void) strcpy( CAST_DAR_USERID dar_rec[DAR_USERID],
            imsDarPtr->user_id );

        /* request time */
        (void) dar_odl2asf_time( imsDarPtr->receive_time,
            CAST_DAR_REQTIME dar_rec[DAR_REQTIME] );

        /* request status */
        if ((tmpPtr = table_lookupIMS2APS( dar_status, imsDarPtr->status,
            NULL )) == NULL)
        {
            tmpPtr = dar_status[0].aps_value;   /* no match, use default */
        }
        (void) strcpy( CAST_DAR_REQSTAT dar_rec[DAR_REQSTAT], tmpPtr );

        /* satellite/sensor */
        tmpPtr = get_sat_code_from_ims_name( imsDarPtr->platform );
        if (tmpPtr != NULL)     /* found a sat code */
        {
            (void) strcpy( CAST_DAR_SAT dar_rec[DAR_SAT], tmpPtr );
        }
        if (*(tmpPtr) == 'R')   /* RADARSAT is special case for sensor */
        {
            if ((tmpSensor = fa_table_lookup2APS( ODL_dmap_mode,
                imsDarPtr->mode, NULL )) == NULL)
            {
                tmpSensor = DEF_R1_SENSOR;
            }
        }
        else
        {
            tmpSensor = imsDarPtr->sensor;  /* ims: SAR -> aps: SAR */
        }
        (void) strcpy( CAST_DAR_SENSOR dar_rec[DAR_SENSOR], tmpSensor );

        /* strttime and endtime */
        (void) dar_odl2asf_time( imsDarPtr->start_time,
            CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME] );
        (void) dar_odl2asf_time( imsDarPtr->end_time,
            CAST_DAR_ENDTIME dar_rec[DAR_ENDTIME] );

        /* sitename */
        (void) strcpy( CAST_DAR_SITENAME dar_rec[DAR_SITENAME],
            imsDarPtr->site_name );

        /* shape (site shape) */
        CAST_DAR_SHAPE dar_rec[DAR_SHAPE] = imsDarPtr->site_shape;

        /* radius */
        CAST_DAR_RADIUS dar_rec[DAR_RADIUS] = imsDarPtr->radius;

        /* nwlat/nwlon: special case if shape is point */
        if (CAST_DAR_SHAPE dar_rec[DAR_SHAPE] == POINT_CHAR)
        {
            tmpNWLat = imsDarPtr->center_lat;
            tmpNWLon = imsDarPtr->center_lon;
        }
        else
        {
            tmpNWLat = imsDarPtr->north_west_lat;
            tmpNWLon = imsDarPtr->north_west_lon;
        }
        CAST_DAR_NWLAT dar_rec[DAR_NWLAT] = tmpNWLat;
        CAST_DAR_NWLON dar_rec[DAR_NWLON] = tmpNWLon;
        
        /* nelat/nelon, selat/selon, swlat/swlon */
        CAST_DAR_NELAT dar_rec[DAR_NELAT] = imsDarPtr->north_east_lat;
        CAST_DAR_NELON dar_rec[DAR_NELON] = imsDarPtr->north_east_lon;
        CAST_DAR_SELAT dar_rec[DAR_SELAT] = imsDarPtr->south_east_lat;
        CAST_DAR_SELON dar_rec[DAR_SELON] = imsDarPtr->south_east_lon;
        CAST_DAR_SWLAT dar_rec[DAR_SWLAT] = imsDarPtr->south_west_lat;
        CAST_DAR_SWLON dar_rec[DAR_SWLON] = imsDarPtr->south_west_lon;

        /* number of observations */
        CAST_DAR_NOBS dar_rec[DAR_NOBS] = imsDarPtr->observation_num;

        /* frequency of observation */
        (void) strcpy( CAST_DAR_FOBS dar_rec[DAR_FOBS],
            imsDarPtr->observation_freq );

        /* ascending/descending flag */
        CAST_DAR_ASCDSC dar_rec[DAR_ASCDSC] = imsDarPtr->asc_desc;

        /* [science] user comments
        --  r1b' KLUGE: have to prepend certain new fields to the comments
        */
        (void) sprintf( display_string,
            "discipline=%s, priority=%s, active=%c, start_date=%1.8s, end_date=%1.8s; ",
            imsDarPtr->pi_discipline, imsDarPtr->priority, imsDarPtr->active_p,
            imsDarPtr->activity_start_date, imsDarPtr->activity_end_date );
        tmpPtr = display_string + strlen( display_string );
        i = (APS_SIZE( DAR, DAR_USERCMNT ) - 1) - (tmpPtr - display_string);
        (void) strncpy( tmpPtr, imsDarPtr->user_comment, i );
        *(tmpPtr + i) = STREND;
        (void)strcpy( CAST_DAR_USERCMNT dar_rec[DAR_USERCMNT], display_string );

        /* planner comments */
        (void) strcpy( CAST_DAR_PLNRCMNT dar_rec[DAR_PLNRCMNT],
            imsDarPtr->planner_comment );

        /* quicklook (science) */
        CAST_DAR_QUICKLOOK dar_rec[DAR_QUICKLOOK] = imsDarPtr->quicklook_p;

        /*
        -- put the record in the list
        */

        APPEND( darlist, dar_rec, free_db_record, dar_rec );
    }

    free_imsDarList( &imsDarList );

    return (darlist);
}



/*==============================================================================
Function:       cb_show_dar_records

Description:    
    This function retrieves DAR records from the database and displays
    them on the screen

Parameters:     Standard X Callback parameters

Returns:        None

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:      
==============================================================================*/
/* ARGSUSED2 */
void
cb_show_dar_records(Widget widget, XtPointer client_data, XtPointer cbs)
{
    char            *sat_string ;
    char            *order_columns ;
    char            *search_str ;
    char            format_string[255] = { NULL } ;
    cursor          ptr ;
    DB_RECORD       **dar_rec ;
    int             numDars;
 
    XmStringTable   str_list ;

    static char     blank[] = " " ;
    int             i ;

    Widget list_widget = (Widget) client_data ;

BEGIN_CONTEXT( list_widget ) 

        /*
        -- get the available dars from the db table
        -- and allocate a String Table to hold them
        */
 
        if (dars)
        {
            DEL_LIST(dars) ;
        }
 
        order_columns = XmTextFieldGetString(TF_DAR_sortclause) ;
        search_str = XmTextFieldGetString(TF_DAR_searchclause) ;

        (void) sprintf( orderby_cols, "%s", order_columns ) ;
        (void) sprintf( where_clause, "%s", search_str ) ;

        XtFree(order_columns) ;
        XtFree(search_str) ;

        if (strcmp(where_clause, "where") == 0)
            where_clause[0] = NULL ;

        TimeoutCursors(True, True, NULL) ;
        dars = db_get_records(APS_dbproc, APS_TABLE(DAR), 
            where_clause, orderby_cols, APS_CDEFS(DAR), ALL_COLS) ;

        if (!dars)
        {
            (void) sprintf( display_string,
                "Error with the APS DB\nCan't get DARs\nNeed to fix the DB" );
            popup_message(XmDIALOG_ERROR, "APS:DB ERROR",
                display_string, XtGrabNone) ;

            numDars = 0;
            /* ensure that no items are in the list */
            XtVaSetValues(list_widget,
                XmNitemCount, numDars,
                NULL) ;
        }
        else
        {
            numDars = NUMELTS( dars ) ;
            str_list = 
                (XmStringTable) XtMalloc( numDars * sizeof(XmString) ) ;

            /* create the format string for the display window */ 
            if (*format_string == NULL)
            {
                char    *tmpstr;

                (void) sprintf( format_string, "%s",
                    tmpstr = aps_set_fw_pfmt( DAR, DAR_DARID, 7, FALSE ) );
                    free( tmpstr );
                (void) sprintf( format_string, "%s %s", format_string,
                    tmpstr = aps_max_pfmt( DAR, DAR_REQSTAT, TRUE ) );
                    free( tmpstr );
                (void) sprintf( format_string, "%s %s/%s",  /*SAT/SENSOR*/
                    format_string, "%8s",
                    tmpstr = aps_max_pfmt( DAR, DAR_SENSOR, TRUE ) );
                    free( tmpstr );
                (void) sprintf( format_string, "%s%*s%s%*s%s%*s%s",
                    format_string,
                    1, blank,
                    "%17.17s", /* APS_PFMT(DAR, DAR_STRTTIME), */
                    1, blank,
                    "%17.17s", /* APS_PFMT(DAR, DAR_ENDTIME), */
                    1, blank,
                    "%s" /* DAR_REQTIME */ ) ; 
            }

            for ( i = 0, dar_rec = (DB_RECORD **) FIRST(dars, ptr)
                ; dar_rec
                ; dar_rec = (DB_RECORD **) NEXT(dars, ptr))
            {
                if ((sat_string = get_satellite_name(
                    CAST_DAR_SAT dar_rec[DAR_SAT] )) == NULL)
                {
                    /*
                    -- sat code is not in internal satellite table; remove
                    -- dar from list: later processing is based on sat name
                    */

                    /* popup error message */
                    (void) sprintf( display_string,
                        "WARNING: Satellite \"%s\" is not\n    in internal sat_name table\n    SKIPPING this DAR",
                        CAST_DAR_SAT dar_rec[DAR_SAT] ) ;
                    gui_aps_internal_error( XmDIALOG_ERROR,
                        __FILE__, __LINE__, display_string ) ;

                    /* remove from the dars list */
                    (void) DEL_AT_CURSOR( dars, ptr );
                    numDars-- ;
                    /* skip this one and get the next */
                    continue;
                }
     
                (void) sprintf( display_string, format_string,
                    CAST_DAR_DARID dar_rec[DAR_DARID],
                    CAST_DAR_REQSTAT dar_rec[DAR_REQSTAT],
                    sat_string, CAST_DAR_SENSOR dar_rec[DAR_SENSOR],
                    CAST_DAR_STRTTIME dar_rec[DAR_STRTTIME],
                    CAST_DAR_ENDTIME dar_rec[DAR_ENDTIME],
                    CAST_DAR_SITENAME dar_rec[DAR_SITENAME] ) ;

                str_list[i] = XmStringCreateLocalized(display_string) ;
                i++ ;
            }

            XtVaSetValues(list_widget,
                XmNitems, str_list,
                XmNitemCount, numDars,
                NULL) ;
 
            for (i = 0; i < numDars ;i++)
                XmStringFree(str_list[i]) ;
            XtFree((char *) str_list) ;
        }

        (void) sprintf( display_string, "%5d", numDars ) ;
        XmTextFieldSetString(TF_DAR_recordcount, display_string) ;

        TimeoutCursors(False, False, NULL) ;
END_CONTEXT
}




/*==============================================================================
Function:       cb_update_darmanager_form

Description:    

    This is the BrowseSelection Callback for the DAR list.  As each DAR
is browsed in the DAR window, this callback is executed

Parameters:     Standard X Callback parameters

Returns:        None    

Creator:        Ron Green

Creation Date:  09/dd/1994
Notes:      
==============================================================================*/
void
cb_update_darmanager_form(
    Widget widget, XtPointer client_data, XmListCallbackStruct *cbs)
{
BEGIN_CONTEXT(widget)

    double      et_total_days ;

    Widget      menu_status ;

    DB_RECORD   **dar_rec ;

    dar_rec = (DB_RECORD **) db_nth_record(dars, cbs->item_position) ;

#define SHOW_FIELD(column, cast, textField) \
    (void) sprintf( display_string, \
        APS_PFMT(DAR, (column)), cast dar_rec[(column)] ) ; \
    XmTextFieldSetString((textField), display_string) ;

    SHOW_FIELD(DAR_DARID,      CAST_DAR_DARID,      TF_DAR_DARID)
    SHOW_FIELD(DAR_REQSTAT,    CAST_DAR_REQSTAT,    TF_REQSTAT)
    SHOW_FIELD(DAR_REQTIME,    CAST_DAR_REQTIME,    TF_REQTIME)
    SHOW_FIELD(DAR_USERID,     CAST_DAR_USERID,     TF_USERID)

    /* set the quicklook flag */
    if ( CAST_DAR_QUICKLOOK dar_rec[DAR_QUICKLOOK] == 'Y' )
    {
        XtVaSetValues( LABEL_QUICKLOOK,
            XtVaTypedArg, XmNlabelString, XmRString, "Yes", strlen("Yes") + 1,
            NULL );
    }
    else
    {
        XtVaSetValues( LABEL_QUICKLOOK,
            XtVaTypedArg, XmNlabelString, XmRString, "No", strlen("No") + 1,
            NULL );
    }

    SHOW_FIELD(DAR_STRTTIME,   CAST_DAR_STRTTIME,   TF_DAR_STRTTIME)
    SHOW_FIELD(DAR_ENDTIME,    CAST_DAR_ENDTIME,    TF_DAR_ENDTIME)

    tc_et_ASF_datetime_diff(
        (char *) dar_rec[DAR_STRTTIME], (char *) dar_rec[DAR_ENDTIME], 
        &et_total_days) ;

    (void) sprintf( display_string, "%8.2f", et_total_days ) ;
    XmTextFieldSetString(TF_DAR_total_days, display_string) ;

    SHOW_FIELD(DAR_REV,        CAST_DAR_REV,        TF_DAR_REV)

    SHOW_FIELD(DAR_ASCDSC,     CAST_DAR_ASCDSC,     TF_ASCDSC)
    SHOW_FIELD(DAR_NOBS,       CAST_DAR_NOBS,       TF_NOBS)
    SHOW_FIELD(DAR_REV,        CAST_DAR_REV,        TF_DAR_REV)

    /*
    -- show the full satellite name, not just the code.
    -- NOTE: get_satellite_name will not return NULL because of
    -- how it's handled in cb_show_dar_records()
    */
    (void) sprintf( display_string,
        APS_PFMT(DAR, DAR_SAT), get_satellite_name((char *) dar_rec[DAR_SAT]) );
    XmTextSetString(TF_SAT, display_string) ;
 
    SHOW_FIELD(DAR_SENSOR,     CAST_DAR_SENSOR,     TF_SENSOR)
    SHOW_FIELD(DAR_SITENAME,   CAST_DAR_SITENAME,   TF_DAR_SITENAME)

    SHOW_FIELD(DAR_NWLAT,     CAST_DAR_NWLAT,       TF_NWLAT)
    SHOW_FIELD(DAR_NWLON,     CAST_DAR_NWLON,       TF_NWLON)
    SHOW_FIELD(DAR_SWLAT,     CAST_DAR_SWLAT,       TF_SWLAT)
    SHOW_FIELD(DAR_SWLON,     CAST_DAR_SWLON,       TF_SWLON)

    SHOW_FIELD(DAR_NELAT,     CAST_DAR_NELAT,       TF_NELAT)
    SHOW_FIELD(DAR_NELON,     CAST_DAR_NELON,       TF_NELON)
    SHOW_FIELD(DAR_SELAT,     CAST_DAR_SELAT,       TF_SELAT)
    SHOW_FIELD(DAR_SELON,     CAST_DAR_SELON,       TF_SELON)

    SHOW_FIELD(DAR_NWLAT,     CAST_DAR_NWLAT,       TF_DAR_center_lat)
    SHOW_FIELD(DAR_NWLON,     CAST_DAR_NWLON,       TF_DAR_center_lon)
    SHOW_FIELD(DAR_RADIUS,    CAST_DAR_RADIUS,      TF_RADIUS)

    SHOW_FIELD(DAR_J1_OBS_FREQ,    CAST_DAR_J1_OBS_FREQ,      TF_J1OBS) 

#undef SHOW_FIELD

#define SHOW_FIELD(column, cast, textField) \
    (void) sprintf( display_string, \
        APS_PFMT(DAR, (column)), cast dar_rec[(column)] ) ; \
    XmTextSetString((textField), display_string) ;

    SHOW_FIELD(DAR_FOBS,       CAST_DAR_FOBS,       T_FOBS)
    SHOW_FIELD(DAR_USERCMNT,   CAST_DAR_USERCMNT,   T_USERCMNT)

    SHOW_FIELD(DAR_PLNRCMNT,   CAST_DAR_PLNRCMNT,   T_PLNRCMNT)

#undef SHOW_FIELD

    /*
    -- display the appropiate form
    -- that describes the shape
    */
    switch (CAST_DAR_SHAPE dar_rec[DAR_SHAPE])
    {
        case RECT_CHAR : /* rectangle */
                /* FALLTHROUGH */
        case QUAD_CHAR :  /* quadrilateral */
            (void) sprintf( display_string, "%6s", "QUAD" ) ;
            XtUnmanageChild(form_DARcircle) ;
            XtManageChild(form_DARquad) ;
            break ;

        case POINT_CHAR :  /* circle */
            (void) sprintf( display_string, "%6s", "CIRCLE" ) ;
            XtUnmanageChild(form_DARquad) ;
            XtManageChild(form_DARcircle) ;
            break ;
        default :   /* ERROR */
            (void) sprintf( display_string,
                "APS DB ERROR:\n Shape %c is not valid\n Fix DB and retry",
                CAST_DAR_SHAPE dar_rec[DAR_SHAPE] ) ;
            popup_message(XmDIALOG_ERROR, "APS:DB ERROR", 
                display_string, XtGrabNone) ;
            return ;
    }
    XmTextFieldSetString(TF_DAR_SHAPE, display_string) ;

    /* update the status option menu */
    if (strcmp( (char *) dar_rec[DAR_REQSTAT], APS_QUEUED_STATUS) == 0)
    {
        menu_status = DAR_status_QUE ;
    }
    else /* better be PLN */
    {
        menu_status = DAR_status_PLN ;
    }

    XtVaSetValues(DAR_menu_status,
        XmNmenuHistory, menu_status,
        NULL) ;

    /* turn on the edit and delete buttons */
    XtSetSensitive(pushButton_EditDAR, True) ;
    XtSetSensitive(menuBar_DELETE, True) ;
END_CONTEXT
}



/*==============================================================================
Function:       cb_set_darmanager_editability

Description:    
    Set the darmanager form's editability so that items that can
    be modified are set so that they can be
Parameters:     Standard X Callback parameters

Returns:        None    

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:      
==============================================================================*/
/* ARGSUSED2 */
void
cb_set_darmanager_editability(
    Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( widget )

    Boolean editability ;
    char    *darid = NULL ;
    int     dar_id ;
    int     retcode ;

    /* make sure a DAR is selected */
    if (!check_for_selected_dar( scrolledList_DARS ))
        return ;    /* error msg was already popped up */

    /* 
    -- Get permission before proceeding.  If cbs is NULL, this was not called
    -- because the edit button was pushed, but by the cb_save_dar_changes 
    -- function.  Therefore, we would NOT want to get permission, as it was 
    -- already granted.
    */

    /* 
    -- First get the DAR id and make sure it's an integer.  If not, pop up an
    -- error dialog.
    */

    if (cbs)
    {
        darid = gui_TF_string(TF_DAR_DARID) ;
        retcode = sscanf(darid, "%d", &dar_id) ;
        XtFree(darid) ;
        
        if( retcode != 1 )
        {
            (void) sprintf( display_string,
                           "DAR id must be an integer." );
            popup_message(XmDIALOG_ERROR, "APS:ERROR", 
                          display_string, XtGrabNone) ;
            return ;
        }

        retcode = gui_get_dar_permission(MU_EDIT_OR_DELETE_DAR, dar_id) ;

        /* 
        -- If the return code was <= 0, a dialog box explaining why the user
        -- was not granted permission to proceed has already popped up.
        */

        if (retcode <= 0)
          return ;

        darmgr_perm_id = retcode ;

        /* Permission granted.  We may now proceed. */
    }

    editability = (Boolean ) client_data ;

    XtSetSensitive(DAR_status_options, editability) ;

    gui_setEditable( T_PLNRCMNT, AG_TEXT, editability ) ;
    gui_setEditable( TF_J1OBS, AG_TEXTFIELD, editability ) ;

    /* turn off/on the other buttons */
    XtSetSensitive(menuBar_LOAD_DAR, !editability) ;
    XtSetSensitive(pushButton_SearchDAR, !editability) ;
    XtSetSensitive(pushButton_SortDAR, !editability) ;
    XtSetSensitive(pushButton_EditDAR, !editability) ;
    XtSetSensitive(menuBar_DELETE, !editability) ;
    XtSetSensitive(scrolledList_DARS, !editability) ;

    if (editability)
    {
        XtManageChild(pushButton_SaveDARChanges) ;
        XtManageChild(pushButton_CancelDARChanges) ;
    }
    else
    {
        XtUnmanageChild(pushButton_SaveDARChanges) ;
        XtUnmanageChild(pushButton_CancelDARChanges) ;
    }

END_CONTEXT
}



/*==============================================================================
Function:       cb_save_dar_changes

Description:    
    The activate callback for the SAVE pushbutton

Parameters:     Standard X Callback parameters

Returns:        None    

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:      
==============================================================================*/
/* ARGSUSED2 */
void
cb_save_dar_changes(Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( widget )

    Boolean cancel_changes ;

    char    *darid = NULL ;
    char    *plnrcmnt = NULL ;
    char    j1obsfreq[10] ;
    char    *darstatus ;
    int     index ;
    int     status ;
    char    *tmpStr;

    /* 
    -- if this function was activated as the result of the cancel edit
    -- pb then we want to do everything... other wise it is just
    -- being called to reset the screen, therefore we don't have
    -- to ask if the changes are to be kept... this is a cancel 
    -- activation
    */
    cancel_changes = (Boolean) client_data ;

    /* 
    -- get the current selected position, it will
    -- be needed for either the update or to reset the
    -- modified values
     */
    index = gui_XmList_selected_item(scrolledList_DARS) ;

    if (!cancel_changes)
    {
        /* verify the changes want to be made */
        darid = gui_TF_string(TF_DAR_DARID) ;
        (void) sprintf( question, "Save Changes to DAR ID %s?", darid ) ;

        if (AskUser(widget, question, NO) == YES)
        {
            /* get the modified values */
            tmpStr = gui_TF_string(T_PLNRCMNT) ;
            if (db_quote_doubler( tmpStr, &plnrcmnt ) < 0)
            {
                (void) sprintf( display_string,
                    "Out of Memory\n Too many processes running?" );
                popup_message(XmDIALOG_ERROR, "APS:ERROR", 
                    display_string, XtGrabNone) ;
                XtFree(darid) ;
                return ;
            }
            XtFree( tmpStr );

            tmpStr = gui_TF_string(TF_J1OBS) ;
            strcpy(j1obsfreq, tmpStr) ;
            XtFree( tmpStr );

            /* get the currently selected menu option choice */
            darstatus = gui_optionMenu_string(DAR_menu_status) ;

            (void) sprintf( fields_to_set, 
                "%s = '%s',\n%s = '%s',\n%s = %s",
                APS_COL(DAR, DAR_REQSTAT), darstatus,
                APS_COL(DAR, DAR_PLNRCMNT), plnrcmnt,
                APS_COL(DAR, DAR_J1_OBS_FREQ), j1obsfreq) ;

            (void) sprintf( where_clause, "\nwhere %s = %s",
                APS_COL(DAR, DAR_DARID), darid ) ;

            status = db_update_records(APS_dbproc, 
                APS_TABLE(DAR), fields_to_set, where_clause) ;

            if (status <= 0)
            {
                (void) sprintf( display_string,
                    "Error Updating DAR %s\nSee sybase messages on screen ",
                    darid ) ;
                popup_message(XmDIALOG_ERROR, "APS:DB ERROR", 
                    display_string, XtGrabNone) ;
                free(plnrcmnt) ;
                XtFree(darid) ;
                XtFree(darstatus) ;
                return ;
            }

            /* refresh the DAR list */
            cb_show_dar_records(widget, (XtPointer)scrolledList_DARS, NULL);

            free(plnrcmnt) ;
            XtFree(darstatus) ;
        }

        XtFree(darid) ;

    }  /* if not cancel pb */

    /* 
    -- whether the update was done or not, reselect the item in the list 
    -- and force the selection cb... 
    -- if the update was done it will display the changes... 
    -- if the update wasn't done it will display the original values
    -- 
    -- the record should be in the same position in the list
    -- NOTE: This may not be true if DARS were added while this update
    -- is being done.
     */
    XmListSelectPos(scrolledList_DARS, index, True) ;
    cb_set_darmanager_editability(widget, False, NULL) ;

    /* Free the permissions and reset the perm_id */

    (void) gui_free_permission(darmgr_perm_id, 
                        MU_EDIT_OR_DELETE_DAR, 
                        MU_DAR_ACTIVITY_TYPE) ;
    darmgr_perm_id = 0 ;

    /* 
    -- If there was a problem releasing the permission, an error dialog
    -- describing it has already popped up.
    */

END_CONTEXT
}



/*==============================================================================
Function:       cb_delete_dar_record

Description:    
    The activate callback for the DELETE options: REJECTED and COMPLETED.

Parameters:     Standard X Callback parameters

Returns:        None    

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:      
==============================================================================*/
/* ARGSUSED2 */
void
cb_delete_dar_record(Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( widget )

    char            *darid ;
    char            *apsStatus;
    char            *plnrcmnt;
    char            *tmpStr;

    int             retcode ;
    int             dar_id ;

    IMS_MSG_STRUCT  *imsMsgStruct = NULL;   /* struct for IMS error messages */

    char            *deleteOption;
    XmString        tmpXStr;

    /* make sure a DAR is selected */
    if (!check_for_selected_dar( scrolledList_DARS ))
        return ;    /* error msg was already popped up */

    apsStatus   = (char *) client_data;
    darid       = gui_TF_string(TF_DAR_DARID) ;
    XtVaGetValues( widget,
                    XmNlabelString, &tmpXStr,
                    NULL );
    XmStringGetLtoR( tmpXStr, XmFONTLIST_DEFAULT_TAG, &deleteOption );
    
    (void) sprintf( question, "DELETE DAR ID %s?\n\n(marked as %s)",
        darid, deleteOption ) ;
    if (AskUser(widget, question, NO) == YES)
    {
        /* Get permission before proceeding */

        retcode = sscanf(darid, "%d", &dar_id) ;
        
        if( retcode != 1 )
        {
            (void) sprintf( display_string,
                           "DAR id must be an integer." );
            popup_message(XmDIALOG_ERROR, "APS:ERROR", 
                          display_string, XtGrabNone) ;

            XtFree(darid) ;
            XtFree( deleteOption );

            return ;
        }

        retcode = gui_get_dar_permission(MU_EDIT_OR_DELETE_DAR, dar_id) ;

        /* 
        -- If the return code was <= 0, a dialog box explaining why the user was not
        -- granted permission to proceed has already popped up.
        */

        if (retcode <= 0)
        {
            XtFree(darid) ;
            XtFree( deleteOption );
            return ;
        }

        darmgr_perm_id = retcode ;

        /* Permission granted.  We may now proceed. */

        /*
        -- update the status & planner comments in the IMS data base
        */

        if ((imsMsgStruct = alloc_imsMsgStruct()) == NULL)
        {
            /* message was already popped up */
            XtFree(darid) ;
            XtFree( deleteOption );

            /* Free the permission and reset the perm_id */

            (void) gui_free_permission(darmgr_perm_id, 
                                MU_EDIT_OR_DELETE_DAR, 
                                MU_DAR_ACTIVITY_TYPE) ;
            darmgr_perm_id = 0 ;

            return ;
        }

        tmpStr = gui_TF_string(T_PLNRCMNT);
        if (db_quote_doubler( tmpStr, &plnrcmnt ) < 0)
        {
            (void) sprintf( display_string,
                "IMS/DADS and APS DBs unchanged\n Out of Memory\n Too many processes running?" );
            popup_message(XmDIALOG_ERROR, "APS:ERROR", 
                display_string, XtGrabNone) ;
            free_imsMsgStruct( &imsMsgStruct );
            XtFree(darid) ;
            XtFree( deleteOption );

            /* Free the permission and reset the perm_id */

            (void) gui_free_permission(darmgr_perm_id, 
                                MU_EDIT_OR_DELETE_DAR, 
                                MU_DAR_ACTIVITY_TYPE) ;
            darmgr_perm_id = 0 ;

            return ;
        }
        XtFree( tmpStr );
        
        if (aps_darStatus( atoi( darid ), apsStatus, plnrcmnt, imsMsgStruct )
            < IMS_OK)
        {
            /* couldn't update the IMS DAR, do NOT delete from the APS DB */
            (void) sprintf( display_string,
                "ERROR can't update IMS/DADS DAR status\n APS DAR %s is not deleted\n",
                darid );
            display_ims_error( display_string, imsMsgStruct );

            free_imsMsgStruct( &imsMsgStruct );
        }
        else    /* updated in IMS DAR, ie, status: Completed or Rejected */
        {
            free_imsMsgStruct( &imsMsgStruct );

            /* IMS DAR is updated, now delete the DAR from the APS db */
            (void) sprintf( where_clause,
                "where %s = %s", APS_COL(DAR, DAR_DARID ), darid ) ;
            if (db_delete_records( APS_dbproc, APS_TABLE(DAR), where_clause )
                != 1)
            {
                /* DBs not in sync: can't delete from APS, IMS shows it's done*/
                (void) sprintf( display_string,
                    "APS DB Error:\n can't delete DAR %s\n IMS/DADS db shows delete is done\n (fix APS db, then delete again)",
                    darid );
                popup_message( XmDIALOG_ERROR, "APS:DB ERROR", 
                    display_string, XtGrabNone ) ;
            }
            else
            {
                /* refresh the DAR list */
                cb_show_dar_records( widget, (XtPointer) scrolledList_DARS,
                    NULL ) ;

                /* clear out the deleted record from the fields */
                XmTextFieldSetString(TF_DAR_DARID,      blank) ;
                XmTextFieldSetString(TF_REQSTAT,    blank) ;
                XmTextFieldSetString(TF_REQTIME,    blank) ;
                XmTextFieldSetString(TF_USERID,     blank) ;
                XtVaSetValues( LABEL_QUICKLOOK,
                    XtVaTypedArg, XmNlabelString, XmRString,
                    blank, strlen(blank) + 1,
                    NULL );
                XmTextFieldSetString(TF_DAR_STRTTIME,   blank) ;
                XmTextFieldSetString(TF_DAR_ENDTIME,    blank) ;
                XmTextFieldSetString(TF_DAR_total_days, blank) ;
                XmTextFieldSetString(TF_DAR_REV,        blank) ;
                XmTextFieldSetString(TF_NOBS,   blank) ;

                XmTextFieldSetString(TF_SAT,      blank) ;
                XmTextFieldSetString(TF_SENSOR,   blank) ;

                XmTextFieldSetString(TF_DAR_SITENAME, blank) ;
                XmTextFieldSetString(TF_ASCDSC, blank) ;
 
                XmTextFieldSetString(TF_DAR_SHAPE, blank) ;

                XmTextFieldSetString(TF_NWLAT, blank) ;
                XmTextFieldSetString(TF_NWLON, blank) ;
                XmTextFieldSetString(TF_SWLAT, blank) ;
                XmTextFieldSetString(TF_SWLON, blank) ;
                XmTextFieldSetString(TF_NELAT, blank) ; 
                XmTextFieldSetString(TF_NELON, blank) ;
                XmTextFieldSetString(TF_SELAT, blank) ;
                XmTextFieldSetString(TF_SELON, blank) ;

                XmTextFieldSetString(TF_DAR_center_lat, blank) ;
                XmTextFieldSetString(TF_DAR_center_lon, blank) ;
                XmTextFieldSetString(TF_RADIUS,     blank) ;
                XmTextFieldSetString(TF_J1OBS,     blank) ;

                XmTextSetString(T_FOBS,   blank) ;
                XmTextSetString(T_PLNRCMNT, blank) ;
                XmTextSetString(T_USERCMNT, blank) ;
            }
        }
        free( plnrcmnt );

        /* Free the permission and reset the perm_id */
        (void) gui_free_permission(darmgr_perm_id, 
                            MU_EDIT_OR_DELETE_DAR, 
                            MU_DAR_ACTIVITY_TYPE) ;
        darmgr_perm_id = 0 ;
    }

    XtFree(darid) ;
    XtFree( deleteOption );


END_CONTEXT
}



/*==============================================================================
Function:       cb_load_dars_from_ims

Description:
    The activate callback for the LOAD function.

Parameters:     Standard X Callback parameters

Returns:        None

Creator:        Ron Green

Creation Date:  09/dd/1994
Notes:      
==============================================================================*/
/* ARGSUSED2 */
void
cb_load_dars_from_ims(
    Widget widget, XtPointer client_data, XtPointer call_data)
{
BEGIN_CONTEXT( DAR_manager )

    IMS_MSG_STRUCT  *imsMsgStruct = NULL;   /* struct for IMS error messages */

    int             save_type ;
    int             dars_added = 0 ;

    llist           *darlist = NULL;
    cursor          ptr ;
    DB_RECORD       **dar_rec ;

    char            *tmpPlnrCmnt = NULL;
    char            *tmpStatus;
    int             tmpDarid;

    /* get the type of load (replace/append the dars */
    save_type = (int) client_data ;

    /* get the new DARs from IMS and create a DAR list */
    if ((darlist = get_darList_from_ims()) == NULL)
        return;

    if (save_type == LOAD_DARS_REPLACE)
    {
        (void) sprintf( question, 
            "DELETE ALL CURRENT APS DARs and REPLACE\nthem with the %d %s IMS/DADS DARs?\n",
            NUMELTS(darlist), APS_QUERY_STAT_TYPE ) ;

        if (AskUser(UxTopLevel, question, NO) == YES)
        {
            /* delete all records in the DAR relation */
            if (db_delete_records(APS_dbproc, APS_TABLE(DAR), NULL) == -1)
            {
                (void) sprintf( display_string,
                    "APS DB ERROR:\n can't delete all the DARs\n Fix the DB and retry" ) ;
                popup_message(XmDIALOG_ERROR, "APS:DB ERROR", 
                    display_string, XtGrabNone);
                DEL_LIST(darlist) ;
                return ;  
            }
        }
        else  /* no replacement of records */
        {
            DEL_LIST(darlist) ;
            return ;  
        }   
    }
    else /* this is an append operation */
    {
        (void) sprintf( question, 
            "Add the %d %s IMS/DADS DARs\ninto the APS DAR DB?\n", 
            NUMELTS(darlist), APS_QUERY_STAT_TYPE ) ;

        if (AskUser(UxTopLevel, question, NO) == NO)
        {
            DEL_LIST(darlist) ;
            return ;  
        }
        /* remove any duplicate dars that are already in the dbase */
        dar_rec = FIRST(darlist, ptr) ;
        while (dar_rec)
        {
            /* check if the id to be loaded already exists */
            (void) sprintf(where_clause, "where %s = %ld",
                APS_COL(DAR, DAR_DARID), 
                CAST_DAR_DARID dar_rec[DAR_DARID]) ;

            if (db_num_records(APS_dbproc, APS_TABLE(DAR), where_clause))
            {
                (void) sprintf(display_string, 
                "DUPLICATE DAR ERROR:\n DAR ID %ld already exists in APS DB\n DAR will not be appended\n",
                    CAST_DAR_DARID dar_rec[DAR_DARID]) ;
                popup_message(XmDIALOG_WARNING, "APS:WARNING", 
                    display_string, XtGrabNone);
                (void) DEL_AT_CURSOR(darlist, ptr) ;
            }
            dar_rec = NEXT(darlist, ptr) ;
        }
    }

    if (NUMELTS(darlist))
    {
        for (dar_rec = FIRST( darlist, ptr ) ; dar_rec != NULL
            ; dar_rec = NEXT( darlist, ptr ))
        {
            if (db_insert_single_record( APS_dbproc, dar_rec,
                APS_TABLE(DAR), APS_CDEFS(DAR) ))
            {
                dars_added++;

                /* update the status in the IMS data base */

                if ((imsMsgStruct = alloc_imsMsgStruct()) == NULL)
                {
                    DEL_LIST(darlist) ;
                    return ;    /* message was already popped up */
                }

                tmpDarid  = CAST_DAR_DARID dar_rec[DAR_DARID];
                tmpStatus = CAST_DAR_REQSTAT dar_rec[DAR_REQSTAT];
                tmpPlnrCmnt = CAST_DAR_PLNRCMNT dar_rec[DAR_PLNRCMNT];

                if (aps_darStatus( tmpDarid, tmpStatus, tmpPlnrCmnt,
                    imsMsgStruct ) < IMS_OK)
                {
                    /* couldn't update the IMS DAR, delete from the APS DB */

                    (void) sprintf( where_clause, 
                        "where %s = %d",  APS_COL(DAR, DAR_DARID ), tmpDarid );
                    if (db_delete_records( APS_dbproc, APS_TABLE(DAR),
                        where_clause ) == 1)
                    {
                        /* succeeded in deleting DAR from the APS db */
                        dars_added--;
                        (void) sprintf( display_string,
                            "IMS/DADS DB Error: can't update status\n DAR %d not added to APS DB\n",
                            tmpDarid );
                    }
                    else
                    {
                        /* couldn't delete from the aps db, leave DBs as is */
                        (void) sprintf( display_string,
                            "IMS/DADS and APS DB ERRORs: can't update status,\n but DAR %d IS added to APS DB\n (IMS/DADS and APS DBs are out of sync); IMS/DADS db ERROR:\n",
                            tmpDarid );
                    }
                    display_ims_error( display_string, imsMsgStruct );
                }

                free_imsMsgStruct( &imsMsgStruct );
            }
            else    /* couldn't insert the record in the APS db */
            {
                (void) sprintf( display_string, 
                    "APS DB ERROR:\n can't add DAR %d \n to APS DB, skipping it\n",
                    tmpDarid ) ;
                popup_message( XmDIALOG_ERROR, "APS:ERROR", 
                    display_string, XtGrabNone );
            }
        }

        (void) sprintf( display_string, "Number of DARs added: %d\n",
            dars_added ) ;
        popup_message( XmDIALOG_INFORMATION, "APS:INFORMATION",
            display_string, XtGrabNone );
    }

    /*
    -- the global variable is used here to refresh the list
    -- another way may have to be come up with so as to
    -- refresh the original list without referencing a global
    */
    cb_show_dar_records(widget, (XtPointer) scrolledList_DARS, NULL) ;

    /* remove the llist generated from the file records */
    DEL_LIST(darlist) ;

END_CONTEXT

    return;
}



/*==============================================================================
Function:       dar_report_done 

Description:    

Parameters:     

Returns:        

Creator:        Ron Green

Creation Date:  12/06/1994

Notes:      
==============================================================================*/
/* ARGSUSED1 */
static void
dar_report_done(process, notused)
    PROCESS_INFO *process ;
    void *notused ;
{
    switch (process->exit_status)
    {
    case APS_EXIT_OK :
        (void) sprintf( display_string,
                "DAR Report:\n\n Completed Successfully\n" ) ;
        popup_message(XmDIALOG_INFORMATION, "APS:INFORMATION", display_string,
                XtGrabNone);
        break ;
    case APS_EXIT_ERROR :
        (void) sprintf( display_string, "DAR Report:\n\n Unsuccessful Run" ) ;
        popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone);
        break ;
    case APSGUI_EXIT_COREDUMP : /* process core dumped */
        (void) sprintf( display_string,
                "DAR Report:\n\n Signal Caught CORE DUMPED" ) ;
        popup_message( XmDIALOG_ERROR, "APS:ERROR",
                display_string, XtGrabNone );
        break ;
    default :   /* process caught a signal, but no core dump */
        (void) sprintf( display_string,
                "DAR Report:\n\n SIGNAL Caught (signal = %d)",
                -(process->exit_status) ) ;
        popup_message( XmDIALOG_ERROR, "APS:ERROR",
                display_string, XtGrabNone );
        break ;
    }
}   



/*==============================================================================
Function:       cb_print_dars

Description:    print dars to printer

Parameters:     Standard X Callback Parameters

Returns:        none

Creator:        Ron Green

Creation Date:  11/07/1994

Notes:      
==============================================================================*/
void
cb_print_dars(
    Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( DAR_manager )

    PROCESS_INFO *process ;
    DB_RECORD **dar_rec ;
    char command[255] ;
    char report_option[255] ;
    char *search_str ;
    int status ;
    char *file = NULL ;

    int print_type ;
    int index ;

    print_type = (int) client_data ;

    /* 
    -- get the search criteria (where clause) 
    -- to pass to the report
    */
    switch (print_type)
    {
    case PRINT_DARS_ALL :
    case PRINT_DARS_ALL_TO_FILE :
        (void) sprintf( where_clause, "where %s > 0", APS_COL(DAR, DAR_DARID) );
        break ;

    case PRINT_DARS_SELECTED :
    case PRINT_DARS_SELECTED_TO_FILE :
        if (!(index = check_for_selected_dar( scrolledList_DARS )))
        {
            /*
            -- should never get here...
            -- since check is also done in cb_set_print_dar
            */
            return ;    /* error msg was already popped up */
        }

        dar_rec = db_nth_record(dars, index) ;

        (void) sprintf( where_clause, "where %s = %ld",
            APS_COL(DAR, DAR_DARID), 
            CAST_DAR_DARID dar_rec[DAR_DARID] ) ;
        break ;

    case PRINT_DARS_CURRENT :
    case PRINT_DARS_CURRENT_TO_FILE :
        search_str = XmTextFieldGetString(TF_DAR_searchclause) ;
        if (strlen(search_str) == 0)
            (void) sprintf( where_clause, "%s", blank ) ;
        else
            (void) sprintf( where_clause, "%s", search_str ) ;
        XtFree(search_str) ;
        break ;

    default :
        (void) sprintf( display_string,
            "Incorrect DAR TYPE %d passed to\ncb_print_dars()",
            print_type ) ;
        gui_aps_internal_error( XmDIALOG_ERROR,
            __FILE__, __LINE__, display_string ) ;
        return ;
    }

    /* if a dar is to be written to a file get the filename */
    if (print_type == PRINT_DARS_SELECTED_TO_FILE
    ||  print_type == PRINT_DARS_CURRENT_TO_FILE    
    ||  print_type == PRINT_DARS_ALL_TO_FILE)
    {
        file = gui_filebox_filename(widget,
                (XmFileSelectionBoxCallbackStruct *)cbs) ;
        if (!file)
            return ;

        if (gui_ask_file_replacement(file) == NO)
        {
            XtFree(file) ;
            return ;
        }
        (void) sprintf( report_option, "-o %s", file ) ; 
        XtFree(file) ; 
    }
    else /* report is to be written to printer */
        (void) sprintf( report_option, "-p" ) ;

    (void) sprintf( command, 
        "runrw dar_report -U %s -P %s -V where_clause \"%s\" %s",
        userid, password, 
        where_clause,
        report_option ) ; 

    process = (PROCESS_INFO *) create_process(command, &status, TRUE, NULL,
        NULL, NULL, dar_report_done, NULL) ;

    if (!process || start_process( process ))
    {
        (void) sprintf(display_string, 
            "Can't run process to print the dars\n Too many processes running?"
            ) ;
        popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone) ;
        if (process)
            destroy_process( process ) ;
        return ;
    }

END_CONTEXT
}



/*==============================================================================
Function:       cb_set_print_dars_to_file_cb

Description:    

Parameters:     Standard X Callback parameters

Returns:        None

Creator:        Ron Green

Creation Date:  12/03/1994

Notes:      
==============================================================================*/
/* ARGSUSED2 */
void
cb_set_print_dars_to_file_cb(
    Widget widget, XtPointer client_data, XtPointer call_data)
{
BEGIN_CONTEXT( widget )

    int print_type ;
    char *aps_fullpath_dirmask ;
    XmString dirmask ;

    aps_fullpath_dirmask = aps_fullpath(APS_REPORTS, "*.rpt") ;

    dirmask = XmStringCreateLocalized(aps_fullpath_dirmask) ;
    XmFileSelectionDoSearch(filebox, dirmask) ;

    XmStringFree(dirmask) ;
    free(aps_fullpath_dirmask) ;

    print_type = (int) client_data ;

    if (print_type == PRINT_DARS_SELECTED
    ||  print_type == PRINT_DARS_SELECTED_TO_FILE)
    {
        if (!check_for_selected_dar( scrolledList_DARS ))
            return ;    /* error msg was already popped up */
    }

    /* remove any currently installed callbacks for the filebox */
    XtRemoveAllCallbacks(filebox, XmNokCallback) ;

    XtVaSetValues(filebox, 
        RES_CONVERT(XmNdialogTitle, "APS File: Save DAR Report") ,
        NULL) ;

    /* now install the print_dars... callback passing the type of print to do */
    XtAddCallback(filebox, XmNokCallback, cb_print_dars, client_data) ;

    /* now bring up the filebox */
    XtPopup(XtParent(filebox), XtGrabExclusive) ;

END_CONTEXT
}
