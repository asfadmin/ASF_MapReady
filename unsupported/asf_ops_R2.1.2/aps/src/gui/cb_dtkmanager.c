#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:   cb_dtkmanager.c

Description:    Contains the callback code and subfunctions for the dtk manager

External Functions Defined:
                void cb_init_dtk_search_clause
                void cb_show_dtk_records
                void cb_clear_dtkmanager_form
                void cb_update_dtkmanager_form
                void cb_set_dtkmanager_editability
                void cb_get_cvrg_points
                void cb_save_dtk_changes
                void cb_create_dtk
                void cb_delete_dtk_record
                void cb_print_dtks
                void cb_set_print_dtks_to_file_cb
                void cb_set_dtk_status_menus
    
File Scope Functions:
                void t_cb_planning_perm_timeout
                void check_in_station_mask_status
                void display_dtkmanager_fields
                void dtk_report_done
                void form_bad_dtkstat_msg
                void form_bad_transid_msg
                void free_timed_planning_permission
                int  get_timed_planning_permission
                char *get_fa_tapes_from_form
                void mini_report
                void set_cvrg_fields
                int  update_direction_status

    
External Variables Defined:
    
File Scope Variables:
    
Notes:

==============================================================================*/
#pragma ident   "@(#)cb_dtkmanager.c	5.2 98/03/12 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_dtkmanager.c"

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h> 
#include <time.h>

#include <Xm/FileSB.h>
#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>

#include "dapps_defs.h"
#include "aps.h"
#include "aps_defs.h"
#include "aps_extern.h"

#include "mu_utilities.h"
#include "db_sybint.h"
#include "dtkm_utilities.h"
#include "apspath.h"
#include "aps_db_table.h"
#include "db_dtk.h"
#include "db_j1_dn_times.h"
#include "db_dl2obs.h"
#include "nmalloc.h"

#include "satmenus.h"
#include "gui_defs.h"
#include "gui_utils.h"
#include "gui_mu_utils.h"
#include "subprocess.h"
#include "timeconv.h"

#define CONTEXT_MACRO_ACCESS 1
#include "vc_dtkmanager.h"
#undef CONTEXT_MACRO_ACCESS

#include "cb_dtkmanager.h"
#include "cb_datetime.h"


/*==============================================================================
    Constants/Macros
==============================================================================*/

#define XDOWNLINK                   0
#define XOBSERVE                    1
#define XDUMP                       2
#define XRECORD                     3

/* list-types used to differentiate dtk reporting means: used in mini_report */
#define DTK_TYPE_LIST               0
#define ANT_DOWN_LIST               1

/* timed permission action values */
#define DTK_PERM_T_O_OFF            0
#define DTK_PERM_T_O_ON             1
#define DTK_PERM_T_O_WARN           2
#define DTK_PERM_T_O_EXPIRE         3


/*==============================================================================
    Function Prototypes (extern or static)
==============================================================================*/
extern void     popup_message();
extern void     sscvrev2time() ;


/*==============================================================================
    Global Variables
==============================================================================*/
extern XtAppContext UxAppContext ;
extern Widget       DTK_manager ;
extern Widget       filebox ;

extern char         display_string[] ;

static llist        *dtks ;

static int          dtk_timed_pln_perm = 0 ; /*currently held in-plan perm. id*/

static char         *FA_TapeCode_ASF        = "ASF" ;
static char         *FA_TapeCode_ESA        = "ESA" ;
static char         *FA_TapeCode_NASDA      = "NAS" ;
static char         *FA_TapeCode_CSA        = "CSA" ;
static char         *FA_TapeCode_ESAASF     = "ESF" ;
static char         *FA_TapeCode_NASDAASF   = "NSF" ;
static char         *FA_TapeCode_CSAASF     = "CEF" ;

/* valid activities */
static char         *activities[] =
{
    DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
    DTKM_ACTID_REALTIME_OBSERVATION_CODE,
    DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE,
    DTKM_ACTID_RECORDING_OBSERVATION_CODE
} ;

static DB_RECORD    **orig_dtk_rec = NULL;  /* dtk record at start of editing */


#define BEGIN_CONTEXT( widget ) \
    _UxCDTKManager          *UxSaveCtx; \
    UxSaveCtx = UxDTKManagerContext; \
    UxDTKManagerContext = \
            (_UxCDTKManager *) UxGetContext( widget ); \
    {

#define END_CONTEXT \
    } \
    UxDTKManagerContext = UxSaveCtx;


/*==============================================================================
Function:       turn_off_non_sar_sensors()
 
Description:    In the sensor menu, turn off all non-sar-sensors.
                If the sensor does not start with S, then turn it
                off using XtUnmanageChild(sensor_button)
 
Creator:        Lawrence Stevens
 
Creation Date:  Wed Oct 15 18:22:49 PDT 1997
 
==============================================================================*/
#include <db_satsensor.h>   /* for CAST_SATSENSOR_SENSOR  etc.   */
void
turn_off_non_sar_sensors(Widget widget)
{
BEGIN_CONTEXT( widget )

    Widget      sensor_button ;
    char        *sat_name ;
    char        *curr_satname ;
    char        *sensorname ;

    SATELLITE   *curr_sat_rec ;
    cursor      sat_ptr ;
    DB_RECORD   **sensor_rec ;
    cursor      sensor_ptr ;
 
    /*
    -- now get the name of the current
    -- satellite for the purpose of determining
    -- what sensors to turn off
    -- the sensor list is in the matching satellite record
    */
    sat_name     = gui_optionMenu_string( optionMenu_sat ) ;
    curr_satname = get_satellite_code( sat_name ) ;
 
    for (curr_sat_rec = FIRST( satellites, sat_ptr )
        ; curr_sat_rec
        ; curr_sat_rec = NEXT(satellites, sat_ptr))
    {
        if (strcmp(curr_sat_rec->satname, sat_name) == 0)
            break ;
    }
    if (curr_sat_rec)
    {
 
        /*
        -- turn off all non-SAR sensors
        -- set the option menu to one of the needed sensors
        -- this way one that is needed is displayed
        */
     
        for (sensor_rec = FIRST(curr_sat_rec->sensors, sensor_ptr)
            ; sensor_rec != NULL
            ; sensor_rec = NEXT(curr_sat_rec->sensors, sensor_ptr))
        {
            sensorname = (char *)
                CAST_SATSENSOR_SENSOR sensor_rec[SATSENSOR_SENSOR] ;
            if( *sensorname != 'S' )
            {
                /*
                -- sensor is NOT a SAR sensor
                -- unmanage this sensor.
                */
                sensor_button = XtNameToWidget(subMenu_sensor, sensorname) ;
                if (sensor_button )
                {
                    XtUnmanageChild(sensor_button) ;
                }
            }
        }/* end for sensors.  */
    }  /* endif   if (curr_sat_rec)   */

END_CONTEXT

    return ;
 
}


/*==============================================================================
Function:       cb_init_dtk_search_clause

Description:    
                Creation callback to set the initial search clause for
                querying for DTK records

Parameters:     Standard X Callback parameters

Returns:        None

Creator:        Teresa McKillop

Creation Date:  05/07/96

Notes:      
==============================================================================*/
/* ARGSUSED1 */
void
cb_init_dtk_search_clause( Widget widget, XtPointer client_data_UNUSED,
    XtPointer cbs_UNUSED )
{
BEGIN_CONTEXT( widget )

    char    current_time[ASF_TIME_STR_LENGTH+1] ;
    char    *timePtr = current_time ;

    char    *colonStr = ":" ;
    int     year ;
    int     doy ;

    /* get "yesterday" in asf time format */
    (void) tc_systime2asf( current_time ) ; /* only ret.s TRUE, so ignore ret */
    year = atoi( strtok( timePtr, colonStr ) ) ;
    doy  = atoi( strtok( NULL, colonStr ) ) ;
    if (doy > 1)
        doy-- ;
    else    /* 1st day of year: yesterday is last day of previous year */
    {
        year-- ;
        doy = 365 + tc_leapyr( year ) ;
    }

    (void) sprintf( display_string, "where %s >= '%d:%03d:00:00:00.000'",
        APS_COL( DTK, DTK_STRTTIME ), year, doy ) ;

    /* update the search text field */
    XmTextFieldSetString( TF_DTK_searchclause, display_string ) ;

END_CONTEXT
    return ;
}


/*==============================================================================
Function:       cb_show_dtk_records

Description:    
    This function retrieves DTK records from the database and displays
    them on the screen

Parameters:     Standard X Callback parameters

Returns:        None

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:      
==============================================================================*/
/* ARGSUSED2 */
void
cb_show_dtk_records( Widget widget_UNUSED, XtPointer client_data,
    XtPointer cbs_UNUSED )
{
    int     i, num_dtks ;
    char    *sat_string ;
    char    *order_columns ;
    char    *search_str ;
    char    format_string[255] = { NULL } ;
    cursor  ptr ;
    DB_RECORD **dtk_rec ;
 
    XmStringTable str_list ;

    Widget list_widget = (Widget) client_data ;

BEGIN_CONTEXT( list_widget )

    /*
    -- get the available dtks from the db table
    -- and allocate a String Table to hold them
    */

    if (dtks)
    {
        DEL_LIST( dtks ) ;
        dtks = NULL ;
    }

    order_columns = XmTextFieldGetString( TF_DTK_sortclause ) ;
    search_str    = XmTextFieldGetString( TF_DTK_searchclause ) ;

    (void) sprintf( orderby_cols, "%s", order_columns ) ;
    (void) sprintf( where_clause, "%s", search_str ) ;

    if (strcmp( where_clause, "where" ) == 0)
        where_clause[0] = NULL ;

    XtFree( order_columns ) ;
    XtFree( search_str ) ;

    TimeoutCursors( True, True, NULL ) ;
    dtks = db_get_records( APS_dbproc, APS_TABLE( DTK ), 
        where_clause, orderby_cols, APS_CDEFS( DTK ), ALL_COLS ) ;

    if (!dtks)
    {
        TimeoutCursors( False, False, NULL ) ;
        (void) sprintf( display_string, 
            "Error with the APS DB\nNeed to fix the DB" ) ;
        popup_message( XmDIALOG_ERROR, "APS:DB ERROR" , 
            display_string, XtGrabNone ) ;

        /* since we had a db problem ensure that no items are in the list */
        XtVaSetValues( list_widget,
            XmNitemCount, 0,
            NULL ) ;

        return ;
    }

    num_dtks = NUMELTS( dtks );
    str_list = (XmStringTable) XtMalloc( num_dtks * sizeof(XmString) ) ;

    /* create the format string for the display window */ 
    if (*format_string == NULL)
    {
        int     fieldWidth;
        int     tmpint;
        char    *tmpstr;

        (void) sprintf( format_string, "%s/%s", "%8s",
            tmpstr = aps_max_pfmt( DTK, DTK_SENSOR, TRUE ) );
        free( tmpstr );
        for (tmpint = MAX_REV, fieldWidth = 0 ; tmpint > 0 ; fieldWidth++)
            tmpint /= 10;
        (void) sprintf( format_string, "%s %s", format_string,
            tmpstr = aps_set_fw_pfmt( DTK, DTK_REV, fieldWidth, FALSE ) );
        free( tmpstr );
        for (tmpint = APS_MAX_DTKID, fieldWidth = 0 ; tmpint > 0 ; fieldWidth++)
            tmpint /= 10;
        (void) sprintf( format_string, "%s %s", format_string,
            tmpstr = aps_set_fw_pfmt( DTK, DTK_DTKID, fieldWidth, FALSE ) );
        free( tmpstr );
        (void) sprintf( format_string, "%s %s", format_string, "%-8.8s") ;
        (void) sprintf( format_string, "%s%*s%s%*s%s%*s%s", format_string,
            2, blank_str,
            "%17.17s",
            2, blank_str,
            "%17.17s",
            2, blank_str,
            tmpstr = aps_max_pfmt( DTK, DTK_DTKSTAT, FALSE ) );
        free( tmpstr );
        (void) sprintf( format_string, "%s  %s", format_string,
            tmpstr = aps_max_pfmt( DTK, DTK_FADTKID, TRUE ) ) ; 
        free( tmpstr );
    }

    i = 0 ;
    for (dtk_rec = (DB_RECORD **) FIRST( dtks, ptr ) ; dtk_rec ;
        dtk_rec = (DB_RECORD **) NEXT( dtks, ptr ))
    {
        if ((sat_string = get_satellite_name( CAST_DTK_SAT dtk_rec[DTK_SAT] ))
            == NULL)
        {
            /*
            -- sat code is not in internal satellite table:
            -- remove dtk from list: later processing is based on sat name
            */

            (void) sprintf( display_string,
                "Satellite \"%s\" is not\n    in internal sat_name table\n    SKIPPING this DTK",
                CAST_DTK_SAT dtk_rec[DTK_SAT] ) ;
            gui_aps_internal_error( XmDIALOG_ERROR,
                __FILE__, __LINE__, display_string ) ;

            /* remove from the dtk list */
            (void) DEL_AT_CURSOR( dtks, ptr );
            num_dtks-- ;
            /* skip this one and get the next */
            continue;
        }

        (void) sprintf( display_string, format_string,
            sat_string,
            CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
            CAST_DTK_REV dtk_rec[DTK_REV],
            CAST_DTK_DTKID dtk_rec[DTK_DTKID],
            CAST_DTK_DTKDATE dtk_rec[DTK_DTKDATE],
            CAST_DTK_STRTTIME dtk_rec[DTK_STRTTIME],
            CAST_DTK_STOPTIME dtk_rec[DTK_STOPTIME],
            CAST_DTK_DTKSTAT dtk_rec[DTK_DTKSTAT],
            CAST_DTK_FADTKID dtk_rec[DTK_FADTKID] ) ;

        str_list[i++] = XmStringCreateLocalized( display_string ) ;
    }

    XtVaSetValues( list_widget,
        XmNitems, str_list,
        XmNitemCount, num_dtks,
        NULL ) ;

    (void) sprintf( display_string, "%5d", num_dtks ) ;
    XmTextFieldSetString( TF_DTK_recordcount, display_string ) ;

    for (i = 0; i < num_dtks ;i++)
        XmStringFree( str_list[i] ) ;
    XtFree( (char *) str_list ) ;
    TimeoutCursors( False, False, NULL ) ;

END_CONTEXT
}



/*==============================================================================
Function:       set_cvrg_fields

Description:    Sets the coverage fields on the dtk form

Parameters:     
                strtlat - start latitude
                stoplat - stop latitude
                nrlat1  - latitude for near point 1
                nrlon1  - longitude for near point 1
                farlat1 - latitude for far point 1
                farlon1 - longitude for far point 1
                nrlat2  - latitude for near point 2
                nrlon2  - longitude for near point 2
                farlat2 - latitude for far point 2
                farlon2 - longitude for far point 2

Returns:        

Creator:        Teresa McKillop

Creation Date:  01/24/1995

Notes:      
==============================================================================*/
static void
set_cvrg_fields( float strtlat, float stoplat,
    float nrlat1, float nrlon1, float farlat1, float farlon1,
    float nrlat2, float nrlon2, float farlat2, float farlon2 )
{
#define SHOW_FIELD( column, textField ) \
    (void) sprintf( display_string, "%-8.3f", (column) ) ; \
    XmTextFieldSetString( (textField), display_string ) ; \

    SHOW_FIELD( strtlat, TF_STRTLAT ) ;
    SHOW_FIELD( nrlat1, TF_NRLAT1 ) ;
    SHOW_FIELD( nrlon1, TF_NRLON1 ) ;
    SHOW_FIELD( farlat1, TF_FARLAT1 ) ;
    SHOW_FIELD( farlon1, TF_FARLON1 ) ;

    SHOW_FIELD( stoplat, TF_STOPLAT ) ;
    SHOW_FIELD( nrlat2, TF_NRLAT2 ) ;
    SHOW_FIELD( nrlon2, TF_NRLON2 ) ;
    SHOW_FIELD( farlat2, TF_FARLAT2 ) ;
    SHOW_FIELD( farlon2, TF_FARLON2 ) ;

#undef SHOW_FIELD
}



/*==============================================================================
Function:       update_direction_status

Description:    Updates the DTK direction option menu

Parameters:     

Returns:        OK      - if direction status is updated without problems
                ERROR   - if direction is not a valid direction status

Creator:        Ron Green

Creation Date:  01/24/1995

Notes:      
==============================================================================*/
static int
update_direction_status( Widget widget, char direction )
{
BEGIN_CONTEXT( widget ) 

    Widget  menu_status ;

    /* update the direction option menu */
    switch (direction)
    {
    case 'A' :
        menu_status = subMenu_dtk_direction_ascend ;
        break ;
    case 'D' :
        menu_status = subMenu_dtk_direction_descend ;
        break ;
    case '-' :
        menu_status = subMenu_dtk_direction_cvrgNotAllowed ;
        break;
    default :
        return (ERROR);
    }
 
    XtVaSetValues( optionMenu_dtk_direction,
        XmNmenuHistory, menu_status,
        NULL ) ;

END_CONTEXT

    return (OK);
}



/*==============================================================================
Function:       display_dtkmanager_fields

Description:    displays the fields in the dtkmanager form,
                getting their values from the input dtk record

Parameters:     

Returns:        

Creator:        Teresa McKillop (stolen from original cb_update_dtkmanager_form)

Creation Date:  12/14/95

Notes:      
==============================================================================*/
static void
display_dtkmanager_fields( Widget widget, DB_RECORD **dtk_rec )
{
BEGIN_CONTEXT( widget ) 

    Widget menu_status ;
    Widget sat_button ;
    Widget stn_button ;

    char activity[4] ;
    char fa_tapes[4] ;
    char antenna[APS_MAX_ANTENNA_LEN+1];
    char *satname ;

    /* update the status option menu */
    menu_status = NULL ;
    if (strcmp( (char *) dtk_rec[DTK_DTKSTAT], "QUE") == 0)
        menu_status = subMenu_dtk_status_QUE ;
    else if (strcmp( (char *) dtk_rec[DTK_DTKSTAT], "PLN") == 0)
        menu_status = subMenu_dtk_status_PLN ;
    else if (strcmp( (char *) dtk_rec[DTK_DTKSTAT], "REJ") == 0)
        menu_status = subMenu_dtk_status_REJ ;
    else if (strcmp( (char *) dtk_rec[DTK_DTKSTAT], "DEL") == 0)
        menu_status = subMenu_dtk_status_DEL ;
    else if (strcmp( (char *) dtk_rec[DTK_DTKSTAT], "SCH") == 0)
        menu_status = subMenu_dtk_status_SCH ;
    else if (strcmp( (char *) dtk_rec[DTK_DTKSTAT], "SUB") == 0)
        menu_status = subMenu_dtk_status_SUB ;
    else if (strcmp( (char *) dtk_rec[DTK_DTKSTAT], "INV") == 0)
        menu_status = subMenu_dtk_status_INV ;
    else if (strcmp( (char *) dtk_rec[DTK_DTKSTAT], "CON") == 0)
        menu_status = subMenu_dtk_status_CON ;
    else    /* not a status for a dtk that is managed herein */
    {
        (void) sprintf( display_string,
            "DTK has status '%s' which is\nnot managed by this program",
            (char *) dtk_rec[DTK_DTKSTAT] );
        popup_message( XmDIALOG_INFORMATION, "APS:INFORMATION",
            display_string, XtGrabNone ) ;
        cb_clear_dtkmanager_form( widget, NULL, NULL ) ;
        return ;
    }
    if (menu_status != NULL)
    {
        XtVaSetValues( optionMenu_dtk_status,
            XmNmenuHistory, menu_status,
            NULL ) ;
    }

#define SHOW_FIELD( column, cast, textField ) \
    (void) sprintf( display_string, \
        APS_PFMT( DTK, (column) ), cast dtk_rec[(column)] ) ; \
    XmTextFieldSetString( (textField), display_string ) ; 

    SHOW_FIELD( DTK_REV,              CAST_DTK_REV,              TF_REV )
    SHOW_FIELD( DTK_DTKID,            CAST_DTK_DTKID,            TF_DTKID )

    SHOW_FIELD( DTK_STRTTIME,         CAST_DTK_STRTTIME,
            TF_DTK_STRTTIME )
    SHOW_FIELD( DTK_STOPTIME,         CAST_DTK_STOPTIME,
            TF_DTK_STOPTIME )
    SHOW_FIELD( DTK_DTKDATE,          CAST_DTK_DTKDATE,          TF_DTKDATE )

    SHOW_FIELD( DTK_FADTKID,          CAST_DTK_FADTKID,          TF_FADTKID )
    SHOW_FIELD( DTK_DARID,            CAST_DTK_DARID,            TF_DTK_DARID )

    SHOW_FIELD( DTK_NOTES,            CAST_DTK_NOTES,            TF_NOTES )

    SHOW_FIELD( DTK_FA_SCHEDULE_LINK, CAST_DTK_FA_SCHEDULE_LINK,
            TF_FA_SCHEDULE_LINK )

    SHOW_FIELD( DTK_SITENAME,         CAST_DTK_SITENAME,
            TF_DTK_SITENAME )

    SHOW_FIELD( DTK_STRTLAT,          CAST_DTK_STRTLAT,          TF_STRTLAT )
    SHOW_FIELD( DTK_STOPLAT,          CAST_DTK_STOPLAT,          TF_STOPLAT )

    SHOW_FIELD( DTK_NRLAT1,           CAST_DTK_NRLAT1,           TF_NRLAT1 )
    SHOW_FIELD( DTK_NRLON1,           CAST_DTK_NRLON1,           TF_NRLON1 )
    SHOW_FIELD( DTK_NRLAT2,           CAST_DTK_NRLAT2,           TF_NRLAT2 )
    SHOW_FIELD( DTK_NRLON2,           CAST_DTK_NRLON2,           TF_NRLON2 )

    SHOW_FIELD( DTK_FARLAT1,          CAST_DTK_FARLAT1,          TF_FARLAT1 )
    SHOW_FIELD( DTK_FARLON1,          CAST_DTK_FARLON1,          TF_FARLON1 )
    SHOW_FIELD( DTK_FARLAT2,          CAST_DTK_FARLAT2,          TF_FARLAT2 )
    SHOW_FIELD( DTK_FARLON2,          CAST_DTK_FARLON2,          TF_FARLON2 )

#undef SHOW_FIELD

    if (update_direction_status( widget, CAST_DTK_ASCDSC dtk_rec[DTK_ASCDSC] )
        == ERROR)
    {
        (void) sprintf( display_string, 
            "ERROR: Invalid ascdsc value: '%c'\n in a DTK record",
            CAST_DTK_ASCDSC dtk_rec[DTK_ASCDSC] ) ;
        popup_message( XmDIALOG_ERROR, "APS:DB ERROR", 
            display_string, XtGrabNone ) ;
        return ;
    }

    satname = get_satellite_name( (char *) dtk_rec[DTK_SAT] ) ;
    if (satname == NULL)
    {
        (void) sprintf( display_string,
            "Satellite \"%s\" is not\n    in internal sat_name table",
            (char *) dtk_rec[DTK_SAT] ) ;
        gui_aps_internal_error( XmDIALOG_ERROR,
            __FILE__, __LINE__, display_string ) ;
        return ;
    }
    else if ((sat_button = XtNameToWidget( subMenu_sat, satname )) == NULL)
    {
        (void) sprintf( display_string,
            "Satellite \"%s\" is not\n    in satsensor relation",
            (char *) dtk_rec[DTK_SAT] ) ;
        popup_message( XmDIALOG_ERROR, "APS:DB ERROR", 
            display_string, XtGrabNone ) ;
        return ;
    }
    else
    {
        XtCallActionProc( sat_button, "ArmAndActivate", NULL, NULL, 0 );
        if (set_satellite_menu(
            optionMenu_sat, subMenu_sat, (char *) dtk_rec[DTK_SAT], 
            optionMenu_sensor, subMenu_sensor, (char *) dtk_rec[DTK_SENSOR],
            FALSE ) != SATMENU_OK )
        {
            /* display_string is set in set_satellite_menu() */
            popup_message( XmDIALOG_ERROR, "APS:DB ERROR", 
                display_string, XtGrabNone ) ;
            return ;
        }
    }

    /* update the station and antenna id option menu */
    stn_button = XtNameToWidget( subMenu_dtkm_stnid,
        CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID] );
    XtCallActionProc( stn_button, "ArmAndActivate", NULL, NULL, 0 );
    set_stationid_menu( optionMenu_dtkm_station_id, subMenu_dtkm_stnid,
        CAST_DTK_STATION_ID dtk_rec[DTK_STATION_ID] ) ;
    (void)sprintf( antenna, "%d", CAST_DTK_ANTENNA_ID dtk_rec[DTK_ANTENNA_ID] );
    set_antenna_menu( optionMenu_antenna, subMenu_antenna, antenna );

    /*
    -- set the activity and fa tape toggle buttons 
    -- the actid field is overloaded (unfortunately) as follows
    -- XXXYYY where XXX is the activity and YYY is the fa tapes
    -- to be mounted when recording the data
    -- use a strncpy to get each piece of data
    */
    (void) strncpy( activity, (char *) dtk_rec[DTK_ACTID], 3 ) ;
    (void) strncpy( fa_tapes, (char *) dtk_rec[DTK_ACTID]+3, 3 ) ;
    activity[3] = 0 ;
    fa_tapes[3] = 0 ;

    /* first set the appropiate activity toggle */

    if (strncmp( activity, DTKM_SENSOR_REALTIME_DOWNLINK_CODE,
        strlen( DTKM_SENSOR_REALTIME_DOWNLINK_CODE ) ) == 0)
    {
        XmToggleButtonSetState( Activity_Downlink, True, True ) ;
    }

    else if (strncmp( activity, DTKM_ACTID_REALTIME_OBSERVATION_CODE,
        strlen( DTKM_ACTID_REALTIME_OBSERVATION_CODE ) ) == 0)
    {
        XmToggleButtonSetState( Activity_Observe, True, True ) ;
    }

    else if (strcmp( activity, DTKM_SENSOR_TAPEDUMP_DOWNLINK_CODE ) == 0)
        XmToggleButtonSetState( Activity_Dump, True, True ) ;

    else if (strcmp( activity, DTKM_ACTID_RECORDING_OBSERVATION_CODE ) == 0)
        XmToggleButtonSetState( Activity_Record, True, True ) ;

    else
    {
        (void) sprintf( display_string, 
            "ERROR: invalid ACTID field: %s\n in a DTK record", activity ) ;
        popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string,
            XtGrabNone ) ;
        return ;
    }

    /* now set the fa tapes toggle */

    /* first reset all the toggles to off (false) */
    XmToggleButtonSetState( FATape_ASF, False, True ) ;
    XmToggleButtonSetState( FATape_ESA, False, True ) ;
    XmToggleButtonSetState( FATape_NASDA, False, True ) ;
    XmToggleButtonSetState( FATape_CSA, False, True ) ;

    if (strcmp( fa_tapes, FA_TapeCode_ASF ) == 0
    ||  strcmp( fa_tapes, FA_TapeCode_CSAASF ) == 0
    ||  strcmp( fa_tapes, FA_TapeCode_ESAASF ) == 0
    ||  strcmp( fa_tapes, FA_TapeCode_NASDAASF ) == 0)
        XmToggleButtonSetState( FATape_ASF, True, True ) ;

    if (strcmp( fa_tapes, FA_TapeCode_ESA ) == 0
    ||  strcmp( fa_tapes, FA_TapeCode_ESAASF ) == 0)
        XmToggleButtonSetState( FATape_ESA, True, True ) ;
    
    if (strcmp( fa_tapes, FA_TapeCode_NASDA ) == 0
    ||  strcmp( fa_tapes, FA_TapeCode_NASDAASF ) == 0)
        XmToggleButtonSetState( FATape_NASDA, True, True ) ;

    if (strcmp( fa_tapes, FA_TapeCode_CSA ) == 0
    ||  strcmp( fa_tapes, FA_TapeCode_CSAASF ) == 0)
        XmToggleButtonSetState( FATape_CSA, True, True ) ;

    /* set the Downlink Channel option menu */
    /* non J1 is default 00 */

    if (strcmp( (char *) dtk_rec[DTK_TRANSID], "00" ) == 0)
        menu_status = subMenu_J1_DLink_00 ;
    else if (strcmp( (char *) dtk_rec[DTK_TRANSID], "F1" ) == 0)
        menu_status = subMenu_J1_DLink_F1 ;
    else if (strcmp( (char *) dtk_rec[DTK_TRANSID], "F2" ) == 0)
        menu_status = subMenu_J1_DLink_F2 ;
    else if (strcmp( (char *) dtk_rec[DTK_TRANSID], "CB" ) == 0)
        menu_status = subMenu_J1_DLink_CB ;
    else if (strcmp( (char *) dtk_rec[DTK_TRANSID], "F3" ) == 0)
        menu_status = subMenu_R1_DLink_F3 ;
    else if (strcmp( (char *) dtk_rec[DTK_TRANSID], "F4" ) == 0)
        menu_status = subMenu_R1_DLink_F4 ;
    else if (strcmp( (char *) dtk_rec[DTK_TRANSID], "F5" ) == 0)
        menu_status = subMenu_A1_DLink_F5 ;
    else if (strcmp( (char *) dtk_rec[DTK_TRANSID], "F6" ) == 0)
        menu_status = subMenu_A1_DLink_F6 ;
    else if (strcmp( (char *) dtk_rec[DTK_TRANSID], "F7" ) == 0)
        menu_status = subMenu_A1_DLink_F7 ;
    else
    {
        (void) sprintf( display_string, 
            "Invalid DownLink Channel: %s\nUsing Default Channel", 
            (char *) dtk_rec[DTK_TRANSID]) ;
        popup_message( XmDIALOG_WARNING, "APS:WARNING", display_string,
            XtGrabNone ) ;
        menu_status = subMenu_J1_DLink_00 ;
    }

    XtVaSetValues( optionMenu_J1_DLinkChannel,
        XmNmenuHistory, menu_status,
        NULL ) ;

    /* set the quicklook flag */
    if ( CAST_DTK_SCIENCE_QUICKLOOK dtk_rec[DTK_SCIENCE_QUICKLOOK] == 'Y' )
    {
        XtVaSetValues( label_SciQuicklook,
            XtVaTypedArg, XmNlabelString, XmRString, "Yes", strlen( "Yes" ) + 1,
            NULL );
    }
    else
    {
        XtVaSetValues( label_SciQuicklook,
            XtVaTypedArg, XmNlabelString, XmRString, "No", strlen( "No" ) + 1,
            NULL );
    }

    if ( CAST_DTK_PLANNER_QUICKLOOK dtk_rec[DTK_PLANNER_QUICKLOOK] == 'Y' )
    {
      	XtVaSetValues( optionMenu_PlanQuicklook, 
		       XmNmenuHistory, subMenu_PlanQuicklook_yes,
		       NULL ) ;
    }
    else
    {
    	XtVaSetValues( optionMenu_PlanQuicklook, 
		       XmNmenuHistory, subMenu_PlanQuicklook_no,
		       NULL ) ;
    }

END_CONTEXT

    return;
}


/*==============================================================================
Function:       cb_rtobservation_toggle

Description:    Resets key fields when the user presses the Realtime 
                Observation toggle button.

Parameters:     Standard X params.

Returns:        nothing

Creator:        Philip Yurchuk

Creation Date:  4/5/97

Notes:      
==============================================================================*/
void cb_rtobservation_toggle
( Widget widget, XtPointer client_data_UNUSED, XmListCallbackStruct *cbs)
{

BEGIN_CONTEXT( widget ) 

    /* set the antenna menu to "0" */
    set_antenna_menu( optionMenu_antenna, subMenu_antenna, "0" );

    /* set the trans id to "00" */
    XtVaSetValues( optionMenu_J1_DLinkChannel,
                  XmNmenuHistory, subMenu_J1_DLink_00,
                  NULL ) ;

END_CONTEXT

    return;
}


/*==============================================================================
Function:       cb_clear_dtkmanager_form

Description:    This is the CLEAR pushbutton callback.  It clears all the
                fields in the dtk manager form (ie, all the fill-in fields
                that are used in creating a dtk).

Parameters:     Standard X Callback parameters

Returns:        None    

Creator:        Teresa McKillop

Creation Date:  02/16/96

Notes:      
==============================================================================*/
/* ARGSUSED1 */
void
cb_clear_dtkmanager_form( Widget widget, XtPointer client_data_UNUSED,
    XmListCallbackStruct *cbs)
{
BEGIN_CONTEXT( widget )

    SATELLITE   *sat_rec ;
    cursor      sat_ptr ;

    sat_rec = FIRST( satellites, sat_ptr ) ;
    XtCallActionProc( XtNameToWidget( subMenu_sat, sat_rec->satname ),
        "ArmAndActivate", NULL, NULL, 0 );

    XmTextFieldSetString( TF_REV,               "0" ) ;
    XmTextFieldSetString( TF_DTKID,             "0" ) ;
    XmTextFieldSetString( TF_DTK_DARID,         EMPTY_STR ) ;
    XmTextFieldSetString( TF_FADTKID,           EMPTY_STR ) ;
    XmTextFieldSetString( TF_DTK_STRTTIME,      EMPTY_STR ) ;
    XmTextFieldSetString( TF_DTK_STOPTIME,      EMPTY_STR ) ;
    XmTextFieldSetString( TF_FA_SCHEDULE_LINK,  EMPTY_STR ) ;
    XmTextFieldSetString( TF_DTK_SITENAME,      EMPTY_STR ) ;
    XmTextFieldSetString( TF_NOTES,             EMPTY_STR ) ;
    XmTextFieldSetString( TF_STRTLAT,           EMPTY_STR ) ;
    XmTextFieldSetString( TF_STOPLAT,           EMPTY_STR ) ;
    XmTextFieldSetString( TF_NRLAT1,                EMPTY_STR ) ;
    XmTextFieldSetString( TF_NRLON1,                EMPTY_STR ) ;
    XmTextFieldSetString( TF_NRLAT2,                EMPTY_STR ) ; 
    XmTextFieldSetString( TF_NRLON2,                EMPTY_STR ) ;
    XmTextFieldSetString( TF_FARLAT1,           EMPTY_STR ) ;
    XmTextFieldSetString( TF_FARLON1,           EMPTY_STR ) ;
    XmTextFieldSetString( TF_FARLAT2,           EMPTY_STR ) ;
    XmTextFieldSetString( TF_FARLON2,           EMPTY_STR ) ;

    XtVaSetValues( optionMenu_dtk_status,
        XmNmenuHistory, subMenu_dtk_status_PLN,
        NULL ) ;

    /* reset all the fa tape toggles to off (false) */
    XmToggleButtonSetState( FATape_ASF,     False, True ) ;
    XmToggleButtonSetState( FATape_ESA,     False, True ) ;
    XmToggleButtonSetState( FATape_NASDA,   False, True ) ;
    XmToggleButtonSetState( FATape_CSA,     False, True ) ;

END_CONTEXT
    return;
}



/*==============================================================================
Function:       cb_update_dtkmanager_form

Description:    This is the BrowseSelection Callback for the DTK list.
                As each DTK is browsed in the DTK window, this callback
                is executed

Parameters:     Standard X Callback parameters

Returns:        None    

Creator:        Ron Green

Creation Date:  09/dd/1994
Notes:      
==============================================================================*/
void
cb_update_dtkmanager_form( Widget widget, XtPointer client_data_UNUSED,
    XmListCallbackStruct *cbs)
{
BEGIN_CONTEXT( widget )
    DB_RECORD **dtk_rec ;

    dtk_rec = (DB_RECORD **) db_nth_record( dtks, cbs->item_position ) ;

    display_dtkmanager_fields( widget, dtk_rec );
    
    /* turn on the edit and delete buttons */
    XtSetSensitive( pushButton_EditDTK, True ) ;
    XtSetSensitive( pushButton_DeleteDTK, True ) ;
END_CONTEXT
}



/*==============================================================================
Function:       t_cb_planning_perm_timeout

Description:    For the timed in-planning permission, the permission
                "expires" after a certain amount of time.  The Planner
                is warned when there is one minute left before this
                timeout.  This routine handles turning the timeout on
                and off and both the warning and the expiration.

Parameters:     (XtTimerCallbackProc parameters)
                client_data - DTK_PERM_T_O_ON, turn on the timeout
                            - DTK_PERM_T_O_WARN, give the warning
                            - DTK_PERM_T_O_EXPIRE, expire the permission
                            - DTK_PERM_T_O_OFF, turn the timer off

Returns:        

Creator:        Teresa McKillop

Creation Date:  12/12/96

Notes:      
==============================================================================*/
/* ARGSUSED1 */
static void
t_cb_planning_perm_timeout( XtPointer client_data, XtIntervalId *id_UNUSED )
{
    static int              timeoutValue = 0 ;  /*minutes before perm. expires*/
    static XtIntervalId     timeoutId = 0 ; /* set to the id when timer is on */
    char                    asfTime[ASF_TIME_STR_LENGTH+1] ;

    /* do this only once: get the timeout value (in minutes) */
    if (!timeoutValue)
    {
        char    *tmpStr ;

        if ((tmpStr = getenv( APS_MU_T_O_ENVVAR )) != NULL)
            timeoutValue = atoi( tmpStr ) ;

        /* the timeout value must be greater than the warning time */
        if (timeoutValue <= MU_TIMEOUT_WARNING) /* use the constant */
        {
            timeoutValue = (MU_DEFAULT_TIMEOUT < MU_TIMEOUT_WARNING + 1)
                    ? MU_TIMEOUT_WARNING + 1
                    : MU_DEFAULT_TIMEOUT ;
        }
    }

    switch( (int) client_data )
    {
    case DTK_PERM_T_O_WARN :
        (void) tc_systime2asf( asfTime ) ;  /* always returns TRUE */

        /* set the timer for the remaining time */
        timeoutId = XtAppAddTimeOut( UxAppContext,
                MU_TIMEOUT_WARNING * MINUTE_TO_MILLISECS,
                (XtTimerCallbackProc) t_cb_planning_perm_timeout,
                (XtPointer) DTK_PERM_T_O_EXPIRE ) ;

        /* see if the user wants to restart the timeout */
        (void) sprintf( display_string, "Permission to create/edit will expire in %d minute(s).\nStart the timer over?\n\n(%s)", MU_TIMEOUT_WARNING, asfTime ) ;
        if (AskUser( XtParent( DTKManager ), display_string, YES ) == YES)
        {
            if (!timeoutId) /* already expired, no creating/editing in-session*/
            {
                (void) sprintf( display_string,
                    "Timer previously expired.\nRESTART the create/edit datatake." ) ;
                popup_message( XmDIALOG_WARNING, "APS:WARNING", 
                    display_string, XtGrabNone ) ;
                break ;
            }

            /* restart the timer */
            t_cb_planning_perm_timeout( (XtPointer) DTK_PERM_T_O_ON, NULL ) ;
        }
        break ;
    case DTK_PERM_T_O_EXPIRE :
        timeoutId = 0 ;
        (void) sprintf( display_string, "Time has expired for creating/editing.\n\n*** CANCELLING *** THE CREATE/EDIT" ) ;
        popup_message( XmDIALOG_WARNING, "APS:WARNING", 
            display_string, XtGrabNone ) ;

        /* includes freeing the permission and turning off the timer */
        cb_set_dtkmanager_editability( pushButton_CancelDTKChanges,
                (XtPointer) DTK_RESET, NULL ) ;
        break ;
    case DTK_PERM_T_O_OFF :
        if (timeoutId)
        {
            XtRemoveTimeOut( timeoutId ) ;
            timeoutId   = 0 ;
        }
        break ;
    default :   /* DTK_PERM_T_O_ON: turn ON the timer */
        if (timeoutId)
        {
            /* turn off the timer (and reset the variables) */
            t_cb_planning_perm_timeout( (XtPointer) DTK_PERM_T_O_OFF, NULL ) ;
        }
        /* set timeout to "warning" time: timeout value - MU_TIMEOUT_WARNING */
        timeoutId = XtAppAddTimeOut( UxAppContext,
            (timeoutValue - MU_TIMEOUT_WARNING) * MINUTE_TO_MILLISECS,
            (XtTimerCallbackProc) t_cb_planning_perm_timeout,
            (XtPointer) DTK_PERM_T_O_WARN ) ;
        break ;
    }

    return ;
}


/*==============================================================================
Function:       free_timed_planning_permission

Description:    Turns off the timer and frees the planning permission.

Parameters:     None

Returns:        None

Creator:        Teresa McKillop

Creation Date:  12/17/96

Notes:      
==============================================================================*/
static void
free_timed_planning_permission()
{
    /* turn off the timer (and reset the variables) */
    t_cb_planning_perm_timeout( (XtPointer) DTK_PERM_T_O_OFF, NULL ) ;

    /* release the in-planning permission: error msg done in the function */
    (void) gui_free_permission( dtk_timed_pln_perm,
            MU_CREATE_EDIT_OR_DELETE_DTK, MU_PLANNING_ACTIVITY_TYPE ) ;

    dtk_timed_pln_perm = 0 ;    /* reset */

    return ;
}


/*==============================================================================
Function:       get_timed_planning_permission

Description:    Tries to get the in-planning permission and if successful
                turns on the timeout.

Parameters:     strttime    - start time for planning time bracket
                stoptime    - stop time for planning time bracket
                stationid   - station id

Returns:        return value from gui_get_planning_permission():
                    > 0, the permission id
                    = 0, permission not granted; is blocked
                    < 0, an mu library error occurred

Creator:        Teresa McKillop

Creation Date:  12/12/96

Notes:      
==============================================================================*/
static int
get_timed_planning_permission( char *strttime, char *stoptime, char *stationid )
{
    int         retval ;

    if ((retval = gui_get_planning_permission(
            MU_CREATE_EDIT_OR_DELETE_DTK, strttime, stoptime, stationid )) > 0)
    {
        /* if successful, save the perm id */
        dtk_timed_pln_perm = retval ;

        /* turn on the timeout */
        t_cb_planning_perm_timeout( (XtPointer) DTK_PERM_T_O_ON, NULL ) ;
    }

    return (retval) ;
}


/*==============================================================================
Function:       cb_set_dtkmanager_editability

Description:    Set the dtkmanager form's editability so that items that can
                be modified are set so that they can be.

Parameters:     Standard X Callback parameters

Returns:        None    

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:      
==============================================================================*/
/* ARGSUSED2 */
void
cb_set_dtkmanager_editability( Widget widget, XtPointer client_data,
    XtPointer cbs_UNUSED )
{
BEGIN_CONTEXT( widget )

    Boolean editability ;
    Boolean sensitivity ;

    int     edit_mode ;
    int     *pos_list ;
    int     pos_cnt ;

    char    *sat ;

    String  oldLabel ;

    edit_mode = (int) client_data ;

    if (edit_mode == DTK_EDIT
    ||  edit_mode == DTK_CREATE)    
        editability = True ;
    else /* DTK_RESET */
        editability = False ;
        
    if (edit_mode == DTK_EDIT)
    {
        char    *stationid ;
        char    strttime[ASF_TIME_STR_LENGTH+1] ;
        char    stoptime[ASF_TIME_STR_LENGTH+1] ;
        char    *tmpStr ;
        char    *satcode ;
        char    *sensor ;
        char    *rev_string ;
        char    *dtkid ;
        int     rev ;
        int     retCode ;
        int     nrecs ;
        double  dummy_et1 ;
        double  dummy_et2 ;
        DB_RECORD   **dtk_rec ;
        llist       *dtk_list = NULL ;
        cursor      dtk_list_ptr ;

        /* 
        -- Before we do anything, check to see if the datatake still exists.
        -- It could have been deleted by another user.
        */
        
        /* Get information which will form the where clause */

        sat         = gui_optionMenu_string( optionMenu_sat ) ;
        /*
        -- NOTE: get_satellite_code() won't return NULL because of
        -- how the satellites list and sat option menus are created
        */
        satcode     = get_satellite_code( sat ) ;
        sensor      = gui_optionMenu_string( optionMenu_sensor ) ;
        rev_string  = gui_TF_string( TF_REV ) ;
        dtkid       = gui_TF_string( TF_DTKID ) ;

        /* create a where clause that describes this dtk */

        (void) sprintf( where_clause, 
            "\nwhere %s = '%s' and %s = '%s'\nand %s = %s and %s = %s",
            APS_COL( DTK, DTK_SAT ), satcode,
            APS_COL( DTK, DTK_SENSOR ), sensor,
            APS_COL( DTK, DTK_REV ), rev_string,
            APS_COL( DTK, DTK_DTKID ), dtkid) ;
        XtFree( sat ) ; 
        XtFree( sensor ) ; 
        XtFree( rev_string ) ; 
        XtFree( dtkid ) ;

        /* Get the number of records that fit the clause (should equal one) */

        nrecs = db_num_records(APS_dbproc, APS_TABLE(DTK), where_clause ) ;

        if (nrecs < 0)
        {
            /* An error has occurred - inform the user */

            (void) sprintf( display_string, "An error occurred while attempting to verify the existence of the \nselected datatake.") ;
            popup_message( XmDIALOG_ERROR, "APS:ERROR", 
                display_string, XtGrabNone ) ;

            /* refresh the list of datatakes */
            cb_show_dtk_records( widget, (XtPointer) scrolledList_DTKS, NULL ) ;
            
            return ;
        }

        if (nrecs == 0 )
        {
            /* The datatake no longer exists - inform the user */

            (void) sprintf( display_string, "The datatake no longer exists.  It was most likely deleted by \nanother user.") ;
            popup_message( XmDIALOG_ERROR, "APS:ERROR", 
                display_string, XtGrabNone ) ;

            /* refresh the list of datatakes */
            cb_show_dtk_records( widget, (XtPointer) scrolledList_DTKS, NULL ) ;
            
            return ;
        }

        if (nrecs > 1 )
        {
            /* There are multiple records - error */

            (void) sprintf( display_string, "While attempting to verify the existence of the selected datatake, \nmultiple records were found.  There is an error in the database.") ;
            popup_message( XmDIALOG_ERROR, "APS:ERROR", 
                display_string, XtGrabNone ) ;

            /* refresh the list of datatakes */
            cb_show_dtk_records( widget, (XtPointer) scrolledList_DTKS, NULL ) ;

            return ;
        }

        /* 
        -- We want to retrieve the record and redisplay it in case someone has
        -- already edited it.
        */

        /* get the record */

        dtk_list = db_get_records(DB_SYBINT_USE_APS_READER_DBPROC, 
                                  APS_TABLE(DTK),
                                  where_clause, NULL, 
                                  APS_CDEFS(DTK), ALL_COLS) ;
        if (dtk_list == NULL)
        {
            /* An error has occurred - inform the user */

            (void) sprintf( display_string, "An error occurred while attempting\n to verify the existence of the selected datatake.") ;
            popup_message( XmDIALOG_ERROR, "APS:ERROR", 
                display_string, XtGrabNone ) ;

            return ;
        }

        dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;

        /* redisplay the record in the form */

        display_dtkmanager_fields( widget, dtk_rec ) ;

        /* 
        -- Now that we have verified the existence of the datatake, we may edit
        -- it.
        */

        /* get in-planning permission: Note use rev begin and end time */
        tmpStr  = gui_optionMenu_string( optionMenu_sat ) ;
        sat     = get_satellite_code( tmpStr ) ;
        XtFree( tmpStr ) ;
        tmpStr  = gui_TF_string( TF_REV ) ;
        rev     = atoi( tmpStr ) ;
        XtFree( tmpStr ) ;
        sscvrev2time( APS_dbproc, sat, &rev, &dummy_et1, strttime,
                &dummy_et2, stoptime, &retCode ) ;
        if (retCode)
        {
            (void) sprintf( display_string, "REV (%d) is NOT in a phase\n    Data-take should be deleted or (re)created", rev ) ;
            popup_message( XmDIALOG_ERROR, "APS:ERROR", 
                display_string, XtGrabNone ) ;

            return ;
        }

        /* check if changing a planned or scheduled dtk */
        if (!XmListGetSelectedPos( scrolledList_DTKS, &pos_list, &pos_cnt ))
        {
            (void) sprintf( display_string, "Need to Select a Data Take" ) ;
            popup_message( XmDIALOG_ERROR, "APS:ERROR", 
                display_string, XtGrabNone ) ;
            XtFree( (char *) pos_list ) ;
            return ;
        }

        /* get and save the original record in a global */
        orig_dtk_rec = db_nth_record( dtks, pos_list[0] ) ;
        XtFree( (char *) pos_list ) ;

        if (strcmp( (char *) orig_dtk_rec[DTK_DTKSTAT], "PLN" ) == 0
        ||  strcmp( (char *) orig_dtk_rec[DTK_DTKSTAT], "SCH" ) == 0)
        {
            (void) sprintf( question, 
                "You are about to edit a DTK\nwhich has status '%s'\n\nContinue Edit?",
                CAST_DTK_DTKSTAT orig_dtk_rec[DTK_DTKSTAT] ) ;

            if (AskUser( XtParent( widget ), question, NO ) == NO)
                return ;
        }

        stationid   = gui_optionMenu_string( optionMenu_dtkm_station_id ) ;
        if (get_timed_planning_permission( strttime, stoptime, stationid ) <= 0)
        {
            /* Couldn't get permission, message already popped up */
            XtFree( stationid ) ;
            return ;
        }

        XtFree( stationid ) ;

        /* set the labels on the SaveButton and the CancelButton */

        /* free current label memory */
        XtVaGetValues( pushButton_SaveDTKChanges,
                XmNlabelString, &oldLabel,
                NULL ) ;
        XtFree( oldLabel ) ;
        XtVaSetValues( pushButton_SaveDTKChanges,
                RES_CONVERT( XmNlabelString, "SAVE\nEDIT" ),
                NULL );
        XtVaSetValues( pushButton_SaveDTKChanges,
                XmNwidth, 90,
                XmNheight, 40,
                NULL );
        /* free current label memory */
        XtVaGetValues( pushButton_CancelDTKChanges,
                XmNlabelString, &oldLabel,
                NULL ) ;
        XtFree( oldLabel ) ;
        XtVaSetValues( pushButton_CancelDTKChanges,
                XmNwidth, 90,
                XmNheight, 40,
                RES_CONVERT( XmNlabelString, "CANCEL\nEDIT" ),
                NULL );

        /* remove any currently installed cb's for the SaveButton */
        XtRemoveAllCallbacks(
            pushButton_SaveDTKChanges, XmNactivateCallback) ;

        /* now install the cb_save_dtk cb passing the DTK_EDIT flag */
        XtAddCallback( pushButton_SaveDTKChanges, XmNactivateCallback, 
            cb_save_dtk_changes, (XtPointer) DTK_EDIT ) ;


        /*******************************************************************
        *                                                                  *
        *   ALLOW SENSOR MODE EDITING.  ( Radarsat sensor = S??? only )    *
        *                                                                  *
        *******************************************************************/
        sat         = gui_optionMenu_string( optionMenu_sat ) ;
        satcode     = get_satellite_code( sat ) ;
        sensor      = gui_optionMenu_string( optionMenu_sensor ) ;
        if( strcmp( satcode, "R1" ) == 0  &&  *sensor == 'S' )
        {
            /* 
            -- if sat = radarsat and sensor = "S??" 
            -- then allow the sensor mode to be editible.  
            -- this means that the sensor can be changed 
            -- from S?? to any other S??.  
            -- make the menu sensitive, and allow only SAR 
            -- mode choices.  
            */
            XtSetSensitive( optionMenu_sensor, editability ) ;
            turn_off_non_sar_sensors(widget) ;
        }
        XtFree( sat ) ; 
        XtFree( sensor ) ; 
        /*   (End of ALLOW SENSOR MODE EDITING.)    */



    }
    else if (edit_mode == DTK_RESET) /* canceling changes */
    {
        /* if set, free the in-planning permission: canceling EDIT */
        if (dtk_timed_pln_perm)
            free_timed_planning_permission() ;

        /* clear out the possibly set original dtk_rec */ 
        orig_dtk_rec = NULL;

        /* 
        -- if the cb is a result of a cancel of dtk changes make sure
        -- the original values are replaced via the XmListSelect cb
        -- we do this by first updating the scrolled list, then selecting
        -- the original dtk to update the form.
        */
        if (XmListGetSelectedPos( scrolledList_DTKS, &pos_list, &pos_cnt ))
        {
            cb_show_dtk_records( widget, (XtPointer) scrolledList_DTKS, NULL ) ;

            /* 
            -- once selected, True will force an update of the screen 
            -- due to the List Selection Callback
            */
            XmListSelectPos( scrolledList_DTKS, pos_list[0], True ) ;
            XtFree( (char *) pos_list ) ;
        }
    }
    else if (edit_mode == DTK_CREATE)
    {
        char *satcode ;

        sat = gui_optionMenu_string( optionMenu_sat ) ;
        /*
        -- NOTE: get_satellite_code() won't return NULL because of
        -- how the satellites list and sat option menus are created
        */
        satcode = get_satellite_code( sat ) ;

        /* clear out the possibly set original dtk_rec */ 
        orig_dtk_rec = NULL;

        /* set the sat/sensor menus appropiately */
        (void) set_satellite_menu( optionMenu_sat, subMenu_sat, satcode,
            NULL, NULL, NULL, FALSE ) ; /* gotten from option menu, can't err */

        XtFree( sat ) ;

        /* set the labels on the SaveButton and the CancelButton */

        /* free current label memory */
        XtVaGetValues( pushButton_SaveDTKChanges,
                XmNlabelString, &oldLabel,
                NULL ) ;
        XtFree( oldLabel ) ;
        XtVaSetValues( pushButton_SaveDTKChanges,
                RES_CONVERT( XmNlabelString, "SAVE\nCREATE" ),
                NULL );
        XtVaSetValues( pushButton_SaveDTKChanges,
                XmNwidth, 90,
                XmNheight, 40,
                NULL );
        /* free current label memory */
        XtVaGetValues( pushButton_CancelDTKChanges,
                XmNlabelString, &oldLabel,
                NULL ) ;
        XtFree( oldLabel ) ;
        XtVaSetValues( pushButton_CancelDTKChanges,
                XmNwidth, 90,
                XmNheight, 40,
                RES_CONVERT( XmNlabelString, "CANCEL\nCREATE" ),
                NULL );
        /*
        ** Reset the dtkid to zero for newly created dtk;
        ** APS will automatically assign the next available dtkid.
        */
        XmTextFieldSetString( TF_DTKID, "0" ) ;
    }   

    /*
    -- when creating or resetting dtks, the following fields are affected
    -- NOTE: these fields do not apply to dtks being edited (DTK_EDIT
    -- and therefore we don't allow changes to take place
    -- Also take care to reset them if we're resetting the form
    */
    if (edit_mode == DTK_CREATE
    ||  edit_mode == DTK_RESET)
    {
        XtSetSensitive( optionMenu_sat, editability ) ;
        XtSetSensitive( optionMenu_sensor, editability ) ;
        gui_setEditable( TF_REV,   AG_TEXTFIELD, editability ) ;
        XtSetSensitive( optionMenu_dtkm_station_id, editability ) ;
        XtSetSensitive( optionMenu_antenna, editability ) ;
        XtSetSensitive( frameActivity, editability ) ;
    }

    gui_setEditable( TF_DTK_STRTTIME,     AG_TEXTFIELD, editability ) ;
    gui_setEditable( TF_DTK_STOPTIME,     AG_TEXTFIELD, editability ) ;

    gui_setEditable( TF_FADTKID,          AG_TEXTFIELD, editability ) ;
    gui_setEditable( TF_DTK_DARID,        AG_TEXTFIELD, editability ) ;
    gui_setEditable( TF_NOTES,            AG_TEXTFIELD, editability ) ;
    gui_setEditable( TF_FA_SCHEDULE_LINK, AG_TEXTFIELD, editability ) ;

    /* turn off/on the option menus/buttons/etc */
    XtSetSensitive( optionMenu_dtk_status, editability ) ;
    XtSetSensitive( optionMenu_J1_DLinkChannel, editability ) ;
    XtSetSensitive( optionMenu_PlanQuicklook, editability ) ;

    XtSetSensitive( scrolledWindow_FATapes, editability ) ;
    XtSetSensitive( pushButton_GetCvrgPoints, editability ) ;

    /*
    -- based on the operation (resetting/editing)
    -- turn off/on (unmanage/manage)  buttons that aren't needed
    -- set the sensitivity of other menu items
    */
    
    if (edit_mode == DTK_RESET)
    {
        XtUnmanageChild( pushButton_ClearDTKForm ) ;
        XtUnmanageChild( pushButton_SaveDTKChanges ) ;
        XtUnmanageChild( pushButton_CancelDTKChanges ) ;
        sensitivity = TRUE ;
    }
    else  /* we're making changes */
    {
        if (edit_mode == DTK_CREATE)
            XtManageChild( pushButton_ClearDTKForm ) ;
        XtManageChild( pushButton_SaveDTKChanges ) ;
        XtManageChild( pushButton_CancelDTKChanges ) ;
        sensitivity = FALSE ;
    }

    XtSetSensitive( scrolledList_DTKS,    sensitivity ) ;
    XtSetSensitive( menuBar_DTK_FILE,     sensitivity ) ;
    XtSetSensitive( pushButton_SearchDTK, sensitivity ) ;
    XtSetSensitive( pushButton_SortDTK,   sensitivity ) ;
    XtSetSensitive( pushButton_CreateDTK, sensitivity ) ;

    /* 
    -- if no dtk is selected, the Edit/Delete buttons should be off
    -- XmListGetSelected will return false if this is the case, skip this
    -- code, and use the returned value (sensitivity) to set the EDIT/DEL
    -- buttons, after this block
    -- [NOTE: for DTK_EDIT there IS a selected dtk or would have
    -- told user to select one and error returned, above]
    */
    if ((sensitivity =
        XmListGetSelectedPos( scrolledList_DTKS, &pos_list, &pos_cnt )) == TRUE)
    {
        /* free the pos_list */
        XtFree( (char *) pos_list ) ;

        /*
        -- a dtk is selected....
        -- if RESET mode, the edit/delete buttons should be on
        -- if we are EDIT/CREATE mode , the edit/delete buttons should be off
        */
        if (edit_mode == DTK_RESET)
            sensitivity = TRUE ;
        else
            sensitivity = FALSE ;
    }

    XtSetSensitive( pushButton_EditDTK,   sensitivity ) ;
    XtSetSensitive( pushButton_DeleteDTK, sensitivity ) ;
END_CONTEXT
}



/*==============================================================================
Function:       cb_get_cvrg_points

Description:    
    The activate callback for the GET CVRG POINTS pushbutton.
    Calls the get_cvg routine to determine the data points at start and
    stop time of a datatake

Parameters:     Standard X Callback parameters

Returns:        None    

Creator:        Ron Green

Creation Date:  01/24/1995

Notes:      
==============================================================================*/
/* ARGSUSED1 */
void
cb_get_cvrg_points( Widget widget, XtPointer client_data_UNUSED,
    XtPointer cbs_UNUSED )
{
BEGIN_CONTEXT( widget )

    float 
        f_strtlat, f_stoplat,
        f_nrlat1, f_nrlon1,
        f_farlat1, f_farlon1,
        f_nrlat2, f_nrlon2,
        f_farlat2, f_farlon2 ;
    char direction ;
    char strt_direction ;
    char stop_direction ;

    char *sat, *sensor, *rev ;
    char *strttime ;
    char *stoptime ;
    char *satcode ;

    int status ;

    /* 
    -- verify start/stop time is valid before we do anything so we can 
    -- make sure that we can actually get datapoints 
    */

    /* activate the start/stop time fields for error validation */
    XtCallActionProc( TF_DTK_STRTTIME, "activate", NULL, NULL, 0 ) ;
    if (ASF_datetime_err)
        return ;
 
    XtCallActionProc( TF_DTK_STOPTIME, "activate", NULL, NULL, 0 ) ;
    if (ASF_datetime_err)
        return ;

    strttime = gui_TF_string( TF_DTK_STRTTIME ) ;
    stoptime = gui_TF_string( TF_DTK_STOPTIME ) ;

    if (time_range_error( strttime, stoptime ) == TRUE)
    {
        /* time_range_error "pops up" an error message */
        XtFree( strttime ) ; XtFree( stoptime ) ;
        return ;
    }

    /* get and verify a valid rev before retrieving cvrg points */
    rev    = gui_TF_string( TF_REV ) ;
    if (*rev == STREND)
    {
        (void) sprintf( display_string, "No REV value given" ) ;
        popup_message( XmDIALOG_ERROR, "APS:ERROR", 
            display_string, XtGrabNone ) ;
        return ;
    }

    sat    = gui_optionMenu_string( optionMenu_sat ) ;
    sensor = gui_optionMenu_string( optionMenu_sensor ) ;
    satcode = get_satellite_code( sat ) ;

    /* now get the rest of the data needed for calculating points */

    status = get_cvg( APS_dbproc, satcode, sensor, atoi( rev ), strttime,
        &f_strtlat, &f_nrlat1, &f_nrlon1, &f_farlat1, &f_farlon1,
        &strt_direction ) ;
    if (status  != 0)
    {
        (void) sprintf( display_string, "No Coverage Points found for time:\n%s",
            strttime ) ;
        popup_message( XmDIALOG_ERROR, "APS:ERROR", 
            display_string, XtGrabNone ) ;
        XtFree( sat ) ;
        XtFree( sensor ) ;
        XtFree( strttime ) ;
        XtFree( stoptime ) ;
        return ;
    }

    status = get_cvg( APS_dbproc, satcode, sensor, atoi( rev ), stoptime,
        &f_stoplat, &f_nrlat2, &f_nrlon2, &f_farlat2, &f_farlon2,
        &stop_direction ) ;
    if (status  != 0)
    {
        (void) sprintf( display_string,
            "No Coverage Points found for time:\n%s", stoptime ) ;
        popup_message( XmDIALOG_ERROR, "APS:ERROR",
            display_string, XtGrabNone ) ;
        XtFree( sat ) ;
        XtFree( sensor ) ;
        XtFree( strttime ) ;
        XtFree( stoptime ) ;
        return ;
    }

    set_cvrg_fields( f_strtlat, f_stoplat, f_nrlat1, f_nrlon1,
        f_farlat1, f_farlon1, f_nrlat2, f_nrlon2, f_farlat2, f_farlon2 );

    /*
    -- check the direction of the dtk, it must be all asc
    -- or all dsc,  dtks change from asc to dsc when the 
    -- sat crosses the it's most northern point; a value of 'S'
    -- indicates a value of stationary, the point where the sat is 
    -- neither asc or dsc 
    */
    direction = 0 ;
    if (strt_direction != 'S')
        direction = strt_direction ;
    else
    {
        if (stop_direction == 'S') 
        {
            (void) sprintf( display_string,
                "Data Take would be both ASC and DSC\nreduce the DTK time\nso the DTK is all ASC or DSC" ) ;
            popup_message( XmDIALOG_ERROR, "APS:ERROR",
                display_string, XtGrabNone ) ;
        }
        else
            direction = stop_direction ;
    }

    if (direction)  
    {
        if (update_direction_status( widget, direction ) == ERROR)
        {
            (void) sprintf( display_string, 
                "Invalid ascdsc value: '%c'\n in code",
                direction ) ;
            gui_aps_internal_error( XmDIALOG_ERROR,
                __FILE__, __LINE__, display_string ) ;
        }
    }

    XtFree( sat ) ;
    XtFree( sensor ) ;
    XtFree( strttime ) ;
    XtFree( stoptime ) ;
END_CONTEXT
}


/*==============================================================================
Function:       form_bad_dtkstat_msg

Description:    Determines the reason why a dtkstat would return
                DTKM_ERROR_DTK_SHOULD_BE_DEL_REJ_INV_QUE_SUB_REQ_PLN_OR_SCH_STATUS
                or DTKM_ERROR_BAD_DTKSTAT_VALUE and formats an appropriate
                error message in msg.

Parameters:     dtk_rec - the dtk record with the erroneous dtkstat
                msg     - a buffer containing the formatted error message.

Returns:        none

Creator:        Teresa McKillop

Creation Date:  06/11/96

Notes:          Have to make sure the error message reflects the checks
                done in dtkm_process_dtk() and dtkm_check_dtkstat().
==============================================================================*/
static void
form_bad_dtkstat_msg( DB_RECORD **dtk_rec, char *msg )
{
    char *satcode = CAST_DTK_SAT dtk_rec[DTK_SAT] ;
    char *sat ;

    if (!(sat = get_satellite_name( satcode )))
        sat = satcode ;     /* Shouldn't happen; if does, use satcode */

    if (*satcode == 'A')    /* ADEOS satellite */
    {
        (void) sprintf( msg,
            "Invalid status for SAT %s\nValid Values: REJ, DEL, PLN, SCH, INV\n"
            , sat );
    }
    else    /* non-ADEOS satellite */
    {
        /* check for special case */
        if (dtkm_is_a_tape_dump( dtk_rec ) == TRUE)
        {
            (void) sprintf( msg,
                "Invalid status for SAT %s SENSOR %s\nValid Values: REJ, DEL, \
PLN, SCH\n",
                sat, CAST_DTK_SENSOR dtk_rec[DTK_SENSOR] );
        }
        else
        {
            (void) sprintf( msg,
                "Invalid status for SAT %s\nValid Values: REJ, DEL, QUE, SUB, \
PLN, SCH\n",
                sat );
        }
    }

    return ;
}


/*==============================================================================
Function:       form_bad_transid_msg

Description:    Determines the specific reason why a transaction id is bad
                and formats an appropriate error message in the msg buffer.

Parameters:     

Returns:        none

Creator:        Teresa McKillop

Creation Date:  12/13/95

Notes:          Have to make sure checking is the same as dtkm_check_transid(),
                but need more specific error information, so have to repeat
                the processing here.
==============================================================================*/
static void
form_bad_transid_msg( char *satcode, char *transid, int activity_mode,
        char *msg )
{
    if (strcmp( transid, "00" ) == 0)   /* error: mode is not record */
    {
        (void) sprintf( msg,
            "Invalid Downlink Channel: 00\nfor %s mode\n", transid );
    }
    /* transid is NOT "00" */
    else if (*satcode == 'E')   /* error: ESA must be "00" */
    {
        (void) sprintf( msg,
            "Invalid Downlink Channel: %s\nfor %s Satellite\nValid Value: 00\n",
             transid, satcode ) ;
    }

    if (*satcode == 'A')        /* error: bad ADEOS transid for r/t, p/b */
    {
        (void) sprintf( msg,
            "Invalid Downlink Channel: %s\nfor %s Satellite, %s Mode\nValid Values: F5, F6, or F7\n",
            transid, satcode,  activities[activity_mode] ) ;
    }
    else if (*satcode == 'J')   /* error: bad JERS transid for r/t, p/b */
    {
        (void) sprintf( msg,
            "Invalid Downlink Channel: %s\nfor %s Satellite, %s Mode\nValid Values: F1 or F2\n",
            transid, satcode, activities[activity_mode] ) ;
    }
    else                    /* error: bad RADARSAT transid for r/t, p/b */
    {
        if (activity_mode == XDOWNLINK)
        {
            (void) sprintf( msg,
                "Invalid Downlink Channel: %s\nfor %s Satellite, %s Mode\nValid Value: F3\n",
                transid, satcode, activities[activity_mode] ) ;
        }
        else
        {
            (void) sprintf( msg,
                "Invalid Downlink Channel: %s\nfor %s Satellite, %s Mode\nValid Value: F4\n",
                transid, satcode, activities[activity_mode] ) ;
        }
    }

    return;
}



/*==============================================================================
Function:       check_in_station_mask_status

Description:    Processes the non-ok status return from the check
                for the dtk in-station mask.

                If given start/stop times (i.e., overriding is allowed):

                    if can and user okays it, overrides the times
                    if times not overridden, asks if should continue
                        even though there are in-station mask problems

                Otherwise, formats the error message in display_string,
                unless stopFlag is returned with a value of TRUE, in which
                case there the contents of display_string are not a valid
                station mask error message.

Parameters:     reduced_strttime, reduced_stoptime: if both are not NULL,
                    allow overriding of dtk times

                stopFlag: TRUE if should stop processing when return to caller

Returns:        None    

Creator:        Teresa McKillop

Creation Date:  12/13/95

Notes:      
==============================================================================*/
static void
check_in_station_mask_status( Widget widget, int status, DB_RECORD **newdtk_rec,
    char *reduced_strttime, char *reduced_stoptime, int *stopFlag )
{
    char    *sat;
    char    tmpstr[APS_GUI_DISPLAY_STR_SIZE];

    *stopFlag       = FALSE;
    sat             = get_satellite_name( CAST_DTK_SAT newdtk_rec[DTK_SAT] );

    switch (status)
    {
        case DTKM_ERROR_DOWNLINK_DTK_NOT_ENTIRELY_WITHIN_STATION_PASS:
        case DTKM_DTK_HAS_TIME_IN_MASK:
            (void) sprintf( display_string,
                "SAT: %s is not completely in the mask during the DTK\n",
                sat );

            if (reduced_strttime == NULL || reduced_stoptime == NULL)
                break;

            /* see if can override the dtk start/stop times */
            if (strcmp( reduced_strttime,
                    CAST_DTK_STRTTIME newdtk_rec[DTK_STRTTIME] ) > 0 ||
                strcmp( reduced_stoptime,
                    CAST_DTK_STOPTIME newdtk_rec[DTK_STOPTIME] ) < 0)
            {
                (void) sprintf( tmpstr,
                    "Do you wish to modify the DTK start/stop times as follows:\n\nSTART: %s -> %s\n STOP: %s -> %s",
                    CAST_DTK_STRTTIME newdtk_rec[DTK_STRTTIME],
                    reduced_strttime,
                    CAST_DTK_STOPTIME newdtk_rec[DTK_STOPTIME],
                    reduced_stoptime ) ;
                (void) strcat( display_string, tmpstr );

                if (AskUser( widget, display_string, YES ) == YES)
                {
                    (void) memcpy( CAST_DTK_STRTTIME newdtk_rec[DTK_STRTTIME], 
                        reduced_strttime, strlen( reduced_strttime ) ) ;

                    (void) memcpy( CAST_DTK_STOPTIME newdtk_rec[DTK_STOPTIME], 
                        reduced_stoptime, strlen( reduced_stoptime ) ) ;

                    XmTextFieldSetString( TF_DTK_STRTTIME, reduced_strttime ) ;
                    XmTextFieldSetString( TF_DTK_STOPTIME, reduced_stoptime ) ;

                    *display_string = STREND;   /* empty it out */
                }
            }
            break;
        case DTKM_ERROR_NULL_DTK_PROPOSAL :
            gui_aps_internal_error( XmDIALOG_ERROR, __FILE__, __LINE__,
                "NULL DTK RECORD") ;
            *stopFlag = TRUE;
            return ;
        case DTKM_ERROR_DB_QUERY_FAILED:
            gui_aps_internal_error( XmDIALOG_ERROR, __FILE__, __LINE__,
                "Incorrect Query") ;
            *stopFlag = TRUE;
            return ;
        case DTKM_ERROR_BAD_STRTTIME_VALUE :
        case DTKM_ERROR_BAD_STOPTIME_VALUE :
            (void) sprintf( display_string,
                "Error in specified START/STOP times.\n" ) ;
            break ;
        case DTKM_ERROR_DOWNLINK_DTK_HAS_NO_TIME_IN_MASK :
            (void) sprintf( display_string,
                "The SAT/DTK has no time in the mask.\n" ) ;
            break ;
        case DTKM_ERROR_NO_MASKINOUT_RECS_FOUND_IN_REV    :
            (void) sprintf( display_string, 
                "The SAT/DTK has no time in mask during REV: %ld\n",
                CAST_DTK_REV newdtk_rec[DTK_REV] ) ;
            break ;
        case DTKM_ERROR_MASKINOUT_PROBLEM_PLEASE_RUN_aps_crt_nom_mask_FOR_DTK :
            (void) sprintf( display_string, 
                "aps_crt_nom_mask must be run\nfor the time period of the dtk.\n" ) ;
            break ;
        default:
            (void) sprintf( display_string,
                "Invalid STATUS: %d\nfrom dtkm_in_station_mask",
                status ) ;
            gui_aps_internal_error( XmDIALOG_ERROR, __FILE__, __LINE__,
                display_string) ;
            *stopFlag = TRUE;
            return ;
    }

    /* can't override, so return with the error string set */
    if (reduced_strttime == NULL || reduced_stoptime == NULL)
        return;

    if (*stopFlag == FALSE)
    {
        (void) sprintf( question,
            "Problems were noted with your Data Take\nand the nominal station mask\n\n%s\n\nDo you wish to continue save with current dtk?\n\n(Answer NO to continue editing)", 
                display_string );

        if (AskUser( widget, question, NO ) == NO)
            *stopFlag = TRUE;
    }

    return;
}   


/*==============================================================================
Function:       get_fa_tapes_from_form

Description:    gets the fa tape string based on the values set
                on the dtk form.

Parameters:     None

Returns:        the fa tape string
                EMPTY_STR if no fa tape values are set on the dtk form
                NULL if the values on the dtk form are invalid

Creator:        Teresa McKillop

Creation Date:  08/06/96

Notes:          
==============================================================================*/
static char *
get_fa_tapes_from_form()
{
    Boolean     asf_tapes;
    Boolean     esa_tapes;
    Boolean     nasda_tapes;
    Boolean     csa_tapes ; 
    char        *fa_tapes = EMPTY_STR;

    asf_tapes   = XmToggleButtonGetState( FATape_ASF ) ;
    esa_tapes   = XmToggleButtonGetState( FATape_ESA ) ;
    nasda_tapes = XmToggleButtonGetState( FATape_NASDA ) ;
    csa_tapes   = XmToggleButtonGetState( FATape_CSA ) ;

    if (asf_tapes && !esa_tapes && !nasda_tapes && !csa_tapes)
        fa_tapes = FA_TapeCode_ASF ;
    else if (!asf_tapes && esa_tapes && !nasda_tapes && !csa_tapes)
        fa_tapes = FA_TapeCode_ESA ;
    else if (!asf_tapes && !esa_tapes && nasda_tapes && !csa_tapes)
        fa_tapes = FA_TapeCode_NASDA ;
    else if (!asf_tapes && !esa_tapes && !nasda_tapes && csa_tapes)
        fa_tapes = FA_TapeCode_CSA ;
    else if (asf_tapes && esa_tapes && !nasda_tapes && !csa_tapes)
        fa_tapes = FA_TapeCode_ESAASF ;
    else if (asf_tapes && !esa_tapes && nasda_tapes && !csa_tapes)
        fa_tapes = FA_TapeCode_NASDAASF ;
    else if (asf_tapes && !esa_tapes && !nasda_tapes && csa_tapes)
        fa_tapes = FA_TapeCode_CSAASF ;
    else if (asf_tapes || esa_tapes || nasda_tapes || csa_tapes)
    {
        return (NULL);
    }

    return (fa_tapes) ;
}


/*==============================================================================
Function:       mini_report

Description:    Concatenates to display_string variable a report containing
                dtk conflict analysis lists.

Parameters:     dtk_list, header, type

Returns:        none

Creator:        Ron Green

Creation Date:  Tue Jun 20 18:09:19 PDT 1995

Notes:          Assumes there may not be a newline in display_string and
                so starts each line with a newline.
==============================================================================*/
static void
mini_report( dtk_list, header, type ) 
    llist *dtk_list ;
    char *header ;
{
    DB_RECORD   **dtk_rec ;
    cursor      ptr ;
    char        *truncateStr = "**INCOMPLETE**";
    char        headerBuf[500]; /* ridiculously large: don't want to overrun */
    int         printRecLen;
    int         display_stringLen;

    display_stringLen   = strlen( display_string );

    if (type == ANT_DOWN_LIST)
    {
        /* 
        -- NOTE: only the first 32 chars of the ant. down time string is
        -- displayed, we want to identify only the stn, antenna, and strttime
        */
        printRecLen = 32;   /* stn, antenna, strt */
    }
    else
    {
        /* 
        -- NOTE: only the first 35 chars of the dtk string is displayed,
        -- we want to identify only the stn, antenna, sat, sensor, rev,
        -- dtkid, and  activity
        */
        printRecLen = 35;
    }

    /* print the header */
    (void) sprintf( headerBuf, "\n%s Items: %d", header, NUMELTS( dtk_list ) ) ;
    if (display_stringLen + 1 < APS_GUI_DISPLAY_STR_SIZE)
    {
        (void)strncat( display_string, headerBuf,
            APS_GUI_DISPLAY_STR_SIZE - display_stringLen - 1 );
        display_stringLen = strlen( display_string );
    }

    /* print the list */
    for (dtk_rec = (DB_RECORD **) FIRST( dtk_list, ptr ) ; dtk_rec
        ; dtk_rec = (DB_RECORD **) NEXT( dtk_list, ptr ))
    {
        if (type == ANT_DOWN_LIST)
            (void) dtkm_antenna_down_times_rec2str( dtk_rec );
        else
            (void) dtkm_rec2str( dtk_rec ) ;

        if (display_stringLen + printRecLen + 1 < APS_GUI_DISPLAY_STR_SIZE)
        {
            (void) sprintf( display_string + display_stringLen,
                "\n%.*s", printRecLen, dtk_string ) ;
            display_stringLen = strlen( display_string );
        }
        /* indicate report is truncated */
        else if (display_stringLen + (int) strlen( truncateStr ) + 1
            < APS_GUI_DISPLAY_STR_SIZE)
        {
            (void) sprintf( display_string + display_stringLen,
                "\n%s", truncateStr );
            display_stringLen = strlen( display_string );
            break;
        }

    }
    if (display_stringLen + 1 < APS_GUI_DISPLAY_STR_SIZE)
    (void) sprintf( display_string + (int) strlen( display_string ), "\n" ) ;

    return;
}



/*==============================================================================
Function:       cb_save_dtk_changes

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
cb_save_dtk_changes( Widget widget, XtPointer client_data, XtPointer cbs_UNUSED)
{
BEGIN_CONTEXT( widget )

    /* make some headers for mini reporting */
    static char HSATDOWNTIMES[] = "SATELLITE DOWN TIMES" ;
    static char HANTDOWNTIMES[] = "ANTENNA DOWN TIMES" ;
    static char HCONFLICTS[] = "CONFLICTING" ;
    static char HUPDATES[] = "UPDATED" ;

    int         *pos_list,
                pos_cnt = 0;    

    int         dtk_savemode ;
    int         temp_perm = 0 ; 
                                 
    DB_RECORD   **newdtk_rec ;  /* new/edited dtk values */
    DB_RECORD   **parentdtk_rec ;  /* dummy, usually carries a downlink rec  */
    int         nrecs ;

    /* these are dtk record fields on the gui screen */
    char        *sat, *sensor, *rev, *dtkid,    /*fields comprising unique id.*/
                *dtkstatus, *darid,
                *fadtkid, *transid,
                *stationid, *antennaid,
                *strttime, *stoptime,
                *notes, *fa_sch_link, *plnr_quicklook,
                *ascdsc, *strtlat, *stoplat,
                *nrlat1, *nrlon1, *nrlat2, *nrlon2,
                *farlat1, *farlon1, *farlat2, *farlon2 ;
    /*
    -- these are the rest of the fields on the gui screen;
    -- they are combined to make up dtk record fields
    */
    char        activity_string[4] ;
    char        *fa_tapes ;

    llist       *dtk_sat_down_times,
                *antenna_down_times,
                *dtk_concurs,
                *dtk_similars,
                *dtk_conflicts,
                *dtk_updates ;

    int         stopFlag = FALSE;   /* TRUE if save should be discontinued */

    char        *satcode ;
    char        actid[7] ;

    int         irev ;
    int         mode ;

    int         status ;

    /* 
    -- this function was activated during an edit or during a 
    -- create, we need to keep track of which because save
    -- processing differs
    */
    dtk_savemode = (int) client_data ;

    /* 
    -- verify that it is ok to create/edit this dtk
    */

    sat         = gui_optionMenu_string( optionMenu_sat ) ;
    /*
    -- NOTE: get_satellite_code() won't return NULL because of
    -- how the satellites list and sat option menus are created
    */
    satcode     = get_satellite_code( sat ) ;
    sensor      = gui_optionMenu_string( optionMenu_sensor ) ;
    rev         = gui_TF_string( TF_REV ) ;
    irev        = atoi( rev ) ;
    dtkid       = gui_TF_string( TF_DTKID ) ;

    /* verify that fill-in fields for unique dtk key have been filled in */
    if ((*rev == STREND) || (*dtkid == STREND))
    {
        (void) sprintf( display_string, "No REV or DTK ID\nvalue given" ) ;
        popup_message( XmDIALOG_ERROR, "APS:ERROR",
            display_string, XtGrabNone ) ;

        XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; XtFree( dtkid ) ;
            return ;
    }   

    /* create a where clause that describes this dtk */

    (void) sprintf( where_clause, 
        "\nwhere %s = '%s' and %s = %s and %s = %s",
        APS_COL( DTK, DTK_SAT ), satcode,
        APS_COL( DTK, DTK_REV ), rev,
        APS_COL( DTK, DTK_DTKID ), dtkid) ;

    if (dtk_savemode == DTK_EDIT)
        (void) sprintf( question, "Allow changes to DTK for\n\n   SAT: %s\nSENSOR: %s\n   REV: %s\n    ID: %s?",
            sat, sensor, rev, dtkid ) ;
    else /* DTK_CREATE mode */
    {
        /* check that the new dtk doesn't already exist */
        nrecs = db_num_records( APS_dbproc, APS_TABLE( DTK ), where_clause ) ;
        if (nrecs > 0)
        {
            (void) sprintf( display_string, 
                "A Data Take already exists for\n\n   SAT: %s\n   REV: %s\n    ID: %s",
                sat, rev, dtkid ) ;
            popup_message( XmDIALOG_ERROR, "APS:ERROR", 
                display_string, XtGrabNone ) ;

            XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; XtFree( dtkid ) ;
            return ;
        }
        (void) sprintf( question, 
            "Create new DTK?\n\n   SAT: %s\nSENSOR: %s\n   REV: %s\n    ID: %s",
            sat, sensor, rev, dtkid ) ;
    }   

    if (AskUser( widget, question, NO ) == NO)
    {
        XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; XtFree( dtkid ) ;
        return ;
    }

    /* continue with the save/update */

    /*
    -- get the modified values
    */

    newdtk_rec =  new_table_record( APS_CDEFS( DTK ) ) ;
    parentdtk_rec =  new_table_record( APS_CDEFS( DTK ) ) ;
    /* if editing, initialize the new dtk rec w/ the original dtk values */
    if (dtk_savemode == DTK_EDIT)
        db_copy_record ( APS_CDEFS( DTK ), newdtk_rec, orig_dtk_rec ) ;

    (void) strcpy( CAST_DTK_SAT newdtk_rec[DTK_SAT], satcode) ;
    (void) strcpy( CAST_DTK_SENSOR newdtk_rec[DTK_SENSOR], sensor ) ;
    CAST_DTK_REV newdtk_rec[DTK_REV] = irev ;
    CAST_DTK_DTKID newdtk_rec[DTK_DTKID] = atoi( dtkid ) ;

    /* get the station and antenna id's */

    stationid   = gui_optionMenu_string( optionMenu_dtkm_station_id ) ;
    (void) strcpy( CAST_DTK_STATION_ID newdtk_rec[DTK_STATION_ID], stationid ) ;
    antennaid   = gui_optionMenu_string( optionMenu_antenna ) ;
    CAST_DTK_ANTENNA_ID newdtk_rec[DTK_ANTENNA_ID] = atoi( antennaid );

    /* get the start/stop times */

    /* activate the start/stop time fields for error validation */
    XtCallActionProc( TF_DTK_STRTTIME, "activate", NULL, NULL, 0 ) ;
    if (ASF_datetime_err)
    {
        XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; XtFree( dtkid ) ;
        XtFree( stationid ) ; XtFree( antennaid ) ;
        free_db_record( newdtk_rec );
        return ;
    }

    XtCallActionProc( TF_DTK_STOPTIME, "activate", NULL, NULL, 0 ) ;
    if (ASF_datetime_err)
    {
        XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; XtFree( dtkid ) ;
        XtFree( stationid ) ; XtFree( antennaid ) ;
        free_db_record( newdtk_rec );
        return ;
    }

    strttime = gui_TF_string( TF_DTK_STRTTIME ) ;
    stoptime = gui_TF_string( TF_DTK_STOPTIME ) ;
    (void) strcpy( CAST_DTK_STRTTIME newdtk_rec[DTK_STRTTIME], strttime ) ;
    (void) strcpy( CAST_DTK_STOPTIME newdtk_rec[DTK_STOPTIME], stoptime ) ;

    /* At this point have valid start/stop times and stn, so get permission */

    /* 
    -- NOTE: If (dtk_timed_pln_perm == 0), the permission has timed out, and we
    -- need to get it again temporarily to carry out the save action.
    */

    if ((dtk_savemode == DTK_CREATE) || (dtk_timed_pln_perm == 0))
    {
        /* get in-planning permission */
        if (get_timed_planning_permission( strttime, stoptime, stationid ) <= 0)
        {
            /* Couldn't get permission, message already popped up */
            XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; XtFree( dtkid ) ;
            XtFree( stationid ) ; XtFree( antennaid ) ;
            XtFree( strttime ) ; XtFree( stoptime ) ;
            free_db_record( newdtk_rec );
            
            if (dtk_timed_pln_perm == 0)
            {
                (void) sprintf( display_string, 
                               "The edited datatake has not been saved.  You must press EDIT again to be able to save it." ) ;
                popup_message( XmDIALOG_ERROR, "APS:ERROR", 
                              display_string, XtGrabNone ) ;
            }
            return ;
        }
        if (dtk_timed_pln_perm == 0)
            temp_perm = 1 ;
    }

    /* get the activity & fa tape flags in the actid */

    if (XmToggleButtonGetState( Activity_Downlink ))
    {
        /* check to see if a matching ROB has already been entered */

        fa_sch_link = gui_TF_string( TF_FA_SCHEDULE_LINK ) ;

        (void) sprintf( where_clause, 
                       "where %s = '%s' and %s = %s and %s = '%s' and %s like '%s%%'",
                       APS_COL( DTK, DTK_SAT ), satcode,
                       APS_COL( DTK, DTK_REV ), rev,
                       APS_COL(DTK, DTK_FA_SCHEDULE_LINK), fa_sch_link, 
                       APS_COL(DTK, DTK_ACTID), DTKM_ACTID_REALTIME_OBSERVATION_CODE );

        nrecs = db_num_records( APS_dbproc, APS_TABLE( DTK ), where_clause ) ;

        /* 
        -- No records means no ROBs; warn the user that he/she must enter ROBs
        -- after this save.
        */

        if (nrecs == 0)
        {
            (void) sprintf( display_string, "You must enter the corresponding Realtime Observation(s) after this save has completed." ) ;

            popup_message( XmDIALOG_WARNING, "APS:WARNING", 
                          display_string, XtGrabNone ) ;
        }
        else if (nrecs < 0)
        {
            /* An error has occurred - inform the user */

            (void) sprintf( display_string, "An error occurred while attempting\n to access the database.") ;
            popup_message( XmDIALOG_ERROR, "APS:ERROR", 
                display_string, XtGrabNone ) ;

            XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; 
            XtFree( dtkid ) ; XtFree( stationid ) ; XtFree( antennaid ) ;
            XtFree( strttime ) ; XtFree( stoptime ) ; XtFree( fa_sch_link ) ;
            free_db_record( newdtk_rec );

            return ;
        }
        /* 
        -- There are matching ROBs, so we can continue with the save without
        -- bothering the user.
        */

        mode = XDOWNLINK ;
        (void) sprintf( activity_string, "%s", activities[mode] ) ;

        XtFree( fa_sch_link ) ;
    }  /* end of handling downlink.   */
    else if (XmToggleButtonGetState( Activity_Observe ))
    {
        /* check to see if a matching RDL has already been entered */

        fa_sch_link = gui_TF_string( TF_FA_SCHEDULE_LINK ) ;

        (void) sprintf( where_clause, 
                       "where %s = '%s' and %s = %s and %s = '%s' and %s like '%s%%'",
                       APS_COL( DTK, DTK_SAT ), satcode,
                       APS_COL( DTK, DTK_REV ), rev,
                       APS_COL(DTK, DTK_FA_SCHEDULE_LINK), fa_sch_link, 
                       APS_COL(DTK, DTK_ACTID), 
                       DTKM_SENSOR_REALTIME_DOWNLINK_CODE );

        nrecs = db_num_records( APS_dbproc, APS_TABLE( DTK ), where_clause ) ;

        /* no records means no RDL; the save cannot be processed */

        if (nrecs == 0)
        {

            (void) sprintf(display_string,
                            "There are no Realtime Downlinks to match the \nFA Schedule Link: %s\nYou must enter a Realtime Downlink before entering \nmatching Realtime Observations.", 
                           fa_sch_link) ;

            popup_message( XmDIALOG_ERROR, "APS:ERROR", 
                          display_string, XtGrabNone ) ;
            
            XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; 
            XtFree( dtkid ) ; XtFree( stationid ) ; XtFree( antennaid ) ;
            XtFree( strttime ) ; XtFree( stoptime ) ; XtFree( fa_sch_link ) ;
            free_db_record( newdtk_rec );

            return ;
        }
        else if (nrecs < 0)
        {
            /* An error has occurred - inform the user */

            (void) sprintf( display_string, "An error occurred while attempting\n to access the database.") ;
            popup_message( XmDIALOG_ERROR, "APS:ERROR", 
                display_string, XtGrabNone ) ;

            XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; 
            XtFree( dtkid ) ; XtFree( stationid ) ; XtFree( antennaid ) ;
            XtFree( strttime ) ; XtFree( stoptime ) ; XtFree( fa_sch_link ) ;
            free_db_record( newdtk_rec );

            return ;
        }
        /* 
        -- There is a matching RDL, so we can continue with the save.
        */

        mode = XOBSERVE ;
        (void) sprintf( activity_string, "%s", activities[mode] ) ;
    }  /* end of handling realtime observation.   */
    else if (XmToggleButtonGetState( Activity_Record ))
    {
        mode = XRECORD ;
        (void) sprintf( activity_string, "%s", activities[mode] ) ;
    }
    else /* not downlink, observation or record, must be dump */
    {
        mode = XDUMP ;
        (void) sprintf( activity_string, "%s", activities[mode] ) ;
    }

    if ((fa_tapes = get_fa_tapes_from_form()) == NULL)
    {
        (void) sprintf( display_string,
            "Invalid destination selected for FA tapes" ) ; 
        popup_message( XmDIALOG_ERROR, "APS:ERROR",
            display_string, XtGrabNone ) ;
        XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; XtFree( dtkid ) ;
        XtFree( stationid ) ; XtFree( antennaid ) ;
        XtFree( strttime ) ; XtFree( stoptime ) ;
        free_db_record( newdtk_rec );
        if ((dtk_savemode == DTK_CREATE) || temp_perm)
            free_timed_planning_permission() ;
        return ;
    }

    (void) sprintf( actid, "%s%s", activity_string, fa_tapes ) ;
    (void) strcpy( CAST_DTK_ACTID newdtk_rec[DTK_ACTID], actid ) ;

    /* get the dtkstatus and transid */

    dtkstatus = gui_optionMenu_string( optionMenu_dtk_status ) ;
    (void) strcpy( CAST_DTK_DTKSTAT newdtk_rec[DTK_DTKSTAT], dtkstatus ) ;

    transid =  gui_optionMenu_string( optionMenu_J1_DLinkChannel ) ;
    (void) strcpy( CAST_DTK_TRANSID newdtk_rec[DTK_TRANSID], transid ) ;

    /* get the miscellany */

    plnr_quicklook = gui_optionMenu_string( optionMenu_PlanQuicklook ) ;
    if (strcmp(plnr_quicklook, "YES") == 0)
      	CAST_DTK_PLANNER_QUICKLOOK newdtk_rec[DTK_PLANNER_QUICKLOOK] = 'Y' ;
    else
      	CAST_DTK_PLANNER_QUICKLOOK newdtk_rec[DTK_PLANNER_QUICKLOOK] = 'N' ;

    fadtkid     = gui_TF_string( TF_FADTKID ) ;
    (void) strcpy( CAST_DTK_FADTKID newdtk_rec[DTK_FADTKID], fadtkid ) ;

    darid       = gui_TF_string( TF_DTK_DARID ) ;
    CAST_DTK_DARID newdtk_rec[DTK_DARID]    = atoi( darid ) ;

    notes       = gui_TF_string( TF_NOTES ) ;
    (void) strcpy( CAST_DTK_NOTES newdtk_rec[DTK_NOTES], notes ) ;

    fa_sch_link = gui_TF_string( TF_FA_SCHEDULE_LINK ) ;
    (void) strcpy( CAST_DTK_FA_SCHEDULE_LINK newdtk_rec[DTK_FA_SCHEDULE_LINK],
        fa_sch_link ) ;

    /*
    -- if default values are known, get and put them in any UNFILLED fields
    -- (includes getting the coverage values)
    */

    if ((status = dtkm_default_values( newdtk_rec, newdtk_rec )) < 0) 
    {
        char    msg[BUFSIZ] ;

        (void) sprintf( display_string, "Error setting default values\n" );
        switch (status)
        {
            case 
            DTKM_ERROR_MASKINOUT_PROBLEM_PLEASE_RUN_aps_crt_nom_mask_FOR_DTK :
                (void) sprintf( display_string, 
        "aps_crt_nom_mask must be run\nfor the time period of the dtk.\n" ) ;
                break ;

            case DTKM_ERROR_BAD_DTKSTAT_VALUE :
                form_bad_dtkstat_msg( newdtk_rec, msg ) ;
                (void) strcat( display_string, msg ) ;
                break ;

            case DTKM_ERROR_NO_CVRG_MUST_RUN_CREATE_NOMINAL_COVERAGE :
                (void) sprintf( display_string, 
        "Nominal Coverage must be run\nfor the time period of the dtk.\n" ) ;
                break ;

            default :
                (void) strcat( display_string, DTKM_ERROR_MESSAGE( status ) ) ;
                break ;
        }
        popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string,
            XtGrabNone );

        XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; XtFree( dtkid ) ;
        XtFree( stationid ) ; XtFree( antennaid ) ;
        XtFree( strttime ) ; XtFree( stoptime ) ;
        XtFree( dtkstatus ) ; XtFree( transid ) ; XtFree( plnr_quicklook ) ;
        XtFree( fadtkid ); XtFree( darid ); XtFree( notes );
        XtFree( fa_sch_link );
        free_db_record( newdtk_rec );
        if ((dtk_savemode == DTK_CREATE) || temp_perm)
            free_timed_planning_permission() ;
        return ;
    } 

    /* if need to allow overriding of dtk being completely in nominal mask */
    if (satellite_coverage_not_allowed( satcode, sensor ) == FALSE &&
        (strcmp( dtkstatus, "PLN" ) == 0 || strcmp( dtkstatus, "QUE" )
        || strcmp( dtkstatus, "SUB" ) == 0))
    {
        status = dtkm_check_values( newdtk_rec, newdtk_rec );

        /* in case they were changed: redisplay */
        display_dtkmanager_fields( widget, newdtk_rec );
        /* also set new default local variables */
        if ((fa_tapes = get_fa_tapes_from_form()) == NULL)
        {
            (void) sprintf( display_string,
                    "Invalid destination\nselected for FA tapes" ) ;    
            popup_message( XmDIALOG_ERROR, "APS:ERROR",
                    display_string, XtGrabNone ) ;
            XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; XtFree( dtkid ) ;
            XtFree( stationid ) ; XtFree( antennaid ) ;
            XtFree( strttime ) ; XtFree( stoptime ) ;
            free_db_record( newdtk_rec );
            if ((dtk_savemode == DTK_CREATE) || temp_perm)
                free_timed_planning_permission() ;
            return ;
        }
        ascdsc   = (char *) malloc( APS_SIZE( DTK, DTK_ASCDSC ) );
        *ascdsc  = CAST_DTK_ASCDSC newdtk_rec[DTK_ASCDSC];
        strtlat  = gui_TF_string( TF_STRTLAT ) ;
        stoplat  = gui_TF_string( TF_STOPLAT ) ;
        nrlat1   = gui_TF_string( TF_NRLAT1 ) ;
        nrlon1   = gui_TF_string( TF_NRLON1 ) ;
        nrlat2   = gui_TF_string( TF_NRLAT2 ) ;
        nrlon2   = gui_TF_string( TF_NRLON2 ) ;
        farlat1  = gui_TF_string( TF_FARLAT1 ) ;
        farlon1  = gui_TF_string( TF_FARLON1 ) ;
        farlat2  = gui_TF_string( TF_FARLAT2 ) ;
        farlon2  = gui_TF_string( TF_FARLON2 ) ;

        if (status >= 0)
        /* ignore errors: will be caught below in dtkm_process_dtk_proposal() */
        {
            char reduced_strttime[22] ;
            char reduced_stoptime[22] ;

            /* check in-station mask separately to allow overriding of times */
            TimeoutCursors( True, True, NULL ) ;
            status = dtkm_in_station_mask(
                CAST_DTK_STATION_ID newdtk_rec[DTK_STATION_ID],
                newdtk_rec, 0, reduced_strttime, reduced_stoptime) ;
            TimeoutCursors( False, False, NULL ) ;

            if (status < 0)
            {
                check_in_station_mask_status( widget, status, newdtk_rec,
                    reduced_strttime, reduced_stoptime, &stopFlag );
                if (stopFlag == TRUE)
                {
                    XtFree( sat ) ; XtFree( sensor ) ;
                    XtFree( rev ) ; XtFree( dtkid ) ;
                    XtFree( stationid ) ; XtFree( antennaid ) ;
                    XtFree( strttime ) ; XtFree( stoptime ) ;
                    XtFree( dtkstatus ) ; XtFree( transid ) ; 
		    XtFree( plnr_quicklook ) ;
                    XtFree( fadtkid ); XtFree( darid ); XtFree( notes );
                    XtFree( fa_sch_link );
                    free( ascdsc );
                    XtFree( strtlat ); XtFree( stoplat );
                    XtFree( nrlat1 ); XtFree( nrlon1 );
                    XtFree( nrlat2 ); XtFree( nrlon2 );
                    XtFree( farlat1 ); XtFree( farlon1 );
                    XtFree( farlat2 ); XtFree( farlon2 );
                    free_db_record( newdtk_rec );
                    if ((dtk_savemode == DTK_CREATE) || temp_perm)
                        free_timed_planning_permission() ;
                    return;
                }

                if (*display_string != STREND)
                {
                    (void) sprintf( question,
                        "Problems were noted with your Data Take\n\n%s\n\nDo you wish to continue save with current status: %s?\n\n(Answer NO to continue editing)", 
                            display_string,
                            CAST_DTK_DTKSTAT newdtk_rec[DTK_DTKSTAT] ) ;

                    if (AskUser( widget, question, NO ) == NO)
                    {
                        XtFree( sat ) ; XtFree( sensor ) ;
                        XtFree( rev ) ; XtFree( dtkid ) ;
                        XtFree( stationid ) ; XtFree( antennaid ) ;
                        XtFree( strttime ) ; XtFree( stoptime ) ;
                        XtFree( dtkstatus ) ; XtFree( transid ) ;  
			XtFree( plnr_quicklook ) ;
                        XtFree( fadtkid ); XtFree( darid ); XtFree( notes );
                        XtFree( fa_sch_link );
                        free( ascdsc );
                        XtFree( strtlat ); XtFree( stoplat );
                        XtFree( nrlat1 ); XtFree( nrlon1 );
                        XtFree( nrlat2 ); XtFree( nrlon2 );
                        XtFree( farlat1 ); XtFree( farlon1 );
                        XtFree( farlat2 ); XtFree( farlon2 );
                        free_db_record( newdtk_rec );
                        if ((dtk_savemode == DTK_CREATE) || temp_perm)
                            free_timed_planning_permission() ;
                        return ;
                    }
                }
                /* update local variables, in case the gui was changed */
                XtFree( strttime ); XtFree( stoptime );
                strttime = gui_TF_string( TF_DTK_STRTTIME ) ;
                stoptime = gui_TF_string( TF_DTK_STOPTIME ) ;
            }
        }
    }
    else
    {
        /* in case they were changed: redisplay */
        display_dtkmanager_fields( widget, newdtk_rec );
        /* also update local variables */
        if ((fa_tapes = get_fa_tapes_from_form()) == NULL)
        {
            (void) sprintf( display_string,
                    "Invalid destination\nselected for FA tapes" ) ;    
            popup_message( XmDIALOG_ERROR, "APS:ERROR",
                    display_string, XtGrabNone ) ;
            XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; XtFree( dtkid ) ;
            XtFree( stationid ) ; XtFree( antennaid ) ;
            XtFree( strttime ) ; XtFree( stoptime ) ;
            free_db_record( newdtk_rec );
            if ((dtk_savemode == DTK_CREATE) || temp_perm)
                free_timed_planning_permission() ;
            return ;
        }
        ascdsc   = (char *) malloc( APS_SIZE( DTK, DTK_ASCDSC ) );
        *ascdsc  = CAST_DTK_ASCDSC newdtk_rec[DTK_ASCDSC];
        strtlat  = gui_TF_string( TF_STRTLAT ) ;
        stoplat  = gui_TF_string( TF_STOPLAT ) ;
        nrlat1   = gui_TF_string( TF_NRLAT1 ) ;
        nrlon1   = gui_TF_string( TF_NRLON1 ) ;
        nrlat2   = gui_TF_string( TF_NRLAT2 ) ;
        nrlon2   = gui_TF_string( TF_NRLON2 ) ;
        farlat1  = gui_TF_string( TF_FARLAT1 ) ;
        farlon1  = gui_TF_string( TF_FARLON1 ) ;
        farlat2  = gui_TF_string( TF_FARLAT2 ) ;
        farlon2  = gui_TF_string( TF_FARLON2 ) ;

        if (satellite_coverage_not_allowed( satcode, sensor ) == TRUE &&
            (orig_dtk_rec == NULL || strcmp( CAST_DTK_STATION_ID
            orig_dtk_rec[DTK_STATION_ID], stationid ) != 0))
        {
            /* REMINDER re: no coverage/in-station mask checking is done */
            (void) sprintf( question,
                "WARNING: no checking done for\n DTK times 'in a pass'\n\n'%s' is correct station id?",
                stationid ) ;
            if (AskUser( widget, question, YES ) == NO)
            {
                XtFree( sat ) ; XtFree( sensor ) ;
                XtFree( rev ) ; XtFree( dtkid ) ;
                XtFree( stationid ) ; XtFree( antennaid ) ;
                XtFree( strttime ) ; XtFree( stoptime ) ;
                XtFree( dtkstatus ) ; XtFree( transid ) ; 
		XtFree( plnr_quicklook ) ;
                XtFree( fadtkid ); XtFree( darid ); XtFree( notes );
                XtFree( fa_sch_link );
                free( ascdsc );
                XtFree( strtlat ); XtFree( stoplat );
                XtFree( nrlat1 ); XtFree( nrlon1 );
                XtFree( nrlat2 ); XtFree( nrlon2 );
                XtFree( farlat1 ); XtFree( farlon1 );
                XtFree( farlat2 ); XtFree( farlon2 );
                free_db_record( newdtk_rec );
                if ((dtk_savemode == DTK_CREATE) || temp_perm)
                    free_timed_planning_permission() ;
                return;
            }
        }
    }

    /* initialize informational dtk lists for conflict analysis */

    dtk_sat_down_times  = create_dyn_llist() ;
    antenna_down_times  = create_dyn_llist() ;
    dtk_concurs         = create_dyn_llist() ;
    dtk_similars        = create_dyn_llist() ;
    dtk_conflicts       = create_dyn_llist() ;
    dtk_updates         = create_dyn_llist() ;
    
    /*
    -- check the values and try to insert the new dtk (incl. conflict analysis)
    */

    TimeoutCursors( True, True, NULL ) ;
    status = dtkm_process_dtk_proposal( APS_dbproc, 
        newdtk_rec, newdtk_rec, parentdtk_rec,
        dtk_sat_down_times, antenna_down_times,
        dtk_concurs, dtk_similars, dtk_conflicts, dtk_updates );
    TimeoutCursors( False, False, NULL ) ;

    /* free the in-planning permission: for both EDIT and CREATE */
    free_timed_planning_permission() ;

    /* redisplay fields, in case they were changed in process dtk proposal */
    display_dtkmanager_fields( widget, newdtk_rec );

    /*
    -- NOTE: values in newdtk_rec may differ from some local variable
    -- settings, but these local variables are not used in the rest of
    -- this function, except to free their allocated memory, so they are
    -- not being updated here.
    */

    if (status < 0) /* an error occurred */
    {
#ifdef DEBUG
        db_print_record( newdtk_rec, APS_CDEFS( DTK ) ) ;
        dtkm_print( stdout, newdtk_rec ) ;
        (void) printf( "MESSAGE: %s\n %s", DTKM_ERROR_MESSAGE( status ),
            CAST_DTK_DTKSTAT newdtk_rec[DTK_DTKSTAT] ) ;
#endif

        /* create informational error messages (newline terminated) */

        switch (status)
        {
            /* station id errors */
            case DTKM_ERROR_BAD_SAT_STATION_ID_VALUE:
                (void) sprintf( display_string,
                    "Invalid STATION ID (%s) for SAT %s\n", stationid, sat );
                break;

            /* dtk status errors */
            case
            DTKM_ERROR_DTK_SHOULD_BE_DEL_REJ_INV_QUE_SUB_REQ_PLN_OR_SCH_STATUS:
            case DTKM_ERROR_BAD_DTKSTAT_VALUE:
                form_bad_dtkstat_msg( newdtk_rec, display_string ) ;
                break;
            case DTKM_ERROR_A1_REALTIME_SENSOR_DTKSTAT_NOT_INV:
                (void) sprintf( display_string,
                    "Invalid status for SAT/Sensor/Activity: %s/%s/Realtime\nValid Value is: INV\n",
                    sat, sensor ) ;
                break ;

            /* start/stop times */
            case DTKM_ERROR_TIMES_NOT_WITHIN_REV:
                (void) sprintf( display_string,
                    "Start and/or Stop time is outside of the REV: %d\n",
                    irev );
                break;

            /* bad activity */
            case DTKM_ERROR_BAD_ACTID_VALUE:
            case DTKM_ERROR_ACTIVITY_ILLEGAL:
            case DTKM_ERROR_ACTIVITY_NOT_FOUND:
            case DTKM_ERROR_SAT_DOES_NOT_HAVE_A_RECORDER:
                (void) sprintf( display_string,  
                    "This Activity\nNot valid for SAT %s\n", sat ) ;
                break;
            /* bad fa tapes */
            case DTKM_ERROR_BAD_ACTID_AGENCY_VALUE:
                (void) sprintf( display_string,  
                    "Destination for FA tape\n is Not valid for SAT %s\n",
                    sat ) ;
                break;

            /* bad transid (downlink channel) */
            case DTKM_ERROR_BAD_TRANSID_VALUE:
                form_bad_transid_msg( satcode, transid, mode, display_string );
                break;

            /* coverage errors */
            case DTKM_ERROR_NO_CVRG_MUST_RUN_CREATE_NOMINAL_COVERAGE:
                (void) sprintf( display_string, 
                    "Nominal Coverage must be run\nfor the time period of the dtk.\n" ) ;
                break ;

            /* in-station mask error */
            case 
            DTKM_ERROR_MASKINOUT_PROBLEM_PLEASE_RUN_aps_crt_nom_mask_FOR_DTK :
            case DTKM_ERROR_DOWNLINK_DTK_NOT_ENTIRELY_WITHIN_STATION_PASS:
            case DTKM_ERROR_DOWNLINK_DTK_HAS_NO_TIME_IN_MASK :
            case DTKM_ERROR_NO_MASKINOUT_RECS_FOUND_IN_REV    :
                check_in_station_mask_status( widget, status, newdtk_rec,
                    NULL, NULL, &stopFlag );
                break ;
                
            /* use this if check values' status value is self-explanatory */

            case DTKM_ERROR_BAD_REV_VALUE:
            case DTKM_ERROR_DTKID_LT_ZERO:
            case DTKM_ERROR_INPUT_DTK_WITH_DTKID_LT_ZERO:
            case DTKM_ERROR_DTKID_TOO_BIG:
            case DTKM_ERROR_OBSERVATION_DTK_HAS_ANTENNA_ID_NE_ZERO:
            case DTKM_ERROR_SENSOR_AND_ACTID_DONT_MATCH:
            case DTKM_ERROR_STOPTIME_LE_STRTTIME:
            case DTKM_ERROR_TIME_INTERVAL_LT_SAT_INCLUSION_PERIOD:
            case DTKM_ERROR_ANTENNA_ID_LE_ZERO:
            case DTKM_ERROR_ANTENNA_ID_IS_ZERO_NO_PRIORITY:
                (void) sprintf( display_string, "Error in DTK values\n" );
                break;

            /* database table records missing for given field */

            case DTKM_ERROR_NO_ANTENNAS_FOUND_IN_ANTENNA_PREF_TABLE:
                (void) sprintf( display_string,
                    "No records for SAT %s\nin antenna_pref relation\n", sat );
                break;
            case DTKM_ERROR_ANTENNA_PREFERENCE_NOT_FOUND_FOR_DTK_PROPOSAL:
                (void) sprintf( display_string,
                    "No records for SAT %s\nSTATION %s\nand ANTENNA %s\nin antenna_pref relation\n",
                    sat, stationid, antennaid );
                break;
            case
                DTKM_ERROR_ANTENNA_REC_NOT_FOUND_FOR_DTK_PROPOSAL:
                (void) sprintf( display_string,
                    "No STATION/ANTENNA %s/%s record\nin antenna relation\n",
                    stationid, antennaid );
                break;
            case DTKM_ERROR_ANTENNA_PRIORITY_NOT_FOUND_FOR_DTK_PROPOSAL:
                (void) sprintf( display_string,
                    "No ANTENNA %s record\nin antenna priority relation\n",
                    antennaid );
                break;

            /* dtk change/conflict lists errors */

            case DTKM_ERROR_IN_DUPLICATING_DTK_RECORD_INTO_LIST:
                (void) sprintf( display_string,
                    "Unable to copy DTK\ninto UPDATE list\n" );
                break;
            case DTKM_ERROR_IN_MOVING_REC_TO_OTHER_LIST:
            case DTKM_ERROR_IN_MOVING_RECORDS_TO_OTHER_LIST:
                (void) sprintf( display_string,
                    "Conflict analysis\nunable to place DTK into list\n" );
                break;
            case DTKM_ERROR_DTK_LIST_IS_MIXED_WITH_DIFFERENT_ANTENNAS:
                (void) sprintf( display_string, "Conflict analysis\n" );
                break;
            case DTKM_ERROR_UNLINKING_RECORD:
                (void) sprintf( display_string,
                    "Conflict analysis\nerror processing bumped dtks\n" );
                break;

            /* database modification errors */

            case DTKM_ERROR_DTK_NOT_INSERTED:
            case DTKM_ERROR_ATTEMPT_TO_UPDATE_NONEXISTING_DTK:
            case DTKM_ERROR_DELETION_DURING_UPDATE:
            case DTKM_ERROR_DTK_NOT_INSERTED_DURING_UPDATE:
                (void) sprintf( display_string,
                    "FAILED to place dtk in database\n" );
                break;

            /* database access/table error */

            case DTKM_ERROR_BAD_SAT_SENSOR_VALUES:
            case DTKM_ERROR_ACTIVITY_GT_1_RECORD:
            case DTKM_ERROR_NO_RECS_IN_ACTIVITIES_RELATION:
            case DTKM_ERROR_UNKNOWN_SAT_IN_dtkm_check_transid:
            case DTKM_ERROR_CANNOT_IDENTIFY_DTK_TO_UPDATE:
            case DTKM_ERROR_NO_RECORDS_IN_ANTENNA_RELATION:
            case DTKM_ERROR_NO_RECORDS_IN_ANTENNA_PRIORITY_RELATION:
            case DTKM_ERROR_NO_RECORDS_IN_ANTENNA_PREF_RELATION:
                (void) sprintf( display_string, "Data Base is in error\n" );
                break;
            case DTKM_ERROR_DB_QUERY_FAILED:
                gui_aps_internal_error( XmDIALOG_ERROR, __FILE__, __LINE__,
                    "Incorrect Query" ) ;
                break;

            /* internal code errors: PROGRAMMER needs to fix these problems */

            case DTKM_ERROR_NULL_DTK_PROPOSAL :
            case DTKM_ERROR_NULL_DTK_RESULT_RECORD :
            case DTKM_ERROR_NULL_RECORD:
                gui_aps_internal_error( XmDIALOG_ERROR, __FILE__, __LINE__,
                    "NULL DTK record" ) ;
                break;
            case DTKM_ERROR_UNEXPECTED_RETURN_CODE_FROM_dtkm_in_station_mask:
            case DTKM_ERROR_UNKNOWN_RETURN_CODE_FROM_dtkm_update_best_antenna:
            case DTKM_ERROR_UNKNOWN_RETURN_CODE_FROM_dtkm_check_bumpability:
            case DTKM_ERROR_SAT_NOT_R1_FOR_dtkm_r1_update_sensor:
            case DTKM_ERROR_UNKNOWN_NORMAL_RETURN_CODE_FROM_DTKM_PROCESS_DTK:
                gui_aps_internal_error( XmDIALOG_ERROR, __FILE__, __LINE__,
                    DTKM_ERROR_MESSAGE( status ) ) ;
                break;
            case DTKM_ERROR_ANTENNA_DOWN_TIMES_LIST_NOT_INITIALIZED:
            case DTKM_ERROR_ANTENNA_DOWN_TIMES_LIST_NOT_EMPTY:
            case DTKM_ERROR_SAT_DOWN_TIMES_LIST_NOT_INITIALIZED:
            case DTKM_ERROR_SAT_DOWN_TIMES_LIST_NOT_EMPTY:
            case DTKM_ERROR_BUMPS_LIST_NOT_INITIALIZED:
            case DTKM_ERROR_BUMPS_LIST_NOT_EMPTY:
            case DTKM_ERROR_CONCURS_LIST_NOT_INITIALIZED:
            case DTKM_ERROR_CONCURS_LIST_NOT_EMPTY:
            case DTKM_ERROR_CONFLICTS_LIST_NOT_INITIALIZED:
            case DTKM_ERROR_CONFLICTS_LIST_NOT_EMPTY:
            case DTKM_ERROR_DTK_UPDATES_LIST_NOT_EMPTY:
            case DTKM_ERROR_DTK_UPDATES_LIST_NOT_INITIALIZED:
            case DTKM_ERROR_PARALLELS_LIST_NOT_INITIALIZED:
            case DTKM_ERROR_PARALLELS_LIST_NOT_EMPTY:
            case DTKM_ERROR_SAME_PASS_LIST_NOT_INITIALIZED:
            case DTKM_ERROR_SAME_PASS_LIST_NOT_EMPTY:
            case DTKM_ERROR_SIMILARS_LIST_NOT_INITIALIZED:
            case DTKM_ERROR_SIMILARS_LIST_NOT_EMPTY:
            case DTKM_ERROR_INPUT_DTK_LIST_NOT_INITIALIZED:
            case DTKM_ERROR_INPUT_DTK_LIST_EMPTY:
            case DTKM_ERROR_DTK_SAT_LIST_NOT_INITIALIZED:
            case DTKM_ERROR_DTK_SAT_LIST_NOT_EMPTY:
            case DTKM_ERROR_NULL_ANTENNA_LIST_PTR:
            case DTKM_ERROR_NO_STRTTIME:
            case DTKM_ERROR_NO_STOPTIME:
            case DTKM_ERROR_INPUT_NULL_POINTER_FOR_VALUE:
            case DTKM_ERROR_NULL_OUTPUT_DTK_LIST:
                (void) sprintf( display_string,
                    "sub-function's argument is in error: %s",
                     DTKM_ERROR_MESSAGE( status ) ) ;
                gui_aps_internal_error( XmDIALOG_ERROR, __FILE__, __LINE__,
                    display_string) ;
                break;
            case DTKM_ERROR_IN_CODE_dtkm_j1_equipment_status_check:
            case DTKM_ERROR_IN_CODE_dtkm_activities_conflict:
                (void) sprintf( display_string, "sub-function code error: %s",
                     DTKM_ERROR_MESSAGE( status ) ) ;
                gui_aps_internal_error( XmDIALOG_ERROR, __FILE__, __LINE__,
                    display_string) ;
                break;

            default:
                (void) sprintf( display_string,
                    "Error in processing the DTK\n" );
                break;
        }

        if (stopFlag != TRUE)
        {
            (void) strcat( display_string, DTKM_ERROR_MESSAGE( status ) ) ;
            popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string,
                XtGrabNone );
        }

        XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; XtFree( dtkid ) ;
        XtFree( stationid ) ; XtFree( antennaid ) ;
        XtFree( strttime ) ; XtFree( stoptime ) ;
        XtFree( dtkstatus ) ; XtFree( transid ) ;  XtFree( plnr_quicklook ) ;
        XtFree( fadtkid ); XtFree( darid ); XtFree( notes );
        XtFree( fa_sch_link );
        free( ascdsc );
        XtFree( strtlat ); XtFree( stoplat );
        XtFree( nrlat1 ); XtFree( nrlon1 );
        XtFree( nrlat2 ); XtFree( nrlon2 );
        XtFree( farlat1 ); XtFree( farlon1 );
        XtFree( farlat2 ); XtFree( farlon2 );
        free_db_record( newdtk_rec );
        DEL_LIST( dtk_sat_down_times );
        DEL_LIST( antenna_down_times );
        DEL_LIST( dtk_concurs );
        DEL_LIST( dtk_similars );
        DEL_LIST( dtk_conflicts );
        DEL_LIST( dtk_updates );
        return ;
    }
    else    /* no errs occurred to terminate processing, now interpret results*/
    {
        /* NOTE: desire was to insert/modify dtk */
        switch (status)
        {
        case DTKM_DTK_ACCEPTED_INHERITANCE:  
            (void) sprintf( question, 
                "SUCCESS:  However, STATUS was obtained from downlink.\n" );
            break;
        case DTKM_DTK_ACCEPTED:
        case DTKM_DTK_DELETED_OK:   /* insert/modify succeeded */
            (void) sprintf( question, "SUCCESS\n" );
            break;
        default:    /* some kind of conflict */
            /* display error and ask question */
            switch (status)
            {
            case DTKM_DTK_PROPOSAL_REJ:
                (void) sprintf( question, "DTK rejected\n" );
                break;
            case DTKM_DTK_PROPOSAL_CONFLICT:
                (void) sprintf( question, "DTK conflicts with other(s)\n" );
                break;
            default:    /* unknown "normal" return */
                (void) sprintf( question,
                    "Result of Operation is unknown.\n\nAPS INTERNAL ERROR:\n  FILE: %s  LINE: %d\n  %s\n  return code: %d\n\n",
                    __FILE__, __LINE__, DTKM_ERROR_MESSAGE(
                    DTKM_ERROR_UNKNOWN_NORMAL_RETURN_CODE_FROM_DTKM_PROCESS_DTK
                    ), status ) ;
                break;
            }
            (void) strcat( question,
                "Do you want the down times/conflicts displayed?\n" );
            if (AskUser( widget, question, YES ) == YES)
            {
                /* Conflicts list */
                *display_string = STREND;   /* clear out for mini_report */
                mini_report( dtk_conflicts, HCONFLICTS, DTK_TYPE_LIST );
                /* Down Times lists */
                if (NUMELTS( dtk_sat_down_times ) > 0)
                {
                    mini_report( dtk_sat_down_times, HSATDOWNTIMES,
                        DTK_TYPE_LIST );
                }
                /* Antenna Down Times list */
                if (NUMELTS( antenna_down_times ) > 0)
                {
                    mini_report( antenna_down_times, HANTDOWNTIMES,
                        ANT_DOWN_LIST );
                }
                if (*display_string != STREND)
                    (void)AskUser( widget, display_string, ASK_OK );
            }
            *question = STREND;
            break;
        }
        if (NUMELTS( dtk_updates ) > 0)
        {
            (void) strcat( question,
                "Do you want all the updated DTKs displayed?\n" );
            if (AskUser( widget, question, YES ) == YES)
            {
                *display_string = STREND;   /* clear out for mini_report */
                mini_report( dtk_updates, HUPDATES, DTK_TYPE_LIST );
                (void)AskUser( widget, display_string, ASK_OK );
            }
        }
    }

    DEL_LIST( dtk_sat_down_times );
    DEL_LIST( antenna_down_times );
    DEL_LIST( dtk_concurs );
    DEL_LIST( dtk_similars );
    DEL_LIST( dtk_conflicts );
    DEL_LIST( dtk_updates );

    /* if we are editing  get the currently selected dtk */
    if (dtk_savemode == DTK_EDIT)
        XmListGetSelectedPos( scrolledList_DTKS, &pos_list, &pos_cnt ) ;

    /* refresh the DTK list */
    cb_show_dtk_records( widget, (XtPointer) scrolledList_DTKS, NULL ) ;

    /* 
    -- reselect the item in the list if one was previously selected, force 
    -- the selection cb so as to update the fields on the DTK Manager form 
    --
    -- NOTE: the record should be in the same position in the list
    -- However, this may not be true if DTKS were added while this update
    -- is being done.
    */
    if (pos_cnt)
    {
        XmListSelectPos( scrolledList_DTKS, pos_list[0], True ) ;
        XtFree( (char *) pos_list ) ;
    }   
    else /* select the first one */
        XmListSelectPos( scrolledList_DTKS, 1, True ) ;

    cb_set_dtkmanager_editability( widget, DTK_RESET, NULL ) ;

    /* free all the pointers */
    XtFree( sat ) ; XtFree( sensor ) ; XtFree( rev ) ; XtFree( dtkid ) ;
    XtFree( stationid ) ; XtFree( antennaid ) ;
    XtFree( strttime ) ; XtFree( stoptime ) ;
    XtFree( dtkstatus ) ; XtFree( transid ) ; XtFree( plnr_quicklook ) ;
    XtFree( fadtkid ); XtFree( darid ); XtFree( notes );
    XtFree( fa_sch_link );
    free( ascdsc );
    XtFree( strtlat ); XtFree( stoplat );
    XtFree( nrlat1 ); XtFree( nrlon1 );
    XtFree( nrlat2 ); XtFree( nrlon2 );
    XtFree( farlat1 ); XtFree( farlon1 );
    XtFree( farlat2 ); XtFree( farlon2 );
    free_db_record( newdtk_rec );

END_CONTEXT

    return ;
}



/*==============================================================================
Function:       cb_create_dtk

Description:    
    The activate callback for the CREATE pushbutton

Parameters:     Standard X Callback parameters

Returns:        None    

Creator:        Ron Green

Creation Date:  10/31/1994

Notes:      
==============================================================================*/
/* ARGSUSED1 */
void
cb_create_dtk( Widget widget,
    XtPointer client_data_UNUSED, XtPointer cbs_UNUSED )
{
BEGIN_CONTEXT( widget )

    XtSetSensitive( optionMenu_sat, TRUE ) ;
    XtSetSensitive( optionMenu_sensor, TRUE ) ;
    XtSetSensitive( optionMenu_dtkm_station_id, TRUE ) ;
    XtSetSensitive( optionMenu_dtk_direction, TRUE ) ;

    /* 
    -- use the set_dtkmanager_editablility cb to turn on the fields 
    -- for edit pass null as the cbs structure so it knows this isn't 
    -- called by a widget
    */
    cb_set_dtkmanager_editability( widget, (XtPointer) DTK_CREATE, NULL ) ;

    /* remove any currently installed callbacks for the SaveButton */
     XtRemoveAllCallbacks( pushButton_SaveDTKChanges, XmNactivateCallback ) ;

    /* now install the cb_save_dtk cb passing the DTK_CREATE flag */
    XtAddCallback( pushButton_SaveDTKChanges, XmNactivateCallback, 
        cb_save_dtk_changes, (XtPointer) DTK_CREATE ) ;
END_CONTEXT 
}


/*==============================================================================
Function:       cb_delete_dtk_record

Description:    
    The activate callback for the DELETE button.

Parameters:     Standard X Callback parameters

Returns:        None    

Creator:        Ron Green

Creation Date:  09/dd/1994

Notes:      
==============================================================================*/
/* ARGSUSED1 */
void
cb_delete_dtk_record( Widget widget, XtPointer client_data_UNUSED,
    XtPointer cbs_UNUSED )
{
BEGIN_CONTEXT( widget )

    char *satcode ;
    char *satname ;
    char *sensor ;
    char *rev ;
    int   rev_int ;
    char *dtkid ;
    int   dtkid_int ;
    char temp_string[1024] ;

    DB_RECORD       **dtk_rec ;
    DB_RECORD       **new_rec ;
    llist            *dtk_list = NULL ;
    cursor            dtk_list_ptr ; 
    llist            *obs_dtks;         /* list of observations */

    int ret_code ;
    int i ;

    satname = gui_optionMenu_string( optionMenu_sat ) ;
    sensor = gui_optionMenu_string( optionMenu_sensor ) ;
    rev    = gui_TF_string( TF_REV ) ;
    dtkid  = gui_TF_string( TF_DTKID ) ;

    (void) sprintf( question, 
        "Delete DTK for\n\n   SAT: %s\nSENSOR: %s\n   REV: %s\n    ID: %s?",
        satname, sensor, rev, dtkid ) ;

    if (AskUser( widget, question, NO ) == YES)
    {
        char *strttime ;
        char *stoptime ;
        char *stationid ;

        /* get in-planning permission to delete the DTK */
        strttime    = gui_TF_string( TF_DTK_STRTTIME ) ;
        stoptime    = gui_TF_string( TF_DTK_STOPTIME ) ;
        stationid   = gui_optionMenu_string( optionMenu_dtkm_station_id ) ;
        if (get_timed_planning_permission( strttime, stoptime, stationid ) <= 0)
        {
            /* Couldn't get permission, message already popped up */
            XtFree( strttime ) ;
            XtFree( stoptime ) ;
            XtFree( stationid ) ;
            return ;
        }
        XtFree( strttime ) ;
        XtFree( stoptime ) ;
        XtFree( stationid ) ;

        satcode = get_satellite_code( satname ) ;

    /* Retrieve the record to be deleted*/

        (void) sprintf( where_clause, 
                       "\nwhere %s = '%s' and %s = '%s'\nand %s = %s and %s = %s",
                       APS_COL( DTK, DTK_SAT ), satcode,
                       APS_COL( DTK, DTK_SENSOR ), sensor,
                       APS_COL( DTK, DTK_REV ), rev,
                       APS_COL( DTK, DTK_DTKID ), dtkid) ;

    dtk_list = db_get_records(APS_dbproc, APS_TABLE( DTK ), 
                  where_clause, NULL, APS_CDEFS( DTK ), 
                  ALL_COLS ) ;

    /* if we ran into an error, inform the user and get out */
    if (dtk_list == NULL) 
    {
        sprintf(display_string, "Database query failed.") ; 
      
        popup_message(XmDIALOG_ERROR, 
                  "APS:ERROR", 
                  display_string, 
                  XtGrabNone);
        return ;
    }

    dtk_rec = (DB_RECORD **) FIRST(dtk_list, dtk_list_ptr) ;

        /* Now, see if this is a downlink */

        if( dtkm_is_a_downlink( dtk_rec ) == TRUE )
        {
            /* 
            -- Since it's a downlink, we have to collect the related observations
            -- to display to the user.
            */
          
        /* initialize linked list to obtain data-takes.  */
        obs_dtks = create_dyn_llist() ;
        
        /* 
        -- find the corresponding observation data-takes 
        -- for this downlink: 
        */ 
        ret_code = dtkm_dl2obs( dtk_rec, obs_dtks ) ;

        /* if we ran into an error, inform the user and get out */
        if ( ret_code < 0 )
        {
        DEL_LIST( obs_dtks ) ;
            sprintf(display_string, "Database query failed.") ; 
        
        popup_message(XmDIALOG_ERROR, 
                  "APS:ERROR", 
                  display_string, 
                  XtGrabNone);
        return ;
        }

            /* 
        -- If there were any related observations, 
        -- form the message for display. */

        /* 
        -- If the first record in the list is not NULL, there are related
        -- observations, and we may proceed.
        */
        if (dtk_rec = (DB_RECORD **) FIRST(obs_dtks, dtk_list_ptr))
        {
                sprintf(display_string, "The following is a list of datatakes related\nto the downlink being deleted:\n\n");
        
        i = 0 ; /* counter for number of observations */


        /* 
        -- Loop through and add all the observations to the
        -- display string.
        */
        while (dtk_rec)
                {
            i++ ;
            sprintf(temp_string, "Record %d:\n", i) ;
            strcat(display_string, temp_string) ;

                    sprintf(temp_string, "SAT: %s\nSENSOR: %s\nREV: %ld\nDTKID: %d\n\n",
                CAST_DTK_SAT dtk_rec[DTK_SAT],
                CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
                CAST_DTK_REV dtk_rec[DTK_REV],
                CAST_DTK_DTKID dtk_rec[DTK_DTKID]) ;

            strcat(display_string, temp_string) ;
            
            dtk_rec = (DB_RECORD **) NEXT(dtk_list, dtk_list_ptr) ;
        }

        popup_message( XmDIALOG_WARNING, "APS:WARNING", display_string,
                  XtGrabNone ) ;
        }
    }

        /* Now delete this from the DTK table and the DL2OBS table */

    rev_int = atoi(rev) ;
    dtkid_int = atoi(dtkid) ;
        ret_code = dtkm_delete_dtk(APS_dbproc, 
                   satcode, 
                   sensor, 
                   rev_int, 
                   dtkid_int) ;

    /* Inform the user of any errors */
        if (ret_code < 0)
        {
            sprintf(display_string, "An error has occurred while attempting to delete the datatake:\n");

            switch (ret_code)
            {
            case DTKM_ERROR_ATTEMPT_TO_DELETE_NONEXISTING_DTK:
              strcat(display_string, "DTKM_ERROR_ATTEMPT_TO_DELETE_NONEXISTING_DTK\n");
              break ;

            case DTKM_ERROR_ATTEMPTING_TO_DELETE_DTK:
              strcat(display_string, "DTKM_ERROR_ATTEMPTING_TO_DELETE_DTK\n");
              break ;

            case DTKM_ERROR_GT_1_DTK_DELETED:
              strcat(display_string, "DTKM_ERROR_GT_1_DTK_DELETED\n");
              break ;

            case DTKM_ERROR_NULL_RECORD:
              strcat(display_string, "DTKM_ERROR_NULL_RECORD\n");
              break ;

            case DTKM_ERROR_DB_DELETE_FAILED_ON_DL2OBS_RELATION:
              strcat(display_string, "DTKM_ERROR_DB_DELETE_FAILED_ON_DL2OBS_RELATION\n");
              break ;
            }
        }
        else
        {
            /* refresh the DTK list */
            cb_show_dtk_records( widget, (XtPointer) scrolledList_DTKS, NULL ) ;
            cb_set_dtkmanager_editability( widget, DTK_RESET, NULL ) ;

            /*
            -- 2/96: Intentionally NOT clearing out the deleted record's
            -- values from the gui form
            --
            -- cb_clear_dtkmanager_form( widget, NULL, NULL ) ;
            */
        }
    }

    XtFree( satname ) ;
    XtFree( sensor ) ;
    XtFree( rev ) ;
    XtFree( dtkid ) ;

END_CONTEXT
}


/*==============================================================================
Function:       dtk_report_done

Description:    

Parameters:     PROCESS_INFO *process

Returns:        none

Creator:        Ron Green

Creation Date:  12/06/1994

Notes:      
==============================================================================*/
/* ARGSUSED1 */
static void
dtk_report_done( process, arg_UNUSED )
    PROCESS_INFO *process ;
    void *arg_UNUSED ;
{
    switch (process->exit_status)
    {
    case APS_EXIT_OK :
        (void) sprintf( display_string,
                "DTK Report:\n\n Completed Successfully\n" ) ;
        popup_message( XmDIALOG_INFORMATION, "APS:INFORMATION", display_string,
                XtGrabNone );
        break ;
    case APS_EXIT_ERROR :
        (void) sprintf( display_string, "DTK Report:\n\n Unsuccessful Run" ) ;
        popup_message( XmDIALOG_ERROR, "APS:ERROR",
                display_string, XtGrabNone );
        break ;
    case APSGUI_EXIT_COREDUMP : /* process core dumped */
        (void) sprintf( display_string,
                "DTK Report:\n\n Signal Caught CORE DUMPED" ) ;
        popup_message( XmDIALOG_ERROR, "APS:ERROR",
                display_string, XtGrabNone );
        break ;
    default :   /* process caught a signal, but no core dump */
        (void) sprintf( display_string,
                "DTK Report:\n\n SIGNAL Caught (signal = %d)",
                -(process->exit_status) ) ;
        popup_message( XmDIALOG_ERROR, "APS:ERROR",
                display_string, XtGrabNone );
        break ;
    }
}   



/*==============================================================================
Function:       cb_print_dtks

Description:    print dtks to printer

Parameters:     Standard X Callback Parameters

Returns:        none

Creator:        Ron Green

Creation Date:  11/07/1994

Notes:      
==============================================================================*/
void
cb_print_dtks( Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( DTK_manager )

    PROCESS_INFO *process ;
    DB_RECORD **dtk_rec ;
    char command[255] ;
    char report_option[255] ;
    char *search_str ;
    char *file ;
    int *pos_list, pos_cnt ;
    int status ;
    int print_type ;

    print_type = (int) client_data ;

    /* get the search criteria (where clause) to pass to the report */

    switch (print_type)
    {
    case PRINT_DTKS_ALL :  
    case PRINT_DTKS_ALL_TO_FILE : 
        (void) sprintf( where_clause, "where %s > 0",
                APS_COL( DTK, DTK_DTKID ) ) ;
    break ;

    case PRINT_DTKS_SELECTED :  
    case PRINT_DTKS_SELECTED_TO_FILE :  

        if (!XmListGetSelectedPos( scrolledList_DTKS, &pos_list, &pos_cnt ))
        {
            /*
            -- this code should never execute...
            -- since it is also checked in cb_set_print_dtk
            */
            (void) sprintf( display_string, "No Data Take is Selected" ) ;
            popup_message( XmDIALOG_ERROR, "APS:ERROR", 
                display_string, XtGrabNone ) ;
            return ;
        }
    
        dtk_rec = db_nth_record( dtks, pos_list[0] ) ;
        XtFree( (char *) pos_list ) ;

        (void) sprintf( where_clause,
            "where %s = '%s' and %s = '%s' and %s = %ld and %s = %d",
            APS_COL( DTK, DTK_SAT ), CAST_DTK_SAT dtk_rec[DTK_SAT],
            APS_COL( DTK, DTK_SENSOR ), CAST_DTK_SENSOR dtk_rec[DTK_SENSOR],
            APS_COL( DTK, DTK_REV ), CAST_DTK_REV dtk_rec[DTK_REV],
            APS_COL( DTK, DTK_DTKID ), CAST_DTK_DTKID dtk_rec[DTK_DTKID] ) ;
        break ;

    case PRINT_DTKS_CURRENT :  
    case PRINT_DTKS_CURRENT_TO_FILE : 
        search_str = XmTextFieldGetString( TF_DTK_searchclause ) ;
        if (strlen( search_str ) == 0)
            (void) sprintf( where_clause, "%s", blank_str ) ;
        else
            (void) sprintf( where_clause, "%s", search_str ) ;
        XtFree( search_str ) ;
        break ;

    default :
        (void) sprintf( display_string, 
            "Incorrect DTK TYPE %d passed to\ncb_print_dtks()",
            print_type ) ;
        gui_aps_internal_error( XmDIALOG_ERROR,
            __FILE__, __LINE__, display_string ) ;
        return ;
    }

    /* if it is to be written to a file get the filename */
    if (print_type == PRINT_DTKS_SELECTED_TO_FILE
    ||  print_type == PRINT_DTKS_CURRENT_TO_FILE
    ||  print_type == PRINT_DTKS_ALL_TO_FILE)
    {
        file = gui_filebox_filename(
            widget, (XmFileSelectionBoxCallbackStruct *) cbs) ;
        if (!file)
            return ;

        /* check for file replacement */
        if (gui_ask_file_replacement( file ) == NO)
        {
            XtFree( file ) ;
            return ;
        }
        (void) sprintf( report_option, "-o %s", file ) ;
        XtFree( file ) ;
    }
    else /* report is to be written to printer */
        (void) sprintf( report_option, "-p" ) ;
    
    (void) sprintf( command, 
        "runrw dtk_report -U %s -P %s -V where_clause \"%s\" %s",
        userid, password, 
        where_clause,
        report_option ) ;

    process = (PROCESS_INFO *) create_process( command, &status, TRUE, NULL,
        NULL, NULL, dtk_report_done, NULL ) ;

    if (!process || start_process( process ))
    {
        (void) sprintf( display_string, 
            "Can't run process to print the dtks\n Too many processes running?"
             ) ;
        popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone ) ;
        if (process)
            destroy_process( process ) ;
        return ;
    }

END_CONTEXT
}



/*==============================================================================
Function:       cb_set_print_dtks_to_file_cb

Description:    Sets the filebox to get a file to print the dtk report
                to.   After the file is selected (OK) the cb_print_dtk
                callback is executed.

Parameters:     Standard X Callback parameters

Returns:        None

Creator:        Ron Green

Creation Date:  12/06/1994

Notes:      
==============================================================================*/
/* ARGSUSED2 */
void
cb_set_print_dtks_to_file_cb( Widget widget_UNUSED, XtPointer client_data,
    XtPointer call_data_UNUSED)
{
    int print_type ;
    XmString dirmask ;
    int *pos_list ;
    int pos_cnt ;
    char *aps_fullpath_dirmask ;

BEGIN_CONTEXT( DTK_manager )
 
    aps_fullpath_dirmask = aps_fullpath( APS_REPORTS, "*.rpt" ) ;
 
    dirmask = XmStringCreateLocalized( aps_fullpath_dirmask ) ;
    XmFileSelectionDoSearch( filebox, dirmask ) ;
 
    XmStringFree( dirmask ) ;
    free( aps_fullpath_dirmask ) ;
 
    print_type = (int) client_data ;
    
    if (print_type == PRINT_DTKS_SELECTED
    ||  print_type == PRINT_DTKS_SELECTED_TO_FILE)
    {
        if (!XmListGetSelectedPos( scrolledList_DTKS, &pos_list, &pos_cnt ))
        {
            (void) sprintf( display_string, "No Data Take is Selected" ) ;
            popup_message( XmDIALOG_ERROR, "APS:ERROR",
                display_string, XtGrabNone ) ;
            return ;
        }
        XtFree( (char *) pos_list ) ;
    }

    /* remove any currently installed callbacks for the filebox */
    XtRemoveAllCallbacks( filebox, XmNokCallback ) ;

    XtVaSetValues( filebox, 
        RES_CONVERT( XmNdialogTitle,    "APS File: Save DTK Report" ) ,
        NULL ) ;

    /* now install the print_dtks... callback passing the type of print to do */
    XtAddCallback( filebox, XmNokCallback, cb_print_dtks, client_data ) ;

    /* now bring up the filebox */
    XtPopup( XtParent( filebox ), XtGrabExclusive ) ;
END_CONTEXT
}



/*==============================================================================
Function:       cb_set_dtk_status_menus

Description:    Makes manageable the dtk status menus for the selected
                satellite.

Parameters:     Standard X Callback parameters

Returns:        None

Creator:        Ron Green

Creation Date:  12/06/1994

Notes:      
==============================================================================*/
void
cb_set_dtk_status_menus( Widget widget, XtPointer client_data_UNUSED,
     XmRowColumnCallbackStruct *cbs )
{
    char    *sat;

BEGIN_CONTEXT( widget )

    sat = XtName( cbs->widget ) ;

    if (*sat == 'A')    /* if is ADEOS, unmanage some of the menus */
    {
        XtManageChild( subMenu_dtk_status_INV ) ;

        XtUnmanageChild( subMenu_dtk_status_QUE ) ;
        XtUnmanageChild( subMenu_dtk_status_SUB ) ;
    }
    else
    {
        XtUnmanageChild( subMenu_dtk_status_INV ) ;

        XtManageChild( subMenu_dtk_status_QUE ) ;
        XtManageChild( subMenu_dtk_status_SUB ) ;
    }

END_CONTEXT

    return;
}
