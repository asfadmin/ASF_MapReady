#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:       cb_apsmenu.c

Description:    Callbacks for the aps menu widget

External Functions Defined:
                void cb_create_aps_interfaces
                void cb_start_mapper
                void cb_get_pixmap
    
File Scope Functions:
    
External Variables Defined:
    
File Scope Variables:
    
Notes:          

==============================================================================*/
#pragma ident   "@(#)cb_apsmenu.c	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_apsmenu.c"

#include <stdio.h>
#include <stdlib.h>

#include <Xm/FileSB.h>  /* for gui_utils.h: XmFileSelectionBoxCallbackStruct */
#include <X11/StringDefs.h>
#include <X11/Intrinsic.h>
#include <X11/Xatom.h>

#include "db_sybint.h"
#include "aps.h"
#include "aps_defs.h"
#include "aps_extern.h"
#include "aps_exe_names.h"
#include "apspath.h"
#include "gui_utils.h"

#define CONTEXT_MACRO_ACCESS 1
#include "vc_apsmenu.h"
#undef CONTEXT_MACRO_ACCESS

#include "vc_cnomorb.h"
#include "vc_cnomcov.h"
#include "vc_sortform.h"
#include "vc_searchform.h"
#include "vc_cdtakeopps.h"
#include "vc_crtapsfile.h"
#include "vc_muinterval.h"
#include "vc_permstatus.h"
#include "vc_prcapsfile.h"
#include "vc_darmanager.h"
#include "vc_dtkmanager.h"
#include "vc_rgsdntimes.h"
#include "vc_antdntimes.h"
#include "vc_fileselectionbox.h"
#include "vc_fileviewer.h"
#include "vc_conrndup.h"
#include "vc_apswoscompare.h"
#include "vc_phaseselect.h"

extern Widget cnomorb_form ;
extern Widget cnomcov_form ;
extern Widget sortform ;
extern Widget searchform ;
extern Widget cdtakeopps_form ;
extern Widget apsfilegen_form ;
extern Widget apsfileproc_form ;
extern Widget DAR_manager ;
extern Widget DTK_manager ;
extern Widget DownTime_manager ;
extern Widget AntennaDownTime_manager ;
extern Widget PermStatus_viewer ;
extern Widget PermStatusInterval_popup ;
extern Widget con_roundup_form ;
extern Widget apswoscompare_form ;
extern Widget apsphaseselect_form ;
extern Widget filebox ;
extern Widget File_viewer ;

extern Widget mainIface ;

#include "dapps_defs.h"
#include "subprocess.h"

#define APS_LOGO        "APSLogo.xpm"   /* aps logo pixmap filename */
#define MAPPER_TITLE    "Mapper_Menu"   /* title for mapper menu window */

/* declare a global Dbase Process for APS queries */
DBPROCESS *APS_dbproc ;

#define BEGIN_CONTEXT(widget) \
    _UxCAPSMainMenu          *UxSaveCtx; \
    UxSaveCtx = UxAPSMainMenuContext; \
    UxAPSMainMenuContext = \
            (_UxCAPSMainMenu *) UxGetContext( widget ); \
    {

#define END_CONTEXT \
    } \
    UxAPSMainMenuContext = UxSaveCtx;


/* ARGSUSED0 */
void
cb_create_aps_interfaces(widget, client_data, cbs)
    Widget widget ;
    XtPointer client_data, cbs ;
{
    int status ;
    char *dbname ;

    dbname = getenv( APSDB_ENVVAR ) ;

    /* logon to the APS database */
    APS_dbproc = db_open(
        dbname,     /* db name   */
        "APS GUI",  /* prog name */
        userid,     /* db user   */
        password,   /* db passwd */
        NULL,       /* msg handl */
        NULL,       /* err handl */
        &status) ;  /* db status */

    if (status != DB_OPEN_OK) 
    {
        db_open_errs(status, dbname, userid) ;
        exit( APS_EXIT_OK ) ;
    }

    sortform                    = create_SortForm( mainIface ) ;
    searchform                  = create_SearchForm( mainIface ) ;
    cnomorb_form                = create_CreateNominalOrbit( mainIface ) ;
    cnomcov_form                = create_CreateNominalCoverage( mainIface ) ;
    apsfilegen_form             = create_APSFileGeneration( mainIface ) ;
    apsfileproc_form            = create_APSFileProcessing( mainIface ) ;
    cdtakeopps_form             = create_CreateDatatakeOpps( mainIface ) ;
    DAR_manager                 = create_DARManager( mainIface ) ;
    DTK_manager                 = create_DTKManager( mainIface ) ;
    DownTime_manager            = create_DownTimeManager( mainIface ) ;
    AntennaDownTime_manager     = create_AntennaDownTimeManager( mainIface ) ;
    PermStatus_viewer           = create_MUPermissionStatus( mainIface ) ;
    PermStatusInterval_popup    =
            create_MUInterval_dialogShell( PermStatus_viewer ) ;
    con_roundup_form            = create_ConRoundupForm( mainIface ) ;
    apswoscompare_form          = create_APSWOSCompare( mainIface ) ;
    apsphaseselect_form	    	= create_APSPhaseSelection( mainIface ) ;
    filebox                     = create_APSFileSelection( mainIface ) ;
    File_viewer                 = create_FileViewer( mainIface ) ;

    /*
    -- load the interfaces into a table for use when all
    -- have to be modified .e.g. mod cursors...
    */

    if ( gui_AddToTopLevelInterfaces(sortform ) <= 0 ||
        gui_AddToTopLevelInterfaces( searchform ) <= 0 ||
        gui_AddToTopLevelInterfaces( cnomorb_form ) <= 0 ||
        gui_AddToTopLevelInterfaces( cnomcov_form ) <= 0 ||
        gui_AddToTopLevelInterfaces( apsfilegen_form ) <= 0 ||
        gui_AddToTopLevelInterfaces( apsfileproc_form ) <= 0 ||
        gui_AddToTopLevelInterfaces( cdtakeopps_form ) <= 0 ||
        gui_AddToTopLevelInterfaces( DAR_manager ) <= 0 ||
        gui_AddToTopLevelInterfaces( DTK_manager ) <= 0 ||
        gui_AddToTopLevelInterfaces( DownTime_manager ) <= 0 ||
        gui_AddToTopLevelInterfaces( AntennaDownTime_manager ) <= 0 ||
        gui_AddToTopLevelInterfaces( PermStatus_viewer ) <= 0 ||
        gui_AddToTopLevelInterfaces( PermStatusInterval_popup ) <= 0 ||
        gui_AddToTopLevelInterfaces( con_roundup_form ) <= 0 ||
        gui_AddToTopLevelInterfaces( apswoscompare_form ) <= 0 ||
        gui_AddToTopLevelInterfaces( filebox ) <= 0 ||
        gui_AddToTopLevelInterfaces( File_viewer ) <= 0)
    {
        aps_internal_error( stderr, __FILE__, __LINE__,
            "\n*** EXITING ***, can't start the Main Menu (subwindows)\n\
(array in gui_AddToTopLevelInterfaces() is too small)\n" ) ;
        exit( APS_EXIT_ERROR ) ;
    }
}


/* ARGSUSED0 */
void
cb_start_mapper(widget, client_data, cbs)
    Widget widget ;
    XtPointer client_data, cbs ;
{
    PROCESS_INFO *process ;
    char command_string[255] ;
    int status ;

    (void) sprintf(command_string, 
        "xterm -n %s -title %s -sl 1024 -ls -sb -geometry 60x25+575+490 -fn 9x15bold -e %s -U %s -P %s", 
        MAPPER_TITLE, MAPPER_TITLE, MAPPER_CMD, userid, password) ;

    process = (PROCESS_INFO *) create_process(command_string, &status, TRUE,
        NULL, NULL, NULL, NULL, NULL) ;

    if (!process)
        (void) printf("Unable to start process....\n") ;
    else if (start_process(process))
        destroy_process( process ) ;
}


/*==============================================================================
Function:       cb_get_pixmap

Description:    Callback to get the pixmap to display on the menu widget

Parameters:     Standard X Parameters

Returns:        

Creator:        Teresa McKillop

Creation Date:  03/14/96

Notes:      
==============================================================================*/
/* ARGSUSED1 */
void
cb_get_pixmap( Widget widget, XtPointer client_data, XtPointer cbs )
{
BEGIN_CONTEXT( widget )
    char                *pixmapFileName = NULL;

    /*
    ** Set the APS logo if a pixmap file can be found.
    */
    pixmapFileName = aps_fullpath(APS_CONFIG_XRESOURCE, APS_LOGO);
    if (pixmapFileName != NULL)
    {
        XtVaSetValues( Pixmap_applWorkArea,
                       RES_CONVERT(XmNlabelPixmap, pixmapFileName),
                       NULL );
        /* free the allocated memory */
        free( pixmapFileName ) ;
    }

END_CONTEXT
    return ;
}
