#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	cb_cdtakeopps.c

Description:	

External Functions:
			cb_display_hypo_sites
			cb_display_dar_sites
			cb_update_cdtakeopps_form
			cb_do_create_dtk_opps
			cb_show_dar_relations
			cb_add_HypoSite
			cb_delete_HypoSite
			cb_edit_new_HypoSite
			cb_cancel_edit_new_HypoSite
	
Static Functions:
			get_dar_list
			get_site_list
			check_analysis_period
			set_HypoSite_edit_fields
			set_cdtakeopps_form_sensitivity
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:

==============================================================================*/
#pragma ident	"@(#)cb_cdtakeopps.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_cdtakeopps.c"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <malloc.h>

#include "db_sybint.h"

#include "dapps_defs.h"
#include "aps_defs.h"
#include "aps_db_table.h"
#include "db_dar.h"
#include "db_site.h"
#include "db_satsensor.h"
#include "db_station.h"

#include "aps_extern.h"
#include "aps_exe_names.h"
#include "gui_defs.h"

#include "nmalloc.h"

#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>

#include "UxXt.h"

#define CONTEXT_MACRO_ACCESS 1
#include "vc_cdtakeopps.h"
#undef CONTEXT_MACRO_ACCESS
#include "cb_cdtakeopps.h"
#include "cb_datetime.h"
#include "timeconv.h"

#include "gui_utils.h"

#include "satmenus.h"
#include "subprocess.h"

extern void		popup_message() ;
extern int		sscvcheckq() ;

extern Widget	cdtakeopps_form ;
extern char		display_string[] ;

static llist	*dars ;
static llist	*sites ;

char			command_string[255] ;

static char		*blank = " " ;	

#define BEGIN_CONTEXT(widget) \
	_UxCCreateDatatakeOpps		*UxSaveCtx; \
	UxSaveCtx           = UxCreateDatatakeOppsContext; \
	UxCreateDatatakeOppsContext = \
			(_UxCCreateDatatakeOpps *) UxGetContext( widget ) ; \
	{
 
#define END_CONTEXT \
	} \
	UxCreateDatatakeOppsContext = UxSaveCtx;



/*==============================================================================
Function:		get_dar_list
Description:	
Parameters:		
Returns:		
Creator:		Teresa McKillop
Creation Date:	01/18/96
Notes:		
==============================================================================*/
static llist *
get_dar_list()
{
	TimeoutCursors(True, True, NULL) ;

	if (dars)
		DEL_LIST( dars ) ;

	(void) sprintf(orderby_cols, "%s", APS_COL(DAR, DAR_DARID)) ;
	(void) sprintf(where_clause, "where %s > 0", APS_COL(DAR, DAR_DARID)) ;	

	dars = db_get_records(APS_dbproc, APS_TABLE(DAR), where_clause, 
		orderby_cols, APS_CDEFS(DAR), ALL_COLS) ;
	if (dars == NULL)
	{
		(void) sprintf(display_string,
			"APS DB ERROR: can't access DataBase\n") ;
		popup_message(XmDIALOG_ERROR, "APS:DB ERROR", display_string,
			XtGrabNone) ;
	}

	TimeoutCursors(False, False, NULL) ;

	return (dars);
}



/*==============================================================================
Function:		get_site_list
Description:	
Parameters:		
Returns:		
Creator:		Teresa McKillop
Creation Date:	01/18/96
Notes:			Caller must free the returned list by calling DEL_LIST.
==============================================================================*/
static llist *
get_site_list()
{
	TimeoutCursors(True, True, NULL) ;

	if (sites)
		DEL_LIST(sites) ;

	(void) sprintf(orderby_cols, "%s", APS_COL(SITE, SITE_SITENAME)) ;

	sites = db_get_records(APS_dbproc, APS_TABLE(SITE), NULL, orderby_cols, 
		APS_CDEFS(SITE), ALL_COLS) ;
	if (sites == NULL)
	{
		(void) sprintf(display_string,
			"APS DB ERROR: can't access DataBase\n") ;
		popup_message(XmDIALOG_ERROR, "APS:DB ERROR", display_string,
			XtGrabNone) ;
	}

	TimeoutCursors(False, False, NULL) ;

	return (sites);
}



/*==============================================================================
Function:		check_analysis_period
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
static void
check_analysis_period( Widget widget )
{
BEGIN_CONTEXT( widget )

	double		et_start ;
	double		et_stop  ;
	double		et_total ;
	char		*starttime_str ;
	char		*stoptime_str ;

	/* Get the start and end times */
	starttime_str = gui_TF_string( TF_dtkopps_start ) ;
	stoptime_str  = gui_TF_string( TF_dtkopps_end ) ;
	if (*starttime_str != STREND && *stoptime_str != STREND)
	{
		(void) tc_asf2et( starttime_str, &et_start ) ;
		(void) tc_asf2et( stoptime_str, &et_stop ) ;

		et_total = et_stop - et_start ;
		(void) sprintf(display_string, "%8.2f", et_total) ;
		XmTextFieldSetString( TF_dtkopps_total_days, display_string ) ;
	}
	else
		XmTextFieldSetString( TF_dtkopps_total_days, EMPTY_STR ) ;

	XtFree( starttime_str );
	XtFree( starttime_str );

END_CONTEXT
}



/*==============================================================================
Function:		

Description:	

Parameters:		

Returns:     	

Creator:		Ron Green

Creation Date:	XX/XX/1994

Notes:		
==============================================================================*/
static void
set_HypoSite_edit_fields(Boolean editable)
{
BEGIN_CONTEXT( cdtakeopps_form )

	gui_setEditable( TF_SITENAME,      AG_TEXTFIELD, editable ) ;
	gui_setEditable( TF_COMMENTS,      AG_TEXTFIELD, editable ) ;

	gui_setEditable( TF_NW_LAT,        AG_TEXTFIELD, editable ) ;
	gui_setEditable( TF_NW_LON,        AG_TEXTFIELD, editable ) ;

	gui_setEditable( TF_NE_LAT,        AG_TEXTFIELD, editable ) ;
	gui_setEditable( TF_NE_LON,        AG_TEXTFIELD, editable ) ;

	gui_setEditable( TF_SW_LAT,        AG_TEXTFIELD, editable ) ;
	gui_setEditable( TF_SW_LON,        AG_TEXTFIELD, editable ) ;

	gui_setEditable( TF_SE_LAT,        AG_TEXTFIELD, editable ) ;
	gui_setEditable( TF_SE_LON,        AG_TEXTFIELD, editable ) ;
 
	gui_setEditable( TF_center_lat,    AG_TEXTFIELD, editable ) ;
	gui_setEditable( TF_center_lon,    AG_TEXTFIELD, editable ) ;
	gui_setEditable( textField_radius, AG_TEXTFIELD, editable ) ;

	/* 
	-- set the sensitivity of the Create Button to the
	-- opposite of the function we're performing
	-- if we are editing (TRUE) then we want the create
	-- button to be insensitive (FALSE) if we are finished
	-- editing then we want it to be sensitive (TRUE)
	*/
	XtSetSensitive(menuCreateHypoSite, !editable) ;
	XtSetSensitive(pushButton_create_dtkopps, !editable) ;

END_CONTEXT
}



/*==============================================================================
Function:		set_cdtakeopps_form_sensitivity
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/

static void
set_cdtakeopps_form_sensitivity(process, sensitivity)
	PROCESS_INFO *process ;
	Boolean sensitivity ;
{
BEGIN_CONTEXT( cdtakeopps_form )

	if (process)
	{
		switch (process->exit_status)
		{
		case APS_EXIT_OK :
			(void) sprintf(display_string,
					"Create DTK Opportunities:\n\n Completed Successfully\n") ;
			popup_message(XmDIALOG_INFORMATION, "APS:INFORMATION",
					display_string, XtGrabNone);
			break ;
		case APS_EXIT_ERROR :
			(void) sprintf(display_string,
				"Create DTK Opportunities:\n\n Unsuccessful Run") ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
				display_string, XtGrabNone);
			break ;
		case APSGUI_EXIT_COREDUMP : /* process core dumped */
			(void) sprintf( display_string,
					"Create DTK Opportunities:\n\n Signal Caught CORE DUMPED" ) ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
					display_string, XtGrabNone);
			break ;
		default :	/* process caught a signal, but no core dump */
			(void) sprintf( display_string,
					"Create DTK Opportunities:\n\n SIGNAL Caught (signal = %d)",
					-(process->exit_status) ) ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
					display_string, XtGrabNone);
			break ;
		}
	}


	XtSetSensitive(pushButton_create_dtkopps, sensitivity) ;

	XtSetSensitive(scrolledList_sites, sensitivity) ;
	XtSetSensitive(optionMenu_cdtk_sat, sensitivity) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_delete_HypoSite
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	08/29/1994
Notes:		
==============================================================================*/
/* ARGSUSED2 */
void
cb_delete_HypoSite(Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( widget )

	char	*sitename ;
	char	*tmpname ;
	int		nrecs ;

	tmpname = XmTextFieldGetString(TF_SITENAME) ;
	db_quote_doubler( tmpname, &sitename ) ;
	XtFree( tmpname ) ;

	if (!gui_XmList_selected_item(scrolledList_sites))
	{
		(void) sprintf(display_string, "No site is selected for deletion\n") ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone) ;
		free( sitename );
		return ;
	}

	(void) sprintf(question, "Delete Hypothetical Site\n\n%s", sitename) ;	

	if (AskUser(widget, question, NO) == NO)
	{
		free( sitename );
		return ;
	}

	(void) sprintf(where_clause, "where %s = '%s'", 
		APS_COL(SITE, SITE_SITENAME), sitename) ;

	nrecs = db_delete_records(APS_dbproc, APS_TABLE(SITE), where_clause) ;

	if (nrecs < 1)
	{
		(void) sprintf( display_string,
			"APS DB ERROR:\n Can't delete the record\n Fix DB and retry" ) ;
		popup_message(XmDIALOG_ERROR, "APS:DB ERROR",
			display_string, XtGrabNone) ;
		free( sitename );
		return ;
	}

	/* 
	-- refresh the window list: retrieve and display the new sites
	*/

	(void) get_site_list(); /* ignore error - want display_hypo to clear gui */

	cb_display_hypo_sites(widget, client_data, NULL) ;

	free( sitename );

END_CONTEXT
	return;
}



/*==============================================================================
Function:		cb_add_HypoSite
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	08/22/1994
Notes:		
==============================================================================*/
/* ARGSUSED2 */
void
cb_add_HypoSite(Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( widget )

	char *sitename ;
	char *comments ;
	char shape, *shapestr ;

	float nw_lat, nw_lon ;  /* also used as center_lat, center_lon */
	float radius ;

	float sw_lat, sw_lon ;
		
	float ne_lat, ne_lon ;
	float se_lat, se_lon ;

	int status ;

	char	*tmpStr;

	/* TO DO use the db_ routines to complete the add these records */
	char col_names[1024] ;
	char col_values[1024] ;

	shapestr = gui_TF_string( TF_SHAPE ) ; 
	if (strstr(shapestr, "QUAD"))
		shape = QUAD_CHAR ;
	else /* assume circle */
		shape = POINT_CHAR ;	

	tmpStr   = gui_TF_string( TF_COMMENTS ) ;
	if (db_quote_doubler( tmpStr, &comments ) < 0)
	{
		(void) sprintf( display_string,
			"Out of Memory\n Too many processes running?" );
		popup_message(XmDIALOG_ERROR, "APS:ERROR", 
			display_string, XtGrabNone) ;
		XtFree( shapestr );
		XtFree( tmpStr ) ;
		return;
	}
	XtFree( tmpStr ) ;

	tmpStr = gui_TF_string( TF_SITENAME ) ;
	if (db_quote_doubler( tmpStr, &sitename ) < 0)
	{
		(void) sprintf( display_string,
			"Out of Memory\n Too many processes running?" );
		popup_message(XmDIALOG_ERROR, "APS:ERROR", 
			display_string, XtGrabNone) ;
		XtFree( shapestr ) ;
		free( comments ) ;
		XtFree( tmpStr ) ;
		return;
	}
	XtFree( tmpStr ) ;

	/* check for empty sitenames */
	if (*sitename == STREND)
	{
		(void) sprintf(display_string, "Invalid Sitename: ''") ;
		popup_message(XmDIALOG_ERROR, "APS:SITE ERROR", 
			display_string, XtGrabNone) ;
		XtFree( shapestr );
		free( comments );
		free( sitename );
		return ;
	}

	/* check for a duplicate sitename */
	(void) sprintf(where_clause, "where %s = '%s'", 
		APS_COL(SITE, SITE_SITENAME), sitename) ;

#ifdef DEBUG
	(void) printf("WHERE: %s\n", where_clause) ;
#endif

	status = db_num_records(APS_dbproc, APS_TABLE(SITE), where_clause) ;
	if (status > 0)	
	{
		(void) sprintf(display_string, 
			"Duplicate:\n Sitename already in relation\n Use another name") ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone) ;
		XtFree( shapestr );
		free( comments );
		free( sitename );
		return ;
	}
	else if (status == -1)  /* db error */
	{
		(void) sprintf(display_string, "Syntax Error in Sitename") ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone) ;
		XtFree( shapestr );
		free( comments );
		free( sitename );
		return ;
	}

	if (strstr(shapestr, "QUAD"))
	{
		shape = QUAD_CHAR ;

		nw_lat = atof(tmpStr = XmTextFieldGetString(TF_NW_LAT)) ; 
			XtFree( tmpStr ) ;
		nw_lon = atof(tmpStr = XmTextFieldGetString(TF_NW_LON)) ;
			XtFree( tmpStr ) ;
		sw_lat = atof(tmpStr = XmTextFieldGetString(TF_SW_LAT)) ; 
			XtFree( tmpStr ) ;
		sw_lon = atof(tmpStr = XmTextFieldGetString(TF_SW_LON)) ;
			XtFree( tmpStr ) ;
		
		ne_lat = atof(tmpStr = XmTextFieldGetString(TF_NE_LAT)) ; 
			XtFree( tmpStr ) ;
		ne_lon = atof(tmpStr = XmTextFieldGetString(TF_NE_LON)) ;
			XtFree( tmpStr ) ;
		se_lat = atof(tmpStr = XmTextFieldGetString(TF_SE_LAT)) ; 
			XtFree( tmpStr ) ;
		se_lon = atof(tmpStr = XmTextFieldGetString(TF_SE_LON)) ;
			XtFree( tmpStr ) ;

		radius =  0 ;

		/* check validity of entries */

#define INVALID_LAT(x) \
	(x) < -90.00 || (x) > 90.00
	
#define INVALID_LON(x) \
	(x) < -180.00 || (x) > 180.00

		status = FALSE ;  /* use status for error checking */	
		if (INVALID_LAT(nw_lat)) 
		{
			(void) sprintf(display_string, 
				"Latitude of NW must be\n-90.00 <= lat <= 90.00") ;
			status = TRUE ;
		}

		if (INVALID_LON(nw_lon)) 
		{
			(void) sprintf(display_string, 
				"Longitude of NW must be\n-180.00 <= lat <= 180.00") ;
			status = TRUE ;
		}

		if (INVALID_LAT(sw_lat)) 
		{
			(void) sprintf(display_string, 
				"Latitude of SW must be\n-90.00 <= lat <= 90.00") ;
			status = TRUE ;
		}

		if (INVALID_LON(sw_lon)) 
		{
			(void) sprintf(display_string, 
				"Longitude of SW must be\n-180.00 <= lat <= 180.00") ;
			status = TRUE ;
		}

		if (INVALID_LAT(ne_lat))
		{
			(void) sprintf(display_string, 
				"Latitude of NE must be\n-90.00 <= lat <= 90.00") ;
			status = TRUE ;
		}

		if (INVALID_LON(ne_lon))
		{
			(void) sprintf(display_string, 
				"Longitude of NE must be\n-180.00 <= lat <= 180.00") ;
			status = TRUE ;
		}

		if (INVALID_LAT(se_lat))
		{

			(void) sprintf(display_string, 
				"Latitude of SE must be\n-90.00 <= lat <= 90.00") ;
			status = TRUE ;
		}

		if (INVALID_LON(se_lon))
		{
			(void) sprintf(display_string, 
				"Longitude of SE must be\n-180.00 <= lat <= 180.00") ;
			status = TRUE ;
		}

		if (status) /* if an error occurred */
		{
			popup_message(XmDIALOG_ERROR, "APS:ERROR",
				display_string, XtGrabNone) ;
			XtFree( shapestr );
			free( comments );
			free( sitename );
			return ;
		}

		status = sscvcheckq(  
			nw_lat, nw_lon,
			ne_lat, ne_lon,
			se_lat, se_lon,
			sw_lat, sw_lon) ;

		if (status == 1)
		{
			(void) sprintf(display_string,
				"Quadrilateral has invalid angles...") ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR",
				display_string, XtGrabNone) ;
			XtFree( shapestr );
			free( comments );
			free( sitename );
			return ;
		}
	}
	else
	{
		shape = POINT_CHAR ; /* circle */

		/* center_lat = */  
			nw_lat = atof(tmpStr = XmTextFieldGetString(TF_center_lat)) ;
				XtFree( tmpStr ) ;
		/* center_lon = */  
			nw_lon = atof(tmpStr = XmTextFieldGetString(TF_center_lon)) ;
				XtFree( tmpStr ) ;

		radius = atof(tmpStr = XmTextFieldGetString(textField_radius)) ;
			XtFree( tmpStr ) ;

		/* check validity of entries */

		if (radius > 9999)
		{
			(void) sprintf(display_string,
				"Radius value of %f\nis too large\nRadius must be less than 10,000km\n",
				radius) ;
			
			popup_message(XmDIALOG_ERROR, "APS:ERROR",
				display_string, XtGrabNone) ;
			XtFree( shapestr );
			free( comments );
			free( sitename );
			return ;
		}

    	/*  center lat:  must be >= -90.0 and <= 90.0   */
		if (nw_lat > 90.00 || nw_lat < -90.00)
		{
			(void) sprintf(display_string, 
				"Latitude of center must be\n-90.00 <= lat <= 90.00") ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR",
				display_string, XtGrabNone) ;
			XtFree( shapestr );
			free( comments );
			free( sitename );
			return ;
		}

    	/*  center lon:  must be >= -180.0 and <= 180.0   */
		if (nw_lat > 180.00 || nw_lat < -180.00)
		{
			(void) sprintf(display_string, 
				"Longitude of center must be\n-180.00 <= lat <= 180.00") ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR",
				display_string, XtGrabNone) ;
			XtFree( shapestr );
			free( comments );
			free( sitename );
			return ;
		}

		sw_lat = 0 ; sw_lon = 0 ;
		
		ne_lat = 0 ; ne_lon = 0 ;
		se_lat = 0 ; se_lon = 0 ;
	}

	(void) sprintf(col_names, "%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s",
		APS_COL(SITE, SITE_SITENAME), APS_COL(SITE, SITE_COMMENTS),
		APS_COL(SITE, SITE_SHAPE),
		APS_COL(SITE, SITE_NWLAT), APS_COL(SITE, SITE_NWLON),
		APS_COL(SITE, SITE_SWLAT), APS_COL(SITE, SITE_SWLON),
		APS_COL(SITE, SITE_NELAT), APS_COL(SITE, SITE_NELON),
		APS_COL(SITE, SITE_SELAT), APS_COL(SITE, SITE_SELON),
		APS_COL(SITE, SITE_RADIUS)) ;
		
	(void) sprintf(col_values, 
		"'%s', '%s', '%c', \
		%f, %f, %f, %f, \
		%f, %f, %f, %f, \
		%f",
		sitename, comments, shape,
		nw_lat, nw_lon,
		sw_lat, sw_lon,
		ne_lat, ne_lon,
		se_lat, se_lon,
		radius) ;

	if (db_insert_values(APS_dbproc, APS_TABLE(SITE), col_names, col_values)
		!= 1) 
	{
		(void) sprintf(display_string, 
			"Unable to Add site\nPossible Syntax Error in a field") ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone) ;
		XtFree( shapestr );
		free( comments ) ;
		free( sitename );
		return ;
	}
	(void) sprintf(display_string, "SITE: %s\nadded successfully", sitename) ;
	popup_message(XmDIALOG_INFORMATION, "APS:INFORMATION",
			display_string, XtGrabNone) ;

	/* display the hypothetical sites list if it is not displayed */
	if (!XmToggleButtonGetState(toggleButton_HypoSites))
	{
		/*
		-- switch the toggle buttons
		*/
		XmToggleButtonSetState(toggleButton_HypoSites, True, False) ;
		XmToggleButtonSetState(toggleButton_DARSites, False, False) ;
	}

	/* 
	-- refresh the window list: retrieve and display the new sites
	*/

	(void) get_site_list();  /* ignore error - continue setting state of gui */

	cb_display_hypo_sites(widget, client_data, NULL) ;

	/* turn on the DELETE BUTTON */
	XtManageChild(pushButton_delete_site) ;

	/* turn off the DONE and CANCEL buttons */
	XtUnmanageChild(pushButton_done_create_site) ;
	XtUnmanageChild(pushButton_cancel_create_site) ;

	/* reset the site information fields to uneditable */
	set_HypoSite_edit_fields(FALSE) ;

	XtFree( shapestr );
	free( comments ) ;
	free( sitename );

END_CONTEXT
	return;
}



/*==============================================================================
Function:		cb_display_hypo_sites
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
/* ARGSUSED0 */
void
cb_display_hypo_sites(widget, client_data, cbs)
	Widget widget ;
	XtPointer client_data ;
	XmListCallbackStruct *cbs ;
{
BEGIN_CONTEXT( cdtakeopps_form )

	DB_RECORD		**site_rec ;
	cursor			ptr ;
	XmStringTable	str_list ;
	int				numSites ;
	int				i ;

	/* if deactivating just return */ 
	if (!XmToggleButtonGetState(toggleButton_HypoSites))
		return ;

	numSites = (sites) ? NUMELTS(sites) : 0 ;

	str_list = (XmStringTable) 
		XtMalloc(numSites * sizeof(XmString)) ;

	site_rec = (DB_RECORD **) FIRST(sites, ptr) ;
	for (i = 0 ; i < numSites ; i++)
	{
		(void) sprintf(display_string, "XXXXXX  %-32s",
			CAST_SITE_SITENAME site_rec[SITE_SITENAME]) ;
		str_list[i] = XmStringCreateLocalized(display_string) ;
		site_rec = (DB_RECORD **) NEXT(sites, ptr) ;
	}

	XtVaSetValues(scrolledList_sites,
		XmNitems, str_list,
		XmNitemCount, numSites,
		NULL) ;
																  
	for (i = 0; i < numSites; i++)
		XmStringFree(str_list[i]) ;

	XtManageChild(pushButton_delete_site) ;
	XtSetSensitive( menuCreateHypoSite, True ) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_display_dar_sites
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
/* ARGSUSED1 */
void
cb_display_dar_sites(widget, client_data, cbs)
	Widget widget ;
	XtPointer client_data ;
	XmListCallbackStruct *cbs ;
{
BEGIN_CONTEXT( cdtakeopps_form )

	DB_RECORD **dar_rec ;
	cursor ptr ;
	XmStringTable str_list ;
	int i ;

	/* if deactivating do return */
	if (!XmToggleButtonGetState(toggleButton_DARSites))
		return ;

	if (dars == NULL && get_dar_list() == NULL)
		return;

	str_list = (XmStringTable) XtMalloc( NUMELTS(dars) * sizeof(XmString) ) ;

	dar_rec = (DB_RECORD **) FIRST(dars, ptr) ;
	for (i = 0 ; i < NUMELTS(dars) ; i++)
	{
		(void) sprintf(display_string, "%6d %-32s",
			CAST_DAR_DARID dar_rec[DAR_DARID], 
			CAST_DAR_SITENAME dar_rec[DAR_SITENAME]) ;
		str_list[i] = XmStringCreateLocalized(display_string) ;
		dar_rec = (DB_RECORD **) NEXT(dars, ptr) ;
	}

	XtVaSetValues(scrolledList_sites,
		XmNitems, str_list,
		XmNitemCount, NUMELTS(dars) ,
		NULL) ;
																  
	for (i = 0; i < NUMELTS(dars); i++)
		XmStringFree(str_list[i]) ;

	XtUnmanageChild(pushButton_delete_site) ;
	XtSetSensitive( menuCreateHypoSite, False ) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_show_dar_relations
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
/* ARGSUSED2 */
void
cb_show_dar_relations(widget, client_data, cbs)
	Widget widget ;
	XtPointer client_data, cbs ;
{
BEGIN_CONTEXT( cdtakeopps_form )

	/*
	-- get the number of dars available
	-- and allocate a String Table to hold them
	*/

	if (get_dar_list() == NULL)
		return;

	if (get_site_list() == NULL)
		return;

	/*
	-- display either the dar or hypothetical sites based on the
	-- toggle button setting
	*/

	if (XmToggleButtonGetState(toggleButton_DARSites)) /* DAR Sites */
	{
		cb_display_dar_sites(widget, client_data, NULL) ;
	}
	else	/* HYPOTHETICAL sites */
	{
		cb_display_hypo_sites(widget, client_data, NULL) ;
	}

END_CONTEXT
}

														 

/*==============================================================================
Function:		cb_update_cdtakeopps_form
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
void
cb_update_cdtakeopps_form(widget, client_data, cbs)
	Widget widget ;
	XtPointer client_data ;
	XmListCallbackStruct *cbs ;
{
BEGIN_CONTEXT( widget )

	char	*sat_name ;
	Widget	sat_button ;

	char	*sensor ;

	int		index ;
	DB_RECORD **record ;
	int /* indices for the dbase record (dar or site) */
		sitename, shape, comments,
		nw_lat,  nw_lon, sw_lat,  sw_lon,
		ne_lat,  ne_lon, se_lat,  se_lon, 
		center_lat, center_lon, radius ;
	char	*starttime;
	char	*endtime;
	double	et_total_days ;
	int		bothHaveTimes = TRUE ;

	COLUMN_DEFS *field ;	

	index = cbs->item_position ;
	if (XmToggleButtonGetState(toggleButton_DARSites)) /* DAR Site */
	{
		if (dars == NULL && get_dar_list() == NULL)
		{
			return;
		}

		record = (DB_RECORD **) db_nth_record(dars, index) ;
		field = APS_CDEFS(DAR) ;

		(void) sprintf(display_string, APS_PFMT(DAR, DAR_DARID), 
			CAST_DAR_DARID record[DAR_DARID]) ;
		XmTextFieldSetString(TF_DARID, display_string) ;

		if (CAST_DAR_QUICKLOOK record[DAR_QUICKLOOK] == 'Y')
		{
			XtVaSetValues( TF_QUICKLOOK,
				XtVaTypedArg,
				XmNlabelString, XmRString, "Yes", strlen("Yes") + 1,
				NULL );
		}
		else
		{
			XtVaSetValues( TF_QUICKLOOK,
				XtVaTypedArg,
				XmNlabelString, XmRString, "No", strlen("No") + 1,
				NULL );
		}

		/*
		-- if the start or stop time is blank
		-- set the field to 0 length string
		*/

		starttime = CAST_DAR_STRTTIME record[DAR_STRTTIME] ;
		if (*starttime == STREND || strchr( starttime, *blank ))
		{
			XmTextFieldSetString( TF_dtkopps_start, EMPTY_STR ) ;
			bothHaveTimes = FALSE ;
		}
		else
			XmTextFieldSetString( TF_dtkopps_start, starttime ) ;

		endtime = CAST_DAR_ENDTIME record[DAR_ENDTIME] ;
		if (*endtime == STREND || strchr( endtime, *blank ))
		{
			XmTextFieldSetString( TF_dtkopps_end, EMPTY_STR ) ;
			bothHaveTimes = FALSE ;
		}
		else
			XmTextFieldSetString( TF_dtkopps_end, endtime ) ;

		XmTextFieldSetString( TF_dtkopps_total_days, EMPTY_STR ) ;
		/* update the total_days field if both fields have data */
		if (bothHaveTimes == TRUE)
		{
			if (tc_et_ASF_datetime_diff( starttime, endtime, 
				&et_total_days ) == TRUE)
			{
				(void) sprintf(display_string, "%.2f", et_total_days) ;
				XmTextFieldSetString(TF_dtkopps_total_days, display_string) ;
			}
		}

		/* update the direction segments toggle buttons */
		switch (CAST_DAR_ASCDSC record[DAR_ASCDSC])
		{
		case 'A' :
			XmToggleButtonSetState(toggleButton_Ascending, True, False) ;
			XmToggleButtonSetState(toggleButton_Descending, False, False) ;
			break ;

		case 'D' :
			XmToggleButtonSetState(toggleButton_Ascending, False, False) ;
			XmToggleButtonSetState(toggleButton_Descending, True, False) ;
			break ;

		case 'B' :
		default  :
			XmToggleButtonSetState(toggleButton_Ascending, True, False) ;
			XmToggleButtonSetState(toggleButton_Descending, True, False) ;
		}

		

		sitename = DAR_SITENAME ; shape = DAR_SHAPE ;
		nw_lat = DAR_NWLAT ; nw_lon = DAR_NWLON ;
		sw_lat = DAR_SWLAT ; sw_lon = DAR_SWLON ;
		ne_lat = DAR_NELAT ; ne_lon = DAR_NELON ;
		se_lat = DAR_SELAT ; se_lon = DAR_SELON ;
		center_lat = DAR_NWLAT ; center_lon = DAR_NWLON ;
		radius = DAR_RADIUS ;
		comments = DAR_USERCMNT ;

		/* display the correct satellite and sensor */
		sat_name = get_satellite_name( (char *) record[DAR_SAT] ) ;
		if (!sat_name)
		{
			(void) sprintf( display_string,
				"INTERNAL ERROR:\nsatellite code (%s)\nis not in \"sat\" table",
				(char *) record[DAR_SAT] ) ;
			popup_message(XmDIALOG_ERROR, "APS:TIME ERROR", 
				display_string, XtGrabNone) ;
			return ;
		}
		sat_button = XtNameToWidget(subMenu_cdtk_sat, sat_name) ;
		XtCallActionProc(sat_button, "ArmAndActivate", NULL, NULL, 0);
		switch (set_satellite_menu(
			optionMenu_cdtk_sat, subMenu_cdtk_sat, (char *) record[DAR_SAT], 
			optionMenu_cdtk_sensor, subMenu_cdtk_sensor,
			(char *) record[DAR_SENSOR], TRUE ))
		{
			case SATMENU_OK:	/* succeeded, continue processing */
					break ;
			case SENSOR_ERROR:	/* try again with default sensor */
				sensor =
					get_default_sensor( (char *) record[DAR_SAT], TRUE ) ;
				if (sensor)
				{
					(void) strcat( display_string, "Using default sensor" ) ;
					popup_message(XmDIALOG_WARNING, "APS:WARNING", 
						display_string, XtGrabNone) ;

					if (set_satellite_menu( optionMenu_cdtk_sat,
						subMenu_cdtk_sat, (char *) record[DAR_SAT],
						optionMenu_cdtk_sensor, subMenu_cdtk_sensor,
						sensor, TRUE ) != SATMENU_OK)
					{
						popup_message(XmDIALOG_ERROR, "APS:ERROR", 
							display_string, XtGrabNone) ;
						return ;
					}
				}
				else
				{
					popup_message(XmDIALOG_ERROR, "APS:ERROR", 
						display_string, XtGrabNone) ;
					return ;
				}
				break;
			default:
				popup_message(XmDIALOG_ERROR, "APS:ERROR", 
					display_string, XtGrabNone) ;
				return ;
		}
	}	
	else /* Hypothetical Site */
	{
		if (sites == NULL && get_site_list() == NULL)
		{
			return;
		}

		record = (DB_RECORD **) db_nth_record(sites, index) ;
		field = APS_CDEFS(SITE) ;

		XmTextFieldSetString(TF_DARID, "XXXXXX") ;
		XtVaSetValues( TF_QUICKLOOK,
			XtVaTypedArg,
			XmNlabelString, XmRString, "No", strlen("No") + 1,
			NULL );

		sitename = SITE_SITENAME ;
		shape = SITE_SHAPE ;
		nw_lat = SITE_NWLAT ; nw_lon = SITE_NWLON ;
		sw_lat = SITE_SWLAT ; sw_lon = SITE_SWLON ;
		ne_lat = SITE_NELAT ; ne_lon = SITE_NELON ;
		se_lat = SITE_SELAT ; se_lon = SITE_SELON ;
		center_lat = SITE_NWLAT ; center_lon = SITE_NWLON ;
		radius = SITE_RADIUS ;
		comments = SITE_COMMENTS ;

		/* set the start/stop/total times to blank for hypo coverage */
		XmTextFieldSetString(TF_dtkopps_start, EMPTY_STR) ;
		XmTextFieldSetString(TF_dtkopps_end, EMPTY_STR) ;
		XmTextFieldSetString( TF_dtkopps_total_days, EMPTY_STR ) ;
	}

	(void) sprintf(display_string, field[sitename].format, 
		CAST_SITE_SITENAME record[sitename]) ;
	XmTextFieldSetString(TF_SITENAME, display_string) ;

	(void) sprintf(display_string, field[comments].format, 
		CAST_SITE_COMMENTS record[comments]) ;
	XmTextFieldSetString(TF_COMMENTS, display_string) ;

	(void) sprintf(display_string,
		field[nw_lat].format, CAST_DAR_NWLAT record[nw_lat]) ;
	XmTextFieldSetString(TF_NW_LAT, display_string) ;

	(void) sprintf(display_string,
		field[nw_lon].format, CAST_DAR_NWLON record[nw_lon]) ;
	XmTextFieldSetString(TF_NW_LON, display_string) ;

	(void) sprintf(display_string,
		field[sw_lat].format, CAST_DAR_SWLAT record[sw_lat]) ;
	XmTextFieldSetString(TF_SW_LAT, display_string) ;

	(void) sprintf(display_string,
		field[sw_lon].format, CAST_DAR_SWLON record[sw_lon]) ;
	XmTextFieldSetString(TF_SW_LON, display_string) ;

	(void) sprintf(display_string,
		field[ne_lat].format, CAST_DAR_NELAT record[ne_lat]) ;
	XmTextFieldSetString(TF_NE_LAT, display_string) ;

	
	(void) sprintf(display_string,
		field[ne_lon].format, CAST_DAR_NELON record[ne_lon]) ;
	XmTextFieldSetString(TF_NE_LON, display_string) ;

	(void) sprintf(display_string,
		field[se_lat].format, CAST_DAR_SELAT record[se_lat]) ;
	XmTextFieldSetString(TF_SE_LAT, display_string) ;

	(void) sprintf(display_string,
		field[se_lon].format, CAST_DAR_SELON record[se_lon]) ;
	XmTextFieldSetString(TF_SE_LON, display_string) ;
	 
	(void) sprintf(display_string, field[center_lat].format, 
		CAST_DAR_SELAT record[center_lat]) ;
	XmTextFieldSetString(TF_center_lat, display_string) ;

	(void) sprintf(display_string, field[center_lon].format, 
		CAST_DAR_SELON record[center_lon]) ;
	XmTextFieldSetString(TF_center_lon, display_string) ;
	 
	(void) sprintf(display_string,
		field[radius].format, CAST_DAR_SELON record[radius]) ;
	XmTextFieldSetString(textField_radius, display_string) ;


	/*
	-- display the appropiate form 
	-- that describes the shape
	*/
	switch (CAST_DAR_SHAPE record[shape])
	{
		case RECT_CHAR : /* rectangle */
				/* FALLTHROUGH */
		case QUAD_CHAR :	/* quadrilateral */
			(void) sprintf(display_string, "%6s", "QUAD") ;
			XtUnmanageChild(form_circle) ;
			XtManageChild(form_quad) ;
			break ;

		case POINT_CHAR :	/* circle */
			(void) sprintf(display_string, "%6s", "CIRCLE") ;
			XtUnmanageChild(form_quad) ;
			XtManageChild(form_circle) ;
			break ;

		default :
			(void) sprintf(display_string, "");
			break ;
	}
	XmTextFieldSetString(TF_SHAPE, display_string) ;

	/* always set the rev fields to zero */
	XmTextFieldSetString(TF_start_rev, "0") ;
	XmTextFieldSetString(TF_stop_rev, "0") ;

	set_cdtakeopps_form_sensitivity((PROCESS_INFO *)NULL, TRUE) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_cancel_edit_new_HypoSite
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	08/23/1994
Notes:		
==============================================================================*/
/* ARGSUSED1 */
void
cb_cancel_edit_new_HypoSite(Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( widget )
 
    /* turn on the DELETE BUTTON */
    XtManageChild(pushButton_delete_site) ;

	/* turn off the DONE and CANCEL buttons */
	XtUnmanageChild(pushButton_done_create_site) ;
	XtUnmanageChild(pushButton_cancel_create_site) ;

	/* redisplay the selected site */
	XtCallActionProc(scrolledList_sites, "ListKbdActivate", NULL, NULL, 0) ;

	/* reset the site information fields to uneditable */
	set_HypoSite_edit_fields(FALSE) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_edit_new_HypoSite
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	08/22/1994
Notes:		
==============================================================================*/
/* ARGSUSED2 */
void
cb_edit_new_HypoSite(Widget widget, XtPointer client_data, XtPointer cbs)
{
BEGIN_CONTEXT( widget )

	char *shape ;
	 
	/* turn off the DELETE BUTTON */
	XtUnmanageChild(pushButton_delete_site) ;

	/* turn on the DONE and CANCEL buttons */
	XtManageChild(pushButton_done_create_site) ;
	XtManageChild(pushButton_cancel_create_site) ;
	
	shape = (char * ) client_data ;
	switch (shape[0])
	{
		case QUAD_CHAR :	/* quadrilateral */
			(void) sprintf(display_string, "%6s", "QUAD") ;
			XtUnmanageChild(form_circle) ;
			XtManageChild(form_quad) ;
			break ;

		case POINT_CHAR :	/* circle */
			(void) sprintf(display_string, "%6s", "CIRCLE") ;
			XtUnmanageChild(form_quad) ;
			XtManageChild(form_circle) ;
			break ;

		default :
			(void) sprintf( display_string, "" ) ;
			break ;
	}

	XmTextFieldSetString(TF_SHAPE, display_string) ;
	XmTextFieldSetString(TF_DARID, "XXXXXX") ;

	set_HypoSite_edit_fields(TRUE) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_do_create_dtk_opps
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
/* ARGSUSED1 */
void
cb_do_create_dtk_opps(widget, client_data, cbs)
	Widget widget ;
	XtPointer client_data, cbs ;
{
BEGIN_CONTEXT( widget )

	Widget selected_sat ;
	Widget selected_sensor ;

	Boolean asc_segments ;
	Boolean des_segments ;

	static char asc_flag[] = "-A" ;
	static char dsc_flag[] = "-D" ;
	char *ascdsc_flag ;

	char *start_time = NULL;
	char *stop_time  = NULL;
	char *start_rev ;
	char *stop_rev  ;

	/*  string for range 
	--   -t <start_time> <stop_time> 
	-- | -r <start_rev> <stop_rev>
	*/
	char range_string[50] ; 

	char *darid ;
	char *sitename ;
	char darsite[64] ;

	int status ;
	PROCESS_INFO *process ;

	SATELLITE *sat_rec ;
	cursor sat_ptr ;

	char *sensorname ;
	char *satname ;
	char *satname2 ;
	char *start_range, *stop_range ;

	darid = gui_TF_string(TF_DARID) ;

	/* if darid is for hypo site i.e. XXXX use sitename instead of darid */
	sitename = gui_TF_string(TF_SITENAME) ;
	if (*darid == 'X')
		(void) sprintf(darsite, "-s '%s'", sitename) ;
	else /* use the dar id */
		(void) sprintf(darsite, "-d %s", darid) ;

	XtVaGetValues(subMenu_cdtk_sat,
		XmNmenuHistory, &selected_sat,
		NULL) ;

	satname = XtName(selected_sat) ;

	for (sat_rec = (SATELLITE *) FIRST(satellites, sat_ptr)
		; sat_rec
		; sat_rec = (SATELLITE *) NEXT(satellites, sat_ptr))
	{
		/* get the long sat name for comparison purposes */
		satname2 = sat_rec->satname ;

		/* TODO use a strip function to remove leading
		-- and trailing spaces for true comparison
		(void) printf("STRCMP %sX %sX\n", satname2, satname) ;
		--
		-- compare only up to the length of satname2 
		-- since the displayed sat name has been formatted
		*/
		if (strncmp(satname, satname2, strlen(satname2)) == 0)
			break ;
	}

	XtVaGetValues(subMenu_cdtk_sensor,
		XmNmenuHistory, &selected_sensor,
		NULL) ;

	sensorname = XtName(selected_sensor) ;

	start_rev = gui_TF_string(TF_start_rev) ;
	stop_rev  = gui_TF_string(TF_stop_rev) ;


#ifdef DEBUG
	(void) printf("STARTREV: #%s# %d\n", start_rev, strlen(start_rev)) ;
	(void) printf("STOPREV: #%s# %d\n", stop_rev, strlen(stop_rev)) ;
#endif

	/* update the total days (for the user) */
	check_analysis_period( widget );

	/* if no range is completely specified use time */
	if (atoi(start_rev) == 0 ||  atoi(stop_rev) == 0)
	{
		/* activate the start/stop time fields for error validation */
		XtCallActionProc(TF_dtkopps_start, "activate", NULL, NULL, 0) ;
		if (ASF_datetime_err)
		{
			XtFree( darid );
			XtFree( sitename );
			XtFree( start_rev );
			XtFree( stop_rev );
			return ;
		}
 
		XtCallActionProc(TF_dtkopps_end, "activate", NULL, NULL, 0) ;
		if (ASF_datetime_err)
		{
			XtFree( darid );
			XtFree( sitename );
			XtFree( start_rev );
			XtFree( stop_rev );
			return ;
		}
 
		/* times were already validated, now set the local variables */
		start_time = gui_TF_string( TF_dtkopps_start ) ;
		stop_time  = gui_TF_string( TF_dtkopps_end ) ;

#ifdef DEBUG
		(void) printf("STARTTIME: #%s# %d\n", start_time, strlen(start_time)) ;
		(void) printf("STOPTIME: #%s# %d\n", stop_time, strlen(stop_rev)) ;
#endif

		/* use the time strings to check the range */
		if (time_range_error(start_time, stop_time) == TRUE)
		{
			XtFree( darid );
			XtFree( sitename );
			XtFree( start_rev );
			XtFree( stop_rev );
			XtFree( start_time );
			XtFree( stop_time );
			return ;
		}

		(void) sprintf(range_string, "-b %s -e %s", start_time, stop_time) ;

		/* point to the start/stop for question purposes */
		start_range = start_time ;
		stop_range = stop_time ;
	}
	else /* use rev as range */
	{
		if (atoi(start_rev) > atoi(stop_rev))
		{
			(void) sprintf(display_string, 
				"Stop Rev  '%s' is before\nStart Rev '%s'",
				stop_rev, start_rev) ;
			popup_message(XmDIALOG_ERROR, "APS:TIME ERROR", 
				display_string, XtGrabNone) ;
			XtFree( darid );
			XtFree( sitename );
			XtFree( start_rev );
			XtFree( stop_rev );
			return ;
		}
		(void) sprintf(range_string, "-f %s -l %s", start_rev, stop_rev) ;
		/* point to the start/stop for question purposes */
		start_range = start_rev ;
		stop_range = stop_rev ;
	}

	/* check the asc/dsc buttons to see what's activated */
	asc_segments = XmToggleButtonGetState(toggleButton_Ascending) ;
	des_segments = XmToggleButtonGetState(toggleButton_Descending) ;

	if ((!asc_segments) && (!des_segments))
	{
		(void) sprintf(display_string,
		"Potential Segments\nAscending or Descending\nare not set") ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR", 
			display_string, XtGrabNone);
		XtFree( darid );
		XtFree( sitename );
		XtFree( start_rev );
		XtFree( stop_rev );
		return ;
	}

	if ((asc_segments) && (des_segments))
		ascdsc_flag = blank ;
	else if (asc_segments)
		ascdsc_flag = asc_flag ;
	else  /* descending */
		ascdsc_flag = dsc_flag ;

	(void) sprintf(question,
		"Create Data Take Opportunities for\n\nSAT:       %s\nSENSOR:    %s\nDARID:     %s\nSITE:      %s\nSTART:     %s\nSTOP:      %s\nDIRECTION: %s",
		sat_rec->satname, sensorname, darid, sitename,
		start_range, stop_range,
		((asc_segments) && (des_segments)) ? "(both)" : ascdsc_flag) ;

	if (AskUser(widget, question, YES) == YES)
	{
		(void) sprintf(command_string, "%s -U %s -P %s %s %s %s %s %s",
			CREATE_DTK_OPPS_CMD, userid, password,
			darsite, ascdsc_flag, range_string, 
			sat_rec->sat, sensorname) ;

		/* clear the prior status/messages */
		XmTextSetString(scrolledText_create_dtkopps, EMPTY_STR) ;

		process = (PROCESS_INFO *) create_process(command_string, &status,
			TRUE, NULL, gui_display_message_widget, scrolledText_create_dtkopps,
			set_cdtakeopps_form_sensitivity, (void *)TRUE) ;

		if (!process)
		{
			(void) sprintf(display_string, 
				"Can't create DTK Opportunities\n Too many processes running?");
			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
				display_string, XtGrabNone);
		}
		if (start_process(process))	/* msg already popped up */
			destroy_process( process ) ;
	}

	if (start_time != NULL)
	{
		XtFree(start_time) ;
		XtFree(stop_time) ;
	}
	XtFree( darid );
	XtFree( sitename );
	XtFree( start_rev );
	XtFree( stop_rev );

END_CONTEXT
}
