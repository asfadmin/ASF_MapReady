#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
ALL RIGHTS RESERVED.  U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	cb_cnomcov.c

Description:	

External Functions:
			set_coverage_type
			cb_show_coverage_relations
			cb_set_coverage_type
			cb_update_coverage_filename
			cb_update_cnomcov_form
			cb_adjust_phase_stoptime
			cb_check_phase_time_range
			cb_do_create_coverage
	
Static Functions:
			set_cnomcov_form_editability
			remote_cvrg_filename
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:

==============================================================================*/
#pragma ident	"@(#)cb_cnomcov.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_cnomcov.c"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "db_sybint.h"
#include "dtkm_utilities.h"

#include "dapps_defs.h"
#include "aps_defs.h"
#include "aps_db_table.h"
#include "aps_db_tablenums.h"
#include "apspath.h"
#include "apsfiledef.h"
#include "db_dtk.h"
#include "db_ephemeris.h"
#include "db_phase.h"
#include "db_satsensor.h"
#include "db_station.h"
#include "aps_extern.h"
#include "aps_exe_names.h"

#include "nmalloc.h" 


#include <Xm/List.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include "UxXt.h"

#include "subprocess.h"
#include "satmenus.h"
#include "gui_utils.h"

#define CONTEXT_MACRO_ACCESS 1
#include "vc_cnomcov.h"
#undef CONTEXT_MACRO_ACCESS

#include "cb_cnomcov.h"
#include "cb_datetime.h"
#include "timeconv.h"

extern void	popup_message() ;

extern Widget cnomcov_form ;
extern char display_string[] ;

static llist *coverages = NULL ;
static char TF_total_days_format[] = "%.2f" ;
char command_string[255] ;

#define BEGIN_CONTEXT(widget) \
	_UxCCreateNominalCoverage *UxSaveCtx ; \
	UxSaveCtx = UxCreateNominalCoverageContext; \
	UxCreateNominalCoverageContext = \
		(_UxCCreateNominalCoverage *) UxGetContext( widget ); \
    {

#define END_CONTEXT \
	} \
	UxCreateNominalCoverageContext = UxSaveCtx;


/*==============================================================================
Function:		set_coverage_type
Description:	sets the coverage type option menu based on the
				satellite.
Parameters:		
Returns:		returns the option menu item Widget
				or NULL if the option menu could not be set
Creator:		Teresa McKillop
Creation Date:	04/10/96
Notes:		
==============================================================================*/
Widget
set_coverage_type( char *satcode )
{
	DB_RECORD	**dtk_rec ;
	Widget		menuItem = NULL ;

	if (satcode != NULL)
	{
		/* dummy up a dtk db record with just the sat code */
		dtk_rec = new_table_record( APS_CDEFS( DTK ) ) ;
		(void) strcpy( CAST_DTK_SAT dtk_rec[DTK_SAT], satcode) ;

		/*
		-- set the option menu based on whether the sat has a
		-- recorder on board (if yes == global, if no == station mask)
		*/

		if (dtkm_sat_has_recorder( dtk_rec ) == TRUE)
		{
			/* GLOBAL coverage */
			menuItem = subMenu_covtype_GBL ;
		}
		else
		{
			/* STN MASK coverage */
			menuItem = subMenu_covtype_STN ;
		}
		XtVaSetValues( optionMenu_covtype,
			XmNmenuHistory, menuItem,
			NULL ) ;

		free_db_record( dtk_rec ) ;
	}

	return (menuItem) ;
}


/*==============================================================================
Function:		set_cnomcov_form_editability
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
static void
set_cnomcov_form_editability(process, editability)
	PROCESS_INFO *process ;
	Boolean editability ;
{
BEGIN_CONTEXT( cnomcov_form )

	gui_setEditable( T_PHASE_START, AG_TEXTFIELD, editability ) ;
	gui_setEditable( T_PHASE_END,   AG_TEXTFIELD, editability ) ;
	gui_setEditable( TF_total_days, AG_TEXTFIELD, editability ) ;

	XtSetSensitive(pushButton_refresh, editability) ;

	XtSetSensitive(optionMenu_ncov_sensor, editability) ;
	XtSetSensitive(optionMenu_covtype, editability) ;

	XtSetSensitive(scrolledList_ephm, editability) ;
	XtSetSensitive(pushButton_create_coverage, editability) ;

	if (process)
	{
		switch (process->exit_status)
		{
		case APS_EXIT_OK :
			(void) sprintf(display_string,
					"Create Nominal Coverage:\n\n Completed Successfully\n");
			popup_message(XmDIALOG_INFORMATION, "APS:INFORMATION", 
					display_string, XtGrabNone);
			break ;
		case APS_EXIT_ERROR :
			(void) sprintf(display_string,
					"Create Nominal Coverage:\n\n Unsuccessful Run") ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
					display_string, XtGrabNone);
			break ;
		case APSGUI_EXIT_COREDUMP : /* process core dumped */
			(void) sprintf( display_string,
					"Create Nominal Coverage:\n\n Signal Caught CORE DUMPED" ) ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
					display_string, XtGrabNone);
			break ;
		default :	/* process caught a signal, but no core dump */
			(void) sprintf( display_string,
					"Create Nominal Coverage:\n\n SIGNAL caught (signal = %d)",
					-(process->exit_status) ) ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
					display_string, XtGrabNone);
			break ;
		}
		TimeoutCursors(False, False, cnomcov_form) ;
	}

END_CONTEXT
}


/*==============================================================================
Function:		cb_show_coverage_relations
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
/* ARGSUSED2 */
void
cb_show_coverage_relations(widget, client_data, cbs)
	Widget widget ;
	XtPointer client_data, cbs ;
{
BEGIN_CONTEXT( widget )

	int i ;
	char *sat_string ;
		 
	XmStringTable str_list ;

	DB_RECORD **coverage_rec ;
	cursor ptr ;
					  
	Widget list_widget = (Widget) client_data ;

	/* delete any old coverage records */
	if (coverages)
	{
		DEL_LIST(coverages) ;
		coverages = NULL ;
	}

	/*
	-- get the number of coverages available
	-- and allocate a String Table to hold them
	*/
	(void) sprintf(orderby_cols, "%s, %s",
		APS_COL(EPHEMERIS, EPHEMERIS_SAT),
		APS_COL(EPHEMERIS, EPHEMERIS_PHASE_NAME)) ;

	coverages = db_get_records(APS_dbproc, APS_TABLE(EPHEMERIS), 
		NULL, orderby_cols, APS_CDEFS(EPHEMERIS), ALL_COLS) ;

	if (coverages == NULL)
	{
		(void) sprintf(display_string,
				"(aps) ERROR: Fail to query Sybase EPHEMERIS table.\n"
				"orderby_cols = \n\"%s\"\n", orderby_cols);
		popup_message(XmDIALOG_ERROR, "APS:ERROR",
					display_string, XtGrabNone) ;

		return;
	}

	str_list = (XmStringTable) 
		XtMalloc(NUMELTS(coverages) * sizeof(XmString)) ;

	for (i = 0, coverage_rec = (DB_RECORD **) FIRST(coverages, ptr)
		; coverage_rec
		; coverage_rec = (DB_RECORD **) NEXT(coverages, ptr))
	{
		if ((sat_string = get_satellite_name(
			(char *) coverage_rec[EPHEMERIS_SAT] )) == NULL)
		{
			/*
			-- sat code is not in internal satellite table;
			-- can't handle this one: sat button is based on sat name
			*/

			/* popup error message */
			(void) sprintf( display_string,
				"WARNING: Satellite \"%s\" is not\n    in internal sat_name table\n    SKIPPING this ephemeris record",
				(char *) coverage_rec[EPHEMERIS_SAT] ) ;
			gui_aps_internal_error( XmDIALOG_ERROR,
				__FILE__, __LINE__, display_string ) ;

			/* remove from the coverages list */
			(void) DEL_AT_CURSOR( coverages, ptr );
			/* skip this one and get the next */
			continue;
		}

		(void) sprintf(display_string, "%-16s %-9s %2c    %21s %4d",
			CAST_EPHEMERIS_FILENAME coverage_rec[EPHEMERIS_FILENAME],
			sat_string,
			CAST_PHASE_PHASE_NAME coverage_rec[EPHEMERIS_PHASE_NAME],
			CAST_EPHEMERIS_STARTTIME coverage_rec[EPHEMERIS_STARTTIME],
			CAST_EPHEMERIS_STARTREV coverage_rec[EPHEMERIS_STARTREV]) ;

		str_list[i] = XmStringCreateLocalized(display_string) ;
		i++ ;
	}

	XtVaSetValues(list_widget,
		XmNitems, str_list,
		XmNitemCount, NUMELTS(coverages),
		NULL) ;
														  
	for (i = 0; i < NUMELTS(coverages); i++)
		XmStringFree(str_list[i]) ;

	XtFree((char *) str_list) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_set_coverage_type
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	11/20/1994
Notes:		
==============================================================================*/
void
cb_set_coverage_type(widget, client_data, cbs)
	Widget widget ;
	XtPointer client_data ;
	XmRowColumnCallbackStruct *cbs ;
{
BEGIN_CONTEXT( widget )

	XmString str ;
	char *label ;

	/*
	-- get the label of the activating button for
	-- the covtype option button... it will be used
	-- to tell what type of coverage is to be done
	*/

	XtVaGetValues(cbs->widget,
		XmNlabelString, &str,
		NULL) ;
	XmStringGetLtoR(str, XmFONTLIST_DEFAULT_TAG, &label) ;

	XtFree(label) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_update_coverage_filename
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
void
cb_update_coverage_filename(widget, client_data, cbs)
	Widget widget ;
	XtPointer client_data ;
	XmRowColumnCallbackStruct *cbs ;
{
BEGIN_CONTEXT( widget )

	Widget selected_option ;

	int index ;

	char *sensor_string ;
	char *update_type ;  /*
	                     -- It can be "SENSOR" or "STATION".
						 -- However, STATION is not currently used. 
	                     -- This tells us what the activating option that
	                     -- caused this cb... I suppose we could have used
	                     -- the name of the Widget, but it's easier
	                     */

	DB_RECORD ** coverage_rec ;

	selected_option = cbs->widget ;

	/*
	-- get the update type we're doing (SENSOR or STATION)
	-- The item in the Option Menu that activates this function is 	i
	-- the selected option widget... It's name contains the string
	-- to be written to the file name 
	*/
	update_type = (char *) client_data ;

	if (strcmp(update_type, "SENSOR") == 0)
	{
		sensor_string = XtName(selected_option) ;
	}


	/* get the record of the selected sat to run Nominal Cov on */
	index = gui_XmList_selected_item(scrolledList_ephm) ;
	if (index != 0)	/* an item is selected, update the filename */
	{
		coverage_rec = (DB_RECORD **) db_nth_record(coverages, index) ;

		(void) sprintf(display_string, "%s_%c_%s.COV",
			CAST_EPHEMERIS_SAT coverage_rec[EPHEMERIS_SAT],
			CAST_EPHEMERIS_PHASE_NAME coverage_rec[EPHEMERIS_PHASE_NAME],
			sensor_string) ; 


		XmTextFieldSetString(TF_coverage_filename, display_string) ;
	}

END_CONTEXT
}




/*==============================================================================
Function:		cb_update_cnomcov_form
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
void
cb_update_cnomcov_form(widget, client_data, cbs)
	Widget widget ;
	XtPointer client_data ;
	XmListCallbackStruct *cbs ;
{
BEGIN_CONTEXT( widget )

	int index ;
	char *sat_name ;
	char *filename ;
	char *ephmfile ;

	char *selected_sensor ;
	DB_RECORD **coverage_rec ;

	Widget sat_button ;

	index = cbs->item_position ;
	coverage_rec = (DB_RECORD **) db_nth_record(coverages, index) ;

	filename = (char *) coverage_rec[EPHEMERIS_FILENAME] ;

	/*
	-- NOTE: get_satellite_name will not return NULL because of
	-- how it's handled in cb_show_coverage_relations()
	*/
	sat_name = get_satellite_name((char *) coverage_rec[EPHEMERIS_SAT]) ;

	/* 
	-- check that the file in fact exists
	-- it should be in the directory pointed to by
	-- APS_ORBGIN; if not display the error message 
	*/
    ephmfile = aps_fullpath(APS_ORBGIN, filename) ;
	if (access(ephmfile, R_OK) != 0)
	{
		(void) sprintf(display_string, 
			"Unable to find EPHEMERIS file:\n   %s\nRun Create Nominal Orbit to create EPHEMERIS file for\n   Satellite '%s' and Phase '%c'", 
			command_string, sat_name, 
			CAST_EPHEMERIS_PHASE_NAME coverage_rec[EPHEMERIS_PHASE_NAME] ) ;
		popup_message(XmDIALOG_ERROR, "APS:FILE ERROR", 
			display_string, XtGrabNone) ;

		/* TODO actually need to clear form so as to not confuse */
		XtSetSensitive(pushButton_create_coverage, FALSE) ;
		free(ephmfile) ;
		return ;
	}
	free(ephmfile) ;

	sat_button = XtNameToWidget(subMenu_ncov_sat, sat_name) ;
	XtCallActionProc(sat_button, "ArmAndActivate", NULL, NULL, 0);

	if (set_satellite_menu( optionMenu_ncov_sat, subMenu_ncov_sat,
		(char *) coverage_rec[EPHEMERIS_SAT], NULL, NULL, NULL, TRUE )
		!= SATMENU_OK)
	{
		popup_message(XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone) ;
		return ;
	}

	(void) sprintf(display_string, 
		APS_PFMT(EPHEMERIS, EPHEMERIS_FILENAME), 
		(char *) coverage_rec[EPHEMERIS_FILENAME] ) ;
	XmTextFieldSetString(TF_ephemeris_file, display_string) ;

	(void) sprintf(display_string, APS_PFMT(EPHEMERIS, EPHEMERIS_STARTTIME),
		CAST_EPHEMERIS_STARTTIME coverage_rec[EPHEMERIS_STARTTIME]) ;
	XmTextFieldSetString(T_PHASE_START, display_string) ;

	/* 
	-- update the end time by calling the callback
	-- associated with the stopttime toggle button
	*/
	cb_adjust_phase_stoptime(toggleButton_stoptime, client_data, NULL) ;

	selected_sensor = gui_optionMenu_string(optionMenu_ncov_sensor) ;

	/* if can't set coverage type, leave it as is */
	(void) set_coverage_type( CAST_EPHEMERIS_SAT coverage_rec[EPHEMERIS_SAT] );

	(void) sprintf(display_string, "%s_%c_%s.COV",
		CAST_EPHEMERIS_SAT coverage_rec[EPHEMERIS_SAT],
		CAST_EPHEMERIS_PHASE_NAME coverage_rec[EPHEMERIS_PHASE_NAME],
		selected_sensor) ;
	XmTextFieldSetString(TF_coverage_filename, display_string) ;

	set_cnomcov_form_editability((PROCESS_INFO *) NULL, TRUE) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_adjust_phase_stoptime
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
/* ARGSUSED1 */
void
cb_adjust_phase_stoptime(widget, client_data, cbs)
	Widget widget ;
	XtPointer client_data, cbs ;
{
BEGIN_CONTEXT( widget )

	int index ;
	double et_start= 0.0 ;
	double et_stop = 0.0 ;
	double et_total_days ;

	char asf_time[22] ;

	llist *phases ;
	DB_RECORD **coverage_rec ;
	DB_RECORD **phase_rec ;
	cursor ptr ;

	index = gui_XmList_selected_item(scrolledList_ephm) ;
	if (!index)
	{
		(void) sprintf( display_string,
			"Select a coverage relation, first" ) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone) ;
		return ;
	}

	coverage_rec = (DB_RECORD **) db_nth_record(coverages, index) ;

	/* 
	-- get the start time now, it's needed to figure
	-- the end time and total days; start by converting 
	-- it to ephemeris time
	*/
	if (!tc_asf2et( CAST_EPHEMERIS_STARTTIME
		coverage_rec[EPHEMERIS_STARTTIME], &et_start ))
	{
		(void) sprintf( display_string,
			"Invalid start time '%s'\nin APS DB coverage record.\nMake another selection",
			CAST_EPHEMERIS_STARTTIME coverage_rec[EPHEMERIS_STARTTIME] ) ;
		popup_message(XmDIALOG_ERROR, "APS:DB ERROR",
			display_string, XtGrabNone) ;
		return ;
	}

	if (XmToggleButtonGetState(widget))
	{
		/* button selected adjust the end time to the end of the phase */
		(void) sprintf(where_clause, "where sat = '%s' and phase_name = '%c'", 
			CAST_EPHEMERIS_SAT coverage_rec[EPHEMERIS_SAT], 
			CAST_EPHEMERIS_PHASE_NAME coverage_rec[EPHEMERIS_PHASE_NAME]) ;

		/* get the appropiate data from the phase relation */
		phases = db_get_records(APS_dbproc, 
			APS_TABLE(PHASE), where_clause, NULL, 
			APS_CDEFS(PHASE),
			PHASE_PHASE_ORBITS, PHASE_CYCLE_DAYS,
			PHASE_CYCLE_REVS, END_COLS) ;

		if (phases == NULL)
		{
			(void) sprintf(display_string,
					"(aps) ERROR: Fail to query Sybase PHASE table.\n"
					"where_clause = \n\"%s\"\n", where_clause);
			popup_message(XmDIALOG_ERROR, "APS:ERROR",
							display_string, XtGrabNone) ;
			return;
		}

		/* an error if not exactly one record found */
		if (NUMELTS(phases) == 1)
			phase_rec = FIRST(phases, ptr) ;
		else 
		{
			(void) sprintf(display_string, 
					"(aps) ERROR: %d records found in relation 'PHASE' matching \n\"%s\"\nOnly exactly one record should exist.", 
				NUMELTS(phases), where_clause) ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
				display_string, XtGrabNone) ;
			return ;
		}

		et_stop = et_start + 
			CAST_PHASE_PHASE_ORBITS phase_rec[PHASE_PHASE_ORBITS] * 
			CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS] / 
			CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;
		
		if (!tc_et2asf(et_stop, asf_time))
		{
			(void) sprintf( display_string,
				"Invalid stop time calculated\nfrom phase record matching\n\"%s\"\nFix database and retry",
				where_clause ) ;
			popup_message(XmDIALOG_ERROR, "APS:DB ERROR",
				display_string, XtGrabNone) ;
			return ;
		}

		XmTextFieldSetString(T_PHASE_END, asf_time) ;

		/* show the total # of days on the screen */
		et_total_days = et_stop - et_start ;
		(void) sprintf(display_string, TF_total_days_format, et_total_days) ;
		XmTextFieldSetString(TF_total_days, display_string) ;
	}
	else  /* end time is just the end of an orbit */
	{
		/*
		-- NOTE: this code is currently not executed because
		-- the default operation is that we give the user is the
		-- absolute end time of the phase
		--
		-- if orbits are not replicated then we need 
		-- to determine the total number of days here also
		*/
		(void) sprintf(display_string, APS_PFMT(EPHEMERIS, EPHEMERIS_ENDTIME),
			CAST_EPHEMERIS_ENDTIME coverage_rec[EPHEMERIS_ENDTIME]) ;
		XmTextFieldSetString(T_PHASE_END, display_string) ;
	}
	DEL_LIST(phases) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_check_phase_time_range
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
void
cb_check_phase_time_range(widget, client_data, cbs)
	Widget widget ;
	XtPointer client_data ;
	XmTextVerifyCallbackStruct *cbs ;
{
BEGIN_CONTEXT( widget )

	char *stop_time ;
	char asf_time[22] ;
	int index ;
	double et_start= 0.0 ;
	double et_stop = 0.0 ;
	double et_total_days ;

	llist *phases ;
	DB_RECORD **phase_rec ;
	DB_RECORD **coverage_rec ;
	cursor ptr ;

	index = gui_XmList_selected_item(scrolledList_ephm) ;
	coverage_rec = (DB_RECORD **) db_nth_record(coverages, index) ;

	/* 
	-- get the start time now, it's needed to figure
	-- the end time and total days; start by converting 
	-- it to ephemeris time
	*/
	if (!tc_asf2et( CAST_EPHEMERIS_STARTTIME
		coverage_rec[EPHEMERIS_STARTTIME], &et_start))
	{
		(void) sprintf( display_string,
			"Invalid start time '%s'\nin APS DB coverage record.\nMake another selection",
			CAST_EPHEMERIS_STARTTIME coverage_rec[EPHEMERIS_STARTTIME] ) ;
		popup_message(XmDIALOG_ERROR, "APS:DB ERROR",
			display_string, XtGrabNone) ;
		return ;
	}

	/* button selected adjust the end time to the end of the phase */
	(void) sprintf(where_clause, "where sat = '%s' and phase_name = '%c'", 
		CAST_EPHEMERIS_SAT coverage_rec[EPHEMERIS_SAT], 
		CAST_EPHEMERIS_PHASE_NAME coverage_rec[EPHEMERIS_PHASE_NAME]) ;

	/* get the appropiate data from the phase relation */
	phases = db_get_records(APS_dbproc, APS_TABLE(PHASE), 
		where_clause, NULL, APS_CDEFS(PHASE),
		PHASE_PHASE_ORBITS, PHASE_CYCLE_DAYS,
		PHASE_CYCLE_REVS, END_COLS) ;

	if (phases == NULL)
	{
		(void) sprintf(display_string,
				"(aps) ERROR: Fail to query Sybase PHASE table.\n"
				"where_clause = \n\"%s\"\n", where_clause);
		popup_message(XmDIALOG_ERROR, "APS:ERROR",
					display_string, XtGrabNone) ;
		return;
	}

	/* an error if not exactly one record found */
	if (NUMELTS(phases) == 1)
		phase_rec = FIRST(phases, ptr) ;
	else	
	{
		(void) sprintf(display_string, 
			"(aps) ERROR: %d records found in relation 'PHASE' matching \n\"%s\"\nOnly exactly one record should exist.", 
			NUMELTS(phases), where_clause) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR", 
			display_string, XtGrabNone) ;
		return ;
	}

	et_stop = et_start + 
		CAST_PHASE_PHASE_ORBITS phase_rec[PHASE_PHASE_ORBITS] * 
		CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS] / 
		CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS] ;
		
	/* asf_time will contain the latest time stop time we can have */
	if (!tc_et2asf(et_stop, asf_time))
	{
		(void) sprintf( display_string,
			"Invalid stop time calculated\nfrom phase record matching\n\"%s\"\nFix database and retry",
			where_clause ) ;
		popup_message(XmDIALOG_ERROR, "APS:DB ERROR",
			display_string, XtGrabNone) ;
		return ;
	}

	if (strcmp(cbs->text->ptr, (char *) coverage_rec[EPHEMERIS_STARTTIME]) < 0)  
	{
		(void) sprintf(display_string, 
			"Proposed Time:\n   %s\nis before Phase Start Time:\n   %s", 
			cbs->text->ptr,
			CAST_EPHEMERIS_STARTTIME coverage_rec[EPHEMERIS_STARTTIME]) ;
		popup_message(XmDIALOG_ERROR, "APS:TIME INPUT ERROR", 
			display_string, XtGrabNone);
		cbs->doit = False ;
	}
	else if (strcmp(cbs->text->ptr, asf_time) > 0)
	{
		(void) sprintf(display_string, 
			"Proposed Time:\n   %s\nis after Phase End Time:\n   %s", 
			cbs->text->ptr, asf_time) ;
		popup_message(XmDIALOG_ERROR, "APS:TIME INPUT ERROR", 
			display_string, XtGrabNone);
		cbs->doit = False ;
	}

	/* if everything is ok; update the total days field */
	if (cbs->doit == True) 
	{
		/* check if we're modifying the start or the stop time */
		if (strcmp(XtName(widget), "T_PHASE_START") == 0)
		{
			stop_time = XmTextGetString(T_PHASE_END) ;
			tc_et_ASF_datetime_diff(
				cbs->text->ptr, stop_time, &et_total_days) ;
		}
		else  /* modifying the stop time */
		{
			stop_time = XmTextGetString(T_PHASE_START) ;
			tc_et_ASF_datetime_diff(
				stop_time, cbs->text->ptr, &et_total_days) ;
		}
			
		(void) sprintf(display_string, TF_total_days_format, et_total_days) ;
		XmTextFieldSetString(TF_total_days, display_string) ;
	}
	DEL_LIST(phases) ;

END_CONTEXT
}


/*==============================================================================
Function:		cb_do_create_coverage
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
/* ARGSUSED1 */
void
cb_do_create_coverage(widget, client_data, cbs)
	Widget widget ;
	XtPointer client_data, cbs ;
{
BEGIN_CONTEXT( widget )

	char *start_time ;
	char *stop_time ;
	int index ;
	int status ;
	DB_RECORD **coverage_rec ;
	PROCESS_INFO *process ;

	char coverage_type[10] ;  /* coverage type .e.g. "-m ASF" */
	char *satname ;
	char *sensor ;
	char *days ;
    char *selectedCoverageType;

	double et_total_days ;

	static char global_str[] = "GLOBAL" ;
	static char stnMasks_str[] = "Station Mask(s)" ;
	char station_name[20]; 

	XtCallActionProc(T_PHASE_START, "activate", NULL, NULL, 0) ;
	if (ASF_datetime_err)
		return ;

	XtCallActionProc(T_PHASE_END, "activate", NULL, NULL, 0) ;
	if (ASF_datetime_err)
		return ;

	start_time = XmTextGetString(T_PHASE_START) ;	
	stop_time = XmTextGetString(T_PHASE_END) ;	

	if (strcmp(start_time, stop_time) >= 0)
	{
		(void) sprintf(display_string,
			"Start Time\n   %s\nmust come before Stop Time\n   %s\nRespecify Start/Stop Times\n",
			start_time, stop_time) ; 
		popup_message(XmDIALOG_ERROR, "APS:TIME ERROR", 
			display_string, XtGrabNone) ;
		XtFree(start_time) ;
		XtFree(stop_time) ;
		return ;
	}

	/*
	-- the start/stop times have been verified
	-- so update the total days field 
	*/
	tc_et_ASF_datetime_diff(start_time, stop_time, &et_total_days) ;
	(void) sprintf(display_string, TF_total_days_format, et_total_days) ;
	XmTextFieldSetString(TF_total_days, display_string) ;
	days = gui_TF_string(TF_total_days) ;	

	satname = gui_optionMenu_string(optionMenu_ncov_sat) ;
	sensor = gui_optionMenu_string(optionMenu_ncov_sensor) ;
	index = gui_XmList_selected_item(scrolledList_ephm) ;
	coverage_rec = (DB_RECORD **) db_nth_record(coverages, index) ;

	(void) sprintf(display_string, "%s_%c_%s.COV",
		CAST_EPHEMERIS_SAT coverage_rec[EPHEMERIS_SAT],
		CAST_EPHEMERIS_PHASE_NAME coverage_rec[EPHEMERIS_PHASE_NAME],
		sensor) ;
	XmTextFieldSetString(TF_coverage_filename, display_string) ;

	selectedCoverageType = gui_optionMenu_string(optionMenu_covtype);
	if (strcmp(selectedCoverageType, global_str) != 0)
	{
		/* 
		-- mod to support stations ASF and MCM 
		-- coverage for both stations are included
		*/
		(void) sprintf(coverage_type, "-M ") ;
		(void) strcpy(station_name, stnMasks_str) ;

	}
	else  /* global coverage */
	{
		(void) sprintf(coverage_type, "%s", blank_str) ;
		(void) strcpy(station_name, global_str) ;
	}

	(void) sprintf(question,
		"Create Nominal Coverage for\nSAT:      %s\nSENSOR:   %s\nSTART:    %s\nSTOP:     %s\n# DAYS:   %s\nTYPE:     %s",
		satname, sensor,
		start_time, stop_time,
		days, station_name) ;

	if (AskUser(UxTopLevel, question, YES) == YES)
	{
		(void) sprintf(command_string, 
#ifdef DEBUG
			/* DEBUG: create the coverage file (see the vc_*.c file also) */
			"%s -F -U %s -P %s -b %s -e %s %s %s %s",
#else
			"%s -U %s -P %s -b %s -e %s %s %s %s",
#endif
			CREATE_NOM_CVRG_CMD, userid, password, 
			start_time, stop_time,
			coverage_type,
			CAST_EPHEMERIS_SAT coverage_rec[EPHEMERIS_SAT],
			sensor) ;

		process = (PROCESS_INFO *) create_process(command_string, &status,
			TRUE, NULL, gui_display_message_widget, scrolledText_cnomcov_status,
			set_cnomcov_form_editability, (void *) TRUE) ;

		XmTextSetString(scrolledText_cnomcov_status, EMPTY_STR) ;

		if (!process)
		{
			(void) sprintf(display_string, 
				"Can't create nominal coverage process\nToo many processes running?" ) ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", 
				display_string, XtGrabNone);
		}
		else
		{
			/* make the cnomcov form uneditable until the run is done */
			set_cnomcov_form_editability((PROCESS_INFO *) NULL, FALSE) ;
			TimeoutCursors(True, True, cnomcov_form) ;
			if (start_process(process))
				destroy_process( process ) ;
		}
	}

	/* free the allocated strings */
	XtFree(start_time) ;
	XtFree(stop_time) ;
	XtFree(sensor) ;
	XtFree(days) ;
	XtFree(selectedCoverageType) ;

END_CONTEXT
}
