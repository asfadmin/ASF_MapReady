#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	cb_cnomorb.c

Description:	

External Functions:
			cb_update_cnomorb_form
			cb_show_orbit_relations  
			set_cnomorb_form_sensitivity
			cb_do_cnom
	
Static Functions:
			get_phases
	
External Variables Defined:
	
File Scope Static Variables:
	
Notes:

==============================================================================*/
#pragma ident	"@(#)cb_cnomorb.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.cb_cnomorb.c"

#include <stdio.h>

#include <Xm/List.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include "UxXt.h"

#define CONTEXT_MACRO_ACCESS
#include "vc_cnomorb.h"
#undef CONTEXT_MACRO_ACCESS
#include "vc_msgbox.h"

#include "dapps_defs.h"
#include "aps_defs.h"
#include "aps_extern.h"
#include "aps_exe_names.h"
#include "db_sybint.h"
#include "aps_db_table.h"
#include "db_phase.h"
#include "subprocess.h"
#include "satmenus.h"
#include "gui_utils.h"

extern void     popup_message();

static void		set_cnomorb_form_sensitivity() ;

extern char		*sys_errlist[];
extern char		display_string[] ;
extern Widget	cnomorb_form ;

static llist	*phases = NULL ;

#define BEGIN_CONTEXT(widget) \
	_UxCCreateNominalOrbit *UxSaveCtx ; \
	UxSaveCtx = UxCreateNominalOrbitContext; \
	UxCreateNominalOrbitContext = \
			(_UxCCreateNominalOrbit *) UxGetContext( widget ); \
    {

#define END_CONTEXT \
	} \
	UxCreateNominalOrbitContext = UxSaveCtx ;


/*==============================================================================
Function:			get_phases
Description:	
Parameters:		
Returns:	
Creator:		Teresa McKillop
Creation Date:	01/18/96
Notes:		
==============================================================================*/
static llist *
get_phases( llist **phases )
{
	if (*phases)
	{
		DEL_LIST(*phases) ;
		*phases = NULL ;
	}

	(void) sprintf(orderby_cols, "%s, %s", 
		APS_COL(PHASE, PHASE_SAT),
		APS_COL(PHASE, PHASE_PHASE_NAME)) ;

	*phases = db_get_records(APS_dbproc, APS_TABLE(PHASE), NULL, orderby_cols, 
		APS_CDEFS(PHASE),
		PHASE_SAT, PHASE_PHASE_NAME,
		PHASE_PHASE_START, PHASE_PHASE_DAYS, PHASE_PHASE_ORBITS,
		PHASE_CYCLE_DAYS, PHASE_CYCLE_REVS,
		PHASE_PHASE_LON, PHASE_ORB_A, PHASE_ORB_E, PHASE_ORB_I,
		PHASE_ORB_ARG_PERI, PHASE_LAST_REV,
		END_COLS) ;
	if (*phases == NULL)
	{
		(void) sprintf(display_string,
				"APS DB ERROR: can't access DataBase\n") ;
		popup_message(XmDIALOG_ERROR, "APS:DB ERROR", display_string,
			XtGrabNone) ;
	}

	return (*phases);
}



/*==============================================================================
Function:			cb_update_cnomorb_form
Description:	
Parameters:		
Returns:	
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
void
cb_update_cnomorb_form(widget, client_data, cbs)
   Widget widget ;
   XtPointer client_data ;
   XtPointer cbs ;
{
BEGIN_CONTEXT( widget )

	char *sat_name ;
	DB_RECORD **phase_rec ;


	if (phases == NULL && get_phases( &phases ) == NULL)
		return;

	phase_rec = (DB_RECORD **) db_nth_record(phases,
			((XmListCallbackStruct *) cbs)->item_position) ;

	/*
	-- NOTE: get_satellite_name will not return NULL because of
	-- how it's handled in cb_show_orbit_relations()
	*/
	sat_name = get_satellite_name((char *) phase_rec[PHASE_SAT]) ;
	(void) sprintf(display_string, APS_PFMT(PHASE, PHASE_SAT), sat_name) ;
	XmTextFieldSetString(textField_sat, display_string) ;

#define SHOW_FIELD(column, cast, textField) \
	(void) sprintf(display_string, \
		APS_PFMT(PHASE, (column)), cast phase_rec[(column)]) ; \
	XmTextFieldSetString((textField), display_string) ;

	SHOW_FIELD(PHASE_PHASE_NAME, CAST_PHASE_PHASE_NAME, textField_phase_name)

	SHOW_FIELD(PHASE_CYCLE_DAYS, CAST_PHASE_CYCLE_DAYS, textField_cycle_days)
	SHOW_FIELD(PHASE_CYCLE_DAYS, CAST_PHASE_CYCLE_DAYS, textField_cycle_days2)

	SHOW_FIELD(PHASE_CYCLE_REVS, CAST_PHASE_CYCLE_REVS, textField_cycle_revs)
	SHOW_FIELD(PHASE_CYCLE_REVS, CAST_PHASE_CYCLE_REVS, textField_cycle_revs2)

	SHOW_FIELD(PHASE_PHASE_START, CAST_PHASE_PHASE_START, textField_phase_start)
	SHOW_FIELD(PHASE_PHASE_DAYS, CAST_PHASE_PHASE_DAYS, textField_phase_days)
	SHOW_FIELD(PHASE_PHASE_DAYS, CAST_PHASE_PHASE_DAYS, textField_phase_days2)

	SHOW_FIELD(PHASE_PHASE_ORBITS, CAST_PHASE_PHASE_ORBITS, textField_phase_orbits)
	SHOW_FIELD(PHASE_LAST_REV, CAST_PHASE_LAST_REV, textField_last_rev)
	SHOW_FIELD(PHASE_ORB_A, CAST_PHASE_ORB_A, textField_orb_a)
	SHOW_FIELD(PHASE_ORB_E, CAST_PHASE_ORB_E, textField_orb_e)
	SHOW_FIELD(PHASE_ORB_I, CAST_PHASE_ORB_I, textField_orb_i)

	SHOW_FIELD(PHASE_PHASE_LON, CAST_PHASE_PHASE_LON, textField_phase_lon)
	SHOW_FIELD(PHASE_PHASE_LON, CAST_PHASE_PHASE_LON, textField_phase_lon2)
	
	SHOW_FIELD(PHASE_ORB_ARG_PERI, CAST_PHASE_ORB_ARG_PERI, textField_orb_arg_peri)
#undef SHOW_FIELD

	/* enable the CreateVectorFile Pushbutton */
	set_cnomorb_form_sensitivity(NULL, TRUE) ;

END_CONTEXT
}



/*==============================================================================
Function:			cb_show_orbit_relations  
Description:	
Parameters:		
Returns:	
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
/* ARGSUSED1 */
void
cb_show_orbit_relations(widget, client_data, cbs)
	Widget widget ;
	XtPointer client_data, cbs ;
{
	int			i ;
	int			j ;
	char		*sat_string ;
	DB_RECORD	**phase_rec ;
	cursor		ptr;


	XmStringTable str_list ;
	Widget list_widget = (Widget) client_data ;

	/*
	-- get the available phases from the db table 
   	-- and allocate a String Table to hold them
	*/

	if (get_phases( &phases ) == NULL)
		return;

	str_list = (XmStringTable) XtMalloc(NUMELTS(phases) * sizeof(XmString)) ;

	for (i = 0, phase_rec = (DB_RECORD **) FIRST(phases, ptr) ;
		 phase_rec != NULL ; phase_rec = (DB_RECORD **)NEXT(phases, ptr))
	{
		if ((sat_string = get_satellite_name((char *) phase_rec[PHASE_SAT]))
			== NULL)
		{
			/*
			-- sat code is not in internal satellite table;
			-- don't handle this one: later processing is based on sat name
			*/

			/* popup error message */
			(void) sprintf( display_string,
				"WARNING: Satellite \"%s\" is not\n    in internal sat_name table\n    SKIPPING this phase record",
				(char *) phase_rec[PHASE_SAT] ) ;
			gui_aps_internal_error( XmDIALOG_ERROR,
				__FILE__, __LINE__, display_string ) ;

			/* remove from the phase list */
			(void) DEL_AT_CURSOR( phases, ptr );
			/* skip this one and get the next */
			continue;
		}

		switch (satellite_coverage_not_allowed( (char *) phase_rec[PHASE_SAT],
			(char *) NULL ))
		{
		case ERROR:
			(void) sprintf( display_string,
				"Error with the APS DB\nNeed to fix the DB and retry" ) ;
			popup_message(XmDIALOG_ERROR, "APS:DB ERROR", display_string,
				XtGrabNone) ;
			for (j = 0; j < i ; j++)
				XmStringFree(str_list[j]) ;
			XtFree((char *) str_list) ;
			return ;
		case FALSE:
			(void) sprintf(display_string,
				"%-9s  %2c    %21s %4d    %4d    %4d   %8.3f",
				sat_string,
				CAST_PHASE_PHASE_NAME phase_rec[PHASE_PHASE_NAME],
				CAST_PHASE_PHASE_START phase_rec[PHASE_PHASE_START],
				CAST_PHASE_PHASE_DAYS phase_rec[PHASE_PHASE_DAYS],
				CAST_PHASE_CYCLE_DAYS phase_rec[PHASE_CYCLE_DAYS],
				CAST_PHASE_CYCLE_REVS phase_rec[PHASE_CYCLE_REVS],
				CAST_PHASE_PHASE_LON phase_rec[PHASE_PHASE_LON]) ;
			str_list[i++] = XmStringCreateLocalized(display_string) ;
			break;
		default:
			(void) DEL_AT_CURSOR( phases, ptr );
			break;
		}
   }

	XtVaSetValues(list_widget,
		XmNitems, str_list,
		XmNitemCount, NUMELTS(phases),
		NULL) ;

	for (i = 0; i < NUMELTS(phases) ;i++)
		XmStringFree(str_list[i]) ;
	XtFree((char *) str_list) ;

	return;
}



/*==============================================================================

Function:		set_cnomorb_form_sensitivity
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
static void
set_cnomorb_form_sensitivity(process, sensitivity)
	PROCESS_INFO *process ;
	Boolean sensitivity ;
{
BEGIN_CONTEXT( cnomorb_form )

	if (process)
	{
		switch (process->exit_status)
		{
		case APS_EXIT_OK :
			(void) sprintf(display_string,
					"Create Nominal Orbit:\n\n Completed Successfully\n") ;
			popup_message(XmDIALOG_INFORMATION, "APS:INFORMATION",
					display_string, XtGrabNone);
			break ;
		case APS_EXIT_ERROR :
			(void) sprintf(display_string,
					"Create Nominal Orbit:\n\n Unsuccessful Run") ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string,
					XtGrabNone);
			break ;
		case APSGUI_EXIT_COREDUMP : /* process core dumped */
			(void) sprintf( display_string,
					"Create Nominal Orbit:\n\n Signal Caught CORE DUMPED" ) ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string,
					XtGrabNone);
			break ;
		default :	/* process caught a signal, but no core dump */
			(void) sprintf( display_string,
					"Create Nominal Orbit:\n\n SIGNAL caught (signal = %d)",
					-(process->exit_status) ) ;
			popup_message(XmDIALOG_ERROR, "APS:ERROR", display_string,
					XtGrabNone);
			break ;
		}
		TimeoutCursors(False, False, cnomorb_form) ;
	}

	XtSetSensitive(pushButton_CreateVectorFile, sensitivity) ;
	XtSetSensitive(scrolledList2, sensitivity) ;
	XtSetSensitive(pushButton_refresh1, sensitivity) ;

END_CONTEXT
}



/*==============================================================================
Function:		cb_do_cnom
Description:	
Parameters:		
Returns:		
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:		
==============================================================================*/
/* ARGSUSED1 */
void
cb_do_cnom(widget, client_data, cbs)
	Widget widget ;
	XtPointer client_data, cbs ;
{
BEGIN_CONTEXT( widget )

	int status ;
	PROCESS_INFO * process ;
	int index ;
	DB_RECORD **phase_rec ;

	/*
	-- get the selected item in the list
	-- adjust for the appropiate data record 
	-- and create the command for nominal orbit
	*/
	index = gui_XmList_selected_item(scrolledList2) ;
	phase_rec = db_nth_record(phases, index) ;

	/*
	-- NOTE: get_satellite_name will not return NULL because of
	-- how it's handled in cb_show_orbit_relations()
	*/
	(void) sprintf(question, "Create Nominal Orbit for\n\nSAT:   %s\nPHASE: %c",
		get_satellite_name((char *) CAST_PHASE_SAT phase_rec[PHASE_SAT]),
		CAST_PHASE_PHASE_NAME phase_rec[PHASE_PHASE_NAME]) ;

	if (AskUser(widget, question, YES) == NO)
		return ;	

	(void) sprintf(display_string,
		"%s -U %s -P %s %s %c",
			CREATE_NOM_ORBIT_CMD, userid, password,
			CAST_PHASE_SAT phase_rec[PHASE_SAT], 
			CAST_PHASE_PHASE_NAME phase_rec[PHASE_PHASE_NAME]) ;

	(void) printf("COMMAND: %s\n", display_string) ; 

	/* create and start the process */
	process = (PROCESS_INFO *) create_process(display_string, &status, 
		TRUE, NULL, gui_display_message_widget, scrolledText_cnomorb_status, 
		set_cnomorb_form_sensitivity, (void *) True) ;

	/* clear the message/status window */
	XmTextSetString(scrolledText_cnomorb_status, EMPTY_STR) ;

	if (!process)
	{
		(void) sprintf(display_string,
			"Can't create nom. orbit process\n Too many processes running?"
			) ;
		popup_message(XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone);
	}
	else
	{
		/* 
		-- since we only want to run one of these at a time
		-- disable the necessary items on this form  
		*/
		set_cnomorb_form_sensitivity((PROCESS_INFO *) NULL, FALSE) ;

		if (start_process(process))
			destroy_process( process ) ;
		else
			TimeoutCursors(True, True, cnomorb_form) ;
	}

END_CONTEXT
}
