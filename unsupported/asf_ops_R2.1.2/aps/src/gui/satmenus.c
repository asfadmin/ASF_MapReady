#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.  U.S. Government
Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:	satmenus.c

Description:
			Functions to get the basic information on a satellite
			e.g. sat, sensors, stations etc.  This info is used
			later to build menus

External Functions:
			get_satellite_name
			get_satellite_code
			get_sat_code_from_ims_name
			satellite_coverage_not_allowed
			cb_build_satellite_option_menu
			cb_build_cvrg_allowed_satellite_option_menu
			cb_build_sensor_option_menu
			cb_build_cvrg_allowed_sensor_option_menu
			cb_build_station_option_menu
			set_satellite_menu
			cb_set_sensor_menus
			cb_set_cvrg_allowed_sensor_menus
			get_default_sensor
			cb_set_station_menus
			set_stationid_menu
			cb_build_antenna_option_menu
			cb_set_antenna_menus
			set_antenna_menu

Static Functions:
			sat_popup_message
			sat_aps_internal_error
			free_sat_record
			get_satinfo_records
			get_stn_antenna_records
			build_satellite_option_menu
			build_sensor_option_menu
			set_sensor_menus
			free_stn_antenna_record
			free_antennas_item
			aps_db_error_EXIT	 *** APS db error msg; then EXITs ***

External Variables Defined:

File Scope Static Variables:
			sat
			satellites
			stn_antennas

Notes:		EXITS if there are APS db problems so some option menus cannot
			be built.

==============================================================================*/
#pragma ident	"@(#)satmenus.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.satmenus.c"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <malloc.h>
#include <string.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include "UxXt.h"

#include "dapps_defs.h"
#include "aps.h"
#include "aps_defs.h"
#include "db_sybint.h"
#include "aps_db_table.h"
#include "db_satsensor.h"
#include "db_station.h"
#include "db_antenna.h"
#include "aps_extern.h"
#include "nmalloc.h"

#include "satmenus.h"
#include "gui_utils.h"

static void		aps_db_error_EXIT() ;

extern void		popup_message() ;
extern char		display_string[] ;

extern Widget	mainIface;

llist			*satellites = NULL ;
static llist	*stn_antennas = NULL ;

static char		no_antennaStr[APS_MAX_ANTENNA_LEN+1];

#ifdef	DEBUG_PRINT
	static void	print_satellites()
	{
		SATELLITE	*sat_rec ;
		cursor		sat_ptr;
		DB_RECORD	**sensor_rec ;
		cursor		sensor_ptr ;

		for (sat_rec = (SATELLITE *) FIRST(satellites, sat_ptr)
			; sat_rec != (SATELLITE *) NULL
			; sat_rec = (SATELLITE *) NEXT(satellites, sat_ptr))
		{
			(void) printf( "%10s %3s %-10s (cvrg)\n",
				"satellite:", sat_rec->sat, "(sensor)" );
			for (sensor_rec = (DB_RECORD **) FIRST(sat_rec->sensors, sensor_ptr)
				; sensor_rec != (DB_RECORD **)NULL
				; sensor_rec = (DB_RECORD **)NEXT(sat_rec->sensors, sensor_ptr))
			{
				(void) printf( "%14s  %-10s  %c\n", " ",
					(char *) CAST_SATSENSOR_SENSOR sensor_rec[SATSENSOR_SENSOR],
					(char)CAST_SATSENSOR_CVRG_ALLOWED
					sensor_rec[SATSENSOR_CVRG_ALLOWED] );
			}
			(void) printf( "\n" );
		}
	}
#endif	/*DEBUG_PRINT*/

typedef
	struct SATSENSOR_NO_CVRG
	{
		char *satcode;
		char *sensor;
	} SATSENSOR_NO_CVRG;

/*
-- array of sat/sensors with coverage not allowed;
-- NULL sensor means coverage never allowed for this sat.
*/
static SATSENSOR_NO_CVRG *satsensor_no_cvrg=NULL;

/* TODO Build the "sat" table dynamically */
typedef
	struct _SAT
	{
		char *satcode ;
		char *satname ;
		char *ims_satname ;
	} SAT ;

static SAT sat[] =
{
	{"A1",	"ADEOS",	""},
	{"E1",	"ERS-1",	"ERS-1"},
	{"E2",	"ERS-2",	"ERS-2"},
	{"J1",	"JERS-1",	"JERS-1"},
	{"R1",	"RADARSAT",	"RADARSAT-1"},
	{NULL,	NULL,		NULL}
} ;

/*==============================================================================
Function:		get_satellite_name
Function:		get_satellite_code
Function:		get_sat_code_from_ims_name

Description:

Parameters:

Returns:		NULL if argument is not in the "sat" table

Creator:		Ron Green

Creation Date:	09/dd/1994

Notes:
==============================================================================*/
char *
get_satellite_name(code)
	char *code ;
{
	int i ;

	for (i = 0 ; sat[i].satname != NULL; i++)
	{
		if (strcmp(code, sat[i].satcode) == 0)
			return(sat[i].satname) ;
	}

	return(NULL) ;
}

char *
get_satellite_code(name)
	char *name ;
{
	int i ;

	for (i = 0 ; sat[i].satname != NULL; i++)
	{
		if (strcmp(name, sat[i].satname) == 0)
			return(sat[i].satcode) ;
	}

	return(NULL) ;
}

char *
get_sat_code_from_ims_name( imsName )
	char *imsName ;
{
	int i ;

	for (i = 0 ; sat[i].satname != NULL; i++)
	{
		if (strcmp(imsName, sat[i].ims_satname) == 0)
			return(sat[i].satcode) ;
	}

	return(NULL) ;
}



/*==============================================================================
Function:		sat_popup_message
Description:	Tries to use "popup_message" to display a message, but if
				can't, prints the message to stderr
Parameters:
Returns:		None
Creator:		Teresa McKillop
Creation Date:	04/29/96
Notes:
==============================================================================*/
static void
sat_popup_message( int dialog_type, char *dialog_title, char *msg,
	XtGrabKind grab_kind )
{
	if (mainIface)
		popup_message( dialog_type, dialog_title, msg, grab_kind ) ;
	else
		(void) fprintf ( stderr, "%s : %s\n", dialog_title, msg ) ;

	return ;
}



/*==============================================================================
Function:		sat_aps_internal_error
Description:	Tries to use "gui_aps_internal_error" to display a message,
				but if can't, prints the message to stderr
Parameters:
Returns:		None
Creator:		Teresa McKillop
Creation Date:	04/29/96
Notes:
==============================================================================*/
static void
sat_aps_internal_error( int	dialog_type, char *source_file, int line_number,
	char *msg )
{
	if (mainIface)
		gui_aps_internal_error( dialog_type, source_file, line_number, msg ) ;
	else
		aps_internal_error( stderr, source_file, line_number, msg ) ;

	return ;
}



/*==============================================================================
Function:		free_sat_record
Description:	frees the "SATELLITE" portion in a satellites list item
Parameters:
Returns:		None
Creator:		Teresa McKillop
Creation Date:	12/19/95
Notes:
==============================================================================*/
static void
free_sat_record( SATELLITE	*sat_rec )
{
	DEL_LIST( sat_rec->sensors );
	DEL_LIST( sat_rec->stations );
	free( sat_rec );
}



/*==============================================================================
Function:		free_stn_antenna_record
Description:	frees the "STN_ANTENNAS" portion in a stn_antenna list item
Parameters:
Returns:		None
Creator:		Teresa McKillop
Creation Date:	12/19/95
Notes:
==============================================================================*/
static void
free_stn_antenna_record( STN_ANTENNAS *stn_antenna_rec )
{
	DEL_LIST( stn_antenna_rec->antennas );
	free( stn_antenna_rec->station );
	free( stn_antenna_rec );
}



/*==============================================================================
Function:		free_antennas_item
Description:	frees the item in an antennas list record
Parameters:
Returns:		None
Creator:		Teresa McKillop
Creation Date:	12/20/95
Notes:
==============================================================================*/
static void
free_antennas_item( char *antennaNumStr )
{
	free( antennaNumStr );
}



/*==============================================================================
Function:		get_satinfo_records
Description:	gets the info for the global satellite list and
				creates the no-cvrg table of satellites
Parameters:
Returns:		TRUE if no errors;
				FALSE if can't access the APS db.
Creator:		Ron Green
Creation Date:	mm/dd/yyyy
Notes:			** WARNING **: the current satellite list and no-cvrg table,
				if they exist, will be destroyed
==============================================================================*/
static int
get_satinfo_records()
{
	/* dbase record variables */
	llist		*records ;
	DB_RECORD	**record ;
	cursor		ptr ;

	/* satellite information variables */
	SATELLITE	*sat_rec ;
	cursor		sat_ptr ;
	char		*current_rec_sat;

	/* satellite coverage variables */
	char		*current_rec_sensor;
	char		current_rec_cvrg_allowed;
	int			numElements_sat_no_cvrg = 0;
	int			found;

	int			nrecs;
	int			i;

	if (satellites)
		DEL_LIST( satellites );
	if (satsensor_no_cvrg)
	{
		for (i = 0 ; satsensor_no_cvrg[i].satcode != NULL ; i++ )
		{
			free( satsensor_no_cvrg[i].satcode );
			free( satsensor_no_cvrg[i].sensor );
		}
		free( satsensor_no_cvrg );
	}

	/*
	-- get the records from the satsensor relation
	-- every satellite has at least one sensor
	*/
	records = db_get_records(APS_dbproc,
		APS_TABLE(SATSENSOR), NULL, NULL, APS_CDEFS(SATSENSOR),
		SATSENSOR_SAT, SATSENSOR_SENSOR, SATSENSOR_CVRG_ALLOWED, END_COLS) ;
	if (!records)
		return (FALSE) ;

	/*
	-- now 1) create the array of sat/sensors with coverage not allowed
	-- and 2) create a list for the satellites,
	-- each item in the satellite list contains the satellite
	-- followed by a list of sensors and stations
	*/
	satellites = create_dyn_llist() ;
	for (record = (DB_RECORD **) FIRST(records, ptr) ; record
		; record = (DB_RECORD **) NEXT(records, ptr))
	{
		current_rec_sat = (char *) CAST_SATSENSOR_SAT record[SATSENSOR_SAT];

		/* create the list for the satellites */
		/* do the processing for creating the array of sat/sensors w/out cvrg */
		found = FALSE;
		current_rec_sensor =
			(char *) CAST_SATSENSOR_SENSOR record[SATSENSOR_SENSOR];
		current_rec_cvrg_allowed =
			(char) CAST_SATSENSOR_CVRG_ALLOWED record[SATSENSOR_CVRG_ALLOWED];
		switch (current_rec_cvrg_allowed)	/* SWITCH CVRG ALLOWED */
		{
			case 'N':
			case 'n':	/* coverage is NOT allowed for this sat/sensor */
				for (i = 0 ; i < numElements_sat_no_cvrg ; i++ ) /*FOR NO-CVRG*/
				{
					if (strcmp( satsensor_no_cvrg[i].satcode,
						current_rec_sat ) == 0)
					{
						found = TRUE;
						break;	/* FOR NO-CVRG */
					}
				}

				if (found == TRUE)	/* the sat is in no-cvrg table */
				{
					/* is the entry already in the table? */
					if (satsensor_no_cvrg[i].sensor == NULL
						|| strcmp( satsensor_no_cvrg[i].sensor,
								  current_rec_sensor ) == 0)
					{
						/* entry is already in no-cvrg table: DONE */
						break;	/* SWITCH CVRG ALLOWED */
					}
					nrecs = 1;	/* some sensor has cvrg allowed for this sat */
				}
				else	/* sat is not in no-cvrg, see if cvrg is ever allowed */
				{
					(void) sprintf( where_clause,
						"\nwhere %s = '%s' and %s != '%c'",
						APS_COL( SATSENSOR, SATSENSOR_SAT ), current_rec_sat,
						APS_COL( SATSENSOR, SATSENSOR_CVRG_ALLOWED ),
						current_rec_cvrg_allowed );
					nrecs = db_num_records( APS_dbproc, APS_TABLE( SATSENSOR ),
						where_clause );
				}

				/* sat/sensor needs to be inserted into no-cvrg table */
				satsensor_no_cvrg = realloc( satsensor_no_cvrg,
					sizeof (SATSENSOR_NO_CVRG) * (numElements_sat_no_cvrg + 1));
				satsensor_no_cvrg[numElements_sat_no_cvrg].satcode =
					(char *) malloc( strlen(current_rec_sat ) + 1 );
				(void)strcpy(
					satsensor_no_cvrg[numElements_sat_no_cvrg].satcode,
					current_rec_sat );
				if (nrecs)
				{
					satsensor_no_cvrg[numElements_sat_no_cvrg].sensor =
						(char *) malloc( strlen(current_rec_sensor ) + 1 );
					(void)strcpy(
						satsensor_no_cvrg[numElements_sat_no_cvrg].sensor,
						current_rec_sensor );
				}
				else
				{
					satsensor_no_cvrg[numElements_sat_no_cvrg].sensor = NULL;
				}
				numElements_sat_no_cvrg++;

				break;
			default:	/* cvrg IS allowed for this sat/sensor, do nothing */
				break;
		}

		for (sat_rec = (SATELLITE *) FIRST(satellites, sat_ptr) ; sat_rec
			; sat_rec = (SATELLITE *) NEXT(satellites, sat_ptr))
		{
			/* if this is a sensor on a previously defined satellite */
			if (strcmp( sat_rec->sat, current_rec_sat ) == 0)
			{
				/*
				-- add the sensor to the satellite's sensor list
				-- then get the next sensor to process
				*/
				record = (DB_RECORD **) UNLINK_AT_CURSOR( records, ptr );
				if (record != NULL)	/* should never be NULL, so ignoring */
					APPEND(sat_rec->sensors, record, free_db_record, record) ;
				break ;
			}
		}

		/* if this sensor is on an undefined satellite */
		if (!sat_rec)
		{
			/*
			-- add the new satellite to the satellite list
			-- also create the satellite's sensor and station
			-- lists and add the sensor
			*/
			sat_rec = (SATELLITE *) NEW(sizeof(SATELLITE)) ;
			sat_rec->sat = current_rec_sat ;
			if ((sat_rec->satname = get_satellite_name(sat_rec->sat)) == NULL)
			{
				(void) sprintf( display_string,
					"WARNING: Satellite %s is not in sat_name table, ignoring it",
					sat_rec->sat ) ;
				sat_aps_internal_error( XmDIALOG_WARNING,
					__FILE__, __LINE__, display_string ) ;
				FREE( sat_rec );
				continue ;
			}

			sat_rec->sensors = create_dyn_llist() ;
			sat_rec->stations = create_dyn_llist() ;

			APPEND(satellites, sat_rec, free_sat_record, sat_rec) ;
			record = (DB_RECORD **) UNLINK_AT_CURSOR( records, ptr );
			if (record != NULL)	/* should never be NULL, so ignoring */
				APPEND(sat_rec->sensors, record, free_db_record, record) ;
		}
	}

	/* insert a termination element in the no-cvrg table */
	satsensor_no_cvrg = realloc( satsensor_no_cvrg,
		sizeof (SATSENSOR_NO_CVRG) * (numElements_sat_no_cvrg + 1) );
	satsensor_no_cvrg[numElements_sat_no_cvrg].satcode = NULL;
	satsensor_no_cvrg[numElements_sat_no_cvrg++].sensor = NULL;

	/* free up whatever is left in the records list */
	DEL_LIST( records );

	/*
	-- get the records from the station relation
	-- every satellite has at least one station
	*/
	records = db_get_records(APS_dbproc,
		APS_TABLE(STATION), NULL, NULL, APS_CDEFS(STATION),
		STATION_SAT, STATION_STATIONID,
		END_COLS) ;
	if (!records)
		return (FALSE) ;

	for (record = (DB_RECORD **) FIRST(records, ptr) ; record
		; record = (DB_RECORD **) NEXT(records, ptr))
	{
		sat_rec = (SATELLITE *) FIRST(satellites, sat_ptr)  ;
		while (sat_rec)
		{
			/*
			-- if this is a station associated with a
			-- previously defined satellite
			*/
			if (strcmp(sat_rec->sat,
				(char *) CAST_STATION_SAT record[STATION_SAT]) == 0)
			{
				/*
				-- add the station to the satellite's station list
				-- then get the next station to process
				*/
				record = (DB_RECORD **) UNLINK_AT_CURSOR( records, ptr );
				if (record != NULL)	/* should never be NULL, so ignoring */
					APPEND(sat_rec->stations, record, free_db_record, record) ;
				break ;
			}
			sat_rec = (SATELLITE *) NEXT(satellites, sat_ptr) ;
		}

		/* if this station is associated with an undefined satellite */
		if (!sat_rec)
		{
			/*
			-- add the new satellite to the satellite list
			-- also create the satellite's sensor and station
			-- lists and add the sensor
			*/
			sat_rec = (SATELLITE *) NEW(sizeof(SATELLITE)) ;
			sat_rec->sat = (char *) CAST_STATION_SAT record[STATION_SAT] ;
			if ((sat_rec->satname = get_satellite_name(sat_rec->sat)) == NULL)
			{
				(void) sprintf( display_string,
					"WARNING: Satellite %s is not in sat_name table, ignoring it",
					sat_rec->sat ) ;
				sat_aps_internal_error( XmDIALOG_WARNING,
					__FILE__, __LINE__, display_string ) ;
				FREE( sat_rec );
				continue ;
			}

			sat_rec->sensors = create_dyn_llist() ;
			sat_rec->stations = create_dyn_llist() ;

			APPEND(satellites, sat_rec, free_sat_record, sat_rec) ;
			record = (DB_RECORD **) UNLINK_AT_CURSOR( records, ptr );
			if (record != NULL)	/* should never be NULL, so ignoring */
				APPEND(sat_rec->stations, record, free_db_record, record) ;
		}
	}

	/* free up whatever is left in the records list */
	DEL_LIST( records );

#ifdef	DEBUG_PRINT
	print_satellites();
#endif /*DEBUG_PRINT */

	return (TRUE) ;
}



/*==============================================================================
Function:		get_stn_antenna_records
Description:	gets the antenna relation records and makes a linked list
				of stations with a list of their antennas (also adds "0", no
				antenna to the list)
Parameters:
Returns:		TRUE if made the list;
				FALSE if APS db errors occurred.
Creator:		Teresa McKillop
Creation Date:	12/19/95
Notes:			** WARNING **: the current stn_antenna list, if it exists,
				will be destroyed
==============================================================================*/
static int
get_stn_antenna_records()
{
	/* dbase record variables */
	llist			*records ;
	DB_RECORD		**record ;
	cursor			ptr ;

	/* stn_antenna information variables */
	STN_ANTENNAS	*stn_antenna_rec ;
	cursor			stn_antenna_ptr ;
	char			*current_rec_stn_antenna;

	char			tmpAntennaStr[APS_MAX_ANTENNA_LEN+1];
	char			*tmpMallocAntenna;

	/* destroy the current stn_antenna list */
	if (stn_antennas)
		DEL_LIST( stn_antennas );

	/* set the no-antenna string */
	(void) sprintf( no_antennaStr, "%d", APS_NO_ANTENNA );

	/*
	-- get the records from the antenna relation
	-- every station has at least one antenna
	*/
	records = db_get_records( APS_dbproc,
		APS_TABLE(ANTENNA), NULL, NULL, APS_CDEFS(ANTENNA),
		ANTENNA_STATION_ID, ANTENNA_ANTENNA_ID, END_COLS ) ;
	if (!records)
		return (FALSE) ;

	/*
	-- now 1) create a list for the stations,
	--   (each item in the stn_antenna list contains the station
	--   followed by a list of antennas)
	*/
	stn_antennas = create_dyn_llist() ;
	for (record = (DB_RECORD **) FIRST( records, ptr )  ; record
		; record = (DB_RECORD **) NEXT( records, ptr ))
	{
		current_rec_stn_antenna = (char *) record[ANTENNA_STATION_ID];
		/* create the list for the stations */
		for (stn_antenna_rec =
				(STN_ANTENNAS *) FIRST( stn_antennas, stn_antenna_ptr )
			; stn_antenna_rec ; stn_antenna_rec =
				(STN_ANTENNAS *) NEXT( stn_antennas, stn_antenna_ptr ))
		{
			/* if this is an antenna on a previously defined station */
			if (strcmp( stn_antenna_rec->station, current_rec_stn_antenna) == 0)
			{
				(void) sprintf( tmpAntennaStr, "%d",
					CAST_ANTENNA_ANTENNA_ID record[ANTENNA_ANTENNA_ID] );
				tmpMallocAntenna = (char *)malloc(
					(int)strlen( tmpAntennaStr ) + 1);
				(void) strcpy( tmpMallocAntenna, tmpAntennaStr );
				/*
				-- add the antenna to the station's antenna list
				*/
				APPEND( stn_antenna_rec->antennas, tmpMallocAntenna,
					free_antennas_item, tmpMallocAntenna ) ;
				break ;
			}
		}

		/* if this antenna is on an undefined station */
		if (!stn_antenna_rec)
		{
			/*
			-- add the new station to the stn_antenna list
			-- also create the station's antenna list
			-- and add "0", no antenna, and the current antenna
			*/
			stn_antenna_rec = (STN_ANTENNAS *) NEW( sizeof (STN_ANTENNAS) ) ;
			stn_antenna_rec->station =
				(char *) malloc( strlen( current_rec_stn_antenna ) + 1 ) ;
            (void)strcpy( stn_antenna_rec->station, current_rec_stn_antenna ) ;

			stn_antenna_rec->antennas = create_dyn_llist() ;

			APPEND( stn_antennas, stn_antenna_rec,
				free_stn_antenna_record, stn_antenna_rec ) ;

			tmpMallocAntenna = (char *)malloc((int)strlen( no_antennaStr ) + 1);
			(void) strcpy( tmpMallocAntenna, no_antennaStr );
			APPEND( stn_antenna_rec->antennas, tmpMallocAntenna,
				free_antennas_item, tmpMallocAntenna ) ;

			(void) sprintf( tmpAntennaStr, "%d",
				CAST_ANTENNA_ANTENNA_ID record[ANTENNA_ANTENNA_ID] );
			tmpMallocAntenna = (char *)malloc((int)strlen( tmpAntennaStr ) + 1);
			(void) strcpy( tmpMallocAntenna, tmpAntennaStr );
			APPEND( stn_antenna_rec->antennas, tmpMallocAntenna,
				free_antennas_item, tmpMallocAntenna ) ;
		}
	}

	/* free up the records list */
	DEL_LIST( records );

	return (TRUE) ;
}


/*==============================================================================
Function:		satellite_coverage_not_allowed
Description:	checks to see if coverage is not allowed for a sat/sensor
				pair or is disallowed for an entire satellite.
Parameters:		char *	code: the satellite's code
				char *  sensor: the sensor code, if NULL check for disallowed
Returns:		TRUE	- coverage is NOT allowed
				FALSE	- coverage is allowed.
				ERROR	- an APS db error occurred, couldn't check coverage.
Creator:		Teresa McKillop
Creation Date:	11/06/95
Notes:
==============================================================================*/
int
satellite_coverage_not_allowed( char *code, char *sensor )
{
	int		i;
	int		ReturnsValue = FALSE;

	if ((code == NULL)
		|| (satsensor_no_cvrg == NULL && get_satinfo_records() == FALSE))
	{
		return (ERROR) ;
	}

	for (i = 0 ; satsensor_no_cvrg[i].satcode != NULL ; i++ )
	{
		if (strcmp( satsensor_no_cvrg[i].satcode, code ) == 0)
		{

			if (satsensor_no_cvrg[i].sensor == NULL)
			{
				/* coverage is disallowed for this satellite: therefore the
				-- checks for individual sensors and for disallows are true
				*/
				ReturnsValue = TRUE;
				break;
			}
			if (sensor == NULL)
			{
				/* cvrg is not disallowed: so the check for disallow is false */
				break;
			}
			if (strcmp( satsensor_no_cvrg[i].sensor, sensor ) == 0)
			{
				/* coverage is not allowed for this sat/sensor */
				ReturnsValue = TRUE;
				break;
			}
		}
	}

	return (ReturnsValue);
}



/*==============================================================================
Function:		build_satellite_option_menu
Description:
Parameters:
				check_cvrg_allowed	- if TRUE, menu will exclude satellites
										that have coverage disallowed
									- if FALSE will include all satellites
Returns:		TRUE - if option menu is built;
				FALSE if it is not built
Creator:		T. McKillop (modified R Green's cb_build_satellite_option_menu)
Creation Date:	11/06/95
Notes:
==============================================================================*/
static int
build_satellite_option_menu( Widget widget, int check_cvrg_allowed )
{
	Widget temp ;
	Widget satmenu ;
	XmFontList fontlist ;
	XmString menu_str ;

	SATELLITE *sat_rec ;
	cursor sat_ptr ;

	/*
	-- this shell is used to create a secondary menu
	-- for the SATELLITE option menu ;
	-- the menu added to this shell will be used as
	-- the XmNsubmenuID of optionMenu_satellite
	*/
	satmenu = XtParent(widget) ;

	/*
	-- get the currently set resources for the menu
	-- so we can match when we create the new menu;
	-- use the item menu passed in
	*/
	XtVaGetValues(widget,
		XmNfontList, &fontlist,
		NULL) ;

	if (!satellites && get_satinfo_records() == FALSE)
		return (FALSE) ;

	for (sat_rec = FIRST(satellites, sat_ptr) ; sat_rec != NULL
		; sat_rec = (SATELLITE *) NEXT(satellites, sat_ptr))
	{
		if (check_cvrg_allowed == TRUE)
		{
			switch (satellite_coverage_not_allowed(
				sat_rec->sat, (char *) NULL ))
			{
				case ERROR:
					aps_db_error_EXIT() ;	/* EXITs */
					break;	/* never get here; needed for lint */
				case TRUE:
					/* cvrg is disallowed for this satellite, skip it */
					continue;
				default:
					break;
			}
		}

		/* add the satellite to the satellite option menu */
		menu_str = XmStringCreateLocalized(sat_rec->satname) ;
   		temp = XtVaCreateManagedWidget(sat_rec->satname,
			xmPushButtonWidgetClass, satmenu,
			XmNlabelString, menu_str,
			XmNfontList, fontlist,
			NULL) ;
		XmStringFree(menu_str) ;
	}

	/*
	-- set default menu choice to the first satellite in the list.
	-- This assumes that if a sensor or station option menu
	-- their option values will also be set to valid first satellite options
	*/
	sat_rec = FIRST(satellites, sat_ptr) ;
	temp = XtNameToWidget( satmenu, sat_rec->satname ) ;
	XtVaSetValues(satmenu,
		XmNmenuHistory, temp,
		NULL) ;

	/* now remove the default choice, since it's only a place holder */
	XtUnmanageChild(widget) ;

	return (TRUE) ;
}



/*==============================================================================
Function:		cb_build_satellite_option_menu
Description:
Parameters:
Returns:		None
Creator:		Teresa McKillop
Creation Date:	11/06/95
Notes:
==============================================================================*/
/* ARGSUSED1 */
void
cb_build_satellite_option_menu( Widget widget, XtPointer client_data,
	XtPointer cbs )
{
	if (build_satellite_option_menu( widget, FALSE ) == FALSE)
		aps_db_error_EXIT();	/* EXITs */

	return ;
}


/*==============================================================================
Function:		cb_build_cvrg_allowed_satellite_option_menu
Description:
Parameters:
Returns:		None
Creator:		Teresa McKillop
Creation Date:	11/06/95
Notes:
==============================================================================*/
/* ARGSUSED1 */
void
cb_build_cvrg_allowed_satellite_option_menu( Widget widget,
	XtPointer client_data, XtPointer cbs )
{
	if (build_satellite_option_menu( widget, TRUE ) == FALSE)
		aps_db_error_EXIT() ;	/* EXITs */

	return ;
}



/*==============================================================================
Function:		build_sensor_option_menu
Description:
Parameters:		check_cvrg_allowed	- if TRUE, menu will contain only
										sat/sensors with coverage allowed
									- if FALSE will include all sat/sensors
Returns:		TRUE if sensor menu is built;
				FALSE if it is not built
Creator:		T. McKillop (modified R Green's cb_build_sensor_option_menu)
Creation Date:	11/06/95
Notes:
==============================================================================*/
static int
build_sensor_option_menu(Widget widget, int check_cvrg_allowed )
{
	DB_RECORD **sensor_rec ;
	cursor sensor_ptr ;

	XmFontList fontlist ;
	XmString menu_str ;

	Widget temp ;
	Widget sensormenu ;

	SATELLITE *sat_rec ;
	cursor sat_ptr ;

	char *sensorname ;
	int	defaultSet = FALSE ;


	/*
	-- get the option menu holding this button
	-- it will be needed to add the sensors to
	*/
	sensormenu = XtParent(widget) ;

	/*
	-- get the currently set resources for the menu
	-- so we can match when we create the new menu;
	-- use the item menu passed in
	*/
	XtVaGetValues(widget,
		XmNfontList, &fontlist,
		NULL) ;

	if (!satellites && get_satinfo_records() == FALSE)
		return (FALSE) ;

	for (sat_rec = FIRST(satellites, sat_ptr)  ; sat_rec != NULL
		; sat_rec = (SATELLITE *) NEXT(satellites, sat_ptr))
	{
		/* create the sensor menu */

		for (sensor_rec = (DB_RECORD **) FIRST(sat_rec->sensors, sensor_ptr)
			; sensor_rec != NULL
			; sensor_rec = (DB_RECORD **) NEXT(sat_rec->sensors, sensor_ptr))
		{
			sensorname =
				(char *) CAST_SATSENSOR_SENSOR sensor_rec[SATSENSOR_SENSOR] ;
			if (check_cvrg_allowed == TRUE)
			{
				switch (satellite_coverage_not_allowed(
					sat_rec->sat, sensorname ))
				{
					case ERROR:
						aps_db_error_EXIT() ;	/* EXITs */
						break;	/* never get here; needed for lint */
					case TRUE:
						/* cvrg is disallowed for this satellite, skip it */
						continue;
					default:
						break;
				}
			}

			/*
			-- if the sensor has not already been
			-- added to the menu, add it
			*/
			if (!XtNameToWidget(sensormenu, sensorname))
			{
				/* add the sensor to the sensor option menu */
				menu_str = XmStringCreateLocalized(sensorname) ;
   				temp = XtVaCreateWidget(sensorname,
					xmPushButtonWidgetClass, sensormenu,
					XmNlabelString, menu_str,
					XmNfontList, fontlist,
					NULL) ;
				XmStringFree(menu_str) ;
			}
		}
	}

	/*
	-- set default sensor menu choices to the sensors of the 1st sat.
	-- This assumes that any satellite or station option menu
	-- have their option values also set to valid 1st sat options.
	-- Also use the first menu as the default sensor menu.
	*/

	sat_rec = (SATELLITE *) FIRST(satellites, sat_ptr) ;

	/* set the sensor menu choices for the chosen satellite */
	for (sensor_rec = (DB_RECORD **) FIRST(sat_rec->sensors, sensor_ptr)
		; sensor_rec
		; sensor_rec = (DB_RECORD **) NEXT(sat_rec->sensors, sensor_ptr))
	{
		temp = XtNameToWidget( sensormenu,
			(char *) CAST_SATSENSOR_SENSOR sensor_rec[SATSENSOR_SENSOR]) ;
		if (temp)
		{
			XtManageChild(temp) ;
			if (defaultSet == FALSE)
			{
				XtVaSetValues(sensormenu,
					XmNmenuHistory, temp,
					NULL) ;
				defaultSet = TRUE ;
			}
		}
	}

	/* now remove the default choice, since it's only a place holder */
	XtUnmanageChild(widget) ;

	return (TRUE) ;
}



/*==============================================================================
Function:		cb_build_sensor_option_menu
Description:
Parameters:
Returns:		None
Creator:		T. McKillop
Creation Date:	11/06/95
Notes:
==============================================================================*/
/* ARGSUSED1 */
void
cb_build_sensor_option_menu(Widget widget, XtPointer client_data,
	XtPointer cbs)
{
	if (build_sensor_option_menu( widget, FALSE ) == FALSE)
		aps_db_error_EXIT() ;	/* EXITs */

	return ;
}


/*==============================================================================
Function:		cb_build_cvrg_allowed_sensor_option_menu
Description:
Parameters:
Returns:		None
Creator:		T. McKillop (modified R Green's cb_build_sensor_option_menu)
Creation Date:	11/06/95
Notes:
==============================================================================*/
/* ARGSUSED1 */
void
cb_build_cvrg_allowed_sensor_option_menu(Widget widget, XtPointer client_data,
	XtPointer cbs)
{
	if (build_sensor_option_menu( widget, TRUE ) == FALSE)
		aps_db_error_EXIT() ;	/* EXITs */

	return ;
}



/*==============================================================================
Function:		cb_build_station_option_menu
Description:
Parameters:
Returns:		None
Creator:		Ron Green
Creation Date:	10/26/1994
Notes:			EXITS if there are APS db problems so some option menus cannot
				be built.
==============================================================================*/
/* ARGSUSED1 */
void
cb_build_station_option_menu(Widget widget, XtPointer client_data,
	XtPointer cbs)
{
	DB_RECORD **stn_rec ;
	cursor stn_ptr ;

	SATELLITE *sat_rec ;
	cursor sat_ptr ;

	XmFontList fontlist ;
	XmString menu_str ;

	Widget temp ;
	Widget stationmenu ;

	char *stationid ;
	int	defaultSet = FALSE ;

	/*
	-- get the option menu holding this button
	-- it will be needed to add the sensors to
	*/
	stationmenu = XtParent(widget) ;

	/*
	-- get the currently set resources for the menu
	-- so we can match when we create the new menu;
	-- use the item menu passed in
	*/
	XtVaGetValues(widget,
		XmNfontList, &fontlist,
		NULL) ;

	if (!satellites && get_satinfo_records() == FALSE)
		aps_db_error_EXIT() ;	/* EXITs */

	for (sat_rec = FIRST(satellites, sat_ptr)  ; sat_rec
		; sat_rec = (SATELLITE *) NEXT(satellites, sat_ptr))
	{
		for (stn_rec = (DB_RECORD **) FIRST(sat_rec->stations, stn_ptr)
			; stn_rec
			; stn_rec = (DB_RECORD **) NEXT(sat_rec->stations, stn_ptr))
		{
			stationid = (char *) stn_rec[STATION_STATIONID] ;

			/*
			-- if the station has not already been added
			-- to the menu if not add it
			*/
			if (!XtNameToWidget(stationmenu, stationid))
			{
				/* add the station to the station option menu */
				menu_str = XmStringCreateLocalized(stationid) ;
   				temp = XtVaCreateWidget(stationid,
					xmPushButtonWidgetClass, stationmenu,
					XmNlabelString, menu_str,
					XmNfontList, fontlist,
					NULL) ;
				XmStringFree(menu_str) ;
			}
		}
	}

	/*
	-- set default station menu choices to the stations for the 1st sat.
	-- This assumes that any satellite or sensor option menus
	-- have their option values also set to valid 1st sat options.
	-- Also use the first menu as the default station menu.
	*/

	sat_rec = (SATELLITE *) FIRST(satellites, sat_ptr) ;

	for (stn_rec = (DB_RECORD **) FIRST(sat_rec->stations, stn_ptr)
		; stn_rec
		; stn_rec = (DB_RECORD **) NEXT(sat_rec->stations, stn_ptr))
	{

		temp = XtNameToWidget( stationmenu,
			(char *) CAST_STATION_STATIONID stn_rec[STATION_STATIONID]) ;
		if (temp)
		{
			XtManageChild(temp) ;
			if (defaultSet == FALSE)
			{
				XtVaSetValues(stationmenu,
					XmNmenuHistory, temp,
					NULL) ;
				defaultSet = TRUE ;
			}
		}
		else
		{
			(void) sprintf( display_string, "NO STATION FOR %s",
				(char*) stn_rec[STATION_STATIONID] ) ;
			sat_popup_message( XmDIALOG_ERROR, "APS:ERROR",
				display_string, XtGrabNone ) ;
		}
	}

	/* now remove the default choice, since it's only a place holder */
	XtUnmanageChild(widget) ;
}



/*==============================================================================
Function:		cb_build_antenna_option_menu
Description:
Parameters:		client_data: True if no-antenna is to be one of the choices
							 otherwise False
Returns:		None
Creator:		T. McKillop
Creation Date:	12/20/95
Notes:			EXITS if there are APS db problems so some option menus cannot
				be built.
==============================================================================*/
/* ARGSUSED2 */
void
cb_build_antenna_option_menu( Widget widget, XtPointer client_data,
	XtPointer cbs )
{
	char			*antennaStr ;
	cursor			antenna_ptr ;

	XmFontList		fontlist ;
	XmString		menu_str ;

	Widget			antennamenu ;
	Widget			temp ;

	STN_ANTENNAS	*stn_antenna_rec ;
	cursor			stn_antenna_ptr ;

	Boolean			noAntenna_flag;
	int				defaultSet = FALSE ;

	/*
	-- get the option menu holding this button
	-- it will be needed to add the sensors to
	*/
	antennamenu = XtParent(widget) ;

	/* see if no-antenna should be one of the antennas */
	noAntenna_flag = (Boolean) client_data;

	/*
	-- get the currently set resources for the menu
	-- so we can match when we create the new menu;
	-- use the item menu passed in
	*/
	XtVaGetValues(widget,
		XmNfontList, &fontlist,
		NULL) ;

	if (!stn_antennas && get_stn_antenna_records() == FALSE)
		aps_db_error_EXIT() ;	/* EXITs */

	for (stn_antenna_rec = (STN_ANTENNAS *)FIRST( stn_antennas, stn_antenna_ptr)
		; stn_antenna_rec != NULL
		; stn_antenna_rec = (STN_ANTENNAS *)NEXT(stn_antennas, stn_antenna_ptr))
	{
		/* create the antenna menus */

		for (antennaStr = FIRST( stn_antenna_rec->antennas, antenna_ptr )
			; antennaStr != NULL
			; antennaStr = NEXT( stn_antenna_rec->antennas, antenna_ptr ))
		{
			/* if this is no-antenna and no-antenna flag is False, skip it */
			if (strcmp( antennaStr, no_antennaStr ) == 0
				&& noAntenna_flag == False)
			{
				continue;
			}

			/*
			-- if the antenna has not already been
			-- added to the menu, add it
			*/
			if (!XtNameToWidget(antennamenu, antennaStr))
			{
				/* add the antenna to the antenna option menu */
				menu_str = XmStringCreateLocalized(antennaStr) ;
   				temp = XtVaCreateWidget(antennaStr,
					xmPushButtonWidgetClass, antennamenu,
					XmNlabelString, menu_str,
					XmNfontList, fontlist,
					NULL) ;
				XmStringFree(menu_str) ;
			}
		}
	}

	/*
	-- set default antenna menu choices to the 1st station's antennas
	*/

	stn_antenna_rec = (STN_ANTENNAS *)FIRST( stn_antennas, stn_antenna_ptr) ;
	if (stn_antenna_rec == NULL)
	{
		(void) sprintf( display_string,
			"no STATION IDs in antenna relation" );
		sat_popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
	}
	else
	{
		for (antennaStr = FIRST( stn_antenna_rec->antennas, antenna_ptr )
			; antennaStr
			; antennaStr = NEXT( stn_antenna_rec->antennas, antenna_ptr ))
		{

			/* if this is no-antenna and no-antenna flag is False, skip it */
			if (strcmp( antennaStr, no_antennaStr ) == 0
				&& noAntenna_flag == False)
			{
				continue;
			}

			temp = XtNameToWidget( antennamenu, antennaStr ) ;
			if (temp)
			{
				XtManageChild(temp) ;
				if (defaultSet == FALSE)
				{
					XtVaSetValues( antennamenu,
						XmNmenuHistory, temp,
						NULL) ;
					defaultSet = TRUE ;
				}
			}

			else
			{
				(void) sprintf( display_string,
					"NO ANTENNA widget for antenna %s", antennaStr );
				sat_popup_message( XmDIALOG_ERROR, "APS:ERROR",
					display_string, XtGrabNone ) ;
			}
		}
	}

	/* now remove the default choice, since it's only a place holder */
	XtUnmanageChild(widget) ;

	return;
}



/*==============================================================================
Function:		set_satellite_menu

Description:	sets the satellite option menu.  If no error in this
				and the 3 sensor arguments are non-NULL, sets the sensor
				option menu.

Parameters:

Returns:		SAT_SENSOR_OK if no errors
				SAT_MENU_ERROR if couldn't set satellite option menu
				SENSOR_MENU_ERROR if couldn't set sensor option menu

Creator:		Ron Green

Creation Date:	10/27/1994

Notes:			If an error occurs, err message is in global display_string.
==============================================================================*/
int
set_satellite_menu( Widget sat_option_menu, Widget submenu_sat, char *code,
	Widget sensor_option_menu, Widget submenu_sensor, char *sensor,
	int check_cvrg_allowed )
{
	SATELLITE	*sat_rec ;
	cursor		sat_ptr ;
	Widget		sat_button ;

	Widget		sensor_button ;
	DB_RECORD	**sensor_rec ;
	cursor		sensor_ptr ;
	char		*sensorname ;


	if (!sat_option_menu || !submenu_sat)
	{
		(void) sprintf( display_string,
			"INTERNAL ERROR: invalid NULL satellite arg for set_satellite_menu" ) ;
		return (SATELLITE_ERROR) ;
	}

	for (sat_rec = FIRST(satellites, sat_ptr) ; sat_rec
		; sat_rec = NEXT(satellites, sat_ptr))
	{
		if (strcmp(sat_rec->sat, code) == 0)
			break ;
	}

	if (!sat_rec)
	{
		(void) sprintf( display_string, 
			"APS DB ERROR: menu for satellite (%s) not found\n", code ) ;
		return (SATELLITE_ERROR) ;
	}
	else
	{
		sat_button = XtNameToWidget(submenu_sat, sat_rec->satname) ;

		XtVaSetValues(sat_option_menu,
			XmNmenuHistory, sat_button,
			NULL) ;
	}

	/* if sensor menu needs updating, (i.e. not NULL) */
	if (sensor_option_menu && submenu_sensor && sensor)
	{
		/* see if this sensor is ok for this satellite */
		for (sensor_rec = FIRST( sat_rec->sensors, sensor_ptr )
			; sensor_rec != NULL
			; sensor_rec = NEXT( sat_rec->sensors, sensor_ptr ))
		{
			sensorname = (char *)
				CAST_SATSENSOR_SENSOR sensor_rec[SATSENSOR_SENSOR] ;

			if (check_cvrg_allowed == TRUE)
			{
				switch (satellite_coverage_not_allowed(
					sat_rec->sat, sensorname ))
				{
					case ERROR:
						aps_db_error_EXIT() ;	/* EXITs */
						break;	/* never get here; needed for lint */
					case TRUE:
						/* cvrg is disallowed for this satellite, skip it */
						continue;
					default:
						break;
				}
			}

			if (strcmp( sensorname, sensor ) == 0)
				break ;	/* break out of for sensor... */
		}

		if (!sensor_rec)	/* sensor not ok for the given satellite */
		{
			(void) sprintf( display_string, "sensor %s not allowed on sat %s\n",
				sensor, sat_rec->sat ) ;
			return (SENSOR_ERROR) ;
		}

		/* set the menu to the requested sensor */
		sensor_button = XtNameToWidget(submenu_sensor, sensor) ;
		if (sensor_button)
		{
			XtVaSetValues(sensor_option_menu,
				XmNmenuHistory, sensor_button,
				NULL) ;
		}
		else
		{
			(void) sprintf( display_string, "ERROR w sensor button..." ) ;
			return (SENSOR_ERROR) ;
		}
	}
	return (SATMENU_OK) ;
}



/*==============================================================================
Function:		set_sensor_menus

Description:

Parameters:
				check_cvrg_allowed	- if TRUE, menu will contain only
										sat/sensors with coverage allowed
									- if FALSE will include all sat/sensors

Returns:		None

Creator:		T. McKillop (modified R Green's cb_set_sensor_menus)

Creation Date:	11/08/95

Notes:
==============================================================================*/
static void
set_sensor_menus( Widget widget, XtPointer client_data,
	XmRowColumnCallbackStruct *cbs, int check_cvrg_allowed )
{
	Widget sensor_button ;
	Widget previous_sat ;

	SATELLITE *prev_sat_rec ;
	SATELLITE *curr_sat_rec ;
	cursor ptr ;
	cursor sat_ptr ;

	DB_RECORD **sensor_rec ;
	cursor sensor_ptr ;
	OPTION_MENU_WIDGETS *sensormenu ;

	char *prev_satname ;
	char *curr_satname ;

	char *sensorname ;

	sensormenu = (OPTION_MENU_WIDGETS *) client_data ;
	if (!sensormenu)
	{
		(void) sprintf( display_string,
			"set_sensor_menus: \"sensormenu\" arg is NULL" ) ;
		sat_aps_internal_error( XmDIALOG_ERROR,
			__FILE__, __LINE__, display_string ) ;
		return ;
	}

	/*
	-- get the previously activated (satellite) button from
	-- the XmNmenuHistory resource ... it will be used
	-- to turn off the unneeded sensors
	*/
	XtVaGetValues(widget,
		XmNmenuHistory, &previous_sat,
		NULL) ;

	/*
	-- now get the names of the previous and current
	-- satellites for the purpose of determining
	-- what sensors to turn off and on
	-- the sensor list is in the matching satellite record
	*/
	prev_satname = XtName(previous_sat) ;
	curr_satname = XtName(cbs->widget) ;

	for (prev_sat_rec = FIRST(satellites, sat_ptr) ; prev_sat_rec
		; prev_sat_rec = NEXT(satellites, sat_ptr))
	{
		if (strcmp(prev_sat_rec->satname, prev_satname) == 0)
			break ;
	}

	for (curr_sat_rec = FIRST( satellites, sat_ptr )
		; curr_sat_rec
		; curr_sat_rec = NEXT(satellites, sat_ptr))
	{
		if (strcmp(curr_sat_rec->satname, curr_satname) == 0)
			break ;
	}
	if (!curr_sat_rec)
	{
		(void) sprintf( display_string,
			"ERROR with satellite button (%s)\n", curr_satname ) ;
		sat_popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
	}
	else
	{
		/*
		-- turn on all needed sensors
		-- set the option menu to one of the needed sensors
		-- this way one that is needed is displayed
		*/

		for (sensor_rec = FIRST(curr_sat_rec->sensors, sensor_ptr)
			; sensor_rec != NULL
			; sensor_rec = NEXT(curr_sat_rec->sensors, sensor_ptr))
		{
			sensorname = (char *)
				CAST_SATSENSOR_SENSOR sensor_rec[SATSENSOR_SENSOR] ;

			if (check_cvrg_allowed == TRUE)
			{
				switch (satellite_coverage_not_allowed(
					curr_sat_rec->sat, sensorname ))
				{
					case ERROR:
						aps_db_error_EXIT() ;	/* EXITs */
						break;	/* never get here; needed for lint */
					case TRUE:
						/* cvrg is disallowed for this satellite, skip it */
						continue;
					default:
						break;
				}
			}

			/* turn on all needed buttons */
			if ((sensor_button =
				XtNameToWidget( sensormenu->submenu, sensorname )))
			{
				XtManageChild(sensor_button) ;
				XtVaSetValues(sensormenu->optionmenu,
					XmNmenuHistory, sensor_button,
					NULL) ;
			}
			else
			{
				(void) sprintf( display_string,
					"no option menu exists\n  for sensor %s\n  on sat %s",
					sensorname, curr_satname ) ;
				sat_aps_internal_error( XmDIALOG_ERROR,
					__FILE__, __LINE__, display_string ) ;
				/* delete the sensor, it won't ever have an option menu */
				DEL_AT_CURSOR( curr_sat_rec->sensors, sensor_ptr ) ;
			}
		}

		/*
		-- now turn off any sensors not part of the current satellite
		-- NOTE: prev_sat_rec should NEVER be NULL, can't do anything if it is
		*/
		if (prev_sat_rec)
		{
			for (sensor_rec = FIRST(prev_sat_rec->sensors, sensor_ptr)
				; sensor_rec != NULL
				; sensor_rec = NEXT(prev_sat_rec->sensors, sensor_ptr))
			{
				sensorname = (char *)
					CAST_SATSENSOR_SENSOR sensor_rec[SATSENSOR_SENSOR] ;

				if (check_cvrg_allowed == TRUE)
				{
					switch (satellite_coverage_not_allowed(
						prev_sat_rec->sat, sensorname ))
					{
						case ERROR:
							aps_db_error_EXIT() ;	/* EXITs */
							break;	/* never get here; needed for lint */
						case TRUE:
							/* sensor was disallowed for previous sat, skip it*/
							continue;
						default:
							break;
					}
				}

				if ((sensor_button =
					XtNameToWidget(sensormenu->submenu, sensorname)))
				{
					int		found = FALSE;

					/* remove it if it is not needed by this sat */
					for (sensor_rec = FIRST( curr_sat_rec->sensors, ptr )
						; sensor_rec
						; sensor_rec = NEXT( curr_sat_rec->sensors, ptr ))
					{
						if (strcmp( (char *) CAST_SATSENSOR_SENSOR
							sensor_rec[SATSENSOR_SENSOR], sensorname ) == 0)
						{
							found = TRUE ;
							if (check_cvrg_allowed == TRUE)
							{
								switch (satellite_coverage_not_allowed(
									curr_sat_rec->sat, sensorname ))
								{
									case ERROR:
										aps_db_error_EXIT() ;	/* EXITs */
										break;	/* never get here; needed for lint */
									case TRUE:
										/* cvrg is disallowed remove it */
										sensor_rec = NULL ;
										break;
									default:
										/* do not remove the sensor button */
										break;	/* break out of "switch" */
								}
							}
						}
						if (found == TRUE)
							break ;	/* break out of "for" loop */
					}

					if (!sensor_rec)  /* sensor not on the current satellite */
						XtUnmanageChild(sensor_button) ;
				}
				else
				{
					(void) sprintf( display_string,
						"no option menu exists\n  for sensor %s\n  on sat %s",
						sensorname, prev_sat_rec->sat ) ;
					sat_aps_internal_error( XmDIALOG_ERROR,
						__FILE__, __LINE__, display_string ) ;
					/* delete the sensor, it won't ever have an option menu */
					DEL_AT_CURSOR( prev_sat_rec->sensors, sensor_ptr ) ;
				}
			}
		}
		else
		{
			(void) sprintf( display_string,
				"original sat (%s) is not in list, sensors may be wrong\n",
				prev_satname ) ;
			sat_aps_internal_error( XmDIALOG_ERROR,
				__FILE__, __LINE__, display_string ) ;
		}
	}
}



/*==============================================================================
Function:		cb_set_sensor_menus

Description:

Parameters:

Returns:

Creator:		Teresa McKillop

Creation Date:	11/08/95

Notes:
==============================================================================*/
void
cb_set_sensor_menus(Widget widget, XtPointer client_data,
	XmRowColumnCallbackStruct *cbs)
{
	set_sensor_menus( widget, client_data, cbs, FALSE );

	return;
}

/*==============================================================================
Function:		cb_set_cvrg_allowed_sensor_menus

Description:

Parameters:

Returns:		None

Creator:		Teresa McKillop

Creation Date:	11/08/95

Notes:
==============================================================================*/
void
cb_set_cvrg_allowed_sensor_menus(Widget widget, XtPointer client_data,
	XmRowColumnCallbackStruct *cbs)
{
	set_sensor_menus( widget, client_data, cbs, TRUE );

	return;
}


/*==============================================================================
Function:		get_default_sensor

Description:	Gets the name of sat's first sensor (or first sensor
				with coverage allowed).

Parameters:		sat					- the satellite code
				check_cvrg_allowed	- TRUE if sensor must have coverage allowed
									- FALSE if can ignore coverage allowed

Returns:		the sensor name or NULL, if any errors occurred.

Creator:		Teresa McKillop

Creation Date:	03/29/96

Notes:
==============================================================================*/
char *
get_default_sensor( char *sat, int check_cvrg_allowed )
{
	SATELLITE	*curr_sat_rec ;
	cursor		sat_ptr ;

	DB_RECORD	**sensor_rec ;
	cursor		sensor_ptr ;
	char		*sensorname = NULL ;

	for (curr_sat_rec = FIRST( satellites, sat_ptr )
		; curr_sat_rec
		; curr_sat_rec = NEXT(satellites, sat_ptr))
	{
		if (strcmp( curr_sat_rec->sat, sat ) == 0)
			break ;
	}

	if (!curr_sat_rec)
	{
		(void) sprintf( display_string, "invalid sat arg: %s", sat ) ;
		sat_popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
	}
	else
	{
		/* get the first qualified sensor */
		for (sensor_rec = FIRST( curr_sat_rec->sensors, sensor_ptr )
			; sensor_rec != NULL
			; sensor_rec = NEXT( curr_sat_rec->sensors, sensor_ptr ))
		{
			sensorname = CAST_SATSENSOR_SENSOR sensor_rec[SATSENSOR_SENSOR] ;

			if (check_cvrg_allowed == TRUE)
			{
				switch (satellite_coverage_not_allowed( sat, sensorname ))
				{
					case ERROR:
						aps_db_error_EXIT() ;	/* EXITs */
						break;	/* never get here; needed for lint */
					case TRUE:
						/* cvrg is disallowed for this sensor, skip it */
						continue;
					default:
						/* this sensor is good */
						break;
				}
			}
			break ;	/* break out of for loop, have the first sensor */
		}

		if (!sensor_rec)
			sensorname = NULL ;
	}

	return (sensorname) ;
}



/*==============================================================================
Function:		cb_set_station_menus

Description:

Parameters:

Returns:		None

Creator:		Ron Green

Creation Date:	10/28/1994

Notes:
==============================================================================*/
void
cb_set_station_menus(Widget widget, XtPointer client_data,
	XmRowColumnCallbackStruct *cbs)
{
	Widget station_button ;
	Widget previous_sat ;

	SATELLITE *prev_sat_rec ;
	SATELLITE *curr_sat_rec ;
	cursor ptr ;
	cursor sat_ptr ;

	DB_RECORD **station_rec ;
	cursor station_ptr ;
	OPTION_MENU_WIDGETS *stationmenu ;

	char *prev_satname ;
	char *curr_satname ;

	char *stationname ;

	stationmenu = (OPTION_MENU_WIDGETS *) client_data ;
	if (!stationmenu)
	{
		(void) sprintf( display_string,
			"cb_set_station_menus: \"stationmenu\" arg is NULL" ) ;
		sat_aps_internal_error( XmDIALOG_ERROR,
			__FILE__, __LINE__, display_string ) ;
		return ;
	}

	/*
	-- get the previously activated (satellite) button from
	-- the XmNmenuHistory resource ... it will be used
	-- to turn off the unneeded stations
	*/
	XtVaGetValues(widget,
		XmNmenuHistory, &previous_sat,
		NULL) ;

	/*
	-- now get the names of the previous and current
	-- satellites for the purpose of determining
	-- what stations to turn off and on
	-- the station list is in the matching satellite record
	*/
	prev_satname = XtName(previous_sat) ;
	curr_satname = XtName(cbs->widget) ;

	for (prev_sat_rec = FIRST(satellites, sat_ptr) ; prev_sat_rec
		; prev_sat_rec = NEXT(satellites, sat_ptr))
	{
		if (strcmp(prev_sat_rec->satname, prev_satname) == 0)
			break ;
	}

	for (curr_sat_rec = FIRST(satellites, sat_ptr) ; curr_sat_rec
		; curr_sat_rec = NEXT(satellites, sat_ptr))
	{
		if (strcmp(curr_sat_rec->satname, curr_satname) == 0)
			break ;
	}

	if (!curr_sat_rec)
	{
		(void) sprintf( display_string,
			"ERROR with satellite button (%s)\n", curr_satname ) ;
		sat_popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
	}
	else
	{
		/*
		-- turn on all needed stations
		-- set the option menu to one of the needed stations
		-- this way one that is needed is displayed
		*/

		for (station_rec = FIRST(curr_sat_rec->stations, station_ptr)
			; station_rec
			; station_rec = NEXT(curr_sat_rec->stations, station_ptr))
		{
			stationname = (char *)
				CAST_STATION_STATIONID station_rec[STATION_STATIONID] ;

			/* turn on all needed buttons */
			if ((station_button =
				XtNameToWidget(stationmenu->submenu, stationname)))
			{
				XtManageChild(station_button) ;
				XtVaSetValues(stationmenu->optionmenu,
					XmNmenuHistory, station_button,
					NULL) ;
			}
			else
			{
				(void) sprintf( display_string,
					"no option menu exists\n  for station %s\n  on sat %s",
					stationname, curr_satname ) ;
				sat_aps_internal_error( XmDIALOG_ERROR,
					__FILE__, __LINE__, display_string ) ;
				/* delete the station, it won't ever have an option menu */
				DEL_AT_CURSOR( curr_sat_rec->stations, station_ptr ) ;
			}
		}

		/*
		-- now turn off any stations not part of the current satellite
		-- NOTE: prev_sat_rec should NEVER be NULL, can't do anything if it is
		*/
		if (prev_sat_rec)
		{
			for (station_rec = FIRST(prev_sat_rec->stations, station_ptr)
				; station_rec
				; station_rec = NEXT(prev_sat_rec->stations, station_ptr))
			{
				stationname = (char *)
					CAST_STATION_STATIONID station_rec[STATION_STATIONID] ;

				if ((station_button =
					XtNameToWidget(stationmenu->submenu, stationname)))
				{
					/* remove it if it is not needed by this sat */
					for (station_rec = FIRST(curr_sat_rec->stations, ptr)
						; station_rec
						; station_rec = NEXT(curr_sat_rec->stations, ptr))
					{
						if (strcmp( CAST_STATION_STATIONID
							station_rec[STATION_STATIONID], stationname ) == 0)
							break ;
					}

					if (!station_rec)  /* station not on the current sat */
						XtUnmanageChild(station_button) ;
				}
				else
				{
					(void) sprintf( display_string,
						"no option menu exists\n  for station %s\n  on sat %s",
						stationname, prev_satname ) ;
					sat_aps_internal_error( XmDIALOG_ERROR,
						__FILE__, __LINE__, display_string ) ;
					/* delete the station, it won't ever have an option menu */
					DEL_AT_CURSOR( prev_sat_rec->stations, station_ptr ) ;
				}
			}
		}
		else
		{
			(void) sprintf( display_string,
				"original sat (%s) is not in list, stations may be wrong\n",
				prev_satname ) ;
			sat_aps_internal_error( XmDIALOG_ERROR,
				__FILE__, __LINE__, display_string ) ;
		}
	}
}



/*==============================================================================
Function:		set_stationid_menu

Description:	Display the corresponding station in a station option menu

Parameters:		id of the station to display

Returns:     	None

Creator:		Ron Green

Creation Date:	04/11/1995

Notes:
==============================================================================*/
void
set_stationid_menu( Widget stationid_option_menu, Widget stationid_submenu,
	char *id)
{
	Widget stationid_button ;
	STN_ANTENNAS	*stn_antenna_rec ;
	cursor			stn_antenna_ptr ;

	/* validate the input arguments */
	if (!stationid_submenu)
	{
		(void) sprintf( display_string,
			"set_stationid_menu: \"stationid_submenu\" arg is NULL" ) ;
		sat_aps_internal_error( XmDIALOG_ERROR,
			__FILE__, __LINE__, display_string ) ;
		return ;
	}
	if (!id)
	{
		(void) sprintf( display_string,
			"set_stationid_menu: \"station id\" arg is NULL\n" ) ;
		sat_aps_internal_error( XmDIALOG_ERROR,
			__FILE__, __LINE__, display_string ) ;
		return ;
	}

	stationid_button = XtNameToWidget(stationid_submenu, id) ;
	if (!stationid_button)
	{
		/* set station menu to the 1st station */
		stn_antenna_rec = (STN_ANTENNAS *)FIRST( stn_antennas, stn_antenna_ptr);
		if (stn_antenna_rec == NULL)
		{
			(void) sprintf( display_string,
				"Can't display STATION ID: no STATION IDs in antenna relation\n"
				);
			sat_popup_message( XmDIALOG_ERROR, "APS:ERROR",
				display_string, XtGrabNone ) ;
			return ;
		}

		(void) sprintf( display_string,
			"STATION ID: %s is invalid\nUsing default of %s\n",
			id, stn_antenna_rec->station ) ;
		sat_popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
		stationid_button = XtNameToWidget(stationid_submenu,
			stn_antenna_rec->station ) ;
	}

	XtVaSetValues(stationid_option_menu,
		XmNmenuHistory, stationid_button,
		NULL) ;
}



/*==============================================================================
Function:		cb_set_antenna_menus

Description:

Parameters:

Returns:		None

Creator:		Teresa McKillop

Creation Date:	12/30/95

Notes:
==============================================================================*/
void
cb_set_antenna_menus( Widget widget, XtPointer client_data,
	XmRowColumnCallbackStruct *cbs )
{
	Widget				antenna_button ;
	Widget				previous_stn ;

	STN_ANTENNAS		*prev_stn_rec ;
	STN_ANTENNAS		*curr_stn_rec ;
	cursor				stn_ptr ;
	cursor				tmpPtr ;

	char				*antennaStr ;
	cursor				antenna_ptr ;
	char				*tmpAntennaStr;

	ANTENNA_CLIENT_DATA	*antennamenu ;

	char				*prev_stnname ;
	char				*curr_stnname ;
	Boolean				noAntenna_flag;
	Widget				ant_submenu;
	Widget				ant_optionmenu;

	antennamenu    = (ANTENNA_CLIENT_DATA *) client_data ;
	if (!antennamenu)
	{
		(void) sprintf( display_string,
			"cb_set_antenna_menus: \"antennamenu\" arg is NULL" ) ;
		sat_aps_internal_error( XmDIALOG_ERROR,
			__FILE__, __LINE__, display_string ) ;
		return ;
	}

	noAntenna_flag = antennamenu->noAntenna_flag;
	ant_submenu     = antennamenu->menuWidgets.submenu;
	ant_optionmenu  = antennamenu->menuWidgets.optionmenu;

	/*
	-- get the previously activated (station) button from
	-- the XmNmenuHistory resource ... it will be used
	-- to turn off the unneeded antennas
	*/
	XtVaGetValues(widget,
		XmNmenuHistory, &previous_stn,
		NULL) ;

	/*
	-- now get the names of the previous and current
	-- stations for the purpose of determining
	-- what antennas to turn off and on;
	-- the antenna list is in the matching stn_antenna record
	*/
	prev_stnname = XtName(previous_stn) ;
	curr_stnname = XtName(cbs->widget) ;

	for (prev_stn_rec = FIRST( stn_antennas, stn_ptr ) ; prev_stn_rec
		; prev_stn_rec = NEXT( stn_antennas, stn_ptr ))
	{
		if (strcmp(prev_stn_rec->station, prev_stnname) == 0)
			break ;
	}

	for (curr_stn_rec = FIRST( stn_antennas, stn_ptr ) ; curr_stn_rec
		; curr_stn_rec = NEXT( stn_antennas, stn_ptr ))
	{
		if (strcmp(curr_stn_rec->station, curr_stnname) == 0)
			break ;
	}
	if (!curr_stn_rec)
	{
		(void) sprintf( display_string,
			"ERROR with station button (%s)\n", curr_stnname ) ;
		sat_popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
	}
	else
	{
		/*
		-- turn on all needed antennas
		-- set the option menu to one of the needed antennas
		-- this way one that is needed is displayed
		*/

		for (antennaStr = FIRST( curr_stn_rec->antennas, antenna_ptr )
			; antennaStr != NULL
			; antennaStr = NEXT( curr_stn_rec->antennas, antenna_ptr ))
		{
			/* if noAntenna_flag is False, skip no-antenna */
			if (strcmp( antennaStr, no_antennaStr ) == 0
				&& noAntenna_flag == False)
			{
				continue;
			}

			/* turn on all needed buttons */
			if ((antenna_button = XtNameToWidget(ant_submenu, antennaStr)))
			{
				XtManageChild(antenna_button) ;
				XtVaSetValues(ant_optionmenu,
					XmNmenuHistory, antenna_button,
					NULL) ;
			}
			else
			{
				(void) sprintf( display_string,
					"no option menu exists\n  for antenna %s\n  at station %s",
					antennaStr, curr_stnname ) ;
				sat_aps_internal_error( XmDIALOG_ERROR,
					__FILE__, __LINE__, display_string ) ;
				/* delete the antenna, it won't ever have an option menu */
				DEL_AT_CURSOR( curr_stn_rec->antennas, antenna_ptr ) ;
			}
		}

		/*
		-- now turn off any antennas not part of the current station
		-- NOTE: prev_stn_rec should NEVER be NULL, can't do anything if it is
		*/
		if (prev_stn_rec)
		{
			for (antennaStr = FIRST( prev_stn_rec->antennas, antenna_ptr )
				; antennaStr != NULL
				; antennaStr = NEXT( prev_stn_rec->antennas, antenna_ptr ))
			{
				/* if this is no-antenna and no-antenna flag is False, skip it*/
				if (strcmp( antennaStr, no_antennaStr ) == 0
					&& noAntenna_flag == False)
				{
					continue;
				}

				if ((antenna_button = XtNameToWidget(ant_submenu, antennaStr)))
				{
					/* remove it if it is not needed by this sat */
					for (tmpAntennaStr = FIRST( curr_stn_rec->antennas, tmpPtr )
						; tmpAntennaStr
						; tmpAntennaStr = NEXT( curr_stn_rec->antennas, tmpPtr))
					{
						if (strcmp( tmpAntennaStr, antennaStr ) == 0)
							break ;
					}

					if (!tmpAntennaStr)  /* antenna not on the current stn */
						XtUnmanageChild(antenna_button) ;
				}
				else
				{
					(void) sprintf( display_string,
						"no option menu exists\n  for antenna %s\n  at station %s",
						antennaStr, prev_stnname ) ;
					sat_aps_internal_error( XmDIALOG_ERROR,
						__FILE__, __LINE__, display_string ) ;
					/* delete the antenna, it won't ever have an option menu */
					DEL_AT_CURSOR( prev_stn_rec->antennas, antenna_ptr ) ;
				}
			}
		}
		else
		{
			(void) sprintf( display_string,
				"original station (%s) is not in list, antennas may be wrong\n",
				(char *) previous_stn ) ;
			sat_aps_internal_error( XmDIALOG_ERROR,
				__FILE__, __LINE__, display_string ) ;
		}
	}

	return;
}



/*==============================================================================
Function:		set_antenna_menu

Description:	Display the corresponding antenna in an antenna option menu

Parameters:		id of the antenna to display

Returns:     	None

Creator:		Teresa McKillop

Creation Date:	12/20/95

Notes:
==============================================================================*/
void
set_antenna_menu( Widget antenna_option_menu, Widget antenna_submenu, char *id )
{
	Widget	antenna_button ;

	STN_ANTENNAS	*stn_antenna_rec ;
	cursor			stn_antenna_ptr ;

	char			*antennaStr ;
	cursor			antenna_ptr ;

	/* validate the input arguments */
	if (!antenna_option_menu)
	{
		(void) sprintf( display_string,
			"set_antenna_menu: \"antenna_option_menu\" arg is NULL" ) ;
		sat_aps_internal_error( XmDIALOG_ERROR,
			__FILE__, __LINE__, display_string ) ;
		return ;
	}
	if (!antenna_submenu)
	{
		(void) sprintf( display_string,
			"set_antenna_menu: \"antenna_submenu\" arg is NULL" ) ;
		sat_aps_internal_error( XmDIALOG_ERROR,
			__FILE__, __LINE__, display_string ) ;
		return ;
	}
	if (!id)
	{
		(void) sprintf( display_string,
			"set_antenna_menu: \"station id\" arg is NULL" ) ;
		sat_aps_internal_error( XmDIALOG_ERROR,
			__FILE__, __LINE__, display_string ) ;
		return ;
	}

	antenna_button = XtNameToWidget( antenna_submenu, id ) ;
	if (!antenna_button)
	{
		/* set antenna menu to the 1st station's 1st "real" antenna */
		stn_antenna_rec = (STN_ANTENNAS *)FIRST( stn_antennas, stn_antenna_ptr);
		if (stn_antenna_rec == NULL)
		{
			(void) sprintf( display_string,
				"Can't display ANTENNA: missing ANTENNAs in antenna relation\n"
				);
			sat_popup_message( XmDIALOG_ERROR, "APS:ERROR",
				display_string, XtGrabNone ) ;
			return ;
		}

		for (antennaStr = FIRST( stn_antenna_rec->antennas, antenna_ptr )
			; !antenna_button && antennaStr
			; antennaStr = NEXT( stn_antenna_rec->antennas, antenna_ptr ))
		{
			/* if this is no-antenna, skip it */
			if (strcmp( antennaStr, no_antennaStr ) == 0)
				continue;

			antenna_button = XtNameToWidget( antenna_submenu, antennaStr ) ;
		}
		if (!antenna_button)
		{
			(void) sprintf( display_string,
				"Can't display ANTENNA: missing ANTENNAs in antenna relation\n"
				);
			sat_popup_message( XmDIALOG_ERROR, "APS:ERROR",
				display_string, XtGrabNone ) ;
			return ;
		}

		(void) sprintf( display_string,
			"ANTENNA %s is invalid\nUsing default of %s\n",
			id, antennaStr ) ;
		sat_popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
	}

	XtVaSetValues( antenna_option_menu,
		XmNmenuHistory, antenna_button,
		NULL ) ;

	return;
}


static void
aps_db_error_EXIT()
{
	(void) sprintf( display_string,
		"Error with the APS DB\nNeed to fix the DB\n" ) ;
	sat_popup_message( XmDIALOG_ERROR, "APS:ERROR", display_string, XtGrabNone ) ;
	exit (APS_EXIT_ERROR) ;
}
