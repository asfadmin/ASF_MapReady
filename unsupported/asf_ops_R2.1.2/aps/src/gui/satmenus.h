#ifndef SATMENUS_H
#define SATMENUS_H

#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
/*==============================================================================
Filename:		satmenus.h
Description:	to be included in every file that calls satmenu.c functions
Creator:	unknown
Notes:		
				10/31/95	Teresa McKillop		deleted	get_satinfo_records;
												added extern "satellites"
==============================================================================*/
#pragma ident	"@(#)satmenus.h	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.satmenus.h"

#include <X11/Intrinsic.h>
#include "dapps_list.h"

/* Return Values */
#define SATMENU_OK			0
#define SATMENU_EINVAL		-1		/* invalid argument */
#define	SATELLITE_ERROR		-10		/* error for satellite */
#define	SENSOR_ERROR		-11		/* error for sensor */

/******************************************************************************
**	Type Definitions
******************************************************************************/
typedef
	struct _OPTION_MENU_WIDGETS
	{
		Widget optionmenu ;
		Widget submenu ;
	} OPTION_MENU_WIDGETS ;

typedef
	struct _ANTENNA_CLIENT_DATA
	{
		Boolean				noAntenna_flag;
		OPTION_MENU_WIDGETS menuWidgets;
	} ANTENNA_CLIENT_DATA ;

typedef
	struct _SATELLITE
	{
		char *sat ;
		char *satname ;
		llist *sensors ;
		llist *stations ;
	} SATELLITE ;

typedef
	struct _STN_ANTENNAS
	{
		char *station ;
		llist *antennas ;
	} STN_ANTENNAS ;

/******************************************************************************
**	Function Prototypes
******************************************************************************/
char * get_satellite_name( char *code ) ;

char * get_satellite_code( char *name ) ;

char * get_sat_code_from_ims_name( char *imsName ) ;

int satellite_coverage_not_allowed( char *code, char *sensor ) ;

void cb_build_satellite_option_menu( Widget widget,
	XtPointer client_data, XtPointer cbs ) ;

void cb_build_cvrg_allowed_satellite_option_menu( Widget widget,
	XtPointer client_data, XtPointer cbs ) ;

void cb_build_sensor_option_menu( Widget widget,
     XtPointer client_data, XtPointer cbs ) ;

void cb_build_cvrg_allowed_sensor_option_menu( Widget widget,
     XtPointer client_data, XtPointer cbs ) ;

char * get_default_sensor( char *sat, int check_cvrg_allowed ) ;

void cb_build_station_option_menu( Widget widget,
     XtPointer client_data, XtPointer cbs ) ;

void cb_set_sensor_menus( Widget widget,
     XtPointer client_data, XmRowColumnCallbackStruct *cbs ) ;

void cb_set_cvrg_allowed_sensor_menus( Widget widget,
     XtPointer client_data, XmRowColumnCallbackStruct *cbs ) ;

void cb_set_station_menus( Widget widget,
     XtPointer client_data, XmRowColumnCallbackStruct *cbs ) ;

int set_satellite_menu(
	Widget sat_option_menu, Widget submenu_sat, char *code,
    Widget sensor_option_menu, Widget submenu_sensor, char *sensor,
	int check_cvrg_allowed ) ;

void set_stationid_menu(
	Widget stationid_option_menu, Widget stationid_submenu, char *id ) ;

void cb_build_antenna_option_menu(
	Widget widget, XtPointer client_data, XtPointer cbs ) ;

void cb_set_antenna_menus(
	Widget widget, XtPointer client_data, XmRowColumnCallbackStruct *cbs ) ;

void set_antenna_menu(
	Widget antenna_option_menu, Widget antenna_submenu, char *id ) ;

/******************************************************************************
**	Global Variables
******************************************************************************/
extern llist *satellites ;


#endif /* SATMENUS_H */
