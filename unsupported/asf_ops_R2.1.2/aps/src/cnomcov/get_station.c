#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		get_station.c

Description:	holds the function get_station()

External Functions Defined:
				get_station()
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)get_station.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/cnomcov/SCCS/s.get_station.c"

#include <sybfront.h>
#include <sybdb.h>
#include <syberror.h>
 
#include <stdlib.h>

/* FOR SYBASE INTERFACES   */
#include "db_sybint.h"      /* for APS sybase interface routines    */
#include "dapps_list.h"     /* for APS linked list macros           */
#include "aps_db_table.h"   /* for APS DB tables sybase interface   */
#include "db_station.h"     /* for the station relation.              */



/*==============================================================================
Function:       get_station()

Description:    GIVEN A SAT AND station code, gets station data.

Parameters:     
   Input Parameters:
   Name         Type    Definition
   APS_dbproc	*DBPROCESS address of structure of dbprocess info.  
   SAT          *CH*2    SATELLITE NAME:  E1, J1, OR RS
   STATIONID   	*CH*3    STATION NAME, I.E., ASF 
   Output Parameters:
   LAT		*real*8	station latitude
   LON		*real*8	station longitude
   RAD		*real*8	radius in km
   BUFFR	*real*8	buffer (km) around radius, to capture the last point
 			before mask entrance and the first point after 
 			mask exit.  Talking about coverage points.  
      if any errors are incountered, the routine will terminate the run.

Returns:        >= 0  :  the number of records found.  

Creator:        Lawrence Stevens

Creation Date:  Sun Oct  1 23:21:37 PDT 1995

Notes:		
	This routine was created using 4-character tabs.  If you don't have 
	4-character tabs, this routine will look funny.  use set tabstop=4 in vi.

==============================================================================*/
int get_station(
	DBPROCESS	*APS_dbproc,
	char		*SAT,
	char		*STATIONID, 
	double		*LAT,
	double		*LON,
	double		*RAD,
	double		*BUFFR	)
{

char    sat[3];
char	stationid[4];

DB_RECORD       **station_rec ;
llist           *station_list = NULL ;
cursor          station_list_ptr ;

RETCODE return_code;

int	nrecs, rcode, system();

*LAT 	= -180.888888;
*LON 	= -400.888888;
*RAD 	= -1.888888;
*BUFFR 	= -2.888888;

/*	this routine is called from Fortran; for safety, copy the 
	input strings for use in local storage and terminate them.	*/
strncpy(sat, SAT, 2);
sat[2] = '\0';
strncpy(stationid, STATIONID, 3);
stationid[3] = '\0';

printf("get_station:  APS_dbproc = %x, sat = %s, stationid = %s\n",
        APS_dbproc, sat, stationid);

sprintf(where_clause, "where %s = '%2.2s' and %s = '%3.3s'", 
	APS_COL(STATION, STATION_SAT), sat, 
	APS_COL(STATION, STATION_STATIONID), stationid);

printf("get_station:  where_clause = >%s<\n", where_clause);

station_list = db_get_records(APS_dbproc, APS_TABLE(STATION),
	where_clause, NULL, APS_CDEFS(STATION), ALL_COLS) ;

if (station_list == NULL)
{
	printf("DB query on the station relation failed.\n" ) ;
	printf("              sat = >%s< stationid = >%s<\n", sat, stationid);
	printf("              where_clause used:  \n%s\n", where_clause);
	printf("                terminating the run.\n");
	rcode = system("banner ERROR");
	exit (1);
}

nrecs = NUMELTS( station_list ) ;

if(nrecs <= 0)
{
	printf("get_station:  no records found in station relation for\n");
	printf("                sat = >%s< stationid = >%s<\n", sat, stationid);
	printf("                where_clause used:  \n%s\n", where_clause);

	/* free all the memory.  */
	DEL_ALL( station_list ) ;
	/* return and indicate 0 records found.  */
	return 0  ;
}
else if(nrecs > 1)
{
	printf(
		"get_station:  more than 1 record found in station relation for\n");
	printf("              sat = >%s< stationid = >%s<\n", sat, stationid);
	printf("              there must be only one record; %d recs found.\n",
		nrecs);
	printf("              where_clause used:  \n%s\n", where_clause);
	printf("                terminating the run.\n");
	rcode = system("banner ERROR");
	/* free all the memory.  */
	DEL_ALL( station_list ) ;
	exit (1);
}

/* a single record was found.  */
station_rec = (DB_RECORD **) FIRST(station_list, station_list_ptr) ;

*LAT 	= CAST_STATION_STLAT station_rec[STATION_STLAT] ;
*LON 	= CAST_STATION_STLON station_rec[STATION_STLON] ;
*RAD 	= CAST_STATION_STRADIUSKM station_rec[STATION_STRADIUSKM] ;
*BUFFR 	= CAST_STATION_RUNBUFFKM station_rec[STATION_RUNBUFFKM] ;

/* free all the memory.  */
DEL_ALL( station_list ) ;

return nrecs ;

}
