#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		f77_db_station.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			
	This file was written with a 4-character tab setting.  If you don't use 
	4-character tabs, it will look funny.  Use set tabstop=4 in vi to 
	browse.  

==============================================================================*/
#pragma ident	"@(#)f77_db_station.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_F77APSdb/SCCS/s.f77_db_station.c"


#include <db_sybint.h>

#include "db_station.h"

struct f77_station_rec {
	DBFLT8	stalat;
        DBFLT8	stalon;
	DBFLT8	staltm;
	DBFLT8	stradiuskm;
        DBFLT8	runbuffkm;
        DBFLT8	elevangle;
	
        char stationid [4] ;
        char stationname [20] ;
        char sat [3] ;
} f77_station_rec_;



/* To access a station record from fortran enter the following
   includes in the fortran code:

      INCLUDE 'f77_db_extern.h'
      INCLUDE 'f77_db_station.h'

   The db_extern contains declarations for the following:

      db_get_records
      db_ftn_first_record
      db_ftn_next_record

   while the db_station contains the declarations for
   the column names in the station table. The names
   are exactly the same as in the database table
*/


void load_f77_station_rec(DB_RECORD **station_rec) 
{
        f77_station_rec_.stalat =
		CAST_STATION_STLAT	    station_rec[STATION_STLAT] ;

	f77_station_rec_.stalon =	
		CAST_STATION_STLON 	    station_rec[STATION_STLON] ;

	f77_station_rec_.staltm =	
		CAST_STATION_STALTM 	    station_rec[STATION_STALTM] ;

	f77_station_rec_.stradiuskm =	
		CAST_STATION_STRADIUSKM     station_rec[STATION_STRADIUSKM] ;

        f77_station_rec_.runbuffkm = 		
                CAST_STATION_RUNBUFFKM	    station_rec[STATION_RUNBUFFKM] ;

	f77_station_rec_.elevangle = 		
		CAST_STATION_ELEVANGLE	    station_rec[STATION_ELEVANGLE] ;

        strcpy (f77_station_rec_.stationid,
                CAST_STATION_STATIONID      station_rec[STATION_STATIONID]) ;
   
        strcpy (f77_station_rec_.stationname,
                CAST_STATION_STATIONNAME    station_rec[STATION_STATIONNAME]) ;

        strcpy (f77_station_rec_.sat,
                CAST_STATION_SAT            station_rec[STATION_STATIONNAME]) ;

}




