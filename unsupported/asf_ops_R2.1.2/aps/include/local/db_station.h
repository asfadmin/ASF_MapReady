#ifndef    DB_STATION_H
#define    DB_STATION_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_station.h
Description:  Header file for APS db table:  station.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:53:27 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_station.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_station.h"
 
#define STATION_STATIONID                0 
#define STATION_STATIONNAME              1 
#define STATION_SAT                      2 
#define STATION_STLAT                    3 
#define STATION_STLON                    4 
#define STATION_STALTM                   5 
#define STATION_STRADIUSKM               6 
#define STATION_RUNBUFFKM                7 
#define STATION_ELEVANGLE                8 
#define STATION_FA_STATION               9 
#define NUM_STATION_COLS                10 

#define CAST_STATION_STATIONID    (char *)
#define CAST_STATION_STATIONNAME  (char *)
#define CAST_STATION_SAT          (char *)
#define CAST_STATION_STLAT        *(DBFLT8 *)
#define CAST_STATION_STLON        *(DBFLT8 *)
#define CAST_STATION_STALTM       *(DBFLT8 *)
#define CAST_STATION_STRADIUSKM   *(DBFLT8 *)
#define CAST_STATION_RUNBUFFKM    *(DBFLT8 *)
#define CAST_STATION_ELEVANGLE    *(DBFLT8 *)
#define CAST_STATION_FA_STATION   (char *)
 
#endif   /*    DB_STATION_H    */
