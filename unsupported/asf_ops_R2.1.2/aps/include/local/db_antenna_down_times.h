#ifndef    DB_ANTENNA_DOWN_TIMES_H
#define    DB_ANTENNA_DOWN_TIMES_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_antenna_down_times.h
Description:  Header file for APS db table:  antenna_down_times.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:51:26 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_antenna_down_times.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_antenna_down_times.h"
 
#define ANTENNA_DOWN_TIMES_STATION_ID               0 
#define ANTENNA_DOWN_TIMES_ANTENNA_ID               1 
#define ANTENNA_DOWN_TIMES_STRTTIME                 2 
#define ANTENNA_DOWN_TIMES_STOPTIME                 3 
#define ANTENNA_DOWN_TIMES_COMMENTS                 4 
#define NUM_ANTENNA_DOWN_TIMES_COLS                 5 

#define CAST_ANTENNA_DOWN_TIMES_STATION_ID   (char *)
#define CAST_ANTENNA_DOWN_TIMES_ANTENNA_ID   *(DBSMALLINT *)
#define CAST_ANTENNA_DOWN_TIMES_STRTTIME     (char *)
#define CAST_ANTENNA_DOWN_TIMES_STOPTIME     (char *)
#define CAST_ANTENNA_DOWN_TIMES_COMMENTS     (char *)
 
#endif   /*    DB_ANTENNA_DOWN_TIMES_H    */
