#ifndef    DB_ANTENNA_PREF_H
#define    DB_ANTENNA_PREF_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_antenna_pref.h
Description:  Header file for APS db table:  antenna_pref.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:51:32 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_antenna_pref.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_antenna_pref.h"
 
#define ANTENNA_PREF_SAT                      0 
#define ANTENNA_PREF_STATION_ID               1 
#define ANTENNA_PREF_ANTENNA_ID               2 
#define ANTENNA_PREF_PREFERENCE               3 
#define NUM_ANTENNA_PREF_COLS                 4 

#define CAST_ANTENNA_PREF_SAT          (char *)
#define CAST_ANTENNA_PREF_STATION_ID   (char *)
#define CAST_ANTENNA_PREF_ANTENNA_ID   *(DBSMALLINT *)
#define CAST_ANTENNA_PREF_PREFERENCE   *(DBSMALLINT *)
 
#endif   /*    DB_ANTENNA_PREF_H    */
