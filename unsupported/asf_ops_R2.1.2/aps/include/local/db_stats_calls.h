#ifndef    DB_STATS_CALLS_H
#define    DB_STATS_CALLS_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_stats_calls.h
Description:  Header file for APS db table:  stats_calls.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /home/aps/r2.1.2/etc/install/sh_scripts/Create_db_include_files.csh
Date:         Fri Jan 30 16:36:22 PST 1998
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_stats_calls.h	1.3 98/01/30 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_stats_calls.h"
 
#define STATS_CALLS_TYPE                     0 
#define STATS_CALLS_CONDITION                1 
#define STATS_CALLS_SAT                      2 
#define STATS_CALLS_REV                      3 
#define STATS_CALLS_DTKID                    4 
#define STATS_CALLS_STRTTIME                 5 
#define STATS_CALLS_STOPTIME                 6 
#define STATS_CALLS_ANTENNA_ID               7 
#define STATS_CALLS_DTKSTAT                  8 
#define STATS_CALLS_CALL_TIME                9 
#define NUM_STATS_CALLS_COLS                10 

#define CAST_STATS_CALLS_TYPE         (char *)
#define CAST_STATS_CALLS_CONDITION    (char *)
#define CAST_STATS_CALLS_SAT          (char *)
#define CAST_STATS_CALLS_REV          *(DBINT *)
#define CAST_STATS_CALLS_DTKID        *(DBTINYINT *)
#define CAST_STATS_CALLS_STRTTIME     (char *)
#define CAST_STATS_CALLS_STOPTIME     (char *)
#define CAST_STATS_CALLS_ANTENNA_ID   *(DBSMALLINT *)
#define CAST_STATS_CALLS_DTKSTAT      (char *)
#define CAST_STATS_CALLS_CALL_TIME    (char *)
 
#endif   /*    DB_STATS_CALLS_H    */
