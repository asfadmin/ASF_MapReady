#ifndef    DB_FRAMEGEN_CALLS_H
#define    DB_FRAMEGEN_CALLS_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_framegen_calls.h
Description:  Header file for APS db table:  framegen_calls.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:52:23 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_framegen_calls.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_framegen_calls.h"
 
#define FRAMEGEN_CALLS_SAT                      0 
#define FRAMEGEN_CALLS_SENSOR                   1 
#define FRAMEGEN_CALLS_REV                      2 
#define FRAMEGEN_CALLS_DTKID                    3 
#define FRAMEGEN_CALLS_STRTTIME                 4 
#define FRAMEGEN_CALLS_STOPTIME                 5 
#define FRAMEGEN_CALLS_DTKSTAT                  6 
#define FRAMEGEN_CALLS_DTKDATE                  7 
#define FRAMEGEN_CALLS_STATION_ID               8 
#define NUM_FRAMEGEN_CALLS_COLS                 9 

#define CAST_FRAMEGEN_CALLS_SAT          (char *)
#define CAST_FRAMEGEN_CALLS_SENSOR       (char *)
#define CAST_FRAMEGEN_CALLS_REV          *(DBINT *)
#define CAST_FRAMEGEN_CALLS_DTKID        *(DBTINYINT *)
#define CAST_FRAMEGEN_CALLS_STRTTIME     (char *)
#define CAST_FRAMEGEN_CALLS_STOPTIME     (char *)
#define CAST_FRAMEGEN_CALLS_DTKSTAT      (char *)
#define CAST_FRAMEGEN_CALLS_DTKDATE      (char *)
#define CAST_FRAMEGEN_CALLS_STATION_ID   (char *)
 
#endif   /*    DB_FRAMEGEN_CALLS_H    */
