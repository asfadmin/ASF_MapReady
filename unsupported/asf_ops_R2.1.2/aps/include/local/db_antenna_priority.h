#ifndef    DB_ANTENNA_PRIORITY_H
#define    DB_ANTENNA_PRIORITY_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_antenna_priority.h
Description:  Header file for APS db table:  antenna_priority.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:51:37 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_antenna_priority.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_antenna_priority.h"
 
#define ANTENNA_PRIORITY_STATION_ID               0 
#define ANTENNA_PRIORITY_ANTENNA_ID               1 
#define ANTENNA_PRIORITY_SAT                      2 
#define ANTENNA_PRIORITY_PRIORITY                 3 
#define NUM_ANTENNA_PRIORITY_COLS                 4 

#define CAST_ANTENNA_PRIORITY_STATION_ID   (char *)
#define CAST_ANTENNA_PRIORITY_ANTENNA_ID   *(DBSMALLINT *)
#define CAST_ANTENNA_PRIORITY_SAT          (char *)
#define CAST_ANTENNA_PRIORITY_PRIORITY     *(DBSMALLINT *)
 
#endif   /*    DB_ANTENNA_PRIORITY_H    */
