#ifndef    DB_ACTIVITIES_H
#define    DB_ACTIVITIES_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_activities.h
Description:  Header file for APS db table:  activities.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:51:16 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_activities.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_activities.h"
 
#define ACTIVITIES_OBJ                      0 
#define ACTIVITIES_ACTY                     1 
#define ACTIVITIES_TRANSID                  2 
#define ACTIVITIES_N_ACTY                   3 
#define NUM_ACTIVITIES_COLS                 4 

#define CAST_ACTIVITIES_OBJ          (char *)
#define CAST_ACTIVITIES_ACTY         (char *)
#define CAST_ACTIVITIES_TRANSID      (char *)
#define CAST_ACTIVITIES_N_ACTY       *(DBSMALLINT *)
 
#endif   /*    DB_ACTIVITIES_H    */
