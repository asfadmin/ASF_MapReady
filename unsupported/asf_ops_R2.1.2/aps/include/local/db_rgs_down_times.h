#ifndef    DB_RGS_DOWN_TIMES_H
#define    DB_RGS_DOWN_TIMES_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_rgs_down_times.h
Description:  Header file for APS db table:  rgs_down_times.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:52:45 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_rgs_down_times.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_rgs_down_times.h"
 
#define RGS_DOWN_TIMES_STATION_ID               0 
#define RGS_DOWN_TIMES_STRTTIME                 1 
#define RGS_DOWN_TIMES_STOPTIME                 2 
#define RGS_DOWN_TIMES_DISPOSITION              3 
#define RGS_DOWN_TIMES_FA_NOTIFICATION          4 
#define RGS_DOWN_TIMES_UTYPE                    5 
#define RGS_DOWN_TIMES_UREASON                  6 
#define RGS_DOWN_TIMES_REMARKS                  7 
#define RGS_DOWN_TIMES_UNAVAIL_EVENT_COUNTER    8 
#define NUM_RGS_DOWN_TIMES_COLS                 9 

#define CAST_RGS_DOWN_TIMES_STATION_ID   (char *)
#define CAST_RGS_DOWN_TIMES_STRTTIME     (char *)
#define CAST_RGS_DOWN_TIMES_STOPTIME     (char *)
#define CAST_RGS_DOWN_TIMES_DISPOSITION  *(DBCHAR *)
#define CAST_RGS_DOWN_TIMES_FA_NOTIFICATION       *(DBCHAR *)
#define CAST_RGS_DOWN_TIMES_UTYPE        *(DBCHAR *)
#define CAST_RGS_DOWN_TIMES_UREASON      *(DBCHAR *)
#define CAST_RGS_DOWN_TIMES_REMARKS      (char *)
#define CAST_RGS_DOWN_TIMES_UNAVAIL_EVENT_COUNTER *(DBINT *)
 
#endif   /*    DB_RGS_DOWN_TIMES_H    */
