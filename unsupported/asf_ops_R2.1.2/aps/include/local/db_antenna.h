#ifndef    DB_ANTENNA_H
#define    DB_ANTENNA_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_antenna.h
Description:  Header file for APS db table:  antenna.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Wed Jul 17 16:21:24 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_antenna.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_antenna.h"
 
#define ANTENNA_STATION_ID               0 
#define ANTENNA_ANTENNA_ID               1 
#define ANTENNA_PRE_PASS_TIME_SEC        2 
#define ANTENNA_PRE_DTK_TRACK_PAD_SEC    3 
#define ANTENNA_POST_DTK_TRACK_PAD_SEC   4 
#define ANTENNA_POST_PASS_TIME_SEC       5 
#define ANTENNA_COMMENT                  6 
#define NUM_ANTENNA_COLS                 7 

#define CAST_ANTENNA_STATION_ID   (char *)
#define CAST_ANTENNA_ANTENNA_ID   *(DBSMALLINT *)
#define CAST_ANTENNA_PRE_PASS_TIME_SEC     *(DBINT *)
#define CAST_ANTENNA_PRE_DTK_TRACK_PAD_SEC *(DBINT *)
#define CAST_ANTENNA_POST_DTK_TRACK_PAD_SEC         *(DBINT *)
#define CAST_ANTENNA_POST_PASS_TIME_SEC    *(DBINT *)
#define CAST_ANTENNA_COMMENT      (char *)
 
#endif   /*    DB_ANTENNA_H    */
