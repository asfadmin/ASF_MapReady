#ifndef    DB_MASKINOUT_H
#define    DB_MASKINOUT_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_maskinout.h
Description:  Header file for APS db table:  maskinout.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:52:34 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_maskinout.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_maskinout.h"
 
#define MASKINOUT_STATIONID                0 
#define MASKINOUT_EJDATE                   1 
#define MASKINOUT_SAT                      2 
#define MASKINOUT_REV                      3 
#define MASKINOUT_INOUT                    4 
#define NUM_MASKINOUT_COLS                 5 

#define CAST_MASKINOUT_STATIONID    (char *)
#define CAST_MASKINOUT_EJDATE       *(DBFLT8 *)
#define CAST_MASKINOUT_SAT          (char *)
#define CAST_MASKINOUT_REV          *(DBINT *)
#define CAST_MASKINOUT_INOUT        (char *)
 
#endif   /*    DB_MASKINOUT_H    */
