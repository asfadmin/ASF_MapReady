#ifndef    DB_CVRGMDAT_H
#define    DB_CVRGMDAT_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_cvrgmdat.h
Description:  Header file for APS db table:  cvrgmdat.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:51:53 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_cvrgmdat.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_cvrgmdat.h"
 
#define CVRGMDAT_MARKER                   0 
#define CVRGMDAT_SAT                      1 
#define CVRGMDAT_SENSOR                   2 
#define CVRGMDAT_START_TIME               3 
#define CVRGMDAT_STOP_TIME                4 
#define CVRGMDAT_GEN_TIME                 5 
#define CVRGMDAT_RUNMASK                  6 
#define CVRGMDAT_REPFLAG                  7 
#define CVRGMDAT_VERSION                  8 
#define CVRGMDAT_EPHEMERIS                9 
#define NUM_CVRGMDAT_COLS                10 

#define CAST_CVRGMDAT_MARKER       *(DBINT *)
#define CAST_CVRGMDAT_SAT          (char *)
#define CAST_CVRGMDAT_SENSOR       (char *)
#define CAST_CVRGMDAT_START_TIME   (char *)
#define CAST_CVRGMDAT_STOP_TIME    (char *)
#define CAST_CVRGMDAT_GEN_TIME     (char *)
#define CAST_CVRGMDAT_RUNMASK      (char *)
#define CAST_CVRGMDAT_REPFLAG      *(DBCHAR *)
#define CAST_CVRGMDAT_VERSION      (char *)
#define CAST_CVRGMDAT_EPHEMERIS    (char *)
 
#endif   /*    DB_CVRGMDAT_H    */
