#ifndef    DB_EPHEMERIS_H
#define    DB_EPHEMERIS_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_ephemeris.h
Description:  Header file for APS db table:  ephemeris.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:52:18 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_ephemeris.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_ephemeris.h"
 
#define EPHEMERIS_FILENAME                 0 
#define EPHEMERIS_STARTTIME                1 
#define EPHEMERIS_ENDTIME                  2 
#define EPHEMERIS_STARTREV                 3 
#define EPHEMERIS_ENDREV                   4 
#define EPHEMERIS_SAT                      5 
#define EPHEMERIS_EPOCH                    6 
#define EPHEMERIS_PHASE_NAME               7 
#define EPHEMERIS_SV_FILENAME              8 
#define EPHEMERIS_SV_TYPE                  9 
#define EPHEMERIS_SV_REV                  10 
#define EPHEMERIS_SV_TIME                 11 
#define EPHEMERIS_SV_R_X                  12 
#define EPHEMERIS_SV_R_Y                  13 
#define EPHEMERIS_SV_R_Z                  14 
#define EPHEMERIS_SV_V_X                  15 
#define EPHEMERIS_SV_V_Y                  16 
#define EPHEMERIS_SV_V_Z                  17 
#define NUM_EPHEMERIS_COLS                18 

#define CAST_EPHEMERIS_FILENAME     (char *)
#define CAST_EPHEMERIS_STARTTIME    (char *)
#define CAST_EPHEMERIS_ENDTIME      (char *)
#define CAST_EPHEMERIS_STARTREV     *(DBINT *)
#define CAST_EPHEMERIS_ENDREV       *(DBINT *)
#define CAST_EPHEMERIS_SAT          (char *)
#define CAST_EPHEMERIS_EPOCH        *(DBFLT8 *)
#define CAST_EPHEMERIS_PHASE_NAME   *(DBCHAR *)
#define CAST_EPHEMERIS_SV_FILENAME  (char *)
#define CAST_EPHEMERIS_SV_TYPE      (char *)
#define CAST_EPHEMERIS_SV_REV       (char *)
#define CAST_EPHEMERIS_SV_TIME      (char *)
#define CAST_EPHEMERIS_SV_R_X       (char *)
#define CAST_EPHEMERIS_SV_R_Y       (char *)
#define CAST_EPHEMERIS_SV_R_Z       (char *)
#define CAST_EPHEMERIS_SV_V_X       (char *)
#define CAST_EPHEMERIS_SV_V_Y       (char *)
#define CAST_EPHEMERIS_SV_V_Z       (char *)
 
#endif   /*    DB_EPHEMERIS_H    */
