#ifndef    DB_PHASE_H
#define    DB_PHASE_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_phase.h
Description:  Header file for APS db table:  phase.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /home/aps/r1bprime/etc/install/sh_scripts/Create_db_include_files.csh
Date:         Tue Dec 17 11:10:31 PST 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_phase.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_phase.h"
 
#define PHASE_SAT                      0 
#define PHASE_PHASE_NAME               1 
#define PHASE_PHASE_START              2 
#define PHASE_PHASE_LON                3 
#define PHASE_PHASE_DAYS               4 
#define PHASE_PHASE_ORBITS             5 
#define PHASE_LAST_REV                 6 
#define PHASE_CYCLE_DAYS               7 
#define PHASE_CYCLE_REVS               8 
#define PHASE_ORB_A                    9 
#define PHASE_ORB_E                   10 
#define PHASE_ORB_I                   11 
#define PHASE_ORB_ARG_PERI            12 
#define PHASE_RSP_0_LON               13 
#define PHASE_N_ROWS                  14 
#define PHASE_MIN_ROW                 15 
#define PHASE_MAX_ROW                 16 
#define PHASE_ANTARCTIC_MODE          17 
#define PHASE_EW_TOL_KM               18 
#define PHASE_NS_TOL_MIN              19 
#define NUM_PHASE_COLS                20 

#define CAST_PHASE_SAT          (char *)
#define CAST_PHASE_PHASE_NAME   *(DBCHAR *)
#define CAST_PHASE_PHASE_START  (char *)
#define CAST_PHASE_PHASE_LON    *(DBFLT8 *)
#define CAST_PHASE_PHASE_DAYS   *(DBINT *)
#define CAST_PHASE_PHASE_ORBITS          *(DBINT *)
#define CAST_PHASE_LAST_REV     *(DBINT *)
#define CAST_PHASE_CYCLE_DAYS   *(DBINT *)
#define CAST_PHASE_CYCLE_REVS   *(DBINT *)
#define CAST_PHASE_ORB_A        *(DBFLT8 *)
#define CAST_PHASE_ORB_E        *(DBFLT8 *)
#define CAST_PHASE_ORB_I        *(DBFLT8 *)
#define CAST_PHASE_ORB_ARG_PERI          *(DBFLT8 *)
#define CAST_PHASE_RSP_0_LON    *(DBFLT8 *)
#define CAST_PHASE_N_ROWS       *(DBINT *)
#define CAST_PHASE_MIN_ROW      *(DBINT *)
#define CAST_PHASE_MAX_ROW      *(DBINT *)
#define CAST_PHASE_ANTARCTIC_MODE        *(DBCHAR *)
#define CAST_PHASE_EW_TOL_KM    *(DBFLT8 *)
#define CAST_PHASE_NS_TOL_MIN   *(DBFLT8 *)
 
#endif   /*    DB_PHASE_H    */
