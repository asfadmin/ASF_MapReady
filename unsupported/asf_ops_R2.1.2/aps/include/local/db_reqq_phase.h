#ifndef    DB_REQQ_PHASE_H
#define    DB_REQQ_PHASE_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_reqq_phase.h
Description:  Header file for APS db table:  reqq_phase.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /home/aps/r2.1.1/etc/install/sh_scripts/Create_db_include_files.csh
Date:         Mon Sep 8 17:41:13 PDT 1997
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_reqq_phase.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_reqq_phase.h"
 
#define REQQ_PHASE_REQQ_ID                  0 
#define REQQ_PHASE_DUE_DATE                 1 
#define REQQ_PHASE_STRTTIME                 2 
#define REQQ_PHASE_STOPTIME                 3 
#define NUM_REQQ_PHASE_COLS                 4 

#define CAST_REQQ_PHASE_REQQ_ID      *(DBTINYINT *)
#define CAST_REQQ_PHASE_DUE_DATE     (char *)
#define CAST_REQQ_PHASE_STRTTIME     (char *)
#define CAST_REQQ_PHASE_STOPTIME     (char *)
 
#endif   /*    DB_REQQ_PHASE_H    */
