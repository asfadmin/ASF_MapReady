#ifndef    DB_SAT_INCLUSION_PERIOD_H
#define    DB_SAT_INCLUSION_PERIOD_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_sat_inclusion_period.h
Description:  Header file for APS db table:  sat_inclusion_period.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:52:51 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_sat_inclusion_period.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_sat_inclusion_period.h"
 
#define SAT_INCLUSION_PERIOD_SAT                      0 
#define SAT_INCLUSION_PERIOD_LENGTH                   1 
#define NUM_SAT_INCLUSION_PERIOD_COLS                 2 

#define CAST_SAT_INCLUSION_PERIOD_SAT          (char *)
#define CAST_SAT_INCLUSION_PERIOD_LENGTH       *(DBINT *)
 
#endif   /*    DB_SAT_INCLUSION_PERIOD_H    */
