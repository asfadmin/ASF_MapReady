#ifndef    DB_CSA_DATA_H
#define    DB_CSA_DATA_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_csa_data.h
Description:  Header file for APS db table:  csa_data.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:51:42 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_csa_data.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_csa_data.h"
 
#define CSA_DATA_UNAVAIL_EVENT_COUNTER    0 
#define CSA_DATA_RAR_DAILY_COUNTER        1 
#define NUM_CSA_DATA_COLS                 2 

#define CAST_CSA_DATA_UNAVAIL_EVENT_COUNTER *(DBINT *)
#define CAST_CSA_DATA_RAR_DAILY_COUNTER     *(DBINT *)
 
#endif   /*    DB_CSA_DATA_H    */
