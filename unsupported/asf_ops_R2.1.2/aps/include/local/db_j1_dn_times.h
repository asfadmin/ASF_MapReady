#ifndef    DB_J1_DN_TIMES_H
#define    DB_J1_DN_TIMES_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_j1_dn_times.h
Description:  Header file for APS db table:  j1_dn_times.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:52:29 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_j1_dn_times.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_j1_dn_times.h"
 
#define J1_DN_TIMES_STRTTIME                 0 
#define J1_DN_TIMES_STOPTIME                 1 
#define J1_DN_TIMES_SAR_STATUS               2 
#define J1_DN_TIMES_OPS_STATUS               3 
#define J1_DN_TIMES_MDR_STATUS               4 
#define J1_DN_TIMES_MDR_OP_TIME              5 
#define J1_DN_TIMES_MDT_STATUS               6 
#define NUM_J1_DN_TIMES_COLS                 7 

#define CAST_J1_DN_TIMES_STRTTIME     (char *)
#define CAST_J1_DN_TIMES_STOPTIME     (char *)
#define CAST_J1_DN_TIMES_SAR_STATUS   *(DBCHAR *)
#define CAST_J1_DN_TIMES_OPS_STATUS   *(DBCHAR *)
#define CAST_J1_DN_TIMES_MDR_STATUS   *(DBCHAR *)
#define CAST_J1_DN_TIMES_MDR_OP_TIME  *(DBSMALLINT *)
#define CAST_J1_DN_TIMES_MDT_STATUS   *(DBCHAR *)
 
#endif   /*    DB_J1_DN_TIMES_H    */
