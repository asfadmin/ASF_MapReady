#ifndef    DB_SCHSTAT_H
#define    DB_SCHSTAT_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_schstat.h
Description:  Header file for APS db table:  schstat.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:53:01 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_schstat.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_schstat.h"
 
#define SCHSTAT_STATUS                   0 
#define SCHSTAT_PAP                      1 
#define SCHSTAT_WOS                      2 
#define SCHSTAT_EURF                     3 
#define SCHSTAT_REQQ                     4 
#define SCHSTAT_REQW                     5 
#define SCHSTAT_OPLE                     6 
#define NUM_SCHSTAT_COLS                 7 

#define CAST_SCHSTAT_STATUS       (char *)
#define CAST_SCHSTAT_PAP          *(DBCHAR *)
#define CAST_SCHSTAT_WOS          *(DBCHAR *)
#define CAST_SCHSTAT_EURF         *(DBCHAR *)
#define CAST_SCHSTAT_REQQ         *(DBCHAR *)
#define CAST_SCHSTAT_REQW         *(DBCHAR *)
#define CAST_SCHSTAT_OPLE         *(DBCHAR *)
 
#endif   /*    DB_SCHSTAT_H    */
