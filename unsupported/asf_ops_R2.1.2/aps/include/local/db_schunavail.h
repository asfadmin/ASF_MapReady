#ifndef    DB_SCHUNAVAIL_H
#define    DB_SCHUNAVAIL_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_schunavail.h
Description:  Header file for APS db table:  schunavail.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:53:06 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_schunavail.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_schunavail.h"
 
#define SCHUNAVAIL_FACID                    0 
#define SCHUNAVAIL_PRAREID                  1 
#define SCHUNAVAIL_SYSSUBSTAT               2 
#define SCHUNAVAIL_UTYPE                    3 
#define SCHUNAVAIL_UREASON                  4 
#define SCHUNAVAIL_NON_ESA                  5 
#define SCHUNAVAIL_RESERVED                 6 
#define SCHUNAVAIL_STRTTIME                 7 
#define SCHUNAVAIL_STOPTIME                 8 
#define SCHUNAVAIL_REMARKS                  9 
#define NUM_SCHUNAVAIL_COLS                10 

#define CAST_SCHUNAVAIL_FACID        (char *)
#define CAST_SCHUNAVAIL_PRAREID      (char *)
#define CAST_SCHUNAVAIL_SYSSUBSTAT   (char *)
#define CAST_SCHUNAVAIL_UTYPE        *(DBCHAR *)
#define CAST_SCHUNAVAIL_UREASON      *(DBCHAR *)
#define CAST_SCHUNAVAIL_NON_ESA      *(DBCHAR *)
#define CAST_SCHUNAVAIL_RESERVED     (char *)
#define CAST_SCHUNAVAIL_STRTTIME     (char *)
#define CAST_SCHUNAVAIL_STOPTIME     (char *)
#define CAST_SCHUNAVAIL_REMARKS      (char *)
 
#endif   /*    DB_SCHUNAVAIL_H    */
