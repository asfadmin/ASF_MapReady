#ifndef    DB_PERMISSION_COUNTER_H
#define    DB_PERMISSION_COUNTER_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_permission_counter.h
Description:  Header file for APS db table:  permission_counter.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /home/aps/r2.1/etc/install/sh_scripts/Create_db_include_files.csh
Date:         Mon Nov 11 11:17:29 PST 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_permission_counter.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_permission_counter.h"
 
#define PERMISSION_COUNTER_PERMISSION_ID            0 
#define NUM_PERMISSION_COUNTER_COLS                 1 

#define CAST_PERMISSION_COUNTER_PERMISSION_ID         *(DBINT *)
 
#endif   /*    DB_PERMISSION_COUNTER_H    */
