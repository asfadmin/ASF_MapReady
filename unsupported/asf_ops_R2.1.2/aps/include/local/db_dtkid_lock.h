#ifndef    DB_DTKID_LOCK_H
#define    DB_DTKID_LOCK_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_dtkid_lock.h
Description:  Header file for APS db table:  dtkid_lock.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /home/aps/r2.1/etc/install/sh_scripts/Create_db_include_files.csh
Date:         Tue May 13 12:57:24 PDT 1997
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_dtkid_lock.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_dtkid_lock.h"
 
#define DTKID_LOCK_SEMAPHORE                0 
#define NUM_DTKID_LOCK_COLS                 1 

#define CAST_DTKID_LOCK_SEMAPHORE    *(DBINT *)
 
#endif   /*    DB_DTKID_LOCK_H    */
