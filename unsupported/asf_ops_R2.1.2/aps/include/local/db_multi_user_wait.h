#ifndef    DB_MULTI_USER_WAIT_H
#define    DB_MULTI_USER_WAIT_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_multi_user_wait.h
Description:  Header file for APS db table:  multi_user_wait.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /home/aps/r2.1/etc/install/sh_scripts/Create_db_include_files.csh
Date:         Mon Nov 11 11:16:54 PST 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_multi_user_wait.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_multi_user_wait.h"
 
#define MULTI_USER_WAIT_STATUS                   0 
#define NUM_MULTI_USER_WAIT_COLS                 1 

#define CAST_MULTI_USER_WAIT_STATUS       (char *)
 
#endif   /*    DB_MULTI_USER_WAIT_H    */
