#ifndef    DB_ACTIV_CONF_H
#define    DB_ACTIV_CONF_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_activ_conf.h
Description:  Header file for APS db table:  activ_conf.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:51:11 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_activ_conf.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_activ_conf.h"
 
#define ACTIV_CONF_N_ACTY                   0 
#define ACTIV_CONF_M_ACTY                   1 
#define ACTIV_CONF_CONF_STATUS              2 
#define NUM_ACTIV_CONF_COLS                 3 

#define CAST_ACTIV_CONF_N_ACTY       *(DBSMALLINT *)
#define CAST_ACTIV_CONF_M_ACTY       *(DBSMALLINT *)
#define CAST_ACTIV_CONF_CONF_STATUS  *(DBSMALLINT *)
 
#endif   /*    DB_ACTIV_CONF_H    */
