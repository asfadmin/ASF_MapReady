#ifndef    DB_SAT_REV_DTKID_H
#define    DB_SAT_REV_DTKID_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_sat_rev_dtkid.h
Description:  Header file for APS db table:  sat_rev_dtkid.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /home/aps/r2.1/etc/install/sh_scripts/Create_db_include_files.csh
Date:         Tue May 13 12:58:08 PDT 1997
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_sat_rev_dtkid.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_sat_rev_dtkid.h"
 
#define SAT_REV_DTKID_SAT                      0 
#define SAT_REV_DTKID_REV                      1 
#define SAT_REV_DTKID_DTKID                    2 
#define NUM_SAT_REV_DTKID_COLS                 3 

#define CAST_SAT_REV_DTKID_SAT          (char *)
#define CAST_SAT_REV_DTKID_REV          *(DBINT *)
#define CAST_SAT_REV_DTKID_DTKID        *(DBTINYINT *)
 
#endif   /*    DB_SAT_REV_DTKID_H    */
