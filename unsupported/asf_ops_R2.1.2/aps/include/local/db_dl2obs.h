#ifndef    DB_DL2OBS_H
#define    DB_DL2OBS_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_dl2obs.h
Description:  Header file for APS db table:  dl2obs.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /home/aps/r2.1/etc/install/sh_scripts/Create_db_include_files.csh
Date:         Tue May 13 16:42:31 PDT 1997
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_dl2obs.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_dl2obs.h"
 
#define DL2OBS_SAT                      0 
#define DL2OBS_REV_OBS                  1 
#define DL2OBS_DTKID_OBS                2 
#define DL2OBS_REV_DL                   3 
#define DL2OBS_DTKID_DL                 4 
#define NUM_DL2OBS_COLS                 5 

#define CAST_DL2OBS_SAT          (char *)
#define CAST_DL2OBS_REV_OBS      *(DBINT *)
#define CAST_DL2OBS_DTKID_OBS    *(DBTINYINT *)
#define CAST_DL2OBS_REV_DL       *(DBINT *)
#define CAST_DL2OBS_DTKID_DL     *(DBTINYINT *)
 
#endif   /*    DB_DL2OBS_H    */
