#ifndef    DB_MASKINOUT_MDAT_H
#define    DB_MASKINOUT_MDAT_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_maskinout_mdat.h
Description:  Header file for APS db table:  maskinout_mdat.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /home/aps/r2.1/etc/install/sh_scripts/Create_db_include_files.csh
Date:         Fri May 2 18:01:37 PDT 1997
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_maskinout_mdat.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_maskinout_mdat.h"
 
#define MASKINOUT_MDAT_SAT                      0 
#define MASKINOUT_MDAT_START_TIME               1 
#define MASKINOUT_MDAT_STOP_TIME                2 
#define MASKINOUT_MDAT_GEN_TIME                 3 
#define MASKINOUT_MDAT_VERSION                  4 
#define MASKINOUT_MDAT_EPHEMERIS                5 
#define NUM_MASKINOUT_MDAT_COLS                 6 

#define CAST_MASKINOUT_MDAT_SAT          (char *)
#define CAST_MASKINOUT_MDAT_START_TIME   (char *)
#define CAST_MASKINOUT_MDAT_STOP_TIME    (char *)
#define CAST_MASKINOUT_MDAT_GEN_TIME     (char *)
#define CAST_MASKINOUT_MDAT_VERSION      (char *)
#define CAST_MASKINOUT_MDAT_EPHEMERIS    (char *)
 
#endif   /*    DB_MASKINOUT_MDAT_H    */
