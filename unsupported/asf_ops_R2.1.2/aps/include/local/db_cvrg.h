#ifndef    DB_CVRG_H
#define    DB_CVRG_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_cvrg.h
Description:  Header file for APS db table:  cvrg.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:51:47 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_cvrg.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_cvrg.h"
 
#define CVRG_MARKER                   0 
#define CVRG_SAT                      1 
#define CVRG_SENSOR                   2 
#define CVRG_MJDATE                   3 
#define CVRG_REV                      4 
#define CVRG_SUBLAT                   5 
#define CVRG_SUBLON                   6 
#define CVRG_NRLAT                    7 
#define CVRG_NRLON                    8 
#define CVRG_FARLAT                   9 
#define CVRG_FARLON                  10 
#define CVRG_SATALT                  11 
#define CVRG_SUN                     12 
#define CVRG_SUNANG                  13 
#define CVRG_STATION_ID              14 
#define CVRG_MASKS                   15 
#define CVRG_OPERMODE                16 
#define CVRG_CROSSFLAG               17 
#define CVRG_ASCDSC                  18 
#define NUM_CVRG_COLS                19 

#define CAST_CVRG_MARKER       *(DBINT *)
#define CAST_CVRG_SAT          (char *)
#define CAST_CVRG_SENSOR       (char *)
#define CAST_CVRG_MJDATE       *(DBFLT8 *)
#define CAST_CVRG_REV          *(DBINT *)
#define CAST_CVRG_SUBLAT       *(DBREAL *)
#define CAST_CVRG_SUBLON       *(DBREAL *)
#define CAST_CVRG_NRLAT        *(DBREAL *)
#define CAST_CVRG_NRLON        *(DBREAL *)
#define CAST_CVRG_FARLAT       *(DBREAL *)
#define CAST_CVRG_FARLON       *(DBREAL *)
#define CAST_CVRG_SATALT       *(DBREAL *)
#define CAST_CVRG_SUN          *(DBCHAR *)
#define CAST_CVRG_SUNANG       *(DBREAL *)
#define CAST_CVRG_STATION_ID   (char *)
#define CAST_CVRG_MASKS        (char *)
#define CAST_CVRG_OPERMODE     *(DBTINYINT *)
#define CAST_CVRG_CROSSFLAG    *(DBCHAR *)
#define CAST_CVRG_ASCDSC       *(DBCHAR *)
 
#endif   /*    DB_CVRG_H    */
