#ifndef    DB_SSCVRG_H
#define    DB_SSCVRG_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_sscvrg.h
Description:  Header file for APS db table:  sscvrg.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:53:22 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_sscvrg.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_sscvrg.h"
 
#define SSCVRG_DARID                    0 
#define SSCVRG_SITENAME                 1 
#define SSCVRG_SAT                      2 
#define SSCVRG_SENSOR                   3 
#define SSCVRG_REV                      4 
#define SSCVRG_STRTTIME                 5 
#define SSCVRG_STOPTIME                 6 
#define SSCVRG_STRTET                   7 
#define SSCVRG_STOPET                   8 
#define SSCVRG_STRTLAT                  9 
#define SSCVRG_STOPLAT                 10 
#define SSCVRG_NRLAT1                  11 
#define SSCVRG_NRLON1                  12 
#define SSCVRG_FARLAT1                 13 
#define SSCVRG_FARLON1                 14 
#define SSCVRG_NRLAT2                  15 
#define SSCVRG_NRLON2                  16 
#define SSCVRG_FARLAT2                 17 
#define SSCVRG_FARLON2                 18 
#define SSCVRG_ASCDSC                  19 
#define NUM_SSCVRG_COLS                20 

#define CAST_SSCVRG_DARID        *(DBINT *)
#define CAST_SSCVRG_SITENAME     (char *)
#define CAST_SSCVRG_SAT          (char *)
#define CAST_SSCVRG_SENSOR       (char *)
#define CAST_SSCVRG_REV          *(DBINT *)
#define CAST_SSCVRG_STRTTIME     (char *)
#define CAST_SSCVRG_STOPTIME     (char *)
#define CAST_SSCVRG_STRTET       *(DBFLT8 *)
#define CAST_SSCVRG_STOPET       *(DBFLT8 *)
#define CAST_SSCVRG_STRTLAT      *(DBREAL *)
#define CAST_SSCVRG_STOPLAT      *(DBREAL *)
#define CAST_SSCVRG_NRLAT1       *(DBREAL *)
#define CAST_SSCVRG_NRLON1       *(DBREAL *)
#define CAST_SSCVRG_FARLAT1      *(DBREAL *)
#define CAST_SSCVRG_FARLON1      *(DBREAL *)
#define CAST_SSCVRG_NRLAT2       *(DBREAL *)
#define CAST_SSCVRG_NRLON2       *(DBREAL *)
#define CAST_SSCVRG_FARLAT2      *(DBREAL *)
#define CAST_SSCVRG_FARLON2      *(DBREAL *)
#define CAST_SSCVRG_ASCDSC       *(DBCHAR *)
 
#endif   /*    DB_SSCVRG_H    */
