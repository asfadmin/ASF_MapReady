#ifndef    DB_SEG_H
#define    DB_SEG_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_seg.h
Description:  Header file for APS db table:  seg.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:53:11 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_seg.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_seg.h"
 
#define SEG_DARID                    0 
#define SEG_SEGID                    1 
#define SEG_SAT                      2 
#define SEG_SENSOR                   3 
#define SEG_REV                      4 
#define SEG_DTKID                    5 
#define SEG_ASCDSC                   6 
#define SEG_STRTTIME                 7 
#define SEG_STOPTIME                 8 
#define SEG_STRTLAT                  9 
#define SEG_STOPLAT                 10 
#define SEG_NRLAT1                  11 
#define SEG_NRLON1                  12 
#define SEG_FARLAT1                 13 
#define SEG_FARLON1                 14 
#define SEG_NRLAT2                  15 
#define SEG_NRLON2                  16 
#define SEG_FARLAT2                 17 
#define SEG_FARLON2                 18 
#define SEG_LOOKANGL                19 
#define SEG_SEGSTAT                 20 
#define SEG_SEGDATE                 21 
#define NUM_SEG_COLS                22 

#define CAST_SEG_DARID        *(DBINT *)
#define CAST_SEG_SEGID        *(DBSMALLINT *)
#define CAST_SEG_SAT          (char *)
#define CAST_SEG_SENSOR       (char *)
#define CAST_SEG_REV          *(DBINT *)
#define CAST_SEG_DTKID        (char *)
#define CAST_SEG_ASCDSC       *(DBCHAR *)
#define CAST_SEG_STRTTIME     (char *)
#define CAST_SEG_STOPTIME     (char *)
#define CAST_SEG_STRTLAT      *(DBREAL *)
#define CAST_SEG_STOPLAT      *(DBREAL *)
#define CAST_SEG_NRLAT1       *(DBREAL *)
#define CAST_SEG_NRLON1       *(DBREAL *)
#define CAST_SEG_FARLAT1      *(DBREAL *)
#define CAST_SEG_FARLON1      *(DBREAL *)
#define CAST_SEG_NRLAT2       *(DBREAL *)
#define CAST_SEG_NRLON2       *(DBREAL *)
#define CAST_SEG_FARLAT2      *(DBREAL *)
#define CAST_SEG_FARLON2      *(DBREAL *)
#define CAST_SEG_LOOKANGL     *(DBREAL *)
#define CAST_SEG_SEGSTAT      (char *)
#define CAST_SEG_SEGDATE      (char *)
 
#endif   /*    DB_SEG_H    */
