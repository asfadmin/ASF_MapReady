#ifndef    DB_DTK_H
#define    DB_DTK_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_dtk.h
Description:  Header file for APS db table:  dtk.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /home/aps/r2.1.2/etc/install/sh_scripts/Create_db_include_files.csh
Date:         Thu Jan 15 19:36:27 PST 1998
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_dtk.h	5.2 98/01/23 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_dtk.h"
 
#define DTK_SAT                      0 
#define DTK_SENSOR                   1 
#define DTK_REV                      2 
#define DTK_DTKID                    3 
#define DTK_FADTKID                  4 
#define DTK_DARID                    5 
#define DTK_ACTID                    6 
#define DTK_ASCDSC                   7 
#define DTK_STRTTIME                 8 
#define DTK_STOPTIME                 9 
#define DTK_STRTLAT                 10 
#define DTK_STOPLAT                 11 
#define DTK_NRLAT1                  12 
#define DTK_NRLON1                  13 
#define DTK_FARLAT1                 14 
#define DTK_FARLON1                 15 
#define DTK_NRLAT2                  16 
#define DTK_NRLON2                  17 
#define DTK_FARLAT2                 18 
#define DTK_FARLON2                 19 
#define DTK_LOOKANGL                20 
#define DTK_DTKSTAT                 21 
#define DTK_PROPOSED_DTKSTAT        22 
#define DTK_TRANSID                 23 
#define DTK_SITENAME                24 
#define DTK_NOTES                   25 
#define DTK_DTKDATE                 26 
#define DTK_STATION_ID              27 
#define DTK_FA_SCHEDULE_LINK        28 
#define DTK_PLANNER_QUICKLOOK       29 
#define DTK_SCIENCE_QUICKLOOK       30 
#define DTK_SUBMIT_TIME             31 
#define DTK_ANTENNA_ID              32 
#define DTK_FA_STRTTIME             33 
#define DTK_FA_STOPTIME             34 
#define DTK_FA_DURATION_MIN         35 
#define DTK_ASF_REDUCTION_MIN       36 
#define NUM_DTK_COLS                37 

#define CAST_DTK_SAT          (char *)
#define CAST_DTK_SENSOR       (char *)
#define CAST_DTK_REV          *(DBINT *)
#define CAST_DTK_DTKID        *(DBTINYINT *)
#define CAST_DTK_FADTKID      (char *)
#define CAST_DTK_DARID        *(DBINT *)
#define CAST_DTK_ACTID        (char *)
#define CAST_DTK_ASCDSC       *(DBCHAR *)
#define CAST_DTK_STRTTIME     (char *)
#define CAST_DTK_STOPTIME     (char *)
#define CAST_DTK_STRTLAT      *(DBREAL *)
#define CAST_DTK_STOPLAT      *(DBREAL *)
#define CAST_DTK_NRLAT1       *(DBREAL *)
#define CAST_DTK_NRLON1       *(DBREAL *)
#define CAST_DTK_FARLAT1      *(DBREAL *)
#define CAST_DTK_FARLON1      *(DBREAL *)
#define CAST_DTK_NRLAT2       *(DBREAL *)
#define CAST_DTK_NRLON2       *(DBREAL *)
#define CAST_DTK_FARLAT2      *(DBREAL *)
#define CAST_DTK_FARLON2      *(DBREAL *)
#define CAST_DTK_LOOKANGL     *(DBREAL *)
#define CAST_DTK_DTKSTAT      (char *)
#define CAST_DTK_PROPOSED_DTKSTAT      (char *)
#define CAST_DTK_TRANSID      (char *)
#define CAST_DTK_SITENAME     (char *)
#define CAST_DTK_NOTES        (char *)
#define CAST_DTK_DTKDATE      (char *)
#define CAST_DTK_STATION_ID   (char *)
#define CAST_DTK_FA_SCHEDULE_LINK      (char *)
#define CAST_DTK_PLANNER_QUICKLOOK     *(DBCHAR *)
#define CAST_DTK_SCIENCE_QUICKLOOK     *(DBCHAR *)
#define CAST_DTK_SUBMIT_TIME  (char *)
#define CAST_DTK_ANTENNA_ID   *(DBSMALLINT *)
#define CAST_DTK_FA_STRTTIME  (char *)
#define CAST_DTK_FA_STOPTIME  (char *)
#define CAST_DTK_FA_DURATION_MIN       *(DBREAL *)
#define CAST_DTK_ASF_REDUCTION_MIN     *(DBREAL *)
 
#endif   /*    DB_DTK_H    */
