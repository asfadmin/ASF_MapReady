#ifndef    DB_DAR_H
#define    DB_DAR_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_dar.h
Description:  Header file for APS db table:  dar.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /home/aps/r2.1.1/etc/install/sh_scripts/Create_db_include_files.csh
Date:         Mon Sep 8 17:41:08 PDT 1997
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_dar.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_dar.h"
 
#define DAR_DARID                    0 
#define DAR_USERID                   1 
#define DAR_REQTIME                  2 
#define DAR_REQSTAT                  3 
#define DAR_PRVDARID                 4 
#define DAR_PRVREQSTAT               5 
#define DAR_SAT                      6 
#define DAR_SENSOR                   7 
#define DAR_STRTTIME                 8 
#define DAR_ENDTIME                  9 
#define DAR_SITENAME                10 
#define DAR_SHAPE                   11 
#define DAR_RADIUS                  12 
#define DAR_NWLAT                   13 
#define DAR_NWLON                   14 
#define DAR_NELAT                   15 
#define DAR_NELON                   16 
#define DAR_SELAT                   17 
#define DAR_SELON                   18 
#define DAR_SWLAT                   19 
#define DAR_SWLON                   20 
#define DAR_NOBS                    21 
#define DAR_FOBS                    22 
#define DAR_REV                     23 
#define DAR_ASCDSC                  24 
#define DAR_USERCMNT                25 
#define DAR_PLNRCMNT                26 
#define DAR_QUICKLOOK               27 
#define DAR_J1_OBS_FREQ             28 
#define NUM_DAR_COLS                29 

#define CAST_DAR_DARID        *(DBINT *)
#define CAST_DAR_USERID       (char *)
#define CAST_DAR_REQTIME      (char *)
#define CAST_DAR_REQSTAT      (char *)
#define CAST_DAR_PRVDARID     *(DBINT *)
#define CAST_DAR_PRVREQSTAT   (char *)
#define CAST_DAR_SAT          (char *)
#define CAST_DAR_SENSOR       (char *)
#define CAST_DAR_STRTTIME     (char *)
#define CAST_DAR_ENDTIME      (char *)
#define CAST_DAR_SITENAME     (char *)
#define CAST_DAR_SHAPE        *(DBCHAR *)
#define CAST_DAR_RADIUS       *(DBREAL *)
#define CAST_DAR_NWLAT        *(DBREAL *)
#define CAST_DAR_NWLON        *(DBREAL *)
#define CAST_DAR_NELAT        *(DBREAL *)
#define CAST_DAR_NELON        *(DBREAL *)
#define CAST_DAR_SELAT        *(DBREAL *)
#define CAST_DAR_SELON        *(DBREAL *)
#define CAST_DAR_SWLAT        *(DBREAL *)
#define CAST_DAR_SWLON        *(DBREAL *)
#define CAST_DAR_NOBS         *(DBTINYINT *)
#define CAST_DAR_FOBS         (char *)
#define CAST_DAR_REV          *(DBINT *)
#define CAST_DAR_ASCDSC       *(DBCHAR *)
#define CAST_DAR_USERCMNT     (char *)
#define CAST_DAR_PLNRCMNT     (char *)
#define CAST_DAR_QUICKLOOK    *(DBCHAR *)
#define CAST_DAR_J1_OBS_FREQ  *(DBINT *)
 
#endif   /*    DB_DAR_H    */
