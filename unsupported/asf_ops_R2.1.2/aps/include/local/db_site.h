#ifndef    DB_SITE_H
#define    DB_SITE_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_site.h
Description:  Header file for APS db table:  site.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /ua/aps/aps/r1bprime/bin/Create_db_include_files.csh
Date:         Fri Apr 12 20:53:17 PDT 1996
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_site.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_site.h"
 
#define SITE_SITENAME                 0 
#define SITE_SHAPE                    1 
#define SITE_RADIUS                   2 
#define SITE_NWLAT                    3 
#define SITE_NWLON                    4 
#define SITE_NELAT                    5 
#define SITE_NELON                    6 
#define SITE_SELAT                    7 
#define SITE_SELON                    8 
#define SITE_SWLAT                    9 
#define SITE_SWLON                   10 
#define SITE_COMMENTS                11 
#define NUM_SITE_COLS                12 

#define CAST_SITE_SITENAME     (char *)
#define CAST_SITE_SHAPE        *(DBCHAR *)
#define CAST_SITE_RADIUS       *(DBREAL *)
#define CAST_SITE_NWLAT        *(DBREAL *)
#define CAST_SITE_NWLON        *(DBREAL *)
#define CAST_SITE_NELAT        *(DBREAL *)
#define CAST_SITE_NELON        *(DBREAL *)
#define CAST_SITE_SELAT        *(DBREAL *)
#define CAST_SITE_SELON        *(DBREAL *)
#define CAST_SITE_SWLAT        *(DBREAL *)
#define CAST_SITE_SWLON        *(DBREAL *)
#define CAST_SITE_COMMENTS     (char *)
 
#endif   /*    DB_SITE_H    */
