#ifndef    DB_SATSENSOR_H
#define    DB_SATSENSOR_H
 
#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif
 
/*==============================================================================
Filename:     db_satsensor.h
Description:  Header file for APS db table:  satsensor.
              For use by programs that call lib_sybint or 
              routines like db_get_records().   
 
Creator:      /home/aps/r2.1/etc/install/sh_scripts/Create_db_include_files.csh
Date:         Tue Mar 11 11:09:07 PST 1997
 
Notes:        This file was created by a c-shell script.
 
==============================================================================*/
#pragma ident   "@(#)db_satsensor.h	5.1 98/01/08 APS/ASF"
#pragma ident   "@(#) /home/aps/r2.1.2/include/local/SCCS/s.db_satsensor.h"
 
#define SATSENSOR_SAT                      0 
#define SATSENSOR_SENSOR                   1 
#define SATSENSOR_OPERMODE                 2 
#define SATSENSOR_CHOPERMODE               3 
#define SATSENSOR_BEAMRADIUSD              4 
#define SATSENSOR_LOOKANGLED               5 
#define SATSENSOR_CVRG_ALLOWED             6 
#define SATSENSOR_LOW_BIT_RATE_FLAG        7 
#define NUM_SATSENSOR_COLS                 8 

#define CAST_SATSENSOR_SAT          (char *)
#define CAST_SATSENSOR_SENSOR       (char *)
#define CAST_SATSENSOR_OPERMODE     *(DBINT *)
#define CAST_SATSENSOR_CHOPERMODE   (char *)
#define CAST_SATSENSOR_BEAMRADIUSD  *(DBREAL *)
#define CAST_SATSENSOR_LOOKANGLED   *(DBREAL *)
#define CAST_SATSENSOR_CVRG_ALLOWED          *(DBCHAR *)
#define CAST_SATSENSOR_LOW_BIT_RATE_FLAG     *(DBCHAR *)
 
#endif   /*    DB_SATSENSOR_H    */
