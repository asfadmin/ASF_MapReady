C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

C--------------------------------------------------------------------------
* f77_db_sscvrg.h
*
*
*    This file contains declarations for columns contained in the 
*  sscvrg relation. A Common block maps the declarations to a 
* C struct that is loaded by C routines... hence f77 can make 
* C calls to load the common structure and f77 can use the COMMON
* block to access the table data.
* NOTE: the order of the declared variable in the common block are
* different than those contained in the table definition, this is               * to avoid alignment problems.
* See the asociated C source file: f77_sscvrg.c
C--   character*100 SccsFileID
C--  -/'@(#)f77_db_sscvrg.inc	5.1 98/01/08 APS/ASF\0'/
C-- 
C-----------------------------------------------------------------------

C--    IMPLICIT NONE

       INTEGER*4 sscvrg_columns
       COMMON /sscvrg_columns/sscvrg_columns

       INTEGER*2 NUM_SSCVRG_COLS
       DATA NUM_SSCVRG_COLS/20/

       EXTERNAL load_f77_sscvrg_rec !$pragma c(load_f77_sscvrg_rec)

       REAL*8  sscvrg_strtet
       REAL*8  sscvrg_stopet
       REAL*8  sscvrg_strtlat
       REAL*8  sscvrg_stoplat
       REAL*8  sscvrg_nrlat1
       REAL*8  sscvrg_nrlon1
       REAL*8  sscvrg_farlat1
       REAL*8  sscvrg_farlon1
       REAL*8  sscvrg_nrlat2
       REAL*8  sscvrg_nrlon2
       REAL*8  sscvrg_farlat2
       REAL*8  sscvrg_farlon2
         

       INTEGER*4 sscvrg_darid
       INTEGER*4 sscvrg_rev

       CHARACTER*1 sscvrg_ascdsc 
       CHARACTER*33 sscvrg_sitename 
       CHARACTER*3 sscvrg_sat
       CHARACTER*4 sscvrg_sensor
       CHARACTER*22 sscvrg_strttime
       CHARACTER*22 sscvrg_stoptime
      
      COMMON /f77_sscvrg_rec/
     + sscvrg_strtet,
     + sscvrg_stopet,
     + sscvrg_strtlat,
     + sscvrg_stoplat,
     + sscvrg_nrlat1,
     + sscvrg_nrlon1,
     + sscvrg_farlat1,
     + sscvrg_farlon1,
     + sscvrg_nrlat2,
     + sscvrg_nrlon2,
     + sscvrg_farlat2,
     + sscvrg_farlon2,
     + sscvrg_darid,
     + sscvrg_rev,
     + sscvrg_ascdsc,
     + sscvrg_sitename,
     + sscvrg_sat,
     + sscvrg_sensor,
     + sscvrg_strttime,
     + sscvrg_stoptime


     

