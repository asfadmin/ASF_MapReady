C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.

C--------------------------------------------------------------------------
* f77_db_cvrg.h
*
*
*    This file contains declarations for columns contained in the 
*  cvrg relation. A Common block maps the declarations to a 
* C struct that is loaded by C routines... hence f77 can make 
* C calls to load the common structure and f77 can use the COMMON
* block to access the table data.
* NOTE: the order of the declared variable in the common block are
* different than those contained in the table definition, this is               
* to avoid alignment problems.
* See the asociated C source file: f77_cvrg.c
C--   character*100 SccsFileID
C--  -/'@(#)f77_db_cvrg.inc	5.1 98/01/08 APS/ASF\0'/
C-- 
C-----------------------------------------------------------------------
C--    IMPLICIT NONE

       INTEGER*4 cvrg_columns
       COMMON /cvrg_columns/cvrg_columns 
       
       INTEGER*2 NUM_CVRG_COLS
       DATA NUM_CVRG_COLS/19/

       EXTERNAL load_f77_cvrg_rec !$pragma c(load_f77_cvrg_rec)



       REAL*8  cvrg_mjdate
       REAL*8  cvrg_sublat
       REAL*8  cvrg_sublon
       REAL*8  cvrg_nrlat
       REAL*8  cvrg_nrlon
       REAL*8  cvrg_farlat
       REAL*8  cvrg_farlon
       REAL*8  cvrg_satalt
       REAL*8  cvrg_sunang

       INTEGER*4  cvrg_marker
       INTEGER*4  cvrg_rev
       INTEGER*2  cvrg_opermode


       CHARACTER*1 cvrg_sunn
       CHARACTER*1 cvrg_asfmask
       CHARACTER*1 cvrg_crossflag
       CHARACTER*1 cvrg_ascdsc
       CHARACTER*3 cvrg_sat
       CHARACTER*4 cvrg_sensor
       CHARACTER*5 cvrg_masks


      COMMON /f77_cvrg_rec/
     + cvrg_mjdate,
     + cvrg_sublat,
     + cvrg_sublon,
     + cvrg_nrlat,
     + cvrg_nrlon,
     + cvrg_farlat,
     + cvrg_farlon,
     + cvrg_satalt, 
     + cvrg_sunang,
     + cvrg_marker,
     + cvrg_rev,
     + cvrg_opermode,
     + cvrg_sunn,
     + cvrg_asfmask,
     + cvrg_crossflag,
     + cvrg_ascdsc,
     + cvrg_sat,
     + cvrg_sensor,
     + cvrg_masks




      
     
    
   



