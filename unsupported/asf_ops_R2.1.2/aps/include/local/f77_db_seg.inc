C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C--------------------------------------------------------------------------
* f77_db_seg.inc
*
*
*    This file contains declarations for columns contained in the 
*  seg relation. A Common block maps the declarations to a 
* C struct that is loaded by C routines... hence f77 can make 
* C calls to load the common structure and f77 can use the COMMON
* block to access the table data.
* NOTE: the order of the declared variable in the common block are
* different than those contained in the table definition, this is    
* to avoid alignment problems.
* See the asociated C source file: f77_seg.c
C--   character*100 SccsFileID
C--  -/'@(#)f77_db_seg.inc	5.1 98/01/08 APS/ASF\0'/
C-- 
C-----------------------------------------------------------------------

C--    IMPLICIT NONE

        INTEGER*4 seg_columns
        COMMON /seg_columns/seg_columns

       INTEGER*2 NUM_SEG_COLS/22/

       EXTERNAL load_f77_seg_rec !$pragma c(load_f77_seg_rec)


       REAL*8  seg_strtlat 
       REAL*8  seg_stoplat
       REAL*8  seg_nrlat1
       REAL*8  seg_nrlon1
       REAL*8  seg_farlat1
       REAL*8  seg_farlon1
       REAL*8  seg_nrlat2
       REAL*8  seg_nrlon2 
       REAL*8  seg_farlat2
       REAL*8  seg_farlon2
       REAL*8  seg_lookangl


       INTEGER*4 seg_darid
       INTEGER*4 seg_rev
       INTEGER*2 seg_segid
    
       CHARACTER*1 seg_ascdsc
       CHARACTER*3 seg_sat
       CHARACTER*4 seg_sensor
       CHARACTER*16 seg_dtkid 
       CHARACTER*22 seg_strttime
       CHARACTER*22 seg_stoptime
       CHARACTER*4 seg_segstat
       CHARACTER*10 seg_segdate

       COMMON/f77_seg_rec/
     + seg_strtlat,
     + seg_stoplat,
     + seg_nrlat1,
     + seg_nrlon1,
     + seg_farlat1,
     + seg_farlon1,
     + seg_nrlat2,
     + seg_nrlon2,
     + seg_farlat2,
     + seg_farlon2,
     + seg_lookangl,
     + seg_darid,
     + seg_rev,
     + seg_segid,
     + seg_ascdsc,
     + seg_sat,
     + seg_sensor,
     + seg_dtkid,
     + seg_strttime,
     + seg_stoptime,
     + seg_segstat,
     + seg_segdate




