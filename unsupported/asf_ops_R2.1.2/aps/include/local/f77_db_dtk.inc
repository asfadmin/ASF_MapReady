C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C--------------------------------------------------------------------------
* f77_db_dtk.inc
*
*
*    This file contains declarations for columns contained in the 
*  dtk relation. A Common block maps the declarations to a 
* C struct that is loaded by C routines... hence f77 can make 
* C calls to load the common structure and f77 can use the COMMON
* block to access the table data.
* NOTE: the order of the declared variable in the common block are
* different than those contained in the table definition, this is              * to avoid alignment problems.
* See the asociated C source file: f77_dtk.c
C--   character*100 SccsFileID
C--  -/'@(#)f77_db_dtk.inc	5.1 98/01/08 APS/ASF\0'/
C-- 
C-----------------------------------------------------------------------

C--    IMPLICIT NONE

       INTEGER*4 dtk_columns
       COMMON /dtk_columns/dtk_columns

       INTEGER*2 NUM_DTK_COLS/30/

       EXTERNAL load_f77_dtk_rec !$pragma c(load_f77_dtk_rec)


       REAL*8  dtk_strtlat 
       REAL*8  dtk_stoplat
       REAL*8  dtk_nrlat1
       REAL*8  dtk_nrlonl
       REAL*8  dtk_farlatl
       REAL*8  dtk_farlon1
       REAL*8  dtk_nrlat2
       REAL*8  dtk_nrlon2
       REAL*8  dtk_farlat2
       REAL*8  dtk_farlon2
       REAL*8  dtk_lookangl

       INTEGER*4 dtk_rev
       INTEGER*4 dtk_darid
       
       INTEGER*2 dtk_dtkid
       INTEGER*2 dtk_e1
       INTEGER*2 dtk_j1
       INTEGER*2 dtk_rs
       INTEGER*2 dtk_e1_pvalue

       CHARACTER*1 dtk_ascdsc
       CHARACTER*3 dtk_sat
       CHARACTER*4 dtk_sensor
       CHARACTER*21 dtk_fadtkid
       CHARACTER*7 dtk_actid
       CHARACTER*22 dtk_strttime
       CHARACTER*22 dtk_endtime
       CHARACTER*4 dtk_dtkstat
       CHARACTER*3 dtk_transid
       CHARACTER*33 dtk_sitename
       CHARACTER*41 dtk_notes
       CHARACTER*22 dtk_dtkdate

       COMMON/f77_dtk_rec/
     + dtk_strtlat,
     + dtk_stoplat,
     + dtk_nrlat1,
     + dtk_nrlonl,
     + dtk_farlatl,
     + dtk_farlon1, 
     + dtk_nrlat2,
     + dtk_nrlon2,
     + dtk_farlat2,
     + dtk_farlon2,
     + dtk_lookangl,
     + dtk_rev,
     + dtk_darid,
     + dtk_dtkid,
     + dtk_e1,
     + dtk_j1,
     + dtk_rs,
     + dtk_e1_pvalue,
     + dtk_ascdsc,
     + dtk_sat,
     + dtk_sensor,
     + dtk_fadtkid,
     + dtk_actid,
     + dtk_strttime,
     + dtk_endtime,
     + dtk_dtkstat,
     + dtk_transid,
     + dtk_sitename,
     + dtk_notes,
     + dtk_dtkdate




