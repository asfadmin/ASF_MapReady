
C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C--------------------------------------------------------------------------
* f77_db_site.h
*
*
*    This file contains declarations for columns contained in the 
*  site relation. A Common block maps the declarations to a 
* C struct that is loaded by C routines... hence f77 can make 
* C calls to load the common structure and f77 can use the COMMON
* block to access the table data.
* NOTE: the order of the declared variable in the common block are
* different than those contained in the table definition, this is               
* to avoid alignment problems.
* See the asociated C source file: f77_site.c
C--   character*100 SccsFileID
C--  -/'@(#)f77_db_site.inc	5.1 98/01/08 APS/ASF\0'/
C-- 
C-----------------------------------------------------------------------

C--    IMPLICIT NONE

       INTEGER*4 site_columns
       COMMON /site_columns/site_columns

       INTEGER*2 NUM_SITE_COLS
       DATA NUM_SITE_COLS/12/

       EXTERNAL load_f77_site_rec !$pragma c(load_f77_site_rec)


       REAL*8  site_radius
       REAL*8  site_nwlat
       REAL*8  site_nwlon
       REAL*8  site_nelat
       REAL*8  site_nelon
       REAL*8  site_selat
       REAL*8  site_selon
       REAL*8  site_swlat
       REAL*8  site_swlon

       CHARACTER*1 site_shape
       CHARACTER*32 site_sitename
       CHARACTER*50 site_comments
     

      COMMON /f77_site_rec/
     + site_radius,
     + site_nwlat,
     + site_nwlon,
     + site_nelat,
     + site_nelon,
     + site_selat,
     + site_selon,
     + site_swlat,
     + site_swlon,
     + site_shape,
     + site_sitename,
     + site_comments

