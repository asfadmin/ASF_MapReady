C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C--------------------------------------------------------------------------
* f77_db_dar.h
*
*
*    This file contains declarations for columns contained in the 
*  dar relation. A Common block maps the declarations to a 
* C struct that is loaded by C routines... hence f77 can make 
* C calls to load the common structure and f77 can use the COMMON
* block to access the table data.
* NOTE: the order of the declared variable in the common block are
* different than those contained in the table definition, this is               * to avoid alignment problems.
* See the asociated C source file: f77_dar.c
C--   character*100 SccsFileID
C--  -/'@(#)f77_db_dar.inc	5.1 98/01/08 APS/ASF\0'/
C-- 
C-----------------------------------------------------------------------

C--    IMPLICIT NONE

       INTEGER*4 dar_columns
       COMMON /dar_columns/dar_columns

       INTEGER*2 NUM_DAR_COLS
       DATA NUM_DAR_COLS/27/

       EXTERNAL load_f77_dar_rec !$pragma c(load_f77_dar_rec)


       REAL*8  dar_radius
       REAL*8  dar_nwlat
       REAL*8  dar_nwlon
       REAL*8  dar_nelat
       REAL*8  dar_nelon
       REAL*8  dar_selat
       REAL*8  dar_selon
       REAL*8  dar_swlat
       REAL*8  dar_swlon

       INTEGER*4  dar_darid
       INTEGER*4  dar_prvdarid
       INTEGER*4  dar_nobs
       INTEGER*4  dar_rev

       CHARACTER*1 dar_shape
       CHARACTER*1 dar_ascdsc
       CHARACTER*16 dar_fobs   
       CHARACTER*16 dar_userid
       CHARACTER*22 dar_reqtime
       CHARACTER*3 dar_reqstat
       CHARACTER*4 dar_prvreqsat
       CHARACTER*3 dar_sat
       CHARACTER*4 dar_sensor
       CHARACTER*22 dar_strttime
       CHARACTER*22 dar_endtime
       CHARACTER*33 dar_sitename
       CHARACTER*256 dar_usercmnt
       CHARACTER*256 dar_plnrcmnt

      COMMON /f77_dar_rec/
     + dar_radius,
     + dar_nwlat,
     + dar_ nwlon,
     + dar_nelat,
     + dar_nelon,
     + dar_selat,
     + dar_selon,
     + dar_swlat,
     + dar_swlon,
     + dar_darid,
     + dar_prvdarid,
     + dar_nobs,
     + dar_rev,
     + dar_shape,
     + dar_fobs,
     + dar_ascdsc,
     + dar_userid,
     + dar_reqtime,
     + dar_reqstat,
     + dar_prvreqsat,
     + dar_sat,
     + dar_sensor,
     + dar_strttime,
     + dar_endtime,
     + dar_sitename,
     + dar_usercmnt,
     + dar_plnrcmnt

