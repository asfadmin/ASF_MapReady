C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C--------------------------------------------------------------------------
* f77_db_ephemeris.h
*
*
*    This file contains declarations for columns contained in the 
*  ephemeris relation. A Common block maps the declarations to a 
* C struct that is loaded by C routines... hence f77 can make 
* C calls to load the common structure and f77 can use the COMMON
* block to access the table data.
* NOTE: the order of the declared variable in the common block are
* different than those contained in the table definition, this is               
* to avoid alignment problems.
* See the asociated C source file: f77_ephemeris.c
C-----------------------------------------------------------------------
      character*100 SccsFileID
     -/'@(#)f77_db_ephemeris.inc	5.1 98/01/08 APS/ASF\0'/

C--   IMPLICIT NONE

      INTEGER*4 ephemeris_columns
      COMMON /ephemeris_columns/ephemeris_columns

      INTEGER*2 NUM_EPHEMERIS_COLS
      DATA NUM_EPHEMERIS_COLS/18/

      EXTERNAL load_f77_ephemeris_rec !$pragma c(load_f77_ephemeris_rec)

       REAL*8  ephemeris_epoch
       
       INTEGER*4 ephemeris_startrev 
       INTEGER*4 ephemeris_endrev 
       
       CHARACTER*1 ephemeris_phase_name

       CHARACTER*100 ephemeris_filename
       CHARACTER*22 ephemeris_starttime
       CHARACTER*22 ephemeris_endtime 
       CHARACTER*3  ephemeris_sat
       CHARACTER*100 ephemeris_sv_filename
       CHARACTER*10 ephemeris_sv_type
       CHARACTER*5  ephemeris_sv_rev
       CHARACTER*21 ephemeris_sv_time
       CHARACTER*11 ephemeris_sv_r_x
       CHARACTER*11 ephemeris_sv_r_y
       CHARACTER*11 ephemeris_sv_r_z
       CHARACTER*11 ephemeris_sv_v_x
       CHARACTER*11 ephemeris_ sv_v_y
       CHARACTER*11 ephemeris_sv_v_z


      COMMON /f77_ephemeris_rec/
     + ephemeris_epoch,
     + ephemeris_startrev,
     + ephemeris_endrev,
     + ephemeris_phase_name,
     + ephemeris_filename,
     + ephemeris_starttime,
     + ephemeris_endtime,
     + ephemeris_sat,
     + ephemeris_sv_filename,
     + ephemeris_sv_type,
     + ephemeris_sv_rev,
     + ephemeris_sv_time,
     + ephemeris_sv_r_x,
     + ephemeris_sv_r_y,
     + ephemeris_sv_r_z,
     + ephemeris_sv_v_x,
     + ephemeris_sv_v_y,
     + ephemeris_sv_v_z    
