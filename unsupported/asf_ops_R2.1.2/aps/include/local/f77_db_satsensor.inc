C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C--------------------------------------------------------------------------
* f77_db_satsensor.inc
*
*
*    This file contains declarations for columns contained in the 
*  satsensor relation. A Common block maps the declarations to a 
* C struct that is loaded by C routines... hence f77 can make 
* C calls to load the common structure and f77 can use the COMMON
* block to access the table data.
* NOTE: the order of the declared variable in the common block are
* different than those contained in the table definition, this is               
* to avoid alignment problems.
* See the asociated C source file: f77_satsensor.c
C-----------------------------------------------------------------------
      character*100 SccsFileID
     -/'@(#)f77_db_satsensor.inc	5.1 98/01/08 APS/ASF\0'/

C--    IMPLICIT NONE

       INTEGER*4 satsensor_columns
       COMMON /satsensor_columns/satsensor_columns

       INTEGER*2 NUM_SATSENSOR_COLS/6/

       EXTERNAL load_f77_rec !$pragma c(load_f77_satsensor_rec)


       REAL*8  satsensor_beamradiusd
       REAL*8  satsensor_lookangl
       
       INTEGER*4 satsensor_opermode
       INTEGER*4 satsensor_chopermode

        
       CHARACTER*3 satsensor_sat
       CHARACTER*4 satsensor_sensor
      
       COMMON /f77_satsensor_rec/
     + satsensor_beamradiusd,
     + satsensor_lookangl,
     + satsensor_opermode,
     + satsensor_chopermode,
     + satsensor_sat,
     + satsensor_sensor



















































































































