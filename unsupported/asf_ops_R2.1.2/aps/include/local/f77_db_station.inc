C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C--------------------------------------------------------------------------
* f77_db_station.h
*
*
*    This file contains declarations for columns contained in the 
*  station relation. A Common block maps the declarations to a 
* C struct that is loaded by C routines... hence f77 can make 
* C calls to load the common structure and f77 can use the COMMON
* block to access the table data.
* NOTE: the order of the declared variable in the common block are
* different than those contained in the table definition, this is               
* to avoid alignment problems.
* See the asociated C source file: f77_station.c
C-----------------------------------------------------------------------
      character*100 SccsFileID
     -/'@(#)f77_db_station.inc	5.1 98/01/08 APS/ASF\0'/

C--    IMPLICIT NONE

       INTEGER*4 station_columns
       COMMON /station_columns/station_columns

       INTEGER*2 NUM_STATION_COLS
       DATA NUM_STATION_COLS/9/

       EXTERNAL load_f77_station_rec !$pragma c(load_f77_station_rec)


       REAL*8  station_stalat
       REAL*8  station_stalon
       REAL*8  station_staltm
       REAL*8  station_stradiuskm
       REAL*8  station_runbuffkm
       REAL*8  station_elevangle
       
       CHARACTER*4 station_stationid
       CHARACTER*20 station_stationname
       CHARACTER*3 station_sat

      COMMON /f77_station_rec/
     + station_stalat,
     + station_stalon,
     + station_staltm,
     + station_stradiuskm,
     + station_runbuffkm,
     + station_elevangle,
     + station_stationid,
     + station_stationname,
     + station_sat
     
   
 

