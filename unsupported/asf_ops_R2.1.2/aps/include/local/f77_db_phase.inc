C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
*---------------------------------------------------------------------
*  f77_db_phase.h
*     This file contains declarations for columns contained in the
*  phase relation.  A COMMON block maps the declarations to a
*  C struct that is loaded by C routines... hence f77 can make
*  C calls to load the common structure and F77 can use the COMMMON 
*  block to access the table data 
*
*   NOTE: the order of the declared variables in the COMMON block are
*   different than those contained in the table definition, this is
*   to avoid alignment problems.  The variables are are ordered a
*   according to type with REALs first, INTEGERs second, DBCHARs third,
*   and char fourth.  Any additions should be placed in the associated
*   group in the COMMON block
*
*  See the associated C source file: f77_phase.c
C---------------------------------------------------------------------
      character*100 SccsFileID
     -/'@(#)f77_db_phase.inc	5.1 98/01/08 APS/ASF\0'/

C--   IMPLICIT NONE

      INTEGER*4 phase_columns
      COMMON /phase_columns/phase_columns

      INTEGER*2 NUM_PHASE_COLS
      DATA NUM_PHASE_COLS/19/

      EXTERNAL  load_f77_phase_rec !$pragma c(load_f77_phase_rec)

      REAL*8	phase_phase_lon 
      REAL*8	phase_orb_a 
      REAL*8	phase_orb_e 
      REAL*8	phase_orb_i 
      REAL*8	phase_orb_arg_peri 
      REAL*8	phase_rsp_0_lon 
      REAL*8	phase_EW_tol_km 
      REAL*8	phase_NS_tol_min 
      INTEGER*4	phase_phase_days 
      INTEGER*4	phase_phase_orbits
      INTEGER*4	phase_last_rev 
      INTEGER*4	phase_cycle_days 
      INTEGER*4	phase_cycle_revs 
      INTEGER*4	phase_n_rows 
      INTEGER*4	phase_min_row 
      INTEGER*4	phase_max_row 
      CHARACTER*1	phase_phase_name 
      CHARACTER*3	phase_sat 
      CHARACTER*22	phase_phase_start 
      CHARACTER*6   PADDING

      COMMON /f77_phase_rec/
     + phase_phase_lon,
     + phase_orb_a,
     + phase_orb_e,
     + phase_orb_i,
     + phase_orb_arg_peri,
     + phase_rsp_0_lon,
     + phase_EW_tol_km,
     + phase_NS_tol_min,
     + phase_phase_days,
     + phase_phase_orbits,
     + phase_last_rev,
     + phase_cycle_days,
     + phase_cycle_revs,
     + phase_n_rows,
     + phase_min_row,
     + phase_max_row,
     + phase_phase_name,
     + phase_sat,
     + phase_phase_start,
     + PADDING

