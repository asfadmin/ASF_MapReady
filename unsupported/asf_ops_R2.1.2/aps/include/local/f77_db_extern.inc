C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	f77_db_extern.inc
C--
C--  Description:	
C--	
C--  Notes:
C--
C-- ==========================================================================
      character*100 SccsFileID
     -/'@(#)f77_db_extern.inc	5.1 98/01/08 APS/ASF\0'/

C--   IMPLICIT NONE

c---  External functions to support calls to SYBASE

      INTEGER*4 db_get_records
      EXTERNAL  db_get_records !$pragma c(db_get_records)

      INTEGER*4 db_ftn_first_record
      EXTERNAL  db_ftn_first_record !$pragma c(db_ftn_first_record)

      INTEGER*4 db_ftn_next_record
      EXTERNAL  db_ftn_next_record !$pragma c(db_ftn_next_record)

      EXTERNAL  db_ftn_free_llist !$pragma c(db_ftn_free_llist)

      CHARACTER*1024 FIELDS_TO_GET
      CHARACTER*1024 WHERE_CLAUSE
      CHARACTER*1024 FIELDS_TO_ORDER

      POINTER(llistptr, llist)
      POINTER(ptrptr, ptr)
      INTEGER*4 llist, ptr

      INTEGER*4 DATA_RECORD(0:60) 
      POINTER(P2_DATA_RECORD, DATA_RECORD)

