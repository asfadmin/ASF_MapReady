C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	mapr_db_extern.inc
C--
C--  Description:	
C--	
C--  Notes:
C--
C--   character*100 SccsFileID
C--  -/'@(#)mapr_db_extern.inc	5.1 98/01/08 APS/ASF\0'/
C-- 
C-- ==========================================================================

C--   IMPLICIT NONE  

      INTEGER*4 f77_getenv
      EXTERNAL  f77_getenv !$pragma c(f77_getenv)

      INTEGER*4 db_get_records
      EXTERNAL  db_get_records !$pragma c(db_get_records)

      INTEGER*4 db_num_records
      EXTERNAL  db_num_records !$pragma c(db_num_records)

      INTEGER*4 db_ftn_first_record
      EXTERNAL  db_ftn_first_record !$pragma c(db_ftn_first_record)
 
      INTEGER*4 db_ftn_next_record
      EXTERNAL  db_ftn_next_record !$pragma c(db_ftn_next_record)

      EXTERNAL  db_ftn_free_llist !$pragma c(db_ftn_free_llist)

      EXTERNAL  retrieve_db_rec !$pragma c(retrieve_db_rec)

      EXTERNAL  get_cvrg_rec !$pragma c(get_cvrg_rec)
      EXTERNAL  get_dar_rec !$pragma c(get_dar_rec)
      EXTERNAL  get_dtk_rec !$pragma c(get_dtk_rec)
      EXTERNAL  get_seg_rec !$pragma c(get_seg_rec)
      EXTERNAL  get_site_rec !$pragma c(get_site_rec)
      EXTERNAL  get_sscvrg_rec !$pragma c(get_sscvrg_rec)
      EXTERNAL  get_satsensor_rec !$pragma c(get_satsensor_rec)
      EXTERNAL  get_no_elements !$pragma c(get_no_elements)

      INTEGER*4 llist
      POINTER(llistptr, llist)

      INTEGER*4 ptr
      POINTER(ptrptr, ptr)

      INTEGER*4 DATA_RECORD(0:60)
      POINTER(P2_DATA_RECORD, DATA_RECORD)

      CHARACTER*1024 WHERE_CLAUSE

      INTEGER*4 MAPR_DBPROC
      COMMON  /MAPR_DBPROC/MAPR_DBPROC

      INTEGER*4  ALL_COLS
      DATA       ALL_COLS/
     *           -1/
 

