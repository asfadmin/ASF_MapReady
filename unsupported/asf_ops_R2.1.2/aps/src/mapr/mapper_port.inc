C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:	mapper_port.inc
C--
C--  Description:	
C--	
C--  Notes:
C--
C--   character*100 SccsFileID
C--  -/'@(#)mapper_port.inc	5.1 98/01/08 APS/ASF\0'/
C-- 
C-- ==========================================================================

C     Prior GKS

      INTEGER*4  GLA75
      DATA       GLA75/
     *           101/

      INTEGER*4  GWSWIS
      DATA       GWSWIS/
     *           1/
      
      INTEGER*4  GHP755
      DATA       GHP755/
     *           7550/
      
      INTEGER*4  GMOTIF
      DATA       GMOTIF/
     *           1100/
      
      INTEGER*4  GGCCP,GATOPN,GGAC2P
      DATA       GGCCP,GATOPN,GGAC2P/
     *               -200,    1,    -202/

      REAL  	 DEF_HEIGHT
      COMMON     /DEF_HEIGHT/DEF_HEIGHT

      INTEGER*4 TESTMODE
      COMMON  /TESTMODE/TESTMODE

      CHARACTER*80 DEF_START_TIME
      COMMON  /DEF_START_TIME/DEF_START_TIME
 
      CHARACTER*80 WINDOW_NAME
      COMMON  /WINDOW_NAME/WINDOW_NAME

C     PRIOR GKS _ Workstation Description Manual p.9-3 
      INTEGER    TEXT_FONT
      COMMON     /TEXT_FONT/TEXT_FONT

      REAL    MINLAT_PREV
      COMMON  /MINLAT_PREV/MINLAT_PREV
      REAL    MINLON_PREV
      COMMON  /MINLON_PREV/MINLON_PREV
      REAL    MAXLAT_PREV
      COMMON  /MAXLAT_PREV/MAXLAT_PREV
      REAL    MAXLON_PREV
      COMMON  /MAXLON_PREV/MAXLON_PREV
 

