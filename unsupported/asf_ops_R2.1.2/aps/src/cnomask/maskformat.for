C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  Maskformat.for
C--
C--  Description:   
C-- 
C--  Notes:
C--
C-- ==========================================================================
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
c
c
c ****************************************************************************
c  NOTE:  this program is a big kluge.  The original program wrote
c         both the sensor and the maskinout data to the database.
c         This program is supposed to write only the maskinout data.
c         So this code uses the original coverage program, puts in
c         dummy data for the sensor, which is not needed.  The sensor 
c         is here only because there are fewer changes that need to be 
c         made in the original program to get it to write the maskinout 
c         records.   The code changes suppress the database
c         writes into the coverage relation, while keeping the
c         database writes into the maskinout relation.
c         This is a fast, reliable way to make this program.
c         Also, we are in a hurry, and this is NOT the only thing going
c         on this week:
c
c  Greetings,
c
c  I have updated the /home/aps/r2.1/etc/NOTES/TO_DO/TO_DO_LIST to include
c  things that need to be done before we can deliver APS for R2.1.
c
c  I would prefer to have these all completed one week from Thursday (tomorrow)
c  if possible. All the DL2DTK and statistics items should be done by the end
c  of this week so that Douglas can proceed with subsystem testing early next
c  week.
c
c  Hope that you can all put in some overtime this week to accomplish the
c  above goal.
c
c  Thanks.
c
c  quentin
c
c  Anyway, the resulting program may not make sense to you unless you
c  understand the situation at the time of creating the program.
c  If this looks like a big kluge to you, it is.  It is just expedient
c  at the moment.  Thu May  1 10:29:33 PDT 1997
c
c
c  Name: MaskFormat
c  Module Type: Subroutine  Language: FORTRAN
c  Purpose: 
c  Takes previously calculated state vector info and formats it for output
c   the input also contains sensor coverage on the earth 
c  Input Parameters:
c  Name     Type        Description
c  dbproc   *DBPROC     address of structure with sybase info in it.
c  ACT      CHAR*(*)    'IN'    ENTER MASK RECORD
C               'OUT'   EXIT MASK RECORD
C               '-' NEITHER ENTER OR EXIT.
c  x        real*8(12)  state vector
c  xnlat,xnlon  Real*8      lat/lons of sensor swath
c  xflat,xflon
c  se       real*8(3)   Cartesian coords of the center point on the 
c               earth of the satellite swath
C  marker   INT     Number corresponding to this coverage run.
C  nodeflag CH*1        D = descending node
c               N = ascending node
C  amask    CH*1        ASF = inside the asf mask
C  sat      CH*2        Satellite:  E1, J1, RS etc.
C  sensor   CH*3        SAR or OPS etc.
c  Output Parameters:
c  Name     Type        Description
C  NRECSM   INT     NUMBER OF maskinout RECORDS APPENDED.
c  Modification History:
c  Author   Revision    Date
c  HERB     1.0     17 Apr 1989 14:52:00 
c  CRAIG    1.1     02 May 1989 13:24:42
c  $Author$ $Revision$ $Date$
c
c ****************************************************************************
      SUBROUTINE MaskFormat(dbproc, ACT, x, amask, sat, NRECSM )

      character*100 SccsFileID
     -/'@(#)maskformat.for	5.1 98/01/08 APS/ASF\0'/

c
      IMPLICIT NONE

      integer*4 dbproc
      external insert_maskinout !$pragma C(insert_maskinout)
      CHARACTER*(*) ACT
      REAL*8 x(12)
c
      CHARACTER*21 asftime
      CHARACTER*(*) amask, sat
      INTEGER irev, NRECSM
      INTEGER IER
      DOUBLE PRECISION hite
      DOUBLE PRECISION clatd, jtime
      INTEGER nrecs
      REAL*8 glatd, glond, rr, trgf
      COMMON /COM_GDLAT/ glatd, glond, clatd, rr(3), hite, trgf(6)
C---      INCLUDE 'MPS_LIB:MPS_CONST_EARTH.FOR'
      INCLUDE 'APS_HOME:include/local/mps_const_earth.inc'
      INCLUDE 'APS_HOME:include/local/timeconv.inc'
c
      NRECSM = 0
c
      irev = x(8) + 0.5d0
      jtime = x(7)
C---      CALL ET2ASF(jtime,asftime)
      IER = tc_et2asf(%VAL(jtime), asftime)

      IF(ACT .NE. '-') THEN
C--     write the mask entry or exit record to the db.  check for mask.
        IF( amask(1:1) .EQ. 'A') 
     ?     call insert_maskinout(%VAL(dbproc), 
     ?                          'ASF', sat, irev, jtime, ACT, nrecs)
        IF( amask(1:1) .EQ. 'M') 
     ?     call insert_maskinout(%VAL(dbproc), 
     ?                          'MCM', sat, irev, jtime, ACT, nrecs)
        NRECSM = NRECSM + nrecs
      ENDIF

 9999 CONTINUE
      RETURN
      END
