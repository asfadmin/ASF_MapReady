C--  Copyright (c)1996, California Institute of Technology.
C--  U.S. Government Sponsorship acknowledged.
C-- ==========================================================================
C--
C--  Fortran Filename:  et2ut1.for
C--
C--  Description:
C--
C--  Notes:
C--
C-- ==========================================================================

      SUBROUTINE ET2UT1 (ET,UT1)
********************************************************************
*  Name: ET2UT1
*  Module Type: SUBROUTINE Language: FORTRAN 77
*  $Logfile:   ACS003:[BLD.MPS.GHA.SRC]ET2UT1.FOV  $
*  Purpose: CHANGE EPHEMERIS TIME TO ASF TIME.  
*  Subroutines called:
*  Input Parameters:
*  ET		REAL*8	THEN EPHEMERIS TIME IN JULIAN DATE.
*  Name         Type    Definition
*  Output Parameters:
*  UT1		REAL*8	UT1
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*********************************************************************/
      character*100 SccsFileID
     -/'@(#)et2ut1.for	5.1 98/01/08 APS/ASF\0'/
 

      IMPLICIT NONE
      REAL*8 ET, UT1, ETMTIM, XSEC
      XSEC = ETMTIM(ET,'UT1')
      UT1 = ET - XSEC/3600.0D0/24.0D0
      RETURN
      END
