#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/*==============================================================================
Filename:		sscvcheckq.c

Description:	

External Functions Defined:
	
File Scope Functions:
	
External Variables Defined:
	
File Scope Variables:
	
Notes:			

==============================================================================*/
#pragma ident	"@(#)sscvcheckq.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/gui/SCCS/s.sscvcheckq.c"

/********************************************************************
*  Name:	SSCVCHECKQ
*  Module Type: INT FUNCTION	Language: EQUAL FORTRAN
*  $Logfile:   ACS003:[BLD.MPS.ORBG.SRC]SSCVCHECKQ.QFV  $
*  Purpose:	CHECK QUADRILATERAL/RECTANGLE SITE INPUT DATA.
*  Functions called:
*  VECTOR LIBRARY:  SPHERE
*  Input Parameters:
*  Name         Type    Definition
*  xlat1	REAL*8	LATITUDE IN DEGREES OF POINT 1
*  xlon1	REAL*8	LONGITUDE IN DEGREES OF POINT 1
*  xlat2	REAL*8	LONGITUDE IN DEGREES OF POINT 2
*    ..		..	..		.		
*    ..		..	..		.		
*    ..		..	..		.		
*  Output Parameters:
*  Name         Type    Definition
*  SSCVCHECKQ	INTEGER	0 IF NO ERRORS, 1 OTHERWISE.  
*  Variables:
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*                                                                   
*********************************************************************/
#include <stdio.h>
#include <Xm/Xm.h>

#include "mpsf77calls.h"

extern void		popup_message() ;
extern char		display_string[] ;

int sscvcheckq(double xlat1, double xlon1, 
	double xlat2, double xlon2, 
	double xlat3, double xlon3, 
	double xlat4, double xlon4)
{
	int j, iflag ;
	double angld ;
	double xll[2] ;
	double q1[3], q2[3], q3[3], q4[3], xlat[4], xlon[4] ;

	xlat[0] = xlat1 ;	xlon[0] = xlon1 ;
	xlat[1] = xlat2 ;	xlon[1] = xlon2 ;
	xlat[2] = xlat3 ;	xlon[2] = xlon3 ;
	xlat[3] = xlat4 ;	xlon[3] = xlon4 ;

	for (j = 0; j < 4; j++)
	{
		/* check latitude */
		if (xlat[j] > 90.00 && xlat[j] < -90.000)
		{
			(void) sprintf( display_string,
				"Error in Latitude '%f' \nLatitude Range: -90.00 <= x <= 90.00\n",
				xlat[j]) ;
			popup_message( XmDIALOG_ERROR, "APS:ERROR",
				display_string, XtGrabNone ) ;
			return(1) ;
		}

		/* check longitude */
		if (xlon[j] > 180.00 && xlon[j] < -180.000)
		{
			(void) sprintf( display_string,
				"Error in Longitude '%f' \nLongitude Range: -180.00 <= x <= 180.00\n",
				xlon[j]) ;
			popup_message( XmDIALOG_ERROR, "APS:ERROR",
				display_string, XtGrabNone ) ;
			return(1) ;
		}
	}

	xll[0] = xlat1 ; xll[1] = xlon1 ;
	ll2xyz_(xll, q1) ;

	xll[0] = xlat2 ; xll[1] = xlon2 ;
	ll2xyz_(xll, q2) ;

	xll[0] = xlat3 ; xll[1] = xlon3 ;
	ll2xyz_(xll, q3) ;

	xll[0] = xlat4 ; xll[1] = xlon4 ;
	ll2xyz_(xll, q4) ;

	gclose_(&iflag, q1, q2) ;
	if (iflag == 1)
	{
		(void) sprintf( display_string,
			"Error in quadrilateral site...\nPoints 1 and 2 are too close\n") ;
		popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
		return(1) ;
	}
	else if (iflag == -1)
	{
		(void) sprintf( display_string,
			"Error in quadrilateral site...\nPoints 1 and 2 are opposite\nThis makes an undefined border..." ) ;
		popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
		return(1) ;
	}

	gclose_(&iflag, q2, q3) ;
	if (iflag == 1)
	{
		(void) sprintf( display_string,
			"Error in quadrilateral site...\nPoints 2 and 3 are too close") ;
		popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
		return(1) ;
	}
	else if (iflag == -1)
	{
		(void) sprintf( display_string,
			"Error in quadrilateral site...\nPoints 2 and 3 are opposite\nThis makes an undefined border..." ) ;
		popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
		return(1) ;
	}

	gclose_(&iflag, q3, q4) ;
	if (iflag == 1)
	{
		(void) sprintf( display_string,
			"Error in quadrilateral site...\nPoints 3 and 4 are too close") ;
		popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
		return(1) ;
	}
	else if (iflag == -1)
	{
		(void) sprintf( display_string,
			"Error in quadrilateral site...\nPoints 3 and 4 are opposite\nThis makes an undefined border..." ) ;
		popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
		return(1) ;
	}

	gclose_(&iflag, q4, q1) ;
	if (iflag == 1)
	{
		(void) sprintf( display_string,
			"Error in quadrilateral site...\nPoints 4 and 1 are too close") ;
		popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
		return(1) ;
	}
	else if (iflag == -1)
	{
		(void) sprintf( display_string,
			"Error in quadrilateral site...\nPoints 4 and 1 are opposite \nThis makes an undefined border..." ) ;
		popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
		return(1) ;
	}

	sangld_(&angld, q4, q1, q2) ;
#ifdef DEBUG
	(void) printf("ANGLE %f\n", angld) ;
#endif

	if (angld <= 0.0)	/* if interior angle < 0 */
	{
		(void) sprintf( display_string,
			"Point number 1: angle %9.4f\nERROR: Quadrilateral Site interior angles\n must be 0 < x < 180",
			angld ) ;
		popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
		return(1) ;
	}

	sangld_(&angld, q1, q2, q3) ;
#ifdef DEBUG
	(void) printf("ANGLE %f\n", angld) ;
#endif

	if (angld <= (double) 0.0)	/* if interior angle > 0 */
	{
		(void) sprintf( display_string,
			"Point number 2: angle %9.4f\nERROR: Quadrilateral Site interior angles\n must be\n' 0 < x < 180",
			angld ) ;
		popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
		return(1) ;
	}
	
	sangld_(&angld, q2, q3, q4) ;
	if (angld <= 0.0)	/* if interior angle > 0 */
	{
		(void) sprintf( display_string,
			"Point number 3: angle %9.4f\nERROR: Quadrilateral Site interior angles\n must be\n' 0 < x < 180",
			angld ) ;
		popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
		return(1) ;
	}
	
	sangld_(&angld, q1, q2, q3) ;
	if (angld <= 0.0)	/* if interior angle > 0 */
	{
		(void) sprintf( display_string,
			"Point number 4: angle %9.4f\nERROR: Quadrilateral Site interior angles\n must be\n' 0 < x < 180\n",
			angld ) ;
		popup_message( XmDIALOG_ERROR, "APS:ERROR",
			display_string, XtGrabNone ) ;
		return(1) ;
	}
	return(0) ;
}
