#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/****************************************************************************
*  Function rsp_check
*  Purpose: check rsp bracket against valid ranges.  
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]RSP_CHECK.C_V  $
*  Input Parameters:
*	Name		Type    Definition
*   fp			FILE*	pointer to output file.  
*	rsp_path_1	int	start J1 RSP path number [1-659]
*	rsp_angle_1	double	start J1 RSP angle [  0.00 - 360.00]
*	rsp_path_2	int	end J1 RSP path number [1-659]
*	rsp_angle_2	double	end J1 RSP angle [  0.00 - 360.00]
*  Output Parameters:
*	returned value 
*	of function	int	 0 = Input RSP bracket is O.K.
*				 1 = error in input RSP bracket.  
*  $Date$ $Revision$ $Author$
****************************************************************************/
#pragma ident	"@(#)rsp_check.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.rsp_check.c"

#include <stdio.h>

int rsp_check ( 
	FILE *fp,
	int rsp_path_1,
	double rsp_angle_1,
	int rsp_path_2,
	double rsp_angle_2 )
{
int n_err ;
n_err = 0 ;
if ( rsp_path_check ( rsp_path_1 ) != 0 ) n_err ++ ;
if ( rsp_path_check ( rsp_path_2 ) != 0 ) n_err ++ ;
if ( rsp_path_1 != rsp_path_2 )
{	
	fprintf (fp, " ERROR:  RSP PATHS ARE NOT EQUAL.  START PATH = %03.3d\n", 
	rsp_path_1 ) ;
	fprintf (fp, "                                     END PATH = %03.3d\n", 
	rsp_path_2 ) ;
	n_err ++ ;
}
if ( rsp_angle_check ( rsp_angle_1 ) != 0 ) n_err ++ ;
if ( rsp_angle_check ( rsp_angle_2 ) != 0 ) n_err ++ ;
return n_err ;
}
