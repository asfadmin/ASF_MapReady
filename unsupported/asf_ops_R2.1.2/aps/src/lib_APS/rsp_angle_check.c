#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/********************************************************************
*  Function rsp_angle_check
*  Purpose: check rsp_angle against valid range [1-659]
*  Input Parameters:
*	Name		Type    Definition
*	rsp_angle	double	J1 RSP angle value [0.00-360.00]
*  Output Parameters:
*	returned value 
*	of function	int	 0 = Input RSP angle value is O.K.
*				 1 = Input RSP angle value is too large
*				-1 = Input RSP angle value is too small
*  $Date$ $Revision$ $Author$
*********************************************************************/
#pragma ident	"@(#)rsp_angle_check.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.rsp_angle_check.c"

int rsp_angle_check ( 
	double 	rsp_angle )
{
if ( rsp_angle > 360.00000 ) 
{	
	printf(" ERROR:  RSP ANGLE VALUE %.2f is too large.\n",rsp_angle);
	return  1 ;
}
if ( rsp_angle <   0.00000 )
{	
	printf(" ERROR:  RSP ANGLE VALUE %.2f is too small.\n",rsp_angle);
	return -1 ;
}
return 0 ;
}
