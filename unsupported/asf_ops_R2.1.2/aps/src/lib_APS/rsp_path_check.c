#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/********************************************************************
*  Function rsp_path_check
*  Purpose: check rsp_path against valid range [1-659]
*  Input Parameters:
*	Name		Type    Definition
*	rsp_path	int	J1 RSP path number [1-659]
*  Output Parameters:
*	returned value 
*	of function	int	 0 = Input RSP value is O.K.
*				 1 = Input RSP value is too large
*				-1 = Input RSP value is too small
*  $Date$ $Revision$ $Author$
*********************************************************************/
#pragma ident	"@(#)rsp_path_check.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.rsp_path_check.c"

int rsp_path_check ( 
	int 	rsp_path )
{
if ( rsp_path > 659 ) 
{	
	printf ( " ERROR:  RSP PATH VALUE %d is too large.\n",rsp_path);
	return  1 ;
}
if ( rsp_path < 1   )
{	
	printf ( " ERROR:  RSP PATH VALUE %d is too small.\n",rsp_path);
	return -1 ;
}
return 0 ;
}
