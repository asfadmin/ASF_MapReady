#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/********************************************************************
*  Name:	J1DRSP2R_C.C
*  Module Type: RUNCTION	Language: C
*  Purpose:	GIVEN A DATE AND J1 RSP, COMPUTE THE REV NUMBER.
*		THIS IS A C SHELL TO CALL THE FORTRAN ROUTINE.   
*  Input Parameters:
*  Name         Type    Definition
*  ASFTIME	*ch	Time in ASF-SIS format.  ONLY THE DATE IS USED.
*			JUST THE FIRST 8 CHARACTERS:  1990:245
*			but ASFTIME can be 21 characters long.  
*  IRSP		INT	RSP
*  Output Parameters:
*  IREV		INT	rev number corresponding to the RSP.
*  [RETURN VALUE] INT	ERROR CODE:
*			-2= INPUT ERROR; BAD ASF TIME.  
*			-1= INPUT ERROR; THE RSP IS NOT WITHIN [1-CYREVS]
*  			0 = NORMAL CONDITION
*  			1 = THE REV DOES NOT OCCUR WITHIN 1 DAY OF THE INPUT
*			    TIME.
*			2 = THE INPUT TIME DOES NOT FALL WITHIN A PHASE.
*			3 = NO RECORDS WERE FOUND IN THE PHASE RELATION.
*  Variables:	CYREVS = THE NUMBER OF REVS IN ONE REPEAT CYCLE.
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*********************************************************************/
#pragma ident	"@(#)j1drsp2r_c.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.j1drsp2r_c.c"

int j1drsp2r_c (
	void	*dbproc,  	/* a pointer to a sybase process		*/
	char 	*asftime ,	/* input time yyyy:ddd:hh:mm:ss.sss 	*/
	int 	irsp ,		/* input J1 RSP	*/
	int 	*irev )		/* output rev number	*/
{
int			istat;
long int	jrsp, jrev, jstat;
extern j1drsp2r_(void*, char*, long int*, long int*, long int*);

/* move to long integers for call	*/
jrsp = irsp;

/* FORTRAN call	*/

j1drsp2r_(&dbproc, asftime, &jrsp, &jrev, &jstat ) ;

*irev = jrev;
istat = jstat;

return istat ;

}
