#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/********************************************************************
*  Name:	J1RT2RSP_C.C
*  Module Type: INT function 	Language: C
*  $Logfile:   ACS003:[BLD.MPS.LIB.SRC]J1RT2RSP_C.C_V  $
*  Purpose:	GIVEN A J-ERS-1 REV AND START/STOP TIMES, COMPUTE 
*		THE J1 RSP PATH AND START/STOP ANGLES AND TIMES.  
*		this is a shell (wrapper) for J1RT2RSP.FOR
*  Input Parameters:
*  Name         Type    Definition
*  dbproc	int*4		pointer to sybase process info.  
*  IREV		INT	INPUT REV NUMBER 
*  ASFT1	CH*21	ASF FORMAT START TIME.  
*  ASFT2	CH*21	ASF FORMAT STOP TIME.  
*  Output Parameters:
*  IRSP		INT 	RSP PATH NUMBER CORRESPONDING TO IREV.
*  XANGL1	REAL*8	RSP ANGLE CORRESPONDING TO ASFT1
*  XANGL2	REAL*8	RSP ANGLE CORRESPONDING TO ASFT2
*
*  [return variable]
*  IER		INT	ERROR CODE:
*			-2= INPUT ERROR; BAD REV.
*			-1= INPUT ERROR; BAD ASF TIME.  
*  			0 = NORMAL CONDITION
*  			1 = 1ST INPUT TIME DOES NOT FALL WITHIN THE GIVEN REV.
*			2 = THE INPUT REV DOES NOT FALL WITHIN A PHASE.
*			3 = NO RECORDS WERE FOUND IN THE PHASE RELATION.
*  			11= 2ND INPUT TIME DOES NOT FALL WITHIN THE GIVEN REV.
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
**********************************************************************/
#pragma ident	"@(#)j1rt2rsp_c.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.j1rt2rsp_c.c"


/*
	# include descrip
	# include stdio
	# include string
*/
#include <string.h>
#include <stdio.h>

int j1rt2rsp_c(
	void 	*dbproc,
	int 	irev,		/* input rev number	*/
	char 	*asft1,		/* input start time	*/
	char 	*asft2,		/* input end time	*/
	int 	*irsp,		/* output j1 RSP path	*/
	double 	*xangl1,	/* output start RSP angle  */
	double 	*xangl2 )	/* output end   RSP angle  */
{

extern j1rt2rsp_(void*, int*, char*, char*, int*, double*, double*, int*);
int ier ;
char asft_start[22] = { "1990:123:12:12:12.000" } ;
char asft_end[22]   = { "1990:123:12:12:12.000" } ;

/* FORTRAN CALL 	*/
/*
printf("j1rt2rsp_c.c:  dbproc = %x, irev = %d, asft1 = %s, asft2 = %s\n",
	dbproc, irev, asft1, asft2);
 */

j1rt2rsp_(&dbproc, &irev,asft1,asft2, irsp,xangl1,xangl2,&ier ) ;

return ier ;

}
