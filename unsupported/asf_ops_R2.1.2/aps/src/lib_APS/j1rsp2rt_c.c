#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/********************************************************************
*  Name:	J1RSP2RT_C.C
*  Module Type: INT function 	Language: C
*  Purpose:	GIVEN A DATE AND J1 RSP PATH/ANGLE, COMPUTE THE REV NUMBER
*		AND TIME.  
*		Calling shell (wrapper) for J1RSP2RT.FOR
*  Input Parameters:
*  Name         Type    Definition
*  ASFTIME	CH*21	Time in ASF-SIS format
*  ET0		REAL*8	Time in Ephemeris time julian days
*  IFRWRD	INT	FLAG:  
*			= +1	LOOKING FROM THE INPUT TIME 24 HOURS FORWARD.
*			= -1	LOOKING FROM THE INPUT TIME 24 HOURS BACKWARD.
*  IRSP		INT	RSP PATH; BETWEEN 1 AND ICREVS.  
*  XANGL	REAL*8	RSP ANGLE; BETWEEN 0 AND 360.
*  Output Parameters:
*  IREV		INT	REV NUMBER CORRESPONDING TO THE RSP.
*  ASFTOUT	CH*21	ASF TIME CORRESPONDING TO THE RSP PATH/ANGLE.
*
*  [return variable]
*  IER		INT	ERROR CODE:
*			-3= INPUT ERROR; XANGL. SHOULD BE BETWEEN 0 AND 360.
*			-2= INPUT ERROR; BAD ASF TIME.  
*			-1= INPUT ERROR; THE RSP IS NOT WITHIN [1-CYREVS]
*  			0 = NORMAL CONDITION
*  			1 = THE RSP DOES NOT OCCUR WITHIN 1 DAY OF THE INPUT
*			    TIME.  NO REV OR TIME IS COMPUTED.  
*			2 = THE INPUT TIME DOES NOT FALL WITHIN A PHASE.
*			3 = NO RECORDS WERE FOUND IN THE PHASE RELATION.
*  Variables:	CYREVS = THE NUMBER OF REVS IN ONE REPEAT CYCLE.
*  Locals :
*  Externals :
*  Modification History:                                            
*  Date			Revision	Author
*  $Date$ $Revision$ $Author$
*********************************************************************/
#pragma ident	"@(#)j1rsp2rt_c.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/lib_APS/SCCS/s.j1rsp2rt_c.c"

/*
	# include descrip
	# include string
	# include stdio
 */

extern j1rsp2rt_(void*, 
	char*, double*, int*, int*, double*, int*, char*, int*) ;

int j1rsp2rt_c(
	void	*dbproc,	/* pointer to a Sybase process.  but don't want Sybase 
						/* includes to have to go into this routine.  	*/
						/* it really is DBPROCESS*						*/
	char 	*asftime,
	double 	et0,
	int 	ifrwrd,
	int 	irsp,
	double 	xangl,
	int 	*irev,		/* output rev number for asftime, irsp inputs.		*/
	char 	*asftout )	/* output ASF time for asftime, irsp, xangl inputs	*/
{

int		j;
int 	ier;
char	asft_in[22] = "1990:365:23:59:59.999" ; /* initialize to show format */

strcpy(asft_in, asftime);

/* FORTRAN CALL 		*/
/* if there is a value we want to send there, give the address of it:  */
j1rsp2rt_(&dbproc, asftime,&et0,&ifrwrd,&irsp,&xangl, irev,asftout,&ier) ;
if ( ier == 2) 
{
	/* the time was not within a phase.  try again with 		*/
	/* the last second in the same day and looking backwards 	*/
	/* from it 24 hours in the search.  						*/
	strcpy ( asft_in+9, "23:59:59.999" ) ;
	/* FORTRAN CALL 		*/
	j = - ifrwrd;
	/* j1rsp2rt(&asftime_desc,&et0,&j,&irsp,&xangl,irev,&asftout_desc,&ier);*/
	j1rsp2rt_(dbproc, asftime,&et0,&ifrwrd,&irsp,&xangl, irev,asftout,&ier) ;
}

/*  terminate the string:		*/
*(asftout+21) = '\0';

if ( ier == 3) 
{
	printf ( "\nNO RECORDS WERE FOUND IN THE PHASE RELATION.\n" ) ;
	printf ( "NO RECORDS WERE FOUND IN THE PHASE RELATION.\n" ) ;
	printf ( "NO RECORDS WERE FOUND IN THE PHASE RELATION.\n\n" ) ;
}
return ier ;

}
