#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/********************************************************************
*  Name:        get_dar_sscv
*  Module Type: SUBROUTINE      Language: C
*  Purpose:     GIVEN A DARID, gets dar data subset for specific site
*		coverage.
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  dbproc	*DBPROCESS pointer to info about syabase database session.
*  DARID        *int    darid which identifies the desired dar.
*  Output Parameters:
*       --  fields froam the dar relation  -----
*  SHAPE	*char 1	shape of dar from dar relation.  
*			P if a point/radius, Q if quadrilataral, R if rectangle
*  RADIUS	*double	radius from dar relation.  
*			if point/radius, this is the radius in km.  otherwise=0
*  NWLAT	*double	if point/radius, the center.  otherwise, the NW corner
*  NWLON	*double	
*
*  NELAT	*double	if point/radius, 0.  otherwise, the NE corner
*  NELON	*double	
*
*  SELAT	*double	if point/radius, 0.  otherwise, the SE corner
*  SELON	*double	
*
*  SWLAT	*double	if point/radius, 0.  otherwise, the SW corner
*  SWLON	*double	
*
*  IER		*int	return value.  0 if no errors; 1 if an error.  
****************************************************************************/
#pragma ident	"@(#)get_dar_sscv.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/cdtkopps/SCCS/s.get_dar_sscv.c"
 
#include <stdlib.h>
 
void get_dar_sscv(
        int       	*dbproc,
	int			*DARID,
	char		*SITENAME,
	char		*SHAPE,
	double		*RADIUS,
	double		*NWLAT,
	double		*NWLON,
	double		*NELAT,
	double		*NELON,
	double		*SELAT,
	double		*SELON,
	double		*SWLAT,
	double		*SWLON,
	int			*IER    )
	
{

char	buf[100];

char        	userid[16];
char        	reqtime[22];
char        	reqstat[4];
int		prvdarid;
char        	prvreqstat[4];
char        	sat[3];
char        	sensor[4];
char        	strttime[22];
char        	endtime[22];
char        	sitename[33];
char        	shape[2];
float  		radius;
float  		nwlat;
float  		nwlon;
float  		nelat;
float  		nelon;
float  		selat;
float  		selon;
float  		swlat;
float  		swlon;
int	  	nobs;
char        	fobs[16];
int   		rev;
char        	ascdsc[2];
char        	usercmnt[256];
char        	plnrcmnt[256];

printf("get_dar_sscv:  dbproc = %x, darid = %d\n", dbproc, *DARID);

get_dar(dbproc, DARID, userid, reqtime, reqstat, &prvdarid, prvreqstat, 
	sat, sensor, strttime, endtime, sitename, shape, &radius, 
	&nwlat, &nwlon, 
	&nelat, &nelon, 
	&selat, &selon, 
	&swlat, &swlon, 
	&nobs, fobs, &rev, ascdsc, usercmnt, plnrcmnt, IER);

if(*IER == 0)
{
	printf("get_dar_sscv.c:  shape 	  = %s\n", shape);
	printf("                 sitename = %s\n", sitename);
	printf("                 radius   = %f\n", radius);
	printf("                 nwlat 	  = %f\n", nwlat);
	printf("                 nwlon 	  = %f\n", nwlon);
	printf("                 nelat 	  = %f\n", nelat);
	printf("                 nelon 	  = %f\n", nelon);
	printf("                 selat 	  = %f\n", selat);
	printf("                 selon 	  = %f\n", selon);
	printf("                 swlat 	  = %f\n", swlat);
	printf("                 swlon 	  = %f\n", swlon);
	printf("                 IER 	  = %d\n", *IER);
}

strcpy(SHAPE, "X");
*SHAPE = shape[0];
strcpy(SITENAME, sitename);
*RADIUS = radius;
*NWLAT  = nwlat;
*NWLON  = nwlon;
*NELAT  = nelat;
*NELON  = nelon;
*SELAT  = selat;
*SELON  = selon;
*SWLAT  = swlat;
*SWLON  = swlon;

if(*IER == 0)
{
	printf("get_dar_sscv.c:  returning values:\n");
	printf("                 shape     = %s\n", SHAPE);
	printf("                 sitename  = %s\n", SITENAME);
	printf("                 radius    = %f\n", *RADIUS);
	printf("                 nwlat 	   = %f\n", *NWLAT);
	printf("                 nwlon 	   = %f\n", *NWLON);
	printf("                 nelat 	   = %f\n", *NELAT);
	printf("                 nelon 	   = %f\n", *NELON);
	printf("                 selat 	   = %f\n", *SELAT);
	printf("                 selon 	   = %f\n", *SELON);
	printf("                 swlat 	   = %f\n", *SWLAT);
	printf("                 swlon 	   = %f\n", *SWLON);
}
else
{
	printf("get_dar_sscv.c:  ERROR CODE = %d\n", *IER);
}

return;

}
