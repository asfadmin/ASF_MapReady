#ifdef COPYRIGHT
Copyright (c)1996, California Institute of Technology.
U.S. Government Sponsorship acknowledged.
#endif

/********************************************************************
*  Name:        get_site_sscv
*  Module Type: SUBROUTINE      Language: C
*  Purpose:     GIVEN A sitename, gets site data for specific site
*		coverage.
*		used for specific site coverage, which works in 
*		REAL*8 but the database is in REAL*4
*  Functions called:
*  Input Parameters:
*  Name         Type    Definition
*  dbproc	*DBPROCESS pointer to info about syabase database session.
*  SITENAME     *char[33]    sitename which identifies the desired site.
*  Output Parameters:
*       --  fields from the site relation  -----
*  SHAPE	*char 1	shape of site from site relation.  
*			P if a point/radius, Q if quadrilataral, R if rectangle
*  RADIUS	*double	radius from site relation.  
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
#pragma ident	"@(#)get_site_sscv.c	5.1 98/01/08 APS/ASF"
#pragma ident	"@(#) /home/aps/r2.1.2/src/cdtkopps/SCCS/s.get_site_sscv.c"
 
#include <stdlib.h>
 
void get_site_sscv(
	int       	*dbproc,
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
	int		*IER    )
	
{

char        shape[2];
float  		radius;
float  		nwlat;
float  		nwlon;
float  		nelat;
float  		nelon;
float  		selat;
float  		selon;
float  		swlat;
float  		swlon;
char        comments[51];

printf("get_site_sscv:  dbproc = %x, sitename = >%s<\n", dbproc, SITENAME);

get_site(dbproc, SITENAME, 
	shape, &radius, 
	&nwlat, &nwlon, 
	&nelat, &nelon, 
	&selat, &selon, 
	&swlat, &swlon, 
	comments, IER);

/*
printf("get_site_sscv.c: shape 	= %s\n", shape);
printf("                 radius = %f\n", radius);
printf("                 nwlat 	= %f\n", nwlat);
printf("                 nwlon 	= %f\n", nwlon);
printf("                 nelat 	= %f\n", nelat);
printf("                 nelon 	= %f\n", nelon);
printf("                 selat 	= %f\n", selat);
printf("                 selon 	= %f\n", selon);
printf("                 swlat 	= %f\n", swlat);
printf("                 swlon 	= %f\n", swlon);
printf("                 IER 	= %d\n", *IER);
*/

strcpy(SHAPE, "X");
*SHAPE = shape[0];
*RADIUS = radius;
*NWLAT  = nwlat;
*NWLON  = nwlon;
*NELAT  = nelat;
*NELON  = nelon;
*SELAT  = selat;
*SELON  = selon;
*SWLAT  = swlat;
*SWLON  = swlon;

printf("get_site_sscv.c:  returning values:\n");
printf("                 shape 	= %s\n", SHAPE);
printf("                 radius = %f\n", *RADIUS);
printf("                 nwlat 	= %f\n", *NWLAT);
printf("                 nwlon 	= %f\n", *NWLON);
printf("                 nelat 	= %f\n", *NELAT);
printf("                 nelon 	= %f\n", *NELON);
printf("                 selat 	= %f\n", *SELAT);
printf("                 selon 	= %f\n", *SELON);
printf("                 swlat 	= %f\n", *SWLAT);
printf("                 swlon 	= %f\n", *SWLON);

return;

}
