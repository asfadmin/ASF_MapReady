/*******************************************************************************
NAME 
	Geograph

PURPOSE:	Transforms input longitude and latitude to Easting and
		Northing for the geographic projection.  The lats &
                lons are radians; the easting and northing are 
                arc-seconds.
  
PROGRAMMER              DATE            
----------              ----           
O. Lawlor (ASF)         1/1999

ALGORITHM REFERENCES

1.  "New Equal-Area Map Projections for Noncircular Regions", John P. Snyder,
    The American Cartographer, Vol 15, No. 4, October 1988, pp. 341-355.

2.  Snyder, John P., "Map Projections--A Working Manual", U.S. Geological
    Survey Professional Paper 1395 (Supersedes USGS Bulletin 1532), United
    State Government Printing Office, Washington D.C., 1987.

3.  "Software Documentation for GCTP General Cartographic Transformation
    Package", U.S. Geological Survey National Mapping Division, May 1982.
*******************************************************************************/
#include "cproj.h"

/* Forward equations--mapping lat,long to x,y
  ------------------------------------------------------------*/
int geographfor(double lon, double lat, double *x, double *y)
{
*x=lon*R2D*3600.0;
*y=lat*R2D*3600.0;
return(OK);
}
/* Inverse equations--mapping x,y to lat,long
  ------------------------------------------------------------*/
int geographinv(double x, double y, double *lon, double *lat)
{
*lon=x/(R2D*3600.0);
*lat=y/(R2D*3600.0);
return(OK);
}

