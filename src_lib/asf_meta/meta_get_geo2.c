/****************************************************************
FUNCTION NAME:  meta_get_*

DESCRIPTION:
   Extract relevant parameters from CEOS.
   These are the external entry points for
   the geolocation-related routines.

RETURN VALUE:
   
SPECIAL CONSIDERATIONS:

PROGRAM HISTORY:
  1.0 - O. Lawlor.  9/10/98.  CEOS Independance.
****************************************************************/
#include "asf.h"
#include "asf_meta.h"

/*Geolocation Calls:*/

/*Internal utilities:*/
typedef struct {
	double lat;
	double lon;
} lat_lon;
double get_distance(lat_lon one,lat_lon two)
{
	double dlat=one.lat-two.lat;
	double dlon=(one.lon-two.lon)*cos(one.lat*PI/180.0);
	return dlat*dlat+dlon*dlon;
}
double get_error(meta_parameters *sar,
	lat_lon target,double elev, double xSamp,double yLine)
{
	lat_lon new;
	meta_get_latLon(sar,yLine,xSamp,elev,&new.lat,&new.lon);
	return get_distance(new,target);
}

/* Converts given latitude and longitude back
to the original line and sample.
*/
void meta_get_lineSamp(meta_parameters *sar,
	double lat, double lon,double elev,double *yLine,double *xSamp)
{
#define DELTA 0.1 /*Number of pixels along which to 
  perform finite difference approximation of the derivative.*/
	double x=0.0,y=0.0;
	double x_old=1000,y_old=1000;
	int iter=0;

	lat_lon target;
	target.lat=lat;target.lon=lon;
	while (fabs(x-x_old)+fabs(y-y_old)>DELTA)
	{
		double cur_err=get_error(sar,target,elev,x,y);
		double del_x=(get_error(sar,target,elev,x+DELTA,y)-cur_err)/DELTA;
		double del_y=(get_error(sar,target,elev,x,y+DELTA)-cur_err)/DELTA;
		double rad=fabs(del_x)+fabs(del_y);

		/*printf("   x=%6.1f; y=%6.1f, err=%.6f\n",x,y,cur_err);
		*/
		x_old=x;y_old=y;
		x=x-(fabs(del_x)/rad)*cur_err/del_x;
		y=y-(fabs(del_y)/rad)*cur_err/del_y;
		iter++;
	}
	/*printf("  %d iterations\n",iter);*/
 
	*yLine=y-DELTA/2;
	*xSamp=x-DELTA/2;
}
