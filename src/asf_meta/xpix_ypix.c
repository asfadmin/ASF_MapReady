#include "asf.h"
#include "geolocate.h"

#ifndef pi
#define pi M_PI
#endif

static double er_target;

static vector latLon2cart(GEOLOCATE_REC *g,double elev,
			  double deg_lat,double deg_lon)
{
/*Convert latitude and longitude to radians */
	double lat=deg_lat*D2R;
	double lon=deg_lon*D2R;
	/* e2==Earth eccentricity, squared */
	double e2=1.0-(g->rp*g->rp)/(g->re*g->re);
	double cosLat=cos(lat), sinLat=sin(lat);
	/* "prime vertical radius of curvature" at our latitude */
	double N=g->re/sqrt(1-e2*sinLat*sinLat);
	vector ret;
	/* Cartesian to spherical, in a coordinate system stretched along Z by (1-e2) */
	ret.x=(N+elev)*cosLat*cos(lon);
	ret.y=(N+elev)*cosLat*sin(lon);
	ret.z=(N*(1-e2)+elev)*sinLat;
	return ret;
}


/************ SAR Geolocation Algorithm ************/

static vector getLocCart(GEOLOCATE_REC *g,double range,double dop)
{
	double yaw=0,look=0;
	vector target;
	getLookYaw(g,range,dop,&look,&yaw);
	getDoppler(g,look,yaw,NULL,NULL,&target,NULL);
	return target;
}

static void getLatLon(GEOLOCATE_REC *g,double range,double dop,  /*  Inputs.*/
		      double *out_lat,double *out_lon,double *earthRadius) /*Outputs.*/
{
	double lat,lon;
	cart2sph(getLocCart(g,range,dop),earthRadius,&lat,&lon);
	
/*Convert longitude to (-pi, pi].*/
	if (lon < -pi)
		lon += 2*pi;
	
/*Convert latitude to geodetic, from geocentric.*/
	lat=atan(tan(lat)*(g->re/g->rp)*(g->re/g->rp));
	if (out_lon) *out_lon = lon*R2D;
	if (out_lat) *out_lat = lat*R2D;
}

static GEOLOCATE_REC * meta_make_geolocate(meta_parameters *meta,
				    double time,double elev)
{
        stateVector st=meta_get_stVec(meta,time);
        GEOLOCATE_REC *g=init_geolocate_meta(&st,meta);
        g->re+=elev;
        g->rp+=elev;
        return g;
}

static double getPixSize(meta_parameters *meta,int axis,int *loc,
			 float elev,float dop) {

	GEOLOCATE_REC *g1, *g2;
	int shift=1.0;
	vector t1,t2,diff;
	dop*=1.0/meta->geo->azPixTime; /* multiply by PRF */
	g1=meta_make_geolocate(meta,meta_get_time(meta,loc[1],loc[0]),elev);
	t1=getLocCart(g1,meta_get_slant(meta,loc[1],loc[0]),meta_get_dop(meta,loc[1],loc[0])+dop);
	loc[axis]+=shift;
	g2=meta_make_geolocate(meta,meta_get_time(meta,loc[1],loc[0]),elev);
	t2=getLocCart(g2,meta_get_slant(meta,loc[1],loc[0]),meta_get_dop(meta,loc[1],loc[0])+dop);
	loc[axis]-=shift;
	er_target=vecMagnitude(t1); /* magnitude of target location==earth radius */
	vecSub(t2,t1,&diff);
	return vecMagnitude(diff)/shift;
}

static double vecCosAng(vector a,vector b)
{
	vecNormalize(&a); vecNormalize(&b);
	return vecDot(a,b);
}

void getPixSizes(meta_parameters *meta, float *range_pixsiz, float *az_pixsiz)
{
  int sar_x = (int) meta->general->sample_count / 2;
  int sar_y=  (int) meta->general->line_count / 2;

  int loc[2];

  meta_get_original_line_sample(meta,sar_y,sar_x, &loc[1],&loc[0]);

  *range_pixsiz = getPixSize(meta,0,loc,0,0);
  *az_pixsiz = getPixSize(meta,1,loc,0,0);
}
