/* getLatLong: part of JPL's earthloc software.	
	Translated to C by Orion Lawlor, ASF, 6/97

subroutine getlocc(statevec,range,fd,re,rp,xlambda, --------------------
c    .iyear,iday,ihour,min,sec,
c    .targlat,targlon,targrad,istat)
c
c
c   THIS ROUTINE COMBINES CALLS TO THE FOLLOWING: makeMatrix,getloc, AND utc2gha_.
c   C callable version  
c
c   Input:      statevec:  double precision x,y,z,xdot,ydot,zdot of s/c
c		   range:  Known slant range to target (double)
c		      fd:  Known Doppler frequency between s/c and stationary
c     			   target on Earth's (ellipsoidal) surface (double)
c		      re:  equatorial radius (double)
c		      rp:  polar radius (double)
c		 xlambda:  wavelength (double)
c		   iyear:  year of statevec (integer)
c		    iday:  day of statevec (integer)
c		   ihour:  hour of statevec (integer)
c		     min:  minute of statevec (integer)
c		     sec:  second of statevec (float)
c   Output:      
c		 targlat:  geodetic latitude of target (double degrees)
c		 targlon:  geocentric longitude+hourangle of target (double degrees)
c		 targrad:  radius of ellipsoidal earth at target(double meters)       
c		   istat:  =0 success, = -1 error occured
c
c   UNITS:      Distance dimensions are all meters
c  	        Angles are all radians
c		Frequencies are all Hz
c
c	NOTE: See getloc.f for complete comments
c
c   
c   COMMON BLOCKS:   USED to pass to calling routines, not from/to main
c	                earth:   contains the polar (rp) and equatorial (re)
c				 radius of the Earth as given by calling
c				 program.               
c			  sar:   contains wavelenth of radar
c*/

#include <assert.h>
#include "asf.h"
#include "earthloc.h"
#include "asf_meta.h"

/*
	MakeMatrix:
Constructs 3x3 matrix which rotates spacecraft-
centered coordinates to earth-centered coordinates.
*/

void makeMatrix(GEOLOCATE_REC *g)
{
	vector v,ax,ay,az;
	az=g->stVec.pos;
	v=g->stVec.vel;
	vecNormalize(&az);
	vecNormalize(&v);
	vecCross(az,v,&ay);
	vecCross(ay,az,&ax);
	g->look_matrix[0] = ax;
	g->look_matrix[1] = ay;
	g->look_matrix[2] = az;
}



GEOLOCATE_REC *init_geolocate(const stateVector *stVec)
{
	GEOLOCATE_REC *g=(GEOLOCATE_REC *)MALLOC(sizeof(geolocate_rec));
	g->stVec=*stVec;
	makeMatrix(g);
	
	g->lambda=0.0565646;
	g->side='R';
	
	g->dayLength=24.0*60.0*60.0;
	g->angularVelocity=(366.225/365.225)*2*PI/g->dayLength;
	g->gxMe=3.986005e14; /*Gravitational constant times mass of Earth (g times Me, or gxMe) */
	g->re=6378137.0000; /*Equatorial Radius (m) WGS-84*/
	g->rp=g->re-g->re/298.257223563; 
		/*Polar Radius (m) WGS-84*/
	return g;
}

GEOLOCATE_REC * init_geolocate_meta(const stateVector *stVec,meta_parameters *meta)
{
	GEOLOCATE_REC *g=init_geolocate(stVec);
	g->lambda=meta->sar->wavelength;
	g->side=meta->sar->look_direction;
	if (meta->sar->image_type=='P')/*Image is map projected-- get earth radii from there*/
	{
		g->re=meta->projection->re_major;
		g->rp=meta->projection->re_minor;
	}
	return g;
}

void free_geolocate(GEOLOCATE_REC *g)
{
	g->lambda=-99.0;
	g->side='X';
	g->re=g->rp=-98.0;
	FREE((void *)g);
}

void getLatLongMeta(const stateVector stVec,meta_parameters *meta,
	double range,double doppler,double elev,
	double *targLat, double *targLon, double *targRadius)
{
  // No effort has been made to make this routine work with
  // pseudoprojected images.
  assert (meta->projection == NULL
	  || meta->projection->type != LAT_LONG_PSEUDO_PROJECTION);

	double lat,lon;
	
/*Set up globals.*/
	GEOLOCATE_REC *g=init_geolocate_meta(&stVec,meta);
	g->re+=elev;
	g->rp+=elev;

/*Refine look and yaw angles over spheroid to correct doppler*/
	getLoc(g,range,doppler,&lat,&lon,targRadius);

/*Convert longitude to (-pi, pi].*/
	if (lon < -pi)
		lon += 2*pi;
/*Convert latitude to geodetic, from geocentric.*/
	lat=atan(tan(lat)*(g->re/g->rp)*(g->re/g->rp));
	*targLon = lon*R2D;
	*targLat = lat*R2D;
	
	g->re-=elev;
	g->rp-=elev;
	free_geolocate(g);
}








