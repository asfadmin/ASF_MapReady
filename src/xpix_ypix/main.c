/******************************************************************************
NAME:  xpix_ypix

SYNOPSIS:

   xpix_ypix <meta file>
   
   Measure real ground-range pixel sizes for this image.


EXTERNAL ASSOCIATES:
    NAME:               USAGE:
    ---------------------------------------------------------------

FILE REFERENCES:
    NAME:               USAGE:
    ---------------------------------------------------------------

PROGRAM HISTORY:
    VERS:    DATE:   AUTHOR:
    -----------------------------------------------------------------
    1.0	     2/98    O. Lawlor	For demIFM. Initial development.
    1.01     7/01    R. Gens    Added logfile switch
    1.25     4/02    P. Denny   Standardized commandline parsing & usage()

HARDWARE/SOFTWARE LIMITATIONS:

ALGORITHM DESCRIPTION:

ALGORITHM REFERENCES:

BUGS:

******************************************************************************/
/****************************************************************************
*								            *
*   create_dem_grid creates a grid which can be used to extract a portion   *
*		    of a DEM to fit a given SAR image. 			    *
*   Copyright (C) 2001  ASF Advanced Product Development    	    	    *
*									    *
*   This program is free software; you can redistribute it and/or modify    *
*   it under the terms of the GNU General Public License as published by    *
*   the Free Software Foundation; either version 2 of the License, or       *
*   (at your option) any later version.					    *
*									    *
*   This program is distributed in the hope that it will be useful,	    *
*   but WITHOUT ANY WARRANTY; without even the implied warranty of    	    *
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the   	    *
*   GNU General Public License for more details.  (See the file LICENSE     *
*   included in the asf_tools/ directory).				    *
*									    *
*   You should have received a copy of the GNU General Public License       *
*   along with this program; if not, write to the Free Software		    *
*   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.               *
*									    *
*   ASF Advanced Product Development LAB Contacts:			    *
*	APD E-mail:	apd@asf.alaska.edu 				    *
* 									    *
*	Alaska SAR Facility			APD Web Site:	            *	
*	Geophysical Institute			www.asf.alaska.edu/apd	    *
*       University of Alaska Fairbanks					    *
*	P.O. Box 757320							    *
*	Fairbanks, AK 99775-7320					    *
*									    *
****************************************************************************/

#include "asf.h"
#include "ddr.h"
#include "cproj.h"
#include "proj.h"
#include "asf_meta.h"

double getPixSize(meta_parameters *meta,int axis,int *loc,float elev,float dop) {
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
	vecSub(t2,t1,&diff);
	return vecMagnitude(diff)/shift;
}

double vecCosAng(vector a,vector b)
{
	vecNormalize(&a); vecNormalize(&b);
	return vecDot(a,b);
}


int main(int argc,char *argv[])
{
	int axis;
	const char *sarName;
	meta_parameters *meta;
	struct DDR sar_ddr;
	double elev=0.0;
	int loc[2];

	sarName = argv[1];

	system("date");
	printf("Program: xpix_ypix <.meta and .ddr file>\n\n");

	c_getddr(sarName,&sar_ddr);
	meta=meta_init(sarName);
	
	printf("Along-the-ellipsoid pixel sizes (%d x %d pixel image)\n",sar_ddr.nl,sar_ddr.ns);
	for (axis=0;axis<2;axis++) {
		const char *axisNames[2]={"  range","azimuth"};
		int sar_x=(int)(sar_ddr.ns/2);
		int sar_y=(int)(sar_ddr.nl/2);
		meta_get_orig((void *)&sar_ddr,sar_y,sar_x,&loc[1],&loc[0]);
		
		printf(     "%s pixel size at scene center: %.7f meters\n",
			axisNames[axis], getPixSize(meta,axis,loc,0.0,0.0));
		
		printf("               ...at 1km elevation: %.7f meters\n",
			getPixSize(meta,axis,loc,1.0e3,0.0));
		
		printf("               ...at +1prf doppler: %.7f meters\n",
			getPixSize(meta,axis,loc,1.0e3,1.0));
		
		sar_x=0;
		meta_get_orig((void *)&sar_ddr,sar_y,sar_x,&loc[1],&loc[0]);
		printf("             ...at scene left edge: %.7f meters\n",
			getPixSize(meta,axis,loc,0.0,0.0));

		sar_x=sar_ddr.ns;
		meta_get_orig((void *)&sar_ddr,sar_y,sar_x,&loc[1],&loc[0]);
		printf("            ...at scene right edge: %.7f meters\n",
			getPixSize(meta,axis,loc,0.0,0.0));

		sar_x=0; sar_y=0;
		meta_get_orig((void *)&sar_ddr,sar_y,sar_x,&loc[1],&loc[0]);
		printf("              ...at scene top-left: %.7f meters\n",
			getPixSize(meta,axis,loc,0.0,0.0));
		
		sar_x=sar_ddr.ns; sar_y=sar_ddr.nl;
		meta_get_orig((void *)&sar_ddr,sar_y,sar_x,&loc[1],&loc[0]);
		printf("          ...at scene bottom-right: %.7f meters\n",
			getPixSize(meta,axis,loc,0.0,0.0));
	}
	
	if (1) {
		double azSize, azTime, azVel;
		double t,dt,sc_vel,earth_rad,sc_rad,cos_earth_ang,swath_nr;
		stateVector scFix,scGEI,ts;
		vector target1,target2,targVel; double tv,v;
		printf("Azimuth velocity estimation at topleft:\n");
	/* Use meta routines to find target point at time t and t+dt */
		loc[0]=loc[1]=0;
		t=meta_get_time(meta,loc[1],loc[0]);
		scFix=meta_get_stVec(meta,t);
		target1=getLocCart(  /* body-fixed position of target at time t */
			meta_make_geolocate(meta,t,0),
			meta_get_slant(meta,loc[1],loc[0]),
			meta_get_dop(meta,loc[1],loc[0])
		);
		dt=0.001;
		target2=getLocCart(  /* body-fixed position of target at time t+dt */
			meta_make_geolocate(meta,t+dt,0),
			meta_get_slant(meta,loc[1],loc[0]),
			meta_get_dop(meta,loc[1],loc[0])
		);
		vecSub(target2,target1,&targVel); /* velocity of target point */
		vecScale(&targVel,1.0/dt);
		tv=vecMagnitude(targVel);
		printf("  ASF target azimuth velocity: %.3f m/s\n",tv);
		
	/* Use getPixSize to doublecheck target velocity */
		azSize=getPixSize(meta,1,loc,0.0,0.0);
		azTime=meta->geo->azPixTime;
		azVel=azSize/azTime;
		printf("  xpix_ypix target azimuth velocity: %.3f m/s = %.3f m / %.6f s\n",azVel,azSize,azTime);
	
	/* Find spacecraft vectors and use Tom Bicknell approach */
		scGEI=scFix; fixed2gei(&scGEI,0.0); /* inertial velocities */
		printf("  Orbital velocity: %.3f m/s fixed, %.3f m/s inertial\n",
			vecMagnitude(scFix.vel), vecMagnitude(scGEI.vel));
		sc_vel=vecMagnitude(scGEI.vel); /* GEI velocity */
		sc_rad=vecMagnitude(scGEI.pos); /* distance from center of earth to spacecraft */
		earth_rad=vecMagnitude(target1); /* target earth radius */
		cos_earth_ang=vecCosAng(scFix.pos,target1); /* cosine of target earth angle */
		swath_nr=sc_vel*(earth_rad/sc_rad)*cos_earth_ang; /* target velocity, ignoring earth rotation */
		printf("  JPL 'non-rotating swath velocity': %.3f m/s = %.3f m/s * (%.3f/%.3f) * %.6f\n",
			swath_nr,sc_vel,earth_rad,sc_rad,cos_earth_ang);
		ts.pos=target1; /* target1 fixed-earth position */
		ts.vel=scGEI.vel; /* direction: same as spacecraft */
		vecScale(&ts.vel,swath_nr/vecMagnitude(ts.vel)); /* scale: swath_nr long */
		v=vecMagnitude(ts.vel);
		printf("  JPL-scaled velocity magnitude: %.3f m/s (%.2f%% error)\n",v,100.0*(v-tv)/tv);
		{ /* Bizarre scalar sum to convert to fixed-earth velocity */
			stateVector tz;
			double tz2d,ts2d,f2d;
			tz.pos=target1;
			tz.vel=vecNew(0,0,0);
			fixed2gei(&tz,0.0); /* tz now contains earth rotation velocity in inertial coords */
			tz2d=sqrt(tz.vel.x*tz.vel.x+tz.vel.y*tz.vel.y);
			ts2d=sqrt(ts.vel.x*ts.vel.x+ts.vel.y*ts.vel.y);
			f2d=tz2d+ts2d; 
			v=sqrt(f2d*f2d+ts.vel.z*ts.vel.z);
			printf("  JPL scalar-fixed-earth velocity magnitude: %.3f m/s (%.2f%% error), %.3f + %.3f = %.3f\n",v,100.0*(v-tv)/tv,tz2d,ts2d,f2d);
		}
		gei2fixed(&ts,0.0); /* convert to fixed-earth velocity */
		printf("  Angle between real swath vel and satellite GEI velocity: %.3f deg\n",
			acos(vecCosAng(targVel,scGEI.vel))*180.0/M_PI);
		printf("  Angle between real swath vel and satellite fixed-earth velocity: %.3f deg\n",
			acos(vecCosAng(targVel,ts.vel))*180.0/M_PI);
		v=vecMagnitude(ts.vel);
		printf("  JPL orion-fixed-earth velocity magnitude: %.3f m/s (%.2f%% error)\n",v,100.0*(v-tv)/tv);
		
	}
	
	return (0);
}
