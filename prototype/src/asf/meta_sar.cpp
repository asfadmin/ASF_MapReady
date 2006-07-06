/*
Metadata routines that relate to Synthetic Aperture Radar.

This includes:
	- Geolocation based on slant range, time, and doppler
	- 

Orion Sky Lawlor, olawlor@acm.org, 2006/06/20
*/
#include "asf/meta.h"
#include "asf/units.h"
using namespace asf;

double asf::metadata_sar::meta1D(asf::metadata_1D_enum v,const asf::metaCoord_t &loc)
{
	metadata_source &meta=*this;
	switch (v) {
	case SLANT_RANGE:
		return meta(SLANT_TIME_DOPPLER,loc).x;
	case TIME_SINCE_START:
		return meta(SLANT_TIME_DOPPLER,loc).y;
	case DOPPLER:
		return meta(SLANT_TIME_DOPPLER,loc).z;
	case DOPPLER_RATE:
		asf::die("FIXME: compute doppler rate in meta1D...\n");
		return 0.0;
	/* PRF is a bedrock field */
	/* wavelength is a bedrock field */
	case FREQUENCY: 
		return SPEED_OF_LIGHT/meta1D(WAVELENGTH,loc);
	case WAVENUMBER:
		return 2*M_PI/meta1D(WAVELENGTH,loc);
	/* everything else is fundamental */
	
	case INTERFEROMETRIC_REFERENCE_LOOK_RADIANS: {
		/* Arbitrary choice of reference angle: corner of image */
		return meta1D(LOOK_RADIANS,meta3D_t(0,0,0));
	}
	case INTERFEROMETRIC_LOOK_RADIANS: {
		/* Interferometric look angle difference is just actual look minus reference */
		return meta1D(LOOK_RADIANS,loc)-meta1D(INTERFEROMETRIC_REFERENCE_LOOK_RADIANS,loc);
	}
	case INTERFEROMETRIC_FLAT_PHASE: {
		/* flat-Earth look angle difference to reference range */
		double flat=meta1D(INTERFEROMETRIC_LOOK_RADIANS,loc);
		meta2D_t base=meta2D(INTERFEROMETRIC_BASELINE,loc);
		return 2.0*meta1D(WAVENUMBER,loc)*(base.x*cos(flat)+base.y*sin(flat));
	}
	
	case INTERFEROMETRIC_PHASE_RATE: {
		double sr=meta1D(SLANT_RANGE,loc);
		double flat=meta1D(INTERFEROMETRIC_LOOK_RADIANS,loc);
		double incid=meta1D(INCIDENCE_RADIANS,loc);
		meta2D_t base=meta2D(INTERFEROMETRIC_BASELINE,loc);
		
		/* This is the derivative of INTERFEROMETRIC_FLAT_PHASE with respect to "flat" */
		double deriv=2.0*meta1D(WAVENUMBER,loc)*(-base.x*sin(flat)+base.y*cos(flat));
		/* "sr*sin(incid)" is our lever arm length.  "deriv" converts look angle to phase difference */
		/* FIXME: there's a nonlinear version of this.  I just don't know what it is... */
		return (sr*sin(incid))/deriv;
	}
	default:
		return super::meta1D(v,loc);
	}
}

asf::meta2D_t asf::metadata_sar::meta2D(asf::metadata_2D_enum v,const asf::metaCoord_t &loc)
{
	return super::meta2D(v,loc);
}
asf::meta3D_t asf::metadata_sar::meta3D(asf::metadata_3D_enum v,const asf::metaCoord_t &loc)
{
	metadata_source &meta=*this;
	switch (v) {
	/* The only actual geolocation call we support. Convert SAR parameters
	  (slant range, time, doppler, and a state vector) to a position.
	*/
	case TARGET_POSITION: {
		meta3D_t std=meta(SLANT_TIME_DOPPLER,loc);
		double range=std.x, time=std.y, doppler=std.z, elev=loc.z;
		meta_state_t sat=meta(SATELLITE_FROM_TIME,asf::meta3D_t(time,0,0));
		double re=meta(ELLIPSOID_EQUATORIAL,loc);
		double rp=meta(ELLIPSOID_POLAR,loc);
		
		/**
		If millimeter-scale geolocations are needed, adjust for different
		 up vector at different locations (see pp/pp_ssp/src/pp_correlator/seetarg.f):
		ratio = (r+elev/cos((lat-lat_d)*rad_per_deg))/r
		re = re*ratio
		rp = rp*ratio
		For normal purposes, just bump up the equatorial and polar radii:
		*/
		re+=elev; rp+=elev;
		
		asf::die("meta_sar.cpp: ... copy in geolocate.c routines here ... \n");
		return *(asf::meta3D_t *)0;
	}
	
	default:
		return super::meta3D(v,loc);
	}
}

