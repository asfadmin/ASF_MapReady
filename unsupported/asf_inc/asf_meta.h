/*************************************************************************** 
   NAME:  asf_meta.h

   Header file for the asf_meta.a library's meta_get* routines.
   
   These routines are intended as a higher-level SAR image
    metadata extraction layer.  By using these routines,
    we can add support for new CEOS formats, CEOS bug fixes,
    and other data types in *one* place, instead of being
    scattered through the software.
   
   This library is intended to completely replace the 
    asf_sar.a and geolocate.a libraries.

   Orion Lawlor, 9/10/98

***************************************************************************/

#ifndef __meta_GET_H     /* include only once */

#define __meta_GET_H

#include "geolocate.h" /*for stateVector.*/
#include "ddr.h"

/******************Baseline Utilities******************/
typedef struct {
	double Bn;	 /* Normal Baseline: perpendicular to look direction.*/
	double dBn;	 /* Change in normal baseline per scene.*/
	double Bp;	 /* Parallel Baseline: parallel to look direction.*/
	double dBp;	 /* Change in parallel baseline per scene.*/
	double temporal; /* Temporal baseline, in fractional days.*/
} baseline;

/* Reads baseline from given baseline file, or exits.*/
baseline read_baseline(char *fName);

/******************** Metadata Utilities ***********************/
/*  These structures are used by the meta_get* routines.
    Try to use the functions (meta_*) where possible; don't just
    read values directly.
---------------------------------------------------------------*/
/* Proj_parameters: These describe a map projection.*/
/* Projection parameter components: one for each projection.*/

        /* Along-track/cross-track.*/
	typedef struct {
		double rlocal;	       /* Radius of earth at scene center (meters).*/
		double alpha1,alpha2,alpha3;  /*Rotation angles, in degrees.*/
	} proj_atct;

	/* Lambert Conformal projection.*/
	typedef struct {
		double plat1; /* the first standard parallel for Lambert */
		double plat2; /* the second standard parallel for Lambert */
		double lat0;  /* original lat for Lambert */
		double lon0;  /* original lon for Lambert */
	} proj_lambert;

	/* Polar Sterographic.*/
	typedef struct {
		double slat;  /* reference latitude for polar stereographic */
		double slon;  /* reference longitude for polar stereographic */
	} proj_ps;

	/* Universal Transverse Mercator.*/
	typedef struct {int zone;} proj_utm;
	
typedef struct {
	char type;    /* 'A'->Along Track/Cross Track; 'P'->Polar Stereographic
			 'L'->Lambert Conformal; 'U'->Universal Transverse Mercator.*/
	double startX,startY;  /* Projection coordinates of top, lefthand corner.*/
	double perX,perY;      /* Projection coordinates per X and Y pixel.*/
	char hem;              /* Hemisphere Code-- 'S'->southern; other northern.*/
	double re_major;       /* Semimajor axis length (equator) (meters)*/
	double re_minor;       /* Semiminor axis length (poles) (meters)*/
	double ecc;            /* First eccentricity of earth ellipsoid (unitless)*/
	   /* Note: we compute ecc=sqrt(1-re_major^2/re_minor^2) */
	union {                /* Projection parameters for each projection.*/
		proj_atct     atct;    /*Along-track/cross-track.*/
		proj_lambert  lambert; /*Lambert Conformal projection.*/
		proj_ps       ps;      /*Polar Sterographic.*/
		proj_utm      utm;     /*Universal Transverse Mercator.*/
	} param;
} proj_parameters;

/*Geo_parameters: These are used in geolocating the image.*/
typedef struct {
	char type;		/* 'S'-> Slant Range; 'G'-> Ground Range; 
				   'P'-> Map Projected.*/
	proj_parameters *proj;	/* Projection parameters, for map-projected images.*/
	char lookDir;		/* 'L'-> Left Looking; 'R'-> Right Looking.*/
	int deskew;		/* Image moved to zero-doppler? (1-> yes; 0->no)*/
	double xPix,yPix;	/* Range, azimuth pixel size, in m*/
	double rngPixTime,azPixTime;  /* Range, Azimuth pixel time, in s.*/
	double timeShift, slantShift; /* Image correction (fudge)
					 factors in azimuth and range, in s and m.*/
	double slantFirst;      /* Slant range to first pixel, in m.*/
	double wavelen;         /* Satellite wavelength, in meters.*/
	double dopRange[3], dopAz[3]; /* Doppler centroid constant, linear, and
                                         quadratic terms, in azimuth and range (Hz)*/
} geo_parameters;

/*Ifm_parameters: These are used only for interferometry.*/
typedef struct {
	double er;  /* Earth radius at scene center.*/
	double ht;  /* Satellite height from earth's center.*/
	int nLooks; /* Number of looks to take on SLC data to make square pixels*/
	int orig_nLines,orig_nSamples;
	double lookCenter;/*Look angle to image center (CALCULATED).*/
} ifm_parameters;

/*State_vectors: Some collection of fixed-earth
	state vectors around the image.  These are always
	increasing in time; but beyond that, have no assumptions.*/
typedef struct {
	double time;     /* Time of state vector, in seconds
		 	    from the start of the image.*/
	stateVector vec; /* Fixed-earth state vector.*/
} state_loc;
typedef struct {
	int  year;	/* Year for first state vector */
	int  julDay;	/* Julian day of year for first state vector */
	double second;	/* Seconds of day for first state vector */
	int  num;
	state_loc vecs[1];/*Array is sized at run-time.*/
} state_vectors;

/*extra_info: extra information needed to re-create CEOS files.*/
typedef struct {
	char	sensor[256];	 /* name of imaging sensor.  */
	char	mode[5];	 /* mode of imaging sensor.  */
	char    processor[256];  /* Name and version of SAR processor.*/
	int     orbit;		 /* Orbit number of satellite         */
	double  bitErrorRate;    /* Exactly what it says              */
	char    satBinTime[256]; /* Satellite binary clock time       */
	char    satClkTime[256]; /* Satellite UTC time                */
	double  prf;		 /* Pulse Repition Frequency	      */
} extra_info;

/*Sar_parameters: Collection of all above.*/
typedef struct {
	geo_parameters *geo;
	ifm_parameters *ifm;
	state_vectors *stVec;/*Can be NULL (check!).*/
	extra_info    *info; /*Can be NULL (check!).*/
} meta_parameters;


/****************** Creation/IO *********************
meta_init: in meta_init.c
	Extracts and returns SAR parameters from
CEOS metadata.
*/
/*In meta_init.c.  
 * These are the routines to use, generally.*/
meta_parameters *meta_init(const char *fName);
void meta_free(meta_parameters *meta);

/*In meta_coni.c*/
void meta_write(meta_parameters *meta,const char *outName);
meta_parameters *meta_read(const char *inName);

/**Internal creation routines:*/
state_vectors *raw_init_state(int nState);
meta_parameters *raw_init(void);
meta_parameters *meta_create(const char *fName);

/*************************************************************
These routines all return various parameters from the
relevant metadata.  Values are always in m, m/s, s, and radians.
All arrays and coordinates are zero-based.
*/

/******************** General ***********************
General Calls: in meta_get.c*/
/* Convert a line, sample pair to a time, slant-range pair.
These only use the geo_parameters structure, and work
for all images.  They apply the time and slant range 
correction fudge factors. Returns seconds and meters.
*/
double meta_get_time(meta_parameters *sar,double yLine,double xSample);
double meta_get_slant(meta_parameters *sar,double yLine, double xSample);

/*Converts a line, sample pair to the doppler value
at that location. Returns Hz.  Only works for SR & GR.
*/
double meta_get_dop(meta_parameters *sar,double yLine, double xSample);

/*Return fixed-earth state vector for the given time.*/
stateVector meta_get_stVec(meta_parameters *sar,double time);

/*Return the incidence angle: this is the angle measured
	by the target between straight up and the satellite.
	Returns radians.*/
double meta_incid(meta_parameters *sar,double y,double x);

/*Return the look angle: this is the angle measured
	by the satellite between earth's center and the target point.
	Returns radians.*/
double meta_look(meta_parameters *sar,double y,double x);

/************* Geolocation ***********************
Geolocation Calls: in meta_get_geo.c.
Here, latitude and longitude are always in degrees.*/

/* Converts given line and sample to that for
the non-multilooked, non-windowed, equivalent image.*/
void meta_get_orig(void *ddr,
	int y, int x,int *yOrig,int *xOrig);

/* Converts given line and sample to geodetic
latitude and longitude.  Works with all image types.
*/
void meta_get_latLon(meta_parameters *sar,
	double yLine, double xSample,double elev,double *lat,double *lon);

/* Finds line and sample corresponding to given
latitude and longitude. */
void meta_get_lineSamp(meta_parameters *meta,
	double lat,double lon,double elev,double *yLine,double *xSample);

/* Converts a given line and sample in image into time,
slant-range, and doppler.  Works with all image types.
*/  
void meta_get_timeSlantDop(meta_parameters *sar,
	double yLine,double xSample,
	double *time,double *slant,double *dop);

/*Converts the given time, slant range, doppler, and elevation
off earth's surface into a latitude and longitude.*/
void meta_timeSlantDop2latLon(meta_parameters *sar,
	double time, double slant,double dop,double elev,
	double *lat,double *lon);

/*Convert given latitude and longitude to time, slant
range, and doppler, using state vectors.*/
void latLon2timeSlant(meta_parameters *sar,
	double lat,double lon,
	double *time,double *slant,double *dop);

/*These low-level routines are used internally by asf_meta.*/
GEOLOCATE_REC *init_geolocate_meta(const stateVector *stVec,meta_parameters *meta);
void getLatLongMeta(const stateVector stVec,meta_parameters *meta,
	double range,double doppler,double elev,
	double *targLat, double *targLon, double *targRadius);

/************* Interferometry *******************
Interferometry Calls: in meta_get_ifm.c*/
/*Return satellite height.*/
double meta_get_sat_height(meta_parameters *sar);

/*Return earth radius at scene center.*/
double meta_get_earth_radius(meta_parameters *sar);

/*Return slant range to first (leftmost) pixel, and slant range per pixel.*/
void meta_get_slants(meta_parameters *sar,double *slantFirst, double *slantPer);

/*return satellite wavenumber k=2*PI/wavelength.*/
double meta_get_k(meta_parameters *sar);

/*Return the fraction of the scene that is after line y*/
double meta_scene_frac(meta_parameters *sar,int y);

/*Interpolate the given baseline for the (original image) line y.*/
void meta_interp_baseline(meta_parameters *sar,const baseline base,
	int y,double *Bn_y,double *Bp_y);

/*Return the "flat earth" look deviation--
	this is the x look angle minus the center look angle.*/
double meta_flat(meta_parameters *sar,double y,double x);

/*Return the expected phase of the given point for a bald earth-- elevation 0.*/
double meta_flat_phase(meta_parameters *sar,const baseline base,int y,int x);

/*Return the "phase rate"-- the number of meters of elevation per radian of phase.*/
double meta_phase_rate(meta_parameters *sar,const baseline base,int y,int x);


#endif
