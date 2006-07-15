/*
Implementation of metadata.h and util.h routines.

Orion Sky Lawlor, olawlor@acm.org, 2006/06/12
*/
#include "asf_meta/metadata.h"
#include "asf/units.h"

using namespace asf; /* <- avoid having to type asf:: everywhere... */


void asf::metadata_missing(int field_enum,const asf::metadata_source &fromClass)
{
	metadata_field_describe(field_enum);
	asf::die("Program requested missing metadata field.  This usually indicates bad input or an inappropriate plugin.");
}

/** Describes a metadata value, based on its enum value "v" (e.g., SLANT_RANGE) */
void asf::metadata_field_describe(int v) {
	const char *kind;
	const enum_value_description_t *d=0;
	if (NULL!=(d=lookup_enum_value_NULL(v,metadata_1D_enum_table))) kind="1D";
	else if (NULL!=(d=lookup_enum_value_NULL(v,metadata_2D_enum_table))) kind="2D";
	else if (NULL!=(d=lookup_enum_value_NULL(v,metadata_3D_enum_table))) kind="3D";
	else if (NULL!=(d=lookup_enum_value_NULL(v,metadata_state_enum_table))) kind="state";
	else if (NULL!=(d=lookup_enum_value_NULL(v,metadata_string_enum_table))) kind="string";
	else if (NULL!=(d=lookup_enum_value_NULL(v,metadata_int_enum_table))) kind="int";
	else if (NULL!=(d=lookup_enum_value_NULL(v,metadata_glob_enum_table))) kind="glob";
	else { /* description not found! */
		printf("Unidentified and invalid metadata value %d",v);
	}
	printf("enum %s (meta_%s, integer value %d)",d->name,kind,v);
}

/** Counts number of nested metadata_source calls, to track infinite loops... */
static int metasource_nest=0, metasource_printcount=-100000;
asf::metasource_watcher::metasource_watcher(int v,const char *fromWhere,const metadata_source &fromClass)
{
	metasource_nest++;
	enum {maxNest=100}; /* start printing after this many nested calls */
	enum {maxPrint=10}; /* keep printing for this many calls */
	if (metasource_nest==maxNest) 
	{
		printf("Infinite loop detected--over %d nested calls to metasource access routines!\n",metasource_nest);
		metasource_printcount=1;
	}
	if (metasource_printcount) {
		printf("Infinite loop calls:  %s::",fromWhere);
		metadata_field_describe(v);
		printf("\n");
		if (metasource_printcount++>=maxPrint) asf::die("Infinite loop in metadata_source calls--fix your metadata_source objects");
	}
}

asf::metasource_watcher::~metasource_watcher()
{
	metasource_nest--;
}


/*********** metadata_source **********/
asf::metadata_source::~metadata_source() {}


/*********** metadata_transform **********/
asf::metadata_transform::metadata_transform(const asf::metadata_source *source_meta_)
	:source_meta(source_meta_) {}

/// These implementations all transform coordinates with "source_from_user"
///   when needed, and then call source_meta to get the actual values.
double asf::metadata_transform::meta1D(asf::metadata_1D_enum v,const asf::metaCoord_t &loc) const
{
	metasource_watcher watcher(v,"metadata_transform::meta1D",*this);
	switch (v) { 
	/* Don't transform coordinates for known non-image inputs: */
	case GHA_DEGREES_FROM_TIME: /* loc is just a time */
		return source_meta->meta1D(v,loc);
	case SATELLITE_HEIGHT_FROM_TIME: /* loc is just a time */
		return source_meta->meta1D(v,loc);
	default: {
		asf::metaCoord_t source_loc=source_from_user(v,loc);
		return source_meta->meta1D(v,source_loc);
	}
	}
}

asf::meta2D_t asf::metadata_transform::meta2D(asf::metadata_2D_enum v,const asf::metaCoord_t &loc) const
{
	metasource_watcher watcher(v,"metadata_transform::meta2D",*this);
	switch (v) { 
	/* Don't transform coordinates for known non-image inputs: */
	/* none yet */
	default: {
		asf::metaCoord_t source_loc=source_from_user(v,loc);
		return source_meta->meta2D(v,source_loc);
	}
	}
}
asf::meta3D_t asf::metadata_transform::meta3D(asf::metadata_3D_enum v,const asf::metaCoord_t &loc) const
{
	metasource_watcher watcher(v,"metadata_transform::meta3D",*this);
	switch (v) { 
	/* special: inputs and outputs are non-image */
	case asf::LLE_FROM_MAP:
	case asf::MAP_FROM_LLE:
	case asf::LLE_FROM_XYZ:
	case asf::XYZ_FROM_LLE:
	case asf::STD_FROM_XYZ:
	case asf::XYZ_FROM_STD:
		return source_meta->meta3D(v,loc);
	 /* special: output is image coordinates */
	case asf::IMAGE_FROM_XYZ: 
	case asf::IMAGE_FROM_LLE: 
	case asf::IMAGE_FROM_STD: 
	case asf::IMAGE_FROM_MAP: 
	{
		asf::metaCoord_t source_loc=source_meta->meta3D(v,loc);
		return user_from_source(v,source_loc);
	}
	/* normal case: input in image coordinates */
	default: { 
		asf::metaCoord_t source_loc=source_from_user(v,loc);
		return source_meta->meta3D(v,source_loc);
	}
	}
}
asf::meta_state_t asf::metadata_transform::meta_state(asf::metadata_state_enum v,const asf::metaCoord_t &loc) const
{
	switch (v) { 
	/* Don't transform coordinates for known non-image inputs: */
	case asf::SATELLITE_FROM_TIME: /* input is actually time */
		return source_meta->meta_state(v,loc);
	default: {
		asf::metaCoord_t source_loc=source_from_user(v,loc);
		return source_meta->meta_state(v,source_loc);
	}
	}
}

// Life is easy for metadata that doesn't depend on location--just call our source.
int asf::metadata_transform::meta_int(asf::metadata_int_enum v) const
{
	return source_meta->meta_int(v);
}
asf::meta_string_t asf::metadata_transform::meta_string(asf::metadata_string_enum v) const
{
	return source_meta->meta_string(v);
}
asf::meta_glob_t asf::metadata_transform::meta_glob(asf::metadata_glob_enum v) const
{
	return source_meta->meta_glob(v);
}

/***************************** metadata_earth *****************************/



/* Utility routines (probably should be listed in util.h) */

/* Geodata sources:

http://www-ssc.igpp.ucla.edu/personnel/russell/papers/gct1.html/
	C.T. Russell, originally published in: Cosmic Electrodynamics, 2, 184-196, 1971
	The definitive reference on coordinate transforms.

http://en.wikipedia.org/wiki/Earth_radius#Radius_at_a_given_geocentric_latitude
	Gives equations for radii of curvature at various points.
	
http://www.posc.org/Epicentre.2_2/DataModel/ExamplesofUsage/eu_cs35.html
	Describes several common mapping transforms.

http://www.ceegs.ohio-state.edu/gsreports/reports/report_453.pdf
	Section 2.5.2 gives radii of curvature.
*/

/** Convert geodetic (normal map) latitude lat to geocentric (from center of earth) latitude.
  FIXME: this assumes the point is at the Earth's surface; it's more
  complicated to do accurately for a point with high elevation.

See:
http://www.mathworks.com/access/helpdesk/help/toolbox/aeroblks/geocentrictogeodeticlatitude.html 
	Describes geocentric to geodetic conversion in detail.  For a point on the ellipsoid,
		tan(geocentric) = tan(geodetic) * (1-f)^2
*/
double geocentric_from_geodetic_radians(double lat,const asf::metadata_source &meta) {
	double re=meta(ELLIPSOID_EQUATORIAL,0), rp=meta(ELLIPSOID_POLAR,0);
	double rerp=re/rp;
	return atan(tan(lat)/(rerp*rerp));
}
/** Convert geocentric (from center of earth) latitude lat to geodetic (normal map) latitude */
double geodetic_from_geocentric_radians(double lat,const asf::metadata_source &meta) {
	double re=meta(ELLIPSOID_EQUATORIAL,0), rp=meta(ELLIPSOID_POLAR,0);
	double rerp=re/rp;
	return atan(tan(lat)*(rerp*rerp));
}

/** Return the radius of the earth at this geodetic lon/lat/elevation (degrees) */
double earth_radius_from_lle(const asf::meta3D_t &lle,const asf::metadata_source &meta) {
	double lat_cen=geocentric_from_geodetic_radians(RADIANS_FROM_DEGREES*lle.y,meta);
	double cos_lat=cos(lat_cen);
	double a=meta(ELLIPSOID_EQUATORIAL,0), b=meta(ELLIPSOID_POLAR,0);
	/* see http://en.wikipedia.org/wiki/Earth_radius */
	return a*b/sqrt(a*a-(a*a-b*b)*cos_lat*cos_lat);
}


static int month_from_day_of_year(int year,int day_of_year) {
	return -1; // FIXME!
}

static int day_of_month_from_day_of_year(int year,int day_of_year) {
	return -1; // FIXME!
}

/** A type wrapper around a longitude/latitude/elevation (degrees/meters) meta3D value */
class meta3D_lle_degrees : public meta3D_t {
public: meta3D_lle_degrees(const meta3D_t &m) :meta3D_t(m) {}
};

/** A type wrapper around a body-fixed meta3D value */
class meta3D_bodyfixed : public meta3D_t {
public: meta3D_bodyfixed(const meta3D_t &m) :meta3D_t(m) {}
};

/** A geocentric position.  Geocentric coordinates are measured from the center of the Earth. */
class geocentric_radians {
public:
	double lon; /* XY longitude (radians) */
	double lat_cen; /* Geocentric latitude (radians) */
	double r; /* distance to center of earth (meters) */
	
	/** Create from lon/lat/elevation (degrees) */
	geocentric_radians(const meta3D_lle_degrees &lle,const metadata_source &meta);
	
	/** Create from a 3D body-fixed position */
	geocentric_radians(const meta3D_bodyfixed &bodyfixed);
	
	/** Convert us to a 3D body-fixed position */
	asf::meta3D_t get_cartesian(void) const;
	
	/** Return our geodetic latitude, in radians */
	double geodetic_latitude(const metadata_source &meta) const {
		return geodetic_from_geocentric_radians(lat_cen,meta);
	}
};
geocentric_radians::geocentric_radians(const meta3D_lle_degrees &lle,const metadata_source &meta)
{ 
/* FIXME: to be centimeter-accurate on mountains, need real geocentric/geodetic here... */
	lon=RADIANS_FROM_DEGREES*lle.x;
	lat_cen=geodetic_from_geocentric_radians(RADIANS_FROM_DEGREES*lle.y,meta);
	r=earth_radius_from_lle(lle,meta)+lle.z;
}
geocentric_radians::geocentric_radians(const meta3D_bodyfixed &v)
{
	r=v.mag();    
	lat_cen=asin(v.z/r);      
	lon=atan2_check(v.y,v.x);
}
asf::meta3D_t geocentric_radians::get_cartesian(void) const
{
	asf::meta3D_t v;
	v.x=r*cos(lat_cen)*cos(lon);	 
	v.y=r*cos(lat_cen)*sin(lon);	      
	v.z=r*sin(lat_cen);
	return v;
}

/** A geodetic position--normal lat/lon/elevation.  Geodetic positions are measured
relative to the *local* normal, which due to the non-spherical nature of Earth,
isn't the same as the direction toward the Earth center. */
class geodetic_radians {
public:
	double lon; /* XY longitude (radians) */
	double lat; /* geodetic latitude (radians) */
	double elev; /* distance from local surface (meters) */
	
	/** Create from lat/lon/elevation (degrees) */
	geodetic_radians(const meta3D_lle_degrees &lle)
		:lon(RADIANS_FROM_DEGREES*lle.x), 
		 lat(RADIANS_FROM_DEGREES*lle.y), 
		 elev(lle.z) {}
	
	/** Create from a 3D body-fixed position */
	geodetic_radians(const meta3D_bodyfixed &bodyfixed,const metadata_source &meta);
	
	/** Convert us to a lon/lat/elevation (degrees) */
	meta3D_lle_degrees get_lle(void) const;
	
	/** Convert us to a 3D body-fixed position */
	asf::meta3D_t get_cartesian(const metadata_source &meta) const;
};

/** Create from a 3D body-fixed position */
geodetic_radians::geodetic_radians(const meta3D_bodyfixed &bodyfixed,const metadata_source &meta)
{
/* FIXME: to be centimeter-accurate on mountains, need real geocentric/geodetic here... */
	geocentric_radians cen(bodyfixed);
	lat=cen.geodetic_latitude(meta);
	lon=cen.lon;
	elev=cen.r-earth_radius_from_lle(get_lle(),meta);
}

/** Convert us to a lon/lat/elevation (degrees) */
meta3D_lle_degrees geodetic_radians::get_lle(void) const
{
	meta3D_t m3(DEGREES_FROM_RADIANS*lon,DEGREES_FROM_RADIANS*lat,elev);
	meta3D_lle_degrees lle(m3);
	return lle;
}

/** Convert us to a 3D body-fixed position */
asf::meta3D_t geodetic_radians::get_cartesian(const metadata_source &meta) const
{
	geocentric_radians cen(get_lle(),meta);
	return cen.get_cartesian();
}

double asf::metadata_earth::meta1D(asf::metadata_1D_enum v,const asf::metaCoord_t &loc) const
{
	const metadata_source &meta=*this;
	metasource_watcher watcher(v,"metadata_earth::meta1D",meta);
	switch (v) {
	/* TIME_SINCE_START is fundamental */
	case TIME_SECONDS_OF_DAY: 
		return meta1D(IMAGE_START_SECONDS_OF_DAY,0)+meta1D(TIME_SINCE_START,loc);
	case ELLIPSOID_LOCAL: { /* calculate from lat/lon/elevation */
		metaCoord_t lle=meta3D(LONGITUDE_LATITUDE_ELEVATION_DEGREES,loc);
		return earth_radius_from_lle(lle,meta);
		};
	case ELLIPSOID_EQUATORIAL: return 6378137.0; /* WGS-84 ellipsoid (by default) */
	case ELLIPSOID_POLAR: return 6356752.3; /* WGS-84 ellipsoid (by default) */
	case ELLIPSOID_FLATTENING: {
		double major=meta1D(ELLIPSOID_EQUATORIAL,loc);
		double minor=meta1D(ELLIPSOID_POLAR,loc);
		return (major-minor)/major;
		};
	case ELLIPSOID_ECCENTRICITY: {
		double major=meta1D(ELLIPSOID_EQUATORIAL,loc);
		double minor=meta1D(ELLIPSOID_POLAR,loc);
		return sqrt(1-major*major/(minor*minor));
		};
	case SIDEREAL_ROTATION_RATE_RADIANS: 
		return sidereal_rotation_rate_radians_earth;
	case G_TIMES_MASS_PLANET:
		return 3.986005e14; /*Gravitational constant times mass of Earth (si units) */
	case GHA_DEGREES:
		return meta1D(GHA_DEGREES_FROM_TIME,metaCoord_t(meta1D(TIME_SINCE_START,loc),0,0));
	case GHA_DEGREES_FROM_TIME:
		return utc2gha(meta_int(TIME_YEAR),meta_int(TIME_DAY_OF_YEAR),
			0,0,meta1D(IMAGE_START_SECONDS_OF_DAY,0)+loc.x);
	case SATELLITE_HEIGHT:
		return meta3D(SATELLITE_POSITION,loc).mag();
	case SATELLITE_HEIGHT_FROM_TIME:
		return meta_state(SATELLITE_FROM_TIME,loc).pos.mag();
	
	/* all SAR-specific fields handled in meta_sar.cpp */
	
	case INCIDENCE_DEGREES:
		return DEGREES_FROM_RADIANS*meta1D(INCIDENCE_RADIANS,loc);
	case INCIDENCE_RADIANS:
		return acos(
			meta3D(TARGET_UP_DIRECTION,loc)
		  .cosAng(
		  	meta3D(TARGET_SATELLITE_DIRECTION,loc)
		));
	case LOOK_DEGREES:
		return DEGREES_FROM_RADIANS*meta1D(LOOK_RADIANS,loc);
	case LOOK_RADIANS:
		return acos(
			meta3D(SATELLITE_DOWN_DIRECTION,loc)
		  .cosAng(
		  	meta3D(SATELLITE_TARGET_DIRECTION,loc)
		));
	
	/* CLOUD_COVER and BIT_ERROR_RATE are fundamental */
	case GEOCODING_HEIGHT:
		return 0.0; /* geocode images to ellipsoid by default */
	/* interferometry fields handled below */
	default: /* We can't handle a request for any other metadata field... */
		metadata_missing(v,meta);
		return -999999.0; /* never executed */
	}
}

asf::meta2D_t asf::metadata_earth::meta2D(asf::metadata_2D_enum v,const asf::metaCoord_t &loc) const
{
	const metadata_source &meta=*this;
	metasource_watcher watcher(v,"metadata_earth::meta2D",meta);
	switch (v) {
	case LONGITUDE_LATITUDE_DEGREES: {
		meta3D_t lle=meta3D(LONGITUDE_LATITUDE_ELEVATION_DEGREES,loc);
		return meta2D_t(lle.x,lle.y);
	}
	/* PROJECTION_COORDINATES is fundamental */
	/* INTERFEROMETRIC_BASELINE is fundamental */
	default:
		metadata_missing(v,meta);
		return meta2D_t(-999999.0,-999999.0); /* never executed */
	}
}

asf::meta3D_t asf::metadata_earth::meta3D(asf::metadata_3D_enum v,const asf::metaCoord_t &loc) const
{
	const metadata_source &meta=*this;
	metasource_watcher watcher(v,"metadata_earth::meta3D",meta);
	switch(v) {
	case TARGET_POSITION: { /* FIXME: infinite mutual recursion is possible here, if a subclass fails to override *any* of the geolocation calls. */
		return meta3D(XYZ_FROM_LLE,meta3D(LONGITUDE_LATITUDE_ELEVATION_DEGREES,loc));
	}
	case TARGET_SATELLITE_DIRECTION:
		return (meta3D(SATELLITE_POSITION,loc)-meta3D(TARGET_POSITION,loc)).dir();

/*
  SUBTLE: "up" means along the gravitational gradient.  It doesn't mean away
  from the center of the Earth--because of the flattening of the Earth, up and
  away from center can be as much as 0.2 degrees apart (at +-45 degrees latitude).
*/
#define step_dir(step_code) { \
		meta3D_lle_degrees lle=meta3D(LONGITUDE_LATITUDE_ELEVATION_DEGREES,loc); \
		geocentric_radians g_lo(lle,meta); \
		step_code; \
		geocentric_radians g_hi(lle,meta); \
		return (g_hi.get_cartesian()-g_hi.get_cartesian()).dir(); \
	}
	case TARGET_UP_DIRECTION: step_dir(lle.z+=1.0e3); /* Raise elevation by 1km */
	case TARGET_NORTH_DIRECTION: step_dir(lle.y+=1.0e-4; /* Go north a tiny bit */
		if (lle.y>=90.0) /* Went past the north pole--give up. */
			return meta3D_t(0);
		);
	case TARGET_EAST_DIRECTION: step_dir(lle.x+=1.0e-4); /* Go east a tiny bit */
	
	/*
	  FIXME: implement TARGET_SUN_DIRECTION and TARGET_MOON_DIRECTION.
	  Code exists on the net for this.  Really only need accuracy to a degree
	  or so for accurate optical shading/shadowing...
	*/
	case SATELLITE_POSITION:
		return meta_state(SATELLITE_BODYFIXED,loc).pos;
	case SATELLITE_VELOCITY:
		return meta_state(SATELLITE_BODYFIXED,loc).vel;
	case SATELLITE_TARGET_DIRECTION:
		return -meta3D(TARGET_SATELLITE_DIRECTION,loc);
	case SATELLITE_DOWN_DIRECTION: {
		meta3D_t sat=meta3D(SATELLITE_POSITION,loc);
		meta3D_bodyfixed bf(sat);
		geodetic_radians g(bf);
		g.elev-=1.0e3;
		return (g.get_cartesian(meta)-sat).dir();
	}
	case SATELLITE_FLIGHT_DIRECTION:
		return meta_state(SATELLITE_BODYFIXED,loc).vel.dir();
	case LONGITUDE_LATITUDE_ELEVATION_DEGREES: { 
		/* Danger!  Mutual recursion with TARGET_POSITION! You MUST override one or the other! */
		return meta3D(LLE_FROM_XYZ,meta3D(TARGET_POSITION,loc));
	}
	case LLE_FROM_XYZ: {
		meta3D_bodyfixed target=loc;
		geodetic_radians geodetic(target,meta);
		return geodetic.get_lle();
	}
	case XYZ_FROM_LLE: {
		meta3D_lle_degrees lle=loc;
		geodetic_radians geodetic(lle);
		return geodetic.get_cartesian(meta);
	}
	
	/* TIME_SLANT_DOPPLER and LLE_FROM_TSD are handled in metadata_sar.cpp */
	default:
		metadata_missing(v,meta);
		return meta3D_t(-999999.0,-999999.0,-999999.0); /* never executed */
	}
}

asf::meta_state_t asf::metadata_earth::meta_state(asf::metadata_state_enum v,const asf::metaCoord_t &loc) const
{
	const metadata_source &meta=*this;
	metasource_watcher watcher(v,"metadata_earth::meta_state",meta);
	switch(v) {
	case SATELLITE_BODYFIXED:
		return meta_state(SATELLITE_FROM_TIME,meta3D_t(meta1D(TIME_SINCE_START,loc),0,0));
	// Could also calculate this field based on SATELLITE_GEI:
	//	return meta_state(SATELLITE_GEI,loc).rotate_coriolis(
	//		-meta1D(GHA_DEGREES,loc),-meta1D(SIDEREAL_ROTATION_RATE_RADIANS,loc));
	case SATELLITE_GEI:
		return meta_state(SATELLITE_BODYFIXED,loc).rotate_coriolis(
			+meta1D(GHA_DEGREES,loc),+meta1D(SIDEREAL_ROTATION_RATE_RADIANS,loc));
	case SATELLITE_GEI0:
		return meta_state(SATELLITE_BODYFIXED,loc).rotate_coriolis(
			0.0,+meta1D(SIDEREAL_ROTATION_RATE_RADIANS,loc));
	case TARGET_BODYFIXED:
		return meta_state_t(meta3D(TARGET_POSITION,loc),meta3D_t(0.0));
	case TARGET_GEI:
		return meta_state(TARGET_BODYFIXED,loc).rotate_coriolis(
			+meta1D(GHA_DEGREES,loc),+meta1D(SIDEREAL_ROTATION_RATE_RADIANS,loc));
	case TARGET_GEI0:
		return meta_state(TARGET_BODYFIXED,loc).rotate_coriolis(
			0.0,+meta1D(SIDEREAL_ROTATION_RATE_RADIANS,loc));
	/* SATELLITE_FROM_TIME is fundamental */
	default:
		metadata_missing(v,meta);
		return meta_state_t(meta3D_t(-999999.0),meta3D_t(-999999.0)); /* never executed */
	}
}

int asf::metadata_earth::meta_int(asf::metadata_int_enum v) const {
	const metadata_source &meta=*this;
	metasource_watcher watcher(v,"metadata_earth::meta_int",meta);
	switch(v) {
	case SPHEROID_TYPE:
		return WGS84_SPHEROID; /* default values in meta1D(ELLIPSOID_...) */
	/* TIME_YEAR and TIME_DAY_OF_YEAR are fundamental */
	case TIME_MONTH:
		return month_from_day_of_year(meta_int(TIME_YEAR),meta_int(TIME_DAY_OF_YEAR));
	case TIME_DAY_OF_MONTH:
		return day_of_month_from_day_of_year(meta_int(TIME_YEAR),meta_int(TIME_DAY_OF_YEAR));
	case IS_DESCENDING: /* descending if moving south--or in negative Z */
		return meta_state(SATELLITE_FROM_TIME,meta3D_t(0)).vel.z<0;
	default:
		metadata_missing(v,meta);
		return -999999; /* never executed */
	}
}
asf::meta_string_t asf::metadata_earth::meta_string(asf::metadata_string_enum v) const
{
	const metadata_source &meta=*this;
	metasource_watcher watcher(v,"metadata_earth::meta_string",meta);
	switch(v) {
	/* SENSOR, MODE, and PROCESSOR are all fundamental */
	default:
		metadata_missing(v,meta);
		return "friggit"; /* never executed */
	}
}
asf::meta_glob_t asf::metadata_earth::meta_glob(asf::metadata_glob_enum v) const 
{
	const metadata_source &meta=*this;
	metasource_watcher watcher(v,"metadata_earth::meta_glob",meta);
	switch(v) {
	/* all globs are fundamental */
	default:
		metadata_missing(v,meta);
		return 0; /* never executed */
	}
}

