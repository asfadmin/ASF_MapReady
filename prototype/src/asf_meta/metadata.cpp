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

/** Counts number of nested metadata_source calls, to track infinite loops. 
 FIXME: not threadsafe.  Probably a bad idea in general.
*/
static int metasource_nest=0, metasource_printcount=-100;
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
		printf(" metasource_watcher:  %s::",fromWhere);
		metadata_field_describe(v);
		printf("\n");
		if (metasource_printcount++>=maxPrint) asf::die("Infinite loop in metadata_source calls--fix your metadata_source objects");
	}
}

asf::metasource_watcher::~metasource_watcher()
{
	metasource_nest--;
}

/*********** meta_coordinate_transformer **********/

asf::meta_coordinate_transformer::transform_info::transform_info()
{
	double zero=0.0;
	cost=1.0/zero; /* floating-point infinity--a very high cost! */
}

/** Create an empty transform set */
asf::meta_coordinate_transformer::meta_coordinate_transformer() {
	/* Initialize the diagonal transforms to zero-cost-- no function needed. */
	for (int i=0;i<nCoords;i++)
		transforms[i][i].cost=0.0; /* costs nothing to go from i to i; and you don't need to do anything either! */
}
/** Copy initial transforms from this class */
asf::meta_coordinate_transformer::meta_coordinate_transformer(const meta_coordinate_transformer &src)
	:transforms(src.transforms)
{}
/** Dispose of all transforms */
asf::meta_coordinate_transformer::~meta_coordinate_transformer() {}

/** Add a new 3D coordinate transform function to our table. 
  This updates the table in a "dynamic programming" style.

  "cost" is an estimate of the number of FLOPs this transform requires.
  For example, a simple 2D linear transform might only cost 6 flops;
  while a complicated iterative search might required thousands of flops.
  
  This routine is usually called by subclasses of metadata_source
  to register new transform functions.
*/
void asf::meta_coordinate_transformer::add(asf::metadata_3D_enum dest_coords,fn_t sd_fn,
	asf::metadata_3D_enum src_coords,double sd_cost)
{
	int s=src_coords-IMAGE_PIXELS, d=dest_coords-IMAGE_PIXELS;
	if (s<0 || s>=nCoords) asf::die("meta_coordinate_transformer::add> source coordinate system enum invalid");
	if (d<0 || d>=nCoords) asf::die("meta_coordinate_transformer::add> dest coordinate system enum invalid");
	
	sd_cost+=5.0; /* function call overhead */
	
	/**
	 for all i,j: consider adding a new path from i to j like this:
		i to s   s to d   d to j
	*/
	for (int i=0;i<nCoords;i++)
	for (int j=0;j<nCoords;j++) {
		const transform_info &is=transforms[i][s];
		const transform_info &dj=transforms[d][j];
		      transform_info &ij=transforms[i][j];
		double newCost=is.cost+sd_cost+dj.cost;
		if (newCost<ij.cost) 
		{ /* This new function is faster than the previous way-- add it */
			ij.cost=newCost;
			/* First get from i coordinates to s coordinates */
			ij.fns=is.fns;
			/* Now go from s to d coordinates */
			ij.fns.push_back(sd_fn);
			/* And finally from d to j coordinates */
			for (unsigned int k=0;k<dj.fns.size();k++)
				ij.fns.push_back(dj.fns[k]);
		}
	}
}

// Return true if this coordinate system's Z coordinate is elevation (in meters)
bool z_is_elevation(asf::metadata_3D_enum coords) {
	switch (coords) {
	case IMAGE_PIXELS: 
	case LONGITUDE_LATITUDE_DEGREES: case LATITUDE_LONGITUDE_DEGREES:
	case MAP_COORDINATES:
		return true;
	default:
		return false;
	}
}

/**
  Transform the point src, which is in coordinate system src_coords,
  into the coordinate system dest_coords, assuming an elevation of elev meters.
  
  This routine is usually called by metadata_source itself.
*/
asf::meta3D_t asf::meta_coordinate_transformer::transform(const asf::metadata_source &meta,
		asf::metadata_3D_enum dest_coords,
		const asf::meta_coord_t &src,asf::metadata_3D_enum src_coords,
		double elev) const
{
	int s=src_coords-IMAGE_PIXELS, d=dest_coords-IMAGE_PIXELS;
	if (s<0 || s>=nCoords) asf::die("Metadata_source::transform> source coordinate system enum invalid");
	if (d<0 || d>=nCoords) asf::die("Metadata_source::transform> dest coordinate system enum invalid");
	
	if (elev==0.0 && src.z!=0.0 && z_is_elevation(src_coords)) 
	{ /* Use elevation from passed-in coordinate */
		elev=src.z;
	}
	
	const std::vector<fn_t> &fns=transforms[s][d].fns;
	int i,len=fns.size();
	if (len==0) { /* No functions listed-- weird. */
		if (s==d) return src; /* identity transform */
		else metadata_missing(dest_coords,meta); /* don't know how to do transform (FIXME: synthesize inverse transform here) */
	}

/* Apply each coordinate transformation in turn */
	if (len==1) return fns[0](meta,src,elev); /* Optimization: skip coordinate copying for common case */
	asf::meta_coord_t cur=src;
	for (i=0;i<len;i++) {
		cur=fns[i](meta,cur,elev); 
	}
	return cur;
}

/*********** metadata_source **********/
asf::metadata_source::metadata_source() {}
asf::metadata_source::~metadata_source() {}

asf::meta_coord_t asf::metadata_source::transform(asf::metadata_3D_enum dest_coords,
		const asf::meta_coord_t &src,asf::metadata_3D_enum src_coords,
		double elev) const
{
	return get_transformer().transform(*this,dest_coords,src,src_coords,elev);
}

/** Simple coordinate transform function: swaps X and Y coordinates */
static meta_coord_t swapXY_transform_fn(const asf::metadata_source &meta,const asf::meta_coord_t &src,double elev) {
	return meta3D_t(src.y,src.x,src.z);
}

/** 
  Return the coordinate transformer we use.  Typically returns a copy of a static class.
  This implementation returns the default set of transforms.
*/
const meta_coordinate_transformer &asf::metadata_source::get_transformer(void) const
{
	static meta_coordinate_transformer *trans=0;
	if (trans==0) {
		meta_coordinate_transformer *t=new meta_coordinate_transformer(/* superclass::get_transformer() */);
		t->add(LONGITUDE_LATITUDE_DEGREES,swapXY_transform_fn,LATITUDE_LONGITUDE_DEGREES,0.5);
		t->add(LATITUDE_LONGITUDE_DEGREES,swapXY_transform_fn,LONGITUDE_LATITUDE_DEGREES,0.5);
		trans=t;
	}
	return *trans;
}

/*********** metadata_transform **********/
asf::metadata_transform::metadata_transform(const asf::metadata_source *source_meta_)
	:source_meta(source_meta_) {}

/// These implementations all transform coordinates with "source_from_user"
///   when needed, and then call source_meta to get the actual values.
double asf::metadata_transform::meta1D(asf::metadata_1D_enum v,const asf::meta_coord_t &loc) const
{
	metasource_watcher watcher(v,"metadata_transform::meta1D",*this);
	switch (v) { 
	/* Don't transform coordinates for known non-image inputs: */
	case GHA_DEGREES_FROM_TIME: /* loc is just a time */
		return source_meta->meta1D(v,loc);
	case SATELLITE_HEIGHT_FROM_TIME: /* loc is just a time */
		return source_meta->meta1D(v,loc);
	default: {
		asf::meta_coord_t source_loc=source_from_user(v,loc);
		return source_meta->meta1D(v,source_loc);
	}
	}
}

asf::meta2D_t asf::metadata_transform::meta2D(asf::metadata_2D_enum v,const asf::meta_coord_t &loc) const
{
	metasource_watcher watcher(v,"metadata_transform::meta2D",*this);
	switch (v) { 
	/* Don't transform coordinates for known non-image inputs: */
	/* none yet */
	default: {
		asf::meta_coord_t source_loc=source_from_user(v,loc);
		return source_meta->meta2D(v,source_loc);
	}
	}
}
asf::meta_coord_t asf::metadata_transform::transform(asf::metadata_3D_enum dest_coords,
		const asf::meta_coord_t &src_param,asf::metadata_3D_enum src_coords,
		double elev) const
{
	asf::meta_coord_t src=src_param,dest;
	if (src_coords==IMAGE_PIXELS) src=source_from_user(dest_coords,src);
	dest=source_meta->transform(dest_coords,src,src_coords,elev);
	if (dest_coords==IMAGE_PIXELS) dest=user_from_source(src_coords,dest);
	return dest;
}

asf::meta_state_t asf::metadata_transform::meta_state(asf::metadata_state_enum v,const asf::meta_coord_t &loc) const
{
	switch (v) { 
	/* Don't transform coordinates for known non-image inputs: */
	case asf::SATELLITE_FROM_TIME: /* input is actually time */
		return source_meta->meta_state(v,loc);
	default: {
		asf::meta_coord_t source_loc=source_from_user(v,loc);
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

double asf::metadata_earth::meta1D(asf::metadata_1D_enum v,const asf::meta_coord_t &loc) const
{
	const metadata_source &meta=*this;
	metasource_watcher watcher(v,"metadata_earth::meta1D",meta);
	switch (v) {
	/* TIME_SINCE_START is fundamental */
	case TIME_SECONDS_OF_DAY: 
		return meta1D(IMAGE_START_SECONDS_OF_DAY,0)+meta1D(TIME_SINCE_START,loc);
	case ELLIPSOID_LOCAL: { /* calculate from lat/lon/elevation */
		meta_coord_t lle=meta3D(LONGITUDE_LATITUDE_DEGREES,loc);
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
		return meta1D(GHA_DEGREES_FROM_TIME,meta_coord_t(meta1D(TIME_SINCE_START,loc),0,0));
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

asf::meta2D_t asf::metadata_earth::meta2D(asf::metadata_2D_enum v,const asf::meta_coord_t &loc) const
{
	const metadata_source &meta=*this;
	metasource_watcher watcher(v,"metadata_earth::meta2D",meta);
	switch (v) {
	/* INTERFEROMETRIC_BASELINE is fundamental */
	default:
		metadata_missing(v,meta);
		return meta2D_t(-999999.0,-999999.0); /* never executed */
	}
}


// meta_coordinate_transformer coordinate transform functions:
static asf::meta_coord_t xyz_from_lle(const asf::metadata_source &meta,const asf::meta_coord_t &loc,double elev)
{
	meta3D_lle_degrees lle=loc;
	geodetic_radians geodetic(lle);
	return geodetic.get_cartesian(meta);
}
static asf::meta_coord_t lle_from_xyz(const asf::metadata_source &meta,const asf::meta_coord_t &loc,double elev)
{
	meta3D_bodyfixed target=loc;
	geodetic_radians geodetic(target,meta);
	return geodetic.get_lle();
}

const meta_coordinate_transformer &asf::metadata_earth::get_transformer(void) const {
	static meta_coordinate_transformer *trans=0;
	if (trans==0) { /* Add our transforms to those of our superclass */
		meta_coordinate_transformer *t=new meta_coordinate_transformer(super::get_transformer());
		t->add(TARGET_POSITION,xyz_from_lle,LONGITUDE_LATITUDE_DEGREES,300.0); /* expensive because of sines and cosines */
		t->add(LONGITUDE_LATITUDE_DEGREES,lle_from_xyz,TARGET_POSITION,300.0);
		trans=t;
	}
	return *trans;
}

asf::meta_coord_t asf::metadata_earth::transform(asf::metadata_3D_enum dest_coords,
		const asf::meta_coord_t &src,asf::metadata_3D_enum src_coords,
		double elev) const
{
	const metadata_source &meta=*this;
	metasource_watcher watcher(dest_coords,"metadata_earth::transform",meta);
	/* Special destination-only transforms: */
	switch(dest_coords) {
	case TARGET_SATELLITE_DIRECTION:
		return (
		     transform(SATELLITE_POSITION,src,src_coords,elev)
		    -transform(TARGET_POSITION,src,src_coords,elev)
		).dir();

/*
  SUBTLE: "up" means along the gravitational gradient.  It doesn't mean away
  from the center of the Earth--because of the flattening of the Earth, up and
  away from center can be as much as 0.2 degrees apart (at +-45 degrees latitude).
*/
#define step_dir(step_code) { \
		meta3D_lle_degrees lle=transform(LONGITUDE_LATITUDE_DEGREES,src,src_coords,elev); \
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
		return meta_state(SATELLITE_BODYFIXED,transform(IMAGE_PIXELS,src,src_coords,elev)).pos;
	case SATELLITE_VELOCITY:
		return meta_state(SATELLITE_BODYFIXED,transform(IMAGE_PIXELS,src,src_coords,elev)).vel;
	case SATELLITE_TARGET_DIRECTION:
		return -transform(TARGET_SATELLITE_DIRECTION,src,src_coords,elev);
	case SATELLITE_DOWN_DIRECTION: {
		meta3D_t sat=transform(SATELLITE_POSITION,src,src_coords,elev);
		meta3D_bodyfixed bf(sat);
		geodetic_radians g(bf);
		g.elev-=1.0e3;
		return (g.get_cartesian(meta)-sat).dir();
	}
	case SATELLITE_FLIGHT_DIRECTION:
		return meta_state(SATELLITE_BODYFIXED,transform(IMAGE_PIXELS,src,src_coords,elev)).vel.dir();
	default: /* All normal coordinate transforms are handled by our transformer */
		return metadata_source::transform(dest_coords,src,src_coords,elev);
	}
}

asf::meta_state_t asf::metadata_earth::meta_state(asf::metadata_state_enum v,const asf::meta_coord_t &loc) const
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

