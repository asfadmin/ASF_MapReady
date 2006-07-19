/*
Metadata handling for ASF plugins.  The central abstraction here 
is a metadata "field", which is just an int that identifies a value,
like SLANT_RANGE.  You look up the value by passing the int and an
image location to a metadata_source.

The nice part is the metadata_source is then free to do any of:
	- Return a value from one of its internal records.
	- Compute the value based on other metadata.
	- Adjust coordinates and call another metadata_source.
	- Throw an exception because the field just isn't there.
	- Transform coordinates and call some other metadata_source.
	- Call some other metadata_source and sanity-check its outputs.

This file should include everything in the old asf_meta.h header,
and everything that might be profitably used in more than one place.

Orion Sky Lawlor, olawlor@acm.org, 2006/06/12
*/
#ifndef __ASF_META_METADATA_H
#define __ASF_META_METADATA_H

#include "asf/plugin.h"
#include "asf_meta/util.h"

namespace asf {

/********************** Metadata sources ****************/

/** Metadata is indexed based on these coordinates. 
Normally these are 2D image meta coordinates--that is, XY pixels.
The Z axis is the target height in meters, or 0.0 if unknown.
For a very few other metadata values, the coordinates are strange,
like latitude/longitude/elevation, or time/slantrange/doppler, 
or just "time", and so on.
*/
typedef osl::Vector3d meta_coord_t;

class metadata_source; /* Forward declaration of class */

/** This metadata field is missing--throw an exception. */
ASF_COREDLL void metadata_missing(int field_enum,const metadata_source &fromClass);

/** Describes a metadata value, based on its enum value "v" (e.g., SLANT_RANGE) */
void metadata_field_describe(int v);

/** This debugging class traces accesses to the metasource classes. */
class ASF_COREDLL metasource_watcher {
public:
	metasource_watcher(int field_enum,const char *fromWhere,const metadata_source &fromClass);
	~metasource_watcher();
};

/** This class figures out how to transform points between coordinate systems.
Typically, there is only one static copy of this class per metadata_source subclass,
describing all the coordinate transforms that apply to that subclass.

Normal users can ignore this class and just call metadata_source::transform.
*/
class ASF_COREDLL meta_coordinate_transformer {
public:
	/** Create an empty transform set */
	meta_coordinate_transformer();
	/** Copy initial transforms from this class */
	meta_coordinate_transformer(const meta_coordinate_transformer &src);
	/** Dispose of all transforms */
	~meta_coordinate_transformer();
	
	/** A function pointer to a coordinate transform function. */
	typedef asf::meta_coord_t (*fn_t)(const asf::metadata_source &meta,const asf::meta_coord_t &src,double elev);
	
	/** Add a new 3D coordinate transform function to our table. 
	  This updates the table in a "dynamic programming" style.

	  "cost" is an estimate of the number of FLOPs this transform requires.
	  For example, a simple 2D linear transform might only cost 6 flops;
	  while a complicated iterative search might required thousands of flops.
	  
	  This routine is usually called by subclasses of metadata_source
	  to register new transform functions.
	*/
	void add(asf::metadata_3D_enum dest_coords,fn_t fn,asf::metadata_3D_enum src_coords,
		double cost);
	
	/**
	  Transform the point src, which is in coordinate system src_coords,
	  into the coordinate system dest_coords, assuming an elevation of elev meters.
	  
	  This routine is usually called by metadata_source itself.
	*/
	asf::meta_coord_t transform(const asf::metadata_source &meta,
		asf::metadata_3D_enum dest_coords,
		const asf::meta_coord_t &src,asf::metadata_3D_enum src_coords,
		double elev) const;
	
private:
	/** Number of coordinate systems we list in our table of transforms: */
	enum {nCoords=MAP_COORDINATES+1-IMAGE_PIXELS};
	
	/**
	 Records information about a transform process.
	*/
	class transform_info {
	public:
		double cost; /**< Estimated cost in flops */
		/** Functions to call to perform the transform, in call order */
		std::vector<meta_coordinate_transformer::fn_t> fns;
		transform_info();
	};
	
	/** Precomputed table showing how to transform coordinate systems:
	  transforms[src][dest] lists the transformations to make, in order,
	  to go from src coordinates to dest coordinates.  Note that an empty 
	  list means you can't get there from here!
	*/
	transform_info transforms[nCoords][nCoords];
};



/**
 Get metadata values.  This interface class is extended by all sources of metadata.
*/
class ASF_COREDLL metadata_source {
public:
	metadata_source();
	virtual ~metadata_source();

/** Virtual methods that subclasses can override */
	/// Look up the real-valued 1D field "v" at image coordinates location "loc".
	virtual double meta1D(asf::metadata_1D_enum v,const asf::meta_coord_t &loc) const =0;
	
	/// Look up the 2D vector field "v" at image coordinates location "loc".
	virtual asf::meta2D_t meta2D(asf::metadata_2D_enum v,const asf::meta_coord_t &loc) const =0;
	
	/// Look up this 3D vector field "v" at image coordinates location "loc".
	inline asf::meta3D_t meta3D(asf::metadata_3D_enum v,const asf::meta_coord_t &loc) const {
		return transform(v,loc);
	}
	
	/**
	  Transform the point src, which is in coordinate system src_coords,
	  into the coordinate system dest_coords, assuming an elevation of elev meters.
	
	  For example, meta.transform(TARGET_POSITION,meta_coord_t(x,y,0)) returns
	  the XYZ body-fixed position of the target at image meta-coordinates pixel (x,y).
	
	  meta.transform(SLANT_TIME_DOPPLER,lle,LATITUDE_LONGITUDE_ELEVATION_DEGREES,elev)
	  returns slant range, time, and doppler for a given lat-lon and elev.
	  
	  Internally, this routine calls get_transformer().transform.
	*/
	virtual asf::meta_coord_t transform(asf::metadata_3D_enum dest_coords,
		const asf::meta_coord_t &src,asf::metadata_3D_enum src_coords=IMAGE_PIXELS,
		double elev=0.0) const;
	
	/** 
	  Return the coordinate transformer we use.  Typically returns a copy of a static class.
	  This implementation returns the default set of transforms.
	*/
	virtual const meta_coordinate_transformer &get_transformer(void) const;
	
	/// Look up the state vector field "v" at location "loc".
	virtual asf::meta_state_t meta_state(asf::metadata_state_enum v,const asf::meta_coord_t &loc) const =0;
	
	/// Look up the integer field "v".
	virtual int meta_int(asf::metadata_int_enum v) const =0;
	
	/// Look up the string field "v".
	virtual asf::meta_string_t meta_string(asf::metadata_string_enum v) const =0;
	
	/// Look up the user-defined type field "v".  Returns NULL if none exists.
	virtual asf::meta_glob_t meta_glob(asf::metadata_glob_enum v) const =0;
	
/** Convenience wrapper routines-- use like "double d=someMetaObject(SLANT_RANGE,imageLoc);" */
	inline double operator() (asf::metadata_1D_enum v,const asf::meta_coord_t &loc) const 
		{return meta1D(v,loc);}
	inline meta2D_t operator() (asf::metadata_2D_enum v,const asf::meta_coord_t &loc) const
		{return meta2D(v,loc);}
	inline meta3D_t operator() (asf::metadata_3D_enum v,const asf::meta_coord_t &loc) const
		{return meta3D(v,loc);}
	inline meta_state_t operator() (asf::metadata_state_enum v,const asf::meta_coord_t &loc) const
		{return meta_state(v,loc);}
	inline int operator() (asf::metadata_int_enum v) const
		{return meta_int(v);}
	inline std::string operator() (asf::metadata_string_enum v) const
		{return meta_string(v);}
	inline asf::meta_glob_t operator() (asf::metadata_glob_enum v) const
		{return meta_glob(v);}

};

/**
 Get metadata values based on a simple image-to-image transform 
 from another metadata source.
*/
class ASF_COREDLL metadata_transform : public metadata_source {
public:
	metadata_transform(const metadata_source *source_meta_);
	
	/// These implementations all transform coordinates with "source_from_user"
	///   when needed, and then call source_meta to get the actual values.
	virtual double meta1D(asf::metadata_1D_enum v,const asf::meta_coord_t &loc) const;
	virtual asf::meta2D_t meta2D(asf::metadata_2D_enum v,const asf::meta_coord_t &loc) const;
	virtual asf::meta_coord_t transform(asf::metadata_3D_enum dest_coords,
		const asf::meta_coord_t &src,asf::metadata_3D_enum src_coords=IMAGE_PIXELS,
		double elev=0.0) const;
	virtual asf::meta_state_t meta_state(asf::metadata_state_enum v,const asf::meta_coord_t &loc) const;
	virtual int meta_int(asf::metadata_int_enum v) const;
	virtual asf::meta_string_t meta_string(asf::metadata_string_enum v) const;
	virtual asf::meta_glob_t meta_glob(asf::metadata_glob_enum v) const;
protected:
	/// Return source image coordinates given user image coordinates.
	///   Subclasses must implement this routine.
	virtual asf::meta_coord_t source_from_user(int v,const asf::meta_coord_t &user) const =0;
	
	/// Return user image coordinates given source image coordinates.
	///   Subclasses must implement this routine.
	virtual asf::meta_coord_t user_from_source(int v,const asf::meta_coord_t &source) const =0;
	
	/// This is the source of all our metadata values.
	const metadata_source *source_meta;
};

/**
 Compute metadata values based on the known features of the planet Earth.
 This includes radii, rotation rate, and the features of the moon and sun.
 Note that if the ASF tools end up being used off-planet a lot (e.g., Mars), 
 these should be pulled out into a "planet_parameters" object.
*/
class ASF_COREDLL metadata_earth : public metadata_source {
	typedef asf::metadata_source super;
public:
	virtual double meta1D(asf::metadata_1D_enum v,const asf::meta_coord_t &loc) const;
	virtual asf::meta2D_t meta2D(asf::metadata_2D_enum v,const asf::meta_coord_t &loc) const;
	virtual asf::meta_coord_t transform(asf::metadata_3D_enum dest_coords,
		const asf::meta_coord_t &src,asf::metadata_3D_enum src_coords=IMAGE_PIXELS,
		double elev=0.0) const;
	virtual const meta_coordinate_transformer &get_transformer(void) const;
	virtual asf::meta_state_t meta_state(asf::metadata_state_enum v,const asf::meta_coord_t &loc) const;
	virtual int meta_int(asf::metadata_int_enum v) const;
	virtual asf::meta_string_t meta_string(asf::metadata_string_enum v) const;
	virtual asf::meta_glob_t meta_glob(asf::metadata_glob_enum v) const;
};

/**
 Compute metadata values based on a few key Synthetic Aperture
 Radar fields.

 These implementations can compute almost every possible geolocation
  field based on these "bedrock" SAR fields:
	SLANT_TIME_DOPPLER
	SATELLITE_FROM_TIME
	WAVELENGTH and PRF
 You should inherit from this class and override meta1D and meta_state
 to provide at least these values.

 These implementations respond to unknown fields by calling the metadata_earth
 routines.
*/
class ASF_COREDLL metadata_sar : public metadata_earth {
	typedef asf::metadata_earth super;
public:
	virtual double meta1D(asf::metadata_1D_enum v,const asf::meta_coord_t &loc) const;
	virtual asf::meta2D_t meta2D(asf::metadata_2D_enum v,const asf::meta_coord_t &loc) const;
	virtual const meta_coordinate_transformer &get_transformer(void) const;
	virtual asf::meta_state_t meta_state(asf::metadata_state_enum v,const asf::meta_coord_t &loc) const
		{return super::meta_state(v,loc);}
	virtual int meta_int(asf::metadata_int_enum v) const
		{return super::meta_int(v);}
	virtual asf::meta_string_t meta_string(asf::metadata_string_enum v) const
		{return super::meta_string(v);}
	virtual asf::meta_glob_t meta_glob(asf::metadata_glob_enum v) const
		{return super::meta_glob(v);}
};


}; /* End ASF namespace */

#endif
