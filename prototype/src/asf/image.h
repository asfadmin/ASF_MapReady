/*
ASF Plugin Architecture
PROTOTYPE 1-L

Definitions used for image processing: specifically, 
image tiles.

Orion Sky Lawlor, olawlor@acm.org, 2005/09/08.
*/
#ifndef __ASF_IMAGE_H
#define __ASF_IMAGE_H

#include "asf/plugin.h"
#include "osl/vector2d.h"

namespace asf {

/**
 A 2D floating-point location coordinate.
*/
typedef osl::Vector2d image_location;
ASF_COREDLL void print(const asf::image_location &v,FILE *f=NULL);

/** (0.5,0.5), the bias from a pixel corner to pixel center.
Used to convert floating-point locations to integer pixels & vice versa. 
*/
ASF_COREDLL extern image_location half_pixel_bias;

/** 
 Linear 2D rotation-translation-scaling-skew function. 
   Also can be thought of as a homogenous 3x3 matrix, like:
<pre>
  [ n.x ]   [ x.x  y.x  o.x ] [ i.x ]
  [ n.y ] = [ x.y  y.y  o.y ] [ i.y ]
  [  1  ]   [  0     0   1  ] [  1  ]
</pre>
*/
class ASF_COREDLL linear2d_function {
public:
	image_location o; ///< Origin of function output.  o==apply(0,0)
	image_location x; ///< X axis of output. x==apply(1,0)-o
	image_location y; ///< Y axis of output. y==apply(0,1)-o
	linear2d_function(
		const image_location &o_,
		const image_location &x_,
		const image_location &y_)
			:o(o_), x(x_), y(y_) {}
	linear2d_function(void) :o(0,0), x(1,0), y(0,1) {}
	/** Return the inverse linear function, or abort if singular */
	linear2d_function inverse(void);
	/** Apply this function to this input point. */
	image_location apply(const image_location &i) const {
		return image_location(i.x*x.x+i.y*y.x+o.x,i.x*x.y+i.y*y.y+o.y);
	}
};
ASF_COREDLL void print(const asf::linear2d_function &v,FILE *f=NULL);


/**
 * A 2D, integer coordinates location.
 */
class ASF_COREDLL location_integer_2d {
public:
	/// 2D coordinates
	int x,y;
	
	location_integer_2d() :x(0), y(0) {}
	location_integer_2d(int nx,int ny) :x(nx), y(ny) {}
	void pup(PUP::er &p) {PUPn(x); PUPn(y);}
	
	bool operator==(const location_integer_2d &l) const 
		{return x==l.x && y==l.y;}
	bool operator!=(const location_integer_2d &l) const 
		{return x!=l.x || y!=l.y;}
	
};
ASF_COREDLL void print(const asf::location_integer_2d &v,FILE *f=NULL);

/** A bunch of types are basically just (x,y) pairs, but you don't
want them to be silently interconvertable.  Hence we declare them as 
subclasses of a location_integer_2d using this macro. */
#define location_integer_2d_class(className) \
class ASF_COREDLL className : public location_integer_2d { public: \
	className() {}  \
	className(int nx,int ny) :location_integer_2d(nx,ny) {} \
	explicit className(const location_integer_2d &l) \
		:location_integer_2d(l.x,l.y) {} \
};

/// A metadata pixel location. 
location_integer_2d_class(meta_pixel_location);
/// An alloc coordinates location.
location_integer_2d_class(alloc_pixel_location);
/// A pixel/tile coordinates pixel location (typical plugin's coordinate system)
location_integer_2d_class(pixel_location);

/// A rectangle's size, in pixels.
location_integer_2d_class(pixel_size);
/// A 2D ID for one tile in an xy array of tiles. 
location_integer_2d_class(tile_location);

/**
 * A 2D, integer coordinates axis-aligned rectangle of pixels.  
 *
 * Pixels inside the rectangle lie between
 *       x=lo_x .. (hi_x-1)  and    y=lo_y .. (hi_y-1)
 *  That is, lo_x<=hi_x, and lo_y<=hi_y.
 */
class ASF_COREDLL pixel_rectangle {
#undef min
#undef max  /* remove stupid windows.h macros */
	static inline int min(int a,int b) {return (a<b)?a:b;}
	static inline int max(int a,int b) {return (a>b)?a:b;}	
public:
	int lo_x,lo_y,hi_x,hi_y;///< Clip boundaries
	/// Build a rectangle with zero initial coordinates
	pixel_rectangle() { lo_x=lo_y=hi_x=hi_y=0; }
	/// Build a rectangle from the origin to this width and height.
	pixel_rectangle(int wid,int ht) 
		:lo_x(0),lo_y(0),hi_x(wid),hi_y(ht) { }
	/// Build a rectangle with this (lo_x,lo_y) and (hi_x,hi_y) corners.
	/// Remember, the rightmost, bottommost pixel will be (hi_x-1,hi_y-1).
	pixel_rectangle(int l,int t,int r,int b) 
		:lo_x(l),lo_y(t),hi_x(r),hi_y(b) { }
	/// Build a rectangle with these corners.
	/// Remember, the rightmost, bottommost pixel will be (hi.x-1,hi.y-1).
	pixel_rectangle(const location_integer_2d &lo,const location_integer_2d &hi) 
		:lo_x(lo.x),lo_y(lo.y),hi_x(hi.x),hi_y(hi.y) { }
	void pup(PUP::er &p) {PUPn(lo_x); PUPn(lo_y); PUPn(hi_x); PUPn(hi_y);}
	
	/// Move this rectangle by the shift p
	pixel_rectangle get_shift(const location_integer_2d &p) const {
		return pixel_rectangle(lo_x+p.x,lo_y+p.y,hi_x+p.x,hi_y+p.y);
	}
	pixel_rectangle operator+(const location_integer_2d &p) const {return get_shift(p);}
	
	/// Scale this rectangle down by a factor of sx and sy
	pixel_rectangle get_scale(int sx,int sy) const {
		return pixel_rectangle(lo_x/sx,lo_y/sy,1+(hi_x-1)/sx,1+(hi_y-1)/sy);
	}

	pixel_rectangle get_intersect(const pixel_rectangle &o) const {
		return pixel_rectangle(max(lo_x,o.lo_x),max(lo_y,o.lo_y),
			min(hi_x,o.hi_x),min(hi_y,o.hi_y));
	}
	pixel_rectangle get_inset(int shift) const {
		return pixel_rectangle(lo_x+shift,lo_y+shift,hi_x-shift,hi_y-shift);
	}
	bool operator==(const pixel_rectangle &b) const {
		return lo_x==b.lo_x && lo_y==b.lo_y && hi_x==b.hi_x && hi_y==b.hi_y;
	}
	
	location_integer_2d get_min(void) const {return location_integer_2d(lo_x,lo_y);}
	location_integer_2d get_max(void) const {return location_integer_2d(hi_x,hi_y);}
	
	bool is_empty(void) const {
		return (hi_x<=lo_x)||(hi_y<=lo_y);
	}
	inline int width(void) const {return hi_x-lo_x;}
	inline int height(void) const {return hi_y-lo_y;}
	inline pixel_size size(void) const {return pixel_size(width(),height());}
	inline double area(void) const {
		return width()*height();
	}
	
//Clip utility functions
	/// Return true if this x value is out of bounds
	inline bool oobX(int x) const 
	  {return (x<lo_x)||(x>=hi_x);}
	/// Return true if this y value is out of bounds
	inline bool oobY(int y) const 
	  {return (y<lo_y)||(y>=hi_y);}
	
	/// Return true if the integer point (x,y) lies inside this rectangle.
	inline bool inbounds(int x,int y) const {
		if (oobX(x)) return false;
		if (oobY(y)) return false;
		return true;
	}
	/// Return true if this integer Point lies inside this rectangle.
	inline bool inbounds(const pixel_location &p) const {
		return inbounds(p.x,p.y);
	}
	/// Return true if this rectangle lies entirely inside us.
	inline bool contains(const pixel_rectangle &r) const {
		return get_intersect(r)==r; /* intersecting r with us doesn't shrink r */
	}
};
ASF_COREDLL void print(const asf::pixel_rectangle &v,FILE *f=NULL);


/**
  Return the pixel located this far into this rectangle.
  (x,y) is in fractions of the rectangle, so 
    (0,0) is topleft, (1,1) is bottomright.
  Covers entire square pixel area of rectangle, including boundaries.
*/
inline asf::image_location rect_frac(const asf::pixel_rectangle &rect,double dx,double dy) 
{
	return asf::image_location(
		rect.lo_x+dx*(rect.hi_x-1-rect.lo_x),
		rect.lo_y+dy*(rect.hi_y-1-rect.lo_y)
	);
}

/** Loop over all pixels in this rectangle or image */
#define ASF_FOR_PIXELS(x,y,rect) \
	asf::pixel_rectangle rect_##x##y##__LINE__=(rect); \
	for (int y=rect_##x##y##__LINE__.lo_y;(y)<rect_##x##y##__LINE__.hi_y;(y)++) \
	for (int x=rect_##x##y##__LINE__.lo_x;(x)<rect_##x##y##__LINE__.hi_x;(x)++) 


class metadata_source; /* In asf/meta.h */

/**
  Describes all geometric and radiometric properties of an image.
*/
class ASF_COREDLL image_metadata {
public:
	metadata_source *src;
};


/******************** Image-Image Geometric Relationship *******************/

/**
 This class can compute the geometric relationship between locations
 in two images (the source and destination images).
*/
class ASF_COREDLL location_function {
public:
	/** Describes the geometric relationship this function computes. 
	   For pixel images, no resampling is needed for identity and translation relationships.
	   Simple summation is sufficent for a scaling function.
	   Image interpolation is required for general linear, smooth, 
	     or rough functions.
	*/
	typedef enum {
	/// Identity: Pixels of both images cover exactly the same ground
		identity=0, 
	/// Translation: Pixels are shifted by an INTEGRAL number of pixels 
	/// from one another, but same size   
		translation=1, 
	/// Scaling: Pixels are different sizes by some integral factor, 
	///  but have same axes
		scaling=2,   
	/// Linear: Pixel axes have a linear 2D (2x2 matrix+offset) relationship.  
		linear=3,     
	/// Smooth: Pixels are related nonlinearly, but still smoothly 
	///   (e.g., a map projection) 
		smooth=4, 
	/// Rough: Pixels are related, but in a highly nonlinear way 
	///   (e.g., terrain correction)
		rough=5
	/// If pixels are not related at all, then don't use this class.
	} relationship_t;
	
	/// Return the class of relationship this function computes.
	inline relationship_t getRelationship(void) const {return relationship;}
	
	/**
	  Apply this function to this location.
	  All subclasses must override this method,
	   but don't need to override anything else.
	*/
	virtual asf::image_location apply(const asf::image_location &loc) =0;
	
	/**
	  Return the smallest pixel rectangle that contains 
	  	apply(loc)
	  for all locations in this source rectangle.
	  The default implementation transforms the corners for up to 
	  linear mappings, and walks a dense set of edge points for
	  smooth and rough mappings.  Subclasses may override this method, 
	  but the vast majority will never need to.
	*/
	virtual asf::pixel_rectangle apply_rectangle(asf::pixel_rectangle rect);
	
	/**
	  Create a new inverse mapping function.  It is always the case that 
	  (up to roundoff, and for non-singular x):
	         inverse->apply(this->apply(x))==x.
	    and  this->apply(inverse->apply(x))==x.
	  The default implementation finds and inverts the matrix for up to 
	  linear relationships, and creates a relatively fast iterative search 
	  object to invert more complex relationships.
	  The default implementation is probably good enough for most uses, 
	  although map projections might be a bit faster by creating an 
	  explicit inverse here.
	*/
	virtual asf::location_function *make_inverse(void);
	
	/**
	  Return a linear approximation to this function, using origin at 
	  o and axes with lengths l.x (x axis length) and l.y (y axis length).
	*/
	virtual linear2d_function linearize(const image_location &o,
		image_location l=image_location(1.0,1.0));
	
	virtual ~location_function();
	
protected:
	/// Don't directly create location_function objects; create only its subclasses.
	location_function(relationship_t rel);
	
	/// Class of relationship this function computes.
	relationship_t relationship;
};

/**  Indicates two images have exactly the same coordinates.  */
class ASF_COREDLL location_function_identity : public location_function {
public:
	location_function_identity(void) :location_function(identity) {}
	/// Returns loc (unmodified)
	image_location apply(const image_location &loc);
};

/**  Indicates two images are shifted by an INTEGRAL number of pixels  */
class ASF_COREDLL location_function_translation : public location_function {
public:
	location_function_translation(pixel_location offset_) 
		:location_function(translation), offset(offset_) {}
	/// Returns loc+offset
	image_location apply(const image_location &loc);
	/// Returns offset
	pixel_location get_offset(void) const {return offset;}
private:
	pixel_location offset;
};

/**  Indicates two images are shifted by a 2d linear function  */
class ASF_COREDLL location_function_linear : public location_function {
public:
	location_function_linear(const linear2d_function &m_) 
		:location_function(linear), m(m_) {}
	/// Returns m.apply(loc)
	image_location apply(const image_location &loc);
	/// Returns linear function
	const linear2d_function &get_matrix(void) const {return m;}
private:
	linear2d_function m;
};

/**  Find inverse of (nonlinear) function as the inverse of the given function.
  CURRENTLY NONFUNCTIONAL--a FIXME placeholder only.
*/
class ASF_COREDLL location_function_inverse_search : public location_function {
public:
	location_function_inverse_search(location_function *srcFmDest_); 
	/// Returns point dest such that srcFmDest->apply(dest)==src.
	image_location apply(const image_location &src);
private:
	/// Mapping from destination to source
	location_function *srcFmDest;
	/// Last-used point correspondence (used in search).
	image_location src,dest;
	/// Local linearization of function near src point.
	linear2d_function lin;
	/// Re-linearize function about this point.
	void relinearize(const image_location &src);
};


/********************** Image Storage *********************/
/**
  The most generic possible image class: an interpolated
  pixel source.  This class is abstract.
*/
class ASF_COREDLL parameter_interpolated_image : public parameter {
	/** Number of bands (color channels per pixel) in the image */
	int n_band;
	/** Metadata for this image */
	image_metadata *meta;
public:
	static const type static_type;
	parameter_interpolated_image(PupMigrateMessage *m);
	parameter_interpolated_image(void);
	~parameter_interpolated_image();
	void pup(PUP::er &p);
	
	/// Set the number of bands (color channels per pixel) in this image.
	virtual void set_bands(int nBands_);
	
	/// Return the number of bands (color channels per pixel)
	inline int bands(void) const {return n_band;}
	
	/// Return this image's metadata. Images begin with empty metadata.
	image_metadata *metadata(void) {return meta;}
	const image_metadata *metadata(void) const {return meta;}
	
	/**
	  Extract the interpolated pixel value of the image at this pixel 
	  location.  By convention, (0.0,0.0) is the top-left corner of 
	  the top-left pixel; (0.5,0.5) is the center of the top-left pixel.
	  The band number must be between 0 and getBands()-1.
	  The (x,y) need NOT be in-bounds on the image.
	*/
	virtual float interpolate(double x,double y,int band) const =0;
};

/**
 Describes a highly restricted 2D pixel coordinate system: a
 clipped and power-of-two zoomed out window of a higher-resolution 
 (source) coordinate system.  This is used to describe the allocated
 region of an image, or the viewable tile of an image.

  "zoom" is the log-base-2 of the zoom-out factor to source coordinates.
   zoom==0 means no zooming (1x scale); zoom==1 is 2x zoomout; zoom=2 is 4x; etc.
  In OpenGL, "zoom" is just the mipmap level.
*/
class pixel_viewport {
public:
	pixel_viewport();
	/** Build a viewport to show this source rectangle, zoomed out by zoom factor (log) */
	pixel_viewport(const pixel_rectangle &src,int zoom=0)
		{set(src,zoom);}
	/** Build a viewport to show this source rectangle, zoomed out by zoom factor (log) */
	void set(const pixel_rectangle &src,int zoom=0);
	void pup(PUP::er &p);
	
	/** Return our bounding rectangle in source coordinates */
	inline const pixel_rectangle &source_rect(void) const {return m_src;}
	
	/** Return our log-base-2 zoom factor. */
	inline int zoom(void) const {return m_zoom;}

	/** Return our bounding rectangle in our own (self) coordinates. 
	   By definition, lo_x and lo_y are zero.
	*/
	inline const pixel_rectangle &self_rect(void) const {return m_self;}
	
	/** Return our size, in self-coordinates pixels */
	inline const pixel_size &size(void) const {return m_size;}
	inline int size_x(void) const {return m_size.x;}
	inline int size_y(void) const {return m_size.y;}
	inline int width(void) const {return m_size.x;}
	inline int height(void) const {return m_size.y;}
	
	/** Return true if this self-coordinates pixel is in-bounds */
	bool inbounds(const location_integer_2d &sel) const {return inbounds(sel.x,sel.y);}
	bool inbounds(int x,int y) const {
		return ((unsigned int)x<(unsigned int)m_size.x)
		     &&((unsigned int)y<(unsigned int)m_size.y);
	}
	
	/// Return self coordinates given source coordinates (integer coordinates)
	inline location_integer_2d to(const location_integer_2d &src) const {
		return location_integer_2d(
			(src.x-m_src.lo_x)>>m_zoom, /* FIXME: bias? */
			(src.y-m_src.lo_y)>>m_zoom
		);
	}
	/// Return self coordinates given source coordinates (float coordinates)
	inline image_location to(const image_location &src) const {
		return image_location(
			(src.x-m_src.lo_x)*m_zoomInv,
			(src.y-m_src.lo_y)*m_zoomInv
		);
	}
	
	/// Return source coordinates given self coordinates (integer coordinates)
	inline location_integer_2d from(const location_integer_2d &self) const {
		return location_integer_2d(
			(self.x<<m_zoom)+m_src.lo_x,
			(self.y<<m_zoom)+m_src.lo_y
		);
	}
	/// Return source coordinates given self coordinates (float coordinates)
	inline image_location from(const image_location &self) const {
		return image_location(
			self.x*m_zoomScale+m_src.lo_x,
			self.y*m_zoomScale+m_src.lo_y
		);
	}
	
private:
	/// Bounding rectangle in source coordinate system.
	pixel_rectangle m_src;
	/// Bounding rectangle in self coordinate system.
	///  By definition, lo_x and lo_y are always zero.
	pixel_rectangle m_self;
	/// Dimensions in self coordinate system (i.e., number of pixels).
	pixel_size m_size;
	
	/// Factor by which self is zoomed-out from src, in log-base-2 format.
	///   zoom==0 => no zoom (1x); zoom==1 => 2x zoomout; 2 => 4x; 3 => 8x, etc.
	int m_zoom;
	/// Actual multiplication factor to convert self coordinates to src coordinates.
	///  == 1<<m_zoom
	double m_zoomScale;
	/// Multiplication factor to convert src coordinates to self coordinates
	double m_zoomInv;
	/// Set zoom factor
	void set_zoom(int z) {m_zoom=z;m_zoomScale=1<<m_zoom; m_zoomInv=1.0/m_zoomScale;}
};

/**
 A slightly less generic image class: an integral-pixel source.

There are two separate coordinate systems supported here:
  - Meta coordinates: final image pixels, referenced to image metadata.
      Meta coordinates are what the "interpolate" routine above accesses.
  
  - Pixel/tile coordinates: a zoomed/clipped subwindow of meta coordinates that are
     being accessed by a particular plugin. 

These coordinate systems exist so we can conveniently chop images into 
little pieces, and process pieces independently.  "meta" is the theoretical
reassembled image we're working on.  "pixel/tile" is the piece we're working 
on right now (which may not be the whole thing).
*/
class ASF_COREDLL parameter_pixel_image : public parameter_interpolated_image
{
protected:
	/** Our image's coverage in meta coordinates. */
	pixel_rectangle m_meta_bounds;
	/** Pixel tile, expressed in meta coordinates */
	pixel_viewport pixel_meta;
public:
	static const type static_type;
	parameter_pixel_image(PupMigrateMessage *m);
	parameter_pixel_image(void);
	void pup(PUP::er &p);

// Meta coordinates
	/// Set metadata for a (theoretical) image of this overall final pixel size.
	///   Unlike pixel_setsize, this actually allocates no storage.
	/// This routine is normally called by image-handling plugins themselves.
	virtual void meta_setsize(int nBands_,const pixel_rectangle &meta_bounds);
	
	/// Return our entire (theoretical) image's bounding region in meta coordinates.
	inline const pixel_rectangle &total_meta_bounds(void) const {return m_meta_bounds;}
	
// Pixel/tile coordinates:
	/**
	  Allocate pixel storage for this many meta pixels but at this zoom level.
	    This version actually allocates some storage space, and is hence slow
	    unless the overall allocated size is unchanged.
	    "zoom_meta" is the log-base-2 of the zoom-out factor to meta coordinates.
	     zoom==0 means no zooming (1x scale); zoom==1 is 2x zoomout; zoom=2 is 4x; etc.
	    In OpenGL, "zoom" is just the mipmap level.
	    Region *must* be part of our meta_bounds, or an error is thrown.
	    This routine accomplishes pixel_setsize, so if you've allocated the 
	    space you want to use, it's ready to access.
	*/
	virtual void pixel_setsize(const pixel_rectangle &pixel_meta,int zoom_meta=0);
	
	/// Return the viewport that expresses zoomed-out tile pixels 
	///   in meta coordinates (source).
	///  For example, pixel().size_x() is our number of x pixels.
	inline const pixel_viewport &pixel(void) const {return pixel_meta;}
	
	/// Return our image's tile bounding region in meta coordinates.
	inline const pixel_rectangle &pixel_meta_bounds(void) const 
		{return pixel().source_rect();}
	/// Return our image's pixel bounding region in pixel coordinates.
	inline const pixel_rectangle &pixels(void) const 
		{return pixel().self_rect();}

	/**
	  Points us at this existing image.  After this call, the two images share
	    the same data, so reads and writes go to the same place in memory.
	  We will never delete this image, so src must be deleted after us.
	  This is a very fast call, since it doesn't actually move or allocate anything.
	*/
	virtual void pixel_pointat(parameter_pixel_image *src,const pixel_rectangle &pixel_meta,int zoom_meta=0) =0;

// Convenience routines:
	/// Convenience routine: wrap up a call to meta_setsize, alloc_setsize, 
	///  and pixel_setsize for a simple flat (unzoomed, unclipped) image.
	void flat_setsize(int nbands,const pixel_rectangle &r) {
		meta_setsize(nbands,r); pixel_setsize(r,0);
	}
	void flat_setsize(int nbands,int w,int h) 
		{ flat_setsize(nbands,pixel_rectangle(w,h));}
	
	/// Return meta coordinates given pixel coordinates (integer coordinates)
	inline meta_pixel_location meta_from_pixel(const pixel_location &p)  const
		{return meta_pixel_location(pixel().from(p));}
	/// Return meta coordinates given pixel coordinates (float coordinates)
	inline image_location meta_from_pixel(const image_location &p) const
		{return pixel().from(p);}
	
	/// Return pixel coordinates given meta coordinates (integer coordinates)
	inline pixel_location pixel_from_meta(const meta_pixel_location &p)  const
		{return pixel_location(pixel().to(p));}
	/// Return pixel coordinates given meta coordinates (float coordinates)
	inline image_location pixel_from_meta(const image_location &p) const
		{return pixel().to(p);}
	
	/**
	  Return the x and y size of this image, in pixels.
	*/
	inline int size_x(void) const {return pixel().size_x();}
	inline int size_y(void) const {return pixel().size_y();}

// Pixel access
	/**
	  Default interpolation coordinate system is meta coordinates,
	  and default interpolation is bilinear.
	*/
	virtual float interpolate(double x,double y,int band) const;
	
	/**
	  Extract the pixel value of the image at this location.  
	  By convention, (0,0) is the top-left pixel.
	  The band number must be between 0 and getBands()-1.
	  The (x,y) might NOT be in-bounds on the image (Out Of Bounds ok)
	  the default implementation returns 0.0 for out-of-bounds pixels.
	*/
	virtual float get_pixelOOB(int x,int y,int band) const;
	
	/**  Set the pixel value of the image at this location. */
	virtual void set_pixelOOB(int x,int y,int band,float to);
	
	/// Like get_pixelOOB, but guaranteed to be in-bounds.
	virtual float get_pixel(int x,int y,int band) const =0;
	
	/// Like set_pixelOOB, but guaranteed to be in-bounds.
	virtual void set_pixel(int x,int y,int band,float to) =0;
};

/**
Finally, a concrete image class!
Stores & accesses pixel data as floats.
  Pixel (x,y) band b is stored at
  	data[xd*x+yd*y+b]
For example:
  xd=1, yd=x_size is a simple row-major monochrome image.
  xd=y_size, yd=1 is a column-major monochrome image.
  xd=n, yd=n*x_size is a row-major n-band image.
  xd=k*n, yd=k*n*x_size is zoomed out by a factor of k.
*/
class ASF_COREDLL parameter_float_image : public parameter_pixel_image {
public:
	/// Our superclass's name
	typedef parameter_pixel_image super;
	
	/// This image's pixel's bands are represented as floats.
	typedef float band_t;
	
	parameter_float_image(PupMigrateMessage *m);
	void pup(PUP::er &p);
	parameter_float_image(void);
	~parameter_float_image();
	ASF_parameter_class(parameter_float_image);
	
	/// Allocate this many pixels.
	virtual void pixel_setsize(const pixel_rectangle &pixel_meta,int zoom_meta=0);

	/// Points us at this existing float image.  After this call, the two images share
	///   the same data, so reads and writes go to the same place in memory.
	/// We will never delete this image, so src must be deleted after us.
	virtual void pixel_pointat(parameter_pixel_image *src,const pixel_rectangle &pixel_meta,int zoom_meta=0);
	
	/// Return a pointer to the first pixel's first band.
	inline band_t *pixel_data(void) const {return data;}
	/// Return the distance in band_t's between pixel (x,y) and (x+1,y)
	inline int dist_x(void) const {return xd;}
	/// Return the distance in band_t's between pixel (x,y) and (x,y+1)
	inline int dist_y(void) const {return yd;}

	/// Get or set generic pixels.  Provided for parameter_pixel_image compatability.
	///  Probably just about as fast as "at" routines.
	inline float get_pixel(int x,int y,int band) const {return at(x,y,band);}
	inline void set_pixel(int x,int y,int band,float to) {at(x,y,band)=to;}
	
	inline band_t at(const pixel_location &p,int band=0) const {return at(p.x,p.y,band);}
	inline band_t &at(const pixel_location &p,int band=0) {return at(p.x,p.y,band);}
#ifndef ASF_CAREFUL /* No bounds checks-- much better performance */
	/// Return this in-bounds pixel for read or write access.
	///  These routines are very fast, and should be the main way to access pixels.
	inline band_t at(int x,int y,int band=0) const {return data[x*xd+y*yd+band];}
	inline band_t &at(int x,int y,int band=0) {return data[x*xd+y*yd+band];}
#else /* defined(ASF_CAREFUL): do extra error checking */
	inline band_t at(int x,int y,int band=0) const {return at_careful(x,y,band);}
	inline band_t &at(int x,int y,int band=0) {return at_careful(x,y,band);}
#endif
	/// Error-checking pixel access.  Aborts if x,y, or band are out of bounds.
	band_t &at_careful(int x,int y,int band=0) const;
	
	/// Set all pixels to this value.
	void set(float toValue); 
	
private:
	/// Source of pixel data:
	typedef enum {
		src_none=0, // No pixel data present.
		src_ext=1, // Data passed in from outside (& will be deallocated there)
		src_alloc=2 // Data dynamically allocated by us
	} src_t;
	src_t src;
	/// Pointer to pixel data: pixel (0,0) band 0
	band_t *data;
	/// Reallocate us so we point to this data:
	void data_alloc(src_t src,band_t *data,int xd_,int yd_);
	void data_free(void);
	/// Distance (in band_t's) between pixel (x,y) and (x+1,y)
	///  AKA "pixel size".
	int xd;
	/// Distance (in band_t's) between pixel (x,y) and (x,y+1).
	///  AKA "row size".
	int yd;
	/// FIXME: assumes band_dist==1;  i.e., that the data
	///  for all the bands of a pixel are stored contiguously.
	
	// Clear all fields
	void init(void);
};


/**************** Image Size Constraints *************/
/**
  Constrains size of a parameter_image.
*/
class ASF_COREDLL parameter_image_constraint : public parameter_constraint {
public:
	/* Returns that this constraint applies to pixel images:
		returns &asf::parameter_pixel_image::static_type
	*/
	virtual const type *get_type(void);
	
	/**
	  Given that the entire image is ix by iy pixels,
	  return (in *ox and *oy) the subimage size you're willing
	  to process at one time.  *ox and *oy on input contain a suggested
	  pixel size, so it'd be best to stay close to that size if possible.
	  *oz is the zoom shift we'll be processing at, which normally shouldn't
	  affect this decision in any way.
	*/
	virtual void constrain_subimage(int ix,int iy,int *ox,int *oy,int *oz) =0;
};

/** Constrains the processed subimage to consist only of entire 
scanlines of the entire image--no line will be divided.  This is 
most commonly used for I/O.  Does not allow lines to be zoomed out.
*/
class ASF_COREDLL parameter_scanline_constraint : public parameter_image_constraint {
public:
	/** Return an instance of this constraint.
	   Note that since this object has no locals, there only ever
	   needs to be one instance... */
	static parameter_scanline_constraint *instance(void);
	
	/** Sets *ox=ix, and *oz=0.  This ensures raw scanline access */
	virtual void constrain_subimage(int ix,int iy,int *ox,int *oy,int *oz);
	virtual void print(void);
};

/** Constrains the processed subimage to consist only of 
the entire image--no subdivisions are allowed.
*/
class ASF_COREDLL parameter_wholeimage_constraint : public parameter_image_constraint {
public:
	/** Return an instance of this constraint.
	   Note that since this object has no locals, there only ever
	   needs to be one instance... */
	static parameter_wholeimage_constraint *instance(void);
	
	/** Sets *ox == ix and *oy==iy */
	virtual void constrain_subimage(int ix,int iy,int *ox,int *oy,int *oz);
	virtual void print(void);
};


/****************** Image-processing Plugin Superclasses *****************/

/**
 A "pixel_filter" plugin--a plugin that doesn't change the location
 of pixels; only their values.  Subclasses just need to implement
 the "execute" method to compute the "dest" image pixels as a function
 of the "src" image pixels.
*/
class ASF_COREDLL plugin_pixel_filter : public asf::plugin {
public:
	asf::parameter_float_image *src; /* Image to read from (input image) */
	asf::parameter_float_image *dest; /* Image to write to (output image) */
	
	plugin_pixel_filter(asf::plugin_parameters &param);
	
	/**
	 Initialize the output image metadata based on the 
	 input image metadata.
	*/
	virtual void meta_execute(void);
	
	/**
	 input and output cover the same space, 
	 so the location_function is an identity.
	*/
	virtual location_function *image_in_from_out(int i,int o);
	
	static const asf::type static_type;
};

/**
 A "pixel_kernel" plugin--a plugin that computes each output pixel
 based on a fixed-size neighborhood ("kernel") of surrounding input pixels.
 Note that in the "execute" method, you compute *all* the dest pixels based on 
 a *shifted* copy of the source pixels--the source-from-dest transform is
 	source_x=dest_x-neighborhood.lo_x;
 	source_y=dest_y-neighborhood.lo_y;
*/
class ASF_COREDLL plugin_pixel_kernel : public asf::plugin_pixel_filter {
public:
	plugin_pixel_kernel(asf::plugin_parameters &param);
	
	virtual void meta_execute(void);
	virtual location_function *image_in_from_out(int i,int o);
	
	
	/**
	  This class describes the geometry of a kernel--the kernel's extent in space.
	*/
	class kernel_geometry {
	public: 
	/**
	  "neighborhood" is the bounding box centered on (0,0) of our input source pixels.
	    For example, *neighborhood=asf::pixel_rectangle(-1,-1,1,1); is a 3x3 kernel.
	*/
		asf::pixel_rectangle neighborhood;
	/**
	 "scale" is multiplied by output coordinates to get input coordinates.
	    By default, scale is 1.0,1.0, which means input is the same as output.
	    A scale of 3.0,3.0 means the output is 1/3 the size of the input.
	*/
		osl::Vector2d scale;
		kernel_geometry() :scale(1.0,1.0) {}
	};
	
	/**
	 Return the geometry of the pixels we need to compute an output pixel.
	*/
	virtual kernel_geometry get_kernel_geometry(void) =0;
	
	static const asf::type static_type;
};


}; /* End ASF namespace */

ASF_parameter_header(asf::parameter_float_image);


#endif
