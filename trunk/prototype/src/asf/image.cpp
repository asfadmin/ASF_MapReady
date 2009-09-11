/*
ASF Plugin Architecture
PROTOTYPE 1-L

Definitions used for image processing: specifically, 
image tiles.

Orion Sky Lawlor, olawlor@acm.org, 2005/09/08.
*/
#include "asf/image.h"
#include <math.h> /* for floor() */

ASF_COREDLL void asf::print(const asf::image_location &v,FILE *f)
{
	if (!f) f=stdout;
	fprintf(f,"(%.2f,%.2f)\n",v.x,v.y);
}
ASF_COREDLL void asf::print(const asf::location_integer_2d &v,FILE *f)
{
	if (!f) f=stdout;
	fprintf(f," pixel_location(%d,%d)\n",v.x,v.y);
}
ASF_COREDLL void asf::print(const asf::pixel_rectangle &v,FILE *f)
{
	if (!f) f=stdout;
	fprintf(f," rectangle(%d,%d) to(%d,%d) size(%d,%d)\n",
		v.lo_x,v.lo_y,v.hi_x,v.hi_y, v.hi_x-v.lo_x,v.hi_y-v.lo_y);
}
ASF_COREDLL void asf::print(const asf::linear2d_function &v,FILE *f)
{
	if (!f) f=stdout;
	fprintf(f,"  origin(%.2f,%.2f)  x(%g,%g)  y(%g,%g) skew(%g)\n",
		v.o.x,v.o.y, v.x.x,v.x.y, v.y.x,v.y.y, v.x.cosAng(v.y));
}

ASF_COREDLL asf::image_location asf::half_pixel_bias(0.5,0.5);

/************* Image Geometric Relationships *****************/

/** Return the inverse linear function, or abort if singular */
asf::linear2d_function asf::linear2d_function::inverse(void) {
	asf::linear2d_function ret;
	double det=x.x*y.y-x.y*y.x;
	if (det==0) asf::die("asf::linear2d_function::inverse called on singular matrix");
	det=1.0/det;
	ret.x.x=det*y.y;
	ret.x.y=-det*x.y;
	ret.y.x=-det*y.x;
	ret.y.y=det*x.x;
	ret.o=-ret.apply(o); /* so ret.apply(o) gives zero */
	return ret;
}

/// Return d rounded to an integer.  Aborts if rounding is not exact.
int round_exact(double d) {
	int i=(int)d;
	if (i!=d) asf::die("location_function_translation distance must be an integral number of pixels");
	return i;
}

asf::pixel_location pixel_fm_loc(const asf::image_location &loc) {
	return asf::pixel_location(round_exact(loc.x),round_exact(loc.y));
}

asf::location_function *asf::location_function::make_inverse(void)
{
	switch (relationship) {
	case identity: return new location_function_identity();
	case translation: return new location_function_translation(pixel_fm_loc(-apply(image_location(0,0))));
	case scaling: case linear: 
		return new location_function_linear(linearize(image_location(0,0)).inverse());
	default: /* nonlinear--just search */
		return new location_function_inverse_search(this);
	};
	return 0;
}


/**
  Collect axis-aligned bounding box for these points.
*/
class bbox2d {
public:
	asf::image_location lo,hi;
	bbox2d() :lo(1.0e99,1.0e99), hi(-1.0e99,-1.0e99) {}
	void add(const asf::image_location &loc) {
		for (int axis=0;axis<2;axis++) {
			if (loc[axis]<lo[axis]) lo[axis]=loc[axis];
			if (loc[axis]>hi[axis]) hi[axis]=loc[axis];
		}
	}
	/// Return the smallest pixel rectangle that covers all our points, leaving this many
	///  pixels to spare.
	asf::pixel_rectangle getRect(double spare=0.0);
};

asf::pixel_rectangle bbox2d::getRect(double spare) {
	asf::pixel_rectangle ret;
	ret.lo_x=(int)floor(lo.x-spare);
	ret.lo_y=(int)floor(lo.y-spare);
	ret.hi_x=1+(int)ceil(hi.x+spare);
	ret.hi_y=1+(int)ceil(hi.y+spare);
	return ret;
}


asf::pixel_rectangle asf::location_function::apply_rectangle(pixel_rectangle rect)
{
	switch (relationship) {
	case identity: return rect;
	case translation: case scaling: case linear: {
		// Map corner points
		bbox2d b; 
		for (int dy=0;dy<=1;dy++)
		for (int dx=0;dx<=1;dx++)
			b.add(apply(rect_frac(rect,dx,dy)));
		return b.getRect(0.0);
		}
	default: { /* nonlinear transform: */
		// Map edge points as well
		bbox2d b; 
		enum {pointsPerEdge=32};
		/* FIXME: a fixed number of points along each edge is silly.
		   Right approach is to use Lagrange multipliers.
		   Easier is to use a recursive expansion, with curvature detection.
		*/
		for (double frac=0;frac<1.0;frac+=(1.0/pointsPerEdge)) {
			b.add(apply(rect_frac(rect,frac,0)));
			b.add(apply(rect_frac(rect,1,frac)));
			b.add(apply(rect_frac(rect,1-frac,1)));
			b.add(apply(rect_frac(rect,0,1-frac)));
		}
		return b.getRect(relationship==smooth?3.0:50.0);
		}
	};
}

asf::linear2d_function asf::location_function::linearize(
	const image_location &o,image_location l)
{
	image_location mo=apply(o);
	image_location mx=(apply(o+image_location(l.x,0))-mo)/l.x;
	image_location my=(apply(o+image_location(0,l.y))-mo)/l.y;
	return linear2d_function(mo-o.x*mx-o.y*my,mx,my);
}
	
asf::location_function::~location_function() {}
	
/// Don't directly create image_location_map objects; create only its subclasses.
asf::location_function::location_function(relationship_t rel)
	:relationship(rel) {}

asf::image_location asf::location_function_identity::apply(const image_location &loc) {
	return loc;
}
asf::image_location asf::location_function_translation::apply(const image_location &loc) {
	return image_location(loc.x+offset.x,loc.y+offset.y);
}
asf::image_location asf::location_function_linear::apply(const image_location &loc) {
	return m.apply(loc);
}


asf::location_function_inverse_search::location_function_inverse_search(
	location_function *srcFmDest_) 
	:location_function(srcFmDest_->getRelationship()), srcFmDest(srcFmDest_) { }

/// Returns point dest such that srcFmDest->apply(dest)==src.
asf::image_location asf::location_function_inverse_search::apply(const image_location &src) {
	die("FIXME: asf::location_function_inverse_search::apply not yet implemented");
	return src;
}

/************* interpolated image *************/
/* static */ const asf::type asf::parameter_interpolated_image::static_type(
  /* parent  */ &asf::parameter::static_type,
  /* name    */ "interpolated_image",
  /* desc    */ "An interpolated pixel source",
  /* authors */ "v0.1 by Orion Lawlor (olawlor@acm.org) 2005/10/12",
  /* version */ 0.1
);

asf::parameter_interpolated_image::parameter_interpolated_image(PupMigrateMessage *m)
	:parameter(m),meta(new image_metadata) {}
asf::parameter_interpolated_image::parameter_interpolated_image(void) 
	:n_band(0),meta(new image_metadata)
{}

void asf::parameter_interpolated_image::set_bands(int nBands_)
{
	n_band=nBands_;
}
void asf::parameter_interpolated_image::pup(PUP::er &p)
{
	parameter::pup(p);
	PUPn(n_band);
}
asf::parameter_interpolated_image::~parameter_interpolated_image() {
	delete meta;
}

/************** pixel viewport ****************/
asf::pixel_viewport::pixel_viewport() 
	:m_size(0,0)
{
	set_zoom(0);
}
/** Build a viewport to show this source rectangle, zoomed out by zoom factor */
void asf::pixel_viewport::set(const asf::pixel_rectangle &src,int zoom)
{
	set_zoom(zoom);
	m_src=src;
	m_size=pixel_size(m_src.width()>>m_zoom, m_src.height()>>m_zoom);
	m_self=pixel_rectangle(0,0,m_size.x,m_size.y);
	
}
void asf::pixel_viewport::pup(PUP::er &p) {
	PUPn(m_src);
	PUPn(m_zoom);
	set(m_src,m_zoom);
}

/************* pixel image *************/
/* static */ const asf::type asf::parameter_pixel_image::static_type(
  /* parent  */ &asf::parameter_interpolated_image::static_type,
  /* name    */ "pixel_image",
  /* desc    */ "A pixel source",
  /* authors */ "v0.1 by Orion Lawlor (olawlor@acm.org) 2005/10/12",
  /* version */ 0.1
);

asf::parameter_pixel_image::parameter_pixel_image(PupMigrateMessage *m)
	:parameter_interpolated_image(m) 
{}

asf::parameter_pixel_image::parameter_pixel_image(void)
{}

void asf::parameter_pixel_image::meta_setsize(int nBands_,const pixel_rectangle &meta_bounds)
{
	parameter_interpolated_image::set_bands(nBands_);
	m_meta_bounds=meta_bounds;
}

void asf::parameter_pixel_image::pixel_setsize(const pixel_rectangle &pixel_rect,int zoom)
{
	if (!m_meta_bounds.contains(pixel_rect)) {
		printf("Our meta bounds: "); asf::print(m_meta_bounds); 
		printf("Requested pixel rect: "); asf::print(pixel_rect);
		asf::die("asf::parameter_pixel_image::pixel_setsize asked to access pixels not inside our meta bounds!\n");
	}
	if (zoom<0) asf::die("asf::parameter_pixel_image::pixel_setsize zoom factor cannot be negative!\n");
	
	pixel_meta=pixel_viewport(pixel_rect,zoom);
}

void asf::parameter_pixel_image::pup(PUP::er &p) {
	parameter_interpolated_image::pup(p);
	PUPn(m_meta_bounds); 
	PUPn(pixel_meta);
}
/*
  Linear interpolation: if dx==0, return left;
    if dx==1, return right; else return blend.
*/
float lerp(float dx,float left,float right) {
	return left+dx*(right-left);
}
/* Bilinear interpolation: default "interpolate".
   FIXME: split off separate "parameter_interpolant" here...
*/
float asf::parameter_pixel_image::interpolate(double x,double y,int band) const {
	/* Convert meta x,y to pixel x,y */
	image_location p=pixel_from_meta(image_location(x,y));
	x=p.x-0.5; y=p.y-0.5; /* Shift pixel *center* to (0,0) */
	int ix=(int)floor(x), iy=(int)floor(y);
	double dx=x-ix, dy=y-iy;
	return lerp(dy, 
	  lerp(dx, get_pixelOOB(ix,iy  ,band),get_pixelOOB(ix+1,iy  ,band)),
	  lerp(dx, get_pixelOOB(ix,iy+1,band),get_pixelOOB(ix+1,iy+1,band))
	);              
}
float asf::parameter_pixel_image::get_pixelOOB(int x,int y,int band) const
{
	if (pixels().inbounds(x,y)) return get_pixel(x,y,band);
	else return 0.0;
}
	
void asf::parameter_pixel_image::set_pixelOOB(int x,int y,int band,float to)
{
	if (pixels().inbounds(x,y)) set_pixel(x,y,band,to);
	/* else do nothing-- not a pixel we cover */
}

/************* float image *************/
/* static */ const asf::type asf::parameter_float_image::static_type(
  /* parent  */ &asf::parameter_float_image::static_type,
  /* name    */ "float_image",
  /* desc    */ "An image consisting of floating-point pixels",
  /* authors */ "v0.1 by Orion Lawlor (olawlor@acm.org) 2005/10/12",
  /* version */ 0.1
);

asf::parameter_float_image::parameter_float_image(PupMigrateMessage *m)
	:parameter_pixel_image(m) {init();}
void asf::parameter_float_image::pup(PUP::er &p) {
	parameter_pixel_image::pup(p);
	// Allocate pixel data (if needed)
	if (p.isUnpacking()) data_alloc(src_alloc,0,bands(),bands()*pixel().size_x());
	// Pup all pixel data (FIXME: speed up contiguous case?)
	ASF_FOR_PIXELS(x,y,pixels()) 
		p(&at(x,y,0),bands());
}
asf::parameter_float_image::parameter_float_image(void)  {init();}

void asf::parameter_float_image::init(void) {
	src=src_none; /* no data */
	data=0;
	xd=yd=0;
}

/// Allow access to these pixels.
void asf::parameter_float_image::pixel_setsize(const pixel_rectangle &pixel_meta,int zoom_meta)
{
	super::pixel_setsize(pixel_meta,zoom_meta);
	int new_dx=bands(), new_dy=bands()*pixel().size_x();
	if ((data!=0) && (new_dx==xd) && (new_dy==yd))
		{ /* then re-use existing buffer--it's still OK. */ }
	else { /* Have to allocate a new buffer */
		data_alloc(src_alloc,0,new_dx,new_dy);
	}
}

void asf::parameter_float_image::pixel_pointat(parameter_pixel_image *src_p,const pixel_rectangle &pixel_meta,int zoom_meta)
{
	parameter_float_image *src=dynamic_cast<parameter_float_image *>(src_p);
	if (src==NULL) {
		asf::die("asf::parameter_float_image::pixel_pointat called with a non-float image.  You can only point at an image of the same type!\n");
	}
	if (!src->pixel_meta_bounds().contains(pixel_meta)) {
		printf("Source image pixel bounds: "); asf::print(src->pixel_meta_bounds()); 
		printf("Requested pixel bounds: "); asf::print(pixel_meta);
		asf::die("asf::parameter_float_image::pixel_pointat asked to point at pixels not inside source image!\n");
	}
	super::pixel_setsize(pixel_meta,zoom_meta);
	
	set_bands(src->bands());
	
	/// Check the zoom factor
	int rz=pixel().zoom()-src->pixel().zoom(); /* relative zoom from alloc to pixel */
	if (rz<0) asf::die("asf::parameter_float_image::pixel_pointat zoom factor cannot be less than the source image's zoom!\n");
	
	/// Figure out our pixel region's topleft in source coordinates
	pixel_location src_tl=src->pixel_from_meta(asf::meta_pixel_location(pixel().source_rect().get_min()));
	data_alloc(src_ext, &src->at(src_tl),src->xd<<rz,src->yd<<rz);
}

/*
/// Allocate new image:
void asf::parameter_float_image::reallocate(int nBands_,int x_size_,int y_size_)
{
	parameter_pixel_image::reallocate(nBands_,x_size_,y_size_);
	data_alloc(src_alloc,0,bands(),bands()*size_x());
}
void asf::parameter_float_image::reallocate(int nBands_,int x_size_,int y_size_,band_t *data_)
{
	parameter_pixel_image::reallocate(nBands_,x_size_,y_size_);
	data_alloc(src_ext,data_,bands(),bands()*size_x());
}
void asf::parameter_float_image::reallocate(int nBands_,int x_size_,int y_size_,
		band_t *data_,int xd_,int yd_)
{
	parameter_pixel_image::reallocate(nBands_,x_size_,y_size_);
	data_alloc(src_ext,data_,xd_,yd_);
}
*/

asf::parameter_float_image::~parameter_float_image()
{
	data_free();
}

void asf::parameter_float_image::data_alloc(src_t src_,band_t *data_,int xd_,int yd_)
{
	data_free();
	src=src_;
	data=data_;
	xd=xd_;
	yd=yd_;
	if (data==NULL && src==src_alloc) 
		data=new band_t[size_y()*yd];
}
void asf::parameter_float_image::data_free(void)
{
	if (src==src_alloc) delete[] data;
	data=NULL;
	xd=yd=-1;
}

/// Error-checking pixel access.  Aborts if x,y, or band are out of bounds.
asf::parameter_float_image::band_t &asf::parameter_float_image::at_careful(int x,int y,int band) const
{
	if (!pixels().inbounds(x,y)) {
		char buf[1024];
		sprintf(buf,"at_careful pixel access: pixel (%d,%d) is not in-bounds on a %d x %d pixel image",
			x,y,size_x(),size_y());
		die(buf);
	}
	if (band<0) die("at_careful pixel access: Bogus negative band number");
	if (band>=bands()) die("at_careful pixel access: Bogus too-big band number");
	return data[x*xd+y*yd+band];
}

void asf::parameter_float_image::set(float toValue) {
	for (int b=0;b<bands();b++) {
		ASF_FOR_PIXELS(x,y,pixels()) 
			at(x,y,b)=toValue;
	}
}

ASF_parameter_def(asf::parameter_float_image);


/************ Image constraints ***************/
const asf::type *asf::parameter_image_constraint::get_type(void)
{return &asf::parameter_pixel_image::static_type;}

void asf::parameter_scanline_constraint::constrain_subimage(int ix,int iy,int *ox,int *oy,int *oz) {
	*ox=ix; /* must read an entire image row at a time */
	/* any number of scanlines (oy) will work */
	*oz=0; /* must read full-resolution data */
}
void asf::parameter_scanline_constraint::print(void) 
	{ printf("parameter_scanline_constraint\n"); }

void asf::parameter_wholeimage_constraint::constrain_subimage(int ix,int iy,int *ox,int *oy,int *oz) {
	*ox=ix; *oy=iy; *oz=0;
}
void asf::parameter_wholeimage_constraint::print(void) 
	{ printf("parameter_wholeimage_constraint\n"); }

/* Define a static "instance" method that returns a single
  static instance of this class. */
#define singleton_static_instance(classname) \
classname *classname::instance(void) { \
	static classname *i=new classname; \
	return i; \
}

singleton_static_instance(asf::parameter_scanline_constraint)
singleton_static_instance(asf::parameter_wholeimage_constraint)

/****************** Image Plugin Superclasses *****************/
/**** Plugin */
/* static */ const asf::type asf::plugin_pixel_filter::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "pixel_filter",
  /* desc    */ "Changes pixel values, but does *not* change pixel locations.",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/11/9",
  /* version */ 1.0
);

asf::plugin_pixel_filter::plugin_pixel_filter(asf::plugin_parameters &param)
	:asf::plugin(param)
{
	asf::input(param,"in",&in);
	asf::output(param,"out",&out);
}

/**
 Initialize the output image metadata based on the 
 input image metadata.
*/
void asf::plugin_pixel_filter::meta_execute(void)
{
	out->meta_setsize(in->bands(),in->total_meta_bounds());
}

/**
 Return a new location_function that returns 
 input image coordinates given output image coordinates.
*/
asf::location_function *asf::plugin_pixel_filter::image_in_from_out(int i,int o)
{
	/* Input and output are colocated; so this is an identity function */
	return new asf::location_function_identity();
}

/**** Plugin */
/* static */ const asf::type asf::plugin_pixel_kernel::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "pixel_kernel",
  /* desc    */ "Computes pixel values based on a fixed neighborhood",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/11/9",
  /* version */ 1.0
);

asf::plugin_pixel_kernel::plugin_pixel_kernel(asf::plugin_parameters &param)
	:asf::plugin_pixel_filter(param) {}

/**
 Initialize the output image metadata based on the 
 input image metadata.
*/
void asf::plugin_pixel_kernel::meta_execute(void)
{
	kernel_geometry g=get_kernel_geometry();
	asf::pixel_rectangle inR=in->total_meta_bounds(),outR;
	/* Scale the pixel rectangle by our scale factor,
	  rounded to an integral number of whole pixels. */
	outR.lo_x=(int) ceil((inR.lo_x-g.neighborhood.lo_x)/g.scale.x);
	outR.lo_y=(int) ceil((inR.lo_y-g.neighborhood.lo_y)/g.scale.y);
	outR.hi_x=(int)floor((inR.hi_x-g.neighborhood.hi_x)/g.scale.x);
	outR.hi_y=(int)floor((inR.hi_y-g.neighborhood.hi_y)/g.scale.y);
	out->meta_setsize(in->bands(),outR);
}


/**
  Here's how we compute the input pixels needed for a given output pixel rectangle.
*/
class location_function_kernel : public asf::location_function {
public:
	asf::plugin_pixel_kernel::kernel_geometry g;
	
	/** point-to-point transform: just scale coordinates */
	virtual asf::image_location apply(const asf::image_location &loc) {
		return asf::image_location(g.scale.x*loc.x,g.scale.y*loc.y);
	}
	
	/** rect-to-rect transform: include padding for kernel */
	virtual asf::pixel_rectangle apply_rectangle(asf::pixel_rectangle b) {
		asf::pixel_rectangle ret;
		ret.lo_x=(int)floor(b.lo_x*g.scale.x+g.neighborhood.lo_x);
		ret.lo_y=(int)floor(b.lo_y*g.scale.y+g.neighborhood.lo_y);
		ret.hi_x=(int)ceil(b.hi_x*g.scale.x+g.neighborhood.hi_x);
		ret.hi_y=(int)ceil(b.hi_y*g.scale.y+g.neighborhood.hi_y);
		return ret;
	}
	
	/** Constructor. */
	location_function_kernel(const asf::plugin_pixel_kernel::kernel_geometry &g_) 
		:asf::location_function(asf::location_function::scaling),
		 g(g_) {}
};

/**
 Return a new location_function that returns 
 input image coordinates given output image coordinates.
*/
asf::location_function *asf::plugin_pixel_kernel::image_in_from_out(int i,int o)
{
	return new location_function_kernel(get_kernel_geometry());
}

