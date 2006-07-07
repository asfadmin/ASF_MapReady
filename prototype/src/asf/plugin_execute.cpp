/**
Determine execution order for asf plugins.
Basically a compiler-like dependency analysis
and lazy execution system.  This file is way too complicated
and buggy--need to figure out a cleaner, simpler approach.

The only external entry point is "execute_list" at the bottom.
External calls go out from here to the parameter_image classes,
and plugin::execute.

The flow of control inside this file is truly horrific, so here's the cheat sheet:

All these classes are shadows of the real working classes like 
plugin and parameter_pixel_image.

execute_plugin shadows "plugin". It's used to keep track of dependencies.
It's got an "render" method, which loops over sub-tiles 
to drive the rendering.

execute_image shadows "parameter_pixel_image".  It
keeps track of the desired size and source of each image.
The key method is again "render", which loops over input images.

execute_block shadows "parameter_control_list".  Its job is to keeps track 
of everybody, and allow execute_plugins to find one another via their 
shared parameters.  Everything starts at the execute_block object.

See the comment at the bottom routine for overall control flow.

Orion Sky Lawlor, olawlor@acm.org, 2005/11/29.
*/
#include "asf/plugin.h"
#include "asf/plugin_control.h"
#include "asf/image.h"

/* Base execution logging level */
#define ex_loglevel 20

using namespace asf;

class execute_plugin; /* forward declaration */
class execute_image;

/**
  Cover a rectangle with tiles of the given size.
*/
class rect_tiling {
public:
	asf::pixel_rectangle r; ///< Region to cover (pixels)
	pixel_size tile_size; ///< Pixel size of a tile
	tile_location tile_count; ///< total number of tiles in each direction
	pixel_location origin; ///< pixel coordinates of topleft corner of first tile
	
	/** Create a tiling of the rectangle r, clipped to bounds, of this tile size */ 
	rect_tiling(const asf::pixel_rectangle &r_,const asf::pixel_rectangle &bounds_,const pixel_size &tile_sz_);
	
	// Return the outline of this tile:
	asf::pixel_rectangle get_outline(const tile_location &t) {
		asf::pixel_rectangle outer( /* entire region to potentially cover */
			origin.x+tile_size.x*t.x,origin.y+tile_size.y*t.y,
			origin.x+tile_size.x*(t.x+1),origin.y+tile_size.y*(t.y+1)
		);
		return outer.get_intersect(r);
	}
};

/** Create a tiling of the rectangle r, clipped to bounds, of this tile size */ 
rect_tiling::rect_tiling(const asf::pixel_rectangle &r_,const asf::pixel_rectangle &bounds_,const pixel_size &sz_)
	:r(bounds_.get_intersect(r_)), tile_size(sz_), 
	 tile_count((r.width()+tile_size.x-1)/tile_size.x,
	            (r.height()+tile_size.y-1)/tile_size.y),
	 origin(r.get_min())
{
}


/**
  Keeps a list of plugins, e.g., that read or write a particular parameter.
*/
class execute_plugins {
	std::vector<execute_plugin *> l;
public:
	inline execute_plugin *operator[](int i) const {return l[i];}
	inline unsigned int size(void) const {return l.size();}
	void push_back(execute_plugin *p) {
		int s=size();
		if (s>0 && l[s-1]==p) return; /* already there--don't bother */
		l.push_back(p);
	}
};

/// Describes desired properties of processing tiles
class set_size_param {
public:
	/// Preferred size of a processing tile.  
	///  May actually end up bigger or smaller than this.
	pixel_size size;
	/// Preferred log-base-2 zoom factor of a tile.
	int zoom;
};

/// Describes a region to be rendered
class render_request {
public:
	/// Region in meta coordinates to be rendered
	asf::pixel_rectangle rect;
	/// Maximum zoom factor to use in rendering (detail requested)
	int zoom;
	render_request() {zoom=-1234;}
	render_request(const asf::pixel_rectangle &r,int z) :rect(r), zoom(z) {}
};

/**
  Lets plugins find each other through their shared parameters.
*/
class execute_block {
public:
	/** Create a new empty top-level block */
	execute_block(void);
	
	/** Create a new sub-block for sub-plugins of this master.
	   The outer block is the master's context */
	execute_block(execute_plugin *master,execute_block *outer);

	/** Add this block of plugins */
	void add(const parameter_control_list *list);
	
	/** Add this plugin to our lists */
	void add(plugin *pl);

	/** This plugin creates this parameter */
	void add_output(execute_plugin *pl,parameter *pa,parameter_constraint *constraint);
	
	/** This plugin reads this parameter. 
	   Return the image object if this turns out to be an image.
	*/
	execute_image *add_input(execute_plugin *pl,parameter *pa,parameter_constraint *constraint);
	
	/** Set up the sizes of all our images */
	void set_size(const set_size_param &preferred);
	
	/** Execute all sink plugins to render at this size and detail */
	void render(const render_request &req);
private:
	/// Keeps a big list of all possible plugins 
	///  (for eventual cleanup)
	execute_plugins plugins;
	
	/// Keeps a list of plugins that have side effects (i.e., sink plugins)
	///  These are plugins that might send stuff off to the outside world.
	execute_plugins sink_plugins;
	void add_sink(execute_plugin *ex);
	
	/// Connects parameters to the plugins that create them.
	typedef std::multimap<parameter *,execute_plugin *> creators_t;
	creators_t creators;

	/** Return the execute_image corresponding to this parameter, or NULL if not an image */
	execute_image *if_image(parameter *);
	
	/// Connects parameters to their execute_image objects.
	typedef std::map<parameter *,execute_image *> images_t;
	images_t images;
	
	/// Used only for sub-registries:
	execute_plugin *master; execute_block *outer;
};

/**
  Represents an image parameter that is passed between two plugins.
  Manages the conversions between:
    1.) The image total size, which is readonly by us.
    2.) The image's allocated size, which we control.
    3.) The image's processing tile size, which is under plugin control.
*/
class execute_image {
public:
	/** Datatype of images we deal with */
	typedef parameter_float_image img_t;
	
	execute_image(img_t *img_);
	
	/** 
	  "Plugin", or "Portal" image.
	  Image that all the plugins are pointing at. 
	  Normally points to data stored in Aimg--that is,
	  Pimg is a plugin's "portal" to Aimg's data.
	*/
	img_t *Pimg;
	
	/**
	 "Allocated", or "All" image.
	 Image used for allocating pixels.  Different from Pimg because
	 sometimes we want to allocate a big piece (e.g., to pass to an I/O plugin)
	 but pass some plugins smaller chunks (e.g., for cache efficiency).
	*/
	img_t *Aimg;
	
	/// Add a plugin that might read or write this image
	void add_plugin(execute_plugin *p,parameter_constraint *constraint,bool isOutput);
	
	/** Set up the allocated and per-plugin sizes of our image. 
	 default_tile_size is the desired size of a processing tile, in pixels.
	   Small tile sizes will fit in cache better.
	   Large tile sizes have lower book-keeping overhead.
	   The best tile size must still be determined by experiment.
	*/
	void set_size(const set_size_param &preferred);
	
	/**
	  Render this meta-coordinates region of us.  Always:
	  	- Allocates this region.
		- Fills region with data from input plugins.
	*/
	void render(const render_request &req);
	
	/**
	  An input plugin has changed-- now none of us is computed correctly.
	*/
	void flush(void);
	
	/// The total finished size of our image (virtual region divided into tiles)
	pixel_size total;
	/// The size our image should be allocated to
	pixel_size alloc;
	int zoom; /* Zoom factor for our image */
	
	/// The tile size our plugins should shoot for (during set_size only)
	pixel_size preferred_tile;
	
	/**
	  This rectangle stores the portion of us that is allocated and fully computed.
	*/
	asf::pixel_rectangle finished_rect;
	
	/** Stores the relationship of a plugin to this image */
	class plug_img {
	public:
		/// Plugin we represent
		execute_plugin *p;
		/// Constraint this plugin places on image size (may be NULL)
		parameter_image_constraint *constraint;
		/// Image size for use by this plugin.  -1, -1 for unknown
		pixel_size tile;
		/// Zoom factor used by this plugin.
		int zoom;
	};
	
	/// Return the plug_img for this plugin.
	plug_img *get_img(execute_plugin *p) {
		unsigned int i;
		for (i=0;i<inp.size();i++) if (inp[i]->p==p) return inp[i];
		for (i=0;i<outp.size();i++) if (outp[i]->p==p) return outp[i];
		return 0;
	}
	
	/** List of plugins that (may) write to our image */
	std::vector<plug_img *> inp;
	
	/** List of plugins that (may) read from our output image */
	std::vector<plug_img *> outp;
	
private:
	void size_pass(plug_img *pi);
};


/**
  Represents one execution of a plugin.  Basically a wrapper
  around a real plugin object, used to keep track of dependencies.
  Possibly should be merged with asf::plugin.
  
  Initial state: flushed.
  After "execute", outputs become ready.
*/
class execute_plugin {
public:
	execute_plugin(plugin *p,execute_block &r) {setup(p,r);}
	virtual ~execute_plugin() {}
	
	/// Compute this meta rectangle of our output image(s), which are already allocated.
	virtual void render(const render_request &req);
	
	/// Render self as a sink, covering yourself completely.
	///   You need not leave output images allocated to the requested size.
	virtual void sink_render(const render_request &req);
	
	/**
	 One of our inputs has changed--invalidate all our outputs.
	*/
	virtual void flush(void);
	
	/// State of this plugin:
	typedef enum {
		state_needy=0, /* Need inputs (initial state) */
		state_busy=1, /* Actually computing outputs from inputs */
		state_ready=2 /* Outputs are ready */
	} state_t;
	/// Current state of this plugin
	state_t state;
	
	/// Real plugin we will execute
	plugin *pl;
	
	/// Datatype for images we read or write
	typedef parameter_pixel_image img_t;
	
	/// List of plugins that create any of our input parameters
	execute_plugins in_dep;
	/// Our plugin's input image objects
	std::vector<execute_image *> in_img;
	/// The map from our output image to the corresponding input image above
	std::vector<location_function *> in_map;
	/// Add this input image
	void add_in_img(execute_image *i);
	/// Return true if we shouldn't bother with any image crap.
	bool no_images(void) const {return in_img.size()==0 && out_img.size()==0;}
	
	/// List of plugins that use any of our output parameters
	execute_plugins out_dep;
	/// Our plugin's output images
	std::vector<execute_image *> out_img;
protected:
	execute_plugin() {} /* MUST call setup afterwards! */
	void setup(plugin *p,execute_block &r);
	/// Render all inputs needed for this output rectangle. 
	void render_inputs(const render_request &req);
	/// Indicate to all our output dependencies that we've been flushed
	void out_flush(void);
};

/**
  An execute_plugin wrapper around a parameter_control_flow plugin.
*/
class execute_plugin_control : public execute_plugin {
public:
	execute_plugin_control(plugin_control_flow *plf,execute_block &r);
	~execute_plugin_control();
	
	virtual void render(const render_request &req);
private:
	/** Registries used by our plugin's different control lists */
	std::vector<execute_block *> subs;
	
	/** Real plugin we're wrapping */
	plugin_control_flow *plf;
};

/************* block *****************/
/** Create a new empty top-level block */
execute_block::execute_block(void) {
	master=0; outer=0;
}
	
/** Create a new sub-block for sub-plugins of this master.
   The outer block is the master's context */
execute_block::execute_block(execute_plugin *m,execute_block *o) {
	master=m; outer=o;
}

void execute_block::add(const parameter_control_list *list)
{
	for (unsigned int i=0;i<list->size();i++)
		add(list->index(i));
}

/**
  Add this plugin object to us, by wrapping it in an execute_plugin
  (or execute_plugin_control).
*/
void execute_block::add(plugin *pl)
{
	execute_plugin *ex;
	plugin_control_flow *plf=dynamic_cast<plugin_control_flow *>(pl);
	if (plf) ex=new execute_plugin_control(plf,*this);
	else ex=new execute_plugin(pl,*this);
	plugins.push_back(ex);
	if (pl->has_side_effects()) {
		add_sink(ex);
	}
}

/**
  Add a "sink" plugin (that writes files or creates output, and hence
  has to be called on each execution).
*/
void execute_block::add_sink(execute_plugin *ex)
{
	sink_plugins.push_back(ex);
	if (outer) outer->add_sink(master); /* if we've got sinks, our master is a sink too */
}

/** This plugin creates this parameter */
void execute_block::add_output(execute_plugin *creator,parameter *pa,parameter_constraint *constraint)
{
	execute_image *img=if_image(pa);
	if (img) { /* creator's output is image's input */
		creator->out_img.push_back(img);
		img->add_plugin(creator,constraint,false);
	}
	
	creator->pl->log(ex_loglevel+5,"execute_block::add_output> I create parameter %p (%s)\n",pa,pa->get_type()->name());
	
	creators.insert(std::make_pair(pa,creator));
	if (master)
		outer->add_output(master,pa,constraint); /* seen from outside, master creates this parameter */
}

/** This plugin reads this parameter */
execute_image *execute_block::add_input(execute_plugin *reader,parameter *pa,parameter_constraint *constraint) 
{
	execute_image *img=if_image(pa);
	if (img) { /* image output is reader's input */
		reader->in_img.push_back(img);
		img->add_plugin(reader,constraint,true);
	}
	
	creators_t::iterator start=creators.equal_range(pa).first;
	creators_t::iterator end=creators.equal_range(pa).second;
	for (creators_t::iterator it=start;it!=end;++it)
	{/* Somebody we know creates that input */
		execute_plugin *creator=(*it).second;
		if (creator!=reader && img==NULL) {
			reader->pl->log(ex_loglevel+5,"execute_block::add_input> I depend on plugin %s via parameter %p (%s)\n",creator->pl->get_type()->name(),pa,pa->get_type()->name());
				
			reader->in_dep.push_back(creator); /* he goes in our input dependency list */
			creator->out_dep.push_back(reader); /* we go in his output dependency list */
		}
	}
	if (outer) { /* Perhaps the parameter also comes from outside */
		outer->add_input(master,pa,constraint); /* ... and master needs it too (so we get called). */
		outer->add_input(reader,pa,constraint); /* ... we need to go in his out_dep lists. */
	}
	return img;
}
execute_image *execute_block::if_image(parameter *p)
{
	if (outer) return outer->if_image(p); /* don't keep image lists in sub-registries; only at top level */
	images_t::iterator it=images.find(p);
	if (it!=images.end()) return (*it).second; /* found it! */
	/* else nothing listed yet-- check if it's even an image */
	execute_image::img_t *img=dynamic_cast<execute_image::img_t *>(p);
	if (!img) return 0; /* not even an image */
	/* else make a new execute_image to store this new image */
	execute_image *i=new execute_image(img);
	images[p]=i;
	return i;
}

/// For "printf" on a rect object:
#define rect_fmt "rect (%d-%d, %d-%d)"
#define rect_args(rect) rect.lo_x,rect.hi_x,rect.lo_y,rect.hi_y

/// For "printf" on a render_request object:
#define req_fmt " region("rect_fmt ", zoom %d)"
#define req_args(req) rect_args(req.rect), req.zoom

void execute_block::set_size(const set_size_param &preferred) {
	asf::log(ex_loglevel,"execute_block> set_size {\n");
	for (images_t::iterator it=images.begin();it!=images.end();++it)
		(*it).second->set_size(preferred);
	asf::log(ex_loglevel,"execute_block> } set_size\n");
}

static int big_int=1999999999; /**< A big signed integer.  Don't use (~0)>>1 because of wraparound problems. */
static asf::pixel_rectangle big_rect(-big_int,-big_int,big_int,big_int);
static asf::pixel_rectangle ok_rect(0,0,1,1);
static asf::pixel_rectangle empty_rect(0,0,0,0);

void execute_block::render(const render_request &req) {
	asf::log(ex_loglevel,"execute_block> render { \n");
	for (unsigned int i=0;i<sink_plugins.size();i++) {
		asf::log(ex_loglevel+3,"   execute_block> rendering next sink\n");
		sink_plugins[i]->sink_render(req); /* will recursively prep all required inputs */
	}
	asf::log(ex_loglevel,"execute_block> } render \n");
}

/***************** Images ********************/
execute_image::execute_image(img_t *img_) 
	:Pimg(img_), Aimg(0), 
	 total(-1,-1), alloc(-1,-1), zoom(0),
	 finished_rect(empty_rect)
{
	Aimg=(img_t *)(Pimg->clone());
}

void execute_image::add_plugin(execute_plugin *p,parameter_constraint *constraint,bool isOutput)
{
	plug_img *i=new plug_img;
	i->p=p; 
	i->constraint=dynamic_cast<parameter_image_constraint *>(constraint);
	i->tile.x=i->tile.y=-1;
	if (isOutput) outp.push_back(i);
	else inp.push_back(i);
}

/** Round this rectangle up to a multiple of preferred.size */
pixel_rectangle roundup(const set_size_param &preferred,const pixel_rectangle &r) {
	pixel_rectangle ret;
	ret.lo_x=r.lo_x;
	ret.lo_y=r.lo_y;
	/* Round size up to a multiple of preferred.size */
	ret.hi_x=r.lo_x+preferred.size.x*(int)ceil(r.width() /(double)preferred.size.x);
	ret.hi_y=r.lo_y+preferred.size.y*(int)ceil(r.height()/(double)preferred.size.y);
	return ret;
}

/** Set up the sizes of our image */
void execute_image::set_size(const set_size_param &preferred)
{
	/* The image's total pixel size comes from the plugins-- 
	   it's not under our control.
	*/
	Aimg->meta_setsize(Pimg->bands(),roundup(preferred,Pimg->total_meta_bounds()));
	total=Pimg->total_meta_bounds().size();
	
	/* We *want* to allocate the smaller of the tile and total image sizes */
	alloc.x=std::min(preferred.size.x,total.x);
	alloc.y=std::min(preferred.size.y,total.y);
	zoom=preferred.zoom;
	
	bool alloc_changed=false;
	do {
		/* Don't bother tiling really small allocations--just bump up the allocation */
		if (alloc.x*2>total.x) alloc.x=total.x;
		if (alloc.y*2>total.y) alloc.y=total.y;
		pixel_size old=alloc;
		unsigned int i;
		/* We *have* to allocate the largest tile size of any plugin */
		for (i=0;i< inp.size();i++) size_pass( inp[i]);
		for (i=0;i<outp.size();i++) size_pass(outp[i]);
		alloc_changed=(old.x!=alloc.x || old.y!=alloc.y); 
		inp[0]->p->pl->log(ex_loglevel+1,"execute_image> set_size alloc loop: %d x %d total; %d x %d z=%d alloc\n",
			total.x,total.y,alloc.x,alloc.y,zoom);
		/* if alloc_changed, processed chunk got bigger-- try again */
	} while (alloc_changed);
	
	/* We now have the subimage size to allocate, and all tile sizes.  
	   We don't actually allocate the subimage until it's first needed. */
}

/** Utility routine called only during set_size:
  Set this plugin's processed tile size, and enlarge our 
  allocated size if needed. */
void execute_image::size_pass(plug_img *pi)
{
	pi->tile=alloc; /* process the preferred tile size by default */
	pi->zoom=zoom; /* process at standard zoom by default */
	if (pi->constraint) {
		pi->constraint->constrain_subimage(total.x,total.y,&pi->tile.x,&pi->tile.y,&pi->zoom);
		if (alloc!=pi->tile || zoom!=pi->zoom)
			pi->p->pl->log(ex_loglevel,"execute_image> set_size %d x %d image: constraint required %d x %d/%d instead of %d x %d/%d\n",
			total.x,total.y,pi->tile.x,pi->tile.y,pi->zoom,alloc.x,alloc.y,zoom);
		/* We must allocate the *largest* tile size needed by any plugin */
		alloc.x=std::max(alloc.x,pi->tile.x);
		alloc.y=std::max(alloc.y,pi->tile.y);
		/* We need the *smallest* (finest) zoom factor used by any plugin */
		zoom=std::min(zoom,pi->zoom);
	}
}

/******** Execute_plugin ************/
void execute_plugin::setup(plugin *pl_,execute_block &r) {
	pl=pl_;
	state=state_needy;
	
/* Figure out what parameters this plugin takes, and where they come from */
	const plugin_parameter_signature *sig=pl->get_signature();
	plugin_parameters &ps=pl->get_parameters();
	const plugin_parameter_signature::params *v;
	unsigned int i;
	parameter *pa=0;
	v=&sig->inputs();
	for (i=0;i<v->size();i++) {
		ps.param((*v)[i]->name,(*v)[i]->t,&pa,0,0,plugin_parameters::dir_input);
		add_in_img(r.add_input(this,pa,(*v)[i]->constraint));
	}
	v=&sig->optionals();
	for (i=0;i<v->size();i++) {
		ps.param((*v)[i]->name,(*v)[i]->t,&pa,0,0,plugin_parameters::dir_optional);
		if (pa) add_in_img(r.add_input(this,pa,(*v)[i]->constraint));
	}
	
	v=&sig->outputs();
	for (i=0;i<v->size();i++) {
		ps.param((*v)[i]->name,(*v)[i]->t,&pa,0,0,plugin_parameters::dir_output);
		r.add_output(this,pa,(*v)[i]->constraint);
	}
}
void execute_plugin::add_in_img(execute_image *i)
{
	if (i==0) return;
	in_map.push_back(pl->image_in_from_out(in_map.size(),0));
}

/// Render self as a sink, covering yourself completely.
///   You need not leave output images allocated to the requested size.
void execute_plugin::sink_render(const render_request &req)
{
	render(req);	
}

/// Compute this meta rectangle of our output image(s).
void execute_plugin::render(const render_request &req)
{
	pl->log(ex_loglevel,"execute_plugin> render {  "req_fmt"\n",req_args(req));
	if (no_images()) 
	{ /* A non-image plugin-- just run once and we're done. */
		if (state==state_ready) return; /* nothing to do */
		pl->log(ex_loglevel,"execute_plugin>   non-image run\n");
		render_inputs(req);
		pl->execute();
	}
	else 
	{ /* An image plugin-- loop over tiles */
		execute_image *ei=0;
		if (out_img.size()>0) ei=out_img[0]; /* Normal case: loop over output image */
		else if (in_img.size()>0) ei=in_img[0]; /* for sinks: loop over input image */
		else asf::die("Logic error in execute_plugin::render!"); /* should be no_images case */

		/* Find the image's record for us */
		execute_image::plug_img *p=ei->get_img(this);

		/* Now loop over tiles.  
			SUBTLE: ei->Pimg gives the maximum region we should consider touching.
		*/
		rect_tiling tiles(req.rect,ei->Pimg->total_meta_bounds(),p->tile);
		tile_location t;
		for (t.y=0;t.y<tiles.tile_count.y;t.y++)
		for (t.x=0;t.x<tiles.tile_count.x;t.x++)
		{
			pixel_rectangle tile_r=tiles.get_outline(t);
			pl->log(ex_loglevel,"execute_plugin>  rendering tile (%d,%d)\n",t.x,t.y);
			state=state_needy; /* flush outputs; need output tile */
			
			/* Prepare input images */
			render_request t; t.rect=tile_r; t.zoom=req.zoom;
			render_inputs(t);
			
			/* Compute outputs from inputs */
			ei->Pimg->pixel_pointat(ei->Aimg,t.rect,t.zoom);
			pl->log(ex_loglevel-5,"execute_plugin>  execute plugin  "req_fmt"\n",req_args(t));
			pl->execute();
			pl->log(ex_loglevel,"execute_plugin>  } execute plugin\n");
		}
	}
	
	state=state_ready; /* our outputs are now ready */
	pl->log(ex_loglevel,"execute_plugin> } render\n");
}

/* Make sure each input image is really there */
void execute_plugin::render_inputs(const render_request &req) {
	unsigned int i;
	
	pl->log(ex_loglevel,"execute_plugin>  render_inputs { "req_fmt"\n",req_args(req));
	
	if (state==state_busy) asf::die("Circular dependency?  execute_plugin::execute called, but already running execute method!\n");
	state=state_busy; /* busy computing input requirements */
	
	/* Make sure each non-image input plugin is ready */
	for (i=0;i<in_dep.size();i++) {
		pl->log(ex_loglevel+3,"execute_plugin>    rendering non-image input %d\n",i);
		in_dep[i]->render(req); /* FIXME: same request OK? */
	}
	
	/* Make sure each input image is ready */
	for (i=0;i<in_img.size();i++) {
		render_request in;
		/* Find the portion of the input image we'll need to cover this part of our output. */
		if (in_map[i]) in.rect=in_map[i]->apply_rectangle(req.rect);
		else in.rect=req.rect;
		/* FIXME: adjust zoom factor for input */
		in.zoom=req.zoom;
		
		pl->log(ex_loglevel,"execute_plugin>    my "req_fmt" is input image %d's "req_fmt"\n",
			req_args(req), i, req_args(in));
		
		execute_image *ei=in_img[i];
		
		/* Ask the image to create that portion of its output. 
		  This may involve recursive calls out to other plugins.  */
		ei->render(in);
		
		/* Leave the image's pixels pointing where *we* need them */
		ei->Pimg->pixel_pointat(ei->Aimg,in.rect,in.zoom);
	}
	pl->log(ex_loglevel,"execute_plugin>  } render_inputs\n");
}


/**
  Make sure this rectangle of us has been computed properly,
  by allocating ourselves and calling our input plugins.
*/
void execute_image::render(const render_request &req)
{
	inp[0]->p->pl->log(ex_loglevel,"execute_image> image render " req_fmt "\n",req_args(req));
	
	if (finished_rect.contains(req.rect)) return; /* already computed */
	/* else we need to ask our input plugins to compute us: */
	
	/* FIXME: image shift & caching here?  Or are overlapping requests rare? */

	/* Allocate image to be big enough to handle the request *and* all our plugins */
	int w=std::max(req.rect.width(),alloc.x);
	int h=std::max(req.rect.height(),alloc.y);
	int z=std::min(zoom,req.zoom);
	pixel_rectangle r(
		req.rect.lo_x  ,req.rect.lo_y,
		req.rect.lo_x+w,req.rect.lo_y+h);
	Aimg->pixel_setsize(r,z); /* allocate ourselves on the requested rectangle */
	inp[0]->p->pl->log(ex_loglevel,"execute_image> image allocated with (%d,%d) meta pixels, zoom %d\n",
		w,h,z);
	r=r.get_intersect(Pimg->total_meta_bounds());
	for (unsigned int i=0;i<inp.size();i++) {
		render_request in;
		in.rect=r;
		in.zoom=inp[i]->zoom;
		inp[i]->p->render(in);
	}
	/* We've now finished calculating this part of ourselves. */
	finished_rect=r;
}

void execute_plugin::flush(void)
{
	if (state==state_needy) return; /* already flushed */
	state=state_needy;
	out_flush();
}
void execute_plugin::out_flush(void) 
{
	pl->log(ex_loglevel,"execute_plugin> out_flush {\n");
	/* Make sure each output knows we're gone */
	for (unsigned int i=0;i<out_dep.size();i++)
		out_dep[i]->flush();
	/* Make sure each outgoing image knows we're gone */
	for (unsigned int i=0;i<out_img.size();i++)
		out_img[i]->flush();
	pl->log(ex_loglevel,"execute_plugin> } out_flush\n");
}

/// An input has changed-- set our valid region to empty
void execute_image::flush(void)
{
	finished_rect=empty_rect;
}

/************* execute_plugin_control **************
  Wraps plugins capable of control flow management, like loops.
*/
execute_plugin_control::execute_plugin_control(plugin_control_flow *p,execute_block &outer)
{
	plf=p;
	
	for (int i=0;i<plf->get_list_count();i++) 
	{ /* make a new execute_block for our plugin's i'th list */
		execute_block *sub=new execute_block(this,&outer);
		setup(p,*sub); /* Add our parameters to the sub-block */
		
		/* Register this list's contents in the sub-block */
		sub->add(plf->get_list(i));
		
		subs.push_back(sub);
	}
}
execute_plugin_control::~execute_plugin_control() {
	for (int i=0;i<plf->get_list_count();i++) 
		delete subs[i];
}

/*
 Execute our inner loop plugins.
*/
void execute_plugin_control::render(const render_request &req)
{
	pl->log(ex_loglevel,"execute_plugin> plugin_control render {\n");
	if (state==state_ready) return; /* nothing to do */
	render_inputs(req);
	// Idiomatic plugin_control_flow::execute, passing control to block
	int step=0, list=-1;
	while (0<=(list=plf->execute_iteration(step++))) {
		state=state_ready; /* output for this iteration is ready */
		out_flush(); /* because execute_iteration changed the loop index */
		
		pl->log(ex_loglevel,"execute_plugin> plugin_control iteration {\n");
		subs[list]->render(req); /* runs needy plugins inside the loop body */
		pl->log(ex_loglevel,"execute_plugin> } plugin_control iteration\n");
	} 
	
	pl->log(ex_loglevel,"execute_plugin> } plugin_control render\n");
}


/**
 Execute the plugins in this list in some sensible order,
 creating tiles as needed, and pruning useless branches.
*/
void asf::execute_list(const asf::parameter_control_list &list)
{
/**
Call the plugin's meta_execute routines.  Since meta_execute
routines are called in dependency order, each plugin's input parameters
already have metadata by the time the plugin gets called.

meta_execute normally figures out the theoretical meta size of each
plugin's output images. 
*/
	asf::log(ex_loglevel-1,"plugin_execute> Finding meta image sizes\n");
	list.meta_execute(); 

/**
Create execute_* shadows of the plugins and parameters in this control list.
*/
	asf::log(ex_loglevel  ,"plugin_execute> Building execute objects\n");
	execute_block r; 
	r.add(&list);

/** 
Figure out how big our image tiles should be at each stage of processing.
*/
	asf::log(ex_loglevel-1,"plugin_execute> Finding tile sizes\n");
	set_size_param preferred;
	preferred.size=pixel_size(64,64);
	preferred.zoom=0;
	r.set_size(preferred); /* find processed tile sizes */

/**
Actually render pixels.  This process is demand-driven, starting at the *output*
required, and recursively bubbling requests up to the root inputs.
*/
	asf::log(ex_loglevel-1,"plugin_execute> Executing plugins\n");
	render_request req;
	req.rect=big_rect;
	req.zoom=0;
	r.render(req); /* move pixels */
	
	asf::log(ex_loglevel-1,"plugin_execute> All plugins executed\n");
}
