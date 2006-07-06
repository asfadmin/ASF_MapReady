/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include "asf/image.h"
#include <math.h>
#include <vector>
#include <algorithm> /* for std::sort */

/* Plugin name */
#define self plugin_image_median
class self : public asf::plugin_pixel_kernel {
	asf::parameter_int *radius; /* neighborhood radius, in pixels */
	asf::parameter_real *selected; /* fraction of sorted values to output; default 0.5 */
	std::vector<float> neigh; /* neighbor values */
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin_pixel_kernel(param)
	{
		asf::input(param,"radius",&radius);
		asf::optional(param,"selected",&selected);
	}
	
	/** Move pixels */
	virtual void execute(void) { 
		int r=*radius; /* Radius of kernel */
		int w=r+1+r; /* total width of kernel: r on the left, center, r on the right */
		int nNeigh=w*w;
		float sel=0.5;
		if (selected) sel=*selected;
		int outNeigh=(int)(sel*(nNeigh-1));
		neigh.resize(nNeigh);
		
		/* Loop over output pixels */
		for (int b=0;b<dest->bands();b++) {
		  ASF_FOR_PIXELS(x,y,dest->pixels()) {
			/* Loop over input pixels. 
			  Note that dest->at(x,y,b) is really src->at(x+r,y+r,b)
			*/
			for (int dy=0;dy<w;dy++)
			for (int dx=0;dx<w;dx++) {
				neigh[dy*w+dx]=src->at(x+dx,y+dy,b);
			}
			std::sort(&neigh[0],&neigh[nNeigh]);
			dest->at(x,y,b)=neigh[outNeigh];
		  }
		}
	}
	
	/** Return our kernel geometry */
	virtual asf::plugin_pixel_kernel::kernel_geometry 
		get_kernel_geometry(void) 
	{
		asf::plugin_pixel_kernel::kernel_geometry g;
		int r=*radius;
		g.neighborhood=asf::pixel_rectangle(-r,-r,+r,+r);
		return g;
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin_pixel_filter::static_type,
  /* name    */ "image_median",
  /* desc    */ "Apply the (speckle-reducing) median filter to an image.",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/05/11",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,image_median)
