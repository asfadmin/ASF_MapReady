/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include "asf/image.h"
#include <math.h>

/* Plugin name */
#define self plugin_image_blur

class self : public asf::plugin_pixel_kernel {
	asf::parameter_int *radius; /* Blurring radius, in pixels */
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin_pixel_kernel(param)
	{
		asf::input(param,"radius",&radius);
	}
	
	/** Move pixels */
	virtual void execute(void) { 
		int r=*radius; /* Radius of kernel */
		int w=r+1+r; /* total width of kernel: r on the left, center, r on the right */
		double normalization=1.0/(w*w);
		/* Loop over output pixels */
		for (int b=0;b<out->bands();b++) {
		  ASF_FOR_PIXELS(x,y,out->pixels()) {
			double sum=0.0;
			/* Loop over input pixels. 
			  Note that out->at(x,y,b) is really in->at(x+r,y+r,b)
			*/
			for (int dy=0;dy<w;dy++)
			for (int dx=0;dx<w;dx++) {
				sum+=in->at(x+dx,y+dy,b);
			}
			out->at(x,y,b)=sum*normalization;
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
  /* name    */ "image_blur",
  /* desc    */ "Blur an image, by appling a centered blurring kernel.",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/05/11",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,image_blur)
