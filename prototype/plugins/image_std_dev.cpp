/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include "asf/image.h"
#include "osl/statistics.h" /* means and standard deviations */
#include <math.h>

/* Plugin name */
#define self plugin_image_std_dev
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
		/* Loop over output pixels */
		for (int b=0;b<dest->bands();b++) {
		  ASF_FOR_PIXELS(x,y,dest->pixels()) {
			osl::statistics::Sample s;
			/* Loop over input pixels. 
			  Note that dest->at(x,y,b) is really src->at(x+r,y+r,b)
			*/
			for (int dy=0;dy<w;dy++)
			for (int dx=0;dx<w;dx++) {
				s+=src->at(x+dx,y+dy,b);
			}
			/* Coefficient of variation: std. dev. over mean */
			double coef_vari=s.getStddev()/s.getMean();
			dest->at(x,y,b)=coef_vari;
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
  /* name    */ "image_std_dev",
  /* desc    */ "Compute local means and standard deviations.\n"
"This is similar to a paper:\n"
" 'The JERS-1 Amazon Multi-Season Mapping Study (JAMMS):\n"
"    Science Objectives and Implications for Future Missions'\n"
"  A. Freeman, B. Chapman, and P. Siqueira\n"
"  Int. J. Remote Sensing, 2002, Vol.23-7, 1447-1460\n",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/05/11",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,image_std_dev)
