/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include "asf/image.h"
#include <math.h>

/* Plugin name */
#define self plugin_image_fma

class self : public asf::plugin_pixel_filter {
	asf::parameter_real *mul; /* Value to multiply pixels by (default 1.0) */
	asf::parameter_real *add; /* Value to add to pixels (default 0.0) */
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin_pixel_filter(param)
	{
		asf::optional(param,"mul",&mul);
		asf::optional(param,"add",&add);
	}
	
	void execute(void) { 
		double m=1.0; if (mul) m=*mul;
		double a=0.0; if (add) a=*add;
		for (int b=0;b<in->bands();b++) {
			ASF_FOR_PIXELS(x,y,in->pixels()) {
				double v=in->at(x,y,b);
				out->at(x,y,b)=v*m+a;
			}
		}
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin_pixel_filter::static_type,
  /* name    */ "image_fma",
  /* desc    */ "Floating-point Multiply-Add (FMA): multiply image pixel values by 'mul', then add 'add'.",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/08/18",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,image_fma)
