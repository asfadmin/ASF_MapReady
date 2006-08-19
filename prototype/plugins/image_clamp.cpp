/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include "asf/image.h"
#include <math.h>

/* Plugin name */
#define self plugin_image_clamp

class self : public asf::plugin_pixel_filter {
	asf::parameter_real *max; /* Scale of features in test pattern (default 20 pixels) */
	asf::parameter_real *min; /* Value at which features wrap around (default 1.0) */
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin_pixel_filter(param)
	{
		asf::optional(param,"max",&max);
		asf::optional(param,"min",&min);
	}
	
	void execute(void) { 
		double lo=-1.0e99; if (min) lo=*min;
		double hi=+1.0e99; if (max) hi=*max;
		for (int b=0;b<in->bands();b++) {
			ASF_FOR_PIXELS(x,y,in->pixels()) {
				double v=in->at(x,y,b);
				if (v<lo) v=lo;
				if (v>hi) v=hi;
				out->at(x,y,b)=v;
			}
		}
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin_pixel_filter::static_type,
  /* name    */ "image_clamp",
  /* desc    */ "Constrain output image pixel values to lie between min and max.",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/11/03",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,image_clamp)
