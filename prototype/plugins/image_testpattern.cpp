/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include "asf/image.h"
#include <math.h>

/* Plugin name */
#define self plugin_image_testpattern
class self : public asf::plugin {
	asf::parameter_int *w,*h; /* Size of image */
	asf::parameter_int *type; /* Kind of test pattern to make */
	asf::parameter_real *scale; /* Scale of features in test pattern (default 20 pixels) */
	asf::parameter_real *value; /* Value at which features wrap around (default 1.0) */
	asf::parameter_float_image *dest; /* Image to fill with test pattern */
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin(param)
	{
		asf::input(param,"w",&w);
		asf::input(param,"h",&h);
		asf::optional(param,"type",&type);
		asf::optional(param,"scale",&scale);
		asf::optional(param,"value",&value);
		asf::output(param,"dest",&dest);
	}
	void meta_execute(void) {
		dest->meta_setsize(1,asf::pixel_rectangle(*w,*h));
	}
	void execute(void) { 
		int t=0; if (type) t=*type;
		
		double scl=20.0; if (scale) scl=*scale;
		double i_scl=1.0/scl;
		
		double val=1.0; if (value) val=*value;
		
		int xc=*w/2, yc=*h/2; // center coordinate (meta pixels)
		ASF_FOR_PIXELS(x,y,dest->pixels()) {
			double v=0.0;
			asf::meta_pixel_location m=dest->
				meta_from_pixel(asf::pixel_location(x,y));
			int cx=m.x-xc, cy=m.y-yc;
			double r=sqrt((double)(cx*cx+cy*cy));
			switch (t) {
			case 0: v=(r<scl)?1.0:0.0; break; // white center circle
			case 1: v=((((int)(r*i_scl)))%2)?1.0:0.0; break; // white/black bulls-eye
			case 2: v=fmod(r*i_scl,1.0); break; // ramp bulls-eye
			case 3: v=fmod(r*r*i_scl*i_scl,1.0); break; // Fresnel zone pattern
			case 10: v=fmod(x*i_scl,1.0); break; // x ramps
			case 11: v=fmod(y*i_scl,1.0); break; // y ramps
			default:
				asf::die("Unrecognized test pattern type");
			}
			dest->at(x,y,0)=v*val; // v*(hi-lo)+lo;
		}
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "image_testpattern",
  /* desc    */ "Create a variety of 'test patterns' in the output image, for debugging.",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/10/11",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,image_testpattern)
