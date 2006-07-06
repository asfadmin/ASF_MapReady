/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include "asf/image.h"

/* Plugin name */
#define self plugin_image_output_binary
class self : public asf::plugin {
	asf::parameter_filename *filename; /* Output filename */
	FILE *f;
	asf::parameter_float_image *src; /* Image to write */
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin(param), f(0)
	{
		asf::input(param,"filename",&filename);
		asf::input(param,"src",&src,asf::parameter_scanline_constraint::instance());
	}
	void meta_execute(void) {
		f=fopen(filename->c_str(),"wb");
		if (f==NULL) asf::die("Couldn't create file "+(std::string)*filename);
		// Try running makeddr, just for kicks
		char cmd[1024];
		asf::pixel_size sz=src->total_meta_bounds().size();
		sprintf(cmd,"makeddr '%s' %d %d float > /dev/null\n",
			filename->c_str(),sz.y,sz.x);
		system(cmd);
	}
	void execute(void) { 
		asf::pixel_rectangle p=src->pixel_meta_bounds();
		asf::pixel_rectangle m=src->total_meta_bounds();
		long pixel_no=(p.lo_x-m.lo_x)+(p.lo_y-m.lo_y)*m.width();
		fseek(f,sizeof(float)*pixel_no,SEEK_SET);
		ASF_FOR_PIXELS(x,y,src->pixels()) {
			fwrite(&src->at(x,y,0),sizeof(float),1,f); /* FIXME: multiband? */
		}
	}
	~self() {
		if (f) fclose(f);
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "image_output_binary",
  /* desc    */ "Write an image to a native-endian binary float output file.",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/10/11",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,image_output_binary)
