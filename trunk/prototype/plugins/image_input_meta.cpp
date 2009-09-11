/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include "asf/image.h"
#include "asf/ddr.h"
#include "asf_meta/meta_parameters.h"

/* Plugin name */
#define self plugin_image_input_meta
class self : public asf::plugin {
	asf::parameter_filename *filename; /**< Image filename */
	FILE *f; /**< Image file handle */
	asf::parameter_float_image *out; /**< Image to read */
	
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin(param), f(0)
	{
		asf::input(param,"filename",&filename);
		//asf::output(param,"out",&out,asf::parameter_scanline_constraint::instance());
	}
	void meta_execute(void) {
		log(1,"Creating blank .meta structure\n");
		asf::meta_parameters *blank=asf::meta_raw_init();
		asf::meta_write(blank,"test_blank_output.meta");
		log(1,"Creating reading file .meta structure\n");
		asf::meta_parameters *file=asf::meta_read(filename->c_str());
		asf::meta_write(file,"test_file_output.meta");
	}
	void execute(void) { 
		printf("Put code here...\n");
	 	if (0)
		for (int b=0;b<out->bands();b++) 
		{ /* LAS images are stored in "Band sequential" order on disk,
		   so it's easiest to just read one band at a time. */
			read_band(b);
		}
	}
	void read_band(int b) {
	}
	
	~self() {
		if (f) fclose(f);
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "image_input_meta",
  /* desc    */ "Read an image in the ASF .meta format--e.g., from asf_import.\n",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/07/13",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,image_input_meta)
