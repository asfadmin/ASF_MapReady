/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"

/* Plugin name */
#define self textfile_write
class self : public asf::plugin {
	asf::parameter_filename *filename;
	asf::parameter *a;
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin(param)
	{
		asf::input(param,"filename",&filename);
		asf::input(param,"a",&a);
	}
	void execute(void) {
		FILE *f=fopen(filename->c_str(),"w");
		if (!f) asf::die(std::string("Can't create output file ")+std::string(*filename));
		fprintf(f,"ASF_TEXTWRITE");
		PUP::toTextFile p(f,true);
		asf::pup_type_packing(p,a->get_type());
		a->pup(p);
		fclose(f);
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "textfile_write",
  /* desc    */ "Write a parameter to an ASCII text file.",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/06/08",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,textfile_write)
