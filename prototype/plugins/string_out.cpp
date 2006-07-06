/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"

/* Plugin name */
#define self string_out
class self : public asf::plugin {
	asf::parameter_filename *filename;
	asf::parameter_string *pstring;
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin(param)
	{
		asf::input(param,"filename",&filename);
		asf::input(param,"string",&pstring);
	}
	void execute(void) { 
		log("Writing string '%s' to a file named '%s'.\n",
			pstring->c_str(),filename->c_str());
		FILE *f=fopen(filename->c_str(),"w");
		if (!f) asf::die(std::string("Can't create output file ")+std::string(*filename));
		fprintf(f,"%s",pstring->c_str());
		fclose(f);
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "string_out",
  /* desc    */ "Write a string to an ASCII text file.",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/06/06",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,string_out)
