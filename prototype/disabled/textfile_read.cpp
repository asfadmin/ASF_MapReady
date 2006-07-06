/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"

#define me_class textfile_read  /* Plugin name.  "me class!" */
class me_class : public asf::plugin {
	asf::parameter_filename *filename;
	asf::parameter *a;
public:
	ASF_plugin_class(self)
	me_class(asf::plugin_parameters &param) 
		:asf::plugin(param)
	{
		asf::input(param,"filename",&filename);
		asf::output(param,"a",&a);
	}
	void execute(void) {
		FILE *f=fopen(filename->c_str(),"r");
		if (!f) asf::die("Can't read input file "+std::string(*filename));
		char fileType[100],paramType[100];
		double version;
		if (3!=fscanf(f,"%99s %99s v%lf\n",fileType,paramType,&version))
			asf::die("Bad file header format for "+std::string(*filename));
		if (0!=strcmp(fileType,"ASF_TEXTWRITE"))
			asf::die("Bad file header field for "+std::string(*filename));
		//asf::registry &r=get_parameters().
		// FIXME: find registry, create object of type paramType
		
		a->read(f);
		fclose(f);
	}
};

/* static */ const asf::type me_class::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "textfile_read",
  /* desc    */ "Read a parameter from an ASCII text file.",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/06/08",
  /* version */ 1.0
);
ASF_plugin_def(me_class)
ASF_plugin_dll(me_class,textfile_read)
