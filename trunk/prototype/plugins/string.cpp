/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include "asf/image.h"
#include <stdlib.h>

/* Plugin name */
#define self plugin_string

std::string makeString(asf::parameter *p) {
	asf::parameter_string *ps=dynamic_cast<asf::parameter_string *>(p);
	if (ps) return *ps;
	char tmp[30];
	asf::parameter_int *pi=dynamic_cast<asf::parameter_int *>(p);
	if (pi) {sprintf(tmp,"%d",pi->value); return tmp;}
	asf::parameter_real *pr=dynamic_cast<asf::parameter_real *>(p);
	if (pr) {sprintf(tmp,"%.6g",pr->value); return tmp;}
	asf::parameter_pixel_image *pp=dynamic_cast<asf::parameter_pixel_image *>(p);
	if (pp) {
		sprintf(tmp,"%s: %d x %d metapixels",
			pp->get_type()->name(),
			pp->total_meta_bounds().width(),
			pp->total_meta_bounds().height());
		return tmp;
	}
	sprintf(tmp,"%s",p->get_type()->name());
	return tmp;
}

class self : public asf::plugin {
	asf::parameter        *a;
	asf::parameter        *b;
	asf::parameter        *c;
	asf::parameter        *d;
	asf::parameter        *e;
	asf::parameter_string *out;
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin(param)
	{
		asf::optional(param,"a",&a);
		asf::optional(param,"b",&b);
		asf::optional(param,"c",&c);
		asf::optional(param,"d",&d);
		asf::optional(param,"e",&e);
		asf::output(param,"out",&out);
	}
	void execute(void) { 
		std::string o;
		if (a) o+=makeString(a);
		if (b) o+=makeString(b);
		if (c) o+=makeString(c);
		if (d) o+=makeString(d);
		if (e) o+=makeString(e);
		*out=o;
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "string",
  /* desc    */ "Make a string out of the input arguments (which can be of any type)",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/06/08",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,string)
