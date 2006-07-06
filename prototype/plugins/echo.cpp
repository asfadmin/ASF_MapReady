/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include <stdlib.h>

/* Plugin name */
#define self plugin_echo
class self : public asf::plugin {
	asf::parameter_int    *pint;
	asf::parameter_real   *preal;
	asf::parameter_string *pstring;
	asf::parameter        *pgeneric;
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin(param)
	{
		asf::optional(param,"int",&pint);
		asf::optional(param,"real",&preal);
		asf::optional(param,"string",&pstring);
		asf::optional(param,"generic",&pgeneric);
	}
	void execute(void) { 
		std::string out;
		if (pstring) out+=pstring->value;
		char tmp[20];
		if (pint) {sprintf(tmp,"%d",pint->value); out+=" "; out+=tmp;}
		if (preal) {sprintf(tmp,"%f",preal->value); out+=" "; out+=tmp;}
		printf("echo: %s\n",out.c_str());
		log("%s\n",out.c_str());
		if (pgeneric) {
			pgeneric->print(stdout);
		}
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "echo",
  /* desc    */ "Print output to screen and logfile",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/09/14",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,echo)
