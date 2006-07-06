/**
ASF plugin registration test

by Orion Sky Lawlor, olawlor@acm.org, 2005/09/07 (Public Domain)
*/
#include "asf/plugin.h"

/** Tiniest possible plugin */
class crap_test1 : public asf::plugin {
	asf::parameter_int *booga; /* Some random parameter */
public:
	crap_test1(asf::plugin_parameters &param) 
		:asf::plugin(param)
	{
		asf::input(param,"booga",&booga);
	}
	void execute(void) { 
		int b=*booga;
		log("inside execute routine: booga=%d\n",b);
	}
	
	static const asf::type static_type;
	ASF_plugin_class(crap_test1)
};

/* static */ const asf::type crap_test1::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "crap_test1",
  /* desc    */ "A silly test plugin",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/09/01",
  /* version */ 1.0
);
ASF_plugin_def(crap_test1)
ASF_plugin_dll(crap_test1,crap_test1)

