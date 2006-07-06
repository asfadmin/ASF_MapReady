/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include <stdlib.h>

#define self plugin_assert_equal  /* Plugin name */
class self : public asf::plugin {
	asf::parameter *a, *b;
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin(param)
	{
		asf::input(param,"a",&a);
		asf::input(param,"b",&b);
	}
	void execute(void) { 
		const char *diff=a->differences(*b);
		if (diff!=NULL) {
			log(0,"assert_equal failed!  The arguments %s!\n",diff);
			a->print(stderr);
			b->print(stderr);
			asf::die("assert_equal failed");
		}
	}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "assert_equal",
  /* desc    */ "Compare two arbitrary input objects for equality",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/09/15",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,assert_equal)
