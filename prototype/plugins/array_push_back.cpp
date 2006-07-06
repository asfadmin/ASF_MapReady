/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include <stdlib.h>

/* Plugin name */
#define self plugin_array_push_back
class self : public asf::plugin {
	asf::parameter    *value; /**< Generic input parameter */
	asf::parameter_array_generic *array; /**< array output parameter */
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin(param)
	{
		asf::input(param,"value",&value);
		asf::output(param,"array",&array);
	}
	void execute(void) { 
		/* We make a copy of our value, because:
			- Somebody may change value later.
			- Array wants to *own* the parameter you give it.
		*/
		asf::parameter *p=value->clone();
		array->push_back(p);
	}
	/// This class is a sink, because it has side effects on its output.
	virtual bool has_side_effects(void) const {return true;}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "array_push_back",
  /* desc    */ "Add a value to an array",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2006/06/12",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,array_push_back)
