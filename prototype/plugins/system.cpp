/* ASF plugin: see static_type at bottom of file for author and purpose */
#include "asf/plugin.h"
#include <stdlib.h>

/* Plugin name */
#define self plugin_system
class self : public asf::plugin {
	asf::parameter_string *command; /* Command to execute */
	asf::parameter_int *returnCode; /* Return code from command */
public:
	ASF_plugin_class(self)
	self(asf::plugin_parameters &param) 
		:asf::plugin(param)
	{
		asf::input(param,"command",&command);
		asf::output(param,"return",&returnCode);
	}
	void execute(void) { 
		log("Executing command: %s\n",command->c_str());
		int ret=system(command->c_str());
		if (ret==-1) asf::die("Fork failed for command "+ (std::string)*command);
		log("Command returned %d\n",ret);
		*returnCode=ret;
	}
	
	/// This class is a sink, even though it has outputs,
	///  because most commands have an effect on the outside world.
	virtual bool has_side_effects(void) const {return true;}
};

/* static */ const asf::type self::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "system",
  /* desc    */ "Execute a command on the command line using system(2).",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/09/08",
  /* version */ 1.0
);
ASF_plugin_def(self)
ASF_plugin_dll(self,system)
