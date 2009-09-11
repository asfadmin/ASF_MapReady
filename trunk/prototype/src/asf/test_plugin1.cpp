/**
ASF plugin registration test

by Orion Sky Lawlor, olawlor@acm.org, 2005/09/01 (Public Domain)
*/
#include "asf/plugin.h"

/** Tiniest possible plugin */
class test_plugin1 : public asf::plugin {
	asf::parameter_int *booga; /* Some random parameter */
public:
	ASF_plugin_class(test_plugin1)
	test_plugin1(asf::plugin_parameters &param) 
		:asf::plugin(param)
	{
		asf::input(param,"booga",&booga);
	}
	void execute(void) { 
		int b=*booga;
		log("inside execute routine: booga=%d\n",b);
	}
};

/* static */ const asf::type test_plugin1::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "test_plugin1",
  /* desc    */ "A silly test plugin",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/09/01",
  /* version */ 1.0
);
ASF_plugin_def(test_plugin1)


ASF_PLUGIN_EXPORT int main() {
	asf::registry r;
	asf::system_register(r);
	ASF_plugin_reg(r,test_plugin1)
	printf("------ Registered types:\n");
	r.print_types();
	
	const char *plugName="test_plugin1";
	printf("------ Signature of plugin %s:\n",plugName);
	r.plugin_signature(plugName).print();
	printf("------ Running plugin\n");
	asf::plugin_parameter_list param;
	asf::parameter_int *booga=new asf::parameter_int(3);
	param.add("booga",booga); /* unused */
	param.add("log_verbosity",new asf::parameter_int(3));
	
	asf::plugin *plug=r.plugin_factory(plugName)(param);
	plug->execute();
	delete plug;
}

