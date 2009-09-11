/**
ASF plugin registration test

by Orion Sky Lawlor, olawlor@acm.org, 2005/09/01 (Public Domain)
*/
#include "asf/plugin.h"

/******************* Random integer source ************/
class test_plugin1 : public asf::plugin {
	asf::parameter_int *pnaz; /* Output parameter */
public:
	ASF_plugin_class(test_plugin1)
	test_plugin1(asf::plugin_parameters &param) 
		:asf::plugin(param)
	{
		asf::output(param,"pnaz",&pnaz);
	}
	void execute(void) { 
		int r=rand();
		*pnaz=r;
		log("inside execute routine: pnaz=%d\n",r);
	}
};

/* static */ const asf::type test_plugin1::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "test_plugin1",
  /* desc    */ "A silly test plugin: generates random ints to 'pnaz' parameter",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/09/01",
  /* version */ 1.0
);
ASF_plugin_def(test_plugin1)


/************** Consumes bogus int "booga" *************/
class test_plugin2 : public asf::plugin {
	asf::parameter_int *booga; /* This gets printed out */
public:
	ASF_plugin_class(test_plugin2)
	test_plugin2(asf::plugin_parameters &param) 
		:asf::plugin(param)
	{
		asf::input(param,"booga",&booga);
	}
	void execute(void) { 
		int b=*booga;
		log("inside execute routine: booga=%d\n",b);
	}
};

/* static */ const asf::type test_plugin2::static_type(
  /* parent  */ &asf::plugin::static_type,
  /* name    */ "test_plugin2",
  /* desc    */ "A silly test plugin",
  /* authors */ "v1.0 by Orion Lawlor (olawlor@acm.org) 2005/09/01",
  /* version */ 1.0
);
ASF_plugin_def(test_plugin2)


ASF_PLUGIN_EXPORT int main() {
	asf::registry r;
	asf::system_register(r);
	ASF_plugin_reg(r,test_plugin1)
	ASF_plugin_reg(r,test_plugin2)
	printf("\n------ Registered types:\n");
	r.print_types();
	
	printf("\n------ Signature of plugins:\n");
	r.plugin_signature("test_plugin1").print();
	r.plugin_signature("test_plugin2").print();

	printf("\n------ Building plugins\n");
	asf::plugin_parameter_list param1;
	asf::parameter *shared=new asf::parameter_int;
	asf::parameter *verbosity=new asf::parameter_int(10);
	param1.add("pnaz",shared); 
	param1.add("log_verbosity",verbosity);
	asf::plugin *plug1=r.plugin_factory("test_plugin1")(param1);
	
	asf::plugin_parameter_list param2;
	param2.add("booga",shared); 
	param2.add("log_verbosity",verbosity);
	asf::plugin *plug2=r.plugin_factory("test_plugin2")(param2);

	for (int i=0;i<3;i++) {
		printf("\n------ Running plugins\n");
		plug1->execute();
		plug2->execute();
	}
	
	delete plug1;
	delete plug2;
}

