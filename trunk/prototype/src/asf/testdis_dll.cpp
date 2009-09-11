/**
ASF plugin registration test

by Orion Sky Lawlor, olawlor@acm.org, 2005/09/01 (Public Domain)
*/
#include "asf/plugin.h"
#include "asf/plugin_loader.h"

ASF_PLUGIN_EXPORT int main() {
	asf::registry r;
	asf::system_register(r);
	asf::plugin_loader l;
	r.merge(l.load());
	r.print_types();
}
