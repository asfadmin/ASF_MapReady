#include <stdio.h>
#include "asf/plugin.h"

ASF_PLUGIN_EXPORT int asf_plugin_init(void *v) {
	fprintf(stderr,"crap_bad_init.dll> Loaded plugin\n");
	return asf::plugin_init_function_ok;
}
