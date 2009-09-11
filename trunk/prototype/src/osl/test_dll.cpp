#include <stdio.h>
#include "osl/dll.h"
#include "osl/dll.cpp"

typedef int (*foo_fn_t)(void);

/* Called by libraries */
int main_export(int i) {
	return 1000*i;
}

int main() {
	printf("About to load library\n");
	void *libm=osl_dll_open("./test_dll_server.dll");
	printf("Loaded library at handle %p\n",libm);
	foo_fn_t foo_fn=(foo_fn_t)osl_dll_lookup(libm,"foo");
	printf("Loaded foo at address %p\n",foo_fn);
	double result=(foo_fn)();
	printf(" dynamically loaded foo returns %f\n"
			" foo is at %p, libm handle is %p\n",
			result, (void *)foo_fn,libm);
	osl_dll_close(libm);
	return 0;
}
