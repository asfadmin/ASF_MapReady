#include "osl/dll.h"

/* Called by dll's */
OSL_DLL_EXPORT int sub_export(int i) {
	return 100*i;
}
