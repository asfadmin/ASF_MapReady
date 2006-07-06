#include <stdio.h>
#include <osl/dll.h>

/* Routine exported from sub library */
extern "C" int sub_export(int i);

/* Routine exported from main program */
extern "C" int main_export(int i);

OSL_DLL_EXPORT int foo(void) {
	printf("Inside dll: calling sub export (%p)\n",sub_export);
	int s=sub_export(2);
	printf("Calling main export (%p)\n",main_export);
	int m=main_export(3);
	printf("Returning\n");
	return 10000+s+m;
}

