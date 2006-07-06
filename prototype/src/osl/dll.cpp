/**
Support loading up shared libraries on all platforms.
Orion Sky Lawlor, olawlor@acm.org, 2005/8/5 (Public Domain)
*/
#include "osl/dll.h"

#ifdef WIN32 /**************** Windows ****************
Link with "cl /LD foo.cpp /ofoo.dll "
*/
#include <windows.h>

/** Open this dynamic library file. Returns NULL if not present or not a library. */
osl_dll osl_dll_open(const char *dllName)
{
	return (osl_dll)LoadLibrary(dllName);
}

/** Look up this C symbol in the dynamic library file.  
    Returns NULL if the symbol isn't there.
*/
void *osl_dll_lookup(osl_dll dll,const char *symName)
{
	return GetProcAddress((HMODULE)dll,symName);
}

/** Close this dynamic library, and unload all symbols. */
void osl_dll_close(osl_dll dll)
{
	FreeLibrary((HMODULE)dll);
}



#elif defined(__APPLE__) /************ MACH "dyld" version (MacOS X) ***************
Note: MacOS 10.3 and later can just use the dlopen version...
Link with "gcc -dynamiclib foo.cpp -o foo.so"
*/ 
#include <mach-o/dyld.h>
#include <stdlib.h>
#include <string.h>

/** Open this dynamic library file. Returns NULL if not present or not a library. */
osl_dll osl_dll_open(const char *dllName)
{
	return (void *)NSAddImage(dllName,0);
}

/** Look up this C symbol in the dynamic library file.  
    Returns NULL if the symbol isn't there.
*/
void *osl_dll_lookup(osl_dll dll,const char *symName)
{
	char symNameUnderscore[1024];
	NSSymbol sym;
	sprintf(symNameUnderscore,"_%s",symName); /* why? */
	sym=NSLookupSymbolInImage((const struct mach_header *)dll,
		symNameUnderscore,
		NSLOOKUPSYMBOLINIMAGE_OPTION_RETURN_ON_ERROR);
	if (sym==NULL) return NULL;
	else return NSAddressOfSymbol(sym);
}

/** Close this dynamic library, and unload all symbols. */
void osl_dll_close(osl_dll dll)
{
	/* NSFreeImage(dll); */
}
#else /************ UNIX dlopen version ****************/
#include <dlfcn.h>

/** Open this dynamic library file. Returns NULL if not present or not a library. */
osl_dll osl_dll_open(const char *dllName)
{
	return dlopen(dllName,RTLD_LAZY);
}

/** Look up this C symbol in the dynamic library file.  
    Returns NULL if the symbol isn't there.
*/
void *osl_dll_lookup(osl_dll dll,const char *symName)
{
	return dlsym(dll,symName);
}

/** Close this dynamic library, and unload all symbols. */
void osl_dll_close(osl_dll dll)
{
	dlclose(dll);
}

#endif

