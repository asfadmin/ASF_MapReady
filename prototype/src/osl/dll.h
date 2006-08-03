/**
Support dynamically-loaded libraries (DLLs), or shared libraries.
C or C++ interface and implementation.
Orion Sky Lawlor, olawlor@acm.org, 2005/8/5 (Public Domain)
*/
#ifndef __OSL_DLL_H
#define __OSL_DLL_H

#ifdef __cplusplus
extern "C" {
#endif

/** A handle to a dynamic library. */
typedef void *osl_dll;

/** Open this dynamic library file. Returns NULL if not present or not a library. */
osl_dll osl_dll_open(const char *dllName);


/**
    Look up this C symbol in the dynamic library file.  
    Returns NULL if the symbol isn't there.
*/
void *osl_dll_lookup(osl_dll dll,const char *symName);

/** Close this dynamic library, and unload all symbols. */
void osl_dll_close(osl_dll dll);

/** Use this macro to declare routines inside a dll */
#ifdef WIN32
# define OSL_DLL_EXPORT extern "C" __declspec(dllexport)
#else
# define OSL_DLL_EXPORT extern "C" 
#endif

#ifdef __cplusplus
};
#endif

#endif
