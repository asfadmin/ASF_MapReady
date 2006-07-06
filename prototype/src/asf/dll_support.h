/**
Defines (stupid) macros needed to build shared libraries
on Windows machines.

  Orion Sky Lawlor, olawlor@acm.org, 2005/11/16
 */
#ifndef __ASF_DLL_SUPPORT_H
#define __ASF_DLL_SUPPORT_H

/**
 This macro should be used before the return type for 
 all subroutines defined in a dll and called from outside.
*/
#ifdef WIN32
#  define ASF_PLUGIN_EXPORT extern "C" __declspec(dllexport)
#else /* UNIX-- nothing special needed */
#  define ASF_PLUGIN_EXPORT extern "C"
#endif

/**
 The ASF_COREDLL macro is needed to make Windows DLLs work.
 When building the asf core dll, it expands to 
	__declspec(dllexport)
 And when building libraries that depend on the core dll, it expands to
    __declspec(dllimport)
 It's important that *all* externally-used routines and 
 classes use ASF_COREDLL, or they won't be visible from outside 
 on Windows.
 
 Typical uses:
    class ASF_COREDLL foo_class { ... };
    ASF_COREDLL void foo_routine(...);
    ASF_COREDLL void foo_routine(...) {}
*/
#ifdef WIN32
#  ifdef ASF_BUILD_COREDLL /* building the core dll */
#     define ASF_COREDLL __declspec(dllexport)
#  else /* using the core dll */
#     define ASF_COREDLL __declspec(dllimport)
#  endif
#else /* UNIX exports all symbols automatically */
#  define ASF_COREDLL /* empty */
#endif

/* Use COREDLL macro above for all osl and pup routines */
#define OSL_DLL ASF_COREDLL
#define PUP_DLL ASF_COREDLL


#endif
