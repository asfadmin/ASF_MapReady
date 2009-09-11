/* This file is used for Windows DLL generation.
All the pup classes are declared with "PUP_DLL",
which must be set to "__declspec(dllimport)" for users,
or "__declspec(dllexport)" when building pup itself.
*/
#include "asf/dll_support.h"  /* pick up real dll support */

#ifndef PUP_DLL 
#  define PUP_DLL /* empty */
#endif
