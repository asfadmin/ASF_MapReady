/* UNGH.  Windows requires "__declspec(dllimport)" or "__declspec(dllexport)"
for external access to functions and classes inside a DLL.  OSL_DLL can be
made to expand to either of these, or (the usual case) nothing. */
#ifndef OSL_DLL
#  define OSL_DLL  /* empty, for non-Windows and/or non-DLL use */
#endif
