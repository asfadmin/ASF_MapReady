#ifndef INCL_WINSHL_H
#define INCL_WINSHL_H

#ifdef win32
#define BYTE __byte
#define POINT __point
#include <windows.h>
#include <shellapi.h>
#include <shlobj.h>
#undef BYTE
#undef POINT
#endif

#endif
