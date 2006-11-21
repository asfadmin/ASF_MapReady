#ifndef INCLUDED_AIT_H
#define INCLUDED_AIT_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <unistd.h>
#include <gtk/gtk.h>
#include <glade/glade.h>
#include <glib.h>
#include <glib/gprintf.h>
#include <sys/wait.h>

/* for win32, need __declspec(dllexport) on all signal handlers */
#if !defined(SIGNAL_CALLBACK)
#  if defined(win32)
#    define SIGNAL_CALLBACK __declspec(dllexport)
#  else
#    define SIGNAL_CALLBACK
#  endif
#endif

#define IPS_GUI_VERSION "0.0.1"

#if defined(win32)
#include <pango/pango.h>
#define BYTE __byte
#include "asf.h"
#undef BYTE
#include <Windows.h>
#undef DIR_SEPARATOR
#else /* #if defined(win32) */
#include "asf.h"
#if defined(DIR_SEPARATOR)
#undef DIR_SEPARATOR
#endif
#endif /* #if defined(win32) */

#include "ips.h"

extern GladeXML *glade_xml;
extern const char PATH_SEPARATOR;
extern const char DIR_SEPARATOR;

// config.c
dem_config *get_settings_from_gui();

#endif
