#ifndef __ASF_CONVERT_GUI_H
#define __ASF_CONVERT_GUI_H

#define _GNU_SOURCE

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <gtk/gtk.h>
#include <glade/glade.h>
#include <glib.h>
#include <glib/gprintf.h>

/* make these enums once we've settled down */
#define OUTPUT_FORMAT_JPEG 0
#define OUTPUT_FORMAT_PPM 1
#define OUTPUT_FORMAT_GEOTIFF 2
#define OUTPUT_FORMAT_ASF_INTERNAL 3
#define OUTPUT_FORMAT_CEOS 4

#define INPUT_FORMAT_CEOS_LEVEL0 0
#define INPUT_FORMAT_CEOS_LEVEL1 1
#define INPUT_FORMAT_STF 2
#define INPUT_FORMAT_ESRI 3
#define INPUT_FORMAT_ENVI 4
#define INPUT_FORMAT_COMPLEX 5
#define INPUT_FORMAT_ASF_INTERNAL 6

#define INPUT_TYPE_SIGMA 0
#define INPUT_TYPE_BETA 1
#define INPUT_TYPE_GAMMA 2
#define INPUT_TYPE_AMP 3
#define INPUT_TYPE_POWER 4

/* for win32, need __declspec(dllexport) on all signal handlers */
#if !defined(SIGNAL_CALLBACK)
#  if defined(win32)
#    define SIGNAL_CALLBACK __declspec(dllexport)
#  else
#    define SIGNAL_CALLBACK
#  endif
#endif

#include "settings.h"
#include "find_in_path.h"
#include "execute.h"
#include "callbacks.h"
#include "utility.h"
#include "dnd.h"
#include "popup_menu.h"
#include "file_list.h"
#include "help.h"
#include "rename_output.h"
#include "file_selection.h"
#include "state.h"

extern const char DIR_SEPARATOR;
extern const char PATH_SEPATATOR;

/* these are our global variables ... */

/* xml version of the .glade file */
extern GladeXML *glade_xml;

/* the files listing */
extern GtkListStore *list_store;

/* TRUE during processing until the user clicks "STOP" */
extern gboolean keep_going;

/* TRUE during processing */
extern gboolean processing;

/* The settings when the user clicked "Execute" (or, "Load") */
extern Settings * settings_on_execute;

#endif
