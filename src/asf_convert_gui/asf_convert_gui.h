#ifndef __ASF_CONVERT_GUI_H
#define __ASF_CONVERT_GUI_H

#define _GNU_SOURCE
#define USE_GTK_22

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <gtk/gtk.h>
#include <glade/glade.h>
#include <glib.h>
#include <glib/gprintf.h>

enum OutputFormat
{
    OUTPUT_FORMAT_JPEG = 0,
    OUTPUT_FORMAT_PPM = 1,
    OUTPUT_FORMAT_TIFF = 2,
    OUTPUT_FORMAT_GEOTIFF = 3,
    OUTPUT_FORMAT_ASF_INTERNAL = 4,
    OUTPUT_FORMAT_CEOS = 5
};

enum InputFormat
{
    INPUT_FORMAT_CEOS_LEVEL0 = 0,
    INPUT_FORMAT_CEOS_LEVEL1 = 1,
    INPUT_FORMAT_STF = 2,
    INPUT_FORMAT_COMPLEX = 3,
    INPUT_FORMAT_ESRI = 4,
    INPUT_FORMAT_ENVI = 5,
    INPUT_FORMAT_ASF_INTERNAL = 6
};

enum InputType
{
    INPUT_TYPE_SIGMA = 0,
    INPUT_TYPE_BETA = 1,
    INPUT_TYPE_GAMMA = 2,
    INPUT_TYPE_AMP = 3,
    INPUT_TYPE_POWER = 4
};

enum ScalingMethod
{
    SCALING_METHOD_SIGMA = 0,
    SCALING_METHOD_MINMAX = 1,
    SCALING_METHOD_TRUNCATE = 2
};
/*    
enum ProjectionOptions
{
    UNIVERSAL_TRANSVERSE_MERCATOR = 0,
    POLAR_STEREOGRAPHIC = 1,
    LAMBERT_AZIMUTHAL_EQUAL_AREA = 2,
    LAMBERT_CONFORMAL_CONIC = 3,
    ALBERS_CONICAL_EQUAL_AREA = 4
};
*/
enum Datums
{
    DATUM_WGS84 = 0,
    DATUM_NAD27 = 1,
    DATUM_NAD83 = 2
};

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
#include "naming_scheme.h"
#include "metadata.h"
#include "summary.h"
#include "geocode.h"
#include "win_font.h"
#include "projfile.h"

extern const char DIR_SEPARATOR;
extern const char PATH_SEPATATOR;

/* these are our global variables ... */

/* xml version of the .glade file */
extern GladeXML *glade_xml;

/* the files listing */
extern GtkListStore *list_store;

/* TRUE during processing */
extern gboolean processing;

/* The settings when the user clicked "Execute" (or, "Load") */
extern Settings * settings_on_execute;

/* where should we put the generated files */
extern gchar * output_directory;

/* current naming scheme */
extern NamingScheme * current_naming_scheme;

#endif
