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

// The global vars
extern GladeXML *glade_xml;
extern const char PATH_SEPARATOR;
extern const char DIR_SEPARATOR;

// A couple enums
enum OutputFormat
{
    OUTPUT_FORMAT_JPEG = 0,
    OUTPUT_FORMAT_PPM = 1,
    OUTPUT_FORMAT_TIFF = 2,
    OUTPUT_FORMAT_GEOTIFF = 3
};

enum ProjectionOptions
{
    PROJ_UTM = 0,
    PROJ_PS = 1,
    PROJ_ALBERS = 2,
    PROJ_LAMAZ = 3,
    PROJ_LAMCC = 4
};

enum Datums
{
    DATUM_WGS84 = 0,
    DATUM_NAD27 = 1,
    DATUM_NAD83 = 2
};

// Prototypes

// ait.c
void show_summary(int show);
void message_box(const char *format, ...);

// config.c
dem_config *get_settings_from_gui();
void apply_settings_to_gui(dem_config *cfg, const char *cfg_name);

// projfile.c
project_parameters_t *
load_selected_predefined_projection_parameters(int projection);
void set_predefined_projections(int projection);
void release_predefined_projections();

// geocode.c
void geocode_options_changed();
const char * datum_string(int datum);
const char * resample_method_string(resample_method_t resample_method);

#endif
