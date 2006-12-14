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
#include "asf_nan.h"

// Fixed-width font, used in metadata & log areas
#if defined(win32)
#define FW_FNT_NAME "Courier"
#else
#define FW_FNT_NAME "Mono"
#endif

// The global vars
extern GladeXML *glade_xml;
extern const char PATH_SEPARATOR;
extern const char DIR_SEPARATOR;
extern GtkListStore *images_list;

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

typedef struct ait_params 
{
    char *name;

    // main config settings
    dem_config *cfg;

    // geocoding stuff
    meta_projection *proj; // NULL if geocoding off
    resample_method_t resample_method;
    int force;
} ait_params_t;

// Used by the "Browse" fuctionality - the callback should take the selected
// file and populate the appropriate widgets.
typedef void browse_callback(char *selected_file);

// Prototypes

// ait.c
void show_summary(int show);
void message_box(const char *format, ...);
GtkWidget *get_widget_checked(const char *widget_name);
void update_everything();
char *meta_file_name(const char *data_file_name);
void file_into_textview(char *filename, const char *textview_name);

// config.c
ait_params_t *get_settings_from_gui();
void apply_settings_to_gui(ait_params_t *params);
void update_summary();
void write_settings(ait_params_t *params);
ait_params_t *read_settings(char *config_file);
void free_ait_params(ait_params_t *params);

// projfile.c
project_parameters_t *
load_selected_predefined_projection_parameters(int projection);
void set_predefined_projections(int projection);
void release_predefined_projections();
meta_projection *read_proj_file(char *filename, ait_params_t *ait_params);

// geocode.c
void geocode_options_changed();
const char * datum_string(int datum);
const char * resample_method_string(resample_method_t resample_method);

// browse.c
void browse(browse_callback bcb);

// imagery.c
void setup_images_treeview();
void clear_image_list();
int add_to_image_list(const char * data_file);
int add_to_image_list2(const char * path, const char * data_file);

#endif
