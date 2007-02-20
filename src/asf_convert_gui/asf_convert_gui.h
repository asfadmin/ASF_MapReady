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

#include "asf.h"

#define THUMB_SIZE 48
#define THUMB_SIZE_BIG 256
#define THUMBNAILS

enum OutputFormat
{
    OUTPUT_FORMAT_JPEG = 0,
    OUTPUT_FORMAT_PGM = 1,
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
    SCALING_METHOD_TRUNCATE = 2,
    SCALING_METHOD_HISTOGRAM_EQUALIZE = 3
};
    
enum ProjectionOptions
{
  PROJ_UTM = 0,
  PROJ_PS = 1,
  PROJ_ALBERS = 2,
  PROJ_LAMAZ = 3,
  PROJ_LAMCC = 4 // currently unavailable through GUI
};

enum Datums
{
    DATUM_WGS84 = 0,
    DATUM_NAD27 = 1,
    DATUM_NAD83 = 2
};

enum
{
  RESAMPLE_NEAREST_NEIGHBOR = 0,
  RESAMPLE_BILINEAR = 1,
  RESAMPLE_BICUBIC = 2
};
  

/* for win32, need __declspec(dllexport) on all signal handlers. */
#if !defined(SIGNAL_CALLBACK)
#  if defined(win32)
#    define SIGNAL_CALLBACK __declspec(dllexport)
#  else
#    define SIGNAL_CALLBACK
#  endif
#endif

typedef struct
{
  /* import */
  int input_data_format;
  int process_to_level1;

  /* transformations */
  int data_type;
  int output_db;
  int latitude_checked;
  double latitude_low;
  double latitude_hi;

  /* export */
  int export_is_checked;
  int output_format;
  int apply_scaling;
  int longest_dimension;
  int output_bytes;
  int scaling_method;
  int export_bands;
  char red[10];
  char green[10];
  char blue[10];

  /* geocode */
  int geocode_is_checked;
  int projection;
  int zone;
  double plat1;
  double plat2;
  double lat0;
  double lon0;
  double false_easting;
  double false_northing;

  int specified_height;
  double height;
  int specified_pixel_size;
  double pixel_size;
  int datum;
  int resample_method;
  int geocode_force;

  /* terrcorr options */
  int terrcorr_is_checked;
  int refine_geolocation_is_checked;
  char dem_file[2048];
  int specified_tc_pixel_size;
  double tc_pixel_size;
  int interp;
  int auto_water_mask_is_checked;
  int mask_file_is_checked;
  char mask_file[2048];
  int generate_layover_mask;
  int generate_dem;
  int do_radiometric;

  /* misc */
  int keep_files;
  int apply_metadata_fix;
}
Settings;

typedef struct
{
    gchar * prefix;
    gchar * suffix;
    gchar * scheme;
} NamingScheme;

extern int COL_DATA_FILE;
extern int COL_INPUT_THUMBNAIL;
extern int COL_OUTPUT_FILE;
extern int COL_STATUS;
extern int COL_LOG;

extern int COMP_COL_DATA_FILE;
extern int COMP_COL_OUTPUT_FILE;
extern int COMP_COL_OUTPUT_THUMBNAIL;
extern int COMP_COL_OUTPUT_THUMBNAIL_BIG;
extern int COMP_COL_STATUS;
extern int COMP_COL_LOG;

/********************************** Prototypes ******************************/

/* settings.c */
Settings * settings_get_from_gui();
void settings_apply_to_gui();
Settings * settings_copy(const Settings *);
int settings_equal(const Settings *, const Settings *);
const gchar * settings_get_size_argument(const Settings *);
const gchar * settings_get_latitude_argument(const Settings *);
const gchar * settings_get_apply_metadata_fix_argument(const Settings *);
const gchar * settings_get_output_bytes_argument(const Settings *s);
const gchar * settings_get_data_type_string(const Settings *);
const gchar * settings_get_data_type_arg_string(const Settings *);
const gchar * settings_get_input_data_format_string(const Settings *);
const gchar * settings_get_output_format_extension(const Settings *);
const gchar * settings_get_output_format_string(const Settings *);
const gchar * settings_get_geocode_options(const Settings *);
const gchar * settings_get_terrcorr_options(const Settings *);
const gchar * settings_get_projection_abbrev(const Settings *);
int settings_get_run_import(const Settings *);
int settings_get_run_export(const Settings *);
int settings_get_run_geocode(const Settings *);
int settings_get_run_terrcorr(const Settings *);
int  settings_get_output_format_can_be_thumbnailed(const Settings *s);
void settings_delete(Settings *);
void settings_update_dem(Settings *s, const char *output_path, int is_first);
void settings_update_mask(Settings *s, const char *output_path, int is_first);
char * settings_to_config_file(const Settings *s,
			     const gchar *input_file, const gchar *output_file,
			     const gchar *output_path, const gchar *tmp_dir);

/* find_in_path.c */
gchar *find_in_path(gchar * file);
gchar *find_dir_in_path(gchar * file);

/* execute.c */
int do_system_exec(const char *cmd);
void process_items_from_list(GList *, gboolean);

/* callbacks.c */
void output_format_combobox_changed();
void input_data_format_combobox_changed();
void show_execute_button(gboolean);
void latitude_checkbutton_toggle();
void input_data_type_changed();
void rgb_settings_changed();

/* utility.c */
void setup_band_comboboxes();
void set_combo_box_item(GtkWidget *, gint);
gint get_combo_box_item(GtkWidget *);
void message_box(const gchar *);
gchar * meta_file_name(const gchar *);
char *getPath(const char *);
GtkWidget *get_widget_checked(const char *widget_name);
void set_combo_box_item_checked(const char *, gint);
void rgb_combo_box_setup();
void rb_select(const char *, gboolean);

/* dnd.c */
void setup_dnd();

/* popup_menu.c */
gboolean get_iter_to_first_selected_row();
void setup_popup_menu();
void show_please_select_message();
void set_toolbar_images();

/* file_list.c */
void setup_files_list(int, char **);
gboolean add_to_files_list(const gchar *);
gboolean add_to_files_list_iter(const gchar *, GtkTreeIter *);
void update_all_extensions();
void set_output_name(GtkTreeIter *, const gchar *);
gboolean is_L_file(const gchar *);
void show_queued_thumbnails();
int has_prepension(const gchar *);
void move_to_completed_files_list(GtkTreeIter *, GtkTreeIter *, const gchar *);
void move_from_completed_files_list(GtkTreeIter *);

/* help.c */
char * escapify(const char * s);

/* rename_output.c */
gboolean rename_selected_output_filename();
void do_rename_selected(const gchar *new_name);

/* file_selection.c */

/* state.c */

/* naming_scheme.c */
NamingScheme * naming_scheme_new(const gchar *, const gchar *,
                                 const gchar *);
NamingScheme * naming_scheme_default();
void naming_scheme_delete(NamingScheme *);
gchar * naming_scheme_apply(const NamingScheme *, const gchar *);
gboolean naming_schemes_equal(const NamingScheme *, const NamingScheme *);
NamingScheme * naming_scheme_copy(const NamingScheme *);

/* metadata.c */
gchar * build_asf_metadata_filename(gchar * name);
gchar * build_ceos_metadata_filename(gchar * name);
void show_asf_meta_data(gchar *);
void show_ceos_meta_data(gchar *);

/* summary.c */
void update_summary();

/* geocode.c */
const char * geocode_options_string(const Settings * settings);
void geocode_options_changed();
const char * datum_string(int datum);
const char * resample_method_string(int resample_method);

/* win_font.c */
void set_font();

/* projfile.c */
project_parameters_t * 
  load_selected_predefined_projection_parameters(int projection);

void set_predefined_projections(int projection);
void release_predefined_projections();

/* view_output.c */
void show_output_image(const gchar * filename);

/* share.c */
char * find_in_bin(const char *);
char * find_in_share(const char *);
void print_share_dir();

/* terrcorr.c */
void terrcorr_options_changed();
const char * terrcorr_options_string(const Settings *settings);
void default_to_terrcorr_on();

/* log.c */
void show_log(gchar * log_txt, gchar * data_file);

/* blow_up.c */
typedef struct {        // for the fake motion signal callback
    gboolean is_valid;
    GtkWidget *widget;
    GdkEventMotion *event;
    gdouble x, y;			/* x and y of fake event.  */
} fake_motion_signal_args_t;

gboolean files_list_motion_notify_event_handler(GtkWidget *,
             GdkEventMotion *, gpointer user_data);
gboolean files_list_leave_notify_event_handler(GtkWidget *widget,
             GdkEventCrossing *, GtkWidget *);
gboolean files_list_scroll_event_handler (GtkWidget *, GdkEventScroll *,
             gpointer user_data);

gboolean completed_files_list_motion_notify_event_handler(GtkWidget *,
             GdkEventMotion *, gpointer user_data);
gboolean completed_files_list_leave_notify_event_handler(GtkWidget *widget,
             GdkEventCrossing *, GtkWidget *);
gboolean completed_files_list_scroll_event_handler (GtkWidget *,
             GdkEventScroll *, gpointer user_data);

#ifdef win32
#ifdef DIR_SEPARATOR
#undef DIR_SEPARATOR
#endif
extern const char DIR_SEPARATOR;
#endif

extern const char PATH_SEPATATOR;

/* these are our global variables ... */

/* xml version of the .glade file */
extern GladeXML *glade_xml;

/* The files listings */
extern GtkListStore *list_store;
extern GtkListStore *completed_list_store;

/* TRUE during processing */
extern gboolean processing;

/* The settings when the user clicked "Execute" (or, "Load") */
extern Settings * settings_on_execute;

/* where should we put the generated files */
extern gchar * output_directory;

/* current naming scheme */
extern NamingScheme * current_naming_scheme;

/* are thumbnails supported ? */
extern gboolean use_thumbnails;
#endif
