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
#include "asf_meta.h"
#include "xml_util.h"
#include "envi.h"
#include "asf_import.h"

#define THUMB_SIZE 48
#define THUMB_SIZE_BIG 512
#define THUMBNAILS

/* Gtk 2.6 has a very nice file chooser */
/* On Windows, though, we'll still use the native one */
#if GTK_MAJOR_VERSION >= 2 && GTK_MINOR_VERSION >= 6 && !defined(win32)
#define USE_GTK_FILE_CHOOSER
#endif

// must match Glade's order
enum OutputFormat
{
    OUTPUT_FORMAT_JPEG = 0,
    OUTPUT_FORMAT_PNG = 1,
    OUTPUT_FORMAT_PGM = 2,
    OUTPUT_FORMAT_TIFF = 3,
    OUTPUT_FORMAT_GEOTIFF = 4,
    OUTPUT_FORMAT_POLSARPRO = 5,
    OUTPUT_FORMAT_ASF_INTERNAL = 6,
    OUTPUT_FORMAT_CEOS = 7
};

enum InputFormat
{
    //INPUT_FORMAT_CEOS_LEVEL0 = 0, // Removed 8-12-2008
    INPUT_FORMAT_CEOS_LEVEL1 = 0,
    //INPUT_FORMAT_STF = 1,
    INPUT_FORMAT_GEOTIFF = 1,
    //INPUT_FORMAT_COMPLEX
    INPUT_FORMAT_ASF_INTERNAL = 2,
    INPUT_FORMAT_AIRSAR = 3,
    INPUT_FORMAT_ESRI = 4, // not implemented
    INPUT_FORMAT_ENVI = 5,  // not implemented
    INPUT_FORMAT_POLSARPRO = 6,
    INPUT_FORMAT_TERRASARX = 7,
    INPUT_FORMAT_RADARSAT2 = 8,
    INPUT_FORMAT_GAMMA = 9,
    INPUT_FORMAT_ROIPAC = 10,
    INPUT_FORMAT_ALOS_MOSAIC = 11
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
  PROJ_LAMCC = 4,
  PROJ_MER = 5,
  PROJ_EQR = 6
};

enum Datums
{
    DATUM_WGS84 = 0,
    DATUM_NAD27 = 1,
    DATUM_NAD83 = 2,
    DATUM_HUGHES = 3,
    DATUM_ITRF97 = 4,
    DATUM_ED50 = 5,
    DATUM_SAD69 = 6
};

enum Spheroids
{
  SPHEROID_UNKNOWN = 0,
  SPHEROID_WGS84 = 1,
  SPHEROID_HUGHES = 2,
  SPHEROID_GRS1967 = 3,
  SPHEROID_GRS1980 = 4,
  SPHEROID_INTERNATIONAL1924 = 5
};

enum PolarimetricDecompositions
{
    POLARIMETRY_NONE = 0,
    POLARIMETRY_PAULI = 1,
    POLARIMETRY_SINCLAIR = 2,
    POLARIMETRY_CLOUDE8 = 3,
    POLARIMETRY_CLOUDE16 = 4,
    POLARIMETRY_CLOUDE_NOCLASSIFY = 5,
    POLARIMETRY_FREEMAN_DURDEN = 6
};

enum ImageDataType
{
  SELECT_POLARIMETRIC_SEGMENTATION = 0,
  SELECT_POLARIMETRIC_DECOMPOSITION = 1,
  SELECT_POLARIMETRIC_PARAMETER = 2,
  SELECT_POLARIMETRIC_MATRIX = 3
};

/*enum
{
  RESAMPLE_NEAREST_NEIGHBOR = 0,
  RESAMPLE_BILINEAR = 1,
  RESAMPLE_BICUBIC = 2
};*/


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
  int process_to_level1;
  int airsar_l_vv;
  int airsar_c_vv;
  int airsar_l_pol;
  int airsar_c_pol;
  int airsar_p_pol;
  int apply_ers2_gain_fix;
  char polsarpro_colormap[512];

  /* transformations */
  int data_type;
  int output_db;
  int latitude_checked;
  double latitude_low;
  double latitude_hi;

  /* external */
  int external_is_checked;
  int external_selected;
  char cmd[512];

  /* polarimetry */
  int polarimetric_decomp_setting;
  int do_farcorr;
  int farcorr_global_avg;
  double farcorr_threshold;

  /* export */
  int export_is_checked;
  int output_format;
  int apply_scaling;
  int longest_dimension;
  int output_bytes;
  int scaling_method;
  int truecolor_is_checked;
  int falsecolor_is_checked;
  int user_defined_is_checked;
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
  int spheroid;
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
  int save_incid_angles;
  int interp_dem_holes;
  int no_matching;
  double offset_x, offset_y;

  // calibration
  int do_calibrate;

  /* misc */
  int keep_files; // 0= keep none, 1=keep temp, 2= keep all
  int apply_metadata_fix;
  int dem_was_generated;
  int mask_was_generated;
}
Settings;

typedef struct
{
    gchar * prefix;
    gchar * suffix;
    gchar * scheme;
} NamingScheme;

extern int COL_INPUT_FILE;
extern int COL_INPUT_FILE_SHORT;
extern int COL_ANCILLARY_FILE;
extern int COL_METADATA_FILE;
extern int COL_ALL_AUX_FILES;
extern int COL_ALL_AUX_FILES_SHORT;
extern int COL_INPUT_THUMBNAIL;
extern int COL_BAND_LIST;
extern int COL_OUTPUT_FILE;
extern int COL_OUTPUT_FILE_SHORT;
extern int COL_STATUS;
extern int COL_LOG;
extern int COL_POLSARPRO_INFO;
extern int COL_POLSARPRO_DISPLAY;
extern int COL_INTERFEROGRAM;
extern int COL_COHERENCE;
extern int COL_SLAVE_METADATA;
extern int COL_BASELINE;

extern int COMP_COL_INPUT_FILE;
extern int COMP_COL_INPUT_FILE_SHORT;
extern int COMP_COL_ANCILLARY_FILE;
extern int COMP_COL_ORIGINAL_METADATA_FILE;
extern int COMP_COL_OUTPUT_FILE;
extern int COMP_COL_OUTPUT_FILE_SHORT;
extern int COMP_COL_OUTPUT_THUMBNAIL;
extern int COMP_COL_OUTPUT_THUMBNAIL_BIG;
extern int COMP_COL_STATUS;
extern int COMP_COL_LOG;
extern int COMP_COL_TMP_DIR;
extern int COMP_COL_LAYOVER_SHADOW_MASK_FILE;
extern int COMP_COL_CLIPPED_DEM_FILE;
extern int COMP_COL_SIMULATED_SAR_FILE;
extern int COMP_COL_FARADAY_FILE;
extern int COMP_COL_HIST_FILE;
extern int COMP_COL_CLASS_MAP_FILE;
extern int COMP_COL_METADATA_FILE;
extern int COMP_COL_INCID_ANGLES_FILE;

/********************************** Prototypes ******************************/

/* asf_convert_gui.c */
void select_polsarpro_classification_lut(const char *);

/* ceos_thumbnail.c */
GdkPixbuf *
make_input_image_thumbnail_pixbuf (const char *input_metadata,
                                   char *input_data,
                                   const char *lut_basename,
                                   size_t max_thumbnail_dimension);

/* settings.c */
Settings *settings_get_from_gui();
void settings_apply_to_gui();
Settings *settings_copy(const Settings *);
int settings_equal(const Settings *, const Settings *);
const gchar *settings_get_size_argument(const Settings *);
const gchar *settings_get_latitude_argument(const Settings *);
const gchar *settings_get_apply_metadata_fix_argument(const Settings *);
const gchar *settings_get_output_bytes_argument(const Settings *s);
const gchar *settings_get_data_type_string(const Settings *);
const gchar *settings_get_data_type_arg_string(const Settings *);
const gchar *settings_get_input_data_format_string(const Settings *);
const gchar *settings_get_output_format_extension(const Settings *);
const gchar *settings_get_output_format_string(const Settings *);
const gchar *settings_get_geocode_options(const Settings *);
const gchar *settings_get_terrcorr_options(const Settings *);
const gchar *settings_get_projection_abbrev(const Settings *);
int settings_get_run_import(const Settings *);
int settings_get_run_export(const Settings *);
int settings_get_run_geocode(const Settings *);
int settings_get_run_terrcorr(const Settings *);
int  settings_get_output_format_can_be_thumbnailed(const Settings *s);
void settings_delete(Settings *);
void settings_update_dem(Settings *s, const char *output_path);
void settings_delete_dem_and_mask(Settings *s);
void settings_update_mask(Settings *s, const char *output_path);
char *settings_to_config_file(const Settings *s,
			      const gchar *input_file, 
			      const gchar *ancillary_file,
			      const gchar *meta_file,
			      const gchar *output_full, 
			      const gchar *output_path,
			      const gchar *tmp_dir,
			      const gchar *polsarpro_aux_info,
			      const gchar *interferogram,
			      const gchar *coherence,
			      const gchar *slave_metadata,
			      const gchar *baseline);
int apply_settings_from_config_file(char *configFile);
void default_to_keep_temp(void);

/* find_in_path.c */
gchar *find_in_path(gchar * file);
gchar *find_dir_in_path(gchar * file);

/* execute.c */
int do_system_exec(const char *cmd);
void process_items_from_list(GList *, gboolean);
void set_stop();

/* callbacks.c */
void output_format_combobox_changed();
void input_data_formats_changed();
void show_execute_button(gboolean);
void latitude_checkbutton_toggle();
void input_data_type_changed();
void rgb_settings_changed();
void import_settings_changed();
void hide_sections_for_execute();
void polarimetry_settings_changed();
void external_settings_changed();
void input_data_type_combobox_changed();
void clear_completed_tmp_dirs();
void set_show_polsarpro_optionmenu(gboolean flag);
void polsarpro_classification_checkbutton_toggled();
void polsarpro_image_data_type_changed();

/* utility.c */
void setup_band_comboboxes();
void set_combo_box_item(GtkWidget *, gint);
gint get_combo_box_item(GtkWidget *);
void message_box(const gchar *);
gchar *meta_file_name(const gchar *);
gchar *data_file_name(const gchar *);
char *getPath(const char *);
GtkWidget *get_widget_checked(const char *widget_name);
void set_combo_box_item_checked(const char *, gint);
void rgb_combo_box_setup();
void rb_select(const char *, gboolean);
char *get_string_from_entry(const char *widget_name);
void put_string_to_entry(const char *widget_name, const char *txt);
double get_double_from_entry(const char *widget_name);
void put_double_to_entry(const char *widget_name, double val);
int get_int_from_entry(const char *widget_name);
void put_int_to_entry(const char *widget_name, int val);
int get_checked(const char *widget_name);
void set_checked(const char *widget_name, int checked);
void enable_widget(const char *widget_name, int enable);
void show_widget(const char *widget_name, int show);
void put_string_to_label(const char *widget_name, const char *txt);
const char *get_string_from_label(const char *widget_name);
gboolean is_polsarpro(const gchar *);
gboolean is_geotiff(const char *infile);
gboolean is_asf_internal(const char *infile);
gboolean is_airsar(const char *infile);
gboolean is_terrasarx(const char *infile);
gboolean is_radarsat2(const char *infile);
gboolean is_roipac(const char *infile);
gboolean is_alos_mosaic(const char *infile);
char *extract_lut_name(const char *polsarpro_aux_info);
int extract_image_data_type(const char *polsarpro_aux_info);
char *encode_polsarpro_aux_info(int image_data_type_flag, char *lut_basename);

/* dnd.c */
void setup_dnd();

/* popup_menu.c */
gboolean get_iter_to_first_selected_row(GtkWidget *, GtkListStore *,
                                        GtkTreeIter *);
void setup_popup_menu();
void show_please_select_message();
void set_toolbar_images();

/* file_list.c */
void setup_files_list();
void populate_files_list(int, char **);
void refresh_file_names();
gboolean add_to_files_list(const gchar *);
gboolean add_to_files_list_iter(const gchar *, const gchar *, const gchar *, const gchar *, 
				const gchar *, const gchar *, const gchar *, const gchar *, GtkTreeIter *);
gboolean add_to_ancillary_files_list(const gchar *);
void update_all_extensions();
void set_output_name(GtkTreeIter *, const gchar *);
gboolean is_meta_file(const gchar *);
void add_thumbnail(const gchar *);
void show_queued_thumbnails();
int has_prepension(const gchar *);
void move_to_completed_files_list(GtkTreeIter *, GtkTreeIter *, const gchar *,
                                  const char *);
void move_from_completed_files_list(GtkTreeIter *);
gboolean have_ancillary_files_in_list();
gboolean have_meta_files_in_list();
gchar * get_ancillary_file_from_input_list(const gchar *);
gchar * get_meta_file_from_input_list(const gchar *);

/* help.c */
char * escapify(const char * s);

/* rename_output.c */
gboolean rename_selected_output_filename();
void do_rename_selected(const gchar *new_name);
void do_rename(GtkTreeModel *model, GtkTreeIter *iter, const gchar *new_name);

/* file_selection.c */
void handle_browse_ancillary_file();
void init_browse_format_combobox();
void init_image_data_type_combobox();
void clear_entries();

/* state.c */

/* naming_scheme.c */
NamingScheme *naming_scheme_new(const gchar *, const gchar *, const gchar *);
NamingScheme *naming_scheme_default();
void naming_scheme_delete(NamingScheme *);
gchar *naming_scheme_apply(const NamingScheme *, const gchar *);
gboolean naming_schemes_equal(const NamingScheme *, const NamingScheme *);
NamingScheme *naming_scheme_copy(const NamingScheme *);
gchar * determine_default_output_file_name_schemed(const gchar *,
                                                   const NamingScheme *scheme);

/* metadata.c */
gchar *build_asf_metadata_filename(gchar * name);
void show_asf_meta_data(gchar *);
void show_ceos_meta_data(gchar *);

/* summary.c */
const char *get_summary_text();
void update_summary();

/* geocode.c */
const char *geocode_options_string(const Settings * settings);
void geocode_options_changed();
const char *datum_string(int datum);
const char *spheroid_string(int spheroid);
const char *resample_method_string(int resample_method);

/* win_font.c */
void set_font();

/* projfile.c */
project_parameters_t *
  load_selected_predefined_projection_parameters(int projection,
                                                 datum_type_t *datum, 
						 spheroid_type_t *spheroid);

void set_predefined_projections(int projection);
void release_predefined_projections();

/* view_output.c */
void show_image_with_asf_view(gchar * in_name);
void show_image_with_asf_view_arg(gchar * in_name, gchar *arg);

/* share.c */
char *find_in_bin(const char *);
char *find_in_share(const char *);
void print_share_dir();

/* terrcorr.c */
void terrcorr_options_changed();
const char *terrcorr_options_string(const Settings *settings);
void default_to_terrcorr_on();

/* log.c */
void show_log(gchar * log_txt, gchar * data_file);

/* blow_up.c */
typedef struct {        // for the fake motion signal callback
    gboolean is_valid;
    GtkWidget *widget;
    GdkEventMotion *event;
    gdouble x, y;     /* x and y of fake event.  */
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

/* png_util.c */
int pixbuf2png(GdkPixbuf *pb, const char *output_png);

/* plugins.c */
void load_external_commands();
const char *get_external_command_line();
void external_settings_changed();
const char *get_external_parameters_as_csv();
void populate_external_params_from_csv(char *csv_str);

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

/* TRUE if full path names should be displayed in the input files and completed files lists */
extern gboolean show_full_paths;
extern gboolean show_ancillary_files;
extern gboolean show_meta_files;
extern gboolean animate_ancillary_files_button;

/* The settings when the user clicked "Execute" (or, "Load") */
extern Settings * settings_on_execute;

/* where should we put the generated files */
extern gchar * output_directory;

/* current naming scheme */
extern NamingScheme * current_naming_scheme;

/* are thumbnails supported ? */
extern gboolean use_thumbnails;
#endif
