#ifndef __ASF_VIEW_H
#define __ASF_VIEW_H

#include <asf_version.h>
#define VERSION TOOL_SUITE_VERSION_STRING

// Define JASC_PALETTE_SUPPORT to turn on support for
// JASC (Paint Shop Pro and other) palettes
//
// FIXME: We need to make generic the way that we assign/remember
// combo box indices (or peruse the list) so that when a new lut is picked
// and applied that the right item is selected in the drop-down box.
// FOR NOW... Don't enable JASC_PALETTE_SUPPORT until we do this, or just
// enable it for development reasons.
//
#define JASC_PALETTE_SUPPORT

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

#include "asf.h"
#include "asf_meta.h"
#include "asf_endian.h"
#include "envi.h"
#include "float_image.h"
#include "asf_raster.h"
#include "asf_vector.h"
#include "cache.h"
#include "asf_tiff.h"

#define EMBEDDED_TIFF_COLORMAP_LUT        "Embedded_TIFF_Colormap"
#define EMBEDDED_TIFF_COLORMAP_LUT_FILE   "Embedded_TIFF_Colormap.lut"
#define EMBEDDED_ASF_COLORMAP_LUT         "Embedded_Metadata_Colormap"
#define EMBEDDED_ASF_COLORMAP_LUT_FILE    "Embedded_Metadata_Colormap.lut"

typedef struct {
    unsigned char *data;
    int size_x;
    int size_y;
} ThumbnailData;

/* for win32, need __declspec(dllexport) on all signal handlers. */
#if !defined(SIGNAL_CALLBACK)
#  if defined(win32)
#    define SIGNAL_CALLBACK __declspec(dllexport)
#  else
#    define SIGNAL_CALLBACK
#  endif
#endif

typedef struct {
    int is_rgb;
    int band_gs;
    int band_r;
    int band_g;
    int band_b;
} BandConfig;

typedef struct {
    int nl, ns;
    meta_parameters *meta;
    CachedImage *data_ci;
    BandConfig band_cfg;
    ImageStats stats;
    ImageStatsRGB stats_r;
    ImageStatsRGB stats_g;
    ImageStatsRGB stats_b;
    char *filename;
    char *data_name;
    char *meta_name;
} ImageInfo;

typedef struct {
    // metadata
    int num_meta_cols;
    csv_meta_column_t *meta_cols;
    char **meta_info;

    // data
    int num_points;
    double *lines;
    double *samps;

    // display information
    int color_code;
    int marker_code;
} Shape;

/********************************** Prototypes ******************************/

/* font.c */
void set_font(void);

/* utility.c */
void clear_combobox(const char *widget_name);
void add_to_combobox(const char *widget_name, const char *txt);
void set_combo_box_item(const char *widget_name, gint index);
int get_combo_box_item(const char *widget_name);
char *get_band_combo_text(meta_parameters *meta, const char *widget_name);
void rb_select(const char *widget_name, gboolean is_on);
double get_double_from_entry(const char *widget_name);
void put_double_to_entry(const char *widget_name, double val);
void put_double_to_entry_fmt(const char *widget_name, double val,
                             const char *format);
int get_int_from_entry(const char *widget_name);
void put_int_to_entry(const char *widget_name, int val);
int get_checked(const char *widget_name);
void set_checked(const char *widget_name, int checked);
void message_box(const char *format, ...);
GtkWidget *get_widget_checked(const char *widget_name);
void set_combobox_entry_maxlen(const char *widget_name, int maxlen);
char* get_string_from_entry(const char *widget_name);
int entry_has_text(const char *widget_name);
void put_string_to_entry(const char *widget_name, char *txt);
char *get_string_from_comboboxentry(const char *widget_name);
void put_string_to_comboboxentry(const char *widget_name, char *txt);
void put_file_in_textview(const char *file, const char *widget_name);
void put_text_in_textview(const char *txt, const char *widget_name);
void put_string_to_label(const char *widget_name, const char *txt);
void show_widget(const char *widget_name, int show);
void enable_widget(const char *widget_name, int enable);
char *trim_whitespace(const char *s);

/* asf_view.c */
char *find_in_share(const char * filename);
void image_info_free(ImageInfo *ii);

/* read.c */
int read_file(const char *filename, const char *band, int multilook,
              int on_fail_abort);
int try_ext(const char *filename, const char *ext);
int try_prepension(const char *filename, const char *prepension);

/* read_asf.c */
int try_asf(const char *filename, int try_extensions);
int handle_asf_file(const char *filename, char *meta_name, char *data_name,
                    char **err);
meta_parameters *read_asf_meta(const char *meta_name);
int open_asf_data(const char *filename, const char *band, int multilook,
                  meta_parameters *meta, ClientInterface *client);
void free_asf_client_info(void *read_client_info);

/* read_ceos.c */
int try_ceos(const char *filename);
int handle_ceos_file(const char *filename, char *meta_name, char *data_name,
                    char **err);
meta_parameters *read_ceos_meta(const char *meta_name);
int open_ceos_data(const char *dataname, const char *metaname,
                   const char *band, int multilook, meta_parameters *meta,
                   ClientInterface *client);
void free_ceos_client_info(void *read_client_info);

/* read_airsar.c */
int try_airsar(const char *filename);
int handle_airsar_file(const char *filename, char *meta_name, char *data_name,
                       char **err);
meta_parameters *open_airsar(const char *data_name, const char *meta_name,
                             const char *band, ClientInterface *client);

// read_roipac.c
int try_roipac(const char *filename);
int handle_roipac_file(const char *filename, char *meta_name, char *data_name,
                       char **err);
meta_parameters *open_roipac(const char *filename, const char *band,
                             const char *metaname, const char *dataname,
                             int multilook, ClientInterface *client);

// read_terrasar.c
int try_terrasar(const char *filename);
int handle_terrasar_file(const char *filename, char *meta_name, char *data_name,
			 char **err);
meta_parameters *open_terrasar(const char *data_name, const char *meta_name,
			       const char *band, int multilook,
			       ClientInterface *client);

/* read_jpeg.c */
int try_jpeg(const char *filename, int try_extensions);
int handle_jpeg_file(const char *filename, char *meta_name, char *data_name,
                     char **err);
meta_parameters* open_jpeg(const char *data_name, ClientInterface *client);

/* read_tiff.c */
int try_tiff(const char *filename, int try_extensions);
int handle_tiff_file(const char *filename, char *meta_name, char *data_name,
                     char **err);
meta_parameters *read_tiff_meta(const char *meta_name, ClientInterface *client, char *filename);
int open_tiff_data(const char *data_name, const char *band, ClientInterface *client);

/* read_png.c */
int try_png(const char *filename, int try_extensions);
int handle_png_file(const char *filename, char *meta_name, char *data_name,
                     char **err);
meta_parameters* open_png(const char *data_name, ClientInterface *client);

/* read_pgm.c */
int try_pgm(const char *filename, int try_extensions);
int handle_pgm_file(const char *filename, char *meta_name, char *data_name,
                     char **err);
meta_parameters* open_pgm(const char *data_name, ClientInterface *client);

/* read_brs.c */
int try_brs(const char *filename, int try_extensions);
int handle_brs_file(const char *filename, char *meta_name, char *data_name,
                     char **err);
meta_parameters* open_brs(const char *data_name, ClientInterface *client);

// read_envi.c
int try_envi(const char *filename, int try_extensions);
int handle_envi_file(const char *filename, char *meta_name, 
		     char *data_name, char **err);
meta_parameters* open_envi(const char *meta_name, const char *data_name,
                           const char *band_str,
			   ClientInterface *client);

// read_generic.c
int handle_generic_file(const char *filename, char **err);
meta_parameters *read_generic_meta(const char *filename);
int open_generic_data(const char *filename,
                      meta_parameters *meta, ClientInterface *client);

// read_uavsar.c
int try_uavsar(const char *filename, int try_extensions);
int handle_uavsar_file(const char *filename, char *meta_name, char *data_name,
                       char **err);
meta_parameters *read_uavsar_meta(const char *meta_name, const char *data_name);
int open_uavsar_data(const char *filename, int multilook,
                     meta_parameters *meta, ClientInterface *client);

/* big_image.c */
GdkPixbuf * make_big_image(ImageInfo *ii, int show_crosshair);
void fill_big(ImageInfo *ii);
void update_zoom(void);
int get_big_image_width(void);
int get_big_image_width2(void);
int get_big_image_height(void);
int get_big_image_height2(void);
GdkPixbuf *get_saved_pb();
void get_color(int color, unsigned char *r, unsigned char *g,
               unsigned char *b);
void big_clicked(GdkEventButton *event);
void small_clicked(GdkEventButton *event);
void img2ls(int x, int y, double *line, double *samp);
void put_line(GdkPixbuf *pixbuf, double line0, double samp0, 
              double line1, double samp1, int color,
              ImageInfo *ii);

/* small_image.c */
ThumbnailData *get_thumbnail_data(ImageInfo *ii);
void fill_small(ImageInfo *ii);
void fill_small_force_reload(ImageInfo *ii);
void fill_small_have_data(ThumbnailData *thumbnail_data, ImageInfo *ii);
void setup_small_image_size(void);

/* meta.c */
char * escapify(const char * s);
void fill_meta_info(void);
void open_mdv(void);
char *br(const char *s);
void disable_meta_button_if_necessary();

/* stats.c */
unsigned char *generate_thumbnail_data(ImageInfo *ii, int tsx, int tsy);
int fill_stats(ImageInfo *ii);
void calc_stats_thread(gpointer user_data);
int is_ignored(ImageStats *stats, float val);
int is_ignored_rgb(ImageStatsRGB *stats, float val);
int calc_scaled_pixel_value(ImageStats *stats, float val);
int calc_rgb_scaled_pixel_value(ImageStatsRGB *stats, float val);
void clear_stats(ImageInfo *ii);
void update_map_settings(ImageInfo *ii);
void set_mapping_defaults(ImageInfo *ii);
void get_rgb_with_masking(ImageInfo *ii, ImageInfo *mask,
         int l, int s, unsigned char *r, unsigned char *g, unsigned char *b);
int apply_mask(int v, unsigned char *r, unsigned char *g, unsigned char *b);

/* google.c */
char *find_in_path(char * file);
int open_google_earth(void);

/* new.c */
void new_file(void);
void load_file(const char *file);
void load_file_banded(const char *file, const char *band, int multilook);
void reload_file_banded(const char *file, const char *band, int multilook);
void reset_globals(int reset_position);
void set_title(int band_specified, const char *band);

/* subset.c */
void save_subset(ImageInfo *ii);
void update_poly_extents(meta_parameters *meta);

/* bands.c */
void setup_bands_tab(meta_parameters *meta);
void set_bands_rgb(int r, int g, int b);
void set_bands_greyscale(int b);

/* info.c */
int meta_supports_meta_get_latLon(meta_parameters *meta);
void update_pixel_info(ImageInfo *);

/* lut.c */
void populate_lut_combo(void);
void set_lut(const char *lut_basename);
void select_lut(const char *lut_basename);
void check_lut(void);
int have_lut(void);
void apply_lut(int val, unsigned char *r,
               unsigned char *g, unsigned char *b);
int set_lut_based_on_image_type(image_data_type_t image_data_type);
void apply_lut_to_data(ThumbnailData *thumbnail_data);
int check_for_embedded_tiff_lut (char *curr_file, int *lut_specified, char *lut);
int get_cloude16_lut_index(void);
int get_cloude8_lut_index(void);
int get_dem_lut_index(void);
int get_interferogram_lut_index(void);
int get_unwrapping_mask_lut_index(void);
int get_layover_mask_lut_index(void);
int get_polarimetry_lut_index(void);
int get_water_mask_lut_index(void);
int get_tiff_lut_index(void);
int get_asf_lut_index(void);
void set_current_index(int index);
int get_current_index(void);
int is_colormap_ASF_file(char *file);

/* plan.c */
int planner_is_active(void);
void setup_planner(void);
int row_is_checked(int);
void calibrate_planner_reference(void);
void planner_click(int l, int s);

/* csv.c */
const char * detect_csv_assoc();
void open_csv(const char *csv_file);

/* pan.c */
void clear_nb_callback(void);
void setup_gdk_window_ids(void);

/* shape.c */
void free_shapes();

/* plugins.c */
void load_external_commands();
const char *get_external_command_line();
void external_settings_changed();

#ifdef HAVE_DELTA_CR
void add_delta_shapes(meta_parameters *meta);
#endif

#ifdef win32
#ifdef DIR_SEPARATOR
#undef DIR_SEPARATOR
#endif
extern const char DIR_SEPARATOR;
#endif

extern const char PATH_SEPATATOR;

#define MAX_POLY_LEN 1450
typedef struct {
    int n;                    // How many points in the polygon
    int c;                    // Currently "active" point (-1 for none)
    double line[MAX_POLY_LEN];// vertices of the polygon
    double samp[MAX_POLY_LEN];
    int show_extent;          // draw bounding box of polygon?
    int extent_x_min, extent_x_max; // bounding box values
    int extent_y_min, extent_y_max; // when show_extent==TRUE, these must
                                    //   be made valid
} UserPolygon;

/*************** these are our global variables ... ***********************/

extern GladeXML *glade_xml;

// Can hold five images
#define MAX_IMAGES 5
extern ImageInfo image_info[MAX_IMAGES];
// "curr" always points to the currently being displayed image info
extern ImageInfo *curr;
extern ImageInfo *mask;
extern int current_image_info_index;
extern int n_images_loaded;

// these globals all relate to the current viewing settings
#define MAX_POLYS 50
extern UserPolygon g_polys[MAX_POLYS];
extern UserPolygon *g_poly;
extern int which_poly;
extern int g_show_north_arrow;

extern Shape **g_shapes;
extern int num_shapes;

extern double zoom;
extern double center_line, center_samp;
extern double crosshair_line, crosshair_samp;

extern int g_saved_line_count;

// keeps track of whether or not the arrow keys should affect the
// crosshair or the ctrl-crosshair
extern int last_was_crosshair;

extern int is_asf_internal;

extern int ignore_grey_value;
extern int ignore_red_value;
extern int ignore_green_value;
extern int ignore_blue_value;

// if using generic binary
extern int generic_specified;
extern int generic_bin_width, generic_bin_height, generic_bin_datatype,
           generic_bin_byteswap;

#endif
