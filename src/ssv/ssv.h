#ifndef __SV_H
#define __SV_H

#define VERSION "1.1"

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
#include "float_image.h"
#include "asf_raster.h"
#include "cache.h"

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

/********************************** Prototypes ******************************/

/* font.c */
void set_font(void);

/* utility.c */
void add_to_combobox(const char *widget_name, const char *txt);
void set_combo_box_item_checked(const char *widget_name, gint index);
void rb_select(const char *widget_name, gboolean is_on);
double get_double_from_entry(const char *widget_name);
void put_double_to_entry(const char *widget_name, double val);
int get_int_from_entry(const char *widget_name);
void put_int_to_entry(const char *widget_name, int val);
int get_checked(const char *widget_name);
void set_checked(const char *widget_name, int checked);
void message_box(const char *format, ...);
GtkWidget *get_widget_checked(const char *widget_name);
void set_combobox_entry_maxlen(const char *widget_name, int maxlen);
char* get_string_from_entry(const char *widget_name);
void put_string_to_entry(const char *widget_name, char *txt);
char *get_string_from_comboboxentry(const char *widget_name);
void put_string_to_comboboxentry(const char *widget_name, char *txt);
void put_file_in_textview(const char *file, const char *widget_name);
void put_string_to_label(const char *widget_name, const char *txt);

/* ssv.c */
char *find_in_share(const char * filename);

/* read.c */
int read_file(const char *filename, const char *band, int on_fail_abort);
int try_ext(const char *filename, const char *ext);
int try_prepension(const char *filename, const char *prepension);

/* read_asf.c */
int try_asf(const char *filename);
int handle_asf_file(const char *filename, char *meta_name, char *data_name,
                    char **err);
meta_parameters *read_asf_meta(const char *meta_name);
int open_asf_data(const char *filename, const char *band,
                  meta_parameters *meta, ClientInterface *client);
void free_asf_client_info(void *read_client_info);

/* read_ceos.c */
int try_ceos(const char *filename);
int handle_ceos_file(const char *filename, char *meta_name, char *data_name,
                    char **err);
meta_parameters *read_ceos_meta(const char *meta_name);
int open_ceos_data(const char *dataname, const char *metaname, const char *band,
                   meta_parameters *meta, ClientInterface *client);
void free_ceos_client_info(void *read_client_info);

/* read_alos.c */
int try_alos(const char *filename);
int handle_alos_file(const char *filename, const char *band, char *meta_name,
                     char *data_name, char **err);

/* read_jpeg.c */
int try_jpeg(const char *filename);
int handle_jpeg_file(const char *filename, char *meta_name, char *data_name,
                     char **err);
meta_parameters *read_jpeg_meta(const char *meta_name);
int open_jpeg_data(const char *data_name, const char *meta_name,
                   const char *band, meta_parameters *meta,
                   ClientInterface *client);

/* big_image.c */
void fill_big(void);
void update_pixel_info(void);
void update_zoom(void);
int get_big_image_width(void);
int get_big_image_width2(void);
int get_big_image_height(void);
int get_big_image_height2(void);
int calc_scaled_pixel_value(float val);

/* small_image.c */
ThumbnailData *get_thumbnail_data(void);
void fill_small(void);
void fill_small_force_reload(void);
void fill_small_have_data(ThumbnailData *thumbnail_data);

/* meta.c */
char * escapify(const char * s);
void fill_meta_info(void);
void open_mdv(void);

/* stats.c */
unsigned char *generate_thumbnail_data(int tsx, int tsy);
int fill_stats(void);
void calc_stats_thread(gpointer user_data);
int calc_scaled_pixel_value(float val);
void clear_stats(void);

/* google.c */
char *find_in_path(char * file);
int open_google_earth(void);

/* new.c */
void new_file(void);
void load_file(const char *file);
void reset_globals(void);
void set_title(int band_specified, char *band);

#ifdef win32
#ifdef DIR_SEPARATOR
#undef DIR_SEPARATOR
#endif
extern const char DIR_SEPARATOR;
#endif

extern const char PATH_SEPATATOR;

// This is defined/managed in stats.c, it is a singleton
typedef struct {
    double map_min, map_max; // max/min for 2-sigma mapping
    double avg, stddev;
    double act_min, act_max; // absolute min/max of all values
    int hist[256];           // histogram
} ImageStats;

// This is defined/managed in big_image.c, it is a singleton
typedef struct {
    int n;                  // How many points in the polygon
    int c;                  // Currently "active" point (-1 for none)
    double line[256];       // vertices of the polygon
    double samp[256];
} UserPolygon;

/*************** these are our global variables ... ***********************/

extern GladeXML *glade_xml;
extern meta_parameters *meta;
extern CachedImage *data_ci;

extern int nl, ns;
extern ImageStats g_stats;
extern UserPolygon g_poly;

extern double zoom;
extern double center_line, center_samp;
extern double crosshair_line, crosshair_samp;

extern char *g_filename;
extern char *g_data_name;
extern char *g_meta_name;

#endif
