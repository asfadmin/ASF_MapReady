#ifndef __SV_H
#define __SV_H

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
float get_pixel(int line, int sample);

/* read.c */
void read_file(const char *filename, const char *band);

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
void fill_small(void);

/* meta.c */
char * escapify(const char * s);
void fill_meta_info(void);
void open_mdv(void);

/* stats.c */
void calc_image_stats(void);

/* google.c */
char *find_in_path(char * file);
int open_google_earth(void);

/* new.c */
void new_file(void);
void browse_new_file(void);

#ifdef win32
#ifdef DIR_SEPARATOR
#undef DIR_SEPARATOR
#endif
extern const char DIR_SEPARATOR;
#endif

extern const char PATH_SEPATATOR;

/*************** these are our global variables ... ***********************/

/* xml version of the .glade file */
extern GladeXML *glade_xml;
extern meta_parameters *meta;
extern float *data;
extern FloatImage *data_fi;
extern int nl, ns;
extern double g_min, g_max;
extern double zoom;
extern double center_line, center_samp;
extern double crosshair_line, crosshair_samp;
extern double ctrl_clk_line, ctrl_clk_samp;
extern char *g_filename;

#endif
