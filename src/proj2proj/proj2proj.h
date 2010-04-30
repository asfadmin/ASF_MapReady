#ifndef __P2P_H
#define __P2P_H

#define VERSION "1.0"

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

enum ProjectionOptions
{
  PROJ_UTM = 0,
  PROJ_PS = 1,
  PROJ_ALBERS = 2,
  PROJ_LAMAZ = 3,
  PROJ_LAMCC = 4,
  PROJ_LATLON = 5
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

/********************************** Prototypes ******************************/

/* font.c */
void set_font(void);

/* utility.c */
void clear_combobox(const char *widget_name);
void add_to_combobox(const char *widget_name, const char *txt);
void set_combo_box_item_checked(const char *widget_name, gint index);
void set_combo_box_item(GtkWidget *ddl, gint index);
void rb_select(const char *widget_name, gboolean is_on);
double get_double_from_entry(const char *widget_name);
void put_double_to_entry(const char *widget_name, double val);
int get_int_from_entry(const char *widget_name);
void put_int_to_entry(const char *widget_name, int val);
int get_checked(const char *widget_name);
void set_checked(const char *widget_name, int checked);
void message_box(const char *format, ...);
GtkWidget *get_widget_checked(const char *widget_name);
GtkWidget *get_widget_checked2(int source, const char *widget_name);
void set_combobox_entry_maxlen(const char *widget_name, int maxlen);
char* get_string_from_entry(const char *widget_name);
void put_string_to_entry(const char *widget_name, char *txt);
char *get_string_from_comboboxentry(const char *widget_name);
void put_string_to_comboboxentry(const char *widget_name, char *txt);
void put_file_in_textview(const char *file, const char *widget_name);
void put_text_in_textview(const char *txt, const char *widget_name);
void put_string_to_label(const char *widget_name, const char *txt);
void show_widget(const char *widget_name, int show);
void enable_widget(const char *widget_name, int enable);

/* proj2proj.c */
char *find_in_share(const char * filename);

/* projfile.c */
project_parameters_t *
load_selected_predefined_projection_parameters(int is_source, int projection,
                                               datum_type_t *datum,
					       spheroid_type_t *spheroid);
void set_predefined_projections(int is_source, int projection);

/* geocode.c */
void geocode_options_changed(int is_source);

/* execute.c */
void forward();
void backward();

#ifdef win32
#ifdef DIR_SEPARATOR
#undef DIR_SEPARATOR
#endif
extern const char DIR_SEPARATOR;
#endif

extern const char PATH_SEPATATOR;

/*************** these are our global variables ... ***********************/

extern GladeXML *glade_xml;

#endif
