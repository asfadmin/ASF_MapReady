#ifndef __C2V_H
#define __C2V_H

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

/* for win32, need __declspec(dllexport) on all signal handlers. */
#if !defined(SIGNAL_CALLBACK)
#  if defined(win32)
#    define SIGNAL_CALLBACK __declspec(dllexport)
#  else
#    define SIGNAL_CALLBACK
#  endif
#endif

// These two *MUST* match the order of items in the dropdown, as set in
// the c2v.glade file.
enum
{
  INPUT_AUTO = 0,
  INPUT_META = 1,
  INPUT_LEADER = 2,
  INPUT_POINT = 3,
  INPUT_POLYGON = 4,
  INPUT_SHAPE = 5,
  INPUT_KML = 6,
  INPUT_GEOTIFF = 7,
  INPUT_ALOS_CSV = 8,
  INPUT_URSA = 9,
  INPUT_GENERIC_CSV = 10,
  INPUT_TERRASAR = 11,
  INPUT_STF = 12
};

enum
{
  OUTPUT_KML = 0,
  OUTPUT_SHAPE = 1,
  OUTPUT_TEXT = 2,
  OUTPUT_ALOS_CSV = 3
};

/********************************** Prototypes ******************************/

/* font.c */
void set_font(void);

/* utility.c */
void clear_combobox(const char *widget_name);
void add_to_combobox(const char *widget_name, const char *txt);
void set_combo_box_item(const char *widget_name, gint index);
int get_combo_box_item(const char *widget_name);
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

/* browse.c */
void add_input_file(char *file);
void select_defaults_by_file_type(char *f, int set_output_also);
void clear_results_message();

/* process.c */
void process(void);

/* c2v.c */
char *find_in_share(const char * filename);

/* alos2kml.c */
int alos_csv_to_kml(const char *in_file, const char *out_file);
int kml_to_alos_csv(const char *in_file, const char *out_file);

/* google.c */
void open_in_google_earth(const char *kml_file);
void open_in_excel(const char *csv_file);

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

#endif
