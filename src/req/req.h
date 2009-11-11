#ifndef __REQ_H
#define __REQ_H

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

#define UNSELECTED_REQUEST_TYPE 0
#define OBSERVATION_REQUEST 1
#define ACQUISITION_REQUEST 2
#define ON_DEMAND_LEVEL_0 3

/********************************** Settings ********************************/
typedef struct
{
    char *csv_dir;
    char *output_dir;
    int obs_req_num;
    int obs_req_id_aadn;
    int obs_req_id_tdrs;
    int acq_req_num;
    char acq_req_stn_codes[5][5];
    int acq_req_ids[5];
    int odl0_seq_num;
    int odl0_req_id;
    char *station_code;
}
Settings;

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
long get_long_from_entry(const char *widget_name);
void put_long_to_entry(const char *widget_name, long val);
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
void put_string_in_textview(const char *widget_name, const char *txt);
void put_string_to_label(const char *widget_name, const char *format, ...);
void show_widget(const char *widget_name, int show);
void get_combo_box_entry_item(const char *widget_name, char *dest);

/* req.c */
char *find_in_share(const char * filename);

/* settings.c */
void apply_saved_settings(void);
char *settings_get_output_dir(void);
char *settings_get_csv_dir(void);
Settings *settings_load(void);
void settings_free(Settings *s);
int settings_get_next_req_id(int request_type, const char *drf);
void settings_set_next_req_id_and_incr_req_num(int req_id, int request_type,
                                               const char *drf);
int settings_get_is_emergency(void);
long settings_get_start_date(void);
long settings_get_end_date(void);
void settings_set_start_date(long l);
void settings_set_end_date(long l);
void settings_set_request_type(int request_type);
int settings_get_request_type(void);
const char *settings_get_station_code(void);
int settings_get_sequence_number(void);
int settings_get_is_aadn(void);
const char *settings_get_aadn_or_tdrs(void);

/* csv_list.c */
void populate_csvs(void);
void hook_up_csv_dir_entry_changed(void);
void alert(const char *s);

/* process.c */
char *process(const char *csv_file, int is_emergency,
              int *req_id, int *request_type, char **drf,
              long start_date_user, long end_date_user);
void gui_process(int for_real);

/* output.c */
void update_output_file(void);
char *get_output_file(void);

/* Date utils */
#include "date.h"

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

/* blocks (re-)processing in events that are triggered during processing */
int block_processing;

#endif
