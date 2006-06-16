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

#define STP_VERSION "1.0.7"

/* for win32, set the font to the standard windows one */
#if defined(win32)
#include <pango/pango.h>

#define BYTE __byte
#include "asf.h"
#undef BYTE
#include <Windows.h>
#undef DIR_SEPARATOR

static char appfontname[128] = "tahoma 8"; /* fallback value */

static void set_app_font (const char *fontname)
{
    GtkSettings *settings;

    if (fontname != NULL && *fontname == 0) return;

    settings = gtk_settings_get_default();

    if (fontname == NULL) {
	g_object_set(G_OBJECT(settings), "gtk-font-name", appfontname, NULL);
    } else {
	GtkWidget *w;
	PangoFontDescription *pfd;
	PangoContext *pc;
	PangoFont *pfont;

	w = gtk_label_new(NULL);
	pfd = pango_font_description_from_string(fontname);
	pc = gtk_widget_get_pango_context(w);
	pfont = pango_context_load_font(pc, pfd);

	if (pfont != NULL) {
	    strcpy(appfontname, fontname);
	    g_object_set(G_OBJECT(settings), "gtk-font-name", appfontname,
			 NULL);
	}

	gtk_widget_destroy(w);
	pango_font_description_free(pfd);
    }
}

char *default_windows_menu_fontspec (void)
{
    gchar *fontspec = NULL;
    NONCLIENTMETRICS ncm;

    memset(&ncm, 0, sizeof ncm);
    ncm.cbSize = sizeof ncm;

    if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, ncm.cbSize, &ncm, 0)) {
	HDC screen = GetDC(0);
	double y_scale = 72.0 / GetDeviceCaps(screen, LOGPIXELSY);
	int point_size = (int) (ncm.lfMenuFont.lfHeight * y_scale);

	if (point_size < 0) point_size = -point_size;
	fontspec = g_strdup_printf("%s %d", ncm.lfMenuFont.lfFaceName,
				   point_size);
	ReleaseDC(0, screen);
    }

    return fontspec;
}

static void try_to_get_windows_font (void)
{
    gchar *fontspec = default_windows_menu_fontspec();

    if (fontspec != NULL) {
	int match = 0;
	PangoFontDescription *pfd;
	PangoFont *pfont;
	PangoContext *pc;
	GtkWidget *w;

	pfd = pango_font_description_from_string(fontspec);

	w = gtk_label_new(NULL);
	pc = gtk_widget_get_pango_context(w);
	pfont = pango_context_load_font(pc, pfd);
	match = (pfont != NULL);

	pango_font_description_free(pfd);
	g_object_unref(G_OBJECT(pc));
	gtk_widget_destroy(w);

	if (match) set_app_font(fontspec);
	g_free(fontspec);
    }
}

void set_font ()
{
    try_to_get_windows_font();
}

#else /* defined(win32) */

#include "asf.h"
#if defined(DIR_SEPARATOR)
#undef DIR_SEPARATOR
#endif

/* on unix, GTK will select the appropriate fonts */
void set_font () {}

#endif /* defined(win32) */

#include "ardop_defs.h"

#ifdef win32
const char PATH_SEPARATOR = ';';
const char DIR_SEPARATOR = '\\';
#else
const char PATH_SEPARATOR = ':';
const char DIR_SEPARATOR = '/';
#endif

GladeXML *glade_xml;
gboolean user_modified_output_file = FALSE;

//static char *
//find_in_bin(const char * filename)
//{
//    char * ret = (char *) malloc(sizeof(char) *
//                      (strlen(get_asf_bin_dir()) + strlen(filename) + 5));
//    sprintf(ret, "%s/%s", get_asf_bin_dir(), filename);
//    return ret;
//}

static char *
find_in_share(const char * filename)
{
    char * ret = (char *) malloc(sizeof(char) *
                      (strlen(get_asf_share_dir()) + strlen(filename) + 5));
    sprintf(ret, "%s/%s", get_asf_share_dir(), filename);
    return ret;
}


/*
gchar *
find_in_path(gchar * file)
{
  gchar *path, *buf, *name, *p;
  int len, pathlen;

  // first see if file is in current directory
  if (g_file_test(file, G_FILE_TEST_EXISTS))
  {
    return g_strdup(file);
  }

  path = (gchar *)g_getenv("PATH");

  len = strlen(file) + 1;
  pathlen = strlen(path);

  // work area
  buf = (gchar *) g_malloc( sizeof(gchar) * (pathlen + len + 2) ); 

  // put separator + filename at the end of the buffer
  name = buf + pathlen + 1;
  *name = DIR_SEPARATOR;
  memcpy(name + 1, file, len);

  // now try each path item, prepended to the filename in the work area
  p = path;
  do
  {
    gchar * start;
    gchar * q = strchr(p + 1, PATH_SEPARATOR);

    // if separator not found, point to the end
    if ( !q ) 
      q = path + pathlen;

    start = name - (q - p);

    // copy path portion to the work area
    memcpy( start, p, q - p );

    if (g_file_test( start, G_FILE_TEST_EXISTS ))
    {
      gchar * ret = g_strdup(start);
      g_free(buf);
      return ret; 
    }

    p = q;
  } 
  while (*p++ != '\0');

  // not found!
  g_free(buf);
  return NULL;
}
*/

/* danger: returns pointer to static data!! */
static const char * imgloc(char * file)
{
    static char loc[1024];
    gchar * tmp = find_in_share(file);
    if (tmp) {
      strcpy(loc, tmp);
      g_free(tmp);
    } else {
      strcpy(loc, file);
    }

    return loc;
}

static void set_help_image(int step)
{
    char widget_name[256];
    sprintf(widget_name, "step%d_help_image", step);

    GtkWidget * w =
      glade_xml_get_widget(glade_xml, widget_name);

    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("info_on_sml.gif"));
}

static void set_images()
{
    GtkWidget * range_compression_image;
    GtkWidget * azimuth_compression_image;

    range_compression_image =
	glade_xml_get_widget(glade_xml, "range_compression_image");

    azimuth_compression_image =
	glade_xml_get_widget(glade_xml, "azimuth_compression_image");

    gtk_image_set_from_file(GTK_IMAGE(range_compression_image),
			    imgloc("rc.gif"));
    gtk_image_set_from_file(GTK_IMAGE(azimuth_compression_image),
			    imgloc("ac.gif"));

    GtkWidget * flowchart_image;

    flowchart_image =
	glade_xml_get_widget(glade_xml, "flowchart_image");
    
    int i;
    for (i = 1; i <= 12; ++i)
      set_help_image(i);
}

static void
add_file (const gchar * filename)
{
    GtkWidget *input_file_entry;

    input_file_entry =
	glade_xml_get_widget(glade_xml, "input_file_entry");

    gtk_entry_set_text(GTK_ENTRY(input_file_entry), filename);
}

void
message_box(const gchar * message)
{
  GtkWidget *dialog, *label;

  dialog = gtk_dialog_new_with_buttons( "Message",
	NULL,
	GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
	GTK_STOCK_OK,
	GTK_RESPONSE_NONE,
	NULL);

  label = gtk_label_new(message);

  g_signal_connect_swapped(dialog, 
			   "response", 
			   G_CALLBACK(gtk_widget_destroy),
			   dialog);

  gtk_container_add(GTK_CONTAINER(GTK_DIALOG(dialog)->vbox), label);

  gtk_widget_show_all(dialog);
}

SIGNAL_CALLBACK void
on_input_file_browse_button_clicked(GtkWidget *button)
{
#if defined(win32)
  OPENFILENAME of;
  int retval;
  char fname[1024];

  fname[0] = '\0';

  memset(&of, 0, sizeof(of));

#ifdef OPENFILENAME_SIZE_VERSION_400
  of.lStructSize = OPENFILENAME_SIZE_VERSION_400;
#else
  of.lStructSize = sizeof(of);
#endif

  of.hwndOwner = NULL;
  of.lpstrFilter = "ASF Internal Format (*.raw)\0*.raw\0"
                   "All Files\0*\0";
  of.lpstrCustomFilter = NULL;
  of.nFilterIndex = 1;
  of.lpstrFile = fname;
  of.nMaxFile = sizeof(fname);
  of.lpstrFileTitle = NULL;
  of.lpstrInitialDir = ".";
  of.lpstrTitle = "Select File";
  of.lpstrDefExt = NULL;
  of.Flags = OFN_HIDEREADONLY | OFN_EXPLORER;
  
  retval = GetOpenFileName(&of);
  
  if (!retval) {
    if (CommDlgExtendedError())
      printf("File dialog box error");
    return;
  }

  add_file(fname);

  /* leaving this code for multiple filenames here ... 
    we may wish to use it again at some point 

  // the returned "fname" has the following form:            
  //   <directory>\0<first file>\0<second file>\0<third ...  
  char * dir = strdup(fname);
  char * p = fname + strlen(dir) + 1;

  if (*p) { 
    while (*p) {
      char * dir_and_file = malloc(sizeof(char)*(strlen(dir)+strlen(p)+5));
      sprintf(dir_and_file, "%s/%s", dir, p);
      printf("Adding: %s\n", dir_and_file);
      add_file(dir_and_file);
      p += strlen(p) + 1;
      free(dir_and_file);
    }
  } else {
    add_file(dir);
  }

  free(dir);
  */

#else

    GtkWidget * file_selection_dialog =
	glade_xml_get_widget (glade_xml, "file_selection_dialog");

    gtk_widget_show (file_selection_dialog);

#endif
}

static void
hide_file_selection_dialog ()
{
    GtkWidget *file_selection_dialog =
	glade_xml_get_widget(glade_xml, "file_selection_dialog");
 
    gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_file_selection_dialog_cancel_button_clicked(GtkWidget *w)
{
    hide_file_selection_dialog ();
}

SIGNAL_CALLBACK gboolean
on_file_selection_dialog_delete_event(GtkWidget *w)
{
    hide_file_selection_dialog ();
    return TRUE;
}
 
SIGNAL_CALLBACK gboolean
on_file_selection_dialog_destroy_event(GtkWidget *w)
{
    hide_file_selection_dialog ();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_file_selection_dialog_destroy(GtkWidget *w)
{
    hide_file_selection_dialog ();
    return TRUE;
}

SIGNAL_CALLBACK void
on_file_selection_dialog_ok_button_clicked(GtkWidget *w)
{
    GtkWidget *file_selection_dialog;

    gchar **selections;
    gchar **current;

    file_selection_dialog =
	glade_xml_get_widget(glade_xml, "file_selection_dialog");

    selections = gtk_file_selection_get_selections(
	GTK_FILE_SELECTION(file_selection_dialog));

    current = selections;
    
    while (*current)
    {	
	add_file(*current);
	++current;
    }

    g_strfreev(selections);
    gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_ardop_main_destroy(GtkWidget *w, gpointer data)
{
    gtk_main_quit();
}

void
set_step_enabled(int step, gboolean enabled)
{
    char label_id[32];
    char table_id[32];

    sprintf(label_id, "step%d_label", step);
    sprintf(table_id, "step%d_table", step);

    GtkWidget * label =
	glade_xml_get_widget(glade_xml, label_id);

    GtkWidget * table =
	glade_xml_get_widget(glade_xml, table_id);

    gtk_widget_set_sensitive(label, enabled);
    gtk_widget_set_sensitive(table, enabled);
}

void
range_complex_multiply_checkbutton_toggled()
{
    GtkWidget * range_complex_multiply_checkbutton =
	glade_xml_get_widget(glade_xml, "range_complex_multiply_checkbutton");

    gboolean is_checked =
	gtk_toggle_button_get_active(
	    GTK_TOGGLE_BUTTON(range_complex_multiply_checkbutton));

    set_step_enabled(3, is_checked);
    set_step_enabled(4, is_checked);
    set_step_enabled(5, is_checked);
}

void
range_migration_checkbutton_toggled()
{
    GtkWidget * range_migration_checkbutton =
	glade_xml_get_widget(glade_xml, "range_migration_checkbutton");

    gboolean is_checked =
	gtk_toggle_button_get_active(
	    GTK_TOGGLE_BUTTON(range_migration_checkbutton));

//  set_step_enabled(7, is_checked);
    set_step_enabled(8, is_checked);
}

void
azimuth_complex_multiply_checkbutton_toggled()
{
    GtkWidget * azimuth_complex_multiply_checkbutton =
	glade_xml_get_widget(glade_xml,
			     "azimuth_complex_multiply_checkbutton");

    gboolean is_checked =
	gtk_toggle_button_get_active(
	    GTK_TOGGLE_BUTTON(azimuth_complex_multiply_checkbutton));

    set_step_enabled(9, is_checked);
    set_step_enabled(10, is_checked);
    set_step_enabled(11, is_checked);
}

SIGNAL_CALLBACK void
on_range_complex_multiply_checkbutton_toggled(GtkWidget *widget)
{
    range_complex_multiply_checkbutton_toggled();
}

SIGNAL_CALLBACK void
on_range_migration_checkbutton_toggled(GtkWidget *widget)
{
    range_migration_checkbutton_toggled();
}

SIGNAL_CALLBACK void
on_azimuth_complex_multiply_checkbutton_toggled(GtkWidget *widget)
{
    azimuth_complex_multiply_checkbutton_toggled();
}

void
set_button_text(int step, const char *file, const char *postfix)
{
    gchar tmp[1024];
    sprintf(tmp, "%s%s", file, postfix);

    char button_id[32];

    sprintf(button_id, "step%d_togglebutton", step);

    GtkWidget * button =
	glade_xml_get_widget(glade_xml, button_id);

    gtk_button_set_label(GTK_BUTTON(button), tmp);
}

static void
set_widget_sensitive(const char * w, gboolean setting)
{
    GtkWidget *ardop_main_scrolledwindow = glade_xml_get_widget(glade_xml, w);
    gtk_widget_set_sensitive(ardop_main_scrolledwindow, setting);
}

const gchar *
update_output_filename(const gchar * input_file_and_path)
{
    GtkWidget * output_file_entry =
	glade_xml_get_widget(glade_xml, "output_file_entry");

    if (user_modified_output_file || strlen(input_file_and_path) == 0) {
        const gchar * r = gtk_entry_get_text(GTK_ENTRY(output_file_entry));
        if (!r) r = "";
        return r;
    }
    
    gchar *output_file_and_path;

    output_file_and_path = 
      (gchar *) g_malloc(strlen(input_file_and_path) + 32);

    strcpy(output_file_and_path, input_file_and_path);

    char * p = strrchr(output_file_and_path, '.');

    if (p) {
        gchar * ext = g_strdup(p + 1);
	*p = '\0';
	strcat(output_file_and_path, "_cpx.");
	strcat(output_file_and_path, ext);
	g_free (ext);
    } else {
        strcat(output_file_and_path, "_cpx");
    }

    gtk_entry_set_text(GTK_ENTRY(output_file_entry), output_file_and_path);

    return output_file_and_path;
}

void
update_buttons()
{
    GtkWidget * input_file_entry =
	glade_xml_get_widget(glade_xml, "input_file_entry");

    const gchar * input_file_and_path =
	gtk_entry_get_text(GTK_ENTRY(input_file_entry));

    if (!input_file_and_path)
        input_file_and_path = "";

    set_widget_sensitive("execute_button", 
        strlen(input_file_and_path) > 0);

    const gchar * output_file_and_path =
        update_output_filename(input_file_and_path);

    gchar * input_file =
	g_path_get_basename(output_file_and_path);

    if (strlen(output_file_and_path) == 0 || strlen(input_file) == 0) {
	g_free(input_file);
	input_file = g_strdup("*");
    } else {
	char * p = strrchr(input_file, '.');
	if (p)
	    *p = '\0';
    }

    set_button_text(1, input_file, "_range_raw_t");
    set_button_text(2, input_file, "_range_raw_f");
    set_button_text(3, "", "range_ref_t");
    set_button_text(4, "", "range_ref_f");
    set_button_text(5, input_file, "_range_X_f");
    set_button_text(6, input_file, "_az_raw_t");
    set_button_text(7, input_file, "_az_raw_f");
    set_button_text(8, input_file, "_az_mig_f");
    set_button_text(9, input_file, "_az_ref_t");
    set_button_text(10, input_file, "_az_ref_f");
    set_button_text(11, input_file, "_az_X_f");
    set_button_text(12, input_file, "_az_X_t");

    g_free(input_file);
}

SIGNAL_CALLBACK void
on_input_file_entry_changed(GtkEditable *editable, gpointer user_data)
{
    update_buttons();
}

SIGNAL_CALLBACK gboolean
on_output_file_entry_key_press_event(GtkEditable *editable, gpointer user_data)
{
    user_modified_output_file = TRUE;
    return FALSE;
}

int
keep_flag(int step, int value)
{
    char button_id[32];
    sprintf(button_id, "step%d_togglebutton", step);

    GtkWidget * toggle_button =
	glade_xml_get_widget(glade_xml, button_id);

    if (!GTK_WIDGET_IS_SENSITIVE(toggle_button))
	return 0;

    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(toggle_button)))
	return value;
    else
	return 0;
}

int
execute_flag(const char *id, int value)
{
    char button_id[128];
    sprintf(button_id, "%s_checkbutton", id);

    GtkWidget * checkbutton =
	glade_xml_get_widget(glade_xml, button_id);

    if (gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(checkbutton)))
	return 0;
    else
	return value;
}

void
set_widgets_sensitive(gboolean setting)
{
    set_widget_sensitive("execute_button", setting);
    set_widget_sensitive("azimuth_complex_multiply_checkbutton", setting);
    set_widget_sensitive("range_complex_multiply_checkbutton", setting);
    set_widget_sensitive("range_migration_checkbutton", setting);
    set_widget_sensitive("step1_togglebutton", setting);
    set_widget_sensitive("step2_togglebutton", setting);
    set_widget_sensitive("step3_togglebutton", setting);
    set_widget_sensitive("step4_togglebutton", setting);
    set_widget_sensitive("step5_togglebutton", setting);
    set_widget_sensitive("step6_togglebutton", setting);
    set_widget_sensitive("step7_togglebutton", setting);
    set_widget_sensitive("step8_togglebutton", setting);
    set_widget_sensitive("step9_togglebutton", setting);
    set_widget_sensitive("step10_togglebutton", setting);
    set_widget_sensitive("step11_togglebutton", setting);
    set_widget_sensitive("step12_togglebutton", setting);
    set_widget_sensitive("input_file_entry", setting);
    set_widget_sensitive("input_file_browse_button", setting);
}

gchar *
change_extension(const gchar * file, const gchar * ext)
{
    gchar * replaced = (gchar *)
        g_malloc(sizeof(gchar) * (strlen(file) + strlen(ext) + 10));

    strcpy(replaced, file);
    char * p = strrchr(replaced, '.');

    if (p)
    {
        *p = '\0';
    }

    strcat(replaced, ".");
    strcat(replaced, ext);

    return replaced;
}

static int
check_files(const char * input_file)
{
    const int STATUS_OK = 1;
    const int STATUS_FILE_NOT_FOUND = 2;
    const int STATUS_META_FILE_NOT_FOUND = 3;
    const int STATUS_LDR_INSTEAD = 4;

    int status;

    gchar * meta_file = change_extension(input_file, "meta");
    gchar * in_file = change_extension(input_file, "in");
    gchar * fmt_file = change_extension(input_file, "fmt");

    int meta_exists = g_file_test(meta_file, G_FILE_TEST_EXISTS);
    int in_exists = g_file_test(in_file, G_FILE_TEST_EXISTS);
    int fmt_exists = g_file_test(fmt_file, G_FILE_TEST_EXISTS);

    if (!g_file_test(input_file, G_FILE_TEST_EXISTS))
    {
      status = STATUS_FILE_NOT_FOUND;
    }
    else
    {
      if (meta_exists && in_exists && fmt_exists)
      {
	status = STATUS_OK;
      }
      else
      {
	gchar * ldr_file = change_extension(input_file, "ldr");
      
	if (g_file_test(ldr_file, G_FILE_TEST_EXISTS))
	{
	  status = STATUS_LDR_INSTEAD;
	}
	else
	{
	  g_free(ldr_file);
	  ldr_file = change_extension(input_file, "LDR");
	  
	  if (g_file_test(ldr_file, G_FILE_TEST_EXISTS))
	    status = STATUS_LDR_INSTEAD;
	  else
	    status = STATUS_META_FILE_NOT_FOUND;
	}

	g_free(ldr_file);
      }
    }

    if (status == STATUS_FILE_NOT_FOUND)
    {
      char msg[1024];
      sprintf(msg, "Couldn't find the input file: %s", input_file);
      message_box(msg);
    }
    else if (status == STATUS_META_FILE_NOT_FOUND)
    {
      char *meta_file_ok = meta_exists ? "FOUND" : "NOT FOUND";
      char *in_file_ok = in_exists ? "FOUND" : "NOT FOUND";
      char *fmt_file_ok = fmt_exists ? "FOUND" : "NOT FOUND";

      char msg[2048];
      sprintf(msg, "Not all of the required files were found.\n"
	      "  %s: FOUND\n"
	      "  %s: %s\n"
	      "  %s: %s\n"
	      "  %s: %s",
	      input_file,
	      meta_file, meta_file_ok,
	      in_file, in_file_ok,
	      fmt_file, fmt_file_ok);

      message_box(msg);
    }
    else if (status == STATUS_LDR_INSTEAD)
    {
      message_box("It looks like you have selected a Level 0 CEOS File.\n"
		  "This tool requires that you first import the data into\n"
		  "ASF Internal Format.  You will need to run the ASF\n"
		  "Convert tool first.");
    }

    g_free (meta_file);
    g_free (in_file);
    g_free (fmt_file);

    return status == STATUS_OK;
}

SIGNAL_CALLBACK void
on_execute_button_clicked(GtkWidget *button, gpointer user_data)
{
    int debug_flag = 
	keep_flag(1, 8192) |
	keep_flag(2, 4096) |
	keep_flag(3, 2048) |
	keep_flag(4, 1024) |
	keep_flag(5, 512) |
	keep_flag(6, 128) |
	keep_flag(7, 64) |
	keep_flag(8, 32) |
	keep_flag(9, 16) |
	keep_flag(10, 8) |
	keep_flag(11, 4) |
	keep_flag(12, 2) |
	execute_flag("range_complex_multiply", 16384) |
	execute_flag("range_migration", 32768) | 
        execute_flag("azimuth_complex_multiply", 65536);

    if (debug_flag == 0)
      debug_flag = 1;

    GtkWidget * input_file_entry =
	glade_xml_get_widget(glade_xml, "input_file_entry");

    const char * input_file_c =
	gtk_entry_get_text(GTK_ENTRY(input_file_entry));

    GtkWidget * output_file_entry =
	glade_xml_get_widget(glade_xml, "output_file_entry");

    const char * output_file_c =
	gtk_entry_get_text(GTK_ENTRY(output_file_entry));

	/* make a copy for ourselves - on Windows, after fork we can't */
	/* access pointers to GTK-owned data (like input_file_c)       */
    char *input_file = MALLOC(sizeof(char)*(strlen(input_file_c)+2));
    strcpy(input_file, input_file_c);

    char *output_file = MALLOC(sizeof(char)*(strlen(output_file_c)+2));
    strcpy(output_file, output_file_c);

    GtkWidget * start_line_entry =
	glade_xml_get_widget(glade_xml, "start_line_entry");

    int ifirstline =
        atoi(gtk_entry_get_text(GTK_ENTRY(start_line_entry)));

    /* check that we have all the required files */
    if (!check_files(input_file))
        return;

    if (!output_file || strlen(output_file) == 0) {
        char * p = strrchr(input_file, '.');

        output_file =
            (char *) malloc(sizeof(char) * (strlen(input_file) + 20));

        if (p) {
            char * ext = strdup(p + 1);
            strcpy(output_file, input_file);
            *(output_file + (p - input_file)) = '\0';
            strcat(output_file, "_cpx");
            //strcat(output_file, ext);
            free(ext);
        } else {
            sprintf(output_file, "%s%s", input_file, "_cpx");
        }
    }

    // strip off the output file extension, ardop expects it that way
    char *output_file_ext = findExt(output_file);
    if (output_file_ext) *output_file_ext = '\0';

    GtkWidget *ardop_main =
        glade_xml_get_widget(glade_xml, "ardop_main");

    gtk_widget_hide(ardop_main);

    int pid = fork();

    if (pid == 0)
    {
      struct INPUT_ARDOP_PARAMS *params_in;
      params_in = get_input_ardop_params_struct(input_file, output_file);

      int npatches = 1;
      params_in->iflag = &debug_flag;
      params_in->npatches = &npatches;
      params_in->ifirstline = &ifirstline;

      ardop(params_in);

      exit(EXIT_SUCCESS);
    }
    else
    {
      while (waitpid(-1, NULL, WNOHANG) == 0)
      {
	while (gtk_events_pending())
          gtk_main_iteration();

	g_usleep(50);
      }     
    }
    
    free(output_file);

#if 1
    gtk_widget_show(ardop_main);
#else
    gtk_main_quit();
#endif
}

void
set_underlines_off(int step)
{
    char button_id[32];

    sprintf(button_id, "step%d_togglebutton", step);

    GtkWidget * button =
	glade_xml_get_widget(glade_xml, button_id);

    gtk_button_set_use_underline(GTK_BUTTON(button), FALSE);
}

void
toggle_button(const char * name, gboolean setting)
{
    GtkWidget * btn =
      glade_xml_get_widget(glade_xml, name);

    gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(btn), setting);
}

void
set_toggles()
{
    toggle_button("range_migration_checkbutton", TRUE);
    toggle_button("range_complex_multiply_checkbutton", TRUE);
    toggle_button("azimuth_complex_multiply_checkbutton", TRUE);

    range_complex_multiply_checkbutton_toggled();
    range_migration_checkbutton_toggled();
    azimuth_complex_multiply_checkbutton_toggled();

    int i;
    for (i = 1; i <= 12; ++i)
	set_underlines_off(i);
}

void
help_text(int step)
{
  static const char * step1_help_title =
    "Step 1: Raw data (time)";

  static const char * step1_help =
    "The raw data is, at its simplest, a series of complex numbers "
    "(I and Q) that were sampled at the instrument and recorded at "
    "the ground station. By the time the data gets to the processor, "
    "it typically is digitized and stripped of encoding and metadata. "
    "Values are generally 4-bit or 5-bit.";

  static const char * step2_help_title =
    "Step 2: Range FFT";

  static const char * step2_help =
    "A fast Fourier transform converts the data from time domain (the "
    "way it was recorded at the receiver) to frequency domain. The data "
    "were acquired as a function of range, so each range line is "
    "transformed one at a time.\n"
    "\n"
    "This raw data set has the same information as the one in the time "
    "domain, but is now in the frequency domain.";

  static const char * step3_help_title =
    "Step 3: Range reference function";

  static const char * step3_help =
    "The range reference function is a series of complex numbers that "
    "represents the original chirp transmitted by the antenna. It is "
    "sampled at the same rate as the raw data, and is either pulled "
    "from the raw data or generated from metadata values such as chirp "
    "rate, center frequency, or bandwidth.";

  static const char * step4_help_title =
    "Step 4: Range reference function (frequency domain)";

  static const char * step4_help =
    "This range reference function has the same information as the one "
    "in the time domain, but is now in the frequency domain.";

  static const char * step5_help_title =
    "Step 5: Complex multiplication";

  static const char * step5_help =
    "Each element of the raw data (in frequency domain) is multiplied "
    "by the complex conjugate of the corresponding element of the "
    "range reference function (in frequency domain).\n"
    "\n"
    "The result from the complex multiplication is now ready for an "
    "inverse FFT.";

  static const char * step6_help_title =
    "Step 6: Range Inverse FFT";

  static const char * step6_help =
    "The result from the complex multiplication is converted back to "
    "time domain with the inverse fast Fourier transform.\n"
    "\n"
    "Range compressed data consists of the convolution of the range "
    "reference function with the raw data in the range direction. "
    "According to convolution theorem, converting two functions to "
    "frequency domain followed by a complex multiply, and converting "
    "back to time domain is the equivalent of the convolution of the "
    "two functions. The data is said to be 'compressed' because the "
    "chirp has been removed and all the signal returns from a single "
    "target are compressed into a single area. ";

  static const char * step7_help_title =
    "Step 7: Azimuth FFT";

  static const char * step7_help =
    "A fast Fourier transform converts the data from time domain (the "
    "way it was recorded at the receiver) to frequency domain. The "
    "data are typically corner-turned so that what was a column of "
    "data in the raw data array is now a row. This is the physical "
    "equivalent of ordering values from different pulses that were "
    "acquired at the same relative time within the pulse. These "
    "iso-range azimuth lines are transformed one at a time.\n"
    "\n"
    "This data set has the same information as the one in the time "
    "domain, but is now in the frequency domain after the time domain "
    "data was corner turned, changing the orientation from range to "
    "azimuth.";

  static const char * step8_help_title =
    "Step 8: Range Cell migration";

  static const char * step8_help =
    "As a target was sampled in the azimuth direction, the returns "
    "are not necessarily contained within a signal iso-range line. "
    "The migration of the signal from one bin into another means "
    "that some of the power you want to compress is in a completely "
    "different column of data. Range cell migration realigns all "
    "the returns for a single target into an appropriate single "
    "line of data in preparation for azimuth compression.\n"
    "\n"
    "All the azimuth returns from a single target have now been "
    "shifted into their respective lines so that azimuth compression "
    "will lead to well-focussed data. If you leave out range cell "
    "migration, returns from targets can be distributed across two "
    "or three azimuth lines. The result in the final image is "
    "poor resolution and smearing of the target in the range "
    "direction.";

  static const char * step9_help_title =
    "Step 9: Azimuth reference function (time)";

  static const char * step9_help =
    "The azimuth reference function is similar to the range reference "
    "function. In this case, however, the geometry of the satellite "
    "moving past the target changes the frequency on the return signal "
    "in the azimuth direction.  This Doppler effect, with higher "
    "frequencies as the satellite is moving toward the target, and "
    "lower frequencies as it moves away, is a sort of 'chirp'. The "
    "azimuth reference function is an array of numbers that represent "
    "the chirp, and is calculated uniquely for each image, and each "
    "iso-range line in the azimuth direction.\n"
    "\n"
    "It is critical that the geometry is estimated accurately, but this "
    "can be difficult, especially for low signal-to-noise data. Most "
    "processing errors come from getting this geometry wrong. The "
    "geometry is also referred to as the Doppler Centroid.";

  static const char * step10_help_title =
    "Step 10: Azimuth reference function (frequency)";

  static const char * step10_help =
    "This azimuth reference function has the same information as the "
    "one in the time domain, but is now in the frequency domain.";

  static const char * step11_help_title =
    "Step 11: Complex multiplication";

  static const char * step11_help =
    "Each element of the data (in frequency domain) is multiplied by "
    "the complex conjugate of the corresponding element of the "
    "azimuth reference function (in frequency domain).";

  static const char * step12_help_title =
    "Step 12: Azimuth compressed data (time domain - final product)";

  static const char * step12_help =
    "The result from the complex multiplication is converted back to "
    "time domain with the inverse fast Fourier transform.\n"
    "\n"
    "The data have now been compressed in both dimensions (range "
    "and azimuth) and include adjustments for range-cell migration. "
    "A few steps remain to get the final image. The data are typically "
    "corner turned again (to return it to the recognizable range-azimuth "
    "direction), and then converted from complex data to an amplitude "
    "and a phase image. The images are then resampled to the desired "
    "pixel size, and any radiometric calibration or adjustments are "
    "applied. For visualization purposes, we have taken the phase "
    "image and converted it to color assigned by phase range.";

  const char *title;
  const char *help_text;

  switch (step) {
    default:
      title = "";                help_text = "";          break;
    case 1:
      title = step1_help_title;  help_text = step1_help;  break;
    case 2:
      title = step2_help_title;  help_text = step2_help;  break;
    case 3:
      title = step3_help_title;  help_text = step3_help;  break;
    case 4:
      title = step4_help_title;  help_text = step4_help;  break;
    case 5:
      title = step5_help_title;  help_text = step5_help;  break;
    case 6:
      title = step6_help_title;  help_text = step6_help;  break;
    case 7:
      title = step7_help_title;  help_text = step7_help;  break;
    case 8:
      title = step8_help_title;  help_text = step8_help;  break;
    case 9:
      title = step9_help_title;  help_text = step9_help;  break;
    case 10:
      title = step10_help_title; help_text = step10_help; break;
    case 11:
      title = step11_help_title; help_text = step11_help; break;
    case 12:
      title = step12_help_title; help_text = step12_help; break;
  }

  GtkWidget * help_label =
    glade_xml_get_widget(glade_xml, "help_label");

  char label_text[2048];
  sprintf(label_text, "\n%s\n\n%s\n", title, help_text);

  gtk_label_set_text(GTK_LABEL(help_label), label_text);

  GtkWidget * flowchart_image;

  flowchart_image =
    glade_xml_get_widget(glade_xml, "flowchart_image");

  char image_file[128];
  sprintf(image_file, "step%d.gif", step);

  gtk_image_set_from_file(GTK_IMAGE(flowchart_image), imgloc(image_file));
}

SIGNAL_CALLBACK void
on_step1_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(1);
}

SIGNAL_CALLBACK void
on_step2_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(2);
}

SIGNAL_CALLBACK void
on_step3_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(3);
}

SIGNAL_CALLBACK void
on_step4_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(4);
}

SIGNAL_CALLBACK void
on_step5_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(5);
}

SIGNAL_CALLBACK void
on_step6_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(6);
}

SIGNAL_CALLBACK void
on_step7_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(7);
}

SIGNAL_CALLBACK void
on_step8_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(8);
}

SIGNAL_CALLBACK void
on_step9_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(9);
}

SIGNAL_CALLBACK void
on_step10_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(10);
}

SIGNAL_CALLBACK void
on_step11_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(11);
}

SIGNAL_CALLBACK void
on_step12_help_button_clicked(GtkWidget *button, gpointer user_data)
{
  help_text(12);
}

static void
hide_doppler_parameters_dialog ()
{
    GtkWidget *doppler_parameters_dialog =
	glade_xml_get_widget(glade_xml, "doppler_parameters_dialog");
 
    gtk_widget_hide(doppler_parameters_dialog);
}

SIGNAL_CALLBACK void
on_doppler_parameters_dialog_cancel_button_clicked(GtkWidget *w)
{
    hide_doppler_parameters_dialog ();
}

SIGNAL_CALLBACK gboolean
on_doppler_parameters_dialog_delete_event(GtkWidget *w)
{
    hide_doppler_parameters_dialog ();
    return TRUE;
}
 
SIGNAL_CALLBACK gboolean
on_doppler_parameters_dialog_destroy_event(GtkWidget *w)
{
    hide_doppler_parameters_dialog ();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_doppler_parameters_dialog_destroy(GtkWidget *w)
{
    hide_file_selection_dialog ();
    return TRUE;
}

static void readline(FILE * f, gchar * buffer, size_t n)
{
    gchar * p;
    gchar * newline;

    p = fgets(buffer, n, f);

    if (!p)
    {
        strcpy(buffer, "");
    }
    else
    {
        newline = strrchr(buffer, '\n');
	if (newline)
	    *newline = '\0';
    }
}

static void
read_doppler_parameters(const gchar * filename, double *constant,
			double *linear, double *quadratic)
{
    FILE * fp = fopen(filename, "rt");
    gboolean found = FALSE;

    while (fp && !feof(fp))
    {
        gchar buf[256];
        readline(fp, buf, 256);

	char * p = strstr(buf, "Dopp quad coefs(Hz/prf)");
	if (p)
	{
	    int n = sscanf(buf, "%lg %lg %lg", constant, linear, quadratic);
	    if (n == 3) {
	        found = TRUE;
		break;
	    }
	}
    }

    fclose(fp);

    if (!found)
    {
        *constant = 0.0;
	*linear = 0.0;
	*quadratic = 0.0;
    }
}

static gchar *
generate_org_filename(const gchar * filename)
{
    gchar * org_filename = (gchar *)
        g_malloc(sizeof(gchar) * (strlen(filename) + 5));

    strcpy(org_filename, filename);
    strcat(org_filename, ".org");

    return org_filename;
}

static gchar *
get_in_file_name()
{
    GtkWidget * input_file_entry =
	glade_xml_get_widget (glade_xml, "input_file_entry");

    const gchar * input_file =
        gtk_entry_get_text(GTK_ENTRY(input_file_entry));

    gchar * in_file = change_extension(input_file, "in");

    return in_file;
}

static void
generate_org_file_if_needed()
{
    const gchar * filename = get_in_file_name();

    gchar * org_filename = generate_org_filename(filename);

    if (!g_file_test(org_filename, G_FILE_TEST_EXISTS ))
    {
        gchar buf[1024];
#ifdef win32
	/* on windows, we seem to have to brute force the file copy */
	FILE * in = fopen(filename, "rt");
	FILE * out = fopen(org_filename, "wt");

	if (!in || !out) {
	  printf("Error creating .org file.\n");
	}
	else
	{
	  while (!feof(in)) {
	    readline(in, buf, 1024);
	    fprintf(out, "%s\n", buf);
	  }
	}

	if (in) fclose(in);
	if (out) fclose(out);
#else
	sprintf(buf, "cp \"%s\" \"%s\"", filename, org_filename);
	system(buf);
#endif
    }

    g_free(org_filename);
}

static void
write_doppler_parameters(const gchar * filename, double constant,
			 double linear, double quadratic)
{
    gchar * org_filename = generate_org_filename(filename);
 
    FILE * ifp = fopen(org_filename, "rt");
    FILE * ofp = fopen(filename, "wt");

    while (ifp && !feof(ifp))
    {
        gchar buf[256];
        readline(ifp, buf, 256);

	char * p = strstr(buf, "Dopp quad coefs(Hz/prf)");
	if (p)
	{
	    fprintf(ofp, "%g %g %g\t\t! Dopp quad coefs(Hz/prf)\n",
		    constant, linear, quadratic); 
	}
	else
	{
	    fprintf(ofp, "%s\n", buf);
	}
    }

    if (ifp) fclose(ifp);
    if (ofp) fclose(ofp);
}

static void
set_entry_with_double_value(const gchar * entry_name, double value)
{
    gchar s[64];
    sprintf(s, "%g", value);

    GtkWidget * w = glade_xml_get_widget (glade_xml, entry_name);
    gtk_entry_set_text(GTK_ENTRY(w), s);
}

SIGNAL_CALLBACK void
on_edit_doppler_parameters_button_clicked(GtkWidget *w)
{
    GtkWidget * doppler_parameters_dialog =
	glade_xml_get_widget (glade_xml, "doppler_parameters_dialog");

    gtk_widget_show (doppler_parameters_dialog);

    gchar * in_file = get_in_file_name();

    double constant, linear, quadratic;
    read_doppler_parameters(in_file, &constant, &linear, &quadratic);

    set_entry_with_double_value("constant_entry", constant);
    set_entry_with_double_value("linear_entry", linear);
    set_entry_with_double_value("quadratic_entry", quadratic);

    g_free(in_file);
}

static void
get_entry_with_double_value(const char * entry_name, double * value)
{
    GtkWidget * w = glade_xml_get_widget (glade_xml, entry_name);
    const gchar * s = gtk_entry_get_text(GTK_ENTRY(w));
    sscanf(s, "%lg", value);
}

SIGNAL_CALLBACK void
on_doppler_parameters_dialog_ok_button_clicked(GtkWidget *w)
{
    generate_org_file_if_needed();

    double constant, linear, quadratic;
    get_entry_with_double_value("constant_entry", &constant);
    get_entry_with_double_value("linear_entry", &linear);
    get_entry_with_double_value("quadratic_entry", &quadratic);

    const gchar * filename = get_in_file_name();
    write_doppler_parameters(filename, constant, linear, quadratic);

    hide_doppler_parameters_dialog();
}

SIGNAL_CALLBACK void
on_doppler_parameters_dialog_restore_button_clicked(GtkWidget *w)
{
    gchar * in_file = get_in_file_name();
    gchar * org_file = generate_org_filename(in_file);
    
    double constant, linear, quadratic;
    read_doppler_parameters(org_file, &constant, &linear, &quadratic);

    set_entry_with_double_value("constant_entry", constant);
    set_entry_with_double_value("linear_entry", linear);
    set_entry_with_double_value("quadratic_entry", quadratic);

    g_free(in_file);
    g_free(org_file);
}

int
main(int argc, char **argv)
{
    gchar *glade_xml_file;

    gtk_init(&argc, &argv);

    glade_xml_file = (gchar *) find_in_share("stp.glade");
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);

    g_free(glade_xml_file);

    if (argc > 1)
        add_file(argv[1]);

    /* add version number to window title */
    char title[256];
    sprintf(title,
            "SAR Training Processor: Version %s", STP_VERSION);

    GtkWidget *widget = glade_xml_get_widget (glade_xml, "ardop_main");
    gtk_window_set_title(GTK_WINDOW(widget), title);

    set_font();
    set_images();
    set_toggles();
    help_text(1);
    update_buttons();

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();
    
    exit (EXIT_SUCCESS);
}
