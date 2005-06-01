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

#include "find_in_path.h"


/* for win32, need __declspec(dllexport) on all signal handlers */
#if !defined(SIGNAL_CALLBACK)
#  if defined(win32)
#    define SIGNAL_CALLBACK __declspec(dllexport)
#  else
#    define SIGNAL_CALLBACK
#  endif
#endif

GladeXML *glade_xml;

static void set_images()
{
    GtkWidget * range_compression_image;
    GtkWidget * azimuth_compression_image;

    range_compression_image =
	glade_xml_get_widget(glade_xml, "range_compression_image");

    azimuth_compression_image =
	glade_xml_get_widget(glade_xml, "azimuth_compression_image");

    gtk_image_set_from_file(GTK_IMAGE(range_compression_image), "rc.png");
    gtk_image_set_from_file(GTK_IMAGE(azimuth_compression_image), "ac.png");
}

SIGNAL_CALLBACK void
on_input_file_browse_button_clicked(GtkWidget *button)
{
    GtkWidget * file_selection_dialog =
	glade_xml_get_widget (glade_xml, "file_selection_dialog");

    gtk_widget_show (file_selection_dialog);
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

static void
add_file (const gchar * filename)
{
    GtkWidget *input_file_entry;

    input_file_entry =
	glade_xml_get_widget(glade_xml, "input_file_entry");

    gtk_entry_set_text(GTK_ENTRY(input_file_entry), filename);

    /* FIXME: Update all the temp output file labels */
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
on_aisp_main_destroy(GtkWidget *w, gpointer data)
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

    set_step_enabled(7, is_checked);
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

void
update_buttons()
{
    GtkWidget * input_file_entry =
	glade_xml_get_widget(glade_xml, "input_file_entry");

    const gchar * input_file_and_path =
	gtk_entry_get_text(GTK_ENTRY(input_file_entry));

    gchar * input_file =
	g_path_get_basename(input_file_and_path);

    if (strlen(input_file) == 0) {
	g_free(input_file);
	input_file = g_strdup("*");
    } else {
	char * p = strrchr(input_file, '.');
	if (p)
	    *p = '\0';
    }

    set_button_text(1, input_file, "_range_raw_t");
    set_button_text(2, input_file, "_range_raw_f");
    set_button_text(3, input_file, "_range_ref_t");
    set_button_text(4, input_file, "_range_ref_f");
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
	return value;
    else
	return 0;
}

void
execute()
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
	execute_flag("azimuth_complex_multiply", 32768) |
	execute_flag("range_migration", 65536);

    GtkWidget * input_file_entry =
	glade_xml_get_widget(glade_xml, "input_file_entry");

    const char * input_file =
	gtk_entry_get_text(GTK_ENTRY(input_file_entry));

    char * p = strrchr(input_file, '.');

    char * output_file =
	(char *) malloc(sizeof(char) * (strlen(input_file) + 20));
    
    if (p) {
	char * ext = strdup(p + 1);
	strcpy(output_file, input_file);
	*(output_file + (p - input_file)) = '\0';
	strcat(output_file, "_output.");
	strcat(output_file, ext);
	free(ext);
    } else {
	sprintf(output_file, "%s%s", input_file, "_output");
    }

    char cmd[1024];
    sprintf(cmd, "aisp -debug %d %s %s", debug_flag, input_file, output_file);

    free(output_file);
    printf("%s\n", cmd);

    system(cmd);
}

SIGNAL_CALLBACK void
on_execute_button_clicked(GtkWidget *button, gpointer user_data)
{
    GtkWidget *aisp_main_scrolledwindow =
      glade_xml_get_widget(glade_xml, "aisp_main_scrolledwindow");

    gtk_widget_set_sensitive(aisp_main_scrolledwindow, FALSE);

    int pid = fork();

    if (pid == 0)
    {
      execute();
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

    gtk_widget_set_sensitive(aisp_main_scrolledwindow, TRUE);
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
set_toggles()
{
    // FIXME set default checkboxes here...

    range_complex_multiply_checkbutton_toggled();
    range_migration_checkbutton_toggled();
    azimuth_complex_multiply_checkbutton_toggled();

    int i;
    for (i = 1; i <= 12; ++i)
	set_underlines_off(i);
}

int
main(int argc, char **argv)
{
    gchar *glade_xml_file;

    gtk_init(&argc, &argv);
    // set_font();

    glade_xml_file = (gchar *) find_in_path("aisp_gui.glade");
    glade_xml = glade_xml_new(glade_xml_file, NULL, NULL);

    g_free(glade_xml_file);

    set_images();
    set_toggles();

    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();
    
    exit (EXIT_SUCCESS);
}
