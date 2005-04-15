#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <gtk/gtk.h>
#include <glade/glade.h>
#include <glib.h>
#include <glib/gprintf.h>

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
on_execute_button_clicked (GtkWidget *w)
{
}

SIGNAL_CALLBACK void
on_aisp_main_destroy(GtkWidget *w, gpointer data)
{
    gtk_main_quit();
}

void
range_complex_multiply_checkbutton_toggled()
{
    GtkWidget * range_complex_multiply_checkbutton =
	glade_xml_get_widget(glade_xml, "range_complex_multiply_checkbutton");

    GtkWidget * step3_label =
	glade_xml_get_widget(glade_xml, "step3_label");

    GtkWidget * step3_table =
	glade_xml_get_widget(glade_xml, "step3_table");

    GtkWidget * step4_label =
	glade_xml_get_widget(glade_xml, "step4_label");

    GtkWidget * step4_table =
	glade_xml_get_widget(glade_xml, "step4_table");

    GtkWidget * step5_label =
	glade_xml_get_widget(glade_xml, "step5_label");

    GtkWidget * step5_table =
	glade_xml_get_widget(glade_xml, "step5_table");

    gboolean is_checked =
	gtk_toggle_button_get_active(
	    GTK_TOGGLE_BUTTON(range_complex_multiply_checkbutton));

    gtk_widget_set_sensitive(step3_label, is_checked);
    gtk_widget_set_sensitive(step3_table, is_checked);
    gtk_widget_set_sensitive(step4_label, is_checked);
    gtk_widget_set_sensitive(step4_table, is_checked);
    gtk_widget_set_sensitive(step5_label, is_checked);
    gtk_widget_set_sensitive(step5_table, is_checked);
}

SIGNAL_CALLBACK void
on_range_complex_multiply_checkbutton_toggled(GtkWidget *widget)
{
    range_complex_multiply_checkbutton_toggled();
}

SIGNAL_CALLBACK void
on_range_migration_checkbutton_toggled(GtkWidget *widget)
{

}

SIGNAL_CALLBACK void
on_azimuth_complex_multiply_checkbutton_toggled(GtkWidget *widget)
{

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
    range_complex_multiply_checkbutton_toggled();
    glade_xml_signal_autoconnect(glade_xml);
    gtk_main ();
    
    exit (EXIT_SUCCESS);
}
