#include "asf_convert_gui.h"
#include <gdk/gdkkeysyms.h>

void show_output_image(const gchar * filename)
{
    GtkWidget *output_image_dialog;
    GtkWidget *output_image;
    gchar title[256];

    output_image_dialog =
	glade_xml_get_widget(glade_xml, "output_image_dialog");
    
    output_image =
	glade_xml_get_widget(glade_xml, "output_image");

    gtk_image_set_from_file(GTK_IMAGE(output_image), filename);

    snprintf(title, sizeof(title), "Output Image - %s", filename);
    gtk_window_set_title(GTK_WINDOW(output_image_dialog), title);

    gtk_window_set_default_size(GTK_WINDOW(output_image_dialog), 800, 600);

    gtk_widget_show(output_image_dialog);

    /* user may have selected "View Output" when the image window
       window was already opened (for another image) -- bring it to the top */
    gtk_window_present(GTK_WINDOW(output_image_dialog));
}

void
output_image_hide()
{
    GtkWidget *output_image_dialog =
	glade_xml_get_widget(glade_xml, "output_image_dialog");
    
    gtk_widget_hide(output_image_dialog);
}

SIGNAL_CALLBACK void
on_output_image_dialog_ok_button_clicked(GtkWidget *widget)
{
    output_image_hide();
}

SIGNAL_CALLBACK gboolean
on_output_image_dialog_delete_event(GtkWidget *widget)
{
    output_image_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_output_image_dialog_destroy_event(GtkWidget *widget)
{
    output_image_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_output_image_dialog_destroy(GtkWidget *widget)
{
    output_image_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_output_image_dialog_key_press_event(GtkWidget * widget, 
			       GdkEventKey * event,
			       GtkWidget * win)
{
    if (event->keyval == GDK_Return)
    {
	output_image_hide();
	return TRUE;
    }

    return FALSE;
}
