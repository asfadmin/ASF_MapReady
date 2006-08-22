#include "asf_convert_gui.h"
#include <gdk/gdkkeysyms.h>

static double scale = 0.3;

double get_selected_scale()
{
    return scale;
}

static void show_it(const gchar * filename, int is_new)
{
    GtkWidget *output_image_dialog;
    GtkWidget *output_image;
    gchar title[256];
    static GdkPixbuf *output_pixbuf = NULL;
    static GdkPixbuf *shown_pixbuf = NULL;

    GtkWidget *scrolled_window =
        glade_xml_get_widget(glade_xml, "view_output_scrolledwindow");
    GtkAdjustment *hadjust = gtk_scrolled_window_get_hadjustment(
        GTK_SCROLLED_WINDOW(scrolled_window));
    GtkAdjustment *vadjust = gtk_scrolled_window_get_vadjustment(
        GTK_SCROLLED_WINDOW(scrolled_window));

    double hpos = 0.5, vpos = 0.5;
    if (!is_new)
    {
        if (hadjust->upper == hadjust->page_size)
            hpos = hadjust->page_size / 2.0 / hadjust->upper;
        else
            hpos = (hadjust->value + hadjust->page_size / 2) / hadjust->upper;
        
        if (vadjust->upper == vadjust->page_size)
            vpos = vadjust->page_size / 2.0 / vadjust->upper;
        else
            vpos = (vadjust->value + vadjust->page_size / 2) / vadjust->upper;
    }

    if (is_new)
    {
        if (output_pixbuf) {
            // free any image from a previous "view output"
            gdk_pixbuf_unref(output_pixbuf);
            output_pixbuf = NULL;
        }
    }
    else
    {
        // should already have an output image loaded
        assert(output_pixbuf);
        assert(shown_pixbuf);
    }

    if (shown_pixbuf) {
        gdk_pixbuf_unref(shown_pixbuf);
        shown_pixbuf = NULL;
    }

    if (!output_pixbuf) {
        assert(filename);
        GError *err = NULL;
        output_pixbuf = gdk_pixbuf_new_from_file(filename, &err);
        if ( err != NULL ) {
            printf ("Couldn't open output image: %s\n", err->message);
            message_box("Error opening output image");
            return;
        }
    }

    double s = get_selected_scale();
    int w = gdk_pixbuf_get_width(output_pixbuf);
    int h = gdk_pixbuf_get_height(output_pixbuf);
    int scaled_w = (int)(w*s);
    int scaled_h = (int)(h*s);
    int bps = gdk_pixbuf_get_bits_per_sample(output_pixbuf);
    shown_pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE,
                                  bps, scaled_w, scaled_h);
    GdkInterpType interp = s>.2 ? GDK_INTERP_BILINEAR : GDK_INTERP_NEAREST;
    gdk_pixbuf_scale(output_pixbuf, shown_pixbuf, 0, 0, scaled_w, scaled_h,
                     0, 0, s, s, interp);

    output_image_dialog =
        glade_xml_get_widget(glade_xml, "output_image_dialog");

    output_image =
        glade_xml_get_widget(glade_xml, "output_image");

    gtk_image_set_from_pixbuf(GTK_IMAGE(output_image), shown_pixbuf);

    if (is_new)
    {
        GtkWidget *widget =
            glade_xml_get_widget (glade_xml, "zoom_level_optionmenu");
        set_combo_box_item(widget, 9);

        snprintf(title, sizeof(title), "Output Image - %s", filename);
        gtk_window_set_title(GTK_WINDOW(output_image_dialog), title);
        
        gtk_window_set_default_size(GTK_WINDOW(output_image_dialog), 800, 600);
        
        gtk_widget_show(output_image_dialog);
        
        /* user may have selected "View Output" when the image window 
           was already opened (for another image) -- bring it to the top */
        gtk_window_present(GTK_WINDOW(output_image_dialog));
    }

    double pos;
    hadjust = gtk_scrolled_window_get_hadjustment(
        GTK_SCROLLED_WINDOW(scrolled_window));
    pos = hpos * hadjust->upper - hadjust->page_size/2;
    gtk_adjustment_set_value(hadjust, pos);
    gtk_adjustment_changed(hadjust);

    vadjust = gtk_scrolled_window_get_vadjustment(
        GTK_SCROLLED_WINDOW(scrolled_window));
    pos = vpos * vadjust->upper - vadjust->page_size/2;
    gtk_adjustment_set_value(vadjust, pos);
    gtk_adjustment_changed(vadjust);
}

void show_output_image(const gchar * filename)
{
    show_it(filename, TRUE);
}

static void update_image()
{
    show_it(NULL, FALSE);
}

static void
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

SIGNAL_CALLBACK void on_zoom_level_200_activate(GtkWidget *widget)
{
    scale = 2.0;
    update_image();
}

SIGNAL_CALLBACK void on_zoom_level_150_activate(GtkWidget *widget)
{
    scale = 1.5;
    update_image();
}

SIGNAL_CALLBACK void on_zoom_level_100_activate(GtkWidget *widget)
{
    scale = 1.0;
    update_image();
}

SIGNAL_CALLBACK void on_zoom_level_90_activate(GtkWidget *widget)
{
    scale = 0.9;
    update_image();
}

SIGNAL_CALLBACK void on_zoom_level_80_activate(GtkWidget *widget)
{
    scale = 0.8;
    update_image();
}

SIGNAL_CALLBACK void on_zoom_level_70_activate(GtkWidget *widget)
{
    scale = 0.7;
    update_image();
}

SIGNAL_CALLBACK void on_zoom_level_60_activate(GtkWidget *widget)
{
    scale = 0.6;
    update_image();
}

SIGNAL_CALLBACK void on_zoom_level_50_activate(GtkWidget *widget)
{
    scale = 0.5;
    update_image();
}

SIGNAL_CALLBACK void on_zoom_level_40_activate(GtkWidget *widget)
{
    scale = 0.4;
    update_image();
}

SIGNAL_CALLBACK void on_zoom_level_30_activate(GtkWidget *widget)
{
    scale = 0.3;
    update_image();
}

SIGNAL_CALLBACK void on_zoom_level_20_activate(GtkWidget *widget)
{
    scale = 0.2;
    update_image();
}

SIGNAL_CALLBACK void on_zoom_level_10_activate(GtkWidget *widget)
{
    scale = 0.1;
    update_image();
}

SIGNAL_CALLBACK void on_zoom_level_5_activate(GtkWidget *widget)
{
    scale = 0.05;
    update_image();
}
