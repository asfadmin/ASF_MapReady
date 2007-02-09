#include "asf_convert_gui.h"
#include "asf.h"
#include <gdk/gdkkeysyms.h>

void show_log(gchar * log_txt, gchar * data_file)
{
    GtkWidget *log_dialog = get_widget_checked("log_dialog");
    GtkWidget *log_text = get_widget_checked("log_text");

    GtkTextBuffer * text_buffer;
    text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(log_text));
    gtk_text_buffer_set_text(text_buffer, "", -1);

    gchar *label_text = g_malloc(sizeof(gchar) * (strlen(data_file) + 128));

    GtkTextIter start, end;
    gtk_text_buffer_get_end_iter(text_buffer, &end);
    gtk_text_buffer_insert(text_buffer, &end, log_txt, -1);

    static GtkTextTag *tt = NULL;
    
    if (!tt)
    {
#ifdef win32
        const char *fnt = "Courier";
#else
        const char *fnt = "Mono";
#endif
        tt = gtk_text_buffer_create_tag(text_buffer, "mono",
                                        "font", fnt, NULL);
    }

    gtk_text_buffer_get_start_iter(text_buffer, &start);
    gtk_text_buffer_get_end_iter(text_buffer, &end);

    gtk_text_buffer_apply_tag(text_buffer, tt, &start, &end);    

    GtkWidget *log_label = get_widget_checked("log_label");

    sprintf(label_text, "Processing Log For: %s", data_file);
    gtk_label_set_text(GTK_LABEL(log_label), label_text);

    gtk_widget_show(log_dialog);

    /* user may have selected "View Log" when the log
       window was already opened -- bring it to the top */
    gtk_window_present(GTK_WINDOW(log_dialog));

    g_free(label_text);
}

void log_hide()
{
    GtkWidget *log_dialog = get_widget_checked("log_dialog");
    gtk_widget_hide(log_dialog);
}


SIGNAL_CALLBACK void
on_log_dialog_ok_button_clicked(GtkWidget *widget)
{
    log_hide();
}

SIGNAL_CALLBACK gboolean
on_log_dialog_delete_event(GtkWidget *widget)
{
    log_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_log_dialog_destroy_event(GtkWidget *widget)
{
    log_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_log_dialog_destroy(GtkWidget *widget)
{
    log_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_log_dialog_key_press_event(GtkWidget * widget, 
                              GdkEventKey * event,
                              GtkWidget * win)
{
    if (event->keyval == GDK_Return)
    {
        log_hide();
        return TRUE;
    }

    return FALSE;
}
