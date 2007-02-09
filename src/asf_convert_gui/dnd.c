#include "asf_convert_gui.h"

static void
target_drag_data_received(GtkWidget *widget,
                          GdkDragContext *context,
                          gint x,
                          gint y,
                          GtkSelectionData *data,
                          guint info,
                          guint time)
{
    gchar ** list, ** iter;
    gchar * delim = "\n";
    int i, n;

    list = g_strsplit((const gchar *)data->data, delim, 0);
    iter = list;
    i = n = 0;

    while (*iter)
    {
        gchar * data_file = g_strdup( *iter );
        g_strstrip( data_file );

        if (data_file && strlen(data_file) > 0)
        {
            gboolean result;
            gchar * p = data_file;

            if (g_str_has_prefix(p, "file://"))
                p += 7;

            result = add_to_files_list(p);

            if (result)
                ++i;

            ++n;
        }

        ++iter;
        g_free(data_file);
    }

    if (i != n)
    {
        if (n == 1 || i == 0)
        {
            message_box("Error: Unrecognized extension.");
        }
        else
        {
            message_box("Some of the files were not added -- unknown extensions.");
        }
    }

    g_strfreev(list);
	show_queued_thumbnails();
}

static GtkTargetEntry target_table[] = {
    { "text/plain", 0, 0 }
};

void
setup_dnd()
{
    GtkWidget *widget= get_widget_checked("files_list");

    gtk_drag_dest_set (widget, GTK_DEST_DEFAULT_ALL, 
        target_table, 1, GDK_ACTION_COPY);

    gtk_signal_connect( GTK_OBJECT(widget), "drag_data_received",
        GTK_SIGNAL_FUNC(target_drag_data_received), NULL );
}
