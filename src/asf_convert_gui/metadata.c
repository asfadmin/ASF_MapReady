#include "asf_convert_gui.h"
#include <gdk/gdkkeysyms.h>

static const int max_line_len = 2048;

gchar * build_metadata_filename(gchar * name)
{
    gchar * p;

    p = strrchr(name, '.');
    if (!p)
    {
	return g_strdup(name);
    }
    else
    {
	gchar * ret;	
	ret = (gchar *) g_malloc( sizeof(gchar) * (strlen(name) + 5) );

	strcpy(ret, name);
	*(ret + (p - name + 1)) = '\0';

	strcat(ret, "meta");

	return ret;
    }
}

void show_meta_data(gchar * out_name)
{
    GtkWidget *metadata_dialog;
    GtkWidget *metadata_text;
    GtkWidget *metadata_label;
    GtkTextBuffer * text_buffer;
    FILE * metadata_file;
    gchar * metadata_filename;
    gchar * label_text;
    
    metadata_dialog =
	glade_xml_get_widget(glade_xml, "metadata_dialog");
    
    metadata_text =
	glade_xml_get_widget(glade_xml, "metadata_text");
    
    text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(metadata_text));
    
    gtk_text_buffer_set_text(text_buffer, "", -1);
    
    metadata_filename = build_metadata_filename(out_name);
    
    label_text = (gchar *) g_malloc(sizeof(gchar) * 
				    (strlen(metadata_filename) + 32));

    metadata_file = fopen(metadata_filename, "rt");

    if (metadata_file)
    {
	gchar * buffer = (gchar *) g_malloc(sizeof(gchar) * max_line_len);
	while (!feof(metadata_file))
	{
	    gchar *p = fgets(buffer, max_line_len, metadata_file);
	    if (p)
	    {
		GtkTextIter end;
		
		gtk_text_buffer_get_end_iter(text_buffer, &end);
		gtk_text_buffer_insert(text_buffer, &end, buffer, -1);
	    }
	}
	
	fclose(metadata_file);
	g_free(buffer);

	metadata_label =
	    glade_xml_get_widget(glade_xml, "metadata_label");
		
	sprintf(label_text, "Meta Data File: %s", metadata_filename);
	gtk_label_set_text(GTK_LABEL(metadata_label), label_text);
		
	gtk_widget_show(metadata_dialog);

	/* user may have selected "Display Metadata" when the meta data
	   window was already opened -- bring it to the top */
	gtk_window_present(GTK_WINDOW(metadata_dialog));
    }
    else
    {
	sprintf(label_text, "Meta Data File Not Found: %s", metadata_filename);
	message_box(label_text);
    }

    g_free(label_text);
    g_free(metadata_filename);
}

void
metadata_hide()
{
    GtkWidget *metadata_dialog =
	glade_xml_get_widget(glade_xml, "metadata_dialog");
    
    gtk_widget_hide(metadata_dialog);
}


SIGNAL_CALLBACK void
on_metadata_dialog_ok_button_clicked(GtkWidget *widget)
{
    metadata_hide();
}

SIGNAL_CALLBACK gboolean
on_metadata_dialog_delete_event(GtkWidget *widget)
{
    metadata_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_metadata_dialog_destroy_event(GtkWidget *widget)
{
    metadata_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_metadata_dialog_destroy(GtkWidget *widget)
{
    metadata_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_metadata_dialog_key_press_event(GtkWidget * widget, 
			       GdkEventKey * event,
			       GtkWidget * win)
{
    if (event->keyval == GDK_Return)
    {
	metadata_hide();
	return TRUE;
    }

    return FALSE;
}
