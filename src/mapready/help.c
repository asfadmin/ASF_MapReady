#include <unistd.h>
#include "mapready.h"
#include "asf_version.h"
#include "asf.h"
#include <gdk/gdkkeysyms.h>

#ifdef win32
#include <windows.h>
#endif

static const int max_line_len = 2048;

// defined in src/asf/share.c
long get_string_from_registry_ex(const char *folder, const char * key, char * str_value);
//const char * get_asf_bin_dir();

char * escapify(const char * s)
{
    int i,j;
    char * ret = MALLOC(2*strlen(s)*sizeof(char));
    for (i = 0, j = 0; i <= strlen(s); ++i)
    {
        switch(s[i])
        {
            case '\\':
                ret[j] = ret[j+1] = s[i];
                ++j;
                break;
            default:
                ret[j] = s[i];
                break;
        }
        ++j;
    }

    return ret;
}

SIGNAL_CALLBACK void
on_help_button_clicked(GtkWidget *widget)
{
#ifdef win32
    char pdf_dir[1024], pdf_file[128], pdf_viewer[1024];
    snprintf(pdf_dir, 1023, "%s/doc/", get_asf_share_dir());
    strcpy(pdf_file, "mapready_manual.pdf");
    //printf("pdf: %s/%s\n", pdf_dir, pdf_file);

    FindExecutable((LPCTSTR)pdf_file, (LPCTSTR)pdf_dir, (LPTSTR)pdf_viewer);
    printf("Found PDF Viewer: %s\n", pdf_viewer);

    if (strlen(pdf_viewer)) {
      asfSystem_NoWait("\"%s\" \"%s/%s\"", pdf_viewer, pdf_dir, pdf_file);
    } else {
      message_box("Couldn't find path to a PDF Viewer!");
    }
#else
    GtkWidget *help_dialog;
    GtkWidget *help_text;
    GtkTextBuffer * text_buffer;
    FILE * help_file;
    gchar * help_filename;

    help_dialog =
        get_widget_checked("help_dialog");

    help_text =
        get_widget_checked("help_text");

    text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text));

    gtk_text_buffer_set_text(text_buffer, "", -1);

    help_filename = "mapready.txt";
    //help_file = fopen(help_filename, "rt");
    help_file = fopen_share_file(help_filename, "rt");
    if (help_file)
    {
        int line_count = 0;
        gchar * buffer = (gchar *) g_malloc(sizeof(gchar) * max_line_len);
        while (!feof(help_file))
        {
            gchar *p = fgets(buffer, max_line_len, help_file);
            if (p)
            {
                if (strlen(p)) line_count++;
                GtkTextIter end;
                gchar * q = strstr(buffer, "$VERSION");

                if (q)
                {
                    gchar * r = g_strdup(q + 8); /* 8 = length of '$VERSION' */

                    strcpy(q, MAPREADY_VERSION_STRING);
                    strcat(buffer, r);
                    g_free(r);
                }

                gtk_text_buffer_get_end_iter(text_buffer, &end);
                gtk_text_buffer_insert(text_buffer, &end, buffer, -1);
            }
        }
        if (!line_count) {
            sprintf(buffer, "\n\n  ERROR: Empty help file (mapready.txt) in share folder\n(%s)\n",
                    get_asf_share_dir());
            GtkTextIter end;
            gtk_text_buffer_get_end_iter(text_buffer, &end);
            gtk_text_buffer_insert(text_buffer, &end, buffer, -1);
        }

        fclose(help_file);
        g_free(buffer);
    }
    else {
        // No help file found
        gchar *buffer = (gchar *) g_malloc(sizeof(gchar) * max_line_len);
        strcpy(buffer, "\n\n  ERROR: Cannot find help file (mapready.txt) in share folder.\n");
        GtkTextIter end;
        gtk_text_buffer_get_end_iter(text_buffer, &end);
        gtk_text_buffer_insert(text_buffer, &end, buffer, -1);
        g_free(buffer);
    }

    gtk_widget_show(help_dialog);
#endif
}

void
help_hide()
{
    GtkWidget *help_dialog =
        get_widget_checked("help_dialog");

    gtk_widget_hide(help_dialog);
}


SIGNAL_CALLBACK void
on_help_dialog_ok_button_clicked(GtkWidget *widget)
{
    help_hide();
}

SIGNAL_CALLBACK gboolean
on_help_dialog_delete_event(GtkWidget *widget)
{
    help_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_help_dialog_destroy_event(GtkWidget *widget)
{
    help_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_help_dialog_destroy(GtkWidget *widget)
{
    help_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_help_dialog_key_press_event(GtkWidget * widget,
                               GdkEventKey * event,
                               GtkWidget * win)
{
    if (event->keyval == GDK_Return)
    {
        help_hide();
        return TRUE;
    }

    return FALSE;
}
