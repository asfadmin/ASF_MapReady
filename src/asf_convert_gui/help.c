#include <unistd.h>
#include "asf_convert_gui.h"
#include "asf_version.h"
#include "asf.h"
#include <gdk/gdkkeysyms.h>

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
    if (fork() == 0)
    {
        char hh[1024];
        int ret;

        // First method, get location of help viewer from the standard registry location
        get_string_from_registry_ex("SOFTWARE\\Classes\\chm.file\\shell\\open\\command", "", hh);

        if (strlen(hh) > 0) {
            char *p = strstr(hh, "%1");
            if (p) *p = '\0';

            char * escaped_share_dir = escapify(get_asf_share_dir());
            strcat(hh, escaped_share_dir);
            strcat(hh, "/asf_convert_gui.chm");
            FREE(escaped_share_dir);
            ret = do_system_exec(hh);
            if (ret != -1) exit(0);
        }

        // Failed to find location of hh.exe through registry... try the system directory
        char *sr = getenv("SYSTEMROOT");
        if (strlen(sr) > 0) {
            int i, j = 0;
            for (i = 0; i < strlen(sr); ++i) {
                switch(sr[i]) {
                    case '\\': hh[j] = '\\'; hh[j+1] = '\\'; ++j; break;
                    default:   hh[j] = sr[i]; break;
                }
                ++j;
            }
            hh[j] = '\0';
            strcat(hh, "/hh.exe asf_convert_gui.chm");
            ret = system(hh);
            if (ret != -1) exit(0);
        }

        // Failed, give up.
        message_box("Couldn't find the help viewer!");
        exit(0);
    }
#else
    GtkWidget *help_dialog;
    GtkWidget *help_text;
    GtkTextBuffer * text_buffer;
    FILE * help_file;
    gchar * help_filename;

    help_dialog =
        glade_xml_get_widget(glade_xml, "help_dialog");

    help_text =
        glade_xml_get_widget(glade_xml, "help_text");

    text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(help_text));

    gtk_text_buffer_set_text(text_buffer, "", -1);

    help_filename = "asf_convert_gui.txt";
    //help_file = fopen(help_filename, "rt");
    help_file = fopen_share_file(help_filename, "rt");
    if (help_file)
    {
        gchar * buffer = (gchar *) g_malloc(sizeof(gchar) * max_line_len);
        while (!feof(help_file))
        {
            gchar *p = fgets(buffer, max_line_len, help_file);
            if (p)
            {
                GtkTextIter end;
                gchar * q = strstr(buffer, "$VERSION");

                if (q)
                {
                    gchar * r = g_strdup(q + 8); /* 8 = length of '$VERSION' */

                    strcpy(q, CONVERT_PACKAGE_VERSION_STRING);
                    strcat(buffer, r);
                    g_free(r);
                }

                gtk_text_buffer_get_end_iter(text_buffer, &end);
                gtk_text_buffer_insert(text_buffer, &end, buffer, -1);
            }
        }

        fclose(help_file);
        g_free(buffer);
    }

    gtk_widget_show(help_dialog);
#endif
}

void
help_hide()
{
    GtkWidget *help_dialog =
        glade_xml_get_widget(glade_xml, "help_dialog");

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
