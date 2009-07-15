#include "asf_convert_gui.h"

static void asf_view_thread (GString *file, gpointer user_data)
{
#ifdef win32
    gchar * asf_view = find_in_bin("asf_view.exe");
#else
    gchar * asf_view = find_in_bin("asf_view");
#endif

    char buf[1024];
    char *escaped_str = escapify(file->str);
    snprintf(buf, sizeof(buf), "\"%s\" %s", asf_view, escaped_str);
    free(escaped_str);
    asfSystem(buf);
    g_string_free(file, TRUE);
}

void show_image_with_asf_view(gchar * in_name)
{
  show_image_with_asf_view_arg(in_name, "");
}

void show_image_with_asf_view_arg(gchar * in_name, gchar *arg)
{
#ifdef win32
    gchar * asf_view = find_in_bin("asf_view.exe");
#else
    gchar * asf_view = find_in_bin("asf_view");
#endif

    if (asf_view)
    {
        static GThreadPool *ttp = NULL;
        GError *err = NULL;

        if (!ttp)
        {
            if (!g_thread_supported ()) g_thread_init (NULL);
            ttp = g_thread_pool_new ((GFunc) asf_view_thread, NULL, 4, TRUE, &err);
            g_assert(!err);
        }

        char *cl = MALLOC(sizeof(char)*(strlen(in_name)+strlen(arg)+10));

        if (strlen(arg) > 0)
          sprintf(cl, "%s \"%s\"", arg, in_name);
        else
          sprintf(cl, "\"%s\"", in_name);

        g_thread_pool_push (ttp, g_string_new (cl), &err);

        free(cl);
        g_assert(!err);
    }
    else
    {
        message_box("Failed to open external viewer!");
    }
}

