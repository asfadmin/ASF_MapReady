#undef win32
#ifdef win32

/* ugly hack here... windef.h and asf_meta.h both define a BYTE symbol. */
/* since we don't use the BYTE from asf_meta.h here, we'll #define BYTE */
/* to something else during the processing of that header, leaving BYTE */
/* defined in windef.h alone (that's the one we want)                   */

#define BYTE __byte
#include "asf.h"
#undef BYTE
#include <windows.h>
#undef DIR_SEPARATOR
#endif // #ifdef win32

#include "ait.h"

typedef struct CallbackAndWidget
{
    browse_callback *bcb;
    GtkFileSelection *filew;
} callback_and_widget_t;

static void file_ok_sel(GtkWidget *w, callback_and_widget_t *s)
{
    GtkFileSelection *fs = s->filew;
    const char *str =
        gtk_file_selection_get_filename (GTK_FILE_SELECTION (fs));

    // make a copy of the pointer to static memory
    char *selected_file = MALLOC(sizeof(char)*(strlen(str)+2));
    strcpy(selected_file, str);
    printf ("Selected: %s\n", selected_file);

    s->bcb(selected_file);

    gtk_widget_destroy(GTK_WIDGET(fs));
    free(selected_file);
    free(s);
}

static void file_cancel_sel(GtkWidget *w, callback_and_widget_t *s)
{
    if (s) {
        gtk_widget_destroy(GTK_WIDGET(s->filew));
        free(s);
    }
}

void browse(browse_callback bcb)
{
#ifdef win32
    OPENFILENAME of;
    int retval;
    char fname[1024];

    fname[0] = '\0';

    memset(&of, 0, sizeof(of));

#ifdef OPENFILENAME_SIZE_VERSION_400
    of.lStructSize = OPENFILENAME_SIZE_VERSION_400;
#else
    of.lStructSize = sizeof(of);
#endif

    of.hwndOwner = NULL;
    of.lpstrFilter = "CEOS Level 1 Data Files (*.D)\0*.D\0"
        "CEOS Level 0 Data Files (*.raw)\0*.raw\0"
        "STF Files (*.000)\0*.000\0"
        "Complex Files (*.cpx)\0*.cpx\0"
        "ALOS Files (LED-*)\0LED-*\0"
        "All Files\0*\0";
    of.lpstrCustomFilter = NULL;
    of.nFilterIndex = 1;
    of.lpstrFile = fname;
    of.nMaxFile = sizeof(fname);
    of.lpstrFileTitle = NULL;
    of.lpstrInitialDir = ".";
    of.lpstrTitle = "Select File";
    of.lpstrDefExt = NULL;
    of.Flags = OFN_HIDEREADONLY | OFN_EXPLORER;

    retval = GetOpenFileName(&of);

    if (!retval) {
        if (CommDlgExtendedError())
            message_box("File dialog box error");
        return;
    }

    /* the returned "fname" has the following form:            */
    /*   <directory>\0<first file>\0<second file>\0<third ...  */ 
    char * dir = strdup(fname);
    char * p = fname + strlen(dir) + 1;

    if (*p) { 
        while (*p) {
            char * dir_and_file = malloc(sizeof(char)*(strlen(dir)+strlen(p)+5));
            sprintf(dir_and_file, "%s%c%s", dir, DIR_SEPARATOR, p);
            add_to_files_list(dir_and_file);
            p += strlen(p) + 1;
            free(dir_and_file);
        }
    } else {
        add_to_files_list(dir);
    }

    free(dir);

#else
    GtkWidget *filew = gtk_file_selection_new("Select File");
    gtk_window_set_modal(GTK_WINDOW(filew), TRUE);

    callback_and_widget_t *s = MALLOC(sizeof(callback_and_widget_t));
    s->filew = GTK_FILE_SELECTION(filew);
    s->bcb = bcb;

    g_signal_connect(G_OBJECT(filew), "destroy", 
        G_CALLBACK(file_cancel_sel), NULL);
    g_signal_connect(G_OBJECT(GTK_FILE_SELECTION(filew)->ok_button),
        "clicked", G_CALLBACK(file_ok_sel), (gpointer)s);
    g_signal_connect_swapped(
        G_OBJECT(GTK_FILE_SELECTION(filew)->cancel_button),
        "clicked", G_CALLBACK(file_cancel_sel), (gpointer)s);

    gtk_widget_show(filew);
#endif
}
