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
#endif

#include "asf_convert_gui.h"

SIGNAL_CALLBACK void
on_browse_input_files_button_clicked(GtkWidget *widget)
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
    of.Flags = OFN_HIDEREADONLY | OFN_ALLOWMULTISELECT | OFN_EXPLORER;

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
	show_queued_thumbnails();

#else
    GtkWidget *file_selection_dialog =
        get_widget_checked("input_file_selection");

    gtk_widget_show(file_selection_dialog);
#endif
}

void
hide_input_file_selection_dialog()
{
    GtkWidget *file_selection_dialog =
        get_widget_checked("input_file_selection");

    gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_input_file_selection_cancel_button_clicked(GtkWidget *widget)
{
    hide_input_file_selection_dialog();
}

SIGNAL_CALLBACK gboolean
on_input_file_selection_delete_event(GtkWidget *w)
{
    hide_input_file_selection_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_input_file_selection_destroy_event(GtkWidget *w)
{
    hide_input_file_selection_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_input_file_selection_destroy(GtkWidget *w)
{
    hide_input_file_selection_dialog();
    return TRUE;
}

SIGNAL_CALLBACK void
on_input_file_selection_ok_button_clicked(GtkWidget *widget)
{
    GtkWidget *file_selection_dialog;
    gchar **selections;
    gchar **current;
    int i, n;

    file_selection_dialog =
        get_widget_checked("input_file_selection");

    selections = gtk_file_selection_get_selections(
        GTK_FILE_SELECTION(file_selection_dialog));

    current = selections;
    i = n = 0;

    while (*current)
    {
        /* second clause here allows silent fail for .L files, PR 92 */
        if (add_to_files_list(*current) || is_L_file(*current))
            ++i;

        ++current;
        ++n;
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

    g_strfreev(selections);
    gtk_widget_hide(file_selection_dialog);

	show_queued_thumbnails();
}
