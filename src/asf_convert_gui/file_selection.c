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
on_add_button_clicked(GtkWidget *widget)
{
    GtkWidget *input_entry;
    G_CONST_RETURN gchar *in_data;
    gboolean result;

    input_entry = 
        glade_xml_get_widget(glade_xml, "input_entry");

    in_data =
        gtk_entry_get_text(GTK_ENTRY(input_entry));

    if (strlen(in_data) == 0)
    {
        message_box("Enter the name of a data file to add to the list.");
        return;
    }

    if (!g_file_test(in_data, G_FILE_TEST_EXISTS))
    {
        gchar * message =
            (gchar *) g_malloc(sizeof(gchar) * (strlen(in_data) + 256));

        g_sprintf(message, "Error: Couldn't find the file \"%s\".", in_data);
        message_box(message);
        return;
    }

    /* add file to the list */
    result = add_to_files_list(in_data);

    if (!result)
    {
        gchar * ext;
        gchar * message =
            (gchar *) g_malloc(sizeof(gchar) * (strlen(in_data) + 256));

        ext = strrchr(in_data, '.');
        if (!ext)
        {
            message_box("Error: Unknown file type, did not have extension.");
        }
        else
        {
            ++ext;
            g_sprintf(message, "Error: Unknown file type: %s!", ext);
            message_box(message);
        }
    }
}

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
            printf("Adding: %s\n", dir_and_file);
            add_to_files_list(dir_and_file);
            p += strlen(p) + 1;
            free(dir_and_file);
        }
    } else {
        add_to_files_list(dir);
    }

    free(dir);

#else
    GtkWidget *file_selection_dialog =
        glade_xml_get_widget(glade_xml, "input_file_selection");

    gtk_widget_show(file_selection_dialog);
#endif
}

void
hide_input_file_selection_dialog()
{
    GtkWidget *file_selection_dialog =
        glade_xml_get_widget(glade_xml, "input_file_selection");

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
        glade_xml_get_widget(glade_xml, "input_file_selection");

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
}

void
hide_input_file_chooser_dialog()
{
    GtkWidget *file_selection_dialog =
        glade_xml_get_widget(glade_xml, "input_file_chooser");

    gtk_widget_hide(file_selection_dialog);
}

SIGNAL_CALLBACK void
on_input_file_chooser_cancel_button_clicked(GtkWidget *widget)
{
    hide_input_file_chooser_dialog();
}

SIGNAL_CALLBACK gboolean
on_input_file_chooser_delete_event(GtkWidget *w)
{
    hide_input_file_chooser_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_input_file_chooser_destroy_event(GtkWidget *w)
{
    hide_input_file_chooser_dialog();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_input_file_chooser_destroy(GtkWidget *w)
{
    hide_input_file_chooser_dialog();
    return TRUE;
}

SIGNAL_CALLBACK void
on_input_file_chooser_ok_button_clicked(GtkWidget *widget)
{
    GtkWidget *file_chooser_dialog;
    GSList *selections;
    GSList *current;
    int i, n;

    file_chooser_dialog =
        glade_xml_get_widget(glade_xml, "input_file_chooser");

    selections =
        gtk_file_chooser_get_filenames(GTK_FILE_CHOOSER(file_chooser_dialog));

    current = selections;
    i = n = 0;

    while (current)
    {
        gchar * file = (gchar *) current->data;

        /* second clause here allows silent fail for .L files, PR 92 */
        if (add_to_files_list(file) || is_L_file(file))
            ++i;

        current = g_slist_next(current);
        ++n;

        g_free(file);
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

    g_slist_free(selections);
    gtk_widget_hide(file_chooser_dialog);
}
