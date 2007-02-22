#include "asf_convert_gui.h"
#include <gdk/gdkkeysyms.h>

/*
%I: Basename of the input file
*/

static const gchar token = '%';

static const gchar * default_prefix = "";
static const gchar * default_suffix = "";
static const gchar * default_scheme = "%I";

static const gchar * show_advanced = "Advanced >>";
static const gchar * hide_advanced = "Advanced <<";

NamingScheme * naming_scheme_new( const gchar * prefix,
                                 const gchar * suffix,
                                 const gchar * scheme)
{
    NamingScheme * ns = (NamingScheme *)
        g_malloc(sizeof(NamingScheme));

    ns->prefix = g_strdup(prefix);
    ns->suffix = g_strdup(suffix);

    if (scheme == NULL || strlen(scheme) == 0)
        ns->scheme = g_strdup(default_scheme);
    else
        ns->scheme = g_strdup(scheme);

    return ns;    
}

NamingScheme * naming_scheme_default( )
{
    return naming_scheme_new(
        default_prefix,
        default_suffix,
        default_scheme);
}

NamingScheme * naming_scheme_copy( const NamingScheme * ns )
{
    return naming_scheme_new(
        ns->prefix,
        ns->suffix,
        ns->scheme);
}

void naming_scheme_delete( NamingScheme * ns )
{
    g_free(ns->prefix);
    g_free(ns->suffix);
    g_free(ns->scheme);
    g_free(ns);
}

gchar * naming_scheme_apply( const NamingScheme * ns,
                            const gchar * basename )
{
    gchar work[1024];
    int i, j, len;

    if (!ns)
        return g_strdup(basename);

    assert(ns->prefix && ns->suffix && ns->scheme);

    strcpy(work, ns->prefix);
    j = strlen(ns->prefix);

    len = strlen(ns->scheme);
    for(i = 0; i < len; ++i)
    {
        char c = ns->scheme[i];
        if (c == token)
        {
            ++i;
            c = ns->scheme[i];

            switch (c)
            {
            case 'I':
                strcpy(work + j, basename);
                j += strlen(basename);
                break;

            default:
                /* Unknown substitution found... append as found */
                work[j] = token;
                work[j+1] = c;
                j += 2;
            }
        }
        else
        {
            work[j++] = c;
        }

        if (j + strlen(ns->suffix) > 1020 )
        {
            /* too long, truncate here */
            break;
        }
    }

    work[j] = '\0';
    strcat(work, ns->suffix);

    return g_strdup(work);
}

static void chop_ext(gchar * p)
{
    gchar * q = strrchr(p, '.');
    if (q)
        *q = '\0';
}

gchar * naming_scheme_apply_with_ext(const NamingScheme * ns,
                                     const gchar * basename,
                                     const gchar * ext)
{
    gchar * new_base = naming_scheme_apply(ns, basename);

    chop_ext(new_base);

    new_base = (gchar *) g_realloc(new_base,
        sizeof(gchar) * (strlen(new_base) + strlen(ext) + 2));

    strcat(new_base, ext);
    return new_base;
}

gboolean naming_schemes_equal( const NamingScheme * ns1,
                              const NamingScheme * ns2 )
{
    return
        strcmp(ns1->prefix, ns2->prefix) == 0 &&
        strcmp(ns1->suffix, ns2->suffix) == 0 &&
        strcmp(ns1->scheme, ns2->scheme) == 0;
}

static void set_vbox_advanced_visibility(const gboolean show_it)
{
    GtkWidget * dialog_cons_vbox_advanced;
    GtkWidget * dialog_cons_button_advanced;
    GtkWidget * dialog_cons;

    dialog_cons_vbox_advanced =
        get_widget_checked("dialog_cons_vbox_advanced");

    dialog_cons_button_advanced =
        get_widget_checked("dialog_cons_button_advanced");

    dialog_cons =
        get_widget_checked("dialog_cons");

    if (show_it)
    {
        gtk_widget_show(dialog_cons_vbox_advanced);

        gtk_button_set_label(
            GTK_BUTTON(dialog_cons_button_advanced),
            hide_advanced);
    }
    else
    {
        gtk_widget_hide(dialog_cons_vbox_advanced);

        gtk_button_set_label(
            GTK_BUTTON(dialog_cons_button_advanced),
            show_advanced);
    }

    /* not sure why the following is necessary ...
    if it is not here, multiple hides/shows by clicking
    "Advanced" would result in some of the informational
    label being overwritten with whitespace. */
    gtk_window_present(GTK_WINDOW(dialog_cons));
}

static gboolean advanced_is_shown()
{
    GtkWidget * dialog_cons_button_advanced;
    const gchar * current_text;

    dialog_cons_button_advanced =
        get_widget_checked("dialog_cons_button_advanced");

    current_text =
        gtk_button_get_label(GTK_BUTTON(dialog_cons_button_advanced));

    return strcmp(current_text, show_advanced) != 0;
}

static void toggle_vbox_advanced()
{
    set_vbox_advanced_visibility(!advanced_is_shown());
}

static void dialog_cons_hide()
{
    GtkWidget *dialog_cons;

    dialog_cons =
        get_widget_checked("dialog_cons");

    gtk_widget_hide(dialog_cons);
}

static void prepare_dialog_cons()
{
    GtkWidget * dialog_cons;

    GtkWidget * dialog_cons_prefix_entry;
    GtkWidget * dialog_cons_suffix_entry;
    GtkWidget * dialog_cons_scheme_entry;

    dialog_cons =
        get_widget_checked("dialog_cons");

    dialog_cons_prefix_entry =
        get_widget_checked("dialog_cons_prefix_entry");

    dialog_cons_suffix_entry =
        get_widget_checked("dialog_cons_suffix_entry");

    dialog_cons_scheme_entry =
        get_widget_checked("dialog_cons_scheme_entry");

    gtk_entry_set_text(GTK_ENTRY(dialog_cons_prefix_entry),
        current_naming_scheme->prefix);

    gtk_entry_set_text(GTK_ENTRY(dialog_cons_suffix_entry),
        current_naming_scheme->suffix);

    if (strcmp(current_naming_scheme->scheme, default_scheme) == 0)
    {
        set_vbox_advanced_visibility(FALSE);
        gtk_entry_set_text(GTK_ENTRY(dialog_cons_scheme_entry), "");
    }
    else
    {
        set_vbox_advanced_visibility(TRUE);
        gtk_entry_set_text(GTK_ENTRY(dialog_cons_scheme_entry),
            current_naming_scheme->scheme);
    }

    gtk_widget_show(dialog_cons);
}

static void apply_naming_scheme(const NamingScheme * new,
                                const NamingScheme * old)
{
    gboolean valid;
    GtkTreeIter iter;
    Settings * user_settings;
    gchar * ext;

    assert(list_store);

    user_settings = settings_get_from_gui();
    ext = (gchar *) settings_get_output_format_extension(user_settings);

    valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
    while (valid)
    {
        gchar * current_output_name;
        gchar * input_file_name;
        gchar * old_output_name;

        gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
            COL_DATA_FILE, &input_file_name,
            COL_OUTPUT_FILE, &current_output_name, -1);

        old_output_name =
            determine_default_output_file_name_schemed(input_file_name, old);

        if (strcmp(current_output_name, old_output_name) == 0)
        {
            /* Matches what was produced by the old naming scheme --
               generate new name based on the new naming scheme */
            gchar *new_output_name =
              determine_default_output_file_name_schemed(input_file_name, new);

            set_output_name(&iter, new_output_name);

            g_free(new_output_name);
        }
        else
        {
            /* user has customized this output name -- ignore */
        }

        g_free(input_file_name);
        g_free(current_output_name);
        g_free(old_output_name);

        valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
    }

    settings_delete(user_settings);
}

SIGNAL_CALLBACK void
on_button_change_output_naming_scheme_clicked(GtkWidget *widget)
{
    prepare_dialog_cons();
}

SIGNAL_CALLBACK gboolean
on_dialog_cons_destroy(GtkWidget *w)
{
    dialog_cons_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_dialog_cons_delete_event(GtkWidget *w)
{
    dialog_cons_hide();
    return TRUE;
}

SIGNAL_CALLBACK gboolean
on_dialog_cons_destroy_event(GtkWidget *w)
{
    dialog_cons_hide();
    return TRUE;
}

SIGNAL_CALLBACK void
on_dialog_cons_button_cancel_clicked(GtkWidget *widget)
{
    dialog_cons_hide();
}

static void dialog_cons_button_ok_clicked()
{
    GtkWidget * dialog_cons_prefix_entry;
    GtkWidget * dialog_cons_suffix_entry;
    GtkWidget * files_list;
    NamingScheme * old_naming_scheme;

    assert(current_naming_scheme);
    old_naming_scheme = naming_scheme_copy(current_naming_scheme);

    naming_scheme_delete(current_naming_scheme);
    current_naming_scheme = naming_scheme_default();

    dialog_cons_prefix_entry =
        get_widget_checked("dialog_cons_prefix_entry");

    dialog_cons_suffix_entry =
        get_widget_checked("dialog_cons_suffix_entry");

    current_naming_scheme->prefix = g_strdup( (gchar *)
        gtk_entry_get_text(GTK_ENTRY(dialog_cons_prefix_entry)));

    current_naming_scheme->suffix = g_strdup( (gchar *)
        gtk_entry_get_text(GTK_ENTRY(dialog_cons_suffix_entry)));

    if (advanced_is_shown())
    {
        GtkWidget * dialog_cons_scheme_entry;

        dialog_cons_scheme_entry =
            get_widget_checked("dialog_cons_scheme_entry");

        current_naming_scheme->scheme = g_strdup( (gchar *)
            gtk_entry_get_text(GTK_ENTRY(dialog_cons_scheme_entry)));
    }

    dialog_cons_hide();

    apply_naming_scheme(current_naming_scheme, old_naming_scheme);

    naming_scheme_delete(old_naming_scheme);

    /* refresh the grid */
    files_list = get_widget_checked("files_list");
    gtk_widget_queue_draw(files_list);
}

SIGNAL_CALLBACK void
on_dialog_cons_button_ok_clicked(GtkWidget *widget)
{
    dialog_cons_button_ok_clicked();
}

SIGNAL_CALLBACK void
on_dialog_cons_button_advanced_clicked(GtkWidget *widget)
{
    toggle_vbox_advanced();
}

SIGNAL_CALLBACK gboolean
on_dialog_cons_key_press_event(GtkWidget * widget, GdkEventKey * event,
                               GtkWidget * win)
{
    if (event->keyval == GDK_Return)
    {
        dialog_cons_button_ok_clicked();
        return TRUE;
    }

    return FALSE;
}

gchar *
determine_default_output_file_name_schemed(const gchar *data_file_name,
                                           const NamingScheme *scheme)
{
    Settings * user_settings;
    const gchar * ext;
    gchar * output_name_full;
    gchar * path;
    gchar * basename;
    gchar * filename;
    gchar * schemed_filename;
    gchar * p;

    int prepension = has_prepension(data_file_name);
    if (prepension > 0) {
        basename = g_path_get_basename(data_file_name);
        filename = g_strdup(basename + prepension);
        //printf("Filename: %s\n", filename);
    } else { 
        basename = g_strdup(data_file_name);
        p = findExt(basename);
        if (p)
            *p = '\0';

        filename = g_path_get_basename(basename);
    }

    schemed_filename = naming_scheme_apply(scheme, filename);

    if (output_directory && strlen(output_directory) > 0)
    {
        path = g_strdup(output_directory);
    }
    else
    {
        gchar * tmp = g_path_get_dirname(data_file_name);
        path = g_malloc( sizeof(gchar) * (strlen(tmp) + 4) );
        g_sprintf(path, "%s%c", tmp, DIR_SEPARATOR);
        g_free(tmp);
    }

    basename = (gchar *) 
        g_realloc(basename,
        sizeof(gchar) * (strlen(path) + strlen(schemed_filename) + 2));

    sprintf(basename, "%s%s", path, schemed_filename);

    g_free(schemed_filename);
    g_free(filename);
    g_free(path);

    user_settings = settings_get_from_gui();
    ext = settings_get_output_format_extension(user_settings);

    output_name_full = 
        (gchar *) g_malloc(sizeof(gchar) * 
        (strlen(basename) + strlen(ext) + 10));

    g_sprintf(output_name_full, "%s.%s", basename, ext);

    // CEOS Level 0 uses RAW and raw as default extensions...
    // so we have this kludge to avoid constant Errors due to the same
    // input and output filename.
    if (strcmp_case(output_name_full, data_file_name) == 0)
        g_sprintf(output_name_full, "%s_out.%s", basename, ext);

    g_free(basename);
    settings_delete(user_settings);

    return output_name_full;
}
