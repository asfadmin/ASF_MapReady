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
            glade_xml_get_widget(glade_xml, "dialog_cons_vbox_advanced");

    dialog_cons_button_advanced =
            glade_xml_get_widget(glade_xml, "dialog_cons_button_advanced");

    dialog_cons =
            glade_xml_get_widget(glade_xml, "dialog_cons");
            
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
            glade_xml_get_widget(glade_xml, "dialog_cons_button_advanced");

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
            glade_xml_get_widget(glade_xml, "dialog_cons");
    
    gtk_widget_hide(dialog_cons);
}

static void prepare_dialog_cons()
{
    GtkWidget * dialog_cons;
    
    GtkWidget * dialog_cons_prefix_entry;
    GtkWidget * dialog_cons_suffix_entry;
    GtkWidget * dialog_cons_scheme_entry;

    dialog_cons =
            glade_xml_get_widget(glade_xml, "dialog_cons");
    
    dialog_cons_prefix_entry =
            glade_xml_get_widget(glade_xml, "dialog_cons_prefix_entry");

    dialog_cons_suffix_entry =
            glade_xml_get_widget(glade_xml, "dialog_cons_suffix_entry");

    dialog_cons_scheme_entry =
            glade_xml_get_widget(glade_xml, "dialog_cons_scheme_entry");
    
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

static void apply_naming_scheme(const NamingScheme * new, const NamingScheme * old)
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
        gchar * current_output_basename;
        gchar * old_output_basename;
        gchar * input_file_name;
        gchar * input_basename;
        gchar * path;
                
        gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
                            0, &input_file_name,
                            1, &current_output_name, -1);

        path = g_path_get_dirname(input_file_name);
        
        input_basename = g_path_get_basename(input_file_name);
        chop_ext(input_basename);
        
        current_output_basename = g_path_get_basename(current_output_name);
        chop_ext(current_output_basename);
        
        old_output_basename = naming_scheme_apply(old, input_basename);

        if (strcmp(current_output_basename, old_output_basename) == 0)
        {
            /* current name is using default scheme - apply */
            gchar * new_output_name;
            gchar * new_output_basename;
            
            new_output_basename =
                   naming_scheme_apply(new, input_basename);

            /* put the path & extension back on */
            new_output_name = (gchar *) g_malloc( sizeof(gchar) *
                                (strlen(new_output_basename) +
                                 strlen(path) +
                                 strlen(ext) + 4) );

            sprintf(new_output_name, "%s%c%s.%s", path, DIR_SEPARATOR,
                    new_output_basename, ext);

            set_output_name(&iter, new_output_name);

            g_free(new_output_name);
            g_free(new_output_basename);
        }
        else
        {
            /* user has customized this output name -- ignore */
        }

        g_free(old_output_basename);
        g_free(current_output_basename);
        g_free(input_basename);
        g_free(path);
        g_free(input_file_name);
        g_free(current_output_name);

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
    NamingScheme * old_naming_scheme;
    
    assert(current_naming_scheme);
    old_naming_scheme = naming_scheme_copy(current_naming_scheme);

    naming_scheme_delete(current_naming_scheme);
    current_naming_scheme = naming_scheme_default();

    dialog_cons_prefix_entry =
            glade_xml_get_widget(glade_xml, "dialog_cons_prefix_entry");

    dialog_cons_suffix_entry =
            glade_xml_get_widget(glade_xml, "dialog_cons_suffix_entry");

    current_naming_scheme->prefix = g_strdup( (gchar *)
            gtk_entry_get_text(GTK_ENTRY(dialog_cons_prefix_entry)));

    current_naming_scheme->suffix = g_strdup( (gchar *)
            gtk_entry_get_text(GTK_ENTRY(dialog_cons_suffix_entry)));
        
    if (advanced_is_shown())
    {
        GtkWidget * dialog_cons_scheme_entry;
    
        dialog_cons_scheme_entry =
                glade_xml_get_widget(glade_xml, "dialog_cons_scheme_entry");

        current_naming_scheme->scheme = g_strdup( (gchar *)
                gtk_entry_get_text(GTK_ENTRY(dialog_cons_scheme_entry)));
    }

    dialog_cons_hide();

    apply_naming_scheme(current_naming_scheme, old_naming_scheme);

    naming_scheme_delete(old_naming_scheme);
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
