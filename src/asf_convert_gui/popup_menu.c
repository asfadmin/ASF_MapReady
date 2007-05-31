#include <unistd.h>

#ifdef win32
#define BYTE __byte
#include "asf.h"
#undef BYTE
#include <windows.h>
#include <shellapi.h>
#endif

#include "asf_convert_gui.h"
#include <asf.h>
#include <asf_meta.h>

#define POINT __tmp_point
#include <asf_vector.h>
#undef POINT

static const int popup_menu_item_remove = 0;
static const int popup_menu_item_process = 1;
static const int popup_menu_item_reprocess = 1;
static const int popup_menu_item_display_ceos_metadata = 4;
static const int popup_menu_item_google_earth = 5;

/* danger: returns pointer to static data!! */
static const char * imgloc(char * file)
{
    static char loc[1024];
    gchar * tmp = find_in_share(file);
    if (tmp) {
      strcpy(loc, tmp);
      g_free(tmp);
    } else {
      strcpy(loc, file);
    }

    return loc;
}

void set_toolbar_images()
{
    GtkWidget * w = get_widget_checked("google_earth_toolbar_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("earth2.gif"));

    w = get_widget_checked("completed_files_google_earth_toolbar_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("earth2.gif"));

    w = get_widget_checked("ceos_metadata_toolbar_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("information_icon.gif"));

    w = get_widget_checked("asf_metadata_toolbar_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("information_icon.gif"));
}

void
show_please_select_message()
{
    static const char *msg = "Please select a file first!\n";
    message_box(msg);
}

static void
enable_menu_items(GtkMenu * menu, gboolean enable_display_ceos_metadata)
{
    GList * children;
    GList * iter;
    int n = 0;

    children = gtk_container_get_children(GTK_CONTAINER(menu));
    iter = children;

    while (iter)
    {
        gboolean enable = TRUE;
        GtkWidget * item = GTK_WIDGET(iter->data);

        if (n == popup_menu_item_display_ceos_metadata &&
            (!enable_display_ceos_metadata || !use_thumbnails))
        {
            enable = FALSE;
        }      

        gtk_widget_set_sensitive(item, enable);

        iter = g_list_next(iter);
        ++n;
    }

    g_list_free(children);
}

/* static */ void
enable_toolbar_buttons(gboolean enable_view_output,
		       gboolean enable_display_ceos_metadata,
		       gboolean enable_display_asf_metadata)
{
    GtkWidget *rename_button;
    GtkWidget *view_log_button;
    GtkWidget *view_log_button2;
    GtkWidget *display_ceos_button;
    GtkWidget *display_asf_button;
    GtkWidget *view_output_button;
    GtkWidget *google_earth_button;
    GtkWidget *google_earth_button2;

    rename_button = get_widget_checked("rename_button");
    view_log_button = get_widget_checked("view_log_button");
    view_log_button2 = get_widget_checked("view_log_button2");
    display_ceos_button = get_widget_checked("display_ceos_button");
    display_asf_button = get_widget_checked("display_asf_button");
    view_output_button = get_widget_checked("view_output_button");
    google_earth_button = get_widget_checked("google_earth_button");
    google_earth_button2 =
      get_widget_checked("completed_files_google_earth_button");

    gtk_widget_set_sensitive(rename_button, TRUE);
    gtk_widget_set_sensitive(view_log_button, TRUE);
    gtk_widget_set_sensitive(view_log_button2, TRUE);
    gtk_widget_set_sensitive(view_output_button, enable_view_output);
    gtk_widget_set_sensitive(display_asf_button, enable_display_asf_metadata);
    gtk_widget_set_sensitive(display_ceos_button, enable_display_ceos_metadata);
    gtk_widget_set_sensitive(google_earth_button, TRUE);
    gtk_widget_set_sensitive(google_earth_button2, TRUE);
}

static void
disable_popups_for_multiple_selected(GtkMenu *menu)
{
    GList * children;
    GList * iter;
    int n = 0;

    children = gtk_container_get_children(GTK_CONTAINER(menu));
    iter = children;

    while (iter)
    {
        gboolean enable;
        GtkMenuItem * item = GTK_MENU_ITEM(iter->data);

        enable = n == popup_menu_item_remove || n == popup_menu_item_process ||
                 n == popup_menu_item_google_earth;
        gtk_widget_set_sensitive(GTK_WIDGET(item), enable);

        ++n;
        iter = g_list_next(iter);
    }

    g_list_free(children);
}

static void
disable_popups_for_multiple_selected2(GtkMenu *menu)
{
    GList * children;
    GList * iter;
    int n = 0;

    children = gtk_container_get_children(GTK_CONTAINER(menu));
    iter = children;

    while (iter)
    {
        gboolean enable;
        GtkMenuItem * item = GTK_MENU_ITEM(iter->data);

        enable = n == popup_menu_item_reprocess || popup_menu_item_remove;
        gtk_widget_set_sensitive(GTK_WIDGET(item), enable);

        ++n;
        iter = g_list_next(iter);
    }

    g_list_free(children);
}

/* static */ void
disable_toolbar_buttons_for_multiple_selected()
{
    GtkWidget *rename_button;
    GtkWidget *view_log_button;
    GtkWidget *view_log_button2;
    GtkWidget *display_ceos_button;
    GtkWidget *display_asf_button;
    GtkWidget *view_output_button;

    rename_button = get_widget_checked("rename_button");
    view_log_button = get_widget_checked("view_log_button");
    view_log_button2 = get_widget_checked("view_log_button2");
    display_ceos_button = get_widget_checked("display_ceos_button");
    display_asf_button = get_widget_checked("display_asf_button");
    view_output_button = get_widget_checked("view_output_button");

    gtk_widget_set_sensitive(rename_button, FALSE);
    gtk_widget_set_sensitive(view_log_button, FALSE);
    gtk_widget_set_sensitive(view_log_button2, FALSE);
    gtk_widget_set_sensitive(display_asf_button, FALSE);
    gtk_widget_set_sensitive(display_ceos_button, FALSE);
    gtk_widget_set_sensitive(view_output_button, FALSE);
}

static void
disable_for_multiple_selected(GtkMenu * menu)
{
    disable_popups_for_multiple_selected(menu);
    // disable_toolbar_buttons_for_multiple_selected();
}

static void
disable_for_multiple_selected2(GtkMenu * menu)
{
    disable_popups_for_multiple_selected2(menu);
}

gint
files_popup_handler(GtkWidget *widget, GdkEvent *event)
{
    GtkMenu *menu;
    GdkEventButton *event_button;
    GtkTreeSelection *selection;
    GtkWidget *files_list;
    GtkTreeIter iter;

    g_return_val_if_fail(widget != NULL, FALSE);
    g_return_val_if_fail(GTK_IS_MENU(widget), FALSE);
    g_return_val_if_fail(event != NULL, FALSE);

    files_list = get_widget_checked("files_list");
    event_button = (GdkEventButton *) event;
    menu = GTK_MENU(widget);

    if (event->type == GDK_BUTTON_PRESS && event_button->button == 3)
    {
        int num_selected;

        /* if an item is not selected in the file grid,
        select what was clicked on */

        selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list));

        num_selected = gtk_tree_selection_count_selected_rows(selection);
        if (num_selected <= 1)
        {
            GtkTreePath *path;

            if (gtk_tree_view_get_path_at_pos(GTK_TREE_VIEW(files_list),
                event_button->x, event_button->y,
                &path, NULL, NULL, NULL))
            {
                gchar *status, *out_name, *in_name, *ceos_meta_name;

                gtk_tree_selection_unselect_all(selection);
                gtk_tree_selection_select_path(selection, path);
                gtk_tree_model_get_iter(GTK_TREE_MODEL(list_store),
                    &iter, path);

                gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter, 
                    COL_STATUS, &status,
		    COL_DATA_FILE, &in_name,
                    COL_OUTPUT_FILE, &out_name, -1);

                gtk_tree_path_free(path);

                if (strstr(status, "...") != NULL)
                {
                    /* right-clicked on what is currently being processed */
                    return FALSE;
                }

                /* check if we should disable "Display CEOS Metadata" */
                ceos_meta_name = meta_file_name(in_name);
                gboolean show_display_ceos_metadata_menu_item =
                    g_file_test(ceos_meta_name, G_FILE_TEST_EXISTS);
                g_free(ceos_meta_name);

                /* enable/disable the items */
                enable_menu_items(menu, show_display_ceos_metadata_menu_item);
            }
            else
            {
                /* nothing selected & nothing was under mouse when clicked */
                return FALSE;
            }
        }
        else
        {
            /* disable some of the items that are appropriate for 1 only */
            disable_for_multiple_selected(menu);
        }

        gtk_menu_popup(menu, NULL, NULL, NULL, NULL,
            event_button->button, event_button->time);
        return TRUE;
    }
    return FALSE;
}

gint
completed_files_popup_handler(GtkWidget *widget, GdkEvent *event)
{
    GtkMenu *menu;
    GdkEventButton *event_button;
    GtkTreeSelection *selection;
    GtkWidget *completed_files_list;

    g_return_val_if_fail(widget != NULL, FALSE);
    g_return_val_if_fail(GTK_IS_MENU(widget), FALSE);
    g_return_val_if_fail(event != NULL, FALSE);

    completed_files_list = get_widget_checked("completed_files_list");
    event_button = (GdkEventButton *) event;
    menu = GTK_MENU(widget);

    if (event->type == GDK_BUTTON_PRESS && event_button->button == 3)
    {
        int num_selected;

        /* if an item is not selected in the file grid,
        select what was clicked on */

        selection = gtk_tree_view_get_selection(
            GTK_TREE_VIEW(completed_files_list));

        num_selected = gtk_tree_selection_count_selected_rows(selection);
        if (num_selected <= 1)
        {
            GtkTreePath *path;

            if (gtk_tree_view_get_path_at_pos(
                    GTK_TREE_VIEW(completed_files_list),
                event_button->x, event_button->y,
                &path, NULL, NULL, NULL))
            {
                // normal case -- don't need to do anything
            }
            else
            {
                /* nothing selected & nothing was under mouse when clicked */
                return FALSE;
            }
        }
        else
        {
            /* disable some of the items that are appropriate for 1 only */
            disable_for_multiple_selected2(menu);
        }

        gtk_menu_popup(menu, NULL, NULL, NULL, NULL,
            event_button->button, event_button->time);
        return TRUE;
    }
    return FALSE;
}

static gboolean confirm_overwrite()
{
    gboolean ret = TRUE;

    GtkWidget * dialog_confirm_overwrite;
    gint result;

    dialog_confirm_overwrite =
        get_widget_checked("dialog_confirm_overwrite");

    result = gtk_dialog_run( GTK_DIALOG(dialog_confirm_overwrite) );
    gtk_widget_hide( dialog_confirm_overwrite );

    switch (result)
    {
    case GTK_RESPONSE_OK:
        break;
    default:
        ret = FALSE;
        break;
    }

    return ret;
}

static int
handle_remove_imp(const char *widget_name, GtkListStore *store)
{
    GtkWidget *files_list;
    GtkTreeModel * model;
    GtkTreeSelection *selection;
    GList * selected_rows, * i;
    GList * refs;

    files_list = get_widget_checked(widget_name);
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list));
    model = GTK_TREE_MODEL(store);

    selected_rows = gtk_tree_selection_get_selected_rows(
        selection, &model);

    refs = NULL;
    i = selected_rows;

    if (!selected_rows)
    {
        show_please_select_message();
        return FALSE;
    }

    while (i)
    {
        GtkTreePath * path;
        GtkTreeRowReference * ref;

        path = (GtkTreePath *) i->data;
        ref = gtk_tree_row_reference_new(model, path);

        refs = g_list_append(refs, ref);

        i = g_list_next(i);
    }

    i = refs;

    while (i)
    {
        GtkTreePath * path;
        GtkTreeIter iter;
        GtkTreeRowReference * ref;

        ref = (GtkTreeRowReference *) i->data;
        path = gtk_tree_row_reference_get_path(ref);
        gtk_tree_model_get_iter(model, &iter, path);
        gtk_list_store_remove(GTK_LIST_STORE(model), &iter);

        i = g_list_next(i);
    }

    g_list_foreach(selected_rows, (GFunc)gtk_tree_path_free, NULL);
    g_list_free(selected_rows);

    g_list_foreach(refs, (GFunc)gtk_tree_row_reference_free, NULL);
    g_list_free(refs);

    return TRUE;
}

static int
handle_remove_completed()
{
    return handle_remove_imp("completed_files_list", completed_list_store);
}

static int
handle_remove()
{
    return handle_remove_imp("files_list", list_store);
}

gboolean
get_iter_to_first_selected_row(GtkWidget * files_list, GtkTreeIter * iter)
{
    GList * selected_rows;
    GtkTreeModel *model;
    GtkTreeSelection *selection;
    gboolean found;

    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list));
    model = GTK_TREE_MODEL(list_store);

    selected_rows = gtk_tree_selection_get_selected_rows(selection, &model);
    if (selected_rows)
    {
        GtkTreePath * path;

        path = (GtkTreePath *) selected_rows->data;
        gtk_tree_model_get_iter(model, iter, path);

        g_list_foreach(selected_rows, (GFunc)gtk_tree_path_free, NULL);
        g_list_free(selected_rows);

        found = TRUE;
    }
    else
    {
        found = FALSE;
    }

    return found;
}

static int
handle_display_ceos_metadata()
{
    GtkWidget *files_list;
    GtkTreeIter iter;

    files_list = get_widget_checked("files_list");

    if (get_iter_to_first_selected_row(files_list, &iter))
    {
        gchar * in_name;

        gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter, 
            COL_DATA_FILE, &in_name, -1);

        show_ceos_meta_data(in_name);
    }
    else
    {
        show_please_select_message();
    }

    return TRUE;
}

static int
handle_display_asf_metadata()
{
    GtkWidget *completed_files_list;
    GtkTreeIter iter;

    completed_files_list = get_widget_checked("completed_files_list");

    if (get_iter_to_first_selected_row(completed_files_list, &iter))
    {
        gchar * out_name;

        gtk_tree_model_get(GTK_TREE_MODEL(completed_list_store), &iter, 
            COMP_COL_OUTPUT_FILE, &out_name, -1);

        show_asf_meta_data(out_name);
        g_free(out_name);
    }
    else
    {
        show_please_select_message();
    }

    return TRUE;
}

static int
handle_view_log(int completed)
{
    GtkListStore *ls;
    char *widget;
    int log_col, data_col;

    if (completed) {
        widget = "completed_files_list";
        ls = completed_list_store;
        log_col = COMP_COL_LOG;
        data_col = COMP_COL_DATA_FILE;
    } else {
        widget = "files_list";
        ls = list_store;
        log_col = COL_LOG;
        data_col = COL_DATA_FILE;
    }

    GtkWidget *list = get_widget_checked(widget);
    GtkTreeIter iter;

    if (get_iter_to_first_selected_row(list, &iter))
    {
        gchar *log_txt, *data_file;

        gtk_tree_model_get(GTK_TREE_MODEL(ls), &iter,
                           data_col, &data_file,
                           log_col, &log_txt, -1);

        show_log(log_txt, data_file);

        g_free(log_txt);
        g_free(data_file);
    }
    else
    {
        show_please_select_message();
    }

    return TRUE;
}

static int
handle_process()
{
    GtkWidget *files_list;
    GtkTreeIter iter;
    GtkTreeModel * model;
    GtkTreeSelection * selection;
    GList * selected_rows, * i;
    GList * refs;
    gboolean confirm_needed = FALSE;

    /* gui should prevent this from happening */
    if (processing) {
        return TRUE;
    }

    files_list = get_widget_checked("files_list");
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list));
    model = GTK_TREE_MODEL(list_store);

    selected_rows = gtk_tree_selection_get_selected_rows(selection, &model);

    if (!selected_rows)
    {
        show_please_select_message();
		return FALSE;
    }

    refs = NULL;
    i = selected_rows;

    while (i)
    {
        GtkTreePath * path = (GtkTreePath *) i->data;

        if (!confirm_needed)
        {
            gchar *output_file;
            gchar *status;

            gtk_tree_model_get_iter(model, &iter, path);
            gtk_tree_model_get (model, &iter, 
                COL_OUTPUT_FILE, &output_file, 
                COL_STATUS, &status,
                -1);

            if (strcmp(status, "Done") != 0 &&
                g_file_test(output_file, G_FILE_TEST_EXISTS))
            {
                confirm_needed = TRUE;
            }

            g_free(output_file);
            g_free(status);
        }

        refs = g_list_append(refs, gtk_tree_row_reference_new(model, path));
        i = g_list_next(i);
    }

    if (!confirm_needed || confirm_overwrite())
    {
        process_items_from_list(refs, FALSE);
    }

    g_list_foreach(selected_rows, (GFunc)gtk_tree_path_free, NULL);
    g_list_free(selected_rows);

    return TRUE;
}

static int
handle_rename()
{
  return rename_selected_output_filename();
}

static int
handle_view_output()
{
    GtkWidget *completed_files_list;
    GtkTreeIter iter;

    completed_files_list = get_widget_checked("completed_files_list");

    if (get_iter_to_first_selected_row(completed_files_list, &iter))
    {
        gchar * out_name;

        gtk_tree_model_get(GTK_TREE_MODEL(completed_list_store), &iter, 
            COMP_COL_OUTPUT_FILE, &out_name, -1);

	if (g_file_test(out_name, G_FILE_TEST_EXISTS))
	{
	    show_output_image(out_name);
	}
	else
	{
	    char msg[2048];
	    sprintf(msg, "Processing on selected file not complete OR\noutput image file was not found:\n"
		         "   %s\n", out_name);
	    message_box(msg);
	}

        g_free(out_name);
    }
    else
    {
        show_please_select_message();
    }

    return TRUE;
}

static int
handle_google_earth_imp(const char *widget_name, GtkListStore *store)
{
    GtkWidget *files_list;
    GtkTreeModel * model;
    GtkTreeSelection *selection;
    GList * selected_rows, * i;
    GList * refs;
    FILE *kml_file = NULL;
    char kml_filename[256];
    int pid, first = TRUE;
    gchar *ge;
    char *output_dir = NULL;
    int n_ok = 0;
    int n_bad = 0;

    files_list = get_widget_checked(widget_name);
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list));
    model = GTK_TREE_MODEL(store);
    selected_rows = gtk_tree_selection_get_selected_rows(selection, &model);

    refs = NULL;
    i = selected_rows;

    if (!selected_rows)
    {
        show_please_select_message();
        return FALSE;
    }

    while (i)
    {
        GtkTreePath * path;
        GtkTreeRowReference * ref;

        path = (GtkTreePath *) i->data;
        ref = gtk_tree_row_reference_new(model, path);

        refs = g_list_append(refs, ref);

        i = g_list_next(i);
    }

    i = refs;
    pid = getpid();

    while (i)
    {
        GtkTreePath * path;
        GtkTreeIter iter;
        GtkTreeRowReference * ref;
        gchar * input_name;
        gchar * out_name;
        gchar * metadata_name;
        GdkPixbuf *pb = NULL;
        meta_parameters *meta;

        ref = (GtkTreeRowReference *) i->data;
        path = gtk_tree_row_reference_get_path(ref);
        gtk_tree_model_get_iter(model, &iter, path);

        if (strstr(widget_name, "completed")) {
            gtk_tree_model_get(model, &iter, 
                COMP_COL_DATA_FILE, &input_name, 
                COMP_COL_OUTPUT_FILE, &out_name,
                COMP_COL_OUTPUT_THUMBNAIL_BIG, &pb,
                -1);
        } else {
            gtk_tree_model_get(model, &iter, 
                COL_DATA_FILE, &input_name, 
                COL_OUTPUT_FILE, &out_name,
                -1);
        }

        if (first)
        {
            output_dir = MALLOC(sizeof(char) * (strlen(out_name) + 1));
            char *tmp = MALLOC(sizeof(char) * (strlen(out_name) + 1));
            split_dir_and_file(out_name, output_dir, tmp);
            free(tmp);

            sprintf(kml_filename, "%stmp%d.kml", output_dir, pid);
            printf("Temporary kml file: %s\n", kml_filename);
            kml_file = fopen(kml_filename, "w");
            if (!kml_file)
            {
                message_box("Couldn't open kml file!");

                g_list_foreach(selected_rows, (GFunc)gtk_tree_path_free, NULL);
                g_list_free(selected_rows);
                
                g_list_foreach(refs, (GFunc)gtk_tree_row_reference_free, NULL);
                g_list_free(refs);
                
                return FALSE;
            }

            kml_header(kml_file);

            first = FALSE;
        }

        metadata_name = build_asf_metadata_filename(out_name);
        char *base_output_name = get_basename(out_name);
        
        if (fileExists(metadata_name)) 
            meta = meta_read(metadata_name);
        else
            meta = meta_create(metadata_name);

        if (meta && meta->general &&
            meta_is_valid_double(meta->general->center_latitude) &&
            meta_is_valid_double(meta->general->center_longitude))
        {
            printf("Adding to kml: %s\n", input_name);
            if (pb) {
                // we have a pixbuf to use as an overlay
                char *png_filename = MALLOC(sizeof(char)*
                    (strlen(output_dir) + strlen(base_output_name) + 10));
                sprintf(png_filename, "%s%s.png", output_dir,
                    base_output_name);
                printf("Generating png: %s\n", png_filename);
                pixbuf2png(pb, png_filename);
                kml_entry_with_overlay(kml_file, meta, base_output_name, 
                    png_filename, output_dir);
                FREE(png_filename);
                // FIXME: remember png files, so we can delete later?
            } else {
                kml_entry(kml_file, meta, base_output_name);
            }
            meta_free(meta);
            ++n_ok;
        }
        else
        {
            printf("Failed to add to kml: %s\n", metadata_name);
            if (meta) meta_free(meta);
            ++n_bad;
        }

        free(base_output_name);

        g_free(metadata_name);
        i = g_list_next(i);
    }

    kml_footer(kml_file);
    fclose(kml_file);

    if (n_bad > 0)
    {
        message_box("Some of the metadata files failed to load.\n");
    }

#ifdef win32
    char path[1024];
    FindExecutable((LPCTSTR)kml_filename, (LPCTSTR)output_dir, (LPTSTR)path);
    ge = STRDUP(escapify(path));
    printf("Path to google earth: %s\n", ge);
#else
    ge = find_in_path("googleearth");
    if (!ge)
    {
       message_box("Couldn't find googleearth!  Is it installed?");
       return FALSE;
    }
#endif

    if (n_ok > 0)
    {
        int pid = fork();
        if (pid == 0) {
            asfSystem("\"%s\" \"%s\"", ge, kml_filename);
            //unlink(kml_filename);
            exit(EXIT_SUCCESS);
        }
    }
    
    g_list_foreach(selected_rows, (GFunc)gtk_tree_path_free, NULL);
    g_list_free(selected_rows);

    g_list_foreach(refs, (GFunc)gtk_tree_row_reference_free, NULL);
    g_list_free(refs);
    free(output_dir);

    return TRUE;
}

static int
handle_google_earth()
{
    return handle_google_earth_imp("files_list", list_store);
}

static int
handle_completed_files_google_earth()
{
    return handle_google_earth_imp("completed_files_list", 
                                   completed_list_store);
}

static int 
handle_reprocess()
{
    GtkWidget *completed_files_list;
    GtkTreeModel * model;
    GtkTreeSelection *selection;
    GList * selected_rows, * i;
    GList * refs;

    completed_files_list = get_widget_checked("completed_files_list");
    selection = gtk_tree_view_get_selection(
        GTK_TREE_VIEW(completed_files_list));
    model = GTK_TREE_MODEL(completed_list_store);

    selected_rows = gtk_tree_selection_get_selected_rows(selection, &model);

    refs = NULL;
    i = selected_rows;

    if (!selected_rows)
    {
        show_please_select_message();
        return FALSE;
    }

    while (i)
    {
        GtkTreePath * path;
        GtkTreeRowReference * ref;

        path = (GtkTreePath *) i->data;
        ref = gtk_tree_row_reference_new(model, path);

        refs = g_list_append(refs, ref);

        i = g_list_next(i);
    }

    i = refs;

    while (i)
    {
        GtkTreePath * path;
        GtkTreeIter iter;
        GtkTreeRowReference * ref;

        ref = (GtkTreeRowReference *) i->data;
        path = gtk_tree_row_reference_get_path(ref);
        gtk_tree_model_get_iter(model, &iter, path);
        move_from_completed_files_list(&iter);

        i = g_list_next(i);
    }

    g_list_foreach(selected_rows, (GFunc)gtk_tree_path_free, NULL);
    g_list_free(selected_rows);

    g_list_foreach(refs, (GFunc)gtk_tree_row_reference_free, NULL);
    g_list_free(refs);

    show_queued_thumbnails();

    return TRUE;
}

SIGNAL_CALLBACK void
on_remove_button_clicked(GtkWidget *widget)
{
  handle_remove();
}

SIGNAL_CALLBACK void
on_remove_completed_button_clicked(GtkWidget *widget)
{
  handle_remove_completed();
}

SIGNAL_CALLBACK void
on_process_button_clicked(GtkWidget *widget)
{
  handle_process();
}

SIGNAL_CALLBACK void
on_reprocess_button_clicked(GtkWidget *widget)
{
  handle_reprocess();
}

SIGNAL_CALLBACK void
on_rename_button_clicked(GtkWidget *widget)
{
  handle_rename();
}

SIGNAL_CALLBACK void
on_view_log_button_clicked(GtkWidget *widget)
{
  handle_view_log(0);
}

SIGNAL_CALLBACK void
on_view_log_button2_clicked(GtkWidget *widget)
{
  handle_view_log(1);
}

SIGNAL_CALLBACK void
on_display_ceos_button_clicked(GtkWidget *widget)
{
  handle_display_ceos_metadata();
}

SIGNAL_CALLBACK void
on_display_asf_button_clicked(GtkWidget *widget)
{
  handle_display_asf_metadata();
}

SIGNAL_CALLBACK void
on_view_output_button_clicked(GtkWidget *widget)
{
  handle_view_output();
}

SIGNAL_CALLBACK void
on_google_earth_button_clicked(GtkWidget *widget)
{
  handle_google_earth();
}

SIGNAL_CALLBACK void
on_completed_files_google_earth_button_clicked(GtkWidget *widget)
{
  handle_completed_files_google_earth();
}

SIGNAL_CALLBACK gint
popup_menu_ceos_metadata(GtkWidget *widget, GdkEvent *event)
{
  return handle_display_ceos_metadata();
}

SIGNAL_CALLBACK gint
popup_menu_asf_metadata(GtkWidget *widget, GdkEvent *event)
{
  return handle_display_asf_metadata();
}

SIGNAL_CALLBACK gint
popup_menu_remove(GtkWidget *widget, GdkEvent *event)
{
  return handle_remove();
}

SIGNAL_CALLBACK gint
popup_menu_remove_completed(GtkWidget *widget, GdkEvent *event)
{
  return handle_remove_completed();
}

SIGNAL_CALLBACK gint
popup_menu_view_log(GtkWidget *widget, GdkEvent *event)
{
  return handle_view_log(0);
}

SIGNAL_CALLBACK gint
popup_menu_view_log2(GtkWidget *widget, GdkEvent *event)
{
  return handle_view_log(1);
}

SIGNAL_CALLBACK gint
popup_menu_process(GtkWidget *widget, GdkEvent *event)
{
  return handle_process();
}

SIGNAL_CALLBACK gint
popup_menu_reprocess(GtkWidget *widget, GdkEvent *event)
{
  return handle_reprocess();
}

SIGNAL_CALLBACK gint
popup_menu_rename(GtkWidget *widget, GdkEvent *event)
{
  return handle_rename();
}

SIGNAL_CALLBACK gint
popup_menu_view_output(GtkWidget *widget, GdkEvent *event)
{
  return handle_view_output();
}

SIGNAL_CALLBACK gint
popup_menu_google_earth(GtkWidget *widget, GdkEvent *event)
{
  return handle_google_earth();
}

SIGNAL_CALLBACK gint
popup_menu_completed_files_google_earth(GtkWidget *widget, GdkEvent *event)
{
  return handle_completed_files_google_earth();
}

static void
setup_completed_files_popup_menu()
{
    GtkWidget *menu, *widget, *item;

    /* if they right click in the files list, we'll pop up */
    widget = get_widget_checked("completed_files_list");

    menu = gtk_menu_new();

    item = gtk_menu_item_new_with_label("Remove");  
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_remove_completed), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("Queue for Reprocessing");  
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_reprocess), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("View Log");
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );  
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_view_log2), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("Display ASF Metadata");
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );  
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_asf_metadata), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("View Output");
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );  
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_view_output), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("View With Google Earth");
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );  
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_completed_files_google_earth), NULL);
    gtk_widget_show(item);

    gtk_widget_show(menu);

    g_signal_connect_swapped(widget, "button_press_event",
        G_CALLBACK(completed_files_popup_handler), menu);
    g_signal_connect_swapped(widget, "popup_menu",
        G_CALLBACK(completed_files_popup_handler), menu);
}

static void
setup_files_popup_menu()
{
    GtkWidget *menu, *widget, *item;

    /* if they right click in the files list, we'll pop up */
    widget = get_widget_checked("files_list");

    menu = gtk_menu_new();

    item = gtk_menu_item_new_with_label("Remove");  
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_remove), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("Process");
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );  
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_process), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("Rename Output");
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );  
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_rename), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("View Log");
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );  
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_view_log), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("Display CEOS Metadata");
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );  
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_ceos_metadata), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("View With Google Earth");
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );  
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_google_earth), NULL);
    gtk_widget_show(item);

    gtk_widget_show(menu);

    g_signal_connect_swapped(widget, "button_press_event",
        G_CALLBACK(files_popup_handler), menu);
    g_signal_connect_swapped(widget, "popup_menu",
        G_CALLBACK(files_popup_handler), menu);
}

void
setup_popup_menu()
{
    setup_files_popup_menu();
    setup_completed_files_popup_menu();
}
