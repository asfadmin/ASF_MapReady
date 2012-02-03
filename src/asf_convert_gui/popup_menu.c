#include <unistd.h>

#ifdef win32
#define BYTE __byte
#include "asf.h"
#include <asf_meta.h>
#undef BYTE
#include <windows.h>
#include <shellapi.h>
#endif

#include <tiff.h>
#include <tiffio.h>
#include <xtiffio.h>
#include <geotiff_support.h>
#include "asf_convert_gui.h"
#include <asf.h>

#define POINT __tmp_point
#include <asf_vector.h>
#undef POINT

static const int popup_menu_item_remove = 0;
static const int popup_menu_item_process = 1;
static const int popup_menu_item_reprocess = 1;
static const int popup_menu_item_display_ceos_metadata = 4;
static const int popup_menu_item_view_input = 5;
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
  // Input files toolbar images
    GtkWidget * w = get_widget_checked("input_files_remove_button_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("trash_can.png"));

    w = get_widget_checked("input_files_process_button_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("process_files.png"));

    w = get_widget_checked("input_files_rename_button_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("rename_icon.png"));

    w = get_widget_checked("input_files_view_log_button_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("view_log.png"));

    w = get_widget_checked("ceos_metadata_toolbar_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("ceos_metadata.png"));

    w = get_widget_checked("input_files_view_input_button_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("asf_view_button.png"));

    w = get_widget_checked("google_earth_toolbar_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("google_earth_button.gif"));

  // Completed files toolbar images
    w = get_widget_checked("completed_files_remove_button_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("trash_can.png"));

    w = get_widget_checked("completed_files_reprocess_button_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("up_arrow.png"));

    w = get_widget_checked("completed_files_view_log_button_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("view_log.png"));

    w = get_widget_checked("asf_metadata_toolbar_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("asf_metadata.png"));

    w = get_widget_checked("completed_files_view_output_button_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("asf_view_button.png"));

    w = get_widget_checked("completed_files_google_earth_toolbar_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("google_earth_button.gif"));

  // Other button images
    w = get_widget_checked("settings_button_expanded_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("minus.gif"));

    w = get_widget_checked("settings_button_collapsed_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("plus.gif"));

    w = get_widget_checked("files_button_expanded_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("minus.gif"));

    w = get_widget_checked("files_button_collapsed_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("plus.gif"));

    w = get_widget_checked("completed_files_button_expanded_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("minus.gif"));

    w = get_widget_checked("completed_files_button_collapsed_image");
    gtk_image_set_from_file(GTK_IMAGE(w), imgloc("plus.gif"));
}

void
show_please_select_message()
{
    static const char *msg = "Please select a file first!\n";
    message_box(msg);
}

void
show_not_available_message()
{
    static const char *msg = "That intermediate product is not available.\n\n"
      "  It either was not generated, or has been deleted.\n\n"
      "  It may not have been generated because the particular option that  \n"
      "  produced that intermediate file was not selected (for example,\n"
      "  to view the simulated SAR image, you need to have selected\n"
      "  terrain correction).\n\n"
      "  It may have been deleted, if you selected the 'Keep no intermediate\n"
      "  files' option on the General tab.\n";

    message_box(msg);
}

static GtkMenu *find_submenu(GtkMenu *menu)
{
    GList *children = gtk_container_get_children(GTK_CONTAINER(menu));
    GList *iter = children;

    while (iter)
    {
        GtkWidget *sub = gtk_menu_item_get_submenu(GTK_MENU_ITEM(iter->data));
        if (sub) return GTK_MENU(sub);
        iter = g_list_next(iter);
    }

    g_list_free(children);
    return NULL;
}

static void
enable_menu_items(GtkMenu * menu, gboolean enable_display_ceos_metadata,
                  gboolean enable_view_input)
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

        if ((n == popup_menu_item_view_input && !enable_view_input) ||
            (n == popup_menu_item_display_ceos_metadata &&
             (!enable_display_ceos_metadata || !use_thumbnails)))
        {
            enable = FALSE;
        }

        gtk_widget_set_sensitive(item, enable);

        iter = g_list_next(iter);
        ++n;
    }

    g_list_free(children);
}

static void
enable_submenu_items(GtkMenu *menu, int *e)
{
    int n = 0;

    GList *children = gtk_container_get_children(GTK_CONTAINER(menu));
    GList *iter = children;

    while (iter)
    {
        gtk_widget_set_sensitive(GTK_WIDGET(iter->data), e[n++]);
        iter = g_list_next(iter);
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
              gchar *status, *out_name, *in_name, *ceos_meta_name, *aux_meta;

                gtk_tree_selection_unselect_all(selection);
                gtk_tree_selection_select_path(selection, path);
                gtk_tree_model_get_iter(GTK_TREE_MODEL(list_store),
                    &iter, path);

                gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
                    COL_STATUS, &status,
                    COL_INPUT_FILE, &in_name,
                    COL_METADATA_FILE, &aux_meta,
                    COL_OUTPUT_FILE, &out_name, -1);

                gtk_tree_path_free(path);

                if (strstr(status, "...") != NULL)
                {
                  /* right-clicked on what is currently being processed */
                  g_free(status);
                  g_free(in_name);
                  g_free(out_name);

                  return FALSE;
                }

                /* check if we should disable "Display CEOS Metadata" */
                ceos_meta_name = meta_file_name(in_name);
                gboolean show_display_ceos_metadata_menu_item =
                  g_file_test(ceos_meta_name, G_FILE_TEST_EXISTS);
                g_free(ceos_meta_name);

                /* check if we should disable "View Input" (gamma) */
                gboolean show_view_input = TRUE;
                if (strlen(aux_meta) > 0)
                  show_view_input = FALSE;

                /* enable/disable the items */
                enable_menu_items(menu, show_display_ceos_metadata_menu_item,
                                  show_view_input);

                g_free(status);
                g_free(in_name);
                g_free(out_name);
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
                // figure out which of the "View Intermediates" items
                // should be enabled
                GtkTreeIter iter;
                gtk_tree_selection_unselect_all(selection);
                gtk_tree_selection_select_path(selection, path);
                gtk_tree_model_get_iter(GTK_TREE_MODEL(completed_list_store),
                    &iter, path);

                gchar *layover, *dem, *simsar, *faraday, *hist, *class_map, *incid_angles;
                gtk_tree_model_get(GTK_TREE_MODEL(completed_list_store), &iter,
                    COMP_COL_LAYOVER_SHADOW_MASK_FILE, &layover,
                    COMP_COL_CLIPPED_DEM_FILE, &dem,
                    COMP_COL_SIMULATED_SAR_FILE, &simsar,
                    COMP_COL_FARADAY_FILE, &faraday,
                    COMP_COL_HIST_FILE, &hist,
                    COMP_COL_INCID_ANGLES_FILE, &incid_angles,
                    COMP_COL_CLASS_MAP_FILE, &class_map, -1);
                gtk_tree_path_free(path);

                int enable[7];
                enable[0] = fileExists(layover);
                enable[1] = fileExists(dem);
                enable[2] = fileExists(simsar);
                enable[3] = fileExists(faraday);
                enable[4] = fileExists(hist);
                enable[5] = fileExists(class_map);
                enable[6] = fileExists(incid_angles);

                GtkMenu *submenu = find_submenu(menu);
                if (submenu)
                    enable_submenu_items(submenu, enable);

                // enable all top-level menu items
                enable_menu_items(menu, TRUE, TRUE);

                g_free(layover);
                g_free(dem);
                g_free(simsar);
                g_free(faraday);
                g_free(hist);
                g_free(class_map);
                g_free(incid_angles);
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

static GList *get_selected_rows(GtkWidget *files_list, GtkListStore *store)
{
  GtkTreeSelection *selection;
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list));

  GtkTreeModel *model = GTK_TREE_MODEL(store);

  GList *selected_rows;
  selected_rows = gtk_tree_selection_get_selected_rows(selection, &model);

  if (!selected_rows) {
    // if we have just 1 item in the whole list, select it, and return it
    int n = gtk_tree_model_iter_n_children(model, NULL);
    if (n==1) {
      gtk_tree_selection_select_all(selection);
      selected_rows = gtk_tree_selection_get_selected_rows(selection, &model);
    }
  }

  return selected_rows;
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

    //  Don't allow the auto-pick for "remove" -- don't want to have
    //  that happen by accident
    //selected_rows = get_selected_rows(files_list, store);

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

    refresh_file_names();
    input_data_formats_changed();

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
get_iter_to_first_selected_row(GtkWidget *files_list,
                               GtkListStore *store,
                               GtkTreeIter *iter)
{
    GList * selected_rows;
    GtkTreeModel *model;
    gboolean found;

    model = GTK_TREE_MODEL(store);

    selected_rows = get_selected_rows(files_list, store);
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

    if (get_iter_to_first_selected_row(files_list, list_store, &iter))
    {
        gchar * in_name;

        gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
            COL_INPUT_FILE, &in_name, -1);

        show_ceos_meta_data(in_name);
        g_free(in_name);
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

    if (get_iter_to_first_selected_row(completed_files_list,
                                       completed_list_store,
                                       &iter))
    {
        gchar *meta_name;

        gtk_tree_model_get(GTK_TREE_MODEL(completed_list_store), &iter,
            COMP_COL_METADATA_FILE, &meta_name, -1);

        show_asf_meta_data(meta_name);
        g_free(meta_name);
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
    int log_col, file_col;

    if (completed) {
        widget = "completed_files_list";
        ls = completed_list_store;
        log_col = COMP_COL_LOG;
        file_col = COMP_COL_INPUT_FILE;
    } else {
        widget = "files_list";
        ls = list_store;
        log_col = COL_LOG;
        file_col = COL_INPUT_FILE;
    }

    GtkWidget *list = get_widget_checked(widget);
    GtkTreeIter iter;

    if (get_iter_to_first_selected_row(list, ls, &iter))
    {
        gchar *log_txt, *input_file;

        gtk_tree_model_get(GTK_TREE_MODEL(ls), &iter,
                           file_col, &input_file,
                           log_col, &log_txt, -1);

        show_log(log_txt, input_file);

        g_free(log_txt);
        g_free(input_file);
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
    GList * selected_rows, * i;
    GList * refs;
    gboolean confirm_needed = FALSE;

    /* gui should prevent this from happening */
    if (processing) {
        return TRUE;
    }

    files_list = get_widget_checked("files_list");
    model = GTK_TREE_MODEL(list_store);
    selected_rows = get_selected_rows(files_list, list_store);

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
    GtkWidget *files_list = get_widget_checked("files_list");
    GtkTreeSelection *selection =
        gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list));

    int num_selected = gtk_tree_selection_count_selected_rows(selection);

    if (num_selected > 1) {
      message_box("Please select only one file at a time to rename.");
      return TRUE;
    }
    else {
      return rename_selected_output_filename();
    }
}

static int try_suffix(const gchar *in_name, const char *suffix,
                      char **out_name)
{
    char *tmp = appendToBasename(in_name, suffix);
    if (g_file_test(tmp, G_FILE_TEST_EXISTS)) {
        *out_name = STRDUP(tmp);
        free(tmp);
        return TRUE;
    } else {
        free(tmp);
        return FALSE;
    }
}

static int
handle_view_output()
{
    GtkWidget *completed_files_list;
    GtkTreeIter iter;

    completed_files_list = get_widget_checked("completed_files_list");

    if (get_iter_to_first_selected_row(completed_files_list,
                                       completed_list_store,
                                       &iter))
    {
        gchar *out_name;
        gtk_tree_model_get(GTK_TREE_MODEL(completed_list_store), &iter,
            COMP_COL_OUTPUT_FILE, &out_name, -1);

        if (g_file_test(out_name, G_FILE_TEST_EXISTS))
        {
          show_image_with_asf_view(out_name);
        }
        else
        {
          // Could be that band names were appended...
          // This should not be necessary any longer with the code that
          // puts the output filename in the "intermediates" list.
          // Doesn't hurt to leave it, though it shouldn't ever run.
          char *tmp_out=NULL;

          if (try_suffix(out_name, "_POLSARPRO", &tmp_out))
            show_image_with_asf_view(tmp_out);

          else if (try_suffix(out_name, "_HH", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_VV", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_HV", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_VH", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_01", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_02", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_03", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_04", &tmp_out))
            show_image_with_asf_view(tmp_out);

          // some SLC possibilities
          else if (try_suffix(out_name, "_AMP-HH", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_AMP-VV", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_AMP-VH", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_AMP-HV", &tmp_out))
            show_image_with_asf_view(tmp_out);

          else if (try_suffix(out_name, "_SIGMA-HH", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_BETA-HH", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_GAMMA-HH", &tmp_out))
            show_image_with_asf_view(tmp_out);

          else if (try_suffix(out_name, "_Entropy", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_Anisotropy", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_Alpha", &tmp_out))
            show_image_with_asf_view(tmp_out);

          else if (try_suffix(out_name, "_SIGMA-AMP-HH", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_BETA-AMP-HH", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_GAMMA-AMP-HH", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_SIGMA_DB-AMP-HH", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_BETA_DB-AMP-HH", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_GAMMA_DB-AMP-HH", &tmp_out))
            show_image_with_asf_view(tmp_out);

          // some airsar possibilities
          else if (try_suffix(out_name, "_c_vv", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_l_vv", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_c_POWER", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_l_POWER", &tmp_out))
            show_image_with_asf_view(tmp_out);
          else if (try_suffix(out_name, "_p_POWER", &tmp_out))
            show_image_with_asf_view(tmp_out);

          // give up
          else {
            char msg[2048];
            sprintf(msg, "Processing on selected file not complete OR\n"
                    "output image file was not found:\n"
                    "   %s\n", out_name);
            message_box(msg);
          }

          FREE(tmp_out);
        }

        g_free(out_name);
    }
    else {
      show_please_select_message();
    }

    return TRUE;
}

static int
handle_view_input()
{
    GtkWidget *files_list;
    GtkTreeIter iter;

    files_list = get_widget_checked("files_list");

    if (get_iter_to_first_selected_row(files_list, list_store, &iter))
    {
        gchar * in_name;
        gchar * polsarpro_aux_info;

        gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter,
                           COL_INPUT_FILE, &in_name,
                           COL_POLSARPRO_INFO, &polsarpro_aux_info,
                           -1);

        if (polsarpro_aux_info && strlen(polsarpro_aux_info)>0) {
          char *lut_name = extract_lut_name(polsarpro_aux_info);
          if (lut_name && strlen(lut_name)>0) {
            char *arg = MALLOC(sizeof(char)*(strlen(lut_name)+16));
            sprintf(arg, "-colormap %s", lut_name);
            show_image_with_asf_view_arg(in_name, arg);
            free(arg);
          }
          else
            show_image_with_asf_view(in_name);
          if (lut_name)
            free(lut_name);
        }
        else
          show_image_with_asf_view(in_name);

        g_free(in_name);
        g_free(polsarpro_aux_info);
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
    GList * selected_rows, * i;
    GList * refs;
    //FILE *kml_file = NULL;
    char kml_filename[256];
    gchar *ge;
    char *output_dir = NULL;
    //int n_ok = 0;
    //int n_bad = 0;

    files_list = get_widget_checked(widget_name);
    model = GTK_TREE_MODEL(store);
    selected_rows = get_selected_rows(files_list, store);

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
    
    GtkTreePath * path;
    GtkTreeIter iter;
    GtkTreeRowReference * ref;
    gchar * input_name=NULL;
    gchar * out_name=NULL;
    gchar * tmp_dir=NULL;
    gchar * metadata_name=NULL;
    GdkPixbuf *pb = NULL;
    //meta_parameters *meta;

    ref = (GtkTreeRowReference *) i->data;
    path = gtk_tree_row_reference_get_path(ref);
    gtk_tree_model_get_iter(model, &iter, path);
    
    if (strstr(widget_name, "completed")) {
      gtk_tree_model_get(model, &iter,
                COMP_COL_INPUT_FILE, &input_name,
			 COMP_COL_OUTPUT_FILE, &out_name,
			 COMP_COL_OUTPUT_THUMBNAIL_BIG, &pb,
			 COMP_COL_TMP_DIR, &tmp_dir,
			 -1);
      
      metadata_name = build_asf_metadata_filename(out_name);
    }
    else {
      gtk_tree_model_get(model, &iter,
			 COL_INPUT_FILE, &input_name,
			 COL_OUTPUT_FILE, &out_name,
			 -1);
      
      metadata_name = meta_file_name(input_name);
    }
    
    char *base_output_name = get_basename(out_name);
    sprintf(kml_filename, "%s/%s_overlay.kml", tmp_dir, base_output_name);
    free(base_output_name);
    g_free(metadata_name);
    g_free(input_name);
    g_free(out_name);

#ifdef win32
    char ge_path[1024];
    FindExecutable((LPCTSTR)kml_filename, (LPCTSTR)output_dir, 
                   (LPTSTR)ge_path);
    ge = STRDUP(escapify(ge_path));
    printf("Path to google earth: %s\n", ge);
#else
    ge = find_in_path("googleearth");
    if (!ge)
    {
       message_box("Couldn't find googleearth!  Is it installed?");
       return FALSE;
    }
#endif

    if (fileExists(kml_filename))
    {
#ifdef win32
        asfSystem("\"%s\" \"%s\"", ge, kml_filename);
#else
        int pid = fork();
        if (pid == 0) {
	  asfSystem("\"%s\" \"%s\"", ge, kml_filename);
	  exit(EXIT_SUCCESS);
        }
#endif
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
    GList * selected_rows, * i;
    GList * refs;

    completed_files_list = get_widget_checked("completed_files_list");
    model = GTK_TREE_MODEL(completed_list_store);

    selected_rows = get_selected_rows(completed_files_list,
                                      completed_list_store);

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

    input_data_formats_changed();
    refresh_file_names();
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
on_view_input_button_clicked(GtkWidget *widget)
{
  handle_view_input();
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
popup_menu_view_input(GtkWidget *widget, GdkEvent *event)
{
  return handle_view_input();
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

static int handle_view_intermediate(int column, char *arg)
{
    GtkWidget *completed_files_list;
    GtkTreeIter iter;

    completed_files_list = get_widget_checked("completed_files_list");

    if (get_iter_to_first_selected_row(completed_files_list,
                                       completed_list_store,
                                       &iter))
    {
        gchar *name;
        gtk_tree_model_get(GTK_TREE_MODEL(completed_list_store), &iter,
            column, &name, -1);

        if (name && strlen(name) > 0) {
          show_image_with_asf_view_arg(name, arg);
        }
        else {
          show_not_available_message();
        }
        g_free(name);
    }
    else
    {
        show_please_select_message();
    }

    return TRUE;
}

SIGNAL_CALLBACK gint
view_layover_mask(GtkWidget *widget, GdkEvent *event)
{
  return handle_view_intermediate(COMP_COL_LAYOVER_SHADOW_MASK_FILE, "");
}

SIGNAL_CALLBACK gint
view_clipped_dem(GtkWidget *widget, GdkEvent *event)
{
  return handle_view_intermediate(COMP_COL_CLIPPED_DEM_FILE, "");
}

SIGNAL_CALLBACK gint
view_simsar(GtkWidget *widget, GdkEvent *event)
{
  return handle_view_intermediate(COMP_COL_SIMULATED_SAR_FILE, "");
}

SIGNAL_CALLBACK gint
view_faraday(GtkWidget *widget, GdkEvent *event)
{
  return handle_view_intermediate(COMP_COL_FARADAY_FILE, "");
}

SIGNAL_CALLBACK gint
view_incid_angles(GtkWidget *widget, GdkEvent *event)
{
  return handle_view_intermediate(COMP_COL_INCID_ANGLES_FILE, "");
}

SIGNAL_CALLBACK gint
view_hist(GtkWidget *widget, GdkEvent *event)
{
  return handle_view_intermediate(COMP_COL_HIST_FILE, "-colormap polarimetry");
}

SIGNAL_CALLBACK gint
view_class_map(GtkWidget *widget, GdkEvent *event)
{
  return handle_view_intermediate(COMP_COL_CLASS_MAP_FILE,
                                  "-colormap cloude16");
}

static void
setup_completed_files_popup_menu()
{
    GtkWidget *menu, *view_submenu, *widget, *item;

    /* if they right click in the files list, we'll pop up */
    widget = get_widget_checked("completed_files_list");

    view_submenu = gtk_menu_new();

    item = gtk_menu_item_new_with_label("View Layover/Shadow Mask");
    gtk_menu_shell_append(GTK_MENU_SHELL(view_submenu), item);
    g_signal_connect_swapped(G_OBJECT(item), "activate",
                             G_CALLBACK(view_layover_mask), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("View Clipped DEM");
    gtk_menu_shell_append(GTK_MENU_SHELL(view_submenu), item);
    g_signal_connect_swapped(G_OBJECT(item), "activate",
                             G_CALLBACK(view_clipped_dem), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("View Simulated SAR Image");
    gtk_menu_shell_append(GTK_MENU_SHELL(view_submenu), item);
    g_signal_connect_swapped(G_OBJECT(item), "activate",
                             G_CALLBACK(view_simsar), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("View Faraday Rotations");
    gtk_menu_shell_append(GTK_MENU_SHELL(view_submenu), item);
    g_signal_connect_swapped(G_OBJECT(item), "activate",
                             G_CALLBACK(view_faraday), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("View Cloude-Pottier Histogram");
    gtk_menu_shell_append(GTK_MENU_SHELL(view_submenu), item);
    g_signal_connect_swapped(G_OBJECT(item), "activate",
                             G_CALLBACK(view_hist), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("View Entropy/Alpha Class Map");
    gtk_menu_shell_append(GTK_MENU_SHELL(view_submenu), item);
    g_signal_connect_swapped(G_OBJECT(item), "activate",
                             G_CALLBACK(view_class_map), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("View Incidence Angles");
    gtk_menu_shell_append(GTK_MENU_SHELL(view_submenu), item);
    g_signal_connect_swapped(G_OBJECT(item), "activate",
                             G_CALLBACK(view_incid_angles), NULL);
    gtk_widget_show(item);

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

    item = gtk_menu_item_new_with_label("View Intermediates");
    gtk_menu_item_set_submenu(GTK_MENU_ITEM(item), view_submenu);
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );
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

    item = gtk_menu_item_new_with_label("View With Google Earth(tm)");
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

    item = gtk_menu_item_new_with_label("View Input");
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_view_input), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("View With Google Earth(tm)");
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
