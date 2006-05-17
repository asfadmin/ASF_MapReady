#include "asf_convert_gui.h"

static const int popup_menu_item_remove = 0;
static const int popup_menu_item_process = 1;
static const int popup_menu_item_display_ceos_metadata = 4;
static const int popup_menu_item_display_asf_metadata = 5;
static const int popup_menu_item_view_output = 6;

void
show_please_select_message()
{
    static const char *msg = 
      "Please select a file first!\n";

    message_box(msg);
}

static void
enable_menu_items(GtkMenu * menu, gboolean enable_view_output,
                  gboolean enable_display_ceos_metadata,
                  gboolean enable_display_asf_metadata)
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

        if ((n == popup_menu_item_view_output &&
            !enable_view_output) ||
            (n == popup_menu_item_display_ceos_metadata &&
            (!enable_display_ceos_metadata || !use_thumbnails)) ||
            (n == popup_menu_item_display_asf_metadata &&
            !enable_display_asf_metadata))
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
    GtkWidget *jump_button;
    GtkWidget *display_ceos_button;
    GtkWidget *display_asf_button;
    GtkWidget *view_output_button;

    rename_button =
      glade_xml_get_widget(glade_xml, "rename_button");
    jump_button =
      glade_xml_get_widget(glade_xml, "jump_button");
    display_ceos_button =
      glade_xml_get_widget(glade_xml, "display_ceos_button");
    display_asf_button =
      glade_xml_get_widget(glade_xml, "display_asf_button");
    view_output_button =
      glade_xml_get_widget(glade_xml, "view_output_button");

    gtk_widget_set_sensitive(view_output_button, TRUE);
    gtk_widget_set_sensitive(view_output_button, TRUE);
    gtk_widget_set_sensitive(view_output_button, enable_view_output);
    gtk_widget_set_sensitive(display_asf_button, enable_display_asf_metadata);
    gtk_widget_set_sensitive(display_ceos_button, enable_display_ceos_metadata);
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

        enable = n == popup_menu_item_remove || n == popup_menu_item_process;
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
    GtkWidget *jump_button;
    GtkWidget *display_ceos_button;
    GtkWidget *display_asf_button;
    GtkWidget *view_output_button;

    rename_button =
      glade_xml_get_widget(glade_xml, "rename_button");
    jump_button =
      glade_xml_get_widget(glade_xml, "jump_button");
    display_ceos_button =
      glade_xml_get_widget(glade_xml, "display_ceos_button");
    display_asf_button =
      glade_xml_get_widget(glade_xml, "display_asf_button");
    view_output_button =
      glade_xml_get_widget(glade_xml, "view_output_button");

    gtk_widget_set_sensitive(rename_button, FALSE);
    gtk_widget_set_sensitive(jump_button, FALSE);
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

gint
popup_handler(GtkWidget *widget, GdkEvent *event)
{
    LSL;

    GtkMenu *menu;
    GdkEventButton *event_button;
    GtkTreeSelection *selection;
    GtkWidget *files_list;
    GtkTreeIter iter;

    g_return_val_if_fail(widget != NULL, FALSE);
    g_return_val_if_fail(GTK_IS_MENU(widget), FALSE);
    g_return_val_if_fail(event != NULL, FALSE);

    files_list = glade_xml_get_widget(glade_xml, "files_list");
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
                gchar * status,
                    * out_name,
                    * in_name,
                    * asf_meta_name,
                    * ceos_meta_name;

                gboolean show_view_output_menu_item = FALSE,
                    show_display_ceos_metadata_menu_item,
                    show_display_asf_metadata_menu_item;

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
                    LSU;
                    return FALSE;
                }

                /* check if we should disable "View Output" */
                if (strcmp(status, "Done") == 0)
                {
                    Settings * s = settings_get_from_gui();
                    if (settings_get_output_format_can_be_thumbnailed(s))
                    {
                        show_view_output_menu_item = TRUE;
                    }
                }

                /* check if we should disable "Display CEOS Metadata" */
                ceos_meta_name = build_ceos_metadata_filename(in_name);
                show_display_ceos_metadata_menu_item =
                    g_file_test(ceos_meta_name, G_FILE_TEST_EXISTS);

                /* check if we should disable "Display ASF Metadata" */
                asf_meta_name = build_asf_metadata_filename(out_name);
                show_display_asf_metadata_menu_item =
                    g_file_test(asf_meta_name, G_FILE_TEST_EXISTS);

                /* enable/disable the items */
                enable_menu_items(menu, show_view_output_menu_item,
                    show_display_ceos_metadata_menu_item,
                    show_display_asf_metadata_menu_item);

		//enable_toolbar_buttons(show_view_output_menu_item,
                //    show_display_ceos_metadata_menu_item,
                //    show_display_asf_metadata_menu_item);

                g_free(ceos_meta_name);
                g_free(asf_meta_name);
            }
            else
            {
                /* nothing selected, and nothing was under mouse when
                clicked */
                LSU;
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
        LSU;
        return TRUE;
    }
    LSU;
    return FALSE;
}

static gboolean confirm_overwrite()
{
    gboolean ret = TRUE;

    GtkWidget * dialog_confirm_overwrite;
    gint result;

    dialog_confirm_overwrite =
        glade_xml_get_widget(glade_xml, "dialog_confirm_overwrite");

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
handle_remove()
{
    LSL;

    GtkWidget *files_list;
    GtkTreeModel * model;
    GtkTreeSelection *selection;
    GList * selected_rows, * i;
    GList * refs;

    files_list = glade_xml_get_widget(glade_xml, "files_list");
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list));
    model = GTK_TREE_MODEL(list_store);

    selected_rows = gtk_tree_selection_get_selected_rows(
        selection, &model);

    refs = NULL;
    i = selected_rows;

    if (!selected_rows)
    {
        LSU;
	show_please_select_message();
	return FALSE;
    }

    while (i)
    {
        GtkTreePath * path;
        GtkTreeRowReference * ref;

        path = (GtkTreePath *) i->data;
        ref = gtk_tree_row_reference_new(GTK_TREE_MODEL(list_store), path);

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
        gtk_tree_model_get_iter(GTK_TREE_MODEL(list_store), &iter, path);
        gtk_list_store_remove(GTK_LIST_STORE(model), &iter);

        i = g_list_next(i);
    }

    g_list_foreach(selected_rows, (GFunc)gtk_tree_path_free, NULL);
    g_list_free(selected_rows);

    g_list_foreach(refs, (GFunc)gtk_tree_row_reference_free, NULL);
    g_list_free(refs);

    LSU;
    return TRUE;
}

gboolean
get_iter_to_first_selected_row(GtkWidget * files_list, GtkTreeIter * iter)
{
    GList * selected_rows;
    GtkTreeModel *model;
    GtkTreeSelection *selection;
    gboolean found;

    LSL;

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

    LSU;
    return found;
}

static int
handle_jump()
{
    LSL;

    GtkWidget *files_list;
    GtkWidget * textview_output;
    GtkTreeIter iter;

    textview_output = glade_xml_get_widget(glade_xml, "textview_output");
    files_list = glade_xml_get_widget(glade_xml, "files_list");

    if (get_iter_to_first_selected_row(files_list, &iter))
    {
        gchar * in_data;
        GtkTextMark * mark;
        GtkTextBuffer * text_buffer;

        gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter, 
            COL_DATA_FILE, &in_data, -1);

        text_buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(textview_output));
        mark = gtk_text_buffer_get_mark(text_buffer, in_data);

        if (mark)
        {
            gtk_text_view_scroll_to_mark(GTK_TEXT_VIEW(textview_output), 
                mark, 0, TRUE, 0, 0);
        }
        else
        {
            message_box("No log information available for that file.");
        }
    }
    else
    {
        show_please_select_message();
    }

    LSU;
    return TRUE;
}

static int
handle_display_ceos_metadata()
{
    LSL;
    GtkWidget *files_list;
    GtkTreeIter iter;

    files_list = glade_xml_get_widget(glade_xml, "files_list");

    if (get_iter_to_first_selected_row(files_list, &iter))
    {
        gchar * out_name;

        gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter, 
            COL_OUTPUT_FILE, &out_name, -1);

        show_ceos_meta_data(out_name);
    }
    else
    {
        show_please_select_message();
    }

    LSU;
    return TRUE;
}

static int
handle_display_asf_metadata()
{
    LSL;
    GtkWidget *files_list;
    GtkTreeIter iter;

    files_list = glade_xml_get_widget(glade_xml, "files_list");

    if (get_iter_to_first_selected_row(files_list, &iter))
    {
        gchar * out_name;

        gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter, 
            COL_OUTPUT_FILE, &out_name, -1);

        show_asf_meta_data(out_name);
    }
    else
    {
        show_please_select_message();
    }

    LSU;
    return TRUE;
}

static int
handle_process()
{
    LSL;
    GtkWidget *files_list;
    GtkTreeIter iter;
    GtkTreeModel * model;
    GtkTreeSelection * selection;
    GList * selected_rows, * i;
    GList * refs;
    gboolean confirm_needed = FALSE;

    /* gui should prevent this from happening */
    if (processing) {
        LSU;
        return TRUE;
    }

    files_list = glade_xml_get_widget(glade_xml, "files_list");
    selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list));
    model = GTK_TREE_MODEL(list_store);

    selected_rows = gtk_tree_selection_get_selected_rows(selection, &model);

    if (!selected_rows)
    {
        LSU;
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

    LSU;
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
    LSL;

    GtkWidget *files_list;
    GtkTreeIter iter;

    files_list = glade_xml_get_widget(glade_xml, "files_list");

    if (get_iter_to_first_selected_row(files_list, &iter))
    {
        gchar * out_name;

        gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter, 
            COL_OUTPUT_FILE, &out_name, -1);

	if (g_file_test(out_name, G_FILE_TEST_EXISTS))
	{
	    show_output_image(out_name);
	}
	else
	{
	    char msg[2048];
	    sprintf(msg, "The output image file was not found:\n"
		         "   %s\n", out_name);
	    message_box(msg);
	}
    }
    else
    {
        show_please_select_message();
    }

    LSU;
    return TRUE;
}

SIGNAL_CALLBACK void
on_remove_button_clicked(GtkWidget *widget)
{
  handle_remove();
}

SIGNAL_CALLBACK void
on_process_button_clicked(GtkWidget *widget)
{
  handle_process();
}

SIGNAL_CALLBACK void
on_rename_button_clicked(GtkWidget *widget)
{
  handle_rename();
}

SIGNAL_CALLBACK void
on_jump_button_clicked(GtkWidget *widget)
{
  handle_jump();
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

SIGNAL_CALLBACK gint
popup_menu_jump(GtkWidget *widget, GdkEvent *event)
{
  return handle_jump();
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
popup_menu_process(GtkWidget *widget, GdkEvent *event)
{
  return handle_process();
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

void
setup_popup_menu()
{
    GtkWidget *menu, *widget, *item;

    /* if they right click in the files list, we'll pop up */
    widget = glade_xml_get_widget(glade_xml, "files_list");

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

    item = gtk_menu_item_new_with_label("Jump To Log");
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );  
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_jump), NULL);
    gtk_widget_show(item);

    item = gtk_menu_item_new_with_label("Display CEOS Metadata");
    gtk_menu_shell_append( GTK_MENU_SHELL(menu), item );  
    g_signal_connect_swapped(G_OBJECT(item), "activate",
        G_CALLBACK(popup_menu_ceos_metadata), NULL);
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

    gtk_widget_show(menu);

    g_signal_connect_swapped(widget, "button_press_event",
        G_CALLBACK(popup_handler), menu);
    g_signal_connect_swapped(widget, "popup_menu",
        G_CALLBACK(popup_handler), menu);
}
