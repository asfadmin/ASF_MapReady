#include <unistd.h>
#include <strings.h>

#include "asf_convert_gui.h"
#include "ceos_thumbnail.h"
#include "asf.h"
#include "get_ceos_names.h"

int COL_DATA_FILE;
int COL_INPUT_THUMBNAIL;
int COL_BAND_LIST;
int COL_OUTPUT_FILE;
int COL_STATUS;
int COL_LOG;

int COMP_COL_DATA_FILE;
int COMP_COL_OUTPUT_FILE;
int COMP_COL_OUTPUT_THUMBNAIL;
int COMP_COL_OUTPUT_THUMBNAIL_BIG;
int COMP_COL_STATUS;
int COMP_COL_LOG;

/* Returns the length of the prepension if there is an allowed
   prepension, otherwise returns 0 (no prepension -> chceck extensions) */
int has_prepension(const gchar * data_file_name)
{
    /* at the moment, the only prepension we allow is LED- (ALOS) */
    char *basename = get_basename(data_file_name);
    int ret = strncmp(basename, "LED-", 4) == 0;
    free(basename);
    return ret ? 4 : 0;
}

static gchar *
determine_default_output_file_name(const gchar * data_file_name)
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
        printf("Filename: %s\n", filename);
    } else { 
        basename = g_strdup(data_file_name);
        p = findExt(basename);
        if (p)
            *p = '\0';

        filename = g_path_get_basename(basename);
    }

    schemed_filename = naming_scheme_apply(current_naming_scheme, filename);

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

gboolean is_L_file(const gchar * data_file)
{
    char *p = findExt(data_file);

    if (!p)
        return FALSE;
    else
        return strcmp_case(p, "L") == 0;
}

static gboolean file_is_valid(const gchar * data_file)
{
    /* not sure how much error checking we want to do */

    /* for now, just ensure that the extension is ok */
    /* don't even look at the actual file itself... */

    /* prepension check first */
    int has_alos_prepension = has_prepension(data_file);

    if (has_alos_prepension) {
        /* this is a file that uses prepending */
        return TRUE;
    }

    gchar * p;

    p = findExt(data_file);

    if (!p)
    {
        /* needs to have an extension */
        return FALSE;
    }
    else
    {
        ++p;
        if (strcmp_case(p, "D") == 0 ||
            /*strcmp_case(p, "img") == 0 ||*/
            /*strcmp_case(p, "L") == 0 ||*/
            /*strcmp_case(p, "meta") == 0 ||*/
            strcmp_case(p, "raw") == 0 ||
            strcmp_case(p, "000") == 0)
        {
            return TRUE;
        }
        else
        {
            return FALSE;
        }
    }
}

#ifdef THUMBNAILS

static void set_input_image_thumbnail(GtkTreeIter *iter, 
                                      const gchar *metadata_file,
                                      const gchar *data_file)
{
    GdkPixbuf *pb = make_input_image_thumbnail_pixbuf (
        metadata_file, data_file, THUMB_SIZE);

    if (pb)
        gtk_list_store_set (list_store, iter, COL_INPUT_THUMBNAIL, pb, -1);
}

static void
do_thumbnail (const gchar *file)
{
    gchar *metadata_file = meta_file_name (file);
    if (metadata_file && strlen(metadata_file) > 0) {

        /* Find the element of the list store having the file name we are
           trying to add a thumbnail of.  */
        GtkTreeIter iter;
        gboolean valid;
        /* Get the first iter in the list */
        valid = gtk_tree_model_get_iter_first (GTK_TREE_MODEL (list_store), 
                                               &iter);
        while ( valid ) {
            /* Walk through the list, reading each row */
            gchar *data_file;
            
            gtk_tree_model_get (GTK_TREE_MODEL (list_store), &iter, 
                                COL_DATA_FILE, &data_file, -1);
            
            if ( strcmp (data_file, file) == 0 ) {
                /* We found it, so load the thumbnail.  */
                set_input_image_thumbnail (&iter, metadata_file, data_file);
                g_free (metadata_file);
                return;
            }
            
            valid = gtk_tree_model_iter_next (GTK_TREE_MODEL (list_store),
                                              &iter);
        }

    /* The data file must have gotten removed from the list before we
    got a chance to draw it's thumbnail.  Oh well.  */

        g_free (metadata_file);
    }
}

#endif

static char *build_band_list(const char *file)
{
    // this only applies to ALOS data -- other types we'll just return "-"
    if (has_prepension(file))
    {
        int ii,nBands;
        char *ret;

        char **dataName = MALLOC(sizeof(char*)*MAX_BANDS);
        for (ii=0; ii<MAX_BANDS; ++ii)
            dataName[nBands] = MALLOC(sizeof(char)*255);
        
        get_ceos_data_name(file, dataName, &nBands);
        
        if (nBands <= 1) {
            // not multiband
            ret = STRDUP("-");
        }
        else {
            // 8 characters per band, plus ", " means 10 characters allocated per band
            char *ret = MALLOC(sizeof(char)*(nBands*10+2));

            // kludge: assume that band names come between 1st & 2nd dashes (-)
            for (ii=0; ii<nBands; ++ii) {
                char *basename = get_basename(dataName[ii]);
                
                // point to first -, or the start of the string
                char *p = strchr(basename, '-');
                if (!p) p = basename;

                // point to second -, or the end of the string
                char *q = strchr(p + 1, '-');
                if (!q) q = basename + strlen(basename);

                // if more than 8 characters, just use the first 8.
                if (q-p > 8) q = p + 8;
                *q = '\0';

                if (ii==0)
                    strcpy(ret, p);
                else
                    strcat(ret, p);
                
                if (ii < nBands - 1)
                    strcat(ret, ", ");

                FREE(basename);
            }
        }

        FREE_BANDS(dataName);
        return ret;
    }
    else
    {
        return STRDUP("-");
    }
}

gboolean
add_to_files_list(const gchar * data_file)
{
    GtkTreeIter iter;
    gboolean ret = add_to_files_list_iter(data_file, &iter);
    return ret;
}

void
move_to_completed_files_list(GtkTreeIter *iter, GtkTreeIter *completed_iter,
                             const gchar *log_txt)
{
    // iter: points into "files_list"
    // completed_iter: (returned) points into "completed_files_list"    
    gchar *output_file, *data_file;

    GtkTreeModel *model = GTK_TREE_MODEL(list_store);
    gtk_tree_model_get(model, iter, COL_DATA_FILE, &data_file,
                       COL_OUTPUT_FILE, &output_file, -1);

    gtk_list_store_append(completed_list_store, completed_iter);
    gtk_list_store_set(completed_list_store, completed_iter,
                       COMP_COL_DATA_FILE, data_file,
                       COMP_COL_OUTPUT_FILE, output_file,
                       COMP_COL_STATUS, "Done",
                       COMP_COL_LOG, log_txt,
                       -1);

    gtk_list_store_remove(GTK_LIST_STORE(model), iter);

    g_free(data_file);
    g_free(output_file);
}

void
move_from_completed_files_list(GtkTreeIter *iter)
{
    gchar *data_file;
    GtkTreeModel *model = GTK_TREE_MODEL(completed_list_store);
    gtk_tree_model_get(model, iter, COL_DATA_FILE, &data_file, -1);

    add_to_files_list(data_file);
    gtk_list_store_remove(GTK_LIST_STORE(model), iter);
    g_free(data_file);
}

// The thumbnailing works like this: When a user adds a file, or a bunch of
// files, the file names are added immediately to the list, and each data
// file name is added to this global thumbnailing queue.  (It isn't really
// a queue, it is just an array of char* pointers.)  After all the files have
// been added to the list, and queue_thumbnail has been called for each one,
// we call show_queued_thumbnails(), which runs through the list of saved
// names, and populates the thumbnails for each.  So it appears as though
// we are still threading but everything occurs sequentially.  The only
// tricky thing is that show_queued_thumbnails periodically processes the
// gtk events, to keep the app from seeming unresponsive.  This isn't a
// problem, except that one possible event is removing files that were
// added, another is adding even more files, which will queue up more
// thumbnails, which means show_queued_thumbnails can recurse.  This isn't
// really a problem if we keep in mind that the gtk_main_iteration() call
// in show_queued_thumbnails() can result in the thumb_files[] array
// changing on us (sort of as if we were doing threading).

#define QUEUE_SIZE 255
static char *thumb_files[QUEUE_SIZE];
static void queue_thumbnail(const gchar * data_file)
{
    int i;
    for (i = 0; i < QUEUE_SIZE; ++i) {
        if (!thumb_files[i]) {
            thumb_files[i] = g_strdup(data_file);
            break;
        }
    }
}

void
show_queued_thumbnails()
{
    int i;
    for (i = 0; i < QUEUE_SIZE; ++i) {
        if (thumb_files[i]) {
            // do a gtk main loop iteration
            while (gtk_events_pending())
                gtk_main_iteration();    
            
            // must check files[i]!=NULL again, since gtk_main_iteration could
            // have processed a "Browse..." event and thus already processed 
            // this item.  The alternative would be to move the above while()
            // loop below the statements below, however that makes the app feel
            // a little less responsive.
            if (thumb_files[i]) {
                do_thumbnail(thumb_files[i]);
                
                g_free(thumb_files[i]);
                thumb_files[i] = NULL;
            }
        }
    }
}

gboolean
add_to_files_list_iter(const gchar * data_file, GtkTreeIter *iter_p)
{
    gboolean valid = file_is_valid(data_file);

    if (valid)
    {
        GtkWidget *files_list;
        gchar * out_name_full;
        
        files_list = get_widget_checked("files_list");
        char *bands = build_band_list(data_file);

        gtk_list_store_append(list_store, iter_p);
        gtk_list_store_set(list_store, iter_p,
                           COL_DATA_FILE, data_file,
                           COL_BAND_LIST, bands,
                           COL_STATUS, "-",
                           COL_LOG, "Has not been processed yet.",
                           -1);
        
        out_name_full = determine_default_output_file_name(data_file);
        set_output_name(iter_p, out_name_full);
        g_free(out_name_full);
        FREE(bands);

        queue_thumbnail(data_file);
        
        /* Select the file automatically if this is the first
           file that was added (this makes the toolbar buttons
           immediately useful)                                 */
        if (1 == gtk_tree_model_iter_n_children(GTK_TREE_MODEL(list_store), 
                                                NULL))
        {
            GtkTreeSelection *selection =
                gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list));
            gtk_tree_selection_select_all(selection);
        }
    }

    return valid;
}

void
update_all_extensions()
{
    Settings * user_settings;
    const gchar * ext;
    gboolean ok;
    GtkTreeIter iter;

    if (list_store)
    {
        user_settings = settings_get_from_gui();
        ext = settings_get_output_format_extension(user_settings);

        ok = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(list_store), &iter);
        while (ok)
        {
            gchar * current_output_name;
            gchar * new_output_name;
            gchar * basename;
            gchar * p;

            gtk_tree_model_get(GTK_TREE_MODEL(list_store), &iter, 
                COL_OUTPUT_FILE, &current_output_name, -1);

            basename = g_strdup(current_output_name);
            p = strrchr(basename, '.');
            if (p)
                *p = '\0';

            new_output_name = 
                (gchar *) g_malloc(sizeof(gchar) * (strlen(basename) +
                strlen(ext) + 2));

            g_sprintf(new_output_name, "%s.%s", basename, ext);

            set_output_name(&iter, new_output_name);      

            g_free(basename);
            g_free(new_output_name);
            g_free(current_output_name);

            ok = gtk_tree_model_iter_next(GTK_TREE_MODEL(list_store), &iter);
        }

        settings_delete(user_settings);
    }
}

void
edited_handler(GtkCellRendererText *ce, gchar *arg1, gchar *arg2, 
               gpointer user_data)
{
    /* arg1 indicates which row -- should assert() that it matches
    the selected row, since we're asssuming that */
    do_rename_selected(arg2);
}

void render_status(GtkTreeViewColumn *tree_column,
                   GtkCellRenderer *cell,
                   GtkTreeModel *tree_model,
                   GtkTreeIter *iter,
                   gpointer data)
{
    gchar *status;
    gboolean done;
    gboolean settings_are_stale = FALSE;

    gtk_tree_model_get (tree_model, iter, COL_STATUS, &status, -1);
    done = strcmp(status, "Done") == 0;

    if (done && settings_on_execute)
    {
        Settings * user_settings =
            settings_get_from_gui();

        settings_are_stale =
            !settings_equal(user_settings, settings_on_execute);

        settings_delete(user_settings);
    }

    if (done && settings_are_stale)
    {
        GdkColor c;

        c.red = c.green = c.blue = 32768;

        g_object_set( G_OBJECT (cell), "foreground-gdk", &c, NULL);
    }
    else
    {
        g_object_set( G_OBJECT (cell), "foreground-gdk", NULL, NULL);
    }

    g_object_set (G_OBJECT (cell), "text", status, NULL);
    g_free(status);
}

void render_output_name(GtkTreeViewColumn *tree_column,
                        GtkCellRenderer *cell,
                        GtkTreeModel *tree_model,
                        GtkTreeIter *iter,
                        gpointer data)
{
    gchar *output_file;
    gchar *status;
    gboolean done;
    gboolean processing;

    gtk_tree_model_get (tree_model, iter, 
        COL_OUTPUT_FILE, &output_file, 
        COL_STATUS, &status, -1);

    /* Do not mark the file in red if the item has been marked "Done"
    However, if the user has changed the settings, the "Done"
    marks are stale... so in that case do not look at "Done" */

    done = strcmp("Done", status) == 0;
    processing = strcmp("Processing...", status) == 0;

    if (done && settings_on_execute)
    {
        Settings * user_settings =
            settings_get_from_gui();

        if (!settings_equal(user_settings, settings_on_execute))
            done = FALSE;

        settings_delete(user_settings);
    }

    if (!processing && !done &&
        g_file_test(output_file, G_FILE_TEST_EXISTS))
    {
        GdkColor c;

        c.red = 65535;
        c.green = c.blue = 0;

        g_object_set( G_OBJECT (cell), "foreground-gdk", &c, NULL);
    }
    else
    {
        g_object_set( G_OBJECT (cell), "foreground-gdk", NULL, NULL);
    }

    g_object_set (G_OBJECT (cell), "text", output_file, NULL);

    g_free(output_file);
    g_free(status);
}

void
setup_files_list(int argc, char *argv[])
{
    GtkTreeViewColumn *col;
    GtkCellRenderer *renderer;
    GValue val = {0,};

    list_store = gtk_list_store_new(6, 
                                    G_TYPE_STRING, 
                                    GDK_TYPE_PIXBUF,
                                    G_TYPE_STRING, 
                                    G_TYPE_STRING, 
                                    G_TYPE_STRING,
                                    G_TYPE_STRING);
    
    COL_DATA_FILE = 0;
    COL_INPUT_THUMBNAIL = 1;
    COL_BAND_LIST = 2;
    COL_OUTPUT_FILE = 3;
    COL_STATUS = 4;
    COL_LOG = 5;

    completed_list_store = gtk_list_store_new(6,
                                              G_TYPE_STRING, 
                                              G_TYPE_STRING, 
                                              GDK_TYPE_PIXBUF,
                                              GDK_TYPE_PIXBUF,
                                              G_TYPE_STRING,
                                              G_TYPE_STRING);
    
    COMP_COL_DATA_FILE = 0;
    COMP_COL_OUTPUT_FILE = 1;
    COMP_COL_OUTPUT_THUMBNAIL = 2;
    COMP_COL_OUTPUT_THUMBNAIL_BIG = 3;
    COMP_COL_STATUS = 4;
    COMP_COL_LOG = 5;

    int i;
    for (i = 1; i < argc; ++i)
        add_to_files_list(argv[i]);
    show_queued_thumbnails();

/*** First, the "pending" files list ****/
    GtkWidget *files_list = get_widget_checked("files_list");

    /* First Column: Input File Name */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Data File");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    g_object_set(renderer, "text", "?", NULL);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COL_DATA_FILE);

    /* Next Column: thumbnail of input image.  */
    col = gtk_tree_view_column_new ();
    gtk_tree_view_column_set_title (col, "Input Thumbnail");
    gtk_tree_view_column_set_resizable (col, FALSE);
    gtk_tree_view_append_column (GTK_TREE_VIEW (files_list), col);
    renderer = gtk_cell_renderer_pixbuf_new ();
    gtk_tree_view_column_pack_start (col, renderer, FALSE);
    gtk_tree_view_column_add_attribute (col, renderer, "pixbuf",
                                        COL_INPUT_THUMBNAIL);

    g_signal_connect (files_list, "motion-notify-event",
                      G_CALLBACK (files_list_motion_notify_event_handler), 
                      NULL);
    
    g_signal_connect (files_list, "leave-notify-event",
                      G_CALLBACK (files_list_leave_notify_event_handler), 
                      files_list);
    
    g_signal_connect (files_list, "scroll-event",
                      G_CALLBACK (files_list_scroll_event_handler), NULL);

    /* Next Column: Band List */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Bands");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COL_BAND_LIST);

    /* Next Column: Output File Name */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Output File");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();

    /* allow editing the output filename right in the grid */
    g_value_init(&val, G_TYPE_BOOLEAN);
    g_value_set_boolean(&val, TRUE);
    g_object_set_property(G_OBJECT(renderer), "editable", &val);

    /* connect "editing-done" signal */
    g_signal_connect(G_OBJECT(renderer), "edited",
        G_CALLBACK(edited_handler), NULL);

    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COL_OUTPUT_FILE);

    /* add our custom renderer (turns existing files red) */
    gtk_tree_view_column_set_cell_data_func(col, renderer,
        render_output_name, NULL, NULL);

    /* Next Column: Current Status */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Status");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COL_STATUS);

    /* Next Column: Log Info (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Log Info");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COL_LOG);

    gtk_tree_view_set_model(GTK_TREE_VIEW(files_list), 
        GTK_TREE_MODEL(list_store));  

    g_object_unref(list_store);

    gtk_tree_selection_set_mode(
        gtk_tree_view_get_selection(GTK_TREE_VIEW(files_list)),
        GTK_SELECTION_MULTIPLE);

/*** Second, the "completed" files list ****/
    GtkWidget *completed_files_list =
        get_widget_checked("completed_files_list");

    /* First Column: Input File Name */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Data File");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    g_object_set(renderer, "text", "?", NULL);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_DATA_FILE);

    /* Next Column: Output File Name */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Output File");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text",
                                       COMP_COL_OUTPUT_FILE);

    /* Next Column: Pixbuf of output image */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Output Thumbnail");
    gtk_tree_view_column_set_resizable(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_pixbuf_new();
    gtk_tree_view_column_pack_start(col, renderer, FALSE);
    gtk_tree_view_column_add_attribute(col, renderer, "pixbuf",
                                       COMP_COL_OUTPUT_THUMBNAIL);

    g_signal_connect (completed_files_list, "motion-notify-event",
                G_CALLBACK (completed_files_list_motion_notify_event_handler), 
                NULL);
    
    g_signal_connect (completed_files_list, "leave-notify-event",
                G_CALLBACK (completed_files_list_leave_notify_event_handler), 
                completed_files_list);
    
    g_signal_connect (completed_files_list, "scroll-event",
                G_CALLBACK (completed_files_list_scroll_event_handler),
                NULL);

    /* Next Column: Pixbuf of output image (larger version, for popup) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "");
    gtk_tree_view_column_set_resizable(col, FALSE);
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_pixbuf_new();
    gtk_tree_view_column_pack_start(col, renderer, FALSE);
    gtk_tree_view_column_add_attribute(col, renderer, "pixbuf",
                                       COMP_COL_OUTPUT_THUMBNAIL_BIG);

    /* Next Column: Current Status */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Status");
    gtk_tree_view_column_set_resizable(col, TRUE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COMP_COL_STATUS);

    /* Next Column: Log Info (hidden) */
    col = gtk_tree_view_column_new();
    gtk_tree_view_column_set_title(col, "Log Info");
    gtk_tree_view_column_set_visible(col, FALSE);
    gtk_tree_view_append_column(GTK_TREE_VIEW(completed_files_list), col);
    renderer = gtk_cell_renderer_text_new();
    gtk_tree_view_column_pack_start(col, renderer, TRUE);
    gtk_tree_view_column_add_attribute(col, renderer, "text", COMP_COL_LOG);

    gtk_tree_view_set_model(GTK_TREE_VIEW(completed_files_list), 
        GTK_TREE_MODEL(completed_list_store));  

    g_object_unref(completed_list_store);

    gtk_tree_selection_set_mode(
        gtk_tree_view_get_selection(GTK_TREE_VIEW(completed_files_list)),
        GTK_SELECTION_MULTIPLE);
}

void
set_output_name(GtkTreeIter *iter, const gchar *name)
{
    gtk_list_store_set(list_store, iter, COL_OUTPUT_FILE, name, -1);
}
