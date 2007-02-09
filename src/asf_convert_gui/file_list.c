#include <unistd.h>
#include <strings.h>

#include "asf_convert_gui.h"
#include "ceos_thumbnail.h"
#include "asf.h"

int COL_DATA_FILE;
int COL_INPUT_THUMBNAIL;
int COL_OUTPUT_FILE;
int COL_STATUS;
int COL_LOG;

int COMP_COL_DATA_FILE;
int COMP_COL_OUTPUT_FILE;
int COMP_COL_OUTPUT_THUMBNAIL;
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

    printf("Moving back: %s\n", data_file);
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
        printf("Adding: %s\n", data_file);

        GtkWidget *files_list;
        gchar * out_name_full;
        
        files_list = get_widget_checked("files_list");
        
        gtk_list_store_append(list_store, iter_p);
        gtk_list_store_set(list_store, iter_p,
                           COL_DATA_FILE, data_file,
                           COL_STATUS, "-",
                           COL_LOG, "Has not been processed yet.",
                           -1);
        
        out_name_full = determine_default_output_file_name(data_file);
        set_output_name(iter_p, out_name_full);
        g_free(out_name_full);
        
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

#ifdef THUMBNAILS

static GtkTreePath *
thumbnail_path (GtkWidget *widget, GdkEventMotion *event)
{
    GtkTreePath *path;
    gboolean row_exists = gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (widget),
        event->x,
        event->y,
        &path,
        NULL,
        NULL,
        NULL);
    if ( row_exists ) {
        return path;
    }
    else {
        return NULL;
    }
}

static GtkTreeViewColumn *
input_thumbnail_column (GtkWidget *widget, GdkEventMotion *event)
{
    return gtk_tree_view_get_column (GTK_TREE_VIEW (widget), 
        COL_INPUT_THUMBNAIL);
}

static GtkTreeViewColumn *
output_thumbnail_column (GtkWidget *widget, GdkEventMotion *event)
{
    return gtk_tree_view_get_column (GTK_TREE_VIEW (widget), 
        COMP_COL_OUTPUT_THUMBNAIL);
}

static gboolean
in_input_thumbnail (GtkWidget *widget, GdkEventMotion *event)
{
    g_assert (GTK_IS_TREE_VIEW (widget));

    gboolean result = FALSE;	/* Result to be returned. */

    GtkTreePath *tp = thumbnail_path (widget, event);

    if ( tp == NULL ) {
        return FALSE;		/* Pointer is not over a filled in row.  */
    }

    /* Check if we are over the input image thumbnail.  */
    GtkTreeViewColumn *itc = input_thumbnail_column (widget, event);
    GdkRectangle itnc_rect;	/* Input thumbnail cell rectangle.  */
    gtk_tree_view_get_cell_area (GTK_TREE_VIEW (widget), tp, itc, &itnc_rect);
    /* Here we depend on the fact that the input thumbnail is packed at
    the beginning of the cell horizontally, and centered in the cell
    vertically (FIXME: find a way to verify this with
    assertions).  */
    GdkRectangle itn_rect;	/* Input thumbnail rectangle.  */
    /* FIXME: fix this border hackery to be precise somehow.  */
    itn_rect.x = itnc_rect.x + 1;	/* There is probably a small border so +1.  */
    itn_rect.y = itnc_rect.y + 1;
    itn_rect.width = THUMB_SIZE;
    itn_rect.height = THUMB_SIZE;
    GdkRegion *itn_region = gdk_region_rectangle (&itn_rect);
    if ( gdk_region_point_in (itn_region, (int) event->x, (int) event->y) ) {
        result = TRUE;
        //    g_message ("Over input thumbnail!");
    }
    gdk_region_destroy (itn_region);

    return result;
}

static gboolean
in_output_thumbnail (GtkWidget *widget, GdkEventMotion *event)
{
    g_assert (GTK_IS_TREE_VIEW (widget));

    gboolean result = FALSE;	/* Result to be returned. */

    GtkTreePath *tp = thumbnail_path (widget, event);

    if ( tp == NULL ) {
        return FALSE;		/* Pointer is not over a filled in row.  */
    }

    /* Check if we are over the output image thumbnail.  */
    GtkTreeViewColumn *otc = output_thumbnail_column (widget, event);
    GdkRectangle otnc_rect;	/* Output thumbnail cell rectangle.  */
    gtk_tree_view_get_cell_area (GTK_TREE_VIEW (widget), tp, otc, &otnc_rect);
    /* Here we depend on the fact that the output thumbnail is packed at
    the beginning of the cell horizontally, and centered in the cell
    vertically (FIXME: find a way to verify this with assertions).  */
    GdkRectangle otn_rect;	/* Input thumbnail rectangle.  */
    /* FIXME: fix this border hackery to be precise.  */
    otn_rect.x = otnc_rect.x + 2;	/* There is probably a small border so +2.  */
    otn_rect.y = otnc_rect.y + 2;
    otn_rect.width = THUMB_SIZE;
    otn_rect.height = THUMB_SIZE;
    GdkRegion *otn_region = gdk_region_rectangle (&otn_rect);
    if ( gdk_region_point_in (otn_region, (int) event->x, (int) event->y) ) {
        result = TRUE;
    }
    gdk_region_destroy (otn_region);

    return result;
}

static gboolean
in_thumbnail (GtkWidget *widget, GdkEventMotion *event)
{
    g_assert (GTK_IS_TREE_VIEW (widget));

    return (in_input_thumbnail (widget, event) 
        || in_output_thumbnail (widget, event));
}

/* Get a new thumbnail region of GtkTreeView widget in which event
falls.  It is an error to call this function if !in_thumbnail
(widget, event).  */
static GdkRegion *
thumbnail_region (GtkWidget *widget, GdkEventMotion *event)
{
    g_assert (GTK_IS_TREE_VIEW (widget));

    g_assert (in_thumbnail (widget, event));

    GtkTreePath *tp = thumbnail_path (widget, event);
    g_assert (tp != NULL);

    /* Rectangle of region to be returned.  */
    GdkRectangle tn_rect;

    /* If over the input image thumbnail, return the input thumbnail region, */
    GtkTreeViewColumn *tc = NULL;	/* Thumbnail column we are over.  */
    if ( in_input_thumbnail (widget, event) ) {
        tc = input_thumbnail_column (widget, event);
    }
    /* otherwise, we better be over the output thumbnail region.  */
    else if ( in_output_thumbnail (widget, event) ) {
        tc = output_thumbnail_column (widget, event);
    }

    gtk_tree_view_get_cell_area (GTK_TREE_VIEW (widget), tp, tc, &tn_rect);

    /* Here we depend on the fact that the thumbnails are packed at the
    beginning of the cell horizontally, and centered in the cell
    vertically (FIXME: find a way to verify this with assertions).  */
    GdkRectangle itn_rect;		/* Image thumbnail rectangle.  */
    /* FIXME: fix this border hackery to be precise somehow.  */
    itn_rect.x = tn_rect.x + 1;	/* There is probably a small border so +1.  */
    itn_rect.y = tn_rect.y + 1;
    itn_rect.width = THUMB_SIZE;
    itn_rect.height = THUMB_SIZE;

    return gdk_region_rectangle (&itn_rect);
}

/* Returns a reference to the thumbnail column of view in which event
falls.  It is an error to call this routine if the event doesn't
fall in one of the thumbnail columns.  */
static GtkTreeViewColumn * 
thumbnail_column (GtkWidget *widget, GdkEventMotion *event)
{
    g_assert (in_thumbnail (widget, event));

    GtkTreeViewColumn *result;
    gboolean row_exists 
        = gtk_tree_view_get_path_at_pos (GTK_TREE_VIEW (widget),
        event->x,
        event->y,
        NULL,
        &result,
        NULL,
        NULL);
    g_assert (row_exists);

    return result;
}

/* Arguments for the fake motion signal callback.  */
typedef struct {
    gboolean is_valid;
    GtkWidget *widget;
    GdkEventMotion *event;
    gdouble x, y;			/* x and y of fake event.  */
} fake_motion_signal_args_t;

static gboolean
emit_fake_motion_signal (fake_motion_signal_args_t *args)
{
    /* Unpack the arguments which had to be packed into a single
    structure.  */
    GtkWidget *widget = args->widget;
    g_assert (GTK_IS_TREE_VIEW (widget));
    GdkEventMotion *event = args->event;

    //g_message ("Doing %s, event = %p", __func__, event);

    if ( args->is_valid ) {
        /* We don't care about the return value of the signal we are emitting.  */
        gboolean junk;	
        g_signal_emit_by_name (widget, "motion-notify-event", event, &junk);
    }    

    g_free (args);

    return FALSE;
}

static GdkWindow *
draw_popup_image (GtkWidget *widget, GtkTreePath *path, 
                  GtkTreeViewColumn *column, GdkRegion *tr)
{
    g_assert (GTK_IS_TREE_VIEW (widget));
    g_assert (!GTK_WIDGET_NO_WINDOW (widget));

    /* We want to center the popup over the original thumbnail.  Since
    we know the original thumbnail is square, the clipbox corresponds
    to the region, so we can just get the clipbox of the thumbnail
    and take the center of that as the center of our popum image.  */
    GdkRectangle tn_rec;	
    gdk_region_get_clipbox (tr, &tn_rec); 

    /* Size of popup image to use, in pixels on a side.  */
    const gint popup_size = 256;

    gint tree_view_x, tree_view_y;
    gdk_window_get_origin (widget->window, &tree_view_x, &tree_view_y);

    GdkWindowAttr nwa;
    nwa.event_mask = GDK_ALL_EVENTS_MASK;
    // FIXME: when the pointer is over the popup itself,
    // maybe_clear_popup_image doesn't notice that we are still over the
    // thumbnail below and so clears things!  FIXME: I don't understand
    // wy this code puts the popup image top left corner only halfway
    // down the thumbnail edge.  It looks fine this way actually, but as
    // I understande this code it should put popup top left an thumbnail
    // bottom right.
    nwa.x = tree_view_x + tn_rec.x + tn_rec.width;
    nwa.y = tree_view_y + tn_rec.y + tn_rec.height;
    nwa.width = popup_size;
    nwa.height = popup_size;
    nwa.wclass = GDK_INPUT_OUTPUT;
    nwa.window_type = GDK_WINDOW_CHILD;
    nwa.override_redirect = TRUE;

    GdkWindow *root_window 
        = gdk_screen_get_root_window (gdk_screen_get_default ());

    GdkWindow *popup_image_window 
        = gdk_window_new (root_window, &nwa, 
        GDK_WA_X | GDK_WA_Y | GDK_WA_NOREDIR);

    /* Iterator for the data in the current row of the list.  */
    GtkTreeIter iter;
    gtk_tree_model_get_iter (GTK_TREE_MODEL (list_store), &iter, path);

    char *data_file;
    gtk_tree_model_get (GTK_TREE_MODEL (list_store), &iter, COL_DATA_FILE, 
        &data_file, -1);

    char *metadata_file = meta_file_name (data_file);

    GdkPixbuf *popup_image_pixbuf 
        = make_input_image_thumbnail_pixbuf (metadata_file, data_file, 256);

    g_free (metadata_file);
    g_free (data_file);

    if (popup_image_pixbuf)
    {
        gdk_window_show (popup_image_window);

        /* Magic number understood by gdk_draw_pixbuf to mean "use pixbuf
        width".  */
        const gint use_pixbuf_width = -1;
        gdk_draw_pixbuf (GDK_DRAWABLE (popup_image_window),
            NULL,
            popup_image_pixbuf,
            0, 0, 0, 0,
            use_pixbuf_width, use_pixbuf_width,
            GDK_RGB_DITHER_NONE,
            0, 0);

        g_object_unref (popup_image_pixbuf);

        return popup_image_window;
    }
    else
    {
        return NULL;
    }
}

typedef gboolean motion_notify_event_handler(GtkWidget *widget,
                                             GdkEventMotion *event,
                                             gpointer user_data);
typedef struct {
    GdkWindow *popup;
    GtkTreeView *tree_view;
    GdkRegion *thumbnail_region;
} maybe_clear_popup_image_args_t;

static maybe_clear_popup_image_args_t maybe_clear_popup_image_args 
= {NULL, NULL, NULL};

/* Forward declarations.  */
static gboolean
files_list_motion_notify_event_handler (GtkWidget *widget,
                                        GdkEventMotion *event,
                                        gpointer user_data);
//static gboolean
//completed_files_list_motion_notify_event_handler (GtkWidget *widget,
//                                                  GdkEventMotion *event,
//                                                  gpointer user_data);

/* Forward declaration.  */
static void
update_thumbnail_popup_process (GtkWidget *widget, GdkEventMotion *event);

static gboolean
maybe_clear_popup_image (GtkWidget *widget, GdkEventMotion *event,
                         maybe_clear_popup_image_args_t *args)
{
    //g_message ("Doing %s", __func__);

    GdkWindow *popup_to_kill = args->popup;
    GtkTreeView *tree_view = args->tree_view;
    GdkRegion *tn_region = args->thumbnail_region; /* Thumbnail region.  */

    g_assert (GDK_IS_WINDOW (popup_to_kill));
    g_assert (GTK_IS_TREE_VIEW (tree_view));

    if ( !gdk_region_point_in (tn_region, (int) event->x, (int) event->y) ) {

        //g_message ("Clearing popup state!");

        gdk_window_destroy (popup_to_kill);

        /* Disconnect self.  */
        guint signal_id = g_signal_lookup ("motion-notify-event",
            GTK_WIDGET_TYPE (tree_view));
        guint disconnect_count 
            = g_signal_handlers_disconnect_matched (tree_view,
            G_SIGNAL_MATCH_ID 
            | G_SIGNAL_MATCH_FUNC
            | G_SIGNAL_MATCH_DATA,
            signal_id,
            (GQuark) 0,
            NULL,
            maybe_clear_popup_image,
            args);
        g_assert (disconnect_count == 1);

        /* Update the popup process to reflect the current position of the
        pointer (i.e. deal with the event that got us in this
        handler.  */
        update_thumbnail_popup_process (widget, event);

        /* Unblock the handler for motion events on the files list.  */
        gulong unblock_count 
            = g_signal_handlers_unblock_by_func 
            (tree_view, files_list_motion_notify_event_handler, NULL);
        g_assert (unblock_count == 1);
    }    

    return FALSE;
}

static void
update_thumbnail_popup_process (GtkWidget *widget, GdkEventMotion *event)
{
    //g_message ("Doing %s, event = %p", __func__, event);

    static gboolean witn = FALSE;	/* "Was in thumbnail".  */
    static GtkTreePath *otp = NULL;
    static GtkTreeViewColumn *otc = NULL;
    static GTimer *hover_timer = NULL;
    if ( hover_timer == NULL ) {
        hover_timer = g_timer_new ();
    }

    /* Fos some crazy reason, drawing the popup image changes the result
    of in_thumbnail (I suspect because gdk is reusing event
    structures in some strange way).  So memorize in_thumbnail up
    front.  */
    // Also, this assertion should pass, but doesn't... synthesizing
    // motion events is apparently a bit harder than I realized.
    // g_assert (event->type == GDK_MOTION_NOTIFY);
    //g_message ("event x: %lf, event y: %lf", event->x, event->y); 
    gboolean event_in_thumbnail = in_thumbnail (widget, event);

    /* Hover time in milliseconds required before a popup image is
    displayed.  */
    const gint hover_time = 70;

    // FIXME: It would be nice to allow popup images of both the input
    // and output thumbnails, but it requires more hassle keeping track
    // of where we were, where we are now, etc., so its not going to
    // happen until someone asks.
    if ( !witn && in_input_thumbnail (widget, event) ) {
        witn = TRUE;
        otp = thumbnail_path (widget, event);
        g_assert (gtk_tree_path_get_depth (otp) == 1);
        otc = thumbnail_column (widget, event);
        g_timer_start (hover_timer);
        //g_message ("Adding timeout from !with && in_input_thumbnail");
        fake_motion_signal_args_t *fmsa = g_new (fake_motion_signal_args_t, 1);
        fmsa->widget = widget;
        fmsa->event = event;      
        fmsa->is_valid = TRUE;
        g_timeout_add (hover_time, (GSourceFunc) emit_fake_motion_signal, fmsa);
    }

    if ( witn && in_input_thumbnail (widget, event) ) {
        //g_message ("in_thumbnail: %d", in_thumbnail (widget, event));
        GtkTreePath *ctp = thumbnail_path (widget, event);
        g_assert (gtk_tree_path_get_depth (ctp) == 1);
        GtkTreeViewColumn *ctc = thumbnail_column (widget, event);
        if ( gtk_tree_path_compare (ctp, otp) == 0 && ctc == otc ) {
            /* Sometimes the timeout handler seems to go off a bit before
            timer has measured the correct amount of time, so we add a
            small margin here.  */
            const gint slop_time = 5;
            const gint seconds_to_mseconds_factor = 1000;
            /* If we hover for this long or longer, we should show the popup.  */
            if ( g_timer_elapsed (hover_timer, NULL) * seconds_to_mseconds_factor 
                >= hover_time - slop_time) {
                    //g_message ("elapsed time: %lf\n", g_timer_elapsed (hover_timer, NULL));
                    //g_message ("Do popup!!!");
                    //g_message ("in_thumbnail: %d", in_thumbnail (widget, event));
                    GdkRegion *tr = thumbnail_region (widget, event);
                    //g_message ("bpw in_thumbnail: %d", in_thumbnail (widget, event));
                    //g_message ("widget: %p", widget);
                    GdkWindow *popup_window = draw_popup_image (widget, ctp, ctc, tr);
                    //g_message ("apw in_thumbnail: %d", in_thumbnail (widget, event));
                    //g_message ("widget: %p", widget);
                    /* We don't want continuous redrawing of the popup, so we
                    disable the handler that triggers it until the popup is
                    removed.  */

                    if (popup_window)
                    {
                        guint signal_id = g_signal_lookup ("motion-notify-event",
                            GTK_WIDGET_TYPE (widget));
                        gulong handler_id 
                            = g_signal_handler_find (widget,
                            G_SIGNAL_MATCH_ID | G_SIGNAL_MATCH_FUNC
                            | G_SIGNAL_MATCH_DATA,
                            signal_id,
                            (GQuark) 0,
                            NULL,
                            files_list_motion_notify_event_handler,
                            NULL);
                        g_assert (handler_id != 0);
                        g_signal_handler_block (widget, handler_id);
                        /* We want to get rid of the popup window as soon as the user
                        moves the mouse outside of the original thumbnail
                        space.  */
                        maybe_clear_popup_image_args.popup = popup_window;
                        maybe_clear_popup_image_args.tree_view = GTK_TREE_VIEW (widget);
                        if ( maybe_clear_popup_image_args.thumbnail_region != NULL ) {
                            gdk_region_destroy (maybe_clear_popup_image_args.thumbnail_region);
                        }
                        //g_message ("in_thumbnail: %d", in_thumbnail (widget, event));
                        maybe_clear_popup_image_args.thumbnail_region = tr;
                        g_signal_connect (widget, "motion-notify-event", 
                            G_CALLBACK (maybe_clear_popup_image), 
                            &maybe_clear_popup_image_args);
                        //g_message ("in_thumbnail: %d", in_thumbnail (widget, event));
                    }
                } 
            else {
                gtk_tree_path_free (ctp);
            }
        }
        else {
            gtk_tree_path_free (otp);
            otp = ctp;
            otc = ctc;
            g_timer_start (hover_timer);
            //g_message ("Adding timeout from 'different thumbnails'");
            fake_motion_signal_args_t *fmsa = g_new (fake_motion_signal_args_t, 1);
            fmsa->widget = widget;
            fmsa->event = event;      
            fmsa->is_valid = TRUE;
            g_timeout_add (hover_time, (GSourceFunc) emit_fake_motion_signal, fmsa);
        }
    }

    if ( witn && !event_in_thumbnail ) {
        //g_message ("in_thumbnail: %d", in_thumbnail (widget, event));
        //g_message ("Setting witn false");
        witn = FALSE;
    }
}

static gboolean
files_list_motion_notify_event_handler (GtkWidget *widget,
                                        GdkEventMotion *event,
                                        gpointer user_data)
{
    //g_message ("Doing %s, event = %p", __func__, event);
    // g_message ("x: %lf, y: %lf", event->x, event->y);
    update_thumbnail_popup_process (widget, event);

    return FALSE;
}

static gboolean
files_list_leave_notify_event_handler (GtkWidget *widget,
                                       GdkEventCrossing *event,
                                       GtkWidget *tree_view)
{
    g_assert ((gpointer) widget == (gpointer) tree_view);

    GdkEventMotion motion_event;
    fake_motion_signal_args_t *fmsa = g_new (fake_motion_signal_args_t, 1);
    fmsa->widget = tree_view;
    fmsa->event = &motion_event;
    fmsa->is_valid = TRUE;
    emit_fake_motion_signal (fmsa);

    return FALSE;
}

static gboolean    
files_list_scroll_event_handler (GtkWidget *widget, GdkEventScroll *event,
                                 gpointer user_data)
{
    GdkEventMotion motion_event;
    fake_motion_signal_args_t *fmsa = g_new (fake_motion_signal_args_t, 1);
    fmsa->widget = widget;
    fmsa->event = &motion_event;
    fmsa->is_valid = TRUE;
    emit_fake_motion_signal (fmsa);

    return FALSE;
}

#endif

void
setup_files_list(int argc, char *argv[])
{
    GtkTreeViewColumn *col;
    GtkCellRenderer *renderer;
    GValue val = {0,};

    list_store = gtk_list_store_new(5, 
                                    G_TYPE_STRING, 
                                    GDK_TYPE_PIXBUF,
                                    G_TYPE_STRING, 
                                    G_TYPE_STRING,
                                    G_TYPE_STRING);
    
    COL_DATA_FILE = 0;
    COL_INPUT_THUMBNAIL = 1;
    COL_OUTPUT_FILE = 2;
    COL_STATUS = 3;
    COL_LOG = 4;

    completed_list_store = gtk_list_store_new(5,
                                              G_TYPE_STRING, 
                                              G_TYPE_STRING, 
                                              GDK_TYPE_PIXBUF,
                                              G_TYPE_STRING,
                                              G_TYPE_STRING);
    
    COMP_COL_DATA_FILE = 0;
    COMP_COL_OUTPUT_FILE = 1;
    COMP_COL_OUTPUT_THUMBNAIL = 2;
    COMP_COL_STATUS = 3;
    COMP_COL_LOG = 4;

    int i;
    for (i = 1; i < argc; ++i)
        add_to_files_list(argv[i]);
    show_queued_thumbnails();

    GtkWidget *files_list = get_widget_checked("files_list");
    GtkWidget *completed_files_list =
        get_widget_checked("completed_files_list");

/*** First, the "pending" files list ****/
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

/*** Second, the "pending" files list ****/
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
/*
    g_signal_connect (completed_files_list, "motion-notify-event",
                G_CALLBACK (completed_files_list_motion_notify_event_handler), 
                NULL);
    
    g_signal_connect (completed_files_list, "leave-notify-event",
                G_CALLBACK (completed_files_list_leave_notify_event_handler), 
                completed_files_list);
    
    g_signal_connect (completed_files_list, "scroll-event",
                G_CALLBACK (completed_files_list_scroll_event_handler),
                NULL);
*/
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
