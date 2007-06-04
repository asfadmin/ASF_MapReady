#include <unistd.h>
#include <strings.h>

#include "asf_convert_gui.h"
#include "ceos_thumbnail.h"
#include "asf.h"

static GtkTreePath *
thumbnail_path (GtkWidget *widget, GdkEventMotion *event)
{
    GtkTreePath *path=NULL;
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
    vertically (FIXME: find a way to verify this with assertions).  */
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
in_thumbnail (GtkWidget *widget, GdkEventMotion *event)
{
    g_assert (GTK_IS_TREE_VIEW (widget));
    return in_input_thumbnail (widget, event);
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
    const gint popup_size = THUMB_SIZE_BIG;

    gint tree_view_x, tree_view_y;
    gdk_window_get_origin (widget->window, &tree_view_x, &tree_view_y);

    GdkWindowAttr nwa;
    nwa.event_mask = GDK_ALL_EVENTS_MASK;

    // popup must take into account any horizontal scrolling
    // that has taken place in the tree view
    GtkScrolledWindow *s =
       GTK_SCROLLED_WINDOW(get_widget_checked("scrolledwindow_in"));
    GtkAdjustment *adj = gtk_scrolled_window_get_hadjustment(s);
    gint h_adj = (gint)(.5 + gtk_adjustment_get_value(adj));

    // FIXME: when the pointer is over the popup itself,
    // maybe_clear_popup_image doesn't notice that we are still over the
    // thumbnail below and so clears things!  FIXME: I don't understand
    // why this code puts the popup image top left corner only halfway
    // down the thumbnail edge.  It looks fine this way actually, but as
    // I understand this code it should put popup top left at thumbnail
    // bottom right.
    nwa.x = tree_view_x + tn_rec.x + tn_rec.width - h_adj;
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
        = make_input_image_thumbnail_pixbuf (metadata_file, data_file,
                                             THUMB_SIZE_BIG);

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

typedef struct {
    GdkWindow *popup;
    GtkTreeView *tree_view;
    GdkRegion *thumbnail_region;
} maybe_clear_popup_image_args_t;

static maybe_clear_popup_image_args_t maybe_clear_popup_image_args 
= {NULL, NULL, NULL};

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
            G_SIGNAL_MATCH_ID | G_SIGNAL_MATCH_FUNC | G_SIGNAL_MATCH_DATA,
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

gboolean
files_list_motion_notify_event_handler (GtkWidget *widget,
                                        GdkEventMotion *event,
                                        gpointer user_data)
{
    update_thumbnail_popup_process (widget, event);
    return FALSE;
}

gboolean
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

gboolean    
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
