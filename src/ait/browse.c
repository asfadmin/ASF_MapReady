#undef win32
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
#endif // #ifdef win32

#include "ait.h"

static GtkWidget *open_img_win = NULL;
static GtkWidget *open_cfg_win = NULL;
static browse_callback *last_callback = NULL;

static void handle_ok(GtkWidget *open_win)
{
    assert(open_win);
    assert(last_callback);

    char *selected_file = gtk_file_chooser_get_filename(
        GTK_FILE_CHOOSER(open_win));

    printf("Selected: %s\n", selected_file);
    gtk_widget_hide(open_win);

    if (selected_file)
    {
        last_callback(selected_file);
        g_free(selected_file);
    }

    last_callback = NULL;
}

static void handle_cancel(GtkWidget *open_win)
{
    assert(open_win);
    gtk_widget_hide(open_win);
    last_callback = NULL;
}

static void img_ok_clicked()
{
    handle_ok(open_img_win);
}

static void cfg_ok_clicked()
{
    printf("Yo!\n");
    handle_ok(open_cfg_win);
}

static void img_cancel_clicked()
{
    handle_cancel(open_img_win);
}

static void cfg_cancel_clicked()
{
    handle_cancel(open_cfg_win);
}

void create_open_dialogs()
{
    GtkWidget *parent = get_widget_checked("ait_main");

    // Create dialog for opening up a D/img file
    {
        open_img_win = gtk_file_chooser_dialog_new(
            "Open Image File", GTK_WINDOW(parent), GTK_FILE_CHOOSER_ACTION_OPEN,
            GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, //Cancel button
            GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,   //Open button
            NULL);

        // we need to extract the buttons, so we can connect them to our
        // button handlers, above
        GtkHButtonBox *box = 
            (GtkHButtonBox*)(((GtkDialog*)open_img_win)->action_area);
        GList *buttons = box->button_box.box.children;

        GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
        GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

        g_signal_connect((gpointer)cancel_btn, "clicked",
            G_CALLBACK(img_cancel_clicked), NULL);
        g_signal_connect((gpointer)ok_btn, "clicked",
            G_CALLBACK(img_ok_clicked), NULL);

        // add the filters
        GtkFileFilter *D_filt = gtk_file_filter_new();
        gtk_file_filter_set_name(D_filt, "CEOS Data Files (*.D)");
        gtk_file_filter_add_pattern(D_filt, "*.D");
        gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(open_img_win), D_filt);

        GtkFileFilter *img_filt = gtk_file_filter_new();
        gtk_file_filter_set_name(img_filt, "ASF Image Files (*.img)");
        gtk_file_filter_add_pattern(img_filt, "*.img");
        gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(open_img_win), img_filt);

        GtkFileFilter *all_filt = gtk_file_filter_new();
        gtk_file_filter_set_name(all_filt, "All Files (*.*)");
        gtk_file_filter_add_pattern(all_filt, "*.*");
        gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(open_img_win), all_filt);
    }

    // Create dialog for opening up a .cfg file
    {
        open_cfg_win = gtk_file_chooser_dialog_new(
            "Open Configuration File", GTK_WINDOW(parent), GTK_FILE_CHOOSER_ACTION_OPEN,
            GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, //Cancel button
            GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,   //Open button
            NULL);

        GtkHButtonBox *box = 
            (GtkHButtonBox*)(((GtkDialog*)open_img_win)->action_area);
        GList *buttons = box->button_box.box.children;

        GtkWidget *cancel_btn = ((GtkBoxChild*)buttons->data)->widget;
        GtkWidget *ok_btn = ((GtkBoxChild*)buttons->next->data)->widget;

        g_signal_connect((gpointer)cancel_btn, "clicked",
            G_CALLBACK(cfg_cancel_clicked), NULL);
        g_signal_connect((gpointer)ok_btn, "clicked",
            G_CALLBACK(cfg_ok_clicked), NULL);

        GtkFileFilter *cfg_filt = gtk_file_filter_new();
        gtk_file_filter_set_name(cfg_filt, "Configuration Files (*.cfg)");
        gtk_file_filter_add_pattern(cfg_filt, "*.cfg");
        gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(open_cfg_win), cfg_filt);

        GtkFileFilter *all_filt = gtk_file_filter_new();
        gtk_file_filter_set_name(all_filt, "All Files (*.*)");
        gtk_file_filter_add_pattern(all_filt, "*.*");
        gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(open_cfg_win), all_filt);
    }
}

void browse(int filter_type, browse_callback bcb)
{
    last_callback = bcb;
    if (filter_type == FILTER_IMAGERY)
    {
        assert(open_img_win);
        gtk_widget_show(open_img_win);
    } 
    else if (filter_type == FILTER_CFG)
    {
        assert(open_cfg_win);
        gtk_widget_show(open_cfg_win);
    }
}
