#include "asf_view.h"

// global variable for the band info
BandConfig g_band_cfg;

void set_bands_rgb(int r, int g, int b)
{
    g_band_cfg.is_rgb = TRUE;
    g_band_cfg.band_r = r;
    g_band_cfg.band_g = g;
    g_band_cfg.band_b = b;
    g_band_cfg.band_gs = -1;
}

void set_bands_greyscale(int b)
{
    g_band_cfg.is_rgb = FALSE;
    g_band_cfg.band_r = -1;
    g_band_cfg.band_g = -1;
    g_band_cfg.band_b = -1;
    g_band_cfg.band_gs = b;
}

static void single_band(const char *str)
{
    show_widget("hbox_single", TRUE);
    show_widget("hbox_multi", FALSE);
    show_widget("hbox_bands_buttons", FALSE);

    if (strlen(str)>0 && strcmp(str, MAGIC_UNSET_STRING)!=0)
        put_string_to_label("single_band_label", str);
    else
        put_string_to_label("single_band_label", "Bands: -");
}

static void multi_band(const char *str)
{
    show_widget("hbox_single", FALSE);
    show_widget("hbox_multi", TRUE);
    show_widget("hbox_bands_buttons", TRUE);

    char tmp[128];
    snprintf(tmp, 128, "Bands: %s", str);
    put_string_to_label("multi_band_label", tmp);
}

static void disable_correct_hbox()
{
    int is_rgb = get_checked("combine_bands_into_rgb_radiobutton");
    enable_widget("single_band_as_greyscale_hbox", !is_rgb);
    enable_widget("combine_bands_into_rgb_hbox", is_rgb);
}

static void populate_combo_csv(const char *widget_name, char *csv, int i)
{
    clear_combobox(widget_name);

    char *p = csv; // current point (past the last seen comma)
    char *q = strchr(p, ','); // next point - 1 (the next comma)
    while (q) {
        *q = '\0'; // temporary
        add_to_combobox(widget_name, p);
        *q = ',';

        // go to next item
        p = q+1; 
        q = strchr(p, ',');
    }

    // add the last piece of the string
    add_to_combobox(widget_name, p);

    // select the correct item
    if (i<0) i=0;
    set_combo_box_item_checked(widget_name, i);
}

void setup_bands_tab(meta_parameters *meta)
{
    if (!meta || meta->general->band_count == 1 || 
        meta->general->band_count == MAGIC_UNSET_INT)
    {
        single_band(meta->general->bands);
    }
    else {
        // populate band selectors
        //enable_widget("single_band_as_greyscale_hbox", TRUE);
        //enable_widget("combine_bands_into_rgb_hbox", TRUE);
        populate_combo_csv("red_combobox",
            meta->general->bands, g_band_cfg.band_r);
        populate_combo_csv("green_combobox",
            meta->general->bands, g_band_cfg.band_g);
        populate_combo_csv("blue_combobox",
            meta->general->bands, g_band_cfg.band_b);
        populate_combo_csv("single_band_as_greyscale_combobox",
            meta->general->bands, g_band_cfg.band_gs);
        disable_correct_hbox();

        multi_band(meta->general->bands);
    }
}

SIGNAL_CALLBACK void
on_single_band_as_greyscale_radiobutton_toggled(GtkWidget *w)
{
    disable_correct_hbox();
}

SIGNAL_CALLBACK void
on_combine_bands_into_rgb_radiobutton_toggled(GtkWidget *w)
{
    disable_correct_hbox();
}

SIGNAL_CALLBACK void
on_bands_apply_button_clicked(GtkWidget *w)
{
    if (get_checked("combine_bands_into_rgb_radiobutton")) {
        char *r = get_band_combo_text("red_combobox");
        char *g = get_band_combo_text("green_combobox");
        char *b = get_band_combo_text("blue_combobox");
        char *s = MALLOC(sizeof(char)*(strlen(r)+strlen(g)+strlen(b)+10));
        sprintf(s,"%s,%s,%s",r?r:"-",g?g:"-",b?b:"-");
        printf("Load banded: %s, %s\n", g_filename, s);
        char *f = STRDUP(g_filename);
        reload_file_banded(f, s);
        free(s);
        FREE(r); FREE(g); FREE(b);
        free(f);
    } else {
        // greyscale
        char *gs = get_band_combo_text("single_band_as_greyscale_combobox");
        if (gs) {
            char *f = STRDUP(g_filename);
            reload_file_banded(f, gs);
            FREE(gs);
            FREE(f);
        }
    }
}

SIGNAL_CALLBACK void
on_bands_cancel_button_clicked(GtkWidget *w)
{
    setup_bands_tab(meta);
}
